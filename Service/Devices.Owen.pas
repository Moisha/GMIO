////////////////////////////////////////////
// Кодировка и расчет CRC для приборов типа ОВЕН
////////////////////////////////////////////
unit Devices.Owen;

interface

Uses Windows, Classes, GMGlobals, SysUtils, Devices.ReqCreatorBase, GMConst;

type
  TOwenTRM138ReqCreator = class(TDevReqCreator)
  private
    procedure AddTRM138ToSendBuf_OneQuery(nChannel: int);
  public
    procedure AddRequests(); override;
  end;

procedure OwenDecodeString(const s: AnsiString; var ResBuf: array of byte);
function OwenCheckPack(const s: AnsiString): bool;
function OwenCheckCRC(Buf: array of Byte; nBytes: int): bool;
function OwenIdToHashStr(const str: AnsiString): WORD;
function OwenCRC(Buf: array of Byte; nBytes: int): WORD;
function OwenEncodeBufToASCII(buf: array of byte; Len: int): AnsiString;

implementation

uses AnsiStrings, GMSqlQuery, ProgramLogFile;

function OwenCharForHash(const Str: AnsiChar; bAddDot: bool): Byte;
var s: AnsiString;
begin
  s := AnsiUpperCase(Str);
  Result := 78; // пусть будет пробел на случай косячного ввода данных

  case s[1] of
    '0'..'9': Result := Ord(s[1]) - Ord('0');
    'A'..'Z': Result := Ord(s[1]) - Ord('A') + 10;
         '_': Result := 10 + 26 + 1;
         '/': Result := 10 + 26 + 2;
         ' ': Result := 10 + 26 + 3;
    else Exit;
  end;

  Result := Result * 2;
  if bAddDot then
    inc(Result);
end;

function OwenCRC(Buf: array of Byte; nBytes: int): WORD;
var i, j: int;
    b: Byte;
begin
	Result := 0;
	for i := 0 to nBytes - 1 do
	begin
		b := Buf[i];
		for j := 0 to 7 do
		begin
			if ((b xor (Result shr 8)) and $80) > 0 then
			begin
				Result := (Result and $7FFF) shl 1;
        Result := Result xor $8F57;
			end
			else
				Result := (Result and $7FFF) shl 1;

      b := (b and $7F) shl 1;
		end;
	end;
end;

function OwenCheckCRC(Buf: array of Byte; nBytes: int): bool;
var CRC: DWORD;
begin
  Result := false;
  if nBytes < 2 then Exit;

  CRC := OwenCRC(Buf, nBytes - 2);
  Result := (CRC = ReadWordInv(buf, nBytes - 2));
end;

function OwenIdToHash(Buf: array of Byte): WORD;
var i, j: int;
    b: Byte;
begin
	Result := 0;
	for i := 0 to 3 do
	begin
		b := Buf[i];
		b := (b and $7F) shl 1; // используются только младшие 7 бит
		for j := 0 to 6 do
		begin
			if ((b xor (Result shr 8)) and $80) > 0 then
			begin
				Result := (Result and $7FFF) shl 1;
				Result := Result xor $8F57;
			end
			else
				Result := (Result and $7FFF) shl 1;

      b := (b and $7F) shl 1;
		end;
	end;
end;

function OwenIdToHashStr(const str: AnsiString): WORD;
var buf: array [0..20] of Byte;
    s: AnsiString;
    i, n: int;
    bDot: bool;
begin
  n := 0;
  s := Trim(str);
  for i := 1 to Length(s) do
  begin
    if s[i] <> '.' then
    begin
      bDot := (i < Length(s)) and (s[i + 1] = '.');
      buf[n] := OwenCharForHash(s[i], bDot);
      inc(n);
    end;
  end;

  Result := OwenIdToHash(buf);
end;

function OwenEncodeBufToASCII(buf: array of byte; Len: int): AnsiString;

  function TetradeToSymbol(b: byte): AnsiChar;
  begin
    Result := AnsiChar(b + ord('G'));
  end;

var i: int;
begin
  Result := '';
  for i := 0 to Len - 1 do
    Result := Result + TetradeToSymbol(buf[i] shr 4) + TetradeToSymbol(buf[i] and $F);
end;

procedure OwenDecodeBuf(buf: array of byte; Len: int; var ResBuf: array of byte);
var n1, n2: Byte;
    i: int;
begin
  i := 1;
  while i < Len - 1 do
  begin
    n1 := buf[i] - ord('G');
    n2 := buf[i + 1] - ord('G');
    ResBuf[i div 2] := (n1 shl 4) + n2;
    inc(i, 2);
 end;
end;

procedure OwenDecodeString(const s: AnsiString; var ResBuf: array of byte);
var buf: array[0..100] of Byte;
begin
  StringToArray(s, buf);
  OwenDecodeBuf(buf, Length(s), ResBuf);
end;

function OwenCheckPack(const s: AnsiString): bool;
var i: int;
begin
  Result := false;
  
  if (Length(s) < 10)
      or (s[1] <> '#')
      or (s[Length(s)] <> #13)
      or Odd(Length(s)) then Exit;

  for i := 2 to Length(s) - 1 do
    if not (s[i] in ['G'..'V']) then Exit; 

  Result := true;
end;

{ TOwenTRM138ReqCreator }

procedure TOwenTRM138ReqCreator.AddTRM138ToSendBuf_OneQuery(nChannel: int);
var buf: array[0..6] of byte;
    sReq: AnsiString;
    rqtp: T485RequestType;
begin
  rqtp := T485RequestType(ord(rqtTRM138_T1) + nChannel - 1);

  buf[0] := (FReqDetails.DevNumber + nChannel - 1) and $FF;
  buf[1] := 0 + 0 + 0 + 16 + 0 + 0 + 0 + 0;
  WriteWORDInv(buf, 2, OwenIdToHashStr('rEAd'));
  WriteWORDInv(buf, 4, OwenCRC(buf, 4));
  sReq := '#' + OwenEncodeBufToASCII(buf, 6) + #13;

  AddStringRequestToSendBuf(string(sReq), rqtp);
end;

procedure TOwenTRM138ReqCreator.AddRequests();
var q: TGMSqlQuery;
    N_Src: int;
begin
  q := TGMSqlQuery.Create();
  try
    q.SQL.Text := 'select * from Params where ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and ID_PT > 0 order by ID_Src, N_Src';
    q.Open();
    while not q.Eof do
    begin
      N_Src := q.FieldByName('N_Src').AsInteger;
      if N_Src in [1..8] then
        AddTRM138ToSendBuf_OneQuery(N_Src);

      q.Next();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('AddTRM138ToSendBuf - ' + e.Message);
  end;
  q.Free();
end;

end.
