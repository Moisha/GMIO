////////////////////////////////////////////
// Утилиты для работы сприборами завода Термотроник
////////////////////////////////////////////

unit Devices.Termotronic;

interface

uses Windows, SysUtils, GMGlobals, IEC_2061107, Devices.ReqCreatorBase, GMConst;

// Дописывает взад LRC
procedure Termotronic_CRC(var buf: array of Byte; Len: int);

// Берет сообщение шифровке ASCII
// проверяет на корректность и CRC
function Termotronic_CheckCRC(buf: array of Byte; Len: int): bool;

// Дописывает LRC и шифрует в ASCII
function Termotronic_PrepareMessage(buf: array of Byte; len: int): ArrayOfByte;

// Берет сообщение шифровке ASCII
// убирает служебные символы
// LRC не проверяет
function Termotronic_UndressMessage(buf: array of Byte; len: int): ArrayOfByte;

function Termotronic_BuildValuesRequest(addr, fnc: byte; regN, regCnt: int): ArrayOfByte;

type
  TTermotronicParser = class
    FBaseAddress: int;
    FBuffer, FMsg: ArrayOfByte;
  private
    function Offset(regN: int): int;
  public
    property BaseAddress: int read FBaseAddress write FBaseAddress;
    function SetBuffer(buf: array of Byte; len: int): bool;
    function DataLength: int;
    function CheckCRC(): bool;
    function ReadRegisterWord(regN: int): int;
    function ReadRegisterULong(regN: int): ULONG;
    function ReadRegisterSingle(regN: int): double;
    function ReadRegisterDouble(regN: int): double;
    property Msg: ArrayOfbyte read FMsg;
    constructor Create();
  end;

  TPeterflowRSReqCreator = class(TDevReqCreator)
  public
    procedure AddRequests(); override;
  end;

implementation

function Termotronic_BuildValuesRequest(addr, fnc: byte; regN, regCnt: int): ArrayOfByte;
var buf: array [0..6] of byte;
begin
  buf[0] := addr;
  buf[1] := fnc;
  WriteWORDInv(buf, 2, regN);
  WriteWORDInv(buf, 4, regCnt);

  Result := Termotronic_PrepareMessage(buf, 6);
end;

// Берет сообщение в шифровке ASCII
// Проверяет длину и служебные символы
// LRC не проверяет
function Termotronic_CheckASCIIMessage(buf: array of Byte; len: int): bool;
begin
  Result :=
    (Len > 3)
    and Odd(Len) // за вычетом начала и конца остается четное число символов
    and (buf[0] = $3A) // двоеточие - знак начала посылки
    and (buf[Length(buf) - 2] = 13) // <CR><LF> - признак конца посылки
    and (buf[Length(buf) - 1] = 10);
end;

procedure Termotronic_CRC(var buf: array of Byte; Len: int);
begin
  buf[Len] := Iso1155LRC_Calc(buf, Len);
end;

function Termotronic_CheckCRC(buf: array of Byte; Len: int): bool;
var msg: ArrayOfByte;
begin
  Result := false;
  msg := Termotronic_UndressMessage(buf, len);
  if Length(msg) > 0 then
  begin
    Result := Iso1155LRC_Calc(msg, Length(msg) - 1) = msg[Length(msg) - 1];
  end;
end;

function Termotronic_PrepareMessage(buf: array of Byte; len: int): ArrayOfByte;
var i: int;
    lrc: byte;
begin
  SetLength(Result, Len * 2 + 5);
  Result[0] := $3A; // двоеточие - знак начала посылки
  Result[Length(Result) - 2] := 13; // <CR><LF> - признак конца посылки
  Result[Length(Result) - 1] := 10;

  for i := 0 to len - 1 do
  begin
    Result[i * 2 + 1] := Ord(AnsiString(IntToHex(buf[i], 2))[1]);
    Result[i * 2 + 2] := Ord(AnsiString(IntToHex(buf[i], 2))[2]);
  end;

  lrc := Iso1155LRC_Calc(buf, len);
  Result[len * 2 + 1] := Ord(AnsiString(IntToHex(lrc, 2))[1]);
  Result[len * 2 + 2] := Ord(AnsiString(IntToHex(lrc, 2))[2]);
end;

function Termotronic_UndressMessage(buf: array of Byte; len: int): ArrayOfByte;
var i, resLen: int;
begin
  SetLength(Result, 0);
  if not Termotronic_CheckASCIIMessage(buf, len) then Exit;

  resLen := (len - 3) div 2;
  SetLength(Result, resLen);
  for i := 0 to resLen - 1 do
  begin
    Result[i] := StrToInt('$' + string(AnsiChar(buf[i * 2 + 1])) + string(AnsiChar(buf[i * 2 + 2])));
  end;
end;

{ TTermotronicParser }

function TTermotronicParser.CheckCRC: bool;
begin
  Result := Termotronic_CheckCRC(FBuffer, Length(FBuffer));
end;

constructor TTermotronicParser.Create;
begin
  inherited;
  FBaseAddress := 100;
end;

function TTermotronicParser.DataLength: int;
begin
  if Length(FMsg) < 3 then
    Result := 0
  else
    Result := Length(FMsg) - 3;
end;

function TTermotronicParser.Offset(regN: int): int;
begin
  Result := 3 + (regN - FBaseAddress) * 2;
end;

function TTermotronicParser.ReadRegisterDouble(regN: int): double;
var pos: int;
    buf: array[0..7] of byte;
begin
  pos := Offset(regN);
  buf[0] := Fmsg[pos + 1];
  buf[1] := Fmsg[pos + 0];
  buf[2] := Fmsg[pos + 3];
  buf[3] := Fmsg[pos + 2];
  buf[4] := Fmsg[pos + 5];
  buf[5] := Fmsg[pos + 4];
  buf[6] := Fmsg[pos + 7];
  buf[7] := Fmsg[pos + 6];


  Result := ReadDouble(buf, 0);
end;

function TTermotronicParser.ReadRegisterSingle(regN: int): double;
var pos: int;
    buf: array[0..3] of byte;
begin
  pos := Offset(regN);
  buf[0] := Fmsg[pos + 1];
  buf[1] := Fmsg[pos + 0];
  buf[2] := Fmsg[pos + 3];
  buf[3] := Fmsg[pos + 2];

  Result := ReadSingle(buf, 0);
end;

function TTermotronicParser.ReadRegisterULong(regN: int): ULONG;
var pos: int;
    buf: array[0..3] of byte;
begin
  pos := Offset(regN);
  buf[0] := Fmsg[pos + 1];
  buf[1] := Fmsg[pos + 0];
  buf[2] := Fmsg[pos + 3];
  buf[3] := Fmsg[pos + 2];

  Result := ReadUINT(buf, 0);
end;

function TTermotronicParser.ReadRegisterWord(regN: int): int;
begin
  Result := ReadWORDInv(FMsg, Offset(regN));
end;

function TTermotronicParser.SetBuffer(buf: array of Byte; len: int): bool;
var msgLen: int;
begin
  SetLength(FBuffer, len);
  WriteBuf(FBuffer, 0, buf, Len);
  Result := CheckCRC();
  if Result then
  begin
    FMsg := Termotronic_UndressMessage(FBuffer, Length(FBuffer));
    msgLen := Length(FMsg);
    Result := FMsg[2] = msgLen - 4; // Длина ответной посылки. 4 - это номер устройства, код ф-и, сама длина посылки и LRC
    Result := Result and ((msgLen div 4) * 4 = msgLen); // все, что нас интересует, кратно 4
  end;

  if not Result then
  begin
    SetLength(FBuffer, 0);
    SetLength(FMsg, 0);
  end;
end;

{ TPeterflowRSReqCreator }

procedure TPeterflowRSReqCreator.AddRequests;
var buf: ArrayOfByte;
begin
  buf := Termotronic_BuildValuesRequest(FReqDetails.DevNumber, 4, 100, 14);
  AddBufRequestToSendBuf(buf, Length(buf), rqtPeterFlowRS_Currents);
end;

end.
