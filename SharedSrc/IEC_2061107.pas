unit IEC_2061107;

interface

uses Windows, GMGlobals, SysUtils;

function Iso1155LRC_CalcEx(buf: array of Byte; First, Last: int; bInvert: bool): byte;
function Iso1155LRC_Calc(buf: array of Byte; len: int): byte;

procedure IEC2061107_CRC(var buf: array of Byte; Len: int);
function IEC2061107_CheckCRC(buf: array of Byte; Len: int): bool;

const
  strIEC_SOH = ' 01 ';
  strIEC_ISI = ' 1F ';
  strIEC_STX = ' 02 ';
  strIEC_ETX = ' 03 ';
  strIEC_DLE = ' 10 ';
  strIEC_HT  = ' 09 ';
  strIEC_FF  = ' 0C ';

  IEC_SOH = $01;
  IEC_ISI = $1F;
  IEC_STX = $02;
  IEC_ETX = $03;
  IEC_DLE = $10;
  IEC_HT  = $09;
  IEC_FF  = $0C;

implementation

function Iso1155LRC_Calc(buf: array of Byte; len: int): byte;
begin
  Result := Iso1155LRC_CalcEx(buf, 0, len - 1, true);
end;

function Iso1155LRC_CalcEx(buf: array of Byte; First, Last: int; bInvert: bool): byte;
var i: int;
begin
  Result := 0;

  if (First >= Length(buf)) or (Last >= Length(buf)) or (First < 0) or (Last < 0) then
  begin
    raise Exception.Create('Iso1155LRC_Calc - Range Check error');
  end;

  for i := First to Last do
    Result := (Result + buf[i]) and $FF;

  if bInvert then
    Result := ((Result xor $FF) + 1) and $FF; // в стандарте есть. В реале почему-то не всегда требуется.
end;

procedure IEC2061107_FindCRCLimits(buf: array of Byte; len: int; var first, last: int);
var i: int;
begin
  first := 0;
  last := len - 1;

  for i := len - 1 downto 0 do
    if buf[i] = IEC_ETX then
    begin
      last := i;
      break;
    end;

  for i := 0 to last - 1 do
    if buf[i] in [IEC_STX, IEC_SOH] then
    begin
      first := i + 1;
      break;
    end;
end;

procedure IEC2061107_CRC(var buf: array of Byte; Len: int);
var first, last: int;
begin
  if Len > Length(buf) then
  begin
    raise Exception.Create('IEC2061107_CRC - Range Check error');
  end;

  IEC2061107_FindCRCLimits(buf, Len, first, last);
  buf[Len] := Iso1155LRC_CalcEx(buf, first, last, false);
end;

function IEC2061107_CheckCRC(buf: array of Byte; Len: int): bool;
var first, last: int;
begin
  Result := false;

  if len < 2 then Exit;

  if Len > Length(buf) then
  begin
    raise Exception.Create('IEC2061107_CheckCRC - Range Check error');
  end;

  IEC2061107_FindCRCLimits(buf, Len - 1, first, last);
  Result := buf[Len - 1] = Iso1155LRC_CalcEx(buf, first, last, false);
end;

end.
