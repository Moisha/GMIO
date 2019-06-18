unit Devices.ModbusBase;

interface

uses Windows, GMGlobals, SysUtils, GMConst, StrUtils, Math;

const MODBUS_TCP_ANSWER_DATA_START = 9;
      MODBUS_RTU_ANSWER_DATA_START = 3;

procedure Modbus_CRC(var buf: array of Byte; Len: int);
procedure Modbus_LRC(var buf: array of Byte; Len: int);
function Modbus_CheckCRC(buf: array of Byte; Len: int): bool;
function Modbus_ReadWord(buf: array of Byte; dataAddr: int): int;
function Modbus_ResultTypeLength(resultType: int): int;

implementation

function Modbus_ResultTypeLength(resultType: int): int;
begin
  Result := 1;
  if resultType in [MODBUS_RESULT_DOUBLE_BIG_ENDIAN, MODBUS_RESULT_DOUBLE_LITTLE_ENDIAN] then
    Inc(Result, 3)
  else
  if resultType in [MODBUS_RESULT_LONG_BIG_ENDIAN..MODBUS_RESULT_SINGLE_LITTLE_ENDIAN] then
    Inc(Result);
end;

function Modbus_CRC_Calc(var buf: array of Byte; Len: int): WORD;
var i, j: int;
    CRC: WORD;
begin
  CRC := $FFFF;
  if Len < High(CRC) then
    for i := 0 to Len - 1 do
    begin
      CRC := (CRC xor buf[i]) and $FFFF;
      for j := 0 to 7 do
        if (CRC and 1) = 1 then
          CRC := ((CRC shr 1) xor $A001) and $FFFF
        else
          CRC := (CRC shr 1) and $FFFF;
    end;

  Result := CRC;
end;

procedure Modbus_LRC(var buf: array of Byte; Len: int);
var i: int;
    lrc: byte;
begin
  lrc := 0;
  for i := 1 to Len - 1 do
    lrc := (lrc + StrToInt('$' + AnsiChar(buf[i])) * IfThen(Odd(i), 16, 1)) and $FF;

  lrc := ($100 - lrc) and $FF;

  buf[Len] := ord(AnsiString(IntToHex(lrc, 2))[1]);
  buf[Len + 1] := ord(AnsiString(IntToHex(lrc, 2))[2]);
end;

procedure Modbus_CRC(var buf: array of Byte; Len: int);
var CRC: WORD;
begin
  CRC := Modbus_CRC_Calc(buf, Len);

  buf[Len] := CRC mod 256;
  buf[Len + 1] := CRC div 256;
end;

function Modbus_CheckCRC(buf: array of Byte; Len: int): bool;
var CRC: WORD;
begin
  if Len < 3 then
  begin
    Result := false;
    Exit;
  end;

  CRC := Modbus_CRC_Calc(buf, Len - 2);

  Result := (buf[Len - 2] = CRC mod 256) and (buf[Len - 1] = CRC div 256);
end;

function Modbus_ReadWord(buf: array of Byte; dataAddr: int): int;
begin
  Result := 0;
  // 6 - это номер устройства, код ф-и, к-во слов в ответе, CRC и второй байт искомого слова
  // addr нумеруются с 0
  if dataAddr * 2 + 6 >= Length(buf) then Exit;

  Result := buf[dataAddr * 2 + 3] * 256 + buf[dataAddr * 2 + 4];
end;

end.
