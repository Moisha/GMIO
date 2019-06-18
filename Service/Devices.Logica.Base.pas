////////////////////////////////////////////
// Ѕазовые функции дл€ приборов фирмы Ћогика
////////////////////////////////////////////
unit Devices.Logica.Base;

interface

uses Windows, GMGlobals;

function Logica_CalcCRC(buf: array of Byte; Len, Start: int): WORD;
procedure LogicaM4_CRC(var buf: array of Byte; Len: int);
function LogicaM4_CheckCRC(buf: array of Byte; Len: int): bool;
procedure LogicaSPBUS_CRC(var buf: array of Byte; Len: int);

implementation

procedure LogicaM4_CRC(var buf: array of Byte; Len: int);
var CRC: WORD;
begin
  CRC := Logica_CalcCRC(buf, Len, 1);

  buf[Len] := CRC div 256;
  buf[Len + 1] := CRC mod 256;
end;

function LogicaM4_CheckCRC(buf: array of Byte; Len: int): bool;
var CRC: WORD;
begin
  Result := false;
  if Len < 3 then Exit;

  CRC := Logica_CalcCRC(buf, Len, 1);
  Result := CRC = 0;
end;

procedure LogicaSPBUS_CRC(var buf: array of Byte; Len: int);
var CRC: WORD;
begin
  CRC := Logica_CalcCRC(buf, Len, 2);

  buf[Len] := CRC div 256;
  buf[Len + 1] := CRC mod 256;
end;

function Logica_CalcCRC(buf: array of Byte; Len, Start: int): WORD;
var i, j: int;
begin
  Result := 0;
  for i := Start to Len - 1 do // первые Start символов управл€ющие, они отбрасываютс€
  begin
    Result := (Result xor buf[i] shl 8) and $FFFF;
    for j := 0 to 7 do
    begin
      if Result and $8000 > 0 then
        Result := ((Result shl 1) xor $1021) and $FFFF
      else
        Result := (Result shl 1) and $FFFF;
    end;
  end;
end;

end.
