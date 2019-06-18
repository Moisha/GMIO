////////////////////////////////////////////
// Утилиты для работы с модемами Ancom
////////////////////////////////////////////

unit Devices.Ancom;

interface

uses Windows, GMGlobals;

function ReadAncomID(buf: array of byte; cnt: int): int;

implementation

// проверяем на идентификатор от Ancom
// идентификатор - 20 байт: цифровой код не длиннее 8ми цифр, остальные байты - нули
function ReadAncomID(buf: array of byte; cnt: int): int;
var i, n: int;
    s: AnsiString;
begin
  Result := -1;
  if cnt < 20 then Exit;

  n := -1;                                             
  for i := 0 to 9 do
  begin
    if not (buf[i] in [Ord('0')..Ord('9')]) then
    begin
      n := i;
      break;
    end;
  end;

  if not (n in [1..8]) then Exit;

  for i := n to 19 do
    if buf[i] <> 0 then Exit;

  for i := 0 to n - 1 do
    s := s + AnsiChar(buf[i]);

  Val(string(s), Result, n);
  if n > 0 then Result := -1;
end;

end.
