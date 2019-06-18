unit DCVCommon;

interface

uses SysUtils, StrUtils, Windows, Classes, Math;

const EvalBufLen = 4096;

function Unqoute(const s: string): string;
function MakeReadable(const str: string): string;

implementation

function Unqoute(const s: string): string;
begin
  if (Result <> '') and (Result[1] = '''') then
    Delete(Result, 1, 1);

  if (Result <> '') and (Result[Length(Result)] = '''') then
    Delete(Result, Length(Result), 1);

  Result := StringReplace(s, '''''', '''', [rfReplaceAll]);
end;

function DecodeSymbols(const s: string): string;
var n, pos: integer;
    ch: byte;
    substr: string;
begin
  Result := '';
  n := 1;
  while n < Length(s) do
  begin
    pos := PosEx('#', s, n + 1);
    if pos <= 0 then
      pos := Length(s) + 1;

    substr := Copy(s, n + 1, pos - n - 1);
    ch := StrToIntDef(substr, 32);

    if ch in [9, 10, 13] then
      Result := Result + string(AnsiChar(ch))
    else
      Result := Result + '.';

    n := pos;
  end;
end;

function MakeReadable(const str: string): string;
var n, pos: integer;
    symbols, substr, s: string;
    builder: TStringBuilder;
    lastBlockIsText: bool;

  function ProcessQuotation(): bool;
  var cnt: integer;
  begin
    cnt := 0;
    while (n < Length(s)) and (s[n] = '''') and ((s[n + 1] = '''') or (cnt > 0)) do
    begin
      Inc(n);
      Inc(cnt);
    end;

    Result := cnt > 1;
    if Result then
    begin
      builder.Append(StringOfChar('''', cnt div 2));
      if not lastBlockIsText then
        dec(n);
    end;
  end;

    
begin
  s := str;
  if RightStr(s, 3) = '...' then
    Delete(s, Length(s) - 2, 3);

  builder := TStringBuilder.Create();
  
  n := 1;
  lastBlockIsText := false;
  while n <= Length(s) do
  begin
    ProcessQuotation();
    if n > Length(s) then break;

    pos := PosEx('''', s, n + 1);
    if pos <= 0 then
      pos := Length(s) + 1;

    if s[n] = '#' then  // спецсимволы
    begin
      substr := Copy(s, n, pos - n);
      symbols := DecodeSymbols(substr);
      builder.Append(symbols);
      n := pos; // позиция апострофа
      lastBlockIsText := false;
    end
    else
    begin
      if s[n] = '''' then // тут может оказаться не апостроф, если был апостроф в середине текстового блока
        substr := Copy(s, n + 1, pos - n - 1)
      else
        substr := Copy(s, n, pos - n);

      builder.Append(substr);
      n := pos + IfThen((pos < Length(s)) and (s[pos + 1] = ''''), 0, 1); // если за апострофом в конце идут еще апострофы, то их надо перерабатывать все вместе
      lastBlockIsText := true;
    end;
  end;

  Result := builder.ToString();
  builder.Free();
end;

end.
