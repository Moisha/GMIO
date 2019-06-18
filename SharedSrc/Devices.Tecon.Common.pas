unit Devices.Tecon.Common;

interface

uses Windows, GMGlobals;

procedure Tecon_CRC(var buf: array of Byte; Len: int);
function Tecon_CheckCRC(const buf: array of Byte; Len: int): bool;

procedure Tecon_ReplyAuthorization(var bufs: TTwoBuffers);
function Tecon_CheckK105Authorization(buf: array of Byte; Len: int): bool;

function Tecon_CheckCommonPrmReply(buf: array of Byte; Len: int): bool;
function Tecon_CheckCurrentsPrmReply(buf: array of Byte; Len: int): bool;
function Tecon_CheckArchPrmReply(buf: array of Byte; Len: int): bool;
function Tecon_CheckCurrentsPrmReplyCOM(buf: array of Byte; Len: int): bool;
function Tecon_CheckPrmReply(buf: array of Byte; Len: int): bool;

function DecToHexDigits(n: Byte): byte;

implementation

// функция для переделки времени нормального во время ТЭКОН
// специфика в том, что компоненты времени передаются в шестнадцатиричном виде, но такие же как в десятичном
// Например dec25 -> 0x25
function DecToHexDigits(n: Byte): byte;
var n1, n2: byte;
begin
  Result := 0;
  if n >= 100 then Exit;

  n1 := n div 10;
  n2 := n mod 10;

  Result := n1 * 16 + n2;
end;

// ТЭКОН может использовать ту же самую CRC с полиномом $A001, что и УБЗ
// А может ограничиться тупой суммой по модулю 256

// EN по модулю 256
function Tecon_CalcCRC(const buf: array of Byte; Len: int): Byte;
var i, n, CRC: int;
begin
  CRC := 0;

  if buf[0] = $68 then
    n := 4
  else
    n := 1;

  for i := n to Len - 1 do
    CRC := (CRC + buf[i]) and $FFFF;

  Result := CRC and $FF;
end;

procedure Tecon_CRC(var buf: array of Byte; Len: int);
begin
  buf[Len] := Tecon_CalcCRC(buf, Len);
  buf[Len + 1] := $16;
end;

function Tecon_ValidateMsg(const buf: array of Byte; Len: int): bool;
begin
  Result := false;
  if (Len < 2) or (Len > Length(buf)) then Exit;
  if not buf[0] in [$10, $68] then Exit;
  if buf[Len - 1] <> $16 then Exit;

  if buf[0] = $68 then
  begin
    // 68 LL+2 LL+2 68 0Р Адрес ХХо..ХХn КС 16
    if Len < 9 then Exit;
    if buf[3] <> $68 then Exit;
    // if (buf[4] and $F0) > 0 then Exit;
    if buf[1] <> buf[2] then Exit;
    if Len <> 6 + buf[1] then Exit;

    Result := true;
  end
  else
  if buf[0] = $10 then
  begin
    // 10 0Р Адрес ХХ0 ХХ1 ХХ2 ХХ3 КС 16
    if Len < 6 then Exit;
    if (buf[1] and $F0) > 0 then Exit;
    Result := true;
  end;
end;

function Tecon_CheckCRC(const buf: array of Byte; Len: int): bool;
begin
  Result := false;
  // предполагается, что мы проверяем прием
  // у приема подписывается все, включая номер пакета, не подписывется только заголовок "10" или "68 LL+2 LL+2 68"

  // Особый случай, квитанция Ok - $A2 и квитанция Error - $E5
  if (Len = 1) and (buf[0] in [$A2, $E5]) then
  begin
    Result := true;
    Exit;
  end;

  // предварительно имеет смысл провести валидацию, корректна ли посылка
  if not Tecon_ValidateMsg(buf, Len) then Exit;

  Result := Tecon_CalcCRC(buf, Len - 2) = buf[Len - 2];
end;

procedure Tecon_ReplyAuthorization(var bufs: TTwoBuffers);
begin
  bufs.BufSend[0] := $10;
  bufs.BufSend[1] := $40;
  bufs.BufSend[2] := bufs.BufRec[7];
  bufs.BufSend[3] := $1B;
  bufs.BufSend[4] := 0;
  bufs.BufSend[5] := 0;
  bufs.BufSend[6] := 0;

  Tecon_CRC(bufs.BufSend, 7);
  bufs.LengthSend := 9;
end;

function Tecon_CheckK105Authorization(buf: array of Byte; Len: int): bool;
begin
  Result := (Len = 14) and Tecon_CheckCRC(buf, Len) and
            (buf[0] = $68) and (buf[1] = $08) // заголовок
            and (buf[6] = $1B); // 1В - функция авторизации
end;

function Tecon_CheckReplyLength(buf: array of Byte; len: int; lens: SetOfInt): bool;
begin
  Result := (buf[1] = buf[2])
            and (buf[1] > 2)
            and ( (lens = []) or (buf[1] in lens) )
            and (len = buf[1] + 6); // buf[1] - номер преобразователя + длина данных + CRC. 6 - это заголовок, номер посылки и 0x16 в конце
end;

function Tecon_CheckCommonPrmReply(buf: array of Byte; Len: int): bool;
begin
  Result := Tecon_CheckCRC(buf, Len) and
            (buf[0] = $68) and (buf[3] = $68) // заголовок
            and (buf[4] and $F0 = 0); // номер запроса в формате $0P
end;

function Tecon_CheckCurrentsPrmReply(buf: array of Byte; Len: int): bool;
begin
  Result := Tecon_CheckCommonPrmReply(buf, len)
            and Tecon_CheckReplyLength(buf, len, [$03, $04, $06]);
end;

function Tecon_CheckArchPrmReply(buf: array of Byte; Len: int): bool;
begin
  Result := Tecon_CheckCommonPrmReply(buf, len)
            and Tecon_CheckReplyLength(buf, len, [])
            and (buf[1] mod 4 = 2);
end;

function Tecon_CheckCurrentsPrmReplyCOM(buf: array of Byte; Len: int): bool;
begin
  Result := Tecon_CheckCRC(buf, Len)
            and (Len = 9)
            and (buf[0] = $10) and (buf[1] = 0);
end;

function Tecon_CheckPrmReply(buf: array of Byte; Len: int): bool;
begin
  Result := Tecon_CheckCurrentsPrmReplyCOM(buf, len)
            or Tecon_CheckCurrentsPrmReply(buf, len)
            or  Tecon_CheckArchPrmReply(buf, len);
end;

end.
