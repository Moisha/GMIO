////////////////////////////////////////////
// Утилиты для работы со счетчиками Меркурий
////////////////////////////////////////////

unit Devices.Mercury;

interface

uses Windows, Classes, SysUtils, GMGlobals, Devices.ReqCreatorBase, GMConst, UsefulQueries, DateUtils, Math, GmSqlQuery,
     ArchIntervalBuilder, Generics.Collections;

procedure Mercury_CRC(buf: ArrayOfByte; len: int);
function Mercury_CheckCRC(buf: ArrayOfByte; len: int): bool;

type
  TMercuryReadingsChannel = (mrcAin, mrcAout, mrcQin, mrcQout);
  TMercuryParser = class
  private
    FBuffer: ArrayOfByte;

    // служебных байта всегда 3: номер устройства в начале и CRC  вконце
    const ReadingsReplyLength = 19;  // 16 байт информации (по 4 на канал)
          MeterInfoReplyLength = 9;  // 6 байт информации
          LastArchAddrReplyLength = 12;  // 9 байт информации: адрес(2), статус(1), дата (5), длина периода интегрирования в минутах (1)
          ArchRecordReplyLength = 18; // 15 байт информации: статус(1), дата(5), длина интервала(1), данные(8 = 2байта * 4 канала),

    function SetBuffer(buf: ArrayOfByte; len: int): bool;
  protected
    function BinToDec(b: byte): int;
    function ReadArchUDT(dtpos: int): LongWord;
  public
    function SetReadingsBuffer(buf: ArrayOfByte; len: int): bool;
    function CheckReadingChannel(chn: TMercuryReadingsChannel): bool;
    function ReadingsChannelImpulses(chn: TMercuryReadingsChannel): ULong;

    // свойства счетчика
    function SetMeterInfoBuffer(buf: ArrayOfByte; len: int): bool;
    function ReadImpulsePerKWt(): int;

    // адрес и дата последней архзивной записи
    function SetLastArchAddrBuffer(buf: ArrayOfByte; len: int): bool;
    function ReadLastArchAddr: Word;
    function ReadLastArchUDT: LongWord;

    // архивная запись
    function SetArchiveRecordBuffer(buf: ArrayOfByte; len: int): bool;
    function ReadArchiveRecordUDT: LongWord;
    function CheckArchiveRecordChannel(chn: TMercuryReadingsChannel): bool;
    function ReadArchiveRecordChannelImpulses(chn: TMercuryReadingsChannel): int;
  end;

  TMercury230ReqCreator = class(TDevReqCreator)
  private
    FLastArchAddr: int64;
    FLastArchUtime: int64; // время начала интервала
    FLastArchAddrLastUpdate: LongWord;
    FMeterInfoLastUpdate: LongWord;

    function CheckLastArchRecord: bool;
    function Function8(Param: int): ArrayOfByte;

    procedure AddOpenExchangeChannel;
    procedure AddMeterInfo;
    procedure AddLastArchAddr;
    procedure AddMeterReadings;
    procedure AddOneArchiveRequest(UTime: LongWord);
  protected
    function ArchRecordAddr(Utime: LongWord): WORD;
    function ArchRecordsMinUDT: LongWord;

    property LastArchAddr: int64 read FLastArchAddr write FLastArchAddr;
    property LastArchUtime: int64 read FLastArchUtime write FLastArchUtime;
    property LastArchAddrLastUpdate: LongWord read FLastArchAddrLastUpdate write FLastArchAddrLastUpdate;
    property MeterInfoLastUpdate: LongWord read FMeterInfoLastUpdate write FMeterInfoLastUpdate;

    function ReadingsRequest(): ArrayOfByte;
    function OpenChannelRequest(): ArrayOfByte;
    function MeterInfoRequest(): ArrayOfByte;
    function LastArchAddrRequest: ArrayOfByte;

    function OneArchRecordRequest(addr: WORD): ArrayOfByte;
    procedure AddArchiveRequestsToSendBuf;

    procedure CheckDevStates; virtual;
    function NeedRequestLastArchAddr: bool; virtual;
    function CanRequestArchives: bool; virtual;
    function NeedRequestMeterInfo: bool; virtual;
  public
    procedure AddRequests(); override;
    constructor Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails); override;
  end;

implementation

uses ProgramLogFile;

const srCRCHi:array[0..255] of byte = (
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41,
$00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
$00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41,
$00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40,
$01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40,
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40,
$01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41,
$00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41,
$01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40,
$01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40);

      srCRCLo:array[0..255] of byte = (
$00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07, $C7, $05, $C5, $C4, $04, $CC, $0C, $0D, $CD,
$0F, $CF, $CE, $0E, $0A, $CA, $CB, $0B, $C9, $09, $08, $C8, $D8, $18, $19, $D9, $1B, $DB, $DA, $1A,
$1E, $DE, $DF, $1F, $DD, $1D, $1C, $DC, $14, $D4, $D5, $15, $D7, $17, $16, $D6, $D2, $12, $13, $D3,
$11, $D1, $D0, $10, $F0, $30, $31, $F1, $33, $F3, $F2, $32, $36, $F6, $F7, $37, $F5, $35, $34, $F4,
$3C, $FC, $FD, $3D, $FF, $3F, $3E, $FE, $FA, $3A, $3B, $FB, $39, $F9, $F8, $38, $28, $E8, $E9, $29,
$EB, $2B, $2A, $EA, $EE, $2E, $2F, $EF, $2D, $ED, $EC, $2C, $E4, $24, $25, $E5, $27, $E7, $E6, $26,
$22, $E2, $E3, $23, $E1, $21, $20, $E0, $A0, $60, $61, $A1, $63, $A3, $A2, $62, $66, $A6, $A7, $67,
$A5, $65, $64, $A4, $6C, $AC, $AD, $6D, $AF, $6F, $6E, $AE, $AA, $6A, $6B, $AB, $69, $A9, $A8, $68,
$78, $B8, $B9, $79, $BB, $7B, $7A, $BA, $BE, $7E, $7F, $BF, $7D, $BD, $BC, $7C, $B4, $74, $75, $B5,
$77, $B7, $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70, $B0, $50, $90, $91, $51, $93, $53, $52, $92,
$96, $56, $57, $97, $55, $95, $94, $54, $9C, $5C, $5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B,
$99, $59, $58, $98, $88, $48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E, $8F, $4F, $8D, $4D, $4C, $8C,
$44, $84, $85, $45, $87, $47, $46, $86, $82, $42, $43, $83, $41, $81, $80, $40);

const InitCRC: word = $FFFF;

function UpdCRC(C : byte; oldCRC : word) : word;
var i: byte;
    arrCRC: array [0..1] of byte absolute oldCRC;
begin
  i := arrCRC[1] xor C;
  arrCRC[1] := arrCRC[0] xor srCRCHi[i];
  arrCRC[0] := srCRCLo[i];
  UpdCRC := oldCRC;
end;

function Mercury_CalcCRC(buf: ArrayOfByte; len: int): WORD;
var i: int;
begin
  Result := UpdCRC(buf[0], InitCRC);

  For i := 1 to len-1 do
    Result := UpdCRC(buf[I], Result);

end;

procedure Mercury_CRC(buf: ArrayOfByte; len: int);
var CRC: WORD;
begin
  CRC := Mercury_CalcCRC(buf, len);
  buf[len] := CRC div 256;
  buf[len + 1] := CRC mod 256;
end;

function Mercury_CheckCRC(buf: ArrayOfByte; len: int): bool;
var CRC: WORD;
begin
  Result := false;
  if (len < 3) or (Length(buf) < len) then Exit;

  CRC := Mercury_CalcCRC(buf, len - 2);
  Result := (buf[len - 2] = CRC div 256) and (buf[len - 1] = CRC mod 256);
end;

{ TMercuryParser }

function TMercuryParser.CheckArchiveRecordChannel(chn: TMercuryReadingsChannel): bool;
var res: int;
begin
  Result := false;
  if Length(FBuffer) <> ArchRecordReplyLength then Exit;

  res := ReadArchiveRecordChannelImpulses(chn);
  Result := res < $FFFF;
end;

function TMercuryParser.CheckReadingChannel(chn: TMercuryReadingsChannel): bool;
var dw: UINT;
begin
  Result := false;
  if Length(FBuffer) <> ReadingsReplyLength then Exit;

  dw := ReadingsChannelImpulses(chn);
  Result := dw <> $FFFFFFFF;
end;

function TMercuryParser.ReadImpulsePerKWt: int;
begin
  Result := 0;
  if Length(FBuffer) = MeterInfoReplyLength then
  begin
    case FBuffer[2] and $0F of
      0: Result := 5000;
      1: Result := 25000;
      2: Result := 1250;
      3: Result := 500;
      4: Result := 1000;
      5: Result := 250;
    end;
  end;
end;

function TMercuryParser.ReadLastArchAddr: Word;
begin
  Result := 0;
  if Length(FBuffer) <> LastArchAddrReplyLength then Exit;

  Result := ReadWORDInv(FBuffer, 1);
end;

function TMercuryParser.BinToDec(b: byte): int;
begin
  Result := (b and $F) + ((b shr 4) and $F) * 10;
end;

function TMercuryParser.ReadArchUDT(dtpos: int): LongWord;
var y, m, d, h, n: int;
begin
  Result := 0;

  h := BinToDec(FBuffer[dtpos]);
  n := BinToDec(FBuffer[dtpos + 1]);
  d := BinToDec(FBuffer[dtpos + 2]);
  m := BinToDec(FBuffer[dtpos + 3]);
  y := 2000 + BinToDec(FBuffer[dtpos + 4]);

  if (m > 0) and (d > 0) then
  try
    Result := EncodeDateTimeUTC(y, m, d, h, n);
    if Result > 0 then Result := Result - UTC_HALFHOUR; // счетчик возвращает конец интервала!
  except end;
end;

function TMercuryParser.ReadArchiveRecordChannelImpulses(chn: TMercuryReadingsChannel): int;
var pos: int;
begin
  Result := 0;
  if Length(FBuffer) <> ArchRecordReplyLength then Exit;

  pos := 8 + 2 * Ord(chn);
  Result := ReadWORD(FBuffer, pos);
end;

function TMercuryParser.ReadArchiveRecordUDT: LongWord;
begin
  Result := 0;
  if Length(FBuffer) <> ArchRecordReplyLength then Exit;

  Result := ReadArchUDT(2);
end;

function TMercuryParser.ReadLastArchUDT: LongWord;
begin
  Result := 0;
  if Length(FBuffer) <> LastArchAddrReplyLength then Exit;

  Result := ReadArchUDT(4);
end;

function TMercuryParser.ReadingsChannelImpulses(chn: TMercuryReadingsChannel): ULong;
var buf: array[0..3] of byte;
    pos: int;
    u: UINT;
begin
  Result := 0;
  if Length(FBuffer) <> ReadingsReplyLength then Exit;

  pos := 1 + 4 * Ord(chn);
  buf[0] := FBuffer[pos + 1];
  buf[1] := FBuffer[pos + 0];
  buf[2] := FBuffer[pos + 3];
  buf[3] := FBuffer[pos + 2];

  u := ReadUINTInv(buf, 0);
  Result := u;
end;

function TMercuryParser.SetArchiveRecordBuffer(buf: ArrayOfByte; len: int): bool;
begin
  SetLength(FBuffer, 0);
  Result := (len = ArchRecordReplyLength) and SetBuffer(buf, len);
end;

function TMercuryParser.SetBuffer(buf: ArrayOfByte; len: int): bool;
begin
  Result := Mercury_CheckCRC(buf, len);
  if Result then
  begin
    SetLength(FBuffer, len);
    WriteBuf(FBuffer, 0, buf, Len);
  end;
end;

function TMercuryParser.SetLastArchAddrBuffer(buf: ArrayOfByte; len: int): bool;
begin
  SetLength(FBuffer, 0);
  Result := (len = LastArchAddrReplyLength) and SetBuffer(buf, len);
end;

function TMercuryParser.SetMeterInfoBuffer(buf: ArrayOfByte; len: int): bool;
begin
  SetLength(FBuffer, 0);
  Result := (len = MeterInfoReplyLength) and SetBuffer(buf, len);
end;

function TMercuryParser.SetReadingsBuffer(buf: ArrayOfByte; len: int): bool;
begin
  SetLength(FBuffer, 0);
  Result := (len = ReadingsReplyLength) and SetBuffer(buf, len);
end;

{ TMercury230ReqCreator }

function TMercury230ReqCreator.ReadingsRequest(): ArrayOfByte;
begin
  SetLength(Result, 6);
  Result[0] := FReqDetails.DevNumber and $FF;
  Result[1] := 5; // код ф-и запроса массивов
  Result[2] := 0; // накопленная энергия от сброса
  Result[3] := 0; // тариф
  Mercury_CRC(Result, 4);
end;

function TMercury230ReqCreator.OpenChannelRequest(): ArrayOfByte;
var i: int;
begin
  SetLength(Result, 11);
  Result[0] := FReqDetails.DevNumber and $FF;
  Result[1] := 1; // код ф-и ввода пароля
  Result[2] := 1; // уровень доступа - чтение
  for i := 0 to 5 do
    Result[i + 3] := 1; // пароль - 111111

  Mercury_CRC(Result, 9);
end;

function TMercury230ReqCreator.Function8(Param: int): ArrayOfByte;
begin
  SetLength(Result, 5);
  Result[0] := FReqDetails.DevNumber and $FF;
  Result[1] := 8; // код ф-и чтения параметра
  Result[2] := Param;
  Mercury_CRC(Result, 3);
end;

function TMercury230ReqCreator.MeterInfoRequest(): ArrayOfByte;
begin
  Result := Function8($12); // Чтение варианта исполнения счетчика
end;

function TMercury230ReqCreator.LastArchAddrRequest(): ArrayOfByte;
begin
  Result := Function8($13); // Чтение варианта исполнения счетчика
end;

function TMercury230ReqCreator.ArchRecordsMinUDT(): LongWord;
begin
  // одна запись о 16 байтах - это получасовка
  // Начинаются с 0
  Result := Max(0, FLastArchUtime - (FLastArchAddr div 16) * UTC_HALFHOUR);
end;

function TMercury230ReqCreator.OneArchRecordRequest(addr: WORD): ArrayOfByte;
begin
  SetLength(Result, 8);
  Result[0] := FReqDetails.DevNumber;
  Result[1] := 6; // код функции
  Result[2] := 3; // по идее тут д.б. 0, но архиватор Меркурия почему-то ставит 3. Хотя 3 - это "Чтение записей средних мощностей по R+"
  Result[3] := addr div 256;
  Result[4] := addr mod 256;
  Result[5] := 15; // число байт информации в ответе, вероятно можно увеличить и запрашивать по несколько значений за раз
  Mercury_CRC(Result, 6)
end;

function TMercury230ReqCreator.ArchRecordAddr(Utime: LongWord): WORD; // дата начала интервала, как FLastArchUtime
begin
  // старт записи всегда кратен 16 байтам
  if UTime > FLastArchUtime then
    Result := 0
  else
    Result := max(FLastArchAddr - 16 * ((FLastArchUtime - UTime) div UTC_HALFHOUR), 0);
end;

procedure TMercury230ReqCreator.AddOneArchiveRequest(UTime: LongWord); // дата начала интервала, как FLastArchUtime
var buf: ArrayOfByte;
    addr: WORD;
begin
  addr := ArchRecordAddr(Utime);
  if addr = 0 then Exit;

  buf := OneArchRecordRequest(addr);
  if Length(buf) > 0 then
  begin
    FReqDetails.Priority := rqprArchives;
    AddBufRequestToSendBuf(buf, Length(buf), rqtMercury230_Archive);
  end;
end;

procedure TMercury230ReqCreator.AddArchiveRequestsToSendBuf();
var q: TGMSqlQuery;
    lst: TList<ArchReqInterval>;
    a: ArchReqInterval;
    intrvBuilder: TArchIntervalBuilder;
    ut_min: LongWord;
begin
  q := TGMSqlQuery.Create();

  try
  try
    ut_min := Max(ArchRecordsMinUDT(), LocalToUTC(Floor(Now() - 20)));
    // начальные даты отсутствующих интервалов за последний месяц
    q.SQL.Text := Format('select UTime from ArchHourRequestStartDate((select ID_Prm from DeviceSystem where ID_Device = %d and ID_Src = %d and N_Src = %d), %d, %s) order by 1',
                          // возьмем SRC_CNT_MTR 1 - это A+. Канал А+ есть всегда, дырки будем определять именно по нему
                          [FReqDetails.ID_Device, SRC_CNT_MTR, 1, ut_min, '''30 minutes''']);
    q.Open();

    // Сгруппируем
    intrvBuilder := TArchIntervalBuilder.Create(q);
    lst := intrvBuilder.BuildIntervalList(0, 0); // интервалы берем по одному
    try
      for a in lst do
      begin
        if a.UTime1 <= FLastArchUtime then  // BuildIntervalList вернет начальные даты, FLastArchUtime - тоже начальная
          AddOneArchiveRequest(a.UTime1)
        else
          break;
      end;
    finally
      lst.Free();
      intrvBuilder.Free();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('TMercury230ReqCreator.AddArchiveRequestsToSendBuf - ' + e.Message);
  end;
  finally
    q.Free();
  end;
end;

function TMercury230ReqCreator.CheckLastArchRecord: bool;
begin
  Result := (FLastArchAddr > 0) and (FLastArchAddr < $FFFF)
            and (FLastArchUtime > 0) and (FLastArchUtime < $FFFFFFFF);
end;

function TMercury230ReqCreator.NeedRequestLastArchAddr(): bool;
var u: LongWord;
    n1, n2: int;
begin
  Result := not CheckLastArchRecord();

  if not Result then
  begin
    // последнее обновление адреса последней записи в архивах
    u := FLastArchAddrLastUpdate;
    n1 := NowGM() mod 3600;
    n2 := u mod 3600;

    // если с момента последнего уточнения последнего адреса прошла граница получасовки, переопросим адрес
    Result := (Abs(NowGM() - u) > 600) or ((n1 > 1800) and (n2 < 1800)) or ((n1 < 1800) and (n2 > 1800));
  end;
end;

function TMercury230ReqCreator.NeedRequestMeterInfo: bool;
begin
  // импульсы на КВт будем опрашивать раз в час
  Result := Abs(NowGM() - FMeterInfoLastUpdate) > UTC_DAY;
end;

function TMercury230ReqCreator.CanRequestArchives(): bool;
begin
  Result := CheckLastArchRecord();
end;

procedure TMercury230ReqCreator.CheckDevStates();
begin
  FLastArchAddrLastUpdate := SQLReq_GetDevStateLastUpdate(FReqDetails.ID_Device, DEV_STATE_NAMES_LAST_ARCH_ADDR);
  FLastArchAddr := StrToIntDef(SQLReq_GetDevState(FReqDetails.ID_Device, DEV_STATE_NAMES_LAST_ARCH_ADDR), 0);
  FLastArchUtime := StrToIntDef(SQLReq_GetDevState(FReqDetails.ID_Device, DEV_STATE_NAMES_LAST_ARCH_UDT), 0); // время начала интервала
  FMeterInfoLastUpdate := SQLReq_GetDevStateLastUpdate(FReqDetails.ID_Device, DEV_STATE_NAMES_IMP_PER_KWT);
end;

constructor TMercury230ReqCreator.Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails);
begin
  inherited;

  FLastArchAddrLastUpdate := 0;
  FLastArchAddr := 0;
  FMeterInfoLastUpdate := 0;

  CheckDevStates();
end;

procedure TMercury230ReqCreator.AddOpenExchangeChannel();
var buf: ArrayOfByte;
begin
  buf := OpenChannelRequest();
  AddBufRequestToSendBuf(buf, Length(buf), rqtDummy);
end;

procedure TMercury230ReqCreator.AddMeterInfo();
var buf: ArrayOfByte;
begin
    buf := MeterInfoRequest();
    AddBufRequestToSendBuf(buf, Length(buf), rqtMercury230_MeterInfo);
end;

procedure TMercury230ReqCreator.AddLastArchAddr();
var buf: ArrayOfByte;
begin
  buf := LastArchAddrRequest();
  AddBufRequestToSendBuf(buf, Length(buf), rqtMercury230_LastArchAddr);
end;

procedure TMercury230ReqCreator.AddMeterReadings();
var buf: ArrayOfByte;
begin
  buf := ReadingsRequest();
  AddBufRequestToSendBuf(buf, Length(buf), rqtMercury230_Currents);
end;

procedure TMercury230ReqCreator.AddRequests;
begin
  FReqDetails.Priority := rqprCurrents;

  // пароль и открытие сеанса
  AddOpenExchangeChannel();

  // коэффициент имп/КВт счетчика
  if NeedRequestMeterInfo() then
    AddMeterInfo();

  // показания
  AddMeterReadings();

  // Адрес последней записи архивов
  if NeedRequestLastArchAddr() then
    AddLastArchAddr();

  // запрос архивов
  if CanRequestArchives() then
    AddArchiveRequestsToSendBuf();
end;

end.
