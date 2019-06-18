////////////////////////////////////////////
// Кодировка и расчет CRC для приборов Логика СПТ-941
////////////////////////////////////////////
unit Devices.Logica.SPT941;

interface

uses Windows, GMGlobals, GMConst, Classes, SysUtils, Devices.ReqCreatorBase, GMSqlQuery, GMBlockValues,
     GMGenerics, Generics.Collections, UsefulQueries;

function LogicaSPT941_CheckCRC(buf: array of Byte; Len: int): bool;

type
  TSPT941ArchReqPeriodInfo = record
    name: string;
    period: int;
    depth: int;

    function NameWithChannel(channel: int): string;
  end;

const
  FSPT941ArchReqPeriodTypes: array[0..2] of TSPT941ArchReqPeriodInfo =
  (
    (name: DEV_STATE_NAMES_LAST_ARCH_REQ_FULL_DEPTH; period: 30 * UTC_DAY; depth: 1990 * UTC_HOUR),
    (name: DEV_STATE_NAMES_LAST_ARCH_REQ_MONTH_DEPTH; period: 7 * UTC_DAY; depth: 31 * UTC_DAY),
    (name: DEV_STATE_NAMES_LAST_ARCH_REQ_DAY_DEPTH; period: UTC_HOUR; depth: UTC_DAY)
  );

type
  TSPT941ReqCreator = class(TDevReqCreator)
  private
    FRequestNumber: int;
    FCurrentReqChn: int;

    procedure InitSessionRequest;
    procedure CurrentsRequestChn();
    procedure ProcessCurrentsQuery(q: TGMSqlQuery; obj: pointer);
    procedure WrapAndSend(const body: string; reqType: T485RequestType);
    procedure CurrentsRequest;
    procedure LoadRequestNumber;
    procedure SaveRequestNumber;
    procedure AddArchives;
    procedure AddOneArchiveRequest(udt1, udt2: LongWord);
    function UTimeToReqString(udt: LongWord): string;
    function MaxArchRecordsInRequest: int;
    procedure AddArchives_OneChannel;
    function NeedReqArch_OnePeriod(const periodName: string; period: int): bool;
  protected
    function NeedReqArch(): int; virtual; // для тестов
    function CurrentsRequestType(Chn: int): T485RequestType; virtual;
    function CanRequestPack(): bool; virtual;
    function ChannelCount: int; virtual;
  public
    procedure AddRequests(); override;
  end;

  T941ArchChannnel = class
  public
    ID_Prm: int;
    RecordIndex: int;
  end;

  TSPT941AnswerParser = class
  private
    FValues: TGMCollection<TValueFromBaseClass>;
    FChannels: TGMCollection<T941ArchChannnel>;
    FParseError: string;
    Fgbv: TGeomerBlockValues;
    function CheckPackage: bool;
    function ParsePackage: bool;
    function ParseCurrents: bool;
    function FindWordPos(idx: int): int;
    function ReadValueFromPosition(pos: int; var res: double): bool;
    function CheckEnoughLengthForVal(pos, valLength: int): bool;
    procedure AddValue(id_prm: int; UTime: LongWord; val: double; valType: TGMValueType);
    function ParseArch: bool;
    procedure ReadArchChannels;
    procedure ReadArchChannel(q: TGMSqlQuery; obj: pointer);
    function ReadArcRecord(var pos: int): bool;
    function ReadArcRecord_CheckLength(sequenceStart: int; var dataStart, dataLen: int): bool;
    function ReadArcRecord_ProcessValues(utime: LongWord; dataStart, dataLen: int): bool;
  public
    function Parse(gbv: TGeomerBlockValues): bool;
    property ParseError: string read FParseError;
    property Values: TGMCollection<TValueFromBaseClass> read FValues;
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses DateUtils, ProgramLogFile, ArchIntervalBuilder, Math, Devices.Logica.Base;

const
  PNUM_TAG = $4A;
  PNUM_TAG_STR = ' 4A ';

  OCTET_STRING_TAG = $04;
  OCTET_STRING_TAG_STR = ' 04 ';

  ARCHDATE_TAG = $49;
  ARCHDATE_TAG_STR = ' 49 ';

  SEQUENCE_TAG = $30;

  FNC_ARCH = $61;
  FNC_ARCH_STR = ' 61 ';

  FNC_CURRENTS = $72;
  FNC_CURRENTS_STR = ' 72 ';

function LogicaSPT941_CheckCRC(buf: array of Byte; Len: int): bool;
begin
  Result := LogicaM4_CheckCRC(buf, len);
end;

{ TSPT941ReqCreator }

procedure TSPT941ReqCreator.InitSessionRequest();
var buf: array[0..15] of Byte;
    i: int;
begin
  for i := 0 to 15 do
    buf[i] := $FF;

  // инициализация линии
  AddBufRequestToSendBuf(buf, 16, rqtDummy);

  buf[0] := $10;
  buf[1] := FReqDetails.DevNumber and $FF;
  buf[2] := $90;
  buf[3] := $00;
  buf[4] := $00;
  buf[5] := $05;
  buf[6] := $00;
  buf[7] := $3f;
  buf[8] := $00;
  buf[9] := $00;
  buf[10] := $00;
  buf[11] := $00;
  LogicaM4_CRC(buf, 12);

  AddBufRequestToSendBuf(buf, 14, rqtDummy);
end;

function TSPT941ReqCreator.CanRequestPack: bool;
begin
  Result := FReqDetails.ObjType <> OBJ_TYPE_GM;
end;

procedure TSPT941ReqCreator.WrapAndSend(const body: string; reqType: T485RequestType);
const SOH = ' 10 ';
      FRM = ' 90 ';
      ATR = ' 00 ';
var buf: array[0..1024] of byte;
    tmp: ArrayOfByte;
    req, NT, ID, DL1, DL0: string;
    l: int;
begin
  inc(FRequestNumber);
  FRequestNumber := FRequestNumber mod 100;

  NT := ' ' + IntToHex(FReqDetails.DevNumber and $FF, 2);
  ID := ' ' + IntToHex(FRequestNumber, 2);
  l := Length(TextNumbersStringToArray(body));
  DL1 := ' ' + IntToHex(l mod 256, 2);
  DL0 := ' ' + IntToHex((l div 256) and $FF, 2);

  req := SOH + NT + FRM + ID + ATR + DL1 + DL0 + body;
  tmp := TextNumbersStringToArray(req);
  l := Length(tmp);
  WriteBuf(buf, 0, tmp, l);
  LogicaM4_CRC(buf, l);

  FReqDetails.ReqID := FRequestNumber;
  AddBufRequestToSendBuf(buf, l + 2, reqType);
end;

procedure TSPT941ReqCreator.ProcessCurrentsQuery(q: TGMSqlQuery; obj: pointer);
const DL = ' 03 ';
var Ch, Data, res: string;
    prm: int;
begin
  if not CanRequestPack() then
    FReqDetails.ClearReqLink();

  prm := q.FieldByName('CurrentsAddr').AsInteger;
  Data := ' ' + IntToHex(prm mod 256, 2) + ' ' + IntToHex((prm div 256) and $FF, 2);
  Ch := ' ' + IntToHex(q.FieldByName('CurrentsArgument').AsInteger, 2);

  res := PNUM_TAG_STR + DL + Ch + Data;

  FReqDetails.AddReqLink(q.FieldByName('ID_Prm').AsInteger);

  if CanRequestPack() then
    TSTringBuilder(obj).Append(res)
  else
    WrapAndSend(FNC_CURRENTS_STR + res, CurrentsRequestType(FCurrentReqChn));
end;

function TSPT941ReqCreator.CurrentsRequestType(Chn: int): T485RequestType;
begin
  Result := rqtSPT941;
end;

procedure TSPT941ReqCreator.CurrentsRequestChn();
var sb: TSTringBuilder;
begin
  FReqDetails.ReqLinkCount := 0;
  sb := TSTringBuilder.Create();
  try
    ReadFromQuery(      'select * from Params '+
                  #13#10'  where ID_Device = ' + IntToStr(FReqDetails.ID_Device) +
                  #13#10'  and coalesce(CurrentsArgument, 0) = ' + IntToStr(FCurrentReqChn) +
                  #13#10' order by ID_Src, N_Src', ProcessCurrentsQuery, sb);

    if CanRequestPack() and (Trim(sb.ToString()) > '') then
      WrapAndSend(FNC_CURRENTS_STR + sb.ToString(), CurrentsRequestType(FCurrentReqChn));
  finally
    sb.Free();
  end;
end;

function TSPT941ReqCreator.ChannelCount(): int;
begin
  Result := 1;
end;

procedure TSPT941ReqCreator.CurrentsRequest();
var i: int;
begin
  for i := 0 to ChannelCount() - 1 do
  begin
    FCurrentReqChn := i;
    CurrentsRequestChn();
  end;
end;

procedure TSPT941ReqCreator.LoadRequestNumber();
begin
  FRequestNumber := StrToIntDef(QueryResult('select LastReqNumber from ObjectStates where ID_Obj = ' + IntToStr(FReqDetails.ID_Obj)), 0);
end;

procedure TSPT941ReqCreator.SaveRequestNumber();
begin
  ExecPLSQL(Format('perform ObjectSetLastReqNumber(%d, %d)', [FReqDetails.ID_Obj, (FRequestNumber + 1) mod 256]));
end;

function TSPT941ReqCreator.UTimeToReqString(udt: LongWord): string;
var y, m, d, h, n, s, z: Word;
begin
  DecodeDateTime(UTCtoLocal(udt), y, m, d, h, n, s, z);
  if (y < 2000) or (y > 2255) then
    y := YearOf(Now());

  Result := Format('04 %2x %2x %2x %2x', [y - 2000, m, d, h]);
end;

procedure TSPT941ReqCreator.AddOneArchiveRequest(udt1, udt2: LongWord);
const DL = ' 05 ';
      CNT = ' FF FF ';
      RECTYPE = ' 00 ';
var N, body, ch: string;
begin
  ProgramLog.AddMessage(Format('AddOneArchiveRequest id_device = %d chn = %d, u1 = %d(%s), u2 = %d(%s)',
    [FReqDetails.ID_Device, FCurrentReqChn, udt1, DateTimeToStr(UTCtoLocal(udt1)), udt2, DateTimeToStr(UTCtoLocal(udt2))]));

  N := ' ' + IntToHex(MaxArchRecordsInRequest(), 2) + ' ';
  Ch := ' ' + IntToHex(FCurrentReqChn, 2) + ' ';

  body := FNC_ARCH_STR +
          OCTET_STRING_TAG_STR + DL + CNT + Ch + RECTYPE + N +
          ARCHDATE_TAG_STR + UTimeToReqString(udt1);
          // ARCHDATE_TAG_STR + UTimeToReqString(udt2);
  WrapAndSend(body, CurrentsRequestType(FCurrentReqChn));
end;

function TSPT941ReqCreator.MaxArchRecordsInRequest(): int;
begin
  Result := IfThen(CanRequestPack(), 4, 1);
end;

function TSPT941ReqCreator.NeedReqArch_OnePeriod(const periodName: string; period: int): bool;
var lastReq: ULong;
begin
  Result := false;
  lastReq := Min(StrToIntDef(SQLReq_GetDevState(FReqDetails.ID_Device, periodName), 0), NowGM() + UTC_HOUR);
  if int64(NowGM()) - lastReq > period then
  begin
    Result := true;
  end;
end;

function TSPT941ReqCreator.NeedReqArch(): int;
var i: int;
begin
  Result := -1;
  for i := 0 to High(FSPT941ArchReqPeriodTypes) do
  begin
    if NeedReqArch_OnePeriod(FSPT941ArchReqPeriodTypes[i].NameWithChannel(FCurrentReqChn), FSPT941ArchReqPeriodTypes[i].period) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TSPT941ReqCreator.AddArchives_OneChannel;
var lst: TList<ArchReqInterval>;
    a: ArchReqInterval;
    intrvBuilder: TArchIntervalBuilder;
    sql: string;
    ut_depth: ULong;
    reqType, i: int;
begin
  reqType := NeedReqArch();
  if reqType < 0 then Exit;

  ut_depth := int64((NowGM() div UTC_HOUR) * UTC_HOUR) - FSPT941ArchReqPeriodTypes[reqType].depth;
  sql := Format('select UTime from ArchHourRequestStartDateForDevice(%d, %d) order by 1', [FReqDetails.ID_Device, ut_depth]);

  intrvBuilder := TArchIntervalBuilder.Create(sql);
  try
    lst := intrvBuilder.BuildIntervalList(UTC_HOUR, MaxArchRecordsInRequest());
    try
      for a in lst do
      begin
        FReqDetails.UTimeArch := a.UTime1;
        FReqDetails.Priority := rqprArchives;
        AddOneArchiveRequest(a.UTime1, a.UTime2);
      end;

      for i := High(FSPT941ArchReqPeriodTypes) downto reqType do
        SQLReq_SetDevState(FReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[i].NameWithChannel(FCurrentReqChn), NowGM());
    finally
      lst.Free();
    end;
  finally
    intrvBuilder.Free();
  end;
end;

procedure TSPT941ReqCreator.AddArchives;
var i: int;
begin
  for i := 0 to ChannelCount() - 1 do
  begin
    FCurrentReqChn := i;
    AddArchives_OneChannel();
  end;
end;

procedure TSPT941ReqCreator.AddRequests;
begin
  LoadRequestNumber();
  InitSessionRequest();
  CurrentsRequest();
  AddArchives();
  SaveRequestNumber();
end;

{ TSPT941ReqParser }

function TSPT941AnswerParser.CheckPackage(): bool;
var len: int;
begin
  Result := false;

  len := ReadWord(Fgbv.gmBufRec, 5) + 7{header} + 2{crc};
  if (Fgbv.gmLenRec < 7) or (len <> Fgbv.gmLenRec) then
  begin
    FParseError := 'Bad Length';
    Exit;
  end;

  if not LogicaSPT941_CheckCRC(Fgbv.gmBufRec, Fgbv.gmLenRec) then
  begin
    FParseError := 'Bad CRC';
    Exit;
  end;

  if (Fgbv.gmBufRec[0] <> Fgbv.ReqDetails.buf[0]) // SOH
     or (Fgbv.gmBufRec[1] <> Fgbv.ReqDetails.buf[1]) // DevNumber
     or (Fgbv.gmBufRec[2] <> Fgbv.ReqDetails.buf[2]) // FRM
     or (Fgbv.gmBufRec[3] <> Fgbv.ReqDetails.buf[3]) // ID
     or (Fgbv.gmBufRec[4] <> Fgbv.ReqDetails.buf[4]) // ATR
     or (Fgbv.gmBufRec[7] <> Fgbv.ReqDetails.buf[7]) // FNC
    then
  begin
    FParseError := 'Wrong Package';
    Exit;
  end;

  Result := true;
end;

constructor TSPT941AnswerParser.Create;
begin
  inherited;

  FValues := TGMCollection<TValueFromBaseClass>.Create();
  FChannels := TGMCollection<T941ArchChannnel>.Create();
end;

destructor TSPT941AnswerParser.Destroy;
begin
  FValues.Free();
  FChannels.Free();

  inherited;
end;

function TSPT941AnswerParser.CheckEnoughLengthForVal(pos, valLength: int): bool;
begin
  Result := (Fgbv.gmLenRec > pos + 1{len} + valLength{val} + 2{crc}) and (Fgbv.gmBufRec[pos + 1] = valLength);
  FParseError := 'Bad Data: too short ';
end;

function TSPT941AnswerParser.ReadValueFromPosition(pos: int; var res: double): bool;
var
  y, m, d, h, n, s, dl: int;
begin
  Result := false;
  case Fgbv.gmBufRec[pos] of
    $43: // IEEFloat
      begin
        Result := CheckEnoughLengthForVal(pos, 4);
        if Result then
          res := ReadSingle(Fgbv.gmBufRec, pos + 2);
      end;
    $44: // MIXED
      begin
        Result := CheckEnoughLengthForVal(pos, 8);
        if Result then
          res := ReadUINT(Fgbv.gmBufRec, pos + 2) + ReadSingle(Fgbv.gmBufRec, pos + 6);
      end;
    $05:  // NULL
      begin
        res := NAN;
        Result := true;
      end;
    $41: // IntU
      begin
        res := ReadUINT(Fgbv.gmBufRec, pos + 2);
        Result := true;
      end;
    $47: // TIME
      begin
        s := Fgbv.gmBufRec[pos + 2] * 256 + Fgbv.gmBufRec[pos + 3];
        n := Fgbv.gmBufRec[pos + 4];
        h := Fgbv.gmBufRec[pos + 5];
        res := EncodeTime(h, n, s, 0);
        Result := true;
      end;
    $48: // DATE
      begin
        d := Fgbv.gmBufRec[pos + 2];
        m := Fgbv.gmBufRec[pos + 3];
        y := 2000 + Fgbv.gmBufRec[pos + 4];
        res := Encodedate(y, m, d);
        Result := true;
      end;
    $49: // ARCHDATE
      begin
        m := 1;
        d := 1;
        h := 0;
        dl := Fgbv.gmBufRec[pos + 1];
        Result := dl > 0;
        if Result then
        begin
          y := 2000 + Fgbv.gmBufRec[pos + 2];
          if dl > 1 then
            m := Fgbv.gmBufRec[pos + 3];
          if dl > 2 then
            d := Fgbv.gmBufRec[pos + 4];
          if dl > 3 then
            h := Fgbv.gmBufRec[pos + 5];

          res := EncodeDateTime(y, m, d, h, 0, 0, 0);
        end;
      end;
    $4B: // FLAGS
      begin
        res := ReadInt64(Fgbv.gmBufRec, pos + 2);
        Result := true;
      end
    else
    begin
      FParseError := 'Bad Data: unknown value type at pos ' + IntToStr(pos);
    end;
  end;

  if not Result then
    FParseError := 'Bad Data: Too short';
end;

function TSPT941AnswerParser.FindWordPos(idx: int): int;
var i: int;
    n: int;
    val: double;
begin
  Result := -1;
  n := 8;

  if not ReadValueFromPosition(n, val) then
    Exit;

  for i := 0 to idx - 1 do
  begin
    if not ReadValueFromPosition(n, val) then Exit;
    inc(n, 2{байт типа данных + байт длины данных} + Fgbv.gmBufRec[n + 1]{длина данных});
  end;

  Result := n;
end;

procedure TSPT941AnswerParser.AddValue(id_prm: int; UTime: LongWord; val: double; valType: TGMValueType);
var v: TValueFromBaseClass;
begin
  v := FValues.Add();
  v.ID_Prm := id_prm;
  v.Val.Chn := TObject(id_prm);
  v.Val.UTime := UTime;
  v.Val.Val := val;
  v.Val.ValType := valType;
end;

function TSPT941AnswerParser.ParseCurrents(): bool;
var i: int;
    n: int;
    val: double;
begin
  Result := false;
  for i := 0 to Fgbv.ReqDetails.ReqLinkCount - 1 do
  begin
    n := FindWordPos(i);
    if n < 0 then
      Exit;

    if not ReadValueFromPosition(n, val) then Exit;
    if not IsNan(val) then
      AddValue(Fgbv.ReqDetails.ReqLinks[i].id, Fgbv.gmTime, val, valueTypeCurrent);
  end;

  Result := true;
end;

procedure TSPT941AnswerParser.ReadArchChannel(q: TGMSqlQuery; obj: pointer);
var chn: T941ArchChannnel;
begin
  chn := FChannels.Add();
  chn.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
  chn.RecordIndex := q.FieldByName('HourArchAddr').AsInteger;
end;

procedure TSPT941AnswerParser.ReadArchChannels();
var sql: string;
begin
  FChannels.Clear();
  sql := Format('select ID_Prm, HourArchAddr from Params where ID_Device = %d and coalesce(HourArchAddr, 0) > 0', [Fgbv.ReqDetails.ID_Device]);
  ReadFromQuery(sql, ReadArchChannel);
end;

function TSPT941AnswerParser.ReadArcRecord_CheckLength(sequenceStart: int; var dataStart, dataLen: int): bool;
var n, coeff: int;
begin
  Result := false;
  if Fgbv.gmBufRec[sequenceStart + 1] < $80 then
  begin
    dataLen := Fgbv.gmBufRec[sequenceStart + 1];
    dataStart := sequenceStart + 2;
  end
  else
  begin
    n := Fgbv.gmBufRec[sequenceStart + 1] - $80; // длина DL
    if sequenceStart + n + 1 >= Fgbv.gmLenRec then Exit;

    dataStart := sequenceStart + 1 {DL} + n {длина DL} + 1;

    dataLen := 0;
    coeff := 1;
    while n > 0 do
    begin
      dataLen := dataLen + Fgbv.gmBufRec[sequenceStart + 1 + n] * coeff;

      dec(n);
      coeff := coeff * 256;
    end;
  end;

  Result := dataStart + dataLen < Fgbv.gmLenRec;;
end;

function TSPT941AnswerParser.ReadArcRecord_ProcessValues(utime: LongWord; dataStart, dataLen: int): bool;
var i, n, pos: int;
    val: double;
begin
  Result := false;
  n := 1;
  pos := dataStart;
  while pos < dataStart + dataLen do
  begin
    if not ReadValueFromPosition(pos, val) then Exit;

    if not IsNan(val) then
    begin
      for i := 0 to FChannels.Count - 1 do
      begin
        if FChannels[i].RecordIndex = n then
          AddValue(FChannels[i].ID_Prm, utime, val, valueTypeHourArch);
      end;
    end;

    pos := pos + Fgbv.gmBufRec[pos + 1] + 2;
    inc(n);
  end;

  Result := true;
end;

function TSPT941AnswerParser.ReadArcRecord(var pos: int): bool;
var dt: TDateTime;
    sequenceStart, dataStart, dataLen: int;
    val: double;
begin
  Result := false;
  if Fgbv.gmBufRec[pos] <> ARCHDATE_TAG then
  begin
    FParseError := 'ARCHDATE_TAG not found pos: ' + IntToStr(pos);
    Exit;
  end;

  if not ReadValueFromPosition(pos, val) then
  begin
    FParseError := 'ARCHDATE not found pos: ' + IntToStr(pos);
    Exit;
  end;

  dt := val;
  sequenceStart := pos + 1 + Fgbv.gmBufRec[pos + 1] + 1; // ARCHDATE_TAG + DL + DL bytes

  if (sequenceStart >= Fgbv.gmLenRec) or (Fgbv.gmBufRec[sequenceStart] <> SEQUENCE_TAG) then
  begin
    FParseError := 'SEQUENCE_TAG not found pos: ' + IntToStr(pos);
    Exit;
  end;

  if not ReadArcRecord_CheckLength(sequenceStart, dataStart, dataLen) then
  begin
    FParseError := 'CheckLength failed pos: ' + IntToStr(pos);
    Exit;
  end;

  Result := ReadArcRecord_ProcessValues(LocalToUTC(dt), dataStart, dataLen);

  pos := dataStart + dataLen;
end;

function TSPT941AnswerParser.ParseArch(): bool;
var pos: int;
begin
  Result := false;
  ReadArchChannels();

  pos := 8;
  while pos < FGbv.gmLenRec - 2{CRC} do
  begin
    if not ReadArcRecord(pos) then Exit;
  end;

  Result := true;
end;

function TSPT941AnswerParser.ParsePackage(): bool;
begin
  Result := false;

  case Fgbv.gmBufRec[7] of
    FNC_CURRENTS:
      Result := ParseCurrents();
    FNC_ARCH:
      Result := ParseArch();
    else
      FParseError := 'Bad FNC';
  end;
end;

function TSPT941AnswerParser.Parse(gbv: TGeomerBlockValues): bool;
begin
  FGbv := gbv;
  Result := CheckPackage() and ParsePackage();
end;

{ TSPT941ArchReqPeriodInfo }

function TSPT941ArchReqPeriodInfo.NameWithChannel(channel: int): string;
begin
  Result := name + IntToStr(channel);
end;

end.
