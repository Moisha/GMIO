////////////////////////////////////////////
//  одировка и расчет CRC дл€ приборов Ћогика —ѕ“-961
////////////////////////////////////////////
unit Devices.Logica.SPT961;

interface

uses Windows, GMGlobals, GMConst, Classes, SysUtils, Devices.ReqCreatorBase;

function LogicaSPT961_CheckCRC(buf: array of Byte; Len: int): bool;

type
  TSPT961AnswerType = (logicaAnswerNone, logicaAnswerValue, logicaAnswerTimeArray);

  TSPT961ReqCreator = class(TDevReqCreator)
  protected
    FNC, DataDLEHead, DataDLESet, DAD: string;

    Chn: int;
    Prm: int;
    IdxStart, IdxCount: int;
    archDT1, archDT2: TDateTime;
    ArchType: TGMArchType;

    BufSend: array[0..1024] of Byte;
    LengthSend: int;

    function FromExtData(const Extdata: string; archType: T485RequestType = rqtSPT961): bool;

    procedure CommonPrm();
    procedure IndexPrm();
    procedure ArchStructure();
    procedure ArchData();
    procedure TimeArray();

    function ResultString(): string;

    function ArrayTypePrmNumber(): int;
    procedure MakeBuf();
    function EncodeNumber(n: int): string;
    function InternalResultString(): string;
    function EncodeDT(dt: TDateTime): string;
    function EncodeChnAndPrm(nPrm: int): string;
    procedure AddArchReqToSendBuf(ID_Prm: int; ExtData: string; archType: T485RequestType);
    property SAD: string read DAD; // адрес главного модул€ не контролируем, всегда берем равным номеру самого прибора
  public
    class function ExtractFromExtData(const Extdata: string; archType: T485RequestType; var Chn, Prm: int): bool;

    procedure AddRequests(); override;
  end;

  TSPT961AnswerParser = class
  private
    FArchive: TValuesCollection;
    FValue: double;
    FResDevNumber: int;
    FResType: TSPT961AnswerType;

    Buf: ArrayOfByte;
    Len: int;
    FResPrm: int;
    FResChn: int;
    procedure InitResult;
    function FindMarker(Start, Marker: int): int;
    function ParseHeader(posDLE, posFF: int): bool;
    function ExtractChnAndParam(pos: int): bool;
    function NextWord(marker: int; var pos: int; var res: string): bool;
    function ParseBody: bool;
    function ParseValue: bool;
    function ParseTimeArray: bool;
    function ParseTimeArrayString(const str: string): bool;
    function ParseTimeArrayDT(const s: string): TDateTime;
  public
    property Archive: TValuesCollection read FArchive;
    property Value: double read FValue;
    property ResType: TSPT961AnswerType read FResType;
    property ResDevNumber: int read FResDevNumber;
    property ResChn: int read FResChn;
    property ResPrm: int read FResPrm;

    function Parse(ABuf: ArrayOfByte; ALen: int): bool;
    function CheckInitialRequest(ReqDetails: TRequestDetails; PrmExtData: string): bool;

    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses DateUtils, IEC_2061107, ProgramLogFile, ArchIntervalBuilder, GMSqlQuery, Generics.Collections, Math, Devices.Logica.Base;

function LogicaSPT961_CheckCRC(buf: array of Byte; Len: int): bool;
var CRC: WORD;
    n: int;
begin
  Result := false;
  if Len < 3 then Exit;

  // в начале могут быть левые символы FF, их отбросим
  n := 0;
  while (n < Len) and (buf[n] <> IEC_DLE) do
    inc(n);

  if n >= Len then Exit;

  CRC := Logica_CalcCRC(buf, Len - 2, n + 2);
  Result := (buf[Len - 2] = CRC div 256) and (buf[Len - 1] = CRC mod 256);
end;

{ SPT961RequestCreator }

function TSPT961ReqCreator.EncodeDT(dt: TDateTime): string;
var dd, mm, yy, hh, nn, ss, zz: WORD;
begin
  DecodedateTime(dt, yy, mm, dd, hh, nn, ss, zz);
  Result := strIEC_HT + EncodeNumber(dd) + strIEC_HT + EncodeNumber(mm) + strIEC_HT + EncodeNumber(yy) +
            strIEC_HT + EncodeNumber(hh) + strIEC_HT + EncodeNumber(nn) + strIEC_HT + EncodeNumber(ss) ; // dd mm yy hh nn ss
end;

function TSPT961ReqCreator.EncodeChnAndPrm(nPrm: int): string;
begin
  Result := strIEC_HT + EncodeNumber(Chn) + strIEC_HT + EncodeNumber(nPrm);
end;

procedure TSPT961ReqCreator.ArchData;
begin
  FNC := '18';  // 18 - чтение архива
  DataDLESet := EncodeChnAndPrm(ArrayTypePrmNumber()) + strIEC_FF + EncodeDT(archDT1) + strIEC_FF;
  MakeBuf();
end;

procedure TSPT961ReqCreator.TimeArray;
begin
  FNC := '0E';  // 0E - временной массив
  // даты с-по в обратном пор€дке!
  DataDLESet := EncodeChnAndPrm(Prm) + strIEC_FF + EncodeDT(archDT2) + strIEC_FF + EncodeDT(archDT1) + strIEC_FF;
  MakeBuf();
end;

procedure TSPT961ReqCreator.ArchStructure;
begin
  FNC := '19';  // 19 - структура архива
  DataDLESet := EncodeChnAndPrm(ArrayTypePrmNumber()) + strIEC_FF;
  MakeBuf();
end;

function TSPT961ReqCreator.ArrayTypePrmNumber: int;
begin
  case ArchType of
    gmarchHour: Result := 65530;
    gmarchDay: Result := 65532;
    gmarchMonth: Result := 65534;
    else Result := 0;
  end;
end;

procedure TSPT961ReqCreator.CommonPrm;
begin
  FNC := '1D';  // 1D - чтение обычного параметра
  DataDLESet := EncodeChnAndPrm(Prm) + strIEC_FF;
  MakeBuf();
end;

function TSPT961ReqCreator.EncodeNumber(n: int): string;
var i: int;
    s: string;
begin
  s := IntToStr(n);

  for i := 1 to Length(s) do
    Result := Result + IntToHex(Ord(s[i]), 2) + ' ';

  Result := Trim(Result);
end;

procedure TSPT961ReqCreator.IndexPrm;
begin
  FNC := '0C';  // 0C - чтение индексного параметра
  DataDLESet := strIEC_HT + EncodeNumber(Chn) + strIEC_HT + EncodeNumber(Prm) +
                strIEC_HT + EncodeNumber(IdxStart) + strIEC_HT + EncodeNumber(IdxCount) + strIEC_FF;
  MakeBuf();
end;

function TSPT961ReqCreator.InternalResultString: string;
begin
  Result := strIEC_DLE + strIEC_SOH + DAD + SAD + strIEC_DLE + strIEC_ISI + FNC + DataDLEHead;
  Result := Trim(StringReplace(Result, '  ', ' ', [rfReplaceAll]));
end;

procedure TSPT961ReqCreator.MakeBuf;
var n: int;
    buf: ArrayOfByte;
begin
  DAD := ' ' + IntToHex(FReqDetails.DevNumber, 2) + ' ';
  DataDLEHead := strIEC_DLE + strIEC_STX + DataDLESet + strIEC_DLE + strIEC_ETX;

  buf := TextNumbersStringToArray(InternalResultString());
  n := Length(buf);

  WriteBuf(BufSend, 0, buf, n);
  LogicaSPBUS_CRC(BufSend, n);
  LengthSend := n + 2;
end;

function TSPT961ReqCreator.ResultString: string;
begin
  Result := ArrayToString(bufSend, LengthSend, false, true);
end;

function TSPT961ReqCreator.FromExtData(const Extdata: string; archType: T485RequestType = rqtSPT961): bool;
begin
  Result := ExtractFromExtData(Extdata, archType, Chn, Prm);
end;

class function TSPT961ReqCreator.ExtractFromExtData(const Extdata: string;
  archType: T485RequestType; var Chn, Prm: int): bool;
var n1, n2, c: int;
    sl: TStringList;
begin
  Result := false;
  sl := TStringList.Create();
  try
    sl.Delimiter := ' ';
    sl.DelimitedText := Trim(Extdata);

    if sl.Count < 2 then Exit;

    Val(sl[0], n1, c);
    if (c > 0) or (n1 < 0) then Exit;

    Val(sl[1], n2, c);
    if (c > 0) or (n2 < 0) then Exit;

    if archType >= rqtSPT961_DAY then // проверим третий аргумент в любом случае, даже если нам надо что-то дальше
    begin
      if sl.Count < 3 then Exit;
      Val(sl[2], n2, c);
      if (c > 0) or (n2 < 0) then Exit;
    end;

    if archType = rqtSPT961_HOUR then
    begin
      if sl.Count < 4 then Exit;
      Val(sl[3], n2, c);
      if (c > 0) or (n2 < 0) then Exit;
    end;

    Chn := n1;
    Prm := n2;
    Result := true;
  finally
    sl.Free();
  end;
end;

procedure TSPT961ReqCreator.AddArchReqToSendBuf(ID_Prm: int; ExtData: string; archType: T485RequestType);
var q: TGMSqlQuery;
    uptime: TDatetime;
    step: Cardinal;
    archTable: string;
    lst: TList<ArchReqInterval>;
    a: ArchReqInterval;
    rqtp: T485RequestType;
    intrvBuilder: TArchIntervalBuilder;
begin
  // даем максимальную разбежку по времени —ѕ“ в 5 мин
  // тогда за 5 мин уже запишутс€ полноценные часовки и суточные
  if MinuteOf(Now()) < 5 then Exit;

  case archType of
    rqtSPT961_DAY:
      begin
        uptime := Floor(Now());
        archTable := 'ArchDay';
        step := UTC_DAY;
        rqtp := rqtSPT961_DAY;
      end;

    rqtSPT961_HOUR:
      begin
        uptime := Floor(Now()) + HourOf(Now()) * OneHour;
        archTable := 'ArchHour';
        step := UTC_HOUR;
        rqtp := rqtSPT961_HOUR;
      end;

    else Exit;
  end;

  q := TGMSqlQuery.Create();

  try
  try
    if not FromExtData(ExtData, archType) then Exit;

    // начальные даты отсутствующих интервалов за последний мес€ц
    q.SQL.Text := Format('select UTime from %sRequestStartDate(%d, %d) order by 1',
                          [archTable, ID_Prm, NowGM() - 20 * UTC_DAY]);
    q.Open();

    // —группируем
    intrvBuilder := TArchIntervalBuilder.Create(q);
    lst := intrvBuilder.BuildIntervalList(step, 5);
    try
      for a in lst do
      begin
        if UTCtoLocal(a.UTime1) > uptime then break;

        // ¬ lst нам вернутс€ начальные даты. ј прибор оперирует конечными.
        archDT1 := UTCtoLocal(a.UTime1 + step);
        archDT2 := Min(uptime, UTCtoLocal(a.UTime2 + step + UTC_MINUTE));
        TimeArray();

        FReqDetails.ID_Prm := ID_Prm;
        FReqDetails.Priority := rqprArchives;
        AddBufRequestToSendBuf(BufSend, LengthSend, rqtp);
      end;
    finally
      lst.Free();
      intrvBuilder.Free();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('AddSPT961ArchReqToSendBuf - ' + e.Message);
  end;
  finally
    q.Free();
  end;
end;

procedure TSPT961ReqCreator.AddRequests();
var q: TGMSqlQuery;
begin
  q := TGMSqlQuery.Create();
  q.SQL.Text :=       'select *, Abs(NowGM() - coalesce(LastArchDay, 0)) as DayReqAge, Abs(NowGM() - coalesce(LastArchHour, 0)) as HourReqAge ' +
                #13#10'  from Params p'  +
                #13#10'       join Devices d on p.ID_Device = d.ID_Device ' +
                #13#10'       left outer join ParamStates ps on p.ID_Prm = ps.ID_Prm ' +
                #13#10'    where p.ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and ID_PT > 0 order by ID_Src, N_Src';
  try
    q.Open();

    while not q.Eof do
    begin
      if FromExtData(q.FieldByName('ExtData').AsString, rqtSPT961) then
      begin
        CommonPrm();

        FReqDetails.Priority := rqprCurrents;
        FReqDetails.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
        AddBufRequestToSendBuf(BufSend, LengthSend, rqtSPT961);

        if (q.FieldByName('DayReqAge').AsInteger > UTC_HOUR) // суточные - раз в час
           and (q.FieldByName('ID_Src').AsInteger in [SRC_AI, SRC_CNT_DI]) then
        begin
          AddArchReqToSendBuf(FReqDetails.ID_Prm, q.FieldByName('ExtData').AsString, rqtSPT961_DAY);
        end;

        if (q.FieldByName('HourReqAge').AsInteger > 10 * UTC_MINUTE) // часовки - каждые 10 мин
           and (q.FieldByName('ID_Src').AsInteger in [SRC_AI, SRC_CNT_DI]) then
        begin
          AddArchReqToSendBuf(FReqDetails.ID_Prm, q.FieldByName('ExtData').AsString, rqtSPT961_HOUR);
        end;
      end;

      q.Next();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('TSPT961ReqCreator.AddRequests - ' + e.Message);
  end;
  q.Free();
end;

{ TSPT961RequestParser }

procedure TSPT961AnswerParser.InitResult();
begin
  FResType := logicaAnswerNone;
  FResDevNumber := -1;
  FArchive.Clear();
end;

function TSPT961AnswerParser.FindMarker(Start, Marker: int): int;
var i: int;
begin
  Result := -1;
  for i := Start to Len - 1 do
  begin
    if buf[i] = Marker then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TSPT961AnswerParser.NextWord(marker: int; var pos: int; var res: string): bool;
var next: int;
begin
  next := FindMarker(pos + 1, marker);
  Result := next > pos;
  if Result then
  begin
    res := string(ReadString(buf, pos + 1, next - pos - 1));
    pos := next;
  end;
end;

function TSPT961AnswerParser.ExtractChnAndParam(pos: int): bool;
var next, c: int;
    s: string;
begin
  Result := false;
  if buf[pos] <> IEC_HT then Exit;
  next := pos;

  if not NextWord(IEC_HT, next, s) then Exit;
  Val(s, FResChn, c);
  if c > 0 then Exit;

  //inc(next);
  if not NextWord(IEC_FF, next, s) then Exit;
  Val(s, FResPrm, c);
  if c > 0 then Exit;

  Result := true;
end;

function TSPT961AnswerParser.ParseHeader(posDLE, posFF: int): bool;
begin
  Result := false;

  // проверим звголовок
  if (posFF - posDLE < 6)
     or (buf[posDLE + 1] <> IEC_SOH)
     or (buf[posDLE + 4] <> IEC_DLE)
     or (buf[posDLE + 5] <> IEC_ISI)
     or (buf[posDLE + 7] <> IEC_DLE)
     or (buf[posDLE + 8] <> IEC_STX) then Exit;

  case buf[posDLE + 6] of
    $03: FResType := logicaAnswerValue;
    //$14: FResType := logicaAnswerIndexValue;
    $16: FResType := logicaAnswerTimeArray;
    else Exit;
  end;

  if not ExtractChnAndParam(posDLE + 9) then Exit;

  FResDevNumber := buf[posDLE + 3];

  Result := true;
end;

function TSPT961AnswerParser.ParseValue(): bool;
var posDLE, posFF, pos: int;
    s: string;
begin
  Result := false;
  posDLE := FindMarker(0, IEC_DLE);
  posFF := FindMarker(posDLE, IEC_FF);

  pos := posFF + 1;
  if not NextWord(IEC_HT, pos, s) then Exit;

  try
    FValue := MyStrToFloat(s);
    Result := FValue <> INCORRECT_VALUE;
  except
  end;
end;

function TSPT961AnswerParser.ParseTimeArrayDT(const s: string): TDateTime;
var dd, mm, yy, hh, nn: int;
begin
  Result := 0;
  if (Length(s) < 14)
     or (s[3] <> '-') or (s[6] <> '-')
     or (s[9] <> '/') or (s[12] <> ':') then Exit;

  dd := StrToInt(s[1] + s[2]);
  mm := StrToInt(s[4] + s[5]);
  yy := StrToInt(s[7] + s[8]);
  hh := StrToInt(s[10] + s[11]);
  nn := StrToInt(s[13] + s[14]);

  Result := EncodeDateTime(2000 + yy, mm, dd, hh, nn, 0, 0);
end;

function TSPT961AnswerParser.ParseTimeArrayString(const str: string): bool;
var n: int;
    value: double;
    dt: TDateTime;
    v: TValueFromBaseItem;
begin
  Result := false;
  if str[1] <> AnsiChar(IEC_HT) then Exit;

  n := 2;
  while (n <= Length(str)) and (str[n] <> AnsiChar(IEC_HT)) do
    inc(n);

  if n > Length(str) then Exit;

  try
    value := MyStrToFloat(Copy(str, 2, n - 2));

    inc(n); // пропустим единицы измерени€
    while (n <= Length(str)) and (str[n] <> AnsiChar(IEC_HT)) do
      inc(n);

    if n > Length(str) then Exit;

    dt := ParseTimeArrayDT(Copy(str, n + 1, Length(str)));
    if dt <= 0 then Exit;

    v := FArchive.Add();
    v.Val.Val := value;
    v.Val.UTime := LocalToUTC(dt);

    Result := value <> INCORRECT_VALUE;
  except
  end;
end;

function TSPT961AnswerParser.ParseTimeArray(): bool;
var posDLE, posFF, pos: int;
    s: string;
begin
  Result := false;
  posDLE := FindMarker(0, IEC_DLE);
  posFF := FindMarker(posDLE, IEC_FF);

  // ѕосле канала надо пропустить две даты
  pos := posFF + 1;
  if not NextWord(IEC_FF, pos, s) then Exit;
  if not NextWord(IEC_FF, pos, s) then Exit;

  Result := true;
  while NextWord(IEC_FF, pos, s) do
  begin
    Result := Result and ParseTimeArrayString(s);
    if not Result then Exit;
  end;
end;

function TSPT961AnswerParser.ParseBody(): bool;
begin
  case FResType of
    logicaAnswerValue: Result := ParseValue();
    logicaAnswerTimeArray: Result := ParseTimeArray();
    else Result := false;
  end;
end;

function TSPT961AnswerParser.Parse(ABuf: ArrayOfByte; ALen: int): bool;
var posDLE, posFF: int;
begin
  Result := false;
  InitResult();

  buf := ABuf;
  Len := ALen;

  if not LogicaSPT961_CheckCRC(buf, Len) then Exit;

  posDLE := FindMarker(0, IEC_DLE);
  if posDLE < 0 then Exit;

  posFF := FindMarker(posDLE, IEC_FF);
  if posFF < 0 then Exit;

  if not ParseHeader(posDLE, posFF) then Exit;

  Result := ParseBody();
end;

function TSPT961AnswerParser.CheckInitialRequest(ReqDetails: TRequestDetails; PrmExtData: string): bool;
var Chn, Prm: int;
begin
  // тип
  Result := ( (ResType = logicaAnswerValue) and (ReqDetails.rqtp = rqtSPT961) )
            or ( (ResType = logicaAnswerTimeArray) and (ReqDetails.rqtp in [rqtSPT961_HOUR, rqtSPT961_DAY]) );

  if not Result then Exit;

  // номер прибора и канал
  Result := TSPT961ReqCreator.ExtractFromExtData(PrmExtData, ReqDetails.rqtp, Chn, Prm);
  if not Result then Exit;

  Result := (FResChn = Chn) and (FResPrm = Prm) and (FResDevNumber = ReqDetails.DevNumber);
end;

constructor TSPT961AnswerParser.Create;
begin
  FArchive := TValuesCollection.Create();
end;

destructor TSPT961AnswerParser.Destroy;
begin
  FArchive.Free();
  inherited;
end;

end.
