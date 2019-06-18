////////////////////////////////////////////
// Кодировка и расчет CRC для приборов типа ТЭКОН
////////////////////////////////////////////
unit Devices.Tecon;

interface

uses Windows, GMGlobals, GMConst, Devices.ReqCreatorBase, GmSqlQuery, SysUtils, DateUtils,
     ArchIntervalBuilder, Generics.Collections, Devices.Tecon.Common;

type
  TTecon19ReqCreator = class(TDevReqCreator)
  private
    FNeedSetTime: bool;
    procedure AddTecon19AI(N_Src: int; ExtData: string);
    procedure AddTecon19CNT(N_Src: int);
    procedure AddTecon19DI(N_Src: int);
    procedure AddTecon19OneParam(NPrm: int);
    procedure AddTecon19SetTime;
    procedure Tecon19PasteBuffer(buf: array of byte; Len: int; rqtp: T485RequestType);
    procedure CheckNeedSetTime;
    procedure AddCurrents;
    procedure AddArchives;
    procedure AddSetTimeCmd;
    procedure AddCurrents_OnePrm(q: TGMSqlQuery; obj: pointer);
    procedure AddArchives_OnePrm(q: TGMSqlQuery; obj: pointer);
  protected
     function GetNeedSetTime: bool; virtual;
     procedure AddOneArchiveRequest(ChnAddr, timeIndex, count: int);
     function HourArrayTimeIndex(utime: TUtcDateTime; archDepthDays: int): int;
  public
    procedure AddRequests(); override;
    constructor Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails); override;
  end;

  TTecon19AnswerParser = class
  private
    FVals: TList<TValueFromBase>;
    FReqDetails: TRequestDetails;
    FBuf: ArrayOfByte;
    FLen: int;

    procedure ReadCurrents();
    procedure ReadHours();
    function ExtractCurrentValue(): double;
    function ExtractArchValue(n: int): double;
  public
    function Parse(buf: ArrayOfByte; len: int; reqDetails: TRequestDetails): bool;
    property Vals: TList<TValueFromBase> read FVals;
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses ProgramLogFile, Math;

{ TTecon19ReqCreator }

procedure TTecon19ReqCreator.AddTecon19OneParam(NPrm: int);
var buf: array [0..20] of byte;
begin
  buf[0] := $10;
  buf[1] := $40 + FReqDetails.ReqID; // сюда бы как-то передать номер пакета, чтобы в них, пакетах, не запутаться
  if FReqDetails.ObjType in [OBJ_TYPE_K104, OBJ_TYPE_K105] then
  begin
    buf[2] := FReqDetails.N_Car and $FF;
    buf[3] := $11;
    buf[4] := FReqDetails.DevNumber and $FF;
    buf[5] := NPrm mod 256;
    buf[6] := (NPrm div 256) and $FF;
  end
  else
  begin
    buf[2] := $01;
    buf[3] := FReqDetails.DevNumber and $FF;
    buf[4] := NPrm mod 256;
    buf[5] := (NPrm div 256) and $FF;
    buf[6] := 0;
  end;

  Tecon19PasteBuffer(buf, 7, rqtUSER_DEFINED);
end;

procedure TTecon19ReqCreator.AddTecon19AI(N_Src: int; ExtData: string);
var NPrm: int;
begin
  if ExtData = 'R' then
  begin
    case N_Src of
      0..3: NPrm := $0404 + N_Src;
      4: NPrm := $0418;
      else Exit;
    end;
  end
  else if ExtData = 'I' then
  begin
    case N_Src of
      0..3: NPrm := $0400 + N_Src;
      4..9: NPrm := $0420 + (N_Src - 4);
      else Exit;
    end;
  end
  else
    Exit;

  AddTecon19OneParam(NPrm);
end;

procedure TTecon19ReqCreator.AddTecon19DI(N_Src: int);
var NPrm: int;
begin
  case N_Src of
    0..7: NPrm := $0506 + N_Src;
    else Exit;
  end;

  AddTecon19OneParam(NPrm);
end;

procedure TTecon19ReqCreator.AddTecon19CNT(N_Src: int);
var NPrm: int;
begin
  case N_Src of
    0..7: NPrm := $0208 + N_Src;
    else Exit;
  end;

  AddTecon19OneParam(NPrm);
end;

procedure TTecon19ReqCreator.Tecon19PasteBuffer(buf: array of byte; Len: int; rqtp: T485RequestType);
begin
  Tecon_CRC(buf, Len);
  AddBufRequestToSendBuf(buf, Len + 2, rqtp);
  FReqDetails.ReqID := (FReqDetails.ReqID + 1) and $0F;
end;

procedure TTecon19ReqCreator.AddTecon19SetTime();
var buf: array [0..20] of byte;
    dt: TDateTime;
begin
  dt := Now() + OneSecond; // секунда про запас
  if FReqDetails.ObjType in [OBJ_TYPE_K104, OBJ_TYPE_K105] then
  begin
    // уровень доступа
    buf[0] := $68;
    buf[1] := $08;
    buf[2] := $08;
    buf[3] := $68;
    buf[4] := $40 + FReqDetails.ReqID;
    buf[5] := FReqDetails.N_Car and $FF;
    buf[6] := $14;

    buf[7] := $03;
    buf[8] := FReqDetails.DevNumber and $FF;
    buf[9] := $05;
    buf[10] := $02;

    Tecon19PasteBuffer(buf, 11, rqtUSER_DEFINED);

    // время
    buf[0] := $68;
    buf[1] := $0D;
    buf[2] := $0D;
    buf[3] := $68;
    buf[4] := $40 + FReqDetails.ReqID;
    buf[5] := FReqDetails.N_Car and $FF;
    buf[6] := $14;

    buf[7] := $08;
    buf[8] := FReqDetails.DevNumber and $FF;
    buf[9] := $03;
    buf[10] := $18;
    buf[11] := $F0;
    buf[12] := $00;
    buf[13] := DecToHexDigits(SecondOf(dt));
    buf[14] := DecToHexDigits(MinuteOf(dt));
    buf[15] := DecToHexDigits(HourOf(dt));

    Tecon19PasteBuffer(buf, 16, rqtTECON_SET_TIME);
  end
  else
  begin
    // уровень доступа
    buf[0] := $10;
    buf[1] := $40 + FReqDetails.ReqID;
    buf[2] := FReqDetails.N_Car and $FF;
    buf[3] := $17;
    buf[4] := $02;
    buf[5] := $00;
    buf[6] := $00;

    Tecon19PasteBuffer(buf, 7, rqtUSER_DEFINED);

    // время
    buf[0] := $68;
    buf[1] := $09;
    buf[2] := $09;
    buf[3] := $68;
    buf[4] := $40 + FReqDetails.ReqID;
    buf[5] := FReqDetails.N_Car and $FF;
    buf[6] := $05;
    buf[7] := $18;
    buf[8] := $F0;
    buf[9] := $00;
    buf[10] := DecToHexDigits(SecondOf(dt));
    buf[11] := DecToHexDigits(MinuteOf(dt));
    buf[12] := DecToHexDigits(HourOf(dt));

    Tecon19PasteBuffer(buf, 13, rqtTECON_SET_TIME);
  end;
end;

procedure TTecon19ReqCreator.CheckNeedSetTime();
var s: string;
begin
  s := QueryResult('select dt_updated from Devices where ID_Device = ' + IntToStr(FReqDetails.ID_Device));
  // время взбадриваем раз в сутки, подальше от их границы
  FNeedSetTime := (HourOf(Now) in [10..20]) and (Abs(int64(NowGM()) - StrToInt64Def(s, 0)) > 3600 * 24);
end;

constructor TTecon19ReqCreator.Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails);
begin
  inherited;
  CheckNeedSetTime();
end;

function TTecon19ReqCreator.GetNeedSetTime: bool;
begin
  Result := FNeedSetTime;
end;

procedure TTecon19ReqCreator.AddCurrents_OnePrm(q: TGMSqlQuery; obj: pointer);
var ID_Src, N_Src: int;
begin
  N_Src := q.FieldByName('N_Src').AsInteger;
  ID_Src := q.FieldByName('ID_Src').AsInteger;

  FReqDetails.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
  case ID_Src of
    SRC_AI: AddTecon19AI(N_Src, q.FieldByName('ExtData').AsString);
    SRC_DI: AddTecon19DI(N_Src);
    SRC_CNT_DI: AddTecon19CNT(N_Src);
    SRC_USR: AddTecon19OneParam(q.FieldByName('CurrentsAddr').AsInteger);
  end;
end;

procedure TTecon19ReqCreator.AddCurrents;
begin
  ReadFromQuery('select * from Params ' +
                '  where ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and ID_PT > 0 and (N_Src > 0 or coalesce(CurrentsAddr, 0) > 0) ' +
                '    order by ID_Src, N_Src',
                AddCurrents_OnePrm);
end;

procedure TTecon19ReqCreator.AddOneArchiveRequest(ChnAddr, timeIndex, count: int);
var buf: array [0..20] of byte;
begin
  if not FReqDetails.ObjType in [OBJ_TYPE_K104, OBJ_TYPE_K105] then Exit; // напрямки пока не надо

  buf[0] := $68;
  buf[1] := $09;
  buf[2] := $09;
  buf[3] := $68;
  buf[4] := $40 + FReqDetails.ReqID;
  buf[5] := FReqDetails.N_Car and $FF;
  buf[6] := $19;
  buf[7] := FReqDetails.DevNumber and $FF;
  buf[8] := ChnAddr mod 256;
  buf[9] := (ChnAddr div 256) and $FF;
  buf[10] := timeIndex mod 256;
  buf[11] := (timeIndex div 256) and $FF;
  buf[12] := count;

  Tecon19PasteBuffer(buf, 13, rqtTECON_HourArch);
end;

function TTecon19ReqCreator.HourArrayTimeIndex(utime: TUtcDateTime; archDepthDays: int): int;
var utOurZone: int64;
begin
  // Тэкон живет в своем (ну и нашем заодним) часовом поясе.
  // поэтому дату по Гринвичу надо привести в наш пояс
  utOurZone := int64(utime) + (TimeZone * UTC_HOUR);

  // Индекс архива Тэкон - это номер часа в последнем блоке из archDepthDays суток часовых архивов
  // Отсчет блоков начинается с 01.01.2000
  // TUtcTime - это к-во секунд, прошедшее с 01.01.2000
  // т.е. нам надо взять к-во часов с начала последнего блока
  Result := utOurZone mod (archDepthDays * UTC_DAY) div UTC_HOUR;
end;

procedure TTecon19ReqCreator.AddArchives_OnePrm(q: TGMSqlQuery; obj: pointer);
var lst: TList<ArchReqInterval>;
    a: ArchReqInterval;
    intrvBuilder: TArchIntervalBuilder;
    sql: string;
    depth: int;
    udtStart: TUtcDateTime;
begin
  depth := q.FieldByName('HourArchArgument').AsInteger;
  if not depth in [16, 32, 64] then Exit;

  FReqDetails.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
  udtStart := (int64(NowGM()) - (depth - 1) * UTC_DAY) div UTC_HOUR * UTC_HOUR;
  sql := Format('select UTime from ArchHourRequestStartDate(%d, %d) order by 1',
                [FReqDetails.ID_Prm, udtStart]);

  intrvBuilder := TArchIntervalBuilder.Create(sql);
  try
    lst := intrvBuilder.BuildIntervalList(UTC_HOUR, 1);
    try
      for a in lst do
      begin
        FReqDetails.UTimeArch := a.UTime1;
        FReqDetails.Priority := rqprArchives;
        AddOneArchiveRequest(q.FieldByName('HourArchAddr').AsInteger,
                             HourArrayTimeIndex(a.UTime1, depth),
                             (a.UTime2 - a.UTime1) div UTC_HOUR + 1);
      end;
    finally
      lst.Free();
    end;
  finally
    intrvBuilder.Free();
  end;
end;

procedure TTecon19ReqCreator.AddArchives;
begin
  ReadFromQuery('select * from Params ' +
                '  where ID_Device = ' + IntToStr(FReqDetails.ID_Device) +
                '        and ID_PT > 0 ' +
                '        and coalesce(HourArchArgument, 0) > 0 ' +
                '        and coalesce(HourArchAddr, 0) > 0 ' +
                '  order by ID_Src, N_Src',
                AddArchives_OnePrm);
end;

procedure TTecon19ReqCreator.AddSetTimeCmd;
begin
    if GetNeedSetTime() then
    begin
      AddTecon19SetTime();
      ExecSQL('update Devices set dt_updated = NowGM() where ID_Device = ' + IntToStr(FReqDetails.ID_Device));
    end;
end;

procedure TTecon19ReqCreator.AddRequests;
begin
  FReqDetails.ReqID := 0;
  AddCurrents();
  AddSetTimeCmd();
  AddArchives();
end;

{ TTecon19AnswerParser }

constructor TTecon19AnswerParser.Create;
begin
  inherited;
  FVals := TList<TValueFromBase>.Create();
end;

destructor TTecon19AnswerParser.Destroy;
begin
  FVals.Free();
  inherited;
end;

function TTecon19AnswerParser.ExtractCurrentValue(): double;
begin
  Result := 0;

  if Tecon_CheckCurrentsPrmReply(FBuf, FLen) then
  begin
    case FBuf[1] of
      $03: Result := FBuf[6];
      $04: Result := ReadWORDInv(FBuf, 6);
      $06: Result := ReadSingle(FBuf, 6);
    end;
  end
  else
  if Tecon_CheckCurrentsPrmReplyCOM(FBuf, FLen) then
  begin
    case FLen - 5 of
      $01: Result := FBuf[6];
      $02: Result := ReadWORDInv(FBuf, 6);
      $04: Result := ReadSingle(FBuf, 6);
    end;
  end;
end;

function TTecon19AnswerParser.ExtractArchValue(n: int): double;
begin
  Result := ReadSingle(FBuf, 6 + 4 * n);
end;

procedure TTecon19AnswerParser.ReadCurrents();
var v: TValueFromBase;
begin
  v.UTime := NowGM();
  v.Val := ExtractCurrentValue();
  v.ValType := valueTypeCurrent;
  v.Chn := nil;

  if not IsNan(v.Val) then
    FVals.Add(v);
end;

procedure TTecon19AnswerParser.ReadHours();
var v: TValueFromBase;
    i, n: int;
begin
  n := (FBuf[1] - 2) div 4;

  for i := 0 to n - 1 do
  begin
    v.UTime := int64(FReqDetails.UTimeArch) + i * UTC_HOUR;
    v.Val := ExtractArchValue(i);
    v.ValType := valueTypeHourArch;
    v.Chn := nil;

    if not IsNan(v.Val) then
      FVals.Add(v);
  end;
end;

function TTecon19AnswerParser.Parse(buf: ArrayOfByte; len: int; reqDetails: TRequestDetails): bool;
begin
  FVals.Clear();
  FBuf := buf;
  FLen := len;
  FReqDetails := reqDetails;

  Result := false;
  if not Tecon_CheckCRC(buf, len) then Exit;

  // структура
  case reqDetails.rqtp of
    rqtUSER_DEFINED: if not Tecon_CheckCurrentsPrmReply(FBuf, FLen) then Exit;
    rqtTECON_HourArch: if not Tecon_CheckArchPrmReply(FBuf, FLen) then Exit;
    else Exit;
  end;

  // ID запроса
  if buf[4] <> reqDetails.ReqID then Exit;

  // номер К104/105
  if buf[5] <> reqDetails.N_Car then Exit;

  Result := true;

  case reqDetails.rqtp of
    rqtUSER_DEFINED: ReadCurrents();
    rqtTECON_HourArch: ReadHours();
  end;
end;

end.
