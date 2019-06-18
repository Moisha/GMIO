////////////////////////////////////////////
// Поток выдачи структуры объектов и данных на удаленный сервер
////////////////////////////////////////////
unit Threads.RemoteSrv;

interface

uses Windows, Classes, SysUtils, Math, GMSqlQuery, GMGlobals,
     RecordsForRmSrv, StrUtils, ActiveX, GMConst, Threads.Base,
     Connection.Base, Connection.TCP, StdRequest, StdResponce,
     Generics.Collections, GMGenerics;

type
  TConnectionObjectRemoteSrvXml = class(TConnectionObjectTCP_OwnSocket)
  protected
    function InternalCheckGetAllData: bool; override;
  end;

  TGMRemoteSrvChannel = class
  public
    ID_Prm: int;
    CanReceiveCommand: bool;

    LastCurr,
    LastHour,
    LastDay: LongWORD;

    RemoteLastCurr,
    RemoteLastHour,
    RemoteLastDay: LongWORD;

    UpdateSucceed: bool;

    constructor Create();
  end;

  TGMRemoteSrvDataThreadXml = class(TGMThread)
  private
    FChannels: TGMCollection<TGMRemoteSrvChannel>;
    FRemoteName: string;
    FConnectionObject: TConnectionObjectBase;
    function GetTerminated(): bool;
    function RequestRemoteDataLastVal(): bool;
    procedure ProcessOneChannel_RequestData(src: StdResponce.IXMLSourceType; ut: LongWord);
    procedure ProcessOneChannel_RequestData_OneVal(q: TGMSqlQuery; obj: pointer);
    function RequestRemoteDataLastVal_InitRequest: IXMLGMIORequestType;
    function RequestRemoteDataLastVal_ProcessCurrentReq(req: IXMLGMIORequestType): bool;
    procedure LogChn(chn: TGMRemoteSrvChannel);
  protected
    function ChannelByID(ID_Prm: int): TGMRemoteSrvChannel;
    procedure SafeExecute(); override;
    // virtual - для тестов
    procedure WaitForMainServerReady; virtual;
    procedure RefreshArchList; virtual;
    function SendStructure: bool; virtual;
    function CreateConnectionObject(const AHost: string; APort: int): TConnectionObjectBase; virtual;
    procedure SendArchives_ProcessChannels(); virtual;
    procedure SendArchives; virtual;
  public
    constructor Create(const AName, AHost: string; APort: int);
    destructor Destroy; override;
  end;

implementation

uses ClientResponce, ProgramLogFile, Threads.Pool, RequestThreadsContainer, UsefulQueries;

{ TGMRemoteSrvDataThreadXml }

function TGMRemoteSrvDataThreadXml.CreateConnectionObject(const AHost: string; APort: int): TConnectionObjectBase;
begin
  Result := TConnectionObjectRemoteSrvXml.Create();
  Result.LogPrefix := 'Remote';
  TConnectionObjectRemoteSrvXml(Result).Host := AHost;
  TConnectionObjectRemoteSrvXml(Result).Port := APort;
  Result.CheckTerminated := GetTerminated;
end;

constructor TGMRemoteSrvDataThreadXml.Create(const AName, AHost: string; APort: int);
begin
  inherited Create();

  FRemoteName := AName;
  FConnectionObject := CreateConnectionObject(AHost, APort);
  FChannels := TGMCollection<TGMRemoteSrvChannel>.Create();
end;

destructor TGMRemoteSrvDataThreadXml.Destroy;
begin
  inherited;

  FConnectionObject.Free();
  FChannels.Free();
end;

procedure TGMRemoteSrvDataThreadXml.WaitForMainServerReady();
var req: IXMLGMIORequestType;
    resp: IXMLGMIOResponceType;
begin
  ProgramLog.AddMessage(ClassName() + '.WaitForMainServerReady');
  req := NewGMIORequest();
  req.State.RemoteServerReady := 1;

  while not Terminated do
  begin
    if (FConnectionObject.RequestUniversal(req.XML, AUTH_DEFAULT_LOGIN, true, ruxcRequest, resp) = ccrBytes)
       and (resp.State.RemoteServerReady = 1) then
    begin
      break;
    end;

    SleepThread(2000);
  end;

  req := nil;
end;

function TGMRemoteSrvDataThreadXml.SendStructure(): bool;
var processor: TClientResponce;
    report: IXMLGMIOResponceType;
    resp: IXMLGMIOResponceType;
begin
  ProgramLog.AddMessage(ClassName() + '.SendStructure');
  Result := false;
  processor := TClientResponce.Create(nil);
  try
    try
      report := processor.DBStructure();
      report.Comment := 'RemoteSrv_SendStructure';
      report.Auth.OK := 1;
      report.Auth.RemoteName := FRemoteName;
      report.Auth.RequestID := GenerateGUID();

      if Terminated then Exit;

      if FConnectionObject.RequestUniversal(report.XML, '', false, ruxcResponce, resp) = ccrBytes then
        Result := resp.Auth.OK = 1
      else
        ProgramLog.AddError('TGMRemoteSrvDataThreadXml.SendStructure failed: no responce');
    except
      on e: Exception do
        ProgramLog.AddException('TGMRemoteSrvDataThreadXml.SendStructure - ' + e.Message);
    end;
  finally
    processor.Free();
    report := nil;
  end;
end;

function TGMRemoteSrvDataThreadXml.GetTerminated: bool;
begin
  Result := Terminated;
end;

function TGMRemoteSrvDataThreadXml.ChannelByID(ID_Prm: int): TGMRemoteSrvChannel;
var i: int;
begin
  for i := 0 to FChannels.Count - 1 do
  begin
    if FChannels[i].ID_Prm = ID_Prm then
    begin
      Result := FChannels[i];
      Exit;
    end;
  end;

  Result := FChannels.Add();
  Result.ID_Prm := ID_Prm;
end;

procedure TGMRemoteSrvDataThreadXml.RefreshArchList();
var
  sql: string;
begin
  sql :=      'select p.*, c.UTime as ut_curr,' +
        #13#10'      (select max(UTime) from ArchHour h where h.ID_Prm = p.ID_Prm) as ut_hour,' +
        #13#10'      (select max(UTime) from ArchDay d where d.ID_Prm = p.ID_Prm) as ut_day' +
        #13#10'  from DeviceSystem p left outer join CurrVals c on p.ID_Prm = c.ID_Prm where ID_PT > 0' +
        #13#10'  order by p.ID_Obj, p.ID_Device, p.ID_Src, p.N_Src';
  ReadFromQuery(
    sql,
    procedure (q: TGMSqlQuery)
    var
      chn: TGMRemoteSrvChannel;
    begin
      chn := ChannelByID(q.FieldByName('ID_Prm').AsInteger);
      chn.CanReceiveCommand := RealChnIdSrc(q.FieldByName('ID_Src').AsInteger, q.FieldByName('UserSrc').AsInteger) in [SRC_AO, SRC_DO];
      chn.LastCurr := q.FieldByName('ut_curr').AsInteger;
      chn.LastHour := q.FieldByName('ut_hour').AsInteger;
      chn.LastDay := q.FieldByName('ut_day').AsInteger;
    end);
end;

function TGMRemoteSrvDataThreadXml.RequestRemoteDataLastVal_InitRequest(): IXMLGMIORequestType;
begin
  Result := NewGMIORequest();
  Result.Comment := 'RemoteSrv_RequestRemoteDataLastVal ' + FRemoteName;
  Result.Auth.Login := AUTH_DEFAULT_LOGIN;
  Result.Auth.Password := CalcMD5(AUTH_DEFAULT_PASSWORD);
  Result.Auth.RemoteName := FRemoteName;
end;

function TGMRemoteSrvDataThreadXml.RequestRemoteDataLastVal_ProcessCurrentReq(req: IXMLGMIORequestType): bool;
var
  i: int;
  chn: TGMRemoteSrvChannel;
  resp: IXMLGMIOResponceType;
begin
  for i := 0 to 2 do // на случай, если связь говно, попробуем несколько раз
  begin
    Result := FConnectionObject.RequestUniversal(req.XML, AUTH_DEFAULT_LOGIN, true, ruxcRequest, resp) = ccrBytes;
    if Result then break;
  end;

  if not Result then Exit;

  for i := 0 to req.Data.Sources.Count - 1 do
    ChannelByID(req.Data.Sources[i].Id).UpdateSucceed := true;

  for i := 0 to resp.Data.Count - 1 do
  begin
    chn := ChannelByID(resp.Data[i].Id);
    if resp.Data[i].Count > 0 then
    begin
      case resp.Data[i].ValType of
        DATATYPE_CURRENT_DIAGRAM: chn.RemoteLastCurr := resp.Data[i][0].Utime;
        DATATYPE_HOUR_DIAGRAM: chn.RemoteLastHour := resp.Data[i][0].Utime;
        DATATYPE_DAY_DIAGRAM: chn.RemoteLastDay := resp.Data[i][0].Utime;
        DATATYPE_COMMAND: SQLReq_SetChannel(resp.Data[i].Id, resp.Data[i][0].Val, 'RemoteSrv', 0, resp.Data[i][0].Id);
      end;
    end;
  end;
end;

procedure TGMRemoteSrvDataThreadXml.LogChn(chn: TGMRemoteSrvChannel);
begin
  ProgramLog.AddMessage(ClassName() + Format('ID_Prm = %d, LastCurr = %d, RemoteLastCurr = %d', [chn.ID_Prm, chn.LastCurr, chn.RemoteLastCurr]));
end;

function TGMRemoteSrvDataThreadXml.RequestRemoteDataLastVal(): bool;
var
  req: IXMLGMIORequestType;
  chn: TGMRemoteSrvChannel;

  function AddSrc(valType: int; ut: LongWord): StdRequest.IXMLSourceType;
  begin
    Result := req.Data.Sources.Add();
    Result.Id := chn.ID_Prm;
    Result.ValSrcType := DATACHANNEL_PARAM;
    Result.ValType := valType;
    Result.UTime1 := ut;
    Result.DiagramType := DIAGRAMTYPE_REMOTE;
  end;

  procedure AddChn(chn: TGMRemoteSrvChannel);
  begin
    LogChn(chn);
    if chn.LastCurr > chn.RemoteLastCurr then
      AddSrc(DATATYPE_CURRENT_DIAGRAM, chn.LastCurr);

    if chn.LastHour > chn.RemoteLastHour then
      AddSrc(DATATYPE_HOUR_DIAGRAM, chn.LastHour);

    if chn.LastDay > chn.RemoteLastDay then
      AddSrc(DATATYPE_DAY_DIAGRAM, chn.LastDay);

    if chn.CanReceiveCommand then
      AddSrc(DATATYPE_COMMAND, 0);
  end;

begin
  Result := false;
  ProgramLog.AddMessage(ClassName() + 'RequestRemoteDataLastVal');

  try
    req := RequestRemoteDataLastVal_InitRequest();

    for chn in FChannels do
    begin
      chn.UpdateSucceed := false;
      AddChn(chn);

      if req.Data.Sources.Count >= 20 then
      begin
        ProgramLog.AddMessage(ClassName() + 'RequestRemoteDataLastVal_ProcessCurrentReq cycle');
        if RequestRemoteDataLastVal_ProcessCurrentReq(req) then
          Result := true;

        req := RequestRemoteDataLastVal_InitRequest();
        if Terminated then Exit;
      end;
    end;

    if req.Data.Sources.Count > 0 then
    begin
      ProgramLog.AddMessage(ClassName() + 'RequestRemoteDataLastVal_ProcessCurrentReq last');
      if RequestRemoteDataLastVal_ProcessCurrentReq(req) then
        Result := true;
    end;
  finally
    if not Result then
      ProgramLog.AddError(ClassName() + '.RequestRemoteDataLastVal failed');
  end;
end;

procedure TGMRemoteSrvDataThreadXml.ProcessOneChannel_RequestData_OneVal(q: TGMSqlQuery; obj: pointer);
var src: StdResponce.IXMLSourceType;
    val: IXMLValueType;
begin
  src := StdResponce.IXMLSourceType(obj^);
  val := src.Add();
  val.Utime := q.FieldByName('UTime').AsInteger;
  val.Val := q.FieldByName('Val').AsFloat;
end;

procedure TGMRemoteSrvDataThreadXml.ProcessOneChannel_RequestData(src: StdResponce.IXMLSourceType; ut: LongWord);
var tbl: string;
    sql: string;
begin
  case src.ValType of
    DATATYPE_CURRENT_DIAGRAM: tbl := 'Vals';
    DATATYPE_HOUR_DIAGRAM: tbl := 'ArchHour';
    DATATYPE_DAY_DIAGRAM: tbl := 'ArchDay';
    else Exit;
  end;

  sql := Format('select * from RemoteData_%s(%d, %d) order by UTime', [tbl, src.Id, ut]);
  ReadFromQuery(sql, ProcessOneChannel_RequestData_OneVal, @src);
end;

procedure TGMRemoteSrvDataThreadXml.SendArchives_ProcessChannels();
var chn: TGMRemoteSrvChannel;
    reqData, resp: IXMLGMIOResponceType;
    src: StdResponce.IXMLSourceType;
    n: int;

  function AddSrc(valType: int; ut: LongWord): int;
  begin
    src := reqData.Data.Add();
    src.Id := chn.ID_Prm;
    src.ValSrcType := DATACHANNEL_PARAM;
    src.ValType := valType;
    ProcessOneChannel_RequestData(src, ut);
    Result := src.Count;
    if Result = 0 then
      reqData.Data.Delete(reqData.Data.Count - 1);
  end;

  function AddChanelData(chn: TGMRemoteSrvChannel): int;
  begin
    Result := 0;
    if chn.LastCurr > chn.RemoteLastCurr then
      inc(Result, AddSrc(DATATYPE_CURRENT_DIAGRAM, chn.RemoteLastCurr));

    if chn.LastHour > chn.RemoteLastHour then
      inc(Result, AddSrc(DATATYPE_HOUR_DIAGRAM, chn.RemoteLastHour));

    if chn.LastDay <= chn.RemoteLastDay then
      inc(Result, AddSrc(DATATYPE_DAY_DIAGRAM, chn.RemoteLastDay));
  end;

  procedure CreateNewReport();
  begin
    reqData := NewGMIOResponce();
    reqData.Comment := 'RemoteSrv_ProcessChannels: ';
    reqData.Auth.RemoteName := FRemoteName;
    reqData.Auth.RequestID := GenerateGUID();
  end;

begin
  ProgramLog.AddMessage(ClassName() + '.SendArchives_ProcessChannels');
  n := 0;
  CreateNewReport();
  try
    for chn in FChannels do
    begin
      if not chn.UpdateSucceed then continue;

      if (chn.LastCurr <= chn.RemoteLastCurr)
         and (chn.LastHour <= chn.RemoteLastHour)
         and (chn.LastDay <= chn.RemoteLastDay) then continue;

      ProgramLog.AddMessage(ClassName() + '.SendArchives_ProcessChannels ID_Prm = ' + IntToStr(chn.ID_Prm));

      reqData.Comment := reqData.Comment + IntToStr(chn.ID_Prm) + ' ';
      inc(n, AddChanelData(chn));

      if n >= RemoteSrvMaxDataValuesCount then
      begin
        ProgramLog.AddMessage(ClassName() + '.SendArchives_ProcessChannels RequestUniversal');
        FConnectionObject.RequestUniversal(reqData.XML, '', false, ruxcResponce, resp);
        n := 0;
        CreateNewReport();
      end;

      if Terminated then Exit;
    end;

    if n > 0 then
    begin
      ProgramLog.AddMessage(ClassName() + '.SendArchives_ProcessChannels RequestUniversal');
      FConnectionObject.RequestUniversal(reqData.XML, '', false, ruxcResponce, resp);
    end;
  finally
    reqData := nil;
  end;
end;

procedure TGMRemoteSrvDataThreadXml.SendArchives();
begin
  ProgramLog.AddMessage(ClassName() + '.SendArchives');
  RefreshArchList();
  if Terminated then Exit;

  if RequestRemoteDataLastVal() then
    SendArchives_ProcessChannels();
end;

procedure TGMRemoteSrvDataThreadXml.SafeExecute;
begin
  ProgramLog.AddMessage(ClassName() + '.SafeExecute');
  CoInitialize(nil);
  while not Terminated do
  begin
    try
      WaitForMainServerReady();
      if Terminated then Exit;

      if SendStructure() then
      begin
        WaitForMainServerReady();
        if Terminated then Exit;

        SendArchives();
      end;

    except
      on e: Exception do
        ProgramLog.AddException('TGMRemoteSrvDataThreadXml.SafeExecute - ' + e.Message);
    end;
    SleepThread(1000);
  end;
end;

{ TConnectionObjectRemoteSrvXml }

function TConnectionObjectRemoteSrvXml.InternalCheckGetAllData: bool;
var len: int;
begin
  Result := buffers.CheckBufRecHeader(250, 10, 6) or buffers.CheckBufRecHeader(255, 0, 6);
  if not Result then Exit;

  len := ReadUINT(buffers.BufRec, 2);
  Result := buffers.NumberOfBytesRead >= len + 6;
end;

{ TGMRemoteSrvChannel }

constructor TGMRemoteSrvChannel.Create;
begin
  ID_Prm := 0;
  CanReceiveCommand := false;

  LastCurr := 0;
  LastHour := 0;
  LastDay := 0;

  RemoteLastCurr := 0;
  RemoteLastHour := 0;
  RemoteLastDay := 0;

  UpdateSucceed := false;
end;

end.
