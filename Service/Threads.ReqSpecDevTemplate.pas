////////////////////////////////////////////
// Шаблонный класс для опроса приборов через пассивные конвертеры/модемы/порты
////////////////////////////////////////////
unit Threads.ReqSpecDevTemplate;

interface

uses Classes, Windows, SysUtils, GMGlobals, GMConst, ActiveX,
     GMSqlQuery, GM485, GMGenerics, RequestList, Threads.Base, Connection.Base, Devices.ReqCreatorBase;

type
  TRequestListBuilder = class
  private
    FObjType: int;
    FID_Obj: int;
    FAllowLowPriorityRequests: bool;

    q: TGMSqlQuery;
    FReqList: TRequestCollection;
    ReqCreator: T485RequestCreator;

    procedure FillReqDetails;
    procedure AddRequestToSendBuf(ReqDetails: TRequestDetails);
    procedure CreateQuery;
  public
    constructor Create(ObjType: int; ID_Obj: int = 0);
    destructor Destroy; override;
    procedure Build(lst: TRequestCollection);
  end;

  TRequestSpecDevices = class (TGMThread)
  private
    FObjType: int;
    FID_Obj: int;
    FReqID: int;

    FConnectionObject: TConnectionObjectBase;

    ReqListBuilder: TRequestListBuilder;
    FReqList: TRequestCollection;
    FCurrentRequest: TRequestDetails;

    procedure RequestDataFromCOM;
    procedure RequestDataWithPriority(priority: TRequestPriority; count: int);
    function CheckTerminated(): bool;
  protected
    procedure PostGBV(ReqDetails: TRequestDetails; bufRec: array of byte; cnt: int);
    procedure BackGroungProc; virtual;
    procedure LoadCommands(); overload;
    procedure CycleProc();
    procedure DoOneRequest(ri: TSpecDevReqListItem); virtual;
    function ConfigurePort(ri: TSpecDevReqListItem): bool; virtual;
    procedure FreePort(); virtual;
    property CurrentRequest: TRequestDetails read FCurrentRequest;
    function CheckGetAllData: bool; virtual;
    procedure SafeExecute; override;
    procedure CreateConnectionObject(connectionObjectClass: TConnectionObjectClass);
    constructor Create(ObjType: int; ID_Obj: int = 0); overload;
  public
    property ObjType: int read FObjType;
    property ID_Obj: int read FID_Obj;
    destructor Destroy; override;
    property ConnectionObject: TConnectionObjectBase read FConnectionObject;
    procedure AddRequest(reqDetails: TRequestDetails);
    property RequestList: TRequestCollection read FReqList;
    class procedure LoadCommands(ID_Obj, ID_Device, ID_Prm: int; AAddRequestToSendBuf: TGMAddRequestToSendBufProc); overload;
  end;

  TRequestSpecDevicesClass = class of TRequestSpecDevices;

  TRequestDetailsHelper = record helper for TRequestDetails
  public
    procedure InitFromQuery(q: TGMSqlQuery);
  end;

implementation

uses StrUtils, DB, GMBlockValues, ProgramLogFile, Math, Devices.Logica.SPT961, UsefulQueries, EsLogging;

constructor TRequestSpecDevices.Create(ObjType: int; ID_Obj: int = 0);
begin
  inherited Create(false);

  FObjType := ObjType;
  FID_Obj := ID_Obj;
  FReqID := 0;

  FReqList := TRequestCollection.Create();
  ReqListBuilder := TRequestListBuilder.Create(ObjType, ID_Obj);
end;

procedure TRequestSpecDevices.CreateConnectionObject(connectionObjectClass: TConnectionObjectClass);
begin
  if FConnectionObject <> nil then
    TryFreeAndNil(FConnectionObject);

  if connectionObjectClass <> nil then
  begin
    FConnectionObject := connectionObjectClass.Create();
    FConnectionObject.CheckTerminated := CheckTerminated;
    FConnectionObject.ExternalCheckGetAllData := CheckGetAllData;
  end;
end;

destructor TRequestSpecDevices.Destroy;
begin
  FReqList.Free();
  ReqListBuilder.Free();
  TryFreeAndNil(FConnectionObject);

  inherited;
end;

procedure TRequestSpecDevices.PostGBV(ReqDetails: TRequestDetails; bufRec: array of byte; cnt: int);
var gbv: TGeomerBlockValues;
begin
  gbv := TGeomerBlockValues.Create();
  gbv.gbt := gbt485;
  gbv.gmTime := NowGM();
  gbv.ReqDetails := ReqDetails;
  gbv.SetBufRec(bufRec, cnt);

  GMPostMessage(WM_GEOMER_BLOCK, WPARAM(gbv), 0, DefaultLogger);
  DefaultLogger.Debug('WM_GEOMER_BLOCK');
end;

procedure TRequestSpecDevices.DoOneRequest(ri: TSpecDevReqListItem);
var ext: TPrepareRequestBufferInfo;
begin
  if not ConfigurePort(ri) then
    Exit;

  FCurrentRequest := ri.ReqDetails;

  ext.ReqID := FReqID;
  ri.PrepareBuffer(ext);
  FReqID := (FReqID + 1) and $FF;

  WriteBuf(FConnectionObject.buffers.BufSend, 0, ri.ReqDetails.buf, ri.ReqDetails.BufCnt);
  FConnectionObject.buffers.LengthSend := ri.ReqDetails.BufCnt;

  if FConnectionObject.ExchangeBlockData(etSenRec) = ccrBytes then
    PostGBV(ri.ReqDetails, FConnectionObject.buffers.bufRec, FConnectionObject.buffers.NumberOfBytesRead);

  FreePort();
end;

procedure TRequestSpecDevices.FreePort();
begin
  FConnectionObject.FreePort();
end;

procedure TRequestSpecDevices.LoadCommands;
begin
  LoadCommands(fID_Obj, 0, 0, AddRequest);
end;

procedure TRequestSpecDevices.RequestDataWithPriority(priority: TRequestPriority; count: int);
var i: int;
    cnt: int;
begin
  FReqList.SortByPriority();
  cnt := 0;
  for i := 0 to FReqList.Count - 1 do
  begin
    if Terminated then Exit;

    if not FReqList[i].Processed and
       ((priority = rqprNone) or (FReqList[i].ReqDetails.Priority <= priority)) then
    begin
      DoOneRequest(FReqList[i]);
      FReqList[i].Processed := true;
      if count > 0 then
      begin
        inc(cnt);
        if cnt >= count then break
      end;
    end;
  end;
end;

procedure TRequestSpecDevices.RequestDataFromCOM();
begin
  RequestDataWithPriority(rqprCurrents, 0);
end;

procedure TRequestSpecDevices.CycleProc();
begin
  try
    FReqList.ClearProcessed();
    ReqListBuilder.Build(FReqList);
    RequestDataFromCOM();
  except
    on e: Exception do
      ProgramLog().AddException(ClassName() + '.SafeExecute - ' + e.Message);
  end;
end;

procedure TRequestSpecDevices.AddRequest(reqDetails: TRequestDetails);
begin
  FReqList.Add(reqDetails);
end;

class procedure TRequestSpecDevices.LoadCommands(ID_Obj, ID_Device, ID_Prm: int; AAddRequestToSendBuf: TGMAddRequestToSendBufProc);
begin
  try
    ReadFromQuery(
      SqlText_LoadCommands(ID_Obj, ID_Device, ID_Prm),
      procedure(q: TGMSqlQuery)
      var
        channelIds: TChannelIds;
        reqCreator: ISmartPointer<T485RequestCreator>;
      begin
        channelIds := ChannelIdsFromQ(q);
        reqCreator := TSmartPointer<T485RequestCreator>.Create(T485RequestCreator.Create(AAddRequestToSendBuf));
        reqCreator.ReqDetails.InitFromQuery(q);
        reqCreator.ReqDetails.Priority := rqprCommand;
        reqCreator.ReqDetails.rqtp := rqtCommand;

        if not reqCreator.SetChannelValue(channelIds, q.FieldByName('Value').AsFloat, q.FieldByName('TimeHold').AsInteger) then
          SQLReq_SetCmdState(q.FieldByName('ID_Cmd').AsInteger, COMMAND_STATE_ERROR)
        else
          SQLReq_SetCmdState(q.FieldByName('ID_Cmd').AsInteger, COMMAND_STATE_EXECUTED)
      end);
  except
    ProgramLog.AddException(ClassName() + '.LoadCommands');
  end;
end;

procedure TRequestSpecDevices.BackGroungProc;
begin
  FReqList.ClearProcessed();
  LoadCommands();
  RequestDataWithPriority(rqprCommand, 0);
  RequestDataWithPriority(rqprNone, 5); // запрашиваем недозапрошенное в основном цикле
end;

procedure TRequestSpecDevices.SafeExecute;
var tLastReq: int64;
begin
  CoInitialize(nil);

  while not Terminated do
  begin
    tLastReq := GetTickCount();

    CycleProc();

    repeat // опрос раз в минуту
      if Terminated then Exit;
      BackGroungProc();
      SleepThread(2000);
    until Terminated or (Abs(tLastReq - GetTickCount()) >= COMDevicesRequestInterval * 1000);
  end;
end;

function TRequestSpecDevices.CheckGetAllData: bool;
begin
  Result := false;
  case CurrentRequest.ID_DevType of
    DEVTYPE_SPT_961: Result := LogicaSPT961_CheckCRC(FConnectionObject.buffers.BufRec, FConnectionObject.buffers.NumberOfBytesRead);
  end;
end;

function TRequestSpecDevices.CheckTerminated: bool;
begin
  Result := Terminated;
end;

function TRequestSpecDevices.ConfigurePort(ri: TSpecDevReqListItem): bool;
begin
  FConnectionObject.WaitFirst := ri.ReqDetails.TimeOut;
  Result := true;
end;

{ TRequestListBuilder }

procedure TRequestListBuilder.CreateQuery();
begin
  CoInitialize(nil);
  q := TGMSqlQuery.Create();
end;

constructor TRequestListBuilder.Create(ObjType: int; ID_Obj: int = 0);
begin
  inherited Create();
  FID_Obj := ID_Obj;
  FObjType := ObjType;

  CreateQuery();
  ReqCreator := T485RequestCreator.Create(AddRequestToSendBuf);
end;

destructor TRequestListBuilder.Destroy;
begin
  TryFreeAndNil(q);
  TryFreeAndNil(ReqCreator);

  inherited;
end;

procedure TRequestListBuilder.FillReqDetails();
begin
  ReqCreator.ReqDetails.InitFromQuery(q);
end;

procedure TRequestListBuilder.AddRequestToSendBuf(ReqDetails: TRequestDetails);
begin
  if FAllowLowPriorityRequests or (ReqDetails.Priority <= rqprCurrents) then
    FReqList.Add(ReqDetails);
end;

procedure TRequestListBuilder.Build(lst: TRequestCollection);
var action: string;
begin
  FReqList := lst;

  FAllowLowPriorityRequests := FReqList.MaxPriority() <= rqprCurrents; // разрешаем опрос архивов, только когда старые заявки на архивы все обработаны

  ProgramLog.AddMessage(ClassName() + '.ReadRequestListFromSQL');

  try
    q.SQL.Text := ' select * from Devices d join objects o on o.ID_Obj = d.ID_Obj join DevTypes dt on d.ID_DevType = dt.ID_DevType' +
                  '  where (COALESCE(d.Number, 0) > 0 or (COALESCE(d.Number, 0) = 0 and AllowZeroNumber > 0))' +
                  '        and d.ID_Devtype not in (' + SetOfIntCommaList(GeomerFamily) + ')' +
                  IfThen(FID_Obj > 0,
                           ' and d.ID_Obj = ' + IntToStr(FID_Obj),
                           ' and COALESCE(ObjType, 0) = ' + IntToStr(FObjType)) +
                  '        and exists (select * from Params p where p.ID_Device = d.ID_Device and ID_PT > 0)';

    action := 'OpenQuery ' + q.SQL.Text;
    q.Open();

    while not q.Eof do
    begin
      action := 'ReqDetails';
      FillReqDetails();

      action := 'AddDeviceToSendBuf';
      ReqCreator.AddDeviceToSendBuf();

      ProgramLog.AddMessage(ClassName() + '.ReadRequestListFromSQL object Found ID = ' + IntToStr(ReqCreator.ReqDetails.ID_Obj));

      q.Next();
    end;
    q.Close();
  except
    on e: Exception do
      ProgramLog().AddException(ClassName() + '.ReadRequestListFromSQL.' + action + ' - ' + e.Message);
  end;
end;

{ TRequestDetailsHelper }

procedure TRequestDetailsHelper.InitFromQuery(q: TGMSqlQuery);
begin
  Init();
  Priority := rqprCurrents; // кому надо - поменяет по месту
  N_Car := q.FieldByName('N_Car').AsInteger;
  DevNumber := q.FieldByName('Number').AsInteger and $FF; // на всякий случай отрежем старшие байты
  BaudRate := q.FieldByName('BaudRate').AsInteger;
  IPPort := q.FieldByName('RemotePort').AsInteger;
  IP := q.FieldByName('RemoteSrv').AsString;
  Converter := q.FieldByName('Converter').AsInteger;
  ID_Obj := q.FieldByName('ID_Obj').AsInteger;
  ObjType := q.FieldByName('ObjType').AsInteger;
  ID_Device := q.FieldByName('ID_Device').AsInteger;
  ID_DevType := q.FieldByName('ID_DevType').AsInteger;
  if q.Fields.FindField('ID_Prm') <> nil then
    ID_Prm := q.FieldByName('ID_Prm').AsInteger;
end;

end.
