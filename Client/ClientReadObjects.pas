////////////////////////////////////////////
// Поток для считывания стуктуры объектов с сервера
////////////////////////////////////////////
unit ClientReadObjects;

interface

uses Windows, Classes, SysUtils, GMGlobals, GMConst, Threads.Base, Threads.GMClient, StdResponce,
     GMDBClasses;

type TClientReadObjectsThread = class (TGMThread)
  private
    function ExecReadStructureOrder(nRetry: int = 3): IXMLGMIOResponceType;
    function ReadObjectsFromSrvUniversal: int;
    function TryExecReadStructureOrder(): IXMLGMIOResponceType;
    procedure ReadPT(resp: IXMLGMIOResponceType);
    procedure ReadObjects(resp: IXMLGMIOResponceType);
    procedure ReadDevices(devices: IXMLDevicesType; obj: TGMObject);
    procedure ReadChannels(channels: IXMLChannelsType; dev: TGMDevice);
    procedure InitDB;
    procedure ReadAggregates(aggregates: IXMLAggregatesType; obj: TGMObject);
    procedure ReadNodes(resp: IXMLGMIOResponceType);
  protected
    procedure SafeExecute; override;
  end;

implementation

uses ActiveX, EsLogging;

function TClientReadObjectsThread.ExecReadStructureOrder(nRetry: int = 3): IXMLGMIOResponceType;
var i: int;
begin
  Result := nil;
  for i := 0 to nRetry do
  begin
    Result := TryExecReadStructureOrder();
    if (Result <> nil) or Terminated then Exit;
  end;
end;

function TClientReadObjectsThread.TryExecReadStructureOrder(): IXMLGMIOResponceType;
var order: TDataOrder;
begin
  Result := nil;

  order := TDataOrders.Add();
  order.OrderType := dotUniversal;
  order.req.Structure := 1;
  order.State := dosWaiting;
  order.WaitFor(60000);

  if Terminated then Exit;

  if order.State = dosDone then
    Result := order.resp;

  order.State := dosDelete;
end;

procedure TClientReadObjectsThread.ReadPT(resp: IXMLGMIOResponceType);
var i: int;
    pt: IXMLParamType;
begin
  for i := 0 to resp.ParamTypes.Count - 1 do
  begin
    pt := resp.ParamTypes[i];
    GMParamTypes.Add(pt.ID_PT, pt.PSign, pt.PName, pt.IsAlarm > 0);
  end;
end;

procedure TClientReadObjectsThread.ReadChannels(channels: IXMLChannelsType; dev: TGMDevice);
var chn, baseChn: TGMParam;
    i: int;
    pt: TGMParamType;
begin
  for i := 0 to channels.Count - 1 do
  begin
    pt := GMParamTypes.ParamTypeByID(channels[i].ID_PT);
    baseChn := GMParams.ByID(channels[i].BaseChn);

    if ( (channels[i].BaseChn > 0) or (pt <> nil) ) // если канал имеет базовый, то тип ему не обязателен
       and ( (channels[i].BaseChn <= 0) or (baseChn <> nil) ) then // если канал имеет базовый, то базовый обязан быть кошерным
    begin
      chn := GMParams.Add();
      chn.ID_Prm := channels[i].Id;
      chn.PType := pt;
      chn.Device := dev;
      chn.ID_Src := channels[i].ID_Src;
      chn.N_Src := channels[i].Nsrc;
      chn.AlarmSignal := channels[i].AlarmSignal;
      chn.AlarmThreshold := MyStrToFloatDef(channels[i].AlarmThreshold, 0);
      // chn.MeaUnitName := channels[i].ID_MeaUnit;
      chn.PrmName := channels[i].Name;
      chn.BaseChn := baseChn;
    end;
  end;
end;

procedure TClientReadObjectsThread.ReadDevices(devices: IXMLDevicesType; obj: TGMObject);
var dev: TGMDevice;
    i: int;
begin
  for i := 0 to devices.Count - 1 do
  begin
    dev := GMDevices.Add();

    dev.Obj := obj;
    dev.ID_Device := devices[i].Id;
    dev.ID_DevType := devices[i].ID_DevType;
    dev.Number := devices[i].Number;
    dev.TypeName := devices[i].DevType;
    dev.DevName := devices[i].Name;

    ReadChannels(devices[i].Channels, dev);
  end;
end;

procedure TClientReadObjectsThread.ReadAggregates(aggregates: IXMLAggregatesType; obj: TGMObject);
var i, j: int;
    aggr: TGMAggregate;
    prm: TGMParam;
    aggrprm: TGMAggregateParam;
begin
  for i := 0 to aggregates.Count - 1 do
  begin
    aggr := GMAggregates.AggregateByID(aggregates[i].Id);
    if aggr = nil then
      aggr := GMAggregates.Add();

    aggr.ID_Aggregate := aggregates[i].Id;
    aggr.Obj := obj;
    aggr.AggrType := aggregates[i].ID_AggrType;
    aggr.AggrName := aggregates[i].Name;

    for j := 0 to aggregates[i].AggregateParams.Count - 1 do
    begin
      prm := GMParams.ByID(aggregates[i].AggregateParams[j].ID_Prm);
      if prm <> nil then
      begin
        aggrprm := TGMAggregateParam.Create();
        aggrprm.prm := prm;
        aggrprm.PrmKind := aggregates[i].AggregateParams[j].PrmKind;
        aggrprm.Caption := aggregates[i].AggregateParams[j].Caption;
        aggr.ParamList.Add(aggrprm);
      end;
    end;
  end;
end;

procedure TClientReadObjectsThread.ReadObjects(resp: IXMLGMIOResponceType);
var i: int;
    obj: TGMObject;
begin
  for i := 0 to resp.Objects.Count - 1 do
  begin
    obj := GMObjects.Add();
    obj.ID_Obj := resp.Objects[i].Id;
    obj.Name := resp.Objects[i].Name;
    obj.N_Car := resp.Objects[i].Ncar;

    ReadDevices(resp.Objects[i].Devices, obj);
    ReadAggregates(resp.Objects[i].Aggregates, obj);
  end;
end;

procedure TClientReadObjectsThread.ReadNodes(resp: IXMLGMIOResponceType);
var node: TGMNode;
    chn: TGMNodeChannel;
    i, j: int;
begin
  for i := 0 to resp.Nodes.Count - 1 do
  begin
    node := GMNodes.Add();
    node.ID_Node := resp.Nodes[i].Id;
    node.ID_Parent := resp.Nodes[i].Parent;
    node.Name := resp.Nodes[i].Name;
    node.NodeType := resp.Nodes[i].NodeType;

    for j := 0 to resp.Nodes[i].NodeChannels.Count - 1 do
    begin
      chn := GMNodeChannels.Add();
      chn.ID_Prm := resp.Nodes[i].NodeChannels[j].Id;
      chn.Node := node;
      chn.Name := resp.Nodes[i].NodeChannels[j].Name;
      chn.BaseChn := GMParams.ByID(resp.Nodes[i].NodeChannels[j].BaseChn);
      chn.PType := GMParamTypes.ParamTypeByID(resp.Nodes[i].NodeChannels[j].ID_PT);
    end;
  end;

  for i := 0 to GMNodes.Count - 1 do
    GMNodes[i].Parent := GMNodes.NodeById(GMNodes[i].ID_Parent);
end;

procedure TClientReadObjectsThread.InitDB();
begin
  GMAggregates.Clear();
  GMParams.Clear();
  GMDevices.Clear();
  GMObjects.Clear();
  GMParamTypes.Prepare();
  GMNodeChannels.Clear();
  GMNodes.Clear()
end;

function TClientReadObjectsThread.ReadObjectsFromSrvUniversal: int;
var
  resp: IXMLGMIOResponceType;
begin
  DefaultLogger.Info('ReadObjectsFromSrv - req');
  resp := ExecReadStructureOrder();
  try
    if resp = nil then
    begin
      DefaultLogger.Error('ReadObjectsFromSrv - connection failed');
      Exit(RESULT_CLIENT_CREATE_OBJECTS_CONNECTION_FAILED);
    end;

    if Resp.Auth.OK = 0 then
    begin
      DefaultLogger.Error('ReadObjectsFromSrv - auth failed');
      Exit(RESULT_CLIENT_CREATE_OBJECTS_LOGIN_FAILED);
    end;

    DefaultLogger.Info('ReadObjectsFromSrv - req done');
    Result := RESULT_CLIENT_CREATE_OBJECTS_OK;

    InitDB();
    ReadPT(resp);
    ReadObjects(resp);
    ReadNodes(resp);
  finally
    DefaultLogger.Info('ReadObjectsFromSrv done');
  end;
end;

{ TClientReadObjects }

procedure TClientReadObjectsThread.SafeExecute;
var res: int;
begin
  CoInitialize(nil);
  while not Terminated do
  begin
    res := ReadObjectsFromSrvUniversal();
    GMPostMessage(WM_CLIENT_CREATE_OBJECTS, res, 0, DefaultLogger);

    if res = RESULT_CLIENT_CREATE_OBJECTS_OK then
      break;

    Sleep(1000);
  end;
end;

end.
