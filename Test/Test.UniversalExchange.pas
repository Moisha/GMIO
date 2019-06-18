unit Test.UniversalExchange;

interface

uses Windows, TestFrameWork, GMGlobals, GMSocket, Classes,
     GMSQLQuery, AppConfigFile, StdRequest, StdResponce,
     ConnParamsStorage, ClientResponce, GeomerLastValue;

type
  TUniversalTransponderTest = class(TTestCase)
  private
    transponder: TClientResponce;
    function Sources_AddPrm(id_prm, ValSrcType, ValType: int): StdRequest.IXMLSourceType;
    procedure CheckRequest;
    function Diagram_PrepareSource(src: StdRequest.IXMLSourceType): StdRequest.IXMLSourceType;
  protected
    req: IXMLGMIORequestType;
    resp: IXMLGMIOResponceType;
    glvBuffer: TGeomerLastValuesBuffer;

    procedure DoRequest; virtual;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckUser();
    procedure Structure();
    procedure Currents();
    procedure CurrDiagram();
    procedure CurrDiagramDay;
    procedure HourDiagram();
    procedure DayDiagram();
    procedure Commands();
  end;

  TXMLTest = class(TTestCase)
  published
    procedure Responce_Commands;
  end;

implementation

uses Forms, DateUtils, SysUtils, GMConst, WinSock, Math, UsefulQueries;

{ TGMSocketTest }

procedure TUniversalTransponderTest.CheckUser;
var
  userId: int;
begin
  Check(transponder.CheckAuthorization(CalcMD5(LowerCase('UserInGroup')), CalcMD5('789'), userId), 'UserInGroup');
  Check(userId = 4, 'UserInGroup');
  Check(transponder.CheckAuthorization(CalcMD5(LowerCase('admin')), CalcMD5('456'), userId), 'admin good');
  Check(userId = 2, 'admin good');
  Check(not transponder.CheckAuthorization(CalcMD5(LowerCase('admin')), CalcMD5('dwedf'), userId), 'admin bad');
  Check(userId = 0, 'admin bad');
end;

procedure TUniversalTransponderTest.Commands;
var
  id_cmd, id_prm_remote: int;
  src: StdRequest.IXMLSourceType;
begin
  id_prm_remote := SQLReq_GetIdPrmForRemotePrmId('TEST', 68);
  id_cmd := SQLReq_SetChannel(id_prm_remote, 100.001);
  req.Auth.RemoteName := 'TEST';
  src := req.Data.Sources.Add();
  src.Id := 68;
  src.ValType := DATATYPE_COMMAND;

  DoRequest();

  CheckEquals(1, resp.Data.Count);
  CheckEquals(src.Id, resp.Data[0].Id);
  Check(CompareValue(100.001, resp.Data[0][0].Val) = 0);
  CheckEquals(id_cmd, resp.Data[0][0].Id);
end;

function TUniversalTransponderTest.Sources_AddPrm(id_prm, ValSrcType, ValType: int): StdRequest.IXMLSourceType;
begin
  Result := req.Data.Sources.Add();
  Result.Id := id_prm;
  Result.ValSrcType := ValSrcType;
  Result.ValType := ValType;
end;

procedure TUniversalTransponderTest.DayDiagram;
var src: StdRequest.IXMLSourceType;
begin
  src := Sources_AddPrm(1, DATACHANNEL_PARAM, DATATYPE_DAY_DIAGRAM);
  src.AggrIntervalLen := 60 * 24;
  Diagram_PrepareSource(src);

  DoRequest();
  CheckRequest();

  Check(resp.Data.Count = 1, 'resp.Data.Count');
  Check(resp.Data[0].Count = 1, 'resp.Data[0].Count');
  Check(resp.Data[0][0].Val = 1, 'resp.Data[0][0]');
  Check(resp.Data[0][0].Utime = int64(src.UTime1), 'resp.Data[0][0]');
end;

function TUniversalTransponderTest.Diagram_PrepareSource(src: StdRequest.IXMLSourceType): StdRequest.IXMLSourceType;
begin
  src.UTime1 := LocalToUTC(Floor(Now()) - 1);
  src.UTime2 := LocalToUTC(Floor(Now()));
  src.AggrIntervalLen := 1;
  Result := src;
end;

procedure TUniversalTransponderTest.CurrDiagram;
begin
  Diagram_PrepareSource(Sources_AddPrm(1, DATACHANNEL_PARAM, DATATYPE_CURRENT_DIAGRAM));
  Diagram_PrepareSource(Sources_AddPrm(1, DATACHANNEL_NODECHANNEL, DATATYPE_CURRENT_DIAGRAM));

  DoRequest();
  CheckRequest();

  Check(resp.Data.Count = 2, 'resp.Data.Count');
  Check(resp.Data[0].Count = 24 * 60, 'resp.Data[0].Count');
  Check(resp.Data[1].Count = 24 * 60, 'resp.Data[1].Count');
end;

function AggrDay(src: StdRequest.IXMLSourceType): StdRequest.IXMLSourceType;
begin
  Result := src;
  Result.AggrIntervalLen := 24 * 60;
  Result.AggrType := DATA_AGGREGATION_TYPE_DEFAULT;
end;

procedure TUniversalTransponderTest.CurrDiagramDay;
begin
  AggrDay(Diagram_PrepareSource(Sources_AddPrm(1, DATACHANNEL_PARAM, DATATYPE_CURRENT_DIAGRAM)));
  AggrDay(Diagram_PrepareSource(Sources_AddPrm(1, DATACHANNEL_NODECHANNEL, DATATYPE_CURRENT_DIAGRAM)));

  DoRequest();
  CheckRequest();

  Check(resp.Data.Count = 2, 'resp.Data.Count');
  Check(resp.Data[0].Count = 1, 'resp.Data[0].Count');
  Check(resp.Data[1].Count = 1, 'resp.Data[1].Count');
end;

procedure TUniversalTransponderTest.Currents;
begin
  Sources_AddPrm(1, DATACHANNEL_PARAM, DATATYPE_CURRENTS);
  Sources_AddPrm(2, DATACHANNEL_PARAM, DATATYPE_CURRENTS);
  Sources_AddPrm(3, DATACHANNEL_PARAM, DATATYPE_CURRENTS);
  Sources_AddPrm(4, DATACHANNEL_PARAM, DATATYPE_CURRENTS);

  Sources_AddPrm(1, DATACHANNEL_NODECHANNEL, DATATYPE_CURRENTS);
  Sources_AddPrm(2, DATACHANNEL_NODECHANNEL, DATATYPE_CURRENTS);

  DoRequest();
  CheckRequest();

  Check(resp.Data.Count = 6, 'resp.Data.Count');

  // параметры
  Check(resp.Data[0].Value[0].Val = 10, 'Value 1');
  Check(resp.Data[1].Value[0].Val = 20, 'Value 2');
  Check(resp.Data[2].Value[0].Val = 30, 'Value 3');
  Check(resp.Data[3].Value[0].Val = 80, 'Value 4');

  // каналы
  Check(resp.Data[4].Value[0].Val = 30, 'Value 5');
  Check(resp.Data[5].Value[0].Val = 80, 'Value 6');
end;

procedure TUniversalTransponderTest.CheckRequest();
begin
  Check(resp <> nil, 'resp <> nil');
  Check(resp.Auth.OK = 1, 'Auth failed');
end;

procedure TUniversalTransponderTest.DoRequest();
begin
  req.Auth.Login := 'user';
  req.Auth.Password := CalcMD5('123');
  resp := transponder.ProcessRequest(req, CalcMd5('user'));
end;

procedure TUniversalTransponderTest.HourDiagram;
var
  src: StdRequest.IXMLSourceType;
begin
  src := Sources_AddPrm(1, DATACHANNEL_PARAM, DATATYPE_HOUR_DIAGRAM);
  src.AggrIntervalLen := 60;
  Diagram_PrepareSource(src);

  DoRequest();
  CheckRequest();

  Check(resp.Data.Count = 1, 'resp.Data.Count');
  Check(resp.Data[0].Count = 24, 'resp.Data[0].Count');
  Check(resp.Data[0][0].Val = 0, 'resp.Data[0][0]');
  Check(resp.Data[0][1].Val = 1, 'resp.Data[0][1]');
  Check(resp.Data[0][0].Utime = int64(src.UTime1), 'resp.Data[0][0].Utime');
  Check(resp.Data[0][1].Utime = int64(src.UTime1) + UTC_HOUR, 'resp.Data[0][1].Utime');

  src := nil;
end;

procedure TUniversalTransponderTest.Structure;
begin
  req.Structure := 1;
  DoRequest();
  CheckRequest();

  Check(resp.ParamTypes.Count = 48, 'ParamTypes.Count');

  Check(resp.Objects.Count = 1, 'Objects.Count');

  Check(resp.Objects[0].Devices.Count = 2, 'Devices.Count 0');
  Check(resp.Objects[0].Devices[0].Channels.Count = 12, 'Channels.Count1');
  Check(resp.Objects[0].Devices[1].Channels.Count = 28, 'Channels.Count2');

  Check(resp.Objects[0].Aggregates.Count = 1, 'Aggregates.Count');
  Check(resp.Objects[0].Aggregates[0].AggregateParams.Count = 4, 'AggregateParams.Count');

  Check(resp.Nodes.Count = 10, 'Nodes.Count');
  Check(resp.Nodes.NodeById(5).NodeChannels.Count = 2, 'NodeChannels.Count');
end;

procedure TUniversalTransponderTest.SetUp;
begin
  inherited;

  req := NewGMIORequest();
  req.Auth.Login := 'user';

  resp := NewGMIOResponce();

  glvBuffer := TGeomerLastValuesBuffer.Create();
  glvBuffer.AddValue(1, 1, 1{ID_Prm}, 0, false{bCounter}, false{bNI}, 0, 1, 0, 1 {калибровка}, NowGM(), 10);
  glvBuffer.AddValue(1, 1, 2{ID_Prm}, 0, false{bCounter}, false{bNI}, 0, 1, 0, 1 {калибровка}, NowGM(), 20);
  glvBuffer.AddValue(1, 1, 3{ID_Prm}, 0, false{bCounter}, false{bNI}, 0, 1, 0, 1 {калибровка}, NowGM(), 30);
  glvBuffer.AddValue(1, 1, 4{ID_Prm}, 0, false{bCounter}, false{bNI}, 0, 1, 0, 2 {калибровка}, NowGM(), 40);

  transponder := TClientResponce.Create(glvBuffer);
  ExecSQL('delete from Commands');
end;

procedure TUniversalTransponderTest.TearDown;
begin
  inherited;

  FreeAndNil(glvBuffer);
  transponder.Free();
  req := nil;
  resp := nil;
  ExecSQL('delete from Commands');
end;

var Params: TZConnectionParams;

{ TXMLTest }

procedure TXMLTest.Responce_Commands;
var
  resp: IXMLGMIOResponceType;
  cmd: IXMLCommandType;
begin
  resp := NewGMIOResponce();
  cmd := resp.Commands.Add();
  CheckNotNull(cmd);
  cmd.ID_Prm := 1;
  cmd.Utime := NowGM();
  cmd.Val := 11.5;

  CheckEquals(1, cmd.ID_Prm);
  Check(Abs(cmd.Utime - int64(NowGM())) <= 1);
  Check(CompareValue(11.5, cmd.Val) = 0);
end;

initialization
  SetMainConfigFileClass(TGMServerMainConfigFile);
  GMMainConfigFile.ReadSQLConnectionParams(Params);
  Params.LibraryLocation := '..\Server\libpq.dll';
  SetGlobalSQLConnectionParams(Params);

  RegisterTest('GMIOPSrv/Socket', TUniversalTransponderTest.Suite);
  RegisterTest('GMIOPSrv/Socket', TXMLTest.Suite);
end.


