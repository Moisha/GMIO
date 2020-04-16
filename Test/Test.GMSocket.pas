unit Test.GMSocket;

interface

uses Windows, TestFrameWork, GMGlobals, GMSocket, Classes, ScktComp, Threads.ReqSpecDevTemplate, WinSock,
     GMSQLQuery, AppConfigFile, StdRequest, StdResponce, Threads.GMClient,
     ConnParamsStorage, Test.UniversalExchange, GeomerLastValue, System.Generics.Collections, Threads.RemoteSrv, Connection.Base, blcksock;

type
  TGeomerSocketForTest = class(TGeomerSocket)
  private
    FCommands: TSTringList;
  protected
    function GMSendText(const s: string): int; override;
    procedure SetSocketObjectType(objType: int);
  public
    constructor Create(ASocket: TTCPBlockSocket; AglvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy; override;
  end;

  TSocketListForTest = class(TSocketList)
  private
    FList: TObjectList<TGeomerSocketForTest>;
  protected
    function GetSocket(i: int): TGeomerSocket; override;
    function GetCout: int; override;
  public
    constructor Create(ASocket: TServerWinSocket);
    destructor Destroy(); override;
  end;

  TDataOrderForTest = class(TDataOrder)
  protected
    function GetLogin: string; override;
    function GetPassword: string; override;
  end;

  TConnectionObjectRemoteSrvForTest = class(TConnectionObjectBase)
  private
    FSocket: TGeomerSocketForTest;
  protected
    function LogSignature: string; override;
    function MakeExchange(etAction: TExchangeType): TCheckCOMResult; override;
  end;

  TGMRemoteSrvDataThreadXmlForTest = class(TGMRemoteSrvDataThreadXml)
  private
    FSocket: TGeomerSocketForTest;
  protected
    procedure RefreshArchList; override;
    function SendStructure: bool; override;
    function CreateConnectionObject(const AHost: string; APort: int): TConnectionObjectBase; override;
    procedure WaitForMainServerReady; override;
    procedure SendArchives; override;
  public
    constructor Create(ASocket: TGeomerSocketForTest);
  end;

  TGMSocketTest = class(TUniversalTransponderTest)
  private
    FSocket: TGeomerSocketForTest;
    FOrder: TDataOrderForTest;
    FRequestList: TList<TRequestDetails>;
    procedure InternalDoRequest(var bufs: TTwoBuffers);
    procedure CheckRemoteStructure;
    procedure ImportRemoteStructure_DoCheck(bufs: TTwoBuffers);
    procedure WaitForRemoteSrv;
    procedure ClearRemoteObjects;
    function FindThread(objType, ID_Obj: int): TRequestSpecDevices;
    procedure AddRequestToSendBuf(ReqDetails: TRequestDetails);
  protected
    procedure DoRequest; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TestSetChannelValue(ID_Prm: int; value: double; timeHold: int = 0);
  published
    procedure ImportRemoteStructure();
    procedure RequestRemoteSrvChannelsLastData();
    procedure RemoteServerXmlData;
    procedure ReadAndParseDataBlock();

    procedure SetChannelValueTCPModbus();
    procedure SetChannelValueComUBZ();
    procedure SetChannelValueGeomerVacon();
    procedure SetChannelValueGeomer();
    procedure SetChannelValueGeomer_TimeHold();
    procedure SetChannelValueRemote();
    procedure LoadCommandsSqlSyntax();
    procedure RemoteCommand;
  end;

implementation

uses Forms, DateUtils, SysUtils, GMConst, ClientResponce, RequestThreadsContainer, Test.Threads.RegularControl, System.Math, UsefulQueries;

const
  PRM_ID_FOR_COMMAND = 71;

type
  TRequestSpecDevicesLocal = class(TRequestSpecDevices);

{ TGMSocketTest }

procedure CreateRemoteDataReport(const xml: string; var bufs: TTwoBuffers);
begin
  bufs.RequestUniversal(xml, '', false, 250, 10);
end;

procedure TGMSocketTest.CheckRemoteStructure;
var sql, res: string;
begin
  sql :=       'select count (*) ' +
         #13#10'  from Objects o1 left outer join objects o2 on o1.ID_Obj = o2.RemoteID' +
         #13#10'    where o1.RemoteID is null' +
         #13#10'          and (select count(*) from DeviceSystem ds where ds.ID_Obj = o1.ID_Obj) > 0' +
         #13#10'          and (' +
         #13#10'             o2.ID_Obj is null' +
         #13#10'          or o1.Name <> o2.Name' +
         #13#10'          or o1.N_Car <> o2.N_Car' +
         #13#10'          or o2.ObjType <> ' + IntToStr(OBJ_TYPE_REMOTE_SRV_XML) +
         #13#10'          or coalesce(o1.ObjType, 0) <> coalesce(o2.RemoteType, 0)' +
         #13#10'          or o1.UserData <> o2.UserData' +
         #13#10'             )';

  res := QueryResult(sql);
  Check(QueryResult(sql) = '0', 'objects: ' + res);

  sql :=       'select count (*) ' +
         #13#10'  from Devices d1 left outer join Devices d2 on d1.ID_Device = d2.RemoteID' +
         #13#10'    where d1.RemoteID is null' +
         #13#10'          and (select count(*) from DeviceSystem ds where ds.ID_Device = d1.ID_Device) > 0' +
         #13#10'          and (' +
         #13#10'             d2.ID_Device is null' +
         #13#10'          or d1.DevName <> d2.DevName' +
         #13#10'          or d1.ID_DevType <> d2.ID_DevType' +
         #13#10'          or coalesce(d1.Number, 0) <> coalesce(d2.Number, 0)' +
         #13#10'             )';

  res := QueryResult(sql);
  Check(QueryResult(sql) = '0', 'devices: ' + res);

  sql :=       'select count (*) ' +
         #13#10'  from Params p1 left outer join Params p2 on p1.ID_Prm = p2.RemoteID' +
         #13#10'    where p1.RemoteID is null' +
         #13#10'          and p1.ID_PT > 0 ' +
         #13#10'          and (' +
         #13#10'             p2.ID_Prm is null' +
         #13#10'          or p1.ID_PT <> p2.ID_PT' +
         #13#10'          or p1.Name <> p2.Name' +
         #13#10'          or p1.ID_Src <> p2.ID_Src ' +
         #13#10'          or case when p1.N_Src > 0 then p1.N_Src else p1.CurrentsAddr end <> p2.N_Src ' +
         #13#10'          or coalesce(p1.DevMin, -1) <> coalesce(p2.DevMin, -1)' +
         #13#10'          or coalesce(p1.DevMax, -1) <> coalesce(p2.DevMax, -1) ' +
         #13#10'          or coalesce(p1.RealMin, -1) <> coalesce(p2.RealMin, -1)' +
         #13#10'          or coalesce(p1.RealMax, -1) <> coalesce(p2.RealMax, -1)  ' +
         #13#10'          or coalesce(p1.ID_MeaUnit, -1) <> coalesce(p2.ID_MeaUnit, -1)  ' +
         #13#10'          or coalesce(p1.AlarmSignal, -1) <> coalesce(p2.AlarmSignal, -1) ' +
         #13#10'          or coalesce(p1.AlarmThreshold, -1) <> coalesce(p2.AlarmThreshold, -1) ' +
         #13#10'          or coalesce(p1.NI_StartDT, -1) <> coalesce(p2.NI_StartDT, -1) ' +
         #13#10'          or coalesce(p1.NI_StartVal, -1) <> coalesce(p2.NI_StartVal, -1)' +
         #13#10'          or coalesce(p1.BaseChn, -1) <> coalesce(p2.BaseChn, -1)  ' +
         #13#10'          or coalesce(p1.CalibrType, -1) <> coalesce(p2.CalibrType, -1) ' +
         #13#10'          or coalesce(p1.Resistor, -1) <> coalesce(p2.Resistor, -1)  ' +
         #13#10'          or coalesce(p1.MeterCalcNIType, -1) <> coalesce(p2.MeterCalcNIType, -1) ' +
         #13#10'          or coalesce(p1.ReqIntervalType, -1) <> coalesce(p2.ReqIntervalType, -1)  ' +
         #13#10'          or p1.ID_Device <> coalesce((select RemoteID from Devices where ID_Device = p2.ID_Device), -1)' +
         #13#10'             )';

  res := QueryResult(sql);
  Check(QueryResult(sql) = '0', 'channels: ' + res);
end;

procedure TGMSocketTest.WaitForRemoteSrv;
var res: int;
    bFirst: bool;
begin
  bFirst := true;
  repeat
    req.State.RemoteServerReady := 1;
    DoRequest();

    res := resp.State.RemoteServerReady;
    if bFirst then
    begin
      Check(res = 0, 'too fast');
      bFirst := false;
    end;
  until res = 1;
end;

procedure TGMSocketTest.ImportRemoteStructure_DoCheck(bufs: TTwoBuffers);
begin
  InternalDoRequest(bufs);
  Check(FOrder.resp.Auth.OK = 1);
  WaitForRemoteSrv();
  CheckRemoteStructure();
end;

procedure TGMSocketTest.ImportRemoteStructure;
var processor: TClientResponce;
    resp: IXMLGMIOResponceType;
    bufs: TTwoBuffers;
begin
  ClearRemoteObjects();
  processor := TClientResponce.Create(nil);
  try
    resp := processor.DBStructure();
    resp.Auth.RemoteName := 'TEST';
    CreateRemoteDataReport(resp.XML, bufs);
    // 1. по пустому
    ImportRemoteStructure_DoCheck(bufs);
    // 2. по живому
    ImportRemoteStructure_DoCheck(bufs);
  finally
    processor.Free();
  end;
end;

procedure TGMSocketTest.TestSetChannelValue(ID_Prm: int; value: double; timeHold: int = 0);
var
  bufs: TTwoBuffers;
begin
  bufs.BufRec[0] := 171;
  bufs.BufRec[1] := 26; // управление
  WriteUINT(bufs.BufRec, 2, ID_Prm); // ID_Prm
  WriteFloat(bufs.BufRec, 6, value); // Value
  WriteUINT(bufs.BufRec, 14, timeHold); // TimeHold, в TCP не используется
  bufs.NumberOfBytesRead := 18;

  FSocket.AnalyzeClientRequest(bufs);
  CheckEquals(3, bufs.LengthSend);
  CheckEquals(0, bufs.BufSend[2]);
end;

procedure TGMSocketTest.SetChannelValueGeomer;
var
  socket: TGeomerSocketForTest;
begin
  TestSetChannelValue(9, 25);
  socket := TGeomerSocketForTest(lstSockets.SocketByIdObj(1));
  CheckNotNull(socket);
  socket.LoadCommands();
  CheckEquals(1, socket.FCommands.Count);
  CheckEquals(#13#10'OUT1:25'#13#10, socket.FCommands[0]);
end;

procedure TGMSocketTest.SetChannelValueGeomer_TimeHold;
var
  socket: TGeomerSocketForTest;
begin
  TestSetChannelValue(9, 25, 7);

  socket := TGeomerSocketForTest(lstSockets.SocketByIdObj(1));
  CheckNotNull(socket);
  socket.LoadCommands();
  CheckEquals(1, socket.FCommands.Count);
  CheckEquals(#13#10'OUT:1,25,7'#13#10, socket.FCommands[0]);
end;

procedure TGMSocketTest.SetChannelValueGeomerVacon;
var
  socket: TGeomerSocketForTest;
begin
  TestSetChannelValue(162, 1);
  socket := TGeomerSocketForTest(lstSockets.SocketByIdObj(3));
  socket.LoadCommands();
  CheckNotNull(socket);
  CheckEquals(1, socket.ReqList.Count);
end;

function TGMSocketTest.FindThread(objType, ID_Obj: int): TRequestSpecDevices;
begin
  Result := ThreadsContainer.MultiObjectRequestThread.FindThread(objType, ID_Obj);
  CheckNotNull(Result);
end;

procedure TGMSocketTest.SetChannelValueTCPModbus;
var
  thr: TRequestSpecDevices;
begin
  TestSetChannelValue(1072, 1, 0);

  thr := FindThread(OBJ_TYPE_TCP, 4);
  TRequestSpecDevicesLocal(thr).LoadCommands();
  CheckEquals(1, thr.RequestList.Count);
  CheckEquals(12, thr.RequestList[0].ReqDetails.BufCnt);
end;

procedure TGMSocketTest.SetChannelValueRemote;
var
  sqlGetPrm: string;
  id_prm: int;
  a: ArrayOfString;
begin
  sqlGetPrm := Format('select * from DeviceSystem where ID_Src = %d and coalesce(RemoteName, '''') <> ''''', [SRC_AO]);
  id_prm := StrToIntDef(QueryResult(sqlGetPrm), 0);
  if id_prm <= 0 then
  begin
    ImportRemoteStructure();
    id_prm := StrToIntDef(QueryResult(sqlGetPrm), 0);
  end;
  Check(id_prm > 0);
  TestSetChannelValue(id_prm, 11.4, 0);
  CheckEquals(1, StrToIntDef(QueryResult('select count(*) from Commands'), 0));
  a := QueryResultArray_FirstRow('select ID_Prm, UTime, Value from Commands');
  CheckEquals(id_prm, StrToIntDef(a[0], 0));
  Check(Abs(int64(NowGM()) - StrToIntDef(a[1], 0)) < 10);
  Check(CompareValue(11.4, MyStrToFloatDef(a[2], 0)) = 0);
end;

procedure TGMSocketTest.SetChannelValueComUBZ;
var
  thr: TRequestSpecDevices;
begin
  TestSetChannelValue(52, 8, 0);

  thr := FindThread(OBJ_TYPE_COM, 6);
  TRequestSpecDevicesLocal(thr).LoadCommands();
  CheckEquals(1, thr.RequestList.Count);
  CheckEquals(8, thr.RequestList[0].ReqDetails.BufCnt);
  CheckEquals(2, thr.RequestList[0].ReqDetails.buf[5]);
end;

procedure TGMSocketTest.SetUp;
begin
  inherited SetUp;

  FSocket := TGeomerSocketForTest.Create(nil, glvBuffer);
  FOrder := TDataOrderForTest.Create();
  FRequestList := TList<TRequestDetails>.Create();
  ThreadsContainer := TRequestThreadsContainerForTest.Create();

  lstSockets := TSocketListForTest.Create(nil);
  TSocketListForTest(lstSockets).FList.Add(TGeomerSocketForTest.Create(nil, glvBuffer));
  TGeomerSocketForTest(lstSockets[0]).SetSocketObjectType(OBJ_TYPE_GM);
  lstSockets[0].N_Car := 100;
  TSocketListForTest(lstSockets).FList.Add(TGeomerSocketForTest.Create(nil, glvBuffer));
  TGeomerSocketForTest(lstSockets[1]).SetSocketObjectType(OBJ_TYPE_GM);
  lstSockets[1].N_Car := 300;

  ExecSQL('delete from Commands');
end;

procedure TGMSocketTest.TearDown;
begin
  inherited;

  FreeAndNil(lstSockets);
  FSocket.Free();
  FOrder.Free();
  FRequestList.Free();
  ThreadsContainer.Free();

  ExecSQL('delete from Commands');
end;

procedure TGMSocketTest.InternalDoRequest(var bufs: TTwoBuffers);
begin
  bufs.NumberOfBytesRead := bufs.LengthSend;
  WriteBuf(bufs.BufRec, 0, bufs.BufSend, bufs.LengthSend);

  FSocket.AnalyzeClientRequest(bufs);
  Check(FOrder.ParseResult(bufs.BufSend, bufs.LengthSend));
  resp := FOrder.resp;
end;

procedure TGMSocketTest.LoadCommandsSqlSyntax;
begin
  ExecSQL(SqlText_LoadCommands(0, 0, 0));
  ExecSQL(SqlText_LoadCommands(1, 0, 0));
  ExecSQL(SqlText_LoadCommands(1, 1, 0));
  ExecSQL(SqlText_LoadCommands(1, 1, 1));
  Check(true);
end;

procedure TGMSocketTest.ReadAndParseDataBlock;
var buf: ArrayOfByte;
begin
  buf := GMGlobals.TextNumbersStringToArray('FF 00 EE 11 CB B1 90 52 E4 0B 07 AA C0 CA 06 0C 23 EE 95 00 00 00 78 DA B3 71 F7 ' +
                                            'F5 F4 0F 4A 2D 2C 4D 2D 2E B1 B3 71 49 2C 49 B4 B3 09 CE 2F 2D 4A 4E 2D 86 31 14 3C ' +
                                            '53 6C 95 CC 2D CC 94 14 C2 12 73 82 8B 92 43 2A 0B 52 6D 95 0C C0 5C 08 DB 50 49 C1 ' +
                                            '25 33 31 BD 28 31 17 2E E7 98 9E 5E E4 99 57 52 54 96 E3 93 9A 67 AB 64 AA A4 10 1A ' +
                                            '92 99 9B 6A 68 AB 64 62 61 6E 64 6A 60 6E 60 06 15 32 02 0B 19 1B 9B 1B 02 85 F4 ED ' +
                                            '6C F4 E1 D6 EB 43 9C E3 58 5A 92 A1 E0 93 9F 9E 09 34 A7 B4 38 B5 08 AC 08 D9 D9 00 ' +
                                            'A6 A6 3C 7E');
  FSocket.ReadAndParseDataBlock_ProcessBuf(buf);

  buf := GMGlobals.TextNumbersStringToArray('46	55 18 6D 01 C7 31 0C 1D 20 53 08 02 40 CF 2A 02 12 00 F8 00 0D 01 01 00 FF FF FF FF FF FF FF FF 08 03 00 00 00 00 04 00 00 00 00 00 00 00');
  FSocket.ReadAndParseDataBlock_ProcessBuf(buf);

  buf := GMGlobals.TextNumbersStringToArray('E7 B4 02 00 00 00 06 00 00 00 68 00 00 00 01 00 00 00 08 00 00 00 01 03 00 00 00 82 33 0C 1D FB FF FF DF 4B F3 31 40 BE 33 0C 1D 08 00 00 C0 56 E9 31 40 FA 33 0C 1D F3 FF FF 1F C5 EF 31 40');
  FSocket.ReadAndParseDataBlock_ProcessBuf(buf);

  Check(true);
end;

procedure TGMSocketTest.AddRequestToSendBuf(ReqDetails: TRequestDetails);
begin
  FRequestList.Add(ReqDetails);
end;

procedure TGMSocketTest.RemoteCommand;
var
  id_prm_remote: int;
  thread: TGMRemoteSrvDataThreadXmlForTest;
begin
  id_prm_remote := SQLReq_GetIdPrmForRemotePrmId('TEST', PRM_ID_FOR_COMMAND);
  SQLReq_SetChannel(id_prm_remote, 100.001);
  thread := TGMRemoteSrvDataThreadXmlForTest.Create(FSocket);
  thread.WaitFor();
  thread.Free();

  TRequestSpecDevices.LoadCommands(0, 0, PRM_ID_FOR_COMMAND, AddRequestToSendBuf);
  CheckEquals(1, FRequestList.Count);
  CheckEquals(PRM_ID_FOR_COMMAND, FRequestList[0].ID_Prm);
end;

procedure TGMSocketTest.RemoteServerXmlData;
var src: IXMLSourceType;
    val: IXMLValueType;
    i: int;
    bufs: TTwoBuffers;
    t, t1: int64;
begin
  resp := NewGMIOResponce();
  resp.Auth.RemoteName := 'TEST';
  src := resp.Data.Add();
  src.ValType := DATATYPE_CURRENT_DIAGRAM;
  for i := 0 to 5000 do
  begin
    val := src.Add();
    val.Utime := NowGM();
    val.Val := 100.00000001;
  end;

  CreateRemoteDataReport(resp.xml, bufs);

  t := GetTickCount();
  InternalDoRequest(bufs);

  t1 := GetTickCount();
  Check((t1 < t) or (t1 - t < 2000), 'timeout');
  Check(FSocket.ExecSQLThread.Count > 0, 'sql');
end;

procedure TGMSocketTest.RequestRemoteSrvChannelsLastData;
var
  src: StdRequest.IXMLSourceType;
  t, t1: int64;
begin
  req := NewGMIORequest();
  req.Auth.RemoteName := 'TEST';
  req.Auth.Login := 'user';
  req.Auth.Password := CalcMD5('123');
  src := req.Data.Sources.Add();
  src.Id := 68;
  src.ValSrcType := DATACHANNEL_PARAM;
  src.ValType := DATATYPE_CURRENT_DIAGRAM;
  src.UTime1 := 0;
  src.DiagramType := DIAGRAMTYPE_REMOTE;

  t := GetTickCount();
  DoRequest();

  t1 := GetTickCount();
  Check((t1 < t) or (t1 - t < 2000), 'timeout');
  CheckEquals(1, resp.Data.Count);
end;

procedure TGMSocketTest.DoRequest();
var bufs: TTwoBuffers;
begin
  FOrder.req := req;
  FOrder.OrderType := dotUniversal;
  FOrder.CreateRequest(bufs);
  InternalDoRequest(bufs);
end;

procedure TGMSocketTest.ClearRemoteObjects();
begin
  ExecSQL('delete from Objects where coalesce(RemoteName, '''') <> '''' ');
end;

{ TDataOrderForTest }

function TDataOrderForTest.GetLogin: string;
begin
  Result := 'user';
end;

function TDataOrderForTest.GetPassword: string;
begin
  Result := CalcMD5('123');
end;

{ TGeomerSocketForTest }

constructor TGeomerSocketForTest.Create(ASocket: TTCPBlockSocket; AglvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create(TTCPBlockSocket.Create(), AglvBuffer);
  FCommands := TSTringList.Create();
end;

destructor TGeomerSocketForTest.Destroy;
begin
  FCommands.Free();
  Socket.Free();
  inherited;
end;

function TGeomerSocketForTest.GMSendText(const s: string): int;
begin
  FCommands.Add(s);
  Result := 0;
end;

procedure TGeomerSocketForTest.SetSocketObjectType(objType: int);
begin
  FSocketObjectType := objType;
end;

{ TSocketListForTest }

constructor TSocketListForTest.Create(ASocket: TServerWinSocket);
begin
  inherited;
  FList := TObjectList<TGeomerSocketForTest>.Create(true);
end;

destructor TSocketListForTest.Destroy;
begin
  FList.Free();
  inherited;
end;

function TSocketListForTest.GetCout: int;
begin
  Result := FList.Count;
end;

function TSocketListForTest.GetSocket(i: int): TGeomerSocket;
begin
  Result := FList[i];
end;

{ TGMRemoteSrvDataThreadXmlForTest }

constructor TGMRemoteSrvDataThreadXmlForTest.Create(ASocket: TGeomerSocketForTest);
begin
  FSocket := ASocket;
  inherited Create('TEST', '', 0);
end;

function TGMRemoteSrvDataThreadXmlForTest.CreateConnectionObject(const AHost: string; APort: int): TConnectionObjectBase;
begin
  Result := TConnectionObjectRemoteSrvForTest.Create();
  TConnectionObjectRemoteSrvForTest(Result).FSocket := FSocket;
end;

procedure TGMRemoteSrvDataThreadXmlForTest.RefreshArchList;
var
  chn: TGMRemoteSrvChannel;
begin
  chn := ChannelByID(PRM_ID_FOR_COMMAND);
  chn.CanReceiveCommand := true;
  chn.LastCurr := 0;
  chn.LastHour := 0;
  chn.LastDay := 0;
end;

procedure TGMRemoteSrvDataThreadXmlForTest.SendArchives;
begin
  inherited;
  Terminate();
end;

function TGMRemoteSrvDataThreadXmlForTest.SendStructure: bool;
begin
  Result := true;
end;

procedure TGMRemoteSrvDataThreadXmlForTest.WaitForMainServerReady;
begin

end;

{ TConnectionObjectRemoteSrvForTest }

function TConnectionObjectRemoteSrvForTest.LogSignature: string;
begin
  Result := '';
end;

function TConnectionObjectRemoteSrvForTest.MakeExchange(etAction: TExchangeType): TCheckCOMResult;
var
  bufs: TTwoBuffers;
begin
  bufs.NumberOfBytesRead := buffers.LengthSend;
  bufs.BufRec := buffers.BufSend;
  bufs.LengthSend := 0;

  FSocket.AnalyzeClientRequest(bufs);

  buffers.NumberOfBytesRead := bufs.LengthSend;
  buffers.BufRec := bufs.BufSend;

  Result := ccrBytes;
end;

initialization
  RegisterTest('GMIOPSrv/Socket', TGMSocketTest.Suite);
end.


