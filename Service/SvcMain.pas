unit SvcMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, System.Win.ScktComp,
  GMSocket, GMSqlQuery, ConnParamsStorage, GMGlobals, IdSocketHandle, IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, IdGlobal,
  UDPBindingStorage, WinSvc, Vcl.Consts, UITypes, GeomerLastValue, GMConst, Threads.ObjectOnline,
  Threads.Base, RequestThreadsContainer, Threads.NIUpdater, Threads.ReqSpecDevTemplate, Threads.RemoteSrv, Threads.ResponceParser,
  Vcl.ExtCtrls;

{$I ScriptVersion.inc}

type
  TManageServicetype = (mstStart, mstStop, mstInstall, mstDelete);

  TSvcMessageHandleThread = class(TGMThread)
  private
    procedure ProcessMessage(MSG: TMSG);
  protected
    procedure WMGeomerBlockReceived(var Msg: TMessage); message WM_GEOMER_BLOCK;
    procedure WMDeviceOnline(var Msg: TMessage); message WM_DEVICE_ONLINE;
    procedure SafeExecute(); override;
  end;

  TGMIOPService = class(TService)
    ssGeomer: TServerSocket;
    udpSrv: TIdUDPServer;
    timerResetSocket: TTimer;
    procedure ServiceExecute(Sender: TService);
    procedure ssGeomerAccept(Sender: TObject; Socket: TCustomWinSocket);
    procedure ssGeomerClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ssGeomerClientError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure ssGeomerClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure udpSrvUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure ServiceCreate(Sender: TObject);
    procedure ssGeomerGetSocket(Sender: TObject; Socket: NativeInt; var ClientSocket: TServerClientWinSocket);
    procedure ssGeomerGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceDestroy(Sender: TObject);
    procedure timerResetSocketTimer(Sender: TObject);
  private
    { Private declarations }
    FResetSocketCounter: int;
    FClosingInProcess: bool;
    FUDPBindingsAndThreads: TUDPBindingsAndThreads;

    rmSrvData: TGMThread;
    thrResponceParser: TResponceParserThread;
    thrNIUpdater: TNIUpdateThread;
    glvBuffer: TGeomerLastValuesBuffer;
    thrObjectOnlineState: TObjectOnlineThread;
    thrMessageHandler: TSvcMessageHandleThread;

    bBadName: bool;

    procedure WMGeomerBlockReceived(var Msg: TMessage); message WM_GEOMER_BLOCK;
    procedure WMDeviceOnline(var Msg: TMessage); message WM_DEVICE_ONLINE;
    function ReadINI: bool;
    function ReadAndCheckSQLParams: bool;
    function CheckConnectionParams(params: TZConnectionParams): bool;
    procedure SleepMainThread(sleepTimeMs: int);
    function OpenTCPIPPorts: bool;
    procedure ManageService(action: TManageServicetype; Silent: Boolean);
    procedure InstallService(SvcMgr: SC_HANDLE);
    procedure UninstallService(SvcMgr: SC_HANDLE);
    procedure DisplayMessage(const Msg: string; const MsgType: TMsgDlgType);
    procedure Start(SvcMgr: SC_HANDLE);
    procedure Stop(SvcMgr: SC_HANDLE);
    procedure ObjectOnline(ID_Obj: int; ObjType: int = -1; N_Car: int = 0);
    procedure ProcessK105Autorization(bufs: TTwoBuffers; ABinding: TIdSocketHandle);
    procedure ProcessTeconReply(bufs: TTwoBuffers; ABinding: TIdSocketHandle);
{$ifdef Application}
  public
{$endif}
    procedure InitService;
  public
    { Public declarations }
    function GetServiceController: TServiceController; override;
    function CanStart: bool;
    function StartAsManageService(): bool;
  end;

var
  GMIOPService: TGMIOPService;

implementation

{$R *.DFM}

uses WinSock, ProgramLogFile, AppConfigFile, IniFiles, ActiveX, Devices.Tecon, Threads.K105, Threads.GMSocket,
     StrUtils, UsefulQueries, SvcLogFile, GMBlockValues, Devices.Tecon.Common;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  GMIOPService.Controller(CtrlCode);
end;

function TGMIOPService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

function TGMIOPService.OpenTCPIPPorts: bool;
begin
  Result := false;

  if ssGeomer.Port > 0 then
    try
      ssGeomer.Open;
    except
      on e: Exception do
      begin
        ProgramLog.AddError(e.Message + ''#13''#10'Запуск невозможен до освобождения TCP-порта ' + IntToStr(ssGeomer.Port));
        Exit;
      end;
    end;

  if udpSrv.DefaultPort > 0 then
    try
      udpSrv.Active := true;
    except
      on e: Exception do
      begin
        ProgramLog.AddError(e.Message + ''#13''#10'Запуск невозможен до освобождения UDP-порта ' + IntToStr(udpSrv.DefaultPort));
        Exit;
      end;
    end;

  Result := true;
end;

function TGMIOPService.CanStart: bool;
begin
  Result := not bBadName;
end;

function TGMIOPService.CheckConnectionParams(params: TZConnectionParams): bool;
var q: TGMSqlQuery;
    action, ver: string;
begin
  Result := false;

  CoInitialize(nil);
  q := TGMSqlQuery.Create();
  q.ConnectionParams := params;

  try
    try
      action := 'Connecting';
      q.SQL.Text := 'select 1';
      q.Open();

      action := 'Check script version';
      q.SQL.Text := 'select PrmValue from SysConfig where PrmName = ''ScriptVersion''';
      q.Open();
      if q.Eof or (Trim(q.Fields[0].AsString) = '') then
      begin
        ProgramLog.AddError('Не обнаружена версия скрипта. ' + 'Прогоните скрипт версии не ниже ' + IntToStr(MIN_SCRIPT_VERSION));
        Exit;
      end;

      ver := Trim(q.Fields[0].AsString);
      Result := StrToIntDef(ver, 0) >= MIN_SCRIPT_VERSION;
      if not Result then
      begin
        ProgramLog.AddError(Format('Проверка параметров соединения с БД: Текущая версия скрипта БД (%s) не подходит. ' +
                                   'Прогоните скрипт версии не ниже %d.', [ver, MIN_SCRIPT_VERSION]));
      end;
    except
      on e: Exception do
        ProgramLog.AddError('Проверка параметров соединения с БД: ' + action + ': ' + e.Message)
    end;
  finally
    q.Free();
  end;
end;

function TGMIOPService.ReadAndCheckSQLParams(): bool;
var params: TZConnectionParams;
begin
  Result := GMMainConfigFile.ReadSQLConnectionParams(Params);
  if not Result then Exit;

  Result := CheckConnectionParams(params);
  if not Result then Exit;

  SetGlobalSQLConnectionParams(params);
end;

function TGMIOPService.ReadINI: bool;
var f: TIniFile;
    remoteSrv, remoteName: string;
    remotePort: int;
begin
  Result := (GMMainConfigFile.CheckMainINIFile() = iftINI);
  if not Result then
  begin
    ProgramLog.AddError('Файл конфигурации не наден.');
    Exit;
  end;

  Result := ReadAndCheckSQLParams();
  if Result then
  begin
    f := TIniFile.Create(GMMainConfigFile.GetMainINIFileName());
    try
      ProgramLog.SetNeedProgramLog(f.ReadInteger('LOG', 'PROGRAM', 0) = 1);
      ProgramLog.SetNeedErrorLog(f.ReadInteger('LOG', 'ERROR', 1) = 1);
      ProgramLog.SetNeedComLog(f.ReadInteger('LOG', 'COM', 0) = 1);
      ProgramLog.SetLogPath(f.ReadString('LOG', 'PATH', ''));

      ssGeomer.Port := f.ReadInteger('COMMON', 'PORT', 0);
      udpSrv.DefaultPort := f.ReadInteger('COMMON', 'UDP_PORT', 0);
      COMDevicesRequestInterval := f.ReadInteger('COMMON', 'COM_INTERVAL', 60);
      CommonWaitFirst := f.ReadInteger('COMMON', 'TIMEOUT', CommonWaitFirst);
      CommonWaitNext := CommonWaitFirst div 3;

      remoteSrv := Trim(f.ReadString('COMMON', 'REMOTE_SRV', ''));
      if remoteSrv <> '' then
      begin
        ProgramLog.AddMessage('RemoteSrv = ' + remoteSrv);
        RemoteSrvMaxDataValuesCount := f.ReadInteger('COMMON', 'REMOTE_MAX_PACKAGE', RemoteSrvMaxDataValuesCount);
        remoteName := Trim(f.ReadString('COMMON', 'REMOTE_NAME', ''));
        remotePort := f.ReadInteger('COMMON', 'REMOTE_PORT', 65500);
        if (RemotePort > 0) and (remoteName <> '') then
        begin
          rmSrvData := TGMRemoteSrvDataThreadXML.Create(remoteName, remoteSrv, remotePort);
          ProgramLog.AddMessage('Starting RemoteSrvNew');
        end;
      end;
    finally
      f.Free();
    end;

    Result := OpenTCPIPPorts()
  end;
end;

procedure TGMIOPService.SleepMainThread(sleepTimeMs: int);
var t: int64;
begin
  t := GetTickCount();
  while not Terminated do
  begin
    Sleep(10);
{$ifndef Application}
    ServiceThread.ProcessRequests(false);
{$endif}
    if Abs(GetTickCount() - t) >= sleepTimeMs then break;
  end;
end;

procedure TGMIOPService.InitService();
begin
{$ifndef Application}
  {$ifdef DEBUG}
  SleepMainThread(10000);
  {$endif}
{$endif}
  while not ReadINI() do
  begin
{$ifndef DEBUG}
    SleepMainThread(60000); // читаем INI раз в минуту до победы
{$else}
    SleepMainThread(2000);
{$endif}
    if Terminated then Exit;
  end;

  ProgramLog.AddMessage('========== Starting Service ==========');

  ThreadsContainer := TRequestThreadsContainer.Create();

  thrResponceParser := TResponceParserThread.Create(glvBuffer);
  thrNIUpdater := TNIUpdateThread.Create(glvBuffer);
  thrObjectOnlineState := TObjectOnlineThread.Create();
  thrMessageHandler := TSvcMessageHandleThread.Create();
  Tag := thrMessageHandler.ThreadID;
  ThreadForMessagesHandle := thrMessageHandler.ThreadID;
end;

function TGMIOPService.StartAsManageService(): bool;
var bSilent: bool;
begin
  bSilent := FindSwitch('SILENT');
  Result := true;

  if FindSwitch('INSTALL') then
    ManageService(mstInstall, bSilent)
  else
  if FindSwitch('UNINSTALL') then
    ManageService(mstDelete, bSilent)
  else
  if FindSwitch('START') then
    ManageService(mstStart, bSilent)
  else
  if FindSwitch('STOP') then
    ManageService(mstStop, bSilent)
  else
    Result := false;
end;

procedure TGMIOPService.InstallService(SvcMgr: SC_HANDLE);
var
  Svc: SC_HANDLE;
  Path, ini: string;
  failureActions: SERVICE_FAILURE_ACTIONS;
  actions: array[0..2] of SC_ACTION;
  i: int;
begin
  ini := CmdLineSwitchValue('ini');
  Path := ParamStr(0) + ' /name ' + Name + IfThen(ini <> '', ' ' + ini);

  if Assigned(BeforeInstall) then BeforeInstall(self);

  Svc := CreateService(
                      SvcMgr,
                      PChar(Name),
                      PChar(DisplayName + ' ' + Name),
                      SERVICE_ALL_ACCESS,
                      SERVICE_WIN32_OWN_PROCESS,
                      SERVICE_AUTO_START,
                      SERVICE_ERROR_NORMAL,
                      PChar(Path),
                      nil, // lpLoadOrderGroup
                      nil, // lpdwTagId
                      nil, // lpDependencies
                      nil, // lpServiceStartName, If this parameter is NULL, CreateService uses the LocalSystem account.
                      PChar('')); // NULL вроде не предусмотрен

  if Svc = 0 then
    RaiseLastOSError();


  for i := 0 to 2 do
  begin
    actions[i].&Type := SC_ACTION_RESTART;
    actions[i].Delay := 0;
  end;

  failureActions.dwResetPeriod := 3600;
  failureActions.lpRebootMsg := nil;
  failureActions.lpCommand := nil;
  failureActions.cActions := 3;
  failureActions.lpsaActions := @actions[0];

  if not ChangeServiceConfig2(Svc, SERVICE_CONFIG_FAILURE_ACTIONS, @failureActions) then
    RaiseLastOSError();

  try
    try
      if Assigned(AfterInstall) then AfterInstall(self);
    except
      on E: Exception do
      begin
        DeleteService(Svc);
        raise;
      end;
    end;
  finally
    CloseServiceHandle(Svc);
  end;
end;

procedure TGMIOPService.Start(SvcMgr: SC_HANDLE);
var
  Svc: SC_HANDLE;
  lpServiceArgVectors: LPCWSTR;
begin
  Svc := OpenService(SvcMgr, PChar(Name), SERVICE_ALL_ACCESS);
  if Svc = 0 then
    RaiseLastOSError();

  lpServiceArgVectors := nil;
  try
    if not StartService(Svc, 0, lpServiceArgVectors) then
      RaiseLastOSError();
  finally
    CloseServiceHandle(Svc);
  end;
end;

procedure TGMIOPService.Stop(SvcMgr: SC_HANDLE);
var
  Svc: SC_HANDLE;
  state: SERVICE_STATUS;
begin
  Svc := OpenService(SvcMgr, PChar(Name), SERVICE_ALL_ACCESS);
  if Svc = 0 then
    RaiseLastOSError();

  try
    if not ControlService(Svc, SERVICE_CONTROL_STOP, state) then
      RaiseLastOSError();
  finally
    CloseServiceHandle(Svc);
  end;
end;

procedure TGMIOPService.timerResetSocketTimer(Sender: TObject);
begin
  inc(FResetSocketCounter);

  if FResetSocketCounter >= 30 * 60 then
  begin
    FResetSocketCounter := 0;
    ssGeomer.Close();
    ssGeomer.Open();
  end;
end;

procedure TGMIOPService.UninstallService(SvcMgr: SC_HANDLE);
var
  Svc: SC_HANDLE;
begin
  if Assigned(BeforeUninstall) then BeforeUninstall(self);
  Svc := OpenService(SvcMgr, PChar(Name), SERVICE_ALL_ACCESS);
  if Svc = 0 then RaiseLastOSError;

  try
    if not DeleteService(Svc) then RaiseLastOSError;
  finally
    CloseServiceHandle(Svc);
  end;
  if Assigned(AfterUninstall) then AfterUninstall(self);
end;

procedure TGMIOPService.ObjectOnline(ID_Obj: int; ObjType: int = -1; N_Car: int = 0);
begin
  thrObjectOnlineState.ObjectOnline(ID_Obj, ObjType, N_Car);
end;

procedure TGMIOPService.WMDeviceOnline(var Msg: TMessage);
begin
  ObjectOnline(Msg.WParam);
end;

procedure TGMIOPService.WMGeomerBlockReceived(var Msg: TMessage);
var gbv: TGeomerBlockValues;
begin
  ProgramLog.AddMessage('GeomerBlockReceived');
  gbv := TGeomerBlockValues(Msg.WParam);
  thrResponceParser.Add(gbv);
  ObjectOnline(gbv.ReqDetails.ID_Obj, gbv.ReqDetails.ObjType, gbv.ReqDetails.N_Car);
end;

procedure TGMIOPService.DisplayMessage(const Msg: string; const MsgType: TMsgDlgType);
begin
  if IsConsole then
    WriteLn(Msg)
  else
    MessageDlg(Msg, MsgType, [mbOk], 0);
end;

procedure TGMIOPService.ManageService(action: TManageServicetype; Silent: Boolean);
var
  SvcMgr: SC_HANDLE;
  Success: Boolean;
  Msg: string;
begin
  Success := True;

  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SvcMgr = 0 then RaiseLastOSError;
  try
    try
      case action of
        mstInstall: InstallService(SvcMgr);
        mstDelete: UninstallService(SvcMgr);
        mstStart: Start(SvcMgr);
        mstStop: Stop(SvcMgr);
      end;
    except
      on E: Exception do
      begin
        Success := False;
        case action of
          mstInstall: Msg := SServiceInstallFailed;
          mstDelete: Msg := SServiceUninstallFailed;
          mstStart: Msg := 'Service Start failed';
          mstStop: Msg := 'Service Stop failed';
        end;

        DisplayMessage(Format(Msg, [DisplayName, E.Message]), mtError);
      end;
    end;

    if Success and not Silent then
    begin
      case action of
        mstInstall: Msg := SServiceInstallOK;
        mstDelete: Msg := SServiceUninstallOK;
        mstStart: Msg := 'Service Start OK';
        mstStop: Msg := 'Service Stop OK';
      end;

      DisplayMessage(Msg, mtInformation);
    end;
  finally
    CloseServiceHandle(SvcMgr);
  end;
end;

procedure TGMIOPService.ServiceCreate(Sender: TObject);
var SvcName: string;
begin
  bBadName := false;
  FResetSocketCounter := 0;

  ProgramLogFileClass := TServiceProgramLogFile;

  SvcName := CmdLineSwitchValue('name');
  if SvcName <> '' then
  try
    Name := SvcName;
  except
    on e: Exception do
    begin
      bBadName := true;
      ProgramLog.AddError('Bad Service name: ' + SvcName);
    end;
  end;

  FClosingInProcess := false;

  rmSrvData := nil;
  thrResponceParser := nil;
  thrObjectOnlineState := nil;
  thrMessageHandler := nil;
  ThreadsContainer := nil;

  glvBuffer := TGeomerLastValuesBuffer.Create();

  SetMainConfigFileClass(TGMServiceConfigFile);

  lstSockets := TSocketList.Create(ssGeomer.Socket);
  FUDPBindingsAndThreads := TUDPBindingsAndThreads.Create(udpSrv);
end;

procedure TGMIOPService.ServiceDestroy(Sender: TObject);
begin
  FreeAndNil(thrMessageHandler);
  FreeAndNil(thrNIUpdater);
  FreeAndNil(rmSrvData);
  FreeAndNil(thrResponceParser);
  FreeAndNil(thrObjectOnlineState);
  FreeAndNil(glvBuffer);
  FreeAndNil(ThreadsContainer);
end;

procedure TGMIOPService.ServiceExecute(Sender: TService);
begin
  InitService();

  while not Terminated do
  begin
    ServiceThread.ProcessRequests(True);
  end;
end;

procedure TGMIOPService.ServiceStop(Sender: TService; var Stopped: Boolean);
var tLastDec: int64;
    i, n: int;
begin
  ProgramLog.AddMessage('========== Stopping Service ==========');
  try
    ProgramLog.AddMessage('Closing TCP Server');
    try ssGeomer.Close(); except end;

    ProgramLog.AddMessage('Closing UDP Server');
    try udpSrv.Active := false; except end;
    FUDPBindingsAndThreads.PrepareToShutDown();

    ProgramLog.AddMessage('Terminating Threads');
    if rmSrvData <> nil then rmSrvData.Terminate();
    thrNIUpdater.Terminate();
    ThreadsContainer.Terminate();

    ProgramLog.AddMessage('Wait for UDP threads');
    tLastDec := GetTickCount();
    while (FUDPBindingsAndThreads.Count > 0) and (Abs(GetTickCount() - tLastDec) < 2000)  do
    begin
      Sleep(100);
    end;

    if FUDPBindingsAndThreads.Count > 0 then
      ProgramLog.AddError('Closing: UDP threads not closed in time: ' + IntToStr(FUDPBindingsAndThreads.Count));

    ProgramLog.AddMessage('Wait for threads');
    ThreadsContainer.WaitFor();
    thrNIUpdater.WaitForTimeout(5000);
    if rmSrvData <> nil then rmSrvData.WaitForTimeout(30000);

    // подождем, пока очередь не иссякнет (максимум минуту)
    // поступать данным неоткуда, потоки поступления уже застопорили
    // поэтому долги должны сойти на нет
    tLastDec := GetTickCount();
    n := thrResponceParser.Queue();

    if n > 0 then
    begin
      ProgramLog.AddMessage('Wait for thrResponceParser.Queue ' + IntToStr(n));
      for i := 0 to 120 do
      begin
        Sleep(500);
        if thrResponceParser.Queue() <> n then
        begin
          n := thrResponceParser.Queue();
          tLastDec := GetTickCount();
        end;

        if (n = 0) or (Abs(GetTickCount() - tLastDec) > 3000) then
          break; // если очередь кончилась или не уменьшается больше 3 секунд, вырубим
      end;
    end;

    ProgramLog.AddMessage('thrResponceParser.Terminate');
    thrResponceParser.Terminate();
    thrResponceParser.WaitForTimeout(2000); // больше уже совсем неприлично, все равно очередь не уменьшается или кончилась
  except end; // ничто не должно омрачить выхода из приложения
end;

procedure TGMIOPService.ssGeomerAccept(Sender: TObject; Socket: TCustomWinSocket);
var opt: integer;
begin
  opt := 1;
  SetSockOpt(Socket.SocketHandle, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@opt), SizeOf(opt));
end;

procedure TGMIOPService.ssGeomerClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  ErrorCode := 0;
  Socket.Close();
end;

procedure TGMIOPService.ssGeomerClientRead(Sender: TObject; Socket: TCustomWinSocket);
var Sckt: TGeomerSocket;
    action: string;
begin
  if FClosingInProcess then Exit;

  try
    if (Socket = nil) or not (Socket is TGeomerSocket) then
    begin
      ProgramLog.AddError('ClientRead - Socket type is not TGeomerSocket');
      Exit;
    end;

    action := 'ReadAndParseDataBlock';
    Sckt := TGeomerSocket(Socket);
    Sckt.ReadAndParseDataBlock();

    if FClosingInProcess then Exit;

    // обновим время, кому оно надо
    action := 'SendCurrentTime';
    Sckt.SendCurrentTime();
  except
    on e: Exception do
      ProgramLog().AddException('ClientRead - ' + action + ': ' + e.Message);
  end;
end;

procedure TGMIOPService.ssGeomerGetSocket(Sender: TObject; Socket: NativeInt; var ClientSocket: TServerClientWinSocket);
begin
  if FClosingInProcess then Exit;

  try
    ClientSocket := TGeomerSocket.Create(Socket, Sender as TServerWinSocket, glvBuffer);
  except
    on e: Exception do
      ProgramLog.AddException('GetSocket - ' + e.Message);
  end;
end;

procedure TGMIOPService.ssGeomerGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);
begin
  SocketThread := TGMSocketThread.Create(false, ClientSocket);
end;

procedure TGMIOPService.ProcessK105Autorization(bufs: TTwoBuffers; ABinding: TIdSocketHandle);
var id_obj: int;
    thread: TRequestSpecDevices;
begin
  Tecon_ReplyAuthorization(bufs);
  ProgramLog.AddExchangeBuf('UDP', COM_LOG_OUT, bufs.BufSend, bufs.LengthSend);

  ABinding.SendTo(ABinding.PeerIP, ABinding.PeerPort, IdBytesFromBuf(bufs.BufSend, bufs.LengthSend));
  thread := FUDPBindingsAndThreads.ByBinding(ABinding);
  if thread = nil then
  begin
    id_obj := SQLReq_IdObjByNCar(bufs.BufRec[7], [OBJ_TYPE_K105]);

    if id_obj > 0 then
    begin
      FUDPBindingsAndThreads.ClearByIdObj(id_obj, TRequestK105Devices);
      GMPostMessage(WM_DEVICE_ONLINE, id_obj, 0);

      with TUDPBindingAndThread(FUDPBindingsAndThreads.Add()) do
      begin
        binding := ABinding;
        thread := TRequestK105Devices.Create(id_obj, FUDPBindingsAndThreads);
      end;
    end;
  end
  else
  begin
    GMPostMessage(WM_DEVICE_ONLINE, thread.ID_Obj, 0);
  end;
end;

procedure TGMIOPService.ProcessTeconReply(bufs: TTwoBuffers; ABinding: TIdSocketHandle);
var thread: TRequestSpecDevices;
begin
  thread := FUDPBindingsAndThreads.ByBinding(ABinding);
  if (thread <> nil) and (thread is TRequestK105Devices) then
  begin
    TRequestK105Devices(thread).AddReplyBlock(bufs.BufRec, bufs.NumberOfBytesRead);
    GMPostMessage(WM_DEVICE_ONLINE, thread.ID_Obj, 0);
  end;
end;

procedure TGMIOPService.udpSrvUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var bufs: TTwoBuffers;
begin
  if FClosingInProcess then Exit;
  bufs.NumberOfBytesRead := Length(AData);
  WriteBuf(bufs.BufRec, 0, Adata, Length(AData));
  ProgramLog.AddExchangeBuf('UDP', COM_LOG_IN, bufs.BufRec, bufs.NumberOfBytesRead);

  if not Tecon_CheckCRC(bufs.BufRec, bufs.NumberOfBytesRead) then Exit;

  if Tecon_CheckK105Authorization(bufs.BufRec, bufs.NumberOfBytesRead) then
    ProcessK105Autorization(bufs, ABinding)
  else
  if Tecon_CheckPrmReply(bufs.BufRec, bufs.NumberOfBytesRead) then
    ProcessTeconReply(bufs, ABinding);
end;

procedure TGMIOPService.ssGeomerClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  Socket.OnErrorEvent := ssGeomerClientError;
end;

{ TSvcMessageHandleThread }

procedure TSvcMessageHandleThread.ProcessMessage(MSG: TMSG);
var Message: TMessage;
begin
  Message.Msg := MSG.message;
  Message.WParam := MSG.wParam;
  Message.LParam := MSG.lParam;
  Message.Result := 0;
  Dispatch(Message);
end;

procedure TSvcMessageHandleThread.SafeExecute;
var msg : TMsg;
begin
  while (not Terminated) do
  begin
    if PeekMessage(MSG, 0, 0, 0, PM_REMOVE) then
      ProcessMessage(MSG)
    else
      SleepThread(100);
  end;
end;

procedure TSvcMessageHandleThread.WMDeviceOnline(var Msg: TMessage);
begin
  GMIOPService.WMDeviceOnline(Msg);
end;

procedure TSvcMessageHandleThread.WMGeomerBlockReceived(var Msg: TMessage);
begin
  GMIOPService.WMGeomerBlockReceived(Msg);
end;

end.
