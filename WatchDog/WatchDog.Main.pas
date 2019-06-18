unit WatchDog.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  IniFiles, Threads.Base, Generics.Collections, GMGenerics, GMGlobals, Connection.Base, Connection.TCP, WinApi.WinSvc,
  ShellAPI, StdRequest, StdResponce, GMConst, ActiveX;

type
  TGMService = class
  public
    SvcName: string;
    Port: int;
    LastAccess, LastRun, LastStop: TDateTime;
  end;

  GMWDThread = class(TGMThread)
  private
    const SVC_TIMEOUT_RESTART = 1 / 24 / 60 * 2;
{$ifdef DEBUG}
    const SVC_TIMEOUT_REPLY = 1 / 24 / 60 / 10;
{$else}
    const SVC_TIMEOUT_REPLY = 1 / 24 / 60 * 5;
{$endif}
  private
    FServers: TGMCollection<TGMService>;
    FIniFile: string;
    FConnection: TConnectionObjectTCP_OwnSocket;
    function ReadINI: bool;
    procedure PingServices;
    procedure AddService(const AName: string; APort: int);
    procedure PingService(svc: TGMService);
    procedure RestartService(svc: TGMService);
    function GetTerminated(): bool;
    function KillService(pid: DWORD): bool;
  protected
    procedure SafeExecute; override;
  public
    constructor Create();
    destructor Destroy; override;
  end;

  TSiriusWatchDog = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    thr: GMWDThread;
{$ifdef Application}
  public
{$endif}
    procedure InitService();
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  SiriusWatchDog: TSiriusWatchDog;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SiriusWatchDog.Controller(CtrlCode);
end;

function TSiriusWatchDog.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

{ GMWDThread }

procedure GMWDThread.AddService(const AName: string; APort: int);
var i: int;
begin
  for i := 0 to FServers.Count - 1 do
    if AnsiSameText(FServers[i].SvcName, AName) then
    begin
      FServers[i].Port := APort;
      Exit;
    end;

  with FServers.Add() do
  begin
    SvcName := AName;
    Port := APort;
    LastAccess := Now();
    LastRun := 0;
    LastStop := 0;
  end;
end;

function GMWDThread.ReadINI(): bool;
var f: TIniFile;
    n, port: int;
    name: string;
begin
  Result := false;

  if not FileExists(FIniFile) then
  begin
    AddService('GMIOPService', 65500); // без INI пытаемся найти сервис по умолчанию
    Result := true;
    Exit();
  end;

  try
    f := TIniFile.Create(FIniFile);
    n := 1;
    while f.SectionExists(IntToStr(n)) do
    begin
      port := f.ReadInteger(IntToStr(n), 'PORT', 65500);
      name := f.ReadString(IntToStr(n), 'NAME', '');
      if (port > 0) and (Trim(name) <> '') then
        AddService(name, port);

      inc(n);
    end;
    Result := true;
  except end; // не смог и хрен с ним
end;

constructor GMWDThread.Create;
begin
  inherited Create();

  FConnection := TConnectionObjectTCP_OwnSocket.Create();
  FConnection.Host := '127.0.0.1';
  FConnection.WaitFirst := 7000;
  FConnection.WaitNext := 100;
  FConnection.CheckTerminated := GetTerminated;
  FConnection.NumAttempts := 1;
  FServers := TGMCollection<TGMService>.Create();
  FIniFile := ChangeFileExt(ParamStr(0), '.ini');
end;

destructor GMWDThread.Destroy;
begin
  FConnection.Free();
  FServers.Free();

  inherited;
end;

function GMWDThread.GetTerminated: bool;
begin
  Result := Terminated;
end;

procedure GMWDThread.PingService(svc: TGMService);
var req: IXMLGMIORequestType;
    resp: IXMLGMIOResponceType;
begin
  FConnection.Port := svc.Port;
  if FConnection.ExchangeBlockData(etRec) = ccrBytes then
  begin
    req := NewGMIORequest();
    req.State.RemoteServerReady := 1;
    if FConnection.RequestUniversal(req.XML, AUTH_DEFAULT_LOGIN, true, ruxcRequest, resp) = ccrBytes then
      svc.LastAccess := Now();
  end;
  FConnection.FreePort();
  req := nil;
  resp := nil;
end;

function RunAsAdmin(const Path, Params: string): Boolean;
var
  sei: TShellExecuteInfo;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := 0;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(Path);
  sei.lpParameters := PChar(Params);
  sei.nShow := SW_SHOWNORMAL;
  Result := ShellExecuteEx(@sei);
end;

function GMWDThread.KillService(pid: DWORD): bool;
begin
  Result := RunAsAdmin('taskkill.exe', '/F /PID ' + IntToStr(pid));
end;

procedure GMWDThread.RestartService(svc: TGMService);
var hscm, hsvc: SC_HANDLE;
    st: TServiceStatus;
    ssStatus: SERVICE_STATUS_PROCESS;
    dwBytesNeeded: DWORD;
    lpServiceArgVectors: LPCWSTR;
begin
  hscm := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hscm = 0 then Exit;

  try
    hsvc := OpenService(hscm, PChar(svc.SvcName), SC_MANAGER_ALL_ACCESS);
    if hsvc = 0 then Exit;

    try
      if not QueryServiceStatusEx(hsvc, SC_STATUS_PROCESS_INFO, @ssStatus, sizeof(SERVICE_STATUS_PROCESS), dwBytesNeeded) then Exit;

      case ssStatus.dwCurrentState of
        SERVICE_RUNNING:
          begin
            // остановим, если давно запущен, но не работает
            if (Now() - svc.LastRun > SVC_TIMEOUT_RESTART) and ControlService(hsvc, SERVICE_CONTROL_STOP, st) then
              svc.LastStop := Now();
          end;
        SERVICE_STOPPED:
          begin
            lpServiceArgVectors := nil;
            if StartService(hsvc, 0, lpServiceArgVectors) then
              svc.LastRun := Now();
          end;
        SERVICE_STOP_PENDING:
          begin
            if Now() - svc.LastStop > SVC_TIMEOUT_RESTART then // если никак не может закрыться - прибьём
              KillService(ssStatus.dwProcessId);
          end;
      end;
    finally
      CloseServiceHandle(hsvc);
    end;
  finally
    CloseServiceHandle(hscm);
  end;
end;

procedure GMWDThread.PingServices;
var i: int;
begin
  for i := 0 to FServers.Count - 1 do
  begin
    PingService(FServers[i]);
    if Abs(Now() - FServers[i].LastAccess) > SVC_TIMEOUT_REPLY then
      RestartService(FServers[i]);
  end;
end;

procedure GMWDThread.SafeExecute;
begin
  CoInitialize(nil);
  while not Terminated do
  begin
    if ReadINI() then
      PingServices();

    SleepThread(1000);
  end;
end;

procedure TSiriusWatchDog.InitService;
begin
  thr := GMWDThread.Create();
end;

procedure TSiriusWatchDog.ServiceExecute(Sender: TService);
begin
  InitService();

  while not Terminated do
  begin
    ServiceThread.ProcessRequests(True);
  end;
end;

procedure TSiriusWatchDog.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  thr.Free();
end;

end.
