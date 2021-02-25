unit Threads.TcpServer;

interface

uses Windows, Classes, SysUtils, Threads.Base, blcksock, WinSock, GMGlobals, GMSocket, GeomerLastValue, Connection.Base;

type
  TGMTCPServerThread = class;

  TGMTCPServerDaemon = class(TGMThread)
  private
    FPort: int;
    FServerSocket: TTCPBlockSocket;
    FGlvBuffer: TGeomerLastValuesBuffer;
  protected
    procedure SafeExecute; override;
    function CreateClientSocketThread(clientSocket: TSocket; glvBuffer: TGeomerLastValuesBuffer): TGMTCPServerThread; virtual;
  public
    constructor Create(port: int; glvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy; override;
    function Listen(): bool;
  end;

  TGMTCPServerThread = class(TGMThread)
  private
    FClientSocket: TTCPBlockSocket;
    FGMSocket: TGeomerSocket;
    FStartTime: UInt64;
    FInitialSendTimeDelay: int;
    FInitialCurrentTimeSent: bool;
    FLastDataRead: UInt64;
  protected
    property GMSocket: TGeomerSocket read FGMSocket;
    procedure SafeExecute; override;
    function CreateGMSocket(clientSocket: TTCPBlockSocket; glvBuffer: TGeomerLastValuesBuffer): TGeomerSocket; virtual;
    function ReadAndParseDataBlock: TCheckCOMResult; virtual;
    procedure BackgroundWork; virtual;
    function SendInitialCurrentTime: bool; virtual;
    function SocketLocked: bool;
  public
    constructor Create(clientSocket: TSocket; glvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy; override;
  end;

implementation

uses
  EsLogging, Winapi.ActiveX, System.Math, GMConst;

{ TGMTCPServerDaemon }

constructor TGMTCPServerDaemon.Create(port: int; glvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create(true);
  FPort := port;
  FGlvBuffer := glvBuffer;
  FServerSocket := TTCPBlockSocket.Create;
end;

function TGMTCPServerDaemon.CreateClientSocketThread(clientSocket: TSocket; glvBuffer: TGeomerLastValuesBuffer): TGMTCPServerThread;
begin
  Result := TGMTCPServerThread.Create(clientSocket, glvBuffer)
end;

destructor TGMTCPServerDaemon.Destroy;
begin
  FServerSocket.Free();
end;

function TGMTCPServerDaemon.Listen: bool;
begin
  if FServerSocket.Socket <> INVALID_SOCKET then
    Exit(true);

  DefaultLogger.Info('Start on port  %d', [FPort]);
  FServerSocket.CreateSocket();
  if FServerSocket.LastError = 0 then
  begin
    FServerSocket.Bind('0.0.0.0', IntToStr(FPort));
    if FServerSocket.LastError = 0 then
      FServerSocket.Listen();
  end;

  Result := FServerSocket.LastError = 0;
  if Result then
  begin
    DefaultLogger.Info('Listen on port  %d', [FPort]);
    Start();
  end
  else
    DefaultLogger.Error('Failed Listen on port  %d - %s', [FPort, FServerSocket.LastErrorDesc]);
end;

procedure TGMTCPServerDaemon.SafeExecute;
var
  clientSocket: TSocket;
begin
  while not Terminated do
  begin
    if FServerSocket.CanRead(1000) then
    begin
      clientSocket := FServerSocket.Accept();
      if FServerSocket.LastError = 0 then
        CreateClientSocketThread(clientSocket, FGlvBuffer)
      else
        DefaultLogger.Error(FServerSocket.LastErrorDesc);
    end
    else
    if FServerSocket.LastError <> 0 then
    begin
      DefaultLogger.Error(FServerSocket.LastErrorDesc);
      Exit;
    end;

    SleepThread(10);
  end;
end;

{ TGMTCPServerThread }

function TGMTCPServerThread.CreateGMSocket(clientSocket: TTCPBlockSocket; glvBuffer: TGeomerLastValuesBuffer): TGeomerSocket;
begin
  Result := TGeomerSocket.Create(clientSocket, glvBuffer);
end;

constructor TGMTCPServerThread.Create(clientSocket: TSocket; glvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create(false);
  FClientSocket := TTCPBlockSocket.Create;
  FClientSocket.Socket := clientSocket;
  FGMSocket := CreateGMSocket(FClientSocket, glvBuffer);
  FInitialSendTimeDelay := 5;
  FInitialCurrentTimeSent := false;
  FLastDataRead := 0;
  FreeOnTerminate := true;
end;

destructor TGMTCPServerThread.Destroy;
begin
  FGMSocket.Free();
  FClientSocket.Free();

  inherited;
end;

function TGMTCPServerThread.ReadAndParseDataBlock(): TCheckCOMResult;
begin
  Result := ccrEmpty;
  case FGMSocket.SocketObjectType of
    OBJ_TYPE_UNKNOWN,
    OBJ_TYPE_CLIENT,
    OBJ_TYPE_GM,
    OBJ_TYPE_REMOTE_SRV,
    OBJ_TYPE_K105:
      Result := FGMSocket.ReadAndParseDataBlock();
  end;
end;

procedure TGMTCPServerThread.BackgroundWork();
begin
  FGMSocket.BackgroundWork();
end;

function TGMTCPServerThread.SendInitialCurrentTime: bool;
begin
  if FInitialCurrentTimeSent or (NowGM() - FStartTime < FInitialSendTimeDelay) then
    Exit(true);

  DefaultLogger.Info('SendCurrentTime');
  Result := FGMSocket.SendCurrentTime() = 0;
  FInitialCurrentTimeSent := true;
end;

function TGMTCPServerThread.SocketLocked: bool;
begin
  Result := FGMSocket.Locked();
end;

procedure TGMTCPServerThread.SafeExecute;
var
  res: TCheckCOMResult;
  FLastLockTime: UInt64;
begin
  CoInitialize(nil);
  try
    FClientSocket.GetSins();
    FStartTime := NowGM();
    FLastLockTime := 0;
    while not Terminated do
    begin
      if SocketLocked() then
      begin
        if FLastLockTime = 0 then
          FLastLockTime := GetTickCount64();

        if GetTickCount64() - FLastLockTime > 300 * 1000 then
        begin
          DefaultLogger.Info('Too long lock, breaking');
          break;
        end;

        SleepThread(10);
        continue;
      end
      else
        FLastLockTime := 0;

      res := ReadAndParseDataBlock();
      if res = ccrError then
        break;

      BackgroundWork();
      FLastDataRead := GMSocket.LastReadUTime;

      if (FLastDataRead = 0) and not SendInitialCurrentTime() then
      begin
        DefaultLogger.Info('SendInitialCurrentTime failed');
        break;
      end;

      if Abs(NowGM() - Max(FLastDataRead, FStartTime)) > 300 then
      begin
        DefaultLogger.Info('Too long idle, breaking');
        break;
      end;

      SleepThread(10);
    end;
  finally
    CoUninitialize();
  end;
end;

end.
