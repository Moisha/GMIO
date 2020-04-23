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
    property InitialSendTimeDelay: int read FInitialSendTimeDelay write FInitialSendTimeDelay;
  end;

implementation

uses
  EsLogging;

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

    Sleep(10);
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
  FInitialSendTimeDelay := 5000;
  FInitialCurrentTimeSent := false;
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
  Result := FGMSocket.ReadAndParseDataBlock();
end;

procedure TGMTCPServerThread.BackgroundWork();
begin
  FGMSocket.BackgroundWork();
end;

function TGMTCPServerThread.SendInitialCurrentTime: bool;
begin
  if FInitialCurrentTimeSent or (GetTickCount64() - FStartTime < FInitialSendTimeDelay) then
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
  bData: bool;
  res: TCheckCOMResult;
begin
  bData := false;
  FClientSocket.GetSins();
  FStartTime := GetTickCount64();
  while not Terminated do
  begin
    if SocketLocked() then
    begin
      SleepThread(10);
      continue;
    end;

    res := ReadAndParseDataBlock();
    case res of
      ccrBytes:
        bData := true;
      ccrError:
        break;
    end;

    BackgroundWork();
    if not bData and not SendInitialCurrentTime() then
      break;

    Sleep(0);
  end;
end;

end.
