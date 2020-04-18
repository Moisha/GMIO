unit Threads.TcpServer;

interface

uses Windows, Classes, SysUtils, Threads.Base, blcksock, WinSock, GMGlobals, GMSocket, GeomerLastValue;

type
  TGMTCPServerDaemon = class(TGMThread)
  private
    FPort: int;
    FServerSocket: TTCPBlockSocket;
    FGlvBuffer: TGeomerLastValuesBuffer;
  protected
    procedure SafeExecute; override;
  public
    constructor Create(port: int; glvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy; override;
    function Listen(): bool;
  end;

  TGMTCPServerThread = class(TGMThread)
  private
    FClientSocket: TTCPBlockSocket;
    FGMSocket: TGeomerSocket;
  protected
    procedure SafeExecute; override;
  public
    constructor Create(clientSocket: TSocket; glvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy; override;
  end;

implementation

uses
  EsLogging, Connection.Base;

{ TGMTCPServerDaemon }

constructor TGMTCPServerDaemon.Create(port: int; glvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FPort := port;
  FGlvBuffer := glvBuffer;
  FServerSocket := TTCPBlockSocket.Create;
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
        TGMTCPServerThread.Create(clientSocket, FGlvBuffer)
      else
        DefaultLogger.Error(FServerSocket.LastErrorDesc);
    end
    else
    if FServerSocket.LastError <> 0 then
    begin
      DefaultLogger.Error(FServerSocket.LastErrorDesc);
      Exit;
    end;
  end;
end;

{ TGMTCPServerThread }

constructor TGMTCPServerThread.Create(clientSocket: TSocket; glvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create(false);
  FClientSocket := TTCPBlockSocket.Create;
  FClientSocket.Socket := clientSocket;
  FGMSocket := TGeomerSocket.Create(FClientSocket, glvBuffer);
  FreeOnTerminate := true;
end;

destructor TGMTCPServerThread.Destroy;
begin
  FGMSocket.Free();
  FClientSocket.Free();

  inherited;
end;

procedure TGMTCPServerThread.SafeExecute;
var
  bData: bool;
  startTime: UInt64;
  res: TCheckCOMResult;
begin
  bData := false;
  FClientSocket.GetSins();
  startTime := GetTickCount64();
  while not Terminated do
  begin
    res := FGMSocket.ReadAndParseDataBlock();
    case res of
      ccrBytes:
        bData := true;
      ccrError:
        break;
    end;

    if not bData and (GetTickCount64() - startTime >= 5000) and (FGMSocket.SendCurrentTime() <> 0) then
      break;
  end;
end;

end.
