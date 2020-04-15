unit Threads.TcpServer;

interface

uses Windows, Classes, SysUtils, Threads.Base, blcksock, WinSock, GMGlobals;

type
  TGMTCPServerDaemon = class(TGMThread)
  private
    FPort: int;
    FServerSocket: TTCPBlockSocket;
  public
    constructor Create(port: int);
    destructor Destroy; override;
    procedure SafeExecute; override;
  end;

  TGMTCPServerThread = class(TGMThread)
  private
    FClientSocket: TSocket;
  public
    constructor Create(clientSocket: TSocket);
    procedure SafeExecute; override;
  end;

implementation

uses
  EsLogging;

{ TGMTCPServerDaemon }

constructor TGMTCPServerDaemon.Create(port: int);
begin
  inherited Create(false);
  FPort := port;
  FServerSocket := TTCPBlockSocket.Create;
  FreeOnTerminate := true;
end;

destructor TGMTCPServerDaemon.Destroy;
begin
  FServerSocket.Free();
end;

procedure TGMTCPServerDaemon.SafeExecute;
var
  clientSocket: TSocket;
begin
  DefaultLogger.Info('Start on port  %d', [FPort]);
  FServerSocket.CreateSocket();
  FServerSocket.Bind('0.0.0.0', IntToStr(FPort));
  FServerSocket.Listen();
  DefaultLogger.Info('Listen on port  %d', [FPort]);
  while not Terminated do
  begin
    if FServerSocket.CanRead(1000) then
    begin
      clientSocket := FServerSocket.Accept();
      if FServerSocket.LastError = 0 then
        TGMTCPServerThread.Create(clientSocket)
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

constructor TGMTCPServerThread.Create(clientSocket: TSocket);
begin
  inherited Create(false);
  FClientSocket := clientSocket;
  FreeOnTerminate := true;
end;

procedure TGMTCPServerThread.SafeExecute;
var
  socket: TTCPBlockSocket;
  s: string;
begin
  socket := TTCPBlockSocket.Create;
  try
    socket.Socket := FClientSocket;
    socket.GetSins();
    while not Terminated do
    begin
      s := socket.RecvPacket(60000);
      if socket.LastError <> 0 then
        break;

      socket.SendString(s);

      if socket.LastError <> 0 then
        break;
    end;
  finally
    socket.Free;
  end;
end;

end.
