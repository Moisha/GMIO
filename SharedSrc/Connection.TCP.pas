unit Connection.TCP;

interface

uses Windows, SysUtils, GMConst, GMGlobals, Connection.Base, StrUtils, blcksock, System.Classes;

type
  TConnectionObjectTCP = class(TConnectionObjectBase)
  private
    FSocket: TTCPBlockSocket;
    function ReadFromSocket(): TCheckCOMResult;
  protected
    FLastError: string;
    function OpenSocket(): int; virtual;
    procedure HandleException(e: Exception; const source: string);
    function MakeExchange(etAction: TExchangeType): TCheckCOMResult; override;
    function GetLastConnectionError: string; override;
    function ExceptionLogInfo(e: Exception): string; virtual;
  public
    property Socket: TTCPBlockSocket read FSocket write FSocket;
  end;

  TConnectionObjectTCP_IncomingSocket = class (TConnectionObjectTCP)
  protected
    function ConnectionEquipmentInitialized(): bool; override;
    function LogSignature: string; override;
  end;

  TConnectionObjectTCP_OwnSocket = class (TConnectionObjectTCP)
  private
    FHost: string;
    FPort: int;
  protected
    function LogSignature: string; override;
    function OpenSocket(): int; override;
    function ExceptionLogInfo(e: Exception): string; override;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    property Host: string read FHost write FHost;
    property Port: int read FPort write FPort;
    procedure FreePort; override;
  end;

  TBlockSocketHelper = class helper for TBlockSocket
  public
    function IsWaitDataSocketError(): bool;
  end;

implementation

{ TConnectionObjectTCP }

uses ProgramLogFile, Winapi.WinSock, synautil;

function TConnectionObjectTCP.ExceptionLogInfo(e: Exception): string;
begin
  Result := '';
end;

procedure TConnectionObjectTCP.HandleException(e: Exception; const source: string);
begin
  ProgramLog().AddException(ClassName() + '.' + source + ' - ' + e.Message + ExceptionLogInfo(e));
  FLastError := e.Message;
  FreePort();
end;

function TConnectionObjectTCP.ReadFromSocket(): TCheckCOMResult;
var
  s: AnsiString;
  ms: ISmartPointer<TMemoryStream>;
begin
  try
    buffers.NumberOfBytesRead := 0;
    ms := TSmartPointer<TMemoryStream>.Create();

    s := Socket.RecvPacket(WaitFirst);
    case Socket.LastError of
      0:
        begin
          WriteStrToStream(ms, s);
          Result := ccrBytes;
        end;
      WSAETIMEDOUT:
        Exit(ccrEmpty);
      else
        Exit(ccrError);
    end;

    if not CheckGetAllData() then
      while true do
      begin
        s := Socket.RecvPacket(WaitNext);
        if Socket.LastError = 0 then
        begin
          WriteStrToStream(ms, s);
          if CheckGetAllData() then
            break;
        end
        else
          break;

        Sleep(10);
      end;

    ms.Seek(0, soFromBeginning);
    buffers.NumberOfBytesRead := ms.Size;
    CopyMemory(@buffers.BufRec, ms.Memory, ms.Size);
  except
    on e: Exception do
    begin
      HandleException(e, 'ReadFromSocket');
      Result := ccrError;
    end;
  end;
end;

function TConnectionObjectTCP.GetLastConnectionError: string;
begin
  Result := FLastError;
end;

function TConnectionObjectTCP.OpenSocket(): int;
begin
  Result := 0;
end;

function TConnectionObjectTCP.MakeExchange(etAction: TExchangeType): TCheckCOMResult;
var
  res: int;
begin
  Result := ccrError;
  try
    res := OpenSocket();
    if res <> 0 then
    begin
      ProgramLog().AddError(GetGoodLastError(res));
      Exit;
    end;

    if etAction in [etSenRec, etSend] then
      Socket.SendBuffer(@buffers.BufSend, buffers.LengthSend);

    if etAction in [etSenRec, etRec] then
      Result := ReadFromSocket()
    else
      Result := ccrEmpty;
  except
    on e: Exception do
      HandleException(e, 'MakeExchange');
  end;
end;

{ TConnectionObjectTCP_OwnSocket }

constructor TConnectionObjectTCP_OwnSocket.Create;
begin
  inherited Create();

  FSocket := TTCPBlockSocket.Create();
end;

destructor TConnectionObjectTCP_OwnSocket.Destroy;
begin
  inherited Destroy; // здесь вызывается FreePort, где закрывается FSocket, поэтому очистим FSocket после inherited
  FSocket.Free();
end;

function TConnectionObjectTCP_OwnSocket.ExceptionLogInfo(e: Exception): string;
begin
  if FSocket <> nil then
    Result := Format(' (host = %s, port = %d)', [FHost.QuotedString(), FPort])
  else
    Result := '';
end;

procedure TConnectionObjectTCP_OwnSocket.FreePort;
begin
  inherited FreePort;

  FSocket.CloseSocket();
end;

function TConnectionObjectTCP_OwnSocket.LogSignature: string;
begin
  Result := IfThen(LogPrefix <> '', LogPrefix, 'TCP');
  if FSocket <> nil then
    Result := Result + ' ' + FHost + ' ' + IntToStr(FPort);
end;

function TConnectionObjectTCP_OwnSocket.OpenSocket: int;
begin
  Result := inherited OpenSocket;
  if Result <> 0 then
    Exit;

  if FSocket.Socket <> INVALID_SOCKET then
    Exit;

  FSocket.Connect(FHost, IntToStr(FPort));
  Result := FSocket.LastError;
end;

{ TConnectionObjectTCP_ExternalSocket }

function TConnectionObjectTCP_IncomingSocket.ConnectionEquipmentInitialized: bool;
begin
  Result := FSocket <> nil;
end;

function TConnectionObjectTCP_IncomingSocket.LogSignature: string;
begin
  Result := IfThen(LogPrefix <> '', LogPrefix, 'TCP Incoming');
end;

{ TBlockSocketHelper }

function TBlockSocketHelper.IsWaitDataSocketError: bool;
begin
  Result := (LastError <> 0) and (LastError <> WSAETIMEDOUT);
end;

end.
