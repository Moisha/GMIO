unit Connection.TCP;

interface

uses Windows, SysUtils, GMConst, GMGlobals, Connection.Base, ScktComp, StrUtils;

type
  TConnectionObjectTCP = class(TConnectionObjectBase)
  private
    function ReadFromWinSocketStream(wss: TWinSocketStream): TCheckCOMResult;
    function WaitForData(wss: TWinSocketStream; TimeOutMs: int; FirstByteOnly: bool): TCheckCOMResult;
  protected
    FLastError: string;
    procedure LockSocket; virtual;
    procedure UnlockSocket; virtual;
    procedure OpenSocket; virtual;
    procedure HandleException(e: Exception; const source: string);
    function CreateWinSocketStream(): TWinSocketStream; virtual; abstract;
    function MakeExchange(etAction: TExchangeType): TCheckCOMResult; override;
    function GetLastConnectionError: string; override;
    function ExceptionLogInfo(e: Exception): string; virtual;
  public
  end;

  TConnectionObjectTCP_IncomingSocket = class (TConnectionObjectTCP)
  private
    FSocket: TCustomWinSocket;
  protected
    procedure LockSocket; override;
    procedure UnlockSocket; override;
    function CreateWinSocketStream(): TWinSocketStream; override;
    function ConnectionEquipmentInitialized(): bool; override;
    function LogSignature: string; override;
  public
    property Socket: TCustomWinSocket read FSocket write FSocket;
  end;

  TConnectionObjectTCP_OwnSocket = class (TConnectionObjectTCP)
  private
    FSocket: TClientSocket;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: int);
    procedure ScktError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  protected
    function LogSignature: string; override;
    procedure OpenSocket; override;
    function CreateWinSocketStream(): TWinSocketStream; override;
    function ExceptionLogInfo(e: Exception): string; override;
  public
    constructor Create(); override;
    destructor Destroy(); override;
    property Socket: TClientSocket read FSocket;
    property Host: string write SetHost;
    property Port: int write SetPort;
    procedure FreePort; override;
  end;

implementation

{ TConnectionObjectTCP }

uses ProgramLogFile;

function TConnectionObjectTCP.WaitForData(wss: TWinSocketStream; TimeOutMs: int; FirstByteOnly: bool): TCheckCOMResult;
var t: int64;
    nRead: int;
    LocalBuf: array[0..102400] of byte;
begin
  t := GetTickCount();
  Result := ccrEmpty;

  repeat
    nRead := 0;
    if wss.WaitForData(100) then
      nRead := wss.Read(LocalBuf, High(LocalBuf));

    if nRead > 0 then
    begin
      Result := ccrBytes;
      WriteBuf(buffers.BufRec, buffers.NumberOfBytesRead, LocalBuf, nRead);
      buffers.NumberOfBytesRead := buffers.NumberOfBytesRead + nRead;
      if FirstByteOnly or CheckGetAllData() then break;

      if ParentTerminated then break;
    end;
  until Abs(GetTickCount() - t) >= TimeOutMs;
end;

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

function TConnectionObjectTCP.ReadFromWinSocketStream(wss: TWinSocketStream): TCheckCOMResult;
begin
  try
    buffers.NumberOfBytesRead := 0;
    wss.TimeOut := 200;

    Result := WaitForData(wss, WaitFirst, true);
    if (Result = ccrBytes) and (buffers.NumberOfBytesRead > 0) then
    begin
      while not CheckGetAllData() do // пока не сработает CheckGetAllData или не вычерпаем все
      begin
        if WaitForData(wss, WaitNext, false) <> ccrBytes then break;
      end;
    end;
  except
    on e: Exception do
    begin
      HandleException(e, 'ReadFromWinSocketStream');
      Result := ccrError;
    end;
  end;
end;

function TConnectionObjectTCP.GetLastConnectionError: string;
begin
  Result := FLastError;
end;

procedure TConnectionObjectTCP.LockSocket();
begin

end;

procedure TConnectionObjectTCP.UnlockSocket();
begin

end;

procedure TConnectionObjectTCP.OpenSocket();
begin

end;

function TConnectionObjectTCP.MakeExchange(etAction: TExchangeType): TCheckCOMResult;
var wss: TWinSocketStream;
begin
  Result := ccrError;

  LockSocket();
  try
    try
      OpenSocket();
      wss := CreateWinSocketStream();

      if etAction in [etSenRec, etSend] then
        wss.WriteBuffer(buffers.BufSend, buffers.LengthSend);

      if etAction in [etSenRec, etRec] then
      begin
        try
          Result := ReadFromWinSocketStream(wss);
        finally
          TryFreeAndNil(wss);
        end;
      end
      else
      begin
        Result := ccrEmpty;
      end;
    except
      on e: Exception do
        HandleException(e, 'MakeExchange');
    end;
  finally
    UnlockSocket();
  end;
end;

{ TConnectionObjectTCP_OwnSocket }

constructor TConnectionObjectTCP_OwnSocket.Create;
begin
  inherited Create();

  FSocket := TClientSocket.Create(nil);
  FSocket.ClientType := ctBlocking;
  FSocket.OnError := ScktError;
end;

procedure TConnectionObjectTCP_OwnSocket.ScktError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ErrorCode := 0;
end;

function TConnectionObjectTCP_OwnSocket.CreateWinSocketStream: TWinSocketStream;
begin
  Result := TWinSocketStream.Create(FSocket.Socket, WaitNext);
end;

destructor TConnectionObjectTCP_OwnSocket.Destroy;
begin
  inherited Destroy; // здесь вызывается FreePort, где закрывается FSocket, поэтому очистим FSocket после inherited
  FSocket.Free();
end;

function TConnectionObjectTCP_OwnSocket.ExceptionLogInfo(e: Exception): string;
begin
  if FSocket <> nil then
    Result := Format(' (host = %s, port = %d)', [FSocket.Host.QuotedString(), FSocket.Port])
  else
    Result := '';
end;

procedure TConnectionObjectTCP_OwnSocket.FreePort;
begin
  inherited FreePort;

  FSocket.Close();
end;

function TConnectionObjectTCP_OwnSocket.LogSignature: string;
begin
  Result := IfThen(LogPrefix <> '', LogPrefix, 'TCP');
  if FSocket <> nil then
    Result := Result + ' ' + FSocket.Host + ' ' + IntToStr(FSocket.Port);
end;

procedure TConnectionObjectTCP_OwnSocket.OpenSocket;
begin
  FSocket.Open();
end;

procedure TConnectionObjectTCP_OwnSocket.SetHost(const Value: string);
begin
  FSocket.Host := Value;
end;

procedure TConnectionObjectTCP_OwnSocket.SetPort(const Value: int);
begin
  FSocket.Port := Value;
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

function TConnectionObjectTCP_IncomingSocket.CreateWinSocketStream: TWinSocketStream;
begin
  Result := TWinSocketStream.Create(FSocket, WaitNext);
end;

procedure TConnectionObjectTCP_IncomingSocket.LockSocket;
begin
  FSocket.Lock();
end;

procedure TConnectionObjectTCP_IncomingSocket.UnlockSocket;
begin
  FSocket.Unlock();
end;

end.
