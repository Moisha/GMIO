unit Connection.UDP;

interface

uses Windows, SysUtils, StrUtils, GMConst, GMGlobals, Connection.Base, IdUDPBase, IdUDPClient, IdGlobal, IdSocketHandle;

type
  TConnectionObjectUDP = class(TConnectionObjectBase)
  private
    udp: TIdUDPClient;
  protected
    function LogSignature: string; override;
    function MakeExchange(etAction: TExchangeType): TCheckCOMResult; override;
    function ConnectionEquipmentInitialized(): bool; override;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    procedure FreePort; override;
    property UDPObject: TIdUDPClient read udp;
  end;

  TConnectionObjectIncomingUDP = class(TConnectionObjectBase)
  private
    FSocket: TIdSocketHandle;
  protected
    function MakeExchange(etAction: TExchangeType): TCheckCOMResult; override;
    function LogSignature: string; override;
    property Socket: TIdSocketHandle read FSocket write FSocket;
  end;

implementation

{ TConnectionObjectUDP }

uses ProgramLogFile;

function TConnectionObjectUDP.ConnectionEquipmentInitialized: bool;
begin
  Result := udp.Active;
end;

constructor TConnectionObjectUDP.Create;
begin
  inherited;
  udp := TIdUDPClient.Create(nil);
end;

destructor TConnectionObjectUDP.Destroy;
begin
  udp.Free();
  inherited;
end;

procedure TConnectionObjectUDP.FreePort;
begin
  if udp <> nil then
    udp.Active := false;
end;

function TConnectionObjectUDP.LogSignature: string;
begin
  Result := IfThen(LogPrefix <> '', LogPrefix, 'UDP') + ' ' + udp.Host + ':' + IntToStr(udp.Port);
end;

function TConnectionObjectUDP.MakeExchange(etAction: TExchangeType): TCheckCOMResult;
var bytes: TIdBytes;
    action: string;
begin
  Result := ccrEmpty;
  buffers.NumberOfBytesRead := 0;
  action := '';
  try
    if etAction in [etSend, etSenRec] then
    begin
      action := 'udp.SendBuffer';
      SetLength(bytes, buffers.LengthSend);
      WriteBuf(bytes, 0, buffers.bufSend, buffers.LengthSend);
      udp.SendBuffer(bytes);
    end;

    if etAction in [etRec, etSenRec] then
    begin
      action := 'udp.ReceiveBuffer';
      SetLength(bytes, 1000);
      buffers.NumberOfBytesRead := udp.ReceiveBuffer(bytes);
      WriteBuf(buffers.bufRec, 0, bytes, buffers.NumberOfBytesRead);
    end;

    if buffers.NumberOfBytesRead > 0 then
      Result := ccrBytes;
  except
    on e: Exception do
    begin
      ProgramLog.AddException(ClassName() + '.MakeExchange.' + action + ': ' + e.Message);
      Result := ccrError;
    end;
  end;
end;

{ TConnectionObjectIncomingUDP }

function TConnectionObjectIncomingUDP.LogSignature: string;
begin
  Result := IfThen(LogPrefix <> '', LogPrefix, 'Inc. UDP');
end;

function TConnectionObjectIncomingUDP.MakeExchange(etAction: TExchangeType): TCheckCOMResult;
begin
  Socket.SendTo(Socket.PeerIP, Socket.PeerPort, IdBytesFromBuf(buffers.BufSend, buffers.LengthSend));
  Result := ccrEmpty;
end;

end.
