////////////////////////////////////////////
// Поток опроса приборов через GPRS-модемы Ancom
////////////////////////////////////////////
unit Threads.AncomGPRS;

interface

uses Windows, Threads.ReqSpecDevTemplate, ScktComp, GMGlobals, SysUtils, GMSocket, GM485;

type
  TRequestAncomGPRS = class(TRequestSpecDevices)
  private
    FSocket: TGeomerSocket;
  protected
    function ConfigurePort(ri: TSpecDevReqListItem): bool; override;
    procedure FreePort; override;
  public
    constructor Create(id_obj: int; socket: TGeomerSocket);
  end;

implementation

uses GMConst, ProgramLogFile, Math, Connection.TCP;

type
   TConnectionObjectTCP_IncomingSocketForAncom = class(TConnectionObjectTCP_IncomingSocket)
   protected
     function DefaultWaitFirst: int; override;
     function DefaultWaitNext: int; override;
   end;

function TConnectionObjectTCP_IncomingSocketForAncom.DefaultWaitFirst: int;
begin
  Result := Max(inherited DefaultWaitFirst(), 5000);
end;

function TConnectionObjectTCP_IncomingSocketForAncom.DefaultWaitNext: int;
begin
  Result := Max(inherited DefaultWaitNext(), 2000);
end;

{ TRequestAncomGPRS }

function TRequestAncomGPRS.ConfigurePort(ri: TSpecDevReqListItem): bool;
begin
  if FSocket = nil then
    Exit(false);

  Result := inherited ConfigurePort(ri);
  FSocket.Lock(ThreadId);
end;

constructor TRequestAncomGPRS.Create(id_obj: int; socket: TGeomerSocket);
begin
  inherited Create(OBJ_TYPE_ANCOM, id_obj);
  FSocket := socket;
  CreateConnectionObject(TConnectionObjectTCP_IncomingSocketForAncom);
  TConnectionObjectTCP_IncomingSocketForAncom(ConnectionObject).Socket := FSocket.Socket;
  ConnectionObject.LogPrefix := 'Ancom_' + IntToStr(FSocket.N_Car);
end;

procedure TRequestAncomGPRS.FreePort;
begin
  inherited;
  FSocket.Unlock();
end;

end.

