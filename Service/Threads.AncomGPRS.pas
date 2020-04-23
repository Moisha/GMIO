////////////////////////////////////////////
// Поток опроса приборов через GPRS-модемы Ancom
////////////////////////////////////////////
unit Threads.AncomGPRS;

interface

uses Windows, Threads.ReqSpecDevTemplate, ScktComp, GMGlobals, SysUtils, GMSocket, GM485;

type
  TRequestAncomGPRS = class(TRequestSpecDevices)
  private
    function GetSocket: TGeomerSocket;
  protected
    function ConfigurePort(ri: TSpecDevReqListItem): bool; override;
    procedure FreePort; override;
  public
    constructor Create(id_obj: int);
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

function TRequestAncomGPRS.GetSocket: TGeomerSocket;
begin
  Result := lstSockets.SocketByIdObj(ID_Obj);
end;

function TRequestAncomGPRS.ConfigurePort(ri: TSpecDevReqListItem): bool;
var
  Sckt: TGeomerSocket;
begin
  Sckt := GetSocket();
  if Sckt = nil then
    Exit(false);

  Result := inherited ConfigurePort(ri);
  TConnectionObjectTCP_IncomingSocket(ConnectionObject).Socket := Sckt.Socket;
  ConnectionObject.LogPrefix := 'Ancom_' + IntToStr(Sckt.N_Car);
  Sckt.Lock(ThreadId);
end;

constructor TRequestAncomGPRS.Create(id_obj: int);
begin
  inherited Create(OBJ_TYPE_ANCOM, id_obj);
  CreateConnectionObject(TConnectionObjectTCP_IncomingSocketForAncom);
end;

procedure TRequestAncomGPRS.FreePort;
begin
  inherited;
  GetSocket().Unlock();
end;

end.

