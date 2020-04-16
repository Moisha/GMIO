////////////////////////////////////////////
// Поток опроса приборов через GPRS-модемы Ancom
////////////////////////////////////////////
unit Threads.AncomGPRS;

interface

uses Windows, Threads.ReqSpecDevTemplate, ScktComp, GMGlobals, SysUtils, GM485;

type
  TRequestAncomGPRS = class(TRequestSpecDevices)
  protected
    function ConfigurePort(ri: TSpecDevReqListItem): bool; override;
  public
    constructor Create(id_obj: int);
  end;

implementation

uses GMSocket, GMConst, ProgramLogFile, Math, Connection.TCP;

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
var
  Sckt: TGeomerSocket;
begin
  Sckt := lstSockets.SocketByIdObj(ID_Obj);
  if Sckt = nil then
    Exit(false);

  Result := inherited;
  TConnectionObjectTCP_IncomingSocket(ConnectionObject).Socket := Sckt.Socket;
  if Sckt <> nil then
    ConnectionObject.LogPrefix := 'Ancom_' + IntToStr(Sckt.N_Car)
  else
    ConnectionObject.LogPrefix := 'Ancom_ID_Obj=' + IntToStr(ID_Obj);
end;

constructor TRequestAncomGPRS.Create(id_obj: int);
begin
  inherited Create(OBJ_TYPE_ANCOM, id_obj);
  CreateConnectionObject(TConnectionObjectTCP_IncomingSocket);
end;

end.

