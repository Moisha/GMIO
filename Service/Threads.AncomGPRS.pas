////////////////////////////////////////////
// Поток опроса приборов через GPRS-модемы Ancom
////////////////////////////////////////////
unit Threads.AncomGPRS;

interface

uses Windows, Threads.ReqSpecDevTemplate, ScktComp, GMGlobals, SysUtils, GM485;

type
  TRequestAncomGPRS = class(TRequestSpecDevices)
  protected
    procedure ConfigurePort(ri: TSpecDevReqListItem); override;
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

procedure TRequestAncomGPRS.ConfigurePort(ri: TSpecDevReqListItem);
var Sckt: TGeomerSocket;
begin
  Sckt := lstSockets.SocketByIdObj(ID_Obj);
  TConnectionObjectTCP_IncomingSocket(ConnectionObject).Socket := Sckt;
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

