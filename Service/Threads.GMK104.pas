////////////////////////////////////////////
// Поток опроса приборов ТЭКОН К-104
////////////////////////////////////////////
unit Threads.GMK104;

interface

uses Windows, SysUtils, GMGlobals, GM485,
     Classes, GMConst, Threads.ReqSpecDevTemplate, IdUDPBase,
     IdUDPClient, Connection.UDP, Connection.Base;

type
  TRequestK104Devices = class (TRequestSpecDevices)
  protected
    function ConfigurePort(ri: TSpecDevReqListItem): bool; override;
  public
    procedure AfterConstruction; override;
    constructor Create(id_obj: int);
  end;

implementation

uses GMBlockValues, Math, IdGlobal, ProgramLogFile;

{ TRequestK104Devices }

procedure TRequestK104Devices.AfterConstruction;
begin
  CreateConnectionObject(TConnectionObjectUDP);
  inherited;
end;

function TRequestK104Devices.ConfigurePort(ri: TSpecDevReqListItem): bool;
begin
  Result := inherited;
  TConnectionObjectUDP(ConnectionObject).UDPObject.Active := false;
  TConnectionObjectUDP(ConnectionObject).UDPObject.Host := ri.ReqDetails.IP;
  TConnectionObjectUDP(ConnectionObject).UDPObject.Port := ri.ReqDetails.IPPort;
  TConnectionObjectUDP(ConnectionObject).UDPObject.ReceiveTimeout := 2000;
  TConnectionObjectUDP(ConnectionObject).UDPObject.Active := true;
  ConnectionObject.LogPrefix := 'K-104';
end;

constructor TRequestK104Devices.Create(id_obj: int);
begin
  inherited Create(OBJ_TYPE_K104, id_obj);
end;

end.

