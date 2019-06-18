////////////////////////////////////////////
// ����� ������ �������� �������� �� TCP
////////////////////////////////////////////
unit Threads.TCP;

interface

uses Windows, SysUtils, GMGlobals, GM485,
     Classes, GMConst, Threads.ReqSpecDevTemplate, Connection.Base;

type
  TRequestTCPDevicesBase = class (TRequestSpecDevices)
  protected
    procedure ConfigurePort(ri: TSpecDevReqListItem); override;
  public
    procedure AfterConstruction; override;
  end;

  TRequestTCPDevices = class (TRequestTCPDevicesBase)
  public
    constructor Create(id_obj: int);
  end;

implementation

uses GMBlockValues, Math, IdGlobal, ProgramLogFile, Connection.TCP;

{ TRequestTCPDevicesBase }

procedure TRequestTCPDevicesBase.AfterConstruction;
begin
  CreateConnectionObject(TConnectionObjectTCP_OwnSocket);
  ConnectionObject.LogPrefix := 'TCP';
  inherited;
end;

procedure TRequestTCPDevicesBase.ConfigurePort(ri: TSpecDevReqListItem);
begin
  inherited;
  TConnectionObjectTCP_OwnSocket(ConnectionObject).Host := ri.ReqDetails.IP;
  TConnectionObjectTCP_OwnSocket(ConnectionObject).Port := ri.ReqDetails.IPPort;
end;

{ TRequestTCPDevices }

constructor TRequestTCPDevices.Create(id_obj: int);
begin
  inherited Create(OBJ_TYPE_TCP, id_obj);
end;

end.

