////////////////////////////////////////////
// Поток опроса приборов через СОМ-порты
////////////////////////////////////////////
unit Threads.GMCOM;

interface

uses Windows, SysUtils, GMGlobals, ADODB, INIFiles, ActiveX, GM485,
     Classes, GMConst, Threads.ReqSpecDevTemplate, Connection.COM;

type
  TRequestCOMDevices = class (TRequestSpecDevices)
  protected
    function ConfigurePort(ri: TSpecDevReqListItem): bool; override;
  public
    procedure AfterConstruction; override;
    constructor Create(id_obj: int);
  end;

implementation

uses GMBlockValues, Math;

{ TRequestCOMDevices }

procedure TRequestCOMDevices.AfterConstruction;
begin
  CreateConnectionObject(TConnectionObjectCOM);
  inherited;
end;

function TRequestCOMDevices.ConfigurePort(ri: TSpecDevReqListItem): bool;
begin
  Result := inherited;

  ConnectionObject.FreePort();

  TConnectionObjectCOM(ConnectionObject).nPort := ri.ReqDetails.N_Car;
  TConnectionObjectCOM(ConnectionObject).nBaudRate := IfThen(ri.ReqDetails.BaudRate > 0, ri.ReqDetails.BaudRate, 9600);
  TConnectionObjectCOM(ConnectionObject).nWordLen := 8;
  TConnectionObjectCOM(ConnectionObject).nParity := 0;
  TConnectionObjectCOM(ConnectionObject).nStopBits := 0;
end;

constructor TRequestCOMDevices.Create(id_obj: int);
begin
  inherited Create(OBJ_TYPE_COM, id_obj);
end;

end.

