unit Threads.MainSrvDataPicker;

interface

uses Windows, SysUtils, Classes, Threads.Base, Math, Threads.ReqSpecDevTemplate, Threads.TCP, GMConst, GMGlobals;

type
  TMainSrvDataPicker = class(TRequestTCPDevicesBase)
  public
    constructor Create(id_obj: int); overload;
  end;

implementation

{ TMainSrvDataPicker }

constructor TMainSrvDataPicker.Create(id_obj: int);
begin
  Create(OBJ_TYPE_REMOTE_MAIN, id_obj);
end;

end.
