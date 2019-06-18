////////////////////////////////////////////
// Служебные функции для приборов Simag
////////////////////////////////////////////
unit Devices.Simag;

interface

uses Devices.ICP, GMConst, GMGlobals;

type
  // Simag11 - аналог ICP7017
  TSimag11ReqCreator = class(TICP7017ReqCreator)
  protected
    function GetReqType(): T485RequestType; override;
  end;

implementation

{ TSimag11ReqCreator }

function TSimag11ReqCreator.GetReqType: T485RequestType;
begin
  Result := rqtSimag11;
end;

end.
