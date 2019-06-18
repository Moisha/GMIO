unit Test.Devices.TRM138.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst;

type
  TTRM138ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

implementation

{ TTRM138ReqCreatorTest }

procedure TTRM138ReqCreatorTest.DoCheckRequests;
begin
  CheckReqString(0, '#GMHGONOKTRLT'#$D);
  CheckReqString(1, '#GNHGONOKUHQJ'#$D);
  CheckReqString(2, '#GOHGONOKHTVU'#$D);
  CheckReqString(3, '#GPHGONOKINGG'#$D);
  CheckReqString(4, '#GQHGONOKMOGI'#$D);
  CheckReqString(5, '#GRHGONOKLIVS'#$D);
  CheckReqString(6, '#GSHGONOKVMGM'#$D);
  CheckReqString(7, '#GTHGONOKSSVO'#$D);
end;

function TTRM138ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_TRM138;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/TRM138', TTRM138ReqCreatorTest.Suite);
end.
