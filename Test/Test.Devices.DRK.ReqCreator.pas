unit Test.Devices.DRK.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst;

type
  TDRK04OPReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

implementation

{ TDRK04OPReqCreatorTest }

procedure TDRK04OPReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, 'D0 D0 D0 D0 D1 25 D2 C1');
  CheckReqHexString(1, 'D0 D0 D0 D0 D1 25 D2 C2');
end;

function TDRK04OPReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_DRK;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/ÄÐÊ', TDRK04OPReqCreatorTest.Suite);
end.
