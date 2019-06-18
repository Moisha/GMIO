unit Test.Devices.Vacon.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst;

type
  TVaconNXLReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

implementation

{ TVaconNXLReqCreatorTest }

procedure TVaconNXLReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, '1, 3, 8, 38, 0, 1, 7,  A7');
  CheckReqHexString(1, '1, 3, 8, 39, 0, 1, 56, 67');
  CheckReqHexString(2, '1, 3, 8, 3E, 0, 1, E7, A6');
  CheckReqHexString(3, '1, 3, 8, 3B, 0, 1, F7, A7');
  CheckReqHexString(4, '1, 3, 7, D2, 0, 1, 25, 47');
  CheckReqHexString(5, '1, 3, 7, D3, 0, 1, 74, 87');
end;

function TVaconNXLReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_VACON_NXL;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Vacon', TVaconNXLReqCreatorTest.Suite);
end.

