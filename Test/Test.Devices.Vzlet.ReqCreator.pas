unit Test.Devices.Vzlet.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst;

type
  TVzletURSVReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  published
  end;

implementation

{ TUBZReqCreatorTest }

procedure TVzletURSVReqCreatorTest.DoCheckRequests;
begin
	CheckReqHexString(0, '7, 4, C1, A8, 0, 2, CD, B1');
  CheckReqHexString(1, '7, 4, C1, AA, 0, 2, 6C, 71');
  CheckReqHexString(2, '7, 4, C1, AC, 0, 2, 8C, 70');
  CheckReqHexString(3, '7, 4, C1, AE, 0, 2, 2D, B0');
end;

function TVzletURSVReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_VZLET_URSV;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Vzlet', TVzletURSVReqCreatorTest.Suite);
end.

