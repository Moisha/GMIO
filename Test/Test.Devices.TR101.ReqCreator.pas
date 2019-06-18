unit Test.Devices.TR101.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.UBZ, GMConst;

type
  TTR101ReqCreatorTest = class(TUBZReqCreatorTest)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

implementation

{ TTR101ReqCreatorTest }

procedure TTR101ReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, '5, 3, 0, 2, 0, 6, 65, 8C');
  CheckReqHexString(1, '5, 3, 0, 20, 0, 1, 84, 44');
  CheckReqHexString(2, '5, 3, 0, 2E, 0, 1, E5, 87');
  CheckReqHexString(3, '5, 3, 0, 3C, 0, 1, 45, 82');
  CheckReqHexString(4, '5, 3, 0, 4A, 0, 1, A4, 58');
end;

function TTR101ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_TR101;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/TR101', TTR101ReqCreatorTest.Suite);
end.
