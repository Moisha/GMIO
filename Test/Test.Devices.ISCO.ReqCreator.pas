unit Test.Devices.ISCO.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst;

type
  TIsco4250ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

implementation

{ TIsco4250ReqCreatorTest }

procedure TIsco4250ReqCreatorTest.DoCheckRequests;
begin
  CheckReqString(0, '?????????????'#$D);
  CheckReqString(1, 'DATA'#$D);
end;

function TIsco4250ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_ISCO_4250;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/ISCO', TIsco4250ReqCreatorTest.Suite);
end.

