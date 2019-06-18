unit Test.Devices.ICP.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst;

type
  TICP7017ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

  TICP7041DReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

implementation

{ TICP7017ReqCreatorTest }

procedure TICP7017ReqCreatorTest.DoCheckRequests;
begin
  CheckReqString(0, '#04'#13);
end;

function TICP7017ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_I7017;
end;

{ TICP7041DReqCreatorTest }

procedure TICP7041DReqCreatorTest.DoCheckRequests;
begin
  CheckReqString(0, '@02'#13);
  CheckReqString(1, '#020'#13);
  CheckReqString(14, '#02D'#13);
end;

function TICP7041DReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_I7041D;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/ICP', TICP7017ReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/ICP', TICP7041DReqCreatorTest.Suite);
end.
