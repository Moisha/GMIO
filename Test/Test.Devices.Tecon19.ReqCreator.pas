unit Test.Devices.Tecon19.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst, GmSqlQuery, SysUtils, Devices.Tecon;

type
  TTecon19ReqCreatorForTest = class(TTecon19ReqCreator)
  protected
    function GetNeedSetTime: bool; override;
  end;

  TTecon19ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    procedure SetUp; override;
    function GetDevType(): int; override;
    procedure DoCheckRequests; override;
  published
    procedure HourArrayTimeIndex();
  end;

implementation

{ TTecon19ReqCreatorTest }

procedure TTecon19ReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0,  '10, 40, 1, 9, 1, 4, 0, 4F, 16');
  CheckReqHexString(1,  '10, 41, 1, 9, 2, 4, 0, 51, 16');
  CheckReqHexString(2,  '10, 42, 1, 9, 3, 4, 0, 53, 16');
  CheckReqHexString(3,  '10, 43, 1, 9, 7, 5, 0, 59, 16');
  CheckReqHexString(4,  '10, 44, 1, 9, 8, 5, 0, 5B, 16');
  CheckReqHexString(5,  '10, 45, 1, 9, 9, 5, 0, 5D, 16');
  CheckReqHexString(6,  '10, 46, 1, 9, A, 5, 0, 5F, 16');
  CheckReqHexString(7,  '10, 47, 1, 9, 9, 2, 0, 5C, 16');
  CheckReqHexString(8,  '10, 48, 1, 9, A, 2, 0, 5E, 16');
  CheckReqHexString(9,  '10, 49, 1, 9, B, 2, 0, 60, 16');
  CheckReqHexString(10, '10, 4A, 1, 9, C, 2, 0, 62, 16');
  CheckReqHexString(11, '10, 4B, 1, 9,23,80, 0, F8, 16');
  CheckReqHexString(12, '10, 4C, 0,17, 2, 0, 0, 65, 16');
end;

function TTecon19ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_TECON_19_01;
end;

procedure TTecon19ReqCreatorTest.HourArrayTimeIndex;
var idx: int;
begin
  idx := TTecon19ReqCreatorForTest(ReqCreator).HourArrayTimeIndex(EncodeDateTimeUTC(2000, 1, 1), 64);
  Check(idx = 5); // это точка отсчета, но часовой пояс наш, поэтому +5

  idx := TTecon19ReqCreatorForTest(ReqCreator).HourArrayTimeIndex(EncodeDateTimeUTC(2015, 1, 10, 14, 00), 64);
  Check(idx = $048E);

  idx := TTecon19ReqCreatorForTest(ReqCreator).HourArrayTimeIndex(EncodeDateTimeUTC(2015, 1, 26, 00, 00), 32);
  Check(idx = 0); // полночь на 26.01.15 - это как раз день завершения 32-двухдневного цикла

  idx := TTecon19ReqCreatorForTest(ReqCreator).HourArrayTimeIndex(EncodeDateTimeUTC(2015, 1, 30, 00, 00), 32);
  Check(idx = $0060);
end;

procedure TTecon19ReqCreatorTest.SetUp;
begin
  inherited;
  ExecSQL('update Devices set dt_updated = null where ID_Device = ' + IntToStr(ReqCreator.ReqDetails.ID_Device));
end;

{ TTecon19ReqCreatorForTest }

function TTecon19ReqCreatorForTest.GetNeedSetTime: bool;
begin
  Result := true;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Tecon', TTecon19ReqCreatorTest.Suite);
end.

