unit Test.Devices.Mercury.ReqCreator;

interface

uses TestFrameWork, GMGlobals, Classes, SysUtils, Devices.Mercury, Math, Test.Devices.Base.ReqCreator,
     GMConst, DateUtils, Windows;

type
  TMercury230ReqCreatorForTest = class(TMercury230ReqCreator)
  protected
    procedure CheckDevStates; override;
    function NeedRequestLastArchAddr: bool; override;
    function CanRequestArchives: bool; override;
    function NeedRequestMeterInfo: bool; override;
  end;

  TMercury230ReqCreatorForTestArch = class(TMercury230ReqCreator)
  protected
    procedure CheckDevStates; override;
    function NeedRequestLastArchAddr: bool; override;
    function NeedRequestMeterInfo: bool; override;
  end;

  TMercury230ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  private
    mercuryRecCreator: TMercury230ReqCreatorForTest;
    mercuryRecCreatorArch: TMercury230ReqCreatorForTestArch;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  published
    procedure OpenChannel;
    procedure RequestReadings;
    procedure MeterInfoRequest;
    procedure LastArchAddrRequest;

    procedure Archives();
    procedure Archives_Utils();
  end;

implementation

{ TMercury230ReqCreatorTest }

procedure TMercury230ReqCreatorTest.RequestReadings;
var buf: ArrayOfByte;
    s: string;
begin
  buf := mercuryRecCreator.ReadingsRequest();
  s := ArrayToString(buf, Length(buf), false, true);
  Check(s = '31 05 00 00 1E D9');
end;

procedure TMercury230ReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, '1C, 1, 1, 1, 1, 1, 1, 1, 1, EF, 41');
  CheckReqHexString(1, '1C, 8, 12, 37, CB');
  CheckReqHexString(2, '1C, 5, 0, 0, 17, B5');
  CheckReqHexString(3, '$1C, 8, $13, $F6, $B');
end;

function TMercury230ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_MERCURY_230;
end;

procedure TMercury230ReqCreatorTest.LastArchAddrRequest;
var buf: ArrayOfByte;
    s: string;
begin
  buf := mercuryRecCreator.LastArchAddrRequest();
  s := ArrayToString(buf, Length(buf), false, true);
  Check(s = '31 08 13 66 02');
end;

procedure TMercury230ReqCreatorTest.MeterInfoRequest;
var buf: ArrayOfByte;
    s: string;
begin
  buf := mercuryRecCreator.MeterInfoRequest();
  s := ArrayToString(buf, Length(buf), false, true);
  Check(s = '31 08 12 A7 C2');
end;

procedure TMercury230ReqCreatorTest.OpenChannel;
var buf: ArrayOfByte;
    s: string;
begin
  buf := mercuryRecCreator.OpenChannelRequest();
  s := ArrayToString(buf, Length(buf), false, true);
  Check(s = '31 01 01 01 01 01 01 01 01 2E 10');
end;

procedure TMercury230ReqCreatorTest.SetUp;
var RecDetails: TRequestDetails;
begin
  inherited;
  RecDetails.Init();
  RecDetails.DevNumber := 49;
  mercuryRecCreator := TMercury230ReqCreatorForTest.Create(nil, RecDetails);
  mercuryRecCreatorArch := TMercury230ReqCreatorForTestArch.Create(ArchRequest, RecDetails);
end;

procedure TMercury230ReqCreatorTest.TearDown;
begin
  inherited;
  mercuryRecCreator.Free();
  mercuryRecCreatorArch.Free();
end;

{ TMercury230ReqCreatorForTest }

function TMercury230ReqCreatorForTest.CanRequestArchives: bool;
begin
  Result := false;
end;

procedure TMercury230ReqCreatorForTest.CheckDevStates;
begin
  // ничего не делаем
end;

function TMercury230ReqCreatorForTest.NeedRequestLastArchAddr: bool;
begin
  Result := true;
end;

function TMercury230ReqCreatorForTest.NeedRequestMeterInfo: bool;
begin
  Result := true;
end;

procedure TMercury230ReqCreatorTest.Archives;
begin
  mercuryRecCreatorArch.AddArchiveRequestsToSendBuf();
  Check(ReqList.Count = 48 * 20);
  CheckReqHexString(0,           '31, 6, 3, AF, 40, F, CD, 9B');
	CheckReqHexString(48 * 20 - 1, '31, 6, 3, EB, 30, F, A8, 4E');
end;

procedure TMercury230ReqCreatorTest.Archives_Utils();
begin
  Check(mercuryRecCreatorArch.ArchRecordAddr(mercuryRecCreatorArch.LastArchUtime) = mercuryRecCreatorArch.LastArchAddr);
  Check(mercuryRecCreatorArch.ArchRecordAddr(mercuryRecCreatorArch.LastArchUtime - UTC_HOUR) = mercuryRecCreatorArch.LastArchAddr - $20);
  Check(mercuryRecCreatorArch.ArchRecordAddr(EncodeDateTimeUTC(2001, 01, 01)) = 0);
  Check(mercuryRecCreatorArch.ArchRecordAddr(EncodeDateTimeUTC(YearOf(Now()) + 1, 01, 01)) = 0);
end;

{ TMercury230ReqCreatorForTestArch }

procedure TMercury230ReqCreatorForTestArch.CheckDevStates;
begin
  LastArchAddr := $EB30;
  LastArchUtime := EncodeDateTimeUTC(YearOf(Now()), MonthOf(Now()), DayOf(Now())) - UTC_HALFHOUR;
end;

function TMercury230ReqCreatorForTestArch.NeedRequestLastArchAddr: bool;
begin
  Result := false;
end;

function TMercury230ReqCreatorForTestArch.NeedRequestMeterInfo: bool;
begin
  Result := false;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Mercury230', TMercury230ReqCreatorTest.Suite);
end.
