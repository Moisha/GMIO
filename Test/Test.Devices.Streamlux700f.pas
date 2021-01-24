unit Test.Devices.Streamlux700f;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst,
     Test.Devices.Base.ReqParser;

type
  TStreamlux700fReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

  TStreamlux700fParserTest = class(TDeviceReqParserTestBase)
  private
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Test1;
    procedure DV;
  end;

implementation

uses
  Threads.ResponceParser, Devices.ModbusBase, SysUtils, System.Math;

{ TADCPChannelMasterReqCreatorTest }

procedure TStreamlux700fReqCreatorTest.DoCheckRequests;
begin
  CheckReqString(0, 'DQD'#13);
  CheckReqString(1, 'DQH'#13);
  CheckReqString(2, 'DQS'#13);
  CheckReqString(3, 'DV'#13);
  CheckReqString(4, 'AI1'#13);
  CheckReqString(5, 'DC'#13);
  CheckReqString(6, 'DL'#13);
  CheckReqString(7, 'DI+'#13);
end;

function TStreamlux700fReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_STREAMLUX700F;
end;

{ TADCPChannelMasterParserTest }

function TStreamlux700fParserTest.GetDevType: int;
begin
  Result := DEVTYPE_STREAMLUX700F;
end;

type
  TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

function TStreamlux700fParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TStreamlux700fParserTest.Test1;
begin
  gbv.ReqDetails.rqtp := rqtStreamlux700f;
  gbv.gmTime := NowGM();
  gbv.SetBufRec('+3.405755E+01m3/m'#13#10);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_STREAMLUX700F;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;

  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'AI1');
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'AI1');
  Check(CompareFloatRelative(TLocalSQLWriteThreadForTest(thread).Values[0].Val, 34.05755), 'AI1');
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'AI1');

  gbv.gmTime := NowGM();
  gbv.SetBufRec('+3.211965E+01 m3'#13#10);
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'MTR1');
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'MTR1');
  Check(CompareFloatRelative(TLocalSQLWriteThreadForTest(thread).Values[0].Val, 32.11965), 'MTR1');
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'MTR1');
end;

procedure TStreamlux700fParserTest.DV;
begin
  gbv.ReqDetails.rqtp := rqtStreamlux700f;
  gbv.gmTime := NowGM();
  gbv.SetBufRec('+0.000000E+00m/s'#13#10);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_STREAMLUX700F;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;

  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'AI1');
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'AI1');
  Check(CompareFloatRelative(TLocalSQLWriteThreadForTest(thread).Values[0].Val, 0), 'AI1');
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'AI1');
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Streamlux700f', TStreamlux700fReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/Streamlux700f', TStreamlux700fParserTest.Suite);
end.
