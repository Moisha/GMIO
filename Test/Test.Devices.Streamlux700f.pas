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
    procedure String1;
    procedure StringWithEmpties;
    procedure RealData;
  end;

implementation

uses
  Threads.ResponceParser, Devices.ModbusBase, SysUtils;

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

procedure TStreamlux700fParserTest.RealData;
const
  ai_vals: array [1..4] of double = (31.55, 0.34, 3.13, 21);
var
  i: int;
begin
  gbv.ReqDetails.rqtp := rqtADCP_Channel_Master;
  gbv.gmTime := NowGM();
  gbv.SetBufRec('PRDIQ,+196,+743201.31,+3.13,+31.55,+0.34,+91.87,+21.00,+0.00,+0.00,0'#13#10);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_ADCP_CHANNEL_MASTER;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;

  for i := 1 to 4 do
  begin
    TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
    Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'AI' + IntToStr(i));
    Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = ai_vals[i], 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'AI' + IntToStr(i));
  end;

  i := 1;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'MTR' + IntToStr(i));
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'MTR' + IntToStr(i));
  Check(CompareFloatRelative(TLocalSQLWriteThreadForTest(thread).Values[0].Val, 196743201.31), 'MTR' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'MTR' + IntToStr(i));
end;

procedure TStreamlux700fParserTest.String1;
const
  ai_vals: array [1..4] of double = (234.45, 0.65, 2.45, 15.12);
var
  i: int;
begin
  gbv.ReqDetails.rqtp := rqtADCP_Channel_Master;
  gbv.gmTime := NowGM();
  gbv.SetBufRec('PRDIQ, 12, 432456.123, 2.45, 234.45, 0.65, 345.33, 15.12, 2.56, -0.32, 0'#13#10);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_ADCP_CHANNEL_MASTER;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;

  for i := 1 to 4 do
  begin
    TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
    Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'AI' + IntToStr(i));
    Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = ai_vals[i], 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'AI' + IntToStr(i));
  end;

  i := 1;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'MTR' + IntToStr(i));
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'MTR' + IntToStr(i));
  Check(CompareFloatRelative(TLocalSQLWriteThreadForTest(thread).Values[0].Val, 12432456.123), 'MTR' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'MTR' + IntToStr(i));
end;

procedure TStreamlux700fParserTest.StringWithEmpties;
var
  i: int;
begin
  gbv.ReqDetails.rqtp := rqtADCP_Channel_Master;
  gbv.gmTime := NowGM();
  gbv.SetBufRec('PRDIQ,-140674992,-0.000000001,,,,,+21.00,+0.00,+0.00,10'#13#10);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_ADCP_CHANNEL_MASTER;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;

  for i := 1 to 3 do
  begin
    TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
    Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresEmpty, 'AI' + IntToStr(i));
  end;

  i := 4;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'AI' + IntToStr(i));
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'AI' + IntToStr(i));
  Check(CompareFloatRelative(TLocalSQLWriteThreadForTest(thread).Values[0].Val, 21), 'AI' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'AI' + IntToStr(i));

  i := 1;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'MTR' + IntToStr(i));
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'MTR' + IntToStr(i));
  Check(CompareFloatRelative(TLocalSQLWriteThreadForTest(thread).Values[0].Val, -140674992e6 - 0.000000001), 'MTR' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'MTR' + IntToStr(i));
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Streamlux700f', TStreamlux700fReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/Streamlux700f', TStreamlux700fParserTest.Suite);
end.
