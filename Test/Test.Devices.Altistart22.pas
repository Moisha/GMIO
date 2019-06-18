unit Test.Devices.Altistart22;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst,
     Test.Devices.Base.ReqParser;

type
  TAltistart22ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

  TAltistart22ParserTest = class(TDeviceReqParserTestBase)
  private
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Thread1;
    procedure Thread2;
  end;

implementation

uses Threads.ResponceParser, Devices.ModbusBase, SysUtils;

{ TAltistart22ReqCreatorTest }

procedure TAltistart22ReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, '$1D, 3, 1, 1, 0, $17, $57, $A4');
end;

function TAltistart22ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_ALTISTART_22;
end;

{ TAltistart22ParserTest }

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

function TAltistart22ParserTest.GetDevType: int;
begin
  Result := DEVTYPE_ALTISTART_22;
end;

function TAltistart22ParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TAltistart22ParserTest.Thread1;
var buf: ArrayOfByte;
    i: int;
begin
  gbv.ReqDetails.rqtp := rqtAltistart22_All;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('29 03 46 ' +
                                  '00 01 02 03 04 05 06 07 ' +
                                  '00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ' +
                                  '00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 08 09 ' +
                                  '00 00', false); // под CRC
  Modbus_CRC(buf, Length(buf) - 2);
  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_ALTISTART_22;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.NDevNumber := gbv.ReqDetails.DevNumber;

  for i := 1 to 4 do
  begin
    TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
    Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'AI' + IntToStr(i));
    Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = ((i - 1) * 2) * 256 + ((i - 1) * 2 + 1), 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'AI' + IntToStr(i));
  end;

  i := 1;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_DI;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'DI' + IntToStr(i));
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'DI' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = $0809, 'DI' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'DI' + IntToStr(i));
end;

procedure TAltistart22ParserTest.Thread2;
var buf: ArrayOfByte;
    i: int;
begin
  gbv.ReqDetails.rqtp := rqtAltistart22_All;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('01 03 2E 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 04 00 01 40 00 00 00 00 00 04 00 00 00 00 00 00 00 00 00 00 00 06 00 11 F3 F1');
  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_ALTISTART_22;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.NDevNumber := 1;

  for i := 1 to 4 do
  begin
    TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
    Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'AI' + IntToStr(i));
    Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 0, 'AI' + IntToStr(i));
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'AI' + IntToStr(i));
  end;

  i := 1;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_DI;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'DI' + IntToStr(i));
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, 'DI' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = $0011, 'DI' + IntToStr(i));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'DI' + IntToStr(i));
end;


initialization
  RegisterTest('GMIOPSrv/Devices/Altistart22', TAltistart22ReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/Altistart22', TAltistart22ParserTest.Suite);
end.

