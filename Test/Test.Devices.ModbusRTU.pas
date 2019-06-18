unit Test.Devices.ModbusRTU;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst, SysUtils,
     Test.Devices.Base.ReqParser, Threads.ResponceParser, Devices.ModbusBase;

type
  TModusRTUReqCreatorTest = class(TDeviceReqCreatorTestBase)
  private
    procedure CheckOneFirstReqLink(n: int);
  protected
    procedure SetUp; override;
    function GetDevType(): int; override;
    procedure DoCheckRequests; override;
  published
  end;

  TModusRTUParserTest = class(TDeviceReqParserTestBase)
  private
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Fnc_04;
    procedure Fnc_02;
    procedure CustomDataTypes;
  end;

implementation

uses
  Devices.Modbus.ReqCreatorBase, System.Math;

{ TModusRTUReqCreatorTest }

procedure TModusRTUReqCreatorTest.CheckOneFirstReqLink(n: int);
begin
  CheckEquals(1, ReqList[n].ReqLinkCount);
  Check(ReqList[n].ReqLinks[0].id > 0);
end;

procedure TModusRTUReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, '$24, 4, 0, 0, 0, 3, $B7, $3E');
  Check(ReqList[0].ReqLinkCount = 3);
  Check(ReqList[0].ReqLinks[0].id > 0);
  Check(ReqList[0].ReqLinks[1].id = 0);
  Check(ReqList[0].ReqLinks[2].id > 0);

  CheckReqHexString(1, '$24, 4, 1, 2, 0, 1, $96, $C3');
  CheckOneFirstReqLink(1);

  CheckReqHexString(2, '24 04 03 E7 00 01 86 8C');
  CheckOneFirstReqLink(2);
  CheckEquals(0, ReqList[2].ReqLinks[0].ext);
  CheckReqHexString(3, '24 04 04 4B 00 01 47 D9');
  CheckOneFirstReqLink(3);
  CheckEquals(1, ReqList[3].ReqLinks[0].ext);
  CheckReqHexString(4, '24 04 04 AF 00 02 47 EF');
  CheckOneFirstReqLink(4);
  CheckEquals(2, ReqList[4].ReqLinks[0].ext);
  CheckReqHexString(5, '24 04 05 13 00 02 87 F7');
  CheckOneFirstReqLink(5);
  CheckEquals(3, ReqList[5].ReqLinks[0].ext);
  CheckReqHexString(6, '24 04 05 77 00 02 C6 28');
  CheckOneFirstReqLink(6);
  CheckEquals(4, ReqList[6].ReqLinks[0].ext);
  CheckReqHexString(7, '24 04 05 DB 00 02 06 09');
  CheckOneFirstReqLink(7);
  CheckEquals(5, ReqList[7].ReqLinks[0].ext);
  CheckReqHexString(8, '24 04 06 3F 00 04 C6 78');
  CheckOneFirstReqLink(8);
  CheckEquals(6, ReqList[8].ReqLinks[0].ext);
  CheckReqHexString(9, '24 04 06 A3 00 04 06 56');
  CheckOneFirstReqLink(9);
  CheckEquals(7, ReqList[9].ReqLinks[0].ext);

  CheckReqHexString(10, '$24, 2, 1, 2, 0, 1, $1E, $C3');
  Check(ReqList[10].ReqLinkCount = 1);
  Check(ReqList[10].ReqLinks[0].id > 0);
end;

function TModusRTUReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_MODBUS_RTU;
end;

procedure TModusRTUReqCreatorTest.SetUp;
begin
  inherited;
end;

{ TModusRTUParserTest }

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

function TModusRTUParserTest.GetDevType: int;
begin
  Result := DEVTYPE_MODBUS_RTU;
end;

function TModusRTUParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TModusRTUParserTest.CustomDataTypes;
var buf: ArrayOfByte;
begin
  gbv.ReqDetails.rqtp := rqtUSER_DEFINED;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('24 04 08');
  SetLength(buf, Length(buf) + 10);
  WriteFloat(buf, 3, 2.5);
  Modbus_CRC(buf, Length(buf) - 2);

  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_USR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1700;

  gbv.ReqDetails.ReqLinkCount := 1;
  gbv.ReqDetails.ReqLinks[0].id := 1700;
  gbv.ReqDetails.ReqLinks[0].ext := MODBUS_RESULT_DOUBLE_LITTLE_ENDIAN;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 2.5);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);


  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('24 04 04 1A 2A 43 5E');
  SetLength(buf, Length(buf) + 2);
  Modbus_CRC(buf, Length(buf) - 2);

  gbv.SetBufRec(buf);
  gbv.ReqDetails.ReqLinks[0].ext := MODBUS_RESULT_SINGLE_SHUFFLE;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(Floor(TLocalSQLWriteThreadForTest(thread).Values[0].Val) = 222);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);
end;

procedure TModusRTUParserTest.Fnc_04;
var buf: ArrayOfByte;
begin
  gbv.ReqDetails.rqtp := rqtUSER_DEFINED;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('24 04 02 03 E8');
  SetLength(buf, Length(buf) + 2);
  Modbus_CRC(buf, Length(buf) - 2);

  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_USR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 1000);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  buf := TextNumbersStringToArray('24 04 02 FF FD');
  SetLength(buf, Length(buf) + 2);
  Modbus_CRC(buf, Length(buf) - 2);
  gbv.SetBufRec(buf);

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = -3);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  // колхозом
  buf := TextNumbersStringToArray('24 04 06 FF FD 00 00 03 E8');
  SetLength(buf, Length(buf) + 2);
  Modbus_CRC(buf, Length(buf) - 2);
  gbv.SetBufRec(buf);
  gbv.ReqDetails.ClearReqLink();
  gbv.ReqDetails.ReqLinkCount := 3;
  gbv.ReqDetails.ReqLinks[0].id := 1;
  gbv.ReqDetails.ReqLinks[2].id := 3;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 2);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Chn = TObject(1));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = -3);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].Chn = TObject(3));
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].Val = 1000);
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].UTime = gbv.gmTime);
end;

procedure TModusRTUParserTest.Fnc_02;
var buf: ArrayOfByte;
    i: int;
begin
  gbv.ReqDetails.rqtp := rqtUSER_DEFINED;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('24 02 01 01');
  SetLength(buf, Length(buf) + 2);
  Modbus_CRC(buf, Length(buf) - 2);
  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_USR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  // колхозом
  buf := TextNumbersStringToArray('24 02 02 02 01');
  SetLength(buf, Length(buf) + 2);
  Modbus_CRC(buf, Length(buf) - 2);
  gbv.SetBufRec(buf);
  gbv.ReqDetails.ClearReqLink();
  gbv.ReqDetails.ReqLinkCount := 11;
  gbv.ReqDetails.ReqLinks[0].id := 1;
  gbv.ReqDetails.ReqLinks[8].id := 2;
  gbv.ReqDetails.ReqLinks[9].id := 3;
  gbv.ReqDetails.ReqLinks[10].id := 4;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 4);
  for i := 0 to 3 do
  begin
    Check(TLocalSQLWriteThreadForTest(thread).Values[i].Chn = TObject(i + 1));
    Check(TLocalSQLWriteThreadForTest(thread).Values[i].UTime = gbv.gmTime);
  end;

  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].Val = 0);
  Check(TLocalSQLWriteThreadForTest(thread).Values[2].Val = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[3].Val = 0);
end;

initialization
  RegisterTest('GMIOPSrv/Devices/ModusRTU', TModusRTUReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/ModusRTU', TModusRTUParserTest.Suite);
end.

