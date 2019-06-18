unit Test.Devices.ModbusTCP;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst, SysUtils,
     Test.Devices.Base.ReqParser, Threads.ResponceParser;

type
  TModbusTCPCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests; override;
  end;

  TModbusRTUTCPConverterCreatorTest = class(TModbusTCPCreatorTest)
  protected
    function GetDevType(): int; override;
    procedure SetUp; override;
  end;

  TModbusTCPParserTest = class(TDeviceReqParserTestBase)
  private
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Fnc_04;
    procedure Fnc_02;
  end;

implementation

{ TModbusTCPCreatorTest }

procedure TModbusTCPCreatorTest.DoCheckRequests;
var i: int;
begin
  CheckReqHexString(0, '0, 0, 0, 0, 0, 6, 1F, 4, 0, 0, 1, 3');
  Check(ReqList[0].ReqLinkCount = 259);
  Check(ReqList[0].ReqLinks[0].id > 0);
  Check(ReqList[0].ReqLinks[1].id = 0);
  Check(ReqList[0].ReqLinks[2].id > 0);
  for i := 3 to 257 do
    Check(ReqList[0].ReqLinks[i].id = 0);
  Check(ReqList[0].ReqLinks[258].id > 0);
  CheckReqHexString(1, '0, 0, 0, 0, 0, 6, 1F, 2, 1, 2, 0, 1');
  Check(ReqList[1].ReqLinkCount = 1);
  Check(ReqList[1].ReqLinks[0].id > 0);
end;

function TModbusTCPCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_ALLEN_BRADLEY_MICRO_8XX;
end;

{ TModbusRTUTCPConverterCreatorTest }

function TModbusRTUTCPConverterCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_MODBUS_RTU;
end;

procedure TModbusRTUTCPConverterCreatorTest.SetUp;
begin
  inherited SetUp;
  ReqCreator.ReqDetails.ID_Device := 2036;
  ReqCreator.ReqDetails.ID_Obj := 5;
  ReqCreator.ReqDetails.DevNumber := 31;
  ReqCreator.ReqDetails.Converter := PROTOCOL_CONVETER_MODBUS_TCP_RTU;
end;

{ TModbusTCPParserTest }

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

function TModbusTCPParserTest.GetDevType: int;
begin
  Result := DEVTYPE_ALLEN_BRADLEY_MICRO_8XX;
end;

function TModbusTCPParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TModbusTCPParserTest.Fnc_04;
var buf: ArrayOfByte;
begin
  gbv.ReqDetails.rqtp := rqtUSER_DEFINED;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('00 05 00 00 00 05 00 04 02 03 E8');
  gbv.SetBufRec(buf);

  gbv.ReqDetails.ReqID := 5;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.NDevNumber := 0;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_USR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 1000);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  buf := TextNumbersStringToArray('00 05 00 00 00 05 00 04 02 FF FD');
  gbv.SetBufRec(buf);

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = -3);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  // колхозом
  buf := TextNumbersStringToArray('00 05 00 00 00 0B 00 04 08 FF FD 00 00 00 00 03 E8');
  gbv.SetBufRec(buf);
  gbv.ReqDetails.ClearReqLink();
  gbv.ReqDetails.ReqLinkCount := 4;
  gbv.ReqDetails.ReqLinks[0].id := 1;
  gbv.ReqDetails.ReqLinks[3].id := 3;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 2);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Chn = TObject(1));
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = -3);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].Chn = TObject(3));
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].Val = 1000);
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].UTime = gbv.gmTime);
end;

procedure TModbusTCPParserTest.Fnc_02;
var buf: ArrayOfByte;
    i: int;
begin
  gbv.ReqDetails.rqtp := rqtUSER_DEFINED;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('00 05 00 00 00 04 00 02 01 01');
  gbv.SetBufRec(buf);

  gbv.ReqDetails.ReqID := 5;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.NDevNumber := 0;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_USR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  // колхозом
  buf := TextNumbersStringToArray('00 05 00 00 00 05 00 02 02 03 01');
  gbv.SetBufRec(buf);
  gbv.ReqDetails.ClearReqLink();
  gbv.ReqDetails.ReqLinkCount := 11;
  gbv.ReqDetails.ReqLinks[0].id := 1;
  gbv.ReqDetails.ReqLinks[7].id := 2;
  gbv.ReqDetails.ReqLinks[8].id := 3;
  gbv.ReqDetails.ReqLinks[9].id := 4;
  gbv.ReqDetails.ReqLinks[10].id := 5;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 5);
  for i := 0 to 4 do
  begin
    Check(TLocalSQLWriteThreadForTest(thread).Values[i].Chn = TObject(i + 1));
    Check(TLocalSQLWriteThreadForTest(thread).Values[i].UTime = gbv.gmTime);
  end;

  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].Val = 0);
  Check(TLocalSQLWriteThreadForTest(thread).Values[2].Val = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[3].Val = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[4].Val = 0);
end;

initialization
  RegisterTest('GMIOPSrv/Devices/ModbusTCP', TModbusTCPCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/ModbusTCP', TModbusTCPParserTest.Suite);
  RegisterTest('GMIOPSrv/Devices/Modbus RTU-TCP', TModbusRTUTCPConverterCreatorTest.Suite);
end.

