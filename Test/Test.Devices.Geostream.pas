unit Test.Devices.Geostream;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst,
     Test.Devices.Base.ReqParser;

type
  TGeostream71ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

  TGeostream71ParserTest = class(TDeviceReqParserTestBase)
  private
    procedure TestParse(buf: ArrayOfByte);
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Thread1;
    procedure TCP;
  end;

implementation

uses Threads.ResponceParser, Devices.ModbusBase, SysUtils, Math;

{ TGeostreamReqCreatorTest }

procedure TGeostream71ReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, '1E 04 00 00 00 0A 72 62');
end;

function TGeostream71ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_GEOSTREAM_71;
end;

{ TGeostreamParserTest }

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

function TGeostream71ParserTest.GetDevType: int;
begin
  Result := DEVTYPE_GEOSTREAM_71;
end;

function TGeostream71ParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TGeostream71ParserTest.TestParse(buf: ArrayOfByte);
var i: int;
    src: string;
const vals: array [0..4] of single = (0, 0, 0, 1804855.625, 149.158630371094);
begin
  gbv.ReqDetails.rqtp := rqtGeostream_All;
  gbv.gmTime := NowGM();
  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := GetDevType();
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.NDevNumber := gbv.ReqDetails.DevNumber;

  for i := 1 to 3 do
  begin
    TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i;
    src := ID_SrcToStr(TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src) + IntToStr(TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src);

    Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, src);
    Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, src);
    Check(CompareValue(TLocalSQLWriteThreadForTest(thread).Values[0].Val, vals[i - 1]) = 0, src);
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, src);
  end;

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  for i := 4 to 5 do
  begin
    TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := i - 3;
    src := ID_SrcToStr(TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src) + IntToStr(TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src);
    Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, src);
    Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1, src);
    Check(CompareValue(TLocalSQLWriteThreadForTest(thread).Values[0].Val, vals[i - 1]) = 0, src);
    Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, src);
  end;

  // Это архивные записи, мы их решили не подтягивать, примеры ответа я на всякий случай сохраню.
  // Они могут быть полезны тем, что в них есть ошибки и моточасы
  // AA 64 55 F0 00 00 00 00 00 00 00 00 08 3F 28 16 28 3C 23 00 00 00 20 3F F6 28 5C 3F 1F 81 E6 44 E4 33 57 48 EF 8A 11 43 61 CF
  // AA 64 55 F0 00 00 00 00 00 00 00 00 80 12 28 16 A0 0F 23 00 AE 47 61 3E FC A9 81 3F DB 66 8C 44 D9 13 51 48 EF 8A 11 43 AB 7A
end;

procedure TGeostream71ParserTest.TCP;
var buf: ArrayOfByte;
begin
  gbv.ReqDetails.Converter := PROTOCOL_CONVETER_MODBUS_TCP_RTU;
  gbv.ReqDetails.ReqID := $DD;
  buf := TextNumbersStringToArray('00 DD 00 00 00 17 1E 04 14 00 00 00 00 00 00 00 00 00 00 00 00 49 DC 51 BD 43 15 28 9C');
  TestParse(buf);
end;

procedure TGeostream71ParserTest.Thread1;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('1E 04 14 00 00 00 00 00 00 00 00 00 00 00 00 49 DC 51 BD 43 15 28 9C 00 00');
  Modbus_CRC(buf, Length(buf) - 2);
  TestParse(buf);
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Geostream71', TGeostream71ReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/Geostream71', TGeostream71ParserTest.Suite);
end.

