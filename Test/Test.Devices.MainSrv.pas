unit Test.Devices.MainSrv;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst,
     Test.Devices.Base.ReqParser, StdRequest, StdResponce;

type
  TMainSrvReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

  TMainSrvParserTest = class(TDeviceReqParserTestBase)
  private
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Thread1;
  end;

implementation

uses Threads.ResponceParser, Devices.ModbusBase, SysUtils, Math;

{ TGeostreamReqCreatorTest }

procedure TMainSrvReqCreatorTest.DoCheckRequests;
var len: UINT;
    buf: array [0..1000] of byte;
    i: int;
    xml: string;
    req: IXMLGMIORequestType;
begin
  Check(ReqList.Count = 1);
  Check(ReqList[0].BufCnt > 22);

  for i := 0 to High(buf) do
    buf[i] := ReqList[0].Buf[i];
  len := ReadUINT(buf, 18);
  Check(len = int64(ReqList[0].BufCnt - 22));

  xml := string(DecompressBufferToString(buf, 22, len));
  req := LoadXMLData_GMIORequest(xml);
  Check(req.Data.Sources.Count = 1);

  Check(req.Data.Sources[0].Id = 1234);
  Check(Abs(int64(req.Data.Sources[0].UTime1) - NowGM()) < UTC_DAY );
end;

function TMainSrvReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_MAIN_SRV;
end;

{ TGeostreamParserTest }

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

function TMainSrvParserTest.GetDevType: int;
begin
  Result := DEVTYPE_MAIN_SRV;
end;

function TMainSrvParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TMainSrvParserTest.Thread1;
var buf, bufXML: ArrayOfByte;
    i: int;
    resp: IXMLGMIOResponceType;
    src: IXMLSourceType;
    val: IXMLValueType;
const vals: array [0..4] of single = (0, 0, 0, 1804855.625, 149.158630371094);

begin
  gbv.ReqDetails.rqtp := rqtMainSrv;
  gbv.gmTime := NowGM();

  resp := NewGMIOResponce();
  resp.Auth.OK := 1;
  src := resp.Data.Add();
  src.Id := 1234;

  val := src.Add();
  val.Utime := 100;
  val.Val := 100.25;

  val := src.Add();
  val.Utime := 200;
  val.Val := 200.5;

  val := src.Add();
  val.Utime := 300;
  val.Val := 300.75;

  bufXML := CompressStringToBuffer(resp.XML);
  SetLength(buf, Length(bufXML) + 6);
  buf[0] := 255;
  buf[1] := 0;
  WriteUINT(buf, 2, Length(bufXML));
  WriteBuf(buf, 6, bufXML, Length(bufXML));

  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Prm := 1051;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.MainSrvPrmID := 1234;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := GetDevType();
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.NDevNumber := gbv.ReqDetails.DevNumber;

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 3);
  for i := 0 to 2 do
  begin
    Check(CompareValue(TLocalSQLWriteThreadForTest(thread).Values[i].Val, (i + 1) * 100.25) = 0);
    Check(int64(TLocalSQLWriteThreadForTest(thread).Values[i].UTime) = (i + 1) * 100);
  end;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/MainSrv', TMainSrvReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/MainSrv', TMainSrvParserTest.Suite);
end.

