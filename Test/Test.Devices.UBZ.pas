unit Test.Devices.UBZ;

interface

uses Windows, TestFrameWork, GMGlobals, Test.Devices.Base.ReqCreator, GMConst, Threads.ResponceParser, Test.Devices.Base.ReqParser;

type
  TUBZReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  end;

  TUBZParserTest = class(TDeviceReqParserTestBase)
  private
    procedure U_CheckValues;
    procedure U_CheckValues_OneValue(nsrc, val: int);
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure U;
  end;

  TUBZCommon = class(TTestCase)
  published
    procedure DecodedataBlock;
  end;

implementation

uses
  Devices.ModbusBase, GeomerLastValue, Devices.UBZ.Common, System.Classes;

{ TUBZReqCreatorTest }

procedure TUBZReqCreatorTest.DoCheckRequests;
begin
  CheckReqHexString(0, '03 03 00 96 00 02 25 C5');
  CheckReqHexString(1, '3, 3, 0, 64, 0, 4, 4, 34');
  CheckReqHexString(2, '3, 3, 0, 72, 0, 3, A4, 32');
  CheckReqHexString(3, '3, 3, 0, D0, 0, 1, 84, 11');
  CheckReqHexString(4, '3, 3, 0, 87, 0, 1, 35, C1');
  CheckReqHexString(5, '3, 3, 0, F0, 0, 3, 4, 1A');
  CheckReqHexString(6, '3, 3, 0, F3, 0, C, B4, 1E');
  CheckReqHexString(7, '3, 3, 0, FF, 0, 8, 75, DE');
end;

function TUBZReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_UBZ;
end;

{ TLocalResponceParserThreadForTest }

type
  TLocalResponceParserThreadForTest = class(TResponceParserThreadForTest)
  private
    Fubz: TGeomerLastValue;
  protected
    function GetUBZ(ID_Obj, NAddr: int): TGeomerLastValue; override;
  public
    destructor Destroy; override;
  end;

function TLocalResponceParserThreadForTest.GetUBZ(ID_Obj, NAddr: int): TGeomerLastValue;
begin
  if Fubz = nil then
    Fubz := TGeomerLastValue.Create();

  Fubz.ID_Obj := ID_Obj;
  Fubz.NAddr := NAddr;
  Fubz.ID_DevType := DEVTYPE_UBZ;

  Result := FUBZ;
end;

destructor TLocalResponceParserThreadForTest.Destroy;
begin
  Fubz.Free();
  inherited;
end;

{ TUBZParserTest }

function TUBZParserTest.GetDevType: int;
begin
  Result := DEVTYPE_UBZ;
end;

function TUBZParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalResponceParserThreadForTest;
end;

procedure TUBZParserTest.U_CheckValues_OneValue(nsrc, val: int);
begin
  TLocalResponceParserThreadForTest(thread).ChannelIds.N_Src := nsrc;
  Check(TLocalResponceParserThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  CheckEquals(1, Length(TLocalResponceParserThreadForTest(thread).Values));
  CheckEquals(val, TLocalResponceParserThreadForTest(thread).Values[0].Val);
end;

procedure TUBZParserTest.U_CheckValues;
begin
  TLocalResponceParserThreadForTest(thread).ProcessUBZDataBlock(gbv);

  TLocalResponceParserThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;
  U_CheckValues_OneValue(1, $6F);
  U_CheckValues_OneValue(2, $6E);
  U_CheckValues_OneValue(3, $02);
end;

procedure TUBZParserTest.U;
var buf: ArrayOfByte;
begin
  gbv.ReqDetails.rqtp := rqtUBZ_U;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('03 03 06 00 6F 00 6E 00 02');
  SetLength(buf, Length(buf) + 2);
  Modbus_CRC(buf, Length(buf) - 2);

  gbv.SetBufRec(buf);
  U_CheckValues();

  gbv.ReqDetails.rqtp := rqtUBZ_U;
  gbv.ReqDetails.ReqID := 0;
  gbv.ReqDetails.Converter := 1;
  buf := TextNumbersStringToArray('00 00 00 00 00 09 03 03 06 00 6F 00 6E 00 02');
  SetLength(buf, Length(buf));
  gbv.SetBufRec(buf);
  U_CheckValues();
end;

{ TUBZCommon }

procedure TUBZCommon.DecodedataBlock;
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    DecodeUBZAlarms(1024, sl);
    Check(sl.Count > 0)
  finally
    sl.Free();
  end;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/UBZ', TUBZReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/UBZ', TUBZParserTest.Suite);
  RegisterTest('GMIOPSrv/Devices/UBZ', TUBZCommon.Suite);
end.

