unit Test.Devices.Tecon19.Parser;

interface

uses TestFrameWork, Devices.Tecon, Devices.Tecon.Common, GMGlobals, GMConst, Classes, Test.Devices.Base.ReqParser, Math;

type
  TTeconReqParserTest = class(TDeviceReqParserTestBase)
  private
    reqParser: TTecon19AnswerParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Currents;
    procedure Arch;
    procedure K105Autorization();
  end;

implementation

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

{ TLogicaReqParserTest }

procedure TTeconReqParserTest.Arch;
var buf: ArrayOfByte;
    r: TRequestDetails;
    i: int;
begin
  buf := TextNumbersStringToArray('68 42 42 68 0F 01 FF FF FF FF FF FF FF FF FF FF FF FF 00 00 A0 40 00 00 A0 40 00 00 A0 40 00 00 A0 40 FF FF FF FF 00 00 A0 40 00 00 A0 40 00 00 A0 40 00 00 A0 40 00 00 A0 40 00 00 A0 40 00 00 A0 40 00 00 A0 40 80 16');
  r.N_Car := 1;
  r.ReqID := $0F;
  r.UTimeArch := EncodeDateTimeUTC(2015, 01, 10, 14, 00);
  r.rqtp := rqtTECON_HourArch;

  Check(reqParser.Parse(buf, Length(buf), r));
  Check(reqParser.Vals.Count = 12);
  for i := 0 to reqParser.Vals.Count - 1 do
  begin
    Check(reqParser.Vals[i].Val = 5);
    Check(reqParser.Vals[i].UTime = int64(r.UTimeArch) + (3 + IfThen(i >= 4, 1, 0) + i) * UTC_HOUR);
    Check(reqParser.Vals[i].ValType = valueTypeHourArch);
    Check(reqParser.Vals[i].Chn = nil);
  end;
end;

procedure TTeconReqParserTest.Currents;
var buf: ArrayOfByte;
    r: TRequestDetails;
begin
  buf := TextNumbersStringToArray('68 06 06 68 08 01 00 00 C0 3F 08 16');
  r.N_Car := 1;
  r.ReqID := $08;
  r.rqtp := rqtUSER_DEFINED;

  Check(reqParser.Parse(buf, Length(buf), r));
  Check(reqParser.Vals.Count = 1);
  Check(reqParser.Vals[0].Val = 1.5);
  Check(Abs(NowGM() - reqParser.Vals[0].UTime) < 5);
  Check(reqParser.Vals[0].ValType = valueTypeCurrent);
  Check(reqParser.Vals[0].Chn = nil);
end;

function TTeconReqParserTest.GetDevType: int;
begin
  Result := DEVTYPE_TECON_19_01;
end;

function TTeconReqParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TTeconReqParserTest.K105Autorization;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('68 08 08 68 41 25 1B 09 C1 06 79 24 EE 16');
  Check(Tecon_CheckK105Authorization(buf, Length(buf)));
end;

procedure TTeconReqParserTest.SetUp;
begin
  inherited;
  reqParser := TTecon19AnswerParser.Create();
end;

procedure TTeconReqParserTest.TearDown;
begin
  reqParser.Free();
  inherited;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Tecon', TTeconReqParserTest.Suite);
end.
