unit Test.Devices.Logica.SPT941.Parser;

interface

uses TestFrameWork, GMGlobals, Classes, Test.Devices.Base.ReqParser;

type
  TSPT941ReqParserTest = class(TDeviceReqParserTestBase)
  protected
    function GetDevType: int; override;
    function GetThreadClass: TSQLWriteThreadForTestClass; override;
  published
    procedure Arch;
    procedure Currents;
  end;

implementation

uses DateUtils, SysUtils, GMConst, Math, GMSqlQuery, Threads.ResponceParser;

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

{ TSPT941ReqParserTest }

procedure TSPT941ReqParserTest.Arch;
var
  thr: TLocalSQLWriteThreadForTest;
  req: ArrayOfByte;
begin
  thr := TLocalSQLWriteThreadForTest(thread);
  req := TextNumbersStringToArray('10 1B 90 98 00 0E 00 61 04 05 FF FF 00 00 0A 49 04 11 05 11 12 D1 60');
  WriteBuf(gbv.ReqDetails.buf, 0, req, Length(req));
  gbv.SetBufRec(TextNumbersStringToArray(
    '10 1B 90 98 00 15 03 61 49 04 11 05 11 12 30 82 00 FA 47 04 00 00 00 12 48 04 11 05 11 02 41 04 07 00 00 00 43 04 00 00 ' +
    '00 00 43 04 00 00 00 00 05 00 43 04 00 00 00 00 43 04 00 00 00 00 05 00 43 04 E0 7D EF 3C 43 04 00 00 00 00 43 04 00 00 ' +
    '00 00 43 04 00 00 80 3F 05 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 ' +
    '43 04 00 00 00 00 43 04 00 00 00 00 05 00 43 04 00 00 80 3F 43 04 00 00 80 3F 43 04 00 00 00 00 43 04 00 00 00 00 43 04 ' +
    '00 00 00 00 43 04 00 00 80 3F 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 ' +
    '43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 ' +
    '00 00 43 04 00 00 00 00 4B 08 00 00 00 00 00 00 00 00 4B 08 00 01 00 00 00 00 00 00 49 04 11 05 11 13 30 82 00 FA 47 04 ' +
    '00 00 00 13 48 04 11 05 11 02 41 04 07 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 05 00 43 04 00 00 00 00 43 04 00 00 ' +
    '00 00 05 00 43 04 D1 29 93 3F 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 80 3F 05 00 43 04 2C 87 38 40 43 04 00 00 ' +
    '00 00 43 04 00 00 00 00 43 04 F9 82 38 40 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 05 00 43 04 00 00 80 3F ' +
    '43 04 00 00 80 3F 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 80 3F 43 04 00 00 00 00 43 04 00 00 ' +
    '00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 ' +
    '00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 4B 08 00 00 00 00 00 00 00 00 4B 08 ' +
    '00 01 00 00 00 00 00 00 49 04 11 05 11 14 30 82 00 FA 47 04 00 00 00 14 48 04 11 05 11 02 41 04 07 00 00 00 43 04 00 00 ' +
    '00 00 43 04 00 00 00 00 05 00 43 04 00 00 00 00 43 04 00 00 00 00 05 00 43 04 20 40 F9 3C 43 04 00 00 00 00 43 04 00 00 ' +
    '00 00 43 04 00 00 80 3F 05 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 ' +
    '43 04 00 00 00 00 43 04 00 00 00 00 05 00 43 04 00 00 80 3F 43 04 00 00 80 3F 43 04 00 00 00 00 43 04 00 00 00 00 43 04 ' +
    '00 00 00 00 43 04 00 00 80 3F 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 ' +
    '43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 00 00 43 04 00 00 ' +
    '00 00 43 04 00 00 00 00 4B 08 00 00 00 00 00 00 00 00 4B 08 00 01 00 00 00 00 00 00 49 04 11 05 11 15 30 00 55 78 '
  ));

  gbv.ReqDetails.rqtp := rqtSPT941;
  Check(RecognizeAndCheckChannel() = recchnresData);
  Check(Length(thr.Values) > 0);
end;

procedure TSPT941ReqParserTest.Currents;
begin
  // ѕроверка парсинга текущих гораздо полнее организована по 943
  // Ќет смысла повтор€тьс€
  Check(true);
end;

function TSPT941ReqParserTest.GetDevType: int;
begin
  Result := DEVTYPE_SPT_941;
end;

function TSPT941ReqParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Logica', TSPT941ReqParserTest.Suite);
end.
