unit Test.Devices.Logica.SPT943.Parser;

interface

uses TestFrameWork, GMGlobals, Classes, Test.Devices.Base.ReqParser;

type
  TSPT943ReqParserTest = class(TDeviceReqParserTestBase)
  protected
    function GetDevType: int; override;
    function GetThreadClass: TSQLWriteThreadForTestClass; override;
  published
    procedure Currents0;
    procedure Currents1;
  end;

implementation

uses DateUtils, SysUtils, GMConst, Math, GMSqlQuery, Threads.ResponceParser;

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

{ TSPT941ReqParserTest }

function TSPT943ReqParserTest.GetDevType: int;
begin
  Result := DEVTYPE_SPT_943;
end;

function TSPT943ReqParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TSPT943ReqParserTest.Currents0;
var thr: TLocalSQLWriteThreadForTest;
begin
  thr := TLocalSQLWriteThreadForTest(thread);
  gbv.SetBufRec(TextNumbersStringToArray('10 22 90 01 00 0F 00 72 05 00 05 00 44 08 00 00 00 00 00 00 00 00 56 84'));
  gbv.ReqDetails.rqtp := rqtSPT943_0;
  gbv.ReqDetails.ReqLinkCount := 3;
  gbv.ReqDetails.ReqLinks[0].id := 1;
  gbv.ReqDetails.ReqLinks[1].id := 2;
  gbv.ReqDetails.ReqLinks[2].id := 3;
  gbv.ReqDetails.SetBuffer(TextNumbersStringToArray('10 22 90 1 0 0 10 72 4A 3 0 2 4 4A 3 0 3 4 4A 3 0 0 8 49 E4'));
  Check(RecognizeAndCheckChannel() = recchnresData);
  Check(Length(thr.Values) = 1, 'Length(thread.Values)');
  Check(CompareValue(thr.Values[0].Val, 0) = 0, 'Length(thread.Values)');
  Check(thr.Values[0].UTime = gbv.gmTime, 'Values.UTime');
  Check(thr.Values[0].ValType = valueTypeCurrent, 'Values.ValType');
  Check(int(thr.Values[0].Chn) = 3, 'Values.Chn');
end;

procedure TSPT943ReqParserTest.Currents1;
var thr: TLocalSQLWriteThreadForTest;
    i: int;
begin
  thr := TLocalSQLWriteThreadForTest(thread);
  gbv.SetBufRec(TextNumbersStringToArray('10 22 90 02 00 67 00 72 05 00 05 00 05 00 05 00 05 00 05 00 05 00 05 00 05 00 05 00 05 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 ' +
                                                                  '44 08 00 00 00 00 00 00 00 00 12 5F'));
  gbv.ReqDetails.rqtp := rqtSPT943_1;
  gbv.ReqDetails.ReqLinkCount := 19;
  for i := 0 to gbv.ReqDetails.ReqLinkCount - 1 do
    gbv.ReqDetails.ReqLinks[i].id := i + 1;

  gbv.ReqDetails.SetBuffer(TextNumbersStringToArray('10 22 90 1 0 0 10 72 4A 3 0 2 4 4A 3 0 3 4 4A 3 0 0 8 49 E4'));
  gbv.ReqDetails.SetBuffer(TextNumbersStringToArray(
                       '10 22 90 2 0 60 00 ' +  // заголовок
                       '72 ' + // FNC
                       '4A 3 1 1 4 4A 3 1 2 4 4A 3 1 3 4 4A 3 1 4 4 4A 3 1 5 4 4A 3 1 6 4 4A 3 1 7 4 4A 3 1 8 4 4A 3 1 9 4 ' + // DATA
                       '4A 3 1 A 4 4A 3 1 B 4 4A 3 1 0 8 4A 3 1 1 8 4A 3 1 2 8 4A 3 1 3 8 4A 3 1 4 8 4A 3 1 5 8 4A 3 1 6 8 4A 3 1 7 8 ' + // DATA cont.
                       '1D 0F')); // CRC

  Check(RecognizeAndCheckChannel() = recchnresData);
  Check(Length(thr.Values) = 8, 'Length(thread.Values)');
  for i := 0 to 7 do
  begin
    Check(CompareValue(thr.Values[i].Val, 0) = 0, 'Length(thread.Values)');
    Check(thr.Values[i].UTime = gbv.gmTime, 'Values.UTime');
    Check(thr.Values[i].ValType = valueTypeCurrent, 'Values.ValType');
    Check(int(thr.Values[i].Chn) = i + 12, 'Values.Chn');
  end;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Logica', TSPT943ReqParserTest.Suite);
end.


