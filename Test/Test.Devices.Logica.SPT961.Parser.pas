unit Test.Devices.Logica.SPT961.Parser;

interface

uses TestFrameWork, Devices.Logica.SPT961, GMGlobals, Classes, Test.Devices.Base.ReqParser;

type
  TSPT961ReqParserTest = class(TDeviceReqParserTestBase)
  private
    reqParser: TSPT961AnswerParser;

    ResDevNumber: int;
    ResType: TSPT961AnswerType;
    ResChn: int;
    ResPrm: int;
    ResValue: double;
    ReqDetails: TRequestDetails;
    ExtData: string;

    procedure CheckValue(buf: ArrayOfByte);
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function TimeArrayDayTestData: ArrayOfByte;
    function TimeArrayHourTestData: ArrayOfByte;

    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    //procedure testArchStructure;
    procedure TimeArrayDay;
    procedure TimeArrayHour();
    procedure EmptyTimeArray;
    procedure Value;

    procedure Thread_Day_FindChn;
    procedure Thread_Hour_FindChn;
  end;

implementation

uses DateUtils, SysUtils, GMConst, Math, GMSqlQuery;

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

{ TLogicaTest }

procedure TSPT961ReqParserTest.SetUp;
begin
  inherited;
  reqParser := TSPT961AnswerParser.Create();
end;

procedure TSPT961ReqParserTest.TearDown;
begin
  inherited;
  reqParser.Free();
end;

function TSPT961ReqParserTest.TimeArrayDayTestData(): ArrayOfByte;
begin
  Result := TextNumbersStringToArray(
    ' FF FF 10 01 00 00 10 1F 16 10 02 09 30 09 37 32 0C' +
    ' 09 31 34 09 31 30 09 32 30 31 33 09 30 09 30 09 30 0C' +
    ' 09 39 09 31 30 09 32 30 31 33 09 30 09 30 09 30 0C' +
    ' 09 30 09 27 43 09 31 34 2D 31 30 2D 31 33 2F 20 30 3A 30 30 3A 30 30 0C' +
    ' 09 30 09 27 43 09 31 33 2D 31 30 2D 31 33 2F 20 30 3A 30 30 3A 30 30 0C' +
    ' 09 30 09 27 43 09 31 32 2D 31 30 2D 31 33 2F 20 30 3A 30 30 3A 30 30 0C' +
    ' 09 30 09 27 43 09 31 31 2D 31 30 2D 31 33 2F 20 30 3A 30 30 3A 30 30 0C' +
    ' 09 30 09 27 43 09 31 30 2D 31 30 2D 31 33 2F 20 30 3A 30 30 3A 30 30 0C' +
    ' 09 30 09 27 43 09 20 39 2D 31 30 2D 31 33 2F 20 30 3A 30 30 3A 30 30 0C' +
    ' 10 03 A9 92');

end;

procedure TSPT961ReqParserTest.TimeArrayDay();
var buf: ArrayOfByte;
    i: int;
    dt: Longword;
begin
  buf := TimeArrayDayTestData();

  Check(LogicaSPT961_CheckCRC(buf, Length(buf)), 'CRC TimeArray');
  Check(reqParser.Parse(buf, Length(buf)), 'Parse TimeArray');
  Check(reqParser.ResDevNumber = 0, 'Parse TimeArray - DevNumber <> 0: ' + IntToStr(reqParser.ResDevNumber));
  Check(reqParser.ResType = logicaAnswerTimeArray, 'Parse TimeArray - Type mismatch');
  Check(reqParser.ResChn = 0, 'Parse TimeArray - Chn <> 0: ' + IntToStr(reqParser.ResChn) );
  Check(reqParser.ResPrm = 72, 'Parse TimeArray - Prm <> 72: ' + IntToStr(reqParser.ResPrm) );
  Check(reqParser.Archive.Count = 6, 'Parse ParseValue - data count <> 6: ' + IntToStr(reqParser.Archive.Count));
  for i := 0 to reqParser.Archive.Count - 1 do
  begin
    Check(reqParser.Archive[i].Val.Val = 0, 'Parse ParseValue[' + IntToStr(i) + '] <> 0: ' + MyFloatToStr(reqParser.Archive[i].Val.Val));
    dt := LocalToUTC(EncodeDate(2013, 10, 14 - i));
    Check(reqParser.Archive[i].Val.UTime = dt, 'Parse ParseValue[' + IntToStr(i) + '].Utime');
  end;
end;

function TSPT961ReqParserTest.TimeArrayHourTestData(): ArrayOfByte;
begin
  Result := TextNumbersStringToArray(
    'FF FF 10 01 0B 0B 10 1F 16 10 02 09 30 09 38 32 0C 09 31 34 09 32 09 32 30 31 34 09 31 37 09 39 09 31 37 0C 09 31 34 09 32 09 32 30 31 34 09 30 09 30 09 30 0C 09 32 30 09 27 43 09 31 ' +
    '34 2D 30 32 2D 31 34 2F 31 37 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 31 35 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 31 34 3A ' +
    '30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 31 33 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 31 32 3A 30 30 3A 30 30 0C 09 32 30 09 27 ' +
    '43 09 31 34 2D 30 32 2D 31 34 2F 31 31 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 31 30 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F ' +
    '20 39 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 38 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 37 3A 30 30 3A 30 30 0C 09 32 ' +
    '30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 36 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 35 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D ' +
    '31 34 2F 20 34 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 33 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 32 3A 30 30 3A 30 30 ' +
    '0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 31 3A 30 30 3A 30 30 0C 09 32 30 09 27 43 09 31 34 2D 30 32 2D 31 34 2F 20 30 3A 30 30 3A 30 30 0C 10 03 75 6D');
end;

procedure TSPT961ReqParserTest.TimeArrayHour();
var buf: ArrayOfByte;
    i: int;
    dt: Longword;
begin
  buf := TimeArrayHourTestData();

  Check(LogicaSPT961_CheckCRC(buf, Length(buf)), 'CRC TimeArray');
  Check(reqParser.Parse(buf, Length(buf)), 'Parse TimeArray');
  Check(reqParser.ResDevNumber = 11, 'Parse TimeArray - DevNumber <> 11: ' + IntToStr(reqParser.ResDevNumber));
  Check(reqParser.ResType = logicaAnswerTimeArray, 'Parse TimeArray - Type mismatch');
  Check(reqParser.ResChn = 0, 'Parse TimeArray - Chn <> 0: ' + IntToStr(reqParser.ResChn) );
  Check(reqParser.ResPrm = 82, 'Parse TimeArray - Prm <> 82: ' + IntToStr(reqParser.ResPrm) );
  Check(reqParser.Archive.Count = 17, 'Parse ParseValue - data count <> 17: ' + IntToStr(reqParser.Archive.Count));
  for i := 0 to reqParser.Archive.Count - 1 do
  begin
    Check(reqParser.Archive[i].Val.Val = 20, 'Parse ParseValue[' + IntToStr(i) + '] <> 20: ' + MyFloatToStr(reqParser.Archive[i].Val.Val));
    // у нас дырка на 16:00, поэтому IfThen(i > 0, 1, 0)
    dt := LocalToUTC(EncodeDateTime(2014, 02, 14, 17 - i - IfThen(i > 0, 1, 0), 0, 0, 0));
    Check(reqParser.Archive[i].Val.UTime = dt, 'Parse ParseValue[' + IntToStr(i) + '].Utime');
  end;
end;

procedure TSPT961ReqParserTest.EmptyTimeArray();
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray(
    ' FF FF 10 01 00 00 10 1F 16 10 02 09 30 09 37 32 0C 09 32 31 09 30 39 09 32' +
    ' 30 31 33 09 30 09 30 09 30 0C 09 31 09 30 39 09 32 30 31 33 09 30 09 30 09 30 0C 10 03 E4 EC');

  Check(LogicaSPT961_CheckCRC(buf, Length(buf)), 'CRC EmptyTimeArray');
  Check(reqParser.Parse(buf, Length(buf)), 'Parse EmptyTimeArray');
  Check(reqParser.ResDevNumber = 0, 'Parse EmptyTimeArray - DevNumber <> 0: ' + IntToStr(reqParser.ResDevNumber));
  Check(reqParser.ResType = logicaAnswerTimeArray, 'Parse EmptyTimeArray - Type mismatch');
  Check(reqParser.ResChn = 0, 'Parse EmptyTimeArray - Chn <> 0: ' + IntToStr(reqParser.ResChn) );
  Check(reqParser.ResPrm = 72, 'Parse EmptyTimeArray - Prm <> 72: ' + IntToStr(reqParser.ResPrm) );
  Check(reqParser.Archive.Count = 0, 'Parse EmptyTimeArray - data count <> 0: ' + IntToStr(reqParser.Archive.Count));
end;

function TSPT961ReqParserTest.GetDevType: int;
begin
  Result := DEVTYPE_SPT_961;
end;

function TSPT961ReqParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TSPT961ReqParserTest.CheckValue(buf: ArrayOfByte);
var r: TRequestDetails;
begin
  Check(LogicaSPT961_CheckCRC(buf, Length(buf)), 'CRC Value');
  Check(reqParser.Parse(buf, Length(buf)), 'ParseValue');

  Check(reqParser.ResDevNumber = ResDevNumber, 'Parse ParseValue - DevNumber <> 0: ' + IntToStr(reqParser.ResDevNumber));
  Check(reqParser.ResType = ResType, 'Parse ParseValue - Type mismatch');
  Check(reqParser.ResChn = ResChn, 'Parse ParseValue - Chn <> 0: ' + IntToStr(reqParser.ResChn) );
  Check(reqParser.ResPrm = ResPrm, 'Parse ParseValue - Prm <> 63: ' + IntToStr(reqParser.ResPrm) );
  Check(reqParser.Value = ResValue, 'Parse ParseValue - Value <> 20: ' + FloatToStr(reqParser.Value) );
  Check(reqParser.Archive.Count = 0, 'Parse ParseValue - Arch not empty');

  r := ReqDetails;
  Check(reqParser.CheckInitialRequest(r, ExtData));
  Check(not reqParser.CheckInitialRequest(r, ''));

  r := ReqDetails;
  r.DevNumber := 80;
  Check(not reqParser.CheckInitialRequest(r, ''));

  r := ReqDetails;
  r.rqtp := rqtSPT961_DAY;
  Check(not reqParser.CheckInitialRequest(r, ''));
end;

procedure TSPT961ReqParserTest.Value;
var buf: ArrayOfByte;
begin
  ResDevNumber := 0;
  ResType := logicaAnswerValue;
  ResChn := 0;
  ResPrm := 63;
  ResValue := 20;

  ReqDetails.DevNumber := ResDevNumber;
  ReqDetails.rqtp := rqtSPT961;
  ExtData := '0 63 83 82';

  // ответ без "соплей", но с номером ведущего устройства $80
  buf := TextNumbersStringToArray('10 01 80 00 10 1F 03 10 02 09 30 09 30 36 33 0C 09 32 30 09 27 43 0C 10 03 D7 E8');
  CheckValue(buf);

  // ответ с соплями и номером 0
  buf := TextNumbersStringToArray('FF FF 10 01 00 00 10 1F 03 10 02 09 30 09 30 36 33 0C 09 32 30 09 27 43 0C 10 03 44 79');
  CheckValue(buf);

  // ответ с соплями и номером 11
  ResDevNumber := 11;
  ReqDetails.DevNumber := ResDevNumber;
  buf := TextNumbersStringToArray('FF FF 10 01 00 0B 10 1F 03 10 02 09 30 09 30 36 33 0C 09 32 30 09 27 43 0C 10 03 AC B2');
  CheckValue(buf);

  // (||Б|||||||2|155||2.874|кгс/cм2|||uЮ)
  buf := TextNumbersStringToArray('10 01 81 01 10 1F 03 10 02 09 32 09 31 35 35 0C 09 32 2E 38 37 34 09 AA A3 E1 2F 63 AC 32 0C 10 03 75 9E');

  ResDevNumber := 1;
  ResType := logicaAnswerValue;
  ResChn := 2;
  ResPrm := 155;
  ResValue := 2.874;

  ReqDetails.DevNumber := ResDevNumber;
  ReqDetails.rqtp := rqtSPT961;
  ExtData := '2 155 206 205';

  CheckValue(buf);
end;

procedure TSPT961ReqParserTest.Thread_Day_FindChn;
var arr: ArrayOfString;
begin
  gbv.SetBufRec(TimeArrayDayTestData());

  gbv.ReqDetails.ID_Obj := 2;
  gbv.ReqDetails.ID_Prm := -1;

  TLocalSQLWriteThreadForTest(thread).ParseReqDetails_SPT(gbv);
  check((gbv.ReqDetails.ID_Prm > 1000) and (gbv.ReqDetails.ID_Prm <= 1015), 'ID_Prm');  // в нужный номер не попадем все равно, попадем хотя бы в нужный прибор
  check(gbv.ReqDetails.rqtp = rqtSPT961_DAY, 'rqtp');
  arr := QueryResultArray_FirstRow('select ID_Device, ID_Src, N_Src from Params where ID_Prm = ' + IntToStr(gbv.ReqDetails.ID_Prm));
  check(arr[0] = '3', 'ID_Device');
  check(arr[1] = '1', 'ID_Src');
  check(arr[2] = '2', 'N_Src');
end;

procedure TSPT961ReqParserTest.Thread_Hour_FindChn;
var arr: ArrayOfString;
begin
  gbv.SetBufRec(TimeArrayHourTestData());

  gbv.ReqDetails.ID_Obj := 2;
  gbv.ReqDetails.ID_Prm := -1;

  TLocalSQLWriteThreadForTest(thread).ParseReqDetails_SPT(gbv);
  check((gbv.ReqDetails.ID_Prm > 1015) and (gbv.ReqDetails.ID_Prm <= 1030), 'ID_Prm');
  check(gbv.ReqDetails.rqtp = rqtSPT961_HOUR, 'rqtp');
  arr := QueryResultArray_FirstRow('select ID_Device, ID_Src, N_Src from Params where ID_Prm = ' + IntToStr(gbv.ReqDetails.ID_Prm));
  check(arr[0] = '4', 'ID_Device');
  check(arr[1] = '1', 'ID_Src');
  check(arr[2] = '1', 'N_Src');
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Logica', TSPT961ReqParserTest.Suite);
end.


