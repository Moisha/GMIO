unit Test.Devices.Termotronic;

interface

uses TestFrameWork, GMGlobals, Classes, SysUtils, Devices.Termotronic, Test.Devices.Base.ReqCreator, GMConst,
     Test.Devices.Base.ReqParser, Threads.ResponceParser;

type
  TTermotronicReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    procedure UpdateComLog();
    procedure DoCheckRequests(); override;
    function GetDevType(): int; override;
  published
    procedure LRC();
    procedure PrepareMessage();
    procedure Request;
  end;

  TTermotronicParserTest = class(TDeviceReqParserTestBase)
  private
    FParser: TTermotronicParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure ParseCurrents;
    procedure Thread1;
  end;

implementation

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

{ TTermotronicReqCreatorTest }

procedure TTermotronicReqCreatorTest.DoCheckRequests;
begin
  CheckReqString(0, ':1B040064000E6F'#$D#$A);
end;

function TTermotronicReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_PETERFLOWRS;
end;

procedure TTermotronicReqCreatorTest.LRC;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('3A 30 30 30 34 30 30 30 30 30 30 32 38 44 34 0D 0A');
  Check(Termotronic_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray(' 3A 30 31 31 34 30 43 30 42 30 36 30 30 ' +
                                  ' 34 30 30 30 30 31 30 30 34 30 30 30 34 30 30 30 30 34 30 ' +
                                  ' 39 0D 0A');
  Check(Termotronic_CheckCRC(buf, Length(buf)));
end;

procedure TTermotronicReqCreatorTest.PrepareMessage;
var buf, res: ArrayOfByte;
    s: string;
begin
  buf := TextNumbersStringToArray('00 04 00 00 00 28');
  res := Termotronic_PrepareMessage(buf, Length(buf));
  s := ArrayToString(res, Length(res), false, true);
  Check(s = '3A 30 30 30 34 30 30 30 30 30 30 32 38 44 34 0D 0A');
end;

procedure TTermotronicReqCreatorTest.Request;
var buf: ArrayOfByte;
    s: string;
begin
  buf := Termotronic_BuildValuesRequest(0, 4, 100, 18);
  s := ArrayToString(buf, Length(buf), false, true);
  Check(s = '3A 30 30 30 34 30 30 36 34 30 30 31 32 38 36 0D 0A');
end;

procedure TTermotronicReqCreatorTest.SetUp;
begin
  inherited;
end;

procedure TTermotronicReqCreatorTest.TearDown;
begin
  inherited;
end;

procedure TTermotronicReqCreatorTest.UpdateComLog;
var sl: TStringList;
    s: string;
    i, j, n: int;
    buf: ArrayOfByte;
begin
  Check(true);

  sl := TStringList.Create();
  try
    sl.LoadFromFile('D:\Programs\Delphi\Geomer\GMIO\Doc\ТЕРМОТРОНИК\comlog.txt');
    for i := 0 to sl.Count - 1 do
    begin
      n := Pos('==>>', sl[i]);
      if n > 0 then
      begin
        buf := TextNumbersStringToArray(Copy(sl[i], n + 5, Length(sl[i])));
        s := '';
        for j := 0 to High(buf) do
        begin
          if buf[j] < 32 then
            s := s + '<' + IntToStr(buf[j]) + '>'
          else
            s := s + Char(AnsiChar(buf[j]));
        end;

        if s <> '' then
          sl[i] := sl[i] + '  (' + s + ')';
      end;
    end;

    sl.SaveToFile('D:\Programs\Delphi\Geomer\GMIO\Doc\ТЕРМОТРОНИК\comlog_.txt');
  finally
    sl.Free();
  end;
end;

{ TTermotronicParserTest }

function TTermotronicParserTest.GetDevType: int;
begin
  Result := DEVTYPE_PETERFLOWRS;
end;

function TTermotronicParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TTermotronicParserTest.SetUp;
begin
  inherited;
  FParser := TTermotronicParser.Create();
end;

procedure TTermotronicParserTest.TearDown;
begin
  inherited;
  FParser.Free();
end;

procedure TTermotronicParserTest.Thread1;
var buf: ArrayOfByte;
begin
  gbv.ReqDetails.rqtp := rqtPeterFlowRS_Currents;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('3A 30 31 30 34 32 34 30 30 30 30 30 30 30 30 30 30 31 31 30 30 30 30 31 45 44 30 34 33 46 45 31 46 45 33 34 30 35 32 34 31 38 30 36 39 31 30 30 39 38 33 33 46 46 36 31 35 45 39 30 30 30 30 43 35 41 41 34 33 38 46 30 30 30 41 30 30 30 30 42 46 0D 0A');
  gbv.SetBufRec(buf);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_PETERFLOWRS;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.NDevNumber := 1;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Val = 0);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 2;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresUnknown);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 1;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(Abs(TLocalSQLWriteThreadForTest(thread).Values[0].Val - 72.5) < 0.1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 2;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(Abs(TLocalSQLWriteThreadForTest(thread).Values[0].Val - 1.3) < 0.1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 3;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 1);
  Check(Abs(TLocalSQLWriteThreadForTest(thread).Values[0].Val - 93) < 0.1);
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime);

  TLocalSQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_CNT_MTR;
  TLocalSQLWriteThreadForTest(thread).ChannelIds.N_Src := 4;
  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresUnknown);
end;

procedure TTermotronicParserTest.ParseCurrents;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('3A 30 31 30 34 32 34 30 30 30 30 30 30 30 30 30 30 31 31 30 30 30 30 31 45 44 30 34 33 46 45 31 46 45 33 34 30 35 32 34 31 38 30 36 39 31 30 30 39 38 33 33 46 46 36 31 35 45 39 30 30 30 30 43 35 41 41 34 33 38 46 30 30 30 41 30 30 30 30 42 46 0D 0A');

  Check(FParser.SetBuffer(buf, Length(buf)));

  // Текущий расход (м3) 100 4 float R/O
  Check(FParser.ReadRegisterSingle(100) = 0);

  // Код ошибки 102 4 unsigned long R/O
  Check(FParser.ReadRegisterULong(102) = $11);

  // Интеграл V+ (м3) 104 8 double R/O
  Check(Abs(FParser.ReadRegisterDouble(104) - 72.5) < 0.1);

  // Интеграл V- (м3) 108 8 double R/O
  Check(Abs(FParser.ReadRegisterDouble(108) - 1.3) < 0.1);

  // Время наработки (мин.) 112 4 unsigned long R/O
  Check(FParser.ReadRegisterULong(112) = 5609);
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Termotronic', TTermotronicReqCreatorTest.Suite);
  RegisterTest('GMIOPSrv/Devices/Termotronic', TTermotronicParserTest.Suite);

end.


