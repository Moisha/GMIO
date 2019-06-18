unit Test.Devices.DRK.ReqParser;

interface

uses Windows, TestFrameWork, GMGlobals, GMConst, Test.Devices.Base.ReqParser;

type
  TDRKParserTest = class(TDeviceReqParserTestBase)
  private
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure TestParse;
  end;

implementation

uses Threads.ResponceParser, SysUtils, System.Math;

{ TDRKParserTest }

type TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);

function TDRKParserTest.GetDevType: int;
begin
  Result := DEVTYPE_DRK;
end;

function TDRKParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

procedure TrySingles(buf: ArrayOfByte);
var
  s: string;
  i, j, k, l: int;
  resbuf: array[0..3] of byte;
  v: double;
begin
  for i := 0 to 3 do
  begin
    for j := 0 to 3 do
    begin
      if i = j then continue;
      for k := 0 to 3 do
      begin
        if k in [i, j] then continue;
        for l := 0 to 3 do
        begin
          if l in [i, j, k] then continue;
          resbuf[0] := buf[i];
          resbuf[1] := buf[j];
          resbuf[2] := buf[k];
          resbuf[3] := buf[l];
          try
            v := ReadSingle(resbuf, 0);
            s := s + Format('%d%d%d%d - %f - %d'#13#10, [i, j, k, l, v, ReadLongIntInv(resbuf, 0)]);
          except end;
        end;
      end;
    end;
  end;

  ShowMessageBox(s);
end;

procedure TDRKParserTest.TestParse;
var
  buf: ArrayOfByte;
begin
  gbv.ReqDetails.rqtp := rqtDRK;
  gbv.gmTime := NowGM();
  buf := TextNumbersStringToArray('25 00 00 00 00 02 05 12 54 83 75 00 17 50 23');
  gbv.SetBufRec(buf);
  gbv.ReqDetails.AddReqLink(1);
  gbv.ReqDetails.AddReqLink(2);

  Check(TLocalSQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv) = recchnresData, 'RecognizeAndCheckChannel');
  Check(Length(TLocalSQLWriteThreadForTest(thread).Values) = 2);
  Check(CompareValue(1750.23, TLocalSQLWriteThreadForTest(thread).Values[0].Val, 1e-9) = 0, 'flow');
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].UTime = gbv.gmTime, 'flow_time');
  Check(TLocalSQLWriteThreadForTest(thread).Values[0].Chn <> nil, 'flow_chn');
  Check(CompareValue(1254837.5, TLocalSQLWriteThreadForTest(thread).Values[1].Val, 1e-9) = 0, 'NI');
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].UTime = gbv.gmTime, 'ni_time');
  Check(TLocalSQLWriteThreadForTest(thread).Values[1].Chn <> nil, 'ni_chn');
end;

initialization
  RegisterTest('GMIOPSrv/Devices/ÄÐÊ', TDRKParserTest.Suite);
end.

