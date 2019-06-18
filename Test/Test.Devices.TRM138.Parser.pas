unit Test.Devices.TRM138.Parser;

interface

uses TestFrameWork, Devices.Owen, GMGlobals, GMConst, Classes, Test.Devices.Base.ReqParser;

type
  TTRM138SQLWriteThreadForTest = class(TResponceParserThreadForTest);

  TTRM138ReqParserTest = class(TDeviceReqParserTestBase)
  protected
    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure Test1;
  end;

implementation

{ TLogicaReqParserTest }

function TTRM138ReqParserTest.GetDevType: int;
begin
  Result := DEVTYPE_TRM138;
end;

function TTRM138ReqParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TTRM138SQLWriteThreadForTest;
end;

procedure TTRM138ReqParserTest.Test1;
begin
  gbv.ReqDetails.rqtp := rqtTRM138_T1;

  TTRM138SQLWriteThreadForTest(thread).ChannelIds.ID_DevType := DEVTYPE_TRM138;
  TTRM138SQLWriteThreadForTest(thread).ChannelIds.ID_Src := SRC_AI;
  TTRM138SQLWriteThreadForTest(thread).ChannelIds.N_Src := ord(gbv.ReqDetails.rqtp) - ord(rqtTRM138_T1) + 1;

  gbv.SetBufRec(TextNumbersStringToArray('35 77 80 71 77 79 78 79 75 74 85 86 86 77 73 82 77 73 77 82 71 77 72 77 13'), 25);

  TTRM138SQLWriteThreadForTest(thread).RecognizeAndCheckChannel(gbv);
  Check(true); // тут главное, чтобы не упало
end;

initialization
  RegisterTest('GMIOPSrv/Devices/TRM138', TTRM138ReqParserTest.Suite);
end.
