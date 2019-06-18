unit Test.Devices.Base.ReqParser;

interface

uses Windows, TestFrameWork, GMGlobals, Threads.ResponceParser, GMBlockValues;

type
  TResponceParserThreadForTest = class(TResponceParserThread)
  protected
    procedure Execute; override;
  end;

  TSQLWriteThreadForTestClass = class of TResponceParserThreadForTest;

  TDeviceReqParserTestBase = class(TTestCase)
  protected
    thread: TResponceParserThreadForTest;
    gbv: TGeomerBlockValues;

    procedure SetUp; override;
    procedure TearDown; override;
    function GetDevType(): int; virtual; abstract;
    function GetThreadClass(): TSQLWriteThreadForTestClass; virtual;

    function RecognizeAndCheckChannel(): TRecognizeChannelResult;
  end;

implementation

{ TDeviceReqParserTestBase }

function TDeviceReqParserTestBase.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TResponceParserThreadForTest;
end;

function TDeviceReqParserTestBase.RecognizeAndCheckChannel: TRecognizeChannelResult;
begin
  Result := thread.RecognizeAndCheckChannel(gbv);
end;

procedure TDeviceReqParserTestBase.SetUp;
begin
  inherited;

  gbv := TGeomerBlockValues.Create();
  gbv.gbt := gbt485;
  gbv.gmTime := NowGM();

  gbv.ReqDetails.ClearReqLink();
  gbv.ReqDetails.ID_Device := 1000 + GetDevType();
  gbv.ReqDetails.ID_DevType := GetDevType();
  gbv.ReqDetails.DevNumber := GetDevType();

  thread := GetThreadClass().Create(nil);
  thread.ChannelIDs.ID_Device := gbv.ReqDetails.ID_Device;
  thread.ChannelIDs.ID_DevType := gbv.ReqDetails.ID_DevType;
  thread.ChannelIDs.NDevNumber := gbv.ReqDetails.DevNumber;
end;

procedure TDeviceReqParserTestBase.TearDown;
begin
  inherited;

  thread.Free();
  gbv.Free();
end;

{ TSQLWriteThreadForTest }

procedure TResponceParserThreadForTest.Execute;
begin

end;

end.
