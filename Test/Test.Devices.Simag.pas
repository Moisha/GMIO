unit Test.Devices.Simag;

interface

uses Windows, TestFrameWork, GMGlobals, Threads.ResponceParser, GMBlockValues, Test.Devices.Base.ReqCreator;

type
  TSQLWriteThreadForTest = class(TResponceParserThread)
  public
    procedure Execute; override;
    function CheckSimagChannel(gbv: TGeomerBlockValues; AID_Src, AN_Src: int; Val: double): bool;
  end;

  TSimagParserTest = class(TTestCase)
  private
    thr: TSQLWriteThreadForTest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ParseResult();
  end;

  TSimagReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    function GetDevType(): int; override;
    procedure DoCheckRequests; override;
  end;

implementation

uses DateUtils, SysUtils, GMConst, GMSQLQuery, Math;

{ TLogicaTest }

procedure TSimagParserTest.ParseResult;
var gbv: TGeomerBlockValues;
begin
  gbv := TGeomerBlockValues.Create();
  gbv.ReqDetails.rqtp := rqtSimag11;
  gbv.SetBufRec('яяяЫВВАЧСВРОР198377.218750;-2951.881836;+0.015561;100;18115535;344794560;-1.000000;-1.00000');
  Check(thr.CheckSimagChannel(gbv, SRC_CNT_MTR, 1, 198377.218750), 'SRC_CNT_DI, 1');
  Check(thr.CheckSimagChannel(gbv, SRC_CNT_MTR, 2, -2951.881836), 'SRC_CNT_DI, 2');
  Check(thr.CheckSimagChannel(gbv, SRC_AI, 1, 0.015561 * 3600), 'AI, 1');
end;

procedure TSimagParserTest.SetUp;
begin
  inherited;
  thr := TSQLWriteThreadForTest.Create(nil);
end;

procedure TSimagParserTest.TearDown;
begin
  inherited;
  thr.Free();
end;

{ TSQLWriteThreadForTest }

function TSQLWriteThreadForTest.CheckSimagChannel(gbv: TGeomerBlockValues; AID_Src, AN_Src: int; Val: double): bool;
begin
  ChannelIds.ID_Src := AID_Src;
  ChannelIds.N_Src := AN_Src;
  SetLength(Values, 1);
  CheckSimag11Channel(gbv);
  Result := CompareValue(Values[0].Val, Val, 1e-3) = 0;
end;

procedure TSQLWriteThreadForTest.Execute;
begin

end;

{ TSimagReqCreator }

procedure TSimagReqCreatorTest.DoCheckRequests;
begin
  CheckReqString(0, '#19'#13);
end;

function TSimagReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_SIMAG11;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Simag', TSimagParserTest.Suite);
  RegisterTest('GMIOPSrv/Devices/Simag', TSimagReqCreatorTest.Suite);
end.


