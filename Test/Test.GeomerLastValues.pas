unit Test.GeomerLastValues;

interface

uses Windows, TestFrameWork, GMGlobals, Classes, GeomerLastValue;

type
  TGeomerLastValueTest = class(TTestCase)
  private
    glv: TGeomerLastValue;
    procedure SetUpSlowCNT_DI;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SlowCNT_DI();
  end;

implementation

uses DateUtils, SysUtils, GMConst, Math;

{ TGeomerLastValueTest }

procedure TGeomerLastValueTest.SetUp;
begin
  inherited;
  glv := TGeomerLastValue.Create();
end;

procedure TGeomerLastValueTest.SetUpSlowCNT_DI();
var i: int;
    udt: LongWord;
begin
  glv.bCounter := true;

  udt := EncodeDateTimeUTC(2014, 01, 01);
  glv.LastVal := MakeValueFromBase(udt, 99);
  for i := 0 to 15 do
  begin
    udt := EncodeDateTimeUTC(2014, 01, 01) + LongWord((i + 1) * UTC_MINUTE);
    glv.ShiftVals();
    glv.LastVal := MakeValueFromBase(udt, 100);
  end;
end;

procedure TGeomerLastValueTest.SlowCNT_DI;
begin
  SetUpSlowCNT_DI();
  Check(glv.LastVal.Val = 100, 'LastVal');
  Check(glv.PrevVal.Val = 99, 'PrevVal');
  Check(CompareValue(glv.CalcLastVal().Val, 1 / (16 * UTC_MINUTE)) = 0, 'CalcLastVal');
end;

procedure TGeomerLastValueTest.TearDown;
begin
  inherited;
  glv.Free();
end;

initialization
  RegisterTest('GMIOPSrv/Classes/GeomerLastValues', TGeomerLastValueTest.Suite);
end.


