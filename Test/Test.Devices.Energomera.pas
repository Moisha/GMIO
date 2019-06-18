unit Test.Devices.Energomera;

interface

uses TestFrameWork, GMGlobals, Classes, IEC_2061107, SysUtils;

type
  TEnergomeraTest = class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LRC();
  end;

implementation

{ TEnergomeraTest }

procedure TEnergomeraTest.LRC;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('01 52 31 02 4D 4F 44 45 4C 28 29 03 4A');
  Check(IEC2061107_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray('02 4D 4F 44 45 4C 28 33 29 0D 0A 03 0F');
  Check(IEC2061107_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray('01 52 31 02 45 54 30 50 45 28 29 03 37');
  Check(IEC2061107_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray('02 45 54 30 50 45 28 30 2E 34 32 35 31 33 34 34 29' +
                                    ' 0D 0A 45 54 30 50 45 28 30 2E 34 32 35 31 33 34' +
                                    ' 34 29 0D 0A 45 54 30 50 45 28 30 2E 30 29 0D 0A' +
                                    ' 45 54 30 50 45 28 30 2E 30 29 0D 0A 45 54 30 50' +
                                    ' 45 28 30 2E 30 29 0D 0A 45 54 30 50 45 28 30 2E' +
                                    ' 30 29 0D 0A 03 69');

  Check(IEC2061107_CheckCRC(buf, Length(buf)));
end;

procedure TEnergomeraTest.SetUp;
begin
  inherited;

end;

procedure TEnergomeraTest.TearDown;
begin
  inherited;

end;

initialization
  RegisterTest('GMIOPSrv/Devices/Energomera', TEnergomeraTest.Suite);
end.


