unit Test.Devices.Ancom;

interface

uses TestFrameWork, Devices.Ancom, GMGlobals;

type
  TAncomTest = class(TTestCase)
  private
    buf: array [0..25] of byte;
    procedure ZeroBuf;
    procedure DoCheck(n: int);
  protected
  published
    procedure CheckId;
  end;

implementation

uses DateUtils, SysUtils, GMConst;

{ TAncomTest }

procedure TAncomTest.ZeroBuf();
var i: int;
begin
  for i := 0 to High(buf) do
    buf[i] := 0;
end;

procedure TAncomTest.DoCheck(n: int);
var res: int;
begin
  res := ReadAncomID(buf, Length(buf));
  Check(res = n, 'res = ' + IntToStr(res) + ' <> ' + IntToStr(n));
end;

procedure TAncomTest.CheckId;
begin
  ZeroBuf();
  WriteString(buf, 0, '111');
  DoCheck(111);

  ZeroBuf();
  WriteString(buf, 1, '111');
  DoCheck(-1);

  ZeroBuf();
  WriteString(buf, 0, '111222');
  DoCheck(111222);

  ZeroBuf();
  WriteString(buf, 0, '111A222');
  DoCheck(-1);

  ZeroBuf();
  WriteString(buf, 0, '111222333');
  DoCheck(-1);

  ZeroBuf();
  WriteString(buf, 0, '12345678');
  DoCheck(12345678);

  ZeroBuf();
  WriteString(buf, 0, '12345678');
  buf[20] := 1;
  DoCheck(12345678);
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Ancom', TAncomTest.Suite);
end.


