unit Test.Devices.Vzlet;

interface

uses Windows, TestFrameWork, Devices.Vzlet;

type
  TVzletTest = class(TTestCase)
  private
  protected
  published
    procedure CRC();
  end;

implementation

{ VzletTest }

procedure TVzletTest.CRC;
var buf: array [0..10] of byte;
begin
  buf[0] := $01;
  buf[1] := $04;
  buf[2] := $C1;
  buf[3] := $AA;
  buf[4] := $00;
  buf[5] := $02;
  buf[6] := $6C;
  buf[7] := $17;

  Check(Vzlet_CheckCRC(buf, 8), 'Vzlet good CRC');

  buf[0] := $02;
  Check(not Vzlet_CheckCRC(buf, 8), 'Vzlet bad CRC');
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Vzlet', TVzletTest.Suite);
end.


