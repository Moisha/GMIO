program Vacon;

uses
  Forms,
  VaconMain in 'VaconMain.pas' {Form1},
  GMGlobals in '..\GMGlobals.pas',
  SOThread in '..\SOThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
