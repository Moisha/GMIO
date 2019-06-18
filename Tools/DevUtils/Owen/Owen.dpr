program Owen;

uses
  Forms,
  OwenMain in 'OwenMain.pas' {Form1},
  SOThread in '..\SOThread.pas',
  GMGlobals in '..\GMGlobals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
