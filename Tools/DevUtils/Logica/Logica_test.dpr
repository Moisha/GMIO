program Logica_test;

uses
  Forms,
  Logica_Main in 'Logica_Main.pas' {Form1},
  Logica_Utils in '..\..\SharedSrc\Logica_Utils.pas',
  SOThread in '..\..\SharedSrc\SOThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
