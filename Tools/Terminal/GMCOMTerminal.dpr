program GMCOMTerminal;

{$R 'GMCOMTerminal.res' 'GMCOMTerminal.rc'}

uses
  Forms,
  GMTermMain in 'GMTermMain.pas' {Form1},
  Devices.UBZ.Common in '..\..\SharedSrc\Devices.UBZ.Common.pas',
  Devices.Tecon.Common in '..\..\SharedSrc\Devices.Tecon.Common.pas';

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
