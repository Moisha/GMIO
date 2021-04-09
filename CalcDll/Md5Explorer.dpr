program Md5Explorer;

uses
  Vcl.Forms,
  Md5ExplorerMain in 'Md5ExplorerMain.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
