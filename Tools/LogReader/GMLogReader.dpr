program GMLogReader;

{$R 'GMLogReader.res' 'GMLogReader.rc'}

uses
  FastMM4,
  Vcl.Forms,
  LogReaderMain in 'LogReaderMain.pas' {LogReaderMainForm},
  XmlPreview in 'XmlPreview.pas' {XmlPreviewDlg};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLogReaderMainForm, LogReaderMainForm);
  Application.CreateForm(TXmlPreviewDlg, XmlPreviewDlg);
  Application.Run;
end.
