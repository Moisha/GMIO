program GMOPC;

{$R 'GMOPC.res' 'GMOPC.rc'}

uses
  FastMM4,
  Forms,
  GMOpcMain in 'GMOpcMain.pas' {GMOpcMainForm},
  GMOPCSrv in 'GMOPCSrv.pas',
  Threads.GMClient in '..\Client\Threads.GMClient.pas',
  OPCDataThread in 'OPCDataThread.pas',
  Opts in '..\SharedSrc\Opts.pas' {OptsDlg},
  ComLog in '..\SharedSrc\ComLog.pas' {ComLogFrm},
  prOpcComn in 'prOPC\prOpcComn.pas',
  prOpcDa in 'prOPC\prOpcDa.pas',
  prOpcEnum in 'prOPC\prOpcEnum.pas',
  prOpcError in 'prOPC\prOpcError.pas',
  prOpcServer in 'prOPC\prOpcServer.pas',
  prOpcTypes in 'prOPC\prOpcTypes.pas',
  prOpcUtils in 'prOPC\prOpcUtils.pas',
  prOpcVarUtils in 'prOPC\prOpcVarUtils.pas',
  TimeZone in '..\SharedSrc\TimeZone.pas';

begin
  Application.Initialize;
  Application.CreateForm(TGMOpcMainForm, GMOpcMainForm);
  Application.CreateForm(TOptsDlg, OptsDlg);
  Application.CreateForm(TComLogFrm, ComLogFrm);
  Application.Run;
end.
