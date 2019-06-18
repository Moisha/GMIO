////////////////////////////////////////////
// Основной файл проекта
////////////////////////////////////////////

program GMIOSrv;

{$R 'GMIOPSrv.res' 'GMIOPSrv.rc'}

uses
  FastMM4,
  Forms,
  SysUtils,
  Windows,
  IOMain in 'IOMain.pas' {IOMainFrm},
  GMGlobals in '..\SharedSrc\GMGlobals.pas',
  Frame.EditAggrControl in 'Frame.EditAggrControl.pas' {EditAggrFrame: TFrame},
  Frame.ControlOneAddPrm in 'Frame.ControlOneAddPrm.pas' {ControlOneAddPrmFrame: TFrame},
  GMDBClasses in '..\SharedSrc\GMDBClasses.pas',
  GMConst in '..\SharedSrc\GMConst.pas',
  CheckConnStr in 'CheckConnStr.pas' {CheckConnStrDlg},
  VT_Utils in '..\SharedSrc\VT_Utils.pas',
  GMSqlQuery in '..\SharedSrc\GMSqlQuery.pas',
  SysConfig in 'SysConfig.pas' {SysConfigDlg},
  ProgramLogFile in '..\SharedSrc\ProgramLogFile.pas',
  GMGenerics in '..\SharedSrc\GMGenerics.pas',
  AppConfigFile in '..\SharedSrc\AppConfigFile.pas',
  Threads.Base in '..\SharedSrc\Threads.Base.pas',
  ConnParamsStorage in '..\SharedSrc\ConnParamsStorage.pas',
  BaseNodeReader in '..\SharedSrc\BaseNodeReader.pas',
  SQLNodeReader in 'SQLNodeReader.pas',
  Frame.NodeTree in '..\SharedSrc\Frame.NodeTree.pas' {frmNodeTree: TFrame},
  UsefulQueries in '..\SharedSrc\UsefulQueries.pas',
  UserPermissions in 'UserPermissions.pas' {UsersDlg},
  User in 'User.pas' {UserDlg},
  dmSkin in 'dmSkin.pas' {DataModuleSkin: TDataModule};

begin
  Application.Initialize;
  SetThreadLocale(1049);
  GetFormatSettings();
  Application.Title := 'Сервер Инженерные сети онлайн';
  Application.CreateForm(TIOMainFrm, IOMainFrm);
  Application.CreateForm(TUsersDlg, UsersDlg);
  Application.CreateForm(TUserDlg, UserDlg);
  Application.CreateForm(TDataModuleSkin, DataModuleSkin);
  Application.Run;
end.
