////////////////////////////////////////////
// Проект приложения-клиента
////////////////////////////////////////////

program GMClient;

{$R 'EsLogging.res' 'EsLogging.RC'}

uses
  FastMM4,
  Forms,
  esLogging,
  GMClMain in 'GMClMain.pas' {GMClMainFrm},
  GMGlobals in '..\SharedSrc\GMGlobals.pas',
  Frame.ClientObject in 'Frame.ClientObject.pas' {FrmObject: TFrame},
  Threads.GMClient in 'Threads.GMClient.pas',
  GMClXL in 'GMClXL.pas' {FrmXL},
  AlarmRep in 'AlarmRep.pas' {AlarmRepDlg},
  Frame.ObjectTree in 'Frame.ObjectTree.pas' {ObjTreeFrame: TFrame},
  EditINIWithDevices in 'EditINIWithDevices.pas' {EditINIWithDevicesDlg},
  Frame.EditPrm in 'Frame.EditPrm.pas' {EditPrmWithDevicesFrm: TFrame},
  Frame.ControlOneAggregate in 'Frame.ControlOneAggregate.pas' {ControlOneAggregateFrame: TFrame},
  ControlFrame in 'ControlFrame.pas' {ControlFrm: TFrame},
  Opts in '..\SharedSrc\Opts.pas' {OptsDlg},
  GMLogin in 'GMLogin.pas' {LoginDlg},
  Frame.DiagramAndTable in 'Frame.DiagramAndTable.pas' {FrmDiagramAndTable: TFrame},
  ParamsForDiagram in 'ParamsForDiagram.pas',
  DiagramImage in 'DiagramImage.pas',
  GMDBClasses in '..\SharedSrc\GMDBClasses.pas',
  Devices.Vacon.Common in '..\SharedSrc\Devices.Vacon.Common.pas',
  Frame.ReportBlank in 'Frame.ReportBlank.pas' {FrmReportBlank: TFrame},
  HydroGeoRep in 'HydroGeoRep.pas' {HydroGeoRepDlg},
  GMConst in '..\SharedSrc\GMConst.pas',
  ClientReadObjects in 'ClientReadObjects.pas',
  Devices.UBZ.Common in '..\SharedSrc\Devices.UBZ.Common.pas',
  ConfigXml in '..\SharedSrc\ConfigXml.pas',
  TemplatedRep in 'TemplatedRep.pas' {TemplatedRepDlg},
  SoundCfg in 'SoundCfg.pas' {SoundCfgDlg},
  AppConfigFile in '..\SharedSrc\AppConfigFile.pas',
  StdRequest in '..\SharedSrc\StdRequest.pas',
  StdResponce in '..\SharedSrc\StdResponce.pas',
  Frame.NodeTree in '..\SharedSrc\Frame.NodeTree.pas' {frmNodeTree: TFrame},
  ArrayNodeReader in 'ArrayNodeReader.pas',
  Frame.TimeInterval in '..\SharedSrc\Frame.TimeInterval.pas' {frmSelArchTime: TFrame},
  Frame.BigWindow in 'Frame.BigWindow.pas' {BigWindowFrame: TFrame},
  BigWindowData in 'BigWindowData.pas',
  Connection.Base in '..\SharedSrc\Connection.Base.pas',
  Connection.TCP in '..\SharedSrc\Connection.TCP.pas',
  PasScript in 'PasScript.pas' {PascalDlg},
  h_GMClient in 'h_GMClient.pas',
  fChannelsProps in 'FlexEdit\fChannelsProps.pas' {ChannelsPropForm},
  fChild in 'FlexEdit\fChild.pas' {FlexChildFrame: T},
  fDocProps in 'FlexEdit\fDocProps.pas' {fmDocProps},
  fInspector in 'FlexEdit\fInspector.pas' {fmInspector},
  fLayers in 'FlexEdit\fLayers.pas' {fmLayers},
  FlexEdit.Main in 'FlexEdit\FlexEdit.Main.pas' {EditMainForm},
  fLibItemProps in 'FlexEdit\fLibItemProps.pas' {fmLibItemProps},
  fLibProps in 'FlexEdit\fLibProps.pas' {fmLibProps},
  fLibrary in 'FlexEdit\fLibrary.pas' {fmLibrary},
  fOptions in 'FlexEdit\fOptions.pas' {fmOptions},
  fPreview in 'FlexEdit\fPreview.pas' {fmPreview},
  fUserData in 'FlexEdit\fUserData.pas' {fmUserData},
  ToolMngr in 'FlexEdit\ToolMngr.pas' {ToolContainer},
  BackgroundFrm in 'FlexEdit\PropForms\BackgroundFrm.pas' {BackgroundOptionsForm},
  BrushFrm in 'FlexEdit\PropForms\BrushFrm.pas' {BrushPropForm},
  LineCapFrm in 'FlexEdit\PropForms\LineCapFrm.pas' {LineCapPropForm},
  PenFrm in 'FlexEdit\PropForms\PenFrm.pas' {PenPropForm},
  PictureFrm in 'FlexEdit\PropForms\PictureFrm.pas' {PicturePropForm},
  StrListFrm in 'FlexEdit\PropForms\StrListFrm.pas' {StrListPropForm},
  UserDataFrm in 'FlexEdit\PropForms\UserDataFrm.pas' {UserDataForm},
  ScriptRunner in 'ScriptRunner.pas',
  FlexActions in '..\ThirdParty\Flex\Src\FlexActions.pas',
  FlexAlpha in '..\ThirdParty\Flex\Src\FlexAlpha.pas',
  FlexBase in '..\ThirdParty\Flex\Src\FlexBase.pas',
  FlexControls in '..\ThirdParty\Flex\Src\FlexControls.pas',
  FlexFileFormats in '..\ThirdParty\Flex\Src\FlexFileFormats.pas',
  FlexHistory in '..\ThirdParty\Flex\Src\FlexHistory.pas',
  FlexLibs in '..\ThirdParty\Flex\Src\FlexLibs.pas',
  FlexPath in '..\ThirdParty\Flex\Src\FlexPath.pas',
  FlexProps in '..\ThirdParty\Flex\Src\FlexProps.pas',
  FlexReg in '..\ThirdParty\Flex\Src\FlexReg.pas',
  FlexUtils in '..\ThirdParty\Flex\Src\FlexUtils.pas',
  PropForm.ControlObject in 'FlexEdit\PropForms\PropForm.ControlObject.pas' {ControlObjectPropForm},
  VT_Utils in '..\SharedSrc\VT_Utils.pas',
  Frame.CurrentsDiagramBase in 'Frame.CurrentsDiagramBase.pas' {CurrentsDiagramBaseFrame: TFrame},
  dxStandardControlsSkinHelper in '..\SharedSrc\dxStandardControlsSkinHelper.pas',
  GMDock in 'GMDock.pas',
  ScadaForm in 'ScadaForm.pas' {ScadaFormForDocking},
  BigWindowForm in 'BigWindowForm.pas' {BigWindowFormForDocking},
  Frames.ScriptEditor in 'Frames.ScriptEditor.pas' {ScriptEditorFrame: TFrame},
  Threads.Sound in 'Threads.Sound.pas';

{$R 'GMPClient.res' 'GMPClient.rc'}
{$R 'dx_rus.res'}

begin
  Application.Initialize;
  Application.Title := 'Клиент Инженерные сети онлайн';
  TEsLogging.ConfigureLogging();
  Application.CreateForm(TGMClMainFrm, GMClMainFrm);
  Application.CreateForm(TFrmXL, FrmXL);
  Application.CreateForm(TAlarmRepDlg, AlarmRepDlg);
  Application.CreateForm(TOptsDlg, OptsDlg);
  Application.CreateForm(TLoginDlg, LoginDlg);
  Application.CreateForm(THydroGeoRepDlg, HydroGeoRepDlg);
  Application.CreateForm(TTemplatedRepDlg, TemplatedRepDlg);
  Application.CreateForm(TSoundCfgDlg, SoundCfgDlg);
  Application.CreateForm(TfmInspector, fmInspector);
  Application.CreateForm(TfmLibrary, fmLibrary);
  Application.CreateForm(TfmUserData, fmUserData);
  Application.CreateForm(TfmLayers, fmLayers);
  Application.CreateForm(TEditMainForm, EditMainForm);
  Application.Run;
end.
