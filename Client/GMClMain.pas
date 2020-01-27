////////////////////////////////////////////
// Главное окно клиента
////////////////////////////////////////////
unit GMClMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GMGlobals, IniFiles, Winapi.Networking.Sockets, Frame.BigWindow,
  Frame.ClientObject, ComCtrls, ImgList, ExtCtrls, Threads.GMClient, AppEvnts, Menus,
  MPlayer, ShellAPI, ControlFrame, GMDBClasses, GMConst, GMDock,
  ClientReadObjects, ConfigXml, Generics.Collections, FlexBase, dxStandardControlsSkinHelper,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, dxSkinsdxBarPainter, dxBar, cxClasses, cxLocalization, dxSkinsForm,
  dxSkinsdxStatusBarPainter, dxStatusBar, dxSkinsdxDockControlPainter, dxDockControl, dxDockPanel, Threads.Sound;

type
  TGMClMainFrm = class(TGMEnhancedScrollForm)
    tmrCheckUpdates: TTimer;
    ApplicationEvents1: TApplicationEvents;
    tmrUpdateTime: TTimer;
    Panel2: TcxGroupBox;
    tmrBlinkAlarms: TTimer;
    tmrScript: TTimer;
    cxLocalizer1: TcxLocalizer;
    dxSkinController1: TdxSkinController;
    Statusbar1: TdxStatusBar;
    dxDockingManager1: TdxDockingManager;
    dxDockPanelEvents: TdxDockPanel;
    pnAlarms: TcxGroupBox;
    lbAlarms: TcxListBox;
    pnBlinkAlarm: TcxGroupBox;
    dxDockPanelObjects: TdxDockPanel;
    pnControl: TcxGroupBox;
    ControlFrm1: TControlFrm;
    ScrollObjFrames: TcxScrollBox;
    lConnectionTest: TcxLabel;
    dxDockSiteMain: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxBarManager1: TdxBarManager;
    dxBarMainMenu: TdxBar;
    miWindowList: TdxBarSubItem;
    dxBarSubItemBigWindows: TdxBarSubItem;
    dxBarButtonObjects: TdxBarButton;
    dxBarButtonEvents: TdxBarButton;
    N2: TdxBarSubItem;
    N3: TdxBarSubItem;
    N7: TdxBarSubItem;
    N10: TdxBarSubItem;
    miCommonRep: TdxBarButton;
    N5: TdxBarButton;
    miHydroGeoRep: TdxBarButton;
    N6: TdxBarButton;
    miConnectionSettings: TdxBarButton;
    N4: TdxBarButton;
    N9: TdxBarButton;
    N1: TdxBarButton;
    miViewMode: TdxBarButton;
    miControlMode: TdxBarButton;
    N11: TdxBarButton;
    miLoadScada: TdxBarButton;
    dxVertContainerDockSite1: TdxVertContainerDockSite;
    odScada: TOpenDialog;
    miEditMode: TdxBarButton;
    dxBarButtonCloseAll: TdxBarButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbAlarmsDblClick(Sender: TObject);
    procedure tmrCheckUpdatesTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure N4Click(Sender: TObject);
    procedure lbAlarmsDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure miCommonRepClick(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miConnectionSettingsClick(Sender: TObject);
    procedure miControlModeClick(Sender: TObject);
    procedure N7Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miHydroGeoRepClick(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure tmrUpdateTimeTimer(Sender: TObject);
    procedure StatusBar1Resize(Sender: TObject);
    procedure N9Click(Sender: TObject);
    procedure tmrBlinkAlarmsTimer(Sender: TObject);
    procedure miLoadScadaClick(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure miWindowListClick(Sender: TObject);
    procedure dxBarButtonObjectsClick(Sender: TObject);
    procedure dxBarButtonEventsClick(Sender: TObject);
    procedure dxSkinController1SkinControl(Sender: TObject; AControl: TWinControl; var UseSkin: Boolean);
    procedure dxSkinController1SkinForm(Sender: TObject; AForm: TCustomForm; var ASkinName: string;
      var UseSkin: Boolean);
    procedure dxDockPanelEventsCloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
    procedure dxBarButtonCloseAllClick(Sender: TObject);
  private
    { Private declarations }
    lObjFrames: TList<TFrmObject>;
    lBigWindows: TList<TBigWindowFrame>;
    ReadObjThr: TClientReadObjectsThread;
    bConnected: bool;

    bSoundForAlarms, bSoundForNoData: bool;
    iBlinkingPanelWidth: int;
    FControlMode: TGMControlMode;
    FSoundThread: TGMSoundThread;

    procedure WMDestroyObject(var Msg: TMessage); message WM_DESTROY_OBJECT;
    procedure WMUpdateObject(var Msg: TMessage); message WM_UPDATE_OBJECT;
    procedure WMAddAlarm(var Msg: TMessage); message WM_ALARM_STATE_CHANGED;
    procedure WMAlarmStateChanged(var Msg: TMessage); message WM_ALARM_CHANGED;
    procedure WMShowControlPanel(var Msg: TMessage); message WM_SHOW_CONTROL_PANEL;
    procedure WMCreateObjects(var Msg: TMessage); message WM_CLIENT_CREATE_OBJECTS;
    procedure WMRequestDiagrams(var Msg: TMessage); message WM_REQUEST_DIAGRAMS;
    procedure WMThreadException(var Msg: TMessage); message WM_THREAD_EXCEPTION;
    procedure CreateObjects;
    procedure EditObjects;
    procedure CheckAllFramesUpdates();
    procedure RestartApplication();
    procedure DestroyFrames;
    procedure ReadAlarms(xml: IGMConfigConfigType);
    function ReadBigWindows(xml: IGMConfigConfigType): TdxDockPanel;
    procedure ClearApplication;
    procedure ReadObjectsFromSrv;
    procedure InitConnectionLabel;
    procedure DoSound(bPlay: bool);
    procedure CheckSound;
    procedure ReadSound(xml: IGMConfigConfigType);
    procedure ShowConnectionError(const txt: string);
    procedure ReadScada(xml: IGMConfigConfigType; toDock: TdxDockPanel);
    procedure DisableMenuSubitemsWhileNotConnected(mi: TdxBarSubItem);
    function CreateScadaPanel(const fn: string): TGMDockPanel;
    procedure CreateNewScadaPage(fn: string);
    //procedure MenuCallWindow(Sender: TObject);
    procedure SaveStartWndData;
    function LoadStartWndData(): bool;
    function StartWndDataFile: string;
    procedure CloseAllDockPanels;
    procedure StopExchangeThread;
    procedure StopTimers;
    procedure ReadBigWindowList(xml: IGMConfigConfigType);
    procedure MenuCallBigWindow(Sender: TObject);
    procedure RequestAllFramesDiagrams;
    procedure PostRefreshObjectTreeMgs;
    procedure ReloadObjects;
    procedure SetControlMode(const Value: TGMControlMode);
    procedure InitialDock(panel: TdxDockPanel; dockType: TdxDockingTypeEx = dtNone);
    function FindDockSiteIfAny: TdxLayoutDockSite;
    procedure DockToEventsPanel(panel: TdxDockPanel);
    function AddBigWindowMenuButtonLink(const buttonCaption: string; clickEvent: TNotifyEvent; buttonTag: int): TdxBarItemLink;
    procedure MenuOpenAllBigWindows(Sender: TObject);
    property ControlMode: TGMControlMode read FControlMode write SetControlMode;
  public
    { Public declarations }
  end;

var
  GMClMainFrm: TGMClMainFrm;

implementation

uses GMClXL, Math, Types, DateUtils, AlarmRep,
  EditINIWithDevices, Opts, GMLogin, HydroGeoRep, TemplatedRep, SoundCfg, ProgramLogFile,
  AppConfigFile, BigWindowData, PasScript, FlexEdit.Main, ScadaForm, BigWindowForm;

{$R *.dfm}

procedure TGMClMainFrm.DestroyFrames();
var i: int;
begin
  for i := 0 to lObjFrames.Count - 1 do
    lObjFrames[i].Free();

  lObjFrames.Clear();
end;

procedure TGMClMainFrm.ReadAlarms(xml: IGMConfigConfigType);
var i: int;
    p: TGMParam;
begin
  // тревоги
  for i := 0 to xml.Alarms.Count - 1 do
  begin
    p := GMParams.ByID(xml.Alarms[i].Id_prm);
    if (p <> nil) and (p.AlarmSignal > 0) then // в аварии записываем только параметры с указанным типом аварии
    begin
      acAlarms.Add().prm := p;
      TGMCOMThreadPool.AddReqCurrentsPrm(p);
    end;
  end;
end;

function TGMClMainFrm.CreateScadaPanel(const fn: string): TGMDockPanel;
begin
  Result := nil;
  if FileExists(fn) then
    Result := CreateDockableScada(fn);
end;

procedure TGMClMainFrm.ReadScada(xml: IGMConfigConfigType; toDock: TdxDockPanel);
var sl: TStringList;
    i: int;
    p: TGMDockPanel;
begin
  sl := TStringList.Create();
  try
    sl.Text := xml.Scada.Filename;
    for i := 0 to sl.Count - 1 do
    begin
      p := CreateScadaPanel(sl[i]);
      p.DockTo(toDock, dtClient, 1);
      toDock := p;
    end;
  except end;
  sl.Free();
end;

procedure TGMClMainFrm.ReadSound(xml: IGMConfigConfigType);
begin
  FSoundThread.AlarmFileName := xml.Sound.AlarmSoundFile;

  bSoundForAlarms := xml.Sound.NeedAlarmSound > 0;
  bSoundForNoData := xml.Sound.NeedNoDataSound > 0;
  iBlinkingPanelWidth := xml.Sound.BlinkingPanelWidth;
end;

procedure TGMClMainFrm.MenuCallBigWindow(Sender: TObject);
var i, idx: int;
begin
  idx := TdxBarButton(Sender).Tag;
  for i := dxDockingController().DockControlCount - 1 downto 0 do
  begin
    if (dxDockingController().DockControls[i] is TBigWindowDockPanel)
       and (TBigWindowDockPanel(dxDockingController().DockControls[i]).BigWindowIndex = idx)  then
    begin
      dxDockingController().DockControls[i].Visible := true;
      dxDockingController().DockControls[i].Activate();
      InitialDock(TGMDockPanel(dxDockingController().DockControls[i]));
      Exit;
    end;
  end;

  InitialDock(CreateDockableBigWindow(idx));
end;

function TGMClMainFrm.AddBigWindowMenuButtonLink(const buttonCaption: string; clickEvent: TNotifyEvent; buttonTag: int): TdxBarItemLink;
var
  b: TdxBarButton;
begin
  Result := dxBarSubItemBigWindows.ItemLinks.AddItem(TdxBarButton);
  b := TdxBarButton(Result.Item);
  b.Caption := buttonCaption;
  b.OnClick := clickEvent;
  b.Tag := buttonTag;
  b.ImageIndex := -1;
end;

procedure TGMClMainFrm.MenuOpenAllBigWindows(Sender: TObject);
begin
  for var i := 0 to dxBarSubItemBigWindows.ItemLinks.Count - 1 do
  begin
    var b := TdxBarButton(dxBarSubItemBigWindows.ItemLinks.Items[i].Item);
    if b.Tag >= 0 then
      b.Click();
  end;
end;

procedure TGMClMainFrm.ReadBigWindowList(xml: IGMConfigConfigType);
var
  i: int;
begin
  BigWindowList.LoadFromINI(xml);

  while dxBarSubItemBigWindows.ItemLinks.Count > 0 do
    dxBarSubItemBigWindows.ItemLinks.Items[0].Free();

  for i := 0 to BigWindowList.Count - 1 do
    AddBigWindowMenuButtonLink(BigWindowList[i].Caption, MenuCallBigWindow, i);

  AddBigWindowMenuButtonLink('Открыть все', MenuOpenAllBigWindows, -1).BeginGroup := true;
end;

function TGMClMainFrm.ReadBigWindows(xml: IGMConfigConfigType): TdxDockPanel;
var p: TGMDockPanel;
    i: int;
begin
  Result := dxDockPanelObjects;

  for i := 0 to BigWindowList.Count - 1 do
  begin
    p := CreateDockableBigWindow(i);
    p.DockTo(Result, dtClient, 1);
    TBigWindowFormForDocking(p.GMForm).BigWindowFrame1.RequestDiagrams();
    Result := p;
  end;
end;

procedure TGMClMainFrm.CreateObjects();

const MAX_OBJECT_ROW = 15;

var f: TFrmObject;
    i, x, y, l, ID_Obj: int;
    xmlObj: IGMConfigObjectType;
    obj: TGMObject;
    xml: IGMConfigConfigType;
    RowHeights: array [0..MAX_OBJECT_ROW - 1] of int;
    toDock: TdxDockPanel;

  function RowTop(n: int): int;
  var i:int;
  begin
    Result := 0;
    for i := 0 to min(Length(RowHeights), n) - 1 do
      inc(Result, RowHeights[i])
  end;

  function LookForXY(x, y: int): IGMConfigObjectType;
  var i: int;
  begin
    Result := nil;

    for i := 0 to xml.Objects.Count - 1 do
      if (xml.Objects[i].X = x) and (xml.Objects[i].Y = y) then
      begin
        Result := xml.Objects[i];
        Exit;
      end;
  end;

  procedure ReadControlParamsList(ID_Obj: int);
  var i, j: int;
  begin
    // с аггрегатов соберем расширенные параметры
    for i := 0 to GMAggregates.Count - 1 do
      if GMAggregates[i].Obj.ID_Obj = ID_Obj then
        for j := 0 to GMAggregates[i].ParamCount - 1 do
        begin
          TGMCOMThreadPool.AddReqCurrentsPrm(GMAggregates[i].Params[j].prm);
        end;

    // аварии на всякий случай опросим все
    for i := 0 to GMParams.Count - 1 do
      if (GMParams[i].Device.Obj.ID_Obj = ID_Obj)
         and GMParams[i].IsAlarm then
      begin
        TGMCOMThreadPool.AddReqCurrentsPrm(GMParams[i]);
      end;
  end;

begin
  DestroyFrames();

  xml := ReadConfigXML();

  l := xml.Common.Alarm_window_height;
  iNoRefreshAlarm := xml.Common.Norefresh_alarm;

  if (l <= 0) or (l > 300) then l := 100;
  pnAlarms.Height:=l;

  for i := 0 to High(RowHeights) do
    RowHeights[i] := 0;

  try
    for y := 0 to MAX_OBJECT_ROW do
    begin
      l := 0;
      for x := 0 to MAX_OBJECT_ROW do
      begin
        xmlObj := LookForXY(x + 1, y + 1);
        if xmlObj <> nil then
        begin
          ID_Obj := xmlObj.Id_obj;
          if ID_Obj > 0 then
          begin
            obj := GMObjects.ObjByID(ID_Obj);
            if obj <> nil then obj.Active := true;
          end;

          f := TFrmObject.Create(self);
          f.Name := 'FrmObject' + IntToStr(y) + IntToStr(x);
          f.Parent := ScrollObjFrames;
          lObjFrames.Add(f);

          f.ReadINI(xmlObj);
          f.Left := l;
          f.Top := RowTop(y);
          f.FrameResize(f);

          l := l + f.Width;
          RowHeights[y] := max(RowHeights[y], f.Height);

          ReadControlParamsList(ID_Obj);
        end;
      end;
    end;

    ReadAlarms(xml);
    ReadSound(xml);
    ReadBigWindowList(xml);

    if lObjFrames.Count = 0 then
      dxDockPanelObjects.Close();

    if not FileExists(StartWndDataFile())
       or not LoadStartWndData() then
    begin
      toDock := ReadBigWindows(xml);
      ReadScada(xml, toDock);
    end;

    PostMessage(Handle, WM_REQUEST_DIAGRAMS, 0, 0);
    CheckAllFramesUpdates();
  finally
    ScrollObjFrames.Visible := true;
  end;
end;

procedure TGMClMainFrm.DoSound(bPlay: bool);
begin
  FSoundThread.Alarm := bPlay;
end;

procedure TGMClMainFrm.dxBarButtonCloseAllClick(Sender: TObject);
begin
  TGMDockPanel.CloseAll();
end;

procedure TGMClMainFrm.dxBarButtonEventsClick(Sender: TObject);
begin
  if not dxDockPanelEvents.Visible then
  begin
    dxDockPanelEvents.Show();
    dxDockPanelEvents.DockTo(dxDockSiteMain, dtBottom, 0);
    dxDockPanelEvents.Activate();
  end
  else
  begin
    dxDockPanelEvents.Hide();
  end;
end;

procedure TGMClMainFrm.dxBarButtonObjectsClick(Sender: TObject);
begin
  if not dxDockPanelObjects.Visible then
  begin
    dxDockPanelObjects.Show();
    InitialDock(dxDockPanelObjects);
  end
  else
  begin
    dxDockPanelObjects.Hide();
  end;
end;

procedure TGMClMainFrm.dxDockPanelEventsCloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
begin
  CanClose := FControlMode = cmEdit;
end;

procedure TGMClMainFrm.dxSkinController1SkinControl(Sender: TObject; AControl: TWinControl; var UseSkin: Boolean);
begin
  UseSkin := true;
end;

procedure TGMClMainFrm.dxSkinController1SkinForm(Sender: TObject; AForm: TCustomForm; var ASkinName: string;
  var UseSkin: Boolean);
begin
  UseSkin := true;
end;

{procedure TGMClMainFrm.MenuCallWindow(Sender: TObject);
begin
  if (Sender is TdxBarButton) and (TdxBarButton(Sender).Tag <> 0) then
    TdxFloatForm(TdxBarButton(Sender).Tag).BringToFront();
end;
}
procedure TGMClMainFrm.miWindowListClick(Sender: TObject);
{var
  b: TdxBarButton;
  l: TdxBarItemLink;
  i: Integer;
}
begin
  // !!!!!!  Надо ли?
  dxBarButtonEvents.Down := dxDockPanelEvents.Visible;
  dxBarButtonObjects.Down := dxDockPanelObjects.Visible;

{  while miWindowList.ItemLinks.Count > 0 do
    miWindowList.ItemLinks.Items[0].Free();

  for i := 0 to dxDockingController().DockControlCount - 1 do
  begin
    if not (dxDockingController().DockControls[i] is TdxFloatDockSite) then
      continue;

    l := miWindowList.ItemLinks.AddItem(TdxBarButton);
    b := TdxBarButton(l.Item);
    b.Caption := TdxFloatDockSite(dxDockingController().DockControls[i]).FloatForm.Caption;
    b.OnClick := MenuCallWindow;
    b.Tag := NativeInt(dxDockingController().DockControls[i].FloatForm);
    b.ImageIndex := dxDockingController().DockControls[i].ImageIndex;
  end;
}
end;

procedure TGMClMainFrm.CheckSound();
var bPlay: bool;
    i: int;
begin
  bPlay := bSoundForAlarms and (lbAlarms.Count > 0);
  if bSoundForNoData then
  begin
    for i := 0 to lObjFrames.Count - 1 do
    begin
      if bPlay then break;

      bPlay := bPlay or lObjFrames[i].NoData;
    end;
  end;

  DoSound(bPlay);
end;

procedure TGMClMainFrm.WMAddAlarm(var Msg: TMessage);
var a: TGMAlarm;
    i, n: int;
begin
  a:=TGMAlarm(Msg.WParam);
  n:=-1;

  for i:=0 to lbAlarms.Count-1 do
    if lbAlarms.Items.Objects[i]=a then
    begin
      n:=i;
      break;
    end;

  case a.alCurrState of
    asUnknown, asNotSet, as1AndQuoted:
      if n>=0 then lbAlarms.Items.Delete(n);
    asSetTo1:
      if n<0 then
        lbAlarms.AddItem(DateTimeToStr(UTCtoLocal(a.utLast1)) + ' - ' + a.GetAlarmTxt(), a);
  end;

  // Включим песенку, если есть аварии
  CheckSound();
end;

procedure TGMClMainFrm.WMUpdateObject(var Msg: TMessage);
var i: int;
    p: TGMBaseParam;
begin
  // Msg.WParam - это TGMBaseParam
  p := TGMBaseParam(Msg.WParam);
  if p <> nil then
  begin
    for i := 0 to lObjFrames.Count - 1 do
    try
      lObjFrames[i].AddValue(p.LastVal, false);
    except end;

{
    for i := 0 to lBigWindows.Count - 1 do
    try
      lBigWindows[i].DiagramAndTable.AddValue(p.LastVal, false);
    except end;
}
    if pnControl.Visible and (p is TGMParam) and (TGMParam(p).Device.Obj.ID_Obj = ControlFrm1.ID_Obj) then
      ControlFrm1.RefreshData();
  end;
end;

procedure TGMClMainFrm.ClearApplication();
begin
  SaveStartWndData();
  StopExchangeThread();
  TryFreeAndNil(ReadObjThr);

  DestroyFrames();
  CloseAllDockPanels();

  lbAlarms.Clear();
  acAlarms.Clear();

  TGMCOMThreadPool.CreateThreads();
end;

procedure TGMClMainFrm.InitConnectionLabel();
begin
  lConnectionTest.Visible := true;
  lConnectionTest.Caption := 'Соединение с сервером ...';
  lConnectionTest.Style.Font.Color := clWindowtext;
end;

procedure TGMClMainFrm.ReadObjectsFromSrv();
begin
  bConnected := false;
  InitConnectionLabel();
  ReadObjThr := TClientReadObjectsThread.Create(false);
end;

procedure TGMClMainFrm.PostRefreshObjectTreeMgs();
var i: int;
begin
  for i := 0 to Screen.FormCount - 1 do
    PostMessage(Screen.Forms[i].Handle, WM_REFRESH_OBJECTS_TREE, 0, 0);
end;

procedure TGMClMainFrm.RestartApplication();
begin
  ClearApplication();
  Application.ProcessMessages();
  CloseAllDockPanels();
  Application.ProcessMessages();
  ReadObjectsFromSrv();
end;

procedure TGMClMainFrm.ReloadObjects();
begin
  ClearApplication();
  CreateObjects();
end;

procedure TGMClMainFrm.EditObjects();
begin
  if TEditINIWithDevicesDlg.EditINI() then
    ReloadObjects();
end;

procedure TGMClMainFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl, ssAlt]) and (Key = VK_F12) then
    EditObjects();

  if (Shift = [ssCtrl, ssAlt]) and (Key = Ord('X')) then
    FrmXL.ShowModal();

  if (Shift = [ssCtrl, ssAlt]) and (Key = Ord('S')) then
    FSoundThread.Alarm := not FSoundThread.Alarm;
end;

procedure TGMClMainFrm.lbAlarmsDblClick(Sender: TObject);
begin
  if lbAlarms.ItemIndex<0 then Exit;

  TGMAlarm(lbAlarms.Items.Objects[lbAlarms.ItemIndex]).Quote();
end;

procedure TGMClMainFrm.RequestAllFramesDiagrams();
var i: int;
begin
  for i := 0 to lObjFrames.Count - 1 do
    lObjFrames[i].RequestDiagrams();
end;

procedure TGMClMainFrm.CheckAllFramesUpdates();
var i: int;
begin
  for i := 0 to lObjFrames.Count - 1 do
    lObjFrames[i].CheckLastData();

  for i := 0 to lBigWindows.Count - 1 do
    lBigWindows[i].CheckLastData();
end;

procedure TGMClMainFrm.tmrCheckUpdatesTimer(Sender: TObject);
begin
  CheckAllFramesUpdates();
end;

function TGMClMainFrm.StartWndDataFile(): string;
begin
  Result := ChangeFileExt(GMMainConfigFile().GetMainINIFileName(), '.wnd');
end;

type
  TdxDockingControllerAccess = class(TdxDockingController);

procedure TGMClMainFrm.SaveStartWndData;
begin
  try
    TdxDockingControllerAccess(dxDockingController()).SaveLayoutToIniFile(StartWndDataFile(), self);
  except
  end;
end;

procedure TGMClMainFrm.SetControlMode(const Value: TGMControlMode);
var i: int;
begin
  FControlMode := Value;
  ControlFrm1.ControlMode := FControlMode;

  for i := 0 to dxDockingController().DockControlCount - 1 do
    dxDockingController().DockControls[i].Dockable := FControlMode = cmEdit;
end;

function TGMClMainFrm.LoadStartWndData: bool;
begin
  try
    TdxDockingControllerAccess(dxDockingController()).LoadLayoutFromIniFile(StartWndDataFile(), self);
    Result := true;
  except
    Result := false;
    CloseAllDockPanels();
  end;
end;

procedure TGMClMainFrm.CloseAllDockPanels();
var i: int;
begin
  for i := dxDockingController().DockControlCount - 1 downto 0 do
  begin
    if dxDockingController().DockControls[i] is TGMDockPanel then
      dxDockingController().DockControls[i].Free();
  end;
end;

procedure TGMClMainFrm.StopTimers();
var i: int;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TTimer then
      TTimer(Components[i]).Enabled := false;
  end;
end;

procedure TGMClMainFrm.StopExchangeThread();
begin
  TGMCOMThreadPool.DestroyThreads();
end;

procedure TGMClMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopTimers();
  SaveStartWndData();
  StopExchangeThread();
end;

procedure TGMClMainFrm.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  ProgramLog().AddException(E.Message + '(' + E.ClassName() + ') addr = ' + IntToStr(int(ExceptAddr())));
end;

function TGMClMainFrm.FindDockSiteIfAny(): TdxLayoutDockSite;
var
  I: Integer;
  tmp: TdxCustomDockControl;
begin
  for I:= 0 to dxDockSiteMain.ChildCount - 1 do
  begin
    tmp := dxDockSiteMain.Children[I];
    if (tmp is TdxLayoutDockSite) and ((tmp as TdxLayoutDockSite).ChildCount = 0) then
    begin
      Result := tmp as TdxLayoutDockSite;
      exit;
    end;
  end;
  Result := nil;
end;

procedure TGMClMainFrm.DockToEventsPanel(panel: TdxDockPanel);
begin
  if dxDockPanelEvents.Visible and (dxDockSiteMain.DockState = dsDocked) then
    case dxDockPanelEvents.DockType of
      dtClient:
        begin
          dxDockPanelEvents.Height := Min(dxDockSiteMain.Height div 2, dxDockPanelEvents.Height);
          panel.DockTo(dxDockSiteMain, dtClient, -1);
        end;
      dtLeft:
        panel.DockTo(dxDockSiteMain, dtRight, -1);
      dtTop:
        begin
          dxDockPanelEvents.Height := Min(dxDockSiteMain.Height div 2, dxDockPanelEvents.Height);
          panel.DockTo(dxDockSiteMain, dtBottom, -1);
        end;
      dtRight:
        panel.DockTo(dxDockSiteMain, dtLeft, -1);
      dtBottom:
        begin
          dxDockPanelEvents.Height := Min(dxDockSiteMain.Height div 2, dxDockPanelEvents.Height);
          panel.DockTo(dxDockSiteMain, dtTop, -1);
        end;
    end
  else
    panel.DockTo(dxDockSiteMain, dtClient, -1);
end;

procedure TGMClMainFrm.InitialDock(panel: TdxDockPanel; dockType: TdxDockingTypeEx = dtNone);
var
  destDock: TdxCustomDockControl;
begin
  dxDockingController.BeginUpdate();
  try
    if dockType <> dtNone then
      panel.DockTo(dxDockSiteMain, dockType, 0)
    else
    if dxDockSiteMain.ChildCount = 0 then
      panel.DockTo(dxDockSiteMain, dtClient, 0)
    else
    begin
      destDock := TGMDockPanel.FindAnyPanelAsDockDestination(
        function (p: TdxDockPanel): bool
        begin
          Result := (p <> panel) and (p <> dxDockPanelEvents);
        end);

      if destDock = nil then
        destDock := FindDockSiteIfAny();

      if destDock <> nil then
      begin
        if Assigned(destDock.TabContainer) then
          panel.DockTo(destDock.TabContainer, dtClient, -1)
        else
          panel.DockTo(destDock, dtClient, 1);
      end
      else
      begin
        DockToEventsPanel(panel);
      end;
    end;
  finally
    dxDockingController.EndUpdate();
  end;
end;

procedure TGMClMainFrm.CreateNewScadaPage(fn: string);
begin
  InitialDock(CreateScadaPanel(fn));
end;

procedure TGMClMainFrm.miLoadScadaClick(Sender: TObject);
var
  i: int;
begin
  if odScada.Execute(Handle) then
  begin
    for i := 0 to odScada.Files.Count - 1 do
      CreateNewScadaPage(odScada.Files[i]);
  end;
end;

procedure TGMClMainFrm.N11Click(Sender: TObject);
begin
  EditMainForm.ShowModal('');
  if EditMainForm.FileName <> '' then
    CreateNewScadaPage(EditMainForm.FileName);
end;

procedure TGMClMainFrm.N4Click(Sender: TObject);
begin
  EditObjects();
end;

procedure TGMClMainFrm.lbAlarmsDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  if odSelected in AState then
    ACanvas.Brush.Color := clHighlight
  else
    ACanvas.Brush.Color := lbAlarms.Style.Color;

  case TGMAlarm(lbAlarms.Items.Objects[AIndex]).alCurrState of
    asSetTo1:
      begin
        if odSelected in AState then
          ACanvas.Font.Color := clFuchsia
        else
          ACanvas.Font.Color := clRed;
      end;
    as1DropTo0:
      begin
        if odSelected in AState then
          ACanvas.Font.Color := clLime
        else
          ACanvas.Font.Color := clGreen;
      end;
    else ACanvas.Font.Color := clWindowText;
  end;
  
  ACanvas.FillRect(ARect);

  OffsetRect(ARect, 4, 0);
  DrawText( ACanvas.Canvas.Handle,
            PChar(lbAlarms.Items[AIndex]),
            Length(lbAlarms.Items[AIndex]),
            ARect,
            DT_LEFT	or DT_SINGLELINE or DT_VCENTER
          );
end;

procedure TGMClMainFrm.WMAlarmStateChanged(var Msg: TMessage);
begin
  lbAlarms.Refresh();
end;

procedure TGMClMainFrm.miCommonRepClick(Sender: TObject);
begin
  FrmXL.ShowModal();
end;

procedure TGMClMainFrm.N5Click(Sender: TObject);
begin
  AlarmRepDlg.ShowModal();
end;

procedure TGMClMainFrm.FormShow(Sender: TObject);
begin
  if not FileExists(GMMainConfigFile.GetMainINIFileName()) and (OptsDlg.ShowModal <> mrOk) then
  begin
    Application.Terminate();
    Exit;
  end;

  TGMCOMThreadPool.CreateThreads();
  ReadObjectsFromSrv();
end;

procedure TGMClMainFrm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;

  cxLocalizer1.Locale := 1049;

  lObjFrames := TList<TFrmObject>.Create();
  lBigWindows := TList<TBigWindowFrame>.Create();
  FSoundThread := TGMSoundThread.Create();

  SetMainConfigFileClass(TGMClientMainConfigFile);
  ControlMode := cmView;
end;

procedure TGMClMainFrm.WMShowControlPanel(var Msg: TMessage);
begin
  if (Msg.LParam <= 0) or not GMAggregates.IsObjectControllable(Msg.WParam) then
    pnControl.Visible := false
  else
  begin
    pnControl.Visible := true;
    ControlFrm1.ID_Obj := Msg.WParam;
  end;
end;

procedure TGMClMainFrm.WMThreadException(var Msg: TMessage);
begin
  ReloadObjects();
end;

procedure TGMClMainFrm.miConnectionSettingsClick(Sender: TObject);
begin
  if OptsDlg.ShowModal() = mrOk then
    RestartApplication();
end;

procedure TGMClMainFrm.miControlModeClick(Sender: TObject);
begin
  if (Sender = miControlMode) and (ControlFrm1.ControlMode <> cmControl) then
  begin
    if LoginDlg.ShowModal() = mrOk then
      ControlMode := cmControl;
  end
  else
  if Sender = miViewMode then
    ControlMode := cmView
  else
  if Sender = miEditMode then
    ControlMode := cmEdit
end;

procedure TGMClMainFrm.DisableMenuSubitemsWhileNotConnected(mi: TdxBarSubItem);
var i: int;
begin
  for i := 0 to mi.ItemLinks.Count - 1 do
  begin
    if mi.ItemLinks[i].Item <> miConnectionSettings then
      mi.ItemLinks[i].Item.Enabled := bConnected;
  end;
end;

procedure TGMClMainFrm.N7Click(Sender: TObject);
var cfg: IGMConfigConfigType;
begin
  miControlMode.Down := FControlMode = cmControl;
  miViewMode.Down := FControlMode = cmView;
  miEditMode.Down := FControlMode = cmEdit;

  DisableMenuSubitemsWhileNotConnected(TdxBarSubItem(Sender));

  miHydroGeoRep.Visible := ivAlways;
  miCommonRep.Visible := ivAlways;
  try
    cfg := ReadConfigXML();

    if cfg.Hydrogeoreps.Count = 0 then
      miHydroGeoRep.Visible := ivNever;

    if cfg.Reports.Count > 0 then
      miCommonRep.Visible := ivNever;
  except
  end;
end;

procedure TGMClMainFrm.FormDestroy(Sender: TObject);
begin
  DestroyFrames();
  lObjFrames.Free();
  lBigWindows.Free();

  TGMCOMThreadPool.DestroyThreads();
  TryFreeAndNil(ReadObjThr);
  FSoundThread.Free();
end;

procedure TGMClMainFrm.miHydroGeoRepClick(Sender: TObject);
begin
  HydroGeoRepDlg.ShowModal();
end;

procedure TGMClMainFrm.ShowConnectionError(const txt: string);
begin
  lConnectionTest.Visible := true;
  lConnectionTest.Caption := txt;
  lConnectionTest.Style.Font.Color := clRed;
end;

procedure TGMClMainFrm.WMCreateObjects(var Msg: TMessage);
begin
  case Msg.WParam of

    RESULT_CLIENT_CREATE_OBJECTS_OK:
      begin
        lConnectionTest.Visible := false;
        CreateObjects();
        TryFreeAndNil(ReadObjThr);
        bConnected := true;
        PostRefreshObjectTreeMgs();
      end;

    RESULT_CLIENT_CREATE_OBJECTS_LOGIN_FAILED:
      ShowConnectionError('Ошибка авторизаии. Проверьте логин и пароль.');

    else
      ShowConnectionError('Нет связи с сервером.');

   end;
end;

procedure TGMClMainFrm.WMDestroyObject(var Msg: TMessage);
begin
  if not Application.Terminated then
    TObject(Msg.WParam).Free();
end;

procedure TGMClMainFrm.WMRequestDiagrams(var Msg: TMessage);
begin
  RequestAllFramesDiagrams();
end;

procedure TGMClMainFrm.N6Click(Sender: TObject);
begin
  TemplatedRepDlg.ShowModal();
end;

procedure TGMClMainFrm.tmrUpdateTimeTimer(Sender: TObject);
begin
  Statusbar1.Panels[1].Text := DateTimeToStr(Now() + TGMCOMThreadPool.ServerTimeDiffSec * OneSecond) + '  ';
  CheckSound();
end;

procedure TGMClMainFrm.StatusBar1Resize(Sender: TObject);
begin
  Statusbar1.Panels[0].Width := StatusBar1.Width - Statusbar1.Panels[1].Width - Statusbar1.Panels[2].Width; 
end;

procedure TGMClMainFrm.N9Click(Sender: TObject);
var xml: IGMConfigConfigType;
begin
  if SoundCfgDlg.ShowModal() = mrOk then
  begin
    xml := ReadConfigXML();
    ReadSound(xml);
  end;
end;

procedure TGMClMainFrm.tmrBlinkAlarmsTimer(Sender: TObject);
begin
  if (iBlinkingPanelWidth <= 0) or (lbAlarms.Count <= 0 ) then
  begin
    pnBlinkAlarm.Visible := false;
  end
  else
  begin
    pnBlinkAlarm.Visible := true;
    pnBlinkAlarm.Width := Min(Min(lbAlarms.Height, iBlinkingPanelWidth), lbAlarms.Width div 2);
    pnBlinkAlarm.Color := IfThen(pnBlinkAlarm.Color = clWhite, clRed, clWhite);
  end;
end;

end.
