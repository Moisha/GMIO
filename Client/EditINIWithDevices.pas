////////////////////////////////////////////
// Редактор объектов
////////////////////////////////////////////
unit EditINIWithDevices;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GMGlobals, GMConst, IniFiles, CheckLst, Buttons,
  ComCtrls, Frame.ObjectTree, VirtualTrees, ActiveX, GMDBClasses, ConfigXML,
  Generics.Collections, Frame.EditPrm, Vcl.Menus,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  TBWOneParam = record
    prm: TGMBaseParam;
    DiagramMin, DiagramMax: double;
    Title: string;
    Color: TColor;
    bWantDiagram: bool;
    Precision: int;
  end;
  PBWOneParam = ^TBWOneParam;

  TEditINIWithDevicesDlg = class(TGMEnhancedScrollForm)
    Panel2: TcxGroupBox;
    frmTree: TObjTreeFrame;
    PageControl1: TcxPageControl;
    tsObjects: TcxTabSheet;
    tsAlarms: TcxTabSheet;
    Panel1: TcxGroupBox;
    Label1: TcxLabel;
    lObjects: TcxCheckListBox;
    btDelObject: TcxButton;
    Splitter1: TcxSplitter;
    Panel3: TcxGroupBox;
    leName: TcxTextEdit;
    cmbColor: TcxComboBox;
    Label2: TcxLabel;
    leSqueeze: TcxTextEdit;
    leX: TcxTextEdit;
    leY: TcxTextEdit;
    eDiagramTicks: TcxTextEdit;
    cbDiagramTicks: TcxCheckBox;
    leID: TcxTextEdit;
    lbAlarms: TcxListBox;
    btAddObject: TcxButton;
    tsBigWindow: TcxTabSheet;
    lbBigWindows: TcxListBox;
    Label3: TcxLabel;
    Label4: TcxLabel;
    leBigWindowName: TcxTextEdit;
    btBigWindowAdd: TcxButton;
    btBWDelete: TcxButton;
    vstBWParams: TVirtualStringTree;
    tsHydroGeoRep: TcxTabSheet;
    vstHydroGeoRep: TVirtualStringTree;
    Panel4: TcxGroupBox;
    btAddHGR: TcxButton;
    btDelHGR: TcxButton;
    TabSheet1: TcxTabSheet;
    Label5: TcxLabel;
    lbReports: TcxListBox;
    btReportAdd: TcxButton;
    btReportDel: TcxButton;
    leReportName: TcxTextEdit;
    vstReportChannels: TVirtualStringTree;
    Label6: TcxLabel;
    cmbReportTemplate: TcxComboBox;
    cmbReportDataLength: TcxComboBox;
    Label7: TcxLabel;
    Label8: TcxLabel;
    cmbReportDataGroup: TcxComboBox;
    leSqueezeY: TcxTextEdit;
    pcChannels: TcxPageControl;
    ts_line0: TcxTabSheet;
    PrmScrollBox: TcxScrollBox;
    pmObjectLines: TPopupMenu;
    miAddEditBoxLine: TMenuItem;
    miDelEditBoxLine: TMenuItem;
    cmbReportDataType: TcxComboBox;
    Label9: TcxLabel;
    Label10: TcxLabel;
    Label11: TcxLabel;
    Label12: TcxLabel;
    Label13: TcxLabel;
    Label14: TcxLabel;
    Label15: TcxLabel;
    Label16: TcxLabel;
    Label17: TcxLabel;
    cmbReportDataGroupLen: TcxComboBox;
    procedure lObjectsClick(Sender: TObject);
    procedure leIDExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btAddObjectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure leNameChange(Sender: TObject);
    procedure btDelObjectClick(Sender: TObject);
    procedure leIDDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbAlarmsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbAlarmsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbAlarmsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PageControl1Change(Sender: TObject);
    procedure lObjectsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lObjectsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure leIDDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lObjectsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbBigWindowsClick(Sender: TObject);
    procedure btBigWindowAddClick(Sender: TObject);
    procedure btBWDeleteClick(Sender: TObject);
    procedure leBigWindowNameExit(Sender: TObject);
    procedure vstBWParamsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstBWParamsDragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
      Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstBWParamsDragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; Pt: TPoint; var Effect: Integer;
      Mode: TDropMode);
    procedure vstBWParamsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstBWParamsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstBWParamsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstBWParamsDblClick(Sender: TObject);
    procedure vstBWParamsEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstBWParamsNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure lbBigWindowsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbAlarmsDblClick(Sender: TObject);
    procedure vstBWParamsChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstHydroGeoRepGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstHydroGeoRepEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstHydroGeoRepFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstHydroGeoRepDragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
      Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstHydroGeoRepDragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; Pt: TPoint; var Effect: Integer;
      Mode: TDropMode);
    procedure vstHydroGeoRepGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure btAddHGRClick(Sender: TObject);
    procedure btDelHGRClick(Sender: TObject);
    procedure tsBigWindowResize(Sender: TObject);
    procedure vstHydroGeoRepNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure btReportAddClick(Sender: TObject);
    procedure lbReportsClick(Sender: TObject);
    procedure lbReportsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btReportDelClick(Sender: TObject);
    procedure leReportNameExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbReportTemplateDropDown(Sender: TObject);
    procedure cmbReportTemplateChange(Sender: TObject);
    procedure cmbReportDataLengthChange(Sender: TObject);
    procedure cmbReportDataGroupChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcChannelsChange(Sender: TObject);
    procedure miAddEditBoxLineClick(Sender: TObject);
    procedure miDelEditBoxLineClick(Sender: TObject);
    procedure cmbReportDataTypeChange(Sender: TObject);
    procedure vstBWParamsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure cmbReportDataGroupLenPropertiesChange(Sender: TObject);
  private
    function GetCurrentObj: IGMConfigObjectType;
    procedure ReadHydroReps;
    procedure ReadTemplatedReports;
    procedure ReadOneTemplatedReport(n: int);
    procedure SaveCurrentTemplatedReport;
    procedure ReadReportTemplateList;
    procedure ReadReportTemplateListFromPath(const path: string);
  private
    { Private declarations }
    lValFrames: TList<TEditPrmWithDevicesFrm>;
    xml: IGMConfigConfigType;
    FReadingReport: bool;
    procedure PrapareEditBoxPage;
    procedure SetEditBoxLineCount(cnt: int);
    procedure AddEditBoxLine;
    procedure LoadEditButtonsPage();
    procedure AddTab(xmlObj: IGMConfigObjectType);
    procedure ExitActiveControl;
    procedure ExitControl(control: TWinControl);

    property CurrentObj: IGMConfigObjectType read GetCurrentObj;

    procedure LoadCurrentObjFromINI;
    procedure SaveControlToINI(Sender: TObject);
    procedure DelCurrentAlarm;
    procedure ReadAlarms;
    procedure CheckTreeState;
    procedure AddObject;
    procedure AssignObjectToTree;
    procedure LoadSelectedObject;
    function CheckObjectXY(n: int): bool;
    procedure ReadBigWindows;
    procedure SaveCurrentBigWindow;
    procedure ReadOneBigWindow(n: int);
    function AddPrmToBigWindowParamsList(vst: TVirtualStringTree; p: TGMBaseParam): bool; overload;
    function AddPrmToBigWindowParamsList(vst: TVirtualStringTree; bwop: TBWOneParam): bool; overload;
    procedure CommitINIFile;
    function FindBWParam(vst: TVirtualStringTree; prm: TGMBaseParam): bool;
    procedure WMRefreshObjectsTree(var Msg: TMessage); message WM_REFRESH_OBJECTS_TREE;
    function XMLChanged: bool;
    function ShowModal(): TModalResult; reintroduce;
  public
    { Public declarations }
    class function EditINI(): bool;
  end;

implementation

uses math, VT_Utils, XMLDoc, StrUtils;

{$R *.dfm}

{ TEditINIDlg }

procedure TEditINIWithDevicesDlg.ReadAlarms();
var i: int;
    p: TGMParam;
begin
  lbAlarms.Clear();

  for i := 0 to xml.Alarms.Count - 1 do
  begin
    p := GMParams.ByID(xml.Alarms[i].Id_prm);
    if p <> nil then
      lbAlarms.AddItem(p.GetNameWithPath([ppcObj, ppcDevNoGM]), TObject(p.ID_Prm));
  end;
end;

procedure TEditINIWithDevicesDlg.ReadBigWindows();
var i: int;
begin
  lbBigWindows.Clear();
  vstBWParams.Clear();

  for i := 0 to xml.Bigwindows.Count - 1 do
    lbBigWindows.Items.Add(xml.Bigwindows[i].Name);

  if lbBigWindows.Count > 0 then
  begin
    lbBigWindows.ItemIndex := 0;
    ReadOneBigWindow(LBSelectedObj(lbBigWindows));
  end;
end;

procedure TEditINIWithDevicesDlg.ReadTemplatedReports();
var i: int;
begin
  lbReports.Clear();
  vstReportChannels.Clear();

  for i := 0 to xml.Reports.Count - 1 do
    lbReports.Items.Add(xml.Reports[i].Name);

  if lbReports.Count > 0 then
  begin
    lbReports.ItemIndex := 0;
    ReadOneTemplatedReport(LBSelectedObj(lbReports));
  end;
end;

procedure TEditINIWithDevicesDlg.ReadHydroReps();
begin
  vstHydroGeoRep.RootNodeCount := xml.Hydrogeoreps.Count;
end;

function TEditINIWithDevicesDlg.ShowModal: TModalResult;
var i: int;
begin
  xml := ReadConfigXML();

  lObjects.Clear();
  for i := 0 to xml.Objects.Count - 1 do
  begin
    lObjects.Items.Add().Text := xml.Objects[i].Name;
  end;

  if lObjects.Count>0 then
  begin
    lObjects.ItemIndex:=0;
    lObjectsClick(lObjects);
  end;

  ReadAlarms();
  ReadBigWindows();
  ReadHydroReps();
  ReadTemplatedReports();

  lObjectsClick(lObjects);
  Result := inherited ShowModal();
end;

procedure TEditINIWithDevicesDlg.LoadCurrentObjFromINI();
begin
  leID.Tag := CurrentObj.Id_obj;
  if GMObjects.ObjByID(leID.Tag) <> nil then
    leID.Text := GMObjects.ObjByID(leID.Tag).Name
  else
  if leID.Tag > 0 then
    leID.Text := 'ID_Object = ' + IntToStr(leID.Tag)
  else
    leID.Text := '';

  leName.Text := CurrentObj.Name;
  leX.Text := IntToStr(CurrentObj.X);
  leY.Text := IntToStr(CurrentObj.Y);
  leSqueeze.Text := IntToStr(CurrentObj.Squeeze);
  leSqueezeY.Text := IntToStr(CurrentObj.SqueezeY);
  SelectCmbObject(cmbColor, CurrentObj.Color, 0);

  cbDiagramTicks.Checked := CurrentObj.Ticks > 0;

  SetEditBoxLineCount(CurrentObj.Tabs.Count);
end;

procedure TEditINIWithDevicesDlg.LoadSelectedObject();
begin
  if lObjects.ItemIndex < 0 then Exit;

  LoadCurrentObjFromINI();
  CheckTreeState();
end;

procedure TEditINIWithDevicesDlg.lObjectsClick(Sender: TObject);
begin
  LoadSelectedObject();
end;

procedure TEditINIWithDevicesDlg.SaveControlToINI(Sender: TObject);
begin
  if Sender = leID then
    CurrentObj.Id_obj := leID.Tag;

  if Sender = leSqueeze then
    CurrentObj.Squeeze := EditToInt(leSqueeze);

  if Sender = leSqueezeY then
    CurrentObj.SqueezeY := EditToInt(leSqueezeY);

  if Sender = leX then
    CurrentObj.X := EditToInt(leX);

  if Sender = leY then
    CurrentObj.Y := EditToInt(leY);

  if Sender = leName then
    CurrentObj.Name := leName.Text;

  if Sender = cmbColor then
    CurrentObj.Color := CmbSelectedObj(cmbColor);

  if cbDiagramTicks.Checked then
    CurrentObj.Ticks := EditToInt(eDiagramTicks)
  else
    CurrentObj.Ticks := 0;

  lObjects.Refresh();
end;

procedure TEditINIWithDevicesDlg.leIDExit(Sender: TObject);
begin
  SaveControlToINI(Sender);
end;

procedure TEditINIWithDevicesDlg.CommitINIFile();
begin
  SaveConfigXML(xml);
end;

function TEditINIWithDevicesDlg.XMLChanged(): bool;
var old: IGMConfigConfigType;
begin
  old := ReadConfigXML();
  Result := FormatXMLData(xml.XML) <> FormatXMLData(old.XML);
end;

procedure TEditINIWithDevicesDlg.ExitControl(control: TWinControl);
begin
  if control = nil then Exit;

  if (control is TcxTextEdit) and Assigned(TcxTextEdit(control).OnExit) then
    TcxTextEdit(control).OnExit(control);

  if (control is TcxComboBox) and Assigned(TcxComboBox(control).OnExit) then
    TcxComboBox(control).OnExit(control);
end;

procedure TEditINIWithDevicesDlg.ExitActiveControl();
begin
  ExitControl(ActiveControl);
  if ActiveControl <> nil then
    ExitControl(ActiveControl.Parent);
end;

procedure TEditINIWithDevicesDlg.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var res: int;
begin
  CanClose := true;

  ExitActiveControl();

  if XMLChanged() then
  begin
    res := ShowMessageBox('Сохранить изменения', MB_ICONEXCLAMATION or MB_YESNOCANCEL);

    case res of

      IDYES:
        begin
          CommitINIFile();
          ModalResult := mrOK;
        end;

      IDCANCEL:
        CanClose := false;

      IDNO:
        ModalResult := mrCancel;
    end;
  end
  else
  begin
    ModalResult := mrCancel;
  end;
end;

procedure TEditINIWithDevicesDlg.AddObject();
var n: int;
    xmlObj: IGMConfigObjectType;
begin
  n := lObjects.Items.Count + 1;

  xmlObj := xml.Objects.Add();
  xmlObj.Name := 'Object ' + IntToStr(n);
  xmlObj.Squeeze := 100;
  AddTab(xmlObj);
  lObjects.Items.Add().Text := 'Object' + IntToStr(n);
  lObjects.ItemIndex := n - 1;

  LoadSelectedObject();

  btAddObject.Down := false;
end;

procedure TEditINIWithDevicesDlg.btAddObjectClick(Sender: TObject);
begin
  if btAddObject.Down then
  begin
    frmTree.ShowObjects(fotAll);
    frmTree.Tree.FullCollapse();
  end
  else
    LoadSelectedObject();
end;

procedure TEditINIWithDevicesDlg.PrapareEditBoxPage();
var i: int;
    f: TEditPrmWithDevicesFrm;
begin
  for i := 0 to NominalEditBoxOnLineCount - 1 do
  begin
    f := TEditPrmWithDevicesFrm.Create(self);
    lValFrames.Add(f);
    f.Parent := PrmScrollBox;
    f.Name := 'EditPrm' + IntToStr(i);
    f.Left := 0;
    f.Top := (f.Height + 1) * i;
    f.frmTree := frmTree;
  end;
end;

procedure TEditINIWithDevicesDlg.AddEditBoxLine();
var p: TcxTabSheet;
begin
  p := TcxTabSheet.Create(pcChannels);
  p.Name := 'ts_line' + IntToStr(pcChannels.PageCount);
  p.Caption := 'Ряд ' + IntToStr(pcChannels.PageCount + 1);
  p.Parent := pcChannels;
  p.PageControl := pcChannels;
end;

procedure TEditINIWithDevicesDlg.SetEditBoxLineCount(cnt: int);
begin
  if cnt <= 0 then
    cnt := 1;

  if pcChannels.ActivePageIndex >= cnt then
  begin
    pcChannels.ActivePageIndex := cnt - 1;
    LoadEditButtonsPage();
  end;

  while cnt < pcChannels.PageCount do
    pcChannels.Pages[pcChannels.PageCount - 1].Free();

  while cnt > pcChannels.PageCount do
    AddEditBoxLine();

  LoadEditButtonsPage();
end;

procedure TEditINIWithDevicesDlg.FormCreate(Sender: TObject);
begin
  FReadingReport := false;
  frmTree.Init();
  lValFrames := TList<TEditPrmWithDevicesFrm>.Create();

  cmbColor.Properties.Items.Clear();
  cmbColor.Properties.Items.AddObject('Синий', TObject(1));
  cmbColor.Properties.Items.AddObject('Зеленый', TObject(2));
  cmbColor.Properties.Items.AddObject('Коричневый', TObject(3));

  cmbReportDataLength.Properties.Items.Clear();
  cmbReportDataLength.Properties.Items.AddObject('На выбор', TObject(REPORTLENGTH_USER_SEFINE));
  cmbReportDataLength.Properties.Items.AddObject('Сутки', TObject(REPORTLENGTH_DAY));
  cmbReportDataLength.Properties.Items.AddObject('Месяц', TObject(REPORTLENGTH_MONTH));
  cmbReportDataLength.Properties.Items.AddObject('Год', TObject(REPORTLENGTH_YEAR));

  cmbReportDataGroup.Properties.Items.Clear();
  cmbReportDataGroup.Properties.Items.AddObject('Указать (мин)', TObject(REPORTLENGTH_USER_SEFINE));
  cmbReportDataGroup.Properties.Items.AddObject('Полчаса', TObject(REPORTLENGTH_HALFHOUR));
  cmbReportDataGroup.Properties.Items.AddObject('Час', TObject(REPORTLENGTH_HOUR));
  cmbReportDataGroup.Properties.Items.AddObject('Сутки', TObject(REPORTLENGTH_DAY));
  cmbReportDataGroup.Properties.Items.AddObject('Месяц', TObject(REPORTLENGTH_MONTH));

  cmbReportDataType.Properties.Items.Clear();
  cmbReportDataType.Properties.Items.AddObject('Текущие', TObject(DATATYPE_CURRENT_DIAGRAM));
  cmbReportDataType.Properties.Items.AddObject('Часовки', TObject(DATATYPE_HOUR_DIAGRAM));
  cmbReportDataType.Properties.Items.AddObject('Суточные', TObject(DATATYPE_DAY_DIAGRAM));

  PageControl1.ActivePage := tsObjects;
  PrapareEditBoxPage();
end;

procedure TEditINIWithDevicesDlg.FormDestroy(Sender: TObject);
begin
  lValFrames.Free();
  vstBWParams.Clear(); // иначе падает при дестрое формы, причину я объяснить не способоен
end;

procedure TEditINIWithDevicesDlg.leNameChange(Sender: TObject);
begin
  if lObjects.ItemIndex >= 0 then
    lObjects.Items[lObjects.ItemIndex].Text := leName.Text;
end;

procedure TEditINIWithDevicesDlg.DelCurrentAlarm();
var n: int;
begin
  n := lbAlarms.ItemIndex;
  if n < 0 then Exit;

  xml.Alarms.Delete(n);
  lbAlarms.Items.Delete(n);
  lbAlarms.ItemIndex := min(lbAlarms.Count - 1, n);
end;

var EditINIWithDevicesDlg: TEditINIWithDevicesDlg = nil;

class function TEditINIWithDevicesDlg.EditINI: bool;
begin
  if EditINIWithDevicesDlg = nil then
    Application.CreateForm(TEditINIWithDevicesDlg, EditINIWithDevicesDlg);

  Result := EditINIWithDevicesDlg.ShowModal() = mrOk;
end;

procedure TEditINIWithDevicesDlg.btDelObjectClick(Sender: TObject);
var n: int;
begin
  n := lObjects.ItemIndex;
  if (n < 0) or (ShowMessageBox('Удалить', MB_YESNO) <> IDYES) then Exit;

  XML.Objects.Remove(CurrentObj);
  lObjects.DeleteSelected();

  lObjects.ItemIndex := Min(n, lObjects.Count - 1);
end;

procedure TEditINIWithDevicesDlg.leIDDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := frmTree.Tree.GetNodeLevel(frmTree.Tree.GetFirstSelected()) = 0;
end;

procedure TEditINIWithDevicesDlg.AssignObjectToTree();
var obj: TObject;
begin
  obj := frmTree.SelectedObject();
  if obj is TGMObject then
  begin
    leID.Text :=  TGMObject(obj).Name;
    leID.Tag := TGMObject(obj).ID_Obj;
  end;

  SaveControlToINI(leID);

  leName.Text := leID.Text;
  leNameChange(nil);
  SaveControlToINI(leName);

  CheckTreeState();
end;

procedure TEditINIWithDevicesDlg.lbAlarmsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = frmTree.Tree) and frmTree.IsParamSelected();
end;

procedure TEditINIWithDevicesDlg.lbAlarmsDragDrop(Sender, Source: TObject; X, Y: Integer);
var i: int;
    prm: TGMBaseParam;
begin
  prm := frmTree.SelectedParam();
  if not (prm is TGMParam) then Exit;

  for i := 0 to lbAlarms.Count - 1 do
    if int(lbAlarms.Items.Objects[i]) = prm.ID_Prm then
    begin
      lbAlarms.ItemIndex := i;
      Exit;
    end;

  lbAlarms.Items.AddObject(GMParams.ByID(prm.ID_Prm).GetNameWithPath([ppcObj, ppcDevNoGM]), TObject(prm.ID_Prm));
  xml.Alarms.Add().Id_prm := prm.ID_Prm;
end;

procedure TEditINIWithDevicesDlg.lbAlarmsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    DelCurrentAlarm();
end;

procedure TEditINIWithDevicesDlg.CheckTreeState();
begin
  if PageControl1.ActivePage = tsObjects then
    frmTree.ShowObjects(fotSpecified, leID.Tag)
  else
    frmTree.ShowObjects(fotAll);
end;

procedure TEditINIWithDevicesDlg.PageControl1Change(Sender: TObject);
begin
  CheckTreeState();
end;

procedure TEditINIWithDevicesDlg.LoadEditButtonsPage();
var i, n: int;
begin
  PrmScrollBox.Parent := pcChannels.ActivePage;
  n := pcChannels.ActivePageIndex;

  for i := 0 to NominalEditBoxOnLineCount - 1 do
    lValFrames[i].XmlNode := CurrentObj.Tabs[n].Channels[i];

  lValFrames[0].Visible := (n = 0);
end;

procedure TEditINIWithDevicesDlg.pcChannelsChange(Sender: TObject);
begin
  LoadEditButtonsPage();
end;

procedure TEditINIWithDevicesDlg.lObjectsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := frmTree.Tree.GetNodeLevel(frmTree.Tree.GetFirstSelected()) = 0;
end;

procedure TEditINIWithDevicesDlg.lObjectsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  AddObject();
  AssignObjectToTree();
end;

procedure TEditINIWithDevicesDlg.leIDDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  AssignObjectToTree();
end;

function TEditINIWithDevicesDlg.CheckObjectXY(n: int): bool;
var i, x, y: int;
begin
  Result := false;
  if (n < 0) or (n >= xml.Objects.Count) then Exit;

  x := xml.Objects[n].X;
  y := xml.Objects[n].Y;

  if (x = 0) or (y = 0) then Exit;

  for i := 0 to xml.Objects.Count - 1 do
    if (i <> n) and (xml.Objects[i].X = x) and (xml.Objects[i].Y = y) then Exit;

  Result := true;
end;

procedure TEditINIWithDevicesDlg.lObjectsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  lObjects.Canvas.FillRect(Rect);
  lObjects.Canvas.Font.Color := IfThen(CheckObjectXY(Index), IfThen(Index = lObjects.ItemIndex, clHighlightText, clWindowText), clRed);
  lObjects.Canvas.TextOut(Rect.Left + 20, Rect.Top, lObjects.Items[Index].Text);
end;

procedure TEditINIWithDevicesDlg.AddTab(xmlObj: IGMConfigObjectType);
var tab: IGMConfigTabType;
begin
  tab := xmlObj.Tabs.Add();
  AdjustChannelCount(tab.Channels);
end;

procedure TEditINIWithDevicesDlg.miAddEditBoxLineClick(Sender: TObject);
begin
  AddTab(CurrentObj);
  AddEditBoxLine();
  pcChannels.ActivePageIndex := pcChannels.PageCount - 1;
  LoadEditButtonsPage();
end;

procedure TEditINIWithDevicesDlg.miDelEditBoxLineClick(Sender: TObject);
begin
  if pcChannels.PageCount <= 1 then Exit;

  CurrentObj.Tabs.Delete(pcChannels.ActivePageIndex);
  SetEditBoxLineCount(CurrentObj.Tabs.Count);
end;

procedure TEditINIWithDevicesDlg.SaveCurrentBigWindow();
var i, n: int;
    nd: PVirtualNode;
    vst: PBWOneParam;
    d: IGMConfigDiagramType;
    bw: IGMConfigBigwindowType;
begin
  n := lbBigWindows.ItemIndex;
  if n < 0 then Exit;

  lbBigWindows.Items[n] := leBigWindowName.Text;
  bw := xml.Bigwindows[n];
  bw.Name := leBigWindowName.Text;

  // всем поставим отрицательную ширину. По ней потом определим, кого удалить
  for i := 0 to bw.Diagrams.Count - 1 do
    bw.Diagrams[i].ColWidth := Min(-1, -Abs(xml.Bigwindows[n].Diagrams[i].ColWidth));

  nd := vstBWParams.RootNode.FirstChild;
  while nd <> nil do
  begin
    vst := PBWOneParam(vstBWParams.GetNodeData(nd));
    d := nil;

    for i := 0 to bw.Diagrams.Count - 1 do
    begin
      if (bw.Diagrams[i].Id_prm = vst.prm.ID_Prm)
         and (bw.Diagrams[i].DataChnType = vst.prm.DataChannelType) then
      begin
        d := bw.Diagrams[i];
        break;
      end;
    end;

    if d = nil then
    begin
      d := bw.Diagrams.Add();
      d.Id_prm := vst.prm.ID_Prm;
      d.DataChnType := vst.prm.DataChannelType;
      d.ColWidth := 150;
    end;

    d.Dmin := vst.DiagramMin;
    d.Dmax := vst.DiagramMax;
    d.Diagram := IfThen(vst.bWantDiagram, 1, 0);
    d.Text := vst.Title;
    d.ColWidth := Abs(d.ColWidth);
    d.Precision := vst.Precision;

    nd := nd.NextSibling;
  end;

  for i := bw.Diagrams.Count - 1 downto 0 do
    if bw.Diagrams[i].ColWidth < 0 then
      bw.Diagrams.Delete(i);
end;

procedure TEditINIWithDevicesDlg.SaveCurrentTemplatedReport();
var n: int;
    nd: PVirtualNode;
    vst: PBWOneParam;
    d: IGMConfigReportColumnType;
begin
  if FReadingReport then Exit;

  n := lbReports.ItemIndex;
  if n < 0 then Exit;

  lbReports.Items[n] := leReportName.Text;
  xml.Reports[n].Name := leReportName.Text;
  xml.Reports[n].Template := cmbReportTemplate.Text;

  xml.Reports[n].DataLength := CmbSelectedObj(cmbReportDataLength);
  xml.Reports[n].DataGroup := CmbSelectedObj(cmbReportDataGroup);
  xml.Reports[n].DataGroupLen := StrToIntDef(cmbReportDataGroupLen.Text, 1);
  xml.Reports[n].DataType := CmbSelectedObj(cmbReportDataType);

  xml.Reports[n].ReportColumns.Clear();
  nd := vstReportChannels.RootNode.FirstChild;
  while nd <> nil do
  begin
    vst := PBWOneParam(vstReportChannels.GetNodeData(nd));
    d := xml.Reports[n].ReportColumns.Add();
    d.Id_prm := vst.prm.ID_Prm;
    d.DataChnType := vst.prm.DataChannelType;

    nd := nd.NextSibling;
  end;
end;

function TEditINIWithDevicesDlg.FindBWParam(vst: TVirtualStringTree; prm: TGMBaseParam): bool;
var nd: PVirtualNode;
begin
  Result := false;
  nd := vst.RootNode.FirstChild;
  while nd <> nil do
  begin
    if PBWOneParam(vst.GetNodeData(nd)).prm.Equals(prm) then
    begin
      Result := true;
      break;
    end;

    nd := nd.NextSibling;
  end;
end;

function TEditINIWithDevicesDlg.AddPrmToBigWindowParamsList(vst: TVirtualStringTree; bwop: TBWOneParam): bool;
var nd: PVirtualNode;
    bwopTree: PBWOneParam;
begin
  Result := false;
  if (bwop.prm <> nil) and not FindBWParam(vst, bwop.prm) then
  begin
    nd := vst.AddChild(nil);
    vst.CheckType[nd] := ctCheckBox;
    Include(nd.States, vsInitialized);

    bwopTree := PBWOneParam(vst.GetNodeData(nd));
    bwopTree^ := bwop;

    if bwopTree.bWantDiagram then
      vst.CheckState[nd] := csCheckedNormal;

    Result := true;
  end;
end;

function TEditINIWithDevicesDlg.AddPrmToBigWindowParamsList(vst: TVirtualStringTree; p: TGMBaseParam): bool;
var
  bwop: TBWOneParam;
begin
  bwop.prm := p;
  bwop.DiagramMin := 0;
  bwop.DiagramMax := 100;
  bwop.Color := clWhite;
  bwop.Precision := -1;
  Result := AddPrmToBigWindowParamsList(vst, bwop);
end;

procedure TEditINIWithDevicesDlg.ReadOneBigWindow(n: int);
var i: int;
    bwop: TBWOneParam;
begin
  leBigWindowName.Text := '';
  vstBWParams.Clear();
  vstBWParams.OnChecked := nil;
  try
    if n >= 0 then
    begin
      leBigWindowName.Text := xml.Bigwindows[n].Name;
      for i := 0 to xml.Bigwindows[n].Diagrams.Count - 1 do
      begin
        bwop.prm := ParamOrNodeChannel(xml.Bigwindows[n].Diagrams[i].DataChnType, xml.Bigwindows[n].Diagrams[i].Id_prm);
        bwop.DiagramMin := xml.Bigwindows[n].Diagrams[i].Dmin;
        bwop.DiagramMax := xml.Bigwindows[n].Diagrams[i].Dmax;
        bwop.bWantDiagram := xml.Bigwindows[n].Diagrams[i].Diagram > 0;
        bwop.Title := xml.Bigwindows[n].Diagrams[i].Text;
        bwop.Precision := xml.Bigwindows[n].Diagrams[i].Precision;

        AddPrmToBigWindowParamsList(vstBWParams, bwop);
      end;
    end;
  finally
    vstBWParams.OnChecked := vstBWParamsChecked;
  end;
end;

procedure TEditINIWithDevicesDlg.ReadOneTemplatedReport(n: int);
var i: int;
    bwop: TBWOneParam;
begin
  FReadingReport := true;
  try
    leReportName.Text := '';
    vstReportChannels.Clear();

    if n >= 0 then
    begin
      for i := 0 to xml.Reports[n].ReportColumns.Count - 1 do
      begin
        bwop.prm := ParamOrNodeChannel(xml.Reports[n].ReportColumns[i].DataChnType, xml.Reports[n].ReportColumns[i].ID_Prm);
        AddPrmToBigWindowParamsList(vstReportChannels, bwop);
      end;

      leReportName.Text := xml.Reports[n].Name;
      cmbReportTemplate.Text := xml.Reports[n].Template;
      SelectCmbObject(cmbReportDataLength, xml.Reports[n].DataLength, cmbReportDataLength.Properties.Items.IndexOfObject(TObject(REPORTLENGTH_DAY)));
      SelectCmbObject(cmbReportDataGroup, xml.Reports[n].DataGroup, cmbReportDataGroup.Properties.Items.IndexOfObject(TObject(REPORTLENGTH_HOUR)));
      cmbReportDataGroupLen.ItemIndex := Max(cmbReportDataGroupLen.Properties.Items.IndexOf(IntToStr(xml.Reports[n].DataGroupLen)), 0);
      SelectCmbObject(cmbReportDataType, xml.Reports[n].DataType, cmbReportDataType.Properties.Items.IndexOfObject(TObject(DATATYPE_CURRENT_DIAGRAM)));
    end;
  finally
    FReadingReport := false;
  end;
end;

procedure TEditINIWithDevicesDlg.lbBigWindowsClick(Sender: TObject);
begin
  ReadOneBigWindow(lbBigWindows.ItemIndex);
end;

procedure TEditINIWithDevicesDlg.btBigWindowAddClick(Sender: TObject);
var s: string;
begin
  if not InputQuery('Введите название', '', s) then Exit;

  lbBigWindows.Items.Add(s);
  xml.Bigwindows.Add().Name := s;

  lbBigWindows.ItemIndex := lbBigWindows.Count - 1;
  lbBigWindowsClick(nil);
end;

procedure TEditINIWithDevicesDlg.btBWDeleteClick(Sender: TObject);
var n: int;
begin
  n := lbBigWindows.ItemIndex;

  xml.Bigwindows.Delete(n);
  lbBigWindows.Items.Delete(n);

  lbBigWindows.ItemIndex := min(n, lbBigWindows.Count - 1);

  ReadOneBigWindow(LBSelectedObj(lbBigWindows));
end;

procedure TEditINIWithDevicesDlg.leBigWindowNameExit(Sender: TObject);
begin
  SaveCurrentBigWindow();
end;

procedure TEditINIWithDevicesDlg.vstBWParamsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var vst: TVirtualStringTree;
begin
  vst := TVirtualStringTree(Sender);
  
  if (Key = VK_DELETE) and (vst.SelectedCount > 0) then
  begin
    DelNodeAndSelectNextSibling(vst.FocusedNode);
    if Sender = vstBWParams then SaveCurrentBigWindow();
    if Sender = vstReportChannels then SaveCurrentTemplatedReport();
  end;
end;

procedure TEditINIWithDevicesDlg.vstBWParamsDragOver(
  Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer;
  var Accept: Boolean);
begin
  if frmTree.IsSourceTree(Source) then
    Accept := frmTree.IsParamSelected()
  else
    Accept := (Source = Sender); 
end;

procedure TEditINIWithDevicesDlg.vstBWParamsDragDrop(
  Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
  var Effect: Integer; Mode: TDropMode);
begin
  if frmTree.IsSourceTree(Source) then
  begin
    if Sender = vstBWParams then
    begin
      if AddPrmToBigWindowParamsList(vstBWParams, frmTree.SelectedParam()) then
        SaveCurrentBigWindow();
    end;

    if Sender = vstReportChannels then
    begin
      if AddPrmToBigWindowParamsList(vstReportChannels, frmTree.SelectedParam()) then
        SaveCurrentTemplatedReport();
    end;
  end;

  if Source = Sender then
  begin
    VST_SortByDragging(Sender, Pt, Mode);
    if Sender = vstBWParams then SaveCurrentBigWindow();
    if Sender = vstReportChannels then SaveCurrentTemplatedReport();
  end;
end;

procedure TEditINIWithDevicesDlg.vstBWParamsGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TBWOneParam);
end;

procedure TEditINIWithDevicesDlg.vstBWParamsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column > 0 then
    Sender.EditNode(Node, Column);
end;

procedure TEditINIWithDevicesDlg.vstBWParamsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  Finalize(PBWOneParam(vstBWParams.GetNodeData(Node))^);
end;

procedure TEditINIWithDevicesDlg.vstBWParamsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var vst: PBWOneParam;
begin
  vst := PBWOneParam(vstBWParams.GetNodeData(Node));
  case Column of
    0: CellText := vst.prm.GetNameWithPath([ppcObj, ppcDevNoGM]);
    1: CellText := vst.Title;
    2: CellText := MyFloatToStr(vst.DiagramMin);
    3: CellText := MyFloatToStr(vst.DiagramMax);
    4: CellText := IfThen(vst.Precision >= 0, IntToStr(vst.Precision), '');
  end;
end;

procedure TEditINIWithDevicesDlg.vstBWParamsDblClick(Sender: TObject);
var vst: TVirtualStringTree;
    obj: PBWOneParam;
begin
  vst := TVirtualStringTree(Sender);
  if vst.GetFirstSelected() <> nil then
  begin
    obj := PBWOneParam(vst.GetNodeData(vst.GetFirstSelected()));
    frmTree.FindObject(obj.prm);
  end;
end;

procedure TEditINIWithDevicesDlg.vstBWParamsEditing(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  Allowed := Column > 0;
end;

procedure TEditINIWithDevicesDlg.vstBWParamsNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: string);
begin
  case Column of
    1: PBWOneParam(Sender.GetNodeData(Node)).Title := NewText;
    2: PBWOneParam(Sender.GetNodeData(Node)).DiagramMin := MyStrToFloat(NewText);
    3: PBWOneParam(Sender.GetNodeData(Node)).DiagramMax := MyStrToFloat(NewText);
    4: PBWOneParam(Sender.GetNodeData(Node)).Precision := StrToIntdef(NewText, -1);
  end;
  SaveCurrentBigWindow();
end;

procedure TEditINIWithDevicesDlg.WMRefreshObjectsTree(var Msg: TMessage);
begin
  frmTree.Init();
end;

procedure TEditINIWithDevicesDlg.lbBigWindowsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    btBWDeleteClick(nil);
end;

procedure TEditINIWithDevicesDlg.lbAlarmsDblClick(Sender: TObject);
begin
  if lbAlarms.ItemIndex >= 0 then
    frmTree.FindObject(GMParams.ByID(LBSelectedObj(lbAlarms)));
end;

procedure TEditINIWithDevicesDlg.vstBWParamsChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  PBWOneParam(vstBWParams.GetNodeData(Node)).bWantDiagram := vstBWParams.CheckState[Node] = csCheckedNormal;
  SaveCurrentBigWindow();
end;

procedure TEditINIWithDevicesDlg.vstHydroGeoRepGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := 0;
end;

procedure TEditINIWithDevicesDlg.vstHydroGeoRepEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column = 0;
end;

procedure TEditINIWithDevicesDlg.vstHydroGeoRepFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  // Finalize(PHydroGeoRep(vstHydroGeoRep.GetNodeData(Node))^);
end;

function TEditINIWithDevicesDlg.GetCurrentObj: IGMConfigObjectType;
begin
  Result := xml.Objects[lObjects.ItemIndex];
end;

procedure TEditINIWithDevicesDlg.vstHydroGeoRepDragOver(
  Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer;
  var Accept: Boolean);
begin
  Accept := (Source = frmTree.Tree) and frmTree.IsParamSelected();
end;

procedure TEditINIWithDevicesDlg.vstHydroGeoRepDragDrop(
  Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
  var Effect: Integer; Mode: TDropMode);
var h: THitInfo;
begin
  vstHydroGeoRep.GetHitTestInfoAt(Pt.X, Pt.Y, false, h);

  if h.HitNode = nil then Exit;

  if not (frmTree.SelectedParam() is TGMParam) then Exit;

  case h.HitColumn of
    1: xml.HydroGeoReps[h.HitNode.Index].LevelPrm :=frmTree.SelectedParam().ID_Prm;
    2: xml.HydroGeoReps[h.HitNode.Index].FlowPrm := frmTree.SelectedParam().ID_Prm;
  end;

  vstHydroGeoRep.Refresh();
end;

procedure TEditINIWithDevicesDlg.vstHydroGeoRepGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var prm: TGMParam;
begin
  CellText := '';
  case Column of
    0: CellText := xml.HydroGeoReps[Node.Index].Name;
    1: begin
         prm := GMParams.ByID(xml.HydroGeoReps[Node.Index].LevelPrm);
         if prm <> nil then
           CellText := prm.GetNameWithPath([ppcObj, ppcDev]);
       end;
    2: begin
         prm := GMParams.ByID(xml.HydroGeoReps[Node.Index].FlowPrm);
         if prm <> nil then
           CellText := prm.GetNameWithPath([ppcObj, ppcDev]);
       end;
  end;
end;

procedure TEditINIWithDevicesDlg.btAddHGRClick(Sender: TObject);
begin
  xml.HydroGeoReps.Add().Name := 'Скважина ' + IntToStr(vstHydroGeoRep.AddChild(nil).Index);
end;

procedure TEditINIWithDevicesDlg.btDelHGRClick(Sender: TObject);
var nd: PVirtualNode;
begin
  nd := vstHydroGeoRep.GetFirstSelected();
  if nd = nil then Exit;

  xml.HydroGeoReps.Delete(nd.Index);
  vstHydroGeoRep.DeleteNode(nd);
end;

procedure TEditINIWithDevicesDlg.tsBigWindowResize(Sender: TObject);
begin
  vstBWParams.Width := tsBigWindow.Width - 22;
  leBigWindowName.Width := vstBWParams.Width;

  btBigWindowAdd.Left := tsBigWindow.Width - 42;
  btBWDelete.Left := btBigWindowAdd.Left;
end;

procedure TEditINIWithDevicesDlg.vstHydroGeoRepNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: string);
begin
  xml.HydroGeoReps[Node.Index].Name := NewText;
end;

procedure TEditINIWithDevicesDlg.btReportAddClick(Sender: TObject);
var s: string;
    rep: IGMConfigReportType;
begin
  if not InputQuery('Введите название', '', s) then Exit;

  lbReports.Items.Add(s);
  rep := xml.Reports.Add();
  rep.Name := s;
  rep.DataLength := REPORTLENGTH_DAY;
  rep.DataGroup := REPORTLENGTH_HOUR;

  lbReports.ItemIndex := lbReports.Count - 1;
  ReadOneTemplatedReport(lbReports.ItemIndex);
end;
                               
procedure TEditINIWithDevicesDlg.lbReportsClick(Sender: TObject);
begin
  ReadOneTemplatedReport(lbReports.ItemIndex);
end;

procedure TEditINIWithDevicesDlg.lbReportsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    btReportDelClick(nil);
end;

procedure TEditINIWithDevicesDlg.btReportDelClick(Sender: TObject);
var n: int;
begin
  n := lbReports.ItemIndex;
  if n < 0 then
    Exit;

  xml.Reports.Delete(n);
  lbReports.Items.Delete(n);

  lbReports.ItemIndex := min(n, lbReports.Count - 1);

  ReadOneTemplatedReport(LBSelectedObj(lbReports));
end;

procedure TEditINIWithDevicesDlg.leReportNameExit(Sender: TObject);
begin
  SaveCurrentTemplatedReport();
end;

procedure TEditINIWithDevicesDlg.ReadReportTemplateListFromPath(const path: string);
var r: TSearchRec;
begin
  if FindFirst(path, faAnyFile, r) = 0 then
  repeat
    if cmbReportTemplate.Properties.Items.IndexOf(r.Name) < 0 then
      cmbReportTemplate.Properties.Items.Add(r.Name);
  until FindNext(r) <> 0;

  FindClose(r);
end;

procedure TEditINIWithDevicesDlg.ReadReportTemplateList();
var dir: string;
begin
  dir := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));
  ReadReportTemplateListFromPath(dir + '*.xls');
  ReadReportTemplateListFromPath(dir + '*.xlsx');
  ReadReportTemplateListFromPath(dir + 'Reports\*.xls');
  ReadReportTemplateListFromPath(dir + 'Reports\*.xlsx');
end;

procedure TEditINIWithDevicesDlg.FormShow(Sender: TObject);
begin
  ReadReportTemplateList();
end;

procedure TEditINIWithDevicesDlg.cmbReportTemplateDropDown(
  Sender: TObject);
begin
  ReadReportTemplateList();
end;

procedure TEditINIWithDevicesDlg.cmbReportTemplateChange(Sender: TObject);
begin
  SaveCurrentTemplatedReport();
end;

procedure TEditINIWithDevicesDlg.cmbReportDataLengthChange(Sender: TObject);
begin
  SaveCurrentTemplatedReport();
end;

procedure TEditINIWithDevicesDlg.cmbReportDataTypeChange(Sender: TObject);
begin
  SaveCurrentTemplatedReport();
end;

procedure TEditINIWithDevicesDlg.cmbReportDataGroupChange(Sender: TObject);
begin
  SaveCurrentTemplatedReport();
  cmbReportDataGroupLen.Visible := CmbSelectedObj(cmbReportDataGroup) = REPORTLENGTH_USER_SEFINE;
end;

procedure TEditINIWithDevicesDlg.cmbReportDataGroupLenPropertiesChange(Sender: TObject);
begin
  SaveCurrentTemplatedReport();
end;

procedure TEditINIWithDevicesDlg.FormResize(Sender: TObject);
begin
  btReportAdd.Left := TabSheet1.Width - 40;
  btReportDel.Left := TabSheet1.Width - 40;
  lbReports.Width := TabSheet1.Width - 60;
  vstReportChannels.Width := TabSheet1.Width - 20;
  leReportName.Width := TabSheet1.Width - 20;
  cmbReportTemplate.Width := TabSheet1.Width - 20;
end;

end.
