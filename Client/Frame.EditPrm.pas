////////////////////////////////////////////
// Блок привязки одного канала для редактора объектов  
////////////////////////////////////////////
unit Frame.EditPrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, GMGlobals, INIFiles, ExtCtrls, VirtualTrees, Frame.ObjectTree,
  StrUtils, GMDBClasses, ConfigXML,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxBevel, dxSkinOffice2010Silver;

type
  TEditPrmWithDevicesFrm = class(TFrame)
    Label1: TcxLabel;
    cbDiagram: TcxCheckBox;
    leDiagramMin: TcxTextEdit;
    leDiagramMax: TcxTextEdit;
    Bevel1: TdxBevel;
    lNPrm: TcxLabel;
    lePrefix: TcxTextEdit;
    cmbImage: TcxComboBox;
    Label2: TcxLabel;
    Label3: TcxLabel;
    leBarFloat: TcxTextEdit;
    Label5: TcxLabel;
    cmbDigits: TcxComboBox;
    ePumpDownParam: TcxTextEdit;
    eMainParam: TcxTextEdit;
    lBarFloat: TcxLabel;
    Label6: TcxLabel;
    Label7: TcxLabel;
    Label8: TcxLabel;
    procedure leDevMinExit(Sender: TObject);
    procedure ePumpDownParamDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ePumpDownParamDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure eMainParamDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure eMainParamDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure eMainParamDblClick(Sender: TObject);
    procedure ePumpDownParamKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FXmlNode: IGMConfigChannelType;
    function AcceptOnlyParams: bool;
    procedure AssignID_PrmWithEdit(Edit: TcxTextEdit; prm: TGMBaseParam);
    procedure AssignParamWithEdit(Edit: TcxTextEdit);
    procedure ChekDelParam(Sender: TObject; Key: Word);
    procedure LoadFromINI();
    procedure InitControls();
  protected
    procedure SaveControlToINI(Sender: TObject);
    procedure SetXMLNode(const Value: IGMConfigChannelType);
  public
    { Public declarations }
    frmTree: TObjTreeFrame;

    property XmlNode: IGMConfigChannelType read FXmlNode write SetXMLNode;
    procedure SaveToINI(Section: string);
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TEditPrmFrm }

constructor TEditPrmWithDevicesFrm.Create(AOwner: TComponent);
begin
  inherited;

  Width := Bevel1.Width + 1;
  Height := Bevel1.Top + Bevel1.Height + 1;
end;

procedure TEditPrmWithDevicesFrm.LoadFromINI();
begin
  if XmlNode = nil then Exit;
  InitControls();

  leDiagramMin.Text := MyFloatToStr(XmlNode.Dmin);
  leDiagramMax.Text := MyFloatToStr(XmlNode.Dmax);
  lePrefix.Text := XmlNode.Prefix;
  leBarFloat.Text := XmlNode.Barfloat;

  cbDiagram.Checked := XmlNode.Diagram > 0;
  AssignID_PrmWithEdit(eMainParam, ParamOrNodeChannel(XmlNode.DataChnType, XmlNode.Id_prm));
  AssignID_PrmWithEdit(ePumpDownParam, GMNodeChannels.ByID(XmlNode.Pump_prm));

  SelectCmbObject(cmbImage, XmlNode.Showtype, 0);
  cmbDigits.ItemIndex := XmlNode.Digits;
end;

procedure TEditPrmWithDevicesFrm.SaveControlToINI(Sender: TObject);
begin
  if Sender = ePumpDownParam then
  begin
    XmlNode.Pump_prm := 0;
    if ePumpDownParam.Tag > 0 then
      XmlNode.Pump_prm := TGMParam(ePumpDownParam.Tag).ID_Prm;
  end;

  if Sender = eMainParam then
  begin
    XmlNode.Id_prm := 0;
    if eMainParam.Tag > 0 then
    begin
      XmlNode.Id_prm := TGMBaseParam(eMainParam.Tag).ID_Prm;
      XmlNode.DataChnType := TGMBaseParam(eMainParam.Tag).DataChannelType;
    end;
  end;

  if Sender = lePrefix then
    XmlNode.Prefix := lePrefix.Text;

  if Sender = leDiagramMin then
    XmlNode.Dmin := EditToDouble(leDiagramMin);

  if Sender = leDiagramMax then
    XmlNode.Dmax := EditToDouble(leDiagramMax);

  if Sender = leBarFloat then
    XmlNode.Barfloat := MyFloatToStr(EditToDouble(leBarFloat));

  if Sender = cmbDigits then
    XmlNode.Digits := cmbDigits.ItemIndex;

  if Sender = cmbImage then
    XmlNode.Showtype := CmbSelectedObj(cmbImage);

  if Sender = cbDiagram then
    XmlNode.Diagram := int(cbDiagram.Checked);
end;

procedure TEditPrmWithDevicesFrm.SaveToINI(Section: string);
var i: int;
begin
  for i := 0 to ControlCount - 1 do
    SaveControlToINI(Controls[i]);
end;

procedure TEditPrmWithDevicesFrm.InitControls();
begin
  cmbImage.Properties.Items.Clear();
  if XmlNode.Num = 0 then
  begin
    // Это трэкбар
    Label2.Caption := 'Бегунок';
    cmbImage.Properties.Items.AddObject('Превышение', TObject(0));
    cmbImage.Properties.Items.AddObject('Понижение', TObject(2));
    cmbImage.Properties.Items.AddObject('Гистерезис', TObject(1));

    ePumpDownParam.Visible:=false;
    Label3.Visible:=false;
  end
  else
  begin
    // Это Edit
    cmbImage.Properties.Items.AddObject('Цифра', TObject(0));
    cmbImage.Properties.Items.AddObject('Насос', TObject(1));

    leBarFloat.Visible := false;
    lBarFloat.Visible := false;
  end;

  cmbImage.ItemIndex:=0;

  lePrefix.Visible := XmlNode.Num > 0;
  Label5.Visible := XmlNode.Num > 0;
  cmbDigits.Visible := XmlNode.Num > 0;

  lNPrm.Caption := IfThen( XmlNode.Num > 0, 'Цифровой индикатор ' + IntToStr(XmlNode.Num), 'Шкальный индикатор');
end;

procedure TEditPrmWithDevicesFrm.leDevMinExit(Sender: TObject);
begin
  SaveControlToINI(Sender);
end;

procedure TEditPrmWithDevicesFrm.ePumpDownParamDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := AcceptOnlyParams();
end;

procedure TEditPrmWithDevicesFrm.eMainParamDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := AcceptOnlyParams();
end;

procedure TEditPrmWithDevicesFrm.ePumpDownParamDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  if frmTree.SelectedParam() is TGMParam then
    AssignParamWithEdit(TcxTextEdit(Sender));
end;

procedure TEditPrmWithDevicesFrm.eMainParamDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  AssignParamWithEdit(TcxTextEdit(Sender));
end;

procedure TEditPrmWithDevicesFrm.eMainParamDblClick(Sender: TObject);
begin
  frmTree.FindObject(TGMBaseParam(TCustomEdit(Sender).Tag));
end;

procedure TEditPrmWithDevicesFrm.ePumpDownParamKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ChekDelParam(Sender, Key);
end;

procedure TEditPrmWithDevicesFrm.AssignID_PrmWithEdit(Edit: TcxTextEdit; prm: TGMBaseParam);
begin
  Edit.Text := '';
  Edit.Hint := '';
  Edit.Tag := NativeInt(prm);
  if prm <> nil then
  begin
    Edit.Text := prm.GetNameWithPath([ppcDev]);
    Edit.Hint := prm.GetNameWithPath([ppcID, ppcSrc, ppcDev, ppcPT]);
  end;
end;

procedure TEditPrmWithDevicesFrm.AssignParamWithEdit(Edit: TcxTextEdit);
begin
  AssignID_PrmWithEdit(Edit, frmTree.SelectedParam());
  SaveControlToINI(Edit);
end;

function TEditPrmWithDevicesFrm.AcceptOnlyParams(): bool;
begin
  Result := frmTree.IsParamSelected();
end;

procedure TEditPrmWithDevicesFrm.ChekDelParam(Sender: TObject; Key: Word);
begin
  if Key = VK_DELETE then
  begin
    TCustomEdit(Sender).Tag := 0;
    TCustomEdit(Sender).Text := '';
    TCustomEdit(Sender).Hint := '';
    SaveControlToINI(Sender);
  end;
end;

procedure TEditPrmWithDevicesFrm.SetXMLNode(const Value: IGMConfigChannelType);
begin
  FXmlNode := Value;
  LoadFromINI();
end;

end.
