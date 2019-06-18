unit fChannelsProps;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FlexProps, VirtualTrees, Frame.NodeTree, Vcl.ExtCtrls, Vcl.StdCtrls,
  Frame.ObjectTree, GMDBClasses, ActiveX, GMGlobals, GMConst,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  TChannelsPropForm = class(TGMEnhancedScrollForm)
    Panel1: TcxGroupBox;
    Button1: TcxButton;
    Button2: TcxButton;
    frmTree: TObjTreeFrame;
    vstChannels: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure vstChannelsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstChannelsDblClick(Sender: TObject);
    procedure vstChannelsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
      State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstChannelsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure FormDestroy(Sender: TObject);
    procedure vstChannelsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure vstChannelsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure vstChannelsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure FormShow(Sender: TObject);
    procedure vstChannelsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private
    { Private declarations }
    FChannels: TSTringList;
    procedure AddParam(prm: TGMBaseParam);
  public
    { Public declarations }
  end;

var
  ChannelsPropForm: TChannelsPropForm;

implementation

{$R *.dfm}

uses VT_Utils;

procedure TChannelsPropForm.FormShow(Sender: TObject);
var i: int;
begin
  FChannels.Clear();
  if TObject(Tag) is TChannelProp then
    FChannels.Text := TChannelProp(Tag).Text;

  for i := FChannels.Count - 1 downto 0 do
  begin
    if FChannels[i].Trim() = '' then
      FChannels.Delete(i);
  end;

  vstChannels.RootNodeCount := FChannels.Count;
end;

procedure TChannelsPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult <> mrOk) or (Tag <= 0) then Exit;

  TChannelProp(Tag).Text := FChannels.Text;
end;

procedure TChannelsPropForm.FormCreate(Sender: TObject);
begin
  FChannels := TSTringList.Create();
  frmTree.Init();
end;

procedure TChannelsPropForm.FormDestroy(Sender: TObject);
begin
  FChannels.Free();
end;

procedure TChannelsPropForm.vstChannelsDblClick(Sender: TObject);
var nd: PVirtualNode;
    s: string;
begin
  nd := vstChannels.FocusedNode;
  if nd = nil then Exit;

  s := FChannels.Names[nd.Index];
  frmTree.FindObject(GMParamFromString(s));
end;

procedure TChannelsPropForm.AddParam(prm: TGMBaseParam);
var s: string;
begin
  s := IntToStr(prm.ID_Prm);
  if prm is TGMNodeChannel then
    s := 'n' + s + '='
  else
    s := 'c' + s + '=';

  if FChannels.IndexOfName(s) < 0 then
  begin
    FChannels.Add(s);
    vstChannels.RootNodeCount := FChannels.Count;
  end;
end;

procedure TChannelsPropForm.vstChannelsDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
  if frmTree.IsSourceTree(Source) then
  begin
    AddParam(frmTree.SelectedParam());
  end;
end;

procedure TChannelsPropForm.vstChannelsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  if frmTree.IsSourceTree(Source) then
    Accept := frmTree.IsParamSelected()
  else
    Accept := false;
end;

procedure TChannelsPropForm.vstChannelsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column = 1;
end;

procedure TChannelsPropForm.vstChannelsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column = 1 then
    Sender.EditNode(Node, Column);
end;

procedure TChannelsPropForm.vstChannelsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var prm: TGMBaseParam;
begin
  case Column of
    0:
      begin
        prm := GMParamFromString(FChannels.Names[Node.Index]);
        if prm = nil then
          CellText := FChannels.Names[Node.Index]
        else
          CellText := prm.GetNameWithPath();
      end;
    1: CellText := Fchannels.ValueFromIndex[Node.Index];
  end;
end;

procedure TChannelsPropForm.vstChannelsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var vst: TVirtualStringTree;
    nd: PVirtualNode;
begin
  vst := TVirtualStringTree(Sender);

  if (Key = VK_DELETE) and (vst.SelectedCount > 0) then
  begin
    nd := vst.RootNode.LastChild;
    while nd <> nil do
    begin
      if vst.Selected[nd] then
        FChannels.Delete(nd.Index);
      nd := nd.PrevSibling;
    end;

    vst.RootNodeCount := FChannels.Count;
    vst.Refresh();
  end;
end;

procedure TChannelsPropForm.vstChannelsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
begin
  FChannels.ValueFromIndex[Node.Index] := NewText;
end;

initialization
  RegisterDefaultPropEditForm(TChannelProp, TChannelsPropForm);
end.
