unit Frame.NodeTree;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, VirtualTrees,
  GMGlobals, Generics.Collections, BaseNodeReader, Vcl.Menus, GMConst, ActiveX, Vcl.ImgList,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type

  TfrmNodeTree = class(TFrame)
    Tree: TVirtualStringTree;
    Panel1: TcxGroupBox;
    pmNodeTree: TPopupMenu;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miExpandAll: TMenuItem;
    miSeparator1: TMenuItem;
    pmTree: TPopupMenu;
    N1: TMenuItem;
    ilNodeTree: TImageList;
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure miAddClick(Sender: TObject);
    procedure TreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miDeleteClick(Sender: TObject);
    procedure TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure TreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
      Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure miExpandAllClick(Sender: TObject);
    procedure pmNodeTreePopup(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
  private
    { Private declarations }
    FCheckBoxLevels: SetOfInt;
    reader: TBaseNodeReader;
    FReadOnly: bool;
    procedure AddLoadedChildren(Node: PVirtualNode);
    procedure DelCurrentNode;
    function AddNode(vnd: PVTNodeData): PVirtualNode;
    procedure SetReadOnly(const Value: bool);
    procedure ActivateCheckBoxes_CycleProc(nd: PVirtualNode; CycleObject: pointer);
  public
    { Public declarations }
    procedure AfterConstruction(); override;
    destructor Destroy; override;
    property ReadOnly: bool read FReadOnly write SetReadOnly;
    procedure Load(bWithInit: bool; readerClass: TBaseNodeReaderClass);
    procedure LoadAllTree();
    procedure ActivateCheckBoxes(CheckBoxLevels: SetOfInt);
    function IsChannelSelected(): bool;
    function SelectedObject(): TObject;
  end;

implementation

{$R *.dfm}

uses VT_Utils, GMDBClasses;

procedure TfrmNodeTree.AfterConstruction;
begin
  inherited;
  Tree.NodeDataSize := SizeOf(TVTNodeData);
  reader := nil;
end;

destructor TfrmNodeTree.Destroy;
begin
  if reader <> nil then
    reader.Free();

  inherited;
end;

function TfrmNodeTree.IsChannelSelected: bool;
var v: PVTNodeData;
    nd: PVirtualNode;
begin
  Result := false;
  nd := Tree.FocusedNode;
  if nd = nil then Exit;

  v := Tree.GetNodeData(nd);
  Result := v.ID_Prm > 0;
end;

procedure TfrmNodeTree.ActivateCheckBoxes_CycleProc(nd: PVirtualNode; CycleObject: pointer);
var v: PVTNodeData;
begin
  v := Tree.GetNodeData(nd);
  if v.Obj is TGMNodeChannel then
  begin
    if NODE_LEVEL_PARAM in FCheckBoxLevels then
      Tree.CheckType[nd] := ctCheckBox;
  end
  else
  begin
    if NODE_LEVEL_OBJECT in FCheckBoxLevels then
      Tree.CheckType[nd] := ctCheckBox;
  end
end;

procedure TfrmNodeTree.ActivateCheckBoxes(CheckBoxLevels: SetOfInt);
begin
  FCheckBoxLevels := CheckBoxLevels;
  CycleTree(Tree.RootNode, ActivateCheckBoxes_CycleProc, nil);
end;

procedure TfrmNodeTree.AddLoadedChildren(Node: PVirtualNode);
var v: TVTNodeData;
    p: PVTNodeData;
    nd: PVirtualNode;
begin
  for v in reader.ReadResult do
  begin
    nd := Tree.AddChild(Node);
    Include(nd.States, vsInitialized);
    Tree.HasChildren[nd] := v.ID_Prm <= 0; // каналы не имеют потомков

    p := Tree.GetNodeData(nd);
    p^ := v;
  end;
end;

procedure TfrmNodeTree.Load(bWithInit: bool; readerClass: TBaseNodeReaderClass);
begin
  if bWithInit then
    Tree.Clear();

  if Reader <> nil then
    reader.Free();

  reader := readerClass.Create();
  reader.ReadChildren(0);
  AddLoadedChildren(nil);
end;

procedure TfrmNodeTree.LoadAllTree;
begin
  Tree.FullExpand();
  Tree.FullCollapse();
end;

function TfrmNodeTree.AddNode(vnd: PVTNodeData): PVirtualNode;
var ndp: PVirtualNode;
    idp: int;
    v: PVTNodeData;
begin
  ndp := Tree.FocusedNode;
  idp := 0;
  if ndp <> nil then
  begin
    v := Tree.GetNodeData(ndp);
    idp := v.ID_Obj;
  end;

  reader.AddNew(idp, vnd);
  Result := Tree.AddChild(ndp);
  v := Tree.GetNodeData(Result);
  v^ := vnd^;

  if ndp <> nil then
    Tree.Expanded[ndp] := true;

  SelectNode(Result);
end;

procedure TfrmNodeTree.miAddClick(Sender: TObject);
var v: TVTNodeData;
begin
  InitVND(@v);
  v.sTxt := 'Новый узел';
  v.ObjType := NODE_TYPE_NODE;
  AddNode(@v);
end;

procedure TfrmNodeTree.miDeleteClick(Sender: TObject);
begin
  DelCurrentNode();
end;

procedure TfrmNodeTree.miExpandAllClick(Sender: TObject);
begin
  Tree.FullExpand(Tree.FocusedNode);
end;

procedure TfrmNodeTree.N1Click(Sender: TObject);
begin
  Tree.FullExpand(Tree.FocusedNode);
end;

procedure TfrmNodeTree.pmNodeTreePopup(Sender: TObject);
begin
  miAdd.Visible := not FReadOnly;
  miDelete.Visible := not FReadOnly;
  miSeparator1.Visible := not FReadOnly;
end;

function TfrmNodeTree.SelectedObject: TObject;
begin
  Result := nil;
  if Tree.FocusedNode <> nil then
    Result := PVTNodeData(Tree.GetNodeData(Tree.FocusedNode)).Obj;
end;

procedure TfrmNodeTree.SetReadOnly(const Value: bool);
begin
  FReadOnly := Value;
  Panel1.Visible := FReadOnly;
end;

procedure TfrmNodeTree.TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var vSrc: PVTNodeData;
    vNew: TVTNodeData;
    srcTree: TVirtualStringTree;
    HitInfo: THitInfo;
begin
  Tree.GetHitTestInfoAt(Pt.X, Pt.Y, false, HitInfo);
  if (HitInfo.HitNode = nil) or not (hiOnItem in HitInfo.HitPositions) then Exit;

  srcTree := TVirtualStringTree(Source);

  vSrc := srcTree.GetNodeData(srcTree.FocusedNode);
  InitVND(@vNew);

  vNew.sTxt := vSrc.sTxt;
  vNew.BaseChn := vSrc.ID_Prm;
  vNew.ID_PT := vSrc.ID_PT;

  SelectNode(HitInfo.HitNode);
  AddNode(@vNew);
end;

procedure TfrmNodeTree.TreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
  Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var v: PVTNodeData;
    srcTree: TVirtualStringTree;
    HitInfo: THitInfo;
begin
  Accept := (Mode = dmOnNode)
             and (Source is TVirtualStringTree)
             and (TVirtualStringTree(Source).NodeDataSize = sizeOf(TVTNodeData));
  if not Accept then Exit;

  srcTree := TVirtualStringTree(Source);
  v := srcTree.GetNodeData(srcTree.FocusedNode);
  Accept := (v.ID_Prm > 0);
  if not Accept then Exit;

  Tree.GetHitTestInfoAt(Pt.X, Pt.Y, false, HitInfo);
  Accept := HitInfo.HitNode <> nil;
  if not Accept then Exit;

  v := Tree.GetNodeData(HitInfo.HitNode);
  Accept := v.ID_Prm <= 0; // Это пока будет единственный метод добавки каналов к узлам
end;

procedure TfrmNodeTree.TreeEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
var v: PVTNodeData;
begin
  v := Tree.GetNodeData(Node);
  Allowed := not ReadOnly and (v.ID_Prm <= 0);
end;

procedure TfrmNodeTree.TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
var v: PVTNodeData;
begin
  if Tree.HasChildren[Node] and (Node.FirstChild = nil) then
  begin
    v := Tree.GetNodeData(Node);
    reader.ReadChildren(v.ID_Obj);
    AddLoadedChildren(Node);
  end;
end;

procedure TfrmNodeTree.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var v: PVTNodeData;
begin
  v := Tree.GetNodeData(Node);
  Finalize(v^);
end;

procedure TfrmNodeTree.TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
  var ImageIndex: Integer);
var v: PVTNodeData;
begin
  ImageIndex := -1;
  if Kind in [ikNormal, ikSelected] then
  begin
    v := Tree.GetNodeData(Node);
    if v.ID_Prm > 0 then
      ImageIndex := 1
    else
    if v.ObjType < ilNodeTree.Count - 2 then
      ImageIndex := v.ObjType + 2
    else
      ImageIndex := 0;
  end;
end;

procedure TfrmNodeTree.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var v: PVTNodeData;
begin
  v := Tree.GetNodeData(Node);
  CellText := v.sTxt;
end;

procedure TfrmNodeTree.DelCurrentNode();
var v: PVTNodeData;
    nd: PVirtualNode;
begin
  nd := Tree.FocusedNode;
  if nd = nil then Exit;

  v := Tree.GetNodeData(nd);
  reader.Delete(v);

  DelNodeAndSelectNextSibling(nd);
end;

procedure TfrmNodeTree.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    DelCurrentNode();
end;

procedure TfrmNodeTree.TreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var v: PVTNodeData;
begin
  v := Tree.GetNodeData(Node);
  v.sTxt := NewText;
  reader.NewCaption(v);
end;

end.
