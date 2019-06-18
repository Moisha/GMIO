////////////////////////////////////////////
// Дерево объектов, приборов и каналов
////////////////////////////////////////////
unit Frame.ObjectTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, GMConst, GMGlobals, Math, StrUtils, GMDBClasses,
  UITypes, Vcl.ExtCtrls, Frame.NodeTree,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  TFrameObjType = (fotSpecified, fotAll, fotActive);
  TDeviceSystemViewMode = (dsvmObjects, dsvmDevices, dsvmAll);
  TLastFocusedTree = (LastFocusedTreeObjects, LastFocusedTreeNodes);

  TObjTreeFrame = class(TFrame)
    Tree: TVirtualStringTree;
    frmNodes: TfrmNodeTree;
    Splitter1: TcxSplitter;
    pnSearch: TcxGroupBox;
    eSearch: TcxButtonEdit;
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure TreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure TreeDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure TreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeEnter(Sender: TObject);
    procedure frmNodesTreeEnter(Sender: TObject);
    procedure TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure frmNodesTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure frmNodesTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  private
    FLastFocusedTree: TLastFocusedTree;
    procedure InitNode(Node: PVirtualNode);
    procedure EnlistCheckedParam(nd: PVirtualNode; CycleObject: pointer);
    function FindBaseChn(ndDev: PVirtualNode; ID_Prm: int): PVirtualNode;
    function IsNodeParam(nd: PVirtualNode): bool;
    procedure FindNode(nd: PVirtualNode; CycleObject: pointer);
    function GetTreeNodeData(nd: PVirtualNode = nil): PVTNodeData;
    procedure ShowDeviceSystemItems(nd: PVirtualNode; CycleObject: pointer);
    procedure SearchTree(nd: PVirtualNode; CycleObject: pointer);
  public
    bHighLightObjects: bool;

    function IsSourceTree(Source: TObject): bool;
    function SelectedParam: TGMBaseParam;
    function SelectedObject: TObject;
    function IsParamSelected: bool;
    procedure FindObject(obj: TObject);
    procedure Init(CheckBoxLevels: SetOfInt = []);
    procedure ShowObjects(ObjShowType: TFrameObjType; ID_Obj: int = 0);
    procedure GetCheckedParamsList(lst: TList);
    procedure SetDeviceSystemViewMode(m: TDeviceSystemViewMode);
    procedure HideNodeTree();
    constructor Create(AObject: TComponent); override;
  end;

implementation

uses VT_Utils, ArrayNodeReader;

{$R *.dfm}

{ TObjTreeFrame }

function TObjTreeFrame.SelectedObject: TObject;
begin
  case FLastFocusedTree of
    LastFocusedTreeObjects: Result := GetTreeNodeData().Obj;
    LastFocusedTreeNodes: Result := frmNodes.SelectedObject();
    else Result := nil;
  end;
end;

function TObjTreeFrame.SelectedParam(): TGMBaseParam;
begin
  Result := nil;
  if IsParamSelected() then
    Result := TGMBaseParam(SelectedObject());
end;

procedure TObjTreeFrame.SetDeviceSystemViewMode(m: TDeviceSystemViewMode);
begin
  CycleTree(Tree.RootNode, ShowDeviceSystemItems, pointer(m));
end;

function TObjTreeFrame.GetTreeNodeData(nd: PVirtualNode = nil): PVTNodeData;
var ndSel: PVirtualNode;
begin
  if nd = nil then
  begin
    ndSel := Tree.GetFirstSelected();
    if ndSel <> nil then
      Result := PVTNodeData(Tree.GetNodeData(ndSel))
    else
      Result := nil;
  end
  else
    Result := PVTNodeData(Tree.GetNodeData(nd));
end;

procedure TObjTreeFrame.HideNodeTree;
begin
  frmNodes.Visible := false;
  Splitter1.Visible := false;
  FLastFocusedTree := LastFocusedTreeObjects;
end;

procedure TObjTreeFrame.InitNode(Node: PVirtualNode);
begin
  InitVND(GetTreeNodeData(Node));
  Include(Node.States, vsInitialized);
end;                            

function TObjTreeFrame.FindBaseChn(ndDev: PVirtualNode; ID_Prm: int): PVirtualNode;
var nd: PVirtualNode;
begin
  Result := nil;
  nd := ndDev.FirstChild;
  while nd <> nil do
  begin
    if GetTreeNodeData(nd).ID_Prm = ID_Prm then
    begin
      Result := nd;
      Exit;
    end;

    nd := nd.NextSibling;
  end;
end;

procedure TObjTreeFrame.Init(CheckBoxLevels: SetOfInt = []);
var i, j, k: int;
    ndObj, ndDev, ndPrm, baseChn: PVirtualNode;
    vnd: PVTNodeData;
begin
  Tree.OnGetText := TreeGetText;
  Tree.Clear();
  Tree.Header.Columns[0].Options := Tree.Header.Columns[0].Options - [coVisible];

  for i := 0 to GMObjects.Count - 1 do
  begin
    ndObj := Tree.AddChild(nil);
    InitNode(ndObj);
    if 0 in CheckBoxLevels then
      Tree.CheckType[ndObj] := ctCheckBox
    else
      Tree.CheckType[ndObj] := ctNone;

    vnd := GetTreeNodeData(ndObj);
    vnd.Obj := GMObjects[i];
    vnd.sTxt := GMObjects[i].Name;
    vnd.ID_Obj := GMObjects[i].ID_Obj;
    vnd.N_Car := GMObjects[i].N_Car;

    for j := 0 to GMDevices.Count - 1 do
      if GMDevices[j].Obj.ID_Obj = GMObjects[i].ID_Obj then
      begin
        ndDev := Tree.AddChild(ndObj);
        InitNode(ndDev);
        if 1 in CheckBoxLevels then
          Tree.CheckType[ndDev] := ctCheckBox
        else
          Tree.CheckType[ndDev] := ctNone;

        vnd := GetTreeNodeData(ndDev);
        vnd.Obj := GMDevices[j];
        vnd.sTxt := GMDevices[j].Name;
        vnd.ID_Obj := GMDevices[j].Obj.ID_Obj;
        vnd.ID_Obj := GMDevices[j].ID_Device;
        vnd.ID_DevType := GMDevices[j].ID_DevType;
        vnd.Number485 := GMDevices[j].Number;

        for k := 0 to GMParams.Count - 1 do
          if GMParams[k].Device.ID_Device = GMDevices[j].ID_Device then
          begin
            ndPrm := nil;
            if GMParams[k].BaseChn <> nil then
            begin
              baseChn := FindBaseChn(ndDev, GMParams[k].BaseChn.ID_Prm);
              if baseChn <> nil then
                ndPrm := Tree.AddChild(baseChn);
            end
            else
              ndPrm := Tree.AddChild(ndDev);

            if ndPrm = nil then continue;

            InitNode(ndPrm);
            if 2 in CheckBoxLevels then
              Tree.CheckType[ndPrm] := ctCheckBox
            else
              Tree.CheckType[ndPrm] := ctNone;

            vnd := GetTreeNodeData(ndPrm);
            vnd.Obj := GMParams[k];
            vnd.sTxt := GMParams[k].GetNameWithPath([ppcSrc, ppcPT, ppcNoBase]);
            vnd.ID_Obj := GMDevices[j].Obj.ID_Obj;
            vnd.ID_Device := GMDevices[j].ID_Device;
            vnd.ID_DevType := GMDevices[j].ID_DevType;
            vnd.ID_Prm := GMParams[k].ID_Prm;
            vnd.ID_PT := GMParams[k].PType.ID_PT;
          end;
      end;
  end;

  frmNodes.ReadOnly := true;
  frmNodes.Load(true, TArrayNodeReader);
  frmNodes.LoadAllTree();
  frmNodes.ActivateCheckBoxes(CheckBoxLevels);
end;

procedure TObjTreeFrame.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := sizeof(TVTNodeData);
end;

procedure TObjTreeFrame.TreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var vnd: PVTNodeData;
    n: int;
begin
  vnd := GetTreeNodeData(Node);
  case Column of
    1: CellText := vnd.sTxt;
    2: begin
         n := max(vnd.N_Car, vnd.Number485);
         CellText := IfThen(n > 0, IntToStr(n));
       end;
    else CellText := '';
  end;
end;

procedure TObjTreeFrame.TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  UpdateSortOrder(Sender, HitInfo.Column);
end;

procedure TObjTreeFrame.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var vnd1, vnd2: PVTNodeData;
    s1, s2: WideString;
begin
  vnd1 := GetTreeNodeData(Node1);
  vnd2 := GetTreeNodeData(Node2);

  s1 := Tree.Text[Node1, Column];
  s2 := Tree.Text[Node2, Column];

  case Sender.GetNodeLevel(Node1) of
    NODE_LEVEL_OBJECT:
      begin
        if Column = 2 then
        begin
          Result := CompareValue(vnd1.N_Car, vnd2.N_Car)
        end
        else
        begin
          Result := CompareStr(s1, s2);
          if Result = 0 then
            Result := CompareValue(vnd1.N_Car, vnd2.N_Car)
        end;
      end;

    NODE_LEVEL_DEVICE:
      begin
        if Column = 0 then
          Result := CompareStr(s1, s2)
        else
          Result := CompareValue(vnd1.Number485, vnd2.Number485);
      end;

    else
      begin
        Result := CompareValue(vnd1.ID_Src, vnd2.ID_Src);
        if Result = 0 then
          Result := CompareValue(vnd1.N_Src, vnd2.N_Src);

        if Tree.Header.SortDirection = sdDescending then
          Result := -Result;
      end;
  end;
end;

procedure TObjTreeFrame.TreeDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := true;
end;

procedure TObjTreeFrame.TreeDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := Mode <> dmOnNode;
end;

procedure TObjTreeFrame.TreeEnter(Sender: TObject);
begin
  FLastFocusedTree := LastFocusedTreeObjects;
end;

procedure TObjTreeFrame.FindNode(nd: PVirtualNode; CycleObject: pointer);
var obj: TObject;
begin
  obj := TObject(CycleObject);
  if (obj is TGMBaseParam) and (GetTreeNodeData(nd).ID_Prm = TGMBaseParam(obj).ID_Prm)
     or (obj is TGMObject) and (GetTreeNodeData(nd).ID_Obj = TGMObject(obj).ID_Obj)
     or (obj is TGMDevice) and (GetTreeNodeData(nd).ID_Device = TGMDevice(obj).ID_Device)
     or (obj is TGMNode) and (GetTreeNodeData(nd).ID_Obj = TGMNode(obj).ID_Node)
   then
  begin
    TreeFromNode(nd).Selected[nd] := true;
    TreeFromNode(nd).FocusedNode := nd;
  end;
end;

procedure TObjTreeFrame.frmNodesTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var s1, s2: string;
begin
  s1 := TVirtualStringTree(Sender).Text[Node1, Column];
  s2 := TVirtualStringTree(Sender).Text[Node2, Column];

  Result := CompareText(s1, s2);
end;

procedure TObjTreeFrame.frmNodesTreeEnter(Sender: TObject);
begin
  FLastFocusedTree := LastFocusedTreeNodes;
end;

procedure TObjTreeFrame.frmNodesTreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  UpdateSortOrder(Sender, HitInfo.Column);
end;

procedure TObjTreeFrame.ShowDeviceSystemItems(nd: PVirtualNode; CycleObject: pointer);
begin
  if TreeFromNode(nd).GetNodeLevel(nd) > 0 then
    TreeFromNode(nd).IsVisible[nd] := false
  else
    TreeFromNode(nd).HasChildren[nd] := false;
end;


procedure TObjTreeFrame.FindObject(obj: TObject);
begin
  if obj = nil then Exit;

  if obj is TGMParam then
    CycleTree(Tree.RootNode, FindNode, obj);

  if obj is TGMNodeChannel then
    CycleTree(frmNodes.Tree.RootNode, FindNode, obj);
end;

function TObjTreeFrame.IsNodeParam(nd: PVirtualNode): bool;
begin
  Result := Tree.GetNodeLevel(nd) in [NODE_LEVEL_PARAM, NODE_LEVEL_SUBPARAM];
end;

function TObjTreeFrame.IsParamSelected(): bool;
begin
  case FLastFocusedTree of
    LastFocusedTreeObjects: Result := IsNodeParam(Tree.GetFirstSelected());
    LastFocusedTreeNodes: Result := frmNodes.IsChannelSelected();
    else Result := false;
  end;
end;

function TObjTreeFrame.IsSourceTree(Source: TObject): bool;
begin
  Result := (Source = Tree) or (Source = frmNodes.Tree);
end;

procedure TObjTreeFrame.ShowObjects(ObjShowType: TFrameObjType; ID_Obj: int = 0);
var nd: PVirtualNode;
    bShow: bool;
    obj: TGMObject;
begin
  nd := Tree.RootNode.FirstChild;
  while nd <> nil do
  begin

    case ObjShowType of
      fotAll: bShow := true;
      fotSpecified: bShow := (GetTreeNodeData(nd).ID_Obj = ID_Obj);
      fotActive:
        begin
          bShow := false;
          obj := GMObjects.ObjByID(GetTreeNodeData(nd).ID_Obj);
          if obj <> nil then
            bShow := obj.Active;
        end;
      else bShow := true;
    end;

    if bShow then
    begin
      Tree.IsVisible[nd] := true;
      if ID_Obj > 0 then
        Tree.FullExpand(nd);
    end
    else
      Tree.IsVisible[nd] := false;

    nd := nd.NextSibling;
  end;

  Tree.Refresh();
end;

var bFocusedNodePassed, bNodeFound: bool;

procedure TObjTreeFrame.SearchTree(nd: PVirtualNode; CycleObject: pointer);
var s: string;
begin
  if bNodeFound or not Tree.IsVisible[nd] or not Tree.VisiblePath[nd] then Exit;

  if bFocusedNodePassed then
  begin
    s := Trim(LowerCase(eSearch.Text));
    if Pos(s, LowerCase(Tree.Text[nd, 1])) > 0 then
    begin
      SelectNode(nd);
      bNodeFound := true;
    end;
  end
  else
  begin
    bFocusedNodePassed := nd = Tree.FocusedNode;
  end
end;

procedure TObjTreeFrame.cxButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  bFocusedNodePassed := (Tree.FocusedNode = nil)
                        or not Tree.IsVisible[Tree.FocusedNode]
                        or not Tree.VisiblePath[Tree.FocusedNode];
  bNodeFound := false;
  CycleTree(Tree.RootNode, SearchTree, Tree.FocusedNode);

  if not bNodeFound then
  begin
    bFocusedNodePassed := true;
    CycleTree(Tree.RootNode, SearchTree, Tree.FocusedNode);
  end;
end;

procedure TObjTreeFrame.EnlistCheckedParam(nd: PVirtualNode; CycleObject: pointer);
var Tree: TBaseVirtualTree;
    vnd: PVTNodeData;
begin
  Tree := TreeFromNode(nd);
  vnd := Tree.GetNodeData(nd);

  if (TObject(CycleObject) is TList)
     and (Tree.CheckState[nd] = csCheckedNormal) then
  begin
    TList(CycleObject).Add(vnd.Obj);
  end;
end;

procedure TObjTreeFrame.GetCheckedParamsList(lst: TList);
begin
  CycleTree(Tree.RootNode, EnlistCheckedParam, lst);
  CycleTree(frmNodes.Tree.RootNode, EnlistCheckedParam, lst);
end;

procedure TObjTreeFrame.TreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if bHighLightObjects and (Tree.GetNodeLevel(Node) = NODE_LEVEL_OBJECT) then
    TargetCanvas.Font.Style := Tree.Font.Style + [fsBold]
  else
    TargetCanvas.Font.Style := Tree.Font.Style - [fsBold];
end;

procedure TObjTreeFrame.TreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Finalize(PVTNodeData(Tree.GetNodeData(Node))^);
end;

constructor TObjTreeFrame.Create(AObject: TComponent);
begin
  inherited;

  bHighLightObjects := true;
end;

end.
