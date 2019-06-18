////////////////////////////////////////////
// Процедуры для работы с деревом
////////////////////////////////////////////
unit VT_Utils;

interface
uses GMGlobals, VirtualTrees, Classes, Types;

type
  TCycleTreeProc = procedure (nd: PVirtualNode; CycleObject: pointer) of object;
  TCycleTreeProcRef = reference to procedure (nd: PVirtualNode);

procedure CycleTree(root: PVirtualNode; proc: TCycleTreeProc; CycleObject: pointer = nil); overload;
procedure CycleTree(root: PVirtualNode; proc: TCycleTreeProcRef); overload;
procedure VST_SortByDragging(vst: TBaseVirtualTree; Pt: TPoint; Mode: TDropMode);
procedure DelNodeAndSelectNextSibling(Node: PVirtualNode);
procedure SelectNode(nd: PVirtualNode);
procedure UpdateSortOrder(header: TVTHeader; Column: TColumnIndex);

implementation

procedure UpdateSortOrder(header: TVTHeader; Column: TColumnIndex);
begin
  if header.SortColumn = Column then
  begin
    if header.SortDirection = sdAscending then
      header.SortDirection := sdDescending
    else
      header.SortDirection := sdAscending;
  end
  else
    header.SortColumn := Column;
end;

procedure SelectNode(nd: PVirtualNode);
begin
  TreeFromNode(nd).Selected[nd] := true;
  TreeFromNode(nd).FocusedNode := nd;
  TreeFromNode(nd).FullyVisible[nd] := true;
end;

procedure DelNodeAndSelectNextSibling(Node: PVirtualNode);
var nd: PVirtualNode;
    tree: TBaseVirtualTree;
begin
  nd := Node.NextSibling;
  if nd = nil then
    nd := Node.PrevSibling;
  if (nd = nil) and (Node.Parent <> TreeFromNode(Node).RootNode) then
    nd := Node.Parent;

  tree := TreeFromNode(Node);
  tree.DeleteNode(Node);
  if nd <> nil then
    SelectNode(nd);
end;

procedure CycleTree(root: PVirtualNode; proc: TCycleTreeProc; CycleObject: pointer = nil);
var nd: PVirtualNode;
    Tree: TBaseVirtualTree;
begin
  Tree := TreeFromNode(root);
  if root = nil then
    nd := Tree.RootNode.FirstChild
  else
    nd := root.FirstChild;

  while nd <> nil do
  begin
    proc(nd, CycleObject);
    CycleTree(nd, proc, CycleObject);

    nd := nd.NextSibling;
  end;
end;

procedure CycleTree(root: PVirtualNode; proc: TCycleTreeProcRef);
var
  nd: PVirtualNode;
  Tree: TBaseVirtualTree;
begin
  Tree := TreeFromNode(root);
  if root = nil then
    nd := Tree.RootNode.FirstChild
  else
    nd := root.FirstChild;

  while nd <> nil do
  begin
    proc(nd);
    CycleTree(nd, proc);

    nd := nd.NextSibling;
  end;
end;

procedure VST_SortByDragging(vst: TBaseVirtualTree; Pt: TPoint; Mode: TDropMode);
var h: THitInfo;
begin
    vst.GetHitTestInfoAt(Pt.X, Pt.Y, false, h);
    if h.HitNode = vst.GetFirstSelected() then Exit;

    if h.HitNode <> nil then
    begin
      case Mode of
        dmAbove, dmOnNode: vst.MoveTo(vst.GetFirstSelected(), h.HitNode, amInsertBefore, false);
        dmBelow: vst.MoveTo(vst.GetFirstSelected(), h.HitNode, amInsertAfter, false);
      end;
    end
    else
      vst.MoveTo(vst.GetFirstSelected(), vst.RootNode.LastChild, amInsertAfter, false);
end;

end.
