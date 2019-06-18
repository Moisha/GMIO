unit fLayers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ImgList, FlexBase, ComCtrls, FlexEdit.Main, ToolWin, Menus,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

const
  WM_FREEDRAG = WM_USER + 1;

type
  TfmLayers = class(TForm)
    panLayers: TcxGroupBox;
    panSpace: TcxGroupBox;
    tvLayers: TTreeView;
    imgStates: TImageList;
    tbrLayers: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    pmLayers: TPopupMenu;
    Newlayer1: TMenuItem;
    Deletelayer1: TMenuItem;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvLayersStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure tvLayersDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvLayersClick(Sender: TObject);
    procedure tvLayersDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvLayersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvLayersEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure tvLayersCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private declarations }
    FActiveFlex: TFlexPanel;
    FDragObject: TDragObject;
    procedure SetActiveFlex(const Value: TFlexPanel);
    function  GetIconIndex(Layer: TFlexLayer): integer;
    procedure WMFreeDrag(var Message: TMessage); message WM_FREEDRAG;
  public
    { Public declarations }
    property  ActiveFlex: TFlexPanel read FActiveFlex write SetActiveFlex;
    procedure UpdateData(Layer: TFlexLayer = Nil);
  end;

var
  fmLayers: TfmLayers;

implementation

{$R *.DFM}

uses
  ToolMngr;

// TfmLayers //////////////////////////////////////////////////////////////////

procedure TfmLayers.FormCreate(Sender: TObject);
begin
 RegisterToolForm(Self);
 UpdateData;
end;

procedure TfmLayers.FormDestroy(Sender: TObject);
begin
 UnRegisterToolForm(Self);
 fmLayers := Nil;
end;

procedure TfmLayers.SetActiveFlex(const Value: TFlexPanel);
begin
 if Value = FActiveFlex then exit;
 FActiveFlex := Value;
 UpdateData;
end;

procedure TfmLayers.UpdateData(Layer: TFlexLayer = Nil);
var i, IconIdx, Count: integer;
    SelNode: TTreeNode;
    Sel: TFlexLayer;
begin
 Count := 0;
 tvLayers.Items.BeginUpdate;
 try
  if not Assigned(FActiveFlex) then begin
   tvLayers.Items.Clear;
   exit;
  end;
  if tvLayers.Items.Count <> FActiveFlex.Layers.Count then begin
   while tvLayers.Items.Count > FActiveFlex.Layers.Count do
    tvLayers.Items.Delete(tvLayers.Items[tvLayers.Items.Count-1]);
   while tvLayers.Items.Count < FActiveFlex.Layers.Count do
    tvLayers.Items.Add(Nil, '');
  end;
  Sel := FActiveFlex.ActiveLayer;
  SelNode := Nil;
  Count := FActiveFlex.Layers.Count;
  for i:=0 to Count-1 do with tvLayers.Items[i] do begin
   Text := FActiveFlex.Layers[i].Name;
   Data := FActiveFlex.Layers[i];
   IconIdx := GetIconIndex(FActiveFlex.Layers[i]);
   ImageIndex := IconIdx;
   SelectedIndex := IconIdx;
   StateIndex := IconIdx;
   if FActiveFlex.Layers[i] = Sel then SelNode := tvLayers.Items[i];
  end;
  tvLayers.Selected := SelNode;
 finally
  tvLayers.Items.EndUpdate;
  if Count = 0
   then panLayers.Caption := ''
   else panLayers.Caption := ' '+IntToStr(Count)+' Layer(s)';
 end;
end;

function TfmLayers.GetIconIndex(Layer: TFlexLayer): integer;
var IsVisible, IsEditable, IsMoveable: boolean;
begin
 if not Assigned(Layer) then begin
  Result := -1;
  exit;
 end;
 IsVisible := Layer.Visible;
 IsEditable := Layer.Selectable and not Layer.ReadOnly;
 IsMoveable := Layer.Moveable;
 Result := 4*byte(not IsVisible) + 2*byte(not IsEditable) + byte(not IsMoveable);
end;

procedure TfmLayers.tvLayersStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
 DragObject := TDragObject.Create;
 FDragObject := DragObject;
end;

procedure TfmLayers.tvLayersDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept := Assigned(Source);
end;

procedure TfmLayers.tvLayersDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var Node: TTreeNode;
    i, Src, Targ: integer;
    Layer: TFlexLayer;
begin
 if not Assigned(FActiveFlex) then exit;
 Node := tvLayers.GetNodeAt(X, Y);
 if not Assigned(Node) then
  // Get last node
  Node := tvLayers.Items[tvLayers.Items.Count-1];
 if not Assigned(Node) or (tvLayers.Selected = Node) then exit;
 // Define layers
 Layer := TFlexLayer(tvLayers.Selected.Data);
 Src := FActiveFlex.Layers.IndexOf(Layer);
 Targ := FActiveFlex.Layers.IndexOf(TFlexLayer(Node.Data));
 FActiveFlex.Layers.ChangeOrder(Src, Targ);
 // Find new active layer
 for i:=0 to tvLayers.Items.Count-1 do
  if tvLayers.Items[i].Data = Layer then begin
   tvLayers.Selected := tvLayers.Items[i];
   break;
  end;
 // Need free drag object
 PostMessage(Handle, WM_FREEDRAG, 0, 0);
end;

procedure TfmLayers.WMFreeDrag(var Message: TMessage);
begin
 // Free FDragObject
 FDragObject.Free;
 FDragObject := Nil;
end;

procedure TfmLayers.tvLayersClick(Sender: TObject);
begin
 if not Assigned(tvLayers.Selected) or not Assigned(FActiveFlex) then exit;
 FActiveFlex.ActiveLayer := TFlexLayer(tvLayers.Selected.Data);
end;

procedure TfmLayers.tvLayersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Node: TTreeNode;
begin
 if not Assigned(FActiveFlex) then exit;
 if X < imgStates.Width then begin
  Node := tvLayers.GetNodeAt(X, Y);
  if not Assigned(Node) then exit;
  // Click on layer state image
  if X < imgStates.Width div 3 then begin
   // Visible property
   with TFlexLayer(Node.Data) do Visible := not Visible;
  end else
  if X < 2*(imgStates.Width div 3) then begin
   // Editable (ReadOnly + Selectable) property
   with TFlexLayer(Node.Data) do
    if not ReadOnly and Selectable then begin
     ReadOnly := true;
     Selectable := false;
    end else begin
     ReadOnly := false;
     Selectable := true;
    end;
  end else begin
   // Moveable property
   with TFlexLayer(Node.Data) do Moveable := not Moveable;
  end;
  if Assigned(Node) then FActiveFlex.ActiveLayer := TFlexLayer(Node.Data);
  // Move to selected layer
 { for i:=0 to tvLayers.Items.Count-1 do
   if tvLayers.Items[i].Data = FActiveFlex.ActiveLayer then begin
    tvLayers.Selected := tvLayers.Items[i];
    break;
   end; }
 end;
end;

procedure TfmLayers.tvLayersEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
 TFlexLayer(Node.Data).Name := s;
 s := TFlexLayer(Node.Data).Name;
end;

procedure TfmLayers.tvLayersCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var Layer: TFlexLayer;
begin
 Layer := TFlexLayer(Node.Data);
 // Check active layer
 if not (cdsSelected in State) and
    Assigned(FActiveFlex) and (Layer = FActiveFlex.ActiveLayer) then
   // Set red font color for active layer
   tvLayers.Canvas.Font.Color := clRed;
end;

end.
