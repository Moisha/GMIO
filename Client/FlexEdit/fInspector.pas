unit fInspector;

interface

uses
  Windows, Types, UITypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Ex_Grid, Ex_Inspector, StdCtrls, ExtCtrls, ImgList,
  FlexBase, FlexProps, FlexControls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

const
  WM_REFRESH_DATA  = WM_USER + 1;

type
  TfmInspector = class(TForm)
    Panel1: TcxGroupBox;
    cbInspItems: TcxComboBox;
    imgCtrlIcons: TImageList;
    grInspector: TExInspector;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grInspectorGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: String);
    procedure grInspectorGetEditStyle(Sender: TObject; Cell: TGridCell;
      var Style: TGridEditStyle);
    procedure grInspectorEditButtonPress(Sender: TObject; Cell: TGridCell);
    procedure grInspectorSetEditText(Sender: TObject; Cell: TGridCell;
      var Value: String);
    procedure grInspectorGetEditList(Sender: TObject; Cell: TGridCell;
      Items: TStrings);
    procedure grInspectorChange(Sender: TObject; Cell: TGridCell;
      Selected: Boolean);
    procedure grInspectorDblClick(Sender: TObject);
    procedure cbInspItemsDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure cbInspItemsChange(Sender: TObject);
  private
    { Private declarations }
    FActiveFlex: TFlexPanel;
    FFlexControl: TFlexControl;
    FLastPropName: string;
    procedure grInspectorEditCanModify(Sender: TObject; Cell: TGridCell;
      var CanModify: Boolean);
    procedure grInspectorEditCloseUp(Sender: TObject; Cell: TGridCell;
      ItemIndex: Integer; var Accept: Boolean);
    function  GetControlIconIndex(AControl: TFlexControl): integer;
    procedure SetActiveFlex(const Value: TFlexPanel);
    procedure SetFlexControl(const Value: TFlexControl);
    procedure ChangePropValue(Cell: TGridCell; var Value: string;
      var Accept: Boolean);
    procedure WMRefreshData(var Message: TMessage); message WM_REFRESH_DATA;
    procedure SetcbInspItemsItemIndex(n: integer);
  public
    { Public declarations }
    procedure UpdateData;
    procedure UpdateProps(Prop: TCustomProp);
    property  ActiveFlex: TFlexPanel read FActiveFlex write SetActiveFlex;
    property  Control: TFlexControl read FFlexControl write SetFlexControl;
  end;

var
  fmInspector: TfmInspector;

implementation

{$R *.DFM}

uses
  ToolMngr, FlexActions;

procedure TfmInspector.FormCreate(Sender: TObject);
begin
 RegisterToolForm(Self);
 grInspector.Columns[0].Width := 80;
 grInspector.OnEditCanModify := grInspectorEditCanModify;
 grInspector.OnEditCloseUp := grInspectorEditCloseUp;
end;

procedure TfmInspector.FormDestroy(Sender: TObject);
begin
 UnRegisterToolForm(Self);
 fmInspector := Nil;
end;

procedure TfmInspector.FormShow(Sender: TObject);
begin
 // Avoid align bug
 cbInspItems.Align := alTop;
 cbInspItems.Width := Self.ClientWidth;
end;

procedure TfmInspector.SetActiveFlex(const Value: TFlexPanel);
begin
 if Value = FActiveFlex then exit;
 {if not Assigned(Value) then }Control := Nil;
 FActiveFlex := Value;
 UpdateData;
 UpdateProps(Nil);
end;

procedure TfmInspector.SetcbInspItemsItemIndex(n: integer);
begin
  cbInspItems.Properties.OnChange := nil;
  cbInspItems.ItemIndex := n;
  cbInspItems.Properties.OnChange := cbInspItemsChange;
end;

procedure TfmInspector.SetFlexControl(const Value: TFlexControl);
var i: integer;
begin
 if Value = FFlexControl then exit;
 if Assigned(FActiveFlex) and not
    (csDestroying in FActiveFlex.ComponentState) then
  grInspector.Editing := False;
 grInspector.AllowEdit := False;
 try
  FFlexControl := Value;
  if Assigned(FActiveFlex) and Assigned(FFlexControl) then begin
   SetcbInspItemsItemIndex(cbInspItems.Properties.Items.IndexOfObject(FFlexControl));
   FFlexControl.Props.Sort();
   grInspector.Rows.Count := FFlexControl.Props.VisibleCount;
   for i:=0 to grInspector.Rows.Count-1 do
    if CompareStr(FLastPropName,
         FFlexControl.Props.VisiblePropNames[i]) = 0 then begin
     grInspector.CellFocused := GridCell(1, i);
     break;
    end;
   if grInspector.EditCell.Row >= 0 then begin
    grInspector.Editing := True;
    grInspector.Edit.Text :=
     FFlexControl.Props.VisibleProps[grInspector.EditCell.Row].DisplayValue;
   end;
  end else begin
   SetcbInspItemsItemIndex(-1);
   grInspector.Rows.Count := 0;
  end;
 finally
  grInspector.AllowEdit := True;
  if grInspector.Rows.Count > 0 then begin
   grInspector.AlwaysEdit := True;
   grInspector.Editing := True;
  end;
 end;
end;

procedure TfmInspector.UpdateData;
var Msg: TMsg;
begin
 if HandleAllocated then begin
   // Remove all existing refresh messages
   while PeekMessage(Msg, Handle, WM_REFRESH_DATA, WM_REFRESH_DATA, PM_REMOVE) do ;
   // Add new refresh message to the end
   PostMessage(Handle, WM_REFRESH_DATA, 0, 0);
 end;
end;

procedure TfmInspector.WMRefreshData(var Message: TMessage);
var SelControl, Control, NewControl: TFlexControl;
    StrList, LayersList: TStringList;
    Flex: TFlexPanel;
    Scheme: TFlexCustomScheme;
    PassRec: TPassControlRec;
    i, j, Level, DLevel: integer;
    AName, APrefix, NewPrefix: string;
begin
 StrList := Nil;
 cbInspItems.Properties.Items.BeginUpdate;
 try
{  if cbInspItems.ItemIndex >= 0
   then SelControl := cbInspItems.Items.Objects[cbInspItems.ItemIndex]
   else SelControl := Nil; }
  SelControl := Nil;
  StrList := TStringList.Create;
  Flex := ActiveFlex;
  if Assigned(Flex) and not (csDestroying in Flex.ComponentState) then begin
   if Flex.SelectedCount = 1 then SelControl := Flex.Selected[0];
   Scheme := Flex.ActiveScheme;
   if Assigned(Scheme) then begin
    APrefix := '';
    Control := Scheme;
    FirstControl(Control, PassRec);
    while Assigned(Control) do begin
     AName := Control.Name;
     StrList.AddObject(APrefix + AName, Control);
     Level := High(PassRec.Indexes);
     Control := NextControl(PassRec);
     DLevel := High(PassRec.Indexes) - Level;
     if DLevel = 0 then continue;
     NewPrefix := '';
     NewControl := Control;
     for i:=0 to Abs(DLevel)-1 do
      if DLevel > 0 then begin
       NewControl := NewControl.Parent;
       if not (NewControl is TFlexScheme) then
        NewPrefix := NewControl.Name + '.' + NewPrefix;
      end else begin
       j := Length(APrefix) - 1;
       while (j > 0) and (APrefix[j] <> '.') do dec(j);
       SetLength(APrefix, j);
      end;
     APrefix := APrefix + NewPrefix;
    end;
    StrList.Sort;
    i := StrList.IndexOfObject(Scheme);
    if i >= 0 then StrList.Move(i, 0);
   end;
   if Flex.Layers.Count > 0 then begin
    LayersList := TStringList.Create;
    try
     for i:=0 to Flex.Layers.Count-1 do
      LayersList.AddObject(Flex.Layers[i].Name, Flex.Layers[i]);
     LayersList.Sort;
     j := 0;
     if (StrList.Count > 0) and (StrList.Objects[0] is TFlexScheme) then inc(j);
     for i:=LayersList.Count-1 downto 0 do
      StrList.InsertObject(j, LayersList[i], LayersList.Objects[i]);
    finally
     LayersList.Free;
    end;
   end;
  end;
  cbInspItems.Properties.Items.Assign(StrList);
  SetcbInspItemsItemIndex(cbInspItems.Properties.Items.IndexOfObject(SelControl));
  Self.Control := SelControl;
 finally
  cbInspItems.Properties.Items.EndUpdate;
  StrList.Free;
 end;
end;

procedure TfmInspector.UpdateProps(Prop: TCustomProp);
var Index: integer;
    Value: string;
begin
 if Assigned(Prop) and Assigned(FFlexControl) then begin
  Index := FFlexControl.Props.VisibleIndexOf(Prop);
  if Index >= 0 then begin
   if grInspector.EditCell.Row = Index then begin
    Value := '';
    grInspectorGetCellText(grInspector, GridCell(1, Index), Value);
    if grInspector.Edit.DropListVisible and
       Assigned(grInspector.Edit.ActiveList) then
     with TGridListBox(grInspector.Edit.ActiveList) do begin
      Index := Items.IndexOf(Value);
      ItemIndex := Index;
     end
    else
     grInspector.Edit.Text := Value;
   end else
    grInspector.InvalidateCell(GridCell(1, Index));
  end;
 end else
  grInspector.Refresh(); // InvalidateColumn(1)
end;

procedure TfmInspector.grInspectorGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: String);
begin
 if not Assigned(FFlexControl) then exit;
 try
  if Cell.Col = 0 then
   Value := FFlexControl.Props.VisiblePropTranslatedNames[Cell.Row]
  else
   Value := FFlexControl.Props.VisibleProps[Cell.Row].DisplayValue;
 except
  Value := '';
 end;
end;

procedure TfmInspector.grInspectorGetEditStyle(Sender: TObject;
  Cell: TGridCell; var Style: TGridEditStyle);
var Prop: TCustomProp;
begin
 if not Assigned(FFlexControl) then exit;
 Prop := FFlexControl.Props.VisibleProps[Cell.Row];
 if psEditForm in Prop.Style then
  Style := geEllipsis
 else
 if Prop.IsEnum then
  Style := gePickList
 else
  Style := geSimple;
end;

procedure TfmInspector.ChangePropValue(Cell: TGridCell; var Value: string;
  var Accept: Boolean);
var Prop: TCustomProp;
//    CanModify: boolean;
begin
 Accept := false;
 if not Assigned(FFlexControl) or grInspector.IsCellReadOnly(Cell) then exit;
 // grInspectorEditCanModify(grInspector, Cell, CanModify);
 Prop := Nil;
 try
  Prop := FFlexControl.Props.VisibleProps[Cell.Row];
  if psReadOnly in Prop.Style then exit;
  if Prop.DisplayValue <> Value then begin
   FActiveFlex.History.BeginPanelGroup(TPanelPropsHistoryGroup);
   try
    Prop.DisplayValue := Value;
    Accept := true;
   finally
    FActiveFlex.History.EndPanelGroup(TPanelPropsHistoryGroup);
   end;
  end else
   exit;
 except
  Application.HandleException(Self);
 end;
 if Assigned(Prop) then
 try
  Value := Prop.DisplayValue;
 except
  Value := '';
 end;
end;

procedure TfmInspector.grInspectorEditCloseUp(Sender: TObject;
  Cell: TGridCell; ItemIndex: Integer; var Accept: Boolean);
var Prop: TCustomProp;
    Value: string;
begin
 if not Assigned(FFlexControl) or (ItemIndex < 0) then exit;
 Prop := FFlexControl.Props.VisibleProps[Cell.Row];
 Value := TGridListBox(grInspector.Edit.ActiveList).Items[ItemIndex];
 ChangePropValue(Cell, Value, Accept);
 UpdateProps(Prop);
 // Accept := true;
end;

procedure TfmInspector.grInspectorEditButtonPress(Sender: TObject;
  Cell: TGridCell);
begin
 if not Assigned(FFlexControl) then exit;
 FFlexControl.Props.VisibleProps[Cell.Row].Edit;
end;

procedure TfmInspector.grInspectorSetEditText(Sender: TObject;
  Cell: TGridCell; var Value: String);
var Accept: boolean;
begin
 ChangePropValue(Cell, Value, Accept);
end;

procedure TfmInspector.grInspectorGetEditList(Sender: TObject;
  Cell: TGridCell; Items: TStrings);
begin
 if (Cell.Col <> 1) or not Assigned(FFlexControl) then exit;
 FFlexControl.Props.VisibleProps[Cell.Row].GetEnumList(Items);
end;

procedure TfmInspector.grInspectorEditCanModify(Sender: TObject;
  Cell: TGridCell; var CanModify: Boolean);
begin
 if (Cell.Col <> 1) or not Assigned(FFlexControl) then
  CanModify := False
 else
 with FFlexControl.Props.VisibleProps[Cell.Row] do
  CanModify :=
    not (psReadOnly in Style) and
    ( not (psEditForm in Style) and not IsEnum or (psDisplayEdit in Style) );
{  CanModify := not (ReadOnly or IsEnum or IsFormEdit) or IsDisplayEditEnabled; }
end;

procedure TfmInspector.grInspectorChange(Sender: TObject; Cell: TGridCell;
  Selected: Boolean);
begin
 if Assigned(FFlexControl) and Selected then
  FLastPropName := FFlexControl.Props.VisiblePropNames[Cell.Row];
end;

procedure TfmInspector.grInspectorDblClick(Sender: TObject);
var Items: TStrings;
    Prop: TCustomProp;
    NewIndex: integer;
    Value: string;
    Accept: boolean;
begin
 if not Assigned(FFlexControl) then exit;
 Prop := FFlexControl.Props.VisibleProps[grInspector.Row];
 if Prop is TEnumProp then begin
  with TEnumProp(Prop) do
   if EnumIndex = EnumCount - 1
    then NewIndex := 0
    else NewIndex := EnumIndex + 1;
  Items := TStringList.Create;
  try
   TEnumProp(Prop).GetEnumList(Items);
   Value := Items[NewIndex];
  finally
   Items.Free;
  end;
  ChangePropValue(grInspector.EditCell, Value, Accept);
 end;
end;

function TfmInspector.GetControlIconIndex(AControl: TFlexControl): integer;
var CType: TClass;
begin
 Result := -1;
 if not Assigned(AControl) then exit;
 CType := AControl.ClassType;
 if CType = TFlexScheme         then Result := 0 else
 if CType = TFlexLayer          then Result := 1 else
 if CType = TFlexBox            then Result := 2 else
 if CType = TFlexEllipse        then Result := 3 else
 if CType = TFlexCurve          then Result := 4 else
 if CType = TFlexRegularPolygon then Result := 4 else
 if CType = TFlexText           then Result := 5 else
 if CType = TFlexPicture        then Result := 6 else
 if CType = TFlexGroup          then Result := 7 else
 if CType = TFlexConnector      then Result := 9
                                else Result := 8;
end;

procedure TfmInspector.cbInspItemsDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var s: string;
    Idx: integer;
    FlexControl: TFlexControl;
    Org, Cli: TPoint;
begin
 with cbInspItems do begin
  ACanvas.Canvas.FillRect(ARect);
  FlexControl := TFlexControl(Properties.Items.Objects[AIndex]);
  Idx := GetControlIconIndex(FlexControl);
  if Idx >= 0 then begin
   imgCtrlIcons.Draw(ACanvas.Canvas, ARect.Left+2, ARect.Top+2, Idx);
  end;
  s := Properties.Items[AIndex];
  ACanvas.Canvas.TextOut(ARect.Left+22, ARect.Top +
                ((ARect.Bottom - ARect.Top) - ACanvas.Canvas.TextHeight(s)) div 2, s);
  GetDCOrgEx(ACanvas.Canvas.Handle, Org);
  Cli := ClientToScreen(Point(Top, Left));
  if (AIndex < Properties.Items.Count-1) and (Org.Y <> Cli.Y) and
     ((FlexControl is TFlexScheme) or (FlexControl is TFlexLayer)) then begin
   FlexControl := TFlexControl(Properties.Items.Objects[AIndex+1]);
   if not (FlexControl is TFlexScheme) and
      not (FlexControl is TFlexLayer) then
    with ACanvas.Canvas do begin
     Pen.Color := clBlack;
     Pen.Style := psSolid;
     Pen.Mode := pmCopy;
     MoveTo(ARect.Left, ARect.Bottom-1);
     LineTo(ARect.Right, ARect.Bottom-1);
    end;
  end;
 end;
end;

procedure TfmInspector.cbInspItemsChange(Sender: TObject);
var AControl: TFlexControl;
begin
 if not Assigned(FActiveFlex) then exit;
 AControl := TFlexControl(cbInspItems.Properties.Items.Objects[cbInspItems.ItemIndex]);
 with FActiveFlex do begin
  UnselectAll;
  Select(AControl);
 end;
 Control := AControl;
end;

end.
