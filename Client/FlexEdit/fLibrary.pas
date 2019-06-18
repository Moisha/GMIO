unit fLibrary;

interface

uses
  Windows, Types, UITypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ImgList, StdCtrls, ExtCtrls, ComCtrls, ToolWin,
  FlexBase, FlexLibs, System.Actions,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  TfmLibrary = class(TForm)
    tbMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Panel1: TcxGroupBox;
    cbLibs: TcxComboBox;
    ToolButton5: TToolButton;
    imgTools: TImageList;
    alTools: TActionList;
    acLibNew: TAction;
    acLibOpen: TAction;
    acLibSave: TAction;
    acLibRemove: TAction;
    Panel2: TcxGroupBox;
    Panel3: TcxGroupBox;
    edDesc: TcxTextEdit;
    Splitter1: TcxSplitter;
    lbItems: TcxListBox;
    acLibDeleteItem: TAction;
    ToolButton7: TToolButton;
    acLibAddItem: TAction;
    ToolButton6: TToolButton;
    acLibProperties: TAction;
    Panel4: TcxGroupBox;
    pbLibItem: TPaintBox;
    sd_SaveLib: TSaveDialog;
    od_OpenLib: TOpenDialog;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    acLibSaveAll: TAction;
    ToolButton10: TToolButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acLibNewExecute(Sender: TObject);
    procedure acLibSaveExecute(Sender: TObject);
    procedure acLibRemoveExecute(Sender: TObject);
    procedure acLibAddItemExecute(Sender: TObject);
    procedure acLibDeleteItemExecute(Sender: TObject);
    procedure acLibOpenExecute(Sender: TObject);
    procedure acLibPropertiesExecute(Sender: TObject);
    procedure pbLibItemPaint(Sender: TObject);
    procedure pbLibItemStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure lbItemsClick(Sender: TObject);
    procedure cbLibsEnter(Sender: TObject);
    procedure cbLibsExit(Sender: TObject);
    procedure cbLibsChange(Sender: TObject);
  private
    { Private declarations }
    FActiveFlex: TFlexPanel;
    FActiveLibrary: TFlexLibrary;
    FLibItemScale: integer;
    FLibItemOrigin: TPoint;
    FLibPropsFocused: boolean;
    FPropsEdited: boolean;
    FOnLibChange: TNotifyEvent;
    procedure SetActiveLibrary(const Value: TFlexLibrary);
    procedure FlexLibsChange(Sender: TObject);
    procedure UpdateData;
    procedure CheckTools;
    procedure SetActiveFlex(const Value: TFlexPanel);
  public
    { Public declarations }
    property  ActiveFlex: TFlexPanel read FActiveFlex write SetActiveFlex;
    property  ActiveLibrary: TFlexLibrary read FActiveLibrary write
      SetActiveLibrary;
    procedure FlexSelectionChange;
    property  OnLibChange: TNotifyEvent read FOnLibChange write FOnLibChange;
  end;

var
  fmLibrary: TfmLibrary;

implementation

{$R *.DFM}

uses
  ToolMngr, FlexUtils, fLibProps, fLibItemProps;

procedure TfmLibrary.FormCreate(Sender: TObject);
begin
 RegisterToolForm(Self);
 FlexLibraries.OnChange := FlexLibsChange;
 CheckTools;
end;

procedure TfmLibrary.FormDestroy(Sender: TObject);
begin
 UnRegisterToolForm(Self);
 fmLibrary := Nil;
end;

procedure TfmLibrary.FormShow(Sender: TObject);
begin
 // Avoid delphi 4 bug
 cbLibs.Width := ClientWidth;
end;

procedure TfmLibrary.CheckTools;
var LibExists, LibModified: boolean;
    IsSelected, PropAvail: boolean;
begin
 LibExists := Assigned(FActiveLibrary);
 PropAvail := LibExists or (lbItems.ItemIndex >= 0) and not FLibPropsFocused;
 LibModified := LibExists and FActiveLibrary.Modified;
 IsSelected := Assigned(ActiveFlex) and (ActiveFlex.SelectedCount > 0);
 acLibProperties.Enabled := LibExists;
 acLibRemove.Enabled := LibExists;
 acLibSave.Enabled := LibExists and LibModified;
 cbLibs.Enabled := cbLibs.Properties.Items.Count > 0;
 acLibProperties.Enabled := PropAvail;
 acLibAddItem.Enabled := LibExists and IsSelected;
 acLibDeleteItem.Enabled := LibExists and (lbItems.ItemIndex >= 0);
end;

procedure TfmLibrary.UpdateData;
var i: integer;
    LibItem: TFlexLibItem;
begin
 cbLibs.ItemIndex := cbLibs.Properties.Items.IndexOfObject(FActiveLibrary);
 lbItems.Items.BeginUpdate;
 try
  if (lbItems.Items.Count > 0) and (lbItems.ItemIndex >= 0)
   then LibItem := TFlexLibItem(lbItems.Items.Objects[lbItems.ItemIndex])
   else LibItem := Nil;
  lbItems.Clear;
  if Assigned(FActiveLibrary) then with FActiveLibrary do
  for i:=0 to Count-1 do
   lbItems.Items.AddObject(Controls[i].Name, Controls[i]);
  if lbItems.Items.Count > 0 then begin
   i := lbItems.Items.IndexOfObject(LibItem);
   if i < 0 then i := 0;
   lbItems.ItemIndex := i;
   LibItem := TFlexLibItem(lbItems.Items.Objects[lbItems.ItemIndex]);
  end else
   LibItem := Nil;
  if Assigned(LibItem)
   then edDesc.Text := LibItem.Hint
   else edDesc.Text := '';
 finally
  lbItems.Items.EndUpdate;
 end;
 pbLibItem.Invalidate;
end;

procedure TfmLibrary.FlexLibsChange(Sender: TObject);
var i: integer;
begin
 cbLibs.Properties.Items.BeginUpdate;
 try
  cbLibs.Properties.Items.Clear;
  for i:=0 to FlexLibraries.Count-1 do
   cbLibs.Properties.Items.AddObject(FlexLibraries[i].Name, FlexLibraries[i]);
 finally
  cbLibs.Properties.Items.EndUpdate;
 end;
 CheckTools;
end;

procedure TfmLibrary.FlexSelectionChange;
begin
 CheckTools;
end;

procedure TfmLibrary.SetActiveFlex(const Value: TFlexPanel);
begin
 if Value = FActiveFlex then exit;
 FActiveFlex := Value;
 FlexSelectionChange;
end;

procedure TfmLibrary.SetActiveLibrary(const Value: TFlexLibrary);
begin
 if Value = FActiveLibrary then exit;
 FActiveLibrary := Value;
 UpdateData;
 CheckTools;
 if Assigned(FOnLibChange) then FOnLibChange(Self);
end;

procedure TfmLibrary.lbItemsClick(Sender: TObject);
begin
 //pbLibItem.Invalidate;
 UpdateData; 
 CheckTools;
end;

procedure TfmLibrary.pbLibItemPaint(Sender: TObject);
var LibItem: TFlexLibItem;
    CoeffX, CoeffY: double;
    W, H: integer;
    B: TBitmap;
begin
 B := TBitmap.Create;
 with pbLibItem, B.Canvas do
 try
  Brush.Color := clWindow;
  Brush.Style := bsSolid;
  FillRect(Rect(0, 0, Width, Height));
  if (lbItems.Items.Count = 0) or (lbItems.ItemIndex < 0) then exit;
  LibItem := TFlexLibItem( lbItems.Items.Objects[lbItems.ItemIndex] );
  CoeffX := (Width - 20) / ScaleValue(LibItem.Width, 100);
  CoeffY := (Height - 20) / ScaleValue(LibItem.Height, 100);
  if CoeffX < CoeffY
   then FLibItemScale := Round(100*CoeffX)
   else FLibItemScale := Round(100*CoeffY);
  W := ScaleValue(LibItem.Width, FLibItemScale);
  H := ScaleValue(LibItem.Height, FLibItemScale);
  FLibItemOrigin.X := -(Width - W) div 2;
  FLibItemOrigin.Y := -(Height - H) div 2;
  //FLibItemOrigin := Point(-10, -10);
  LibItem.Owner.PaintTo(Canvas, Rect(0, 0, Width, Height),
    FLibItemOrigin, FLibItemScale, LibItem, True, True, False, False);
  pbLibItem.Canvas.Draw(0, 0, B);
 finally
  B.Free;
 end;
end;

procedure TfmLibrary.cbLibsChange(Sender: TObject);
begin
 if cbLibs.ItemIndex >= 0 then
  ActiveLibrary := TFlexLibrary(cbLibs.Properties.Items.Objects[cbLibs.ItemIndex]);
 CheckTools;
end;

procedure TfmLibrary.cbLibsEnter(Sender: TObject);
begin
 FLibPropsFocused := true;
end;

procedure TfmLibrary.cbLibsExit(Sender: TObject);
begin
 FLibPropsFocused := false;
end;

procedure TfmLibrary.pbLibItemStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var LibItem: TFlexLibItem;
    MousePos: TPoint;
begin
 if lbItems.ItemIndex < 0 then exit;
 LibItem := TFlexLibItem( lbItems.Items.Objects[lbItems.ItemIndex] );
 GetCursorPos(MousePos);
 MousePos := (Sender as TControl).ScreenToClient(MousePos);
 DragObject :=
   LibItem.Owner.StartDrag(LibItem, True, False, MousePos,
     FLibItemOrigin, FLibItemScale);
 // Fix drag delta if necessary
 if Assigned(DragObject) and LibItem.AnchorProp.Enabled then
  TFlexDragObject(DragObject).MouseDragDelta := LibItem.AnchorProp.AsPoint;
end;

procedure TfmLibrary.acLibNewExecute(Sender: TObject);
begin
 ActiveLibrary := FlexLibraries.New('');
 FLibPropsFocused := true;
 acLibProperties.Execute;
 if not FActiveLibrary.Modified then
  acLibRemove.Execute
 else
 if FActiveLibrary.LibFilename <> '' then begin
  if FileExists(FActiveLibrary.LibFilename) then begin
   if MessageDlg( Format('File %s already exists. Overwrite?',
       [FActiveLibrary.LibFilename]),
       mtWarning, [mbYes, mbNo], 0) <> mrOk
    then FActiveLibrary.LibFilename := '';
  end;
  if FActiveLibrary.LibFilename <> '' then
    FlexLibraries.SaveLibrary(FActiveLibrary);
 end;
end;

procedure TfmLibrary.acLibSaveExecute(Sender: TObject);
var SaveName: string;
begin
 with FActiveLibrary do begin
  if LibFilename = '' then begin
   sd_SaveLib.FileName := Name;
   if not sd_SaveLib.Execute then exit;
   SaveName := sd_SaveLib.FileName;
   if ExtractFileExt(SaveName) = '' then SaveName := SaveName + '.fxl';
   LibFilename := SaveName;
  end;
  if LibFilename <> '' then FlexLibraries.SaveLibrary(FActiveLibrary);
 end;
 CheckTools;
end;

procedure TfmLibrary.acLibOpenExecute(Sender: TObject);
var Lib: TFlexLibrary;
begin
 if not od_OpenLib.Execute or not FileExists(od_OpenLib.Filename) then exit;
 Lib := FlexLibraries.New(od_OpenLib.Filename);
 if Assigned(Lib) then ActiveLibrary := Lib;
end;

procedure TfmLibrary.acLibRemoveExecute(Sender: TObject);
var Index: integer;
begin
 if not Assigned(FActiveLibrary) then exit;
 Index := cbLibs.ItemIndex;
 FlexLibraries.Remove(FActiveLibrary);
 if FlexLibraries.IndexOf(FActiveLibrary) < 0 then begin
  if Index = cbLibs.Properties.Items.Count then dec(Index);
  if Index >= 0 then
   ActiveLibrary := TFlexLibrary(cbLibs.Properties.Items.Objects[Index])
  else
  if cbLibs.Properties.Items.Count > 0 then
   ActiveLibrary := TFlexLibrary(cbLibs.Properties.Items.Objects[cbLibs.Properties.Items.Count-1])
  else
   ActiveLibrary := Nil;
 end;
end;

procedure TfmLibrary.acLibAddItemExecute(Sender: TObject);
var LibItem: TFlexLibItem;
begin
 if not (Assigned(FActiveFlex) and Assigned(FActiveLibrary)) then exit;
 LibItem := FActiveLibrary.NewFromFlex(FActiveFlex);
 UpdateData;
 lbItems.ItemIndex := lbItems.Items.IndexOfObject(LibItem);
 FLibPropsFocused := false;
 acLibProperties.Execute;
 if not FPropsEdited then
  acLibDeleteItem.Execute
 else begin
  UpdateData;
  CheckTools;
 end;
end;

procedure TfmLibrary.acLibDeleteItemExecute(Sender: TObject);
var LibItem: TFlexLibItem;
begin
 if not Assigned(FActiveLibrary) or (lbItems.ItemIndex < 0) then exit;
 LibItem := TFlexLibItem(lbItems.Items.Objects[lbItems.ItemIndex]);
 FActiveLibrary.Remove(LibItem);
 UpdateData;
 CheckTools;
end;

procedure TfmLibrary.acLibPropertiesExecute(Sender: TObject);
begin
 FPropsEdited := False;
 if not FLibPropsFocused then begin
  // Library item properties
  fmLibItemProps := TfmLibItemProps.Create(Nil);
  with fmLibItemProps do
  try
   LibItem := TFlexLibItem(lbItems.Items.Objects[lbItems.ItemIndex]);
   if ShowModal = mrOk then begin
    FActiveLibrary.Modified := true;
    FPropsEdited := True;
    UpdateData;
   end;
  finally
   fmLibItemProps.Free;
   fmLibItemProps := Nil;
  end;
 end else begin
  // Library properties
  fmLibProps := TfmLibProps.Create(Nil);
  with fmLibProps do
  try
   FlexLibrary := FActiveLibrary;
   if ShowModal = mrOk then begin
    FPropsEdited := True;
    UpdateData;
   end;
  finally
   fmLibProps.Free;
   fmLibProps := Nil;
  end;
 end;
 CheckTools;
end;

end.
