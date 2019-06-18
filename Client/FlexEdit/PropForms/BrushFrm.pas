unit BrushFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ExtDlgs, Mask, FlexProps,
  RxToolEdit, RxCombos, ColorComboEdit,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, Vcl.Menus;

type
  TBrushPropForm = class(TForm)
    GroupBox1: TcxGroupBox;
    cbGradStyle: TcxComboBox;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    rbStandard: TcxRadioButton;
    rbGradient: TcxRadioButton;
    rbBitmap: TcxRadioButton;
    GroupBox2: TcxGroupBox;
    Label1: TcxLabel;
    cbBrushStyles: TcxComboBox;
    Label2: TcxLabel;
    GroupBox3: TcxGroupBox;
    Panel1: TcxGroupBox;
    imgBitmap: TcxImage;
    bbLoad: TcxButton;
    bbSave: TcxButton;
    bbClear: TcxButton;
    opd_Bitmap: TOpenPictureDialog;
    spd_Bitmap: TSavePictureDialog;
    chMasked: TcxCheckBox;
    chPaintCache: TcxCheckBox;
    cbDisplay: TcxComboBox;
    Label3: TcxLabel;
    chLinked: TcxCheckBox;
    ccbGradBegin: TcxColorComboBox;
    ccbGradEnd: TcxColorComboBox;
    ccbBrushColor: TcxColorComboBox;
    ccbMaskColor: TcxColorComboBox;
    edLinked: TcxButtonEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbLoadClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure bbClearClick(Sender: TObject);
    procedure edLinkedAfterDialog(Sender: TObject; AButtonIndex: Integer);
    procedure edLinkedChange(Sender: TObject);
    procedure edLinkedExit(Sender: TObject);
    procedure edLinkedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chLinkedClick(Sender: TObject);
    procedure cbBrushStylesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
  private
    { Private declarations }
    FBrushProp: TBrushProp;
    FBrushBmp: TBitmap;
    FChanging: boolean;
    FIsDialogChange: boolean;
    FLastLinkName: string;
    procedure CheckTools;
    procedure BitmapChange(Sender: TObject);
    function  IsLinked: boolean;
    procedure UpdateLink;
  public
    { Public declarations }
  end;

var
  BrushPropForm: TBrushPropForm;

implementation

{$R *.DFM}

uses
  FlexUtils;

procedure TBrushPropForm.FormCreate(Sender: TObject);
var i: integer;
begin
 FBrushBmp := TBitmap.Create;
 FBrushBmp.OnChange := BitmapChange;
 for i:=0 to integer(High(TBrushStyle)) do
  case TBrushStyle(i) of
   bsSolid        : cbBrushStyles.Properties.Items.Add('Solid');
   bsClear        : cbBrushStyles.Properties.Items.Add('Clear');
   bsHorizontal   : cbBrushStyles.Properties.Items.Add('Horizontal');
   bsVertical     : cbBrushStyles.Properties.Items.Add('Vertical');
   bsFDiagonal    : cbBrushStyles.Properties.Items.Add('FDiagonal');
   bsBDiagonal    : cbBrushStyles.Properties.Items.Add('BDiagonal');
   bsCross        : cbBrushStyles.Properties.Items.Add('Cross');
   bsDiagCross    : cbBrushStyles.Properties.Items.Add('DiagCross');
   else             cbBrushStyles.Properties.Items.Add('???');
  end;
 for i:=0 to integer(High(TBitmapDisplay)) do
  case TBitmapDisplay(i) of
   bdCenter       : cbDisplay.Properties.Items.Add('Center');
   bdTile         : cbDisplay.Properties.Items.Add('Tile');
   bdStretch      : cbDisplay.Properties.Items.Add('Stretch');
   else             cbDisplay.Properties.Items.Add('???');
  end;
 CheckTools;
end;

procedure TBrushPropForm.FormDestroy(Sender: TObject);
begin
 FBrushBmp.Free;
end;

procedure TBrushPropForm.FormShow(Sender: TObject);
begin
 if (Tag <> 0) and (TObject(Tag) is TBrushProp) then
  FBrushProp := TBrushProp(Tag);
 if Assigned(FBrushProp) then with FBrushProp do begin
  // Method
  case Method of
   bmHatch    : rbStandard.Checked := true;
   bmGradient : rbGradient.Checked := true;
   bmBitmap   : rbBitmap.Checked := true;
  end;
  // Hatch
  ccbBrushColor.ColorValue := Color;
  cbBrushStyles.ItemIndex := integer(Style);
  // Grad
  cbGradStyle.ItemIndex := integer(GradStyle);
  ccbGradBegin.ColorValue := GradBeginColor;
  ccbGradEnd.ColorValue := GradEndColor;
  // Bitmap
  chMasked.Checked := BitmapMasked;
  ccbMaskColor.ColorValue := BitmapMaskColor;
  chPaintCache.Checked := BitmapCache;
  cbDisplay.ItemIndex := integer(BitmapDisplay);
  edLinked.Text := FBrushProp.BitmapLinkName;
  chLinked.Checked := FBrushProp.BitmapLinkName <> '';
  if IsLinked then
   UpdateLink
  else
  if Assigned(Bitmap) then FBrushBmp.Assign(Bitmap);
 end;
end;

procedure TBrushPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
var Recording: boolean;
begin
 if (ModalResult <> mrOk) or not Assigned(FBrushProp) then exit;
 with FBrushProp do begin
  Recording := Assigned(Owner.History) and
    Assigned(Owner.History.BeginAction(TPropHistoryGroup, FBrushProp));
  try
   // Method
   if rbStandard.Checked then
    Method := bmHatch
   else
   if rbGradient.Checked then
    Method := bmGradient
   else
   if rbBitmap.Checked then
    Method := bmBitmap;
   // Hatch
   Color := ccbBrushColor.ColorValue;
   Style := TBrushStyle(cbBrushStyles.ItemIndex);
   // Graident
   GradStyle := TGradientStyle(cbGradStyle.ItemIndex);
   GradBeginColor := ccbGradBegin.ColorValue;
   GradEndColor := ccbGradEnd.ColorValue;
   // Bitmap
   if IsLinked then
    FBrushProp.BitmapLinkName := edLinked.Text
   else begin
    FBrushProp.BitmapLinkName := '';
    if not FBrushBmp.Empty then begin
     Bitmap := FBrushBmp;
     //FBrushBmp := Nil;
    end else
     Bitmap := Nil;
   end;
   BitmapMasked := chMasked.Checked;
   BitmapMaskColor := ccbMaskColor.ColorValue;
   BitmapCache := chPaintCache.Checked;
   BitmapDisplay := TBitmapDisplay(cbDisplay.ItemIndex);
  finally
   if Recording then Owner.History.EndAction;
  end;
 end;
end;

procedure TBrushPropForm.cbBrushStylesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var R, R2: TRect;
begin
 with AControl as TcxComboBox do begin
  with ACanvas.Canvas do begin
   Brush.Style := bsSolid;
   FillRect(ARect);
   R := Classes.Rect(ARect.Left+3, ARect.Top+3, ARect.Left + 43, ARect.Bottom - 3);
   Brush.Style := TBrushStyle(AIndex);
   if odSelected in AState then begin
    case Brush.Style of
     bsSolid: Brush.Color := clBlack;
     bsClear: Brush.Color := clHighlight;
     else Brush.Color := clWhite;
    end;
    Pen.Color := clWhite;
   end else begin
    if Brush.Style = bsClear
     then Brush.Color := clWhite
     else Brush.Color := clBlack;
    Pen.Color := clBlack;
   end;
   InflateRect(R, 1, 1);
   Rectangle(R.Left, R.Top, R.Right, R.Bottom);
   Brush.Style := bsClear;
   R2 := ARect;
   OffsetRect(R2, 52, 0);
   R.Top := R2.Top + ((R2.Bottom - R2.Top) - TextHeight(Properties.Items[AIndex])) div 2;
   TextOut(R2.Left, R.Top, Properties.Items[AIndex]);
  end;
 end;
end;

procedure TBrushPropForm.bbLoadClick(Sender: TObject);
begin
 if opd_Bitmap.Execute then
  FBrushBmp.LoadFromFile(opd_Bitmap.FileName);
 CheckTools;
end;

procedure TBrushPropForm.bbSaveClick(Sender: TObject);
begin
 if spd_Bitmap.Execute then
  FBrushBmp.SaveToFile(spd_Bitmap.FileName);
 CheckTools;
end;

procedure TBrushPropForm.bbClearClick(Sender: TObject);
begin
 DeleteObject(FBrushBmp.ReleaseHandle);
 BitmapChange(FBrushBmp);
 if IsLinked then edLinked.Text := '';
 CheckTools; 
end;

procedure TBrushPropForm.BitmapChange(Sender: TObject);
var Dest: TRect;
    CX, CY: Double;
    Ofs: TPoint;
begin
 if FChanging then exit;
 FChanging := true;
 try
  if not FBrushBmp.Empty then begin
   if (FBrushBmp.Width <= imgBitmap.Width) and
      (FBrushBmp.Height <= imgBitmap.Height) then begin
    imgBitmap.Picture.Graphic := FBrushBmp;
   end else begin
    imgBitmap.Picture.Graphic := Nil;
    Dest := Rect(0, 0, FBrushBmp.Width, FBrushBmp.Height);
    if Dest.Right > imgBitmap.Width
     then CX := imgBitmap.Width / Dest.Right
     else CX := 1;
    if Dest.Bottom > imgBitmap.Height
     then CY := imgBitmap.Height / Dest.Bottom
     else CY := 1;
    if CY < CX then CX := CY;
    Dest.Right  := Round(Dest.Right  * CX);
    Dest.Bottom := Round(Dest.Bottom * CX);
    Ofs.X := (imgBitmap.Width - Dest.Right) div 2;
    Ofs.Y := (imgBitmap.Height - Dest.Bottom) div 2;
    OffsetRect(Dest, Ofs.X, Ofs.Y);
    imgBitmap.Canvas.StretchDraw(Dest, FBrushBmp);
   end;
  end else
   imgBitmap.Picture.Graphic := Nil;
 finally
  FChanging := False;
 end;
end;

function TBrushPropForm.IsLinked: boolean;
begin
 Result := chLinked.Checked;
end;

procedure TBrushPropForm.CheckTools;
begin
 edLinked.Enabled := IsLinked;
 bbLoad.Enabled := not IsLinked;
 bbSave.Enabled := not IsLinked and not FBrushBmp.Empty;
end;

procedure TBrushPropForm.chLinkedClick(Sender: TObject);
begin
 CheckTools;
 if edLinked.Enabled
  then edLinked.Text := FBrushProp.BitmapLinkName
  else edLinked.Text := '';
end;

procedure TBrushPropForm.UpdateLink;
var LastDir: string;
begin
 if edLinked.Text = FLastLinkName then exit;
 FLastLinkName := edLinked.Text;
 if Assigned(ResolveBitmapLink) then
  ResolveBitmapLink(FBrushProp, FLastLinkName, FBrushBmp)
 else begin
  LastDir := GetCurrentDir;
  try
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
   FBrushBmp.LoadFromFile(ExpandFilename(FLastLinkName));
  finally
   SetCurrentDir(LastDir);
  end;
 end;
 ccbMaskColor.ColorValue :=
  FBrushBmp.Canvas.Pixels[0, FBrushBmp.Height-1];
end;

procedure TBrushPropForm.edLinkedAfterDialog(Sender: TObject; AButtonIndex: Integer);
begin
 if opd_Bitmap.Execute() then
 begin
  FIsDialogChange := true;
  edLinked.Text := ExtractRelativePath( ExtractFilePath(ParamStr(0)), opd_Bitmap.FileName );
 end;
end;

procedure TBrushPropForm.edLinkedChange(Sender: TObject);
var s: string;
begin
 if not FIsDialogChange then exit;
 s := edLinked.Text;
 if s[1] = '"' then begin
  s := copy(s, 2, Length(s)-2);
  edLinked.Text := s;
  exit;
 end;
 FIsDialogChange := false;
 UpdateLink;
end;

procedure TBrushPropForm.edLinkedExit(Sender: TObject);
begin
 UpdateLink;
end;

procedure TBrushPropForm.edLinkedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_RETURN then UpdateLink;
end;

initialization
  RegisterDefaultPropEditForm(TBrushProp, TBrushPropForm);

end.
