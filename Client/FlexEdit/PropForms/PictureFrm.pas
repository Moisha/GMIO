unit PictureFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Mask, StdCtrls, ExtCtrls, Buttons, ExtDlgs, FlexProps,
  RxVCLUtils, RxToolEdit, RxCombos, ColorComboEdit, RXSpin,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, Vcl.Menus;

type
  TPicturePropForm = class(TForm)
    Panel1: TcxGroupBox;
    bbLoad: TcxButton;
    bbSave: TcxButton;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    Panel2: TcxGroupBox;
    labCols: TcxLabel;
    labRows: TcxLabel;
    chMask: TcxCheckBox;
    opd_Load: TOpenPictureDialog;
    spd_Save: TSavePictureDialog;
    Panel3: TcxGroupBox;
    imgPreview: TcxImage;
    bbClear: TcxButton;
    Panel4: TcxGroupBox;
    chLinked: TcxCheckBox;
    sedCols: TcxSpinEdit;
    sedRows: TcxSpinEdit;
    cceMaskColor: TcxColorComboBox;
    edLinked: TcxButtonEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bbSaveClick(Sender: TObject);
    procedure bbLoadClick(Sender: TObject);
    procedure bbClearClick(Sender: TObject);
    procedure chMaskClick(Sender: TObject);
    procedure chLinkedClick(Sender: TObject);
    procedure edLinkedAfterDialog(Sender: TObject; AButtonIndex: Integer);
    procedure edLinkedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edLinkedExit(Sender: TObject);
    procedure edLinkedChange(Sender: TObject);
  private
    { Private declarations }
    FPictureProp: TPictureProp;
    FPicture: TPicture;
    FChanging: boolean;
    FIsDialogChange: boolean;
    FLastLinkName: string;
    procedure CheckTools;
    procedure PictureChange(Sender: TObject);
    procedure UpdateLink;
    function  IsLinked: boolean;
  public
    { Public declarations }
  end;

var
  PicturePropForm: TPicturePropForm;

implementation

{$R *.DFM}

procedure TPicturePropForm.FormCreate(Sender: TObject);
begin
 FPicture := TPicture.Create;
 FPicture.OnChange := PictureChange;
end;

procedure TPicturePropForm.FormDestroy(Sender: TObject);
begin
 FPicture.Free;
end;

procedure TPicturePropForm.FormShow(Sender: TObject);
begin
 if (Tag <> 0) and (TObject(Tag) is TPictureProp) then
  FPictureProp := TPictureProp(Tag);
 CheckTools;
 if Assigned(FPictureProp) then begin
  sedCols.Value := FPictureProp.Columns;
  sedRows.Value := FPictureProp.Rows;
  chMask.Checked := FPictureProp.Masked;
  cceMaskColor.ColorValue := ColorToRGB(FPictureProp.MaskColor);
  edLinked.Text := FPictureProp.LinkName;
  chLinked.Checked := FPictureProp.LinkName <> '';
  if IsLinked
   then UpdateLink
   else FPicture.Graphic := FPictureProp.Graphic;
 end;
end;

procedure TPicturePropForm.FormClose(Sender: TObject; var Action: TCloseAction);
var Recording: boolean;
begin
 if (ModalResult <> mrOk) or not Assigned(FPictureProp) then exit;
 Recording :=
   Assigned(FPictureProp.Owner.History) and
   Assigned(
     FPictureProp.Owner.History.BeginAction(TPropHistoryGroup, FPictureProp));
 try
  if IsLinked
   then FPictureProp.LinkName := edLinked.Text
   else FPictureProp.Graphic := FPicture.Graphic;
  FPictureProp.Columns := Trunc(sedCols.Value);
  FPictureProp.Rows := Trunc(sedRows.Value);
  FPictureProp.Masked := chMask.Checked;
  FPictureProp.MaskColor := cceMaskColor.ColorValue;
 finally
  if Recording then FPictureProp.Owner.History.EndAction;
 end;
end;

procedure TPicturePropForm.CheckTools;
begin
 edLinked.Enabled := IsLinked;
 bbLoad.Enabled := not IsLinked;
 bbSave.Enabled := not IsLinked;
end;

function TPicturePropForm.IsLinked: boolean;
begin
 Result := chLinked.Checked;
end;

procedure TPicturePropForm.bbLoadClick(Sender: TObject);
begin
 if not opd_Load.Execute then exit;
 FPicture.LoadFromFile(opd_Load.FileName);
 if Assigned(FPicture.Graphic) and (FPicture.Graphic is TBitmap) then
  cceMaskColor.ColorValue :=
   FPicture.Bitmap.Canvas.Pixels[0, FPicture.Bitmap.Height-1];
end;

procedure TPicturePropForm.bbSaveClick(Sender: TObject);
begin
 spd_Save.Filter := GraphicFilter(TGraphicClass(FPicture.Graphic.ClassType));
 spd_Save.DefaultExt :=
   GraphicExtension(TGraphicClass(FPicture.Graphic.ClassType));
 if not spd_Save.Execute then exit;
 FPicture.SaveToFile(spd_Save.Filename);
end;

procedure TPicturePropForm.UpdateLink;
var LastDir: string;
begin
 if edLinked.Text = FLastLinkName then exit;
 FLastLinkName := edLinked.Text;
 if Assigned(ResolvePictureLink) then
  ResolvePictureLink(FPictureProp, FLastLinkName, FPicture)
 else begin
  LastDir := GetCurrentDir;
  try
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
   FPicture.LoadFromFile(ExpandFilename(FLastLinkName));
  finally
   SetCurrentDir(LastDir);
  end;
 end;
 if Assigned(FPicture.Graphic) and (FPicture.Graphic is TBitmap) then
   cceMaskColor.ColorValue :=
    FPicture.Bitmap.Canvas.Pixels[0, FPicture.Bitmap.Height-1];
end;

procedure TPicturePropForm.PictureChange(Sender: TObject);
var IsRaster: boolean;
    Dest: TRect;
    CX, CY: Double;
    Ofs: TPoint;
begin
 if FChanging then exit;
 FChanging := true;
 try
  IsRaster := Assigned(FPicture.Graphic) and (FPicture.Graphic is TBitmap);
  sedCols.Enabled := IsRaster;
  sedRows.Enabled := IsRaster;
  if not IsRaster then begin
   //chMask.Checked := False;
   sedCols.Value := 1;
   sedRows.Value := 1;
  end;
  chMask.Enabled := Assigned(FPicture.Graphic); //IsRaster;
  if IsRaster
   then chMask.Caption := 'Mask color'
   else chMask.Caption := 'Transparent';
  cceMaskColor.Enabled := IsRaster; //chMask.Checked;
  if Assigned(FPicture.Graphic) then begin
   if (FPicture.Graphic.Width <= imgPreview.Width) and
      (FPicture.Graphic.Height <= imgPreview.Height) then begin
    imgPreview.Picture.Graphic := FPicture.Graphic;
   end else begin
    imgPreview.Picture.Graphic := Nil;
    Dest := Rect(0, 0, FPicture.Graphic.Width, FPicture.Graphic.Height);
    if Dest.Right > imgPreview.Width
     then CX := imgPreview.Width / Dest.Right
     else CX := 1;
    if Dest.Bottom > imgPreview.Height
     then CY := imgPreview.Height / Dest.Bottom
     else CY := 1;
    if CY < CX then CX := CY;
    Dest.Right  := Round(Dest.Right  * CX);
    Dest.Bottom := Round(Dest.Bottom * CX);
    Ofs.X := (imgPreview.Width - Dest.Right) div 2;
    Ofs.Y := (imgPreview.Height - Dest.Bottom) div 2;
    OffsetRect(Dest, Ofs.X, Ofs.Y);
    imgPreview.Canvas.StretchDraw(Dest, FPicture.Graphic);
   end;
  end else
   imgPreview.Picture.Graphic := Nil;
 finally
  FChanging := False;
 end;
end;

procedure TPicturePropForm.bbClearClick(Sender: TObject);
begin
 if IsLinked then edLinked.Text := '';
 FPicture.Graphic := Nil;
end;

procedure TPicturePropForm.chMaskClick(Sender: TObject);
begin
 cceMaskColor.Enabled := chMask.Checked;
end;

procedure TPicturePropForm.chLinkedClick(Sender: TObject);
begin
 CheckTools;
 if edLinked.Enabled
  then edLinked.Text := FPictureProp.LinkName
  else edLinked.Text := '';
end;

procedure TPicturePropForm.edLinkedAfterDialog(Sender: TObject; AButtonIndex: Integer);
begin
 if opd_Load.Execute() then
 begin
  FIsDialogChange := true;
  edLinked.Text := ExtractRelativePath( ExtractFilePath(ParamStr(0)), opd_Load.FileName );
 end;
end;

procedure TPicturePropForm.edLinkedChange(Sender: TObject);
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

procedure TPicturePropForm.edLinkedExit(Sender: TObject);
begin
 UpdateLink;
end;

procedure TPicturePropForm.edLinkedKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_RETURN then UpdateLink;
end;

initialization
  RegisterDefaultPropEditForm(TPictureProp, TPicturePropForm);

end.
