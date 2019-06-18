unit fLibItemProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, RXSpin, FlexLibs, FlexUtils, Vcl.Mask,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, Vcl.Menus;

type
  TfmLibItemProps = class(TForm)
    Panel1: TcxGroupBox;
    Panel2: TcxGroupBox;
    imgItemView: TcxImage;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    Label1: TcxLabel;
    edTitle: TcxTextEdit;
    Label2: TcxLabel;
    edDesc: TcxTextEdit;
    chAnchor: TcxCheckBox;
    lblAnchorX: TcxLabel;
    lblAnchorY: TcxLabel;
    sedAnchorX: TcxSpinEdit;
    sedAnchorY: TcxSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sedAnchorChange(Sender: TObject);
    procedure chAnchorClick(Sender: TObject);
    procedure imgItemViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure imgItemViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FLibItem: TFlexLibItem;
    FSurface: TBitmap;
    FScale: integer;
    FOrigin: TPoint;
    procedure CheckTools;
    procedure SetLibItem(const Value: TFlexLibItem);
    procedure AnchorByViewPoint(X, Y: integer);
    procedure RefreshItemView;
  public
    { Public declarations }
    property  LibItem: TFlexLibItem read FLibItem write SetLibItem;
  end;

var
  fmLibItemProps: TfmLibItemProps;

implementation

{$R *.DFM}

procedure TfmLibItemProps.FormCreate(Sender: TObject);
begin
 FSurface := TBitmap.Create;
 CheckTools;
end;

procedure TfmLibItemProps.FormDestroy(Sender: TObject);
begin
  FSurface.Free;
end;

procedure TfmLibItemProps.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if ModalResult = mrOk then begin
  FLibItem.Name := edTitle.Text;
  FLibItem.Hint := edDesc.Text;
  FLibItem.AnchorProp.Enabled := chAnchor.Checked;
  FLibItem.AnchorProp.PosX := Round(sedAnchorX.Value * PixelScaleFactor);
  FLibItem.AnchorProp.PosY := Round(sedAnchorY.Value * PixelScaleFactor);
 end;
end;

procedure TfmLibItemProps.sedAnchorChange(Sender: TObject);
begin
 RefreshItemView;
end;

procedure TfmLibItemProps.chAnchorClick(Sender: TObject);
begin
 CheckTools;
 RefreshItemView;
end;

procedure TfmLibItemProps.CheckTools;
begin
 lblAnchorX.Enabled := chAnchor.Checked;
 sedAnchorX.Enabled := chAnchor.Checked;
 lblAnchorY.Enabled := chAnchor.Checked;
 sedAnchorY.Enabled := chAnchor.Checked;
end;

procedure TfmLibItemProps.SetLibItem(const Value: TFlexLibItem);
var CoeffX, CoeffY: double;
begin
 if Value = FLibItem then exit;
 FLibItem := Value;
 if Assigned(FLibItem) then begin
  // Setup controls
  edTitle.Text := FLibItem.Name;
  edDesc.Text := FLibItem.Hint;
  chAnchor.Checked := FLibItem.AnchorProp.Enabled;
  sedAnchorX.Value := FLibItem.AnchorProp.PosX / PixelScaleFactor;
  sedAnchorY.Value := FLibItem.AnchorProp.PosY / PixelScaleFactor;
  // Calc scaling factor
  with imgItemView do begin
   CoeffX := ((Width - 20)* PixelScaleFactor) / FLibItem.Width;
   CoeffY := ((Height -20)* PixelScaleFactor) / FLibItem.Height;
   if CoeffX < CoeffY
    then FScale := Round(100*CoeffX)
    else FScale := Round(100*CoeffY);
   // Calc preview origin
   FOrigin.X := -(Width - ScaleValue(FLibItem.Width, FScale)) div 2;
   FOrigin.Y := -(Height - ScaleValue(FLibItem.Height, FScale)) div 2;
  end;
 end;
 RefreshItemView;
end;

procedure TfmLibItemProps.RefreshItemView;
var Anchor: TPoint;
begin
 with FSurface.Canvas do begin
  FSurface.Width := imgItemView.Width;
  FSurface.Height := imgItemView.Height;
  // Fill background
  Brush.Color := clWindow;
  Brush.Style := bsSolid;
  FillRect(Rect(0, 0, Width, Height));
  if not Assigned(LibItem) then exit;
  LibItem.Owner.PaintTo(FSurface.Canvas, Rect(0, 0, Width, Height),
    FOrigin, FScale, FLibItem, True, True, False, False);
  // Paint anchor point (as cross)
  if chAnchor.Checked then begin
   Anchor.X := Round(sedAnchorX.Value * FScale / 100) - FOrigin.X;
   Anchor.Y := Round(sedAnchorY.Value * FScale / 100) - FOrigin.Y;
   Pen.Color := clSilver;
   Pen.Style := psSolid;
   Pen.Width := 1;
   with Anchor do begin
    MoveTo(0, Y);
    LineTo(Width, Y);
    MoveTo(X, 0);
    LineTo(X, Height);
   end;
  end;
 end;
 // Paint on imgItemView
 imgItemView.Canvas.Draw(0, 0, FSurface);
end;

procedure TfmLibItemProps.imgItemViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if ssLeft in Shift then AnchorByViewPoint(X, Y);
end;

procedure TfmLibItemProps.imgItemViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then AnchorByViewPoint(X, Y);
end;

procedure TfmLibItemProps.AnchorByViewPoint(X, Y: integer);
begin
 if chAnchor.Checked then begin
  sedAnchorX.Value := (FOrigin.X + X) * 100 / FScale;
  sedAnchorY.Value := (FOrigin.Y + Y) * 100 / FScale;
 end;
end;

end.
