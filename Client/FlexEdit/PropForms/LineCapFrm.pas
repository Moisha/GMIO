unit LineCapFrm;

interface

uses
  Windows, Types, UITypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, FlexProps, FlexUtils, FlexPath, FlexControls,
  RXSpin, RxCombos, ColorComboEdit, Vcl.Mask, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, dxSkinsCore, Vcl.Menus, cxButtons, cxCheckBox, cxSpinEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxLabel, cxGroupBox, dxSkinOffice2010Silver,
  cxColorComboBox;

type
  TLineCapPropForm = class(TForm)
    Panel1: TcxGroupBox;
    Label1: TcxLabel;
    cbLineCapStyles: TcxComboBox;
    chFixedSize: TcxCheckBox;
    chFixedOutline: TcxCheckBox;
    chFixedFill: TcxCheckBox;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    ccbOutlineColor: TcxColorComboBox;
    ccbFillColor: TcxColorComboBox;
    sedSize: TcxSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chCheckClick(Sender: TObject);
    procedure cbLineCapStylesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
  private
    { Private declarations }
    FLineCapProp: TLineCapProp;
  public
    { Public declarations }
  end;

var
  LineCapPropForm: TLineCapPropForm;

implementation

{$R *.DFM}


procedure TLineCapPropForm.FormCreate(Sender: TObject);
var i: integer;
begin
 // Fill cap styles
 for i:=0 downto psLastStandard do cbLineCapStyles.Properties.Items.Add('');
end;

procedure TLineCapPropForm.FormShow(Sender: TObject);
begin
 if (Tag <> 0) and (TObject(Tag) is TLineCapProp) then
  FLineCapProp := TLineCapProp(Tag);
 if Assigned(FLineCapProp) then with FLineCapProp do begin
  cbLineCapStyles.ItemIndex := -CapStyle;
  // Size
  chFixedSize.Checked := FixedSize;
  sedSize.Value := Size / PixelScaleFactor;
  sedSize.Enabled := FixedSize;
  // Outline
  chFixedOutline.Checked := FixedOutlineColor;
  ccbOutlineColor.ColorValue := OutlineColor;
  ccbOutlineColor.Enabled := FixedOutlineColor;
  // Fill
  chFixedFill.Checked := FixedFillColor;
  ccbFillColor.ColorValue := FillColor;
  ccbFillColor.Enabled := FixedFillColor;
 end;
end;

procedure TLineCapPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
var Recording: boolean;
begin
 if (ModalResult <> mrOk) or not Assigned(FLineCapProp) then exit;
 with FLineCapProp do begin
  Recording := Assigned(Owner.History) and
    Assigned(Owner.History.BeginAction(TPropHistoryGroup, FLineCapProp));
  try
   CapStyle := -cbLineCapStyles.ItemIndex;
   FixedSize := chFixedSize.Checked;
   Size := Round(sedSize.Value * PixelScaleFactor);
   FixedOutlineColor := chFixedOutline.Checked;
   OutlineColor := ccbOutlineColor.ColorValue;
   FixedFillColor := chFixedFill.Checked;
   FillColor := ccbFillColor.ColorValue;
  finally
   if Recording then Owner.History.EndAction;
  end;
 end;
end;

procedure TLineCapPropForm.chCheckClick(Sender: TObject);
begin
 sedSize.Enabled := chFixedSize.Checked;
 ccbOutlineColor.Enabled := chFixedOutline.Checked;
 ccbFillColor.Enabled := chFixedFill.Checked;
end;

procedure TLineCapPropForm.cbLineCapStylesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var R: TRect;
    Y: integer;
    p0, p1: TPoint;
    CapStyle: integer;
    Info: TLineCapInfo;
begin
 with AControl as TcxComboBox, ACanvas.Canvas do begin
  Brush.Style := bsSolid;
  FillRect(ARect);
  //R := Classes.Rect(Rect.Left+3, Rect.Top+3, Rect.Left + 38, Rect.Bottom - 3);
  //R := Rect;
  //InflateRect(R, -3, -3);
  R := Classes.Rect(ARect.Left+6, ARect.Top+3, ARect.Right-6, ARect.Bottom-3);
  if odSelected in AState
   then Pen.Color := clWhite
   else Pen.Color := clBlack;
  Brush.Color := Pen.Color;
  Y := R.Top + (R.Bottom - R.Top) div 2;
  // Define cap style
  CapStyle := -AIndex;
  if GetLineCapInfo(CapStyle, Info, R.Bottom - R.Top) then begin
   // Adjust line size
   dec(R.Right, Info.Bounds.Right);
   // Draw line
   MoveTo(R.Left, Y);
   LineTo(R.Right, Y);
   // Draw cap
   p0.x := R.Left;
   p0.y := Y;
   p1.x := R.Right;
   p1.y := Y;
   RenderCap(Handle, CapStyle, p0, p1, R.Bottom - R.Top);
  end else begin
   // Draw line only
   MoveTo(R.Left, Y);
   LineTo(R.Right, Y);
  end;
 end;
end;

initialization
  RegisterDefaultPropEditForm(TLineCapProp, TLineCapPropForm);

end.
