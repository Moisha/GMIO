unit PenFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, FlexProps, FlexUtils,
  RxCombos, ColorComboEdit, RXSpin, Vcl.Mask,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, Vcl.Menus;

type
  TPenPropForm = class(TForm)
    Panel1: TcxGroupBox;
    Label1: TcxLabel;
    Label2: TcxLabel;
    Label3: TcxLabel;
    Label4: TcxLabel;
    cbPenStyles: TcxComboBox;
    cbPenModes: TcxComboBox;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    gbEndCap: TcxGroupBox;
    rbEndCapFlat: TcxRadioButton;
    rbEndCapSquare: TcxRadioButton;
    rbEndCapRound: TcxRadioButton;
    gbJoin: TcxGroupBox;
    rbJoinBevel: TcxRadioButton;
    rbJoinMiter: TcxRadioButton;
    rbJoinRound: TcxRadioButton;
    chSolidAsInside: TcxCheckBox;
    ccbPenColor: TcxColorComboBox;
    sedPenWidth: TcxSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbPenStylesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
  private
    { Private declarations }
    FPenProp: TPenProp;
  public
    { Public declarations }
  end;

var
  PenPropForm: TPenPropForm;

implementation

{$R *.DFM}

procedure TPenPropForm.FormCreate(Sender: TObject);
var i: integer;
begin
 for i:=0 to integer(High(TPenStyle)) do
  case TPenStyle(i) of
   psSolid        : cbPenStyles.Properties.Items.Add('Solid');
   psDash         : cbPenStyles.Properties.Items.Add('Dash');
   psDot          : cbPenStyles.Properties.Items.Add('Dot');
   psDashDot      : cbPenStyles.Properties.Items.Add('DashDot');
   psDashDotDot   : cbPenStyles.Properties.Items.Add('DashDotDot');
   psClear        : cbPenStyles.Properties.Items.Add('Clear');
   psInsideFrame  : ; // Skiped // cbPenStyles.Items.Add('Inside-frame');
   else             cbPenStyles.Properties.Items.Add('???');
  end;
 for i:=0 to integer(High(TPenMode)) do
  case TPenMode(i) of
   pmBlack        : cbPenModes.Properties.Items.Add('Black');
   pmWhite        : cbPenModes.Properties.Items.Add('White');
   pmNop          : cbPenModes.Properties.Items.Add('Nop');
   pmNot          : cbPenModes.Properties.Items.Add('Not');
   pmCopy         : cbPenModes.Properties.Items.Add('Copy');
   pmNotCopy      : cbPenModes.Properties.Items.Add('NotCopy');
   pmMergePenNot  : cbPenModes.Properties.Items.Add('MergePenNot');
   pmMaskPenNot   : cbPenModes.Properties.Items.Add('MaskPenNot');
   pmMergeNotPen  : cbPenModes.Properties.Items.Add('MergeNotPen');
   pmMaskNotPen   : cbPenModes.Properties.Items.Add('MaskNotPen');
   pmMerge        : cbPenModes.Properties.Items.Add('Merge');
   pmNotMerge     : cbPenModes.Properties.Items.Add('NotMerge');
   pmMask         : cbPenModes.Properties.Items.Add('Mask');
   pmNotMask      : cbPenModes.Properties.Items.Add('NotMask');
   pmXor          : cbPenModes.Properties.Items.Add('Xor');
   pmNotXor       : cbPenModes.Properties.Items.Add('NotXor');
   else             cbPenModes.Properties.Items.Add('???');
  end;
end;

procedure TPenPropForm.FormShow(Sender: TObject);
begin
 if (Tag <> 0) and (TObject(Tag) is TPenProp) then FPenProp := TPenProp(Tag);
 if Assigned(FPenProp) then with FPenProp do begin
  ccbPenColor.ColorValue := Color;
  cbPenStyles.ItemIndex := integer(Style);
  sedPenWidth.Value := Width / PixelScaleFactor;
  cbPenModes.ItemIndex := integer(Mode);
  case EndCap of
   pecFlat    : rbEndCapFlat.Checked := True;
   pecSquare  : rbEndCapSquare.Checked := True;
   pecRound   : rbEndCapRound.Checked := True;
  end;
  case Join of
   pjBevel    : rbJoinBevel.Checked := True;
   pjMiter    : rbJoinMiter.Checked := True;
   pjRound    : rbJoinRound.Checked := True;
  end;
  chSolidAsInside.Checked := SolidAsInside;
 end;
end;

procedure TPenPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
var Recording: boolean;
begin
 if (ModalResult <> mrOk) or not Assigned(FPenProp) then exit;
 with FPenProp do begin
  Recording := Assigned(Owner.History) and
    Assigned(Owner.History.BeginAction(TPropHistoryGroup, FPenProp));
  try
   Color := ccbPenColor.ColorValue;
   Style := TPenStyle(cbPenStyles.ItemIndex);
   Width := Round(sedPenWidth.Value * PixelScaleFactor);
   Mode := TPenMode(cbPenModes.ItemIndex);
   // End cap
   if rbEndCapFlat.Checked    then EndCap := pecFlat else
   if rbEndCapSquare.Checked  then EndCap := pecSquare else
   if rbEndCapRound.Checked   then EndCap := pecRound;
   // Join
   if rbJoinBevel.Checked then Join := pjBevel else
   if rbJoinMiter.Checked then Join := pjMiter else
   if rbJoinRound.Checked then Join := pjRound;
   // SolidAsInside
   SolidAsInside := chSolidAsInside.Checked;
  finally
   if Recording then Owner.History.EndAction;
  end;
 end;
end;

procedure TPenPropForm.cbPenStylesDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
var R, r2: TRect;
    Y: integer;
begin
 with AControl as TcxComboBox do begin
  with ACanvas.Canvas do begin
   Brush.Style := bsSolid;
   FillRect(ARect);
   R := Classes.Rect(ARect.Left+3, ARect.Top+3, ARect.Left + 38, ARect.Bottom - 3);
   Pen.Style := TPenStyle(AIndex);
   if odSelected in AState
    then Pen.Color := clWhite
    else Pen.Color := clBlack;
   Y := R.Top + (R.Bottom - R.Top) div 2;
   MoveTo(R.Left, Y);
   LineTo(R.Right, Y);
   Brush.Style := bsClear;
   r2 := ARect;
   OffsetRect(r2, 47, 0);
   R.Top := r2.Top + ((r2.Bottom - r2.Top) - TextHeight(Properties.Items[AIndex])) div 2;
   TextOut(r2.Left, R.Top, Properties.Items[AIndex]);
  end;
 end;
end;

initialization
  RegisterDefaultPropEditForm(TPenProp, TPenPropForm);

end.
