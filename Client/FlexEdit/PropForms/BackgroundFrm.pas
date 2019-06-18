unit BackgroundFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, FlexProps, FlexUtils, RXSpin, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  Vcl.Menus, dxSkinsCore, dxSkinOffice2010Silver, cxControls, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxSpinEdit,
  cxCheckBox, cxGroupBox, cxButtons;

type
  TBackgroundOptionsForm = class(TForm)
    chBrushEnabled: TcxCheckBox;
    chPictureEnabled: TcxCheckBox;
    gbPicture: TcxGroupBox;
    chLeft: TcxCheckBox;
    chTop: TcxCheckBox;
    chWidth: TcxCheckBox;
    chHeight: TcxCheckBox;
    chCenterHoriz: TcxCheckBox;
    chCenterVert: TcxCheckBox;
    chAlignRight: TcxCheckBox;
    chAlignBottom: TcxCheckBox;
    chStretchHoriz: TcxCheckBox;
    chStretchVert: TcxCheckBox;
    chScaledSize: TcxCheckBox;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    sedLeft: TcxSpinEdit;
    sedTop: TcxSpinEdit;
    sedWidth: TcxSpinEdit;
    sedHeight: TcxSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chClick(Sender: TObject);
  private
    FBackgroundOptions: TBackgroundOptionsProp;
    procedure CheckTools;
  end;

var
  BackgroundOptionsForm: TBackgroundOptionsForm;

implementation

{$R *.dfm}

procedure TBackgroundOptionsForm.FormShow(Sender: TObject);
var Options: TBackgroundPictureOptions;
begin
 if (Tag <> 0) and (TObject(Tag) is TBackgroundOptionsProp) then
  FBackgroundOptions := TBackgroundOptionsProp(Tag);
 if Assigned(FBackgroundOptions) then with FBackgroundOptions do begin
  chBrushEnabled.Checked := BrushEnabled;
  chPictureEnabled.Checked := PictureEnabled;
  Options := PictureOptions;
  chLeft.Checked := bpOffsetLeft in Options;
  sedLeft.Value := Left / PixelScaleFactor;
  chTop.Checked := bpOffsetTop in Options;
  sedTop.Value := Top / PixelScaleFactor;
  chWidth.Checked := bpNewWidth in Options;
  sedWidth.Value := Width / PixelScaleFactor;
  chHeight.Checked := bpNewHeight in Options;
  sedHeight.Value := Height / PixelScaleFactor;
  chCenterHoriz.Checked := bpCenterHoriz in Options;
  chCenterVert.Checked := bpCenterVert in Options;
  chAlignRight.Checked := bpAlignRight in Options;
  chAlignBottom.Checked := bpAlignBottom in Options;
  chStretchHoriz.Checked := bpStretchHoriz in Options;
  chStretchVert.Checked := bpStretchVert in Options;
  chScaledSize.Checked := bpScaledSize in Options;
  CheckTools;
 end;
end;

procedure TBackgroundOptionsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var Recording: boolean;
    Options: TBackgroundPictureOptions;
begin
 if (ModalResult <> mrOk) or not Assigned(FBackgroundOptions) then exit;
 with FBackgroundOptions do begin
  Recording := Assigned(Owner.History) and
    Assigned(Owner.History.BeginAction(TPropHistoryGroup, FBackgroundOptions));
  try
   BrushEnabled := chBrushEnabled.Checked;
   PictureEnabled := chPictureEnabled.Checked;
   Options := [];
   if chLeft.Checked then Include(Options, bpOffsetLeft);
   Left := Round(sedLeft.Value * PixelScaleFactor);
   if chTop.Checked then Include(Options, bpOffsetTop);
   Top := Round(sedTop.Value * PixelScaleFactor);
   if chWidth.Checked then Include(Options, bpNewWidth);
   Width := Round(sedWidth.Value * PixelScaleFactor);
   if chHeight.Checked then Include(Options, bpNewHeight);
   Height := Round(sedHeight.Value * PixelScaleFactor);
   if chCenterHoriz.Checked then Include(Options, bpCenterHoriz);
   if chCenterVert.Checked then Include(Options, bpCenterVert);
   if chAlignRight.Checked then Include(Options, bpAlignRight);
   if chAlignBottom.Checked then Include(Options, bpAlignBottom);
   if chStretchHoriz.Checked then Include(Options, bpStretchHoriz);
   if chStretchVert.Checked then Include(Options, bpStretchVert);
   if chScaledSize.Checked then Include(Options, bpScaledSize);
   PictureOptions := Options;
  finally
   if Recording then Owner.History.EndAction;
  end;
 end;
end;

procedure TBackgroundOptionsForm.chClick(Sender: TObject);
begin
 CheckTools;
end;

procedure TBackgroundOptionsForm.CheckTools;
begin
 sedTop.Enabled := chTop.Checked;
 sedLeft.Enabled := chLeft.Checked;
 sedWidth.Enabled := chWidth.Checked;
 sedHeight.Enabled := chHeight.Checked;
end;

initialization
  RegisterDefaultPropEditForm(TBackgroundOptionsProp, TBackgroundOptionsForm);

end.
