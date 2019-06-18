unit ColorComboEdit;

interface

uses
  Windows, Types, UITypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RxCombos, CommDlg;

type
  TColorComboEdit = class(TColorComboBox)
  private
   { Private declarations }
   FColorDlg: TColorDialog;
   FCanTransparent: boolean;
   procedure SetCanTransparent(const Value: boolean);
  protected
   { Protected declarations }
   procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
   procedure PopulateList; override;
   procedure Click; override;
   procedure DoChange; override;
   procedure ChooseColor;
   procedure SetNewColor(Value: TColor);
  public
   { Public declarations }
  published
   { Published declarations }
   property  CanTransparent: boolean read FCanTransparent
     write SetCanTransparent;
  end;

var
  CustomColors: TStringList;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TColorComboEdit]);
end;

// TColorComboEdit ////////////////////////////////////////////////////////////

const
  ColorValues: array [0..17] of TColor = (
    clNone,
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
    clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite,
    clDefault );

procedure TColorComboEdit.SetNewColor(Value: TColor);
begin

end;

procedure TColorComboEdit.Click;
var OldColor: TCOlor;
begin
 OldColor := ColorValue;
 inherited;
 if ItemIndex = Items.Count - 1 then begin
  ColorValue := OldColor;
  ChooseColor;
 end;
end;

procedure TColorComboEdit.DoChange;
begin
 inherited;

 if ItemIndex = -1 then ItemIndex := Items.Count-1;
end;

procedure TColorComboEdit.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
const
  ColorWidth = 22;
var
  ARect: TRect;
  Text: array[0..255] of Char;
  Safer: TColor;
begin
  ARect := Rect;
  Inc(ARect.Top, 2);
  Inc(ARect.Left, 2);
  Dec(ARect.Bottom, 2);
  if DisplayNames then ARect.Right := ARect.Left + ColorWidth
  else Dec(ARect.Right, 3);
  with Canvas do begin
    FillRect(Rect);
    Safer := Brush.Color;
    try
     Pen.Color := clWindowText;
     Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
     InflateRect(ARect, -1, -1);
     if TColor(Items.Objects[Index]) = clNone then begin
      Brush.Color := clWhite;
      FillRect(ARect);
      MoveTo(ARect.Left, ARect.Top);
      LineTo(ARect.Right, ARect.Bottom-1);
      MoveTo(ARect.Right, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom-1);
     end else
     if Index = Items.Count-1 then begin
      Brush.Color := ColorValue;
      FillRect(ARect);
     end else begin
      Brush.Color := TColor(Items.Objects[Index]);
      FillRect(ARect);
     end;
    finally
     Brush.Color := Safer;
    end;
    if DisplayNames then begin
      StrPCopy(Text, Items[Index]);
      Rect.Left := Rect.Left + ColorWidth + 6;
      DrawText(Canvas.Handle, Text, StrLen(Text), Rect,
        DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX));
    end;
  end;
end;

procedure TColorComboEdit.PopulateList;
var
  i: Integer;
  ColorName: string;
begin
  Items.BeginUpdate;
  try
    Clear;
    for i := 0 to High(ColorValues) do
    begin
     if (i = 0) and not FCanTransparent then continue;

     if i = High(ColorValues) then
       ColorName := 'Other ...'
     else
       { delete two first characters which prefix "cl" educated }
       ColorName := Copy(ColorToString(ColorValues[i]), 3, MaxInt);

     Items.AddObject(ColorName, TObject(ColorValues[i]));
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TColorComboEdit.ChooseColor;
begin
 FColorDlg := TColorDialog.Create(Self);
 with FColorDlg do
 try
  CustomColors.Assign(ColorComboEdit.CustomColors);
  Color := ColorValue;
  Options := [cdFullOpen, cdAnyColor];
  if Execute then begin
   ColorValue := Color;
   ColorComboEdit.CustomColors.Assign(CustomColors);
  end;
 finally
  FColorDlg.Free;
  FColorDlg := Nil;
 end;
end;

procedure TColorComboEdit.SetCanTransparent(const Value: boolean);
begin
 if Value = FCanTransparent then exit;
 FCanTransparent := Value;
 if not FCanTransparent and (ColorValue = clNone) then ColorValue := clBlack;
 PopulateList;
end;

initialization
  CustomColors := TStringList.Create;

finalization
  CustomColors.Free;
  CustomColors := Nil;

end.
