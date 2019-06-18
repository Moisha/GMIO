unit fUserData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ActnList, ImgList, Ex_Grid, Ex_Inspector,
  FlexBase, FlexProps,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  TfmUserData = class(TForm)
    imgToolIcons: TImageList;
    tbrMain: TToolBar;
    tbNew: TToolButton;
    tbDelete: TToolButton;
    tbDelim: TToolButton;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
    grUserProps: TExInspector;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbMoveUpClick(Sender: TObject);
    procedure tbMoveDownClick(Sender: TObject);
    procedure grUserPropsGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: String);
    procedure grUserPropsSetEditText(Sender: TObject; Cell: TGridCell;
      var Value: String);
    procedure grUserPropsChange(Sender: TObject; Cell: TGridCell;
      Selected: Boolean);
  private
    { Private declarations }
    FActiveFlex: TFlexPanel;
    FLastControl: TFlexControl;
    procedure CheckTools;
    procedure SetActiveFlex(const Value: TFlexPanel);
    function  GetSelControl: TFlexControl;
    procedure DrawCell(Sender: TObject; Cell: TGridCell;
      var Rect: TRect; var DefaultDrawing: Boolean);
  public
    { Public declarations }
    property  ActiveFlex: TFlexPanel read FActiveFlex write SetActiveFlex;
    property  SelControl: TFlexControl read GetSelControl;
    procedure UpdateData;
  end;

var
  fmUserData: TfmUserData;

implementation

{$R *.DFM}

uses
  ToolMngr;

procedure TfmUserData.FormCreate(Sender: TObject);
begin
 RegisterToolForm(Self);
 with grUserProps do begin
  Columns[0].ReadOnly := False;
  Columns[0].TabStop := True;
  Columns[0].Width := 80;
  Fixed.Count := 0;
  OnDrawCell := DrawCell;
  //AlwaysEdit := false;
 end;
 CheckTools;
end;

procedure TfmUserData.FormDestroy(Sender: TObject);
begin
 UnRegisterToolForm(Self);
 fmUserData := Nil;
end;

procedure TfmUserData.CheckTools;
var DataExists: boolean;
    Control: TFlexControl;
begin
 Control := SelControl;
 if Assigned(Control)
  then DataExists := Control.UserData.LinesCount > 0
  else DataExists := False;
 tbNew.Enabled := Assigned(Control);
 tbDelete.Enabled := DataExists;
 if DataExists then begin
  tbMoveUp.Enabled := grUserProps.CellFocused.Row > 0;
  tbMoveDown.Enabled :=
    grUserProps.CellFocused.Row < Control.UserData.LinesCount-1;
 end else begin
  tbMoveUp.Enabled := False;
  tbMoveDown.Enabled := False;
 end;
end;

procedure TfmUserData.UpdateData;
var Control: TFlexControl;
begin
 Control := SelControl;
 with grUserProps do begin
  if Assigned(FLastControl) and (FLastControl <> Control) then Editing := False;
  if Assigned(Control) then begin
   Rows.Count := Control.UserData.LinesCount;
   if Editing then Edit.Text := Cells[EditCell.Col, EditCell.Row];
   if EditCell.Row >= 0 then begin
    AlwaysEdit := False;
    Editing := False;
    AlwaysEdit := True;
   end;
  end else begin
   Rows.Count := 0;
   AlwaysEdit := False;
   Editing := False;
   AlwaysEdit := True;
  end;
  Invalidate;
 end;
 CheckTools;
 FLastControl := Control;
end;

function TfmUserData.GetSelControl: TFlexControl;
begin
 if Assigned(FActiveFlex) and (FActiveFlex.SelectedCount = 1)
  then Result := FActiveFlex.Selected[0]
  else Result := Nil;
end;

procedure TfmUserData.SetActiveFlex(const Value: TFlexPanel);
begin
 if Value = FActiveFlex then exit;
 if not Assigned(Value) then FLastControl := Nil;
 FActiveFlex := Value;
 UpdateData;
end;

procedure TfmUserData.DrawCell(Sender: TObject; Cell: TGridCell;
  var Rect: TRect; var DefaultDrawing: Boolean);
begin
 DefaultDrawing := true;
 if Cell.Col = 0 then with TGridView(grUserProps).Canvas do begin
  Pen.Color := clBtnShadow;
  Pen.Width := 1;
  MoveTo(Rect.Right - 2, Rect.Top - 1);
  LineTo(Rect.Right - 2, Rect.Bottom);
  Pen.Color := clBtnHighlight;
  MoveTo(Rect.Right - 1, Rect.Bottom - 1);
  LineTo(Rect.Right - 1, Rect.Top - 1);
  dec(Rect.Right, 2);
 end;
end;

procedure TfmUserData.grUserPropsGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: String);
var Control: TFlexControl;
begin
 Control := SelControl;
 if Assigned(Control) then
  case Cell.Col of
   0: Value := Control.UserData.Names[Cell.Row];
   1: Value := Control.UserData.ValuesByIndex[Cell.Row];
  end;
end;

procedure TfmUserData.grUserPropsSetEditText(Sender: TObject;
  Cell: TGridCell; var Value: String);
begin
 if Assigned(FLastControl) then
  if Cell.Col = 0
   then FLastControl.UserData.Names[Cell.Row] := Value
   else FLastControl.UserData.ValuesByIndex[Cell.Row] := Value;
 grUserProps.InvalidateCell(Cell);
end;

procedure TfmUserData.grUserPropsChange(Sender: TObject; Cell: TGridCell;
  Selected: Boolean);
begin
 CheckTools;
end;

procedure TfmUserData.tbNewClick(Sender: TObject);
var Control: TFlexControl;
begin
 Control := SelControl;
 if Assigned(Control) then begin
  grUserProps.Editing := False;
  Control.UserData.Add('');
  UpdateData;
  grUserProps.CellFocused := GridCell(0, grUserProps.Rows.Count-1);
 end;
end;

procedure TfmUserData.tbDeleteClick(Sender: TObject);
var Control: TFlexControl;
begin
 Control := SelControl;
 if Assigned(Control) then begin
  Control.UserData.Delete(grUserProps.CellFocused.Row);
  UpdateData;
 end;
end;

procedure TfmUserData.tbMoveUpClick(Sender: TObject);
var Control: TFlexControl;
    s1, s2: string;
begin
 Control := SelControl;
 if Assigned(Control) then with Control.UserData, grUserProps do begin
  Editing := False;
  s1 := Lines[CellFocused.Row-1];
  s2 := Lines[CellFocused.Row];
  Lines[CellFocused.Row-1] := s2;
  Lines[CellFocused.Row] := s1;
  UpdateData;
  CellFocused := GridCell(CellFocused.Col, CellFocused.Row-1);
 end;
end;

procedure TfmUserData.tbMoveDownClick(Sender: TObject);
var Control: TFlexControl;
    s1, s2: string;
begin
 Control := SelControl;
 if Assigned(Control) then with Control.UserData, grUserProps do begin
  Editing := False;
  s1 := Lines[CellFocused.Row];
  s2 := Lines[CellFocused.Row+1];
  Lines[CellFocused.Row] := s2;
  Lines[CellFocused.Row+1] := s1;
  UpdateData;
  CellFocused := GridCell(CellFocused.Col, CellFocused.Row+1);
 end;
end;

end.
