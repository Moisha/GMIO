unit UserDataFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ActnList, ImgList, StdCtrls, Buttons, ExtCtrls, FlexProps,
  Ex_Grid, Ex_Inspector, System.Actions, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, dxSkinsCore,
  dxSkinOffice2010Silver, cxControls, cxContainer, cxEdit, cxGroupBox, cxButtons;

type
  TUserDataForm = class(TForm)
    imgToolIcons: TImageList;
    alMain: TActionList;
    acUserPropAdd: TAction;
    acUserPropDelete: TAction;
    acUserPropMoveUp: TAction;
    acUserPropMoveDown: TAction;
    bbOk: TcxButton;
    bbCancel: TcxButton;
    panMain: TcxGroupBox;
    tbMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    grUserProps: TExInspector;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acUserPropAddExecute(Sender: TObject);
    procedure acUserPropDeleteExecute(Sender: TObject);
    procedure acUserPropMoveUpExecute(Sender: TObject);
    procedure acUserPropMoveDownExecute(Sender: TObject);
    procedure grUserPropsChange(Sender: TObject; Cell: TGridCell;
      Selected: Boolean);
    procedure grUserPropsGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: String);
    procedure grUserPropsSetEditText(Sender: TObject; Cell: TGridCell;
      var Value: String);
  private
    { Private declarations }
    FUserDataProp: TUserDataProp;
    FProps: TStringList;
    procedure DrawCell(Sender: TObject; Cell: TGridCell; var Rect: TRect;
      var DefaultDrawing: Boolean);
    procedure CheckTools;
  public
    { Public declarations }
  end;

var
  UserDataForm: TUserDataForm;

implementation

uses
  ToolMngr;

{$R *.DFM}

procedure TUserDataForm.FormCreate(Sender: TObject);
begin
 FProps := TStringList.Create;
 with grUserProps do begin
  Columns[0].ReadOnly := False;
  Columns[0].TabStop := True;
  Columns[0].Width := 80;
  Fixed.Count := 0;
  OnDrawCell := DrawCell;
  //AlwaysEdit := false;
 end;
end;

procedure TUserDataForm.FormDestroy(Sender: TObject);
begin
 FProps.Free;
end;

procedure TUserDataForm.FormShow(Sender: TObject);
begin
 if (Tag <> 0) and (TObject(Tag) is TUserDataProp) then
  FUserDataProp := TUserDataProp(Tag);
 if Assigned(FUserDataProp) then with FUserDataProp do begin
  AssignTo(FProps);
  grUserProps.Rows.Count := FProps.Count;
  if FProps.Count > 0 then grUserProps.CellFocused := GridCell(1, 0);
 end;
 CheckTools;
end;

procedure TUserDataForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 if (ModalResult <> mrOk) or not Assigned(FUserDataProp) then exit;
 grUserProps.Editing := False;
 FUserDataProp.Assign(FProps);
end;

procedure TUserDataForm.CheckTools;
var DataExists: boolean;
begin
 DataExists := FProps.Count > 0;
 acUserPropAdd.Enabled := true;
 acUserPropDelete.Enabled := DataExists;
 acUserPropMoveUp.Enabled := DataExists and (grUserProps.CellFocused.Row > 0);
 acUserPropMoveDown.Enabled := DataExists and
  (grUserProps.CellFocused.Row < FProps.Count-1);
end;

procedure TUserDataForm.DrawCell(Sender: TObject; Cell: TGridCell;
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

procedure TUserDataForm.grUserPropsGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: String);
var i: integer;
begin
 if Cell.Col = 0 then
  Value := FProps.Names[Cell.Row]
 else begin
  Value := FProps[Cell.Row];
  i := Pos('=', Value);
  Value := copy(Value, i+1, MaxInt);
 end;
end;

procedure TUserDataForm.grUserPropsSetEditText(Sender: TObject;
  Cell: TGridCell; var Value: String);
var s: string;
    i: integer;
begin
 if Cell.Col = 0 then begin
  s := FProps[Cell.Row];
  i := Pos('=', s);
  FProps[Cell.Row] := Value + '=' + copy(s, i+1, MaxInt);
 end else
  FProps[Cell.Row] := FProps.Names[Cell.Row] + '=' + Value;
end;

procedure TUserDataForm.acUserPropAddExecute(Sender: TObject);
var i: integer;
begin
 i := FProps.Add('');
 with grUserProps do begin
  Rows.Count := FProps.Count;
  CellFocused := GridCell(0, i);
  Editing := true;
 end;
 CheckTools;
end;

procedure TUserDataForm.acUserPropDeleteExecute(Sender: TObject);
begin
 FProps.Delete(grUserProps.CellFocused.Row);
 grUserProps.Rows.Count := FProps.Count;
end;

procedure TUserDataForm.acUserPropMoveUpExecute(Sender: TObject);
begin
 with grUserProps, CellFocused do begin
  AlwaysEdit := False;
  Editing := False;
  FProps.Move(Row, Row-1);
  CellFocused := GridCell(Col, Row-1);
  AlwaysEdit := True;
 end;
 Invalidate;
 CheckTools;
end;

procedure TUserDataForm.acUserPropMoveDownExecute(Sender: TObject);
begin
 with grUserProps, CellFocused do begin
  AlwaysEdit := False;
  Editing := False;
  FProps.Move(Row, Row+1);
  CellFocused := GridCell(Col, Row+1);
  AlwaysEdit := True;
 end;
 Invalidate;
 CheckTools;
end;

procedure TUserDataForm.grUserPropsChange(Sender: TObject; Cell: TGridCell;
  Selected: Boolean);
begin
 CheckTools;
end;

initialization
  RegisterDefaultPropEditForm(TUserDataProp, TUserDataForm);

end.
   
