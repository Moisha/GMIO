unit fChild;

interface

uses
  Windows, Types, UITypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, FlexBase, SynEdit, SynEditHighlighter, SynHighlighterPas, GMGlobals,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, JvExControls, JvEditorCommon, JvEditor, JvHLEditor, Frames.ScriptEditor;

type
  TFlexChildFrame = class(TFrame)
    Flex: TFlexPanel;
    Splitter1: TcxSplitter;
    ScriptEditorFrame1: TScriptEditorFrame;
    procedure FlexPanel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mScriptExit(Sender: TObject);
    procedure mScriptChange(Sender: TObject);
    procedure FlexKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FFilename: string;
    FNewDocument: boolean;
    FLastClickPoint: TPoint;
    procedure SetFilename(const Value: string);
    procedure MoveSelected(shiftX, shiftY: int);
    procedure UpdateNames;
    procedure RenameControl(control: TFlexControl);
    function LookForControlWithSameName(parentControl, originalControl: TFlexControl): bool;
    function GetMaxControlNumberWithGivenPrefix(parentControl: TFlexControl; const prefix: string): int;
    procedure SelectedToPoint;
    procedure CheckTopLeft(control: TFlexControl; var top, left: int);
    function GetScript: string;
    procedure SetScript(const Value: string);
  public
    { Public declarations }
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    procedure CloseFlex();

    property  Filename: string read FFileName write SetFilename;
    property  NewDocument: boolean read FNewDocument write FNewDocument;

    procedure VKMenuUp();
    procedure VKMenuDown();
    procedure UpdateScript;

    procedure Copy;
    procedure Cut;
    procedure Paste(usePoint: bool = false);
    procedure Del;

    procedure Undo;
    procedure Redo;

    property Script: string read GetScript write SetScript;
  end;

implementation

uses FlexUtils, FlexControls, Character, Math;

{$R *.DFM}

// TFlexChildFrame /////////////////////////////////////////////////////////////

procedure TFlexChildFrame.Copy;
begin
  if ScriptEditorFrame1.mScript.Focused() then
    ScriptEditorFrame1.mScript.ClipboardCopy
  else
    Flex.CopyToClipboard;
end;

constructor TFlexChildFrame.Create(AOwner: TComponent);
begin
  inherited;

  FNewDocument := true;
  Flex.InDesign := true;
  Flex.History.Active := true;

  ScriptEditorFrame1.mScript.Lines.Text :=
          'unit Sirius;'+
    #13#10 +
    #13#10'procedure Main();' +
    #13#10'begin' +
    #13#10 +
    #13#10'end;' +
    #13#10 +
    #13#10'end.';
end;

procedure TFlexChildFrame.Cut;
begin
  if ScriptEditorFrame1.mScript.Focused() then
    ScriptEditorFrame1.mScript.ClipboardCut
  else
    Flex.CutToClipboard;
end;

procedure TFlexChildFrame.Del;
begin
  if ScriptEditorFrame1.mScript.Focused() then
  begin
    ScriptEditorFrame1.mScript.SelText := '';
  end
  else
  begin
    if not Flex.Focused then exit;
    if (Flex.ToolMode = ftmPointEdit) and (Flex.EditPointSelectedTotal > 0) then
    begin
      if Flex.EditPointControl.PointCount - Flex.EditPointSelectedTotal > 2 then
        Flex.DeleteSelectedPoints;
    end
    else
     Flex.DeleteSelected;
  end;
end;


destructor TFlexChildFrame.Destroy;
begin
 inherited;
end;

procedure TFlexChildFrame.CloseFlex();
begin
 Screen.Cursor := crHourGlass;
 try
  Flex.UnselectAll;
  Flex.EmptyDocument;
 finally
  Screen.Cursor := crDefault;
 end;
end;

procedure TFlexChildFrame.MoveSelected(shiftX, shiftY: int);
begin
  if not Flex.Focused then exit;
  if (Flex.ToolMode = ftmPointEdit) and (Flex.EditPointSelectedTotal > 0) then Exit;

  Flex.MoveSelected(shiftX, shiftY);
end;

procedure TFlexChildFrame.FlexKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    Del();

  if Shift = [ssCtrl] then
  begin
    case AnsiChar(Key) of
      'C': Copy();
      'X': Cut();
      'V': Paste();
    end;
  end;

  if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT] then
    MoveSelected(ord(Key = VK_RIGHT) - ord(Key = VK_LEFT), ord(Key = VK_DOWN) - ord(Key = VK_UP));
end;

procedure TFlexChildFrame.FlexPanel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Flex.SetFocus;
  if not Flex.Focused then
    Windows.SetFocus(Flex.Handle);

  if Button = mbRight then
  begin
    FLastClickPoint.X := X;
    FLastClickPoint.Y := Y;
  end;
end;

procedure TFlexChildFrame.mScriptChange(Sender: TObject);
begin
  Flex.Modified := true;
end;

procedure TFlexChildFrame.mScriptExit(Sender: TObject);
begin
  UpdateScript();
end;

function TFlexChildFrame.LookForControlWithSameName(parentControl, originalControl: TFlexControl): bool;
var i: int;
begin
  Result := (parentControl <> originalControl) and AnsiSameText(parentControl.Name, originalControl.Name);
  if Result then Exit;

  for i := 0 to parentControl.Count - 1 do
  begin
    Result := LookForControlWithSameName(parentControl[i], originalControl);
    if Result then Exit;
  end;
end;

function TFlexChildFrame.GetMaxControlNumberWithGivenPrefix(parentControl: TFlexControl; const prefix: string): int;
var i: int;
    tmpprefix: string;
    tmpnumber: int;
begin
  Result := -1;

  GetNamePrefix(parentControl.Name, tmpprefix, tmpnumber);
  if AnsiSameText(prefix, tmpprefix) then
    Result := tmpnumber;

  for i := 0 to parentControl.Count - 1 do
  begin
    Result := Max(Result, GetMaxControlNumberWithGivenPrefix(parentControl[i], prefix));
  end;
end;

function TFlexChildFrame.GetScript: string;
begin
  Result := ScriptEditorFrame1.mScript.Lines.Text;
end;

procedure TFlexChildFrame.RenameControl(control: TFlexControl);
var prefix: string;
    i, number: int;
    hasSameName: bool;
begin
  hasSameName := LookForControlWithSameName(Flex.Schemes, control);
  if not hasSameName then Exit;

  GetNamePrefix(control.Name, prefix, number);
  number := GetMaxControlNumberWithGivenPrefix(Flex.Schemes, prefix);
  control.Name := prefix + IntToStr(number + 1);

  for i := 0 to control.Count - 1 do
    RenameControl(control[i]);
end;

procedure TFlexChildFrame.UpdateNames;
var i, j: int;
begin
  for i := 0 to Flex.Schemes.Count - 1 do
  begin
    for j := 0 to Flex.Schemes[i].Count - 1 do
      if Flex.Schemes[i][j].IsSelected then
        RenameControl(Flex.Schemes[i][j]);
  end;
end;

procedure TFlexChildFrame.CheckTopLeft(control: TFlexControl; var top, left: int);
var i: int;
begin
  top := min(top, control.Top);
  left := min(left, control.Left);

  for i := 0 to control.Count - 1 do
    CheckTopLeft(control[i], top, left);
end;

procedure TFlexChildFrame.SelectedToPoint();
var i, j, top, left: int;
    p: TPoint;
begin
  top := MaxInt;
  left := MaxInt;

  for i := 0 to Flex.Schemes.Count - 1 do
  begin
    for j := 0 to Flex.Schemes[i].Count - 1 do
      if Flex.Schemes[i][j].IsSelected then
        CheckTopLeft(Flex.Schemes[i][j], top, left);
  end;

  p := FLastClickPoint;
  Flex.UnTransformPoint(p.X, p.Y);
  MoveSelected(p.X - left, p.Y - top);
end;

procedure TFlexChildFrame.Paste(usePoint: bool = false);
begin
  if ScriptEditorFrame1.mScript.Focused() then
  begin
    ScriptEditorFrame1.mScript.ClipboardPaste();
  end
  else
  begin
    Flex.PasteFromClipboard();
    if usePoint then
      SelectedToPoint()
    else
      Flex.MoveSelected(10 * PixelScaleFactor, 10 * PixelScaleFactor);

    UpdateNames();
  end;
end;

procedure TFlexChildFrame.Redo;
begin
  if ScriptEditorFrame1.mScript.Focused() then
  begin
    ScriptEditorFrame1.mScript.Redo();
  end
  else
  begin
    Flex.History.Redo();
    Flex.Modified := true;
  end;
end;

procedure TFlexChildFrame.SetFilename(const Value: string);
begin
 if Value = FFilename then exit;
 FFileName := Value;
 Caption := ExtractFileName(FFilename);
 FNewDocument := not FileExists(FFileName);
end;

procedure TFlexChildFrame.SetScript(const Value: string);
begin
  ScriptEditorFrame1.mScript.Lines.Text := Value;
end;

procedure TFlexChildFrame.Undo;
begin
  if ScriptEditorFrame1.mScript.Focused() then
  begin
    ScriptEditorFrame1.mScript.Undo();
  end
  else
  begin
    Flex.History.Undo();
    Flex.Modified := true;
  end;
end;

procedure TFlexChildFrame.UpdateScript;
begin
  Flex.Script.Assign(ScriptEditorFrame1.mScript.Lines);
end;

procedure TFlexChildFrame.VKMenuDown;
begin
  if (Flex.ToolMode = ftmSelect) and not Assigned(Flex.CreatingControlClass) then
    Flex.ToolMode := ftmPan;
end;

procedure TFlexChildFrame.VKMenuUp;
begin
  if (Flex.ToolMode = ftmPan) or (Flex.ToolMode = ftmPanning) then
    Flex.ToolMode := ftmSelect;
end;

end.
