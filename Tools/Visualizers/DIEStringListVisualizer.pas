{*******************************************************}
{                                                       }
{            RadStudio Debugger Visualizer Sample       }
{ Copyright(c) 2009-2013 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit DIEStringListVisualizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolsAPI, Vcl.StdCtrls, Vcl.Buttons;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope, asNotAvailable);

  TStringListViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater, 
    IOTAThreadNotifier, IOTAThreadNotifier160)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    StringListView: TListView;
    TabSheet2: TTabSheet;
    memoString: TMemo;
    speedButtonCopy: TSpeedButton;
    labelStrings: TLabel;
    labelLength: TLabel;
    procedure StringListViewData(Sender: TObject; Item: TListItem);
    procedure speedButtonCopyClick(Sender: TObject);
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FItems: TStrings;
    FAvailableState: TAvailableState;
    function Evaluate(Expression: string): string;
    function GetDelimiter: string;
    function GetLength: Integer;
    function GetSubString(Index, Count: Integer): string;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    procedure SetForm(AForm: TCustomForm);
    procedure AddStringListItems(const Expression, TypeName, EvalResult: string);

    { IOTAThreadNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer);
  end;

procedure Register;

implementation

uses
  DesignIntf, Actnlist, ImgList, Menus, IniFiles, DCVCommon;

{$R *.dfm}

resourcestring
  sStringListVisualizerName = 'DIE TStrings Visualizer for Delphi';
  sStringListVisualizerDescription = 'Displays a list of the actual strings held in a TStrings instance';
  sMenuText = 'DIE Show Strings';
  sFormCaption = 'DIE TStrings Visualizer for %s';
  sProcessNotAccessible = 'process not accessible';
  sValueNotAccessible = 'value not accessible';
  sOutOfScope = 'out of scope';

type

  IFrameFormHelper = interface
    ['{0FD4A98F-CE6B-422A-BF13-14E59707D3B2}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TStringListVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
  private
    FMyFrame: TStringListViewerFrame;
    FMyForm: TCustomForm;
    FExpression: string;
  public
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerStringListVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerExternalViewer{, IOTADebuggerVisualizerValueReplacer})
  public
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string; Suggestedleft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
    //function GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
  end;

{ TDebuggerDateTimeVisualizer }

function TDebuggerStringListVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;
{
function TDebuggerStringListVisualizer.GetReplacementValue(const Expression,
  TypeName, EvalResult: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..255] of Char;
  CanModify: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  Result := EvalResult;

  Application.MessageBox(PChar(Result), PChar(Expression), 0);

  if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    CurProcess := DebugSvcs.CurrentProcess;
  if CurProcess = nil then Exit;

  CurThread := CurProcess.CurrentThread;
  if CurThread = nil then Exit;

  EvalRes := CurThread.Evaluate(Expression+'.Text', @ResultStr, Length(ResultStr),
    CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
  if EvalRes = erOK then
  begin
    Result := ResultStr;
  end;

  Application.MessageBox(PChar(Result), PChar(IntToStr(Ord(EvalRes))), 0);
end;
 }
procedure TDebuggerStringListVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  TypeName := 'TStrings';
  AllDescendants := True;
end;

function TDebuggerStringListVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := 1;
end;

function TDebuggerStringListVisualizer.GetVisualizerDescription: string;
begin
  Result := sStringListVisualizerDescription;
end;

function TDebuggerStringListVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerStringListVisualizer.GetVisualizerName: string;
begin
  Result := sStringListVisualizerName;
end;

function TDebuggerStringListVisualizer.Show(const Expression, TypeName, EvalResult: string; SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TStringListViewerFrame;
  VisDockForm: INTACustomDockableForm;
begin
  VisDockForm := TStringListVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.Left := SuggestedLeft;
  AForm.Top := SuggestedTop;
  (VisDockForm as IFrameFormHelper).SetForm(AForm);
  AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TStringListViewerFrame;
  AFrame.AddStringListItems(Expression, TypeName, EvalResult);
  Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
end;


{ TStringListViewerFrame }

procedure TStringListViewerFrame.AddStringListItems(const Expression, TypeName,
  EvalResult: string);
var
  Delim, s: string;
  pos, len: integer;
begin
  FAvailableState := asAvailable;
  FExpression := Expression;
  if FItems = nil then
    FItems := TStringList.Create
  else
    FItems.Clear;

  Delim := GetDelimiter;
  if Length(Delim) > 1 then
  begin
    len := GetLength();
    pos := 1;
    while pos <= len do
    begin
      s := s + MakeReadable(GetSubString(pos, EvalBufLen));
      pos := Length(s) + 1;
    end;

    FItems.Text := s;

    StringListView.Items.Count := FItems.Count;
    StringListView.Invalidate;

    memoString.Lines.Assign(FItems);
    labelLength.Caption := 'Длина: ' + IntToStr(Length(memoString.Lines.Text) - 2);
    labelStrings.Caption := 'Строк: ' + IntToStr(memoString.Lines.Count);
    memoString.Invalidate();
  end else
  begin
    FAvailableState := asNotAvailable;
    StringListView.Invalidate;
  end;
end;

procedure TStringListViewerFrame.AfterSave;
begin

end;

procedure TStringListViewerFrame.BeforeSave;
begin

end;

procedure TStringListViewerFrame.CloseVisualizer;
begin
  if FOwningForm <> nil then
    FOwningForm.Close;
end;

procedure TStringListViewerFrame.Destroyed;
begin

end;

function TStringListViewerFrame.Evaluate(Expression: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..EvalBufLen - 1] of Char;
  CanModify: Boolean;
  Done: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  begin
    Result := '';
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        repeat
        begin
          Done := True;
          EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
            CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
          case EvalRes of
            erOK: Result := ResultStr;
            erDeferred:
              begin
                FCompleted := False;
                FDeferredResult := '';
                FDeferredError := False;
                FNotifierIndex := CurThread.AddNotifier(Self);
                while not FCompleted do
                  DebugSvcs.ProcessDebugEvents;
                CurThread.RemoveNotifier(FNotifierIndex);
                FNotifierIndex := -1;
                if not FDeferredError then
                begin
                  if FDeferredResult <> '' then
                    Result := FDeferredResult
                  else
                    Result := ResultStr;
                end;
              end;
            erBusy:
              begin
                DebugSvcs.ProcessDebugEvents;
                Done := False;
              end;
          end;
        end
        until Done = True;
      end;
    end;
  end;
end;

procedure TStringListViewerFrame.EvaluteComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress), ResultSize, ReturnCode);
end;

procedure TStringListViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

function TStringListViewerFrame.GetDelimiter: string;
begin
  Result := Evaluate(Format('%s.Delimiter', [FExpression]));
end;

function TStringListViewerFrame.GetSubString(Index, Count: Integer): string;
begin
  Result := Evaluate(Format('Copy(%s.Text, %d, %d)', [FExpression, Index, Count]));
end;

function TStringListViewerFrame.GetLength: Integer;
begin
  Result := StrToInt(Evaluate(Format('Length(%s.Text)', [FExpression]))) - 2;
end;

procedure TStringListViewerFrame.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
  begin
    FAvailableState := asProcRunning;
  end else if Reason = ovurOutOfScope then
    FAvailableState := asOutOfScope;
  StringListView.Items.Count := 1;
  StringListView.Invalidate;
end;

procedure TStringListViewerFrame.Modified;
begin

end;

procedure TStringListViewerFrame.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TStringListViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  FAvailableState := asAvailable;
  AddStringListItems(Expression, TypeName, EvalResult);
end;

procedure TStringListViewerFrame.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TStringListViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TStringListViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    FreeAndNil(FItems);
    if Assigned(FClosedProc) then
      FClosedProc;
  end;
  inherited;
end;

procedure TStringListViewerFrame.speedButtonCopyClick(Sender: TObject);
var n: integer;
begin
  n := memoString.SelStart;
  memoString.SelectAll();
  memoString.CopyToClipboard();
  memoString.SelStart := n;
end;


procedure TStringListViewerFrame.StringListViewData(Sender: TObject;
  Item: TListItem);
var
  ItemCaption: string;
  ItemText: string;
begin
  case FAvailableState of
    asAvailable:
      begin
        ItemCaption := Format('[%d]', [Item.Index]);
        ItemText := FItems[Item.Index];
      end;
    asProcRunning:
      begin
        ItemCaption := sProcessNotAccessible;
        ItemText := sProcessNotAccessible;
      end;
    asOutOfScope:
      begin
        ItemCaption := sOutOfScope;
        ItemText := sOutOfScope;
      end;
    asNotAvailable:
      begin
        ItemCaption := sValueNotAccessible;
        ItemText := sValueNotAccessible;
      end;
  end;
  Item.Caption := ItemCaption;
  if Item.SubItems.Count = 0 then
    Item.SubItems.Add(ItemText)
  else
    Item.SubItems[0] := ItemText;
end;

procedure TStringListViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

{ TStringListVisualizerForm }

constructor TStringListVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TStringListVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TStringListVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
 // no toolbar
end;

function TStringListVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TStringListVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame :=  TStringListViewerFrame(AFrame);
end;

function TStringListVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TStringListVisualizerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TStringListVisualizerForm.GetForm: TCustomForm;
begin
  Result := FMyForm;
end;

function TStringListVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

function TStringListVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TStringListViewerFrame;
end;

function TStringListVisualizerForm.GetIdentifier: string;
begin
  Result := 'DIEStringListDebugVisualizer';
end;

function TStringListVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TStringListVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TStringListVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TStringListVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TStringListVisualizerForm.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

procedure TStringListVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
   FMyFrame := TStringListViewerFrame(Frame);
end;

var
  StringListVis: IOTADebuggerVisualizer;

procedure Register;
begin
  StringListVis := TDebuggerStringListVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(StringListVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(StringListVis);
    StringListVis := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.

