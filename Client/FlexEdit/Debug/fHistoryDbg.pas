unit fHistoryDbg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ActnList, Buttons, ImgList, ToolWin,
  FlexBase, FlexControls, FlexUtils, FlexProps, FlexPath,
  FlexHistory, FlexActions;

type
  TfmHistoryDebug = class(TForm)
    tvHistory: TTreeView;
    sbrMain: TStatusBar;
    mmUndo: TMemo;
    Splitter3: TSplitter;
    mmRedo: TMemo;
    Splitter1: TSplitter;
    procedure tvHistoryCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure acHistoryUndoExecute(Sender: TObject);
    procedure acHistoryRedoExecute(Sender: TObject);
    procedure tvHistoryChange(Sender: TObject; Node: TTreeNode);
    procedure fpMainNotify(Sender: TObject; Control: TFlexControl;
      Notify: TFlexNotify);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FInUpdate: boolean;
    FPanel: TFlexPanel;
    procedure CheckTools;
    procedure SetPanel(const Value: TFlexPanel);
  public
    { Public declarations }
    property  Flex: TFlexPanel read FPanel write SetPanel;
    procedure HistoryChange(Sender: TObject);
  end;

var
  fmHistoryDebug: TfmHistoryDebug;

implementation

{$R *.DFM}

procedure TfmHistoryDebug.FormCreate(Sender: TObject);
begin
 HistoryChange(Nil);
 tvHistoryChange(tvHistory, tvHistory.Selected);
 CheckTools;
end;

procedure TfmHistoryDebug.FormDestroy(Sender: TObject);
begin
 fmHistoryDebug := Nil;
end;

procedure TfmHistoryDebug.SetPanel(const Value: TFlexPanel);
begin
 if Value = FPanel then exit;
 FPanel := Value;
 if (csDestroying in ComponentState) then exit;
 HistoryChange(FPanel);
 tvHistoryChange(tvHistory, tvHistory.Selected);
end;

procedure TfmHistoryDebug.HistoryChange(Sender: TObject);

 procedure AddAction(Action: THistoryAction; AParent: TTreeNode);
 var Node: TTreeNode;
     i: integer;
 begin
  if Assigned(AParent)
   then Node := tvHistory.Items.AddChildObject(AParent, Action.Caption, Action)
   else Node := tvHistory.Items.AddObject(AParent, Action.Caption, Action);
  if Action is THistoryGroup then begin
   with THistoryGroup(Action) do
    for i:=0 to ActionCount-1 do AddAction(Actions[i], Node);
   Node.Expand(true);
  end;
 end;

var
 i: integer;

begin
 tvHistory.Items.BeginUpdate;
 try
  FInUpdate := true;
  tvHistory.Items.Clear;
  if not Assigned(FPanel) then exit;
  for i:=0 to FPanel.History.ActionCount-1 do
   AddAction(FPanel.History[i], Nil);
 finally
  tvHistory.Items.EndUpdate;
  FInUpdate := false;
 end;
 CheckTools;
 tvHistoryChange(tvHistory, tvHistory.Selected);
end;

procedure TfmHistoryDebug.tvHistoryCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var Index: integer;
begin
 if not Assigned(FPanel) then exit;
 Index := FPanel.History.IndexOf(THistoryAction(Node.Data));
 if (Index >= 0) and (FPanel.History.ActionIndex = Index) then
  tvHistory.Canvas.Font.Style := [fsBold];
 DefaultDraw := true;
end;

procedure TfmHistoryDebug.acHistoryUndoExecute(Sender: TObject);
begin
 if not Assigned(FPanel) then exit;
 FPanel.History.Undo;
 tvHistory.Invalidate;
 tvHistoryChange(tvHistory, tvHistory.Selected);
 CheckTools;
end;

procedure TfmHistoryDebug.acHistoryRedoExecute(Sender: TObject);
begin
 FPanel.History.Redo;
 tvHistory.Invalidate;
 tvHistoryChange(tvHistory, tvHistory.Selected);
 CheckTools;
end;

procedure TfmHistoryDebug.CheckTools;
begin
{ acHistoryUndo.Enabled := fpMain.History.ActionIndex >= 0;
 acHistoryRedo.Enabled :=
   fpMain.History.ActionIndex < fpMain.History.ActionCount - 1;}
end;

procedure TfmHistoryDebug.tvHistoryChange(Sender: TObject; Node: TTreeNode);
var Action: THistoryAction;
    Memo: TMemo;

 procedure AddOrderInfo(const Info: TOrderHistoryActionInfo; Memo: TMemo);
 begin
  Memo.Clear;
  if not Info.Exist then exit;
  with Info do begin
   Memo.Lines.Add(Format('Index = %d', [Index]));
   Memo.Lines.Add(Format('ParentId = %d', [ParentId]));
   Memo.Lines.Add(Format('LayerId = %d', [LayerId]));
  end;
 end;

 procedure LoadStream(Stream: TStream; Memo: TMemo);
 var List: TStringList;
     i: integer;
 begin
  List := TStringList.Create;
  try
   Stream.Position := 0;
   List.LoadFromStream(Stream);
   for i:=0 to List.Count-1 do
    Memo.Lines.Add(List[i]);
  finally
   List.Free;
  end;
 end;

 procedure LoadPoints(const Info: TPointsHistoryActionInfo; Memo: TMemo);
 var i: integer;
     PtType: string;
 begin
  with Info do begin
   if not Exist then exit;
   Memo.Lines.Add(Format('DocRect: (%d, %d, %d, %d)',
    [DocRect.Left, DocRect.Top, DocRect.Right, DocRect.Bottom]));
   for i:=0 to Length(Info.Points)-1 do begin
    case Info.Types[i] of
     ptNode         : PtType := 'ptNode';
     ptEndNode      : PtType := 'ptEndNode';
     ptEndNodeClose : PtType := 'ptEndNodeClose';
     ptControl      : PtType := 'ptControl';
     else             PtType := '?';
    end;
    Memo.Lines.Add(Format('(%d, %d) - %s',
      [Info.Points[i].X, Info.Points[i].Y, PtType]));
   end;
  end;
 end;
 
begin
 mmUndo.Clear;
 mmRedo.Clear;
 if FInUpdate then exit;
 if not Assigned(Node) then exit;
 Action := TObject(Node.Data) as THistoryAction;
 if Action is TIdHistoryAction then
 with TIdHistoryAction(Action) do begin
  mmUndo.Lines.Add(Format('SourceId = %d', [SourceId]));
  mmRedo.Lines.Add(Format('SourceId = %d', [SourceId]));
 end;
 if Action is TOrderHistoryAction then
 with TOrderHistoryAction(Action) do begin
  AddOrderInfo(UndoInfo, mmUndo);
  AddOrderInfo(RedoInfo, mmRedo);
 end;
 if Action is THistoryStreamAction then
 with TCustomControlHistoryAction(Action) do begin
  if Assigned(UndoStream) then LoadStream(UndoStream, mmUndo);
  if Assigned(RedoStream) then LoadStream(RedoStream, mmRedo);
 end;
 if Action is TCreateDestroyHistoryGroup then
 with TCreateDestroyHistoryGroup(Action) do begin
  if Action is TControlCreateHistoryGroup
   then Memo := mmRedo
   else Memo := mmUndo;
  Memo.Lines.Text :=
    Format(
      'SourceId = %d'#13#10 +
      'ParentId = %d'#13#10 +
      'ParentIndex = %d', [SourceId, ParentId, ParentIndex]);
  if Assigned(Stream) then LoadStream(Stream, Memo);
 end;
 if Action is TDocRectHistoryAction then
 with TDocRectHistoryAction(Action) do begin
  if UndoExist then with UndoDocRect do
   mmUndo.Lines.Add(Format('DocRect: (%d, %d, %d, %d)',
    [Left, Top, Right, Bottom]));
  if RedoExist then with RedoDocRect do
   mmRedo.Lines.Add(Format('DocRect: (%d, %d, %d, %d)',
    [Left, Top, Right, Bottom]));    
 end;
 if Action is TPointsHistoryAction then
 with TPointsHistoryAction(Action) do begin
  LoadPoints(UndoInfo, mmUndo);
  LoadPoints(RedoInfo, mmRedo);
 end;
end;

procedure TfmHistoryDebug.fpMainNotify(Sender: TObject; Control: TFlexControl;
  Notify: TFlexNotify);
begin
{ if csDestroying in ComponentState then exit;
 if Notify = fnSelect then
  if fpMain.SelectedCount = 1
   then sbrMain.SimpleText := Format('ID: %d', [fpMain.Selected[0].ID])
   else sbrMain.SimpleText := ''; }
end;

end.
