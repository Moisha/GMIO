/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    History (undo/redo) base classes                 //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexHistory;

{$I FlexDefs.inc}

interface

uses
  Classes;

const
  DefaultHistoryDepth = 100; // actions

type
  THistory = class;
  THistoryGroup = class;

  THistoryActionClass = class of THistoryAction;

  THistoryState = ( hsIdle, hsRecord, hsUndo, hsRedo );

  THistoryAction = class
  private
   FOwner: THistory;
   FParent: THistoryGroup;
   FCaption: string;
   FTag: integer;
  protected
   FState: THistoryState;
   FDestroyAfterEnd: boolean;
   procedure ChangeState(NewState: THistoryState); virtual;
   function  BeginState(NewState: THistoryState): boolean; virtual;
   function  EndState: boolean; virtual;
   class function DoIsRecordable(Source: TObject;
     Parent: THistoryGroup): boolean; virtual;
   function  DoIsSame(ActionClass: THistoryActionClass;
     Source: TObject): boolean; virtual;
   procedure DoActionCaption(Source: TObject; var ACaption: string); virtual;
   procedure DoBeginAction(Source: TObject); virtual;
   procedure DoEndAction; virtual;
   procedure DoRecordAction(Source: TObject); virtual; abstract;
   procedure DoUndo(Source: TObject); virtual; abstract;
   procedure DoRedo(Source: TObject); virtual; abstract;
  public
   constructor Create(AOwner: THistory; AParent: THistoryGroup); virtual;
   property  Owner: THistory read FOwner;
   property  Caption: string read FCaption write FCaption;
   property  State: THistoryState read FState;
   property  Parent: THistoryGroup read FParent;
   property  DestroyAfterEnd: boolean read FDestroyAfterEnd;
   property  Tag: integer read FTag write FTag;
  end;

  THistoryStreamAction = class(THistoryAction)
  protected
   FUndoStream: TStream;
   FRedoStream: TStream;
   function  ProcessSource(Stream: TStream; Source: TObject;
     DoLoad: boolean): boolean; virtual; abstract;
   procedure DoRecordAction(Source: TObject); override;
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
  public
   destructor Destroy; override;
   property  UndoStream: TStream read FUndoStream;
   property  RedoStream: TStream read FRedoStream;
  end;

  THistoryGroupClass = class of THistoryGroup;

  THistoryGroup = class(THistoryAction)
  private
   FActions: TList;
   FInProcessIndex: integer;
   FInProcessSource: TObject;
   function  GetAction(Index: integer): THistoryAction;
   function  GetActionCount: integer;
  protected
   FDestroyIfEmpty: boolean;
   procedure Clear;
   procedure DoEndAction; override;
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
   procedure DoRecordAction(Source: TObject); override;
   function  BeginAction(ActionClass: THistoryActionClass;
     const Source: TObject; ATag: integer = 0): THistoryAction;
   function  EndAction: boolean;
   function  EraseAction: boolean;
  public
   constructor Create(AOwner: THistory; AParent: THistoryGroup); override;
   destructor Destroy; override;
   property  ActionCount: integer read GetActionCount;
   property  Actions[Index: integer]: THistoryAction read GetAction; default;
   property  InProcessIndex: integer read FInProcessIndex;
   property  InProcessSource: TObject read FInProcessSource;
  end;

  THistoryGetActionSourceEvent = procedure(Sender: TObject;
    Action: THistoryAction; var Source: TObject;
    var Enabled: boolean) of object;

  THistorySetActionSourceEvent = procedure(Sender: TObject;
    Action: THistoryAction; const Source: TObject) of object;

  THistoryGetActionCaptionEvent = procedure(Sender: TObject;
    Action: THistoryAction; const Source: TObject;
    var Caption: string) of object;

  THistoryIsRecordableEvent = procedure(Sender: TObject;
    var ActionClass: THistoryActionClass; var Source: TObject;
    Parent: THistoryGroup; var IsRecordable: boolean) of object;

  THistoryIsSameEvent = procedure(Sender: TObject;
    ExistedAction: THistoryAction; NewActionClass: THistoryActionClass;
    const NewSource: TObject; var IsSame: boolean) of object;

  THistory = class
  private
   FOwner: TObject;
   FActive: boolean;
   FActions: TList;
   FActionIndex: integer;
   FDepth: integer;
   FOnChange: TNotifyEvent;
   FOnGetActionCaption: THistoryGetActionCaptionEvent;
   FOnGetActionSource: THistoryGetActionSourceEvent;
   FOnSetActionSource: THistorySetActionSourceEvent;
   FOnIsRecordable: THistoryIsRecordableEvent;
   FOnIsSame: THistoryIsSameEvent;
   function  GetInProcess: boolean;
   function  GetInProcessAction: THistoryAction;
   function  GetInProcessSource: TObject;
   function  GetIsRecordable: boolean;
   function  GetCaption(Index: integer): string;
   function  GetAction(Index: integer): THistoryAction;
   function  GetActionCount: integer;
   procedure SetDepth(const Value: integer);
   procedure SetActive(const Value: boolean);
  protected
   FGroup: THistoryGroup;
   FGroupLevel: integer;
   FDisableLevel: integer;
   FState: THistoryState;
   FInProcessSource: TObject;   
   procedure ChangeState(NewState: THistoryState); virtual;
   procedure DeleteActions(FromIndex: integer = 0; ToIndex: integer = -1);
   function  Scroll: integer;
   procedure DoChange; virtual;
   function  DoGetActionSource(Action: THistoryAction;
     var Enabled: boolean): TObject; virtual;
   procedure DoSetActionSource(Action: THistoryAction;
     const Source: TObject); virtual;
   function  DoGetActionCaption(Action: THistoryAction;
     const Source: TObject): string; virtual;
   function  DoIsRecordable(var ActionClass: THistoryActionClass;
     var Source: TObject; Parent: THistoryGroup): boolean; virtual;
   function  DoIsSame(ExistedAction: THistoryAction;
     NewActionClass: THistoryActionClass; NewSource: TObject): boolean; virtual;
  public
   constructor Create(AOwner: TObject);
   destructor Destroy; override;
   class procedure RegisterAction(Action: THistoryActionClass);
   procedure Clear;
   function  IndexOf(Action: THistoryAction): integer;
   function  BeginAction(ActionClass: THistoryActionClass;
     Source: TObject; ATag: integer = 0;
     DoRecord: boolean = true): THistoryAction;
   function  EndAction: boolean;
   function  CancelAction: boolean;
   function  RecordAction(ActionClass: THistoryActionClass;
     const Source: TObject; ATag: integer = 0): THistoryAction;
   function  EraseAction: boolean;
   function  OpenLastGroup: THistoryGroup;
   function  Undo: boolean; virtual;
   function  Redo: boolean; virtual;
   procedure DisableRecording; 
   function  EnableRecording: boolean;
   property  Active: boolean read FActive write SetActive default false;
   property  Owner: TObject read FOwner;
   property  State: THistoryState read FState;
   property  Depth: integer read FDepth write SetDepth
     default DefaultHistoryDepth;
   property  DisableLevel: integer read FDisableLevel;
   property  GroupLevel: integer read FGroupLevel;
   property  ActionCount: integer read GetActionCount;
   property  Actions[Index: integer]: THistoryAction read GetAction; default;
   property  ActionIndex: integer read FActionIndex;
   property  IsRecordable: boolean read GetIsRecordable;
   property  InProcess: boolean read GetInProcess;
   property  InProcessAction: THistoryAction read GetInProcessAction;
   property  InProcessSource: TObject read GetInProcessSource;
   property  Captions[Index: integer]: string read GetCaption;
   property  OnGetActionCaption: THistoryGetActionCaptionEvent
     read FOnGetActionCaption write FOnGetActionCaption;
   property  OnGetActionSource: THistoryGetActionSourceEvent
     read FOnGetActionSource write FOnGetActionSource;
   property  OnSetActionSource: THistorySetActionSourceEvent
     read FOnSetActionSource write FOnSetActionSource;
   property  OnIsRecordable: THistoryIsRecordableEvent read FOnIsRecordable
     write FOnIsRecordable;
   property  OnIsSame: THistoryIsSameEvent read FOnIsSame write FOnIsSame;
   property  OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

var
  HistoryActionClasses: TList;

// THistoryAction /////////////////////////////////////////////////////////////

constructor THistoryAction.Create(AOwner: THistory;
  AParent: THistoryGroup);
begin
 FOwner := AOwner;
 FParent := AParent;
end;

procedure THistoryAction.ChangeState(NewState: THistoryState);
begin
 FState := NewState;
end;

function THistoryAction.BeginState(NewState: THistoryState): boolean;
begin
 Result := false;
 if (FState = NewState) or
    ((NewState in [hsRecord, hsUndo, hsRedo]) and (FState <> hsIdle)) then exit;
 ChangeState(NewState);
 Result := true;
end;

function THistoryAction.EndState: boolean;
begin
 Result := false;
 if FState = hsIdle then exit;
 ChangeState(hsIdle);
 Result := true;
end;

function THistoryAction.DoIsSame(ActionClass: THistoryActionClass;
  Source: TObject): boolean;
begin
 Result := false;
end;

class function THistoryAction.DoIsRecordable(Source: TObject;
  Parent: THistoryGroup): boolean;
begin
 Result := true;
end;

procedure THistoryAction.DoBeginAction(Source: TObject);
begin
 if FState <> hsIdle then exit;
 ChangeState(hsRecord);
end;

procedure THistoryAction.DoEndAction;
begin
 if FState = hsRecord then ChangeState(hsIdle);
end;

procedure THistoryAction.DoActionCaption(Source: TObject; var ACaption: string);
begin
 // Abstract
end;

// THistoryStreamAction ///////////////////////////////////////////////////////

destructor THistoryStreamAction.Destroy;
begin
 inherited;
 FUndoStream.Free;
 FRedoStream.Free;
end;

procedure THistoryStreamAction.DoRedo(Source: TObject);
begin
 if Assigned(FRedoStream) and BeginState(hsRedo) then
 try
  // Do redo
  FRedoStream.Position := 0;
  ProcessSource(FRedoStream, Source, true);
 finally
  EndState;
 end;
end;

procedure THistoryStreamAction.DoUndo(Source: TObject);
begin
 if Assigned(FUndoStream) and BeginState(hsUndo) then
 try
  // Create redo stream
  if Assigned(FRedoStream)
   then FRedoStream.Size := 0
   else FRedoStream := TMemoryStream.Create;
  // Save current value (for redo)
  ProcessSource(FRedoStream, Source, false);
  // Do undo
  FUndoStream.Position := 0;
  ProcessSource(FUndoStream, Source, true);
 finally
  EndState;
 end;
end;

procedure THistoryStreamAction.DoRecordAction(Source: TObject);
begin
 if State <> hsRecord then exit;
 // Create undo stream
 if Assigned(FUndoStream)
  then FUndoStream.Size := 0
  else FUndoStream := TMemoryStream.Create;
 // Save current value (for undo)
 ProcessSource(FUndoStream, Source, false);
end;

// THistoryGroup //////////////////////////////////////////////////////////////

constructor THistoryGroup.Create(AOwner: THistory;
  AParent: THistoryGroup);
begin
 inherited;
 FActions := TList.Create;
 FInProcessIndex := -1;
end;

destructor THistoryGroup.Destroy;
begin
 inherited;
 Clear;
 FActions.Free;
end;

function THistoryGroup.GetAction(Index: integer): THistoryAction;
begin
 Result := THistoryAction(FActions[Index]);
end;

function THistoryGroup.GetActionCount: integer;
begin
 Result := FActions.Count;
end;

procedure THistoryGroup.Clear;
var i: integer;
begin
 for i:=FActions.Count-1 downto 0 do THistoryAction(FActions[i]).Free;
 FActions.Clear;
end;

function THistoryGroup.BeginAction(ActionClass: THistoryActionClass;
  const Source: TObject; ATag: integer = 0): THistoryAction;
var i: integer;
begin
 Result := Nil;
 if HistoryActionClasses.IndexOf(ActionClass) < 0 then exit;
 // Check action alredy exist
 for i:=0 to FActions.Count-1 do
  if Owner.DoIsSame(THistoryAction(FActions[i]), ActionClass, Source) then exit;
 // Create action
 Result := ActionClass.Create(Owner, Self);
 try
  // Just add to list
  FActions.Add(Result);
 except
  Result.Free;
  raise;
 end;
 Result.Tag := ATag; 
end;

function THistoryGroup.EndAction: boolean;
var Action: THistoryAction;
begin
 if FActions.Count = 0
  then Action := Nil
  else Action := THistoryAction(FActions[FActions.Count-1]);
 if Assigned(Action) and (Action.State = hsRecord) then begin
  Action.DoEndAction;
  if Action.DestroyAfterEnd then EraseAction;
 end else
  DoEndAction; // Self end action
 Result := true;
end;

function THistoryGroup.EraseAction: boolean;
var Action: THistoryAction;
begin
 Result := FActions.Count > 0;
 if not Result then exit;
 Action := THistoryAction(FActions[FActions.Count-1]);
 FActions.Delete(FActions.Count-1);
 Action.Free;
 FOwner.DoChange;
end;

procedure THistoryGroup.DoRecordAction(Source: TObject);
begin
 // Save nothing
end;

procedure THistoryGroup.DoEndAction;
begin
 inherited;
 FDestroyAfterEnd := FDestroyIfEmpty and (FActions.Count = 0);
end;

procedure THistoryGroup.DoUndo(Source: TObject);
var i: integer;
    Enabled: boolean;
begin
 if FState <> hsIdle then exit;
 ChangeState(hsUndo);
 try
  for i:=FActions.Count-1 downto 0 do begin
   FInProcessIndex := i;  
   FInProcessSource :=
     FOwner.DoGetActionSource(THistoryAction(FActions[i]), Enabled);
   if Enabled then THistoryAction(FActions[i]).DoUndo(FInProcessSource);
  end;
 finally
  ChangeState(hsIdle);
  FInProcessIndex := -1;
  FInProcessSource := Nil;
 end;
end;

procedure THistoryGroup.DoRedo(Source: TObject);
var i: integer;
    Enabled: boolean;
begin
 if FState <> hsIdle then exit;
 ChangeState(hsRedo);
 try
  for i:=0 to FActions.Count-1 do begin
   FInProcessIndex := i;
   FInProcessSource :=
     FOwner.DoGetActionSource(THistoryAction(FActions[i]), Enabled);
   if Enabled then THistoryAction(FActions[i]).DoRedo(FInProcessSource);
  end;
 finally
  ChangeState(hsIdle);
  FInProcessIndex := -1;
  FInProcessSource := Nil;  
 end;
end;

// THistory ///////////////////////////////////////////////////////////////////

constructor THistory.Create(AOwner: TObject);
begin
 FOwner := AOwner;
 FActions := TList.Create;
 FActionIndex := -1;
 FDepth := DefaultHistoryDepth;
 FState := hsIdle;
end;

destructor THistory.Destroy;
begin
 inherited;
 DeleteActions;
 FActions.Free;
end;

class procedure THistory.RegisterAction(Action: THistoryActionClass);
begin
 if Assigned(Action) and (HistoryActionClasses.IndexOf(Action) < 0) then
  HistoryActionClasses.Add(Action);
end;

function THistory.GetAction(Index: integer): THistoryAction;
begin
 Result := THistoryAction(FActions[Index]);
end;

function THistory.GetActionCount: integer;
begin
 Result := FActions.Count;
end;

function THistory.GetCaption(Index: integer): string;
begin
 Result := THistoryAction(FActions[Index]).Caption;
end;

procedure THistory.SetDepth(const Value: integer);
begin
 if Value = FDepth then exit;
 FDepth := Value;
 if FActions.Count > FDepth + 1 then begin
  // Delete all out of range actions
  DeleteActions(FDepth + 1);
  DoChange;
 end;
end;

procedure THistory.SetActive(const Value: boolean);
begin
 if (Value = FActive) or (FState <> hsIdle) then exit;
 FActive := Value;
 if not FActive then begin
  // Delete all actions
  DeleteActions;
  DoChange;
 end;
end;

procedure THistory.ChangeState(NewState: THistoryState);
begin
 FState := NewState;
end;

function THistory.DoGetActionCaption(Action: THistoryAction;
  const Source: TObject): string;
begin
 Result := '';
 Action.DoActionCaption(Source, Result);
 if Assigned(FOnGetActionCaption) then
  FOnGetActionCaption(Self, Action, Source, Result);
end;

procedure THistory.DoChange;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

function THistory.DoGetActionSource(Action: THistoryAction;
  var Enabled: boolean): TObject;
begin
 Result := Nil;
 if Assigned(FOnGetActionSource) then begin
  Enabled := true;
  FOnGetActionSource(Self, Action, Result, Enabled);
 end else
  Enabled := false;
end;

procedure THistory.DoSetActionSource(Action: THistoryAction;
  const Source: TObject);
begin
 if Assigned(FOnSetActionSource) then FOnSetActionSource(Self, Action, Source);
end;

function THistory.DoIsRecordable(var ActionClass: THistoryActionClass;
  var Source: TObject; Parent: THistoryGroup): boolean;
begin
 Result := ActionClass.DoIsRecordable(Source, Parent);
  // HistoryActionClasses.IndexOf(ActionClass) >= 0;
 if Result and Assigned(FOnIsRecordable) then
  FOnIsRecordable(Self, ActionClass, Source, Parent, Result);
end;

function THistory.DoIsSame(ExistedAction: THistoryAction;
  NewActionClass: THistoryActionClass; NewSource: TObject): boolean;
begin
 Result := ExistedAction.DoIsSame(NewActionClass, NewSource);
 if Assigned(FOnIsSame) then
  FOnIsSame(Self, ExistedAction, NewActionClass, NewSource, Result);
end;

procedure THistory.Clear;
begin
 if FActions.Count > 0 then begin
  DeleteActions;
  DoChange;
 end;
end;

procedure THistory.DeleteActions(FromIndex: integer = 0;
  ToIndex: integer = -1);
var i: integer;
    NeedDel: boolean;
begin
 if FromIndex < 0 then FromIndex := 0;
 if ToIndex < 0 then ToIndex := FActions.Count + ToIndex;
 NeedDel := (FromIndex > 0) or (ToIndex < FActions.Count-1);
 for i:=ToIndex downto FromIndex do begin
  THistoryAction(FActions[i]).Free;
  if NeedDel then FActions.Delete(i);
 end;
 if not NeedDel then FActions.Clear;
 if FActionIndex >= FActions.Count then FActionIndex := FActions.Count - 1;
end;

function THistory.IndexOf(Action: THistoryAction): integer;
begin
 Result := FActions.IndexOf(Action);
end;

function THistory.Scroll: integer;
begin
 if FActions.Count <= FDepth then
  Result := 0
 else begin
  Result := FActions.Count - FDepth;
  // Delete first out of range actions
  DeleteActions(0, Result-1);
 end;
end;

function THistory.BeginAction(ActionClass: THistoryActionClass;
  Source: TObject; ATag: integer = 0; DoRecord: boolean = true): THistoryAction;
var Action: THistoryAction;
    NewIndex: integer;
    ParentGroup: THistoryGroup;
begin
 Result := Nil;
 if not FActive or (FDisableLevel > 0) or
    not (FState in [hsIdle, hsRecord]) or
    not DoIsRecordable(ActionClass, Source, FGroup) then exit;
 // Check insert action in current group
 if Assigned(FGroup) then begin
  Result := FGroup.BeginAction(ActionClass, Source);
 end else begin
  if (HistoryActionClasses.IndexOf(ActionClass) < 0) then exit;
  Action := ActionClass.Create(Self, Nil);
  try
   // Put action in list
   NewIndex := FActionIndex + 1;
   // Delete all following actions
   if NewIndex < FActions.Count then DeleteActions(NewIndex);
   // Add to end
   FActions.Add(Action);
  except
   Action.Free;
   raise;
  end;
  Result := Action;
  Result.Tag := ATag;
  // Scroll action list if count bigger then Depth
  dec(NewIndex, Scroll);
  FActionIndex := NewIndex;
  // Set state
  ChangeState(hsRecord);
 end;
 if Assigned(Result) then begin
  // Encode source in action
  DoSetActionSource(Result, Source);
  // Set action caption
  Result.Caption := DoGetActionCaption(Result, Source);
  // Check action is a group
  ParentGroup := FGroup;
  if Result is THistoryGroup then begin
   FGroup := THistoryGroup(Result);
   inc(FGroupLevel);
  end;
  // Begin action
  Result.DoBeginAction(Source);
  if Result.State = hsRecord then begin
   // Record action
   if DoRecord then Result.DoRecordAction(Source);
   // List changed
   DoChange;
  end else begin
   // Action not in Record state. Erase action
   if Assigned(ParentGroup) then begin
    ParentGroup.EraseAction;
    if FGroup <> ParentGroup then begin
     FGroup := ParentGroup;
     dec(FGroupLevel);
    end;
   end else
    EraseAction;
   Result := Nil;
  end;
 end;
end;

function THistory.EndAction: boolean;
var Action: THistoryAction;
    NeedErase: boolean;
begin
 Result := false;
 if not FActive or (FDisableLevel > 0) or
    (FActionIndex < 0) or (FState <> hsRecord) then exit;
 if Assigned(FGroup) then begin
  Result := FGroup.EndAction;
  if FGroup.State = hsIdle then begin
   // Check self destroying
   NeedErase := FGroup.DestroyAfterEnd;
   // Up one level
   FGroup := FGroup.Parent;
   dec(FGroupLevel);
   // Erase if need
   if NeedErase then
    if Assigned(FGroup)
     then FGroup.EraseAction
     else EraseAction;
  end;
  // Check state
  if not Assigned(FGroup) then ChangeState(hsIdle);
 end else begin
  Action := THistoryAction(FActions[FActionIndex]);
  if Action.State = hsRecord then begin
   Action.DoEndAction;
   Result := true;
  end;
  // Set state
  if (Action.State = hsIdle) or Action.DestroyAfterEnd then
   ChangeState(hsIdle);
  // Check destroying
  if Action.DestroyAfterEnd then
   EraseAction;
 end;
end;

function THistory.CancelAction: boolean;
var Action: THistoryAction;
    ActionParent: THistoryGroup;
begin
 Result := false;
 if not FActive or (FDisableLevel > 0) then exit;
 if Assigned(FGroup) then begin
  if FGroup.ActionCount > 0
   then Action := FGroup.Actions[FGroup.ActionCount-1]
   else Action := FGroup;
 end else
 if FActions.Count > 0
  then Action := THistoryAction(FActions[FActions.Count-1])
  else Action := Nil;
 if not Assigned(Action) or (Action.State <> hsRecord) then exit;
 ActionParent := Action.Parent;
 if Assigned(ActionParent)
  then Result := ActionParent.EraseAction
  else Result := EraseAction;
 if Result and (FGroup = Action) then begin
  FGroup := ActionParent;
  dec(FGroupLevel);
 end;
end;

function THistory.Redo: boolean;
var Action: THistoryAction;
begin
 Result := false;
 if (FState <> hsIdle) or (FActionIndex = FActions.Count-1) then exit;
 ChangeState(hsRedo);
 try
  Action := THistoryAction(FActions[FActionIndex + 1]);
  FInProcessSource := DoGetActionSource(Action, Result);
  if not Result then exit;
  Action.DoRedo(FInProcessSource);
  inc(FActionIndex);
 finally
  FInProcessSource := Nil;
  ChangeState(hsIdle);
 end;
 Result := true;
end;

function THistory.Undo: boolean;
var Action: THistoryAction;
begin
 Result := false;
 if (FState <> hsIdle) or (FActionIndex < 0) then exit;
 ChangeState(hsUndo);
 try
  Action := THistoryAction(FActions[FActionIndex]);
  FInProcessSource := DoGetActionSource(Action, Result);
  if not Result then exit;
  Action.DoUndo(FInProcessSource);
  dec(FActionIndex);
 finally
  FInProcessSource := Nil;
  ChangeState(hsIdle);
 end;
 Result := true;
end;

function THistory.EraseAction: boolean;
begin
 Result := FActive and (FActions.Count > 0);
 if not Result then exit;
 DeleteActions(FActions.Count - 1);
 DoChange;
end;

function THistory.RecordAction(ActionClass: THistoryActionClass;
  const Source: TObject; ATag: integer = 0): THistoryAction;
begin
 Result := BeginAction(ActionClass, Source, ATag);
 if Assigned(Result) then EndAction;
end;

procedure THistory.DisableRecording;
begin
 inc(FDisableLevel);
end;

function THistory.EnableRecording: boolean;
begin
 Result := FDisableLevel = 0;
 if Result then exit;
 dec(FDisableLevel);
end;

function THistory.OpenLastGroup: THistoryGroup;
var Action: THistoryAction;
    NewGroup: THistoryGroup;
begin
 Result := Nil;
 NewGroup := Nil;
 if (FState <> hsIdle) or (FDisableLevel > 0) then exit;
 if Assigned(FGroup) then begin
  if (FGroup.ActionCount = 0) or (FGroup.State <> hsIdle) or not
     (FGroup[FGroup.ActionCount-1] is THistoryGroup) then exit;
  NewGroup := THistoryGroup(FGroup[FGroup.ActionCount-1]);
 end else
 if FActions.Count > 0 then begin
  Action := THistoryAction(FActions[FActions.Count-1]);
  if not (Action is THistoryGroup) or (Action.State <> hsIdle) then exit;
  NewGroup := THistoryGroup(Action);
 end;
 // Change group
 if not Assigned(NewGroup) then exit;
 NewGroup.ChangeState(hsRecord);
 if NewGroup.State <> hsRecord then exit;
 ChangeState(hsRecord);
 FGroup := NewGroup;
 Result := FGroup;
 inc(FGroupLevel);
 // List changed
 DoChange;
end;

function THistory.GetInProcessAction: THistoryAction;
var Group: THistoryGroup;
    Index: integer;
begin
 Result := Nil;
 if (FState <> hsUndo) and (FState <> hsRedo) then exit;
 Index := FActionIndex;
 if FState = hsRedo then inc(Index);
 Result := THistoryAction(FActions[Index]);
 if Result is THistoryGroup then begin
  Group := THistoryGroup(Result);
  while Assigned(Group) do begin
   if Group.InProcessIndex < 0 then break;
   Result := Group[Group.InProcessIndex];
   if Result is THistoryGroup
    then Group := THistoryGroup(Result)
    else Group := Nil;
  end;
 end;
end;

function THistory.GetInProcessSource: TObject;
var Action: THistoryAction;
    Group: THistoryGroup;
    Index: integer;
begin
 Result := Nil;
 if (FState <> hsUndo) and (FState <> hsRedo) then exit;
 Index := FActionIndex;
 if FState = hsRedo then inc(Index);
 Action := THistoryAction(FActions[Index]);
 if Action is THistoryGroup then begin
  Group := THistoryGroup(Action);
  while Assigned(Group) do begin
   if Group.InProcessIndex < 0 then break;
   Action := Group[Group.InProcessIndex];
   if Action is THistoryGroup
    then Group := THistoryGroup(Action)
    else break;
  end;
 end else
  Group := Nil;
 if Assigned(Group)
  then Result := Group.InProcessSource
  else Result := FInProcessSource;
end;

function THistory.GetIsRecordable: boolean;
begin
 Result := FActive and (FState in [hsIdle, hsRecord]);
end;

function THistory.GetInProcess: boolean;
begin
 Result := FState in [hsUndo, hsRedo];
end;

initialization
  HistoryActionClasses := TList.Create;
  // THistory.RegisterAction(THistoryAction); - Abstract class
  THistory.RegisterAction(THistoryGroup);

finalization
  HistoryActionClasses.Free;

end.
