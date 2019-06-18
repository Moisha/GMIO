/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    History actions and groups                       //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexActions;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes,
  FlexBase, FlexProps, FlexHistory, FlexPath, FlexUtils;

type
  TDocRectHistoryAction = class(TIdHistoryAction)
  protected
   FUndoDocRect: TRect;
   FRedoDocRect: TRect;
   FUndoExist: boolean;
   FRedoExist: boolean;
   class function DoIsRecordable(Source: TObject;
     Parent: THistoryGroup): boolean; override;
   procedure DoRecordAction(Source: TObject); override;
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
  public
   procedure OffsetUndo(const Diff: TPoint);
   property  UndoDocRect: TRect read FUndoDocRect write FUndoDocRect;
   property  RedoDocRect: TRect read FRedoDocRect write FRedoDocRect;
   property  UndoExist: boolean read FUndoExist write FUndoExist;
   property  RedoExist: boolean read FRedoExist write FRedoExist;
  end;

  TPointsHistoryActionInfo = record
   Exist: boolean;
   DocRect: TRect;
   Points: TPointArray;
   Types: TPointTypeArray;
  end;

  TPointsHistoryAction = class(TIdHistoryAction)
  protected
   FUndo: TPointsHistoryActionInfo;
   FRedo: TPointsHistoryActionInfo;
   class function DoIsRecordable(Source: TObject;
     Parent: THistoryGroup): boolean; override;
   procedure Save(var Info: TPointsHistoryActionInfo; Control: TFlexControl);
   procedure Restore(var Info: TPointsHistoryActionInfo; Control: TFlexControl);
   procedure DoRecordAction(Source: TObject); override;
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
  public
   procedure OffsetUndo(const Diff: TPoint);
   procedure SetUndoSavedDocRect(Control: TFlexControl);
   property  UndoInfo: TPointsHistoryActionInfo read FUndo write FUndo;
   property  RedoInfo: TPointsHistoryActionInfo read FRedo write FRedo;
  end;

  TOrderHistoryActionInfo = record
   Exist: boolean;
   Index: integer;
   ParentId: LongWord;
   LayerId: LongWord;
  end;

  TOrderHistoryAction = class(TIdHistoryAction)
  protected
   FUndo: TOrderHistoryActionInfo;
   FRedo: TOrderHistoryActionInfo;
   procedure Save(var Info: TOrderHistoryActionInfo; Control: TFlexControl);
   procedure Restore(var Info: TOrderHistoryActionInfo; Control: TFlexControl;
     CurIndex: integer);
   procedure DoRecordAction(Source: TObject); override;
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
  public
   property UndoInfo: TOrderHistoryActionInfo read FUndo;
   property RedoInfo: TOrderHistoryActionInfo read FRedo;
  end;

  TGroupUngroupHistoryAction = class(TIdHistoryAction)
  protected
   FSourcePanel: TFlexPanel;
   FGroupClass: TFlexGroupClass;
   FSelected: TList;
   procedure SelectControls;
   procedure Save(Source: TFlexGroup);
  public
   destructor Destroy; override;
  end;

  TControlGroupHistoryAction = class(TGroupUngroupHistoryAction)
  protected
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
   procedure DoRecordAction(Source: TObject); override;
  end;

  TControlUngroupHistoryAction = class(TGroupUngroupHistoryAction)
  protected
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
   procedure DoRecordAction(Source: TObject); override;
  end;

  TCustomControlHistoryAction = class(TIdHistoryAction)
  protected
   function  ProcessSource(Stream: TStream; Source: TObject;
     DoLoad: boolean): boolean; override;
  public
   class function RecordAction(
     Source: TFlexControl): TCustomControlHistoryAction;
  end;

  TCreateDestroyHistoryGroup = class(THistoryGroup)
  protected
   FStream: TMemoryStream;
   FSourcePanel: TFlexPanel;
   FSourceId: LongWord;
   FParentId: LongWord;
   FParentIndex: integer;
   class function DoIsRecordable(Source: TObject;
     Parent: THistoryGroup): boolean; override;
   procedure SaveSource(Source: TObject);
   function  ProcessSource(Source: TFlexControl; DoLoad: boolean): boolean;
  public
   destructor Destroy; override;
   property  SourcePanel: TFlexPanel read FSourcePanel write FSourcePanel;
   property  SourceId: LongWord read FSourceId write FSourceId;
   property  ParentId: LongWord read FParentId write FParentId;
   property  ParentIndex: integer read FParentIndex write FParentIndex;
   property  Stream: TMemoryStream read FStream;
  end;

  TControlCreateHistoryGroup = class(TCreateDestroyHistoryGroup)
  protected
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
   procedure DoRecordAction(Source: TObject); override;
  end;

  TControlDestroyHistoryGroup = class(TCreateDestroyHistoryGroup)
  protected
   procedure DoUndo(Source: TObject); override;
   procedure DoRedo(Source: TObject); override;
   procedure DoRecordAction(Source: TObject); override;
  end;

  TFlexPanelHistoryGroup = class(THistoryGroup)
  protected
   FSourcePanel: TFlexPanel;
  public
   constructor Create(AOwner: THistory; AParent: THistoryGroup); override;
   property  SourcePanel: TFlexPanel read FSourcePanel write FSourcePanel;
  end;

  TPanelSizeMoveHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelAlignHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelCreateControlsHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelDestroyControlsHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelUngroupControlsHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelDuplicateHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelCloneHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelOrderHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelPointsHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelRerouteHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelColorHistoryGroup = class(TFlexPanelHistoryGroup);
  TPanelPropsHistoryGroup = class(TFlexPanelHistoryGroup);

implementation

uses
  FlexControls;

type
  TFlexControlOpen = class(TFlexControl);
  TFlexCurveOpen = class(TFlexCurve);

// TDocRectHistoryAction //////////////////////////////////////////////////////

class function TDocRectHistoryAction.DoIsRecordable(Source: TObject;
  Parent: THistoryGroup): boolean;
var i: integer;
begin
 Result := (Source is TFlexControl) and
  ( (TFlexControl(Source).UpdateCounter > 0) or
    (fsEndUpdating in TFlexControl(Source).State) );
 // Check TPointsHistoryAction existance
 if not Result or not Assigned(Parent) then exit;
 for i:=0 to Parent.ActionCount-1 do
  if (Parent[i] is TPointsHistoryAction) and
     (TPointsHistoryAction(Parent[i]).SourceId =
      TFlexControl(Source).IdProp.Value) then begin
   Result := false;
   break;
  end;
end;

procedure TDocRectHistoryAction.DoRecordAction(Source: TObject);
begin
 FUndoDocRect := TFlexControlOpen(Source).FSavedDocRect;
 FUndoExist := true;
 with FUndoDocRect do
  FDestroyAfterEnd := (Left = 0) and (Top = 0) and (Right <= 1) and (Bottom <= 1);
end;

procedure TDocRectHistoryAction.DoRedo(Source: TObject);
begin
 if not FRedoExist or not (Source is TFlexControl) then exit;
 if Source is TFlexConnector then TFlexConnector(Source).BeginLinkUpdate;
 try
  TFlexControl(Source).DocRect := FRedoDocRect;
 finally
  if Source is TFlexConnector then TFlexConnector(Source).EndLinkUpdate;
 end;
end;

procedure TDocRectHistoryAction.DoUndo(Source: TObject);
begin
 if not FUndoExist or not (Source is TFlexControl) then exit;
 if not FRedoExist then begin
  FRedoDocRect := TFlexControl(Source).DocRect;
  FRedoExist := true;
 end;
 if Source is TFlexConnector then TFlexConnector(Source).BeginLinkUpdate;
 try
  TFlexControl(Source).DocRect := FUndoDocRect;
 finally
  if Source is TFlexConnector then TFlexConnector(Source).EndLinkUpdate;
 end;
end;

procedure TDocRectHistoryAction.OffsetUndo(const Diff: TPoint);
begin
 if FUndoExist then OffsetRect(FUndoDocRect, Diff.X, Diff.Y);
end;

// TOrderHistoryAction ////////////////////////////////////////////////////////

procedure TOrderHistoryAction.Restore(var Info: TOrderHistoryActionInfo;
  Control: TFlexControl; CurIndex: integer);
begin
 // Restore parent if need
 if Info.ParentId <> 0 then begin
  if not Assigned(Control.Parent) or
    (Info.ParentId <> Control.Parent.IdProp.Value) then
   Control.Parent := Control.Owner.FindControlByID(Info.ParentId);
 end else
  Control.Parent := Nil;
 // Restore layer if need
 if Info.LayerId <> 0 then begin
  if not Assigned(Control.Layer) or
     (Info.LayerId <> Control.Layer.IdProp.Value) then
   Control.Layer := TFlexLayer(
     Control.Owner.FindControlByID(Info.LayerId, Control.Owner.Layers))
 end else
  Control.Layer := Nil;
 // Restore order
 if Assigned(Control.Parent) then begin
  if CurIndex < 0 then CurIndex := Control.Parent.IndexOf(Control);
  if CurIndex >= 0 then Control.Parent.ChangeOrder(CurIndex, Info.Index);
 end;
end;

procedure TOrderHistoryAction.Save(var Info: TOrderHistoryActionInfo;
  Control: TFlexControl);
begin
 if Assigned(Control.Parent)
  then Info.ParentId := Control.Parent.IdProp.Value
  else Info.ParentId := 0;
 if Assigned(Control.Layer)
  then Info.LayerId := Control.Layer.IdProp.Value
  else Info.LayerId := 0;
 if Assigned(Control.Parent)
  then Info.Index := Control.Parent.IndexOf(Control)
  else Info.Index := -1;
 Info.Exist := true;
end;

procedure TOrderHistoryAction.DoRecordAction(Source: TObject);
begin
 if not (Source is TFlexControl) {or
    not Assigned(TFlexControl(Source).Parent)} then exit;
 // Create undo
 Save(FUndo, TFlexControl(Source));
end;

procedure TOrderHistoryAction.DoUndo(Source: TObject);
begin
 if not (Source is TFlexControl) or not FUndo.Exist then exit;
 // Create redo
 Save(FRedo, TFlexControl(Source));
 // Undo
 Restore(FUndo, TFlexControl(Source), FRedo.Index);
end;

procedure TOrderHistoryAction.DoRedo(Source: TObject);
begin
 if not (Source is TFlexControl) or not FRedo.Exist then exit;
 // Redo
 Restore(FRedo, TFlexControl(Source), FUndo.Index);
end;

// TPointsHistoryAction ///////////////////////////////////////////////////////

procedure TPointsHistoryAction.DoRecordAction(Source: TObject);
begin
 if not (Source is TFlexControl) then exit;
 // Create undo
 Save(FUndo, TFlexControl(Source));
end;

procedure TPointsHistoryAction.DoRedo(Source: TObject);
begin
 if not (Source is TFlexControl) or not FRedo.Exist then exit;
 // Redo
 Restore(FRedo, TFlexControl(Source));
end;

procedure TPointsHistoryAction.DoUndo(Source: TObject);
begin
 if not (Source is TFlexControl) or not FUndo.Exist then exit;
 // Create redo
 Save(FRedo, TFlexControl(Source));
 // Undo
 Restore(FUndo, TFlexControl(Source));
end;

procedure TPointsHistoryAction.Restore(var Info: TPointsHistoryActionInfo;
  Control: TFlexControl);
begin
 if not Info.Exist then exit;
 if Control is TFlexConnector then TFlexConnector(Control).BeginLinkUpdate;
 try
  Control.DocRect := Info.DocRect;
  Control.SetPointsEx(Info.Points, Info.Types);
 finally
  if Control is TFlexConnector then TFlexConnector(Control).EndLinkUpdate;
 end;
end;

class function TPointsHistoryAction.DoIsRecordable(Source: TObject;
  Parent: THistoryGroup): boolean;
begin
 Result :=
   (inherited DoIsRecordable(Source, Parent)) and not
   ( (Source is TFlexEllipse) and
     not TFlexEllipse(Source).IsNeedHistoryPointsAction );
end;

procedure TPointsHistoryAction.Save(var Info: TPointsHistoryActionInfo;
  Control: TFlexControl);
var i: integer;
begin
 if Info.Exist then exit;
 Control.GetPointsEx(Info.Points, Info.Types);
 if fsResizing in Control.State then begin
  with TFlexControlOpen(Control) do begin
   Info.DocRect := FResizingRect;
   OffsetRect(Info.DocRect, FResizingTopLeft.X, FResizingTopLeft.Y);
  end;
  if Control is TFlexCurve then with TFlexCurveOpen(Control) do begin
   SetLength(Info.Points, Length(FResizePoints));
   for i:=0 to Length(Info.Points)-1 do Info.Points[i] := FResizePoints[i];
  end;
 end else
  Info.DocRect := Control.DocRect;
  //else Info.DocRect := TFlexControlOpen(Control).FSavedDocRect;
 Info.Exist := true;
end;

procedure TPointsHistoryAction.OffsetUndo(const Diff: TPoint);
begin
 if FUndo.Exist then OffsetRect(FUndo.DocRect, Diff.X, Diff.Y);
end;

procedure TPointsHistoryAction.SetUndoSavedDocRect(Control: TFlexControl);
begin
 if FUndo.Exist and not (fsResizing in Control.State) then
  FUndo.DocRect := TFlexControlOpen(Control).FSavedDocRect;
end;

// TGroupUngroupHistoryAction /////////////////////////////////////////////////

destructor TGroupUngroupHistoryAction.Destroy;
begin
 inherited;
 FSelected.Free;
end;

procedure TGroupUngroupHistoryAction.Save(Source: TFlexGroup);
var i: integer;
begin
 if not Assigned(Source) or not Assigned(Source.Owner) then exit;
 FGroupClass := TFlexGroupClass(Source.ClassType);
 FSourcePanel := Source.Owner;
 FSourceId := Source.IdProp.Value;
 if not Assigned(FSelected)
  then FSelected := TList.Create
  else FSelected.Clear;
 // Store child controls
 for i:=0 to Source.Count-1 do
  FSelected.Add(pointer(Source[i].IdProp.Value));
end;

procedure TGroupUngroupHistoryAction.SelectControls;
var i: integer;
    Control: TFlexControl;
begin
 FSourcePanel.UnselectAll;
 for i:=0 to FSelected.Count-1 do begin
  Control := FSourcePanel.FindControlByID(LongWord(FSelected[i]));
  if Assigned(Control) then FSourcePanel.Select(Control);
 end;
end;

// TControlGroupHistoryAction /////////////////////////////////////////////////

procedure TControlGroupHistoryAction.DoRecordAction(Source: TObject);
begin
 if not (Source is TFlexGroup) or
    not Assigned(TFlexGroup(Source).Owner) then exit;
 // Init group operation
 Save(TFlexGroup(Source));
end;

procedure TControlGroupHistoryAction.DoRedo(Source: TObject);
var GroupControl: TFlexGroup;
begin
 if not Assigned(FSourcePanel) then exit;
 // Group
 SelectControls;
 GroupControl := FSourcePanel.Group(FGroupClass);
 if Assigned(GroupControl) then GroupControl.IdProp.Value := FSourceId;
end;

procedure TControlGroupHistoryAction.DoUndo(Source: TObject);
begin
 if not Assigned(FSourcePanel) or
    not (Source is TFlexGroup) or
    (TFlexGroup(Source).Owner <> FSourcePanel) then exit;
 // Ungroup
 with FSourcePanel do begin
  UnselectAll;
  Select(TFlexGroup(Source));
  Ungroup;
 end;
end;

// TControlUngroupHistoryAction ///////////////////////////////////////////////

procedure TControlUngroupHistoryAction.DoRecordAction(Source: TObject);
begin
 if not (Source is TFlexGroup) or
    not Assigned(TFlexGroup(Source).Owner) then exit;
 // Init ungroup operation
 Save(TFlexGroup(Source));
end;

procedure TControlUngroupHistoryAction.DoRedo(Source: TObject);
begin
 if not Assigned(FSourcePanel) or
    not (Source is TFlexGroup) or
    (TFlexGroup(Source).Owner <> FSourcePanel) then exit;
 // Ungroup
 with FSourcePanel do begin
  UnselectAll;
  Select(TFlexGroup(Source));
  Ungroup;
 end;
end;

procedure TControlUngroupHistoryAction.DoUndo(Source: TObject);
var GroupControl: TFlexGroup;
begin
 if not Assigned(FSourcePanel) then exit;
 // Group
 SelectControls;
 GroupControl := FSourcePanel.Group(FGroupClass);
 if Assigned(GroupControl) then GroupControl.IdProp.Value := FSourceId;
end;

// TCustomControlHistoryAction ////////////////////////////////////////////////

class function TCustomControlHistoryAction.RecordAction(
  Source: TFlexControl): TCustomControlHistoryAction;
var History: THistory;
begin
 Result := Nil;
 if not Assigned(Source) or not Assigned(Source.Owner) then exit;
 History := Source.Owner.History;
 Result := TCustomControlHistoryAction(History.RecordAction(Self, Source));
end;

function TCustomControlHistoryAction.ProcessSource(Stream: TStream;
  Source: TObject; DoLoad: boolean): boolean;
var Filer: TFlexFiler;
    Control: TFlexControl;
    Props: TPropList;
    FirstStr: string;
begin
 Result := false;
 if not (Source is TFlexControl) then exit;
 Control := TFlexControl(Source);
 Props := Control.Props;
 Filer := Nil;
 // Set PropList History state (to skip read-only checking in load phase)
 Props.HistoryStateForList := Owner.State;
 try
  // Create filer
  Filer := TFlexFiler.Create(Stream);
  if DoLoad then begin
   if Control.Owner.IsLoading then Props.RefList := Control.Owner.PropRefList;
   // Load control properties
   while Filer.LoadStrCheck(FirstStr) do
    Props.LoadProperty(Filer, FirstStr);
   Result := true;
  end else
   // Save control properties
   Result := Props.SaveToFiler(Filer, '', true);
 finally
  Props.HistoryStateForList := hsIdle;
  Props.RefList := Nil;
  Filer.Free;
 end;
end;

// TCreateDestroyHistoryGroup /////////////////////////////////////////////////

destructor TCreateDestroyHistoryGroup.Destroy;
begin
  FStream.Free();
  inherited;
end;

class function TCreateDestroyHistoryGroup.DoIsRecordable(Source: TObject;
  Parent: THistoryGroup): boolean;
begin
 Result := Assigned(Source) and (Source is TFlexControl) and
  not (Assigned(Parent) and (Parent is TCreateDestroyHistoryGroup));
 if Result then
  Result := inherited DoIsRecordable(Source, Parent);
end;

procedure TCreateDestroyHistoryGroup.SaveSource(Source: TObject);
var Control: TFlexControl;
begin
 if not Assigned(Source) or not (Source is TFlexControl) then exit;
 Control := TFlexControl(Source);
 FSourcePanel := Control.Owner;
 FSourceId := Control.IdProp.Value;
 if Assigned(Control.Parent) then begin
  FParentId := Control.Parent.IdProp.Value;
  FParentIndex := Control.Parent.IndexOf(Control);
 end else begin
  FParentId := 0;
  FParentIndex := -1;
 end;
end;

function TCreateDestroyHistoryGroup.ProcessSource(Source: TFlexControl;
  DoLoad: boolean): boolean;
var Filer: TFlexFiler;
    ParentControl: TFlexControl;
    NewControl: TFlexControl;
    CurIndex: integer;
begin
 Result := false;
 // Create stream
 if DoLoad then begin
  if not Assigned(FStream) then exit;
 end else
 if not Assigned(FStream)
  then FStream := TMemoryStream.Create
  else FStream.Size := 0;
 // Create filer
 Filer := TFlexFiler.Create(FStream);
 try
  if DoLoad then begin
   // Create and load flex control
   FStream.Position := 0;
   if FParentId <> 0
    then ParentControl := FSourcePanel.FindControlByID(FParentId)
    else ParentControl := Nil;
   NewControl :=
     SourcePanel.LoadFlexControl(Filer, ParentControl, Filer.LoadStr, true);
   if (FParentIndex >= 0) and Assigned(ParentControl) then begin
    CurIndex := ParentControl.IndexOf(NewControl);
    ParentControl.ChangeOrder(CurIndex, FParentIndex);
   end;
   Result := Assigned(NewControl);
  end else begin
   // Save flex control
   Result := Assigned(Source);
   if not Result then exit;
   Source.SaveToFiler(Filer, '');
  end;
 finally
  Filer.Free;
 end;
end;

// TControlCreateHistoryGroup /////////////////////////////////////////////////

procedure TControlCreateHistoryGroup.DoRecordAction(Source: TObject);
begin
 SaveSource(Source);
end;

procedure TControlCreateHistoryGroup.DoRedo(Source: TObject);
begin
 // Recreate source control
 ProcessSource(Nil, True);
end;

procedure TControlCreateHistoryGroup.DoUndo(Source: TObject);
begin
 if not (Source is TFlexControl) then exit;
 // Save control for redo
 ProcessSource(TFlexControl(Source), False);
 // Destroy source
 Source.Free;
end;

// TControlDestroyHistoryGroup ////////////////////////////////////////////////

procedure TControlDestroyHistoryGroup.DoRecordAction(Source: TObject);
begin
 SaveSource(Source);
 // Save control for Undo
 ProcessSource(TFlexControl(Source), False);
end;

procedure TControlDestroyHistoryGroup.DoRedo(Source: TObject);
begin
 if not (Source is TFlexControl) then exit;
 // Just destroy source
 Source.Free;
end;

procedure TControlDestroyHistoryGroup.DoUndo(Source: TObject);
begin
 // Recreate source control
 ProcessSource(Nil, True);
end;

// TFlexPanelHistoryGroup /////////////////////////////////////////////////////

constructor TFlexPanelHistoryGroup.Create(AOwner: THistory;
  AParent: THistoryGroup);
begin
 inherited;
 FDestroyIfEmpty := true;
end;

///////////////////////////////////////////////////////////////////////////////

procedure RegisterHistoryActions;
begin
 // Control actions
 THistory.RegisterAction(TDocRectHistoryAction);
 THistory.RegisterAction(TOrderHistoryAction);
 THistory.RegisterAction(TPointsHistoryAction);
 THistory.RegisterAction(TControlCreateHistoryGroup);
 THistory.RegisterAction(TControlDestroyHistoryGroup);
 THistory.RegisterAction(TControlGroupHistoryAction);
 THistory.RegisterAction(TControlUngroupHistoryAction);
 // Panel actions
 THistory.RegisterAction(TPanelSizeMoveHistoryGroup);
 THistory.RegisterAction(TPanelAlignHistoryGroup);
 THistory.RegisterAction(TPanelCreateControlsHistoryGroup);
 THistory.RegisterAction(TPanelDestroyControlsHistoryGroup);
 THistory.RegisterAction(TPanelUngroupControlsHistoryGroup);
 THistory.RegisterAction(TPanelDuplicateHistoryGroup);
 THistory.RegisterAction(TPanelCloneHistoryGroup);
 THistory.RegisterAction(TPanelOrderHistoryGroup);
 THistory.RegisterAction(TPanelPointsHistoryGroup);
 THistory.RegisterAction(TPanelRerouteHistoryGroup);
 THistory.RegisterAction(TPanelColorHistoryGroup);
 THistory.RegisterAction(TPanelPropsHistoryGroup);
end;

initialization
  RegisterHistoryActions;

end.
