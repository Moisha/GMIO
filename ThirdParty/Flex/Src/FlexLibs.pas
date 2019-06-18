/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    Flex-libraries support                           //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexLibs;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes, Controls, SysUtils, Graphics,
  FlexBase, FlexProps, FlexUtils;

type
  TLibItemAnchorProp = class(TCustomProp)
  private
   function  GetAsPoint: TPoint;
   procedure SetAsPoint(const Value: TPoint);
   procedure SetEnabled(const Value: boolean);
   procedure SetPosX(const Value: integer);
   procedure SetPosY(const Value: integer);
  protected
   FEnabled: boolean;
   FPosX: integer;
   FPosY: integer;
   function  GetDisplayValue: string; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  AsPoint: TPoint read GetAsPoint write SetAsPoint;
  published
   property  Enabled: boolean read FEnabled write SetEnabled default False;
   property  PosX: integer read FPosX write SetPosX default 0;
   property  PosY: integer read FPosY write SetPosY default 0;
  end;

  TFlexLibItem = class(TFlexControl)
  protected
   FAnchorProp: TLibItemAnchorProp;
   procedure CreateProperties; override;
   procedure ControlCreate; override;
  public
   function  CreateDragObject: TFlexDragObject;
   function  MakeCopy(AFlex: TFlexPanel; ALeft, ATop: integer): boolean;
   property  AnchorProp: TLibItemAnchorProp read FAnchorProp;
  end;

  TFlexLibraryClass = class of TFlexLibrary;

  TFlexLibrary = class(TFlexCustomScheme)
  private
   FFilename: string;
   FModified: boolean;
   function  GetByName(const Name: string): TFlexLibItem;
   function  GetLibItem(Index: integer): TFlexLibItem;
  protected
   procedure CreateProperties; override;
   procedure ControlCreate; override;
  public
   function  New: TFlexLibItem; virtual;
   function  NewFromFlex(AFlex: TFlexPanel): TFlexLibItem;
   function  Add(AControl: TFlexControl): integer; override;
   procedure Delete(Index: integer); override;
   procedure SaveToFiler(Filer: TFlexFiler; const Indent: string); override;
   procedure LoadFromFiler(Filer: TFlexFiler); override;
   property  Controls[Index: integer]: TFlexLibItem read GetLibItem; default;
   property  ByName[const Name: string]: TFlexLibItem read GetByName;
   property  LibFilename: string read FFilename write FFilename;
   property  Modified: boolean read FModified write FModified;
  end;

  TFlexLibraryWithIDs = class(TFlexLibrary)
  protected
   procedure CreateProperties; override;
  public
   function  Add(AControl: TFlexControl): integer; override;
  end;

  TFlexLibraries = class
  private
   FFlex: TFlexPanel;
   FDragSource: TFlexPanel;
   FOnChange: TNotifyEvent;
   function GetCount: integer;
   function GetItem(Index: integer): TFlexLibrary;
  protected
   procedure DoOnChange; virtual;
   property  DragSource: TFlexPanel read FDragSource;
  public
   constructor Create;
   destructor Destroy; override;
   function  New(const Filename: string;
     LibClass: TFlexLibraryClass = Nil): TFlexLibrary; virtual;
   function  IndexOf(ALibrary: TFlexLibrary): integer;
   procedure Remove(ALibrary: TFlexLibrary);
   function  SaveLibrary(ALibrary: TFlexLibrary): boolean;
   function  LoadLibrary(ALibrary: TFlexLibrary): boolean;
   property  Flex: TFlexPanel read FFlex;
   property  Count: integer read GetCount;
   property  Items[Index: integer]: TFlexLibrary read GetItem; default;
   property  OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // Libraries with IDs classes ///////////////////////////////////////////////

  TFlexSingleLibraries = class;
  TFlexSingleLibraryClass = class of TFlexSingleLibrary;

  TFlexSingleLibrary = class
  private
   FOwner: TFlexSingleLibraries;
   FOnChange: TNotifyEvent;
  protected
   FId: LongWord;
   FFlex: TFlexPanel;
   FLibrary: TFlexLibrary;
   FDragSource: TFlexPanel;
   procedure DoOnChange; virtual;
   property  Flex: TFlexPanel read FFlex;
   property  DragSource: TFlexPanel read FDragSource;
  public
   constructor Create(AOwner: TFlexSingleLibraries;
     LibClass: TFlexLibraryClass = Nil); virtual;
   destructor Destroy; override;
   function  SaveLibrary: boolean;
   function  LoadLibrary: boolean;
   property  Owner: TFlexSingleLibraries read FOwner;
   property  Id: LongWord read FId;
   property  FlexPanel: TFlexPanel read FFlex;
   property  FlexLibrary: TFlexLibrary read FLibrary;
   property  OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFlexSingleLibraries = class
  private
   function  GetCount: integer;
   function  GetLibrary(Index: Integer): TFlexSingleLibrary;
  protected
   FList: TList;
   FMaxId: LongWord;
   function  InternalAdd(Item: TFlexSingleLibrary): integer;
   procedure InternalDelete(Index: integer);
  public
   constructor Create;
   destructor Destroy; override;
   procedure Clear;
   function  New(AClass: TFlexSingleLibraryClass = Nil;
     LibClass: TFlexLibraryClass = Nil): TFlexSingleLibrary; virtual;
   procedure Delete(Index: integer);
   function  IndexOf(Item: TFlexSingleLibrary): integer;
   function  FindByName(const FlexPanelName: string): integer;
   property  Count: integer read GetCount;
   property  Items[Index: Integer]: TFlexSingleLibrary read GetLibrary; default;
  end;

var
  FlexLibraries: TFlexLibraries;

implementation

// TLibItemAnchorProp /////////////////////////////////////////////////////////

constructor TLibItemAnchorProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FPropType := ptComplex;
end;

function TLibItemAnchorProp.GetAsPoint: TPoint;
begin
 Result.X := FPosX;
 Result.Y := FPosY;
end;

function TLibItemAnchorProp.GetDisplayValue: string;
begin
 Result := '(Anchor)';
end;

procedure TLibItemAnchorProp.SetAsPoint(const Value: TPoint);
begin
 if ((Value.X = FPosX) and (Value.Y = FPosY)) or Owner.IsReadOnly(Self) then
  exit;
 DoBeforeChanged;
 FPosX := Value.X;
 FPosY := Value.Y;
 DoChanged;
end;

procedure TLibItemAnchorProp.SetEnabled(const Value: boolean);
begin
 if (Value = FEnabled) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FEnabled := Value;
 DoChanged;
end;

procedure TLibItemAnchorProp.SetPosX(const Value: integer);
begin
 if (Value = FPosX) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FPosX := Value;
 DoChanged;
end;

procedure TLibItemAnchorProp.SetPosY(const Value: integer);
begin
 if (Value = FPosY) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FPosY := Value;
 DoChanged; 
end;

// TFlexLibItem //////////////////////////////////////////////////////////////

procedure TFlexLibItem.ControlCreate;
begin
 NonVisual := true;
 ShowHintProp.Value := True;
 if Assigned(Owner) then begin
  Name := Owner.GetDefaultNewName(Self, Parent);
  Owner.GenerateID(Self);
 end;
 //inherited;
 Visible := true;
 DoNotify(fnCreated);
end;

procedure TFlexLibItem.CreateProperties;
const HideSet: TPropStyle = [ psReadOnly, psDontStore, psNonVisual ];
begin
 inherited;
 LeftProp.Style := HideSet;
 TopProp.Style := HideSet;
 IdProp.Style := HideSet;
 ShowHintProp.Style := HideSet;
 LayerProp.Style := HideSet;
 ReferenceProp.Style := HideSet;
 FAnchorProp := TLibItemAnchorProp.Create(Props, 'Anchor');
end;

function TFlexLibItem.CreateDragObject: TFlexDragObject;
begin
 Result := Owner.CreateDragObject(Self, True, False);
end;

function TFlexLibItem.MakeCopy(AFlex: TFlexPanel;
  ALeft, ATop: integer): boolean;
var Drag: TFlexDragObject;
    DragGroup: TFlexGroup;
begin
 // Create drag group (for extract library item controls)
 Drag := Owner.CreateDragObject(Self, True, False);
 try
  DragGroup := Drag.DragGroup;
  Drag.DragGroup := Nil;
 finally
  Drag.Free;
 end;
 // Move DragGroup to fpView panel
 DragGroup.Owner := AFlex;
 DragGroup.Parent := AFlex.ActiveScheme;
 DragGroup.Layer := AFlex.ActiveLayer;
 // Set DragGroup position
 DragGroup.Left := ALeft;
 DragGroup.Top := ATop;
 // Ungroup
 AFlex.UnselectAll;
 AFlex.Select(DragGroup);
 AFlex.Ungroup;                
 // Success
 Result := true;
end;

// TFlexLibrary //////////////////////////////////////////////////////////////

procedure TFlexLibrary.ControlCreate;
begin
 NonVisual := true;
 ShowHintProp.Value := True;
 inherited;
end;

procedure TFlexLibrary.CreateProperties;
const HideSet: TPropStyle = [ psReadOnly, psDontStore, psNonVisual ];
begin
 inherited;
 ShowHintProp.Style := HideSet;
 IdProp.Style := HideSet;
 ReferenceProp.Style := HideSet;
end;

function TFlexLibrary.Add(AControl: TFlexControl): integer;
var PassRec: TPassControlRec;
begin
 Result := inherited Add(AControl);
 if Result >= 0 then begin
  FirstControl(AControl, PassRec);
  try
   while Assigned(AControl) do begin
    AControl.Reference := Nil;
    AControl := NextControl(PassRec);
   end;
  finally
   ClosePassRec(PassRec);
  end;
  FModified := true;
 end;
end;

procedure TFlexLibrary.Delete(Index: integer);
begin
 inherited;
 FModified := true;
end;

function TFlexLibrary.GetByName(const Name: string): TFlexLibItem;
begin
 Result := TFlexLibItem( inherited ByName[Name] );
end;

function TFlexLibrary.GetLibItem(Index: integer): TFlexLibItem;
begin
 Result := TFlexLibItem( inherited Controls[Index] );
end;

procedure TFlexLibrary.LoadFromFiler(Filer: TFlexFiler);
var s: string;
    i: integer;
begin
 s := Filer.LoadStr;
 if StrBeginsFrom(s, fcBinary) then s := Filer.LoadStr;
 if not StrBeginsFrom(s, fcLibrary) then
  raise Exception.Create('Data format error');
 i := Pos(' ', s);
 if i > 0 then
  Name := Trim(copy(s, i+1, MaxInt));
 inherited;
end;

function TFlexLibrary.New: TFlexLibItem;
begin
 Result := TFlexLibItem.Create(Owner, Self, Nil);
end;

function TFlexLibrary.NewFromFlex(AFlex: TFlexPanel): TFlexLibItem;
var MS: TMemoryStream;
    Filer: TFlexFiler;
    s: string;
    LoadOrigin: TPoint;
    Control: TFlexControl;
begin
 Result := New;
 if not Assigned(Result) then exit;
 try
  MS := Nil;
  Filer := Nil;
  try
   MS := TMemoryStream.Create;
   Filer := TFlexFiler.Create(MS);
   with AFlex.SelectedRange do begin
    LoadOrigin.X := -Left;
    LoadOrigin.Y := -Top;
    Result.Width := Right - Left;
    Result.Height := Bottom - Top;
   end;
   AFlex.SaveToFiler(Filer, True);
   Filer.Rewind;
   Owner.BeginLoading;
   try
    s := Filer.LoadStr;
    if not StrBeginsFrom(s, fcClipboard) then
     raise Exception.Create('Data format error');
    while Filer.LoadStrCheck(s) do begin
     if StrBeginsFrom(s, fcEnd) then
      break
     else
     if StrBeginsFrom(s, fcObject) then begin
      Control := Owner.LoadFlexControl(Filer, Result, s);
      if Assigned(Control) then begin
       Control.Left := Control.Left + LoadOrigin.X;
       Control.Top  := Control.Top  + LoadOrigin.Y;
      end;
     end else
      Filer.LoadSkipToEnd;
    end;
   finally
    Owner.EndLoading;
   end;
  finally
   Filer.Free;
   MS.Free;
  end;
 except
  Result.Free;
  raise;
 end;
end;

procedure TFlexLibrary.SaveToFiler(Filer: TFlexFiler; const Indent: string);
var i: integer;
begin
 Filer.SaveStr(fcLibrary + ' ' + Name);
 Props.SaveToFiler(Filer, IndentStep);
 for i:=0 to Count-1 do
  Controls[i].SaveToFiler(Filer, IndentStep);
 Filer.SaveStr(fcEnd);
end;

// TFlexLibraries ////////////////////////////////////////////////////////////

constructor TFlexLibraries.Create;
begin
 inherited;
 FFlex := TFlexPanel.Create(Nil);
 FFlex.Name := 'FlexLibraries';
 FFlex.EmptyDocument;
 FFlex.ShowDocFrame := False;
end;

destructor TFlexLibraries.Destroy;
begin
 FFlex.Free;
 inherited;
end;

procedure TFlexLibraries.DoOnChange;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

function TFlexLibraries.GetCount: integer;
begin
 Result := FFlex.Schemes.Count;
end;

function TFlexLibraries.GetItem(Index: integer): TFlexLibrary;
begin
 Result := TFlexLibrary(FFlex.Schemes[Index]);
end;

function TFlexLibraries.New(const Filename: string;
  LibClass: TFlexLibraryClass = Nil): TFlexLibrary;
begin
 if Assigned(LibClass)
  then Result := LibClass.Create(FFlex, FFlex.Schemes, Nil)
  else Result := TFlexLibrary.Create(FFlex, FFlex.Schemes, Nil);
 if Filename <> '' then begin
  Result.LibFilename := Filename;
  if FileExists(Filename) then
   try
    LoadLibrary(Result);
   except
    Result.Free;
    raise;
   end
  else
   // Save empty library to the file
   try
    SaveLibrary(Result);
   except
    Result.Free;
    raise;
   end;
 end;
 DoOnChange;
end;

procedure TFlexLibraries.Remove(ALibrary: TFlexLibrary);
begin
 FFlex.Schemes.Remove(ALibrary);
 DoOnChange;
end;

function TFlexLibraries.IndexOf(ALibrary: TFlexLibrary): integer;
begin
 Result := FFlex.Schemes.IndexOf(ALibrary);
end;

function TFlexLibraries.LoadLibrary(ALibrary: TFlexLibrary): boolean;
var FS: TFileStream;
    Filer: TFlexFiler;
begin
 Result := False;
 if not Assigned(ALibrary) or (FFlex.Schemes.IndexOf(ALibrary) < 0) then exit;
 FS := Nil;
 Filer := Nil;
 try
  FS := TFileStream.Create(ALibrary.LibFilename,
    fmOpenRead or fmShareDenyWrite);
  Filer := TFlexFiler.Create(FS);
  ALibrary.LoadFromFiler(Filer);
  ALibrary.Modified := False;
  Result := true;
 finally
  Filer.Free;
  FS.Free;
 end;
end;

function TFlexLibraries.SaveLibrary(ALibrary: TFlexLibrary): boolean;
var FS: TFileStream;
    Filer: TFlexFiler;
begin
 Result := False;
 if not Assigned(ALibrary) or (FFlex.Schemes.IndexOf(ALibrary) < 0) then exit;
 FS := Nil;
 Filer := Nil;
 try
  FS := TFileStream.Create(ALibrary.LibFilename, fmCreate);
  Filer := TFlexFiler.Create(FS);
  ALibrary.SaveToFiler(Filer, '');
  ALibrary.Modified := False;
  Result := true;
 finally
  Filer.Free;
  FS.Free;
 end;
end;

// TFlexLibraryWithIDs ////////////////////////////////////////////////////////

procedure TFlexLibraryWithIDs.CreateProperties;
begin
 inherited;
 // Unlock ID property
 IdProp.Style := IdProp.Style - [psReadOnly, psDontStore];
end;

function TFlexLibraryWithIDs.Add(AControl: TFlexControl): integer;
begin
 Result := inherited Add(AControl);
 if Result < 0 then exit;
 // Generate ID for library item
 AControl.IdProp.Style := AControl.IdProp.Style - [psReadOnly, psDontStore];
 if AControl.IdProp.Value = 0 then Owner.GenerateID(AControl);
end;

// TFlexSingleLibrary /////////////////////////////////////////////////////////

constructor TFlexSingleLibrary.Create(AOwner: TFlexSingleLibraries;
  LibClass: TFlexLibraryClass = Nil);
begin
 FOwner := AOwner;
 FFlex := TFlexPanel.Create(Nil);
 FFlex.Name := 'FlexSingleLibrary';
 FFlex.EmptyDocument;
 FFlex.ShowDocFrame := False;
 if Assigned(LibClass)
  then FLibrary := LibClass.Create(FFlex, FFlex.Schemes, Nil)
  else FLibrary := TFlexLibraryWithIDs.Create(FFlex, FFlex.Schemes, Nil);
 if Assigned(FOwner) then FOwner.InternalAdd(Self);
end;

destructor TFlexSingleLibrary.Destroy;
begin
 if Assigned(FOwner) then FOwner.InternalDelete(FOwner.IndexOf(Self));
 FFlex.Free;
 inherited;
end;

procedure TFlexSingleLibrary.DoOnChange;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

function TFlexSingleLibrary.LoadLibrary: boolean;
var FS: TFileStream;
    Filer: TFlexFiler;
begin
 FS := Nil;
 Filer := Nil;
 try
  FS := TFileStream.Create(FLibrary.LibFilename,
    fmOpenRead or fmShareDenyWrite);
  Filer := TFlexFiler.Create(FS);
  FLibrary.LoadFromFiler(Filer);
  FLibrary.Modified := False;
  Result := true;
 finally
  Filer.Free;
  FS.Free;
 end;
end;

function TFlexSingleLibrary.SaveLibrary: boolean;
var FS: TFileStream;
    Filer: TFlexFiler;
begin
 FS := Nil;
 Filer := Nil;
 try
  FS := TFileStream.Create(FLibrary.LibFilename, fmCreate);
  Filer := TFlexFiler.Create(FS);
  FLibrary.SaveToFiler(Filer, '');
  FLibrary.Modified := False;
  Result := true;
 finally
  Filer.Free;
  FS.Free;
 end;
end;

///////////////////////////////////////////////////////////////////////////////

procedure RegisterControls;
begin
 RegisterFlexControl(TFlexLibItem);
end;

// TFlexSingleLibraries ///////////////////////////////////////////////////////

constructor TFlexSingleLibraries.Create;
begin
 FList := TList.Create;
end;

destructor TFlexSingleLibraries.Destroy;
begin
 Clear;
 FList.Free;
 inherited;
end;

procedure TFlexSingleLibraries.Clear;
begin
 while FList.Count > 0 do TFlexSingleLibrary(FList[FList.Count-1]).Free;
end;

function TFlexSingleLibraries.GetCount: integer;
begin
 Result := FList.Count;
end;

function TFlexSingleLibraries.GetLibrary(Index: Integer): TFlexSingleLibrary;
begin
 Result := TFlexSingleLibrary(FList[Index]);
end;

function TFlexSingleLibraries.IndexOf(Item: TFlexSingleLibrary): integer;
begin
 Result := FList.IndexOf(Item);
end;

function TFlexSingleLibraries.FindByName(const FlexPanelName: string): integer;
var i: integer;
begin
 Result := -1;
 for i:=0 to FList.Count-1 do
  if TFlexSingleLibrary(FList[i]).FFlex.Name = FlexPanelName then begin
   Result := i;
   break;
  end;
end;

function TFlexSingleLibraries.InternalAdd(Item: TFlexSingleLibrary): integer;
begin
 Result := -1;
 if not Assigned(Item) or (Item.Owner <> Self) then exit;
 Result := FList.IndexOf(Item);
 if Result >= 0 then exit;
 Result := FList.Add(Item);
 inc(FMaxId);
 Item.FId := FMaxId;
 Item.FFlex.Name := Format('FlexSingleLibrary%d', [Item.FId]);
end;

procedure TFlexSingleLibraries.InternalDelete(Index: integer);
begin
 FList.Delete(Index);
end;

function TFlexSingleLibraries.New(AClass: TFlexSingleLibraryClass = Nil;
  LibClass: TFlexLibraryClass = Nil): TFlexSingleLibrary;
begin
 if Assigned(AClass)
  then Result := AClass.Create(Self, LibClass)
  else Result := TFlexSingleLibrary.Create(Self, LibClass);
end;

procedure TFlexSingleLibraries.Delete(Index: integer);
begin
 TFlexSingleLibrary(FList[Index]).Free;
end;

initialization
  RegisterControls;
  FlexLibraries := TFlexLibraries.Create;

finalization
  FlexLibraries.Free;
  FlexLibraries := Nil;

end.
