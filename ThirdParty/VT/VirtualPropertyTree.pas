unit VirtualPropertyTree;

// Version 2.0
//
// The contents of this Package are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is Virtual Property Tree, first released on October 15, 2002.
//
// The initial developer of the original code is Carmi Grushko (sf_yourshadow@bezeqint.net)
//
// Updated by Tomasz Trejderowski (tomasz@trejderowski.pl)
//
//----------------------------------------------------------------------------------------------------------------------
//
// History :
// => 15/10/2002 - First release
//
// => 21/08/2008 - Version 2.0
// Changes:
// - Changed: MinValue to MinIntValue and MaxValue to MaxIntValue,
// - Introduced: MinExtValue and MaxExtValue for true extended range support,
// - Introduced: TPropertyTreeOptions class (PropertyTreeOptions group of properties
//   for managing basic PropertyTree Behaviour and for possible translation of errror
//   messages.
//
// To be Fixed (Urgently!):
// - AV exception in line 586 (DoPaintNode) when more than TWO field categories are
// used - I have COMPLETELY no idea what is wrong? :/  
//
//----------------------------------------------------------------------------------------------------------------------
//

interface

{$I Compilers.inc}

uses Windows, Messages, VirtualTrees, Classes, Controls, SysUtils, Graphics, StdCtrls,
     Forms, Buttons, Math, Dialogs, Mask, Types, UITypes;

type

  TDataType = ( dtInteger, dtString, dtCommonString, dtCombo, dtColor, dtExtended, dtButton, dtTime );
  TField = record
    Name: String;
    Category: String;

    IntData: integer;
    StringData: string;
    ExtendedData: Extended;
    //Data: Pointer;
    DataType: TDataType;
    ComboData: array of String; //dtCombo support
    ComboInt: array of Integer; //dtCombo support
    MinIntValue, MaxIntValue: Integer; //dtInteger support
    AllowNull: bool;
    MinExtValue, MaxExtValue: Extended; //true dtExtended range support - Tomasz Trejderowski
    // Мой креатив:
    // доступность редактирования
    Enabled: bool;
    // ссылка на внешний объект
    ExtObject: TObject;
    // для отображения на dtButton
    StringForUser: string;
    // маска редактирования
    EditMask: string;
    PasswordChar: char;

    procedure Init();
  end;
  PField = ^TField;

  TEditButtonClickEvt = procedure (Sender: TObject; Field: PField) of object;

  TCategory = record
    Name : string;
    Fields : array of PField;
  end;
  TCategories = array of TCategory;

  TVTPropertyEdit = class(TCustomMaskEdit)
  private
    FNode : PVirtualNode;
    ExecuteKillFocus : boolean;
    FOnDelBtnClick: TEditButtonClickEvt;

    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;

    procedure EndEdit;
    procedure UpdateParent;
  public
    constructor Create(AOwner : TComponent); override;
    procedure CancelEdit;
    procedure UpdateSize( ATree : TVirtualStringTree );
    property OnDelBtnClick: TEditButtonClickEvt read FOnDelBtnClick write FOnDelBtnClick;
  end;

  TVTPropertyCombo = class( TComboBox )
  private
    FNode : PVirtualNode;
  protected
    procedure CreateWnd; override;
    procedure Change; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure UpdateSize( ATree : TVirtualStringTree );
  end;

  TVTPropertyColor = class(TCustomControl)
  private
    FColor : TColor;

    FButton : TSpeedButton;
    FEdit : TVTPropertyEdit;

    procedure UpdateEdit; virtual;
    procedure MyButtonClick( Sender : TObject ); virtual;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure UpdateSize( ATree : TVirtualStringTree ); virtual;
    property InternalEdit: TVTPropertyEdit read FEdit;
  end;

  TVTPropertyButton = class(TVTPropertyColor)
  private
    FStringForUser : string;
    FValue: string;
    FOnBtnClick: TEditButtonClickEvt;

    procedure UpdateEdit; override;
    procedure MyButtonClick( Sender : TObject ); override;
  protected
    procedure Paint; override;
  public
    property OnBtnClick: TEditButtonClickEvt read FOnBtnClick write FOnBtnClick;
    procedure UpdateSize( ATree : TVirtualStringTree ); override;
  end;

  TVTPropertyEditWithButton = class(TVTPropertyButton)
  private
    procedure MyButtonClick( Sender : TObject ); override;
  public
    constructor Create(AOwner : TComponent); override;
  end;

  // A class to manage property tree options - Tomasz Trejderowski
  TPropertyTreeOptions = class(TPersistent)
  private
    FShowRangeError, FShowValidityError: Boolean;
    FRangeErrorText, FValidityErrorText, FErrorTitle: String;
    FOwner: TVirtualStringTree;
    FTimeErrorText: String;
    procedure SetShowRangeError(Value: Boolean);
    procedure SetShowValidityError(Value: Boolean);
    procedure SetRangeErrorText(Value: String);
    procedure SetValidityErrorText(Value: String);
    procedure SetErrorTitle(Value: String);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TVirtualStringTree);
    procedure Assign(Source: TPersistent); override;
  published
    //Whether to show error message about value not in range
    property ShowRangeError: Boolean read FShowRangeError write SetShowRangeError default True;
    //Whether to show error message about incorrect value
    property ShowValidityError: Boolean read FShowValidityError write SetShowValidityError default True;
    //Serie of options for translating error messages
    property RangeErrorText: String read FRangeErrorText write SetRangeErrorText;
    property ValidityErrorText: String read FValidityErrorText write SetValidityErrorText;
    property TimeErrorText: String read FTimeErrorText write FTimeErrorText;
    property ErrorTitle: String read FErrorTitle write SetErrorTitle;
  end;
  
  TVirtualPropertyTree = class( TVirtualStringTree )
  private
    FEdit : TVTPropertyEdit;
    FCombo : TVTPropertyCombo;
    FTridot : TVTPropertyColor;
    FButton : TVTPropertyButton;
    FEditButton : TVTPropertyEditWithButton;
    FCategories : TCategories;
    FPropertyTreeOptions: TPropertyTreeOptions;
    FOnPropertyChange: TEditButtonClickEvt;
    procedure SetPropertyTreeOptions(Value: TPropertyTreeOptions);
    function IndexOf(CategoryName: string): integer;
    function GetButtonIndex(Node: PVirtualNode): integer;
    procedure MoveEditor( Node : PVirtualNode );
    function SetFieldValue( Node : PVirtualNode; Text : string ) : boolean;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSize( var Message : TWMSize ); message WM_SIZE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure OnEditChange(Sender: TObject);
    procedure HideAllEditors;
  protected
{$ifndef COMPILER_17_UP}
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: WideString); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: WideString); override;
{$else}
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: UnicodeString); override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: UnicodeString); override;
{$endif}
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType); override;
    procedure DoAfterItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect); override;
    function DoPaintBackground(Canvas: TCanvas; R: TRect): Boolean; override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure DoChange(Node: PVirtualNode); override;
    procedure DblClick; override;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;

    procedure Loaded; override;

    procedure DetermineHitPositionLTR(var HitInfo: THitInfo; Offset, Right: Integer; Alignment: TAlignment); override;
  public
    Fields : array of TField;

    procedure Clear; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PostEdit;
    function GetNodeField( Node : PVirtualNode ) : PField;
    procedure RefreshFields;
    property Categories: TCategories read FCategories write FCategories;
    property ButtonEdit: TVTPropertyButton read FButton;
    property OnPropertyChange: TEditButtonClickEvt read FOnPropertyChange write FOnPropertyChange;
  published
    property PropertyTreeOptions: TPropertyTreeOptions read FPropertyTreeOptions write SetPropertyTreeOptions;
  end;

var
  EditorButtons : array[ 0..1 ] of TBitmap;

const
  ebDrop = 0;
  ebTridot = 1;

procedure Register;

{$R editor-buttons.res}

implementation

uses PropTxtEditor, GMGlobals;

var PropertyTextEditor: TPropertyTextEditor;


//TPropertyTreeOptions

procedure TPropertyTreeOptions.SetShowRangeError(Value: Boolean);
begin
        if FShowRangeError <> Value then FShowRangeError := Value;
end;

procedure TPropertyTreeOptions.SetShowValidityError(Value: Boolean);
begin
        if FShowValidityError <> Value then FShowValidityError := Value;
end;

procedure TPropertyTreeOptions.SetRangeErrorText(Value: String);
begin
        if FRangeErrorText <> Value then FRangeErrorText := Value;
end;

procedure TPropertyTreeOptions.SetValidityErrorText(Value: String);
begin
        if FValidityErrorText <> Value then FValidityErrorText := Value;
end;

procedure TPropertyTreeOptions.SetErrorTitle(Value: String);
begin
        if FErrorTitle <> Value then FErrorTitle := Value;
end;

function TPropertyTreeOptions.GetOwner: TPersistent;
begin
        Result := FOwner;
end;

constructor TPropertyTreeOptions.Create(AOwner: TVirtualStringTree);
begin
        inherited Create;
        FOwner := AOwner;
        FShowRangeError := True;
        FShowValidityError := True;
        FRangeErrorText := 'Значение должно быть в диапазоне %d - %d.';
        FValidityErrorText := 'Введите число ("%s" - не число).';
        FTimeErrorText := 'Введите время в формате чч:мм ("%s" - неверный формат).';
        FErrorTitle := 'Ошибка ...';
end;

procedure TPropertyTreeOptions.Assign(Source: TPersistent);
begin
        if Source is TPropertyTreeOptions then
        begin
                ShowRangeError := TPropertyTreeOptions(Source).ShowRangeError;
                ShowValidityError := TPropertyTreeOptions(Source).ShowValidityError;
                RangeErrorText := TPropertyTreeOptions(Source).RangeErrorText;
                ValidityErrorText := TPropertyTreeOptions(Source).ValidityErrorText;
                ErrorTitle := TPropertyTreeOptions(Source).ErrorTitle;
        end
        else
                inherited;
end;

//TVirtualPropertyTree

procedure TVirtualPropertyTree.SetPropertyTreeOptions(Value: TPropertyTreeOptions);
begin
        FPropertyTreeOptions.Assign(Value);
end;

function FieldToStr( Field : TField ) : string;
begin
{     with Field do
       case DataType of
         dtInteger:  Result := IntToStr( IntData );
         dtString:   Result := PString(Data)^;
         dtCombo:    Result := ComboData[ PInteger(Data)^ ];
         dtColor:    Result := Format( '%d, %d, %d', [
                                                      (PColor(Data)^ and $000000FF) shr 0, // Red
                                                      (PColor(Data)^ and $0000FF00) shr 8, // Green
                                                      (PColor(Data)^ and $00FF0000) shr 16 // Blue
                                                    ] );
         dtExtended: Result := FloatToStr( PExtended(Data)^ );
         dtButton:   Result := StringForUser;
       end;}

  if Field.DataType=dtButton then
    Result:=Field.StringForUser
  else
  begin
    if Field.PasswordChar <> #0 then
      Result:=StringOfChar(Field.PasswordChar, Length(Field.StringData))
    else
      Result:=Field.StringData;
  end;
end;

function MyStrToColor( Text : string ) : TColor;
var Values : array[0..2] of Byte;
    i : integer;
begin
     Text := Text + ',';

     for i := 0 to 2 do
     begin
          Values[i] := StrToInt( Copy(Text, 1, Pos(',', Text)-1 ) );
          Delete( Text, 1, Pos(',', Text) );
     end;

     Result := 0;
     Result := Result or Values[0] shl 0;
     Result := Result or Values[1] shl 8;
     Result := Result or Values[2] shl 16;
end;

function TVirtualPropertyTree.GetButtonIndex( Node : PVirtualNode ) : integer;
var DataType : TDataType;
begin
     Result := -1;
     if (Node^.Parent <> RootNode) then
     begin
          DataType := FCategories[Node^.Parent^.Index].Fields[Node^.Index]^.DataType;
          if DataType in [dtCombo, dtColor, dtButton] then
            Result := 0;
     end else
     begin
          Result := -2;
     end;
end;

procedure Register;
begin
     RegisterComponents( 'Virtual Controls', [TVirtualPropertyTree] );
end;

// If EditorButtons[Index] is nil, we have to load it from resources; not before.
function AccessEditorBitmap( Index : integer ) : TBitmap;
begin
     if EditorButtons[Index] = nil then
     begin
          EditorButtons[Index] := TBitmap.Create;
          EditorButtons[Index].LoadFromResourceID( HInstance, Index+1 );
     end;

     Result := EditorButtons[Index];
end;

{ TVirtualPropertyTree }

procedure TVirtualPropertyTree.OnEditChange(Sender: TObject);
begin

end;

constructor TVirtualPropertyTree.Create(AOwner: TComponent);
begin
     inherited;

     SetLength( Fields, 0 );
     SetLength( FCategories, 0 );

     // Editors
     FEdit := TVTPropertyEdit.Create( Self );
     with FEdit do
     begin
          Parent := Self;
          Hide;
          OnChange:=OnEditChange;
     end;

     FCombo := TVTPropertyCombo.Create( Self );
     with FCombo do
     begin
          Parent := Self;
          Hide;
     end;

     FTridot := TVTPropertyColor.Create( Self );
     with FTridot do
     begin
          Parent := Self;
          Hide;
     end;

     FButton := TVTPropertyButton.Create( Self );
     with FButton do
     begin
          Parent := Self;
          Hide;
          FEdit.OnChange:=OnEditChange;
     end;

     FEditButton := TVTPropertyEditWithButton.Create( Self );
     with FEditButton do
     begin
          Parent := Self;
          Hide;
          FEdit.OnChange:=OnEditChange;
     end;

     FPropertyTreeOptions := TPropertyTreeOptions.Create(Self);
end;

procedure TVirtualPropertyTree.DetermineHitPositionLTR(
  var HitInfo: THitInfo; Offset, Right: Integer; Alignment: TAlignment);
begin
     inherited;
     if (HitInfo.HitColumn = 0) and (Offset in [0..14]) then
     begin
          Include( HitInfo.HitPositions, hiOnItemButton );
     end;
     if (HitInfo.HitColumn = 1) and (Offset > Right-17) and (vsSelected in HitInfo.HitNode^.States) and
        (GetButtonIndex( HitInfo.HitNode ) > -1) then
        HitInfo.HitPositions := [hiOnItemCheckbox];
end;

procedure TVirtualPropertyTree.DoAfterItemPaint(Canvas: TCanvas;
  Node: PVirtualNode; ItemRect: TRect);
begin
     with Canvas do
     begin
          Brush.Color := clWindow;
          FillRect( Rect(0, ItemRect.Top, 15, ItemRect.Bottom) );

          Pen.Color := Colors.GridLineColor;
          PolyLine( [Point(15, ItemRect.Top), Point(15, ItemRect.Bottom)] );

          // Modified from VirtualTrees.pas :
          //   Show node button if allowed, if there child nodes and at least one of the child
          //   nodes is visible or auto button hiding is disabled.
          if (vsHasChildren in Node.States) and
            not (vsAllChildrenHidden in Node.States) then
            PaintNodeButton( Canvas, Node, 0, ItemRect, 3, 3, bdLeftToRight );
     end;
     inherited;
end;

{$ifndef COMPILER_17_UP}
procedure TVirtualPropertyTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: WideString);
{$else}
procedure TVirtualPropertyTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: UnicodeString);
{$endif}
begin
     if (Node^.Parent = RootNode) then
     begin
          if Column = 0 then
          begin
               Text := FCategories[Node^.Index].Name;
          end else
          begin
               Text := '';
          end;
     end else
     begin
          if Column = 0 then
            Text := (FCategories[Node^.Parent^.Index].Fields[Node^.Index])^.Name
          else
            Text := FieldToStr( (FCategories[Node^.Parent^.Index].Fields[Node^.Index])^ );
     end;

     inherited;
end;

procedure TVirtualPropertyTree.DoInitChildren(Node: PVirtualNode;
  var ChildCount: Cardinal);
begin
     ChildCount := Length( FCategories[Node^.Index].Fields );

     inherited;
end;

procedure TVirtualPropertyTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
     if Node^.Parent = RootNode then
       Include( InitStates, ivsHasChildren );

     Include( InitStates, ivsExpanded );

     inherited;
end;

function TVirtualPropertyTree.DoPaintBackground(Canvas: TCanvas;
  R: TRect): Boolean;
begin
     with Canvas do
     begin
          Pen.Color := Colors.GridLineColor;
          PolyLine( [Point(15, 0), Point(15, GetTreeRect.Bottom)] );  // Check ! what about R ?!
     end;
     Result := inherited DoPaintBackground( Canvas, R );
end;

procedure TVirtualPropertyTree.DoPaintText(Node: PVirtualNode;
  const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
     if Node^.Parent = RootNode then
     begin
          Canvas.Font.Style := Canvas.Font.Style + [fsBold];
     end;

     if (GetNodeField(Node)<>nil) and  not GetNodeField(Node).Enabled then
       Canvas.Font.Color := Colors.DisabledColor
     else
     if vsSelected in Node^.States then
     begin
          if Column = 1 then
            Canvas.Font.Color := Font.Color
          else
            Canvas.Font.Color := clWindow;
     end else
     begin
          Canvas.Font.Color := Font.Color;
     end;

     inherited;
end;

function TVirtualPropertyTree.IndexOf( CategoryName : string ) : integer;
var i : integer;
begin
     Result := -1;
     for i := 0 to Length(FCategories)-1 do
       if FCategories[i].Name = CategoryName then
       begin
            Result := i;
            exit;
       end;
end;

procedure TVirtualPropertyTree.HideAllEditors();
begin
   FEdit.Hide;
   FCombo.Hide;
   FTridot.Hide;
   FButton.Hide;
   FEditButton.Hide;
end;

procedure TVirtualPropertyTree.RefreshFields;
var i, j, k : integer;
begin
     HideAllEditors();
     ClearSelection;

     SetLength( FCategories, 0 );

     // Build Categories
     for i := 0 to Length(Fields)-1 do
     begin
          j := IndexOf( Fields[i].Category );
          if j <> -1 then
          begin
               // Add this field to this category
               k := Length( FCategories[j].Fields );
               SetLength( FCategories[j].Fields, k+1 );
               FCategories[j].Fields[k] := @(Fields[i]);
          end else
          begin
               // Create a new category
               j := Length( FCategories );
               SetLength( FCategories, j+1 );
               FCategories[j].Name := Fields[i].Category;
               SetLength( FCategories[j].Fields, 1 );
               FCategories[j].Fields[0] := @(Fields[i]);
          end;
     end;

     RootNodeCount := Length( FCategories );
     ReinitNode( nil, true );

     Header.Columns[0].Width := GetMaxColumnWidth( 0 ) + 4;
end;

//----------------- TVTPropertyEdit --------------------------------------------------------------------------------------------

// Implementation of a generic node caption editor.

constructor TVTPropertyEdit.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  ShowHint := False;
  ParentShowHint := False;
  BorderStyle := bsNone;
  ExecuteKillFocus := true;
  FNode := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTPropertyEdit.WMChar(var Message: TWMChar);

begin
  if not (Message.CharCode in [VK_ESCAPE{, VK_TAB}]) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTPropertyEdit.WMGetDlgCode(var Message: TWMGetDlgCode);

begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTTAB;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTPropertyEdit.WMKeyDown(var Message: TWMKeyDown);

// Handles some control keys.

begin
  case Message.CharCode of
    // pretend these keycodes were sent to the tree
    VK_ESCAPE:
      CancelEdit;
    VK_RETURN:
      EndEdit;
    VK_TAB:
      EndEdit;
    VK_UP:
      begin
        Message.CharCode := VK_LEFT;
        inherited;
      end;
    VK_DOWN:
      begin
        Message.CharCode := VK_RIGHT;
        inherited;
      end;
    VK_DELETE:
      begin
        if Assigned(FOnDelBtnClick) and (Owner is TVirtualPropertyTree) then
          FOnDelBtnClick(Owner, TVirtualPropertyTree(Owner).GetNodeField(FNode));

        inherited;
      end;
  else
    inherited;
  end;
end;

procedure TVirtualPropertyTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
var f: TField;
begin
  if (PaintInfo.Column = 1) and (PaintInfo.Node^.Parent<>RootNode) then
  begin
       dec( PaintInfo.ContentRect.Left, Margin );
       f:=(FCategories[PaintInfo.Node^.Parent^.Index].Fields[PaintInfo.Node^.Index])^;

       //with PaintInfo.Node^, (FCategories[PaintInfo.Node^.Parent^.Index].Fields[PaintInfo.Node^.Index])^ do
         if (f.DataType = dtColor) and (PaintInfo.Node^.Parent <> RootNode) then
         begin
              PaintInfo.Canvas.Pen.Color := clBlack;
              PaintInfo.Canvas.Brush.Color := f.IntData;//PColor(Data)^;
              PaintInfo.Canvas.Rectangle( PaintInfo.ContentRect.Left+1, PaintInfo.ContentRect.Top+1, PaintInfo.ContentRect.Left+13, PaintInfo.ContentRect.Bottom-1 );

              inc( PaintInfo.ContentRect.Left, 18 );
         end;
  end;

  if (PaintInfo.Column = 0) and (vsSelected in PaintInfo.Node^.States) then
  begin
       PaintInfo.Canvas.Brush.Color := clHighlight;
       PaintInfo.Canvas.FillRect( PaintInfo.CellRect );
  end;

  if (PaintInfo.Column = 1) and (vsSelected in PaintInfo.Node^.States) then
  begin
       PaintInfo.Canvas.Brush.Color := Self.Color;
       PaintInfo.Canvas.FillRect( PaintInfo.CellRect );
  end;

  inherited;
end;

procedure TVirtualPropertyTree.DoChange(Node: PVirtualNode);
begin
     inherited;
     if (Node <> nil) and (vsSelected in Node^.States) then
       MoveEditor( Node );
end;

procedure TVirtualPropertyTree.MoveEditor(Node: PVirtualNode);

var Field : PField;
    i : integer;
begin
     // Move FEdit to Column 1 of Node
     FEdit.EditMask:='';
     Field := GetNodeField(Node);
     if (Field <> nil) then
     begin
         FCombo.Enabled := Field.Enabled;

         FTridot.FButton.Enabled := Field.Enabled;
         FTridot.FEdit.ReadOnly := not Field.Enabled;

         FButton.Enabled := Field.Enabled;

         FEdit.ReadOnly := not Field.Enabled;

         FEditButton.FEdit.ReadOnly := not Field.Enabled;
         FEditButton.FButton.Enabled := Field.Enabled;

         HideAllEditors();

         if Field^.DataType = dtString then
         begin
              with FEditButton do
              begin
                   FEdit.EditMask:=Field^.EditMask;
                   FEdit.PasswordChar:=Field^.PasswordChar;
                   FEdit.Text := FieldToStr(Field^);
                   FEdit.FNode := Node;

                   FButton.Visible := (FEdit.PasswordChar = '');

                   FStringForUser:=Field^.StringForUser;
                   FValue:= Field^.Stringdata;// PString( Field^.Data )^;

                   UpdateSize( Self );

                   Show;
                   FEdit.SetFocus();
              end;
         end
         else if (Field^.DataType = dtInteger) or (Field^.DataType = dtCommonString) or
            (Field^.DataType = dtExtended) or (Field^.DataType = dtTime) then
         begin
              with FEdit do
              begin
                   EditMask:=Field^.EditMask;
                   PasswordChar:=Field^.PasswordChar;
                   EditText := FieldToStr( Field^ );
                   FNode := Node;
                   UpdateSize( Self );
                   Show;
                   SetFocus();
              end;
         end else
         if (Field^.DataType = dtCombo) then
         begin
              with FCombo do
              begin
                   Text := FieldToStr(Field^);
                   FNode := Node;

                   Items.Clear;
                   for i := 0 to Length(Field^.ComboData)-1 do
                     Items.AddObject( Field^.ComboData[i], TObject(Field^.ComboInt[i]) );
                   ItemIndex := Items.IndexOfObject( TObject(Field^.IntData) );

                   UpdateSize( Self );

                   Show;
              end;
         end else
         if (Field^.DataType = dtColor) then
         begin
              with FTridot do
              begin
                   FTridot.FEdit.Text := FieldToStr(Field^);
                   FTridot.FEdit.FNode := Node;

                   FTridot.FColor := Field^.IntData;//PColor( Field^.Data )^;

                   FTridot.UpdateSize( Self );
                   FTridot.Show;
              end;
         end else
         if (Field^.DataType = dtButton) then
         begin
              with FButton do
              begin
                   FEdit.Text := FieldToStr(Field^);
                   FEdit.FNode := Node;

                   FStringForUser:=Field^.StringForUser;
                   FValue:= Field^.Stringdata;// PString( Field^.Data )^;

                   UpdateSize( Self );

                   Show;
              end;
         end;
     end
     else
     begin
          // No editor
     end;
end;

procedure TVTPropertyEdit.CancelEdit;
begin
     with (Owner as TVirtualPropertyTree) do
     begin
          ExecuteKillFocus := false;
          SetFocus;
          Self.Text := FieldToStr( FCategories[FNode^.Parent^.Index].Fields[FNode^.Index]^ );
          ExecuteKillFocus := true;
     end;
end;

procedure TVTPropertyEdit.EndEdit;
begin
     with (Owner as TVirtualPropertyTree) do
     begin
          ExecuteKillFocus := false;
          UpdateParent;
          SetFocus;
          ExecuteKillFocus := true;
     end;
end;

{$ifndef COMPILER_17_UP}
procedure TVirtualPropertyTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: WideString);
{$else}
procedure TVirtualPropertyTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: UnicodeString);
{$endif}
begin
     if SetFieldValue( Node, Text ) then
       inherited;
end;

function TVirtualPropertyTree.SetFieldValue(Node: PVirtualNode; Text: string) : boolean;
var Field : PField;
    t : integer;
    x : extended;
    tm: TTime;
    s: string;
begin
     Result := true;
     Field := FCategories[Node.Parent.Index].Fields[Node.Index];
     with Field^ do
     begin
          case DataType of
            dtInteger:  try
                          if Field.AllowNull and (Trim(Text)='') then
                          begin
                            IntData:=0;
                            StringData:=Text;
                          end
                          else
                          begin
                            t := StrToInt( Text );
                            if (MinIntValue<MaxIntValue) and (t >= MinIntValue) and (t <= MaxIntValue) then
                            begin
                              IntData:=t; //PInteger(Data)^ := t
                              StringData:=Text;
                            end
                            else
                            if PropertyTreeOptions.ShowRangeError then MessageBox(Handle, PChar(Format(PropertyTreeOptions.RangeErrorText, [MinIntValue, MaxIntValue])), PChar(PropertyTreeOptions.ErrorTitle), mb_ok or MB_ICONEXCLAMATION );
                          end;
                        except
                          if PropertyTreeOptions.ShowValidityError then MessageBox( Handle, PChar(Format(PropertyTreeOptions.ValidityErrorText, [Text] )), PChar(PropertyTreeOptions.ErrorTitle), mb_ok or MB_ICONEXCLAMATION );
                        end;
            dtTime:     try
                          s := StringReplace(Text, ' ', '0', [rfReplaceAll]);
                          tm := StrToTime(s);
                          ExtendedData := tm;
                          StringData := s;
                        except
                          if PropertyTreeOptions.ShowValidityError then
                            MessageBox( Handle, PChar(Format(PropertyTreeOptions.TimeErrorText, [Text] )), PChar(PropertyTreeOptions.ErrorTitle), MB_ICONEXCLAMATION );
                        end;
            dtString,
            dtCommonString:   StringData:=Text;// PString(Data)^ := Text;
            dtCombo:    begin
                             // We assume (hope) that Text is one of ComboData
                             for t := 0 to Length(ComboData)-1 do
                             begin
                                  if ComboData[t] = Text then
                                  begin
                                    IntData:=ComboInt[t];
                                    StringData:=Text;
                                    //PInteger(Data)^ := t;
                                    break;
                                  end;
                             end;
                        end;
            dtColor:    begin
                          IntData:=MyStrToColor( Text );//PColor(Data)^ := MyStrToColor( Text );
                          StringData:=Text;
                        end;
            dtExtended: try
                          if Field.AllowNull and (Trim(Text)='') then
                          begin
                            IntData:=0;
                            StringData:=Text;
                          end
                          else
                          begin
                            x := MyStrToFloat( Text );
                            if (x >= MinExtValue) and (x <= MaxExtValue) then
                            begin
                              ExtendedData:=x;
                              StringData:=Text; //PExtended(Data)^ := x
                            end
                            else
                            if PropertyTreeOptions.ShowRangeError then
                              MessageBox( Handle, PChar(Format(StringReplace(PropertyTreeOptions.RangeErrorText, '%d', '%g', [rfReplaceAll, rfIgnoreCase]), [MinExtValue, MaxExtValue])), PChar(PropertyTreeOptions.ErrorTitle), mb_ok or MB_ICONEXCLAMATION );
                          end;
                        except
                          if PropertyTreeOptions.ShowValidityError then
                            MessageBox( Handle, PChar(Format(PropertyTreeOptions.ValidityErrorText, [Text] )), PChar(PropertyTreeOptions.ErrorTitle), mb_ok or MB_ICONEXCLAMATION );
                        end;
          end;
     end;

     if Assigned(FOnPropertyChange) then FOnPropertyChange(self, Field);
end;

function TVirtualPropertyTree.GetNodeField( Node : PVirtualNode ): PField;
begin
     Result := nil;
     if Node^.Parent <> RootNode then
       Result := FCategories[Node^.Parent^.Index].Fields[Node^.Index];
end;

procedure TVTPropertyEdit.WMKillFocus(var Message: TWMKillFocus);
begin
     if ExecuteKillFocus then
     begin
          EndEdit;
     end;
     inherited;
end;

procedure TVirtualPropertyTree.Loaded;
begin
     inherited;

     // Initialize things like Columns and Options
     Colors.GridLineColor := clSilver;
     DefaultNodeHeight := 16;
     EditDelay := 0;
     Header.AutoSizeIndex := 1;
     Header.Columns.Clear;
     with Header.Columns.Add do
     begin
          MaxWidth := MAXINT;
          Width := 80;
          //Text := 'Параметр';
     end;
     with Header.Columns.Add do
     begin
          Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible];
          Width := 229;
          //Text := 'Значение';
     end;
     Header.Options := [hoAutoResize, hoColumnResize, hoDblClickResize{, hoDrag}];

     Indent := 0;
     LineStyle := lsSolid;
     Margin := 16;
     TextMargin := 0;

     NodeDataSize := SizeOf( TField );
     RootNodeCount := 0;

     TreeOptions.MiscOptions := [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning];
     TreeOptions.PaintOptions := [toHideFocusRect, toHideSelection, toPopupMode, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages];
     TreeOptions.SelectionOptions := [toFullRowSelect];
end;

procedure TVirtualPropertyTree.WMLButtonDown(var Message: TWMLButtonDown);
var HitInfo : THitInfo;
begin
     inherited;

     GetHitTestInfoAt( Message.XPos, Message.YPos, true, HitInfo );

     // If we're talking about Drop-fields
     if (HitInfo.HitNode <> nil) and (HitInfo.HitPositions = [hiOnItemCheckbox]) then
       EditNode( HitInfo.HitNode, 0 );

     if (HitInfo.HitNode <> nil) and (hiOnItemButton in HitInfo.HitPositions) then
     begin
          Selected[FocusedNode] := false;
          FocusedNode := HitInfo.HitNode;
          Selected[FocusedNode] := true;
     end;
end;

destructor TVirtualPropertyTree.Destroy;
begin
        Finalize(FCategories);
        PropertyTreeOptions.Free;

        inherited;
end;

procedure TVirtualPropertyTree.WMSize(var Message: TWMSize);
begin
     inherited;
     if FEdit.Visible then
       FEdit.UpdateSize( Self );
     if FCombo.Visible then
       FCombo.UpdateSize( Self );
     if FTridot.Visible then
       FTridot.UpdateSize( Self );
     if FButton.Visible then
       FButton.UpdateSize( Self );
end;

procedure TVTPropertyCombo.DblClick;
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self)
  else
  if TVirtualPropertyTree(Owner).GetNodeField(FNode).Enabled then
  begin
    if Items.Count > 1 then
    begin
      if ItemIndex = Items.Count - 1 then
        ItemIndex := 0
      else
        ItemIndex := ItemIndex + 1;

      Change();
    end;
  end;
end;

procedure TVTPropertyCombo.Change;
var VPT: TVirtualPropertyTree;
begin
  inherited;

  VPT:=(Owner as TVirtualPropertyTree);

  if FieldToStr( VPT.FCategories[FNode^.Parent^.Index].Fields[FNode^.Index]^ ) <> Self.Text then
  begin
       VPT.DoNewText( FNode, 1, Self.Text );
       Self.Text := FieldToStr( VPT.FCategories[FNode^.Parent^.Index].Fields[FNode^.Index]^ );
  end;
end;

procedure TVTPropertyCombo.CreateWnd;
var r : TRect;
begin
     inherited;
     // Set the Edit part of the ComboBox to ReadOnly
     SendMessage( EditHandle, EM_SETREADONLY, 1, 0 );
     DropDownCount:=20;

     // Setting sizes and positions
     GetWindowRect( EditHandle, r );
     SetWindowPos( EditHandle, HWND_TOP, r.Left-1, r.Top+1, r.Bottom, r.Right, SWP_NOZORDER );
end;

procedure TVTPropertyCombo.UpdateSize(ATree: TVirtualStringTree);
var FBounds : TRect;
begin
     FBounds := ATree.GetDisplayRect( FNode, 1, false );
     dec( FBounds.Top, 3 );
     dec( FBounds.Left, 2 );
     inc( FBounds.Right, 4 );
     BoundsRect := FBounds;

     SetWindowRgn( Handle,
       CreateRectRgn( 2, 3, Width-3, Height-3 ), true );
end;

procedure TVirtualPropertyTree.PostEdit;
begin
  if FEdit.Focused then
    FEdit.EndEdit();
end;

procedure TVirtualPropertyTree.DblClick;
begin
  inherited DblClick;
  if FCombo.Visible then FCombo.DblClick();
end;

procedure TVirtualPropertyTree.CMMouseWheel(var Message: TCMMouseWheel);
begin
  if FCombo.Visible and FCombo.DroppedDown then
    ScrollWindow(FCombo.ListHandle, 0, Message.WheelDelta div WHEEL_DELTA, nil, nil)
  else
    inherited;
end;

procedure TVirtualPropertyTree.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;

  if (Message.CharCode = VK_DELETE)
      and (FButton.Visible)
      and Assigned(FButton.InternalEdit.FOnDelBtnClick) then
    FButton.InternalEdit.FOnDelBtnClick(self, GetNodeField(FButton.InternalEdit.FNode));
end;

procedure TVirtualPropertyTree.Clear;
begin
  inherited;

  FEdit.FNode := nil;
  FCombo.FNode := nil;
  FTridot.FEdit.FNode := nil;
  FButton.FEdit.FNode := nil;
  FEditButton.FEdit.FNode := nil;
end;

{ TVTPropertyColor }

constructor TVTPropertyColor.Create(AOwner: TComponent);
begin
     inherited;

     FEdit := TVTPropertyEdit.Create( AOwner );
     with FEdit do
     begin
          Parent := Self;
          ReadOnly := true;
     end;

     FButton := TSpeedButton.Create( Self );
     with FButton do
     begin
          Parent := Self;
          OnClick := MyButtonClick;
          Glyph := AccessEditorBitmap( ebTridot );
     end;
end;

procedure TVTPropertyColor.MyButtonClick(Sender: TObject);
var ColorDialog : TColorDialog;
begin
     ColorDialog := TColorDialog.Create( Self );
     with ColorDialog do
     begin
          Color := FColor;
          Options := [cdFullOpen, cdAnyColor];

          if Execute then
          begin
               FColor := Color;
               UpdateEdit;
               with FEdit do
               begin
                    EndEdit;
               end;
               Paint;
          end;
     end;
end;

procedure TVTPropertyColor.Paint;
begin
     inherited Paint;
     with Canvas do
     begin
          Pen.Color := (Parent as TVirtualPropertyTree).Colors.GridLineColor;
          PolyLine( [ Point(0, 0), Point(Width, 0) ] );
          PolyLine( [ Point(0, Height-1), Point(Width, Height-1) ] );

          Pen.Color := clBlack;
          Brush.Color := FColor;
          Rectangle( 1, 2, 1+12, Height-2 );
     end;
end;

procedure TVTPropertyColor.UpdateEdit;
begin
     FEdit.Text := Format( '%d, %d, %d', [
                                                  (FColor and $000000FF) shr 0, // Red
                                                  (FColor and $0000FF00) shr 8, // Green
                                                  (FColor and $00FF0000) shr 16 // Blue
                                                ] );
end;

procedure TVTPropertyEdit.UpdateSize(ATree: TVirtualStringTree);
var FBounds : TRect;
begin
     FBounds := ATree.GetDisplayRect( FNode, 1, false );
     dec( FBounds.Bottom );
     inc( FBounds.Top );

     BoundsRect := FBounds;
end;

procedure TVTPropertyColor.UpdateSize(ATree: TVirtualStringTree);
var FBounds : TRect;
begin
     FBounds := ATree.GetDisplayRect( FEdit.FNode, 1, false );
     dec( FBounds.Top );
     inc( FBounds.Right );
     BoundsRect := FBounds;

     FEdit.BoundsRect := Rect( 18, 2, Width-17, Height-1 );
     FButton.BoundsRect := Rect( Width-18, 0, Width, Height );
end;

procedure TVTPropertyEdit.UpdateParent;
var vpt: TVirtualPropertyTree;
    f: TField;
begin
    vpt := TVirtualPropertyTree(Owner);
    if (FNode <> nil) then
    begin
      f := vpt.FCategories[FNode^.Parent^.Index].Fields[FNode^.Index]^;
      if FieldToStr( f ) <> Self.Text then
      begin
        vpt.DoNewText( FNode, 1, Self.Text );
        Self.Text := FieldToStr( f );
      end;
    end;
end;

{ TVTPropertyButton }

procedure TVTPropertyButton.MyButtonClick(Sender: TObject);
begin
  if Assigned(FOnBtnClick) and (Owner is TVirtualPropertyTree) then
    FOnBtnClick(Owner, TVirtualPropertyTree(Owner).GetNodeField(FEdit.FNode));
end;

procedure TVTPropertyButton.Paint;
begin
  inherited;
  with Canvas do
  begin
       Pen.Color := (Parent as TVirtualPropertyTree).Colors.GridLineColor;
       PolyLine( [ Point(0, 0), Point(Width, 0) ] );
       PolyLine( [ Point(0, Height-1), Point(Width, Height-1) ] );
  end;
end;

procedure TVTPropertyButton.UpdateEdit;
begin
  FEdit.EditText := FStringForUser;
end;

procedure TVTPropertyButton.UpdateSize(ATree: TVirtualStringTree);
var FBounds : TRect;
begin
     FBounds := ATree.GetDisplayRect( FEdit.FNode, 1, false );
     dec( FBounds.Top );
     inc( FBounds.Right );
     BoundsRect := FBounds;

     FEdit.BoundsRect := Rect( 2, 2, Width-17, Height-1 );
     FButton.BoundsRect := Rect( Width-18, 0, Width, Height );
end;

{ TVTPropertyEditWithButton }

constructor TVTPropertyEditWithButton.Create(AOwner: TComponent);
begin
  inherited;

  FEdit.ReadOnly := false;
end;

procedure TVTPropertyEditWithButton.MyButtonClick(Sender: TObject);
var f: PField;
    s: string;
begin
  f := (Owner as TVirtualPropertyTree).FCategories[FEdit.FNode^.Parent^.Index].Fields[FEdit.FNode^.Index];
  if PropertyTextEditor.ShowModal(f.Name, FEdit.Text) = mrOk then
  begin
    s := PropertyTextEditor.mEditor.Text;
    while (s <> '') and CharInSet(s[Length(s)], [#13, #10, #9]) do
      Delete(s, Length(s), 1);

    FEdit.Text := s;
    FEdit.EndEdit();
  end;
end;

constructor TVTPropertyCombo.Create(AOwner: TComponent);
begin
  inherited;

  FNode := nil;
end;

{ TField }

procedure TField.Init;
begin
  IntData := -1;
  ExtendedData := 0;
  DataType := dtString;
  MinIntValue := 0;
  MaxIntValue := 0;
  AllowNull := false;
  MinExtValue := 0;
  MaxExtValue := 0;
  Enabled := true;
  ExtObject := nil;
end;

initialization
  PropertyTextEditor := TPropertyTextEditor.Create(nil);


  EditorButtons[0] := nil;
  EditorButtons[1] := nil;

finalization
  PropertyTextEditor.Free();

  if EditorButtons[0] <> nil then EditorButtons[0].Free;
  if EditorButtons[1] <> nil then EditorButtons[1].Free;

end.

