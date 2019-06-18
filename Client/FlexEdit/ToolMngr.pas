unit ToolMngr;

interface

uses
  Windows, Types, UITypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Menus,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  {$IFDEF VER120}
  TWMContextMenu = packed record
    Msg: Cardinal;
    hWnd: HWND;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;
  {$ENDIF}

  TToolContainer = class(TForm)
    pgTools: TcxPageControl;
    pmTools: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FTools: TList;
    FOnNeedClose: TNotifyEvent;
    FOnPopupChange: TNotifyEvent;
    procedure ArrangeTools;
    function  GetPageForm(PageIndex: integer): TCustomForm;
    function  FindPage(ToolForm: TCustomForm): TcxTabSheet;
    procedure PopupClosePage(Sender: TObject);
    procedure PopupCloseAll(Sender: TObject);
    procedure PopupToolClick(Sender: TObject);
    procedure DoToolsPopup(Sender: TObject; MousePos: TPoint);
    function  GetActivePageForm: TCustomForm;
    procedure SetActivePageForm(const Value: TCustomForm);
    function  GetTool(Index: integer): TCustomForm;
    function  GetToolCount: integer;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
  protected
    procedure DoPopupChange; virtual;
  public
    { Public declarations }
    procedure InsertTool(ToolForm: TCustomForm);
    procedure RemoveTool(ToolForm: TCustomForm);
    procedure RemoveAll;
    function  IsToolExists(ToolForm: TCustomForm): boolean;
    property  Tools[Index: integer]: TCustomForm read GetTool;
    property  ToolCount: integer read GetToolCount;  
    property  ActivePageForm: TCustomForm read GetActivePageForm
      write SetActivePageForm;
    property  OnNeedClose: TNotifyEvent read FOnNeedClose write FOnNeedClose;
    property  OnPopupChange: TNotifyEvent read FOnPopupChange
      write FOnPopupChange; 
  end;

var
  ToolForms: TList;
  ToolContainer: TToolContainer;

procedure RegisterToolForm(ToolForm: TCustomForm);
procedure UnRegisterToolForm(ToolForm: TCustomForm);

function  FindChildContainer(Control: TWinControl): TToolContainer;
function  FindToolParentContainer(ToolForm: TCustomForm): TToolContainer;
function  FindToolForm(const AName: string): TCustomForm;

implementation

{$R *.DFM}

function FindToolParentContainer(ToolForm: TCustomForm): TToolContainer;
var Control: TControl;
begin
 Result := Nil;
 if not Assigned(ToolForm) then exit;
 Control := ToolForm.Parent;
 while Assigned(Control) do
  if Control is TToolContainer then begin
   Result := TToolContainer(Control);
   break;
  end else
   Control := Control.Parent;
end;

function FindChildContainer(Control: TWinControl): TToolContainer;
var i: integer;
begin
 Result := Nil;
 with Control do
 for i:=0 to ControlCount-1 do
  if Controls[i] is TToolContainer then begin
   Result := TToolContainer(Controls[i]);
   break;
  end;
end;

function FindToolForm(const AName: string): TCustomForm;
var i: integer;
begin
 Result := Nil;
 for i:=0 to ToolForms.Count-1 do
  if TCustomForm(ToolForms[i]).Name = AName then begin
   Result := TCustomForm(ToolForms[i]);
   break;
  end;
end;

procedure RegisterToolForm(ToolForm: TCustomForm);
begin
 if not Assigned(ToolForms) then ToolForms := TList.Create;
 ToolForms.Add(ToolForm);
end;

procedure UnRegisterToolForm(ToolForm: TCustomForm);
var Container: TToolContainer;
begin
 if Assigned(ToolForms) then begin
  ToolForms.Remove(ToolForm);
  if ToolForms.Count = 0 then
   {$IFDEF VER120}
   begin
    ToolForms.Free;
    ToolForms := Nil;
   end;
   {$ELSE}
   FreeAndNil(ToolForms);
   {$ENDIF}
 end;
 Container := FindToolParentContainer(ToolForm);
 if Assigned(Container) then Container.RemoveTool(ToolForm);
end;

// TToolContainer ///////////////////////////////////////////////////////////

procedure TToolContainer.FormCreate(Sender: TObject);
begin
 FTools := TList.Create;
end;

procedure TToolContainer.FormDestroy(Sender: TObject);
begin
 FTools.Free;
end;

procedure TToolContainer.FormShow(Sender: TObject);
begin
 with pgTools do begin
  Left := -1;
  Top := 0;
  Width := Self.ClientWidth +3;
  Height := Self.ClientHeight +2;
 end;
end;

procedure TToolContainer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TToolContainer.DoPopupChange;
begin
 if Assigned(FOnPopupChange) then FOnPopupChange(Self);
end;

function TToolContainer.GetTool(Index: integer): TCustomForm;
begin
 Result := TCustomForm(FTools[Index]);
end;

function TToolContainer.GetToolCount: integer;
begin
 Result := FTools.Count;
end;

procedure TToolContainer.InsertTool(ToolForm: TCustomForm);
var Container: TToolContainer;
begin
 if not Assigned(ToolForm) or (FTools.IndexOf(ToolForm) >= 0) then exit;
 Container := FindToolParentContainer(ToolForm);
 if Assigned(Container) and (Container <> Self) then
  Container.RemoveTool(ToolForm);
 FTools.Add(ToolForm);
 ArrangeTools;
 DoPopupChange;
end;

procedure TToolContainer.RemoveTool(ToolForm: TCustomForm);
begin
 FTools.Remove(ToolForm);
 ArrangeTools;
 if (FTools.Count = 0) and Assigned(FOnNeedClose) then FOnNeedClose(Self);
 if not (csDestroying in ComponentState) then DoPopupChange;
end;

procedure TToolContainer.RemoveAll;
begin
 FTools.Clear;
 ArrangeTools;
 if (FTools.Count = 0) and Assigned(FOnNeedClose) then FOnNeedClose(Self);
 if not (csDestroying in ComponentState) then DoPopupChange;
end;

function TToolContainer.GetPageForm(PageIndex: integer): TCustomForm;
begin
 Result :=
  TCustomForm( TWinControl(pgTools.Pages[PageIndex].Controls[0]).Controls[0] );
end;

function TToolContainer.FindPage(ToolForm: TCustomForm): TcxTabSheet;
var i: integer;
begin
 Result := Nil;
 for i:=0 to FTools.Count-1 do
  if GetPageForm(i) = ToolForm then begin
   Result := pgTools.Pages[i];
   break;
  end;
end;

procedure TToolContainer.ArrangeTools;
var i, Index: integer;
    Exists: array of boolean;
    ToolForm: TCustomForm;
    Page: TcxTabSheet;
    PagePanel: TPanel;
begin
 SetLength(Exists, FTools.Count);
 FillChar(Exists[0], Length(Exists) * SizeOf(Exists[0]), 0);
 i := pgTools.PageCount-1;
 while i >= 0 do begin
  ToolForm := GetPageForm(i);
  Index := FTools.IndexOf(ToolForm);
  if Index < 0 then begin
   ToolForm.Hide;
   ToolForm.Parent := Nil;
   try
    pgTools.Pages[i].Free
   except
   end;
  end else
   Exists[Index] := True;
  dec(i);
 end;
 for i:=0 to Length(Exists)-1 do begin
  if Exists[i] then continue;
  ToolForm := TCustomForm(FTools[i]);
  Page := TcxTabSheet.Create(Self);
  Page.PageControl := pgTools;
  Page.Caption := ToolForm.Caption;
  PagePanel := TPanel.Create(Self);
  with PagePanel do begin
   Parent := Page;
   Align := alClient;
   BevelOuter := bvNone;
   Caption := '';
  end;
  with ToolForm do begin
   Parent := PagePanel;
   SetBounds(0, 0, Width, Height);
   Show;
  end;
 end;
end;

function TToolContainer.IsToolExists(ToolForm: TCustomForm): boolean;
begin
 Result := FTools.IndexOf(ToolForm) >= 0;
end;

procedure TToolContainer.PopupClosePage(Sender: TObject);
begin
 RemoveTool(ActivePageForm);
end;

procedure TToolContainer.PopupCloseAll(Sender: TObject);
begin
 RemoveAll;
end;

procedure TToolContainer.PopupToolClick(Sender: TObject);
var ToolForm: TCustomForm;
begin
 if not (Sender is TMenuItem) then exit;
 ToolForm := TCustomForm(TMenuItem(Sender).Tag);
 if IsToolExists(ToolForm) then
  RemoveTool(ToolForm)
 else begin
  InsertTool(ToolForm);
  pgTools.ActivePage := FindPage(ToolForm);
 end;
end;

function TToolContainer.GetActivePageForm: TCustomForm;
begin
 Result := GetPageForm( pgTools.ActivePage.PageIndex );
end;

procedure TToolContainer.SetActivePageForm(const Value: TCustomForm);
begin
 pgTools.ActivePage := FindPage(Value);
end;

procedure TToolContainer.DoToolsPopup(Sender: TObject;
  MousePos: TPoint);
var i: integer;
    pmItem: TMenuItem;
    ToolForm: TCustomForm;
begin
 if not Assigned(ToolForms) or (ToolForms.Count = 0) then exit;
 with pmTools do begin
  {$IFNDEF VER120}
  Items.Clear;
  {$ELSE}
  while Items.Count > 0 do Items.Delete(Items.Count-1);
  {$ENDIF}
  pmItem := TMenuItem.Create(pmTools);
  pmItem.Caption := '&Close';
  pmItem.OnClick := PopupClosePage;
  Items.Add(pmItem);
  pmItem := TMenuItem.Create(pmTools);
  pmItem.Caption := '-';
  Items.Add(pmItem);
  for i:=0 to ToolForms.Count-1 do begin
   ToolForm := TCustomForm(ToolForms[i]);
   pmItem := TMenuItem.Create(pmTools);
   pmItem.Caption := ToolForm.Caption;
   pmItem.OnClick := PopupToolClick;
   pmItem.Tag := integer(ToolForm);
   if IsToolExists(ToolForm) then pmItem.Checked := true;
   Items.Add(pmItem);
  end;
  pmItem := TMenuItem.Create(pmTools);
  pmItem.Caption := '-';
  Items.Add(pmItem);
  pmItem := TMenuItem.Create(pmTools);
  pmItem.Caption := 'Close &All';
  pmItem.OnClick := PopupCloseAll;
  Items.Add(pmItem);
  MousePos := Self.ClientToScreen(MousePos);
  Popup(MousePos.X, MousePos.Y);
 end;
end;

procedure TToolContainer.WMContextMenu(var Message: TWMContextMenu);
var Pt: TPoint;
    i: integer;
    Found: boolean;
begin
 Pt := ScreenToClient(Point(Message.XPos, Message.YPos));
 if PtInRect(pgTools.BoundsRect, Pt) then begin
  dec(Pt.X, pgTools.Left);
  dec(Pt.Y, pgTools.Top);
  Found := False;
  for i:=0 to pgTools.ControlCount-1 do
   if PtInRect(pgTools.Controls[i].BoundsRect, Pt) then begin
    Found := True;;
    break;
   end;
  if Found then
   inherited
  else begin
   DoToolsPopup(pgTools, Pt);
   Message.Result := 1;
  end;
 end else
  inherited;
end;

initialization

finalization
  ToolForms.Free;

end.
