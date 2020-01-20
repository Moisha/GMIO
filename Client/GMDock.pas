unit GMDock;

interface

uses Windows, Classes, Forms, Controls, GMGlobals, cxScrollBox, dxDockControl, dxDockPanel, ControlFrame,
     Graphics, Messages, GMConst, IniFiles;

type
  TCheckPanelFunctionRef = reference to function (p: TdxDockPanel): bool;
  TGMDockPanel = class(TdxDockPanel)
  private
    FGMForm: TGMForm;
    procedure ScrollboxResize(Sender: TObject);
    procedure WMAfterConstruction(var Msg: TMessage); message WM_AFTER_CONSTRUCTION;
    procedure DockPanelClose(Sender: TdxCustomDockControl);
    procedure DockPanelCloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
  protected
    procedure InsertForm(gmFormClass: TGMFormClass);
    procedure CreateWnd; override;
    procedure LoadLayoutFromCustomIni(AIniFile: TCustomIniFile; AParentForm: TCustomForm;
      AParentControl: TdxCustomDockControl; ASection: string); override;
    procedure SaveLayoutToCustomIni(AIniFile: TCustomIniFile; ASection: string); override;
    function NeedDestroyOnClose: bool; virtual;
  public
    property GMForm: TGMForm read FGMForm;
    class function FindAnyPanelAsDockDestination(checkPanelFunc: TCheckPanelFunctionRef): TGMDockPanel;
    class procedure CloseAll();
  end;

implementation

{ TGMDockPanel }

uses dxStandardControlsSkinHelper, EsLogging;

class procedure TGMDockPanel.CloseAll();
var
  i, n: int;
  b: bool;
begin
  // всякие отщепенцы
  while true do
  begin
    n := dxDockingController().DockControlCount;
    b := false;
    for i := n - 1 downto 0 do
    begin
      if dxDockingController().DockControls[i] is TGMDockPanel then
      begin
        dxDockingController().DockControls[i].OnCloseQuery := nil;
        dxDockingController().DockControls[i].Free();
        b := true;
      end;
    end;

    if not b then break;
  end;
end;

procedure TGMDockPanel.InsertForm(gmFormClass: TGMFormClass);
var scrollbox: TcxScrollBox;
begin
  scrollbox := TcxScrollBox.Create(self);
  scrollbox.Parent := self;
  scrollbox.Align := alClient;
  scrollbox.OnResize := ScrollboxResize;
  scrollbox.HorzScrollBar.Tracking := true;
  scrollbox.VertScrollBar.Tracking := true;

  FGMForm := gmFormClass.Create(self);
  FGMForm.Parent := scrollbox;
  FGMForm.Left := 0;
  FGMForm.Top := 0;

  Width := FGMForm.Width;
  Height := FGMForm.Height;
end;

class function TGMDockPanel.FindAnyPanelAsDockDestination(checkPanelFunc: TCheckPanelFunctionRef): TGMDockPanel;
var
  p: TdxCustomDockControl;
  i: int;
begin
  for i := 0 to dxDockingController().DockControlCount - 1 do
  begin
    p := dxDockingController().DockControls[i];
    if (p is TdxDockPanel)
       and (not Assigned(checkPanelFunc) or checkPanelFunc(TdxDockPanel(p)))
       and (p.Visible)
       and (p.DockState = dsDocked)//это условие необходимо, потому что при закрытии FloatDockSite, на котором Notebook (более одного окна задочено), будет исключение
      then
    begin
      Result := TGMDockPanel(p);
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TGMDockPanel.LoadLayoutFromCustomIni(AIniFile: TCustomIniFile; AParentForm: TCustomForm;
  AParentControl: TdxCustomDockControl; ASection: string);
begin
  inherited;

end;

procedure TGMDockPanel.CreateWnd;
begin
  inherited;
  PostMessage(Handle, WM_AFTER_CONSTRUCTION, 0, 0);
end;

function TGMDockPanel.NeedDestroyOnClose(): bool;
begin
  Result := false;
end;

procedure TGMDockPanel.DockPanelClose(Sender: TdxCustomDockControl);
begin
  if NeedDestroyOnClose() then
    GMPostMessage(WM_DESTROY_OBJECT, WParam(self), 0, DefaultLogger);
end;

procedure TGMDockPanel.DockPanelCloseQuery(Sender: TdxCustomDockControl; var CanClose: Boolean);
begin
  CanClose := Dockable;
end;

procedure TGMDockPanel.WMAfterConstruction(var Msg: TMessage);
begin
  FGMForm.Visible := true;
  FGMForm.BorderStyle := bsNone;
  FGMForm.Font.Quality := fqClearType;
  FGMForm.Color := GetSkinFormContentColor();

  Caption := FGMForm.Caption;

  Constraints.MinWidth := 200;
  Constraints.MinHeight := 200;

  OnClose := DockPanelClose;
  OnCloseQuery := DockPanelCloseQuery;
end;

procedure TGMDockPanel.SaveLayoutToCustomIni(AIniFile: TCustomIniFile; ASection: string);
begin
  inherited;

end;

procedure TGMDockPanel.ScrollboxResize(Sender: TObject);
var scrollbox: TcxScrollBox;
    minWidth, minHeight, vertScrollbarWidth, horzScrollbarHeight: int;
begin
  scrollbox := TcxScrollBox(Sender);
  FGMForm.Align := alNone;

  vertScrollbarWidth := GetSystemMetrics(SM_CXVSCROLL);
  horzScrollbarHeight := GetSystemMetrics(SM_CYHSCROLL);

  minWidth := FGMForm.GetMinWidth() + vertScrollbarWidth;
  if scrollbox.ClientBounds.Width < minWidth then
  begin
    FGMForm.Width := minWidth;
    scrollbox.HorzScrollBar.Range := FGMForm.Width;
  end
  else
  begin
    FGMForm.Width := scrollbox.ClientBounds.Width;
    if FGMForm.Width > scrollbox.ClientBounds.Width then
      scrollbox.HorzScrollBar.Range := FGMForm.Width
    else
      scrollbox.HorzScrollBar.Range := 0;
  end;

  minHeight := FGMForm.GetMinHeight() + horzScrollbarHeight;
  if scrollbox.ClientBounds.Height < minHeight then
  begin
    FGMForm.Height := minHeight;
    scrollbox.VertScrollBar.Range := minHeight;
  end
  else
  begin
    FGMForm.Height := scrollbox.ClientBounds.Height;
    scrollbox.VertScrollBar.Range := 0;
  end;
end;

end.
