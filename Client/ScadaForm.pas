unit ScadaForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FlexBase, ControlFrame, GMDock, GMGlobals, GMDBClasses,
  GMConst, IniFiles, dxDockControl, Vcl.ImgList, cxGraphics, dxSkinsCore, dxSkinOffice2010Silver, dxSkinsdxBarPainter,
  dxBar, cxClasses, Types, Vcl.ExtCtrls;

type
  TScadaFormForDocking = class(TGMForm)
    ControlFrm1: TControlFrm;
    Flex: TFlexPanel;
    cxImageList1: TcxImageList;
    dxBarManager1: TdxBarManager;
    dxBarPopupMenu1: TdxBarPopupMenu;
    dxBarButton1: TdxBarButton;
    dxBarButton2: TdxBarButton;
    odScada: TOpenDialog;
    timerScript: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FlexScadaMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FlexScadaUpdateCursor(Sender: TObject; var ACursor: TCursor);
    procedure WMShowControlPanel(var Msg: TMessage); message WM_SHOW_CONTROL_PANEL;
    procedure sbCloseClick(Sender: TObject);
    procedure dxBarButton1Click(Sender: TObject);
    procedure dxBarButton2Click(Sender: TObject);
    procedure timerScriptTimer(Sender: TObject);
  private
    function FlexScadaMouseOverControlObject(): TGMObject;
    procedure AddFlexScadaChannels();
    procedure AddFlexScadaChannels_OneControl(c: TFlexControl);
    procedure RunScript();
    function FlexScadaMouseOverClickableObject: bool;
    { Private declarations }
  protected
    function GetControlFrame(): TControlFrm; override;
    procedure LoadScada(const fileName: string);
  public
    { Public declarations }
  end;

  function CreateDockableScada(const fileName: string): TGMDockPanel;

implementation

{$R *.dfm}

uses dxStandardControlsSkinHelper, Threads.GMClient, FlexEdit.Main, ScriptRunner;

type
  TScadaDockPanel = class(TGMDockPanel)
  protected
    function NeedDestroyOnClose: bool; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadLayoutFromCustomIni(AIniFile: TCustomIniFile; AParentForm: TCustomForm;
      AParentControl: TdxCustomDockControl; ASection: string); override;
    procedure SaveLayoutToCustomIni(AIniFile: TCustomIniFile; ASection: string); override;
  end;

{ TScadaDockPanel }

constructor TScadaDockPanel.Create(AOwner: TComponent);
var editScada: TdxDockControlCaptionButton;
begin
  inherited;
  InsertForm(TScadaFormForDocking);
  CustomCaptionButtons.Images := TScadaFormForDocking(GMForm).cxImageList1;
  editScada := CustomCaptionButtons.Buttons.Add();
  editScada.ImageIndex := 0;
end;

function CreateDockableScada(const fileName: string): TGMDockPanel;
begin
  Result := TScadaDockPanel.Create(Application.MainForm);
  Result.Parent := Application.MainForm;
  if fileName <> '' then
    TScadaFormForDocking(Result.GMForm).LoadScada(fileName);

  Result.Caption := ExtractFileNameWithoutExt(fileName);
  Result.GMForm.Caption := Result.Caption;
end;

procedure TScadaDockPanel.LoadLayoutFromCustomIni(AIniFile: TCustomIniFile; AParentForm: TCustomForm;
  AParentControl: TdxCustomDockControl; ASection: string);
var fn: string;
begin
  inherited;

  fn := AIniFile.ReadString(ASection, 'ScadaFileName', '');
  TScadaFormForDocking(GMForm).LoadScada(fn);
  Caption := ExtractFileNameWithoutExt(fn);
  GMForm.Caption := Caption;
end;

function TScadaDockPanel.NeedDestroyOnClose: bool;
begin
  Result := true;
end;

procedure TScadaDockPanel.SaveLayoutToCustomIni(AIniFile: TCustomIniFile; ASection: string);
begin
  inherited;

  AIniFile.WriteString(ASection, 'ScadaFileName', TScadaFormForDocking(GMForm).Flex.FileName);
end;

{ TScadaFormForDocking }

procedure TScadaFormForDocking.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Flex.EmptyDocument();
end;

procedure TScadaFormForDocking.FormCreate(Sender: TObject);
begin
  Flex.Color := GetSkinFormContentColor();
end;

function TScadaFormForDocking.GetControlFrame: TControlFrm;
begin
  Result := ControlFrm1;
end;

procedure TScadaFormForDocking.LoadScada(const fileName: string);
begin
  if (fileName = '') or not FileExists(fileName) then Exit;

  Flex.LoadFromFile(fileName);
  Caption := ExtractFileNameWithoutExt(fileName);
  AddFlexScadaChannels();
end;

procedure TScadaFormForDocking.sbCloseClick(Sender: TObject);
begin
  ControlFrm1.Visible := false;
end;

procedure TScadaFormForDocking.timerScriptTimer(Sender: TObject);
begin
  if not Flex.ModalDialogMode then
    RunScript();
end;

procedure TScadaFormForDocking.RunScript();
begin
  TScriptRunner.RunScript(Flex.Script.Text, Flex);
end;

procedure TScadaFormForDocking.WMShowControlPanel(var Msg: TMessage);
begin
  if (Msg.LParam <= 0) or not GMAggregates.IsObjectControllable(Msg.WParam) then
    ControlFrm1.Visible := false
  else
  begin
    ControlFrm1.Visible := true;
    ControlFrm1.ID_Obj := Msg.WParam;
  end;
end;

procedure TScadaFormForDocking.FlexScadaMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var obj: TGMObject;
    p: TPoint;
begin
  if Button = mbLeft then
  begin
    if Flex.MouseControl = nil then Exit;

    obj := FlexScadaMouseOverControlObject();
    if obj <> nil then
    begin
      ControlFrm1.Visible :=  true;
      ControlFrm1.ID_Obj := obj.ID_Obj;
    end
    else
    begin
      TScriptRunner.RunScript(Flex.MouseControl.ActionProp.Text, Flex, Flex.MouseControl);
    end;
  end
  else
  if Button = mbRight then
  begin
    p := Point(X, Y);
    p := Flex.ClientToScreen(p);
    dxBarPopupMenu1.Popup(p.X, p.Y);
  end;
end;

function TScadaFormForDocking.FlexScadaMouseOverControlObject(): TGMObject;
var id_obj: int;
begin
  Result := nil;
  if Flex.MouseControl = nil then Exit;

  id_obj := Flex.MouseControl.ControlObjectProp.Value;
  if id_obj <= 0 then Exit;

  Result := GMObjects.ObjByID(id_obj);
end;

function TScadaFormForDocking.FlexScadaMouseOverClickableObject(): bool;
begin
  Result := (FlexScadaMouseOverControlObject() <> nil)
            or
            ((Flex.MouseControl <> nil) and not Flex.MouseControl.ActionProp.StrList.IsEmpty());
end;

procedure TScadaFormForDocking.FlexScadaUpdateCursor(Sender: TObject; var ACursor: TCursor);
begin
  if FlexScadaMouseOverClickableObject() then
    ACursor := crHandPoint;
end;

procedure TScadaFormForDocking.AddFlexScadaChannels_OneControl(c: TFlexControl);
var i: int;
begin
  for i := 0 to c.ChannelsProp.LinesCount - 1 do
    TGMCOMThreadPool.AddReqCurrentsPrm(GMParamFromString(c.ChannelsProp.Names[i]));

  for i := 0 to c.Count - 1 do
    AddFlexScadaChannels_OneControl(c[i]);
end;

procedure TScadaFormForDocking.dxBarButton1Click(Sender: TObject);
begin
  EditMainForm.ShowModal(Flex.FileName);
  if (Flex.FileName <> '') and FileExists(Flex.FileName) then
  begin
    Flex.LoadFromFile(Flex.FileName);
    AddFlexScadaChannels();
  end;
end;

procedure TScadaFormForDocking.dxBarButton2Click(Sender: TObject);
begin
  if odScada.Execute(Handle) then
    Flex.LoadFromFile(odScada.FileName);
end;

procedure TScadaFormForDocking.AddFlexScadaChannels();
begin
  AddFlexScadaChannels_OneControl(Flex.Schemes);
end;

initialization
  RegisterClass(TScadaDockPanel);
end.
