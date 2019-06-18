unit BigWindowForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Frame.CurrentsDiagramBase, Frame.BigWindow, GMDock, dxDockControl,
  IniFiles, GMGlobals, ControlFrame;

type
  TBigWindowFormForDocking = class(TGMForm)
    BigWindowFrame1: TBigWindowFrame;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TBigWindowDockPanel = class(TGMDockPanel)
  private
    function GetBigWindowIndex: int;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadLayoutFromCustomIni(AIniFile: TCustomIniFile; AParentForm: TCustomForm;
      AParentControl: TdxCustomDockControl; ASection: string); override;
    procedure SaveLayoutToCustomIni(AIniFile: TCustomIniFile; ASection: string); override;
    property BigWindowIndex: int read GetBigWindowIndex;
  end;

  function CreateDockableBigWindow(index: int): TGMDockPanel;

implementation

{$R *.dfm}

uses BigWindowData;

{ TBigWindowDockPanel }

constructor TBigWindowDockPanel.Create(AOwner: TComponent);
begin
  inherited;
  InsertForm(TBigWindowFormForDocking);
end;

function CreateDockableBigWindow(index: int): TGMDockPanel;
begin
  Result := TBigWindowDockPanel.Create(Application.MainForm);
  Result.Parent := Application.MainForm;
  TBigWindowFormForDocking(Result.GMForm).BigWindowFrame1.BigWindowIndex := index;
  Result.Caption := BigWindowList[index].Caption;
  Result.GMForm.Caption := Result.Caption;
end;

function TBigWindowDockPanel.GetBigWindowIndex: int;
begin
  Result := TBigWindowFormForDocking(GMForm).BigWindowFrame1.BigWindowIndex;
end;

procedure TBigWindowDockPanel.LoadLayoutFromCustomIni(AIniFile: TCustomIniFile; AParentForm: TCustomForm;
  AParentControl: TdxCustomDockControl; ASection: string);
var idx: int;
begin
  inherited;

  idx := AIniFile.ReadInteger(ASection, 'Index', -1);
  if (idx >= 0) and (idx < BigWindowList.Count) then
  begin
    TBigWindowFormForDocking(GMForm).BigWindowFrame1.BigWindowIndex  := idx;
    Caption := BigWindowList[idx].Caption;
    GMForm.Caption := Caption;
  end
  else
    Close();
end;

procedure TBigWindowDockPanel.SaveLayoutToCustomIni(AIniFile: TCustomIniFile; ASection: string);
begin
  inherited;
  AIniFile.WriteInteger(ASection, 'Index', GetBigWindowIndex());
end;

initialization
  RegisterClass(TBigWindowDockPanel);
end.
