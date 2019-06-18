unit PasScript;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GMGlobals, FlexBase, ScriptRunner,
  OleServer, SynEditHighlighter, SynHighlighterPas, SynEdit,
  ExtCtrls, JvInterpreter, FlexProps,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, Vcl.Menus;

type
  TPascalDlg = class(TGMEnhancedScrollForm)
    mScript: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Panel1: TcxGroupBox;
    BitBtn3: TcxButton;
    BitBtn2: TcxButton;
    BitBtn1: TcxButton;
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FProp: TScriptProp;
    FParentControl: TFlexControl;
    FParentPanel: TFlexPanel;

    function CheckScript: bool;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TPascalDlg }

function TPascalDlg.CheckScript(): bool;
var runner: TScriptRunner;
begin
  Result := true;
  if Trim(mScript.Lines.Text) <> '' then
  begin
    runner := TScriptRunner.Create();
    runner.Script.Assign(mScript.Lines);
    runner.ParentObject := FParentControl;
    runner.ParentPanel := FParentPanel;
    Result := runner.Compile();
    if not Result then
      ShowMessageBox(runner.LastError, MB_ICONSTOP);
    runner.Free();
  end;
end;

procedure TPascalDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (ModalResult = mrOk) and (FProp <> nil) then
    FProp.Assign(mScript.Lines);
end;

procedure TPascalDlg.FormShow(Sender: TObject);
begin
  FProp := TScriptProp(Tag);
  if FProp <> nil then
  begin
    FParentControl := TFlexControl(FProp.Owner.Owner);
    FParentPanel := FParentControl.Owner;
    FProp.AssignTo(mScript.Lines);

    if Trim(FProp.Text) = '' then
    begin
      mScript.Lines.Add('unit ControlAction;');
      mScript.Lines.Add('');
      mScript.Lines.Add('procedure Main();');
      mScript.Lines.Add('begin');
      mScript.Lines.Add('');
      mScript.Lines.Add('end;');
      mScript.Lines.Add('');
      mScript.Lines.Add('end.');
    end;
  end;
end;

procedure TPascalDlg.BitBtn3Click(Sender: TObject);
begin
  if CheckScript() and (Sender = BitBtn3) then
  begin
    ShowMessageBox('Скрипт проверен. Oшибок не обнаружено', MB_ICONINFORMATION);
  end;
end;

procedure TPascalDlg.BitBtn2Click(Sender: TObject);
begin
  if not CheckScript() then Exit;

  ModalResult := mrOk;
end;

initialization
  RegisterDefaultPropEditForm(TScriptProp, TPascalDlg);
end.

