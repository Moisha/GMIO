unit fPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FlexBase, FlexUtils, Vcl.ExtCtrls, Vcl.StdCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  TfmPreview = class(TForm)
    Flex: TFlexPanel;
    tmrScript: TTimer;
    pnScriptError: TcxGroupBox;
    mScriptError: TcxMemo;
    procedure FormShow(Sender: TObject);
    procedure tmrScriptTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmPreview: TfmPreview;

implementation

{$R *.DFM}

uses ScriptRunner;

procedure TfmPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmrScript.Enabled := false;
end;

procedure TfmPreview.FormShow(Sender: TObject);
begin
 try
  Flex.ClientWidth := ScaleValue(Flex.DocWidth, 100);
  Flex.ClientHeight := ScaleValue(Flex.DocHeight, 100);
  ClientWidth := Flex.Width;
  ClientHeight := Flex.Height;
  Flex.Align := alClient;
  Flex.HorzScrollBar.Visible := True;
  Flex.VertScrollBar.Visible := True;
  if Flex.HorzScrollBar.IsScrollBarVisible or
     Flex.VertScrollBar.IsScrollBarVisible then
   WindowState := wsMaximized
  else begin
   Left := (Screen.Width - Width) div 2;
   Top := (Screen.Height - Height) div 2;
  end;
  Flex.ActiveScheme := Flex.DefaultScheme;
  tmrScript.Enabled := true;
 except
 end;
end;

procedure TfmPreview.tmrScriptTimer(Sender: TObject);
var runner: TScriptRunner;
begin
  if Flex.ModalDialogMode then Exit;

  runner := TScriptRunner.Create();
  try
    runner.Script.Text := Flex.Script.Text;
    runner.ParentPanel := Flex;
    if not runner.Run() then
    begin
      pnScriptError.Visible := true;
      mScriptError.Lines.Text := runner.LastError;
      tmrScript.Enabled := false;
    end;
  finally
    runner.Free();
  end;
end;

end.
