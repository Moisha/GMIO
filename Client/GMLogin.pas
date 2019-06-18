////////////////////////////////////////////
// Запрос логина и пароля на доступ к управлению
////////////////////////////////////////////
unit GMLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GMGlobals,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus;

type
  TLoginDlg = class(TGMEnhancedScrollForm)
    btOk: TcxButton;
    Button2: TcxButton;
    leLogin: TcxTextEdit;
    lePassword: TcxTextEdit;
    Label1: TcxLabel;
    Label2: TcxLabel;
    procedure FormShow(Sender: TObject);
    procedure btOkClick(Sender: TObject);
  private
    function CheckCredentials: bool;
    { Private declarations }
  public
    { Public declarations }
    function ShowModal(): TModalResult; reintroduce;
  end;

var
  LoginDlg: TLoginDlg;

implementation

uses Threads.GMClient;

{$R *.dfm}

{ TLoginDlg }

function TLoginDlg.CheckCredentials(): bool;
var order: TDataOrder;
begin
  order := TDataOrders.Add();
  order.OrderType := dotRequestControl;
  order.SQL := leLogin.Text + #13 + lePassword.Text;
  order.State := dosWaiting;
  Result := order.WaitFor(20000);

  if not Result or (Length(order.lDiagram) = 0) then
  begin
    ShowMessageBox('Обрыв связи!', MB_ICONSTOP);
    Result := false;
  end
  else
  if order.lDiagram[0].Val = 0 then
  begin
    ShowMessageBox('Неверный логин/пароль!', MB_ICONSTOP);
    Result := false;
  end;
end;

function TLoginDlg.ShowModal: TModalResult;
begin
  lePassword.Text := '';
  Result := inherited ShowModal();
end;

procedure TLoginDlg.FormShow(Sender: TObject);
begin
  leLogin.SetFocus();
end;

procedure TLoginDlg.btOkClick(Sender: TObject);
begin
  if CheckCredentials() then
    ModalResult := mrOk; 
end;

end.
