unit User;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dmSkin, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSkinOffice2010Silver,
  dxSkinscxPCPainter, cxClasses, dxLayoutContainer, dxLayoutControl, Vcl.Menus, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  cxTextEdit, Vcl.StdCtrls, cxButtons, GMGlobals, cxMaskEdit, cxDropDownEdit;

type
  TUserDlg = class(TForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    cxButton1: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    cxButton2: TcxButton;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    eLogin: TcxTextEdit;
    dxLayoutItem3: TdxLayoutItem;
    ePassword: TcxTextEdit;
    dxLayoutItem4: TdxLayoutItem;
    ePassword2: TcxTextEdit;
    dxLayoutItem5: TdxLayoutItem;
    cmbUserState: TcxComboBox;
    dxLayoutItem6: TdxLayoutItem;
    procedure cxButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FUserId: int;
    procedure LoadUser;
    procedure Clear;
    function CheckUserName: bool;
    function CheckPassword: bool;
    function GetLogin: string;
    function GetPassword: string;
    function GetUserState: int;
  public
    class procedure UserStateList(sl: TStrings);
    function ShowModal(UserId: int): TModalResult; reintroduce;
    property Login: string read GetLogin;
    property UserState: int read GetUserState;
    property Password: string read GetPassword;
  end;

var
  UserDlg: TUserDlg;

implementation

uses
  GMSqlQuery, GMConst;

{$R *.dfm}

{ TUserDlg }

procedure TUserDlg.Clear();
begin
  eLogin.Text := '';
  ePassword.Text := '';
  ePassword2.Text := '';
  cmbUserState.ItemIndex := 0;
end;

function TUserDlg.CheckUserName(): bool;
begin
  if FUserId > 0 then
    Exit(true);

  if Trim(eLogin.Text) = '' then
  begin
    ShowMessageBox('Введите имя пользователя!', MB_ICONEXCLAMATION);
    Exit(false);
  end;

  if QueryResultFmt('select Login from Users where Login = %s', [QuotedStr(Trim(eLogin.Text))]) <> '' then
  begin
    ShowMessageBox('Пользователь с таким именем уже есть!', MB_ICONEXCLAMATION);
    Exit(false);
  end;

  Result := true;
end;

function TUserDlg.CheckPassword(): bool;
begin
  if (ePassword.Text = '') or (ePassword2.Text = '') then
  begin
    ShowMessageBox('Введите и подтвердите пароль!', MB_ICONEXCLAMATION);
    Exit(false);
  end;

  if not AnsiSameStr(ePassword.Text, ePassword2.Text) then
  begin
    ShowMessageBox('Пароль и подтверждение не совпадают!', MB_ICONEXCLAMATION);
    Exit(false);
  end;

  Result := true;
end;

procedure TUserDlg.cxButton1Click(Sender: TObject);
begin
  if not CheckUserName() or not CheckPassword() then
    Exit;

  ModalResult := mrOk;
end;

procedure TUserDlg.FormCreate(Sender: TObject);
begin
  UserStateList(cmbUserState.Properties.Items);
end;

function TUserDlg.GetLogin: string;
begin
  Result := Trim(eLogin.Text);
end;

function TUserDlg.GetPassword: string;
begin
  Result := ePassword.Text;
end;

function TUserDlg.GetUserState: int;
begin
  Result := CmbSelectedObj(cmbUserState);
end;

procedure TUserDlg.LoadUser();
var
  a: ArrayOfString;
begin
  a := QueryResultArray_FirstRowFmt('select Login, State from Users where ID_User = ', [FUserId]);
  eLogin.Text := a[0];
  SelectCmbObject(cmbUserState, StrToIntDef(a[1], USER_STATE_COMMON), 0);
end;

function TUserDlg.ShowModal(UserId: int): TModalResult;
begin
  Clear();

  FUserId := UserId;
  eLogin.Enabled := FUserId <= 0;
  cmbUserState.Enabled := FUserId <= 0;
  if FUserId > 0 then
    LoadUser();

  Result := inherited ShowModal();
end;

class procedure TUserDlg.UserStateList(sl: TStrings);
begin
  sl.AddObject('Пользователь', TObject(USER_STATE_COMMON));
  sl.AddObject('Администратор', TObject(USER_STATE_ADMIN));
  sl.AddObject('Группа пользователей', TObject(USER_STATE_GROUP));
  sl.AddObject('Группа администраторов', TObject(USER_STATE_ADMIN_GROUP));
end;

end.
