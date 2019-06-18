unit UserPermissions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, dmSkin,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSkinOffice2010Silver,
  dxSkinscxPCPainter, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutContainer, cxLabel, dxLayoutLookAndFeels, cxClasses, dxLayoutControl,
  dxLayoutControlAdapters, Vcl.Menus, cxCheckListBox, Vcl.StdCtrls, cxButtons, cxTextEdit, cxMaskEdit, cxDropDownEdit, GMGlobals;

type
  TUsersDlg = class(TForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    labelState: TcxLabel;
    dxLayoutItem1: TdxLayoutItem;
    cmbUsers: TcxComboBox;
    dxLayoutItem2: TdxLayoutItem;
    buttonAddUser: TcxButton;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    cxButton2: TcxButton;
    dxLayoutItem4: TdxLayoutItem;
    clbRoles: TcxCheckListBox;
    dxLayoutItem5: TdxLayoutItem;
    cxButton3: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutGroupUserProperties: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    cmbUserState: TcxComboBox;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    clbObjects: TcxCheckListBox;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutSplitterItem1: TdxLayoutSplitterItem;
    cxButton4: TcxButton;
    dxLayoutItem9: TdxLayoutItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbUsersPropertiesChange(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure buttonAddUserClick(Sender: TObject);
    procedure clbRolesClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
    procedure clbObjectsClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  private
    FLoading: bool;
    procedure LoadUsers;
    procedure UpdateSecurityState;
    procedure LoadRoles;
    procedure LoadObjects;
    function GetUserId: int;
    procedure UpdateObjectPermissions;
    procedure UpdateRoles;
    procedure UpdateUserState;
    procedure UpdateClbFromQuery(clb: TcxCheckListBox; const sql, fieldId: string);
    procedure CheckRolesEnabled;
    property UserId: int read GetUserId;
  public
    { Public declarations }
  end;

var
  UsersDlg: TUsersDlg;

implementation

uses
  GMSqlQuery, UsefulQueries, GMConst, User;

{$R *.dfm}

procedure TUsersDlg.LoadUsers();
begin
  cmbUsers.Properties.Items.Clear();
  ReadFromQuery('select * from Users order by Login',
    procedure (q: TGMSqlQuery)
    begin
      cmbUsers.Properties.Items.AddObject(q.FieldByName('Login').AsString, pointer(q.FieldByName('ID_User').AsInteger))
    end);

  if cmbUsers.Properties.Items.Count > 0 then
    cmbUsers.ItemIndex := 0;
end;

procedure TUsersDlg.LoadRoles();
var
  item: TcxCheckListBoxItem;
begin
  clbRoles.Items.Clear();
  ReadFromQueryFmt('select * from Users where State in (%d, %d) order by Login', [USER_STATE_GROUP, USER_STATE_ADMIN_GROUP],
    procedure (q: TGMSqlQuery)
    begin
      item := clbRoles.Items.Add();
      item.Text := q.FieldByName('Login').AsString;
      item.Tag := q.FieldByName('ID_User').AsInteger;
    end);
end;

procedure TUsersDlg.LoadObjects();
var
  item: TcxCheckListBoxItem;
begin
  clbObjects.Items.Clear();
  ReadFromQuery('select * from Objects order by Name',
    procedure (q: TGMSqlQuery)
    begin
      item := clbObjects.Items.Add();
      item.Text := q.FieldByName('Name').AsString + '(' + q.FieldByName('N_Car').AsString + ')';
      item.Tag := q.FieldByName('ID_Obj').AsInteger;
    end);
end;

procedure TUsersDlg.UpdateSecurityState();
begin
  if SQLReq_ConfigParam(SYS_CONFIG_PARAM_AUTH_REQUIRED) = '' then
    labelState.Caption := 'Âõîä áåç ïàðîëÿ ÐÀÇÐÅØÅÍ'
  else
    labelState.Caption := 'Âõîä áåç ïàðîëÿ ÇÀÏÐÅÙÅÍ';
end;

procedure TUsersDlg.clbObjectsClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  if FLoading then
    Exit;

  if ANewState = cbsChecked then
    ExecSQLFmt('insert into ObjectPermissions(ID_User, ID_Obj) select %d, %d', [UserId, clbObjects.Items[AIndex].Tag])
  else
    ExecSQLFmt('delete from ObjectPermissions where ID_User = %d and ID_Obj = %d', [UserId, clbObjects.Items[AIndex].Tag]);
end;

procedure TUsersDlg.clbRolesClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  if FLoading then
    Exit;

  if ANewState = cbsChecked then
    ExecSQLFmt('insert into UserGroups(ID_User, ID_Group) select %d, %d', [UserId, clbRoles.Items[AIndex].Tag])
  else
    ExecSQLFmt('delete from UserGroups where ID_User = %d and ID_Group = %d', [UserId, clbRoles.Items[AIndex].Tag]);
end;

procedure TUsersDlg.cmbUsersPropertiesChange(Sender: TObject);
begin
  dxLayoutGroupUserProperties.Enabled := UserId > 0;
  if UserId <= 0 then
    Exit;

  FLoading := true;
  try
    UpdateUserState();
    UpdateRoles();
    UpdateObjectPermissions();
  finally
    FLoading := false;
  end;
end;

procedure TUsersDlg.UpdateUserState();
begin
  SelectCmbObject(cmbUserState, StrToIntDef(QueryResultFmt('select State from Users where ID_User = %d', [UserId]), USER_STATE_COMMON));
end;

procedure TUsersDlg.UpdateClbFromQuery(clb: TcxCheckListBox; const sql, fieldId: string);
var
  i: int;
begin
  for i := 0 to clb.Items.Count - 1 do
    clb.Items[i].Checked := false;

  ReadFromQuery(sql,
    procedure(q: TGMSqlQuery)
    var
      i: int;
    begin
      for i := 0 to clb.Items.Count - 1 do
        if clb.Items[i].Tag = q.FieldByName(fieldId).AsInteger then
        begin
          clb.Items[i].Checked := true;
          break;
        end;
    end);
end;

procedure TUsersDlg.CheckRolesEnabled();
var
  i: int;
begin
  for i := 0 to clbRoles.Items.Count - 1 do
    clbRoles.Items[i].Enabled := true;

  ReadFromQueryFmt('select *, CanAddUserToGroup(%d, ID_User) as Allow from Users', [UserId],
    procedure (q: TGMSqlQuery)
    var
      i: int;
    begin
      for i := 0 to clbRoles.Items.Count - 1 do
        if clbRoles.Items[i].Tag = q.FieldByName('ID_User').AsInteger then
        begin
          clbRoles.Items[i].Enabled := q.FieldByName('Allow').AsInteger = 1;
          break;
        end;
    end);
end;

procedure TUsersDlg.UpdateRoles();
begin
  UpdateClbFromQuery(clbRoles, Format('select * from UserGroups where ID_User = %d', [UserId]), 'ID_Group');
  CheckRolesEnabled();
end;

procedure TUsersDlg.UpdateObjectPermissions();
begin
  UpdateClbFromQuery(clbObjects, Format('select * from ObjectPermissions where ID_User = %d', [UserId]), 'ID_Obj');
end;

procedure TUsersDlg.buttonAddUserClick(Sender: TObject);
begin
  if UserDlg.ShowModal(-1) <> mrOk then Exit;

  ExecSQLFmt('insert into Users(Login, Password, State) select %s, %s, %d',
    [QuotedStr(UserDlg.Login), QuotedStr(CalcMD5(UserDlg.Password)), UserDlg.UserState]);

  LoadUsers();
  cmbUsers.ItemIndex := cmbUsers.Properties.Items.IndexOf(UserDlg.Login);
end;

procedure TUsersDlg.cxButton2Click(Sender: TObject);
begin
  if UserId <= 0 then
    Exit;

  if ShowMessageBox('Óäàëèòü ïîëüçîâàòåëÿ ' + cmbUsers.Text, MB_ICONEXCLAMATION or MB_YESNO) <> IDYES then
    Exit;

  ExecSQLFmt('delete from Users where ID_User = %d', [UserId]);
end;

procedure TUsersDlg.FormCreate(Sender: TObject);
begin
  TUserDlg.UserStateList(cmbUserState.Properties.Items);
end;

procedure TUsersDlg.FormShow(Sender: TObject);
begin
  dxLayoutGroupUserProperties.Enabled := false;
  UpdateSecurityState();
  LoadRoles();
  LoadObjects();
  LoadUsers();
end;

function TUsersDlg.GetUserId: int;
begin
  Result := CmbSelectedObj(cmbUsers);
end;

end.
