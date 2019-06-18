////////////////////////////////////////////
// Модуль проверки соединения с SQL
////////////////////////////////////////////
unit CheckConnStr;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, ADODB, GMGlobals, GMConst, ActiveX, GMSqlQuery,
  Threads.Base, ConnParamsStorage;

{$I ScriptVersion.inc}

type
  TCheckConnStrThread = class(TGMThread)
  private
    FConnParams: TZConnectionParams;
    procedure PostErrorMsg(const msg: string);
  protected
    procedure SafeExecute; override;
  public
    bFinished: bool;
    constructor Create(Params: TZConnectionParams);
  end;     

  TCheckConnStrDlg = class(TForm)
    Button1: TButton;
    lState: TLabel;
    Label2: TLabel;
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    thr: TCheckConnStrThread;
    procedure WMUpdateCOMLog(var Msg: TMessage); message WM_SQL_CONNECTION_STATE;
  public
    { Public declarations }
    function CheckConnectionString(Params: TZConnectionParams): bool;
  end;

var
  CheckConnStrDlg: TCheckConnStrDlg;

implementation

uses ComObj;

{$R *.dfm}

{ TCheckConnStrThread }

constructor TCheckConnStrThread.Create(Params: TZConnectionParams);
begin
  inherited Create(false);
  FConnParams := Params;
  bFinished := false;
end;

procedure TCheckConnStrThread.PostErrorMsg(const msg: string);
begin
  PostMessage(CheckConnStrDlg.Handle, WM_SQL_CONNECTION_STATE, WParam(TStringClass.Create(msg)), 0);
end;

procedure TCheckConnStrThread.SafeExecute;
var q: TGMSqlQuery;
    action, ver: string;
begin
  CoInitialize(nil);
  q := TGMSqlQuery.Create();
  q.ConnectionParams := FConnParams;

  repeat
    try
      action := 'Connecting';
      q.SQL.Text := 'select 1';
      q.Open();

      action := 'Check script version';
      q.SQL.Text := 'select PrmValue from SysConfig where PrmName = ''ScriptVersion''';
      q.Open();
      if q.Eof or (Trim(q.Fields[0].AsString) = '') then
        raise Exception.Create('Не обнаружена версия скрипта. Прогоните последний скрипт.');

      ver := Trim(q.Fields[0].AsString);
      if StrToIntDef(ver, 0) < MIN_SCRIPT_VERSION then
      begin
        PostErrorMsg(Format('Текущая версия скрипта БД (%s) не подходит.'#13#10 +
                            'Прогоните скрипт версии не ниже %d.', [ver, MIN_SCRIPT_VERSION]));
      end
      else
      begin
        PostMessage(CheckConnStrDlg.Handle, WM_SQL_CONNECTION_STATE, 0, 0);
        break;
      end;
    except
      on e: Exception do
        PostErrorMsg(action + #13#10 + e.Message)
    end;
  until Terminated;

  bFinished := true;
  q.Free();
end;

{ TCheckConnStrDlg }

function TCheckConnStrDlg.CheckConnectionString(Params: TZConnectionParams): bool;
begin
  thr := TCheckConnStrThread.Create(Params);
  Result := (ShowModal() = mrOk);
  thr.Terminate(); // тут будет утечка памяти, если вышли по кнопке Отмена
                   // ждать завершения смысла нет, пускай будет утечка
end;

procedure TCheckConnStrDlg.WMUpdateCOMLog(var Msg: TMessage);
var sc: TStringClass;
begin
  if Msg.WParam = 0 then
    ModalResult := mrOk
  else
  begin
    sc := TStringClass(Msg.WParam);
    lState.Caption := sc.s;
    sc.Free();
  end;
end;

procedure TCheckConnStrDlg.FormDestroy(Sender: TObject);
begin
  // Если поток закончил работу, то убьем его.
  // Если мы прервали ожидание, то нет смысла ждать завершения, проще пусть утечет немного памяти на выходе.
  if thr.bFinished then
    thr.Free();
end;

end.
