unit Test.Script;

interface

uses Windows, TestFrameWork, GMGlobals, Classes, GMSQLQuery, ConnParamsStorage, SysUtils;

type
  TScriptTest = class(TTestCase)
  private
    qpg, qscript: TGMSQLQuery;
    params: TZConnectionParams;
    procedure RunScript;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RunScript_TwoTimes();
  end;

implementation

const test_db_name = 'gm_db_for_unit_test_1029384756';

{ TScriptTest }

procedure TScriptTest.RunScript();
begin
  qscript.Close();
  qscript.SQL.LoadFromFile('..\DB\pg_gm_db.sql');
  qscript.ExecSQL();
end;

procedure TScriptTest.RunScript_TwoTimes;
begin
  RunScript();
  RunScript();
  Check(true);
end;

procedure TScriptTest.SetUp;
begin
  inherited;

  params := GlobalSQLConnectionParams();
  params.Database := 'postgres';
  params.Login := 'postgres';
  params.Password := '123';
  qpg := TGMSQLQuery.Create(params);
  qpg.SQL.Text := 'select * from pg_database where datname = ' + QuotedStr(test_db_name);
  qpg.Open();
  if not qpg.Eof then
    qpg.Execute('drop database ' + test_db_name);

  qpg.Execute('create database ' + test_db_name);
  qpg.Execute('GRANT ALL ON DATABASE ' + test_db_name + ' TO GROUP "GM_Admin" WITH GRANT OPTION');

  params.Database := test_db_name;
  qscript := TGMSQLQuery.Create(params);
end;

procedure TScriptTest.TearDown;
begin
  inherited;

  qscript.Free();

  qpg.Execute('drop database ' + test_db_name);
  qpg.Free();
end;

initialization
  RegisterTest('Script', TScriptTest.Suite);
end.

