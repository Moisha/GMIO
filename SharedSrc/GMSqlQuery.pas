////////////////////////////////////////////
// ������� ��� Query � Connection � SQL
// ��������� ��� ������������ ����������� �������� �� MS SQL � Postgres 
////////////////////////////////////////////
unit GMSqlQuery;

interface

uses Windows, Classes, ZDataset, ZConnection, GMGlobals, DB, ConnParamsStorage, SysUtils;

{$ifndef SQL_APP}
  message ��� ������������� ������ define SQL_APP
{$endif}

type
  TGMSqlQuery = class
  private
    q: TZReadOnlyQuery;
    FConnectionParams: TZConnectionParams;
    FOwnsConnection: bool;
    function GetSQL: TStrings;
    function GetFields: TFields;
    function GetEof: bool;
    function GetState: TDataSetState;
    procedure Connect();
    procedure Connect_Own;
    procedure Connect_Thread;
    procedure UpdateConnectionParams(conn: TZConnection; params: TZConnectionParams);
    function GetThreadDescription: string;
    function CommentQueryText(const sql: string): string;
    procedure SetAppNameForConnection;
  public
    constructor Create(); overload;
    constructor Create(Params: TZConnectionParams); overload;
    destructor Destroy; override;

    property SQL: TStrings read GetSQL;
    property Fields: TFields read GetFields;
    property State: TDataSetState read GetState;
    function FieldByName(const FieldName: string): TField;

    procedure Open();
    procedure ExecSQL();
    procedure Execute(const s: string);
    procedure Next();
    procedure Close();

    property Eof: bool read GetEof;
  end;

  TReadGMQueryProc = procedure(q: TGMSqlQuery; obj: pointer) of object;
  TReadGMQueryProcRef = reference to procedure(q: TGMSqlQuery);

  procedure ReadFromQuery(const sql: string; proc: TReadGMQueryProc; obj: pointer = nil); overload;
  procedure ReadFromQuery(const sql: string; proc: TReadGMQueryProcRef); overload;
  procedure ReadFromQueryFmt(const sql: string; args: array of const; proc: TReadGMQueryProcRef);

  procedure ExecSQL(const sql: string);
  procedure ExecSQLFmt(const sql: string; args: array of const);
  procedure ExecPLSQL(const sql: string);
  procedure ExecPLSQLFmt(const sql: string; args: array of const);
  function QueryResult(const sql: string): string;
  function QueryResultFmt(const sql: string; args: array of const): string;
  function QueryResultArray_FirstRow(const sql: string): ArrayOfString;
  function QueryResultArray_FirstRowFmt(const sql: string; args: array of const): ArrayOfString;
  function QueryResultArray_FirstColumn(const sql: string): ArrayOfString;
  procedure DropThreadConnection(thrId: int);

implementation

uses
  System.Generics.Collections, Threads.Base;

var
 ThreadConnections: TDictionary<Cardinal, TZConnection>;

procedure ReadFromQuery(const sql: string; proc: TReadGMQueryProc; obj: pointer = nil);
var
  q: TGMSqlQuery;
begin
  q := TGMSqlQuery.Create();
  try
    q.SQL.Text := sql;
    q.Open();
    while not q.Eof do
    begin
      proc(q, obj);
      q.Next();
    end;
  finally
    q.Free();
  end;
end;

procedure ReadFromQuery(const sql: string; proc: TReadGMQueryProcRef);
var
  q: TGMSqlQuery;
begin
  q := TGMSqlQuery.Create();
  try
    q.SQL.Text := sql;
    q.Open();
    while not q.Eof do
    begin
      proc(q);
      q.Next();
    end;
  finally
    q.Free();
  end;
end;

procedure ReadFromQueryFmt(const sql: string; args: array of const; proc: TReadGMQueryProcRef);
begin
  ReadFromQuery(Format(sql, args), proc);
end;

procedure ExecSQL(const sql: string);
var
  q: TGMSqlQuery;
begin
  if sql.Trim() = '' then Exit;

  q := TGMSqlQuery.Create();
  try
    q.Execute(sql);
  finally
    q.Free();
  end;
end;

procedure ExecSQLFmt(const sql: string; args: array of const);
var
  s: string;
begin
  s := Format(sql, args);
  ExecSQL(s);
end;

procedure ExecPLSQL(const sql: string);
var s: string;
begin
  s := sql.Trim();
  if s = '' then Exit;

  if not s.EndsWith(';') then
    s := s + ';';

  s := 'do $$ begin ' + s + ' end $$';
  ExecSQL(s);
end;

procedure ExecPLSQLFmt(const sql: string; args: array of const);
var
  s: string;
begin
  s := Format(sql, args);
  ExecPLSQL(s);
end;

function QueryResultArray_FirstRow(const sql: string): ArrayOfString;
var q: TGMSqlQuery;
    i: int;
begin
  q := TGMSqlQuery.Create();
  SetLength(Result, 0);
  try
    q.SQL.Text := sql;
    q.Open();
    if not q.Eof then
    begin
      SetLength(Result, q.Fields.Count + 1);
      for i := 0 to q.Fields.Count - 1 do
        Result[i] := q.Fields[i].AsString;

      // � ��������� ������� ������� 1, ���� ������ ������ ������ ����� ������
      q.Next();
      if q.Eof then
        Result[q.Fields.Count] := '0'
      else
        Result[q.Fields.Count] := '1';
    end;
  finally
    q.Free();
  end;
end;

function QueryResultArray_FirstRowFmt(const sql: string; args: array of const): ArrayOfString;
begin
  Result := QueryResultArray_FirstRow(Format(sql, args));
end;

function QueryResultArray_FirstColumn(const sql: string): ArrayOfString;
var q: TGMSqlQuery;
    n: int;
begin
  q := TGMSqlQuery.Create();
  SetLength(Result, 0);
  try
    q.SQL.Text := sql;
    q.Open();
    if q.Eof or (q.Fields.Count = 0) then Exit;

    n := 0;
    while not q.Eof do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[n] := q.Fields[0].AsString;

      q.Next();
      inc(n);
    end;
  finally
    q.Free();
  end;
end;

function QueryResult(const sql: string): string;
var
  arr: ArrayOfString;
begin
  Result := '';
  arr := QueryResultArray_FirstRow(sql);
  if Length(arr) > 0 then
    Result := arr[0];
end;

function QueryResultFmt(const sql: string; args: array of const): string;
begin
  Result := QueryResult(Format(sql, args));
end;

{ TGMSqlQuery }

procedure TGMSqlQuery.Close;
begin
  q.Close();
end;

procedure TGMSqlQuery.UpdateConnectionParams(conn: TZConnection; params: TZConnectionParams);
begin
  conn.HostName := params.Host;
  conn.Port := params.Port;
  conn.User := params.Login;
  conn.Password := params.Password;
  conn.Database := params.Database;
  conn.Protocol := 'postgresql-9';
  conn.ClientCodepage := 'WIN1251';
  // ������ ��������, �� ������-�� ��� ������������ �������� ����������
  // �������� ������ �������� ����� SetAppNameForConnection
  // conn.Properties.Values['application_name'] := Paramstr(0);
  if params.LibraryLocation <> '' then
    conn.LibraryLocation := params.LibraryLocation;
end;

procedure TGMSqlQuery.SetAppNameForConnection;
begin
  if (q.Connection <> nil) and q.Connection.Connected then
    q.Connection.ExecuteDirect(CommentQueryText('set application_name to ' + AnsiQuotedStr(Paramstr(0), '"')));
end;

procedure TGMSqlQuery.Connect_Own();
begin
  if q.Connection = nil then
    q.Connection := TZConnection.Create(nil);

  if not q.Connection.Connected then
  begin
    UpdateConnectionParams(TZConnection(q.Connection), FConnectionParams);
    q.Connection.Connect();
    SetAppNameForConnection();
  end;
end;

procedure TGMSqlQuery.Connect_Thread();
var
  conn: TZConnection;
  thrId: cardinal;
begin
  thrId := GetCurrentThreadId();
  if not ThreadConnections.TryGetValue(thrId, conn) then
    conn := TZConnection.Create(nil);

  if not conn.Connected then
  begin
    UpdateConnectionParams(conn, GlobalSQLConnectionParams());
    conn.Connect();
  end;

  ThreadConnections.AddOrSetValue(thrId, conn);
  q.Connection := conn;
  SetAppNameForConnection();
end;

procedure TGMSqlQuery.Connect;
begin
  if (q.Connection <> nil) and q.Connection.Connected then
    Exit;

  if FOwnsConnection then
    Connect_Own()
  else
    Connect_Thread();
end;

constructor TGMSqlQuery.Create;
begin
  inherited;
  FConnectionParams := GlobalSQLConnectionParams();
  q := TZReadOnlyQuery.Create(nil);
  q.ParamCheck := false;
  FOwnsConnection := false;
end;

constructor TGMSqlQuery.Create(Params: TZConnectionParams);
begin
  Create();
  FConnectionParams := Params;
  FOwnsConnection := true;
end;

destructor TGMSqlQuery.Destroy;
begin
  if FOwnsConnection then
    q.Connection.Free();

  q.Free();

  inherited;
end;

procedure TGMSqlQuery.ExecSQL;
begin
  Connect();
  q.SQL.Text := CommentQueryText(q.SQL.Text);
  try
    q.ExecSQL();
  except
    q.Connection.Disconnect();
    Connect();
    q.ExecSQL();
  end;
end;

function TGMSqlQuery.GetThreadDescription(): string;
var
  thr: TThread;
begin
  Result := '';
  thr := TThread.CurrentThread;
  if thr = nil then
    Exit;

  if thr is TGMThread then
    Result := TGMThread(thr).Description;

  if Trim(Result) = '' then
    Result := thr.ClassName();

  Result := '/* ' + Result + ' */ ';
end;

function TGMSqlQuery.CommentQueryText(const sql: string): string;
var
  desc: string;
begin
  desc := GetThreadDescription();
  if (desc = '') or (Pos(AnsiUpperCase(desc), AnsiUpperCase(sql)) > 0) then
    Exit(sql);

  Result := desc + sql;
end;

procedure TGMSqlQuery.Execute(const s: string);
begin
  Connect();
  try
    q.Connection.ExecuteDirect(CommentQueryText(s));
  except
    q.Connection.Disconnect();
    Connect();
    q.Connection.ExecuteDirect(CommentQueryText(s));
  end;
end;

function TGMSqlQuery.FieldByName(const FieldName: string): TField;
begin
  Result := q.FieldByName(FieldName);
end;

function TGMSqlQuery.GetEof: bool;
begin
  Result := q.Eof;
end;

function TGMSqlQuery.GetFields: TFields;
begin
  Result := q.Fields;
end;

function TGMSqlQuery.GetSQL: TStrings;
begin
  Result := q.SQL;
end;

function TGMSqlQuery.GetState: TDataSetState;
begin
  Result := q.State;
end;

procedure TGMSqlQuery.Next;
begin
  q.Next();
end;

procedure TGMSqlQuery.Open;
begin
  Connect();

  if not q.ControlsDisabled then
    q.DisableControls();

  q.SQL.Text := CommentQueryText(q.SQL.Text);
  try
    q.Open();
  except
    q.Connection.Disconnect();
    Connect();
    q.Open();
  end;
end;

procedure FreeThreadConnections();
var
  conn: TZConnection;
begin
  for conn in ThreadConnections.Values do
    conn.Free();

  FreeAndNil(ThreadConnections);
end;

procedure DropThreadConnection(thrId: int);
var
  conn: TZConnection;
begin
  if (ThreadConnections = nil) or not ThreadConnections.TryGetValue(thrId, conn) then
    Exit;

  conn.Free();
  ThreadConnections.Remove(thrId);
end;

initialization
  ThreadConnections := TDictionary<Cardinal, TZConnection>.Create();
finalization
  FreeThreadConnections();
end.
