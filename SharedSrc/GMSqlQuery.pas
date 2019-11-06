////////////////////////////////////////////
// Обертка над Query и Connection к SQL
// Требуется для относительно бескровного перехода от MS SQL к Postgres 
////////////////////////////////////////////
unit GMSqlQuery;

interface

uses Windows, Classes, ZDataset, ZConnection, GMGlobals, DB, ConnParamsStorage, SysUtils;

type
  TGMSqlQuery = class
  private
    q: TZReadOnlyQuery;
    FConnectionParams: TZConnectionParams;
    function GetSQL: TStrings;
    function GetFields: TFields;
    function GetEof: bool;
    function GetState: TDataSetState;
    procedure Connect();
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

implementation

uses
  System.Generics.Collections;

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

      // в последнюю колонку запишем 1, если запрос вернул больше одной строки
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

procedure TGMSqlQuery.Connect;
var
  conn: TZConnection;
  thrId: cardinal;
begin
  if (q.Connection <> nil) and q.Connection.Connected then
    Exit;

  thrId := GetCurrentThreadId();
  if not ThreadConnections.TryGetValue(thrId, conn) then
    conn := TZConnection.Create(nil);

  if not conn.Connected then
  begin
    if FConnectionParams.Host = '' then
      FConnectionParams := GlobalSQLConnectionParams();

    conn.HostName := FConnectionParams.Host;
    conn.Port := FConnectionParams.Port;
    conn.User := FConnectionParams.Login;
    conn.Password := FConnectionParams.Password;
    conn.Database := FConnectionParams.Database;
    conn.Protocol := 'postgresql-9';
    conn.ClientCodepage := 'WIN1251';
    if FConnectionParams.LibraryLocation <> '' then
      conn.LibraryLocation := FConnectionParams.LibraryLocation;

    conn.Connect();
  end;

  ThreadConnections.AddOrSetValue(thrId, conn);
  q.Connection := conn;
end;

constructor TGMSqlQuery.Create;
begin
  inherited;
  FConnectionParams := GlobalSQLConnectionParams();
  q := TZReadOnlyQuery.Create(nil);
  q.ParamCheck := false;
end;

constructor TGMSqlQuery.Create(Params: TZConnectionParams);
begin
  Create();
  FConnectionParams := Params;
end;

destructor TGMSqlQuery.Destroy;
begin
  q.Free();
  inherited;
end;

procedure TGMSqlQuery.ExecSQL;
begin
  Connect();
  try
    q.ExecSQL();
  except
    q.Connection.Disconnect();
    Connect();
    q.ExecSQL();
  end;
end;

procedure TGMSqlQuery.Execute(const s: string);
begin
  Connect();
  try
    q.Connection.ExecuteDirect(s);
  except
    q.Connection.Disconnect();
    Connect();
    q.Connection.ExecuteDirect(s);
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

  ThreadConnections.Free();
end;

initialization
  ThreadConnections := TDictionary<Cardinal, TZConnection>.Create();
finalization
  FreeThreadConnections();
end.
