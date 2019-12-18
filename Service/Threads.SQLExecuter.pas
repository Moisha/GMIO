unit Threads.SQLExecuter;

interface

uses Windows, SysUtils, Classes, Threads.Base, GMGlobals, GMConst, Math;

const QUERY_PACK_SIZE = 50;

type
  TSQLExecuteThread = class(TGMThread)
  private
    FSQL: TStringList;
    FSynch: TMultiReadExclusiveWriteSynchronizer;
    function AddSemicolonAndCRLF(const s: string): string;
    function GetSqlRange(n: int): string;
    function DelSqlRange(start, count: int): string;
  protected
    property Synch: TMultiReadExclusiveWriteSynchronizer read FSynch;
    procedure SafeExecute(); override;
    procedure SafeExecute_ExtraStep; virtual;
    function GetCount: int; virtual;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Add(const s: string);
    procedure AddList(sl: TStrings);

    property Count: int read GetCount;
  end;

implementation

{ TSQLExecuteThread }

uses GMSqlQuery, Winapi.ActiveX;

procedure TSQLExecuteThread.Add(const s: string);
begin
  if Terminated then Exit;

  FSynch.BeginWrite();
  try
    FSQL.Add(s);
  finally
    FSynch.EndWrite();
  end;
end;

procedure TSQLExecuteThread.AddList(sl: TStrings);
begin
  if Terminated then Exit;

  FSynch.BeginWrite();
  try
    FSQL.AddStrings(sl);
  finally
    FSynch.EndWrite();
  end;
end;

constructor TSQLExecuteThread.Create;
begin
  inherited Create();

  FSQL := TStringList.Create();
  FSynch := TMultiReadExclusiveWriteSynchronizer.Create();
end;

destructor TSQLExecuteThread.Destroy;
begin
  inherited;

  try
    if FSQL.Count > 0 then
      FSQL.SaveToFile('sql_' + IntToStr(NowGM()) + '.sql');
  except end;

  FSQL.Free();
  FSynch.Free();
end;

function TSQLExecuteThread.GetCount: int;
begin
  FSynch.BeginRead();
  try
    Result := FSQL.Count;
  finally
    FSynch.EndRead();
  end;
end;

function TSQLExecuteThread.AddSemicolonAndCRLF(const s: string): string;
begin
  Result := s;
  while (Result <> '') and (CharInSet(Result[Result.Length], [#13, #10, ';', ' '])) do
    Delete(Result, Result.Length, 1);

  Result := Result + ';'#13#10;
end;

procedure TSQLExecuteThread.SafeExecute_ExtraStep;
begin

end;

function TSQLExecuteThread.GetSqlRange(n: int): string;
var
  i: int;
begin
  Result := '';
  FSynch.BeginRead();
  try
    for i := 0 to n - 1 do
      Result := Result + AddSemicolonAndCRLF(FSQL[i]);
  finally
    FSynch.EndRead();
  end;
end;

function TSQLExecuteThread.DelSqlRange(start, count: int): string;
var
  i: int;
begin
    FSynch.BeginWrite();
    try
      for i := start to start + count - 1 do
      begin
        if FSQL.Count = 0 then Exit;

        FSQL.Delete(0);
      end;
    finally
      FSynch.EndWrite();
    end;
end;

procedure TSQLExecuteThread.SafeExecute;
var
  sql: string;
  n: int;
begin
  CoInitialize(nil);
  while not Terminated do
  begin
    SafeExecute_ExtraStep();

    if FSQL.Count = 0 then
    begin
      SleepThread(100);
      continue;
    end;

    n := Min(QUERY_PACK_SIZE, FSQL.Count);
    sql := GetSqlRange(n);

    try
      ExecPLSQL(sql);
    except
      SaveStringToFile(sql, 'sqlerr_' + IntToStr(NowGM()) + '.sql');
    end;

    DelSqlRange(0, n);
  end;
end;

end.
