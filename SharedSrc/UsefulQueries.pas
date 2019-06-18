////////////////////////////////////////////
// Часто встречающиеся запросы
////////////////////////////////////////////
unit UsefulQueries;

interface

uses Windows, GMGlobals, GMConst, GMSqlQuery;

const DEV_STATE_NAMES_IMP_PER_KWT = 'imp_per_kwt';
      DEV_STATE_NAMES_LAST_ONLINE = 'last_online';
      DEV_STATE_NAMES_LAST_ARCH_ADDR = 'last_arch_addr';
      DEV_STATE_NAMES_LAST_ARCH_UDT = 'last_arch_udt';
      DEV_STATE_NAMES_LAST_ARCH_REQ_FULL_DEPTH = 'last_arch_req_full_depth';
      DEV_STATE_NAMES_LAST_ARCH_REQ_MONTH_DEPTH = 'last_arch_req_month_depth';
      DEV_STATE_NAMES_LAST_ARCH_REQ_DAY_DEPTH = 'last_arch_req_day_depth';

      // поля SystemStates
      SYSTEM_STATES_SQL_QUEUE = 'svc_sql_queue';
      SYSTEM_STATES_PARSER_QUEUE = 'svc_parser_queue';

function SQLReq_IdObjByNCar(N_Car: int; ObjTypes: SetOfInt): int;
function SQLReq_NCarByIdObj(id_obj: int): int;
function SQLReq_ObjType(id_obj: int): int;
function SQLReq_ConfigParam(const stateName: string): string;
function SQLReq_GetIdPrmForRemotePrmId(const RemoteName: string; RemotePrmId: int): int;

procedure SQLReq_SetSystemState(const stateName, stateValue: string);
function SQLReq_GetSystemState(const paramName: string): string;

procedure SQLReq_SetDevState(id_device: int; const stateName, stateValue: string); overload;
procedure SQLReq_SetDevState(id_device: int; const stateName: string; stateValue: int); overload;
function SQLReq_GetDevState(id_device: int; const paramName: string): string;
function SQLReq_GetDevStateLastUpdate(id_device: int; const paramName: string): LongWord;

function SQLReq_SetChannel(id_prm: int; value: double; const userName: string = ''; TimeHold: int = 0; ID_Cmd_Remote: int = 0): int;
procedure SQLReq_SetCmdState(id_cmd, state: int);

function ChannelIdsFromQ(q: TGMSqlQuery): TChannelIds;
function LoadChannelIds(ID_Prm: int): TChannelIds;

function SqlText_LoadCommands(ID_Obj, ID_Device, ID_Prm: int): string;

implementation

uses SysUtils, StrUtils, ProgramLogFile, System.AnsiStrings;

procedure SQLReq_SetDevState(id_device: int; const stateName, stateValue: string);
begin
  try
    ExecPLSQL(Format('perform SetDevState(%d, %s, %s)', [id_device, QuotedStr(stateName), QuotedStr(stateValue)]));
  except
    on e: exception do
      ProgramLog.AddException('SQLReq_SetDevState - ' + e.Message);
  end;
end;

procedure SQLReq_SetDevState(id_device: int; const stateName: string; stateValue: int);
begin
  try
    SQLReq_SetDevState(id_device, stateName, IntToStr(stateValue));
  except
    on e: exception do
      ProgramLog.AddException('SQLReq_SetDevState - ' + e.Message);
  end;end;

function SQLReq_GetDevState(id_device: int; const paramName: string): string;
begin
  Result := QueryResult(Format('select StateValue from DevStates where ID_Device = %d and StateName = %s', [id_device, QuotedStr(paramName)]));
end;

function SQLReq_GetDevStateLastUpdate(id_device: int; const paramName: string): LongWord;
begin
  Result := StrToIntDef(QueryResult(Format('select LastOnline from DevStates where ID_Device = %d and StateName = %s', [id_device, QuotedStr(paramName)])), 0);
end;

procedure SQLReq_SetSystemState(const stateName, stateValue: string);
begin
  try
    ExecPLSQL(Format('perform SetSystemState(%s, %s)', [QuotedStr(stateName), QuotedStr(stateValue)]));
  except
    on e: exception do
      ProgramLog.AddException('SQLReq_SetSystemState - ' + e.Message);
  end;
end;

function SQLReq_GetSystemState(const paramName: string): string;
begin
  Result := QueryResult('select StateValue from SystemStates where StateName = ' + QuotedStr(paramName));
end;

function SQLReq_IdObjByNCar(N_Car: int; ObjTypes: SetOfInt): int;
var res, types, sql: string;
begin
  Result := INCORRECT_VALUE;

  types := SetOfIntCommaList(ObjTypes);

  if types <> '' then
  begin
    sql := 'select ID_Obj from Objects where N_Car = ' + IntToStr(N_Car) + ' and coalesce(ObjType, 0) in (' + types + ')';
    res := QueryResult(sql);
    Result := StrToIntDef(res, Result);
  end;
end;

function SQLReq_NCarByIdObj(id_obj: int): int;
var res, sql: string;
begin
  sql := 'select N_Car from Objects where ID_Obj = ' + IntToStr(id_obj);
  res := QueryResult(sql);
  Result := StrToIntDef(res, INCORRECT_VALUE);
end;

function SQLReq_ObjType(id_obj: int): int;
var res, sql: string;
begin
  sql := 'select coalesce(ObjType, 0) from Objects where ID_Obj = ' + IntToStr(id_obj);
  res := QueryResult(sql);
  Result := StrToIntDef(res, INCORRECT_VALUE);
end;

function SQLReq_ConfigParam(const stateName: string): string;
begin
  Result := QueryResultFmt('select GetConfigPrm(%s)', [QuotedStr(stateName)]);
end;

function SQLReq_SetChannel(id_prm: int; value: double; const userName: string = ''; TimeHold: int = 0; ID_Cmd_Remote: int = 0): int;
var
  res: string;
begin
  res := QueryResultFmt(
    'select SetChannelValue(%d, %s, %s, %s, %s)',
     [id_prm,
      MyFloatToStr(value),
      IfThen(userName <> '', QuotedStr(userName), 'null'),
      IfThen(TimeHold > 0, IntToStr(TimeHold), 'null'),
      IfThen(ID_Cmd_Remote > 0, IntToStr(ID_Cmd_Remote), 'null')
     ]);
  Result := StrToIntDef(res, 0);
end;

procedure SQLReq_SetCmdState(id_cmd, state: int);
begin
  ExecSQLFmt('update Commands set State = %d where ID_Cmd = %d', [state, id_cmd])
end;

function ChannelIdsFromQ(q: TGMSqlQuery): TChannelIds;
begin
  Result.ID_Src := q.FieldByName('ID_Src').AsInteger;
  Result.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
  Result.ID_PT := q.FieldByName('ID_PT').AsInteger;
  Result.N_Src := q.FieldByName('N_Src').AsInteger;
  if Result.N_Src <= 0 then
    Result.N_Src := q.FieldByName('CurrentsAddr').AsInteger;
  Result.ID_DevType := q.FieldByName('ID_DevType').AsInteger;
  Result.ID_Device := q.FieldByName('ID_Device').AsInteger;
  Result.ID_Obj := q.FieldByName('ID_Obj').AsInteger;
  Result.ObjType := q.FieldByName('ObjType').AsInteger;
  Result.NDevNumber := q.FieldByName('Number').AsInteger;
  Result.PrmExtData := q.FieldByName('ExtData').AsString;
  Result.MainSrvPrmID := q.FieldByName('MainSrvPrmID').AsInteger;
  Result.UserSrc := q.FieldByName('UserSrc').AsInteger;
  Result.AddrBase := q.FieldByName('AddrBase').AsInteger;
  Result.RemoteName := q.FieldByName('RemoteName').AsString;
end;

function LoadChannelIds(ID_Prm: int): TChannelIds;
var
  q: TGMSqlQuery;
begin
  Result.ID_Prm := -1;
  q := TGMSqlQuery.Create();
  try
    q.SQL.Text := 'select * from DeviceSystem where ID_Prm = ' + IntToStr(ID_Prm);
    q.Open();
    if not q.Eof then
      Result := ChannelIdsFromQ(q);
  finally
    q.Free();
  end;
end;

function SQLReq_GetIdPrmForRemotePrmId(const RemoteName: string; RemotePrmId: int): int;
begin
  Result := StrToIntDef(QueryResultFmt('select ID_Prm from DeviceSystem where RemoteName = %s and PrmRemoteID = %d', [QuotedStr(RemoteName), RemotePrmId]), 0);
end;

function SqlText_LoadCommands(ID_Obj, ID_Device, ID_Prm: int): string;
var
  condition: string;
begin
  if ID_Prm > 0 then
    condition := 'where c.ID_Prm = ' + IntToStr(ID_Prm)
  else
  if ID_Device > 0 then
    condition := 'where d.ID_Device = ' + IntToStr(ID_Device)
  else
  if ID_Obj > 0 then
    condition := 'where d.ID_Obj = ' + IntToStr(ID_Obj);

  Result := Format(
    #13#10'select c1.*, d.* from' +
    #13#10'  (select ID_Prm, Max(ID_Cmd) as ID_Cmd' +
    #13#10'   from Commands' +
    #13#10'     where coalesce(State, %d) = %d' +
    #13#10'     group by ID_Prm) c' +
    #13#10'  join Commands c1 on c1.ID_Cmd = c.ID_Cmd' +
    #13#10'  join DeviceSystem d on c.ID_Prm = d.ID_Prm' +
    #13#10'  ' + condition +
    #13#10'  order by c1.ID_Cmd'  ,
    [COMMAND_STATE_PENDING, COMMAND_STATE_PENDING]);
end;

end.
