unit Threads.Pool;

interface

uses Windows, Classes, GMGlobals, Generics.Collections, Threads.Base, GmSqlQuery,
     Threads.ReqSpecDevTemplate, SysUtils, GMConst, GM485;

type
  TMultiObjectRequestThread = class(TGMThread)
  private
    FThreadPool: TGMThreadPool<TRequestSpecDevices>;

    procedure UpdateOneObjectThread(q: TGMSqlQuery; obj: pointer);
    procedure DropOddThreads;
    procedure DropUpdateFlag;
    procedure AddMissingThreads;
    function CreateRequestThread(objType, id_obj: int): TRequestSpecDevices;
    function CheckOutputChannelIds(id_prm: int; channelIds: TChannelIds): bool;
    function SendRemoteCommand(ID_Prm: int; value: double; const userName: string = ''): bool;
    function SetChannelValue_Geomer(channelIds: TChannelIds; fVal: double; TimeHold: UINT; const userName: string = ''): bool;
    procedure DropStoppedThreads;
  protected
    procedure SafeExecute(); override;
    procedure UpdateThreadList; virtual;
    property ThreadPool: TGMThreadPool<TRequestSpecDevices> read FThreadPool;
  public
    constructor Create();
    destructor Destroy(); override;
    function SetChannelValue(id_prm: int; Val: double; timeHold: int = 0; age: int = -1): bool;
    function FindThread(objType, ID_Obj: int): TRequestSpecDevices;
  end;

implementation

uses Threads.GMCOM, Threads.TCP, Threads.GMK104, Threads.MainSrvDataPicker, UsefulQueries, GMSocket, ProgramLogFile, System.StrUtils;

{ TMultiObjectRequestThread }

const THREAD_TAG_UP_TO_DATE = 0;
      THREAD_TAG_ODD = 1;

constructor TMultiObjectRequestThread.Create();
begin
  inherited Create();
  FThreadPool := TGMThreadPool<TRequestSpecDevices>.Create();
end;

destructor TMultiObjectRequestThread.Destroy;
begin
  FThreadPool.Free();
  inherited;
end;

function TMultiObjectRequestThread.CreateRequestThread(objType, id_obj: int): TRequestSpecDevices;
begin
  case objType of
    OBJ_TYPE_COM: Result := TRequestCOMDevices.Create(id_obj);
    OBJ_TYPE_TCP: Result := TRequestTCPDevices.Create(id_obj);
    OBJ_TYPE_K104: Result := TRequestK104Devices.Create(id_obj);
    OBJ_TYPE_REMOTE_MAIN: Result := TMainSrvDataPicker.Create(id_obj);
    else Result := nil;
  end;

  if Result <> nil then
    FThreadPool.ThreadList.Add(Result);
end;

procedure TMultiObjectRequestThread.UpdateOneObjectThread(q: TGMSqlQuery; obj: pointer);
var thr: TGMThread;
    id_obj: int;
    objType: int;
begin
  id_obj := q.FieldByName('ID_Obj').AsInteger;
  objType := q.FieldByName('ObjType').AsInteger;

  thr := FindThread(objType, id_obj);
  if thr = nil then
    thr := CreateRequestThread(objType, id_obj);

  if thr <> nil then
    thr.Tag := THREAD_TAG_UP_TO_DATE;
end;

procedure TMultiObjectRequestThread.AddMissingThreads();
begin
  ReadFromQuery('select * from Objects', UpdateOneObjectThread);
end;

procedure TMultiObjectRequestThread.DropUpdateFlag();
var thr: TGMThread;
begin
  for thr in FThreadPool.ThreadList do
    thr.Tag := THREAD_TAG_ODD;
end;

procedure TMultiObjectRequestThread.DropOddThreads();
var
  i: int;
begin
  for i := FThreadPool.ThreadList.Count - 1 downto 0 do
    if FThreadPool.ThreadList[i].Tag = THREAD_TAG_ODD then
      FThreadPool.DropThread(i);
end;

procedure TMultiObjectRequestThread.DropStoppedThreads();
var
  i: int;
begin
  for i := FThreadPool.ThreadList.Count - 1 downto 0 do
    if FThreadPool.ThreadList[i].Done then
      FThreadPool.DropThread(i);
end;

procedure TMultiObjectRequestThread.UpdateThreadList();
begin
  DropUpdateFlag();
  DropStoppedThreads();
  AddMissingThreads();
  DropOddThreads();
end;

procedure TMultiObjectRequestThread.SafeExecute;
begin
  while not Terminated do
  begin
    UpdateThreadList();
    SleepThread(5000);
  end;

  FThreadPool.Terminate();
  FThreadPool.WaitFor();
end;

function TMultiObjectRequestThread.FindThread(objType, ID_Obj: int): TRequestSpecDevices;
var
  thr: TRequestSpecDevices;
begin
  Result := nil;
  for thr in FThreadPool.ThreadList do
    if (thr.ObjType = objType) and (thr.ID_Obj = ID_Obj) then
    begin
      Result := thr;
      Exit;
    end;
end;

function TMultiObjectRequestThread.CheckOutputChannelIds(id_prm: int; channelIds: TChannelIds): bool;
begin
  // id_prm передается на случай, когда канал не найден. Тогда channelIds.ID_Prm не информативен
  Result := false;
  if channelIds.ID_Prm <= 0 then
    ProgramLog.AddError(Format('SetChannelValue ID_Prm = %d not found', [id_prm]))
  else
  if not (channelIds.RealSrc() in OutputParamTypes) then
    ProgramLog.AddError(Format('SetChannelValue ID_Prm = %d type mismatch', [id_prm]))
  else
    Result := true;
end;

function TMultiObjectRequestThread.SendRemoteCommand(ID_Prm: int; value: double; const userName: string = ''): bool;
begin
  try
    SQLReq_SetChannel(ID_Prm, value, userName);
    ProgramLog.AddMessage(Format('SetOutputParam ID_Prm = %d to remote queue', [id_prm]));
    Result := true;
  except
    on e: Exception do
    begin
      Result := false;
      ProgramLog.AddException('SetOutputParam ID_Prm = %d sql error');
    end;
  end;
end;

function TMultiObjectRequestThread.SetChannelValue_Geomer(channelIds: TChannelIds; fVal: double; TimeHold: UINT; const userName: string = ''): bool;
var
  Sckt: TGeomerSocket;
begin
  Sckt := lstSockets.SocketByIdObj(channelIds.ID_Obj);
  if Sckt = nil then
  begin
    ProgramLog.AddError(Format('SetOutputParam ID_Prm = %d Line not found', [channelIds.ID_Prm]));
    Exit(false);
  end;

  SQLReq_SetChannel(channelIds.ID_Prm, fVal, userName, TimeHold);

{  Sckt.Geomer_SetOutputChannel(channelIds.N_Src, fVal, TimeHold);
  Sckt.RequestInfo0(2);
  ProgramLog.AddMessage(Format('SetOutputParam ID_Prm = %d done', [channelIds.ID_Prm]));
}
  Result := true;
end;

function TMultiObjectRequestThread.SetChannelValue(id_prm: int; Val: double; timeHold: int = 0; age: int = -1): bool;
var
  channelIds: TChannelIds;
  sql: string;
  res: ArrayOfString;
  N_Src_Age: int;
  id_prm_age: int;
begin
  Result := false;

  channelIds := LoadChannelIds(id_prm);
  if not CheckOutputChannelIds(id_prm, channelIds) then Exit;

  sql := Format('select BaudRate, RemoteSrv, RemotePort, AgeAddr from DeviceSystem where ID_Prm = %d', [id_prm]);
  res := QueryResultArray_FirstRow(sql);
  if Length(res) < 4 then Exit;

  if channelIds.RemoteName <> '' then
    Exit(SendRemoteCommand(channelIds.ID_Prm, Val));

  if (channelIds.ObjType = OBJ_TYPE_GM) and (channelIds.ID_DevType in GeomerFamily) then // Геомер - особый случай
    Exit(SetChannelValue_Geomer(channelIds, Val, TimeHold));

  Result := true;
  SQLReq_SetChannel(channelIds.ID_Prm, Val, '', TimeHold);

  N_Src_Age := StrToIntDef(res[3], -1);
  if (age < 0) or (N_Src_Age <= 0) then
    Exit;

  id_prm_age :=
    StrToIntDef(
      QueryResultFmt('select ID_Prm from Params where ID_Device = %d and PrmRealSrc(ID_Prm) = %d and %d in (N_Src, CurrentsAddr) limit 1',
        [channelIds.ID_Device, SRC_AO, N_Src_Age]),
      0);

  if id_prm_age > 0 then
    SQLReq_SetChannel(id_prm_age, age);
end;

end.
