unit Threads.ObjectOnline;

interface

uses Windows, sysUtils, GMConst, GMGlobals, GMGenerics, Threads.Base, GMSqlQuery;

type
  TObjectOnlineState = class
    ID_Obj, ObjType, N_Car: int;
    utOnline, utLastSaved: DWORD;

    function UpdateQuery(): string;
  end;

  TObjectOnlineThread = class(TGMThread)
  private
    lock: TMultiReadExclusiveWriteSynchronizer;
    lstObjects: TGMCollection<TObjectOnlineState>;
  protected
    procedure SafeExecute; override;
    function OnlineTime: LongWord; virtual;
    procedure ProcessObjects;
    function UpdateQuery(): string;
  public
    constructor Create();
    destructor Destroy; override;
    procedure ObjectOnline(ID_Obj: int; ObjType: int = -1; N_Car: int = 0);
  end;

implementation

{ TObjectOnlineThread }

uses ProgramLogFile;

constructor TObjectOnlineThread.Create;
begin
  inherited Create(false);

  lstObjects := TGMCollection<TObjectOnlineState>.Create();
  lock := TMultiReadExclusiveWriteSynchronizer.Create();
end;

destructor TObjectOnlineThread.Destroy;
begin
  lstObjects.Free();
  lock.Free();

  inherited;
end;

function TObjectOnlineThread.OnlineTime(): LongWord;
begin
  Result := NowGM();
end;

procedure TObjectOnlineThread.ObjectOnline(ID_Obj, ObjType, N_Car: int);
var i: int;
    obj: TObjectOnlineState;
begin
  obj := nil;
  lock.BeginWrite();
  try
    for i := 0 to lstObjects.Count - 1 do
    begin
      if    ( (ID_Obj > 0) and (lstObjects[i].ID_Obj = ID_Obj) )
         or ( (ID_Obj <= 0) and (lstObjects[i].ObjType = ObjType) and (lstObjects[i].N_Car = N_Car) ) then
      begin
        obj := lstObjects[i];
      end;
    end;

    if obj = nil then
    begin
      obj := lstObjects.Add();
      obj.ID_Obj := 0;
      obj.ObjType := ObjType;
      obj.N_Car := N_Car;
      obj.utLastSaved := 0;
    end;

    if ID_Obj > 0 then
      obj.ID_Obj := ID_Obj;

    obj.utOnline := OnlineTime();
  finally
    lock.EndWrite();
  end;
end;

function TObjectOnlineThread.UpdateQuery(): string;
var i: int;
begin
  lock.BeginRead();
  try
    Result := '';
    for i := 0 to lstObjects.Count - 1 do
      Result := Result + lstObjects[i].UpdateQuery();
  finally
    lock.EndRead();
  end;
end;

procedure TObjectOnlineThread.ProcessObjects();
var sql: string;
begin
  try
    sql := UpdateQuery();
    ExecPLSQL(sql);
  except
    on e: Exception do
      ProgramLog.AddException('TObjectOnlineThread - ' + e.Message);
  end;
end;

procedure TObjectOnlineThread.SafeExecute;
begin
  while not Terminated do
  begin
    ProcessObjects();
    SleepThread(1000);
  end;
end;

{ TObjectOnlineState }

function TObjectOnlineState.UpdateQuery: string;
var s: string;
begin
  if utLastSaved = utOnline then Exit;

  if ID_Obj > 0 then
    s := IntToStr(ID_Obj)
  else
    s := Format('(select ID_Obj from Objects where coalesce(ObjType, 0) = %d and N_Car = %d)', [ID_Obj, N_Car]);

  Result := 'perform ObjectOnline(' + s + ', ' + IntToStr(utOnline) + ');'#13#10;
  utLastSaved := utOnline;
end;

end.
