unit Threads.Base;

interface

uses Windows, Classes, SysUtils, GMGlobals, Math, Generics.Collections, SyncObjs;

type
  TGMThread = class(TThread)
  private
    FTag: NativeInt;
    FRestartOnError: bool;
    FTermEvent: TEvent;
  protected
    procedure SleepThread(TimeoutMs: int);
    procedure SafeExecute(); virtual; abstract;
    procedure Execute(); override;
    function GetTerminated: bool;
    function GetDescription: string; virtual;
    procedure TerminatedSet; override;
  public
    property Description: string read GetDescription;
    property Tag: NativeInt read FTag write FTag;
    property RestartOnError: bool read FRestartOnError write FRestartOnError;
    function WaitForTimeout(TimeOut: int; KillUnterminated: bool = true): int; virtual;
    constructor Create(createSuspended: bool = false);
    destructor Destroy; override;
  end;

  TGMThreadPool<T: TGMThread> = class(TList<T>)
  private
    FTimeOut: int;
  public
    property TimeOut: int read FTimeOut write FTimeOut;
    constructor Create();
    destructor Destroy(); override;
    procedure Terminate();
    procedure WaitFor(timeoutMs: int = -1; killUnfinished: bool = true);
    procedure DropThread(Index: int);
  end;

implementation

uses ProgramLogFile, GMConst{$ifdef SQL_APP}, GMSqlQuery{$endif}, EsLogging;

{ TGMThread }

constructor TGMThread.Create(createSuspended: bool);
begin
  inherited Create(createSuspended);
  FTermEvent := TEvent.Create(nil, True, False, '');
end;

destructor TGMThread.Destroy;
begin
  inherited;
  //inherited calls TerminatedSet where FTermEvent should not be freed yet
  FTermEvent.Free();
{$ifdef SQL_APP}
  DropThreadConnection(ThreadID);
{$endif}
end;

procedure TGMThread.Execute;
begin
  var done := false;
  var error := false;
  repeat
    try
      if ProgramLog <> nil then
        ProgramLog.AddMessage(ClassName() + '.SafeExecute');

      error := false;
      SafeExecute();
      done := true;
    except
      on e: Exception do
      begin
        error := true;
        GMPostMessage(WM_THREAD_EXCEPTION, IfThen(FreeOnTerminate, 0, WParam(self)), WParam(ClassType), DefaultLogger);
        ProgramLog.AddException('TGMThread(' + ClassName() + ').Execute - ' + e.ToString());
      end;
    end;
  until Terminated or done or (error and not FRestartOnError);
end;

procedure TGMThread.TerminatedSet;
begin
  FTermEvent.SetEvent;
end;

function TGMThread.GetDescription: string;
begin
  Result := ClassName();
end;

function TGMThread.GetTerminated: bool;
begin
  Result := Terminated;
end;

procedure TGMThread.SleepThread(TimeoutMs: int);
begin
  if TimeoutMs <= 0 then
    Exit;

  FTermEvent.WaitFor(TimeoutMs);
end;

function TGMThread.WaitForTimeout(TimeOut: int; KillUnterminated: bool = true): int;
begin
  if Finished then
  begin
    Result := WAIT_OBJECT_0;
    Exit;
  end;

  if TimeOut > 0 then
    Result := WaitForSingleObject(Handle, TimeOut)
  else
    Result := WaitForSingleObject(Handle, INFINITE);

  if (Result <> WAIT_OBJECT_0) and KillUnterminated then
    TerminateThread(Handle, 0);
end;

{ TGMThreadPool }

constructor TGMThreadPool<T>.Create;
begin
  inherited;
  FTimeOut := 30000;
end;

destructor TGMThreadPool<T>.Destroy;
var thr: TGMThread;
begin
  Terminate();
  WaitFor();

  for thr in self do
    thr.Free();

  inherited;
end;

procedure TGMThreadPool<T>.DropThread(Index: int);
begin
  try
    Items[Index].Terminate();
    Items[Index].WaitForTimeout(FTimeOut);
    Items[Index].Free();
  except end;

  Delete(Index);
end;

procedure TGMThreadPool<T>.Terminate;
var thr: TGMThread;
begin
  for thr in self do
    thr.Terminate();
end;

procedure TGMThreadPool<T>.WaitFor(timeoutMs: int = -1; killUnfinished: bool = true);
var
  H: TWOHandleArray;
begin
  var n := 0;
  for var i := 0 to Count - 1 do
    if not Items[i].Finished then
    begin
      H[n] := Items[i].Handle;
      inc(n);
      if n > High(H) then
        break;
    end;

  if n = 0 then
    Exit;

  // ����������� �� �������� - 64 ��������������� ������, ���������� ������ 64 � ������, �����,
  // ��� ������� ��� ������, � ��������� ����� ��������������� ������� ������ ��� �� ����������� ������
  var res: DWORD := WaitForMultipleObjects(n, @H, true, IfThen(timeoutMs > 0, timeoutMs, FTimeout));
  if res = WAIT_FAILED then
    ProgramLog().AddError(ClassName() + '.WaitFor.WaitForMultipleObjects: ' + GetGoodLastError());

  for var thr in self do
    thr.WaitForTimeout(1);
end;

end.
