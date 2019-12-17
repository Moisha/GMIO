unit Threads.Base;

interface

uses Windows, Classes, SysUtils, GMGlobals, Math, Generics.Collections;

type
  TGMThread = class(TThread)
  private
    FTag: NativeInt;
  protected
    procedure SleepThread(TimeoutMs: int);
    procedure SafeExecute(); virtual; abstract;
    procedure Execute(); override;
    function GetTerminated: bool;
    function GetDescription: string; virtual;
  public
    property Description: string read GetDescription;
    property Tag: NativeInt read FTag write FTag;
    function WaitForTimeout(TimeOut: int; KillUnterminated: bool = true): int; virtual;
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

uses ProgramLogFile, GMConst{$ifdef SQL_APP}, GMSqlQuery{$endif};

{ TGMThread }

destructor TGMThread.Destroy;
begin
  inherited;
{$ifdef SQL_APP}
  DropThreadConnection(ThreadID);
{$endif}
end;

procedure TGMThread.Execute;
begin
  try
    if ProgramLog <> nil then
      ProgramLog.AddMessage(ClassName() + '.SafeExecute');
    SafeExecute();
  except
    on e: Exception do
    begin
      GMPostMessage(WM_THREAD_EXCEPTION, IfThen(FreeOnTerminate, 0, WParam(self)), WParam(ClassType));
      ProgramLog.AddException('TGMThread(' + ClassName() + ').Execute - ' + e.Message);
    end;
  end;
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

  WaitForSingleObject(Handle, TimeoutMs);
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

  // ограничение на ожидание - 64 неостановленных потока, используем первые 64 в списке, веруя,
  // что потоков или меньше, а остальные будут останавливаться быстрее первых или не остановятся совсем
  var res: DWORD := WaitForMultipleObjects(n, @H, true, IfThen(timeoutMs > 0, timeoutMs, FTimeout));
  if res = WAIT_FAILED then
    ProgramLog().AddError(ClassName() + '.WaitFor.WaitForMultipleObjects: ' + GetGoodLastError());

  for var thr in self do
    thr.WaitForTimeout(1);
end;

end.
