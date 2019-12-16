unit Threads.Base;

interface

uses Windows, Classes, SysUtils, GMGlobals, Math, Generics.Collections;

type
  TGMThread = class(TThread)
  private
    FDone: bool;
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
    property Done: bool read FDone;
    procedure AfterConstruction; override;
    function WaitForTimeout(TimeOut: int; KillUnterminated: bool = true): int; virtual;
    destructor Destroy; override;
  end;

  TGMThreadPool<T: TGMThread> = class
  private
    FThreadList: TList<T>;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Terminate();
    procedure WaitFor();
    property ThreadList: TList<T> read FThreadList;
    procedure DropThread(Index: int);
  end;

implementation

uses ProgramLogFile, GMConst{$ifdef SQL_APP}, GMSqlQuery{$endif};

{ TGMThread }

procedure TGMThread.AfterConstruction;
begin
  FDone := false;
  inherited;
end;

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
  FDone := true;
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
  if FDone then
  begin
    Result := WAIT_OBJECT_0;
    Exit;
  end;

  if TimeOut > 0 then
    Result := WaitForSingleObject(Handle, TimeOut)
  else
    Result := WaitForSingleObject(Handle, INFINITE);

  if (Result <> WAIT_OBJECT_0) and KillUnterminated then
  begin
    TerminateThread(Handle, 0);
    FDone := true;
  end;
end;

{ TGMThreadPool }

constructor TGMThreadPool<T>.Create;
begin
  inherited;
  FThreadList := TList<T>.Create();
end;

destructor TGMThreadPool<T>.Destroy;
var thr: TGMThread;
begin
  for thr in FThreadList do
    thr.Free();

  FThreadList.Free();

  inherited;
end;

procedure TGMThreadPool<T>.DropThread(Index: int);
begin
  try
    FThreadList[Index].Terminate();
    FThreadList[Index].WaitForTimeout(20000);
    FThreadList[Index].Free();
  except end;

  FThreadList.Delete(Index);
end;

procedure TGMThreadPool<T>.Terminate;
var thr: TGMThread;
begin
  for thr in FThreadList do
    thr.Terminate();
end;

procedure TGMThreadPool<T>.WaitFor;
var thr: TGMThread;
begin
  for thr in FThreadList do
    thr.WaitForTimeout(30000);
end;

end.
