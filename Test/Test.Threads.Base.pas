unit Test.Threads.Base;

interface

uses Windows, TestFrameWork, GMGlobals, Classes, Threads.Base;

type
  TGMHangingThread = class(TGMThread)
  protected
    procedure SafeExecute(); override;
  end;

  TGMFinishedThread = class(TGMThread)
  protected
    procedure SafeExecute(); override;
  end;

  TGMGoodThread = class(TGMThread)
  protected
    procedure SafeExecute(); override;
  end;

  TGMThreadTest = class(TTestCase)
  published
    procedure Hanging;
    procedure HangingPool;
    procedure GoodThreadPool;
  end;

implementation

uses
  IdGlobal, System.SysUtils;

{ TGMHangingThread }

procedure TGMHangingThread.SafeExecute;
begin
  while true do
    SleepThread(10);
end;

{ TGMFinishedThread }

procedure TGMFinishedThread.SafeExecute;
begin

end;

{ TGMGoodThread }

procedure TGMGoodThread.SafeExecute;
begin
  while not Terminated do
    Sleep(10);
end;

{ TGMThreadTest }

procedure TGMThreadTest.GoodThreadPool;
var
  pool: TGMThreadPool<TGMThread>;
  i: int;
  timeStart, t: uint64;
begin
  pool := TGMThreadPool<TGMThread>.Create();
  pool.Add(TGMFinishedThread.Create());
  pool.Add(TGMFinishedThread.Create());

  for i := 0 to 100 do
    pool.Add(TGMGoodThread.Create());

  timeStart := GetTickCount64();
  pool.Free();
  t := GetTickCount64() - timeStart;
  Check(t < 2000, IntToStr(t) + ' must be 2000 or less');
end;

procedure TGMThreadTest.Hanging;
var
  thr: TGMHangingThread;
begin
  thr := TGMHangingThread.Create();
  Sleep(100);
  thr.Terminate();
  thr.WaitForTimeout(100, true);
  thr.Free();
  Check(true);
end;

procedure TGMThreadTest.HangingPool;
var
  pool: TGMThreadPool<TGMThread>;
  i: int;
  t, t1, dt: int64;
begin
  pool := TGMThreadPool<TGMThread>.Create();
  pool.Add(TGMFinishedThread.Create());
  pool.Add(TGMFinishedThread.Create());

  for i := 0 to 100 do
    pool.Add(TGMHangingThread.Create());

  pool.TimeOut := 1000;
  t := GetTickCount64();
  pool.Free();
  t1 := GetTickCount64();
  dt := t1 - t;
  Check(dt < 1000, IntTostr(dt) + ' must be 1000 or less');
end;

initialization
  RegisterTest('Threads', TGMThreadTest.Suite);
end.
