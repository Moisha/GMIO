unit Test.Threads.RegularControl;

interface

uses SysUtils, Classes, GMGlobals, Windows, TestFrameWork, Threads.RegularControl, Threads.Pool,
     Threads.ReqSpecDevTemplate, RequestThreadsContainer, Threads.TCP, GMConst, GM485, GMSqlQuery, Threads.GMCOM;

type
  TMultiObjectRequestThreadForTest = class(TMultiObjectRequestThread)
  protected
    procedure SafeExecute(); override;
    procedure UpdateThreadList; override;
    constructor Create();
  end;

  TRequestTCPDevicesForTest = class(TRequestTCPDevices)
  protected
    procedure SafeExecute; override;
  end;

  TRequestCOMDevicesForTest = class(TRequestCOMDevices)
  protected
    procedure SafeExecute; override;
  end;

  TRequestThreadsContainerForTest = class(TRequestThreadsContainer)
  protected
    procedure CreateThreads; override;
  end;

  TRegularControlThreadForTest = class(TRegularControl)
  public
    procedure Execute(); override;
  end;

  TRegularControlThreadTest = class(TTestCase)
  private
    thr: TRegularControlThreadForTest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CheckTestData();
  end;

implementation

{ TRegularControlThreadForTest }

procedure TRegularControlThreadForTest.Execute;
begin
  // Ничего не делаем, просто выходим
end;

{ TRegularControlThreadTest }

type TRequestTCPDevicesHack = class(TRequestTCPDevices);

procedure TRegularControlThreadTest.CheckTestData;
var thrReq: TRequestSpecDevices;
    req: TSpecDevReqListItem;
    origin: ArrayOfByte;
    i: int;
begin
  thr.Process();
  Check(ThreadsContainer.ThreadList[0] is TMultiObjectRequestThreadForTest);
  thrReq := TRequestSpecDevices(TMultiObjectRequestThreadForTest(ThreadsContainer.ThreadList[0]).ThreadPool.ThreadList[0]);
  Check(thrReq is TRequestTCPDevices);
  TRequestTCPDevicesHack(thrReq).LoadCommands();
  Check(TRequestTCPDevicesForTest(thrReq).RequestList.Count = 2);

  // само значение
  req := TRequestTCPDevicesForTest(thrReq).RequestList[0];
  origin := TextNumbersStringToArray('00 00 00 00 00 06 01 06 01 02 27 10');
  for i := 0 to High(origin) do
    CheckEquals(origin[i], req.ReqDetails.buf[i], 'Cmd: ' + IntToStr(i));

  // возраст значения. если падает тест - прогнать testdb.sql
  req := TRequestTCPDevicesForTest(thrReq).RequestList[1];
  origin := TextNumbersStringToArray('00 00 00 00 00 06 01 06 02 BB 00 00');
  for i := 0 to High(origin) do
    CheckEquals(origin[i], req.ReqDetails.buf[i], 'Age: ' + IntToStr(i));
end;

procedure TRegularControlThreadTest.SetUp;
begin
  inherited;
  thr := TRegularControlThreadForTest.Create();
  ThreadsContainer := TRequestThreadsContainerForTest.Create();
  ThreadsContainer.Terminate();
  ExecSQL('update CurrVals set UTime = ' + IntToStr(NowGM()) + ' where ID_Prm = 1');
end;

procedure TRegularControlThreadTest.TearDown;
begin
  inherited;
  thr.Free();
  FreeAndNil(ThreadsContainer);
end;

{ TRequestThreadsContainerForTest }

procedure TRequestThreadsContainerForTest.CreateThreads;
begin
  ThreadList.Add(TMultiObjectRequestThreadForTest.Create());
end;

{ TRequestTCPDevicesForTest }

procedure TRequestTCPDevicesForTest.SafeExecute;
begin

end;

{ TMultiObjectRequestThreadForTest }

constructor TMultiObjectRequestThreadForTest.Create;
begin
  inherited;
  UpdateThreadList();
end;

procedure TMultiObjectRequestThreadForTest.SafeExecute;
begin

end;

procedure TMultiObjectRequestThreadForTest.UpdateThreadList;
begin
  ThreadPool.ThreadList.Add(TRequestTCPDevicesForTest.Create(4)); // ID_Obj = 4 для теста управления по TCP
  ThreadPool.ThreadList.Add(TRequestCOMDevicesForTest.Create(6)); // ID_Obj = 6 для теста управления по COM
end;

{ TRequestCOMDevicesForTest }

procedure TRequestCOMDevicesForTest.SafeExecute;
begin

end;

initialization
  RegisterTest('GMIOPSrv/Threads', TRegularControlThreadTest.Suite);
end.
