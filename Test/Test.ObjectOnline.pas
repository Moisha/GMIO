unit Test.ObjectOnline;

interface

uses Windows, Classes, TestFrameWork, GMGlobals, Threads.ObjectOnline, GMSqlQuery, GMBlockValues;

type
  TObjectOnlineThreadForTest = class(TObjectOnlineThread)
  protected
    utOnlineTime: LongWord;
    procedure SafeExecute; override;
    function OnlineTime: LongWord; override;
  end;

  TObjectOnlineThreadTest = class(TTestCase)
  private
    thread: TObjectOnlineThreadForTest;
    procedure InsertData();
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ProcessObjects;
    procedure DB();
  end;

implementation

uses DateUtils, SysUtils, GMConst;

function TObjectOnlineThreadForTest.OnlineTime: LongWord;
begin
  Result := utOnlineTime;
end;

procedure TObjectOnlineThreadForTest.SafeExecute;
begin
end;

procedure TObjectOnlineThreadTest.DB;
var cnt: string;
    res: ArrayOfString;
begin
  thread.ProcessObjects();
  cnt := QueryResult('select count(*) from ObjectStates');
  Check(cnt = '3');

  res := QueryResultArray_FirstColumn('select ID_Obj from ObjectStates order by 1');
  Check(Length(res) = 3);
  Check((res[0] = '1') and (res[1] = '2') and (res[2] = '4'));

  res := QueryResultArray_FirstRow('select min(LastOnline), max(LastOnline) from ObjectStates');
  Check(Length(res) = 3);
  Check((StrToInt(res[0]) = int64(thread.utOnlineTime)) and (res[0] = res[1]));
end;

procedure TObjectOnlineThreadTest.InsertData;
var i: int;
begin
  for i := 0 to 10 do
  begin
    thread.ObjectOnline(1, -1, 0);
    thread.ObjectOnline(0, OBJ_TYPE_GM, 200);
    thread.ObjectOnline(0, OBJ_TYPE_GM, 100);
    thread.ObjectOnline(4, -1, 0);
    thread.ObjectOnline(0, OBJ_TYPE_K105, 1);
  end;
end;

procedure TObjectOnlineThreadTest.SetUp;
begin
  inherited;
  thread := TObjectOnlineThreadForTest.Create();
  thread.utOnlineTime := NowGM();
  InsertData();
  ExecSQL('delete from ObjectStates; delete from ParamStates;');
end;

procedure TObjectOnlineThreadTest.TearDown;
begin
  inherited;
  thread.Free();
end;

procedure TObjectOnlineThreadTest.ProcessObjects;
var sl: TstringList;
begin
  sl := TStringList.Create();
  try
    sl.Text := thread.UpdateQuery();
    Check(sl.Count = 5);

    // второй раз не засылаем
    sl.Text := thread.UpdateQuery();
    Check(sl.Count = 0);

    // типа пришли обновления
    thread.utOnlineTime := thread.utOnlineTime + 1;
    InsertData();
    sl.Text := thread.UpdateQuery();
    Check(sl.Count = 5);
  finally
    sl.Free();
  end;
end;

initialization
  RegisterTest('GMIOPSrv/Threads', TObjectOnlineThreadTest.Suite);
end.


