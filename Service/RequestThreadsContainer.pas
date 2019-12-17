unit RequestThreadsContainer;

interface

uses Windows, Classes, SysUtils, ProgramLogFile, GMGlobals, GMConst, Math, Threads.Base, Threads.Pool,
     Generics.Collections, GM485;

type
  TRequestThreadsContainer = class(TGMThreadPool<TGMThread>)
  private
    function GetMultiObjectRequestThread: TMultiObjectRequestThread;
  protected
    procedure CreateThreads; virtual;
  public
    constructor Create();
    function SetChannelValue(id_prm: int; Val: double; timeHold: int = 0; age: int = -1): bool;
    property MultiObjectRequestThread: TMultiObjectRequestThread read GetMultiObjectRequestThread;
  end;

  var ThreadsContainer: TRequestThreadsContainer;

implementation

uses Threads.GMCOM, Threads.GMK104, GMSqlQuery, Threads.RegularControl, Threads.MainSrvDataPicker,
     Threads.ReqSpecDevTemplate, Threads.TCP;

{ TRequestThreadsContainer }

procedure TRequestThreadsContainer.CreateThreads();
begin
  Add(TMultiObjectRequestThread.Create());
  Add(TRegularControl.Create());
end;

function TRequestThreadsContainer.GetMultiObjectRequestThread: TMultiObjectRequestThread;
begin
  Result := TMultiObjectRequestThread(Items[0]);
end;

constructor TRequestThreadsContainer.Create();
begin
  inherited Create();
  CreateThreads();
end;

function TRequestThreadsContainer.SetChannelValue(id_prm: int; Val: double; timeHold: int = 0; age: int = -1): bool;
begin
  Result := GetMultiObjectRequestThread().SetChannelValue(id_prm, Val, timeHold, age);
end;

end.
