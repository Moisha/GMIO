unit UDPBindingStorage;

interface

uses Windows, Classes, Threads.ReqSpecDevTemplate, IdSocketHandle, IdUDPServer, SysUtils, GMGlobals;

type
  TUDPBindingAndThread = class(TCollectionItem)
  public
    thread: TRequestSpecDevices;
    binding: TIdSocketHandle;
  end;

  TUDPBindingsAndThreads = class(TCollection)
  private
    synch: TMultiReadExclusiveWriteSynchronizer;
    FUdpSrv: TIdUDPServer;
    function CheckBinding(binding: TIdSocketHandle): bool;
  public
    function ByBinding(ABinding: TIdSocketHandle): TRequestSpecDevices;
    function ByThread(AThread: TThread): TIdSocketHandle;
    procedure RemoveThread(AThread: TThread);

    procedure ClearByIdObj(id_obj: int; AThreadClass: TRequestSpecDevicesClass);
    procedure PrepareToShutDown();

    constructor Create(AUdpSrv: TIdUDPServer);
    destructor Destroy; override;
  end;

implementation

{ TUDPBindingsAndThreads }

function TUDPBindingsAndThreads.ByBinding(ABinding: TIdSocketHandle): TRequestSpecDevices;
var i: int;
    r: TUDPBindingAndThread;
begin
  Result := nil;
  synch.BeginRead();
  try
    for i := Count - 1 downto 0 do
    begin
      r := TUDPBindingAndThread(Items[i]);
      if r.binding = ABinding then
      begin
        Result := r.thread;
        Exit;
      end;
    end;
  finally
    synch.EndRead();
  end;
end;

function TUDPBindingsAndThreads.ByThread(AThread: TThread): TIdSocketHandle;
var i, n: int;
    r: TUDPBindingAndThread;
begin
  n := -1;
  Result := nil;
  synch.BeginRead();
  try
    for i := Count - 1 downto 0 do
    begin
      r := TUDPBindingAndThread(Items[i]);
      if r.thread = AThread then
      begin
        n := i;
        break;
      end;
    end;
  finally
    synch.EndRead();
  end;

  if n < 0 then Exit;

  // проверим отпавшие соединения
  r := TUDPBindingAndThread(Items[n]);
  if CheckBinding(r.binding) then
  begin
    Result := r.binding;
  end
  else
  begin
    synch.BeginWrite();
    try
      Delete(n);
    finally
      synch.EndWrite();
    end;

    Result := ByThread(AThread);
  end;
end;

procedure TUDPBindingsAndThreads.ClearByIdObj(id_obj: int; AThreadClass: TRequestSpecDevicesClass);
var i: int;
    r: TUDPBindingAndThread;
begin
  synch.BeginWrite();
  try
    for i := Count - 1 downto 0 do
    begin
      r := TUDPBindingAndThread(Items[i]);
      if (r.thread is AThreadClass) and (r.thread.ID_Obj =id_obj) then
      begin
        Delete(i);
      end
      else
      begin
        if not CheckBinding(r.binding) then
        begin
          Delete(i);
        end;
      end;
    end;
  finally
    synch.EndWrite();
  end;
end;

constructor TUDPBindingsAndThreads.Create(AUdpSrv: TIdUDPServer);
begin
  inherited Create(TUDPBindingAndThread);
  synch := TMultiReadExclusiveWriteSynchronizer.Create();
  FUdpSrv := AUdpSrv;
end;

destructor TUDPBindingsAndThreads.Destroy;
begin
  synch.Free();
  inherited;
end;

procedure TUDPBindingsAndThreads.PrepareToShutDown;
var i: int;
    r: TUDPBindingAndThread;
begin
  synch.BeginWrite();
  try
    for i := Count - 1 downto 0 do
    begin
      r := TUDPBindingAndThread(Items[i]);
      r.binding := nil;
    end;
  finally
    synch.EndWrite();
  end;
end;

procedure TUDPBindingsAndThreads.RemoveThread(AThread: TThread);
var i: int;
    r: TUDPBindingAndThread;
begin
  synch.BeginWrite();
  try
    for i := Count - 1 downto 0 do
    begin
      r := TUDPBindingAndThread(Items[i]);
      if r.thread = AThread then
      begin
        Delete(i);
        break;
      end;
    end;
  finally
    synch.EndWrite();
  end;
end;

function TUDPBindingsAndThreads.CheckBinding(binding: TIdSocketHandle): bool;
var i: int;
begin
  Result := false;
  synch.BeginRead();
  try
    for i := 0 to FUdpSrv.Bindings.Count - 1 do
      if FUdpSrv.Bindings[i] = binding then
      begin
        Result := true;
        break;
      end;
  finally
    synch.EndRead();
  end;
end;

end.
