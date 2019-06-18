unit RequestList;

interface

uses Windows, GMGenerics, GMGlobals, GM485, Generics.Collections;

type
  TRequestCollection = class(TGMThreadSafeCollection<TSpecDevReqListItem>)
  public
    procedure ClearProcessed();
    function HasRequestsWithPriority(priority: TRequestPriority): bool;
    function CountRequestsWithPriority(priority: TRequestPriority): int;
    function MaxPriority(): TRequestPriority;
    procedure Add(reqDetails: TRequestDetails); reintroduce;
    procedure EnlistRequestsWithPriority(list: TList<TSpecDevReqListItem>; priority: TRequestPriority; maxCount: int = 0);
    procedure SortByPriority;
  end;

implementation

uses
  System.Generics.Defaults, System.Math;

{ TRequestCollection }

procedure TRequestCollection.ClearProcessed;
var i: int;
begin
  Lock.BeginWrite();
  try
    for i := Count - 1 downto 0 do
      if Items[i].Processed then
        Delete(i);
  finally
    Lock.EndWrite();
  end;
end;

function TRequestCollection.CountRequestsWithPriority(priority: TRequestPriority): int;
var
  ri: TSpecDevReqListItem;
begin
  Lock.BeginRead();
  try
    Result := 0;
    for ri in self do
    begin
      if not ri.Processed and (ri.ReqDetails.Priority = priority) then
        inc(Result);
    end;
  finally
    Lock.EndRead();
  end;
end;

procedure TRequestCollection.EnlistRequestsWithPriority(list: TList<TSpecDevReqListItem>; priority: TRequestPriority; maxCount: int = 0);
var
  ri: TSpecDevReqListItem;
begin
  Lock.BeginRead();
  try
    for ri in self do
    begin
      if (maxCount > 0) and (list.Count >= maxCount) then Exit;
      if not ri.Processed and (ri.ReqDetails.Priority = priority) then
        list.Add(ri);
    end;
  finally
    Lock.EndRead();
  end;
end;

function TRequestCollection.HasRequestsWithPriority(priority: TRequestPriority): bool;
begin
  Result := CountRequestsWithPriority(priority) > 0;
end;

function TRequestCollection.MaxPriority: TRequestPriority;
var
  ri: TSpecDevReqListItem;
begin
  Lock.BeginRead();
  try
    Result := rqprNone;
    for ri in self do
      if not ri.Processed and (ri.ReqDetails.Priority > Result) then
        Result := ri.ReqDetails.Priority;
  finally
    Lock.EndRead();
  end;
end;

procedure TRequestCollection.SortByPriority;
begin
  Sort(TComparer<TSpecDevReqListItem>.Construct(
    function (const item1, item2: TSpecDevReqListItem): integer
    begin
      Result := CompareValue(ord(item1.ReqDetails.Priority), ord(item2.ReqDetails.Priority));
      if Result = 0 then
        Result := CompareValue(item1.ReqDetails.ID_Device, item2.ReqDetails.ID_Device);
    end));
end;

procedure TRequestCollection.Add(reqDetails: TRequestDetails);
begin
  Lock.BeginWrite();
  try
    inherited Add().ReqDetails := ReqDetails;
  finally
    Lock.EndWrite();
  end;
end;

end.
