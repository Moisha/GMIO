unit ArchIntervalBuilder;

interface

uses Windows, GMSqlQuery, Classes, Generics.Collections, DB;

type
  ArchReqInterval = record
    UTime1, UTime2: LongWord;
  end;

  TArchIntervalBuilder = class
  private
    FQuery: TGMSqlQuery;
    FOwnQuery: bool;
  protected
    // такой изврат нам нужен, чтобы отвязаться от TGMSqlQuery при тестировании алгоритма
    function GetNextHole(): LongWord; virtual;
    function IsAllProcessed: bool; virtual;
    procedure OpenQuery; virtual;
  public
    constructor Create(q: TGMSqlQuery); overload;
    constructor Create(const sql: string); overload;
    destructor Destroy; override;
    function BuildIntervalList(step, maxreq: Cardinal): TList<ArchReqInterval>;
  end;

implementation

{ TArchIntervalBuilder }

// На выходе вернутся начальные даты интервалов!
function TArchIntervalBuilder.BuildIntervalList(step, maxreq: Cardinal): TList<ArchReqInterval>;
var a: ArchReqInterval;
    ut: LongWord;
begin
  Result := TList<ArchReqInterval>.Create();
  OpenQuery();

  if IsAllProcessed() then Exit;

  a.UTime1 := 0;
  a.UTime2 := 0;

  while not IsAllProcessed() do
  begin
    ut := GetNextHole();

    if a.UTime1 = 0 then
    begin
      a.UTime1 := ut;
      a.UTime2 := ut;
    end
    else
    if ut - a.UTime2 > step then
    begin
      // разрыв. т.е. между предыдущей и текущей дырой есть интервал с данными
      // в таком случае мы добавляем предыдущий интервал и стартуем новый.
      Result.Add(a);

      a.UTime1 := ut;
      a.UTime2 := ut;
    end
    else
    if ut - a.UTime1 >= step * maxreq then
    begin
      // набралось нужное количество запросов
      // добавим и начнем жизнь с чистого листа
      Result.Add(a);

      a.UTime1 := ut;
      a.UTime2 := ut;
    end
    else
    begin
      // дырка продолжается
      a.UTime2 := ut;
    end;
  end;

  if a.UTime1 > 0 then
    Result.Add(a);
end;

constructor TArchIntervalBuilder.Create(q: TGMSqlQuery);
begin
  inherited Create();
  FQuery := q;
  FOwnQuery := false;
end;

constructor TArchIntervalBuilder.Create(const sql: string);
begin
  inherited Create();
  FQuery := TGMSqlQuery.Create();
  FQuery.SQL.Text := sql;
  FOwnQuery := true;
end;

destructor TArchIntervalBuilder.Destroy;
begin
  if FOwnQuery then
    FQuery.Free();

  inherited;
end;

function TArchIntervalBuilder.GetNextHole: LongWord;
begin
  Result := FQuery.Fields[0].AsInteger;
  FQuery.Next();
end;

function TArchIntervalBuilder.IsAllProcessed: bool;
begin
  Result := FQuery.Eof;
end;

procedure TArchIntervalBuilder.OpenQuery;
begin
  if FQuery.State <> dsBrowse then
    FQuery.Open();
end;

end.
