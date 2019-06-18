do $$ begin perform DropFunction('ObjectSetLastReqNumber'); end $$;
create or replace function ObjectSetLastReqNumber(
  _ID_Obj int,
  _number int
  ) 
returns void 
AS $$
begin 
  if coalesce(_ID_Obj, 0) > 0 then
    begin
      insert into ObjectStates(ID_Obj, LastReqNumber) select _ID_Obj, _number;
    exception
      when others then
        update ObjectStates set LastReqNumber = _number where ID_Obj = _ID_Obj;
    end; 
  end if;
end;
$$ language plpgsql;

grant execute on function ObjectSetLastReqNumber(_ID_Obj int, _number int) to "GM_Admin";