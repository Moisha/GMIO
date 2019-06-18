do $$ begin perform DropFunction('ObjectOnline'); end $$;
create or replace function ObjectOnline(
  _ID_Obj int,
  _utime bigint = 0
  ) 
returns void 
AS $$
begin 
  if _utime = 0 then
    _utime := NowGM();
  end if;
  
  if coalesce(_ID_Obj, 0) > 0 then
    begin
      insert into ObjectStates(ID_Obj, LastOnline) select _ID_Obj, _utime;
    exception
      when others then
        update ObjectStates set LastOnline = _utime where ID_Obj = _ID_Obj and LastOnline < _utime ;
    end; 
  end if;
end;
$$ language plpgsql;

grant execute on function ObjectOnline(_ID_Obj int, _utime bigint) to "GM_Admin";