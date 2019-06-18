do $$ begin perform DropFunction('SetDevState'); end $$;
create or replace function SetDevState(
  _ID_Device int,
  _StateName text,
  _StateValue text,
  _utime bigint = 0
  ) 
returns void 
AS $$
begin 
  if _utime = 0 then
    _utime := NowGM();
  end if;
  
  if coalesce(_ID_Device, 0) > 0 then
    begin
      insert into DevStates(ID_Device, LastOnline, StateName, StateValue) select _ID_Device, _utime, _StateName, _StateValue;
    exception
      when others then
        update DevStates set LastOnline = _utime, StateValue = _StateValue where ID_Device = _ID_Device and StateName = _StateName;
    end; 
  end if;
end;
$$ language plpgsql;

grant execute on function SetDevState(int, text, text, bigint) to "GM_Admin";