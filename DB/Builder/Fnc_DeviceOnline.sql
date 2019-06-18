do $$ begin perform DropFunction('DeviceOnline'); end $$;
create or replace function DeviceOnline(
  _ID_Device int,
  _utime bigint = 0
  ) 
returns void 
AS $$
begin 
  if coalesce(_ID_Device, 0) > 0 then
    perform SetDevState(_ID_Device, 'last_online', null, _utime);
    perform ObjectOnline((select ID_Obj from Devices where ID_Device = _ID_Device), _utime);
  end if;
end;
$$ language plpgsql;

grant execute on function DeviceOnline(_ID_Device int, _utime bigint) to "GM_Admin";