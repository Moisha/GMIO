do $$ begin perform DropFunction('GetCounterCeiling'); end $$;
create or replace function GetCounterCeiling(_ID_Prm int)
returns float
as $$
  declare _res float; _devtype int;
begin
  select CounterCeiling, d.ID_DevType into _res, _devtype
    from Params p join Devices d on p.ID_Device = d.ID_Device
      where ID_Prm = _ID_Prm;

  if coalesce(_res, 0) <= 0 then
    select 
      case _devtype
        when 23 then 1e6 -- ISCO
        when 2 then 65536 -- ICP7041D
        else 65536 end into _res;
  end if;
  
  return _res;
end;
$$ language plpgsql;

grant execute on function GetCounterCeiling(_ID_Prm int) to "GM_Admin";

--select * from Params p join Devices d on p.ID_Device = d.ID_Device
-- select * from DevTypes
-- select GetCounterCeiling(24)