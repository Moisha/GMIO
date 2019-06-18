do $$ begin perform DropFunction('PrmNSrcStr'); end $$;
create or replace function PrmNSrcStr(_ID_Prm int)
returns text
as $$
declare _DevTypeGroup int; _N_Src int; _res varchar(500);
begin
  _res := '';
  	
  select ID_DevTypeGroup, N_Src into _DevTypeGroup, _N_Src 
    from Params p left outer join Devices d on p.ID_Device = d.ID_Device
                  left outer join DevTypes dt on dt.ID_DevType = d.ID_DevType
    where p.ID_Prm = _ID_Prm;

  if _N_Src is not null then  
    if coalesce(_DevTypeGroup, 0) = 1 then
      _res := to_hex(_N_Src);
      while length(_res) < 4 loop
        _res := '0' || _res;
      end loop;
    else
      _res := cast(_N_Src as text);
    end if;
  end if;

  return trim(_res);
end;
$$ language plpgsql;

grant execute on function PrmNSrcStr(_ID_Prm int) to "GM_Admin";
