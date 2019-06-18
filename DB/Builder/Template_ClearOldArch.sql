do $$ begin perform DropFunction('ClearOld#Table#'); end $$;
create or replace function ClearOld#Table#()
returns void
as $$
declare _minprm int; _maxprm int; _id int; _cnt int;
        _days int; _add interval; _utime bigint;
begin
  begin
    select cast(PrmValue as int) into _days from SysConfig where PrmName = 'Store#Table#_Days';  
  exception
    when OTHERS then 
    begin
      raise notice 'no need to clear';
      return;  
    end;
  end;
  
  _add := cast (_days as text) || ' days';
  _utime := DTtoUTC(now() - _add);

  -- raise notice 'clear up to %', UTCtoDT(_utime);

  select Min(ID_Prm), Max(ID_Prm) into _minprm, _maxprm from #Table#;
  if _minprm is null then 
    return;
  end if; 
  
  for _id in _minprm .. _maxprm 
  loop
    delete from #Table# where ID_Prm = _id and UTime < _utime;

    GET DIAGNOSTICS _cnt = ROW_COUNT;
    if _cnt > 0 then
      raise notice 'deleted %, %', _id, _cnt;
    end if;
  end loop;
end;
$$ language plpgsql;

grant execute on function ClearOld#Table#() to "GM_Admin";

-- do $$ begin perform SetConfigPrm('Store#Table#_Days', '700'); end $$;  
-- do $$ begin perform ClearOld#Table#(); end $$;	