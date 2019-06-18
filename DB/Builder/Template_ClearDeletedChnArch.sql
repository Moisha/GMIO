do $$ begin perform DropFunction('ClearDeletedChn#Table#'); end $$;
create or replace function ClearDeletedChn#Table#()
returns void
as $$
declare _minprm int; _maxprm int; _id int; _cnt int;
begin
  select Min(ID_Prm), Max(ID_Prm) into _minprm, _maxprm from #Table#;
  if _minprm is null then 
    return;
  end if; 
  
  for _id in _minprm .. _maxprm 
  loop
    if not exists(select * from Params where ID_Prm = _id) then
      delete from #Table# where ID_Prm = _id;

      GET DIAGNOSTICS _cnt = ROW_COUNT;
      if _cnt > 0 then
        raise notice 'deleted %, %', _id, _cnt;
      end if;
    end if;
  end loop;
end;
$$ language plpgsql;

grant execute on function ClearDeletedChn#Table#() to "GM_Admin";
