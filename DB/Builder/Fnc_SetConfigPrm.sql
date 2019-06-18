do $$ begin perform DropFunction('SetConfigPrm'); end $$;
create or replace function SetConfigPrm(_key text, _val text)
returns void
as $$
declare _cnt int;
begin
  update SysConfig set PrmValue = _val
    where PrmName = _key;

  get diagnostics _cnt = ROW_COUNT;
  if coalesce(_cnt, 0) <= 0 then
    insert into SysConfig(PrmName, PrmValue) values(_key, _val);
  end if;
end;
$$ language plpgsql;

grant execute on function SetConfigPrm(_key text, _val text) to "GM_Admin";

-- select * from sysconfig
-- delete from sysconfig
-- do $$ begin perform SetConfigPrm('StoreVals_Days', '700'); end $$;  