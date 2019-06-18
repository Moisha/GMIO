do $$ begin perform DropFunction('GetConfigPrm'); end $$;
create or replace function GetConfigPrm(_key text)
returns text
as $$
declare _cnt int;
begin
  return (select PrmValue from SysConfig where PrmName =  _key);
end;
$$ language plpgsql;

grant execute on function GetConfigPrm(_key text) to "GM_Admin";
