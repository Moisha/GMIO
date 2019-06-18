do $$
begin
  if exists (select * from pg_views where ViewName = lower('DeviceSystem')) then
    drop view DeviceSystem;
  end if;
end $$;