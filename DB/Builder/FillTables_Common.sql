do $$ begin
  if not exists(select * from SysConfig where PrmName = 'CurrentsMaxHoleMinutes') then
    insert into SysConfig(PrmName, PrmValue) select 'CurrentsMaxHoleMinutes', 5;
  end if;
end; $$;