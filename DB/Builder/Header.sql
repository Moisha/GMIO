--------------------------------------------------------
--------------------------------------------------------
-- Скрипт для создания и обновления структур базы данных в БД PostreSQL
--------------------------------------------------------

-- Полностью остановить скрипт невозможно, поэтому в служебной базе или при наличии более новой версии подвесим выполнение
-- pg_gm_db_template_str_replace #HEAD_REVISION# $HEAD_REVISION$

do $$
declare _version int;
begin 
  if current_database() = 'postgres' then
    loop
      raise notice 'Скрипт запущен в системной БД';
      perform pg_sleep(2);
    end loop;
  end if;

  begin
  select cast(PrmValue as int) into _version from SysConfig where PrmName = 'ScriptVersion';
  exception
    when OTHERS then 
      _version := 0;
  end;

  if _version > #HEAD_REVISION# then
    loop
      raise notice 'БД имеет более новую версию: %', _version;
      perform pg_sleep(2);
    end loop;
  end if; 
end $$;
