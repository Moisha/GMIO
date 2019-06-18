do $$
declare _version int;
begin 
  -- Запишем новую версию скрипта
  -- pg_gm_db_template_str_replace #HEAD_REVISION# $HEAD_REVISION$
  perform SetConfigPrm('ScriptVersion', '#HEAD_REVISION#');
end $$;
