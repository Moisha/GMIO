DO $$
begin
  if not exists(select * from pg_tables where tablename = lower('Commands')) then 
    create table Commands (
	  ID_Cmd serial -- ID
	, ID_Cmd_On_Remote_Host int  null -- ID_Cmd команды на головном сервере
	, ID_Prm int not null references Params on delete cascade -- ID канала
	, UTime bigint not null -- дата и время выдачи команды
	, Value float not null
	, TimeHold int null -- время удержания для Геомера
	, State int null -- статус команды: null - не выполнена, 1 - выполнена, 2 - ошибки, 3 - отмена
	, UserName text null -- пользователь, отдавший команду
	, PRIMARY KEY(ID_Cmd)
    );
  end if;
  grant all on Commands to "GM_Admin";
  grant all on Commands_ID_Cmd_seq to "GM_Admin";  
  
  pg_gm_db_template_add_column Commands ID_Cmd_On_Remote_Host int null
  pg_gm_db_template_add_column Commands TimeHold int null
end $$;