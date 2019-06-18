DO $$
begin

  if not exists(select * from pg_tables where tablename = lower('objects')) then 
    create table Objects (
		ID_Obj serial -- ID
		, Name	varchar (500) not null	-- имя
		, N_Car int not null	-- номер из геомера (номер машины), номер порта
		, ObjType int null -- тип объекта:
/* OBJ_TYPE_GM = 0;
   OBJ_TYPE_COM = 1;
   OBJ_TYPE_REMOTE_SRV = 2;
   OBJ_TYPE_K104 = 3;
   OBJ_TYPE_K105 = 4;
   OBJ_TYPE_ANCOM = 5;
   OBJ_TYPE_TCP = 6;
   OBJ_TYPE_REMOTE_SRV_XML = 7;
   OBJ_TYPE_REMOTE_MAIN = 8;   */	
		, RemoteSrv varchar(100) null -- удаленный сервер для опроса
		, RemotePort int null -- удаленный сервер для опроса
		, RemoteName text null -- название подчиненного сервера
		, RemoteID int null -- идентификатор объекта на подчиненном сервере
		, RemoteType int null -- тип объекта на подчиненном сервере (Когда ObjType OBJ_TYPE_REMOTE_SRV_XML)
		, UserData text null -- любая строка на усмотрение пользователя
		, Converter int null -- преобразователь интерфесов. 1 - ModbusRTU<->ModbusTCP
		, PRIMARY KEY(ID_Obj)
    );
  end if;
  
  pg_gm_db_template_add_column Objects RemoteName text null
  pg_gm_db_template_add_column Objects RemoteID int null
  pg_gm_db_template_add_column Objects RemoteType int null
  pg_gm_db_template_add_column Objects UserData text null
  pg_gm_db_template_add_column Objects Converter int null
  
  grant all on Objects to "GM_Admin";
  grant all on Objects_ID_Obj_seq to "GM_Admin";
  
  if not exists(select * from pg_tables where tablename = lower('ObjectStates')) then 
    create table ObjectStates (
		ID_Obj int not null references Objects on delete cascade -- ID объекта
		, LastOnline bigint null
		, LastReqNumber int null
	, PRIMARY KEY(ID_Obj)
    );
  end if;  
  
  pg_gm_db_template_add_column ObjectStates LastReqNumber int null
  
  grant all on ObjectStates to "GM_Admin";
  
end $$;