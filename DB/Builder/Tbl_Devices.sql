DO $$
begin

  if not exists(select * from pg_tables where tablename = lower('DevTypeGroups')) then 
    create table DevTypeGroups (
       ID_DevTypeGroup serial -- ID
       ,Name varchar(100)
    );
  end if;
  grant all on DevTypeGroups to "GM_Admin";

   -- drop table DevTypes
  if not exists(select * from pg_tables where tablename = lower('DevTypes')) then 
    create table DevTypes (
          ID_DevType serial -- ID
	, Name varchar(100) not null
	, FixedPT int null
	, ID_DevTypeGroup int null
	, AllowZeroNumber int null
	, DefaultChannelsArgument int null
	, PRIMARY KEY(ID_DevType)
    );
  end if;
  grant all on DevTypes to "GM_Admin";

  pg_gm_db_template_add_column DevTypes AllowZeroNumber int null
  pg_gm_db_template_add_column DevTypes DefaultChannelsArgument int null

   -- drop table Devices
  if not exists(select * from pg_tables where tablename = lower('Devices')) then 
    create table Devices (
         ID_Device serial -- ID
       , ID_Obj int not null references Objects on delete cascade -- ID объекта
       , ID_DevType int not null references DevTypes -- ID типа
       , Number int null -- номер на интерфейсе для устройств, работающих по RS-485
       , DevName varchar(100) null
       , BaudRate int null -- скорость по СОМ-порту
       , PortType int null -- тип порта 0 - 485, 1 - 232
       , dt_updated int null -- последняя установка времени прибора
	   , RemoteID int null -- идентификатор канала на подчиненном сервере
	   , ReqPackSize int null -- максимальный размер пачки на опрос каналов
	   , AddrBase int not null default (1) -- базовый адрес для каналов Modbus. Для разных устройств разный, может быть 0 или 1
	   
       , PRIMARY KEY(ID_Device)
    );
  end if;
  grant all on Devices to "GM_Admin";
  grant all on Devices_id_device_seq to "GM_Admin";  

  pg_gm_db_template_add_column Devices RemoteID int null
  pg_gm_db_template_add_column Devices ReqPackSize int null
  pg_gm_db_template_add_column Devices AddrBase int not null default (1)
  
  pg_gm_db_template_foreign_key Devices ID_Obj Objects cascade  
  pg_gm_db_template_foreign_key Devices ID_DevType DevTypes
  
  if not exists(select * from pg_tables where tablename = lower('DevStates')) then 
    create table DevStates (
		ID_Device int not null references Devices on delete cascade -- ID объекта
		, LastOnline bigint null
		, StateName text null
		, StateValue text null
    );
  end if; 

  pg_gm_db_template_add_column DevStates StateName text null
  pg_gm_db_template_add_column DevStates StateValue text null 

  alter table DevStates drop constraint if exists devstates_pkey;

  if not exists(select * from pg_indexes where tablename = Lower('DevStates') and indexname = 'cidx_devstates_id_name') then
     create unique index cidx_devstates_id_name on DevStates(ID_Device, StateName);
  end if;  
  
  grant all on DevStates to "GM_Admin";  
  
end $$;