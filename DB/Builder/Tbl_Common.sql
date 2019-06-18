DO $$
begin
  if not exists(SELECT * FROM pg_roles WHERE lower(rolname) = lower('GM_Admin')) then
    CREATE ROLE "GM_Admin"
      NOSUPERUSER NOINHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
  end if;

  if not exists(select * from pg_tables where tablename = lower('SysConfig')) then
    create table SysConfig (
	    PrmName varchar (500) not null	-- имя параметра конфигурации
	  , PrmValue varchar (500) not null	-- значение
	);
  end if;  
  grant all on SysConfig to "GM_Admin";

  -- различные счетики состояния системы
  if not exists(select * from pg_tables where tablename = lower('SystemStates')) then
    create table SystemStates (
	    StateName text not null	-- имя параметра
	  , StateValue text not null	-- значение
	  , UTUpdate bigint not null	-- время последнего обновления
	  , PRIMARY KEY(StateName)
	);
  end if;  
  grant all on SystemStates to "GM_Admin";

  if not exists(select * from pg_tables where tablename = lower('MeaUnits')) then
    create table MeaUnits (
        ID_MeaUnit int not null -- ID
      , Name varchar(50) -- название
      , LongName varchar(500)  
      , PRIMARY KEY(ID_MeaUnit)
    );
   end if;
   grant all on MeaUnits to "GM_Admin";
  
end $$;