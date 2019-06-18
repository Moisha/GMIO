DO $$
begin

  if not exists(select * from pg_tables where tablename = lower('ParamTypes')) then 
    create table ParamTypes ( 
        ID_PT serial -- ID типа параметра
      , PName varchar (500) not null -- название типа параметра
      , PSign varchar (500) not null -- короткое название
      , DefMeaUnit int null references MeaUnits(ID_MeaUnit) -- единицы измерения по умолчанию
      , IsAlarm int null -- является ли тревогой
      , PRIMARY KEY(ID_PT)
    );
  end if;
  grant all on ParamTypes to "GM_Admin";

  pg_gm_db_template_foreign_key ParamTypes DefMeaUnit MeaUnits(ID_MeaUnit) set null

  if not exists(select * from pg_tables where tablename = lower('SrcTypes')) then
    create table SrcTypes (
	  ID_Src serial -- ID типа параметра
	, SrcName varchar (500) not null -- название типа параметра
	, PRIMARY KEY(ID_Src)
    );
  end if;
  grant all on SrcTypes to "GM_Admin";

  if not exists(select * from pg_constraint where conname = 'srctypes_pkey') then
    alter table SrcTypes add primary key (ID_Src);
  end if;
  
  -- drop table Params
  if not exists(select * from pg_tables where tablename = lower('Params')) then
    create table Params (
     ID_Prm serial -- ID параметра
    ,ID_PT int null references ParamTypes on delete cascade -- ID типа параметра
    ,Name varchar(100) null -- название
    ,ID_Device int references Devices on delete cascade -- ID устройства
    ,ID_Src int references SrcTypes -- тип источника 1 - AI, 2 - DI, 3 - DO, 4 - CNT, 5 - AO, 6 - CNT_AI
    ,N_Src int not null -- номер в источнике
    ,UserSrc int references SrcTypes(ID_Src) -- тип источника 1 - AI, 2 - DI, 3 - DO, 4 - CNT, 5 - AO, 6 - CNT_AI
    ,ArcType int null -- тип архива 0 - не чистить, 1 - текущие, 2 - короткий архив, 3 - длинный архив
    ,DevMin float null
    ,DevMax float null
    ,RealMin float null
    ,RealMax float null
    ,ID_MeaUnit int null references MeaUnits
    ,AlarmSignal int null -- тип аварийного сигнала 0 - аварий не бывает, 1 - nomal closed, 2 - normal opened
    ,AlarmThreshold float null -- порог аварии для AI
    ,NI_StartDT int null
    ,NI_StartVal float null
    ,ExtData varchar(100) null
    ,HourArchAddr int null -- ТЭКОН: номер ячейки с архивами по каналу. Отличается от номера ячейки самого канала, который хранится в N_Src
    ,HourArchArgument int null -- ТЭКОН: глубина архивирования часовых архивов (в днях)
    ,CurrentsAddr int null -- номер ячейки с текущими по каналу. Применяется, если N_Src <= 0
    ,CurrentsArgument int null -- Расширенный номер ячейки. Введен для приборов СПТ, где канал задается номером ячейки (CurrentsAddr) плюс номер трудопровода (CurrentsArgument)
    ,AgeAddr int null -- номер ячейки с возрастом данных. Применяется для каналов AO и DO, используемых в межконтроллерном обмене
    ,BaseChn int null -- базовый канал для фейковых и расчетных
    ,CalibrType int null -- тип датчика
    ,Resistor int null -- нагрузочное сопротивление для токовых входов
    ,CounterCeiling int null -- предел измерения для счетчика, где происходит переход через 0
    ,MeterCalcNIType int null -- метод расчета показаний. 0 - расчетный, 1 - с прибора
    ,OpcTag varchar(100) null -- Тэг для OPC
    ,ReqIntervalType int null -- тип данных из интервала. 0 - обычные, 1 - накопительные на интервале, при расчете брать максимум
    ,RemoteID int null -- идентификатор канала на подчиненном сервере
    ,SourcePrmID int null references Params(ID_Prm) on delete set null -- идентификатор канала, с которого брать данные для выдачи управления
    ,MainSrvPrmID int null -- идентификатор канала на головном сервере
    ,ModbusResultType int null -- тип возвращаемого значения по Modbus (int, single, double ...)
    ,MinIntegralInterval int null -- минимальный интервал усреднения, нужен для медленных счетчиков во избежание скачков на графике
    ,PRIMARY KEY(ID_Prm)
    );
  end if;
  grant all on Params to "GM_Admin";
  grant all on Params_ID_Prm_seq to "GM_Admin";  

  pg_gm_db_template_add_column Params ReqIntervalType int null 
  pg_gm_db_template_add_column Params HourArchArgument int null 
  pg_gm_db_template_add_column Params HourArchAddr int null
  pg_gm_db_template_add_column Params CurrentsAddr int null
  pg_gm_db_template_add_column Params CurrentsArgument int null  
  pg_gm_db_template_add_column Params AgeAddr int null
  pg_gm_db_template_add_column Params UserSrc int null
  pg_gm_db_template_add_column Params RemoteID int null
  pg_gm_db_template_add_column Params SourcePrmID int null
  pg_gm_db_template_add_column Params MainSrvPrmID int null
  pg_gm_db_template_add_column Params ModbusResultType int null
  pg_gm_db_template_add_column Params MinIntegralInterval int null

  pg_gm_db_template_foreign_key Params ID_Device Devices cascade
  pg_gm_db_template_foreign_key Params ID_PT ParamTypes
  pg_gm_db_template_foreign_key Params ID_Src SrcTypes
  pg_gm_db_template_foreign_key Params ID_MeaUnit MeaUnits
  pg_gm_db_template_foreign_key Params UserSrc SrcTypes(ID_Src)
  pg_gm_db_template_foreign_key Params SourcePrmID Params(ID_Prm) set null
  
  if exists(select * from information_schema.columns where table_name = lower('Params') and column_name = lower('LastArchDay')) then
    alter table Params drop column LastArchDay;
  end if;
  
  if exists (select * from information_schema.columns where table_name = lower('Params') and column_name = lower('LastArchHour')) then
    alter table Params drop column LastArchHour;
  end if;

  update Params set CurrentsAddr = N_Src where CurrentsAddr is null and ID_Src = 7; -- SRC_USR
  update Params set N_Src = 0 where ID_Src = 7; 
  
  if not exists(select * from pg_tables where tablename = lower('ParamStates')) then 
    create table ParamStates (
		ID_Prm int not null references Params on delete cascade -- ID объекта
		, LastArchDay bigint null -- отметка времени последнего опроса суточных архивов
		, LastArchHour bigint null -- отметка времени последнего опроса часовых архивов
		, LastCurrent bigint null -- отметка времени последнего опроса текущих
	, PRIMARY KEY(ID_Prm)
    );
  end if;  
  
  grant all on ParamStates to "GM_Admin";  
  
  if not exists(select * from pg_tables where tablename = lower('DevParamListGuide')) then
    create table DevParamListGuide (
        ID_DevType int not null -- ID
      , ID_PT int -- ID типа параметра
      , ID_Src int -- тип источника 1 - AI, 2 - DI, 3 - DO, 4 - CNT_DI, 5 - AO, 6 - SRC_SUM_AI, 7 - USR
      , N_Src int -- номер в источнике
      , PName varchar(500) null -- название параметра
      , ExtData varchar(100) null
      , ID_MeaUnit int null
      , Argument int null -- для добавки параметров по частям, например, номер трубопровода
      , CurrentsAddr int null -- номер ячейки с текущими по каналу. Применяется, если N_Src <= 0
      , HourArchAddr int null -- ТЭКОН: номер ячейки с архивами по каналу. Отличается от номера ячейки самого канала, который хранится в N_Src
      , HourArchArgument int null -- ТЭКОН: глубина архивирования часовых архивов (в днях)
);
  end if;
  grant all on DevParamListGuide to "GM_Admin";

  pg_gm_db_template_add_column DevParamListGuide ID_MeaUnit int null
  pg_gm_db_template_add_column DevParamListGuide Argument int null
  pg_gm_db_template_add_column DevParamListGuide CurrentsAddr int null
  pg_gm_db_template_add_column DevParamListGuide HourArchArgument int null
  pg_gm_db_template_add_column DevParamListGuide HourArchAddr int null
  alter table DevParamListGuide alter column PName type varchar(500);
  alter table DevParamListGuide alter column PName drop not null;
end $$;