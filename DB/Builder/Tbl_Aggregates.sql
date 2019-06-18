DO $$
begin

  if not exists(select * from pg_tables where tablename = lower('Aggregates')) then
    create table Aggregates (
        ID_Aggregate serial -- ID управляемого агрегата
      , ID_Obj int not null references Objects on delete cascade -- ID объекта
      , ID_AggrType int null -- ID типа управляемого агрегата
      , Name varchar(100) null -- название
      ,PRIMARY KEY(ID_Aggregate)
    );
  end if;
  grant all on Aggregates to "GM_Admin";
  grant all on aggregates_id_aggregate_seq to "GM_Admin";

  pg_gm_db_template_foreign_key Aggregates ID_Obj Objects cascade
  
  if not exists(select * from pg_tables where tablename = lower('AggregateParams')) then
    create table AggregateParams (
        ID_AggregatePrm serial -- ID записи контролируемого параметра
      , ID_Aggregate int not null references Aggregates on delete cascade -- ID управляемого агрегата
      , Caption varchar(100) null -- подпись
      , ID_Prm int null -- ID_Prm 
      , PrmKind int not null -- тип параметра :
						  --CONTROL_PRM_ENG_I = 1;
						  --CONTROL_PRM_ENG_U = 2;
						  --CONTROL_PRM_ENG_P = 3;
						  --CONTROL_PRM_ENG_A = 4;

						  --CONTROL_PRM_VALVE_P1 = 5;
						  --CONTROL_PRM_VALVE_P2 = 6;
						  --CONTROL_PRM_VALVE_Q = 7;

						  --CONTROL_PRM_HEAT_T = 8;

						  --CONTROL_PRM_MANUAL = 9;
						  --CONTROL_PRM_RUN = 10;
	
    );
  end if;
  grant all on AggregateParams to "GM_Admin";
  grant all on AggregateParams_ID_AggregatePrm_seq to "GM_Admin";
 
  pg_gm_db_template_foreign_key AggregateParams ID_Aggregate Aggregates cascade
  pg_gm_db_template_foreign_key AggregateParams ID_Prm Params set null
end $$;