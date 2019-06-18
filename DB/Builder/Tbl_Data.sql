DO $$
begin
  if not exists(select * from pg_tables where tablename = lower('Vals')) then
    create table Vals (
        ID_Prm int not null -- ID параметра
      , UTime bigint not null -- дата и время
      , Val float not null -- значение
    );

    create unique index cidx_vals_id_dt on Vals(ID_Prm, UTime);
  end if;
  alter table Vals cluster on cidx_vals_id_dt;
  grant all on Vals to "GM_Admin";
  
  if not exists(select * from pg_tables where tablename = lower('ArchDay')) then
    create table ArchDay (
        ID_Prm int not null -- ID параметра
      , UTime bigint not null -- дата и время
      , Val float not null -- значение
    );

    create unique index cidx_ArchDay_id_dt on ArchDay(ID_Prm, UTime);
  end if;
  alter table ArchDay cluster on cidx_ArchDay_id_dt;
  grant all on ArchDay to "GM_Admin";

  if not exists(select * from pg_tables where tablename = lower('ArchHour')) then
    create table ArchHour (
        ID_Prm int not null -- ID параметра
      , UTime bigint not null -- дата и время
      , Val float not null -- значение
    );

    create unique index cidx_ArchHour_id_dt on ArchHour(ID_Prm, UTime);
  end if;
  alter table ArchHour cluster on cidx_ArchHour_id_dt;
  grant all on ArchHour to "GM_Admin";

  if not exists(select * from pg_tables where tablename = lower('CurrVals')) then
    create table CurrVals (
        ID_Prm int not null -- ID параметра
      , UTime int not null -- дата и время
      , Val float not null -- значение
    );

    create unique index cidx_currvals_id on CurrVals(ID_Prm);
  end if;
  grant all on CurrVals to "GM_Admin";

  if not exists(select * from pg_tables where tablename = lower('PrecalcNIs')) then
    create table PrecalcNIs (
        ID_Prm int not null -- ID параметра
      , UTime int not null -- дата и время
      , Val float not null -- значение
    );

    create unique index cidx_precalcnis_id_dt on PrecalcNIs(ID_Prm, UTime);
  end if;
  grant all on PrecalcNIs to "GM_Admin";
  
end $$;