-- clean up
delete from SystemStates;
delete from Params;
delete from ParamStates;
delete from AggregateParams;
delete from Aggregates;
delete from Devices;
delete from Objects;
delete from ObjectStates;
delete from Nodes;
delete from NodeChannels;
delete from Users;
delete from UserGroups;
delete from ObjectPermissions;
delete from NodePermissions;
delete from Vals;
delete from CurrVals;
delete from ArchDay;
delete from ArchHour;

do $$ begin perform UpdateAutoindent(1); end; $$;

-- Objects
insert into objects (id_obj, Name, N_Car) select 1, 'Объект 1', 100;
insert into objects (id_obj, Name, N_Car) select 2, 'Объект 2', 200;
insert into objects (id_obj, Name, N_Car) select 3, 'Объект 3', 300;
insert into objects (id_obj, Name, N_Car, ObjType) select 4, 'Объект 4', 400, 6;
insert into objects (id_obj, Name, N_Car, ObjType, Converter) select 5, 'Объект 5 TCP', 500, 6, 1;
insert into objects (id_obj, Name, N_Car, ObjType) select 6, 'Объект 6 COM', 1, 1;

-- Devices
insert into devices(ID_Device, id_obj, id_devtype, devname) select 1, 1, 1, 'Geomer';
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 2, 1, 2, 'I7041D', 1;
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 3, 2, 24, 'СПТ 961 N0', 0;
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 4, 2, 24, 'СПТ 961 N11', 11;
-- Allen Bradley для проверки управления 
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 5, 4, 31, 'Управление Allen Bradley Micro8xx', 1;
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 6, 5, 31, 'Управление Allen Bradley Micro8xx TCP', 1;

-- Приборы для проверки управления
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 7, 6, 3, 'Управление УБЗ', 1;
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 8, 6, 8, 'Управление Vacon', 2;
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 9, 6, 5, 'Управление TR-101', 3;
insert into devices(ID_Device, id_obj, id_devtype, devname, number) select 10, 6, 36, 'Modbus RTU', 4;

insert into Devices(ID_Device, id_obj, id_devtype, devname, number)
  select 1000 + ID_DevType, 3, ID_DevType, name, ID_DevType from DevTypes; -- не менять ID_Obj = 3, используется в тестах

insert into Devices(ID_Device, id_obj, id_devtype, devname, number, ReqPackSize)
  select 2000 + ID_DevType, 5, ID_DevType, name, ID_DevType, 500 from DevTypes where ID_DevType = 36; -- для проверки схемы, когда стоит конвертер Modbus TCP-RTU

-- Раздадим всем каналы
do $$ 
declare _idd int;
begin 
  for _idd in (select ID_Device from Devices where ID_DevType <> 24) 
  loop
    perform AddDeviceLostParams(_idd, null); 	
  end loop;

  -- СПТ 961 - отдельно
  insert into Params(ID_Prm, n_src) select 1000, 1;
  perform UpdateAutoindent(1);
  delete from Params where ID_Prm = 1000;

  for _idd in (select ID_Device from Devices where ID_DevType = 24 order by ID_Device) 
  loop
    perform AddDeviceLostParams(_idd, 0);
  end loop;
end $$;

-- Тэкон 19
insert into Params(ID_Device, ID_PT, ID_Src, n_src, CurrentsAddr) select 1009, 12, 7, 0, 32803 /*0x8023*/;

-- Головной сервер
insert into Params(ID_Prm, ID_Device, ID_PT, ID_Src, n_src, CurrentsAddr, UserSrc, MainSrvPrmID) select 1051, 1032, 12, 7, 0, 259, 2, 1234;
do $$ begin perform UpdateAutoindent(1); end; $$;

-- Стандартные Modbus TCP и Modbus RTU имеет только свободно конфигурируемые каналы
do $$ 
declare _i int; _j int;
begin
  for _i in 1031..2036 loop
    if _i in (1031, 1036, 2036) then
      insert into Params(ID_Device, ID_PT, ID_Src, n_src, CurrentsAddr, UserSrc) select _i, 12, 7, 0, 1, 1;
      insert into Params(ID_Device, ID_PT, ID_Src, n_src, CurrentsAddr, UserSrc) select _i, 12, 7, 0, 3, 1;
      insert into Params(ID_Device, ID_PT, ID_Src, n_src, CurrentsAddr, UserSrc) select _i, 12, 7, 0, 259, 1;
      insert into Params(ID_Device, ID_PT, ID_Src, n_src, CurrentsAddr, UserSrc) select _i, 12, 7, 0, 259, 2;

      if _i = 1036 then
        for _j in 0..7 loop
          insert into Params(ID_Device, ID_PT, ID_Src, n_src, CurrentsAddr, UserSrc, ModbusResultType) select _i, 12, 7, 0, 1000 + _j * 100, 1, _j;
        end loop;
      end if;
    end if;
  end loop;
end; $$;

-- для проверки управления RegularControl
insert into Params(ID_Device, ID_PT, ID_Src, n_src, UserSrc, CurrentsAddr, AgeAddr, SourcePrmID) select 5, 12, 7, 0, 5, 259, 700, 1;
-- RegularControl Age
insert into Params(ID_Device, ID_PT, ID_Src, n_src, UserSrc, CurrentsAddr) select 5, 12, 7, 0, 5, 700;
-- ModbusFnc 0x10 TCP
insert into Params(ID_Device, ID_PT, ID_Src, n_src, UserSrc, CurrentsAddr) select 5, 12, 7, 0, 10, 800;
-- ModbusFnc RTU управление
insert into Params(ID_Device, ID_PT, ID_Src, n_src, UserSrc, CurrentsAddr) select 10, 12, 7, 0, 5, 1;
-- ModbusFnc 0x10 RTU
insert into Params(ID_Device, ID_PT, ID_Src, n_src, UserSrc, CurrentsAddr) select 10, 12, 7, 0, 10, 2;

do $$ begin perform UpdateAutoindent(1); end; $$;

-- И все каналы доступными
update Params 
  set ID_PT = case ID_Src 
                  when 1 then 1 -- уровень
                  when 2 then 8 -- авария
                  when 4 then 14 -- расход
                end 
  where ID_PT is null;

-- Aggregates
insert into Aggregates(ID_Aggregate, ID_Obj, ID_AggrType, Name) select 1, 1, 1, 'Двигатель';

-- AggregateParams
insert into AggregateParams(ID_AggregatePrm, ID_Aggregate, Caption, ID_Prm, PrmKind) select 1, 1, 'Сброс аварии', 1, 12;
insert into AggregateParams(ID_AggregatePrm, ID_Aggregate, Caption, ID_Prm, PrmKind) select 2, 1, 'Ручн.',        2, 9;
insert into AggregateParams(ID_AggregatePrm, ID_Aggregate, Caption, ID_Prm, PrmKind) select 3, 1, 'Старт',        3, 10;
insert into AggregateParams(ID_AggregatePrm, ID_Aggregate, Caption, ID_Prm, PrmKind) select 4, 1, 'Уставка',      4, 11;

-- Nodes
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 1, 1, 'Главный узел', null;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 2, 1, 'Район 1', 1;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 3, 2, 'Узел теплоучета 11', 2;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 4, 2, 'Узел теплоучета 12', 2;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 5, 4, 'Подача', 3;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 6, 5, 'Обратка', 3;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 7, 6, 'Подпитка', 3;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 8, 4, 'Подача', 4;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 9, 5, 'Обратка', 4;
insert into Nodes(ID_Node, ID_NodeType, Name, ID_Parent) select 10, 6, 'Подпитка', 4;

insert into NodeChannels(ID_Chn, ID_Node, Name, ID_PT, ID_MeaUnit, BaseChn) select 1, 5, '', 49, null, 3; 
insert into NodeChannels(ID_Chn, ID_Node, Name, ID_PT, ID_MeaUnit, BaseChn) select 2, 5, 'Имя: Расход', 14, null, 4;

-- пользователи
insert into Users (ID_User, login, Password, State) select 1, 'user', md5('123'), 0;
insert into Users (ID_User, login, Password, State) select 2, 'admin', md5('456'), 1;
insert into Users (ID_User, login, Password, State) select 3, 'group', '' , 2;
insert into Users (ID_User, login, Password, State) select 4, 'UserInGroup', md5('789'), 0;

-- select * from Users

insert into UserGroups(ID_User, ID_Group) select 4, 3;

insert into ObjectPermissions(ID_User, ID_Obj) select 1, 1;
insert into ObjectPermissions(ID_User, ID_Obj) select 3, 2;
insert into ObjectPermissions(ID_User, ID_Obj) select 4, 3;

do $$ begin perform UpdateAutoindent(1); end; $$;

-- включим обязательную авторизацию
do $$
begin
  perform SetConfigPrm('AuthRequired', 'login');
end; $$;

-- данные
do $$ 
declare _dt timestamp with time zone; _dt_start timestamp with time zone; _dt_end timestamp with time zone; _s text; _hour int; _day int; id_chn_cnt int; id_chn_mtr int;
begin 
  select ID_Prm into id_chn_cnt from Params where ID_Device = 1 and ID_Src = 4 and N_Src = 1; 
  -- канал с показаниями счетчика подвергнем калибровке
  select ID_Prm into id_chn_mtr from Params where ID_Device = 1038 and ID_Src = 9 and N_Src = 1;
  update Params set DevMin = 0, DevMax = 1, RealMin = 10000, RealMax =  10001 where ID_Prm = id_chn_mtr;

  _dt := now()::date - interval '1 day';
  _s := extract(year from _dt) || '-' || extract(month from _dt) || '-' || extract(day from _dt) || ' 00:00:00+5'; -- переведем в заведомо наш часовой пояс
  _dt_start := _s;
  _dt_end := _dt_start + interval '2 day';

  _dt := _dt_start;
  while _dt <= _dt_end
  loop
    _hour := extract(hour from _dt - _dt_start)::int;
    _day := extract(day from _dt - _dt_start)::int;
    
    perform WriteVal(1, DTtoUTC(_dt), 10000 + _hour * 100 + extract(minute from _dt));
    perform WriteVal(2, DTtoUTC(_dt), 20000 + _hour * 100 + extract(minute from _dt));
    perform WriteVal(3, DTtoUTC(_dt), 30000 + _hour * 100 + extract(minute from _dt));
    perform WriteVal(4, DTtoUTC(_dt), 40000 + _hour * 100 + extract(minute from _dt));
    perform WriteVal(id_chn_cnt, DTtoUTC(_dt), _day * 3000 + _hour * 100 + extract(minute from _dt));
    perform WriteVal(id_chn_mtr, DTtoUTC(_dt), _day * 3000 + _hour * 100 + extract(minute from _dt));

    _dt := _dt + interval '1 minute';
  end loop;

  -- часовки и события
  _dt := _dt_start;
  while _dt <= _dt_end
  loop
    _hour := extract(hour from _dt - _dt_start)::int;
    perform WriteVal(17, DTtoUTC(_dt), _hour % 2);
    perform WriteArchHour(1, DTtoUTC(_dt), _hour);

    _dt := _dt + interval '1 hour';
  end loop;

  -- суточные
  _dt := _dt_start;
  perform WriteArchDay(1, DTtoUTC(_dt), 1);
end; $$;