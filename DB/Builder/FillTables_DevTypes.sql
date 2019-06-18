
do $$ begin perform DropFunction('SetDevType'); end $$;
create or replace function SetDevType(
  _ID_DevType int,
  _Name varchar (500),
  _FixedPT int = 0,
  _ID_Group int = null,
  _AllowZeroNumber int = null,
  _DefaultChannelsArgument int = null
  )
  returns void
as $$
begin
  if exists(select * from DevTypes where ID_DevType = _ID_DevType) then
    update DevTypes set Name = _Name, FixedPT = _FixedPT, ID_DevTypeGroup = _ID_Group, AllowZeroNumber = _AllowZeroNumber, DefaultChannelsArgument = _DefaultChannelsArgument where ID_DevType = _ID_DevType;
  else
    insert into DevTypes(ID_DevType, Name, FixedPT, ID_DevTypeGroup, AllowZeroNumber, DefaultChannelsArgument) select _ID_DevType, _Name, _FixedPT, _ID_Group, _AllowZeroNumber, _DefaultChannelsArgument;
  end if;
end;
$$ language plpgsql;


do $$
begin
	delete from DevTypeGroups where ID_DevTypeGroup <= 2;
	insert into DevTypeGroups (ID_DevTypeGroup, Name) select 1, 'ТЭКОН 19';
	insert into DevTypeGroups (ID_DevTypeGroup, Name) select 2, 'Логика';

	perform SetDevType (1, 'Geomer');
	perform SetDevType (2, 'ICP7041D');
	perform SetDevType (3, 'УБЗ-302', 1);
	perform SetDevType (4, 'ICP7017');
	perform SetDevType (5, 'ТР-101', 1);
	perform SetDevType (6, 'ТРМ-138');
	perform SetDevType (7, 'Взлет УРСВ', 1);
	perform SetDevType (8, 'Vacon NXL', 1);

	perform SetDevType (9, 'ТЭКОН 19-01', 0, 1, 1);
	perform SetDevType (10, 'ТЭКОН 19-02', 0, 1, 1);
	perform SetDevType (11, 'ТЭКОН 19-03', 0, 1, 1);
	perform SetDevType (12, 'ТЭКОН 19-04', 0, 1, 1);
	perform SetDevType (13, 'ТЭКОН 19-05', 0, 1, 1);
	perform SetDevType (14, 'ТЭКОН 19-06', 0, 1, 1);
	perform SetDevType (15, 'ТЭКОН 19-07', 0, 1, 1);
	perform SetDevType (16, 'ТЭКОН 19-08', 0, 1, 1);
	perform SetDevType (17, 'ТЭКОН 19-09', 0, 1, 1);
	perform SetDevType (18, 'ТЭКОН 19-10', 0, 1, 1);
	perform SetDevType (19, 'ТЭКОН 19-11', 0, 1, 1);
	perform SetDevType (20, 'ТЭКОН 19-12', 0, 1, 1);
	perform SetDevType (21, 'ТЭКОН 19-13', 0, 1, 1);
	perform SetDevType (22, 'ТЭКОН 19-14', 0, 1, 1);

	perform SetDevType (23, 'Isco 4250', 1);
	perform SetDevType (24, 'СПТ961', 1, 2, 1, 2);
	perform SetDevType (25, 'Симаг 11', 1);
	perform SetDevType (26, 'Geomer + ISCO');
	perform SetDevType (27, 'Питерфлоу РС', 1, null, 1);
	perform SetDevType (28, 'Меркурий 230', 1);
	perform SetDevType (29, 'Altistart 22', 1);
	perform SetDevType (30, 'Геострим 71', 1);
	perform SetDevType (31, 'Modbus TCP', 0, null, 1);
	perform SetDevType (32, 'Головной сервер', 0, null, 1);
	perform SetDevType (33, 'СПТ941', 1, 2, 1);
	perform SetDevType (34, 'СПТ943', 1, 2, 1);
	perform SetDevType (35, 'Geomer + УБЗ', 0, null, 1, 1);
	perform SetDevType (36, 'Modbus RTU', 0, null, 1);
	perform SetDevType (37, 'ДРК-4ОП', 1);
	perform SetDevType (38, 'ADCP ChannelMaster', 1, null, 1);
end; $$;