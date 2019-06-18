do $$ begin perform DropFunction('SetPT'); end $$;
create or replace function SetPT (
  _ID_PT int,
  _Name varchar (500),
  _Sign varchar(500),
  _DefMeaUnit int = null,
  _IsAlarm int = 0
  )
  returns void
as $$
begin
  if exists(select * from ParamTypes where ID_PT=_ID_PT) then
    update ParamTypes set PName = _Name, PSign=_Sign, DefMeaUnit = _DefMeaUnit, IsAlarm = _IsAlarm where ID_PT = _ID_PT;
  else
    insert into ParamTypes(ID_PT, PName, PSign, DefMeaUnit, IsAlarm) select _ID_PT, _Name, _Sign, _DefMeaUnit, _IsAlarm;
  end if;
end;
$$ language plpgsql;

do $$
begin
	perform SetPT (1, 'Уровень', 'LEVEL', 6);
	perform SetPT (2, 'Ток', 'CURR_PUMP', 2);
	--perform SetPT 3, 'Ток насоса №2', 'CURR_PUMP2', 2);
	perform SetPT (4, 'Авария: отсутствие питания', 'CONTROL_POWER', null, 1);
	perform SetPT (5, 'Авария: затопление машинного отделения', 'CONTROL_FLOOD', null, 1);
	perform SetPT (6, 'Сработка охранной сигнализации', 'CONTROL_SECURITY', null, 1);
	perform SetPT (7, 'Сработка пожарной сигнализации', 'CONTROL_FIRE', null, 1);
	perform SetPT (8, 'Авария насоса', 'ALARM_PUMP', null, 1);
	--perform SetPT 9, 'Авария насоса №2', 'ALARM_PUMP2', null, 1);
	perform SetPT (10, 'Авария «Отсутствие питания»', 'ALARM_NO_POWER', null, 1);
	perform SetPT (11, 'Общая авария', 'ALARM_COMMON', null, 1);
	perform SetPT (12, 'Давление', 'PRESSURE_DT', 7);
	perform SetPT (13, 'Наличие питания', 'POWER_ON', null, 1);
	perform SetPT (14, 'Расход', 'FLOW', 5);
	--perform SetPT 15, 'Расход 2', 'OUTLET_DT2', 5
	--perform SetPT 16, 'Расход 3', 'OUTLET_DT3', 5
	--perform SetPT 17, 'Давление 2', 'PRESSURE_DT2', 7
	--perform SetPT 18, 'Ток насоса №3', 'CURR_PUMP3', 2
	perform SetPT (19, 'Напряжение', 'U_FEED', 1);
	perform SetPT (20, 'Авария УБЗ', 'ALARM_UBZ', null, 1);
	perform SetPT (21, 'Напряжение по фазе 1', 'U_PH1', 1);
	perform SetPT (22, 'Напряжение по фазе 2', 'U_PH2', 1);
	perform SetPT (23, 'Напряжение по фазе 3', 'U_PH3', 1);
	perform SetPT (24, 'Ток по фазе 1', 'I_PH1', 2);
	perform SetPT (25, 'Ток по фазе 2', 'I_PH2', 2);
	perform SetPT (26, 'Ток по фазе 3', 'I_PH3', 2);
	perform SetPT (27, 'Счетчик электроэнергии', 'E', 8);
	perform SetPT (28, 'Управление двигателем', 'START_UBZ');
	perform SetPT (29, 'Ручное управление', 'MANUAL_CONTROL');
	--perform SetPT 30, 'Ручное управление 2', 'MANUAL_CONTROL2', 0
	--perform SetPT 31, 'Ручное управление 3', 'MANUAL_CONTROL3', 0
	--perform SetPT 32, 'Ручное управление 4', 'MANUAL_CONTROL4', 0
	perform SetPT (33, 'Температура', 'T', 9);
	--perform SetPT 34, 'Температура 2', 'T2', 9
	--perform SetPT 35, 'Температура 3', 'T3', 9
	--perform SetPT 36, 'Температура 4', 'T4', 9
	perform SetPT (37, 'Авария ТР', 'ALARM_TR', null, 1);
	perform SetPT (40, 'Наработка двигателя', 'UBZ_ENG_HOURS', 10);
	perform SetPT (41, 'Мощность', 'W', 29);
	perform SetPT (42, 'Уставка', 'Preset');
	perform SetPT (43, 'Сброс аварии', 'RESET_ALARM', null, 1);
	perform SetPT (44, 'Ток нулевой последовательности', 'I0', 2);
	perform SetPT (45, 'Скорость вращения', 'Vrot', 12);
	perform SetPT (46, 'Опорное значение ПИД', 'PID_REFERENCE');
	perform SetPT (47, 'Объем', 'V', 14);
	perform SetPT (48, 'Скорость', 'Vlc', 15);
	perform SetPT (49, 'Давление', 'P', 20);
	perform SetPT (50, 'Массовый расход', 'FLOW_MASS', 5);
	perform SetPT (51, 'Масса', 'MASS', 22);
	perform SetPT (52, 'Энергия А+', 'Ain', null);
	perform SetPT (53, 'Энергия А-', 'Aout', null);
	perform SetPT (54, 'Энергия R+', 'Qin', null);
	perform SetPT (55, 'Энергия R-', 'Qout', null);
	perform SetPT (56, 'Частота', 'F', 24);
	perform SetPT (57, 'Момент двигателя', 'Мm', null);
	perform SetPT (58, 'Напряжение постоянного тока', 'Uac', null);
	perform SetPT (59, 'Напряжение двигателя', 'Um', 1);
	perform SetPT (60, 'Прочие', '?', null);
	perform SetPT (61, 'Тепловая энергия', 'Q(h)', 27);
	perform SetPT (62, 'Деньги', 'GOLD', 28);
end; $$;
