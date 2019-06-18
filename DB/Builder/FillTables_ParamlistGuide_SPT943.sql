-- СПТ 943

do $$
declare _t int;
begin
	-- Текущие параметры по каналу ОБЩ (Ch=0)
	perform SetDevParamListGuideA (34, 'T', 'AI', 1, 'Температура холодной воды', null, null, 1026, 0);
	perform SetDevParamListGuideA (34, 'T', 'AI', 2, 'Температура воздуха', null, null, 1027, 0);
	
	-- Текущие параметры по каналам ТВ1 (Ch=1) и ТВ2 (Ch=2)
	for _t in 1..2 
	loop
		perform SetDevParamListGuideA (34, 'FLOW', 'AI', _t * 100 + 1, 'ТВ' || _t || '. Объемный расход теплоносителя по трубопроводу 1', null, null, 1025, _t);
		perform SetDevParamListGuideA (34, 'FLOW', 'AI', _t * 100 + 2, 'ТВ' || _t || '. Объемный расход теплоносителя по трубопроводу 2', null, null, 1026, _t);
		perform SetDevParamListGuideA (34, 'FLOW', 'AI', _t * 100 + 3, 'ТВ' || _t || '. Объемный расход теплоносителя по трубопроводу 3', null, null, 1027, _t);
		
		perform SetDevParamListGuideA (34, 'P', 'AI', _t * 100 + 4, 'ТВ' || _t || '. Давление теплоносителя по трубопроводу 1', null, null, 1028, _t);
		perform SetDevParamListGuideA (34, 'P', 'AI', _t * 100 + 5, 'ТВ' || _t || '. Давление теплоносителя по трубопроводу 2', null, null, 1029, _t);
		
		perform SetDevParamListGuideA (34, 'T', 'AI', _t * 100 + 6, 'ТВ' || _t || '. Температура теплоносителя по трубопроводу 1', null, null, 1030, _t);
		perform SetDevParamListGuideA (34, 'T', 'AI', _t * 100 + 7, 'ТВ' || _t || '. Температура теплоносителя по трубопроводу 2', null, null, 1031, _t);
		perform SetDevParamListGuideA (34, 'T', 'AI', _t * 100 + 8, 'ТВ' || _t || '. Разность температур',                         null, null, 1032, _t);
		perform SetDevParamListGuideA (34, 'T', 'AI', _t * 100 + 9, 'ТВ' || _t || '. Температура теплоносителя по трубопроводу 3', null, null, 1033, _t);
		
		perform SetDevParamListGuideA (34, 'T', 'AI', _t * 100 + 10, 'ТВ' || _t || '. Температура холодной воды', null, null, 1034, _t);
		perform SetDevParamListGuideA (34, 'T', 'AI', _t * 100 + 11, 'ТВ' || _t || '. Температура воздуха',       null, null, 1035, _t);		
	end loop;
	
	-- Тотальные параметры по каналу ОБЩ (Ch=0)
	perform SetDevParamListGuideA (34, 'Q(h)', 'SRC_CNT_MTR', 1, 'Суммарная тепловая энергия', null, null, 2048, _t);		
	
	--Тотальные параметры по каналам ТВ1 (Ch=1) и ТВ2 (Ch=2)
	for _t in 1..2 
	loop
		perform SetDevParamListGuideA (34, 'V', 'SRC_CNT_MTR', _t * 100 + 1, 'ТВ' || _t || '. Объем теплоносителя по трубопроводу 1', null, null, 2048, _t);
		perform SetDevParamListGuideA (34, 'V', 'SRC_CNT_MTR', _t * 100 + 2, 'ТВ' || _t || '. Объем теплоносителя по трубопроводу 2', null, null, 2049, _t);
		perform SetDevParamListGuideA (34, 'V', 'SRC_CNT_MTR', _t * 100 + 3, 'ТВ' || _t || '. Объем теплоносителя по трубопроводу 3', null, null, 2050, _t);
		
		perform SetDevParamListGuideA (34, 'MASS', 'SRC_CNT_MTR', _t * 100 + 4, 'ТВ' || _t || '. Масса теплоносителя по трубопроводу 1', null, null, 2051, _t);
		perform SetDevParamListGuideA (34, 'MASS', 'SRC_CNT_MTR', _t * 100 + 5, 'ТВ' || _t || '. Масса теплоносителя по трубопроводу 2', null, null, 2052, _t);
		perform SetDevParamListGuideA (34, 'MASS', 'SRC_CNT_MTR', _t * 100 + 6, 'ТВ' || _t || '. Масса теплоносителя по трубопроводу 3', null, null, 2053, _t);
		
		perform SetDevParamListGuideA (34, 'Q(h)', 'SRC_CNT_MTR', _t * 100 + 7, 'ТВ' || _t || '. Тепловая энергия', null, null, 2054, _t);
		perform SetDevParamListGuideA (34, 'Q(h)', 'SRC_CNT_MTR', _t * 100 + 8, 'ТВ' || _t || '. Тепловая энергия ГВС', null, null, 2055, _t);
	end loop;
end; $$;
