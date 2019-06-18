-- СПТ 941

do $$
begin
    -- Текущие параметры 
	perform SetDevParamListGuideA (33, 'FLOW', 'AI', 1, 'Объемный расход теплоносителя по трубопроводу 1', null, 5, 1027, 0, 15);
	perform SetDevParamListGuideA (33, 'FLOW', 'AI', 2, 'Объемный расход теплоносителя по трубопроводу 2', null, 5, 1028, 0, 16);
	perform SetDevParamListGuideA (33, 'FLOW', 'AI', 3, 'Объемный расход теплоносителя по трубопроводу 3', null, 5, 1029, 0, 17);

	perform SetDevParamListGuideA (33, 'FLOW_MASS', 'AI', 4, 'Массовый расход теплоносителя по трубопроводу 1', null, 21, 1030, 0, 18);
	perform SetDevParamListGuideA (33, 'FLOW_MASS', 'AI', 5, 'Массовый расход теплоносителя по трубопроводу 2', null, 21, 1031, 0, 19);
	perform SetDevParamListGuideA (33, 'FLOW_MASS', 'AI', 6, 'Массовый расход теплоносителя по трубопроводу 3', null, 21, 1032, 0, 20);

	perform SetDevParamListGuideA (33, 'T', 'AI', 7, 'Температура теплоносителя по трубопроводу 1', null, 9, 1033, 0, 4);
	perform SetDevParamListGuideA (33, 'T', 'AI', 8, 'Температура теплоносителя по трубопроводу 2', null, 9, 1034, 0, 5);
	perform SetDevParamListGuideA (33, 'T', 'AI', 9, 'Разность температур', null, 9, 1035, 0, 6);
	perform SetDevParamListGuideA (33, 'T', 'AI', 10, 'Температура теплоносителя по трубопроводу 3', null, 9, 1036, 0, 7);
	perform SetDevParamListGuideA (33, 'T', 'AI', 11, 'Температура холодной воды', null, 9, 1037, 0, 8);
	perform SetDevParamListGuideA (33, 'T', 'AI', 12, 'Температура по дополнительному каналу измерений', null, 9, 1038, 0);

	perform SetDevParamListGuideA (33, 'P', 'AI', 13, 'Давление теплоносителя по трубопроводу 1', null, 20, 1039, 0, 10);
	perform SetDevParamListGuideA (33, 'P', 'AI', 14, 'Давление теплоносителя по трубопроводу 2', null, 20, 1040, 0, 11);
	perform SetDevParamListGuideA (33, 'P', 'AI', 15, 'Давление теплоносителя по трубопроводу 3', null, 20, 1041, 0, 12);
	perform SetDevParamListGuideA (33, 'P', 'AI', 16, 'Давление холодной воды', null, 20, 1042, 0, 13);
	perform SetDevParamListGuideA (33, 'P', 'AI', 17, 'Давление по дополнительному каналу измерений', null, 20, 1043, 0, 14);
	
	-- Тотальные параметры тепловычислителя
	perform SetDevParamListGuideA (33, 'V', 'SRC_CNT_MTR', 1, 'Объем теплоносителя по трубопроводу 1', null, null, 2048);
	perform SetDevParamListGuideA (33, 'V', 'SRC_CNT_MTR', 2, 'Объем теплоносителя по трубопроводу 2', null, null, 2049);
	perform SetDevParamListGuideA (33, 'V', 'SRC_CNT_MTR', 3, 'Объем теплоносителя по трубопроводу 3', null, null, 2050);
	
	perform SetDevParamListGuideA (33, 'MASS', 'SRC_CNT_MTR', 4, 'Масса теплоносителя по трубопроводу 1', null, null, 2051);
	perform SetDevParamListGuideA (33, 'MASS', 'SRC_CNT_MTR', 5, 'Масса теплоносителя по трубопроводу 2', null, null, 2052);
	perform SetDevParamListGuideA (33, 'MASS', 'SRC_CNT_MTR', 6, 'Масса теплоносителя по трубопроводу 3', null, null, 2053);
	
	update Params set HourArchAddr = (select HourArchAddr from DevParamListGuide g where g.ID_DevType = 33 and g.ID_Src = Params.ID_Src and g.N_Src = Params.N_Src)
		where ID_Device in (select ID_Device from Devices where ID_DevType = 33)
		      and RemoteId is null;	
end; $$;

