do $$
begin
	-- Geomer

	perform SetDevParamListGuide_Bunch (1, 1, 0, 3); -- AI 0..3
	perform SetDevParamListGuide_Bunch (1, 2, 1, 4); -- DI 1..4
	perform SetDevParamListGuide_Bunch (1, 3, 1, 4); -- DO 1..4
	perform SetDevParamListGuide_Bunch (1, 4, 1, 4); -- CNT 1..4

	-- Geomer + ISCO

	perform SetDevParamListGuide_Bunch (26, 1, 0, 3); -- AI 0..3
	perform SetDevParamListGuide_Bunch (26, 2, 1, 4); -- DI 1..4
	perform SetDevParamListGuide_Bunch (26, 3, 1, 4); -- DO 1..4
	perform SetDevParamListGuide_Bunch (26, 4, 1, 4); -- CNT 1..4

	perform SetDevParamListGuide (26, 1, 1, 10); -- ISCO уровень
	perform SetDevParamListGuide (26, 48, 1, 11); -- ISCO скорость
	perform SetDevParamListGuide (26, 14, 1, 12); -- ISCO расход
	perform SetDevParamListGuide (26, 47, 4, 10); -- ISCO объем
	
	-- Geomer + УБЗ

	perform SetDevParamListGuide_Bunch (35, 1, 0, 3); -- AI 0..3
	perform SetDevParamListGuide_Bunch (35, 2, 1, 4); -- DI 1..4
	perform SetDevParamListGuide_Bunch (35, 3, 1, 4); -- DO 1..4
	perform SetDevParamListGuide_Bunch (35, 4, 1, 4); -- CNT 1..4
	
	perform SetDevParamListGuideA (35, 'GOLD', 'AI', 100, 'Баланс', null, 28);
	
	for _t in 0..7
	loop
		perform SetDevParamListGuideA (35, 'CURR_PUMP', 	'AI', 		101 + 10 * _t, 'УБЗ ' || _t + 1 || '. Усредененный ток');
		perform SetDevParamListGuideA (35, 'U_FEED', 	'AI', 		102 + 10 * _t, 'УБЗ ' || _t + 1 || '. Усредненное напряжение');
		perform SetDevParamListGuideA (35, 'W', 	'AI', 		103 + 10 * _t, 'УБЗ ' || _t + 1 || '. Активная мощность');
		perform SetDevParamListGuideA (35, 'ALARM_UBZ', 	'AI', 		104 + 10 * _t, 'УБЗ ' || _t + 1 || '. Авария');

		update DevParamListGuide set Argument = _t + 1 where ID_DevType = 35 and N_Src between 101 + _t * 10 and 109 + _t * 10;
	end loop;	
end; $$;