-- СПТ 961

do $$
declare _t int;
begin
	-- общие
	perform SetDevParamListGuideA (24, 'T', 'AI', 1, 'Температура НВ', '0 63 83 82', 9);
	perform SetDevParamListGuideA (24, 'T', 'AI', 2, 'Температура ХВ', '0 65 72 71', 9);
	perform SetDevParamListGuideA (24, 'P', 'AI', 3, 'Давление ХВ', '0 66 75 74', 20);

	for _t in 1..12 
	loop
		-- первый канал
		perform SetDevParamListGuideA (24, 'P', 	'AI', 		10 * _t + 0, 'T' || _t || ' Давление теплоносителя', 		_t || ' 155 206 205', 20);
		perform SetDevParamListGuideA (24, 'T', 	'AI', 		10 * _t + 2, 'T' || _t || ' Температура теплоносителя', 	_t || ' 156 201 200', 9);
		perform SetDevParamListGuideA (24, 'FLOW_MASS', 'AI', 		10 * _t + 3, 'T' || _t || ' Массовый расход теплоносителя', 	_t || ' 157 211 210', 21);
		perform SetDevParamListGuideA (24, 'FLOW', 	'AI', 		10 * _t + 4, 'T' || _t || ' Расход теплоносителя', 		_t || ' 171 221 220', 5);

		perform SetDevParamListGuideA (24, 'MASS', 	'SRC_CNT_MTR', 	10 * _t + 1, 'T' || _t || ' Показания счетчика по массе', 	_t || ' 160', 22); 
		perform SetDevParamListGuideA (24, 'V', 	'SRC_CNT_MTR', 	10 * _t + 2, 'T' || _t || ' Показания счетчика по объему', 	_t || ' 163', 14);

		update DevParamListGuide set Argument = _t where ID_DevType = 24 and N_Src between 10 * _t and 10 * (_t + 1) - 1;
	end loop;
end; $$;

