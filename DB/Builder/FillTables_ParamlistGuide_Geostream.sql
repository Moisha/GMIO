
do $$
declare _i int;
begin
	_i := 30;
	perform SetDevParamListGuideA(_i, 'LEVEL', 'AI', 1, 'Уровень жидкости', null, 6);
	perform SetDevParamListGuideA(_i, 'Vlc', 'AI', 2, 'Скорость жидкости', null, 15);
	perform SetDevParamListGuideA(_i, 'FLOW', 'AI', 3, 'Расход жидкости', null, 5);

	perform SetDevParamListGuideA(_i, 'V', 'SRC_CNT_MTR', 1, 'Объем в прямом направлении', null, 14);
	perform SetDevParamListGuideA(_i, 'V', 'SRC_CNT_MTR', 2, 'Объем в обратном направлении', null, 14);
end; $$;
