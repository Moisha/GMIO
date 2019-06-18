do $$
begin
	-- ДРК-4ОП	
	perform SetDevParamListGuideA (37, 'FLOW', 'AI', 1, 'Расход 1', null, 5/*'м3/ч*/);
	perform SetDevParamListGuideA (37, 'FLOW', 'AI', 2, 'Расход 2', null, 5/*'м3/ч*/);
	perform SetDevParamListGuideA (37, 'V', 'SRC_CNT_MTR', 1, 'Накопленный объем 1', null, 14/*'м3'*/);
	perform SetDevParamListGuideA (37, 'V', 'SRC_CNT_MTR', 2, 'Накопленный объем 2', null, 14/*'м3'*/);
end; $$;