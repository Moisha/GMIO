do $$
begin
	-- ADCP ChannelMaster
	perform SetDevParamListGuideA (39, 'FLOW', 'AI', 1, 'Расход в день', null, 5/*'м3/ч*/);
	perform SetDevParamListGuideA (39, 'FLOW', 'AI', 2, 'Расход в час', null, 5/*'м3/ч*/);
	perform SetDevParamListGuideA (39, 'FLOW', 'AI', 3, 'Расход в секунду', null, 5/*'м3/ч*/);
	perform SetDevParamListGuideA (39, 'VLC', 'AI', 4, 'Скорость потока', null, 15/*'м/c'*/);
	perform SetDevParamListGuideA (39, '?', 'AI', 5, 'Аналоговый выход 1', null, null);
	perform SetDevParamListGuideA (39, '?', 'AI', 6, 'Код ошибки', null, null);
	perform SetDevParamListGuideA (39, '?', 'AI', 7, 'Качество сигнала', null, null);
	perform SetDevParamListGuideA (39, 'V', 'SRC_CNT_MTR', 1, 'Накопленный объем', null, 14/*'м3'*/);	
end; $$;