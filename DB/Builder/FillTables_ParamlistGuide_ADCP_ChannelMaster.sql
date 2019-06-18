do $$
begin
	-- ADCP ChannelMaster
	perform SetDevParamListGuideA (38, 'FLOW', 'AI', 1, 'Расход', null, 5/*'м3/ч*/);
	perform SetDevParamListGuideA (38, 'VLC', 'AI', 2, 'Скорость потока', null, 15/*'м/c'*/);
	perform SetDevParamListGuideA (38, 'LEVEL', 'AI', 3, 'Глубина', null, 6/*'м'*/); -- В инструкции - Pitch	
	perform SetDevParamListGuideA (38, 'T', 'AI', 4, 'Температура', null, 9/*'C'*/); 
	perform SetDevParamListGuideA (38, 'V', 'SRC_CNT_MTR', 1, 'Накопленный объем', null, 14/*'м3'*/);	
end; $$;