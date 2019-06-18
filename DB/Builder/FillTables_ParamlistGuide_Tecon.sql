do $$
begin
	-- perform SetDevType 9, 'ТЭКОН 19-01'
	-- AI 1..4
	perform SetDevParamListGuide_Bunch (9, 1, 0, 0, 'R');
	perform SetDevParamListGuide_Bunch (9, 1, 1, 3, 'I');
	perform SetDevParamListGuide_Bunch (9, 2, 1, 4); -- DI 1..4
	perform SetDevParamListGuide_Bunch (9, 4, 1, 4); -- CNT 1..4

	-- perform SetDevType 10, 'ТЭКОН 19-02'
	-- AI 1..4
	perform SetDevParamListGuide_Bunch (10, 1, 0, 0, 'R'); 
	perform SetDevParamListGuide_Bunch (10, 1, 1, 3, 'I'); 
	perform SetDevParamListGuide_Bunch (10, 2, 0, 3); -- DI
	perform SetDevParamListGuide_Bunch (10, 4, 0, 3); -- CNT

	-- perform SetDevType 11, 'ТЭКОН 19-03'
	perform SetDevParamListGuide_Bunch (11, 1, 1, 3, 'R'); -- AI 1..3
	perform SetDevParamListGuide_Bunch (11, 2, 0, 2); -- DI
	perform SetDevParamListGuide_Bunch (11, 4, 0, 2); -- CNT

	-- perform SetDevType 12, 'ТЭКОН 19-04'
	perform SetDevParamListGuide_Bunch (12, 2, 0, 7); -- DI
	perform SetDevParamListGuide_Bunch (12, 4, 0, 7); -- CNT

	-- perform SetDevType 13, 'ТЭКОН 19-05'
	-- AI 0..3
	perform SetDevParamListGuide_Bunch (13, 1, 0, 1, 'R'); 
	perform SetDevParamListGuide_Bunch (13, 1, 2, 3, 'I'); 
	perform SetDevParamListGuide_Bunch (13, 2, 0, 2); -- DI
	perform SetDevParamListGuide_Bunch (13, 4, 0, 2); -- CNT

	-- perform SetDevType 14, 'ТЭКОН 19-06'
	-- AI 0..6
	perform SetDevParamListGuide_Bunch (14, 1, 0, 3, 'R'); 
	perform SetDevParamListGuide_Bunch (14, 1, 4, 6, 'I'); 
	perform SetDevParamListGuide_Bunch (14, 2, 0, 3); -- DI
	perform SetDevParamListGuide_Bunch (14, 4, 0, 3); -- CNT

	-- perform SetDevType 15, 'ТЭКОН 19-07'
	perform SetDevParamListGuide_Bunch (15, 1, 0, 2, 'R'); -- AI
	perform SetDevParamListGuide_Bunch (15, 2, 0, 2); -- DI
	perform SetDevParamListGuide_Bunch (15, 4, 0, 2); -- CNT

	-- perform SetDevType 16, 'ТЭКОН 19-08'
	perform SetDevParamListGuide_Bunch (16, 1, 0, 1, 'R'); 
	perform SetDevParamListGuide_Bunch (16, 1, 2, 3, 'I'); 
	perform SetDevParamListGuide_Bunch (16, 2, 0, 2); -- DI
	perform SetDevParamListGuide_Bunch (16, 4, 0, 2); -- CNT

	-- perform SetDevType 17, 'ТЭКОН 19-09'
	perform SetDevParamListGuide_Bunch (17, 2, 0, 7); -- DI
	perform SetDevParamListGuide_Bunch (17, 4, 0, 7); -- CNT

	-- perform SetDevType 18, 'ТЭКОН 19-10'
	perform SetDevParamListGuide_Bunch (18, 1, 0, 3, 'R'); -- AI
	perform SetDevParamListGuide_Bunch (18, 2, 0, 6); -- DI
	perform SetDevParamListGuide_Bunch (18, 4, 0, 6); -- CNT

	-- perform SetDevType 19, 'ТЭКОН 19-11'
	perform SetDevParamListGuide_Bunch (19, 1, 0, 3, 'I'); -- AI

	-- perform SetDevType 20, 'ТЭКОН 19-12'
	perform SetDevParamListGuide_Bunch (20, 2, 0, 7); -- DI
	perform SetDevParamListGuide_Bunch (20, 4, 0, 7); -- CNT

	-- perform SetDevType 21, 'ТЭКОН 19-13'
	perform SetDevParamListGuide_Bunch (21, 1, 0, 4, 'R'); -- AI

	-- perform SetDevType 22, 'ТЭКОН 19-14'
	perform SetDevParamListGuide_Bunch (22, 1, 0, 9, 'I'); -- AI
end; $$;