
do $$ begin perform DropFunction('SetDevParamListGuide'); end $$;
create or replace function SetDevParamListGuide(
   _ID_DevType int -- тип прибора
  ,_ID_PT int -- ID типа параметра
  ,_ID_Src int -- тип источника 1 - AI, 2 - DI, 3 - DO, 4 - CNT_DI, 5 - AO, 6 - CNT_AI
  ,_N_Src int -- номер в источнике
  ,_PName text = null -- название параметра
  ,_ExtData varchar (100) = null -- дополнительная информация
  ,_ID_MeaUnit int = null -- единицы измерения
  ,_CurrentsAddr int = null -- ячейка адреса текущих значений
  ,_CurrentsArgument int = null -- ячейка адреса текущих значений
  ,_HourArchAddr int = null -- ячейка адреса часовых архивов
  ,_HourArchArgument int = null -- доп. признак часовых архивов
  )
  returns void 
as $$
begin
  if not exists (select * from DevParamListGuide 
                  where ID_DevType = _ID_DevType
                        and ID_PT = _ID_PT
                        and ID_Src = _ID_Src
                        and N_Src = _N_Src
                  ) then
    insert into DevParamListGuide (ID_DevType, ID_PT, ID_Src, N_Src, PName, ExtData, ID_MeaUnit, CurrentsAddr, Argument, HourArchAddr, HourArchArgument)
      select _ID_DevType, _ID_PT, _ID_Src, _N_Src, _PName, _ExtData, _ID_MeaUnit, _CurrentsAddr, _CurrentsArgument, _HourArchAddr, _HourArchArgument;
  end if;
end;
$$ language plpgsql;

do $$ begin perform DropFunction('SetDevParamListGuideA'); end $$;
create or replace function SetDevParamListGuideA(
   _ID_DevType int -- тип прибора
  ,_Name_PT text  -- короткое имя типа параметра
  ,_Name_Src text -- тип источника 1 - AI, 2 - DI, 3 - DO, 4 - CNT_DI, 5 - AO, 6 - CNT_AI
  ,_N_Src int -- номер в источнике
  ,_PName text = null -- название параметра
  ,_ExtData text = null -- дополнительная информация
  ,_ID_MeaUnit int = null -- единицы измерения
  ,_CurrentsAddr int = null -- ячейка адреса текущих значений
  ,_CurrentsArgument int = null -- доп. признак текущих значений
  ,_HourArchAddr int = null -- ячейка адреса часовых архивов
  ,_HourArchAddrArgument int = null -- доп. признак часовых архивов
  )
  returns void 
as $$
declare _ID_Src int; _ID_PT int;
begin
  select ID_Src into _ID_Src from SrcTypes where SrcName = _Name_Src limit 1;
  select ID_PT into _ID_PT from ParamTypes where PSign = _Name_PT limit 1;

  perform SetDevParamListGuide( _ID_DevType, _ID_PT, _ID_Src, _N_Src, _PName, _ExtData, _ID_MeaUnit, _CurrentsAddr, _CurrentsArgument, _HourArchAddr, _HourArchAddrArgument);
end;
$$ language plpgsql;

do $$ begin perform DropFunction('SetDevParamListGuide_Bunch'); end $$;
create or replace function SetDevParamListGuide_Bunch(
   _ID_DevType int -- тип прибора
  ,_ID_Src int -- тип источника 1 - AI, 2 - DI, 3 - DO, 4 - CNT_DI, 5 - AO, 6 - CNT_AI
  ,_N_Src1 int -- номер первого канала в источнике
  ,_N_Src2 int -- номер последнего канала в источнике
  ,_ExtData varchar (100) = null -- дополнительная информация
  )
  returns void
as $$
declare _i int;
begin
  _i := _N_Src1;
  while _i <= _N_Src2 loop
    perform SetDevParamListGuide (_ID_DevType, null, _ID_Src, _i, null, _ExtData, null);
    _i := _i + 1;
  end loop;
end; 
$$ language plpgsql;

delete from DevParamListGuide;

do $$
begin
	-- Altistart 22
	perform SetDevParamListGuideA(29, 'I_PH1', 'AI', 1, 'Ток в фазе 1', null, 2);
	perform SetDevParamListGuideA(29, 'I_PH2', 'AI', 2, 'Ток в фазе 2', null, 2);
	perform SetDevParamListGuideA(29, 'I_PH3', 'AI', 3, 'Ток в фазе 3', null, 2);
	perform SetDevParamListGuideA(29, 'U_FEED', 'AI', 4, 'Напряжение', null, 2);		

	perform SetDevParamListGuideA(29, 'ALARM_UBZ', 'DI', 1, 'Последняя неисправность');

	-- Меркурий 230
	perform SetDevParamListGuideA(28, 'Ain', 'SRC_CNT_MTR', 1, 'A+', null, 8);
	perform SetDevParamListGuideA(28, 'Aout', 'SRC_CNT_MTR', 2, 'A-', null, 8);
	perform SetDevParamListGuideA(28, 'Qin', 'SRC_CNT_MTR', 3, 'Q+', null, 23);
	perform SetDevParamListGuideA(28, 'Qout', 'SRC_CNT_MTR', 4, 'Q-', null, 23);

	--ICP7041D
	perform SetDevParamListGuide_Bunch (2, 2, 0, 13); -- DI 0..13
	perform SetDevParamListGuide_Bunch (2, 4, 0, 13); --  CNT 0..13

	--ICP7017
	perform SetDevParamListGuide_Bunch (4, 1, 0, 7); -- AI 0..7

	-- УБЗ
	perform SetDevParamListGuide (3, 20, 2, 1); -- 'Авария УБЗ', 'ALARM_UBZ'
	perform SetDevParamListGuide (3, 21, 1, 1); -- 'Напряжение по фазе 1', 'U_PH1'
	perform SetDevParamListGuide (3, 22, 1, 2); -- 'Напряжение по фазе 2', 'U_PH2'
	perform SetDevParamListGuide (3, 23, 1, 3); -- 'Напряжение по фазе 3', 'U_PH3'
	perform SetDevParamListGuide (3, 24, 1, 4); -- 'Ток по фазе 1', 'I_PH1'
	perform SetDevParamListGuide (3, 25, 1, 5); -- 'Ток по фазе 2', 'I_PH2'
	perform SetDevParamListGuide (3, 26, 1, 6); -- 'Ток по фазе 3', 'I_PH3'
	perform SetDevParamListGuide (3, 28, 3, 1); -- 'Управление', 'START_UBZ'
	perform SetDevParamListGuide (3, 40, 1, 7); -- 'Наработка двигателя', 'UBZ_ENG_HOURS'
	perform SetDevParamListGuide (3, 41, 1, 8); -- 'Мощность', 'P'
	perform SetDevParamListGuide (3, 44, 1, 9); -- 'Ток нулевой последовательности', 'P'

	-- ТР-101
	perform SetDevParamListGuide (5, 37, 2, 1); -- 'Авария ТР', 'ALARM_TR'
	perform SetDevParamListGuide (5, 33, 1, 1, 'Температура 1'); -- 'Температура 1', 'T1'
	perform SetDevParamListGuide (5, 33, 1, 2, 'Температура 2'); -- 'Температура 2', 'T2'
	perform SetDevParamListGuide (5, 33, 1, 3, 'Температура 3'); -- 'Температура 3', 'T3'
	perform SetDevParamListGuide (5, 33, 1, 4, 'Температура 4'); -- 'Температура 4', 'T4'
	perform SetDevParamListGuide (5, 42, 5, 1, 'Уставка T1'); -- 'Уставка T1', 'PresetT1'
	perform SetDevParamListGuide (5, 42, 5, 2, 'Уставка T2'); -- 'Уставка T2', 'PresetT1'
	perform SetDevParamListGuide (5, 42, 5, 3, 'Уставка T3'); -- 'Уставка T3', 'PresetT1'
	perform SetDevParamListGuide (5, 42, 5, 4, 'Уставка T4' ); -- 'Уставка T4', 'PresetT1'

	-- ТРМ-138
	perform SetDevParamListGuide_Bunch (6, 1, 1, 8); -- AI 0..3

	-- Взлет УРСВ
	perform SetDevParamListGuide (7, 14, 1, 1, 'Расход 1'); 
	perform SetDevParamListGuide (7, 14, 1, 2, 'Расход 2'); 
	perform SetDevParamListGuide (7, 14, 1, 3, 'Расход 3');
	perform SetDevParamListGuide (7, 14, 1, 4, 'Расход 4'); 

	-- Vacon
	perform SetDevParamListGuide (8,  2, 1, 1, 'Ток');
	perform SetDevParamListGuide (8, 45, 1, 2 ); -- скорость
	perform SetDevParamListGuide (8, 41, 1, 3 ); -- мощность %
	perform SetDevParamListGuide (8, 11, 2, 1, 'Неисправность');
	perform SetDevParamListGuide (8, 42, 5, 1, 'Уставка скорости двигателя');
	perform SetDevParamListGuide (8, 46, 5, 2); -- PID Reference
	perform SetDevParamListGuide (8, 28, 3, 1); -- 'Управление'

	-- perform SetDevType 23, 'Isco 4250'
	perform SetDevParamListGuide (23, 14, 1, 1); -- расход
	perform SetDevParamListGuide (23, 1, 1, 2); -- уровень
	perform SetDevParamListGuide (23, 48, 1, 3); -- скорость
	perform SetDevParamListGuide (23, 47, 4, 1); -- объем

	-- Симаг 11
	perform SetDevParamListGuideA(25, 'V', 'SRC_CNT_MTR', 1, 'V+', null, 14);
	perform SetDevParamListGuideA(25, 'V', 'SRC_CNT_MTR', 2, 'V-', null, 14);
	perform SetDevParamListGuideA(25, 'FLOW', 'AI', 1, 'Расход', null, 5);	

	-- Питерфлоу РС
	perform SetDevParamListGuideA(27, 'FLOW', 'AI', 1, 'Расход', null, 5);
	perform SetDevParamListGuideA(27, 'V', 'SRC_CNT_MTR', 1, 'V+', null, 14);
	perform SetDevParamListGuideA(27, 'V', 'SRC_CNT_MTR', 2, 'V-', null, 14);
	perform SetDevParamListGuideA(27, 'UBZ_ENG_HOURS', 'SRC_CNT_MTR', 3, 'Время наработки (мин.)');	
end; $$;
