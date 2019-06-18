do $$ begin perform DropFunction('CalcNI'); end $$;
create or replace function CalcNI(_ID_Prm int, _DT2 bigint)
returns Vals
as $$
  declare _d bigint; _d2 bigint; _v float; _v1 float; _v2 float; _ID_Src int; _N_Src int; _ID_Device int; _t1 bigint; _t2 bigint; _IntrvCnt int;
          _DevMin float; _DevMax float; _RealMin float; _RealMax float; _UTime1 bigint; _Val_NI float; _UTime1_Precalc bigint; _Val_NI_Precalc float;
          _Intrv int; _NI_DT bigint; _CounterCeiling int; _MeterCalcNIType int; _res Vals; _ID_PrmMain int; _BaseChn int; _defaultCaliber int;
begin
  _res.UTime := 0;
  _res.ID_Prm := _ID_Prm;
  _res.Val := 0;

  if coalesce(_ID_Prm, 0) = 0 then
    return _res;
  end if;

  if coalesce(_DT2, 0) = 0 then
    _DT2 := NowGM();
  end if;  
  
  select ID_Src, NI_StartDT, NI_StartVal, MeterCalcNIType, BaseChn, DevMin, DevMax, RealMin, RealMax, ID_Src, N_Src, ID_Device, CounterCeiling 
		 into _ID_Src, _UTime1, _Val_NI, _MeterCalcNIType, _BaseChn, _DevMin, _DevMax, _RealMin, _RealMax, _ID_Src, _N_Src, _ID_Device, _CounterCeiling
    from Params where ID_Prm = _ID_Prm; -- показания берем из переданного канала, это может быть счетчик

  _defaultCaliber := 0;
  -- если не хватает параметров калибровки или они косячные, то калибровать не будем 
  if _DevMin is null or _DevMax is null or _RealMin is null or _RealMax is null or Abs(_DevMin - _DevMax) < 1e-7 then
    _DevMin := 0;
    _DevMax := 1;
    _RealMin := 0;
    _RealMax := 3600; -- это 1 импульс на реальную единицу
    _defaultCaliber := 1;
  end if;   

  if (_ID_Src = 9 and coalesce(_BaseChn, 0) = 0) -- это отдельно стоящие счетчики. Родительского канала не имеют, в них сразу лежат показания
     or coalesce(_MeterCalcNIType, 0) = 1  then -- это каналы других типов, но тоже счетчики
    select UTime, Val into _res.UTime, _res.Val 
      from Vals where ID_Prm = _ID_Prm and UTime <= _DT2 order by UTime desc limit 1;
    
    if _defaultCaliber = 0 then
      _res.Val := _RealMin + (_res.Val - _DevMin)  * (_RealMax - _RealMin) / (_DevMax - _DevMin);
    end if;
      
    return _res;
  end if;
    
  if _Val_NI is null or coalesce(_UTime1, 0) = 0 then
    return _res;
  end if;

  select UTime, Val into _UTime1_Precalc, _Val_NI_Precalc -- проверим что там у нас в буфере
    from PrecalcNIs where ID_Prm = _ID_Prm
                          and UTime <= _DT2 
    order by UTime desc
    limit 1;

  if coalesce(_UTime1_Precalc, 0) > _UTime1 then
    _UTime1 := _UTime1_Precalc;
    _Val_NI := _Val_NI_Precalc;
  end if;
  
  if coalesce(_CounterCeiling, 0) <= 0 then
    _CounterCeiling := GetCounterCeiling(_ID_Prm);
  end if;

  _Intrv := 20; -- считаем по 20 минут, чтобы не было переполнения на яростно мотающих счетчиках

  if _ID_Src in (6, 9) then -- SRC_SUM_AI, SRC_MTR
    _ID_PrmMain := GetMainAIParamForSummParam(_ID_Prm);
    select rid.UTime, rid.Val into _NI_DT, _v from ReadIntegralDiagram (_ID_PrmMain, _UTime1, _DT2, 0, _Val_NI) rid limit 1;
    if _NI_DT is not null then
      _Val_NI := _v;
    else
      _NI_DT := _UTime1;
    end if;
  else -- SRC_CNT_DI
    _d := _UTime1;
    _NI_DT := _UTime1;

    while _d < _DT2 loop
      _d2 := _d + _Intrv * 60; _v := null; _v1 := null; _v2 := null; _t1 := null; _t2 := null;
    
      select Val, UTime into _v1, _t1 from Vals where ID_Prm = _ID_Prm and UTime <= _d order by UTime desc limit 1;
      select Val, UTime into _v2, _t2 from Vals where ID_Prm = _ID_Prm and UTime <= _d2  order by UTime desc limit 1;
    
      if _v1 is not null and _v1 is not null and _t1 > 0 and _t2 > 0 and _t1 < _t2 then
        if _v2 >= _v1 then
          _v := _v2 - _v1; -- нормальный прирост
        else
          _v := (_v2 + (case when _counterCeiling > _v1 then _counterCeiling else _v1 end - _v1)); -- переход через 0. Если _counterCeiling введен неправильно, то он игнорируется.
        end if;
  
        -- _v - это импульсы, натикавшие за _Intrv
        -- перевод сразу в реальные единицы, плюс делим на 3600, потому что у нас _RealMax заведомо умножен на 3600
        _v := _v * (_RealMax - _RealMin) / (_DevMax - _DevMin) / 3600;
        _Val_NI := _Val_NI + _v;
        _NI_DT := _t2;
      end if;
  
      _d := _d2;
    end loop;
  end if;

  _res.Val = _Val_NI;
  _res.Utime = _NI_DT;

  if _res.UTime > 0 and (coalesce(_UTime1_Precalc, 0) = 0 or (_DT2 - _UTime1 > 30 * 24 * 3600)) then -- разлет не больше месяца
    perform WritePrecalcNI(_ID_Prm, _res.Utime, _res.Val);
  end if;
  
  return _res;
end;
$$ language plpgsql;

grant execute on function CalcNI(_ID_Prm int, _DT2 bigint) to "GM_Admin";