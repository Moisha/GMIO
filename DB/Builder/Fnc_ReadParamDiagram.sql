do $$ begin perform DropFunction('ReadParamDiagram'); end $$;
create or replace function ReadParamDiagram(
  _ID_Prm int,
  _UTime1 bigint,
  _UTime2 bigint,
  _Intrv int,
  _DiagramType int) -- 1 - принудительный интеграл, 2 - сумма за весь период
-- значения на начало интервала
returns table (Utime bigint, Val float)
as $$
declare _ID_MainPrm int; _v float; _res float; _v1 float; _v2 float; _src int; 
        _t01 bigint; _t02 bigint; _t0base bigint; _t1 bigint; _t2 bigint; _IntrvCnt int; _coeff float;
        _DevMin float; _DevMax float; _RealMin float; _RealMax float; _val Vals; _counterCeiling int;
        _ReqIntervalType int; _MinIntegralInterval int; _id_src int; _ni_startVal float; _ni_startUTime bigint;
begin
  select ID_Src into _id_src from Params where ID_Prm = _ID_Prm;

  if _id_src = 6 then -- SRC_SUM_AI
    _ID_MainPrm := GetMainAIParamForSummParam(_ID_Prm);
    return query select rpd.UTime, rpd.Val from ReadParamDiagram(_ID_MainPrm, _UTime1, _UTime2, _Intrv, 1) rpd;
    return;
  end if;
  
  if _id_src = 9 then -- SRC_CNT_MTR
    if coalesce((select BaseChn from Params where ID_Prm = _ID_Prm), 0) > 0 then
      select cn.Val, cn.UTime into _ni_startVal, _ni_startUTime from CalcNI(_ID_Prm, _UTime1) cn;
      if _ni_startVal is not null then
         _UTime1 := case when _ni_startUTime > _UTime1 then _ni_startUTime else _UTime1 end;
        _ID_MainPrm := GetMainAIParamForSummParam(_ID_Prm);
        return query select rid.Utime, rid.Val 
          from ReadIntegralDiagram(_ID_MainPrm, _UTime1, _UTime2, _Intrv, _ni_startVal) rid;
      end if;
      return;	  
    end if;
  end if;
  
  if (coalesce(_ID_Prm, 0) <= 0) or (coalesce(_Intrv, 0) < 0)
     or (coalesce(_UTime1, 0) <= 0) or (coalesce(_UTime2, 0) <= 0) then
      return;
  end if; 
  
  select DevMin, DevMax, RealMin, RealMax, ID_Src, CounterCeiling, case ID_Src when 9 then 1 else ReqIntervalType end, coalesce(MinIntegralInterval, 0)
    into _DevMin, _DevMax, _RealMin, _RealMax, _src, _counterCeiling, _ReqIntervalType, _MinIntegralInterval
     from Params where ID_Prm = _ID_Prm;

  if coalesce(_CounterCeiling, 0) <= 0 then
    _counterCeiling := GetCounterCeiling(_ID_Prm);
  end if;
  
  if _Intrv <= 1 and _MinIntegralInterval > _Intrv then
    _Intrv := _MinIntegralInterval;
  end if;
  
  if _Intrv > 0 then
    _UTime1 := _Utime1 / (_Intrv * 60) * _Intrv * 60;
    _UTime2 := _UTime2 / (_Intrv * 60) * _Intrv * 60;
  end if;
 
  if _UTime1 >= _UTime2 then
    return;
  end if; 
 
  -- если не хватает параметров калибровки или они косячные, то калибровать не будем 
  if _DevMin is null or _DevMax is null or _RealMin is null or _RealMax is null or Abs(_DevMin - _DevMax) < 1e-7 then
    _DevMin := 0; _DevMax := 1; _RealMin := 0; _RealMax := 1;
  end if;

  _coeff := (_RealMax - _RealMin) / (_DevMax - _DevMin);
    
  if coalesce(_Intrv, 0) = 0 then-- это у нас события
    -- _UTime1 - 60 взяли, чтобы проинициализировать архив на тот случай, 
    -- когда первое же значение за указанный период сразу событие 
    _v1 := null;
    for _val in (select * from Vals where ID_Prm = _ID_Prm and Vals.UTime >= _UTime1 - 60 and Vals.UTime < _UTime2 order by Vals.UTime) 
    loop
      if _v1 is not null and _val.Val <> _v1 then
        return query select _val.UTime, _val.Val;
      end if;

      _v1 := _val.Val;
    end loop;
  else
    if _DiagramType = 1 then
      return query select rid.Utime, rid.Val from ReadIntegralDiagram(_ID_Prm, _UTime1, _UTime2, _Intrv) rid;
    elsif _DiagramType = 2 then
      return query select rid.Utime, rid.Val from ReadIntegralDiagram(_ID_Prm, _UTime1, _UTime2, 0) rid;
    else
      if coalesce(_ReqIntervalType, 0) = 1
      then -- спецканал с накоплением, берем максимум
        return query(
         select cast(_UTime1 + floor((Vals.Utime - _UTime1) / (_Intrv * 60)) * (_Intrv * 60) as bigint) as startTime, 
                max(_RealMin + (Vals.Val - _DevMin) * _coeff ) 
           from Vals where ID_Prm = _ID_Prm and Vals.UTime >= _UTime1 and Vals.UTime < _UTime2
           group by ID_Prm, startTime 
           order by startTime);
      elsif _src not in (4, 5) -- аналоговый канал, берем среднее
      then
        return query(
          select cast(_UTime1 + floor((Vals.Utime - _UTime1) / (_Intrv * 60)) * (_Intrv * 60) as bigint) as startTime, 
                 avg(_RealMin + (Vals.Val - _DevMin) * _coeff ) 
            from Vals where ID_Prm = _ID_Prm and Vals.UTime >= _UTime1 and Vals.UTime < _UTime2
            group by ID_Prm, startTime 
            order by startTime);
      else
        -- счетчики, всегда усредняем минимум за пять минут
        _IntrvCnt := _Intrv; --case when _Intrv < 5 then 5 else _Intrv end;
        
        for _val in (select * from Vals where ID_Prm = _ID_Prm and Vals.UTime >= _UTime1 - _IntrvCnt * 60 and Vals.UTime < _UTime2 order by Vals.UTime) 
        loop
          if _t1 is null then -- если не было первого значения, то инициализируем его и едем дальше
            _t1 := _val.UTime;
            _t0base := _t1;
            _v1 := _val.Val;
            _t2 := null; _v2 := null; _v := null; _res := null;
            continue;
          end if;	  

          _t2 := _val.UTime;
          _v2 := _val.Val;

          --raise notice 't1 = %     t2 = %   v1 = %   v2 = %', utctodt(_t1), utctodt(_t2), _v1, _v2 ;

          -- посмотрим, где у нас значения
          _t01 := _UTime1 + floor( (_t1 - _UTime1) / (_IntrvCnt * 60) ) * (_IntrvCnt * 60); -- результирующий интервал для _t1
          _t02 := _UTime1 + floor( (_t2 - _UTime1) / (_IntrvCnt * 60) ) * (_IntrvCnt * 60); -- результирующий интервал для _t2

          if _t01 = _t02 then -- идем по датам в одном результирующем интервале, просто считаем _res
            if _v2 >= _v1 then
              _v := (_v2 - _v1) ; -- нормальный прирост
            else
              _v := (_v2 + (case when _counterCeiling > _v1 then _counterCeiling else _v1 end - _v1)); -- переход через 0. Если _counterCeiling введен неправильно, то он игнорируется.
            end if;

            --raise notice 'adding t1 %,   res %,   _v %   _v1 %     _v2 %', utctodt(_t1), _res, _v, _v1, _v2;

           _res := coalesce(_res, 0) + _v;
          else
            --raise notice 'saving %   %  delta %    res %    t1 %   base %', utctodt(_t01), _res, _t1 - _t0base, _RealMin + (_res / (_t1 - _t0base) - _DevMin) / (_DevMax - _DevMin) * (_RealMax - _RealMin), utctodt(_t1), utctodt(_t0base);

            if _res is not null and _t01 >= _UTime1 then -- начался следующий интервал, запишем чего мы там насчитали в предыдущем c переводом в реальные единицы
              return query select  _t01, _RealMin + (_res / (_t1 - _t0base) - _DevMin) * _coeff;
            end if;

            if _t02 - _t01 = _IntrvCnt * 60 then -- аккуратно закончили интервал и сразу начали следующий
              -- посчитаем начальный результат для интервла _t02
              if _v2 >= _v1 then
                _res := (_v2 - _v1) ; -- нормальный прирост
              else 
                _res := (_v2 + (case when _counterCeiling > _v1 then _counterCeiling else _v1 end - _v1)); -- переход через 0. Если _counterCeiling введен неправильно, то он игнорируется.
              end if;

              _t0base := _t1;
            else
              _res := null;
              _t0base := _t2;
            end if;

            --raise notice 'after save res = %', _res;
           
          end if;
          
          -- запомним новую опорную точку
          _t1 := _t2;
          _v1 := _v2;
        end loop;

        if _res is not null then -- Vals закончились, возможно остался результат в последнем инетрвале
          _t01 := floor( (_t1) / (_IntrvCnt * 60) ) * (_IntrvCnt * 60); -- результирующий интервал для _t1
          return query select _t01, _RealMin + (_res / (_t1 - _t0base) - _DevMin) * _coeff;
        end if;
      end if;
    end if;
  end if;  
end;
$$ language plpgsql;

grant execute on function ReadParamDiagram(
  _ID_Prm int,
  _UTime1 bigint,
  _UTime2 bigint,
  _Intrv int,
  _DiagramType int) to "GM_Admin";