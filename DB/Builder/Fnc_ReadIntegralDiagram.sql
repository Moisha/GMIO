do $$ begin perform DropFunction('ReadIntegralDiagram'); end $$;
create or replace function ReadIntegralDiagram(
  _ID_Prm int,
  _UTime1 bigint,
  _UTime2 bigint,
  _Intrv int, -- интервал интегрирования. Если 0, то вернем интеграл за весь диапазон
  _StartVal float default null) 
returns table (Utime bigint, Val float)
as $$
declare _d1 bigint; _d2 bigint; _t1 bigint; _t2 bigint; _v1 float; _v2 float; _summ float; _v float;
        _DevMin float; _DevMax float; _RealMin float; _RealMax float; _val Vals; _total float; _maxHole int;
begin
  select DevMin, DevMax, RealMin, RealMax into _DevMin, _DevMax, _RealMin, _RealMax
   from Params where Params.ID_Prm = _ID_Prm;

  begin
    _maxHole := coalesce((select PrmValue from SysConfig where PrmName = 'CurrentsMaxHoleMinutes'), '5')::int;
    _maxHole := case when _maxHole < 5 then 5 else _maxHole end;
  exception
    when others then
      _maxHole := 5;
  end;

  -- если не хватает параметров калибровки или они косячные, то калибровать не будем 
  if _DevMin is null or _DevMax is null or _RealMin is null or _RealMax is null or Abs(_DevMin - _DevMax) < 1e-7 then
    _DevMin := 0;
    _DevMax := 1; 
    _RealMin := 0;
    _RealMax := 1;
  end if;

  _d1 := _UTime1;
  _d2 := case when _Intrv > 0 then _d1 + _Intrv * 60 else _UTime2 end;

  _total := _StartVal;
  
  while _d2 <= _UTime2 loop
    _t1 := null; _t2 := null; _v1 := null; _v2 = null; _summ = 0;

    for _val in (select * from Vals
                   where ID_Prm = _ID_Prm and Vals.UTime >= _d1 - 60 and Vals.UTime < _d2
                     order by Vals.UTime)
    loop
      if _t1 is null then 
        -- это первое значение на интервале
        -- если оно совпадает с началом интервала, то считаем от него
        if _val.UTime = _d1 then
          _v1 := _val.Val;
          _t1 := _val.UTime;
          continue;
        end if;

        -- если нет, то ищем передыдущее значение
        select Vals.UTime, Vals.Val into _t1, _v1 
          from Vals 
            where Vals.ID_Prm = _ID_Prm and Vals.UTime < _val.UTime 
              order by Vals.UTime desc limit 1;

        -- если не нашли предыдущего значения или оно было слишкром давно, то считаем от первого значения на текущем интервале
        if _t1 is null or (_val.UTime - _t1 > _maxHole * 60) then
          _v1 := _val.Val;
          _t1 := _val.UTime;
          continue;
        else
        -- значение нашлось и оно былонедавно, считаем, что оно и было на начало интервала
          _t1 = _d1;
        end if;
      end if;

      _v2 := _val.Val;
      _t2 := _val.UTime;

      if _t2 > _d1 then -- если заехали  в отрезок, то интегрируем, если попали строго на границу или раньше, то предыдущее значение отбросим и крутим цикл дальше
        if _t1 < _d1 then
          _t1 := _d1; -- интегрируем "прямоугольниками", поэтому значение оставляем, отрезок укорачиваем
        end if;
      
        _v1 := _RealMin + (_v1 - _DevMin) / (_DevMax - _DevMin) * (_RealMax - _RealMin);
        -- предполагаем, что значения - это расход в м3/ч, умножим его на долю часа, прошедшую между отсечками
        if _t2 - _t1 < _maxHole * 60 then -- даем максимальную интегрируемую разбежку в 5 минут
          _summ := _summ + _v1 * (_t2 - _t1) / 3600.0;
        else
          _summ := _summ + _v1 * 60 / 3600.0; -- вылетели за разбежку, интегрируем за минуту
        end if;
      end if;
      
      _t1 := _t2;
      _v1 := _v2;
      _t2 := null;
      _v2 := null;
    end loop;
    
    if _t1 is not null then -- что-то было и оно недоинтегрировано до верхней границы
      _v1 := _RealMin + (_v1 - _DevMin) / (_DevMax - _DevMin) * (_RealMax - _RealMin);
      
      if _d2 - _t1 < _maxHole * 60 then -- даем максимальную интегрируемую разбежку в 5 минут
        _summ := _summ + _v1 * (_d2 - _t1) / 3600.0;
      else
        _summ := _summ + _v1 * 60 / 3600.0; -- вылетели за разбежку, интегрируем за минуту
      end if;

      if _total is not null then
        _total := _total + _summ;
      end if;

      if _Intrv = 0 then
        return query select _t1, coalesce(_total, _summ);
      else
        return query select _d1, coalesce(_total, _summ);
      end if;
    end if;

    if _Intrv = 0 then
      exit;
    else
      _d1 := _d1 + _Intrv * 60;
      _d2 := _d2 + _Intrv * 60;
    end if;
    
  end loop;
end;
$$ language plpgsql;

grant execute on function ReadIntegralDiagram(
  _ID_Prm int,
  _UTime1 bigint,
  _UTime2 bigint,
  _Intrv int,
  _StartVal float) to "GM_Admin";
  