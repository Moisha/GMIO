do $$ begin perform DropFunction('ReadSpecialDaySummIntegral'); end $$;
create or replace function ReadSpecialDaySummIntegral(
  _ID_Prm int,
  _UTime1 bigint,
  _UTime2 bigint) 
  -- Возвращает сумму за день
  -- Если канал ТЭКОНа, то это самое большое число за день
  -- Если DI_CNT, то сумма импульсов
  -- Если AI, то среднее
  -- Если MTR, то показания на конец интервала
  -- Для других типов каналов пока не работает
returns float
as $$
declare _type int; _res float; _dtg int; _ReqIntervalType int;
begin
  _res := null;
  select ID_Src, coalesce(ReqIntervalType, 0) into _type, _ReqIntervalType from Params where ID_Prm = _ID_Prm;

  if _type = 9 then -- DI_CNT_MTR
    select Val into _res from CalcNI (_ID_Prm, _UTime2);

  elsif _type = 7 then -- USR, проверим, не ТЭКОН ли
    select ID_DevTypeGroup into _dtg
      from Params p join Devices d on p.ID_Device = d.ID_Device
                    join DevTypes dt on dt.ID_DevType = d.ID_DevType 
        where ID_Prm = _ID_Prm
      limit 1;

    if _dtg = 1 then
      if _ReqIntervalType = 1 then -- это накопление на интервале
        select Max(Val) into _res from Vals where ID_Prm = _ID_Prm and UTime >= _UTime1 and UTime < _UTime2;
      else
        _type := 1; -- как обычный AI
      end if;
    end if;
  end if;

  if _type = 4 then -- DI_CNT
    select Sum(rpd.Val) into _res from ReadParamDiagram(_ID_Prm, _UTime1, _UTime2, 60, 0) rpd;

  elsif _type = 1 then -- AI
    select Avg(Val) into _res from Vals where ID_Prm = _ID_Prm and UTime >= _UTime1 and UTime < _UTime2;
    
  end if;
  
  return _res;
end;
$$ language plpgsql;

grant execute on function ReadSpecialDaySummIntegral(
  _ID_Prm int,
  _UTime1 bigint,
  _UTime2 bigint) to "GM_Admin";
