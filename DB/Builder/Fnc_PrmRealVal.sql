do $$ begin perform DropFunction('PrmRealVal'); end $$;
create or replace function PrmRealVal(_ID_Prm int, _UTime bigint, _Val float)
returns float
as $$
declare _v float; _DevMin float; _DevMax float; _RealMin float; _RealMax float;
begin
  _v := _Val;
  if _v is null then
    select Val into _v from Vals where ID_Prm = _ID_Prm and UTime = _UTime;
  end if;
  
  if _v is not null then
    select DevMin, DevMax, RealMin, RealMax into _DevMin, _DevMax, _RealMin, _RealMax
     from Params where ID_Prm = _ID_Prm;
   
    -- если не хватает параметров калибровки или они косячные, то калибровать не будем 
    if _DevMin is null or _DevMax is null or _RealMin is null or _RealMax is null or Abs(_DevMin - _DevMax) < 1e-7 then
      _DevMin := 0;
      _DevMax  := 1;
      _RealMin := 0;
      _RealMax := 1;
    end if;
      
    _v := _RealMin + (_v - _DevMin) / (_DevMax - _DevMin) * (_RealMax - _RealMin);
  end if;
  
  return _v;
end;
$$ language plpgsql;

grant execute on function PrmRealVal(_ID_Prm int, _UTime bigint, _Val float) to "GM_Admin";