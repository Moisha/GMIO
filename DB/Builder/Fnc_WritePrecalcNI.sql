do $$ begin perform DropFunction('WritePrecalcNI'); end $$;
create or replace function WritePrecalcNI(_ID_Prm int, _UTime bigint, _Val float)
returns void
as $$
  declare _d bigint; _d2 bigint; _v float; _v1 float; _v2 float; _ID_Src int; _N_Src int; _ID_Device int; _t1 bigint; _t2 bigint; _IntrvCnt int;
          _DevMin float; _DevMax float; _RealMin float; _RealMax float; _UTime1 bigint; _Val_NI float; _UTime1_Precalc bigint; _Val_NI_Precalc float;
          _Intrv int; _NI_DT bigint; _CounterCeiling int; _MeterCalcNIType int; _res Vals; _ID_PrmMain int;
begin
  if exists(select * from PrecalcNIs where ID_Prm = _ID_Prm and UTime = _UTime) then
    update PrecalcNIs set Val = _Val where ID_Prm = _ID_Prm and UTime = _UTime;
  else
    insert into PrecalcNIs (ID_Prm, UTime, Val) select _ID_Prm, _UTime, _Val;
  end if;
end;
$$ language plpgsql;

grant execute on function WritePrecalcNI(_ID_Prm int, _UTime bigint, _Val float) to "GM_Admin";