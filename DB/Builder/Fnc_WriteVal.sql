do $$ begin perform DropFunction('WriteVal'); end $$;
create or replace function WriteVal(
  _ID_Prm int,
  _UTime bigint,
  _Val float) 
returns void 
AS $$
begin
  if coalesce(_ID_Prm, 0) <= 0 or coalesce(_UTime, 0) <= 0 then
    return;
  end if;

  begin
    insert into Vals(ID_Prm, UTime, Val) select _ID_Prm, _UTime, _Val;
  exception
     when others then
  end;

  begin
    insert into CurrVals(ID_Prm, UTime, Val) select _ID_Prm, _UTime, _Val;
  exception
    when others then
      update CurrVals set UTime = _UTime, Val = _Val where ID_Prm = _ID_Prm and UTime < _UTime;
  end;  
  
  begin
    insert into ParamStates(ID_Prm, LastCurrent) select _ID_Prm, _UTime;
  exception
    when others then
      update ParamStates set LastCurrent = _UTime where ID_Prm = _ID_Prm and LastCurrent < _UTime;
  end;
  
end;
$$ language plpgsql;

grant execute on function WriteVal(_ID_Prm int, _UTime bigint, _Val float) to "GM_Admin";