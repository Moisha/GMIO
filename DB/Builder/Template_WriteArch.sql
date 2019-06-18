do $$ begin perform DropFunction('Write#Table#'); end $$;
create or replace function Write#Table#(
  _ID_Prm int,
  _UTime int,
  _Val float) 
returns void 
AS $$
begin
  if coalesce(_ID_Prm, 0) <= 0 or coalesce(_UTime, 0) <= 0 then
    return;
  end if;
  
  if not exists(select * from #Table# where ID_Prm = _ID_Prm and UTime = _UTime) then
    insert into #Table#(ID_Prm, UTime, Val) select _ID_Prm, _UTime, _Val;
  else
    update #Table# set Val = _Val where ID_Prm = _ID_Prm and UTime = _UTime;
  end if;
  
  begin
    insert into ParamStates(ID_Prm, Last#Table#) select _ID_Prm, _UTime;
  exception
    when others then
      update ParamStates set Last#Table# = _UTime where ID_Prm = _ID_Prm;
  end;  
end;
$$ language plpgsql;

grant execute on function Write#Table#(_ID_Prm int, _UTime int, _Val float) to "GM_Admin";