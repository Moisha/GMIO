do $$ begin perform DropFunction('#Table#RequestStartDate#Appendix#'); end $$;
create or replace function #Table#RequestStartDate#Appendix#(#Identifier# int, _utStart bigint = null, _step interval = '1 #IntrvLen#')
returns table(UTime bigint)
as $$
declare _dt1 timestamp; _dt2 timestamp; _utime bigint; _interval interval;
begin
  create temp table emptydates (utime bigint) on commit drop;

  _dt1 := UTCtoDT(_utStart);
  if _dt1 is null then
    _interval := '20 days';
    _dt1 = date_trunc('day', now()) - _interval;
  end if;
  _dt2 := date_trunc('#IntrvLen#', now());

  while _dt1 < _dt2
  loop
    insert into emptydates select DTtoUTC(_dt1);
    _dt1 := _dt1 + _step;
  end loop;
  
  delete from emptydates 
    where exists(select * from #Table# where #Condition# and #Table#.UTime = emptydates.utime);
  
  return query select emptydates.UTime from emptydates;
end;
$$ language plpgsql;

grant execute on function #Table#RequestStartDate#Appendix#(int, bigint, interval) to "GM_Admin";
