do $$ begin perform DropFunction('SimulateVals'); end $$;
create or replace function SimulateVals(
  _id_prm int, 
  _dt1 timestamp, 
  _dt2 timestamp) 
returns void 
as $$
declare _d bigint; _d1 bigint; _d2 bigint; _n int;
begin
  _d1 := DTtoUTC(_dt1);
  _d2 := DTtoUTC(_dt2);
  _n := 0;

  delete from Vals where ID_Prm = _id and UTime >= _d1 and UTime <= _d2;

  _d := _d1;
  while _d <= _d2
  loop
    perform WriteVal(_id_prm, _d, _n);
    _d := _d + 60;
    _n := _n + 1;
  end loop; 
end;
$$ language plpgsql;

grant execute on function SimulateVals(_id_prm int, _dt1 timestamp, _dt2 timestamp) to "GM_Admin";