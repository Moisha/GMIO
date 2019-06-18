do $$ begin perform DropFunction('RemoteData_#Table#'); end $$;
create or replace function RemoteData_#Table#(
  _ID_Prm int,
  _UTime bigint) 
returns table (Utime bigint, Val float)
as $$
begin
  return query 
    select tbl.UTime, tbl.Val from #Table# tbl
      where tbl.ID_Prm = _ID_Prm and tbl.UTime > _UTime 
        order by tbl.UTime
		  limit 3000; 
end;
$$ language plpgsql;

grant execute on function RemoteData_#Table#(_ID_Prm int, _UTime bigint) to "GM_Admin";