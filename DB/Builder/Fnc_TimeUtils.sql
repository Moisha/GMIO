
do $$ begin perform DropFunction('GetTimeZone'); end $$;
create or replace function GetTimeZone()
returns int
as $$
begin
  return extract(timezone from now()); 
end;
$$ language plpgsql;

grant execute on function GetTimeZone() to "GM_Admin";

do $$ begin perform DropFunction('UTCtoDT'); end $$;
create or replace function UTCtoDT(_utc bigint, _timezone int = null)
returns timestamp
as $$
declare _dt timestamp without time zone; _add interval; 
begin
  _add := cast (_utc + coalesce(_timezone * 3600, GetTimeZone()) as text) || ' seconds';
  _dt := '20000101'; 
  
  return _dt + _add;
end;
$$ language plpgsql;

grant execute on function UTCtoDT(_utc bigint, _timezone int) to "GM_Admin";

do $$ begin perform DropFunction('DTtoUTC'); end $$;
create or replace function DTtoUTC(_dt timestamp with time zone)
returns int
as $$
declare _cmd text;
begin
  _cmd = 'SET TIME ZONE ' || cast(GetTimeZone() / 3600 as text);-- это для нормального отсчета, чтобы 20000101 и _dt были в одном поясе
  execute _cmd;

  return round(extract (epoch from _dt - '20000101') - GetTimeZone());
end;
$$ language plpgsql;

grant execute on function DTtoUTC(_dt timestamp with time zone) to "GM_Admin";

do $$ begin perform DropFunction('NowGM'); end $$;
create or replace function NowGM()
returns bigint
as $$
begin
  return DTtoUTC(current_timestamp);
end; 
$$ language plpgsql;

grant execute on function NowGM() to "GM_Admin";