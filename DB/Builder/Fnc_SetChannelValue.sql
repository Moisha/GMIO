
do $$ begin perform DropFunction('SetChannelValue'); end $$;
create or replace function SetChannelValue(
	_ID_Prm int,  
	_Value float,	
	_UserName text = null,
	_TimeHold int = null,
	_ID_Cmd_On_Remote_Host int = null) 
  returns int
as $$
declare 
  _res int;
begin
  update Commands 
    set State = 3 -- отмена
	where ID_Prm = _ID_Prm and State is null; 
  
  if coalesce(_ID_Cmd_On_Remote_Host, 0) > 0 then
    select ID_Cmd into _res 
	  from Commands 
	  where ID_Cmd_On_Remote_Host = _ID_Cmd_On_Remote_Host limit 1;
  end if;

  if _res is null then
    insert into Commands(ID_Prm, UTime, Value, TimeHold, UserName, ID_Cmd_On_Remote_Host)
      select _ID_Prm, NowGM(), _Value, _TimeHold, _UserName, _ID_Cmd_On_Remote_Host
	    returning ID_Cmd into _res; 
  end if;
  
  return _res;
end;
$$ language plpgsql;

grant execute on function SetChannelValue(_ID_Prm int, _Value float, _UserName text, _TimeHold int, _ID_Cmd_On_Remote_Host int) to "GM_Admin";
