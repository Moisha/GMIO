do $$ begin perform DropFunction('CanAddUserToGroup'); end $$;
create or replace function CanAddUserToGroup(
  _ID_User int, 
  _ID_Group int
  ) 
returns int 
AS $$
declare _cnt int; _level int; _res int;
begin 
  if _ID_User = _ID_Group then
    return 0;
  end if;
	
  create temp table _UserGroupBuf(id_user int, level int) on commit drop;
  _level := 0;
  insert into _UserGroupBuf select _ID_Group, _level;  
  loop
    insert into _UserGroupBuf 
	  select ID_Group, _level + 1 from UserGroups 
	    where ID_User in (select id_user from _UserGroupBuf where level = _level);
	  
    get diagnostics _cnt = row_count;
	if _cnt = 0 then
	  _res := 1;
	  exit;
	end if;
	
	if exists(select id_user from _UserGroupBuf group by id_user having count(*) > 1) -- зацикливание групп
	   or exists(select id_user from _UserGroupBuf where id_user = _ID_User) -- зациклились на искомого юзера
	then
	  _res := 0;
	  exit;
	end if;
	
    _level := _level + 1;	
  end loop;
  
  drop table _UserGroupBuf;
  return _res;  
end;
$$ language plpgsql;

grant execute on function CanAddUserToGroup(_ID_User int, _ID_Group int) to "GM_Admin";