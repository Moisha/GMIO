do $$ begin perform DropFunction('GetUser#Object#s'); end $$;
create or replace function GetUser#Object#s(_ID_User int)
returns SETOF #Object#s
as $$
declare _st int; _group int; 
begin
  if coalesce(GetConfigPrm('AuthRequired'), '') <> '' then -- требуется авторизация
    if coalesce(_ID_User, 0) <= 0 then
      return;  
	end if;
	  
    select State into _st from Users where ID_User = _ID_User;
  else
    _st := 1;
  end if;
  
  if _st is null then
    return;
  elsif _st & 1 > 0 then
    return query (select * from #Object#s #AdminCondition#);    
  else -- простой пользователь или группа
    return query (select * from #Object#s 
	                where #KeyField# in (select #KeyField# from #Object#Permissions where ID_User = _ID_User));
    for _group in (select ID_Group from UserGroups where ID_User = _ID_User)
    loop
      return query (select * from GetUser#Object#s(_group));
    end loop;
  end if;
end;
$$ language plpgsql;

grant execute on function GetUser#Object#s(_ID_User int) to "GM_Admin";
-- select * from GetUser#Object#s(4);
