do $$ begin perform DropFunction('PrmRealSrc'); end $$;
create or replace function PrmRealSrc(_ID_Prm int)
returns int
as $$
declare _ID_Src int; _UserSrc int; _res int;
begin  
  select ID_Src, UserSrc into _ID_Src, _UserSrc from Params where ID_Prm = _ID_Prm;
  if coalesce(_ID_Src, 0) = 7 then -- SRC_USR
    _ID_Src := _UserSrc;
  end if;
  
  return _ID_Src;
end;
$$ language plpgsql;

grant execute on function PrmRealSrc(_ID_Prm int) to "GM_Admin";