
do $$ begin perform DropFunction('PrmName'); end $$;
create or replace function PrmName(_ID_Prm int, _NameFields int, _Separator varchar(10))
returns text
as $$
declare _s varchar(200); _res varchar(500); _PName varchar(200); _SrcName varchar (20); _IsSrcOnly int;
begin
    --_NameFields - поле битовых флагов:
	--1 - ID_Prm
	--2 - Объект
	--4 - Прибор
	--8 - Тип параметра при наличии имени  
	--16 - тип источника при наличии имени  

  select Name into _s from Params where ID_Prm = _ID_Prm;
  _res := coalesce(_s, '');

  _IsSrcOnly := 0;
  select PName into _PName from ParamTypes where ID_PT = (select ID_PT from Params where ID_Prm = _ID_Prm);
  select SrcName into _SrcName from SrcTypes where ID_Src = (select ID_Src from Params where ID_Prm = _ID_Prm);
  
  if LTrim(_res) = '' then
    -- имя не задали, городим тип
    if coalesce(_PName, '') <> '' then
      _res := _PName;
    elsif coalesce(_SrcName, '') <> '' then
    -- тип тоже не задали, городим тип источника
      _res := _SrcName || ' ' || PrmNSrcStr(_ID_Prm);
      _IsSrcOnly := 1;
    end if;
  else
    if _NameFields & 8 > 0 and coalesce(_PName, '') <> '' then
      _res := _res || ' (' || _PName || ')';
    end if;
  end if;
  
  if _IsSrcOnly = 0 and _NameFields & 16 > 0 then
    _res := _SrcName || ' ' || PrmNSrcStr(_ID_Prm) || ' - ' || _res;
  end if;

  if _NameFields & 4 > 0 then
    select Name into _s from DevTypes 
      where ID_DevType = (select ID_DevType from Devices 
                            where ID_Device = (select ID_Device from Params where ID_Prm = _ID_Prm)); 
								
      _res := coalesce(_s, '') || _Separator || _res;
  end if;
  
  if _NameFields & 2 > 0 then
    select Name into _s from Objects 
      where ID_Obj = (select ID_Obj from Devices 
                        where ID_Device = (select ID_Device from Params where ID_Prm = _ID_Prm)); 
								
    _res := coalesce(_s, '') || _Separator || _res;
  end if;
  
  if _NameFields & 1 > 0 then
    _res := '(ID_Prm=' || LTrim(RTrim(cast(_ID_Prm as text))) || ') ' || _res;
  end if;

  return _res;
end;
$$ language plpgsql;

grant execute on function PrmName(_ID_Prm int, _NameFields int, _Separator varchar(10)) to "GM_Admin";