
do $$ begin perform DropFunction('AddDeviceLostParams'); end $$;
create or replace function AddDeviceLostParams(
	_ID_Device int,  
	_argument int = null) -- доп. аргумент, обычно номер трубопровода
	                      -- null - добавить все
	                      -- 0 - добавить только с аргументом по умолчанию
	                      -- другое число - добавить вплоть до указанного аргумента
  returns void
as $$
begin
  insert into Params (ID_Device, ID_PT, ID_Src, N_Src, ID_MeaUnit, Name, ExtData, CurrentsAddr, CurrentsArgument, HourArchAddr, HourArchArgument)
    select _ID_Device, pl.ID_PT, pl.ID_Src, pl.N_Src, coalesce(pl.ID_MeaUnit, pt.DefMeaUnit), pl.PName, pl.ExtData, pl.CurrentsAddr, pl.Argument, pl.HourArchAddr, pl.HourArchArgument
      from DevParamListGuide pl 
           left outer join ParamTypes pt on pt.ID_PT = pl.ID_PT
           inner join Devices d on ID_Device = _ID_Device and pl.ID_DevType = d.ID_DevType
           inner join DevTypes dt on dt.ID_DevType = d.ID_DevType
        where (  
                 (_argument is null) -- добавить все
                 or Argument is null -- общие добавляем всегда
                 or (_argument = 0 and (DefaultChannelsArgument is null or Argument <= DefaultChannelsArgument)) -- добавить по умолчанию
                 or (Argument <= _argument) -- добавить все вплоть до указанного
              )              
              and not exists(select * from Params p where ID_Device = _ID_Device and p.ID_Src = pl.ID_Src and p.N_Src = pl.N_Src);
end;
$$ language plpgsql;

grant execute on function AddDeviceLostParams(_ID_Device int, _argument int) to "GM_Admin";
