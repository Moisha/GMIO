drop trigger if exists Trg_ParamStates_I on ParamStates;
drop trigger if exists Trg_ParamStates_U on ParamStates;

do $$ begin perform DropFunction('Trg_ParamStates_IU_Fnc'); end $$;
create function Trg_ParamStates_IU_Fnc() 
returns trigger 
as
$$
declare _id_dev int;
begin
  select ID_Device into _id_dev from Params where ID_Prm = new.ID_Prm;
  perform DeviceOnline(_id_dev);
  return new;
end; $$ language plpgsql;

-- почему-то триггер сразу на Insert и Update не заработал.
-- разнес на разные

create trigger Trg_ParamStates_I 
  after insert
  on ParamStates    
  for each row
  execute procedure Trg_ParamStates_IU_Fnc();

create trigger Trg_ParamStates_U 
  after update 
  on ParamStates    
  for each row
  execute procedure Trg_ParamStates_IU_Fnc();
