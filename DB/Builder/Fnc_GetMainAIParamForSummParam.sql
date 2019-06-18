do $$ begin perform DropFunction('GetMainAIParamForSummParam'); end $$;
create or replace function GetMainAIParamForSummParam(_ID_Prm int)
returns int
as $$
begin 
  return (select BaseChn from Params where ID_Prm = _ID_Prm);
end;
$$ language plpgsql;

grant execute on function GetMainAIParamForSummParam(_ID_Prm int) to "GM_Admin";