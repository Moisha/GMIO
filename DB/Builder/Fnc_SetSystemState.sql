do $$ begin perform DropFunction('SetSystemState'); end $$;
create or replace function SetSystemState(
  _stateName text,
  _stateValue text
  ) 
returns void 
AS $$
begin 
  begin
    insert into SystemStates(StateName, StateValue, UTUpdate) select _stateName, _stateValue, NowGM();
  exception
    when others then
      update SystemStates set StateValue = _stateValue, UTUpdate = NowGM() where StateName = _stateName;
  end; 
end;
$$ language plpgsql;

grant execute on function SetSystemState(text, text) to "GM_Admin";