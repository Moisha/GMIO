-------------------------------------------
-- Взбодрим идентификаторы
------------------------------------------
do $$ begin perform DropFunction('UpdateAutoindent'); end $$;
create or replace function UpdateAutoindent(_FromNull int = null)
  returns void
as $$
begin
  perform UpdateSequence('params', 'id_prm', _FromNull);
  perform UpdateSequence('objects', 'id_obj', _FromNull);
  perform UpdateSequence('Devices', 'id_device', _FromNull);
 
  perform UpdateSequence('Aggregates', 'ID_Aggregate', _FromNull);
  perform UpdateSequence('AggregateParams', 'id_aggregateprm', _FromNull);

  perform UpdateSequence('Users', 'ID_User', _FromNull);
  
  perform UpdateSequence('Nodes', 'ID_Node', _FromNull);
  perform UpdateSequence('NodeChannels', 'ID_Chn', _FromNull);
end; 
$$ language plpgsql;

do $$
begin
  perform UpdateAutoindent();
end; $$;