do $$ begin perform DropFunction('SetNodeType'); end $$;
create or replace function SetNodeType(
  _ID_NodeType int,
  _Name varchar (500)
  )
  returns void
as $$
begin
  if exists(select * from NodeTypes where ID_NodeType = _ID_NodeType) then
    update NodeTypes set Name = _Name where ID_NodeType = _ID_NodeType;
  else
    insert into NodeTypes(ID_NodeType, Name) select _ID_NodeType, _Name;
  end if;
end;
$$ language plpgsql;

do $$
begin
  perform SetNodeType (1, 'Узел');
  perform SetNodeType (2, 'Узел теплоучета');
  perform SetNodeType (3, 'Труба');
  perform SetNodeType (4, 'Труба прямая');
  perform SetNodeType (5, 'Труба обратка');
  perform SetNodeType (6, 'Труба подпитка');
  perform SetNodeType (7, 'Трубопровод');
end; $$;
