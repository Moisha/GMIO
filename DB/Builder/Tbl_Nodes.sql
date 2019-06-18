DO $$
begin
  
  if not exists(select * from pg_tables where tablename = lower('NodeTypes')) then 
    create table NodeTypes (
	  ID_NodeType int not null -- ID
	, Name text not null	-- имя
	, PRIMARY KEY(ID_NodeType)
    );
  end if;
  grant all on NodeTypes to "GM_Admin";  
  
  if not exists(select * from pg_tables where tablename = lower('Nodes')) then 
    create table Nodes (
	  ID_Node serial -- ID
	, Name text not null	-- имя
	, ID_NodeType int not null references NodeTypes -- тип узла
	, ID_Parent int null references Nodes on delete cascade -- родительский узел
	, PRIMARY KEY(ID_Node)
    );
  end if;
  grant all on Nodes to "GM_Admin";
  grant all on Nodes_ID_Node_seq to "GM_Admin";
  
end $$;