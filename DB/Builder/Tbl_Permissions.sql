DO $$
begin
  if not exists(select * from pg_tables where tablename = lower('ObjectPermissions')) then 
    create table ObjectPermissions (
	  ID_User int not null references Users on delete cascade
	, ID_Obj int not null references Objects on delete cascade
    );
  end if;
  grant all on ObjectPermissions to "GM_Admin";

  pg_gm_db_template_foreign_key ObjectPermissions ID_User Users cascade
  
  pg_gm_db_template_foreign_key ObjectPermissions ID_Obj Objects cascade

  if not exists(select * from pg_tables where tablename = lower('NodePermissions')) then 
    create table NodePermissions (
	  ID_User int not null references Users on delete cascade
	, ID_Node int not null references Nodes on delete cascade
    );
  end if;
  grant all on NodePermissions to "GM_Admin";  
end $$;