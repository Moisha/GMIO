DO $$
begin
  if not exists(select * from pg_tables where tablename = lower('Users')) then 
    create table Users (
	  ID_User serial -- ID
	, Login	text not null	-- логин
	, Password char(32) not null	-- md5 пароля
	, Name text null -- имя
	, State int not null -- 0 - обычный пользователь, 1 - админ, 2 - группа, 3 - группа админов
	, PRIMARY KEY(ID_User)
    );
  end if;
  grant all on Users to "GM_Admin";
  grant all on Users_ID_User_seq to "GM_Admin";  

  if not exists(select * from pg_tables where tablename = lower('UserGroups')) then 
    create table UserGroups (
	  ID_User int not null references Users on delete cascade -- ID_User
	, ID_Group int not null references Users(ID_User) on delete cascade -- ID_User группы
    );
  end if;
  grant all on UserGroups to "GM_Admin";

  pg_gm_db_template_foreign_key UserGroups ID_User Users cascade
  
  pg_gm_db_template_foreign_key UserGroups ID_Group Users(ID_User) cascade

  -- времянка, пока не развернем полноценную систему работы с пользователями
  if not exists(select * from Users where login = 'user') then  
    insert into Users (login, Password, State) select 'user', md5('123'), 1;
   end if;
end $$;