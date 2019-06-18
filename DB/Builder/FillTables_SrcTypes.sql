DO $$
begin
  insert into SrcTypes (SrcName, ID_Src)
  select * from( 
	select 'AI' as n, 1 as s
	union select 'DI', 2
	union select 'DO', 3
	union select 'CNT_DI', 4
	union select 'AO', 5
	union select 'SUM_AI', 6
	union select 'USR', 7
	union select 'FK', 8
	union select 'SRC_CNT_MTR', 9
	) t where not exists (select * from SrcTypes s where s.ID_Src = t.s);
end $$;
