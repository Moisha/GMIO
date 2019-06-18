
-------------------------------------------
-- Вспомогательные ф-и
------------------------------------------

create or replace function DropFunction(_name text)
  returns void
as $$
declare _cmd text; _arg text;
begin
  if not exists(select * from pg_proc where proname = lower(_name)) then
    return;
  end if;

  select oidvectortypes(p.proargtypes) into _arg
    from pg_proc p
      where p.proname = lower(_name);

  _cmd := 'drop function ' || _name || '( ' || coalesce(_arg, '') ||' )';
  execute _cmd;
end;
$$ language plpgsql;

do $$ begin perform DropFunction('UpdateSequence'); end $$;
create or replace function UpdateSequence(
   _table varchar (100)
  ,_field varchar (100)
  ,_FromNull int = null
  )
  returns void
as $$
declare _cmd varchar(1000); _seq varchar(1000); _nt int; _nseq int;
begin
  _cmd := 'select max(' || _field || ') from ' || _table;
  execute _cmd into _nt;
  _nt := coalesce(_nt, 0);

  _seq := _table || '_' || _field || '_seq';
  _cmd := 'select last_value from ' || _seq;
  execute _cmd into _nseq;
  _nseq :=  coalesce(_nseq, 0);

  if _nt >= _nseq or coalesce(_FromNull, 0) = 1 then
    _cmd := 'alter sequence ' || _seq || ' restart with ' || cast(_nt + 1 as text);
    execute _cmd;  
  end if;
end;
$$ language plpgsql;