DO $$
begin
  if not exists(select * from pg_tables where tablename = lower('NodeChannels')) then
    create table NodeChannels (
     ID_Chn serial -- ID канала
    ,ID_Node int not null references Nodes on delete cascade
    ,ID_PT int null references ParamTypes -- ID типа 
    ,Name varchar(100) null -- название
    ,ID_MeaUnit int null references MeaUnits on delete set null
    ,AlarmSignal int null -- тип аварийного сигнала 0 - аварий не бывает, 1 - nomal closed, 2 - normal opened
    ,AlarmThreshold float null -- порог аварии для AI
    ,BaseChn int null -- базовый канал для фейковых и расчетных
    ,OpcTag varchar(100) null -- Тэг для OPC
    ,PRIMARY KEY(ID_Chn)
    );

    create unique index cidx_NodeChannels_node_type on NodeChannels(ID_Node, ID_PT);
  end if;
  grant all on NodeChannels to "GM_Admin";
  grant all on NodeChannels_ID_Chn_seq to "GM_Admin";  
end $$;