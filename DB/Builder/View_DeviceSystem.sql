create view DeviceSystem as
  select p.*,
         d.ID_Obj,
         d.id_devtype,
         "number",
         devname,
         baudrate,
         porttype,
         dt_updated,
         AddrBase,
         ReqPackSize,
         o.name as ObjectName,
         n_car,
         objtype,
         remotesrv,
         remoteport,
         remoteName,
         o.RemoteID as ObjRemoteID,
         d.RemoteID as DevRemoteID,
         p.RemoteID as PrmRemoteID,
         Converter,
         AllowZeroNumber,
         LastArchDay,
         LastArchHour,
         LastCurrent
         
    from Params p 
         left outer join ParamStates ps on ps.ID_Prm = p.ID_Prm
         left outer join Devices d on d.ID_Device = p.ID_Device
         left outer join Devtypes dt on dt.ID_Devtype = d.ID_DevType
         left outer join Objects o on o.ID_Obj = d.ID_Obj;

grant select on DeviceSystem to "GM_Admin";