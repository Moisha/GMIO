object GMIOPService: TGMIOPService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  AllowPause = False
  DisplayName = 'Sirius ATM IO Server'
  OnExecute = ServiceExecute
  OnStop = ServiceStop
  Height = 470
  Width = 608
  object ssGeomer: TServerSocket
    Active = False
    Port = 65500
    ServerType = stThreadBlocking
    ThreadCacheSize = 0
    OnAccept = ssGeomerAccept
    OnGetThread = ssGeomerGetThread
    OnGetSocket = ssGeomerGetSocket
    OnClientConnect = ssGeomerClientConnect
    OnClientRead = ssGeomerClientRead
    OnClientError = ssGeomerClientError
    Left = 14
    Top = 16
  end
  object udpSrv: TIdUDPServer
    Bindings = <>
    DefaultPort = 0
    OnUDPRead = udpSrvUDPRead
    Left = 80
    Top = 16
  end
  object timerResetSocket: TTimer
    OnTimer = timerResetSocketTimer
    Left = 160
    Top = 16
  end
end
