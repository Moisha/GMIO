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
  object udpSrv: TIdUDPServer
    Bindings = <>
    DefaultPort = 0
    OnUDPRead = udpSrvUDPRead
    Left = 80
    Top = 16
  end
end
