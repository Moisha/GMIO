program GMIOPSvc;

{$R 'GMIOPSvc.res' 'GMIOPSvc.rc'}
{$R 'EsLogging.res' 'EsLogging.RC'}

uses
  FastMM4,
  esLogging,
  Vcl.SvcMgr,
  IniFiles,
  {$ifdef Application}
  Forms,
  {$endif }
  SvcMain in 'SvcMain.pas' {GMIOPService: TService},
  Threads.AncomGPRS in 'Threads.AncomGPRS.pas',
  ClientResponce in 'ClientResponce.pas',
  GeomerLastValue in 'GeomerLastValue.pas',
  GM485 in 'GM485.pas',
  GMBlockValues in 'GMBlockValues.pas',
  Threads.GMCOM in 'Threads.GMCOM.pas',
  Threads.GMK104 in 'Threads.GMK104.pas',
  Threads.K105 in 'Threads.K105.pas',
  RecordsForRmSrv in 'RecordsForRmSrv.pas',
  Threads.ReqSpecDevTemplate in 'Threads.ReqSpecDevTemplate.pas',
  UDPBindingStorage in 'UDPBindingStorage.pas',
  GMSqlQuery in '..\SharedSrc\GMSqlQuery.pas',
  UsefulQueries in '..\SharedSrc\UsefulQueries.pas',
  ProgramLogFile in '..\SharedSrc\ProgramLogFile.pas',
  GMGlobals in '..\SharedSrc\GMGlobals.pas',
  AppConfigFile in '..\SharedSrc\AppConfigFile.pas',
  StdRequest in '..\SharedSrc\StdRequest.pas',
  StdResponce in '..\SharedSrc\StdResponce.pas',
  Threads.ObjectOnline in 'Threads.ObjectOnline.pas',
  GMConst in '..\SharedSrc\GMConst.pas',
  Threads.Base in '..\SharedSrc\Threads.Base.pas',
  IEC_2061107 in '..\SharedSrc\IEC_2061107.pas',
  Devices.Ancom in 'Devices.Ancom.pas',
  Devices.Owen in 'Devices.Owen.pas',
  Devices.Tecon in 'Devices.Tecon.pas',
  Devices.UBZ in 'Devices.UBZ.pas',
  Devices.Vzlet in 'Devices.Vzlet.pas',
  Devices.Mercury in 'Devices.Mercury.pas',
  Devices.Vacon in 'Devices.Vacon.pas',
  Devices.Termotronic in 'Devices.Termotronic.pas',
  Devices.ReqCreatorBase in 'Devices.ReqCreatorBase.pas',
  Devices.ICP in 'Devices.ICP.pas',
  Devices.Simag in 'Devices.Simag.pas',
  GMSocket in 'GMSocket.pas',
  Devices.TR101 in 'Devices.TR101.pas',
  Devices.Isco4250 in 'Devices.Isco4250.pas',
  RequestList in 'RequestList.pas',
  Devices.ModbusBase in '..\SharedSrc\Devices.ModbusBase.pas',
  Devices.Altistart22 in 'Devices.Altistart22.pas',
  Devices.Geostream in 'Devices.Geostream.pas',
  Connection.Base in '..\SharedSrc\Connection.Base.pas',
  Connection.TCP in '..\SharedSrc\Connection.TCP.pas',
  Connection.COM in 'Connection.COM.pas',
  Connection.UDP in '..\SharedSrc\Connection.UDP.pas',
  Threads.TCP in 'Threads.TCP.pas',
  Devices.AllenBradley.Micro8xx in 'Devices.AllenBradley.Micro8xx.pas',
  Threads.ResponceParser in 'Threads.ResponceParser.pas',
  Threads.SQLExecuter in 'Threads.SQLExecuter.pas',
  Threads.NIUpdater in 'Threads.NIUpdater.pas',
  Threads.RemoteSrv in 'Threads.RemoteSrv.pas',
  Devices.Tecon.Common in '..\SharedSrc\Devices.Tecon.Common.pas',
  RequestThreadsContainer in 'RequestThreadsContainer.pas',
  Threads.RegularControl in 'Threads.RegularControl.pas',
  Threads.MainSrvDataPicker in 'Threads.MainSrvDataPicker.pas',
  Devices.MainSrv in 'Devices.MainSrv.pas',
  Devices.Logica.Base in 'Devices.Logica.Base.pas',
  Devices.Logica.SPT961 in 'Devices.Logica.SPT961.pas',
  Devices.Logica.SPT941 in 'Devices.Logica.SPT941.pas',
  Devices.Logica.SPT943 in 'Devices.Logica.SPT943.pas',
  Threads.Pool in 'Threads.Pool.pas',
  Devices.Modbus.ReqCreatorBase in 'Devices.Modbus.ReqCreatorBase.pas',
  Devices.DRK in 'Devices.DRK.pas',
  Devices.ADCPChannelMaster in 'Devices.ADCPChannelMaster.pas',
  Devices.Geomer in 'Devices.Geomer.pas',
  Threads.TcpServer in 'Threads.TcpServer.pas';

procedure ConfigureLogging();
  var
    f: TIniFile;
  begin
    if GMMainConfigFile.CheckMainINIFile() <> iftINI then
      Exit;

    try
      f := TIniFile.Create(GMMainConfigFile.GetMainINIFileName());
      try
        TesLogging.TCPPort := f.ReadInteger('COMMON', 'PORT', 0);
      finally
        f.Free();
      end;
    except
    end;

    TesLogging.ConfigureLogging();
  end;

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  ConfigureLogging();
{$ifdef Application}
  Forms.Application.Initialize();
  Forms.Application.CreateForm(TGMIOPService, GMIOPService);
  Forms.Application.ShowMainForm := false;
  GMIOPService.InitService();
  Forms.Application.Run;
  Readln;
{$else}
  if not Vcl.SvcMgr.Application.DelayInitialize or Vcl.SvcMgr.Application.Installing then
    Vcl.SvcMgr.Application.Initialize;
  Vcl.SvcMgr.Application.CreateForm(TGMIOPService, GMIOPService);

  if GMIOPService.CanStart() and not GMIOPService.StartAsManageService() then
    Vcl.SvcMgr.Application.Run;
{$endif}
end.
