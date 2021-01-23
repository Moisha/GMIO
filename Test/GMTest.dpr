program GMTest;

uses
  FastMM4,
  TestFramework,
  GUITestRunner,
  Test.GBV in 'Test.GBV.pas',
  Test.Devices.Simag in 'Test.Devices.Simag.pas',
  Test.CommonRoutines in 'Test.CommonRoutines.pas',
  Test.Devices.Ancom in 'Test.Devices.Ancom.pas',
  Test.Devices.Vzlet in 'Test.Devices.Vzlet.pas',
  Test.ReqCreator in 'Test.ReqCreator.pas',
  Test.Threads.GMOPC in 'Test.Threads.GMOPC.pas',
  Test.GMSocket in 'Test.GMSocket.pas',
  Test.UniversalExchange in 'Test.UniversalExchange.pas',
  Test.Script in 'Test.Script.pas',
  Test.ObjectOnline in 'Test.ObjectOnline.pas',
  Threads.ObjectOnline in '..\Service\Threads.ObjectOnline.pas',
  Test.Devices.Energomera in 'Test.Devices.Energomera.pas',
  Test.Devices.Termotronic in 'Test.Devices.Termotronic.pas',
  Test.Devices.Base.ReqCreator in 'Test.Devices.Base.ReqCreator.pas',
  Test.Devices.ICP.ReqCreator in 'Test.Devices.ICP.ReqCreator.pas',
  Devices.ICP in '..\Service\Devices.ICP.pas',
  Devices.Logica.Base in '..\Service\Devices.Logica.Base.pas',
  Devices.Mercury in '..\Service\Devices.Mercury.pas',
  Devices.Owen in '..\Service\Devices.Owen.pas',
  Devices.ReqCreatorBase in '..\Service\Devices.ReqCreatorBase.pas',
  Devices.Simag in '..\Service\Devices.Simag.pas',
  Devices.Tecon in '..\Service\Devices.Tecon.pas',
  Devices.Termotronic in '..\Service\Devices.Termotronic.pas',
  Devices.UBZ in '..\Service\Devices.UBZ.pas',
  Devices.Vacon in '..\Service\Devices.Vacon.pas',
  Devices.Vzlet in '..\Service\Devices.Vzlet.pas',
  Test.Devices.UBZ in 'Test.Devices.UBZ.pas',
  Test.Devices.TR101.ReqCreator in 'Test.Devices.TR101.ReqCreator.pas',
  Test.Devices.TRM138.ReqCreator in 'Test.Devices.TRM138.ReqCreator.pas',
  Test.Devices.Vzlet.ReqCreator in 'Test.Devices.Vzlet.ReqCreator.pas',
  Test.Devices.Vacon.ReqCreator in 'Test.Devices.Vacon.ReqCreator.pas',
  Test.Devices.Tecon19.ReqCreator in 'Test.Devices.Tecon19.ReqCreator.pas',
  Test.Devices.ISCO.ReqCreator in 'Test.Devices.ISCO.ReqCreator.pas',
  Test.Devices.Base.ReqParser in 'Test.Devices.Base.ReqParser.pas',
  Test.Devices.TRM138.Parser in 'Test.Devices.TRM138.Parser.pas',
  Test.Devices.Mercury.ReqCreator in 'Test.Devices.Mercury.ReqCreator.pas',
  Test.Devices.Mercury.Parser in 'Test.Devices.Mercury.Parser.pas',
  Test.GeomerLastValues in 'Test.GeomerLastValues.pas',
  Test.Devices.Altistart22 in 'Test.Devices.Altistart22.pas',
  Devices.Altistart22 in '..\Service\Devices.Altistart22.pas',
  Devices.ModbusBase in '..\SharedSrc\Devices.ModbusBase.pas',
  Test.Devices.Tecon19.Parser in 'Test.Devices.Tecon19.Parser.pas',
  Test.Devices.Geostream in 'Test.Devices.Geostream.pas',
  Devices.Geostream in '..\Service\Devices.Geostream.pas',
  Threads.ResponceParser in '..\Service\Threads.ResponceParser.pas',
  Test.RemoteServer in 'Test.RemoteServer.pas',
  Test.Devices.MainSrv in 'Test.Devices.MainSrv.pas',
  Devices.MainSrv in '..\Service\Devices.MainSrv.pas',
  Test.Threads.RegularControl in 'Test.Threads.RegularControl.pas',
  Devices.Logica.SPT961 in '..\Service\Devices.Logica.SPT961.pas',
  Test.Devices.Logica.SPT961.Parser in 'Test.Devices.Logica.SPT961.Parser.pas',
  Test.Devices.Logica.SPT961.ReqCreator in 'Test.Devices.Logica.SPT961.ReqCreator.pas',
  Test.Devices.Logica.SPT941.ReqCreator in 'Test.Devices.Logica.SPT941.ReqCreator.pas',
  Test.Devices.Logica.SPT943.ReqCreator in 'Test.Devices.Logica.SPT943.ReqCreator.pas',
  Test.Devices.Logica.SPT943.Parser in 'Test.Devices.Logica.SPT943.Parser.pas',
  GMConst in '..\SharedSrc\GMConst.pas',
  Test.Devices.ModbusRTU in 'Test.Devices.ModbusRTU.pas',
  Test.Devices.ModbusTCP in 'Test.Devices.ModbusTCP.pas',
  Test.Devices.Logica.SPT941.Parser in 'Test.Devices.Logica.SPT941.Parser.pas',
  Test.Devices.DRK.ReqCreator in 'Test.Devices.DRK.ReqCreator.pas',
  Test.Devices.DRK.ReqParser in 'Test.Devices.DRK.ReqParser.pas',
  Test.Devices.ADCPChannelMaster in 'Test.Devices.ADCPChannelMaster.pas',
  Test.RequestListBuilder in 'Test.RequestListBuilder.pas',
  Test.Threads.Base in 'Test.Threads.Base.pas',
  Test.Devices.Streamlux700f in 'Test.Devices.Streamlux700f.pas';

{$R *.res}

begin
  GUITestRunner.runRegisteredTests;
end.

