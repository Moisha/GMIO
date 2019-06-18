program GMWatchDog;

uses
  Vcl.SvcMgr,
{$ifdef Application}
  Forms,
{$endif}
  WatchDog.Main in 'WatchDog.Main.pas' {SiriusWatchDog: TService};

{$R *.RES}

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
{$ifdef Application}
  Forms.Application.Initialize();
  Forms.Application.CreateForm(TSiriusWatchDog, SiriusWatchDog);
  Forms.Application.ShowMainForm := false;
  SiriusWatchDog.InitService();
  Readln;
{$else}
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TSiriusWatchDog, SiriusWatchDog);
  Application.Run;
{$endif}
end.
