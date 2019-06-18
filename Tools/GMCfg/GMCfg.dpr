
program GMCfg;

uses
  Forms,
  GmCfgMain in 'GmCfgMain.pas' {GMCfgMainFrm};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGMCfgMainFrm, GMCfgMainFrm);
  Application.Run;
end.
