library CalcDevices;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes,
  Math,
  Vcl.Forms;

{$R *.res}
{$R CalcData.res}

function CalcTecon(p: pointer): integer;
begin
  Result := 0;
end;

function CalcVacon(p: pointer): integer;
begin
  Result := 0;
end;

function CalcICP(p: pointer): integer;
begin
  Result := 0;
end;

function CalcModbus(p: pointer): integer;
begin
  Result := 0;
end;

function CalcGeostream(p: pointer): integer;
begin
  Result := 0;
end;

function CalcIsco(p: pointer): integer;
begin
  Result := 0;
end;

function CalcSPT941(p: pointer): integer;
begin
  Result := 0;
end;

function CalcSPT961(p: pointer): integer;
begin
  Result := 0;
end;

function CalcOven(p: pointer): integer;
begin
  Result := 0;
end;

function CalcUBZ(p: pointer): integer;
begin
  Result := 0;
end;

function CalcVzlet(p: pointer): integer;
begin
  Result := 0;
end;

function CalcSimag(p: pointer): integer;
begin
  Result := 0;
end;

function CalcMercury(p: pointer): integer;
begin
  Result := 0;
end;

function CalcADCP(p: pointer): integer;
begin
  Result := 0;
end;

function CalcTR101(p: pointer): integer;
begin
  Result := 0;
end;

exports
  CalcTecon,
  CalcVacon,
  CalcICP,
  CalcModbus,
  CalcGeostream,
  CalcIsco,
  CalcSPT941,
  CalcSPT961,
  CalcOven,
  CalcUBZ,
  CalcVzlet,
  CalcSimag,
  CalcMercury,
  CalcADCP,
  CalcTR101;

begin

end.
