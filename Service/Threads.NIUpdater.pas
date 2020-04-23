unit Threads.NIUpdater;

interface

uses Windows, SysUtils, Classes, Threads.Base, GMGlobals, GMConst, GeomerLastValue, GMSqlQuery, ProgramLogFile;

type
  TNIUpdateThread = class(TGMThread)
  private
    FglvBuffer: TGeomerLastValuesBuffer;
    function LoadNI(glv: TGeomerLastValue): bool;
    procedure ProcessDBMTR(q: TGMSqlQuery; obj: pointer);
  protected
    procedure SafeExecute(); override;
  public
    constructor Create(AglvBuffer: TGeomerLastValuesBuffer);
  end;

implementation

{ NIUpdateThread }

constructor TNIUpdateThread.Create(AglvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create();
  FglvBuffer := AglvBuffer;
end;

function TNIUpdateThread.LoadNI(glv: TGeomerLastValue): bool;
var qNI: TGMSqlQuery;
begin
  Result := false;

  qNI := TGMSqlQuery.Create();
  try
    qNI.Close();
    qNI.SQL.Text := 'select * from CalcNI (' + IntToStr(glv.ID_Prm) + ', null)';
    qNI.Open();

    if Terminated then Exit;

    if not qNI.Eof then
    begin
      Result := qNI.FieldByName('UTime').AsInteger > int64(glv.utNI);
      if Result then
      begin
        glv.utNI := qNI.FieldByName('UTime').AsInteger;
        glv.ValNI := qNI.FieldByName('Val').AsFloat;
      end;
    end
    else
      glv.utNI := 0;
  except
    on e: Exception do ProgramLog().AddException('LoadNI - ' + e.Message);
  end;

  qNI.Free();
end;

procedure TNIUpdateThread.ProcessDBMTR(q: TGMSqlQuery; obj: pointer);
begin
  if Terminated then Exit;

  if FglvBuffer.ValByID[q.FieldByName('ID_Prm').AsInteger] = nil then
    FglvBuffer.AddSimpleValue(q.FieldByName('ID_Prm').AsInteger, 0, 0, q);
end;

procedure TNIUpdateThread.SafeExecute;
var i: int;
    res, bWasAction: bool;
begin
  while not Terminated do
  begin
    bWasAction := false;

    ReadFromQuery('select * from DeviceSystem where ID_Src = ' + IntToStr(SRC_CNT_MTR), ProcessDBMTR);

    for i := 0 to FglvBuffer.Count - 1 do
    begin
      if FglvBuffer[i].bNI then
      begin
        res := LoadNI(FglvBuffer[i]);
        bWasAction := res or bWasAction;
      end;

      if Terminated then Exit;
    end;

    SleepThread(2000);
  end;
end;

end.
