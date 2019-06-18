////////////////////////////////////////////
// Специфика ведения лого для сервиса
////////////////////////////////////////////
unit SvcLogFile;

interface

uses Windows, ProgramLogFile, SysUtils, DateUtils;

type
  TServiceProgramLogFile = class(TProgramLogFile)
  private
    FLogSettingsLastUpdate: TDateTime;

    FComLog: bool;
    FProgramLog: bool;
    FErrorLog: bool;
  protected
    function NeedComLog: bool; override;
    function NeedProgramLog(): bool; override;
    function NeedErrorLog(): bool; override;
  public
    constructor Create();

    procedure SetNeedComLog(Value: bool); override;
    procedure SetNeedProgramLog(Value: bool); override;
    procedure SetNeedErrorLog(Value: bool); override;
  end;

implementation

{ TServiceProgramLogFile }

constructor TServiceProgramLogFile.Create;
begin
  inherited Create();

  FLogSettingsLastUpdate := 0;

  FComLog := true;
  FProgramLog := true;
  FErrorLog := true;
end;

function TServiceProgramLogFile.NeedComLog: bool;
begin
  Result := FComLog;
end;

function TServiceProgramLogFile.NeedErrorLog: bool;
begin
  Result := true;
end;

function TServiceProgramLogFile.NeedProgramLog: bool;
begin
  Result := FProgramLog;
end;

procedure TServiceProgramLogFile.SetNeedComLog(Value: bool);
begin
  FComLog := Value;
end;

procedure TServiceProgramLogFile.SetNeedErrorLog(Value: bool);
begin
  FErrorLog := Value;
end;

procedure TServiceProgramLogFile.SetNeedProgramLog(Value: bool);
begin
  FProgramLog := Value;
end;

end.
