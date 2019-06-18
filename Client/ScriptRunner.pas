unit ScriptRunner;

interface

uses
  Windows, SysUtils, Classes, GMGlobals, FlexBase, JvInterpreter, h_GMClient;


type
  TPasScriptAction = (psaCompile, psaRun);
  TPasScriptActionSet = set of TPasScriptAction;

  TScriptRunner = class
  private
    FScript: TStrings;
    FLastError: string;
    FStartTick: int64;
    hcPasScript: TJvInterpreterProgram;

    procedure HandlePasScriptLine(Sender: TObject);
    procedure FindObjects(Sender: TObject; Identifier: string; var Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
    procedure Prepare;
    function DoAction(action: TPasScriptActionSet): bool;
    function FindObjectInControl(control: TFlexControl; const Identifier: string; var Value: Variant): bool;
  public
    ParentPanel: TFlexPanel;
    TimeOut: int;
    ParentObject: TObject;

    property LastError: string read FLastError;
    property Script: TStrings read FScript;

    constructor Create();
    destructor Destroy; override;

    function Run(): bool;
    function Compile(): bool;

    class function RunScript(script: string; panel: TFlexPanel; owner: TFlexControl = nil): bool;
  end;

implementation

{ TScriptRunner }

function TScriptRunner.FindObjectInControl(control: TFlexControl; const Identifier: string; var Value: Variant): bool;
var i: int;
begin
  Result := AnsiSameText(Identifier, control.Name);
  if Result then
  begin
    Value := O2V(control);
    Exit;
  end;

  for i := 0 to control.Count - 1 do
  begin
    Result := FindObjectInControl(control[i], Identifier, Value);
    if Result then Exit;
  end;
end;

procedure TScriptRunner.FindObjects(Sender: TObject; Identifier: string; var Value: Variant; Args: TJvInterpreterArgs; var Done: Boolean);
begin
  if AnsiSameText(Identifier, 'System') then
  begin
    Value := O2V(ParentPanel);
    Done := true;
    Exit;
  end;

  if AnsiSameText(Identifier, 'Owner') then
  begin
    Value := O2V(ParentObject);
    Done := true;
    Exit;
  end;

  if AnsiSameText(Identifier, 'GlobalArray') then
  begin
    Value := true;
    Value := O2V(TGlobalArrayContainer.GetInstance());
    Done := true;
    Exit;
  end;

  if ParentPanel = nil then Exit;

  Done := FindObjectInControl(ParentPanel.Schemes, Identifier, Value);
end;

constructor TScriptRunner.Create;
begin
  FScript := TStringList.Create();
  hcPasScript := TJvInterpreterProgram.Create(nil);
  hcPasScript.OnGetValue := FindObjects;
  hcPasScript.OnStatement := HandlePasScriptLine;
  TimeOut := 2000;
  FLastError := '';
  ParentPanel := nil;
  ParentObject := nil;
end;

destructor TScriptRunner.Destroy;
begin
  FScript.Free();
  hcPasScript.Free();

  inherited;
end;

procedure TScriptRunner.HandlePasScriptLine(Sender: TObject);
begin
  if (ParentPanel <> nil) and ParentPanel.ModalDialogMode then
  begin
    FStartTick := GetTickCount();
    ParentPanel.ModalDialogMode := false;
  end;

{$ifndef DEBUG}
  if Abs(int64(GetTickCount()) - FStartTick) > TimeOut then
    raise Exception.Create('Execution timeout expired');
{$endif}
end;

procedure TScriptRunner.Prepare();
begin
  FLastError := '';
  hcPasScript.Pas.Assign(Script);
end;

function TScriptRunner.DoAction(action: TPasScriptActionSet): bool;
begin
  Result := true;
  Prepare();

  if Script.IsEmpty() then Exit;

  try
    if psaCompile in action then
      hcPasScript.Compile();

    if psaRun in action then
    begin
      FStartTick := GeTTickCount();
      hcPasScript.Run();
    end;
  except
    on e: Exception do
    begin
      Result := false;
      FLastError := e.Message;
    end;
  end;
end;

function TScriptRunner.Compile: bool;
begin
  Result := DoAction([psaCompile]);
end;

function TScriptRunner.Run(): bool;
begin
  Result := DoAction([psaCompile, psaRun]);
end;

class function TScriptRunner.RunScript(script: string; panel: TFlexPanel; owner: TFlexControl = nil): bool;
var runner: TScriptRunner;
begin
  runner := TScriptRunner.Create();
  runner.Script.Text := script;
  runner.ParentPanel := panel;
  runner.ParentObject := owner;
  runner.TimeOut := 1000;
  try
    Result := runner.Run();
  finally
    runner.Free();
    panel.Refresh();
  end;
end;

end.
