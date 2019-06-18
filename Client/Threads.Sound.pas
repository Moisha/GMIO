unit Threads.Sound;

interface

uses
  Windows, Threads.Base, Classes;

type
  TGMSoundThread = class(TGMThread)
  private
    FAlarm: bool;
    FAlarmFileName: string;
    FAlarmSound: TMemoryStream;
    procedure SetAlarm(const Value: bool);
    procedure SetAlarmFileName(const Value: string);
  protected
    procedure SafeExecute(); override;
  public
    procedure Stop;
    property Alarm: bool read FAlarm write SetAlarm;
    property AlarmFileName: string read FAlarmFileName write SetAlarmFileName;
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses GMMP3, MMSystem, SysUtils;

{ TGMSoundThread }

procedure TGMSoundThread.Stop();
begin
  PlaySound(nil, 0, 0);
end;

constructor TGMSoundThread.Create;
begin
  inherited Create(false);
  FAlarmSound := TMemoryStream.Create();
end;

destructor TGMSoundThread.Destroy;
begin
  Stop();
  FAlarmSound.Free();

  inherited;
end;

procedure TGMSoundThread.SafeExecute;
begin
  while not Terminated do
  begin
    if FAlarm and (FAlarmSound.Size > 0) then
      PlaySound(FAlarmSound.Memory, 0, SND_MEMORY or SND_ASYNC or SND_NOSTOP)
    else
      Stop();

    Sleep(50);
  end;

  PlaySound(nil, 0, 0);
end;

procedure TGMSoundThread.SetAlarm(const Value: bool);
begin
  FAlarm := Value;
  if not FAlarm then
    Stop();
end;

procedure TGMSoundThread.SetAlarmFileName(const Value: string);
var
  b: bool;
  fs: TFileStream;
begin
  if FAlarmFileName = Value then
    Exit;

  b := FAlarm;
  FAlarm := false;
  Stop();
  FAlarmFileName := Value;

  FAlarmSound.Clear();
  if (FAlarmFileName <> '') and FileExists(FAlarmFileName) then
  try
    fs := TFileStream.Create(FAlarmFileName, fmOpenRead or fmShareDenyNone);
    try
      ConvertMp3ToWav(fs, FAlarmSound);
    finally
      fs.Free();
    end;
  except end;

  SetAlarm(b);
end;

end.
