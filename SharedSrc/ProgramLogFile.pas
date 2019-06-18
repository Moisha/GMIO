////////////////////////////////////////////
// Ћоги
////////////////////////////////////////////
unit ProgramLogFile;

interface

uses Windows, Classes, GMGlobals, SysUtils;

type
  TLogFile = class
  private
    FTemplate: string;
    FLogFileNumber: int;
    FLogFileWriteCount: int;
    procedure CheckLogSize;
    procedure UpdateWriteCount;
    procedure AppendString(const s: string);
  protected
    function Prefix: string; virtual; abstract;
    function FileName(): string;
    procedure UpdateTemplate(const path: string);
  public
    procedure Write(const Msg: string); virtual;
    constructor Create(const LogPath: string = '');
    destructor Destroy; override;
  end;

  TProgramLogFile = class
  private
    FProgramLog, FErrorLog, FExchangeLog: TLogFile;
  protected
    function NeedExchangeBuf: bool; virtual;
    function NeedComLog: bool; virtual;
    function NeedProgramLog(): bool; virtual;
    function NeedErrorLog(): bool; virtual;
  public
    procedure AddMessage(const s: string);
    procedure AddError(const s: string);
    procedure AddException(const s: string);
    procedure AddExchangeBuf(const s: string); overload;
    procedure AddExchangeBuf(const comDevice: string; direction: int; buf: array of Byte; bufLen: int); overload;
    procedure AddExchangeBuf(const comDevice: string; direction: int; const buf: string); overload;

    constructor Create();
    destructor Destroy; override;

    procedure SetLogPath(const Value: string);
    procedure SetNeedComLog(Value: bool); virtual;
    procedure SetNeedProgramLog(Value: bool); virtual;
    procedure SetNeedErrorLog(Value: bool); virtual;
  end;

  TProgramLogFileClass = class of TProgramLogFile;

  function ProgramLog(): TProgramLogFile;

var ProgramLogFileClass: TProgramLogFileClass = TProgramLogFile;

implementation

uses GMConst, Forms, JclDebug;

function JCLLastExceptionCallStack(): string;
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    JclLastExceptStackListToStrings(sl, true, true, true, true);
    Result := sl.Text;
  finally
    sl.Free();
  end;
end;

var applicationProgramLog: TProgramLogFile = nil;
    LogSynch: TMultiReadExclusiveWriteSynchronizer;

function ProgramLog(): TProgramLogFile;
begin
  LogSynch.BeginWrite();
  try
    if applicationProgramLog = nil then
      applicationProgramLog := ProgramLogFileClass.Create();

    Result := applicationProgramLog;
  finally
    LogSynch.EndWrite();
  end;
end;

type
  TFileForErrorLog = class(TLogFile)
  protected
    function Prefix(): string; override;
  end;

  TFileForProgramLog = class(TLogFile)
  protected
    function Prefix(): string; override;
  end;

  TFileForExchangeLog = class(TLogFile)
  protected
    function Prefix(): string; override;
    procedure Write(const Msg: string); override;
  end;

function TFileForErrorLog.Prefix(): string;
begin
  Result := '_error';
end;

function TFileForProgramLog.Prefix(): string;
begin
  Result := '_trace';
end;

function TFileForExchangeLog.Prefix: string;
begin
  Result := '_com';
end;

procedure TFileForExchangeLog.Write(const Msg: string);
begin
  inherited Write(Msg);

{$ifdef MESSAGE_COM_LOG}
  if (Application <> nil) and (Application.MainForm <> nil) then
    GMPostMessage(WM_UPDATE_COM_LOG, WPARAM(TStringClass.Create(Msg)), 0);
{$endif}
end;

{ TProgramLogFile }

procedure TProgramLogFile.AddError(const s: string);
begin
  AddMessage('!!!! ' + s); // в логе программы пометим ошибку

  if NeedErrorLog() then // в логе ошибок - незачем
    FErrorLog.Write(s);
end;

procedure TProgramLogFile.AddExchangeBuf(const comDevice: string; direction: int; buf: array of Byte; bufLen: int);
var s: string;
begin
  if not NeedExchangeBuf() then Exit;

  case direction of
    COM_LOG_IN: s := '<';
    COM_LOG_OUT: s := '?';
    else s := '';
  end;

  s := s + #9 + comDevice + #9 + IntToStr(bufLen) + #9 + ArrayToString(buf, bufLen, true, true);
  AddExchangeBuf(s);
end;

procedure TProgramLogFile.AddException(const s: string);
begin
  AddError(s + #13#10 + JCLLastExceptionCallStack());
end;

procedure TProgramLogFile.AddExchangeBuf(const comDevice: string; direction: int; const buf: string);
var s: string;
begin
  if not NeedExchangeBuf() then Exit;

  case direction of
    COM_LOG_IN: s := '<';
    COM_LOG_OUT: s := '?';
    else s := '';
  end;

  s := s + #9 + comDevice + #9 + IntToStr(Length(buf)) + #9 + buf;
  AddExchangeBuf(s);
end;

procedure TProgramLogFile.AddExchangeBuf(const s: string);
begin
  if NeedComLog() then
    FExchangeLog.Write(s);

  if NeedProgramLog() then
    FProgramLog.Write(s);
end;

procedure TProgramLogFile.AddMessage(const s: string);
begin
  if NeedProgramLog() then
    FProgramLog.Write(s);
end;

function TProgramLogFile.NeedErrorLog: bool;
begin
  Result := true;
end;

function TProgramLogFile.NeedExchangeBuf: bool;
begin
  Result := NeedComLog() or NeedProgramLog();
end;

function TProgramLogFile.NeedProgramLog: bool;
begin
  Result := FindCmdLineSwitch('log');
end;

procedure TProgramLogFile.SetLogPath(const Value: string);
var path: string;
begin
  if Value.Trim() = '' then
    path := ExtractFileDir(Paramstr(0))
  else
    path := Value;

  if not DirectoryExists(path) and not ForceDirectories(path) then Exit;

  FProgramLog.UpdateTemplate(path);
  FErrorLog.UpdateTemplate(path);
  FExchangeLog.UpdateTemplate(path);
end;

procedure TProgramLogFile.SetNeedComLog(Value: bool);
begin

end;

procedure TProgramLogFile.SetNeedErrorLog(Value: bool);
begin

end;

procedure TProgramLogFile.SetNeedProgramLog(Value: bool);
begin

end;

function TProgramLogFile.NeedComLog: bool;
begin
  Result := FindCmdLineSwitch('log');
end;

constructor TProgramLogFile.Create;
begin
  FProgramLog := TFileForProgramLog.Create();
  FErrorLog := TFileForErrorLog.Create();
  FExchangeLog := TFileForExchangeLog.Create();
end;

destructor TProgramLogFile.Destroy;
begin
  FProgramLog.Free();
  FErrorLog.Free();
  FExchangeLog.Free();

  inherited;
end;

{ TLogFile }

function FileSize(fileName : wideString) : Int64;
var sr : TSearchRec;
begin
  if FindFirst(fileName, faAnyFile, sr ) = 0 then
     result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
  else
     result := 0;

  FindClose(sr) ;
end;

procedure TLogFile.CheckLogSize();
var
  i: int;
  updated: bool;
begin
  updated := false;
  while FileExists(FileName()) and (FileSize(FileName()) >= 1e7) do // даем 10 ћб на один трассировочный файл
  begin
    inc(FLogFileNumber);
    updated := true;
  end;

  if updated then
  begin
    for i := 1 to FLogFileNumber - 10 do
      Windows.DeleteFile(PChar(Format(FTemplate, [i])));

    for i := FLogFileNumber + 1 to FLogFileNumber + 10 do
      Windows.DeleteFile(PChar(Format(FTemplate, [i])));
  end;
end;

constructor TLogFile.Create(const LogPath: string = '');
var path: string;
begin
  inherited Create();

  FLogFileNumber := 1;
  FLogFileWriteCount := 0;

  path := LogPath;
  if path = '' then
    path := ExtractFileDir(ParamStr(0));

  UpdateTemplate(path);
  CheckLogSize();
end;

destructor TLogFile.Destroy;
begin

  inherited;
end;

function TLogFile.FileName: string;
begin
  Result := Format(FTemplate, [FLogFileNumber]);
end;

procedure TLogFile.UpdateTemplate(const path: string);
var fn, ext: string;
begin
  fn := ExtractFileName(ParamStr(0));
  ext := ExtractFileExt(fn);
  if ext <> '' then
    Delete(fn, Length(fn) - Length(ext) + 1, Length(ext));

  FTemplate := IncludeTrailingPathDelimiter(path) + fn + prefix + '%d' + '.log';
end;

procedure TLogFile.UpdateWriteCount();
begin
  inc(FLogFileWriteCount);

  if FLogFileWriteCount >= 100 then
  begin
    CheckLogSize();
    FLogFileWriteCount := 0;
  end;
end;

procedure TLogFile.AppendString(const s: string);
var f: TextFile;
begin
  LogSynch.BeginWrite();
  try
{$I-}
    AssignFile(f, FileName());
    if not FileExists(FileName()) then
      Rewrite(f)
    else
      Append(f);

    Writeln(f, s);
    CloseFile(f);
{$I+}
  except end;
  LogSynch.EndWrite();
end;

procedure TLogFile.Write(const Msg: string);
var s: string;
begin
  s := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now()) + #9 + IntToStr(GetCurrentThreadId()) + #9 + Msg;
  AppendString(s);

  UpdateWriteCount();
end;

initialization
  LogSynch := TMultiReadExclusiveWriteSynchronizer.Create();

  // Enable raw mode (default mode uses stack frames which aren't always generated by the compiler)
  Include(JclStackTrackingOptions, stRawMode);
  // Disable stack tracking in dynamically loaded modules (it makes stack tracking code a bit faster)
  Include(JclStackTrackingOptions, stStaticModuleList);
  // Initialize Exception tracking
  JclStartExceptionTracking();

finalization
  JclStopExceptionTracking();

  TryFreeAndNil(applicationProgramLog);
  LogSynch.Free();
end.
