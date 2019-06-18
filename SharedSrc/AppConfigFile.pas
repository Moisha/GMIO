////////////////////////////////////////////
// Конфигурационный файл
////////////////////////////////////////////
unit AppConfigFile;

interface

uses Windows, GMGlobals, IniFiles, ConnParamsStorage;

type
  TGMIniFileType = (iftINI, iftXML, iftError);

  TGMMainConfigFile = class
  private
    function ExpandConfigFileName(fn: string): string;
    function LookForCmdLineConfigParam(ext: string): string;
  protected
    procedure ReportError(const msg: string; isFatal: bool); virtual;
  public
    function GetMainINIFileName(): string; virtual; abstract;
    function CheckMainINIFile(): TGMIniFileType;
    function ReadSQLConnectionParams(var params: TZConnectionParams): bool;
  end;

  TGMMainConfigFileClass = class of TGMMainConfigFile;

  TGMClientMainConfigFile = class(TGMMainConfigFile)
  private
    function IniToXml(const fn: string): bool;
    function IsClientINI(const fn: string): bool;
    function ParseAlarms(f: TINIFile): string;
    function ParseBigWindow(f: TINIFile; n: int): string;
    function ParseBigWindowChannel(f: TINIFile; const section,
      Prefix: string): string;
    function ParseBigWindows(f: TINIFile): string;
    function ParseCommon(f: TINIFile): string;
    function ParseObject(f: TINIFile; n: int): string;
    function ParseObjectChannel(f: TINIFile; const section, Prefix: string;
      num: int): string;
    function ParseObjects(f: TINIFile): string;
  public
    function GetMainINIFileName(): string; override;
  end;

  TGMServerMainConfigFile = class(TGMMainConfigFile)
  public
    function GetMainINIFileName(): string; override;
  end;

  TGMServiceConfigFile = class(TGMServerMainConfigFile)
  protected
    procedure ReportError(const msg: string; isFatal: bool); override;
  public
    function GetMainINIFileName(): string; override;
  end;

function GMMainConfigFile(): TGMMainConfigFile;
procedure SetMainConfigFileClass(AMainConfigFileClass: TGMMainConfigFileClass);

implementation

uses SysUtils, Forms, Classes, ProgramLogFile;

var MainConfigFile: TGMMainConfigFile = nil;
    MainConfigFileClass: TGMMainConfigFileClass = nil;

function GMMainConfigFile(): TGMMainConfigFile;
begin
  if (MainConfigFile = nil) and (MainConfigFileClass <> nil) then
    MainConfigFile := MainConfigFileClass.Create();

  Result := MainConfigFile;
end;

procedure SetMainConfigFileClass(AMainConfigFileClass: TGMMainConfigFileClass);
begin
  if MainConfigFile <> nil then
    FreeAndNil(MainConfigFile);

  MainConfigFileClass := AMainConfigFileClass;
end;

function TGMMainConfigFile.ExpandConfigFileName(fn: string): string;
begin
  if ExtractFileDir(fn) <> '' then
    Result := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + fn
  else
    Result := fn;
end;

function TGMMainConfigFile.LookForCmdLineConfigParam(ext: string): string;
var i: int;
begin
  Result := '';
  for i := 1 to ParamCount() do
  begin
    if ext = LowerCase(ExtractFileExt(ParamStr(i))) then
    begin
      Result := ExpandConfigFileName(ParamStr(i));
      Exit;
    end;
  end;
end;

function TGMMainConfigFile.ReadSQLConnectionParams(var params: TZConnectionParams): bool;
var f: TIniFile;
begin
  Result := CheckMainINIFile() = iftINI;
  if Result then
  begin
    f := TIniFile.Create(GetMainINIFileName());
    try
      Params.Host := f.ReadString('SQL', 'POSTGRES_SRV', '127.0.0.1');
      Params.Port := f.ReadInteger('SQL', 'POSTGRES_PORT', 5432);
      Params.Database := f.ReadString('SQL', 'DB', '');
      Params.Login := f.ReadString('SQL', 'LOGIN', '');
      Params.Password := f.ReadString('SQL', 'PASS', '');
    finally
      f.Free();
    end;
  end;
end;

procedure TGMMainConfigFile.ReportError(const msg: string; isFatal: bool);
begin
  ShowMessageBox(msg, MB_ICONSTOP);
  if isFatal then
    Application.Terminate();
end;

function TGMMainConfigFile.CheckMainINIFile(): TGMIniFileType;
var fn: string;
begin
  Result := iftError;

  fn := GetMainINIFileName();
  try
    if not FileExists(fn) then
    begin
      ReportError('Файл ' + fn + ' не найден!', true);
      Exit;
    end;

    if LowerCase(ExtractFileExt(fn)) = '.ini' then
      Result := iftINI
    else
    if LowerCase(ExtractFileExt(fn)) = '.xml' then
      Result := iftXML;
  except
    on e: Exception do
    begin
      ReportError('Ошибка чтения файла ' + fn + #13#10 + e.Message, true);
    end;
  end;
end;

{ TGMServerMainConfigFile }

function TGMServerMainConfigFile.GetMainINIFileName: string;
begin
  Result := LookForCmdLineConfigParam('.ini');
  if Result = '' then
    Result := ChangeFileExt(Application.ExeName, '.ini');

  Result := ExpandFileName(Result);
end;

{ TGMClientMainConfigFile }

function TGMClientMainConfigFile.ParseObjectChannel(f: TINIFile; const section, Prefix: string; num: int): string;
begin
  Result := Format('        <channel num="%d" id_prm="%d" dmax="%s" dmin="%s" prefix=%s postfix=%s diagram="%d" barfloat="%s" pump_prm="%d" pump_signal="%d" digits="%d" showtype="%d" />'#13#10,
                   [ num,
                     f.ReadInteger(section, Prefix + 'ID_PRM', 0),
                     MyFloatToStr(MyStrToFloatDef(f.ReadString(section, Prefix+'DIAGRAMMAX', ''), 100)),
                     MyFloatToStr(MyStrToFloatDef(f.ReadString(section, Prefix+'DIAGRAMMIN', ''), 0)),
                     AnsiQuotedStr(f.ReadString(section, Prefix + 'TEXT1', ''), '"'),
                     AnsiQuotedStr(f.ReadString(section, Prefix + 'TEXT2', ''), '"'),
                     f.ReadInteger(section, Prefix + 'DIAGRAM', 0),
                     f.ReadString(section, Prefix + 'BARFLOAT', ''),
                     f.ReadInteger(section, Prefix + 'ID_PUMPDOWNPRM', 0),
                     f.ReadInteger(section, Prefix + 'PUMPDOWNSIGNAL', 1),
                     f.ReadInteger(section, Prefix + 'DIGITS', 0),
                     f.ReadInteger(section, Prefix + 'SHOWTYPE', 0)
                   ]);
end;

function TGMClientMainConfigFile.ParseObject(f: TINIFile; n: int): string;
var section: string;
begin
  section := 'OBJECT' + IntToStr(n);
  Result := Format('    <object name=%s x="%d" y="%d" ticks="%d" color="%d" squeeze="%d" id_obj="%d">'#13#10,
                   [ AnsiQuotedStr(f.ReadString(section, 'NAME', ''), '"'),
                     f.ReadInteger(section, 'X', 0),
                     f.ReadInteger(section, 'Y', 0),
                     f.ReadInteger(section, 'DIAGRAM_TICKS', 0),
                     f.ReadInteger(section, 'COLOR', 1),
                     f.ReadInteger(section, 'SQUEEZE', 100),
                     f.ReadInteger(section, 'ID', 0)
                    ]);

  Result := Result + '      <channels>'#13#10;
  Result := Result + ParseObjectChannel(f, section, 'BAR_', 0);
  Result := Result + ParseObjectChannel(f, section, 'VAL1_', 1);
  Result := Result + ParseObjectChannel(f, section, 'VAL2_', 2);
  Result := Result + ParseObjectChannel(f, section, 'VAL3_', 3);
  Result := Result + ParseObjectChannel(f, section, 'VAL4_', 4);
  Result := Result + '      </channels>'#13#10;

  Result := Result + '    </object>'#13#10;
end;

function TGMClientMainConfigFile.ParseObjects(f: TINIFile): string;
var i: int;
begin
  Result := Result + '  <objects>'#13#10;
  i := 1;
  while f.SectionExists('OBJECT' + IntToStr(i)) do
  begin
    Result := Result + ParseObject(f, i);
    inc(i);
  end;
  Result := Result + '  </objects>'#13#10;
end;

function TGMClientMainConfigFile.ParseAlarms(f: TINIFile): string;
var sl: TStringList;
    i, prm: int;
begin
  sl := TStringList.Create();
  try
    Result := Result + '  <alarms>'#13#10;

    f.ReadSection('ALARMS', sl);
    for i := 0 to sl.Count - 1 do
    begin
      prm := f.ReadInteger('ALARMS', sl[i], 0);
      if prm > 0 then
        Result := Result + '    <alarm id_prm="' + IntToStr(prm) + '" />'#13#10;
    end;

    Result := Result + '  </alarms>'#13#10;
  finally
    sl.Free();
  end;
end;

function TGMClientMainConfigFile.ParseCommon(f: TINIFile): string;
var sl: TStringList;
    i: int;
    s: string;
begin
  sl := TStringList.Create();
  try
    Result := '  <common>'#13#10;

    f.ReadSection('COMMON', sl);
    for i := 0 to sl.Count - 1 do
    begin
      s := LowerCase(sl[i]);
      Result := Result + Format('    <%s>%s</%s>'#13#10, [s, f.ReadString('COMMON', s, ''), s]);
    end;

    Result := Result + '  </common>'#13#10;
  finally
    sl.Free();
  end;
end;

function TGMClientMainConfigFile.ParseBigWindowChannel(f: TINIFile; const section, Prefix: string): string;
begin
  Result := Format('        <diagram id_prm="%d" dmax="%s" dmin="%s" text=%s diagram="%d" colwidth="%d" />'#13#10,
                   [ f.ReadInteger(section, Prefix, 0),
                     MyFloatToStr(f.ReadFloat(section, Prefix + '_DMAX', 100)),
                     MyFloatToStr(f.ReadFloat(section, Prefix + '_DMIN', 0)),
                     AnsiQuotedStr(f.ReadString(section, Prefix + '_TITLE', ''), '"'),
                     f.ReadInteger(section, Prefix + '_WANTDIAGRAM', 1),
                     f.ReadInteger(section, Prefix + '_COLWIDTH', 0) ]);
end;

function TGMClientMainConfigFile.ParseBigWindow(f: TINIFile; n: int): string;
var section: string;
    i: int;
begin
  section := 'BIGWINDOW' + IntToStr(n);
  Result := Format('    <bigwindow name=%s>'#13#10,
                   [ AnsiQuotedStr(f.ReadString(section, 'NAME', ''), '"') ]);

  Result := Result + '      <diagrams>'#13#10;
  for i := 0 to f.ReadInteger(section, 'PRM_COUNT', 0) - 1 do
    Result := Result + ParseBigWindowChannel(f, section, 'PRM_' + IntToStr(i));
  Result := Result + '      </diagrams>'#13#10;

  Result := Result + '    </bigwindow>'#13#10;
end;

function TGMClientMainConfigFile.ParseBigWindows(f: TINIFile): string;
var i: int;
begin
  Result := Result + '  <bigwindows>'#13#10;
  i := 1;
  while f.SectionExists('BIGWINDOW' + IntToStr(i)) do
  begin
    Result := Result + ParseBigWindow(f, i);
    inc(i);
  end;
  Result := Result + '  </bigwindows>'#13#10;
end;

function TGMClientMainConfigFile.IniToXml(const fn: string): bool;
var f: TINIFile;
    xml: string;
begin
  Result := false;

  try
    f := TIniFile.Create(fn);

    xml := '<?xml version="1.0" encoding="Windows-1251"?>'#13#10 +
           '<config>'#13#10;

    xml := xml + ParseCommon(f);
    xml := xml + ParseObjects(f);
    xml := xml + ParseAlarms(f);
    xml := xml + ParseBigWindows(f);

    xml := xml + '</config>';

    SaveStringToFile(xml, ChangeFileExt(ExpandFileName(fn), '.xml'));

    f.free();
    Result := true;
  except
  end;
end;

function TGMClientMainConfigFile.IsClientINI(const fn: string): bool;
var f: TINIFile;
begin
  Result := false;
  try
    f := TIniFile.Create(fn);
    Result := f.SectionExists('ALARMS')
              or f.SectionExists('OBJECT1')
              or f.SectionExists('BIGWINDOW1')
              or f.SectionExists('BIGWINDOW1');

    Result := Result and not f.SectionExists('SQL');
    f.free();
  except
  end;
end;

function TGMClientMainConfigFile.GetMainINIFileName: string;
var xmlname, ininame: string;
begin
  Result := LookForCmdLineConfigParam('.xml');
  if Result = '' then
    Result := LookForCmdLineConfigParam('.ini');

  if Result = '' then
  begin
    Result := ChangeFileExt(Application.ExeName, '.xml');
    if not FileExists(Result) then
    begin
      ininame := ChangeFileExt(Result, '.ini');
      if FileExists(ininame) then
        Result := ininame;
    end;
  end;

  if (ExtractFileExt(Result) = '.ini') and IsClientINI(Result) then
  begin
    xmlname := ChangeFileExt(Result, '.xml');
    if not FileExists(xmlname) then
      IniToXml(Result);

    Result := xmlname;
  end;
end;

{ TGMServiceConfigFile }

function TGMServiceConfigFile.GetMainINIFileName: string;
begin
  Result := inherited GetMainINIFileName();
  if (Result = '') or not FileExists(Result) then // не нашли свою - поищем от сервера
    Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) +  'GMIOPSrv.ini';
end;

procedure TGMServiceConfigFile.ReportError(const msg: string; isFatal: bool);
begin
  ProgramLog.AddError(msg);
end;

initialization

finalization
  if Assigned(MainConfigFile) then
    GMMainConfigFile.Free();
end.
