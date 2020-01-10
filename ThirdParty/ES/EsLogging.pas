unit EsLogging;

interface
uses
  Windows, SysUtils, Classes, System.IOUtils, System.StrUtils, Winapi.SHFolder,
  Log4D, JclDebug,
  JclBase, JclSysInfo, JclFileUtils,
  EsExceptions, ComnDirs;

{{
Для упрощения начального конфигурирования лога можно в проект добавить директиву
   $R 'EsLogging.res' 'EsLogging.RC'
 при этом надо в файл .dproj добавить:
<RcCompile Include="$(DCM)\Es8\Logging\EsLogging.RC">
   <Form>EsLogging.res</Form>
</RcCompile>
К сожалению, Xe4 развернет $(DCM) в абсолютный путь.

 Однако, предпочтительным вариантом является создание своей дефолтной конфигурации лога
 с помошью включения в проект ресурса с именем, равным значению переменной TEsLogging.DefaultConfigResourceName и типа RCDATA
}
type
  ECantConfigureLogging = class(EsException);

  TEsLogger = class(TLogLogger)//no need to Free(), will be destroyed in finalization
  protected
    procedure DoLogException(const ex: Exception; const msg: string);
  public
    function  GetChildLogger(const childName: string): TEsLogger;overload;
    function  GetChildLogger(childClass: TClass): TEsLogger;overload;
    procedure Error(const AFormat: string; const AArgs: array of const); overload;
    procedure Warn(const AFormat: string; const AArgs: array of const); overload;
    procedure Info(const AFormat: string; const AArgs: array of const); overload;
    procedure Debug(const AFormat: string; const AArgs: array of const); overload;
    {{Записывает в лог как минимум текст и класс сообщения. Пытается также записать StackTrace, при наличии}
    procedure LogException(const ex: Exception; const msg: string = ''); overload;
    procedure LogException(const ex: Exception; const AFormat: string; const AArgs: array of const); overload;
  end;

  {{ Facade для доступа к логам, обертка над Log4D }
  TEsLogging = class
  private
    class function CreateDefaultDirectoryIfNeeded(): string;
    class procedure TryToLoadConfiguration(const configFileName: string);
    class procedure TryToCreateDefaultConfiguration(const destFileName: string);overload;
    class constructor ClassCreate();
    class function ReadDefaultConfiguration(const sourceResourceName: string): string;
  protected
    class procedure TryToCreateDefaultConfiguration(const destFileName, sourceResourceName: string);overload;
    class procedure LoadFromConfigFile(const configFileName: string);
    class function GenerateTemporaryLogName(): string;
    class procedure DoConfigureWithDefaults;
    class function  IsConfigFileObsolete(const configFileName, resourceFileName: string): boolean;
  public
    {{ Простейший способ получения логгера.
     Полученный объект не надо дестроить. Будет Destroy'ед в секции finalization}
    class function GetLogger(clazz: TClass): TEsLogger;overload;

    {{Использовать, Если требуется тонкая настройка имени логгера для организации иерархии
     Полученный объект не надо дестроить. Будет Destroy'ед в секции finalization}
    class function GetLogger(const loggerName: string): TEsLogger;overload;

    {{Возвращает логгер с именем вида "$(parentLogger.Name).childName"}
    class function GetLogger(parentLogger: TEsLogger; const childName: string): TEsLogger;overload;

    {{Возвращает логгер с именем вида "$(parentLogger.Name).childClass.ClassName"}
    class function GetLogger(parentLogger: TEsLogger; const childClass: TClass): TEsLogger;overload;

   {{ Конфигурирование системы логгирования внешним файлом}
    class procedure ConfigureLogging(configFileName: string = '');
    class function  IsConfigured(): boolean;
    class function  GetDefaultLoggingConfigFileName(): string;
    class var
      DefaultConfigResourceName: string;
    class var
      AppLogConfigurationKeyName: string;
  end;

  {{LogRollingFileAppender c поддержкой Enviromnent variable в имени файла}
  TEsLogRollingFileAppender = class(TLogRollingFileAppender)
  public
    procedure SetLogFile(const Name: string);override;
  end;

  { Send log messages to Console. }
  TLogConsoleAppender = class(TLogCustomAppender)
  protected
    procedure DoAppend(const Message: string); override;
  end;

type
  TExceptionDescription = Record
    RHeader: string;//Details, StackTrace, Exe Info
    RDescription: string;//Строковое описание
  End;
  TExceptionDescriptionArray = array[0..2] of TExceptionDescription;

function ExpandEnvironmentStrings(const szInput: string): string;
function GetThisExeBuildInfo(): string;
function GetExeBuildInfo(fileName: string = ''): string;
function CreateDetailedExceptionDescriptions(const Ex: Exception): TExceptionDescriptionArray;

implementation

function ExpandEnvironmentStrings(const szInput: string): string;
const
  MAXSIZE = 32768;
begin
  SetLength(Result, MAXSIZE);
  SetLength(Result, Windows.ExpandEnvironmentStrings(pchar(szInput), @Result[1], Length(Result)));
  Result:= Trim(Result);
end;

function GetExeBuildInfo(fileName: string = ''): string;
var
  info: TJclFileVersionInfo;
  s: string;
begin
  if Trim(fileName) = '' then
    fileName := ParamStr(0);
  Result := '';
  if FileExists(fileName) and VersionResourceAvailable(fileName) then
  begin
    info := TJclFileVersionInfo.Create(fileName);
    try
      Result:= Format('%s %s', [TPath.GetFileName(fileName), info.FileVersion]);
      if (Trim(info.InternalName) <> '') then
        s:= info.InternalName
      else
        s:= '<Empty additional>';
      Result:= Format('%s%s%s', [Result, sLineBreak, s]);
    finally
      info.Free();
    end;
  end;
end;

var
  ThisExeBuildInfo : string;

function GetThisExeBuildInfo(): string;
begin
  Result := ThisExeBuildInfo;
end;

function DoGetThisExeBuildInfo(): string;
var
  info: TJclFileVersionInfo;
  fn, s: string;
begin
  fn := TPath.GetFileName(ParamStr(0));
  if not VersionResourceAvailable(ParamStr(0)) then
  begin
    Result := Format('%s [No Version Resource]', [fn]);
    exit;
  end;
  info := TJclFileVersionInfo.Create(ParamStr(0));
  try
    Result:= Format('%s %s', [fn, info.FileVersion]);
    if (Trim(info.InternalName) <> '') then
      s:= info.InternalName
    else
      s:= '<Empty additional>';
    Result:= Format('%s%s%s', [Result, sLineBreak, s]);
  finally
    info.Free();
  end;
end;

function TryCreateFullStackTraceWithJcl(): string;
var
  stackTrace: TStringList;
begin
  stackTrace := TStringList.Create();
  try
    JclLastExceptStackListToStrings(stackTrace, True, True, True, True);
    if stackTrace.Count > 0 then
      Result := stackTrace.Text
    else
      Result := '[Empty]';
  finally
    stackTrace.Free();
  end;
end;

function CreateDetailedExceptionDescriptions(const Ex: Exception): TExceptionDescriptionArray;
var
  Index: Integer;
  procedure TryAssignResult(const aHeader, aDescription: string);
  begin
    if Trim(aDescription) <> '' then
    begin
      Result[Index].RHeader := aHeader;
      Result[Index].RDescription := aDescription;
      if (Index < High(Result))  then
        Inc(Index);
    end;
  end;
var
  ed: ExceptionWithDetails;
  s: string;
  I: Integer;
begin
  for I:= Low(Result) to High(Result) do
  begin
    Result[I].RHeader := '';
    Result[I].RDescription := '';
  end;
  Index := Low(Result);
  if (Ex is ExceptionWithDetails) then
  begin
    ed := Ex as ExceptionWithDetails;
    TryAssignResult('Details', ed.DetailsAsText);
  end;
  s:= ex.StackTrace;
  if (Trim(s) <> '') then
    TryAssignResult('StackTrace', Ex.StackTrace);
  TryAssignResult('FullStackTrace', TryCreateFullStackTraceWithJcl());
end;

{ TEsLogRollingFileAppender }

procedure TEsLogRollingFileAppender.SetLogFile(const Name: string);
  procedure TryToAssignNameWithPid(const expandedName : string);
  var
    theName, ext, newName: string;
  begin
    ext := TPath.GetExtension(expandedName);
    theName := TPath.GetFileNameWithoutExtension(expandedName);
    newName :=  TPath.Combine(TPath.GetDirectoryName(expandedName), Format('%s%x%s', [theName, GetCurrentProcessID(), ext]));
    try
      inherited SetLogFile(newName);
    except
      on E: EFOpenError do
        raise ECantConfigureLogging.CreateFmt(E, 'Ошибка при открытии лог-файла "%s"', [newName]);
      on E: EInOutError do
        raise ECantConfigureLogging.CreateFmt(E, 'Ошибка при открытии лог-файла "%s"', [newName]);
    end;
  end;

  procedure CallInheritedSetLogFile(const expandedName : string);
  begin
    try
      inherited SetLogFile(expandedName);
    except
      on E: EFOpenError do
        TryToAssignNameWithPid(expandedName);
      on E: EInOutError do
        TryToAssignNameWithPid(expandedName);
    end;
  end;

var
  expandedName : string;
  temp: string;
begin
  if Length(Trim(Name)) > 0  then
  begin
    temp := Format('%sapp.log', [PathDelim]);
    if Name.EndsWith(temp) then//раньше вот такой был дефолт, по ошибке
      temp := ReplaceText(Name, temp, (PathDelim + '%UserName%.log'))
    else
      temp := Name;
    expandedName := ExpandEnvironmentStrings(temp);
    if (expandedName.StartsWith('%ProgramData%')) then//в XP не работает
      expandedName:= ReplaceText(expandedName, '%ProgramData%', GetSpecialFolderPath(CSIDL_COMMON_APPDATA));
  end
  else
    expandedName := Name;

  try
    CallInheritedSetLogFile(expandedName);
  except
    on E: EStreamError do
      raise ECantConfigureLogging.CreateFmt(E, 'Ошибка при открытии лог-файла "%s"', [expandedName]);
  end;
end;

{ TLogConsoleAppender }

procedure TLogConsoleAppender.DoAppend(const Message: string);
begin
  Write(Message);
end;

type
  TEsLogFactory = class(TInterfacedObject, ILogLoggerFactory)
  private
    class var
      FInstance: ILogLoggerFactory;
    class procedure Init();
    class procedure CleanUp();
  public
    constructor Create();
    destructor Destroy();override;
    class function GetInstance(): ILogLoggerFactory;
    function MakeNewLoggerInstance(const Name: string): TLogLogger;
  end;

{ TEsLogFactory }

constructor TEsLogFactory.Create();
begin
  inherited Create();
  if Assigned(Log4D.LogLog) then//реально этот логгинг никогда не сработает. Когда его вызываем мы, у Log4D.LogLog запрещен Debug. А когда это вызывает Log4D, он это делает через другой конструктор
    Log4D.LogLog.Debug('TEsLogFactory.Create');
end;

destructor TEsLogFactory.Destroy;
begin
  if Assigned(Log4D.LogLog) then
  try
    Log4D.LogLog.Debug('TEsLogFactory.Destroy');
  except
    //swallow it
  end;
  inherited;
end;

function TEsLogFactory.MakeNewLoggerInstance(const Name: string): TLogLogger;
begin
  Result := TEsLogger.Create(Name);
end;

class function TEsLogFactory.GetInstance(): ILogLoggerFactory;
begin
  Result := FInstance;
end;

class procedure TEsLogFactory.Init();
begin
  FInstance := TEsLogFactory.Create();
end;

class procedure TEsLogFactory.CleanUp();
begin
  FInstance := nil;
end;

{ TEsLogging }

class constructor TEsLogging.ClassCreate();
begin
  DefaultConfigResourceName := 'DefaultLog4d';
  AppLogConfigurationKeyName := 'app.LoggingConfig';
end;

class function TEsLogging.CreateDefaultDirectoryIfNeeded(): string;
begin
  Result := CommonAppDataPathForExecutable();
end;

class procedure TEsLogging.LoadFromConfigFile(const configFileName: string);
var
  props: TStringList;
begin
  props:= TStringList.Create();
  try
    props.LoadFromFile(configFileName);
    if (props.Values[LoggerFactoryKey] = '') then
      props.Values[LoggerFactoryKey] := TEsLogFactory.ClassName;
    if props.Count < 1 then
      raise ECantConfigureLogging.CreateFmt('No properties in config file "%s"', [configFileName]);
    TLogPropertyConfigurator.Configure(props);
  finally
    props.Free();
  end;
end;

class procedure TEsLogging.TryToCreateDefaultConfiguration(const destFileName: string);
begin
  TryToCreateDefaultConfiguration(destFileName, DefaultConfigResourceName);
end;

class function TEsLogging.ReadDefaultConfiguration(const sourceResourceName: string): string;
var
  ResStream: TResourceStream;
  dest: TStringStream;
begin
  ResStream := TResourceStream.Create(HInstance, sourceResourceName, RT_RCDATA);
  try
    dest:= TStringStream.Create();
    try
      ResStream.SaveToStream(dest);
      Result := StringReplace(dest.DataString, '%ApplicationName%', TPath.GetFileNameWithoutExtension(ParamStr(0)), [rfReplaceAll]);
    finally
      dest.Free();
    end;
  finally
    ResStream.Free();
  end;
end;

class procedure TEsLogging.TryToCreateDefaultConfiguration(const destFileName, sourceResourceName: string);
begin
  TFile.WriteAllText(destFileName, ReadDefaultConfiguration(sourceResourceName), TEncoding.Default);
end;

class function TEsLogging.IsConfigFileObsolete(const configFileName, resourceFileName: string): boolean;
var
  defaultConfig, existingConfig: TStringList;
  versionFromResource, existingVersion: string;
begin
  Result := False;
  defaultConfig:= TStringList.Create();
  try
    defaultConfig.Text := ReadDefaultConfiguration(resourceFileName);
    versionFromResource := Trim(defaultConfig.Values[AppLogConfigurationKeyName]);
    if versionFromResource <> '' then
    begin
      existingConfig:= TStringList.Create();
      try
        existingConfig.LoadFromFile(configFileName);
        existingVersion := Trim(existingConfig.Values[AppLogConfigurationKeyName]);
        Result := existingVersion <> versionFromResource;
      finally
        existingConfig.Free();
      end;
    end;
  finally
    defaultConfig.Free();
  end;
end;

class procedure TEsLogging.TryToLoadConfiguration(const configFileName: string);
var
  overWriteDefaultConfig: boolean;
begin
  overWriteDefaultConfig := not FileExists(configFileName);
  if not overWriteDefaultConfig then
    overWriteDefaultConfig := IsConfigFileObsolete(configFileName, DefaultConfigResourceName);
  if (overWriteDefaultConfig) then
    TryToCreateDefaultConfiguration(configFileName);
  LoadFromConfigFile(configFileName);
end;

class function TEsLogging.GetLogger(parentLogger: TEsLogger; const childName: string): TEsLogger;
begin
  if Assigned(parentLogger) then
    Result := parentLogger.GetChildLogger(childName)
  else
    Result := TEsLogging.GetLogger(childName);
end;

class function TEsLogging.GetLogger(parentLogger: TEsLogger; const childClass: TClass): TEsLogger;
begin
  mustBeTrue(Assigned(childClass), 'childClass is nil');
  Result := GetLogger(parentLogger, childClass.ClassName);
end;

class function TEsLogging.GetDefaultLoggingConfigFileName: string;
begin
  Result := TPath.Combine(CreateDefaultDirectoryIfNeeded(), 'logging.properties');
end;

var
  LOGGING_CONFIGURED: boolean = False;

class procedure TEsLogging.ConfigureLogging(configFileName: string = '');
  procedure ConfigureWithDefaults();
  begin
    try
      DoConfigureWithDefaults();
      LOGGING_CONFIGURED := True;
    except
      on E: EStreamError do
         OutputDebugString(PChar(E.Message));
      on E: ECantConfigureLogging do
         OutputDebugString(PChar(E.Message));
    end;
  end;
begin
  try
    if Trim(configFileName) = '' then
      configFileName := GetDefaultLoggingConfigFileName();
    TryToLoadConfiguration(configFileName);
    LOGGING_CONFIGURED := True;
  except
    on E: EResNotFound do
    begin
      OutputDebugString(PChar(Format('%s. Developer must add resource %s to linking', [E.Message, TEsLogging.DefaultConfigResourceName])));
      ConfigureWithDefaults();
    end;
    on E: EStreamError do
    begin
      OutputDebugString(PChar(E.Message));
      ConfigureWithDefaults();
    end;
    on E: ECantConfigureLogging do
    begin
      OutputDebugString(PChar(E.Message));
      ConfigureWithDefaults();
    end;
  end;
end;

class function TEsLogging.IsConfigured: boolean;
begin
  Result := LOGGING_CONFIGURED;
end;

type
  THackLogOptionHandler = class(TLogOptionHandler);

class procedure TEsLogging.DoConfigureWithDefaults;
var
  LogLayout: TLogPatternLayout;
  FileAppender: TLogRollingFileAppender;
begin
  LogLayout := TLogPatternLayout.Create('%d [%p] %c - %m%n');
  THackLogOptionHandler(LogLayout).SetOption(DateFormatOpt, 'dd/MM/yyyy hh:mm:ss.zzz');
  FileAppender := TEsLogRollingFileAppender.Create('fileAppender', GenerateTemporaryLogName, LogLayout, True);
  THackLogOptionHandler(FileAppender).SetOption(MaxFileSizeOpt, '100KB');
  TLogBasicConfigurator.Configure(FileAppender);
  TLogLogger.GetRootLogger.Level := Info;
end;

class function TEsLogging.GenerateTemporaryLogName: string;
var
  fileName: string;
begin
  fileName:= Format('%s%x.log', [GetLocalUserName(), GetCurrentProcessID()]);
  Result:= TPath.Combine(CreateDefaultDirectoryIfNeeded(), fileName);
end;

class function TEsLogging.GetLogger(const loggerName: string): TEsLogger;
var
  tmp: TLogLogger;
begin
  tmp := TLogLogger.GetLogger(loggerName, TEsLogFactory.GetInstance());
  if (tmp = nil) then
    raise EInternalException.CreateFmt('TLogLogger.GetLogger for "%s" = nil', [loggerName]);
  if not (tmp is TEsLogger) then
    raise EInternalException.CreateFmt('TLogLogger.GetLogger for "%s" is of bad class: "%s:"', [loggerName, tmp.ClassType.QualifiedClassName]);
  Result := TEsLogger(tmp);
end;

class function TEsLogging.GetLogger(clazz: TClass): TEsLogger;
begin
  if clazz = nil then
    Result:= GetLogger('Error.Clazz.is.nil')
  else
    Result:= GetLogger(clazz.QualifiedClassName);
end;

{ TEsLogger }

procedure TEsLogger.Debug(const AFormat: string; const AArgs: array of const);
begin
  if IsDebugEnabled then
    DoLog(Log4D.Debug, Format(AFormat, AArgs));
end;

procedure TEsLogger.Info(const AFormat: string; const AArgs: array of const);
begin
  if IsInfoEnabled then
    DoLog(Log4D.Info, Format(AFormat, AArgs));
end;

procedure TEsLogger.Warn(const AFormat: string; const AArgs: array of const);
begin
  if IsWarnEnabled then
    DoLog(Log4D.Warn, Format(AFormat, AArgs));
end;

procedure TEsLogger.Error(const AFormat: string; const AArgs: array of const);
begin
  if IsErrorEnabled then
    DoLog(Log4D.Error, Format(AFormat, AArgs));
end;

function TEsLogger.GetChildLogger(childClass: TClass): TEsLogger;
begin
  mustBeTrue(Assigned(childClass), 'childClass is nil');
  Result := GetChildLogger(childClass.ClassName);
end;

function TEsLogger.GetChildLogger(const childName: string): TEsLogger;
begin
  EnsureNotEmpty(childName, 'childName');
  Result := TEsLogging.GetLogger(Format('%s.%s', [Name, childName]));
end;

procedure TEsLogger.LogException(const ex: Exception; const msg: string = '');
begin
  if not IsErrorEnabled then exit;
  if (ex = nil) then
  begin
    Warn('LogException called but ex=nil. msg = "%s"', [msg]);
    exit;
  end;
  DoLogException(ex, msg);
  Error('Exe: %s', [GetThisExeBuildInfo()]);
  Error('JCL: %d.%d.%d.%d', [JclVersionMajor, JclVersionMinor, JclVersionRelease, JclVersionBuild]);
end;

procedure TEsLogger.DoLogException(const ex: Exception; const msg: string);
var
  msg2: string;
  descriptions: TExceptionDescriptionArray;
  I: Integer;
  lastChar: char;
begin
  msg2 := Trim(msg);
  if msg2 <> '' then
  begin
    lastChar := msg2[msg2.Length];
    if CharInSet(lastChar, ['.', ';', ',', ':']) then
      msg2 := Format('%s ', [msg])
    else
      msg2:= Format('%s. ', [msg])
  end;
  Error('%s%s: %s', [msg2, Ex.ClassName, Ex.Message]);
  descriptions := CreateDetailedExceptionDescriptions(ex);
  for I:= Low(descriptions) to High(descriptions) do
    with descriptions[I] do
      if Trim(RDescription) <> '' then
        Error('%s:%s%s', [RHeader, sLineBreak, RDescription]);
  if Assigned(ex.InnerException) then
    DoLogException(ex.InnerException, 'INNER Exception Present!');
end;

procedure TEsLogger.LogException(const ex: Exception; const AFormat: string; const AArgs: array of const);
begin
  if IsErrorEnabled then
    LogException(ex, Format(AFormat, AArgs));
end;

initialization
// Enable raw mode (default mode uses stack frames which aren't always generated by the compiler)
  Include(JclStackTrackingOptions, stRawMode);
  // Disable stack tracking in dynamically loaded modules (it makes stack tracking code a bit faster)
  Include(JclStackTrackingOptions, stStaticModuleList);
  // Initialize Exception tracking
  JclStartExceptionTracking;

  RegisterLoggerFactory(TEsLogFactory);
  RegisterAppender(TEsLogRollingFileAppender);
  TEsLogFactory.Init();
  ThisExeBuildInfo := DoGetThisExeBuildInfo();//SmartRun может быть переименован прямо во время работы, и тогда мы не узнаем его версию
finalization
  TEsLogFactory.CleanUp();//все равно весь Log4D разрушается в finalization
  JclStopExceptionTracking;
end.
