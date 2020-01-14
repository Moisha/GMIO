unit ComnDirs;

interface

uses Windows, SysUtils, Forms, shlobj;

{{Wrapper над SHGetFolderPath.
Возвращаемый Path без оканчивающего '\'}
function GetSpecialFolderPath(folder : integer) : string;

// утилиты: c:\ProgramData\Prosoft-Systems\EnergoSphere 7
function CommonAppDataPath() : string;
// и c:\ProgramData\Prosoft-Systems\EnergoSphere 7\ECollect (пример для ECollect.exe)
function CommonAppDataPathForExecutable() : string;

function MyDocumentsDir(): string;

{{Возращает путь (без оканчивающего '\') до папки для хранения документов для программы в каталоге документов пользователя (CSIDL_PERSONAL)
E.g. 'C:\Users\USERNAME\Documents\Prosoft-Systems\EnergoSphere 7\ECollect'}
function GetMyDocumentsPathForExecutable(): string;

{{Возращает путь (без оканчивающего '\') до папки для хранения артефактов программы в каталоге данных программы (CSIDL_APPDATA)
E.g. 'C:\Users\USERNAME\AppData\Roaming\Prosoft-Systems\EnergoSphere 7\ECollect'}
function GetAppDataPath(): string;

function CommonDocumentsDir(): string;

implementation

uses SHFolder;

var
  FCommonAppDataPath : string = '';
  FCommonAppDataPathForExecutable : string = '';

function GetSpecialFolderPath(folder : integer) : string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0..MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0,folder,0,SHGFP_TYPE_CURRENT,@path[0])) then
    Result := path
  else
    Result := '';
end;

function CommonDocumentsDir(): string;     
begin
  Result := GetSpecialFolderPath(CSIDL_COMMON_DOCUMENTS);
end;

function MyDocumentsDir(): string;
begin
  Result := GetSpecialFolderPath(CSIDL_PERSONAL);
end;

function GetProsoftEnergoSpherePath(csIdl: Integer): string;
begin
  Result:= Format('%sSitiusATM%s%s',
    [IncludeTrailingPathDelimiter(GetSpecialFolderPath(csIdl)), PathDelim,
     StringReplace(ExtractFileName(ParamStr(0)), ExtractFileExt(ParamStr(0)), '', [])]);
end;

function GetMyDocumentsPathForExecutable(): string;
begin
  Result := GetProsoftEnergoSpherePath(CSIDL_PERSONAL);
end;

function GetAppDataPath(): string;
begin
  Result := GetProsoftEnergoSpherePath(CSIDL_APPDATA);
end;

procedure InitPathName();
begin
  FCommonAppDataPath := GetSpecialFolderPath(CSIDL_COMMON_APPDATA) + '\SiriusATM\EN Online';
  if not ForceDirectories(FCommonAppDataPath) then
    FCommonAppDataPath := ''
  else begin
    FCommonAppDataPathForExecutable := FCommonAppDataPath + '\'
        + StringReplace(ExtractFileName(Application.ExeName), ExtractFileExt(Application.ExeName), '', []);
    if not ForceDirectories(FCommonAppDataPathForExecutable) then
      FCommonAppDataPathForExecutable := '';
  end;
end;

function CommonAppDataPath() : string;
begin
  if (FCommonAppDataPath = '') then
    InitPathName();
  Result := FCommonAppDataPath;
end;

function CommonAppDataPathForExecutable() : string;
begin
  if (FCommonAppDataPathForExecutable = '') then
    InitPathName();
  Result := FCommonAppDataPathForExecutable;
end;

end.
