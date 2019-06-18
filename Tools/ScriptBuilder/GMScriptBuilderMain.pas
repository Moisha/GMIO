unit GMScriptBuilderMain;

interface

uses
  Windows, SysUtils, Variants, Classes, IniFiles, GMGlobals, StrUtils;

  function ProcessCmdLineParameters(): int;

implementation

var
  slTemplateFrom, slTemplateTo: TSTringList;

procedure NormalizeConsoleOutput(const consoleOutput: string; slOutput: TStrings);
var
  s: string;
  i: int;
begin
  s := consoleOutput;
  for i := 1 to Length(s) do // радикально быстрее StringReplace
    if s[i] = #10 then
      s[i] := #13;

  slOutput.Text := s;
  for i := slOutput.Count - 1 downto 0 do
    if Trim(slOutput[i]) = '' then
      slOutput.Delete(i)
    else
      break;
end;

procedure CaptureConsoleOutput(const AParameters: String; slOutput: TStrings);
 const
   CReadBuffer = 2400;
   ACommand = 'git';
 var
   builder: TStringBuilder;
   saSecurity: TSecurityAttributes;
   hRead: THandle;
   hWrite: THandle;
   suiStartup: TStartupInfo;
   piProcess: TProcessInformation;
   pBuffer: array[0..CReadBuffer] of AnsiChar;
   dRead: DWord;
   dRunning: DWord;
 begin
   slOutput.Clear();
   builder := TStringBuilder.Create();
   try
     ZeroMemory(@saSecurity, SizeOf(TSecurityAttributes));
     saSecurity.nLength := SizeOf(TSecurityAttributes);
     saSecurity.bInheritHandle := True;
     saSecurity.lpSecurityDescriptor := nil;

     if CreatePipe(hRead, hWrite, @saSecurity, 0) then
     begin
       FillChar(suiStartup, SizeOf(TStartupInfo), #0);
       suiStartup.cb := SizeOf(TStartupInfo);
       suiStartup.hStdInput := hRead;
       suiStartup.hStdOutput := hWrite;
       suiStartup.hStdError := hWrite;
       suiStartup.dwFlags := STARTF_USESTDHANDLES;
       suiStartup.wShowWindow := SW_HIDE;

       if CreateProcess(nil, PChar(ACommand + ' ' + AParameters), @saSecurity, @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess)  then
       begin
         CloseHandle(hWrite); // иначе зависает на считывании последне порции
         repeat
           dRunning  := WaitForSingleObject(piProcess.hProcess, 100);
           repeat
             dRead := 0;
             ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
             pBuffer[dRead] := #0;

             OemToAnsi(pBuffer, pBuffer);
             builder.Append(String(pBuffer));
           until (dRead < CReadBuffer);
         until (dRunning <> WAIT_TIMEOUT);
         CloseHandle(piProcess.hProcess);
         CloseHandle(piProcess.hThread);
       end;

       CloseHandle(hRead);
     end;

     NormalizeConsoleOutput(Builder.ToString(), slOutput);
   finally
     builder.Free();
   end;
end;

function RevisionCount(): int;
var
  sl: TSTringList;
  s: string;
begin
  Result := 0;
  sl := TSTringList.Create();
  try
    CaptureConsoleOutput('rev-list head --count', sl);
    if sl.Count <> 1 then
      raise Exception.Create('rev-list head --count failed. git: ' + sl.Text);

    s := sl[0].Trim([' ', #13, #10]);
    Result := StrToIntDef(s, -1);
    if Result <= 0 then
      raise Exception.Create('rev-list head --count failed. git: ' + sl.Text);
  finally
    sl.Free();
  end;
end;

function DirectoryRevisionSha(const path: string): string;
var
  sl: TSTringList;
begin
  sl := TSTringList.Create();
  try
    CaptureConsoleOutput('rev-list head --max-count=1 -- ' + path, sl);
    if sl.Count <> 1 then
      raise Exception.Create('rev-list "' + path + '" faled. git: ' + sl.Text);

    Result := sl[0].Trim([' ', #13, #10]);
    if Length(Result) <> 40 then
      raise Exception.Create('rev-list "' + path + '" faled. git: ' + sl.Text);
  finally
    sl.Free();
  end;
end;

function FindRevision(const sha: string): int;
var
  sl: TSTringList;
begin
  Result := 0;
  sl := TSTringList.Create();
  try
    CaptureConsoleOutput('rev-list head', sl);
    if sl.Count = 0 then
      raise Exception.Create('rev-list head failed');

    Result := sl.IndexOf(sha);
  finally
    sl.Free();
  end;
end;

function ReadHeadRevision(const directory: string): int;
var
  root, path, sha: string;
  cnt, n: int;
begin
  // SHA последнего коммита для указанной папки: git rev-list head --max-count=1 -- D:\Programs\Delphi\Geomer\GMIO_git\DB\Builder
  // список SHA для всех коммитов в репозиторий: git rev-list head
  // общее к-во коммитов в репозиторий: git rev-list head --count

  // Т.о. алгоритм следующий
  // 1. Получаем SHA для последнего коммита в directory
  // 2. Получаем к-во коммитов в корень (CNT)
  // 3. Получаем список коммитов в корень
  // 4. Находим наш последний коммит в общем списке (N)
  // 5. Получаем номер как CNT - N + 912, где 912 - это номер последней ревизии в SVN

  root := ExpandFileName(ExcludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)))+ '\..\..\');
  path := root + directory;

  cnt := RevisionCount();
  sha := DirectoryRevisionSha(path);
  n := FindRevision(sha);

  Result := cnt - n + 912;
end;

function CheckFileTemplate(sl: TStringList; var fnTemplate: string): bool;
var i, n: int;
begin
  Result := false;

  for i := 0 to sl.Count - 1 do
  begin
    n := Pos('pg_gm_db_template_from_file', sl[i]);
    if n > 0 then
    begin
      Result := true;
      fnTemplate := Trim(Copy(sl[i], PosEx(' ', sl[i], n) + 1, Length(sl[i])));
    end;
  end;
end;

procedure ProcessFKTemplate(const fileString: string);
var args: TStringList;
    n, n1, n2: int;
    table, reftable, column, s, refcolumn: string;
    isCascade: bool;
begin
  n := Pos('pg_gm_db_template_foreign_key', fileString);
  if n <= 0 then Exit;

  args := TStringList.Create();
  args.Delimiter := ' ';
  args.DelimitedText := fileString;

  if args.Count >= 4 then // pg_gm_db_template_foreign_key table column reftable[(refcolumn)]
  begin
    table := args[1];
    column := args[2];
    reftable := args[3];
    refcolumn := column;
    if pos('(', reftable) > 0 then
    begin
      n1 := pos('(', reftable);
      n2 := pos(')', reftable);
      if n2 > 0 then
      begin
        refcolumn := Copy(refTable, n1 + 1, n2 - n1 - 1);
        reftable := Copy(refTable, 1, n1 - 1);
      end
      else
      begin
        refColumn := 'Error in declaration: missin ")"';
      end;
    end;
    isCascade := args.Count > 4;

    s := StringOfChar(' ', n - 1) + 'if not exists(select * from pg_constraint where conname = ''' + LowerCase(table + '_' + column) + '_fkey'') then '#10 +
         StringOfChar(' ', n - 1) + '  update ' + table + ' set ' + column + ' = null where ' + column + ' <= 0;'#10 +
         StringOfChar(' ', n - 1) + Format('  delete from %s where %s is not null and %s not in (select %s from %s);'#10, [table, column, column, refcolumn, reftable]) +
         StringOfChar(' ', n - 1) + '  alter table ' + table + ' add foreign key (' + column + ') references ' + reftable + '(' + refColumn + ')';


    if isCascade then
      s := s + ' on delete ' + args.DelimitedRande(4, args.Count);

    s := s + ';'#10 +
         StringOfChar(' ', n - 1) + 'end if;';

    slTemplateFrom.Add(fileString);
    slTemplateTo.Add(s + #10);
  end;

  args.Free();
end;

procedure ProcessAddColumnTemplate(const fileString: string);
var args: TStringList;
    n: int;
    s, table, column, datatype: string;
begin
  n := Pos('pg_gm_db_template_add_column', fileString);
  if n <= 0 then Exit;

  args := TStringList.Create();
  args.Delimiter := ' ';
  args.DelimitedText := fileString;

  if args.Count >= 4 then // pg_gm_db_template_add_column table column type
  begin
    table := args[1];
    column := args[2];
    datatype := args.DelimitedRande(3, args.Count);

    s := StringOfChar(' ', n - 1) + 'if not exists (select * from information_schema.columns where table_name = lower(''' + table + ''') and column_name = lower(''' + column + ''')) then'#10 +
         StringOfChar(' ', n - 1) + '  alter table ' + table + ' add ' + column + ' ' + datatype +';'#10 +
         StringOfChar(' ', n - 1) + 'end if;';

    slTemplateFrom.Add(fileString);
    slTemplateTo.Add(s + #10);
  end;

  args.Free();
end;

procedure ProcessStringReplaceTemplate(const fileString: string);
var n, n1, n2: int;
    sFrom, sTo: string;
begin
  n := Pos('pg_gm_db_template_str_replace', fileString);
  if n > 0 then
  begin
    n1 := PosEx(' ', fileString, n);
    n2 := PosEx(' ', fileString, n1 + 1);
    if n2 = 0 then
      n2 := Length(fileString) + 1;

    sFrom := Copy(fileString, n1 + 1, n2 - n1 - 1);
    sTo := TrimRight(Copy(fileString, n2 + 1, Length(fileString)));
    slTemplateFrom.Add(sFrom);
    slTemplateTo.Add(sTo);
  end;
end;

procedure PrepareTemplates(sl: TStringList);
var i: int;
begin
  slTemplateFrom.Clear();
  slTemplateTo.Clear();

  for i := 0 to sl.Count - 1 do
  begin
    ProcessStringReplaceTemplate(sl[i]);
    ProcessFKTemplate(sl[i]);
    ProcessAddColumnTemplate(sl[i]);
  end;
end;

function ApplyTemplates(const s: string): string;
var i, n: int;
    sTo: string;
begin
  Result := s;
  for i := 0 to slTemplateFrom.Count - 1 do
  begin
    sTo := slTemplateTo[i];
    if sTo = '$HEAD_REVISION$' then
    begin
      n := ReadHeadRevision('DB/Builder');
      if n <= 0 then
        raise Exception.Create('Не удалось найти головную ревизию!');

      sTo := IntToStr(n);
    end;

    Result := StringReplace(Result, slTemplateFrom[i], sTo, [rfIgnoreCase, rfReplaceAll]);
  end;
end;

procedure BuildScript();
var lstFiles: TStringList;
    script: TFileStream;
    str: TStringStream;
    slFile: TStringList;
    i, j: int;
    path, resFile, fnTemplate: string;
    header: array [0..2] of Byte;
    slUsedFiles: TStringList;

  procedure LoadPart(fn: string);
  var part: TFileStream;
  begin
    slUsedFiles.Add(ExtractFileName(fn));
    part := TFileStream.Create(fn, fmOpenRead);
    part.Seek(3, soFromBeginning); // пропускаем кодировку, она везде и так UTF8
    slFile.LoadFromStream(part);
    part.Free();
  end;

  procedure CheckAllFilesUsed();
  var res, n: int;
      rec: TSearchRec;
      errfiles: string;
  begin
    errfiles := '';
    res := FindFirst(path + '*.sql', faAnyFile, rec);
    while res = 0 do
    begin
      if (rec.Name <> '.') and (rec.Name <> '..') then
      begin
        n := slUsedFiles.IndexOf(rec.Name);
        if  n < 0 then
          errfiles := errFiles + rec.Name + #13#10;
      end;
      res := FindNext(rec);
    end;
    FindClose(rec);

    if errfiles <> '' then
      raise Exception.Create('Files not used: ' + errfiles);
  end;

begin
  if ParamCount() < 3 then
    raise Exception.Create('Not enough parameters!');

  path := IncludeTrailingPathDelimiter(ParamStr(2));
  resFile := ParamStr(3);

  lstFiles := TStringList.Create();
  str := TStringStream.Create('');
  slFile := TStringList.Create();
  slUsedFiles := TStringList.Create();

  try
    lstFiles.LoadFromFile(path + '_script.ini');
    for i := 0 to lstFiles.Count - 1 do
    begin
      if Trim(lstFiles[i]) = '' then continue;

      if (Copy(lstFiles[i], 1, 2) = '//') or (Copy(lstFiles[i], 1, 2) = '--') then // Comments
      begin
        str.WriteString('--' + Copy(lstFiles[i], 3, Length(lstFiles[i])));
        str.WriteString(#10);
      end
      else
      begin
        LoadPart(path + lstFiles[i] + '.sql');
        PrepareTemplates(slFile);
        if CheckFileTemplate(slFile, fnTemplate) then
          LoadPart(path + fnTemplate + '.sql');

        for j := 0 to slFile.Count - 1 do
        begin
          str.WriteString(ApplyTemplates(slFile[j]));
          str.WriteString(#10);
        end;
      end;

      if i < lstFiles.Count - 1 then
        str.WriteString(#10);
    end;

    script := TFileStream.Create(resFile, fmCreate);

    header[0] := $EF;
    header[1] := $BB;
    header[2] := $BF;
    script.Write(header, length(header));

    str.Seek(0, soFromBeginning);
    script.CopyFrom(str, str.Size);

    script.Free();

    CheckAllFilesUsed();
  finally
    lstFiles.Free();
    slFile.Free();
    str.Free();
    slUsedFiles.Free();
  end;
end;

procedure InsertVersion();
var resFileName: string;
    baseVersions: TIniFile;
    major, minor, subminor, revision, scriptRevision: int;
    sl: TStringList;
    i: int;
    sha: string;
begin
  if ParamCount() < 2 then
    raise Exception.Create('Not enough parameters!');

  resFileName := ParamStr(2);

  baseVersions := TIniFile.Create('..\..\Build\version.ini');
  try
    major := baseVersions.ReadInteger('VERSION', 'MAJOR', -1);
    minor := baseVersions.ReadInteger('VERSION', 'MINOR', -1);
    subminor := baseVersions.ReadInteger('VERSION', 'SUBMINOR', -1);
  finally
    baseVersions.Free();
  end;

  revision := ReadHeadRevision('');
  scriptRevision := ReadHeadRevision('DB/Builder');

  if (major < 1) or (minor < 0) or (subminor < 0) then
    raise Exception.Create('Wrong version file contents!');

  if (revision < 1) then
    raise Exception.Create('Wrong revision!');

  sha := DirectoryRevisionSha('');

  sl := TStringList.Create();
  try
    sl.LoadFromFile(resFileName);
    for i := 0 to sl.Count - 1 do
    begin
      sl[i] := StringReplace(sl[i], '$MAJOR$', IntToStr(major), [rfReplaceAll]);
      sl[i] := StringReplace(sl[i], '$MINOR$', IntToStr(minor), [rfReplaceAll]);
      sl[i] := StringReplace(sl[i], '$SUBMINOR$', IntToStr(subminor), [rfReplaceAll]);
      sl[i] := StringReplace(sl[i], '$WCREV$', IntToStr(revision), [rfReplaceAll]);
      sl[i] := StringReplace(sl[i], '$SCRIPTREV$', IntToStr(scriptRevision), [rfReplaceAll]);
      sl[i] := StringReplace(sl[i], '$DATETIME$', FormatDateTime('dd.mm.yyyy hh:nn:ss', Now()), [rfReplaceAll]);
      sl[i] := StringReplace(sl[i], '$GITSHA$', sha, [rfReplaceAll]);
    end;

    sl.SaveToFile(resFileName);
  finally
    sl.Free();
  end;
end;

type
  TCheckAllFilesUnderSVN = class
  private
    FMissingFiles, FExternals: TStringList;
    FRepoFiles: THashedStringList;

    procedure ProcessDirectory(const dir: string);
    procedure CheckSVN;
    constructor Create();
    destructor Destroy(); override;
    procedure CheckFile(const fn: string);
    procedure ReadRepoFilesList;
    procedure CreateExternalsList;
  end;

constructor TCheckAllFilesUnderSVN.Create();
begin
  inherited;
  FMissingFiles := TStringList.Create();
  FExternals := TStringList.Create();
  FRepoFiles := THashedStringList.Create();
end;

destructor TCheckAllFilesUnderSVN.Destroy();
begin
  FMissingFiles.Free();
  FExternals.Free();
  FRepoFiles.Free();
  inherited;
end;

procedure TCheckAllFilesUnderSVN.CheckFile(const fn: string);
var
  ext: string;
begin
  ext := ExtractFileExt(fn).ToLower();
  if (ext <> '.sql') and (ext <> '.pas') and (ext <> '.dfm') then Exit;

  if FRepoFiles.IndexOf(AnsiLowerCase(fn)) < 0 then
    FMissingFiles.Add(fn);
end;

procedure TCheckAllFilesUnderSVN.ProcessDirectory(const dir: string);
var rec: TSearchRec;
    res: int;
    path, fn: string;
begin
  if not DirectoryExists(dir) then
    raise Exception.Create('Directory not found ' + dir);

  if FExternals.IndexOf(UpperCase(ExcludeTrailingPathDelimiter(dir))) >= 0 then Exit;

  path := IncludeTrailingPathDelimiter(dir);
  res := FindFirst(path + '*.*', faDirectory, rec);
  while res = 0 do
  begin
    if (rec.Name <> '.') and (rec.Name <> '..') then
    begin
      if (rec.Attr and faDirectory) > 0 then
      begin
        fn := IncludeTrailingPathDelimiter(path + rec.Name);
        ProcessDirectory(fn)
      end
      else
      begin
        fn := path + rec.Name;
        CheckFile(fn);
      end;
    end;
    res := FindNext(rec);
  end;
  FindClose(rec);
end;

procedure TCheckAllFilesUnderSVN.ReadRepoFilesList();
var
  i: int;
  path: string;
begin
  CaptureConsoleOutput('ls-tree --full-tree -r --name-only HEAD', FRepoFiles);
  path := ExpandFileName('..\..\');
  for i := 0 to FRepoFiles.Count - 1 do
    FRepoFiles[i] := AnsiLowerCase(path + StringReplace(FRepoFiles[i], '/', '\', [rfReplaceAll]));
end;

procedure TCheckAllFilesUnderSVN.CreateExternalsList();
begin
end;

procedure TCheckAllFilesUnderSVN.CheckSVN();
var i: int;
begin
  ReadRepoFilesList();
  CreateExternalsList();

  for i := 2 to ParamCount do
    ProcessDirectory(ExpandFileName('..\..\') + ParamStr(i));
end;

function CheckSVN(): int;
var cf: TCheckAllFilesUnderSVN;
begin
  cf := TCheckAllFilesUnderSVN.Create();
  try
    cf.CheckSVN();
    Result := 0;
    if cf.FMissingFiles.Count > 0 then
    begin
      Writeln('Files not under SVN: ' + cf.FMissingFiles.Text);
      Result := 1;
    end;
  finally
    cf.Free();
  end;
end;

function ProcessCmdLineParameters(): int;
begin
  Result := 0;
  try
    if ParamStr(1) = 'script' then
      BuildScript()
    else
    if ParamStr(1) = 'version' then
      InsertVersion()
    else
    if ParamStr(1) = 'checksvn' then
      Result := CheckSVN()
    else
    begin
      Writeln('Uncnown command: ' + ParamStr(1));
      Result := 1;
    end;                                       
  except
    on e: Exception do
    begin
      try
        Writeln('Script build error: ' + e.Message);
      except end;
      Result := 1;
    end;
  end;
end;

initialization
  slTemplateFrom := TSTringList.Create();
  slTemplateTo := TSTringList.Create();

finalization
  slTemplateFrom.Free();
  slTemplateTo.Free();

end.
