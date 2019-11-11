del out.txt
cd ..

set PROJECTSROOT=%CD%
set ERR=0
set LOGFILE=%PROJECTSROOT%\build\out.txt
set RELEASEDIR=%PROJECTSROOT%\Release

if exist "%ProgramFiles(x86)%\Embarcadero\Studio\20.0\bin\dcc32.exe" set DELPHIBIN=%ProgramFiles(x86)%\Embarcadero\Studio\20.0\bin
call "%DELPHIBIN%\rsvars.bat"

echo on
echo %DELPHIBIN% >> %LOGFILE%

call :checkdir %RELEASEDIR%
call :checkdir %RELEASEDIR%\Client
call :checkdir %RELEASEDIR%\Server
call :checkdir %RELEASEDIR%\Tools
call :checkdir %RELEASEDIR%\DB
call :checkdir %RELEASEDIR%\OPC
call :checkdir %RELEASEDIR%\WatchDog

echo Build start >> %LOGFILE%
echo ---------------------- >> %LOGFILE%

call :delshit

del /S /Q %PROJECTSROOT%\Release\*.exe
del /S /Q %PROJECTSROOT%\Release\*.dll
del /S /Q %PROJECTSROOT%\Release\*.sql

if exist %PROJECTSROOT%\DB\pg_gm_db.sql (
  echo Back up pg_gm_db >> %LOGFILE%
  del %PROJECTSROOT%\DB\pg_gm_db.bak
  call :copyfile %PROJECTSROOT%\DB\pg_gm_db.sql %PROJECTSROOT%\DB\pg_gm_db.bak
)
del %PROJECTSROOT%\DB\pg_gm_db.sql

call :buildprog %PROJECTSROOT%\ThirdParty\JEDI\jcl\examples\windows\debug\tools MakeJclDbg

call :buildprog %PROJECTSROOT%\Tools\ScriptBuilder GMScriptBuilder
cd %PROJECTSROOT%\Tools\ScriptBuilder\  
GMScriptBuilder.exe script "%PROJECTSROOT%\DB\Builder" "%PROJECTSROOT%\DB\pg_gm_db.sql"  >> %LOGFILE% 
set RESULT=%ERRORLEVEL%
call :checkresult %RESULT% "Script build OK" "Script build ERROR" 

GMScriptBuilder.exe checksvn "Client" "DB\Builder" "Misc" "OPC" "Server" "Service" "SharedSrc" "Test" "ThirdParty" "Tools" "WatchDog" >> %LOGFILE% 
set RESULT=%ERRORLEVEL%
call :checkresult %RESULT% "SVN OK" "SVN ERROR" 

call :ScriptVersion Server
call :ScriptVersion Service

call :buildprog %PROJECTSROOT%\Server GMIOPSrv "%RELEASEDIR%\Server" 1
if %RESULT%==0 (
  call :copyfile %PROJECTSROOT%\Server\libeay32.dll %RELEASEDIR%\Server
  call :copyfile %PROJECTSROOT%\Server\libpq.dll %RELEASEDIR%\Server
)
call :buildprog %PROJECTSROOT%\Client GMPClient "%RELEASEDIR%\Client" 1
call :buildprog %PROJECTSROOT%\OPC GMOPC "%RELEASEDIR%\OPC" 1

del "%PROJECTSROOT%\Service\uac.manifest"
call :copyfile "%PROJECTSROOT%\Service\uac.manifest.in" "%PROJECTSROOT%\Service\uac.manifest"
call :insertversion "%PROJECTSROOT%\Service\uac.manifest"
call :buildprog %PROJECTSROOT%\Service GMIOPSvc "%RELEASEDIR%\Server" 1

call :buildprog %PROJECTSROOT%\Tools\Terminal GMCOMTerminal "%RELEASEDIR%\Tools" 1 
call :buildprog %PROJECTSROOT%\Tools\GMCfg GMCfg "%RELEASEDIR%\Tools"
call :buildprog %PROJECTSROOT%\Tools\LogReader GMLogReader "%RELEASEDIR%\Tools" 1
call :buildprog %PROJECTSROOT%\WatchDog GMWatchDog "%RELEASEDIR%\WatchDog"

call :copyfile %PROJECTSROOT%\DB\pg_gm_db.sql %RELEASEDIR%\DB
call :copyfile %PROJECTSROOT%\DB\POneParam.sql %RELEASEDIR%\DB

rem батники для управления сервисом
echo GMIOPSvc.exe /stop > "%RELEASEDIR%\Server\SvcStop.bat"
echo GMIOPSvc.exe /start > "%RELEASEDIR%\Server\SvcStart.bat"
echo GMIOPSvc.exe /install > "%RELEASEDIR%\Server\SvcInstall.bat"
echo GMIOPSvc.exe /uninstall > "%RELEASEDIR%\Server\SvcDelete.bat"

cd %PROJECTSROOT%
del /S /Q Release\*.map 

if exist "%PROJECTSROOT%\build\post_build.bat" (
  echo "%PROJECTSROOT%\build\post_build.bat" >> %LOGFILE%
  call "%PROJECTSROOT%\build\post_build.bat" >> %LOGFILE% 
  set RESULT=%ERRORLEVEL%
  call :checkresult %RESULT% "post_build.bat OK" "post_build.bat ERROR" 
)

:end

cd %PROJECTSROOT%\build
echo. >> %LOGFILE%
echo ---------------------- >> %LOGFILE%
call :checkresult %ERR% "Build OK" "!!!!! Build FAILED. Check log for details !!!!!" 
echo ---------------------- >> %LOGFILE%

start out.txt
exit /b

:ScriptVersion Service
rem %1 - project dir
del /S /Q "%PROJECTSROOT%\%1\ScriptVersion.inc"
call :copyfile "%PROJECTSROOT%\%1\ScriptVersion.inc.in" "%PROJECTSROOT%\%1\ScriptVersion.inc"
call :insertversion "%PROJECTSROOT%\%1\ScriptVersion.inc" 1
exit /b

:copyfile
rem %1 - from
rem %2 - to
echo. >> %LOGFILE%
echo copy %1 -^> %2 >> %LOGFILE%
echo ---------------------- >> %LOGFILE%
copy %1 %2 >> %LOGFILE%
set RESULT=%ERRORLEVEL%
call :checkresult %RESULT% OK "COPY ERROR" 
exit /b

:buildprog
rem %1 - project directory
rem %2 - project name
rem %3 - output directory
rem %4 - 1: need compile resource
call :delshit

echo. >> %LOGFILE%
echo %2 >> %LOGFILE%
echo ---------------------- >> %LOGFILE%
cd %1
del %2.exe

if not "%4"=="1" goto compile

del %2.res
del %2.rc

copy %2.rc.in %2.rc
set RESULT=%ERRORLEVEL%
call :checkresult %RESULT% "Copy rc.in OK" "_____ resource ERROR _____"
if not %RESULT%==0 Exit /b

call :insertversion %1\%2.rc
if not %RESULT%==0 Exit /b

:compile
cd %1
msbuild /p:Config=Release;Platform=Win32%OUTDIR% %2.dproj >> %LOGFILE%
set RESULT=%ERRORLEVEL%
call :checkresult %RESULT% "compile OK" "_____ compile ERROR _____"
if not %RESULT%==0 Exit /b

if exist "%1\%2.map" (
  echo MakeJclDbg %2 >> %LOGFILE%
  %PROJECTSROOT%\ThirdParty\JEDI\jcl\bin\MakeJclDbg.exe -E "%1\%2.map" 
  set RESULT=%ERRORLEVEL%
  call :checkresult %RESULT% "MakeJclDbg OK" "_____ MakeJclDbg ERROR _____"
)
if not "%3"=="" call :copyfile "%2.exe" %3

rem cd %3
rem "%PROJECTSROOT%\build\upx.exe" -5 -v %2.exe >> %LOGFILE%
rem set RESULT=%ERRORLEVEL%
rem call :checkresult %RESULT% "upx OK" "_____ upx ERROR _____"

:compile_end
call :delshit
exit /b

:insertversion
rem %1 - file name
cd %PROJECTSROOT%\Tools\ScriptBuilder\  
GMScriptBuilder.exe version %1 >> %LOGFILE% 
set RESULT=%ERRORLEVEL%
call :checkresult %RESULT% "Insert version OK" "_____ Insert version ERROR _____" 
exit /b

:checkresult
rem %1 - error level
rem %2 - OK text
rem %3 - error text
if %1==0 (
  echo %~nx2 >> %LOGFILE%
) else (
  echo %~nx3 >> %LOGFILE% 
  SET ERR=1
)
exit /b

:delshit
cd %PROJECTSROOT%
del /S /Q *.dcu
del /S /Q *.ddp
del /S /Q *.~pas
del /S /Q *.~dfm
del /S /Q *.~ddp 

exit /b

:checkdir
rem %1 - directory
if not exist %1\nul (
  mkdir %1
  echo Directory %1 created >> %LOGFILE%
)
exit /b


