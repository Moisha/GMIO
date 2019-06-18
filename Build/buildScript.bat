echo off
cd ..
set PROJECTSROOT=%CD%
set ERR="0"
cd %PROJECTSROOT%\Tools\ScriptBuilder\  
GMScriptBuilder.exe script "%PROJECTSROOT%\DB\Builder" "%PROJECTSROOT%\DB\pg_gm_db.sql" 
set RESULT=%ERRORLEVEL%
if not "%RESULT%"=="0" set ERR="1"

GMScriptBuilder.exe checksvn "Client" "DB\Builder" "Misc" "OPC" "Server" "Service" "SharedSrc" "Test" "ThirdParty" "Tools" "WatchDog"
set RESULT=%ERRORLEVEL%
if not "%RESULT%"=="0" set ERR="1"

if %ERR%=="0" goto end

echo "ERR" %ERR%
echo Error!
pause
:end
exit /b