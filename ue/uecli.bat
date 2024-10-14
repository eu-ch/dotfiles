@echo off

rem User-configurable
set PROJ_NAME=Kvlt
set BUILD_CONFIG=DebugGame
set ENGINE_PATH=d:\code\UE_427

set PROJ_NAME_EDITOR=%PROJ_NAME%Editor

rem Files and directories
set PROJ_DIR=%~dp0
set PROJ_DIR=%PROJ_DIR:~0,-1%
set UPROJ_PATH=%PROJ_DIR%\%PROJ_NAME%.uproject

rem Executables
set BUILD_BAT=%ENGINE_PATH%\Engine\Build\BatchFiles\Build.bat
set UBT_EXE=%ENGINE_PATH%\Engine\Binaries\DotNET\UnrealBuildTool.exe
set UE_EXE=%ENGINE_PATH%\Engine\Binaries\Win64\UE4Editor-Win64-%BUILD_CONFIG%.exe

rem Validations
set PASSED_VALIDATION=1

if not exist "%ENGINE_PATH%" (
  set PASSED_VALIDATION=0
  echo Error: Unreal Engine directory ^"%ENGINE_PATH%^" does not exist - please set ENGINE_PATH variable correctly
)
if not exist "%UPROJ_PATH%" (
  set PASSED_VALIDATION=0
  echo Error: Project file ^"%UPROJ_PATH%^" does not exist - please set PROJ_NAME variable correctly
)
if not %PASSED_VALIDATION%==1 goto Exit

rem ---------------------------------------------------

:Parse
if "%~1"=="" goto Help
if "%~1"=="BuildEditor" goto BuildEditor
if "%~1"=="GenerateClangDB" goto GenerateClangDB
if "%~1"=="GenerateDefaultProject" goto GenerateDefaultProject
if "%~1"=="GenerateVSCodeProject" goto GenerateVSCodeProject
if "%~1"=="RunEditor" goto RunEditor
goto UnknownArgument

:BuildEditor
%BUILD_BAT% %PROJ_NAME_EDITOR% Win64 %BUILD_CONFIG% -project=%UPROJ_PATH%
goto :Exit

:GenerateClangDB
rem If you see the following error:
rem ERROR: Clang must be installed in order to build this target.
rem Define LLVM_PATH environment variable so that UBT knows where to look for clang executables.
echo Generating Unreal headers
call %BUILD_BAT% %PROJ_NAME_EDITOR% Win64 %BUILD_CONFIG% -project=%UPROJ_PATH% -game -engine -SkipBuild
echo Generating compile_commands.json
%UBT_EXE% %PROJ_NAME_EDITOR% Win64 %BUILD_CONFIG% -mode=GenerateClangDatabase -project=%UPROJ_PATH% -game -engine
echo %UE_PATH%\compile_commands.json
copy /Y %ENGINE_PATH%\compile_commands.json %PROJ_DIR%
goto :Exit

:GenerateDefaultProject
%UBT_EXE% GenerateProjectFiles -Game %UPROJ_PATH% -projectfiles -CurrentPlatform
goto :Exit

:GenerateVSCodeProject
echo Error: not implemented yet
goto :Exit

:RunEditor
%UE_EXE% %UPROJ_PATH%
goto :Exit

:UnknownArgument
echo Unknown argument: %~1
goto Help

:Help
echo Usage: uecli.bat (BuildEditor^|GenerateClangDB^|GenerateDefaultProject^|GenerateVSCodeProject^|RunEditor)
goto :Exit

:Exit

