@echo off
echo called build_exe.bat

@REM ============================================================
@REM Build main exe
@REM ============================================================

@REM msbuild needs variables set by rsvars
call rsvars.bat

REM build
msbuild ..\KaM_Remake.dproj /p:config=Release /target:Build /p:platform=Win32 /clp:ErrorsOnly /fl /flp:LogFile="build_exe.log"

REM build
REM msbuild utils\ScriptValidator\ScriptValidator.dproj /p:Configuration=Debug /t:Build /clp:ErrorsOnly /fl /flp:LogFile="build_ScriptValidator_exe.log"