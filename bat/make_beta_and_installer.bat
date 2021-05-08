@echo off

@REM we could use timer util made by https://www.gammadyne.com/cmdline.htm#timer
@REM if "%~1"=="-timer" (
@REM   call timer.exe /nologo 
@REM )

@REM or we could measure execution time in seconds via next calls
@REM @set /A _tic=%time:~0,2%*3600^+%time:~3,1%*10*60^+%time:~4,1%*60^+%time:~6,1%*10^+%time:~7,1% >nul

@REM let's use timecmd.bat to measure execution time 
call timecmd.bat create_beta.bat

if errorlevel 2 (goto exit2)

@REM @set /A _toc=%time:~0,2%*3600^+%time:~3,1%*10*60^+%time:~4,1%*60^+%time:~6,1%*10^+%time:~7,1% >nul
@REM @set /A _elapsed=%_toc%-%_tic
@REM @echo %_elapsed% seconds.


@REM if "%~1"=="-timer" (
@REM   @echo #
@REM   echo Create Beta took:
@REM   call "timer.exe" /nologo /r
@REM   
@REM   @echo Create Beta took: > build_time.txt
@REM   @call "timer.exe" /nologo /r >> build_time.txt
@REM )

call prepare_for_installer.bat

@echo #                                                          # 
@echo #==========================================================#
@echo #             Start Inno Setup compilation                 #  
@echo #==========================================================# 
@echo #  
@echo #

call timecmd.bat "%ISC_DIR%"\iscc.exe /Q "..\Installer\InstallerFull.iss" 

@REM if "%~1"=="-timer" (
@REM   echo Inno Setup compilation took:
@REM   call "timer.exe" /nologo /S
@REM   
@REM   @echo #
@REM   @echo --------------------------------------------------------- >> build_time.txt 
@REM   @echo #
@REM   @echo Inno Setup compilation took: >> build_time.txt
@REM   @call "timer.exe" /nologo /r >> build_time.txt
@REM )

@echo #
@echo #----------------------------------------------------------#
@echo #                Compilation completed !                   #  
@echo #----------------------------------------------------------#

goto exit0

:exit2
exit /B 2

:exit0
@echo off
exit /B 0
