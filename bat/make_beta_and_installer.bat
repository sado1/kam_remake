@echo off

@REM using timer util made by https://www.gammadyne.com/cmdline.htm#timer
if "%~1"=="-timer" (
  call timer.exe /nologo 
)

call create_beta.bat

if errorlevel 2 (goto exit2)

if "%~1"=="-timer" (
  @echo #
  echo Create Beta took:
  call "timer.exe" /nologo /r
  
  @echo Create Beta took: > build_time.txt
  @call "timer.exe" /nologo /r >> build_time.txt
)

call prepare_for_installer.bat

@echo #                                                          # 
@echo #==========================================================#
@echo #             Start Inno Setup compilation                 #  
@echo #==========================================================# 
@echo #  
@echo #

call "%ISC_DIR%"\iscc.exe /Q "..\Installer\InstallerFull.iss" 

if "%~1"=="-timer" (
  echo Inno Setup compilation took:
  call "timer.exe" /nologo /S
  
  @echo #
  @echo --------------------------------------------------------- >> build_time.txt 
  @echo #
  @echo Inno Setup compilation took: >> build_time.txt
  @call "timer.exe" /nologo /r >> build_time.txt
)

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
