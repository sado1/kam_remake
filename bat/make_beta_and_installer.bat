@echo off

if "%~1"=="-timer" (
  call timer.exe /nologo /n /r
)

call create_beta.bat

if errorlevel 2 (goto exit2)

if "%~1"=="-timer" (
  call "timer.exe" /nologo /n /r
  @REM call "timer.exe"
)

call prepare_for_installer.bat


@echo #                                                          # 
@echo #==========================================================#
@echo #             Start Inno Setup compilation                 #  
@echo #==========================================================# 
@echo #  
@echo .                                                       

call "%ISS_DIR%"\iscc.exe /Q "..\Installer\InstallerFull.iss" 

if "%~1"=="-timer" (
  call "timer.exe" /nologo /S
)

@echo .
@echo #----------------------------------------------------------#
@echo #                Compilation completed !                   #  
@echo #----------------------------------------------------------#

goto exit0

:exit2
exit /B 2

:exit0
@echo off
exit /B 0
