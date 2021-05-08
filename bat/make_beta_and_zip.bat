echo called make_beta_and_zip.bat

@echo off
call timecmd.bat create_beta.bat
if errorlevel 3 (goto exit3)

REM Archive into 7z
call timecmd.bat 7zip.bat
if errorlevel 3 (goto exit3)

goto exit0

:exit3
exit /B 3

:exit0
@echo off
exit /B 0
