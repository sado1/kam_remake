@echo off
echo called update_revision.bat

if defined UPDATE_REV (
if %UPDATE_REV%==True (
goto exit0
)
)

@SET UPDATE_REV=True

@REM
call update_revision_locally.bat

call tag_and_push_revision.bat

@echo !
@echo !
@echo ================================================================================
@echo ######     Rebuild KaM_Remake.exe, as revision number was changed !!!!!    #####
@echo ================================================================================
@echo !  
@echo !


:exit0
@echo off
exit /B 0
