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

@REM add, commit and push changes to github
git reset
git add ..\KM_Revision.inc
git add ..\Installer\Revision.iss
git commit -m "r%kam_revision%: update revision number"
git push
@REM create revision tag and push it to remote origin
git tag -a r%kam_revision% -m "r%kam_revision%: update revision number"
git push origin r%kam_revision%

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
