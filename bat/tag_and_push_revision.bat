@echo off
echo called tag_and_push_revision.bat

@REM add, commit and push changes to github
git reset
git add ..\KM_Revision.inc
git add ..\Installer\Revision.iss
git commit -m "r%kam_revision%: update revision number"
git push
@REM create revision tag and push it to remote origin
git tag -a r%kam_revision% -m "r%kam_revision%: update revision number"
git push origin r%kam_revision%
