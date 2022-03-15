@echo off
echo called update_revision_locally.bat

@REM get revision number from git
@FOR /F "tokens=* USEBACKQ" %%F IN (`git rev-list --count HEAD`) DO (
  @SET kam_revision=%%F
)
@REM increment revision number by 1, as we are going to make one more commit for KM_Revision.inc
@SET /A kam_revision=kam_revision+1

@REM update revision number in KM_Revision.inc file 
@echo '%kam_revision%' > ..\KM_Revision.inc

@REM update revision for Installer
@echo #define Revision 'r%kam_revision%' > ..\Installer\Revision.iss

echo KMR Revision: r%kam_revision% 