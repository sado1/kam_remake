@REM pull from private repo
@echo off
echo called pull_repos.bat

call rsvars.bat

@SET curr_directory=%cd%

cd /D ..

@REM update all submodules
git submodule update --recursive --remote

@REM pull TranslationManager
cd /D "Utils/TranslationManager (from kp-wiki)"

git checkout master
git pull 

@REM pull private repo files
cd /D %KMRPrivateRepoDir%

git pull

@REM pull resource repo
cd /D %KMRResourcesRepoDir%

git pull

@REM get back to our directory
cd /D %curr_directory% 

