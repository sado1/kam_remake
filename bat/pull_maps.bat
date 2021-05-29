@REM pull maps in the kam_remake_maps repo
@echo off
echo called pull_maps.bat

call rsvars.bat

@SET curr_directory=%cd%

cd /D %KMRMapsRepoDir%

git pull

cd /D %curr_directory% 

