@REM pull maps in the kam_remake_maps repo
@echo off
echo called fetch_maps.bat

call rsvars.bat

@SET curr_directory=%cd%

cd /D %KmrMapsRepoDir%

git pull

cd /D %curr_directory% 

