rmdir /S /Q logs 

erase /F /Q /S kmr_dev.xml

@echo off
setlocal

REM Delete all count.dat files from sfx folders recursively, they are temp game files 
for /r "data\sfx\" %%f in ("count.dat") do (
    del "%%f"
)

endlocal

erase /F /Q /S *.tmp *.log thumbs.db KaM_Remake.map descript.ion *.skincfg *.identcache *.tvsconfig *.mi *.stat

erase /F /Q /S /A:H *.tmp *.log thumbs.db KaM_Remake.map descript.ion *.skincfg *.identcache *.tvsconfig *.mi *.stat

erase /F /Q /S uninst_clean.bat
