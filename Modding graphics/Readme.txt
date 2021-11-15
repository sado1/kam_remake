### How to mod KaM Remake graphics

 * Run KaM Remake.
 * Press F11 in menu or in game.
 * Select Menu > Export Data > Resources > Units/Houses/Trees/etc.
 * Folder with exported graphics will be created `\KaM Remake\Export\`.
 * Now you can see all the the exported graphics and plan your mod.
 * Place the replacement graphics into the folder `\KaM Remake\Modding graphics\` preserving file names (e.g. `4_0003.png`). You can also add alpha shadow files, (f.e. `4_0003a.png`) and pivot points files (f.e `4_0003.txt`).  
Those files could be also placed into subfolders.  
If there are several sprites with the same name in the `Modding graphics` folder and its subfolders, then the game will load the one from `Modding graphics` folder, or from the first of subfolders in alphabetical order.  
Folders containing word 'skip' in their name will be ignored and any data from the will not be used. For example all files in the following subfolder will be skipped: `\KaM Remake\Modding graphics\my_mod\skip_units\`

`#_####.png` - main graphic  
`#_####.txt` - offset information, 2 lines, X and Y offset in pixels  
`#_####a.png` - player color mask area (gets processed)  
`#_####m.png` - graphic mask or alpha (gets used 'as is')

 * When you restart the game the replacement graphic will be used.

Source: https://github.com/reyandme/kam_remake/wiki/Modding-graphics