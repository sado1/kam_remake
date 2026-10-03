# Introduction
If you want to install Remake on Linux, it is recommended to use KaM: TPR installers from **GOG** or from an **original CD** - they just work. But if you already bought TPR on **Steam**, there are a few more steps, but it's also possible.

## Known issues
### Fullscreen mode issues
in case when **fullscreen mode does not work properly** (ie. game disappears from your screen, or it's only partially visible), you are especially at risk if you:
- use GNOME
- have more than 1 screen 
- made changes to your display setup recently
- just installed the game and are unlucky

  **Workaround:** in Remake settings file (*WINE_PREFIX/drive_c/users/youruser/Documents/My Games/Knights and Merchants Remake/KaM Remake Settings.xml*) **in \<Main\> ---> \<Window\> section**, set **Position="2" FixedPosition="True"**, then restart Remake to force to regenerate it.

More details in https://trello.com/c/f5SiYudH/2262-disappearing-remake-window-under-linux 
### Dropped from multiplayer game/lobby when window is invisible
This happens when using Wine with enabled native Wayland support (available in Wine 10 or later). This is likely caused by GPU power-saving features of Wayland.

Workaround: never fully cover your Remake window. _On GNOME, I just run Remake in fullscreen, and non-fullscreen windows are smaller because of the top bar_.

Lots of details for interested people in https://trello.com/c/Weu3EMqE/2358-power-saving-features-cause-mp-disconnection
### HiDPI scaling
Remake does not support it natively, so you need recent enough Wine version (9.17 or newer) for "dumb" window surface scaling, which should work the same way as it does on Windows.

### Sound/video issues
- **videos are disabled** under Wine - otherwise they would crash the game
- if you're missing sounds, some distributions might not install all optional dependencies for Wine - ie. for Arch, you will need `lib32-alsa-plugins lib32-libpulse lib32-openal`
- due to using Windows version of OpenAL, sound quality is slightly decreased


# Configure Wine
Decide if you need a Wine helper (a graphical launcher for Windows programs).

If you're using a regular Linux distribution, and you're not afraid of running Wine directly from terminal, continue in **Use Wine directly** section.
If you are a Linux newbie, or you use atomic/image-based distribution (ie. Bazzite, Fedora Silverblue), continue in **Use Bottles helper** section.

## Use Wine directly (without helpers)
- install TPR - pick one option:
  - From **GOG.com** or from **original CD** - it just works
  - From **Steam**:
    - **install Steam for Windows** under Wine
    - launch Steam, log in, **install KaM TPR**
    - **launch KaM TPR once** - otherwise Remake installer will not detect it
- **launch Remake installer**, ensure to **install OpenAL** at the end.

## Use Bottles helper
Download program called Bottles from Flathub: https://flathub.org/apps/com.usebottles.bottles
After you launch it and do initial configuration:
1. Click on **+ sign** at the left, to create a new bottle
2. **Use "custom" bottle** - we don't need gaming extras for Remake
3. Click on the **bottle name**, then continue depending on where did you buy TPR from.

### Install TPR
#### From GOG.com or from original CD
1. Click on **Run executable**, choose your GOG installer and follow the steps.
2. After it's done, continue in **Install Remake** section
#### From Steam
1. Click on **Install Programs** -> **Steam**. Wait for the installer to finish.
2. Temporarily turn off Steam for Linux, if you use it.
3. On the Programs list, click **▶ (play)** near *Steam*. Sign into Steam.
4. **Install KaM TPR**
5. **Launch KaM TPR once** - otherwise Remake installer will not detect it
6. You can **close Steam for Windows** now - you won't need it again.

### Install Remake
1. Optionally, if there are any shortcuts in Programs list, hide/remove them. I hid "Knights and Merchants" and "steam".
2. Click on **Run executable**, choose your KaM Remake installer and follow the steps.
3. To start the game, on the Programs list, click **▶ (play)** next to KaM_Remake.

To run Remake twice - useful, when in multiplayer, you want to preview your location in map editor - click **Run executable**, browse to Remake's directory, and choose KaM_Remake.exe. _(If you chose custom path for Remake installation, ensure Bottles is allowed permissions to Remake's path - you can set it with Flatseal app)_

# Why do we suggest to install this way?
The Windows installer for Remake, needs to detect correctly installed Knights and Merchants: The Peasants Rebellion. TPR installers from GOG and original CD are simple, so typical user just runs them and installs Remake afterwards.

However, based on the cases we supported on Discord in the last few years, a typical Steam user expects, that they will be able to install TPR in Steam for Linux, and Remake will detect it. That is not implemented (if it's at all possible) as of now.

Furthermore, Steam includes a broken Linux version of KaM (with, surprisingly, Remake r6720 pre-installed), based on Wine (bundled by the publisher, before Proton was a thing). This complicates the setup slightly more, as you also need to enable Windows version of the game to proceed.

Therefore, these are our options:
- disable Linux version of TPR in Steam, then force Remake installer to run under Proton prefix
- make a symlink to a specific directory, to fool your system's Wine, so it sees TPR installed in Steam's game installation path
- install Steam for Windows under Wine, login, install TPR there, then finally, install the Remake

and the last option is the best, because the least amount of knowledge and debugging time is needed.
