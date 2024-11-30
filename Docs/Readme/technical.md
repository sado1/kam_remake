# ![](Readme/GUI_0304.gif) Technical details

The game uses a custom, open source engine, written from scratch - but still relies on the original resource files. We assume that you own the original KaM game (the installer checks for it), otherwise you should buy it – it's available in a number of online gaming stores.

The KaM Remake executable is freeware and is built by enthusiasts. Applications used: Delphi XE2 - Delphi Yukon 12, FastMM4, Lazarus, OpenGL, OpenAL, Overbyte ICS, zLib, Ogg Vorbis, PNGImage, MadExcept, libZPlay.

### ![](Readme/GUI_0310.gif) Contributing

Get started at <https://trello.com/c/anU9APld/1265-faq-for-new-developers-testers> or <https://github.com/reyandme/kam_remake/wiki/ProjectCompilation>.  
Our Discord is a good place to seek for help in development.

### ![](Readme/GUI_0312.gif) KaM Remake Maps and Campaigns repository
All Maps and Campaigns are in the [kam_remake_maps repo](https://github.com/reyandme/kam_remake_maps "KaM Remake Maps and Campaigns repository") maps and campaigns

# ![](Readme/GUI_0324.gif) Dedicated Server

Executables for the multiplayer dedicated server are included, available for Windows and Linux x86 and x86_64. The server listens for connections on a TCP port (56789 by default) and acts as a hub to trasfer the data between clients. The dedicated server does not understand gameplay, it simply moves packets around. The first client to join a room will be assigned hosting rights allowing them to setup the game. If that client disconnects, the next one in the list will be assigned hosting rights.
The server can be configured from the file KaM_Remake_Settings.ini. (created on the first of the game or server) Options for the dedicated server are in the section [Server].
To use in a LAN game, first make sure the server will not be published to the master server by setting "AnnounceDedicatedServer=0" in the INI file. (see above) Then start the dedicated server and ensure it is running properly (it should say "Listening on port 56789") Please ensure your firewall is allowing clients to connect to the server, by allowing TCP port 56789. (or whichever you chose) You may then connect to it from the same computer and/or other computers. (For the same computer, connect to the address 127.0.0.1 or localhost)
To use on the internet, you must forward the TCP port 56789 (or whichever you chose) from your router to your PC running the dedicated server. (for more information on port forwarding visit http://portforward.com/) Tell your friends to connect to your server name from the list if you chose to make your server publicly announced, or your external IP address if not, which is displayed to you on the homepage of http://portforward.com/. If you are playing on the same computer that is running the dedicated server, or on the same LAN, connect to its IP address as you would for a LAN game.

Note: The dedicated server is not required to play multiplayer. Clicking "Start Local Server" or "Start Internet Server" from the network menu has the same effect, except you must participate in the game and the server cannot be left to maintain itself. The dedicated server is only necessary if you want to provide a server without playing. The main purpose of the dedicated server is to allow servers to be hosted 24/7, which players can join and participate in any time they like. If you are able to provide this service we would be pleased.

The Linux builds have been tested on Fedora and Ubuntu. Any queries or comments regarding the Linux builds should be sent to Lewin. (lewinjh@gmail.com). 