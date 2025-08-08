unit KM_GUIMenuLobby;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Math, SysUtils,
  KM_Defaults, KM_NetTypes, KM_Console, KM_ResTexts,
  KM_Controls, KM_ControlsBase, KM_ControlsDrop, KM_ControlsEdit, KM_ControlsList, KM_ControlsMemo, KM_ControlsMinimapView,
  KM_ControlsPopUp, KM_ControlsProgressBar, KM_ControlsSwitch, KM_ControlsTrackBar,
  KM_Maps, KM_Saves, KM_Pics, KM_InterfaceDefaults, KM_InterfaceTypes, KM_MinimapMission,
  KM_GUIMapEdRMG;


type
  TKMLobbyTab = (ltDesc, ltOptions);

  TKMMenuLobby = class(TKMMenuPageCommon)
  private
    fOnPageChange: TKMMenuChangeEventText; //will be in ancestor class
    fGuiRMG: TKMMapEdRMG; //RMG

    fLastTimeResetBans: Cardinal;
    fLastTimeAskReady: Cardinal;

    fMapsMP: TKMapsCollection;
    fSavesMP: TKMSavesCollection;
    fMinimap: TKMMinimapMission;

    fLobbyTab: TKMLobbyTab;

    fLocalToSlot: array [1..MAX_LOBBY_SLOTS] of Integer;
    fSlotToLocal: array [1..MAX_LOBBY_SLOTS] of Integer;

    fDropBoxPlayers_LastItemIndex: Integer;

    fMapsSortUpdateNeeded: Boolean;
    fMainHeight: Integer;
    fPanelDescBaseTop: Integer;

    procedure UpdateMappings;
    procedure UpdateSpectatorDivide;

    procedure CreateControls(aParent: TKMPanel);
    procedure CreateChatMenu(aParent: TKMPanel);
    procedure CreatePlayerMenus(aParent: TKMPanel);
    procedure CreateSettingsPopUp(aParent: TKMPanel);

    procedure Reset(aKind: TKMNetPlayerKind; aPreserveMaps: Boolean = False);
    procedure GameOptionsTabSwitch(Sender: TObject);
    procedure GameOptionsChange(Sender: TObject);
    procedure FileDownloadClick(Sender: TObject);
    procedure ReadmeClick(Sender: TObject);

    procedure ChatMenuSelect(aItemTag: TKMNetHandleIndex);
    procedure ChatMenuClick(Sender: TObject);
    procedure ChatMenuShow(Sender: TObject);

    procedure HostMenuClick(Sender: TObject);
    procedure JoinerMenuClick(Sender: TObject);
    function CanShowPlayerMenu(Sender: TObject): Boolean;
    procedure PlayerMenuShow(Sender: TObject);

    procedure ToggleMutePlayer(aPlayerIndex: Integer);
    procedure UpdateMuteMenuItem(aMenu: TKMPopUpMenu; aItemIndex: Integer; aIsMuted: Boolean);
    procedure UpdateImageLobbyFlag(aIndex: Integer);

    procedure SetupChange(Sender: TObject);
    procedure PlayersChange(Sender: TObject);
    procedure MapColumnClick(Sender: TObject; const aColumn: Integer);
    procedure SelectRMGMap(); //RMG
    procedure MapTypeChanged(Sender: TObject);
    procedure InitDropColMapsList;
    procedure MapList_OnShow(Sender: TObject);
    procedure UpdateMapList(aIsHost: Boolean);

    procedure MapList_SortUpdate(Sender: TObject);
    procedure MapList_ScanUpdate(Sender: TObject);
    procedure MapList_ScanComplete(Sender: TObject);

    procedure WakeUpNotReadyClick(Sender: TObject);

    procedure RefreshMapList(aJumpToSelected: Boolean);
    procedure RefreshSaveList(aJumpToSelected: Boolean);
    procedure MapChange(Sender: TObject);
    procedure DropBoxMaps_CellClick(Sender: TObject; const aCellX, aCellY: Integer; var aHandled: Boolean);
    procedure DropBoxPlayers_CellClick(Sender: TObject; const aCellX, aCellY: Integer; var aHandled: Boolean);
    procedure DropBoxPlayers_Show(Sender: TObject);
    procedure PercentBar_PlayerDl_ChVisibility(aPlayerIndex: Integer; aShow: Boolean);

    function DoPost: Boolean;
    function PostKeyDown(Sender: TObject; Key: Word; Shift: TShiftState): Boolean;
    function IsKeyEvent_Return_Handled(Sender: TObject; Key: Word): Boolean;
    procedure PostMsg(const aMsg: UnicodeString);
    procedure PostLocalMsg(const aMsg: UnicodeString);
    procedure HandleError(const aMsg: UnicodeString);

    function AISlotsAvailable(aAIPlayerTypes: TKMNetPlayerTypeSet = [AI_PLAYER_TYPE_MIN..AI_PLAYER_TYPE_MAX]): Byte;
    procedure MinimapLocClick(Sender: TObject; const aLoc: Integer);

    function Speed2TrackBarPos(aSpeed: Single): Integer;
    function TrackBarPos2Speed(aTrackPos: Integer): Single;

    procedure UpdateGameOptionsUI;
    procedure UpdateDescNOptionsUI;
    procedure UpdateDifficultyLevels(aSave: TKMSaveInfo); overload;
    procedure UpdateDifficultyLevels(aMap: TKMMapInfo); overload;

    procedure Lobby_OnDisconnect(const aReason: UnicodeString);
    procedure Lobby_OnGameOptions;
    procedure Lobby_OnMapName(const aData: UnicodeString);
    procedure Lobby_OnMapMissing(const aData: UnicodeString; aStartTransfer: Boolean);
    procedure Lobby_OnMessage(const aText: UnicodeString);
    procedure Lobby_OnPingInfo;
    procedure Lobby_OnPlayersSetup;
    procedure Lobby_OnUpdateMinimap;
    procedure Lobby_OnReassignedToHost;
    procedure Lobby_OnReassignedToJoiner;
    procedure Lobby_OnFileTransferProgress(aTotal, aProgress: Cardinal);
    procedure Lobby_OnPlayerFileTransferProgress(aNetPlayerIndex: Integer; aTotal, aProgress: Cardinal);
    procedure Lobby_OnSetPassword(const aPassword: AnsiString);
    procedure Lobby_AbortAllTransfers;

    procedure StartBtnChangeEnabled(Sender: TObject; aEnable: Boolean);

    function DetectMapType: Integer;
    procedure SettingsClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure EscKeyDown(Sender: TObject);
    procedure KeyDown(Sender: TObject; Key: Word; Shift: TShiftState);

    procedure ChatTextChanged(Sender: TObject);
    procedure SetChatHandlers;
    procedure UpdateChatControls;

    procedure ResetDropColorRandom(aI: Integer);
    procedure UpdateDropColor_BlockColSel(I: Integer);
  protected
    Panel_Lobby: TKMPanel;
      Panel_Settings: TKMPanel;
        Edit_Description: TKMEdit;
        Edit_Password: TKMEdit;
        Button_SettingsUseLastPassword: TKMButton;
        Checkbox_RememberPassword: TKMCheckbox;
        Button_SettingsResetBans: TKMButton;
        Button_SettingsAskReady: TKMButton;
        Button_SettingsSave: TKMButton;
        Button_SettingsCancel: TKMButton;

      PopUpMenu_Chat: TKMPopUpMenu;
      PopUpMenu_Host: TKMPopUpMenu;
      PopUpMenu_Joiner: TKMPopUpMenu;

      Panel_ServerName: TKMPanel;
        Label_ServerName: TKMLabel;

      Panel_Players: TKMPanel;
        Image_PasswordLock: TKMImage;
        Bevel_Players: TKMBevel;
        CheckBox_HostControl: TKMCheckBox;
        CheckBox_RandomizeTeamLocations: TKMCheckBox;
        CheckBox_Spectators: TKMCheckBox;
        Bevel_SpecsDivide: TKMBevel;
        Image_HostStar: TKMImage;
        Image_Flag: array [1..MAX_LOBBY_SLOTS] of TKMImage;
        DropBox_PlayerSlot: array [1..MAX_LOBBY_SLOTS] of TKMDropColumns;
        Label_Player: array [1..MAX_LOBBY_SLOTS] of TKMLabel;
        PercentBar_DownloadProgress: array [1..MAX_LOBBY_SLOTS] of TKMPercentBar;
        DropBox_Loc: array [1..MAX_LOBBY_SLOTS] of TKMDropList;
        DropBox_Team: array [1..MAX_LOBBY_SLOTS] of TKMDropList;
        DropBox_Colors: array [1..MAX_LOBBY_SLOTS] of TKMDropColumns;
        Image_Ready: array [1..MAX_LOBBY_SLOTS] of TKMImage;
        Label_Ping: array [1..MAX_LOBBY_SLOTS] of TKMLabel;

      Panel_Setup: TKMPanel;
        Radio_MapType: TKMRadioGroup;
        DropCol_Maps: TKMDropColumns;
        Label_MapName: TKMLabel;
        Panel_SetupTransfer: TKMPanel;
          Button_SetupDownload: TKMButton;
          PercentBar_SetupProgress: TKMPercentBar;
        Panel_SetupMinimap: TKMPanel;
          MinimapView: TKMMinimapView;
        Button_TabDesc, Button_TabOptions: TKMButton;
        Panel_SetupDesc: TKMPanel;
          Memo_MapDesc: TKMMemo;
          Button_SetupReadme: TKMButton;
        Panel_SetupOptions: TKMPanel;
          Panel_Difficulty: TKMPanel;
            Label_Difficulty: TKMLabel;
            DropBox_Difficulty: TKMDropList;
          Panel_GameOptions: TKMPanel;
            TrackBar_LobbyPeacetime: TKMTrackBar;
            TrackBar_SpeedPT, TrackBar_SpeedAfterPT: TKMTrackBar;

      Memo_Posts: TKMMemo;
      Button_Post: TKMButtonFlat;
      Edit_Post: TKMEdit;

      Button_Back: TKMButton;
      Button_ChangeSettings: TKMButton;
      Button_Start: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
    destructor Destroy; override;

    procedure Show(aKind: TKMNetPlayerKind; aMainHeight: Word);
    procedure Lobby_Resize(aMainHeight: Word);
    procedure ReturnToLobby(const aSaveName: UnicodeString);
    procedure UpdateState;
  end;


implementation
uses
  KM_Log, KM_CommonTypes, KM_Points, KM_CommonUtils,
  KM_GameSettings,
  KM_ControlsTypes,
  KM_Sound,
  KM_RenderUI,
  KM_Resource, KM_ResFonts, KM_ResLocales, KM_ResSound, KM_ResTypes,
  KM_Networking, KM_NetRoom,
  KM_MapTypes, KM_MapUtilsExt,
  KM_GameAppSettings,
  KM_ServerSettings;

const
  PANEL_SETUP_OPTIONS_TOP = 574;
  PANEL_SETUP_OPTIONS_HEIGHT = 144;
  RESET_BANS_COOLDOWN = 1000;
  ASK_READY_COOLDOWN = 1000;
  SPEED_STEP = 0.1;

  MAP_TYPE_INDEX_RMG = 4;
  MAP_TYPE_INDEX_SAVE = 5;


{ TKMGUIMenuLobby }
constructor TKMMenuLobby.Create(aParent: TKMPanel; aOnPageChange: TKMMenuChangeEventText);
begin
  inherited Create(gpLobby);

  fOnPageChange := aOnPageChange;
  OnEscKeyDown := EscKeyDown;
  OnKeyDown := KeyDown;

  fMapsSortUpdateNeeded := False;

  fMinimap := TKMMinimapMission.Create(True);

  fLastTimeResetBans := 0;
  fLastTimeAskReady := 0;

  fMapsMP := TKMapsCollection.Create([mkMP, mkDL], smByNameDesc, True);
  fSavesMP := TKMSavesCollection.Create;

  fDropBoxPlayers_LastItemIndex := -1;
  fLobbyTab := ltDesc;

  CreateControls(aParent);
  CreateChatMenu(aParent);
  CreatePlayerMenus(aParent);
  CreateSettingsPopUp(aParent);
end;


destructor TKMMenuLobby.Destroy;
begin
  fMapsMP.Free;
  fSavesMP.Free;
  fMinimap.Free;
  fGuiRMG.Free;

  inherited;
end;


procedure TKMMenuLobby.UpdateMappings;
var
  I, K: Integer;
  oldLocalToSlotIndex: array[1..MAX_LOBBY_SLOTS] of Integer;
begin
  //First empty everything
  for I:=1 to MAX_LOBBY_SLOTS do
  begin
    oldLocalToSlotIndex[I] := fLocalToSlot[I];
    fLocalToSlot[I] := -1;
    fSlotToLocal[I] := -1;
  end;
  K := 1;

  //Host (unless host is spectator)
  if (gNetworking.HostSlotIndex <> -1) and not gNetworking.Room[gNetworking.HostSlotIndex].IsSpectator then
  begin
    fLocalToSlot[K] := gNetworking.HostSlotIndex;
    fSlotToLocal[gNetworking.HostSlotIndex] := K;
    Inc(K);
  end;

  //Normal players
  for I:=1 to gNetworking.Room.Count do
    if (I <> gNetworking.HostSlotIndex) and not gNetworking.Room[I].IsSpectator then
    begin
      fLocalToSlot[K] := I;
      fSlotToLocal[I] := K;
      Inc(K);
    end;

  //Host if spectator, always goes at the end
  if (gNetworking.HostSlotIndex <> -1) and gNetworking.Room[gNetworking.HostSlotIndex].IsSpectator then
  begin
    fLocalToSlot[MAX_LOBBY_SLOTS] := gNetworking.HostSlotIndex;
    fSlotToLocal[gNetworking.HostSlotIndex] := MAX_LOBBY_SLOTS;
  end;

  //Spectators, place them at the end
  K := MAX_LOBBY_SLOTS - gNetworking.Room.GetSpectatorCount + 1;
  for I:=1 to gNetworking.Room.Count do
    if (I <> gNetworking.HostSlotIndex) and gNetworking.Room[I].IsSpectator then
    begin
      Assert((K <= MAX_LOBBY_SLOTS) and (fLocalToSlot[K] = -1), 'Too many spectators');
      fLocalToSlot[K] := I;
      fSlotToLocal[I] := K;
      Inc(K);
    end;

  //If a player has moved slots on the list the dropboxes can get stuck open
  for I:=1 to MAX_LOBBY_SLOTS do
    if oldLocalToSlotIndex[I] <> fLocalToSlot[I] then
    begin
      DropBox_PlayerSlot[I].CloseList;
      DropBox_Loc[I].CloseList;
      DropBox_Team[I].CloseList;
      DropBox_Colors[I].CloseList;
    end;
end;


procedure TKMMenuLobby.UpdateSpectatorDivide;
const
  TOP_OFF = 68;
  LINE_Y = 23;
  DIVIDE_Y = 5;
var
  I, divideRow, offY: Integer;
begin
  Image_HostStar.Hide; //In case host is unknown
  if (gNetworking <> nil) and (gNetworking.Room <> nil) then
    divideRow := MAX_LOBBY_SLOTS - Max(MAX_LOBBY_SPECTATORS, gNetworking.Room.GetSpectatorCount)
  else
    divideRow := MAX_LOBBY_PLAYERS;
  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    offY := TOP_OFF + (I-1) * LINE_Y;

    if I = divideRow+1 then
      Bevel_SpecsDivide.Top := offY;

    if I > divideRow then
      Inc(offY, DIVIDE_Y);

    Image_Flag[I].Top            := offY;
    Label_Player[I].Top          := offY+2;
    PercentBar_DownloadProgress[I].Top := offY;
    DropBox_PlayerSlot[I].Top    := offY;
    DropBox_Loc[I].Top           := offY;
    DropBox_Team[I].Top          := offY;
    DropBox_Colors[I].Top        := offY;
    Image_Ready[I].Top           := offY;
    Label_Ping[I].Top            := offY;

    if (gNetworking <> nil) and (fLocalToSlot[I] = gNetworking.HostSlotIndex) then
    begin
      Image_HostStar.Top := offY+2;
      Image_HostStar.Show;
      PercentBar_PlayerDl_ChVisibility(I, False);
    end;
  end;
  if (gNetworking <> nil) and (gNetworking.Room <> nil)
  and gNetworking.Room.SpectatorsAllowed then
  begin
    Panel_Players.Height := TOP_OFF + LINE_Y*MAX_LOBBY_SLOTS + DIVIDE_Y + 2;
    Bevel_SpecsDivide.Show;
  end
  else
  begin
    Panel_Players.Height := TOP_OFF + LINE_Y*MAX_LOBBY_PLAYERS + 2;
    Bevel_SpecsDivide.Hide;
  end;
  Bevel_Players.Height := Panel_Players.Height;
  Memo_Posts.Top := Panel_Players.Top + Panel_Players.Height + 5;
  Memo_Posts.Height := Edit_Post.Top - Memo_Posts.Top - 2;
end;


procedure TKMMenuLobby.CreateControls(aParent: TKMPanel);

  function MakeRow(const aCaption: array of string; aIndex: Integer): TKMListRow;
  var
    I: Integer;
  begin
    Result := MakeListRow(aCaption, aIndex);
    for I := Low(aCaption) to High(aCaption) do
    begin
      Result.Cells[I].HighlightOnMouseOver := True;
      if I = 1 then
      begin
        Result.Cells[I].Color := clLobbyOpponentAll;
        Result.Cells[I].HighlightColor := clLobbyOpponentAllHL;
      end
      else
        Result.Cells[I].HighlightColor := icGray;
    end;
  end;

const
  CW = 690; C1 = 35; C2 = 200; C3 = 360; C4 = 445; C5 = 570; C6 = 650;
  C1W = 155; C2W = 150; C3W = 75; C4W = 80;
  TC2_ADD = 50;
  ALL_TXT_W_MIN = 35;
var
  I, K, offY, slotTxtWidth, allTxtWidth, speedsCnt: Integer;
begin
  Panel_Lobby := TKMPanel.Create(aParent,0,0,aParent.Width, aParent.Height);
  Panel_Lobby.AnchorsStretch;

    //Server Name
    Panel_ServerName := TKMPanel.Create(Panel_Lobby, 30, 30, CW, 26);
      TKMBevel.Create(Panel_ServerName,   0,  0, CW, 26);
      Label_ServerName := TKMLabel.Create(Panel_ServerName, 10, 7, CW-20, 20, '', fntMetal, taLeft);

    //Players
    Panel_Players := TKMPanel.Create(Panel_Lobby, 30, 61, CW, 340);
      Bevel_Players := TKMBevel.Create(Panel_Players,  0,  0, CW, 340);

      CheckBox_HostControl := TKMCheckBox.Create(Panel_Players, 10, 10, (CW div 2) + TC2_ADD - 10, 20, gResTexts[TX_LOBBY_HOST_DOES_SETUP], fntMetal);
      CheckBox_HostControl.OnClick := SetupChange;

      CheckBox_Spectators := TKMCheckbox.Create(Panel_Players, (CW div 2) + TC2_ADD, 10, (CW div 2) - TC2_ADD - 10, 20, gResTexts[TX_LOBBY_ALLOW_SPECTATORS], fntMetal);
      CheckBox_Spectators.OnClick := SetupChange;

      CheckBox_RandomizeTeamLocations := TKMCheckBox.Create(Panel_Players, 10, 28, CW-20, 20, gResTexts[TX_LOBBY_RANDOMIZE_LOCATIONS], fntMetal);
      CheckBox_RandomizeTeamLocations.OnClick := SetupChange;

    offY := 49;

      Image_PasswordLock := TKMImage.Create(Panel_Players, 13, offY, 12, 16, 73, rxGuiMain);
      Image_PasswordLock.Hide;

      //Column titles
      TKMLabel.Create(Panel_Players, C1, offY, C1W,  20, gResTexts[TX_LOBBY_HEADER_PLAYERS], fntOutline, taLeft);
      TKMLabel.Create(Panel_Players, C2, offY, C2W,  20, gResTexts[TX_LOBBY_HEADER_STARTLOCATION], fntOutline, taLeft);
      TKMLabel.Create(Panel_Players, C3, offY, C3W,  20, gResTexts[TX_LOBBY_HEADER_TEAM], fntOutline, taLeft);
      TKMLabel.Create(Panel_Players, C4, offY, C4W,  20, gResTexts[TX_LOBBY_HEADER_FLAGCOLOR], fntOutline, taLeft);
      TKMLabel.Create(Panel_Players, C5, offY, gResTexts[TX_LOBBY_HEADER_READY], fntOutline, taCenter);
      TKMLabel.Create(Panel_Players, C6, offY, gResTexts[TX_LOBBY_HEADER_PING], fntOutline, taCenter);

      Bevel_SpecsDivide := TKMBevel.Create(Panel_Players, 10, 50, CW-20, 3);

      Image_HostStar := TKMImage.Create(Panel_Players, C2-25, 50, 20, 20, 77, rxGuiMain);
      Image_HostStar.Hide;

      slotTxtWidth := Max(C1W - 45,
                          gRes.Fonts[fntGrey].GetMaxPrintWidthOfStrings([gResTexts[TX_LOBBY_SLOT_OPEN],
                                                                          gResTexts[TX_LOBBY_SLOT_CLOSED],
                                                                          gResTexts[TX_AI_PLAYER_CLASSIC],
                                                                          gResTexts[TX_AI_PLAYER_ADVANCED]]));

      allTxtWidth := Max(ALL_TXT_W_MIN, gRes.Fonts[fntGrey].GetMaxPrintWidthOfStrings([gResTexts[TX_LOBBY_SLOT_OPEN_ALL],
                                                                             gResTexts[TX_LOBBY_SLOT_CLOSED_ALL],
                                                                             gResTexts[TX_LOBBY_SLOT_AI_ALL]]));

      for I := 1 to MAX_LOBBY_SLOTS do
      begin
        offY := 70 + (I-1) * 23;
        Image_Flag[I] := TKMImage.Create(Panel_Players, 10, offY, 20, 20, 0, rxGuiMain);
        Image_Flag[I].ImageCenter;
        Image_Flag[I].Tag := I; //Required for PlayerMenuShow
        Image_Flag[I].OnClick := PlayerMenuShow;
        Image_Flag[I].HighlightOnMouseOver := True;

        Label_Player[I] := TKMLabel.Create(Panel_Players, C1, offY+2, C1W, 20, '', fntGrey, taLeft);
        Label_Player[I].Hide;

        DropBox_PlayerSlot[I] := TKMDropColumns.Create(Panel_Players, C1, offY, C1W, 20, fntGrey, '', bsMenu, False);
        DropBox_PlayerSlot[I].DropWidth := slotTxtWidth + 5 + allTxtWidth + 7*Byte(allTxtWidth > ALL_TXT_W_MIN); //Add some extra space for 'All' word
        DropBox_PlayerSlot[I].SetColumns(fntOutline, ['', gResTexts[TX_MENU_MAP_TITLE]], [0, slotTxtWidth + 5], [True, False]);
        //1st column is used to set 'All' (All Open/All AI/All Closed),
        //Its external button analogue, so we do not want to invoke f.e. OnChange (AI) when 'AI All' clicked
        DropBox_PlayerSlot[I].List.Columns[1].TriggerOnChange := False;
        if I <= MAX_LOBBY_PLAYERS then
        begin
          DropBox_PlayerSlot[I].Add(MakeRow([gResTexts[TX_LOBBY_SLOT_OPEN], gResTexts[TX_LOBBY_SLOT_OPEN_ALL]], I)); //Player can join into this slot
          DropBox_PlayerSlot[I].Add(MakeRow([gResTexts[TX_LOBBY_SLOT_CLOSED], gResTexts[TX_LOBBY_SLOT_CLOSED_ALL]], I)); //Closed, nobody can join it
          DropBox_PlayerSlot[I].Add(MakeRow([gResTexts[TX_AI_PLAYER_CLASSIC], gResTexts[TX_LOBBY_SLOT_AI_ALL]], I)); //This slot is an AI player
          DropBox_PlayerSlot[I].Add(MakeRow([gResTexts[TX_AI_PLAYER_ADVANCED], gResTexts[TX_LOBBY_SLOT_AI_ALL]], I)); //This slot is an advanced AI player
        end
        else
        begin
          DropBox_PlayerSlot[I].Add(MakeRow([gResTexts[TX_LOBBY_SLOT_OPEN], gResTexts[TX_LOBBY_SLOT_OPEN_ALL]], I));
          DropBox_PlayerSlot[I].Add(MakeRow([gResTexts[TX_LOBBY_SLOT_CLOSED], gResTexts[TX_LOBBY_SLOT_CLOSED_ALL]], I));
        end;
        DropBox_PlayerSlot[I].ItemIndex := 0; //Open
        DropBox_PlayerSlot[I].OnChange := PlayersChange;
        DropBox_PlayerSlot[I].List.OnCellClick := DropBoxPlayers_CellClick;
        DropBox_PlayerSlot[I].OnShowList := DropBoxPlayers_Show;

        DropBox_Loc[I] := TKMDropList.Create(Panel_Players, C2, offY, C2W, 20, fntGrey, '', bsMenu);
        DropBox_Loc[I].Add(gResTexts[TX_LOBBY_RANDOM], LOC_RANDOM);
        DropBox_Loc[I].OnChange := PlayersChange;
        DropBox_Loc[I].DropCount := MAX_LOBBY_PLAYERS + 2; //also 'Random' and possible 'Spectator'

        PercentBar_DownloadProgress[I] := TKMPercentBar.Create(Panel_Players, C2, offY, 150, 20, fntGrey);
        PercentBar_DownloadProgress[I].Caption := gResTexts[TX_LOBBY_DOWNLOADING];
        PercentBar_DownloadProgress[I].Hide;
        PercentBar_DownloadProgress[I].TextYOffset := -3;

        DropBox_Team[I] := TKMDropList.Create(Panel_Players, C3, offY, C3W, 20, fntGrey, '', bsMenu);
        DropBox_Team[I].Add('-');
        for K := 1 to MAX_TEAMS do DropBox_Team[I].Add(IntToStr(K));
        DropBox_Team[I].OnChange := PlayersChange;

        DropBox_Colors[I] := TKMDropColumns.Create(Panel_Players, C4, offY, C4W, 20, fntGrey, '', bsMenu);
        DropBox_Colors[I].SetColumns(fntOutline, [''], [0]);
        DropBox_Colors[I].List.ShowHeader := False;
        DropBox_Colors[I].DropCount := 13;
        DropBox_Colors[I].FadeImageWhenDisabled := False;
        DropBox_Colors[I].Add(MakeListRow([''], [$FFFFFFFF], [MakePic(rxGuiMain, 31)], 0));
        for K := Low(MP_PLAYER_COLORS) to High(MP_PLAYER_COLORS) do
          DropBox_Colors[I].Add(MakeListRow([''], [MP_PLAYER_COLORS[K]], [MakePic(rxGuiMain, 30)]));
        DropBox_Colors[I].OnChange := PlayersChange;

        Image_Ready[I] := TKMImage.Create(Panel_Players, C5-8, offY, 16, 16, 32, rxGuiMain);
        Label_Ping[I] := TKMLabel.Create(Panel_Players, C6, offY, '', fntMetal, taCenter);
      end;

    //Chat area
    Memo_Posts := TKMMemo.Create(Panel_Lobby, 30, 406, CW, 282, fntArial, bsMenu);
    Memo_Posts.Anchors := [anLeft, anTop, anBottom];
    Memo_Posts.WordWrap := True;
    Memo_Posts.IndentAfterNL := True; //Don't let players fake system messages
    Memo_Posts.ScrollDown := True;

    Button_Post := TKMButtonFlat.Create(Panel_Lobby, 30, 696, 30, 22, 0);
    Button_Post.CapOffsetY := -11;
    Button_Post.Font := fntGrey;
    Button_Post.OnClick := ChatMenuShow;
    Button_Post.Anchors := [anLeft, anBottom];

    Edit_Post := TKMEdit.Create(Panel_Lobby, 60, 696, CW, 22, fntArial);
    Edit_Post.OnChange := ChatTextChanged;
    Edit_Post.OnKeyDown := PostKeyDown;
    Edit_Post.OnIsKeyEventHandled := IsKeyEvent_Return_Handled;
    Edit_Post.Anchors := [anLeft, anBottom];
    Edit_Post.ShowColors := True;

    //Setup
    Panel_Setup := TKMPanel.Create(Panel_Lobby, 725, 30, 270, 723);
    Panel_Setup.Anchors := [anLeft, anTop, anBottom];
      offY := 9;
      with TKMBevel.Create(Panel_Setup,  0,  0, 270, 723) do AnchorsStretch;
      Radio_MapType := TKMRadioGroup.Create(Panel_Setup, 10, offY, 250, 96, fntMetal);
      Radio_MapType.Add(gResTexts[TX_LOBBY_MAP_BUILD]);
      Radio_MapType.Add(gResTexts[TX_LOBBY_MAP_FIGHT]);
      Radio_MapType.Add(gResTexts[TX_LOBBY_MAP_COOP]);
      Radio_MapType.Add(gResTexts[TX_LOBBY_MAP_SPECIAL]);
      Radio_MapType.Add(gResTexts[TX_LOBBY_MAP_RANDOM]); //RMG
      Radio_MapType.Add(gResTexts[TX_LOBBY_MAP_SAVED]);
      Radio_MapType.ItemIndex := 0;
      Radio_MapType.OnClick := MapTypeChanged;

      Inc(offY, 101);
      DropCol_Maps := TKMDropColumns.Create(Panel_Setup, 10, offY, 250, 20, fntMetal, gResTexts[TX_LOBBY_MAP_SELECT], bsMenu);
      DropCol_Maps.DropCount := 19;
      InitDropColMapsList;
      DropCol_Maps.ShowHintWhenShort := True;
      DropCol_Maps.HintBackColor := TKMColor4f.New(87, 72, 37);
      DropCol_Maps.OnShowList := MapList_OnShow;
      DropCol_Maps.List.OnColumnClick := MapColumnClick;
      DropCol_Maps.List.SearchColumn := 1;
      DropCol_Maps.List.ColumnIdForScroll := 2;
      DropCol_Maps.OnChange := MapChange;
      DropCol_Maps.List.OnCellClick := DropBoxMaps_CellClick;

      Label_MapName := TKMLabel.Create(Panel_Setup, 10, offY, 250, 20, '', fntMetal, taLeft);

      Inc(offY, 25);
      Panel_SetupMinimap := TKMPanel.Create(Panel_Setup, 0, offY, 270, 200);
        MinimapView := TKMMinimapView.Create(fMinimap, Panel_SetupMinimap, 39, 4, 191, 191, True);
        MinimapView.ShowLocs := True; //In the minimap we want player locations to be shown
        MinimapView.OnMinimapLocationClick := MinimapLocClick;

      Panel_SetupTransfer := TKMPanel.Create(Panel_Setup, 0, offY, 270, 200);
        Button_SetupDownload := TKMButton.Create(Panel_SetupTransfer, 10, 0, 250, 30, gResTexts[TX_LOBBY_DOWNLOAD], bsMenu);
        Button_SetupDownload.OnClick := FileDownloadClick;
        PercentBar_SetupProgress := TKMPercentBar.Create(Panel_SetupTransfer, 10, 0, 250, 24, fntGame);
      Panel_SetupTransfer.Hide;

      Inc(offY, 204);
      Button_TabDesc := TKMButton.Create(Panel_Setup, 10, offY, 125, 20, gResTexts[TX_LOBBY_MAP_DESCRIPTION], bsMenu);
      Button_TabDesc.OnClick := GameOptionsTabSwitch;
      Button_TabDesc.Hide;
      Button_TabOptions := TKMButton.Create(Panel_Setup, 10+125, offY, 125, 20, gResTexts[TX_LOBBY_OPTIONS], bsMenu);
      Button_TabOptions.OnClick := GameOptionsTabSwitch;
      Button_TabOptions.Hide;

      fPanelDescBaseTop := offY;
      Panel_SetupDesc := TKMPanel.Create(Panel_Setup, 0, offY, 270, 218);
      Panel_SetupDesc.Anchors := [anLeft, anTop, anBottom];
        Memo_MapDesc := TKMMemo.Create(Panel_SetupDesc, 10, 0, 250, 218, fntGame, bsMenu);
        Memo_MapDesc.Anchors := [anLeft, anTop, anBottom];
        Memo_MapDesc.WordWrap := True;
        Memo_MapDesc.ItemHeight := 16;

        Button_SetupReadme := TKMButton.Create(Panel_SetupDesc, 10, 193, 250, 25, gResTexts[TX_LOBBY_VIEW_README], bsMenu);
        Button_SetupReadme.Anchors := [anLeft, anBottom];
        Button_SetupReadme.OnClick := ReadmeClick;
        Button_SetupReadme.Hide;

      Panel_SetupOptions := TKMPanel.Create(Panel_Setup, 0, PANEL_SETUP_OPTIONS_TOP, 270, PANEL_SETUP_OPTIONS_HEIGHT);
      Panel_SetupOptions.Anchors := [anLeft,anBottom];

        Panel_Difficulty := TKMPanel.Create(Panel_SetupOptions, 0, 0, 270, 27);
          Label_Difficulty := TKMLabel.Create(Panel_Difficulty, 10, 4, gResTexts[TX_MISSION_DIFFICULTY_CAMPAIGN], fntMetal, taLeft);
          Label_Difficulty.Anchors := [anLeft, anBottom];
          DropBox_Difficulty := TKMDropList.Create(Panel_Difficulty, 130, 2, 130, 20, fntMetal, gResTexts[TX_MISSION_DIFFICULTY], bsMenu);
          DropBox_Difficulty.Anchors := [anLeft, anBottom];
          DropBox_Difficulty.OnChange := GameOptionsChange;
        Panel_Difficulty.Hide;

        Panel_GameOptions := TKMPanel.Create(Panel_SetupOptions, 0, 0, 270, 150);
          TrackBar_LobbyPeacetime := TKMTrackBar.Create(Panel_GameOptions, 10, 0, 250, 0, 120);
          TrackBar_LobbyPeacetime.Anchors := [anLeft,anBottom];
          TrackBar_LobbyPeacetime.Caption := gResTexts[TX_LOBBY_PEACETIME];
          TrackBar_LobbyPeacetime.Step := 5; //Round to 5min steps
          TrackBar_LobbyPeacetime.OnChange := GameOptionsChange;

          speedsCnt := Round((GAME_MP_SPEED_MAX - 1) / SPEED_STEP) + 1;

          TrackBar_SpeedPT := TKMTrackBar.Create(Panel_GameOptions, 10, 46, 250, 1, speedsCnt);
          TrackBar_SpeedPT.Anchors := [anLeft,anBottom];
          TrackBar_SpeedPT.Caption := gResTexts[TX_LOBBY_GAMESPEED_PEACETIME];
          TrackBar_SpeedPT.ThumbWidth := 45; //Enough to fit 'x1.5' 'x1.25'
          TrackBar_SpeedPT.OnChange := GameOptionsChange;

          TrackBar_SpeedAfterPT := TKMTrackBar.Create(Panel_GameOptions, 10, 90, 250, 1, speedsCnt);
          TrackBar_SpeedAfterPT.Anchors := [anLeft,anBottom];
          TrackBar_SpeedAfterPT.Caption := gResTexts[TX_LOBBY_GAMESPEED];
          TrackBar_SpeedAfterPT.ThumbWidth := 45; //Enough to fit 'x1.25'
          TrackBar_SpeedAfterPT.OnChange := GameOptionsChange;

    Button_Back := TKMButton.Create(Panel_Lobby, 30, 723, 220, 30, gResTexts[TX_LOBBY_QUIT], bsMenu);
    Button_Back.Anchors := [anLeft, anBottom];
    Button_Back.OnClick := BackClick;

    Button_ChangeSettings := TKMButton.Create(Panel_Lobby, 265, 723, 220, 30, gResTexts[TX_LOBBY_ROOMSETTINGS], bsMenu);
    Button_ChangeSettings.Anchors := [anLeft, anBottom];
    Button_ChangeSettings.OnClick := SettingsClick;

    Button_Start := TKMButton.Create(Panel_Lobby, 500, 723, 220, 30, NO_TEXT, bsMenu);
    Button_Start.Anchors := [anLeft, anBottom];
    Button_Start.OnClick := StartClick;
    Button_Start.OnChangeEnableStatus := StartBtnChangeEnabled;

  fGuiRMG := TKMMapEdRMG.Create(Panel_Lobby, fMinimap, True); //RMG
  fGuiRMG.OnNewMap := SelectRMGMap;

  UpdateSpectatorDivide;
end;


procedure TKMMenuLobby.CreateChatMenu(aParent: TKMPanel);
begin
  // Use special mode for PopUpMenu, when action is triggered by dragging (down and move) through the list
  PopUpMenu_Chat := TKMPopUpMenu.Create(aParent, 140, pmmActionOnMouseDownNMove);
  PopUpMenu_Chat.Anchors := [anLeft, anBottom];
  //Menu gets populated right before show
  PopUpMenu_Chat.AddItem(NO_TEXT);
  PopUpMenu_Chat.OnClick := ChatMenuClick;
end;


procedure TKMMenuLobby.CreatePlayerMenus(aParent: TKMPanel);
begin
  PopUpMenu_Host := TKMPopUpMenu.Create(aParent, gRes.Fonts[fntGrey].GetMaxPrintWidthOfStrings( // Calc max width for popup which depends of texts translation
    [gResTexts[TX_LOBBY_PLAYER_KICK], 
    gResTexts[TX_LOBBY_PLAYER_BAN], 
    gResTexts[TX_LOBBY_PLAYER_SET_HOST], 
    gResTexts[TX_MUTE_PLAYER],
    gResTexts[TX_UNMUTE_PLAYER]])
    + 10);
  PopUpMenu_Host.AddItem(gResTexts[TX_LOBBY_PLAYER_KICK]);
  PopUpMenu_Host.AddItem(gResTexts[TX_LOBBY_PLAYER_BAN]);
  PopUpMenu_Host.AddItem(gResTexts[TX_LOBBY_PLAYER_SET_HOST]);
  PopUpMenu_Host.AddItem('');
  PopUpMenu_Host.OnClick := HostMenuClick;

  // Calc max width for popup which depends of texts translation
  PopUpMenu_Joiner := TKMPopUpMenu.Create(aParent, gRes.Fonts[fntGrey].GetMaxPrintWidthOfStrings(
    [gResTexts[TX_MUTE_PLAYER],
     gResTexts[TX_UNMUTE_PLAYER]])
    + 10);
  PopUpMenu_Joiner.AddItem('');
  PopUpMenu_Joiner.OnClick := JoinerMenuClick;
end;


procedure TKMMenuLobby.CreateSettingsPopUp(aParent: TKMPanel);
const
  SET_W = 400;
begin
  Panel_Settings := TKMPanel.Create(aParent, 362, 250, SET_W, 350);
  Panel_Settings.AnchorsCenter;
    TKMBevel.Create(Panel_Settings, -2000,  -2000, 5000, 5000);
    with TKMImage.Create(Panel_Settings, -20, -75, SET_W + 40, 440, 15, rxGuiMain) do ImageStretch;
    TKMBevel.Create(Panel_Settings,   0,  0, SET_W, 343);
    TKMLabel.Create(Panel_Settings,  20, 10, SET_W - 40, 20, gResTexts[TX_LOBBY_ROOMSETTINGS], fntOutline, taCenter);

    TKMLabel.Create(Panel_Settings, 20, 50, SET_W - 40, 20, gResTexts[TX_LOBBY_ROOM_DESCRIPTION], fntOutline, taCenter);
    Edit_Description := TKMEdit.Create(Panel_Settings, 20, 70, SET_W - 40, 20, fntGrey);
    Edit_Description.AllowedChars := acText;
    Edit_Description.MaxLen := 60;

    TKMLabel.Create(Panel_Settings, 20, 100, SET_W - 40, 20, gResTexts[TX_LOBBY_ROOM_PASSWORD], fntOutline, taCenter);
    Edit_Password := TKMEdit.Create(Panel_Settings, 20, 120, SET_W - 40, 20, fntGrey);
    Edit_Password.AllowedChars := acANSI7; //Passwords are basic ANSI so everyone can type them
    Checkbox_RememberPassword := TKMCheckbox.Create(Panel_Settings, 20, 153, SET_W - 20, 30, gResTexts[TX_LOBBY_REMEMBER_PASSWORD], fntGrey);
    Checkbox_RememberPassword.Hint := gResTexts[TX_LOBBY_SETTINGS_SAVE_PWD];

    Button_SettingsResetBans := TKMButton.Create(Panel_Settings, 20, 180, SET_W - 40, 30, gResTexts[TX_LOBBY_RESET_BANS], bsMenu);
    Button_SettingsUseLastPassword := TKMButton.Create(Panel_Settings, 20, 220, SET_W - 40, 30, gResTexts[TX_LOBBY_USE_LAST_PASSWORD], bsMenu);
    Button_SettingsResetBans.OnClick := SettingsClick;
    Button_SettingsUseLastPassword.OnClick := SettingsClick;

    Button_SettingsAskReady := TKMButton.Create(Panel_Settings, 20, 260, SET_W - 40, 30, gResTexts[TX_LOBBY_ALERT_READY_BTN], bsMenu);
    Button_SettingsAskReady.OnClick := WakeUpNotReadyClick;
    Button_SettingsAskReady.Enabled := False;
    Button_SettingsSave := TKMButton.Create(Panel_Settings, 20, 300, (SET_W - 50) div 2, 30, gResTexts[TX_LOBBY_ROOM_OK], bsMenu);
    Button_SettingsSave.OnClick := SettingsClick;
    Button_SettingsCancel := TKMButton.Create(Panel_Settings, (SET_W div 2) + 10, 300, (SET_W - 50) div 2, 30, gResTexts[TX_LOBBY_ROOM_CANCEL], bsMenu);
    Button_SettingsCancel.OnClick := SettingsClick;
end;


procedure TKMMenuLobby.ChatMenuSelect(aItemTag: TKMNetHandleIndex);

  procedure UpdateButtonCaption(aCaption: UnicodeString; aColor: Cardinal = 0);
  const
    MIN_SIZE = 80; //Minimum size for the button
  var
    capWidth: Integer;
  begin
    //Update button width according to selected item
    capWidth := gRes.Fonts[Button_Post.Font].GetTextSize(aCaption).X;
    capWidth := Max(MIN_SIZE, capWidth+10); //Apply minimum size
    if aColor <> 0 then
      aCaption := WrapColor(aCaption, aColor);
    Button_Post.Caption := aCaption;
    Button_Post.Width := capWidth;

    Edit_Post.AbsLeft := Button_Post.AbsLeft + Button_Post.Width + 4;
    Edit_Post.Width := Memo_Posts.Width - Button_Post.Width - 4;
  end;

var
  netI: Integer;
begin
  case aItemTag of
    CHAT_MENU_ALL:        begin //All
                            gChat.Mode := cmAll;
                            UpdateButtonCaption(gResTexts[TX_CHAT_ALL]);
                            Edit_Post.DrawOutline := False; //No outline for All
                          end;
    CHAT_MENU_TEAM:       begin //Team
                            gChat.Mode := cmTeam;
                            UpdateButtonCaption(gResTexts[TX_CHAT_TEAM], $FF66FF66);
                            Edit_Post.DrawOutline := True;
                            Edit_Post.OutlineColor := $FF66FF66;
                          end;
    CHAT_MENU_SPECTATORS: begin //Spectators
                            gChat.Mode := cmSpectators;
                            UpdateButtonCaption(gResTexts[TX_CHAT_SPECTATORS], $FF66FF66);
                            Edit_Post.DrawOutline := True;
                            Edit_Post.OutlineColor := $FF66FF66;
                          end;
    else  begin //Whisper to player
            netI := gNetworking.Room.ServerToLocal(aItemTag);
            if netI <> -1 then
            begin
              gChat.Mode := cmWhisper;
              Edit_Post.DrawOutline := True;
              Edit_Post.OutlineColor := $FF00B9FF;
              with gNetworking.Room[netI] do
              begin
                gChat.WhisperRecipient := IndexOnServer;
                UpdateButtonCaption(NicknameU, IfThen(IsColorSet, FlagColorToTextColor(FlagColor), 0));
              end;
            end;
          end;
    end;
end;


procedure TKMMenuLobby.ChatMenuClick(Sender: TObject);
begin
  if PopUpMenu_Chat.ItemIndex <> -1 then
    ChatMenuSelect(PopUpMenu_Chat.ItemTags[PopUpMenu_Chat.ItemIndex]);
end;


procedure TKMMenuLobby.ChatMenuShow(Sender: TObject);
var
  C: TKMControl;
  I: Integer;
  slot: TKMNetRoomSlot;
begin
  // Populate menu with right options
  PopUpMenu_Chat.Clear;

  PopUpMenu_Chat.AddItem(gResTexts[TX_CHAT_ALL], CHAT_MENU_ALL);

  // Only show "Team" if the player is on a team
  if gNetworking.MyRoomSlot.Team <> 0 then
    PopUpMenu_Chat.AddItem('[$66FF66]' + gResTexts[TX_CHAT_TEAM], CHAT_MENU_TEAM);

  // Only show "Spectators" if the player is a spectator
  if gNetworking.MyRoomSlot.IsSpectator then
    PopUpMenu_Chat.AddItem('[$66FF66]' + gResTexts[TX_CHAT_SPECTATORS], CHAT_MENU_SPECTATORS);

  for I := 1 to gNetworking.Room.Count do
  if I <> gNetworking.MySlotIndex then // Can't whisper to yourself
  begin
    slot := gNetworking.Room[I];

    if slot.IsHuman and slot.Connected and not slot.Dropped then
      PopUpMenu_Chat.AddItem(slot.NicknameColoredU, slot.IndexOnServer);
  end;

  C := TKMControl(Sender);
  // Position the menu next to the icon, but do not overlap players name
  PopUpMenu_Chat.ShowAt(C.AbsLeft, C.AbsTop - PopUpMenu_Chat.Height);
end;


//Try to detect which kind it is

//Todo: we should probably refactor it to TryDetectMapType, and add checks if Map / Save is valid, f.e.
// if   ((gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid)
//   or ((gNetworking.SelectGameKind = ngkSave) and gNetworking.SaveInfo.IsValid)
// Its not good to just return 0 (which is Build map),
// since we could get into situation when ngkNone is map type and we had some other Radio.ItemIndex, f.e. 'Save'
function TKMMenuLobby.DetectMapType: Integer;
begin
  //Default
  Result := 0;

  case gNetworking.SelectGameKind of
    ngkMap:  if gNetworking.MapInfo.TxtInfo.IsCoop then
                Result := 2
              else
              if gNetworking.MapInfo.TxtInfo.IsSpecial then
                Result := 3
              else
              if gNetworking.MapInfo.TxtInfo.IsRMG then
                Result := MAP_TYPE_INDEX_RMG
              else
              if gNetworking.MapInfo.IsFightingMission then
                Result := 1;
    ngkSave: Result := MAP_TYPE_INDEX_SAVE;
  end;
end;


procedure TKMMenuLobby.ChatTextChanged(Sender: TObject);
begin
  gChat.Text := Edit_Post.Text;
end;


procedure TKMMenuLobby.UpdateChatControls;
begin
  if gChat.Mode = cmWhisper then
    ChatMenuSelect(gChat.WhisperRecipient)
  else
    ChatMenuSelect(CHAT_TAG[gChat.Mode]);

  Memo_Posts.Text := gChat.Messages;
end;


procedure TKMMenuLobby.SetChatHandlers;
begin
  gChat.OnError := HandleError;
  gChat.OnPost := PostMsg;
  gChat.OnPostLocal := PostLocalMsg;
  gChat.OnChange := UpdateChatControls;

  UpdateChatControls
end;


procedure TKMMenuLobby.Show(aKind: TKMNetPlayerKind; aMainHeight: Word);
var
  I: Integer;
begin
  Reset(aKind, True);

  //Events binding is the same for Host and Joiner because of stand-alone Server
  //E.g. If Server fails, Host can be disconnected from it as well as a Joiner
  gNetworking.OnTextMessage   := Lobby_OnMessage;
  gNetworking.OnPlayersSetup  := Lobby_OnPlayersSetup;
  gNetworking.OnUpdateMinimap := Lobby_OnUpdateMinimap;
  gNetworking.OnGameOptions   := Lobby_OnGameOptions;
  gNetworking.OnMapName       := Lobby_OnMapName;
  gNetworking.OnMapMissing    := Lobby_OnMapMissing;
  gNetworking.OnPingInfo      := Lobby_OnPingInfo;
  //gNetworking.OnStartMap - already assigned in gGameApp when Net is created
  //gNetworking.OnStartSave - already assigned in gGameApp when Net is created
  gNetworking.OnDisconnect   := Lobby_OnDisconnect;
  gNetworking.OnReassignedHost := Lobby_OnReassignedToHost;
  gNetworking.OnReassignedJoiner := Lobby_OnReassignedToJoiner;
  gNetworking.OnFileTransferProgress := Lobby_OnFileTransferProgress;
  gNetworking.OnPlayerFileTransferProgress := Lobby_OnPlayerFileTransferProgress;
  gNetworking.OnSetPassword := Lobby_OnSetPassword;
  gNetworking.OnAbortAllTransfers := Lobby_AbortAllTransfers;

  Radio_MapType.ItemIndex := gGameSettings.MenuLobbyMapType;
  UpdateMapList(aKind = lpkHost);

  //Hide RMG settings PopUp in case it was shown previosly
  if fGuiRMG <> nil then
    fGuiRMG.Hide;

  //Update chat
  SetChatHandlers;
  Edit_Post.Text := gChat.Text; //Update Edit text from Chat (game) only once
  Memo_Posts.ScrollToBottom;

  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    fLocalToSlot[I] := -1;
    fSlotToLocal[I] := -1;
  end;

  Panel_Lobby.Show;
  Lobby_Resize(aMainHeight);
end;


procedure TKMMenuLobby.UpdateDescNOptionsUI;
const
  PAD_Y = 7;
var
  diffHeight: Integer; //Difficulty panel height
begin
  diffHeight := Panel_Difficulty.Height * Byte(Panel_Difficulty.IsSetVisible);
  if Button_TabDesc.Visible then
  begin
    //Not enough space, so enabled tabbed view
    Panel_SetupDesc.Top := fPanelDescBaseTop + 25;
    Panel_SetupDesc.Height := fMainHeight - 415;
    Panel_SetupOptions.Top := fPanelDescBaseTop + 25;
  end
  else
  begin
    //We have enough space, so stack Options below Desc
    Panel_SetupDesc.Top := fPanelDescBaseTop;
    Panel_SetupDesc.Height := Panel_Setup.Height - Panel_SetupDesc.Top - diffHeight - PANEL_SETUP_OPTIONS_HEIGHT - PAD_Y;
    Panel_SetupOptions.Top := Panel_SetupDesc.Bottom + PAD_Y;
  end;
  Panel_SetupOptions.Height := PANEL_SETUP_OPTIONS_HEIGHT + diffHeight;
  Panel_GameOptions.Top := diffHeight;
end;


procedure TKMMenuLobby.Lobby_Resize(aMainHeight: Word);
begin
  if not Panel_Lobby.Visible then Exit;
  fMainHeight := aMainHeight;
  //If the vertical screen height goes below a certain amount we need to switch to "compact" mode
  if aMainHeight >= 660 then
  begin
    //We have enough space, so stack Options below Desc
    Button_TabDesc.Hide;
    Button_TabOptions.Hide;
    Panel_SetupDesc.Show;
    Panel_SetupOptions.Show;
    UpdateDescNOptionsUI;
  end
  else
  begin
    //Not enough space, so enabled tabbed view
    Button_TabDesc.Show;
    Button_TabOptions.Show;
    UpdateDescNOptionsUI;
    GameOptionsTabSwitch(nil);
  end;
end;


procedure TKMMenuLobby.GameOptionsTabSwitch(Sender: TObject);
begin
  if Sender = Button_TabDesc then
    fLobbyTab := ltDesc;
  if Sender = Button_TabOptions then
    fLobbyTab := ltOptions;

  case fLobbyTab of
    ltDesc:    begin
                 Panel_SetupDesc.Show;
                 Panel_SetupOptions.Hide;
               end;
    ltOptions: begin
                 Panel_SetupDesc.Hide;
                 Panel_SetupOptions.Show;
               end;
  end;
end;


procedure TKMMenuLobby.ResetDropColorRandom(aI: Integer);
begin
  DropBox_Colors[aI][0].Cells[0].Color := $FFFFFFFF;
  DropBox_Colors[aI][0].Cells[0].Pic.Id := 31;
end;


procedure TKMMenuLobby.EscKeyDown(Sender: TObject);
begin
  if Button_SettingsCancel.IsClickable then
    SettingsClick(Button_SettingsCancel)
  else
  if fGuiRMG.Visible then
    fGuiRMG.Hide;
end;


procedure TKMMenuLobby.KeyDown(Sender: TObject; Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:  if Panel_Settings.Visible then
                  SettingsClick(Button_SettingsSave);
  end;
end;


procedure TKMMenuLobby.BackClick(Sender: TObject);
begin
  //Scan should be terminated, it is no longer needed
  fMapsMP.TerminateScan;

  gNetworking.AnnounceDisconnect;
  gNetworking.Disconnect;

  fOnPageChange(gpMultiplayer, gResTexts[TX_GAME_ERROR_DISCONNECT]);
end;


//Reset everything to it's defaults depending on users role (Host/Joiner/Reassigned)
procedure TKMMenuLobby.Reset(aKind: TKMNetPlayerKind; aPreserveMaps: Boolean = False);
var
  I: Integer;
begin
  Label_ServerName.Caption := '';
  Image_PasswordLock.Hide;


  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    Label_Player[I].Caption := '.';
    Label_Player[I].FontColor := $FFFFFFFF;
    Image_Flag[I].TexID := 0;
    Image_Flag[I].HighlightOnMouseOver := False;
    Label_Player[I].Hide;
    PercentBar_PlayerDl_ChVisibility(I, False);
    DropBox_PlayerSlot[I].Visible := I <= MAX_LOBBY_PLAYERS; //Spectators hidden initially
    DropBox_PlayerSlot[I].Disable;
    DropBox_PlayerSlot[I].ItemIndex := 0; //Open
    DropBox_Loc[I].ItemIndex := 0;
    DropBox_Loc[I].Disable;
    DropBox_Loc[I].Visible := I <= MAX_LOBBY_PLAYERS; //Spectators hidden initially
    DropBox_Team[I].Disable;
    DropBox_Team[I].ItemIndex := 0;
    DropBox_Team[I].Visible := I <= MAX_LOBBY_PLAYERS; //Spectators hidden initially
    DropBox_Colors[I].Disable;
    DropBox_Colors[I].ItemIndex := 0;
    DropBox_Colors[I].Visible := I <= MAX_LOBBY_PLAYERS; //Spectators hidden initially
    ResetDropColorRandom(I);
    Image_Ready[I].TexID := 0;
    Label_Ping[I].Caption := '';
  end;

  Label_MapName.Caption := '';
  Memo_MapDesc.Clear;

  Memo_MapDesc.Height := Panel_SetupDesc.Height;
  Button_SetupReadme.Hide;

  TrackBar_LobbyPeacetime.Position := 0; //Default peacetime = 0
  TrackBar_SpeedPT.Position := 1; //Default speed = 1
  TrackBar_SpeedPT.ThumbText := 'x1';
  TrackBar_SpeedAfterPT.Position := 1; //Default speed = 1
  TrackBar_SpeedAfterPT.ThumbText := 'x1';

  Lobby_OnMapName('');

  //Setup for Host
  if aKind = lpkHost then
  begin
    Radio_MapType.Enable;
    Radio_MapType.ItemIndex := 0;

    if not aPreserveMaps then
      UpdateMapList(True);

    DropCol_Maps.Show;
    Label_MapName.Hide;
    Button_Start.Caption := gResTexts[TX_LOBBY_START]; //Start
    Button_Start.Disable;
    DropBox_Difficulty.Disable;
    TrackBar_LobbyPeacetime.Disable;
    TrackBar_SpeedPT.Disable;
    TrackBar_SpeedAfterPT.Disable;
    CheckBox_HostControl.Enable;
    CheckBox_RandomizeTeamLocations.Enable;
    CheckBox_Spectators.Enable;
    Button_ChangeSettings.Show;
  end
  else //Setup for Joiner
  begin
    Radio_MapType.Disable;
    Radio_MapType.ItemIndex := 0;
    DropCol_Maps.Hide;
    Label_MapName.Show;
    Button_Start.Caption := gResTexts[TX_LOBBY_READY]; //Ready
    Button_Start.Enable;
    DropBox_Difficulty.Disable;
    TrackBar_LobbyPeacetime.Disable;
    TrackBar_SpeedPT.Disable;
    TrackBar_SpeedAfterPT.Disable;
    CheckBox_HostControl.Disable;
    CheckBox_RandomizeTeamLocations.Disable;
    CheckBox_Spectators.Disable;
    Button_ChangeSettings.Hide;
  end;
  UpdateSpectatorDivide;
end;


procedure TKMMenuLobby.GameOptionsChange(Sender: TObject);
var
  MD: TKMMissionDifficulty;
begin
  if DropBox_Difficulty.Visible
    and (DropBox_Difficulty.Count > 0)
    and DropBox_Difficulty.IsSelected then
    MD := TKMMissionDifficulty(DropBox_Difficulty.GetSelectedTag)
  else
    MD := mdNone;

  //Update the game options
  gNetworking.UpdateGameOptions(EnsureRange(TrackBar_LobbyPeacetime.Position, 0, 300),
                                TrackBarPos2Speed(TrackBar_SpeedPT.Position),
                                TrackBarPos2Speed(TrackBar_SpeedAfterPT.Position),
                                MD);

  //Refresh the data to controls
  Lobby_OnGameOptions;
end;


procedure TKMMenuLobby.FileDownloadClick(Sender: TObject);
begin
  gNetworking.RequestFileTransfer;
  Button_SetupDownload.Hide;
  Lobby_OnFileTransferProgress(1, 0);
  PercentBar_SetupProgress.Caption := gResTexts[TX_LOBBY_DOWNLOADING];
end;


procedure TKMMenuLobby.ReadmeClick(Sender: TObject);
begin
  if not TryOpenMapPDF(gNetworking.MapInfo) then
    gChat.AddLine(gResTexts[TX_LOBBY_PDF_ERROR]);
end;


procedure TKMMenuLobby.UpdateGameOptionsUI;
begin
  TrackBar_SpeedPT.ThumbText := 'x' + FormatFloat('##0.##', TrackBarPos2Speed(TrackBar_SpeedPT.Position));
  TrackBar_SpeedAfterPT.ThumbText := 'x' + FormatFloat('##0.##', TrackBarPos2Speed(TrackBar_SpeedAfterPT.Position));
end;


procedure TKMMenuLobby.Lobby_OnGameOptions;
var
  MD: TKMMissionDifficulty;
begin
  TrackBar_LobbyPeacetime.Position := gNetworking.NetGameOptions.Peacetime;

  TrackBar_SpeedPT.Enabled   := (TrackBar_LobbyPeacetime.Position > 0) and TrackBar_SpeedAfterPT.Enabled;
  TrackBar_SpeedPT.Position  := Speed2TrackBarPos(gNetworking.NetGameOptions.SpeedPT);

  TrackBar_SpeedAfterPT.Position  := Speed2TrackBarPos(gNetworking.NetGameOptions.SpeedAfterPT);

  MD := gNetworking.NetGameOptions.MissionDifficulty;

  if MD <> mdNone then
    DropBox_Difficulty.SelectByTag(Byte(MD));

  UpdateGameOptionsUI;
end;


procedure TKMMenuLobby.HostMenuClick(Sender: TObject);
var
  id: Integer;
begin
  // We can't really do global bans because player's IP addresses change all the time (and we have no other way to identify someone).
  // My idea was for bans to be managed completely by the server, since player's don't actually know each other's IPs.
  // So the host says "please ban client 3257" and the server adds his IP to the ban list for this room. The ban list
  // is then reset when the room becomes empty. Maybe we need to call the button "ban from this lobby" instead.
  // In any way banlist should be editable from within the lobby, so we will need methods to get the list
  // from the server and allow to remove items from it.

  id := gNetworking.Room.ServerToLocal(TKMControl(Sender).Tag);
  if id = -1 then Exit; // Player has quit the lobby

  case PopUpMenu_Host.ItemIndex of
    0: gNetworking.KickPlayer(id);
    1: gNetworking.BanPlayer(id);
    2: gNetworking.SetToHost(id);
    3: ToggleMutePlayer(id);
  end;
end;


procedure TKMMenuLobby.JoinerMenuClick(Sender: TObject);
var
  id: Integer;
begin
  id := gNetworking.Room.ServerToLocal(TKMControl(Sender).Tag);
  if id = -1 then Exit; // Player has quit the lobby

  // Mute/Unmute
  if PopUpMenu_Joiner.ItemIndex = 0 then
    ToggleMutePlayer(id);
end;


function TKMMenuLobby.CanShowPlayerMenu(Sender: TObject): Boolean;
var
  ctrl: TKMControl;
begin
  Result := True;
  ctrl := TKMControl(Sender);
  if fLocalToSlot[ctrl.Tag] = -1 then
  begin
    Result := False;
    Exit;
  end;

  //Only human players (excluding ourselves) have the player menu
  if not gNetworking.Room[fLocalToSlot[ctrl.Tag]].IsHuman //No menu for AI players
  or (gNetworking.MySlotIndex = fLocalToSlot[ctrl.Tag]) //No menu for ourselves
  or not gNetworking.Room[fLocalToSlot[ctrl.Tag]].Connected then //Don't show menu for empty slots
  begin
    Result := False;
    Exit;
  end;
end;


procedure TKMMenuLobby.PercentBar_PlayerDl_ChVisibility(aPlayerIndex: Integer; aShow: Boolean);
begin
  if aShow then
  begin
    DropBox_Loc[aPlayerIndex].Hide;
    PercentBar_DownloadProgress[aPlayerIndex].DoSetVisible; //Don't use Show here, since it will Show parent panels as well...
  end else begin
    //Don't use Show here, since it will Show parent panels as well...
    //And since we call this method on disconnection we will see Lobby window over Multiplayer list window then...
    DropBox_Loc[aPlayerIndex].DoSetVisible;
    PercentBar_DownloadProgress[aPlayerIndex].Hide;
  end;
end;


procedure TKMMenuLobby.PlayerMenuShow(Sender: TObject);
var
  ctrl: TKMControl;
begin
  ctrl := TKMControl(Sender);
  if fLocalToSlot[ctrl.Tag] = -1 then Exit;

  if not CanShowPlayerMenu(Sender) then Exit;

  if gNetworking.IsHost then
  begin
    //Remember which player it is by his server index
    //since order of players can change. If someone above leaves we still have the proper Id
    PopUpMenu_Host.Tag := gNetworking.Room[fLocalToSlot[ctrl.Tag]].IndexOnServer;

    UpdateMuteMenuItem(PopUpMenu_Host, 3, gNetworking.IsMuted(fLocalToSlot[ctrl.Tag]));

    //Position the menu next to the icon, but do not overlap players name
    PopUpMenu_Host.ShowAt(ctrl.AbsLeft, ctrl.AbsTop + ctrl.Height);
  end else begin
    //Remember which player it is by his server index
    //since order of players can change. If someone above leaves we still have the proper Id
    PopUpMenu_Joiner.Tag := gNetworking.Room[fLocalToSlot[ctrl.Tag]].IndexOnServer;

    UpdateMuteMenuItem(PopUpMenu_Joiner, 0, gNetworking.IsMuted(fLocalToSlot[ctrl.Tag]));
    
    //Position the menu next to the icon, but do not overlap players name
    PopUpMenu_Joiner.ShowAt(ctrl.AbsLeft, ctrl.AbsTop + ctrl.Height);
  end;
end;


procedure TKMMenuLobby.ToggleMutePlayer(aPlayerIndex: Integer);
begin
  gNetworking.ToggleMuted(aPlayerIndex);
  UpdateImageLobbyFlag(fSlotToLocal[aPlayerIndex]);
end;


procedure TKMMenuLobby.UpdateMuteMenuItem(aMenu: TKMPopUpMenu; aItemIndex: Integer; aIsMuted: Boolean);
begin
  if aIsMuted then
    aMenu.UpdateItem(aItemIndex, gResTexts[TX_UNMUTE_PLAYER])
  else
    aMenu.UpdateItem(aItemIndex, gResTexts[TX_MUTE_PLAYER]);
end;


procedure TKMMenuLobby.UpdateImageLobbyFlag(aIndex: Integer);
begin
  // Darken player flag when muted
  if (fLocalToSlot[aIndex] <> -1) and gNetworking.IsMuted(fLocalToSlot[aIndex]) then
    Image_Flag[aIndex].Lightness := -0.66
  else
    Image_Flag[aIndex].Lightness := 0;
end;


function TKMMenuLobby.AISlotsAvailable(aAIPlayerTypes: TKMNetPlayerTypeSet = [AI_PLAYER_TYPE_MIN..AI_PLAYER_TYPE_MAX]): Byte;
var
  openedHumansAtAISlots: Byte;
begin
  Result := 0;

  aAIPlayerTypes := aAIPlayerTypes * [AI_PLAYER_TYPE_MIN..AI_PLAYER_TYPE_MAX]; //Restrict with AI player types only

  if (gNetworking.MapInfo <> nil) and gNetworking.MapInfo.IsValid then
  begin
    openedHumansAtAISlots := Max(0, gNetworking.MapInfo.CanBeHumanAndAICount - gNetworking.Room.GetConnectedPlayersCount);
    Result := Max(0, openedHumansAtAISlots
                   //+ gNetworking.MapInfo.CanBeOnlyAICount // Only AI is added at the start of the game...
                   - gNetworking.Room.GetAICount(aAIPlayerTypes));
  end else if (gNetworking.SaveInfo <> nil) and gNetworking.SaveInfo.IsValid then
  begin
    Result := Max(0, gNetworking.SaveInfo.GameInfo.HumanCount
                   - gNetworking.Room.GetConnectedPlayersCount
                   - gNetworking.Room.GetAICount(aAIPlayerTypes));
  end;
end;


procedure TKMMenuLobby.DropBoxPlayers_Show(Sender: TObject);
begin
  if Sender is TKMDropColumns then
    fDropBoxPlayers_LastItemIndex := TKMDropColumns(Sender).ItemIndex;
end;


procedure TKMMenuLobby.DropBoxPlayers_CellClick(Sender: TObject; const aCellX, aCellY: Integer; var aHandled: Boolean);

  function IsAILine: Boolean;
  begin
    Result := aCellY in [2,3];
  end;

var
  I, J, NetI: Integer;
  AISlotsToChange, AISlotsChanged: Byte;
  RowChanged: Boolean;
begin
  // Second column was clicked
  if aCellX = 1 then
  begin
    AISlotsChanged := 0;  //Used to count changed slots while setting ALL to AI
    AISlotsToChange := AISlotsAvailable([TKMNetPlayerType(aCellY)]); //Luckily row in column box is the same as TKMNetPlayer type (for AI)

    for I := 1 to MAX_LOBBY_SLOTS do
    begin
      if Sender = DropBox_PlayerSlot[I].List then
      begin
        DropBox_PlayerSlot[I].CloseList; //Close opened dropbox manually

        //We have to revert ItemIndex to its previous value, because its value was already switched to AI on MouseDown
        //but we are not sure yet about what value should be there, we will set it properly later on
        if IsAILine and (DropBox_PlayerSlot[I].ItemIndex = aCellY)
          and (fDropBoxPlayers_LastItemIndex <> -1) then
          DropBox_PlayerSlot[I].ItemIndex := fDropBoxPlayers_LastItemIndex;

        Break;
      end;
    end;

    for I := 1 to MAX_LOBBY_SLOTS do
    begin
      if IsAILine and (AISlotsChanged >= AISlotsToChange) then //Do not add more AI, then we have slots available
        Break;

      case aCellY of
        0:       J := MAX_LOBBY_SLOTS + 1 - I; // we must Open slots in reverse order
        1, 2, 3: J := I;                       // Closed and AI slots - in straight order
        else     J := I;
      end;

      RowChanged := False;
      NetI := fLocalToSlot[J];
      if (NetI = -1) or not gNetworking.Room[NetI].IsHuman then
      begin
        if DropBox_PlayerSlot[J].ItemIndex <> aCellY then //Do not count this slot as changed, if it already has same AI value
        begin
          RowChanged := True;
          Inc(AISlotsChanged);
        end;
        DropBox_PlayerSlot[J].ItemIndex := aCellY;
        // Do not call for PlayerChange if this row is AIPlayer and did not change (it was AIPlayer before that) - to avoid existing AIPlayer reset
        if RowChanged or not IsAILine then
          PlayersChange(DropBox_PlayerSlot[J]);
      end;
    end;

    // Do not propagate click event further, because
    // we do not want provoke OnChange event handler invokation, we have handled everything here
    aHandled := True;
  end;
end;


procedure TKMMenuLobby.UpdateDropColor_BlockColSel(I: Integer);
var
  ID: Integer;
  color: Cardinal;
begin
  if gNetworking.SelectGameKind = ngkNone then Exit;
  if (gNetworking.SelectGameKind = ngkMap) and not gNetworking.MapInfo.TxtInfo.BlockColorSelection then Exit;
  if (gNetworking.SelectGameKind = ngkSave) and not gNetworking.SaveInfo.GameInfo.TxtInfo.BlockColorSelection then Exit;

  if (DropBox_Loc[I].GetSelectedTag <> LOC_SPECTATE) then
  begin
    if DropBox_Loc[I].GetSelectedTag = LOC_RANDOM then
      ResetDropColorRandom(I)
    else
    begin
      ID := fLocalToSlot[I];
      case gNetworking.SelectGameKind of
        ngkMap:   color := gNetworking.MapInfo.FlagColors[DropBox_Loc[I].GetSelectedTag - 1];
        ngkSave:  color := gNetworking.SaveInfo.GameInfo.Color[DropBox_Loc[I].GetSelectedTag - 1];
        else      color := 0;
      end;

      DropBox_Colors[I][0].Cells[0].Color := color;
      DropBox_Colors[I][0].Cells[0].Pic.Id := 30;
      gNetworking.Room[ID].FlagColor := color;
    end;
    DropBox_Colors[I].ItemIndex := 0;
    DropBox_Colors[I].Disable;
  end
  else
    ResetDropColorRandom(I);
end;


procedure TKMMenuLobby.SetupChange(Sender: TObject);
begin
  //Host control toggle
  if Sender = CheckBox_HostControl then
  begin
    gNetworking.Room.HostDoesSetup := CheckBox_HostControl.Checked;
    gNetworking.SendPlayerListAndRefreshPlayersSetup;
  end;

  if Sender = CheckBox_RandomizeTeamLocations then
  begin
    gNetworking.Room.RandomizeTeamLocations := CheckBox_RandomizeTeamLocations.Checked;
    gNetworking.SendPlayerListAndRefreshPlayersSetup;
  end;

  if Sender = CheckBox_Spectators then
  begin
    if not CheckBox_Spectators.Checked and (gNetworking.Room.GetSpectatorCount > 0) then
    begin
      gNetworking.PostLocalMessage(gResTexts[TX_LOBBY_CANNOT_DISABLE_SPECTATORS], csSystem);
      CheckBox_Spectators.Checked := True;
    end
    else
    begin
      gNetworking.Room.SpectatorsAllowed := CheckBox_Spectators.Checked;
      gNetworking.SendPlayerListAndRefreshPlayersSetup;
    end;
  end;
end;


//Try to change players setup, Networking will check if it can be done under current
//conditions immediately and reverts the change without disturbing Host.
//If the change is possible Networking will send query to the Host.
//Host will reply with OnPlayersSetup event and data will be actualized.
procedure TKMMenuLobby.PlayersChange(Sender: TObject);
var
  I, slotIndex: Integer;
  col: Cardinal;
begin
  for I := 1 to MAX_LOBBY_SLOTS do
  begin
    slotIndex := fLocalToSlot[I];
    //Starting location
    if (Sender = DropBox_Loc[I]) and DropBox_Loc[I].Enabled then
    begin
      // We can still have cmSpectate chat mode if we were in specs. Reset to cmAll in this case
      if (DropBox_Loc[I].GetSelectedTag <> LOC_SPECTATE) and (gChat.Mode = cmSpectators) then
        ChatMenuSelect(CHAT_MENU_ALL);

      gNetworking.SelectHand(slotIndex, DropBox_Loc[I].GetSelectedTag);
      //Host with HostDoesSetup could have given us some location we don't know about
      //from a map/save we don't have, so make sure SelectGameKind is valid
      if (gNetworking.SelectGameKind <> ngkNone)
        and not gNetworking.IsHost then //Changes are applied instantly for host
        //Set loc back to Room value until host processes our request
        DropBox_Loc[I].SelectByTag(gNetworking.Room[slotIndex].StartLocation);
    end;

    //Team
    if (Sender = DropBox_Team[I]) and DropBox_Team[I].Enabled then
      gNetworking.SelectTeam(slotIndex, DropBox_Team[I].ItemIndex);

    //Color
    if (Sender = DropBox_Colors[I])
      and DropBox_Colors[I].Enabled
      and DropBox_Colors[I][DropBox_Colors[I].ItemIndex].Cells[0].Enabled then
    begin
      if (DropBox_Colors[I].ItemIndex = 0) then
        col := 0
      else
        col := DropBox_Colors[I][DropBox_Colors[I].ItemIndex].Cells[0].Color;

      gNetworking.SelectColor(slotIndex, col);
    end;

    if Sender = DropBox_PlayerSlot[I] then
    begin
      //Modify an existing player
      if (slotIndex <> -1) and (slotIndex <= gNetworking.Room.Count) then
      begin
        case DropBox_PlayerSlot[I].ItemIndex of
          0:  //Open
              begin
                if gNetworking.Room[slotIndex].IsComputer
                  or gNetworking.Room[slotIndex].IsClosed then
                  gNetworking.Room.RemPlayer(slotIndex);
              end;
          1:  //Closed
              gNetworking.Room.AddClosedPlayer(slotIndex); //Replace it
          2:  //AI
              gNetworking.Room.AddAIPlayer(False, slotIndex); //Replace it
          3:  //Advanced AI
              gNetworking.Room.AddAIPlayer(True, slotIndex); //Replace it
        end;
      end
      else
      begin
        if I > MAX_LOBBY_PLAYERS then
        begin
          //These are spectator only slots
          case DropBox_PlayerSlot[I].ItemIndex of
            0: gNetworking.Room.SpectatorSlotsOpen := MAX_LOBBY_SLOTS - I + 1;
            1: gNetworking.Room.SpectatorSlotsOpen := MAX_LOBBY_SLOTS - I;
          end;
        end
        else
        begin
          //Add a new player
          case DropBox_PlayerSlot[I].ItemIndex of
            1: gNetworking.Room.AddClosedPlayer;
            2: gNetworking.Room.AddAIPlayer(False);
            3: gNetworking.Room.AddAIPlayer(True);
          end;
        end;
      end;
      DropBox_PlayerSlot[I].CloseList; //We may have cause player list to rearrange
      gNetworking.SendPlayerListAndRefreshPlayersSetup;
    end;
  end;
end;


function TKMMenuLobby.Speed2TrackBarPos(aSpeed: Single): Integer;
begin
  Result := Round((aSpeed - 1) / SPEED_STEP) + 1;
end;


function TKMMenuLobby.TrackBarPos2Speed(aTrackPos: Integer): Single;
begin
  Result := (aTrackPos - 1) * SPEED_STEP + 1
end;


//Players list has been updated
//We should reflect it to UI
//Not very fast operation - ~4ms per player = ~60ms per all
procedure TKMMenuLobby.Lobby_OnPlayersSetup;

  function ConvertSpeedRange(aSpeedRng: TKMRangeSingle): TKMRangeInt;
  begin
    Result.Min := Speed2TrackBarPos(aSpeedRng.Min);
    Result.Max := Speed2TrackBarPos(aSpeedRng.Max);
  end;

  procedure AddLocation(LocationName: UnicodeString; aIndex, aLocation: Integer);
  begin
    if not gNetworking.CanTakeLocation(fLocalToSlot[aIndex], aLocation, False) then
      LocationName := '[$707070]' + LocationName + '[]';
    DropBox_Loc[aIndex].Add(LocationName, aLocation);
  end;

  function ImgReadyToStart(aSlot: TKMNetRoomSlot): Integer;
  begin
    if aSlot.IsSpectator then
    begin
      if not aSlot.ReadyToStart then
        Result := 32 //Not ready
      else
      begin
        if aSlot.HasMapOrSave then
          Result := 33 //Ready
        else
          Result := 88; //Spec ready, but need DL map
      end;
    end
    else
      Result := 32 + Byte(aSlot.ReadyToStart and aSlot.HasMapOrSave); //Not ready or ready
  end;

var
  I, K, freeColorsCnt,ID, localeID, colorID: Integer;
  myNik, canEdit, hostCanEdit, isSave, isValid: Boolean;
  curSlot: TKMNetRoomSlot;
  firstUnused: Boolean;
  fixedLocsColors: TKMCardinalArray;
  colorDist: Single;
  players: set of Byte;
  playersCnt, startLoc, rngPlayersTeam: Integer;
begin
  UpdateMappings;

  isSave := gNetworking.SelectGameKind = ngkSave;

  if Radio_MapType.ItemIndex < MAP_TYPE_INDEX_SAVE then //Limit PT for new game
    TrackBar_LobbyPeacetime.Range := gNetworking.NetGameFilter.PeacetimeRng
  else
    TrackBar_LobbyPeacetime.ResetRange; //No limit for saved game

  //Apply speed range filter for all games, even for saves
  TrackBar_SpeedPT.Range := ConvertSpeedRange(gNetworking.NetGameFilter.SpeedRng);
  TrackBar_SpeedAfterPT.Range := ConvertSpeedRange(gNetworking.NetGameFilter.SpeedAfterPTRng);

  UpdateGameOptionsUI;

  if gNetworking.IsSave then
    fixedLocsColors := gNetworking.SaveInfo.GameInfo.FixedLocsColors
  else
    fixedLocsColors := gNetworking.MapInfo.FixedLocsColors;

  firstUnused := True;
  for I := 1 to MAX_LOBBY_SLOTS do
    if fLocalToSlot[I] = -1 then
    begin
      //This player is unused
      Label_Player[I].Caption := '';
      Image_Flag[I].TexID := 0;
      Label_Player[I].Hide;
      PercentBar_PlayerDl_ChVisibility(I, False);
      DropBox_PlayerSlot[I].Show;
      if I > MAX_LOBBY_PLAYERS then
      begin
        //Spectator slots. Is this one open?
        if MAX_LOBBY_SLOTS - I < gNetworking.Room.SpectatorSlotsOpen then
        begin
          DropBox_PlayerSlot[I].ItemIndex := 0; //Spectator
          DropBox_PlayerSlot[I].Enabled := gNetworking.IsHost and (MAX_LOBBY_SLOTS - I + 1 = gNetworking.Room.SpectatorSlotsOpen);
        end
        else
        begin
          DropBox_PlayerSlot[I].ItemIndex := 1; //Closed
          DropBox_PlayerSlot[I].Enabled := gNetworking.IsHost and (MAX_LOBBY_SLOTS - I = gNetworking.Room.SpectatorSlotsOpen);
        end;
        DropBox_Loc[I].Clear;
        DropBox_Loc[I].Add(gResTexts[TX_LOBBY_SPECTATE], LOC_SPECTATE);

        DropBox_PlayerSlot[I].Visible := gNetworking.Room.SpectatorsAllowed;
        DropBox_Loc[I].Visible        := gNetworking.Room.SpectatorsAllowed;
        DropBox_Colors[I].Visible     := gNetworking.Room.SpectatorsAllowed;
      end
      else
      begin
        DropBox_PlayerSlot[I].ItemIndex := 0; //Open
        //Only host may change player slots, and only the first unused slot may be changed (so there are no gaps in net players list)
        DropBox_PlayerSlot[I].Enabled := gNetworking.IsHost and firstUnused;
        firstUnused := False;

        DropBox_Loc[I].Clear;
        if gNetworking.SelectGameKind = ngkSave then
          DropBox_Loc[I].Add(gResTexts[TX_LOBBY_SELECT], LOC_RANDOM)
        else
          DropBox_Loc[I].Add(gResTexts[TX_LOBBY_RANDOM], LOC_RANDOM);
      end;
      DropBox_Loc[I].ItemIndex := 0;
      DropBox_Team[I].ItemIndex := 0;
      DropBox_Colors[I].ItemIndex := 0;
      ResetDropColorRandom(I);
      Image_Ready[I].TexID := 0; //Hidden
      DropBox_Loc[I].Disable;
      DropBox_Team[I].Disable;
      DropBox_Colors[I].Disable;
      DropBox_Team[I].Visible := I <= MAX_LOBBY_PLAYERS;
    end
    else
    begin
      //This slot is used
      curSlot := gNetworking.Room[fLocalToSlot[I]];

      DropBox_Team[I].Visible := not curSlot.IsSpectator; // Spectators don't get a team
      DropBox_Loc[I].Show;
      DropBox_Colors[I].Show;

      //Flag icon
      if curSlot.IsComputer then
        Image_Flag[I].TexID := GetAIPlayerIcon(curSlot.PlayerNetType)
      else begin
        localeID := gResLocales.IndexByCode(curSlot.LangCode);
        if localeID <> -1 then
          Image_Flag[I].TexID := gResLocales[localeID].FlagSpriteID
        else
          Image_Flag[I].TexID := 0;
      end;

      //Players list
      if gNetworking.IsHost and (not curSlot.IsHuman) then
      begin
        Label_Player[I].Hide;
        PercentBar_PlayerDl_ChVisibility(I, False);
        DropBox_PlayerSlot[I].Enable;
        DropBox_PlayerSlot[I].Show;
        Assert(I <= MAX_LOBBY_PLAYERS, 'Spectator slots can''t have AI or closed');
        if curSlot.IsClassicComputer then
          DropBox_PlayerSlot[I].ItemIndex := 2 //Classic AI
        else if curSlot.IsAdvancedComputer then
          DropBox_PlayerSlot[I].ItemIndex := 3 //Advanced AI
        else
          DropBox_PlayerSlot[I].ItemIndex := 1; //Closed
      end
      else
      begin
        Label_Player[I].Caption := curSlot.SlotName;
        if curSlot.IsColorSet then
          Label_Player[I].FontColor := FlagColorToTextColor(curSlot.FlagColorDef)
        else
          Label_Player[I].FontColor := $FFFFFFFF;

        Label_Player[I].Show;
        PercentBar_PlayerDl_ChVisibility(I, False);
        DropBox_PlayerSlot[I].Disable;
        DropBox_PlayerSlot[I].Hide;
        DropBox_PlayerSlot[I].ItemIndex := 0; //Open
      end;

      //Starting locations
      //If we can't load the map, don't attempt to show starting locations
      isValid := False;
      DropBox_Loc[I].Clear;
      case gNetworking.SelectGameKind of
        ngkNone: AddLocation(gResTexts[TX_LOBBY_RANDOM], I, LOC_RANDOM);
        ngkSave: begin
                    isValid := gNetworking.SaveInfo.IsValid;
                    AddLocation(gResTexts[TX_LOBBY_SELECT], I, LOC_RANDOM);

                    for K := 0 to gNetworking.SaveInfo.GameInfo.PlayerCount - 1 do
                      if gNetworking.SaveInfo.GameInfo.Enabled[K]
                      and (gNetworking.SaveInfo.GameInfo.CanBeHuman[K] or ALLOW_TAKE_AI_PLAYERS) then
                        AddLocation(UnicodeString(gNetworking.SaveInfo.GameInfo.OwnerNickname[K]), I, K+1);
                  end;
        ngkMap:  begin
                    isValid := gNetworking.MapInfo.IsValid;
                    AddLocation(gResTexts[TX_LOBBY_RANDOM], I, LOC_RANDOM);

                    for K := 0 to gNetworking.MapInfo.LocCount - 1 do
                      //AI-only locations should not be listed for AIs in lobby, since those ones are
                      //automatically added when the game starts (so AI checks CanBeHuman too)
                      if (curSlot.IsHuman and (gNetworking.MapInfo.CanBeHuman[K] or ALLOW_TAKE_AI_PLAYERS))
                        or (curSlot.IsClassicComputer
                          and gNetworking.MapInfo.CanBeHuman[K]
                          and gNetworking.MapInfo.CanBeClassicAI[K])
                        or (curSlot.IsAdvancedComputer
                          and gNetworking.MapInfo.CanBeHuman[K]
                          and gNetworking.MapInfo.CanBeAdvancedAI[K]) then
                        AddLocation(gNetworking.MapInfo.LocationName(K), I, K+1);
                  end;
      end;
      if curSlot.IsHuman and gNetworking.Room.SpectatorsAllowed then
        AddLocation(gResTexts[TX_LOBBY_SPECTATE], I, LOC_SPECTATE);

      if isValid or curSlot.IsSpectator then
        DropBox_Loc[I].SelectByTag(curSlot.StartLocation)
      else
        DropBox_Loc[I].ItemIndex := 0;

      //Always show the selected teams, except when the map denies it
      if (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.TxtInfo.BlockTeamSelection then
        DropBox_Team[I].ItemIndex := 0 //Hide selected teams since they will be overridden
      else
        DropBox_Team[I].ItemIndex := curSlot.Team;

      colorID := FindMPColor(curSlot.FlagColor);
      // Reset color to random, in case our color was too close to AI only locs colors
      if (colorID <> 0) and IsColorCloseToColors(MP_PLAYER_COLORS[colorID], fixedLocsColors, MIN_PLAYER_COLOR_DIST) then
        colorID := 0;

      if (gNetworking.SelectGameKind <> ngkMap)
        or not gNetworking.MapInfo.TxtInfo.BlockColorSelection then
        DropBox_Colors[I].ItemIndex := colorID;

      UpdateDropColor_BlockColSel(I);

      //Disable colors that are unavailable
      colorDist := MIN_PLAYER_COLOR_DIST;
      repeat
        freeColorsCnt := 0;
        for K := 0 to DropBox_Colors[I].List.RowCount - 1 do
          if (K <> colorID) and (K <> 0)
          and (not gNetworking.Room.ColorAvailable(MP_PLAYER_COLORS[K])
               or ((gNetworking.SelectGameKind = ngkSave) and gNetworking.SaveInfo.GameInfo.ColorUsed(K))
               or IsColorCloseToColors(MP_PLAYER_COLORS[K], fixedLocsColors, MIN_PLAYER_COLOR_DIST)) then // Disable for AIOnly locs color (close to them)
            DropBox_Colors[I].List.Rows[K].Cells[0].Enabled := False
          else
          begin
            DropBox_Colors[I].List.Rows[K].Cells[0].Enabled := True;
            if K <> 0 then
              Inc(freeColorsCnt);
          end;
        colorDist := colorDist * 0.7; // color distance is reduced to find more colors, if needed
      until (freeColorsCnt > 0) or (colorDist < 0.001); //Try to find at least 1 free color. Stop when its hard to do

      if curSlot.IsClosed then
        Image_Ready[I].TexID := 0
      else
        Image_Ready[I].TexID := ImgReadyToStart(curSlot);

      myNik := (fLocalToSlot[I] = gNetworking.MySlotIndex); //Our index
      //We are allowed to edit if it is our nickname and we are set as NOT ready,
      //or we are the host and this player is an AI
      canEdit := (myNik and (gNetworking.IsHost or not gNetworking.Room.HostDoesSetup) and
                            (gNetworking.IsHost or not curSlot.ReadyToStart)) or
                 (gNetworking.IsHost and curSlot.IsComputer);
      hostCanEdit := (gNetworking.IsHost and gNetworking.Room.HostDoesSetup and
                      not curSlot.IsClosed);
      DropBox_Loc[I].Enabled := (canEdit or hostCanEdit);
      //Can't change color or teams in a loaded save (spectators can set color)
      //Can only edit teams for maps (not saves), but the map may deny this
      DropBox_Team[I].Enabled := (canEdit or hostCanEdit) and not curSlot.IsSpectator
                                      and (gNetworking.SelectGameKind = ngkMap)
                                      and not gNetworking.MapInfo.TxtInfo.BlockTeamSelection;
      DropBox_Colors[I].Enabled := (canEdit or (myNik and not curSlot.ReadyToStart))
                                        and (not isSave or curSlot.IsSpectator)
                                        and (    (gNetworking.SelectGameKind <> ngkMap)
                                           or not gNetworking.MapInfo.TxtInfo.BlockColorSelection
                                           or curSlot.IsSpectator);
      if myNik and not gNetworking.IsHost then
      begin
        if curSlot.ReadyToStart then
          Button_Start.Caption := gResTexts[TX_LOBBY_NOT_READY]
        else
          Button_Start.Caption := gResTexts[TX_LOBBY_READY];
      end;
    end;

  // Players flag highlight, if they are clickable
  for I := 1 to MAX_LOBBY_SLOTS do
    Image_Flag[I].HighlightOnMouseOver := CanShowPlayerMenu(Image_Flag[I]);

  // Darken player flag when muted
  for I := 1 to MAX_LOBBY_SLOTS do
    UpdateImageLobbyFlag(I);

  // If PopUp menu was opened, check if player still connected, otherwise - close PopUp menu
  if PopUpMenu_Host.Visible and (gNetworking.Room.ServerToLocal(PopUpMenu_Host.Tag) = -1) then
    PopUpMenu_Host.Hide;

  if PopUpMenu_Joiner.Visible and (gNetworking.Room.ServerToLocal(PopUpMenu_Joiner.Tag) = -1) then
    PopUpMenu_Joiner.Hide;

  //Update the minimap preview with player colors
  for I := 0 to MAX_HANDS - 1 do
  begin
    ID := gNetworking.Room.StartingLocToLocal(I+1);
    if (ID <> -1) then
      fMinimap.HandColors[I] := gNetworking.Room[ID].FlagColorDef(icBlack)
    else
      fMinimap.HandColors[I] := $7F000000; //Semi-transparent when not selected
  end;

  //If we have a map selected update the preview
  if (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid then
  begin
    fMinimap.Update(not gNetworking.MapInfo.TxtInfo.BlockFullMapPreview);

    // We want to show teams even if player did not chose his location
    // It possible if number of players with random loc = number of free locs and they all have same team chosen
    players := [];
    playersCnt := 0;
    rngPlayersTeam := 0;

    // We assume locs are in a row for now. From 1 to LocCount
    for I := 1 to gNetworking.MapInfo.LocCount do
      begin
        Include(players, I);
        Inc(playersCnt);
      end;

    // Find rngPlayersTeam
    for I := 1 to gNetworking.Room.Count do
    begin
      startLoc := gNetworking.Room[I].StartLocation;
      if startLoc > 0 then //Not LOC_RANDOM and not LOC_SPECTATE
      begin
        Exclude(players, I);
        Dec(playersCnt);
      end
      else
      if (startLoc = LOC_RANDOM)
        and (gNetworking.Room[I].Team <> 0)
        and ((rngPlayersTeam = 0) or (rngPlayersTeam = gNetworking.Room[I].Team)) then
      begin
        Dec(playersCnt);
        rngPlayersTeam := gNetworking.Room[I].Team;
      end;
    end;

    for I := 0 to MAX_HANDS - 1 do
    begin
      ID := gNetworking.Room.StartingLocToLocal(I+1);
      if ID <> -1 then
        fMinimap.HandTeam[I] := gNetworking.Room[ID].Team
      else
      begin
        if (playersCnt = 0) then
          fMinimap.HandTeam[I] := rngPlayersTeam
        else
          fMinimap.HandTeam[I] := 0; // We can't set rng team for player without loc chosen
      end;
    end;
  end;

  //If we are in team chat mode and find ourselves not on a team (player went back to no team), switch back to all
  if (gChat.Mode = cmTeam) and (gNetworking.MyRoomSlot.Team = 0) then
    ChatMenuSelect(CHAT_MENU_ALL);

  //If we are in whisper chat mode and find the player has left, switch back to all
  if gChat.Mode = cmWhisper then
  begin
    if gNetworking.Room.ServerToLocal(gChat.WhisperRecipient) = -1 then
      ChatMenuSelect(CHAT_MENU_ALL)
    else
      ChatMenuSelect(gChat.WhisperRecipient); //In case that player changed his color
  end;

  CheckBox_HostControl.Checked := gNetworking.Room.HostDoesSetup;
  CheckBox_RandomizeTeamLocations.Checked := gNetworking.Room.RandomizeTeamLocations;
  CheckBox_Spectators.Checked := gNetworking.Room.SpectatorsAllowed;
  if gNetworking.IsHost then
  begin
    Button_Start.Enabled := IsGameStartAllowed(gNetworking.CanStart);
    if gNetworking.CanStart in [gsmNoStartWithWarn, gsmStartWithWarn] then
      Button_Start.Caption := gResTexts[TX_MENU_LOBBY_TRY_TO_START]
    else
      Button_Start.Caption := gResTexts[TX_LOBBY_START];
  end;

  UpdateSpectatorDivide;
end;


procedure TKMMenuLobby.Lobby_OnPingInfo;
var
  I: Integer;
begin
  for I := 1 to MAX_LOBBY_SLOTS do
    if (gNetworking.Connected) and (fLocalToSlot[I] <> -1) and
       (gNetworking.Room[fLocalToSlot[I]].IsHuman) then
    begin
      Label_Ping[I].Caption := IntToStr(gNetworking.Room[fLocalToSlot[I]].GetInstantPing);
      Label_Ping[I].FontColor := GetPingColor(gNetworking.Room[fLocalToSlot[I]].GetInstantPing);
    end
    else
      Label_Ping[I].Caption := '';

  Label_ServerName.Caption := gNetworking.GetFullServerName;
end;


procedure TKMMenuLobby.InitDropColMapsList;
begin
  DropCol_Maps.DropWidth := 460;
  DropCol_Maps.SetColumns(fntOutline,
                          ['', gResTexts[TX_MENU_MAP_TITLE], '#', gResTexts[TX_MENU_MAP_SIZE]],
                          [0, 20, 320, 350],
                          [False, True, True, True]);
end;


procedure TKMMenuLobby.UpdateMapList(aIsHost: Boolean);
begin
  //Terminate any running scans otherwise they will continue to fill the drop box in the background
  fMapsMP.TerminateScan;
  fSavesMP.TerminateScan;
  DropCol_Maps.Clear; //Clear previous items in case scanning finds no maps/saves

  // can't use gNetworking.IsHost here, since we could just open lobby, and we didn't set gNetworking.PlayerKind
  if aIsHost then
  begin
    DropCol_Maps.Show;
    Label_MapName.Hide;
  end
  else
  begin
    DropCol_Maps.Hide;
    Label_MapName.Show;
  end;

  case Radio_MapType.ItemIndex of
    0,  //Build Map
    1,  //Fight Map
    2,  //Co-op Map
    3:  //Special map Map
        begin
          InitDropColMapsList;
          fMapsMP.Refresh(MapList_ScanUpdate, nil, MapList_ScanComplete);
          DropCol_Maps.DefaultCaption := gResTexts[TX_LOBBY_MAP_SELECT];
        end;
    MAP_TYPE_INDEX_RMG:  //RMG
        begin
          InitDropColMapsList;
          fMapsMP.Refresh(MapList_ScanUpdate, nil, MapList_ScanComplete);
//          DropCol_Maps.DefaultCaption := MAPS_RMG_NAME;
          DropCol_Maps.Hide;
          Label_MapName.Caption := MAPS_RMG_NAME;
          Label_MapName.Show;

          fGuiRMG.Show;
        end;
    MAP_TYPE_INDEX_SAVE:  //Saved Game
        begin
          fSavesMP.Refresh(MapList_ScanUpdate, True);
          DropCol_Maps.DropWidth := 850;
          DropCol_Maps.DefaultCaption := gResTexts[TX_LOBBY_MAP_SELECT_SAVED];
          DropCol_Maps.SetColumns(fntOutline,
                                  [gResTexts[TX_MENU_LOAD_FILE], '#', gResTexts[TX_MENU_SAVE_TIME], gResTexts[TX_MENU_LOAD_DATE],
                                   gResTexts[TX_MENU_LOAD_MAP_NAME], gResTexts[TX_MENU_LOAD_GAME_VERSION]],
                                  [0, 290, 320, 400, 540, 760]);
        end;
    else
        begin
          DropCol_Maps.DefaultCaption := NO_TEXT;
        end;
  end;
  DropCol_Maps.ItemIndex := -1; //Clear previously selected item
end;


procedure TKMMenuLobby.SelectRMGMap(); //RMG
begin
  if not gNetworking.IsHost then
    Exit; //Only host can select RMG map

  fMapsMP.Lock;
  try
    gNetworking.SelectMap(MAPS_RMG_NAME, mkMP);
  finally
    fMapsMP.Unlock;
  end;
  GameOptionsChange(nil); //Need to update GameOptions, since we could get new MissionDifficulty
end;


procedure TKMMenuLobby.MapTypeChanged(Sender: TObject);
var
  isRngChosen: Boolean; //RMG
begin
  isRngChosen := Radio_MapType.ItemIndex = MAP_TYPE_INDEX_RMG; //RMG
  UpdateMapList(gNetworking.IsHost);
  gGameSettings.MenuLobbyMapType := Radio_MapType.ItemIndex;
  if not isRngChosen then //RMG
    gNetworking.SelectNoMap('');
end;


// Change starting location
procedure TKMMenuLobby.MinimapLocClick(Sender: TObject; const aLoc: Integer);
var
  slotIndex: Integer;
  canEdit: Boolean;
begin
  slotIndex := gNetworking.MySlotIndex;

  canEdit := ((gNetworking.IsHost or not gNetworking.Room.HostDoesSetup) and
              (gNetworking.IsHost or not gNetworking.Room[slotIndex].ReadyToStart));

  if canEdit then
  begin
    gNetworking.SelectHand(slotIndex, aLoc + 1);
    //Host with HostDoesSetup could have given us some location we don't know about from a map/save we don't have
    if gNetworking.SelectGameKind <> ngkNone then
      DropBox_Loc[fSlotToLocal[slotIndex]].SelectByTag(gNetworking.Room[slotIndex].StartLocation);
  end;
end;


procedure TKMMenuLobby.MapList_SortUpdate(Sender: TObject);
begin
  //After sorting jump to the selected item
  if Sender = fSavesMP then
    RefreshSaveList(True);
  if Sender = fMapsMP then
    RefreshMapList(True);
end;


procedure TKMMenuLobby.MapList_ScanUpdate(Sender: TObject);
begin
  //Don't jump to selected with each scan update
  if Sender = fSavesMP then
    RefreshSaveList(False);
  if Sender = fMapsMP then
    RefreshMapList(False);
end;


procedure TKMMenuLobby.MapList_ScanComplete(Sender: TObject);
//var MapsCRCArray: TKMCardinalArray;
//    I: Integer;
begin
//--------//Do not RemoveMissing maps from favourites, as we have only 1 favorites for all maps, including SP maps also//--------//

//  if (Sender = fMapsMP) and (fMapsMP.Count > 0) then
//  begin
//    SetLength(MapsCRCArray, fMapsMP.Count);
//    for I := 0 to fMapsMP.Count - 1 do
//      MapsCRCArray[I] := fMapsMP[I].CRC;
//    gGameSettings.FavouriteMaps.RemoveMissing(MapsCRCArray);
//  end;
end;


procedure TKMMenuLobby.WakeUpNotReadyClick(Sender: TObject);
begin
  if TimeSince(fLastTimeAskReady) > ASK_READY_COOLDOWN then
  begin
    gNetworking.WakeUpNotReady;
    Button_SettingsAskReady.Disable;
    fLastTimeAskReady := TimeGet;
  end;
end;


procedure TKMMenuLobby.RefreshMapList(aJumpToSelected: Boolean);

  procedure SelectByName(const aName: UnicodeString);
  var
    I: Integer;
  begin
    for I := 0 to DropCol_Maps.Count - 1 do
      if DropCol_Maps.Item[I].Cells[1].Caption = aName then
      begin
        DropCol_Maps.ItemIndex := I;
        Break;
      end;
  end;

var
  I, prevTop: Integer;
  prevMap: string;
  addMap: Boolean;
  row: TKMListRow;
  lobbyCl: Cardinal;
begin
  // Reload settings because we could have updated favourite maps, f.e.
  gGameAppSettings.ReloadFavouriteMaps;

  //Remember previous map selected
  if DropCol_Maps.ItemIndex <> -1 then
    prevMap := DropCol_Maps.Item[DropCol_Maps.ItemIndex].Cells[1].Caption
  else
    prevMap := '';

  prevTop := DropCol_Maps.List.TopIndex;
  DropCol_Maps.Clear;

  fMapsMP.Lock;
  try
    for I := 0 to fMapsMP.Count - 1 do
    begin
      //Different modes allow different maps
      case Radio_MapType.ItemIndex of
        0, MAP_TYPE_INDEX_RMG:    
              addMap := fMapsMP[I].IsBuildingMission and not fMapsMP[I].TxtInfo.IsCoop and not fMapsMP[I].TxtInfo.IsSpecial; //BuildMap
        1:    addMap := fMapsMP[I].IsFightingMission and not fMapsMP[I].TxtInfo.IsCoop and not fMapsMP[I].TxtInfo.IsSpecial; //FightMap
        2:    addMap := fMapsMP[I].TxtInfo.IsCoop; //CoopMap
        3:    addMap := fMapsMP[I].TxtInfo.IsSpecial; //Special map
        else  addMap := False; //Other cases are already handled in Lobby_MapTypeSelect
      end;

      //Presect RMG map, if we have it in map list
      if gNetworking.IsHost
        and (Radio_MapType.ItemIndex = MAP_TYPE_INDEX_RMG)
        and (fMapsMP[I].Name = MAPS_RMG_NAME)
        and fMapsMP[I].TxtInfo.IsRMG then
        SelectRMGMap;

      if addMap and gNetworking.NetGameFilter.FilterMap(fMapsMP[I].CRC) then
      begin
        lobbyCl := fMapsMP[I].GetLobbyColor;

        row := MakeListRow(['', fMapsMP[I].Name, IntToStr(fMapsMP[I].HumanPlayerCountMP), fMapsMP[I].SizeText], //Texts
                           [lobbyCl, lobbyCl, lobbyCl, lobbyCl], //Colors
                           I);
        row.Cells[0].Pic := fMapsMP[I].FavouriteMapPic;
        row.Cells[0].HighlightOnMouseOver := True;
        DropCol_Maps.Add(row);
      end;
    end;
  finally
    fMapsMP.Unlock;
  end;

  //Restore previously selected map
  if prevMap <> '' then
  for I := 0 to DropCol_Maps.Count - 1 do
  if DropCol_Maps.Item[I].Cells[1].Caption = prevMap then
    DropCol_Maps.ItemIndex := I;

  //Restore the top index
  DropCol_Maps.List.TopIndex := prevTop;
  if aJumpToSelected and (DropCol_Maps.List.ItemIndex <> -1)
  and not InRange(DropCol_Maps.List.ItemIndex - DropCol_Maps.List.TopIndex, 0, DropCol_Maps.List.GetVisibleRows - 1) then
  begin
    if DropCol_Maps.List.ItemIndex < DropCol_Maps.List.TopIndex + DropCol_Maps.List.GetVisibleRows - 1 then
      DropCol_Maps.List.TopIndex := DropCol_Maps.List.ItemIndex
    else
    if DropCol_Maps.List.ItemIndex > DropCol_Maps.List.TopIndex + DropCol_Maps.List.GetVisibleRows - 1 then
      DropCol_Maps.List.TopIndex := DropCol_Maps.List.ItemIndex - DropCol_Maps.List.GetVisibleRows + 1;
  end;

  //After being reassigned to host we may need to reselect the map
  if (DropCol_Maps.ItemIndex = -1) and (gNetworking.SelectGameKind = ngkMap) then
    SelectByName(gNetworking.MapInfo.Name);
end;


procedure TKMMenuLobby.RefreshSaveList(aJumpToSelected: Boolean);
  procedure SelectByName(const aName: UnicodeString);
  var
    I: Integer;
  begin
    for I := 0 to DropCol_Maps.Count - 1 do
      if DropCol_Maps.Item[I].Cells[0].Caption = aName then
      begin
        DropCol_Maps.ItemIndex := I;
        Break;
      end;
  end;
var
  I, prevTop: Integer;
  prevSave, mapName: UnicodeString;
  color: Cardinal;
begin
  //Remember previous save selected
  if DropCol_Maps.ItemIndex <> -1 then
    prevSave := DropCol_Maps.Item[DropCol_Maps.ItemIndex].Cells[0].Caption
  else
    prevSave := '';

  prevTop := DropCol_Maps.List.TopIndex;

  DropCol_Maps.Clear;

  fSavesMP.Lock;
  try
    for I := 0 to fSavesMP.Count - 1 do
    begin
      if fSavesMP[I].IsValidStrictly then
      begin
        color := clSaveLoadOk;
        mapName := fSavesMP[I].GameInfo.Title;
      end
      else
      if fSavesMP[I].IsValid then
      begin
        color := clSaveLoadTry;
        mapName := gResTexts[TX_SAVE_UNSUPPORTED_VERSION_SHORT];
      end
      else
      begin
        color := clSaveLoadError;
        mapName := gResTexts[TX_SAVE_UNSUPPORTED_VERSION_SHORT];
      end;

      if color = clSaveLoadError then
        DropCol_Maps.Add(MakeListRow([fSavesMP[I].FileName, '', '', '', mapName, fSavesMP[I].GameInfo.VersionU],
                                     [color, color, color, color, color, color], I))
      else
        DropCol_Maps.Add(MakeListRow([fSavesMP[I].FileName,
                                      IntToStr(fSavesMP[I].GameInfo.PlayerCount),
                                      fSavesMP[I].GameInfo.GetTimeText,
                                      fSavesMP[I].GameInfo.GetSaveTimestamp,
                                      mapName,
                                      fSavesMP[I].GameInfo.VersionU],
                                     [color, color, color, color, color, color], I));
    end;

    //Restore previously selected save
    if prevSave <> '' then
    for I := 0 to DropCol_Maps.Count - 1 do
    if DropCol_Maps.Item[I].Cells[0].Caption = prevSave then
      DropCol_Maps.ItemIndex := I;
  finally
    fSavesMP.Unlock;
  end;

  //Restore the top index
  DropCol_Maps.List.TopIndex := prevTop;
  if aJumpToSelected and (DropCol_Maps.List.ItemIndex <> -1)
  and not InRange(DropCol_Maps.List.ItemIndex - DropCol_Maps.List.TopIndex, 0, DropCol_Maps.List.GetVisibleRows - 1) then
  begin
    if DropCol_Maps.List.ItemIndex < DropCol_Maps.List.TopIndex + DropCol_Maps.List.GetVisibleRows - 1 then
      DropCol_Maps.List.TopIndex := DropCol_Maps.List.ItemIndex
    else
    if DropCol_Maps.List.ItemIndex > DropCol_Maps.List.TopIndex + DropCol_Maps.List.GetVisibleRows - 1 then
      DropCol_Maps.List.TopIndex := DropCol_Maps.List.ItemIndex - DropCol_Maps.List.GetVisibleRows + 1;
  end;

  //After being reassigned to host we may need to reselect the save
  if (DropCol_Maps.ItemIndex = -1) and (gNetworking.SelectGameKind = ngkSave) then
    SelectByName(gNetworking.SaveInfo.FileName);
end;


procedure TKMMenuLobby.MapList_OnShow(Sender: TObject);
begin
  if fMapsSortUpdateNeeded then
  begin
    //Update sort
    if Radio_MapType.ItemIndex < MAP_TYPE_INDEX_SAVE then
      fMapsMP.Sort(fMapsMP.SortMethod, MapList_SortUpdate)
    else
      fSavesMP.Sort(fSavesMP.SortMethod, MapList_SortUpdate);
    fMapsSortUpdateNeeded := False;
  end;
end;


procedure TKMMenuLobby.MapColumnClick(Sender: TObject; const aColumn: Integer);
var
  SM: TKMapsSortMethod;
  SSM: TKMSavesSortMethod;
begin
  if Radio_MapType.ItemIndex < MAP_TYPE_INDEX_SAVE then
  begin
    //Determine Sort method depending on which column user clicked
    with DropCol_Maps.List do
    case SortIndex of
      0:  if SortDirection = sdDown then
            SM := smByFavouriteDesc
          else
            SM := smByFavouriteAsc;
      1:  if SortDirection = sdDown then
            SM := smByNameDesc
          else
            SM := smByNameAsc;
      2:  if SortDirection = sdDown then
            SM := smByHumanPlayersMPDesc
          else
            SM := smByHumanPlayersMPAsc;
      3:  if SortDirection = sdDown then
            SM := smBySizeDesc
          else
            SM := smBySizeAsc;
    else
      SM := smByNameAsc;
    end;
    fMapsMP.Sort(SM, MapList_SortUpdate);
  end
  else
  begin
    //Determine Sort method depending on which column user clicked
    with DropCol_Maps.List do
      case SortIndex of
        0:  if SortDirection = sdDown then
              SSM := smByFileNameDesc
            else
              SSM := smByFileNameAsc;
        1:  if SortDirection = sdDown then
              SSM := smByPlayerCountDesc
            else
              SSM := smByPlayerCountAsc;
        2:  if SortDirection = sdDown then
              SSM := smByTimeDesc
            else
              SSM := smByTimeAsc;
        3:  if SortDirection = sdDown then
              SSM := smByDateDesc
            else
              SSM := smByDateAsc;
        4:  if SortDirection = sdDown then
              SSM := smByMapNameDesc
            else
              SSM := smByMapNameAsc;
        5:  if SortDirection = sdDown then
              SSM := smByGameVersionDesc
            else
              SSM := smByGameVersionAsc;
      else
        SSM := smByFileNameAsc;
      end;
    fSavesMP.Sort(SSM, MapList_SortUpdate);
  end;
end;


procedure TKMMenuLobby.DropBoxMaps_CellClick(Sender: TObject; const aCellX, aCellY: Integer; var aHandled: Boolean);
var
  I: Integer;
begin
  if (Radio_MapType.ItemIndex < MAP_TYPE_INDEX_SAVE) and (aCellX = 0) then
  begin
    I := DropCol_Maps.Item[aCellY].Tag;
    fMapsMP.Lock;
    try
      // We could have updated favourite maps made by other game instances
      // Do not reload all other settings, cause we could have some settings in the memory only (selected maps f.e.)
      gGameAppSettings.ReloadFavouriteMaps;

      fMapsMP[I].IsFavourite := not fMapsMP[I].IsFavourite;
      if fMapsMP[I].IsFavourite then
      begin
        gGameSettings.FavouriteMaps.Add(fMapsMP[I].MapAndDatCRC);
        gServerSettings.ServerMapsRoster.Add(fMapsMP[I].CRC);
      end else begin
        gGameSettings.FavouriteMaps.Remove(fMapsMP[I].MapAndDatCRC);
        gServerSettings.ServerMapsRoster.Remove(fMapsMP[I].CRC);
      end;

      // Update pic
      DropCol_Maps.Item[aCellY].Cells[0].Pic := fMapsMP[I].FavouriteMapPic;
      fMapsSortUpdateNeeded := True; //Ask for resort on next list show

      // Save favourite maps immediately, thus updated favourite maps could be seen in the other game instances
      gGameAppSettings.SaveFavouriteMaps;
    finally
      fMapsMP.Unlock;
    end;
    aHandled := True; //we handle mouse click here, and do not want to propagate it further
  end;
end;


//Just pass FileName to Networking, it will check validity itself
procedure TKMMenuLobby.MapChange(Sender: TObject);
var
  I: Integer;
begin
  I := DropCol_Maps.Item[DropCol_Maps.ItemIndex].Tag;
  if Radio_MapType.ItemIndex < MAP_TYPE_INDEX_SAVE then
  begin
    fMapsMP.Lock;
    try
      gNetworking.SelectMap(fMapsMP[I].Name, fMapsMP[I].Kind);
    finally
      fMapsMP.Unlock;
    end;
    GameOptionsChange(nil); //Need to update GameOptions, since we could get new MissionDifficulty
    Button_Start.Caption := gResTexts[TX_LOBBY_START];
  end
  else
  begin
    fSavesMP.Lock;
    try
      gNetworking.SelectSave(fSavesMP[I].FileName);
//      if True then

//      Button_Start.Caption := gResTexts[TX_MENU_LOBBY_TRY_TO_START];
    finally
      fSavesMP.Unlock;
    end;
  end;
end;


procedure TKMMenuLobby.Lobby_OnUpdateMinimap;
var
  si: TKMSaveInfo;
begin
  if not gNetworking.IsSave then Exit;

  si := gNetworking.SaveInfo;

  if si.IsValid
  and (gNetworking.MySlotIndex > 0)
  and si.LoadMinimap(fMinimap, gNetworking.MyRoomSlot.StartLocation) then
    MinimapView.Show
  else
    MinimapView.Hide;
end;


procedure TKMMenuLobby.UpdateDifficultyLevels(aSave: TKMSaveInfo);
var
  MD: TKMMissionDifficulty;
begin
  //Difficulty levels
  DropBox_Difficulty.Clear;
  DropBox_Difficulty.Disable;
  MD := aSave.GameInfo.MissionDifficulty;
  if MD = mdNone then
    Panel_Difficulty.Hide
  else
  begin
    DropBox_Difficulty.Add(gResTexts[DIFFICULTY_LEVELS_TX[MD]], Byte(MD));
    DropBox_Difficulty.ItemIndex := 0; //Select level from save

    Panel_Difficulty.DoSetVisible;
  end;

  UpdateDescNOptionsUI;
end;


procedure TKMMenuLobby.UpdateDifficultyLevels(aMap: TKMMapInfo);
var
  I: Integer;
  MD, oldMD, defMD: TKMMissionDifficulty;
begin
  //Difficulty levels
  if aMap.TxtInfo.HasDifficultyLevels then
  begin
    oldMD := mdNone;
    if DropBox_Difficulty.IsSelected
      and (TKMMissionDifficulty(DropBox_Difficulty.GetSelectedTag) in aMap.TxtInfo.DifficultyLevels) then
      oldMD := TKMMissionDifficulty(DropBox_Difficulty.GetSelectedTag);

    DropBox_Difficulty.Clear;
    I := 0;

    if oldMD <> mdNone then
      defMD := oldMD     //Try to set value from previously selected map
    else
      defMD := mdNormal; //Default diffiuculty is "Normal"

    for MD in aMap.TxtInfo.DifficultyLevels do
    begin
      DropBox_Difficulty.Add(gResTexts[DIFFICULTY_LEVELS_TX[MD]], Byte(MD));
      if MD = defMD then
        DropBox_Difficulty.ItemIndex := I;

      Inc(I);
    end;

    if not DropBox_Difficulty.IsSelected then
      DropBox_Difficulty.ItemIndex := 0;

    Panel_Difficulty.DoSetVisible;
    DropBox_Difficulty.Enabled := gNetworking.IsHost; //Only Host can change map difficulty

  end else
    Panel_Difficulty.Hide;

  UpdateDescNOptionsUI;
end;


//We have received MapName
//Update UI to show it
procedure TKMMenuLobby.Lobby_OnMapName(const aData: UnicodeString);
var
  M: TKMMapInfo;
  S: TKMSaveInfo;
  txt: UnicodeString;
begin
  //Common settings
  MinimapView.Visible := (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid;
  TrackBar_LobbyPeacetime.Enabled := gNetworking.IsHost
                                     and (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid
                                     and not gNetworking.MapInfo.TxtInfo.BlockPeacetime;
  TrackBar_SpeedPT.Enabled := (TrackBar_LobbyPeacetime.Position > 0) and gNetworking.IsHost
                                    and (((gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid)
                                      or ((gNetworking.SelectGameKind = ngkSave) and gNetworking.SaveInfo.IsValid));
  TrackBar_SpeedAfterPT.Enabled := gNetworking.IsHost
                                        and (((gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid)
                                          or ((gNetworking.SelectGameKind = ngkSave) and gNetworking.SaveInfo.IsValid));
  CheckBox_RandomizeTeamLocations.Enabled := gNetworking.IsHost and (gNetworking.SelectGameKind <> ngkSave);

  DropBox_Difficulty.Enabled := gNetworking.IsHost and (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid;

  //In case it was hidden during file transfer
  Panel_SetupTransfer.Hide;
  Panel_SetupMinimap.Show;

  //Don't reset the selection if no map is selected
  if ((gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid)
    or ((gNetworking.SelectGameKind = ngkSave) and gNetworking.SaveInfo.IsValid) then
    Radio_MapType.ItemIndex := DetectMapType;

  Memo_MapDesc.Height := Panel_SetupDesc.Height;
  Button_SetupReadme.Hide;

  case gNetworking.SelectGameKind of
    ngkNone: begin
                Memo_MapDesc.Clear;
                if aData = '' then
                  Label_MapName.Caption := gResTexts[TX_LOBBY_MAP_NONE]
                else
                begin
                  Memo_MapDesc.Add(aData);
                  Label_MapName.Caption := aData; //Some error message
                end;
              end;
    ngkSave: begin
                S := gNetworking.SaveInfo;
                Label_MapName.Caption := aData; //Show save name on host (local is always "downloaded")
                txt := S.GameInfo.GetTitleWithTime + '|' + S.GameInfo.GetSaveTimestamp;
                if not S.IsValid then
                  txt := S.SaveError.ErrorString + '||' + txt
                else
                if not S.IsValidStrictly then //Allow try Load unsupported saves
                  txt := WrapColor(S.SaveError.ErrorString, clSaveLoadTry) + '||' +
                         WrapColor(gResTexts[TX_UNSUPPORTED_SAVE_LOAD_WARNING_TXT], clSaveLoadError) + '||' + txt;
                Memo_MapDesc.Text := txt;
                Lobby_OnUpdateMinimap;
                UpdateDifficultyLevels(S);
              end;
    ngkMap:  begin
                M := gNetworking.MapInfo;

                //Only load the minimap preview if the map is valid
                if M.IsValid then
                begin
                  fMinimap.LoadFromMission(M.FullPath('.dat'), M.HumanUsableLocs);
                  fMinimap.Update(not M.TxtInfo.BlockFullMapPreview);

                  if not TrackBar_LobbyPeacetime.Enabled and gNetworking.IsHost then
                  begin
                    TrackBar_LobbyPeacetime.Position := 0; //No peacetime in coop (trackbar gets disabled above)
                    GameOptionsChange(nil); //Send it to other clients
                  end;

                  if M.HasReadme then
                  begin
                    Memo_MapDesc.Height := Panel_SetupDesc.Height - 30;
                    Button_SetupReadme.Show;
                  end;

                  UpdateDifficultyLevels(M);
                end;
                Label_MapName.Caption := WrapColor(M.Name, M.GetLobbyColor);
                Memo_MapDesc.Text := M.BigDesc;
            end;
  end;
end;


procedure TKMMenuLobby.Lobby_OnMapMissing(const aData: UnicodeString; aStartTransfer: Boolean);
begin
  //Common settings
  MinimapView.Visible := (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid;
  TrackBar_LobbyPeacetime.Enabled := gNetworking.IsHost and (gNetworking.SelectGameKind = ngkMap)
                                     and gNetworking.MapInfo.IsValid and not gNetworking.MapInfo.TxtInfo.BlockPeacetime;
  TrackBar_SpeedPT.Enabled := (TrackBar_LobbyPeacetime.Position > 0) and gNetworking.IsHost
                               and (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid;
  TrackBar_SpeedAfterPT.Enabled := gNetworking.IsHost and (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid;
  CheckBox_RandomizeTeamLocations.Enabled := gNetworking.IsHost and (gNetworking.SelectGameKind <> ngkSave);

  DropBox_Difficulty.Enabled := gNetworking.IsHost and (gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid;

  Label_MapName.Caption := gNetworking.MissingBundleName;
  Memo_MapDesc.Text := aData; //aData is some error message
  if gNetworking.MissingBundleType = ngkSave then
    Radio_MapType.ItemIndex := MAP_TYPE_INDEX_SAVE
  else
    Radio_MapType.ItemIndex := 0;
  Panel_SetupMinimap.Hide;
  Panel_SetupTransfer.Show;
  Button_SetupDownload.Show;
  Lobby_OnFileTransferProgress(0, 0); //Reset progress bar
  if aStartTransfer then
    FileDownloadClick(nil);
end;


//We have been assigned to be the host of the game because the host disconnected. Reopen lobby page in correct mode.
procedure TKMMenuLobby.Lobby_OnReassignedToHost;
begin
  Reset(lpkHost, True); //Will reset the lobby page into host mode, preserving messages/maps

  //Pick correct position of map type selector
  Radio_MapType.ItemIndex := DetectMapType;

  UpdateMapList(True);
  Lobby_OnGameOptions;

  case gNetworking.SelectGameKind of
    ngkMap:  Lobby_OnMapName(gNetworking.MapInfo.Name);
    ngkSave: Lobby_OnMapName(gNetworking.SaveInfo.FileName);
  end;
end;


procedure TKMMenuLobby.Lobby_OnReassignedToJoiner;
begin
  Reset(lpkJoiner, True); //Will reset the lobby page into host mode, preserving messages/maps

  //Pick correct position of map type selector
  Radio_MapType.ItemIndex := DetectMapType;
end;


procedure TKMMenuLobby.HandleError(const aMsg: UnicodeString);
begin
  gNetworking.PostLocalMessage(aMsg, csSystem);
end;


procedure TKMMenuLobby.PostLocalMsg(const aMsg: UnicodeString);
begin
  gNetworking.PostLocalMessage(aMsg, csChat);
end;


procedure TKMMenuLobby.PostMsg(const aMsg: UnicodeString);
begin
  if gChat.Mode = cmWhisper then
    gNetworking.PostChat(aMsg, gChat.Mode, gChat.WhisperRecipient)
  else
    gNetworking.PostChat(aMsg, gChat.Mode);
end;


function TKMMenuLobby.IsKeyEvent_Return_Handled(Sender: TObject; Key: Word): Boolean;
begin
  Result := Key in [VK_RETURN, VK_UP, VK_DOWN];
end;


function TKMMenuLobby.PostKeyDown(Sender: TObject; Key: Word; Shift: TShiftState): Boolean;
var
  str: String;
begin
  Result := False;
  if IsKeyEvent_Return_Handled(Self, Key) then
  begin
    case Key of
      VK_RETURN:  Result := DoPost;
      VK_UP:      begin
                    str := gChat.GetNextHistoryMsg;
                    if str <> '' then
                    begin
                      Edit_Post.Text := str;
                      Result := True;
                    end;
                  end;
      VK_DOWN:    begin
                    str := gChat.GetPrevHistoryMsg;
                    if str <> '' then
                    begin
                      Edit_Post.Text := str;
                      Result := True;
                    end;
                  end;
    end;
  end;
end;


//Post what user has typed
function TKMMenuLobby.DoPost: Boolean;
var
  recipientNetIndex: Integer;
begin
  Result := False;
  if not gChat.IsPostAllowed then
    Exit;

  //Console commands are disabled for now, maybe we'll reuse them later
  //Check for console commands
  {if (Length(ChatMessage) > 1) and (ChatMessage[1] = '/')
  and (ChatMessage[2] <> '/') then //double slash is the escape to place a slash at the start of a sentence
    gNetworking.ConsoleCommand(ChatMessage)
  else
  begin
    if (Length(ChatMessage) > 1) and (ChatMessage[1] = '/') and (ChatMessage[2] = '/') then
      Delete(ChatMessage, 1, 1); //Remove one of the /'s
  end;}

  if gChat.Mode = cmWhisper then
  begin
    recipientNetIndex := gNetworking.Room.ServerToLocal(gChat.WhisperRecipient);
    if not gNetworking.Room[recipientNetIndex].Connected
      or gNetworking.Room[recipientNetIndex].Dropped then
    begin
      gNetworking.PostLocalMessage(Format(gResTexts[TX_MULTIPLAYER_CHAT_PLAYER_NOT_CONNECTED_ANYMORE],
                                          [gNetworking.Room[recipientNetIndex].NicknameColored]),
                                    csSystem);
      ChatMenuSelect(CHAT_MENU_ALL);
    end else
      gChat.Post
  end else
    gChat.Post;
  Result := True;
  Edit_Post.Text := '';
  Memo_Posts.ScrollToBottom;
end;


procedure TKMMenuLobby.Lobby_OnMessage(const aText: UnicodeString);
begin
  if gChat <> nil then
    gChat.AddLine(aText);
end;


//We were disconnected from Server. Either we were kicked, or connection broke down
procedure TKMMenuLobby.Lobby_OnDisconnect(const aReason: UnicodeString);
begin
  gNetworking.Disconnect;
  gSoundPlayer.Play(sfxnError);

  fOnPageChange(gpMultiplayer, aReason);
end;


procedure TKMMenuLobby.Lobby_OnFileTransferProgress(aTotal, aProgress: Cardinal);
begin
  if aTotal = 0 then
  begin
    PercentBar_SetupProgress.Hide;
    PercentBar_SetupProgress.SetCaptions('', gResTexts[TX_LOBBY_DOWNLOADING], '');
  end else begin
    PercentBar_SetupProgress.Show;
    PercentBar_SetupProgress.Position := aProgress / aTotal;
    PercentBar_SetupProgress.SetCaptions(IntToStr(aProgress div 1024) + 'kb ', '/', ' ' + IntToStr(aTotal div 1024) + 'kb');
  end;
end;


procedure TKMMenuLobby.Lobby_OnPlayerFileTransferProgress(aNetPlayerIndex: Integer; aTotal, aProgress: Cardinal);
var
  row: Integer;
begin
  row := fSlotToLocal[aNetPlayerIndex];
  if (aProgress >= aTotal) or (aTotal = 0) then
  begin
    PercentBar_DownloadProgress[row].Position := 0;
    PercentBar_DownloadProgress[row].SetCaptions('', gResTexts[TX_LOBBY_DOWNLOADING], '');
    PercentBar_PlayerDl_ChVisibility(row, False);
  end else begin
    PercentBar_DownloadProgress[row].Position := aProgress / aTotal;
    PercentBar_DownloadProgress[row].SetCaptions(IntToStr(aProgress div 1024) + 'kb ', '/', ' ' + IntToStr(aTotal div 1024) + 'kb');
    PercentBar_PlayerDl_ChVisibility(row, True);
  end;
end;


procedure TKMMenuLobby.Lobby_OnSetPassword(const aPassword: AnsiString);
begin
  Image_PasswordLock.Visible := aPassword <> '';
end;


//Abort all transfers
procedure TKMMenuLobby.Lobby_AbortAllTransfers;
var
  I: Integer;
begin
  for I := 1 to MAX_LOBBY_SLOTS do
    PercentBar_PlayerDl_ChVisibility(I, False);
  gNetworking.Room.SetDownloadAborted; //Mark all players as not downloading
end;


procedure TKMMenuLobby.StartBtnChangeEnabled(Sender: TObject; aEnable: Boolean);
begin
  Button_SettingsAskReady.Enabled := (((gNetworking.MapInfo <> nil) and gNetworking.MapInfo.IsValid)
                                        or ((gNetworking.SaveInfo <> nil) and gNetworking.SaveInfo.IsValid))
                                     and not gNetworking.Room.AllReady;
end;


procedure TKMMenuLobby.StartClick(Sender: TObject);
var
  loadError, version, path: UnicodeString;
begin
  if gNetworking.IsHost then
  begin
    if gNetworking.IsSave then
    begin
      version := gNetworking.SaveInfo.GameInfo.VersionU;
      path := gNetworking.SaveInfo.Path;
      if not gNetworking.SaveInfo.IsValidStrictly then //We are trying to load other version save
      begin
        try
          gNetworking.StartClick;
        except
          on E: Exception do
          begin
            loadError := Format(gResTexts[TX_UNSUPPORTED_SAVE_LOAD_ERROR_MSG], [version, path])
                         + '||' + E.ClassName + ': ' + E.Message;
            gLog.AddTime('Replay load Exception: ' + loadError
              {$IFDEF WDC} + sLineBreak + E.StackTrace {$ENDIF}
              );
            fOnPageChange(gpError, loadError);
          end;
        end;
      end
      else
        gNetworking.StartClick;
    end
    else
      gNetworking.StartClick;
  end
  else
  begin
    if gNetworking.ReadyToStart then
      Button_Start.Caption := gResTexts[TX_LOBBY_NOT_READY]
    else
      Button_Start.Caption := gResTexts[TX_LOBBY_READY];
  end;
end;


procedure TKMMenuLobby.SettingsClick(Sender: TObject);
begin
  if Sender = Button_ChangeSettings then
  begin
    Edit_Description.Text := gNetworking.Description;
    Edit_Password.Text := UnicodeString(gNetworking.Password);
    Button_SettingsUseLastPassword.Enabled := Trim(gGameSettings.LastPassword) <> '';
    Panel_Settings.Show;
  end;

  if Sender = Button_SettingsResetBans then
  begin
    if TimeSince(fLastTimeResetBans) > RESET_BANS_COOLDOWN then
    begin
      gNetworking.ResetBans;
      Button_SettingsResetBans.Disable;
      fLastTimeResetBans := TimeGet;
    end;
  end;

  if Sender = Button_SettingsUseLastPassword then
    Edit_Password.Text := gGameSettings.LastPassword;

  if Sender = Button_SettingsCancel then
    Panel_Settings.Hide;

  if Sender = Button_SettingsSave then
  begin
    Panel_Settings.Hide;
    gNetworking.Description := Edit_Description.Text;
    gNetworking.SetPassword(AnsiString(Edit_Password.Text));
    if Checkbox_RememberPassword.Checked then
      gGameSettings.LastPassword := UnicodeString(gNetworking.Password);
  end;
end;


procedure TKMMenuLobby.ReturnToLobby(const aSaveName: UnicodeString);
begin
  Radio_MapType.ItemIndex := MAP_TYPE_INDEX_SAVE; //Save
  UpdateMapList(gNetworking.IsHost);
  Lobby_OnGameOptions;
  if gNetworking.IsHost then
  begin
    gNetworking.SelectSave(aSaveName);

    //Make sure the save was successfully selected
    if ((gNetworking.SelectGameKind = ngkMap) and gNetworking.MapInfo.IsValid)
      or ((gNetworking.SelectGameKind = ngkSave) and gNetworking.SaveInfo.IsValid) then
      Radio_MapType.ItemIndex := DetectMapType;

    if gNetworking.SelectGameKind = ngkSave then
      Lobby_OnMapName(aSaveName);
  end;
end;


//Should update anything we want to be updated, obviously
procedure TKMMenuLobby.UpdateState;
begin
  fMapsMP.UpdateState;
  fSavesMP.UpdateState;

  if (fLastTimeResetBans <> 0) and (TimeSince(fLastTimeResetBans) > RESET_BANS_COOLDOWN) then
  begin
    Button_SettingsResetBans.Enable;
    fLastTimeResetBans := 0;
  end;

  if (fLastTimeAskReady <> 0) and (TimeSince(fLastTimeAskReady) > ASK_READY_COOLDOWN) then
  begin
    StartBtnChangeEnabled(Button_Start, Button_Start.Enabled);
    fLastTimeAskReady := 0;
  end;
end;


end.
