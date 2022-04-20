unit KM_GUIMapEdMissionPlayers;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsPopUp, KM_ControlsSwitch,
  KM_Defaults, KM_Pics;

type
  TKMMapEdPlayerType = (mptDefault, mptHuman, mptClassicAI, mptAdvancedAI);

  TKMMapEdMissionPlayers = class
  private
    fPlayerIdToDelete: TKMHandID;

    procedure Mission_PlayerTypesChange(Sender: TObject);
    procedure Mission_PlayerTypesAllClick(Sender: TObject);
    procedure Mission_PlayerIdUpdate;
    procedure PlayerDelete_Click(Sender: TObject);
    procedure ClosePlayerTypes_Click(Sender: TObject);

    procedure PlayerDeleteConfirm(aVisible: Boolean);
  protected
    Panel_PlayerTypes: TKMPopUpPanel;
      ChkBox_PlayerTypes: array [0..MAX_HANDS-1, TKMMapEdPlayerType] of TKMCheckBox;
      ChkBox_PlayerTypesAll: array [mptHuman..mptAdvancedAI] of TKMCheckBox;
      Label_PlayerTypesAll: TKMLabel;
      Label_PlayerId: array [0..MAX_HANDS-1] of TKMLabel;
      Button_Close: TKMButton;

    PopUp_Confirm_PlayerDelete: TKMPopUpMenu;
      Image_Confirm_PlayerDelete: TKMImage;
      Button_PlayerDelete, Button_PlayerDeleteConfirm, Button_PlayerDeleteCancel: TKMButton;
      Label_PlayerDeleteConfirmTitle, Label_PlayerDeleteConfirm: TKMLabel;
  public
    constructor Create(aParent: TKMPanel);

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);

    procedure Show;
    function Visible: Boolean;
    procedure Hide;

    procedure UpdatePlayerTypes;
    procedure UpdatePlayer(aIndex: TKMHandID = -1);
  end;


implementation
uses
  KM_Game,
  KM_HandsCollection, KM_Hand,
  KM_ResTexts, KM_ResFonts, KM_ResTypes,
  KM_RenderUI;

const
  PANEL_W  = 200;
  LINE_H = 22;
  ICON_SPACE_W = 25;
  ICON_SPACE_H = 32;

  PLAYER_TYPE_TX: array [mptHuman..mptAdvancedAI] of Integer = (TX_PLAYER_HUMAN, TX_AI_PLAYER_CLASSIC, TX_AI_PLAYER_ADVANCED);

{ TKMMapEdMissionPlayers }
constructor TKMMapEdMissionPlayers.Create(aParent: TKMPanel);
var
  I, Top, PanelH: Integer;
  MPT: TKMMapEdPlayerType;
begin
  inherited Create;

  fPlayerIdToDelete := -1;

  PanelH := LINE_H * MAX_HANDS + 140;

  Panel_PlayerTypes := TKMPopUpPanel.Create(aParent.MasterParent, PANEL_W, PanelH + 20, gResTexts[TX_MAPED_PLAYERS_TYPE],
                                            pubgitYellow, False, False);
  Top := 0;
  TKMLabel.Create(Panel_PlayerTypes.ItemsPanel,  13, Top, 20, 20, '#', fntGrey, taLeft);

  with TKMLabel.Create(Panel_PlayerTypes.ItemsPanel, 33, Top, 30, 20, gResTexts[TX_MAPED_PLAYERS_DEFAULT_SHORT], fntGrey, taLeft) do
    Hint := gResTexts[TX_MAPED_PLAYERS_DEFAULT];
  with TKMImage.Create(Panel_PlayerTypes.ItemsPanel,84, Top, 60, 20, 588, rxGui) do
    Hint := gResTexts[TX_PLAYER_HUMAN];
  with TKMImage.Create(Panel_PlayerTypes.ItemsPanel,127, Top, 20, 20,  62, rxGuiMain) do
    Hint := gResTexts[TX_AI_PLAYER_CLASSIC];
  with TKMImage.Create(Panel_PlayerTypes.ItemsPanel,169, Top, 20, 20,  74, rxGuiMain) do
    Hint := gResTexts[TX_AI_PLAYER_ADVANCED];

  Inc(Top, 25);
  for I := 0 to MAX_HANDS - 1 do
  begin

    Label_PlayerId[I] := TKMLabel.Create(Panel_PlayerTypes.ItemsPanel,  13, Top, 20, 20, IntToStr(I+1), fntOutline, taLeft);

    for MPT := Low(TKMMapEdPlayerType) to High(TKMMapEdPlayerType) do
    begin
      ChkBox_PlayerTypes[I,MPT] := TKMCheckBox.Create(Panel_PlayerTypes.ItemsPanel, 43 + Ord(MPT)*42, Top - 2, 20, 20, '', fntMetal);
      ChkBox_PlayerTypes[I,MPT].Tag     := I;
      ChkBox_PlayerTypes[I,MPT].OnClick := Mission_PlayerTypesChange;
    end;
    Inc(Top, LINE_H);
  end;

  Label_PlayerTypesAll := TKMLabel.Create(Panel_PlayerTypes.ItemsPanel,  13, Top, 75, 20, gResTexts[TX_MAPED_PLAYER_TYPE_ALLOW_ALL],
                                          fntOutline, taLeft);

  for MPT := Low(ChkBox_PlayerTypesAll) to High(ChkBox_PlayerTypesAll) do
  begin
    ChkBox_PlayerTypesAll[MPT] := TKMCheckBox.Create(Panel_PlayerTypes.ItemsPanel, 43 + Ord(MPT)*42, Top - 2, 20, 20, '', fntMetal, True);
    ChkBox_PlayerTypesAll[MPT].Tag     := Ord(MPT);
    ChkBox_PlayerTypesAll[MPT].Hint    := Format(gResTexts[TX_MAPED_PLAYER_TYPE_ALLOW_ALL_HINT],
                                               [gResTexts[PLAYER_TYPE_TX[MPT]]]);
    ChkBox_PlayerTypesAll[MPT].OnClick := Mission_PlayerTypesAllClick;
  end;

  Button_PlayerDelete := TKMButton.Create(Panel_PlayerTypes.ItemsPanel, 15,
                                          Panel_PlayerTypes.ItemsPanel.Height - 80, PANEL_W - 30, 26,
                                          Format(gResTexts[TX_MAPED_PLAYER_DELETE], [1]), bsGame);
  Button_PlayerDelete.Anchors := [anLeft, anRight, anBottom];
  Button_PlayerDelete.OnClick := PlayerDelete_Click;

  Button_Close := TKMButton.Create(Panel_PlayerTypes.ItemsPanel, 15,
                                   Panel_PlayerTypes.ItemsPanel.Height - 40,
                                   PANEL_W - 30, 30, gResTexts[TX_WORD_CLOSE], bsGame);
  Button_Close.Anchors := [anLeft, anRight, anBottom];
  Button_Close.OnClick := ClosePlayerTypes_Click;

  PopUp_Confirm_PlayerDelete := TKMPopUpMenu.Create(aParent.MasterParent, 450);
  PopUp_Confirm_PlayerDelete.Height := 200;
  PopUp_Confirm_PlayerDelete.AnchorsCenter;
  PopUp_Confirm_PlayerDelete.Left := (aParent.MasterParent.Width div 2) - (PopUp_Confirm_PlayerDelete.Width div 2);
  PopUp_Confirm_PlayerDelete.Top := (aParent.MasterParent.Height div 2) - 90;

    TKMBevel.Create(PopUp_Confirm_PlayerDelete, -2000,  -2000, 5000, 5000);

    Image_Confirm_PlayerDelete := TKMImage.Create(PopUp_Confirm_PlayerDelete, 0, 0, PopUp_Confirm_PlayerDelete.Width, PopUp_Confirm_PlayerDelete.Height, 15, rxGuiMain);
    Image_Confirm_PlayerDelete.ImageStretch;

    Label_PlayerDeleteConfirmTitle := TKMLabel.Create(PopUp_Confirm_PlayerDelete, PopUp_Confirm_PlayerDelete.Width div 2, 40, Format(gResTexts[TX_MAPED_PLAYER_DELETE_TITLE], [0]), fntOutline, taCenter);
    Label_PlayerDeleteConfirmTitle.Anchors := [anLeft, anBottom];

    Label_PlayerDeleteConfirm := TKMLabel.Create(PopUp_Confirm_PlayerDelete, 20, 85, PopUp_Confirm_PlayerDelete.Width - 40, 0, gResTexts[TX_MAPED_PLAYER_DELETE_CONFIRM], fntMetal, taCenter);
    Label_PlayerDeleteConfirm.Anchors := [anLeft, anBottom];

    Button_PlayerDeleteConfirm := TKMButton.Create(PopUp_Confirm_PlayerDelete, 20, 155, 170, 30, gResTexts[TX_MENU_LOAD_DELETE_DELETE], bsMenu);
    Button_PlayerDeleteConfirm.Anchors := [anLeft, anBottom];
    Button_PlayerDeleteConfirm.OnClick := PlayerDelete_Click;

    Button_PlayerDeleteCancel  := TKMButton.Create(PopUp_Confirm_PlayerDelete, PopUp_Confirm_PlayerDelete.Width - 190, 155, 170, 30, gResTexts[TX_WORD_CANCEL], bsMenu);
    Button_PlayerDeleteCancel.Anchors := [anLeft, anBottom];
    Button_PlayerDeleteCancel.OnClick := PlayerDelete_Click;
end;


procedure TKMMapEdMissionPlayers.UpdatePlayerTypes;
var
  I: Integer;
  MPT: TKMMapEdPlayerType;
  enabledCnt, checkedCnt: array [mptHuman..mptAdvancedAI] of Integer;
  hasAssets, isAllEnabled: Boolean;
begin
  gGame.MapEditor.ValidatePlayerTypes;

  for MPT := Low(enabledCnt) to High(enabledCnt) do
  begin
    enabledCnt[MPT] := 0;
    checkedCnt[MPT] := 0;
  end;

  for I := 0 to gHands.Count - 1 do
  begin
    hasAssets := gHands[I].HasAssets;
    ChkBox_PlayerTypes[I, mptDefault].Enabled     := hasAssets;
    ChkBox_PlayerTypes[I, mptHuman].Enabled       := hasAssets and not (gGame.MapEditor.DefaultHuman = I);
    ChkBox_PlayerTypes[I, mptClassicAI].Enabled   := hasAssets;
    ChkBox_PlayerTypes[I, mptAdvancedAI].Enabled  := hasAssets;

    ChkBox_PlayerTypes[I, mptDefault].Checked     := hasAssets and (gGame.MapEditor.DefaultHuman = I);
    ChkBox_PlayerTypes[I, mptHuman].Checked       := hasAssets and gGame.MapEditor.PlayerHuman[I];
    ChkBox_PlayerTypes[I, mptClassicAI].Checked   := hasAssets and gGame.MapEditor.PlayerClassicAI[I];
    ChkBox_PlayerTypes[I, mptAdvancedAI].Checked  := hasAssets and gGame.MapEditor.PlayerAdvancedAI[I];

    for MPT := Low(ChkBox_PlayerTypesAll) to High(ChkBox_PlayerTypesAll) do
    begin
      enabledCnt[MPT] := enabledCnt[MPT] + Byte(ChkBox_PlayerTypes[I, MPT].Enabled);
      checkedCnt[MPT] := checkedCnt[MPT] + Byte(ChkBox_PlayerTypes[I, MPT].Checked
                                        and ChkBox_PlayerTypes[I, MPT].Enabled);
    end;
  end;

  isAllEnabled := False;
  for MPT := Low(ChkBox_PlayerTypesAll) to High(ChkBox_PlayerTypesAll) do
  begin
    ChkBox_PlayerTypesAll[MPT].Enabled := enabledCnt[MPT] > 0;
    isAllEnabled := isAllEnabled or ChkBox_PlayerTypesAll[MPT].Enabled;

    //Uncheck if all are unchecked and disabled
    if not ChkBox_PlayerTypesAll[MPT].Enabled
      and (checkedCnt[MPT] = 0) then
      ChkBox_PlayerTypesAll[MPT].Uncheck;

    if enabledCnt[MPT] > 0 then
    begin

      if checkedCnt[MPT] = 0 then
        ChkBox_PlayerTypesAll[MPT].Uncheck //Uncheck if all is unchecked
      else
      if checkedCnt[MPT] >= enabledCnt[MPT] then
        ChkBox_PlayerTypesAll[MPT].Check //Check if all checked
      else
        ChkBox_PlayerTypesAll[MPT].SemiCheck; //SemiCheck in other cases
    end;
    if ChkBox_PlayerTypesAll[MPT].Checked then
      ChkBox_PlayerTypesAll[MPT].Hint := Format(gResTexts[TX_MAPED_PLAYER_TYPE_DISALLOW_ALL_HINT],
                                              [gResTexts[PLAYER_TYPE_TX[MPT]]])
    else
      ChkBox_PlayerTypesAll[MPT].Hint := Format(gResTexts[TX_MAPED_PLAYER_TYPE_ALLOW_ALL_HINT],
                                              [gResTexts[PLAYER_TYPE_TX[MPT]]]);
  end;
  Label_PlayerTypesAll.Enabled := isAllEnabled;
end;


procedure TKMMapEdMissionPlayers.PlayerDeleteConfirm(aVisible: Boolean);
begin
  if aVisible then
  begin
    Label_PlayerDeleteConfirmTitle.Caption := Format(gResTexts[TX_MAPED_PLAYER_DELETE_TITLE], [fPlayerIdToDelete + 1]);
    PopUp_Confirm_PlayerDelete.Show;
  end else
    PopUp_Confirm_PlayerDelete.Hide;
end;


procedure TKMMapEdMissionPlayers.PlayerDelete_Click(Sender: TObject);
begin
  if Sender = Button_PlayerDelete then
    PlayerDeleteConfirm(True);

  if Sender = Button_PlayerDeleteCancel then
    PlayerDeleteConfirm(False);

  if Sender = Button_PlayerDeleteConfirm then
  begin
    gGame.MapEditor.DeletePlayer(fPlayerIdToDelete);
    PlayerDeleteConfirm(False);

    Mission_PlayerIdUpdate;
    UpdatePlayerTypes;
  end;
end;


procedure TKMMapEdMissionPlayers.ClosePlayerTypes_Click(Sender: TObject);
begin
  Hide;
end;


procedure TKMMapEdMissionPlayers.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if aHandled then Exit;

  if Visible then
  begin
    if PopUp_Confirm_PlayerDelete.Visible then
    begin
      if Key = VK_ESCAPE then //If confirmation dialog is opened only Esc button could be handled
      begin
        aHandled := True;
        PopUp_Confirm_PlayerDelete.Hide; //Hide 'delete player' confirmation dialog
      end;
    end
    else
      case Key of
        VK_DELETE:  begin
                      aHandled := True;
                      if Button_PlayerDelete.Enabled then
                        PlayerDelete_Click(Button_PlayerDelete);
                    end;
        VK_UP:      begin
                      aHandled := True;
                      if fPlayerIdToDelete > 0 then
                        UpdatePlayer(fPlayerIdToDelete - 1);
                    end;
        VK_DOWN:    begin
                      aHandled := True;
                      if fPlayerIdToDelete < MAX_HANDS - 1 then
                        UpdatePlayer(fPlayerIdToDelete + 1);
                    end;
        VK_ESCAPE:  begin
                      aHandled := True;
                      Hide;
                    end;
      end;
  end;
end;


procedure TKMMapEdMissionPlayers.Mission_PlayerTypesAllClick(Sender: TObject);
var
  I: Integer;
  MPT: TKMMapEdPlayerType;
  checked: Boolean;
begin
  MPT := TKMMapEdPlayerType(TKMCheckBox(Sender).Tag);

  for I := 0 to MAX_HANDS - 1 do
  begin
    if not ChkBox_PlayerTypes[I, MPT].IsClickable then
      Continue;

    checked := TKMCheckBox(Sender).Checked;

    ChkBox_PlayerTypes[I, MPT].SetChecked(checked);
    case MPT of
      mptHuman:       gGame.MapEditor.PlayerHuman[I] := checked;
      mptClassicAI:   gGame.MapEditor.PlayerClassicAI[I] := checked;
      mptAdvancedAI:  gGame.MapEditor.PlayerAdvancedAI[I] := checked;
    end;
  end;

  UpdatePlayerTypes;
end;


procedure TKMMapEdMissionPlayers.Mission_PlayerTypesChange(Sender: TObject);
var
  playerId: Integer;
begin
  playerId := TKMCheckBox(Sender).Tag;

  UpdatePlayer(playerId);

  //There should be exactly one default human player
  if Sender = ChkBox_PlayerTypes[playerId, mptDefault] then
  begin
    gGame.MapEditor.DefaultHuman := playerId;
    gGame.MapEditor.PlayerHuman[playerId] := True;
  end;

  if Sender = ChkBox_PlayerTypes[playerId, mptHuman] then
  begin
    gGame.MapEditor.PlayerHuman[playerId] := ChkBox_PlayerTypes[playerId, mptHuman].Checked;
    //User cannot set player type undetermined
    if not ChkBox_PlayerTypes[playerId, mptHuman].Checked
        and not ChkBox_PlayerTypes[playerId, mptClassicAI].Checked
        and not ChkBox_PlayerTypes[playerId, mptAdvancedAI].Checked then
        gGame.MapEditor.PlayerClassicAI[playerId] := True;
  end;

  if (Sender = ChkBox_PlayerTypes[playerId, mptClassicAI])
    or (Sender = ChkBox_PlayerTypes[playerId, mptAdvancedAI]) then
  begin
    gGame.MapEditor.PlayerClassicAI[playerId] := ChkBox_PlayerTypes[playerId, mptClassicAI].Checked;
    gGame.MapEditor.PlayerAdvancedAI[playerId] := ChkBox_PlayerTypes[playerId, mptAdvancedAI].Checked;
    if not ChkBox_PlayerTypes[playerId, mptHuman].Checked then
    begin
      //User cannot set player type undetermined
      if not ChkBox_PlayerTypes[playerId, mptClassicAI].Checked
        and not ChkBox_PlayerTypes[playerId, mptAdvancedAI].Checked then
        gGame.MapEditor.PlayerHuman[playerId] := True;
      //Can't be 2 default AI types (without human)
//      if CheckBox_PlayerTypes[PlayerId, mptClassicAI].Checked
//        and CheckBox_PlayerTypes[PlayerId, mptAdvancedAI].Checked then
//      begin
//        if (Sender = CheckBox_PlayerTypes[PlayerId, mptClassicAI]) then
//          gGame.MapEditor.PlayerAdvancedAI[PlayerId] := False
//        else
//          gGame.MapEditor.PlayerClassicAI[PlayerId] := False;
//      end;
    end;
  end;

  UpdatePlayerTypes;
end;

procedure TKMMapEdMissionPlayers.Mission_PlayerIdUpdate;
var
  I: Integer;
begin
  UpdatePlayer;

  for I := 0 to MAX_HANDS - 1 do
    if I < gHands.Count then
      Label_PlayerId[I].Enabled := gHands[I].HasAssets;
end;


procedure TKMMapEdMissionPlayers.UpdatePlayer(aIndex: TKMHandID = -1);
begin
  if aIndex = -1 then
    aIndex := gMySpectator.HandID;

  Button_PlayerDelete.Enabled := gHands[aIndex].HasAssets;
  Button_PlayerDelete.Caption := Format(gResTexts[TX_MAPED_PLAYER_DELETE], [aIndex + 1]);
  fPlayerIdToDelete := aIndex;
end;


procedure TKMMapEdMissionPlayers.Hide;
begin
  Panel_PlayerTypes.Hide;
end;


procedure TKMMapEdMissionPlayers.Show;
begin
  UpdatePlayerTypes;
  Mission_PlayerIdUpdate;
  Panel_PlayerTypes.Show;
end;


function TKMMapEdMissionPlayers.Visible: Boolean;
begin
  Result := Panel_PlayerTypes.Visible;
end;


end.
