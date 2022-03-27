unit KM_GUIGameSpectator;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, StrUtils, SysUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsDrop, KM_ControlsProgressBar,
  KM_HandsCollection, KM_Defaults, KM_Hand,
  KM_ResWares, KM_ResHouses, KM_Pics, KM_CommonTypes, KM_Points, KM_Houses;


type
  // Type of things shown in spectator statistics
  TKMHandStatType = (
    slNone, slResources, slWarfare, slHouses, slConstructions, slSLR,
    slWorkers, slArmy, slArmyTotal, slArmyKilling, slArmyLost
  );

  TKMGUIGameSpectatorItem = class(TKMPanel)
  private
    fHandID: Integer;
    fImageID: Word;
    fValue: String;
    fAdditionalValue: String;
    fProgress: Single;
    fItemTag: Integer;
    FOnItemClick: TIntBoolEvent;
    FDoHighlight: TBoolIntFuncSimple;
    procedure ItemClicked(Sender: TObject; Shift: TShiftState);
  protected
    Bevel: TKMBevel;
    Image: TKMImage;
    PercentBar: TKMPercentBar;
    Label_Text: TKMLabel;
    Label_AddText: TKMLabel;
  public
    constructor Create(aParent: TKMPanel; ATag: Integer; AImageID: Word; const AHint: String; AHandID: Integer;
                       aProgressColor: Cardinal; aDoHighlight: TBoolIntFuncSimple; aOnItemClick: TIntBoolEvent);
    property ItemTag: Integer read FItemTag;
    property Value: String read FValue write FValue;
    property AdditionalValue: String read FAdditionalValue write FAdditionalValue;
    property Progress: Single read FProgress write FProgress;
    procedure CreateChilds;
    procedure PaintPanel(aPaintLayer: TKMPaintLayer); override;
  end;

  TKMGameSpectatorItemLinesAggregator = class
  private
    fItemsVisibility: array of Boolean;
    fCount: Integer;
    procedure ResetItems;
    procedure SetCount(aCount: Integer);
  end;

  TKMGUIGameSpectatorItemLine = class;
  TKMGUIGameSpectatorItemLineClass = class of TKMGUIGameSpectatorItemLine;

  TKMGUIGameSpectatorItemLine = class(TKMPanel)
  private
    fLinesAggregator: TKMGameSpectatorItemLinesAggregator;
    FOnJumpToPlayer: TIntegerEvent;
    FSetViewportPos: TPointFEvent;
    FHandIndex: Integer;
    FItems: array of TKMGUIGameSpectatorItem;
    procedure LineClicked(Sender: TObject);
    procedure LineItemClicked(aItemTag: Integer; aMainFunction: Boolean);
    procedure Update;
    procedure UpdateItemsVisibility;
    procedure CreateChilds;
  protected
    Bevel: TKMBevel;
    Image_Flag: TKMImage;
    Label_Text: TKMLabel;

    function CreateItem(aHandIndex: Integer; ATag: Integer; aOnItemClick: TIntBoolEvent): TKMGUIGameSpectatorItem; virtual; abstract;
    function GetTagCount: Integer; virtual; abstract;
    function GetTag(AIndex: Integer): Integer; virtual; abstract;
    function GetValue(aHandIndex: Integer; ATag: Integer): String; virtual; abstract;
    function GetAdditionalValue(aHandIndex: Integer; ATag: Integer): String; virtual;
    function GetProgress(aHandIndex: Integer; ATag: Integer): Single; virtual;
    function GetNextLoc(aHandIndex: Integer; ATag: Integer; aMainFunction: Boolean): TKMPointF; virtual;
    property SetViewportPos: TPointFEvent read FSetViewportPos;
    function DontHighlight(aIndex: Integer): Boolean;
    function DoHighlight(aIndex: Integer): Boolean;
  public
    constructor Create(aParent: TKMPanel; aHandIndex: Integer;
                       aOnJumpToPlayer: TIntegerEvent; aSetViewportPos: TPointFEvent;
                       aLinesAggregator: TKMGameSpectatorItemLinesAggregator = nil); virtual;
    procedure PaintPanel(aPaintLayer: TKMPaintLayer); override;
    property HandIndex: Integer read FHandIndex;
  end;


  TKMGUIGameSpectator = class(TKMPanel)
  private
    FDropBox: TKMDropList;
    FLastIndex: TKMHandStatType;

    FOnJumpToPlayer: TIntegerEvent;
    FSetViewportPos: TPointFEvent;

    FLinesAggregator: array [TKMHandStatType] of TKMGameSpectatorItemLinesAggregator;
    FLines: array [TKMHandStatType] of array [0..MAX_HANDS - 1] of TKMGUIGameSpectatorItemLine;

    procedure AddLineType(aParent: TKMPanel; aLineType: TKMHandStatType; aLineClass: TKMGUIGameSpectatorItemLineClass);
    procedure ChangePage(Sender: TObject);
  protected
    procedure DoPaint(aPaintLayer: TKMPaintLayer); override;
  public
    constructor Create(aParent: TKMPanel; aOnJumpToPlayer: TIntegerEvent; aSetViewportPos: TPointFEvent);
    destructor Destroy; override;

    function GetOpenedPage: Integer;
    procedure OpenPage(aIndex: Integer);
    procedure CloseDropBox;

    property DropBox: TKMDropList read FDropBox;

    procedure UpdateState(aTick: Cardinal); override;
  end;

implementation
uses
  KM_InterfaceGame, KM_GameParams, KM_RenderUI, KM_ResFonts, KM_Resource, KM_ResTexts, KM_ResUnits,
  KM_ControlsTypes, KM_GUIGameSpectatorItemLines,
  KM_UnitGroup,
  KM_CommonUtils,
  KM_ResTypes;

const
  GUI_SPEC_ITEM_WIDTH = 28;
  GUI_SPEC_ITEM_HEIGHT = 36;
  GUI_SPEC_ITEM_SRLITE_H = 4;
  GUI_SPEC_ITEM_SPRITE_V = 4;
  GUI_SPEC_ITEM_TEAM = 14;

  GUI_SPEC_HEADER_HEIGHT = 14;

  GUI_SPEC_HEADER_FLAG = 1164;
  GUI_SPEC_HEADER_FLAG_FRAME = 5;

  BEVEL_RENDER_LAYER = pl1;
  PERCENTBAR_RENDER_LAYER = pl1;
  IMAGE_RENDER_LAYER = pl2;
  TEXT_RENDER_LAYER = pl3;
  DROPBOX_RENDER_LAYER = pl3;

{ TKMGUIGameSpectatorItem }
constructor TKMGUIGameSpectatorItem.Create(aParent: TKMPanel; ATag: Integer; AImageID: Word; const AHint: String;
                                           AHandID: Integer; aProgressColor: Cardinal; aDoHighlight: TBoolIntFuncSimple; aOnItemClick: TIntBoolEvent);
begin
  inherited Create(aParent, 0, 0, GUI_SPEC_ITEM_WIDTH, GUI_SPEC_ITEM_HEIGHT);

  FItemTag := ATag;
  Hint := AHint;
  fHandID := AHandID;
  FImageID := AImageID;
  FValue := '';
  FAdditionalValue := '';
  FProgress := -1;
  FDoHighlight := aDoHighlight;
  FOnItemClick := aOnItemClick;
  CreateChilds;
  PercentBar.MainColor := aProgressColor;
end;


procedure TKMGUIGameSpectatorItem.ItemClicked(Sender: TObject; Shift: TShiftState);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(FItemTag, ssLeft in Shift);
end;


procedure TKMGUIGameSpectatorItem.CreateChilds;
begin
  Bevel := TKMBevel.Create(Self, 0, 0, Width, Height, BEVEL_RENDER_LAYER);
  Bevel.AnchorsStretch;
  Bevel.OnClickShift := ItemClicked;
  Image := TKMImage.Create(Self, 2, 0, Width - 4, Height - 4, FImageID, rxGui, IMAGE_RENDER_LAYER);
  if fHandID < gHands.Count then
    Image.FlagColor := gHands[fHandID].FlagColor;
  Image.ImageCenter;
  Image.Anchors := [anRight, anTop];
  Image.OnClickShift := ItemClicked;
  PercentBar := TKMPercentBar.Create(Self, 0, Height - 6, Width, 6, fntMini, PERCENTBAR_RENDER_LAYER);
  PercentBar.AnchorsStretch;
  Label_Text := TKMLabel.Create(Self, Width div 2, Height - 16, FValue, fntGrey, taCenter, TEXT_RENDER_LAYER);
  Label_Text.Anchors := [anRight, anTop];
  Label_AddText := TKMLabel.Create(Self, Width - 2, -2, FValue, fntGrey, taRight, TEXT_RENDER_LAYER);
  Label_AddText.Anchors := [anRight, anTop];
end;


procedure TKMGUIGameSpectatorItem.PaintPanel(aPaintLayer: TKMPaintLayer);
var
  paintLightness: Single;
begin
  paintLightness := CTRL_HIGHLIGHT_COEF_DEF * Byte(((csOver in Image.State) or (csOver in Bevel.State)) and FDoHighlight(FItemTag));

  Image.Lightness := paintLightness;

  PercentBar.Visible := (FProgress >= 0);
  PercentBar.Position := fProgress;

  Label_Text.Caption := fValue;
  Label_AddText.Caption := fAdditionalValue;

  inherited PaintPanel(aPaintLayer);
end;


{ TKMGUIGameSpectatorItemLine }
constructor TKMGUIGameSpectatorItemLine.Create(aParent: TKMPanel; aHandIndex: Integer;
                                               aOnJumpToPlayer: TIntegerEvent; aSetViewportPos: TPointFEvent;
                                               aLinesAggregator: TKMGameSpectatorItemLinesAggregator = nil);
var
  I: Integer;
begin
  inherited Create(aParent, aParent.Width, 32 + aHandIndex * (GUI_SPEC_ITEM_HEIGHT + GUI_SPEC_ITEM_SPRITE_V), 0, GUI_SPEC_ITEM_HEIGHT + GUI_SPEC_HEADER_HEIGHT + GUI_SPEC_ITEM_SPRITE_V);
  fOnJumpToPlayer := aOnJumpToPlayer;
  fSetViewportPos := aSetViewportPos;
  fLinesAggregator := aLinesAggregator;
  Anchors := [anTop, anRight];
  Focusable := False;
  FHandIndex := aHandIndex;
  SetLength(fItems, GetTagCount);
  CreateChilds;
  for I := 0 to GetTagCount - 1 do
    fItems[I] := CreateItem(aHandIndex, GetTag(I), LineItemClicked);
end;


procedure TKMGUIGameSpectatorItemLine.LineClicked(Sender: TObject);
begin
  if Assigned(fOnJumpToPlayer) then
    fOnJumpToPlayer(FHandIndex);
end;


procedure TKMGUIGameSpectatorItemLine.LineItemClicked(aItemTag: Integer; aMainFunction: Boolean);
var
  loc: TKMPointF;
begin
  if Assigned(FSetViewportPos) then
  begin
    loc := GetNextLoc(FHandIndex, aItemTag, aMainFunction);
    if loc <> KMPOINTF_INVALID_TILE then
      FSetViewportPos(loc);
  end;
end;


function TKMGUIGameSpectatorItemLine.DontHighlight(aIndex: Integer): Boolean;
begin
  Result := False;
end;


function TKMGUIGameSpectatorItemLine.DoHighlight(aIndex: Integer): Boolean;
begin
  Result := True;
end;


procedure TKMGUIGameSpectatorItemLine.UpdateItemsVisibility;
var
  I, position, count: Integer;
  str: UnicodeString;
begin
  if not Visible then
    Exit;

  count := 0;
  for I := 0 to GetTagCount - 1 do
    if fLinesAggregator.FItemsVisibility[I] then
      Inc(count);

  str := IfThen(gHands[FHandIndex].OwnerNiknameU <> '', gHands[FHandIndex].OwnerNiknameU, gHands[FHandIndex].OwnerName);
  Width := Max(count * (GUI_SPEC_ITEM_WIDTH + GUI_SPEC_ITEM_SRLITE_H) + GUI_SPEC_ITEM_SRLITE_H, gRes.Fonts[fntGrey].GetTextSize(str).X + 32 + 4);
  Left := Parent.Width - Width;

  position := Width - GUI_SPEC_ITEM_SRLITE_H - GUI_SPEC_ITEM_WIDTH;
  for I := 0 to GetTagCount - 1 do
    if fLinesAggregator.FItemsVisibility[I] then
    begin
      fItems[I].Top := GUI_SPEC_HEADER_HEIGHT;
      fItems[I].Left := position;
      Dec(position, GUI_SPEC_ITEM_WIDTH + GUI_SPEC_ITEM_SRLITE_H);
    end;
end;


procedure TKMGUIGameSpectatorItemLine.Update;
var
  I: Integer;
begin
  if not Visible then
    Exit;

  for I := 0 to GetTagCount - 1 do
  begin
    fItems[I].Value := GetValue(FHandIndex, GetTag(I));
    fItems[I].AdditionalValue := GetAdditionalValue(FHandIndex, GetTag(I));
    fItems[I].Progress := GetProgress(FHandIndex, GetTag(I));
    fItems[I].Visible := (fItems[I].Value <> '')
                          or (fItems[I].AdditionalValue <> '')
                          or (fItems[I].Progress >= 0);
    if fItems[I].Visible then
      fLinesAggregator.FItemsVisibility[I] := True;
  end;
end;


function TKMGUIGameSpectatorItemLine.GetAdditionalValue(aHandIndex: Integer; ATag: Integer): String;
begin
  Result := '';
end;


function TKMGUIGameSpectatorItemLine.GetNextLoc(aHandIndex: Integer; ATag: Integer; aMainFunction: Boolean): TKMPointF;
begin
  Result := KMPOINTF_INVALID_TILE;
end;


function TKMGUIGameSpectatorItemLine.GetProgress(aHandIndex: Integer; ATag: Integer): Single;
begin
  Result := -1;
end;


procedure TKMGUIGameSpectatorItemLine.CreateChilds;
begin
  Bevel := TKMBevel.Create(Self, 0, 0, Width, Height, BEVEL_RENDER_LAYER);
  Bevel.AnchorsStretch;
  Bevel.OnClick := LineClicked;
  Bevel.BackAlpha := 0.2;
  Bevel.EdgeAlpha := 0.5;
  Image_Flag := TKMImage.Create(Self, Width - 32, 0, 32, GUI_SPEC_HEADER_HEIGHT, 0, rxHouses, IMAGE_RENDER_LAYER);
  if FHandIndex < gHands.Count then
    Image_Flag.FlagColor := gHands[FHandIndex].FlagColor;
  Image_Flag.ImageCenter;
  Image_Flag.Anchors := [anTop, anRight];
  Image_Flag.OnClick := LineClicked;
  Label_Text := TKMLabel.Create(Self, Width - 32, 0, '', fntGrey, taRight, TEXT_RENDER_LAYER);
  Label_Text.Anchors := [anRight];
end;


procedure TKMGUIGameSpectatorItemLine.PaintPanel(aPaintLayer: TKMPaintLayer);
begin
  Image_Flag.TexId := GUI_SPEC_HEADER_FLAG + gGameParams.Tick mod GUI_SPEC_HEADER_FLAG_FRAME;
  Label_Text.Caption := gHands[FHandIndex].OwnerName(not gGameParams.IsSingleplayer);

  inherited;
end;


{ TKMGUIGameSpectator }
constructor TKMGUIGameSpectator.Create(aParent: TKMPanel; aOnJumpToPlayer: TIntegerEvent; aSetViewportPos: TPointFEvent);
const
  DROPBOX_W = 270;
begin
  inherited Create(aParent, 0, 0, aParent.Width, aParent.Height);
  AnchorsStretch;
  Hitable := False;

  fOnJumpToPlayer := aOnJumpToPlayer;
  fSetViewportPos := aSetViewportPos;

  AddLineType(Self, slNone, nil);
  AddLineType(Self, slResources, TKMGUIGameSpectatorItemLineResources);
  AddLineType(Self, slWarfare, TKMGUIGameSpectatorItemLineWarFare);
  AddLineType(Self, slHouses, TKMGUIGameSpectatorItemLineHouses);
  AddLineType(Self, slConstructions, TKMGUIGameSpectatorItemLineConstructing);
  AddLineType(Self, slSLR, TKMGUIGameSpectatorItemLinePopulationSLR);
  AddLineType(Self, slWorkers, TKMGUIGameSpectatorItemLinePopulationHouseWorkers);
  AddLineType(Self, slArmy, TKMGUIGameSpectatorItemLineArmyInstantenious);
  AddLineType(Self, slArmyTotal, TKMGUIGameSpectatorItemLineArmyTotal);
  AddLineType(Self, slArmyKilling, TKMGUIGameSpectatorItemLineArmyKilling);
  AddLineType(Self, slArmyLost, TKMGUIGameSpectatorItemLineArmyLost);

  //Create DropBox after pages, to show it above them
  FDropBox := TKMDropList.Create(Self, Width - DROPBOX_W - 5, 5, DROPBOX_W, 20, fntMetal, '', bsGame, True, 0.85, DROPBOX_RENDER_LAYER);
  FDropBox.Anchors := [anTop, anRight];
  FDropBox.OnChange := ChangePage;
  FDropBox.DropCount := Ord(High(TKMHandStatType)) + 1;

  FDropBox.Add(gResTexts[TX_WORD_NONE]);
  FDropBox.Add(gResTexts[TX_WORD_RESOURCES]);
  FDropBox.Add(gResTexts[TX_RESOURCES_WARFARE]);
  FDropBox.Add(gResTexts[TX_WORD_HOUSES]);
  FDropBox.Add(gResTexts[TX_WORD_CONSTRUCTING]);
  FDropBox.Add(gResTexts[TX_SPECTATOR_PANEL_CITIZENS_SLR]);
  FDropBox.Add(gResTexts[TX_SPECTATOR_PANEL_CITIZENS_HOUSE_WORKERS]);
  FDropBox.Add(gResTexts[TX_WORD_ARMY] + ' - ' + gResTexts[TX_RESULTS_ARMY_INSTANTANEOUS]);
  FDropBox.Add(gResTexts[TX_WORD_ARMY] + ' - ' + gResTexts[TX_RESULTS_ARMY_TOTAL_EQUIPPED]);
  FDropBox.Add(gResTexts[TX_WORD_ARMY] + ' - ' + gResTexts[TX_RESULTS_ARMY_DEFEATED]);
  FDropBox.Add(gResTexts[TX_WORD_ARMY] + ' - ' + gResTexts[TX_RESULTS_ARMY_LOST]);

  FDropBox.ItemIndex := 0;

  Assert(FDropBox.Count = FDropBox.DropCount);
end;


destructor TKMGUIGameSpectator.Destroy;
var
  I: TKMHandStatType;
begin
  for I := Low(TKMHandStatType) to High(TKMHandStatType) do
    FreeAndNil(FLinesAggregator[I]);
end;


procedure TKMGUIGameSpectator.DoPaint(aPaintLayer: TKMPaintLayer);
var
  I: Integer;
begin
//todo: This is a copy. See how can we improve on that
  for I := 0 to ChildCount - 1 do
    if Childs[I].IsSetVisible then
    begin
      if Childs[I] is TKMPanel then
        TKMPanel(Childs[I]).PaintPanel(aPaintLayer)
      else
      if (Childs[I].PaintLayer = aPaintLayer) then
        Childs[I].Paint;
    end;
end;


procedure TKMGUIGameSpectator.AddLineType(aParent: TKMPanel; aLineType: TKMHandStatType; aLineClass: TKMGUIGameSpectatorItemLineClass);
var
  I: Integer;
begin
  if aLineClass = nil then Exit;

  FLinesAggregator[aLineType] := TKMGameSpectatorItemLinesAggregator.Create;
  for I := 0 to gHands.Count - 1 do
  begin
    FLines[aLineType, I] := aLineClass.Create(aParent, I, fOnJumpToPlayer, fSetViewportPos, FLinesAggregator[aLineType]);
    FLines[aLineType, I].Visible := False;
    FLinesAggregator[aLineType].SetCount(FLines[aLineType, I].GetTagCount);
  end;
end;


procedure TKMGUIGameSpectator.ChangePage(Sender: TObject);
var
  I, J: Integer;
  teams: TKMByteSetArray;
  position, teamAddPos: Integer;
begin
  // Hide all lines
  for I := 0 to gHands.Count - 1 do
    if Assigned(FLines[FLastIndex, I]) then
      FLines[FLastIndex, I].Visible := False;

  FLastIndex := TKMHandStatType(FDropBox.ItemIndex);

  position := 32;
  teams := gHands.Teams;

  teamAddPos := GUI_SPEC_ITEM_TEAM;
  if Length(teams) = gHands.Count then //FFA game
    teamAddPos := GUI_SPEC_ITEM_SPRITE_V;

  for I := Low(teams) to High(teams) do
  begin
    for J in teams[I] do
    begin
      if Assigned(FLines[FLastIndex, J]) then
      begin
        FLines[FLastIndex, J].Top := position;
        FLines[FLastIndex, J].Show;
      end;
      position := position + GUI_SPEC_ITEM_HEIGHT + GUI_SPEC_ITEM_SPRITE_V * 2 + GUI_SPEC_HEADER_HEIGHT;
    end;
    position := position + teamAddPos;
  end;
  UpdateState(0); //Will update all data
end;


procedure TKMGUIGameSpectator.UpdateState(aTick: Cardinal);
var
  I: TKMHandStatType;
  K: Integer;
begin
  inherited;

  //Updates could be done every 5 ticks
  if aTick mod 5 <> 0 then Exit;

  //Reset all aggregators first
  for I := Low(TKMHandStatType) to High(TKMHandStatType) do
    if FLinesAggregator[I] <> nil then
      FLinesAggregator[I].ResetItems;

  //Collect data from lines items - which to show and which not - into aggregator
  for I := Low(TKMHandStatType) to High(TKMHandStatType) do
    for K := 0 to Length(FLines[I]) - 1 do
      if FLines[I, K] <> nil then
        FLines[I, K].Update;

  //Set visibility for items, by aggregated data
  for I := Low(TKMHandStatType) to High(TKMHandStatType) do
    for K := 0 to Length(FLines[I]) - 1 do
      if FLines[I, K] <> nil then
        FLines[I, K].UpdateItemsVisibility;
end;


function TKMGUIGameSpectator.GetOpenedPage: Integer;
begin
  Result := FDropBox.ItemIndex;
end;


procedure TKMGUIGameSpectator.OpenPage(aIndex: Integer);
begin
  FDropBox.ItemIndex := aIndex;
  ChangePage(nil);
end;


procedure TKMGUIGameSpectator.CloseDropBox;
begin
  FDropBox.ItemIndex := 0;
  FDropBox.CloseList;
  ChangePage(nil);
end;


{ TKMGameSpectatorItemLinesAggregator }
procedure TKMGameSpectatorItemLinesAggregator.SetCount(aCount: Integer);
begin
  fCount := aCount;
  SetLength(fItemsVisibility, aCount);
end;


procedure TKMGameSpectatorItemLinesAggregator.ResetItems;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    fItemsVisibility[I] := False;
end;


end.
