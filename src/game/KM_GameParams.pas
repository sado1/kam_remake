unit KM_GameParams;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_GameTypes;

type
  TKMGameModeSetEvent = procedure (aGameMode: TKMGameMode) of object;

  TKMGameParams = class
  private
    fGameMode: TKMGameMode;
    fMissionMode: TKMissionMode;
    fGameTick: Cardinal;
    fVisibleLayers: TKMMapVisibleLayerSet;
    procedure SetGameMode(aGameMode: TKMGameMode);
  public
    constructor Create(out aSetGameModeEvent: TKMGameModeSetEvent);
    destructor Destroy; override;

    property GameMode: TKMGameMode read fGameMode;
    property MissionMode: TKMissionMode read fMissionMode write fMissionMode;
    property GameTick: Cardinal read fGameTick;
    property VisibleLayers: TKMMapVisibleLayerSet read fVisibleLayers write fVisibleLayers;

    function IsMapEditor: Boolean;
    function IsCampaign: Boolean;
    function IsMultiPlayerOrSpec: Boolean;
    function IsMultiplayerGame: Boolean;
    function IsMultiplayer: Boolean;
    function IsReplay: Boolean;
    function IsReplayOrSpectate: Boolean;
    function IsSingleplayerGame: Boolean;
    function IsSingleplayer: Boolean;
    function IsNormalGame: Boolean;

    function IsTactic: Boolean;
    function IsNormalMission: Boolean;

    procedure UpdateState(aGameTick: Cardinal);

    {$IFDEF RUNNER}
    procedure GetGameModeSetEvent(out aSetGameModeEvent: TKMGameModeSetEvent);
    {$ENDIF}
  end;

var
  gGameParams: TKMGameParams;


implementation


{ TKMGameParams }
constructor TKMGameParams.Create(out aSetGameModeEvent: TKMGameModeSetEvent);
begin
  inherited Create;

  fVisibleLayers := [mlObjects, mlHouses, mlUnits, mlOverlays];

  fGameTick := 0;

  aSetGameModeEvent := SetGameMode;

  gGameParams := Self;
end;


destructor TKMGameParams.Destroy;
begin
  gGameParams := nil; //should be destoryed by creator (KM_Game)

  inherited;
end;


{$IFDEF RUNNER}
procedure TKMGameParams.GetGameModeSetEvent(out aSetGameModeEvent: TKMGameModeSetEvent);
begin
  aSetGameModeEvent := SetGameMode;
end;
{$ENDIF}


procedure TKMGameParams.SetGameMode(aGameMode: TKMGameMode);
begin
  fGameMode := aGameMode;
end;


function TKMGameParams.IsMapEditor: Boolean;
begin
  Result := fGameMode = gmMapEd;
end;


function TKMGameParams.IsCampaign: Boolean;
begin
  Result := fGameMode = gmCampaign;
end;


function TKMGameParams.IsTactic: Boolean;
begin
  Result := fMissionMode = mmTactic;
end;


function TKMGameParams.IsNormalMission: Boolean;
begin
  Result := fMissionMode = mmNormal;
end;


function TKMGameParams.IsMultiplayerGame: Boolean;
begin
  Result := fGameMode = gmMulti;
end;


// We often need to see if game is MP
function TKMGameParams.IsMultiPlayerOrSpec: Boolean;
begin
  Result := fGameMode in [gmMulti, gmMultiSpectate];
end;


function TKMGameParams.IsMultiplayer: Boolean;
begin
  Result := fGameMode in [gmMulti, gmMultiSpectate, gmReplayMulti];
end;


function TKMGameParams.IsSingleplayerGame: Boolean;
begin
  Result := fGameMode in [gmSingle, gmCampaign];
end;


function TKMGameParams.IsSingleplayer: Boolean;
begin
  Result := fGameMode in [gmSingle, gmCampaign, gmReplaySingle];
end;


function TKMGameParams.IsNormalGame: Boolean;
begin
  Result := fGameMode in [gmSingle, gmCampaign, gmMulti];
end;


function TKMGameParams.IsReplay: Boolean;
begin
  Result := fGameMode in [gmReplaySingle, gmReplayMulti];
end;


function TKMGameParams.IsReplayOrSpectate: Boolean;
begin
  Result := fGameMode in [gmMultiSpectate, gmReplaySingle, gmReplayMulti];
end;


procedure TKMGameParams.UpdateState(aGameTick: Cardinal);
begin
  fGameTick := aGameTick;
end;



end.
