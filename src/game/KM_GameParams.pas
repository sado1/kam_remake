unit KM_GameParams;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_CommonTypes, KM_GameTypes, KM_MapTypes;

type
  TKMGameModeSetEvent = procedure (aGameMode: TKMGameMode) of object;

  // This class represents some of the game parameters, kind of KM_Game light interface
  // Usefull to split KM_Game and to reduce units dependencies

  TKMGameParams = class
  private
    fMode: TKMGameMode;
    fMissionMode: TKMissionMode;
    fTick: Cardinal;
    fVisibleLayers: TKMMapVisibleLayerSet;

    fName: UnicodeString;
    fMapSimpleCRC: Cardinal; //CRC of map (based on Map and Dat) used in MapEd
    fGameMapFullCRC: Cardinal; //CRC of map for reporting stats to master server. Also used in MapEd
    fMissionFileSP: UnicodeString; //Relative pathname to mission we are playing, so it gets saved to crashreport. SP only, see GetMissionFile.

    fMissionDifficulty: TKMMissionDifficulty;

    fDynamicFOW: Boolean;

    procedure SetGameTick(aGameTick: Cardinal);
    procedure SetGameMode(aGameMode: TKMGameMode);
    function GetMissionFile: UnicodeString;
    procedure SetMissionFileSP(const aMissionFileSP: UnicodeString);

    function GetDynamicFOW: Boolean;
    procedure SetDynamicFOW(const aDynamicFOW: Boolean);
  public
    constructor Create(aGameMode: TKMGameMode; out aSetGameTickEvent: TCardinalEvent; out aSetGameModeEvent: TKMGameModeSetEvent; out aSetMissionFileSP: TUnicodeStringEvent);
    destructor Destroy; override;

    property Mode: TKMGameMode read fMode;
    property MissionMode: TKMissionMode read fMissionMode write fMissionMode;
    property Tick: Cardinal read fTick;
    property VisibleLayers: TKMMapVisibleLayerSet read fVisibleLayers write fVisibleLayers;

    property Name: UnicodeString read fName write fName;
    property MapSimpleCRC: Cardinal read fMapSimpleCRC write fMapSimpleCRC;
    property GameMapFullCRC: Cardinal read fGameMapFullCRC write fGameMapFullCRC;
    property MissionFileSP: UnicodeString read fMissionFileSP;
    property MissionFile: UnicodeString read GetMissionFile;
    property MissionDifficulty: TKMMissionDifficulty read fMissionDifficulty write fMissionDifficulty;
    property DynamicFOW: Boolean read GetDynamicFOW write SetDynamicFOW;

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

    function HasMissionDifficulty: Boolean;

    {$IFDEF RUNNER}
    procedure GetGameModeSetEvent(out aSetGameModeEvent: TKMGameModeSetEvent);
    {$ENDIF}
  end;

var
  gGameParams: TKMGameParams;


implementation
uses
  KM_MapUtils;


{ TKMGameParams }
constructor TKMGameParams.Create(aGameMode: TKMGameMode; out aSetGameTickEvent: TCardinalEvent; out aSetGameModeEvent: TKMGameModeSetEvent; out aSetMissionFileSP: TUnicodeStringEvent);
begin
  inherited Create;

  fVisibleLayers := [mlObjects, mlHouses, mlUnits, mlOverlays];

  fMode := aGameMode;
  fTick := 0;
  fMissionDifficulty := mdNone;
  DynamicFOW := False;

  aSetGameTickEvent := SetGameTick;
  aSetGameModeEvent := SetGameMode;
  aSetMissionFileSP := SetMissionFileSP;

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


function TKMGameParams.GetDynamicFOW: Boolean;
begin
  if Self = nil then Exit(DYNAMIC_FOG_OF_WAR);
  
  Result := fDynamicFOW;
end;


function TKMGameParams.GetMissionFile: UnicodeString;
begin
  if not IsMultiplayer then
    Result := MissionFileSP //In SP we store it
  else
    //In MP we can't store it since it will be MapsMP or MapsDL on different clients
    Result := GuessMPPath(fName, '.dat', fGameMapFullCRC);
end;


procedure TKMGameParams.SetDynamicFOW(const aDynamicFOW: Boolean);
begin
  fDynamicFOW := aDynamicFOW;
end;


procedure TKMGameParams.SetGameMode(aGameMode: TKMGameMode);
begin
  fMode := aGameMode;
end;


procedure TKMGameParams.SetGameTick(aGameTick: Cardinal);
begin
  fTick := aGameTick;
end;


procedure TKMGameParams.SetMissionFileSP(const aMissionFileSP: UnicodeString);
begin
  fMissionFileSP := aMissionFileSP;
end;


function TKMGameParams.IsMapEditor: Boolean;
begin
  Result := fMode = gmMapEd;
end;


function TKMGameParams.IsCampaign: Boolean;
begin
  Result := fMode = gmCampaign;
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
  Result := fMode = gmMulti;
end;


// We often need to see if game is MP
function TKMGameParams.IsMultiPlayerOrSpec: Boolean;
begin
  Result := fMode in [gmMulti, gmMultiSpectate];
end;


function TKMGameParams.IsMultiplayer: Boolean;
begin
  Result := fMode in [gmMulti, gmMultiSpectate, gmReplayMulti];
end;


function TKMGameParams.IsSingleplayerGame: Boolean;
begin
  Result := fMode in [gmSingle, gmCampaign];
end;


function TKMGameParams.IsSingleplayer: Boolean;
begin
  Result := fMode in [gmSingle, gmCampaign, gmReplaySingle];
end;


function TKMGameParams.IsNormalGame: Boolean;
begin
  Result := fMode in [gmSingle, gmCampaign, gmMulti];
end;


function TKMGameParams.IsReplay: Boolean;
begin
  Result := fMode in [gmReplaySingle, gmReplayMulti];
end;


function TKMGameParams.IsReplayOrSpectate: Boolean;
begin
  Result := fMode in [gmMultiSpectate, gmReplaySingle, gmReplayMulti];
end;


function TKMGameParams.HasMissionDifficulty: Boolean;
begin
  Result := fMissionDifficulty <> mdNone;
end;


end.
