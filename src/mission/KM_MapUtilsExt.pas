unit KM_MapUtilsExt;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_Maps, KM_MapTypes;

  function TryOpenMapPDF(aMap: TKMMapInfo): Boolean;

  function GetGoalDescription(aPlayer1, aPlayer2: TKMHandID; aGoalType: TKMGoalType; aGoalCondition: TKMGoalCondition;
                              aColPlayer1, aColPlayer2, aColTxt, aColBld: Cardinal): string;

implementation
uses
  SysUtils,
  KM_Resource, KM_ResTexts,
  KM_CommonShellUtils, KM_CommonUtils;


function TryOpenMapPDF(aMap: TKMMapInfo): Boolean;
var
  pdfPath: string;
begin
  Result := False;
  if aMap = nil then Exit;

  pdfPath := aMap.DetermineReadmeFilePath;
  if pdfPath <> '' then
    Result := ShellOpenFile(pdfPath);
end;


// Format mission goal description
function GetGoalDescription(aPlayer1, aPlayer2: TKMHandID; aGoalType: TKMGoalType; aGoalCondition: TKMGoalCondition;
                            aColPlayer1, aColPlayer2, aColTxt, aColBld: Cardinal): string;
type
  TKMGoalTypeDescWordKind = (dwkFirst, dwkSecond);
const
  GOAL_TYPE_DESC_TX: array[TKMGoalType, TKMGoalTypeDescWordKind] of Integer = (
    (TX_MAPED_GOALS_TYPE_NONE,            TX_MAPED_GOALS_TYPE_NONE),
    (TX_GOAL_CONDITION_DESC_WIN_WORD,     TX_GOAL_CONDITION_DESC_DESTROYED_WORD),
    (TX_GOAL_CONDITION_DESC_SURVIVE_WORD, TX_GOAL_CONDITION_DESC_SAVED_WORD)
  );

  GOAL_COND_DESC_WORD_TX: array[TKMGoalCondition] of Integer = (
    TX_MAPED_GOALS_CONDITION_NONE,
    TX_MAPED_GOALS_CONDITION_TUTORIAL,
    TX_MAPED_GOALS_CONDITION_TIME,
    TX_MAPED_GOALS_CONDITION_BUILDS,
    TX_MAPED_GOALS_CONDITION_TROOPS,
    TX_MAPED_GOALS_CONDITION_UNKNOWN,
    TX_MAPED_GOALS_CONDITION_ASSETS,
    TX_MAPED_GOALS_CONDITION_SERFS,
    TX_MAPED_GOALS_CONDITION_ECONOMY
  );

var
  I: Integer;
  goalCondStr, housesStr: string;
begin
  case aGoalCondition of
    gcBuildings:
      begin
        housesStr := '';
        for I := Low(GOAL_BUILDINGS_HOUSES) to High(GOAL_BUILDINGS_HOUSES) do
        begin
          if housesStr <> '' then
            housesStr := housesStr + ', ';

          housesStr := housesStr + gRes.Houses[GOAL_BUILDINGS_HOUSES[I]].HouseName;
        end;
        goalCondStr := Format(gResTexts[TX_GOAL_CONDITION_COND_DESC_BUILDINGS], [WrapWrappedColor(housesStr, aColBld)]);
      end;
    else
      goalCondStr := gResTexts[GOAL_COND_DESC_WORD_TX[aGoalCondition]];
  end;

  // Format text out of string like:
  // 'For player %0:s to %1:s player's %2:s %3:s must be %4:s'
  // We want to get smth like (depends of the locale, ofc)
  // 'For player 1 to win player's 2 troops must be destroyed'
  Result := Format(gResTexts[TX_GOAL_CONDITION_DESC_PATTERN], [WrapColor(aPlayer1 + 1, aColPlayer1),
                                                               WrapColor(gResTexts[GOAL_TYPE_DESC_TX[aGoalType, dwkFirst]], aColTxt),
                                                               WrapColor(aPlayer2 + 1, aColPlayer2),
                                                               WrapColor(goalCondStr, aColTxt),
                                                               WrapColor(gResTexts[GOAL_TYPE_DESC_TX[aGoalType, dwkSecond]], aColTxt)]);
end;

end.
