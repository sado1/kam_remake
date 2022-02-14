unit KM_ScriptingTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_AITypes, KM_Points;

type
  TKMScriptEventType = (
    evtBeacon,
    evtFieldBuilt,
    evtHouseAfterDestroyed,
    evtHouseBuilt,
    evtHousePlanDigged,
    evtHousePlanPlaced,
    evtHousePlanRemoved,
    evtHouseDamaged,
    evtHouseDestroyed,
    evtHouseRepaired,
    evtHouseWareCountChanged,
    evtGameSpeedChanged,
    evtGroupHungry,
    evtGroupOrderAttackHouse,
    evtGroupOrderAttackUnit,
    evtGroupBeforeOrderSplit,
    evtGroupOrderMove,
    evtGroupOrderLink,
    evtGroupOrderSplit,
    evtMarketTrade,
    evtMissionStart,
    evtPeacetimeEnd,
    evtPlanRoadDigged,
    evtPlanRoadPlaced,
    evtPlanRoadRemoved,
    evtPlanFieldPlaced,
    evtPlanFieldRemoved,
    evtPlanWinefieldDigged,
    evtPlanWinefieldPlaced,
    evtPlanWinefieldRemoved,
    evtPlayerDefeated,
    evtPlayerVictory,
    evtRoadBuilt,
    evtTick,
    evtUnitAfterDied,
    evtUnitDied,
    evtUnitTrained,
    evtUnitWounded,
    evtUnitAttacked,
    evtWareProduced,
    evtWarriorEquipped,
    evtWarriorWalked,
    evtWinefieldBuilt
  );


  TKMScriptFileInfo = record
    FullFilePath: UnicodeString;
    FileName: UnicodeString;
    FileText: AnsiString;
  end;

  // Script error message
  TKMScriptErrorMessage = record
    GameMessage: UnicodeString; // Shown in game as Message box
    LogMessage: UnicodeString;  // Printed to Log (could be more detailed)
  end;

  TKMScriptErrorType = (seInvalidParameter, seException, sePreprocessorError, seCompileError, seCompileWarning, seCompileHint, seLog);

  TKMScriptErrorEvent = procedure (aType: TKMScriptErrorType; const aErrorString: UnicodeString; const aDetailedErrorString: UnicodeString = '') of object;

  TKMDefencePositionInfo = record
    X, Y: Integer;
    Dir: TKMDirection;
    Radius: Integer;
    GroupType: TKMGroupType;
    PositionType: TKMAIDefencePosType;
    function ToStr: string;
  end;

const
  SCRIPT_LOG_EXT = '.log.txt';


implementation
uses
  SysUtils, TypInfo;

function TKMDefencePositionInfo.ToStr: string;
begin
  Result := Format('[%d:%d] Dir=%s Radius=%d GroupType=%s PositionType=%s',
                   [X, Y, GetEnumName(TypeInfo(TKMDirection), Integer(Dir)), Radius,
                    GetEnumName(TypeInfo(TKMGroupType), Integer(GroupType)),
                    GetEnumName(TypeInfo(TKMAIDefencePosType), Integer(PositionType))]);
end;


end.

