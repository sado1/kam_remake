unit KM_GameTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_MapTypes;

type
  TKMGameMode = (
    gmSingle,
    gmCampaign,
    gmMulti,        //Different GIP, networking,
    gmMultiSpectate,
    gmMapEd,        //Army handling, lite updates,
    gmReplaySingle, //No input, different results screen to gmReplayMulti
    gmReplayMulti   //No input, different results screen to gmReplaySingle
    );

  TKMAIType = (aitNone, aitClassic, aitAdvanced);
  TKMAITypeSet = set of TKMAIType;

  TKMGameModeChangeEvent = procedure (aGameMode: TKMGameMode) of object;

  TKMNewSingleMapEvent = procedure (const aMissionFile, aGameName: UnicodeString; aDesiredLoc: ShortInt = -1;
                                    aDesiredColor: Cardinal = $00000000; aDifficulty: TKMMissionDifficulty = mdNone;
                                    aAIType: TKMAIType = aitNone; aAutoselectHumanLoc: Boolean = False) of object;

implementation

end.
