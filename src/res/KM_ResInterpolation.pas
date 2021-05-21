unit KM_ResInterpolation;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math,
  KM_Defaults, KM_Points, KM_ResTypes, KM_ResWares;

const
  INTERP_LEVEL = 8;

type
  TKMInterpolation = array[1..30, 0..INTERP_LEVEL-1] of Integer;
  TKMUnitActionInterp = array[UNIT_MIN..UNIT_MAX, UNIT_ACT_MIN..UNIT_ACT_MAX, dirN..dirNW] of TKMInterpolation;
  TKMSerfCarryInterp = array[WARE_MIN..WARE_MAX, dirN..dirNW] of TKMInterpolation;
  TKMUnitThoughtInterp = array[TKMUnitThought] of TKMInterpolation;

  TKMResInterpolation = class
  private
    fUnitActions: TKMUnitActionInterp;
    fSerfCarry: TKMSerfCarryInterp;
    fUnitThoughts: TKMUnitThoughtInterp;
  public
    procedure LoadFromFile(const FileName: string);

    function UnitAction(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; aStep: Integer; aStepFrac: Single): Integer;
    function UnitActionByPercent(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; aPercent: Single): Integer;
    function SerfCarry(aWare: TKMWareType; aDir: TKMDirection; aStep: Integer; aStepFrac: Single): Integer;
  end;

function GetHouseInterpSpriteOffset(aHT: TKMHouseType; aAct: TKMHouseActionType): Integer;
function GetTreeInterpSpriteOffset(aTree: Integer): Integer;
function GetThoughtInterpSpriteOffset(aTh: TKMUnitThought): Integer;


implementation
uses
  KM_CommonClasses, KM_ResMapElements, KM_Resource, KM_CommonTypes, KM_Pics;



function GetHouseInterpSpriteOffset(aHT: TKMHouseType; aAct: TKMHouseActionType): Integer;
const HOUSE_INTERP_LOOKUP: array[TKMHouseType, TKMHouseActionType] of Integer = (
  (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1), // htNone
  (-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1), // htAny
  (-1,2100,2100,2340,2340,2580,-1,2612,2852,-1,-1,2892,2940,2988,3036,3084,3132,3180,3228), // htArmorSmithy
  (-1,3276,3516,3756,-1,-1,-1,3996,4236,-1,-1,4316,2988,3228,3036,4364,4412,3132,2892), // htArmorWorkshop
  (-1,4460,4652,-1,-1,2580,-1,4892,5132,-1,-1,2892,5212,4316,4316,5260,5308,5356,5260), // htBakery
  (-1,-1,-1,-1,-1,-1,-1,-1,5404,5444,5484,5564,4364,4412,5612,3180,5308,5660,2988), // htBarracks
  (5708,5828,6044,6284,-1,-1,-1,6308,6468,-1,-1,5260,2988,3228,6548,3180,2892,3132,4412), // htButchers
  (6596,6836,-1,-1,6956,-1,-1,-1,7196,-1,-1,2892,4316,3228,3036,7236,7284,3132,3084), // htCoalMine
  (-1,-1,-1,-1,-1,-1,-1,7332,7572,-1,-1,3228,2892,4412,4316,5260,7652,5212,7700), // htFarm
  (-1,-1,-1,-1,-1,-1,-1,7748,5404,-1,-1,2892,3228,2988,3036,4412,3180,4412,5212), // htFisherHut
  (-1,7988,-1,-1,-1,-1,-1,8116,8356,8396,-1,2892,4316,3228,3036,7236,7284,3084,2892), // htGoldMine
  (-1,-1,-1,-1,-1,-1,-1,-1,8436,8396,8476,3228,2988,5308,5612,2940,3180,4412,6548), // htInn
  (-1,8556,-1,-1,-1,-1,-1,8684,8436,-1,-1,2892,4316,2940,3036,7236,3132,3084,3036), // htIronMine
  (-1,8924,9164,-1,-1,2580,-1,9404,9484,-1,-1,2892,3228,7236,4364,3036,4412,5260,7700), // htIronSmithy
  (-1,-1,-1,-1,-1,-1,-1,-1,9564,9604,9644,9684,9732,9780,9828,9876,9924,9972,10020), // htMarketplace
  (-1,10068,10292,10532,-1,2580,-1,10772,2852,-1,-1,3084,5212,4364,4412,3180,4316,2988,3084), // htMetallurgists
  (-1,11012,-1,-1,-1,-1,11076,11156,8356,-1,-1,7236,3132,2988,3228,2892,5564,3084,5308), // htMill
  (-1,11396,-1,-1,11540,-1,-1,11780,8356,-1,-1,7236,3132,2940,3084,7700,5564,4412,2892), // htQuary
  (11924,12164,-1,-1,12244,-1,-1,12484,12724,-1,-1,2892,3228,4316,4412,3036,5260,7236,3084), // htSawmill
  (12804,13044,13284,13524,13764,-1,-1,-1,8476,-1,-1,3132,5308,5564,5612,5260,5308,7284,3132), // htSchool
  (-1,14004,14244,14484,-1,-1,-1,14724,8356,-1,-1,2892,3228,4364,2892,2892,3228,3084,7236), // htSiegeWorkshop
  (14964,15204,15444,15684,15924,-1,-1,16164,16308,-1,-1,3132,4412,5612,5564,3228,2988,7652,7284), // htStables
  (-1,-1,-1,-1,-1,-1,-1,-1,5444,5404,-1,3132,2892,2988,3228,5212,3180,4412,5260), // htStore
  (-1,16388,16628,-1,-1,-1,-1,16868,17108,-1,-1,3132,5660,3228,3084,2988,3180,6548,2940), // htSwine
  (17188,17420,-1,-1,-1,2580,-1,17516,17756,-1,-1,5260,4364,2988,7236,3228,6548,3084,2940), // htTannery
  (-1,-1,-1,-1,-1,-1,-1,-1,8356,17756,-1,2892,3228,4364,5308,5660,3180,6548,5660), // htTownHall
  (-1,-1,-1,-1,-1,-1,-1,17796,4236,-1,-1,2892,3228,4316,4364,5564,7284,2940,4412), // htWatchTower
  (18036,18276,18276,18516,18756,2580,-1,18996,19236,-1,-1,2892,3228,4316,4412,3036,5260,4364,7236), // htWeaponSmithy
  (19316,19444,19684,19924,20164,-1,-1,20404,20644,-1,-1,7236,2988,3132,6548,3228,3036,7652,4412), // htWeaponWorkshop
  (20724,20964,-1,-1,21156,-1,-1,21380,7196,-1,-1,5564,5356,2988,4412,3180,5612,5564,2940), // htWineyard
  (-1,-1,-1,-1,-1,-1,-1,21620,7196,-1,-1,2892,2940,3132,4316,3036,7236,5564,3228) // htWoodcutters
);
begin
  if INTERPOLATED_ANIMS then
  begin
    Result := HOUSE_INTERP_LOOKUP[aHT, aAct];
  end
  else
    Result := -1;
end;


function GetTreeInterpSpriteOffset(aTree: Integer): Integer;
const
  TREE_INTERP_LOOKUP: array [0..OBJECTS_CNT] of Integer = (
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,260,388,516,644,772,868,964,-1,964,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,1060,1124,1188,1252,1412,1460,1524,1588,1652,
  1060,1124,1188,1812,1876,1060,1124,1188,2036,2100,1060,1124,2260,2324,2388,2548,
  2596,2692,2788,2548,2596,2692,2948,3044,1412,3204,3268,3332,3396,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,3556,3628,3676,3740,3556,3628,3900,3964,3556,3628,3900,4124,
  4188,3556,3628,3676,4348,4412,3556,3628,3676,4572,4412,4636,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,4700,4796,4892,4988,5084,5180,5276,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1);
begin
  if INTERPOLATED_ANIMS and (aTree <= High(TREE_INTERP_LOOKUP)) then
  begin
    Result := TREE_INTERP_LOOKUP[aTree];
  end
  else
    Result := -1;
end;


function GetThoughtInterpSpriteOffset(aTh: TKMUnitThought): Integer;
const THOUGHT_INTERP_LOOKUP: array[TKMUnitThought] of Integer = (
  -1,90156,90220,90284,90348,90412,90476,90540,90604
);
begin
  if INTERPOLATED_ANIMS then
  begin
    Result := THOUGHT_INTERP_LOOKUP[aTh];
  end
  else
    Result := -1;
end;


{ TKMResInterpolation }

procedure TKMResInterpolation.LoadFromFile(const FileName: string);
var
  S: TKMemoryStreamBinary;
begin
  if not FileExists(FileName) then Exit;

  S := TKMemoryStreamBinary.Create;
  S.LoadFromFile(FileName);

  S.CheckMarker('UnitAction');
  S.Read(fUnitActions, SizeOf(fUnitActions));

  S.CheckMarker('SerfCarry ');
  S.Read(fSerfCarry, SizeOf(fSerfCarry));

  S.CheckMarker('UnitThoughts  ');
  S.Read(fUnitThoughts, SizeOf(fUnitThoughts));
end;


function TKMResInterpolation.UnitAction(aUnit: TKMUnitType;
  aAct: TKMUnitActionType; aDir: TKMDirection; aStep: Integer;
  aStepFrac: Single): Integer;
var
  A: TKMAnimLoop;
  Step, SubStep, InterpOffset: Integer;
begin
  A := gRes.Units[aUnit].UnitAnim[aAct, aDir];

  Step := aStep mod Byte(A.Count) + 1;
  SubStep := EnsureRange(Floor(INTERP_LEVEL*aStepFrac), 0, INTERP_LEVEL-1);

  Result := fUnitActions[aUnit, aAct, aDir, Step, SubStep];

  //While in development disable interpolation if the sprite is missing
  if (Result <= 0) or (Result > gRes.Sprites[rxUnits].RXData.Count) or (gRes.Sprites[rxUnits].RXData.Size[Result].X = 0) then
    Result := A.Step[Step] + 1;
end;


function TKMResInterpolation.UnitActionByPercent(aUnit: TKMUnitType;
  aAct: TKMUnitActionType; aDir: TKMDirection; aPercent: Single): Integer;
var
  count: Integer;
  fracStep: Single;
begin
  count := gRes.Units[aUnit].UnitAnim[aAct, aDir].Count;
  fracStep := Min(aPercent, 1.0) * (count-1);
  Result := UnitAction(aUnit, aAct, aDir, Trunc(fracStep), Frac(fracStep));
end;


function TKMResInterpolation.SerfCarry(aWare: TKMWareType; aDir: TKMDirection;
  aStep: Integer; aStepFrac: Single): Integer;
var
  A: TKMAnimLoop;
  Step, SubStep, InterpOffset: Integer;
begin
  A := gRes.Units.SerfCarry[aWare, aDir];

  Step := aStep mod Byte(A.Count) + 1;
  SubStep := EnsureRange(Floor(INTERP_LEVEL*aStepFrac), 0, INTERP_LEVEL-1);

  Result := fSerfCarry[aWare, aDir, Step, SubStep];

  //While in development disable interpolation if the sprite is missing
  if (Result <= 0) or (Result > gRes.Sprites[rxUnits].RXData.Count) or (gRes.Sprites[rxUnits].RXData.Size[Result].X = 0) then
    Result := A.Step[Step] + 1;
end;


end.
