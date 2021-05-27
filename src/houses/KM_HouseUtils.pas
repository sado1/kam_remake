unit KM_HouseUtils;
{$I KaM_Remake.inc}
interface
uses
  KM_ResHouses, KM_ResTypes,
  KM_Defaults;

  function GetHouseDoorwayOffset(aHouseType: TKMHouseType; aCheck: TKMCheckAxis): Single;


implementation


// Get doorway offset in tile fraction
function GetHouseDoorwayOffset(aHouseType: TKMHouseType; aCheck: TKMCheckAxis): Single;
begin
  if aCheck = axX then
    Result := gResHouses[aHouseType].EntranceOffsetXpx - CELL_SIZE_PX div 2
  else
    Result := gResHouses[aHouseType].EntranceOffsetYpx;

  Result := Result / CELL_SIZE_PX;
end;

end.
