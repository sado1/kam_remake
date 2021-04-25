unit KM_MapUtils;
{$I KaM_Remake.inc}
interface

  function GuessMPPathRel(const aName, aExt: string; aCRC: Cardinal): string;
  function GuessMissionPathRel(const aMissionFileRelSP, aMissionName: string; aMapFullCRC: Cardinal; aIsMultiplayer: Boolean): string;

implementation
uses
  SysUtils,
  KM_Defaults, KM_MapTypes;

function GuessMPPathRel(const aName, aExt: string; aCRC: Cardinal): string;
var
  S: UnicodeString;
begin
  S := aName + '_' + IntToHex(aCRC, 8);
  Result := MAP_FOLDER[mfDL] + PathDelim + S + PathDelim + S + aExt;
  if not FileExists(ExeDir + Result) then
    Result := MAP_FOLDER[mfMP] + PathDelim + aName + PathDelim + aName + aExt;
end;


function GuessMissionPathRel(const aMissionFileRelSP, aMissionName: string; aMapFullCRC: Cardinal; aIsMultiplayer: Boolean): string;
begin
  if aIsMultiplayer then
    //In MP we can't store it since it will be MapsMP or MapsDL on different clients
    Result := GuessMPPathRel(aMissionName, '.dat', aMapFullCRC)
  else
    Result := aMissionFileRelSP; //In SP we store it
end;


end.

