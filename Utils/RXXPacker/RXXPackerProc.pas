unit RXXPackerProc;
{$I ..\..\KaM_Remake.inc}
interface
uses
  SysUtils, Windows, Generics.Collections,
  KM_ResTypes, KM_ResPalettes, KM_ResSprites;


type
  TKMRXXPacker = class
  private
    fSourcePath2: string;
    fSourcePathRXA2: string;
    fRXXSavePath: string;

    procedure SetRXXSavePath(const aValue: string);
    procedure Pack(aRT: TRXType; aPalettes: TKMResPalettes; aOnMessage: TProc<string>);
  public
    PackToRXX: Boolean;
    PackToRXA: Boolean;
    RXXFormat: TKMRXXFormat;

    constructor Create;

    property SourcePath2: string read fSourcePath2 write fSourcePath2;
    property SourcePathRXA2: string read fSourcePathRXA2 write fSourcePathRXA2;
    property RXXSavePath: string read fRXXSavePath write SetRXXSavePath;

    procedure Pack2(aRxSet: TRXTypeSet; aPalettes: TKMResPalettes; aOnMessage: TProc<string>);

    class function GetAvailableToPack(const aPath: string): TRXTypeSet;
  end;


implementation
uses
  KM_ResHouses, KM_ResUnits, KM_Points, KM_ResSpritesEdit, KM_Defaults, KM_Log;


{ TKMRXXPacker }
constructor TKMRXXPacker.Create;
begin
  inherited;

  // Default values
  PackToRXX := True;
  PackToRXA := False;
  RXXFormat := rxxOne;
end;


procedure TKMRXXPacker.SetRXXSavePath(const aValue: string);
begin
  fRXXSavePath := IncludeTrailingPathDelimiter(aValue);
end;


class function TKMRXXPacker.GetAvailableToPack(const aPath: string): TRXTypeSet;
var
  RT: TRXType;
begin
  Result := [rxTiles]; //Tiles are always in the list

  for RT := Low(TRXType) to High(TRXType) do
    if FileExists(aPath + RX_INFO[RT].FileName + '.rx') then
      Result := Result + [RT];
end;


procedure TKMRXXPacker.Pack(aRT: TRXType; aPalettes: TKMResPalettes; aOnMessage: TProc<string>);
var
  rxName: string;
  deathAnimProcessed: TList<Integer>;
  spritePack: TKMSpritePackEdit;
  trimmedAmount: Cardinal;
  step, spriteID: Integer;
  resHouses: TKMResHouses;
  resUnits: TKMResUnits;
  UT: TKMUnitType;
  dir: TKMDirection;
begin
  //ruCustom sprite packs do not have a main RXX file so don't need packing
  if RX_INFO[aRT].Usage = ruCustom then Exit;

  rxName := fSourcePath2 + RX_INFO[aRT].FileName + '.rx';

  if (aRT <> rxTiles) and not FileExists(rxName) then
    raise Exception.Create('Cannot find "' + rxName + '" file.' + sLineBreak + 'Please copy the file from your KaM\data\gfx\res\ folder.');

  spritePack := TKMSpritePackEdit.Create(aRT, aPalettes);
  try
    // Load base sprites from original KaM RX packages
    if aRT <> rxTiles then
    begin
      // Load base RX
      spritePack.LoadFromRXFile(rxName);
      // Overload (something we dont need in RXXPacker, cos all the custom sprites are in other folders)
      spritePack.OverloadRXDataFromFolder(fSourcePath2, False); // Do not soften shadows, it will be done later on
      trimmedAmount := spritePack.TrimSprites;

      aOnMessage('  trimmed ' + IntToStr(trimmedAmount) + ' bytes');
    end
    else
      if DirectoryExists(fSourcePath2) then
        spritePack.OverloadRXDataFromFolder(fSourcePath2);
      // Tiles don't need to be trimmed, as they can't use pivots

    // Houses need some special treatment to adapt to GL_ALPHA_TEST that we use for construction steps
    if aRT = rxHouses then
    begin
      resHouses := TKMResHouses.Create;
      spritePack.AdjoinHouseMasks(resHouses);
      spritePack.GrowHouseMasks(resHouses);
      spritePack.RemoveSnowHouseShadows(resHouses);
      spritePack.RemoveMarketWaresShadows(resHouses);
      resHouses.Free;
    end;

    // Determine objects size only for units (used for hitbox)
    //todo: do we need it for houses too ?
    if aRT = rxUnits then
      spritePack.DetermineImagesObjectSizeAll;

    // The idea was to blur the water and make it semi-trasparent, but it did not work out as expected
    //if RT = rxTiles then
    //  SpritePack.SoftWater(nil);

    // Save
    if PackToRXX then
      spritePack.SaveToRXXFile(fRXXSavePath + 'data\Sprites\' + RX_INFO[aRT].FileName + '.rxx', RXXFormat);

    // Generate alpha shadows for the following sprite packs
    if aRT in [rxHouses, rxUnits, rxGui, rxTrees] then
    begin
      if aRT = rxHouses then
      begin
        spritePack.SoftenShadowsRange(889, 892, False); // Smooth smoke
        spritePack.SoftenShadowsRange(1615, 1638, False); // Smooth flame
      end;

      if aRT = rxUnits then
      begin
        spritePack.SoftenShadowsRange(6251, 6322, False); // Smooth thought bubbles

        resUnits := TKMResUnits.Create; // Smooth all death animations for all units
        deathAnimProcessed := TList<Integer>.Create; // We need to remember which ones we've done because units reuse them
        try
          for UT := HUMANS_MIN to HUMANS_MAX do
          for dir := dirN to dirNW do
          for step := 1 to 30 do
          begin
            spriteID := resUnits[UT].UnitAnim[uaDie,dir].Step[step]+1; //Sprites in units.dat are 0 indexed
            if (spriteID > 0)
            and not deathAnimProcessed.Contains(spriteID) then
            begin
              spritePack.SoftenShadowsRange(spriteID, spriteID, False);
              deathAnimProcessed.Add(spriteID);
            end;
          end;
        finally
          deathAnimProcessed.Free;
          resUnits.Free;
        end;
      end;

      if aRT = rxGui then
      begin
        spritePack.SoftenShadowsRange(105, 128); //Field plans
        spritePack.SoftenShadowsRange(249, 281); //House tablets only (shadow softening messes up other rxGui sprites)
        spritePack.SoftenShadowsRange(461, 468); //Field fences
        spritePack.SoftenShadowsRange(660, 660); //Woodcutter cutting point sign
      end
      else
        spritePack.SoftenShadowsRange(1, spritePack.RXData.Count);

      if PackToRXX then
        spritePack.SaveToRXXFile(fRXXSavePath + 'data\Sprites\' + RX_INFO[aRT].FileName + '_a.rxx', RXXFormat);

      if PackToRXA then
      begin
        if DirectoryExists(fSourcePathRXA2 + IntToStr(Ord(aRT)+1) + '\') then
          spritePack.OverloadRXDataFromFolder(fSourcePathRXA2 + IntToStr(Ord(aRT)+1) + '\', False); // Shadows are already softened for interps

        spritePack.SaveToRXAFile(fRXXSavePath + 'data\Sprites\' + RX_INFO[aRT].FileName + '.rxa', RXXFormat);
      end;
    end;
  finally
    spritePack.Free;
  end;
end;


procedure TKMRXXPacker.Pack2(aRxSet: TRXTypeSet; aPalettes: TKMResPalettes; aOnMessage: TProc<string>);
var
  rxType: TRXType;
  tick, tickTotal: Cardinal;
begin
  if not DirectoryExists(fSourcePath2) then
  begin
    aOnMessage('Cannot find "' + fSourcePath2 + '" folder.' + sLineBreak + 'Please make sure this folder exists.');
    Exit;
  end;

  tickTotal := GetTickCount;

  for rxType := Low(TRXType) to High(TRXType) do
  if rxType in aRxSet then
  begin
    aOnMessage('Packing ' + RX_INFO[rxType].FileName + '.rxx ... ');
    tick := GetTickCount;
    Pack(rxType, aPalettes, aOnMessage);
    aOnMessage('... packed in ' + IntToStr(GetTickCount - tick) + ' ms');
  end;

  aOnMessage('Everything packed in ' + IntToStr(GetTickCount - tickTotal) + ' ms');
end;


end.
