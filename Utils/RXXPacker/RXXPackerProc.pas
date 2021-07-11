unit RXXPackerProc;

{$I ..\..\KaM_Remake.inc}
interface
uses
  SysUtils, KM_ResTypes, KM_ResPalettes;

type
  TKMRXXPacker = class
  public
    SpritesBaseDir: string;
    constructor Create(const aSpritesBaseDir: string);
    procedure Pack(RT: TRXType; fPalettes: TKMResPalettes);
  end;

implementation
uses
  KM_ResHouses, KM_ResUnits, KM_ResSprites, KM_Points, KM_ResSpritesEdit, KM_Defaults, KM_Log;

{ TRXXPacker }
constructor TKMRXXPacker.Create(const aSpritesBaseDir: string);
begin
  inherited Create;

  SpritesBaseDir := aSpritesBaseDir;
end;


procedure TKMRXXPacker.Pack(RT: TRXType; fPalettes: TKMResPalettes);
var
  DeathAnimProcessed: array of Integer;
  DeathAnimCount: Integer;

  function DeathAnimAlreadyDone(aID: Integer):Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to DeathAnimCount - 1 do
      if DeathAnimProcessed[I] = aID then
        Exit(True);
  end;

var
  SpritePack: TKMSpritePackEdit;
  Step, SpriteID: Integer;
  RXName: string;
  resHouses: TKMResHouses;
  resUnits: TKMResUnits;
  UT: TKMUnitType;
  Dir: TKMDirection;
begin
  //ruCustom sprite packs do not have a main RXX file so don't need packing
  if RXInfo[RT].Usage <> ruCustom then
  begin
    RXName := SpritesBaseDir + 'SpriteResource\' + RXInfo[RT].FileName + '.rx';
    Assert((RT = rxTiles) or FileExists(RXName),
           'Cannot find ' + RXName + ' file.' + #13#10 +
           'Please copy the file from your KaM\data\gfx\res\ folder.');

    SpritePack := TKMSpritePackEdit.Create(RT, fPalettes);
    try
      //Load
      if RT <> rxTiles then
      begin
        SpritePack.LoadFromRXFile(RXName);
        SpritePack.OverloadFromFolder(SpritesBaseDir + 'SpriteResource\', False); // Do not soften shadows, it will be done later on
      end
      else
      if DirectoryExists(SpritesBaseDir + 'SpriteResource\') then
        SpritePack.LoadFromFolder(SpritesBaseDir + 'SpriteResource\');

      //Tiles must stay the same size as they can't use pivots
      if (RT <> rxTiles) and (gLog <> nil) then
        gLog.AddTime('Trimmed ' + IntToStr(SpritePack.TrimSprites));

      //Houses need some special treatment to adapt to GL_ALPHA_TEST that we use for construction steps
      if RT = rxHouses then
      begin
        resHouses := TKMResHouses.Create;
        SpritePack.AdjoinHouseMasks(resHouses);
        SpritePack.GrowHouseMasks(resHouses);
        SpritePack.RemoveSnowHouseShadows(resHouses);
        SpritePack.RemoveMarketWaresShadows(resHouses);
        resHouses.Free;
      end;

      // Determine objects size only for units (used for hitbox)
      //todo: do we need it for houses too ?
      if RT = rxUnits then
        SpritePack.DetermineImagesObjectSize;

      //The idea was to blur the water and make it semitrasparent, but it did not worked out as expected
      //if RT = rxTiles then
      //  SpritePack.SoftWater(nil);

      //Save
      SpritePack.SaveToRXXFile(ExeDir + 'data\Sprites\' + RXInfo[RT].FileName + '.rxx');

      //Generate alpha shadows for the following sprite packs
      if RT in [rxHouses,rxUnits,rxGui,rxTrees] then
      begin
        if RT = rxHouses then
        begin
          SpritePack.SoftenShadows(889, 892, False); //Smooth smoke
          SpritePack.SoftenShadows(1615, 1638, False); //Smooth flame
        end;
        if RT = rxUnits then
        begin
          SpritePack.SoftenShadows(6251, 6322, False); //Smooth thought bubbles
          //Smooth all death animations for all units
          resUnits := TKMResUnits.Create;
          DeathAnimCount := 0; //We need to remember which ones we've done because units reuse them
          SetLength(DeathAnimProcessed, 1000); //Hopefully more than enough
          for UT := HUMANS_MIN to HUMANS_MAX do
            for Dir := dirN to dirNW do
              for Step := 1 to 30 do
              begin
                SpriteID := resUnits[UT].UnitAnim[uaDie,Dir].Step[Step]+1; //Sprites in units.dat are 0 indexed
                if (SpriteID > 0)
                and not DeathAnimAlreadyDone(SpriteID) then
                begin
                  SpritePack.SoftenShadows(SpriteID, False);
                  DeathAnimProcessed[DeathAnimCount] := SpriteID;
                  inc(DeathAnimCount);
                end;
              end;
          resUnits.Free;
        end;

        if RT = rxGui then
        begin
          SpritePack.SoftenShadows(105, 128); //Field plans
          SpritePack.SoftenShadows(249, 281); //House tablets only (shadow softening messes up other rxGui sprites)
          SpritePack.SoftenShadows(461, 468); //Field fences
          SpritePack.SoftenShadows(660, 660); //Woodcutter cutting point sign
        end
        else
          SpritePack.SoftenShadows;

        SpritePack.SaveToRXXFile(ExeDir + 'data\Sprites\' + RXInfo[RT].FileName + '_a.rxx');

        if DirectoryExists(SpritesBaseDir + 'SpriteInterp\Output\' + IntToStr(Byte(RT)+1) + '\') then
          SpritePack.OverloadFromFolder(SpritesBaseDir + 'SpriteInterp\Output\' + IntToStr(Byte(RT)+1) + '\', False); // Shadows are already softened for interps

        SpritePack.SaveToRXAFile(ExeDir + 'data\Sprites\' + RXInfo[RT].FileName + '.rxa');
      end;
    finally
      SpritePack.Free;
    end;
  end;
end;

end.
