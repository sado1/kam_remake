unit KM_RenderPool;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics,
  dglOpenGL, SysUtils, KromOGLUtils, KromUtils, Math,
  KM_Defaults, KM_CommonTypes, KM_CommonClasses, KM_Pics, KM_Points, KM_Render, KM_Viewport,
  KM_RenderTerrain, KM_ResHouses, KM_ResSprites, KM_Units,
  KM_Houses, KM_Terrain, KM_Projectiles, KM_RenderDebug,
  KM_ResTypes;

type
  TKMPaintLayer = (plTerrain, plObjects, plCursors);

  TKMRenderSprite = record
    Loc: TKMPointF; // Where sprite lower-left corner is located
    Feet: TKMPointF; // Feet of the sprite for FOW calculation (X;Y) and Z ordering (Y only)
    RX: TRXType;
    ID: Integer;
    UID: Integer;
    NewInst: Boolean;
    TeamColor: Cardinal;
    AlphaStep: Single; // Only apply-able to HouseBuild
    SelectionRect: TKMRectF; // Used for selecting units by sprite
  end;

  // List of sprites prepared to be rendered
  TRenderList = class
  private
    fUnitsRXData: TRXData; //shortcut
    fCount: Word;
    RenderOrder: array of Word; // Order in which sprites will be drawn ()
    RenderList: array of TKMRenderSprite;

    fStat_Sprites: Integer; // Total sprites in queue
    fStat_Sprites2: Integer;// Rendered sprites
    procedure ClipRenderList;
    procedure SendToRender(aId: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSprite(aRX: TRXType; aID: Integer; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
    procedure AddSpriteG(aRX: TRXType; aID: Integer; aUID: Integer; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);

    property Stat_Sprites: Integer read fStat_Sprites;
    property Stat_Sprites2: Integer read fStat_Sprites2;
    function GetSelectionUID(const CurPos: TKMPointF): Integer;
    procedure Clear;
    procedure SortRenderList;
    procedure Render;
  end;

  // Collect everything that need to be rendered and put it in a list
  TRenderPool = class
  private
    fCounter: Cardinal;
    fRXData: array [TRXType] of TRXData; // Shortcuts
    fViewport: TKMViewport;
    fRender: TRender;
    // fSampleHouse: TOBJModel;
    rPitch,rHeading,rBank: Integer;
    fRenderList: TRenderList;
    fRenderTerrain: TRenderTerrain;
    fRenderDebug: TKMRenderDebug;

    fFieldsList: TKMPointTagList;
    fHousePlansList: TKMPointDirList;
    fTabletsList: TKMPointTagList;
    fMarksList: TKMPointTagList;
    fHouseOutline: TKMPointList;

    function GetUnitAnimSprite(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; aStep: Integer; aStepFrac: Single): Integer;
    function GetUnitAnimSpriteByPercent(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; aPercent: Single): Integer;

    procedure ApplyTransform;
    procedure SetDefaultRenderParams;
    procedure RenderBackgroundUI(const aRect: TKMRect);
    // Terrain overlay cursors rendering (incl. sprites highlighting)
    procedure RenderForegroundUI;
    procedure RenderForegroundUI_Brush;
    procedure RenderForegroundUI_ElevateEqualize;
    procedure RenderForegroundUI_ObjectsBrush;
    procedure RenderForegroundUI_Markers;
    procedure RenderForegroundUI_Units;
    procedure RenderForegroundUI_PaintBucket(aHighlightAll: Boolean);
    procedure RenderForegroundUI_UniversalEraser(aHighlightAll: Boolean);
    function TryRenderUnitOrGroup(aObject: TObject; aUnitFilterFunc, aGroupFilterFunc: TBooleanFunc; aUseGroupFlagColor, aDoHighlight: Boolean; aHandColor, aFlagColor: Cardinal; aHighlightColor: Cardinal = 0): Boolean;
    procedure RenderUnit(U: TKMUnit; const P: TKMPoint; FlagColor: Cardinal; DoHighlight: Boolean; HighlightColor: Cardinal);
    function PaintBucket_UnitToRender(aUnit: TObject): Boolean;
    function PaintBucket_GroupToRender(aGroup: TObject): Boolean;

    procedure RenderSprite(aRX: TRXType; aId: Integer; pX,pY: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False);
    procedure RenderSpriteAlphaTest(aRX: TRXType; aId: Integer; aWoodProgress: Single; pX, pY: Single; aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
    procedure RenderMapElement1(aIndex: Word; AnimStep: Cardinal; LocX,LocY: Integer; aLoopAnim: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderMapElement4(aIndex: Word; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderHouseOutline(aHouseSketch: TKMHouseSketch; aCol: Cardinal = icCyan);

    // Terrain rendering sub-class
    procedure CollectPlans(const aRect: TKMRect);
    procedure CollectTerrainObjects(const aRect: TKMRect; aAnimStep: Cardinal);
    procedure PaintFlagPoint(const aHouseEntrance, aFlagPoint: TKMPoint; aColor: Cardinal; aTexId: Integer; aFirstPass: Boolean;
                             aDoImmediateRender: Boolean = False);
    procedure PaintFlagPoints(aFirstPass: Boolean);

    procedure RenderWireHousePlan(const P: TKMPoint; aHouseType: TKMHouseType);
    procedure RenderTileOwnerLayer(const aRect: TKMRect);
    procedure RenderTilesGrid(const aRect: TKMRect);

    procedure RenderWireTileInt(const X,Y: Integer);
    procedure RenderTileInt(const X, Y: Integer);
  public
    constructor Create(aViewport: TKMViewport; aRender: TRender);
    destructor Destroy; override;

    procedure ReInit;

    procedure AddAlert(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4);
    procedure AddProjectile(aProj: TKMProjectileType; const aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
    procedure AddHouse(aHouse: TKMHouseType; const aLoc: TKMPoint; aWoodStep, aStoneStep, aSnowStep: Single; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
    procedure AddWholeHouse(H: TKMHouse; FlagColor: Cardinal; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);

    procedure AddHouseTablet(aHouse: TKMHouseType; const Loc: TKMPoint);
    procedure AddHouseBuildSupply(aHouse: TKMHouseType; const Loc: TKMPoint; Wood,Stone: Byte);
    procedure AddHouseWork(aHouse: TKMHouseType; const Loc: TKMPoint; aActSet: TKMHouseActionSet; AnimStep: Cardinal; FlagColor: TColor4; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
    procedure AddHouseSupply(aHouse: TKMHouseType; const Loc: TKMPoint; const R1, R2, R3: array of Byte; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
    procedure AddHouseMarketSupply(const Loc: TKMPoint; ResType: TKMWareType; ResCount: Word; AnimStep: Integer);
    procedure AddHouseStableBeasts(aHouse: TKMHouseType; const Loc: TKMPoint; BeastId,BeastAge,AnimStep: Integer; aRX: TRXType = rxHouses);
    procedure AddHouseEater(const Loc: TKMPoint; aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; OffX,OffY: Single; FlagColor: TColor4);
    procedure AddUnit(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; StepFrac: Single; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
    procedure AddUnitCarry(aCarry: TKMWareType; aUID: Integer; aDir: TKMDirection; StepId: Integer; StepFrac: Single; pX,pY: Single);
    procedure AddUnitThought(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; Thought: TKMUnitThought; pX,pY: Single);
    procedure AddUnitFlag(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; FlagAnim: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False);
    procedure AddUnitWithDefaultArm(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; DoHignlight: Boolean = False; HighlightColor: TColor4 = 0);

    procedure RenderMapElement(aIndex: Word; AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderSpriteOnTile(const aLoc: TKMPoint; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF);
    procedure RenderSpriteOnTerrain(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF; aForced: Boolean = False);
    procedure RenderTile(aTerrainId: Word; pX,pY,Rot: Integer);
    procedure RenderWireTile(const P: TKMPoint; aCol: TColor4; aInset: Single = 0.0; aLineWidth: Single = -1);

    property RenderDebug: TKMRenderDebug read fRenderDebug;

    property RenderList: TRenderList read fRenderList;
    property RenderTerrain: TRenderTerrain read fRenderTerrain;
    procedure SetRotation(aH,aP,aB: Integer);

    procedure Render(aTickLag: Single);
  end;


var
  gRenderPool: TRenderPool;


implementation
uses
  KM_RenderAux, KM_HandsCollection, KM_Game, KM_GameSettings, KM_Sound, KM_Resource, KM_ResUnits,
  KM_ResMapElements, KM_AIFields, KM_TerrainPainter, KM_Cursor,
  KM_Hand, KM_UnitGroup, KM_CommonUtils,
  KM_GameParams, KM_Utils, KM_ResTileset, KM_DevPerfLog, KM_DevPerfLogTypes,
  KM_HandTypes,
  KM_TerrainTypes,
  KM_HandEntity;


const
  DELETE_COLOR = $1616FF;
  INTERP_LEVEL = 8;


constructor TRenderPool.Create(aViewport: TKMViewport; aRender: TRender);
var
  RT: TRXType;
begin
  inherited Create;

  fCounter := 0;

  for RT := Low(TRXType) to High(TRXType) do
    fRXData[RT] := gRes.Sprites[RT].RXData;

  fRender := aRender;
  fViewport := aViewport;

  fRenderList     := TRenderList.Create;
  fRenderTerrain  := TRenderTerrain.Create;
  fRenderDebug    := TKMRenderDebug.Create;
  gRenderAux      := TRenderAux.Create;

  fFieldsList     := TKMPointTagList.Create;
  fHousePlansList := TKMPointDirList.Create;
  fTabletsList    := TKMPointTagList.Create;
  fMarksList      := TKMPointTagList.Create;
  fHouseOutline   := TKMPointList.Create;
  // fSampleHouse := TOBJModel.Create;
  // fSampleHouse.LoadFromFile(ExeDir + 'Store.obj');
end;


destructor TRenderPool.Destroy;
begin
  fFieldsList.Free;
  fHousePlansList.Free;
  fTabletsList.Free;
  fMarksList.Free;
  fHouseOutline.Free;
  // fSampleHouse.Free;
  fRenderList.Free;
  fRenderDebug.Free;
  fRenderTerrain.Free;
  gRenderAux.Free;

  inherited;
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


function GetTreeAnimSprite(aTree, aStep: Integer; aStepFrac: Single; aLoop: Boolean): Integer;
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
var
  A: TKMAnimLoop;
begin
  A := gMapElements[aTree].Anim;
  if INTERPOLATED_ANIMS and (aTree <= High(TREE_INTERP_LOOKUP)) and (TREE_INTERP_LOOKUP[aTree] > 0) then
  begin
    Result := TREE_INTERP_LOOKUP[aTree]
      + INTERP_LEVEL*(aStep mod Byte(A.Count));

    //For the last step of non-looping animations, skip the interp
    if aLoop or ((aStep mod Byte(A.Count)) < A.Count-1) then
      Result := Result + EnsureRange(Floor(INTERP_LEVEL*aStepFrac), 0, INTERP_LEVEL-1);
  end
  else
  begin
    Result := A.Step[aStep mod Byte(A.Count) +1]+1;
  end;
end;


function GetCarryInterpSpriteOffset(aWare: TKMWareType; aDir: TKMDirection): Integer;
const
  CARRY_INTERP_LOOKUP: array[TKMWareType, TKMDirection] of Integer = (
  (-1,-1,-1,-1,-1,-1,-1,-1,-1), // wtNone
  (-1,75820,75884,75948,76012,76076,76140,76204,76268), // wtTrunk
  (-1,76332,76396,76460,76524,76588,76652,76716,76780), // wtStone
  (-1,76844,76908,76972,77036,77100,77164,77228,77292), // wtWood
  (-1,77356,77420,77484,77548,77612,77676,77740,77804), // wtIronOre
  (-1,77868,77932,77996,78060,78124,78188,78252,78316), // wtGoldOre
  (-1,78380,78444,78508,78572,78636,78700,78764,78828), // wtCoal
  (-1,78892,78956,79020,79084,79148,79212,79276,79340), // wtSteel
  (-1,79404,79468,79532,79596,79660,79724,79788,79852), // wtGold
  (-1,79916,79980,80044,80108,80172,80236,80300,80364), // wtWine
  (-1,80428,80492,80556,80620,80684,80748,80812,80876), // wtCorn
  (-1,80940,81004,81068,81132,81196,81260,81324,81388), // wtBread
  (-1,81452,81516,81580,81644,81708,81772,81836,81900), // wtFlour
  (-1,81964,82028,82092,82156,82220,82284,82348,82412), // wtLeather
  (-1,82476,82540,82604,82668,82732,82796,82860,82924), // wtSausages
  (-1,82988,83052,83116,83180,83244,83308,83372,83436), // wtPig
  (-1,83500,83564,83628,83692,83756,83820,83884,83948), // wtSkin
  (-1,84012,84076,84140,84204,84268,84332,84396,84460), // wtShield
  (-1,84524,84588,84652,84716,84780,84844,84908,84972), // wtMetalShield
  (-1,85036,85100,85164,85228,85292,85356,85420,85484), // wtArmor
  (-1,85548,85612,85676,85740,85804,85868,85932,85996), // wtMetalArmor
  (-1,86060,86124,86188,86252,86316,86380,86444,86508), // wtAxe
  (-1,86572,86636,86700,86764,86828,86892,86956,87020), // wtSword
  (-1,87084,87148,87212,87276,87340,87404,87468,87532), // wtPike
  (-1,87596,87660,87724,87788,87852,87916,87980,88044), // wtHallebard
  (-1,88108,88172,88236,88300,88364,88428,88492,88556), // wtBow
  (-1,88620,88684,88748,88812,88876,88940,89004,89068), // wtArbalet
  (-1,89132,89196,89260,89324,89388,89452,89516,89580), // wtHorse
  (-1,89644,89708,89772,89836,89900,89964,90028,90092), // wtFish
  (-1,-1,-1,-1,-1,-1,-1,-1,-1), // wtAll
  (-1,-1,-1,-1,-1,-1,-1,-1,-1), // wtWarfare
  (-1,-1,-1,-1,-1,-1,-1,-1,-1) // wtFood
);
begin
  if INTERPOLATED_ANIMS then
  begin
    Result := CARRY_INTERP_LOOKUP[aWare, aDir];
  end
  else
    Result := -1;
end;


function GetUnitInterpSpriteOffset(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection): Integer;
const
  ACTION_INTERP_LOOKUP: array[TKMUnitType, TKMUnitActionType, TKMDirection] of Integer = (
  ( // utNone
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utAny
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSerf
    (-1,9300,9364,9428,9492,9556,9620,9684,9748), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,10316,10556,10796,10556,11036,10556,11276,10556), // uaEat
    (-1,11516,11580,11644,11708,11772,11836,11900,11964), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWoodcutter
    (-1,12028,12092,12156,12220,12284,12348,12412,12476), // uaWalk
    (-1,12540,12604,-1,12684,-1,12764,-1,12844), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,12924,13164,13404,13164,13644,13164,13884,13164), // uaEat
    (-1,14124,14188,14252,14316,14380,-1,14444,14508), // uaWalkArm
    (-1,14572,14636,14700,14764,14828,14892,14956,15020), // uaWalkTool
    (-1,15084,15148,15212,15276,15340,15404,15468,15532), // uaWalkBooty
    (-1,15596,15660,15724,15788,15852,15916,15980,16044), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utMiner
    (-1,16108,16172,16236,16300,16364,16428,16492,16556), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,16620,16860,17100,16860,17340,16860,17580,16860), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utAnimalBreeder
    (-1,17820,17884,17948,18012,18076,18140,18204,18268), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,18332,18572,18812,18572,19052,18572,19292,18572), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utFarmer
    (-1,19532,19596,19660,19724,19788,19852,19916,19980), // uaWalk
    (-1,20044,20172,20172,20300,20300,20428,20428,20044), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,20556,20556,20636,20636,20716,20716,20796,20796), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,20876,21116,21356,21116,21596,21116,21836,21116), // uaEat
    (-1,22076,-1,22140,22204,22268,22332,22396,22460), // uaWalkArm
    (-1,22524,22588,22652,22716,22780,22844,22908,22972), // uaWalkTool
    (-1,23036,23100,23164,23228,23292,23356,23420,23484), // uaWalkBooty
    (-1,23548,23612,23676,23740,23804,23868,23932,23996), // uaWalkTool2
    (-1,24060,24124,24188,24252,24316,24380,24444,24508), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utLamberjack
    (-1,24572,24636,24700,24764,24828,24892,24956,25020), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,25084,25324,25564,25324,25804,25324,26044,25324), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utBaker
    (-1,26284,26348,26412,26476,26540,26604,26668,26732), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,26796,27036,27276,27036,27516,27036,27756,27036), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utButcher
    (-1,27996,28060,28124,28188,28252,28316,28380,28444), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,28508,28748,28988,28748,29228,28748,29468,28748), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utFisher
    (-1,29708,29772,29836,29900,29964,30028,30092,30156), // uaWalk
    (-1,30220,30340,30460,30580,30700,30820,30940,31060), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,31180,31284,31388,31492,31596,31700,31804,31908), // uaWork1
    (-1,32012,32252,32492,32732,32972,33212,33452,33692), // uaWork2
    (-1,33932,34052,34172,34292,34412,34532,34652,34772), // uaWorkEnd
    (-1,34892,35132,35372,35132,35612,35132,35852,35132), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,36092,36156,36220,36284,36348,36412,36476,36540), // uaWalkTool
    (-1,36604,36668,36732,36796,36860,36924,36988,37052), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWorker
    (-1,37116,37180,37244,37308,37372,37436,37500,37564), // uaWalk
    (-1,37628,37628,37716,37716,37804,37804,37892,37892), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,37980,37980,38076,38076,38172,38172,38268,38268), // uaWork1
    (-1,38364,38364,38452,38452,38540,38540,38628,38628), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,38716,38956,39196,38956,39436,38956,39676,38956), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utStoneCutter
    (-1,39916,39980,40044,40108,40172,40236,40300,40364), // uaWalk
    (-1,40428,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,40508,40748,40988,40748,41228,40748,41468,40748), // uaEat
    (-1,41708,41772,41836,41900,41964,42028,42092,42156), // uaWalkArm
    (-1,42220,42284,42348,42412,42476,42540,42604,42668), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSmith
    (-1,42732,42796,42860,42924,42988,43052,43116,43180), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,43244,43484,43724,43484,43964,43484,44204,43484), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utMetallurgist
    (-1,44444,44508,44572,44636,44700,44764,44828,44892), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,44956,45196,45436,45196,45676,45196,45916,45196), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utRecruit
    (-1,46156,46220,46284,46348,46412,46476,46540,46604), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,46668,46668,46668,46668,46668,46668,46668,46668), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,46708,46948,47188,46948,47428,46948,47668,46948), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utMilitia
    (-1,47908,47972,48036,48100,48164,48228,48292,48356), // uaWalk
    (-1,48420,48516,48612,48708,48804,48900,48996,49092), // uaWork
    (-1,49188,49252,49316,49380,49444,49508,49572,49636), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,49700,49764,49828,49892,49700,49764,49828,49892), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utAxeFighter
    (-1,49956,50020,50084,50148,50212,50276,50340,50404), // uaWalk
    (-1,50468,50564,50660,50756,50852,50948,51044,51140), // uaWork
    (-1,51236,51300,51364,51428,51492,51556,51620,51684), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,49700,49764,49828,49892,49700,49764,49828,49892), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSwordsman
    (-1,51748,51812,51876,51940,52004,52068,52132,52196), // uaWalk
    (-1,52260,52356,52452,52548,52644,52740,52836,52932), // uaWork
    (-1,53028,53092,53156,53220,53284,53348,53412,53476), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,49700,49764,49828,49892,49700,49764,49828,49892), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utBowman
    (-1,53540,53604,53668,53732,53796,53860,53924,53988), // uaWalk
    (-1,54052,54196,54340,54484,54628,54772,54916,55060), // uaWork
    (-1,55204,55244,55284,55324,55364,55404,55444,55484), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,55524,55588,55652,55716,55524,55588,55652,55716), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utArbaletman
    (-1,55780,55844,55908,55972,56036,56100,56164,56228), // uaWalk
    (-1,56292,56516,56740,56964,57188,57412,57636,57860), // uaWork
    (-1,58084,58124,58164,58204,58244,58284,58324,58364), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,55524,55588,55652,55716,55524,55588,55652,55716), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utPikeman
    (-1,58404,58468,58532,58596,58660,58724,58788,58852), // uaWalk
    (-1,58916,59012,59108,59204,59300,59396,59492,59588), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,59684,59748,59812,59876,59684,59748,59812,59876), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utHallebardman
    (-1,59940,60004,60068,60132,60196,60260,60324,60388), // uaWalk
    (-1,60452,60548,60644,60740,60836,60932,61028,61124), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,59684,59748,59812,59876,59684,59748,59812,59876), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utHorseScout
    (-1,61220,61268,61316,61364,61412,61460,61508,61556), // uaWalk
    (-1,61604,61700,61796,61892,61988,62084,62180,62276), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,62372,62372,62524,62524,62652,62652,62788,62788), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,62916,62980,63044,63108,62916,62980,63044,63108), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utCavalry
    (-1,63172,63220,63268,63316,63364,63412,63460,63508), // uaWalk
    (-1,63556,63652,63748,63844,63940,64036,64132,64228), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,62372,62372,62524,62524,62652,62652,62788,62788), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,62916,62980,63044,63108,62916,62980,63044,63108), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utBarbarian
    (-1,64324,64388,64452,64516,64580,64644,64708,64772), // uaWalk
    (-1,64836,64932,65028,65124,65220,65316,65412,65508), // uaWork
    (-1,65604,65668,65732,65796,65860,65924,65988,66052), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,49700,49764,49828,49892,49700,49764,49828,49892), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utPeasant
    (-1,66116,66180,66244,66308,66372,66436,66500,66564), // uaWalk
    (-1,66628,66724,66820,66916,67012,67108,67204,67300), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,59684,59748,59812,59876,59684,59748,59812,59876), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSlingshot
    (-1,67396,67460,67524,67588,67652,67716,67780,67844), // uaWalk
    (-1,67908,68148,68388,68628,68868,69108,69348,69588), // uaWork
    (-1,69828,69828,69828,69828,69828,69828,69828,69828), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,55524,55588,55652,55716,55524,55588,55652,55716), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utMetalBarbarian
    (-1,69868,69932,69996,70060,70124,70188,70252,70316), // uaWalk
    (-1,70380,70476,70572,70668,70764,70860,70956,71052), // uaWork
    (-1,71148,71212,71276,71340,71404,71468,71532,71596), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,49700,49764,49828,49892,49700,49764,49828,49892), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utHorseman
    (-1,71660,71708,71756,71804,71852,71900,71948,71996), // uaWalk
    (-1,72044,72140,72236,72332,72428,72524,72620,72716), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,62372,62372,62524,62524,62652,62652,62788,62788), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,62916,62980,63044,63108,62916,62980,63044,63108), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWolf
    (-1,72812,72876,72940,73004,73068,73132,73196,73260), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utFish
    (-1,73324,73372,73420,73468,73516,73564,73612,73660), // uaWalk
    (-1,73708,73756,73804,73852,73900,73948,73996,74044), // uaWork
    (-1,74092,74140,74188,74236,74284,74332,74380,74428), // uaSpec
    (-1,74476,74524,74572,74620,74668,74716,74764,74812), // uaDie
    (-1,74860,74908,74956,75004,75052,75100,75148,75196), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWatersnake
    (-1,75244,75292,75340,75388,75436,75484,75532,75580), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSeastar
    (-1,75628,75628,75628,75628,75628,75628,75628,75628), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utCrab
    (-1,75692,75692,75692,75692,75692,75692,75692,75692), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWaterflower
    (-1,75724,75724,75724,75724,75724,75724,75724,75724), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWaterleaf
    (-1,75772,75772,75772,75772,75772,75772,75772,75772), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utDuck
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  )
);
begin
  if INTERPOLATED_ANIMS then
  begin
    Result := ACTION_INTERP_LOOKUP[aUnit, aAct, aDir];
  end
  else
    Result := -1;
end;


function TRenderPool.GetUnitAnimSprite(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection;
                                       aStep: Integer; aStepFrac: Single): Integer;
var
  A: TKMAnimLoop;
  InterpOffset: Integer;
begin
  A := gRes.Units[aUnit].UnitAnim[aAct, aDir];
  InterpOffset := GetUnitInterpSpriteOffset(aUnit, aAct, aDir);

  //While in development disable interpolation if the sprite is missing
  if (InterpOffset >= 1) and ((InterpOffset >= fRXData[rxUnits].Count) or (fRXData[rxUnits].Size[InterpOffset].X = 0)) then
    InterpOffset := -1;

  if InterpOffset >= 0 then
  begin
    Result := InterpOffset
      + INTERP_LEVEL*(aStep mod Byte(A.Count))
      + EnsureRange(Floor(INTERP_LEVEL*aStepFrac), 0, INTERP_LEVEL-1);
  end
  else
  begin
    Result := A.Step[aStep mod Byte(A.Count) + 1] + 1;
  end;
end;


function TRenderPool.GetUnitAnimSpriteByPercent(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection;
                                                aPercent: Single): Integer;
var
  count: Integer;
  fracStep: Single;
begin
  count := gRes.Units[aUnit].UnitAnim[aAct, aDir].Count;
  fracStep := Min(aPercent, 1.0) * (count-1);
  Result := GetUnitAnimSprite(aUnit, aAct, aDir, Trunc(fracStep), Frac(fracStep));
end;


procedure TRenderPool.ReInit;
begin
  if Self = nil then Exit;

  fRenderDebug.ReInit;
end;


procedure TRenderPool.SetRotation(aH, aP, aB: Integer);
begin
  rHeading := aH;
  rPitch   := aP;
  rBank    := aB;
end;


procedure TRenderPool.ApplyTransform;
var
  viewportPosRound: TKMPointF;
begin
  //Need to round the viewport position so we only translate by whole pixels
  viewportPosRound := RoundToTilePixel(fViewport.Position, 1.0);

  glLoadIdentity; // Reset The View

  //Use integer division so we don't translate by half a pixel if clip is odd
  glTranslatef(fViewport.ViewportClip.X div 2, fViewport.ViewportClip.Y div 2, 0);

  glScalef(fViewport.Zoom*CELL_SIZE_PX, fViewport.Zoom*CELL_SIZE_PX, 1 / 256);

  glTranslatef(-viewportPosRound.X + gGame.ActiveInterface.ToolbarWidth/CELL_SIZE_PX/fViewport.Zoom, -viewportPosRound.Y, 0);

  if RENDER_3D then
  begin
    fRender.SetRenderMode(rm3D);

    glkScale(-CELL_SIZE_PX/14);
    glRotatef(rHeading,1,0,0);
    glRotatef(rPitch  ,0,1,0);
    glRotatef(rBank   ,0,0,1);
    glTranslatef(-viewportPosRound.X + gGame.ActiveInterface.ToolBarWidth/CELL_SIZE_PX/fViewport.Zoom, -viewportPosRound.Y - 8, 10);
    glScalef(fViewport.Zoom, fViewport.Zoom, 1);
  end;

  glRotatef(rHeading,1,0,0);
  glRotatef(rPitch  ,0,1,0);
  glRotatef(rBank   ,0,0,1);
  glTranslatef(0, 0, -viewportPosRound.Y);
end;


procedure TRenderPool.SetDefaultRenderParams;
begin
  glLineWidth(fViewport.Zoom * 2);
  glPointSize(fViewport.Zoom * 5);
  glEnable(GL_LINE_SMOOTH);
end;


// Render:
// 1. Sets viewport
// 2. Renders terrain
// 3. Polls Game objects to add themselves to RenderList through Add** methods
// 4. Renders cursor highlights
procedure TRenderPool.Render(aTickLag: Single);
var
  clipRect: TKMRect;
begin
  if fRender.Blind then Exit;

  Inc(fCounter);

  ApplyTransform;

  glPushAttrib(GL_LINE_BIT or GL_POINT_BIT);
    SetDefaultRenderParams;

    // Render only within visible area
    clipRect := fViewport.GetClip;

    fRenderDebug.ClipRect := clipRect;

    // Collect players plans for terrain layer
    CollectPlans(clipRect);

    // With depth test we can render all terrain tiles and then apply light/shadow without worrying about
    // foothills shadows going over mountain tops. Each tile strip is rendered an next Z plane.
    // Means that Z-test on gpu will take care of clipping the foothill shadows
    glEnable(GL_DEPTH_TEST);

    // Everything flat of terrain
    {$IFDEF PERFLOG}
    gPerfLogs.SectionEnter(psFrameTerrain);
    {$ENDIF}
    fRenderTerrain.ClipRect := clipRect;
    fRenderTerrain.RenderBase(gTerrain.AnimStep, gMySpectator.FogOfWar);

    // Disable depth test //and write to depth buffer,
    // so that terrain shadows could be applied seamlessly ontop
    glDisable(GL_DEPTH_TEST);

    if mlOverlays in gGameParams.VisibleLayers then
    begin
      fRenderTerrain.RenderFences(gMySpectator.FogOfWar);
      fRenderTerrain.RenderPlayerPlans(fFieldsList, fHousePlansList);
    end;

    if mlMiningRadius in gGameParams.VisibleLayers then
      fRenderDebug.PaintMiningRadius;

    {$IFDEF PERFLOG}
    gPerfLogs.SectionLeave(psFrameTerrain);
    {$ENDIF}

    // House highlight, debug display
    RenderBackgroundUI(clipRect);

    // Sprites are added by Terrain/Players/Projectiles, then sorted by position
    fRenderList.Clear;
    CollectTerrainObjects(clipRect, gTerrain.AnimStep);

    PaintFlagPoints(True);

    gHands.Paint(clipRect, aTickLag); // Units and houses
    gProjectiles.Paint(aTickLag);

    if gGame.GamePlayInterface <> nil then
      gGame.GamePlayInterface.Alerts.Paint(0);

    fRenderList.SortRenderList;
    fRenderList.Render;

    if mlDefencesAll in gGameParams.VisibleLayers then
      fRenderDebug.PaintDefences;

    fRenderTerrain.RenderFOW(gMySpectator.FogOfWar);

    // Alerts/rally second pass is rendered after FOW
    PaintFlagPoints(False);
    if gGame.GamePlayInterface <> nil then
      gGame.GamePlayInterface.Alerts.Paint(1);

    // Cursor overlays (including blue-wire plans), go on top of everything
    RenderForegroundUI;

  glPopAttrib;
end;


procedure TRenderPool.RenderBackgroundUI(const aRect: TKMRect);

  procedure HighlightUnit(U: TKMUnit; aCol: Cardinal); inline;
  begin
    gRenderAux.CircleOnTerrain(U.PositionF.X - 0.5 + U.GetSlide(axX),
                               U.PositionF.Y - 0.5 + U.GetSlide(axY),
                               0.35, aCol, icCyan);
  end;

  procedure HighlightEntity(aEntityH: TKMHighlightEntity);
  var
    I: Integer;
    G: TKMUnitGroup;
    col: Cardinal;
  begin
    if aEntityH.Entity = nil then Exit;
    
    case aEntityH.Entity.EntityType of
      etHouse:  RenderHouseOutline(TKMHouseSketch(aEntityH.Entity), aEntityH.Color); //fPositionF.X - 0.5 + GetSlide(axX), fPositionF.Y - 0.5 + GetSlide(axY), 0.35
      etUnit:   HighlightUnit(TKMUnit(aEntityH.Entity), GetRandomColorWSeed(aEntityH.Entity.UID));
      etGroup:  begin
                  G := TKMUnitGroup(aEntityH.Entity);
                  col := GetRandomColorWSeed(G.UID);
                  for I := 0 to G.Count - 1 do
                    HighlightUnit(G.Members[I], col);
                end;
    end;
  end;


var
  I, K: Integer;
begin
  //Reset Texture, just in case we forgot to do it inside some method
  TRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)

  HighlightEntity(gMySpectator.HighlightEntity);
  HighlightEntity(gMySpectator.HighlightDebug);
  HighlightEntity(gMySpectator.HighlightDebug2);
  HighlightEntity(gMySpectator.HighlightDebug3);

  if gGameParams.IsMapEditor then
    gGame.MapEditor.Paint(plTerrain, aRect);

  if gAIFields <> nil then
    gAIFields.Paint(aRect);

  if SHOW_WALK_CONNECT then
  begin
    glPushAttrib(GL_DEPTH_BUFFER_BIT);
      glDisable(GL_DEPTH_TEST);

      for I := aRect.Top to aRect.Bottom do
      for K := aRect.Left to aRect.Right do
        gRenderAux.Text(K, I, IntToStr(gTerrain.Land^[I,K].WalkConnect[wcWalk]), $FFFFFFFF);

    glPopAttrib;
  end;

  if SHOW_TERRAIN_WIRES then
    gRenderAux.Wires(aRect);

  if SHOW_TERRAIN_PASS <> 0 then
    gRenderAux.Passability(aRect, SHOW_TERRAIN_PASS);

  if SHOW_TERRAIN_IDS then
    gRenderAux.TileTerrainIDs(aRect);

  if SHOW_TERRAIN_KINDS then
    gRenderAux.TileTerrainKinds(aRect);

  if SHOW_TERRAIN_OVERLAYS then
    gRenderAux.TileTerrainOverlays(aRect);

  if SHOW_TERRAIN_HEIGHT then
    gRenderAux.TileTerrainHeight(aRect);

  if SHOW_JAM_METER then
    gRenderAux.TileTerrainJamMeter(aRect);

  if SHOW_TILE_OBJECT_ID then
    gRenderAux.TileTerrainTileObjectID(aRect);

  if SHOW_TILES_OWNER then
    RenderTileOwnerLayer(aRect);

  if SHOW_TREE_AGE then
    gRenderAux.TileTerrainTreeAge(aRect);

  if SHOW_FIELD_AGE then
    gRenderAux.TileTerrainFieldAge(aRect);

  if SHOW_TILE_LOCK then
    gRenderAux.TileTerrainTileLock(aRect);

  if SHOW_TILE_UNIT then
    gRenderAux.TileTerrainTileUnit(aRect);

  if SHOW_VERTEX_UNIT then
    gRenderAux.TileTerrainVertexUnit(aRect);

  if SHOW_TERRAIN_TILES_GRID then
    RenderTilesGrid(aRect);

  if SHOW_UNIT_MOVEMENT then
    gRenderAux.UnitMoves(aRect);
end;


procedure TRenderPool.CollectTerrainObjects(const aRect: TKMRect; aAnimStep: Cardinal);
var
  I, K: Integer;
begin
  if not (mlObjects in gGameParams.VisibleLayers) then Exit;

  if gGameParams.IsMapEditor then
    gGame.MapEditor.Paint(plObjects, aRect);

  with gTerrain do
    for I := aRect.Top to aRect.Bottom do
      for K := aRect.Left to aRect.Right do
      begin
        if (Land^[I, K].Obj <> OBJ_NONE) then
          RenderMapElement(Land^[I, K].Obj, AnimStep, K, I);
      end;

  // Falling trees are in a separate list
  with gTerrain do
    for I := 0 to FallingTrees.Count - 1 do
    begin
      RenderMapElement1(FallingTrees.Tag[I], aAnimStep - FallingTrees.Tag2[I], FallingTrees[I].X, FallingTrees[I].Y, False);
      Assert(AnimStep - FallingTrees.Tag2[I] <= 100, 'Falling tree overrun?');
    end;

  // Tablets on house plans, for self and allies
  fTabletsList.Clear;
  if gGameParams.IsReplayOrSpectate then
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        gHands[I].GetPlansTablets(fTabletsList, aRect)
    else
      gHands[gMySpectator.FOWIndex].GetPlansTablets(fTabletsList, aRect)
  else
    gMySpectator.Hand.GetPlansTablets(fTabletsList, aRect);

  for I := 0 to fTabletsList.Count - 1 do
    AddHouseTablet(TKMHouseType(fTabletsList.Tag[I]), fTabletsList[I]);
end;


procedure TRenderPool.PaintFlagPoint(const aHouseEntrance, aFlagPoint: TKMPoint; aColor: Cardinal; aTexId: Integer; aFirstPass: Boolean;
                                     aDoImmediateRender: Boolean = False);

  procedure RenderLineToPoint(const aP: TKMPointF);
  begin
    gRenderAux.LineOnTerrain(aHouseEntrance.X - 0.5, aHouseEntrance.Y - 0.5, aP.X, aP.Y, aColor, $F0F0, False);
  end;

var
  P: TKMPointF;
begin
  P := KMPointF(aFlagPoint.X - 0.5, aFlagPoint.Y - 0.5);
  if not aDoImmediateRender then
  begin
    if aFirstPass then
    begin
      AddAlert(P, aTexId, aColor);
      RenderLineToPoint(P);
    end
    else
    if gMySpectator.FogOfWar.CheckRevelation(P) < FOG_OF_WAR_MAX then
      RenderSpriteOnTerrain(P, aTexId, aColor, True); //Force to paint, even under FOW
  end
  else begin
    RenderSpriteOnTile(aFlagPoint, aTexId, aColor);
    RenderLineToPoint(P);
  end;
end;


procedure TRenderPool.PaintFlagPoints(aFirstPass: Boolean);
var
  HWFP: TKMHouseWFlagPoint;
begin
  //Skip render if no house with flagpoint is chosen
  if  not (gMySpectator.Selected is TKMHouseWFlagPoint) then
    Exit;

  if gMySpectator.Selected is TKMHouseWFlagPoint then
  begin
    HWFP := TKMHouseWFlagPoint(gMySpectator.Selected);
    if HWFP.IsFlagPointSet then
      PaintFlagPoint(HWFP.Entrance, HWFP.FlagPoint, gHands[HWFP.Owner].GameFlagColor, HWFP.FlagPointTexId, aFirstPass);
  end;
end;


procedure TRenderPool.RenderTile(aTerrainId: Word; pX, pY, Rot: Integer);
begin
  fRenderTerrain.RenderTile(aTerrainId, pX, pY, Rot);
end;


procedure TRenderPool.RenderMapElement(aIndex: Word; AnimStep,pX,pY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
begin
  if (gMySpectator.FogOfWar.CheckVerticeRenderRev(pX,pY) <= FOG_OF_WAR_MIN) then Exit;// Do not render tiles fully covered by FOW
  // Render either normal object or quad depending on what it is
  if gMapElements[aIndex].WineOrCorn then
    RenderMapElement4(aIndex,AnimStep,pX,pY,(aIndex in [54..57]),DoImmediateRender,Deleting) // 54..57 are grapes, all others are doubles
  else
    RenderMapElement1(aIndex,AnimStep,pX,pY,True,DoImmediateRender,Deleting);
end;


procedure TRenderPool.RenderMapElement1(aIndex: Word; AnimStep: Cardinal; LocX,LocY: Integer; aLoopAnim: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;
  pX, pY: Integer;
  cornerX, cornerY: Single;
  gX, gY: Single;
  Id, Id0: Integer;
  FOW: Byte;
  A: TKMAnimLoop;
begin
  if (gMySpectator.FogOfWar.CheckVerticeRenderRev(LocX,LocY) <= FOG_OF_WAR_MIN) then Exit;

  if aIndex = OBJ_BLOCK then
  begin
    // Invisible wall
    // Render as a red outline in map editor mode
    if gGameParams.IsMapEditor then
    begin
      gRenderAux.Quad(LocX, LocY, $600000FF);
      RenderWireTile(KMPoint(LocX, LocY), $800000FF);
    end;
  end
  else
  begin
    if gMapElements[aIndex].Anim.Count = 0 then Exit;

    if gGameParams.DynamicFOW then
    begin
      FOW := gMySpectator.FogOfWar.CheckTileRevelation(LocX,LocY);
      if FOW <= 128 then AnimStep := 0; // Stop animation
    end;
    A := gMapElements[aIndex].Anim;
    Id := GetTreeAnimSprite(aIndex, AnimStep, gGameParams.TickFrac, aLoopAnim);
    Id0 := A.Step[1] + 1;
    if Id <= 0 then exit;

    R := fRXData[rxTrees];
    pX := LocX - 1;
    pY := LocY - 1;
    gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;
    cornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
    cornerY := pY - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;
    if DoImmediateRender then
      RenderSprite(rxTrees, Id, cornerX, cornerY, $FFFFFFFF, Deleting, DELETE_COLOR)
    else
      fRenderList.AddSpriteG(rxTrees, Id, 0, cornerX, cornerY, gX, gY);
  end;
end;


// 4 objects packed on 1 tile for Corn and Grapes
procedure TRenderPool.RenderMapElement4(aIndex: Word; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
var
  R: TRXData;

  procedure AddSpriteBy(aAnimStep: Integer; pX,pY: Single);
  var
    Id, Id0: Integer;
    CornerX, CornerY, gX, gY: Single;
    A: TKMAnimLoop;
  begin
    A := gMapElements[aIndex].Anim;
    Id := GetTreeAnimSprite(aIndex, AnimStep, gGameParams.TickFrac, True);
    Id0 := A.Step[1] + 1;

    gX := pX + (R.Pivot[Id0].X + R.Size[Id0].X/2) / CELL_SIZE_PX;
    gY := pY + (R.Pivot[Id0].Y + R.Size[Id0].Y) / CELL_SIZE_PX;
    CornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
    CornerY := pY - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX;

    if DoImmediateRender then
      RenderSprite(rxTrees, Id, CornerX, CornerY, $FFFFFFFF, Deleting, DELETE_COLOR)
    else
      fRenderList.AddSpriteG(rxTrees, Id, 0, CornerX, CornerY, gX, gY);
  end;

var
  FOW: Byte;
begin
  if gGameParams.DynamicFOW then
  begin
    FOW := gMySpectator.FogOfWar.CheckTileRevelation(pX, pY);
    if FOW <= 128 then AnimStep := 0; // Stop animation
  end;

  R := fRXData[rxTrees];
  if IsDouble then
  begin
    AddSpriteBy(AnimStep  , pX - 0.75, pY - 0.6);
    AddSpriteBy(AnimStep+1, pX - 0.25, pY - 0.6);
  end
  else
  begin
    AddSpriteBy(AnimStep  , pX - 0.75, pY - 0.75);
    AddSpriteBy(AnimStep+1, pX - 0.25, pY - 0.75);
    AddSpriteBy(AnimStep+1, pX - 0.75, pY - 0.25);
    AddSpriteBy(AnimStep  , pX - 0.25, pY - 0.25);
  end;
end;


// Render alert
procedure TRenderPool.AddAlert(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4);
var
  cornerX, cornerY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];

  cornerX := aLoc.X + R.Pivot[aId].X / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(aLoc).Y + R.Pivot[aId].Y / CELL_SIZE_PX;

  fRenderList.AddSpriteG(rxGui, aId, 0, cornerX, cornerY, aLoc.X, aLoc.Y, aFlagColor);
end;


// Render house WIP tablet
procedure TRenderPool.AddHouseTablet(aHouse: TKMHouseType; const Loc: TKMPoint);
var
  Id: Integer;
  cornerX, cornerY, gX, gY: Single;
  R: TRXData;
begin
  R := fRXData[rxGui];
  Id := gRes.Houses[aHouse].TabletIcon;

  gX := Loc.X + (R.Pivot[Id].X + R.Size[Id].X / 2) / CELL_SIZE_PX - 0.5;
  gY := Loc.Y + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 0.45;
  cornerX := Loc.X + R.Pivot[Id].X / CELL_SIZE_PX - 0.25;
  cornerY := Loc.Y - gTerrain.RenderHeightAt(gX, gY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 0.55;
  fRenderList.AddSpriteG(rxGui, Id, 0, cornerX, cornerY, gX, gY);
end;


// Render house build supply
procedure TRenderPool.AddHouseBuildSupply(aHouse: TKMHouseType; const Loc: TKMPoint; Wood, Stone: Byte);
var
  rx: TRXData;
  id: Integer;
  supply: THouseBuildSupply;
  cornerX, cornerY: Single;
begin
  rx := fRXData[rxHouses];
  supply := gRes.Houses[aHouse].BuildSupply;

  if Wood <> 0 then
  begin
    id := 260 + Wood - 1;
    cornerX := Loc.X + supply[1, Wood].MoveX / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (supply[1, Wood].MoveY + rx.Size[id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY);
  end;

  if Stone <> 0 then
  begin
    id := 267 + Stone - 1;
    cornerX := Loc.X + supply[2, Stone].MoveX / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (supply[2, Stone].MoveY + rx.Size[id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, id, cornerX, cornerY);
  end;
end;


procedure TRenderPool.AddWholeHouse(H: TKMHouse; FlagColor: Cardinal; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
begin
  if H <> nil then
  begin
    AddHouse(H.HouseType, H.Position, 1, 1, 0, DoImmediateRender, DoHighlight, HighlightColor);
    AddHouseSupply(H.HouseType, H.Position, H.ResourceInArray, H.ResourceOutArray, H.ResourceOutPoolArray, DoImmediateRender, DoHighlight, HighlightColor);
    if H.CurrentAction <> nil then
      gRenderPool.AddHouseWork(H.HouseType, H.Position, H.CurrentAction.SubAction, H.WorkAnimStep, FlagColor, DoImmediateRender, DoHighlight, HighlightColor);
  end;
end;


// Render house in wood
procedure TRenderPool.AddHouse(aHouse: TKMHouseType; const aLoc: TKMPoint; aWoodStep, aStoneStep, aSnowStep: Single; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
var
  R: TRXData;
  picWood, picStone, picSnow: Integer;
  groundWood, groundStone, gX, gY: Single;

  function CornerX(aPic: Integer): Single;
  begin
    Result := aLoc.X + R.Pivot[aPic].X / CELL_SIZE_PX - 1;
  end;

  function CornerY(aPic: Integer): Single;
  begin
    Result := aLoc.Y + (R.Pivot[aPic].Y + R.Size[aPic].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land^[aLoc.Y + 1, aLoc.X].RenderHeight / CELL_HEIGHT_DIV;
  end;

begin
  // We cannot skip when WoodStep = 0 because building supply is rendered as a child.
  // Instead RenderSpriteAlphaTest will skip rendering when WoodStep = 0

  R := fRXData[rxHouses];

  picWood := gRes.Houses[aHouse].WoodPic + 1;
  picStone := gRes.Houses[aHouse].StonePic + 1;
  picSnow := gRes.Houses[aHouse].SnowPic + 1;

  groundWood := R.Pivot[picWood].Y + R.Size[picWood].Y;
  groundStone := R.Pivot[picStone].Y + R.Size[picStone].Y;

  gX := aLoc.X + (R.Pivot[picWood].X + R.Size[picWood].X / 2) / CELL_SIZE_PX - 1;
  gY := aLoc.Y + Max(groundWood, groundStone) / CELL_SIZE_PX - 1.5;

  // If it's fully built we can render without alpha
  if (aWoodStep = 1) and (aStoneStep = 1) then
  begin
    // Snow only happens on fully built houses
    if gGameSettings.AllowSnowHouses
      and (aSnowStep > 0)
      and (picSnow <> 0) then
    begin
      // If snow is 100% we only need to render snow sprite
      if aSnowStep = 1 then
        fRenderList.AddSpriteG(rxHouses, picSnow, 0, CornerX(picSnow), CornerY(picSnow), gX, gY, $0)
      else
      begin
        // Render stone with snow blended on top using AlphaTest
        fRenderList.AddSpriteG(rxHouses, picStone, 0, CornerX(picStone), CornerY(picStone), gX, gY, $0);
        fRenderList.AddSpriteG(rxHouses, picSnow, 0, CornerX(picSnow), CornerY(picSnow), gX, gY, $0, aSnowStep);
      end;
    end
    else if DoImmediateRender then
      RenderSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone), $0, DoHighlight, HighlightColor)
    else
      fRenderList.AddSpriteG(rxHouses, picStone, 0, CornerX(picStone), CornerY(picStone), gX, gY, $0);
  end
  else
  begin
    // Wood part of the house (may be seen below Stone part before construction is complete, e.g. Sawmill)
    fRenderList.AddSpriteG(rxHouses, picWood, 0, CornerX(picWood), CornerY(picWood), gX, gY, $0, aWoodStep);
    if aStoneStep > 0 then
      fRenderList.AddSprite(rxHouses, picStone, CornerX(picStone), CornerY(picStone), $0, aStoneStep);
  end;
end;


procedure TRenderPool.AddHouseWork(aHouse: TKMHouseType; const Loc: TKMPoint; aActSet: TKMHouseActionSet; AnimStep: Cardinal; FlagColor: TColor4; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
var
  Id: Cardinal;
  AT: TKMHouseActionType;
  A: TKMAnimLoop;
  R: TRXData;
  cornerX, cornerY: Single;
begin
  if aActSet = [] then Exit;

  R := fRXData[rxHouses];

  //See if action is in set and render it
  for AT := Low(TKMHouseActionType) to High(TKMHouseActionType) do
  if AT in aActSet then
  begin
    A := gRes.Houses[aHouse].Anim[AT];
    if A.Count > 0 then
    begin
      Id := A.Step[AnimStep mod Byte(A.Count) + 1] + 1;
      cornerX := Loc.X + (R.Pivot[Id].X + A.MoveX) / CELL_SIZE_PX - 1;
      cornerY := Loc.Y + (R.Pivot[Id].Y + A.MoveY + R.Size[Id].Y) / CELL_SIZE_PX - 1
                       - gTerrain.Land^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;

      if DoImmediateRender then
        RenderSprite(rxHouses, Id, cornerX, cornerY, FlagColor, DoHighlight, HighlightColor)
      else
        fRenderList.AddSprite(rxHouses, Id, cornerX, cornerY, FlagColor);
    end;
  end;
end;


procedure TRenderPool.AddHouseSupply(aHouse: TKMHouseType; const Loc: TKMPoint; const R1, R2, R3: array of Byte;
                                     DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
var
  Id, I, K, I2, count: Integer;
  R: TRXData;

  procedure AddHouseSupplySprite(aId: Integer);
  var
    CornerX, CornerY: Single;
  begin
    if aId = 0 then Exit;

    CornerX := Loc.X + R.Pivot[aId].X / CELL_SIZE_PX - 1;
    CornerY := Loc.Y + (R.Pivot[aId].Y + R.Size[aId].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;
    if DoImmediateRender then
    begin
      RenderSprite(rxHouses, aId, CornerX, CornerY, $0, DoHighlight, HighlightColor)
    end else
      fRenderList.AddSprite(rxHouses, aId, CornerX, CornerY);
  end;

begin
  R := fRXData[rxHouses];

  for I := 1 to 4 do
  if (R1[I - 1]) > 0 then
  begin
    count := Min(R1[I - 1], MAX_WARES_IN_HOUSE);
    I2 := I;

    // Need to swap Coal and Steel for the ArmorSmithy
    // For some reason KaM stores these wares in swapped order, here we fix it (1 <-> 2)
    if (aHouse = htArmorSmithy) and (I in [1,2]) then
      I2 := 3-I;

    // Need to swap Timber and Leather for the ArmorWorkshop
    // For some reason KaM stores these wares in swapped order, here we fix it (1 <-> 2)
    if (aHouse = htArmorWorkshop) and (I in [1,2]) then
      I2 := 3-I;

    Id := gRes.Houses[aHouse].SupplyIn[I2, count] + 1;
    AddHouseSupplySprite(Id);
  end;

  if aHouse in HOUSE_WORKSHOP then
  begin
    for K := 0 to 19 do
      if R3[K] > 0 then
      begin
        I2 := R3[K];

        // Need to swap Shields and Armor for the ArmorWorkshop
        // For some reason KaM stores these wares in swapped order, here we fix it (1 <-> 2)
//        if (aHouse = htArmorWorkshop) and (I2 in [1,2]) then
//          I2 := 3-R3[K];

        Id := gRes.Houses[aHouse].SupplyOut[I2, K mod MAX_WARES_IN_HOUSE + 1] + 1;
        AddHouseSupplySprite(Id);
      end;
  end
  else
  begin
    for I := 1 to 4 do
      if R2[I - 1] > 0 then
      begin
        count := Min(R2[I - 1], MAX_WARES_IN_HOUSE);
        Id := gRes.Houses[aHouse].SupplyOut[I, count] + 1;
        AddHouseSupplySprite(Id);
      end;
  end;
end;


procedure TRenderPool.AddHouseMarketSupply(const Loc: TKMPoint; ResType: TKMWareType; ResCount: Word; AnimStep: Integer);
var
  I, Id: Integer;
  cornerX, cornerY: Single;
  R: TRXData;
begin
  if ResType = wtHorse then // Horses are a beast, BeastId is the count, age is 1
    for I := 1 to Min(ResCount, MARKET_WARES[ResType].Count) do // Render each beast
      AddHouseStableBeasts(htMarketplace, Loc, I, 1, AnimStep, rxHouses)
  else
  begin
    if MARKET_WARES[ResType].Count = 0 then Exit;
    Id := (MARKET_WARES[ResType].TexStart-1) + Min(ResCount, MARKET_WARES[ResType].Count);
    if Id = 0 then Exit;

    R := fRXData[rxHouses];
    cornerX := Loc.X + (R.Pivot[Id].X + MARKET_WARES_OFF_X) / CELL_SIZE_PX - 1;
    cornerY := Loc.Y + (R.Pivot[Id].Y + MARKET_WARES_OFF_Y + R.Size[Id].Y) / CELL_SIZE_PX - 1
                     - gTerrain.Land^[Loc.Y+1,Loc.X].RenderHeight / CELL_HEIGHT_DIV;
    fRenderList.AddSprite(rxHouses, Id, cornerX, cornerY);
  end;
end;


procedure TRenderPool.AddHouseStableBeasts(aHouse: TKMHouseType; const Loc: TKMPoint; BeastId,BeastAge,AnimStep: Integer; aRX: TRXType = rxHouses);
var
  cornerX, cornerY: Single;
  Id: Integer;
  R: TRXData;
  A: TKMAnimLoop;
begin
  R := fRXData[aRX];

  A := gRes.Houses.BeastAnim[aHouse,BeastId,BeastAge];

  Id := A.Step[AnimStep mod Byte(A.Count) + 1] + 1;
  cornerX := Loc.X + (A.MoveX + R.Pivot[Id].X) / CELL_SIZE_PX - 1;
  cornerY := Loc.Y + (A.MoveY + R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.Land^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;
  fRenderList.AddSprite(aRX, Id, cornerX, cornerY);
end;


// aRenderPos has gTerrain.HeightAt factored in already, aTilePos is on tile coordinates for Z ordering
procedure TRenderPool.AddProjectile(aProj: TKMProjectileType; const aRenderPos, aTilePos: TKMPointF; aDir: TKMDirection; aFlight: Single);
var
  FOW: Byte;
  id: Integer;
  R: TRXData;
  cornerX, cornerY: Single;
  ground: Single;
begin
  // We don't care about off-map arrows, but still we get TKMPoint error if X/Y gets negative
  if not gTerrain.TileInMapCoords(Round(aRenderPos.X), Round(aRenderPos.Y)) then Exit;

  if gGameParams.DynamicFOW then
  begin
    FOW := gMySpectator.FogOfWar.CheckRevelation(aRenderPos);
    if FOW <= 128 then Exit; // Don't render objects which are behind FOW
  end;

  case aProj of
    ptArrow:     id := GetUnitAnimSpriteByPercent(utBowman, uaSpec, aDir, aFlight);
    ptBolt:      id := GetUnitAnimSpriteByPercent(utArbaletman, uaSpec, aDir, aFlight);
    ptSlingRock: id := GetUnitAnimSpriteByPercent(utSlingshot, uaSpec, aDir, aFlight);
    ptTowerRock: id := GetUnitAnimSpriteByPercent(utRecruit, uaSpec, aDir, aFlight);
    else          id := 1; // Nothing?
  end;

  R := fRXData[rxUnits];

  cornerX := R.Pivot[id].X / CELL_SIZE_PX - 1;
  cornerY := (R.Pivot[id].Y + R.Size[id].Y) / CELL_SIZE_PX - 1;

  case aProj of
    ptArrow, ptBolt, ptSlingRock:  ground := aTilePos.Y + (0.5 - Abs(Min(aFlight, 1) - 0.5)) - 0.5;
    ptTowerRock:                     ground := aTilePos.Y + Min(aFlight, 1)/5 - 0.4;
    else                              ground := aTilePos.Y - 1; // Nothing?
  end;

  fRenderList.AddSpriteG(rxUnits, id, 0, aRenderPos.X + cornerX, aRenderPos.Y + cornerY, aTilePos.X - 1, ground);
end;


procedure TRenderPool.AddUnit(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; StepFrac: Single; pX,pY: Single; FlagColor: TColor4; NewInst: Boolean; DoImmediateRender: Boolean = False; DoHighlight: Boolean = False; HighlightColor: TColor4 = 0);
var
  cornerX, cornerY, ground: Single;
  id, id0: Integer;
  R: TRXData;
begin
  id := GetUnitAnimSprite(aUnit, aAct, aDir, StepId, StepFrac);
  id0 := GetUnitAnimSprite(aUnit, aAct, aDir, UNIT_STILL_FRAMES[aDir], 0.0);
  if id <= 0 then exit;
  R := fRXData[rxUnits];

  cornerX := pX + R.Pivot[id].X / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[id].Y + R.Size[id].Y) / CELL_SIZE_PX;
  ground := pY + (R.Pivot[id0].Y + R.Size[id0].Y) / CELL_SIZE_PX;

  if DoImmediateRender then
    RenderSprite(rxUnits, id, cornerX, cornerY, FlagColor, DoHighlight, HighlightColor)
  else
    if NewInst then
      fRenderList.AddSpriteG(rxUnits, id, aUID, cornerX, cornerY, pX, ground, FlagColor)
    else
      fRenderList.AddSprite(rxUnits, id, cornerX, cornerY, FlagColor);

  if SHOW_UNIT_MOVEMENT then
  if NewInst then
  begin
    gRenderAux.DotOnTerrain(pX, pY, FlagColor);
    gRenderAux.Dot(cornerX, cornerY, $FF000080);
  end;
end;


procedure TRenderPool.AddHouseEater(const Loc: TKMPoint; aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; OffX,OffY: Single; FlagColor: TColor4);
var
  cornerX, cornerY: Single;
  id: Integer;
  R: TRXData;
begin
  id := GetUnitAnimSprite(aUnit, aAct, aDir, StepId, gGameParams.TickFrac);
  if id <= 0 then exit;
  R := fRXData[rxUnits];

  // Eaters need to interpolate land height the same as the inn otherwise they are rendered at the wrong place
  cornerX := Loc.X + OffX + R.Pivot[id].X / CELL_SIZE_PX - 1;
  cornerY := Loc.Y + OffY + (R.Pivot[id].Y + R.Size[id].Y) / CELL_SIZE_PX - 1
                   - gTerrain.Land^[Loc.Y + 1, Loc.X].RenderHeight / CELL_HEIGHT_DIV;

  fRenderList.AddSprite(rxUnits, id, cornerX, cornerY, FlagColor);
end;


procedure TRenderPool.AddUnitCarry(aCarry: TKMWareType; aUID: Integer; aDir: TKMDirection; StepId: Integer; StepFrac: Single; pX,pY: Single);
var
  cornerX, cornerY: Single;
  id: Integer;
  A: TKMAnimLoop;
  R: TRXData;
  InterpOffset: Integer;
begin
  A := gRes.Units.SerfCarry[aCarry, aDir];
  InterpOffset := GetCarryInterpSpriteOffset(aCarry, aDir);

  //While in development disable interpolation if the sprite is missing
  if (InterpOffset >= 1) and ((InterpOffset >= fRXData[rxUnits].Count) or (fRXData[rxUnits].Size[InterpOffset].X = 0)) then
    InterpOffset := -1;

  if InterpOffset >= 0 then
  begin
    id := InterpOffset
      + INTERP_LEVEL*(StepId mod Byte(A.Count))
      + EnsureRange(Floor(INTERP_LEVEL*StepFrac), 0, INTERP_LEVEL-1);
  end
  else
  begin
    id := A.Step[StepId mod Byte(A.Count) + 1] + 1;
  end;

  if id <= 0 then Exit;
  R := fRXData[rxUnits];

  cornerX := pX + (R.Pivot[id].X + a.MoveX) / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[id].Y + R.Size[id].Y + a.MoveY) / CELL_SIZE_PX;
  fRenderList.AddSprite(rxUnits, id, cornerX, cornerY);
end;


procedure TRenderPool.AddUnitThought(aUnit: TKMUnitType; aAct: TKMUnitActionType;
                                     aDir: TKMDirection;
                                     Thought: TKMUnitThought; pX,pY: Single);
var
  Id, InterpOffset: Integer;
  cornerX, cornerY, ground: Single;
  R: TRXData;
  A: TKMAnimLoop;
  id0: Integer;
  AnimCount: Word;
begin
  if Thought = thNone then Exit;
  R := fRXData[rxUnits];

  // Unit position
  A := gRes.Units[aUnit].UnitAnim[aAct, aDir];
  id0 := A.Step[UNIT_STILL_FRAMES[aDir] mod Byte(A.Count) + 1] + 1;

  // Units feet
  ground := pY + (R.Pivot[id0].Y + R.Size[id0].Y) / CELL_SIZE_PX;
  // The thought should be slightly lower than the unit so it goes OVER warrior flags
  ground := ground + THOUGHT_X_OFFSET;

  InterpOffset := GetThoughtInterpSpriteOffset(Thought);

  // Thought bubbles are animated in reverse
  AnimCount := THOUGHT_BOUNDS[Thought, 2] - THOUGHT_BOUNDS[Thought, 1];
  if InterpOffset > 0 then
  begin
    Id := InterpOffset + (AnimCount-1)*INTERP_LEVEL
      - INTERP_LEVEL*(gGameParams.Tick mod AnimCount)
      + EnsureRange(Floor(INTERP_LEVEL*(1.0-gGameParams.TickFrac)), 0, INTERP_LEVEL-1);
  end
  else
  begin
    Id := THOUGHT_BOUNDS[Thought, 2] + 1 -
         (gGameParams.Tick mod AnimCount);
  end;

  cornerX := pX + R.Pivot[Id].X / CELL_SIZE_PX;
  cornerY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[Id].Y + R.Size[Id].Y) / CELL_SIZE_PX - 1.5;
  fRenderList.AddSpriteG(rxUnits, Id, 0, cornerX, cornerY, pX, ground);
end;


procedure TRenderPool.AddUnitFlag(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection;
                                  FlagAnim: Integer; pX, pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False);
const
  // Offsets for flags rendering in pixels
  FlagXOffset: array [TKMGroupType, TKMDirection] of shortint = (
    ( 0, 10, -1,  2,  1, -6,-10,  4, 13),  // gtMelee
    ( 0,  6,  5,  7, -3,-10, -4, 10,  9),  // gtAntiHorse
    ( 0,  8,  6,  6, -6, -8, -3,  8,  6),  // gtRanged
    ( 0,  6,  2,  3, -5,-10, -8,  5,  6)); // gtMounted

  FlagYOffset: array [TKMGroupType, TKMDirection] of shortint = (
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gtMelee
    ( 0, 23, 25, 25, 21, 20, 19, 20, 22),  // gtAntiHorse
    ( 0, 28, 30, 30, 26, 25, 24, 25, 27),  // gtRanged
    ( 0,  4, 16, 16,  4,  5,  2,  3,  4)); // gtMounted
var
  R: TRXData;
  A: TKMAnimLoop;
  id0, idFlag: Integer;
  flagX, flagY, ground: Single;
begin
  R := fRXData[rxUnits];

  // Unit position
  A := gRes.Units[aUnit].UnitAnim[aAct, aDir];
  id0 := A.Step[UNIT_STILL_FRAMES[aDir] mod Byte(A.Count) + 1] + 1;

  // Units feet
  ground := pY + (R.Pivot[id0].Y + R.Size[id0].Y) / CELL_SIZE_PX;

  // Flag position
  idFlag := GetUnitAnimSprite(aUnit, uaWalkArm, aDir, FlagAnim, gGameParams.TickFrac);
  if idFlag <= 0 then Exit;

  flagX := pX + (R.Pivot[idFlag].X + FlagXOffset[UNIT_TO_GROUP_TYPE[aUnit], aDir]) / CELL_SIZE_PX - 0.5;
  flagY := gTerrain.RenderFlatToHeight(pX, pY) + (R.Pivot[idFlag].Y + FlagYOffset[UNIT_TO_GROUP_TYPE[aUnit], aDir] + R.Size[idFlag].Y) / CELL_SIZE_PX - 2.25;

  if DoImmediateRender then
    RenderSprite(rxUnits, idFlag, flagX, flagY, FlagColor)
  else
    fRenderList.AddSpriteG(rxUnits, idFlag, 0, flagX, flagY, pX, ground, FlagColor);
end;


procedure TRenderPool.AddUnitWithDefaultArm(aUnit: TKMUnitType; aUID: Integer; aAct: TKMUnitActionType; aDir: TKMDirection; StepId: Integer; pX,pY: Single; FlagColor: TColor4; DoImmediateRender: Boolean = False; DoHignlight: Boolean = False; HighlightColor: TColor4 = 0);
begin
  if aUnit = utFish then
    aAct := FISH_COUNT_ACT[5]; // In map editor always render 5 fish

  AddUnit(aUnit, aUID, aAct, aDir, StepId, 0.0, pX, pY, FlagColor, True, DoImmediateRender, DoHignlight, HighlightColor);
  if gRes.Units[aUnit].SupportsAction(uaWalkArm) then
    AddUnit(aUnit, aUID, uaWalkArm, aDir, StepId, 0.0, pX, pY, FlagColor, True, DoImmediateRender, DoHignlight, HighlightColor);
end;


{procedure TRenderPool.RenderObject(aRX: TRXType; aId: Word; pX,pY: Single);
type
    TVector4f = record X,Y,Z,W: Single; end;
    TColor4f = record R,G,B,A: Single; end;
const
    LightPos: TVector4f = (X:-1; Y:0; Z:-2; W:0);
    LightAmb: TColor4f = (R:0.1; G:0.1; B:0.1; A:0);
    LightDiff: TColor4f = (R:0.9; G:0.9; B:0.9; A:0);
    LightSpec: TColor4f = (R:1.0; G:1.0; B:1.0; A:0);
begin
  glPushMatrix;
  glPushAttrib(GL_LIGHTING_BIT or GL_DEPTH_BUFFER_BIT);
    glScalef(1, 1, CELL_SIZE_PX);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmb);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);
    glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpec);
    glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);

    glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 0.0); //Directional lighting
    glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR); //Specular does not depend on material color

    glTranslatef(pX, pY, -1);
    glRotatef(32.5, 1, 0, 0);
    glColor4f(0.8, 0.8, 0.8, 1);

    fSampleHouse.DrawModel;
  glPopAttrib;
  glPopMatrix;
end;}

procedure TRenderPool.RenderSprite(aRX: TRXType; aId: Integer; pX,pY: Single; Col: TColor4; DoHighlight: Boolean = False;
                                   HighlightColor: TColor4 = 0; aForced: Boolean = False);
var
  X, Y: Integer;
begin
  X := EnsureRange(Round(pX),1,gTerrain.MapX);
  Y := EnsureRange(Round(pY),1,gTerrain.MapY);
  //Do not render if sprite is under FOW
  if not aForced and (gMySpectator.FogOfWar.CheckVerticeRenderRev(X,Y) <= FOG_OF_WAR_MIN) then
    Exit;

  pX := RoundToTilePixel(pX, fViewport.Zoom);
  pY := RoundToTilePixel(pY, fViewport.Zoom);

  with gGFXData[aRX, aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    TRender.BindTexture(Tex.Id);
    if DoHighlight then
      glColor3ub(HighlightColor AND $FF, HighlightColor SHR 8 AND $FF, HighlightColor SHR 16 AND $FF);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1, Tex.v2); glVertex2f(pX                     , pY                      );
      glTexCoord2f(Tex.u2, Tex.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY                      );
      glTexCoord2f(Tex.u2, Tex.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1, Tex.v1); glVertex2f(pX                     , pY-pxHeight/CELL_SIZE_PX);
    glEnd;
  end;

  if gGFXData[aRX, aId].Alt.Id <> 0 then
    with gGFXData[aRX, aId] do
    begin
      glColor4ubv(@Col);
      TRender.BindTexture(Alt.Id);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1, Alt.v2); glVertex2f(pX                     , pY                      );
        glTexCoord2f(Alt.u2, Alt.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY                      );
        glTexCoord2f(Alt.u2, Alt.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX, pY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1, Alt.v1); glVertex2f(pX                     , pY-pxHeight/CELL_SIZE_PX);
      glEnd;
    end;
end;


// Param - defines at which level alpha-test will be set (acts like a threshhold)
// Then we render alpha-tested Mask to stencil buffer. Only those pixels that are
// white there will have sprite rendered
// If there are two masks then we need to render sprite only there
// where its mask is white AND where second mask is black
procedure TRenderPool.RenderSpriteAlphaTest(aRX: TRXType; aId: Integer; aWoodProgress: Single; pX, pY: Single;
                                            aId2: Integer = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
var
  X, Y: Integer;
begin
  X := EnsureRange(Round(pX),1,gTerrain.MapX);
  Y := EnsureRange(Round(pY),1,gTerrain.MapY);
  if (gMySpectator.FogOfWar.CheckVerticeRenderRev(X,Y) <= FOG_OF_WAR_MIN) then Exit;
  // Skip rendering if alphas are zero (occurs so non-started houses can still have child sprites)
  if (aWoodProgress = 0) and (aStoneProgress = 0) then Exit;

  pX := RoundToTilePixel(pX, fViewport.Zoom);
  pY := RoundToTilePixel(pY, fViewport.Zoom);

  X2 := RoundToTilePixel(X2, fViewport.Zoom);
  Y2 := RoundToTilePixel(Y2, fViewport.Zoom);

  glClear(GL_STENCIL_BUFFER_BIT);

  // Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  glPushAttrib(GL_COLOR_BUFFER_BIT);
    // Do not render anything on screen while setting up stencil mask
    glColorMask(False, False, False, False);

    // Prepare stencil mask. Sprite will be rendered only where are white pixels
    glEnable(GL_ALPHA_TEST);
    glBlendFunc(GL_ONE, GL_ZERO);

    // Wood progress
    glAlphaFunc(GL_GREATER, 1 - aWoodProgress);
    with gGFXData[aRX,aId] do
    begin
      glColor3f(1, 1, 1);
      TRender.BindTexture(Alt.Id);
      glBegin(GL_QUADS);
        glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(pX                     ,pY         );
        glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY         );
        glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY-pxHeight/CELL_SIZE_PX);
        glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(pX                     ,pY-pxHeight/CELL_SIZE_PX);
      glEnd;
      TRender.BindTexture(0);
    end;

    // Stone progress
    if aId2 <> 0 then
    begin
      glStencilOp(GL_DECR, GL_DECR, GL_DECR);

      glAlphaFunc(GL_GREATER, 1 - aStoneProgress);
        with gGFXData[aRX,aId2] do
        begin
          glColor3f(1, 1, 1);
          TRender.BindTexture(Alt.Id);
          glBegin(GL_QUADS);
            glTexCoord2f(Alt.u1,Alt.v2); glVertex2f(X2                     ,Y2         );
            glTexCoord2f(Alt.u2,Alt.v2); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2         );
            glTexCoord2f(Alt.u2,Alt.v1); glVertex2f(X2+pxWidth/CELL_SIZE_PX,Y2-pxHeight/CELL_SIZE_PX);
            glTexCoord2f(Alt.u1,Alt.v1); glVertex2f(X2                     ,Y2-pxHeight/CELL_SIZE_PX);
          glEnd;
          TRender.BindTexture(0);
        end;
    end;

    glDisable(GL_ALPHA_TEST);
    glAlphaFunc(GL_ALWAYS, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // Revert alpha mode

  glPopAttrib;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);

  // Render sprite
  with gGFXData[aRX,aId] do
  begin
    // FOW is rendered over the top so no need to make sprites black anymore
    glColor4ub(255, 255, 255, 255);

    TRender.BindTexture(Tex.Id);
    glBegin(GL_QUADS);
      glTexCoord2f(Tex.u1,Tex.v2); glVertex2f(pX                     ,pY         );
      glTexCoord2f(Tex.u2,Tex.v2); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY         );
      glTexCoord2f(Tex.u2,Tex.v1); glVertex2f(pX+pxWidth/CELL_SIZE_PX,pY-pxHeight/CELL_SIZE_PX);
      glTexCoord2f(Tex.u1,Tex.v1); glVertex2f(pX                     ,pY-pxHeight/CELL_SIZE_PX);
    glEnd;
    TRender.BindTexture(0);
  end;

  glDisable(GL_STENCIL_TEST);
end;


procedure TRenderPool.CollectPlans(const aRect: TKMRect);
var
  I: Integer;
begin
  fFieldsList.Clear;
  fHousePlansList.Clear;

  // Collect field plans (road, corn, wine)
  if gGameParams.IsReplayOrSpectate then
  begin
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        // Don't use Hand.GetFieldPlans as it will give us plans multiple times for allies
        gHands[I].Constructions.FieldworksList.GetFields(fFieldsList, aRect, False)
    else
      gHands[gMySpectator.FOWIndex].GetFieldPlans(fFieldsList, aRect, False)
  end
  else
  begin
    // Field plans for self and allies
    // Include fake field plans for painting
    gMySpectator.Hand.GetFieldPlans(fFieldsList, aRect, True);
  end;

  // House plans for self and allies
  if gGameParams.IsReplayOrSpectate then
  begin
    if gMySpectator.FOWIndex = -1 then
      for I := 0 to gHands.Count - 1 do
        // Don't use Hand.GetHousePlans as it will give us plans multiple times for allies
        gHands[I].Constructions.HousePlanList.GetOutlines(fHousePlansList, aRect)
    else
      gHands[gMySpectator.FOWIndex].GetHousePlans(fHousePlansList, aRect)
  end
  else
    gMySpectator.Hand.GetHousePlans(fHousePlansList, aRect);
end;


//Render wire on tile
//P - tile coords
//Col - Color
//aInset - Internal adjustment, to render wire "inside" tile
procedure TRenderPool.RenderWireTile(const P: TKMPoint; aCol: TColor4; aInset: Single = 0.0; aLineWidth: Single = -1);
begin
  if not gTerrain.TileInMapCoords(P.X, P.Y) then Exit;

  TRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)

  //Change LineWidth
  if aLineWidth > 0 then
    glLineWidth(aLineWidth);

  gRenderAux.RenderWireTile(P, aCol, aInset);

  if aLineWidth > 0 then
    SetDefaultRenderParams;
end;


// Until profiling we use straightforward approach of recreating outline each frame
// Optimize later if needed
procedure TRenderPool.RenderHouseOutline(aHouseSketch: TKMHouseSketch; aCol: TColor4 = icCyan);
var
  I: Integer;
  loc: TKMPoint;
  X, Y: Word;
begin
  if (aHouseSketch = nil) or aHouseSketch.IsEmpty then
    Exit;

  // Get an outline of build area
  fHouseOutline.Clear;

  loc := aHouseSketch.Position;
  gRes.Houses[aHouseSketch.HouseType].Outline(fHouseOutline);

  TRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)
//  glColor3f(0, 1, 1);
  glColor4ubv(@aCol);
  glBegin(GL_LINE_LOOP);
    with gTerrain do
    for I := 0 to fHouseOutline.Count - 1 do
    begin
      X := loc.X + fHouseOutline[I].X - 3;
      Y := loc.Y + fHouseOutline[I].Y - 4;
      glVertex2f(X, Y - Land^[Y+1, X+1].RenderHeight / CELL_HEIGHT_DIV);
    end;
  glEnd;
end;


procedure TRenderPool.RenderSpriteOnTile(const aLoc: TKMPoint; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF);
var
  pX, pY: Single;
begin
  if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y)
    or (gMySpectator.FogOfWar.CheckVerticeRenderRev(aLoc.X,aLoc.Y) <= FOG_OF_WAR_MIN) then Exit;

  pX := aLoc.X - 0.5 + fRXData[rxGui].Pivot[aId].X / CELL_SIZE_PX;
  pY := gTerrain.RenderFlatToHeight(aLoc.X - 0.5, aLoc.Y - 0.5) -
        fRXData[rxGui].Pivot[aId].Y / CELL_SIZE_PX;
  RenderSprite(rxGui, aId, pX, pY, aFlagColor);
end;


procedure TRenderPool.RenderSpriteOnTerrain(const aLoc: TKMPointF; aId: Integer; aFlagColor: TColor4 = $FFFFFFFF; aForced: Boolean = False);
var
  pX, pY: Single;
begin
  // if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then Exit;
  pX := aLoc.X + fRXData[rxGui].Pivot[aId].X / CELL_SIZE_PX;
  pY := gTerrain.RenderFlatToHeight(aLoc.X, aLoc.Y) +
        fRXData[rxGui].Pivot[aId].Y / CELL_SIZE_PX;
  RenderSprite(rxGui, aId, pX, pY, aFlagColor, False, 0, aForced);
end;


procedure TRenderPool.RenderWireHousePlan(const P: TKMPoint; aHouseType: TKMHouseType);
var
  I: Integer;
  showHMarksIgnoreFOW: Boolean;
begin
  fMarksList.Clear;
  //Show house marks ignoring player FOW if we can see all map in replay/spec
  showHMarksIgnoreFOW := gGameParams.IsReplayOrSpectate and (gMySpectator.FOWIndex = -1);
  gMySpectator.Hand.GetHouseMarks(P, aHouseType, fMarksList, showHMarksIgnoreFOW);

  for I := 0 to fMarksList.Count - 1 do
  if fMarksList.Tag[I] = TC_OUTLINE then
    RenderWireTile(fMarksList[I], icCyan) // Cyan rect
  else
    RenderSpriteOnTile(fMarksList[I], fMarksList.Tag[I]); // Icon
end;


procedure TRenderPool.RenderForegroundUI_Markers;
var
  P: TKMPoint;
  HWFP: TKMHouseWFlagPoint;
begin
  P := gCursor.Cell;
  case gCursor.Tag1 of
    MARKER_REVEAL:        begin
                            RenderSpriteOnTile(P, 394, gMySpectator.Hand.FlagColor);
                            gRenderAux.CircleOnTerrain(P.X-0.5, P.Y-0.5,
                             gCursor.MapEdSize,
                             gMySpectator.Hand.FlagColor AND $10FFFFFF,
                             gMySpectator.Hand.FlagColor);
                          end;
    MARKER_DEFENCE:       RenderSpriteOnTile(P, 519, gMySpectator.Hand.FlagColor);
    MARKER_CENTERSCREEN:  RenderSpriteOnTile(P, 391, gMySpectator.Hand.FlagColor);
    MARKER_AISTART:       RenderSpriteOnTile(P, 390, gMySpectator.Hand.FlagColor);
    MARKER_RALLY_POINT:   if gMySpectator.Selected is TKMHouseWFlagPoint then
                          begin
                            HWFP := TKMHouseWFlagPoint(gMySpectator.Selected);
                            PaintFlagPoint(HWFP.Entrance, P, gMySpectator.Hand.FlagColor, HWFP.FlagPointTexId, True, True);
                          end;
  end;
end;


procedure TRenderPool.RenderForegroundUI_ElevateEqualize;
var
  I, K: Integer;
  tmp: Single;
  rad, slope: Byte;
  F: TKMPointF;
begin
  F := gCursor.Float;
  rad := gCursor.MapEdSize;
  slope := gCursor.MapEdSlope;
  for I := Max((Round(F.Y) - rad), 1) to Min((Round(F.Y) + rad), gTerrain.MapY -1) do
    for K := Max((Round(F.X) - rad), 1) to Min((Round(F.X) + rad), gTerrain.MapX - 1) do
    begin
      case gCursor.MapEdShape of
        hsCircle: tmp := 1 - GetLength(I-Round(F.Y), K-Round(F.X)) / rad;
        hsSquare: tmp := 1 - Math.max(abs(I-Round(F.Y)), abs(K-Round(F.X))) / rad;
        else                 tmp := 0;
      end;
      tmp := Power(Abs(tmp), (slope + 1) / 6) * Sign(tmp); // Modify slopes curve
      tmp := EnsureRange(tmp * 2.5, 0, 1); // *2.5 makes dots more visible
      gRenderAux.DotOnTerrain(K, I, $FF or (Round(tmp*255) shl 24));
    end;
    case gCursor.MapEdShape of
      hsCircle: gRenderAux.CircleOnTerrain(round(F.X), round(F.Y), rad, $00000000,  $FFFFFFFF);
      hsSquare: gRenderAux.SquareOnTerrain(round(F.X) - rad, round(F.Y) - rad, round(F.X + rad), round(F.Y) + rad, $FFFFFFFF);
    end;
end;


procedure TRenderPool.RenderForegroundUI_ObjectsBrush;
var
  I, K: Integer;
  tmp: Single;
  rad, slope: Byte;
  F: TKMPointF;
begin
  F := gCursor.Float;
  rad := (gCursor.MapEdSize div 2) +1;
  slope := gCursor.MapEdSlope;
  for I := Max((Round(F.Y) - rad), 1) to Min((Round(F.Y) + rad), gTerrain.MapY -1) do
    for K := Max((Round(F.X) - rad), 1) to Min((Round(F.X) + rad), gTerrain.MapX - 1) do
    begin
      case gCursor.MapEdShape of
        hsCircle: tmp := 1 - GetLength(I-Round(F.Y), K-Round(F.X)) / rad;
        hsSquare: tmp := 1 - Math.max(abs(I-Round(F.Y)), abs(K-Round(F.X))) / rad;
        else                 tmp := 0;
      end;
      tmp := Power(Abs(tmp), (slope + 1) / 6) * Sign(tmp); // Modify slopes curve
      tmp := EnsureRange(tmp * 2.5, 0, 1); // *2.5 makes dots more visible
      gRenderAux.DotOnTerrain(K, I, $FF or (Round(tmp*255) shl 24));
    end;
    case gCursor.MapEdShape of
      hsCircle: gRenderAux.CircleOnTerrain(round(F.X), round(F.Y), rad, $00000000,  $FFFFFFFF);
      hsSquare: gRenderAux.SquareOnTerrain(round(F.X) - rad, round(F.Y) - rad, round(F.X + rad), round(F.Y) + rad, $FFFFFFFF);
    end;
end;

procedure TRenderPool.RenderWireTileInt(const X,Y: Integer);
begin
  RenderWireTile(KMPoint(X, Y), icLightCyan, 0, 0.3);
end;


procedure TRenderPool.RenderTileInt(const X, Y: Integer);
begin
 if gCursor.MapEdSize = 0 then
    // Brush size smaller than one cell
    gRenderAux.DotOnTerrain(Round(gCursor.Float.X), Round(gCursor.Float.Y), $FF80FF80)
  else
    RenderTile(Combo[TKMTerrainKind(gCursor.Tag1), TKMTerrainKind(gCursor.Tag1),1],X,Y,0);
end;


procedure TRenderPool.RenderForegroundUI_Brush;
var
  P, RP: TKMPoint;
  size: Integer;
  isSquare: Boolean;
begin
  P := gCursor.Cell;
  size := gCursor.MapEdSize;
  isSquare := gCursor.MapEdShape = hsSquare;
  if gCursor.MapEdUseMagicBrush then
    IterateOverArea(P, size, isSquare, RenderWireTileInt)
  else
  if gCursor.Tag1 <> 0 then
  begin
    if SHOW_BRUSH_APPLY_AREA then
    begin
      RP := P;
      if size = 0 then
        RP := KMPoint(Round(gCursor.Float.X+1), Round(gCursor.Float.Y+1));
      IterateOverArea(RP, size, isSquare, RenderWireTileInt, True); // Render surrounding tiles, that will be fixed with transitions
    end;
    IterateOverArea(P, size, isSquare, RenderTileInt);
  end;
end;


//Render tile owner layer
procedure TRenderPool.RenderTileOwnerLayer(const aRect: TKMRect);
var
  I, K: Integer;
  P: TKMPoint;
begin
  for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
    begin
      P := KMPoint(K, I);
      if    (gTerrain.Land^[I, K].TileOwner <> PLAYER_NONE) //owner is set for tile
        and (gTerrain.TileIsCornField(P)                   // show only for corn + wine + roads
          or gTerrain.TileIsWineField(P)
          or (gTerrain.Land^[I, K].TileOverlay = toRoad)) then
        RenderWireTile(P, gHands[gTerrain.Land^[I, K].TileOwner].FlagColor, 0.05);
    end;
end;


//Render tiles grid layer
procedure TRenderPool.RenderTilesGrid(const aRect: TKMRect);
var
  I, K: Integer;
begin
  for I := aRect.Top to aRect.Bottom do
    for K := aRect.Left to aRect.Right do
      RenderWireTile(KMPoint(K, I), icDarkCyan, 0, 1);
end;


procedure TRenderPool.RenderForegroundUI;
var
  P: TKMPoint;
  F: TKMPointF;
begin
  if gCursor.Cell.Y * gCursor.Cell.X = 0 then Exit; // Caused a rare crash

  TRender.BindTexture(0); // We have to reset texture to default (0), because it could be bind to any other texture (atlas)

  if gGameParams.IsMapEditor then
    gGame.MapEditor.Paint(plCursors, KMRect(0,0,0,0));

  P := gCursor.Cell;
  F := gCursor.Float;

  if (gCursor.Mode <> cmNone) and (gCursor.Mode <> cmHouses) and
     (gMySpectator.FogOfWar.CheckTileRevelation(P.X, P.Y) = 0) then
    RenderSpriteOnTile(P, TC_BLOCK)       // Red X
  else

  with gTerrain do
  case gCursor.Mode of
    cmNone:       ;
    cmErase:      if not gGameParams.IsMapEditor then
                  begin
                    if ((gMySpectator.Hand.Constructions.FieldworksList.HasFakeField(P) <> ftNone)
                        or gMySpectator.Hand.Constructions.HousePlanList.HasPlan(P)
                        or (gMySpectator.Hand.HousesHitTest(P.X, P.Y) <> nil))
                    then
                      RenderWireTile(P, icCyan) // Cyan quad
                    else
                      RenderSpriteOnTile(P, TC_BLOCK); // Red X
                  end;
    cmRoad:       if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftRoad)) and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmField:      if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftCorn) or (gGameParams.IsMapEditor and gTerrain.TileIsCornField(P)))
                    and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmWine:       if (gMySpectator.Hand.CanAddFakeFieldPlan(P, ftWine) or (gGameParams.IsMapEditor and gTerrain.TileIsWineField(P)))
                    and (gCursor.Tag1 <> Ord(cfmErase)) then
                    RenderWireTile(P, icCyan) // Cyan quad
                  else
                    RenderSpriteOnTile(P, TC_BLOCK);       // Red X
    cmHouses:     RenderWireHousePlan(KMPointAdd(P, gCursor.DragOffset), TKMHouseType(gCursor.Tag1)); // Cyan quads and red Xs
    cmBrush:      RenderForegroundUI_Brush;
    cmTiles:      if gCursor.MapEdDir in [0..3] then
                    RenderTile(gCursor.Tag1, P.X, P.Y, gCursor.MapEdDir)
                  else
                    RenderTile(gCursor.Tag1, P.X, P.Y, (gTerrain.AnimStep div 5) mod 4); // Spin it slowly so player remembers it is on randomized
    cmOverlays:   begin
                    RenderWireTile(P, icCyan);
                    if gCursor.Tag1 > 0 then
                      RenderTile(TILE_OVERLAY_IDS[TKMTileOverlay(gCursor.Tag1)], P.X, P.Y, 0);
                    end;
    cmObjects:    begin
                    // If there's object below - paint it in Red
                    RenderMapElement(gTerrain.Land^[P.Y,P.X].Obj, gTerrain.AnimStep, P.X, P.Y, True, True);
                    RenderMapElement(gCursor.Tag1, gTerrain.AnimStep, P.X, P.Y, True);
                  end;
    cmObjectsBrush: RenderForegroundUI_ObjectsBrush;
    cmMagicWater: begin
                    If gTerrain.Land[P.Y, P.X].BaseLayer.Rotation+1 <=3 then
                      RenderTile(192, P.X, P.Y, gTerrain.Land[P.Y, P.X].BaseLayer.Rotation+1)
                    else
                      RenderTile(192, P.X, P.Y, 0);
                    RenderWireTile(P, icCyan);
                  end;
    cmEyeDropper: RenderWireTile(P, icCyan); // Cyan quad
    cmRotateTile: RenderWireTile(P, icCyan); // Cyan quad
    cmElevate,
    cmEqualize:      RenderForegroundUI_ElevateEqualize;
    cmConstHeight:   RenderForegroundUI_ElevateEqualize;
    cmUnits:      RenderForegroundUI_Units;
    cmMarkers:    RenderForegroundUI_Markers;
    cmPaintBucket:      RenderForegroundUI_PaintBucket(ssShift in gCursor.SState);
    cmUniversalEraser:  RenderForegroundUI_UniversalEraser(ssShift in gCursor.SState);
  end;

  if DISPLAY_SOUNDS then gSoundPlayer.Paint;
end;


procedure TRenderPool.RenderUnit(U: TKMUnit; const P: TKMPoint; FlagColor: Cardinal; DoHighlight: Boolean; HighlightColor: Cardinal);
begin
  AddUnitWithDefaultArm(U.UnitType, 0, uaWalk, U.Direction, U.AnimStep, P.X+UNIT_OFF_X, P.Y+UNIT_OFF_Y, FlagColor, True, DoHighlight, HighlightColor);
end;


//Try to render Unit or Unit group.
//Return True, if succeeded
function TRenderPool.TryRenderUnitOrGroup(aObject: TObject; aUnitFilterFunc, aGroupFilterFunc: TBooleanFunc;
                                          aUseGroupFlagColor, aDoHighlight: Boolean;
                                          aHandColor, aFlagColor: Cardinal; aHighlightColor: Cardinal = 0): Boolean;
var
  U: TKMUnit;
  G: TKMUnitGroup;
  groupFlagColor: Cardinal;
begin
  Result := False;
  if (aObject is TKMUnit) then
  begin
    U := TKMUnit(aObject);
    if not Assigned(aUnitFilterFunc) or aUnitFilterFunc(aObject) then
    begin
      RenderUnit(U, U.Position, aHandColor, aDoHighlight, aHighlightColor);
      Result := True;
    end;
  end else 
  if (aObject is TKMUnitGroup) then
  begin
    G := TKMUnitGroup(aObject);
    if not Assigned(aGroupFilterFunc) or aGroupFilterFunc(aObject) then
    begin
      U := G.FlagBearer;
      if aUseGroupFlagColor then
        groupFlagColor := G.FlagColor
      else
        groupFlagColor := aFlagColor;

      if G.IsFlagRenderBeforeUnit(U.Direction) then
      begin
        G.PaintHighlighted(0.0, aHandColor, groupFlagColor, True, aDoHighlight, aHighlightColor);
        RenderUnit(U, U.Position, aHandColor, aDoHighlight, aHighlightColor);
      end else begin
        RenderUnit(U, U.Position, aHandColor, aDoHighlight, aHighlightColor);
        G.PaintHighlighted(0.0, aHandColor, groupFlagColor, True, aDoHighlight, aHighlightColor);
      end;
      Result := True;
    end;
  end;
end;


procedure TRenderPool.RenderForegroundUI_Units;
var
  obj: TObject;
  P: TKMPoint;
begin
  if gCursor.Tag1 = 255 then
  begin
    obj := gMySpectator.HitTestCursorWGroup(True);
    TryRenderUnitOrGroup(obj, nil, nil, True, True, DELETE_COLOR, 0, DELETE_COLOR);
  end
  else begin
    P := gCursor.Cell;
    if gTerrain.CanPlaceUnit(P, TKMUnitType(gCursor.Tag1)) then
      AddUnitWithDefaultArm(TKMUnitType(gCursor.Tag1), 0, uaWalk, dirS, UNIT_STILL_FRAMES[dirS], P.X+UNIT_OFF_X, P.Y+UNIT_OFF_Y, gMySpectator.Hand.FlagColor, True)
    else
      RenderSpriteOnTile(P, TC_BLOCK); // Red X
  end;
end;


procedure TRenderPool.RenderForegroundUI_UniversalEraser(aHighlightAll: Boolean);
var
  obj: TObject;
  P: TKMPoint;
  isRendered: Boolean;
begin
  P := gCursor.Cell;
  obj := gMySpectator.HitTestCursorWGroup(True);

  isRendered := TryRenderUnitOrGroup(obj, nil, nil, True, True, DELETE_COLOR, 0, DELETE_COLOR);

  if (obj is TKMHouse) then
  begin
    AddWholeHouse(TKMHouse(obj), gHands[TKMHouse(obj).Owner].FlagColor, True, True, DELETE_COLOR);
    isRendered := True;
  end;

  // Terrain object found on the cell
  if (aHighlightAll or not isRendered) and (gTerrain.Land^[P.Y,P.X].Obj <> OBJ_NONE) then
  begin
    RenderMapElement(gTerrain.Land^[P.Y,P.X].Obj, gTerrain.AnimStep, P.X, P.Y, True, True);
    isRendered := True;
  end;

  if (aHighlightAll or not isRendered) and
    (((gTerrain.Land^[P.Y, P.X].TileOverlay <> toNone)
        and (gTerrain.Land^[P.Y, P.X].TileLock = tlNone)) //Sometimes we can point road tile under the house, do not show Cyan quad then
      or (gGame.MapEditor.LandMapEd^[P.Y, P.X].CornOrWine <> 0)) then
    RenderWireTile(P, icCyan); // Cyan quad
end;


function TRenderPool.PaintBucket_GroupToRender(aGroup: TObject): Boolean;
begin
   Result := (aGroup is TKMUnitGroup) and (TKMUnitGroup(aGroup).Owner <> gMySpectator.HandID);
end;


function TRenderPool.PaintBucket_UnitToRender(aUnit: TObject): Boolean;
begin
   Result := (aUnit is TKMUnit) and not (aUnit is TKMUnitAnimal) and
    (TKMUnit(aUnit).Owner <> gMySpectator.HandID);
end;


procedure TRenderPool.RenderForegroundUI_PaintBucket(aHighlightAll: Boolean);
var
  obj: TObject;
  highlightColor: Cardinal;
  P: TKMPoint;
  isRendered: Boolean;
begin
  P := gCursor.Cell;
  highlightColor := MultiplyBrightnessByFactor(gMySpectator.Hand.FlagColor, 2, 0.3, 0.9);
  obj := gMySpectator.HitTestCursorWGroup;

  isRendered := TryRenderUnitOrGroup(obj, PaintBucket_UnitToRender, PaintBucket_GroupToRender,
                                     False, True,
                                     gMySpectator.Hand.FlagColor, gMySpectator.Hand.FlagColor, highlightColor);

  if (obj is TKMHouse) and (TKMHouse(obj).Owner <> gMySpectator.HandID) then
  begin
    AddWholeHouse(TKMHouse(obj), gMySpectator.Hand.FlagColor, True, True, highlightColor);
    isRendered := True;
  end;

  if (aHighlightAll or not isRendered) and
    (((gTerrain.Land^[P.Y, P.X].TileOverlay = toRoad)
        and (gTerrain.Land^[P.Y, P.X].TileLock = tlNone)) //Sometimes we can point road tile under the house, do not show Cyan quad then
      or (gGame.MapEditor.LandMapEd^[P.Y, P.X].CornOrWine <> 0))
    and (gTerrain.Land^[P.Y, P.X].TileOwner <> gMySpectator.HandID) then //Only if tile has other owner
    RenderWireTile(P, icCyan); // Cyan quad
end;


{ TRenderList }
constructor TRenderList.Create;
begin
  inherited;

  fCount := 0;
  SetLength(RenderList, 512); // Allocate some space

  fUnitsRXData := gRes.Sprites[rxUnits].RXData;
end;


destructor TRenderList.Destroy;
begin
  SetLength(RenderList, 0);

  inherited;
end;


function TRenderList.GetSelectionUID(const CurPos: TKMPointF): Integer;
var
  I, K: Integer;
begin
  Result := -1; // Didn't hit anything
  // Skip if cursor is over FOW
  if gMySpectator.FogOfWar.CheckRevelation(CurPos) <= FOG_OF_WAR_MIN then Exit;
  // Select closest (higher Z) units first (list is in low..high Z-order)
  for I := Length(RenderOrder) - 1 downto 0 do
  begin
    K := RenderOrder[I];
    // Don't check child sprites, we don't want to select serfs by the long pike they are carrying
    if (RenderList[K].UID > 0) and KMInRect(CurPos, RenderList[K].SelectionRect) then
    begin
      Result := RenderList[K].UID;
      Exit;
    end;
  end;
end;


procedure TRenderList.Clear;
begin
  fCount := 0;
end;


procedure TRenderList.ClipRenderList;
var
  I, J: Integer;
begin
  SetLength(RenderOrder, fCount);
  J := 0;
  for I := 0 to fCount - 1 do
    if RenderList[I].NewInst then
    begin
      RenderOrder[J] := I;
      Inc(J);
    end;
  SetLength(RenderOrder, J);
end;


// Sort all items in list from top-right to bottom-left
procedure TRenderList.SortRenderList;
var
  renderOrderAux: array of Word;

  procedure DoQuickSort(aLo, aHi: Integer);
  var
    lo, hi: Integer;
    mid: Single;
  begin
    lo := aLo;
    hi := aHi;
    mid := RenderList[RenderOrder[(lo + hi) div 2]].Feet.Y;
    repeat
      while RenderList[RenderOrder[lo]].Feet.Y < mid do Inc(lo);
      while RenderList[RenderOrder[hi]].Feet.Y > mid do Dec(hi);
      if lo <= hi then
      begin
        SwapInt(RenderOrder[lo], RenderOrder[hi]);
        Inc(lo);
        Dec(hi);
      end;
    until lo > hi;
    if hi > aLo then DoQuickSort(aLo, hi);
    if lo < aHi then DoQuickSort(lo, aHi);
  end;

  procedure Merge(aStart, aMid, aEnd: Integer);
  var
    I, A, B: Integer;
  begin
    A := aStart;
    B := aMid;
    for I := aStart to aEnd - 1 do
      if (A < aMid) and ((B >= aEnd)
      or (RenderList[renderOrderAux[A]].Feet.Y <= RenderList[renderOrderAux[B]].Feet.Y)) then
      begin
        RenderOrder[I] := renderOrderAux[A];
        Inc(A);
      end else begin
        RenderOrder[I] := renderOrderAux[B];
        Inc(B);
      end;
  end;

  // The same as Merge, but RenderOrder and RenderOrderAux are switched
  procedure MergeAux(aStart, aMid, aEnd: Integer);
  var
    I, A, B: Integer;
  begin
    A := aStart;
    B := aMid;
    for I := aStart to aEnd-1 do
      if (A < aMid) and ((B >= aEnd)
      or (RenderList[RenderOrder[A]].Feet.Y <= RenderList[RenderOrder[B]].Feet.Y)) then
      begin
        renderOrderAux[I] := RenderOrder[A];
        Inc(A);
      end else begin
        renderOrderAux[I] := RenderOrder[B];
        Inc(B);
      end;
  end;

  // aUseAux tells us which array to store results in, it should flip each recurse
  procedure DoMergeSort(aStart, aEnd: Integer; aUseAux: Boolean);
  var
    mid: Integer;
  begin
    if aEnd - aStart < 2 then Exit;
    mid := (aStart + aEnd) div 2;
    DoMergeSort(aStart, mid, not aUseAux);
    DoMergeSort(mid, aEnd, not aUseAux);
    if aUseAux then
      MergeAux(aStart, mid, aEnd)
    else
      Merge(aStart, mid, aEnd);
  end;

begin
  ClipRenderList;
  if fCount > 0 then
  begin
    SetLength(renderOrderAux, Length(RenderOrder));
    Move(RenderOrder[0], renderOrderAux[0], Length(RenderOrder)*SizeOf(RenderOrder[0]));
    // Quicksort is unstable which causes Z fighting, so we use mergesort
    DoMergeSort(0, Length(RenderOrder), False);
    // DoQuickSort(0, Length(RenderOrder) - 1);
  end;
end;


// New items must provide their ground level
procedure TRenderList.AddSpriteG(aRX: TRXType; aId: Integer; aUID: Integer; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
const
  MAX_SEL_RECT_HEIGHT = CELL_SIZE_PX * 1.5; //Restrict too long images selection rect
var
  W, H, WS: Integer;
begin
  if fCount >= Length(RenderList) then
    SetLength(RenderList, fCount + 256); // Book some space

  RenderList[fCount].Loc        := KMPointF(pX, pY); // Position of sprite, floating-point
  RenderList[fCount].Feet       := KMPointF(gX, gY); // Ground position of sprite for Z-sorting
  RenderList[fCount].RX         := aRX;             // RX library
  RenderList[fCount].Id         := aId;             // Texture Id
  RenderList[fCount].UID        := aUID;            // Object Id
  RenderList[fCount].NewInst    := True;            // Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[fCount].TeamColor  := aTeam;           // Team Id (determines color)
  RenderList[fCount].AlphaStep  := aAlphaStep;      // Alpha step for wip buildings

    if aUID > 0 then
    with RenderList[fCount].SelectionRect do
    begin
      WS := (fUnitsRXData.SizeNoShadow[aId].right div 2) + fUnitsRXData.Pivot[aId].x; //Fix for bow and crossbowman
      //Enlarge rect from image size to the left and right, to be at least CELL_SIZE_PX width and height
      W := Max(0, CELL_SIZE_PX - (Abs(WS div 2) + fUnitsRXData.SizeNoShadow[aId].right - fUnitsRXData.SizeNoShadow[aId].left)); //width to add to image pos. half to the left, half to the right
      H := Max(0, CELL_SIZE_PX - (fUnitsRXData.SizeNoShadow[aId].bottom - fUnitsRXData.SizeNoShadow[aId].top)); //height to add to image pos. half to the top, half to the bottom

      Left := RenderList[fCount].Loc.X + (-(W div 2) - Max(0,WS) + fUnitsRXData.SizeNoShadow[aId].left) / CELL_SIZE_PX;
      Bottom := gY - ((H div 2) + fUnitsRXData.Size[aId].Y - 1 - fUnitsRXData.SizeNoShadow[aId].bottom) / CELL_SIZE_PX;
      Right := Left + (W - Min(0, WS) + gGFXData[aRX, aId].PxWidth - (fUnitsRXData.Size[aId].X - 1 - fUnitsRXData.SizeNoShadow[aId].right)) / CELL_SIZE_PX;
      Top := Bottom - Min(MAX_SEL_RECT_HEIGHT, H + gGFXData[aRX, aId].PxHeight - fUnitsRXData.SizeNoShadow[aId].top) / CELL_SIZE_PX;
    end;

  Inc(fCount); // New item added
end;


// Child items don't need ground level
procedure TRenderList.AddSprite(aRX: TRXType; aId: Integer; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
begin
  if fCount >= Length(RenderList) then
    SetLength(RenderList, fCount + 256); // Book some space

  RenderList[fCount].Loc        := KMPointF(pX,pY); // Position of sprite, floating-point
  RenderList[fCount].Feet       := RenderList[fCount-1].Feet;  // Ground position of sprite for Z-sorting
  RenderList[fCount].RX         := aRX;             // RX library
  RenderList[fCount].Id         := aId;             // Texture Id
  RenderList[fCount].UID        := 0;               // Child sprites aren't used for selecting units
  RenderList[fCount].NewInst    := False;           // Is this a new item (can be occluded), or a child one (always on top of it's parent)
  RenderList[fCount].TeamColor  := aTeam;           // Team Id (determines color)
  RenderList[fCount].AlphaStep  := aAlphaStep;      // Alpha step for wip buildings

  Inc(fCount); // New item added
end;


procedure TRenderList.SendToRender(aId: Integer);
var
  sp1, sp2: TKMRenderSprite;
  sp2Exists: Boolean;
begin
  // Shortcuts to Sprites info
  sp1 := RenderList[aId];
  sp2Exists := (aId + 1 < fCount);
  if sp2Exists then
    sp2 := RenderList[aId + 1];

  if sp1.AlphaStep = -1 then
    gRenderPool.RenderSprite(sp1.RX, sp1.Id, sp1.Loc.X, sp1.Loc.Y, sp1.TeamColor)
  else
  begin
    // Houses are rendered as Wood+Stone part. For Stone we want to skip
    // Wooden part where it is occluded (so that smooth shadows dont overlay)

    // Check if next comes our child, Stone layer
    if sp2Exists and not sp2.NewInst and (sp2.AlphaStep > 0) then
      gRenderPool.RenderSpriteAlphaTest(sp1.RX, sp1.Id, sp1.AlphaStep, sp1.Loc.X, sp1.Loc.Y,
                                                sp2.Id, sp2.AlphaStep, sp2.Loc.X, sp2.Loc.Y)
    else
      gRenderPool.RenderSpriteAlphaTest(sp1.RX, sp1.Id, sp1.AlphaStep, sp1.Loc.X, sp1.Loc.Y);
  end;

  if SHOW_GROUND_LINES and sp1.NewInst then
  begin
    // Child ground lines are useless
    glBegin(GL_LINES);
      glColor3f(1,1,0.5);
      glVertex2f(sp1.Feet.X + 0.15, gTerrain.RenderFlatToHeight(sp1.Feet).Y);
      glVertex2f(sp1.Feet.X - 0.15, gTerrain.RenderFlatToHeight(sp1.Feet).Y);
    glEnd;
  end;
end;


// Now render all these items from list
procedure TRenderList.Render;
var
  I, K, objectsCount: Integer;
begin
  {$IFDEF PERFLOG}
  gPerfLogs.SectionEnter(psFrameRenderList);
  {$ENDIF}
  fStat_Sprites := fCount;
  fStat_Sprites2 := 0;
  objectsCount := Length(RenderOrder);

  for I := 0 to objectsCount - 1 do
  begin
    K := RenderOrder[I];
    glPushMatrix;

      if RENDER_3D then
      begin
        glTranslatef(RenderList[K].Loc.X, RenderList[K].Loc.Y, 0);
        glRotatef(gRenderPool.rHeading, -1, 0, 0);
        glTranslatef(-RenderList[K].Loc.X, -RenderList[K].Loc.Y, 0);
      end;

      repeat // Render child sprites after their parent
        SendToRender(K);
        if SHOW_SEL_BUFFER and RenderList[K].NewInst and (RenderList[K].UID > 0) then
          gRenderAux.SquareOnTerrain(RenderList[K].SelectionRect.Left , RenderList[K].SelectionRect.Top,
                                     RenderList[K].SelectionRect.Right, RenderList[K].SelectionRect.Bottom, RenderList[K].UID or $FF000000);
        Inc(K);
        Inc(fStat_Sprites2);
      until ((K = fCount) or RenderList[K].NewInst);
    glPopMatrix;
  end;
  {$IFDEF PERFLOG}
  gPerfLogs.SectionLeave(psFrameRenderList);
  {$ENDIF}
end;


end.
