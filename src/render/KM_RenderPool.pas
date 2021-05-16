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
    ID: Word;
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

    procedure AddSprite(aRX: TRXType; aID: Word; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
    procedure AddSpriteG(aRX: TRXType; aID: Word; aUID: Integer; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);

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

    procedure RenderSprite(aRX: TRXType; aId: Word; pX,pY: Single; Col: TColor4; DoHighlight: Boolean = False;
                           HighlightColor: TColor4 = 0; aForced: Boolean = False);
    procedure RenderSpriteAlphaTest(aRX: TRXType; aId: Word; aWoodProgress: Single; pX, pY: Single; aId2: Word = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
    procedure RenderMapElement1(aIndex: Word; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderMapElement4(aIndex: Word; AnimStep: Cardinal; pX,pY: Integer; IsDouble: Boolean; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
    procedure RenderHouseOutline(aHouseSketch: TKMHouseSketch; aCol: Cardinal = icCyan);

    // Terrain rendering sub-class
    procedure CollectPlans(const aRect: TKMRect);
    procedure CollectTerrainObjects(const aRect: TKMRect; aAnimStep: Cardinal);
    procedure PaintFlagPoint(const aHouseEntrance, aFlagPoint: TKMPoint; aColor: Cardinal; aTexId: Word; aFirstPass: Boolean;
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

    procedure AddAlert(const aLoc: TKMPointF; aId: Word; aFlagColor: TColor4);
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
    procedure RenderSpriteOnTile(const aLoc: TKMPoint; aId: Word; aFlagColor: TColor4 = $FFFFFFFF);
    procedure RenderSpriteOnTerrain(const aLoc: TKMPointF; aId: Word; aFlagColor: TColor4 = $FFFFFFFF; aForced: Boolean = False);
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


function GetInterpSpriteOffset(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection): Integer;
const
  INTERP_LOOKUP: array[TKMUnitType, TKMUnitActionType, TKMDirection] of Integer = (
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
    (-1,18332,18420,18508,18596,18596,18684,18772,18860), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,18948,19028,19028,19108,19108,19188,19188,18948), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,19268,19508,19748,19508,19988,19508,20228,19508), // uaEat
    (-1,20468,20532,20596,20660,20724,20788,20852,20916), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utFarmer
    (-1,20980,21044,21108,21172,21236,21300,21364,21428), // uaWalk
    (-1,21492,21620,21620,21748,21748,21876,21876,21492), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,22004,22004,22084,22084,22164,22164,22244,22244), // uaWork1
    (-1,22324,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,22484,22724,22964,22724,23204,22724,23444,22724), // uaEat
    (-1,23684,-1,23748,23812,23876,23940,24004,24068), // uaWalkArm
    (-1,24132,24196,24260,24324,24388,24452,24516,24580), // uaWalkTool
    (-1,24644,24708,24772,24836,24900,24964,25028,25092), // uaWalkBooty
    (-1,25156,25220,25284,25348,25412,25476,25540,25604), // uaWalkTool2
    (-1,25668,25732,25796,25860,25924,25988,26052,26116), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utLamberjack
    (-1,26180,26244,26308,26372,26436,26500,26564,26628), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,26692,26932,27172,26932,27412,26932,27652,26932), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utBaker
    (-1,27892,27956,28020,28084,28148,28212,28276,28340), // uaWalk
    (-1,28404,28412,28420,28428,28436,28444,28452,28460), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,28468,28708,28948,28708,29188,28708,29428,28708), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utButcher
    (-1,29668,29732,29796,29860,29924,29988,30052,30116), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,30180,30420,30660,30420,30900,30420,31140,30420), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utFisher
    (-1,31380,31444,31508,31572,31636,31700,31764,31828), // uaWalk
    (-1,31892,32012,32132,32252,32372,32492,32612,32732), // uaWork
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,32852,32956,33060,33164,33268,33372,33476,33580), // uaWork1
    (-1,33684,33924,34164,34404,34644,34884,35124,35364), // uaWork2
    (-1,35604,35724,35844,35964,36084,36204,36324,36444), // uaWorkEnd
    (-1,36564,36804,37044,36804,37284,36804,37524,36804), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,37764,37828,37892,37956,38020,38084,38148,38212), // uaWalkTool
    (-1,38276,38340,38404,38468,38532,38596,38660,38724), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWorker
    (-1,38788,38852,38916,38980,39044,39108,39172,39236), // uaWalk
    (-1,39300,39300,39388,39388,39476,39476,39564,39564), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,39652,39652,39748,39748,39844,39844,39940,39940), // uaWork1
    (-1,40036,40036,40124,40124,40212,40212,40300,40300), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,40388,40628,40868,40628,41108,40628,41348,40628), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utStoneCutter
    (-1,41588,41652,41716,41780,41844,41908,41972,42036), // uaWalk
    (-1,42100,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,42180,42420,42660,42420,42900,42420,43140,42420), // uaEat
    (-1,43380,43444,43508,43572,43636,43700,43764,43828), // uaWalkArm
    (-1,43892,43956,44020,44084,44148,44212,44276,44340), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSmith
    (-1,44404,44468,44532,44596,44660,44724,44788,44852), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,44916,45156,45396,45156,45636,45156,45876,45156), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utMetallurgist
    (-1,46116,46180,46244,46308,46372,46436,46500,46564), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,46628,46868,47108,46868,47348,46868,47588,46868), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utRecruit
    (-1,47828,47892,47956,48020,48084,48148,48212,48276), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,48340,48340,48340,48340,48340,48340,48340,48340), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,48380,48620,48860,48620,49100,48620,49340,48620), // uaEat
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utMilitia
    (-1,49580,49644,49708,49772,49836,49900,49964,50028), // uaWalk
    (-1,50092,50188,50284,50380,50476,50572,50668,50764), // uaWork
    (-1,50860,50924,50988,51052,51116,51180,51244,51308), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,51372,51436,51500,51564,51372,51436,51500,51564), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utAxeFighter
    (-1,51628,51692,51756,51820,51884,51948,52012,52076), // uaWalk
    (-1,52140,52236,52332,52428,52524,52620,52716,52812), // uaWork
    (-1,52908,52972,53036,53100,53164,53228,53292,53356), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,51372,51436,51500,51564,51372,51436,51500,51564), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSwordsman
    (-1,53420,53484,53548,53612,53676,53740,53804,53868), // uaWalk
    (-1,53932,54028,54124,54220,54316,54412,54508,54604), // uaWork
    (-1,54700,54764,54828,54892,54956,55020,55084,55148), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,51372,51436,51500,51564,51372,51436,51500,51564), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utBowman
    (-1,55212,55276,55340,55404,55468,55532,55596,55660), // uaWalk
    (-1,55724,55868,56012,56156,56300,56444,56588,56732), // uaWork
    (-1,56876,56916,56956,56996,57036,57076,57116,57156), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,57196,57260,57324,57388,57196,57260,57324,57388), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utArbaletman
    (-1,57452,57516,57580,57644,57708,57772,57836,57900), // uaWalk
    (-1,57964,58188,58412,58636,58860,59084,59308,59532), // uaWork
    (-1,59756,59796,59836,59876,59916,59956,59996,60036), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,57196,57260,57324,57388,57196,57260,57324,57388), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utPikeman
    (-1,60076,60140,60204,60268,60332,60396,60460,60524), // uaWalk
    (-1,60588,60684,60780,60876,60972,61068,61164,61260), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,61356,61420,61484,61548,61356,61420,61484,61548), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utHallebardman
    (-1,61612,61676,61740,61804,61868,61932,61996,62060), // uaWalk
    (-1,62124,62220,62316,62412,62508,62604,62700,62796), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,61356,61420,61484,61548,61356,61420,61484,61548), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utHorseScout
    (-1,62892,62940,62988,63036,63084,63132,63180,63228), // uaWalk
    (-1,63276,63372,63468,63564,63660,63756,63852,63948), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,64044,64044,64196,64196,64324,64324,64460,64460), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,64588,64652,64716,64780,64588,64652,64716,64780), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utCavalry
    (-1,64844,64892,64940,64988,65036,65084,65132,65180), // uaWalk
    (-1,65228,65324,65420,65516,65612,65708,65804,65900), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,64044,64044,64196,64196,64324,64324,64460,64460), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,64588,64652,64716,64780,64588,64652,64716,64780), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utBarbarian
    (-1,65996,66060,66124,66188,66252,66316,66380,66444), // uaWalk
    (-1,66508,66604,66700,66796,66892,66988,67084,67180), // uaWork
    (-1,67276,67340,67404,67468,67532,67596,67660,67724), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,51372,51436,51500,51564,51372,51436,51500,51564), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utPeasant
    (-1,67788,67852,67916,67980,68044,68108,68172,68236), // uaWalk
    (-1,68300,68396,68492,68588,68684,68780,68876,68972), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,61356,61420,61484,61548,61356,61420,61484,61548), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utSlingshot
    (-1,69068,69132,69196,69260,69324,69388,69452,69516), // uaWalk
    (-1,69580,69820,70060,70300,70540,70780,71020,71260), // uaWork
    (-1,71500,71500,71500,71500,71500,71500,71500,71500), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,57196,57260,57324,57388,57196,57260,57324,57388), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utMetalBarbarian
    (-1,71540,71604,71668,71732,71796,71860,71924,71988), // uaWalk
    (-1,72052,72148,72244,72340,72436,72532,72628,72724), // uaWork
    (-1,72820,72884,72948,73012,73076,73140,73204,73268), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,51372,51436,51500,51564,51372,51436,51500,51564), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utHorseman
    (-1,73332,73380,73428,73476,73524,73572,73620,73668), // uaWalk
    (-1,73716,73812,73908,74004,74100,74196,74292,74388), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,64044,64044,64196,64196,64324,64324,64460,64460), // uaDie
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork1
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWorkEnd
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaEat
    (-1,64588,64652,64716,64780,64588,64652,64716,64780), // uaWalkArm
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkTool2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWalkBooty2
    (-1,-1,-1,-1,-1,-1,-1,-1,-1) // uaUnknown
  ),
  ( // utWolf
    (-1,74484,74548,74612,74676,74740,74804,74868,74932), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,74996,74996,75140,75140,75268,75268,75404,75404), // uaDie
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
    (-1,75564,75612,75660,75708,75756,75804,75852,75900), // uaWalk
    (-1,75948,75996,76044,76092,76140,76188,76236,76284), // uaWork
    (-1,76332,76380,76428,76476,76524,76572,76620,76668), // uaSpec
    (-1,76716,76764,76812,76860,76908,76956,77004,77052), // uaDie
    (-1,77100,77148,77196,77244,77292,77340,77388,77436), // uaWork1
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
    (-1,77484,77532,77580,77628,77676,77724,77772,77820), // uaWalk
    (-1,77868,77876,-1,-1,77884,77892,77900,77908), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,74996,74996,75140,75140,75268,75268,75404,75404), // uaDie
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
    (-1,77916,77916,77916,77916,77916,77916,77916,77916), // uaWalk
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
    (-1,77980,77980,77980,77980,77980,77980,77980,77980), // uaWalk
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
    (-1,78012,78012,78012,78012,78012,78012,78012,78012), // uaWalk
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
    (-1,78060,78060,78060,78060,78060,78060,78060,78060), // uaWalk
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
    (-1,78108,78116,78124,78132,78140,78148,78156,78164), // uaWalk
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaWork
    (-1,-1,-1,-1,-1,-1,-1,-1,-1), // uaSpec
    (-1,9812,9812,9940,9940,10052,10052,10172,10172), // uaDie
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
    Result := INTERP_LOOKUP[aUnit, aAct, aDir];
  end
  else
    Result := -1;
end;


function TRenderPool.GetUnitAnimSprite(aUnit: TKMUnitType; aAct: TKMUnitActionType; aDir: TKMDirection;
                                       aStep: Integer; aStepFrac: Single): Integer;
var
  A: TKMAnimLoop;
  InterpOffset: Integer;
const
  INTERP_LEVEL = 8;
begin
  InterpOffset := GetInterpSpriteOffset(aUnit, aAct, aDir);
  if InterpOffset >= 0 then
  begin
    Result := InterpOffset
      + INTERP_LEVEL*(aStep mod Byte(A.Count))
      + EnsureRange(Floor(INTERP_LEVEL*aStepFrac), 0, INTERP_LEVEL-1);
  end
  else
  begin
    A := gRes.Units[aUnit].UnitAnim[aAct, aDir];
    Result := A.Step[aStep mod Byte(A.Count) + 1] + 1;
  end;
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
      RenderMapElement1(FallingTrees.Tag[I], aAnimStep - FallingTrees.Tag2[I], FallingTrees[I].X, FallingTrees[I].Y);
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


procedure TRenderPool.PaintFlagPoint(const aHouseEntrance, aFlagPoint: TKMPoint; aColor: Cardinal; aTexId: Word; aFirstPass: Boolean;
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
    RenderMapElement1(aIndex,AnimStep,pX,pY,DoImmediateRender,Deleting);
end;


procedure TRenderPool.RenderMapElement1(aIndex: Word; AnimStep: Cardinal; LocX,LocY: Integer; DoImmediateRender: Boolean = False; Deleting: Boolean = False);
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
    Id := A.Step[AnimStep mod Byte(A.Count) +1]+1;
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
    Id := A.Step[aAnimStep mod Byte(A.Count) + 1] + 1;
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
procedure TRenderPool.AddAlert(const aLoc: TKMPointF; aId: Word; aFlagColor: TColor4);
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
    ptArrow:     with gRes.Units[utBowman].UnitAnim[uaSpec, aDir] do
                    id := Step[Round(Min(aFlight, 1) * (Count-1)) + 1] + 1;
    ptBolt:      with gRes.Units[utArbaletman].UnitAnim[uaSpec, aDir] do
                    id := Step[Round(Min(aFlight, 1) * (Count-1)) + 1] + 1;
    ptSlingRock: with gRes.Units[utSlingshot].UnitAnim[uaSpec, aDir] do
                    id := Step[Round(Min(aFlight, 1) * (Count-1)) + 1] + 1;
    ptTowerRock: id := ProjectileBounds[aProj, 1] + 1;
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
begin
  A := gRes.Units.SerfCarry[aCarry, aDir];
  id := A.Step[StepId mod Byte(A.Count) + 1] + 1;
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
  Id: Integer;
  cornerX, cornerY, ground: Single;
  R: TRXData;
  A: TKMAnimLoop;
  id0: Integer;
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

  // Thought bubbles are animated in reverse
  Id := THOUGHT_BOUNDS[Thought, 2] + 1 -
       (gGameParams.Tick mod Word(THOUGHT_BOUNDS[Thought, 2] - THOUGHT_BOUNDS[Thought, 1]));

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

procedure TRenderPool.RenderSprite(aRX: TRXType; aId: Word; pX,pY: Single; Col: TColor4; DoHighlight: Boolean = False;
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
procedure TRenderPool.RenderSpriteAlphaTest(aRX: TRXType; aId: Word; aWoodProgress: Single; pX, pY: Single;
                                            aId2: Word = 0; aStoneProgress: Single = 0; X2: Single = 0; Y2: Single = 0);
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


procedure TRenderPool.RenderSpriteOnTile(const aLoc: TKMPoint; aId: Word; aFlagColor: TColor4 = $FFFFFFFF);
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


procedure TRenderPool.RenderSpriteOnTerrain(const aLoc: TKMPointF; aId: Word; aFlagColor: TColor4 = $FFFFFFFF; aForced: Boolean = False);
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
procedure TRenderList.AddSpriteG(aRX: TRXType; aId: Word; aUID: Integer; pX,pY,gX,gY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
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
procedure TRenderList.AddSprite(aRX: TRXType; aId: Word; pX,pY: Single; aTeam: Cardinal = $0; aAlphaStep: Single = -1);
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
