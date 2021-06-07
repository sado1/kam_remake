unit KM_ResTilesetTypes;
{$I KaM_Remake.inc}
interface

const
  TILES_CNT = 597;
  MAX_TILE_TO_SHOW = TILES_CNT;
  MAX_STATIC_TERRAIN_ID = 9997;

type
  //TKMTileProperty = set of (tpWalkable, tpRoadable);

  TKMTileMaskType = (tmtNone,
    tmt2Straight, // A A
                  // B B

    tmt2Diagonal, // A A
                  // B A

    tmt2Corner,   // A B
                  // B B

    tmt2Opposite, // A B
                  // B A

    tmt3Straight, // A A
                  // C D

    tmt3Opposite, // A B
                  // D A

    tmt4Square);  // A B
                  // D C

  TKMTileMaskSubType = (mstMain, mstExtra);

  TKMTileMaskKind = (mkNone, mkSoft1, mkSoft2, mkSoft3, mkStraight, mkGradient);

  // Mask usage: as a pixel mask, or as a gradient mask
  TKMTileMaskKindUse = (mkuPixel, mkuAlpha);

  TKMMaskFullType = record
    Kind: TKMTileMaskKind;
    MType: TKMTileMaskType;
    SubType: TKMTileMaskSubType;
  end;

  PKMMaskFullType = ^TKMMaskFullType;

  TKMTerrainKind = (
//    tkNone,
    tkCustom,     //0
    tkGrass,      //1
    tkMoss,       //2
    tkPaleGrass,  //3
    tkCoastSand,  //4
    tkGrassSand1, //5
    tkGrassSand2, //6
    tkGrassSand3, //7
    tkSand,       //8
    tkGrassDirt,  //9
    tkDirt,       //10
    tkCobbleStone,//11
    tkGrassyWater,//12
    tkSwamp,      //13
    tkIce,        //14
    tkSnowOnGrass,//15
    tkSnowOnDirt, //16
    tkSnow,       //17
    tkDeepSnow,   //18
    tkStone,      //19
    tkGoldMount,  //20
    tkIronMount,  //21
    tkAbyss,      //22
    tkGravel,     //23
    tkCoal,       //24
    tkGold,       //25
    tkIron,       //26
    tkWater,      //27
    tkFastWater,  //28
    tkLava);      //29


  TKMTerrainKindsArray = array of TKMTerrainKind;

  TKMTerrainKindSet = set of TKMTerrainKind;

  TKMTerrainKindCorners = array[0..3] of TKMTerrainKind;

const
  TER_KIND_ORDER: array[tkCustom..tkLava] of Integer =
    (0,1,2,3,4,5,6,7,8,9,10,11,
      -1,    // To make Water/FastWater-GrassyWater transition possible with layers we need GrassyWater to be above Water because of animation (water above grassy anim looks ugly)
      13,
      -2,
      15,16,17,18,19,20,21,22,23,24,25,26,
      -4,-3, // Put GrassyWater/Water/FastWater always to the base layer, because of animation
      29);

  BASE_TERRAIN: array[TKMTerrainKind] of Word = //tkCustom..tkLava] of Word =
    (0, 0, 8, 17, 32, 26, 27, 28, 29, 34, 35, 215, 48, 40, 44, 315, 47, 46, 45, 132, 159, 164, 245, 20, 155, 147, 151, 192, 209, 7);

//  TILE_MASKS: array[mt_2Straight..mt_4Square] of Word =
//      (279, 278, 280, 281, 282, 277);

  TILE_MASKS_LAYERS_CNT: array[TKMTileMaskType] of Byte =
    (1, 2, 2, 2, 2, 3, 3, 4);

  TILE_MASK_KINDS_PREVIEW: array[TKMTileMaskKind] of Integer =
    (-1, 5551, 5561, 5571, 5581, 5591); //+1 here, so -1 is no image, and not grass

  TILE_MASK_KIND_USAGE: array [TKMTileMaskKind] of TKMTileMaskKindUse =
    (mkuPixel, mkuPixel, mkuPixel, mkuPixel, mkuPixel, mkuAlpha);


  TILE_MASKS_FOR_LAYERS:  array[Succ(Low(TKMTileMaskKind))..High(TKMTileMaskKind)]
                            of array[Succ(Low(TKMTileMaskType))..High(TKMTileMaskType)]
                              of array[TKMTileMaskSubType] of Integer =
     //Softest
    (((5549, -1),
      (5550, -1),
      (5551, -1),
      (5552, -1),
      (5551, 5549),
      (5551, 5552),
      (5551, -1)),
     //Soft
     ((5559, -1),
      (5560, -1),
      (5561, -1),
      (5562, -1),
      (5561, 5559),
      (5561, 5562),
      (5561, -1)),
     //Soft2
     ((5569, -1),
      (5570, -1),
      (5571, -1),
      (5572, -1),
      (5571, 5569),
      (5571, 5572),
      (5571, -1)),
     //Hard
     ((5579, -1),
      (5580, -1),
      (5581, -1),
      (5582, -1),
      (5581, 5579),
      (5581, 5582),
      (5581, -1)),
     //Gradient
     ((5589, -1),
      (5590, -1),
      (5591, -1),
      (5592, -1),
      (5591, 5589),
      (5591, 5592),
      (5591, -1))
      //Hard2
     {((569, -1),
      (570, -1),
      (571, -1),
      (572, -1),
      (573, 574),
      (575, 576),
      (577, -1)),}
      //Hard3
     {((569, -1),
      (570, -1),
      (571, -1),
      (572, -1),
      (571, 569),
      (571, 572),
      (571, -1))}
      );

  // Does masks apply Walkable/Buildable restrictions on tile.
  // F.e. mt_2Corner mask does not add any restrictions
//  TILE_MASKS_PASS_RESTRICTIONS: array[mt_2Straight..mt_4Square] of array[TKMTileMaskSubType]
//                            of array[0..1] of Byte =  // (Walkable, Buildable) (0,1): 0 = False/1 = True
//     (((0,1), (0,0)),  // mt_2Straight
//      ((1,1), (0,0)),  // mt_2Diagonal
//      ((0,0), (0,0)),  // mt_2Corner
//      ((0,1), (0,0)),  // mt_2Opposite
//      ((0,0), (0,1)),  // mt_3Straight
//      ((0,0), (0,1)),  // mt_3Opposite
//      ((0,0), (0,0))); // mt_4Square


  TERRAIN_EQUALITY_PAIRS: array[0..1] of record
      TK1, TK2: TKMTerrainKind;
    end =
      (
//        (TK1: tkGold; TK2: tkGoldMount),
//        (TK1: tkIron; TK2: tkIronMount),
        (TK1: tkWater; TK2: tkFastWater),
        (TK1: tkSnowOnGrass; TK2: tkSnowOnDirt)
      );


  TILE_CORNERS_TERRAIN_KINDS: array [0..MAX_TILE_TO_SHOW-1]
                  of array[0..3] //Corners: LeftTop - RightTop - RightBottom - LeftBottom
                    of TKMTerrainKind = (
  (tkGrass,tkGrass,tkGrass,tkGrass), (tkGrass,tkGrass,tkGrass,tkGrass), (tkGrass,tkGrass,tkGrass,tkGrass),
  (tkGrass,tkGrass,tkGrass,tkGrass),
   //4
  (tkIce,tkIce,tkSnow,tkSnow),
  (tkGrass,tkGrass,tkGrass,tkGrass), (tkGrass,tkGrass,tkGrass,tkGrass),
   //7
  (tkLava,tkLava,tkLava,tkLava),
   //8
  (tkMoss,tkMoss,tkMoss,tkMoss), (tkMoss,tkMoss,tkMoss,tkMoss),
  //10
  (tkSnow,tkIce,tkSnow,tkSnow),      (tkGrass,tkGrass,tkGrass,tkGrass), (tkIce,tkIce,tkWater,tkWater),
  (tkGrass,tkGrass,tkGrass,tkGrass), (tkGrass,tkGrass,tkGrass,tkGrass), (tkGoldMount,tkLava,tkLava,tkLava),
   //16
  (tkPaleGrass,tkPaleGrass,tkPaleGrass,tkPaleGrass), (tkPaleGrass,tkPaleGrass,tkPaleGrass,tkPaleGrass),
  (tkGrass,tkGrass,tkMoss,tkMoss),   (tkMoss,tkGrass,tkMoss,tkMoss),    //??? not sure if they are good there
   //20
  (tkGravel,tkGravel,tkGravel,tkGravel), (tkGravel,tkGravel,tkGravel,tkGravel), (tkWater,tkIce,tkWater,tkWater),
  (tkIce,tkIce,tkIce,tkWater),           (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
   //26
  (tkGrassSand1,tkGrassSand1,tkGrassSand1,tkGrassSand1), (tkGrassSand2,tkGrassSand2,tkGrassSand2,tkGrassSand2),
  (tkGrassSand3,tkGrassSand3,tkGrassSand3,tkGrassSand3), (tkSand,tkSand,tkSand,tkSand),
   //30
  (tkSand,tkSand,tkSand,tkSand),                         (tkCoastSand,tkCoastSand,tkCoastSand,tkCoastSand),
  (tkCoastSand,tkCoastSand,tkCoastSand,tkCoastSand),     (tkCoastSand,tkCoastSand,tkCoastSand,tkCoastSand),
   //34
  (tkGrassDirt,tkGrassDirt,tkGrassDirt,tkGrassDirt),     (tkDirt,tkDirt,tkDirt,tkDirt), (tkDirt,tkDirt,tkDirt,tkDirt),
  (tkDirt,tkDirt,tkDirt,tkDirt),  (tkDirt,tkCobbleStone,tkDirt,tkDirt), (tkCobbleStone,tkCobbleStone,tkDirt,tkDirt),
   //40
  (tkSwamp,tkSwamp,tkSwamp,tkSwamp), (tkSwamp,tkSwamp,tkSwamp,tkSwamp), (tkSwamp,tkSwamp,tkSwamp,tkSwamp), (tkSwamp,tkSwamp,tkSwamp,tkSwamp),
  (tkIce,tkIce,tkIce,tkIce), (tkDeepSnow,tkDeepSnow,tkDeepSnow,tkDeepSnow), (tkSnow,tkSnow,tkSnow,tkSnow),
  (tkSnowOnDirt,tkSnowOnDirt,tkSnowOnDirt,tkSnowOnDirt),
   //48
  (tkGrassyWater,tkGrassyWater,tkGrassyWater,tkGrassyWater), (tkSnowOnDirt,tkSnowOnDirt,tkSnowOnDirt,tkGoldMount),
  (tkAbyss,tkAbyss,tkIronMount,tkIronMount),                 (tkGoldMount,tkSnowOnDirt,tkGoldMount,tkGoldMount),
   //52
  (tkSnow,tkIronMount,tkSnow,tkSnow), (tkIronMount,tkIronMount,tkIronMount,tkAbyss), (tkIronMount,tkIronMount,tkIronMount,tkSnow),
  (tkCustom,tkCustom,tkCustom,tkCustom), // Wine
   //56
  (tkGrass,tkDirt,tkGrass,tkGrass),(tkDirt,tkDirt,tkGrass,tkGrass), (tkDirt,tkDirt,tkDirt,tkGrass),
  (tkCustom,tkCustom,tkCustom,tkCustom), // Corn
   //60
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), // Corn
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), // Corn
   //64
  (tkSnowOnDirt,tkSnowOnDirt,tkDirt,tkDirt), (tkSnowOnDirt,tkSnowOnDirt,tkSnowOnDirt,tkDirt),
   //66
  (tkGrass,tkPaleGrass,tkGrass,tkGrass), (tkPaleGrass,tkPaleGrass,tkGrass,tkGrass), (tkPaleGrass,tkPaleGrass,tkPaleGrass,tkGrass),
   //69
  (tkGrass,tkCoastSand,tkGrass,tkGrass), (tkCoastSand,tkCoastSand,tkGrass,tkGrass), (tkCoastSand,tkCoastSand,tkCoastSand,tkGrass),
   //72
  (tkGrass,tkGrassSand1,tkGrass,tkGrass), (tkGrassSand1,tkGrassSand1,tkGrass,tkGrass), (tkGrassSand1,tkGrassSand1,tkGrassSand1,tkGrass),
   //75
  (tkGrassSand1,tkGrassSand2,tkGrassSand1,tkGrassSand1),(tkGrassSand2,tkGrassSand2,tkGrassSand1,tkGrassSand1),(tkGrassSand2,tkGrassSand2,tkGrassSand2,tkGrassSand1),
   //78
  (tkGrassSand2,tkGrassSand3,tkGrassSand2,tkGrassSand2),(tkGrassSand3,tkGrassSand3,tkGrassSand2,tkGrassSand2),(tkGrassSand3,tkGrassSand3,tkGrassSand3,tkGrassSand2),
   //81
  (tkGrassSand2,tkSand,tkGrassSand3,tkGrassSand3),(tkSand,tkSand,tkGrassSand3,tkGrassSand3),(tkSand,tkSand,tkSand,tkGrassSand3),
   //84
  (tkGrass,tkGrassDirt,tkGrass,tkGrass), (tkGrassDirt,tkGrassDirt,tkGrass,tkGrass), (tkGrassDirt,tkGrassDirt,tkGrassDirt,tkGrass),
   //87
  (tkGrassDirt,tkDirt,tkGrassDirt,tkGrassDirt), (tkDirt,tkDirt,tkGrassDirt,tkGrassDirt), (tkDirt,tkDirt,tkDirt,tkGrassDirt),
   //90
  (tkGrass,tkSwamp,tkGrass,tkGrass), (tkSwamp,tkSwamp,tkGrass,tkGrass), (tkSwamp,tkSwamp,tkSwamp,tkGrass),
   //93
  (tkGrass,tkGrassSand3,tkGrass,tkGrass), (tkGrassSand3,tkGrassSand3,tkGrass,tkGrass), (tkGrassSand3,tkGrassSand3,tkGrassSand3,tkGrass),
   //96
  (tkGrassDirt,tkPaleGrass,tkGrassDirt,tkGrassDirt), (tkPaleGrass,tkPaleGrass,tkGrassDirt,tkGrassDirt), (tkPaleGrass,tkPaleGrass,tkPaleGrass,tkGrassDirt),
   //99
  (tkCoastSand,tkSand,tkCoastSand,tkCoastSand), (tkSand,tkSand,tkCoastSand,tkCoastSand), (tkSand,tkSand,tkSand,tkCoastSand),
   //102
  (tkCoastSand,tkGrassSand2,tkCoastSand,tkCoastSand),(tkGrassSand2,tkGrassSand2,tkCoastSand,tkCoastSand),(tkGrassSand2,tkGrassSand2,tkGrassSand2,tkCoastSand),
   //105
  (tkWater,tkDirt,tkWater,tkWater), (tkDirt,tkDirt,tkWater,tkWater), (tkDirt,tkDirt,tkDirt,tkWater),
   //108
  (tkCoastSand,tkIronMount,tkCoastSand,tkCoastSand),(tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),(tkIronMount,tkIronMount,tkIronMount,tkCoastSand),
   //111
  (tkDirt,tkCoastSand,tkDirt,tkDirt), (tkCoastSand,tkCoastSand,tkDirt,tkDirt), (tkCoastSand,tkCoastSand,tkCoastSand,tkDirt),
   //114
  (tkGrassyWater,tkWater,tkGrassyWater,tkGrassyWater), (tkWater,tkWater,tkGrassyWater,tkGrassyWater),
   //116
  (tkCoastSand,tkWater,tkCoastSand,tkCoastSand), (tkCoastSand,tkCoastSand,tkWater,tkWater), (tkWater,tkWater,tkWater,tkCoastSand),
   //119
  (tkWater,tkWater,tkWater,tkGrassyWater),
   //120
  (tkGrass,tkGrassyWater,tkGrass,tkGrass), (tkGrassyWater,tkGrassyWater,tkGrass,tkGrass), (tkGrassyWater,tkGrassyWater,tkGrassyWater,tkGrass),
   //123
  (tkGrass,tkWater,tkGrass,tkGrass), (tkGrass,tkGrass,tkWater,tkWater), (tkGrass,tkGrass,tkWater,tkWater),
  (tkWater,tkWater,tkWater,tkGrass), (tkWater,tkWater,tkWater,tkGrass),
   //128
  (tkStone,tkStone,tkStone,tkStone),(tkStone,tkStone,tkStone,tkStone),(tkStone,tkStone,tkStone,tkStone),
  (tkStone,tkStone,tkStone,tkStone),(tkStone,tkStone,tkStone,tkStone),(tkStone,tkStone,tkStone,tkStone),
  (tkStone,tkStone,tkStone,tkStone),(tkStone,tkStone,tkStone,tkStone),(tkStone,tkStone,tkStone,tkStone),
  (tkStone,tkStone,tkStone,tkStone),
   //138
  (tkStone,tkStone,tkStone,tkGrass), (tkStone,tkStone,tkGrass,tkGrass),
   //140
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
   //142
  (tkStone,tkStone,tkWater,tkStone), (tkStone,tkStone,tkStone,tkWater),
   //144
  (tkGoldMount,tkGold,tkGoldMount,tkGoldMount),(tkGold,tkGold,tkGoldMount,tkGoldMount), (tkGold,tkGold,tkGold,tkGoldMount),
   //147
  (tkGold,tkGold,tkGold,tkGold),
   //148
  (tkIronMount,tkIron,tkIronMount,tkIronMount), (tkIron,tkIron,tkIronMount,tkIronMount), (tkIron,tkIron,tkIron,tkIronMount),
   //151
  (tkIron,tkIron,tkIron,tkIron),
   //152
  (tkDirt,tkCoal,tkDirt,tkDirt), (tkCoal,tkCoal,tkDirt,tkDirt), (tkCoal,tkCoal,tkCoal,tkDirt),
   //155
  (tkCoal,tkCoal,tkCoal,tkCoal),
   //156
  (tkGoldMount,tkGoldMount,tkGoldMount,tkGoldMount), (tkGoldMount,tkGoldMount,tkGoldMount,tkGoldMount),
  (tkGoldMount,tkGoldMount,tkGoldMount,tkGoldMount), (tkGoldMount,tkGoldMount,tkGoldMount,tkGoldMount),
   //160
  (tkIronMount,tkIronMount,tkIronMount,tkIronMount), (tkIronMount,tkIronMount,tkIronMount,tkIronMount),
  (tkIronMount,tkIronMount,tkIronMount,tkIronMount), (tkIronMount,tkIronMount,tkIronMount,tkIronMount),
  (tkIronMount,tkIronMount,tkIronMount,tkIronMount),
   //165
  (tkAbyss,tkIronMount,tkAbyss,tkAbyss),
   //166
  (tkIronMount,tkIronMount,tkSnow,tkSnow), (tkIronMount,tkIronMount,tkDirt,tkDirt),
   //168
  (tkIronMount,tkIronMount,tkGrass,tkGrass), (tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),
   //170
  (tkIronMount,tkIronMount,tkGrassSand2,tkGrassSand2),
   //171
  (tkGoldMount,tkGoldMount,tkSnowOnDirt,tkSnowOnDirt), (tkGoldMount,tkGoldMount,tkGrass,tkGrass),
   //173
  (tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand), (tkGoldMount,tkGoldMount,tkGrassSand2,tkGrassSand2),
  (tkGoldMount,tkGoldMount,tkDirt,tkDirt),
   //176
  (tkGoldMount,tkGoldMount,tkGoldMount,tkGrass),(tkGoldMount,tkGoldMount,tkGoldMount,tkCoastSand),
  (tkGoldMount,tkGoldMount,tkGoldMount,tkGrassSand2), (tkGoldMount,tkGoldMount,tkGoldMount,tkDirt),
   //180
  (tkGrass,tkGoldMount,tkGrass,tkGrass), (tkCoastSand,tkGoldMount,tkCoastSand,tkCoastSand),
  (tkGrassSand2,tkGoldMount,tkGrassSand2,tkGrassSand2), (tkDirt,tkGoldMount,tkDirt,tkDirt),
   //184
  (tkIronMount,tkIronMount,tkIronMount,tkGrass), (tkIronMount,tkCoastSand,tkIronMount,tkIronMount),
  (tkIronMount,tkGrassSand2,tkIronMount,tkIronMount), (tkIronMount,tkIronMount,tkIronMount,tkDirt),
   //188
  (tkGrass,tkIronMount,tkGrass,tkGrass), (tkCoastSand,tkIronMount,tkCoastSand,tkCoastSand),
   //190
  (tkGrassSand2,tkIronMount,tkGrassSand2,tkGrassSand2), (tkDirt,tkIronMount,tkDirt,tkDirt),
   //192
  (tkWater,tkWater,tkWater,tkWater), (tkWater,tkWater,tkWater,tkWater), (tkWater,tkWater,tkWater,tkWater),
   //195
  (tkStone,tkStone,tkStone,tkStone), (tkWater,tkWater,tkWater,tkWater),
   //197
  (tkCobbleStone,tkCobbleStone,tkCobbleStone,tkCobbleStone),
  (tkCustom,tkCustom,tkCustom,tkWater), (tkCustom,tkCustom,tkWater,tkCustom),
   //200
  (tkWater,tkWater,tkWater,tkWater),//(?)
  (tkGoldMount,tkGoldMount,tkGoldMount,tkGoldMount), (tkCustom,tkCustom,tkCustom,tkCustom),
   //203
  (tkSnow,tkDeepSnow,tkSnow,tkSnow), (tkDeepSnow,tkDeepSnow,tkSnow,tkSnow), (tkDeepSnow,tkDeepSnow,tkDeepSnow,tkSnow),
   //206
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
   //208
  (tkWater,tkWater,tkWater,tkWater), (tkFastWater,tkFastWater,tkFastWater,tkFastWater),
   //210
  (tkWater,tkWater,tkWater,tkWater),(tkWater,tkWater,tkWater,tkWater),//(?)
   //212
  (tkSnow,tkSnow,tkSnowOnDirt,tkSnowOnDirt), (tkSnow,tkSnow,tkSnow,tkSnowOnDirt),
   //214
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCobbleStone,tkCobbleStone,tkCobbleStone,tkCobbleStone),
   //216
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
   //220
  (tkSnowOnDirt,tkSnow,tkSnowOnDirt,tkSnowOnDirt),
   //221
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
   //230
  (tkCustom,tkCustom,tkWater,tkWater), (tkCustom,tkCustom,tkAbyss,tkAbyss),
  (tkCustom,tkCustom,tkWater,tkWater), (tkCustom,tkCustom,tkWater,tkWater),
   //234
  (tkGoldMount,tkGoldMount,tkWater,tkGoldMount), (tkGoldMount,tkWater,tkWater,tkWater),
  (tkWater,tkGoldMount,tkWater,tkWater), (tkGoldMount,tkGoldMount,tkGoldMount,tkWater),
   //238
  (tkIronMount,tkIronMount,tkWater,tkIronMount), (tkIronMount,tkWater,tkIronMount,tkIronMount),
   //240
  (tkWater,tkWater,tkWater,tkWater),
   //241
  (tkWater, tkGrassSand2,tkWater,tkWater), (tkGrassSand2,tkGrassSand2,tkWater,tkWater), (tkGrassSand2,tkGrassSand2,tkGrassSand2,tkWater),
   //244
  (tkFastWater,tkFastWater,tkFastWater,tkFastWater), (tkAbyss,tkAbyss,tkAbyss,tkAbyss), (tkCustom,tkCustom,tkCustom,tkCustom),
   //247
  (tkDirt,tkSnowOnDirt,tkDirt,tkDirt),
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
   //256
  (tkSnowOnDirt,tkIronMount,tkSnowOnDirt,tkSnowOnDirt),(tkIronMount,tkIronMount,tkSnowOnDirt,tkSnowOnDirt), (tkIronMount,tkIronMount,tkIronMount,tkSnowOnDirt),
   //259
  (tkIron,tkIron,tkIron,tkIron), (tkIron,tkIron,tkIron,tkIron),
   //261
  (tkSnow,tkGoldMount,tkSnow,tkSnow), (tkGoldMount,tkGoldMount,tkSnow,tkSnow),
   //263
  (tkCoal,tkCoal,tkCoal,tkCoal), (tkCustom,tkCustom,tkCustom,tkIce), (tkCustom,tkCustom,tkIce,tkCustom),
   //266
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkStone,tkStone,tkStone,tkCoastSand), (tkStone,tkStone,tkCoastSand,tkCoastSand),
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkCoastSand,tkStone,tkCoastSand,tkCoastSand),
   //274
  (tkGrass,tkStone,tkGrass,tkGrass),
   //275
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkStone,tkStone,tkStone,tkDirt), (tkStone,tkStone,tkDirt,tkDirt),
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkDirt,tkStone,tkDirt,tkDirt),
   //283
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkStone,tkStone,tkStone,tkSnow), (tkStone,tkStone,tkSnow,tkSnow),
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkSnow,tkStone,tkSnow,tkSnow),
   //291
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkStone,tkStone,tkStone,tkSnowOnDirt), (tkStone,tkStone,tkSnowOnDirt,tkSnowOnDirt),
  (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone), (tkStone,tkStone,tkStone,tkStone),
  (tkSnowOnDirt,tkStone,tkSnowOnDirt,tkSnowOnDirt),
   //299
  (tkGoldMount,tkIronMount,tkGoldMount,tkGoldMount), (tkIronMount,tkIronMount,tkLava,tkIronMount),
   //301
  (tkStone,tkStone,tkGrass,tkGrass), (tkStone,tkStone,tkCoastSand,tkCoastSand),
  (tkStone,tkStone,tkDirt,tkDirt),
   //304
  (tkStone,tkStone,tkSnow,tkSnow), (tkStone,tkStone,tkSnowOnDirt,tkSnowOnDirt),(tkGoldMount,tkGoldMount,tkGoldMount,tkSnow),
   //307
  (tkGold,tkGold,tkGold,tkGold),
   //308
  (tkStone,tkStone,tkDirt,tkDirt),(tkStone,tkStone,tkStone,tkDirt),
   //310
  (tkStone,tkStone,tkStone,tkStone),(tkStone,tkStone,tkStone,tkStone),
   //312
  (tkSnowOnGrass,tkSnowOnDirt,tkSnowOnGrass,tkSnowOnGrass),(tkSnowOnDirt,tkSnowOnDirt,tkSnowOnGrass,tkSnowOnGrass),
  (tkSnowOnDirt,tkSnowOnDirt,tkSnowOnDirt,tkSnowOnGrass),
   //315
  (tkSnowOnGrass,tkSnowOnGrass,tkSnowOnGrass,tkSnowOnGrass),(tkGrass,tkSnowOnGrass,tkGrass,tkGrass),
  (tkSnowOnGrass,tkSnowOnGrass,tkGrass,tkGrass),(tkSnowOnGrass,tkSnowOnGrass,tkSnowOnGrass,tkGrass),
   //319
  (tkCoastSand,tkGrassSand3,tkCoastSand,tkCoastSand),(tkGrassSand3,tkGrassSand3,tkCoastSand,tkCoastSand),(tkGrassSand3,tkGrassSand3,tkGrassSand3,tkCoastSand),
   //322
  (tkGoldMount,tkIronMount,tkGoldMount,tkGoldMount),(tkIronMount,tkIronMount,tkGoldMount,tkGoldMount),(tkIronMount,tkIronMount,tkIronMount,tkGoldMount),
   //325
  (tkGold,tkIron,tkGold,tkGold),(tkIron,tkIron,tkGold,tkGold),(tkIron,tkIron,tkIron,tkGold),
   //328
  (tkIronMount,tkIron,tkIronMount,tkIronMount),(tkIron,tkIron,tkIronMount,tkIronMount),(tkIron,tkIron,tkIron,tkIronMount),
   //331
  (tkStone,tkIronMount,tkStone,tkStone),(tkIronMount,tkIronMount,tkStone,tkStone),(tkIronMount,tkIronMount,tkIronMount,tkStone),
   //334
  (tkStone,tkIron,tkStone,tkStone),(tkIron,tkIron,tkStone,tkStone),(tkIron,tkIron,tkIron,tkStone),
   //337
  (tkGrass,tkIron,tkGrass,tkGrass),(tkIron,tkIron,tkGrass,tkGrass),(tkIron,tkIron,tkIron,tkGrass),
   //340
  (tkStone,tkGoldMount,tkStone,tkStone),(tkGoldMount,tkGoldMount,tkStone,tkStone),(tkGoldMount,tkGoldMount,tkGoldMount,tkStone),
   //343
  (tkStone,tkGold,tkStone,tkStone),(tkGold,tkGold,tkStone,tkStone),(tkGold,tkGold,tkGold,tkStone),
   //346
  (tkGoldMount,tkAbyss,tkGoldMount,tkGoldMount),(tkAbyss,tkAbyss,tkGoldMount,tkGoldMount),(tkAbyss,tkAbyss,tkAbyss,tkGoldMount),
   //349
  (tkCustom,tkCustom,tkCustom,tkCustom), (tkCustom,tkCustom,tkCustom,tkCustom),
   //351
  (tkGrassDirt,tkGoldMount,tkGrassDirt,tkGrassDirt), (tkGoldMount,tkGoldMount,tkGrassDirt,tkGrassDirt), (tkGoldMount,tkGoldMount,tkGoldMount,tkGrassDirt),
   //354
  (tkGrassDirt,tkIronMount,tkGrassDirt,tkGrassDirt), (tkIronMount,tkIronMount,tkGrassDirt,tkGrassDirt), (tkIronMount,tkIronMount,tkIronMount,tkGrassDirt),
   //357
  (tkGoldMount,tkGoldMount,tkDirt,tkGrass), (tkGoldMount,tkGoldMount,tkGrass,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkGrass), (tkGoldMount,tkGoldMount,tkGrass,tkDirt),
   //361
  (tkGrass,tkGoldMount,tkDirt,tkGrass), (tkDirt,tkGoldMount,tkGrass,tkDirt), (tkGrass,tkGoldMount,tkDirt,tkDirt), (tkDirt,tkGoldMount,tkGrass,tkDirt),
   //365
  (tkGoldMount,tkDirt,tkDirt,tkGrass), (tkGoldMount,tkGrass,tkGrass,tkDirt), (tkGoldMount,tkGrass,tkDirt,tkDirt), (tkGoldMount,tkDirt,tkDirt,tkGrass),
   //369
  (tkIronMount,tkIronMount,tkDirt,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkDirt),
   //373
  (tkGrass,tkIronMount,tkDirt,tkGrass), (tkDirt,tkIronMount,tkGrass,tkDirt), (tkGrass,tkIronMount,tkDirt,tkDirt), (tkDirt,tkIronMount,tkGrass,tkDirt),
   //377
  (tkIronMount,tkDirt,tkDirt,tkGrass), (tkIronMount,tkGrass,tkGrass,tkDirt), (tkIronMount,tkDirt,tkDirt,tkGrass), (tkIronMount,tkGrass,tkDirt,tkDirt),
   //381
  (tkGoldMount,tkGoldMount,tkGrass,tkGrass), (tkGoldMount,tkGoldMount,tkDirt,tkDirt), (tkDirt,tkGoldMount,tkGrass,tkGrass), (tkGrass,tkGoldMount,tkDirt,tkDirt),
   //385
  (tkGoldMount,tkGrass,tkDirt,tkDirt),(tkGoldMount,tkDirt,tkGrass,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkGrass), (tkIronMount,tkIronMount,tkDirt,tkDirt),
   //389
  (tkDirt,tkIronMount,tkGrass,tkGrass), (tkIronMount,tkDirt,tkGrass,tkGrass), (tkGrass,tkIronMount,tkDirt,tkDirt),(tkIronMount,tkGrass,tkDirt,tkDirt),
   //393
  (tkGoldMount,tkGoldMount,tkDirt,tkGrass), (tkGoldMount,tkGoldMount,tkGrass,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkDirt),(tkGoldMount,tkGoldMount,tkDirt,tkDirt),
   //397
  (tkDirt,tkGoldMount,tkDirt,tkGrass), (tkDirt,tkGoldMount,tkGrass,tkDirt), (tkGrass,tkGoldMount,tkDirt,tkDirt),(tkDirt,tkGoldMount,tkDirt,tkDirt),
   //401
  (tkGoldMount,tkDirt,tkDirt,tkGrass), (tkGoldMount,tkDirt,tkGrass,tkDirt), (tkGoldMount,tkDirt,tkDirt,tkDirt),(tkGoldMount,tkGrass,tkDirt,tkDirt),
   //405
  (tkIronMount,tkIronMount,tkDirt,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkDirt),(tkIronMount,tkIronMount,tkDirt,tkDirt),
   //409
  (tkDirt,tkIronMount,tkDirt,tkGrass), (tkDirt,tkIronMount,tkGrass,tkDirt), (tkGrass,tkIronMount,tkDirt,tkDirt),(tkDirt,tkIronMount,tkDirt,tkDirt),
   //413
  (tkIronMount,tkDirt,tkDirt,tkGrass), (tkIronMount,tkDirt,tkGrass,tkDirt), (tkIronMount,tkDirt,tkDirt,tkDirt),(tkIronMount,tkGrass,tkDirt,tkDirt),
   //417
  (tkGoldMount,tkGoldMount,tkDirt,tkDirt),(tkGoldMount,tkGoldMount,tkSnowOnDirt,tkSnowOnDirt), (tkSnowOnDirt,tkGoldMount,tkDirt,tkDirt), (tkGoldMount,tkSnowOnDirt,tkDirt,tkDirt),
   //421
  (tkDirt,tkGoldMount,tkSnowOnDirt,tkSnowOnDirt), (tkGoldMount,tkDirt,tkSnowOnDirt,tkSnowOnDirt), (tkIronMount,tkIronMount,tkDirt,tkDirt),(tkIronMount,tkIronMount,tkSnowOnDirt,tkSnowOnDirt),
   //425
  (tkSnowOnDirt,tkIronMount,tkDirt,tkDirt), (tkIronMount,tkSnowOnDirt,tkDirt,tkDirt), (tkDirt,tkIronMount,tkSnowOnDirt,tkSnowOnDirt), (tkIronMount,tkDirt,tkSnowOnDirt,tkSnowOnDirt),
   //429
  (tkGoldMount,tkGoldMount,tkSnowOnDirt,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkSnowOnDirt), (tkGoldMount,tkGoldMount,tkSnowOnDirt,tkSnowOnDirt), (tkGoldMount,tkGoldMount,tkSnowOnDirt,tkSnowOnDirt),
   //433
  (tkSnowOnDirt,tkGoldMount,tkSnowOnDirt,tkDirt), (tkSnowOnDirt,tkGoldMount,tkDirt,tkSnowOnDirt), (tkSnowOnDirt,tkGoldMount,tkSnowOnDirt,tkSnowOnDirt), (tkDirt,tkGoldMount,tkSnowOnDirt,tkSnowOnDirt),
   //437
  (tkGoldMount,tkSnowOnDirt,tkSnowOnDirt,tkDirt), (tkGoldMount,tkSnowOnDirt,tkDirt,tkSnowOnDirt), (tkGoldMount,tkSnowOnDirt,tkSnowOnDirt,tkSnowOnDirt), (tkGoldMount,tkDirt,tkSnowOnDirt,tkSnowOnDirt),
   //441
   (tkIronMount,tkIronMount,tkSnowOnDirt,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkSnowOnDirt), (tkIronMount,tkIronMount,tkSnowOnDirt,tkSnowOnDirt), (tkIronMount,tkIronMount,tkSnowOnDirt,tkSnowOnDirt),
   //445
  (tkSnowOnDirt,tkIronMount,tkSnowOnDirt,tkDirt), (tkSnowOnDirt,tkIronMount,tkDirt,tkSnowOnDirt), (tkSnowOnDirt,tkIronMount,tkSnowOnDirt,tkSnowOnDirt), (tkDirt,tkIronMount,tkSnowOnDirt,tkSnowOnDirt),
   //449
  (tkIronMount,tkSnowOnDirt,tkSnowOnDirt,tkDirt), (tkIronMount,tkSnowOnDirt,tkDirt,tkSnowOnDirt), (tkIronMount,tkSnowOnDirt,tkSnowOnDirt,tkSnowOnDirt), (tkIronMount,tkDirt,tkSnowOnDirt,tkSnowOnDirt),
   //453
  (tkGoldMount,tkGoldMount,tkGrass,tkGrass), (tkGoldMount,tkGoldMount,tkGrass,tkGrass), (tkGoldMount,tkGoldMount,tkCoastSand,tkGrass), (tkGoldMount,tkGoldMount,tkGrass,tkCoastSand),
   //457
  (tkGrass,tkGoldMount,tkGrass,tkGrass), (tkCoastSand,tkGoldMount,tkGrass,tkGrass), (tkGrass,tkGoldMount,tkCoastSand,tkGrass), (tkGrass,tkGoldMount,tkGrass,tkCoastSand),
   //461
  (tkGoldMount,tkCoastSand,tkGrass,tkGrass), (tkGoldMount,tkGrass,tkGrass,tkGrass), (tkGoldMount,tkGrass,tkGrass,tkCoastSand), (tkGoldMount,tkGrass,tkCoastSand,tkGrass),
   //465
  (tkIronMount,tkIronMount,tkGrass,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkGrass), (tkIronMount,tkIronMount,tkCoastSand,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkCoastSand),
   //469
  (tkGrass,tkIronMount,tkGrass,tkGrass), (tkCoastSand,tkIronMount,tkGrass,tkGrass), (tkGrass,tkIronMount,tkCoastSand,tkGrass), (tkGrass,tkIronMount,tkGrass,tkCoastSand),
   //473
  (tkIronMount,tkCoastSand,tkGrass,tkGrass), (tkIronMount,tkGrass,tkGrass,tkGrass), (tkIronMount,tkGrass,tkGrass,tkCoastSand), (tkIronMount,tkGrass,tkCoastSand,tkGrass),
   //477
  (tkGoldMount,tkGoldMount,tkGrass,tkGrass), (tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand), (tkCoastSand,tkGoldMount,tkGrass,tkGrass), (tkGrass,tkGoldMount,tkCoastSand,tkCoastSand),
   //481
  (tkGoldMount,tkGrass,tkCoastSand,tkCoastSand),(tkGoldMount,tkCoastSand,tkGrass,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkGrass), (tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),
   //485
  (tkCoastSand,tkIronMount,tkGrass,tkGrass), (tkIronMount,tkCoastSand,tkGrass,tkGrass), (tkGrass,tkIronMount,tkCoastSand,tkCoastSand),(tkIronMount,tkGrass,tkCoastSand,tkCoastSand),
   //489
  (tkGoldMount,tkGoldMount,tkCoastSand,tkGrass), (tkGoldMount,tkGoldMount,tkGrass,tkCoastSand), (tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand),(tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand),
   //493
  (tkCoastSand,tkGoldMount,tkCoastSand,tkGrass), (tkCoastSand,tkGoldMount,tkGrass,tkCoastSand), (tkGrass,tkGoldMount,tkCoastSand,tkCoastSand),(tkCoastSand,tkGoldMount,tkCoastSand,tkCoastSand),
   //497
  (tkGoldMount,tkCoastSand,tkCoastSand,tkGrass), (tkGoldMount,tkCoastSand,tkGrass,tkCoastSand), (tkGoldMount,tkCoastSand,tkCoastSand,tkCoastSand),(tkGoldMount,tkGrass,tkCoastSand,tkCoastSand),
   //501
  (tkIronMount,tkIronMount,tkCoastSand,tkGrass), (tkIronMount,tkIronMount,tkGrass,tkCoastSand), (tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),(tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),
   //505
  (tkCoastSand,tkIronMount,tkCoastSand,tkGrass), (tkCoastSand,tkIronMount,tkGrass,tkCoastSand), (tkGrass,tkIronMount,tkCoastSand,tkCoastSand),(tkCoastSand,tkIronMount,tkCoastSand,tkCoastSand),
   //509
  (tkIronMount,tkCoastSand,tkCoastSand,tkGrass), (tkIronMount,tkCoastSand,tkGrass,tkCoastSand), (tkIronMount,tkCoastSand,tkCoastSand,tkCoastSand),(tkIronMount,tkGrass,tkCoastSand,tkCoastSand),
   //513
  (tkGoldMount,tkGoldMount,tkDirt,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkDirt), (tkGoldMount,tkGoldMount,tkCoastSand,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkCoastSand),
   //517
  (tkDirt,tkGoldMount,tkDirt,tkDirt), (tkCoastSand,tkGoldMount,tkDirt,tkDirt), (tkDirt,tkGoldMount,tkCoastSand,tkDirt), (tkDirt,tkGoldMount,tkDirt,tkCoastSand),
   //521
  (tkGoldMount,tkCoastSand,tkDirt,tkDirt), (tkGoldMount,tkDirt,tkDirt,tkDirt), (tkGoldMount,tkDirt,tkDirt,tkCoastSand), (tkGoldMount,tkDirt,tkCoastSand,tkDirt),
   //525
  (tkIronMount,tkIronMount,tkDirt,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkDirt), (tkIronMount,tkIronMount,tkCoastSand,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkCoastSand),
   //529
  (tkDirt,tkIronMount,tkDirt,tkDirt), (tkCoastSand,tkIronMount,tkDirt,tkDirt), (tkDirt,tkIronMount,tkCoastSand,tkDirt), (tkDirt,tkIronMount,tkDirt,tkCoastSand),
   //533
  (tkIronMount,tkCoastSand,tkDirt,tkDirt), (tkIronMount,tkDirt,tkDirt,tkDirt), (tkIronMount,tkDirt,tkDirt,tkCoastSand), (tkIronMount,tkDirt,tkCoastSand,tkDirt),
   //537
  (tkGoldMount,tkDirt,tkCoastSand,tkCoastSand),(tkGoldMount,tkCoastSand,tkDirt,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkDirt), (tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),
   //541
  (tkCoastSand,tkIronMount,tkDirt,tkDirt), (tkIronMount,tkCoastSand,tkDirt,tkDirt), (tkDirt,tkIronMount,tkCoastSand,tkCoastSand),(tkIronMount,tkDirt,tkCoastSand,tkCoastSand),
   //545
  (tkGoldMount,tkGoldMount,tkCoastSand,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkCoastSand), (tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand),(tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand),
   //549
   (tkGoldMount,tkGoldMount,tkCoastSand,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkCoastSand), (tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand),(tkGoldMount,tkGoldMount,tkCoastSand,tkCoastSand),
   //553
  (tkCoastSand,tkGoldMount,tkCoastSand,tkDirt), (tkCoastSand,tkGoldMount,tkDirt,tkCoastSand), (tkDirt,tkGoldMount,tkCoastSand,tkCoastSand),(tkCoastSand,tkGoldMount,tkCoastSand,tkCoastSand),
   //557
  (tkGoldMount,tkCoastSand,tkCoastSand,tkDirt), (tkGoldMount,tkCoastSand,tkDirt,tkCoastSand), (tkGoldMount,tkCoastSand,tkCoastSand,tkCoastSand),(tkGoldMount,tkDirt,tkCoastSand,tkCoastSand),
   //561
  (tkIronMount,tkIronMount,tkCoastSand,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkCoastSand), (tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),(tkIronMount,tkIronMount,tkCoastSand,tkCoastSand),
   //565
  (tkCoastSand,tkIronMount,tkCoastSand,tkDirt), (tkCoastSand,tkIronMount,tkDirt,tkCoastSand), (tkDirt,tkIronMount,tkCoastSand,tkCoastSand),(tkCoastSand,tkIronMount,tkCoastSand,tkCoastSand),
   //569
  (tkIronMount,tkCoastSand,tkCoastSand,tkDirt), (tkIronMount,tkCoastSand,tkDirt,tkCoastSand), (tkIronMount,tkCoastSand,tkCoastSand,tkCoastSand),(tkIronMount,tkDirt,tkCoastSand,tkCoastSand),
   //573
  (tkGoldMount,tkGoldMount,tkDirt,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkDirt), (tkGoldMount,tkGoldMount,tkSnowOnDirt,tkDirt), (tkGoldMount,tkGoldMount,tkDirt,tkSnowOnDirt),
   //577
  (tkDirt,tkGoldMount,tkDirt,tkDirt), (tkSnowOnDirt,tkGoldMount,tkDirt,tkDirt), (tkDirt,tkGoldMount,tkSnowOnDirt,tkDirt), (tkDirt,tkGoldMount,tkDirt,tkSnowOnDirt),
   //579
  (tkGoldMount,tkSnowOnDirt,tkDirt,tkDirt), (tkGoldMount,tkDirt,tkDirt,tkDirt), (tkGoldMount,tkDirt,tkDirt,tkSnowOnDirt), (tkGoldMount,tkDirt,tkSnowOnDirt,tkDirt),
   //585
  (tkIronMount,tkIronMount,tkDirt,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkDirt), (tkIronMount,tkIronMount,tkSnowOnDirt,tkDirt), (tkIronMount,tkIronMount,tkDirt,tkSnowOnDirt),
   //589
  (tkDirt,tkIronMount,tkDirt,tkDirt), (tkSnowOnDirt,tkIronMount,tkDirt,tkDirt), (tkDirt,tkIronMount,tkSnowOnDirt,tkDirt), (tkDirt,tkIronMount,tkDirt,tkSnowOnDirt),
   //593
  (tkIronMount,tkSnowOnDirt,tkDirt,tkDirt), (tkIronMount,tkDirt,tkDirt,tkDirt), (tkIronMount,tkDirt,tkDirt,tkSnowOnDirt), (tkIronMount,tkDirt,tkSnowOnDirt,tkDirt)
  );

implementation

end.
