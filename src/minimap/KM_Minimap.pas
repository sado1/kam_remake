unit KM_Minimap;
{$I KaM_Remake.inc}
interface
uses
  {$IFNDEF NO_OGL}
  KromOGLUtils,
  {$ENDIF}
  KM_Alerts,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points;


type
  //Intermediary class between TTerrain/Players and UI
  TKMMinimap = class abstract
  private
    fSepia: Boolean; //Less saturated display for menu

    fAlerts: TKMAlerts;

    {$IFNDEF NO_OGL}
    fMapTex: TTexture;
    {$ENDIF}
    fWidthPOT: Word;
    fHeightPOT: Word;
    procedure ApplySepia;
    procedure UpdateTexture;
  protected
    //We need to store map properties locally since Minimaps come from various
    //sources which do not have Terrain in them (TMissionParserPreview, Stream)
    fMapY: Word;
    fMapX: Word;
    fBase: TKMCardinalArray; //Base terrain layer
    procedure Resize(aX, aY: Word);
    procedure DoUpdate(aRevealAll: Boolean); virtual; abstract;
  public
    HandColors: array [0..MAX_HANDS-1] of Cardinal;
    HandLocs: array [0..MAX_HANDS-1] of TKMPoint;
    HandShow: array [0..MAX_HANDS-1] of Boolean;
    HandTeam: array [0..MAX_HANDS-1] of ShortInt;
    constructor Create(aSepia: Boolean);

    property Alerts: TKMAlerts read fAlerts write fAlerts;
    property MapX: Word read fMapX;
    property MapY: Word read fMapY;
    {$IFNDEF NO_OGL}
    property MapTex: TTexture read fMapTex;
    {$ENDIF}
    property Base: TKMCardinalArray read fBase;

    procedure LoadFromStream(LoadStream: TKMemoryStream);
    procedure SaveToStream(SaveStream: TKMemoryStream);

    procedure Update(aRevealAll: Boolean = False);
  end;


implementation
uses
  Math,
  KromUtils,
  KM_Render, KM_RenderTypes;


{ TKMMinimap }
constructor TKMMinimap.Create(aSepia: Boolean);
begin
  inherited Create;

  fSepia := aSepia;
  {$IFNDEF NO_OGL}
  fMapTex.Tex := TRender.GenerateTextureCommon(ftNearest, ftNearest);
  {$ENDIF}
end;


procedure TKMMinimap.Resize(aX, aY: Word);
begin
  fMapX := aX;
  fMapY := aY;
  SetLength(fBase, fMapX * fMapY);
  fWidthPOT := MakePOT(fMapX);
  fHeightPOT := MakePOT(fMapY);
  {$IFNDEF NO_OGL}
  fMapTex.U := fMapX / fWidthPOT;
  fMapTex.V := fMapY / fHeightPOT;
  {$ENDIF}
end;


//Sepia method taken from:
//http://www.techrepublic.com/blog/howdoi/how-do-i-convert-images-to-grayscale-and-sepia-tone-using-c/120
procedure TKMMinimap.ApplySepia;
const
  SEPIA_VAL = 0.4;
var
  I: Integer;
  R, G, B, R2, G2, B2: Byte;
begin
  for I := 0 to fMapX * fMapY - 1 do
  begin
    //We split color to RGB values
    R := fBase[I] and $FF;
    G := fBase[I] shr 8 and $FF;
    B := fBase[I] shr 16 and $FF;

    //Apply sepia coefficients and merge back with SEPIA_VAL factor
    R2 := Min(Round(0.393 * R + 0.769 * G + 0.189 * B), 255);
    R2 := Mix(R2, R, SEPIA_VAL);

    G2 := Min(Round(0.349 * R + 0.686 * G + 0.168 * B), 255);
    G2 := Mix(G2, G, SEPIA_VAL);

    B2 := Min(Round(0.272 * R + 0.534 * G + 0.131 * B), 255);
    B2 := Mix(B2, B, SEPIA_VAL);

    fBase[I] := (R2 + G2 shl 8 + B2 shl 16) or $FF000000;
  end;
end;


procedure TKMMinimap.Update(aRevealAll: Boolean = False);
begin
  if SKIP_RENDER then Exit;

  // No need to update if we did not initialize map sizes yet
  // F.e. when Cinematic starts in OnMissionStart script procedure in the replay
  if fMapX*fMapY = 0 then Exit;

  DoUpdate(aRevealAll);

  if fSepia then ApplySepia;

  UpdateTexture;
end;


procedure TKMMinimap.UpdateTexture;
var
  wData: Pointer;
  I: Word;
begin
  GetMem(wData, fWidthPOT * fHeightPOT * 4);

  if fMapY > 0 then //if MapY = 0 then loop will overflow to MaxWord
  for I := 0 to fMapY - 1 do
    Move(Pointer(NativeUint(fBase) + I * fMapX * 4)^,
         Pointer(NativeUint(wData) + I * fWidthPOT * 4)^, fMapX * 4);

  {$IFNDEF NO_OGL}
  TRender.UpdateTexture(fMapTex.Tex, fWidthPOT, fHeightPOT, tfRGBA8, wData);
  {$ENDIF}
  FreeMem(wData);
end;


procedure TKMMinimap.SaveToStream(SaveStream: TKMemoryStream);
var
  L: Cardinal;
  I: Integer;
begin
  SaveStream.PlaceMarker('Minimap');

  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  L := Length(fBase);
  SaveStream.Write(L);
  if L > 0 then
    SaveStream.Write(fBase[0], L * SizeOf(Cardinal));
  for I := 0 to MAX_HANDS - 1 do
  begin
    SaveStream.Write(HandColors[I]);
    SaveStream.Write(HandLocs[I]);
    SaveStream.Write(HandShow[I]);
  end;
end;


procedure TKMMinimap.LoadFromStream(LoadStream: TKMemoryStream);
var
  L: Cardinal;
  I: Integer;
begin
  LoadStream.CheckMarker('Minimap');

  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(L);
  SetLength(fBase, L);
  if L > 0 then
    LoadStream.Read(fBase[0], L * SizeOf(Cardinal));
  for I := 0 to MAX_HANDS - 1 do
  begin
    LoadStream.Read(HandColors[I]);
    LoadStream.Read(HandLocs[I]);
    LoadStream.Read(HandShow[I]);
  end;

  //Resize will update UV bounds. Resizing fBase is ok since the size does not changes
  Resize(fMapX, fMapY);

  if fMapX * fMapY = 0 then Exit;

  if fSepia then ApplySepia;
  UpdateTexture;
end;


end.
