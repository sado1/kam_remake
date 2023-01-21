unit KM_SoftShadows;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, KM_ResSprites, KM_ResTypes;

type
  TKMSoftShadowConverter = class
  private
    // We will be working with the data by this pointer
    fRXData: PRXData;

    fOnlyShadows: boolean;
    TempShadowMap: array of array of Boolean;
    ShadowMap: array of array of Boolean;

    function ReadPixelSafe(aIndex, aX, aY: Integer): Cardinal;

    function IsBlack(aColor: Cardinal): Boolean;
    function IsTransparent(aColor: Cardinal): Boolean;
    function IsObject(aColor: Cardinal): Boolean;
    function IsTransparentOrObject(aColor: Cardinal): Boolean;
    function IsShadow(aIndex, aX, aY: Integer): Boolean;
    procedure PrepareShadows(aIndex: Word; aOnlyShadows: Boolean);

    function IsShadowPixel(aIndex, aX, aY: Word): Boolean;
    function IsObjectPixel(aIndex, aX, aY: Word): Boolean;
  public
    constructor Create(aRXData: PRXData);
    procedure ConvertShadows(aIndex: Word; aOnlyShadows: Boolean);
    procedure DetermineImageObjectSize(aIndex: Word);
    procedure RemoveShadow(aIndex: Word; aByMask: Boolean);
  end;


implementation
const
  BLUR_RADIUS = 2.0; //Amount of blurring applied on shadow edges
  SHADING_LEVEL = 150; //Alpha value for full shadow (0..255)


{ TKMSoftShadowConverter }
constructor TKMSoftShadowConverter.Create(aRXData: PRXData);
begin
  inherited Create;

  fRXData := aRXData;
end;


function TKMSoftShadowConverter.ReadPixelSafe(aIndex, aX, aY: Integer): Cardinal;
begin
  if (aX < 0) or (aY < 0) or (aX >= fRXData.Size[aIndex].X) or (aY >= fRXData.Size[aIndex].Y) then
    Result := 0
  else
    Result := fRXData.RGBA[aIndex, aY * fRXData.Size[aIndex].X + aX];
end;


//Maybe the definition of black will change later (to include almost black colors?)
function TKMSoftShadowConverter.IsBlack(aColor: Cardinal): Boolean;
begin
  if fOnlyShadows then
    Result := (aColor = $FF000000) //Only black areas
  else
    Result := (aColor and $FF000000) <> 0; //Everything that's not transparent
end;


//Maybe the definition of transparent will change later
function TKMSoftShadowConverter.IsTransparent(aColor: Cardinal): Boolean;
begin
  Result := (aColor shr 24 = 0);
end;


//Pixels that are not transparent and not black are an object (part of actual sprite)
function TKMSoftShadowConverter.IsObject(aColor: Cardinal): Boolean;
begin
  Result := not IsTransparent(aColor) and not IsBlack(aColor);
end;


function TKMSoftShadowConverter.IsTransparentOrObject(aColor: Cardinal): Boolean;
begin
  Result := IsTransparent(aColor) or not IsBlack(aColor);
end;


function TKMSoftShadowConverter.IsShadow(aIndex, aX, aY: Integer): Boolean;
var
  colorThis, colorLeft, colorRight, colorTop, colorBottom: Cardinal;
begin
  colorThis   := ReadPixelSafe(aIndex, aX,   aY);
  colorLeft   := ReadPixelSafe(aIndex, aX-1, aY);
  colorRight  := ReadPixelSafe(aIndex, aX+1, aY);
  colorTop    := ReadPixelSafe(aIndex, aX,   aY-1);
  colorBottom := ReadPixelSafe(aIndex, aX,   aY+1);

  Result := False;

  if IsBlack(colorThis) then
  begin
    if IsTransparent(colorLeft) or IsTransparent(colorRight)
    or IsTransparent(colorTop)  or IsTransparent(colorBottom) then
      Result := (
                  Ord(IsTransparentOrObject(colorLeft)) +
                  Ord(IsTransparentOrObject(colorRight)) +
                  Ord(IsTransparentOrObject(colorTop)) +
                  Ord(IsTransparentOrObject(colorBottom))
                ) > 2;
  end else
  if IsTransparent(colorThis) then
  begin
    if IsBlack(colorLeft) or IsBlack(colorRight)
    or IsBlack(colorTop)  or IsBlack(colorBottom) then
      Result := (
                  Ord(not IsTransparent(colorLeft)) +
                  Ord(not IsTransparent(colorRight)) +
                  Ord(not IsTransparent(colorTop)) +
                  Ord(not IsTransparent(colorBottom))
                ) > 2;
  end;
end;


function TKMSoftShadowConverter.IsShadowPixel(aIndex, aX, aY: Word): Boolean;
var
  Color: Cardinal;
begin
  Color := ReadPixelSafe(aIndex, aX, aY);
  Result := (TempShadowMap[aX, aY] or ShadowMap[aX, aY] or IsTransparent(Color)) and not IsObject(Color);
end;


function TKMSoftShadowConverter.IsObjectPixel(aIndex, aX, aY: Word): Boolean;
var
  Color: Cardinal;
begin
  Color := ReadPixelSafe(aIndex, aX, aY);
  Result := IsObject(Color) and not TempShadowMap[aX, aY] and not ShadowMap[aX, aY];
end;


procedure TKMSoftShadowConverter.DetermineImageObjectSize(aIndex: Word);
var
  X,Y: Integer;
begin
  PrepareShadows(aIndex, True);

  fRXData.SizeNoShadow[aIndex].left := fRXData.Size[aIndex].X - 1;
  fRXData.SizeNoShadow[aIndex].right := 0;
  fRXData.SizeNoShadow[aIndex].top := fRXData.Size[aIndex].Y - 1;
  fRXData.SizeNoShadow[aIndex].bottom := 0;

  for X := 0 to fRXData.Size[aIndex].X - 1 do
    for Y := 0 to fRXData.Size[aIndex].Y - 1 do
      if IsObjectPixel(aIndex, X, Y) then
      begin
        fRXData.SizeNoShadow[aIndex].left := Min(fRXData.SizeNoShadow[aIndex].left, X);
        fRXData.SizeNoShadow[aIndex].right := Max(fRXData.SizeNoShadow[aIndex].right, X);
        fRXData.SizeNoShadow[aIndex].top := Min(fRXData.SizeNoShadow[aIndex].top, Y);
        fRXData.SizeNoShadow[aIndex].bottom := Max(fRXData.SizeNoShadow[aIndex].bottom, Y);
      end;
end;


//RemoveShadow via removing its mask
procedure TKMSoftShadowConverter.RemoveShadow(aIndex: Word; aByMask: Boolean);
var
  X,Y: Integer;
begin
  PrepareShadows(aIndex, False);

  for X := 0 to fRXData.Size[aIndex].X - 1 do
    for Y := 0 to fRXData.Size[aIndex].Y - 1 do
      if IsShadowPixel(aIndex, X, Y) then
      begin
        if aByMask then
          fRXData.Mask[aIndex, Y*fRXData.Size[aIndex].X + X] := 0 //Remove mask image outside of object
        else
          fRXData.RGBA[aIndex, Y*fRXData.Size[aIndex].X + X] := 0;
      end;
end;


procedure TKMSoftShadowConverter.ConvertShadows(aIndex: Word; aOnlyShadows: Boolean);

  function GetBlurredShadow(X,Y: Integer): Single;
  var
    aX, aY, XDiff, YDiff, BlurCeil: Integer;
    Distance, Multiplier, Divisor, Ret: Single;
    Shadow, WasRealShadow: Boolean;
  begin
    WasRealShadow := False;
    Ret := 0;
    Divisor := 0;
    BlurCeil := Ceil(BLUR_RADIUS);
    for aX := X - BlurCeil to X + BlurCeil do
      for aY := Y - BlurCeil to Y + BlurCeil do
      begin
        XDiff := aX-X;
        YDiff := aY-Y;
        Distance := Sqrt(XDiff*XDiff + YDiff*YDiff);
        Multiplier := BLUR_RADIUS - Distance;

        if Multiplier > 0 then
        begin
          Divisor := Divisor + Multiplier;
          if (aX < 0) or (aY < 0) or (aX >= fRXData.Size[aIndex].X) or (aY >= fRXData.Size[aIndex].Y) then
            Continue;
          Shadow := ShadowMap[aX, aY];
          if Shadow then WasRealShadow := True;
          if not IsTransparent(ReadPixelSafe(aIndex, aX, aY)) then Shadow := True;
          if Shadow then Ret := Ret + Multiplier;
        end;
      end;
    if not WasRealShadow then
      Result := 0
    else
      Result := Ret/Divisor;
  end;

  function MixColors(colors: array of Cardinal): Cardinal;
  var
    i, R, G, B, Count: Cardinal;
  begin
    R := 0;
    B := 0;
    G := 0;
    count := 0;
    for i := 0 to Length(colors) - 1 do
      if colors[i] and $FF000000 <> 0 then
      begin
        R := R+(colors[i] and $FF);
        G := G+(colors[i] shr 8 and $FF);
        B := B+(colors[i] shr 16 and $FF);
        inc(count);
      end;

    Result := 0;
    if count=0 then exit;
    R := R div count;
    B := B div count;
    G := G div count;

    Result := (R + G shl 8 + B shl 16);
  end;

var
  X,Y: Integer;
  OriginalColor: Cardinal;
  RealShadow: Byte;
begin
  PrepareShadows(aIndex, aOnlyShadows);

  OriginalColor := 0;

  for X := 0 to fRXData.Size[aIndex].X - 1 do
    for Y := 0 to fRXData.Size[aIndex].Y - 1 do
      if IsShadowPixel(aIndex, X, Y) then
      begin
        RealShadow := Min(Round(GetBlurredShadow(X, Y) * SHADING_LEVEL), 255);
        //If we're doing the entire sprite consider the original color, else use black
        if not fOnlyShadows then
        begin
          OriginalColor := fRXData.RGBA[aIndex, Y * fRXData.Size[aIndex].X + X];
          if (OriginalColor and $FF000000) = 0 then
          begin
            //Take a blend of all the surrounding colors and use that to fill in gaps
            OriginalColor :=
            MixColors([fRXData.RGBA[aIndex, Max(Y-1,0                       )*fRXData.Size[aIndex].X + X],
                       fRXData.RGBA[aIndex, Min(Y+1,fRXData.Size[aIndex].Y-1)*fRXData.Size[aIndex].X + X],
                       fRXData.RGBA[aIndex, Y                                *fRXData.Size[aIndex].X + Max(X-1,0)],
                       fRXData.RGBA[aIndex, Y                                *fRXData.Size[aIndex].X + Min(X+1,fRXData.Size[aIndex].X-1)],
                       //Diagonals
                       fRXData.RGBA[aIndex, Max(Y-1,0                       )*fRXData.Size[aIndex].X + Min(X+1,fRXData.Size[aIndex].X-1)],
                       fRXData.RGBA[aIndex, Min(Y+1,fRXData.Size[aIndex].Y-1)*fRXData.Size[aIndex].X + Max(X-1,0)],
                       fRXData.RGBA[aIndex, Max(Y-1,0                       )*fRXData.Size[aIndex].X + Max(X-1,0)],
                       fRXData.RGBA[aIndex, Min(Y+1,fRXData.Size[aIndex].Y-1)*fRXData.Size[aIndex].X + Min(X+1,fRXData.Size[aIndex].X-1)]]);
          end
          else
            OriginalColor := OriginalColor and $00FFFFFF;
        end;
        fRXData.RGBA[aIndex, Y*fRXData.Size[aIndex].X + X] := (RealShadow shl 24) or OriginalColor;
      end;
end;


procedure TKMSoftShadowConverter.PrepareShadows(aIndex: Word; aOnlyShadows: Boolean);

  function ReadTempShadowMapSafe(X, Y: Integer): Boolean;
  begin
    if (X < 0) or (Y < 0) or (X >= fRXData.Size[aIndex].X) or (Y >= fRXData.Size[aIndex].Y) then
      Result := False
    else
      Result := TempShadowMap[X, Y];
  end;

  function IsShadowOrObject(X, Y: Integer): Boolean;
  begin
    if (X < 0) or (Y < 0) or (X >= fRXData.Size[aIndex].X) or (Y >= fRXData.Size[aIndex].Y) then
      Result := False
    else
      Result := TempShadowMap[X, Y] or IsObject(ReadPixelSafe(aIndex, X, Y));
  end;

  function ShadowsNearby(X,Y: Integer): Byte;
  begin
    Result := 0;
    if ReadTempShadowMapSafe(X-1, Y  )
    or ReadTempShadowMapSafe(X+1, Y  )
    or ReadTempShadowMapSafe(X,   Y-1)
    or ReadTempShadowMapSafe(X,   Y+1) then
      Result := Byte(IsShadowOrObject(X-1, Y  )) +
                Byte(IsShadowOrObject(X+1, Y  )) +
                Byte(IsShadowOrObject(X,   Y-1)) +
                Byte(IsShadowOrObject(X,   Y+1));
  end;

var
  X,Y: Integer;
  Shadow: Boolean;
begin
  fOnlyShadows := aOnlyShadows;

  SetLength(TempShadowMap, 0);
  SetLength(ShadowMap,     0);

  SetLength(TempShadowMap, fRXData.Size[aIndex].X, fRXData.Size[aIndex].Y);
  SetLength(ShadowMap,     fRXData.Size[aIndex].X, fRXData.Size[aIndex].Y);

  for X := 0 to fRXData.Size[aIndex].X - 1 do
    for Y := 0 to fRXData.Size[aIndex].Y - 1 do
      TempShadowMap[X, Y] := IsShadow(aIndex,X,Y);

  for X := 0 to fRXData.Size[aIndex].X - 1 do
    for Y := 0 to fRXData.Size[aIndex].Y - 1 do
    begin
      Shadow := TempShadowMap[X, Y];

      if Shadow and not IsObject(ReadPixelSafe(aIndex, X, Y))
      and (ShadowsNearby(X, Y) = 1) then
        Shadow := False;

      ShadowMap[X, Y] := Shadow;
    end;
end;


end.

