unit AnimInterpForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KM_ResPalettes, KM_Defaults, KM_CommonTypes, KM_Points, KM_ResSprites, KM_ResSpritesEdit, KM_Pics, KM_ResUnits;

type
  TAnimCacheItem = record
    PicOffset: Integer;
    A: TKMAnimLoop;
  end;

  TForm1 = class(TForm)
    btnProcessUnits: TButton;
    Memo1: TMemo;
    memoErrors: TMemo;
    Label1: TLabel;
    procedure btnProcessUnitsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fPalettes: TKMResPalettes;
    fResUnits: TKMResUnits;
    fSprites: array[TRXType] of TKMSpritePackEdit;

    fAnimCache: array of TAnimCacheItem;

    fTempDir: string;
    fDainFolder: string;

    function GetMinCanvasSize(A: TKMAnimLoop; RT: TRXType): Integer;
    function GetDainParams(aDir: string; aAlpha: Boolean): string;
    procedure MakeImagesInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aBaseDir: string; aExportType: TInterpExportType);
    function DoInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; var aPicOffset: Integer): Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  ShellApi, Math, RTTI, KM_FileIO, KromUtils,
  KM_ResHouses, KM_Log, KM_PNG;

const
  CANVAS_Y_OFFSET = 14;

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  Caption := 'Animation Interpolator (' + GAME_REVISION + ')';

  fPalettes := TKMResPalettes.Create;
  fPalettes.LoadPalettes(ExeDir + 'data\gfx\');

  fResUnits := TKMResUnits.Create;

  fTempDir := ExeDir + 'SpriteInterp\';
  fDainFolder := 'C:\Dev\kam_sprites\DAIN_APP Alpha 1.0\';
end;

function TForm1.GetDainParams(aDir: string; aAlpha: Boolean): string;
var
  DainFolder, DainExe: string;
begin
  DainExe := fDainFolder+'DAINAPP.exe';
  Result := 'cmd.exe /C "'+DainExe+'" --cli 1 -o '+aDir+' -p 0 -l 1 -in 8 -da 0 -se 0 -si 1 -sr 0 -ha 0 --fast_mode 0';
  if aAlpha then
    Result := Result + ' -a 1';

  //Result := 'cmd.exe /C python inference_video.py --exp=3 --scale=4.0 --png --img='+aDir+'original_frames --output='+aDir+'interpolated_frames & pause';

  //Useful for checking error messages
  //Result := Result + ' & pause';
end;


function TForm1.GetMinCanvasSize(A: TKMAnimLoop; RT: TRXType): Integer;
var
  Step, SpriteID, MaxSoFar, X, Y, W, H: Integer;
begin
  MaxSoFar := 32;
  for Step := 1 to A.Count do
  begin
    SpriteID := A.Step[Step]+1; //Sprites in units.dat are 0 indexed
    if SpriteID > 0 then
    begin
      W := fSprites[RT].RXData.Size[SpriteID].X;
      H := fSprites[RT].RXData.Size[SpriteID].Y;
      X := fSprites[RT].RXData.Pivot[SpriteID].x;
      Y := fSprites[RT].RXData.Pivot[SpriteID].y + CANVAS_Y_OFFSET;
      MaxSoFar := Max(MaxSoFar, -X);
      MaxSoFar := Max(MaxSoFar, X + W);
      MaxSoFar := Max(MaxSoFar, -Y);
      MaxSoFar := Max(MaxSoFar, Y + H);
    end;
  end;
  //Sprite is centred so we need this much padding on both sides
  MaxSoFar := 2*MaxSoFar;
  //Keep 1px padding on all sides
  MaxSoFar := MaxSoFar + 2;
  Result := ((MaxSoFar div 32) + 1)*32;
end;


procedure TForm1.MakeImagesInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aBaseDir: string; aExportType: TInterpExportType);
var
  RT: TRXType;
  origSpritesDir, interpSpritesDir: string;
  A: TKMAnimLoop;
  Step, SpriteID, CanvasSize: Integer;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  NeedAlpha, AllBlank, Worked: Boolean;
begin
  RT := rxUnits;
  if fSprites[RT] = nil then
  begin
    fSprites[RT] := TKMSpritePackEdit.Create(RT, fPalettes);
    fSprites[RT].LoadFromRXXFile(ExeDir + 'data\Sprites\' + RXInfo[RT].FileName + '_a.rxx');
  end;

  origSpritesDir := aBaseDir + 'original_frames\';
  interpSpritesDir := aBaseDir + 'interpolated_frames\';
  ForceDirectories(origSpritesDir);

  KMDeleteFolderContent(origSpritesDir);
  KMDeleteFolderContent(interpSpritesDir);

  AllBlank := True;
  A := fResUnits[aUT].UnitAnim[aAction,aDir];
  CanvasSize := GetMinCanvasSize(A, RT);
  for Step := 1 to A.Count do
  begin
    SpriteID := A.Step[Step]+1; //Sprites in units.dat are 0 indexed
    if SpriteID > 0 then
    begin
      Worked := fSprites[RT].ExportImageForInterp(origSpritesDir + format('%.6d.png', [Step]), SpriteID, aExportType, CanvasSize);
      AllBlank := AllBlank and not Worked;
    end
    else
      raise Exception.Create('SpriteID = 0 for step '+IntToStr(Step));
  end;
  if AllBlank then
    Exit;

  //Write out the first sprite again to create a loop
  SpriteID := A.Step[1]+1; //Sprites in units.dat are 0 indexed
  if SpriteID > 0 then
    fSprites[RT].ExportImageForInterp(origSpritesDir + format('%.6d.png', [A.Count+1]), SpriteID, aExportType, CanvasSize);

  NeedAlpha := aExportType in [ietBase, ietNormal];

  //Interpolate
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_MINIMIZE;
  CreateProcess(nil, PChar(GetDainParams(aBaseDir, NeedAlpha)), nil, nil, false, 0, nil, PChar(fDainFolder), StartupInfo, ProcessInfo);
  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
  //ShellExecute(0, nil, 'cmd.exe', PChar(DainParams), PChar(DainFolder), SW_SHOWNORMAL);
end;


function BlendRGBA(Back, Fore: Cardinal): Cardinal;
var
  C, RGB, Alpha: Cardinal;
  R1, R2, G1, G2, B1, B2, A1, A2: Single;
  Ro, Go, Bo, Ao: Single;
begin
  //Extract and normalise
  R1 := ((Back shr 0) and $FF) / 255;
  R2 := ((Fore shr 0) and $FF) / 255;
  G1 := ((Back shr 8) and $FF) / 255;
  G2 := ((Fore shr 8) and $FF) / 255;
  B1 := ((Back shr 16) and $FF) / 255;
  B2 := ((Fore shr 16) and $FF) / 255;
  A1 := ((Back shr 24) and $FF) / 255;
  A2 := ((Fore shr 24) and $FF) / 255;

  //https://en.wikipedia.org/wiki/Alpha_compositing
  Ao := A1 + A2*(1.0 - A1);
  if Ao > 0.0 then
  begin
    Ro := (R1*A1 + R2*A2*(1.0 - A1)) / Ao;
    Go := (G1*A1 + G2*A2*(1.0 - A1)) / Ao;
    Bo := (B1*A1 + B2*A2*(1.0 - A1)) / Ao;

    Ro := EnsureRange(Ro, 0.0, 1.0);
    Go := EnsureRange(Go, 0.0, 1.0);
    Bo := EnsureRange(Bo, 0.0, 1.0);

    Result :=
      ((Trunc(Ro*255) and $FF) shl 0) or
      ((Trunc(Go*255) and $FF) shl 8) or
      ((Trunc(Bo*255) and $FF) shl 16) or
      ((Trunc(Ao*255) and $FF) shl 24);
  end
  else
    Result := 0;
end;


function TForm1.DoInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; var aPicOffset: Integer): Integer;

  function SameAnim(A, B: TKMAnimLoop): Boolean;
  var
    I: Integer;
  begin
    Result := A.Count = B.Count;
    for I := 1 to 30 do
      Result := Result and (A.Step[I] = B.Step[I]);
  end;

var
  A: TKMAnimLoop;
  I, Step: Integer;
  pngWidth, pngHeight, newWidth, newHeight: Word;
  pngBase, pngShad, pngTeam, pngCrop, pngCropMask: TKMCardinalArray;
  X, Y, MinX, MinY, MaxX, MaxY: Integer;
  NoShadMinX, NoShadMinY, NoShadMaxX, NoShadMaxY: Integer;
  StrList: TStringList;
  dirBase, dirShad, dirTeam, suffixPath: string;
  needsMask: Boolean;
begin
  if aDir = dirNA then
    Exit(-1);

  A := fResUnits[aUT].UnitAnim[aAction,aDir];

  if (A.Count <= 0) or (A.Step[1] = -1) then
    Exit(-1);

  for I := Low(fAnimCache) to High(fAnimCache) do
  begin
    if SameAnim(fAnimCache[I].A, A) then
    begin
      Result := fAnimCache[I].PicOffset;
      Exit;
    end;
  end;

  dirBase := fTempDir + 'base\';
  dirShad := fTempDir + 'shad\';
  dirTeam := fTempDir + 'team\';

  KMDeleteFolder(dirBase);
  KMDeleteFolder(dirShad);
  KMDeleteFolder(dirTeam);

  MakeImagesInterpUnit(aUt, aAction, aDir, dirBase, ietBase);
  MakeImagesInterpUnit(aUt, aAction, aDir, dirShad, ietShadows);
  MakeImagesInterpUnit(aUt, aAction, aDir, dirTeam, ietTeamMask);

  StrList := TStringList.Create;

  //Import and reprocess
  for Step := 1 to 8*A.Count do
  begin
    suffixPath := 'interpolated_frames\' + format('%.15d.png', [Step]);

    LoadFromPng(dirBase + suffixPath, pngWidth, pngHeight, pngBase);

    if FileExists(dirShad + suffixPath) then
      LoadFromPng(dirShad + suffixPath, pngWidth, pngHeight, pngShad);

    if FileExists(dirTeam + suffixPath) then
      LoadFromPng(dirTeam + suffixPath, pngWidth, pngHeight, pngTeam);

    //Determine Min/Max X/Y
    MinX := MaxInt;
    MinY := MaxInt;
    MaxX := -1;
    MaxY := -1;
    NoShadMinX := MaxInt;
    NoShadMinY := MaxInt;
    NoShadMaxX := -1;
    NoShadMaxY := -1;
    needsMask := False;
    for Y := 0 to pngHeight-1 do
    begin
      for X := 0 to pngWidth-1 do
      begin
        if (pngBase[Y*pngWidth + X] shr 24 > 50) or
        ((Length(pngShad) > 0) and ((pngShad[Y*pngWidth + X] and $FF) > 10)) then
        begin
          MinX := Min(MinX, X);
          MinY := Min(MinY, Y);
          MaxX := Max(MaxX, X);
          MaxY := Max(MaxY, Y);
        end;
        //Tighter "no shadow" bounds. Also ignore areas where alpha < 50%
        if pngBase[Y*pngWidth + X] shr 24 >= $7F then
        begin
          NoShadMinX := Min(NoShadMinX, X);
          NoShadMinY := Min(NoShadMinY, Y);
          NoShadMaxX := Max(NoShadMaxX, X);
          NoShadMaxY := Max(NoShadMaxY, Y);
        end;
        if (Length(pngTeam) > 0) and ((pngTeam[Y*pngWidth + X] and $FF) > 0) then
          needsMask := True;
      end;
    end;
    //Crop
    if (MaxX > MinX) and (MaxY > MinY) then
    begin
      newWidth := MaxX - MinX + 1;
      newHeight := MaxY - MinY + 1;
      SetLength(pngCrop, newWidth*newHeight);
      SetLength(pngCropMask, newWidth*newHeight);
      I := 0;
      for Y := MinY to MaxY do
      begin
        for X := MinX to MaxX do
        begin
          //Background is black with alpha from the shadow mask
          if Length(pngShad) > 0 then
            pngCrop[I] := pngShad[Y*pngWidth + X] shl 24;

          //Death animations have semi-transparent white that we treat as shadows
          if (aAction = uaDie) and (pngCrop[I] > 0) then
            pngCrop[I] := pngCrop[I] or $FFFFFF;

          //Layer base sprite on top
          pngCrop[I] := BlendRGBA(pngCrop[I], pngBase[Y*pngWidth + X]);

          if Length(pngTeam) > 0 then
            pngCropMask[I] := pngTeam[Y*pngWidth + X];

          Inc(I);
        end;
      end;

      //Save offsets .txt file
      StrList.Clear;
      StrList.Append(IntToStr(MinX - pngWidth div 2));
      StrList.Append(IntToStr(MinY - CANVAS_Y_OFFSET - pngHeight div 2));
      StrList.Append(IntToStr(NoShadMinX - MinX));
      StrList.Append(IntToStr(NoShadMinY - MinY));
      StrList.Append(IntToStr(newWidth-1 - (MaxX - NoShadMaxX)));
      StrList.Append(IntToStr(newHeight-1 - (MaxY - NoShadMaxY)));

      ForceDirectories(ExeDir+'Sprites\3\');
      StrList.SaveToFile(ExeDir+'Sprites\3\'+format('3_%d.txt', [aPicOffset + Step - 1]));
      SaveToPng(newWidth, newHeight, pngCrop, ExeDir+'Sprites\3\'+format('3_%d.png', [aPicOffset + Step - 1]));
      if needsMask and (Length(pngTeam) > 0) then
        SaveToPng(newWidth, newHeight, pngCropMask, ExeDir+'Sprites\3\'+format('3_%dm.png', [aPicOffset + Step - 1]));
    end;
  end;

  Result := aPicOffset;
  SetLength(fAnimCache, Length(fAnimCache)+1);
  fAnimCache[Length(fAnimCache)-1].A := A;
  fAnimCache[Length(fAnimCache)-1].PicOffset := aPicOffset;
  aPicOffset := aPicOffset + 8*A.Count;

  FreeAndNil(StrList);
end;

procedure TForm1.btnProcessUnitsClick(Sender: TObject);
var
  picOffset, animPicOffset: Integer;
  dir: TKMDirection;
  act: TKMUnitActionType;
  u: TKMUnitType;
  animData: string;
begin
  picOffset := 9300;

  animData := 'array[TKMUnitType, TKMUnitActionType, TKMDirection] of Integer = ('+#13#10;
  for u := Low(TKMUnitType) to High(TKMUnitType) do
  begin
    animData := animData + '  ('+' // '+TRttiEnumerationType.GetName(u)+#13#10;
    for act := Low(TKMUnitActionType) to High(TKMUnitActionType) do
    begin
      animData := animData + '    (';
      for dir := Low(TKMDirection) to High(TKMDirection) do
      begin
        try
          animPicOffset := DoInterpUnit(u, act, dir, picOffset);
        except
          on E: Exception do
          begin
            memoErrors.Text := memoErrors.Text + TRttiEnumerationType.GetName(u) + ' - ' + UNIT_ACT_STR[act] + ' - ' + TRttiEnumerationType.GetName(dir) + ' - ' + E.Message + #13#10;
            animPicOffset := -1;
          end;
        end;

        if animPicOffset >= 0 then
          animData := animData + IntToStr(animPicOffset)
        else
          animData := animData + '-1';

        if dir <> High(TKMDirection) then
          animData := animData + ',';
      end;
      animData := animData + ')';
      if act <> High(TKMUnitActionType) then
        animData := animData + ',';
      animData := animData+' // '+UNIT_ACT_STR[act]+#13#10;
    end;
    animData := animData + '  )';
    if u <> High(TKMUnitType) then
      animData := animData + ','+#13#10;
  end;
  animData := animData + #13#10 + ');';

  Memo1.Text := animData;
end;

end.
