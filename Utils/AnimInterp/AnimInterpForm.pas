unit AnimInterpForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KM_ResPalettes, KM_Defaults, KM_CommonTypes, KM_Points, KM_ResSprites, KM_ResSpritesEdit, KM_Pics, KM_ResUnits;

type
  TForm1 = class(TForm)
    btnProcessUnits: TButton;
    procedure btnProcessUnitsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fPalettes: TKMResPalettes;
    fResUnits: TKMResUnits;
    fSprites: array[TRXType] of TKMSpritePackEdit;

    fTempDir: string;
    fDainFolder: string;

    function GetDainParams(aDir: string; aAlpha: Boolean): string;
    procedure MakeImagesInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aBaseDir: string; aExportType: TInterpExportType);
    function DoInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aPicOffset: Integer): Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  ShellApi, Math, KromUtils,
  KM_ResHouses, KM_Log, KM_PNG;

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


procedure TForm1.MakeImagesInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aBaseDir: string; aExportType: TInterpExportType);
var
  RT: TRXType;
  RXName, origSpritesDir, interpSpritesDir: string;
  A: TKMAnimLoop;
  Step, SpriteID: Integer;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  NeedAlpha: Boolean;
begin
  RT := rxUnits;
  if fSprites[RT] = nil then
  begin
    fSprites[RT] := TKMSpritePackEdit.Create(RT, fPalettes);
    RXName := ExeDir + 'data\Sprites\' + RXInfo[RT].FileName + '_a.rxx';
    fSprites[RT].LoadFromRXXFile(RXName);
  end;

  origSpritesDir := aBaseDir + 'original_frames\';
  interpSpritesDir := aBaseDir + 'interpolated_frames\';
  ForceDirectories(origSpritesDir);

  A := fResUnits[aUT].UnitAnim[aAction,aDir];
  for Step := 1 to A.Count do
  begin
    SpriteID := A.Step[Step]+1; //Sprites in units.dat are 0 indexed
    if SpriteID > 0 then
    begin
      fSprites[RT].ExportImageForInterp(origSpritesDir + format('%.6d.png', [Step]), SpriteID, aExportType);
    end;
  end;
  //Write out the first sprite again to create a loop
  SpriteID := A.Step[1]+1; //Sprites in units.dat are 0 indexed
  if SpriteID > 0 then
    fSprites[RT].ExportImageForInterp(origSpritesDir + format('%.6d.png', [A.Count+1]), SpriteID, aExportType);

  NeedAlpha := aExportType in [ietBase, ietNormal];

  //Interpolate
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
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


function TForm1.DoInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aPicOffset: Integer): Integer;
var
  A: TKMAnimLoop;
  I, Step: Integer;
  pngWidth, pngHeight, newWidth, newHeight: Word;
  pngBase, pngShad, pngTeam, pngCrop, pngCropMask: TKMCardinalArray;
  X, Y, MinX, MinY, MaxX, MaxY: Integer;
  StrList: TStringList;
  dirBase, dirShad, dirTeam: string;
begin
  dirBase := fTempDir + 'base\';
  dirShad := fTempDir + 'shad\';
  dirTeam := fTempDir + 'team\';

  MakeImagesInterpUnit(aUt, aAction, aDir, dirBase, ietBase);
  MakeImagesInterpUnit(aUt, aAction, aDir, dirShad, ietShadows);
  MakeImagesInterpUnit(aUt, aAction, aDir, dirTeam, ietTeamMask);

  StrList := TStringList.Create;
  A := fResUnits[aUT].UnitAnim[aAction,aDir];

  //Import and reprocess
  Result := 8*A.Count;
  for Step := 1 to 8*A.Count do
  begin
    LoadFromPng(dirBase + 'interpolated_frames\' + format('%.15d.png', [Step]), pngWidth, pngHeight, pngBase);
    LoadFromPng(dirShad + 'interpolated_frames\' + format('%.15d.png', [Step]), pngWidth, pngHeight, pngShad);
    LoadFromPng(dirTeam + 'interpolated_frames\' + format('%.15d.png', [Step]), pngWidth, pngHeight, pngTeam);

    //Determine Min/Max X/Y
    MinX := MaxInt;
    MinY := MaxInt;
    MaxX := -1;
    MaxY := -1;
    for Y := 0 to pngHeight-1 do
    begin
      for X := 0 to pngWidth-1 do
      begin
        if (pngBase[Y*pngWidth + X] shr 24 <> 0) or
        ((pngShad[Y*pngWidth + X] and $FFFFFF) <> 0) or
        ((pngTeam[Y*pngWidth + X] and $FFFFFF) <> 0) then
        begin
          MinX := Min(MinX, X);
          MinY := Min(MinY, Y);
          MaxX := Max(MaxX, X);
          MaxY := Max(MaxY, Y);
        end;
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
          pngCrop[I] := pngShad[Y*pngWidth + X] shl 24;
          //Layer base sprite on top
          pngCrop[I] := BlendRGBA(pngCrop[I], pngBase[Y*pngWidth + X]);

          pngCropMask[I] := pngTeam[Y*pngWidth + X];

          Inc(I);
        end;
      end;

      //Save offsets .txt file
      StrList.Clear;
      StrList.Append(IntToStr(MinX - pngWidth div 2));
      StrList.Append(IntToStr(MinY - pngHeight div 2));
      //FIXME: Do SizeNoShadow properly
      StrList.Append(IntToStr(0));
      StrList.Append(IntToStr(0));
      StrList.Append(IntToStr(newWidth-1));
      StrList.Append(IntToStr(newHeight-1));

      ForceDirectories(ExeDir+'Sprites\3\');
      StrList.SaveToFile(ExeDir+'Sprites\3\'+format('3_%d.txt', [aPicOffset + Step]));
      SaveToPng(newWidth, newHeight, pngCrop, ExeDir+'Sprites\3\'+format('3_%d.png', [aPicOffset + Step]));
      SaveToPng(newWidth, newHeight, pngCropMask, ExeDir+'Sprites\3\'+format('3_%dm.png', [aPicOffset + Step]));
    end;
  end;

  FreeAndNil(StrList);
end;

procedure TForm1.btnProcessUnitsClick(Sender: TObject);
var
  picOffset: Integer;
  dir: TKMDirection;
begin
  picOffset := 9300;
  dir := dirE;
  //for dir := dirN to dirNW do
    picOffset := picOffset + DoInterpUnit(utMilitia, uaWalk, dir, picOffset);
end;

end.
