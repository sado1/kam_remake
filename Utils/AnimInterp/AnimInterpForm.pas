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
    fDainExe: string;
    fDainParams: string;
    fOrigSpritesDir: string;
    fInterpSpritesDir: string;

    function DoInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aPicOffset: Integer): Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  ShellApi, Math,
  KM_ResHouses, KM_Log, KM_PNG;

{$R *.dfm}


function TForm1.DoInterpUnit(aUT: TKMUnitType; aAction: TKMUnitActionType; aDir: TKMDirection; aPicOffset: Integer): Integer;
var
  RT: TRXType;
  RXName: string;
  I, Step, SpriteID: Integer;
  A: TKMAnimLoop;
  pngWidth, pngHeight, newWidth, newHeight: Word;
  pngData, pngCrop: TKMCardinalArray;
  X, Y, MinX, MinY, MaxX, MaxY: Integer;
  StrList: TStringList;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  RT := rxUnits;
  if fSprites[RT] = nil then
  begin
    fSprites[RT] := TKMSpritePackEdit.Create(RT, fPalettes);
    RXName := ExeDir + 'data\Sprites\' + RXInfo[RT].FileName + '_a.rxx';
    fSprites[RT].LoadFromRXXFile(RXName);
  end;

  StrList := TStringList.Create;

  A := fResUnits[aUT].UnitAnim[aAction,aDir];
  for Step := 1 to A.Count do
  begin
    SpriteID := A.Step[Step]+1; //Sprites in units.dat are 0 indexed
    if SpriteID > 0 then
    begin
      fSprites[RT].ExportImageForInterp(fOrigSpritesDir + format('%.6d.png', [Step]), SpriteID);
    end;
  end;
  //Write out the first sprite again to create a loop
  SpriteID := A.Step[1]+1; //Sprites in units.dat are 0 indexed
  if SpriteID > 0 then
    fSprites[RT].ExportImageForInterp(fOrigSpritesDir + format('%.6d.png', [A.Count+1]), SpriteID);

  //Interpolate
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  CreateProcess(nil, PChar(fDainParams), nil, nil, false, 0, nil, PChar(fDainFolder), StartupInfo, ProcessInfo);
  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
  //ShellExecute(0, nil, 'cmd.exe', PChar(DainParams), PChar(DainFolder), SW_SHOWNORMAL);

  //Import and reprocess
  Result := 8*A.Count;
  for Step := 1 to 8*A.Count do
  begin
    LoadFromPng(fInterpSpritesDir + format('%.15d.png', [Step]), pngWidth, pngHeight, pngData);

    //Determine Min/Max X/Y
    MinX := MaxInt;
    MinY := MaxInt;
    MaxX := -1;
    MaxY := -1;
    for Y := 0 to pngHeight-1 do
    begin
      for X := 0 to pngWidth-1 do
      begin
        if pngData[Y*pngWidth + X] shr 24 <> 0 then
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
      I := 0;
      for Y := MinY to MaxY do
      begin
        for X := MinX to MaxX do
        begin
          pngCrop[I] := pngData[Y*pngWidth + X];
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
  for dir := dirN to dirNW do
    picOffset := picOffset + DoInterpUnit(utMilitia, uaWalk, dir, picOffset);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  Caption := 'Animation Interpolator (' + GAME_REVISION + ')';

  fPalettes := TKMResPalettes.Create;
  fPalettes.LoadPalettes(ExeDir + 'data\gfx\');

  fResUnits := TKMResUnits.Create;

  fTempDir := ExeDir + 'SpriteInterp\';
  fDainFolder := 'C:\Dev\kam_sprites\DAIN_APP Alpha 1.0\';
  fDainExe := fDainFolder+'DAINAPP.exe';
  fDainParams := 'cmd.exe /C "'+fDainExe+'" --cli 1 -o '+fTempDir+' -p 0 -l 1 -in 8 -da 1 -a 1 -se 0 -si 1 -sr 0 -ha 0 --fast_mode 0'; // & pause
  //fDainParams := 'cmd.exe /C python inference_video.py --exp=3 --scale=4.0 --png --img='+fTempDir+'original_frames --output='+fTempDir+'interpolated_frames & pause';

  fOrigSpritesDir := fTempDir + 'original_frames\';
  fInterpSpritesDir := fTempDir + 'interpolated_frames\';
  ForceDirectories(fOrigSpritesDir);
end;

end.
