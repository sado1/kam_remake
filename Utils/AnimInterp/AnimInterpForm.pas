unit AnimInterpForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  KM_ResPalettes;

type
  TForm1 = class(TForm)
    btnProcessUnits: TButton;
    procedure btnProcessUnitsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fPalettes: TKMResPalettes;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  ShellApi, Math,
  KM_Pics, KM_ResHouses, KM_ResUnits, KM_ResSprites, KM_Points, KM_ResSpritesEdit, KM_Defaults, KM_Log, KM_CommonTypes, KM_PNG;

{$R *.dfm}

procedure TForm1.btnProcessUnitsClick(Sender: TObject);
var
  RT: TRXType;
  SpritePack: TKMSpritePackEdit;
  RXName, TempDir, OrigSpritesDir, InterpSpritesDir, DainFolder, DainExe, DainParams: string;
  resUnits: TKMResUnits;
  UT: TKMUnitType;
  Dir: TKMDirection;
  I, Step, SpriteID: Integer;
  A: TKMAnimLoop;
  pngWidth, pngHeight, newWidth, newHeight: Word;
  pngData, pngCrop: TKMCardinalArray;
  X, Y, MinX, MinY, MaxX, MaxY: Integer;
  StrList: TStringList;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrList := TStringList.Create;
  RT := rxUnits;
  SpritePack := TKMSpritePackEdit.Create(RT, fPalettes);
  try
    RXName := ExeDir + 'data\Sprites\' + RXInfo[RT].FileName + '_a.rxx';
    SpritePack.LoadFromRXXFile(RXName);

    resUnits := TKMResUnits.Create;

    TempDir := ExeDir + 'SpriteInterp\';
    DainFolder := 'C:\Dev\kam_sprites\DAIN_APP Alpha 1.0\';
    DainExe := DainFolder+'DAINAPP.exe';
    DainParams := 'cmd.exe /C "'+DainExe+'" --cli 1 -o '+TempDir+' -p 0 -l 1 -in 8 -da 0 -a 1 -se 0 -si 1 -sr 0 -ha 0 --fast_mode 0 & pause';

    OrigSpritesDir := TempDir + 'original_frames\';
    InterpSpritesDir := TempDir + 'interpolated_frames\';
    ForceDirectories(OrigSpritesDir);

    //Export
    UT := utMilitia;
    Dir := dirE;
    A := resUnits[UT].UnitAnim[uaWalk,Dir];
    for Step := 1 to A.Count do
    begin
      SpriteID := A.Step[Step]+1; //Sprites in units.dat are 0 indexed
      if SpriteID > 0 then
      begin
        SpritePack.ExportImageForInterp(OrigSpritesDir + format('%.6d.png', [Step]), SpriteID);
      end;
    end;
    //Write out the first sprite again to create a loop
    SpriteID := A.Step[1]+1; //Sprites in units.dat are 0 indexed
    if SpriteID > 0 then
      SpritePack.ExportImageForInterp(OrigSpritesDir + format('%.6d.png', [A.Count+1]), SpriteID);

    //Interpolate
    ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
    StartupInfo.cb := SizeOf(StartupInfo);
    CreateProcess(nil, PChar(DainParams), nil, nil, false, 0, nil, PChar(DainFolder), StartupInfo, ProcessInfo);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    //ShellExecute(0, nil, 'cmd.exe', PChar(DainParams), PChar(DainFolder), SW_SHOWNORMAL);

    //Import and reprocess
    for Step := 1 to 8*A.Count do
    begin
      LoadFromPng(InterpSpritesDir + format('%.15d.png', [Step]), pngWidth, pngHeight, pngData);

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
        StrList.SaveToFile(ExeDir+'Sprites\3\'+format('3_93%.2d.txt', [Step]));
        SaveToPng(newWidth, newHeight, pngCrop, ExeDir+'Sprites\3\'+format('3_93%.2d.png', [Step]));
      end;
    end;

  finally
    SpritePack.Free;
    StrList.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  Caption := 'Animation Interpolator (' + GAME_REVISION + ')';

  fPalettes := TKMResPalettes.Create;
  fPalettes.LoadPalettes(ExeDir + 'data\gfx\');
end;

end.
