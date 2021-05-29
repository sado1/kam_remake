unit ConsoleMain;
interface
uses
  KM_MinimapMission,
  KM_Defaults, MapUtilTypes;

type
  TConsoleMain = class(TObject)
  private
    fVerbose: Boolean;

    fMinimap: TKMMinimapMission;
    procedure GenerateAndSaveMapMinimapImage(const aMapDatPath: string);
    procedure SaveToFile(const aFileName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(const aParameterRecord: TCLIParamRecord);
    procedure ShowHelp;
  end;

const
  VALIDATOR_VERSION_MAJOR = '2';
  VALIDATOR_VERSION_MINOR = '03';
var
  VALIDATOR_VERSION: String;
const
  VALIDATOR_START_TEXT    = '' + sLineBreak +
    '++===============================================================================++' + sLineBreak +
    '++===============================================================================++' + sLineBreak +
    '||                          KaM Remake Map Utility                               ||' + sLineBreak +
    '++===============================================================================++' + sLineBreak +
    '++===============================================================================++' + sLineBreak;
  VALIDATOR_HELP_TEXT     = '' +
    '++===============================================================================++' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '||  Script Validator has a few options.                                          ||' + sLineBreak +
    '||  Below we will show these options and give a brief explanation what they do.  ||' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '||===============================================================================||' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '||  Usage:                                                                       ||' + sLineBreak +
    '||    ScriptValidator.exe [OPTIONS] FileName.ext                                 ||' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '||  Options:                                                                     ||' + sLineBreak +
    '||    -h / -help        - Will show this menu                                    ||' + sLineBreak +
    '||    -a / -all         - Check all map scripts                                  ||' + sLineBreak +
    '||    -v / -verbose     - Show verbose messages                                  ||' + sLineBreak +
    '||    -V / -version     - Show the script validator version                      ||' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '||===============================================================================||' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '||  Credits:                                                                     ||' + sLineBreak +
    '||    Programming > Krom                                                         ||' + sLineBreak +
    '||                > Lewin                                                        ||' + sLineBreak +
    '||                > Rey                                                          ||' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '||    Additional programming > Alex                                              ||' + sLineBreak +
    '||                           > Andreus                                           ||' + sLineBreak +
    '||                           > Danjb                                             ||' + sLineBreak +
    '||                           > ZblCoder                                          ||' + sLineBreak +
    '||                           > Thimo                                             ||' + sLineBreak +
    '||                                                                               ||' + sLineBreak +
    '++===============================================================================++' + sLineBreak;

implementation
uses
  {$IFDEF MSWindows} Windows, Vcl.Graphics, Vcl.Imaging.JPEG, {$ENDIF}
  SysUtils, Classes, IOUtils,
  KM_Maps, KM_Resource, KM_IoPNG,
  KM_CommonUtils, KM_Log;

{ TMain }
constructor TConsoleMain.Create;
begin
  inherited;

  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'MapUtil.log');
  fMinimap := TKMMinimapMission.Create(True);

  gRes := TKMResource.Create(nil, nil);
  gRes.LoadMainResources;
end;


destructor TConsoleMain.Destroy;
begin
  FreeAndNil(fMinimap);

  inherited;
end;


procedure TConsoleMain.GenerateAndSaveMapMinimapImage(const aMapDatPath: string);
var
  mapName, dir: string;
  map: TKMMapInfo;
begin
  mapName := TPath.GetFileNameWithoutExtension(aMapDatPath);
  dir := ExtractFileDir(aMapDatPath) + PathDelim;

  map := TKMMapInfo.Create(dir, mapName, False);
  map.TxtInfo.LoadTXTInfo(ChangeFileExt(aMapDatPath, '.txt'));

  fMinimap.LoadFromMission(aMapDatPath, map.HumanUsableLocs);
  fMinimap.Update(not map.TxtInfo.BlockFullMapPreview);

//  fMinimap.ConvertToBGR;
  SaveToFile(ChangeFileExt(aMapDatPath, '.png'));

  map.Free;
end;


procedure TConsoleMain.SaveToFile(const aFileName: string);
begin
  SaveToPng(fMinimap.MapX, fMinimap.MapY, fMinimap.Base, aFileName);
end;


procedure TConsoleMain.Start(const aParameterRecord: TCLIParamRecord);
begin
  fVerbose := aParameterRecord.Verbose;

  GenerateAndSaveMapMinimapImage(aParameterRecord.MapDatPath);
end;


procedure TConsoleMain.ShowHelp;
begin
  Writeln(VALIDATOR_HELP_TEXT);
end;


end.

