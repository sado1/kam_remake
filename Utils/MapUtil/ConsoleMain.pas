unit ConsoleMain;

interface
uses
  KM_Minimap,
  KM_Defaults, MapUtilTypes;

type
  TConsoleMain = class(TObject)
  private
    fVerbose: Boolean;

    fMinimap: TKMMinimap;
    procedure GenerateAndSaveMapMinimapImage(aMapDatPath: string);
    procedure SaveToFile(aFileName: string);
//    procedure ValidateSingleScriptApi(aPath: string);
//    procedure ValidateSingleScript(aPath, aCampaignFile: string; aReportGood: Boolean);
//    procedure ValidateCampaignScript(aPath: string);
//    procedure ValidateAllScripts;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(aParameterRecord: TCLIParamRecord);
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
    '||    -c / -campaign    - Check a campaign type script                           ||' + sLineBreak +
    '||    -g / -graphic     - Start the script validator in GUI mode                 ||' + sLineBreak +
    '||    -v / -verbose     - Show verbose messages                                  ||' + sLineBreak +
    '||    -j / -jsonapi     - Show verbose messages                                  ||' + sLineBreak +
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
var
  map: TKMMapInfo;
begin
  inherited;


  gLog := TKMLog.Create(ExtractFilePath(ParamStr(0)) + 'MapUtil.log');
  fMinimap := TKMMinimap.Create(True, True);

  gRes := TKMResource.Create(nil, nil);
  gRes.LoadMainResources;

end;


destructor TConsoleMain.Destroy;
begin
  FreeAndNil(fMinimap);

  inherited;
end;


//procedure TConsoleMain.ValidateSingleScriptApi(aPath: string);
//var
//  Result: TScriptValidatorResult;
//begin
//  if not FileExists(aPath) then
//  begin
//    Result := TScriptValidatorResult.Create;
//    Result.AddError(0, 0, '', 'File not found: ' + aPath);
//    WriteLn(Result.ToXML);
//    FreeAndNil(Result);
//    Exit;
//  end;
//
//  fScripting.LoadFromFile(aPath, '', nil);
//  writeln(fScripting.ValidationIssues.ToXML);
//end;


//procedure TConsoleMain.ValidateSingleScript(aPath, aCampaignFile: string; aReportGood: Boolean);
//var
//  OutputText: string;
//begin
//  if fVerbose then
//    writeln('VERBOSE: Checking if script file exists.' + sLineBreak);
//
//  if not FileExists(aPath) then
//  begin
//    writeln('File not found: ' + aPath);
//    Exit;
//  end else if fVerbose then
//    writeln('VERBOSE: Found script file.' + sLineBreak);
//
//  if fVerbose then
//    writeln('VERBOSE: Loading script file(s) into dynamic script compiler.' + sLineBreak);
//
//  fScripting.LoadFromFile(aPath, aCampaignFile, nil);
//
//  if fScripting.ErrorHandler.HasErrors then
//  begin
//    OutputText := OutputText + 'Errors:' + sLineBreak;
//    OutputText := OutputText + StringReplace(fScripting.ErrorHandler.ErrorString.GameMessage,
//                                             '|', sLineBreak,
//                                             [rfReplaceAll]);
//  end;
//
//  if fScripting.ErrorHandler.HasWarnings then
//  begin
//    OutputText := OutputText + 'Warnings:' + sLineBreak;
//    OutputText := OutputText + StringReplace(fScripting.ErrorHandler.WarningsString.GameMessage,
//                                             '|', sLineBreak,
//                                             [rfReplaceAll]);
//  end;
//
//  if OutputText <> '' then
//    writeln(aPath + sLineBreak + OutputText)
//  else
//    if aReportGood then
//      writeln(aPath + ' - No errors :)' + sLineBreak);
//end;


//procedure TConsoleMain.ValidateCampaignScript(aPath: string);
//var
//  CampaignFile: string;
//begin
//  if fVerbose then
//    writeln('VERBOSE: Checking if campaign script file exists.' + sLineBreak);
//
//  CampaignFile := ExtractFilePath(aPath) + '..\campaigndata.script';
//
//  if not FileExists(CampaignFile) then
//  begin
//    writeln('Campaign file not found: ' + CampaignFile);
//    CampaignFile := '';
//  end else
//    if fVerbose then
//      writeln('VERBOSE: Found campaign script file.' + sLineBreak);
//
//  ValidateSingleScript(aPath, CampaignFile, True);
//end;
//
//
//procedure TConsoleMain.ValidateAllScripts;
//var
//  maps: TStringList;
//  I:    Integer;
//begin
//  maps := TStringList.Create;
//
//  // Exe path
//  TKMapsCollection.GetAllMapPaths(ExtractFilePath(ParamStr(0)), maps);
//
//  for I := 0 to maps.Count - 1 do
//    ValidateSingleScript(ChangeFileExt(maps[I], '.script'), '', False);
//
//  if fVerbose then
//    writeln('VERBOSE: Checked ' + IntToStr(maps.Count) + ' maps in .\');
//
//  // Utils path
//  TKMapsCollection.GetAllMapPaths(ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\'), maps);
//
//  for I := 0 to maps.Count - 1 do
//    ValidateSingleScript(ChangeFileExt(maps[I], '.script'), '', False);
//
//  if fVerbose then
//    writeln('VERBOSE: Checked ' + IntToStr(maps.Count) + ' maps in ..\..\');
//
//  FreeAndNil(maps);
//end;


procedure TConsoleMain.GenerateAndSaveMapMinimapImage(aMapDatPath: string);
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


procedure TConsoleMain.SaveToFile(aFileName: string);
var
  I: Integer;
begin
  SaveToPng(fMinimap.MapX, fMinimap.MapY, fMinimap.Base, aFileName);
end;


procedure TConsoleMain.Start(aParameterRecord: TCLIParamRecord);
begin
  fVerbose := aParameterRecord.Verbose;



//  if aParameterRecord.AllMaps then
//    ValidateAllScripts
//  else
//    if aParameterRecord.XmlApi then
//      ValidateSingleScriptApi(aParameterRecord.ScriptFile)
//    else if aParameterRecord.Campaign then
//      ValidateCampaignScript(aParameterRecord.ScriptFile)
//    else // Default to single script
//  GetFileDirName
  GenerateAndSaveMapMinimapImage(aParameterRecord.MapDatPath);
end;


procedure TConsoleMain.ShowHelp;
begin
  writeln(VALIDATOR_HELP_TEXT);
end;


initialization
begin
  VALIDATOR_VERSION := VALIDATOR_VERSION_MAJOR + '.' +
                       VALIDATOR_VERSION_MINOR + ' - ' +
                       GAME_REVISION;
end;


end.

