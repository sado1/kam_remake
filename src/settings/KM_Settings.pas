unit KM_Settings;
{$I KaM_Remake.inc}
interface

type
  // Abstract settings entity
  TKMSettings = class abstract
  protected
    fNeedsSave: Boolean;

    procedure Changed;
    function GetDefaultSettingsName: string; virtual; abstract;
    function NeedToSave: Boolean; virtual;

    procedure LoadFromFile(const aPath: string); virtual; abstract;
    procedure SaveToFile(const aPath: string); virtual; abstract;

    procedure LoadFromDefaultFile;
    procedure SaveToDefaultFile;

    function GetSettingsName: string; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ReloadSettings;
    procedure SaveSettings(aForce: Boolean = False);
  end;


implementation
uses
  SysUtils,
  KM_Defaults,
  KM_FileIO,
  KM_Log;


{ TKMSettings }
constructor TKMSettings.Create;
begin
  inherited;

  LoadFromDefaultFile;
  // Save settings to default directory immidiately
  // If there were any problems with settings then we want to be able to customise them
  SaveToDefaultFile;

  fNeedsSave := False;
end;


destructor TKMSettings.Destroy;
begin
  SaveToDefaultFile;

  inherited;
end;


procedure TKMSettings.LoadFromDefaultFile;
var
  path: string;
begin
  path := GetDocumentsSavePath + GetDefaultSettingsName;
  LoadFromFile(path);
  gLog.AddTime(GetSettingsName + ' loaded from ' + path);
end;


procedure TKMSettings.SaveToDefaultFile;
var
  path: string;
begin
  path := GetDocumentsSavePath;
  ForceDirectories(path);
  SaveToFile(path + GetDefaultSettingsName);
  gLog.AddTime(GetSettingsName + ' saved to ' + path);
end;


function TKMSettings.NeedToSave: Boolean;
begin
  Result := fNeedsSave;
end;


procedure TKMSettings.ReloadSettings;
begin
  LoadFromDefaultFile;
end;


procedure TKMSettings.SaveSettings(aForce: Boolean);
begin
  if SKIP_SETTINGS_SAVE then Exit;

  if NeedToSave or aForce then
    SaveToDefaultFile;
end;


procedure TKMSettings.Changed;
begin
  fNeedsSave := True;
end;


end.

