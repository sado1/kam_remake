unit KM_KeysSettings;
{$I KaM_Remake.inc}
interface
uses
  KM_GameAppSettings;

type
  // Hotkey settings loader/saver
  TKMKeysSettings = class(TKMGameAppSettingsPart)
  public
    procedure LoadFromXML; override;
    procedure SaveToXML; override;
  end;

var
  gKeySettings: TKMKeysSettings;


implementation
uses
  KM_ResTexts,
  KM_ResKeys,
  KM_ResKeyFuncs,
  KM_ResTypes,
  KM_IoXML;


{ TKMKeysSettings }
procedure TKMKeysSettings.LoadFromXML;
var
  KF: TKMKeyFunction;
  nHotkeys, nKey: TKMXmlNode;
  keyFuncName: string;
begin
  if Self = nil then Exit;
  inherited;

  nHotkeys := Root.AddOrFindChild('Hotkeys');

  for KF := KEY_FUNC_LOW to High(TKMKeyFunction) do
  begin
    keyFuncName := GetKeyFunctionStr(KF);
    if nHotkeys.HasChild(keyFuncName) then
    begin
      nKey := nHotkeys.AddOrFindChild(keyFuncName);
      if nKey.HasAttribute('Key') then
        gResKeys[KF] := nKey.Attributes['Key'].AsInteger;
    end;
  end;
end;


procedure TKMKeysSettings.SaveToXML;
var
  KF: TKMKeyFunction;
  nHotkeys, nKey: TKMXmlNode;
begin
  if Self = nil then Exit;
  inherited;

  nHotkeys := Root.AddOrFindChild('Hotkeys');

  for KF := KEY_FUNC_LOW to High(TKMKeyFunction) do
  begin
    nKey := nHotkeys.AddOrFindChild(GetKeyFunctionStr(KF));
    nKey.Attributes['Key'] := gResKeys[KF];

    // These are just comments
    nKey.Attributes['KeyDesc'] := gResKeys.GetKeyName(gResKeys[KF]);
    nKey.Attributes['FuncDesc'] := gResTexts[gResKeyFuncs[KF].TextId];
  end;
end;


end.

