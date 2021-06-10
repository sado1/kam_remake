unit KM_SettingsDev;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, StrUtils, Classes, Math,
  ComCtrls, Controls, ExtCtrls, StdCtrls,
  {$IFDEF MSWindows} Vcl.Samples.Spin; {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType; {$ENDIF}


type
  { TKMDevSettings }
  TKMDevSettings = class
  private
    fMainGroup: TCategoryPanelGroup;
    fDontCollapse: TCategoryPanel;

    function GetPath: UnicodeString;
    procedure DoLoad;
    procedure DoSave;
  public
    constructor Create(aMainGroup: TCategoryPanelGroup; aDontCollapse: TCategoryPanel);
    procedure Load;
    procedure Save;
  end;


implementation
uses
  KM_Defaults, KM_Log, KM_IoXML,
  KM_VclHelpers;



function TKMDevSettings.GetPath: UnicodeString;
begin
  Result := ExeDir + DEV_SETTINGS_XML_FILENAME;
end;


// Load dev settings from kmr_dev.xml
constructor TKMDevSettings.Create(aMainGroup: TCategoryPanelGroup; aDontCollapse: TCategoryPanel);
begin
  inherited Create;

  fMainGroup := aMainGroup;
  fDontCollapse := aDontCollapse;
end;


procedure TKMDevSettings.DoLoad;

  procedure ManageSubPanel(aPanel: TWinControl; anParent: TKMXmlNode);
  var
    I: Integer;
    actrl: TControl;
    nSection: TKMXmlNode;
  begin
    for I := 0 to aPanel.ControlCount - 1 do
    begin
      actrl := aPanel.Controls[I];

      if aCtrl is TGroupBox then
        ManageSubPanel(TGroupBox(aCtrl), anParent)
      else
        if (aCtrl is TCheckBox)
        or (aCtrl is TTrackBar)
        or (aCtrl is TRadioGroup)
        or (aCtrl is TSpinEdit)
        or (aCtrl is TEdit) then
        if anParent.HasChild(actrl.Name) then
        begin
          nSection := anParent.FindNode(actrl.Name); // Add section only if its needed

          if (aCtrl is TCheckBox) and nSection.HasAttribute('Checked') then
            TCheckBox(aCtrl).Checked := nSection.Attributes['Checked'].AsBoolean
          else
          if (aCtrl is TTrackBar) and nSection.HasAttribute('Position') then
            TTrackBar(aCtrl).Position := nSection.Attributes['Position'].AsInteger
          else
          if (aCtrl is TRadioGroup) and nSection.HasAttribute('ItemIndex') then
            TRadioGroup(aCtrl).ItemIndex := nSection.Attributes['ItemIndex'].AsInteger
          else
          if (aCtrl is TSpinEdit) and nSection.HasAttribute('Value') then
            TSpinEdit(aCtrl).Value := nSection.Attributes['Value'].AsInteger
          else
          if (aCtrl is TEdit) and nSection.HasAttribute('Text') then
            TEdit(aCtrl).Text := nSection.Attributes['Text'].AsString;
        end;
    end;
  end;

var
  I: Integer;
  devSettingsPath: UnicodeString;
  newXML: TKMXMLDocument;
  cp: TCategoryPanel;
  cpSurface: TCategoryPanelSurface;
  cpName: string;
  nRoot, nSection: TKMXmlNode;
begin
  devSettingsPath := GetPath;
  gLog.AddTime('Loading dev settings from file ''' + devSettingsPath + '''');

  // Apply default settings
  if not FileExists(devSettingsPath) then
  begin
    for I := 0 to fMainGroup.Panels.Count - 1 do
      TCategoryPanel(fMainGroup.Panels[I]).Collapsed := True;

    fDontCollapse.Collapsed := False; //The only not collapsed section
    Exit;
  end;

  //Load dev data from XML
  newXML := TKMXMLDocument.Create;
  newXML.LoadFromFile(devSettingsPath);
  nRoot := newXML.Root;

  for I := 0 to fMainGroup.Panels.Count - 1 do
  begin
    cp := TCategoryPanel(fMainGroup.Panels[I]);
    cpName := cp.XmlSectionName;

    if nRoot.HasChild(cpName) then
    begin
      nSection := nRoot.FindNode(cpName);
      cp.Collapsed := nSection.Attributes['Collapsed'].AsBoolean(True);

      if (cp.ControlCount > 0) and (cp.Controls[0] is TCategoryPanelSurface) then
      begin
        cpSurface := TCategoryPanelSurface(cp.Controls[0]);
        ManageSubPanel(cpSurface, nSection);
      end;
    end;
  end;

  newXML.Free;
end;


// Save dev settings to kmr_dev.xml
procedure TKMDevSettings.DoSave;

  procedure ManageSubPanel(aPanel: TWinControl; anParent: TKMXmlNode);
  var
    I: Integer;
    actrl: TControl;
    nSection: TKMXmlNode;
  begin
    for I := 0 to aPanel.ControlCount - 1 do
    begin
      actrl := aPanel.Controls[I];

      if aCtrl is TGroupBox then
        ManageSubPanel(TGroupBox(aCtrl), anParent)
      else
      if   (aCtrl is TCheckBox)
        or (aCtrl is TTrackBar)
        or (aCtrl is TRadioGroup)
        or (aCtrl is TSpinEdit)
        or (aCtrl is TEdit) then
      begin
        nSection := anParent.AddOrFindChild(actrl.Name); // Add section only if its needed
        if aCtrl is TCheckBox then
          nSection.Attributes['Checked'] := TCheckBox(aCtrl).Checked
        else
        if aCtrl is TTrackBar then
          nSection.Attributes['Position'] := TTrackBar(aCtrl).Position
        else
        if aCtrl is TRadioGroup then
          nSection.Attributes['ItemIndex'] := TRadioGroup(aCtrl).ItemIndex
        else
        if aCtrl is TSpinEdit then
          nSection.Attributes['Value'] := TSpinEdit(aCtrl).Value
        else
        if aCtrl is TEdit then
          nSection.Attributes['Text'] := TEdit(aCtrl).Text;
      end;
    end;
  end;

var
  I: Integer;
  devSettingsPath: UnicodeString;
  newXML: TKMXMLDocument;
  cp: TCategoryPanel;
  cpSurface: TCategoryPanelSurface;
  nRoot, nSection: TKMXmlNode;
begin
  devSettingsPath := GetPath;

  gLog.AddTime('Saving dev settings to file ''' + devSettingsPath + '''');

  //Save dev data to XML
  newXML := TKMXMLDocument.Create;
  newXML.LoadFromFile(devSettingsPath);
  nRoot := newXML.Root;

  for I := 0 to fMainGroup.Panels.Count - 1 do
  begin
    cp := TCategoryPanel(fMainGroup.Panels[I]);

    nSection := nRoot.AddOrFindChild(cp.XmlSectionName);

    nSection.Attributes['Collapsed'] := cp.Collapsed;

    if (cp.ControlCount > 0) and (cp.Controls[0] is TCategoryPanelSurface) then
    begin
      cpSurface := TCategoryPanelSurface(cp.Controls[0]);
      ManageSubPanel(cpSurface, nSection);
    end;
  end;

  newXML.SaveToFile(devSettingsPath);
  newXML.Free;
end;


// Load dev settings from kmr_dev.xml
procedure TKMDevSettings.Load;
begin
  {$IFDEF DEBUG}
  // allow crash while debugging
  DoLoad;
  {$ELSE}
  try
    // Skip crash on released version, only log the error
    DoLoad;
  except
    on E: Exception do
      gLog.AddTime('Error while loading dev settings from ''' + GetPath + ''':' + sLineBreak + E.Message
          {$IFDEF WDC}+ sLineBreak + E.StackTrace{$ENDIF}
        );
  end;
  {$ENDIF}
end;


// Save dev settings to kmr_dev.xml
procedure TKMDevSettings.Save;
begin
  {$IFDEF DEBUG}
  // allow crash while debugging
  DoSave;
  {$ELSE}
  try
    // Skip crash on released version, only log the error
    DoSave;
  except
    on E: Exception do
      gLog.AddTime('Error while saving dev settings to ''' + GetPath + ''':' + sLineBreak + E.Message
          {$IFDEF WDC}+ sLineBreak + E.StackTrace{$ENDIF}
        );
  end;
  {$ENDIF}
end;


end.
