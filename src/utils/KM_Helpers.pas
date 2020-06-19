unit KM_Helpers;
interface
uses
  Vcl.StdCtrls, Vcl.Samples.Spin, ExtCtrls, KM_IoXML;


type
  TCheckBoxHelper = class helper for TCheckBox
    procedure SetStateWithoutClick(const aState: TCheckBoxState);
    procedure SetCheckedWithoutClick(const aChecked: Boolean);
  end;


  TEditHelper = class helper for TEdit
    procedure SetTextWithoutChange(const aText: string);
  end;


  TSpinEditHelper = class helper for TSpinEdit
    procedure SetValueWithoutChange(const aValue: Integer);
  end;


  TCategoryPanelHelper = class helper for TCategoryPanel
  private
    function GetXmlSectionName: string;
  public
    property XmlSectionName: string read GetXmlSectionName;
  end;


  TXMLNodeHelper = class helper for TXMLNode
    function AddOrFindChild(const aChildNodeName: string): TXMLNode;
  end;

implementation
uses
  Classes, SysUtils;


{ TCheckBoxHelper }
procedure TCheckBoxHelper.SetStateWithoutClick(const aState: TCheckBoxState);
var
  bckEvent: TNotifyEvent;
begin
  bckEvent := OnClick;
  OnClick := nil;
  try
    State := aState;
  finally
    OnClick := bckEvent;
  end;
end;


procedure TCheckBoxHelper.SetCheckedWithoutClick(const aChecked: Boolean);
var
  bckEvent: TNotifyEvent;
begin
  bckEvent := OnClick;
  OnClick := nil;
  try
    Checked := aChecked;
  finally
    OnClick := bckEvent;
  end;
end;


{ TEditHelper }
procedure TEditHelper.SetTextWithoutChange(const aText: string);
var
  bckEvent: TNotifyEvent;
begin
  bckEvent := OnChange;
  OnChange := nil;
  try
    Text := aText;
  finally
    OnChange := bckEvent;
  end;
end;


{ TSpinEditHelper }
procedure TSpinEditHelper.SetValueWithoutChange(const aValue: Integer);
var
  bckEvent: TNotifyEvent;
begin
  bckEvent := OnChange;
  OnChange := nil;
  try
    Value := aValue;
  finally
    OnChange := bckEvent;
  end;

end;


{ TCategoryPanelHelper }
function TCategoryPanelHelper.GetXmlSectionName: string;
begin
  Result := StringReplace(Caption, ' ', '_', [rfReplaceAll]);
end;


{ TXMLNodeHelper }
function TXMLNodeHelper.AddOrFindChild(const aChildNodeName: string): TXMLNode;
begin
  if not HasChild(aChildNodeName) then
    Result := AddChild(aChildNodeName)
  else
    Result := ChildNodes.FindNode(aChildNodeName);
end;


end.
