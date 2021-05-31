unit KM_XmlHelper;
{$I KaM_Remake.inc}
interface
uses
  KM_IoXML;


type
  TXMLNodeHelper = class helper for TKMXmlNode
    function AddOrFindChild(const aChildNodeName: string): TKMXmlNode;
  end;

implementation
uses
  Classes, SysUtils;


{ TXMLNodeHelper }
function TXMLNodeHelper.AddOrFindChild(const aChildNodeName: string): TKMXmlNode;
begin
  if not HasChild(aChildNodeName) then
    Result := AddChild(aChildNodeName)
  else
    Result := ChildNodes.FindNode(aChildNodeName);
end;


end.
