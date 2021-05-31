unit KM_IoXML;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,

  Xml.VerySimple; // (Attributes can be only 'string', which is not convenient)

  // We might try NativeXML (compatible with Android and iOS ?)

  (*
  {$IFDEF TABLET} Xml.omnixmldom, {$ENDIF}
  XMLDoc, XMLIntf; // uses DecimalSeparator which we do not control
  // Android requires to have a compatible XML "DOM Vendor", documentation suggests to use OmniXML
  *)

type
  TKMXmlDomDocument = TXmlVerySimple;
  TKMXmlNode = Xml.VerySimple.TXmlNode;
  TKMXmlAttribute = Xml.VerySimple.TXmlAttribute;

  //TXMLDocument = XMLIntf.IXMLDocument;
  //TXMLNode = XMLIntf.IXMLNode;

  TKMXmlDocument = class
  private
    fDocument: TKMXmlDomDocument;
    fRoot: TKMXmlNode;
  public
    constructor Create;
    destructor Destroy; override;

    property Root: TKMXmlNode read fRoot;

    procedure LoadFromFile(const aFilename: string; aReadOnly: Boolean = True);
    procedure SaveToFile(const aFilename: string; aCompressed: Boolean = False);
  end;


implementation


{ TKMXMLDocument }
constructor TKMXmlDocument.Create;
begin
  inherited;

  fDocument := TKMXmlDomDocument.Create;

  //fDocument := XMLDoc.TXMLDocument.Create(nil);
  //fDocument.Active := True;
  fDocument.Version := '1.0';
  fDocument.Encoding := 'UTF-8';
  fDocument.Options := [doNodeAutoIndent]; // Do not write BOM

  fRoot := fDocument.AddChild('Root');
end;


destructor TKMXmlDocument.Destroy;
begin
  fDocument.Free;

  //fDocument.Active := False;

  inherited;
end;


procedure TKMXmlDocument.LoadFromFile(const aFilename: string; aReadOnly: Boolean = True);
begin
  // When no file exists we create an empty XML and let caller handle it
  // e.g. by reading default values from it
  if FileExists(aFilename) then
    fDocument.LoadFromFile(aFilename);

  fRoot := fDocument.ChildNodes.FindNode('Root');

  // Create root if it's missing, so that XML could be processed and default parameters created
  if fRoot = nil then
    fRoot := fDocument.ChildNodes.Add('Root');
end;


procedure TKMXmlDocument.SaveToFile(const aFilename: string; aCompressed: Boolean = False);
begin
  ForceDirectories(ExtractFilePath(ExpandFileName(aFilename)));

  fDocument.SaveToFile(aFilename);
end;


end.
