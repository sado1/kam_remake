unit ScriptValidatorResult;
interface
uses
  SysUtils, KM_IoXML;


type
  TScriptValidatorIssue = record
    Line: Integer;
    Column: Integer;
    Module: string;
    Param: string;
    Msg: string;
  end;
  TScriptValidatorIssueArray = array of TScriptValidatorIssue;

  TScriptValidatorResult = class(TObject)
  strict private
    fHints: TScriptValidatorIssueArray;
    fWarnings: TScriptValidatorIssueArray;
    fErrors: TScriptValidatorIssueArray;
    procedure Add(aLine, aColumn: Integer; const aParam, aMessage: string; var aDest: TScriptValidatorIssueArray); inline;
    procedure ArrayToXML(aSrc: TScriptValidatorIssueArray; var aDest: TKMXmlNode);
    procedure XMLToArray(aSrc: TKMXmlNode; var aDest: TScriptValidatorIssueArray);
    function FixText(const aTest: string): string;
  public
    procedure AddHint(aLine, aColumn: Integer; const aParam, aMessage: string);
    procedure AddWarning(aLine, aColumn: Integer; const aParam, aMessage: string);
    procedure AddError(aLine, aColumn: Integer; const aParam, aMessage: string);
    function ToXML: string;
    procedure FromXML(const aXml: string);
    property Hints: TScriptValidatorIssueArray read fHints write fHints;
    property Warnings: TScriptValidatorIssueArray read fWarnings write fWarnings;
    property Errors: TScriptValidatorIssueArray read fErrors write fErrors;
  end;

implementation
uses
  Classes;

{ TScriptValidatorResult }
procedure TScriptValidatorResult.Add(aLine, aColumn: Integer; const aParam, aMessage: string; var aDest: TScriptValidatorIssueArray);
var
  I: Integer;
  Issue: TScriptValidatorIssue;
begin
  I := Length(aDest);
  SetLength(aDest, I + 1);
  Issue.Line   := aLine;
  Issue.Column := aColumn;
  Issue.Param  := aParam;
  Issue.Msg    := aMessage;
  aDest[I]     := Issue;
end;


procedure TScriptValidatorResult.AddHint(aLine, aColumn: Integer; const aParam, aMessage: string);
begin
  Add(aLine, aColumn, aParam, aMessage, fHints);
end;


procedure TScriptValidatorResult.AddWarning(aLine, aColumn: Integer; const aParam, aMessage: string);
begin
  Add(aLine, aColumn, aParam, aMessage, fWarnings);
end;


procedure TScriptValidatorResult.AddError(aLine, aColumn: Integer; const aParam, aMessage: string);
begin
  Add(aLine, aColumn, aParam, aMessage, fErrors);
end;


procedure TScriptValidatorResult.ArrayToXML(aSrc: TScriptValidatorIssueArray; var aDest: TKMXmlNode);
var
  Node:  TKMXmlNode;
  Issue: TScriptValidatorIssue;
begin
  for Issue in aSrc do
  begin
    Node := aDest.AddChild('Issue');
    Node.Attributes['Line'] := Issue.Line;
    Node.Attributes['Column'] := Issue.Column;
    Node.Attributes['Module'] := Issue.Module;
    Node.Attributes['Param'] := Issue.Param;
    Node.Attributes['Msg'] := Issue.Msg;
  end;
end;


procedure TScriptValidatorResult.XMLToArray(aSrc: TKMXmlNode; var aDest: TScriptValidatorIssueArray);
var
  I: Integer;
  Node: TKMXmlNode;
  Issue: TScriptValidatorIssue;
  Len: Integer;
begin
  for I := 0 to aSrc.ChildsCount - 1 do
  begin
    Node := aSrc.Childs[I];

    Len := Length(aDest);
    SetLength(aDest, Len + 1);
    Issue.Line   := Node.Attributes['Line'].AsInteger;
    Issue.Column := Node.Attributes['Column'].AsInteger;
    Issue.Module := FixText(Node.Attributes['Module'].AsString);
    Issue.Param  := FixText(Node.Attributes['Param'].AsString);
    Issue.Msg    := FixText(Node.Attributes['Msg'].AsString);
    aDest[Len]   := Issue;
  end;
end;


function TScriptValidatorResult.FixText(const aTest: string): string;
begin
  Result := StringReplace(aTest, '&#39;', '"', [rfReplaceAll, rfIgnoreCase]);
end;


function TScriptValidatorResult.ToXML: string;
var
  xmlDoc: TKMXmlDocument;
  nHint, nWarning, nError: TKMXmlNode;
begin
  xmlDoc := TKMXmlDocument.Create('ScriptValidatorResult');
  try
    nHint := xmlDoc.Root.AddChild('Hints');
    nWarning := xmlDoc.Root.AddChild('Warnings');
    nError := xmlDoc.Root.AddChild('Errors');

    ArrayToXML(fHints, nHint);
    ArrayToXML(fWarnings, nWarning);
    ArrayToXML(fErrors, nError);

    Result := xmlDoc.Xml;
  finally
    xmlDoc.Free;
  end;
end;


procedure TScriptValidatorResult.FromXML(const aXml: string);
var
  xmlDoc: TKMXmlDocument;
begin
  xmlDoc := TKMXmlDocument.Create();
  try
    xmlDoc.Xml := aXml;
    XMLToArray(xmlDoc.Root.FindNode('Hints'), fHints);
    XMLToArray(xmlDoc.Root.FindNode('Warnings'), fWarnings);
    XMLToArray(xmlDoc.Root.FindNode('Errors'), fErrors);
  finally
    xmlDoc.Free;
  end;
end;

end.
