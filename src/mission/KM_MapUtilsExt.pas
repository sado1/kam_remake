unit KM_MapUtilsExt;
interface
uses
  KM_Maps;

  function TryOpenMapPDF(aMap: TKMapInfo): Boolean;

implementation
uses
  KM_CommonShellUtils;


function TryOpenMapPDF(aMap: TKMapInfo): Boolean;
var
  pdfPath: string;
begin
  Result := False;
  if aMap = nil then Exit;

  pdfPath := aMap.DetermineReadmeFilePath;
  if pdfPath <> '' then
    Result := OpenPDF(pdfPath);
end;

end.
