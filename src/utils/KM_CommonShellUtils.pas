unit KM_CommonShellUtils;
{$I KaM_Remake.inc}
interface

  function OpenPDF(const aURL: string): Boolean;

implementation
uses
  {$IFDEF MSWindows}Windows, {$ENDIF}
  Forms
  {$IFDEF WDC}, ShellApi {$ENDIF}
  ;


function OpenPDF(const aURL: string): Boolean;
begin
  if aURL = '' then Exit(False);

  {$IFDEF WDC}
  Result := ShellExecute(Application.Handle, 'open', PChar(aURL), nil, nil, SW_SHOWNORMAL) > 32;
  {$ENDIF}

  {$IFDEF FPC}
  Result := OpenDocument(aURL);
  {$ENDIF}
end;


end.
