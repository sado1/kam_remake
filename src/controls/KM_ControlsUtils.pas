unit KM_ControlsUtils;
{$I KaM_Remake.inc}
interface
uses
  KM_ControlsTypes;



  // Check if specified aChar is allowed for specified aAllowedChars type
  function IsCharAllowed(aChar: WideChar; aAllowedChars: TKMAllowedChars): Boolean;
  

implementation
uses
  Math,
  KromUtils;


// Check if specified aChar is allowed for specified aAllowedChars type
function IsCharAllowed(aChar: WideChar; aAllowedChars: TKMAllowedChars): Boolean;
const
  HexDigits:        TSetOfAnsiChar = [#48..#57,#65..#70,#97..#102]; //0..9 A..F a..f
  Ansi7Chars:       TSetOfAnsiChar = [#32..#123,#125..#126]; //except | character
  NonFileChars:     TSetOfAnsiChar = [#0 .. #31, '<', '>', #176, '|', '"', '\', '/', ':', '*', '?'];
  NonTextCharsWEOL: TSetOfAnsiChar = [#0 .. #31, #176, '|']; //° has negative width so acts like a backspace in KaM fonts
  NonTextChars:     TSetOfAnsiChar = [#0 .. #31, #176]; //° has negative width so acts like a backspace in KaM fonts
begin
  Result := not ((aAllowedChars = acDigits)   and not InRange(Ord(aChar), 48, 57)
              or (aAllowedChars = acHex)      and not CharInSet(aChar, HexDigits)
              or (aAllowedChars = acANSI7)    and not CharInSet(aChar, Ansi7Chars)
              or (aAllowedChars = acFileName) and CharInSet(aChar, NonFileChars)
              or (aAllowedChars = acText)     and CharInSet(aChar, NonTextCharsWEOL)
              or (aAllowedChars = acAll)      and CharInSet(aChar, NonTextChars));
end;


end.

