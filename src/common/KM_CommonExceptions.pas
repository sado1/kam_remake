unit KM_CommonExceptions;
interface
uses
  SysUtils, KM_Points;

type
  //Custom Exception that includes a TKMPoint
  ELocError = class(Exception)
    Loc: TKMPoint;
    constructor Create(const aMsg: UnicodeString; const aLoc: TKMPoint);
  end;

  //Error while game initialization
  EGameInitError = class(Exception);

implementation


{ ELocError }
constructor ELocError.Create(const aMsg: UnicodeString; const aLoc: TKMPoint);
begin
  inherited Create(aMsg);
  Loc := aLoc;
end;


end.
