unit KM_UnitVisual;
{$I KaM_Remake.inc}
interface
uses
  KM_Points, KM_Defaults;

type
  TKMUnitVisualState = record
    PosF: TKMPointF;
    Dir: TKMDirection;
    SlideX, SlideY: Single;
    Action: TKMUnitActionType;
    AnimStep: Integer;
    AnimFraction: Single;

    procedure SetFromUnit(aUnit: TObject);
  end;

  // Purely visual thing. Split from TKMUnit to aviod mixup of game-logic and render Positions
  TKMUnitVisual = class
  private
    fUnit: TObject;
    fCurr: TKMUnitVisualState;
    fPrev: TKMUnitVisualState;
  public
    constructor Create(aUnit: TObject);

    function GetLerp(aLag: Single): TKMUnitVisualState;
    procedure UpdateState;
  end;


implementation
uses
  KromUtils, Math, SysUtils,
  KM_Units;


{ TKMUnitVisualState }
procedure TKMUnitVisualState.SetFromUnit(aUnit: TObject);
var
  U: TKMUnit;
begin
  U := TKMUnit(aUnit);
  PosF := U.PositionF;
  Dir := U.Direction;
  SlideX := U.GetSlide(axX);
  SlideY := U.GetSlide(axY);
  AnimStep := U.AnimStep;
  AnimFraction := 0.0;

  if U.Action <> nil then
    Action := U.Action.ActionType
  else
    Action := uaUnknown;
end;


{ TKMUnitVisual }
constructor TKMUnitVisual.Create(aUnit: TObject);
begin
  inherited Create;

  fUnit := TKMUnit(aUnit);
  fPrev.SetFromUnit(fUnit);
  fCurr.SetFromUnit(fUnit);
end;


function TKMUnitVisual.GetLerp(aLag: Single): TKMUnitVisualState;
begin
  Result.PosF := KMLerp(fCurr.PosF, fPrev.PosF, aLag);
  Result.SlideX := KromUtils.Lerp(fCurr.SlideX, fPrev.SlideX, aLag);
  Result.SlideY := KromUtils.Lerp(fCurr.SlideY, fPrev.SlideY, aLag);
  //If there's no lag, use the current state
  if aLag = 0.0 then
  begin
    Result.Dir := fCurr.Dir;
    Result.Action := fCurr.Action;
    Result.AnimStep := fCurr.AnimStep;
    Result.AnimFraction := 0.0;
  end
  else
  begin
    //Don't start a new action or change direction until the last one is 100% finished
    Result.Dir := fPrev.Dir;
    Result.Action := fPrev.Action;
    Result.AnimStep := fPrev.AnimStep;
    Result.AnimFraction := 0.0;
    //If action/dir/step is consistent we can interpolate the animation
    if (fCurr.Action = fPrev.Action) and (fCurr.Dir = fPrev.Dir) and (fCurr.AnimStep - fPrev.AnimStep = 1) then
      Result.AnimFraction := 1.0 - aLag;
  end;
end;


procedure TKMUnitVisual.UpdateState;
begin
  fPrev := fCurr;
  fCurr.SetFromUnit(fUnit);
end;

end.
