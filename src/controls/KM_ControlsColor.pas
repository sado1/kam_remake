unit KM_ControlsColor;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls,
  KromOGLUtils,
  KM_Controls, KM_ControlsBase,
  KM_CommonTypes;


type
  { Color swatch - to select a color from given samples/palette }
  TKMColorSwatch = class(TKMControl)
  private
    fBackAlpha: single; //Alpha of background (usually 0.5, dropbox 1)
    fCellSize: Byte; //Size of the square in pixels
    fColumnCount: Byte;
    fRowCount: Byte;
    fColorIndex: Integer;
    Colors: array of TColor4;
    fOnChange: TNotifyEvent;
    fInclRandom: Boolean;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aColumnCount,aRowCount,aSize: Integer);
    procedure SetColors(const aColors: array of TColor4; aInclRandom: Boolean = False);
    procedure SelectByColor(aColor: TColor4);
    property BackAlpha: single read fBackAlpha write fBackAlpha;
    property ColorIndex: Integer read fColorIndex write fColorIndex;
    function GetColor: TColor4;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
  end;


//  //DropBox with a ColorSwatch
//  TKMDropColors = class(TKMControl)
//  private
//    fColorIndex: Integer;
//    fRandomCaption: UnicodeString;
//    fButton: TKMButton;
//    fSwatch: TKMColorSwatch;
//    fShape: TKMShape;
//    fOnChange: TNotifyEvent;
//    procedure ListShow(Sender: TObject);
//    procedure ListClick(Sender: TObject);
//    procedure ListHide(Sender: TObject);
//    procedure SetColorIndex(aIndex: Integer);
//    procedure UpdateDropPosition;
//  protected
//    procedure SetEnabled(aValue: Boolean); override;
//  public
//    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight,aCount: Integer);
//    property ColorIndex: Integer read fColorIndex write SetColorIndex;
//    procedure SetColors(const aColors: array of TColor4; const aRandomCaption: UnicodeString = '');
//    property OnChange: TNotifyEvent write fOnChange;
//    procedure Paint; override;
//  end;



implementation
uses
  Math,
  KM_RenderUI,
  KM_ResFonts, KM_ResTypes,
  KM_CommonUtils;


{ TKMColorSwatch }
constructor TKMColorSwatch.Create(aParent: TKMPanel; aLeft,aTop,aColumnCount,aRowCount,aSize: Integer);
begin
  inherited Create(aParent, aLeft, aTop, 0, 0);

  fBackAlpha    := 0.5;
  fColumnCount  := aColumnCount;
  fRowCount     := aRowCount;
  fCellSize     := aSize;
  fInclRandom   := false;
  fColorIndex   := -1;

  Width  := fColumnCount * fCellSize;
  Height := fRowCount * fCellSize;
end;


procedure TKMColorSwatch.SetColors(const aColors: array of TColor4; aInclRandom: Boolean = False);
begin
  fInclRandom := aInclRandom;
  if fInclRandom then
  begin
    SetLength(Colors, Length(aColors)+SizeOf(TColor4));
    Colors[0] := $00000000; //This one is reserved for random
    Move((@aColors[0])^, (@Colors[1])^, SizeOf(aColors));
  end
  else
  begin
    SetLength(Colors, Length(aColors));
    Move((@aColors[0])^, (@Colors[0])^, SizeOf(aColors));
  end;
end;


procedure TKMColorSwatch.SelectByColor(aColor: TColor4);
var
  I: Integer;
begin
  fColorIndex := -1;
  for I:=0 to Length(Colors)-1 do
    if Colors[I] = aColor then
      fColorIndex := I;
end;


function TKMColorSwatch.GetColor: TColor4;
begin
  if fColorIndex <> -1 then
    Result := Colors[fColorIndex]
  else
    Result := $FF000000; //Black by default
end;


procedure TKMColorSwatch.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var
  newColor: Integer;
begin
  if Button = mbLeft then
  begin
    newColor := EnsureRange((Y-AbsTop) div fCellSize, 0, fRowCount-1)*fColumnCount +
                EnsureRange((X-AbsLeft) div fCellSize, 0, fColumnCount-1);
    if InRange(newColor, 0, Length(Colors)-1) then
    begin
      fColorIndex := newColor;
      if Assigned(fOnChange) then fOnChange(Self);
    end;
  end;

  inherited;
end;


procedure TKMColorSwatch.Paint;
var
  I, start: Integer;
  selColor: TColor4;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, 1, fBackAlpha);

  start := 0;
  if fInclRandom then
  begin
    //Render miniature copy of all available colors with '?' on top
    for I := 0 to Length(Colors) - 1 do
      TKMRenderUI.WriteShape(AbsLeft+(I mod fColumnCount)*(fCellSize div fColumnCount)+2, AbsTop+(I div fColumnCount)*(fCellSize div fColumnCount)+2, (fCellSize div fColumnCount), (fCellSize div fColumnCount), Colors[I]);
    TKMRenderUI.WriteText(AbsLeft + fCellSize div 2, AbsTop + fCellSize div 4, 0, '?', fntMetal, taCenter);
    start := 1;
  end;

  for I := start to Length(Colors) - 1 do
    TKMRenderUI.WriteShape(AbsLeft+(I mod fColumnCount)*fCellSize, AbsTop+(I div fColumnCount)*fCellSize, fCellSize, fCellSize, Colors[I]);

  if fColorIndex < 0 then Exit;

  if GetColorBrightness(Colors[fColorIndex]) >= 0.5 then
    selColor := $FF000000
  else
    selColor := $FFFFFFFF;

  //Paint selection
  TKMRenderUI.WriteOutline(AbsLeft+(fColorIndex mod fColumnCount)*fCellSize, AbsTop+(fColorIndex div fColumnCount)*fCellSize, fCellSize, fCellSize, 1, selColor);
end;


{ TKMDropColors }
//constructor TKMDropColors.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight,aCount: Integer);
//var
//  MP: TKMPanel;
//  size: Integer;
//begin
//  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
//
//  fColorIndex := 0;
//  fRandomCaption := ''; //Disable random by default
//  OnClick := ListShow; //It's common behavior when click on dropbox will show the list
//
//  fButton := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 5, rxGui, bsMenu);
//  fButton.OnClick := ListShow;
//  fButton.MakesSound := false;
//
//  MP := MasterParent;
//  fShape := TKMShape.Create(MP, 0, 0, MP.Width, MP.Height);
//  fShape.OnClick := ListHide;
//
//  size := Round(Sqrt(aCount)+0.5); //Round up
//
//  fSwatch := TKMColorSwatch.Create(MP, 0, 0, size, size, aWidth div size);
//  fSwatch.BackAlpha := 0.75;
//  fSwatch.OnClick := ListClick;
//
//  ListHide(nil);
//end;
//
//
//procedure TKMDropColors.ListShow(Sender: TObject);
//begin
//  if fSwatch.Visible then
//  begin
//    ListHide(nil);
//    Exit;
//  end;
//
//  fSwatch.Show;
//  fShape.Show;
//end;
//
//
//procedure TKMDropColors.ListClick(Sender: TObject);
//begin
//  fColorIndex := fSwatch.ColorIndex;
//  if Assigned(fOnChange) then fOnChange(Self);
//  ListHide(nil);
//end;
//
//
//procedure TKMDropColors.ListHide(Sender: TObject);
//begin
//  fSwatch.Hide;
//  fShape.Hide;
//end;
//
//
//procedure TKMDropColors.SetEnabled(aValue: Boolean);
//begin
//  inherited;
//  fButton.Enabled := Enabled;
//  fSwatch.Enabled := Enabled;
//end;
//
//
////Set ColorIndex to fSwatch as well since it holds the actual color that we use on Paint
//procedure TKMDropColors.SetColorIndex(aIndex: Integer);
//begin
//  fColorIndex := aIndex;
//  fSwatch.ColorIndex := aIndex;
//end;
//
//
//procedure TKMDropColors.UpdateDropPosition;
//begin
//  fSwatch.Left := AbsLeft;
//  fSwatch.Top := AbsTop + Height;
//end;
//
//
//procedure TKMDropColors.SetColors(const aColors: array of TColor4; const aRandomCaption: UnicodeString = '');
//begin
//  //Store local copy of flag to substitute 0 color with "Random" text
//  fRandomCaption := aRandomCaption;
//  fSwatch.SetColors(aColors, (fRandomCaption <> ''));
//end;
//
//
//procedure TKMDropColors.Paint;
//var
//  col: TColor4;
//begin
//  inherited;
//
//  UpdateDropPosition;
//
//  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width-fButton.Width, Height);
//  TKMRenderUI.WriteShape(AbsLeft+2, AbsTop+1, Width-fButton.Width-3, Height-2, fSwatch.GetColor);
//
//  if (fRandomCaption <> '') and (fSwatch.ColorIndex = 0) then
//  begin
//    if Enabled then
//      col := $FFFFFFFF
//    else
//      col := $FF888888;
//    TKMRenderUI.WriteText(AbsLeft + 4, AbsTop + 3, 0, fRandomCaption, fntMetal, taLeft, col);
//  end;
//end;



end.

