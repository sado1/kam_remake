unit KM_ControlsButton;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls,
  KromOGLUtils,
  KM_Controls,
  KM_RenderUI,
  KM_ResFonts, KM_ResTypes,
  KM_CommonTypes;


type
  {3DButton}
  TKMButton = class(TKMControl)
  private
    fCaption: UnicodeString;
    fTextAlign: TKMTextAlign;
    fStyle: TKMButtonStyle;
    fRX: TRXType;
    fAutoHeight: Boolean; //Set button height automatically depending text size (height)
    procedure InitCommon(aStyle: TKMButtonStyle);
    procedure SetCaption(const aCaption: UnicodeString);
    procedure SetAutoHeight(aValue: Boolean);
    procedure UpdateHeight;
  public
    FlagColor: TColor4; //When using an image
    Font: TKMFont;
    MakesSound: Boolean;
    TexID: Word;
    CapOffsetX: Shortint;
    CapOffsetY: Shortint;
    ShowImageEnabled: Boolean; // show picture as enabled or not (normal or darkened)
    TextVAlign: TKMTextVAlign;
    AutoTextPadding: Byte;      //text padding for autoHeight
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID: Word; aRX: TRXType;
                       aStyle: TKMButtonStyle; aPaintLayer: Integer = 0); overload;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString;
                       aStyle: TKMButtonStyle; aPaintLayer: Integer = 0); overload;
    function Click: Boolean; //Try to click a button and return TRUE if succeded

    property Caption: UnicodeString read fCaption write SetCaption;
    property AutoHeight: Boolean read fAutoHeight write SetAutoHeight;

    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  {Common Flat Button}
  TKMButtonFlatCommon = class abstract(TKMControl)
  private
  public
    RX: TRXType;
    TexID: Word;
    TexOffsetX: Shortint;
    TexOffsetY: Shortint;
    CapOffsetX: Shortint;
    CapOffsetY: Shortint;
    Caption: UnicodeString;
    CapColor: TColor4;
    FlagColor: TColor4;
    Font: TKMFont;
    HideHighlight: Boolean;
    Clickable: Boolean; //Disables clicking without dimming

    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID: Integer; aRX: TRXType = rxGui);

    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    procedure Paint; override;
  end;


  {FlatButton}
  TKMButtonFlat = class(TKMButtonFlatCommon)
  public
    Down: Boolean;
    procedure Paint; override;
  end;


  {FlatButton with Shape on it}
  TKMFlatButtonShape = class(TKMControl)
  private
    fCaption: UnicodeString;
    fFont: TKMFont;
    fFontHeight: Byte;
  public
    Down: Boolean;
    FontColor: TColor4;
    ShapeColor: TColor4;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString; aFont: TKMFont; aShapeColor: TColor4);
    procedure Paint; override;
  end;


implementation
uses
  Math,
  KM_Sound,
  KM_Resource, KM_ResSound,
  KM_Defaults;



{ TKMButton }
constructor TKMButton.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID: Word; aRX: TRXType;
                             aStyle: TKMButtonStyle; aPaintLayer: Integer = 0);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight, aPaintLayer);
  InitCommon(aStyle);
  fRX   := aRX;
  TexID := aTexID;
end;


{Different version of button, with caption on it instead of image}
constructor TKMButton.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString;
                             aStyle: TKMButtonStyle; aPaintLayer: Integer = 0);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight, aPaintLayer);
  InitCommon(aStyle);
  Caption := aCaption;
end;


procedure TKMButton.InitCommon(aStyle: TKMButtonStyle);
begin
  TexID             := 0;
  Caption           := '';
  FlagColor         := $FFFF00FF;
  Font              := fntMetal;
  fTextAlign        := taCenter; //Thats default everywhere in KaM
  TextVAlign        := tvaMiddle;//tvaNone;
  fStyle            := aStyle;
  MakesSound        := True;
  ShowImageEnabled  := True;
  AutoHeight        := False;
  AutoTextPadding   := 5;
end;


procedure TKMButton.UpdateHeight;
var
  textY: Integer;
begin
  if fAutoHeight then
  begin
    textY := gRes.Fonts[Font].GetTextSize(Caption).Y;
    if textY + AutoTextPadding > Height then
      Height := textY + AutoTextPadding;
  end;
end;


procedure TKMButton.SetCaption(const aCaption: UnicodeString);
begin
  fCaption := aCaption;
  UpdateHeight;
end;


procedure TKMButton.SetAutoHeight(aValue: Boolean);
begin
  fAutoHeight := aValue;
  UpdateHeight;
end;


//DoClick is called by keyboard shortcuts
//It puts a focus on the button and depresses it if it was DoPress'ed
//It's important that Control must be:
// Visible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMButton.Click: Boolean;
begin
  if Visible and Enabled then
  begin
    //Mark self as CtrlOver and CtrlUp, don't mark CtrlDown since MouseUp manually Nils it
    Parent.MasterControl.CtrlOver := Self;
    Parent.MasterControl.CtrlUp := Self;
    if Assigned(OnClick) then
      OnClick(Self);
    Result := True; //Click has happened
  end
  else
    Result := False; //No, we couldn't click for Control is unreachable
end;


procedure TKMButton.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if Enabled and MakesSound and (csDown in State) then
    gSoundPlayer.Play(sfxnButtonClick);

  inherited;
end;


procedure TKMButton.Paint;
var
  col: TColor4;
  stateSet: TKMButtonStateSet;
  textY, top: Integer;
begin
  inherited;
  stateSet := [];
  if (csOver in State) and Enabled then
    stateSet := stateSet + [bsOver];
  if (csOver in State) and (csDown in State) then
    stateSet := stateSet + [bsDown];
  if not Enabled then
    stateSet := stateSet + [bsDisabled];

  TKMRenderUI.Write3DButton(AbsLeft, AbsTop, Width, Height, fRX, TexID, FlagColor, stateSet, fStyle, ShowImageEnabled);

  if TexID <> 0 then Exit;

  //If disabled then text should be faded
  col := IfThen(Enabled, icWhite, icGray);

  top := AbsTop + Byte(csDown in State) + CapOffsetY;

  textY := gRes.Fonts[Font].GetTextSize(Caption).Y;
  case TextVAlign of
    tvaNone:    Inc(top, (Height div 2) - 7);
    tvaTop:     Inc(top, 2);
    tvaMiddle:  Inc(top, (Height div 2) - (textY div 2) + 2);
    tvaBottom:  Inc(top, Height - textY);
  end;
  TKMRenderUI.WriteText(AbsLeft + Byte(csDown in State) + CapOffsetX, top,
                        Width, Caption, Font, fTextAlign, col);
end;


{TKMButtonFlatCommon}
constructor TKMButtonFlatCommon.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID: Integer; aRX: TRXType = rxGui);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  RX        := aRX;
  TexID     := aTexID;
  FlagColor := $FFFF00FF;
  CapColor  := $FFFFFFFF;
  Font      := fntGame;
  Clickable := True;
end;


procedure TKMButtonFlatCommon.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if not Clickable then Exit;
  if Enabled and (csDown in State) then
    gSoundPlayer.Play(sfxClick);

  inherited;
end;


procedure TKMButtonFlatCommon.Paint;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);

  if (csOver in State) and Enabled and not HideHighlight then
    TKMRenderUI.WriteShape(AbsLeft+1, AbsTop+1, Width-2, Height-2, $40FFFFFF);
end;


//Simple version of button, with a caption and image
{TKMButtonFlat}
procedure TKMButtonFlat.Paint;
var
  textCol: TColor4;
begin
  inherited;

  if TexID <> 0 then
    TKMRenderUI.WritePicture(AbsLeft + TexOffsetX,
                             AbsTop + TexOffsetY - 6 * Byte(Caption <> ''),
                             Width, Height, [], RX, TexID, Enabled or fEnabledVisually, FlagColor);

  textCol := IfThen(Enabled or fEnabledVisually, CapColor, icGray);
  TKMRenderUI.WriteText(AbsLeft + CapOffsetX, AbsTop + (Height div 2) + 4 + CapOffsetY, Width, Caption, Font, taCenter, textCol);

  if Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, 1, $FFFFFFFF);
  {if not fEnabled then
    TKMRenderUI.WriteShape(Left, Top, Width, Height, $80000000);}
end;


{ TKMFlatButtonShape }
constructor TKMFlatButtonShape.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; const aCaption: UnicodeString; aFont: TKMFont; aShapeColor: TColor4);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fCaption    := aCaption;
  ShapeColor  := aShapeColor;
  fFont       := aFont;
  fFontHeight := gRes.Fonts[fFont].BaseHeight + 2;
  FontColor   := icWhite;
end;


procedure TKMFlatButtonShape.Paint;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);

  //Shape within bevel
  TKMRenderUI.WriteShape(AbsLeft + 1, AbsTop + 1, Width - 2, Width - 2, ShapeColor);

  TKMRenderUI.WriteText(AbsLeft, AbsTop + (Height - fFontHeight) div 2,
                      Width, fCaption, fFont, taCenter, FontColor);

  if (csOver in State) and Enabled then
    TKMRenderUI.WriteShape(AbsLeft + 1, AbsTop + 1, Width - 2, Height - 2, $40FFFFFF);

  if (csDown in State) or Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, 1, icWhite);
end;



end.

