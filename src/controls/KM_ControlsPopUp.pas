unit KM_ControlsPopUp;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  Controls,
  KromOGLUtils,
  KM_Controls, KM_ControlsBase, KM_ControlsList,
  KM_ResFonts,
  KM_Points,
  KM_CommonTypes;


type
  TKMPopUpMenu = class(TKMPanel)
  private
    fShapeBG: TKMShape;
    fList: TKMColumnBox;
    procedure MenuHide(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure SetItemIndex(aValue: Integer);
    function GetItemIndex: Integer;
    function GetItemTag(aIndex: Integer): Integer;
  public
    constructor Create(aParent: TKMPanel; aWidth: Integer);
    procedure AddItem(const aCaption: UnicodeString; aTag: Integer = 0);
    procedure UpdateItem(aIndex: Integer; const aCaption: UnicodeString);
    procedure Clear;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemTags[aIndex: Integer]: Integer read GetItemTag;
    procedure ShowAt(X,Y: Integer);
    procedure HideMenu;
  end;


  TKMPopUpBGImageType = (pubgitGray, pubgitYellow, pubgitScroll);

  TKMPopUpPanel = class(TKMPanel)
  const
    DEF_FONT: TKMFont = fntOutline;
  private
    fDragging: Boolean;
    fDragStartPos: TKMPoint;
    fBGImageType: TKMPopUpBGImageType;
    fHandleCloseKey: Boolean;
    fOnClose: TEvent;
    procedure UpdateSizes;
    procedure Close(Sender: TObject);

    function GetLeftRightMargin: Integer;
    function GetTopMargin: Integer;
    function GetBottomMargin: Integer;
    function GetCrossTop: Integer;
    function GetCrossRight: Integer;

    function GetActualWidth: Integer;
    procedure SetActualWidth(aValue: Integer);
    function GetActualHeight: Integer;
    procedure SetActualHeight(aValue: Integer);

    procedure SetHandleCloseKey(aValue: Boolean);
  protected
    BevelBG: TKMBevel;
    BevelShade: TKMBevel;
    procedure SetWidth(aValue: Integer); override;
    procedure SetHeight(aValue: Integer); override;
  public
    ItemsPanel: TKMPanel;
    DragEnabled: Boolean;
    ImageBG, ImageClose: TKMImage;
    Caption: UnicodeString;
    Font: TKMFont;
    FontColor: TColor4;
    CapOffsetY: Integer;

    constructor Create(aParent: TKMPanel; aWidth, aHeight: Integer; const aCaption: UnicodeString = '';
                       aImageType: TKMPopUpBGImageType = pubgitYellow; aWithCrossImg: Boolean = False;
                       aShowBevel: Boolean = True; aShowShadeBevel: Boolean = True);

    procedure MouseDown (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove (X,Y: Integer; Shift: TShiftState); override;
    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    function KeyUp(Key: Word; Shift: TShiftState): Boolean; override;

    procedure ControlMouseMove(Sender: TObject; X,Y: Integer; Shift: TShiftState);

    procedure ControlMouseDown(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
    procedure ControlMouseUp(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);

    property OnClose: TEvent read fOnClose write fOnClose;

    property ActualHeight: Integer read GetActualHeight write SetActualHeight;
    property ActualWidth: Integer read GetActualWidth write SetActualWidth;

    property HandleCloseKey: Boolean read fHandleCloseKey write SetHandleCloseKey;

    procedure Paint; override;
  end;


  //Form that can be dragged around (and resized?)
//  TKMForm = class(TKMPanel)
//  private
//    fHeaderHeight: Integer;
//    fButtonClose: TKMButtonFlat;
//    fLabelCaption: TKMLabel;
//
//    fDragging: Boolean;
//    fOffsetX: Integer;
//    fOffsetY: Integer;
//    function HitHeader(X, Y: Integer): Boolean;
//    procedure FormCloseClick(Sender: TObject);
//    function GetCaption: UnicodeString;
//    procedure SetCaption(const aValue: UnicodeString);
//  public
//    OnMove: TNotifyEvent;
//    OnClose: TNotifyEvent;
//
//    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
//    property Caption: UnicodeString read GetCaption write SetCaption;
//    procedure MouseDown (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
//    procedure MouseMove (X,Y: Integer; Shift: TShiftState); override;
//    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
//    procedure PaintPanel(aPaintLayer: TKMPaintLayer); override;
//  end;


implementation
uses
  Math,
  KM_RenderUI,
  KM_ResTexts, KM_ResKeys, KM_ResTypes,
  KM_Defaults;
//  {$IFDEF MSWindows} Windows, {$ENDIF}
//  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
//  SysUtils, StrUtils, Math,
//  Clipbrd,
//  KromUtils,
//  KM_System,
//  KM_Resource, KM_ResSprites, KM_ResSound, KM_ResTexts, KM_ResKeys,
//  KM_Render, KM_RenderTypes,
//  KM_Sound, KM_CommonUtils, KM_UtilsExt,
//  KM_GameSettings, KM_InterfaceTypes;


{ TKMPopUpMenu }
constructor TKMPopUpMenu.Create(aParent: TKMPanel; aWidth: Integer);
begin
  inherited Create(aParent, 0, 0, aWidth, 0);

  fShapeBG := TKMShape.Create(Self, 0, 0, aParent.Width, aParent.Height);
  fShapeBG.AnchorsStretch;
  fShapeBG.OnClick := MenuHide;
  fShapeBG.Hide;

  fList := TKMColumnBox.Create(Self, 0, 0, aWidth, 0, fntGrey, bsMenu);
  fList.AnchorsStretch;
  fList.BackAlpha := 0.8;
  fList.Focusable := False;
  fList.SetColumns(fntGrey, [''], [0]);
  fList.ShowHeader := False;
  fList.OnClick := MenuClick;
  fList.Hide;

  Hide;
end;


procedure TKMPopUpMenu.Clear;
begin
  fList.Clear;
end;


function TKMPopUpMenu.GetItemIndex: Integer;
begin
  Result := fList.ItemIndex;
end;


function TKMPopUpMenu.GetItemTag(aIndex: Integer): Integer;
begin
  Result := fList.Rows[aIndex].Tag;
end;


procedure TKMPopUpMenu.SetItemIndex(aValue: Integer);
begin
  fList.ItemIndex := aValue;
end;


procedure TKMPopUpMenu.AddItem(const aCaption: UnicodeString; aTag: Integer = 0);
begin
  fList.AddItem(MakeListRow([aCaption], aTag));
  Height := fList.ItemHeight * fList.RowCount;
end;


procedure TKMPopUpMenu.UpdateItem(aIndex: Integer; const aCaption: UnicodeString);
begin
  fList.Rows[aIndex].Cells[0].Caption := aCaption;
end;


procedure TKMPopUpMenu.MenuClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(Self);

  MenuHide(Self);
end;


procedure TKMPopUpMenu.MenuHide(Sender: TObject);
begin
  Hide;
  fList.Hide;
  fShapeBG.Hide;
end;


procedure TKMPopUpMenu.HideMenu;
begin
  MenuHide(nil);
end;


procedure TKMPopUpMenu.ShowAt(X, Y: Integer);
begin
  fList.AbsLeft := X;
  fList.AbsTop := Y;

  //Reset previously selected item
  fList.ItemIndex := -1;

  Show;
  fShapeBG.Show;
  fList.Show;
end;


{ TKMPopUpPanel }
// aWidth / aHeight represents not TKMPopUpPanel sizes, but its internal panel: ItemsPanel
// PopUpPanel draw bigger image behind it
constructor TKMPopUpPanel.Create(aParent: TKMPanel; aWidth, aHeight: Integer; const aCaption: UnicodeString = '';
                                 aImageType: TKMPopUpBGImageType = pubgitYellow; aWithCrossImg: Boolean = False;
                                 aShowBevel: Boolean = True; aShowShadeBevel: Boolean = True);
var
  margin, l, t, topMarg, baseW, baseH, w, h: Integer;
begin
  fBGImageType := aImageType;

  margin := GetLeftRightMargin;

  baseW := aWidth + 2*margin;
  topMarg := GetTopMargin;
  baseH := aHeight + GetBottomMargin + topMarg;
  w := Min(aParent.Width, baseW);
  h := Min(aParent.Height, baseH);
  l := Max(0, (aParent.Width - w) div 2);
  t := Max(0, (aParent.Height - h) div 2);

  // Create panel with calculated sizes
  inherited Create(aParent, l, t, w, h);

  // Fix its base sizes as a desired one
  BaseWidth := baseW;
  BaseHeight := baseH;

  FitInParent := True;
  Font := DEF_FONT;
  FontColor := icWhite;
  Caption := aCaption;
  DragEnabled := False;
  fHandleCloseKey := False;

  if aShowShadeBevel then
    BevelShade := TKMBevel.Create(Self, -2000,  -2000, 5000, 5000);

  ImageBG := TKMImage.Create(Self, 0, 0, w, h, 15, rxGuiMain);

  ItemsPanel := TKMPanel.Create(Self, margin, topMarg, Width - 2*margin, Height - topMarg - GetBottomMargin);

  case fBGImageType of
    pubgitGray:   ImageBG.TexId := 15;
    pubgitYellow: ImageBG.TexId := 18;
    pubgitScroll: begin
                    ImageBG.Rx := rxGui;
                    ImageBG.TexId := 409;
                  end;
  end;

  if aWithCrossImg then
  begin
    ImageClose := TKMImage.Create(Self, Width - GetCrossRight, GetCrossTop, 31, 30, 52);
    ImageClose.Anchors := [anTop, anRight];
    ImageClose.Hint := gResTexts[TX_MSG_CLOSE_HINT];
    ImageClose.OnClick := Close;
    ImageClose.HighlightOnMouseOver := True;
  end;

  ItemsPanel.AnchorsStretch;
  if aShowBevel then
  begin
    BevelBG := TKMBevel.Create(ItemsPanel, 0, 0, ItemsPanel.Width, ItemsPanel.Height);
    BevelBG.AnchorsStretch;
  end;

  ImageBG.ImageStretch;

  AnchorsCenter;
  Hide;

  // Subscribe to get other controls mouse move events
  fMasterControl.AddMouseMoveCtrlSub(ControlMouseMove);

  // Subscribe to get other controls mouse down events
  fMasterControl.AddMouseDownCtrlSub(ControlMouseDown);

    // Subscribe to get other controls mouse up events
  fMasterControl.AddMouseUpCtrlSub(ControlMouseUp);
end;


function TKMPopUpPanel.GetLeftRightMargin: Integer;
begin
  Result := 0;
  case fBGImageType of
    pubgitGray:   Result := 20;
    pubgitYellow: Result := 35;
    pubgitScroll: Result := 20;
  end;
end;


function TKMPopUpPanel.GetTopMargin: Integer;
begin
  Result := 0;
  case fBGImageType of
    pubgitGray:   Result := 40;
    pubgitYellow: Result := 80;
    pubgitScroll: Result := 50;
  end;
end;


function TKMPopUpPanel.GetBottomMargin: Integer;
begin
  Result := 0;
  case fBGImageType of
    pubgitGray:   Result := 20;
    pubgitYellow: Result := 50;
    pubgitScroll: Result := 20;
  end;
end;


function TKMPopUpPanel.GetCrossTop: Integer;
begin
  Result := 0;
  case fBGImageType of
    pubgitGray:   Result := 24;
    pubgitYellow: Result := 40;
    pubgitScroll: Result := 24;
  end;
end;


function TKMPopUpPanel.GetCrossRight: Integer;
begin
  Result := 0;
  // We probably should calc those sizes as dependant of the Width
  case fBGImageType of
    pubgitGray:   Result := 50;
    pubgitYellow: Result := 130;
    pubgitScroll: Result := 55;
  end;
end;


procedure TKMPopUpPanel.Close(Sender: TObject);
begin
  Hide;

  if Assigned(fOnClose) then
    fOnClose;
end;


procedure TKMPopUpPanel.ControlMouseDown(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if Sender = ImageBG then
    MouseDown(X, Y, Shift, Button);
end;


procedure TKMPopUpPanel.ControlMouseMove(Sender: TObject; X, Y: Integer; Shift: TShiftState);
begin
  inherited;

  MouseMove(X, Y, Shift);
end;


procedure TKMPopUpPanel.ControlMouseUp(Sender: TObject; X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  MouseUp(X, Y, Shift, Button);
end;


procedure TKMPopUpPanel.MouseDown(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;

  if not DragEnabled then Exit;

  fDragging := True;
  fDragStartPos := TKMPoint.New(X,Y);
end;

procedure TKMPopUpPanel.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  inherited;

  if not DragEnabled or not fDragging then Exit;

  Left := EnsureRange(Left + X - fDragStartPos.X, 0, fMasterControl.MasterPanel.Width - Width);
  Top := EnsureRange(Top + Y - fDragStartPos.Y, -ImageBG.Top, fMasterControl.MasterPanel.Height - Height);

  fDragStartPos := TKMPoint.New(X,Y);
end;

procedure TKMPopUpPanel.MouseUp(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;

  if not DragEnabled then Exit;

  fDragging := False;
end;


function TKMPopUpPanel.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := inherited;
  if Result then Exit; // Key already handled

  if not fHandleCloseKey then Exit;

  if Key = gResKeys[kfCloseMenu] then
  begin
    Close(nil);
    Result := True;
  end;
end;


procedure TKMPopUpPanel.Paint;
begin
  inherited;

  TKMRenderUI.WriteText(AbsLeft, AbsTop + GetTopMargin - 25 + CapOffsetY, Width, Caption, Font, taCenter, FontColor);
end;


procedure TKMPopUpPanel.SetHeight(aValue: Integer);
begin
  inherited;

  UpdateSizes;
end;


procedure TKMPopUpPanel.SetWidth(aValue: Integer);
begin
  inherited;

  UpdateSizes;
end;


procedure TKMPopUpPanel.UpdateSizes;
begin
  ImageBG.Width := Width;
  ImageBG.Height := Height;
end;


function TKMPopUpPanel.GetActualWidth: Integer;
begin
  Result := ItemsPanel.Width;
end;


procedure TKMPopUpPanel.SetActualWidth(aValue: Integer);
var
  baseW: Integer;
begin
  baseW := aValue + GetLeftRightMargin*2;
  SetWidth(Min(Parent.Width, baseW));
end;


function TKMPopUpPanel.GetActualHeight: Integer;
begin
  Result := ItemsPanel.Height;
end;


procedure TKMPopUpPanel.SetActualHeight(aValue: Integer);
var
  baseH, h: Integer;
begin
  baseH := aValue + GetBottomMargin + GetTopMargin;
  h := Min(Parent.Height, baseH);
  SetHeight(h);
end;


procedure TKMPopUpPanel.SetHandleCloseKey(aValue: Boolean);
begin
  fHandleCloseKey := aValue;
  Focusable := aValue;
end;


{ TKMForm }
//constructor TKMForm.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
//begin
//  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
//
//  fHeaderHeight := 24;
//
//  fButtonClose := TKMButtonFlat.Create(Self, aWidth - fHeaderHeight + 2, 2, fHeaderHeight-4, fHeaderHeight-4, 340, rxGui);
//  fButtonClose.OnClick := FormCloseClick;
//  fLabelCaption := TKMLabel.Create(Self, 0, 5, aWidth, fHeaderHeight, 'Form1', fntOutline, taCenter);
//  fLabelCaption.Hitable := False;
//end;
//
//
//procedure TKMForm.FormCloseClick(Sender: TObject);
//begin
//  Hide;
//
//  if Assigned(OnClose) then
//    OnClose(Self);
//end;
//
//
//function TKMForm.GetCaption: UnicodeString;
//begin
//  Result := fLabelCaption.Caption;
//end;
//
//
//procedure TKMForm.SetCaption(const aValue: UnicodeString);
//begin
//  fLabelCaption.Caption := aValue;
//end;
//
//
//function TKMForm.HitHeader(X, Y: Integer): Boolean;
//begin
//  Result := InRange(X - AbsLeft, 0, Width) and InRange(Y - AbsTop, 0, fHeaderHeight);
//end;
//
//
//procedure TKMForm.MouseDown(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
//begin
//  inherited;
//
//  if HitHeader(X,Y) then
//  begin
//    fDragging := True;
//    fOffsetX := X - AbsLeft;
//    fOffsetY := Y - AbsTop;
//  end;
//
//  MouseMove(X, Y, Shift);
//end;
//
//
//procedure TKMForm.MouseMove(X,Y: Integer; Shift: TShiftState);
//begin
//  inherited;
//
//  if fDragging and (csDown in State) then
//  begin
//    AbsLeft := EnsureRange(X - fOffsetX, 0, MasterParent.Width - Width);
//    AbsTop := EnsureRange(Y - fOffsetY, 0, MasterParent.Height - Height);
//
//    if Assigned(OnMove) then
//      OnMove(Self);
//  end;
//end;
//
//
//procedure TKMForm.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
//begin
//  inherited;
//  MouseMove(X,Y,Shift);
//
//end;
//
//
//procedure TKMForm.PaintPanel(aPaintLayer: TKMPaintLayer);
//begin
//  TKMRenderUI.WriteShadow(AbsLeft, AbsTop, Width, Height, 15, $40000000);
//
//  TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, 3, $FF000000);
//  TKMRenderUI.WriteOutline(AbsLeft+1, AbsTop+1, Width-2, Height-2, 1, $FF80FFFF);
//
//  inherited;
//end;


end.

