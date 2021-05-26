unit KM_System;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  KM_ResTypes,
  KM_Defaults;


type
  // System related features are encapsulated in this class
  TKMSystem = class
  private
    {$IFNDEF FPC}{$IFDEF MSWindows}
    fFormMainHandle: HWND;
    fFlashing: Boolean;
    {$ENDIF}{$ENDIF}

    function GetCursor: TKMCursor;
    procedure SetCursor(Value: TKMCursor);
  public
    constructor Create(aFormMainHandle: HWND);

    property Cursor: TKMCursor read GetCursor write SetCursor;

    procedure FlashingStart;
    procedure FlashingStop;
  end;


var
  gSystem: TKMSystem;


implementation
uses
  Math,
  Vcl.Forms;


{ TKMSystem }
constructor TKMSystem.Create(aFormMainHandle: HWND);
begin
  inherited Create;

  fFormMainHandle := aFormMainHandle;
end;


function TKMSystem.GetCursor: TKMCursor;
begin
  if InRange(Screen.Cursor - CURSOR_CNT_OFFSET, Ord(Low(TKMCursor)), Ord(High(TKMCursor))) then
    Result := TKMCursor(Screen.Cursor - CURSOR_CNT_OFFSET)
  else
    Result := kmcDefault;
end;


procedure TKMSystem.SetCursor(Value: TKMCursor);
begin
  if SKIP_LOADING_CURSOR then Exit;
  Screen.Cursor := Ord(Value) + CURSOR_CNT_OFFSET;
end;


procedure TKMSystem.FlashingStart;
{$IFNDEF FPC}{$IFDEF MSWindows}
var
  flashInfo: TFlashWInfo;
{$ENDIF}{$ENDIF}
begin
  {$IFNDEF FPC}{$IFDEF MSWindows}
  if (GetForegroundWindow <> fFormMainHandle) then
  begin
    flashInfo.cbSize := 20;
    flashInfo.hwnd := Application.Handle;
    flashInfo.dwflags := FLASHW_ALL;
    flashInfo.ucount := 5; // Flash 5 times
    flashInfo.dwtimeout := 0; // Use default cursor blink rate
    fFlashing := True;
    FlashWindowEx(flashInfo);
  end
  {$ENDIF}{$ENDIF}
end;


procedure TKMSystem.FlashingStop;
{$IFNDEF FPC}{$IFDEF MSWindows}
var
  flashInfo: TFlashWInfo;
{$ENDIF}{$ENDIF}
begin
  {$IFNDEF FPC}{$IFDEF MSWindows}
  if fFlashing then
  begin
    flashInfo.cbSize := 20;
    flashInfo.hwnd := Application.Handle;
    flashInfo.dwflags := FLASHW_STOP;
    flashInfo.ucount := 0;
    flashInfo.dwtimeout := 0;
    fFlashing := False;
    FlashWindowEx(flashInfo);
  end
  {$ENDIF}{$ENDIF}
end;


end.
