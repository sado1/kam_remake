unit RXXPackerForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Spin, StdCtrls, SysUtils,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF FPC} LResources, LCLIntf, {$ENDIF}
  RXXPackerProc, KM_Defaults, KM_Log, KM_Pics, KM_ResPalettes, KM_ResSprites;


type
  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ListBox1: TListBox;
    btnUpdateList: TButton;
    edSpritesLoadDir: TEdit;
    Label2: TLabel;
    chkPackToRXA: TCheckBox;
    chkPackToRXX: TCheckBox;
    Label3: TLabel;
    edSpritesSaveDir: TEdit;
    meLog: TMemo;
    rbRXXFormat0: TRadioButton;
    rbRXXFormat1: TRadioButton;
    procedure btnPackRXXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnUpdateListClick(Sender: TObject);
    procedure chkPackToRXXClick(Sender: TObject);
    procedure chkPackToRXAClick(Sender: TObject);
    procedure edSpritesLoadDirChange(Sender: TObject);
  private
    fPalettes: TKMResPalettes;
    fRxxPacker: TKMRXXPacker;

    fSettingsPath: string;
    fUpdating: Boolean;

    procedure UpdateUI;
    procedure UpdateList;

    procedure LoadSettings;
    procedure SaveSettings;
  end;


implementation
uses
  INIFiles,
  KM_ResHouses, KM_ResUnits, KM_ResTypes,
  KM_Points;

{$R *.dfm}


{ TRXXForm1 }
procedure TRXXForm1.UpdateList;
var
  RT: TRXType;
  rxSet: TRXTypeSet;
begin
  // fRxxPacker is our SPOT, so we ask it about what it dims doable
  rxSet := fRxxPacker.GetAvailableToPack(edSpritesLoadDir.Text);

  ListBox1.Items.Clear;
  for RT := Low(TRXType) to High(TRXType) do
    if RT in rxSet then
      ListBox1.Items.AddObject(RX_INFO[RT].FileName, TObject(RT));

  if ListBox1.Items.Count = 0 then
  begin
    ShowMessage('No .RX files were found in' + sLineBreak + edSpritesLoadDir.Text);
    btnPackRXX.Enabled := False;
  end else
  begin
    btnPackRXX.Enabled := True;
    ListBox1.ItemIndex := 0;
    ListBox1.SelectAll;
  end;
end;


procedure TRXXForm1.btnUpdateListClick(Sender: TObject);
begin
  UpdateList;
end;


procedure TRXXForm1.UpdateUI;
begin
  btnPackRXX.Enabled := chkPackToRXX.Checked or chkPackToRXA.Checked;
end;


procedure TRXXForm1.chkPackToRXAClick(Sender: TObject);
begin
  UpdateUI;
end;


procedure TRXXForm1.chkPackToRXXClick(Sender: TObject);
begin
  UpdateUI;
end;


procedure TRXXForm1.edSpritesLoadDirChange(Sender: TObject);
begin
  if fUpdating then Exit;

  SaveSettings;
end;


procedure TRXXForm1.FormCreate(Sender: TObject);
begin
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  Caption := 'RXX Packer (' + GAME_REVISION + ')';

  gLog := TKMLog.Create(ExeDir + 'RXXPacker.log');

  fUpdating := True;
  edSpritesLoadDir.Text := ExeDir;
  fUpdating := False;

  fRxxPacker := TKMRXXPacker.Create(ExeDir);
  fPalettes := TKMResPalettes.Create;
  fPalettes.LoadPalettes(ExeDir + 'data\gfx\');

  fSettingsPath := ExtractFilePath(ParamStr(0)) + 'RXXPacker.ini';
  LoadSettings;

  UpdateList;
end;


procedure TRXXForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fPalettes);
  FreeAndNil(fRxxPacker);
  FreeAndNil(gLog);
end;


procedure TRXXForm1.LoadSettings;
var
  ini: TINIFile;
begin
  fUpdating := True;

  ini := TINIFile.Create(fSettingsPath);
  try
    edSpritesLoadDir.Text := ini.ReadString('SETTINGS',  'SpritesLoadDir', ExeDir);
    edSpritesSaveDir.Text := ini.ReadString('SETTINGS',  'SpritesSaveDir', ExeDir);
  finally
    ini.Free;
  end;

  fUpdating := False;

  if not FileExists(fSettingsPath) then
    SaveSettings;
end;


procedure TRXXForm1.SaveSettings;
var
  ini: TINIFile;
begin
  ini := TINIFile.Create(fSettingsPath);
  try
    ini.WriteString('SETTINGS',  'SpritesLoadDir', edSpritesLoadDir.Text);
    ini.WriteString('SETTINGS',  'SpritesSaveDir', edSpritesSaveDir.Text);
  finally
    ini.Free;
  end;
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
var
  rxSet: TRXTypeSet;
  I: Integer;
begin
  btnPackRXX.Enabled := False;
  chkPackToRXX.Enabled := False;
  chkPackToRXA.Enabled := False;
  rbRXXFormat0.Enabled := False;
  rbRXXFormat1.Enabled := False;
  try
    fRxxPacker.SpritesSourcePath := edSpritesLoadDir.Text;
    fRxxPacker.RXXSavePath    := edSpritesSaveDir.Text;
    fRxxPacker.PackToRXX      := chkPackToRXX.Checked;
    fRxxPacker.PackToRXA      := chkPackToRXA.Checked;
    if rbRXXFormat0.Checked then fRxxPacker.RXXFormat := rxxZero;
    if rbRXXFormat1.Checked then fRxxPacker.RXXFormat := rxxOne;

    rxSet := [];
    for I := 0 to ListBox1.Items.Count - 1 do
      if ListBox1.Selected[I] then
        rxSet := rxSet + [TRXType(ListBox1.Items.Objects[I])];

    try
      fRxxPacker.Pack2(rxSet, fPalettes, procedure (aMsg: string) begin meLog.Lines.Append(aMsg); end);
    except
      on E: Exception do
        MessageBox(Handle, PWideChar(E.Message), 'Error', MB_ICONEXCLAMATION or MB_OK);
    end;

    ListBox1.ClearSelection;
  finally
    btnPackRXX.Enabled := True;
    chkPackToRXX.Enabled := True;
    chkPackToRXA.Enabled := True;
    rbRXXFormat0.Enabled := True;
    rbRXXFormat1.Enabled := True;
  end;
end;


{$IFDEF FPC}
initialization
  {$i RXXPackerForm.lrs}
{$ENDIF}


end.