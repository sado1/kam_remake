unit RXXPackerForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Spin, StdCtrls, SysUtils, TypInfo,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF FPC} LResources, LCLIntf, {$ENDIF}
  RXXPackerProc, KM_Defaults, KM_Log, KM_Pics, KM_ResPalettes, KM_ResSprites;


type
  TRXXPackData = record
    Name: String;
    Id: Integer;
  end;

  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ListBox1: TListBox;
    lpProgress: TLabel;
    btnUpdateList: TButton;
    edSpritesLoadDir: TEdit;
    Label2: TLabel;
    chkPackToRXA: TCheckBox;
    chkAddRXXHeader: TCheckBox;
    chkPackToRXX: TCheckBox;
    Label3: TLabel;
    edSpritesSaveDir: TEdit;
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
    fPacksData: array of TRXXPackData;
    fPacksCnt: Integer;
    fSettingsPath: string;
    fUpdating: Boolean;

    procedure UpdateUI;
    function AddPackData(aName: String; aId: Integer): TRXXPackData;

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
function TRXXForm1.AddPackData(aName: String; aId: Integer): TRXXPackData;
begin
  Result.Name := aName;
  Result.Id := aId;
  Inc(fPacksCnt);
  SetLength(fPacksData, fPacksCnt);
  fPacksData[fPacksCnt - 1] := Result;
end;


procedure TRXXForm1.UpdateList;
var
  RT: TRXType;
  packData: TRXXPackData;
begin
  fRXXPacker.SpritesLoadDir := edSpritesLoadDir.Text;

  ListBox1.Items.Clear;
  fPacksCnt := 0;
  SetLength(fPacksData, fPacksCnt);
  for RT := Low(TRXType) to High(TRXType) do
    if (RT = rxTiles) //Tiles are always in the list
    or FileExists(fRXXPacker.SpritesLoadDir + 'SpriteResource\' + RXInfo[RT].FileName + '.rx') then
    begin
      packData := AddPackData(GetEnumName(TypeInfo(TRXType), Integer(RT)), Integer(RT));
      ListBox1.Items.Add(packData.Name);
    end;

  if ListBox1.Items.Count = 0 then
  begin
    ShowMessage('No .RX files were found in' + sLineBreak + ExeDir + 'SpriteResource\');
    btnPackRXX.Enabled := False;
  end
  else
  begin
    btnPackRXX.Enabled := True;
    ListBox1.ItemIndex := 0;
    ListBox1.SelectAll;
  end;
end;


procedure TRXXForm1.btnUpdateListClick(Sender: TObject);
begin
  btnUpdateList.Enabled := False;

  UpdateList;

  btnUpdateList.Enabled := True;
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

  fRXXPacker := TKMRXXPacker.Create(ExeDir);
  fPalettes := TKMResPalettes.Create;
  fPalettes.LoadPalettes(ExeDir + 'data\gfx\');

  fSettingsPath := ExtractFilePath(ParamStr(0)) + 'RXXPacker.ini';
  LoadSettings;

  fPacksCnt := 0;
  SetLength(fPacksData, 0);

  UpdateList;
end;


procedure TRXXForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fPalettes);
  FreeAndNil(fRXXPacker);
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
  RT: TRXType;
  I: Integer;
  tick: Cardinal;
begin
  btnPackRXX.Enabled := False;
  chkPackToRXX.Enabled := False;
  chkPackToRXA.Enabled := False;
  chkAddRXXHeader.Enabled := False;
  try
    tick := GetTickCount;

    fRXXPacker.SpritesLoadDir := edSpritesLoadDir.Text;
    fRXXPacker.SpritesSaveDir := edSpritesSaveDir.Text;
    fRxxPacker.PackToRXX      := chkPackToRXX.Checked;
    fRxxPacker.PackToRXA      := chkPackToRXA.Checked;
    fRxxPacker.AddRXXHeader   := chkAddRXXHeader.Checked;

    if not DirectoryExists(fRXXPacker.SpritesLoadDir + SPRITES_RES_DIR + '\') then
    begin
      MessageBox(Handle, PWideChar('Cannot find ' + fRXXPacker.SpritesLoadDir + SPRITES_RES_DIR + '\ folder.' +
        sLineBreak + 'Please make sure this folder exists.'), 'Error', MB_ICONEXCLAMATION + MB_OK);
      Exit;
    end;

    for I := 0 to ListBox1.Items.Count - 1 do
      if ListBox1.Selected[I] then
      begin
        RT := TRXType(fPacksData[I].Id);

        fRxxPacker.Pack(RT, fPalettes);

        ListBox1.Selected[I] := False;
        ListBox1.Refresh;
      end;

    lpProgress.Caption := 'Packed in: ' + IntToStr(GetTickCount - tick) + ' ms';
  finally
    btnPackRXX.Enabled := True;
    chkPackToRXX.Enabled := True;
    chkPackToRXA.Enabled := True;
    chkAddRXXHeader.Enabled := True;
  end;
end;


{$IFDEF FPC}
initialization
  {$i RXXPackerForm.lrs}
{$ENDIF}


end.