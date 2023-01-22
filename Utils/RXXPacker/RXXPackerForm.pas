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
    chkAddRXXHeader: TCheckBox;
    chkPackToRXX: TCheckBox;
    Label3: TLabel;
    edSpritesSaveDir: TEdit;
    meLog: TMemo;
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
begin
  fRXXPacker.SpritesSourcePath := edSpritesLoadDir.Text;

  ListBox1.Items.Clear;
  for RT := Low(TRXType) to High(TRXType) do
    if (RT = rxTiles) //Tiles are always in the list
    or FileExists(fRXXPacker.SpritesSourcePath + 'SpriteResource\' + RX_INFO[RT].FileName + '.rx') then
      ListBox1.Items.AddObject(RX_INFO[RT].FileName, TObject(RT));

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
  tickTotal, tick: Cardinal;
begin
  btnPackRXX.Enabled := False;
  chkPackToRXX.Enabled := False;
  chkPackToRXA.Enabled := False;
  chkAddRXXHeader.Enabled := False;
  try
    tickTotal := GetTickCount;

    fRXXPacker.SpritesSourcePath := edSpritesLoadDir.Text;
    fRXXPacker.RXXSavePath    := edSpritesSaveDir.Text;
    fRxxPacker.PackToRXX      := chkPackToRXX.Checked;
    fRxxPacker.PackToRXA      := chkPackToRXA.Checked;
    fRxxPacker.AddVersionHeader := chkAddRXXHeader.Checked;

    if not DirectoryExists(fRXXPacker.SpritesSourcePath + SPRITES_RES_DIR + '\') then
    begin
      MessageBox(Handle, PWideChar('Cannot find ' + fRXXPacker.SpritesSourcePath + SPRITES_RES_DIR + '\ folder.' +
        sLineBreak + 'Please make sure this folder exists.'), 'Error', MB_ICONEXCLAMATION or MB_OK);
      Exit;
    end;

    try
      for I := 0 to ListBox1.Items.Count - 1 do
        if ListBox1.Selected[I] then
        begin
          tick := GetTickCount;
          meLog.Lines.Append('Packing ' + ListBox1.Items[I] + ' ...');

          RT := TRXType(ListBox1.Items.Objects[I]);

          fRxxPacker.Pack(RT, fPalettes, procedure (aMsg: string) begin meLog.Lines.Append(aMsg); end);

          ListBox1.Selected[I] := False;
          ListBox1.Refresh;

          meLog.Lines.Append(ListBox1.Items[I] + ' packed in ' + IntToStr(GetTickCount - tick) + ' ms');
        end;

      meLog.Lines.Append('Everything packed in ' + IntToStr(GetTickCount - tickTotal) + ' ms');
    except
      on E: Exception do
        MessageBox(Handle, PWideChar(E.Message), 'Error', MB_ICONEXCLAMATION or MB_OK);
    end;
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