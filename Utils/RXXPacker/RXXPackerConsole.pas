unit RXXPackerConsole;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Windows,
  KM_ResTypes, KM_ResPalettes, KM_ResSprites;


type
  TKMRXXPackerConsole = class
  private class var
    fParams: TStringList;
    fSpritesSourcePath: string;
    fRXXSavePath: string;
    fPackToRXA: Boolean;
    fRxs: TRXTypeSet;
  private
    class procedure OutputInstructions;
    class procedure ParseArguments;
    class procedure Pack;
  public
    class procedure Execute;
  end;


implementation
uses
  KM_Defaults, RXXPackerProc;

const
  // Everything except rxCustom
  RXX_TO_PACK: TRXTypeSet = [rxTrees, rxHouses, rxUnits, rxGui, rxGuiMain, rxTiles];


{ TKMRXXPackerConsole }
class procedure TKMRXXPackerConsole.OutputInstructions;
begin
  Writeln('Arguments:');
  Writeln(' - sld spritesloaddir %s - source folder');
  Writeln(' - ssd spritessavedir %s - target folder');
  Writeln(' - rxa packtorxa - pack RXA as well');
  Writeln(' - all - pack all RX libraries');
  Writeln(' - %s - pack specific RX library');
  Writeln('');
  Writeln('Usage examples:');
  Writeln(' - RxxPacker.exe gui guimain houses trees units tileset');
  Writeln(' - RxxPacker.exe all');
  Writeln(' - RxxPacker.exe spritesBaseDir "C:\kmr_sprites\" units');
end;


class procedure TKMRXXPackerConsole.ParseArguments;
var
  I: Integer;
  rxType: TRXType;
begin
  Writeln('Supplied arguments:');

  // Take in all params for simpler parsing down below
  for I := 1 to ParamCount do
  begin
    Writeln(' - ' + ParamStr(I));
    fParams.Append(ParamStr(I));
  end;

  for I := 0 to fParams.Count - 1 do
  begin
    if (LowerCase(fParams[I]) = 'sld') or (LowerCase(fParams[I]) = 'spritesloaddir') then
      if I < fParams.Count - 1 then
      begin
        fSpritesSourcePath := fParams[I+1];
        fParams[I+1] := ''; // Make sure we dont parse it as some other key
      end else
        raise Exception.Create('spritesLoadDir directory not specified');

    if (LowerCase(fParams[I]) = 'ssd') or (LowerCase(fParams[I]) = 'spritessavedir') then
      if I < fParams.Count - 1 then
      begin
        fRXXSavePath := fParams[I+1];
        fParams[I+1] := ''; // Make sure we dont parse it as some other key
      end else
        raise Exception.Create('spritesSaveDir directory not specified');

    if (LowerCase(fParams[I]) = 'packtorxa') or (LowerCase(fParams[I]) = 'rxa') then
      fPackToRXA := True;

    for rxType := Low(TRXType) to High(TRXType) do
    if LowerCase(fParams[I]) = 'all' then
      fRxs := RXX_TO_PACK
    else
    if LowerCase(fParams[I]) = LowerCase(RX_INFO[rxType].FileName) then
      fRxs := fRxs + [rxType];
  end;
end;


class procedure TKMRXXPackerConsole.Pack;
var
  rxType: TRXType;
  rxxPacker: TKMRXXPacker;
  resPalettes: TKMResPalettes;
  tick: Cardinal;
begin
  ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');

  rxxPacker := TKMRXXPacker.Create(ExeDir);
  rxxPacker.SpritesSourcePath := fSpritesSourcePath;
  rxxPacker.RXXSavePath := fRXXSavePath;
  rxxPacker.PackToRXA := fPackToRXA;

  resPalettes := TKMResPalettes.Create;
  resPalettes.LoadPalettes(ExeDir + 'data\gfx\');
  try
    for rxType := Low(TRXType) to High(TRXType) do
    if rxType in fRxs then
    begin
      tick := GetTickCount;
      rxxPacker.Pack(rxType, resPalettes, procedure (aMsg: string) begin Writeln(aMsg); end);
      Writeln(RX_INFO[rxType].FileName + '.rxx packed in ' + IntToStr(GetTickCount - tick) + ' ms');
    end;
  finally
    rxxPacker.Free;
    resPalettes.Free;
  end;
end;


class procedure TKMRXXPackerConsole.Execute;
begin
  Writeln('KaM Remake RXX Packer');
  Writeln('');

  fParams := TStringList.Create;
  try
    ParseArguments;

    if fParams.Count = 0 then
    begin
      OutputInstructions;
      Exit;
    end;

    Pack;
  except
    on E: Exception do
    begin
      Writeln('Exception occurred:');
      Writeln(E.Message);
    end;
  end;
end;


end.
