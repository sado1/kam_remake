unit RXXPackerConsole;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Windows,
  KM_ResTypes, KM_ResPalettes, KM_ResSprites;


type
  TKMRXXPackerConsole = class
  public
    class procedure Execute;
  end;


implementation
uses
  KM_Defaults, RXXPackerProc;

const
  RXX_TO_PACK: array [0..5] of TRXType = (
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxTiles);

{ TKMRXXPackerConsole }
class procedure TKMRXXPackerConsole.Execute;
var
  fParams: TStringList;
  I, K, cnt: Integer;
  paramString: string;
  rxType: TRXType;
  lRxxPacker: TKMRXXPacker;
  palettes: TKMResPalettes;
  tick: Cardinal;
begin
  if ParamCount = 0 then
  begin
    Writeln('No rx packages were set');
    Writeln('');
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
    Exit;
  end else
  if ParamCount >= 1 then
  try
    Writeln('KaM Remake RXX Packer');

    // Take in all params
    fParams := TStringList.Create;
    for I := 1 to ParamCount do
      fParams.Append(ParamStr(I));

    for I := 0 to fParams.Count - 1 do
      Writeln('Arguments: ' + fParams[I]);

    ExeDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');
    lRxxPacker := TKMRXXPacker.Create(ExeDir);
    palettes := TKMResPalettes.Create;
    palettes.LoadPalettes(ExeDir + 'data\gfx\');
    try
      I := 0;
      cnt := ParamCount;
      while I < cnt do // Skip 0, as this is the EXE-path
      begin
        Inc(I);
        paramString := ParamStr(I);
        if (LowerCase(paramString) = 'sld') or (LowerCase(paramString) = 'spritesloaddir') then
        begin
          if (I >= ParamCount) then
          begin
            Writeln('spritesLoadDir directory not specified');
            Exit;
          end;

          Inc(I);

          if not DirectoryExists(ParamStr(I)) then
          begin
            Writeln('spritesLoadDir directory does not exist: ' + ParamStr(I));
            Exit;
          end;
          lRxxPacker.SpritesSourcePath := ParamStr(I);
          Continue;
        end;

        if ((LowerCase(paramString) = 'ssd') or (LowerCase(paramString) = 'spritessavedir')) then
        begin
          if (I >= ParamCount) then
          begin
            Writeln('spritesSaveDir directory not specified');
            Exit;
          end;

          Inc(I);
          lRxxPacker.RXXSavePath := ParamStr(I);
          Continue;
        end;

        if ((LowerCase(paramString) = 'packtorxa') or (LowerCase(paramString) = 'rxa')) then
        begin
          lRxxPacker.PackToRXA := True;

          Continue;
        end;

        paramString := ParamStr(I);

        for K := Low(RXX_TO_PACK) to High(RXX_TO_PACK) do
        if (LowerCase(paramString) = 'all')
        or (LowerCase(paramString) = LowerCase(RX_INFO[rxType].FileName)) then
        begin
          tick := GetTickCount;
          lRxxPacker.Pack(RXX_TO_PACK[K], palettes, procedure (aMsg: string) begin Writeln(aMsg); end);
          Writeln(RX_INFO[RXX_TO_PACK[K]].FileName + '.rxx packed in ' + IntToStr(GetTickCount - tick) + ' ms');
        end;

        Writeln('Unknown argument ' + paramString);
      end;
    finally
      lRxxPacker.Free;
      palettes.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Exception occurred:');
      Writeln(E.Message);
    end;
  end;
end;

end.
