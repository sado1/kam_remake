program MapUtil;

{$IFDEF MSWindows}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}

uses
  {$IFDEF UNIX}
    {$DEFINE UseCThreads}
    cthreads, //We use a thread for deleting old log files
    BaseUnix,
  {$ENDIF}
  SysUtils,
  KM_Defaults,
  MapUtilTypes in 'MapUtilTypes.pas',
  ConsoleMain in 'ConsoleMain.pas';

var
  fConsoleMain: TConsoleMain;
  fParamRecord: TCLIParamRecord;
  fArgs:        string;

procedure ProcessParams;
var
  I: Integer;
begin
  if ParamCount = 0 then
  begin
    fParamRecord.Help := True;
    Exit;
  end;

  for I := 1 to ParamCount do // Skip 0, as this is the EXE-path
  begin
    fArgs := fArgs + ' ' + paramstr(I) + sLineBreak;

    if (paramstr(I) = '-h') or (paramstr(I) = '-help') then
    begin
      fParamRecord.Help := True;
      continue;
    end;

    if (paramstr(I) = '-v') or (paramstr(I) = '-verbose') then
    begin
      fParamRecord.Verbose := True;
      continue;
    end;

    // Only allow one script file
    if fParamRecord.MapDatPath = '' then
      fParamRecord.MapDatPath := paramstr(I);
  end;
end;

begin
  try
    ProcessParams;
    ExeDir := ExtractFilePath(ParamStr(0));
    fConsoleMain := TConsoleMain.Create;
    fConsoleMain.Start(fParamRecord);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  if fConsoleMain <> nil then
    FreeAndNil(fConsoleMain);
end.

