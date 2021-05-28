program MapUtil;
{$IFDEF MSWindows}
  {$APPTYPE CONSOLE}
{$ENDIF}
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

{$R *.res}

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
    fArgs := fArgs + ' ' + ParamStr(I) + sLineBreak;

    if (ParamStr(I) = '-h') or (ParamStr(I) = '-help') then
    begin
      fParamRecord.Help := True;
      Continue;
    end;

    if (ParamStr(I) = '-v') or (ParamStr(I) = '-verbose') then
    begin
      fParamRecord.Verbose := True;
      Continue;
    end;

    // Only allow one script file
    if fParamRecord.MapDatPath = '' then
      fParamRecord.MapDatPath := ParamStr(I);
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

  FreeAndNil(fConsoleMain);
end.

