unit KM_Music;
{$I KaM_Remake.inc}
interface

//We have two choices for music libraries:
//BASS: Free for non-commercial projects. Requires bass.dll. Website: http://www.un4seen.com/
//ZLibPlay: GNU GPL license. Requires libzplay.dll. Website: http://libzplay.sourceforge.net/

//Comparision:  - BASS's DLL is much smaller (102kb vs 2.13mb(!)) and BASS seems faster at loading tracks.
//              - ZLibPlay supports more formats, (FLAC, AC-3, AAC, PCM) but we don't care
//              - ZLibPlay is GPL but BASS is not, and BASS can only be used for free in non-commercial products

{.$DEFINE USEBASS}
{$IFDEF MSWindows}
  {$IFNDEF NO_LIBZPLAY}
    {$DEFINE USELIBZPLAY}
  {$ENDIF}
{$ENDIF}

uses
  Types
  {$IFDEF USEBASS}     , Bass {$ENDIF}
  {$IFDEF USELIBZPLAY} , libZPlay {$ENDIF}
  ;

type
  TKMFadeState = (fsNone,
                  fsFadeOut, // Unfade
                  fsFadeIn,  // Fade
                  fsFaded);

  TKMMusicLib = class
  private
    fMusicCount: Integer;
    fMusicIndex: Integer; //Points to the index in TrackOrder of the current track
    fMusicTracks: TStringDynArray;
    fTrackOrder: TIntegerDynArray; //Each index points to an index of MusicTracks
    //MIDICount,MIDIIndex:integer;
    //MIDITracks:array[1..256]of string;
    fIsMusicInitialized: Boolean;
    fMusicGain: Single;
    {$IFDEF USEBASS} fBassStream, fBassOtherStream: Cardinal; {$ENDIF}
    {$IFDEF USELIBZPLAY} ZPlayer, ZPlayerOther: ZPlay; {$ENDIF} //I dislike that it's not TZPlay... Guess they don't know Delphi conventions.
    fFadeState: TKMFadeState;
    fFadeStarted: Cardinal;
    fFadeTime: Integer;
    fToPlayAfterFade: UnicodeString;
    fFadedToPlayOther: Boolean;
    fOtherVolume: Single;
    function PlayMusicFile(const FileName: UnicodeString): Boolean;
    function PlayOtherFile(const FileName: UnicodeString): Boolean;
    procedure ScanMusicTracks(const aPath: UnicodeString);
    procedure ShuffleSongs; //should not be seen outside of this class
    procedure UnshuffleSongs;

    procedure SetVolume(aValue: Single);
    function GetVolume: Single;
  public
    constructor Create(aVolume: Single);
    destructor Destroy; override;

    property Volume: Single read GetVolume write SetVolume;

    procedure PlayMenuTrack;
    procedure PlayNextTrack;
    procedure PlayPreviousTrack;
    function IsMusicEnded: Boolean;
    function IsOtherEnded: Boolean;
    procedure PauseMusic;
    procedure StopMusic;
    procedure ToggleMusic(aEnableMusic: Boolean);
    procedure ToggleShuffle(aEnableShuffle: Boolean);
    procedure FadeMusic; overload;
    procedure FadeMusic(aFadeTime: Integer); overload;
    procedure UnfadeMusic(aHandleCrackling: Boolean); overload;
    procedure UnfadeMusic(aHandleCrackling: Boolean; aFadeTime: Integer); overload;
    procedure PauseMusicToPlayFile(const aFileName: UnicodeString; aVolume: Single);
    procedure StopPlayingOtherFile;
    function GetTrackTitle: UnicodeString;
    procedure UpdateStateIdle; //Used for fading
  end;


implementation
uses
  SysUtils, KromUtils, Math,
  KM_Defaults,
  KM_Log, KM_CommonUtils;


const
  FADE_TIME = 2000; //Time that a fade takes to occur in ms


{ TKMMusicLib }
constructor TKMMusicLib.Create(aVolume: Single);
var
  I: Integer;
begin
  inherited Create;
  fIsMusicInitialized := True;

  if not DirectoryExists(ExeDir + 'Music') then
    ForceDirectories(ExeDir + 'Music');

  ScanMusicTracks(ExeDir + 'Music' + PathDelim);


  {$IFDEF USELIBZPLAY}
  ZPlayer := ZPlay.Create; //Note: They should have used TZPlay not ZPlay for a class
  ZPlayerOther := ZPlay.Create;
  {$ENDIF}

  {$IFDEF USEBASS}
  // Setup output - default device, 44100hz, stereo, 16 bits
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    gLog.AddTime('Failed to initialize the music playback device');
    fIsMusicInitialized := False;
  end;
  {$ENDIF}

  SetVolume(aVolume);

  // Initialise TrackOrder
  for I := 0 to fMusicCount - 1 do
    fTrackOrder[I] := I;

  gLog.AddTime('Music init done, ' + IntToStr(fMusicCount) + ' tracks found');
end;


destructor TKMMusicLib.Destroy;
begin
  {$IFDEF USELIBZPLAY}
  ZPlayer.Free;
  ZPlayerOther.Free;
  {$ENDIF}

  {$IFDEF USEBASS}
  BASS_Stop; //Stop all Bass output
  //Free the streams we may have used (will just return false if the stream is invalid)
  BASS_StreamFree(fBassStream);
  BASS_StreamFree(fBassOtherStream);
  BASS_Free; //Frees this usage of BASS, allowing it to be recreated successfully
  {$ENDIF}

  inherited;
end;


function TKMMusicLib.PlayMusicFile(const FileName: UnicodeString): Boolean;
{$IFDEF USEBASS}
var
  errorCode: Integer;
{$ENDIF}
begin
  Result := False;
  if not fIsMusicInitialized then Exit;
  if fFadeState <> fsNone then exit; //Don't start a new track while fading or faded

  //Cancel previous sound
  {$IFDEF USELIBZPLAY} ZPlayer.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassStream); {$ENDIF}

  if not FileExists(FileName) then exit; //Make it silent

  {$IFDEF USELIBZPLAY}
  Result := ZPlayer.OpenFile(AnsiString(FileName), sfAutodetect); //Detect file type automatically
  if not Result then exit; //File failed to load
  Result := ZPlayer.StartPlayback;
  if not Result then exit; //Playback failed to start
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_StreamFree(fBassStream); //Free the existing stream (will just return false if the stream is invalid)
  fBassStream := BASS_StreamCreateFile(FALSE, PChar(FileName), 0, 0, BASS_STREAM_AUTOFREE {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});

  BASS_ChannelPlay(fBassStream, True); //Start playback from the beggining

  errorCode := BASS_ErrorGetCode;
  if errorCode <> BASS_OK then exit; //Error
  {$ENDIF}

  SetVolume(fMusicGain); //Need to reset music volume after starting playback
  Result := True;
end;


function TKMMusicLib.PlayOtherFile(const FileName: UnicodeString): Boolean;
{$IFDEF USEBASS}
var
  errorCode: Integer;
{$ENDIF}
begin
  Result := False;
  if not fIsMusicInitialized then exit;

  //Cancel previous sound
  {$IFDEF USELIBZPLAY} ZPlayerOther.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassOtherStream); {$ENDIF}

  if not FileExists(FileName) then exit; //Make it silent

  {$IFDEF USELIBZPLAY}
  Result := ZPlayerOther.OpenFile(AnsiString(FileName), sfAutodetect); //Detect file type automatically
  if not Result then exit; //File failed to load
  Result := ZPlayerOther.StartPlayback;
  if not Result then exit; //Playback failed to start
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_StreamFree(fBassOtherStream); //Free the existing stream (will just return false if the stream is invalid)
  fBassOtherStream := BASS_StreamCreateFile(FALSE, PChar(FileName), 0, 0, BASS_STREAM_AUTOFREE {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});

  BASS_ChannelPlay(fBassOtherStream, True); //Start playback from the beggining

  errorCode := BASS_ErrorGetCode;
  if errorCode <> BASS_OK then exit; //Error
  {$ENDIF}

  //Now set the volume to the desired level
  {$IFDEF USELIBZPLAY}
  ZPlayerOther.SetPlayerVolume(Round(fOtherVolume * 100), Round(fOtherVolume * 100)); //0=silent, 100=max
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelSetAttribute(fBassOtherStream, BASS_ATTRIB_VOL, fOtherVolume); //0=silent, 1=max
  {$ENDIF}

  Result := True;
end;


{Update music gain (global volume for all sounds/music)}
procedure TKMMusicLib.SetVolume(aValue: Single);
begin
  if not fIsMusicInitialized then Exit; //Keep silent
  fMusicGain := aValue;
  {$IFDEF USELIBZPLAY}
  ZPlayer.SetPlayerVolume(Round(aValue * 100), Round(aValue * 100)); //0=silent, 100=max
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelSetAttribute(fBassStream, BASS_ATTRIB_VOL, aValue); //0=silent, 1=max
  {$ENDIF}
end;


function TKMMusicLib.GetVolume: Single;
{$IFDEF USELIBZPLAY}
var
  LeftVolume, RightVolume: Integer;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF USELIBZPLAY}
  ZPlayer.GetPlayerVolume(LeftVolume, RightVolume); //0=silent, 100=max
  Result := LeftVolume / 100;
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelGetAttribute(fBassStream, BASS_ATTRIB_VOL, Result);
  {$ENDIF}
end;


procedure TKMMusicLib.ScanMusicTracks(const aPath: UnicodeString);
var
  searchRec: TSearchRec;
begin
  if not fIsMusicInitialized then Exit;
  fMusicCount := 0;
  if not DirectoryExists(aPath) then Exit;

  SetLength(fMusicTracks, 255);

  FindFirst(aPath + '*.*', faAnyFile - faDirectory, searchRec);
  try
    repeat
      if (GetFileExt(searchRec.Name) = 'MP3') //Allow all formats supported by both libraries
      or (GetFileExt(searchRec.Name) = 'MP2')
      or (GetFileExt(searchRec.Name) = 'MP1')
      or (GetFileExt(searchRec.Name) = 'WAV')
      or (GetFileExt(searchRec.Name) = 'OGG')
      {$IFDEF USEBASS} //Formats supported by BASS but not LibZPlay
      or (GetFileExt(SearchRec.Name) = 'AIFF')
      {$ENDIF}
      {$IFDEF USELIBZPLAY} //Formats supported by LibZPlay but not BASS
      or (GetFileExt(searchRec.Name) = 'FLAC')
      or (GetFileExt(searchRec.Name) = 'OGA')
      or (GetFileExt(searchRec.Name) = 'AC3')
      or (GetFileExt(searchRec.Name) = 'AAC')
      {$ENDIF}
      then
      begin
        Inc(fMusicCount);
        fMusicTracks[fMusicCount - 1] := aPath + searchRec.Name;
      end;
      {if GetFileExt(SearchRec.Name)='MID' then
      begin
        Inc(MIDICount);
        MIDITracks[MIDICount] := Path + SearchRec.Name;
      end;}
    until (FindNext(searchRec) <> 0);
  finally
    FindClose(searchRec);
  end;

  //Cut to length
  SetLength(fMusicTracks, fMusicCount);
  SetLength(fTrackOrder, fMusicCount);

  fMusicIndex := -1;
end;


procedure TKMMusicLib.PlayMenuTrack;
var
  prevVolume: Single;
begin
  if not fIsMusicInitialized then Exit;
  if fMusicCount = 0 then Exit; //no music files found
  if fMusicIndex = 0 then Exit; //It's already playing
  fMusicIndex := 0;
  // There was audio crackling after loading screen, here we fix it by setting a delay and fading the volume.
  prevVolume := fMusicGain;
  fMusicGain := 0;
  PlayMusicFile(fMusicTracks[0]);
  fMusicGain := prevVolume;
  UnfadeMusic(True);
end;


procedure TKMMusicLib.PlayNextTrack;
begin
  if not fIsMusicInitialized then exit;
  if fMusicCount = 0 then exit; //no music files found
  if fFadeState <> fsNone then exit;

  //Set next index, looped or random
  fMusicIndex := (fMusicIndex + 1) mod fMusicCount;
  PlayMusicFile(fMusicTracks[fTrackOrder[fMusicIndex]]);
end;


procedure TKMMusicLib.PlayPreviousTrack;
begin
  if not fIsMusicInitialized then exit;
  if fMusicCount = 0 then exit; //no music files found
  if fFadeState <> fsNone then exit;

  fMusicIndex := (fMusicIndex + fMusicCount - 1) mod fMusicCount;
  PlayMusicFile(fMusicTracks[fTrackOrder[fMusicIndex]]);
end;


//Check if Music is not playing, to know when new mp3 should be feeded
function TKMMusicLib.IsMusicEnded: Boolean;
{$IFDEF USELIBZPLAY}
var
  status: TStreamStatus;
{$ENDIF}
begin
  {$IFDEF USELIBZPLAY} ZPlayer.GetStatus(status); {$ENDIF}
  Result := fIsMusicInitialized
            {$IFDEF USELIBZPLAY}
            and (not status.fPlay and not status.fPause) //Not playing and not paused due to fade
            {$ENDIF}
            {$IFDEF USEBASS}
            and (BASS_ChannelIsActive(fBassStream) = BASS_ACTIVE_STOPPED)
            {$ENDIF}
            ;
end;


//Check if other is not playing, to know when to return to the music
function TKMMusicLib.IsOtherEnded: Boolean;
{$IFDEF USELIBZPLAY}
var
  status: TStreamStatus;
{$ENDIF}
begin
  {$IFDEF USELIBZPLAY} ZPlayerOther.GetStatus(status); {$ENDIF}
  Result := fIsMusicInitialized
            {$IFDEF USELIBZPLAY}
            and (not status.fPlay) //Not playing and not paused due to fade
            {$ENDIF}
            {$IFDEF USEBASS}
            and (BASS_ChannelIsActive(fBassOtherStream) = BASS_ACTIVE_STOPPED)
            {$ENDIF}
            ;
end;


procedure TKMMusicLib.StopMusic;
begin
  if not fIsMusicInitialized then exit;
  {$IFDEF USELIBZPLAY} ZPlayer.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassStream); {$ENDIF}
  fMusicIndex := -1;
end;


procedure TKMMusicLib.ToggleMusic(aEnableMusic: Boolean);
begin
  if aEnableMusic then
    PlayMenuTrack //Start with the default track
  else
    StopMusic;
end;


procedure TKMMusicLib.ToggleShuffle(aEnableShuffle: Boolean);
begin
  if aEnableShuffle then
    ShuffleSongs
  else
    UnshuffleSongs;
end;


procedure TKMMusicLib.ShuffleSongs;
var
  I, R, newIndex: Integer;
begin
  if fMusicIndex = -1 then Exit; // Music is disabled

  newIndex := fMusicIndex;

  //Shuffle everything except for first (menu) track
  for I := fMusicCount - 1 downto 1 do
  begin
    R := RandomRange(1, I);
    //Remember the track number of the current track
    if fTrackOrder[R] = fMusicIndex then
      newIndex := I;
    KromUtils.SwapInt(fTrackOrder[R], fTrackOrder[I]);
  end;
  fMusicIndex := newIndex;
end;


procedure TKMMusicLib.UnshuffleSongs;
var
  I: Integer;
begin
  if fMusicIndex = -1 then Exit; // Music is disabled
  fMusicIndex := fTrackOrder[fMusicIndex];

  //Reset every index of the TrackOrder array
  for I := 0 to fMusicCount - 1 do
    fTrackOrder[I] := I;
end;


procedure TKMMusicLib.FadeMusic;
begin
  FadeMusic(FADE_TIME);
end;


procedure TKMMusicLib.FadeMusic(aFadeTime: Integer);
{$IFDEF USELIBZPLAY}
var
  startTime, endTime: TStreamTime;
  left, right: Integer;
{$ENDIF}
begin
  if (not fIsMusicInitialized) then Exit;
  fFadeTime := aFadeTime;
  fFadeState := fsFadeOut; //Fade it out
  fFadeStarted := TimeGet;
  {$IFDEF USELIBZPLAY}
  ZPlayer.GetPosition(startTime);
  endTime.ms := startTime.ms + aFadeTime;
  ZPlayer.GetPlayerVolume(left, right); //Start fade from the current volume
  ZPlayer.SlideVolume(tfMillisecond, startTime, left, right, tfMillisecond, endTime, 0, 0);
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelSlideAttribute(fBassStream, BASS_ATTRIB_VOL, 0, aFadeTime);
  {$ENDIF}
end;


procedure TKMMusicLib.UnfadeMusic(aHandleCrackling: Boolean);
begin
  UnfadeMusic(aHandleCrackling, FADE_TIME);
end;


// aHandleCrackling flag is used to mitigate initial sound crackling
procedure TKMMusicLib.UnfadeMusic(aHandleCrackling: Boolean; aFadeTime: Integer);
{$IFDEF USELIBZPLAY}
var
  startTime, endTime: TStreamTime;
  left, right: Integer;
{$ENDIF}
begin
  if (not fIsMusicInitialized) then Exit;
  fFadeTime := aFadeTime;
  fFadeState := fsFadeIn; //Fade it in
  fFadeStarted := TimeGet;
  {$IFDEF USELIBZPLAY}
  //LibZPlay has a nice SlideVolume function we can use
  ZPlayer.ResumePlayback; //Music may have been paused due to fade out
  if aHandleCrackling then Sleep(25);
  ZPlayer.GetPosition(startTime);
  endTime.ms := startTime.ms + aFadeTime;
  ZPlayer.GetPlayerVolume(left, right); //Start fade from the current volume
  ZPlayer.SlideVolume(tfMillisecond, startTime, left, right, tfMillisecond, endTime, Round(fMusicGain * 100), Round(fMusicGain * 100));
  {$ENDIF}
  {$IFDEF USEBASS}
  BASS_ChannelPlay(fBassStream, False); //Music may have been paused due to fade out
  if aHandleCrackling then Sleep(25);
  BASS_ChannelSlideAttribute(fBassStream, BASS_ATTRIB_VOL, fMusicGain, aFadeTime);
  {$ENDIF}
end;


procedure TKMMusicLib.UpdateStateIdle;
begin
  if not fIsMusicInitialized then Exit;

  case fFadeState of
    fsFadeIn:   if GetTimeSince(fFadeStarted) > fFadeTime then
                  fFadeState := fsNone;
    fsFadeOut:  begin
                  if GetTimeSince(fFadeStarted) > fFadeTime then
                  begin
                    fFadeState := fsFaded;
                    {$IFDEF USELIBZPLAY} ZPlayer.PausePlayback; {$ENDIF}
                    {$IFDEF USEBASS} BASS_ChannelPause(fBassStream); {$ENDIF}
                  end
                  else
                  //Start playback of other file half way through the fade
                  if (GetTimeSince(fFadeStarted) > fFadeTime div 2)
                    and (fToPlayAfterFade <> '') then
                  begin
                    fFadedToPlayOther := True;
                    PlayOtherFile(fToPlayAfterFade);
                    fToPlayAfterFade := '';
                  end;
                end;
  end;

  if fFadedToPlayOther and (fFadeState = fsFaded) and IsOtherEnded then
  begin
    fFadedToPlayOther := False;
    UnfadeMusic(False);
  end;
end;


procedure TKMMusicLib.PauseMusic;
begin
  if not fIsMusicInitialized then Exit;

  {$IFDEF USELIBZPLAY} ZPlayerOther.PausePlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelPause(fBassStream); {$ENDIF}
end;


procedure TKMMusicLib.PauseMusicToPlayFile(const aFileName: UnicodeString; aVolume: single);
begin
  fOtherVolume := aVolume;
  if fFadeState in [fsNone, fsFadeIn] then
  begin
    FadeMusic;
    fToPlayAfterFade := aFilename
  end
  else
    if (fFadeState = fsFaded) or ((fFadeState = fsFadeOut) and fFadedToPlayOther) then
    begin
      fFadedToPlayOther := True;
      PlayOtherFile(aFilename) //Switch playback immediately
    end
    else
      fToPlayAfterFade := aFilename; //We're still in the process of fading out, the file hasn't started yet
end;


procedure TKMMusicLib.StopPlayingOtherFile;
begin
  if not fIsMusicInitialized then Exit;
  {$IFDEF USELIBZPLAY} ZPlayerOther.StopPlayback; {$ENDIF}
  {$IFDEF USEBASS} BASS_ChannelStop(fBassOtherStream); {$ENDIF}
  fToPlayAfterFade := '';
  if fFadeState = fsFadeOut then fFadedToPlayOther := True; //Make sure the music starts again if we are currently fading out
end;


function TKMMusicLib.GetTrackTitle: UnicodeString;
begin
  if not fIsMusicInitialized then Exit;
  if not InRange(fMusicIndex, Low(fMusicTracks), High(fMusicTracks)) then Exit;

  Result := TruncateExt(ExtractFileName(fMusicTracks[fTrackOrder[fMusicIndex]]));
end;


(*
//Doesn't work unless you change volume in Windows?
s:= ExeDir + 'Music\SpiritOrig.mid';
{PlayMidiFile(s);
{StartSound(Form1.Handle, s);}
MCISendString(PChar('play ' + s), nil, 0, 0);}
*)


(*
function PlayMidiFile(FileName: UnicodeString):word;
var
  wdeviceid: integer;
  mciOpen: tmci_open_parms;
  mciPlay: tmci_play_parms;
  mciStat: tmci_status_parms;
begin
  // Open the device by specifying the device and filename.
  // MCI will attempt to choose the MIDI mapper as the output port.
  mciopen.lpstrDeviceType := 'sequencer';
  mciopen.lpstrElementName := pchar (filename);
  Result := mciSendCommand ($0, mci_open , mci_open_type or mci_open_element, longint (@mciopen));
  if Result <> 0 then exit;
  // The device opened successfully; get the device ID.
  // Check if the output port is the MIDI mapper.
  wDeviceID := mciOpen.wDeviceID;
  mciStat.dwItem := MCI_SEQ_STATUS_PORT;
  Result := mciSendCommand (wDeviceID, MCI_STATUS, MCI_STATUS_ITEM, longint (@mciStat));
  if Result <> 0 then
  begin
    mciSendCommand (wDeviceID, MCI_CLOSE, 0, 0);
    exit;
  end;
  // Begin playback. The window procedure function for the parent
  // Window will be notified with an MM_MCINOTIFY message when
  // Playback is complete. At this time, the window procedure closes
  // The device.
  mciPlay.dwCallback := Form1.Handle;
  Result := mciSendCommand (wDeviceID, MCI_PLAY,
  MCI_NOTIFY, longint (@mciPlay));
  if Result <> 0 then
  begin
    mciSendCommand (wDeviceID, MCI_CLOSE, 0, 0);
    exit;
  end;
end;
*)


end.
