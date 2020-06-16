unit KM_HandEntity;
interface
uses
  KM_Points, KM_CommonClasses;

type
  TKMHandEntityType = (etUnit, etGroup, etHouse);


  TKMHandEntity<T> = class abstract
  private
    fUID: Integer; //unique entity ID
    fPointerCount: Cardinal;
    fType: TKMHandEntityType;
  protected
    function GetInstance: T; virtual; abstract;
    procedure SetUID(aUID: Integer);
    function GetPosition: TKMPoint; virtual; abstract;
  public
    constructor Create(aType: TKMHandEntityType; aUID: Integer);
    constructor Load(LoadStream: TKMemoryStream); virtual;

    procedure Save(SaveStream: TKMemoryStream); virtual;

    function GetPointer: T; //Returns self and adds one to the pointer counter
    procedure ReleasePointer;  //Decreases the pointer counter
    property PointerCount: Cardinal read fPointerCount;

    property EntityType: TKMHandEntityType read fType;

    property UID: Integer read fUID;
    property Position: TKMPoint read GetPosition;

    function ObjToString(const aSeparator: String = '|'): String; virtual;
    function ObjToStringShort(const aSeparator: String = '|'): String; virtual;
  end;


implementation
uses
  SysUtils, KM_GameParams;


{ TKMHandEntity }
constructor TKMHandEntity<T>.Create(aType: TKMHandEntityType; aUID: Integer);
begin
  inherited Create;

  fType := aType;
  fUID := aUID;
  fPointerCount := 0;
end;


constructor TKMHandEntity<T>.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;

  LoadStream.CheckMarker('Entity');
  LoadStream.Read(fUID);
  LoadStream.Read(fPointerCount);
  LoadStream.Read(fType, SizeOf(fType));
end;


procedure TKMHandEntity<T>.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('Entity');
  SaveStream.Write(fUID);
  SaveStream.Write(fPointerCount);
  SaveStream.Write(fType, SizeOf(fType));
end;


// Returns self and adds on to the pointer counter
function TKMHandEntity<T>.GetPointer: T;
begin
  Assert(gGameParams.AllowPointerOperations, 'GetPointer is not allowed outside of game tick update procedure, it could cause game desync');

  Inc(fPointerCount);
  Result := GetInstance;
end;


{Decreases the pointer counter}
//Should be used only by gHands for clarity sake
procedure TKMHandEntity<T>.ReleasePointer;
var
  ErrorMsg: UnicodeString;
begin
  Assert(gGameParams.AllowPointerOperations, 'ReleasePointer is not allowed outside of game tick update procedure, it could cause game desync');

  if fPointerCount < 1 then
  begin
    ErrorMsg := 'Unit remove pointer for U: ';
    try
      ErrorMsg := ErrorMsg + ObjToStringShort(',');
    except
      on E: Exception do
        ErrorMsg := ErrorMsg + IntToStr(UID) + ' Pos = ' + Position.ToString;
    end;
    raise ELocError.Create(ErrorMsg, Position);
  end;

  Dec(fPointerCount);
end;


procedure TKMHandEntity<T>.SetUID(aUID: Integer);
begin
  fUID := aUID;
end;


function TKMHandEntity<T>.ObjToString(const aSeparator: String = '|'): String;
begin
  Result := ''; // stub implementation
end;


function TKMHandEntity<T>.ObjToStringShort(const aSeparator: String = '|'): String;
begin
  Result := ''; // stub implementation
end;


end.

