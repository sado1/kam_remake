unit KM_NetUDPOverbyte;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, OverbyteIcsWSocket, OverbyteIcsWinSock;


type
  TNotifyAddressDataEvent = procedure(const aAddress: string; aData: Pointer; aLength: Cardinal) of object;

  TKMNetUDPOverbyte = class
  private
    fSocketSend: TWSocket;
    fSocketReceive: TWSocket;

    fOnError: TGetStrProc;
    fOnRecieveData: TNotifyAddressDataEvent;
    procedure DataAvailable(Sender: TObject; aError: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendPacket(const aAddress:string; const aPort: Word; aData: Pointer; aLength: Cardinal);
    procedure Listen(const aPort: Word);
    procedure StopListening;
    property OnError: TGetStrProc write fOnError;
    property OnRecieveData: TNotifyAddressDataEvent write fOnRecieveData;
  end;


implementation


{ TKMNetUDPOverbyte }
constructor TKMNetUDPOverbyte.Create;
begin
  inherited Create;

  fSocketSend := TWSocket.Create(nil);
  fSocketReceive := TWSocket.Create(nil);
end;


destructor TKMNetUDPOverbyte.Destroy;
begin
  fSocketSend.Free;
  fSocketReceive.Free;

  inherited;
end;


procedure TKMNetUDPOverbyte.Listen(const aPort: Word);
begin
  fSocketReceive.OnDataAvailable := DataAvailable;
  try
    fSocketReceive.Proto     := 'udp';
    fSocketReceive.Addr      := '0.0.0.0';
    fSocketReceive.Port      := IntToStr(aPort);
    fSocketReceive.Listen;
  except
    on E: Exception do
    begin
      // Trap the exception and tell the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
      fOnError(E.Message);
    end;
  end;
end;


procedure TKMNetUDPOverbyte.StopListening;
begin
  fSocketReceive.Close;
  fSocketReceive.OnDataAvailable := nil;
end;


procedure TKMNetUDPOverbyte.SendPacket(const aAddress:string; const aPort: Word; aData: Pointer; aLength: Cardinal);
begin
  fSocketSend.Proto     := 'udp';
  fSocketSend.Addr      := aAddress;
  fSocketSend.Port      := IntToStr(aPort);
  fSocketSend.LocalPort := '0';
  fSocketSend.Connect;
  fSocketSend.Send(aData, aLength);
  fSocketSend.Close;
end;


procedure TKMNetUDPOverbyte.DataAvailable(Sender: TObject; aError: Word);
const
  BUFFER_SIZE = 10240; //10kb
var
  P: Pointer;
  L: Integer; //L could be -1 when no data is available
  sockAddr: TSockAddr;
  srcLen : Integer;
begin
  srcLen := SizeOf(sockAddr);
  if aError <> 0 then
  begin
    fOnError('UDP DataAvailable. Error ' + WSocketErrorDesc(aError) + ' (#' + IntToStr(aError) + ')');
    exit;
  end;

  GetMem(P, BUFFER_SIZE+1); //+1 to avoid RangeCheckError when L = BUFFER_SIZE
  L := TWSocket(Sender).ReceiveFrom(P, BUFFER_SIZE, sockAddr, srcLen);

  if L > 0 then
    fOnRecieveData(UnicodeString(WSocket_inet_ntoa(sockAddr.sin_addr)), P, L);

  FreeMem(P);
end;


end.
