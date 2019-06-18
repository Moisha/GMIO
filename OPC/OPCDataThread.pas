unit OPCDataThread;

interface

uses Windows, SysUtils, Classes, GMGlobals, Threads.GMClient, Connection.Base;

type
  TConnectionObjectOPC = class(TConnectionObjectGMClient)
  protected
    function InternalCheckGetAllData: bool; override;
  end;

  TGMOPCDataThread = class(TGMCOMThread)
  private
    FMainFormHandle: THandle;
  protected
    function DecodeBufRec(order: TDataOrder): bool; override;
    procedure RequestCurrents; override;
    procedure ExtReadINI(); override;
    function CreateConnectionObject(): TConnectionObjectGMClient; override;
  public
    constructor Create(mainFormHandle: THandle);
  end;

implementation

uses GMConst, ConfigXML;

{ TGMOPCDataThread }

constructor TGMOPCDataThread.Create(mainFormHandle: THandle);
begin
  inherited Create();
  FMainFormHandle := mainFormHandle;
end;

function TGMOPCDataThread.CreateConnectionObject(): TConnectionObjectGMClient;
begin
  Result := TConnectionObjectOPC.Create();
end;

function TGMOPCDataThread.DecodeBufRec(order: TDataOrder): bool;
var len: int;
    s: string;
begin
  Result := false;
  if not ((FConnectionObject.buffers.NumberOfBytesRead >= 6) and (FConnectionObject.buffers.BufRec[0] = 20) and (FConnectionObject.buffers.BufRec[1] = 135) and FConnectionObject.CheckGetAllData()) then Exit;

  len := ReadUINT(FConnectionObject.buffers.BufRec, 2);
  s := string(DecompressBufferToString(FConnectionObject.buffers.BufRec, 6, len));
  Result := s <> '';
  if Result and (FMainFormHandle <> 0) then
  begin
    PostMessage(FMainFormHandle, WM_OPC_UPDATE_CHANNELS, WPARAM(TStringClass.Create(s)), 0);
  end;
end;

procedure TGMOPCDataThread.ExtReadINI;
var xml: IGMConfigConfigType;
begin
  xml := ReadConfigXML();
  TimeZone := xml.Common.Timezone;
end;

procedure TGMOPCDataThread.RequestCurrents;
begin
  FConnectionObject.buffers.BufSend[0] := 20;
  FConnectionObject.buffers.BufSend[1] := 135;
  FConnectionObject.buffers.LengthSend := 2;
  FConnectionObject.ExchangeBlockData(etSenRec);
end;

{ TConnectionObjectOPC }

function TConnectionObjectOPC.InternalCheckGetAllData(): bool;
var n: int;
begin
  Result := inherited InternalCheckGetAllData();
  if Result then Exit;

  if (buffers.NumberOfBytesRead >= 6) and (buffers.BufRec[0] = 20) and (buffers.BufRec[1] = 135) then
  begin
    n := ReadUINT(buffers.BufRec, 2);
    Result := buffers.NumberOfBytesRead >= n + 6;
  end;
end;

end.
