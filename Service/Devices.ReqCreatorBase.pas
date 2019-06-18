////////////////////////////////////////////
// Базовый класс для построителя запросов к устройствам
////////////////////////////////////////////

unit Devices.ReqCreatorBase;

interface

uses Windows, GMGlobals, GMConst, StdRequest;

type
  TGMAddRequestToSendBufProc = procedure (ReqDetails: TRequestDetails) of object;

  TDevReqCreator = class
  private
    FAddRequestToSendBuf: TGMAddRequestToSendBufProc;
    FAllowEmpty: bool;
  protected
    FReqDetails: TRequestDetails;

    property AllowEmpty: bool read FAllowEmpty write FAllowEmpty;
    procedure AddStringRequestToSendBuf(sRequest: string; rqtp: T485RequestType);
    procedure AddBufRequestToSendBuf(buf: array of byte; Len: int; rqtp: T485RequestType);
    procedure AddUniversalRequestToSendBuf(req: IXMLGMIORequestType; rqtp: T485RequestType; const login: string);
    function PrepareCommand(var prmIds: TChannelIds; var Val: double): bool; virtual;
    function SetChannelValue(DevNumber, ID_Src, N_Src: int; Val: double; timeHold: int): bool; overload; virtual;
  public
    constructor Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails); virtual;
    procedure AddRequests(); virtual; abstract;
    function SetChannelValue(prmIds: TChannelIds; Val: double; timeHold: int): bool; overload;
  end;

  TDevReqCreatorClass = class of TDevReqCreator;

implementation

{ TDevReqCreator }

constructor TDevReqCreator.Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails);
begin
  inherited Create();

  FAllowEmpty := false;
  FAddRequestToSendBuf := AddRequestToSendBuf;
  FReqDetails := ReqDetails;
end;

function TDevReqCreator.PrepareCommand(var prmIds: TChannelIds; var Val: double): bool;
begin
  Result := true;
end;

function TDevReqCreator.SetChannelValue(prmIds: TChannelIds; Val: double; timeHold: int): bool;
begin
  Result := PrepareCommand(prmIds, Val) and SetChannelValue(prmIds.NDevNumber, prmIds.RealSrc(), prmIds.N_Src, Val, timeHold);
end;

function TDevReqCreator.SetChannelValue(DevNumber, ID_Src, N_Src: int; Val: double; timeHold: int): bool;
begin
  Result := false;
end;

procedure TDevReqCreator.AddStringRequestToSendBuf(sRequest: string; rqtp: T485RequestType);
var buf: array [0..50] of byte;
begin
  WriteString(buf, 0, sRequest);
  AddBufRequestToSendBuf(buf, Length(sRequest), rqtp);
end;

procedure TDevReqCreator.AddUniversalRequestToSendBuf(req: IXMLGMIORequestType; rqtp: T485RequestType; const login: string);
var buffers: TTwoBuffers;
begin
  buffers.RequestUniversal(req.XML, login, true, 255, 0);
  FReqDetails.BufCnt := buffers.LengthSend;
  FReqDetails.rqtp := rqtp;
  WriteBuf(FReqDetails.buf, 0, buffers.BufSend, FReqDetails.BufCnt);
  if rqtp = rqtCommand then
    FReqDetails.Priority := rqprCommand;
  FAddRequestToSendBuf(FReqDetails);
  FReqDetails.LogRequest();
end;

procedure TDevReqCreator.AddBufRequestToSendBuf(buf: array of byte; Len: int; rqtp: T485RequestType);
begin
  if not FAllowEmpty and (Len <= 0) then Exit;

  FReqDetails.rqtp := rqtp;
  FReqDetails.BufCnt := Len;
  WriteBuf(FReqDetails.buf, 0, buf, FReqDetails.BufCnt);
  if rqtp = rqtCommand then
    FReqDetails.Priority := rqprCommand;
  FAddRequestToSendBuf(FReqDetails);
  FReqDetails.LogRequest();
end;

end.
