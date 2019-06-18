unit Devices.Geomer;

interface

uses Windows, GMGlobals, GMConst, Devices.ReqCreatorBase;

type
  TGeomerDevReqCreator = class(TDevReqCreator)
  protected
    function PrepareCommand(var prmIds: TChannelIds; var Val: double): bool; override;
    function SetChannelValue(DevNumber, ID_Src, N_Src: int; Val: double; timeHold: int): bool; override;
  public
    constructor Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails); override;
    procedure AddRequests(); override;
  end;

implementation

{ TGeomerDevReqCreator }

procedure TGeomerDevReqCreator.AddRequests;
begin

end;

constructor TGeomerDevReqCreator.Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails);
begin
  inherited;
end;

function TGeomerDevReqCreator.PrepareCommand(var prmIds: TChannelIds; var Val: double): bool;
begin
  Result := prmIds.ID_Src in [SRC_AO, SRC_DO];
end;

function TGeomerDevReqCreator.SetChannelValue(DevNumber, ID_Src, N_Src: int; Val: double; timeHold: int): bool;
var
  buf: array[0..20] of byte;
  n: int;
begin
  n := WriteByte(buf, 0, N_Src);
  n := WriteFloat(buf, n, Val);
  n := WriteUINT(buf, n, timeHold);
  AddBufRequestToSendBuf(buf, n, rqtCommand);
  Result := true;
end;

end.
