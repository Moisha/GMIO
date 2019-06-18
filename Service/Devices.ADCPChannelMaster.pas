unit Devices.ADCPChannelMaster;

interface

uses
  GMGlobals, Devices.ReqCreatorBase, GMConst;

type
  TADCPChannelMasterReqCreator = class(TDevReqCreator)
  public
    procedure AddRequests(); override;
    constructor Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails); override;
  end;

implementation

{ TADCPChannelMasterReqCreator }

procedure TADCPChannelMasterReqCreator.AddRequests;
begin
  FReqDetails.TimeOut := 30000;
  AddStringRequestToSendBuf('', rqtADCP_Channel_Master);
end;

constructor TADCPChannelMasterReqCreator.Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails);
begin
  inherited;
  AllowEmpty := true;
end;

end.
