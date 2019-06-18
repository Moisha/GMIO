unit Devices.Altistart22;

interface

uses GMConst, Devices.Modbus.ReqCreatorBase;

type
  TAltistart22ReqCreator = class(TModbusRTUDevReqCreator)
  public
    procedure AddRequests(); override;
  end;

const
      // Линейное напряжение
      ADDR_ALTISTART22_ALL = 257;
      COUNT_ALTISTART22_ALL = 23;

      ADDR_ALTISTART22_LCR1 = ADDR_ALTISTART22_ALL;
      ADDR_ALTISTART22_LCR2 = ADDR_ALTISTART22_ALL + 1;
      ADDR_ALTISTART22_LCR3 = ADDR_ALTISTART22_ALL + 2;
      ADDR_ALTISTART22_LFT = 279;
      ADDR_ALTISTART22_U = 260;

implementation

{ TAltistart22ReqCreator }

procedure TAltistart22ReqCreator.AddRequests;
begin
  // Всем колхозом
  AddModbusRequestToSendBuf(ADDR_ALTISTART22_ALL, COUNT_ALTISTART22_ALL, rqtAltistart22_All);
end;

end.
