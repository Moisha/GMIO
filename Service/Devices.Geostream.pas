unit Devices.Geostream;

interface

uses GMConst, Devices.Modbus.ReqCreatorBase;

type
  TGeostreamReqCreator = class(TModbusRTUDevReqCreator)
  public
    procedure AddRequests(); override;
  end;

const
      ADDR_GEOSTREAM_ALL = 0;
      COUNT_GEOSTREAM_ALL = 10;

implementation

{ TGeostreamReqCreator }

procedure TGeostreamReqCreator.AddRequests;
begin
  // Всем колхозом
  AddModbusRequestToSendBuf(ADDR_GEOSTREAM_ALL, COUNT_GEOSTREAM_ALL, rqtGeostream_All, $04);
end;

end.
