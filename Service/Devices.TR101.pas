unit Devices.TR101;

interface

uses Windows, GMConst, Devices.Modbus.ReqCreatorBase, GMGlobals;

type
  TTR101ReqCreator = class(TModbusRTUDevReqCreator)
  protected
    function PrepareCommand(var prmIds: TChannelIds; var Val: double): bool; override;
  public
    procedure AddRequests(); override;
  end;

const
      COUNT_TR101_STATE = 6;
      COUNT_TR101_SP1 = 1;
      COUNT_TR101_SP2 = 1;
      COUNT_TR101_SP3 = 1;
      COUNT_TR101_SP4 = 1;

      ADDR_TR101_STATE = 2;
      ADDR_TR101_SP1 = 32;
      ADDR_TR101_SP2 = 46;
      ADDR_TR101_SP3 = 60;
      ADDR_TR101_SP4 = 74;

implementation

{ TTR101ReqCreator }

procedure TTR101ReqCreator.AddRequests;
begin
  // состояние, аварии, температуры
  AddModbusRequestToSendBuf(ADDR_TR101_STATE, COUNT_TR101_STATE, rqtTR101_STATE);
  // уставка Т1
  AddModbusRequestToSendBuf(ADDR_TR101_SP1, COUNT_TR101_SP1, rqtTR101_SP1);
  // уставка Т2
  AddModbusRequestToSendBuf(ADDR_TR101_SP2, COUNT_TR101_SP2, rqtTR101_SP2);
  // уставка Т3
  AddModbusRequestToSendBuf(ADDR_TR101_SP3, COUNT_TR101_SP3, rqtTR101_SP3);
  // уставка Т4
  AddModbusRequestToSendBuf(ADDR_TR101_SP4, COUNT_TR101_SP4, rqtTR101_SP4);
end;

function TTR101ReqCreator.PrepareCommand(var prmIds: TChannelIds; var Val: double): bool;
var
  Cmd: int;
  wCmd: WORD;
begin
  Result := inherited;
  if not Result then Exit;

  if Abs(Val) > $7FFFF then
    Exit(false);

  Cmd := Round(Val);

  if Cmd < 0 then
    wCmd := $FFFF + Cmd + 1 // перевод в отрицательные величины, понятные ТР-101
  else
    wCmd := Cmd;

  Val := wCmd;

  if prmIds.ID_PT = ID_PT_PRESET then
    case prmIds.N_Src of
      1:
        prmIds.N_Src := ADDR_TR101_SP1;
      2:
        prmIds.N_Src := ADDR_TR101_SP2;
      3:
        prmIds.N_Src := ADDR_TR101_SP3;
      4:
        prmIds.N_Src := ADDR_TR101_SP4;
      else
        Exit(false);
    end;

  prmIds.ID_Src := SRC_AO;
  Result := true;
end;

end.
