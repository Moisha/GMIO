////////////////////////////////////////////
// Формирование запросов к приборам
// Поток опроса приборов по RS485 через Геомер
////////////////////////////////////////////
unit GM485;

interface

uses Windows, Classes, SysUtils, GMGlobals, Math, StrUtils, ADODB, DB, ActiveX,
     Forms, GMConst, GMSqlQuery, Generics.Collections, Threads.Base,
     Devices.ReqCreatorBase;

type
  TPrepareRequestBufferInfo = record
    ReqID: int;
  end;

  TSpecDevReqListItem = class
  private
    procedure PrepareTeconBuffer(ExtInfo: TPrepareRequestBufferInfo);
    procedure PrepareModbusTCPBuffer(ExtInfo: TPrepareRequestBufferInfo);
  public
    ReqDetails: TRequestDetails;
    Processed: bool;
    constructor Create();
    procedure PrepareBuffer(ExtInfo: TPrepareRequestBufferInfo);
  end;

  T485RequestCreator = class
  private
    FAddRequestToSendBuf: TGMAddRequestToSendBufProc;
    function CreateReqCreator(ID_DevType: int): TDevReqCreator;
  protected
    function GetDevReqCreatorClass(ID_DevType: int): TDevReqCreatorClass; virtual;
  public
    ReqDetails: TRequestDetails;

    procedure AddDeviceToSendBuf();
    function SetChannelValue(prmIds: TChannelIds; Val: double; timeHold: int): bool;

    constructor Create(AAddRequestToSendBuf: TGMAddRequestToSendBufProc);
  end;

  T485RequestListCreator = class(T485RequestCreator)
  private
    reqList: TList<TRequestDetails>;
    procedure AddRequest(ReqDetails: TRequestDetails);
  public
    constructor Create();
  end;

implementation

uses Devices.UBZ, Devices.Owen, Devices.Vzlet, Devices.Vacon, Devices.Tecon.Common, Devices.Tecon,
     DateUtils, ProgramLogFile, ArchIntervalBuilder, Devices.Termotronic, Devices.Mercury,
     Devices.TR101, Devices.ICP, Devices.Isco4250, Devices.Simag, Devices.Altistart22, Devices.Geostream,
     Devices.AllenBradley.Micro8xx, Devices.MainSrv, Devices.Logica.SPT941, Devices.Logica.SPT943,
     Devices.Logica.SPT961, Devices.Modbus.ReqCreatorBase, Devices.ModbusBase, Devices.DRK, Devices.ADCPChannelMaster, Devices.Geomer;

constructor T485RequestCreator.Create(AAddRequestToSendBuf: TGMAddRequestToSendBufProc);
begin
  inherited Create();
  FAddRequestToSendBuf := AAddRequestToSendBuf;
  ReqDetails.Init();
end;

function T485RequestCreator.GetDevReqCreatorClass(ID_DevType: int): TDevReqCreatorClass;
begin
  Result := nil;
  if ID_DevType in Tecon19_Family then
    Result := TTecon19ReqCreator
  else
  if ID_DevType in GeomerFamily then
    Result := TGeomerDevReqCreator
  else
    case ID_DevType of
      DEVTYPE_I7041D: Result := TICP7041DReqCreator;
      DEVTYPE_I7017: Result := TICP7017ReqCreator;
      DEVTYPE_UBZ: Result := TUBZReqCreator;
      DEVTYPE_TR101: Result := TTR101ReqCreator;
      DEVTYPE_TRM138: Result := TOwenTRM138ReqCreator;
      DEVTYPE_VZLET_URSV: Result := TVzletURSVReqCreator;
      DEVTYPE_VACON_NXL: Result := TVaconNXLReqCreator;
      DEVTYPE_ISCO_4250: Result := TIsco4250ReqCreator;
      DEVTYPE_SPT_941: Result := TSPT941ReqCreator;
      DEVTYPE_SPT_943: Result := TSPT943ReqCreator;
      DEVTYPE_SPT_961: Result := TSPT961ReqCreator;
      DEVTYPE_SIMAG11: Result := TSimag11ReqCreator;
      DEVTYPE_PETERFLOWRS: Result := TPeterflowRSReqCreator;
      DEVTYPE_MERCURY_230: Result := TMercury230ReqCreator;
      DEVTYPE_ALTISTART_22: Result := TAltistart22ReqCreator;
      DEVTYPE_GEOSTREAM_71: Result := TGeostreamReqCreator;
      DEVTYPE_ALLEN_BRADLEY_MICRO_8XX: Result := TAllenBradleyMicro8xxReqCreator;
      DEVTYPE_MAIN_SRV: Result := TMainSrvReqCreator;
      DEVTYPE_MODBUS_RTU: Result := TModbusRTUDevReqCreator;
      DEVTYPE_DRK: Result := TDRK4OPReqCreator;
      DEVTYPE_ADCP_CHANNEL_MASTER: Result := TADCPChannelMasterReqCreator;
    end;
end;

function T485RequestCreator.SetChannelValue(prmIds: TChannelIds; Val: double; timeHold: int): bool;
var
  cr: TDevReqCreator;
begin
  Result := false;
  cr := CreateReqCreator(ReqDetails.ID_DevType);
  if cr <> nil then
  try
    Result := cr.SetChannelValue(prmIds, Val, timeHold);
  finally
    cr.Free();
  end;
end;

function T485RequestCreator.CreateReqCreator(ID_DevType: int): TDevReqCreator;
var devclass: TDevReqCreatorClass;
begin
  Result := nil;

  devclass := GetDevReqCreatorClass(ID_DevType);
  if devclass <> nil then
    Result := devclass.Create(FAddRequestToSendBuf, ReqDetails)
  else
    ProgramLog.AddError('T485RequestCreator.CreateReqCreator unknown DevType = ' + IntToStr(ID_DevType));
end;

procedure T485RequestCreator.AddDeviceToSendBuf();
var cr: TDevReqCreator;
begin
  cr := CreateReqCreator(ReqDetails.ID_DevType);
  if cr <> nil then
  try
    cr.AddRequests();
  finally
    cr.Free();
  end;
end;

{ TSpecDevReqListItem }

constructor TSpecDevReqListItem.Create;
begin
  inherited;
  ReqDetails.Init();
  Processed := false;
end;

procedure TSpecDevReqListItem.PrepareBuffer(ExtInfo: TPrepareRequestBufferInfo);
begin
  if ReqDetails.ID_DevType in Tecon19_Family then
    PrepareTeconBuffer(ExtInfo);

  if (ReqDetails.ID_DevType = DEVTYPE_ALLEN_BRADLEY_MICRO_8XX)
     or ((ReqDetails.ID_DevType in ModbusRTUBasedDevices) and (ReqDetails.Converter = PROTOCOL_CONVETER_MODBUS_TCP_RTU)) then
    PrepareModbusTCPBuffer(ExtInfo);
end;

procedure TSpecDevReqListItem.PrepareModbusTCPBuffer(ExtInfo: TPrepareRequestBufferInfo);
begin
  // ModbusTCP Transaction Identifier 2 Bytes
  ReqDetails.buf[0] := ExtInfo.ReqID div 256;
  ReqDetails.buf[1] := ExtInfo.ReqID mod 256;
  ReqDetails.ReqID := ExtInfo.reqID;
end;

procedure TSpecDevReqListItem.PrepareTeconBuffer(ExtInfo: TPrepareRequestBufferInfo);
var dt: TDateTime;
    reqID: byte;
begin
  reqID := ExtInfo.ReqID and $0F;

  if (ReqDetails.BufCnt = 9) and (ReqDetails.buf[0] = $10) and (ReqDetails.buf[3] = $11) then
  begin  // запрос параметра ТЭКОН ф-ей $11. Cтруктура известна, вставим номер пачки, пересчитаем CRC
    ReqDetails.buf[1] := $40 + reqID;
    ReqDetails.ReqID := reqID;
  end;

  if (ReqDetails.BufCnt = 15) and (ReqDetails.buf[0] = $68) and (ReqDetails.buf[6] = $19) then
  begin  // запрос параметра ТЭКОН ф-ей $11. Cтруктура известна, вставим номер пачки, пересчитаем CRC
    ReqDetails.buf[4] := $40 + reqID;
    ReqDetails.ReqID := reqID;
  end;

  if (ReqDetails.BufCnt >= 14) and (ReqDetails.buf[0] = $68) and (ReqDetails.buf[6] in [$05, $14]) then
  begin  // установка параметра ТЭКОН ф-ей $14. Cтруктура известна, вставим номер пачки, пересчитаем CRC
    ReqDetails.buf[4] := $40 + reqID;
    ReqDetails.ReqID := reqID;

    if ReqDetails.rqtp = rqtTECON_SET_TIME then
    begin // установка времени прибора
      dt := Now() + (OneSecond / 2.0); // полсекунды на передачу

      if ReqDetails.buf[6] = $05 then  // COM
      begin
        ReqDetails.buf[9] := $00;
        ReqDetails.buf[10] := DecToHexDigits(SecondOf(dt));
        ReqDetails.buf[11] := DecToHexDigits(MinuteOf(dt));
        ReqDetails.buf[12] := DecToHexDigits(HourOf(dt));
      end;

      if ReqDetails.buf[6] = $14 then  // K-104, K-105
      begin
        ReqDetails.buf[12] := $00;
        ReqDetails.buf[13] := DecToHexDigits(SecondOf(dt));
        ReqDetails.buf[14] := DecToHexDigits(MinuteOf(dt));
        ReqDetails.buf[15] := DecToHexDigits(HourOf(dt));
      end;
    end;
  end;

  Tecon_CRC(ReqDetails.buf, ReqDetails.BufCnt - 2);
end;

{ T485RequestListCreator }

procedure T485RequestListCreator.AddRequest(ReqDetails: TRequestDetails);
begin
  reqList.Add(ReqDetails);
end;

constructor T485RequestListCreator.Create;
begin
  inherited Create(AddRequest);
  reqList := TList<TRequestDetails>.Create();
end;

end.
