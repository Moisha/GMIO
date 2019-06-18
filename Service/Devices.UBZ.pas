////////////////////////////////////////////
// Кодировка и расчет CRC для приборов типа УБЗ-302 и ТР-101
////////////////////////////////////////////
unit Devices.UBZ;

interface

uses GMGlobals, Windows, Classes, StrUtils, Devices.ReqCreatorBase, GMConst, Devices.Modbus.ReqCreatorBase, Devices.ModbusBase;

type TTransformatorType = (ttUnknown, ttInternalOrSmall, ttExternalAndBig);

procedure UBZ_CRC(var buf: array of Byte; Len: int);
function UBZ_CheckCRC(buf: array of Byte; Len: int): bool;
function IntFromTR101Word(w: word): int;

type
  TUBZReqCreator = class(TModbusRTUDevReqCreator)
  protected
    function PrepareCommand(var prmIds: TChannelIds; var Val: double): bool; override;
  public
    procedure AddRequests(); override;
  end;

const
      COUNT_UBZ_ALARMS = 3;
      COUNT_UBZ_TRANSFORMATOR = 2;
      COUNT_UBZ_I = 4;
      COUNT_UBZ_U = 3;
      COUNT_UBZ_A = 1;
      COUNT_UBZ_P = 1;
      COUNT_ALARM_LOG1 = 12;
      COUNT_ALARM_LOG2 = 8;

implementation

uses
  System.Math;

procedure UBZ_CRC(var buf: array of Byte; Len: int);
begin
  Modbus_CRC(buf, Len);
end;

function UBZ_CheckCRC(buf: array of Byte; Len: int): bool;
begin
  Result := Modbus_CheckCRC(buf, Len);
end;

function IntFromTR101Word(w: word): int;
begin
  if w and $8000 > 0 then
    Result := -($FFFF - w + 1)
  else
    Result := w;
end;

{ TUBZReqCreator }

procedure TUBZReqCreator.AddRequests();
begin
  // тип трансформатора
  AddModbusRequestToSendBuf(150, COUNT_UBZ_TRANSFORMATOR, rqtUBZ_TRANSFORMATOR);
  // токи
  AddModbusRequestToSendBuf(100, COUNT_UBZ_I, rqtUBZ_I);
  // напряжения
  AddModbusRequestToSendBuf(114, COUNT_UBZ_U, rqtUBZ_U);
  // время наработки двигателя
  AddModbusRequestToSendBuf(208, COUNT_UBZ_A, rqtUBZ_A);
  // время наработки двигателя
  AddModbusRequestToSendBuf(135, COUNT_UBZ_P, rqtUBZ_P);
  // события
  AddModbusRequestToSendBuf(240, COUNT_UBZ_ALARMS, rqtUBZ_ALARMS);
  // архив событий
  AddModbusRequestToSendBuf(243, COUNT_ALARM_LOG1, rqtUBZ_ALARM_LOG1);
  AddModbusRequestToSendBuf(255, COUNT_ALARM_LOG2, rqtUBZ_ALARM_LOG2);
end;

function TUBZReqCreator.PrepareCommand(var prmIds: TChannelIds; var Val: double): bool;
begin
  Result := inherited;
  if not Result then Exit;

  prmIds.ID_Src := SRC_AO;
  prmIds.N_Src := $00ED;
  Val := IfThen(CompareValue(Val, 0) <> 0, 2, 0);
  Result := true;
end;

end.
