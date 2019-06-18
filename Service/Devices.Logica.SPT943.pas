////////////////////////////////////////////
// Кодировка и расчет CRC для приборов Логика СПТ-943
////////////////////////////////////////////
unit Devices.Logica.SPT943;

interface

uses Windows, GMGlobals, GMConst, Devices.Logica.SPT941, Devices.Logica.Base;

function LogicaSPT943_CheckCRC(buf: array of Byte; Len: int): bool;

type
  TSPT943ReqCreator = class(TSPT941ReqCreator)
  protected
    function CurrentsRequestType(Chn: int): T485RequestType; override;
    function ChannelCount: int; override;
  end;

  TSPT943AnswerParser = class(TSPT941AnswerParser)

  end;

implementation

function LogicaSPT943_CheckCRC(buf: array of Byte; Len: int): bool;
begin
  Result := LogicaM4_CheckCRC(buf, len);
end;

{ TSPT943ReqCreator }

function TSPT943ReqCreator.ChannelCount: int;
begin
  Result := 3;
end;

function TSPT943ReqCreator.CurrentsRequestType(Chn: int): T485RequestType;
begin
  case Chn of
    0: Result := rqtSPT943_0;
    1: Result := rqtSPT943_1;
    2: Result := rqtSPT943_2;
    else Result := rqtNone;
  end;
end;

end.
