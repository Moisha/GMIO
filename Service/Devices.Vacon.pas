////////////////////////////////////////////
// Кодировка и расчет CRC для приборов типа Vacon
////////////////////////////////////////////
unit Devices.Vacon;

interface

uses Windows, GMGlobals, Classes, SysUtils, Devices.ReqCreatorBase, GMConst, Devices.Modbus.ReqCreatorBase;

type
  TVaconNXLReqCreator = class(TModbusRTUDevReqCreator)
  private
    procedure AddVaconNXLToSendBuf_OneQuery(VaconRegister: WORD; rqtp: T485RequestType);
  protected
    function PrepareCommand(var prmIds: TChannelIds; var Val: double): bool; override;
  public
    procedure AddRequests(); override;
  end;

procedure Vacon_CRC(var buf: array of Byte; Len: int);
function Vacon_CheckCRC(buf: array of Byte; Len: int): bool;

implementation

function Vacon_CalcCRC(puchMsg: array of byte; usDataLen: integer): WORD;

const auchCRCHi: array [0..255] of byte = (
 $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00,
 $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1,
 $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81,
 $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40,
 $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01,
 $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0,
 $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80,
 $22, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41,
 $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00,
 $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0,
 $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80,
 $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41,
 $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01,
 $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1,
 $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81,
 $40, $01, $C0, $80, $41, $00, $C0, $80, $41, $00, $C1, $81, $40,
 $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01,
 $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1,
 $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80,
 $41, $01, $C0, $80, $41, $00, $C1, $81, $40 );

const auchCRCLo: array [0..255] of byte = (
 $00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07, $C7, $05,
 $C5, $C4, $04, $CC, $0C, $0D, $CD, $0F, $CF, $CE, $0E, $0A, $CA,
 $CB, $0B, $C9, $09, $08, $C8, $D8, $18, $19, $D9, $1B, $DB, $DA,
 $1A, $1E, $DE, $DF, $1F, $DD, $1D, $1C, $DC, $14, $D4, $D5, $15,
 $D7, $17, $16, $D6, $D2, $12, $13, $D3, $11, $D1, $D0, $10, $F0,
 $30, $31, $F1, $33, $F3, $F2, $32, $36, $F6, $F7, $37, $F5, $35,
 $34, $F4, $3C, $FC, $FD, $3D, $FF, $3F, $3E, $FE, $FA, $3A, $3B,
 $FB, $39, $F9, $F8, $38, $28, $E8, $E9, $29, $EB, $2B, $2A, $EA,
 $EE, $2E, $2F, $EF, $2D, $ED, $EC, $2C, $E4, $24, $25, $E5, $27,
 $E7, $E6, $26, $22, $E2, $E3, $23, $E1, $21, $20, $E0, $A0, $60,
 $61, $A1, $63, $A3, $A2, $62, $66, $A6, $A7, $67, $A5, $65, $64,
 $A4, $6C, $AC, $AD, $6D, $AF, $6F, $6E, $AE, $AA, $6A, $6B, $AB,
 $69, $A9, $A8, $68, $78, $B8, $B9, $79, $BB, $7B, $7A, $BA, $BE,
 $7E, $7F, $BF, $7D, $BD, $BC, $7C, $B4, $74, $75, $B5, $77, $B7,
 $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70, $B0, $50, $90, $91,
 $51, $93, $53, $52, $92, $96, $56, $57, $97, $55, $95, $94, $54,
 $9C, $5C, $5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B, $99,
 $59, $58, $98, $88, $48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E,
 $8F, $4F, $8D, $4D, $4C, $8C, $44, $84, $85, $45, $87, $47, $46,
 $86, $82, $42, $43, $83, $41, $81, $80, $40
	);

var uchCRCHi, uchCRCLo, uIndex: byte;
    i: integer;
begin
	uchCRCHi :=  $FF;
	uchCRCLo :=  $FF;
	for i := 0 to usDataLen - 1 do
	begin
  	uIndex := uchCRCHi xor puchMsg[i];
    uchCRCHi := uchCRCLo xor auchCRCHi [uIndex];
    uchCRCLo := auchCRCLo[uIndex];
	end;
	Result := (uchCRCHi shl 8) or uchCRCLo;
end;

procedure Vacon_CRC(var buf: array of Byte; Len: int);
var CRC: WORD;
begin
  CRC := Vacon_CalcCRC(buf, Len);

  buf[Len] := CRC div 256;
  buf[Len + 1] := CRC mod 256;
end;


function Vacon_CheckCRC(buf: array of Byte; Len: int): bool;
var CRC: WORD;
begin
  if Len < 3 then
  begin
    Result := false;
    Exit;
  end;

  CRC := Vacon_CalcCRC(buf, Len - 2);

  Result := (buf[Len - 2] = CRC div 256) and (buf[Len - 1] = CRC mod 256);
end;

{ TVaconNXLReqCreator }

procedure TVaconNXLReqCreator.AddVaconNXLToSendBuf_OneQuery(VaconRegister: WORD; rqtp: T485RequestType);
var buf: array[0..8] of byte;
begin
  buf[0] := $01;
  buf[1] := $03;
  buf[2] := VaconRegister div 256;
  buf[3] := VaconRegister mod 256;

  buf[4] := $00;
  buf[5] := $01;

  Vacon_CRC(buf, 6);

  AddBufRequestToSendBuf(buf, 8, rqtp);
end;

function TVaconNXLReqCreator.PrepareCommand(var prmIds: TChannelIds; var Val: double): bool;
begin
  Result := inherited;
  if not Result then Exit;

  case prmIds.ID_PT of
    ID_PT_START_ENGINE:
      prmIds.N_Src := 2000;
    ID_PT_PRESET:
      prmIds.N_Src := 2002;
    ID_PT_PID_REFERENCE:
      prmIds.N_Src := 2003;
    else
      Exit(false);
  end;

  prmIds.ID_Src := SRC_AO;
  Result := true;
end;

procedure TVaconNXLReqCreator.AddRequests;
begin
  // скорость двигателя
  AddVaconNXLToSendBuf_OneQuery(2104, rqtVACON_NXL_ENGINE_SPEED);
  // ток
  AddVaconNXLToSendBuf_OneQuery(2105, rqtVACON_NXL_ENGINE_CURRENT);
  // аварии
  AddVaconNXLToSendBuf_OneQuery(2110, rqtVACON_NXL_ALARM);
  // мощность
  AddVaconNXLToSendBuf_OneQuery(2107, rqtVACON_NXL_POWER);

  // уставка скорости двигателя
  AddVaconNXLToSendBuf_OneQuery(2002, rqtVACON_NXL_PRESET_SPEED);
  // опорное значение ПИД
  AddVaconNXLToSendBuf_OneQuery(2003, rqtVACON_NXL_PID_REFERENCE);
end;

end.


