////////////////////////////////////////////
// Кодировка и расчет CRC для приборов типа Взлет УРСВ
////////////////////////////////////////////
unit Devices.Vzlet;

interface

uses Windows, GMGlobals, GMConst, Devices.ReqCreatorBase, GmSqlQuery, SysUtils;

type
  TVzletURSVReqCreator = class(TDevReqCreator)
  private
    procedure AddVzletURSVToSendBuf_OneQuery(ID_Src, nChannel: int);
  public
    procedure AddRequests(); override;
  end;

procedure Vzlet_CRC(var buf: array of Byte; Len: int);
function Vzlet_CheckCRC(const buf: array of Byte; Len: int): bool;
function Vzlet_Register_ID(ID_Src, nChannel: int): WORD;
function Vzlet_Register_ReqType(ID_Src, nChannel: int): T485RequestType;

implementation

uses Devices.UBZ, ProgramLogFile;

// Взлет УРСВ использует ту же самую CRC с полиномом $A001, что и УБЗ

procedure Vzlet_CRC(var buf: array of Byte; Len: int);
begin
  UBZ_CRC(buf, Len);
end;

function Vzlet_CheckCRC(const buf: array of Byte; Len: int): bool;
begin
  Result := UBZ_CheckCRC(buf, Len);
end;

function Vzlet_Register_ID(ID_Src, nChannel: int): WORD;
begin
  Result := 0;

  if ID_Src = SRC_AI then
    case nChannel of
      1: Result := $C1A8;
      2: Result := $C1AA;
      3: Result := $C1AC;
      4: Result := $C1AE;
    end;
end;

function Vzlet_Register_ReqType(ID_Src, nChannel: int): T485RequestType;
begin
  Result := rqtNone;

  if ID_Src = SRC_AI then
    case nChannel of
      1: Result := rqtVZLET_URSV_Q1;
      2: Result := rqtVZLET_URSV_Q2;
      3: Result := rqtVZLET_URSV_Q3;
      4: Result := rqtVZLET_URSV_Q4;
    end;
end;

{ TVzletURSVReqCreator }


procedure TVzletURSVReqCreator.AddVzletURSVToSendBuf_OneQuery(ID_Src, nChannel: int);
var buf: array[0..20] of byte;
    rqtp: T485RequestType;
begin
  rqtp := Vzlet_Register_ReqType(ID_Src, nChannel);
  if rqtp = rqtNone then Exit;

  buf[0] := FReqDetails.DevNumber and $FF;
  buf[1] := 04; // запрос регистра
  WriteWORDInv(buf, 2, Vzlet_Register_ID(ID_Src, nChannel));
  buf[4] := 0;
  buf[5] := 2; // число регистров
  Vzlet_CRC(buf, 6);

  AddBufRequestToSendBuf(buf, 8, rqtp);
end;

procedure TVzletURSVReqCreator.AddRequests;
var q: TGMSqlQuery;
begin
  q := TGMSqlQuery.Create();
  try
    q.SQL.Text := 'select * from Params where ID_Device = ' + IntToStr(FReqDetails.ID_Device) + ' and ID_PT > 0 order by ID_Src, N_Src';
    q.Open();
    while not q.Eof do
    begin
      AddVzletURSVToSendBuf_OneQuery(q.FieldByName('ID_Src').AsInteger,
                                     q.FieldByName('N_Src').AsInteger );

      q.Next();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('AddVzletURSVToSendBuf - ' + e.Message);
  end;
  q.Free();
end;

end.
