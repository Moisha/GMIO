unit Devices.Modbus.ReqCreatorBase;

interface

uses Windows, GMGlobals, SysUtils, GMSqlQuery, GMConst, Devices.ReqCreatorBase, Devices.ModbusBase,
     Generics.Collections, Math;

type
  TModbusChannel = record
    id_prm, userSrc, addr, resultType: int;
    function addr2(): int;
  end;

  TModbusDevReqCreator = class(TDevReqCreator)
  private
    ChnList: TList<TModbusChannel>;
    FPackSize: int;
    procedure AddCurrents_OnePrm(q: TGMSqlQuery; obj: pointer);
    function CheckOutputChannelType(ID_Src: int): bool;
    function FncFromUserSrc(userSrc: int): int;
    procedure CreateChannelList;
    procedure ProcessFunction(userSrc: int);
  protected
    procedure AddModbusRequestToSendBuf(MemAddr, Cnt: int; rqtp: T485RequestType; ModbusFunction: byte = $03); virtual; abstract;
    function GetDefaultReqPackSize(): int; virtual; abstract;
    procedure DoSetChannelValue(DevNumber, ID_Src, N_Src, Val: int); virtual; abstract;
    function PrepareCommand(var prmIds: TChannelIds; var Val: double): bool; override;
  public
    constructor Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails); override;
    destructor Destroy; override;
    procedure AddRequests; override;
    function SetChannelValue(DevNumber, ID_Src, N_Src: int; Val: double; timeHold: int): bool; override;
  end;

  TModbusTCPDevReqCreator = class(TModbusDevReqCreator)
  protected
    procedure AddModbusRequestToSendBuf(MemAddr, Cnt: int; rqtp: T485RequestType; ModbusFunction: byte = $03); override;
    function GetDefaultReqPackSize(): int; override;
    procedure DoSetChannelValue(DevNumber, ID_Src, N_Src, Val: int); override;
  end;

  TModbusRTUDevReqCreator = class(TModbusDevReqCreator)
  protected
    procedure AddModbusRequestToSendBuf(MemAddr, Cnt: int; rqtp: T485RequestType; ModbusFunction: byte = $03); override;
    function GetDefaultReqPackSize(): int; override;
    procedure DoSetChannelValue(DevNumber, ID_Src, N_Src, Val: int); override;
 end;

implementation

{ TModbusDevReqCreator }

function TModbusDevReqCreator.FncFromUserSrc(userSrc: int): int;
begin
  Result := -1;

  case userSrc of
    SRC_DI: Result := 2;  // Modbus Discrete Inputs
    SRC_AI, SRC_CNT_MTR, SRC_CNT_DI: Result := 4; // Modbus input register
    SRC_AO, SRC_AO_10, SRC_DO: Result := 3; // Modbus holding register
  end;
end;

procedure TModbusDevReqCreator.AddCurrents_OnePrm(q: TGMSqlQuery; obj: pointer);
var addrBase: int;
    chn: TModbusChannel;
begin
  FPackSize := q.FieldByName('ReqPackSize').AsInteger;
  if FPackSize <= 0 then
    FPackSize := GetDefaultReqPackSize();

  chn.id_prm := q.FieldByName('ID_Prm').AsInteger;
  chn.userSrc := Max(q.FieldByName('UserSrc').AsInteger, SRC_AI);
  // нюанс: приборы зачастую нумеруют каналы Modbus с 1, а физически они с 0
  // поэтому все каналы со сдвигом на 1
  // Но бывает и наоборот, поэтому введен поправочный сдвиг addrBase, по дефолту 1
  addrBase := q.FieldByName('AddrBase').AsInteger;
  chn.addr := q.FieldByName('CurrentsAddr').AsInteger - addrBase;
  chn.resultType := q.FieldByName('ModbusResultType').AsInteger;

  ChnList.Add(chn);
end;

procedure TModbusDevReqCreator.CreateChannelList();
var sql: string;
begin
  sql := 'select * from DeviceSystem ' +
         '  where ID_Device = ' + IntToStr(FReqDetails.ID_Device) +
         '        and ID_Src = ' + IntToStr(SRC_USR) +
         '        and ID_PT > 0 ' +
         '        and N_Src >= 0 ' +
         '  order by UserSrc, CurrentsAddr';

  ReadFromQuery(sql, AddCurrents_OnePrm);
end;

function TModbusDevReqCreator.PrepareCommand(var prmIds: TChannelIds; var Val: double): bool;
begin
  Result := inherited;
  if not Result then Exit;

  prmIds.N_Src := prmIds.N_Src - prmIds.AddrBase;
  Result := prmIds.N_Src >= 0;
end;

procedure TModbusDevReqCreator.ProcessFunction(userSrc: int);
var i, addr1, addr2, fnc, n: int;

  procedure ClearReqDetails();
  begin
    addr1 := -1;
    FReqDetails.ClearReqLink();
  end;

  procedure InitReqPack(chn: TModbusChannel);
  begin
    addr1 := chn.addr;
    addr2 := chn.addr2();
    FReqDetails.ID_Prm := chn.id_prm;
    FReqDetails.ClearReqLink();
    FReqDetails.ReqLinks[0].id := chn.id_prm;
    FReqDetails.ReqLinks[0].ext := chn.resultType;
    FReqDetails.ReqLinkCount := 1;
  end;

  procedure SendCurrentPack();
  var count: int;
  begin
    if addr1 < 0 then Exit;

    count := addr2 - addr1 + 1;
    AddModbusRequestToSendBuf(addr1, count, rqtUSER_DEFINED, fnc);
  end;

begin
  ClearReqDetails();
  fnc := FncFromUserSrc(userSrc);
  if fnc < 0 then Exit;

  for i := 0 to ChnList.Count - 1 do
  begin
    if ChnList[i].userSrc <> userSrc then
      continue;

    if addr1 < 0 then
    begin
      InitReqPack(ChnList[i]);
    end
    else
    begin
      if ChnList[i].addr > addr1 + FPackSize - 1 then
      begin
        SendCurrentPack();
        InitReqPack(ChnList[i]);
      end
      else
      begin
        n := ChnList[i].addr - addr1;
        addr2 := Max(addr2, ChnList[i].addr2());
        FReqDetails.ReqLinks[n].id := ChnList[i].id_prm;
        FReqDetails.ReqLinks[n].ext := ChnList[i].resultType;
        FReqDetails.ReqLinkCount := n + 1;
      end;
    end;
  end;

  SendCurrentPack();
end;

function TModbusDevReqCreator.SetChannelValue(DevNumber, ID_Src, N_Src: int; Val: double; timeHold: int): bool;
begin
  if not CheckOutputChannelType(ID_Src)
     or not IsWord(Round(Val))
     or not IsWord(N_Src)
     or (N_Src < 0) then Exit(false);

  DoSetChannelValue(DevNumber, ID_Src, N_Src, Round(Val));
  Result := true;
end;

procedure TModbusDevReqCreator.AddRequests;
var i: int;
begin
  CreateChannelList();
  for i := SRC_AI to SRC_MAX do
    ProcessFunction(i);
end;

function TModbusDevReqCreator.CheckOutputChannelType(ID_Src: int): bool;
begin
  Result := ID_Src in [SRC_AO, SRC_AO_10];
end;

constructor TModbusDevReqCreator.Create(AddRequestToSendBuf: TGMAddRequestToSendBufProc; ReqDetails: TRequestDetails);
begin
  inherited;
  ChnList := TList<TModbusChannel>.Create();
end;

destructor TModbusDevReqCreator.Destroy;
begin
  ChnList.Free();
  inherited;
end;

function CreateModbusTCPRequest(DevNumber, MemAddr, Cnt: int; ModbusFunction: byte): ArrayOfByte;
begin
  SetLength(Result, 12);
  // Пример
  // ? 00 FF 00 00 00 06 01 04 00 00 00 0A
  // < 00 FF 00 00 00 17 01 04 14 00 00 00 01 00 02 00 03 27 10 FF FF FF FD 00 04 00 05 00 06

  // Transaction Identifier 2 Bytes
  Result[0] := 0;
  Result[1] := 0;

  // Protocol Identifier 2 Bytes 0 = MODBUS protocol
  Result[2] := 0;
  Result[3] := 0;

  // Length 2 Bytes, max 2000
  Result[4] := 0;
  Result[5] := 6;

  // Unit Identifier 1 Byte
  Result[6] := DevNumber and $FF;

  Result[7] := ModbusFunction;

  // адрес первого слова данных
  Result[8] := (MemAddr div 256) and $FF;
  Result[9] := MemAddr mod 256;

  // к-во слов
  Result[10] := (Cnt div 256) and $FF;
  Result[11] := Cnt mod 256;
end;

{ TModbusTCPDevReqCreator }

procedure TModbusTCPDevReqCreator.AddModbusRequestToSendBuf(MemAddr, Cnt: int; rqtp: T485RequestType; ModbusFunction: byte = $03);
begin
  AddBufRequestToSendBuf(CreateModbusTCPRequest(FReqDetails.DevNumber, MemAddr, Cnt, ModbusFunction), 12, rqtp);
end;

function TModbusTCPDevReqCreator.GetDefaultReqPackSize: int;
begin
  Result := 500;
end;

function CreateRequestSetChannelValueTCPFnc6(DevNumber, N_Src, Val: int): ArrayOfByte;
begin
  SetLength(Result, 12);

  // Transaction Identifier 2 Bytes
  Result[0] := 0;
  Result[1] := 0;

  // Protocol Identifier 2 Bytes 0 = MODBUS protocol
  Result[2] := 0;
  Result[3] := 0;

  // Length 2 Bytes
  Result[4] := 0;
  Result[5] := 6;

  // Unit Identifier 1 Byte
  Result[6] := DevNumber and $FF;

  // Функция 6 - запись значения в один регистр хранения (Preset Single Register)
  Result[7] := 6;

  // адрес
  Result[8] := HiByte(N_Src);
  Result[9] := LoByte(N_Src);

  // значение
  Result[10] := HiByte(Val mod 65536);
  Result[11] := LoByte(Val mod 65536);
end;

function CreateRequestSetChannelValueTCPFnc10(DevNumber, N_Src, Val: int): ArrayOfByte;
begin
  SetLength(Result, 12);

  // Transaction Identifier 2 Bytes
  Result[0] := 0;
  Result[1] := 0;

  // Protocol Identifier 2 Bytes 0 = MODBUS protocol
  Result[2] := 0;
  Result[3] := 0;

  // Length 2 Bytes
  Result[4] := 0;
  Result[5] := 9;

  // Unit Identifier 1 Byte
  Result[6] := DevNumber and $FF;

  // Функция 6 - запись значения в несколько регистров хранения
  Result[7] := $10;

  // адрес
  Result[8] := HiByte(N_Src);
  Result[9] := LoByte(N_Src);

  // пишем 1 значение
  Result[10] := 0;
  Result[11] := 1;

  // Количество байт далее
  Result[12] := 2;

  // значение
  Result[13] := HiByte(Val mod 65536);
  Result[14] := LoByte(Val mod 65536);
end;

function CreateRequestSetChannelValueTCP(DevNumber, ID_Src, N_Src, Val: int): ArrayOfByte;
begin
  SetLength(Result, 0);
  case ID_Src of
    SRC_AO:
      Result := CreateRequestSetChannelValueTCPFnc6(DevNumber, N_Src, Val);
    SRC_AO_10:
      Result := CreateRequestSetChannelValueTCPFnc10(DevNumber, N_Src, Val);
  end;
end;

procedure TModbusTCPDevReqCreator.DoSetChannelValue(DevNumber, ID_Src, N_Src, Val: int);
var buf: ArrayOfByte;
begin
  buf := CreateRequestSetChannelValueTCP(DevNumber, ID_Src, N_Src, Round(Val));
  AddBufRequestToSendBuf(buf, Length(buf), rqtCommand);
end;

function CreateModbusRTURequest(DevNumber, MemAddr, Cnt: int; ModbusFunction: byte): ArrayOfByte;
begin
  SetLength(Result, 8);

  // номер прибора на линии
  Result[0] := DevNumber and $FF;

  // код функции запроса данных
  Result[1] := ModbusFunction;

  // адрес первого слова данных
  Result[2] := (MemAddr div 256) and $FF;
  Result[3] := MemAddr mod 256;

  // к-во слов, до 2000
  Result[4] := (Cnt div 256) and $FF;
  Result[5] := Cnt mod 256;

  // CRC
  Modbus_CRC(Result, 6);
end;

{ TModbusRTUDevReqCreator }

procedure TModbusRTUDevReqCreator.AddModbusRequestToSendBuf(MemAddr, Cnt: int; rqtp: T485RequestType; ModbusFunction: byte  = $03);
var buf: ArrayOfByte;
begin
  if FReqDetails.Converter = PROTOCOL_CONVETER_MODBUS_TCP_RTU then
    buf := CreateModbusTCPRequest(FReqDetails.DevNumber, MemAddr, Cnt, ModbusFunction)
  else
    buf := CreateModbusRTURequest(FReqDetails.DevNumber, MemAddr, Cnt, ModbusFunction);

  AddBufRequestToSendBuf(buf, Length(buf), rqtp);
end;

function TModbusRTUDevReqCreator.GetDefaultReqPackSize: int;
begin
  Result := 100;
end;

function CreateRequestSetChannelValueRTUFnc6(DevNumber, N_Src, Val: int): ArrayOfByte;
begin
  SetLength(Result, 8);
  Result[0] := DevNumber and $FF;
  Result[1] := 6; // запись значения в один регистр хранения (Preset Single Register)
  Result[2] := HiByte(N_Src);
  Result[3] := LoByte(N_Src);
  Result[4] := HiByte(Val mod 65536);
  Result[5] := LoByte(Val mod 65536);

  Modbus_CRC(Result, 6);
end;

function CreateRequestSetChannelValueRTUFnc10(DevNumber, N_Src, Val: int): ArrayOfByte;
begin
  SetLength(Result, 11);
  Result[0] := DevNumber and $FF;
  Result[1] := $10; // запись значения в несколько регистров хранения
  Result[2] := HiByte(N_Src);
  Result[3] := LoByte(N_Src);
  Result[4] := 0;
  Result[5] := 1;  // 4, 5 - Количество регистров
  Result[6] := 2;  // Количество байт далее
  Result[7] := HiByte(Val mod 65536);
  Result[8] := LoByte(Val mod 65536);

  Modbus_CRC(Result, 9);
end;

function CreateRequestSetChannelValueRTU(DevNumber, ID_Src, N_Src, Val: int): ArrayOfByte;
begin
  SetLength(Result, 0);
  case ID_Src of
    SRC_AO: Result := CreateRequestSetChannelValueRTUFnc6(DevNumber, N_Src, Val);
    SRC_AO_10: Result := CreateRequestSetChannelValueRTUFnc10(DevNumber, N_Src, Val);
  end;
end;

procedure TModbusRTUDevReqCreator.DoSetChannelValue(DevNumber, ID_Src, N_Src, Val: int);
var buf: ArrayOfByte;
begin
  if FReqDetails.Converter = PROTOCOL_CONVETER_MODBUS_TCP_RTU then
    buf := CreateRequestSetChannelValueTCP(DevNumber, ID_Src, N_Src, Val)
  else
    buf := CreateRequestSetChannelValueRTU(DevNumber, ID_Src, N_Src, Val);


  AddBufRequestToSendBuf(buf, Length(buf), rqtCommand);
end;

{ TModbusChannel }

function TModbusChannel.addr2: int;
begin
  Result := addr + Modbus_ResultTypeLength(resultType) - 1;
end;

end.
