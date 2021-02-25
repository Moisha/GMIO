unit Connection.Base;

interface

uses Windows, GMConst, GMGlobals, ScktComp, StrUtils, StdResponce;

type
  TExchangeType = (etSend, etRec, etSenRec);
  TCheckCOMResult = (ccrBytes, ccrEmpty, ccrError);
  BoolReturningFunc = function: bool of object;
  TRequestUniversalXmlContent = (ruxcRequest, ruxcResponce);

  TConnectionObjectBase = class
  private
    FNumAttempts: int;
    FLogPrefix: string;

    FCheckTerminated: BoolReturningFunc;
    FExternalCheckGetAllData: BoolReturningFunc;
    FInitEquipmentBeforeExchange: bool;

    FWaitFirst, FWaitNext: int;

    function GetParentTerminated: bool;
    procedure SetWaitFirst(const Value: int);
    procedure SetWaitWaitNext(const Value: int);
  protected
    procedure PrepareEquipment; virtual;
    function ConnectionEquipmentInitialized(): bool; virtual;

    function LogSignature: string; virtual; abstract;
    procedure LogBufSend(AddCOMState: bool = false);
    procedure LogBufRec(logEmpty: bool = false);

    function MakeExchange(etAction: TExchangeType): TCheckCOMResult; virtual; abstract;
    function InternalCheckGetAllData: bool; virtual;
    function GetLastConnectionError: string; virtual;
    function DefaultWaitFirst: int; virtual;
    function DefaultWaitNext: int; virtual;
  public
    buffers: TTwoBuffers;

    constructor Create(); virtual;
    destructor Destroy; override;

    function ExchangeBlockData(etAction: TExchangeType): TCheckCOMResult; virtual;
    function RequestUniversal(const xml, login: string; bUseLogin: bool; contentType: TRequestUniversalXmlContent; var responce: IXMLGMIOResponceType): TCheckCOMResult;

    procedure FreePort; virtual;

    function CheckBufHeader(buf0, buf1, cnt: int): bool;
    function CheckGetAllData: bool;

    property WaitFirst: int read FWaitFirst write SetWaitFirst;
    property WaitNext: int read FWaitNext write SetWaitWaitNext;

    property NumAttempts: int read FNumAttempts write FNumAttempts;
    property CheckTerminated: BoolReturningFunc read FCheckTerminated write FCheckTerminated;
    property ExternalCheckGetAllData: BoolReturningFunc read FExternalCheckGetAllData write FExternalCheckGetAllData;
    property ParentTerminated: bool read GetParentTerminated;
    property LogPrefix: string read FLogPrefix write FLogPrefix;
    property LastError: string read GetLastConnectionError;
    property InitEquipmentBeforeExchange: bool read FInitEquipmentBeforeExchange write FInitEquipmentBeforeExchange;
  end;

  TConnectionObjectClass = class of TConnectionObjectBase;

implementation

{ TConnectionObjectBase }

uses ProgramLogFile, Math, EsLogging, System.SysUtils;

function TConnectionObjectBase.CheckBufHeader(buf0, buf1, cnt: int): bool;
begin
  Result := (buffers.BufRec[0] = buf0) and (buffers.BufRec[1] =  buf1) and (buffers.NumberOfBytesRead >= cnt);
end;

function TConnectionObjectBase.CheckGetAllData: bool;
begin
  Result := InternalCheckGetAllData()
            or (Assigned(FExternalCheckGetAllData) and FExternalCheckGetAllData());
end;

function TConnectionObjectBase.ConnectionEquipmentInitialized: bool;
begin
  Result := true;
end;

constructor TConnectionObjectBase.Create;
begin
  inherited;

  FInitEquipmentBeforeExchange := true;
  FWaitFirst := DefaultWaitFirst();
  FWaitNext := DefaultWaitNext();
  FNumAttempts := 1;
end;

destructor TConnectionObjectBase.Destroy;
begin
  FreePort();

  inherited;
end;

function TConnectionObjectBase.ExchangeBlockData(etAction: TExchangeType): TCheckCOMResult;
var Attempt: word;
begin
  if FInitEquipmentBeforeExchange or not ConnectionEquipmentInitialized() then
    PrepareEquipment();
  //выход если нет инициализации
  if not ConnectionEquipmentInitialized() then
  begin
    Result := ccrError;
  end
  else
  begin
    if etAction in [etSend, etSenRec] then
      LogBufSend(true);

    Attempt := 0;
    repeat
      Result := MakeExchange(etAction);
      Inc(Attempt);
    until (Result = ccrBytes)
          or ((etAction = etSend) and (Result = ccrEmpty))
          or (Attempt >= FNumAttempts);

    if etAction = etSend then
      Exit;

    case Result of
      ccrBytes:
        LogBufRec();

      ccrEmpty:
        if etAction = etSenRec then
          DefaultLogger.Debug(LogSignature() + ' - no answer, timeout = ' + IntToStr(WaitFirst));

      ccrError:
        DefaultLogger.Debug(LogSignature() + ' - error: ' + LastError);
    end;
  end;
end;

function TConnectionObjectBase.RequestUniversal(const xml, login: string; bUseLogin: bool; contentType: TRequestUniversalXmlContent; var responce: IXMLGMIOResponceType): TCheckCOMResult;
var len: int;
    respXml: string;
    buf0, buf1: byte;
begin
  Result := ccrEmpty;
  responce := nil;

  buf0 := IfThen(contentType = ruxcResponce, 250, 255);
  buf1 := IfThen(contentType = ruxcResponce, 10,  0);
  if not buffers.RequestUniversal(xml, login, bUseLogin, buf0, buf1) then Exit;

  Programlog.AddExchangeBuf('RequestUniversal', COM_LOG_OUT, xml);
  Result := ExchangeBlockData(etSenRec);
  if Result <> ccrBytes then Exit;

  Result := ccrEmpty;
  if not buffers.CheckBufRecHeader(buf0, buf1, 6) then Exit;

  len := ReadUINT(buffers.BufRec, 2);
  if buffers.NumberOfBytesRead < len + 6 then Exit;

  try
    respXml := string(DecompressBufferToString(buffers.BufRec, 6, len));
    Programlog.AddExchangeBuf('RequestUniversal', COM_LOG_IN, respXml);
    responce := LoadXMLData_GMIOResponce(respXml);
    if responce <> nil then
      Result := ccrBytes;
  except
    Result := ccrError;
  end;
end;

function TConnectionObjectBase.DefaultWaitFirst: int;
begin
  Result := CommonWaitFirst;
end;

function TConnectionObjectBase.DefaultWaitNext: int;
begin
  Result := CommonWaitNext;
end;

procedure TConnectionObjectBase.SetWaitFirst(const Value: int);
begin
  if Value > 0 then
    FWaitFirst := Value
  else
    FWaitFirst := DefaultWaitFirst;
end;

procedure TConnectionObjectBase.SetWaitWaitNext(const Value: int);
begin
  if Value > 0 then
    FWaitNext := Value
  else
    FWaitNext := DefaultWaitNext;
end;

procedure TConnectionObjectBase.FreePort;
begin

end;

function TConnectionObjectBase.GetLastConnectionError: string;
begin
  Result := ''
end;

function TConnectionObjectBase.GetParentTerminated: bool;
begin
  Result := Assigned(FCheckTerminated) and FCheckTerminated();
end;

function TConnectionObjectBase.InternalCheckGetAllData: bool;
begin
  Result := false;
end;

procedure TConnectionObjectBase.LogBufRec(logEmpty: bool);
begin
  if logEmpty or (buffers.NumberOfBytesRead > 0) then
    ProgramLog.AddExchangeBuf(LogSignature(), COM_LOG_IN, buffers.BufRec, buffers.NumberOfBytesRead);
end;

procedure TConnectionObjectBase.LogBufSend(AddCOMState: bool);
begin
  ProgramLog.AddExchangeBuf(LogSignature() + IfThen(AddCOMState, IfThen(not ConnectionEquipmentInitialized(), '-')), COM_LOG_OUT, buffers.BufSend, buffers.LengthSend);
end;

procedure TConnectionObjectBase.PrepareEquipment;
begin

end;

end.
