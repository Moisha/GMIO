unit Connection.COM;

interface

uses Windows, SysUtils, GMConst, GMGlobals, Connection.Base, AnsiStrings, StrUtils;

type
  TConnectionObjectCOM = class(TConnectionObjectBase)
  private
    hPort : cardinal;
    StateComPort: bool;
    FLastError: string;
    function CheckPortBuf: TComStat;
    function SendBuf: TCheckCOMResult;
    function ReceiveBuf: TCheckCOMResult;
  protected
    procedure PrepareEquipment; override;
    function ConnectionEquipmentInitialized(): bool; override;
    function MakeExchange(etAction: TExchangeType): TCheckCOMResult; override;
    function LogSignature: string; override;
    function GetLastConnectionError: string; override;
  public
    nPort, nBaudRate, nWordLen, nParity, nStopBits: int;

    function SendBreak: bool;
    procedure FreePort; override;
    constructor Create(); override;
  end;

implementation

{ TConnectionObjectCOM }

function TConnectionObjectCOM.ConnectionEquipmentInitialized: bool;
begin
  Result := StateComPort;
end;

constructor TConnectionObjectCOM.Create;
begin
  inherited Create();

  hPort:=INVALID_HANDLE_VALUE;

  nBaudRate := 9600;
  nWordLen := 8;
  nParity := 0;
  nStopBits := 0;
end;

procedure TConnectionObjectCOM.FreePort;
begin
  if hPort <> INVALID_HANDLE_VALUE then
  begin
    try CloseHandle(hPort); except end;
    hPort := INVALID_HANDLE_VALUE;
    StateComPort := false;
  end;
end;

function TConnectionObjectCOM.GetLastConnectionError: string;
begin
  Result := FLastError;
end;

function TConnectionObjectCOM.LogSignature: string;
begin
   Result := 'COM_' + IntToStr(nPort);
end;

function TConnectionObjectCOM.CheckPortBuf(): TComStat;
var Errors: DWORD;
    Stat : TComStat;   // buffer for communications status
begin
  Result.cbInQue:=0;
  Result.cbOutQue:=0;
  if ClearCommError(hPort, Errors, @Stat) then
    Result:=Stat;
end;

function TConnectionObjectCOM.SendBreak: bool;
begin
  PrepareEquipment();
  if not StateComPort then
    Exit(false);

  if not SetCommBreak(hPort) then
  begin
    FLastError := 'SetCommBreak '  + SysErrorMessage(GetLastError());
    Exit(false);
  end;

  Sleep(WaitNext);
  Result := ClearCommBreak(hPort);
  if not Result then
    FLastError := 'ClearCommBreak '  + SysErrorMessage(GetLastError());
end;

function TConnectionObjectCOM.SendBuf(): TCheckCOMResult;
var tick, t: int64;
    NumberOfBytesWritten : DWORD;
begin
  Result := ccrEmpty;

  //Очистка порта
  if not PurgeComm(hPort, PURGE_TXABORT	or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR) then
  begin
    FLastError := 'PurgeComm '  + SysErrorMessage(GetLastError());
    Exit(ccrError);
  end;

  //Пауза в 0,01 сек
  tick := GetTickCount();
  while (tick + 10 > GetTickCount()) do sleep(10);

  //Передача сообщения
  if not WriteFile(hPort, buffers.BufSend, buffers.LengthSend, NumberOfBytesWritten, NIL) then
  begin
    FLastError := 'WriteFile '  + SysErrorMessage(GetLastError());
    Exit(ccrError);
  end;

  //дождемся окончания приема, максимум тупим минуту
  t := GetTickCount();
  while (hPort <> INVALID_HANDLE_VALUE) and (CheckPortBuf().cbOutQue > 0) and (Abs(t - GetTickCount()) < 60000) do
    Sleep(100);
end;

function TConnectionObjectCOM.ReceiveBuf(): TCheckCOMResult;
var tick: UInt64;
    Errors: DWORD;
    n: Cardinal;
    Stat: TComStat;   // buffer for communications status
    i: WORD;
    Buf: array [0..102400] of byte;
begin
  //Ожидаем первого символа
  tick := GetTickCount64();
  Repeat
    if ParentTerminated then
      Exit(ccrEmpty);

    Sleep(100);
    ZeroMemory(@Stat, SizeOf(Stat));
    if not ClearCommError(hPort, Errors, @Stat) then
    begin
      FLastError := 'ClearCommError '  + SysErrorMessage(GetLastError());
      Exit(ccrError);
    end;

    if Abs(tick - GetTickCount64()) > WaitFirst then
      Exit(ccrEmpty);

  Until Stat.cbInQue > 0;

  Result := ccrEmpty;
  tick := GetTickCount64();
  Repeat
    if Stat.cbInQue > 0 then
    begin
      if not ReadFile(hPort, Buf, Stat.cbInQue, n, NIL) then
      begin
        FLastError := Format('ReadFile %d bytes - %s', [Stat.cbInQue, SysErrorMessage(GetLastError())]);
        Exit(ccrError);
      end;

      if n>0 then
      begin
        for i:=0 to n-1 do
          buffers.BufRec[buffers.NumberOfBytesRead + i] := Buf[i];

        // Abs поставил, чтобы убить Warning, n и так всегда положительный
        buffers.NumberOfBytesRead := buffers.NumberOfBytesRead + Abs(n);
        Result := ccrBytes;
        tick := GetTickCount64();
      end;

      if CheckGetAllData() then break;
    end;

    Sleep(100);

    if not ClearCommError(hPort, Errors, @Stat) then
    begin
      FLastError := 'ClearCommError '  + SysErrorMessage(GetLastError());
      Exit(ccrError);
    end;

  Until ParentTerminated or ((Stat.cbInQue = 0)and (Abs(GetTickCount64() - tick) > WaitNext));

  PurgeComm(hPort, PURGE_TXABORT	or PURGE_RXABORT or PURGE_TXCLEAR or PURGE_RXCLEAR);
end;

function TConnectionObjectCOM.MakeExchange(etAction: TExchangeType): TCheckCOMResult;
begin
  Result := ccrEmpty;
  buffers.NumberOfBytesRead := 0;


  if (etAction in [etSend, etSenRec]) and (buffers.LengthSend > 0) then
  begin
    Result := SendBuf();
    if Result = ccrError then
      Exit;
  end;

  if etAction in [etRec, etSenRec] then
    Result := ReceiveBuf();
end;

procedure TConnectionObjectCOM.PrepareEquipment();
var Buf : array [0..10] of char;
    Param : TDCB;
begin
  FreePort();
  if (nPort <= 0) or (nPort > 999) then Exit;

  StrPCopy(Buf, IfThen(nPort >= 10, '\\.\') + 'COM' + IntToStr(nPort)); // для портов 10 и выше формат '\\.\COMхх'

  hPort := CreateFile(
    Buf,
    GENERIC_READ or GENERIC_WRITE,
    0,    // comm devices must be opened w/exclusive-access
    NIL,    // no security attributes
    OPEN_EXISTING, // comm devices must use OPEN_EXISTING
    0,    // not overlapped I/O
    0);  // hTemplate must be NULL for comm devices

  If (hPort = INVALID_HANDLE_VALUE) then
  begin
    FLastError := 'CreateFile '  + SysErrorMessage(GetLastError());
    Exit;
  end;

  if not GetCommState(hPort, Param) then
  begin
    FLastError := 'GetCommState '  + SysErrorMessage(GetLastError());
    FreePort();
    Exit;
  end;

  Param.BaudRate := nBaudRate;
  Param.ByteSize := nWordLen;
  Param.Parity := nParity;
  Param.StopBits := nStopBits;

  If not SetCommState(hPort, Param) then
  begin
    FLastError := 'SetCommState '  + SysErrorMessage(GetLastError());
    FreePort();
    Exit;
  end;

  if not (SetupComm(hPort, 1024, 1024)) then
  begin
    FLastError := 'SetupComm '  + SysErrorMessage(GetLastError());
    FreePort();
    Exit;
  end;

  StateComPort := true;
end;

end.
