//////////////////////////////////////////
// Процедуры для работы с приборами Тэкон через модем К-105
//////////////////////////////////////////

unit Threads.K105;

interface
uses Windows, Classes, SysUtils, GMConst, GMGlobals, IdSocketHandle,
     Threads.ReqSpecDevTemplate, GM485, GMGenerics, UDPBindingStorage,
     Connection.UDP, Connection.Base;

type
  TConnectionObjectK105 = class(TConnectionObjectIncomingUDP)

  end;

  TRequestK105Devices = class(TRequestSpecDevices)
  private
    lst485IDs: array [0..$F] of TRequestDetails;
    FReplyList: TGMCollection<TSpecDevReqListItem>;
    nFirstPacket: int;
    FUDPBindingsAndThreads: TUDPBindingsAndThreads;

    function GetEmptyID: int;
    function GetSocket(): TIdSocketHandle;
    procedure CheckReplyList;
  protected
    procedure BackGroungProc; override;
    procedure DoOneRequest(ri: TSpecDevReqListItem); override;
  public
    constructor Create(id_obj: int; AUDPBindingsAndThreads: TUDPBindingsAndThreads);
    procedure AddReplyBlock(buf: array of Byte; Len: int);
    procedure BeforeDestruction(); override;
  end;

implementation

{ TRequestK105Devices }

uses ProgramLogFile, Devices.Tecon.Common;

function TRequestK105Devices.GetEmptyID(): int;
var i: int;
begin
  Result := -1;
  try
    for i := nFirstPacket to High(lst485IDs) do
      if lst485IDs[i].rqtp = rqtNone then
      begin
        Result := i;
        Exit;
      end;

    // не нашли или перешли через 0
    for i := 0 to High(lst485IDs) do
      if lst485IDs[i].rqtp = rqtNone then
      begin
        Result := i;
        Exit;
      end;
  finally
    if Result >= 0 then
      nFirstPacket := Result + 1;
  end;
end;

procedure TRequestK105Devices.CheckReplyList();
var i, nReq: int;
    ri: TSpecDevReqListItem;
begin
  // зачистим неотвеченные
  for i := 0 to High(lst485IDs) do
    if Abs(lst485IDs[i].TimeTick - GetTickCount()) > 20000 then
      lst485IDs[i].rqtp := rqtNone;

  while FReplyList.Count > 0 do
  begin
    ri := FReplyList.Items[0];

    ConnectionObject.buffers.NumberOfBytesRead := ri.ReqDetails.BufCnt;
    WriteBuf(ConnectionObject.buffers.BufRec, 0, ri.ReqDetails.buf, ConnectionObject.buffers.NumberOfBytesRead);
    TConnectionObjectK105(ConnectionObject).LogBufRec();

    if Tecon_CheckPrmReply(ri.ReqDetails.buf, ri.ReqDetails.BufCnt) then
    begin
      nReq := ri.ReqDetails.buf[4] and $0F;
      if lst485IDs[nReq].rqtp <> rqtNone then
      begin
        PostGBV(lst485IDs[nReq], ri.ReqDetails.buf, ri.ReqDetails.BufCnt);
        lst485IDs[nReq].rqtp := rqtNone;
      end;
    end;

    FReplyList.Delete(0);
  end;
end;

procedure TRequestK105Devices.BackGroungProc();
begin
  inherited BackGroungProc();

  CheckReplyList();
end;

function TRequestK105Devices.GetSocket: TIdSocketHandle;
begin
  Result := FUDPBindingsAndThreads.ByThread(self);
end;

procedure TRequestK105Devices.DoOneRequest(ri: TSpecDevReqListItem);
var n: int;
    sckt: TIdSocketHandle;
    ext: TPrepareRequestBufferInfo;
begin
  // Сначала ищем пустой номер, а потом сокет.
  // Иначе, пока ждем этот самый пустой номер, сокет уже может отвалиться 
  repeat
    if Terminated then Exit;

    n := GetEmptyID();
    if n < 0 then
    begin
      SleepThread(100);
      CheckReplyList();
    end;
  until n >= 0;

  sckt := GetSocket();
  if sckt = nil then
  begin
    Terminate;
    Exit;
  end;

  ext.ReqID := n;

  ri.PrepareBuffer(ext);
  lst485IDs[n] := ri.ReqDetails;
  lst485IDs[n].TimeTick := GetTickCount();

  WriteBuf(ConnectionObject.buffers.BufSend, 0, lst485IDs[n].buf, lst485IDs[n].BufCnt);
  ConnectionObject.buffers.LengthSend := lst485IDs[n].BufCnt;

  // раньше отправка была через message в главную форму.
  // не знаю, зачем так было, сделал напрямки
  try
    TConnectionObjectK105(ConnectionObject).Socket := sckt;
    ConnectionObject.ExchangeBlockData(etSend);
  except
    on e: Exception do
      ProgramLog.AddException('TConnectionObjectK105.ExchangeBlockData ' + e.Message);
  end;

  SleepThread(200);
end;

procedure TRequestK105Devices.BeforeDestruction;
begin
  FUDPBindingsAndThreads.RemoveThread(self);
  FReplyList.Free();
  inherited;
end;

constructor TRequestK105Devices.Create(id_obj: int; AUDPBindingsAndThreads: TUDPBindingsAndThreads);
begin
  inherited Create(OBJ_TYPE_K105, id_obj);

  FReplyList := TGMCollection<TSpecDevReqListItem>.Create();
  nFirstPacket := 0;
  FreeOnTerminate := true;
  FUDPBindingsAndThreads := AUDPBindingsAndThreads;

  CreateConnectionObject(TConnectionObjectK105);
  ConnectionObject.LogPrefix := 'K105_ObjId=' + IntToStr(ID_Obj);
end;

procedure TRequestK105Devices.AddReplyBlock(buf: array of Byte; Len: int);
var rd: TRequestDetails;
begin
  rd.Init();
  if Len < Length(rd.buf) then
  begin
    WriteBuf(rd.buf, 0, buf, Len);
    rd.BufCnt := Len;
    FReplyList.Add().ReqDetails := rd;
  end;
end;

end.
