/// /////////////////////////////////////////
// Разбор запросов и формирование ответов
// для клиентов и приборов Геомер по TCP/IP и GPRS
/// /////////////////////////////////////////
unit GMSocket;

interface

uses Windows, SysUtils, ScktComp, GMGlobals, Winsock, Classes, RecordsForRmSrv, Math,
  GMSqlQuery, GMConst, zlib, StrUtils, StdRequest, StdResponce, ConnParamsStorage,
  GeomerLastValue, GMGenerics, GM485, Generics.Collections, RequestList, Threads.SQLExecuter;

type
  TGeomerSocketSQLExecuteThread = class(TSQLExecuteThread)
  private
    FXMLList: TStringList;
  protected
    procedure SafeExecute_ExtraStep; override;
    function GetCount: int; override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure AddResponceXml(const xml: string);
  end;

  TGeomerSocket = class(TServerClientWinSocket)
  private
    FLastReadUTime: int64;
    RmSrvRec: TRemoteSrvRecords;
    FAncomThread: TThread;
    FExecSQLThread: TGeomerSocketSQLExecuteThread;
    FNCar: int;
    glvBuffer: TGeomerLastValuesBuffer;

    FInfo0Queue: int;
    FInfo0LastReqUTime: int64;
    FLastTimeUpdate: TDateTime;
    FReqList: TRequestCollection;

    FLast485ID: int;
    FLast485BuilderUTime: int64;
    lst485IDs: array [0 .. 255] of TRequestDetails;
    procedure RemoteServerReport(var bufs: TTwoBuffers);
    procedure ProcessRemoteServerReport(report: IXMLGMIOResponceType);
    function RequestUniversal_SimpleResponce(req: IXMLGMIORequestType): IXMLGMIOResponceType;
    procedure SetChannelValue(var bufs: TTwoBuffers);
    procedure UpdateLastReadTime;

  const
    MAX_REQUESTS_IN_DEVICE = 32;
    ARCH_RESERVE = 5; // резерв под архивы. На каждом шаге опросим не меньше ARCH_RESERVE архивных заявок, если они есть
    GEOMER_RS485_TIMEOUT_SEC = 2;
    MAX_ANSWER_WAITING_TIME_MS = 1000 * (MAX_REQUESTS_IN_DEVICE * GEOMER_RS485_TIMEOUT_SEC + 5);
    // ждем ответа как максимум неотвеченных умножить на таймаут Геомера плюс на задержку линии
    GEOMER_485_BUFFER_SIZE = 2048;

    procedure IncLast485ID;
    function TimeToRequestCurrents: bool;
    procedure PackAndSend485(lst: TList<TSpecDevReqListItem>; cnt: int = 0);
    function HasReqsInDevice: bool;
    procedure CheckOld485Ids;
    procedure ProcessReqList;
    procedure Process485;

    procedure CheckInfo0();
    function ProcessAncomID(buf: array of byte; cnt: int): bool;
    procedure RequestOPCChannels(var bufs: TTwoBuffers);
    procedure RequestUniversal(var bufs: TTwoBuffers);
  protected
    FSocketObjectType: int;

    procedure LoadCommands;
    property ReqList: TRequestCollection read FReqList;
    property ExecSQLThread: TGeomerSocketSQLExecuteThread read FExecSQLThread;
    procedure GMSendText(const s: string); virtual;
    procedure SetNCar(const Value: int); virtual;
    procedure BuildReqList; virtual;
    function DecodeGMDataBlock(buf: array of byte; cnt: int): int; virtual;
    procedure ReadAndParseDataBlock_ProcessBuf(var buf: ArrayOfByte);
    function CheckGMWithUBZBlock(buf: array of byte; cnt: int; var blockCnt: int): bool;
  public
    arrDIC: array [0 .. 15] of bool; // нужные счетчики
    arrDI: array [0 .. 15] of bool; // нужные DI
    arrAI: array [0 .. 15] of bool; // нужные AI

    constructor Create(Socket: TSocket; ServerWinSocket: TServerWinSocket; AglvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy; override;

    property LastReadUTime: int64 read FLastReadUTime;
    property SocketObjectType: int read FSocketObjectType;
    property N_Car: int read FNCar write SetNCar;

    procedure BackgroundWork();

    procedure AnalyzeClientRequest(var bufs: TTwoBuffers);
    procedure ReadAndParseDataBlock;

    procedure Disconnect(Socket: TSocket); override;

    procedure SendCurrentTime();
    procedure RequestInfo0(count: int);
    procedure AddRequest(ReqDetails: TRequestDetails);
    procedure Geomer_SetOutputChannel(N_Src: int; fVal: double; TimeHold: UINT);
  end;

  TSocketList = class
  private
    FSynch: TMultiReadExclusiveWriteSynchronizer;
    FSocket: TServerWinSocket;
  protected
    function GetSocket(i: int): TGeomerSocket; virtual;
    function GetCout: int; virtual;
  public
    constructor Create(ASocket: TServerWinSocket);
    destructor Destroy(); override;
    property Sockets[i: int]: TGeomerSocket read GetSocket; default;
    function SocketByIdObj(ID_Obj: int): TGeomerSocket;
    function SocketByNCar(N_Car, ObjType: int): TGeomerSocket;
    procedure CheckSocketOldNCars(Sckt: TGeomerSocket);
    property Count: int read GetCout;
  end;

var
  lstSockets: TSocketList = nil;

implementation

uses Devices.UBZ, DB, Devices.Vacon, HTTPApp, XMLDoc, GMBlockValues,
  Devices.Ancom, Threads.AncomGPRS, ProgramLogFile, ClientResponce, UsefulQueries, Devices.TR101, Threads.Base,
  Threads.ReqSpecDevTemplate, DateUtils, Connection.TCP, Connection.Base, RequestThreadsContainer, Winapi.ActiveX,
  System.NetEncoding;

{ TGeomerSocket }

procedure TGeomerSocket.ProcessRemoteServerReport(report: IXMLGMIOResponceType);
begin
  if FExecSQLThread = nil then
    FExecSQLThread := TGeomerSocketSQLExecuteThread.Create();
  FExecSQLThread.AddResponceXml(report.XML);
end;

procedure TGeomerSocket.RemoteServerReport(var bufs: TTwoBuffers);
var
  len: int;
  xml: string;
  report, resp: IXMLGMIOResponceType;
begin
  FSocketObjectType := OBJ_TYPE_REMOTE_SRV_XML;

  len := ReadUINT(bufs.BufRec, 2);
  if (len = 0) or (bufs.NumberOfBytesRead < len + 6) then
    Exit;

  try
    xml := string(DecompressBufferToString(bufs.BufRec, 6, len));
    report := LoadXMLData_GMIOResponce(xml);
    ProgramLog.AddExchangeBuf('RemoteServerReport', COM_LOG_IN, report.xml);
    ProcessRemoteServerReport(report);
    resp := NewGMIOResponce();
    resp.Auth.OK := 1;
    resp.Auth.RequestID := report.Auth.RequestID;

    bufs.XmlToBufSend(resp.xml);
    ProgramLog.AddExchangeBuf('RemoteServerReport', COM_LOG_OUT, resp.xml);
  except
    on e: Exception do
      ProgramLog.AddException('TGeomerSocket.RequestUniversal: ' + e.Message);
  end;

  report := nil;
  resp := nil;
end;

function TGeomerSocket.RequestUniversal_SimpleResponce(req: IXMLGMIORequestType): IXMLGMIOResponceType;
begin
  Result := nil;
  if req.State.RemoteServerReady = 1 then
  begin
    Result := NewGMIOResponce();
    Result.Auth.OK := 1;
    Result.Auth.RequestID := req.Auth.RequestID;
    Result.State.RemoteServerReady := ord((FExecSQLThread = nil) or (FExecSQLThread.Count = 0));
  end;
end;

procedure TGeomerSocket.RequestUniversal(var bufs: TTwoBuffers);
var
  xml, userMd5: string;
  req: IXMLGMIORequestType;
  resp: IXMLGMIOResponceType;
  processor: TClientResponce;
begin
  if bufs.UniversalResponceLen() <= 0 then
    Exit;

  processor := TClientResponce.Create(glvBuffer);
  try
    userMd5 := ReadMd5String(bufs.BufRec, 2);
    xml := bufs.UniversalResponceXml();
    req := LoadXMLData_GMIORequest(xml);
    ProgramLog.AddExchangeBuf('RequestUniversal', COM_LOG_IN, req.xml);
    resp := RequestUniversal_SimpleResponce(req);
    if resp = nil then
      resp := processor.ProcessRequest(req, userMd5);

    if FSocketObjectType = OBJ_TYPE_UNKNOWN then
    begin
      if req.Auth.RemoteName <> '' then
        FSocketObjectType := OBJ_TYPE_REMOTE_SRV_XML // удаленный сервер нового образца
      else
        FSocketObjectType := OBJ_TYPE_CLIENT; // клиент
    end;

    ProgramLog.AddExchangeBuf('RequestUniversal', COM_LOG_OUT, resp.xml);
    bufs.XmlToBufSend(resp.xml);
  except
    on e: Exception do
      ProgramLog.AddException('TGeomerSocket.RequestUniversal: ' + e.Message);
  end;

  processor.Free();
  req := nil;
  resp := nil;
end;

procedure TGeomerSocket.RequestInfo0(count: int);
begin
  FInfo0Queue := Max(FInfo0Queue, count);
end;

procedure TGeomerSocket.RequestOPCChannels(var bufs: TTwoBuffers);
var
  q: TGMSqlQuery;
  res: string;
  glv: TGeomerLastValue;
  id_prm: int;
  v: TValueFromBase;
begin
  q := TGMSqlQuery.Create();
  q.sql.Text := 'select ID_Prm, OpcTag from Params where trim(coalesce(OpcTag, '''')) <> '''' ';
  res := '<channels>';
  try
    q.Open();
    while not q.Eof do
    begin
      id_prm := q.FieldByName('ID_Prm').AsInteger;
      glv := glvBuffer.ValByID[id_prm];

      if glv <> nil then
        v := glv.CalcLastVal()
      else
        v.Val := 0;

      res := res + Format('<chn id="%d" opctag="%s" val="%s" utime="%d"/>',
        [id_prm, TNetEncoding.HTML.Encode(q.FieldByName('OpcTag').AsString), IfThen(glv <> nil, MyFloatToStr(v.Val), '0'),
        IfThen(glv <> nil, v.UTime, 0)]);
      q.Next();
    end;
  except
    on e: Exception do
      ProgramLog().AddException('RequestOPCChannel - ' + e.Message);
  end;
  q.Free();

  res := res + '</channels>';
  res := '<?xml version="1.0" encoding="windows-1251"?>'#10#13 + FormatXMLData(res);

  bufs.XmlToBufSend(res);
end;

procedure TGeomerSocket.GMSendText(const s: string);
begin
  SendText(AnsiString(s));
end;

procedure TGeomerSocket.AddRequest(ReqDetails: TRequestDetails);
var
  nSrc: int;
  val: double;
  timeHold: UINT;
begin
  if ReqDetails.ID_DevType in GeomerFamily then
  begin
    nSrc := ReqDetails.buf[0];
    val := ReadDouble(ReqDetails.buf, 1);
    timeHold := ReadUINT(ReqDetails.buf, 9);
    Geomer_SetOutputChannel(nSrc, val, timeHold);
  end
  else
    FReqList.Add(ReqDetails);

  RequestInfo0(2);
end;

procedure TGeomerSocket.Geomer_SetOutputChannel(N_Src: int; fVal: double; TimeHold: UINT);
var
  s: string;
begin
  if (TimeHold = $FFFFFFFF) or (TimeHold = 0) then
    s := #13#10 + Format('OUT%d:%d', [N_Src, Round(fVal)]) + #13#10
  else
    s := #13#10 + Format('OUT:%d,%d,%d', [N_Src, Round(fVal), TimeHold]) + #13#10;

  try
    GMSendText(s);
  except
    on e: Exception do
      ProgramLog().AddException('SetOutputParam Sckt.SendText - ' + e.Message);
  end;
end;

procedure TGeomerSocket.SetChannelValue(var bufs: TTwoBuffers);
var
  id_prm: int;
  TimeHold: UINT;
  fVal: double;
  res: bool;
begin
  id_prm := ReadUINT(bufs.BufRec, 2);
  fVal := ReadDouble(bufs.BufRec, 6);
  TimeHold := Min(ReadUINT(bufs.BufRec, 14), 274877); // 274877 - ограничение на длину импульса

  ProgramLog.AddMessage(Format('SetOutputParam ID_Prm = %d, fVal = %s, TimeHold = %d', [id_prm, MyFloatToStr(fVal), TimeHold]));

  try
    res := ThreadsContainer.SetChannelValue(id_prm, fVal, TimeHold);
    bufs.BufSend[2] := IfThen(res, 0, 1); // 0 - ok, 1 - ошибки
    bufs.LengthSend := 3;
  except
    on e: Exception do
      ProgramLog().AddException('SetOutputParam - ' + e.Message);
  end;
end;

procedure TGeomerSocket.AnalyzeClientRequest(var bufs: TTwoBuffers);

  function CheckPrmInRequest159201(id_prm: int): bool;
  var
    nPos: WORD;
    nPrms: byte;
  begin
    Result := false;
    // 5 - это 158, 200, nPrms и хотя бы один номер объекта
    if bufs.NumberOfBytesRead < 5 then
      Exit; // посылка заведомо ошибочная

    nPrms := ReadWord(bufs.BufRec, 2);
    nPos := 4;
    // nObj - к-во параметров, который должны быть в запросе
    // несколько запросов могли склеиться, поэтому проверки на длину посылки будети недостаточно
    while (nPos <= bufs.NumberOfBytesRead - 2) and (nPos < 4 + nPrms * 4) do
    begin
      if ReadUINT(bufs.BufRec, nPos) = int64(id_prm) then // int64 удалит warning
      begin
        Result := true;
        Exit;
      end;

      inc(nPos, 4);
    end;
  end;

  procedure RequestCurrentsAllObjects();
  var
    i: int;
    nPos, nCnt: int;
    Val: TValueFromBase;
  begin
    // запрос текущих все колхозом
    nCnt := 0;
    nPos := 4;

    glvBuffer.synch.BeginRead();
    try
      for i := 0 to glvBuffer.count - 1 do
        if CheckPrmInRequest159201(glvBuffer[i].id_prm) then
        begin
          Val := glvBuffer[i].CalcLastVal();

          nPos := WriteUINT(bufs.BufSend, nPos, glvBuffer[i].id_prm);
          nPos := WriteUINT(bufs.BufSend, nPos, Val.UTime);
          nPos := WriteFloat(bufs.BufSend, nPos, Val.Val);

          inc(nCnt);
        end;
    except
    end;
    glvBuffer.synch.EndRead();

    WriteWORD(bufs.BufSend, 2, nCnt);
    bufs.LengthSend := nPos;
  end;

  procedure RequestDiagram();
  var
    qGetArch: TGMSqlQuery;
    D1, D2: LongWord;
    id_prm, nPos, nCnt, nStep, DiagramType: int;
  begin
    id_prm := ReadUINT(bufs.BufRec, 2);
    nStep := bufs.BufRec[6];
    D1 := ReadUINT(bufs.BufRec, 7);
    D2 := ReadUINT(bufs.BufRec, 11);

    if bufs.NumberOfBytesRead > 15 then
      DiagramType := bufs.BufRec[15]
    else
      DiagramType := DIAGRAMTYPE_DEFAULT;

    // запрос архивных
    qGetArch := TGMSqlQuery.Create();

    try
      try
        qGetArch.sql.Text := Format('select * from ReadParamDiagram (%d, %s, %s, %d, %d)',
          [id_prm, IntToStr(D1), IntToStr(D2), nStep, DiagramType]);
        qGetArch.Open();

        nCnt := 0;
        nPos := 8;

        while not qGetArch.Eof do
        begin
          nPos := WriteUINT(bufs.BufSend, nPos, qGetArch.FieldByName('UTime').AsInteger);
          nPos := WriteFloat(bufs.BufSend, nPos, qGetArch.FieldByName('Val').AsFloat);

          inc(nCnt);
          qGetArch.Next();
        end;

        bufs.LengthSend := nPos;
        WriteUINT(bufs.BufSend, 2, id_prm);
        WriteWORD(bufs.BufSend, 6, nCnt);
      except
        on e: Exception do
          ProgramLog().AddException('RequestDiagram - ' + e.Message);
      end;
    finally
      TryFreeAndNil(qGetArch);
    end;
  end;

  procedure WriteVal(r: TRemoteSrvRecord; ut: ULong; v: double);
  begin
    try
      ExecPLSQL(Format('perform WriteVal (%d, %d, %s);', [r.id_prm, ut, MyFloatToStr(v)]));

      r.uDTLast := ut;
      glvBuffer.AddSimpleValue(r.id_prm, ut, v);
    except
      on e: Exception do
        ProgramLog().AddException('WriteVal - ' + e.Message);
    end;
  end;

  procedure ParseRawDiagram();
  var
    r: TRemoteSrvRecord;
    n, nPos, N_Car, DevType, NDevice, PrmType, NPrm: int;
    ut: ULong;
    v: double;
  begin
    n := ReadUINT(bufs.BufRec, 23);
    r := nil;
    ut := 0;

    nPos := 27;
    while (n > 0) and (nPos + 12 <= bufs.NumberOfBytesRead) do
    begin
      ut := ReadUINT(bufs.BufRec, nPos);
      v := ReadDouble(bufs.BufRec, nPos + 4);

      N_Car := ReadUINT(bufs.BufRec, 2);
      DevType := ReadUINT(bufs.BufRec, 6);
      NDevice := ReadUINT(bufs.BufRec, 10);
      PrmType := ReadUINT(bufs.BufRec, 14);
      NPrm := ReadUINT(bufs.BufRec, 18);

      r := RmSrvRec.RecByParams(N_Car, DevType, NDevice, PrmType, NPrm);
      if r <> nil then
      begin
        WriteVal(r, ut, v);
      end;

      inc(nPos, 12);
      dec(n);
    end;

    if (ut > 0) and (r <> nil) then
      GMPostMessage(WM_DEVICE_ONLINE, r.ID_Obj, r.ID_Device);
  end;

  procedure RequestSQL();
  var
    q: TGMSqlQuery;
    len, i: int;
    nPos: int;
  begin
    len := ReadWord(bufs.BufRec, 2);
    if len > bufs.NumberOfBytesRead - 4 then
      Exit; // что-то где-то недочитано

    q := TGMSqlQuery.Create();
    try
      q.sql.Text := string(ReadString(bufs.BufRec, 4, len));
      q.Open();
      nPos := 6;

      while not q.Eof do
      begin
        for i := 0 to q.Fields.count - 1 do
        begin
          nPos := WriteString(bufs.BufSend, nPos, q.Fields[i].AsAnsiString);
          nPos := WriteByte(bufs.BufSend, nPos, 0);
        end;

        q.Next();
      end;

      WriteUINT(bufs.BufSend, 2, nPos - 6); // длина строки
      bufs.LengthSend := nPos;
    except
      on e: Exception do
        ProgramLog().AddException('RequestSQL - ' + e.Message);
    end;
    q.Free();
  end;

  procedure RequestControlRights();
  var
    q: TGMSqlQuery;
    len, i: int;
    s, Login, Password: string;
    p: TZConnectionParams;
  begin
    len := ReadWord(bufs.BufRec, 2);
    if len > bufs.NumberOfBytesRead - 4 then
      Exit; // что-то где-то недочитано

    Login := '';
    Password := '';
    s := string(ReadString(bufs.BufRec, 4, len)); // login #13 password
    i := 1;
    while (i <= Length(s)) and (s[i] <> #13) do
    begin
      Login := Login + s[i];
      inc(i);
    end;

    inc(i);
    while i <= Length(s) do
    begin
      Password := Password + s[i];
      inc(i);
    end;

    p := GlobalSQLConnectionParams();
    p.Login := Login;
    p.Password := Password;

    q := TGMSqlQuery.Create(p);

    try
      q.sql.Text := 'select 1';
      q.ExecSQL();
      bufs.BufSend[2] := 1;
    except
      bufs.BufSend[2] := 0;
    end;

    q.Free();
    bufs.LengthSend := 3;
  end;

  procedure RequestNextRemotePrm();
  var
    i: int;
  begin
    ProgramLog().AddMessage('RequestNextRemotePrm - RequestNext');
    RmSrvRec.RequestNext(bufs);

    ProgramLog().AddMessage('RequestNextRemotePrm - RequestNext Done');
    if bufs.LengthSend = 0 then
    begin
      bufs.LengthSend := 27;
      for i := 0 to bufs.LengthSend - 1 do
        bufs.BufSend[i] := 0; // 0 - запрос. В данном случае путой, чтобы не дрочить таймаут

      bufs.BufSend[0] := 231;
      bufs.BufSend[1] := 180;
    end;
  end;

  procedure RequestCurrentDT();
  begin
    bufs.LengthSend := 6;
    WriteUINT(bufs.BufSend, 2, NowGM());
  end;

  function CheckBufHeader(buf0, buf1, cnt: int): bool;
  begin
    Result := bufs.CheckBufRecHeader(buf0, buf1, cnt);
    if Result then
      FSocketObjectType := OBJ_TYPE_CLIENT; // это клиент
  end;

begin
  bufs.BufSend[0] := bufs.BufRec[0];
  bufs.BufSend[1] := bufs.BufRec[1];

  bufs.LengthSend := 0;
  try
    if CheckBufHeader(171, 26, 18) then
      SetChannelValue(bufs)
    else
      if CheckBufHeader(159, 201, 4) then
        RequestCurrentsAllObjects()
      else
        if CheckBufHeader(231, 161, 15) then
          RequestDiagram()
        else
          if CheckBufHeader(231, 180, 27) then
            ParseRawDiagram()
          else
            if CheckBufHeader(100, 101, 5) then
              RequestSQL()
            else
              if CheckBufHeader(210, 220, 5) then
                RequestControlRights()
              else
                if CheckBufHeader(231, 182, 2) then
                  RequestNextRemotePrm()
                else
                  if CheckBufHeader(100, 120, 2) then
                    RequestCurrentDT()
                  else
                    if CheckBufHeader(20, 135, 2) then
                      RequestOPCChannels(bufs)
                    else
                      if bufs.CheckBufRecHeader(255, 0, 22) then
                        RequestUniversal(bufs)
                      else
                        if bufs.CheckBufRecHeader(250, 10, 6) then
                          RemoteServerReport(bufs)
                        else
                        begin
                          if bufs.NumberOfBytesRead >= 2 then
                          begin
                            bufs.LengthSend := 2;
                            bufs.BufSend[0] := bufs.BufRec[1];
                            bufs.BufSend[1] := bufs.BufRec[0];
                          end;

                          ProgramLog().AddError('AnalyzeClientRequest: unknown request - ' + ArrayToString(bufs.BufRec,
                            bufs.NumberOfBytesRead, true));
                        end;
  except
    on e: Exception do
      ProgramLog().AddError('AnalyzeClientRequest:' + e.Message + ', ' + ArrayToString(bufs.BufRec,
        bufs.NumberOfBytesRead, true));
  end;
end;

procedure TGeomerSocket.CheckOld485Ids();
var
  i: int;
begin
  for i := 0 to high(lst485IDs) do
  begin
    if (lst485IDs[i].rqtp = rqtDummy) or (Abs(lst485IDs[i].TimeTick - GetTickCount()) > MAX_ANSWER_WAITING_TIME_MS) then
      lst485IDs[i].Init();
  end;
end;

function TGeomerSocket.TimeToRequestCurrents(): bool;
begin
  Result := Abs(FLast485BuilderUTime - NowGM()) >= COMDevicesRequestInterval;
end;

procedure TGeomerSocket.BuildReqList();
var
  builder: TRequestListBuilder;
  ID_Obj: int;
begin
  FReqList.ClearProcessed();

  if (TimeToRequestCurrents() or FReqList.HasRequestsWithPriority(rqprCommand))
  // интервал опроса текущих уже вышел или пришла команда и их надо опрашивать принудительно
    and not FReqList.HasRequestsWithPriority(rqprCurrents) then // если остались запросы с прошлого разато новых не надо
  begin
    ID_Obj := SQLReq_IdObjByNCar(N_Car, [OBJ_TYPE_GM]);
    if ID_Obj <= 0 then
      Exit;

    builder := TRequestListBuilder.Create(0, ID_Obj);
    try
      builder.Build(FReqList);
      FLast485BuilderUTime := NowGM();
    finally
      builder.Free();
    end;
  end;
end;

// cnt на случай, если запросы особо жирные и не лезут в буфер
// мы их тогда оставим до лучших времен
procedure TGeomerSocket.PackAndSend485(lst: TList<TSpecDevReqListItem>; cnt: int = 0);
var
  nPos, i: int;
  buf: array [0 .. GEOMER_485_BUFFER_SIZE - 1] of byte;
  n: int;
  ext: TPrepareRequestBufferInfo;
  reqID: int;
begin
  if lst.count = 0 then
    Exit;

  n := IfThen(cnt > 0, cnt, lst.count);
  nPos := WriteString(buf, 0, #13#10'RS485:P');
  nPos := WriteByte(buf, nPos, n);

  for i := 0 to n - 1 do
  begin
    if nPos + lst[i].ReqDetails.BufCnt >= GEOMER_485_BUFFER_SIZE - 2 then
    begin
      PackAndSend485(lst, i);
      Exit;
    end;

    nPos := WriteByte(buf, nPos, FLast485ID + i);
    nPos := WriteByte(buf, nPos, lst[i].ReqDetails.BufCnt);
  end;

  // здесь n уже точно кошерное
  for i := 0 to n - 1 do
  begin
    reqID := FLast485ID + i;

    ext.reqID := reqID;
    lst[i].PrepareBuffer(ext);
    nPos := WriteBuf(buf, nPos, lst[i].ReqDetails.buf, lst[i].ReqDetails.BufCnt);

    lst[i].Processed := true;
    lst485IDs[reqID] := lst[i].ReqDetails;
    lst485IDs[reqID].TimeTick := GetTickCount();
  end;

  ProgramLog.AddExchangeBuf('Geomer_' + IntToStr(N_Car), COM_LOG_OUT, buf, nPos);
  SendBuf(buf, nPos);
end;

procedure TGeomerSocket.IncLast485ID();
begin
  inc(FLast485ID, MAX_REQUESTS_IN_DEVICE);
  if FLast485ID > 255 then
    FLast485ID := 0;
end;

procedure TGeomerSocket.ProcessReqList();
var
  lst: TList<TSpecDevReqListItem>;
  archCount: int;
begin
  if FReqList.count = 0 then
    Exit;

  IncLast485ID();
  archCount := Max(FReqList.CountRequestsWithPriority(rqprArchives), ARCH_RESERVE);
  lst := TList<TSpecDevReqListItem>.Create();
  try
    // Не обрабатываются две ситуации:
    // 1. У нас больше MAX_REQUESTS_IN_DEVICE управляющих команд к одному Геомеру (маловероятно)
    // 2. Вторая команда поступила до того как успели выполниться команды из предыдущей пачки (тоже крайне маловероятно)
    FReqList.EnlistRequestsWithPriority(lst, rqprCommand, MAX_REQUESTS_IN_DEVICE - archCount);
    FReqList.EnlistRequestsWithPriority(lst, rqprCurrents, MAX_REQUESTS_IN_DEVICE - archCount);
    FReqList.EnlistRequestsWithPriority(lst, rqprArchives, MAX_REQUESTS_IN_DEVICE);

    PackAndSend485(lst);
  finally
    lst.Free();
  end;
end;

procedure TGeomerSocket.Process485();
begin
  CheckOld485Ids(); // зачистим устаревшие

  // продолжаем только если есть команды или все старые запросы уже отработаны
  if HasReqsInDevice() and not FReqList.HasRequestsWithPriority(rqprCommand) then
    Exit;

  BuildReqList(); // построим списки запросов, если пришла пора
  ProcessReqList(); // отправим запросы в Геомер, если есть свободные слоты
end;

procedure TGeomerSocket.CheckInfo0;
begin
  if (FInfo0Queue > 0) and (Abs(FInfo0LastReqUTime - NowGM()) > 5000) then
  begin
    GMSendText(#13#10'INFO:0'#13#10);
    FInfo0LastReqUTime := NowGM();
    dec(FInfo0Queue);
  end;
end;

procedure TGeomerSocket.LoadCommands();
begin
  TRequestSpecDevices.LoadCommands(SQLReq_IdObjByNCar(FNCar, [FSocketObjectType]), 0, 0, AddRequest);
end;

procedure TGeomerSocket.BackgroundWork;
begin
  if FSocketObjectType = OBJ_TYPE_GM then
  begin
    CheckInfo0();
    LoadCommands();
    Process485();
  end;
end;

constructor TGeomerSocket.Create(Socket: TSocket; ServerWinSocket: TServerWinSocket;
  AglvBuffer: TGeomerLastValuesBuffer);
var
  i: int;
begin
  inherited Create(Socket, ServerWinSocket);

  N_Car := -1;
  FLastTimeUpdate := 0;
  FSocketObjectType := OBJ_TYPE_UNKNOWN;
  FAncomThread := nil;
  FInfo0Queue := 0;
  FInfo0LastReqUTime := 0;
  FLast485BuilderUTime := 0;

  FReqList := TRequestCollection.Create();

  FLast485ID := 0;
  for i := 0 to high(lst485IDs) do
  begin
    lst485IDs[i].Init();
  end;

  RmSrvRec := TRemoteSrvRecords.Create();
  glvBuffer := AglvBuffer;
  FExecSQLThread := nil;
  FLastReadUTime := NowGM();
end;

function TGeomerSocket.ProcessAncomID(buf: array of byte; cnt: int): bool;
var
  BufSend: array [0 .. 20] of byte;
  i, ancomId, ID_Obj: int;
begin
  ancomId := ReadAncomID(buf, cnt);
  Result := ancomId > 0;
  if not Result then
    Exit;

  ProgramLog.AddMessage('ProcessAncomID.ID = ' + IntToStr(ancomId));

  N_Car := ancomId;
  FSocketObjectType := OBJ_TYPE_ANCOM;

  // на изменение не проверяем
  // если изменится ID, то все равно перезапустится, а значит будет новое соединение
  if FAncomThread = nil then
  begin
    ID_Obj := SQLReq_IdObjByNCar(N_Car, [OBJ_TYPE_ANCOM]);
    ProgramLog.AddMessage('ProcessAncomID.ID_Obj = ' + IntToStr(ID_Obj));
    if ID_Obj > 0 then
    begin
      FAncomThread := TRequestAncomGPRS.Create(ID_Obj);
      GMPostMessage(WM_DEVICE_ONLINE, ID_Obj, 0);
    end;
  end;

  // отзыв на пароль
  WriteZero(BufSend, 0, 20);
  for i := 1 to Length(AncomServerID) do
    BufSend[i - 1] := ord(AncomServerID[i]);

  SendBuf(BufSend, 20);
end;

function TGeomerSocket.CheckGMWithUBZBlock(buf: array of byte; cnt: int; var blockCnt: int): bool;
var
  cUBZ, goodLen: int;
begin
  Result := false;
  if (cnt >= 38) and (buf[0] = $55) and (buf[1] = $20) then
  begin
    cUBZ := ReadWORD(buf, 36);
    goodLen := GEOMER_WITH_UBZ_BLOCK_LEN + cUBZ * GEOMER_WITH_UBZ_DEVICE_LEN;
    Result := cnt >= goodLen;
    if Result then
      blockCnt := goodLen;
  end;
end;

function TGeomerSocket.DecodeGMDataBlock(buf: array of byte; cnt: int): int;
var
  gbv: TGeomerBlockValues;
  c485: int;
  bufs: TTwoBuffers;

  procedure ProcessGBV();
  begin
    N_Car := gbv.ReqDetails.N_Car;
    GMPostMessage(WM_GEOMER_BLOCK, WPARAM(gbv), 0);
    FSocketObjectType := OBJ_TYPE_GM;
    ProgramLog.AddExchangeBuf('Geomer_' + IntToStr(N_Car), COM_LOG_IN, buf, Result);
  end;

begin
  Result := cnt; // на случай сбоя будем игнорировать дальнейшую посылку
  ProgramLog.AddMessage('DecodeGMDataBlock cnt = ' + IntToStr(cnt));
  try
    bufs.LengthSend := 0;

    if CheckGMWithUBZBlock(buf, cnt, Result) then
    begin
      ProgramLog.AddMessage('ReadLongDataWithUBZ');
      gbv := TGeomerBlockValues.Create();
      gbv.ReadLongDataWithUBZ(buf); // длинный блок + УБЗ
      ProcessGBV();
    end
    else
    if (cnt >= 90) and (buf[0] = $55) and (buf[1] = $19) then
    begin
      ProgramLog.AddMessage('ReadLongDataWithISCO');
      Result := 90;
      gbv := TGeomerBlockValues.Create();
      gbv.ReadLongDataWithISCO(buf); // длинный блок + ISCO
      ProcessGBV();
    end
    else
    if (cnt >= 46) and (buf[0] = $55) and (buf[1] = $18) then
    begin
      ProgramLog.AddMessage('ReadLongData');
      Result := 46;
      gbv := TGeomerBlockValues.Create();
      gbv.ReadLongData(buf); // длинный блок
      ProcessGBV();
    end
    else
    if (cnt >= 24) and (buf[0] = $55) and (buf[1] = $17) then
    begin
      Result := 24;
      ProgramLog.AddMessage('ReadShortData');
      gbv := TGeomerBlockValues.Create();
      gbv.ReadShortData(buf); // короткий блок
      ProcessGBV();
    end
    else
    if (cnt >= 4) and (buf[0] = $55) and (buf[1] = $85) then
    begin
      ProgramLog.AddMessage('GM485');
      c485 := buf[3];
      Result := c485 + 4;

      if c485 > 0 then
      begin
        gbv := TGeomerBlockValues.Create();
        gbv.ReqDetails := lst485IDs[buf[2]];
        gbv.ReqDetails.N_Car := N_Car;
        gbv.ReqDetails.ObjType := OBJ_TYPE_GM;
        gbv.Read485(buf, cnt);

        ProcessGBV();
        ProgramLog.AddExchangeBuf('Geomer485_' + IntToStr(N_Car), COM_LOG_IN, gbv.gmBufRec, gbv.gmLenRec);
      end;

      // обнуляем в любом случае
      lst485IDs[buf[2]].Init();
    end
    else
    if ProcessAncomID(buf, cnt) then
    begin
      Result := cnt;
    end
    else
    begin
      ProgramLog.AddMessage('AnalyzeClientRequest');
      WriteBuf(bufs.BufRec, 0, buf, cnt);
      bufs.NumberOfBytesRead := cnt;
      AnalyzeClientRequest(bufs);

      if bufs.LengthSend > 0 then
        try
          ProgramLog.AddMessage('DecodeGMDataBlock - AnalyzeClientRequest - SendBuf Len = ' +
            IntToStr(bufs.LengthSend));
          SendBuf(bufs.BufSend, bufs.LengthSend);
        except
          on e: Exception do
            ProgramLog.AddException('DecodeGMDataBlock - SendBuf: ' + e.Message);
        end;

      // Result ничему не приравниваем запрос данных от клиента занимает всю посылку
    end;
  except
    // Result ничему не приравниваем, сбой произошел до определения длины посылки, дальнейшее содержимое буфера не анализируем
    on e: Exception do
      ProgramLog.AddException('DecodeGMDataBlock: ' + e.Message);
  end;
end;

function TGeomerSocket.HasReqsInDevice(): bool;
var
  i: int;
begin
  Result := false;
  for i := FLast485ID to FLast485ID + MAX_REQUESTS_IN_DEVICE - 1 do
  begin
    if lst485IDs[i].rqtp <> rqtNone then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

procedure TGeomerSocket.ReadAndParseDataBlock_ProcessBuf(var buf: ArrayOfByte);
var
  cnt, i: int;
  iProcessed: int;
  action: string;
begin
  cnt := Length(buf);
  repeat
    action := 'DecodeGMDataBlock cnt = ' + IntToStr(cnt);
    iProcessed := DecodeGMDataBlock(buf, cnt);
    ProgramLog.AddMessage('ReadAndParseDataBlock_DecodeGMDataBlock iProcessed = ' + IntToStr(iProcessed));
    if (iProcessed > cnt) or (iProcessed <= 0) then
      break;

    action := 'CopyMemory cnt = ' + IntToStr(cnt) + ' iProcessed = ' + IntToStr(iProcessed);
    ProgramLog.AddMessage(action);
    for i := 0 to cnt - iProcessed - 1 do
      buf[i] := buf[i + iProcessed];
    cnt := cnt - iProcessed;
  until (cnt <= 0) or (iProcessed <= 0);
end;

procedure TGeomerSocket.UpdateLastReadTime();
begin
  FLastReadUTime := NowGM();
end;

procedure TGeomerSocket.ReadAndParseDataBlock();
var
  cnt: int;
  buf: ArrayOfByte;
  conn: TConnectionObjectTCP_IncomingSocket;
  action: string;
begin
  ProgramLog.AddMessage('ReadAndParseDataBlock started');
  try
    action := 'CreateConn';
    conn := TConnectionObjectTCP_IncomingSocket.Create();
    try
      conn.Socket := Self;
      action := 'ExchangeBlockData';
      if conn.ExchangeBlockData(etRec) <> ccrBytes then
        Exit;

      UpdateLastReadTime();
      action := 'SetLength';
      cnt := conn.buffers.NumberOfBytesRead;
      SetLength(buf, cnt);
      action := 'SetBuf';
      WriteBuf(buf, 0, conn.buffers.BufRec, cnt);
    finally
      conn.Free();
    end;

    ReadAndParseDataBlock_ProcessBuf(buf);
  except
    on e: Exception do
      raise Exception.Create(action + ' - ' + e.Message);
  end;
end;

destructor TGeomerSocket.Destroy;
begin
  TryFreeAndNil(RmSrvRec);
  TryFreeAndNil(FAncomThread);
  TryFreeAndNil(FExecSQLThread);
  TryFreeAndNil(FReqList);

  inherited Destroy;
end;

procedure TGeomerSocket.Disconnect(Socket: TSocket);
begin
  inherited Disconnect(Socket);
end;

procedure TGeomerSocket.SendCurrentTime();
var
  s: string;
begin
  // обновим время для Геомеров каждые 12 часов
  if ((FSocketObjectType = OBJ_TYPE_UNKNOWN) or ((FSocketObjectType = OBJ_TYPE_GM) and (N_Car > 0))) and
    (Abs(FLastTimeUpdate - Now()) > 12 * OneHour) then
    try
      s := #13#10'TIME:' + FormatDateTime('ddmmyy,hhnnss', NowUTC()) + #13#10;
      GMSendText(s);
      FLastTimeUpdate := Now();
    except
      on e: Exception do
        ProgramLog().AddException('GMSocket.SendCurrentTime - ' + e.Message);
    end;
end;

procedure TGeomerSocket.SetNCar(const Value: int);
var
  i: int;
begin
  if FNCar = Value then
    Exit;

  for i := 0 to high(arrDI) do
    arrDI[i] := true;

  for i := 0 to high(arrDIC) do
    arrDIC[i] := true;

  for i := 0 to high(arrAI) do
    arrAI[i] := true;

  FNCar := Value;

  lstSockets.CheckSocketOldNCars(Self);
end;

{ TSocketList }

procedure TSocketList.CheckSocketOldNCars(Sckt: TGeomerSocket);
var
  i: int;
begin
  if Sckt.N_Car <= 0 then
    Exit;

  FSynch.BeginWrite();
  for i := Count - 1 downto 0 do
  begin
    if (Sockets[i] <> Sckt) and (Sockets[i].N_Car = Sckt.N_Car) and (Sockets[i].SocketObjectType = Sckt.SocketObjectType)
    then
    begin
      Sockets[i].Free();
    end;
  end;
  FSynch.EndWrite();
end;

constructor TSocketList.Create(ASocket: TServerWinSocket);
begin
  inherited Create();

  FSynch := TMultiReadExclusiveWriteSynchronizer.Create();
  FSocket := ASocket;
end;

destructor TSocketList.Destroy;
begin
  FSynch.Free();

  inherited;
end;

function TSocketList.GetCout: int;
begin
  Result := FSocket.ActiveConnections;
end;

function TSocketList.GetSocket(i: int): TGeomerSocket;
begin
  Result := TGeomerSocket(FSocket.Connections[i]);
end;

function TSocketList.SocketByIdObj(ID_Obj: int): TGeomerSocket;
var
  ncar: int;
begin
  Result := nil;

  ncar := SQLReq_NCarByIdObj(ID_Obj);
  if ncar > 0 then
    Result := SocketByNCar(ncar, SQLReq_ObjType(ID_Obj));
end;

function TSocketList.SocketByNCar(N_Car, ObjType: int): TGeomerSocket;
var
  i: int;
begin
  Result := nil;
  // отсечем негодные типы объектов
  if (N_Car <= 0) or not(ObjType in [OBJ_TYPE_GM, OBJ_TYPE_ANCOM]) then
    Exit;

  FSynch.BeginRead();
  try
    for i := Count - 1 downto 0 do
    begin
      if (Sockets[i].N_Car = N_Car) and (Sockets[i].SocketObjectType = ObjType) then
      begin
        Result := Sockets[i];
        break;
      end;
    end;
  finally
    FSynch.EndRead();
  end;
end;

{ TGeomerSocketSQLExecuteThread }

procedure TGeomerSocketSQLExecuteThread.AddResponceXml(const xml: string);
begin
  Synch.BeginWrite();
  try
    FXMLList.Add(xml);
  finally
    Synch.EndWrite();
  end;
end;

constructor TGeomerSocketSQLExecuteThread.Create;
begin
  inherited Create();
  FXMLList := TStringList.Create();
end;

destructor TGeomerSocketSQLExecuteThread.Destroy;
begin
  inherited;
  FXMLList.Free();
end;

function TGeomerSocketSQLExecuteThread.GetCount: int;
begin
  Result := inherited GetCount() + FXMLList.Count;
end;

procedure TGeomerSocketSQLExecuteThread.SafeExecute_ExtraStep;
var report: IXMLGMIOResponceType;
    sql: TStringList;
begin
  CoInitialize(nil);
  while FXMLList.Count > 0 do
  begin
    sql := TStringList.Create();
    try
      Synch.BeginRead();
      try
        report := LoadXMLData_GMIOResponce(FXMLList[0]);
      finally
        Synch.EndRead();
      end;

      report.Objects.SQL_CreateOrUpdateObjects(sql, report.Auth.RemoteName);
      report.Data.SQL_InsertData(sql, report.Auth.RemoteName);
      AddList(sql);

      Synch.BeginWrite();
      try
        FXMLList.Delete(0);
      finally
        Synch.EndWrite();
      end;
    finally
      sql.Free();
    end;
  end;
end;

initialization

finalization

TryFreeAndNil(lstSockets);

end.
