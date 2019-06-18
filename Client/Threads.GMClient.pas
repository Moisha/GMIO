////////////////////////////////////////////
// ѕоток работы с сервером
////////////////////////////////////////////
unit Threads.GMClient;

interface

uses Windows, Classes, GMGlobals, ScktComp, INIFiles, SysUtils,
     Forms, Messages, Math, GMDBClasses, GMConst, ConfigXML, DateUtils,
     StdResponce, StdRequest, GMGenerics, XMLDoc, ConnParamsStorage,
     Generics.Collections, Connection.Base, Connection.TCP, Threads.Base;

type
  TAlarmType = (atpCommon, atpBarThresholdUp, atpBarThresholdDown, atpBarHisteresis);
  TAlarmState = (asUnknown, asNotSet, asSetTo1, as1DropTo0, as1AndQuoted);

  TGMAlarm = class (TCollectionItem)
  private
    FalCurrState: TAlarmState;
    FfBarTreshold: double;
    procedure SetAlCurrState(const Value: TAlarmState);
    procedure SaveToLogFile;
    function GetID_Prm: int;
    function GetAlarmSignal: int;
  public
    prm: TGMBaseParam;

    Enabled: bool;
    AlarmType: TAlarmType;

    AlarmLevel: int;  // предупреждение / опасность
    fBarHisteresis: double;

    utLast1, utLast0: LongWORD;

    property AlarmSignal: int read GetAlarmSignal; // сигнал, на который срабатывать: 0 (NC) или 1 (NO)
    property fBarTreshold: double read FfBarTreshold write FfBarTreshold;
    property ID_Prm: int read GetID_Prm;
    property alCurrState: TAlarmState read FalCurrState write SetAlCurrState;
    constructor Create(Collection: TCollection); override;
    procedure CheckState(ut: LongWord; Val: double);
    procedure Quote();
    function GetAlarmTxt(): string;
  end;

  TAlarmsCollection = class(TCollection)
  private
    function GetAlarm(i: int): TGMAlarm;
    function GetAlarmByID(ID_Prm: int): TGMAlarm;
  public
    constructor Create();
    function Add(): TGMAlarm;
    property Alarms[i: int]: TGMAlarm read GetAlarm; default;
    property AlarmByID[ID_Prm: int]: TGMAlarm read GetAlarmByID;
    procedure RefreshAlarms();
  end;

  TDataOrderState = (dosUnknown, dosWaiting, dosProcessing, dosDone, dosError, dosDelete);
  TDataOrderType = (dotXLS, dotControl, dotSQL, dotRequestControl, dotDiagram, dotServerDateTime, dotUniversal, dotUnknown);
  TGMDiagramType = DIAGRAMTYPE_DEFAULT..DIAGRAMTYPE_TOTAL;

  TDataOrder = class
  private
    function ParseDiagram(buf: array of byte; len: int): bool;
    function ParseOneByteResult(buf: array of byte; len: int): bool;
    function ParseSQL(buf: array of byte; len: int): bool;
    function ParseUBZDataBlock(buf: array of byte; len: int): bool;
    function ParseUniversalResponce(buf: array of byte; len: int): bool;
    procedure ProcessResponce;
    procedure ProcessDiagrams;
    procedure CreateUniversalRequest(var buffer: TTwoBuffers);
    procedure SetReqAuth;
  protected
    function GetLogin: string; virtual;
    function GetPassword: string; virtual;
  public
    ID_Obj: int;
    prm: TGMBaseParam;
    UD1, UD2, TimeHold: LongWord;
    nTimeStep: int;
    DiagramType: TGMDiagramType;
    DataType: int;
    AggrType: int;
    fVal: double;
    bNotify: bool;

    ParentHandle: HWND;
    lDiagram: array of TValueFromBase;
    State: TDataOrderState;
    OrderType: TDataOrderType;
    iRetryCount: int;
    SQL: string;
    slSQLRes: TstringList;

    req: IXMLGMIORequestType;
    resp: IXMLGMIOResponceType;

    constructor Create();
    function AddDiagramRequestDataSource(prm: TGMBaseParam; UD1, UD2: LongWord; nStep: int): IXMLSourceType;
    procedure CreateRequest(var buffer: TTwoBuffers);
    function WaitFor(TimeOutMs: int): bool;
    destructor Destroy; override;
    function ParseResult(buf: array of byte; len: int): bool;
    procedure ProcessCurrentsPack();
    procedure Exec();
    function CommandOrderError: string;
    procedure MarkAsDone();
  end;

  TDataOrders = class (TGMCollection<TDataOrder>)
  strict private
    class var FInstance: TDataOrders;
  strict private
    function DoAdd(): TDataOrder;
    function DoAddSendCommandOrder(prm: TGMParam; value: double): TDataOrder;
    function DoAddDiagram(prm: TGMBaseParam; UD1, UD2: LongWord; nStep: int; Handle: HWnd): TDataOrder;
    procedure DoDeleteCompleted();
    function DoGetNext(): TDataOrder;
  private
    class procedure CheckServerDateTime;
  public
    class function Add(): TDataOrder; reintroduce;
    class function AddSendCommandOrder(prm: TGMParam; value: double): TDataOrder;
    class function AddDiagram(prm: TGMBaseParam; UD1, UD2: LongWord; nStep: int; Handle: HWnd): TDataOrder;
    class procedure DeleteCompleted();
    class function GetNext(): TDataOrder;
    class function QueueForHandle(Handle: HWnd): int;
    class procedure CancelAll();

    class function GetInstance(): TDataOrders;
  end;

  TConnectionObjectGMClient = class(TConnectionObjectTCP_OwnSocket)
  protected
    function InternalCheckGetAllData: bool; override;
  end;

  TGMCOMThread = class(TGMThread)
  strict private
    class var dtServerTimeDiffLastUpdate: TDateTime;
    procedure ProcessNextOrder;
    function ParseServerDT(order: TDataOrder): bool;
    procedure CheckServerDateTime;
    procedure ProcessOrders(t: int64);
  protected
    FConnectionObject: TConnectionObjectGMClient;
    reqPrms: TGMDataChannelList;

    function DecodeBufRec(order: TDataOrder): bool; virtual;
    procedure ReadINI(); virtual;
    procedure ExtReadINI(); virtual;
    procedure RequestCurrents; virtual;
    procedure SafeExecute; override;
    function CreateConnectionObject(): TConnectionObjectGMClient; virtual;
  public
    constructor Create();
    destructor Destroy; override;
  end;

  TGMCOMThreadPool = class(TList<TGMCOMThread>)
  strict private
    const GMCOMThreadPool_MaxThreads = 5;
    class var FInstance: TGMCOMThreadPool;
    class var FServerTimeDiffSec: int; // –азница по времени в секундах между клинетом и сервером.
                                       // Ѕольше нол€, если на сервере врем€ впереди
    procedure DoCreateThreads();
    procedure DoDestroyThreads();
    class function GetInstance(): TGMCOMThreadPool;
    constructor Create();
  private
    class procedure SetServerTimeDiffSec(const Value: int); static;
  public
    class procedure CreateThreads();
    class procedure DestroyThreads();
    class property ServerTimeDiffSec: int read FServerTimeDiffSec write SetServerTimeDiffSec;
    class procedure AddReqCurrentsPrm(prm: TGMBaseParam);
  end;

var
  acAlarms: TAlarmsCollection;
  DataThreadSynch: TMultiReadExclusiveWriteSynchronizer;

implementation

uses ActiveX, ProgramLogFile;

const iReqPause = 10;

{ TGMCOMThread }

function TGMCOMThread.CreateConnectionObject(): TConnectionObjectGMClient;
begin
  Result := TConnectionObjectGMClient.Create();
  Result.CheckTerminated := GetTerminated;
end;

constructor TGMCOMThread.Create;
begin
  inherited Create();

  dtServerTimeDiffLastUpdate := 0;

  if reqPrms = nil then
    reqPrms := TGMDataChannelList.Create();

  FConnectionObject := CreateConnectionObject();
  ReadINI();
end;

function TGMCOMThread.ParseServerDT(order: TDataOrder): bool;
var ut: UINT;
begin
  Result := false;
  if order = nil then Exit;

  ut := ReadUINT(FConnectionObject.buffers.BufRec, 2); // врем€ сервера

  TGMCOMThreadPool.ServerTimeDiffSec := Int64(ut) - NowGM();
  dtServerTimeDiffLastUpdate := Now();

  order.State := dosDelete;
  Result := true;
end;

function TGMCOMThread.DecodeBufRec(order: TDataOrder): bool;
begin
  Result := false;

  if FConnectionObject = nil then Exit;

  if (FConnectionObject.buffers.BufRec[0] = 100) and (FConnectionObject.buffers.BufRec[1] = 120) and (FConnectionObject.buffers.NumberOfBytesRead >= 6) then
    Result := ParseServerDT(order)
  else
  if order <> nil then
    Result := order.ParseResult(FConnectionObject.buffers.BufRec, FConnectionObject.buffers.NumberOfBytesRead);

  if not Result then
  begin
    // видимо битый блок от не пойми чего. ѕопробуем дочитать, может там еще чего есть
    while FConnectionObject.ExchangeBlockData(etRec) = ccrBytes do
      Sleep(100);
  end;
end;

destructor TGMCOMThread.Destroy;
begin
  reqPrms.Free();
  FConnectionObject.Free();

  inherited Destroy;
end;

procedure TGMCOMThread.ExtReadINI;
begin

end;

procedure TGMCOMThread.RequestCurrents();
var i: int;
    order: TDataOrder;
    src: IXMLSourceType;
begin
  if reqPrms.Count = 0 then Exit;

  try
    order := TDataOrder.Create();
    order.OrderType := dotUniversal;

    for i := 0 to reqPrms.Count - 1 do
    begin
      src := order.req.Data.Sources.Add();
      src.Id := reqPrms[i].ID_Prm;
      src.ValSrcType := reqPrms[i].DataChannelType;
      src.ValType := DATATYPE_CURRENTS;
    end;

    order.CreateRequest(FConnectionObject.buffers);

    if (FConnectionObject.ExchangeBlockData(etSenRec) = ccrBytes) and DecodeBufRec(order) then
      order.ProcessCurrentsPack();

    order.Free();
  except
    on e: Exception do
      ProgramLog.AddException(ClassName() + '.RequestCurrents: ' + e.Message);
  end;
end;

procedure TGMCOMThread.ProcessNextOrder();
var
  order: TDataOrder;
  res: TCheckCOMResult;
begin
  TDataOrders.DeleteCompleted();

  order := TDataOrders.GetNext();
  if order <> nil then
  begin
    order.CreateRequest(FConnectionObject.buffers);
    res := FConnectionObject.ExchangeBlockData(etSenRec);
    if (res = ccrBytes) and DecodeBufRec(order) then
    begin
      order.MarkAsDone();
    end
    else
    begin
      dec(order.iRetryCount);
      if order.iRetryCount <= 0 then
        order.State := dosError
      else
        order.State := dosWaiting;
    end;
  end;
end;

procedure TGMCOMThread.CheckServerDateTime();
begin
  if Abs(Now() - dtServerTimeDiffLastUpdate) > 30 * OneMinute then
    TDataOrders.CheckServerDateTime();
end;

procedure TGMCOMThread.ProcessOrders(t: int64);
var
  nPrmsCount: int;
begin
  nPrmsCount := reqPrms.Count;

  repeat
    ProcessNextOrder();

    // если добавили параметров на пересчет, то идем переопрашивать текущие
    if nPrmsCount <> reqPrms.Count then break;

    Sleep(50);
  until (Abs(GetTickCount() - t) > iReqPause * 1000) or Terminated;
end;

procedure TGMCOMThread.SafeExecute;
var
  t: int64;
begin
  CoInitialize(nil);
  while not Terminated do
  begin
    t := GetTickCount();
    try
      CheckServerDateTime();
      if Terminated then Exit;

      RequestCurrents();
      if Terminated then Exit;

      ProcessOrders(t);
    except
      on e: Exception do
      begin
        ProgramLog.AddException('TGMCOMThread.SafeExecute.' + e.Message);
        GMPostMessage(WM_THREAD_EXCEPTION, WParam(self), 0);
      end;
    end;
  end;
end;

procedure TGMCOMThread.ReadINI();
var xml: IGMConfigConfigType;
    prm: TZConnectionParams;
begin
  // клиентский поток, на входе - xml
  xml := ReadConfigXML();

  FConnectionObject.Host := xml.Common.Host;
  FConnectionObject.Port := xml.Common.Port;
  if xml.Common.Timeout > 0 then
    FConnectionObject.WaitFirst := xml.Common.Timeout;
  if xml.Common.Timeout2 > 0 then
    FConnectionObject.WaitNext := xml.Common.Timeout2;

  prm.Host := xml.Common.Host;
  prm.Port := xml.Common.Port;
  prm.Login := xml.Common.Login;
  prm.Password := xml.Common.Password;
  SetGlobalSQLConnectionParams(prm);

  ExtReadINI();
end;

{ TGMAlarm }

procedure TGMAlarm.SaveToLogFile();
var fn, s: string;
    f: TextFile;
begin
  s := '';
  case alCurrState of
    asSetTo1: s := '+';
    asNotSet, as1DropTo0: s := '-';
    as1AndQuoted: s := 'V';
    else Exit;
  end;

  s := s + FormatDateTime(#9'yyyy-mm-dd hh:nn:ss'#9, Now()) + GetAlarmTxt();

  fn := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'GMAlarms.csv';

  try
    AssignFile(f, fn);
    if FileExists(fn) then
      Append(f)
    else
      Rewrite(f);

    Writeln(f, s);
    CloseFile(f);
  except end;
end;

//asUnknown, asNotSet, asSetTo1, as1DropTo0, as1AndQuoted
procedure TGMAlarm.CheckState(ut: LongWord; Val: double);
var bTrigger: bool;
begin
  if ut=0 then Exit;
  
  bTrigger:=false;

  if Enabled then
  begin
    case AlarmType of
      atpBarThresholdUp: bTrigger := (Val > fBarTreshold);
      atpBarThresholdDown: bTrigger := (Val < fBarTreshold);
      atpBarHisteresis: bTrigger:=(Abs(Val-fBarTreshold)>fBarHisteresis);
      atpCommon: bTrigger:=(Round(Val)=AlarmSignal);
    end;
  end;

  if bTrigger then
  case alCurrState of
    asUnknown, asNotSet:
      begin
        alCurrState:=asSetTo1;
        utLast1:=ut;
        SaveToLogFile();
      end;
    as1DropTo0:
      begin
        alCurrState:=asSetTo1;
        utLast1:=0;  // дату начала держим старую, дату окончани€ стираем
        SaveToLogFile(); 
      end;
  end
  else
  case alCurrState of
    as1AndQuoted:
      begin
        alCurrState:=asNotSet;
        utLast1:=0;
        utLast0:=0;
        SaveToLogFile();
      end;
    asSetTo1:
      begin
        alCurrState:=as1DropTo0;
        utLast0:=ut;
        SaveToLogFile();
      end;
    asUnknown: alCurrState:=asNotSet;
  end;

  GMPostMessage(WM_ALARM_STATE_CHANGED, WParam(self), 0);
end;

procedure TGMAlarm.Quote();
begin
  case alCurrState of
    as1DropTo0:
      begin
        alCurrState:=asNotSet;
        utLast1:=0;
        utLast0:=0;
      end;
    asSetTo1: alCurrState:=as1AndQuoted;
  end;

  SaveToLogFile();
  GMPostMessage(WM_ALARM_STATE_CHANGED, WParam(self), 0);
end;

function TGMAlarm.GetAlarmTxt: string;
var p: TGMParam;
begin
  Result := '';
  p := GMParams.ByID(ID_Prm);
  if p <> nil then
  begin
    Result := p.GetNameWithPath([ppcObj, ppcDev]) + ' ';

    case AlarmType of
      atpBarThresholdUp, atpBarThresholdDown: Result := Result + 'выход уровн€ за предел';
      atpBarHisteresis: Result := Result + 'выход уровн€ из диапазона';
    end;
  end;
end;

constructor TGMAlarm.Create(Collection: TCollection);
begin
  inherited;

  prm := nil;
  alCurrState := asUnknown;
  Enabled := true;

  utLast1 := 0;
  utLast0 := 0;

  AlarmLevel := 0;
  AlarmType := atpCommon;

  fBarHisteresis := 1000;
end;

function TGMAlarm.GetID_Prm: int;
begin
  if prm <> nil then
    Result := prm.ID_Prm
  else
    Result := 0;
end;

function TGMAlarm.GetAlarmSignal: int;
begin
  // в базе 1 - NC, 2 - NO, 0 - не задан
  case prm.AlarmSignal of
    1: Result := 0;
    2: Result := 1;
    else Result := INCORRECT_VALUE;
  end;
end;

{ TAlarmsCollection }

function TAlarmsCollection.Add: TGMAlarm;
begin
  Result:=TGMAlarm(inherited Add());
end;

procedure TAlarmsCollection.RefreshAlarms();
var i: int;
    ut: LongWORD;
    Val: double;
begin
  for i:=0 to Count-1 do
  begin
    DataThreadSynch.BeginRead();
    try
      ut := Alarms[i].prm.LastVal.UTime;
      val := Alarms[i].prm.LastVal.Val;
    finally
      DataThreadSynch.EndRead();
    end;

    Alarms[i].CheckState(ut, val);
  end;
end;

constructor TAlarmsCollection.Create;
begin
  inherited Create(TGMAlarm);
end;

function TAlarmsCollection.GetAlarm(i: int): TGMAlarm;
begin
  Result:=TGMAlarm(Items[i]);
end;

function TAlarmsCollection.GetAlarmByID(ID_Prm: int): TGMAlarm;
var i: int;
begin
  Result:=nil;

  for i:=0 to Count - 1 do
    if Alarms[i].ID_Prm = ID_Prm then
    begin
      Result:=Alarms[i];
      Exit;
    end;
end;

{ TDataOrderCollection }

class procedure TDataOrders.CancelAll;
var
  i: int;
begin
  for i := 0 to GetInstance().Count - 1 do
    GetInstance()[i].State := dosError;
end;

class procedure TDataOrders.CheckServerDateTime();
var
  i: int;
  order: TDataOrder;
begin
  DataThreadSynch.BeginWrite();
  try
    for i := 0 to GetInstance().Count - 1 do
      if (GetInstance()[i].OrderType = dotServerDateTime) and (GetInstance()[i].State = dosWaiting) then
        Exit;

    order := TDataOrders.Add();
    order.OrderType := dotServerDateTime;
    order.State := dosWaiting;
  finally
    DataThreadSynch.EndWrite();
  end;
end;


class function TDataOrders.Add: TDataOrder;
begin
  Result := GetInstance().DoAdd();
end;

class function TDataOrders.AddDiagram(prm: TGMBaseParam; UD1, UD2: LongWord; nStep: int; Handle: HWnd): TDataOrder;
begin
  Result := GetInstance().DoAddDiagram(prm, UD1, UD2, nStep, Handle);
end;

class function TDataOrders.AddSendCommandOrder(prm: TGMParam; value: double): TDataOrder;
begin
  Result := GetInstance().DoAddSendCommandOrder(prm, value);
end;

class procedure TDataOrders.DeleteCompleted;
begin
  GetInstance().DoDeleteCompleted();
end;

function TDataOrders.DoAdd: TDataOrder;
begin
  Result := nil;
  DataThreadSynch.BeginWrite();
  try
    Result := inherited Add();
  except end;
  DataThreadSynch.EndWrite();
end;

function TDataOrders.DoAddDiagram(prm: TGMBaseParam; UD1, UD2: LongWord; nStep: int; Handle: HWnd): TDataOrder;
begin
  DataThreadSynch.BeginWrite();
  try
    Result := inherited Add();
    Result.ParentHandle := Handle;
    Result.OrderType := dotUniversal;
    if prm <> nil then
    begin
      Result.AddDiagramRequestDataSource(prm, UD1, UD2, nStep);
      Result.State := dosWaiting;
    end;
  finally
    DataThreadSynch.EndWrite();
  end;
end;

function TDataOrders.DoAddSendCommandOrder(prm: TGMParam; value: double): TDataOrder;
begin
  DataThreadSynch.BeginWrite();
  try
    Result := inherited Add();
    Result.prm := prm;
    Result.fVal := value;
    Result.OrderType := dotControl;
  finally
    DataThreadSynch.EndWrite();
  end;
end;

procedure TDataOrders.DoDeleteCompleted;
var
  i: int;
begin
  DataThreadSynch.BeginWrite();
  try
    for i := Count - 1 downto 0 do
      if Items[i].State = dosDelete then
        Delete(i);
  finally
    DataThreadSynch.EndWrite();
  end;
end;

class function TDataOrders.GetInstance: TDataOrders;
begin
  if FInstance = nil then
    FInstance := TDataOrders.Create();

  Result := FInstance;
end;

class function TDataOrders.GetNext: TDataOrder;
begin
  Result := GetInstance().DoGetNext();
end;

class function TDataOrders.QueueForHandle(Handle: HWnd): int;
var
  i: int;
begin
  Result := 0;
  DataThreadSynch.BeginRead();
  try
    for i := 0 to GetInstance().Count - 1 do
      if GetInstance()[i].bNotify and (GetInstance()[i].ParentHandle = Handle) and (GetInstance()[i].State in [dosWaiting, dosProcessing]) then
        inc(Result);
  finally
    DataThreadSynch.EndRead();
  end;
end;

function TDataOrders.DoGetNext: TDataOrder;
var
  i: int;
begin
  Result := nil;
  DataThreadSynch.BeginWrite();
  try
    for i := Count - 1 downto 0 do
    begin
      if Items[i].State = dosWaiting then
      begin
        Result := Items[i];

        if Result.OrderType in [dotControl, dotXLS] then // первоочередные запросы
          break;
      end;
    end;

    if Result <> nil then
      Result.State := dosProcessing;
  finally
    DataThreadSynch.EndWrite();
  end;
end;

{ TDataOrder }

function TDataOrder.AddDiagramRequestDataSource(prm: TGMBaseParam; UD1, UD2: LongWord; nStep: int): IXMLSourceType;
begin
  if prm = nil then
    Exit;

  Result := req.Data.Sources.Add();
  Result.Id := prm.ID_Prm;
  Result.ValSrcType := prm.DataChannelType;
  Result.ValType := DATATYPE_CURRENT_DIAGRAM;
  Result.DiagramType := DIAGRAMTYPE_DEFAULT;
  Result.AggrIntervalLen := nStep;
  Result.UTime1 := UD1;
  Result.UTime2 := UD2;
end;

function TDataOrder.CommandOrderError: string;
begin
  Result := '';
  if (State = dosDone) and (Length(lDiagram) > 0) then
  begin
    if lDiagram[0].Val <> 0 then
      Result := 'Ќет св€зи с прибором!';
  end
  else
  begin
    Result := 'ќшибка св€зи с сервером!';
  end;
end;

constructor TDataOrder.Create();
begin
  inherited;
  State := dosUnknown;
  OrderType := dotUnknown;
  slSQLRes := TStringList.Create();
  ParentHandle := 0;
  DiagramType := DIAGRAMTYPE_DEFAULT;
  DataType := DATATYPE_CURRENT_DIAGRAM;
  AggrType := DATA_AGGREGATION_TYPE_DEFAULT;
  req := NewGMIORequest();
  prm := nil;
  iRetryCount := 2;
  bNotify := true;
end;

procedure TGMAlarm.SetAlCurrState(const Value: TAlarmState);
begin
  if FalCurrState <> Value then
  begin
    FalCurrState := Value;
    GMPostMessage(WM_ALARM_STATE_CHANGED, WParam(self), 0);
  end;
end;

function TDataOrder.GetLogin(): string;
begin
  Result := GlobalSQLConnectionParams().Login;
  if Result = '' then Result := AUTH_DEFAULT_LOGIN;
end;

function TDataOrder.GetPassword: string;
begin
  Result := GlobalSQLConnectionParams().Password;
  if req.Auth.Password = '' then
    req.Auth.Password := CalcMD5(AUTH_DEFAULT_PASSWORD);
end;

procedure TDataOrder.MarkAsDone;
begin
  if bNotify then
  begin
    State := dosDone;
    if ParentHandle > 0 then
      PostMessage(ParentHandle, WM_ORDER_READY, WParam(self), 0);
  end
  else
  begin
    State := dosDelete;
  end;
end;

procedure TDataOrder.SetReqAuth();
begin
  req.Auth.Login := GetLogin();
  req.Auth.Password := GetPassword();
end;

procedure TDataOrder.CreateUniversalRequest(var buffer: TTwoBuffers);
begin
  SetReqAuth();
  buffer.RequestUniversal(req.XML, GetLogin(), true, 255, 0);
end;

procedure TDataOrder.CreateRequest(var buffer: TTwoBuffers);
var nPos: int;
    src: IXMLSourceType;
begin
  case OrderType of
  dotControl:
    begin
      // управление
      buffer.BufSend[0] := 171;
      buffer.BufSend[1] := 26;

      nPos := WriteUINT(buffer.BufSend, 2, prm.ID_Prm);
      nPos := WriteFloat(buffer.BufSend, nPos, fVal);
      nPos := WriteUINT(buffer.BufSend, nPos, TimeHold);
      buffer.LengthSend := nPos;
    end;
  dotSQL:
    begin
      // SQL-запрос
      buffer.BufSend[0] := 100;
      buffer.BufSend[1] := 101;

      nPos := WriteWORD(buffer.BufSend, 2, Length(SQL));
      nPos := WriteString(buffer.BufSend, nPos, SQL);
      buffer.LengthSend := nPos;
    end;
  dotRequestControl:
    begin
      // ѕроверка доступа к управлению, в строке SQL - логин и пароль
      buffer.BufSend[0] := 210;
      buffer.BufSend[1] := 220;

      nPos := WriteWORD(buffer.BufSend, 2, Length(SQL));
      nPos := WriteString(buffer.BufSend, nPos, SQL);
      buffer.LengthSend := nPos;
    end;
  dotDiagram, dotXLS:
    begin
      // график
      src := req.Data.Sources.Add();
      src.Id := prm.ID_Prm;
      src.ValSrcType := prm.DataChannelType;
      src.ValType := DataType;
      src.DiagramType := DiagramType;
      src.AggrType := AggrType;
      src.AggrIntervalLen := nTimeStep;
      src.UTime1 := UD1;
      src.UTime2 := UD2;

      CreateUniversalRequest(buffer);
    end;
  dotServerDateTime:
    begin
      buffer.BufSend[0] := 100;
      buffer.BufSend[1] := 120;
      buffer.LengthSend := 2;
    end;
  dotUniversal:
    begin
      if FindCmdLineSwitch('logExchange') then
        SaveStringToFile(FormatXMLData(req.XML), IntToStr(GetTickCount()) + '_out.xml');

      CreateUniversalRequest(buffer);
    end;
  end;
end;

destructor TDataOrder.Destroy;
begin
  slSQLRes.Free();
  req := nil;
  resp := nil;

  inherited;
end;

procedure TDataOrder.Exec;
begin
  State := dosWaiting;
end;

function TDataOrder.ParseSQL(buf: array of byte; len: int): bool;
var strLen: int;
    nPos: int;
    s: AnsiString;
begin
  Result := false;
  strLen := ReadUINT(buf, 2);
  if len < strLen + 6 then Exit; // результаты запроса недочитаны

  nPos := 6;
  s := '';
  while nPos < len do
  begin
    if buf[nPos] > 0 then
      s := s + AnsiChar(buf[nPos])
    else
    begin
      slSQLRes.Add(string(s));
      s := '';
    end;

    inc(nPos);
  end;

  Result := true;
end;

function TDataOrder.ParseDiagram(buf: array of byte; len: int): bool;
var i, N_Vals, ID_Prm, nPos: int;
begin
  Result := false;
  ID_Prm := ReadUINT(buf, 2);
  if ID_Prm <> ID_Prm then Exit;

  N_Vals := ReadWORD(buf, 6);
  SetLength(lDiagram, N_Vals);
  for i := 0 to N_Vals - 1 do
    lDiagram[i].UTime := 0;

  nPos := 8;
  for i := 0 to N_Vals - 1 do
  begin
    // предохранимс€ от недочитанного блока
    if nPos + 12 > len then break;

    lDiagram[i].UTime := ReadUINT(buf, nPos);
    lDiagram[i].Val := ReadDouble(buf, nPos + 4);
    lDiagram[i].Chn := GMParams.ByID(ID_Prm);

    nPos := nPos + 12;
  end;

  State:=dosDone;
  Result:=true;
end;

function TDataOrder.ParseOneByteResult(buf: array of byte; len: int): bool;
begin
  SetLength(lDiagram, 1);
  lDiagram[0].Val := buf[2];

  State:=dosDone;
  Result:=true;
end;

function TDataOrder.ParseUBZDataBlock(buf: array of byte; len: int): bool;
begin
  SetLength(lDiagram, 8);

  // I
  lDiagram[0].UTime := ReadUINT(buf, 2);
  lDiagram[0].Val := ReadDouble(buf, 6);
  lDiagram[1].Val := ReadDouble(buf, 14);
  lDiagram[2].Val := ReadDouble(buf, 22);
  //U
  lDiagram[3].Val := ReadDouble(buf, 30);
  lDiagram[4].Val := ReadDouble(buf, 38);
  lDiagram[5].Val := ReadDouble(buf, 46);
  //State
  lDiagram[6].UTime := ReadWORD(buf, 54);
  //Errors
  lDiagram[7].UTime := ReadUINT(buf, 56);

  State:=dosDone;
  Result:=true;
end;

procedure TDataOrder.ProcessDiagrams();
var i, j, n, len: int;
    chn: TGMBaseParam;
begin
  len := 0;
  for i := 0 to resp.Data.Count - 1 do
    if resp.Data[i].ValType in DATATYPE_DIAGRAM_ANY then
      inc(len, resp.Data[i].Count);

  SetLength(lDiagram, len);
  n := 0;
  for i := 0 to resp.Data.Count - 1 do
  begin
    if resp.Data[i].ValType in DATATYPE_DIAGRAM_ANY then
    begin
      chn := ParamOrNodeChannel(resp.Data[i].ValSrcType, resp.Data[i].Id);
      for j := 0 to resp.Data[i].Count - 1 do
      begin
        lDiagram[n].UTime := resp.Data[i][j].Utime;
        lDiagram[n].Val := resp.Data[i][j].Val;
        lDiagram[n].ValType := valueTypeCurrent;
        lDiagram[n].Chn := chn;
        inc(n);
      end;
    end;
  end;
end;

procedure TDataOrder.ProcessResponce();
begin
  ProcessDiagrams();
end;

function TDataOrder.ParseUniversalResponce(buf: array of byte; len: int): bool;
var zipLen: int;
    xml: string;
begin
  Result := false;

  zipLen := ReadUINT(buf, 2);
  if len < zipLen + 6 then Exit;

  try
    xml := string(DecompressBufferToString(buf, 6, zipLen));

    if FindCmdLineSwitch('logExchange') then
      SaveStringToFile(FormatXMLData(xml), IntToStr(GetTickCount()) + '_in.xml');

    resp := GetGMIOResponce(LoadXMLData(xml));
    // старые сервера всегда ответ€т с пустым RequestID
    Result := (resp.Auth.OK = 1) and ((resp.Auth.RequestID = '') or (resp.Auth.RequestID = req.Auth.RequestID));
    if Result then
    begin
      ProcessResponce();
      State := dosDone;
    end;
  except
    on E: Exception do
      ProgramLog.AddException('ParseUniversalResponce');
  end;
end;

procedure TDataOrder.ProcessCurrentsPack;
var i: int;
    p: TGMBaseParam;
    UTime: LongWord;
    val: double;
begin
  if resp = nil then Exit;

  DataThreadSynch.BeginWrite();
  try
    for i := 0 to resp.Data.Count - 1 do
    begin
      if resp.Data[i].ValType = DATATYPE_CURRENTS then
      begin
        p := ParamOrNodeChannel(resp.Data[i].ValSrcType, resp.Data[i].Id);
        if p = nil then continue;

        UTime := resp.Data[i].Value[0].Utime;
        val := resp.Data[i].Value[0].Val;

        if p.UpdateVal(Utime, Val) then
          GMPostMessage(WM_UPDATE_OBJECT, WParam(p), 0);
      end;
    end;
  except end;
  DataThreadSynch.EndWrite();

  acAlarms.RefreshAlarms();
end;

function TDataOrder.ParseResult(buf: array of byte; len: int): bool;
begin
  if (buf[0] = 100) and (buf[1] = 101) and (len >= 6) then
    Result := ParseSQL(buf, len)
  else
  if (buf[0]=231) and (buf[1]=161) and (len>=7) then
    Result := ParseDiagram(buf, len)
  else
  // ѕроверка допуска к управлению
  if (buf[0] = 210) and (buf[1] = 220) and (len >= 3) then
    Result := ParseOneByteResult(buf, len)
  else
  // управление  - объект найден
  if (buf[0] = 171) and (buf[1] = 26) and (len >= 3) then
    Result := ParseOneByteResult(buf, len)
  else
  // ”Ѕ«
  if (buf[0]=211) and (buf[1]=245) and (len>=60) then
    Result := ParseUBZDataBlock(buf, len)
  else
  // запуск двигател€ по ”Ѕ«
  if (buf[0] = 57) and (buf[1] = 98) and (len >= 3) then
    Result := ParseOneByteResult(buf, len)
  else
  if (((buf[0] = 255) and (buf[1] = 0)) or ((buf[0] = 250) and (buf[1] = 10)) ) and (len >= 6) then
    Result := ParseUniversalResponce(buf, len)
  else
    Result := false;
end;

function TDataOrder.WaitFor(TimeOutMs: int): bool;
var t: int64;
begin
  t := GetTickCount();
  repeat
    Sleep(100);
  until not (State in [dosWaiting, dosProcessing])
        or (Abs(GetTickCount() - t) > TimeOutMs)
        or Application.Terminated;

  Result := State = dosDone;
end;

{ TConnectionObjectGMClient }

function TConnectionObjectGMClient.InternalCheckGetAllData(): bool;
var N_Prms, len: int;
begin
  Result := false;

  if (buffers.BufRec[0] = 100) and (buffers.BufRec[1] = 101) and (buffers.NumberOfBytesRead >= 6) then
  begin // SQL
    N_Prms := ReadUINT(buffers.BufRec, 2);
    Result := (buffers.NumberOfBytesRead >= 6 + N_Prms);
  end
  else
  if (buffers.BufRec[0] = 159) and (buffers.BufRec[1] = 201) and (buffers.NumberOfBytesRead >= 4) then
  begin // текущие
    N_Prms := ReadWord(buffers.BufRec, 2);
    Result := (buffers.NumberOfBytesRead >= 4 + N_Prms * 16);
  end
  else
  if (buffers.BufRec[0] = 231) and (buffers.BufRec[1] = 161) and (buffers.NumberOfBytesRead >= 8) then
  begin  // график
    N_Prms := ReadWORD(buffers.BufRec, 6);
    Result := (buffers.NumberOfBytesRead >= 8 + N_Prms * 12);
  end
  else
  if (buffers.BufRec[0] = 255) and (buffers.BufRec[1] = 0) and (buffers.NumberOfBytesRead >= 6) then
  begin  // универсальный запрос
    len := ReadUINT(buffers.BufRec, 2);
    Result := (buffers.NumberOfBytesRead >= 6 + len);
  end;
end;

{ TGMCOMThreadPool }

class procedure TGMCOMThreadPool.AddReqCurrentsPrm(prm: TGMBaseParam);
begin
  GetInstance()[0].reqPrms.AddUnique(prm);
end;

constructor TGMCOMThreadPool.Create;
begin
  inherited Create();
  FServerTimeDiffSec := 0;
  FInstance := nil;
end;

class procedure TGMCOMThreadPool.CreateThreads;
begin
  GetInstance().DoCreateThreads();
end;

class procedure TGMCOMThreadPool.DestroyThreads;
begin
  GetInstance().DoDestroyThreads();
  FreeAndNil(FInstance);
end;

procedure TGMCOMThreadPool.DoCreateThreads;
var
  i: int;
begin
  if Count > 0 then Exit;

  for i := 0 to GMCOMThreadPool_MaxThreads - 1 do
    Add(TGMCOMThread.Create());
end;

procedure TGMCOMThreadPool.DoDestroyThreads;
var
  i: int;
  t: int64;
  bWait: bool;
begin
  for i := 0 to Count - 1 do
    GetInstance()[i].Terminate;

  t := GetTickCount();

  repeat
    bWait := false;
    for i := 0 to Count - 1 do
      bWait := bWait or not GetInstance()[i].Done;
  until not bWait or (Abs(GetTickCount - t) > 30000);

  for i := 0 to Count - 1 do
    if GetInstance()[i].Done then
      GetInstance()[i].Free();

  Clear();
end;

class function TGMCOMThreadPool.GetInstance: TGMCOMThreadPool;
begin
  if FInstance = nil then
    FInstance := TGMCOMThreadPool.Create();

  Result := FInstance;
end;

class procedure TGMCOMThreadPool.SetServerTimeDiffSec(const Value: int);
begin
  if Abs(Value) > 3600 then
    FServerTimeDiffSec := 0 // если больше часа расхождение, то это что-то страшное случилось
  else
    FServerTimeDiffSec := Value;
end;

initialization

  acAlarms := TAlarmsCollection.Create();
  DataThreadSynch := TMultiReadExclusiveWriteSynchronizer.Create();

finalization

  TDataOrders.GetInstance().Free();
  acAlarms.Free();
  DataThreadSynch.Free();
end.
