////////////////////////////////////////////
//  лассы дл€ хранени€ первичных данных 
////////////////////////////////////////////
unit GeomerLastValue;

interface

uses GMConst, GMGlobals, Windows, SysUtils, Classes, GMBlockValues, Devices.UBZ,
     GMSqlQuery, GMGenerics;

type
  TGeomerLastValue = class
  private
    function GetLastVal: TValueFromBase;
    function GetPrevVal: TValueFromBase;
    procedure SetLastVal(const Value: TValueFromBase);
    procedure ReadSpecDataFromQuery(q: TGMSqlQuery);
  public
    ID_Prm: int;
    ID_Obj: int;
    ID_Device: int;
    ID_DevType: int;
    ID_PT: byte;

    DevMin, DevMax, RealMin, RealMax: double;

    bCounter, bNI: bool;
    utNI: LongWord;
    ValNI: double;

    // архив последних значений, заполн€етс€ с конца
    ValsArch: array [0..20] of TValueFromBase;

    // дл€ ”Ѕ« и “–-101
    NAddr: int; // номер устройства на интерфейсе

    I: array[1..3] of double; // фазные токи
    I0: double; // ток нулевой последовательности
    U: array[1..3] of double; // фазные напр€жени€
    T: array[1..4] of double; // температуры TR101
    SP: array[1..4] of double; // уставки температуры TR101
    P: double; // полна€ мощность
    A: double; // наработка двигател€
    wState: WORD; // регистр состо€ни€
    dwAlarms: LongWord; // рагистры ошибок
    UBZ_Transformator: TTransformatorType;
    tLastUBZReq: LongWord; // врем€ последнего опроса
    clUBZEvtLog: TCollection;

    LastUpdate: int64;

    constructor Create();
    destructor Destroy; override;
    procedure ShiftVals();
    property LastVal: TValueFromBase read GetLastVal write SetLastVal;
    property PrevVal: TValueFromBase read GetPrevVal;
    procedure AddUBZAlarmLogOneEvt(dt: UINT; st: WORD; requestUTime: UINT);
    function ValueFromBaseClass(): TValueFromBaseClass;
    procedure UpdateSpecInfo();
    function CalcLastVal(): TValueFromBase;
  end;

  TGeomerLastValuesBuffer = class(TGMCollection<TGeomerLastValue>)
  private
    function GetValByID(ID_Prm: int): TGeomerLastValue;
  public
    synch: TMultiReadExclusiveWriteSynchronizer;

    function AddValue(ID_Obj, ID_PT, ID_Prm, ID_Device: int; bCounter: bool; bNI: bool; DevMin, DevMax, RealMin, RealMax: double; UDT: UINT; Value: double): TGeomerLastValue;
    function AddSimpleValue(ID_Prm: int; UDT: UINT; Value: double; q: TGMSqlQuery = nil): TGeomerLastValue; 
    property ValByID[ID_Prm: int]: TGeomerLastValue read GetValByID;
    function GetUBZ(ID_Obj, NAddr: int): TGeomerLastValue;
    constructor Create();
    destructor Destroy; override;
  end;
  
implementation

uses ProgramLogFile, Math;

{ TGeomerLastValuesBuffer }

function TGeomerLastValuesBuffer.AddSimpleValue(ID_Prm: int; UDT: UINT; Value: double; q: TGMSqlQuery = nil): TGeomerLastValue;
begin
  Result := ValByID[ID_Prm];

  synch.BeginWrite();
  try
    if Result = nil then
    begin
      Result := Add();
      Result.ID_Prm := ID_Prm;
    end;

    if q = nil then
      Result.UpdateSpecInfo()
    else
      Result.ReadSpecDataFromQuery(q);

    if UDT > Result.LastVal.UTime then
    begin
      Result.ShiftVals();
      Result.LastVal := MakeValueFromBase(UDT, Value);
    end;
  except end;
  synch.EndWrite();
end;

function TGeomerLastValuesBuffer.AddValue( ID_Obj, ID_PT, ID_Prm, ID_Device: int;
                                           bCounter: bool;
                                           bNI: bool;
                                           DevMin, DevMax, RealMin, RealMax: double;
                                           UDT: UINT;
                                           Value: double): TGeomerLastValue;
begin
  Result := ValByID[ID_Prm];

  synch.BeginWrite();
  try
    if Result = nil then
      Result := Add();

    Result.bCounter := bCounter;
    Result.bNI := bNI;
    Result.ID_Obj := ID_Obj;
    Result.ID_PT := ID_PT;
    Result.ID_Device := ID_Device;
    Result.ID_Prm := ID_Prm;
    Result.DevMin := DevMin;
    Result.DevMax := DevMax;
    Result.RealMin := RealMin;
    Result.RealMax := RealMax;

    if UDT > Result.LastVal.UTime then
    begin
      Result.ShiftVals();
      Result.LastVal := MakeValueFromBase(UDT, Value);
    end;
  except end;
  synch.EndWrite();
end;

constructor TGeomerLastValuesBuffer.Create;
begin
  inherited Create();
  synch := TMultiReadExclusiveWriteSynchronizer.Create();
end;

destructor TGeomerLastValuesBuffer.Destroy;
begin
  synch.Free();
  inherited;
end;

function TGeomerLastValuesBuffer.GetUBZ(ID_Obj, NAddr: int): TGeomerLastValue;
var i: int;
    q: TGMSqlQuery;
    DevType: int;
begin
  synch.BeginRead();
  try
  for i := 0 to Count - 1 do
    if (Items[i].ID_Obj = ID_Obj) and (Items[i].ID_PT = 0) and (Items[i].NAddr = NAddr) then
    begin
      Result:=Items[i];
      Exit;
    end;
  finally
    synch.EndRead();
  end;

  // не нашли, будем создавать
  DevType := 0;
  q := TGMSqlQuery.Create();
  try
    q.SQL.Text := Format('select * from Devices where ID_Obj = %d and Number = %d',
                         [ID_Obj, NAddr]);
    q.Open();
    if not q.Eof then
      DevType := q.FieldByName('ID_DevType').AsInteger;
  except
    on e: Exception do
      ProgramLog().AddException('TGeomerLastValuesBuffer.GetUBZ - ' + e.Message);
  end;
  q.Free();

  synch.BeginWrite();
  try
    Result := Add();
    Result.ID_Obj := ID_Obj;
    Result.ID_PT := 0;
    Result.ID_DevType := DevType;
    Result.NAddr := NAddr;
  finally
    synch.EndWrite();
  end;
end;

function TGeomerLastValuesBuffer.GetValByID(ID_Prm: int): TGeomerLastValue;
var i: int;
begin
  Result:=nil;

  synch.BeginRead();
  try
  for i:=0 to Count-1 do
    if Items[i].ID_Prm = ID_Prm then
    begin
      Result:=Items[i];
      Exit;
    end;
  finally
    synch.EndRead();
  end;
end;

{ TGeomerLastValue }

procedure TGeomerLastValue.AddUBZAlarmLogOneEvt(dt: UINT; st: WORD; requestUTime: UINT);
var el: TUBZEVTLogEntry;
    i: int;
begin
  while clUBZEvtLog.Count > 10 do
    clUBZEvtLog.Delete(0); // заведомо древние

  for i := 0 to clUBZEvtLog.Count - 1 do
  begin
    el := TUBZEVTLogEntry(clUBZEvtLog.Items[i]);
    if (el.DT_UBZ = dt) and (el.State = st) then Exit; // такое уже есть
  end;

  el := TUBZEVTLogEntry(clUBZEvtLog.Add());
  el.DT_UBZ := dt;
  el.uDT := requestUTime;
  el.State := st;
  el.bProcessed := clUBZEvtLog.Count <= 5; // глубина журнала событий ”Ѕ« 5 записей
                                           // первые 5 пропускаем просто запоминаем, они нам не нужны
                                           // потом начинаем смотреть изменени€ относительно них
end;

constructor TGeomerLastValue.Create();
var i: int;
begin
  inherited;

  ID_Obj := 0;
  ID_PT := 0;
  ID_Prm := 0;
  ID_Device := 0;

  bCounter:=false;
  bNI := false;

  utNI := 0;
  ValNI := 0;

  for i := 0 to High(ValsArch) do
  begin
    ValsArch[i].UTime:=0;
    ValsArch[i].Val:=0;
  end;

  NAddr := 0;

  for i := 1  to High(self.I) do
    self.I[i] := 0;

  for i := 1  to High(self.U) do
    self.U[i] := 0;

  for i := 1  to High(self.T) do
    self.T[i] := 0;

  UBZ_Transformator := ttUnknown;

  wState := 0;
  tLastUBZReq := 0;

  clUBZEvtLog := TCollection.Create(TUBZEVTLogEntry);
end;

destructor TGeomerLastValue.Destroy;
begin
  clUBZEvtLog.Free();
  
  inherited;
end;

function TGeomerLastValue.GetLastVal: TValueFromBase;
begin
  Result:=ValsArch[High(ValsArch)];
end;

function TGeomerLastValue.GetPrevVal: TValueFromBase;
var i: int;
    b: bool;
begin
  Result := ValsArch[High(ValsArch)];

  for i := High(ValsArch) - 1 downto 0 do
  begin
    b := (i = 0) or (ValsArch[i - 1].UTime = 0); // дальше опускатьс€ некуда

    if bCounter then
    begin
      // счетчик может мотать очень медленно, потому поищем значение, которое отличаетс€ от текущего
      b := b or ((LastVal.UTime - ValsArch[i].UTime >= 300) and (CompareValue(LastVal.Val, ValsArch[i].Val) <> 0))
    end
    else
    begin
      b := b or (LastVal.UTime - ValsArch[i].UTime >= 300); // интервал усреднени€ текущих - 5 мин
    end;

    if b then
    begin
      Result := ValsArch[i];
      Exit;
    end;
  end;
end;

procedure TGeomerLastValue.SetLastVal(const Value: TValueFromBase);
begin
  ValsArch[High(ValsArch)]:=Value;
end;

procedure TGeomerLastValue.ShiftVals;
var i: int;
begin
  for i:=0 to High(ValsArch)-1 do
   ValsArch[i]:=ValsArch[i+1];
end;

function TGeomerLastValue.ValueFromBaseClass: TValueFromBaseClass;
begin
  Result := TValueFromBaseClass.Create();
  Result.ID_Prm := ID_Prm;

  if bNI then
  begin
    Result.Val.UTime := utNI;
    Result.Val.Val := ValNI;
  end
  else
  begin
    Result.Val.UTime := LastVal.UTime;
    Result.Val.Val := LastVal.Val;
  end;
end;

procedure TGeomerLastValue.ReadSpecDataFromQuery(q: TGMSqlQuery);
begin
  ID_Obj := q.FieldByName('ID_Obj').AsInteger;
  ID_PT := q.FieldByName('ID_PT').AsInteger;
  ID_Device := q.FieldByName('ID_Device').AsInteger;
  bCounter  := q.FieldByName('ID_Src').AsInteger = SRC_CNT_DI;
  bNI := (q.FieldByName('ID_Src').AsInteger in CounterParamTypes) and not q.FieldByName('NI_StartVal').IsNull and (q.FieldByName('NI_StartDT').AsInteger > 0);
  DevMin := q.FieldByName('DevMin').AsFloat;
  DevMax := q.FieldByName('DevMax').AsFloat;
  RealMin := q.FieldByName('RealMin').AsFloat;
  RealMax := q.FieldByName('RealMax').AsFloat;

  LastUpdate := GetTickCount();
end;

procedure TGeomerLastValue.UpdateSpecInfo();
var q: TGMSqlQuery;
begin
  if (ID_Obj > 0) and (Abs(LastUpdate - GetTickCount()) < 600000) then Exit;

  q := TGMSqlQuery.Create();
  try
    q.SQL.Text := ' select * from Params p join Devices d on p.ID_Device = d.ID_Device' +
                  ' where ID_PT > 0 and ID_Prm = ' + IntToStr(ID_Prm);

    q.Open();
    if not q.Eof then
      ReadSpecDataFromQuery(q);
  except
    on e: Exception do
      ProgramLog().AddException('TGeomerLastValue.UpdateSpecInfo - ' + e.Message);
  end;
  q.Free();
end;

function TGeomerLastValue.CalcLastVal: TValueFromBase;
var delta: int;
    lv, pv: double;
begin
  Result.UTime := LastVal.UTime;

  if bNI and (utNI > 0) then
  begin
    Result.UTime := utNI;
    Result.Val := ValNI;
  end
  else
  begin
    if bCounter then
    begin
      if (LastVal.UTime > 0) and (PrevVal.UTime > 0)
          and (LastVal.UTime <> PrevVal.UTime) then
      begin
        // дельта по времени
        delta := LastVal.UTime - PrevVal.UTime;
        lv := LastVal.Val;
        pv := PrevVal.Val;

        // если все нормально
        if lv >= pv then
          Result.Val := (lv - pv) / delta
        else
        //переход через 0
          Result.Val := (lv + (65536 - pv)) / delta;
      end
      else
        Result.Val := 0;
    end
    else // имтируем аварию обрыв св€зи с прибором
    if ID_PT in [ID_PT_UBZ_ALARM, ID_PT_TR101_ALARM] then
    begin
      if NowGM() - LastVal.UTime > 3 * 60 then
      begin
        Result.Val := $FFFF;
        Result.UTime := NowGM();
      end
      else
        Result.Val := LastVal.Val;
    end
    else
      Result.Val := LastVal.Val;

    if (DevMin <> DevMax) and (RealMin <> RealMax) then
      Result.Val := RealMin + (Result.Val - DevMin) * (RealMax - RealMin) / (DevMax - DevMin);
  end;
end;

end.
