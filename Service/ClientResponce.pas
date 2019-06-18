unit ClientResponce;

interface

uses Classes, Windows, SysUtils, GMSqlQuery, StdRequest, StdResponce, GMGlobals, GMConst, GeomerLastValue,
     Generics.Collections, StrUtils, ProgramLogFile, Math, DB;

type
  TClientResponce = class
  private
    resp: IXMLGMIOResponceType;
    req: IXMLGMIORequestType;
    FID_User: int;
    glvBuffer: TGeomerLastValuesBuffer;
    glvDBBuffer: TGeomerLastValuesBuffer;

    procedure RequestUniversalData();
    procedure RequestUniversalStructure();
    procedure RequestUniversalStructure_Aggregates();
    procedure RequestUniversalStructure_AggregatesFromQuery(q: TGMSqlQuery; procObj: pointer);
    procedure RequestUniversalStructure_Channels(userFilter: bool);
    procedure RequestUniversalStructure_ChannelsFromQuery(q: TGMSqlQuery; procObj: pointer);
    procedure RequestUniversalStructure_Nodes();
    procedure RequestUniversalStructure_NodesChannelsFromQuery(q: TGMSqlQuery; procObj: pointer);
    procedure RequestUniversalStructure_NodesFromQuery(q: TGMSqlQuery; procObj: pointer);
    procedure RequestUniversalStructure_PT();
    procedure RequestUniversalStructure_PTFromQuery(q: TGMSqlQuery; procObj: pointer);
    procedure AddPrmCurrVal(id_prm: int);
    procedure RequestUniversalData_ProcessCurrNodeChannels(nodeChannels: TList<int>);
    procedure RequestUniversalData_ProcessCurrNodeChannels_ReadQuery(q: TGMSqlQuery; obj: pointer);
    procedure RequestUniversalData_AddCurrDiagram(src: StdRequest.IXMLSourceType);
    procedure RequestUniversalData_AddOneValue(q: TGMSqlQuery; obj: pointer); overload;
    function RequestUniversalData_AddOneValue(src: StdResponce.IXMLSourceType; Utime: LongWord; Val: double): IXMLValueType; overload;
    procedure RequestUniversalData_AddNI(src: StdRequest.IXMLSourceType);
    function SrcIDParam(src: StdRequest.IXMLSourceType): string;
    function SourceToResponce(src: StdRequest.IXMLSourceType): StdResponce.IXMLSourceType;
    procedure RequestUniversalData_AddHourDiagram(src: StdRequest.IXMLSourceType);
    procedure RequestUniversalData_AddArchDiagram(src: StdRequest.IXMLSourceType; const table: string);
    procedure RequestUniversalData_AddDayDiagram(src: StdRequest.IXMLSourceType);
    procedure AddPrmCurrVal_DoAdd(currVal: TGeomerLastValue);
    procedure AddPrmCurrVal_FromDB(q: TGMSqlQuery; obj: pointer);
    procedure RequestUniversalData_Command(src: StdRequest.IXMLSourceType);
  public
    constructor Create(AglvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy(); override;
    class function CheckAuthorization(const loginMd5, passwordMd5: string; var userId: int): bool;
    function ProcessRequest(ARequest: IXMLGMIORequestType; const userMd5: string): IXMLGMIOResponceType;
    function DBStructure(): IXMLGMIOResponceType;
  end;

implementation

uses
  UsefulQueries;

class function TClientResponce.CheckAuthorization(const loginMd5, passwordMd5: string; var userId: int): bool;
var
  sql: string;
begin
  userId := 0;
  Result := Trim(QueryResultFmt('select GetConfigPrm(%s)', [QuotedStr(SYS_CONFIG_PARAM_AUTH_REQUIRED)])) = '';
  if Result then
    Exit;

  sql := Format('select ID_User from Users where Md5(lower(login)) = %s and password = %s',
    [QuotedStr(loginMD5), QuotedStr(LowerCase(passwordMd5))]);

  userId := StrToIntDef(QueryResult(sql), 0);

  Result := userId > 0;
end;

constructor TClientResponce.Create(AglvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create();
  glvBuffer := AglvBuffer;
  FID_User := 0;
end;

function TClientResponce.DBStructure: IXMLGMIOResponceType;
begin
  resp := NewGMIOResponce();
  try
    RequestUniversalStructure_Channels(false);
    Result := resp;
  finally
    resp := nil;
  end;
end;

destructor TClientResponce.Destroy;
begin
  TryFreeAndNil(glvDBBuffer);
  inherited;
end;

function FloatFieldToStr(f: TField): string;
begin
  if f.IsNull then
    Result := ''
  else
    Result := MyFloatToStr(f.AsFloat);
end;

procedure TClientResponce.RequestUniversalStructure_ChannelsFromQuery(q: TGMSqlQuery; procObj: pointer);
var obj: IXMLObjectType;
    dev: IXMLDeviceType;
    chn: IXMLChannelType;
    id_obj, id_dev: int;
    resp: IXMLGMIOResponceType;
begin
  resp := IXMLGMIOResponceType(procObj);

  id_obj := q.FieldByName('ID_Obj').AsInteger;
  obj := resp.Objects.ObjectById(id_obj);
  if obj = nil then
  begin
    obj := resp.Objects.Add();
    obj.Id := id_obj;
    obj.Name := q.FieldByName('ObjName').AsString;
    obj.Ncar := q.FieldByName('N_Car').AsInteger;
    obj.ObjType := q.FieldByName('ObjType').AsInteger;
  end;

  id_dev := q.FieldByName('ID_Device').AsInteger;
  dev := obj.Devices.DeviceById(id_dev);
  if dev = nil then
  begin
    dev := obj.Devices.Add();
    dev.Id := id_dev;
    dev.Name := q.FieldByName('DevName').AsString;
    dev.ID_DevType := q.FieldByName('ID_DevType').AsInteger;
    dev.DevType := q.FieldByName('DevTypeName').AsString;
    dev.Number := q.FieldByName('DevNumber').AsInteger;
  end;

  chn := dev.Channels.Add();
  chn.Id := q.FieldByName('ID_Prm').AsInteger;
  chn.Name := q.FieldByName('PrmName').AsString;
  chn.ID_PT := q.FieldByName('ID_PT').AsInteger;
  chn.ID_Src := q.FieldByName('ID_Src').AsInteger;
  chn.UserSrc := q.FieldByName('UserSrc').AsInteger;
  chn.Nsrc := q.FieldByName(IfThen(chn.ID_Src = SRC_USR, 'CurrentsAddr', 'N_Src')).AsInteger;
  chn.AlarmSignal := q.FieldByName('AlarmSignal').AsInteger;
  chn.AlarmThreshold := FloatFieldToStr(q.FieldByName('AlarmThreshold'));
  chn.BaseChn := q.FieldByName('BaseChn').AsInteger;
  chn.ID_MeaUnit := q.FieldByName('ID_MeaUnit').AsInteger;
  chn.DevMin := FloatFieldToStr(q.FieldByName('DevMin'));
  chn.DevMax := FloatFieldToStr(q.FieldByName('DevMax'));
  chn.RealMin := FloatFieldToStr(q.FieldByName('RealMin'));
  chn.RealMax := FloatFieldToStr(q.FieldByName('RealMax'));
  chn.NI_StartDT := q.FieldByName('NI_StartDT').AsVariant;
  chn.NI_StartVal := FloatFieldToStr(q.FieldByName('NI_StartVal'));
  chn.CalibrType := q.FieldByName('CalibrType').AsInteger;
  chn.Resistor := q.FieldByName('Resistor').AsInteger;
  chn.MeterCalcNIType := q.FieldByName('MeterCalcNIType').AsInteger;
  chn.ReqIntervalType := q.FieldByName('ReqIntervalType').AsInteger;

  obj := nil;
  dev := nil;
  chn := nil;
  resp := nil;
end;

procedure TClientResponce.RequestUniversalStructure_Channels(userFilter: bool);
begin
  ReadFromQuery(        'select p.ID_Prm, p.Name as PrmName, p.ID_PT, p.ID_Src, p.N_Src, p.UserSrc, p.CurrentsAddr,' +
                  #13#10'       p.DevMin, p.DevMax, p.RealMin, p.RealMax, p.ID_MeaUnit,' +
                  #13#10'       p.AlarmSignal, p.AlarmThreshold, p.BaseChn, p.OpcTag,' +
                  #13#10'       p.NI_StartDT, p.NI_StartVal, p.CalibrType, p.Resistor, p.MeterCalcNIType, p.ReqIntervalType,' +
                  #13#10'       d.ID_Device, d.ID_DevType, dt.Name as DevTypeName, d.DevName, d.Number as DevNumber,' +
                  #13#10'       o.ID_Obj, o.ObjType, o.Name as ObjName, o.N_Car ' +
                  #13#10'  from Params p ' +
                  #13#10'       join Devices d on p.ID_Device = d.ID_Device ' +
                  #13#10'       join ' + IfThen(userFilter, 'GetUserObjects(' + IntToStr(FID_User) + ')', 'Objects') + ' o on d.ID_Obj = o.ID_Obj ' +
                  #13#10'       join DevTypes dt on d.ID_DevType = dt.ID_DevType ' +
                  #13#10'  where p.ID_PT in (select ID_PT from ParamTypes) ' +
{$ifdef TEST_REMOTE_SRV}
                  #13#10'        and coalesce(o.RemoteName, '''') = '''' ' +
{$endif}
                  #13#10'order by o.ID_Obj, d.ID_Device, BaseChn desc, ID_Src, N_Src',
                  RequestUniversalStructure_ChannelsFromQuery,
                  pointer(resp));
end;

procedure TClientResponce.RequestUniversalStructure_NodesFromQuery(q: TGMSqlQuery; procObj: pointer);
var node: IXMLNodeType;
    resp: IXMLGMIOResponceType;
begin
  resp := IXMLGMIOResponceType(procObj);

  node := resp.Nodes.Add();
  node.Id := q.FieldByName('ID_Node').AsInteger;
  node.Parent := q.FieldByName('ID_Parent').AsInteger;
  node.NodeType := q.FieldByName('ID_NodeType').AsInteger;
  node.Name := q.FieldByName('Name').AsString;

  resp := nil;
  node := nil;
end;

procedure TClientResponce.RequestUniversalStructure_NodesChannelsFromQuery(q: TGMSqlQuery; procObj: pointer);
var node: IXMLNodeType;
    resp: IXMLGMIOResponceType;
    chn: IXMLNodeChannelType;
begin
  resp := IXMLGMIOResponceType(procObj);
  node := resp.Nodes.NodeById(q.FieldByName('ID_Node').AsInteger);
  if node = nil then Exit;

  chn := node.NodeChannels.Add();
  chn.Id := q.FieldByName('ID_Chn').AsInteger;
  chn.ID_PT := q.FieldByName('ID_PT').AsInteger;
  chn.Name := q.FieldByName('Name').AsString;
  chn.BaseChn := q.FieldByName('BaseChn').AsInteger;
  chn.ID_MeaUnit := q.FieldByName('ID_MeaUnit').AsInteger;

  resp := nil;
  node := nil;
  chn := nil;
end;

procedure TClientResponce.RequestUniversalStructure_Nodes();
begin
  ReadFromQuery('select * from Nodes',
                RequestUniversalStructure_NodesFromQuery,
                pointer(resp));

  ReadFromQuery('select * from NodeChannels',
                RequestUniversalStructure_NodesChannelsFromQuery,
                pointer(resp));
end;

procedure TClientResponce.RequestUniversalStructure_PTFromQuery(q: TGMSqlQuery; procObj: pointer);
var pt: IXMLParamType;
    resp: IXMLGMIOResponceType;
begin
  resp := IXMLGMIOResponceType(procObj);

  pt := resp.ParamTypes.Add();
  pt.ID_PT := q.FieldByName('ID_PT').AsInteger;
  pt.PSign := q.FieldByName('PSign').AsString;
  pt.PName := q.FieldByName('PName').AsString;
  pt.IsAlarm := q.FieldByName('IsAlarm').AsInteger;

  pt := nil;
  resp := nil;
end;

procedure TClientResponce.RequestUniversalStructure_PT();
begin
  ReadFromQuery('select * from Paramtypes', RequestUniversalStructure_PTFromQuery, pointer(resp));
end;

procedure TClientResponce.RequestUniversalStructure_AggregatesFromQuery(q: TGMSqlQuery; procObj: pointer);
var aggr: IXMLAggregateType;
    ap: IXMLAggregateParamType;
    obj: IXMLObjectType;
    resp: IXMLGMIOResponceType;
    id_aggr: int;
begin
  resp := IXMLGMIOResponceType(procObj);

  obj := resp.Objects.ObjectById(q.FieldByName('ID_Obj').AsInteger);
  if obj = nil then Exit;

  id_aggr := q.FieldByName('ID_Aggregate').AsInteger;
  aggr := obj.Aggregates.AggregateById(id_aggr);
  if aggr = nil then
  begin
    aggr := obj.Aggregates.Add();
    aggr.Id := id_aggr;
    aggr.ID_AggrType := q.FieldByName('ID_AggrType').AsInteger;
    aggr.Name := q.FieldByName('Name').AsString;
  end;

  ap := aggr.AggregateParams.Add();
  ap.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
  ap.PrmKind := q.FieldByName('PrmKind').AsInteger;
  ap.Caption := q.FieldByName('Caption').AsString;

  aggr := nil;
  ap := nil;
  obj := nil;
  resp := nil;
end;

procedure TClientResponce.RequestUniversalStructure_Aggregates();
begin
  ReadFromQuery( 'select a.ID_Aggregate, a.ID_Obj, a.ID_AggrType, a.Name, ID_Prm, PrmKind, Caption ' +
                 '  from AggregateParams ap ' +
                 '       join Aggregates a on ap.ID_Aggregate = a.ID_Aggregate ' +
                 '       join GetUserObjects(' + IntToStr(FID_User) + ') o on o.ID_Obj = a.ID_Obj ' +
                 '    where ID_Prm > 0 and ID_AggrType > 0',
                 RequestUniversalStructure_AggregatesFromQuery,
                 pointer(resp));
end;

procedure TClientResponce.RequestUniversalStructure();
begin
  RequestUniversalStructure_Channels(true);
  RequestUniversalStructure_PT();
  RequestUniversalStructure_Aggregates();
  RequestUniversalStructure_Nodes();
end;

function TClientResponce.ProcessRequest(ARequest: IXMLGMIORequestType; const userMd5: string): IXMLGMIOResponceType;
begin
  req := ARequest;
  resp := NewGMIOResponce();

  resp.Auth.RequestID := req.Auth.RequestID;
  resp.Auth.OK := int((CalcMd5(req.Auth.Login) = userMd5) and CheckAuthorization(userMd5, req.Auth.Password, FID_User));
  if resp.Auth.OK = 1 then
  begin
    if req.Structure = 1 then
      RequestUniversalStructure();

    RequestUniversalData();
  end;

  Result := resp;

  req := nil;
  resp := nil;
end;

procedure TClientResponce.AddPrmCurrVal_DoAdd(currVal: TGeomerLastValue);
var res: StdResponce.IXMLSourceType;
    resVal: IXMLValueType;
    val: TValueFromBase;
begin
  val := currVal.CalcLastVal();
  res := resp.Data.Add();
  res.Id := currVal.ID_Prm;
  res.ValSrcType := DATACHANNEL_PARAM;
  res.ValType := DATATYPE_CURRENTS;
  resVal := res.Add();
  resVal.Utime := val.UTime;
  resVal.Val := val.Val;

  res := nil;
  resVal := nil;
end;

procedure TClientResponce.AddPrmCurrVal_FromDB(q: TGMSqlQuery; obj: pointer);
begin
  glvDBBuffer.AddSimpleValue(
    q.FieldByName('ID_Prm').AsInteger,
    q.FieldByName('UTime').AsInteger,
    q.FieldByName('Val').AsFloat,
    q);
end;

procedure TClientResponce.AddPrmCurrVal(id_prm: int);
var currVal: TGeomerLastValue;
begin
  currVal := glvBuffer.ValByID[id_prm];
  if currVal <> nil then
  begin
    AddPrmCurrVal_DoAdd(currVal);
  end
  else
  begin
    if glvDBBuffer = nil then
    begin
      glvDBBuffer := TGeomerLastValuesBuffer.Create();
      ReadFromQuery('select * from CurrVals v join DeviceSystem d on v.ID_Prm = d.ID_Prm', AddPrmCurrVal_FromDB);
    end;

    currVal := glvDBBuffer.ValByID[id_prm];
    if currVal <> nil then
      AddPrmCurrVal_DoAdd(currVal);
  end;
end;

procedure TClientResponce.RequestUniversalData_ProcessCurrNodeChannels_ReadQuery(q: TGMSqlQuery; obj: pointer);
var currVal: TGeomerLastValue;
    res: StdResponce.IXMLSourceType;
    resVal: IXMLValueType;
    val: TValueFromBase;
begin
  currVal := glvBuffer.ValByID[q.FieldByName('BaseChn').AsInteger];
  if currVal <> nil then
  begin
    val := currVal.CalcLastVal();
    res := resp.Data.Add();
    res.Id := q.FieldByName('ID_Chn').AsInteger;
    res.ValSrcType := DATACHANNEL_NODECHANNEL;
    res.ValType := DATATYPE_CURRENTS;
    resVal := res.Add();
    resVal.Utime := val.UTime;
    resVal.Val := val.Val;
  end;

  res := nil;
  resVal := nil;
end;

procedure TClientResponce.RequestUniversalData_ProcessCurrNodeChannels(nodeChannels: TList<int>);
var id_chn: int;
    sql: string;
begin
  if nodeChannels.Count <= 0 then
    Exit;

  sql := '';
  for id_chn in nodeChannels do
  begin
    sql := sql + IfThen(sql <> '', #13#10'union ') + 'select ID_Chn, BaseChn from NodeChannels where ID_Chn = ' + IntToStr(id_chn) + ' and BaseChn is not null';
  end;

  ReadFromQuery(sql, RequestUniversalData_ProcessCurrNodeChannels_ReadQuery);
end;

function TClientResponce.RequestUniversalData_AddOneValue(src: StdResponce.IXMLSourceType; Utime: LongWord; Val: double): IXMLValueType;
begin
  Result := src.Add();
  Result.Utime := Utime;
  Result.Val := Val;
end;

procedure TClientResponce.RequestUniversalData_AddOneValue(q: TGMSqlQuery; obj: pointer);
var
  utime: LongWord;
begin
  if q.Fields.FindField('UTimeRes') <> nil then
    utime := q.FieldByName('UTimeRes').AsInteger
  else
    utime := q.FieldByName('UTime').AsInteger;

  RequestUniversalData_AddOneValue(StdResponce.IXMLSourceType(obj), utime, q.FieldByName('Val').AsFloat);
end;

function TClientResponce.SrcIDParam(src: StdRequest.IXMLSourceType): string;
begin
  case src.ValSrcType of

    DATACHANNEL_PARAM:
      Result := IntToStr(src.Id);

    DATACHANNEL_NODECHANNEL:
      Result := '(select BaseChn from NodeChannels where ID_Chn = ' + IntToStr(src.Id) + ')';

    else
      begin
        ProgramLog.AddError('Неизвестный тип источника данных: ' + IntToStr(src.ValSrcType));
        Result := '0';
      end;
  end;
end;

function TClientResponce.SourceToResponce(src: StdRequest.IXMLSourceType): StdResponce.IXMLSourceType;
begin
  Result := resp.Data.Add();
  Result.Id := src.Id;
  Result.ValSrcType := src.ValSrcType;
  Result.ValType := src.ValType;
end;

procedure TClientResponce.RequestUniversalData_AddCurrDiagram(src: StdRequest.IXMLSourceType);
var
  sql, ID_Prm: string;
  res: ArrayOfString;
begin
  // запрос архивных
  try
    if src.DiagramType = DIAGRAMTYPE_REMOTE then
    begin
      ID_Prm := Format(SQLConst_SelectRemotePrmId, [req.Auth.RemoteName.QuotedString(), src.Id]);
      sql := Format('select UTime, Val from CurrVals where ID_Prm = %s', [ID_Prm]);
      res := QueryResultArray_FirstRow(sql);
      if Length(res) > 0 then
        RequestUniversalData_AddOneValue(SourceToResponce(src), StrToIntDef(res[0], 0), MyStrToFloatDef(res[1], 0))
      else
        RequestUniversalData_AddOneValue(SourceToResponce(src), 0, 0);
    end
    else
    begin
      ID_Prm := SrcIDParam(src);
      case src.AggrType of

        DATA_AGGREGATION_TYPE_DEFAULT:
          sql := Format('select * from ReadParamDiagram (%s, %d, %d, %d, %d)',
                         [ID_Prm, src.UTime1, src.UTime2, src.AggrIntervalLen, src.DiagramType]);

        DATA_AGGREGATION_TYPE_DAYSUMM:
          sql := Format('select %d as UTime, ReadSpecialDaySummIntegral(%s, %d, %d) as Val',
                         [src.UTime1, ID_Prm, src.UTime1, src.UTime2]);

        else
          begin
            ProgramLog.AddError('Неизвестный тип аггрегации данных: ' + IntToStr(src.AggrType));
            Exit;
          end;
      end;
      ReadFromQuery(sql, RequestUniversalData_AddOneValue, pointer(SourceToResponce(src)));
    end;
  except
    on e: Exception do
      ProgramLog().AddError('RequestDiagram - ' + e.Message + ', sql = ' + sql);
  end;
end;

procedure TClientResponce.RequestUniversalData_AddArchDiagram(src: StdRequest.IXMLSourceType; const table: string);
var sql, ID_Prm: string;
begin
  // запрос архивных
  try
    if src.DiagramType = DIAGRAMTYPE_REMOTE then
    begin
      ID_Prm := Format(SQLConst_SelectRemotePrmId, [req.Auth.RemoteName.QuotedString(), src.Id]);
      sql := Format('select max(UTime) as UTime, 0 as Val from %s where ID_Prm = %s', [table, ID_Prm])
    end
    else
    begin
      ID_Prm := SrcIDParam(src);
      sql := Format('select %d + cast(floor((v.Utime - %d) / %d) * %d as bigint) as UTimeRes, PrmRealVal(%s, 0, avg(v.Val)) as Val ' +
                    ' from %s v where v.ID_Prm = %s and v.UTime >= %d and v.UTime < %d ' +
                    ' group by v.ID_Prm, UTimeRes ' +
                    ' order by UTimeRes',
                    [src.UTime1, src.UTime1, src.AggrIntervalLen * 60, src.AggrIntervalLen * 60, ID_Prm,
                     table, ID_Prm, src.UTime1, src.UTime2 ]);
    end;

    ReadFromQuery(sql, RequestUniversalData_AddOneValue, pointer(SourceToResponce(src)));
  except
    on e: Exception do
      ProgramLog().AddException('RequestDiagram - ' + e.Message);
  end;
end;

procedure TClientResponce.RequestUniversalData_AddHourDiagram(src: StdRequest.IXMLSourceType);
begin
  RequestUniversalData_AddArchDiagram(src, 'ArchHour');
end;

procedure TClientResponce.RequestUniversalData_AddDayDiagram(src: StdRequest.IXMLSourceType);
begin
  RequestUniversalData_AddArchDiagram(src, 'ArchDay');
end;

procedure TClientResponce.RequestUniversalData_AddNI(src: StdRequest.IXMLSourceType);
var sql: string;
begin
  sql := Format('select * from CalcNI(%s, %d)', [SrcIDParam(src), Max(src.UTime1, src.UTime2)]);
  ReadFromQuery(sql, RequestUniversalData_AddOneValue, pointer(SourceToResponce(src)));
end;

procedure TClientResponce.RequestUniversalData_Command(src: StdRequest.IXMLSourceType);
var
  ID_Prm: int;
begin
  ID_Prm := SQLReq_GetIdPrmForRemotePrmId(req.Auth.RemoteName, src.Id);
  ReadFromQuery(
    SqlText_LoadCommands(0, 0, ID_Prm),
    procedure (q: TGMSqlQuery)
    var
      resVal: IXMLValueType;
    begin
      resVal := SourceToResponce(src).Add();
      resVal.Id := q.FieldByName('ID_Cmd').AsInteger;
      resVal.Utime := q.FieldByName('UTime').AsInteger;
      resVal.Val := q.FieldByName('Value').AsFloat;
      resVal := nil;
    end
    );
end;

procedure TClientResponce.RequestUniversalData();
var i: int;
    src: StdRequest.IXMLSourceType;
    nodeChannelsCurr: TList<int>;
begin
  nodeChannelsCurr := TList<int>.Create();

  try
    for i := 0 to req.Data.Sources.Count - 1 do
    begin
      src := req.Data.Sources[i];
      case src.ValType of

        DATATYPE_CURRENTS:
          case src.ValSrcType of
            DATACHANNEL_PARAM: AddPrmCurrVal(src.Id);
            DATACHANNEL_NODECHANNEL: nodeChannelsCurr.Add(src.Id);
          end;

        DATATYPE_CURRENT_DIAGRAM:
          RequestUniversalData_AddCurrDiagram(src);

        DATATYPE_HOUR_DIAGRAM:
          RequestUniversalData_AddHourDiagram(src);

        DATATYPE_DAY_DIAGRAM:
          RequestUniversalData_AddDayDiagram(src);

        DATATYPE_NI:
          RequestUniversalData_AddNI(src);

        DATATYPE_COMMAND:
          RequestUniversalData_Command(src);
      end;
    end;

    RequestUniversalData_ProcessCurrNodeChannels(nodeChannelsCurr);
  finally
    src := nil;
    nodeChannelsCurr.Free();
  end;
end;

end.
