////////////////////////////////////////////
// Классы, представляющие структуру объектов в базе данных
////////////////////////////////////////////
unit GMDBClasses;

interface

uses Windows, Classes, SysUtils, GMGlobals, GMConst, GMGenerics, Generics.Collections;

type

  TGMParamType = class (TCollectionItem)
  public
    ID_PT: int;
    ParamName, ShowName: string;
    IsAlarm: bool;
    DefMeaUnit: int;
  end;

  TGMParamTypes = class(TCollection)
  private
    procedure Init;
    function GetParamType(i: int): TGMParamType;
  public
    procedure Prepare();
    function ParamTypeByID(ID_PT: int): TGMParamType;
    property ParamTypes[i: int]: TGMParamType read GetParamType; default;
    function Add(ID_PT: int; ParamName, ShowName: string; IsAlarm: bool = false; DefMeaUnit: int = 0): TGMParamType;
  end;

  TGMObject = class(TCollectionItem)
  public
    Active: bool; // устанавливается, если этот объект найден в объектах отображения
    ID_Obj: int;
    N_Car: int;
    Name: string;

    constructor Create(Collection: TCollection); override;
  end;

  TGMObjectList = class(TCollection)
  private
    function GetObject(Index: int): TGMObject;
  public
    property Objects[Index: int]: TGMObject read GetObject; default;
    function ObjByID(ID_Obj: int): TGMObject;
    function Add(): TGMObject;
  end;

  TGMDevice = class(TCollectionItem)
  private
    function GetName(): string;
  public
    Obj: TGMObject;
    ID_Device, ID_DevType: int;
    TypeName: string;
    DevName: string;
    Number: int;

    property Name: string read GetName;
  end;

  TGMDeviceList = class(TCollection)
  private
    function GetDevice(Index: int): TGMDevice;
  public
    property Devices[Index: int]: TGMDevice read GetDevice; default;
    function Add(): TGMDevice;
    function DeviceByID(ID_Device: int): TGMDevice;
  end;

  TParamPathComponent = (ppcID, ppcObj, ppcDev, ppcDevNoGM, ppcSrc, ppcPT, ppcNoBase);
  TParamPathComponents = set of TParamPathComponent;
  TGMParam = class;

  TGMBaseParam = class(TPersistent)
  private
    function GetIsAlarm: bool;
  protected
    FID_Prm: int;
    FPType: TGMParamType;
    procedure SetID_Prm(const Value: int); virtual;
    function GetDataChannelType: int; virtual; abstract;
    function GetIsMeterNI: bool; virtual; abstract;
  public
    BaseChn: TGMParam;
    MeaUnitName: string;
    PrmName: string;
    AlarmSignal: int;
    AlarmThreshold: double;
    LastVal: TValueFromBase;

    property PType: TGMParamType read FPType write FPType;
    property ID_Prm: int read FID_Prm write SetID_Prm;
    function GetNameWithPath(PathComponents: TParamPathComponents = [ppcID, ppcObj, ppcDev]; Separator: string = ' . '): string; virtual; abstract;
    property DataChannelType: int read GetDataChannelType;
    function UpdateVal(UTime: LongWord; Val: double): bool;
    function Equals(Obj: TObject): boolean; override;
    property IsMeterNI: bool read GetIsMeterNI;
    property IsAlarm: bool read GetIsAlarm;

    constructor Create();
  end;

  TGMParam = class(TGMBaseParam)
  private
    function N_SrcString: string;
  protected
    procedure SetID_Prm(const Value: int); override;
    function GetDataChannelType: int; override;
    function GetIsMeterNI: bool; override;
  public
    Device: TGMDevice;
    ID_Src, N_Src: int;

    function GetNameWithPath(PathComponents: TParamPathComponents = [ppcID, ppcObj, ppcDev]; Separator: string = ' . '): string; override;

    constructor Create();
  end;

  TGMBaseParamList<T: TGMBaseParam, constructor> = class(TGMCollection<T>)
  public
    function ByID(ID_Prm: int): T;
    function IndexOf(prm: TGMBaseParam): int;
  end;

  TGMDataChannelList = class(TList<TGMBaseParam>)
  public
    procedure AddUnique(prm: TGMBaseParam);
  end;

  TGMParamList = class(TGMBaseParamList<TGMParam>)
  public
    function GetID_Obj(ID_Prm: int): int;
  end;

  TGMNode = class
  public
    ID_Node: int;
    ID_Parent: int;
    parent: TGMNode;
    NodeType: int;
    Name: string;

    constructor Create();
  end;

  TGMNodeList = class(TGMCollection<TGMNode>)
  public
    function NodeById(ID_Node: int): TGMNode;
  end;

  TGMNodeChannel = class(TGMBaseParam)
  protected
    function GetCaption: string;
    function GetDataChannelType: int; override;
    function GetIsMeterNI: bool; override;
  public
    Node: TGMNode;
    Name: string;

    property Caption: string read GetCaption;
    function GetNameWithPath(PathComponents: TParamPathComponents = [ppcID, ppcObj, ppcDev]; Separator: string = ' . '): string; override;
  end;

  TGMNodeChannelList = class(TGMBaseParamList<TGMNodeChannel>)
  end;

  TGMAggregateParam = class
  public
    PrmKind: int;
    prm: TGMParam;
    Caption: string;
  end;

  TGMAggregate = class(TCollectionItem)
  private
    FParamList: TList;
    FAggrName: string;
    function GetParam(Index: int): TGMAggregateParam;
    function GetParamCount: int;
      function GetAggrName: string;
  public
    ID_Aggregate: int;
    AggrType: int;
    Obj: TGMObject;

    property AggrName: string read GetAggrName write FAggrName ;
    property ParamList: TList read FParamList;
    property ParamCount: int read GetParamCount;
    property Params[Index: int]: TGMAggregateParam read GetParam;
    function ParamByKind(PrmKind: int): TGMAggregateParam;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TGMAggregateList = class(TCollection)
  private
    function GetAggregate(Index: int): TGMAggregate;
  public
    function Add(): TGMAggregate;
    constructor Create();
    property Aggregates[Index: int]: TGMAggregate read GetAggregate; default;
    function AggregateByID(ID_Aggregate: int): TGMAggregate;
    function IsObjectControllable(ID_Obj: int): bool;
  end;

function ParamNameToID(const ParamName: string): int;
function ParamNameFromID(ID_PT: int): string;
function ParamShowNameFromID(ID_PT: int; NullName: string = 'Unknown Param'): string;
function ParamShowNameFromParamName(s: string): string;
function ParamOrNodeChannel(DataChnType, id_prm: int): TGMBaseParam;
function GMParamFromString(const s: string): TGMBaseParam;


var GMParamTypes: TGMParamTypes;
    GMObjects: TGMObjectList;
    GMDevices: TGMDeviceList;
    GMParams: TGMParamList;
    GMAggregates: TGMAggregateList;
    GMNodes: TGMNodeList;
    GMNodeChannels: TGMNodeChannelList;

implementation

uses Math, StrUtils;

function GMParamFromString(const s: string): TGMBaseParam;
var idc, n: int;
    str: string;
begin
  Result := nil;
  if s = '' then Exit;

  str := s;
  n := Pos('=', s);
  if n > 0 then
    Delete(str, n, str.Length);

  idc := StrToIntDef(Copy(str, 2, Length(str) - 1), 0);
  if idc <= 0 then Exit;

  case s[1] of
    'c': Result := GMParams.ByID(idc);
    'n': Result := GMNodeChannels.ByID(idc);
  end;
end;

function ParamOrNodeChannel(DataChnType, id_prm: int): TGMBaseParam;
begin
  if DataChnType = DATACHANNEL_NODECHANNEL then
    Result := GMNodeChannels.ByID(id_prm)
  else
    Result := GMParams.ByID(id_prm);
end;

function ParamNameToID(const ParamName: string): int;
var i: int;
begin
  for i := 0 to GMParamTypes.Count - 1 do
    if UpperCase(ParamName) = GMParamTypes[i].ParamName then
    begin
      Result := GMParamTypes[i].ID_PT;
      Exit;
    end;

  Result := 0;
end;

function ParamNameFromID(ID_PT: int): string;
var i: int;
begin
  for i := 0 to GMParamTypes.Count - 1 do
    if ID_PT = GMParamTypes[i].ID_PT then
    begin
      Result := GMParamTypes[i].ParamName;
      Exit;
    end;

  Result:='Unknown Param';
end;

function ParamShowNameFromID(ID_PT: int; NullName: string = 'Unknown Param'): string;
var i: int;
begin
  for i := 0 to GMParamTypes.Count - 1 do
    if ID_PT = GMParamTypes[i].ID_PT then
    begin
      Result := GMParamTypes[i].ShowName;
      Exit;
    end;

  Result := NullName;
end;

function ParamShowNameFromParamName(s: string): string;
begin
  Result := ParamShowNameFromID(ParamNameToID(s));
end;

{ TGMParamTypes }

function TGMParamTypes.Add(ID_PT: int; ParamName, ShowName: string; IsAlarm: bool = false; DefMeaUnit: int = 0): TGMParamType;
begin
  Result := TGMParamType(inherited Add());
  Result.ID_PT      := ID_PT;
  Result.ParamName  := ParamName;
  Result.ShowName   := ShowName;
  Result.IsAlarm    := IsAlarm;
  Result.DefMeaUnit := DefMeaUnit;
end;

procedure TGMParamTypes.Init();
begin
  Prepare();
end;

function TGMParamTypes.ParamTypeByID(ID_PT: int): TGMParamType;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if TGMParamType(Items[i]).ID_PT = ID_PT then
    begin
      Result := ParamTypes[i];
      Exit;
    end;
  end;
end;

function TGMParamTypes.GetParamType(i: int): TGMParamType;
begin
  Result := TGMParamType(Items[i]);
end;

procedure TGMParamTypes.Prepare;
begin
  Clear();
  Add(0, '', '');
end;

{ TGMObjectList }

function TGMObjectList.Add: TGMObject;
begin
  Result := TGMObject(inherited Add());
end;

function TGMObjectList.GetObject(Index: int): TGMObject;
begin
  Result := TGMObject (Items[Index]);
end;

function TGMObjectList.ObjByID(ID_Obj: int): TGMObject;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Objects[i]).ID_Obj = ID_Obj then
    begin
      Result := Objects[i];
      Exit;
    end;
  end;
end;

{ TGMDeviceList }

function TGMDeviceList.Add: TGMDevice;
begin
  Result := TGMDevice(inherited Add());
end;

function TGMDeviceList.DeviceByID(ID_Device: int): TGMDevice;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Devices[i].ID_Device = ID_Device then
    begin
      Result := Devices[i];
      Exit;
    end;
end;

function TGMDeviceList.GetDevice(Index: int): TGMDevice;
begin
  Result := TGMDevice (Items[Index]);
end;

{ TGMParamList }

function TGMParamList.GetID_Obj(ID_Prm: int): int;
var p: TGMParam;
    d: TGMDevice;
begin
  Result := 0;

  p := ByID(ID_Prm);
  if p <> nil then
  begin
    d := GMDevices.DeviceByID(p.Device.ID_Device);
    if d <> nil then
      Result := d.Obj.ID_Obj;
  end;
end;

{ TGMParam }

constructor TGMParam.Create();
begin
  inherited Create();

  Device := nil;
  ID_Prm := 0;
  ID_Src := 0;
  N_Src := 0;
  MeaUnitName := '';
end;

function TGMParam.GetDataChannelType: int;
begin
  Result := DATACHANNEL_PARAM;
end;

function TGMParam.GetIsMeterNI: bool;
begin
  Result := ID_Src = SRC_CNT_MTR;
end;

function TGMParam.N_SrcString(): string;
begin
  if (Device.ID_DevType in Tecon19_Family) and (ID_Src = SRC_USR) then
    Result := IntToHex(N_Src, 4)
  else
    Result := IntToStr(N_Src);
end;

function TGMParam.GetNameWithPath(PathComponents: TParamPathComponents = [ppcID, ppcObj, ppcDev]; Separator: string = ' . '): string;
begin
  if bProMode then
    Include(PathComponents, ppcID);

  Result := '';
  if (BaseChn <> nil) and not (ppcNoBase in PathComponents) then
  begin
    Result := BaseChn.GetNameWithPath(PathComponents, Separator) + Separator;
    PathComponents := PathComponents - [ppcObj, ppcDev];
  end;

  Result := Result + IfThen(ppcID in PathComponents, '[ID_Prm = ' + IntToStr(ID_Prm) + '] ');

  if ppcSrc in PathComponents then
    Result := Result + ID_SrcToStr(ID_Src) + N_SrcString() + Separator;

  if ppcObj in PathComponents then
    Result := Result + Device.Obj.Name + Separator;

  if (ppcDev in PathComponents)
     or ( (ppcDevNoGM in PathComponents) and not (Device.ID_DevType in GeomerFamily) )then
    Result := Result +
               Device.Name +
               IfThen(not (Device.ID_DevType in GeomerFamily), '(' + IntToStr(Device.Number) + ')') +
               Separator;

  if Trim(PrmName) <> '' then
    Result := Result + PrmName + IfThen(ppcPT in PathComponents, ' (' + PType.ShowName + ')')
  else
    Result := Result + PType.ShowName;
end;

procedure TGMParam.SetID_Prm(const Value: int);
begin
  FID_Prm := Value;
  // LastVal.ID_Prm := FID_Prm;
end;

{ TGMObject }

constructor TGMObject.Create(Collection: TCollection);
begin
  inherited;

  Active := false;
  ID_Obj := 0;
end;

{ TGMAggregate }

constructor TGMAggregate.Create(Collection: TCollection);
begin
  inherited;
  FParamList := TList.Create();
end;

destructor TGMAggregate.Destroy;
var i: int;
begin
  for i := 0 to FParamList.Count - 1 do
    TObject(FParamList[i]).Free();
    
  FParamList.Free();
  inherited;
end;

function TGMAggregate.GetAggrName: string;
begin
  Result := FAggrName;
  if Trim(Result) = '' then
    case AggrType of
      AGGREGATE_TYPE_ENGINE: Result := 'Двигатель';
      AGGREGATE_TYPE_VALVE: Result := 'Задвижка';
      AGGREGATE_TYPE_HEAT: Result := 'Отопитель';
    end;
end;

function TGMAggregate.GetParam(Index: int): TGMAggregateParam;
begin
  Result := TGMAggregateParam(FParamList[Index]);
end;

function TGMAggregate.GetParamCount: int;
begin
  Result := FParamList.Count;
end;

function TGMAggregate.ParamByKind(PrmKind: int): TGMAggregateParam;
var i: int;
begin
  Result := nil;
  for i := 0 to ParamCount - 1 do
    if Params[i].PrmKind = PrmKind then
    begin
      Result := Params[i];
      Exit;
    end;
end;

{ TGMAggregateList }

function TGMAggregateList.Add: TGMAggregate;
begin
  Result := TGMAggregate(Inherited Add());
end;

function TGMAggregateList.AggregateByID(ID_Aggregate: int): TGMAggregate;
var i: int;
begin
  Result := nil;
  if ID_Aggregate > 0 then
    for i := 0 to Count - 1 do
      if Aggregates[i].ID_Aggregate = ID_Aggregate then
      begin
        Result := Aggregates[i];
        Exit;
      end;
end;

constructor TGMAggregateList.Create;
begin
  inherited Create(TGMAggregate);
end;

function TGMAggregateList.GetAggregate(Index: int): TGMAggregate;
begin
  Result := TGMAggregate(Items[Index]);
end;

function TGMAggregateList.IsObjectControllable(ID_Obj: int): bool;
var i: int;
begin
  Result := false;
  for i := 0 to Count - 1 do
    if Aggregates[i].Obj.ID_Obj = ID_Obj then
    begin
      Result := true;
      Exit;
    end;
end;

{ TGMDevice }

function TGMDevice.GetName(): string;
begin
  if DevName <> '' then
    Result := DevName
  else
    Result := TypeName + IfThen(Number > 0, ' (' + IntToStr(Number) + ')');
end;

{ TGMNodeList }

function TGMNodeList.NodeById(ID_Node: int): TGMNode;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].ID_Node = ID_Node then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
end;

{ TGMNodeChannel }

function TGMNodeChannel.GetCaption: string;
begin
  Result := Trim(Name);
  if Result = '' then
  begin
    if PType <> nil then
      Result := PType.ShowName
    else
    if BaseChn <> nil then
      Result := BaseChn.GetNameWithPath([ppcObj, ppcDev]);
  end;
end;

function TGMNodeChannel.GetDataChannelType: int;
begin
  Result := DATACHANNEL_NODECHANNEL;
end;

function TGMNodeChannel.GetIsMeterNI: bool;
begin
  Result := (BaseChn <> nil) and BaseChn.IsMeterNI;
end;

function TGMNodeChannel.GetNameWithPath(PathComponents: TParamPathComponents; Separator: string): string;
begin
  Result := Caption;
end;

{ TGMBaseParam }

constructor TGMBaseParam.Create;
begin
  inherited;

  LastVal.Chn := self;
end;

function TGMBaseParam.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj <> nil)
            and (Obj is TGMBaseParam)
            and (TGMBaseParam(Obj).ID_Prm = ID_Prm)
            and (TGMBaseParam(Obj).DataChannelType = DataChannelType);
end;

function TGMBaseParam.GetIsAlarm: bool;
begin
  Result := FPType.IsAlarm or (AlarmSignal > 0);
end;

procedure TGMBaseParam.SetID_Prm(const Value: int);
begin
  FID_Prm := Value;
end;

function TGMBaseParam.UpdateVal(UTime: LongWord; Val: double): bool;
begin
  if (UTime <> LastVal.UTime) or (Val <> LastVal.Val) then
  begin
    Result := true;
    LastVal.UTime := UTime;
    LastVal.Val := Val;
  end
  else
    Result := false;
end;

{ TGMNode }

constructor TGMNode.Create;
begin
  inherited;

  ID_Node := 0;
  ID_Parent := 0;
  parent := nil;
  NodeType := 0;
  Name := '';
end;

{ TGMBaseParamList<T> }

function TGMBaseParamList<T>.ByID(ID_Prm: int): T;
var i: int;
begin
  Result := nil;
  if ID_Prm <= 0 then Exit;

  for i := 0 to Count - 1 do
    if Items[i].ID_Prm = ID_Prm then
    begin
      Result := Items[i];
      Exit;
    end;
end;

function TGMBaseParamList<T>.IndexOf(prm: TGMBaseParam): int;
var i: int;
begin
  for i := 0 to Count - 1 do
    if Items[i].Equals(prm) then
    begin
      Result := i;
      Exit;
    end;

  Result := -1;
end;

{ TGMDataChannelList }

procedure TGMDataChannelList.AddUnique(prm: TGMBaseParam);
var p: TGMBaseParam;
begin
  if prm = nil then Exit;

  for p in self do
  begin
    if p.Equals(prm) then Exit;
  end;

  Add(prm);
end;

initialization
  GMParamTypes := TGMParamTypes.Create(TGMParamType);
  GMParamTypes.Init();

  GMObjects := TGMObjectList.Create(TGMObject);
  GMDevices := TGMDeviceList.Create(TGMDevice);
  GMParams := TGMParamList.Create();
  GMAggregates := TGMAggregateList.Create();

  GMNodes := TGMNodeList.Create();
  GMNodeChannels := TGMNodeChannelList.Create();

finalization
  GMParamTypes.Free();
  GMObjects.Free();
  GMDevices.Free();
  GMParams.Free();
  GMAggregates.Free();
  GMNodes.Free();
  GMNodeChannels.Free();
end.
