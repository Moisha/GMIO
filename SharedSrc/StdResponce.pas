unit StdResponce;

interface

uses xmldom, XMLDoc, XMLIntf, Variants, Classes, SysUtils, StdRequest;

type

{ Forward Decls }

  IXMLGMIOResponceType = interface;
  IXMLAuthType = interface;
  IXMLObjectsType = interface;
  IXMLObjectType = interface;
  IXMLDevicesType = interface;
  IXMLDeviceType = interface;
  IXMLChannelsType = interface;
  IXMLChannelType = interface;
  IXMLAggregatesType = interface;
  IXMLAggregateType = interface;
  IXMLAggregateParamsType = interface;
  IXMLAggregateParamType = interface;
  IXMLParamTypesType = interface;
  IXMLParamType = interface;
  IXMLMeaUnitsType = interface;
  IXMLMeaUnitType = interface;
  IXMLDataType = interface;
  IXMLSourceType = interface;
  IXMLValueType = interface;
  IXMLSQLType = interface;
  IXMLRowsType = interface;
  IXMLRowType = interface;
  IXMLFieldsType = interface;
  IXMLFieldType = interface;
  IXMLDeviceType2 = interface;
  IXMLAggregateType2 = interface;
  IXMLObjectType2 = interface;
  IXMLSourceType2 = interface;
  IXMLRowType2 = interface;
  IXMLNodesType = interface;
  IXMLNodeType = interface;
  IXMLNodeChannelsType = interface;
  IXMLNodeChannelType = interface;
  IXMLCommandsType = interface;
  IXMLCommandType = interface;

{ IXMLGMIOResponceType }

  IXMLGMIOResponceType = interface(IXMLNode)
    ['{9137F4A4-82E4-48CB-8332-6C0C6EADDCBB}']
    { Property Accessors }
    function Get_Auth: IXMLAuthType;
    function Get_Objects: IXMLObjectsType;
    function Get_ParamTypes: IXMLParamTypesType;
    function Get_MeaUnits: IXMLMeaUnitsType;
    function Get_Data: IXMLDataType;
    function Get_SQL: IXMLSQLType;
    function Get_Nodes: IXMLNodesType;
    function Get_State: IXMLStateType;
    function Get_Commands: IXMLCommandsType;
    function Get_Comment: UnicodeString;
    procedure Set_Comment(Value: UnicodeString);
    { Methods & Properties }
    property Comment: UnicodeString read Get_Comment write Set_Comment;
    property Auth: IXMLAuthType read Get_Auth;
    property Objects: IXMLObjectsType read Get_Objects;
    property ParamTypes: IXMLParamTypesType read Get_ParamTypes;
    property MeaUnits: IXMLMeaUnitsType read Get_MeaUnits;
    property Data: IXMLDataType read Get_Data;
    property SQL: IXMLSQLType read Get_SQL;
    property Nodes: IXMLNodesType read Get_Nodes;
    property State: IXMLStateType read Get_State;
    property Commands: IXMLCommandsType read Get_Commands;
  end;

{ IXMLAuthType }

  IXMLAuthType = interface(IXMLNode)
    ['{D20A5CF6-E21E-43E2-9B02-C4BB29E120CD}']
    { Property Accessors }
    function Get_OK: Integer;
    function Get_RemoteName: UnicodeString;
    function Get_RequestID: UnicodeString;
    procedure Set_RequestID(Value: UnicodeString);
    procedure Set_RemoteName(Value: UnicodeString);
    procedure Set_OK(Value: Integer);
    { Methods & Properties }
    property OK: Integer read Get_OK write Set_OK;
    property RemoteName: UnicodeString read Get_RemoteName write Set_RemoteName;
    property RequestID: UnicodeString read Get_RequestID write Set_RequestID;
  end;

{ IXMLObjectsType }

  IXMLObjectsType = interface(IXMLNodeCollection)
    ['{6AC94114-75BB-4404-8110-277B6D98EFAC}']
    { Property Accessors }
    function Get_Object_(Index: Integer): IXMLObjectType;
    { Methods & Properties }
    function Add: IXMLObjectType;
    function Insert(const Index: Integer): IXMLObjectType;
    property Object_[Index: Integer]: IXMLObjectType read Get_Object_; default;

    function ObjectById(id_obj: Integer): IXMLObjectType;
    procedure SQL_CreateOrUpdateObjects(sql: TStringList; remoteName: UnicodeString);
  end;

{ IXMLObjectType }

  IXMLObjectType = interface(IXMLNode)
    ['{5E2FD2AF-A6D2-4D1A-ADC1-D8D9A83DDE97}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_Name: UnicodeString;
    function Get_Ncar: Integer;
    function Get_Devices: IXMLDevicesType;
    function Get_Aggregates: IXMLAggregatesType;
    function Get_ObjType: Integer;
    procedure Set_ObjType(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_Ncar(Value: Integer);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property ObjType: Integer read Get_ObjType write Set_ObjType;
    property Name: UnicodeString read Get_Name write Set_Name;
    property Ncar: Integer read Get_Ncar write Set_Ncar;
    property Devices: IXMLDevicesType read Get_Devices;
    property Aggregates: IXMLAggregatesType read Get_Aggregates;

    procedure SQL_CreateOrUpdateObject(sql: TStringList; remoteName: UnicodeString);
  end;

{ IXMLDevicesType }

  IXMLDevicesType = interface(IXMLNodeCollection)
    ['{F042A48A-BDFA-4D7D-B094-32A9110769F2}']
    { Property Accessors }
    function Get_Device(Index: Integer): IXMLDeviceType;
    { Methods & Properties }
    function Add: IXMLDeviceType;
    function Insert(const Index: Integer): IXMLDeviceType;
    property Device[Index: Integer]: IXMLDeviceType read Get_Device; default;
    function DeviceById(id_device: Integer): IXMLDeviceType;
    procedure SQL_CreateOrUpdateDevices(sql: TStringList; remoteName: UnicodeString; remoteID_Obj: integer);
  end;

{ IXMLDeviceType }

  IXMLDeviceType = interface(IXMLNode)
    ['{788CFAEE-B9E4-4D34-B0E5-312F7DCC7C2A}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_Name: UnicodeString;
    function Get_ID_DevType: Integer;
    function Get_DevType: UnicodeString;
    function Get_Channels: IXMLChannelsType;
    function Get_Number: Integer;
    procedure Set_Number(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_ID_DevType(Value: Integer);
    procedure Set_DevType(Value: UnicodeString);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property Name: UnicodeString read Get_Name write Set_Name;
    property ID_DevType: Integer read Get_ID_DevType write Set_ID_DevType;
    property DevType: UnicodeString read Get_DevType write Set_DevType;
    property Channels: IXMLChannelsType read Get_Channels;
    property Number: Integer read Get_Number write Set_Number;
    procedure SQL_CreateOrUpdateDevice(sql: TStringList; remoteName: UnicodeString; remoteID_Obj: integer);
  end;

{ IXMLChannelsType }

  IXMLChannelsType = interface(IXMLNodeCollection)
    ['{D86646C6-4B2D-461E-8C45-04C6BE6EEA13}']
    { Property Accessors }
    function Get_Channel(Index: Integer): IXMLChannelType;
    { Methods & Properties }
    function Add: IXMLChannelType;
    function Insert(const Index: Integer): IXMLChannelType;
    property Channel[Index: Integer]: IXMLChannelType read Get_Channel; default;
    procedure SQL_CreateOrUpdateChannels(sql: TStringList; remoteName: UnicodeString; remoteID_Device: integer);
  end;

{ IXMLChannelType }

  IXMLChannelType = interface(IXMLNode)
    ['{2E134A56-0838-462B-8F89-52F5A56E95A9}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_Name: UnicodeString;
    function Get_ID_PT: Integer;
    function Get_ID_Src: Integer;
    function Get_Nsrc: Integer;
    function Get_AlarmSignal: Integer;
    function Get_AlarmThreshold: UnicodeString;
    function Get_BaseChn: Integer;
    function Get_DevMin: UnicodeString;
    function Get_DevMax: UnicodeString;
    function Get_RealMin: UnicodeString;
    function Get_RealMax: UnicodeString;
    function Get_ID_MeaUnit: integer;
    function Get_UserSrc: integer;
    procedure Set_UserSrc(Value: integer);
    procedure Set_ID_MeaUnit(Value: integer);
    procedure Set_DevMin(Value: UnicodeString);
    procedure Set_DevMax(Value: UnicodeString);
    procedure Set_RealMin(Value: UnicodeString);
    procedure Set_RealMax(Value: UnicodeString);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_ID_PT(Value: Integer);
    procedure Set_ID_Src(Value: Integer);
    procedure Set_Nsrc(Value: Integer);
    procedure Set_AlarmSignal(Value: Integer);
    procedure Set_AlarmThreshold(Value: UnicodeString);
    procedure Set_BaseChn(Value: Integer);
    function Get_CalibrType: integer;
    function Get_MeterCalcNIType: integer;
    function Get_NI_StartVal: UnicodeString;
    function Get_NI_StartDT: int64;
    function Get_ReqIntervalType: integer;
    function Get_Resistor: integer;
    procedure Set_CalibrType(const Value: integer);
    procedure Set_MeterCalcNIType(const Value: integer);
    procedure Set_NI_StartVal(const Value: UnicodeString);
    procedure Set_NI_StartDT(const Value: int64);
    procedure Set_ReqIntervalType(const Value: integer);
    procedure Set_Resistor(const Value: integer);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property Name: UnicodeString read Get_Name write Set_Name;
    property ID_PT: Integer read Get_ID_PT write Set_ID_PT;
    property ID_Src: Integer read Get_ID_Src write Set_ID_Src;
    property UserSrc: Integer read Get_UserSrc write Set_UserSrc;
    property Nsrc: Integer read Get_Nsrc write Set_Nsrc;
    property AlarmSignal: Integer read Get_AlarmSignal write Set_AlarmSignal;
    property AlarmThreshold: UnicodeString read Get_AlarmThreshold write Set_AlarmThreshold;
    property BaseChn: Integer read Get_BaseChn write Set_BaseChn;
    property DevMin: UnicodeString read Get_DevMin write Set_DevMin;
    property DevMax: UnicodeString read Get_DevMax write Set_DevMax;
    property RealMin: UnicodeString read Get_RealMin write Set_RealMin;
    property RealMax: UnicodeString read Get_RealMax write Set_RealMax;
    property ID_MeaUnit: integer read Get_ID_MeaUnit write Set_ID_MeaUnit;
    property NI_StartDT: int64 read Get_NI_StartDT write Set_NI_StartDT;
    property NI_StartVal: UnicodeString read Get_NI_StartVal write Set_NI_StartVal;
    property CalibrType: integer read Get_CalibrType write Set_CalibrType;
    property Resistor: integer read Get_Resistor write Set_Resistor;
    property MeterCalcNIType: integer read Get_MeterCalcNIType write Set_MeterCalcNIType;
    property ReqIntervalType: integer read Get_ReqIntervalType write Set_ReqIntervalType;

    procedure SQL_CreateOrUpdateChannel(sql: TStringList; remoteName: UnicodeString; remoteID_Device: integer);
  end;

{ IXMLNodesType }

  IXMLNodesType = interface(IXMLNodeCollection)
    ['{0DA32B3A-EA78-496D-8340-F3E2340C7D25}']
    { Property Accessors }
    function Get_Node(Index: Integer): IXMLNodeType;
    { Methods & Properties }
    function Add: IXMLNodeType;
    function Insert(const Index: Integer): IXMLNodeType;
    property Node[Index: Integer]: IXMLNodeType read Get_Node; default;
    function NodeById(ID_Node: Integer): IXMLNodeType;
  end;

{ IXMLNodeType }

  IXMLNodeType = interface(IXMLNode)
    ['{1342D300-6812-478A-B206-19622FEA07D7}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_Name: string;
    function Get_NodeType: Integer;
    function Get_Nodes: IXMLNodesType;
    function Get_NodeChannels: IXMLNodeChannelsType;
    function Get_Parent: Integer;
    procedure Set_Parent(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: string);
    procedure Set_NodeType(Value: Integer);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property Parent: Integer read Get_Parent write Set_Parent;
    property Name: string read Get_Name write Set_Name;
    property NodeType: Integer read Get_NodeType write Set_NodeType;
    property Nodes: IXMLNodesType read Get_Nodes;
    property NodeChannels: IXMLNodeChannelsType read Get_NodeChannels;
  end;

{ IXMLNodeChannelsType }

  IXMLNodeChannelsType = interface(IXMLNodeCollection)
    ['{AE01EBD8-AAFC-4F55-8006-626AB05624D1}']
    { Property Accessors }
    function Get_NodeChannel(Index: Integer): IXMLNodeChannelType;
    { Methods & Properties }
    function Add: IXMLNodeChannelType;
    function Insert(const Index: Integer): IXMLNodeChannelType;
    property NodeChannel[Index: Integer]: IXMLNodeChannelType read Get_NodeChannel; default;
  end;

{ IXMLNodeChannelType }

  IXMLNodeChannelType = interface(IXMLNode)
    ['{7EA2DCD0-60C7-4CC2-84C1-244DEED6887C}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_ID_PT: Integer;
    function Get_Name: string;
    function Get_BaseChn: Integer;
    function Get_ID_MeaUnit: Integer;
    procedure Set_Id(Value: Integer);
    procedure Set_ID_PT(Value: Integer);
    procedure Set_Name(Value: string);
    procedure Set_BaseChn(Value: Integer);
    procedure Set_ID_MeaUnit(Value: Integer);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property ID_PT: Integer read Get_ID_PT write Set_ID_PT;
    property Name: string read Get_Name write Set_Name;
    property BaseChn: Integer read Get_BaseChn write Set_BaseChn;
    property ID_MeaUnit: Integer read Get_ID_MeaUnit write Set_ID_MeaUnit;
  end;

{ IXMLAggregatesType }

  IXMLAggregatesType = interface(IXMLNodeCollection)
    ['{CAEB7EFA-2F94-41DA-AAF9-531AAFB6B542}']
    { Property Accessors }
    function Get_Aggregate(Index: Integer): IXMLAggregateType;
    { Methods & Properties }
    function Add: IXMLAggregateType;
    function Insert(const Index: Integer): IXMLAggregateType;
    property Aggregate[Index: Integer]: IXMLAggregateType read Get_Aggregate; default;
    function AggregateById(id_aggr: Integer): IXMLAggregateType;
  end;

{ IXMLAggregateType }

  IXMLAggregateType = interface(IXMLNode)
    ['{CAA05766-9AF7-4033-95F8-A07812D9BC6F}']
    { Property Accessors }
    function Get_ID_AggrType: Integer;
    function Get_Name: UnicodeString;
    function Get_AggregateParams: IXMLAggregateParamsType;
    function Get_Id: Integer;
    procedure Set_ID_AggrType(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_Id(Value: Integer);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property ID_AggrType: Integer read Get_ID_AggrType write Set_ID_AggrType;
    property Name: UnicodeString read Get_Name write Set_Name;
    property AggregateParams: IXMLAggregateParamsType read Get_AggregateParams;
  end;

{ IXMLAggregateParamsType }

  IXMLAggregateParamsType = interface(IXMLNodeCollection)
    ['{FC7E6244-4C31-4FDE-82E5-CA8C0C6FD222}']
    { Property Accessors }
    function Get_AggregateParam(Index: Integer): IXMLAggregateParamType;
    { Methods & Properties }
    function Add: IXMLAggregateParamType;
    function Insert(const Index: Integer): IXMLAggregateParamType;
    property AggregateParam[Index: Integer]: IXMLAggregateParamType read Get_AggregateParam; default;
  end;

{ IXMLAggregateParamType }

  IXMLAggregateParamType = interface(IXMLNode)
    ['{F736D5D5-EDCE-4FB1-96EB-3494127B6B8F}']
    { Property Accessors }
    function Get_ID_Prm: Integer;
    function Get_PrmKind: Integer;
    function Get_Caption: UnicodeString;
    procedure Set_ID_Prm(Value: Integer);
    procedure Set_PrmKind(Value: Integer);
    procedure Set_Caption(Value: UnicodeString);
    { Methods & Properties }
    property ID_Prm: Integer read Get_ID_Prm write Set_ID_Prm;
    property PrmKind: Integer read Get_PrmKind write Set_PrmKind;
    property Caption: UnicodeString read Get_Caption write Set_Caption;
  end;

{ IXMLParamTypesType }

  IXMLParamTypesType = interface(IXMLNodeCollection)
    ['{FC0811DE-2E49-480A-8A95-47FCC77C44D4}']
    { Property Accessors }
    function Get_ParamType(Index: Integer): IXMLParamType;
    { Methods & Properties }
    function Add: IXMLParamType;
    function Insert(const Index: Integer): IXMLParamType;
    property ParamType[Index: Integer]: IXMLParamType read Get_ParamType; default;
  end;

{ IXMLParamType }

  IXMLParamType = interface(IXMLNode)
    ['{7C697463-B4FA-46B4-BE30-27061D4E3092}']
    { Property Accessors }
    function Get_ID_PT: Integer;
    function Get_PSign: UnicodeString;
    function Get_PName: UnicodeString;
    function Get_IsAlarm: Integer;
    procedure Set_ID_PT(Value: Integer);
    procedure Set_PSign(Value: UnicodeString);
    procedure Set_PName(Value: UnicodeString);
    procedure Set_IsAlarm(Value: Integer);
    { Methods & Properties }
    property ID_PT: Integer read Get_ID_PT write Set_ID_PT;
    property PSign: UnicodeString read Get_PSign write Set_PSign;
    property PName: UnicodeString read Get_PName write Set_PName;
    property IsAlarm: Integer read Get_IsAlarm write Set_IsAlarm;
  end;

{ IXMLMeaUnitsType }

  IXMLMeaUnitsType = interface(IXMLNodeCollection)
    ['{93E7E58B-5396-464D-AD17-A6C0897CA141}']
    { Property Accessors }
    function Get_MeaUnit(Index: Integer): IXMLMeaUnitType;
    { Methods & Properties }
    function Add: IXMLMeaUnitType;
    function Insert(const Index: Integer): IXMLMeaUnitType;
    property MeaUnit[Index: Integer]: IXMLMeaUnitType read Get_MeaUnit; default;
  end;

{ IXMLMeaUnitType }

  IXMLMeaUnitType = interface(IXMLNode)
    ['{ADD9D6E0-950D-4A60-B3E7-F25FB48A57BF}']
    { Property Accessors }
    function Get_ID_MeaUnit: Integer;
    function Get_Name: UnicodeString;
    function Get_LongName: UnicodeString;
    procedure Set_ID_MeaUnit(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_LongName(Value: UnicodeString);
    { Methods & Properties }
    property ID_MeaUnit: Integer read Get_ID_MeaUnit write Set_ID_MeaUnit;
    property Name: UnicodeString read Get_Name write Set_Name;
    property LongName: UnicodeString read Get_LongName write Set_LongName;
  end;

{ IXMLDataType }

  IXMLDataType = interface(IXMLNodeCollection)
    ['{5AE461B3-C3CB-4FAF-8E7F-F1B33F586709}']
    { Property Accessors }
    function Get_Source(Index: Integer): IXMLSourceType;
    { Methods & Properties }
    function Add: IXMLSourceType;
    function Insert(const Index: Integer): IXMLSourceType;
    property Source[Index: Integer]: IXMLSourceType read Get_Source; default;

    procedure SQL_InsertData(sql: TStringList; remoteName: UnicodeString);
  end;

{ IXMLSourceType }

  IXMLSourceType = interface(IXMLNodeCollection)
    ['{5374AAB2-D883-40E4-A8FA-FD8A71A22040}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_ValSrcType: Integer;
    function Get_ValType: Integer;
    function Get_Name: UnicodeString;
    function Get_Value(Index: Integer): IXMLValueType;
    procedure Set_Id(Value: Integer);
    procedure Set_ValSrcType(Value: Integer);
    procedure Set_ValType(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    { Methods & Properties }
    function Add: IXMLValueType;
    function Insert(const Index: Integer): IXMLValueType;
    property Id: Integer read Get_Id write Set_Id;
    property ValSrcType: Integer read Get_ValSrcType write Set_ValSrcType;
    property ValType: Integer read Get_ValType write Set_ValType;
    property Name: UnicodeString read Get_Name write Set_Name;
    property Value[Index: Integer]: IXMLValueType read Get_Value; default;

    procedure SQL_InsertData(sql: TStringList; remoteName: UnicodeString);
  end;

{ IXMLValueType }

  IXMLValueType = interface(IXMLNode)
    ['{32406424-4B29-439E-A617-CB379A124812}']
    { Property Accessors }
    function Get_Utime: Integer;
    function Get_Val: Double;
    function Get_Id: Integer;
    procedure Set_Id(Value: Integer);
    procedure Set_Utime(Value: Integer);
    procedure Set_Val(Value: Double);
    { Methods & Properties }
    property Utime: Integer read Get_Utime write Set_Utime;
    property Val: Double read Get_Val write Set_Val;
    property Id: Integer read Get_Id write Set_Id;
  end;

{ IXMLSQLType }

  IXMLSQLType = interface(IXMLNode)
    ['{C6E747E2-E707-45AE-A775-ADAB3B510F19}']
    { Property Accessors }
    function Get_Rows: IXMLRowsType;
    { Methods & Properties }
    property Rows: IXMLRowsType read Get_Rows;
  end;

{ IXMLRowsType }

  IXMLRowsType = interface(IXMLNodeCollection)
    ['{418395BA-678C-45B3-B736-DE3C3E7A264A}']
    { Property Accessors }
    function Get_Row(Index: Integer): IXMLRowType;
    { Methods & Properties }
    function Add: IXMLRowType;
    function Insert(const Index: Integer): IXMLRowType;
    property Row[Index: Integer]: IXMLRowType read Get_Row; default;
  end;

{ IXMLCommandsType }

  IXMLCommandsType = interface(IXMLNodeCollection)
    ['{9CAFABA5-0A04-487E-A9D4-1D25D8AD3F0E}']
    { Property Accessors }
    function Get_Command(Index: Integer): IXMLCommandType;
    { Methods & Properties }
    function Add: IXMLCommandType;
    function Insert(const Index: Integer): IXMLCommandType;
    property Command[Index: Integer]: IXMLCommandType read Get_Command; default;
  end;

{ IXMLCommandType }

  IXMLCommandType = interface(IXMLNode)
    ['{F93536ED-F6BE-4FB1-A6AA-266C41B0F8AD}']
    { Property Accessors }
    function Get_ID_Prm: Integer;
    function Get_Val: Double;
    function Get_Utime: Integer;
    procedure Set_ID_Prm(Value: Integer);
    procedure Set_Utime(Value: Integer);
    procedure Set_Val(Value: Double);
    { Methods & Properties }
    property ID_Prm: Integer read Get_ID_Prm write Set_ID_Prm;
    property Utime: Integer read Get_Utime write Set_Utime;
    property Val: Double read Get_Val write Set_Val;
  end;

{ IXMLRowType }

  IXMLRowType = interface(IXMLNode)
    ['{2B0BA265-054F-4F13-AB1E-B48893C3D1E5}']
    { Property Accessors }
    function Get_Fields: IXMLFieldsType;
    { Methods & Properties }
    property Fields: IXMLFieldsType read Get_Fields;
  end;

{ IXMLFieldsType }

  IXMLFieldsType = interface(IXMLNode)
    ['{74C482A3-C1EA-4CB0-83F7-0355BE3C147B}']
    { Property Accessors }
    function Get_Field: IXMLFieldType;
    { Methods & Properties }
    property Field: IXMLFieldType read Get_Field;
  end;

{ IXMLFieldType }

  IXMLFieldType = interface(IXMLNode)
    ['{640C723D-FFB4-4782-BE3B-061B18171791}']
    { Property Accessors }
    function Get_Value: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    { Methods & Properties }
    property Value: UnicodeString read Get_Value write Set_Value;
  end;

{ IXMLDeviceType2 }

  IXMLDeviceType2 = interface(IXMLNode)
    ['{E248ACF2-EE50-4BE4-9D28-982AB4F18E3D}']
  end;

{ IXMLAggregateType2 }

  IXMLAggregateType2 = interface(IXMLNode)
    ['{CFDC36FD-E23A-4366-821A-77094070779F}']
  end;

{ IXMLObjectType2 }

  IXMLObjectType2 = interface(IXMLNode)
    ['{F3C74C8D-847C-4EC9-B176-3902876590D8}']
  end;

{ IXMLSourceType2 }

  IXMLSourceType2 = interface(IXMLNode)
    ['{21CBB63E-9229-4198-AA63-6CF85DA48113}']
  end;

{ IXMLRowType2 }

  IXMLRowType2 = interface(IXMLNode)
    ['{999CB6EE-E468-4C16-BDC3-F65CF3EE741F}']
  end;

{ Forward Decls }

  TXMLGMIOResponceType = class;
  TXMLAuthType = class;
  TXMLObjectsType = class;
  TXMLObjectType = class;
  TXMLDevicesType = class;
  TXMLDeviceType = class;
  TXMLChannelsType = class;
  TXMLChannelType = class;
  TXMLAggregatesType = class;
  TXMLAggregateType = class;
  TXMLAggregateParamsType = class;
  TXMLAggregateParamType = class;
  TXMLParamTypesType = class;
  TXMLParamType = class;
  TXMLMeaUnitsType = class;
  TXMLMeaUnitType = class;
  TXMLDataType = class;
  TXMLSourceType = class;
  TXMLValueType = class;
  TXMLSQLType = class;
  TXMLRowsType = class;
  TXMLRowType = class;
  TXMLFieldsType = class;
  TXMLFieldType = class;
  TXMLDeviceType2 = class;
  TXMLAggregateType2 = class;
  TXMLObjectType2 = class;
  TXMLSourceType2 = class;
  TXMLRowType2 = class;

{ TXMLGMIOResponceType }

  TXMLGMIOResponceType = class(TXMLNode, IXMLGMIOResponceType)
  protected
    { IXMLGMIOResponceType }
    function Get_Auth: IXMLAuthType;
    function Get_Objects: IXMLObjectsType;
    function Get_ParamTypes: IXMLParamTypesType;
    function Get_MeaUnits: IXMLMeaUnitsType;
    function Get_Data: IXMLDataType;
    function Get_SQL: IXMLSQLType;
    function Get_Nodes: IXMLNodesType;
    function Get_State: IXMLStateType;
    function Get_Commands: IXMLCommandsType;
    function Get_Comment: UnicodeString;
    procedure Set_Comment(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAuthType }

  TXMLAuthType = class(TXMLNode, IXMLAuthType)
  protected
    { IXMLAuthType }
    function Get_OK: Integer;
    function Get_RemoteName: UnicodeString;
    function Get_RequestID: UnicodeString;
    procedure Set_RequestID(Value: UnicodeString);
    procedure Set_RemoteName(Value: UnicodeString);
    procedure Set_OK(Value: Integer);
  end;

{ TXMLObjectsType }

  TXMLObjectsType = class(TXMLNodeCollection, IXMLObjectsType)
  protected
    { IXMLObjectsType }
    function Get_Object_(Index: Integer): IXMLObjectType;
    function Add: IXMLObjectType;
    function Insert(const Index: Integer): IXMLObjectType;
  public
    procedure AfterConstruction; override;

    function ObjectById(id_obj: Integer): IXMLObjectType;
    procedure SQL_CreateOrUpdateObjects(sql: TStringList; remoteName: UnicodeString);
  end;

{ TXMLObjectType }

  TXMLObjectType = class(TXMLNode, IXMLObjectType)
  protected
    { IXMLObjectType }
    function Get_Id: Integer;
    function Get_Name: UnicodeString;
    function Get_Ncar: Integer;
    function Get_Devices: IXMLDevicesType;
    function Get_Aggregates: IXMLAggregatesType;
    function Get_UserData: UnicodeString;
    function Get_ObjType: Integer;
    procedure Set_ObjType(Value: Integer);
    procedure Set_UserData(Value: UnicodeString);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_Ncar(Value: Integer);
  public
    procedure AfterConstruction; override;
    procedure SQL_CreateOrUpdateObject(sql: TStringList; remoteName: UnicodeString);
  end;

{ TXMLDevicesType }

  TXMLDevicesType = class(TXMLNodeCollection, IXMLDevicesType)
  protected
    { IXMLDevicesType }
    function Get_Device(Index: Integer): IXMLDeviceType;
    function Add: IXMLDeviceType;
    function Insert(const Index: Integer): IXMLDeviceType;
  public
    procedure AfterConstruction; override;
    function DeviceById(id_device: Integer): IXMLDeviceType;
    procedure SQL_CreateOrUpdateDevices(sql: TStringList; remoteName: UnicodeString; remoteID_Obj: integer);
  end;

{ TXMLDeviceType }

  TXMLDeviceType = class(TXMLNode, IXMLDeviceType)
  protected
    { IXMLDeviceType }
    function Get_Id: Integer;
    function Get_Name: UnicodeString;
    function Get_ID_DevType: Integer;
    function Get_DevType: UnicodeString;
    function Get_Channels: IXMLChannelsType;
    function Get_Number: Integer;
    procedure Set_Number(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_ID_DevType(Value: Integer);
    procedure Set_DevType(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
    procedure SQL_CreateOrUpdateDevice(sql: TStringList; remoteName: UnicodeString; remoteID_Obj: integer);
  end;

{ TXMLChannelsType }

  TXMLChannelsType = class(TXMLNodeCollection, IXMLChannelsType)
  protected
    { IXMLChannelsType }
    function Get_Channel(Index: Integer): IXMLChannelType;
    function Add: IXMLChannelType;
    function Insert(const Index: Integer): IXMLChannelType;
  public
    procedure AfterConstruction; override;
    procedure SQL_CreateOrUpdateChannels(sql: TStringList; remoteName: UnicodeString; remoteID_Device: integer);
  end;

{ TXMLChannelType }

  TXMLChannelType = class(TXMLNode, IXMLChannelType)
  protected
    { IXMLChannelType }
    function Get_Id: Integer;
    function Get_Name: UnicodeString;
    function Get_ID_PT: Integer;
    function Get_ID_Src: Integer;
    function Get_Nsrc: Integer;
    function Get_AlarmSignal: Integer;
    function Get_AlarmThreshold: UnicodeString;
    function Get_BaseChn: Integer;
    function Get_DevMin: UnicodeString;
    function Get_DevMax: UnicodeString;
    function Get_RealMin: UnicodeString;
    function Get_RealMax: UnicodeString;
    function Get_UserSrc: integer;
    procedure Set_UserSrc(Value: integer);
    procedure Set_DevMin(Value: UnicodeString);
    procedure Set_DevMax(Value: UnicodeString);
    procedure Set_RealMin(Value: UnicodeString);
    procedure Set_RealMax(Value: UnicodeString);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_ID_PT(Value: Integer);
    procedure Set_ID_Src(Value: Integer);
    procedure Set_Nsrc(Value: Integer);
    procedure Set_AlarmSignal(Value: Integer);
    procedure Set_AlarmThreshold(Value: UnicodeString);
    procedure Set_BaseChn(Value: Integer);
    function Get_CalibrType: integer;
    function Get_MeterCalcNIType: integer;
    function Get_NI_StartVal: UnicodeString;
    function Get_NI_StartDT: int64;
    function Get_ReqIntervalType: integer;
    function Get_Resistor: integer;
    procedure Set_CalibrType(const Value: integer);
    procedure Set_MeterCalcNIType(const Value: integer);
    procedure Set_NI_StartVal(const Value: UnicodeString);
    procedure Set_NI_StartDT(const Value: int64);
    procedure Set_ReqIntervalType(const Value: integer);
    procedure Set_Resistor(const Value: integer);
    function Get_ID_MeaUnit: integer;
    procedure Set_ID_MeaUnit(Value: integer);

    procedure SQL_CreateOrUpdateChannel(sql: TStringList; remoteName: UnicodeString; remoteID_Device: integer);
  end;

{ TXMLNodesType }

  TXMLNodesType = class(TXMLNodeCollection, IXMLNodesType)
  protected
    { IXMLNodesType }
    function Get_Node(Index: Integer): IXMLNodeType;
    function Add: IXMLNodeType;
    function Insert(const Index: Integer): IXMLNodeType;
  public
    procedure AfterConstruction; override;
    function NodeById(ID_Node: Integer): IXMLNodeType;
  end;

{ TXMLNodeType }

  TXMLNodeType = class(TXMLNode, IXMLNodeType)
  protected
    { IXMLNodeType }
    function Get_Id: Integer;
    function Get_Name: string;
    function Get_NodeType: Integer;
    function Get_Nodes: IXMLNodesType;
    function Get_NodeChannels: IXMLNodeChannelsType;
    function Get_Parent: Integer;
    procedure Set_Parent(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_Name(Value: string);
    procedure Set_NodeType(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNodeChannelsType }

  TXMLNodeChannelsType = class(TXMLNodeCollection, IXMLNodeChannelsType)
  protected
    { IXMLNodeChannelsType }
    function Get_NodeChannel(Index: Integer): IXMLNodeChannelType;
    function Add: IXMLNodeChannelType;
    function Insert(const Index: Integer): IXMLNodeChannelType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNodeChannelType }

  TXMLNodeChannelType = class(TXMLNode, IXMLNodeChannelType)
  protected
    { IXMLNodeChannelType }
    function Get_Id: Integer;
    function Get_ID_PT: Integer;
    function Get_Name: string;
    function Get_BaseChn: Integer;
    function Get_ID_MeaUnit: Integer;
    procedure Set_Id(Value: Integer);
    procedure Set_ID_PT(Value: Integer);
    procedure Set_Name(Value: string);
    procedure Set_BaseChn(Value: Integer);
    procedure Set_ID_MeaUnit(Value: Integer);
  end;

{ TXMLAggregatesType }

  TXMLAggregatesType = class(TXMLNodeCollection, IXMLAggregatesType)
  protected
    { IXMLAggregatesType }
    function Get_Aggregate(Index: Integer): IXMLAggregateType;
    function Add: IXMLAggregateType;
    function Insert(const Index: Integer): IXMLAggregateType;
  public
    procedure AfterConstruction; override;
    function AggregateById(id_aggr: Integer): IXMLAggregateType;
  end;

{ TXMLAggregateType }

  TXMLAggregateType = class(TXMLNode, IXMLAggregateType)
  protected
    { IXMLAggregateType }
    function Get_ID_AggrType: Integer;
    function Get_Name: UnicodeString;
    function Get_AggregateParams: IXMLAggregateParamsType;
    function Get_Id: Integer;
    procedure Set_Id(Value: Integer);
    procedure Set_ID_AggrType(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAggregateParamsType }

  TXMLAggregateParamsType = class(TXMLNodeCollection, IXMLAggregateParamsType)
  protected
    { IXMLAggregateParamsType }
    function Get_AggregateParam(Index: Integer): IXMLAggregateParamType;
    function Add: IXMLAggregateParamType;
    function Insert(const Index: Integer): IXMLAggregateParamType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAggregateParamType }

  TXMLAggregateParamType = class(TXMLNode, IXMLAggregateParamType)
  protected
    { IXMLAggregateParamType }
    function Get_ID_Prm: Integer;
    function Get_PrmKind: Integer;
    function Get_Caption: UnicodeString;
    procedure Set_ID_Prm(Value: Integer);
    procedure Set_PrmKind(Value: Integer);
    procedure Set_Caption(Value: UnicodeString);
  end;

{ TXMLParamTypesType }

  TXMLParamTypesType = class(TXMLNodeCollection, IXMLParamTypesType)
  protected
    { IXMLParamTypesType }
    function Get_ParamType(Index: Integer): IXMLParamType;
    function Add: IXMLParamType;
    function Insert(const Index: Integer): IXMLParamType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLParamType }

  TXMLParamType = class(TXMLNode, IXMLParamType)
  protected
    { IXMLParamType }
    function Get_ID_PT: Integer;
    function Get_PSign: UnicodeString;
    function Get_PName: UnicodeString;
    function Get_IsAlarm: Integer;
    procedure Set_ID_PT(Value: Integer);
    procedure Set_PSign(Value: UnicodeString);
    procedure Set_PName(Value: UnicodeString);
    procedure Set_IsAlarm(Value: Integer);
  end;

{ TXMLMeaUnitsType }

  TXMLMeaUnitsType = class(TXMLNodeCollection, IXMLMeaUnitsType)
  protected
    { IXMLMeaUnitsType }
    function Get_MeaUnit(Index: Integer): IXMLMeaUnitType;
    function Add: IXMLMeaUnitType;
    function Insert(const Index: Integer): IXMLMeaUnitType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMeaUnitType }

  TXMLMeaUnitType = class(TXMLNode, IXMLMeaUnitType)
  protected
    { IXMLMeaUnitType }
    function Get_ID_MeaUnit: Integer;
    function Get_Name: UnicodeString;
    function Get_LongName: UnicodeString;
    procedure Set_ID_MeaUnit(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_LongName(Value: UnicodeString);
  end;

{ IXMLCommandsType }

  TXMLCommandsType = class(TXMLNodeCollection, IXMLCommandsType)
    { Property Accessors }
    function Get_Command(Index: Integer): IXMLCommandType;
    { Methods & Properties }
    function Add: IXMLCommandType;
    function Insert(const Index: Integer): IXMLCommandType;
  public
    property Command[Index: Integer]: IXMLCommandType read Get_Command; default;
    procedure AfterConstruction; override;
  end;

{ IXMLCommandType }

  TXMLCommandType = class(TXMLNode, IXMLCommandType)
    { Property Accessors }
    function Get_ID_Prm: Integer;
    function Get_Val: Double;
    function Get_Utime: Integer;
    procedure Set_ID_Prm(Value: Integer);
    procedure Set_Utime(Value: Integer);
    procedure Set_Val(Value: Double);
    { Methods & Properties }
    property ID_Prm: Integer read Get_ID_Prm write Set_ID_Prm;
    property Utime: Integer read Get_Utime write Set_Utime;
    property Val: Double read Get_Val write Set_Val;
  end;

{ TXMLDataType }

  TXMLDataType = class(TXMLNodeCollection, IXMLDataType)
  protected
    { IXMLDataType }
    function Get_Source(Index: Integer): IXMLSourceType;
    function Add: IXMLSourceType;
    function Insert(const Index: Integer): IXMLSourceType;
  public
    procedure AfterConstruction; override;
    procedure SQL_InsertData(sql: TStringList; remoteName: UnicodeString);
  end;

{ TXMLSourceType }

  TXMLSourceType = class(TXMLNodeCollection, IXMLSourceType)
  protected
    { IXMLSourceType }
    function Get_Id: Integer;
    function Get_ValSrcType: Integer;
    function Get_ValType: Integer;
    function Get_Name: UnicodeString;
    function Get_Value(Index: Integer): IXMLValueType;
    procedure Set_Id(Value: Integer);
    procedure Set_ValSrcType(Value: Integer);
    procedure Set_ValType(Value: Integer);
    procedure Set_Name(Value: UnicodeString);
    function Add: IXMLValueType;
    function Insert(const Index: Integer): IXMLValueType;
  public
    procedure AfterConstruction; override;
    procedure SQL_InsertData(sql: TStringList; remoteName: UnicodeString);
  end;

{ TXMLValueType }

  TXMLValueType = class(TXMLNode, IXMLValueType)
  protected
    { IXMLValueType }
    function Get_Utime: Integer;
    function Get_Val: Double;
    function Get_Id: Integer;
    procedure Set_Id(Value: Integer);
    procedure Set_Utime(Value: Integer);
    procedure Set_Val(Value: Double);
  end;

{ TXMLSQLType }

  TXMLSQLType = class(TXMLNode, IXMLSQLType)
  protected
    { IXMLSQLType }
    function Get_Rows: IXMLRowsType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRowsType }

  TXMLRowsType = class(TXMLNodeCollection, IXMLRowsType)
  protected
    { IXMLRowsType }
    function Get_Row(Index: Integer): IXMLRowType;
    function Add: IXMLRowType;
    function Insert(const Index: Integer): IXMLRowType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLRowType }

  TXMLRowType = class(TXMLNode, IXMLRowType)
  protected
    { IXMLRowType }
    function Get_Fields: IXMLFieldsType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFieldsType }

  TXMLFieldsType = class(TXMLNode, IXMLFieldsType)
  protected
    { IXMLFieldsType }
    function Get_Field: IXMLFieldType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFieldType }

  TXMLFieldType = class(TXMLNode, IXMLFieldType)
  protected
    { IXMLFieldType }
    function Get_Value: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
  end;

{ TXMLDeviceType2 }

  TXMLDeviceType2 = class(TXMLNode, IXMLDeviceType2)
  protected
    { IXMLDeviceType2 }
  end;

{ TXMLAggregateType2 }

  TXMLAggregateType2 = class(TXMLNode, IXMLAggregateType2)
  protected
    { IXMLAggregateType2 }
  end;

{ TXMLObjectType2 }

  TXMLObjectType2 = class(TXMLNode, IXMLObjectType2)
  protected
    { IXMLObjectType2 }
  end;

{ TXMLSourceType2 }

  TXMLSourceType2 = class(TXMLNode, IXMLSourceType2)
  protected
    { IXMLSourceType2 }
  end;

{ TXMLRowType2 }

  TXMLRowType2 = class(TXMLNode, IXMLRowType2)
  protected
    { IXMLRowType2 }
  end;

{ Global Functions }

function GetGMIOResponce(Doc: IXMLDocument): IXMLGMIOResponceType;
function LoadGMIOResponce(const FileName: string): IXMLGMIOResponceType;
function NewGMIOResponce: IXMLGMIOResponceType;
function LoadXMLData_GMIOResponce(const xml: string): IXMLGMIOResponceType;

const
  TargetNamespace = '';

implementation

{ Global Functions }

uses GMGlobals, GMConst, StrUtils;

function GetGMIOResponce(Doc: IXMLDocument): IXMLGMIOResponceType;
begin
  Result := Doc.GetDocBinding('GMIOResponce', TXMLGMIOResponceType, TargetNamespace) as IXMLGMIOResponceType;
end;

function LoadGMIOResponce(const FileName: string): IXMLGMIOResponceType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('GMIOResponce', TXMLGMIOResponceType, TargetNamespace) as IXMLGMIOResponceType;
end;

function NewGMIOResponce: IXMLGMIOResponceType;
begin
  Result := NewXMLDocument.GetDocBinding('GMIOResponce', TXMLGMIOResponceType, TargetNamespace) as IXMLGMIOResponceType;
end;

function LoadXMLData_GMIOResponce(const xml: string): IXMLGMIOResponceType;
begin
  Result := LoadXMLData(xml).GetDocBinding('GMIOResponce', TXMLGMIOResponceType, TargetNamespace) as IXMLGMIOResponceType;
end;

{ TXMLGMIOResponceType }

procedure TXMLGMIOResponceType.AfterConstruction;
begin
  RegisterChildNode('Auth', TXMLAuthType);
  RegisterChildNode('Objects', TXMLObjectsType);
  RegisterChildNode('ParamTypes', TXMLParamTypesType);
  RegisterChildNode('MeaUnits', TXMLMeaUnitsType);
  RegisterChildNode('Data', TXMLDataType);
  RegisterChildNode('SQL', TXMLSQLType);
  RegisterChildNode('Nodes', TXMLNodesType);
  RegisterChildNode('State', TXMLStateType);
  RegisterChildNode('Commands', TXMLCommandsType);
  inherited;
end;

function TXMLGMIOResponceType.Get_Auth: IXMLAuthType;
begin
  Result := ChildNodes['Auth'] as IXMLAuthType;
end;

function TXMLGMIOResponceType.Get_Commands: IXMLCommandsType;
begin
  Result := ChildNodes['Commands'] as IXMLCommandsType;
end;

function TXMLGMIOResponceType.Get_Comment: UnicodeString;
begin
  Result := AttributeNodes['Comment'].Text;
end;

function TXMLGMIOResponceType.Get_Objects: IXMLObjectsType;
begin
  Result := ChildNodes['Objects'] as IXMLObjectsType;
end;

function TXMLGMIOResponceType.Get_ParamTypes: IXMLParamTypesType;
begin
  Result := ChildNodes['ParamTypes'] as IXMLParamTypesType;
end;

function TXMLGMIOResponceType.Get_MeaUnits: IXMLMeaUnitsType;
begin
  Result := ChildNodes['MeaUnits'] as IXMLMeaUnitsType;
end;

function TXMLGMIOResponceType.Get_Nodes: IXMLNodesType;
begin
  Result := ChildNodes['Nodes'] as IXMLNodesType;
end;

function TXMLGMIOResponceType.Get_Data: IXMLDataType;
begin
  Result := ChildNodes['Data'] as IXMLDataType;
end;

function TXMLGMIOResponceType.Get_SQL: IXMLSQLType;
begin
  Result := ChildNodes['SQL'] as IXMLSQLType;
end;

function TXMLGMIOResponceType.Get_State: IXMLStateType;
begin
  Result := ChildNodes['State'] as IXMLStateType;
end;

procedure TXMLGMIOResponceType.Set_Comment(Value: UnicodeString);
begin
  AttributeNodes['Comment'].Text := Value;
end;

{ TXMLAuthType }

function TXMLAuthType.Get_OK: Integer;
begin
  Result := AttributeNodes['OK'].NodeValue;
end;

function TXMLAuthType.Get_RemoteName: UnicodeString;
begin
  Result := ChildNodes['RemoteName'].NodeValue;
end;

function TXMLAuthType.Get_RequestID: UnicodeString;
begin
  Result := ChildNodes['RequestID'].NodeValue;
end;

procedure TXMLAuthType.Set_OK(Value: Integer);
begin
  SetAttribute('OK', Value);
end;

procedure TXMLAuthType.Set_RemoteName(Value: UnicodeString);
begin
  ChildNodes['RemoteName'].NodeValue := Value;
end;

procedure TXMLAuthType.Set_RequestID(Value: UnicodeString);
begin
  ChildNodes['RequestID'].NodeValue := Value;
end;

{ TXMLObjectsType}

procedure TXMLObjectsType.AfterConstruction;
begin
  RegisterChildNode('Object', TXMLObjectType);
  ItemTag := 'Object';
  ItemInterface := IXMLObjectType;
  inherited;
end;

function TXMLObjectsType.Get_Object_(Index: Integer): IXMLObjectType;
begin
  Result := List[Index] as IXMLObjectType;
end;

function TXMLObjectsType.Add: IXMLObjectType;
begin
  Result := AddItem(-1) as IXMLObjectType;
end;

function TXMLObjectsType.Insert(const Index: Integer): IXMLObjectType;
begin
  Result := AddItem(Index) as IXMLObjectType;
end;

function TXMLObjectsType.ObjectById(id_obj: Integer): IXMLObjectType;
var i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Get_Object_(i).Id = id_obj then
    begin
      Result := Get_Object_(i);
      Exit;
    end;
  end;
end;

procedure TXMLObjectsType.SQL_CreateOrUpdateObjects(sql: TStringList; remoteName: UnicodeString);
var i: int;
    objects: string;
begin
  objects := '';
  for i := 0 to Count - 1 do
  begin
    objects := objects + IfThen(i > 0, ', ') + IntToStr(Get_Object_(i).Id);
    Get_Object_(i).SQL_CreateOrUpdateObject(sql, remoteName);
  end;

//  if Count > 0 then
//    sql.Add(Format('delete from Objects where RemoteName = %s and RemoteID not in (%s)', [remoteName.QuotedString(), objects]));
end;

{ TXMLObjectType }

procedure TXMLObjectType.AfterConstruction;
begin
  RegisterChildNode('Devices', TXMLDevicesType);
  RegisterChildNode('Aggregates', TXMLAggregatesType);
  inherited;
end;

function TXMLObjectType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

procedure TXMLObjectType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

function TXMLObjectType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLObjectType.Set_Name(Value: UnicodeString);
begin
  SetAttribute('Name', Value);
end;

function TXMLObjectType.Get_Ncar: Integer;
begin
  Result := AttributeNodes['Ncar'].NodeValue;
end;

function TXMLObjectType.Get_ObjType: Integer;
begin
  Result := AttributeNodes['ObjType'].NodeValue;
end;

function TXMLObjectType.Get_UserData: UnicodeString;
begin
  Result := ChildNodes['UserData'].NodeValue;
end;

procedure TXMLObjectType.Set_Ncar(Value: Integer);
begin
  SetAttribute('Ncar', Value);
end;

procedure TXMLObjectType.Set_ObjType(Value: Integer);
begin
  AttributeNodes['ObjType'].NodeValue := Value;
end;

procedure TXMLObjectType.Set_UserData(Value: UnicodeString);
begin
  ChildNodes['UserData'].NodeValue := Value;
end;

procedure TXMLObjectType.SQL_CreateOrUpdateObject(sql: TStringList; remoteName: UnicodeString);
var s: string;
begin
  s := Format('if not exists(select * from Objects where ObjType = %d and RemoteID = %d and RemoteName = %s) then ' +
              '  insert into Objects(Name, N_Car, ObjType, RemoteName, RemoteType, RemoteID, UserData) select %s, %d, %d, %s, %d, %d, %s;',
              [OBJ_TYPE_REMOTE_SRV_XML, Get_Id(), remoteName.QuotedString(),
               Get_Name().QuotedString, Get_NCar(), OBJ_TYPE_REMOTE_SRV_XML, remoteName.QuotedString(), Get_ObjType(), Get_Id(), Get_UserData().QuotedString()]) +
       Format(' else update Objects set Name = %s, N_Car = %d, UserData = %s where RemoteID = %d and RemoteName = %s; end if;',
              [Get_Name().QuotedString, Get_NCar(), Get_UserData().QuotedString(), Get_Id(), remoteName.QuotedString()]);

  sql.Add(s);

  Get_Devices().SQL_CreateOrUpdateDevices(sql, remoteName, Get_Id());
end;

function TXMLObjectType.Get_Devices: IXMLDevicesType;
begin
  Result := ChildNodes['Devices'] as IXMLDevicesType;
end;

function TXMLObjectType.Get_Aggregates: IXMLAggregatesType;
begin
  Result := ChildNodes['Aggregates'] as IXMLAggregatesType;
end;

{ TXMLDevicesType }

procedure TXMLDevicesType.AfterConstruction;
begin
  RegisterChildNode('Device', TXMLDeviceType);
  ItemTag := 'Device';
  ItemInterface := IXMLDeviceType;
  inherited;
end;

function TXMLDevicesType.DeviceById(id_device: Integer): IXMLDeviceType;
var i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Get_Device(i).Id = id_device then
    begin
      Result := Get_Device(i);
      Exit;
    end;
  end;
end;

function TXMLDevicesType.Get_Device(Index: Integer): IXMLDeviceType;
begin
  Result := List[Index] as IXMLDeviceType;
end;

function TXMLDevicesType.Add: IXMLDeviceType;
begin
  Result := AddItem(-1) as IXMLDeviceType;
end;

function TXMLDevicesType.Insert(const Index: Integer): IXMLDeviceType;
begin
  Result := AddItem(Index) as IXMLDeviceType;
end;

procedure TXMLDevicesType.SQL_CreateOrUpdateDevices(sql: TStringList; remoteName: UnicodeString; remoteID_Obj: integer);
var i: int;
    devices: string;
begin
  devices := '';

  for i := 0 to Count - 1 do
  begin
    devices := devices + IfThen(i > 0, ', ') + IntToStr(Get_Device(i).Id);
    Get_Device(i).SQL_CreateOrUpdateDevice(sql, remoteName, remoteID_Obj);
  end;

{
  if Count > 0 then
    sql.Add(Format('delete from Devices where ID_Device in (select ID_Device from DeviceSystem where RemoteName = %s and DevRemoteID not in (%s))',
                   [remoteName.QuotedString(), devices]));
}
end;

{ TXMLDeviceType }

procedure TXMLDeviceType.AfterConstruction;
begin
  RegisterChildNode('Channels', TXMLChannelsType);
  inherited;
end;

function TXMLDeviceType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

procedure TXMLDeviceType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

function TXMLDeviceType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['Name'].Text;
end;

function TXMLDeviceType.Get_Number: Integer;
begin
  Result := AttributeNodes['Number'].NodeValue;
end;

procedure TXMLDeviceType.Set_Name(Value: UnicodeString);
begin
  SetAttribute('Name', Value);
end;

procedure TXMLDeviceType.Set_Number(Value: Integer);
begin
  SetAttribute('Number', Value);
end;

procedure TXMLDeviceType.SQL_CreateOrUpdateDevice(sql: TStringList; remoteName: UnicodeString; remoteID_Obj: integer);
var s, strObj, sDevCondition: string;
begin
  strObj := Format('(select ID_Obj from Objects where RemoteName = %s and RemoteID = %d limit 1)', [remoteName.QuotedString(), remoteID_Obj]);
  sDevCondition := 'RemoteID = ' + IntToStr(Get_Id()) + ' and ID_Obj = ' + strObj;

  s :=            'if not exists(select * from Devices where ' + sDevCondition + ') then ';
  s := s + Format('  insert into Devices(ID_Obj, ID_DevType, Number, DevName, RemoteID) select %s, %d, %d, %s, %d;',
              [strObj, Get_ID_DevType(), Get_Number(), Get_Name().QuotedString, Get_Id()]);
  s := s + Format(' else update Devices set ID_DevType = %d, Number = %d, DevName = %s where %s; end if;',
              [Get_ID_DevType(), Get_Number(), Get_Name().QuotedString, sDevCondition]);

  sql.Add(s);

  Get_Channels().SQL_CreateOrUpdateChannels(sql, remoteName, Get_Id());
end;

function TXMLDeviceType.Get_ID_DevType: Integer;
begin
  Result := AttributeNodes['ID_DevType'].NodeValue;
end;

procedure TXMLDeviceType.Set_ID_DevType(Value: Integer);
begin
  SetAttribute('ID_DevType', Value);
end;

function TXMLDeviceType.Get_DevType: UnicodeString;
begin
  Result := AttributeNodes['DevType'].Text;
end;

procedure TXMLDeviceType.Set_DevType(Value: UnicodeString);
begin
  SetAttribute('DevType', Value);
end;

function TXMLDeviceType.Get_Channels: IXMLChannelsType;
begin
  Result := ChildNodes['Channels'] as IXMLChannelsType;
end;

{ TXMLChannelsType }

procedure TXMLChannelsType.AfterConstruction;
begin
  RegisterChildNode('Channel', TXMLChannelType);
  ItemTag := 'Channel';
  ItemInterface := IXMLChannelType;
  inherited;
end;

function TXMLChannelsType.Get_Channel(Index: Integer): IXMLChannelType;
begin
  Result := List[Index] as IXMLChannelType;
end;

function TXMLChannelsType.Add: IXMLChannelType;
begin
  Result := AddItem(-1) as IXMLChannelType;
end;

function TXMLChannelsType.Insert(const Index: Integer): IXMLChannelType;
begin
  Result := AddItem(Index) as IXMLChannelType;
end;

procedure TXMLChannelsType.SQL_CreateOrUpdateChannels(sql: TStringList; remoteName: UnicodeString; remoteID_Device: integer);
var i: int;
    channels: string;
begin
  channels := '';
  for i := 0 to Count - 1 do
  begin
    channels := channels + IfThen(i > 0, ', ') + IntToStr(Get_Channel(i).Id);
    Get_Channel(i).SQL_CreateOrUpdateChannel(sql, remoteName, remoteID_Device);
  end;

{
  if Count > 0 then
    sql.Add(Format('delete from Channels where ID_Param in (select ID_Param from DeviceSystem where RemoteName = %s and PrmRemoteID not in (%s))',
                   [remoteName.QuotedString(), channels]));
}
end;

{ TXMLChannelType }

function TXMLChannelType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

procedure TXMLChannelType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

function TXMLChannelType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['name'].Text;
end;

function TXMLChannelType.Get_NI_StartVal: UnicodeString;
begin
  Result := AttributeNodes['NI_StartVal'].Text;
end;

function TXMLChannelType.Get_NI_StartDT: int64;
begin
  Result := AttributeNodes['NI_StartDT'].NodeValue;
end;

procedure TXMLChannelType.Set_Name(Value: UnicodeString);
begin
  SetAttribute('name', Value);
end;

procedure TXMLChannelType.Set_NI_StartVal(const Value: UnicodeString);
begin
  SetAttribute('NI_StartVal', Value);
end;

procedure TXMLChannelType.Set_NI_StartDT(const Value: int64);
begin
  SetAttribute('Get_NI_StartDT', Value);
end;

function TXMLChannelType.Get_ID_MeaUnit: integer;
begin
  Result := AttributeNodes['ID_MeaUnit'].NodeValue;
end;

function TXMLChannelType.Get_ID_PT: Integer;
begin
  Result := AttributeNodes['ID_PT'].NodeValue;
end;

procedure TXMLChannelType.Set_ID_MeaUnit(Value: integer);
begin
  SetAttribute('ID_MeaUnit', Value);
end;

procedure TXMLChannelType.Set_ID_PT(Value: Integer);
begin
  SetAttribute('ID_PT', Value);
end;

function TXMLChannelType.Get_ID_Src: Integer;
begin
  Result := AttributeNodes['ID_Src'].NodeValue;
end;

function TXMLChannelType.Get_MeterCalcNIType: integer;
begin
  Result := AttributeNodes['MeterCalcNIType'].NodeValue;
end;

procedure TXMLChannelType.Set_ID_Src(Value: Integer);
begin
  SetAttribute('ID_Src', Value);
end;

procedure TXMLChannelType.Set_MeterCalcNIType(const Value: integer);
begin
  SetAttribute('MeterCalcNIType', Value);
end;

function TXMLChannelType.Get_Nsrc: Integer;
begin
  Result := AttributeNodes['Nsrc'].NodeValue;
end;

function TXMLChannelType.Get_RealMax: UnicodeString;
begin
  Result := AttributeNodes['RealMax'].Text;
end;

function TXMLChannelType.Get_RealMin: UnicodeString;
begin
  Result := AttributeNodes['RealMin'].Text;
end;

function TXMLChannelType.Get_ReqIntervalType: integer;
begin
  Result := AttributeNodes['ReqIntervalType'].NodeValue;
end;

function TXMLChannelType.Get_Resistor: integer;
begin
  Result := AttributeNodes['Resistor'].NodeValue;
end;

function TXMLChannelType.Get_UserSrc: integer;
begin
  Result := StrToIntDef(AttributeNodes['UserSrc'].NodeValue, 0);
end;

procedure TXMLChannelType.Set_Nsrc(Value: Integer);
begin
  SetAttribute('Nsrc', Value);
end;

procedure TXMLChannelType.Set_RealMax(Value: UnicodeString);
begin
  SetAttribute('RealMax', Value);
end;

procedure TXMLChannelType.Set_RealMin(Value: UnicodeString);
begin
  SetAttribute('RealMin', Value);
end;

procedure TXMLChannelType.Set_ReqIntervalType(const Value: integer);
begin
  SetAttribute('ReqIntervalType', Value);
end;

procedure TXMLChannelType.Set_Resistor(const Value: integer);
begin
  SetAttribute('Resistor', Value);
end;

procedure TXMLChannelType.Set_UserSrc(Value: integer);
begin
  SetAttribute('UserSrc', Value);
end;

function NullIf0(n: int64): string;
begin
  if n <= 0 then
    Result := 'null'
  else
    Result := IntToStr(n);
end;

function NullIfEmpty(const s: string): string;
begin
  if s <> '' then
    Result := s
  else
    Result := 'null';
end;

procedure TXMLChannelType.SQL_CreateOrUpdateChannel(sql: TStringList; remoteName: UnicodeString; remoteID_Device: integer);
var s, strDev, sChnCondition, sBaseChn: string;
begin
  strDev := Format('(select ID_Device from Devices where ID_Obj in (select ID_Obj from Objects where RemoteName = %s) and RemoteID = %d limit 1)',
                  [remoteName.QuotedString(), remoteID_Device]);

  sChnCondition := 'RemoteID = ' + IntToStr(Get_Id()) + ' and ID_Device = ' + strDev;
  if Get_BaseChn() > 0 then
    sBaseChn := '(select ID_Prm from Params where ID_Device = ' + strDev + ' and RemoteID = ' + IntToStr(Get_BaseChn()) + ')'
  else
    sBaseChn := 'null';

  s :=            ' if not exists(select * from Params where ' + sChnCondition + ') then ';
  s := s + Format('   insert into Params(ID_PT, Name, ID_Src, N_Src, CurrentsAddr, UserSrc,' +
                  '                      DevMin, DevMax, RealMin, RealMax, ' +
                  '                      ID_MeaUnit, AlarmSignal, AlarmThreshold,' +
                  '                      NI_StartDT, NI_StartVal,' +
                  '                      BaseChn, CalibrType, Resistor, ' +
                  '                      MeterCalcNIType, ReqIntervalType,' +
                  '                      ID_Device, RemoteID) ' +
                  '     select /*1*/%s, %s, %d, %d, %s, %s,  /*2*/%s, %s, %s, %s,  /*3*/%s, %s, %s,  /*4*/%s, %s,  /*5*/%s, %s, %s,   /*6*/%s, %s,   /*7*/%s, %d ;',
                  [NullIf0(Get_ID_PT()), Get_Name().QuotedString(), Get_ID_Src(), Get_NSrc(), IfThen(Get_ID_Src() = SRC_USR, IntToStr(Get_NSrc()), 'null'), NullIf0(Get_UserSrc()),
                   NullIfEmpty(Get_DevMin()), NullIfEmpty(Get_DevMax()), NullIfEmpty(Get_RealMin()), NullIfEmpty(Get_RealMax()),
                   NullIf0(Get_ID_MeaUnit()), NullIf0(Get_AlarmSignal()), NullIfEmpty(Get_AlarmThreshold()),
                   NullIf0(Get_NI_StartDT()), NullIfEmpty(Get_NI_StartVal()),
                   sBaseChn, NullIf0(Get_CalibrType()), NullIf0(Get_Resistor()),
                   NullIf0(Get_MeterCalcNIType()), NullIf0(Get_ReqIntervalType()),
                   strDev, Get_ID()]);

  s := s + Format(' else update Params set ID_PT = %s, Name = %s, ID_Src = %d, N_Src = %d, CurrentsAddr = %s, UserSrc = %s,' +
                  '                        DevMin = %s, DevMax = %s, RealMin = %s, RealMax = %s, ' +
                  '                        ID_MeaUnit = %s, AlarmSignal = %s, AlarmThreshold = %s,' +
                  '                        NI_StartDT = %s, NI_StartVal = %s,' +
                  '                        BaseChn = %s, CalibrType = %s, Resistor = %s, ' +
                  '                        MeterCalcNIType = %s, ReqIntervalType = %s ' +
                  '        where %s;',
                  [NullIf0(Get_ID_PT()), Get_Name().QuotedString(), Get_ID_Src(), Get_NSrc(), IfThen(Get_ID_Src() = SRC_USR, IntToStr(Get_NSrc()), 'null'), NullIf0(Get_UserSrc()),
                   NullIfEmpty(Get_DevMin()), NullIfEmpty(Get_DevMax()), NullIfEmpty(Get_RealMin()), NullIfEmpty(Get_RealMax()),
                   NullIf0(Get_ID_MeaUnit()), NullIf0(Get_AlarmSignal()), NullIfEmpty(Get_AlarmThreshold()),
                   NullIf0(Get_NI_StartDT()), NullIfEmpty(Get_NI_StartVal()),
                   sBaseChn, NullIf0(Get_CalibrType()), NullIf0(Get_Resistor()),
                   NullIf0(Get_MeterCalcNIType()), NullIf0(Get_ReqIntervalType()),
                   sChnCondition]);

  s := s + ' end if';

  sql.Add(s);
end;

function TXMLChannelType.Get_AlarmSignal: Integer;
begin
  Result := AttributeNodes['AlarmSignal'].NodeValue;
end;

procedure TXMLChannelType.Set_AlarmSignal(Value: Integer);
begin
  SetAttribute('AlarmSignal', Value);
end;

function TXMLChannelType.Get_AlarmThreshold: UnicodeString;
begin
  Result := AttributeNodes['AlarmThreshold'].Text;
end;

procedure TXMLChannelType.Set_AlarmThreshold(Value: UnicodeString);
begin
  SetAttribute('AlarmThreshold', Value);
end;

function TXMLChannelType.Get_BaseChn: Integer;
begin
  Result := AttributeNodes['BaseChn'].NodeValue;
end;

function TXMLChannelType.Get_CalibrType: integer;
begin
  Result := AttributeNodes['CalibrType'].NodeValue;
end;

function TXMLChannelType.Get_DevMax: UnicodeString;
begin
  Result := AttributeNodes['DevMax'].Text;
end;

function TXMLChannelType.Get_DevMin: UnicodeString;
begin
  Result := AttributeNodes['DevMin'].Text;
end;

procedure TXMLChannelType.Set_BaseChn(Value: Integer);
begin
  SetAttribute('BaseChn', Value);
end;

procedure TXMLChannelType.Set_CalibrType(const Value: integer);
begin
  SetAttribute('CalibrType', Value);
end;

procedure TXMLChannelType.Set_DevMax(Value: UnicodeString);
begin
  SetAttribute('DevMax', Value);
end;

procedure TXMLChannelType.Set_DevMin(Value: UnicodeString);
begin
  SetAttribute('DevMin', Value);
end;

{ TXMLAggregatesType }

procedure TXMLAggregatesType.AfterConstruction;
begin
  RegisterChildNode('Aggregate', TXMLAggregateType);
  ItemTag := 'Aggregate';
  ItemInterface := IXMLAggregateType;
  inherited;
end;

function TXMLAggregatesType.AggregateById(id_aggr: Integer): IXMLAggregateType;
var i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Get_Aggregate(i).Id = id_aggr then
    begin
      Result := Get_Aggregate(i);
      Exit;
    end;
  end;
end;

function TXMLAggregatesType.Get_Aggregate(Index: Integer): IXMLAggregateType;
begin
  Result := List[Index] as IXMLAggregateType;
end;

function TXMLAggregatesType.Add: IXMLAggregateType;
begin
  Result := AddItem(-1) as IXMLAggregateType;
end;

function TXMLAggregatesType.Insert(const Index: Integer): IXMLAggregateType;
begin
  Result := AddItem(Index) as IXMLAggregateType;
end;

{ TXMLAggregateType }

procedure TXMLAggregateType.AfterConstruction;
begin
  RegisterChildNode('AggregateParams', TXMLAggregateParamsType);
  inherited;
end;

function TXMLAggregateType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

function TXMLAggregateType.Get_ID_AggrType: Integer;
begin
  Result := AttributeNodes['ID_AggrType'].NodeValue;
end;

procedure TXMLAggregateType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

procedure TXMLAggregateType.Set_ID_AggrType(Value: Integer);
begin
  SetAttribute('ID_AggrType', Value);
end;

function TXMLAggregateType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLAggregateType.Set_Name(Value: UnicodeString);
begin
  SetAttribute('Name', Value);
end;

function TXMLAggregateType.Get_AggregateParams: IXMLAggregateParamsType;
begin
  Result := ChildNodes['AggregateParams'] as IXMLAggregateParamsType;
end;

{ TXMLAggregateParamsType }

procedure TXMLAggregateParamsType.AfterConstruction;
begin
  RegisterChildNode('AggregateParam', TXMLAggregateParamType);
  ItemTag := 'AggregateParam';
  ItemInterface := IXMLAggregateParamType;
  inherited;
end;

function TXMLAggregateParamsType.Get_AggregateParam(Index: Integer): IXMLAggregateParamType;
begin
  Result := List[Index] as IXMLAggregateParamType;
end;

function TXMLAggregateParamsType.Add: IXMLAggregateParamType;
begin
  Result := AddItem(-1) as IXMLAggregateParamType;
end;

function TXMLAggregateParamsType.Insert(const Index: Integer): IXMLAggregateParamType;
begin
  Result := AddItem(Index) as IXMLAggregateParamType;
end;

{ TXMLAggregateParamType }

function TXMLAggregateParamType.Get_ID_Prm: Integer;
begin
  Result := AttributeNodes['ID_Prm'].NodeValue;
end;

procedure TXMLAggregateParamType.Set_ID_Prm(Value: Integer);
begin
  SetAttribute('ID_Prm', Value);
end;

function TXMLAggregateParamType.Get_PrmKind: Integer;
begin
  Result := AttributeNodes['PrmKind'].NodeValue;
end;

procedure TXMLAggregateParamType.Set_PrmKind(Value: Integer);
begin
  SetAttribute('PrmKind', Value);
end;

function TXMLAggregateParamType.Get_Caption: UnicodeString;
begin
  Result := AttributeNodes['Caption'].Text;
end;

procedure TXMLAggregateParamType.Set_Caption(Value: UnicodeString);
begin
  SetAttribute('Caption', Value);
end;

{ TXMLParamTypesType }

procedure TXMLParamTypesType.AfterConstruction;
begin
  RegisterChildNode('ParamType', TXMLParamType);
  ItemTag := 'ParamType';
  ItemInterface := IXMLParamType;
  inherited;
end;

function TXMLParamTypesType.Get_ParamType(Index: Integer): IXMLParamType;
begin
  Result := List[Index] as IXMLParamType;
end;

function TXMLParamTypesType.Add: IXMLParamType;
begin
  Result := AddItem(-1) as IXMLParamType;
end;

function TXMLParamTypesType.Insert(const Index: Integer): IXMLParamType;
begin
  Result := AddItem(Index) as IXMLParamType;
end;

{ TXMLParamType }

function TXMLParamType.Get_ID_PT: Integer;
begin
  Result := AttributeNodes['ID_PT'].NodeValue;
end;

procedure TXMLParamType.Set_ID_PT(Value: Integer);
begin
  SetAttribute('ID_PT', Value);
end;

function TXMLParamType.Get_PSign: UnicodeString;
begin
  Result := AttributeNodes['PSign'].Text;
end;

procedure TXMLParamType.Set_PSign(Value: UnicodeString);
begin
  SetAttribute('PSign', Value);
end;

function TXMLParamType.Get_PName: UnicodeString;
begin
  Result := AttributeNodes['PName'].Text;
end;

procedure TXMLParamType.Set_PName(Value: UnicodeString);
begin
  SetAttribute('PName', Value);
end;

function TXMLParamType.Get_IsAlarm: Integer;
begin
  Result := AttributeNodes['IsAlarm'].NodeValue;
end;

procedure TXMLParamType.Set_IsAlarm(Value: Integer);
begin
  SetAttribute('IsAlarm', Value);
end;

{ TXMLMeaUnitsType }

procedure TXMLMeaUnitsType.AfterConstruction;
begin
  RegisterChildNode('MeaUnit', TXMLMeaUnitType);
  ItemTag := 'MeaUnit';
  ItemInterface := IXMLMeaUnitType;
  inherited;
end;

function TXMLMeaUnitsType.Get_MeaUnit(Index: Integer): IXMLMeaUnitType;
begin
  Result := List[Index] as IXMLMeaUnitType;
end;

function TXMLMeaUnitsType.Add: IXMLMeaUnitType;
begin
  Result := AddItem(-1) as IXMLMeaUnitType;
end;

function TXMLMeaUnitsType.Insert(const Index: Integer): IXMLMeaUnitType;
begin
  Result := AddItem(Index) as IXMLMeaUnitType;
end;

{ TXMLMeaUnitType }

function TXMLMeaUnitType.Get_ID_MeaUnit: Integer;
begin
  Result := AttributeNodes['ID_MeaUnit'].NodeValue;
end;

procedure TXMLMeaUnitType.Set_ID_MeaUnit(Value: Integer);
begin
  SetAttribute('ID_MeaUnit', Value);
end;

function TXMLMeaUnitType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLMeaUnitType.Set_Name(Value: UnicodeString);
begin
  SetAttribute('Name', Value);
end;

function TXMLMeaUnitType.Get_LongName: UnicodeString;
begin
  Result := AttributeNodes['LongName'].Text;
end;

procedure TXMLMeaUnitType.Set_LongName(Value: UnicodeString);
begin
  SetAttribute('LongName', Value);
end;

{ TXMLDataType }

procedure TXMLDataType.AfterConstruction;
begin
  RegisterChildNode('Source', TXMLSourceType);
  ItemTag := 'Source';
  ItemInterface := IXMLSourceType;
  inherited;
end;

function TXMLDataType.Get_Source(Index: Integer): IXMLSourceType;
begin
  Result := List[Index] as IXMLSourceType;
end;

function TXMLDataType.Add: IXMLSourceType;
begin
  Result := AddItem(-1) as IXMLSourceType;
end;

function TXMLDataType.Insert(const Index: Integer): IXMLSourceType;
begin
  Result := AddItem(Index) as IXMLSourceType;
end;

procedure TXMLDataType.SQL_InsertData(sql: TStringList; remoteName: UnicodeString);
var i: int;
begin
  for i := 0 to Count - 1 do
    Get_Source(i).SQL_InsertData(sql, remoteName);
end;

{ TXMLSourceType }

procedure TXMLSourceType.AfterConstruction;
begin
  RegisterChildNode('Value', TXMLValueType);
  ItemTag := 'Value';
  ItemInterface := IXMLValueType;
  inherited;
end;

function TXMLSourceType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

procedure TXMLSourceType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

function TXMLSourceType.Get_ValSrcType: Integer;
begin
  Result := AttributeNodes['ValSrcType'].NodeValue;
end;

procedure TXMLSourceType.Set_ValSrcType(Value: Integer);
begin
  SetAttribute('ValSrcType', Value);
end;

function TXMLSourceType.Get_ValType: Integer;
begin
  Result := AttributeNodes['ValType'].NodeValue;
end;

procedure TXMLSourceType.Set_ValType(Value: Integer);
begin
  SetAttribute('ValType', Value);
end;

procedure TXMLSourceType.SQL_InsertData(sql: TStringList; remoteName: UnicodeString);
var id_prm, s, proc: string;
    i: int;
begin
  case Get_ValType() of
    DATATYPE_CURRENT_DIAGRAM: proc := 'WriteVal';
    DATATYPE_HOUR_DIAGRAM: proc := 'WriteArchHour';
    DATATYPE_DAY_DIAGRAM: proc := 'WriteArchDay';
    else Exit;
  end;

  id_prm := Format(SQLConst_SelectRemotePrmId, [remoteName.QuotedString(), Get_Id()]);

  for i := 0 to Count - 1 do
  begin
    s := Format('perform %s(%s, %d, %s)', [proc, id_prm, Get_Value(i).Utime, MyFloatToStr(Get_Value(i).Val)]);
    sql.Add(s);
  end;
end;

function TXMLSourceType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLSourceType.Set_Name(Value: UnicodeString);
begin
  SetAttribute('Name', Value);
end;

function TXMLSourceType.Get_Value(Index: Integer): IXMLValueType;
begin
  Result := List[Index] as IXMLValueType;
end;

function TXMLSourceType.Add: IXMLValueType;
begin
  Result := AddItem(-1) as IXMLValueType;
end;

function TXMLSourceType.Insert(const Index: Integer): IXMLValueType;
begin
  Result := AddItem(Index) as IXMLValueType;
end;

{ TXMLValueType }

function TXMLValueType.Get_Id: Integer;
begin
  Result := AttributeNodes['id'].NodeValue;
end;

function TXMLValueType.Get_Utime: Integer;
begin
  Result := AttributeNodes['utime'].NodeValue;
end;

procedure TXMLValueType.Set_Id(Value: Integer);
begin
  SetAttribute('id', Value);
end;

procedure TXMLValueType.Set_Utime(Value: Integer);
begin
  SetAttribute('utime', Value);
end;

function TXMLValueType.Get_Val: Double;
begin
  Result := MyStrToFloat(AttributeNodes['val'].NodeValue);
end;

procedure TXMLValueType.Set_Val(Value: Double);
begin
  SetAttribute('val', Value);
end;

{ TXMLSQLType }

procedure TXMLSQLType.AfterConstruction;
begin
  RegisterChildNode('Rows', TXMLRowsType);
  inherited;
end;

function TXMLSQLType.Get_Rows: IXMLRowsType;
begin
  Result := ChildNodes['Rows'] as IXMLRowsType;
end;

{ TXMLRowsType }

procedure TXMLRowsType.AfterConstruction;
begin
  RegisterChildNode('Row', TXMLRowType);
  ItemTag := 'Row';
  ItemInterface := IXMLRowType;
  inherited;
end;

function TXMLRowsType.Get_Row(Index: Integer): IXMLRowType;
begin
  Result := List[Index] as IXMLRowType;
end;

function TXMLRowsType.Add: IXMLRowType;
begin
  Result := AddItem(-1) as IXMLRowType;
end;

function TXMLRowsType.Insert(const Index: Integer): IXMLRowType;
begin
  Result := AddItem(Index) as IXMLRowType;
end;

{ TXMLRowType }

procedure TXMLRowType.AfterConstruction;
begin
  RegisterChildNode('Fields', TXMLFieldsType);
  inherited;
end;

function TXMLRowType.Get_Fields: IXMLFieldsType;
begin
  Result := ChildNodes['Fields'] as IXMLFieldsType;
end;

{ TXMLFieldsType }

procedure TXMLFieldsType.AfterConstruction;
begin
  RegisterChildNode('Field', TXMLFieldType);
  inherited;
end;

function TXMLFieldsType.Get_Field: IXMLFieldType;
begin
  Result := ChildNodes['Field'] as IXMLFieldType;
end;

{ TXMLFieldType }

function TXMLFieldType.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['Value'].Text;
end;

procedure TXMLFieldType.Set_Value(Value: UnicodeString);
begin
  SetAttribute('Value', Value);
end;

{ TXMLNodesType }

procedure TXMLNodesType.AfterConstruction;
begin
  RegisterChildNode('Node', TXMLNodeType);
  ItemTag := 'Node';
  ItemInterface := IXMLNodeType;
  inherited;
end;

function TXMLNodesType.Get_Node(Index: Integer): IXMLNodeType;
begin
  Result := List[Index] as IXMLNodeType;
end;

function TXMLNodesType.Add: IXMLNodeType;
begin
  Result := AddItem(-1) as IXMLNodeType;
end;

function TXMLNodesType.Insert(const Index: Integer): IXMLNodeType;
begin
  Result := AddItem(Index) as IXMLNodeType;
end;

function TXMLNodesType.NodeById(ID_Node: Integer): IXMLNodeType;
var i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Get_Node(i).Id = ID_Node then
    begin
      Result := Get_Node(i);
      Exit;
    end;
  end;
end;

{ TXMLNodeType }

procedure TXMLNodeType.AfterConstruction;
begin
  RegisterChildNode('NodeChannels', TXMLNodeChannelsType);
  RegisterChildNode('Nodes', TXMLNodesType);
  inherited;
end;

function TXMLNodeType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

function TXMLNodeType.Get_Parent: Integer;
begin
  Result := AttributeNodes['Parent'].NodeValue;
end;

procedure TXMLNodeType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

procedure TXMLNodeType.Set_Parent(Value: Integer);
begin
  SetAttribute('Parent', Value);
end;

function TXMLNodeType.Get_Name: string;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLNodeType.Set_Name(Value: string);
begin
  SetAttribute('Name', Value);
end;

function TXMLNodeType.Get_NodeType: Integer;
begin
  Result := AttributeNodes['NodeType'].NodeValue;
end;

procedure TXMLNodeType.Set_NodeType(Value: Integer);
begin
  SetAttribute('NodeType', Value);
end;

function TXMLNodeType.Get_Nodes: IXMLNodesType;
begin
  Result := ChildNodes['Nodes'] as IXMLNodesType;
end;

function TXMLNodeType.Get_NodeChannels: IXMLNodeChannelsType;
begin
  Result := ChildNodes['NodeChannels'] as IXMLNodeChannelsType;
end;

{ TXMLNodeChannelsType }

procedure TXMLNodeChannelsType.AfterConstruction;
begin
  RegisterChildNode('NodeChannel', TXMLNodeChannelType);
  ItemTag := 'NodeChannel';
  ItemInterface := IXMLNodeChannelType;
  inherited;
end;

function TXMLNodeChannelsType.Get_NodeChannel(Index: Integer): IXMLNodeChannelType;
begin
  Result := List[Index] as IXMLNodeChannelType;
end;

function TXMLNodeChannelsType.Add: IXMLNodeChannelType;
begin
  Result := AddItem(-1) as IXMLNodeChannelType;
end;

function TXMLNodeChannelsType.Insert(const Index: Integer): IXMLNodeChannelType;
begin
  Result := AddItem(Index) as IXMLNodeChannelType;
end;

{ TXMLNodeChannelType }

function TXMLNodeChannelType.Get_Id: Integer;
begin
  Result := AttributeNodes['Id'].NodeValue;
end;

procedure TXMLNodeChannelType.Set_Id(Value: Integer);
begin
  SetAttribute('Id', Value);
end;

function TXMLNodeChannelType.Get_ID_PT: Integer;
begin
  Result := AttributeNodes['ID_PT'].NodeValue;
end;

procedure TXMLNodeChannelType.Set_ID_PT(Value: Integer);
begin
  SetAttribute('ID_PT', Value);
end;

function TXMLNodeChannelType.Get_Name: string;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLNodeChannelType.Set_Name(Value: string);
begin
  SetAttribute('Name', Value);
end;

function TXMLNodeChannelType.Get_BaseChn: Integer;
begin
  Result := AttributeNodes['BaseChn'].NodeValue;
end;

procedure TXMLNodeChannelType.Set_BaseChn(Value: Integer);
begin
  SetAttribute('BaseChn', Value);
end;

function TXMLNodeChannelType.Get_ID_MeaUnit: Integer;
begin
  Result := AttributeNodes['ID_MeaUnit'].NodeValue;
end;

procedure TXMLNodeChannelType.Set_ID_MeaUnit(Value: Integer);
begin
  SetAttribute('ID_MeaUnit', Value);
end;

{ TXMLCommandsType }

function TXMLCommandsType.Add: IXMLCommandType;
begin
  Result := AddItem(-1) as IXMLCommandType;
end;

procedure TXMLCommandsType.AfterConstruction;
begin
  RegisterChildNode('Command', TXMLCommandType);
  ItemTag := 'Command';
  ItemInterface := IXMLCommandType;
  inherited;
end;

function TXMLCommandsType.Get_Command(Index: Integer): IXMLCommandType;
begin
  Result := List[Index] as IXMLCommandType;
end;

function TXMLCommandsType.Insert(const Index: Integer): IXMLCommandType;
begin
  Result := AddItem(Index) as IXMLCommandType;
end;

{ TXMLCommandType }

function TXMLCommandType.Get_ID_Prm: Integer;
begin
  Result := AttributeNodes['ID_Prm'].NodeValue;
end;

function TXMLCommandType.Get_Utime: Integer;
begin
  Result := AttributeNodes['utime'].NodeValue;
end;

function TXMLCommandType.Get_Val: Double;
begin
  Result := MyStrToFloat(AttributeNodes['val'].NodeValue);
end;

procedure TXMLCommandType.Set_ID_Prm(Value: Integer);
begin
  SetAttribute('ID_Prm', Value);
end;

procedure TXMLCommandType.Set_Utime(Value: Integer);
begin
  SetAttribute('utime', Value);
end;

procedure TXMLCommandType.Set_Val(Value: Double);
begin
  SetAttribute('val', Value);
end;

initialization
  NullStrictConvert := false;
end.