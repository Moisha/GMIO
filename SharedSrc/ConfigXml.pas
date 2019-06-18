
{*********************************************************************}
{                                                                     }
{                          XML Data Binding                           }
{                                                                     }
{         Generated on: 17.05.2012 19:57:46                           }
{       Generated from: D:\Programs\Delphi\Geomer\GMIO\Скважины.xml   }
{   Settings stored in: D:\Programs\Delphi\Geomer\GMIO\Скважины.xdb   }
{                                                                     }
{*********************************************************************}

unit ConfigXml;

interface

uses Forms, Windows, xmldom, XMLDoc, XMLIntf, GMGlobals, GMConst, SysUtils, Variants;

type

{ Forward Decls }

  IGMConfigConfigType = interface;
  IGMConfigCommonType = interface;
  IGMConfigSoundType = interface;
  IGMConfigObjectsType = interface;
  IGMConfigObjectType = interface;
  IGMConfigChannelsType = interface;
  IGMConfigChannelType = interface;
  IGMConfigTabsType = interface;
  IGMConfigTabType = interface;
  IGMConfigAlarmsType = interface;
  IGMConfigAlarmType = interface;
  IGMConfigBigwindowsType = interface;
  IGMConfigBigwindowType = interface;
  IGMConfigDiagramsType = interface;
  IGMConfigDiagramType = interface;
  IGMConfigHydrogeorepsType = interface;
  IGMConfigHydroreportType = interface;
  IGMConfigReportsType = interface;
  IGMConfigReportType = interface;
  IGMConfigReportColumnsType = interface;
  IGMConfigReportColumnType = interface;
  IGMAuthType = interface;
  IGMScadaType = interface;

{ IGMConfigConfigType }

  IGMConfigConfigType = interface(IXMLNode)
    ['{9B2FA8D6-47CF-44E0-A779-1EBACAE6784F}']
    { Property Accessors }
    function Get_Sound: IGMConfigSoundType;
    function Get_Common: IGMConfigCommonType;
    function Get_Objects: IGMConfigObjectsType;
    function Get_Alarms: IGMConfigAlarmsType;
    function Get_Bigwindows: IGMConfigBigwindowsType;
    function Get_Hydrogeoreps: IGMConfigHydrogeorepsType;
    function Get_Reports: IGMConfigReportsType;
    function Get_Auth: IGMAuthType;
    function Get_Scada: IGMScadaType;
    { Methods & Properties }
    property Sound: IGMConfigSoundType read Get_Sound;
    property Common: IGMConfigCommonType read Get_Common;
    property Objects: IGMConfigObjectsType read Get_Objects;
    property Alarms: IGMConfigAlarmsType read Get_Alarms;
    property Bigwindows: IGMConfigBigwindowsType read Get_Bigwindows;
    property Hydrogeoreps: IGMConfigHydrogeorepsType read Get_Hydrogeoreps;
    property Reports: IGMConfigReportsType read Get_Reports;
    property Auth: IGMAuthType read Get_Auth;
    property Scada: IGMScadaType read Get_Scada;
  end;

{ IGMAuthType }

  IGMScadaType = interface(IXMLNode)
    ['{D32F50F3-97E4-4AC8-AE92-22AC90EB3653}']
    { Property Accessors }
    function Get_Filename: UnicodeString;
    procedure Set_Filename(Value: UnicodeString);
    { Methods & Properties }
    property Filename: UnicodeString read Get_Filename write Set_Filename;
  end;

{ IGMAuthType }

  IGMAuthType = interface(IXMLNode)
    ['{175D4689-29F5-4F50-9D71-1F862C224C6D}']
    { Property Accessors }
    function Get_Login: UnicodeString;
    function Get_Password: UnicodeString;
    procedure Set_Login(Value: UnicodeString);
    procedure Set_Password(Value: UnicodeString);
    { Methods & Properties }
    property Login: UnicodeString read Get_Login write Set_Login;
    property Password: UnicodeString read Get_Password write Set_Password;
  end;

{ IGMConfigSoundType }

  IGMConfigSoundType = interface(IXMLNode)
    ['{E66242FA-E04E-4780-923F-79F387A64D3F}']
    { Property Accessors }
    function Get_AlarmSoundFile: WideString;
    function Get_NeedAlarmSound: int;
    function Get_NeedNoDataSound: int;
    function Get_BlinkingPanelWidth: int;
    procedure Set_AlarmSoundFile(const Value: WideString);
    procedure Set_NeedAlarmSound(const Value: int);
    procedure Set_NeedNoDataSound(const Value: int);
    procedure Set_BlinkingPanelWidth(const Value: int);
    { Methods & Properties }
    property AlarmSoundFile: WideString read Get_AlarmSoundFile write Set_AlarmSoundFile;
    property NeedNoDataSound: int read Get_NeedNoDataSound write Set_NeedNoDataSound;
    property NeedAlarmSound: int read Get_NeedAlarmSound write Set_NeedAlarmSound;
    property BlinkingPanelWidth: int read Get_BlinkingPanelWidth write Set_BlinkingPanelWidth;
  end;

{ IGMConfigCommonType }

  IGMConfigCommonType = interface(IXMLNode)
    ['{41E07877-66A1-4385-B992-1E13BA15F975}']
    { Property Accessors }
    function Get_Com: Integer;
    function Get_Baud: Integer;
    function Get_Wordlen: Integer;
    function Get_Parity: Integer;
    function Get_Stopbits: Integer;
    function Get_Timeout: Integer;
    function Get_Timeout2: Integer;
    function Get_Host: WideString;
    function Get_Port: Integer;
    function Get_Diagram_time: Integer;
    function Get_Alarm_window_height: Integer;
    function Get_Norefresh_alarm: Integer;
    function Get_Req_system: Integer;
    function Get_Timezone: Integer;
    function Get_Login: WideString;
    function Get_Password: WideString;
    procedure Set_Login(Value: WideString);
    procedure Set_Password(Value: WideString);
    procedure Set_Com(Value: Integer);
    procedure Set_Baud(Value: Integer);
    procedure Set_Wordlen(Value: Integer);
    procedure Set_Parity(Value: Integer);
    procedure Set_Stopbits(Value: Integer);
    procedure Set_Timeout(Value: Integer);
    procedure Set_Timeout2(Value: Integer);
    procedure Set_Host(Value: WideString);
    procedure Set_Port(Value: Integer);
    procedure Set_Diagram_time(Value: Integer);
    procedure Set_Alarm_window_height(Value: Integer);
    procedure Set_Norefresh_alarm(Value: Integer);
    procedure Set_Req_system(Value: Integer);
    procedure Set_Timezone(Value: Integer);
    { Methods & Properties }
    property Com: Integer read Get_Com write Set_Com;
    property Baud: Integer read Get_Baud write Set_Baud;
    property Wordlen: Integer read Get_Wordlen write Set_Wordlen;
    property Parity: Integer read Get_Parity write Set_Parity;
    property Stopbits: Integer read Get_Stopbits write Set_Stopbits;
    property Timeout: Integer read Get_Timeout write Set_Timeout;
    property Timeout2: Integer read Get_Timeout2 write Set_Timeout2;
    property Host: WideString read Get_Host write Set_Host;
    property Port: Integer read Get_Port write Set_Port;
    property Diagram_time: Integer read Get_Diagram_time write Set_Diagram_time;
    property Alarm_window_height: Integer read Get_Alarm_window_height write Set_Alarm_window_height;
    property Norefresh_alarm: Integer read Get_Norefresh_alarm write Set_Norefresh_alarm;
    property Req_system: Integer read Get_Req_system write Set_Req_system;
    property Timezone: Integer read Get_Timezone write Set_Timezone;
    property Login: WideString read Get_Login write Set_Login;
    property Password: WideString read Get_Password write Set_Password;
  end;

{ IGMConfigHydrogeorepsType }

  IGMConfigHydrogeorepsType = interface(IXMLNodeCollection)
    ['{A0F057B7-5FE9-4F68-8073-DD06C63BF812}']
    { Property Accessors }
    function Get_Hydroreport(Index: Integer): IGMConfigHydroreportType;
    { Methods & Properties }
    function Add: IGMConfigHydroreportType;
    function Insert(const Index: Integer): IGMConfigHydroreportType;
    property Hydroreport[Index: Integer]: IGMConfigHydroreportType read Get_Hydroreport; default;
  end;

{ IGMConfigHydroreportType }

  IGMConfigHydroreportType = interface(IXMLNode)
    ['{71A844A1-99C1-4157-A0C4-AA5524BE496E}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Level_prm: Integer;
    function Get_Flow_prm: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Level_prm(Value: Integer);
    procedure Set_Flow_prm(Value: Integer);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property LevelPrm: Integer read Get_Level_prm write Set_Level_prm;
    property FlowPrm: Integer read Get_Flow_prm write Set_Flow_prm;
  end;

{ IGMConfigObjectsType }

  IGMConfigObjectsType = interface(IXMLNodeCollection)
    ['{18C58CAB-60B3-4476-94C7-1EDA8356250D}']
    { Property Accessors }
    function Get_Object_(Index: Integer): IGMConfigObjectType;
    { Methods & Properties }
    function Add: IGMConfigObjectType;
    function Insert(const Index: Integer): IGMConfigObjectType;
    property Object_[Index: Integer]: IGMConfigObjectType read Get_Object_; default;
  end;

{ IGMConfigObjectType }

  IGMConfigObjectType = interface(IXMLNode)
    ['{6E2D69CF-34A4-40A2-96DF-166D7F0734F7}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_X: Integer;
    function Get_Y: Integer;
    function Get_Ticks: Integer;
    function Get_Color: Integer;
    function Get_Squeeze: Integer;
    function Get_SqueezeY: Integer;
    function Get_Id_obj: Integer;
    function Get_Channels: IGMConfigChannelsType;
    function Get_Tabs: IGMConfigTabsType;
    procedure Set_Name(Value: WideString);
    procedure Set_X(Value: Integer);
    procedure Set_Y(Value: Integer);
    procedure Set_Ticks(Value: Integer);
    procedure Set_Color(Value: Integer);
    procedure Set_Squeeze(Value: Integer);
    procedure Set_SqueezeY(Value: Integer);
    procedure Set_Id_obj(Value: Integer);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property X: Integer read Get_X write Set_X;
    property Y: Integer read Get_Y write Set_Y;
    property Ticks: Integer read Get_Ticks write Set_Ticks;
    property Color: Integer read Get_Color write Set_Color;
    property Squeeze: Integer read Get_Squeeze write Set_Squeeze;
    property SqueezeY: Integer read Get_SqueezeY write Set_SqueezeY;
    property Id_obj: Integer read Get_Id_obj write Set_Id_obj;
    property Channels: IGMConfigChannelsType read Get_Channels;
    property Tabs: IGMConfigTabsType read Get_Tabs;
  end;

{ IGMConfigChannelsType }

  IGMConfigChannelsType = interface(IXMLNodeCollection)
    ['{0389EE7B-CBDF-4E1A-95A5-9CC13508BA0C}']
    { Property Accessors }
    function Get_Channel(Index: Integer): IGMConfigChannelType;
    function Get_ChannelByNum(Num: Integer): IGMConfigChannelType;
    { Methods & Properties }
    function Add: IGMConfigChannelType;
    function Insert(const Index: Integer): IGMConfigChannelType;
    property Channel[Index: Integer]: IGMConfigChannelType read Get_Channel; default;
    property ChannelByNum[Num: Integer]: IGMConfigChannelType read Get_ChannelByNum;
  end;

{ IGMConfigChannelType }

  IGMConfigChannelType = interface(IXMLNode)
    ['{BBA21A90-DE1E-46CB-BAA7-18E4F0FC0E97}']
    { Property Accessors }
    function Get_Num: integer;
    function Get_Id_prm: Integer;
    function Get_Dmax: double;
    function Get_Dmin: double;
    function Get_Prefix: WideString;
    function Get_Postfix: WideString;
    function Get_Diagram: Integer;
    function Get_Barfloat: WideString;
    function Get_Pump_prm: Integer;
    function Get_Pump_signal: Integer;
    function Get_Digits: Integer;
    function Get_Showtype: Integer;
    function Get_DataChnType: Integer;
    procedure Set_DataChnType(Value: Integer);
    procedure Set_Num(Value: integer);
    procedure Set_Id_prm(Value: Integer);
    procedure Set_Dmax(Value: double);
    procedure Set_Dmin(Value: double);
    procedure Set_Prefix(Value: WideString);
    procedure Set_Postfix(Value: WideString);
    procedure Set_Diagram(Value: Integer);
    procedure Set_Barfloat(Value: WideString);
    procedure Set_Pump_prm(Value: Integer);
    procedure Set_Pump_signal(Value: Integer);
    procedure Set_Digits(Value: Integer);
    procedure Set_Showtype(Value: Integer);
    { Methods & Properties }
    property Num: integer read Get_Num write Set_Num;
    property Id_prm: Integer read Get_Id_prm write Set_Id_prm;
    property DataChnType: Integer read Get_DataChnType write Set_DataChnType;
    property Dmax: double read Get_Dmax write Set_Dmax;
    property Dmin: double read Get_Dmin write Set_Dmin;
    property Prefix: WideString read Get_Prefix write Set_Prefix;
    property Postfix: WideString read Get_Postfix write Set_Postfix;
    property Diagram: Integer read Get_Diagram write Set_Diagram;
    property Barfloat: WideString read Get_Barfloat write Set_Barfloat;
    property Pump_prm: Integer read Get_Pump_prm write Set_Pump_prm;
    property Pump_signal: Integer read Get_Pump_signal write Set_Pump_signal;
    property Digits: Integer read Get_Digits write Set_Digits;
    property Showtype: Integer read Get_Showtype write Set_Showtype;
  end;

{ IGMConfigTabsType }

  IGMConfigTabsType = interface(IXMLNodeCollection)
    ['{15AA2EE2-0363-4F6B-A464-FD25CFCA096B}']
    { Property Accessors }
    function Get_Tab(Index: Integer): IGMConfigTabType;
    { Methods & Properties }
    function Add: IGMConfigTabType;
    function Insert(const Index: Integer): IGMConfigTabType;
    property Tab[Index: Integer]: IGMConfigTabType read Get_Tab; default;
  end;

{ IGMConfigTabType }

  IGMConfigTabType = interface(IXMLNode)
    ['{2B5DA57D-B258-4686-A956-BA5756A24D8C}']
    { Property Accessors }
    function Get_Num: Integer;
    function Get_Channels: IGMConfigChannelsType;
    procedure Set_Num(Value: Integer);
    { Methods & Properties }
    property Num: Integer read Get_Num write Set_Num;
    property Channels: IGMConfigChannelsType read Get_Channels;
  end;

{ IGMConfigAlarmsType }

  IGMConfigAlarmsType = interface(IXMLNodeCollection)
    ['{F309D9FD-B642-45F9-B372-2F0DE9A3F9FF}']
    { Property Accessors }
    function Get_Alarm(Index: Integer): IGMConfigAlarmType;
    { Methods & Properties }
    function Add: IGMConfigAlarmType;
    function Insert(const Index: Integer): IGMConfigAlarmType;
    property Alarm[Index: Integer]: IGMConfigAlarmType read Get_Alarm; default;
  end;

{ IGMConfigAlarmType }

  IGMConfigAlarmType = interface(IXMLNode)
    ['{FEAFD4C5-2F64-4058-AB16-46C88A1A28CD}']
    { Property Accessors }
    function Get_Id_prm: Integer;
    procedure Set_Id_prm(Value: Integer);
    { Methods & Properties }
    property Id_prm: Integer read Get_Id_prm write Set_Id_prm;
  end;

{ IGMConfigBigwindowsType }

  IGMConfigBigwindowsType = interface(IXMLNodeCollection)
    ['{40BC41DA-B08C-425B-A23B-CFD3CC44348B}']
    { Property Accessors }
    function Get_Bigwindow(Index: Integer): IGMConfigBigwindowType;
    { Methods & Properties }
    function Add: IGMConfigBigwindowType;
    function Insert(const Index: Integer): IGMConfigBigwindowType;
    property Bigwindow[Index: Integer]: IGMConfigBigwindowType read Get_Bigwindow; default;
  end;

{ IGMConfigBigwindowType }

  IGMConfigBigwindowType = interface(IXMLNode)
    ['{4AB55C06-4B18-42D4-8FA4-EF4D11922C13}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Diagrams: IGMConfigDiagramsType;
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Diagrams: IGMConfigDiagramsType read Get_Diagrams;
  end;

{ IGMConfigDiagramsType }

  IGMConfigDiagramsType = interface(IXMLNodeCollection)
    ['{617D6377-7E40-491D-A64B-3F0D44071765}']
    { Property Accessors }
    function Get_Diagram(Index: Integer): IGMConfigDiagramType;
    { Methods & Properties }
    function Add: IGMConfigDiagramType;
    function Insert(const Index: Integer): IGMConfigDiagramType;
    property Diagram[Index: Integer]: IGMConfigDiagramType read Get_Diagram; default;
  end;

{ IGMConfigDiagramType }

  IGMConfigDiagramType = interface(IXMLNode)
    ['{E7AB87ED-1F75-4FF4-ABDD-09DE70D3502C}']
    { Property Accessors }
    function Get_Id_prm: Integer;
    function Get_Dmax: Double;
    function Get_Dmin: Double;
    function Get_Text: WideString;
    function Get_Diagram: Integer;
    function Get_ColWidth: Integer;
    function Get_DataChnType: Integer;
    function Get_Precision: Integer;
    procedure Set_Precision(Value: Integer);
    procedure Set_DataChnType(Value: Integer);
    procedure Set_Id_prm(Value: Integer);
    procedure Set_Dmax(Value: Double);
    procedure Set_Dmin(Value: Double);
    procedure Set_Text(Value: WideString);
    procedure Set_Diagram(Value: Integer);
    procedure Set_ColWidth(Value: Integer);
    { Methods & Properties }
    property Id_prm: Integer read Get_Id_prm write Set_Id_prm;
    property DataChnType: Integer read Get_DataChnType write Set_DataChnType;
    property Dmax: Double read Get_Dmax write Set_Dmax;
    property Dmin: Double read Get_Dmin write Set_Dmin;
    property Text: WideString read Get_Text write Set_Text;
    property Diagram: Integer read Get_Diagram write Set_Diagram;
    property Precision: Integer read Get_Precision write Set_Precision;
    property ColWidth: Integer read Get_ColWidth write Set_ColWidth;
  end;

{ IGMConfigReportsType }

  IGMConfigReportsType = interface(IXMLNodeCollection)
    ['{7068CCF2-1A5C-4D77-BC47-1753F7A89459}']
    { Property Accessors }
    function Get_Report(Index: Integer): IGMConfigReportType;
    { Methods & Properties }
    function Add: IGMConfigReportType;
    function Insert(const Index: Integer): IGMConfigReportType;
    property Report[Index: Integer]: IGMConfigReportType read Get_Report; default;
  end;

{ IGMConfigReportType }

  IGMConfigReportType = interface(IXMLNode)
    ['{9AE33FFC-9720-4E0F-A909-AD586C088D58}']
    { Property Accessors }
    function Get_ReportName: WideString;
    function Get_Template: WideString;
    function Get_ReportColumns: IGMConfigReportColumnsType;
    function Get_DataLength: integer;
    function Get_DataGroup: integer;
    function Get_DataType: integer;
    function Get_DataGroupLen: integer;
    procedure Set_DataGroupLen(Value: integer);
    procedure Set_DataType(Value: integer);
    procedure Set_ReportName(Value: WideString);
    procedure Set_Template(Value: WideString);
    procedure Set_DataLength(Value: integer);
    procedure Set_DataGroup(Value: integer);
    { Methods & Properties }
    property Name: WideString read Get_ReportName write Set_ReportName;
    property Template: WideString read Get_Template write Set_Template;
    property DataLength: integer read Get_DataLength write Set_DataLength;
    property DataGroup: integer read Get_DataGroup write Set_DataGroup;
    property DataGroupLen: integer read Get_DataGroupLen write Set_DataGroupLen;
    property DataType: Integer read Get_DataType write Set_DataType;
    property ReportColumns: IGMConfigReportColumnsType read Get_ReportColumns;
  end;

{ IGMConfigReportColumnsType }

  IGMConfigReportColumnsType = interface(IXMLNodeCollection)
    ['{09CD369B-A338-4811-A9BB-2126C7263325}']
    { Property Accessors }
    function Get_ReportColumn(Index: Integer): IGMConfigReportColumnType;
    { Methods & Properties }
    function Add: IGMConfigReportColumnType;
    function Insert(const Index: Integer): IGMConfigReportColumnType;
    property ReportColumn[Index: Integer]: IGMConfigReportColumnType read Get_ReportColumn; default;
  end;

{ IGMConfigReportColumnType }

  IGMConfigReportColumnType = interface(IXMLNode)
    ['{39C60285-FBDE-459B-9682-8CD388F5446A}']
    { Property Accessors }
    function Get_ID_Prm: Integer;
    function Get_Column: WideString;
    function Get_DataChnType: Integer;
    procedure Set_DataChnType(Value: Integer);
    procedure Set_ID_Prm(Value: Integer);
    procedure Set_Column(Value: WideString);
    { Methods & Properties }
    property ID_Prm: Integer read Get_ID_Prm write Set_ID_Prm;
    property DataChnType: Integer read Get_DataChnType write Set_DataChnType;
    property Column: WideString read Get_Column write Set_Column;
  end;  

{ Forward Decls }

  TGMConfigConfigType = class;
  TGMConfigCommonType = class;
  TGMConfigObjectsType = class;
  TGMConfigObjectType = class;
  TGMConfigChannelsType = class;
  TGMConfigChannelType = class;
  TGMConfigAlarmsType = class;
  TGMConfigAlarmType = class;
  TGMConfigBigwindowsType = class;
  TGMConfigBigwindowType = class;
  TGMConfigDiagramsType = class;
  TGMConfigDiagramType = class;
  TGMConfigHydrogeorepsType = class;
  TGMConfigHydroreportType = class;
  TGMConfigReportsType = class;
  TGMConfigReportType = class;
  TGMConfigReportColumnsType = class;
  TGMConfigReportColumnType = class;
  TGMAuthType = class;
  TGMScadaType = class;

{ TGMConfigConfigType }

  TGMConfigConfigType = class(TXMLNode, IGMConfigConfigType)
  protected
    { IGMConfigConfigType }
    function Get_Sound: IGMConfigSoundType;
    function Get_Common: IGMConfigCommonType;
    function Get_Objects: IGMConfigObjectsType;
    function Get_Alarms: IGMConfigAlarmsType;
    function Get_Bigwindows: IGMConfigBigwindowsType;
    function Get_HydroGeoReps: IGMConfigHydroGeoRepsType;
    function Get_Reports: IGMConfigReportsType;
    function Get_Auth: IGMAuthType;
    function Get_Scada: IGMScadaType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigSoundType }

  TGMConfigSoundType = class(TXMLNode, IGMConfigSoundType)
  protected
    { IGMConfigSoundType }
    function Get_AlarmSoundFile: WideString;
    function Get_NeedAlarmSound: int;
    function Get_NeedNoDataSound: int;
    function Get_BlinkingPanelWidth: int;
    procedure Set_AlarmSoundFile(const Value: WideString);
    procedure Set_NeedAlarmSound(const Value: int);
    procedure Set_NeedNoDataSound(const Value: int);
    procedure Set_BlinkingPanelWidth(const Value: int);
  end;

{ TGMConfigCommonType }

  TGMConfigCommonType = class(TXMLNode, IGMConfigCommonType)
  protected
    { IGMConfigCommonType }
    function Get_Com: Integer;
    function Get_Baud: Integer;
    function Get_Wordlen: Integer;
    function Get_Parity: Integer;
    function Get_Stopbits: Integer;
    function Get_Timeout: Integer;
    function Get_Timeout2: Integer;
    function Get_Host: WideString;
    function Get_Port: Integer;
    function Get_Diagram_time: Integer;
    function Get_Alarm_window_height: Integer;
    function Get_Norefresh_alarm: Integer;
    function Get_Req_system: Integer;
    function Get_Timezone: Integer;
    function Get_Login: WideString;
    function Get_Password: WideString;
    procedure Set_Login(Value: WideString);
    procedure Set_Password(Value: WideString);
    procedure Set_Com(Value: Integer);
    procedure Set_Baud(Value: Integer);
    procedure Set_Wordlen(Value: Integer);
    procedure Set_Parity(Value: Integer);
    procedure Set_Stopbits(Value: Integer);
    procedure Set_Timeout(Value: Integer);
    procedure Set_Timeout2(Value: Integer);
    procedure Set_Host(Value: WideString);
    procedure Set_Port(Value: Integer);
    procedure Set_Diagram_time(Value: Integer);
    procedure Set_Alarm_window_height(Value: Integer);
    procedure Set_Norefresh_alarm(Value: Integer);
    procedure Set_Req_system(Value: Integer);
    procedure Set_Timezone(Value: Integer);
  end;

{ TGMScadaType }

  TGMScadaType = class(TXMLNode, IGMScadaType)
  protected
    { IGMScadaType }
    function Get_Filename: UnicodeString;
    procedure Set_Filename(Value: UnicodeString);
  end;

{ TGMAuthType }

  TGMAuthType = class(TXMLNode, IGMAuthType)
  protected
    { IGMAuthType }
    function Get_Login: UnicodeString;
    function Get_Password: UnicodeString;
    procedure Set_Login(Value: UnicodeString);
    procedure Set_Password(Value: UnicodeString);
  end;

{ TGMConfigHydrogeorepsType }

  TGMConfigHydrogeorepsType = class(TXMLNodeCollection, IGMConfigHydrogeorepsType)
  protected
    { IGMConfigHydrogeorepsType }
    function Get_Hydroreport(Index: Integer): IGMConfigHydroreportType;
    function Add: IGMConfigHydroreportType;
    function Insert(const Index: Integer): IGMConfigHydroreportType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigHydroreportType }

  TGMConfigHydroreportType = class(TXMLNode, IGMConfigHydroreportType)
  protected
    { IGMConfigHydroreportType }
    function Get_Name: WideString;
    function Get_Level_prm: Integer;
    function Get_Flow_prm: Integer;
    procedure Set_Name(Value: WideString);
    procedure Set_Level_prm(Value: Integer);
    procedure Set_Flow_prm(Value: Integer);
  end;

{ TGMConfigObjectsType }

  TGMConfigObjectsType = class(TXMLNodeCollection, IGMConfigObjectsType)
  protected
    { IGMConfigObjectsType }
    function Get_Object_(Index: Integer): IGMConfigObjectType;
    function Add: IGMConfigObjectType;
    function Insert(const Index: Integer): IGMConfigObjectType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigObjectType }

  TGMConfigObjectType = class(TXMLNode, IGMConfigObjectType)
  protected
    { IGMConfigObjectType }
    function Get_Name: WideString;
    function Get_X: Integer;
    function Get_Y: Integer;
    function Get_Ticks: Integer;
    function Get_Color: Integer;
    function Get_Squeeze: Integer;
    function Get_SqueezeY: Integer;
    function Get_Id_obj: Integer;
    function Get_Channels: IGMConfigChannelsType;
    function Get_Tabs: IGMConfigTabsType;
    procedure Set_Name(Value: WideString);
    procedure Set_X(Value: Integer);
    procedure Set_Y(Value: Integer);
    procedure Set_Ticks(Value: Integer);
    procedure Set_Color(Value: Integer);
    procedure Set_Squeeze(Value: Integer);
    procedure Set_SqueezeY(Value: Integer);
    procedure Set_Id_obj(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigChannelsType }

  TGMConfigChannelsType = class(TXMLNodeCollection, IGMConfigChannelsType)
  protected
    { IGMConfigChannelsType }
    function Get_Channel(Index: Integer): IGMConfigChannelType;
    function Get_ChannelByNum(Num: Integer): IGMConfigChannelType;
    function Add: IGMConfigChannelType;
    function Insert(const Index: Integer): IGMConfigChannelType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigChannelType }

  TGMConfigChannelType = class(TXMLNode, IGMConfigChannelType)
  protected
    { IGMConfigChannelType }
    function Get_Num: integer;
    function Get_Id_prm: Integer;
    function Get_Dmax: double;
    function Get_Dmin: double;
    function Get_Prefix: WideString;
    function Get_Postfix: WideString;
    function Get_Diagram: Integer;
    function Get_Barfloat: WideString;
    function Get_Pump_prm: Integer;
    function Get_Pump_signal: Integer;
    function Get_Digits: Integer;
    function Get_Showtype: Integer;
    function Get_DataChnType: Integer;
    procedure Set_DataChnType(Value: Integer);
    procedure Set_Num(Value: integer);
    procedure Set_Id_prm(Value: Integer);
    procedure Set_Dmax(Value: double);
    procedure Set_Dmin(Value: double);
    procedure Set_Prefix(Value: WideString);
    procedure Set_Postfix(Value: WideString);
    procedure Set_Diagram(Value: Integer);
    procedure Set_Barfloat(Value: WideString);
    procedure Set_Pump_prm(Value: Integer);
    procedure Set_Pump_signal(Value: Integer);
    procedure Set_Digits(Value: Integer);
    procedure Set_Showtype(Value: Integer);
  end;

{ TGMConfigTabsType }

  TGMConfigTabsType = class(TXMLNodeCollection, IGMConfigTabsType)
  protected
    { IGMConfigTabsType }
    function Get_Tab(Index: Integer): IGMConfigTabType;
    function Add: IGMConfigTabType;
    function Insert(const Index: Integer): IGMConfigTabType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigTabType }

  TGMConfigTabType = class(TXMLNode, IGMConfigTabType)
  protected
    { IGMConfigTabType }
    function Get_Num: Integer;
    function Get_Channels: IGMConfigChannelsType;
    procedure Set_Num(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigAlarmsType }

  TGMConfigAlarmsType = class(TXMLNodeCollection, IGMConfigAlarmsType)
  protected
    { IGMConfigAlarmsType }
    function Get_Alarm(Index: Integer): IGMConfigAlarmType;
    function Add: IGMConfigAlarmType;
    function Insert(const Index: Integer): IGMConfigAlarmType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigAlarmType }

  TGMConfigAlarmType = class(TXMLNode, IGMConfigAlarmType)
  protected
    { IGMConfigAlarmType }
    function Get_Id_prm: Integer;
    procedure Set_Id_prm(Value: Integer);
  end;

{ TGMConfigBigwindowsType }

  TGMConfigBigwindowsType = class(TXMLNodeCollection, IGMConfigBigwindowsType)
  protected
    { IGMConfigBigwindowsType }
    function Get_Bigwindow(Index: Integer): IGMConfigBigwindowType;
    function Add: IGMConfigBigwindowType;
    function Insert(const Index: Integer): IGMConfigBigwindowType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigBigwindowType }

  TGMConfigBigwindowType = class(TXMLNode, IGMConfigBigwindowType)
  protected
    { IGMConfigBigwindowType }
    function Get_Name: WideString;
    function Get_Diagrams: IGMConfigDiagramsType;
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigDiagramsType }

  TGMConfigDiagramsType = class(TXMLNodeCollection, IGMConfigDiagramsType)
  protected
    { IGMConfigDiagramsType }
    function Get_Diagram(Index: Integer): IGMConfigDiagramType;
    function Add: IGMConfigDiagramType;
    function Insert(const Index: Integer): IGMConfigDiagramType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigDiagramType }

  TGMConfigDiagramType = class(TXMLNode, IGMConfigDiagramType)
  protected
    { IGMConfigDiagramType }
    function Get_Id_prm: Integer;
    function Get_Dmax: Double;
    function Get_Dmin: Double;
    function Get_Text: WideString;
    function Get_Diagram: Integer;
    function Get_ColWidth: Integer;
    function Get_DataChnType: Integer;
    function Get_Precision: Integer;
    procedure Set_Precision(Value: Integer);
    procedure Set_DataChnType(Value: Integer);
    procedure Set_Id_prm(Value: Integer);
    procedure Set_Dmax(Value: Double);
    procedure Set_Dmin(Value: Double);
    procedure Set_Text(Value: WideString);
    procedure Set_Diagram(Value: Integer);
    procedure Set_ColWidth(Value: Integer);
  end;

{ TGMConfigReportsType }

  TGMConfigReportsType = class(TXMLNodeCollection, IGMConfigReportsType)
  protected
    { IGMConfigReportsType }
    function Get_Report(Index: Integer): IGMConfigReportType;
    function Add: IGMConfigReportType;
    function Insert(const Index: Integer): IGMConfigReportType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigReportType }

  TGMConfigReportType = class(TXMLNode, IGMConfigReportType)
  protected
    { IGMConfigReportType }
    function Get_ReportName: WideString;
    function Get_Template: WideString;
    function Get_ReportColumns: IGMConfigReportColumnsType;
    function Get_DataLength: integer;
    function Get_DataGroup: integer;
    function Get_DataType: integer;
    function Get_DataGroupLen: integer;
    procedure Set_DataGroupLen(Value: integer);
    procedure Set_DataType(Value: integer);
    procedure Set_ReportName(Value: WideString);
    procedure Set_Template(Value: WideString);
    procedure Set_DataLength(Value: integer);
    procedure Set_DataGroup(Value: integer);
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigReportColumnsType }

  TGMConfigReportColumnsType = class(TXMLNodeCollection, IGMConfigReportColumnsType)
  protected
    { IGMConfigReportColumnsType }
    function Get_ReportColumn(Index: Integer): IGMConfigReportColumnType;
    function Add: IGMConfigReportColumnType;
    function Insert(const Index: Integer): IGMConfigReportColumnType;
  public
    procedure AfterConstruction; override;
  end;

{ TGMConfigReportColumnType }

  TGMConfigReportColumnType = class(TXMLNode, IGMConfigReportColumnType)
  protected
    { IGMConfigReportColumnType }
    function Get_ID_Prm: Integer;
    function Get_Column: WideString;
    function Get_DataChnType: Integer;
    procedure Set_DataChnType(Value: Integer);
    procedure Set_ID_Prm(Value: Integer);
    procedure Set_Column(Value: WideString);
  end;  

{ Global Functions }

function Newconfig: IGMConfigConfigType;

function ReadConfigXML(): IGMConfigConfigType;
procedure SaveConfigXML(xml: IGMConfigConfigType);

procedure AdjustChannelCount(channels: IGMConfigChannelsType);

const
  TargetNamespace = '';

implementation

{ Global Functions }

uses AppConfigFile;

function Getconfig(Doc: IXMLDocument): IGMConfigConfigType;
begin
  Result := Doc.GetDocBinding('config', TGMConfigConfigType, TargetNamespace) as IGMConfigConfigType;
end;

function Loadconfig(const FileName: WideString): IGMConfigConfigType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('config', TGMConfigConfigType, TargetNamespace) as IGMConfigConfigType;
end;

procedure AdjustChannelCount(channels: IGMConfigChannelsType);
var i: int;
begin
  for i := channels.Count to NominalEditBoxOnLineCount - 1 do // добьем к-во каналов до 4х + bar
    channels.Add().Num := i;
end;

procedure PrepareConfigXML(xml: IGMConfigConfigType);
var i, j, k: int;
    tab: IGMConfigTabType;
    chn: IGMConfigChannelType;
begin
  for i := 0 to xml.Objects.Count - 1 do
  begin
    // переход от старой схемы с хранениями каналов прямо на объекте
    // к многострочной схеме
    if (xml.Objects[i].Channels <> nil) and (xml.Objects[i].Channels.Count > 0) then
    begin
      tab := xml.Objects[i].Tabs.Add();
      for j := 0 to xml.Objects[i].Channels.Count - 1 do
      begin
        chn := tab.Channels.Add();

        // в будущем возможно появятся новые св-ва каналов
        // но сюда их вставлять не обязательно, их в старых конфигах быть не может
        chn.Num := xml.Objects[i].Channels[j].Num;
        chn.Id_prm := xml.Objects[i].Channels[j].Id_prm;
        chn.DataChnType := xml.Objects[i].Channels[j].DataChnType;
        chn.Dmax := xml.Objects[i].Channels[j].Dmax;
        chn.Dmin := xml.Objects[i].Channels[j].Dmin;
        chn.Prefix := xml.Objects[i].Channels[j].Prefix;
        chn.Postfix := xml.Objects[i].Channels[j].Postfix;
        chn.Diagram := xml.Objects[i].Channels[j].Diagram;
        chn.Barfloat := xml.Objects[i].Channels[j].Barfloat;
        chn.Pump_prm := xml.Objects[i].Channels[j].Pump_prm;
        chn.Pump_signal := xml.Objects[i].Channels[j].Pump_signal;
        chn.Digits := xml.Objects[i].Channels[j].Digits;
        chn.Showtype := xml.Objects[i].Channels[j].Showtype;
      end;

      xml.Objects[i].Channels.Clear();
    end;

    for k := 0 to xml.Objects[i].Tabs.Count - 1 do
    begin
      AdjustChannelCount(xml.Objects[i].Tabs[k].Channels);
    end;
  end;
end;

function ReadConfigXML(): IGMConfigConfigType;
var fn: string;
begin
  fn := GMMainConfigFile.GetMainINIFileName();
  if not FileExists(fn) then
  begin
    ShowMessageBox('Файл ' + fn + ' не найден!', MB_ICONSTOP);
    Application.Terminate();
  end;

  Result := Loadconfig(fn);
  PrepareConfigXML(Result);
end;

procedure SaveConfigXML(xml: IGMConfigConfigType);
var s: string;
begin
  s := '<?xml version="1.0" encoding="Windows-1251"?>'#13#10 + FormatXMLData(xml.XML);
  SaveStringToFile(s, GMMainConfigFile.GetMainINIFileName())
end;

function Newconfig: IGMConfigConfigType;
begin
  Result := NewXMLDocument.GetDocBinding('config', TGMConfigConfigType, TargetNamespace) as IGMConfigConfigType;
end;

function GetIntFromChildNodeDef(Children: IXMLNodeList; const ChildName: WideString; Def: int = 0): int;
begin
  if Children.FindNode(ChildName) <> nil then
    Result := StrToIntDef(Children[ChildName].NodeValue, def)
  else
    Result := def;
end;

function GetStrFromChildNodeDef(Children: IXMLNodeList; const ChildName: WideString; Def: WideString = ''): WideString;
begin
  if Children.FindNode(ChildName) <> nil then
    Result := Children[ChildName].Text
  else
    Result := def;
end;

{ TGMConfigConfigType }

procedure TGMConfigConfigType.AfterConstruction;
begin
  RegisterChildNode('common', TGMConfigCommonType);
  RegisterChildNode('sound', TGMConfigSoundType);
  RegisterChildNode('objects', TGMConfigObjectsType);
  RegisterChildNode('alarms', TGMConfigAlarmsType);
  RegisterChildNode('bigwindows', TGMConfigBigwindowsType);
  RegisterChildNode('hydrogeoreps', TGMConfigHydrogeorepsType);
  RegisterChildNode('reports', TGMConfigReportsType);
  RegisterChildNode('auth', TGMAuthType);
  RegisterChildNode('scada', TGMScadaType);
  inherited;
end;

function TGMConfigConfigType.Get_Common: IGMConfigCommonType;
begin
  Result := ChildNodes['common'] as IGMConfigCommonType;
end;

function TGMConfigConfigType.Get_Objects: IGMConfigObjectsType;
begin
  Result := ChildNodes['objects'] as IGMConfigObjectsType;
end;

function TGMConfigConfigType.Get_Alarms: IGMConfigAlarmsType;
begin
  Result := ChildNodes['alarms'] as IGMConfigAlarmsType;
end;

function TGMConfigConfigType.Get_Auth: IGMAuthType;
begin
  Result := ChildNodes['auth'] as IGMAuthType;
end;

function TGMConfigConfigType.Get_Bigwindows: IGMConfigBigwindowsType;
begin
  Result := ChildNodes['bigwindows'] as IGMConfigBigwindowsType;
end;

function TGMConfigConfigType.Get_Hydrogeoreps: IGMConfigHydrogeorepsType;
begin
  Result := ChildNodes['hydrogeoreps'] as IGMConfigHydrogeorepsType;
end;

function TGMConfigConfigType.Get_Reports: IGMConfigReportsType;
begin
  Result := ChildNodes['reports'] as IGMConfigReportsType;
end;

function TGMConfigConfigType.Get_Scada: IGMScadaType;
begin
  Result := ChildNodes['scada'] as IGMScadaType;
end;

function TGMConfigConfigType.Get_Sound: IGMConfigSoundType;
begin
  Result := ChildNodes['sound'] as IGMConfigSoundType;
end;

{ TGMConfigCommonType }

function TGMConfigCommonType.Get_Com: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'com');
end;

procedure TGMConfigCommonType.Set_Com(Value: Integer);
begin
  ChildNodes['com'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Baud: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'baud', 9600);
end;

procedure TGMConfigCommonType.Set_Baud(Value: Integer);
begin
  ChildNodes['baud'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Wordlen: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'wordlen', 8);
end;

procedure TGMConfigCommonType.Set_Wordlen(Value: Integer);
begin
  ChildNodes['wordlen'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Parity: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'parity');
end;

function TGMConfigCommonType.Get_Password: WideString;
begin
  Result := GetStrFromChildNodeDef(ChildNodes, 'password');
end;

procedure TGMConfigCommonType.Set_Parity(Value: Integer);
begin
  ChildNodes['parity'].NodeValue := Value;
end;

procedure TGMConfigCommonType.Set_Password(Value: WideString);
begin
  ChildNodes['password'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Stopbits: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'stopbits');
end;

procedure TGMConfigCommonType.Set_Stopbits(Value: Integer);
begin
  ChildNodes['stopbits'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Timeout: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'timeout', 2000);
end;

function TGMConfigCommonType.Get_Timeout2: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'timeout2', 100);
end;

procedure TGMConfigCommonType.Set_Timeout(Value: Integer);
begin
  ChildNodes['timeout'].NodeValue := Value;
end;

procedure TGMConfigCommonType.Set_Timeout2(Value: Integer);
begin
  ChildNodes['timeout2'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Host: WideString;
begin
  Result := GetStrFromChildNodeDef(ChildNodes, 'host');
end;

function TGMConfigCommonType.Get_Login: WideString;
begin
  Result := GetStrFromChildNodeDef(ChildNodes, 'login');
end;

procedure TGMConfigCommonType.Set_Host(Value: WideString);
begin
  ChildNodes['host'].NodeValue := Value;
end;

procedure TGMConfigCommonType.Set_Login(Value: WideString);
begin
  ChildNodes['login'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Port: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'port', 65500);
end;

procedure TGMConfigCommonType.Set_Port(Value: Integer);
begin
  ChildNodes['port'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Diagram_time: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'diagram_time', 24);
end;

procedure TGMConfigCommonType.Set_Diagram_time(Value: Integer);
begin
  ChildNodes['diagram_time'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Alarm_window_height: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'alarm_window_height');
end;

procedure TGMConfigCommonType.Set_Alarm_window_height(Value: Integer);
begin
  ChildNodes['alarm_window_height'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Norefresh_alarm: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'norefresh_alarm', 4);
end;

procedure TGMConfigCommonType.Set_Norefresh_alarm(Value: Integer);
begin
  ChildNodes['norefresh_alarm'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Req_system: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'req_system');
end;

procedure TGMConfigCommonType.Set_Req_system(Value: Integer);
begin
  ChildNodes['req_system'].NodeValue := Value;
end;

function TGMConfigCommonType.Get_Timezone: Integer;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'timezone', 6);
end;

procedure TGMConfigCommonType.Set_Timezone(Value: Integer);
begin
  ChildNodes['timezone'].NodeValue := Value;
end;

{ TGMConfigHydrogeorepsType }

procedure TGMConfigHydrogeorepsType.AfterConstruction;
begin
  RegisterChildNode('hydroreport', TGMConfigHydroreportType);
  ItemTag := 'hydroreport';
  ItemInterface := IGMConfigHydroreportType;
  inherited;
end;

function TGMConfigHydrogeorepsType.Get_Hydroreport(Index: Integer): IGMConfigHydroreportType;
begin
  Result := List[Index] as IGMConfigHydroreportType;
end;

function TGMConfigHydrogeorepsType.Add: IGMConfigHydroreportType;
begin
  Result := AddItem(-1) as IGMConfigHydroreportType;
end;

function TGMConfigHydrogeorepsType.Insert(const Index: Integer): IGMConfigHydroreportType;
begin
  Result := AddItem(Index) as IGMConfigHydroreportType;
end;

{ TGMConfigHydroreportType }

function TGMConfigHydroreportType.Get_Name: WideString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TGMConfigHydroreportType.Set_Name(Value: WideString);
begin
  SetAttribute('name', Value);
end;

function TGMConfigHydroreportType.Get_Level_prm: Integer;
begin
  Result := AttributeNodes['level_prm'].NodeValue;
end;

procedure TGMConfigHydroreportType.Set_Level_prm(Value: Integer);
begin
  SetAttribute('level_prm', Value);
end;

function TGMConfigHydroreportType.Get_Flow_prm: Integer;
begin
  Result := AttributeNodes['flow_prm'].NodeValue;
end;

procedure TGMConfigHydroreportType.Set_Flow_prm(Value: Integer);
begin
  SetAttribute('flow_prm', Value);
end;

{ TGMConfigObjectsType }

procedure TGMConfigObjectsType.AfterConstruction;
begin
  RegisterChildNode('object', TGMConfigObjectType);
  ItemTag := 'object';
  ItemInterface := IGMConfigObjectType;
  inherited;
end;

function TGMConfigObjectsType.Get_Object_(Index: Integer): IGMConfigObjectType;
begin
  Result := List[Index] as IGMConfigObjectType;
end;

function TGMConfigObjectsType.Add: IGMConfigObjectType;
begin
  Result := AddItem(-1) as IGMConfigObjectType;
end;

function TGMConfigObjectsType.Insert(const Index: Integer): IGMConfigObjectType;
begin
  Result := AddItem(Index) as IGMConfigObjectType;
end;

{ TGMConfigObjectType }

procedure TGMConfigObjectType.AfterConstruction;
begin
  RegisterChildNode('channels', TGMConfigChannelsType);
  RegisterChildNode('tabs', TGMConfigTabsType);
  inherited;
end;

function TGMConfigObjectType.Get_Name: WideString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TGMConfigObjectType.Set_Name(Value: WideString);
begin
  SetAttribute('name', Value);
end;

function TGMConfigObjectType.Get_X: Integer;
begin
  Result := AttributeNodes['x'].NodeValue;
end;

procedure TGMConfigObjectType.Set_X(Value: Integer);
begin
  SetAttribute('x', Value);
end;

function TGMConfigObjectType.Get_Y: Integer;
begin
  Result := AttributeNodes['y'].NodeValue;
end;

procedure TGMConfigObjectType.Set_Y(Value: Integer);
begin
  SetAttribute('y', Value);
end;

function TGMConfigObjectType.Get_Tabs: IGMConfigTabsType;
begin
  Result := ChildNodes['tabs'] as IGMConfigTabsType;
end;

function TGMConfigObjectType.Get_Ticks: Integer;
begin
  Result := AttributeNodes['ticks'].NodeValue;
end;

procedure TGMConfigObjectType.Set_Ticks(Value: Integer);
begin
  SetAttribute('ticks', Value);
end;

function TGMConfigObjectType.Get_Color: Integer;
begin
  Result := AttributeNodes['color'].NodeValue;
end;

procedure TGMConfigObjectType.Set_Color(Value: Integer);
begin
  SetAttribute('color', Value);
end;

function TGMConfigObjectType.Get_Squeeze: Integer;
begin
  Result := AttributeNodes['squeeze'].NodeValue;
end;

procedure TGMConfigObjectType.Set_Squeeze(Value: Integer);
begin
  SetAttribute('squeeze', Value);
end;

function TGMConfigObjectType.Get_Id_obj: Integer;
begin
  Result := AttributeNodes['id_obj'].NodeValue;
end;

procedure TGMConfigObjectType.Set_Id_obj(Value: Integer);
begin
  SetAttribute('id_obj', Value);
end;

function TGMConfigObjectType.Get_Channels: IGMConfigChannelsType;
begin
  Result := ChildNodes['channels'] as IGMConfigChannelsType;
end;

function TGMConfigObjectType.Get_SqueezeY: Integer;
begin
  Result := AttributeNodes['squeezey'].NodeValue;
  if Result = 0 then
    Result := 100;
end;

procedure TGMConfigObjectType.Set_SqueezeY(Value: Integer);
begin
  SetAttribute('squeezey', Value);
end;

{ TGMConfigChannelsType }

procedure TGMConfigChannelsType.AfterConstruction;
begin
  RegisterChildNode('channel', TGMConfigChannelType);
  ItemTag := 'channel';
  ItemInterface := IGMConfigChannelType;
  inherited;
end;

function TGMConfigChannelsType.Get_Channel(Index: Integer): IGMConfigChannelType;
begin
  Result := List[Index] as IGMConfigChannelType;
end;

function TGMConfigChannelsType.Add: IGMConfigChannelType;
begin
  Result := AddItem(-1) as IGMConfigChannelType;
end;

function TGMConfigChannelsType.Insert(const Index: Integer): IGMConfigChannelType;
begin
  Result := AddItem(Index) as IGMConfigChannelType;
end;

function TGMConfigChannelsType.Get_ChannelByNum(Num: Integer): IGMConfigChannelType;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Get_Channel(i).Num = Num then
    begin
      Result := Get_Channel(i);
      Exit;
    end;
end;

{ TGMConfigChannelType }

function TGMConfigChannelType.Get_Id_prm: Integer;
begin
  Result := AttributeNodes['id_prm'].NodeValue;
end;

procedure TGMConfigChannelType.Set_Id_prm(Value: Integer);
begin
  SetAttribute('id_prm', Value);
end;

function TGMConfigChannelType.Get_Dmax: double;
begin
  Result := MyStrToFloatDef(AttributeNodes['dmax'].Text, 100);
end;

procedure TGMConfigChannelType.Set_Dmax(Value: double);
begin
  SetAttribute('dmax', Value);
end;

function TGMConfigChannelType.Get_Dmin: double;
begin
  Result := MyStrToFloatDef(AttributeNodes['dmin'].Text, 0);
end;

procedure TGMConfigChannelType.Set_Dmin(Value: double);
begin
  SetAttribute('dmin', Value);
end;

function TGMConfigChannelType.Get_Prefix: WideString;
begin
  Result := AttributeNodes['prefix'].Text;
end;

procedure TGMConfigChannelType.Set_Prefix(Value: WideString);
begin
  SetAttribute('prefix', Value);
end;

function TGMConfigChannelType.Get_Postfix: WideString;
begin
  Result := AttributeNodes['postfix'].Text;
end;

procedure TGMConfigChannelType.Set_Postfix(Value: WideString);
begin
  SetAttribute('postfix', Value);
end;

function TGMConfigChannelType.Get_DataChnType: Integer;
begin
  Result := AttributeNodes['datachntype'].NodeValue;
end;

function TGMConfigChannelType.Get_Diagram: Integer;
begin
  Result := AttributeNodes['diagram'].NodeValue;
end;

procedure TGMConfigChannelType.Set_DataChnType(Value: Integer);
begin
  SetAttribute('datachntype', Value);
end;

procedure TGMConfigChannelType.Set_Diagram(Value: Integer);
begin
  SetAttribute('diagram', Value);
end;

function TGMConfigChannelType.Get_Barfloat: WideString;
begin
  Result := AttributeNodes['barfloat'].Text;
end;

procedure TGMConfigChannelType.Set_Barfloat(Value: WideString);
begin
  SetAttribute('barfloat', Value);
end;

function TGMConfigChannelType.Get_Pump_prm: Integer;
begin
  Result := AttributeNodes['pump_prm'].NodeValue;
end;

procedure TGMConfigChannelType.Set_Pump_prm(Value: Integer);
begin
  SetAttribute('pump_prm', Value);
end;

function TGMConfigChannelType.Get_Pump_signal: Integer;
begin
  Result := AttributeNodes['pump_signal'].NodeValue;
end;

procedure TGMConfigChannelType.Set_Pump_signal(Value: Integer);
begin
  SetAttribute('pump_signal', Value);
end;

function TGMConfigChannelType.Get_Digits: Integer;
begin
  Result := AttributeNodes['digits'].NodeValue;
end;

procedure TGMConfigChannelType.Set_Digits(Value: Integer);
begin
  SetAttribute('digits', Value);
end;

function TGMConfigChannelType.Get_Showtype: Integer;
begin
  Result := AttributeNodes['showtype'].NodeValue;
end;

procedure TGMConfigChannelType.Set_Showtype(Value: Integer);
begin
  SetAttribute('showtype', Value);
end;

function TGMConfigChannelType.Get_Num: integer;
begin
  Result := AttributeNodes['num'].NodeValue;
end;

procedure TGMConfigChannelType.Set_Num(Value: integer);
begin
  SetAttribute('num', Value);
end;

{ TGMConfigAlarmsType }

procedure TGMConfigAlarmsType.AfterConstruction;
begin
  RegisterChildNode('alarm', TGMConfigAlarmType);
  ItemTag := 'alarm';
  ItemInterface := IGMConfigAlarmType;
  inherited;
end;

function TGMConfigAlarmsType.Get_Alarm(Index: Integer): IGMConfigAlarmType;
begin
  Result := List[Index] as IGMConfigAlarmType;
end;

function TGMConfigAlarmsType.Add: IGMConfigAlarmType;
begin
  Result := AddItem(-1) as IGMConfigAlarmType;
end;

function TGMConfigAlarmsType.Insert(const Index: Integer): IGMConfigAlarmType;
begin
  Result := AddItem(Index) as IGMConfigAlarmType;
end;

{ TGMConfigAlarmType }

function TGMConfigAlarmType.Get_Id_prm: Integer;
begin
  Result := AttributeNodes['id_prm'].NodeValue;
end;

procedure TGMConfigAlarmType.Set_Id_prm(Value: Integer);
begin
  SetAttribute('id_prm', Value);
end;

{ TGMConfigBigwindowsType }

procedure TGMConfigBigwindowsType.AfterConstruction;
begin
  RegisterChildNode('bigwindow', TGMConfigBigwindowType);
  ItemTag := 'bigwindow';
  ItemInterface := IGMConfigBigwindowType;
  inherited;
end;

function TGMConfigBigwindowsType.Get_Bigwindow(Index: Integer): IGMConfigBigwindowType;
begin
  Result := List[Index] as IGMConfigBigwindowType;
end;

function TGMConfigBigwindowsType.Add: IGMConfigBigwindowType;
begin
  Result := AddItem(-1) as IGMConfigBigwindowType;
end;

function TGMConfigBigwindowsType.Insert(const Index: Integer): IGMConfigBigwindowType;
begin
  Result := AddItem(Index) as IGMConfigBigwindowType;
end;

{ TGMConfigBigwindowType }

procedure TGMConfigBigwindowType.AfterConstruction;
begin
  RegisterChildNode('diagrams', TGMConfigDiagramsType);
  inherited;
end;

function TGMConfigBigwindowType.Get_Name: WideString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TGMConfigBigwindowType.Set_Name(Value: WideString);
begin
  SetAttribute('name', Value);
end;

function TGMConfigBigwindowType.Get_Diagrams: IGMConfigDiagramsType;
begin
  Result := ChildNodes['diagrams'] as IGMConfigDiagramsType;
end;

{ TGMConfigDiagramsType }

procedure TGMConfigDiagramsType.AfterConstruction;
begin
  RegisterChildNode('diagram', TGMConfigDiagramType);
  ItemTag := 'diagram';
  ItemInterface := IGMConfigDiagramType;
  inherited;
end;

function TGMConfigDiagramsType.Get_Diagram(Index: Integer): IGMConfigDiagramType;
begin
  Result := List[Index] as IGMConfigDiagramType;
end;

function TGMConfigDiagramsType.Add: IGMConfigDiagramType;
begin
  Result := AddItem(-1) as IGMConfigDiagramType;
end;

function TGMConfigDiagramsType.Insert(const Index: Integer): IGMConfigDiagramType;
begin
  Result := AddItem(Index) as IGMConfigDiagramType;
end;

{ TGMConfigDiagramType }

function TGMConfigDiagramType.Get_Id_prm: Integer;
begin
  Result := AttributeNodes['id_prm'].NodeValue;
end;

function TGMConfigDiagramType.Get_Precision: Integer;
begin
  Result := StrToIntDef(AttributeNodes['precision'].Text, -1);
end;

procedure TGMConfigDiagramType.Set_Id_prm(Value: Integer);
begin
  SetAttribute('id_prm', Value);
end;

procedure TGMConfigDiagramType.Set_Precision(Value: Integer);
begin
  SetAttribute('precision', Value);
end;

function TGMConfigDiagramType.Get_Dmax: Double;
begin
  Result := AttributeNodes['dmax'].NodeValue;
end;

procedure TGMConfigDiagramType.Set_Dmax(Value: Double);
begin
  SetAttribute('dmax', Value);
end;

function TGMConfigDiagramType.Get_Dmin: Double;
begin
  Result := AttributeNodes['dmin'].NodeValue;
end;

procedure TGMConfigDiagramType.Set_Dmin(Value: Double);
begin
  SetAttribute('dmin', Value);
end;

function TGMConfigDiagramType.Get_Text: WideString;
begin
  Result := AttributeNodes['text'].Text;
end;

procedure TGMConfigDiagramType.Set_Text(Value: WideString);
begin
  SetAttribute('text', Value);
end;

function TGMConfigDiagramType.Get_Diagram: Integer;
begin
  Result := GetIntFromChildNodeDef(AttributeNodes, 'diagram');
end;

procedure TGMConfigDiagramType.Set_Diagram(Value: Integer);
begin
  SetAttribute('diagram', Value);
end;

function TGMConfigDiagramType.Get_DataChnType: Integer;
begin
  Result := GetIntFromChildNodeDef(AttributeNodes, 'datachntype');
end;

function TGMConfigDiagramType.Get_ColWidth: Integer;
begin
  Result := GetIntFromChildNodeDef(AttributeNodes, 'colwidth');
end;

procedure TGMConfigDiagramType.Set_DataChnType(Value: Integer);
begin
  SetAttribute('datachntype', Value);
end;

procedure TGMConfigDiagramType.Set_ColWidth(Value: Integer);
begin
  SetAttribute('colwidth', Value);
end;

{ TGMConfigReportsType }

procedure TGMConfigReportsType.AfterConstruction;
begin
  RegisterChildNode('Report', TGMConfigReportType);
  ItemTag := 'Report';
  ItemInterface := IGMConfigReportType;
  inherited;
end;

function TGMConfigReportsType.Get_Report(Index: Integer): IGMConfigReportType;
begin
  Result := List[Index] as IGMConfigReportType;
end;

function TGMConfigReportsType.Add: IGMConfigReportType;
begin
  Result := AddItem(-1) as IGMConfigReportType;
end;

function TGMConfigReportsType.Insert(const Index: Integer): IGMConfigReportType;
begin
  Result := AddItem(Index) as IGMConfigReportType;
end;

{ TGMConfigReportType }

procedure TGMConfigReportType.AfterConstruction;
begin
  RegisterChildNode('ReportColumns', TGMConfigReportColumnsType);
  inherited;
end;

function TGMConfigReportType.Get_ReportName: WideString;
begin
  Result := ChildNodes['ReportName'].Text;
end;

procedure TGMConfigReportType.Set_ReportName(Value: WideString);
begin
  ChildNodes['ReportName'].NodeValue := Value;
end;

function TGMConfigReportType.Get_Template: WideString;
begin
  Result := ChildNodes['Template'].Text;
end;

procedure TGMConfigReportType.Set_Template(Value: WideString);
begin
  ChildNodes['Template'].NodeValue := Value;
end;

function TGMConfigReportType.Get_ReportColumns: IGMConfigReportColumnsType;
begin
  Result := ChildNodes['ReportColumns'] as IGMConfigReportColumnsType;
end;

function TGMConfigReportType.Get_DataLength: integer;
begin
  Result := StrToIntDef(ChildNodes['DataLength'].NodeValue, -1);
end;

function TGMConfigReportType.Get_DataType: integer;
begin
  Result := StrToIntDef(ChildNodes['DataType'].NodeValue, DATATYPE_CURRENT_DIAGRAM);
end;

procedure TGMConfigReportType.Set_DataLength(Value: integer);
begin
  ChildNodes['DataLength'].NodeValue := Value;
end;

procedure TGMConfigReportType.Set_DataType(Value: integer);
begin
  ChildNodes['DataType'].NodeValue := Value;
end;

function TGMConfigReportType.Get_DataGroup: integer;
begin
  Result := ChildNodes['DataGroup'].NodeValue;
end;

function TGMConfigReportType.Get_DataGroupLen: integer;
begin
  Result := ChildNodes['DataGroupLen'].NodeValue;
end;

procedure TGMConfigReportType.Set_DataGroup(Value: integer);
begin
  ChildNodes['DataGroup'].NodeValue := Value;
end;

procedure TGMConfigReportType.Set_DataGroupLen(Value: integer);
begin
  ChildNodes['DataGroupLen'].NodeValue := Value;
end;

{ TGMConfigReportColumnsType }

procedure TGMConfigReportColumnsType.AfterConstruction;
begin
  RegisterChildNode('ReportColumn', TGMConfigReportColumnType);
  ItemTag := 'ReportColumn';
  ItemInterface := IGMConfigReportColumnType;
  inherited;
end;

function TGMConfigReportColumnsType.Get_ReportColumn(Index: Integer): IGMConfigReportColumnType;
begin
  Result := List[Index] as IGMConfigReportColumnType;
end;

function TGMConfigReportColumnsType.Add: IGMConfigReportColumnType;
begin
  Result := AddItem(-1) as IGMConfigReportColumnType;
end;

function TGMConfigReportColumnsType.Insert(const Index: Integer): IGMConfigReportColumnType;
begin
  Result := AddItem(Index) as IGMConfigReportColumnType;
end;

{ TGMConfigReportColumnType }

function TGMConfigReportColumnType.Get_DataChnType: Integer;
begin
  Result := AttributeNodes['datachntype'].NodeValue;
end;

function TGMConfigReportColumnType.Get_ID_Prm: Integer;
begin
  Result := AttributeNodes['ID_Prm'].NodeValue;
end;

procedure TGMConfigReportColumnType.Set_ID_Prm(Value: Integer);
begin
  SetAttribute('ID_Prm', Value);
end;

function TGMConfigReportColumnType.Get_Column: WideString;
begin
  Result := AttributeNodes['Column'].Text;
end;

procedure TGMConfigReportColumnType.Set_Column(Value: WideString);
begin
  SetAttribute('Column', Value);
end;

procedure TGMConfigReportColumnType.Set_DataChnType(Value: Integer);
begin
   SetAttribute('datachntype', Value);
end;

{ TGMConfigSoundType }

function TGMConfigSoundType.Get_AlarmSoundFile: WideString;
begin
  Result := GetStrFromChildNodeDef(ChildNodes, 'AlarmSoundFile', 'Danger.mp3');
end;

function TGMConfigSoundType.Get_BlinkingPanelWidth: int;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'BlinkingPanelWidth', 0);
end;

function TGMConfigSoundType.Get_NeedAlarmSound: int;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'NeedAlarmSound', 1);
end;

function TGMConfigSoundType.Get_NeedNoDataSound: int;
begin
  Result := GetIntFromChildNodeDef(ChildNodes, 'NeedNoDataSound');
end;

procedure TGMConfigSoundType.Set_AlarmSoundFile(const Value: WideString);
begin
  ChildNodes['AlarmSoundFile'].NodeValue := Value;
end;

procedure TGMConfigSoundType.Set_BlinkingPanelWidth(const Value: int);
begin
  ChildNodes['BlinkingPanelWidth'].NodeValue := Value;
end;

procedure TGMConfigSoundType.Set_NeedAlarmSound(const Value: int);
begin
  ChildNodes['NeedAlarmSound'].NodeValue := Value;
end;

procedure TGMConfigSoundType.Set_NeedNoDataSound(const Value: int);
begin
  ChildNodes['NeedNoDataSound'].NodeValue := Value;
end;

{ TGMAuthType }

function TGMAuthType.Get_Login: UnicodeString;
begin
  Result := AttributeNodes['login'].Text;
end;

procedure TGMAuthType.Set_Login(Value: UnicodeString);
begin
  SetAttribute('login', Value);
end;

function TGMAuthType.Get_Password: UnicodeString;
begin
  Result := AttributeNodes['password'].Text;
end;

procedure TGMAuthType.Set_Password(Value: UnicodeString);
begin
  SetAttribute('password', Value);
end;

{ TGMConfigTabsType }

procedure TGMConfigTabsType.AfterConstruction;
begin
  RegisterChildNode('tab', TGMConfigTabType);
  ItemTag := 'tab';
  ItemInterface := IGMConfigTabType;
  inherited;
end;

function TGMConfigTabsType.Get_Tab(Index: Integer): IGMConfigTabType;
begin
  Result := List[Index] as IGMConfigTabType;
end;

function TGMConfigTabsType.Add: IGMConfigTabType;
begin
  Result := AddItem(-1) as IGMConfigTabType;
end;

function TGMConfigTabsType.Insert(const Index: Integer): IGMConfigTabType;
begin
  Result := AddItem(Index) as IGMConfigTabType;
end;

{ TGMConfigTabType }

procedure TGMConfigTabType.AfterConstruction;
begin
  RegisterChildNode('channels', TGMConfigChannelsType);
  inherited;
end;

function TGMConfigTabType.Get_Num: Integer;
begin
  Result := AttributeNodes['num'].NodeValue;
end;

procedure TGMConfigTabType.Set_Num(Value: Integer);
begin
  SetAttribute('num', Value);
end;

function TGMConfigTabType.Get_Channels: IGMConfigChannelsType;
begin
  Result := ChildNodes['channels'] as IGMConfigChannelsType;
end;

{ TGMScadaType }

function TGMScadaType.Get_Filename: UnicodeString;
begin
  Result := ChildNodes['filename'].NodeValue;
end;

procedure TGMScadaType.Set_Filename(Value: UnicodeString);
begin
  ChildNodes['filename'].NodeValue := Value;
end;

initialization
  NullStrictConvert := false;
end.