////////////////////////////////////////////
// Гланое окно сервера опроса
////////////////////////////////////////////
unit IOMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShellAPI, AppEvnts, GMGlobals, ActiveX, IniFiles,
  StdCtrls, DateUtils, Buttons, ComCtrls, ExtCtrls,
  Menus, StrUtils, CheckLst, VirtualTrees, DateComboBox, Frame.EditAggrControl,
  Frame.ControlOneAddPrm, ImgList, Threads.Base,  GMConst, GMDBClasses, GMSqlQuery,
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPClient, IdUDPServer, IdSocketHandle,
  VirtualPropertyTree, ConnParamsStorage, Frame.NodeTree;

const ObjectLimitLicense = 8;

type
  TNCarIDObjRelation = class(TCollectionItem)
  public
    N_Car: int;
    ID_Obj: int;
    ObjType: int;
  end;

  TNCarIDObjRelationList = class(TCollection)
  public
    function ByIDObj(ID_Obj: int): TNCarIDObjRelation;
    function ByNCar(N_Car: int; ObjTypes: SetOfInt): TNCarIDObjRelation;
  end;

  TGMTreeEditMode = (temReadOnly, temEditTree, temEditControls);
  THideObjectType = (hotShowAll, hotHideCOMGeomers);

  TGMTreeType = (gmttObjectTree, gmttNodeTree);

  TServerStateThread = class(TGMThread)
  private
    procedure CheckObjectsOnline;
    procedure CheckSqlQueue;
    procedure ReadObjectState(q: TGMSqlQuery; obj: pointer);
    procedure CheckDataWritten;
    procedure ReadDataWritten(q: TGMSqlQuery; obj: pointer);
    procedure CheckDevicesOnline;
    procedure ReadDeviceState(q: TGMSqlQuery; obj: pointer);
    procedure CheckSqlQueueOneParam(const statesType: string; msg: int);
  protected
    procedure SafeExecute(); override;
  end;

  TIOMainFrm = class(TForm)
    ApplicationEvents1: TApplicationEvents;
    Panel1: TPanel;
    Panel3: TPanel;
    btAddObj: TBitBtn;
    btDel: TBitBtn;
    btAddDev: TBitBtn;
    pcConfig: TPageControl;
    tsPrm: TTabSheet;
    Tree: TVirtualStringTree;
    tsEmpty: TTabSheet;
    pmDevices: TPopupMenu;
    pmTree: TPopupMenu;
    miTreeAddDevice: TMenuItem;
    miTreeAddLostParams: TMenuItem;
    miTreeDel: TMenuItem;
    ilTree: TImageList;
    tmrTreeRefr: TTimer;
    pcObj: TPageControl;
    tsObjCommon: TTabSheet;
    tsObjControl: TTabSheet;
    pcControl: TPageControl;
    tsControl1: TTabSheet;
    EditAggrFrame1: TEditAggrFrame;
    tsControl2: TTabSheet;
    EditAggrFrame2: TEditAggrFrame;
    tsControl3: TTabSheet;
    EditAggrFrame3: TEditAggrFrame;
    tsCtrlAddParams: TTabSheet;
    EditAggrFrame4: TEditAggrFrame;
    miTreeAddIntegralPrm: TMenuItem;
    N1: TMenuItem;
    miAddTeconUserPrm: TMenuItem;
    miTreeChnMenu: TMenuItem;
    N2: TMenuItem;
    miTreeAddObject: TMenuItem;
    N3: TMenuItem;
    miAddCntMtr: TMenuItem;
    MainMenu1: TMainMenu;
    N4: TMenuItem;
    miExit: TMenuItem;
    N6: TMenuItem;
    miEditModeTree: TMenuItem;
    miEditModeControls: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    tsNode: TTabSheet;
    NodeTreeFrame: TfrmNodeTree;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    miView: TMenuItem;
    N7: TMenuItem;
    miTreeAddLostParams_All: TMenuItem;
    vpt: TVirtualPropertyTree;
    gbLastData: TGroupBox;
    lePrmLastDT: TLabeledEdit;
    lePrmLastVal: TLabeledEdit;
    miForceArchRequest: TMenuItem;
    miForceArchRequest_Day: TMenuItem;
    miForceArchRequest_Month: TMenuItem;
    miForceArchRequest_Full: TMenuItem;
    miStructure: TMenuItem;
    N5: TMenuItem;
    N8: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btDelClick(Sender: TObject);
    procedure btAddObjClick(Sender: TObject);
    procedure btAddDevClick(Sender: TObject);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ePrmNSrcChange(Sender: TObject);
    procedure miTreeDelClick(Sender: TObject);
    procedure miTreeAddObjectClick(Sender: TObject);
    procedure pmTreePopup(Sender: TObject);
    procedure miTreeAddLostParamsClick(Sender: TObject);
    procedure TreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure tsCtrlAddParamsResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tmrTreeRefrTimer(Sender: TObject);
    procedure pmDevicesPopup(Sender: TObject);
    procedure miTreeAddIntegralPrmClick(Sender: TObject);
    procedure miAddTeconUserPrmClick(Sender: TObject);
    procedure vptFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure vptExit(Sender: TObject);
    procedure TreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure miAddCntMtrClick(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miEditModeTreeClick(Sender: TObject);
    procedure miEditModeControlsClick(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure N10Click(Sender: TObject);
    procedure vptCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure N7Click(Sender: TObject);
    procedure TreeEnter(Sender: TObject);
    procedure NodeTreeFrameTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure NodeTreeFrameTreeEnter(Sender: TObject);
    procedure NodeTreeFrameTreeFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure NodeTreeFrameTreeDblClick(Sender: TObject);
    procedure miTreeAddLostParams_AllClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure TreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
      Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure miForceArchRequest_FullClick(Sender: TObject);
    procedure miStructureClick(Sender: TObject);
    procedure miViewClick(Sender: TObject);
    procedure N8Click(Sender: TObject);
  private
    { Private declarations }
    FClosingInProcess: bool;
    FEditMode: TGMTreeEditMode;
    slID_Src, slUserSrc: TStringList;
    slPrmArcType: TStringList;
    slPrmDIAlarmSignal: TStringList;
    slObjType: TStringList;
    slDevBaudrate: TStringList;
    slDevPortType: TStringList;
    slNodeType: TStringList;

    slCalibrationType: TStringList;
    slCalibrationResistor: TStringList;
    slMeterCalcNITypes: TStringList;
    slDevType: TStringList;
    slPrmType: TStringList;
    slPrmMeaUnits: TStringList;
    slReqIntervalType: TStringList;
    slMinIntegralInterval: TStringList;
    slTecomHourArchDepth: TStringList;
    slConverter: TStringList;
    slModbusResType: TStringList;

    DateRestriction: TDateTime;

    qTmp: TGMSqlQuery;
    CurrentTreeType: TGMTreeType;

    thrSvcState: TServerStateThread;

    FSqlQueue: int;
    FSqlQueue_DT: LongWord;
    FBlockQueue: int;
    FBlockQueue_DT: LongWord;

    procedure IconAction(Action: DWORD);
    procedure On_MYWM_NOTIFYICON(var msg: TMsg); message WM_ACTIVATE_FROM_TRAY;
    procedure WMDeviceOnline(var Msg: TMessage); message WM_DEVICE_ONLINE;
    procedure WMObjectOnline(var Msg: TMessage); message WM_OBJECT_ONLINE;
    procedure WMSQLQueue(var Msg: TMessage); message WM_SQL_QUEUE;
    procedure WMParserQueue(var Msg: TMessage); message WM_PARSER_QUEUE;
    procedure WMParamDataWritten(var Msg: TMessage); message WM_PARAM_DATA_WRITTEN;
    procedure RefreshTreeAndDBLists;
    function GetTreeNodeData(Node: PVirtualNode = nil): PVTNodeData;
    procedure InitNode(Node: PVirtualNode);
    procedure btAddDevMenuClick(Sender: TObject);
    procedure ReadDevices(ndObj: PVirtualNode; CycleObject: pointer = nil);
    procedure ReadDeviceParams(ndDev: PVirtualNode; CycleObject: pointer = nil);
    procedure AddDevice(ndObj: PVirtualNode; ID_DevType: int);
    procedure GetDevTypesList();
    procedure ExecSQL(const sql: string);
    procedure DelCurrentTreeItem;
    procedure AddObjectToTree;
    procedure AddDevicesAsMenuItems(Parent: TMenuItem);
    procedure AddDeviceLostParams(ID_Device, argument: int);
    procedure GetPrmTypesList(bAddEmpty: bool = false);
    procedure PopulateListFromSQL(lst: TStrings; const SQL, NameField,
      IDField: string);
    procedure GetMeaUnitsTypesList(bAddEmpty: bool = false);
    procedure CheckObjectControlScheme;
    procedure ShowObjects(ID_Obj_Except: int);
    procedure GetDraggingParam(var ID_Prm: int);
    function GetPrmTextForControl(ID_Prm: int): string;
    procedure SetEditMode(tem: TGMTreeEditMode);
    function ReadAndCheckSQLParams: bool;
    procedure PostVndNodeSettings(vnd: PVTNodeData);
    procedure PostVndNodeSettings_Chn(vnd: PVTNodeData);
    procedure PostVndNodeSettings_Node(vnd: PVTNodeData);
    procedure EnableEditorButtons;
    procedure FillNodeChannelEditor(vnd: PVTNodeData);
    procedure FillNodeEditor(vnd: PVTNodeData);
    procedure GetNodeTypesList;
    procedure CycleTree_LookForParam(nd: PVirtualNode; CycleObject: pointer);
    procedure ChangeNodeTreeFocus(nd: PVirtualNode);
    procedure ChangeObjectTree(nd: PVirtualNode);
    procedure FillNodeTreeEditor(vnd: PVTNodeData);
    procedure WMDeviceOnline_CycleTree(nd: PVirtualNode; CycleObject: pointer);
    procedure WMObjectOnline_CycleTree(nd: PVirtualNode; CycleObject: pointer);
    function CheckLastAccess(nd: PVirtualNode; tLastAccess: TDateTime): bool;
    function IsCurrentDeviceSupportsUserChannels(ndCurrent: PVirtualNode = nil): bool;
    function GetCurrentNodeDevType(ndCurrent: PVirtualNode = nil): int;
    function IsRemoteServerNode(nd: PVirtualNode): bool;
    procedure FillObjectEditor_EnableControls(bRemote: bool);
    procedure FillObjectEditor_Object(nd: PVirtualNode);
    function VPTPropertyEdited_UpdateProperty(Field: PField; vnd: PVTNodeData): bool;
    procedure VPTPropertyEdited_PostVndSettings(vnd: PVTNodeData);
    procedure VPTPropertyEdited_FillNodeEditor(nd: PVirtualNode);
    function GetCurrentTree: TVirtualStringTree;
    procedure UpdateStatusBar;
    procedure ShowNodeTree(AShow: bool);
    property EditMode: TGMTreeEditMode read FEditMode write SetEditMode;
    procedure ShowObjectsInTreeByCriteria(nd: PVirtualNode; CycleObject: pointer = nil);
    function GetDevTypeByID(ID_DevType: int): string;
    function GetPrmTxt(nd: PVirtualNode; ppc: TParamPathComponents): string;
    function ReadINI: bool;
    procedure PopulateConstCmbs;
    procedure HideCOMObjectsGeomers(hot: THideObjectType);
    procedure RefreshNCarIDObjRelations;
    function GetTopLevelParent(nd: PVirtualNode; Level: int = 0): PVirtualNode;
    function MainConnectSQL(Params: TZConnectionParams): bool;
    procedure SetParamLastVal(nd: PVirtualNode; CycleObject: pointer);
    procedure ApplyPrmLastVals;
    procedure HideCOMObjectsGeomers_proc(nd: PVirtualNode; CycleObject: pointer);
    function GetDeviceNode(ndCurrent: PVirtualNode = nil): PVirtualNode;
    function AddUserDefinedPrm(ID_Src: int; ParentLevel: Cardinal; Unique: bool): PVirtualNode;
    function ParsePrmNSrc(const s: string; bForceHex: bool = false): int;
    function IsCurrentDeviceTecon(ndCurrent: PVirtualNode = nil): bool;
    function NSrcToStr(NSrc: int; nd: PVirtualNode = nil; bForce: bool = false): string;
    procedure FillObjectEditor(nd: PVirtualNode);
    procedure FillObjectEditor_Param(nd: PVirtualNode);
    function AddVPTField(const Name: string;
      PropertyType: TVPTPropertyType;
      const Category: string;
      Enabled: bool = true): VirtualPropertyTree.PField;
    procedure SetVPTCmbVal(field: PField; slValues: TStrings; val: Variant);
    procedure SetVPTFieldIntVal(field: PField; val: int);
    procedure VPTPropertyEdited(Sender: TObject; Field: PField);
    procedure SetVPTFieldFloatVal(field: PField; val: double);
    function CalcCalibrationMark(vnd: PVTNodeData): double;
    procedure PostVndSettings_Object(vnd: PVTNodeData);
    procedure PostVndSettings_Device(vnd: PVTNodeData);
    procedure PostVndSettings_Param(vnd: PVTNodeData);
    procedure SetVPTFieldVariantVal(field: PField; val: Variant);
    function GetParentNodeWithLevel(Level: Cardinal;
      ndCurrent: PVirtualNode = nil): PVirtualNode;
    function FindPrm(ID_Prm: int; ndDev: PVirtualNode): PVirtualNode;
    function CheckObjectCountRestriction: bool;
    procedure FillObjectEditor_Device(nd: PVirtualNode);
    function CheckDateRestriction: bool;
    function CheckRestrictions: bool;
    function IsOriginalMtr(nd: PVirtualNode): bool;
    property CurrentTree: TVirtualStringTree read GetCurrentTree;
  public
    { Public declarations }
    fIgnoreOld_Hours: double;
  end;

var
  IOMainFrm: TIOMainFrm;
  clNCarIDObjRelations: TNCarIDObjRelationList;

implementation

uses Math, CheckConnStr, VT_Utils, SysConfig,
  ProgramLogFile, AppConfigFile, SQLNodeReader, UsefulQueries, UserPermissions;

var nSimulatorCounter: int = 0;
    synch: TMultiReadExclusiveWriteSynchronizer;

{$R *.dfm}

function TIOMainFrm.GetTreeNodeData(Node: PVirtualNode = nil): PVTNodeData;
var nd: PVirtualNode;
begin
  nd := Node;

  if nd = nil then
    nd := Tree.FocusedNode;

  Result := PVTNodeData(Tree.GetNodeData(nd));
end;      

procedure TIOMainFrm.IconAction(Action: DWORD);
var ICON: HICON;
    lpszTip: PChar;
    tnid: NOTIFYICONDATA;
begin
  tnid.cbSize := sizeof(NOTIFYICONDATA);
  tnid.Wnd := Handle;
  tnid.uID := 100;
  ICON:=Application.Icon.Handle;

  tnid.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  tnid.uCallbackMessage := WM_ACTIVATE_FROM_TRAY;
  tnid.hIcon := icon;

  lpszTip := PChar(Application.Title);
  if lpszTip<>'' then
      lstrcpyn(tnid.szTip, lpszTip, sizeof(tnid.szTip))
  else
      tnid.szTip[0] := #0;

  Shell_NotifyIcon(Action, @tnid);
end;

procedure TIOMainFrm.On_MYWM_NOTIFYICON(var msg:TMsg);
begin
  if msg.wParam=513 then
  begin
    ShowWindow(Application.Handle,SW_RESTORE);
    WindowState:=wsNormal;
    Application.ProcessMessages();
    Application.BringToFront();
  end;
end;

type THideObjectClass = class
  hot: THideObjectType;
end;

procedure TIOMainFrm.HideCOMObjectsGeomers_proc(nd: PVirtualNode; CycleObject: pointer);
var bHide: bool;
begin
  bHide := false;
  if TObject(CycleObject) is THideObjectClass then
    bHide := (Tree.GetNodeLevel(nd) = 1)
             and (THideObjectClass(CycleObject).hot = hotHideCOMGeomers)
             and (GetTreeNodeData(nd).ID_DevType in GeomerFamily)
             and not (GetTreeNodeData(nd.Parent).ObjType in [OBJ_TYPE_GM, OBJ_TYPE_REMOTE_SRV]);

  Tree.IsVisible[nd] := not bHide;
end;

procedure TIOMainFrm.HideCOMObjectsGeomers(hot: THideObjectType);
var c: THideObjectClass;
begin
  c := THideObjectClass.Create();
  c.hot := hot;
  CycleTree(Tree.RootNode, HideCOMObjectsGeomers_proc, c);
  c.Free();
  Tree.Refresh();
end;

procedure TIOMainFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IconAction(NIM_DELETE);
end;

procedure TIOMainFrm.ApplicationEvents1Minimize(Sender: TObject);
begin
{$ifndef DEBUG}
//  IconAction(NIM_ADD);
//  ShowWindow(Application.Handle,SW_HIDE);
{$endif}
end;

procedure TIOMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
{$ifndef DEBUG}
  CanClose := ShowMessageBox('Выход?', MB_YESNO or MB_ICONQUESTION) = IDYES;
{$else}
  CanClose := true;
{$endif}
  FClosingInProcess := CanClose;
end;

procedure TIOMainFrm.PopulateConstCmbs();
begin
  slID_Src := TStringList.Create();
  slID_Src.AddObject('AI - Аналоговый вход', TObject(SRC_AI));
  slID_Src.AddObject('DI - Дискретный вход', TObject(SRC_DI));
  slID_Src.AddObject('AO - Аналоговый выход', TObject(SRC_AO));
  slID_Src.AddObject('DO - Дискретный выход', TObject(SRC_DO));
  slID_Src.AddObject('CNT - Счетный вход', TObject(SRC_CNT_DI));
  slID_Src.AddObject('SUM - Интегральный', TObject(SRC_SUM_AI));
  slID_Src.AddObject('USR - Пользовательский', TObject(SRC_USR));
  slID_Src.AddObject('MTR - Показания', TObject(SRC_CNT_MTR));

  slUserSrc := TStringList.Create();
  slUserSrc.AddObject('< По умолчанию >', nil);
  slUserSrc.AddObject('AI - Аналоговый вход', TObject(SRC_AI));
  slUserSrc.AddObject('DI - Дискретный вход', TObject(SRC_DI));
  slUserSrc.AddObject('CNT - Счетный вход', TObject(SRC_CNT_DI));
  slUserSrc.AddObject('MTR - Показания', TObject(SRC_CNT_MTR));
  slUserSrc.AddObject('AO - Аналоговый выход', TObject(SRC_AO));
  slUserSrc.AddObject('DO - Дискретный выход', TObject(SRC_DO));

  slPrmArcType := TStringList.Create();
  slPrmArcType.AddObject('Хранить вечно', TObject(0));
  slPrmArcType.AddObject('Только текущие', TObject(1));
  slPrmArcType.AddObject('Короткий архив', TObject(2));
  slPrmArcType.AddObject('Длинный архив', TObject(3));

  slPrmDIAlarmSignal := TStringList.Create();
  slPrmDIAlarmSignal.AddObject('< Нет >', TObject(0));
  slPrmDIAlarmSignal.AddObject(' NC', TObject(1));
  slPrmDIAlarmSignal.AddObject(' NO', TObject(2));

  slObjType := TStringList.Create();
  slObjType.AddObject('Геомер', TObject(OBJ_TYPE_GM));
  slObjType.AddObject('СОМ-порт', TObject(OBJ_TYPE_COM));
  slObjType.AddObject('Удаленный сервер', TObject(OBJ_TYPE_REMOTE_SRV));
  slObjType.AddObject('K-104', TObject(OBJ_TYPE_K104));
  slObjType.AddObject('K-105', TObject(OBJ_TYPE_K105));
  slObjType.AddObject('AnCom RM', TObject(OBJ_TYPE_ANCOM));
  slObjType.AddObject('Ethernet', TObject(OBJ_TYPE_TCP));
  slObjType.AddObject('Головной сервер', TObject(OBJ_TYPE_REMOTE_MAIN));

  slDevBaudrate := TStringList.Create();
  slDevBaudrate.AddObject('2400', TObject(2400));
  slDevBaudrate.AddObject('4800', TObject(4800));
  slDevBaudrate.AddObject('9600', TObject(9600));
  slDevBaudrate.AddObject('19200', TObject(19200));
  slDevBaudrate.AddObject('33600', TObject(33600));
  slDevBaudrate.AddObject('38400', TObject(38400));
  slDevBaudrate.AddObject('115200', TObject(115200));

  slDevPortType := TStringList.Create();
  slDevPortType.AddObject('RS-485', TObject(PORTTYPE_RS485));
  slDevPortType.AddObject('RS-232', TObject(PORTTYPE_RS232));

  slCalibrationType := TStringList.Create();
  slCalibrationType.AddObject('Вручную', TObject(CALIBR_TYPE_MANUAL));
  slCalibrationType.AddObject('Напряжение 0 - 1 В', TObject(CALIBR_TYPE_U_0_1));
  slCalibrationType.AddObject('Напряжение 0 - 5 В', TObject(CALIBR_TYPE_U_0_5));
  slCalibrationType.AddObject('Напряжение 0 - 10 В', TObject(CALIBR_TYPE_U_0_10));
  slCalibrationType.AddObject('Ток 4 - 20 мА', TObject(CALIBR_TYPE_I_4_20));
  slCalibrationType.AddObject('Ток 0 - 20 мА', TObject(CALIBR_TYPE_I_0_20));
  slCalibrationType.AddObject('Ток 4 - 5 мА', TObject(CALIBR_TYPE_I_0_5));

  slCalibrationResistor := TStringList.Create();
  slCalibrationResistor.AddObject('< Нет >', nil);
  slCalibrationResistor.AddObject(' 100 Ом', TObject(100));
  slCalibrationResistor.AddObject(' 500 Ом', TObject(500));
  slCalibrationResistor.AddObject(' 1000 Ом', TObject(1000));

  slMeterCalcNITypes := TStringList.Create();
  slMeterCalcNITypes.AddObject('Расчетные данные', TObject(CALC_NI_TYPE_CALC));
  slMeterCalcNITypes.AddObject('Показания прибора', TObject(CALC_NI_TYPE_LAST));

  slReqIntervalType := TStringList.Create();
  slReqIntervalType.AddObject('Обычный', TObject(0));
  slReqIntervalType.AddObject('Накопительный', TObject(1));

  slMinIntegralInterval := TStringList.Create();
  slMinIntegralInterval.AddObject('Нет', TObject(0));
  slMinIntegralInterval.AddObject('5 мин', TObject(5));
  slMinIntegralInterval.AddObject('10 мин', TObject(10));
  slMinIntegralInterval.AddObject('15 мин', TObject(15));
  slMinIntegralInterval.AddObject('20 мин', TObject(20));
  slMinIntegralInterval.AddObject('30 мин', TObject(30));
  slMinIntegralInterval.AddObject('1 час', TObject(60));

  slTecomHourArchDepth := TStringList.Create();
  slTecomHourArchDepth.AddObject('< Нет >', nil);
  slTecomHourArchDepth.AddObject('16 суток', TObject(16));
  slTecomHourArchDepth.AddObject('32 суток', TObject(32));
  slTecomHourArchDepth.AddObject('64 суток', TObject(64));

  slConverter := TStringList.Create();
  slConverter.AddObject('< Нет >', nil);
  slConverter.AddObject('Modbus RTU/TCP', TObject(PROTOCOL_CONVETER_MODBUS_TCP_RTU));

  slModbusResType := TStringList.Create();
  slModbusResType.AddObject('< По умолчанию >', nil);
  slModbusResType.AddObject('WORD(2B) младшим вперед', TObject(MODBUS_RESULT_WORD_LITTLE_ENDIAN));
  slModbusResType.AddObject('Int(4B) старшим вперед', TObject(MODBUS_RESULT_LONG_BIG_ENDIAN));
  slModbusResType.AddObject('Int(4B) младшим вперед', TObject(MODBUS_RESULT_LONG_LITTLE_ENDIAN));
  slModbusResType.AddObject('Float(4B) старшим вперед', TObject(MODBUS_RESULT_SINGLE_BIG_ENDIAN));
  slModbusResType.AddObject('Float(4B) младшим вперед', TObject(MODBUS_RESULT_SINGLE_LITTLE_ENDIAN));
  slModbusResType.AddObject('Float(4B) смешанный', TObject(MODBUS_RESULT_SINGLE_SHUFFLE));
  slModbusResType.AddObject('Double(8B) старшим вперед', TObject(MODBUS_RESULT_DOUBLE_BIG_ENDIAN));
  slModbusResType.AddObject('Double(8B) младшим вперед', TObject(MODBUS_RESULT_DOUBLE_LITTLE_ENDIAN));
end;

function TIOMainFrm.CheckRestrictions(): bool;
begin
  Result := CheckDateRestriction();
end;

procedure TIOMainFrm.FormCreate(Sender: TObject);
begin
  ProgramLog().AddMessage('================ Start GMIOPSrv ================');

  FSqlQueue := 0;
  FSqlQueue_DT := 0;
  FBlockQueue := 0;
  FBlockQueue_DT := 0;

  Tree.NodeDataSize := SizeOf(TVTNodeData);
  SetMainConfigFileClass(TGMServerMainConfigFile);

  DateRestriction := EncodeDate(2013, 3, 1);
  Caption := Application.Title;
  FClosingInProcess := false;
  EditMode := temEditTree;

  slDevType := TStringList.Create();
  slPrmType := TStringList.Create();
  slPrmMeaUnits := TStringList.Create();
  slNodeType := TStringList.Create();

  PopulateConstCmbs();

  qTmp := TGMSqlQuery.Create();
  thrSvcState := nil;

  if not ReadINI() or not CheckRestrictions() then
  begin
    Application.Terminate();
    Exit;
  end;

  thrSvcState := TServerStateThread.Create();

  pcConfig.ActivePage := tsEmpty;
  PageControl_HideAllTabs(pcConfig, tsEmpty);

  RefreshNCarIDObjRelations();

  tsObjCommon.TabVisible := false;
  tsObjControl.TabVisible := false;
  pcObj.ActivePage := tsObjCommon;

  vpt.OnPropertyChange := VPTPropertyEdited;

  lePrmLastVal.EditLabel.Caption := ''; // никак не удаляется с редактора

  NodeTreeFrame.Load(true, TSQLNodeReader);
  ShowNodeTree(NodeTreeFrame.Tree.RootNode.ChildCount > 0);
end;

function TIOMainFrm.FindPrm(ID_Prm: int; ndDev: PVirtualNode): PVirtualNode;
var nd: PVirtualNode;
begin
  Result := nil;
  nd := ndDev.FirstChild;
  while nd <> nil do
  begin
    if GetTreeNodeData(nd).ID_Prm = ID_Prm then
    begin
      Result := nd;
      Exit;
    end;

    nd := nd.NextSibling;
  end;
end;

procedure TIOMainFrm.ReadDeviceParams(ndDev: PVirtualNode; CycleObject: pointer = nil);
var ndPrm, ndBasePrm: PVirtualNode;
    vndPrm: PVTNodeData;
begin
  if Tree.GetNodeLevel(ndDev) <> NODE_LEVEL_DEVICE then Exit;

  qTmp.Close();
  qTmp.SQL.Text :=
    'select *, case when COALESCE(BaseChn, 0) <= 0  then 0 else BaseChn end as BaseOrder ' +
    ' from Params p ' +
    '   left outer join ParamTypes pt on p.ID_PT = pt.ID_PT ' +
    '   left outer join Devices d  on d.ID_Device = p.ID_Device ' +
    ' where p.ID_Device = ' + IntToStr(GetTreeNodeData(ndDev).ID_Device) +
    ' order by BaseOrder, ID_Src, N_Src';

  qTmp.Open();
  while not qTmp.Eof do
  begin
    ndPrm := nil;

    if qTmp.FieldByName('BaseChn').AsInteger > 0 then
    begin
      ndBasePrm := FindPrm(qTmp.FieldByName('BaseChn').AsInteger, ndDev);
      if ndBasePrm <> nil then
        ndPrm := Tree.AddChild(ndBasePrm);
    end
    else
      ndPrm := Tree.AddChild(ndDev);

    if ndPrm <> nil then
    begin
      InitNode(ndPrm);
      vndPrm := GetTreeNodeData(ndPrm);

      vndPrm.ID_Obj := GetTreeNodeData(ndDev).ID_Obj;
      vndPrm.ID_Device := GetTreeNodeData(ndDev).ID_Device;
      vndPrm.ID_DevType := GetTreeNodeData(ndDev).ID_DevType;
      vndPrm.bFixedPT := GetTreeNodeData(ndDev).bFixedPT;
      vndPrm.ID_Prm := qTmp.FieldByName('ID_Prm').AsInteger;
      vndPrm.ID_PT := qTmp.FieldByName('ID_PT').AsInteger;
      vndPrm.ID_Src := qTmp.FieldByName('ID_Src').AsInteger;
      vndPrm.UserSrc := qTmp.FieldByName('UserSrc').AsVariant;
      vndPrm.CurrentsAddr := qTmp.FieldByName('CurrentsAddr').AsVariant;
      vndPrm.N_Src := qTmp.FieldByName('N_Src').AsInteger;
      vndPrm.ModbusResultType := qTmp.FieldByName('ModbusResultType').AsInteger;
      vndPrm.sTxt := qTmp.FieldByName('Name').AsString;
      vndPrm.sOpcTag := qTmp.FieldByName('OpcTag').AsString;
      vndPrm.ID_MeaUnit := qTmp.FieldByName('ID_MeaUnit').AsInteger;
      vndPrm.SourcePrmID := qTmp.FieldByName('SourcePrmID').AsVariant;
      vndPrm.MainSrvPrmID := qTmp.FieldByName('MainSrvPrmID').AsVariant;
      vndPrm.AgeAddr := qTmp.FieldByName('AgeAddr').AsVariant;

      vndPrm.DevMin := qTmp.FieldByName('DevMin').AsVariant;
      vndPrm.DevMax := qTmp.FieldByName('DevMax').AsVariant;
      vndPrm.RealMin := qTmp.FieldByName('RealMin').AsVariant;
      vndPrm.RealMax := qTmp.FieldByName('RealMax').AsVariant;

      vndPrm.CalibrType := qTmp.FieldByName('CalibrType').AsInteger;
      vndPrm.Resistor := qTmp.FieldByName('Resistor').AsInteger;
      vndPrm.CounterCeiling := qTmp.FieldByName('CounterCeiling').AsVariant;
      vndPrm.MeterCalcNIType := qTmp.FieldByName('MeterCalcNIType').AsInteger;
      vndPrm.ReqIntervalType := qTmp.FieldByName('ReqIntervalType').AsInteger;
      vndPrm.MinIntegralInterval := qTmp.FieldByName('MinIntegralInterval').AsInteger;

      vndPrm.AlarmSignal := qTmp.FieldByName('AlarmSignal').AsInteger;
      vndPrm.AlarmThreshold := qTmp.FieldByName('AlarmThreshold').AsVariant;

      vndPrm.StartNIUDate := qTmp.FieldByName('NI_StartDT').AsInteger;
      vndPrm.StartNIValue := qTmp.FieldByName('NI_StartVal').AsVariant;
      vndPrm.HourArchArgument := qTmp.FieldByName('HourArchArgument').AsInteger;
      vndPrm.HourArchAddr := qTmp.FieldByName('HourArchAddr').AsInteger;
    end;

    qTmp.Next();
  end;

  qTmp.Close();
end;

procedure TIOMainFrm.ReadDevices(ndObj: PVirtualNode; CycleObject: pointer = nil);
var ndDev: PVirtualNode;
    vndDev: PVTNodeData;
begin
  if tree.GetNodeLevel(ndObj) <> 0 then Exit;

  qTmp.Close();

  qTmp.SQL.Text:='select * from Devices d left outer join DevTypes dt on d.ID_DevType = dt.ID_DevType' +
               ' where ID_Obj = ' + IntToStr(GetTreeNodeData(ndObj).ID_Obj);

  qTmp.Open();
  while not qTmp.Eof do
  begin
    ndDev := Tree.AddChild(ndObj);
    InitNode(ndDev);
    vndDev := GetTreeNodeData(ndDev);
    vndDev.sTxt := qTmp.FieldByName('DevName').AsString;
    vndDev.ID_Obj := GetTreeNodeData(ndObj).ID_Obj;
    vndDev.ObjType := GetTreeNodeData(ndObj).ObjType;
    vndDev.ID_Device := qTmp.FieldByName('ID_Device').AsInteger;
    vndDev.ID_DevType := qTmp.FieldByName('ID_DevType').AsInteger;
    vndDev.Number485 := qTmp.FieldByName('Number').AsInteger;
    vndDev.bFixedPT := (qTmp.FieldByName('FixedPT').AsInteger > 0);
    vndDev.BaudRate := qTmp.FieldByName('BaudRate').AsInteger;
    vndDev.DevPortType := qTmp.FieldByName('PortType').AsInteger;
    vndDev.ReqPackSize := qTmp.FieldByName('ReqPackSize').AsInteger;
    vndDev.AddrBase := qTmp.FieldByName('AddrBase').AsInteger;

    qTmp.Next();
  end;
  qTmp.Close();
end;

procedure TIOMainFrm.PopulateListFromSQL(lst: TStrings; const SQL, NameField, IDField: string);
begin
  qTmp.Close();

  lst.Clear();

  qTmp.SQL.Text := SQL;
  qTmp.Open();

  while not qTmp.Eof do
  begin
    lst.AddObject(qTmp.FieldByName(NameField).AsString, TObject(qTmp.FieldByName(IDField).AsInteger));

    qTmp.Next();
  end;

  qTmp.Close();
end;

procedure TIOMainFrm.GetDevTypesList();
begin
  PopulateListFromSQL(slDevType, 'select * from DevTypes order by Name', 'Name', 'ID_DevType');
end;

procedure TIOMainFrm.GetNodeTypesList();
begin
  PopulateListFromSQL(slNodeType, 'select * from NodeTypes order by Name', 'Name', 'ID_NodeType');
end;

procedure TIOMainFrm.GetPrmTypesList(bAddEmpty: bool = false);
begin
  slPrmType.Clear();
  GMParamTypes.Clear();

  qTmp.Close();
  qTmp.SQL.Text := 'select * from ParamTypes order by PName';
  qTmp.Open();

  if bAddEmpty then
  begin
    slPrmType.AddObject('< Нет >', nil);
  end;

  while not qTmp.Eof do
  begin
    slPrmType.AddObject(qTmp.FieldByName('PName').AsString, TObject(qTmp.FieldByName('ID_PT').AsInteger));
    GMParamTypes.Add( qTmp.FieldByName('ID_PT').AsInteger,
                      qTmp.FieldByName('PSign').AsString,
                      qTmp.FieldByName('PName').AsString,
                      qTmp.FieldByName('IsAlarm').AsInteger > 0,
                      qTmp.FieldByName('DefMeaUnit').AsInteger );

    qTmp.Next();
  end;
  qTmp.Close();
end;

procedure TIOMainFrm.GetMeaUnitsTypesList(bAddEmpty: bool = false);
begin
  PopulateListFromSQL(slPrmMeaUnits, 'select * from MeaUnits order by ID_MeaUnit', 'LongName', 'ID_MeaUnit');
  slPrmMeaUnits.Insert(0, '< Нет >');
end;

procedure TIOMainFrm.RefreshTreeAndDBLists();
var ndObj: PVirtualNode;
    vndObj: PVTNodeData;
begin
  Tree.Clear();

  GetDevTypesList();
  GetPrmTypesList(true);
  GetMeaUnitsTypesList(true);
  GetNodeTypesList();

  qTmp.Close();
  qTmp.SQL.Text:='select * from Objects order by Name';
  qTmp.Open();

  // объекты
  while not qTmp.Eof do
  begin
    ndObj := Tree.AddChild(Tree.RootNode);
    InitNode(ndObj);
    vndObj := GetTreeNodeData(ndObj);
    vndObj.sTxt := qTmp.FieldByName('Name').AsString;
    vndObj.ID_Obj := qTmp.FieldByName('ID_Obj').AsInteger;
    vndObj.N_Car := qTmp.FieldByName('N_Car').AsInteger;
    vndObj.ObjType := qTmp.FieldByName('ObjType').AsInteger;
    vndObj.RemoteType := qTmp.FieldByName('RemoteType').AsInteger;
    vndObj.RemoteServerName := qTmp.FieldByName('RemoteName').AsString;
    vndObj.sRemoteSrv := qTmp.FieldByName('RemoteSrv').AsString;
    vndObj.RemotePort := qTmp.FieldByName('RemotePort').AsInteger;
    vndObj.Converter := qTmp.FieldByName('Converter').AsInteger;
               
    qTmp.Next();
  end;

  // девайсы
  CycleTree(Tree.RootNode, ReadDevices);
  CycleTree(Tree.RootNode, ReadDeviceParams);
  Tree.SortTree(Tree.Header.SortColumn, Tree.Header.SortDirection);

  HideCOMObjectsGeomers(hotHideCOMGeomers);
  RefreshNCarIDObjRelations();
end;

procedure CLBFromInt(clb: TCheckListBox; lst: int);
var i: int;
begin
  for i := 1 to clb.Items.Count do
    clb.Checked[i - 1] := (lst and (1 shl i)) > 0;
end;

function IntFromCLB(clb: TCheckListBox): int;
var i: int;
begin
  Result := 0;
  for i := 1 to clb.Items.Count do
    if clb.Checked[i - 1] then Result := Result or (1 shl i) ;
end;

procedure TIOMainFrm.DelCurrentTreeItem();
var s: string;
    vnd: PVTNodeData;
    nd: PVirtualNode;
begin
  nd := Tree.FocusedNode;
  vnd := GetTreeNodeData();
  if vnd = nil then Exit;

  if ShowMessageBox('Удалить ' + vnd.sTxt + '?', MB_ICONEXCLAMATION OR MB_YESNO) <> IDYES then Exit;

  s := '';
  // удалять можно только пользовательские каналы
  if vnd.ID_Prm > 0 then
  begin
    if vnd.ID_Src in UserDefinedParamTypes then
      s := 'delete from Params where ID_Prm = ' + IntToStr(vnd.ID_Prm);
  end
  else
  if vnd.ID_Device > 0 then
  begin
    s := ' where ID_Device = '+IntToStr(vnd.ID_Device);
    s := 'delete from Params ' + s + '; delete from Devices '+s;
  end
  else
  if vnd.ID_Obj > 0 then
  begin
    s := ' where ID_Obj = '+IntToStr(vnd.ID_Obj);
    s := 'delete from Params where ID_Device in (select ID_Device from Devices ' + s + ');' +
         ' delete from Objects ' + s + '; delete from Devices ' + s;
  end;

  if s <> '' then
  begin
    ExecSQL(s);

    if nd.NextSibling <> nil then
      SelectNode(nd)
    else
    if nd.PrevSibling <> nil then
      SelectNode(nd.PrevSibling)
    else
    if nd.Parent <> nil then
      SelectNode(nd.Parent);

    Tree.DeleteNode(nd);

    RefreshNCarIDObjRelations();
  end;
end;

procedure TIOMainFrm.btDelClick(Sender: TObject);
begin
  DelCurrentTreeItem();
end;

procedure TIOMainFrm.AddObjectToTree();
var id_obj: int;
    nd: PVirtualNode;
begin
  qTmp.Close();

  qTmp.SQL.Text:='insert into Objects(Name, N_Car) values (''Новый объект'', 0) returning id_obj';
  qTmp.Open();
  id_obj := qTmp.Fields[0].AsInteger;
  RefreshTreeAndDBLists();

  nd := Tree.RootNode.FirstChild;
  while nd <> nil do
  begin
    if GetTreeNodeData(nd).ID_Obj = id_obj then
    begin
      SelectNode(nd);
      break;
    end;

    nd := nd.NextSibling;
  end;

  qTmp.Close();
end;

procedure TIOMainFrm.btAddObjClick(Sender: TObject);
begin
  if not CheckObjectCountRestriction() then Exit;
  
  AddObjectToTree();
end;

procedure TIOMainFrm.AddDeviceLostParams(ID_Device, argument: int);
begin
  ExecPLSQL(Format('perform AddDeviceLostParams(%d, %s)',
                 [ID_Device, IfThen(argument < 0, 'null', IntToStr(argument))]));
end;

procedure TIOMainFrm.AddDevice(ndObj: PVirtualNode; ID_DevType: int);
var id_dev: int;
    nd: PVirtualNode;
    vnd: PVTNodeData;
begin
  vnd := GetTreeNodeData(ndObj);

  qTmp.Close();

  qTmp.SQL.Text := Format('insert into Devices (ID_Obj, ID_DevType) select %d, %d '+
                          ' returning id_device ',
                          [vnd.ID_Obj, ID_DevType]);
  qTmp.Open();
  id_dev := qTmp.Fields[0].AsInteger;
  AddDeviceLostParams(id_dev, 0);
  Tree.DeleteChildren(ndObj);
  ReadDevices(ndObj);
  CycleTree(ndObj, ReadDeviceParams);
  Tree.Expanded[ndObj] := true;

  nd := ndObj.FirstChild;
  while nd <> nil do
  begin
    if GetTreeNodeData(nd).ID_Device = id_dev then
    begin
      SelectNode(nd);
      TreeFocusChanged(Tree, nd, 0); // сам почему-то не вызывает
      Tree.FullExpand(nd);
      break;
    end;

    nd := nd.NextSibling;
  end;

  qTmp.Close();
end;

function TIOMainFrm.GetTopLevelParent(nd: PVirtualNode; Level: int = 0): PVirtualNode;
begin
  Result := nd;

  while (Result <> nil) and (int(Tree.GetNodeLevel(Result)) > Level) do
    Result := Result.Parent;
end;

procedure TIOMainFrm.btAddDevMenuClick(Sender: TObject);
var nd: PVirtualNode;
begin
  nd := GetTopLevelParent(Tree.FocusedNode);

  if nd <> nil then
    AddDevice(nd, TMenuItem(Sender).Tag);
end;


procedure TIOMainFrm.AddDevicesAsMenuItems(Parent: TMenuItem);
var i: int;

  function ShortTickCount(): int;
  begin
    Result := GetTickCount() and $FFFFFF;
  end;

  procedure AddChild(parent: TMenuItem; Caption: string; Tag: int; bAction: bool);
  var mi: TMenuItem;
  begin
    mi := TMenuItem.Create(nil);
    mi.Caption := Caption;
    mi.Tag := Tag;
    if bAction then
      mi.OnClick := btAddDevMenuClick;

    parent.Insert(0, mi);
  end;

begin
  if (Parent.Count > 0) and (Abs(Parent.Tag - ShortTickCount()) < 30000) then Exit;
  Parent.Clear();

  qTmp.Close();
  qTmp.SQL.Text := 'select * from DevTypeGroups order by Name desc';
  qTmp.Open();

  while not qTmp.Eof do
  begin
    AddChild(Parent, qTmp.FieldByName('Name').AsString, -qTmp.FieldByName('ID_DevTypeGroup').AsInteger, false);
    qTmp.Next();
  end;

  qTmp.Close();
  qTmp.SQL.Text := 'select * from DevTypes order by Name desc';
  qTmp.Open();

  while not qTmp.Eof do
  begin
    if qTmp.FieldByName('ID_DevTypeGroup').AsInteger > 0 then
    begin
      for i := 0 to Parent.Count - 1 do
        if Parent[i].Tag = - qTmp.FieldByName('ID_DevTypeGroup').AsInteger then
        begin
          AddChild(Parent[i], qTmp.FieldByName('Name').AsString, qTmp.FieldByName('ID_DevType').AsInteger, true);
          break;
        end;
    end
    else
      AddChild(Parent, qTmp.FieldByName('Name').AsString, qTmp.FieldByName('ID_DevType').AsInteger, true);
    qTmp.Next();
  end;

  Parent.Tag := ShortTickCount();
end;

function TIOMainFrm.CheckDateRestriction(): bool;
begin
{$ifdef _DATE_RESTRICTION}
  if Now() > DateRestriction then
  begin
    ShowMessageBox('Ошибка при проверке лицензий!', MB_ICONERROR);
    Result := false;
  end
  else
    Result := true;
{$else}
  Result := true;
{$endif}
end;

function TIOMainFrm.CheckObjectCountRestriction(): bool;
begin
{$ifdef _OBJECTS_RESTRICTION}
  qTmp.Close();
  qTmp.SQL.Text := 'select count (*) from Objects';
  try
    qTmp.Open();
    Result := qTmp.Fields[0].AsInteger <= ObjectLimitLicense;
    qTmp.Close();

    if not Result then
      ShowMessageBox('Превышен лимит на число объектов.'#13#10 +
                     'Максимальное число объектов: ' + IntToStr(ObjectLimitLicense), MB_ICONERROR);
  except
    on e: Exception do
    begin
      ShowMessageBox('Ошибка при проверке лицензий: ' + E.Message, MB_ICONERROR);
      Result := false;
    end;
  end;
{$else}
  Result := true;
{$endif}
end;

procedure TIOMainFrm.btAddDevClick(Sender: TObject);
begin
  if Tree.FocusedNode = nil then Exit;
  
  AddDevicesAsMenuItems(pmDevices.Items);
  pmDevices.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

function TIOMainFrm.CheckLastAccess(nd: PVirtualNode; tLastAccess: TDateTime): bool;
begin
  Result := GetTreeNodeData(nd).tLastAccess < tLastAccess;
  if Result then
    GetTreeNodeData(nd).tLastAccess := tLastAccess;
end;

procedure TIOMainFrm.WMDeviceOnline_CycleTree(nd: PVirtualNode; CycleObject: pointer);
begin
  if (Tree.GetNodeLevel(nd) = NODE_LEVEL_DEVICE) and (GetTreeNodeData(nd).ID_Device = PVTNodeData(CycleObject).ID_Device) then
  begin
    if CheckLastAccess(nd, PVTNodeData(CycleObject).tLastAccess)
       or CheckLastAccess(nd.Parent, PVTNodeData(CycleObject).tLastAccess) then
    begin
      PVTNodeData(CycleObject).bFixedPT := true;
    end;
  end;
end;

procedure TIOMainFrm.WMDeviceOnline(var Msg: TMessage);
var data: TVTNodeData;
begin
  data.ID_Device := Msg.WParam;
  data.tLastAccess := UTCtoLocal(Msg.LParam);
  data.bFixedPT := false;
  CycleTree(Tree.RootNode, WMDeviceOnline_CycleTree, @data);
  if data.bFixedPT then
    Tree.Refresh();
end;

procedure TIOMainFrm.WMObjectOnline_CycleTree(nd: PVirtualNode; CycleObject: pointer);
begin
  if (Tree.GetNodeLevel(nd) = NODE_LEVEL_OBJECT) and (GetTreeNodeData(nd).ID_Obj = PVTNodeData(CycleObject).ID_Obj) then
  begin
    if CheckLastAccess(nd, PVTNodeData(CycleObject).tLastAccess) then
      PVTNodeData(CycleObject).bFixedPT := true;
  end;
end;

procedure TIOMainFrm.WMObjectOnline(var Msg: TMessage);
var data: TVTNodeData;
begin
  data.ID_Obj := Msg.WParam;
  data.tLastAccess := UTCtoLocal(Msg.LParam);
  data.bFixedPT := false;
  CycleTree(Tree.RootNode, WMObjectOnline_CycleTree, @data);
  if data.bFixedPT then
    Tree.Refresh();
end;

procedure TIOMainFrm.InitNode(Node: PVirtualNode);
begin
  InitVND(GetTreeNodeData(Node));
  Include(Node.States, vsInitialized);
end;

function TIOMainFrm.GetDevTypeByID(ID_DevType: int): string;
var n: int;
begin
  Result := '';
  n := slDevType.IndexOfObject(pointer(ID_DevType));
  if n >= 0 then
    Result := slDevType[n];
end;

function TIOMainFrm.NSrcToStr(NSrc: int; nd: PVirtualNode = nil; bForce: bool = false): string;
begin
  Result := IfThen(bForce or IsCurrentDeviceTecon(nd) and (GetTreeNodeData(nd).ID_Src = SRC_USR),
                   IntToHex(NSrc, 4),
                   IntToStr(NSrc));
end;

function TIOMainFrm.GetPrmTxt(nd: PVirtualNode; ppc: TParamPathComponents): string;
var vnd: PVTNodeData;
begin
  Result := '';
  vnd := GetTreeNodeData(nd);
  if vnd <> nil then
  begin
    if  ppcSrc in ppc then
    begin
      Result := ID_SrcToStr(vnd.ID_Src) + IfThen(not (vnd.ID_Src in SubParamTypes) or (vnd.BaseChn <= 0), ' ' + NSrcToStr(vnd.N_Src, nd)) + ' - ';
    end;

    if Trim(vnd.sTxt) <> '' then
    begin
      Result := Result + vnd.sTxt;
      if ppcPT in ppc then
        Result := Result +  ' (' + ParamShowNameFromID(vnd.ID_PT, '') + ')'
    end
    else
      Result := Result + ParamShowNameFromID(vnd.ID_PT, '');
  end;
end;

procedure TIOMainFrm.TreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var vnd: PVTNodeData;
    n: int;
begin
  CellText := '';
  n := 0;
  vnd := GetTreeNodeData(Node);
  case Column of
    0: begin
         case Tree.GetNodeLevel(Node) of
           NODE_LEVEL_OBJECT: CellText := vnd.sTxt;
           NODE_LEVEL_DEVICE: begin
                CellText := vnd.sTxt + IfThen(Trim(vnd.sTxt) <> '', ' - ');
                CellText := CellText + GetDevTypeByID(vnd.ID_DevType);
              end;
           NODE_LEVEL_PARAM: CellText := GetPrmTxt(Node, [ppcSrc, ppcPT]);
           NODE_LEVEL_SUBPARAM: CellText := GetPrmTxt(Node, [ppcSrc, ppcPT]);
         end;
       end;
    1: begin
         if Tree.GetNodeLevel(Node) = NODE_LEVEL_OBJECT then
         begin
           if vnd.ObjType = OBJ_TYPE_REMOTE_SRV_XML then
             CellText := vnd.RemoteServerName + ': ' + slObjType[vnd.RemoteType]
           else
             CellText := slObjType[slObjType.IndexOfObject(TObject(vnd.ObjType))];
         end;
       end;
    2: begin
         case Tree.GetNodeLevel(Node) of
           NODE_LEVEL_OBJECT: n := vnd.N_Car;
           NODE_LEVEL_DEVICE: n := vnd.Number485;
         end;

         CellText := IfThen(n > 0, IntToStr(n));
       end;
  end;
end;

function TIOMainFrm.CalcCalibrationMark(vnd: PVTNodeData): double;
begin
  // импульсы в секунду надо перевести в некие величины в час с ценой импульса,
  // прописанной в leCalibrationMark
  Result := 1;

  if (vnd.DevMin <> vnd.DevMax) and (vnd.RealMin <> vnd.RealMax) then
    Result := (vnd.RealMax - vnd.RealMin) / (vnd.DevMax - vnd.DevMin) / 3600;
end;

procedure TIOMainFrm.ApplyPrmLastVals();
var vnd: PVTNodeData;
begin
  vnd := GetTreeNodeData(nil);
  if vnd <> nil then
  begin
    if vnd.LastDT > 0 then
    begin
      lePrmLastDT.Text := DateTimeToStr(UTCtoLocal(vnd.LastDT));
      lePrmLastVal.Text := FormatFloatToShow(vnd.LastVal);
    end
    else
    begin
      lePrmLastDT.Text := '';
      lePrmLastVal.Text := '';
    end;
  end;
end;

procedure TIOMainFrm.SetVPTFieldIntVal(field: VirtualPropertyTree.PField; val: int);
begin
  field.DataType := dtInteger;
  field.IntData := val;
  field.StringData := IntToStr(val);
end;

procedure TIOMainFrm.SetVPTFieldFloatVal(field: VirtualPropertyTree.PField; val: double);
begin
  field.DataType := dtExtended;
  field.ExtendedData := val;
  field.StringData := MyFloatToStr(val);
end;

procedure TIOMainFrm.SetVPTFieldVariantVal(field: VirtualPropertyTree.PField; val: Variant);
begin
  field.DataType := dtExtended;
  field.AllowNull := true;
  
  if VarIsNull(val) then
  begin
    field.ExtendedData := 0;
    field.StringData := '';
  end
  else
  begin
    field.ExtendedData := val;
    field.StringData := MyFloatToStr(val);
  end;
end;

procedure TIOMainFrm.SetVPTCmbVal(field: VirtualPropertyTree.PField; slValues: TStrings; val: Variant);
var i, n: int;
begin
  SetLength(field.ComboData, slValues.Count);
  SetLength(field.ComboInt, slValues.Count);

  field.DataType := dtCombo;

  for i := 0 to slValues.Count - 1 do
  begin
    field.ComboData[i] := slValues[i];
    field.ComboInt[i] := int(slValues.Objects[i]);
  end;

  if VarIsNull(val) then Exit;
  field.IntData := val;

  n := slValues.IndexOfObject(TObject(int(val)));
  if n >= 0 then
  begin
    field.StringForUser := slValues[n];
    field.StringData := slValues[n];
  end;
end;

function TIOMainFrm.AddVPTField(const Name: string; PropertyType: TVPTPropertyType; const Category: string; Enabled: bool = true): VirtualPropertyTree.PField;
begin
  SetLength(vpt.Fields, Length(vpt.Fields) + 1);
  Result := @vpt.Fields[Length(vpt.Fields) - 1];
  Result.Init();

  Result.MinIntValue := - MaxInt div 2 ;
  Result.MaxIntValue := MaxInt;
  Result.AllowNull := false;
  Result.MinExtValue := - MaxDouble / 2;
  Result.MaxExtValue := MaxDouble;

  Result.DataType := dtCommonString;
  Result.ExtObject := TObject(PropertyType);
  Result.Enabled := Enabled;
  Result.Category := Category;
  Result.Name := Name;
  Result.AllowNull := false;
end;

function TIOMainFrm.IsOriginalMtr(nd: PVirtualNode): bool; // приборный счетчик, не опирающийся на CNT_DI
var vnd: PVTNodeData;
begin
  vnd := GetTreeNodeData(nd);
  Result := (vnd.ID_Src = SRC_CNT_MTR) and (Tree.GetNodeLevel(nd) = NODE_LEVEL_PARAM);
end;

procedure TIOMainFrm.FillObjectEditor_Param(nd: PVirtualNode);
var vnd: PVTNodeData;
    FixedPT: bool;
    devInfo: PVTNodeData;
begin
  vnd := GetTreeNodeData(nd);
  devInfo := GetTreeNodeData(GetDeviceNode(nd));
  FixedPT := devInfo.bFixedPT;
  SetLength(vpt.Fields, 0);
  SetVPTFieldIntVal(AddVPTField('ID_Prm', VPT_ID, VPTCategoryMain, false), vnd.ID_Prm);
  SetVPTCmbVal(AddVPTField('Источник', VPT_ID_Src, VPTCategoryMain, false), slID_Src, vnd.ID_Src);
  if vnd.ID_Src = SRC_USR then
    SetVPTCmbVal(AddVPTField('Пользовательский тип', VPT_UserSrc, VPTCategoryMain), slUserSrc, vnd.UserSrc);

  AddVPTField('Номер', VPT_N_Src, VPTCategoryMain, vnd.ID_Src in UserDefinedParamTypes).StringData := NSrcToStr(vnd.N_Src, nd);

  if (vnd.ID_Src = SRC_USR) and (devInfo.ID_DevType in FreeConfigModbusDevices) then
    SetVPTCmbVal(AddVPTField('Формат данных', VPT_ModbusResultType, VPTCategoryMain), slModbusResType, vnd.ModbusResultType);

  if devInfo.ID_DevType in Tecon19_Family then
  begin
    SetVPTCmbVal(AddVPTField('Часовой архив', VPT_HourArchArgument, VPTCategoryMain), slTecomHourArchDepth, vnd.HourArchArgument);
    AddVPTField('Ячейка час. архива ', VPT_HourArchAddr, VPTCategoryMain).StringData := NSrcToStr(vnd.HourArchAddr, nd, true);
  end;

  AddVPTField('Название', VPT_Name, VPTCategoryAdditional, vnd.ID_PT > 0).StringData := vnd.sTxt;
  AddVPTField('Тэг ОРС', VPT_OpcTag, VPTCategoryAdditional, vnd.ID_PT > 0).StringData := vnd.sOpcTag;
  SetVPTCmbVal(AddVPTField('Тип', VPT_PType, VPTCategoryAdditional, not FixedPT), slPrmType, vnd.ID_PT);
  SetVPTCmbVal(AddVPTField('Ед. измерения', VPT_PrmMeaUnits, VPTCategoryAdditional, vnd.ID_PT > 0), slPrmMeaUnits, vnd.ID_MeaUnit);

  if vnd.ID_Src = SRC_USR then // тип расчета меняем только для пользовательских типов. Про остальные все знаем и так
    SetVPTCmbVal(AddVPTField('Тип расчета', VPT_ReqIntervalType, VPTCategoryAdditional, vnd.ID_PT > 0), slReqIntervalType, vnd.ReqIntervalType);

  if vnd.ID_Src = SRC_CNT_DI then // минимальный интервал усреднения, актуально для медленных счетчиков
    SetVPTCmbVal(AddVPTField('Интервал усреднения', VPT_MinIntegralInterval, VPTCategoryAdditional, vnd.ID_PT > 0), slMinIntegralInterval, vnd.MinIntegralInterval);

  if (vnd.ID_Src = SRC_CNT_MTR) and not IsOriginalMtr(nd) then // метод рачсета показаний рисуем для подчиненных счетчиков, для исходных не надо
  begin
    SetVPTCmbVal(AddVPTField('Показания', VPT_MeterCalcNIType, VPTCategoryCalibr), slMeterCalcNITypes, vnd.MeterCalcNIType);
  end;

  if vnd.ID_Src = SRC_CNT_DI then
  begin
    SetVPTFieldFloatVal(AddVPTField('Цена импульса', VPT_CalibrationMark, VPTCategoryCalibr), CalcCalibrationMark(vnd));
    SetVPTFieldVariantVal(AddVPTField('Предел счетчика', VPT_CounterCeiling, VPTCategoryCalibr), vnd.CounterCeiling);
  end;

  if vnd.ID_Src in [SRC_AI, SRC_USR, SRC_CNT_MTR] then
  begin
    SetVPTCmbVal(AddVPTField('Датчик', VPT_SensorType, VPTCategoryCalibr), slCalibrationType, vnd.CalibrType);
    SetVPTFieldIntVal(AddVPTField('Resistor', VPT_Resistor, VPTCategoryCalibr), vnd.Resistor);
    SetVPTFieldVariantVal(AddVPTField('Предел по сигналу Min', VPT_DevMin, VPTCategoryCalibr, vnd.CalibrType = CALIBR_TYPE_MANUAL), vnd.DevMin);
    SetVPTFieldVariantVal(AddVPTField('Предел по сигналу DevMax', VPT_DevMax, VPTCategoryCalibr, vnd.CalibrType = CALIBR_TYPE_MANUAL), vnd.DevMax);
    SetVPTFieldVariantVal(AddVPTField('Предел по параметру Min', VPT_RealMin, VPTCategoryCalibr), vnd.RealMin);
    SetVPTFieldVariantVal(AddVPTField('Предел по параметру Max', VPT_RealMax, VPTCategoryCalibr), vnd.RealMax);
  end;

  if vnd.ID_Src in [SRC_AI, SRC_DI, SRC_CNT_DI, SRC_USR] then
    SetVPTCmbVal(AddVPTField('Авария', VPT_Alarm, VPTCategoryAlarm), slPrmDIAlarmSignal, vnd.AlarmSignal);

  if vnd.ID_Src in [SRC_CNT_DI, SRC_AI, SRC_USR] then
    SetVPTFieldVariantVal(AddVPTField('Порог', VPT_AlarmThreshold, VPTCategoryAlarm), vnd.AlarmThreshold);

  if (vnd.ID_Src in CounterParamTypes) and not IsOriginalMtr(nd) then
  begin
    with AddVPTField('Дата', VPT_StartNIDate, VPTCategoryStartNI)^ do
    begin
      EditMask := '##.##.#### ##:##:##';
      StringData := FormatDateTime('dd.mm.yyyy hh:nn:ss', UtcToLocal(vnd.StartNIUDate));
    end;
    SetVPTFieldVariantVal(AddVPTField('Значение', VPT_StartNIValue, VPTCategoryStartNI), vnd.StartNIValue);
  end;

  if (vnd.ID_Src in OutputParamTypes) or (int(vnd.UserSrc) in OutputParamTypes) then
  begin
    SetVPTFieldVariantVal(AddVPTField('ID источника', VPT_SourcePrmID, VPTCategoryDeviceToDevice), vnd.SourcePrmID);
    SetVPTFieldVariantVal(AddVPTField('Ячейка даты', VPT_AgeAddr, VPTCategoryDeviceToDevice), vnd.AgeAddr);
  end;

  if vnd.ID_DevType = DEVTYPE_MAIN_SRV then
  begin
    SetVPTFieldVariantVal(AddVPTField('ID на головном сервере', VPT_MainSrvPrmID, VPTCategoryAdditional), vnd.MainSrvPrmID);
  end;

  vpt.RefreshFields();
end;

procedure TIOMainFrm.FillObjectEditor_Device(nd: PVirtualNode);
var vnd: PVTNodeData;
begin
  vnd := GetTreeNodeData(nd);
  SetLength(vpt.Fields, 0);

  SetVPTFieldIntVal(AddVPTField('ID_Device', VPT_ID, VPTCategoryMain, false), vnd.ID_Device);
  SetVPTCmbVal(AddVPTField('Тип', VPT_ID_DevType, VPTCategoryMain, false), slDevType, vnd.ID_DevType);
  AddVPTField('Название', VPT_Name, VPTCategoryMain).StringData := vnd.sTxt;

  if vnd.RealObjType = OBJ_TYPE_REMOTE_MAIN then Exit;

  if not (vnd.ID_DevType in GeomerFamily) then
  begin
    if vnd.ObjType = OBJ_TYPE_GM then
      SetVPTCmbVal(AddVPTField('Порт', VPT_DevPortType, VPTCategoryAdditional), slDevPortType, vnd.DevPortType);

    SetVPTCmbVal(AddVPTField('Скорость', VPT_DevBaudRate, VPTCategoryAdditional), slDevBaudrate, vnd.BaudRate);
  end;

  SetVPTFieldIntVal(AddVPTField('Номер', VPT_DevNumber, IfThen(vnd.ID_DevType in GeomerFamily, VPTCategoryMain, VPTCategoryAdditional)), vnd.Number485);

  if vnd.ID_DevType in FreeConfigModbusDevices then
  begin
    SetVPTFieldIntVal(AddVPTField('Размер пакета каналов', VPT_DevReqPackSize, VPTCategoryAdditional), vnd.ReqPackSize);
    SetVPTFieldIntVal(AddVPTField('Базовый адрес', VPT_DevAddrBase, VPTCategoryAdditional), vnd.AddrBase);
  end;
end;

procedure TIOMainFrm.FillObjectEditor_Object(nd: PVirtualNode);
var vnd: PVTNodeData;
begin
  vnd := GetTreeNodeData(nd);
  SetLength(vpt.Fields, 0);

  SetVPTFieldIntVal(AddVPTField('ID_Obj', VPT_ID, VPTCategoryMain, false), vnd.ID_Obj);
  if vnd.ObjType = OBJ_TYPE_REMOTE_SRV_XML then
    AddVPTField('Удаленный сервер', VPT_Info, VPTCategoryMain).StringData := vnd.RemoteServerName;
  SetVPTCmbVal(AddVPTField('Тип', VPT_ObjType, VPTCategoryMain), slObjType, vnd.RealObjType);
  AddVPTField('Название', VPT_Name, VPTCategoryMain).StringData := vnd.sTxt;

  if vnd.RealObjType <> OBJ_TYPE_REMOTE_MAIN then
    SetVPTFieldIntVal(AddVPTField('№ Прибора', VPT_NCar, VPTCategoryMain), vnd.N_Car);

  if vnd.RealObjType in [OBJ_TYPE_K104, OBJ_TYPE_TCP, OBJ_TYPE_REMOTE_MAIN] then
  begin
    AddVPTField('IP', VPT_IP, VPTCategoryMain).StringData := vnd.sRemoteSrv;
    SetVPTFieldIntVal(AddVPTField('Port', VPT_Port, VPTCategoryMain), vnd.RemotePort);
  end;

  if vnd.RealObjType = OBJ_TYPE_TCP then
    SetVPTCmbVal(AddVPTField('Конвертер интерфесов', VPT_Converter, VPTCategoryMain), slConverter, vnd.Converter);

  AddVPTField('Комментарий', VPT_UserData, VPTCategoryMain).StringData := vnd.UserData;
end;

procedure TIOMainFrm.FillObjectEditor_EnableControls(bRemote: bool);
var i: int;
begin
  if not bRemote then Exit;

  for i := 0 to high(vpt.Fields) do
    vpt.Fields[i].Enabled := false;
end;

procedure TIOMainFrm.FillObjectEditor(nd: PVirtualNode);
var level: int;
begin
  pcConfig.ActivePage := tsPrm;
  level := Tree.GetNodeLevel(nd);

  case level of
    NODE_LEVEL_OBJECT: FillObjectEditor_Object(nd);
    NODE_LEVEL_DEVICE: FillObjectEditor_Device(nd);
    NODE_LEVEL_PARAM,
    NODE_LEVEL_SUBPARAM: FillObjectEditor_Param(nd);
  end;

  gbLastData.Visible := level >= NODE_LEVEL_PARAM;
  FillObjectEditor_EnableControls(IsRemoteServerNode(nd));
  vpt.RefreshFields();
end;

procedure TIOMainFrm.vptFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var f: PField;
begin
  f:=TVirtualPropertyTree(Sender).GetNodeField(NewNode);
  Allowed := (f <> nil) and (f.Enabled or (f.DataType <> dtCombo));
end;

function VariantToString(v: Variant; IsFloat: bool = true): string;
begin
  if VarIsNull(v) or VarIsEmpty(v) then
    Result := 'null'
  else
  if IsFloat then
    Result := MyFloatToStr(v)
  else
    Result := IntToStr(v);
end;

procedure TIOMainFrm.PostVndSettings_Param(vnd: PVTNodeData);
begin
  try
    ExecSQL(Format(      'update Params set '+
                   #13#10'   ID_PT = NullIf(%d, 0), UserSrc = %s, CurrentsAddr = %s, DevMin = %s, DevMax = %s, ' +
                   #13#10'   RealMin = %s, RealMax = %s, ID_MeaUnit = NullIf(%d, 0), ' +
                   #13#10'   ModbusResultType = %d, ' +
                   #13#10'   CalibrType = %d, Resistor = %d, ' +
                   #13#10'   ArcType = %d, AlarmSignal = %d, AlarmThreshold = %s, ' +
                   #13#10'   NI_StartDT = %d, NI_StartVal = %s, ' +
                   #13#10'   Name = %s, CounterCeiling = %s, MeterCalcNIType = %d, ' +
                   #13#10'   OpcTag = %s, ReqIntervalType = %d, MinIntegralInterval = %d,' +
                   #13#10'   HourArchArgument = %d, HourArchAddr = %d, ' +
                   #13#10'   SourcePrmID = %s, MainSrvPrmID = %s, AgeAddr = %s' +
                   #13#10' where ID_Prm = %d',
                   [vnd.ID_PT, VariantToString(vnd.UserSrc, true), VariantToString(vnd.CurrentsAddr, true), VariantToString(vnd.DevMin), VariantToString(vnd.DevMax),
                    VariantToString(vnd.RealMin), VariantToString(vnd.RealMax), vnd.ID_MeaUnit,
                    vnd.ModbusResultType,
                    vnd.CalibrType, vnd.Resistor,
                    0{ArcType, не используется}, vnd.AlarmSignal, VariantToString(vnd.AlarmThreshold),
                    vnd.StartNIUDate, VariantToString(vnd.StartNIValue),
                    QuotedStr(vnd.sTxt), VariantToString(vnd.CounterCeiling), vnd.MeterCalcNIType,
                    QuotedStr(vnd.sOpcTag), vnd.ReqIntervalType, vnd.MinIntegralInterval,
                    vnd.HourArchArgument, vnd.HourArchAddr,
                    VariantToString(vnd.SourcePrmID), VariantToString(vnd.MainSrvPrmID), VariantToString(vnd.AgeAddr),

                    vnd.ID_Prm]));
  except
    on e: Exception do
    begin
      ShowMessageBox(e.Message, MB_ICONSTOP);
      TreeFocusChanged(Tree, Tree.FocusedNode, 0);
    end;
  end;

  Tree.Refresh();
end;

procedure TIOMainFrm.PostVndSettings_Object(vnd: PVTNodeData);
begin
  ExecSQL(Format('update Objects set ObjType = %d, N_Car = %d, Name = %s, RemoteSrv = %s, RemotePort = %d,  UserData = %s, Converter = %d ' +
                 ' where ID_Obj = %d',
                 [vnd.ObjType, vnd.N_Car, vnd.sTxt.QuotedString(), vnd.sRemoteSrv.QuotedString(), vnd.RemotePort, vnd.UserData.QuotedString(), vnd.Converter,

                  vnd.ID_Obj]));
end;

procedure TIOMainFrm.PostVndSettings_Device(vnd: PVTNodeData);
begin
  try
    ExecSQL(Format('update Devices set ID_DevType = %d, BaudRate = %d, DevName = %s, Number = %d, PortType = %d, ' +
                   '                   ReqPackSize = %d, AddrBase = %d ' +
                   ' where ID_Device = %d',
                   [vnd.ID_DevType, vnd.BaudRate, QuotedStr(vnd.sTxt), vnd.Number485, vnd.DevPortType,
                    vnd.ReqPackSize, vnd.AddrBase,

                    vnd.ID_Device]));
  except
    on e: Exception do
    begin
      ShowMessageBox(e.Message, MB_ICONSTOP);
      TreeFocusChanged(Tree, Tree.FocusedNode, 0);
    end;
  end;

  Tree.Refresh();
end;

procedure TIOMainFrm.PostVndNodeSettings_Chn(vnd: PVTNodeData);
begin
  ExecSQL(Format('update NodeChannels set ID_PT = NullIf(%d, 0), ID_MeaUnit = NullIf(%d, 0), ' +
                                ' AlarmSignal = %d, AlarmThreshold = %s, ' +
                                ' Name = %s, OpcTag = %s' +
                 ' where ID_Chn = %d',
                 [vnd.ID_PT, vnd.ID_MeaUnit,
                  vnd.AlarmSignal, VariantToString(vnd.AlarmThreshold),
                  QuotedStr(vnd.sTxt), QuotedStr(vnd.sOpcTag),

                  vnd.ID_Prm]));
end;

procedure TIOMainFrm.PostVndNodeSettings_Node(vnd: PVTNodeData);
begin
  ExecSQL(Format('update Nodes set ID_NodeType = NullIf(%d, 0), Name = %s' +
                 ' where ID_Node = %d',
                 [vnd.ObjType, QuotedStr(vnd.sTxt),
                  vnd.ID_Obj]));
end;

procedure TIOMainFrm.PostVndNodeSettings(vnd: PVTNodeData);
begin
  try
    if vnd.ID_Prm > 0 then
      PostVndNodeSettings_Chn(vnd)
    else
      PostVndNodeSettings_Node(vnd)
  except
    on e: Exception do
    begin
      ShowMessageBox(e.Message, MB_ICONSTOP);
      ChangeNodeTreeFocus(NodeTreeFrame.Tree.FocusedNode);
    end;
  end;

  NodeTreeFrame.Tree.Refresh();
end;

function StringToVariant(const s: string): Variant;
begin
  if Trim(s) = '' then
    Result := Null
  else
    Result := MyStrToFloatDef(s);

  if Result = INCORRECT_VALUE then
    Result := Null;
end;

procedure UpdateCalibrationMark(vnd: PVTNodeData; const s: string);
begin
  vnd.DevMin := 0;
  vnd.DevMax := 1;
  vnd.RealMin := 0;

  if  VarIsNull(StringToVariant(s)) then
    vnd.RealMax := 1
  else
    vnd.RealMax := 3600 * MyStrToFloat(s);
end;

function FieldStringToUTC(const v: string): int;
var y, m, d, h, n, s: int;
begin
  if Trim(v) = '' then
    Result := 0
  else
  try
    d := StrToInt(Copy(v, 1, 2));
    m := StrToInt(Copy(v, 4, 2));
    y := StrToInt(Copy(v, 7, 4));

    h := StrToInt(Copy(v, 12, 2));
    n := StrToInt(Copy(v, 15, 2));
    s := StrToInt(Copy(v, 18, 2));

    Result := LocalToUtc(EncodeDateTime(y, m, d, h, n, s, 0));
  except
    raise Exception.Create('Неверное значение даты и времени: ' + v + #13#10
                           + 'Дата и время вводятся в формате:' + #13#10
                           + ' день.месяц.год часы:минуты:секунды');
  end;
end;

procedure UpdateSensorType(vnd: PVTNodeData; val: int);
var Resistor: double;
begin
  vnd.CalibrType := val;

  if vnd.Resistor > 0 then
    Resistor := vnd.Resistor / 1000
  else
    Resistor := 1;

  case val of
    CALIBR_TYPE_U_0_1:
      begin
        vnd.DevMin := 0;
        vnd.DevMax := 1;
      end;
    CALIBR_TYPE_U_0_5:
      begin
        vnd.DevMin := 0;
        vnd.DevMax := 5;
      end;
    CALIBR_TYPE_U_0_10:
      begin
        vnd.DevMin := 0;
        vnd.DevMax := 10;
      end;

    CALIBR_TYPE_I_4_20:
      begin
        vnd.DevMin := 4 * Resistor;
        vnd.DevMax := 20 * Resistor;
      end;
    CALIBR_TYPE_I_0_20:
      begin
        vnd.DevMin := 0;
        vnd.DevMax := 20 * Resistor;
      end;

    CALIBR_TYPE_I_0_5:
      begin
        vnd.DevMin := 0;
        vnd.DevMax := 5 * Resistor;
      end;
  end;
end;

function TIOMainFrm.VPTPropertyEdited_UpdateProperty(Field: PField; vnd: PVTNodeData): bool;
begin
  Result := true;

  case TVPTPropertyType(Field.ExtObject) of
    // общие
    VPT_Name: vnd.sTxt := Field.StringData;
    // узлы
    VPT_ObjType:
      begin
        vnd.ObjType := Field.IntData;
        if CurrentTreeType = gmttObjectTree then
        begin
          HideCOMObjectsGeomers(hotHideCOMGeomers);
          RefreshNCarIDObjRelations();
        end;
      end;
    // объекты
    VPT_NCar: vnd.N_Car := Field.IntData;
    VPT_IP: vnd.sRemoteSrv := Field.StringData;
    VPT_Port: vnd.RemotePort := Field.IntData;
    VPT_UserData: vnd.UserData := Field.StringData;
    VPT_Converter: vnd.Converter := Field.IntData;
    // каналы
    VPT_UserSrc:
      if Field.IntData > 0 then
        vnd.UserSrc := Field.IntData
      else
        vnd.UserSrc := null;
    VPT_N_Src: vnd.CurrentsAddr := ParsePrmNSrc(Field.StringData);
    VPT_ModbusResultType: vnd.ModbusResultType := Field.IntData;
    VPT_PType: vnd.ID_PT := Field.IntData;
    VPT_PrmMeaUnits: vnd.ID_MeaUnit := Field.IntData;
    VPT_CalibrType: vnd.CalibrType := Field.IntData;
    VPT_DevMin: vnd.DevMin := StringToVariant(Field.StringData);
    VPT_DevMax: vnd.DevMax := StringToVariant(Field.StringData);
    VPT_RealMin: vnd.RealMin := StringToVariant(Field.StringData);
    VPT_RealMax: vnd.RealMax := StringToVariant(Field.StringData);
    VPT_CalibrationMark: UpdateCalibrationMark(vnd, Field.StringData);
    VPT_Resistor:
      begin
        vnd.Resistor := Field.IntData;
        UpdateSensorType(vnd, vnd.CalibrType);
      end;
    VPT_CounterCeiling: vnd.CounterCeiling := StringToVariant(Field.StringData);
    VPT_MeterCalcNIType: vnd.MeterCalcNIType := Field.IntData;
    VPT_SensorType: UpdateSensorType(vnd, Field.IntData);
    VPT_Alarm: vnd.AlarmSignal := Field.IntData;
    VPT_AlarmThreshold: vnd.AlarmThreshold := StringToVariant(Field.StringData);
    VPT_StartNIDate: vnd.StartNIUDate := FieldStringToUTC(Field.StringData);
    VPT_StartNIValue: vnd.StartNIValue := StringToVariant(Field.StringData);
    VPT_OpcTag: vnd.sOpcTag := Field.StringData;
    // приборы
    VPT_DevNumber: vnd.Number485 := Field.IntData;
    VPT_DevReqPackSize: vnd.ReqPackSize := Field.IntData;
    VPT_DevAddrBase: vnd.AddrBase := Field.IntData;
    VPT_DevBaudRate: vnd.BaudRate := Field.IntData;
    VPT_DevPortType: vnd.DevPortType := Field.IntData;
    VPT_ReqIntervalType: vnd.ReqIntervalType := Field.IntData;
    VPT_MinIntegralInterval: vnd.MinIntegralInterval := Field.IntData;
    VPT_HourArchArgument: vnd.HourArchArgument := Field.IntData;
    VPT_HourArchAddr: vnd.HourArchAddr := ParsePrmNSrc(Field.StringData, true);
    VPT_SourcePrmID: vnd.SourcePrmID := StringToVariant(Field.StringData);
    VPT_AgeAddr: vnd.AgeAddr := StringToVariant(Field.StringData);
    VPT_MainSrvPrmID: vnd.MainSrvPrmID := StringToVariant(Field.StringData);
    else Result := false;
  end;
end;

procedure TIOMainFrm.VPTPropertyEdited_PostVndSettings(vnd: PVTNodeData);
begin
  if CurrentTreeType = gmttNodeTree then
  begin
    PostVndNodeSettings(vnd);
  end
  else
  begin
    case Tree.GetNodeLevel(Tree.FocusedNode) of
      NODE_LEVEL_OBJECT: PostVndSettings_Object(vnd);
      NODE_LEVEL_DEVICE: PostVndSettings_Device(vnd);
      else PostVndSettings_Param(vnd);
    end;
  end;
end;

procedure TIOMainFrm.VPTPropertyEdited_FillNodeEditor(nd: PVirtualNode);
begin
  if CurrentTreeType = gmttNodeTree then
    FillNodeTreeEditor(TreeFromNode(nd).GetNodeData(nd))
  else
    FillObjectEditor(nd);

  TreeFromNode(nd).Refresh();
end;

procedure TIOMainFrm.VPTPropertyEdited(Sender: TObject; Field: PField);
var vnd: PVTNodeData;
    nd: PVirtualNode;
begin
  nd := CurrentTree.FocusedNode;
  if nd = nil then Exit;

  vnd := TreeFromNode(nd).GetNodeData(nd);
  if vnd = nil then Exit;

  if VPTPropertyEdited_UpdateProperty(Field, vnd) then
    VPTPropertyEdited_PostVndSettings(vnd);

  VPTPropertyEdited_FillNodeEditor(nd);
end;

procedure TIOMainFrm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  ChangeObjectTree(Node);
end;

procedure TIOMainFrm.ChangeObjectTree(nd: PVirtualNode);
var vnd: PVTNodeData;
begin
  EnableEditorButtons();

  vnd := GetTreeNodeData(nd);

  if vnd = nil then
  begin
    pcConfig.ActivePage := tsEmpty;
    Exit;
  end;

  if EditMode = temEditControls then Exit;

  FillObjectEditor(nd);

  if Tree.GetNodeLevel(nd) >= NODE_LEVEL_PARAM then
    ApplyPrmLastVals();
end;

procedure TIOMainFrm.ePrmNSrcChange(Sender: TObject);
var n, c: int;
    s: string;
begin
  s := TCustomEdit(Sender).Text;
  c := 0;
  
  Val(s, n, c);

  if (c > 0) or (n < 0) then
    TEdit(Sender).Color := clFuchsia
  else
    TEdit(Sender).Color := clWindow;
end;

procedure TIOMainFrm.ExecSQL(const sql: string);
begin
  qTmp.Execute(sql);
end;

function TIOMainFrm.GetCurrentNodeDevType(ndCurrent: PVirtualNode = nil): int;
var nd: PVirtualNode;
begin
  Result := -1;

  nd := GetDeviceNode(ndCurrent);
  if nd = nil then Exit;

  Result := GetTreeNodeData(nd).ID_DevType;
end;

function TIOMainFrm.GetCurrentTree: TVirtualStringTree;
begin
  if CurrentTreeType = gmttNodeTree then
    Result := NodeTreeFrame.Tree
  else
    Result := Tree;
end;

function TIOMainFrm.IsCurrentDeviceTecon(ndCurrent: PVirtualNode = nil): bool;
begin
  Result := GetCurrentNodeDevType(ndCurrent) in Tecon19_Family;
end;

function TIOMainFrm.IsCurrentDeviceSupportsUserChannels(ndCurrent: PVirtualNode = nil): bool;
begin
  Result := GetCurrentNodeDevType(ndCurrent) in DevicesSupportsUserChannels;
end;

function TIOMainFrm.ParsePrmNSrc(const s: string; bForceHex: bool = false): int;
begin
  if bForceHex or (IsCurrentDeviceTecon() and (GetTreeNodeData().ID_Src = SRC_USR)) then
    Result := StrToInt('$' + s) // для Тэконов вводим и отображаем в 16-ричной системе
  else
    Result := StrToInt(s);
end;

procedure TIOMainFrm.miTreeDelClick(Sender: TObject);
begin
  DelCurrentTreeItem();
end;

procedure TIOMainFrm.miTreeAddObjectClick(Sender: TObject);
begin
  AddObjectToTree();
end;

function TIOMainFrm.GetDeviceNode(ndCurrent: PVirtualNode = nil): PVirtualNode;
begin
  Result := nil;

  if ndCurrent = nil then
    ndCurrent := Tree.FocusedNode;

  if (ndCurrent <> nil) and (Tree.GetNodeLevel(ndCurrent) >= NODE_LEVEL_DEVICE) then
  begin
    Result := ndCurrent;
    while (Result <> nil) and (Tree.GetNodeLevel(Result) > NODE_LEVEL_DEVICE) do
      Result := Result.Parent;
  end;
end;

function TIOMainFrm.GetParentNodeWithLevel(Level: Cardinal; ndCurrent: PVirtualNode = nil): PVirtualNode;
begin
  Result := nil;

  if ndCurrent = nil then
    ndCurrent := Tree.FocusedNode;

  if (ndCurrent <> nil) and (Tree.GetNodeLevel(ndCurrent) >= Level) then
  begin
    Result := ndCurrent;
    while (Result <> nil) and (Tree.GetNodeLevel(Result) > Level) do
      Result := Result.Parent;
  end;
end;

procedure TIOMainFrm.pmTreePopup(Sender: TObject);
var bLvlForPrm, bIsPrm: bool;
    level: int;
begin
  level := Tree.GetNodeLevel(Tree.FocusedNode);
  bLvlForPrm := level >= NODE_LEVEL_DEVICE;
  bIsPrm := level >= NODE_LEVEL_PARAM;

  miTreeAddObject.Enabled := btAddObj.Enabled;
  miTreeAddDevice.Enabled := btAddDev.Enabled;
  miTreeDel.Enabled := btDel.Enabled;

  miTreeAddLostParams.Enabled := bLvlForPrm and not IsRemoteServerNode(Tree.FocusedNode);
  miTreeChnMenu.Enabled := bIsPrm and not IsRemoteServerNode(Tree.FocusedNode);
  miAddTeconUserPrm.Enabled := bLvlForPrm and IsCurrentDeviceSupportsUserChannels() and not IsRemoteServerNode(Tree.FocusedNode);
  miForceArchRequest.Enabled := (level = NODE_LEVEL_DEVICE) and (GetTreeNodeData().ID_DevType in [DEVTYPE_SPT_941, DEVTYPE_SPT_943]);

  AddDevicesAsMenuItems(miTreeAddDevice);
end;

procedure TIOMainFrm.miTreeAddLostParamsClick(Sender: TObject);
var i, n: int;
    nd: PVirtualNode;
    mi: TMenuItem;
begin
  for i := miTreeAddLostParams.Count - 1 downto 0 do
  begin
    if miTreeAddLostParams[i] <> miTreeAddLostParams_All then
      miTreeAddLostParams[i].Free();
  end;

  nd := GetDeviceNode();
  if (nd = nil) or IsRemoteServerNode(nd) then Exit;

  n := StrToIntDef(QueryResult('select max(Argument) from DevParamListGuide where ID_DevType = ' + IntToStr(GetTreeNodeData(nd).ID_DevType)), -1);
  for i := 1 to n do
  begin
    mi := TMenuItem.Create(self);
    mi.Name := 'miTreeAddLostParams_' + IntToStr(i);
    mi.Caption := 'Число трубопроводов: ' + IntToStr(i);
    mi.Tag := i;
    mi.OnClick := miTreeAddLostParams_AllClick;
    miTreeAddLostParams.Add(mi);
  end;
end;

procedure TIOMainFrm.CycleTree_LookForParam(nd: PVirtualNode; CycleObject: pointer);
begin
  if (Tree.GetNodeLevel(nd) >= NODE_LEVEL_PARAM) and (GetTreeNodeData(nd).ID_Prm = int(CycleObject)) then
  begin
    SelectNode(nd);
    Tree.FocusedNode := nd;
  end;
end;

function TIOMainFrm.IsRemoteServerNode(nd: PVirtualNode): bool;
var ndObj: PVirtualNode;
begin
  ndObj := GetTopLevelParent(nd);
  Result := (ndObj <> nil) and (GetTreeNodeData(ndObj).ObjType = OBJ_TYPE_REMOTE_SRV_XML);
end;

procedure TIOMainFrm.EnableEditorButtons();
var nd: PVirtualNode;
begin
  if CurrentTreeType = gmttNodeTree then
  begin
    btAddDev.Enabled := false;
    btDel.Enabled := false;
  end
  else
  begin
    nd := Tree.FocusedNode;

    btAddDev.Enabled := (nd <> nil) and not IsRemoteServerNode(nd);
    btDel.Enabled := (nd <> nil)
                     // and not IsRemoteServerNode(nd)
                     and ( (Tree.GetNodeLevel(nd) < NODE_LEVEL_PARAM)
                            or (Tree.GetNodeLevel(nd) = NODE_LEVEL_SUBPARAM)
                            or (GetTreeNodeData(nd).ID_Src in UserDefinedParamTypes) ); // удалять можно только пользовательские каналы
  end;
end;

procedure TIOMainFrm.FillNodeChannelEditor(vnd: PVTNodeData);
begin
  SetLength(vpt.Fields, 0);
  SetVPTFieldIntVal(AddVPTField('ID_Chn', VPT_ID, VPTCategoryMain, false), vnd.ID_Prm);
  AddVPTField('Название', VPT_Name, VPTCategoryAdditional).StringData := vnd.sTxt;
  AddVPTField('Тэг ОРС', VPT_OpcTag, VPTCategoryAdditional).StringData := vnd.sOpcTag;
  SetVPTCmbVal(AddVPTField('Тип', VPT_PType, VPTCategoryAdditional), slPrmType, vnd.ID_PT);
  SetVPTCmbVal(AddVPTField('Ед. измерения', VPT_PrmMeaUnits, VPTCategoryAdditional, vnd.ID_PT > 0), slPrmMeaUnits, vnd.ID_MeaUnit);

  SetVPTCmbVal(AddVPTField('Авария', VPT_Alarm, VPTCategoryAlarm), slPrmDIAlarmSignal, vnd.AlarmSignal);
  SetVPTFieldVariantVal(AddVPTField('Порог', VPT_AlarmThreshold, VPTCategoryAlarm), vnd.AlarmThreshold);
end;

procedure TIOMainFrm.FillNodeEditor(vnd: PVTNodeData);
begin
  SetLength(vpt.Fields, 0);
  SetVPTFieldIntVal(AddVPTField('ID_Obj', VPT_ID, VPTCategoryMain, false), vnd.ID_Obj);
  AddVPTField('Название', VPT_Name, VPTCategoryMain).StringData := vnd.sTxt;
  SetVPTCmbVal(AddVPTField('Тип', VPT_ObjType, VPTCategoryMain), slNodeType, vnd.ObjType);
end;

procedure TIOMainFrm.FillNodeTreeEditor(vnd: PVTNodeData);
begin
  if vnd.ID_Prm > 0 then
    FillNodeChannelEditor(vnd)
  else
    FillNodeEditor(vnd);

  vpt.RefreshFields();
end;

procedure TIOMainFrm.ChangeNodeTreeFocus(nd: PVirtualNode);
var vnd: PVTNodeData;
begin
  EnableEditorButtons();

  vnd := GetTreeNodeData(nd);

  if vnd = nil then
  begin
    pcConfig.ActivePage := tsEmpty;
    Exit;
  end;

  pcConfig.ActivePage := tsPrm;
  FillNodeTreeEditor(vnd);
end;

procedure TIOMainFrm.UpdateStatusBar();
var s: string;
    udt: LongWord;
begin
  if (Abs(int64(FSqlQueue_DT) - NowGM()) < 60) or (Abs(int64(FBlockQueue_DT) - NowGM()) < 60) then
  begin
    StatusBar1.Panels[0].Text := 'Очередь SQL: ' + IntToStr(FSqlQueue) + ', ' + 'ответы: ' + IntToStr(FBlockQueue) + StringOfChar(' ', 10);
    StatusBar1.Tag := 0;
  end
  else
  begin
    s := 'Нет активности сервиса';
    udt := Max(FSqlQueue, FBlockQueue);
    if udt > 0 then
      s := s + ' c ' + FormatDateTime('dd.mm.yyyy hh:nn', UTCtoLocal(udt));

    StatusBar1.Panels[0].Text := s + '!' + StringOfChar(' ', 10);
    StatusBar1.Tag := 1;
  end;
end;

procedure TIOMainFrm.WMSQLQueue(var Msg: TMessage);
begin
  FSqlQueue := Msg.WParam;
  FSqlQueue_DT := Msg.LParam;
  UpdateStatusBar();
end;

procedure TIOMainFrm.WMParserQueue(var Msg: TMessage);
begin
  FBlockQueue := Msg.WParam;
  FBlockQueue_DT := Msg.LParam;
  UpdateStatusBar();
end;

function TIOMainFrm.GetPrmTextForControl(ID_Prm: int): string;
var ndObj, ndDev, ndPrm: PVirtualNode;
    vndDev: PVTNodeData;
begin
  Result := '';

  ndObj := Tree.RootNode.FirstChild;
  while ndObj <> nil do
  begin
    ndDev := ndObj.FirstChild;
    while ndDev <> nil do
    begin
      ndPrm := ndDev.FirstChild;
      while ndPrm <> nil do
      begin
        if GetTreeNodeData(ndPrm).ID_Prm = ID_Prm then
        begin
          vndDev := GetTreeNodeData(ndDev);

          if vndDev.sTxt <> '' then
            Result := vndDev.sTxt
          else
          begin
            Result := GetDevTypeByID(GetTreeNodeData(ndPrm).ID_DevType);
            if vndDev.Number485 > 0 then
              Result := Result + '(' + IntToStr(vndDev.Number485) + ')';
          end;

          Result := Result + ' . ' + GetPrmTxt(ndPrm, []);
          
          Exit;
        end;

        ndPrm := ndPrm.NextSibling;
      end;

      ndDev := ndDev.NextSibling;
    end;

    ndObj := ndObj.NextSibling;
  end;
end;

procedure TIOMainFrm.CheckObjectControlScheme();
var Aggr: TEditAggrFrame;
    i: int;
begin
  Aggr := nil;

  qTmp.Close();
  qTmp.SQL.Text := 'select * from Aggregates where ID_Obj = ' + IntToStr(GetTreeNodeData().ID_Obj) +
                ' order by ID_Aggregate';
  qTmp.Open();
  for i := 1 to 4 do
  begin
    case i of
      1: Aggr := EditAggrFrame1;
      2: Aggr := EditAggrFrame2;
      3: Aggr := EditAggrFrame3;
      4: Aggr := EditAggrFrame4;
    end;

    if not qTmp.Eof then
    begin
      Aggr.ID_Aggr := qTmp.FieldByName('ID_Aggregate').AsInteger;
      qTmp.Next();
    end
    else
    begin
      qTmp.Close();
      qTmp.SQL.Text := ' insert into Aggregates (ID_Obj, ID_AggrType) ' +
                    // четвертый - это сборище доп. параметров, ему сразу пропишем тип 
                    '  select ' + IntToStr(GetTreeNodeData().ID_Obj) + ', ' + IfThen(i = 4, IntToStr(AGGREGATE_TYPE_PARAM_HOLDER), '0') +
                    ' returning ID_Aggregate';
      qTmp.Open();
      Aggr.ID_Aggr := qTmp.Fields[0].AsInteger;
      qTmp.Close();
    end;

    Aggr.OnGetTextForPrm := GetPrmTextForControl;
    Aggr.OnGetSelectedParam := GetDraggingParam;
    Aggr.LoadFromSQL();
  end;

  qTmp.Close();
end;

procedure TIOMainFrm.GetDraggingParam(var ID_Prm: int);
begin
  ID_Prm := 0;
  if GetTreeNodeData().ID_Prm > 0 then
    ID_Prm := GetTreeNodeData().ID_Prm;
end;

var Criteria_ID_Obj_Except: int;
procedure TIOMainFrm.ShowObjectsInTreeByCriteria(nd: PVirtualNode; CycleObject: pointer = nil);
var vnd: PVTNodeData;
begin
  vnd := GetTreeNodeData(nd);
  Tree.IsVisible[nd] := ( (Criteria_ID_Obj_Except = 0) or (vnd.ID_Obj = Criteria_ID_Obj_Except) )
                        and ( (Criteria_ID_Obj_Except = 0) or (Tree.GetNodeLevel(nd) < 2) or (vnd.ID_PT > 0)) ;
end;

procedure TIOMainFrm.ShowObjects(ID_Obj_Except: int);
begin
  Criteria_ID_Obj_Except := ID_Obj_Except;
  CycleTree(Tree.RootNode, ShowObjectsInTreeByCriteria);
end;

procedure TIOMainFrm.SetEditMode(tem: TGMTreeEditMode);
var nd: PVirtualNode;
begin
  if FEditMode = tem then Exit;

  nd := Tree.FocusedNode;
  if nd <> nil then
  begin
    while Tree.GetNodeLevel(nd) > 0 do
      nd := nd.Parent;

    SelectNode(nd);
  end;

  TreeFocusChanged(Tree, nd, 0); // это надо сделать еще со старой схемой 
  
  FEditMode := tem;

  case FEditMode of
    temEditTree:
      begin
        pcObj.ActivePageIndex := 0;
        ShowObjects(0);
      end;

    temEditControls:
      begin
        pcObj.ActivePageIndex := 1;

        ShowObjects(GetTreeNodeData().ID_Obj);
        CheckObjectControlScheme();
        Tree.FullExpand(nd);
      end;
  end;

  btAddObj.Enabled := (EditMode = temEditTree);
  btAddDev.Enabled := (EditMode = temEditTree);
  btDel.Enabled := (EditMode = temEditTree);
end;

procedure TIOMainFrm.TreeDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  // тащим только параметры в режиме создания упраления
  Allowed := (EditMode = temEditControls) and (GetTreeNodeData(Node).ID_Prm > 0);
end;

procedure TIOMainFrm.TreeDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var h: THitInfo;
    vndDrop: PVTNodeData;
begin
  Sender.GetHitTestInfoAt(Pt.X, pt.Y, true, h);
  vndDrop := GetTreeNodeData(h.HitNode);
  vndDrop.SourcePrmID := GetTreeNodeData(Sender.FocusedNode).ID_Prm;
  ExecSQL('update Params set SourcePrmID = ' + IntToStr(vndDrop.SourcePrmID) + ' where ID_Prm = ' + IntToStr(vndDrop.ID_Prm));
  Sender.FocusedNode := h.HitNode;
  Sender.Selected[h.HitNode] := true;
end;

procedure TIOMainFrm.TreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
  Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var ndDrag: PVirtualNode;
    h: THitInfo;
    vndDrop: PVTNodeData;
begin
  Accept := false;
  if (Sender <> Source) or (Mode <> dmOnNode) then Exit;

  ndDrag := Sender.FocusedNode;
  if Sender.GetNodeLevel(ndDrag) <> NODE_LEVEL_PARAM then Exit;

  Sender.GetHitTestInfoAt(Pt.X, pt.Y, true, h);
  if (h.HitNode = nil) or (Sender.GetNodeLevel(h.HitNode) <> NODE_LEVEL_PARAM) then Exit;

  vndDrop := GetTreeNodeData(h.HitNode);
  Accept := (vndDrop.ID_Src in [SRC_DO, SRC_AO])
            or ((vndDrop.ID_Src = SRC_USR) and (int(vndDrop.UserSrc) in [SRC_DO, SRC_AO]))
end;

procedure TIOMainFrm.TreeEnter(Sender: TObject);
begin
  CurrentTreeType := gmttObjectTree;
  ChangeObjectTree(Tree.FocusedNode);
end;

procedure TIOMainFrm.tsCtrlAddParamsResize(Sender: TObject);
var i: int;
begin
  for i := 0 to tsCtrlAddParams.ControlCount - 1 do
    if tsCtrlAddParams.Controls[i] is TLabeledEdit then
      tsCtrlAddParams.Controls[i].Width := tsCtrlAddParams.Width - tsCtrlAddParams.Controls[i].Left - 4;
end;


procedure TIOMainFrm.FormDestroy(Sender: TObject);
begin
  slID_Src.Free();
  slUserSrc.Free();
  slPrmArcType.Free();
  slPrmDIAlarmSignal.Free();
  slObjType.Free();
  slDevBaudrate.Free();
  slDevPortType.Free();
  slCalibrationType.Free();
  slCalibrationResistor.Free();
  slMeterCalcNITypes.Free();
  slReqIntervalType.Free();
  slMinIntegralInterval.Free();
  slTecomHourArchDepth.Free();
  slConverter.Free();
  slModbusResType.Free();

  slDevType.Free();
  slPrmType.Free();
  slPrmMeaUnits.Free();
  slNodeType.Free();

  qTmp.Free();

  thrSvcState.Free();

  ProgramLog().AddMessage('================ Stop GMIOPSrv ================');
end;

procedure TIOMainFrm.TreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Finalize(PVTNodeData(Tree.GetNodeData(Node))^);
end;

function TIOMainFrm.MainConnectSQL(Params: TZConnectionParams): bool;
begin
  Result := false;
  try
    CheckConnStrDlg := TCheckConnStrDlg.Create(Application);
    Result := CheckConnStrDlg.CheckConnectionString(Params);
  except
    on e: Exception do
      ShowMessageBox('Ошибка соединения с SQL: ' + e.Message);
  end;
end;

function TIOMainFrm.ReadAndCheckSQLParams(): bool;
var Params: TZConnectionParams;
begin
  Result := GMMainConfigFile.ReadSQLConnectionParams(Params);
  if not Result then Exit;

  Result := MainConnectSQL(Params);
  if not Result then Exit;

  SetGlobalSQLConnectionParams(Params);
  RefreshTreeAndDBLists();
end;

function TIOMainFrm.ReadINI: bool;
begin
  Result := (GMMainConfigFile.CheckMainINIFile() = iftINI) and ReadAndCheckSQLParams();
end;

procedure TIOMainFrm.StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
var r: TRect;
    s: string;
begin
  r := Rect;
  s := StatusBar.Panels[0].Text;
  StatusBar.Canvas.Font.Color := IfThen(Statusbar.Tag = 0, clBtnText, clRed);
  StatusBar.Canvas.TextRect(r, s, [tfRight, tfSingleLine, tfVerticalCenter]);
end;

procedure TIOMainFrm.RefreshNCarIDObjRelations();
var nd: PVirtualNode;
    vnd: PVTNodeData;
begin
  try
    synch.BeginWrite();
    try
      clNCarIDObjRelations.Clear();
      nd := Tree.RootNode.FirstChild;
      while nd <> nil do
      begin
        vnd := Tree.GetNodeData(nd);

        with TNCarIDObjRelation(clNCarIDObjRelations.Add()) do
        begin
          N_Car := vnd.N_Car;
          ID_Obj := vnd.ID_Obj;
          ObjType := vnd.ObjType;
        end;

        nd := nd.NextSibling;
      end;
    finally
      synch.EndWrite();
    end;
  except
    on e: Exception do
      ProgramLog().AddError('NCarIDObjRelations - ' + e.Message);
  end;
end;

procedure TIOMainFrm.TreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var diff: double;
begin
  if (Column = 0) and (Kind in [ikNormal, ikSelected]) and (Tree.GetNodeLevel(Node) <= 1) then
  begin
    diff := Now() - GetTreeNodeData(Node).tLastAccess;
    if diff > 2 * OneMinute then
      ImageIndex := 1
    else
      ImageIndex := 0;
  end
  else
    ImageIndex := -1;
end;

procedure TIOMainFrm.tmrTreeRefrTimer(Sender: TObject);
begin
  // Изредка будем взбадривать дерево на тему "а вдруг одновременно отвалились все объекты",
  // тогда  надо дернуть перериcовку, чтобы обновились иконки работает / не работает
  Tree.Refresh();
end;

{ TNCarIDObjRelationList }

function TNCarIDObjRelationList.ByIDObj(ID_Obj: int): TNCarIDObjRelation;
var i: int;
    r: TNCarIDObjRelation;
begin
  Result := nil;
  synch.BeginRead();
  try
    for i := 0 to clNCarIDObjRelations.Count - 1 do
    begin
      r := TNCarIDObjRelation(clNCarIDObjRelations.Items[i]);
      if r.ID_Obj = ID_Obj then
      begin
        Result := r;
        break;
      end;
    end;
  finally
    synch.EndRead();
  end;
end;

function TNCarIDObjRelationList.ByNCar(N_Car: int; ObjTypes: SetOfInt): TNCarIDObjRelation;
var i: int;
    r: TNCarIDObjRelation;
begin
  Result := nil;
  synch.BeginRead();
  try
    for i:=0 to clNCarIDObjRelations.Count - 1 do
    begin
      r := TNCarIDObjRelation(clNCarIDObjRelations.Items[i]);
      if (r.N_Car = N_Car) and (r.ObjType in ObjTypes) then
      begin
        Result := r;
        break;
      end;
    end;
  finally
    synch.EndRead();
  end;
end;

procedure TIOMainFrm.pmDevicesPopup(Sender: TObject);
var nd: PVirtualNode;

  procedure EnableDevTypes(Sender: TObject; bEnable: bool; DevTypes: SetOfInt);
  var i: int;
      m: TMenuItem;
  begin
    if Sender is TPopupMenu then
      m := TPopupMenu(Sender).Items
    else
    if Sender is TMenuItem then
      m := TMenuItem(Sender)
    else
      Exit;

    for i := 0 to m.Count - 1 do
    begin
      if m[i].Tag in DevTypes then
        m[i].Enabled := bEnable;

      EnableDevTypes(m[i], bEnable, DevTypes);
    end;
  end;

begin
  nd := GetTopLevelParent(Tree.FocusedNode);

  EnableDevTypes(Sender, GetTreeNodeData(nd).ObjType <> OBJ_TYPE_REMOTE_MAIN, [0..$FF]);
  EnableDevTypes(Sender, GetTreeNodeData(nd).ObjType = OBJ_TYPE_REMOTE_MAIN, [DEVTYPE_MAIN_SRV]);

  if GetTreeNodeData(nd).ObjType = OBJ_TYPE_REMOTE_MAIN then Exit;

  nd := nd.FirstChild;
  EnableDevTypes(Sender, true, GeomerFamily);

  while nd <> nil do
  begin
    if GetTreeNodeData(nd).ID_DevType in GeomerFamily then
    begin
      EnableDevTypes(Sender, false, GeomerFamily);
      break;
    end;

    nd := nd.NextSibling;
  end;
end;

function TIOMainFrm.AddUserDefinedPrm(ID_Src: int; ParentLevel: Cardinal; Unique: bool): PVirtualNode;
var nd, nd1: PVirtualNode;
    vnd: PVTNodeData;
begin
  Result := nil;

  nd := GetParentNodeWithLevel(ParentLevel);
  if nd = nil then Exit;

  if Unique then
  begin
    nd1 := nd.FirstChild;
    while nd1 <> nil do
    begin
      if GetTreeNodeData(nd1).ID_Src = ID_Src then
      begin
        Tree.FocusedNode := nd;
        Exit;
      end;

      nd1 := nd1.NextSibling;
    end;
  end;

  SelectNode(nd);
  Tree.FullExpand(nd);
  vnd := GetTreeNodeData(nd);

  qTmp.Close();
  qTmp.SQL.Text := Format(' insert into Params(ID_Device, ID_Src, N_Src, BaseChn) select %d, %d, 0, %d ' +
                          ' returning ID_Prm',
                          [vnd.ID_Device, ID_Src, IfThen(ParentLevel = NODE_LEVEL_PARAM, vnd.ID_Prm, 0)]);
  qTmp.Open();

  if not qTmp.Eof then
  begin
    Result := Tree.AddChild(nd);

    InitNode(Result);
    vnd := GetTreeNodeData(Result);
    vnd^ := GetTreeNodeData(GetDeviceNode(nd))^;

    vnd.sTxt := '';
    vnd.ID_Prm := qTmp.Fields[0].AsInteger;
    vnd.ID_Src := ID_Src;

    Tree.FocusedNode := Result;
  end;

  qTmp.Close();
  Tree.Refresh();
end;

procedure TIOMainFrm.miTreeAddIntegralPrmClick(Sender: TObject);
begin
  AddUserDefinedPrm(SRC_SUM_AI, NODE_LEVEL_PARAM, true);
end;

procedure TIOMainFrm.SetParamLastVal(nd: PVirtualNode; CycleObject: pointer);
var val: TValueFromBase;
    vnd: PVTNodeData;
begin
  val := TValueFromBaseClass(CycleObject).Val;
  vnd := GetTreeNodeData(nd);

  if (vnd.ID_Prm = TValueFromBaseClass(CycleObject).ID_Prm) and (val.UTime > vnd.LastDT) then
  begin
    vnd.LastDT := val.UTime;
    vnd.LastVal := val.Val;
  end;
end;

procedure TIOMainFrm.WMParamDataWritten(var Msg: TMessage);
begin
  try
    CycleTree(Tree.RootNode, SetParamLastVal, TObject(Msg.WParam));
    ApplyPrmLastVals();
    TryFreeAndNil(TObject(Msg.WParam));
  except
    on e: Exception do
      ProgramLog.AddError('WMParamDataWritten ' + e.Message);
  end;
end;

procedure TIOMainFrm.miAddTeconUserPrmClick(Sender: TObject);
begin
  AddUserDefinedPrm(SRC_USR, NODE_LEVEL_DEVICE, false);
end;

procedure TIOMainFrm.vptCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  TVirtualPropertyTree(Sender).RefreshFields();
end;

procedure TIOMainFrm.vptExit(Sender: TObject);
begin
  VPT.PostEdit();
end;

procedure TIOMainFrm.TreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  VPT.PostEdit();
end;

procedure TIOMainFrm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var vnd1, vnd2: PVTNodeData;
    s1, s2: WideString;
begin
  vnd1 := GetTreeNodeData(Node1);
  vnd2 := GetTreeNodeData(Node2);

  s1 := Tree.Text[Node1, Column];
  s2 := Tree.Text[Node2, Column];

  case Sender.GetNodeLevel(Node1) of
    NODE_LEVEL_OBJECT:
      begin
        if Column = 2 then
        begin
          Result := CompareValue(vnd1.N_Car, vnd2.N_Car)
        end
        else
        begin
          Result := CompareStr(s1, s2);
          if Result = 0 then
            Result := CompareValue(vnd1.N_Car, vnd2.N_Car)
        end;
      end;

    NODE_LEVEL_DEVICE:
      begin
        if Column = 0 then
          Result := CompareStr(s1, s2)
        else
          Result := CompareValue(vnd1.Number485, vnd2.Number485);
      end;

    else
      begin
        Result := CompareValue(vnd1.ID_Src, vnd2.ID_Src);
        if Result = 0 then
          Result := CompareValue(vnd1.N_Src, vnd2.N_Src);

        if Tree.Header.SortDirection = sdDescending then
          Result := -Result;
      end;
  end;
end;

procedure TIOMainFrm.TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  UpdateSortOrder(Sender, HitInfo.Column);
end;

procedure TIOMainFrm.miAddCntMtrClick(Sender: TObject);
var nd: PVirtualNode;
    vnd, vndP: PVTNodeData;
begin
  nd := AddUserDefinedPrm(SRC_CNT_MTR, NODE_LEVEL_PARAM, true);
  if nd <> nil then
  begin
    vnd := GetTreeNodeData(nd);
    vndP := GetTreeNodeData(nd.Parent);

    vnd.StartNIUDate := vndP.StartNIUDate;
    vnd.StartNIValue := vndP.StartNIValue;
    vnd.ID_PT := vndP.ID_PT;
    vnd.sTxt := 'Показания';

    vndP.StartNIUDate := null;
    vndP.StartNIValue := null;

    qTmp.Close();
    qTmp.SQL.Text := Format(' update Params set NI_StartVal = %s, NI_StartDT = %d, ID_PT = %d, Name = ''%s'' where ID_Prm = %d; ' +
                            ' update Params set NI_StartVal = null, NI_StartDT = null where ID_Prm = %d',
                            [MyFloatToStr(vnd.StartNIValue), vnd.StartNIUDate, vnd.ID_PT, vnd.sTxt, vnd.ID_Prm, vndP.ID_Prm]);
    qTmp.ExecSQL();

    TreeFocusChanged(Tree, nd, 0);
  end;
end;

procedure TIOMainFrm.N4Click(Sender: TObject);
begin
  miExit.Caption := IfThen(GetAsyncKeyState(VK_CONTROL) < 0, 'Выход', 'Свернуть окно');
end;

procedure TIOMainFrm.miViewClick(Sender: TObject);
begin
  miStructure.Checked := NodeTreeFrame.Visible;
end;

procedure TIOMainFrm.miExitClick(Sender: TObject);
begin
  Close();
end;

procedure TIOMainFrm.miEditModeTreeClick(Sender: TObject);
begin
  EditMode := temEditTree;
end;

procedure TIOMainFrm.miEditModeControlsClick(Sender: TObject);
begin
  EditMode := temEditControls;
end;

procedure TIOMainFrm.N6Click(Sender: TObject);
begin
  miEditModeTree.Enabled := Tree.FocusedNode <> nil;
  miEditModeTree.Checked := miEditModeTree.Enabled and (EditMode = temEditTree);

  miEditModeControls.Enabled := Tree.FocusedNode <> nil;
  miEditModeControls.Checked := miEditModeControls.Enabled and (EditMode = temEditControls);
end;

procedure TIOMainFrm.N7Click(Sender: TObject);
begin
  EditMode := temEditTree;
  RefreshTreeAndDBLists();
  TreeFocusChanged(Tree, nil, 0);
  NodeTreeFrame.Load(true, TSQLNodeReader);
end;

procedure TIOMainFrm.N8Click(Sender: TObject);
begin
  UsersDlg.ShowModal();
end;

procedure TIOMainFrm.ShowNodeTree(AShow: bool);
begin
  NodeTreeFrame.Visible := AShow;
  Splitter2.Visible := AShow;

  if AShow then
    Splitter2.Left := NodeTreeFrame.Width + 1;
end;

procedure TIOMainFrm.miStructureClick(Sender: TObject);
begin
  ShowNodeTree(not NodeTreeFrame.Visible);
end;

procedure TIOMainFrm.miTreeAddLostParams_AllClick(Sender: TObject);
var nd: PVirtualNode;
begin
  nd := GetDeviceNode();
  if nd = nil then Exit;

  SelectNode(nd);
  Tree.DeleteChildren(nd);
  AddDeviceLostParams(GetTreeNodeData(nd).ID_Device, TMenuItem(Sender).Tag);
  ReadDeviceParams(nd);
  Tree.FullExpand(nd);
end;

procedure TIOMainFrm.NodeTreeFrameTreeDblClick(Sender: TObject);
var vnd: PVTNodeData;
begin
  vnd := NodeTreeFrame.Tree.GetNodeData(NodeTreeFrame.Tree.FocusedNode);
  if (vnd = nil) or (vnd.ID_Prm <= 0) or (vnd.BaseChn <= 0) then Exit;

  CycleTree(Tree.RootNode, CycleTree_LookForParam, TObject(vnd.BaseChn));
end;

procedure TIOMainFrm.NodeTreeFrameTreeEnter(Sender: TObject);
begin
  CurrentTreeType := gmttNodeTree;
  ChangeNodeTreeFocus(NodeTreeFrame.Tree.FocusedNode);
end;

procedure TIOMainFrm.NodeTreeFrameTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  ChangeNodeTreeFocus(Node);
end;

procedure TIOMainFrm.NodeTreeFrameTreeFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  VPT.PostEdit();
end;

procedure TIOMainFrm.N10Click(Sender: TObject);
begin
  TSysConfigDlg.Dlg.ShowModal();
end;

procedure TIOMainFrm.miForceArchRequest_FullClick(Sender: TObject);
var stateName, sql: string;
begin
  case TMenuItem(Sender).Tag of
    0: stateName := DEV_STATE_NAMES_LAST_ARCH_REQ_DAY_DEPTH;
    1: stateName := DEV_STATE_NAMES_LAST_ARCH_REQ_MONTH_DEPTH;
    else stateName := DEV_STATE_NAMES_LAST_ARCH_REQ_FULL_DEPTH;
  end;

  sql := Format('delete from DevStates where ID_Device = %d and StateName like ''%s%%''', [GetTreeNodeData().ID_Device, stateName]);
  ExecSQL(sql);
end;

{ TServerObjectOnlineThread }

procedure TServerStateThread.CheckSqlQueueOneParam(const statesType: string; msg: int);
var n: int;
    ut: LongWord;
    arr: ArrayOfString;
begin
  try
    n := 0;
    ut := 0;
    arr := QueryResultArray_FirstRow('select StateValue, UTUpdate from SystemStates where StateName = ' + statesType.QuotedString());
    if Length(arr) >= 2 then
    begin
      n := StrToIntDef(arr[0], 0);
      ut := StrToIntDef(arr[1], 0);
    end;

    GMPostMessage(msg, n, ut);
  except
  end;
end;

procedure TServerStateThread.CheckSqlQueue();
begin
  CheckSqlQueueOneParam(SYSTEM_STATES_SQL_QUEUE, WM_SQL_QUEUE);
  CheckSqlQueueOneParam(SYSTEM_STATES_PARSER_QUEUE, WM_PARSER_QUEUE);
end;

procedure TServerStateThread.ReadObjectState(q: TGMSqlQuery; obj: pointer);
begin
  GMPostMessage(WM_OBJECT_ONLINE, q.FieldByName('ID_Obj').AsInteger, q.FieldByName('LastOnline').AsInteger)
end;

procedure TServerStateThread.CheckObjectsOnline();
begin
  ReadFromQuery('select * from ObjectStates', ReadObjectState);
end;

procedure TServerStateThread.ReadDeviceState(q: TGMSqlQuery; obj: pointer);
begin
  GMPostMessage(WM_DEVICE_ONLINE, q.FieldByName('ID_Device').AsInteger, q.FieldByName('LastOnline').AsInteger)
end;

procedure TServerStateThread.CheckDevicesOnline();
begin
  ReadFromQuery('select * from DevStates where StateName = ' + QuotedStr(DEV_STATE_NAMES_LAST_ONLINE), ReadDeviceState);
end;

procedure TServerStateThread.ReadDataWritten(q: TGMSqlQuery; obj: pointer);
var val: TValueFromBaseClass;
begin
  val := TValueFromBaseClass.Create();
  val.ID_Prm := q.FieldByName('ID_Prm').AsInteger;
  val.Val.UTime := q.FieldByName('UTime').AsInteger;
  val.Val.Val := q.FieldByName('Val').AsFloat;

  GMPostMessage(WM_PARAM_DATA_WRITTEN, WParam(val), 0);
end;

procedure TServerStateThread.CheckDataWritten();
begin
  ReadFromQuery('select * from CurrVals', ReadDataWritten);
end;

procedure TServerStateThread.SafeExecute;
begin
  while not Terminated do
  begin
    try
      CheckSqlQueue();
      CheckObjectsOnline();
      CheckDevicesOnline();
      CheckDataWritten();
    except
      on e: Exception do
        ProgramLog.AddError('TServerStateThread.SafeExecute - ' + e.Message);
    end;

    SleepThread(1000);
  end;
end;

initialization
  clNCarIDObjRelations := TNCarIDObjRelationList.Create(TNCarIDObjRelation);
  synch := TMultiReadExclusiveWriteSynchronizer.Create();
finalization
  clNCarIDObjRelations.Free();
  synch.Free();
end.

