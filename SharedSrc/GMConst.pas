////////////////////////////////////////////
// Константы
////////////////////////////////////////////
unit GMConst;

interface

uses Messages;

const INCORRECT_VALUE = -100500;

      UTC_MINUTE = 60;
      UTC_HALFHOUR = 30 * UTC_MINUTE;
      UTC_HOUR = 60 * UTC_MINUTE;
      UTC_DAY = 24 * UTC_HOUR;

      DEFAULT_TIME_ZONE = 100;

      MAX_REQ_PACK_SIZE = 1000; // максимально возможный размер пакета опроса MODBUS

const WM_UPDATE_COM_LOG = WM_USER + 1; // LParam - TStringClass
      WM_UPDATE_OBJECT = WM_USER + 2;
      WM_REFRESH_OBJECT_FRAME = WM_USER + 3;
      WM_ALARM_CHANGED = WM_USER + 4;
      WM_CHECK_LAST_DATA = WM_USER + 5;
      WM_ORDER_READY = WM_USER + 7;
      WM_REQUEST_DIAGRAMS = WM_USER + 8;
      WM_ALARM_STATE_CHANGED = WM_USER + 9;
      WM_SHOW_CONTROL_PANEL = WM_USER + 10; // LParam - Visible, WParam - ID_Obj
      WM_DEVICE_ONLINE = WM_USER + 11;
      WM_ACTIVATE_FROM_TRAY = WM_USER + 12;
      WM_GEOMER_BLOCK = WM_USER + 13;
      WM_SQL_QUEUE = WM_USER + 14;
      WM_PARAM_DATA_WRITTEN = WM_USER + 15;
      WM_UDP_SEND = WM_USER + 16;
      WM_CLIENT_CREATE_OBJECTS = WM_USER + 17;
      WM_OPC_UPDATE_CHANNELS = WM_USER + 18;
      WM_SQL_CONNECTION_STATE = WM_USER + 19;
      WM_OBJECT_ONLINE = WM_USER + 20;
      WM_AFTER_CONSTRUCTION = WM_USER + 22;
      WM_DESTROY_OBJECT = WM_USER + 23; // WParam - объект для удаления
      WM_REFRESH_OBJECTS_TREE = WM_USER + 24; // WParam - объект для удаления
      WM_PARSER_QUEUE = WM_USER + 25;
      WM_THREAD_EXCEPTION = WM_USER + 26; // WParam - сбойный поток

      COM_LOG_IN = 1;
      COM_LOG_OUT = 2;

      RESULT_CLIENT_CREATE_OBJECTS_OK = 0;
      RESULT_CLIENT_CREATE_OBJECTS_LOGIN_FAILED = 1;
      RESULT_CLIENT_CREATE_OBJECTS_CONNECTION_FAILED = 2;

const
  // Типы каналов
  SRC_AI = 1;
  SRC_DI = 2;
  SRC_DO = 3;
  SRC_CNT_DI = 4;
  SRC_AO = 5;
  SRC_SUM_AI = 6;
  SRC_USR = 7;
  SRC_FK = 8;
  SRC_CNT_MTR = 9;
  SRC_AO_10 = 10;
  SRC_MAX = SRC_AO_10;

  UserDefinedParamTypes = [SRC_SUM_AI, SRC_USR, SRC_FK, SRC_CNT_MTR];
  CounterParamTypes = [SRC_SUM_AI, SRC_CNT_DI, SRC_CNT_MTR];
  SubParamTypes = [SRC_SUM_AI, SRC_FK, SRC_CNT_MTR];
  OutputParamTypes = [SRC_DO, SRC_AO, SRC_AO_10];

  // Типы опроса объектов
  OBJ_TYPE_CLIENT = -2;
  OBJ_TYPE_UNKNOWN = -1;
  OBJ_TYPE_GM = 0;
  OBJ_TYPE_COM = 1;
  OBJ_TYPE_REMOTE_SRV = 2;
  OBJ_TYPE_K104 = 3;
  OBJ_TYPE_K105 = 4;
  OBJ_TYPE_ANCOM = 5;
  OBJ_TYPE_TCP = 6;
  OBJ_TYPE_REMOTE_SRV_XML = 7;
  OBJ_TYPE_REMOTE_MAIN = 8;

  // Типы конвертеров
  PROTOCOL_CONVETER_MODBUS_TCP_RTU = 1;

  // Типы результирующих запросов по Modbus
  MODBUS_RESULT_WORD_BIG_ENDIAN = 0;
  MODBUS_RESULT_WORD_LITTLE_ENDIAN = 1;
  MODBUS_RESULT_LONG_BIG_ENDIAN = 2;
  MODBUS_RESULT_LONG_LITTLE_ENDIAN = 3;
  MODBUS_RESULT_SINGLE_BIG_ENDIAN = 4;
  MODBUS_RESULT_SINGLE_LITTLE_ENDIAN = 5;
  MODBUS_RESULT_DOUBLE_BIG_ENDIAN = 6;
  MODBUS_RESULT_DOUBLE_LITTLE_ENDIAN = 7;
  MODBUS_RESULT_SINGLE_SHUFFLE = 8;

  // Типы портов Геомера
  PORTTYPE_RS485 = 0;
  PORTTYPE_RS232 = 1;

  // Типы устройств
  DEVTYPE_GM = 1;
  DEVTYPE_I7041D = 2;
  DEVTYPE_UBZ = 3;
  DEVTYPE_I7017 = 4;
  DEVTYPE_TR101 = 5;
  DEVTYPE_TRM138 = 6;
  DEVTYPE_VZLET_URSV = 7;
  DEVTYPE_VACON_NXL = 8;

  DEVTYPE_TECON_19_01 = 9;
  DEVTYPE_TECON_19_02 = 10;
  DEVTYPE_TECON_19_03 = 11;
  DEVTYPE_TECON_19_04 = 12;
  DEVTYPE_TECON_19_05 = 13;
  DEVTYPE_TECON_19_06 = 14;
  DEVTYPE_TECON_19_07 = 15;
  DEVTYPE_TECON_19_08 = 16;
  DEVTYPE_TECON_19_09 = 17;
  DEVTYPE_TECON_19_10 = 18;
  DEVTYPE_TECON_19_11 = 19;
  DEVTYPE_TECON_19_12 = 20;
  DEVTYPE_TECON_19_13 = 21;
  DEVTYPE_TECON_19_14 = 22;

  DEVTYPE_ISCO_4250 = 23;
  DEVTYPE_SPT_961 = 24;
  DEVTYPE_SIMAG11 = 25;
  DEVTYPE_GM_ISCO = 26;
  DEVTYPE_PETERFLOWRS = 27;
  DEVTYPE_MERCURY_230 = 28;
  DEVTYPE_ALTISTART_22 = 29;
  DEVTYPE_GEOSTREAM_71 = 30;
  DEVTYPE_ALLEN_BRADLEY_MICRO_8XX = 31;
  DEVTYPE_MAIN_SRV = 32;
  DEVTYPE_SPT_941 = 33;
  DEVTYPE_SPT_943 = 34;
  DEVTYPE_GM_UBZ = 35;
  DEVTYPE_MODBUS_RTU = 36;
  DEVTYPE_DRK = 37;
  DEVTYPE_ADCP_CHANNEL_MASTER = 38;
  DEVTYPE_STREAMLUX700F = 39;

  Tecon19_Family = [DEVTYPE_TECON_19_01..DEVTYPE_TECON_19_14];
  GeomerFamily = [DEVTYPE_GM, DEVTYPE_GM_ISCO, DEVTYPE_GM_UBZ];
  FreeConfigModbusDevices = [DEVTYPE_ALLEN_BRADLEY_MICRO_8XX, DEVTYPE_MODBUS_RTU];
  DevicesSupportsUserChannels = Tecon19_Family + FreeConfigModbusDevices + [DEVTYPE_MAIN_SRV];
  ModbusRTUBasedDevices = [DEVTYPE_UBZ, DEVTYPE_TR101, DEVTYPE_VACON_NXL, DEVTYPE_ALTISTART_22, DEVTYPE_GEOSTREAM_71, DEVTYPE_MODBUS_RTU];

  // Параметры для управления агрегатами
  CONTROL_PRM_ENG_I = 1;
  CONTROL_PRM_ENG_U = 2;
  CONTROL_PRM_ENG_P = 3;
  CONTROL_PRM_ENG_A = 4;

  CONTROL_PRM_VALVE_P1 = 5;
  CONTROL_PRM_VALVE_P2 = 6;
  CONTROL_PRM_VALVE_Q = 7;

  CONTROL_PRM_HEAT_T = 8;

  CONTROL_PRM_MANUAL = 9;
  CONTROL_PRM_RUN = 10;

  CONTROL_PRM_PRESET = 11;
  CONTROL_PRM_RESETALARM = 12;

  CONTROL_PRM_ADD1 = 101;
  CONTROL_PRM_ADD2 = 102;
  CONTROL_PRM_ADD3 = 103;
  CONTROL_PRM_MAX = CONTROL_PRM_ADD3;

  // Типы управляемых агрегатов
  AGGREGATE_TYPE_UNSPECIFIED = 0;
  AGGREGATE_TYPE_ENGINE = 1;
  AGGREGATE_TYPE_VALVE = 2;
  AGGREGATE_TYPE_HEAT = 3;
  AGGREGATE_TYPE_PARAM_HOLDER = 100;

  // ID_PT
  ID_PT_LEVEL = 1;
  ID_PT_COMMON_ALARM = 11;
  ID_PT_PRESSURE = 12;
  ID_PT_FLOW = 14;
  ID_PT_UBZ_ALARM = 20;
  ID_PT_TR101_ALARM = 37;
  ID_PT_PRESET = 42;
  ID_PT_START_ENGINE = 28;
  ID_PT_PID_REFERENCE = 46;

  // Типы графика в запросе
  DIAGRAMTYPE_DEFAULT = 0;
  DIAGRAMTYPE_INTEGRAL = 1;
  DIAGRAMTYPE_TOTAL = 2;
  DIAGRAMTYPE_REMOTE = 255;

  // Уровни в дереве объектов
  NODE_LEVEL_OBJECT = 0;
  NODE_LEVEL_DEVICE = 1;
  NODE_LEVEL_PARAM = 2;
  NODE_LEVEL_SUBPARAM = 3;

  // Метод расчета показаний
  CALC_NI_TYPE_CALC = 0;
  CALC_NI_TYPE_LAST = 1;

  // Типы узлов
  NODE_TYPE_NODE = 1; // Обычный узел дерева
  NODE_TYPE_STATION = 2; // Узел теплоучета
  NODE_TYPE_PIPE = 3;  // Труба

  // Типы каналов
  DATACHANNEL_PARAM = 0; // параметр из аппаратной чати
  DATACHANNEL_NODECHANNEL = 1; // канал из узлов

  // Типы данных
  DATATYPE_CURRENTS = 0;
  DATATYPE_CURRENT_DIAGRAM = 1;
  DATATYPE_HOUR_DIAGRAM = 2;
  DATATYPE_DAY_DIAGRAM = 3;
  DATATYPE_NI = 4;
  DATATYPE_COMMAND = 5;

  DATATYPE_DIAGRAM_ANY = [DATATYPE_CURRENT_DIAGRAM, DATATYPE_HOUR_DIAGRAM, DATATYPE_DAY_DIAGRAM];

  DATA_AGGREGATION_TYPE_DEFAULT = 0;
  DATA_AGGREGATION_TYPE_DAYSUMM = 1;

  // константы типов данных для VirtualPropertyTree
  type TVPTPropertyType = (

  // общие
    VPT_ID
  , VPT_Name
  , VPT_Info // Пихаем что попало, редактировать все равно будет нельзя

  // каналы
  , VPT_ID_Src
  , VPT_UserSrc
  , VPT_N_Src
  , VPT_PType
  , VPT_PrmMeaUnits
  , VPT_CalibrType
  , VPT_DevMin
  , VPT_DevMax
  , VPT_RealMin
  , VPT_RealMax
  , VPT_CalibrationMark
  , VPT_SensorType
  , VPT_Resistor
  , VPT_Alarm
  , VPT_AlarmThreshold
  , VPT_StartNIDate
  , VPT_StartNIValue
  , VPT_CounterCeiling
  , VPT_MeterCalcNIType
  , VPT_OpcTag
  , VPT_ReqIntervalType
  , VPT_MinIntegralInterval
  , VPT_HourArchArgument
  , VPT_HourArchAddr
  , VPT_SourcePrmID
  , VPT_AgeAddr
  , VPT_MainSrvPrmID
  , VPT_ModbusResultType

  // устройства
  , VPT_ID_DevType
  , VPT_DevPortType
  , VPT_DevBaudRate
  , VPT_DevNumber
  , VPT_DevReqPackSize
  , VPT_DevAddrBase

  // объекты
  , VPT_ObjType
  , VPT_NCar
  , VPT_IP
  , VPT_Port
  , VPT_UserData
  , VPT_Converter
  );

  const VPTCategoryMain = 'Основные';
        VPTCategoryAdditional = 'Дополнительные';
        VPTCategoryCalibr = 'Калибровка';
        VPTCategoryAlarm = 'Авария';
        VPTCategoryStartNI = 'Показания';
        VPTCategoryDeviceToDevice = 'Автоуправление';

  const CALIBR_TYPE_MANUAL = 0;

        CALIBR_TYPE_U_0_1 = 1;
        CALIBR_TYPE_U_0_5 = 2;
        CALIBR_TYPE_U_0_10 = 3;

        CALIBR_TYPE_I_4_20 = 4;
        CALIBR_TYPE_I_0_20 = 5;
        CALIBR_TYPE_I_0_5 = 6;

  // login - доступ по паролю
  SYS_CONFIG_PARAM_AUTH_REQUIRED = 'AuthRequired';

  AUTH_DEFAULT_LOGIN = 'user';
  AUTH_DEFAULT_PASSWORD = '123';

type
  // типы запросов по 485
  T485RequestType = ( rqtNone,

                      rqtDummy,
                      rqtCommand,

                      rqtUBZ_ALARMS,
                      rqtUBZ_TRANSFORMATOR,
                      rqtUBZ_I,
                      rqtUBZ_U,
                      rqtUBZ_A,
                      rqtUBZ_P,
                      rqtUBZ_CMD,
                      rqtUBZ_ALARM_LOG1,
                      rqtUBZ_ALARM_LOG2,

                      rqtTR101_STATE,
                      rqtTR101_SP1,
                      rqtTR101_SP2,
                      rqtTR101_SP3,
                      rqtTR101_SP4,

                      rqtICP7041D_ALLDI,
                      rqtICP7041D_CNT0,
                      rqtICP7041D_CNT1,
                      rqtICP7041D_CNT2,
                      rqtICP7041D_CNT3,
                      rqtICP7041D_CNT4,
                      rqtICP7041D_CNT5,
                      rqtICP7041D_CNT6,
                      rqtICP7041D_CNT7,
                      rqtICP7041D_CNT8,
                      rqtICP7041D_CNT9,
                      rqtICP7041D_CNT10,
                      rqtICP7041D_CNT11,
                      rqtICP7041D_CNT12,
                      rqtICP7041D_CNT13,

                      rqtICP7017,

                      rqtTRM138_T1,
                      rqtTRM138_T2,
                      rqtTRM138_T3,
                      rqtTRM138_T4,
                      rqtTRM138_T5,
                      rqtTRM138_T6,
                      rqtTRM138_T7,
                      rqtTRM138_T8,

                      rqtVZLET_URSV_Q1,
                      rqtVZLET_URSV_Q2,
                      rqtVZLET_URSV_Q3,
                      rqtVZLET_URSV_Q4,

                      rqtVACON_NXL_ENGINE_CURRENT,
                      rqtVACON_NXL_ENGINE_SPEED,
                      rqtVACON_NXL_ALARM,
                      rqtVACON_NXL_POWER,
                      rqtVACON_NXL_PRESET_SPEED,
                      rqtVACON_NXL_PID_REFERENCE,

                      rqtISCO4250,

                      rqtTECON_SET_TIME,
                      rqtTECON_HourArch,

                      rqtUSER_DEFINED,

                      rqtSPT941,
                      rqtSPT943_0,
                      rqtSPT943_1,
                      rqtSPT943_2,

                      rqtSPT961,
                      rqtSPT961_DAY,
                      rqtSPT961_HOUR,

                      rqtSimag11,

                      rqtPeterFlowRS_Currents,

                      rqtMercury230_MeterInfo,
                      rqtMercury230_LastArchAddr,
                      rqtMercury230_Currents,
                      rqtMercury230_Archive,

                      rqtAltistart22_All,
                      rqtGeostream_All,
                      rqtGeostream_TCP,

                      rqtDRK,

                      rqtADCP_Channel_Master,

                      rqtMainSrv,

                      rqtStreamlux700f
                     );

  T485RequestTypeHelper = record helper for T485RequestType
    function ToString: string;
  end;

  TGMArchType = (gmarchHour, gmarchDay, gmarchMonth, gmarchYear);

// константы разные
const
  BAR_ALARM_TYPE_UP = 0;
  BAR_ALARM_TYPE_HYSTERESIS = 1;
  BAR_ALARM_TYPE_DOWN = 2;

  REPORTLENGTH_USER_SEFINE = 0;
  REPORTLENGTH_HALFHOUR = 1;
  REPORTLENGTH_HOUR = 2;
  REPORTLENGTH_DAY = 3;
  REPORTLENGTH_MONTH = 4;
  REPORTLENGTH_YEAR = 5;

// ID сервера при работе с модемами AnCom
  AncomServerID = 'SIRIUS';

// число чисел в линии на объекте в клиенте
  NominalEditBoxOnLineCount = 5;

  USER_STATE_COMMON = 0;
  USER_STATE_ADMIN = 1;
  USER_STATE_GROUP = 2;
  USER_STATE_ADMIN_GROUP = 3;

  // статусы команды
  COMMAND_STATE_PENDING = 0;
  COMMAND_STATE_EXECUTED = 1;
  COMMAND_STATE_ERROR = 2;
  COMMAND_STATE_CANCELLED = 3;
  COMMAND_STATE_EXPIRED = 4;

implementation

uses
  System.TypInfo;

{ T485RequestTypeHelper }

function T485RequestTypeHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(T485RequestType), ord(self));
end;

end.


