unit Test.Devices.Logica.SPT941.ReqCreator;

interface

uses Windows, TestFrameWork, Devices.Logica.SPT941, GMGlobals, Test.Devices.Base.ReqCreator, GmSqlQuery, Test.Devices.Base.ReqParser;

type
  TSPT941ReqCreatorFortest = class(TSPT941ReqCreator)
  protected
    function CanRequestPack(): bool; override;
  end;

  TLogicaSPT941ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  private
    procedure DropDevStates;
    procedure LastReqTimeStates_CheckPeriod(n: int);
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  published
    procedure LastReqTimeStates();
  end;

implementation

uses DateUtils, SysUtils, GMConst, Threads.ResponceParser, UsefulQueries;

{ TLogicaReqCreatorTest }

procedure TLogicaSPT941ReqCreatorTest.DoCheckRequests;
var n: int;
begin
  Check(ReqList.Count >= 3, 'Count = ' + IntToStr(ReqList.Count));

	CheckReqHexString(0, 'FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF'); // инициализация линии
  CheckReqHexString(1, '10 21 90 0 0 5 0 3F 0 0 0 0 12 21'); // запрос сеанса
  CheckReqHexString(2, '10 21 90 01 00 74 00 ' +  // заголовок
                       '72 ' + // FNC
                       '4A 3 0 3 4 4A 3 0 4 4 4A 3 0 5 4 4A 3 0 6 4 4A 3 0 7 4 4A 3 0 8 4 4A 3 0 9 4 4A 3 0 A 4 4A 3 0 B 4 ' + // DATA
                       '4A 3 0 C 4 4A 3 0 D 4 4A 3 0 E 4 4A 3 0 F 4 4A 3 0 10 4 4A 3 0 11 4 4A 3 0 12 4 4A 3 0 13 4 ' + // DATA cont.
                       '4A 3 0 0 8 4A 3 0 1 8 4A 3 0 2 8 4A 3 0 3 8 4A 3 0 4 8 4A 3 0 5 8 ' + // DATA cont.
                       '88 C9'); // CRC
  Check(ReqList[2].ReqID = 1, 'ReqList[2].ReqID');
  Check(ReqList[2].rqtp = rqtSPT941, 'ReqList[2].rqtp');
  Check(ReqList[2].ReqLinkCount = 23, 'ReqList[2].ReqLinkCount');

  n := StrToIntDef(QueryResult('select LastReqNumber from ObjectStates where ID_Obj = (select ID_Obj from Devices where ID_Device = ' + IntToStr(GetID_Device()) + ')'), 0);
  Check(n >= 2, 'ObjectStates');
end;

function TLogicaSPT941ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_SPT_941;
end;

procedure TLogicaSPT941ReqCreatorTest.LastReqTimeStates_CheckPeriod(n: int);
var ut: ULong;
    i: int;
begin
  ReqList.Clear();
  ReqCreator.AddDeviceToSendBuf();
  for i := High(FSPT941ArchReqPeriodTypes) downto n do
  begin
    ut := StrToIntDef(SQLReq_GetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[i].NameWithChannel(0)), 0);
    Check(Abs(NowGM - ut) < UTC_MINUTE);
  end;
end;

procedure TLogicaSPT941ReqCreatorTest.LastReqTimeStates;
begin
  LastReqTimeStates_CheckPeriod(0);
  Check(ReqList.Count > 400, 'FullCount = ' + IntToStr(ReqList.Count));

  DropDevStates();
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[0].NameWithChannel(0), NowGM() - 40 * UTC_DAY);
  LastReqTimeStates_CheckPeriod(0);
  Check(ReqList.Count > 400, 'FullCount2 = ' + IntToStr(ReqList.Count));

  DropDevStates();
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[0].NameWithChannel(0), NowGM() - 10 * UTC_DAY);
  LastReqTimeStates_CheckPeriod(1);
  Check(ReqList.Count > 100, 'MonthCount = ' + IntToStr(ReqList.Count));

  DropDevStates();
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[0].NameWithChannel(0), NowGM() - 10 * UTC_DAY);
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[1].NameWithChannel(0), NowGM() - 8 * UTC_DAY);
  LastReqTimeStates_CheckPeriod(2);
  Check(ReqList.Count > 100, 'MonthCount2 = ' + IntToStr(ReqList.Count));

  DropDevStates();
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[0].NameWithChannel(0), NowGM() - 10 * UTC_DAY);
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[1].NameWithChannel(0), NowGM() - 5 * UTC_DAY);
  LastReqTimeStates_CheckPeriod(2);
  Check(ReqList.Count > 5, 'DayCount = ' + IntToStr(ReqList.Count));

  DropDevStates();
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[0].NameWithChannel(0), NowGM() - 10 * UTC_DAY);
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[1].NameWithChannel(0), NowGM() - 5 * UTC_DAY);
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[2].NameWithChannel(0), NowGM() - 2 * UTC_HOUR);
  LastReqTimeStates_CheckPeriod(2);
  Check(ReqList.Count > 5, 'DayCount2 = ' + IntToStr(ReqList.Count));

  DropDevStates();
  ReqList.Clear();
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[0].NameWithChannel(0), NowGM() - 10 * UTC_DAY);
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[1].NameWithChannel(0), NowGM() - 5 * UTC_DAY);
  SQLReq_SetDevState(ReqCreator.ReqDetails.ID_Device, FSPT941ArchReqPeriodTypes[2].NameWithChannel(0), NowGM() - 5 * UTC_MINUTE);
  ReqCreator.AddDeviceToSendBuf();
  Check(ReqList.Count = 3, 'NoRecCount = ' + IntToStr(ReqList.Count));
end;

procedure TLogicaSPT941ReqCreatorTest.DropDevStates();
begin
  ExecSQL(Format('delete from DevStates where ID_Device = %d', [ReqCreator.ReqDetails.ID_Device]));
end;

procedure TLogicaSPT941ReqCreatorTest.SetUp;
begin
  inherited SetUp;
  DropDevStates();
end;

procedure TLogicaSPT941ReqCreatorTest.TearDown;
begin
  inherited;
end;

{ TSPT941ReqCreatorFortest }

function TSPT941ReqCreatorFortest.CanRequestPack: bool;
begin
  Result := true;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Logica', TLogicaSPT941ReqCreatorTest.Suite);
end.

