unit Test.Devices.Logica.SPT943.ReqCreator;

interface

uses Windows, TestFrameWork, Devices.Logica.SPT943, GMGlobals, Test.Devices.Base.ReqCreator, GmSqlQuery;

type
  TSPT943ReqCreatorFortest = class(TSPT943ReqCreator)
  protected
    function CanRequestPack(): bool; override;
    function NeedReqArch(): int; override;
  end;

  TLogicaSPT943ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetDevType(): int; override;
  protected
    procedure DoCheckRequests(); override;
  published
    procedure CheckCRC();
  end;

implementation

uses DateUtils, SysUtils, GMConst;

{ TLogicaReqCreatorTest }

procedure TLogicaSPT943ReqCreatorTest.CheckCRC;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('10 ff 90 00 00 05 00 3f 00 00 00 00 d9 19');
  Check(LogicaSPT943_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray('10 ff 90 4f 00 0b 00 72 4a 03 00 01 04 4a 03 00 00 04 27 0e');
  Check(LogicaSPT943_CheckCRC(buf, Length(buf)));
end;

procedure TLogicaSPT943ReqCreatorTest.DoCheckRequests;
begin
  Check(ReqList.Count >= 5, 'Count = ' + IntToStr(ReqList.Count));

	CheckReqHexString(0, 'FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF'); // инициализация линии
  CheckReqHexString(1, '10 22 90 0 0 5 0 3F 0 0 0 0 6A DB'); // запрос сеанса

  CheckReqHexString(2, '10 22 90 1 0 10 00 72 4A 3 0 2 4 4A 3 0 3 4 4A 3 0 0 8 A5 1A'); // Chn 0
  Check(ReqList[2].ReqID = 1, 'ReqList[2].ReqID');
  Check(ReqList[2].rqtp = rqtSPT943_0, 'ReqList[2].rqtp');
  Check(ReqList[2].ReqLinkCount = 3, 'ReqList[2].ReqLinkCount');

  CheckReqHexString(3, '10 22 90 2 0 60 00 ' +  // заголовок
                       '72 ' + // FNC
                       '4A 3 1 1 4 4A 3 1 2 4 4A 3 1 3 4 4A 3 1 4 4 4A 3 1 5 4 4A 3 1 6 4 4A 3 1 7 4 4A 3 1 8 4 4A 3 1 9 4 ' + // DATA
                       '4A 3 1 A 4 4A 3 1 B 4 4A 3 1 0 8 4A 3 1 1 8 4A 3 1 2 8 4A 3 1 3 8 4A 3 1 4 8 4A 3 1 5 8 4A 3 1 6 8 4A 3 1 7 8 ' + // DATA cont.
                       '1D 0F'); // CRC
  Check(ReqList[3].ReqID = 2, 'ReqList[3].ReqID');
  Check(ReqList[3].rqtp = rqtSPT943_1, 'ReqList[3].rqtp');
  Check(ReqList[3].ReqLinkCount = 19, 'ReqList[3].ReqLinkCount');

  CheckReqHexString(4, '10 22 90 3 0 60 00 ' +  // заголовок
                       '72 ' + // FNC
                       '4A 3 2 1 4 4A 3 2 2 4 4A 3 2 3 4 4A 3 2 4 4 4A 3 2 5 4 4A 3 2 6 4 4A 3 2 7 4 4A 3 2 8 4 4A 3 2 9 4 ' + // DATA
                       '4A 3 2 A 4 4A 3 2 B 4 4A 3 2 0 8 4A 3 2 1 8 4A 3 2 2 8 4A 3 2 3 8 4A 3 2 4 8 4A 3 2 5 8 4A 3 2 6 8 4A 3 2 7 8 ' + // DATA cont.
                       'F8 65'); // CRC
  Check(ReqList[4].ReqID = 3, 'ReqList[4].ReqID');
  Check(ReqList[4].rqtp = rqtSPT943_2, 'ReqList[4].rqtp');
  Check(ReqList[4].ReqLinkCount = 19, 'ReqList[4].ReqLinkCount');

  CheckEquals('4', QueryResult('select LastReqNumber from ObjectStates where ID_Obj = (select ID_Obj from Devices where ID_Device = ' + IntToStr(GetID_Device()) + ')'), 'ObjectStates');
end;

function TLogicaSPT943ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_SPT_943;
end;

procedure TLogicaSPT943ReqCreatorTest.SetUp;
begin
  inherited;
end;

procedure TLogicaSPT943ReqCreatorTest.TearDown;
begin
  inherited;
end;

{ TSPT943ReqCreatorFortest }

function TSPT943ReqCreatorFortest.CanRequestPack: bool;
begin
  Result := true;
end;

function TSPT943ReqCreatorFortest.NeedReqArch: int;
begin
  Result := -1;
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Logica', TLogicaSPT943ReqCreatorTest.Suite);
end.

