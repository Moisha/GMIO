unit Test.Devices.Base.ReqCreator;

interface

uses TestFrameWork, GMGlobals, GM485, GMGenerics, Generics.Collections, SysUtils, GMSqlQuery, ActiveX, GMConst,
     Devices.ReqCreatorBase;

type
  T485RequestCreatorForTest = class(T485RequestCreator)
  protected
    function GetDevReqCreatorClass(ID_DevType: int): TDevReqCreatorClass; override;
  end;

  TDeviceReqCreatorTestBase = class(TTestCase)
  private
    function GetReqString(Index: int): string;
  protected
    ReqCreator: T485RequestCreatorForTest;
    ReqList: TList<TRequestDetails>;

    procedure ArchRequest(ReqDetails: TRequestDetails);

    property ReqString[Index: int]: string read GetReqString;
    procedure CheckReqString(Index: int; const origin: string);
    procedure CheckReqHexString(Index: int; const origin: string);

    procedure ClearObjectStates;
    procedure SetUp(); override;
    procedure TearDown(); override;

    function GetDevType(): int; virtual; abstract;
    procedure DoCheckRequests(); virtual; abstract;
    function GetID_Device: int; virtual;
  published
    procedure CheckRequests(); virtual;
  end;

implementation

{ TDeviceReqCreatorTestBase }

uses Devices.Tecon, Test.Devices.Tecon19.ReqCreator, Test.Devices.Mercury.ReqCreator, Test.Devices.Logica.SPT941.ReqCreator,
     Test.Devices.Logica.SPT943.ReqCreator;

procedure TDeviceReqCreatorTestBase.ArchRequest(ReqDetails: TRequestDetails);
begin
  ReqList.Add(ReqDetails);
end;

procedure TDeviceReqCreatorTestBase.CheckReqString(Index: int; const origin: string);
begin
  Check(ReqString[Index] = origin, '[' + IntToStr(Index) + '] good = ' + origin + ' bad = ' + ReqString[Index]);
end;

procedure TDeviceReqCreatorTestBase.CheckRequests;
begin
  ClearObjectStates();
  ReqCreator.AddDeviceToSendBuf();
  DoCheckRequests();
end;

procedure TDeviceReqCreatorTestBase.CheckReqHexString(Index: int; const origin: string);
var bufOrigin: ArrayOfbyte;
    res: TRequestDetails;
    i: int;
begin
  bufOrigin := TextNumbersStringToArray(origin);
  res := ReqList[Index];
  Check(Length(bufOrigin) = res.BufCnt, IntToStr(Index) + ' Wrong length: actual ' + IntToStr(res.BufCnt) + ' expected ' + IntToStr(Length(bufOrigin)));
  for i := 0 to res.BufCnt - 1 do
    Check(res.buf[i] = bufOrigin[i],
     '[' + IntToStr(Index) + '] expected: ' + ArrayToString(bufOrigin, Length(bufOrigin), false, true) +
     #13#10'actual:   ' + ArrayToString(res.buf, Length(bufOrigin), false, true) + ', Wrong byte ' + IntToStr(i));
end;

function TDeviceReqCreatorTestBase.GetReqString(Index: int): string;
begin
  Result := StringArrayToString(ReqList[Index].BufToArray(), ReqList[Index].BufCnt);
end;

procedure TDeviceReqCreatorTestBase.ClearObjectStates();
begin
  ExecSQL('delete from ObjectStates');
end;

function TDeviceReqCreatorTestBase.GetID_Device(): int;
begin
  Result := 1000 + GetDevType();
end;

procedure TDeviceReqCreatorTestBase.SetUp;
begin
  inherited;

  ReqCreator := T485RequestCreatorForTest.Create(ArchRequest);

  ReqCreator.ReqDetails.DevNumber := GetDevType();
  ReqCreator.ReqDetails.ID_DevType := GetDevType();
  ReqCreator.ReqDetails.ID_Device := GetID_Device();
  ReqCreator.ReqDetails.ID_Obj := 3; // ID_Obj = 3 - объект для всех мыслимых типов приборов

  ReqList := TList<TRequestDetails>.Create();
end;

procedure TDeviceReqCreatorTestBase.TearDown;
begin
  ReqCreator.Free();
  ReqList.Free();
  inherited;
end;

{ T485RequestCreatorForTest }

function T485RequestCreatorForTest.GetDevReqCreatorClass(ID_DevType: int): TDevReqCreatorClass;
begin
  if ID_DevType in Tecon19_Family then
    Result := TTecon19ReqCreatorForTest
  else
  case ID_DevType of
    DEVTYPE_MERCURY_230: Result := TMercury230ReqCreatorForTest;
    DEVTYPE_SPT_941: Result := TSPT941ReqCreatorFortest;
    DEVTYPE_SPT_943: Result := TSPT943ReqCreatorFortest;
    else
      Result := inherited GetDevReqCreatorClass(ID_DevType);
  end;
end;

end.
