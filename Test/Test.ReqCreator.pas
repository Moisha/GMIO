unit Test.ReqCreator;

interface

uses Windows, TestFrameWork, GMGlobals, GM485, ArchIntervalBuilder,
     Generics.Collections, GMConst;

type
  ArrayOfCardinal = array of Cardinal;
  ArchIntervalBuilderForTest = class(TArchIntervalBuilder)
  private
    FArray: ArrayOfCardinal;
    FArrayIndex: int;
  protected
    function GetNextHole(): LongWord; override;
    function IsAllProcessed: bool; override;
    procedure OpenQuery(); override;
  public
    constructor Create(lst: ArrayOfCardinal);
  end;

  TReqCreatorTest = class(TTestCase)
  private
    builder: ArchIntervalBuilderForTest;
    lst: TList<ArchReqInterval>;
    arr: ArrayOfCardinal;
    procedure CheckInterval(ut: LongWord; dt: TDateTime; err: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ArchIntervalBuilder;
  end;

implementation

uses DateUtils, SysUtils;

{ TReqCreatorTest }

procedure TReqCreatorTest.CheckInterval(ut: LongWord; dt: TDateTime; err: string);
begin
  Check(ut = LocalToUTC(dt), err + ': ' + DateTimeToStr(UTCtoLocal(ut)) + ' <> ' + DateTimeToStr(dt));
end;

procedure TReqCreatorTest.ArchIntervalBuilder;
begin
  Check(lst.Count = 4, 'Count ' + IntToStr(lst.Count) + ' <> 4');

  CheckInterval(lst[0].UTime1, EncodeDate(2014, 03, 01), '1-1');
  CheckInterval(lst[0].UTime2, EncodeDate(2014, 03, 05), '1-2');

  CheckInterval(lst[1].UTime1, EncodeDate(2014, 03, 06), '2-1');
  CheckInterval(lst[1].UTime2, EncodeDate(2014, 03, 06), '2-2');

  CheckInterval(lst[2].UTime1, EncodeDate(2014, 03, 11), '3-1');
  CheckInterval(lst[2].UTime2, EncodeDate(2014, 03, 13), '3-2');

  CheckInterval(lst[3].UTime1, EncodeDate(2014, 03, 24), '4-1');
  CheckInterval(lst[3].UTime2, EncodeDate(2014, 03, 26), '4-2');
end;

procedure TReqCreatorTest.SetUp;
begin
  inherited;

  SetLength(arr, 12);
  arr[0] := LocalToUTC(EncodeDate(2014, 03, 01));
  arr[1] := LocalToUTC(EncodeDate(2014, 03, 02));
  arr[2] := LocalToUTC(EncodeDate(2014, 03, 03));
  arr[3] := LocalToUTC(EncodeDate(2014, 03, 04));
  arr[4] := LocalToUTC(EncodeDate(2014, 03, 05));
  arr[5] := LocalToUTC(EncodeDate(2014, 03, 06));

  arr[6] := LocalToUTC(EncodeDate(2014, 03, 11));
  arr[7] := LocalToUTC(EncodeDate(2014, 03, 12));
  arr[8] := LocalToUTC(EncodeDate(2014, 03, 13));

  arr[9] := LocalToUTC(EncodeDate(2014, 03, 24));
  arr[10] := LocalToUTC(EncodeDate(2014, 03, 25));
  arr[11] := LocalToUTC(EncodeDate(2014, 03, 26));

  builder := ArchIntervalBuilderForTest.Create(arr);
  lst := builder.BuildIntervalList(UTC_DAY, 5);
end;

procedure TReqCreatorTest.TearDown;
begin
  inherited;

  builder.Free();
  lst.Free();
end;

{ ArchIntervalBuilderForTest }

constructor ArchIntervalBuilderForTest.Create(lst: ArrayOfCardinal);
begin
  inherited Create(nil);
  FArrayIndex := 0;
  FArray := lst;
end;

function ArchIntervalBuilderForTest.GetNextHole: LongWord;
begin
  Result := FArray[FArrayIndex];
  inc(FArrayIndex);
end;

function ArchIntervalBuilderForTest.IsAllProcessed: bool;
begin
  Result := FArrayIndex > High(FArray);
end;

procedure ArchIntervalBuilderForTest.OpenQuery;
begin

end;

initialization
  RegisterTest('GMIOPSrv/Devices/Common', TReqCreatorTest.Suite);
end.


