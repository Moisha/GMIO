unit Test.Devices.Logica.SPT961.ReqCreator;

interface

uses TestFrameWork, Devices.Logica.SPT961, GMGlobals, Test.Devices.Base.ReqCreator;

type
  TSPT961ReqCreatorFortest = class(TSPT961ReqCreator)

  end;

  TLogicaSPT961ReqCreatorTest = class(TDeviceReqCreatorTestBase)
  private
    sptReqCreator: TSPT961ReqCreatorFortest;
    procedure CheckResultString(const errMsg, resString: string);
    procedure CleanArchives;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetDevType(): int; override;
    procedure DoCheckRequests(); override;
  published
    procedure testCommonPrm();
    procedure testIndexPrm();
    procedure testArchStructure();
    procedure testArchData();
    procedure testFromExtData();
    procedure testTimeArray;
    procedure CheckRequests(); override;
  end;

implementation

uses DateUtils, SysUtils, GMConst;

{ TLogicaReqCreatorTest }

procedure TLogicaSPT961ReqCreatorTest.CleanArchives();
var i: int;
begin
  for i := ReqList.Count - 1 downto 0 do
  begin
    if ReqList[i].Priority > rqprCurrents then
      ReqList.Delete(i);
  end;
end;

procedure TLogicaSPT961ReqCreatorTest.CheckRequests;
begin
  ReqCreator.AddDeviceToSendBuf();
  CleanArchives();
  Check(ReqList.Count >= 15); // архивы пока не дергаем

	CheckReqHexString(0, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $30, 9, $36, $33, $C, $10, 3, $22, $51');
	CheckReqHexString(1, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $30, 9, $36, $35, $C, $10, 3, 5, $C8');
	CheckReqHexString(2, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $30, 9, $36, $36, $C, $10, 3, $9E, $14');
	CheckReqHexString(3, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $31, 9, $31, $35, $35, $C, $10, 3, $98, $E8');
	CheckReqHexString(4, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $31, 9, $31, $35, $36, $C, $10, 3, 3, $34');
	CheckReqHexString(5, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $31, 9, $31, $35, $37, $C, $10, 3, $75, $80');
	CheckReqHexString(6, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $31, 9, $31, $37, $31, $C, $10, 3, $16, $9A');
	CheckReqHexString(7, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $32, 9, $31, $35, $35, $C, $10, 3, $50, $9D');
	CheckReqHexString(8, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $32, 9, $31, $35, $36, $C, $10, 3, $CB, $41');
	CheckReqHexString(9, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $32, 9, $31, $35, $37, $C, $10, 3, $BD, $F5');
	CheckReqHexString(10, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $32, 9, $31, $37, $31, $C, $10, 3, $DE, $EF');
	CheckReqHexString(11, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $31, 9, $31, $36, $30, $C, $10, 3, $CA, $7F');
	CheckReqHexString(12, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $31, 9, $31, $36, $33, $C, $10, 3, $51, $A3');
	CheckReqHexString(13, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $32, 9, $31, $36, $30, $C, $10, 3, 2, $A');
	CheckReqHexString(14, '$10, 1, $18, $18, $10, $1F, $1D, $10, 2, 9, $32, 9, $31, $36, $33, $C, $10, 3, $99, $D6');
end;

procedure TLogicaSPT961ReqCreatorTest.CheckResultString(const errMsg, resString: string);
var i, n: int;
    creo: string;
begin
  creo := ArrayToString(sptReqCreator.bufSend, sptReqCreator.LengthSend, false, true);
  Check(Length(creo) = Length(resString), errMsg + ' - length differs');

  n := 0;
  for i := 1 to Length(resString) do
  begin
    if creo[i] <> resString[i] then
    begin
      n := i;
      break;
    end;
  end;

  Check(n = 0, errMsg + ' - ErrorPos = ' + IntToStr(n));
end;

procedure TLogicaSPT961ReqCreatorTest.DoCheckRequests;
begin
  inherited;

end;

function TLogicaSPT961ReqCreatorTest.GetDevType: int;
begin
  Result := DEVTYPE_SPT_961;
end;

procedure TLogicaSPT961ReqCreatorTest.SetUp;
var reqdetails: TRequestDetails;
begin
  inherited;

  reqdetails.Init();
  sptReqCreator := TSPT961ReqCreatorFortest.Create(nil, reqdetails);
end;

procedure TLogicaSPT961ReqCreatorTest.TearDown;
begin
  inherited;
  sptReqCreator.Free();
end;

procedure TLogicaSPT961ReqCreatorTest.testArchData;
begin
  sptReqCreator.Chn := 0;
  sptReqCreator.archDT1 := EncodeDate(2013, 10, 14);
  sptReqCreator.ArchType := gmarchDay;
  sptReqCreator.ArchData();
  CheckResultString('ArchData', '10 01 00 00 10 1F 18 10 02 09 30 09 36 35 35 33 32 0C 09 31 34 09 31 30 09 32 30 31 33 09 30 09 30 09 30 0C 10 03 FB C6');
end;

procedure TLogicaSPT961ReqCreatorTest.testArchStructure;
begin
  sptReqCreator.Chn := 0;
  sptReqCreator.ArchType := gmarchDay;
  sptReqCreator.ArchStructure();
  CheckResultString('ArchStructure', '10 01 00 00 10 1F 19 10 02 09 30 09 36 35 35 33 32 0C 10 03 8E 0D');
end;

procedure TLogicaSPT961ReqCreatorTest.testCommonPrm;
begin
  sptReqCreator.Chn := 1;
  sptReqCreator.Prm := 160;
  sptReqCreator.CommonPrm();
  CheckResultString('CommonPrm', '10 01 00 00 10 1F 1D 10 02 09 31 09 31 36 30 0C 10 03 91 3F');
end;

procedure TLogicaSPT961ReqCreatorTest.testFromExtData;
begin
  Check(sptReqCreator.FromExtData('1 123'), 'correct Extdata');
  Check((sptReqCreator.Chn = 1) and (sptReqCreator.Prm = 123), 'correct Extdata result');

  Check(sptReqCreator.FromExtData(' 1  123 '), 'correct Extdata with extra spaces');
  Check((sptReqCreator.Chn = 1) and (sptReqCreator.Prm = 123), 'correct Extdata with extra spaces result');

  Check(sptReqCreator.FromExtData(' 1  123 586 ', rqtSPT961_DAY), 'correct Extdata Arch with extra spaces');
  Check((sptReqCreator.Chn = 1) and (sptReqCreator.Prm = 586), 'correct Extdata Arch with extra spaces result');

  Check(not sptReqCreator.FromExtData(' 0 '), 'Insufficient ExtData');

  Check(not sptReqCreator.FromExtData(''), 'Empty ExtData');

  Check(not sptReqCreator.FromExtData('0 de'), 'Incorrect ExtData 1');
  Check(not sptReqCreator.FromExtData('de 1'), 'Incorrect ExtData 2');
  Check(not sptReqCreator.FromExtData('de ab'), 'Incorrect ExtData both');

  Check(not sptReqCreator.FromExtData('-1 1'), 'Negative ExtData');
  Check(not sptReqCreator.FromExtData('1 -1'), 'Negative ExtData 2');
  Check(sptReqCreator.FromExtData('1 1 -1'), 'Negative ExtData 3');
  Check(not sptReqCreator.FromExtData('1 1 -1', rqtSPT961_DAY), 'Negative ExtData 4');

  Check(not sptReqCreator.FromExtData('1,1'), 'Comma Text');

  Check(sptReqCreator.FromExtData(' 00 002 0003 de'), ' Excessive ExtData');
  Check((sptReqCreator.Chn = 0) and (sptReqCreator.Prm = 2), 'Excessive Extdata result');

  Check(sptReqCreator.FromExtData(' 00 002 0003 de', rqtSPT961_DAY), 'Excessive ExtData Arch');
  Check((sptReqCreator.Chn = 0) and (sptReqCreator.Prm = 3), 'Excessive Extdata Arch result');

  //////

  Check(sptReqCreator.FromExtData(' 1  123 586 789', rqtSPT961_HOUR), 'correct Extdata Arch with extra spaces');
  Check((sptReqCreator.Chn = 1) and (sptReqCreator.Prm = 789), 'correct Extdata Arch with extra spaces result');
  Check(not sptReqCreator.FromExtData('1 1 1 -1', rqtSPT961_HOUR), 'Negative ExtData 4');
  Check(not sptReqCreator.FromExtData('1 1 1 dede', rqtSPT961_HOUR), 'Incorrect ExtData Hour');
  Check(sptReqCreator.FromExtData(' 00 002 0003 000004 de', rqtSPT961_HOUR), 'Excessive ExtData Arch');
  Check((sptReqCreator.Chn = 0) and (sptReqCreator.Prm = 4), 'Excessive Extdata Arch result');
end;

procedure TLogicaSPT961ReqCreatorTest.testIndexPrm;
begin
  sptReqCreator.Chn := 0;
  sptReqCreator.Prm := 96;
  sptReqCreator.IdxStart := 1;
  sptReqCreator.IdxCount := 9;
  sptReqCreator.IndexPrm();
  CheckResultString('IndexPrm', '10 01 00 00 10 1F 0C 10 02 09 30 09 39 36 09 31 09 39 0C 10 03 F6 F9');
end;

procedure TLogicaSPT961ReqCreatorTest.testTimeArray;
begin
  sptReqCreator.Chn := 0;
  sptReqCreator.Prm := 72;
  sptReqCreator.archDT1 := EncodeDate(2013, 10, 09);
  sptReqCreator.archDT2 := EncodeDate(2013, 10, 14);
  sptReqCreator.TimeArray();
  CheckResultString('TimeArray', '10 01 00 00 10 1F 0E 10 02 09 30 09 37 32 0C 09 31 34 09 31 30 09 32 30 31 33 09 30 09 30 09 30 0C 09 39 09 31 30 09 32 30 31 33 09 30 09 30 09 30 0C 10 03 A3 42');
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Logica', TLogicaSPT961ReqCreatorTest.Suite);
end.

