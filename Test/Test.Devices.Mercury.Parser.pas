unit Test.Devices.Mercury.Parser;

interface

uses TestFrameWork, GMGlobals, Classes, SysUtils, Devices.Mercury, Windows, Test.Devices.Base.ReqParser,
     GmSqlQuery, UsefulQueries, Threads.ResponceParser, GMConst, Math;

type
  TLocalSQLWriteThreadForTest = class(TResponceParserThreadForTest);
  TMercuryParserForTest = class(TMercuryParser);
  TMercury230ParserTest = class(TDeviceReqParserTestBase)
  private
    FParser: TMercuryParserForTest;
    thr: TLocalSQLWriteThreadForTest;

    procedure SetThreadIds;

    function Examples_MeterInfo: ArrayOfByte;
    function Examples_Readings: ArrayOfByte;
    function Examples_LastArchAddr: ArrayOfByte;
    function Examples_ArchiveRecord: ArrayOfByte;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetDevType(): int; override;
    function GetThreadClass(): TSQLWriteThreadForTestClass; override;
  published
    procedure CRC();

    procedure ParseReadings;
    procedure ParseMeterInfo;
    procedure ParseLastArchAddr();
    procedure ParseArchiveRecord();

    procedure Bin2Dec();
    procedure ArchDT();

    procedure Thread_MeterInfo;
    procedure Thread_Readings;
    procedure Thread_LastArchAddr;
    procedure Thread_ArchiveRecord;
  end;

implementation

procedure TMercury230ParserTest.ArchDT;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('31 FD 20 09 19 30 15 11 14 1E 7F BD');
  Check(FParser.SetLastArchAddrBuffer(buf, Length(buf)), 'buf');
  Check(FParser.ReadArchUDT(4) = EncodeDateTimeUTC(2014, 11, 15, 19, 00));
end;

procedure TMercury230ParserTest.Bin2Dec;
begin
  Check(FParser.BinToDec($01) = 1);
  Check(FParser.BinToDec($11) = 11);
  Check(FParser.BinToDec($59) = 59);
end;

procedure TMercury230ParserTest.CRC;
var buf: ArrayOfByte;
begin
  buf := TextNumbersStringToArray('31 00 20 00 40 00 27 00 57 00 38 00 38 00 38 00  38 18 A9');
  Check(Mercury_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray('31 00 20 00 40 00 27 00 57 00 38 00 38 00 38 00  38 18 A9 01');
  Check(not Mercury_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray('31 01 01 01 01 01 01 01 01 2E 10');
  Check(Mercury_CheckCRC(buf, Length(buf)));

  buf := TextNumbersStringToArray('49 5 212 35', false);
  Check(Mercury_CheckCRC(buf, Length(buf)));
end;

function TMercury230ParserTest.GetDevType: int;
begin
  Result := DEVTYPE_MERCURY_230;
end;

function TMercury230ParserTest.GetThreadClass: TSQLWriteThreadForTestClass;
begin
  Result := TLocalSQLWriteThreadForTest;
end;

function TMercury230ParserTest.Examples_MeterInfo(): ArrayOfByte;
begin
  Result := TextNumbersStringToArray('31 B4 E4 C2 91 07 00 3E 04');
end;

function TMercury230ParserTest.Examples_Readings(): ArrayOfByte;
begin
  Result := TextNumbersStringToArray('31 00 00 79 BD FF FF FF FF 00 00 61 02 FF FF FF FF 84 0A');
end;

function TMercury230ParserTest.Examples_LastArchAddr(): ArrayOfByte;
begin
  Result := TextNumbersStringToArray('31 FD 20 09 19 30 15 11 14 1E 7F BD');
end;

function TMercury230ParserTest.Examples_ArchiveRecord(): ArrayOfByte;
begin
  Result := TextNumbersStringToArray('31 01 21 00 09 11 14 1E 3E 00 FF FF 00 00 FF FF 85 A6');
end;

procedure TMercury230ParserTest.ParseArchiveRecord;
var buf: ArrayOfByte;
begin
  buf := Examples_ArchiveRecord();
  Check(FParser.SetArchiveRecordBuffer(buf, Length(buf)), 'buf');
  Check(FParser.ReadArchiveRecordUDT() = EncodeDateTimeUTC(2014, 11, 09, 20, 30));
  Check(CompareValue(FParser.ReadArchiveRecordChannelImpulses(TMercuryReadingsChannel.mrcAin), 62) = 0);
  Check(CompareValue(FParser.ReadArchiveRecordChannelImpulses(TMercuryReadingsChannel.mrcQin), 0) = 0);
end;

procedure TMercury230ParserTest.ParseLastArchAddr;
var buf: ArrayOfByte;
begin
  buf := Examples_LastArchAddr();
  Check(FParser.SetLastArchAddrBuffer(buf, Length(buf)), 'buf');
  Check(FParser.ReadLastArchAddr() = $FD20, 'ReadLastArchAddr');
  Check(FParser.ReadLastArchUDT() = EncodeDateTimeUTC(2014, 11, 15, 19, 00, 00));
end;

procedure TMercury230ParserTest.ParseMeterInfo;
var buf: ArrayOfByte;
begin
  buf := Examples_MeterInfo();
  Check(FParser.SetMeterInfoBuffer(buf, Length(buf)));
  Check(FParser.ReadImpulsePerKWt() = 1000);
end;

procedure TMercury230ParserTest.ParseReadings;
var buf: ArrayOfByte;
begin
  buf := Examples_Readings();

  Check(FParser.SetReadingsBuffer(buf, Length(buf)));
  Check(FParser.CheckReadingChannel(TMercuryReadingsChannel.mrcAin));
  Check(not FParser.CheckReadingChannel(TMercuryReadingsChannel.mrcAout));
  Check(FParser.CheckReadingChannel(TMercuryReadingsChannel.mrcQin));
  Check(not FParser.CheckReadingChannel(TMercuryReadingsChannel.mrcQout));

  Check(CompareValue(FParser.ReadingsChannelImpulses(TMercuryReadingsChannel.mrcAin), 48505) = 0);
  Check(CompareValue(FParser.ReadingsChannelImpulses(TMercuryReadingsChannel.mrcQin), 609) = 0);
end;

procedure TMercury230ParserTest.SetUp;
begin
  inherited;
  FParser := TMercuryParserForTest.Create();
  ExecSQL('delete from DevStates');
  thr := TLocalSQLWriteThreadForTest(thread);
end;

procedure TMercury230ParserTest.TearDown;
begin
  FParser.Free();
  inherited;
end;

procedure TMercury230ParserTest.SetThreadIds();
begin
  thr.ChannelIds.ID_DevType := DEVTYPE_MERCURY_230;
  thr.ChannelIds.ID_Src := SRC_CNT_MTR;
  thr.ChannelIds.N_Src := 1;
  thr.ChannelIds.ID_Device := 1;
  thr.ChannelIds.NDevNumber := 49;
end;

procedure TMercury230ParserTest.Thread_ArchiveRecord;
begin
  gbv.SetBufRec(Examples_ArchiveRecord());
  gbv.ReqDetails.rqtp := rqtMercury230_Archive;

  SetThreadIds();
  SQLReq_SetDevState(thr.ChannelIds.ID_Device, DEV_STATE_NAMES_IMP_PER_KWT, '1000'); // потом надо бы не корежить Ѕƒ, а вынести в ...ForTest

  thr.ChannelIds.N_Src := 1;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(thr.Values) = 1);
  Check(CompareValue(thr.Values[0].Val, 0.031) = 0);
  Check(thr.Values[0].UTime = EncodeDateTimeUTC(2014, 11, 09, 20, 30));

  thr.ChannelIds.N_Src := 2;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresUnknown);

  thr.ChannelIds.N_Src := 3;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(thr.Values) = 1);
  Check(CompareValue(thr.Values[0].Val, 0) = 0);
  Check(thr.Values[0].UTime = EncodeDateTimeUTC(2014, 11, 09, 20, 30));

  thr.ChannelIds.N_Src := 4;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresUnknown);
end;

procedure TMercury230ParserTest.Thread_LastArchAddr;
var s: string;
begin
  gbv.ReqDetails.rqtp := rqtMercury230_LastArchAddr;
  gbv.SetBufRec(Examples_LastArchAddr());

  SetThreadIds();
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresSupport); // вспомогательный запрос не несет в себе данных

  s := SQLReq_GetDevState(thr.ChannelIds.ID_Device, DEV_STATE_NAMES_LAST_ARCH_ADDR);
  Check(StrToInt(s) = $FD20);

  s := SQLReq_GetDevState(thr.ChannelIds.ID_Device, DEV_STATE_NAMES_LAST_ARCH_UDT);
  Check(StrToInt(s) = int64(EncodedateTimeUTC(2014, 11, 15, 19, 00)));
end;

procedure TMercury230ParserTest.Thread_MeterInfo;
begin
  gbv.ReqDetails.rqtp := rqtMercury230_MeterInfo;
  gbv.SetBufRec(Examples_MeterInfo());

  SetThreadIds();
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresSupport); // вспомогательный запрос не несет в себе данных
  Check(SQLReq_GetDevState(thr.ChannelIds.ID_Device, DEV_STATE_NAMES_IMP_PER_KWT) = '1000');
end;

procedure TMercury230ParserTest.Thread_Readings;
begin
  gbv.SetBufRec(Examples_Readings());
  gbv.ReqDetails.rqtp := rqtMercury230_Currents;

  SetThreadIds();
  SQLReq_SetDevState(thr.ChannelIds.ID_Device, DEV_STATE_NAMES_IMP_PER_KWT, '1000'); // потом надо бы не корежить Ѕƒ, а вынести в ...ForTest

  thr.ChannelIds.N_Src := 1;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(thr.Values) = 1);
  Check(CompareValue(thr.Values[0].Val, 48.505) = 0);
  Check(thr.Values[0].UTime = gbv.gmTime);

  thr.ChannelIds.N_Src := 2;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresUnknown);

  thr.ChannelIds.N_Src := 3;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresData);
  Check(Length(thr.Values) = 1);
  Check(CompareValue(thr.Values[0].Val, 0.609) = 0);
  Check(thr.Values[0].UTime = gbv.gmTime);

  thr.ChannelIds.N_Src := 4;
  Check(thr.RecognizeAndCheckChannel(gbv) = recchnresUnknown);
end;

initialization
  RegisterTest('GMIOPSrv/Devices/Mercury230', TMercury230ParserTest.Suite);
end.
