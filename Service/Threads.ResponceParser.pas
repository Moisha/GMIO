////////////////////////////////////////////
// Классы для разбора и записи в SQL ответов от приборов
////////////////////////////////////////////
unit Threads.ResponceParser;

interface

uses Classes, Windows, GMConst, GMGlobals, GMBlockValues, SysUtils, Devices.UBZ,
     GeomerLastValue, GMSqlQuery, Threads.Base, Generics.Collections, Threads.SQLExecuter;

type
  TRecognizeChannelResult = (recchnresUnknown, recchnresSupport, recchnresData, recchnresEmpty);

  TResponceParserThread = class(TGMThread)
  private
    FSQLWriter: TSQLExecuteThread;

    procedure ParseReqDetails(gbv: TGeomerBlockValues);
    function CheckSupportRequest(gbv: TGeomerBlockValues): bool;
    function CheckMercury230SupportChannel(gbv: TGeomerBlockValues): bool;
    function CheckAltistart22Channel(gbv: TGeomerBlockValues): bool;
    function CheckGeostreamChannel(gbv: TGeomerBlockValues): bool;
    function CheckSPT941Channel(gbv: TGeomerBlockValues): bool;
    function CheckOnePrmOnly(devType: int): string;
    function CheckGeomerChannel_AI(gbv: TGeomerBlockValues): bool;
    function CheckGeomerChannel_AI_ISCO(gbv: TGeomerBlockValues): bool;
    function CheckGeomerChannel_AI_UBZ(gbv: TGeomerBlockValues): bool;
    function CheckModbusRTUUserDefinedChannel(gbv: TGeomerBlockValues): bool;
    procedure ModbusReadAI(gbv: TGeomerBlockValues; posDataStart: int);
    procedure ModbusReadDI(gbv: TGeomerBlockValues; posDataStart: int);
    function ModbusDIDataLength(valueCount: int): int;
    function ModbusRTUDataStart(gbv: TGeomerBlockValues): int;
    function ModbusRTUDevNumber(gbv: TGeomerBlockValues): int;
    function ModbusRTUFunction(gbv: TGeomerBlockValues): int;
    function ModbusReadAI_Data(gbv: TGeomerBlockValues; posDataStart, dataIndex: int; var data: double): bool;
    function CheckUBZChannel_CheckGBV(gbv: TGeomerBlockValues): bool;
    function CheckDRKChannel(gbv: TGeomerBlockValues): bool;
    function CheckDRKChannel_Val(gbv: TGeomerBlockValues; pos: int): double;
    function CheckDRKChannel_Val_Digit(src: byte): byte;
    function CheckADCPChannelMasterChannel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
    function CheckStreamlux700fChannel(gbv: TGeomerBlockValues): bool;
    function CheckStreamlux700fChannel_ErrorCode(resp: AnsiString): bool;
    function CheckStreamlux700fChannel_SignalQuality(resp: AnsiString): bool;
    function CheckStreamlux700fChannel_CommonVal(gbv: TGeomerBlockValues): bool;
  protected
    ChannelIDs: TChannelIds;

    Values: array of TValueFromBase;

    lBlocks: TList<TGeomerBlockValues>;
    qRead: TGMSqlQuery;
    synch: TMultiReadExclusiveWriteSynchronizer;
    slDecode: TStringList;

    q: TGMSqlQuery;
    lProcessedAIList, lProcessedCNTList: TList;

    glvBuffer: TGeomerLastValuesBuffer;

    function IsAppropriateModbusTCPAnswer(gbv: TGeomerBlockValues; desiredWordCnt: int = 0): bool;
    function IsAppropriateModbusRTUAnswer(gbv: TGeomerBlockValues; desiredWordCnt: int = 0): bool;

    function CheckGeomerChannel(gbv: TGeomerBlockValues): bool;
    function CheckICP_AIChannel(gbv: TGeomerBlockValues): bool;
    function CheckICP_DIChannel(gbv: TGeomerBlockValues): bool;
    function CheckTRM138Channel(gbv: TGeomerBlockValues): bool;
    function CheckUBZChannel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
    procedure ProcessUBZDataBlock(gbv: TGeomerBlockValues);
    function RecognizeAndCheckChannel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
    function CheckUBZAlarm(ubz: TGeomerLastValue; gbv: TGeomerBlockValues): bool;
    function CheckVzletUrsvChannel(gbv: TGeomerBlockValues): bool;
    function CheckVaconNXLChannel(gbv: TGeomerBlockValues): bool;
    function CheckPeterFlowRSChannel(gbv: TGeomerBlockValues): bool;
    function CheckMercury230Channel(gbv: TGeomerBlockValues): bool;
    procedure DelProcessedGBV(g: TGeomerBlockValues);
    procedure WriteVals(const sSQL: string);
    procedure InitQueries;
    function NextGBVToProcess: TGeomerBlockValues;
    procedure ProcessValuesList();
    procedure ProcessSumsForAI;
    function GetPrmQueryTxt(ReqDetails: TRequestDetails): string;
    function CheckTecon19Channel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
    function CheckVaconIsco4250Channel(gbv: TGeomerBlockValues): bool;
    procedure ProcessMtrForCNT;
    procedure ProcessExtendedSummChn(ID_Prm, ExtType: int);
    function CheckSPT961Channel(gbv: TGeomerBlockValues): bool;
    function SPT961_ProcessArchive(gbv: TGeomerBlockValues;  Archive: TValuesCollection): bool;
    function CheckSimag11Channel(gbv: TGeomerBlockValues): bool;
    function CheckModbusTCPUserDefinedChannel(gbv: TGeomerBlockValues): bool;
    function CheckMainSrvUniversalResponce(gbv: TGeomerBlockValues): bool;
    procedure ParseReqDetails_SPT(gbv: TGeomerBlockValues);
    procedure SafeExecute(); override;
    procedure TerminatedSet; override;
    function GetUBZ(ID_Obj, NAddr: int): TGeomerLastValue; virtual;
  public
    procedure Add(gbv: TGeomerBlockValues);
    constructor Create(AglvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy(); override;
    function Queue(): int;
    function WaitForTimeout(TimeOut: int; KillUnterminated: bool = true): int; override;
  end;

  function IfThen(condition: bool; valTrue, valFalse: TRecognizeChannelResult): TRecognizeChannelResult; overload;

implementation

uses Devices.Owen, Math, StrUtils, DateUtils, Devices.Tecon, Devices.Vzlet,
     Devices.Vacon, ActiveX, Devices.Logica.SPT961, ProgramLogFile, UsefulQueries, Devices.Termotronic, Devices.Mercury,
     Devices.ModbusBase, Devices.Altistart22, Devices.Geostream, Devices.MainSrv, Devices.Logica.SPT941,
     Devices.Logica.SPT943, System.Variants, System.TypInfo, EsLogging;

function IfThen(condition: bool; valTrue, valFalse: TRecognizeChannelResult): TRecognizeChannelResult;
begin
  if condition then
    Result := valTrue
  else
    Result := valFalse;
end;

{ TResponceParserThread }

procedure TResponceParserThread.Add(gbv: TGeomerBlockValues);
begin
  synch.BeginWrite();
  try
    lBlocks.Add(gbv);
    ProgramLog.AddMessage('Add gbv');
  finally
    synch.EndWrite();
  end;
end;

constructor TResponceParserThread.Create(AglvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create(false);

  glvBuffer := AglvBuffer;
  lBlocks := TList<TGeomerBlockValues>.Create();

  InitQueries();

  synch := TMultiReadExclusiveWriteSynchronizer.Create();
  slDecode := TStringList.Create();

  lProcessedAIList := TList.Create();
  lProcessedCNTList := TList.Create();

  FSQLWriter := TSQLExecuteThread.Create();
end;

destructor TResponceParserThread.Destroy;
begin
  lBlocks.Free();
  q.Free();
  qRead.Free();
  synch.Free();
  slDecode.Free();
  lProcessedAIList.Free();
  lProcessedCNTList.Free();
  FSQLWriter.Free();

  inherited;
end;

const HexDigits: TSysCharSet = ['0'..'9', 'A'..'F'];

function TResponceParserThread.CheckGeomerChannel_AI_ISCO(gbv: TGeomerBlockValues): bool;
begin
  Result := false;

  case ChannelIds.N_Src of
     10:
       if gbv.gbt in [gbtLongISCO] then
       begin
         Values[0].Val := gbv.LE;
         Result := gbv.iscoTime > 0;
       end;

     11:
       if gbv.gbt in [gbtLongISCO] then
       begin
         Values[0].Val := gbv.VE;
         Result := gbv.iscoTime > 0;
       end;

     12:
       if gbv.gbt in [gbtLongISCO] then
       begin
         Values[0].Val := gbv.FL;
         Result := gbv.iscoTime > 0;
       end;
  end;
end;

function TResponceParserThread.CheckGeomerChannel_AI_UBZ(gbv: TGeomerBlockValues): bool;
var
  nUBZ, nChn: int;
begin
  Result := false;

  if ChannelIds.N_Src = 100 then
  begin
    Values[0].Val := gbv.moneyLeft;
    Result := true;
    Exit;
  end;

  nUBZ := (ChannelIds.N_Src - 100) div 10;
  if (gbv.ubzCount <= nUBZ) or (gbv.ubzList[nUBZ].CommStatus <> 1) then Exit;

  nChn := ChannelIds.N_Src mod 10;

  case nChn of
    1:
       begin
         Values[0].Val := gbv.ubzList[nUBZ].Current;
         Result := true;
       end;

    2:
       begin
         Values[0].Val := gbv.ubzList[nUBZ].Voltage;
         Result := true;
       end;

    3:
       begin
         Values[0].Val := gbv.ubzList[nUBZ].Power;
         Result := true;
       end;

    4:
       begin
         Values[0].Val := gbv.ubzList[nUBZ].AlarmState;
         Result := true;
       end;
  end;
end;

function TResponceParserThread.CheckGeomerChannel_AI(gbv: TGeomerBlockValues): bool;
begin
  Result := false;

  case ChannelIds.N_Src of

     0..High(gbv.AI):
       begin
         Values[0].Val := gbv.AI[ChannelIds.N_Src];
         Result := true;
       end;

     10..12:
       Result := CheckGeomerChannel_AI_ISCO(gbv);

     100..200:
       Result := CheckGeomerChannel_AI_UBZ(gbv);
   end;
end;

function TResponceParserThread.CheckGeomerChannel(gbv: TGeomerBlockValues): bool;
begin
  Result := false;
  if not (ChannelIds.ID_DevType in GeomerFamily) or (ChannelIds.N_Src < 0) then Exit;

  case ChannelIds.ID_Src of
    SRC_AI:
      Result := CheckGeomerChannel_AI(gbv);

    SRC_DI:  if ChannelIds.N_Src <= 7 then
             begin
               Values[0].Val := gbv.DI[ChannelIds.N_Src];
               Result := true;
             end;

    SRC_CNT_DI:
             begin
               if (gbv.gbt in [gbtLong, gbtLongISCO]) and (ChannelIds.N_Src > 0) and (ChannelIds.N_Src <= High(gbv.DIC)) then
               begin
                // В Геомере 65535 на счетчике - это как бы 0
                // Эта цифра случается после включения питания и до прихода первого импульса
                // считаем его нулем
                 Values[0].Val := gbv.DIC[ChannelIds.N_Src];
                 if Values[0].Val = 65535 then
                   Values[0].Val := 0;

                 Result := true;
               end;

               if (gbv.gbt in [gbtLongISCO]) and (ChannelIds.N_Src = 10) then
               begin
                 Values[0].Val := gbv.VO;
                 Result := gbv.iscoTime > 0;
               end;
             end;

    SRC_DO:  if ChannelIds.N_Src <= 7 then
             begin
               Values[0].Val := gbv.DOut[ChannelIds.N_Src];
               Result := true;
             end;
  end;
end;

function TResponceParserThread.CheckICP_AIChannel(gbv: TGeomerBlockValues): bool;
var i, c: int;
    s, s1: AnsiString;
    val: double;
begin
  Result := false;
  s := gbv.BufRecString;

  if (ChannelIds.ID_Src <> SRC_AI) or (gbv.ReqDetails.rqtp <> rqtICP7017) or (Length(s) <= 2) then Exit;

  if s[1]='>' then
  begin
    s1:='';
    for i:=2 to Length(s) do
    begin
      if s[i] ='-' then s1:=s1+',-'
      else
      if s[i]='+' then s1:=s1+','
      else
      s1:=s1+s[i];
    end;

    slDecode.CommaText := string(s1);
    for i:=slDecode.Count-1 downto 0 do
      if slDecode[i]='' then slDecode.Delete(i);

    if ChannelIds.N_Src < slDecode.Count then
    begin
      System.Val(slDecode[ChannelIds.N_Src], val, c);
      Result := (c = 0);
      Values[0].Val := val;
    end;
  end;
end;

function TResponceParserThread.CheckPeterFlowRSChannel(gbv: TGeomerBlockValues): bool;
var parser: TTermotronicParser;
begin
  Result := false;
  if gbv.ReqDetails.rqtp <> rqtPeterFlowRS_Currents then Exit;

  parser := TTermotronicParser.Create();
  try
    if not parser.SetBuffer(gbv.gmBufRec, gbv.gmLenRec) then Exit;
    if (parser.DataLength < 28) or (parser.Msg[0] <> ChannelIds.NDevNumber) then Exit; // данных маловато

    case ChannelIds.ID_Src of
      SRC_AI:
        case ChannelIds.N_Src of
          1: // расход
            begin
              Values[0].Val := parser.ReadRegisterSingle(100);
              Result := true;
            end;
        end;
      SRC_CNT_MTR:
        case ChannelIds.N_Src of
          1: // V+
            begin
              Values[0].Val := parser.ReadRegisterDouble(104);
              Result := true;
            end;
          2: // V-
            begin
              Values[0].Val := parser.ReadRegisterDouble(108);
              Result := true;
            end;
          3: // UBZ_ENG_HOURS
            begin
              Values[0].Val := parser.ReadRegisterULong(112) div 60; // оно в минутах, переведем в часы
              Result := true;
            end;
        end;
    end;
  finally
    parser.Free();
  end;
end;

function TResponceParserThread.CheckSimag11Channel(gbv: TGeomerBlockValues): bool;
var i, c, n: int;
    s, s1: AnsiString;
    val, coeff: double;
    bChnOk, bStartFound: bool;
begin
  Result := false;
  s := gbv.BufRecString;

  bChnOk := ((ChannelIds.ID_Src = SRC_AI) and (ChannelIds.N_Src = 1)) or ((ChannelIds.ID_Src = SRC_CNT_MTR) and (ChannelIds.N_Src in [1, 2]));
  if not bChnOk or (gbv.ReqDetails.rqtp <> rqtSimag11) or (Length(s) <= 2) then Exit;

  s1 := '';
  bStartFound := false;
  for i := 2 to Length(s) do
  begin
    if not bStartFound then // в начале битых посылок может приходить мусор, пропустим его
    begin
    if s[i] in ['+', '-', '0'..'9'] then
      bStartFound := true
    else
      continue;
    end;

    if s[i] = '-' then
      s1 := s1 + ',-'
    else if s[i] = '+' then
      s1 := s1 + ','
    else if s[i] <> ';' then
      s1 := s1 + s[i];
  end;

  slDecode.CommaText := string(s1);
  for i := slDecode.Count - 1 downto 0 do
  begin
    if slDecode[i] = '' then
      slDecode.Delete(i);
  end;

  if (ChannelIds.ID_Src = SRC_CNT_MTR) and (slDecode.Count >= ChannelIds.N_Src) then
  begin
    coeff := 1;
    n := ChannelIds.N_Src - 1
  end
  else
  if (ChannelIds.ID_Src = SRC_AI) and (ChannelIds.N_Src = 1) and (slDecode.Count >= 3) then
  begin
    coeff := 3600;
    n := 2;
  end
  else
    Exit;

  System.Val(slDecode[n], val, c);
  if c > 0 then
    System.Val(StringReplace(slDecode[n], '.', FormatSettings.DecimalSeparator, []), val, c);

  if c = 0 then
  begin
    Result := true;
    Values[0].Val := val * coeff;
  end;
end;

function TResponceParserThread.CheckICP_DIChannel(gbv: TGeomerBlockValues): bool;
var w, mask: WORD;
    s: string;
    v, c, NChn: int;
begin
  Result := false;
  s := string(gbv.BufRecString);

  if gbv.ReqDetails.rqtp = rqtICP7041D_ALLDI then
  begin // текущие DI всем колхозом
    if ChannelIds.ID_Src <> SRC_DI then Exit;
    if s[1] <> '>' then Exit; // '>' маркер начала правильного ответа
    Delete(s, 1, 1);

    if (Length(s) >= 4)
       and CharInSet(s[1], HexDigits)
       and CharInSet(s[2], HexDigits)
       and CharInSet(s[3], HexDigits)
       and CharInSet(s[4], HexDigits) then
    begin
      w := StrToIntDef('$' + Copy(s, 1, 4), $FFFF);
      if (ChannelIds.N_Src >= 0) and (ChannelIds.N_Src < 16) then
      begin
        mask := 1 shl ChannelIds.N_Src;
        // в 7041D инверсные DI
        // в разомкнутом состоянии - 1, в замкнутом - 0
        if (w and mask) > 0 then
          Values[0].Val := 0
        else
          Values[0].Val := 1;

        Result := true;
      end;
    end;
  end
  else
  if gbv.ReqDetails.rqtp in [rqtICP7041D_CNT0..rqtICP7041D_CNT13] then
  begin // накопительные CNT_DI
    if ChannelIds.ID_Src <> SRC_CNT_DI then Exit;

    // формат правильного ответа: ! NN XXXXX
    // '!' - маркер начала правильного ответа
    // NN - HEX-номер прибоора на линии
    // XXXXX - DEС значение счетчика 0..65535

    if Length(s) < 8 then Exit;
    if s[1] <> '!' then Exit;

    NChn := ord(gbv.ReqDetails.rqtp) - ord(rqtICP7041D_CNT0);
    if NChn <> ChannelIds.N_Src then Exit;

    if StrToIntDef('$' + Copy(s, 2, 2), -1) <> ChannelIds.NDevNumber then Exit;

    Val(Copy(s, 4, 5), v, c);
    if c = 0 then
    begin
      Values[0].Val := v;
      Result := true;
    end;
  end;
end;

function TResponceParserThread.CheckMercury230Channel(gbv: TGeomerBlockValues): bool;
var parser: TMercuryParser;
    coeff: int;
    chn: TMercuryReadingsChannel;
begin
  Result := false;

  if ChannelIds.ID_Src <> SRC_CNT_MTR then Exit;

  case ChannelIds.N_Src of
    1: chn := TMercuryReadingsChannel.mrcAin; // A+
    2: chn := TMercuryReadingsChannel.mrcAout; // A-
    3: chn := TMercuryReadingsChannel.mrcQin; // Q+
    4: chn := TMercuryReadingsChannel.mrcQout; // Q-
    else Exit;
  end;

  parser := TMercuryParser.Create();
  try
  try
    if (gbv.gmLenRec < 1) or (gbv.gmBufRec[0] <> ChannelIds.NDevNumber) then Exit;

    if (gbv.ReqDetails.rqtp = rqtMercury230_Currents) and parser.SetReadingsBuffer(gbv.gmBufRec, gbv.gmLenRec) then
    begin
      Result := parser.CheckReadingChannel(chn);
      if Result then
        Values[0].Val := parser.ReadingsChannelImpulses(chn) / 1000; // коэффициент всегда 1000
    end;

    if (gbv.ReqDetails.rqtp = rqtMercury230_Archive) and parser.SetArchiveRecordBuffer(gbv.gmBufRec, gbv.gmLenRec) then
    begin
      Result := parser.CheckArchiveRecordChannel(chn);
      if Result then
      begin
        // Меркурий хранит архивы средних мощностей, по факту - потребление за час.
        // Для получения энергии на получасовке их надо поделить пополам.
        Values[0].Val := parser.ReadArchiveRecordChannelImpulses(chn) / 2.0;
        Values[0].UTime := parser.ReadArchiveRecordUDT();
        Values[0].ValType := valueTypeHourArch;

        Result := Values[0].Utime > 0;

        if Result then
        begin
          coeff := StrToIntDef(SQLReq_GetDevState(ChannelIds.ID_Device, DEV_STATE_NAMES_IMP_PER_KWT), -1);
          Result := coeff > 0;
          if Result then
            Values[0].Val := Values[0].Val / coeff;
        end;
      end;
    end;
  finally
    parser.Free();
  end;
  except
    on e: Exception do
      ProgramLog().AddError('CheckMercury230Channel - ' + e.Message + '(' + ArrayToString(gbv.gmBufRec, gbv.gmLenRec, true, true) + ') ' + ChannelIDs.LogIDs());
  end;
end;

function TResponceParserThread.CheckTRM138Channel(gbv: TGeomerBlockValues): bool;
var s: AnsiString;
    buf: array[0..100] of byte;
begin
  Result := false;

  if not (gbv.ReqDetails.rqtp in [rqtTRM138_T1..rqtTRM138_T8])
     or (ChannelIds.ID_Src <> SRC_AI)
     or (ChannelIds.N_Src <> (ord(gbv.ReqDetails.rqtp) - ord(rqtTRM138_T1) + 1) ) then Exit;

  s := gbv.BufRecString;
  if not OwenCheckPack(s) then Exit;

  OwenDecodeString(s, buf);
  if not OwenCheckCRC(buf, (Length(s) - 2) div 2) then Exit;

  OwenDecodeString(s, buf);
  Values[0].Val := ReadSingleInv(buf, 4);
  Result := true;
end;

function TResponceParserThread.CheckUBZAlarm(ubz: TGeomerLastValue; gbv: TGeomerBlockValues): bool;
var i: int;
begin
  Result := (ChannelIds.ID_Src = SRC_DI) and (ChannelIds.N_Src = 1);
  if Result then //Авария УБЗ
  begin
    // расшифровку авраии напишем, только если сама авария была
    Values[0].Val := IfThen((ubz.wState and 1) > 0, ubz.dwAlarms, 0);

    if gbv.ReqDetails.rqtp in [rqtUBZ_ALARM_LOG1, rqtUBZ_ALARM_LOG2] then
    begin
      for i := 0 to ubz.clUBZEvtLog.Count - 1 do
        if not TUBZEVTLogEntry(ubz.clUBZEvtLog.Items[i]).bProcessed then
        begin
          SetLength(Values, Length(Values) + 1);
          Values[High(Values)].Val := TUBZEVTLogEntry(ubz.clUBZEvtLog.Items[i]).State;
          Values[High(Values)].UTime := TUBZEVTLogEntry(ubz.clUBZEvtLog.Items[i]).uDT;
          Values[High(Values)].ValType := valueTypeCurrent;
          Values[High(Values)].Chn := nil;
        end;
    end;
  end;
end;

function TResponceParserThread.CheckTecon19Channel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
var parser: TTecon19AnswerParser;
    i: int;
begin
  // при запросе ТЭКОНов у нас всегда точно известен ID_Prm
  Result := recchnresUnknown;
  if not (gbv.ReqDetails.rqtp in [rqtUSER_DEFINED, rqtTECON_HourArch])
     or (gbv.ReqDetails.ID_Prm <> ChannelIds.ID_Prm) then Exit;

  parser := TTecon19AnswerParser.Create();
  try
    if not parser.Parse(gbv.gmBufRec, gbv.gmLenRec, gbv.ReqDetails) then Exit;

    Result := recchnresEmpty;
    if parser.Vals.Count > 0 then
    begin
      Result := recchnresData;
      SetLength(Values, parser.Vals.Count);
      for i := 0 to parser.Vals.Count - 1 do
        Values[i] := parser.Vals[i];
    end;
  finally
    parser.Free();
  end;
end;

function TResponceParserThread.CheckUBZChannel_CheckGBV(gbv: TGeomerBlockValues): bool;
var
  NAddr: int;
begin
  Result := false;

  if not (gbv.ReqDetails.rqtp in [rqtUBZ_ALARMS..rqtTR101_SP4]) then
  begin
    ProgramLog.AddMessage('CheckUBZChannel - bad request type ' + IntToStr(ord(gbv.ReqDetails.rqtp)));
    Exit;
  end;

  if not IsAppropriateModbusRTUAnswer(gbv) then
  begin
    // протоколирование внутри IsAppropriateModbusRTUAnswer
    Exit;
  end;

  if ModbusRTUFunction(gbv) <> 3 then
  begin
    ProgramLog.AddMessage('CheckUBZChannel - ModbusRTUFunction failed: ' + IntToStr(ModbusRTUFunction(gbv)));
    Exit;
  end;

  NAddr := ModbusRTUDevNumber(gbv);
  if ChannelIds.NDevNumber <> NAddr then // нужный УБЗ
  begin
    ProgramLog.AddMessage('CheckUBZChannel - UBZ addr failed: ' + IntToStr(NAddr) + ' instead of ' + IntToStr(ChannelIds.NDevNumber));
    Exit;
  end;

  Result := true;
end;

function TResponceParserThread.CheckUBZChannel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
var NAddr: Byte;
    ubz: TGeomerLastValue;

procedure ProcessAO(n: int);
begin
  if (ChannelIds.ID_Src = SRC_AO) and (ChannelIds.N_Src = n) then
  begin
    Result := recchnresData;
    Values[0].Val := ubz.SP[n];
  end;
end;

begin
  Result := recchnresUnknown;
  if not CheckUBZChannel_CheckGBV(gbv) then Exit;

  NAddr := ModbusRTUDevNumber(gbv);
  ubz := GetUBZ(ChannelIds.ID_Obj, NAddr);
  case ubz.ID_DevType of
    DEVTYPE_UBZ:
      case gbv.ReqDetails.rqtp of
        rqtUBZ_ALARMS, rqtUBZ_ALARM_LOG1, rqtUBZ_ALARM_LOG2: // состояние
          Result := IfThen(CheckUBZAlarm(ubz, gbv), recchnresData, recchnresUnknown);

        rqtUBZ_TRANSFORMATOR:
          Result := recchnresSupport;

        rqtUBZ_I: // токи
          if ChannelIds.ID_Src = SRC_AI then
          begin
            if ChannelIds.N_Src in [4, 5, 6] then
            begin
              Result := recchnresData;
              Values[0].Val := ubz.I[ChannelIds.N_Src - 3];
            end
            else
            if ChannelIds.N_Src = 9 then
            begin
              Result := recchnresData;
              Values[0].Val := ubz.I0;
            end
          end;

        rqtUBZ_U: // напряжения
          if ChannelIds.ID_Src = SRC_AI then
          begin
            Result := IfThen(ChannelIds.N_Src in [1, 2, 3], recchnresData, recchnresUnknown);
            if Result = recchnresData then
              Values[0].Val := ubz.U[ChannelIds.N_Src];
          end;

        rqtUBZ_A: // наработка
          if ChannelIds.ID_Src = SRC_AI then
          begin
            Result := IfThen(ChannelIds.N_Src = 7, recchnresData, recchnresUnknown);
            if Result = recchnresData then
              Values[0].Val := ubz.A;
          end;

        rqtUBZ_P: // мощность
          if ChannelIds.ID_Src = SRC_AI then
          begin
            Result := IfThen(ChannelIds.N_Src = 8, recchnresData, recchnresUnknown);
            if Result = recchnresData then
              Values[0].Val := ubz.P;
          end;
        end;

    DEVTYPE_TR101:
      case gbv.ReqDetails.rqtp of
        rqtTR101_STATE: // ТР-101
          begin
            Result := IfThen(CheckUBZAlarm(ubz, gbv), recchnresData, recchnresUnknown);

            if not (Result <> recchnresData) and (ChannelIds.ID_Src = SRC_AI) then
            begin
              Result := IfThen(ChannelIds.N_Src in [1, 2, 3, 4], recchnresData, recchnresUnknown);
              if Result = recchnresData then
                Values[0].Val := ubz.T[ChannelIds.N_Src];
            end;
          end;
        rqtTR101_SP1: ProcessAO(1);
        rqtTR101_SP2: ProcessAO(2);
        rqtTR101_SP3: ProcessAO(3);
        rqtTR101_SP4: ProcessAO(4);
      end;

    else
      ProgramLog.AddMessage('CheckUBZChannel - bad DevType ' + IntToStr(ubz.ID_DevType));
  end;
end;

function TResponceParserThread.CheckVzletUrsvChannel(gbv: TGeomerBlockValues): bool;
var cnt: int;
    NAddr: Byte;
begin
  Result := false;
  if not (gbv.ReqDetails.rqtp in [rqtVZLET_URSV_Q1..rqtVZLET_URSV_Q4]) then Exit;

  if Vzlet_CheckCRC(gbv.gmBufRec, gbv.gmLenRec) and (gbv.gmBufRec[1] = 4) then
  begin
    NAddr := gbv.gmBufRec[0];
    cnt := gbv.gmBufRec[2];
    if (ChannelIds.NDevNumber = NAddr) // нужный прибор
       and (gbv.gmLenRec = 3 + cnt + 2) then // заголовок + данные + CRC
    begin
      if ChannelIds.ID_Src = SRC_AI then
      begin
        Values[0].Val := ReadSingleInv(gbv.gmBufRec, 3);
        case gbv.ReqDetails.rqtp of
          rqtVZLET_URSV_Q1: Result := (ChannelIds.N_Src = 1);
          rqtVZLET_URSV_Q2: Result := (ChannelIds.N_Src = 2);
          rqtVZLET_URSV_Q3: Result := (ChannelIds.N_Src = 3);
          rqtVZLET_URSV_Q4: Result := (ChannelIds.N_Src = 4);
        end;
      end;
    end;
  end;
end;

function TResponceParserThread.CheckVaconIsco4250Channel(gbv: TGeomerBlockValues): bool;
var gbvString: string;

    function ExtractNumberAfterString(const Prefix: string; var f: double): bool;
    var s: string;
        n: int;
    begin
      Result := false;
      n := Pos(Prefix + ',', gbvString);
      if n <= 0 then Exit;

      s := Copy(gbvString, n + 3, Length(gbvString));
      n := Pos(',', s);

      if n <= 0 then Exit;
      s := Copy(s, 1, n - 1);

      try
        f := MyStrToFloatDef(s, INCORRECT_VALUE);
        Result := f <> INCORRECT_VALUE;
      except
      end;
    end;

begin
  Result := false;
  if gbv.ReqDetails.rqtp <> rqtISCO4250 then Exit;

  gbvString := string(gbv.BufRecString);
  // проверим посылку
  if (Pos('DE,', gbvString) <= 0)
     or (Pos('ID,', gbvString) <= 0)
     or (Pos('LE,', gbvString) <= 0)
     or (Pos('VE,', gbvString) <= 0)
     or (Pos('FL,', gbvString) <= 0) then Exit;

  if ChannelIds.ID_Src = SRC_AI then
    case ChannelIds.N_Src of
      1: begin
           Result := ExtractNumberAfterString('FL', Values[0].Val); // расход
           if Result then
             Values[0].Val := Values[0].Val * 3600; // расход приходит в м3/с
         end;
      2: Result := ExtractNumberAfterString('LE', Values[0].Val); // уровень
      3: Result := ExtractNumberAfterString('VE', Values[0].Val); // уровень
    end;

  if (ChannelIds.ID_Src = SRC_CNT_DI) and (ChannelIds.N_Src = 1) then
    Result := ExtractNumberAfterString('VO', Values[0].Val); // объем
end;

function TResponceParserThread.CheckVaconNXLChannel(gbv: TGeomerBlockValues): bool;
var cnt: int;
    NAddr: Byte;
begin
  Result := false;
  if not (gbv.ReqDetails.rqtp in [rqtVACON_NXL_ENGINE_CURRENT..rqtVACON_NXL_PID_REFERENCE]) then Exit;

  if Vacon_CheckCRC(gbv.gmBufRec, gbv.gmLenRec) and (gbv.gmBufRec[1] = 3) then
  begin
    NAddr := gbv.gmBufRec[0];
    cnt := gbv.gmBufRec[2];
    if (ChannelIds.NDevNumber = NAddr) // нужный прибор
       and (cnt = 2) // ответ пришел нужной длины
       and (gbv.gmLenRec =  3 + cnt + 2) then // заголовок + данные + CRC
    begin
      Values[0].Val := ReadWORDInv(gbv.gmBufRec, 3);

      if ChannelIds.ID_Src = SRC_AI then
      case gbv.ReqDetails.rqtp of
        rqtVACON_NXL_ENGINE_CURRENT:  if ChannelIds.N_Src = 1 then
                                      begin
                                        Result := true;
                                        Values[0].Val := Values[0].Val / 10.0;
                                      end;

        rqtVACON_NXL_ENGINE_SPEED:    if ChannelIds.N_Src = 2 then Result := true;

        rqtVACON_NXL_POWER:           if ChannelIds.N_Src = 3 then
                                      begin
                                        Result := true;
                                        if Values[0].Val >= 65536 / 2 then  // отрицательное число
                                          Values[0].Val := Values[0].Val - 65536;

                                        Values[0].Val := Values[0].Val / 10.0;
                                      end;
      end;

      if (ChannelIds.ID_Src = SRC_DI) and (ChannelIds.N_Src = 1) and (gbv.ReqDetails.rqtp = rqtVACON_NXL_ALARM) then Result := true;
      if (ChannelIds.ID_Src = SRC_AO) and (ChannelIds.N_Src = 1) and (gbv.ReqDetails.rqtp = rqtVACON_NXL_PRESET_SPEED) then Result := true;
      if (ChannelIds.ID_Src = SRC_AO) and (ChannelIds.N_Src = 2) and (gbv.ReqDetails.rqtp = rqtVACON_NXL_PID_REFERENCE) then Result := true;
    end;
  end;
end;

function TResponceParserThread.SPT961_ProcessArchive(gbv: TGeomerBlockValues; Archive: TValuesCollection): bool;
var i: int;
    sField: string;
begin
  Result := false;

  case gbv.ReqDetails.rqtp of
    rqtSPT961_HOUR: sField := 'LastArchHour';
    rqtSPT961_DAY: sField := 'LastArchDay';
    else Exit;
  end;

  SetLength(Values, Archive.Count);
  for i := 0 to Archive.Count - 1 do
  begin
    Values[i].Val := Archive[i].Val.Val;
    Values[i].Chn := nil;
    // Время в СПТ961 всегда на конец интервала
    case gbv.ReqDetails.rqtp of
      rqtSPT961_DAY:
        begin
          Values[i].UTime := Archive[i].Val.UTime - UTC_DAY;
          Values[i].ValType := valueTypeDayArch;
        end;

      rqtSPT961_HOUR:
        begin
          Values[i].UTime := Archive[i].Val.UTime - UTC_HOUR;
          Values[i].ValType := valueTypeHourArch;
        end;
    end;
  end;

  Result := Length(Values) > 0;
  try
    FSQLWriter.Add('update ParamStates set ' + sField + ' = NowGM() where ID_Prm = ' + IntToStr(ChannelIds.ID_Prm) + ';');
  except
    on e: Exception do
      ProgramLog().AddException('SPT961_ProcessArchive update ' + sField + ' - ' + e.Message);
  end;
end;

procedure TResponceParserThread.TerminatedSet;
begin
  FSQLWriter.Terminate();
end;

function TResponceParserThread.CheckSPT961Channel(gbv: TGeomerBlockValues): bool;
var parser: TSPT961AnswerParser;
begin
  Result := false;
  if not (gbv.ReqDetails.rqtp in [rqtSPT961, rqtSPT961_HOUR, rqtSPT961_DAY]) then Exit;

  parser := TSPT961AnswerParser.Create();
  try
  try
    if not parser.Parse(gbv.gmBufRec, gbv.gmLenRec) then Exit;
    if not parser.CheckInitialRequest(gbv.ReqDetails, ChannelIds.PrmExtData) then
    begin
      ProgramLog.AddError('SPT961: Initial request mismatch: ' + gbv.ToString());
      Exit;
    end;

    case parser.ResType of
      logicaAnswerValue:
        begin
          Values[0].Val := parser.Value;
          Result := true;
        end;
      logicaAnswerTimeArray:
        Result := SPT961_ProcessArchive(gbv, parser.Archive);
    end;
  except
    on e: Exception do
      ProgramLog().AddError('CheckSPT961Channel - ' + e.Message + '(' + ArrayToString(gbv.gmBufRec, gbv.gmLenRec, true, true) + ') ' + ChannelIDs.LogIDs());
  end;
  finally
    parser.Free();
  end;
end;

function TResponceParserThread.CheckSPT941Channel(gbv: TGeomerBlockValues): bool;
var parser: TSPT941AnswerParser;
    i: int;
begin
  Result := false;
  if not (gbv.ReqDetails.rqtp in [rqtSPT941, rqtSPT943_0, rqtSPT943_1, rqtSPT943_2]) then Exit;

  parser := TSPT941AnswerParser.Create();
  try
  try
    if not parser.Parse(gbv) then
    begin
      ProgramLog.AddError('SPT941: ' + parser.ParseError + ' ' + gbv.ToString());
      Exit;
    end;

    Result := parser.Values.Count > 0;
    if not Result then Exit;

    SetLength(Values, parser.Values.Count);
    for i := 0 to parser.Values.Count - 1 do
      Values[i] := parser.Values[i].Val;
  except
    on e: Exception do
      ProgramLog().AddError('CheckSPT941Channel - ' + e.Message + '(' + ArrayToString(gbv.gmBufRec, gbv.gmLenRec, true, true) + ') ' + ChannelIDs.LogIDs());
  end;
  finally
    parser.Free();
  end;
end;

function TResponceParserThread.CheckMercury230SupportChannel(gbv: TGeomerBlockValues): bool;
var parser: TMercuryParser;
    coeff: int;
    addr: Word;
    archUDT: LongWord;
begin
  Result := false;
  parser := TMercuryParser.Create();
  try
  try
    if (gbv.gmLenRec < 1) or (gbv.gmBufRec[0] <> ChannelIds.NDevNumber) then Exit;

    Result := (gbv.ReqDetails.rqtp = rqtMercury230_MeterInfo) and parser.SetMeterInfoBuffer(gbv.gmBufRec, gbv.gmLenRec);
    if Result then
    begin
      coeff := parser.ReadImpulsePerKWt();
      if coeff > 0 then
        SQLReq_SetDevState(ChannelIds.ID_Device, DEV_STATE_NAMES_IMP_PER_KWT, IntToStr(coeff));

      Exit;
    end;

    Result := (gbv.ReqDetails.rqtp = rqtMercury230_LastArchAddr) and parser.SetLastArchAddrBuffer(gbv.gmBufRec, gbv.gmLenRec);
    if Result then
    begin
      addr := parser.ReadLastArchAddr();
      if addr > 0 then
        SQLReq_SetDevState(ChannelIds.ID_Device, DEV_STATE_NAMES_LAST_ARCH_ADDR, IntToStr(addr));

      archUDT := parser.ReadLastArchUDT();
      if archUDT > 0 then
        SQLReq_SetDevState(ChannelIds.ID_Device, DEV_STATE_NAMES_LAST_ARCH_UDT, IntToStr(archUDT));

      Exit;
    end;
  except
    on e: Exception do
      ProgramLog().AddError('CheckMercury230SupportChannel - ' + e.Message + ' (' + ArrayToString(gbv.gmBufRec, gbv.gmLenRec, true, true) + ') ' + ChannelIDs.LogIDs());
  end;
  finally
    parser.Free();
  end;
end;

function TResponceParserThread.CheckSupportRequest(gbv: TGeomerBlockValues): bool;
begin
  Result := false;
  case ChannelIds.ID_DevType of
    DEVTYPE_MERCURY_230: Result := CheckMercury230SupportChannel(gbv);
  end;
end;

function TResponceParserThread.ModbusDIDataLength(valueCount: int): int;
begin
  Result := (valueCount div 8) + IfThen(valueCount mod 8 > 0, 1, 0); // дискреты, бит на значение
end;

function TResponceParserThread.IsAppropriateModbusRTUAnswer(gbv: TGeomerBlockValues; desiredWordCnt: int = 0): bool;
var cnt, goodCnt: int;
    NAddr, fnc: Byte;
begin
  Result := false;

  if gbv.ReqDetails.Converter = PROTOCOL_CONVETER_MODBUS_TCP_RTU then
  begin
    Result := IsAppropriateModbusTCPAnswer(gbv, desiredWordCnt);
    Exit;
  end;

  // Например,
  // 6/7 = DevNum(1) + ModbusFnc(1) + DataLen(1) + Value1 (1 или 2) + CRC(2)
  if (gbv.gmLenRec < 6) then
  begin
    ProgramLog.AddMessage('IsAppropriateModbusRTUAnswer Short message');
    Exit;
  end;

  NAddr := gbv.gmBufRec[0];
  fnc := gbv.gmBufRec[1];
  cnt := gbv.gmBufRec[2];

  if fnc in [3, 4] then
    goodCnt := desiredWordCnt * 2 // аналоги, WORD на значение
  else
    goodCnt := ModbusDIDataLength(desiredWordCnt); // дискреты, бит на значение

  if not Modbus_CheckCRC(gbv.gmBufRec, gbv.gmLenRec) then
    ProgramLog.AddMessage('IsAppropriateModbusRTUAnswer Bad CRC')
  else
  if NAddr <> ChannelIds.NDevNumber then // нужный прибор
    ProgramLog.AddMessage('IsAppropriateModbusRTUAnswer Wrong Device')
  else
  if not (fnc in [2, 3, 4]) then
    ProgramLog.AddMessage('IsAppropriateModbusRTUAnswer Wrong fnc')
  else
  if (fnc in [3, 4]) and (gbv.gmLenRec < 7) then
    ProgramLog.AddMessage('IsAppropriateModbusRTUAnswer Short message')
  else
  if (desiredWordCnt > 0) and (cnt < goodCnt) then // ответ пришел нужной длины
    ProgramLog.AddMessage('IsAppropriateModbusRTUAnswer Wrong desiredWordCnt')
  else
    Result := true;

  if (gbv.gmLenRec > 3) and Modbus_CheckCRC(gbv.gmBufRec, gbv.gmLenRec) and (gbv.gmBufRec[1] in [2, 3, 4]) then
  begin
    NAddr := gbv.gmBufRec[0];
    cnt := gbv.gmBufRec[2];
    if (ChannelIds.NDevNumber = NAddr) // нужный прибор
       and ( (desiredWordCnt = 0) or (cnt = desiredWordCnt * 2) ) // ответ пришел нужной длины
       and (gbv.gmLenRec =  3 + cnt + 2) then // заголовок + данные + CRC
    begin
      Result := true;
    end;
  end;
end;

function TResponceParserThread.CheckAltistart22Channel(gbv: TGeomerBlockValues): bool;
var dataAddr: int;
begin
  Result := false;
  if (gbv.ReqDetails.rqtp <> rqtAltistart22_All) or not IsAppropriateModbusRTUAnswer(gbv, COUNT_ALTISTART22_ALL) then Exit;

  if ChannelIds.ID_Src = SRC_AI then
  begin
    case ChannelIds.N_Src of
      1..3: dataAddr := ADDR_ALTISTART22_LCR1 + ChannelIds.N_Src - 1;
      4: dataAddr := ADDR_ALTISTART22_U;
      else Exit;
    end;

    Values[0].Val := Modbus_ReadWord(gbv.gmBufRec, dataAddr - ADDR_ALTISTART22_ALL);
    Result := true;
  end;

  // последняя ошибка
  if (ChannelIds.ID_Src = SRC_DI) and (ChannelIds.N_Src = 1) then
  begin
    Values[0].Val := Modbus_ReadWord(gbv.gmBufRec, ADDR_ALTISTART22_LFT - ADDR_ALTISTART22_ALL);
    Result := true;
  end;
end;

function TResponceParserThread.ModbusRTUDataStart(gbv: TGeomerBlockValues): int;
begin
  Result := IfThen(gbv.ReqDetails.Converter = PROTOCOL_CONVETER_MODBUS_TCP_RTU, MODBUS_TCP_ANSWER_DATA_START, MODBUS_RTU_ANSWER_DATA_START);
end;

function TResponceParserThread.ModbusRTUDevNumber(gbv: TGeomerBlockValues): int;
var n: int;
begin
  Result := -1;
  n := IfThen(gbv.ReqDetails.Converter = PROTOCOL_CONVETER_MODBUS_TCP_RTU, 6, 0);
  if n < gbv.gmLenRec then
    Result := gbv.gmBufRec[n];
end;

function TResponceParserThread.ModbusRTUFunction(gbv: TGeomerBlockValues): int;
var n: int;
begin
  Result := -1;
  n := IfThen(gbv.ReqDetails.Converter = PROTOCOL_CONVETER_MODBUS_TCP_RTU, 7, 1);
  if n < gbv.gmLenRec then
    Result := gbv.gmBufRec[n];
end;

function TResponceParserThread.CheckGeostreamChannel(gbv: TGeomerBlockValues): bool;
var dataAddr: int;
    coeff: int;
begin
  Result := false;
  if (gbv.ReqDetails.rqtp <> rqtGeostream_All) or not IsAppropriateModbusRTUAnswer(gbv, COUNT_GEOSTREAM_ALL) then Exit;

  coeff := 1;
  case ChannelIds.ID_Src of
    SRC_AI:
      begin
        if ChannelIds.N_Src in [1..3] then
        begin
          dataAddr := ChannelIds.N_Src - 1;
          if ChannelIds.N_Src = 3 then
            coeff := 3600; // расход из м3/сек в м3/ч
        end
        else
          Exit;
      end;

    SRC_CNT_MTR:
      begin
        if ChannelIds.N_Src in [1, 2] then
          dataAddr := ChannelIds.N_Src + 2
        else
          Exit;
      end

    else Exit;
  end;

  Values[0].Val := ReadSingleInv(gbv.gmBufRec, ModbusRTUDataStart(gbv) + dataAddr * 4) * coeff;
  Result := true;
end;

function TResponceParserThread.CheckDRKChannel_Val_Digit(src: byte): byte;
begin
  // Yfghbvth 75h -> 75
  Result := (src shr 4) * 10 + (src and $0F);
end;

function TResponceParserThread.CheckDRKChannel_Val(gbv: TGeomerBlockValues; pos: int): double;
begin
  Result := CheckDRKChannel_Val_Digit(gbv.gmBufRec[pos]) * 1e6 +
            CheckDRKChannel_Val_Digit(gbv.gmBufRec[pos + 1]) * 1e4 +
            CheckDRKChannel_Val_Digit(gbv.gmBufRec[pos + 2]) * 1e2 +
            CheckDRKChannel_Val_Digit(gbv.gmBufRec[pos + 3]);
end;

function TResponceParserThread.CheckStreamlux700fChannel_ErrorCode(resp: AnsiString): bool;
begin
  Result := false;
  var n := 1;
  while n <= Length(resp) do
  begin
    if CharInSet(resp[n], ['R', 'I', 'H', 'E', 'Q', 'F', 'G', 'K']) then
    begin
      Values[0].Val := Ord(resp[n]);
      Exit(true);
    end;

    inc(n);
  end;
end;

function TResponceParserThread.CheckStreamlux700fChannel_SignalQuality(resp: AnsiString): bool;
begin
  var n := Pos('Q=', string(resp));
  if n <= 0 then
    Exit(false);

  inc(n, 2);
  var s: AnsiString := '';
  while (n < Length(resp)) and (CharInSet(resp[n], ['0'..'9'])) do
  begin
    s := s + resp[n];
    inc(n);
  end;

  var c: int;
  var v: double;
  System.Val(string(s), V, c);
  if c > 0 then
    Exit(false);

  Result := true;
  Values[0].Val := V;
end;

function TResponceParserThread.CheckStreamlux700fChannel_CommonVal(gbv: TGeomerBlockValues): bool;
begin
  var s: AnsiString;
  var n := 0;
  while n < Length(gbv.gmBufRec) do
  begin
    if not CharInSet(AnsiChar(gbv.gmBufRec[n]), ['0'..'9', '.', '+', '-', 'E', 'e']) then
      break;

    s := s + AnsiChar(gbv.gmBufRec[n]);
    inc(n);
  end;

  var c: int;
  var v: double;
  System.Val(string(s), V, c);
  if c > 0 then
    Exit(false);

  Result := true;
  Values[0].Val := V;
end;

function TResponceParserThread.CheckStreamlux700fChannel(gbv: TGeomerBlockValues): bool;
begin
  if (gbv.ReqDetails.rqtp <> rqtStreamlux700f) or (gbv.gmLenRec < 2) then
    Exit(false);

  var l := High(gbv.gmBufRec);
  if (gbv.gmBufRec[l - 1] <> 13) or (gbv.gmBufRec[l] <> 10) then
    Exit(false);

  if (ChannelIds.ID_Src = SRC_AI) and (ChannelIds.N_Src = 6) then
    Result := CheckStreamlux700fChannel_ErrorCode(gbv.BufRecString)
  else
  if (ChannelIds.ID_Src = SRC_AI) and (ChannelIds.N_Src = 7) then
    Result := CheckStreamlux700fChannel_SignalQuality(gbv.BufRecString)
  else
    Result := CheckStreamlux700fChannel_CommonVal(gbv);

  if Result then
  begin
    Values[0].UTime := gbv.gmTime;
    Values[0].Chn := pointer(gbv.ReqDetails.ID_Prm);
  end;
end;

function TResponceParserThread.CheckDRKChannel(gbv: TGeomerBlockValues): bool;
var
  coeff: double;
  c_code: int;
begin
  Result := false;
  if (gbv.ReqDetails.rqtp <> rqtDRK)
     or (gbv.gmLenRec <> 15)
     or (gbv.gmBufRec[0] <> gbv.ReqDetails.DevNumber) then Exit;

  c_code := gbv.gmBufRec[5];
  if not (c_code in [1..5]) then Exit;

  coeff := IntPower(10, c_code - 3);
  SetLength(Values, 2);

  Values[0].UTime := gbv.gmTime;
  Values[0].Val := CheckDRKChannel_Val(gbv, 11) * coeff / 10;
  Values[0].Chn := pointer(gbv.ReqDetails.ReqLinks[0].id);

  Values[1].UTime := gbv.gmTime;
  Values[1].Val := CheckDRKChannel_Val(gbv, 7) * coeff;
  Values[1].Chn := pointer(gbv.ReqDetails.ReqLinks[1].id);

  Result := true;
end;

function TResponceParserThread.CheckADCPChannelMasterChannel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
var
  s, v: string;
  f, f1: double;
  c: int;
  a: TArray<string>;
begin
  Result := recchnresUnknown;
  if gbv.ReqDetails.rqtp <> rqtADCP_Channel_Master then Exit;

  s := string(ReadString(gbv.gmBufRec, 0, gbv.gmLenRec));
  if s = '' then Exit;

  a := s.Split([',']);
  if (Length(a) <> 11) or (a[0] <> 'PRDIQ') then Exit;

  if ChannelIDs.ID_Src = SRC_AI then
  begin
    case ChannelIDs.N_Src of
      1:
        v := a[4]; // Расход
      2:
        v := a[5]; // Скорость потока
      3:
        v := a[3]; // Глубина
      4:
        v := a[7]; // Температура
      else
        Exit;
    end;

    v := v.Trim([' ', #13, #10]);
    Val(v, f, c);
    if c > 0 then
      Exit(recchnresEmpty);

    Values[0].Val := f;
    Result := recchnresData;
  end
  else
  if (ChannelIDs.ID_Src = SRC_CNT_MTR) and (ChannelIDs.N_Src = 1) then // Накопленный объем
  begin
    Val(a[1], f, c);
    if c > 0 then
      Exit(recchnresEmpty);

    Val(a[2], f1, c);
    if c > 0 then
      Exit(recchnresEmpty);

    Values[0].Val := f * 1e6 + f1;
    Result := recchnresData;
  end;
end;

function TResponceParserThread.IsAppropriateModbusTCPAnswer(gbv: TGeomerBlockValues; desiredWordCnt: int = 0): bool;
var cnt, goodCnt: int;
    NAddr, fnc: Byte;
    packetLen, packetID, protID: word;
begin
  Result := false;

  // Например, 00 FF 00 00 00 17 01 04 14 00 00 00 01 00 02 00 03 27 10 FF FF FF FD 00 04 00 05 00 06 (| |||||||||||||||'|   ¤||||||)

  // 10/11 = ID(2) + ProtocolID(2) + Len(2) + DevNum(1) + ModbusFnc(1) + DataLen(1) + Value1 (1 или 2)
  if (gbv.gmLenRec < 10) then Exit;

  packetID := ReadWordInv(gbv.gmBufRec, 0);
  protID := ReadWordInv(gbv.gmBufRec, 2);
  packetLen := ReadWordInv(gbv.gmBufRec, 4);
  NAddr := gbv.gmBufRec[6];
  fnc := gbv.gmBufRec[7];
  cnt := gbv.gmBufRec[8];

  if fnc in [3, 4] then
    goodCnt := desiredWordCnt * 2 // аналоги, WORD на значение
  else
    goodCnt := ModbusDIDataLength(desiredWordCnt); // дискреты, бит на значение

  if NAddr <> ChannelIds.NDevNumber then // нужный прибор
    ProgramLog.AddMessage('IsAppropriateModbusTCPAnswer Wrong Device')
  else
  if (gbv.ReqDetails.ReqID >= 0) and (packetID <> gbv.ReqDetails.ReqID) then // нужный пакет
    ProgramLog.AddMessage('IsAppropriateModbusTCPAnswer Wrong PacketID good = ' + IntToStr(gbv.ReqDetails.ReqID) + ', bad = ' + IntToStr(packetID))
  else
  if not (fnc in [2, 3, 4]) then
    ProgramLog.AddMessage('IsAppropriateModbusTCPAnswer Wrong fnc')
  else
  if (fnc in [3, 4]) and (gbv.gmLenRec < 11) then
    ProgramLog.AddMessage('IsAppropriateModbusTCPAnswer Short message')
  else
  if (protID <> 0) then // протокол Modbus
    ProgramLog.AddMessage('IsAppropriateModbusTCPAnswer Wrong protocol')
  else
  if packetLen <> gbv.gmLenRec - 6 then // длина пакета без TransactionID (2Б), типа протокола(2Б) и самой длины пакета(2Б)
    ProgramLog.AddMessage('IsAppropriateModbusTCPAnswer Wrong packet length')
  else
  if (desiredWordCnt > 0) and (cnt < goodCnt) then // ответ пришел нужной длины
    ProgramLog.AddMessage('IsAppropriateModbusTCPAnswer Wrong desiredWordCnt')
  else
    Result := true;
end;

function TResponceParserThread.CheckModbusTCPUserDefinedChannel(gbv: TGeomerBlockValues): bool;
var fnc: byte;
begin
  Result := (ChannelIds.ID_Src = SRC_USR)
            and (gbv.ReqDetails.rqtp = rqtUSER_DEFINED)
            and IsAppropriateModbusTCPAnswer(gbv, Max(gbv.ReqDetails.ReqLinkCount, 1));

  if not Result then Exit;

  fnc := gbv.gmBufRec[7];
  if fnc = 2 then
    ModbusReadDI(gbv, MODBUS_TCP_ANSWER_DATA_START)
  else
    ModbusReadAI(gbv, MODBUS_TCP_ANSWER_DATA_START);
end;

function TResponceParserThread.ModbusReadAI_Data(gbv: TGeomerBlockValues; posDataStart, dataIndex: int; var data: double): bool;
var addr: int;
    buf: array [0..3] of byte;
begin
  addr := posDataStart + dataIndex * 2;

  Result := gbv.gmLenRec >= addr + Modbus_ResultTypeLength(gbv.ReqDetails.ReqLinks[dataIndex].ext);
  if not Result then Exit;

  case gbv.ReqDetails.ReqLinks[dataIndex].ext of
    MODBUS_RESULT_WORD_LITTLE_ENDIAN:
      if ChannelIds.RealSrc() in [SRC_CNT_DI, SRC_CNT_MTR] then
        data := ReadWord(gbv.gmBufRec, addr)
      else
        data := ReadSmall(gbv.gmBufRec, addr);

    MODBUS_RESULT_LONG_BIG_ENDIAN:
      data := ReadLongIntInv(gbv.gmBufRec, addr);

    MODBUS_RESULT_LONG_LITTLE_ENDIAN:
      data := ReadLongInt(gbv.gmBufRec, addr);

    MODBUS_RESULT_SINGLE_BIG_ENDIAN:
      data := ReadSingleInv(gbv.gmBufRec, addr);

    MODBUS_RESULT_SINGLE_LITTLE_ENDIAN:
      data := ReadSingle(gbv.gmBufRec, addr);

    MODBUS_RESULT_DOUBLE_BIG_ENDIAN:
      data := ReadDoubleInv(gbv.gmBufRec, addr);

    MODBUS_RESULT_DOUBLE_LITTLE_ENDIAN:
      data := ReadDouble(gbv.gmBufRec, addr);

    MODBUS_RESULT_SINGLE_SHUFFLE:
      begin
        buf[0] := gbv.gmBufRec[addr + 2];
        buf[1] := gbv.gmBufRec[addr + 3];
        buf[2] := gbv.gmBufRec[addr + 0];
        buf[3] := gbv.gmBufRec[addr + 1];

        data := ReadSingleInv(buf, 0);
      end

    else
      if ChannelIds.RealSrc() in [SRC_CNT_DI, SRC_CNT_MTR] then
        data := ReadWordInv(gbv.gmBufRec, addr)
      else
        data := ReadSmallInv(gbv.gmBufRec, addr);
  end;
end;

procedure TResponceParserThread.ModbusReadAI(gbv: TGeomerBlockValues; posDataStart: int);
var i, n: int;
    val: double;
    bFirst: bool;
begin
  bFirst := true;
  for i := 0 to Max(gbv.ReqDetails.ReqLinkCount, 1) - 1 do
  begin
    if (i > 0) and (gbv.ReqDetails.ReqLinks[i].id <= 0) then
      continue;

    if not ModbusReadAI_Data(gbv, posDataStart, i, val) then
      continue;

    if not bFirst then
      SetLength(Values, Length(Values) + 1);

    bFirst := false;
    n := High(Values);
    Values[n].Val := val;
    Values[n].UTime := gbv.gmTime;
    Values[n].ValType := valueTypeCurrent;
    Values[n].Chn := pointer(gbv.ReqDetails.ReqLinks[i].id);
  end;
end;

procedure TResponceParserThread.ModbusReadDI(gbv: TGeomerBlockValues; posDataStart: int);
var i, n, addr, lastInfoByte: int;
    mask: byte;
begin
  lastInfoByte := posDataStart + ModbusDIDataLength(gbv.ReqDetails.ReqLinkCount) - 1;

  for i := 0 to Max(gbv.ReqDetails.ReqLinkCount, 1) - 1 do
  begin
    if (i > 0) and (gbv.ReqDetails.ReqLinks[i].id <= 0) then
      continue;

    if i > 0 then
      SetLength(Values, Length(Values) + 1);
    n := High(Values);

    addr := lastInfoByte - i div 8;
    mask := 1 shl (i mod 8);
    Values[n].Val := IfThen(gbv.gmBufRec[addr] and mask > 0, 1, 0);
    Values[n].UTime := gbv.gmTime;
    Values[n].ValType := valueTypeCurrent;
    Values[n].Chn := pointer(gbv.ReqDetails.ReqLinks[i].id);
  end;
end;

function TResponceParserThread.CheckModbusRTUUserDefinedChannel(gbv: TGeomerBlockValues): bool;
var fnc: byte;
begin
  Result := (ChannelIds.ID_Src = SRC_USR)
            and (gbv.ReqDetails.rqtp = rqtUSER_DEFINED)
            and IsAppropriateModbusRTUAnswer(gbv, max(gbv.ReqDetails.ReqLinkCount, 1));

  if not Result then Exit;

  fnc := gbv.gmBufRec[1];
  if fnc = 2 then
    ModbusReadDI(gbv, ModbusRTUDataStart(gbv))
  else
    ModbusReadAI(gbv, ModbusRTUDataStart(gbv));
end;

function TResponceParserThread.CheckMainSrvUniversalResponce(gbv: TGeomerBlockValues): bool;
var parser: TMainSrvReqParser;
    i: int;
begin
  Result := false;
  if gbv.ReqDetails.rqtp <> rqtMainSrv then Exit;

  parser := TMainSrvReqParser.Create();
  try
    if not parser.Parse(gbv.gmBufRec, gbv.gmLenRec) then Exit;
    if (parser.Data.Count <= 0) or (parser.Data[0].ID_Prm <> ChannelIDs.MainSrvPrmID) then exit;

    SetLength(Values, parser.Data.Count);
    for i := 0 to parser.Data.Count - 1 do
      Values[i] := parser.Data[i].Val;

    Result := true;
  finally
    parser.Free();
  end;
end;

function TResponceParserThread.RecognizeAndCheckChannel(gbv: TGeomerBlockValues): TRecognizeChannelResult;
var res: bool;
begin
  Result := recchnresUnknown;
  SetLength(Values, 1);
  Values[0].UTime := gbv.gmTime;
  Values[0].ValType := valueTypeCurrent;
  Values[0].Chn := nil;

  try
    if CheckSupportRequest(gbv) then
    begin
      Result := recchnresSupport;
      Exit; // вспомогательный запрос не несет в себе данных
    end;

    res := false;
    if (gbv.gbt in [gbtShort, gbtLong, gbtLongUBZ, gbtLongISCO]) then
    begin
      res := CheckGeomerChannel(gbv);
      if res then
        Result := recchnresData;
    end
    else
    if (gbv.gbt = gbt485) and (gbv.gmLenRec > 0) then
    begin
      if ChannelIds.ID_DevType in Tecon19_Family then
        Result := CheckTecon19Channel(gbv)
      else
      case ChannelIds.ID_DevType of
        DEVTYPE_UBZ, DEVTYPE_TR101:
          Result := CheckUBZChannel(gbv);
        DEVTYPE_ADCP_CHANNEL_MASTER:
          Result := CheckADCPChannelMasterChannel(gbv);
        else
          begin
            case ChannelIds.ID_DevType of
              DEVTYPE_I7017: res := CheckICP_AIChannel(gbv);
              DEVTYPE_I7041D: res := CheckICP_DIChannel(gbv);
              DEVTYPE_TRM138: res := CheckTRM138Channel(gbv);
              DEVTYPE_VZLET_URSV: res := CheckVzletUrsvChannel(gbv);
              DEVTYPE_VACON_NXL: res := CheckVaconNXLChannel(gbv);
              DEVTYPE_ISCO_4250: res := CheckVaconIsco4250Channel(gbv);
              DEVTYPE_SPT_941,
              DEVTYPE_SPT_943: res := CheckSPT941Channel(gbv);
              DEVTYPE_SPT_961: res := CheckSPT961Channel(gbv);
              DEVTYPE_SIMAG11: res := CheckSimag11Channel(gbv);
              DEVTYPE_PETERFLOWRS: res := CheckPeterFlowRSChannel(gbv);
              DEVTYPE_MERCURY_230: res := CheckMercury230Channel(gbv);
              DEVTYPE_ALTISTART_22: res := CheckAltistart22Channel(gbv);
              DEVTYPE_GEOSTREAM_71: res := CheckGeostreamChannel(gbv);
              DEVTYPE_ALLEN_BRADLEY_MICRO_8XX: res := CheckModbusTCPUserDefinedChannel(gbv);
              DEVTYPE_MAIN_SRV: res := CheckMainSrvUniversalResponce(gbv);
              DEVTYPE_MODBUS_RTU: res := CheckModbusRTUUserDefinedChannel(gbv);
              DEVTYPE_DRK: res := CheckDRKChannel(gbv);
              DEVTYPE_STREAMLUX700F: res := CheckStreamlux700fChannel(gbv);
            end;

            if res then
              Result := recchnresData;
          end;
      end;
    end;
  except
    on e: Exception do
      ProgramLog().AddError('qRecognize - ' + e.Message + '(' + ArrayToString(gbv.gmBufRec, gbv.gmLenRec, true, true) + ') ' + ChannelIDs.LogIDs());
  end;
end;

function TResponceParserThread.GetUBZ(ID_Obj, NAddr: int): TGeomerLastValue;
begin
  Result := glvBuffer.GetUBZ(ID_Obj, NAddr);
end;

procedure TResponceParserThread.ProcessUBZDataBlock(gbv: TGeomerBlockValues);
var // для УБЗ
    NAddr: Byte;
    ubz: TGeomerLastValue;
    coeffI: double;
    dataStart: int;
begin
  if IsAppropriateModbusRTUAnswer(gbv) and (ModbusRTUFunction(gbv) = 3) then
  begin
    NAddr := ModbusRTUDevNumber(gbv);
    ubz := GetUBZ(ChannelIds.ID_Obj, NAddr);
    if ubz = nil then Exit;

    dataStart := ModbusRTUDataStart(gbv);
    ubz.tLastUBZReq := gbv.gmTime;
    if ubz.ID_DevType = DEVTYPE_UBZ then
    begin
      case gbv.ReqDetails.rqtp of
        rqtUBZ_TRANSFORMATOR: // тип трансформатора
          begin
            if (ReadWORDInv(gbv.gmBufRec, dataStart) = 1) and (ReadWORDInv(gbv.gmBufRec, dataStart + 2) > 100) then
              ubz.UBZ_Transformator := ttExternalAndBig
            else
              ubz.UBZ_Transformator := ttInternalOrSmall;
          end;
        rqtUBZ_ALARMS: // состояние
          begin
            ubz.wState := ReadWORDInv(gbv.gmBufRec, dataStart);
            ubz.dwAlarms := DWORD(ReadWORDInv(gbv.gmBufRec, dataStart + 4) shl 16) or DWORD(ReadWORDInv(gbv.gmBufRec, dataStart + 2));
          end;
        rqtUBZ_I: // токи
          begin
            if ubz.UBZ_Transformator <> ttUnknown then // для определения единиц измерения токов обязательно нужен тип трансформатора
            begin
              // токи в десятых долях ампера на маленьких трансформаторах и в амперах на больших
              coeffI := IfThen(ubz.UBZ_Transformator = ttExternalAndBig, 1, 0.1);

              ubz.I[1] := ReadWORDInv(gbv.gmBufRec, dataStart) * coeffI;
              ubz.I[2] := ReadWORDInv(gbv.gmBufRec, dataStart + 2) * coeffI;
              ubz.I[3] := ReadWORDInv(gbv.gmBufRec, dataStart + 4) * coeffI;
            end;
            // ток нулевой последовательности от трансформатора не зависит
            ubz.I0 := ReadWORDInv(gbv.gmBufRec, dataStart + 6) / 10.0;
          end;
        rqtUBZ_U: // напряжения
          begin
            // напряжения в вольтах
            ubz.U[1] := ReadWORDInv(gbv.gmBufRec, dataStart);
            ubz.U[2] := ReadWORDInv(gbv.gmBufRec, dataStart + 2);
            ubz.U[3] := ReadWORDInv(gbv.gmBufRec, dataStart + 4);
          end;
        rqtUBZ_P: // мощность в десятках Вт перевести в кВт
          ubz.P := ReadWORDInv(gbv.gmBufRec, dataStart) * 10 / 1000;
        rqtUBZ_A: // моточасы
          ubz.A := ReadWORDInv(gbv.gmBufRec, dataStart);
        rqtUBZ_ALARM_LOG1, rqtUBZ_ALARM_LOG2:
          begin
            { порядок данных:
               st - код аварии - 2 байта
               значение параметра - 2 байта
               dt - время аварии - 2 х 2 байта }
            ubz.AddUBZAlarmLogOneEvt(ReadUINTInv(gbv.gmBufRec, dataStart + 4), ReadWORDInv(gbv.gmBufRec, dataStart), gbv.gmTime);
            ubz.AddUBZAlarmLogOneEvt(ReadWORDInv(gbv.gmBufRec, dataStart + 12), ReadWORDInv(gbv.gmBufRec, dataStart + 8), gbv.gmTime);
            if gbv.ReqDetails.rqtp = rqtUBZ_ALARM_LOG1 then
              ubz.AddUBZAlarmLogOneEvt(ReadWORDInv(gbv.gmBufRec, dataStart + 20), ReadWORDInv(gbv.gmBufRec, dataStart + 16), gbv.gmTime);
          end;
      end;
    end;

    if ubz.ID_DevType = DEVTYPE_TR101 then
    begin
      case gbv.ReqDetails.rqtp of
        rqtTR101_STATE: // ТР-101
          begin
            // аварии
            ubz.wState := ReadWORDInv(gbv.gmBufRec, dataStart);
            ubz.dwAlarms := ReadWORDInv(gbv.gmBufRec, dataStart + 2);

            // температуры
            ubz.T[1] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart + 4));
            ubz.T[2] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart + 6));
            ubz.T[3] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart + 8));
            ubz.T[4] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart + 10));
          end;
        rqtTR101_SP1: ubz.SP[1] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart));
        rqtTR101_SP2: ubz.SP[2] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart));
        rqtTR101_SP3: ubz.SP[3] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart));
        rqtTR101_SP4: ubz.SP[4] := IntFromTR101Word(ReadWORDInv(gbv.gmBufRec, dataStart));
      end;
    end;
  end;
end;

procedure TResponceParserThread.DelProcessedGBV(g: TGeomerBlockValues);
var n: int;
begin
  try
    if g <> nil then
    begin
      synch.BeginWrite();
      try
        n := lBlocks.IndexOf(g);
        if n >= 0 then
          lBlocks.Delete(n);

        g.Free();
      finally
        synch.EndWrite();
      end;
    end;
  except
    on e: Exception do
      ProgramLog().AddException('DelGBV - ' + e.Message);
  end;
end;

function TResponceParserThread.WaitForTimeout(TimeOut: int; KillUnterminated: bool = true): int;
begin
  FSQLWriter.WaitForTimeout(TimeOut, KillUnterminated);
  Result := inherited;
end;

procedure TResponceParserThread.WriteVals(const sSQL: string);
begin
  try
    ExecSQL(sSQL);
  except
    on e: Exception do
      ProgramLog().AddException('qW_Vals - ' + e.Message);
  end;
end;

procedure TResponceParserThread.InitQueries();
begin
  if q = nil then
    q := TGMSqlQuery.Create();

  if qRead = nil then
    qRead := TGMSqlQuery.Create();
end;

function TResponceParserThread.NextGBVToProcess(): TGeomerBlockValues;
var i, nGBVInPProgress: int;
begin
  nGBVInPProgress := 0;
  synch.BeginRead();
  try
    for i := 0 to lBlocks.Count - 1 do
      if lBlocks.Items[i].gmTime < lBlocks.Items[nGBVInPProgress].gmTime then
        nGBVInPProgress := i;

    Result := lBlocks.Items[nGBVInPProgress];
  finally
    synch.EndRead();
  end;
end;

procedure TResponceParserThread.ProcessValuesList();
var i, id_prm: int;
    sSQL: string;
    qForDataAdd: TGMSqlQuery;
begin
  for i := 0 to High(Values) do
  begin
    GMPostMessage(WM_DEVICE_ONLINE, ChannelIds.ID_Obj, ChannelIds.ID_Device, DefaultLogger);

    if (Values[i].Chn <> nil) and (int(Values[i].Chn) <> ChannelIds.ID_Prm) then
    begin
      id_prm := int(Values[i].Chn);
      qForDataAdd := nil;
    end
    else
    begin
      id_prm := ChannelIds.ID_Prm;
      qForDataAdd := q;
    end;

    case Values[i].ValType of
      valueTypeCurrent:
        begin
          glvBuffer.AddSimpleValue(id_prm, Values[i].UTime, Values[i].Val, qForDataAdd);

          sSQL := Format('perform WriteVal(%d, %d, %s);',
                         [id_prm, Values[i].UTime, MyFloatToStr(Values[i].Val)]);

          FSQLWriter.Add(sSQL);

          if (ChannelIds.ID_Src = SRC_AI) and (lProcessedAIList.IndexOf(pointer(id_prm)) < 0) then
            lProcessedAIList.Add(pointer(id_prm));

          if (ChannelIds.ID_Src = SRC_CNT_DI) and (lProcessedCNTList.IndexOf(pointer(id_prm)) < 0) then
            lProcessedCNTList.Add(pointer(id_prm));
        end;

      valueTypeDayArch:
        begin
          sSQL := Format('perform WriteArchDay(%d, %d, %s);',
                         [id_prm, Values[i].UTime, MyFloatToStr(Values[i].Val)]);
          FSQLWriter.Add(sSQL);
        end;

      valueTypeHourArch:
        begin
          sSQL := Format('perform WriteArchHour(%d, %d, %s);',
                         [id_prm, Values[i].UTime, MyFloatToStr(Values[i].Val)]);
          FSQLWriter.Add(sSQL);
        end;
    end;
  end;
end;

function TResponceParserThread.CheckOnePrmOnly(devType: int): string;
begin
  // Ряд приборов отвечают большими посылками с кучей каналов
  // перебирать их поканально смысла нет, но чтобы не ломать общую логику,
  // выдернем для них первый попавшийся канал
  Result := '';
  if devType in [DEVTYPE_SPT_941, DEVTYPE_SPT_943, DEVTYPE_DRK] then
    Result := ' limit 1';
end;

function TResponceParserThread.GetPrmQueryTxt(ReqDetails: TRequestDetails): string;
begin
  if ReqDetails.ID_Prm > 0 then
    Result := ' select * from DeviceSystem ' +
              ' where ID_PT > 0 and ID_Prm = ' + IntToStr(ReqDetails.ID_Prm)
  else
  if ReqDetails.ID_Device > 0 then
    Result := ' select * from DeviceSystem ' +
              ' where ID_PT > 0 and ID_Device = ' + IntToStr(ReqDetails.ID_Device) + CheckOnePrmOnly(ReqDetails.ID_DevType)
  else
  if ReqDetails.ID_Obj > 0 then
    Result := ' select * from DeviceSystem ' +
              ' where ID_PT > 0 and ID_Obj = ' + IntToStr(ReqDetails.ID_Obj)
  else
    Result := ' select * from DeviceSystem ' +
              '   where ID_PT > 0 ' +
              '         and COALESCE(ObjType, 0) = ' + IntToStr(OBJ_TYPE_GM) + // это уже точно Геомер
              '         and N_Car = ' + IntToStr(ReqDetails.N_Car);
end;

procedure TResponceParserThread.ProcessExtendedSummChn(ID_Prm, ExtType: int);
var qSummChn: TGMSqlQuery;
begin
  qSummChn := TGMSqlQuery.Create();
  try
    qSummChn.SQL.Text := Format(' select (select ID_Obj from Devices where ID_Device = %d) as ID_Obj, * from Params ' +
                         '  where ID_PT > 0 and BaseChn = %d and ID_Src = %d',
                         [ChannelIds.ID_Device, ID_Prm, ExtType] );

    qSummChn.Open();
    if not qSummChn.Eof then
      glvBuffer.AddSimpleValue(qSummChn.FieldByName('ID_Prm').AsInteger, Values[0].UTime, Values[0].Val, qSummChn);
  except
    on e: Exception do
      ProgramLog().AddException('ProcessExtendedSummChn - qSummChn - ' + e.Message);
  end;
  qSummChn.Free();
end;

procedure TResponceParserThread.ProcessSumsForAI();
var i: int;
begin
  for i := 0 to lProcessedAIList.Count - 1 do
    ProcessExtendedSummChn(int(lProcessedAIList[i]), SRC_SUM_AI);
end;

procedure TResponceParserThread.ProcessMtrForCNT();
var i: int;
begin
  for i := 0 to lProcessedCNTList.Count - 1 do
    ProcessExtendedSummChn(int(lProcessedCNTList[i]), SRC_CNT_MTR);
end;

procedure TResponceParserThread.ParseReqDetails_SPT(gbv: TGeomerBlockValues);
var parser: TSPT961AnswerParser;
    prm: ArrayOfString;
    id_prm: int;

  function CheckExtDataValueType(const extdata: string; rqtp: T485RequestType): bool;
  var Chn, Prm: int;
  begin
    Result := false;
    if TSPT961ReqCreator.ExtractFromExtData(extdata, rqtp, Chn, Prm)
       and (Chn = parser.ResChn)
       and (Prm = parser.ResPrm) then
    begin
      gbv.ReqDetails.rqtp := rqtp;
      Result := true;
    end;
  end;

begin
  parser := TSPT961AnswerParser.Create();
  try
    if not parser.Parse(gbv.gmBufRec, gbv.gmLenRec) then
      Exit;

    prm := QueryResultArray_FirstRow(
             Format(
              #13#10' select ID_Prm, ExtData ' +
              #13#10'  from Params p ' +
              #13#10'       inner join Devices d on p.ID_Device = d.ID_Device and ID_DevType = %d' +
              #13#10'       inner join Objects o on o.ID_Obj = d.ID_Obj ' +
              #13#10'    where Position(''%d'' in ExtData) = 1 and Position('' %d'' in ExtData) > 1' +
              #13#10'          and d.Number = %d' +
              #13#10'          and o.ID_Obj = %d',
              [DEVTYPE_SPT_961, parser.ResChn, parser.ResPrm, parser.ResDevNumber, gbv.ReqDetails.ID_Obj]));

    if (Length(prm) = 0) or (prm[High(prm)] = '1') then Exit;

    ID_Prm := StrToIntDef(prm[0], -1);
    if ID_Prm > 0 then
    begin
      if CheckExtDataValueType(prm[1], rqtSPT961)
         or CheckExtDataValueType(prm[1], rqtSPT961_DAY)
         or CheckExtDataValueType(prm[1], rqtSPT961_HOUR) then
      begin
        gbv.ReqDetails.ID_Prm := ID_Prm;
      end;
    end;
  finally
    parser.Free();
  end;
end;

procedure TResponceParserThread.ParseReqDetails(gbv: TGeomerBlockValues);
begin
  ParseReqDetails_SPT(gbv);
end;

procedure TResponceParserThread.SafeExecute;
var gbv: TGeomerBlockValues;
    lastStateReport: int64;
    bOnlyUnknown: bool;
    res: TRecognizeChannelResult;
begin
  CoInitialize(nil);
  lastStateReport := 0;

  while not Terminated do
  begin
    try
      InitQueries();
      lProcessedAIList.Clear();
      lProcessedCNTList.Clear();

      if Abs(lastStateReport - GetTickCount()) > 2000 then // 2 сек, чтобы не частить
      begin
        SQLReq_SetSystemState(SYSTEM_STATES_SQL_QUEUE, IntToStr(FSQLWriter.Count));
        SQLReq_SetSystemState(SYSTEM_STATES_PARSER_QUEUE, IntToStr(lBlocks.Count));
        lastStateReport := GetTickCount();
      end;

      if lBlocks.Count = 0 then
      begin
        SleepThread(100);
        continue;
      end;

      gbv := NextGBVToProcess();
      if gbv = nil then
      begin
        SleepThread(100);
        continue;
      end;

      ProgramLog.AddMessage('lBlocks.Count = ' + IntToStr(lBlocks.Count));
      ParseReqDetails(gbv);
      q.Close();
      q.SQL.Text := GetPrmQueryTxt(gbv.ReqDetails);
      q.Open();

      if q.Eof then
        Defaultlogger.Warn('GetPrmQueryTxt empty result. ' + q.SQL.Text);

      // если это данные от УБЗ, то обработаем блок УБЗ
      if not q.Eof and (gbv.gbt = gbt485) then
      try
        ChannelIDs := ChannelIdsFromQ(q);
        ProcessUBZDataBlock(gbv);
      except
        on e: Exception do
          ProgramLog().AddError('qUBZ - ' + e.Message + ' - ' + ArrayToString(gbv.gmBufRec, gbv.gmLenRec, true, true) + ' ' + ChannelIDs.LogIDs() + ', rqtp = ' + gbv.ReqDetails.rqtp.ToString());
      end;

      bOnlyUnknown := true;
      while not q.Eof do
      begin
        ChannelIDs := ChannelIdsFromQ(q);
        res := RecognizeAndCheckChannel(gbv);

        if res <> recchnresUnknown then
        begin
          ProgramLog.AddMessage(GetEnumName(TypeInfo(TRecognizeChannelResult), ord(res)) + ' GBV ' + gbv.ToString() + ' - ' + ChannelIDs.LogIDs());
          bOnlyUnknown := false;
          if res = recchnresData then
            ProcessValuesList();
        end;

        q.Next();
      end;
      q.Close();

      if bOnlyUnknown then
      begin
        ProgramLog.AddMessage('Unrecognised GBV ' + gbv.ToString());
      end
      else
      begin
        ProcessSumsForAI();
        ProcessMtrForCNT();
      end;

      DelProcessedGBV(gbv);
    except
      on e: Exception do
      begin
        ProgramLog().AddException('q - ' + e.Message);
        TryFreeAndNil(q);
      end;
    end;
  end;

  TryFreeAndNil(q);
end;

function TResponceParserThread.Queue: int;
begin
  synch.BeginRead();
  try
    Result := lBlocks.Count + FSQLWriter.Count;
  except
    Result := 0;
  end;
  synch.EndRead();
end;

end.
