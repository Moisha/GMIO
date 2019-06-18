////////////////////////////////////////////
// Класс для разбора и хранения блока данных от приборов Геомер
////////////////////////////////////////////
unit GMBlockValues;

interface
uses Windows, Classes, SysUtils, GMGlobals, DateUtils, GMConst;

const
  GEOMER_WITH_UBZ_BLOCK_LEN = 38;
  GEOMER_WITH_UBZ_DEVICE_LEN = 18;

type
  TGeomerBlockType = (gbtUnknown, gbtShort, gbtLong, gbtLongUBZ, gbtLongISCO, gbt485);

  TGMUBZData = record
    Address: byte;  // 0 - адрес УБЗ в сети 485
    CommStatus: byte;  // 1 – флаги состояния связи с УБЗ
    Current: Word; // 2 - усредненный фазный ток по 3м фазам
    Voltage: Word; // 4 – усредненное линейное напряжение по 3м фазам, В
    Power: double; // 14 – активная мощность, десятки Вт
    AlarmState: ULONG; // регистры аварий 901 и 902
  end;

  TGeomerBlockValues = class
  private
    FTraced: bool;
    FgmBufRec: ArrayOfByte;

    function GetDT: TDateTime;
    function GetDI(i: int): byte;
    function GetDO(i: int): byte;
    procedure Trace(buf: array of byte; cnt: int);
    procedure ReadBaseData(buf: array of byte);
    function GetBufRecString: AnsiString;
    function GetLenRec: int;
    procedure ReadLongDataWithUBZ_ReadUBZData(buf: array of byte; n: int);
  protected
    function NeedTrace(): bool; virtual;
  public
    gbt: TGeomerBlockType;
    ReqDetails: TRequestDetails;

    AllDI: Byte;
    AllDO: Byte;
    AI: array [0..3] of double;  // анальные нумеруются с 0
                                 // анальный 0 - это температура внутри корпуса прибора
    DIC: array[1..5] of int;     // счетчики с единицы

    LE: double;  // уровень в м, 4 байта: длинное целое младшим байтом вперед, для получения уровня в м разделить на 100 000.
    VE: double;  // - скорость в м/с, 4 байта: длинное целое младшим байтом вперед, для получения скорости в м/c разделить на 10 000.
    FL: double;  // - расход в м3/c, 4 байта: длинное целое младшим байтом вперед, для получения расхода разделить на 1 000 000
    VO: double;  // - объем в м3, 4 байта: длинное целое младшим байтом вперед, для получения объема разделить на 10.

    gmTime, iscoTime: UINT;

    moneyLeft: int;
    ubzList: array[0..7] of TGMUBZData;
    ubzCount: int;

    property gmBufRec: ArrayOfByte read FgmBufRec;
    property gmLenRec: int read GetLenRec;

    property DT: TDateTime read GetDT;
    property DI[i: int]: byte read GetDI;
    property DOut[i: int]: byte read GetDO;

    property BufRecString: AnsiString read GetBufRecString;
    procedure ReadShortData(buf: array of byte);
    procedure ReadLongData(buf: array of byte);
    procedure ReadLongDataWithISCO(buf: array of byte);
    procedure ReadLongDataWithUBZ(buf: array of byte);
    procedure Read485(const buf: array of byte; cnt: int);
    constructor Create; overload;
    procedure SetBufRec(const bufRec: array of byte; cnt: int = -1); overload;
    procedure SetBufRec(const s: AnsiString); overload;
    function ToString(): string; override;
  end;

implementation

uses Forms, Math, ProgramLogFile;

{ TGeomerBlockValues }

procedure TGeomerBlockValues.Trace(buf: array of byte; cnt: int);
var f: TFileStream;
    path: string;
    fname: string;
    n: int;
begin
  if FTraced or (cnt <= 0) or not NeedTrace() then Exit;

  path := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName)) + 'trace\';
  if not ForceDirectories(path) then Exit;

  try
    fname := path + IntToStr(ReqDetails.N_Car) + '_' + IntToStr(gmTime) + '_';
    n := 1;
    while FileExists(fname + IntToStr(n) + '.gbv') do
      inc(n);

    fname := fname + IntToStr(n) + '.gbv';

    f := nil;
    try
      f := TFileStream.Create(fname, fmCreate);
      f.Write(buf, cnt);
    except
    end;

    if f <> nil then
      f.Free();
  except
  end;

  FTraced := true;
end;

function TGeomerBlockValues.GetDI(i: int): byte;
var n: byte;
begin
  Result:=0;
  
  if i in [0..7] then
  begin
    n:=1 shl i;
    if (AllDI and n)>0 then Result:=1;
  end;
end;

function TGeomerBlockValues.GetDO(i: int): byte;
var n: byte;
begin
  Result:=0;

  if i in [0..7] then
  begin
    n:=1 shl i;
    if (AllDO and n)>0 then Result:=1;
  end;
end;

function TGeomerBlockValues.GetDT: TDateTime;
begin
  Result := UTCtoLocal(gmTime);
end;

function TGeomerBlockValues.GetLenRec: int;
begin
  Result := Length(FgmBufRec);
end;

procedure TGeomerBlockValues.ReadBaseData(buf: array of byte);
begin
  ReqDetails.N_Car := ReadWord(buf, 2);
  gmTime := ReadUINT(buf, 4);
end;

procedure TGeomerBlockValues.ReadShortData(buf: array of byte);
begin
  // общая часть короткого и длинного ответов
  ReadBaseData(buf);
  Trace(buf, 24);

  gbt := gbtShort;

  AllDI := buf[16];
  AllDO := buf[17];

  AI[0] := (ReadWord(buf, 18) * 2.5 / 1023.0 - 0.444) / 0.00625; // сразу в градусы
  AI[1] := ReadWord(buf, 20) * 5 / 1023.0;
  AI[2] := ReadWord(buf, 22) * 5 / 1023.0;
  AI[3] := 0;
end;

procedure TGeomerBlockValues.ReadLongData(buf: array of byte);
begin
  ReadBaseData(buf);
  Trace(buf, 46);

  ReadShortData(buf);
  gbt := gbtLong;

  // счетчики
  DIC[1] := ReadWord(buf, 24);
  DIC[2] := ReadWord(buf, 26);
  DIC[3] := ReadWord(buf, 28);
  DIC[4] := ReadWord(buf, 30);

  // вместо 5го счетчика импульсов - 3й анальный вход
  AI[3] := ReadWord(buf, 32) * 32 / 1023.0;
end;

procedure TGeomerBlockValues.ReadLongDataWithISCO(buf: array of byte);
begin
  ReadBaseData(buf);
  Trace(buf, 90);

  ReadLongData(buf);

  gbt := gbtLongISCO;
  iscoTime := ReadUINT(buf, 50);

  LE := ReadUINT(buf, 54) / 100000.0;  // уровень в м, 4 байта: длинное целое младшим байтом вперед, для получения уровня в м разделить на 100 000.
  VE := ReadUINT(buf, 58) / 10000.0;  // - скорость в м/с, 4 байта: длинное целое младшим байтом вперед, для получения скорости в м/c разделить на 10 000.
  FL := ReadUINT(buf, 62) / 1000000.0 * 3600;  // - расход в м3/c, 4 байта: длинное целое младшим байтом вперед, для получения расхода разделить на 1 000 000
  VO := ReadUINT(buf, 66) / 10.0;  // - объем в м3, 4 байта: длинное целое младшим байтом вперед, для получения объема разделить на 10.
end;

procedure TGeomerBlockValues.ReadLongDataWithUBZ_ReadUBZData(buf: array of byte; n: int);
var
  ubzStart: int;
begin
  ubzStart := GEOMER_WITH_UBZ_BLOCK_LEN + GEOMER_WITH_UBZ_DEVICE_LEN * n;

  ubzList[n].Address := buf[ubzStart];  // 0 - адрес УБЗ в сети 485
  ubzList[n].CommStatus := buf[ubzStart + 1];  // 1 – флаги состояния связи с УБЗ
  ubzList[n].Current := ReadWord(buf, ubzStart + 2); // 2 - усредненный фазный ток по 3м фазам
  ubzList[n].Voltage := ReadWord(buf, ubzStart + 4); // 4 – усредненное линейное напряжение по 3м фазам, В
  ubzList[n].Power := ReadUINT(buf, ubzStart + 6) / 100.0; // 6 – активная мощность, десятки Вт, переводим в КВт
  ubzList[n].AlarmState := ReadWordInv(buf, ubzStart + 14) +
                          (ReadWordInv(buf, ubzStart + 16) shl 16); // 14 - регистры аварий 901 и 902
end;

procedure TGeomerBlockValues.ReadLongDataWithUBZ(buf: array of byte);
var
  i: int;
begin
  ReadBaseData(buf);

  ubzCount := ReadWord(buf, 36);
  Trace(buf, GEOMER_WITH_UBZ_BLOCK_LEN + ubzCount * GEOMER_WITH_UBZ_DEVICE_LEN);

  ReadLongData(buf);
  gbt := gbtLongUBZ;

  moneyLeft := ReadWord(buf, 34);
  for i := 0 to ubzCount - 1 do
    ReadLongDataWithUBZ_ReadUBZData(buf, i);
end;

procedure TGeomerBlockValues.Read485(const buf: array of byte; cnt: int);
var lenRec: int;
var i: int;
begin
  if cnt < buf[3] + 4 then Exit; // посылка битая

  lenRec := buf[3];
  Trace(buf, lenRec + 4);

  gmTime := NowGM();
  gbt := gbt485;          

  SetLength(FgmBufRec, lenRec);
  for i := 0 to lenRec - 1 do
    FgmBufRec[i] := buf[i + 4];
end;

constructor TGeomerBlockValues.Create;
var
  i: int;
begin
  inherited;

  ReqDetails.Init();
  iscoTime := 0;
  FTraced := false;

  moneyLeft := 0;
  ubzCount := 0;
  for i := 0 to high(ubzList) do
    ZeroMemory(@(ubzList[i]), SizeOf(TGMUBZData));
end;

function TGeomerBlockValues.NeedTrace: bool;
begin
  Result := FindCmdLineSwitch('trace', ['-'], true);
end;

function TGeomerBlockValues.GetBufRecString: AnsiString;
var i: int;
begin
  SetLength(Result, gmLenRec);
  for i := 1 to gmLenRec do
    Result[i] := AnsiChar(gmBufRec[i - 1]);
end;

procedure TGeomerBlockValues.SetBufRec(const bufRec: array of byte; cnt: int = -1);
begin
  if cnt < 0 then
    cnt := Length(bufRec);
  SetLength(FgmBufRec, cnt);
  WriteBuf(FgmBufRec, 0, bufRec, cnt);
end;

procedure TGeomerBlockValues.SetBufRec(const s: AnsiString);
begin
  SetLength(FgmBufRec, Length(s));
  WriteString(FgmBufRec, 0, s);
end;

function TGeomerBlockValues.ToString: string;
begin
  Result := Format('N_Car %d DevNumber %d rqtp %s ID_Obj %d ID_Device %d ID_DevType %d ID_Prm %d ',
                   [ReqDetails.N_Car, ReqDetails.DevNumber, ReqDetails.rqtp.ToString(), ReqDetails.ID_Obj, ReqDetails.ID_Device, ReqDetails.ID_DevType, ReqDetails.ID_Prm]) +
            'buffer ' + ArrayToString(FgmBufRec, GetLenRec, true, true);
end;

end.
