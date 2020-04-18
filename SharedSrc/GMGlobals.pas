////////////////////////////////////////////
// Типы, классы и процедуры общего назначения
////////////////////////////////////////////
unit GMGlobals;

interface
uses SysUtils, Windows, StrUtils, Forms, IniFiles, DateUtils, Math, StdCtrls, Messages, Types, EsLogging,
     CheckLst, Classes, ComCtrls, Graphics, GMConst, Variants, zlib, cxDropDownEdit, cxTextEdit,
     Controls, AnsiStrings, IdGlobal, SvcMgr, cxListBox, WinSock, blcksock;

type int = integer;
     SetOfInt = set of byte;
     ArrayOfByte = array of byte;
     ArrayOfInt = array of int;
     ArrayOfDouble = array of double;
     ArrayOfString = array of string;
     TUtcDateTime = LongWord;

  TTwoBuffers = record
  public
    BufRec, BufSend: array[0..102400] of byte;
    NumberOfBytesRead, LengthSend: int;
    function XmlToBufSend(const xml: string): bool;
    function CheckBufRecHeader(buf0, buf1, cnt: int): bool;
    function RequestUniversal(const xml, login: string; bUseLogin: bool; buf0, buf1: byte): bool;
    procedure SetSendBuf(buf: ArrayOfByte);
    function UniversalResponceLen: int;
    function UniversalResponceXml: string;
  end;

  TVTNodeData = record
  private
    function GetNSrc: int;
    function GetRealObjType: int;
  public
    tLastAccess: TDatetime;
    sTxt: string;
    sOpcTag: string;
    bFixedPT: bool;
    ID_Obj, ObjType, RemoteType, ID_Device, ID_DevType, ID_Prm: int;
    RemoteServerName: string;
    ID_PT, ID_Src, TrueN_Src, ID_MeaUnit, CalibrType, Resistor, DevPortType, MeterCalcNIType, ReqIntervalType, MinIntegralInterval: int;
    CurrentsAddr, AgeAddr, UserSrc, DevMin, DevMax, RealMin, RealMax, CounterCeiling, SourcePrmID, MainSrvPrmID: Variant;
    AlarmSignal: int;
    AlarmThreshold: Variant;
    StartNIUDate: LongWord;
    StartNIValue: Variant;
    HourArchArgument, HourArchAddr: int;
    N_Car, Number485, BaudRate: int;
    ReqPackSize, AddrBase: int;
    sRemoteSrv: string;
    RemotePort: int;
    LastDT: LongWord;
    LastVal: double;
    BaseChn: int;
    Obj: TObject;
    UserData: string;
    Converter: int;
    ModbusResultType: int;

    property N_Src: int read GetNSrc write TrueN_Src;
    property RealObjType: int read GetRealObjType;
  end;
  PVTNodeData = ^TVTNodeData;

  TGMValueType = (valueTypeCurrent, valueTypeDayArch, valueTypeHourArch);
  TValueFromBase = record
    UTime: LongWord;
    Val: double;
    ValType: TGMValueType;
    Chn: TObject;
  end;

  TValueFromBaseItem = class(TCollectionItem)
  public
    Val: TValueFromBase;

    constructor Create(Collection: TCollection); override;
  end;

  TValueFromBaseClass = class
  public
    ID_Prm: int;
    Val: TValueFromBase;
  end;

  TValuesCollection = class(TCollection)
  private
    function GetVal(Index: int): TValueFromBaseItem;
  public
    constructor Create();
    property Vals[Index: int]: TValueFromBaseItem read GetVal; default;
    function Add(): TValueFromBaseItem;
  end;

  TStringClass = class
  public
    s: string;
    constructor Create(str: string);
  end;

  TIntList = class (TList)
  private
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index, Value: Integer);
  public
    function Add(Value: Integer): Integer;
    function IndexOf(Value: Integer): Integer;
    procedure Insert(Index: Integer; Value: Integer);

    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  TUBZEVTLogEntry = class(TCollectionItem)
  public
    DT_UBZ: UINT;
    State: WORD;
    uDT: LongWord;
    bProcessed: bool;
  end;

  TGMStringsHelper = class helper for TStrings
  public
    function DelimitedRande(Start, Count: int): string;
    function IsEmpty(): bool;
  end;

  TGMEnhancedScrollForm = class(TForm)
  public
    procedure MouseWheelHandler(var Message: TMessage); override;
  end;

  TChannelIds = record
    ID_Obj, ObjType, ID_DevType, ID_Src, N_Src, ID_Device, ID_Prm, ID_PT, MainSrvPrmID, UserSrc, AddrBase: int;
    NDevNumber: int;
    PrmExtData: string;
    RemoteName: string;
    function RealSrc(): int;
    function LogIDs: string;
  end;

function MakeValueFromBase(UTime: LongWord; Val: double): TValueFromBase;

function MyFloatToStr(f: double; const sIncorrect: string = '???'): string;
function MyStrToFloat(const s: string): double;
function MyStrToFloatDef(const s: string; def: double=0): double;
function RoundVal(f: double; n: int): double;
function FormatFloatToShow(Val: double; iDigitsAfterPoint: int = 0): string;
function MultilineTextWidth(Canvas: TCanvas; const Txt: string): int;
function IsWord(n: int64): bool;

function ArrayToString(Buf: array of byte; Len: int; b485toStr: bool = false; bHex: bool = false; Separator: string = ' '): string;
procedure StringToArray(const s: AnsiString; var Buf: array of byte);
function StringBufToString(const s: string): string;
function StringArrayToString(const Buf: Array Of Byte; Len: int): string;
function TextNumbersStringToArray(const s: string; bHex: bool = true): ArrayOfByte;
function SetOfIntCommaList(soi: SetOfInt): string;

procedure TryFreeAndNil(var obj);
procedure InitVND(vnd: PVTNodeData);

function IdBytesFromBuf(buf: array of Byte; Count: int): TIdBytes;

function WriteWORD(var buf: array of Byte; nPos, nVal: int): int;
function WriteWORDInv(var buf: array of Byte; nPos, nVal: int): int;
function WriteUINT(var buf: array of Byte; nPos: int; nVal: UINT): int;
function WriteByte(var buf: array of Byte; nPos, nVal: int): int;
function WriteDT(var buf: array of Byte; nPos: int; dVal: TDateTime): int;
function WriteFloat(var buf: array of Byte; nPos: int; fVal: double): int;
function WriteString(var buf: array of Byte; nPos: int; const sVal: AnsiString): int; overload;
function WriteString(var buf: array of Byte; nPos: int; const sVal: string): int; overload;
function WriteStringMd5(var buf: array of Byte; nPos: int; const sVal: string): int;
function WriteBuf(var buf: array of Byte; nPos: int; bufFrom: array of Byte; cnt: int): int;
function WriteZero(var buf: array of Byte; nPos: int; cnt: int): int;

function ReadString(buf: array of Byte; Npos, Len: int): AnsiString;
function ReadMd5String(buf: array of Byte; Npos: int): String;
function ReadWORD(buf: array of Byte; Npos: int): int;
function ReadWORDInv(buf: array of Byte; Npos: int): WORD; // инвертированный WORD
function ReadSmall(buf: array of Byte; Npos: int): SmallInt;
function ReadSmallInv(buf: array of Byte; Npos: int): SmallInt; // инвертированный Int16
function ReadUINT(buf: array of Byte; Npos: int): UINT;
function ReadUINTInv(buf: array of Byte; Npos: int): UINT;
function ReadLongInt(buf: array of Byte; Npos: int): LongInt;
function ReadLongIntInv(buf: array of Byte; Npos: int): LongInt;
function ReadInt64(buf: array of Byte; Npos: int): Int64;
function ReadFloat(buf: array of Byte; nPos: int): double; deprecated;
function ReadDouble(buf: array of Byte; nPos: int): double;
function ReadDoubleInv(buf: array of Byte; nPos: int): double;
function ReadSingleInv(buf: array of Byte; nPos: int): Single;
function ReadSingle(buf: array of Byte; nPos: int): Single;
function ReadDT(buf: array of Byte; nPos: int): TDateTime;

function CompressStringToBuffer(const str: string): ArrayOfByte;
function DecompressBufferToString(const buf: array of byte; start, len: int): AnsiString;

function ReadStringFromFile(const fileName: string): AnsiString;
function ReadStringFromStream(str: TStream): AnsiString;

{
function CompressStream(StreamToCompress: TStream; const password: string = ''): TMemoryStream;
function DECompressStream(StreamToDecompress: TStream; const password: string = ''): TMemoryStream;
}
function ID_SrcToStr(ID_Src: int): string;

function NowUTC(dest_zone: int = DEFAULT_TIME_ZONE): TDateTime;
function NowGM(dest_zone: int = DEFAULT_TIME_ZONE): LongWord;
function UTCtoLocal(UTime: LongWord; dest_zone: int = DEFAULT_TIME_ZONE): TDateTime;
function LocalToUTC(dt: TDateTime; dest_zone: int = DEFAULT_TIME_ZONE): LongWord;
function NowIsSummer(): bool;
function EncodeDateTimeUTC(year, month, day: int; hour: int = 0; minute: int = 0; second: int = 0): LongWord;
function GMMouseWheelHandler(form: TCustomForm; var Message: TMessage): bool;

function LBSelectedObj(cmb: TcxListBox): int;
function CmbSelectedObj(cmb: TcxComboBox): int;
procedure SelectCmbObject(cmb: TcxComboBox; n: int; def: int = -1);
procedure PageControl_HideAllTabs(pc: TPageControl; DefTab: TTabSheet = nil);
function EditToInt(Edit: TcxTextEdit): int;
function EditToDouble(Edit: TcxTextEdit): double;
procedure BlinkControl(c: TWinControl; bSetFocus: bool = false);
procedure RecoursiveEnableControls(c: TWinControl; bEnable: bool);
procedure EnableWithParents(c: TWinControl);

function ShowMessageBox(const Txt: string; Flags: int = MB_OK): int;
function GMPostMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM; logger: TesLogger): bool;
function GetCLBSelCount(clb: TCheckListBox): int;

function SaveStringToFile(const str: string; const fn: string): bool;

function GetTempDir(): string;
function GetTemporaryFileName(const BaseFileName, Prefix: string; bDelExisting: bool = false): string;

procedure GetNamePrefix(const name: string; var prefix: string; var number: int);
function Oem2Ansi(const s: AnsiString): string;
function SplitToThreeLines(const str: string): string;
function IfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString; overload;
function GetGoodLastError(ErrorCode: DWORD = 0): string;
function ExtractFileNameWithoutExt(const fn: string): string;

function FindSwitch(const Switch: string): bool;
function CmdLineSwitchValue(const Switch: string): string;

function CalcMD5(const text: string) : string;
function GenerateGUID(): string;

function RealChnIdSrc(ID_Src, UserSrc: int): int;

type
  TRequestPriority = (rqprNone, rqprCommand, rqprCurrents, rqprArchives);

  TReqLink = record
    id, ext: int;
    procedure Init();
  end;

  TRequestDetails = record
  private
    function IntToLog(const prefix: string; val: int; bAllow0: bool = false): string;
  public
    N_Car, DevNumber, BaudRate, IPPort, Converter: int;
    IP: string;
    ID_Obj, ObjType, ID_Device, ID_DevType, ID_Prm: int;
    UTimeArch: LongWord;
    ReqID: int;
    buf: array [0..1000] of byte;
    BufCnt: int;
    ReqLinks: array[0..MAX_REQ_PACK_SIZE] of TReqLink;
    ReqLinkCount: int;
    rqtp: T485RequestType;
    TimeTick: int64;
    TimeOut: int;
    Priority: TRequestPriority; // Чем больше число, тем ниже приоритет. Запросы с нулевым приоритетом выполняются сразу в основном цикле
                                // С остальными - во время безделья.

    procedure Init();
    function BufToArray(): ArrayOfByte;
    procedure AddReqLink(link: int; ext: int = 0);
    procedure ClearReqLink();
    procedure SetBuffer(ABuf: ArrayOfByte);
    procedure LogRequest();
  end;

  ISmartPointer<T> = reference to function: T;

  TSmartPointer<T: class, constructor> = class(TInterfacedObject, ISmartPointer<T>)
  private
    FValue: T;
    function Invoke: T;
  public
    constructor Create; overload;
    constructor Create(AValue: T); overload;
    destructor Destroy; override;
  end;


  TBlockSocketHelper = class helper for TBlockSocket
  public
    function IsWaitDataSocketError(): bool;
  end;

const
  SQLConst_SelectRemotePrmId =
    '(select ID_Prm from DeviceSystem where RemoteName = %s and PrmRemoteID = %d)';

var TimeZone: int;
    iNoRefreshAlarm: LongWORD;
    iDiagramHrs: WORD;
    COMDevicesRequestInterval: int = 60; // частота опроса устройств по СОМ-порту в секундах

    CommonWaitFirst: int = 1000;
    CommonWaitNext: int = 200;

    RemoteSrvMaxDataValuesCount: int = 1000;

    bProMode: bool = false;

    ThreadForMessagesHandle: THandle = 0;

implementation

uses IdHashMessageDigest, Character, ProgramLogFile;

function RealChnIdSrc(ID_Src, UserSrc: int): int;
begin
  Result := ID_Src;
  if Result = SRC_USR then
    Result := UserSrc;
end;

function IsWord(n: int64): bool;
begin
  Result := (n >= 0) and (n <= $FFFF);
end;

function GMMouseWheelHandler(form: TCustomForm; var Message: TMessage): bool;
var
  Target: TControl;
begin
  // Find the control under the mouse
  Target := FindDragTarget(SmallPointToPoint(TCMMouseWheel(Message).Pos), False);

  while (Target <> nil) do
  begin
    // If the target control is the focused control then we abort as the focused
    // control is the originator of the call to this method.
    if (Target = form) or ((Target is TWinControl) and (TWinControl(Target).Focused)) then
    begin
      Target := nil;
      break;
    end;

    // Let the target control process the scroll. If the control doesn't handle
    // the scroll then...
    Message.Result := Target.Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam);
    if (Message.Result <> 0) then
      break;

    // ...let the target's parent give it a go instead.
    Target := Target.Parent;
  end;

  // Fall back to the default processing if none of the controls under the mouse
  // could handle the scroll.
  Result := Target <> nil;
end;

function GenerateGUID(): string;
var guid: TGUID;
begin
  CreateGUID(guid); // если ф-я не отработала, то в guid какой-то мусор, что нас в целом тоже устраивает
  Result := GUIDToString(guid);
end;

function ExtractFileNameWithoutExt(const fn: string): string;
var ext: string;
begin
  Result := ExtractFileName(fn);
  ext := ExtractFileExt(Result);
  if (ext <> '') and (Length(ext) < Length(result)) then
    Delete(Result, Length(Result) - Length(ext) + 1, Length(ext));
end;

function CalcMD5(const text: string) : string;
var md5: TIdHashMessageDigest5;
begin
  md5 := TIdHashMessageDigest5.Create;
  try
    Result := LowerCase(md5.HashStringAsHex(text, IndyTextEncoding_ASCII()));
  finally
    md5.Free();
  end;
end;

procedure GetNamePrefix(const name: string; var prefix: string; var number: int);
var n: int;
begin
  n := 0;
  while n < name.Length do
  begin
    if not name[name.Length - n].IsDigit() then
      break;

    inc(n);
  end;

  prefix := System.Copy(name, 1, name.Length - n);
  Number := StrToIntDef(System.Copy(name, name.Length - n + 1, name.Length), -1);
end;

function SetOfIntCommaList(soi: SetOfInt): string;
var i: int;
begin
  Result := '';
  for i := 0 to 255 do
  begin
    if i in soi then
    begin
      Result := Result + IfThen(Result <> '', ',') + IntToStr(i);
    end;
  end;
end;

function IdBytesFromBuf(buf: array of Byte; Count: int): TIdBytes;
begin
  SetLength(Result, Count);
  WriteBuf(Result, 0, buf, Count);
end;

function GetGoodLastError(ErrorCode: DWORD = 0): string;
var res: Cardinal;
    lpMsgBuf: PChar;
    code: DWORD;
begin
  Result := '';

  code := ErrorCode;
  if code = 0 then
    code := GetLastError();

  res := FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER + FORMAT_MESSAGE_FROM_SYSTEM, nil, code, LANG_SYSTEM_DEFAULT, @lpMsgBuf, 0, nil);
  if res <> 0 then
    Result := String(lpMsgBuf);

  LocalFree(HLOCAL(lpMsgBuf));
end;

function FindSwitch(const Switch: string): bool;
begin
  Result := FindCmdLineSwitch(Switch, ['-', '/'], True);
end;

function CmdLineSwitchValue(const Switch: string): string;
begin
  FindCmdLineSwitch(Switch, Result, true, [clstValueNextParam]);
end;

function IfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

type TColorE = class (TWinControl)
public
  property Color;
end;

procedure BlinkControl(c: TWinControl; bSetFocus: bool = false);
var i: int;
    e: TColorE;
    p: pointer;
begin
  for i := 0 to 5 do
  begin
    p := c;
    e := p;

    if not odd(i) then
      e.Color := 11448063
    else
      e.Color := clWindow;

    e.Update();
    Sleep(100);
  end;

  if bSetFocus then
    c.SetFocus();
end;

procedure RecoursiveEnableControls(c: TWinControl; bEnable: bool);
var i: int;
begin
  c.Enabled := bEnable;
  for i := 0 to c.ControlCount - 1 do
  begin
    if c.Controls[i] is TWinControl then
      RecoursiveEnableControls(TWinControl(c.Controls[i]), bEnable);
  end;
end;

procedure EnableWithParents(c: TWinControl);
begin
  c.Enabled := true;
  if c.Parent <> nil then
    EnableWithParents(c.Parent);
end;

function SplitToThreeLines(const str: string): string;
var l: TIntList;
    s: string;
    n1, n2, i, len1, len3: int;

    function IsAllowedChar(c: Char): bool;
    begin
      Result := ((c >= '0') and (c <= '9'))
                or ((c >= 'A') and (c <= 'Z'))
                or ((c >= 'a') and (c <= 'z'))
                or ((c >= 'А') and (c <= 'Я'))
                or ((c >= 'а') and (c <= 'я'))
                or (c = '_');
    end;

    function DoSplit(n1, n2: int): string;
    begin
      Result := Trim(Copy(s, 1, n1)) + #13#10 + Trim(Copy(s, n1 + 1, n2 -n1)) + #13#10 + Trim(Copy(s, n2 + 1, Length(s)));
    end;

begin
  s := StringReplace(str, #13#10, ' ', [rfReplaceAll]);
  s := Trim(StringReplace(s, #13, ' ', [rfReplaceAll]));

  l := TIntList.Create();
  i := 1;
  while i < Length(s) do
  begin
    if not IsAllowedChar(s[i]) then
    begin
      while (i < Length(s)) and not IsAllowedChar(s[i]) do
        inc(i);

      if (i > 1) and (i < Length(s)) then
        l.Add(i - 1);
    end;

    inc(i);
  end;

  case l.Count of
    0: Result := s;
    1: Result := Trim(Copy(s, 1, l[0])) + #13#10 + Trim(Copy(s, l[0] + 1, Length(s)));
    2: Result := DoSplit(l[0], l[1]);
    else
      begin
        n1 := 0;
        n2 := 0;
        for i := 0 to l.Count - 1 do
        begin
          if l[i] <= Length(s) div 3 then
            n1 := i;

          if (n2 = 0) and (l[i] >= (Length(s) div 3) * 2) then
            n2 := i;
        end;

        if l[n2] - l[n1] > Length(s) div 2 then
        begin
          // средняя строка получилась длинновата
          len1 := l[n1 + 1];
          len3 := Length(s) - l[n2 - 1];

          if (len1 <= len3) and (len1 < Length(s) div 2) then
            inc(n1);

          if (len3 < len1) and (len3 < Length(s) div 2) then
            dec(n2);
        end;

        Result := DoSplit(l[n1], l[n2]);
      end;
  end;

  l.Free();
end;

function Oem2Ansi(const s: AnsiString): string;
var i:integer;
begin
  i:=Length(s);
  SetLength(Result,i);
  if i=0 then Exit;
  OemToCharBuff(Pointer(S), Pointer(Result), i);
end;

function CompressStringToBuffer(const str: string): ArrayOfByte;
var ms: TMemoryStream;
    compressor: TCompressionStream;
    zippedLen: int;
    s: AnsiString;
begin
  s := AnsiString(str);
  SetLength(Result, 0);
  try try
    ms := TMemoryStream.Create();
    compressor := TCompressionStream.Create(clMax, ms);
    compressor.Write(pointer(s)^, Length(s)); // pointer(s)^ - странная конструкция, но именно так сохраняет себя в Stream, например, TStringList
    FreeAndNil(compressor);

    zippedLen := ms.Size;
    SetLength(Result, zippedLen);
    ms.Seek(0, soFromBeginning);
    ms.Read(pointer(Result)^, zippedLen);
  except end;
  finally
    TryFreeAndNil(compressor);
    TryFreeAndNil(ms);
  end;
end;
{
function CompressStream(StreamToCompress: TStream; const password: string = ''): TMemoryStream;
var VCLZip: TVCLZip;
begin
  VCLZip := TVCLZip.Create();
  try
    Result := TMemoryStream.Create;
    // Point ArchiveStream to your memorystream
    VCLZip.ArchiveStream := Result;
    VCLZip.Password := password;
    // Compress the data from uncompressed stream into the ArchiveStream;
    VCLZip.ZipFromStream(StreamToCompress, 'DummyFileName');
    // Detach your compressed stream from VCLZip
    VCLZip.ArchiveStream := nil;
  finally
    VCLZip.Free();
  end;
end;

function DeCompressStream(StreamToDecompress: TStream; const password: string = ''): TMemoryStream;
var VCLZip: TVCLZip;
begin
  VCLZip := TVCLZip.Create();
  try
    // Point ArchiveStream at the compressed stream
    VCLZip.ArchiveStream := StreamToDecompress;
    VCLZip.Password := password;
    // Create stream to uncompress into
    Result := TMemoryStream.Create;
    // Unzip using the same dummy filename as you used to zip it
    if (StreamToDecompress.Size > 0) and (VCLZip.UnZipToStream( Result, 'DummyFileName') <= 0) then
    begin
      FreeAndnil(Result);
    end;
    // Done with the compressed stream
    VCLZip.ArchiveStream := nil;
  finally
    VCLZip.Free();
  end;
end;
}
procedure CopyStream(Src, Dest: TStream);
var buf: array[0..10240] of Byte;
    n: int;
begin
  Src.Seek(0, soFromBeginning);
  repeat
    n := Src.Read(buf, High(buf));
    Dest.WriteBuffer(buf, n);
  until n = 0;
end;

function ReadStringFromStream(str: TStream): AnsiString;
const bufsize = 4096;
var  buf: packed array[0..bufsize-1] of AnsiChar;
     n: int;
begin
  Result := '';
  str.Seek(0, soFromBeginning);
  n:=str.Read(buf, bufsize);
  while n > 0 do
  begin
    Result := Result + Copy(buf, 0, n);
    n := str.Read(buf, bufsize);
  end;
end;

function ReadStringFromFile(const fileName: string): AnsiString;
var fs: TFileStream;
begin
  Result := '';
  if not FileExists(fileName) then Exit;

  fs := TFileStream.Create(fileName, fmOpenRead and fmShareDenyNone);
  try
    Result := ReadStringFromStream(fs);
  finally
    fs.Free();
  end;
end;

function DecompressBufferToString(const buf: array of byte; start, len: int): AnsiString;
var ms: TMemoryStream;
    decompressor: TDecompressionStream;
begin
  Result := '';
  try try
    ms := TMemoryStream.Create();
    decompressor := TDecompressionStream.Create(ms);
    ms.Write((@buf[start])^, len);
    Result := ReadStringFromStream(decompressor);
  except end;
  finally
    TryFreeAndNil(decompressor);
    TryFreeAndNil(ms);
  end;
end;

function GetTemporaryFileNameEx(const BaseFileName, Prefix: string; const DefDir: string; bDelExisting: bool = false): string;
var
  Dir: string;
  Buffer: array[0..MAX_PATH] of Char;
begin
  { выделяем каталог }
  Dir := ExcludeTrailingPathDelimiter(ExtractFileDir(BaseFileName));
  if Length(Dir) = 0 then Dir := DefDir;
  { генерим имя стандартными средствами }
  if Windows.GetTempFileName(PChar(Dir), PChar(Prefix), 0, @Buffer) <> 0 then
  begin
    // здесть создается временный файл, скорее всего он нам не нужен
    Result := Buffer;

    if bDelExisting and FileExists(Result) then
      SysUtils.DeleteFile(Result);

    Exit;
  end;
  { генерим имя сами }
  repeat
    Result := Format('%s\%s%d.tmp', [Dir, Prefix, Random(MaxWord)]);
  until not FileExists(Result);
end;

function GetTemporaryFileName(const BaseFileName, Prefix: string; bDelExisting: bool = false): string;
begin
  Result := GetTemporaryFileNameEx(BaseFileName, Prefix, ExtractFileDir(BaseFileName), bDelExisting);
end;

function GetTempDir: string;
var Buffer: array[0..1023] of Char;
begin
  SetString(Result, Buffer, GetTempPath(Length(Buffer), Buffer));
  Result:=ExcludeTrailingPathDelimiter(Result);
end;

function MultilineTextWidth(Canvas: TCanvas; const Txt: string): int;
var i, r: int;
    s, s1, s2: string;
begin
  r := 0;
  s := Txt;
  s := StringReplace(s, #10, '', [rfReplaceAll]);
  for i := 1 to Length(s) do
    if s[i] = #13 then
    begin
      r := i;
      break;
    end;

  if r > 0 then
  begin
    s1 := Copy(s, 1, r - 1);
    s2 := Copy(s, r + 1, Length(s));
    Result := max(Canvas.TextWidth(s1), MultilineTextWidth(Canvas, s2));
  end
  else
   Result := Canvas.TextWidth(s);
end;

function EditToInt(Edit: TcxTextEdit): int;
begin
  if Trim(Edit.Text) = '' then
    Result := 0
  else
  try
    Result := StrToInt(Edit.Text);
  except
    TCustomEdit(Edit).SetFocus();
    raise;
  end;
end;

function EditToDouble(Edit: TcxTextEdit): double;
begin
  if Trim(Edit.Text) = '' then
    Result := 0
  else
  try
    Result := MyStrToFloat(Edit.Text);
  except
    TCustomEdit(Edit).SetFocus();
    raise;
  end;
end;

function FormatFloatToShow(Val: double; iDigitsAfterPoint: int = 0): string;
begin
  if iDigitsAfterPoint = 0 then // это автоматический расчет количества цифр после запятой
  begin
    if Val < 10 then
      Result := MyFloatToStr(RoundVal(Val, 1))
    else
      Result := IntToStr(Round(Val));
  end
  else
    Result := MyFloatToStr(RoundVal(Val, iDigitsAfterPoint - 1));

  if Length(Result) = 1 then
    Result := ' ' + Result; // отступ от левого края
end;

function AlarmKey(n: int): string;
begin
  Result := 'ALARM' + IntToStr(n) + '_ID_PRM';
end;

function HydroGeoRepSection(i: int): string;
begin
  Result := 'HYDROGEOREP' + IntToStr(i);
end;

procedure PageControl_HideAllTabs(pc: TPageControl; DefTab: TTabSheet = nil);
var i: int;
begin
  for i := 0 to pc.PageCount - 1 do
    pc.Pages[i].TabVisible := false;

  if DefTab <> nil then
    pc.ActivePage := DefTab
  else
    if pc.PageCount > 0 then
      pc.ActivePageIndex := 0;
end;

procedure InitVND(vnd: PVTNodeData);
begin
  vnd.ID_Obj := 0;
  vnd.sTxt := '';
  vnd.sOpcTag := '';
  vnd.ObjType := OBJ_TYPE_GM;
  vnd.RemoteType := vnd.ObjType;
  vnd.ID_Device := 0;
  vnd.ID_DevType := 0;
  vnd.ID_Prm := 0;
  vnd.N_Car := 0;
  vnd.Number485 := 0;
  vnd.DevPortType := 0;
  vnd.ID_PT := 0;
  vnd.ID_Src := 0;
  vnd.N_Src := 0;
  vnd.ID_MeaUnit := 0;
  vnd.DevMin := Null;
  vnd.DevMax := Null;
  vnd.RealMin := Null;
  vnd.RealMax := Null;
  vnd.CalibrType := CALIBR_TYPE_MANUAL;
  vnd.Resistor := 0;
  vnd.AlarmSignal := 0;
  vnd.AlarmThreshold := Null;
  vnd.StartNIUDate := 0;
  vnd.StartNIValue := Null;
  vnd.bFixedPT := false;
  vnd.tLastAccess := 0;
  vnd.sRemoteSrv := '';
  vnd.RemoteServerName := '';
  vnd.RemotePort := 0;
  vnd.BaseChn := 0;
  vnd.Obj := nil;
  vnd.UserData := '';
end;

function ID_SrcToStr(ID_Src: int): string;
begin
  case ID_Src of
    SRC_AI: Result := 'AI';
    SRC_AO: Result := 'AO';
    SRC_DI: Result := 'DI';
    SRC_DO: Result := 'DO';
    SRC_CNT_DI: Result := 'CNT';
    SRC_SUM_AI: Result := 'SUM';
    SRC_USR: Result := 'USR';
    SRC_FK: Result := 'FK';
    SRC_CNT_MTR: Result := 'MTR';

    else Result := '';
  end;
end;

function GetCLBSelCount(clb: TCheckListBox): int;
var i: int;
begin
  Result:=0;
  for i:=0 to clb.Count-1 do
    if clb.Checked[i] then
      inc(Result);
end;

function MakeValueFromBase(UTime: LongWord; Val: double): TValueFromBase;
begin
  Result.UTime:=UTime;
  Result.Val:=Val;
end;

function SaveStringToFile(const str: string; const fn: string): bool;
var stream: TStringStream;
begin
  Result := false;
  if FileExists(fn) and not SysUtils.DeleteFile(fn) then Exit;

  stream := TStringStream.Create(str);
  try
    stream.Seek(0, soFromBeginning);
    stream.SaveToFile(fn);
    Result := true;
  except end;
  stream.Free();
end;

function PreDate(Value: TDateTime):int64;
begin
 Result:=round(Value/encodetime(0,0,1,0));
end;

{Выяснение текущего периода}
function NowIsSummer: bool;
var zi: _TIME_ZONE_INFORMATION;
begin
 result:=GetTimeZoneInformation(zi)=TIME_ZONE_ID_DAYLIGHT;
end;

procedure GetDaylightAndStandartDates(y: WORD;
                                      var StandardDateTime: TDateTime;
                                      var SummerDateTime: TDateTime);
var zi: _TIME_ZONE_INFORMATION;
    i,NumWeek,d1,d2: integer;
begin
 GetTimeZoneInformation(zi);

 StandardDateTime:=0;
 SummerDateTime:=0;

 if zi.StandardDate.wYear<>0 then
   // Дата указана точно
   StandardDateTime:=encodedate(zi.StandardDate.wYear,zi.StandardDate.wMonth,zi.StandardDate.wDay)+
    encodetime(zi.standarddate.wHour,zi.standarddate.wMinute,zi.standarddate.wSecond,zi.standarddate.wMilliseconds)
 else
 begin
   NumWeek:=0;
   d1:=trunc(encodedate(Y,zi.StandardDate.wMonth,1));
   d2:=trunc(incmonth(encodedate(Y,zi.StandardDate.wMonth,1),1)-1);
   for i:=d1 to d2 do
    if DayOfWeek(i)-1=zi.StandardDate.wDayOfWeek then
     begin
      inc(NumWeek);
      StandardDateTime:=i;
      if NumWeek=zi.StandardDate.wDay then Break;
     end;
   StandardDateTime:=StandardDateTime+
    encodetime(zi.standarddate.wHour,zi.standarddate.wMinute,zi.standarddate.wSecond,zi.standarddate.wMilliseconds);
 end;

 if zi.DaylightDate.wYear<>0 then
    SummerDateTime:=encodedate(zi.DaylightDate.wYear,zi.DaylightDate.wMonth,zi.DaylightDate.wDay)+
                    encodetime(zi.DaylightDate.wHour,zi.DaylightDate.wMinute,zi.DaylightDate.wSecond,zi.DaylightDate.wMilliseconds)
 else
 begin
   NumWeek:=0;
   d1:=trunc(encodedate(Y,zi.DaylightDate.wMonth,1));
   d2:=trunc(incmonth(encodedate(Y,zi.DaylightDate.wMonth,1),1)-1);
   for i:=d1 to d2 do
    if DayOfWeek(i)-1=zi.DaylightDate.wDayOfWeek then
    begin
      inc(NumWeek);
      SummerDateTime:=i;
      if NumWeek=zi.DaylightDate.wDay then Break;
    end;

   SummerDateTime:=SummerDateTime+
                   encodetime(zi.DaylightDate.wHour,zi.DaylightDate.wMinute,zi.DaylightDate.wSecond,zi.DaylightDate.wMilliseconds);
 end;
end;

function LookForTService(c: TComponent; logger: TEsLogger): TComponent;
var i: int;
begin
  Result := nil;
  logger.Debug('LookForTService' + c.ClassName);
  if c is TService then
  begin
    Result := c;
    Exit;
  end;

  for i := 0 to c.ComponentCount - 1 do
  begin
    Result := LookForTService(c.Components[i], logger);
    if Result <> nil then Exit;
  end;
end;

function GMPostMessage(Msg: UINT; wParam: WPARAM; lParam: LPARAM; logger: TesLogger): bool;
var c, app: TComponent;
begin
  if ThreadForMessagesHandle > 0 then
  begin
    Result := PostThreadMessage(ThreadForMessagesHandle, Msg, wParam, lParam);
    Exit;
  end;

  logger.Debug('GMPostMessage');
  app := {$ifdef Application}Forms{$else}SvcMgr{$endif}.Application;
  if app <> nil then
  begin
    c := LookForTService(app, logger);
    if (c <> nil) and (c is TService) then
    begin
      Result := PostThreadMessage(TService(c).Tag, Msg, wParam, lParam);
      logger.Debug('GMPostMessage(TService)');
      Exit;
    end;
  end;

  if (Forms.Application <> nil) and (Forms.Application.MainForm <> nil) then
  begin
    Result := PostMessage(Forms.Application.MainForm.Handle, Msg, wParam, lParam);
    Exit;
  end;

  logger.Error('GMPostMessage: Service component not found!');

  Result := false;
end;

function ShowMessageBox(const Txt: string; Flags: int = MB_OK): int;
begin
  Result:=Forms.Application.MessageBox(PChar(Txt), PChar(Application.Title), Flags);
end;

function LBSelectedObj(cmb: TcxListBox): int;
begin
  if cmb.ItemIndex<0 then
    Result := -1
  else
    Result := int(cmb.Items.Objects[cmb.ItemIndex])
end;

function CmbSelectedObj(cmb: TcxComboBox): int;
begin
  if cmb.ItemIndex<0 then
    Result := -1
  else
    Result := int(cmb.Properties.Items.Objects[cmb.ItemIndex])
end;

procedure SelectCmbObject(cmb: TcxComboBox; n: int; def: int = -1);
var ii: int;
begin
  ii:=cmb.Properties.Items.IndexOfObject(TObject(n));
  if ii<0 then
    cmb.ItemIndex:=def
  else
    cmb.ItemIndex:=ii;
end;

function UTC_Zero(): TDateTime;
begin
  Result := EncodeDate(2000, 1, 1);
end;

function UTCtoLocal(UTime: LongWord; dest_zone: int = DEFAULT_TIME_ZONE): TDateTime;
begin
  if UTime = 0 then
    Result := 0
  else
    Result := UTC_Zero() + Utime * OneSecond + IfThen(dest_zone = DEFAULT_TIME_ZONE, TimeZone, dest_zone) * 60 * OneMinute;
end;

function LocalToUTC(dt: TDateTime; dest_zone: int = DEFAULT_TIME_ZONE): LongWord;
begin
  if dt <= UTC_Zero() then
    Result := 0
  else
    Result := Round((dt - UTC_Zero()) / OneSecond) - IfThen(dest_zone = DEFAULT_TIME_ZONE, TimeZone, dest_zone) * 60 * 60;
end;

function EncodeDateTimeUTC(year, month, day: int; hour: int = 0; minute: int = 0; second: int = 0): LongWord;
begin
  Result := LocalToUTC(EncodedateTime(year, month, day, hour, minute, second, 0));
end;

function NowUTC(dest_zone: int = DEFAULT_TIME_ZONE): TDateTime;
begin
  Result := Now() - IfThen(dest_zone = DEFAULT_TIME_ZONE, TimeZone, dest_zone) * 60 * OneMinute;
end;

function NowGM(dest_zone: int = DEFAULT_TIME_ZONE): LongWord;
begin
  Result := Trunc((NowUTC(dest_zone) - UTC_Zero()) / OneSecond);
end;

function RoundVal(f: double; n: int): double;
var i: int;
    p: int64;
begin
  p := 1;
  for  i := 1 to n do
    p := p * 10;

  Result:=Round(f * p) / p
end;

function CommaToDot(const s: string): string;
begin
  Result := StringReplace(s, ',', '.', []);
  Result := StringReplace(Result, FormatSettings.DecimalSeparator, '.', []);
end;

function CommaToDecimalSeparator(const s: string): string;
begin
  Result := StringReplace(s, ',', FormatSettings.DecimalSeparator, []);
  Result := StringReplace(Result, '.', FormatSettings.DecimalSeparator, []);
end;

function MyFloatToStr(f: double; const sIncorrect: string = '???'): string;
begin
  if f=INCORRECT_VALUE then
    Result:=sIncorrect
  else
  begin
    Result:=CommaToDot(FloatToStr(f));
  end;
end;

function MyStrToFloat(const s: string): double;
var c: int;
begin
  if (Trim(s) = '') or (Pos('?', s) > 0) then
    Result := INCORRECT_VALUE
  else
  begin
    Val(s, Result, c);
    if c <> 0 then
    begin
      Val(CommaToDot(s), Result, c);
      if c <> 0 then
        Result := INCORRECT_VALUE
    end;
  end;
end;

function MyStrToFloatDef(const s: string; def: double=0): double;
var res: Extended;
begin
  if (Trim(s)='') or (Pos('?', s)>0) then
    Result:=def
  else
  if TextToFloat(PChar(CommaToDecimalSeparator(s)), res, fvExtended) then
    Result := res
  else
    Result:=def;
end;

function ArrayToString(Buf: array of byte; Len: int; b485toStr: bool = false; bHex: bool = false; Separator: string = ' '): string;
var i, nStart: int;
    s: string;
    b, b1, b2: byte;
    c, c1, c2: AnsiChar;
    txt: AnsiString;
    wordLen, separatorLen: int;
begin
  if bHex then
  begin
    separatorLen := Min(Length(Separator), 1);
    wordLen := 2 + Length(Separator);
    SetLength(s, Len * wordLen - separatorLen);
    for i := 0 to Len - 1 do
    begin
      if (i > 0) and (separatorLen > 0) then
        s[i * wordLen] := Separator[1];

      b1 := buf[i] div $10;
      b2 := buf[i] mod $10;

      if b1 > 9 then
        c1 := 'A'
      else
        c1 := '0';

      if b2 > 9 then
        c2 := 'A'
      else
        c2 := '0';

      b1 := b1 mod 10;
      b2 := b2 mod 10;

      s[i * wordLen + 1] := Char(AnsiChar(Ord(c1) + b1));
      s[i * wordLen + 2] := Char(AnsiChar(Ord(c2) + b2));
    end;
  end
  else
  begin
    s := '';
    for i := 0 to Len - 1 do
      s := s + IfThen(i > 0, Separator, '') + IntToStr(buf[i]);
  end;

  Result := s;

  if b485toStr then
  begin
    nStart := IfThen((Len >= 2) and (Buf[0] = 85) and (Buf[1] = 133), 4, 0);

    SetLength(txt, Len - nStart);
    for i := nStart to Len - 1 do
    begin
      b := buf[i];
      if b < 32 then
        c := '|'
      else
        c := AnsiChar(b);

      txt[i - nStart + 1] := c;
    end;

    Result := Result + ' (' + Oem2Ansi(txt) + ')';
  end;

end;

procedure StringToArray(const s: AnsiString; var Buf: array of byte);
var i: int;
begin
  for i := 1 to Min(Length(s), Length(Buf)) do
    buf[i - 1] := Ord(s[i]);
end;

function StringBufToString(const s: string): string;
var buf: array[0..1000] of byte;
    n: int;
begin
  n := Min(Length(s), High(buf));
  StringToArray(AnsiString(Copy(s, 1, n)), buf);
  Result := ArrayToString(buf, n, false, true);
end;

function StringArrayToString(const Buf: Array Of Byte; Len: int): string;
var i: int;
    res: AnsiString;
begin
  res := '';
  for i := 0 to Len - 1 do
    res := res + AnsiChar(Buf[i]);

  Result := string(res);
end;

function TextNumbersStringToArray(const s: string; bHex: bool = true): ArrayOfByte;
var sl: TStringList;
    i: int;
begin
  sl := TStringList.Create();
  try
    sl.CommaText := s;

    for i := sl.Count - 1 downto 0 do
      if Trim(sl[i]) = '' then sl.Delete(i);

    SetLength(result, sl.Count);
    for i := 0 to sl.Count - 1 do
      Result[i] := StrToInt(Ifthen(bHex and (sl[i][1] <> '$'), '$') + sl[i]);
  finally
    sl.Free();
  end;
end;

procedure TryFreeAndNil(var obj);
begin
  try
    if TObject(obj) <> nil then FreeAndNil(obj);
  except end;
end;

function ReadString(buf: array of Byte; Npos, Len: int): AnsiString;
var i: int;
begin
  Result := '';
  for i := NPos to NPos + Len - 1 do
    Result := Result + AnsiChar(buf[i]);
end;

function ReadMd5String(buf: array of Byte; Npos: int): String;
var md5buf: array [0..15] of byte;
begin
  Move(buf[Npos], md5buf[0], 16);
  Result := LowerCase(ArrayToString(md5buf, 16, false, true, ''));
end;

function ReadWORD(buf: array of Byte; Npos: int): int;
begin
  Result := buf[nPos] + buf[nPos + 1] * 256;
end;

function ReadWORDInv(buf: array of Byte; Npos: int): WORD;
begin
  Result := buf[nPos] * 256 + buf[nPos + 1];
end;

function WordToSmall(w: word): SmallInt;
begin
  if w > $7FFF then
    Result := w - $10000
  else
    Result := w;
end;

function ReadSmall(buf: array of Byte; Npos: int): SmallInt;
begin
  Result := WordToSmall(ReadWORD(buf, Npos));
end;

function ReadSmallInv(buf: array of Byte; Npos: int): SmallInt;
begin
  Result := WordToSmall(ReadWORDInv(buf, Npos));
end;

function ReadInt64(buf: array of Byte; Npos: int): Int64;
begin
  CopyMemory(@Result, @buf[nPos], 8);
end;

function ReadUINT(buf: array of Byte; Npos: int): UINT;
begin
  CopyMemory(@Result, @buf[nPos], 4);
end;

function ReadLongInt(buf: array of Byte; Npos: int): LongInt;
begin
  CopyMemory(@Result, @buf[nPos], 4);
end;

function ReadLongIntInv(buf: array of Byte; Npos: int): LongInt;
var dw: UINT;
begin
  dw := ReadUINTInv(buf, nPos);
  CopyMemory(@Result, @dw, 4);
end;

function ReadUINTInv(buf: array of Byte; Npos: int): UINT;
var bufTmp: array [0..3] of byte;
begin
  bufTmp[0] := buf[Npos + 3];
  bufTmp[1] := buf[Npos + 2];
  bufTmp[2] := buf[Npos + 1];
  bufTmp[3] := buf[Npos];

  CopyMemory(@Result, @bufTmp, 4);
end;

function ReadSingleInv(buf: array of Byte; nPos: int): Single;
var dw: UINT;
begin
  dw := ReadUINTInv(buf, nPos);
  CopyMemory(@Result, @dw, 4);
end;

function ReadSingle(buf: array of Byte; nPos: int): Single;
begin
  CopyMemory(@Result, @buf[nPos], 4);
end;

function ReadFloat(buf: array of Byte; nPos: int): double;
begin
  Result := ReadDouble(buf, nPos);
end;

function ReadDouble(buf: array of Byte; nPos: int): double;
begin
  CopyMemory(@Result, @buf[nPos], 8);
end;

function ReadDoubleInv(buf: array of Byte; nPos: int): double;
var bufTmp: array [0..7] of byte;
    i: int;
begin
  for i := 0 to 7 do
    bufTmp[i] := buf[nPos + 7 - i];

  Result := ReadDouble(bufTmp, 0);
end;

function ReadDT(buf: array of Byte; nPos: int): TDateTime;
var Y, M, D, h, n, s, z: WORD;
begin
  Y:=ReadWORD(buf, nPos);
  M:=buf[nPos+2];
  D:=buf[nPos+3];
  h:=buf[nPos+4];
  n:=buf[nPos+5];
  s:=buf[nPos+6];
  z:=ReadWORD(buf, nPos+7);

  if not TryEncodeDateTime(Y, M, D, h, n, s, z, Result) then
    Result:=0;
end;

function WriteWORD(var buf: array of Byte; nPos, nVal: int): int;
begin
  if (nVal<0) or (nVal>65535) then
    nVal:=0;

  buf[nPos]:=nVal mod 256;
  buf[nPos+1]:=nVal div 256;

  Result:=nPos+2;
end;

function WriteWORDInv(var buf: array of Byte; nPos, nVal: int): int;
begin
  if (nVal<0) or (nVal>65535) then
    nVal:=0;

  buf[nPos]:=nVal div 256;
  buf[nPos+1]:=nVal mod 256;

  Result:=nPos+2;
end;

function WriteUINT(var buf: array of Byte; nPos: int; nVal: UINT): int;
begin
  CopyMemory(@buf[nPos], @nVal, 4);
  Result:=nPos+4;
end;

function WriteByte(var buf: array of Byte; nPos, nVal: int): int;
begin
  if (nVal<0) or (nVal>255) then
    nVal:=0;

  buf[nPos]:=nVal;
  Result:=nPos+1;
end;

function WriteDT(var buf: array of Byte; nPos: int; dVal: TDateTime): int;
var Y, M, D, h, n, s, z: WORD;
begin
  DecodeDateTime(dVal, Y, M, D, h, n, s, z);

  Result:=WriteWORD(buf, nPos, Y);
  Result:=WriteByte(buf, Result, M);
  Result:=WriteByte(buf, Result, D);
  Result:=WriteByte(buf, Result, h);
  Result:=WriteByte(buf, Result, n);
  Result:=WriteByte(buf, Result, s);
  Result:=WriteWORD(buf, Result, z);
end;

function WriteFloat(var buf: array of Byte; nPos: int; fVal: double): int;
begin
  CopyMemory(@buf[nPos], @fVal, 8);
  Result:=nPos+8;
end;

function WriteString(var buf: array of Byte; nPos: int; const sVal: AnsiString): int;
var i: int;
begin
  for i:=1 to Length(sVal) do
    buf[nPos+i-1]:=Ord(sVal[i]);

  Result:=nPos+Length(sVal);
end;

function WriteString(var buf: array of Byte; nPos: int; const sVal: string): int;
begin
  Result := WriteString(buf, nPos, AnsiString(sVal));
end;

function WriteStringMd5(var buf: array of Byte; nPos: int; const sVal: string): int;
var i: int;
    s: string;
begin
  s := CalcMd5(sVal);
  Result := nPos;
  for i := 0 to Length(s) div 2 - 1 do
    Result := WriteByte(buf, Result, StrToInt('$' + Copy(s, i * 2 + 1, 2)));
end;

function WriteBuf(var buf: array of Byte; nPos: int; bufFrom: array of Byte; cnt: int): int;
var i: int;
begin
  for i := Low(bufFrom) to Min(cnt - 1, High(BufFrom)) do
    buf[nPos + i - Low(bufFrom)] := bufFrom[i];

  Result := nPos + cnt;
end;

function WriteZero(var buf: array of Byte; nPos: int; cnt: int): int;
var i: int;
begin
  Result := min(nPos + cnt, Length(buf));

  for i := nPos to Result - 1  do
    buf[i] := 0;
end;

{ TStringClass }

constructor TStringClass.Create(str: string);
begin
  s:=str;
end;

{ TValuesCollection }

function TValuesCollection.Add: TValueFromBaseItem;
begin
  Result := TValueFromBaseItem(inherited Add());
end;

constructor TValuesCollection.Create;
begin
  inherited Create(TValueFromBaseItem);
end;

function TValuesCollection.GetVal(Index: int): TValueFromBaseItem;
begin
  Result := TValueFromBaseItem(Items[Index]);
end;

{ TValueFromBaseItem }

constructor TValueFromBaseItem.Create(Collection: TCollection);
begin
  inherited;

  Val.UTime := 0;
  Val.Val := 0;
  Val.ValType := valueTypeCurrent;
  Val.Chn := nil;
end;

var TZ: TTimeZoneInformation;

{TIntList}

function TIntList.GetItem(Index: Integer): Integer;
begin
  Result:=Integer(inherited Items[Index]);
end;

procedure TIntList.SetItem(Index, Value: Integer);
begin
  inherited Items[Index]:=TObject(Value);
end;

function TIntList.Add(Value: Integer): Integer;
begin
  Result:=inherited Add(TObject(Value));
end;

function TIntList.IndexOf(Value: Integer): Integer;
begin
  Result:=inherited IndexOf(TObject(Value));
end;

procedure TIntList.Insert(Index: Integer; Value: Integer);
begin
  inherited Insert(Index, TObject(Value));
end;

{ TGMStringListHelper }

function TGMStringsHelper.DelimitedRande(Start, Count: int): string;
var i: int;
begin
  Result := '';
  for i := Start to Start + Count - 1 do
  begin
    if i >= Count then
      break;

    Result := Result + IfThen(Result <> '', Delimiter) + Strings[i];
  end;
end;

function TGMStringsHelper.IsEmpty: bool;
var i: int;
begin
  Result := true;

  for i := 0 to Count - 1 do
  begin
    if Trim(Strings[i]) <> '' then
    begin
      Result := false;
      Exit;
    end;
  end;
end;

{ TRequestDetails }

procedure TRequestDetails.AddReqLink(link: int; ext: int = 0);
begin
  ReqLinks[ReqLinkCount].id := link;
  ReqLinks[ReqLinkCount].ext := ext;
  inc(ReqLinkCount);
end;

function TRequestDetails.BufToArray: ArrayOfByte;
var i: int;
begin
  SetLength(Result, BufCnt);
  for i := 0 to BufCnt - 1 do
    Result[i] := buf[i];
end;

procedure TRequestDetails.ClearReqLink;
var i: int;
begin
  for i := 0 to High(ReqLinks) do
    ReqLinks[i].Init();

  ReqLinkCount := 0;
end;

procedure TRequestDetails.Init;
begin
  ObjType := OBJ_TYPE_COM;
  ID_Prm := 0;
  ID_Obj := 0;
  ID_Device := 0;
  ID_DevType := 0;
  DevNumber := 0;
  ObjType := 0;
  N_Car := 0;
  Converter := 0;
  TimeOut := 0;
  Priority := rqprNone;
  UTimeArch := 0;
  ReqID := -1;
  ClearReqLink();
  BufCnt := 0;
  rqtp := rqtNone;
  ClearReqLink();
end;

function TRequestDetails.IntToLog(const prefix: string; val: int; bAllow0: bool = false): string;
begin
  Result := '';
  if (val > 0) or (bAllow0 and (val = 0)) then
    Result := ' ' + prefix + ' = ' + IntToStr(val);
end;

procedure TRequestDetails.LogRequest;
var s: string;
begin
  s := 'Request' +
       IntToLog('N_Car', N_Car) +
       IntToLog('DevNumber', DevNumber, true) +
       IntToLog('ID_Obj', ID_Obj) +
       IntToLog('ID_Device', ID_Device) +
       IntToLog('ID_DevType', ID_DevType) +
       IntToLog('ID_Prm', ID_Prm) +
       IntToLog('ObjType', ObjType, true) +
       ' rqtp = ' + rqtp.ToString() +
       ' buf = ' + ArrayToString(buf, BufCnt, false, true);

  ProgramLog.AddMessage(s);
end;

procedure TRequestDetails.SetBuffer(ABuf: ArrayOfByte);
var i: int;
begin
  BufCnt := Length(ABuf);
  for i := 0 to BufCnt - 1 do
    buf[i] := ABuf[i];
end;

{ TVTNodeData }

function TVTNodeData.GetNSrc: int;
begin
  Result := IfThen((ID_Src in UserDefinedParamTypes) and not bFixedPT and (BaseChn <= 0), CurrentsAddr, TrueN_Src);
end;

function TVTNodeData.GetRealObjType: int;
begin
  Result := IfThen(ObjType = OBJ_TYPE_REMOTE_SRV_XML, RemoteType, ObjType);
end;

{ TTwoBuffers }

function TTwoBuffers.CheckBufRecHeader(buf0, buf1, cnt: int): bool;
begin
  Result := (self.NumberOfBytesRead >= cnt) and (self.BufRec[0] = buf0) and (self.BufRec[1] =  buf1);
end;

function TTwoBuffers.XmlToBufSend(const xml: string): bool;
begin
  Result := RequestUniversal(xml, '', false, BufSend[0], BufSend[1]);
end;

function TTwoBuffers.RequestUniversal(const xml, login: string; bUseLogin: bool; buf0, buf1: byte): bool;
var buf: ArrayOfByte;
    zippedLen, dataStart: int;
begin
  BufSend[0] := buf0;
  BufSend[1] := buf1;

  if bUseLogin then
  begin
    WriteStringMd5(BufSend, 2, login);
    dataStart := 18;
  end
  else
  begin
    dataStart := 2;
  end;

  buf := CompressStringToBuffer(xml);
  zippedLen := Length(buf);
  if dataStart + zippedLen + 4 >= Length(self.BufSend) then
  begin
    Result := false;
    ProgramLog.AddError('RequestUniversal - xml too big');
    Exit;
  end;

  Result := zippedLen > 0;
  WriteUINT(BufSend, dataStart, zippedLen);
  LengthSend := WriteBuf(BufSend, dataStart + 4, buf, zippedLen);
end;

function TTwoBuffers.UniversalResponceLen(): int;
var
  len: int;
begin
  Result := -1;
  if NumberOfBytesRead < 22 then
    Exit;

  len := ReadUINT(BufRec, 18);
  if (len > 0) and (NumberOfBytesRead >= len + 22) then
    Result := len;
end;

function TTwoBuffers.UniversalResponceXml: string;
var
  len: int;
begin
  len := UniversalResponceLen;
  if len <= 0 then
    Exit('');

  Result := string(DecompressBufferToString(BufRec, 22, len));
end;

procedure TTwoBuffers.SetSendBuf(buf: ArrayOfByte);
begin
  LengthSend := Length(buf);
  WriteBuf(BufSend, 0, buf, LengthSend);
end;

{ TGMEnhancedScrollForm }

procedure TGMEnhancedScrollForm.MouseWheelHandler(var Message: TMessage);
begin
  if not GMMouseWheelHandler(self, Message) then
    inherited;
end;

{ TReqLink }

procedure TReqLink.Init;
begin
  id := 0;
  ext := 0;
end;

{ TChannelIds }

function TChannelIds.RealSrc: int;
begin
  Result := RealChnIdSrc(ID_Src, UserSrc);
end;

function TChannelIds.LogIDs(): string;
begin
  Result := 'ID_Prm = ' + IntToStr(ID_Prm) +
            ', ID_Src = ' + IntToStr(ID_Src) +
            ', N_Src = ' + IntToStr(N_Src) +
            ', ID_Device = ' + IntToStr(ID_Device) +
            ', ID_DevType = ' + IntToStr(ID_DevType) +
            ', NDevNumber = ' + IntToStr(NDevNumber) +
            ', ID_Obj = ' + IntToStr(ID_Obj) +
            ', ObjType = ' + IntToStr(ObjType);
end;

{ TSmartPointer<T> }

constructor TSmartPointer<T>.Create;
begin
  inherited Create;
  FValue := T.Create;
end;

constructor TSmartPointer<T>.Create(AValue: T);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TSmartPointer<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TSmartPointer<T>.Invoke: T;
begin
  Result := FValue;
end;

{ TBlockSocketHelper }

function TBlockSocketHelper.IsWaitDataSocketError: bool;
begin
  Result := (LastError <> 0) and (LastError <> WSAETIMEDOUT);
end;

initialization
  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT: TimeZone := -(TZ.Bias + TZ.DaylightBias) div 60;
    TIME_ZONE_ID_INVALID: TimeZone := 6;
    else TimeZone := -TZ.Bias div 60;
  end;

  NullStrictConvert := false;
end.
