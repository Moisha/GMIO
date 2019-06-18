/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009 FlexGraphics software.  //
//                                                     //
//    Utility procedures and functions                 //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexUtils;

{$I FlexDefs.inc}

interface

uses
  Windows, Forms, Dialogs, Controls, Messages, Classes, 
  {$IFDEF FG_D6} Variants, RTLConsts, {$ENDIF} SysUtils,
  TypInfo, Graphics, ClipBrd, Consts;

const
  // FlexGraphics document clipboard format
  CF_FLEXDOC : Word = 0;

  // Reserved word's in fxd format
  fcDocument  = 'document';
  fcClipboard = 'clipboard';
  fcLibrary   = 'library';
  fcObject    = 'object';
  fcProperty  = 'property';
  fcEnd       = 'end';
  fcBinary    = 'flexbinary';

  // array of all reserved words
  fcReserved: array[0..6] of string = (
    fcDocument, fcClipboard, fcLibrary, fcObject, fcProperty, fcEnd, fcBinary );

  IndentStep  = '  ';

  BooleanWords: array[boolean] of string = ( 'False', 'True' );

  // Flex binary commands
  fbcUnknown           = $0000; // Not a commmand (unknown command)

  fbcDocument          = $0001; // Key0: Document name, No data
  fbcClipboard         = $0002; // No keys, No data
  fbcLibrary           = $0003; // Key0: Library name, No data
  fbcObject            = $0004; // Key0: Object name, Key1: Class name, No data
  fbcProperty          = $0005; // Key0: Property name, [Key1: property value],
                                // DataBlock: TFlexBinPropertyData
  fbcComplexProperty   = $0006; // Key0: Property name, No data
  fbcEnd               = $0007; // No keys, No data
  fbcReduceDictionary  = $0008; // No keys,
                                // DataBlock: TFlexBinReduceDictionaryData
  fbcBinaryEnd         = $0009; // No keys, No data

  fbcUser              = $1000; // 0..65535 keys, [DataBlock: User type]

  // Flex binary key flags
  fbkIndexSizeMask     = $03; // 2 bits
   fbkIndexByte        = $01; // Next number is byte
   fbkIndexWord        = $02; // Next number is word
   fbkIndexDWord       = $03; // Next number is double word
  fbkIsIndex           = $04; // 1 bit - Next number is index in keys dictinary
                              //         Else is length of the following
                              //         key string
  fbkDontStore         = $08; // 1 bit - This new key must not be stored in
                              //         keys dictionary

  // FlexBinary supported properties types
  fbpEmpty             = $0000; { vt_empty        0 }
  fbpNull              = $0001; { vt_null         1 }
  fbpSmallint          = $0002; { vt_i2           2 }
  fbpInteger           = $0003; { vt_i4           3 }
  fbpSingle            = $0004; { vt_r4           4 }
  fbpDouble            = $0005; { vt_r8           5 }
  fbpCurrency          = $0006; { vt_cy           6 }
  fbpDate              = $0007; { vt_date         7 }
  // fbpOleStr         = $0008; { vt_bstr         8 }
  // fbpDispatch       = $0009; { vt_dispatch     9 }
  // fbpError          = $000A; { vt_error       10 }
  fbpBoolean           = $000B; { vt_bool        11 }
  // fbpVariant        = $000C; { vt_variant     12 }
  // fbpUnknown        = $000D; { vt_unknown     13 }
  fbpShortInt          = $0010; { vt_i1          16 }
  fbpByte              = $0011; { vt_ui1         17 }
  fbpWord              = $0012; { vt_ui2         18 }
  fbpLongWord          = $0013; { vt_ui4         19 }
  fbpInt64             = $0014; { vt_i8          20 }
  fbpString            = $0100; { Pascal string 256 } {not OLE compatible }

  fbpStrList           = $0200; { FlexGraphics string list }
  fbpHexData           = $0201; { FlexGraphics hex data block }

  // Cursors in InDesign mode
  crShapeCursor           = 1;
  crShapeAddCursor        = 2;
  crShapeDelCursor        = 3;
  crShapeCloseCursor      = 4;
  crShapeMoveCursor       = 5;
  crShapeMoveCurveCursor  = 6;

  crCreateControlCursor   = 7;
  crCreateRectCursor      = 8;
  crCreateEllipseCursor   = 9;
  crCreateTextCursor      = 10;
  crCreatePicCursor       = 11;
  crCreatePolyCursor      = 12;

  crZoomInCursor          = 13;
  crZoomOutCursor         = 14;

  crPanCursor             = 15;
  crPanningCursor         = 16;

  crShapeContinueCursor   = 17;
  crShapeMoveLineCursor   = 18;

  crLastFlexCursor        = 49;

  // Scaling
  FloatDisplayFormat: string = '%.3f';
  PixelScaleFactor = 1000;

type
  TFlexNotify = (
    fnName, fnID, fnRect, fnAnchorPoints, fnEditPoints, fnLinkPoints,
    fnOrder,  fnParent, fnLoaded,
    fnLayers, fnSchemes, fnCreated, fnDestroyed, fnSelect, fnSelectPoint,
    fnScale, fnPan, fnChange{User generic} );

  PTranslateInfo = ^TTranslateInfo;
  TTranslateInfo = record
   Center: TPoint;  // in document (owner) coordinate system
   Rotate: integer; // counterclockwise degree
   Mirror: boolean; // horizontal mirroring
  end;

  TBitmapDisplay = ( bdCenter, bdTile, bdStretch );

  PTiledBitmapCache = ^TTiledBitmapCache;
  TTiledBitmapCache = record
   Handle: HBitmap;
   Width: integer;
   Height: integer;
  end;

  TBooleanArray = array of boolean;

  TGradientStyle = (
    gsHorizontal, gsVertical, gsSquare, gsElliptic,
    gsTopLeft, gsTopRight, gsBottomLeft, gsBottomRight );

  TFlexFiler = class;

  TFlexFilerProcess = ( ppUnknown, ppLoad, ppSave, ppCopy );

  // Flex binary command
  // <Header> [<Keys>] [<Data>]
  // <Header>: <TFlexBinHeader>
  // <Keys>: { <Key>, ... }
  // <Key>:  <KeyOptions>: byte, <KeyIndexOrLen>: Byte..DWord, [ <Key chars> ]
  //         KeyIndexOrLen = 0 - Empty string
  //         KeyIndexOrLen > 0 - Index of key in dictionary OR
  //                             Length of new key to load
  //                             (depend from <KeyOptions> flags)              
  // <Data>: <TFlexBinPropertyData> | <User defined type>

  PFlexBinHeader = ^TFlexBinHeader;
  TFlexBinHeader = packed record
   Command: word;
   KeyCount: word;
   Size: integer;
  end;

  PFlexBinPropertyData = ^TFlexBinPropertyData;
  TFlexBinPropertyData = packed record
   case PropType: cardinal of
    fbpEmpty,
    fbpNull:           ( );
    fbpSmallint:       ( VSmallInt: SmallInt );
    fbpInteger:        ( VInteger: integer );
    fbpSingle:         ( VSingle: single );
    fbpDouble:         ( VDouble: double );
    fbpCurrency:       ( VCurrency: currency );
    fbpDate:           ( VDate: {$IFDEF FG_CBUILDER} double
                                {$ELSE}              TDateTime {$ENDIF} );
    fbpBoolean:        ( VBoolean: integer {!} );
    fbpShortInt:       ( VShortInt: ShortInt );
    fbpByte:           ( VByte: Byte );
    fbpWord:           ( VWord: Word );
    fbpLongWord:       ( VLongWord: LongWord );
    fbpInt64:          ( VInt64: Int64 );
    fbpString:         ( VString: integer {KeyIndex arg number} );
    fbpStrList:        ( VStrings: record
                          StrCount: integer;
                          StrData: record end; {<Len: Int>[<Chars>], ...}
                         end );
    fbpHexData:        ( VHexData: record end );  // <Binary block data>
  end;

  PFlexBinReduceDicData = ^TFlexBinReduceDicData;
  TFlexBinReduceDicData = packed record
   DeleteCount: integer;
  end;

  TFlexBinKeyStore = (
    fbsObjectName, fbsClassName, fbsPropertyName, fbsPropertyValue );
  TFlexBinKeyStoreSet = set of TFlexBinKeyStore;

  TFlexBinaryData = class
  protected
   FOwner: TFlexFiler;
   FProcess: TFlexFilerProcess;
   FFinished: boolean;
   // Keys dictionary
   FDictionary: TStringList;
   FDicCapacity: integer;
   FDicDeleteCount: integer;
   // Key types don't store options
   FKeyDontStore: TFlexBinKeyStoreSet;
   // Keys for Read/Write commands
   FCmdKeys: TStringList;
   FCmdKeyCount: integer;
   FCmdKeysSize: integer;
   // Last ReadCommand() data
   FCmdReadHeader: TFlexBinHeader;
   FCmdReadDataSize: integer;
   FCmdReadUserData: pointer;
   FCmdReadUserDataSize: integer;
   FCmdReadDataAhead: boolean;
   // Helpers for LoadStrCheck()
   FReadPropLines: TStringList;
   FReadPropLine: integer;
   // Helpers for SaveStr()
   FWritePropType: integer;
   FWritePropName: string;
   FWritePropData: pointer;
   FWritePropDataPos: integer;
   FWritePropDataSize: integer;
   procedure SetProcess(const Value: TFlexFilerProcess);
   procedure SetDicCapacity(const Value: integer);
   procedure SetDicDeleteCount(const Value: integer);
   function  GetReadKey(Index: integer): string;
   function  GetPropertyDataSize(PropType: integer;
     const Value: Variant): integer;
   function  AddKey(const Key: string; var KeyIndex: integer): boolean;
   function  AddWriteKey(const Key: string; DontStore: boolean = false): integer;
   function  ReadKey: boolean;
   procedure WriteKey(const Key: string; KeyIndex: integer;
     DontStore: boolean = false);
   procedure ResetWriteKeys;
   function  ReadCommandHeaderAndKeys(var Header: TFlexBinHeader): boolean;
   function  WriteCommandHeaderAndKeys(Command: word;
     DataSize: integer): boolean;
   function  ReadPropertyData(var Value: Variant;
     var PropType: integer): boolean;
   function  AllocReadData: pointer;
   procedure ClearReadData;
   function  GrowWriteData(Size: integer): pointer;
   procedure ClearWriteData;
   procedure CheckDontStore(Command: word;
     var DontStoreKey1, DontStoreKey2: boolean);
   procedure ReduceDictionary(ACount: integer);
  public
   constructor Create(AOwner: TFlexFiler);
   destructor Destroy; override;
   procedure Reset;
   function  LoadStrCheck(out s: string): boolean;
   procedure SaveStr(const s: string);
   procedure SaveStrBuf(Buf: pointer; BufSize: integer);
   function  ReadCommand: boolean;
   function  ReadCommandData(out Value: Variant): integer;
   function  ReadCommandUserData: pointer;
   function  WritePropertyCommand(const PropName: string; PropType: integer;
     const Value: Variant; DontStoreValueKey: boolean = false): boolean;
   procedure WriteSimpleCommand(Command: word; KeyCount: integer = 0;
     const Key1: string = ''; const Key2: string = '';
     DontStoreKey1: boolean = false; DontStoreKey2: boolean = false);
   procedure WriteDataCommand(Command: word; var Data; DataSize: integer;
     KeyCount: integer = 0; const Key1: string = ''; const Key2: string = '';
     DontStoreKey1: boolean = false; DontStoreKey2: boolean = false);
   function  GetPropertyType(const PropValue: Variant): integer;
   property  Process: TFlexFilerProcess read FProcess;
   property  KeyDontStore: TFlexBinKeyStoreSet read FKeyDontStore write
     FKeyDontStore;
   property  Finished: boolean read FFinished;
   property  DicCapacity: integer read FDicCapacity write SetDicCapacity;
   property  DicDeleteCount: integer read FDicDeleteCount
     write SetDicDeleteCount;
   property  ReadCmdCommand: word read FCmdReadHeader.Command;
   property  ReadCmdSize: integer read FCmdReadHeader.Size;
   property  ReadCmdDataSize: integer read FCmdReadDataSize;
   property  ReadCmdKeyCount: word read FCmdReadHeader.KeyCount;
   property  ReadCmdKeys[Index: integer]: string read GetReadKey;
   property  ReadCmdKeysSize: integer read FCmdKeysSize;
  end;

  TFlexProgressEvent = procedure(Sender: TObject; Progress: integer;
    Process: TFlexFilerProcess) of object;

  TFlexFiler = class
  private
   FStream: TStream;
   FBuffer: Pointer;
   FBufSize: Integer;
   FBufPos: Integer;
   FBufEnd: Integer;
   FTotal: integer;
   FSaved: integer;
   FLoaded: integer;
   FProgress: integer;
   FLastProcess: TFlexFilerProcess;
   FBinary: boolean;
   FCompleteBinary: boolean;
   FBinaryData: TFlexBinaryData;
   FOnProgress: TFlexProgressEvent;
   procedure SetSaved(const Value: integer);
   procedure SetTotal(const Value: integer);
   procedure SetBinary(const Value: boolean);
  protected
   procedure ReadBuffer;
   procedure DoProgress(Process: TFlexFilerProcess);
   function  GetStreamSize: integer;
   function  ReadBufCheck(Buf: pointer; BufSize: integer): boolean;
   procedure ReadBuf(var Buf; BufSize: integer);
   function  ReadSkipBuf(BufSize: integer): boolean;
   procedure WriteBuf(const Buf; BufSize: integer);
   procedure ReadError(const Msg: string = ''); virtual;
   procedure WriteError(const Msg: string = ''); virtual;
   property  Stream: TStream read FStream;
  public
   constructor Create(AStream: TStream; ACompleteBinary: boolean = false);
   destructor Destroy; override;
   procedure SaveStr(const s: string);
   procedure SaveBuf(Buf: pointer; BufSize: integer);
   function  LoadStr: string;
   function  LoadStrCheck(out s: string): boolean;
   procedure LoadSkipToEnd;
   function  CheckLoadSkipToEnd(const First: string): boolean;
   function  IsEndOfStream: boolean;
   procedure Rewind;
   property  Binary: boolean read FBinary write SetBinary;
   property  CompleteBinary: boolean read FCompleteBinary;
   property  BinaryData: TFlexBinaryData read FBinaryData;
   property  Total: integer read FTotal write SetTotal;
   property  Saved: integer read FSaved write SetSaved;
   property  Loaded: integer read FLoaded;
   property  StreamSize: integer read GetStreamSize;
   property  Progress: integer read FProgress;
   property  OnProgress: TFlexProgressEvent read FOnProgress write FOnProgress;
  end;

  TIdPool = class
  private
   FPool: TList;
   function  GetUsed(Value: cardinal): boolean;
  public
   constructor Create;
   destructor Destroy; override;
   function  Generate: cardinal;
   function  Use(Value: cardinal): boolean;
   function  Release(Value: cardinal): boolean;
   function  NextUsed(Value: cardinal; var Index: integer): cardinal;
   procedure Clear;
   property  Used[Value: cardinal]: boolean read GetUsed;
   property  PoolList: TList read FPool;
  end;

  TNotifyLink = class;

  TNotifyLinkCode = ( ncInfo, ncDestroy, ncPropBeforeChanged, ncPropChanged,
    ncControlNotify );

  PNotifyLinkInfo = ^TNotifyLinkInfo;
  TNotifyLinkInfo = record
   case Code: TNotifyLinkCode of
    ncInfo:
      ( WParam: integer;
        LParam: integer ); // Generic notification
    ncDestroy: ();
    ncPropBeforeChanged,
    ncPropChanged:
      ( Prop: TObject {TCustomProp} );
    ncControlNotify:
      ( Control: TObject {TFlexControl};
        ControlNotify: TFlexNotify );
  end;

  TNotifyLinkEvent = procedure(Sender: TObject; Source: TNotifyLink;
    const Info: TNotifyLinkInfo) of object;

  TNotifyLink = class
  private
   FLinks: TList;
   FOwner: TObject;
   FTag: integer;
   FDestroying: boolean;
   FOnNotify: TNotifyLinkEvent;
   FOnFreeNotify: TNotifyLinkEvent;
   function  GetLink(Index: integer): TNotifyLink;
   function  GetLinkCount: integer;
   function  GetLinkRefCount(Index: integer): integer;
  public
   constructor Create(AOwner: TObject);
   destructor Destroy; override;
   function  IndexOf(Link: TNotifyLink): integer;
   function  Subscribe(Link: TNotifyLink): integer;
   function  Unsubscribe(Link: TNotifyLink): integer;
   function  Notify(const Info: TNotifyLinkInfo): integer;
   procedure DestroyNotify;
   function  PropNotify(AProp: TObject; IsBeforeNotify: boolean): integer;
   function  ControlNotify(AControl: TObject; ANotify: TFlexNotify): integer;
   property  Owner: TObject read FOwner;
   property  LinkCount: integer read GetLinkCount;
   property  Links[Index: integer]: TNotifyLink read GetLink; default;
   property  LinksRefCount[Index: integer]: integer read GetLinkRefCount;
   property  Tag: integer read FTag write FTag;
   property  Destroying: boolean read FDestroying;
   property  OnNotify: TNotifyLinkEvent read FOnNotify write FOnNotify;
   property  OnFreeNotify: TNotifyLinkEvent read FOnFreeNotify
     write FOnFreeNotify;
  end;

var
  SysGradientChecked: boolean = false;
  SysGradientEnabled: boolean = false;
  SysGradientEllipticSteps: integer = 256;

procedure LoadFlexCursors;

function  FlexStrNeedQuote(const s: string): boolean;
function  StrBeginsFrom(const S1, S2: string): boolean;
function  ExtractWord(const s: string; NumWord: integer; Delimiter: char): string;
function  HexCharsToByte(cw: word): byte;
function  ByteToHexChars(b: byte): word;

procedure GetPicReadWrite(Picture: TPicture;
  out ReadProc, WriteProc: TStreamProc);
function  NormalizeRect(const R: TRect): TRect;
function  PointInRect(const p: TPoint; const R: TRect): boolean;

function  IntersectClipRgn(ACanvas: TCanvas; ClipRgn: HRGN): HRGN;
function  IntersectClipPath(DC: HDC): HRGN;
procedure PaintGradient(ACanvas: TCanvas; ARect: TRect; Style: TGradientStyle;
  Color, EndColor: TColor; PenMode: TPenMode);
procedure PaintBitmap(ACanvas: TCanvas; const PaintRect, RefreshRect: TRect;
  ABitmap: TBitmap; BitmapDisplay: TBitmapDisplay;
  BitmapCache: PTiledBitmapCache = Nil; Scale: integer = 100;
  ClipTransparent: boolean = false);
procedure PaintTailed(ACanvas: TCanvas; const PaintRect, RefreshRect: TRect;
  ABitmap: TBitmap; BitmapCache: PTiledBitmapCache = Nil);

function  CreateTransparentClipRgn(DC: HDC; Width, Height: integer;
  var Dest: TRect; TransparentColor: TColor): HRGN;
procedure ExcludeBitmapTransparency(Bmp: TBitmap; var R: TRect;
  var ClipRgn: HRGN);

function  ScaleValue(Value, Scale: integer): integer;
function  UnScaleValue(Value, Scale: integer): integer;
function  ScalePixels(Value: integer): integer;
function  UnScalePixels(Value: integer): integer;

function  ListScan(Value, List: Pointer; Count: integer): integer;
function  ListScanEx(Value, List: Pointer; Index, Count: integer): integer;
function  ListScanLess(Value, List: Pointer; Count: integer): integer;

type
  TSortedListCompare = function(Item1, Item2: Pointer): Integer of object;

// Find Item index in alredy sorted list
function  SortedListFind(List: PPointerList; Count: integer; Item: Pointer;
  Compare: TSortedListCompare; Exact: boolean): integer;
// Return new index for item with ItemIndex in already sorted list. This
// function must be called after some changes in the item to keep list sorted.
// Caller must move item at ItemIndex to resulting index after call.
function  ListSortItem(List: PPointerList; Count: integer; ItemIndex: integer;
  Compare: TSortedListCompare): integer;
// Sort List in order defined by Compare method
procedure ListQuickSort(List: PPointerList; L, R: Integer;
  Compare: TSortedListCompare);

function  IsClassParent(AClass, AParentClass: TClass): boolean;

function  DotFloatToStr(const Value: extended): string;
function  DotStrToFloat(Value: string): extended;

{$IFNDEF FG_D5}
procedure FreeAndNil(var Obj);

function GetPropValue(Instance: TObject; const PropName: string;
  PreferStrings: Boolean): Variant;
procedure SetPropValue(Instance: TObject; const PropName: string;
  const Value: Variant);
function PropType(AClass: TClass; const PropName: string): TTypeKind;
{$ENDIF}

{$IFNDEF FG_D6}
{ Copied from Delphi7 System.pas unit }

const
  varByte     = $0011; { vt_ui1         }
  varWord     = $0012; { vt_ui2         }
  varLongWord = $0013; { vt_ui4         }
  varInt64    = $0014; { vt_i8          }
  varShortInt = $0010; { vt_i1       16 }

type
  TVarType = Word;

{ PChar/PWideChar Unicode <-> UTF8 conversion }
type
  UTF8String = type string;
  PUTF8String = ^UTF8String;

// UnicodeToUTF8(3):
// UTF8ToUnicode(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.

function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: Integer): Integer; overload;
function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: Integer): Integer; overload;

// UnicodeToUtf8(4):
// UTF8ToUnicode(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal; overload;

{ WideString <-> UTF8 conversion }

function UTF8Encode(const WS: WideString): UTF8String;
function UTF8Decode(const S: UTF8String): WideString;

{ Ansi <-> UTF8 conversion }

function AnsiToUtf8(const S: string): UTF8String;
function Utf8ToAnsi(const S: UTF8String): string;
{$ENDIF}

{$IFNDEF FG_D7}
{ Copied from Delphi7 Classes.pas unit}
type
  TGetLookupInfoEvent = procedure(var Ancestor: TPersistent;
    var Root, LookupRoot, RootAncestor: TComponent) of object;

function AncestorIsValid(Ancestor: TPersistent; Root,
  RootAncestor: TComponent): Boolean;
function IsDefaultPropertyValue(Instance: TObject; PropInfo: PPropInfo;
  OnGetLookupInfo: TGetLookupInfoEvent): Boolean;
{$ENDIF}

{$IFNDEF FG_D12}
function  RectWidth(const ARect: TRect): integer;
function  RectHeight(const ARect: TRect): integer;
{$ENDIF}

implementation

{$R cursors.res}

{$IFDEF FG_D12}
uses Types;
{$ENDIF}

const
  fbCmdKeyMask      = $3FFFFFFF;
  fbCmdKeyNew       = $40000000;
  fbCmdKeyDontStore = $80000000;

// TFlexBinaryData ////////////////////////////////////////////////////////////

constructor TFlexBinaryData.Create(AOwner: TFlexFiler);
begin
 FOwner := AOwner;
 FKeyDontStore := [ ]; // [ fbsPropertyValue ];
 FDictionary := TStringList.Create;
 FCmdKeys := TStringList.Create;
 FReadPropLines := TStringList.Create;
 FDicCapacity := 65536;
 FDicDeleteCount := 8192;
 Reset;
end;

destructor TFlexBinaryData.Destroy;
begin
 inherited;
 ClearReadData;
 ClearWriteData;
 FDictionary.Free;
 FCmdKeys.Free;
 FReadPropLines.Free;
end;

procedure TFlexBinaryData.SetProcess(const Value: TFlexFilerProcess);
var s: AnsiString;
begin
 if (FProcess = Value) or (FProcess <> ppUnknown) then exit;
 FProcess := Value;
 if FProcess = ppSave then begin
  // Dictionary must be sorted in ppSave process only
  FDictionary.Sorted := true;
  if not FOwner.CompleteBinary then begin
   // Write binary keyword first
   s := fcBinary + #$0D#$0A;
   FOwner.WriteBuf(s[1], Length(s));
  end;
 end;
end;

procedure TFlexBinaryData.SetDicCapacity(const Value: integer);
begin
 if FProcess <> ppUnknown then exit;
 FDicCapacity := Value;
end;

procedure TFlexBinaryData.SetDicDeleteCount(const Value: integer);
begin
 if FProcess <> ppUnknown then exit;
 FDicDeleteCount := Value;
end;

function TFlexBinaryData.GetReadKey(Index: integer): string;
var KeyIndex: integer;
begin
 Result := '';
 if (FProcess <> ppLoad) or
    (Index < 0) or (Index >= FCmdKeyCount) then exit;
 KeyIndex := integer(FCmdKeys.Objects[Index]);
 if KeyIndex and fbCmdKeyNew = 0
  then Result := FDictionary[KeyIndex]
  else Result := FCmdKeys[Index];
end;

procedure TFlexBinaryData.Reset;
begin
 FDictionary.Clear;
 FDictionary.Sorted := false;
 FDictionary.Add('');
 FProcess := ppUnknown;
 FFinished := false;
 FReadPropLines.Clear;
 ClearWriteData;     
 ClearReadData;
end;

function TFlexBinaryData.AddKey(const Key: string;
  var KeyIndex: integer): boolean;
var DicCount: integer;
begin
 if Key = '' then begin
  // Empty key string
  KeyIndex := 0;
  Result := false;
 end else
 if FProcess = ppSave then begin
  // Add key if not exist
  DicCount := FDictionary.Count;
  //KeyIndex := FDictionary.AddObject(Key, pointer(DicCount));
  KeyIndex := FDictionary.Add(Key);
  Result := DicCount < FDictionary.Count;
  if Result then begin
   // New key added
   FDictionary.Objects[KeyIndex] := pointer(DicCount);
   // Check dictionary reduction
   if (FDicCapacity > 0) and (FDictionary.Count > FDicCapacity) then
    ReduceDictionary(FDicDeleteCount);
  end else
   // Get original (without sorting) index for key
   KeyIndex := integer(FDictionary.Objects[KeyIndex]);
 end else begin
  KeyIndex := FDictionary.Add(Key);
  Result := true;
 end;
end;

procedure TFlexBinaryData.ReduceDictionary(ACount: integer);
var Limit, i: integer;
    Index, DicIndex: integer;
    Header: TFlexBinHeader;
    ReduceData: TFlexBinReduceDicData;
begin
 if ACount <= 0 then exit;
 Limit := FDictionary.Count - ACount;
 if Limit < 1 then Limit := 1;
 case FProcess of
  ppLoad:
    // Just delete last ACount items
    while FDictionary.Count > Limit do FDictionary.Delete(FDictionary.Count-1);
  ppSave:
    begin
     // Fix current command keys
     for i:=0 to FCmdKeyCount-1 do begin
      DicIndex := integer(FCmdKeys.Objects[i]);
      Index := DicIndex and fbCmdKeyMask;
      if (DicIndex and fbCmdKeyNew = 0) and (Index >= Limit) then begin
       // This key must be deleted now. Make new key
       FCmdKeys[i] := FDictionary[Index];
       FCmdKeys.Objects[i] := pointer(fbCmdKeyNew); // Can't have DontStore flag!
      end;
     end;
     // Delete all items with real index greater then Limit
     for i:=FDictionary.Count-1 downto 0 do
      if integer(FDictionary.Objects[i]) >= Limit then FDictionary.Delete(i);
     // Save reduce dictionary command
     Header.Command := fbcReduceDictionary;
     Header.KeyCount := 0;
     Header.Size := SizeOf(Header) + SizeOf(ReduceData);
     FOwner.WriteBuf(Header, SizeOf(Header));
     ReduceData.DeleteCount := ACount;
     FOwner.WriteBuf(ReduceData, SizeOf(ReduceData));
    end;
 end;
end;

procedure TFlexBinaryData.WriteKey(const Key: string; KeyIndex: integer;
  DontStore: boolean = false);
var AnsiKey: AnsiString;
    Len: integer;
    KeyHead: array[0..4] of byte;
    KeyHeadSize: integer;
begin
 AnsiKey := AnsiString(Key);
 Len := Length(AnsiKey);
 if Len > 0 then KeyIndex := Len;
 // Setup key head size: key options + key index or len
 if KeyIndex <= High(Byte) then begin
  KeyHead[0] := fbkIndexByte;
  KeyHead[1] := KeyIndex and $FF;
  KeyHeadSize := 2;
 end else
 if KeyIndex <= High(Word) then begin
  KeyHead[0] := fbkIndexWord;
  KeyHead[1] := KeyIndex and $FF;
  KeyHead[2] := KeyIndex and $FF00 shr 8;
  KeyHeadSize := 3;
 end else begin
  KeyHead[0] := fbkIndexDWord;
  KeyHead[1] := KeyIndex and $FF;
  KeyHead[2] := (KeyIndex shr 8) and $FF;
  KeyHead[3] := (KeyIndex shr 16) and $FF;
  KeyHead[4] := KeyIndex shr 24;
  KeyHeadSize := 5;
 end;
 if DontStore then KeyHead[0] := KeyHead[0] or fbkDontStore;
 if Len = 0 then
  KeyHead[0] := KeyHead[0] or fbkIsIndex;
 // Save key head
 FOwner.WriteBuf(KeyHead, KeyHeadSize);
 // Save key string
 if Len > 0 then FOwner.WriteBuf(AnsiKey[1], Len);
end;

function TFlexBinaryData.ReadKey: boolean;
var KeyFirst: word;
    KeyOptions: byte;
    KeyIndex: integer;
    KeyIndexOrLen: array[0..3] of byte;
    AddSize: integer;

 procedure ReadKeyFromStream(Len: integer; Store: boolean);
 var s: AnsiString;
     KeyIndex: integer;
 begin
  SetLength(s, Len);
  FOwner.ReadBuf(s[1], Len);
  inc(FCmdKeysSize, Len);
  // Add key to dictionary
  if Store
   then AddKey(String(s), KeyIndex)
   else KeyIndex := 0;
  KeyIndex := KeyIndex or fbCmdKeyNew;
  // Save Key in FCmdKeys
  if FCmdKeys.Count = FCmdKeyCount then
   FCmdKeys.AddObject(String(s), pointer(KeyIndex))
  else begin
   FCmdKeys[FCmdKeyCount] := String(s);
   FCmdKeys.Objects[FCmdKeyCount] := pointer(KeyIndex);
  end;
  inc(FCmdKeyCount);
 end;

begin
 Result := true;
 FOwner.ReadBuf(KeyFirst, SizeOf(KeyFirst));
 inc(FCmdKeysSize, SizeOf(KeyFirst));
 KeyOptions := KeyFirst and $FF;
 // Define key length
 case KeyOptions and fbkIndexSizeMask of
  fbkIndexByte  : AddSize := 0;
  fbkIndexWord  : AddSize := 1;
  fbkIndexDWord : AddSize := 3;
  else begin
   FOwner.ReadError;
   AddSize := 0;
  end;
 end;
 if AddSize > 0 then begin
  // Read key index or length
  KeyIndexOrLen[0] := KeyFirst shr 8;
  FOwner.ReadBuf(KeyIndexOrLen[1], AddSize);
  inc(FCmdKeysSize, AddSize);
  if AddSize = 1 then
    KeyIndex := word(KeyIndexOrLen[0] or KeyIndexOrLen[1] shl 8)
  else
    KeyIndex := KeyIndexOrLen[0]        or KeyIndexOrLen[1] shl 8 or
                KeyIndexOrLen[2] shl 16 or KeyIndexOrLen[3] shl 24;
 end else
  // Index size is one byte
  KeyIndex := byte(KeyFirst shr 8);
 if KeyOptions and fbkIsIndex <> 0 then begin
  // Read key from dictionary
   // KeyIndex := -KeyIndex;
  // Save KeyIndex only in FCmdKeys
  if FCmdKeys.Count = FCmdKeyCount
   then FCmdKeys.AddObject('', pointer(KeyIndex))
   else FCmdKeys.Objects[FCmdKeyCount] := pointer(KeyIndex);
  inc(FCmdKeyCount);
 end else
  // Read key from stream and save in FCmdKeys
  ReadKeyFromStream(KeyIndex, KeyOptions and fbkDontStore = 0);
end;

{$HINTS OFF}
function TFlexBinaryData.GetPropertyDataSize(PropType: integer;
  const Value: Variant): integer;
var PropData: TFlexBinPropertyData;
    Count, i: integer;
begin
 case PropType of
  fbpEmpty,
  fbpNull:           Result := 0;
  fbpSmallint:       Result := SizeOf(PropData.VSmallInt);
  fbpInteger:        Result := SizeOf(PropData.VInteger);
  fbpSingle:         Result := SizeOf(PropData.VSingle);
  fbpDouble:         Result := SizeOf(PropData.VDouble);
  fbpCurrency:       Result := SizeOf(PropData.VCurrency);
  fbpDate:           Result := SizeOf(PropData.VDate);
  fbpBoolean:        Result := SizeOf(PropData.VBoolean);
  fbpShortInt:       Result := SizeOf(PropData.VShortInt);
  fbpByte:           Result := SizeOf(PropData.VByte);
  fbpWord:           Result := SizeOf(PropData.VWord);
  fbpLongWord:       Result := SizeOf(PropData.VLongWord);
  fbpInt64:          Result := SizeOf(PropData.VInt64);
  fbpString:         Result := SizeOf(PropData.VString);
  fbpStrList,
  fbpHexData:
    if VarIsEmpty(Value) or VarIsNull(Value) then
     Result := 0
    else
    if VarType(Value) and varArray = 0 then
     Result := -1
    else begin
     Count := VarArrayHighBound(Value, 1) + 1;
     if PropType = fbpHexData then
      Result := Count
     else begin
      Result := SizeOf(Count);
      for i:=0 to Count-1 do inc(Result, SizeOf(Count) + Length(Value[i]));
     end;
    end;
  else
    Result := -1;
 end;
 if Result >= 0 then inc(Result, SizeOf(PropData.PropType));
end;
{$HINTS ON}

function TFlexBinaryData.GetPropertyType(const PropValue: Variant): integer;
begin
 case VarType(PropValue) of
  varEmpty      : Result := fbpEmpty;
  varNull       : Result := fbpNull;
  varSmallInt   : Result := fbpSmallint;
  varInteger    : Result := fbpInteger;
  varSingle     : Result := fbpSingle;
  varDouble     : Result := fbpDouble;
  varCurrency   : Result := fbpCurrency;
  varDate       : Result := fbpDate;
  varBoolean    : Result := fbpBoolean;
  varShortInt   : Result := fbpShortInt;
  varByte       : Result := fbpByte;
  varWord       : Result := fbpWord;
  varLongWord   : Result := fbpLongWord;
  varInt64      : Result := fbpInt64;
  varString     : Result := fbpString;
  else            Result := fbpString;
 end;
end;

procedure TFlexBinaryData.ClearReadData;
begin
 if not Assigned(FCmdReadUserData) then exit;
 FreeMem(FCmdReadUserData);
 FCmdReadUserData := Nil;
 FCmdReadUserDataSize := 0;
end;

function TFlexBinaryData.AllocReadData: pointer;
begin
 if Assigned(FCmdReadUserData) then ClearReadData;
 if FCmdReadDataSize > 0 then begin
  GetMem(FCmdReadUserData, FCmdReadDataSize);
  FCmdReadUserDataSize := FCmdReadDataSize;
 end;
 Result := FCmdReadUserData;
end;

function TFlexBinaryData.GrowWriteData(Size: integer): pointer;
const GrowSize = 65536;
var NewSize: integer;
begin
 Result := FWritePropData;
 NewSize := FWritePropDataPos + Size;
 if NewSize <= FWritePropDataSize then exit;
 NewSize := (NewSize div GrowSize +1) * GrowSize; 
 if Assigned(FWritePropData)
  then ReallocMem(FWritePropData, NewSize)
  else GetMem(FWritePropData, NewSize);
 FWritePropDataSize := NewSize;
 if NewSize < FWritePropDataPos then FWritePropDataPos := NewSize;
 Result := FWritePropData;
end;

procedure TFlexBinaryData.ClearWriteData;
begin
 if not Assigned(FWritePropData) then exit;
 FreeMem(FWritePropData);
 FWritePropData := Nil;
 FWritePropDataSize := 0;
 FWritePropDataPos := 0;
 FWritePropType := fbpEmpty;
end;

function TFlexBinaryData.ReadCommandHeaderAndKeys(
  var Header: TFlexBinHeader): boolean;
var i: integer;
begin
 Result := false;
 if (FProcess = ppSave) or FFinished then exit;
 if FProcess = ppUnknown then SetProcess(ppLoad);
 // Check skip data block
 if FCmdReadDataAhead then begin
  // FOwner.Stream.Position := FOwner.Stream.Position + FCmdReadDataSize;
  FOwner.ReadSkipBuf(FCmdReadDataSize);
  FCmdReadDataSize := 0;
  FCmdReadDataAhead := false;
 end else
  ClearReadData;
 // Check finished
 if FFinished then exit;
 // Read header
 FOwner.ReadBuf(Header, SizeOf(Header));
 if (@FCmdReadHeader <> @Header) then FCmdReadHeader := Header;
 // Read keys
 FCmdKeyCount := 0;
 FCmdKeysSize := 0;
 for i:=0 to Header.KeyCount-1 do ReadKey;
 // Calculate data size
 FCmdReadDataSize :=
   FCmdReadHeader.Size - SizeOf(FCmdReadHeader) - FCmdKeysSize;
 FCmdReadDataAhead := FCmdReadDataSize > 0;
 // Check dictionary reduction
 if Header.Command = fbcReduceDictionary then begin
  // Reduce dictionary
  ReduceDictionary( PFlexBinReduceDicData(ReadCommandUserData).DeleteCount );
 end;
 // Update finished
 FFinished := Header.Command = fbcBinaryEnd;
 if FFinished then FOwner.Binary := false;
end;

function TFlexBinaryData.WriteCommandHeaderAndKeys(Command: word;
  DataSize: integer): boolean;
var Header: TFlexBinHeader;
    i: integer;
    DicIndex: cardinal;
    DontStore: boolean;
begin
 Result := false;
 if (FProcess = ppLoad) or FFinished then exit;
 if FProcess = ppUnknown then SetProcess(ppSave);
 // Check finished
 if FFinished then exit;
 // Write header
 Header.Command := Command;
 Header.KeyCount := FCmdKeyCount;
 Header.Size := SizeOf(Header) + FCmdKeysSize + DataSize;
 FOwner.WriteBuf(Header, SizeOf(Header));
 // Write keys
 for i:=0 to FCmdKeyCount-1 do begin
  DicIndex := cardinal(FCmdKeys.Objects[i]);
  DontStore := DicIndex and fbCmdKeyDontStore <> 0;
  if DicIndex and fbCmdKeyNew <> 0
   then WriteKey(FCmdKeys[i], -1, DontStore)
   else WriteKey('', DicIndex and fbCmdKeyMask, DontStore);
 end;
 // Update finished
 FFinished := Command = fbcBinaryEnd;
 if FFinished then FOwner.Binary := false;
 Result := true;
end;

procedure TFlexBinaryData.ResetWriteKeys;
begin
 if FProcess = ppUnknown then SetProcess(ppSave);
 FCmdKeyCount := 0;
 FCmdKeysSize := 0;
end;

function TFlexBinaryData.AddWriteKey(const Key: string;
  DontStore: boolean = false): integer;
var KeyIndex, DicIndex, Len: integer;
    Size: integer;
begin
 // Test key length
 Len := Length(Key);
 if Len > fbCmdKeyMask then FOwner.WriteError;
 // Store key in dictinary
 if DontStore or AddKey(Key, KeyIndex) then begin
  // New key in dictionary. Set KeyIndex as length
  KeyIndex := Len;
  DicIndex := fbCmdKeyNew;
 end else
  // Key already exist
  DicIndex := KeyIndex;
 // Add DontStore flag if necessary
 if DontStore then DicIndex := integer(Cardinal(DicIndex) or fbCmdKeyDontStore);
 // Calc key size 
 if KeyIndex <= High(Byte) then
  Size := 2
 else
 if KeyIndex <= High(Word) then
  Size := 3
 else
  Size := 5;
 if DicIndex and fbCmdKeyNew <> 0 then inc(Size, Len);
 // Change command keys size
 inc(FCmdKeysSize, Size);
 // Save Key in FCmdWriteKeys
 if FCmdKeys.Count = FCmdKeyCount then
  FCmdKeys.AddObject(Key, pointer(DicIndex))
 else begin
  FCmdKeys[FCmdKeyCount] := Key;
  FCmdKeys.Objects[FCmdKeyCount] := pointer(DicIndex);
 end;
 Result := FCmdKeyCount;
 inc(FCmdKeyCount);
end;

procedure TFlexBinaryData.CheckDontStore(Command: word; var DontStoreKey1,
  DontStoreKey2: boolean);
begin
 case Command of
  fbcDocument,
  fbcLibrary,
  fbcObject:
    begin
     DontStoreKey1 := DontStoreKey1 or (fbsObjectName in FKeyDontStore);
     DontStoreKey2 := DontStoreKey2 or (fbsClassName in FKeyDontStore);
    end;
  fbcProperty,
  fbcComplexProperty:
    begin
     DontStoreKey1 := DontStoreKey1 or (fbsPropertyName in FKeyDontStore);
     DontStoreKey2 := DontStoreKey2 or (fbsPropertyValue in FKeyDontStore);     
    end;
 end;
end;

procedure TFlexBinaryData.WriteSimpleCommand(Command: word;
  KeyCount: integer = 0; const Key1: string = ''; const Key2: string = '';
  DontStoreKey1: boolean = false; DontStoreKey2: boolean = false);
begin
 if FFinished then exit;
 ResetWriteKeys;
 CheckDontStore(Command, DontStoreKey1, DontStoreKey2);
 if KeyCount > 0 then AddWriteKey(Key1, DontStoreKey1);
 if KeyCount > 1 then AddWriteKey(Key2, DontStoreKey2);
 WriteCommandHeaderAndKeys(Command, 0);
end;

procedure TFlexBinaryData.WriteDataCommand(Command: word; var Data;
  DataSize: integer; KeyCount: integer = 0;
  const Key1: string = ''; const Key2: string = '';
  DontStoreKey1: boolean = false; DontStoreKey2: boolean = false);
begin
 if FFinished then exit;
 ResetWriteKeys;
 CheckDontStore(Command, DontStoreKey1, DontStoreKey2);
 if KeyCount > 0 then AddWriteKey(Key1, DontStoreKey1);
 if KeyCount > 1 then AddWriteKey(Key2, DontStoreKey2);
 if WriteCommandHeaderAndKeys(Command, DataSize) then
  FOwner.WriteBuf(Data, DataSize);
end;

function TFlexBinaryData.WritePropertyCommand(const PropName: string;
  PropType: integer; const Value: Variant;
  DontStoreValueKey: boolean = false): boolean;
var DataSize: integer;
    PropData: TFlexBinPropertyData;
    PropDataPtr: PFlexBinPropertyData;
    HexData: pointer;
    HexDataSize: integer;
    i: integer;
    StrCount: integer;
    StrPtr: PAnsiChar;
    StrLen: integer;
    s: AnsiString;
    NameDontStore: boolean;
begin
 Result := false;
 if FFinished then exit;
 // Calc DataSize
 DataSize := GetPropertyDataSize(PropType, Value);
 if DataSize < 0 then exit;
 // Check DontStore flags for keys
 NameDontStore := false;
 CheckDontStore(fbcProperty, NameDontStore, DontStoreValueKey);
 // Init write keys
 ResetWriteKeys;
 AddWriteKey(PropName, NameDontStore);
 // Fill PropData
 PropData.PropType := PropType;
 PropDataPtr := @PropData;
 try
  case PropType of
   fbpSmallint      : PropData.VSmallInt := Value;
   fbpInteger       : PropData.VInteger := Value;
   fbpSingle        : PropData.VSingle := Value;
   fbpDouble        : PropData.VDouble := Value;
   fbpCurrency      : PropData.VCurrency := Value;
   fbpDate          : PropData.VDate := Value;
   fbpBoolean       : PropData.VBoolean := Value;
   fbpShortInt      : PropData.VShortInt := Value;
   fbpByte          : PropData.VByte := Value;
   fbpWord          : PropData.VWord := Value;
   fbpLongWord      : PropData.VLongWord := Value;
   fbpInt64:
     {$IFDEF FG_D6}
     PropData.VInt64 := Value;
     {$ELSE}
     FOwner.WriteError;
     {$ENDIF}
   fbpString:
     PropData.VString := AddWriteKey(Value, DontStoreValueKey);
   fbpStrList:
     begin
      GetMem(PropDataPtr, DataSize);
      PropDataPtr.PropType := PropType;
      if not VarIsEmpty(Value) and not VarIsNull(Value) then begin
       // Save string count
       StrCount := VarArrayHighBound(Value, 1) + 1;
       PropDataPtr.VStrings.StrCount := StrCount;
       // Save strings
       StrPtr := @PropDataPtr.VStrings.StrData;
       for i:=0 to StrCount-1 do begin
        s := AnsiString(Value[i]);
        StrLen := Length(s);
        PInteger(StrPtr)^ := StrLen;
        inc(StrPtr, SizeOf(StrLen));
        Move(s[1], StrPtr^, StrLen);
        inc(StrPtr, StrLen);
       end;
      end;
     end;
   fbpHexData:
     begin
      GetMem(PropDataPtr, DataSize);
      PropDataPtr.PropType := PropType;
      HexDataSize :=
        DataSize - (PAnsiChar(@PropDataPtr.VHexData) - PAnsiChar(PropDataPtr));
      if HexDataSize > 0 then begin
       // Move block
       HexData := VarArrayLock(Value);
       try
        Move(HexData^, PropDataPtr.VHexData, HexDataSize);
       finally
        VarArrayUnlock(Value);
       end;
      end;
     end;
   else
     exit;
  end;
  // Write command
  if WriteCommandHeaderAndKeys(fbcProperty, DataSize) then
   FOwner.WriteBuf(PropDataPtr^, DataSize);
 finally
  if PropDataPtr <> @PropData then FreeMem(PropDataPtr);
 end;
 Result := true;
end;

function TFlexBinaryData.ReadPropertyData(var Value: Variant;
  var PropType: integer): boolean;
var PropSimpleData: TFlexBinPropertyData;
    PropData: PFlexBinPropertyData;
    //KeyIndex: integer;
    i: integer;
    StrPtr: PAnsiChar;
    StrLen: integer;
    StrCount: integer;
    s: AnsiString;
    HexData: pointer;
    HexDataSize: integer;
begin
 Result := false;
 if FCmdReadDataSize < SizeOf(PropData.PropType) then exit;
 PropData := @PropSimpleData;
 try
  if FCmdReadDataSize > SizeOf(PropSimpleData) then
   GetMem(PropData, FCmdReadDataSize);
  if FCmdReadDataAhead then begin
   FOwner.ReadBuf(PropData^, FCmdReadDataSize);
   FCmdReadDataAhead := false;
  end else
   exit;
  PropType := PropData.PropType;
  case PropType of
   fbpSmallint      : Value := PropData.VSmallInt;
   fbpInteger       : Value := PropData.VInteger;
   fbpSingle        : Value := PropData.VSingle;
   fbpDouble        : Value := PropData.VDouble;
   fbpCurrency      : Value := PropData.VCurrency;
   fbpDate          : Value := PropData.VDate;
   fbpBoolean       : if PropData.VBoolean = 0
                       then Value := false
                       else Value := true;
   fbpShortInt      : Value := PropData.VShortInt;
   fbpByte          : Value := PropData.VByte;
   fbpWord          : Value := PropData.VWord;
   {$IFDEF FG_D6}
   fbpLongWord      : Value := PropData.VLongWord;
   fbpInt64         : Value := PropData.VInt64;
   {$ELSE}
   fbpLongWord      : Value := integer(PropData.VLongWord);
   fbpInt64         : FOwner.ReadError;
   {$ENDIF}
   fbpString:
     begin
      {KeyIndex := integer(FCmdKeys[PropData.VString]);
      if KeyIndex = 0
       then Value := ''
       else Value := FDictionary[KeyIndex];}
      Value := ReadCmdKeys[PropData.VString];
     end;
   fbpStrList:
     begin
      // Read strings count and create variant array
      StrCount := PropData.VStrings.StrCount;
      Value := VarArrayCreate([0, StrCount-1], varVariant);
      // Read strings
      StrPtr := @PropData.VStrings.StrData;
      for i:=0 to StrCount-1 do begin
       // Read string length
       StrLen := PInteger(StrPtr)^;
       inc(StrPtr, SizeOf(StrLen));
       SetLength(s, StrLen);
       // Read string chars
       Move(StrPtr^, s[1], StrLen);
       inc(StrPtr, StrLen);
       // Set string in value
       Value[i] := s;
      end;
     end;
   fbpHexData:
     begin
      HexDataSize := FCmdReadDataSize -
        (PAnsiChar(@PropData.VHexData) - PAnsiChar(PropData));
      if HexDataSize > 0 then begin
       // Create byte array
       Value := VarArrayCreate([0, HexDataSize-1], varByte);
       // Move block
       HexData := VarArrayLock(Value);
       try
        Move(PropData.VHexData, HexData^, HexDataSize);
       finally
        VarArrayUnlock(Value);
       end;
      end else
       VarClear(Value);
     end;
  end;
 finally
  if PropData <> @PropSimpleData then FreeMem(PropData);
 end;
 Result := true;
end;

function TFlexBinaryData.LoadStrCheck(out s: string): boolean;
var Value: Variant;
    PropType: integer;
    NotSupported: boolean;
    i, Last, Size: integer;
    HexLine: AnsiString;
    HexData, Pos: PAnsiChar;
begin
 Result := false;
 if (FProcess = ppSave) or FFinished then exit;
 // Check continue reading property lines
 if FReadPropLines.Count > 0 then begin
  s := FReadPropLines[FReadPropLine];
  inc(FReadPropLine);
  if FReadPropLine = FReadPropLines.Count then begin
   FReadPropLines.Clear;
   FReadPropLine := 0;
  end;
 end else begin
  // Read new command
  ReadCommandHeaderAndKeys(FCmdReadHeader);
  case FCmdReadHeader.Command of
   fbcDocument:
     s := fcDocument + ' ' + ReadCmdKeys[0];
   fbcClipboard:
     s := fcClipboard;
   fbcLibrary:
     s := fcLibrary + ' ' + ReadCmdKeys[0];
   fbcObject:
     s := fcObject + ' ' + ReadCmdKeys[0] + ': ' + ReadCmdKeys[1];
   fbcComplexProperty:
     s := fcProperty + ' ' + ReadCmdKeys[0];
   fbcEnd:
     s := fcEnd;
   fbcBinaryEnd:
     exit;
   fbcProperty:
     // Read property
     if not ReadPropertyData(Value, PropType) then
      // Read error. Skip line
      s := ''
     else begin
      // Convert property data to string
      NotSupported := false;
      case PropType of
       fbpSmallint,
       fbpInteger,
       fbpShortInt,
       fbpByte,
       fbpWord,
       fbpLongWord,
       fbpInt64:
         s := Value;
       fbpSingle,
       fbpDouble,
       fbpCurrency,
       fbpDate:
         s := DotFloatToStr(Value);
       fbpBoolean:
         s := BooleanWords[boolean(Value)];
       fbpString:
         begin
          s := Value;
          if FlexStrNeedQuote(s) then s := '''' + s + '''';
         end;
       fbpStrList:
         if VarIsEmpty(Value) or VarIsNull(Value) then
          s := '( )'
         else begin
          s := '(';
          // Generate string list lines in FReadPropLines for further calls
          Last := VarArrayHighBound(Value, 1);
          for i:=0 to Last do
           if i = Last
            then FReadPropLines.Add('''' + string(Value[i]) + ''' )')
            else FReadPropLines.Add('''' + string(Value[i]) + '''');
         end;
       fbpHexData:
         if VarIsEmpty(Value) or VarIsNull(Value) then
          s := '{ }'
         else begin
          s := '{';
          // Generate hex lines list in FReadPropLines for further calls
          Last := VarArrayHighBound(Value, 1);
          Size := 2*(Last+1) +2;
          SetLength(HexLine, Size);
          Pos := PAnsiChar(HexLine);
          HexData := VarArrayLock(Value);
          try
           for i:=0 to Last do begin
            pword(Pos)^ := ByteToHexChars(byte(HexData[i]));
            inc(Pos, 2);
           end;
           Pos^ := ' ';
           Pos[1] := '}';
          finally
           VarArrayUnlock(Value);
          end;
         end;
       else
         // Unknown or unsupported property type
         NotSupported := true;
      end;
      if NotSupported
       then s := ''
       else s := ReadCmdKeys[0] + ' = ' + s;
     end;
   else
     if FCmdReadHeader.Command >= fbcUser then
      // User commands
      s := ''
     else
      // Unknown command;
      exit;
  end;
 end;
 Result := true;
end;

procedure TFlexBinaryData.SaveStr(const s: string);
const
  KeyWords: array[0..6] of record
   KeyWord: String;
   Command: integer;
  end = (
   ( KeyWord: fcDocument;   Command: fbcDocument ),
   ( KeyWord: fcClipboard;  Command: fbcClipboard ),
   ( KeyWord: fcLibrary;    Command: fbcLibrary ),
   ( KeyWord: fcObject;     Command: fbcObject ),
   ( KeyWord: fcProperty;   Command: fbcComplexProperty ),
   ( KeyWord: fcEnd;        Command: fbcEnd ),
   ( KeyWord: fcBinary;     Command: -1 )
  );

 procedure WriteProp;
 begin
  // Write buffer
  WriteDataCommand(fbcProperty, FWritePropData^, FWritePropDataPos, 1,
    FWritePropName);
  // Reset write buffer
  FWritePropDataPos := 0;
  FWritePropType := fbpEmpty;
 end;

 function ThisWord(const s: string; Pos, Len: integer;
   const AWord: string): boolean;
 var i: integer;
     LastLen: integer;
 begin
  LastLen := Len - Pos +1;
  Result := LastLen = Length(AWord);
  if not Result then exit;
  for i:=1 to LastLen do
   if s[Pos + i -1] <> AWord[i] then begin
    Result := false;
    break;
   end;
 end;

var Line, Name, Value, NewValue: string;
    AnsiLine: AnsiString;
    Command: integer;
    i, j, Len, KeyEnd, Size: integer;
    PropData: TFlexBinPropertyData;
    LastLine, IsIntNumber: boolean;
    Buf: PAnsiChar;
    IntNumber: integer;
begin
 if (FProcess = ppLoad) or FFinished then exit;
 // Skip left blanks
 Line := TrimLeft(s);
 AnsiLine := AnsiString(Line);
 Len := Length(Line);
 // Check string list or hex block saving
 case FWritePropType of
  fbpStrList:
    begin
     LastLine := AnsiLine[Len] = ')';
     if LastLine then dec(Len);
     while (Len > 0) and (AnsiLine[Len] = ' ') do dec(Len);
     if (Len > 1) and (AnsiLine[1] = '''') and (AnsiLine[Len] = '''') then begin
      // Add new line to write buffer
      dec(Len, 2);
      Size := SizeOf(Integer) + Len;
      GrowWriteData(Size);
      Buf := PAnsiChar(FWritePropData) + FWritePropDataPos;
      PInteger(Buf)^ := Len;
      inc(PInteger(Buf));
      for i:=0 to Len-1 do Buf[i] := AnsiLine[i+2];
      inc(FWritePropDataPos, Size);
      // Increment line count
      inc(PFlexBinPropertyData(FWritePropData).VInteger);
     end;
     if LastLine then WriteProp;
    end;
  fbpHexData:
    begin
     LastLine := AnsiLine[Len] = '}';
     if LastLine then dec(Len);
     while (Len > 0) and (AnsiLine[Len] = ' ') do dec(Len);
     if Len > 1 then begin
      // Add new line to write buffer
      Size := Len div 2;
      GrowWriteData(Size);
      Buf := PAnsiChar(FWritePropData) + FWritePropDataPos;
      for i:=0 to Size-1 do
       Buf[i] := AnsiChar(
         HexCharsToByte(Byte(AnsiLine[2*i+1]) shl 8 or Byte(AnsiLine[2*i+2]))
       );
      inc(FWritePropDataPos, Size);
     end;
     if LastLine then WriteProp;
    end;
  else
    begin
     // Define command
     KeyEnd := 1;
     Command := fbcProperty;
     for i:=Low(KeyWords) to High(KeyWords) do
      if StrBeginsFrom(KeyWords[i].KeyWord, Line) then begin
       Command := KeyWords[i].Command;
       KeyEnd := Length(KeyWords[i].KeyWord);
       inc(KeyEnd);
       if (Length(Line) > KeyEnd) and (Line[KeyEnd+1] = ' ') then inc(KeyEnd);
       break;
      end;
     // Check binary keyword. Since we already in binary mode nothing need to save
     if Command < 0 then exit;
     // Save command
     case Command of
      fbcDocument,
      fbcLibrary,
      fbcComplexProperty:
        // Get name from line
        WriteSimpleCommand(Command, 1, copy(Line, KeyEnd+1, MaxInt));
      fbcClipboard,
      fbcEnd:
        // Command without keys
        WriteSimpleCommand(Command);
      fbcObject:
        begin
         // Find class name start
         i := Len;
         while (i >= KeyEnd) and (Line[i] <> ':') do dec(i);
         if Line[i] <> ':' then
          raise EWriteError.Create(SWriteError);
         // Save command
         WriteSimpleCommand(Command, 2,
           Trim(copy(Line, KeyEnd, i - KeyEnd)),  // Object name
           Trim(copy(Line, i+1, MaxInt))          // Class name
         );
        end;
      fbcProperty:
        begin
         i := 1;
         while (i <= Len) and (Line[i] <> '=') do inc(i);
         if Line[i] <> '=' then FOwner.WriteError;
         // Get property name
         Name := Trim(copy(Line, 1, i-1));
         // Skip blanks
         inc(i);
         while (i <= Len) and (Line[i] = ' ') do inc(i);
         // Define property type
         case Line[i] of
          '1'..'9', '-': // Without leading zero!!
            // Integer Number
            begin
             // Check all chars
             IsIntNumber := false;
             for j:=i+1 to Len do begin
              IsIntNumber := (Line[j] >= '0') and (Line[j] <= '9');
              if not IsIntNumber then break;
             end;
             Value := copy(Line, i, MaxInt);
             if IsIntNumber then
             try
              IntNumber := StrToInt(Value);
              // Test successful conversion
              NewValue := IntToStr(IntNumber);
              for j:=i to Len do
               if Line[j] <> NewValue[j-i+1] then begin
                IsIntNumber := false;
                break;
               end;
              if IsIntNumber then
               WritePropertyCommand(Name, fbpInteger, IntNumber);
             except
              IsIntNumber := false;
             end;
             if not IsIntNumber then
              // Save as string
              WritePropertyCommand(Name, fbpString, Value);
            end;
          '''':
            if Line[Len] = '''' then
             // String
             WritePropertyCommand(Name, fbpString, copy(Line, i+1, Len-i -1))
            else
             raise EWriteError.Create(SWriteError);
          '(':
            begin
             Size := SizeOf(PropData.PropType) + SizeOf(PropData.VInteger);
             if Line[Len] = ')' then begin
              // Write empty list
              PropData.PropType := fbpStrList;
              PropData.VInteger := 0; // Line count
              WriteDataCommand(fbcProperty, PropData, Size, 1, Name);
             end else begin
              // Start collecting string list lines
              with PFlexBinPropertyData(GrowWriteData(Size))^ do begin
               PropType := fbpStrList;
               VInteger := 0; // Line count
              end;
              inc(FWritePropDataPos, Size);
              FWritePropName := Name;
              FWritePropType := fbpStrList;
             end;
            end;
          '{':
            begin
             Size := SizeOf(PropData.PropType);
             if Line[Len] = '}' then begin
              // Write empty hex block
              PropData.PropType := fbpHexData;
              WriteDataCommand(fbcProperty, PropData, Size, 1, Name);
             end else begin
              // Start collecting hex block lines
              PFlexBinPropertyData(GrowWriteData(Size))^.PropType :=
                fbpHexData;
              inc(FWritePropDataPos, Size);
              FWritePropName := Name;
              FWritePropType := fbpHexData;
             end;
            end;
          else
            // Check TRUE
            if ThisWord(Line, i, Len, BooleanWords[True]) then
             WritePropertyCommand(Name, fbpBoolean, true)
            else
            // Check FALSE
            if ThisWord(Line, i, Len, BooleanWords[False]) then
             WritePropertyCommand(Name, fbpBoolean, false)
            else
             // Simple string or Enum name. Save as is
             WritePropertyCommand(Name, fbpString, copy(Line, i, MaxInt));
         end;
        end;
      else
        // Unkonwn command
        raise EWriteError.Create(SWriteError);
     end;
    end;
 end;
end;

procedure TFlexBinaryData.SaveStrBuf(Buf: pointer; BufSize: integer);
var Start, Pos, Finish: PAnsiChar;
    Delim: AnsiChar;
    Len: integer;
    Line: AnsiString;
    i: integer;
begin
 if BufSize <= 0 then exit;
 Pos := PAnsiChar(Buf);
 Finish := PAnsiChar(Buf) + BufSize;
 repeat
  Start := Pos;
  while (Pos < Finish) and (Pos^ <> #$0D) and (Pos^ <> #$0A) do inc(Pos);
  Len := Pos - Start;
  if Len > 0 then begin
   // Get line
   SetLength(Line, Len);
   for i:=1 to Len do Line[i] := Start[i-1];
   // Parse line
   SaveStr(String(Line));
  end;
  // Skip delims
  if Pos < Finish then begin
   Delim := Pos^;
   inc(Pos);
   if (Pos < Finish) and
      ((Pos^ = #$0D) or (Pos^ = #$0A)) and (Pos^ <> Delim) then
    // Skip second delim
    inc(Pos);
  end;
 until (Pos >= Finish);
end;

function TFlexBinaryData.ReadCommand: boolean;
begin
 ReadCommandHeaderAndKeys(FCmdReadHeader);
 Result := true;
end;

function TFlexBinaryData.ReadCommandData(out Value: Variant): integer;
var PropType: integer;
begin
 if not ReadPropertyData(Value, PropType)
  then Result := -1
  else Result := PropType;
end;

function TFlexBinaryData.ReadCommandUserData: pointer;
begin
 Result := FCmdReadUserData;
 if not FCmdReadDataAhead then exit;
 AllocReadData;
 FOwner.ReadBuf(FCmdReadUserData^, FCmdReadUserDataSize);
 FCmdReadDataAhead := false;
 Result := FCmdReadUserData;
end;

// TFlexFiler ////////////////////////////////////////////////////////////////

constructor TFlexFiler.Create(AStream: TStream;
  ACompleteBinary: boolean = false);
begin
 inherited Create;
 FStream := AStream;
 FCompleteBinary := ACompleteBinary;
 FBufSize := 65536; //4096;
 GetMem(FBuffer, FBufSize);
 try
  FTotal := GetStreamSize;
  FLoaded := FStream.Seek(0, soFromCurrent);
 except

 end;
 Binary := FCompleteBinary;
end;

destructor TFlexFiler.Destroy;
begin
 {try
  // If was binary mode we need save fbcBinaryEnd command
  Binary := false;
 except
 end;}
 FBinaryData.Free;
 if Assigned(FBuffer) then begin
  FreeMem(FBuffer, FBufSize);
  FBuffer := Nil;
 end;
 inherited;
end;

procedure TFlexFiler.ReadBuffer;
begin
 FBufEnd := FStream.Read(FBuffer^, FBufSize);
 //if FBufEnd = 0 then raise EReadError.CreateRes(@SReadError);
 FBufPos := 0;
 inc(FLoaded, FBufEnd);
end;

function TFlexFiler.ReadSkipBuf(BufSize: integer): boolean;
var Size: integer;
begin
 Result := BufSize <= 0;
 if Result or (FBufEnd = 0) then exit;
 repeat
  if FBufPos = FBufEnd then begin
   ReadBuffer;
   if FBufEnd = 0 then break;
  end;
  if FBufPos + BufSize > FBufEnd
   then Size := FBufEnd - FBufPos
   else Size := BufSize;
  inc(FBufPos, Size);
  dec(BufSize, Size);
  Result := BufSize = 0;
 until Result;
 DoProgress(ppLoad);
end;

function TFlexFiler.ReadBufCheck(Buf: pointer; BufSize: integer): boolean;
var LoadBufPos: PAnsiChar;
    Size: integer;
begin
 Result := BufSize <= 0;
 if Result or (FBufEnd = 0) then exit;
 LoadBufPos := PAnsiChar(Buf);
 repeat
  if FBufPos = FBufEnd then begin
   ReadBuffer;
   if FBufEnd = 0 then break;
  end;
  if FBufPos + BufSize > FBufSize
   then Size := FBufSize - FBufPos
   else Size := BufSize;
  Move((PAnsiChar(FBuffer) + FBufPos)^, LoadBufPos^, Size);
  inc(LoadBufPos, Size);
  inc(FBufPos, Size);
  dec(BufSize, Size);
  Result := BufSize = 0;
 until Result;
 DoProgress(ppLoad);
end;

procedure TFlexFiler.ReadBuf(var Buf; BufSize: integer);
begin
 if not ReadBufCheck(@Buf, BufSize) then ReadError;
end;

procedure TFlexFiler.WriteBuf(const Buf; BufSize: integer);
begin
 FStream.WriteBuffer(Buf, BufSize);
end;

procedure TFlexFiler.SaveStr(const s: string);
const EOLN: word = $0A0D;
{$IFDEF FG_D12}
var a: AnsiString;
{$ENDIF}
begin
 if FBinary then
  FBinaryData.SaveStr(s)
 else begin
  if Length(s) > 0 then begin
   {$IFDEF FG_D12}
   a := AnsiString(s);
   FStream.Write(a[1], Length(a));
   {$ELSE}
   FStream.Write(s[1], Length(s));
   {$ENDIF}
  end;
  FStream.Write(EOLN, SizeOf(EOLN));
 end;
 FLastProcess := ppSave;
end;

procedure TFlexFiler.SaveBuf(Buf: pointer; BufSize: integer);
begin
 if FBinary
  then FBinaryData.SaveStrBuf(Buf, BufSize)
  else FStream.WriteBuffer(Buf^, BufSize);
 FLastProcess := ppSave;
end;

function TFlexFiler.LoadStr: string;
begin
 LoadStrCheck(Result);
end;

function TFlexFiler.LoadStrCheck(out s: string): boolean;
var StrBeg, StrEnd, Len: integer;
    Delim: AnsiChar;
    a: AnsiString;
begin
 if FBinary then
  Result := FBinaryData.LoadStrCheck(s)
 else begin
  a := '';
  StrBeg := -1;
  StrEnd := -1;
  repeat
   if (FBufPos = FBufEnd) then begin
    ReadBuffer;
    if FBufEnd = 0 then break;
    StrBeg := FBufPos;
   end;
   if StrEnd < 0 then begin
    StrBeg := FBufPos;
    while (StrBeg < FBufEnd) and (PAnsiChar(FBuffer)[StrBeg] = ' ') do inc(StrBeg);
    FBufPos := StrBeg;
    if FBufPos = FBufEnd then continue;
   end;
   StrEnd := StrBeg;
   while (StrEnd < FBufEnd) and (PAnsiChar(FBuffer)[StrEnd] <> #$0D) and
         (PAnsiChar(FBuffer)[StrEnd] <> #$0A) do inc(StrEnd);
   Delim := PAnsiChar(FBuffer)[StrEnd];
   FBufPos := StrEnd;
   if StrEnd > StrBeg then begin
    Len := Length(a);
    SetLength(a, Len + (StrEnd - StrBeg));
    Move(PAnsiChar(FBuffer)[StrBeg], a[Len+1], StrEnd - StrBeg);
   end;
   if FBufPos = FBufEnd then continue;
   inc(FBufPos);
   if FBufPos = FBufEnd then begin
    ReadBuffer;
    if FBufEnd = 0 then break;
   end;
   if ((PAnsiChar(FBuffer)[FBufPos] = #$0D) or (PAnsiChar(FBuffer)[FBufPos] = #$0A)) and
      (PAnsiChar(FBuffer)[FBufPos] <> Delim) then
    inc(FBufPos);
   break;
  until false;
  Result := FBufEnd > 0;
 { if Result and (FBufPos = FBufEnd) then begin
   ReadBuffer;
   Result := FBufEnd > 0;
  end; }
  s := String(a);
 end;
 DoProgress(ppLoad);
 if not FBinary and
    (Length(s) = Length(fcBinary)) and StrBeginsFrom(fcBinary, s) then
  Binary := true;
end;

procedure TFlexFiler.LoadSkipToEnd;
var s: string;
    Level: integer;
begin
 Level := 1;
 while Level > 0 do
  if Binary then begin
   if not BinaryData.ReadCommand then break;
   case BinaryData.ReadCmdCommand of
    fbcObject,
    fbcComplexProperty  : inc(Level);
    fbcEnd              : dec(Level)
   end;
  end else
  if not LoadStrCheck(s) then
   break
  else
  if StrBeginsFrom(s, fcEnd) then
   dec(Level)
  else
  if StrBeginsFrom(s, fcObject) or StrBeginsFrom(s, fcProperty) then
   inc(level);
end;

function TFlexFiler.CheckLoadSkipToEnd(const First: string): boolean;
begin
 Result :=
   StrBeginsFrom(First, fcDocument) or
   StrBeginsFrom(First, fcClipboard) or
   StrBeginsFrom(First, fcLibrary) or
   StrBeginsFrom(First, fcObject) or
   StrBeginsFrom(First, fcProperty);
 if Result then LoadSkipToEnd;
end;

function TFlexFiler.IsEndOfStream: boolean;
var EndPos, Pos: Longint;
begin
 Pos := FStream.Seek(0, soFromCurrent);
 EndPos := FStream.Seek(0, soFromEnd);
 FStream.Seek(Pos, soFromBeginning);
 Result := Pos = EndPos;
end;

procedure TFlexFiler.Rewind;
begin
 Binary := false;
 FStream.Position := 0;
 FLoaded := 0;
 FSaved := 0;
 FLastProcess := ppUnknown;
end;

function TFlexFiler.GetStreamSize: integer;
var Pos: integer;
begin
 try
  Pos := FStream.Seek(0, soFromCurrent);
  Result:= FStream.Seek(0, soFromEnd);
  FStream.Seek(Pos, soFromBeginning);
 except
  Result := 0;
 end;
end;

procedure TFlexFiler.SetSaved(const Value: integer);
begin
 if Value = FSaved then exit;
 FSaved := Value;
 DoProgress(ppSave);
end;

procedure TFlexFiler.SetTotal(const Value: integer);
begin
 if Value = FTotal then exit;
 FTotal := Value;
end;

procedure TFlexFiler.DoProgress(Process: TFlexFilerProcess);
var NewProgress: integer;
begin
 if not Assigned(FOnProgress) or (FTotal = 0) then exit;
 case Process of
  ppLoad: NewProgress := Round((FLoaded - FBufEnd + FBufPos) / FTotal * 100);
  ppSave: NewProgress := Round(FSaved / FTotal * 100);
  else exit;
 end;
 FLastProcess := Process;
 if NewProgress <> FProgress then begin
  FProgress := NewProgress;
  FOnProgress(Self, FProgress, Process);
 end;
end;

procedure TFlexFiler.SetBinary(const Value: boolean);
begin
 if Value = FBinary then exit;
 if CompleteBinary then begin
  if not Value and Assigned(FBinaryData) then FBinaryData.Reset;
  exit;
 end;
 FBinary := Value;
 if FBinary then begin
  if not Assigned(FBinaryData)
   then FBinaryData := TFlexBinaryData.Create(Self)
   else FBinaryData.Reset;
 end else
 if Assigned(FBinaryData) and not FBinary and
    (FBinaryData.Process = ppSave) and not FBinaryData.Finished then begin
  // Save binary end command and reset
  FBinaryData.WriteSimpleCommand(fbcBinaryEnd);
  FBinaryData.Reset;
 end;
end;

procedure TFlexFiler.ReadError(const Msg: string);
begin
 if Msg <> '' then
  raise EReadError.Create(Msg)
 else
  {$IFDEF FG_D5}
  raise EReadError.CreateRes(@SReadError)
  {$ELSE}
  raise EReadError.Create(SReadError);
  {$ENDIF}
end;

procedure TFlexFiler.WriteError(const Msg: string);
begin
 if Msg <> '' then
  raise EWriteError.Create(Msg)
 else
  {$IFDEF FG_D5}
  raise EWriteError.CreateRes(@SWriteError)
  {$ELSE}
  raise EWriteError.Create(SWriteError);
  {$ENDIF}
end;

// TIdPool ///////////////////////////////////////////////////////////////

constructor TIdPool.Create;
begin
 FPool := TList.Create;
end;

destructor TIdPool.Destroy;
begin
 FPool.Free;
 inherited;
end;

function TIdPool.Generate: cardinal;
begin
 if FPool.Count = 0 then begin
  // Generate first identifier
  FPool.Add(pointer(1));
  FPool.Add(pointer(1));
  Result := 1;
 end else
 if integer(FPool[0]) > 1 then begin
  // The indentifier 1 is not used
  if integer(FPool[0]) = 2 then begin
   FPool[0] := pointer(1);
   Result := 1;
  end else begin
   FPool.Insert(0, pointer(1));
   FPool.Insert(0, pointer(1));
   Result := 1;
  end;
 end else begin
  Result := cardinal(FPool[1]);
  inc(Result);
  if (FPool.Count > 2) and (cardinal(FPool[2]) = Result+1) then begin
   // Combine neighbor regions
   FPool.Delete(2);
   FPool.Delete(1);
  end else
   // Use identifier
   FPool[1] := pointer(Result);
 end; 
end;

function TIdPool.GetUsed(Value: cardinal): boolean;
var Index: integer;
begin
 Result := false;
 if Value <= 0 then exit;
 Index := ListScanLess(pointer(Value), FPool.List, FPool.Count);
 Result := (Index < FPool.Count) and
          ((Index and 1 <> 0) or (cardinal(FPool[Index]) = Value));
end;

function TIdPool.Use(Value: cardinal): boolean;
var Index: integer;
begin
 Result := false;
 if Value <= 0 then exit;
 Index := ListScanLess(pointer(Value), FPool.List, FPool.Count);
 if Index = FPool.Count then begin
  // Value greater then last used identifier or FPool is empty
  if (FPool.Count > 0) and (cardinal(FPool[FPool.Count-1]) = Value-1) then
   FPool[FPool.Count-1] := pointer(Value)
  else begin
   FPool.Add(pointer(Value));
   FPool.Add(pointer(Value));
  end;
  Result := true;
 end else
 if (cardinal(FPool[Index]) <> Value) and (Index and 1 = 0) then begin
  // Value is not in use
  if cardinal(FPool[Index]) = Value+1 then begin
   // Insert value in used region
   if (Index > 0) and (cardinal(FPool[Index-1]) = Value-1) then begin
    // Cancat with previous region
    FPool.Delete(Index);
    FPool.Delete(Index-1);
   end else
    // insert
    FPool[Index] := pointer(Value);
  end else
  if (Index > 0) and (cardinal(FPool[Index-1]) = Value-1) then
   // Insert value in previous region
   FPool[Index-1] := pointer(Value)
  else begin
   // Create new region
   FPool.Insert(Index, pointer(Value));
   FPool.Insert(Index, pointer(Value));
  end;
  Result := true;
 end;
end;

function TIdPool.Release(Value: cardinal): boolean;
var Index: integer;
begin
 Result := false;
 if Value <= 0 then exit;
 Index := ListScanLess(pointer(Value), FPool.List, FPool.Count);
 if Index = FPool.Count then exit; // Value not used
 // Value can be in use
 if cardinal(FPool[Index]) = Value then begin
  // Released value is start or end of used region
  if Index and 1 = 0 then begin
   // It is start of region
   if cardinal(FPool[Index+1]) = Value then begin
    // Delete region
    FPool.Delete(Index+1);
    FPool.Delete(Index);
   end else
    // Move start of region
    FPool[Index] := pointer(Value+1);
  end else begin
   // It is end of region
   if cardinal(FPool[Index-1]) = Value then begin
    // Delete region
    FPool.Delete(Index);
    FPool.Delete(Index-1);
   end else
    // Move end of region
    FPool[Index] := pointer(Value-1);
  end;
  Result := true;
 end else
 if Index and 1 <> 0 then begin
  // Break region
  FPool.Insert(Index, pointer(Value+1));
  FPool.Insert(Index, pointer(Value-1));
  Result := true;
 end;
end;

function TIdPool.NextUsed(Value: cardinal; var Index: integer): cardinal;
begin
 Result := 0;
 if Index < 0 then begin
  if FPool.Count = 0 then exit;
  Index := 0;
  Result := cardinal(FPool[Index]);
 end else
 if (Index < FPool.Count-1) and (Index and 1 = 0) then begin
  Result := Value + 1;
  while (Index < FPool.Count-1) and (Result > cardinal(FPool[Index+1])) do
   inc(Index, 2);
  if Index = FPool.Count then
   Result := 0
  else
  if Result < cardinal(FPool[Index]) then
   Result := cardinal(FPool[Index]);
 end;
end;

procedure TIdPool.Clear;
begin
 FPool.Clear;
end;

// TNotifyLink //////////////////////////////////////////////////////////////////

constructor TNotifyLink.Create(AOwner: TObject);
begin
 if not Assigned(AOwner) then raise Exception.Create('Owner must exist');
 FOwner := AOwner;
 FLinks := TList.Create;
end;

destructor TNotifyLink.Destroy;
begin
 DestroyNotify;
 FLinks.Free;
 inherited;
end;

function TNotifyLink.GetLinkCount: integer;
begin
 Result := FLinks.Count div 2;
end;

function TNotifyLink.GetLink(Index: integer): TNotifyLink;
begin
 Result := TNotifyLink(FLinks[Index*2+1]);
end;

function TNotifyLink.GetLinkRefCount(Index: integer): integer;
begin
 Result := integer(FLinks[Index*2]);
end;

function TNotifyLink.IndexOf(Link: TNotifyLink): integer;
var i: integer;
begin
 Result := -1;
 i := 0;
 while i < FLinks.Count do
  if FLinks[i+1] = Link then begin
   Result := i;
   break;
  end else
   inc(i, 2);
end;

function TNotifyLink.Notify(const Info: TNotifyLinkInfo): integer;
var i: integer;
begin
 Result := 0;
 if FDestroying then exit;
 i := 0;
 while i < FLinks.Count do with TNotifyLink(FLinks[i+1]) do begin
  if (integer(Self.FLinks[i]) > 0) and Assigned(FOnNotify) and
     not FDestroying then begin
   try
    FOnNotify(Self.FLinks[i+1], Self, Info);
   except
    Application.HandleException(Self);
   end;
   inc(Result);
  end;
  inc(i, 2);
 end;
end;

function TNotifyLink.ControlNotify(AControl: TObject;
  ANotify: TFlexNotify): integer;
var Info: TNotifyLinkInfo;
begin
 Info.Code := ncControlNotify;
 Info.Control := AControl;
 Info.ControlNotify := ANotify;
 Result := Notify(Info);
end;

function TNotifyLink.PropNotify(AProp: TObject;
  IsBeforeNotify: boolean): integer;
var Info: TNotifyLinkInfo;
begin
 if IsBeforeNotify
  then Info.Code := ncPropBeforeChanged
  else Info.Code := ncPropChanged;
 Info.Prop := AProp;
 Result := Notify(Info);
end;

procedure TNotifyLink.DestroyNotify;
var Index: integer;
    Link: TNotifyLink;
    Info: TNotifyLinkInfo;
begin
 FillChar(Info, SizeOf(Info), 0);
 Info.Code := ncDestroy;
 if not FDestroying then begin
  FDestroying := true;
  if Assigned(FOnFreeNotify) then FOnFreeNotify(Self, Nil, Info);
 end;
 while FLinks.Count > 0 do begin
  Link := TNotifyLink(FLinks[FLinks.Count-1]);
  with Link do begin
   Index := IndexOf(Self);
   if Index >= 0 then begin
    // Fully unsubscribe
    if Assigned(FOnFreeNotify) then begin
     try
      FOnFreeNotify(Link, Self, Info);
     except
      Application.HandleException(Self);
     end;
     Index := IndexOf(Self);
    end;
    if Index >= 0 then begin
     FLinks.Delete(Index);
     FLinks.Delete(Index);
    end;
   end;
  end;
  FLinks.Count := FLinks.Count - 2;
 end;
end;

function TNotifyLink.Subscribe(Link: TNotifyLink): integer;
var Index: integer;
begin
 if not Assigned(Link) or FDestroying then begin
  Result := -1;
  exit;
 end;
 Index := IndexOf(Link);
 if Index = -1 then begin
  // New subscription
  FLinks.Add(pointer(1));
  FLinks.Add(Link);
  Link.FLinks.Add(pointer(0));
  Link.FLinks.Add(Self);
  Result := 1;
 end else begin
  // Increase subscription count
  Result := integer(FLinks[Index]) + 1;
  FLinks[Index] := pointer(Result);
 end;
end;

function TNotifyLink.Unsubscribe(Link: TNotifyLink): integer;
var Index, LinkIndex: integer;
begin
 Result := -1;
 if not Assigned(Link) or FDestroying then exit;
 Index := IndexOf(Link);
 if Index < 0 then exit; // Nothing unsubscribe
 // Decrease subscription count
 Result := integer(FLinks[Index]) - 1;
 if Result < 0 then exit; // already unsubscribed
 FLinks[Index] := pointer(Result);
 if Result = 0 then begin
  // Check fully unsubscribe
  LinkIndex := Link.IndexOf(Self);
  if (LinkIndex >= 0) and (integer(Link.FLinks[LinkIndex]) = 0) then begin
   FLinks.Delete(Index);
   FLinks.Delete(Index);
   Link.FLinks.Delete(LinkIndex);
   Link.FLinks.Delete(LinkIndex);
  end;
 end;
end;

///////////////////////////////////////////////////////////////////////////////

procedure LoadFlexCursors;
begin
 Screen.Cursors[crShapeCursor] := LoadCursor(HInstance, 'SHAPE_CUR');
 if Screen.Cursors[crShapeCursor] = 0 then
  Screen.Cursors[crShapeCursor] := LoadCursor(HInstance, IDC_UPARROW);
 Screen.Cursors[crShapeAddCursor] := LoadCursor(HInstance, 'SHAPE_ADD_CUR');
 Screen.Cursors[crShapeDelCursor] := LoadCursor(HInstance, 'SHAPE_DEL_CUR');
 Screen.Cursors[crShapeCloseCursor] := LoadCursor(HInstance, 'SHAPE_CLOSE_CUR');
 Screen.Cursors[crShapeMoveCursor] := LoadCursor(HInstance, 'SHAPE_MOVE_CUR');
 Screen.Cursors[crShapeMoveCurveCursor] := LoadCursor(HInstance, 'SHAPE_MOVE_CURVE_CUR');
 Screen.Cursors[crShapeMoveLineCursor] := LoadCursor(HInstance, 'SHAPE_MOVE_LINE_CUR');
 Screen.Cursors[crCreateControlCursor] := LoadCursor(HInstance, 'CREATE_CTRL_CUR');
 Screen.Cursors[crCreateRectCursor] := LoadCursor(HInstance, 'CREATE_RECT_CUR');
 Screen.Cursors[crCreateEllipseCursor] := LoadCursor(HInstance, 'CREATE_ELLIPSE_CUR');
 Screen.Cursors[crCreateTextCursor] := LoadCursor(HInstance, 'CREATE_TEXT_CUR');
 Screen.Cursors[crCreatePicCursor] := LoadCursor(HInstance, 'CREATE_PIC_CUR');
 Screen.Cursors[crCreatePolyCursor] := LoadCursor(HInstance, 'CREATE_POLY_CUR');
 Screen.Cursors[crZoomInCursor] := LoadCursor(HInstance, 'ZOOM_IN_CUR');
 Screen.Cursors[crZoomOutCursor] := LoadCursor(HInstance, 'ZOOM_OUT_CUR');
 Screen.Cursors[crPanCursor] := LoadCursor(HInstance, 'PAN_CUR');
 Screen.Cursors[crPanningCursor] := LoadCursor(HInstance, 'PANNING_CUR');
 Screen.Cursors[crShapeContinueCursor] :=LoadCursor(HInstance, 'CONT_POLY_CUR');
end;

///////////////////////////////////////////////////////////////////////////////

function StrBeginsFrom(const S1, S2: string): boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@1
        MOV     EAX,[EAX-4]
@@1:    OR      EDX,EDX
        JE      @@2
        MOV     EDX,[EDX-4]
@@2:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@3
        MOV     ECX,EDX
@@3:    XOR     EAX,EAX
        CMP     ECX,ECX
        {$IFDEF UNICODE}
        REPE    CMPSW
        {$ELSE}
        REPE    CMPSB
        {$ENDIF}
        JE      @@4
        DEC     EAX
@@4:    INC     EAX
        POP     EDI
        POP     ESI
end;

function FlexStrNeedQuote(const s: string): boolean;
var i: integer;
begin
 Result := (Length(s) > 0) and ( (s[1] = '(') or (s[1] = '{') );
 if Result then exit;
 for i:=1 to Length(s) do
  if (s[i] <= ' ') or (s[i] = '''') then begin
   Result := True;
   break;
  end;
end;

function ExtractWord(const s: string; NumWord: integer; Delimiter: char): string;
var i, WordBeg, WordEnd: integer;
    Len: integer;
begin
 Len := Length(s);
 if (NumWord < 1) or (Len = 0) then begin
  Result := '';
  exit;
 end;
 WordBeg := 1;
 WordEnd := Len;
 for i:=1 to Len do
  if s[i] = Delimiter then begin
   dec(NumWord);
   if NumWord = 1 then
    WordBeg := i+1
   else
   if NumWord = 0 then begin
    WordEnd := i-1;
    break;
   end;
  end;
 if NumWord <= 1
  then Result := copy(s, WordBeg, WordEnd - WordBeg + 1)
  else Result := '';
end;

const
  HexCodes: array[0..15+7] of byte = (
    0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,0,10,11,12,13,14,15
  );

function HexCharsToByte(cw: word): byte; assembler;
asm
        push    ebx
        push    edi
        lea     edi, HexCodes
        movzx   eax, ax
        sub     eax, 00003030h
        movzx   ebx, al
        movzx   ebx, byte ptr [edi + ebx]
        shr     eax, 8
        movzx   eax, byte ptr [edi + eax]
        shl     eax, 4
        or      eax, ebx
        pop     edi
        pop     ebx
end;

const
  HexWords: array[0..255] of word = (
    $3030, $3130, $3230, $3330, $3430, $3530, $3630, $3730,
    $3830, $3930, $4130, $4230, $4330, $4430, $4530, $4630,
    $3031, $3131, $3231, $3331, $3431, $3531, $3631, $3731,
    $3831, $3931, $4131, $4231, $4331, $4431, $4531, $4631,
    $3032, $3132, $3232, $3332, $3432, $3532, $3632, $3732,
    $3832, $3932, $4132, $4232, $4332, $4432, $4532, $4632,
    $3033, $3133, $3233, $3333, $3433, $3533, $3633, $3733,
    $3833, $3933, $4133, $4233, $4333, $4433, $4533, $4633,
    $3034, $3134, $3234, $3334, $3434, $3534, $3634, $3734,
    $3834, $3934, $4134, $4234, $4334, $4434, $4534, $4634,
    $3035, $3135, $3235, $3335, $3435, $3535, $3635, $3735,
    $3835, $3935, $4135, $4235, $4335, $4435, $4535, $4635,
    $3036, $3136, $3236, $3336, $3436, $3536, $3636, $3736,
    $3836, $3936, $4136, $4236, $4336, $4436, $4536, $4636,
    $3037, $3137, $3237, $3337, $3437, $3537, $3637, $3737,
    $3837, $3937, $4137, $4237, $4337, $4437, $4537, $4637,
    $3038, $3138, $3238, $3338, $3438, $3538, $3638, $3738,
    $3838, $3938, $4138, $4238, $4338, $4438, $4538, $4638,
    $3039, $3139, $3239, $3339, $3439, $3539, $3639, $3739,
    $3839, $3939, $4139, $4239, $4339, $4439, $4539, $4639,
    $3041, $3141, $3241, $3341, $3441, $3541, $3641, $3741,
    $3841, $3941, $4141, $4241, $4341, $4441, $4541, $4641,
    $3042, $3142, $3242, $3342, $3442, $3542, $3642, $3742,
    $3842, $3942, $4142, $4242, $4342, $4442, $4542, $4642,
    $3043, $3143, $3243, $3343, $3443, $3543, $3643, $3743,
    $3843, $3943, $4143, $4243, $4343, $4443, $4543, $4643,
    $3044, $3144, $3244, $3344, $3444, $3544, $3644, $3744,
    $3844, $3944, $4144, $4244, $4344, $4444, $4544, $4644,
    $3045, $3145, $3245, $3345, $3445, $3545, $3645, $3745,
    $3845, $3945, $4145, $4245, $4345, $4445, $4545, $4645,
    $3046, $3146, $3246, $3346, $3446, $3546, $3646, $3746,
    $3846, $3946, $4146, $4246, $4346, $4446, $4546, $4646
  );

function ByteToHexChars(b: byte): word; assembler;
asm
        movzx   eax, al
        movzx   eax, word ptr [HexWords + eax*2]
end;

{$IFNDEF FG_D12}
function RectWidth(const ARect: TRect): integer; assembler;
asm
        PUSH    [EAX].TRect.Left
        MOV     EAX, [EAX].TRect.Right
        SUB     EAX, [ESP]
        ADD     ESP, 4
end;

function RectHeight(const ARect: TRect): integer; assembler;
asm
        PUSH    [EAX].TRect.Top
        MOV     EAX, [EAX].TRect.Bottom
        SUB     EAX, [ESP]
        ADD     ESP, 4
end;
{$ENDIF}

{ TDummyPicture }

type
  TDummyPicture = class(TPicture)
  public
   procedure CallFiler(Filer: TFiler);
  end;

procedure TDummyPicture.CallFiler(Filer: TFiler);
begin
 DefineProperties(Filer);
end;

{ TDummyFiler }

type
  TDummyFiler = class(TFiler)
  public
   ReadProc, WriteProc: TStreamProc;
   procedure DefineProperty(const Name: string;
     ReadData: TReaderProc; WriteData: TWriterProc;
     HasData: Boolean); override;
   procedure DefineBinaryProperty(const Name: string;
     ReadData, WriteData: TStreamProc;
     HasData: Boolean); override;
   procedure FlushBuffer; override;
  end;

procedure TDummyFiler.DefineBinaryProperty(const Name: string; ReadData,
  WriteData: TStreamProc; HasData: Boolean);
begin
 ReadProc := ReadData;
 WriteProc := WriteData;
end;

procedure TDummyFiler.FlushBuffer; begin end;
procedure TDummyFiler.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean); begin end;

procedure GetPicReadWrite(Picture: TPicture;
  out ReadProc, WriteProc: TStreamProc);
var Filer: TDummyFiler;
begin
 Filer := TDummyFiler.Create(Nil, 0);
 try
  TDummyPicture(Picture).CallFiler(Filer);
  ReadProc := Filer.ReadProc;
  WriteProc := Filer.WriteProc;
 finally
  Filer.Free;
 end;
end;

///////////////////////////////////////////////////////////////////////////////

function NormalizeRect(const R: TRect): TRect;
begin
 if R.Left > R.Right then begin
  Result.Left := R.Right;
  Result.Right := R.Left;
 end else begin
  Result.Left := R.Left;
  Result.Right := R.Right;
 end;
 if R.Top > R.Bottom then begin
  Result.Top := R.Bottom;
  Result.Bottom := R.Top;
 end else begin
  Result.Top := R.Top;
  Result.Bottom := R.Bottom;
 end;
end;

function PointInRect(const p: TPoint; const R: TRect): boolean;
begin
 Result := (p.X >= R.Left) and (p.X <= R.Right) and
           (p.Y >= R.Top) and (p.Y <= R.Bottom);
end;

///////////////////////////////////////////////////////////////////////////////

function IntersectClipRgn(ACanvas: TCanvas; ClipRgn: HRGN): HRGN;
begin
 Result := CreateRectRgn(0, 0, 1, 1);
 try
  if GetClipRgn(ACanvas.Handle, Result) <> 1 then begin
   DeleteObject(Result);
   Result := 0;
   SelectClipRgn(ACanvas.Handle, ClipRgn);
  end else
   ExtSelectClipRgn(ACanvas.Handle, ClipRgn, RGN_AND);
 except
  DeleteObject(Result);
  raise;
 end;
end;

function IntersectClipPath(DC: HDC): HRGN;
begin
 Result := CreateRectRgn(0, 0, 1, 1);
 try
  if GetClipRgn(DC, Result) <> 1 then begin
   DeleteObject(Result);
   Result := 0;
   if not SelectClipPath(DC, RGN_COPY) then begin
    // Path error. Mask all output
    Result := CreateRectRgn(0, 0, 0, 0);
    SelectClipRgn(DC, Result);
    DeleteObject(Result);
    Result := 0;
   end;
  end else
   SelectClipPath(DC, RGN_AND);
 except
  DeleteObject(Result);
  raise;
 end;
end;

{$IFNDEF FG_CBUILDER}
{$IFNDEF FG_D4}
 {$DEFINE USE_SYS_GRADIENT}
{$ENDIF}
{$ENDIF}

{$IFDEF USE_SYS_GRADIENT}
function SysPaintGradient(DC: HDC; ARect: TRect; Style: TGradientStyle;
  Color, EndColor: TColor): boolean;
type
  PWinTriVertex = ^TWinTriVertex;
  TWinTriVertex = {TTriVertex;} packed record
   x: Longint;
   y: Longint;
   Red: word;
   Green: word;
   Blue: word;
   Alpha: word;
  end; 

  PVertexArray = ^TVertexArray;
  TVertexArray = array[0..MaxInt div SizeOf(TWinTriVertex) -1] of TWinTriVertex;

  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxInt div SizeOf(Integer) -1] of integer;

const
  HVMesh: array[0..5] of cardinal = ( 0, 1, 2, 0, 1, 3 );

  CenterMesh: array[0..11] of cardinal = (0, 1, 4, 1, 2, 4, 2, 3, 4, 3, 0, 4);

  Mode: array[TGradientStyle] of cardinal = (
   GRADIENT_FILL_RECT_H,   GRADIENT_FILL_RECT_V,
   GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_TRIANGLE,
   GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_TRIANGLE,
   GRADIENT_FILL_TRIANGLE, GRADIENT_FILL_TRIANGLE );

var
  Vertex: PVertexArray;
  CornerMesh: array[0..5] of cardinal;
  Index: integer;

 procedure SetVertexCount(Count: integer);
 begin
  if Assigned(Vertex)
   then ReallocMem(Vertex, Count * SizeOf(Vertex[0]))
   else GetMem(Vertex, Count * SizeOf(Vertex[0]));
 end;

 procedure SetVertex(Index, x, y: integer; Color: TColor);
 begin
  Vertex[Index].x := x;
  Vertex[Index].y := y;
  with Vertex[Index] do begin
   Red := GetRValue(Color) shl 8;
   Green := GetGValue(Color) shl 8;
   Blue := GetBValue(Color) shl 8;
   Alpha := 0;
  end;
 end;

begin
 Result := false;
 Vertex := Nil;
 try
  // Try using WinAPI GradientFill
  case Style of
   gsVertical,
   gsHorizontal:
     begin
      SetVertexCount(2);
      SetVertex(0, ARect.Left, ARect.Top, Color);
      SetVertex(1, ARect.Right, ARect.Bottom, EndColor);
      Result := GradientFill(DC, PTriVertex(Vertex), 2, @HVMesh[0], 2,
        Mode[Style]);
     end;
   gsSquare:
     begin
      SetVertexCount(5);
      with ARect do begin
       SetVertex(0, Left, Top, Color);
       SetVertex(1, Right, Top, Color);
       SetVertex(2, Right, Bottom, Color);
       SetVertex(3, Left, Bottom, Color);
       SetVertex(4, (Left+Right) div 2, (Top+Bottom) div 2, EndColor);
      end;
      Result := GradientFill(DC, PTriVertex(Vertex), 5, @CenterMesh[0], 4,
        Mode[Style]);
     end;
   gsElliptic:
     begin
      // Not implemented
     end;
   gsTopLeft,
   gsTopRight,
   gsBottomLeft,
   gsBottomRight:
     begin
      SetVertexCount(4);
      with ARect do begin
       SetVertex(0, Left, Top, EndColor);
       SetVertex(1, Right, Top, EndColor);
       SetVertex(2, Right, Bottom, EndColor);
       SetVertex(3, Left, Bottom, EndColor);
      end;
      Index := 0;
      case Style of
       gsTopLeft     : Index := 0;
       gsTopRight    : Index := 1;
       gsBottomLeft  : Index := 3;
       gsBottomRight : Index := 2;
      end;
      with Vertex[Index] do SetVertex(Index, x, y, Color);
      CornerMesh[0] := Index;
      CornerMesh[1] := (Index + 1) mod 4;
      CornerMesh[2] := (Index + 2) mod 4;
      CornerMesh[3] := Index;
      CornerMesh[4] := (Index + 3) mod 4;
      CornerMesh[5] := (Index + 2) mod 4;
      Result := GradientFill(DC, PTriVertex(Vertex), 4, @CornerMesh[0], 2,
        Mode[Style]);
     end;
  end;
 finally
  if Assigned(Vertex) then FreeMem(Vertex);
 end;
end;
{$ENDIF}

procedure PaintGradient(ACanvas: TCanvas; ARect: TRect; Style: TGradientStyle;
  Color, EndColor: TColor; PenMode: TPenMode);
var i, W, H, x, y, MaxWH: integer;
    AColor: TColor;
    Start: TRect;
    DeltaR, DeltaG, DeltaB: integer;
    StartR, StartG, StartB: integer;
    MaxDelta: integer;
    OldPenMode: TPenMode;
    {$IFDEF USE_SYS_GRADIENT} DC: HDC; {$ENDIF}
begin
 if IsRectEmpty(ARect) then exit;
 W := ARect.Right - ARect.Left;
 H := ARect.Bottom - ARect.Top;
 if W > H
  then MaxWH := W
  else MaxWH := H;
 Color := ColorToRGB(Color);
 EndColor := ColorToRGB(EndColor);
 if not SysGradientChecked or SysGradientEnabled then begin
  {$IFDEF USE_SYS_GRADIENT}
  OldPenMode := ACanvas.Pen.Mode;
  ACanvas.Pen.Style := psClear;
  ACanvas.Pen.Mode := PenMode;
  DC := ACanvas.Handle;
  SysGradientEnabled := SysPaintGradient(DC, ARect, Style, Color, EndColor);
  ACanvas.Pen.Mode := OldPenMode;
  SysGradientChecked := true;
  if SysGradientEnabled then exit;
  {$ENDIF}
 end;
 if Style in [gsTopLeft..gsBottomRight] then begin
  AColor := Color;
  Color := EndColor;
  EndColor := AColor;
 end;
 StartR := GetRValue(Color);
 StartG := GetGValue(Color);
 StartB := GetBValue(Color);
 DeltaR := GetRValue(EndColor) - StartR;
 DeltaG := GetGValue(EndColor) - StartG;
 DeltaB := GetBValue(EndColor) - StartB;
 if Abs(DeltaR) > Abs(DeltaG)
  then MaxDelta := Abs(DeltaR)
  else MaxDelta := Abs(DeltaG);
 if MaxDelta < Abs(DeltaB) then MaxDelta := Abs(DeltaB);
 if MaxDelta < 1 then MaxDelta := 1;
 case Style of
  gsHorizontal  : if MaxDelta > W then MaxDelta := W;
  gsVertical    : if MaxDelta > H then MaxDelta := H;
  gsSquare,
  gsElliptic    : if MaxDelta > MaxWH div 2 then MaxDelta := MaxWH div 2;
  gsTopLeft,
  gsTopRight,
  gsBottomLeft,
  gsBottomRight : if MaxDelta > MaxWH then MaxDelta := MaxWH;
 end;
 case Style of
  gsHorizontal,
  gsVertical    : Start := Rect(0, 0, W, H);
  gsSquare,
  gsElliptic    : Start := Rect(W div 2, H div 2, W div 2, H div 2);
  gsTopLeft     : Start := Rect(0, 0, 1, 1);
  gsTopRight    : Start := Rect(W, 1, W+1, 0);
  gsBottomLeft  : Start := Rect(1, H, 0, H+1);
  gsBottomRight : Start := Rect(W, H, W+1, H+1);
 end;
 OffsetRect(Start, ARect.Left, ARect.Top);
 with ACanvas do begin
  OldPenMode := Pen.Mode;
  Brush.Style := bsSolid;
  Pen.Style := psClear;
  Pen.Mode := PenMode;
  //dec(MaxDelta);
  for i := 0 to MaxDelta-1 do with Start do begin
   if MaxDelta > 1 then
    Brush.Color :=
      RGB( StartR + i * DeltaR div (MaxDelta-1),
           StartG + i * DeltaG div (MaxDelta-1),
           StartB + i * DeltaB div (MaxDelta-1) )
   else
    Brush.Color := Color;
   case Style of
    gsHorizontal:
      begin
       Rectangle(
         Left + MulDiv(i, W, MaxDelta), Top,
         Left + MulDiv(i+1, W, MaxDelta) +1, Bottom +1 );
      end;
    gsVertical:
      begin
       Rectangle(
         Left, Top + MulDiv(i, H, MaxDelta),
         Right +1, Top + MulDiv(i+1, H, MaxDelta) +1 );
      end;
    gsSquare:
      begin
       x := MulDiv((MaxDelta-i), W, MaxDelta) div 2;
       y := MulDiv((MaxDelta-i), H, MaxDelta) div 2;
       Rectangle(Left - x, Top - y, Left + x +2, Top + y +2);
      end;
    gsElliptic:
      begin
       x := Round(MulDiv((MaxDelta-i), W, MaxDelta) / 1.4);
       y := Round(MulDiv((MaxDelta-i), H, MaxDelta) / 1.4);
       Ellipse(Left - x, Top - y, Left + x, Top + y);
      end;
    gsTopLeft..gsBottomRight:
      begin
       x := MulDiv((MaxDelta-i), W, MaxDelta);
       y := MulDiv((MaxDelta-i), H, MaxDelta);
       case Style of
        gsTopLeft     : Rectangle(Left, Top, Right + x, Bottom + y);
        gsTopRight    : Rectangle(Left - x, Top + y, Right, Bottom);
        gsBottomLeft  : Rectangle(Left + x, Top - y, Right, Bottom);
        gsBottomRight : Rectangle(Left - x, Top - y, Right, Bottom);
       end;
      end;
   end;
  end;
  Pen.Mode := OldPenMode;
 end;
end;

function CreateTransparentClipRgn(DC: HDC; Width, Height: integer;
  var Dest: TRect; TransparentColor: TColor): HRGN;
type
  PRects = ^TRects;
  TRects = array[0..MaxInt div SizeOf(TRect) -1] of TRect;
var VisibleRgn, TempRgn: HRGN;
    TranspColor: TColorRef;
    i, x, y, Start: integer;
    RgnData: PRgnData;
    RgnSize: integer;
    RgnRects: PRects;
    CoeffX, CoeffY: extended;
begin
 Result := 0;
 if (Width = 0) or (Height = 0) then exit;
 // Create visible region
 VisibleRgn := 0;
 TranspColor := ColorToRGB(TransparentColor) and $00FFFFFF;
 for y:=0 to Height-1 do begin
  x := 0;
  repeat
   // Skip transparent pixels
   while (x < Width) and (GetPixel(DC, x, y) = TranspColor) do inc(x);
   if x = Width then break;
   // Start visible pixels
   Start := x;
   // Find next first transparent pixel
   while (x < Width) and (GetPixel(DC, x, y) <> TranspColor) do inc(x);
   // Include visible pixels in VisibleRgn
   TempRgn := CreateRectRgn(Start, y, x, y+1);
   if VisibleRgn = 0 then
    VisibleRgn := TempRgn
   else begin
    CombineRgn(VisibleRgn, VisibleRgn, TempRgn, RGN_OR);
    DeleteObject(TempRgn);
   end;
  until x = Width;
 end;
 if VisibleRgn = 0 then exit;
 if (Dest.Right - Dest.Left <> Width) or
    (Dest.Bottom - Dest.Top <> Height) then begin
  // Calc scale coeffs
  CoeffX := RectWidth(Dest) / Width;
  CoeffY := RectHeight(Dest) / Height;
  // Scale VisibleRgn
  RgnSize := GetRegionData(VisibleRgn, 0, Nil);
  GetMem(RgnData, RgnSize);
  try
   GetRegionData(VisibleRgn, RgnSize, RgnData);
   // Get rects pointer
   RgnRects := @RgnData.Buffer;
   // Scale rects
   for i:=0 to RgnData.rdh.nCount-1 do with RgnRects[i] do begin
    Left := Round(Left * CoeffX);
    Top := Round(Top * CoeffY);
    Right := Round((Right{-1}) * CoeffX) {+1};
    Bottom := Round((Bottom{-1}) * CoeffY) {+1};
   end;
   // Create new scaled region
   DeleteObject(VisibleRgn);
   VisibleRgn := ExtCreateRegion(Nil, RgnSize, RgnData^);
  finally
   FreeMem(RgnData);
  end;
 end;
 // Offset region
 OffsetRgn(VisibleRgn, Dest.Left, Dest.Top);
 Result := VisibleRgn;
end;

procedure ExcludeBitmapTransparency(Bmp: TBitmap; var R: TRect;
  var ClipRgn: HRGN);
var NewRgn: HRGN;
begin
 NewRgn := CreateTransparentClipRgn(Bmp.Canvas.Handle, Bmp.Width, Bmp.Height,
   R, Bmp.TransparentColor);
 if NewRgn = 0 then exit;
 CombineRgn(ClipRgn, ClipRgn, NewRgn, RGN_AND);
 DeleteObject(NewRgn);
end;

procedure PaintTailed(ACanvas: TCanvas; const PaintRect, RefreshRect: TRect;
  ABitmap: TBitmap; BitmapCache: PTiledBitmapCache = Nil);
begin
 PaintBitmap(ACanvas, PaintRect, RefreshRect, ABitmap, bdTile, BitmapCache);
end;

procedure PaintBitmap(ACanvas: TCanvas; const PaintRect, RefreshRect: TRect;
  ABitmap: TBitmap; BitmapDisplay: TBitmapDisplay;
  BitmapCache: PTiledBitmapCache = Nil; Scale: integer = 100;
  ClipTransparent: boolean = false);
var W, H: integer;
    Pos, RSize, BSize: TPoint;
    R: TRect;
    MemDC, CanvasDC, BmpDC: HDC;
    MemBmp, MemSavedBmp: HBitmap;
    MaskDC: HDC;
    MaskBmp, MaskSavedBmp: HBitmap;
    IsMasked, BuildBmp: boolean;
    MaskedColor: TColor;
    PrevRgn, ClipRgn: HRGN;
begin
 IntersectRect(R, PaintRect, RefreshRect);
 if not Assigned(ABitmap) or IsRectEmpty(R) or ABitmap.Empty then exit;
 W := ABitmap.Width;
 H := ABitmap.Height;
 if (W = 0) or (H = 0) then exit;
 PrevRgn := 0;
 IsMasked := ABitmap.Transparent;
 MaskedColor := ColorToRGB(ABitmap.TransparentColor);
 case BitmapDisplay of
  bdStretch,
  bdCenter:
    begin
     if BitmapDisplay = bdStretch then
      R := PaintRect
     else begin
      R.Right := MulDiv(W, Scale, 100);
      R.Bottom := MulDiv(H, Scale, 100);
      R.Left := (PaintRect.Left + PaintRect.Right - R.Right) div 2;
      R.Top := (PaintRect.Top + PaintRect.Bottom - R.Bottom) div 2;
      inc(R.Right, R.Left);
      inc(R.Bottom, R.Top);
     end;
     if ClipTransparent and IsMasked then begin
  {   ClipRgn := CreateRectRgnIndirect(R);
      ExcludeBitmapTransparency(ABitmap, R, ClipRgn);
      PrevRgn := IntersectClipRgn(ACanvas, ClipRgn);
      ACanvas.StretchDraw(R, ABitmap);
      SelectClipRgn(ACanvas.Handle, PrevRgn);
      DeleteObject(PrevRgn);
      DeleteObject(ClipRgn); }
      // Create transparent clipping region for bitmap
      CanvasDC := ACanvas.Handle;
      BmpDC := ABitmap.Canvas.Handle;
      ClipRgn := CreateTransparentClipRgn(BmpDC, W, H, R, MaskedColor);
      try
       // Select ClipRgn in CanvasDC
       if ClipRgn <> 0 then begin
        PrevRgn := CreateRectRgn(0, 0, 1, 1);
        if GetClipRgn(CanvasDC, PrevRgn) <> 1 then begin
         DeleteObject(PrevRgn);
         PrevRgn := 0;
         SelectClipRgn(CanvasDC, ClipRgn);
        end else
         ExtSelectClipRgn(CanvasDC, ClipRgn, RGN_AND);
       end else
        PrevRgn := 0;
       // Draw
       StretchBlt(CanvasDC, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
        BmpDC, 0, 0, W, H, SRCCOPY);
      finally
       // Restore clipping region
       if ClipRgn <> 0 then begin
        SelectObject(CanvasDC, PrevRgn);
        DeleteObject(PrevRgn);
        DeleteObject(ClipRgn);
       end;
      end;
     end else
      ACanvas.StretchDraw(R, ABitmap);
    end;
  bdTile:
    begin
     RSize.X := RectWidth(R);
     RSize.Y := RectHeight(R);
     Pos.X := (R.Left - PaintRect.Left) mod W;
     Pos.Y := (R.Top - PaintRect.Top) mod H;
     BSize.X := Pos.X + RSize.X;
     BSize.Y := Pos.Y + RSize.Y;
     CanvasDC := ACanvas.Handle;
     MemDC := 0;
     MemBmp := 0;
     MemSavedBmp := 0;
     try
      {MemDC := CreateCompatibleDC(CanvasDC);
      MemBmp := CreateCompatibleBitmap(CanvasDC, BSize.X, BSize.Y); }
      BmpDC := ABitmap.Canvas.Handle;
      MemDC := CreateCompatibleDC(BmpDC);
      if not Assigned(BitmapCache) then begin
       MemBmp := 0;
       BuildBmp := true;
      end else begin
       // Try use cached bitmap
       BuildBmp :=
         (BitmapCache.Width < BSize.X) or
         (BitmapCache.Height < BSize.Y);
       if BuildBmp and (BitmapCache.Handle <> 0) then begin
        // Need recreate bitmap
        DeleteObject(BitmapCache.Handle);
        MemBmp := 0;
       end else
        // Use cached bitmap
        MemBmp := BitmapCache.Handle;
      end;
      // Check building tiled bitmap
      if BuildBmp then begin
       // Create bitmap if needed
       if MemBmp = 0 then begin
        MemBmp := CreateCompatibleBitmap(BmpDC, BSize.X, BSize.Y);
        if Assigned(BitmapCache) then with BitmapCache^ do begin
         // Store created bitmap in cache
         Handle := MemBmp;
         Width := BSize.X;
         Height := BSize.Y;
        end;
       end;
       // Select bitmap
       MemSavedBmp := SelectObject(MemDC, MemBmp);
       // Create tiled bitmap
       BitBlt(MemDC, 0, 0, W, H, ABitmap.Canvas.Handle, 0, 0, SRCCOPY);
       while H < BSize.Y do begin
        if 2*H > BSize.Y
         then BitBlt(MemDC, 0, H, W, BSize.Y - H, MemDC, 0, 0, SRCCOPY)
         else BitBlt(MemDC, 0, H, W, H, MemDC, 0, 0, SRCCOPY);
        inc(H, H);
       end;
       while W < BSize.X do begin
        if 2*W > BSize.X
         then BitBlt(MemDC, W, 0, BSize.X - W, BSize.Y, MemDC, 0, 0, SRCCOPY)
         else BitBlt(MemDC, W, 0, W, BSize.Y, MemDC, 0, 0, SRCCOPY);
        inc(W, W);
       end;
      end else
       // Select bitmap
       MemSavedBmp := SelectObject(MemDC, MemBmp);
      // Draw tailed bitmap
      if IsMasked then begin
       if ClipTransparent then begin
        // Draw bitmap with transparent clipping region
        R.Right := R.Left + RSize.X;
        R.Bottom := R.Top + RSize.Y;
        // Create transparent clipping region for bitmap
        ClipRgn := CreateTransparentClipRgn(MemDC, RSize.X, RSize.Y,
          R, MaskedColor);
        try
         // Select ClipRgn in CanvasDC
         if ClipRgn <> 0 then begin
          PrevRgn := CreateRectRgn(0, 0, 1, 1);
          if GetClipRgn(CanvasDC, PrevRgn) <> 1 then begin
           DeleteObject(PrevRgn);
           PrevRgn := 0;
           SelectClipRgn(CanvasDC, ClipRgn);
          end else
           ExtSelectClipRgn(CanvasDC, ClipRgn, RGN_AND);
         end else
          PrevRgn := 0;
         // Draw
         BitBlt(CanvasDC, R.Left, R.Top, RSize.X, RSize.Y,
          MemDC, Pos.X, Pos.Y, SRCCOPY);
        finally
         // Restore clipping region
         if ClipRgn <> 0 then begin
          SelectObject(CanvasDC, PrevRgn);
          DeleteObject(PrevRgn);
          DeleteObject(ClipRgn);
         end;
        end;
       end else begin
        // Draw bitmap with transparency
        MaskDC := 0;
        MaskBmp := 0;
        MaskSavedBmp := 0;
        try
         MaskDC := CreateCompatibleDC(0);
         MaskBmp := CreateBitmap(RSize.X, RSize.Y, 1, 1, Nil);
         MaskSavedBmp := SelectObject(MaskDC, MaskBmp);
         SelectObject(MaskDC, MaskBmp);
         SetTextColor(MemDC, clWhite);
         SetBkColor(MemDC, MaskedColor);
         BitBlt(MaskDC, 0, 0, RSize.X, RSize.Y, MemDC, Pos.X, Pos.Y, SRCCOPY);
         TransparentStretchBlt(CanvasDC, R.Left, R.Top, RSize.X, RSize.Y,
           MemDC, Pos.X, Pos.Y, RSize.X, RSize.Y, MaskDC, 0, 0);
        finally
         SelectObject(MaskDC, MaskSavedBmp);
         DeleteObject(MaskDC);
         { ///// DEBUG /////
         with TBitmap.Create do begin
          Handle := MaskBmp;
          SaveToFile('d:\testmask.bmp');
          free;
         end;
         { ///////////////// }
         DeleteObject(MaskBmp);
        end;
       end;
      end else
       // Draw bitmap as is
       BitBlt(CanvasDC, R.Left, R.Top, RSize.X, RSize.Y,
         MemDC, Pos.X, Pos.Y, SRCCOPY);
      // Reset MemBmp if it use in BitmapCache
      if Assigned(BitmapCache) then MemBmp := 0;
     finally
      if MemSavedBmp <> 0 then SelectObject(MemDC, MemSavedBmp);
      if MemBmp <> 0 then DeleteObject(MemBmp);
      DeleteDC(MemDC);
     end;
    end;
 end;
end;

// Scaling routines ///////////////////////////////////////////////////////////

function ScaleValue(Value, Scale: integer): integer;
begin
 Result := MulDiv(Value, Scale, 100 * PixelScaleFactor);
end;

function UnScaleValue(Value, Scale: integer): integer;
begin
 Result := MulDiv(Value, 100 * PixelScaleFactor, Scale);
end;

function ScalePixels(Value: integer): integer;
begin
 Result := Value * PixelScaleFactor;
end;

function UnScalePixels(Value: integer): integer;
begin
 Result := Value div PixelScaleFactor;
end;

// List scan routines /////////////////////////////////////////////////////////

function ListScan(Value, List: Pointer; Count: integer): integer; assembler;
asm
        PUSH    EDI
        MOV     EDI,EDX
        OR      EDI,EDI
        JE      @@2
        REPNE   SCASD
        JE      @@1
        MOV     EDI,EDX
@@1:    SUB     EDI,EDX
        SHR     EDI,2
@@2:    MOV     EAX,EDI
        DEC     EAX
        POP     EDI
end;

function ListScanEx(Value, List: Pointer; Index, Count: integer): integer;
begin
 if Index >= Count then
  Result := -1
 else begin
  List := PAnsiChar(List) + Index * SizeOf(pointer);
  dec(Count, Index);
  Result := ListScan(Value, List, Count);
  if Result >= 0 then inc(Result, Index);
 end;
end;

function ListScanLess(Value, List: Pointer; Count: integer): integer; assembler;
asm
        PUSH    EDI
        MOV     EDI,EDX
        OR      EDI,EDI
        JE      @@2
        OR      ECX,ECX
        JLE     @@2
@@1:    SCASD
        JLE     @@3
        LOOP    @@1
@@2:    ADD     EDI,4
@@3:    SUB     EDI,EDX
        SHR     EDI,2
        MOV     EAX,EDI
        DEC     EAX
        POP     EDI
end;

function SortedListFind(List: PPointerList; Count: integer; Item: Pointer;
  Compare: TSortedListCompare; Exact: boolean): integer;
var L, H, I, C: Integer;
    Found: boolean;
begin
 Found := False;
 L := 0;
 H := Count - 1;
 while L <= H do begin
  I := (L + H) shr 1;
  C := Compare(List^[I], Item);
  if C > 0 then
   L := I + 1
  else begin
   H := I - 1;
   if C = 0 then begin
    Found := True;
    // L := I;
   end;
  end;
 end;
 if Found or not Exact
  then Result := L
  else Result := -1;
end;

function ListSortItem(List: PPointerList; Count: integer; ItemIndex: integer;
  Compare: TSortedListCompare): integer;
var Item: pointer;
    L, H, I, II, C: Integer;
begin
 Result := -1;
 if (ItemIndex < 0) or (ItemIndex >= Count) then exit;
 Item := List^[ItemIndex];
 L := 0;
 H := Count - 2;
 while L <= H do begin
  I := (L + H) shr 1;
  II := I;
  if I >= ItemIndex then inc(II);
  C := Compare(List^[II], Item);
  if C > 0 then
   L := I + 1
  else
   H := I - 1;
 end;
 Result := L;
end;

procedure ListQuickSort(List: PPointerList; L, R: Integer;
  Compare: TSortedListCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
 repeat
  I := L;
  J := R;
  P := List^[(L + R) shr 1];
  repeat
   while Compare(List^[I], P) < 0 do Inc(I);
   while Compare(List^[J], P) > 0 do Dec(J);
   if I <= J then begin
    T := List^[I];
    List^[I] := List^[J];
    List^[J] := T;
    Inc(I);
    Dec(J);
   end;
  until I > J;
  if L < J then ListQuickSort(List, L, J, Compare);
  L := I;
 until I >= R;
end;

function IsClassParent(AClass, AParentClass: TClass): boolean;
begin
 Result := false;
 while Assigned(AClass) do
  if AClass = AParentClass then begin
   Result := true;
   break;
  end else
   AClass := AClass.ClassParent;
end;

function DotFloatToStr(const Value: extended): string;
var i: integer;
begin
 Result := FloatToStr(Value);
 if FormatSettings.DecimalSeparator <> '.' then
  for i:=1 to Length(Result) do
   if Result[i] = FormatSettings.DecimalSeparator then begin
    Result[i] := '.';
    break;
   end;
end;

function DotStrToFloat(Value: string): extended;
var i: integer;
begin
 if FormatSettings.DecimalSeparator <> '.' then
  for i:=1 to Length(Value) do
   if Value[i] = '.' then begin
    Value[i] := FormatSettings.DecimalSeparator;
    break;
   end;
 Result := StrToFloat(Value);
end;

{$IFNDEF FG_D5} ///////////////////////////////////////////////////////////////

procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;

type
  { Set access to an integer }
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;

  EPropertyError = class(Exception);
  EPropertyConvertError = class(Exception);

function GetSetProp(Instance: TObject; PropInfo: PPropInfo;
  Brackets: Boolean): string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Integer(S) := GetOrdProp(Instance, PropInfo);
  TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

procedure SetSetProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
var
  Left, EnumName: string;
  Data, EnumValue: Longint;
  EnumInfo: PTypeInfo;

  // grab the next enum name
  function NextWord: string;
  begin
    Result := '';

    // while we are still dealing with non-whitespace
    while not (Left[1] in [',', ' ']) do
    begin
      Result := Result + Left[1];
      Delete(Left, 1, 1);
      if Left = '' then
        Exit;
    end;

    // skip any whitespace
    while Left[1] in [',', ' '] do
      Delete(Left, 1, 1);
  end;
begin
  // bracket reduction
  Left := Value;
  if Left[1] = '[' then
    Delete(Left, 1, 1);
  if Left[Length(Left)] = ']' then
    Delete(Left, Length(Left), 1);

  // loop it dude!
  EnumInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  Data := 0;
  while Left <> '' do
  begin
    EnumName := NextWord;
    if EnumName = '' then
      Break;
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
      raise EPropertyConvertError.CreateFmt('Invalid property element: %s', [EnumName]);
    Include(TIntegerSet(Data), EnumValue);
  end;
  SetOrdProp(Instance, PropInfo, Data);
end;


procedure SetEnumProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: string);
var
  Data: Longint;
begin
  Data := GetEnumValue(PropInfo^.PropType^, Value);
  if Data < 0 then
    raise EPropertyConvertError.CreateFmt('Invalid property element: %s', [Value]);
  SetOrdProp(Instance, PropInfo, Data);
end;

function GetPropValue(Instance: TObject; const PropName: string;
  PreferStrings: Boolean): Variant;
var
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  // assume failure
  Result := Null;

  // get the prop info
  PropInfo := GetPropInfo(PTypeInfo(Instance.ClassType.ClassInfo), PropName);
  if PropInfo <> nil then
  begin
    TypeData := GetTypeData(PropInfo^.PropType^);

    // return the right type
    case PropInfo^.PropType^^.Kind of
      tkInteger, tkChar, tkWChar, tkClass:
        Result := GetOrdProp(Instance, PropInfo);
      tkEnumeration:
        if PreferStrings then
          Result := GetEnumName(PropInfo^.PropType^, GetOrdProp(Instance, PropInfo))
        else if TypeData^.BaseType^ = TypeInfo(Boolean) then
          Result := Boolean(GetOrdProp(Instance, PropInfo))
        else
          Result := GetOrdProp(Instance, PropInfo);
      tkSet:
        if PreferStrings then
          Result := GetSetProp(Instance, PropInfo, False)
        else
          Result := GetOrdProp(Instance, PropInfo);
      tkFloat:
        {begin}
          Result := GetFloatProp(Instance, PropInfo);
          {if not SimpleConvert and
             (TypeData^.BaseType^ = TypeInfo(TDateTime)) then
            Result := VarAsType(Result, varDate);
        end;}
      tkMethod:
        Result := PropInfo^.PropType^.Name;
      tkString, tkLString, tkWString:
        Result := GetStrProp(Instance, PropInfo);
      tkVariant:
        Result := GetVariantProp(Instance, PropInfo);
      tkInt64:
        Result := GetInt64Prop(Instance, PropInfo) + 0.0;
    else
      raise EPropertyConvertError.CreateFmt('Invalid property type: %s',
                                            [PropInfo.PropType^^.Name]);
    end;
  end;
end;

procedure SetPropValue(Instance: TObject; const PropName: string;
  const Value: Variant);

 function RangedValue(const AMin, AMax: Int64): Int64;
 begin
   Result := Trunc(Value);
   if Result < AMin then
     Result := AMin;
   if Result > AMax then
     Result := AMax;
 end;

var
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  // get the prop info
  PropInfo := GetPropInfo(PTypeInfo(Instance.ClassType.ClassInfo), PropName);
  if PropInfo <> nil then
  begin
    TypeData := GetTypeData(PropInfo^.PropType^);

    // set the right type
    case PropInfo.PropType^^.Kind of
      tkInteger, tkChar, tkWChar:
        SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue,
                                                   TypeData^.MaxValue));
      tkEnumeration:
        if VarType(Value) = varString then
          SetEnumProp(Instance, PropInfo, VarToStr(Value))
        else
          SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue,
                                                     TypeData^.MaxValue));
      tkSet:
        if VarType(Value) = varInteger then
          SetOrdProp(Instance, PropInfo, Value)
        else
          SetSetProp(Instance, PropInfo, VarToStr(Value));
      tkFloat:
        SetFloatProp(Instance, PropInfo, Value);
      tkString, tkLString, tkWString:
        SetStrProp(Instance, PropInfo, VarToStr(Value));
      tkVariant:
        SetVariantProp(Instance, PropInfo, Value);
      tkInt64:
        SetInt64Prop(Instance, PropInfo, RangedValue(TypeData^.MinInt64Value,
                                                     TypeData^.MaxInt64Value));
    else
      raise EPropertyConvertError.CreateFmt('Invalid property type: %s',
                                            [PropInfo.PropType^^.Name]);
    end;
  end;
end;

function PropType(AClass: TClass; const PropName: string): TTypeKind;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(AClass.ClassInfo, PropName);
  if PropInfo = nil then
    raise EPropertyError.Create('Property does not exist');
  Result := PropInfo^.PropType^^.Kind;
end;

{$ENDIF} //////////////////////////////////////////////////////////////////////

{$IFNDEF FG_D6} ///////////////////////////////////////////////////////////////
{ Copied from Delphi7 System.pas unit }

// UnicodeToUTF8(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.

function UnicodeToUtf8(Dest: PChar; Source: PWideChar; MaxBytes: Integer): Integer;
var
  len: Cardinal;
begin
  len := 0;
  if Source <> nil then
    while Source[len] <> #0 do
      Inc(len);
  Result := UnicodeToUtf8(Dest, MaxBytes, Source, len);
end;

// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count+1] := Char($80 or ((c shr 6) and $3F));
        Dest[count+2] := Char($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count+1] := Char($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; Source: PChar; MaxChars: Integer): Integer;
var
  len: Cardinal;
begin
  len := 0;
  if Source <> nil then
    while Source[len] <> #0 do
      Inc(len);
  Result := Utf8ToUnicode(Dest, MaxChars, Source, len);
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

function Utf8Encode(const WS: WideString): UTF8String;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  SetLength(Temp, Length(WS) * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PChar(Temp), Length(Temp)+1, PWideChar(WS), Length(WS));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function Utf8Decode(const S: UTF8String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function AnsiToUtf8(const S: string): UTF8String;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: UTF8String): string;
begin
  Result := Utf8Decode(S);
end;

{$ENDIF} //////////////////////////////////////////////////////////////////////

{$IFNDEF FG_D7} ///////////////////////////////////////////////////////////////
{ Copied from Delphi7 Classes.pas unit }

{$IFNDEF FG_D6}
function SameText(const S1, S2: string): Boolean; assembler;
asm
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
        JZ      @3
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JNE     @3
        CALL    CompareText
        TEST    EAX,EAX
        JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
end;
{$ENDIF}

function AncestorIsValid(Ancestor: TPersistent;
  Root, RootAncestor: TComponent): Boolean;
begin
  Result := (Ancestor <> nil) and (RootAncestor <> nil) and
            Root.InheritsFrom(RootAncestor.ClassType);
end;

function IsDefaultPropertyValue(Instance: TObject; PropInfo: PPropInfo;
  OnGetLookupInfo: TGetLookupInfoEvent): Boolean;
var
  PropType: PTypeInfo;
  Ancestor: TPersistent;
  LookupRoot: TComponent;
  RootAncestor: TComponent;
  Root: TComponent;
  AncestorValid: Boolean;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Longint;
    Default: LongInt;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetOrdProp(Ancestor, PropInfo)
    else
    begin
      Default := PPropInfo(PropInfo)^.Default;
      Result :=  (Default <> LongInt($80000000)) and (Value = Default);
    end;
  end;
  
  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetFloatProp(Ancestor, PropInfo)
    else
      Result := Value = 0;;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetInt64Prop(Ancestor, PropInfo)
      else
    Result := Value = 0;
  end;

  function IsDefaultStrProp: Boolean;
  var
    {$IFNDEF FG_D6}
    Value: string;
    {$ELSE}
    Value: WideString;
    {$ENDIF}
  begin
    {$IFNDEF FG_D6}
    Value := GetStrProp(Instance, PropInfo);
    if AncestorValid
     then Result := Value = GetStrProp(Ancestor, PropInfo)
     else Result := Value = '';
    {$ELSE}
    Value := GetWideStrProp(Instance, PropInfo);
    if AncestorValid then
      Result := Value = GetWideStrProp(Ancestor, PropInfo)
    else
      Result := Value = '';
    {$ENDIF}
  end;

  function ObjectAncestorMatch(AncestorValue, Value: TComponent): Boolean;
  begin
    Result := (AncestorValue <> nil) and (AncestorValue.Owner = RootAncestor) and
      (Value <> nil) and (Value.Owner = Root) and
      SameText(AncestorValue.Name, Value.Name);
  end;

  function IsDefaultObjectProp: Boolean;
  var
    Value: TObject;

    function IsDefault: Boolean;
    var
      AncestorValue: TObject;
    begin
      AncestorValue := nil;
      if AncestorValid then
      begin
        AncestorValue := TObject(GetOrdProp(Ancestor, PropInfo));
        if ObjectAncestorMatch(TComponent(AncestorValue), TComponent(Value)) then
          AncestorValue := Value;
      end;
      Result := Value = AncestorValue;
    end;

  begin
    Result := True;
    Value := TObject(GetOrdProp(Instance, PropInfo));
    if (Value = nil) and not IsDefault then
    begin
      Result := False; // nil wasn't the "default" value
    end
    else if Value is TPersistent then
    begin
      if (Value is TComponent)
        {$IFDEF FG_D6}
        and not (csSubComponent in TComponent(Value).ComponentStyle)
        {$ENDIF}
      then
      begin
        if not IsDefault then
        begin
          // A non sub-component TComponent is only non-default if
          // it actually has a name (that way, it can be streamed out -
          // it can't be streamed without a name).
          if TComponent(Value).Name <> '' then
            Result := False;
        end
      end else
      begin
        Result := False; // The TPersistent should be checked for default's by the caller
      end;
    end;
  end;

  {$IFDEF FG_D6}
  function IsDefaultInterfaceProp: Boolean;
  var
    Intf: IInterface;
    Value: TComponent;

    function IsDefaultValue: Boolean;
    var
      AncestorIntf: IInterface;
      ASR: IInterfaceComponentReference;
    begin
      Result := Intf = nil;
      if AncestorValid then
      begin
        AncestorIntf := GetInterfaceProp(Ancestor, PropInfo);
        Result := Intf = AncestorIntf;
        if not Result then
        begin
          if Supports(AncestorIntf, IInterfaceComponentReference, ASR) then
            Result := ObjectAncestorMatch(ASR.GetComponent, Value);
        end;
      end;
    end;

  var
    SR: IInterfaceComponentReference;
  begin
    Result := True;
    Intf := GetInterfaceProp(Instance, PropInfo);
    if (Intf = nil) or (not Supports(Intf, IInterfaceComponentReference, SR)) then
    begin
      if AncestorValid and (GetInterfaceProp(Ancestor, PropInfo) <> nil) then
        Result := False;
    end
    else
    begin
      Value := SR.GetComponent;
      if not IsDefaultValue then
      begin
        // We can only stream out components (ie: non-default ones)
        // if they actually have a name
        if Value.Name <> '' then
          Result := False;
      end;
    end;
  end;
  {$ENDIF}

  function IsDefaultMethodProp: Boolean;
  var
    Value: TMethod;
    DefaultCode: Pointer;
  begin
    Value := GetMethodProp(Instance, PropInfo);
    DefaultCode := nil;
    if AncestorValid then
      DefaultCode := GetMethodProp(Ancestor, PropInfo).Code;
    Result := (Value.Code = DefaultCode) or
      ((Value.Code <> nil) and (LookupRoot.MethodName(Value.Code) = ''));
  end;

  function IsDefaultVariantProp: Boolean;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    {$IFNDEF FG_D6}
    if AncestorValid
      then Result := Value = GetVariantProp(Ancestor, PropInfo)
      else Result := VarIsEmpty(Value);
    {$ELSE}
    if AncestorValid then
      Result := VarSameValue(Value, GetVariantProp(Ancestor, PropInfo))
    else
      Result := VarIsClear(Value);
    {$ENDIF}
  end;

begin
  Ancestor := nil;
  Root := nil;
  LookupRoot := nil;
  RootAncestor := nil;

  if Assigned(OnGetLookupInfo) then
    OnGetLookupInfo(Ancestor, Root, LookupRoot, RootAncestor);

  AncestorValid := AncestorIsValid(Ancestor, Root, RootAncestor);

  Result := True;
  if (PropInfo^.GetProc <> nil) and
     ((PropInfo^.SetProc <> nil) or
     ((PropInfo^.PropType^.Kind = tkClass) and
      (TObject(GetOrdProp(Instance, PropInfo)) is TComponent)
      {$IFDEF FG_D6}
      and (csSubComponent in TComponent(GetOrdProp(Instance, PropInfo)).ComponentStyle)
      {$ENDIF}
      ) ) then
  begin
    PropType := PropInfo^.PropType^;
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        Result := IsDefaultOrdProp;
      tkFloat:
        Result := IsDefaultFloatProp;
      tkString, tkLString, tkWString:
        Result := IsDefaultStrProp;
      tkClass:
        Result := IsDefaultObjectProp;
      tkMethod:
        Result := IsDefaultMethodProp;
      tkVariant:
        Result := IsDefaultVariantProp;
      tkInt64:
        Result := IsDefaultInt64Prop;
      {$IFDEF FG_D6}
      tkInterface:
        Result := IsDefaultInterfaceProp;
      {$ENDIF}
    end;
  end;
end;

{$ENDIF} //////////////////////////////////////////////////////////////////////

initialization
  CF_FLEXDOC := RegisterClipboardFormat('FlexGraphics controls');

end.

