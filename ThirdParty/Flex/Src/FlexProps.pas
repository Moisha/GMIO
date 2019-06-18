/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    Properties support                               //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexProps;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes, Graphics, Forms, Controls, SysUtils,
  Dialogs, TypInfo, Jpeg, {$IFDEF FG_D6} Variants, {$ENDIF}
  FlexUtils, FlexAlpha, FlexHistory, Types, UITypes;

const
  DefaultLineCapSize = 8 * PixelScaleFactor;

  // Standard pen cap styles
  psNoCap              = 0;

  psArrow0             = -1;
  psDblArrow0          = -2;
  psBackArrow0         = -3;
  psBackDblArrow0      = -4;
  psArrow1             = -5;
  psDblArrow1          = -6;
  psBackArrow1         = -7;
  psBackDblArrow1      = -8;
  psArrow              = -9;
  psTriangle           = -10;
  psDblTriangle        = -11;
  psBackTriangle       = -12;
  psBackDblTriangle    = -13;
  psSquare             = -14;
  psCircle             = -15;
  psDotCircle          = -16;
  psRhombus            = -17;
  psThinRect           = -18;

  psLastStandard       = -18;

type
  TPropType = ( ptSimple, ptComplex, ptStrList, ptHexData );

  TPropStyleItem = ( psReadOnly, psVisible, psDontStore, psNonVisual,
                     psDisplayEdit, psEditForm, psScalable, psRef );
  TPropStyle = set of TPropStyleItem;

  TPropList = class;
  TPropRefList = class;

{$M+}
  TCustomProp = class;
{$M-}

  TEditFormClass = class of TCustomForm;
  TCustomPropClass = class of TCustomProp;

  TPropChangedEvent = procedure(Sender: TObject; Prop: TCustomProp) of object;
  TPropStoredEvent = procedure(Sender: TObject; Prop: TCustomProp;
    var IsStored: boolean; const PropName: string = '') of object;
  TPropReadOnlyEvent = procedure(Sender: TObject; Prop: TCustomProp;
    var IsReadOnly: boolean) of object;
  TPropHistoryActionEvent = procedure(Sender: TObject; Prop: TCustomProp;
    var ActionClass: THistoryActionClass) of object;

  TPictureLinkResolve = procedure(Sender: TObject; const LinkName: string;
    APicture: TPicture);

  TBitmapLinkResolve = procedure(Sender: TObject; const LinkName: string;
    ABitmap: TBitmap);

  TCustomProp = class(TPersistent)
  private
   FOwner: TPropList;
   FParent: TCustomProp;
   FStyle: TPropStyle;
   FEditFormClass: TEditFormClass;
   function  GetName: string;
   procedure SetStyle(const Value: TPropStyle);
   function  GetNotifyLink: TNotifyLink;
  protected
   FPropType: TPropType;
   FIsEnum: boolean;
   FNotifyLink: TNotifyLink;
   FSkipHistoryAction: boolean;
   function  GetDisplayValue: string; virtual;
   procedure SetDisplayValue(const Value: string); virtual;
   procedure DoBeforeChanged; virtual;
   procedure DoChanged; virtual;
   procedure SetPropValue(const PropName: string; Value: Variant); virtual;
   function  GetPropValue(const PropName: string): Variant; virtual;
   function  GetPropType(const PropName: string): TPropType; virtual;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   function  Assign(Prop: TCustomProp): boolean; reintroduce;
   procedure Setup(Canvas: TCanvas; Scale: integer = 100); virtual;
   function  Edit: boolean; virtual;
   procedure GetEnumList(List: TStrings); virtual;
   function  IsComplexStored(const PropName: string): boolean; virtual;
   procedure InitPublished(AOwner: TPropList; PublishedProp: TCustomProp);
   function  GetPublishedComplexProp(const PropName: string): TCustomProp;
   procedure GetPropNames(StrList: TStrings);
   function  IsParentOf(Prop: TCustomProp): Boolean;
   property  Owner: TPropList read FOwner write FOwner;
   property  Name: string read GetName;
   property  Parent: TCustomProp read FParent;
   property  Style: TPropStyle read FStyle write SetStyle;
   property  PropType: TPropType read FPropType;
   property  DisplayValue: string read GetDisplayValue write SetDisplayValue;
   property  IsEnum: boolean read FIsEnum;
   property  EditFormClass: TEditFormClass read FEditFormClass
     write FEditFormClass;
   property  NotifyLink: TNotifyLink read GetNotifyLink write FNotifyLink;
   property  PropData[const PropName: string]: Variant read GetPropValue
     write SetPropValue;
   property  PublishedPropType[const PropName: string]: TPropType
     read GetPropType;
  end;

  TIdHistoryAction = class(THistoryStreamAction)
  protected
   FSourceId: LongWord;
   FSourcePropName: string;
   procedure DoBeginAction(Source: TObject); override;
  public
   property  SourceId: LongWord read FSourceId write FSourceId;
   property  SourcePropName: string read FSourcePropName write FSourcePropName;
  end;

  TPropHistoryAction = class(TIdHistoryAction)
  protected
   function  ProcessSource(Stream: TStream; Source: TObject;
     DoLoad: boolean): boolean; override;
  end;

  TPropHistoryGroup = class(THistoryGroup)
  public
   constructor Create(AOwner: THistory; AParent: THistoryGroup); override;
  end;

  TEnumProp = class(TCustomProp)
  private
   FEnumIndex: integer;
   FEnumList: TStrings;
   function  GetEnumCount: integer;
  protected
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
   function  GetDisplayValue: string; override;
   procedure SetEnumIndex(Value: integer); virtual;
   property  EnumList: TStrings read FEnumList write FEnumList;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   function  AddItem(const s: string): integer;
   procedure SetItem(EnumIndex: integer; const s: string);
   function  SetEnumType(TypeInfo: PTypeInfo): boolean;
   procedure GetEnumList(List: TStrings); override;
   property  EnumIndex: integer read FEnumIndex write SetEnumIndex;
   property  EnumCount: integer read GetEnumCount;
  end;

  TBoolProp = class(TEnumProp)
  private
   procedure SetValue(Value: boolean);
   function  GetValue: boolean;
  protected
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  Value: boolean read GetValue write SetValue;
  end;

  TGetIntEvent = procedure(Sender: TObject; out Value: integer) of object;

  TIntProp = class(TCustomProp)
  private
   FValue: integer;
   FSource: PInteger;
   FOnGetValue: TGetIntEvent;
   procedure SetValue(Value: integer);
   function  GetValue: integer;
  protected
   FBeforeValue: integer;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
   function  GetDisplayValue: string; override;
   procedure SetDisplayValue(const Value: string); override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  Value: integer read GetValue write SetValue;
   property  BeforeValue: integer read FBeforeValue write FBeforeValue;
   property  SavedValue: integer read FValue;
   property  Source: PInteger read FSource write FSource;
   property  OnGetValue: TGetIntEvent read FOnGetValue write FOnGetValue;
  end;

  TLongWordProp = class(TCustomProp)
  protected
   FValue: LongWord;
   procedure SetValue(Value: LongWord); virtual;
   function  GetDisplayValue: string; override;
   procedure SetDisplayValue(const Value: string); override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  Value: LongWord read FValue write SetValue;
  end;

  TDoubleProp = class(TCustomProp)
  private
   FValue: Double;
   procedure SetValue(Value: Double);
  protected
   function  GetDisplayValue: string; override;
   procedure SetDisplayValue(const Value: string); override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  Value: Double read FValue write SetValue;
  end;

  TDateTimeProp = class(TCustomProp)
  private
   FValue: TDateTime;
   procedure SetValue(Value: TDateTime);
  protected
   function  GetDisplayValue: string; override;
   procedure SetDisplayValue(const Value: string); override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  Value: TDateTime read FValue write SetValue;
  end;

  TGetStringEvent = procedure(Sender: TObject; out s: string) of object;

  TStrProp = class(TCustomProp)
  protected
   FValue: string;
   FOnGetString: TGetStringEvent;
   function  GetDisplayValue: string; override;
   procedure SetValue(const Value: string); virtual;
   function  GetValue: string; virtual;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  Value: string read GetValue write SetValue;
   property  SavedValue: string read FValue write FValue;
   property  OnGetString: TGetStringEvent read FOnGetString write FOnGetString;
  end;

  TStrListProp = class(TCustomProp)
  private
   FStrList: TStringList;
   procedure SetText(const Value: TCaption);
   function  GetLine(Index: integer): string;
   function  GetLinesCount: integer;
   function  GetText: TCaption;
   procedure SetLine(Index: integer; const Value: string);
   function  GetName(Index: integer): string;
   function  GetValue(const Name: string): string;
   function  GetValueByIndex(Index: integer): string;
   procedure SetValue(const Name, Value: string);
   procedure SetValueByIndex(Index: integer; const Value: string);
   procedure SetName(Index: integer; const Value: string);
  protected
   function  GetDisplayValue: string; override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   procedure Assign(Source: TStrings);
   procedure AssignTo(Dest: TStrings); reintroduce;
   procedure Clear;
   function  Add(const s: string): integer;
   function  Insert(Index: Integer; const S: string): boolean;
   procedure Delete(Index: integer);
   procedure RemoveByName(const Name: string);
   function  IndexOf(const s: string): integer;
   function  IndexOfName(const s: string): integer;
   property  LinesCount: integer read GetLinesCount;
   property  Lines[Index: integer]: string read GetLine write SetLine; default;
   property  Names[Index: integer]: string read GetName write SetName;
   property  Values[const Name: string]: string read GetValue write SetValue;
   property  ValuesByIndex[Index: integer]: string read GetValueByIndex
     write SetValueByIndex;
   property  Text: TCaption read GetText write SetText;
   property StrList: TStringList read FStrList;
  end;

  TUserDataProp = class(TStrListProp);
  TScriptProp = class(TStrListProp);

  TPropDataEvent = procedure(Sender: TObject; var Value: Variant) of object;

  TDataProp = class(TCustomProp)
  private
   FOnGetPropData: TPropDataEvent;
   FOnSetPropData: TPropDataEvent;
  protected
   function  GetDisplayValue: string; override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   property  OnGetPropData: TPropDataEvent read FOnGetPropData
     write FOnGetPropData;
   property  OnSetPropData: TPropDataEvent read FOnSetPropData
     write FOnSetPropData;
  end;

  TChannelProp = class(TStrListProp)
  end;

  TControlObjectProp = class(TIntProp)
  protected
    function  GetDisplayValue: string; override;
  end;

  TBrushMethod = ( bmHatch, bmGradient, bmBitmap );

  TBrushProp = class(TCustomProp)
  private
   FBrush: TBrush;
   FMethod: TBrushMethod;
   FGradStyle: TGradientStyle;
   FGradBeginColor: TColor;
   FGradEndColor: TColor;
   FBitmap: TBitmap;
   FCache: TTiledBitmapCache;
   FBitmapLinkName: string;
   FBitmapMasked: boolean;
   FBitmapMaskColor: TColor;
   FBitmapCache: boolean;
   FBitmapDisplay: TBitmapDisplay;
   function  GetColor: TColor;
   function  GetStyle: TBrushStyle;
   procedure SetColor(const Value: TColor);
   procedure SetStyle(const Value: TBrushStyle);
   procedure SetGradBeginColor(const Value: TColor);
   procedure SetGradEndColor(const Value: TColor);
   procedure SetGradStyle(const Value: TGradientStyle);
   function  GetIsPaintAlternate: boolean;
   procedure SetBitmap(const Value: TBitmap);
   procedure SetBitmapMaskColor(const Value: TColor);
   procedure SetBitmapMasked(const Value: boolean);
   procedure SetBitmapCache(const Value: boolean);
   procedure SetBitmapDisplay(const Value: TBitmapDisplay);
   procedure SetBitmapLinkName(const Value: string);
   procedure SetMethod(const Value: TBrushMethod);
   function  GetIsClear: boolean;
   function  StoreBitmap: Boolean;
   function  StoreBitmapLinkName: Boolean;
  protected
   function  GetPropType(const PropName: string): TPropType; override;
   function  GetPropValue(const PropName: string): Variant; override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetDisplayValue: string; override;
   procedure BitmapChange(Sender: TObject); virtual;
   procedure ResetCache;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   procedure InitBitmap(Width: integer = 0; Height: integer = 0);
   procedure FreeBitmap;
   function  UpdateBitmapLink: boolean;
   procedure Setup(Canvas: TCanvas; Scale: integer = 100); override;
   procedure Translate(const TranslateInfo: TTranslateInfo); virtual;
   procedure PaintAlternate(Canvas: TCanvas; var PaintRect: TRect;
     const RefreshRect: TRect; Scale: integer = 100;
     ClipTransparent: boolean = false);
   property  IsPaintAlternate: boolean read GetIsPaintAlternate;
   property  IsClear: boolean read GetIsClear;
  published
   property  Method: TBrushMethod read FMethod write SetMethod
     default bmHatch;
   property  Color: TColor read GetColor write SetColor default clNone;
   property  Style: TBrushStyle read GetStyle write SetStyle default bsSolid;
   property  Bitmap: TBitmap read FBitmap write SetBitmap stored StoreBitmap;
   property  BitmapLinkName: string read FBitmapLinkName
     write SetBitmapLinkName stored StoreBitmapLinkName;
   property  BitmapMasked: boolean read FBitmapMasked write SetBitmapMasked
     default False;
   property  BitmapMaskColor: TColor read FBitmapMaskColor
     write SetBitmapMaskColor default clBlack;
   property  BitmapCache: boolean read FBitmapCache write SetBitmapCache
     default False;
   property  BitmapDisplay: TBitmapDisplay read FBitmapDisplay
     write SetBitmapDisplay default bdTile;
   property  GradStyle: TGradientStyle read FGradStyle write SetGradStyle
     default gsHorizontal;
   property  GradBeginColor: TColor read FGradBeginColor write SetGradBeginColor
     default clBlack;
   property  GradEndColor: TColor read FGradEndColor write SetGradEndColor
     default clBlack;
  end;

  TLineCapProp = class(TCustomProp)
  private
   FCapStyle: integer;
   FFixedSize: boolean;
   FSize: integer;
   FFixedOutlineColor: boolean;
   FOutlineColor: TColor;
   FFixedFillColor: boolean;
   FFillColor: TColor;
   procedure SetCapStyle(const Value: integer);
   procedure SetFillColor(const Value: TColor);
   procedure SetFixedFillColor(const Value: boolean);
   procedure SetFixedOutlineColor(const Value: boolean);
   procedure SetFixedSize(const Value: boolean);
   procedure SetOutlineColor(const Value: TColor);
   procedure SetSize(const Value: integer);
  protected
   function  GetDisplayValue: string; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   function   GetActiveSize(PenWidth: integer;
     PixelSize: integer = PixelScaleFactor): integer;
  published
   property  CapStyle: integer read FCapStyle write SetCapStyle
     default psNoCap;
   property  FixedSize: boolean read FFixedSize write SetFixedSize
     default False;
   property  Size: integer read FSize write SetSize default DefaultLineCapSize;
   property  FixedOutlineColor: boolean read FFixedOutlineColor
     write SetFixedOutlineColor default False;
   property  OutlineColor: TColor read FOutlineColor write SetOutlineColor
     default clBlack;
   property  FixedFillColor: boolean read FFixedFillColor
     write SetFixedFillColor default False;
   property  FillColor: TColor read FFillColor write SetFillColor
     default clWhite;
  end;

  TPenEndCap = (
   pecFlat,
   pecSquare,
   pecRound
  );

  TPenJoin = (
   pjBevel,
   pjMiter,
   pjRound
  );

  TPenProp = class(TCustomProp)
  private
   function  GetActiveWidth: integer;
   procedure SetColor(const Value: TColor);
   procedure SetStyle(const Value: TPenStyle);
   procedure SetWidth(const Value: integer);
   procedure SetMode(const Value: TPenMode);
   procedure SetEndCap(const Value: TPenEndCap);
   procedure SetJoin(const Value: TPenJoin);
   procedure SetSolidAsInside(const Value: boolean);
  protected
   FStyle: TPenStyle;
   FColor: TColor;
   FMode: TPenMode;
   FWidth: integer;
   FEndCap: TPenEndCap;
   FJoin: TPenJoin;
   FSolidAsInside: boolean;
   function  GetDisplayValue: string; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   procedure Setup(Canvas: TCanvas; Scale: integer = 100); override;
   procedure GetPaintData(var AWidth: integer; var AStyle: TPenStyle;
     var IsGeometricPen: boolean; Scale: integer = 100);
   property  ActiveWidth: integer read GetActiveWidth;
  published
   property  Color: TColor read FColor write SetColor default clBlack;
   property  Style: TPenStyle read FStyle write SetStyle default psSolid;
   property  Mode: TPenMode read FMode write SetMode default pmCopy;
   property  Width: integer read FWidth write SetWidth
     default PixelScaleFactor;
   property  EndCap: TPenEndCap read FEndCap write SetEndCap default pecRound;
   property  Join: TPenJoin read FJoin write SetJoin default pjRound;
   property  SolidAsInside: boolean read FSolidAsInside write SetSolidAsInside
     default True;
  end;

  TFontProp = class(TCustomProp)
  private
   FFont: TFont;
   FFontHeight: integer;
   function  GetCharset: TFontCharset;
   function  GetHandle: HFont;
   function  GetSize: Integer;
   function  GetName: TFontName;
   function  GetPitch: TFontPitch;
   function  GetStyle: TFontStyles;
   function  GetColor: TColor;
   function  GetPixelsPerInch: Integer;
   procedure SetCharset(const Value: TFontCharset);
   procedure SetColor(const Value: TColor);
   procedure SetHandle(const Value: HFont);
   procedure SetHeight(const Value: Integer);
   procedure SetName(const Value: TFontName);
   procedure SetPitch(const Value: TFontPitch);
   procedure SetSize(const Value: Integer);
   procedure SetStyle(const Value: TFontStyles);
   procedure SetPixelsPerInch(const Value: Integer);
  protected
   function  GetDisplayValue: string; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   procedure Setup(Canvas: TCanvas; Scale: integer = 100); override;
   function  Edit: boolean; override;
   procedure GetFont(var AFont: TFont);
   procedure SetFont(const AFont: TFont);
   property  Handle: HFont read GetHandle write SetHandle;
  published
   property PixelsPerInch: Integer read GetPixelsPerInch write SetPixelsPerInch
     stored False;
   property Charset: TFontCharset read GetCharset write SetCharset
     default DEFAULT_CHARSET;
   property Color: TColor read GetColor write SetColor
     default clBlack;
   property Height: Integer read FFontHeight write SetHeight stored False;
   property Name: TFontName read GetName write SetName;
   property Pitch: TFontPitch read GetPitch write SetPitch
     default fpDefault;
   property Size: Integer read GetSize write SetSize;
   property Style: TFontStyles read GetStyle write SetStyle default [];
  end;

  TPicturePaintBufferEvent = procedure(Sender: TObject; Buffer: TAlphaBuffer;
    Canvas: TCanvas; Graphic: TGraphic; var DrawDefault: boolean) of object;

  TPictureProp = class(TCustomProp)
  private
   FPicture: TPicture;
   FGraphicClass: TGraphicClass;
   FEmpty: boolean;
   FWidth: integer;
   FHeight: integer;
   FColumns: integer;
   FRows: integer;
   FMasked: boolean;
   FMaskColor: TColor;
   FBuffered: boolean;
   FPictureBuffer: TMemoryStream;
   FLinkName: string;
   FUsePaintBuffer: boolean;
   FDrawing: boolean;
   FOnPaintFastBuffer: TPicturePaintBufferEvent;
   function  GetIsLoaded: boolean;
   function  GetImgRect(Index: integer): TRect;
   function  GetCellSizeRect: TRect;
   function  GetGraphic: TGraphic;
   procedure PictureChange(Sender: TObject);
   procedure SetGraphic(const Value: TGraphic);
   procedure SetColumns(const Value: integer);
   procedure SetRows(const Value: integer);
   procedure SetMaskColor(const Value: TColor);
   procedure SetMasked(const Value: boolean);
   procedure SetBuffered(const Value: boolean);
   procedure SetFastBuffer(const Value: boolean);
   procedure SetLinkName(const Value: string);
   function  StoreGraphic: Boolean;
   function  StoreLinkName: Boolean;
  protected
   FPaintBuffer: TAlphaBuffer;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
   function  GetPropType(const PropName: string): TPropType; override;
   function  GetDisplayValue: string; override;
   function  GetIsRaster: boolean; virtual;
   function  GetFrameBitmap(Graphic: TGraphic; FrameIndex: integer): TBitmap;
     virtual;
   procedure RefreshPaintBuffer;
   procedure PaintBufferStore(Sender: TObject; Canvas: TCanvas;
     Graphic: TGraphic); virtual;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   procedure Draw(Canvas: TCanvas; var R: TRect; FrameIndex: integer;
     ClipTransparent: boolean = false; DestBuffer: TAlphaBuffer = Nil);
   procedure Clear(RemoveLink: boolean = false);
   function  UpdateImageLink: boolean;
   function  CreatePictureFromBuffer: TPicture;
   procedure LoadFromStream(Stream: TStream);
   procedure SaveToStream(Stream: TStream);
   procedure LoadFromFile(const FileName: string);
   procedure SaveToFile(const FileName: string);
   property  Buffered: boolean read FBuffered write SetBuffered;
   property  GraphicClass: TGraphicClass read FGraphicClass;
   property  Width: integer read FWidth;
   property  Height: integer read FHeight;
   property  IsLoaded: boolean read GetIsLoaded;
   property  IsRaster: boolean read GetIsRaster;
   property  ImgRect[Index: integer]: TRect read GetImgRect;
   property  CellSizeRect: TRect read GetCellSizeRect;
   property  OnPaintFastBuffer: TPicturePaintBufferEvent read FOnPaintFastBuffer
     write FOnPaintFastBuffer;
  published
   property  Graphic: TGraphic read GetGraphic write SetGraphic
     stored StoreGraphic;
   property  Columns: integer read FColumns write SetColumns default 1;
   property  Rows: integer read FRows write SetRows default 1;
   property  Masked: boolean read FMasked write SetMasked default False;
   property  MaskColor: TColor read FMaskColor write SetMaskColor
     default clBlack;
   property  LinkName: string read FLinkName write SetLinkName
     stored StoreLinkName;
   property  FastBuffer: boolean read FUsePaintBuffer write SetFastBuffer
     default False;
  end;

  TBackgroundPictureOption = (
    bpOffsetLeft,
    bpOffsetTop,
    bpNewWidth,
    bpNewHeight,
    bpStretchHoriz,
    bpStretchVert,
    bpScaledSize,
    bpCenterHoriz,
    bpCenterVert,
    bpAlignRight,
    bpAlignBottom
  );
  TBackgroundPictureOptions = set of TBackgroundPictureOption;

  TBackgroundOptionsProp = class(TCustomProp)
  private
   FBrushEnabled: boolean;
   FPictureEnabled: boolean;
   FPictureOptions: TBackgroundPictureOptions;
   FTop: Integer;
   FLeft: Integer;
   FWidth: Integer;
   FHeight: Integer;
   function  GetCenter: Boolean;
   function  GetStretch: Boolean;
   procedure SetBrushEnabled(const Value: boolean);
   procedure SetPictureEnabled(const Value: boolean);
   procedure SetPictureOptions(const Value: TBackgroundPictureOptions);
   procedure SetTop(Value: Integer);
   procedure SetLeft(Value: Integer);
   procedure SetWidth(Value: Integer);
   procedure SetHeight(Value: Integer);
   procedure SetStretch(Value: Boolean);
   procedure SetCenter(Value: Boolean);
  protected
   function  GetDisplayValue: string; override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   function  GetPicturePaintRect(Picture: TPictureProp;
     const BackgroundRect: TRect; Scale: integer): TRect;
   procedure Draw(Canvas: TCanvas; var R: TRect; Picture: TPictureProp;
     Brush: TBrushProp; Scale: integer; const PanelRefreshRect: TRect; 
     ClipTransparent: boolean = false);
   property  Stretch: Boolean read GetStretch write SetStretch default True;
   property  Center: Boolean read GetCenter write SetCenter default False;
  published
   property  PictureEnabled: boolean read FPictureEnabled
     write SetPictureEnabled default True;
   property  BrushEnabled: boolean read FBrushEnabled write SetBrushEnabled
     default True;
   property  PictureOptions: TBackgroundPictureOptions read FPictureOptions
     write SetPictureOptions default [bpStretchHoriz,bpStretchVert,bpScaledSize];
   property  Top: Integer read FTop write SetTop default 0;
   property  Left: Integer read FLeft write SetLeft default 0;
   property  Width: Integer read FWidth write SetWidth default 0;
   property  Height: Integer read FHeight write SetHeight default 0;
  end;

  PPropRefItem = ^TPropRefItem;
  TPropRefItem = record
   Prop: TCustomProp;
   PropName: ShortString;
   Data: PVariant;
  end;

  TGetIDAlias = function(OldID: integer): integer of object;

  TPropRefList = class
  private
   FList: TList;
   FIdExist: TIdPool;
   FIdRecode: TList;
   function  GetCount: integer;
   function  GetPropRefItem(Index: integer): PPropRefItem;
   function  IndexOfRecode(OldID: integer): integer;
  public
   constructor Create;
   destructor Destroy; override;
   function  AddRef(AProp: TCustomProp; const APropName: ShortString;
     AData: PVariant): integer;
   function  AddIDAlias(OldID, NewID: integer): boolean;
   function  GetIDAlias(OldID: integer): integer;
   procedure DeleteRef(Index: integer);
   procedure ResolveRef(Index: integer);
   procedure ResolveAllRefs;
   procedure Clear;
   property  Count: integer read GetCount;
   property  Refs[Index: integer]: PPropRefItem read GetPropRefItem;
  end;

  TPropListDataMode = ( plmNone, plmLoading, plmSaving );

  TFlexStringListSortCompare = function(List: TStringList; Index1, Index2: Integer): Integer of object;
  TPropNameList = class(TStringList)
    procedure FlexQuickSort(L, R: Integer; SCompare: TFlexStringListSortCompare);
  public
    procedure FlexSort(compare: TFlexStringListSortCompare);
  end;

  TPropList = class
  private
   FOwner: TObject;
   FRefList: TPropRefList;
   FPropList: TPropNameList;
   FPropNameTransation: TStringList;
   FNotifyLink: TNotifyLink;
   FHistory: THistory;
   FHistoryIgnoreReadOnly: boolean;
   FHistoryStateForList: THistoryState;
   FPropValueLoading: boolean;
   FOnPropChanged: TPropChangedEvent;
   FOnPropBeforeChanged: TPropChangedEvent;
   FOnPropStored: TPropStoredEvent;
   FOnPropReadOnly: TPropReadOnlyEvent;
   FOnPropHistoryAction: TPropHistoryActionEvent;
   function  GetPropByIndex(Index: integer): TCustomProp;
   function  GetPropsCount: integer;
   function  GetPropByName(const Name: string): TCustomProp;
   function  GetPropName(Index: integer): string;
   function  GetVisibleCount: integer;
   function  GetVisibleProp(Index: integer): TCustomProp;
   function  GetVisiblePropName(Index: integer): string;
   function  GetResolving: boolean;
   function  GetNotifyLink: TNotifyLink;
   function GetVisiblePropTranslatedName(Index: integer): string;
   function CompareTranslatedStrings(List: TStringList; Index1, Index2: Integer): Integer;
  protected
   FResolveCount: integer;
   FDataMode: TPropListDataMode;
   FDataModeCount: integer;
   procedure DoBeforeChanged(Prop: TCustomProp);
   procedure DoChanged(Prop: TCustomProp);
   function  DoPropIsStored(Prop: TCustomProp;
     const PropName: string = ''): boolean;
   function  IsNameValid(const AName: string): boolean;
   function  GetComplexPropInstance(Prop: TCustomProp;
     const PropName: string): TCustomProp;
   procedure SavePropValue(Filer: TFlexFiler; const Indent: string;
     Prop: TCustomProp; const PropName: string; IsComplex: boolean);
   procedure SaveComplexProp(Filer: TFlexFiler; Prop: TCustomProp;
     const PropName, Indent: string; ForceSave: boolean = false);
   function  LoadPropValue(Filer: TFlexFiler; ComplexProp: TCustomProp;
     const First: string): TCustomProp;
   function  LoadComplexProp(Filer: TFlexFiler; const First: string;
      Prop: TCustomProp): boolean;
   property  PropValueLoading: boolean read FPropValueLoading;
  public
   constructor Create(AOwner: TObject{TFlexControl});
   destructor Destroy; override;
   function  Assign(PropList: TPropList): Boolean;
   procedure BeginResolve(ARefList: TPropRefList);
   procedure EndResolve;
   procedure Clear;
   procedure Sort;
   procedure BeginLoading;
   procedure BeginSaving;
   procedure EndLoadSave;
   function  Add(Prop: TCustomProp; const AName: string; const ATranslatedName: string = ''): integer;
   function  AddLoadedIDAlias(LoadedID, NewID: integer): boolean;
   function  IndexOf(Prop: TCustomProp): integer;
   function  IsReadOnly(Prop: TCustomProp): boolean;
   function  VisibleIndexOf(Prop: TCustomProp): integer;
   function  GetRealIndex(VisibleIndex: integer): integer;
   function  GetVisibleIndex(RealIndex: integer): integer;
   procedure Delete(Index: integer);
   function  SaveProperty(Filer: TFlexFiler; Prop: TCustomProp;
     const Indent: string; PropName: string = '';
     ForceSave: boolean = false): boolean;
   function  LoadProperty(Filer: TFlexFiler; const First: string;
     WithoutBeginLoading: boolean = false): TCustomProp;
   function  SaveToFiler(Filer: TFlexFiler; const Indent: string;
     ForceSave: boolean = false): boolean;
   procedure LoadFromFiler(Filer: TFlexFiler; const First: string;
     RefList: TPropRefList);
   property  Owner: TObject read FOwner;
   property  DataMode: TPropListDataMode read FDataMode;
   property  Count: integer read GetPropsCount;
   property  Props[const Name: string]: TCustomProp read GetPropByName; default;
   property  PropNames[Index: integer]: string read GetPropName;
   property  ByIndex[Index: integer]: TCustomProp read GetPropByIndex;
   property  VisibleCount: integer read GetVisibleCount;
   property  VisibleProps[Index: integer]: TCustomProp read GetVisibleProp;
   property  VisiblePropNames[Index: integer]: string read GetVisiblePropName;
   property  VisiblePropTranslatedNames[Index: integer]: string read GetVisiblePropTranslatedName;
   property  Resolving: boolean read GetResolving;
   property  RefList: TPropRefList read FRefList write FRefList;
   property  NotifyLink: TNotifyLink read GetNotifyLink write FNotifyLink;
   property  History: THistory read FHistory write FHistory;
   property  HistoryIgnoreReadOnly: boolean read FHistoryIgnoreReadOnly
     write FHistoryIgnoreReadOnly;
   property  HistoryStateForList: THistoryState read FHistoryStateForList
     write FHistoryStateForList;
   property  OnPropChanged: TPropChangedEvent read FOnPropChanged
     write FOnPropChanged;
   property  OnPropBeforeChanged: TPropChangedEvent read FOnPropBeforeChanged
     write FOnPropBeforeChanged;
   property  OnPropStored: TPropStoredEvent read FOnPropStored
     write FOnPropStored;
   property  OnPropReadOnly: TPropReadOnlyEvent read FOnPropReadOnly
     write FOnPropReadOnly;
   property  OnPropHistoryAction: TPropHistoryActionEvent
     read FOnPropHistoryAction write FOnPropHistoryAction;
  end;

var
  ResolvePictureLink: TPictureLinkResolve;
  ResolveBitmapLink: TBitmapLinkResolve;

  GeometricPenChecked: boolean;
  GeometricPenEnabled: boolean;

procedure RegisterDefaultPropEditForm(PropClass: TCustomPropClass;
  EditFormClass: TEditFormClass);

implementation

uses GMGlobals, GMDBClasses;

const
  SBufferedPictureChangeError = 'Can''t change buffered picture';

type
  PEditFormReg = ^TEditFormReg;
  TEditFormReg = record
   PropClass: TCustomPropClass;
   EditFormClass: TEditFormClass;
  end;

var
  DefaultPropEditors: TList;

procedure RegisterDefaultPropEditForm(PropClass: TCustomPropClass;
  EditFormClass: TEditFormClass);
var Reg: PEditFormReg;
    i: integer;
begin
 if not Assigned(DefaultPropEditors) then
  DefaultPropEditors := TList.Create
 else
  for i:=0 to DefaultPropEditors.Count-1 do
   if PEditFormReg(DefaultPropEditors[i]).PropClass = PropClass then begin
    PEditFormReg(DefaultPropEditors[i]).EditFormClass := EditFormClass;
    exit;
   end;
 New(Reg);
 try
  Reg.PropClass := PropClass;
  Reg.EditFormClass := EditFormClass;
  DefaultPropEditors.Add(Reg);
 except
  Dispose(Reg);
  raise;
 end;
end;

function FindDefaultPropEditForm(PropClass: TCustomPropClass): TEditFormClass;
var i: integer;
begin
 Result := Nil;
 if Assigned(DefaultPropEditors) then
 for i:=0 to DefaultPropEditors.Count-1 do
  if PEditFormReg(DefaultPropEditors[i]).PropClass = PropClass then begin
   Result := PEditFormReg(DefaultPropEditors[i]).EditFormClass;
   break;
  end;
end;

procedure ClearRegEditForms;
var i: integer;
begin
 if not Assigned(DefaultPropEditors) then exit;
 for i:=0 to DefaultPropEditors.Count-1 do
  Dispose(PEditFormReg(DefaultPropEditors[i]));
 DefaultPropEditors.Free;
 DefaultPropEditors := Nil;
end;

procedure CheckGeometricPen;
var Bmp: TBitmap;
    LogBrush: TLogBrush;
    LastColor: TColor;
    i: integer;
begin
 if GeometricPenChecked then exit;
 Bmp := TBitmap.Create;
 try
  Bmp.Height := 1;
  Bmp.Width := 10;
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := clBlack;
  Bmp.Canvas.Pen.Handle := ExtCreatePen(
    PS_GEOMETRIC or PS_DOT or PS_ENDCAP_SQUARE or PS_JOIN_BEVEL, 3,
    LogBrush, 0, Nil);
  Bmp.Canvas.Polyline([Point(-1, 0), Point(11, 0)]);
  // Check bitmap
  LastColor := 0;
  GeometricPenEnabled := False;
  for i:=0 to Bmp.Width-1 do
    if i = 0 then
      LastColor := Bmp.Canvas.Pixels[i, 0]
    else
    if LastColor <> Bmp.Canvas.Pixels[i, 0] then begin
      GeometricPenEnabled := True;
      break;
    end;
  GeometricPenChecked := True;
 finally
  Bmp.Free;
 end;
end;

// TCustomProp ///////////////////////////////////////////////////////////

constructor TCustomProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited Create;
 if Assigned(AOwner) then AOwner.Add(Self, AName, ATranslatedName);
 FStyle := [ psVisible ];
 FEditFormClass := FindDefaultPropEditForm(TCustomPropClass(ClassType));
 if Assigned(FEditFormClass) then Include(FStyle, psEditForm);
end;

destructor TCustomProp.Destroy;
begin
 FNotifyLink.Free;
 inherited;
end;

procedure TCustomProp.InitPublished(AOwner: TPropList;
  PublishedProp: TCustomProp);
begin
 if not Assigned(PublishedProp) or
   (PublishedProp.PropType <> ptComplex) then exit;
 PublishedProp.FOwner := AOwner;
 PublishedProp.FParent := Self;
end;

function TCustomProp.GetPublishedComplexProp(
  const PropName: string): TCustomProp;
begin
 Result := FOwner.GetComplexPropInstance(Self, PropName);
end;

function TCustomProp.Assign(Prop: TCustomProp): boolean;
var MS: TMemoryStream;
    Filer: TFlexFiler;
begin
 Result := false;
 if not Assigned(Prop) or Owner.IsReadOnly(Prop) then exit;
 MS := Nil;
 Filer := Nil;
 try
  MS := TMemoryStream.Create;
  Filer := TFlexFiler.Create(MS);
  Prop.Owner.SaveProperty(Filer, Prop, '', Name, True);
  // MS.SaveToFile('d:\test.fxp');
  Filer.Rewind;
  // Skip BeginLoading call if the undo/redo history is recordable since we need
  // to save changes in it and that needs subsequent BeginSave call.
  Owner.LoadProperty(Filer, Filer.LoadStr,
    Assigned(Owner.History) and Owner.History.IsRecordable);
  Result := true;
 finally
  Filer.Free;
  MS.Free;
 end;
end;

function TCustomProp.GetNotifyLink: TNotifyLink;
begin
 if not Assigned(FNotifyLink) then FNotifyLink := TNotifyLink.Create(Self);
 Result := FNotifyLink;
end;

function TCustomProp.GetName: string;
var Index: integer;
begin
 Index := FOwner.IndexOf(Self);
 if Index >= 0
  then Result := FOwner.PropNames[Index]
  else Result := '';
end;

procedure TCustomProp.DoBeforeChanged;
begin
 if Assigned(FOwner) then FOwner.DoBeforeChanged(Self);
 if Assigned(FNotifyLink) then FNotifyLink.PropNotify(Self, True);
end;

procedure TCustomProp.DoChanged;
begin
 if Assigned(FOwner) then FOwner.DoChanged(Self);
 if Assigned(FNotifyLink) then FNotifyLink.PropNotify(Self, False);
end;

function TCustomProp.IsComplexStored(const PropName: string): boolean;
var PropInfo: PPropInfo;
begin
 Result := True;
 PropInfo := GetPropInfo(ClassInfo, PropName);
 if not Assigned(PropInfo) then exit;
 // Check default value
 if PropInfo.PropType^.Kind <> tkClass then begin
  Result := not IsDefaultPropertyValue(Self, PropInfo, Nil);
  if not Result then exit;
 end;
 // Check stored value
 {$IFNDEF FG_D5}
 if Assigned(PropInfo) then
  Result := IsStoredProp(Self, PropInfo);
 {$ELSE}
 Result := IsStoredProp(Self, PropName);
 {$ENDIF}
end;

procedure TCustomProp.SetStyle(const Value: TPropStyle);
begin
 FStyle := Value;
end;

procedure TCustomProp.Setup(Canvas: TCanvas; Scale: integer = 100);
begin
 // ABSTRACT //
end;

procedure TCustomProp.GetEnumList(List: TStrings);
begin
 // ABSTRACT //
end;

function TCustomProp.GetDisplayValue: string;
begin
 // GENERIC //
 Result := '(' + ClassName + ')';
end;

procedure TCustomProp.SetDisplayValue(const Value: string);
begin
 // GENERIC //
 SetPropValue('', Value);
end;

function TCustomProp.Edit: boolean;
var EditForm: TCustomForm;
begin
 Result := False;
 if not (psEditForm in FStyle) or not Assigned(FEditFormClass) then exit;
 EditForm := FEditFormClass.Create(Nil);
 try
  EditForm.Tag := integer(Self);
  Result := EditForm.ShowModal = mrOk;
 finally
  EditForm.Free;
 end;
end;

procedure TCustomProp.GetPropNames(StrList: TStrings);
var TypeInfo: PTypeInfo;
    TypeData: PTypeData;
    PropList: PPropList;
    i: integer;
begin
 PropList := Nil;
 StrList.BeginUpdate;
 with StrList do
 try
  TypeInfo := Self.ClassInfo;
  if not Assigned(TypeInfo) then exit;
  TypeData := GetTypeData(TypeInfo);
  if not Assigned(TypeData) then exit;
  GetMem(PropList, TypeData.PropCount * SizeOf(Pointer));
  GetPropInfos(TypeInfo, PropList);
  for i:=0 to TypeData.PropCount-1 do
   StrList.Add(String(PropList[i].Name));
 finally
  EndUpdate;
  if Assigned(PropList) then FreeMem(PropList);
 end;
end;

function TCustomProp.GetPropValue(const PropName: string): variant;
begin
 if PropName <> '' then
  {$IFDEF FG_D5}
  Result := TypInfo.GetPropValue(Self, PropName, true)
  {$ELSE}
  Result := FlexUtils.GetPropValue(Self, PropName, true)
  {$ENDIF}
 else
  VarClear(Result);
end;

procedure TCustomProp.SetPropValue(const PropName: string; Value: variant);
begin
 if PropName <> '' then
  {$IFDEF FG_D5}
  TypInfo.SetPropValue(Self, PropName, Value);
  {$ELSE}
  FlexUtils.SetPropValue(Self, PropName, Value);
  {$ENDIF}
end;

function TCustomProp.GetPropType(const PropName: string): TPropType;
begin
 if PropName = ''
  then Result := FPropType
  else Result := ptSimple; // generic
end;

function TCustomProp.IsParentOf(Prop: TCustomProp): Boolean;
begin
 Result := False;
 while Assigned(Prop) and not Result do begin
  Result := Prop.Parent = Self;
  Prop := Prop.Parent;
 end;
end;

// TIdHistoryAction ///////////////////////////////////////////////////////////

procedure TIdHistoryAction.DoBeginAction(Source: TObject);
begin
 inherited;
 if State <> hsRecord then exit;
 if FSourceId = 0 then ChangeState(hsIdle);
end;

// TPropHistoryAction /////////////////////////////////////////////////////////

function TPropHistoryAction.ProcessSource(Stream: TStream; Source: TObject;
  DoLoad: boolean): boolean;
var Filer: TFlexFiler;
    Prop: TCustomProp;
    PropList: TPropList;
begin
 Result := false;
 if not (Source is TCustomProp) then exit;
 // Find property
 Prop := TCustomProp(Source);
 PropList := Prop.Owner;
 Filer := Nil;
 // Set PropList History state (to skip read-only checking in load phase)
 PropList.HistoryStateForList := Owner.State;
 try
  // Create filer
  Filer := TFlexFiler.Create(Stream);
  if DoLoad
   then Result := Assigned(PropList.LoadProperty(Filer, Filer.LoadStr))
   else Result := PropList.SaveProperty(Filer, Prop, '', '', true)
 finally
  PropList.HistoryStateForList := hsIdle;
  Filer.Free;
 end;
end;

// TPropHistoryGroup //////////////////////////////////////////////////////////

constructor TPropHistoryGroup.Create(AOwner: THistory; AParent: THistoryGroup);
begin
 inherited;
 FDestroyIfEmpty := true;
end;

// TPropRefList ///////////////////////////////////////////////////////////////

constructor TPropRefList.Create;
begin
 inherited;
 FList := TList.Create;
end;

destructor TPropRefList.Destroy;
begin
 Clear;
 FList.Free;
 FIdExist.Free;
 FIdRecode.Free;
 inherited;
end;

function TPropRefList.AddRef(AProp: TCustomProp; const APropName: ShortString;
  AData: PVariant): integer;
var Item: PPropRefItem;
//    i: integer;
begin
{
 for i:=0 to FList.Count-1 do with PPropRefItem(FList[i])^ do
  if (Prop = AProp) and (APropName = PropName) then begin
   DeleteRef(i);
   break;
  end;   }
 New(Item);
 try
  FillChar(Item^, SizeOf(Item^), 0);
  Item.Prop := AProp;
  Item.PropName := APropName;
  Item.Data := AData;
  Result := FList.Add(Item);
 except
  Dispose(Item);
  raise;
 end;
end;

function TPropRefList.IndexOfRecode(OldID: integer): integer;
begin
 Result := -1;
 if not Assigned(FIdRecode) then exit;
 repeat
  inc(Result);
  Result := ListScanEx(pointer(OldID), FIdRecode.List, Result, FIdRecode.Count);
 until (Result < 0) or (Result and 1 = 0);
end;

function TPropRefList.AddIDAlias(OldID, NewID: integer): boolean;
var Index: integer;
begin
 Result := false;
 if (OldID = 0) or (NewID = 0) then exit;
 if OldID = NewID then begin
  // No recode
  if not Assigned(FIdExist) then FIdExist := TIdPool.Create;
  FIdExist.Use(OldID);
 end else begin
  // Need add id recode
  if not Assigned(FIdRecode) then FIdRecode := TList.Create;
  Index := IndexOfRecode(OldID);
  if Index >= 0 then exit;
  FIdRecode.Add(pointer(OldID));
  FIdRecode.Add(pointer(NewID));
 end;
 Result := true;
end;

function TPropRefList.GetIDAlias(OldID: integer): integer;
var Index: integer;
begin
 Result := 0;
 if Assigned(FIdExist) and FIdExist.Used[OldID] then
  // Id without recode
  Result := OldID
 else
 if Assigned(FIdRecode) then begin
  // Find recode id
  Index := IndexOfRecode(OldID);
  if Index >= 0 then Result := integer(FIdRecode[Index+1]);
 end;
end;

procedure TPropRefList.Clear;
begin
 while (FList.Count > 0) do DeleteRef(FList.Count-1);
 FreeAndNil(FIdExist);
 FreeAndNil(FIdRecode);
end;

procedure TPropRefList.DeleteRef(Index: integer);
begin
 with PPropRefItem(FList[Index])^ do begin
  VarClear(Data^);
  Dispose(Data);
 end;
 Dispose(PPropRefItem(FList[Index]));
 FList.Delete(Index);
end;

function TPropRefList.GetCount: integer;
begin
 Result := FList.Count;
end;

function TPropRefList.GetPropRefItem(Index: integer): PPropRefItem;
begin
 Result := PPropRefItem(FList[Index]);
end;

procedure TPropRefList.ResolveRef(Index: integer);
begin
 with PPropRefItem(FList[Index])^ do begin
  Prop.Owner.BeginResolve(Self);
  try
   Prop.PropData[String(PropName)] := Data^;
  finally
   Prop.Owner.EndResolve;
  end;
 end;
 DeleteRef(Index);
end;

procedure TPropRefList.ResolveAllRefs;
begin
 while FList.Count > 0 do ResolveRef(FList.Count-1);
 // Clear Ids aliases
 FreeAndNil(FIdExist);
 FreeAndNil(FIdRecode);
end;

// TPropList //////////////////////////////////////////////////////////////////

constructor TPropList.Create(AOwner: TObject);
begin
 inherited Create;
 FOwner := AOwner;
 FPropList := TPropNameList.Create;
 // FPropList.Sorted := True;
 FPropNameTransation := TStringList.Create();
 FHistoryIgnoreReadOnly := True;
end;

destructor TPropList.Destroy;
begin
 if Assigned(FNotifyLink) then FNotifyLink.DestroyNotify;
 Clear;
 FPropList.Free;
 FPropNameTransation.Free;
 FNotifyLink.Free;
 inherited;
end;

function TPropList.Assign(PropList: TPropList): Boolean;
var MS: TMemoryStream;
    Filer: TFlexFiler;
    s: string;
    i: integer;
begin
 Result := false;
 if not Assigned(PropList) then exit;
 MS := Nil;
 Filer := Nil;
 try
  MS := TMemoryStream.Create;
  Filer := TFlexFiler.Create(MS);
  for i:=0 to PropList.Count-1 do
   PropList.SaveProperty(Filer, PropList.ByIndex[i], '', '', True);
  // MS.SaveToFile('d:\test.fxp');  
  Filer.Rewind;
  while Filer.LoadStrCheck(s) do LoadProperty(Filer, s);
  Result := true;
 finally
  Filer.Free;
  MS.Free;
 end;
end;

function TPropList.GetNotifyLink: TNotifyLink;
begin
 if not Assigned(FNotifyLink) then FNotifyLink := TNotifyLink.Create(Self);
 Result := FNotifyLink;
end;

procedure TPropList.BeginLoading;
begin
 if (FDataMode <> plmNone) and (FDataMode <> plmLoading) then
  raise Exception.Create('Can''t switch to loading mode');
 FDataMode := plmLoading;
 inc(FDataModeCount);
end;

procedure TPropList.BeginSaving;
begin
 if (FDataMode <> plmNone) and (FDataMode <> plmSaving) then
  raise Exception.Create('Can''t switch to saving mode');
 FDataMode := plmSaving;
 inc(FDataModeCount);
end;

procedure TPropList.EndLoadSave;
begin
 if FDataModeCount = 0 then exit;
 dec(FDataModeCount);
 if FDataModeCount = 0 then FDataMode := plmNone;
end;

procedure TPropList.BeginResolve(ARefList: TPropRefList);
begin
 inc(FResolveCount);
 FRefList := ARefList;
end;

procedure TPropList.EndResolve;
begin
 if FResolveCount = 0 then exit;
 dec(FResolveCount);
 if FResolveCount = 0 then FRefList := Nil;
end;

function TPropList.GetResolving: boolean;
begin
 Result := FResolveCount > 0
end;

function TPropList.GetPropsCount: integer;
begin
 Result := FPropList.Count;
end;

function TPropList.GetPropByIndex(Index: integer): TCustomProp;
begin
 Result := TCustomProp(FPropList.Objects[Index]);
end;

function TPropList.GetPropByName(const Name: string): TCustomProp;
var Index: integer;
begin
 Index := FPropList.IndexOf(Name);
 if Index < 0
  then Result := Nil
  else Result := TCustomProp(FPropList.Objects[Index]);
end;

function TPropList.GetPropName(Index: integer): string;
begin
 Result := FPropList[Index];
end;

function TPropList.GetRealIndex(VisibleIndex: integer): integer;
var i, CurIndex: integer;
begin
 Result := -1;
 if VisibleIndex < 0 then exit;
 CurIndex := -1;
 for i:=0 to FPropList.Count-1 do
  if psVisible in TCustomProp(FPropList.Objects[i]).Style then begin
   inc(CurIndex);
   if CurIndex = VisibleIndex then begin
    Result := i;
    break;
   end;
  end;
end;

function TPropList.GetVisibleIndex(RealIndex: integer): integer;
var i: integer;
begin
 if (RealIndex < 0) or
    not (psVisible in TCustomProp(FPropList.Objects[RealIndex]).Style) then
  Result := -1
 else begin
  Result := RealIndex;
  for i:=0 to RealIndex-1 do
   if not (psVisible in TCustomProp(FPropList.Objects[i]).Style) then
    dec(Result);
 end;
end;

function TPropList.GetVisibleCount: integer;
var i: integer;
begin
 Result := FPropList.Count;
 for i:=0 to FPropList.Count-1 do
  if not (psVisible in TCustomProp(FPropList.Objects[i]).Style) then dec(Result);
end;

function TPropList.GetVisibleProp(Index: integer): TCustomProp;
begin
 Index := GetRealIndex(Index);
 if Index >= 0
  then Result := TCustomProp(FPropList.Objects[Index])
  else Result := Nil;
end;

function TPropList.GetVisiblePropName(Index: integer): string;
begin
 Index := GetRealIndex(Index);
 if Index >= 0
  then Result := FPropList[Index]
  else Result := '';
end;

function TPropList.GetVisiblePropTranslatedName(Index: integer): string;
begin
  Result := FPropNameTransation.Values[GetVisiblePropName(Index)];
  if Result = '' then
    Result := GetVisiblePropName(Index);
end;

procedure TPropList.DoBeforeChanged(Prop: TCustomProp);
var ActionClass: THistoryActionClass;
begin
 if Assigned(FHistory) and not Prop.FSkipHistoryAction then begin
  ActionClass := TPropHistoryAction;
  if Assigned(FOnPropHistoryAction) then
   FOnPropHistoryAction(Self, Prop, ActionClass);
  if Assigned(ActionClass) then FHistory.RecordAction(ActionClass, Prop);
 end;
 if Assigned(FOnPropBeforeChanged) then FOnPropBeforeChanged(Self, Prop);
 if Assigned(FNotifyLink) then FNotifyLink.PropNotify(Prop, True);
end;

procedure TPropList.DoChanged(Prop: TCustomProp);
begin
 if Assigned(FOnPropChanged) then FOnPropChanged(Self, Prop);
 if Assigned(FNotifyLink) then FNotifyLink.PropNotify(Prop, False);
end;

function TPropList.DoPropIsStored(Prop: TCustomProp;
  const PropName: string = ''): boolean;
begin
 Result := true;
 if Assigned(FOnPropStored) then FOnPropStored(Self, Prop, Result, PropName);
end;

function TPropList.IsNameValid(const AName: string): boolean;
var i: integer;
begin
 Result := False;
 for i:=0 to High(fcReserved) do
  if CompareStr(fcReserved[i], AName) = 0 then exit;
 if (FPropList.IndexOf(AName) >= 0) then exit;
 Result := true;
end;

function TPropList.AddLoadedIDAlias(LoadedID, NewID: integer): boolean;
begin
 if Assigned(FRefList)
  then Result := FRefList.AddIDAlias(LoadedID, NewID)
  else Result := false;
end;

function TPropList.Add(Prop: TCustomProp; const AName: string; const ATranslatedName: string = ''): integer;
begin
 if not IsNameValid(AName) or (FPropList.IndexOfObject(Prop) >= 0) then
  // Result := -1
  raise Exception.CreateFmt('Property %s already exist', [AName])
 else begin
  Result := FPropList.AddObject(AName, Prop);
  if Result >= 0 then
  begin
    Prop.Owner := Self;
    FPropNameTransation.Values[AName] := ATranslatedName;
  end;
 end;
end;

procedure TPropList.Delete(Index: integer);
begin
 ByIndex[Index].Free;
 FPropList.Delete(Index);
end;

procedure TPropList.Clear;
begin
 while FPropList.Count > 0 do Delete(FPropList.Count-1);
end;

function TPropList.IndexOf(Prop: TCustomProp): integer;
begin
 Result := FPropList.IndexOfObject(Prop);
end;

function TPropList.VisibleIndexOf(Prop: TCustomProp): integer;
begin
 Result := GetVisibleIndex( IndexOf(Prop) );
end;

function TPropList.IsReadOnly(Prop: TCustomProp): boolean;
begin
 if FPropValueLoading and (FHistoryStateForList in [hsUndo, hsRedo]) and
    FHistoryIgnoreReadOnly and Assigned(FHistory) then
  // Ignore property read-only style (to restore values in undo/redo)
  Result := false
 else begin
  Result := psReadOnly in Prop.Style;
  if not Result and Assigned(FOnPropReadOnly) then
   FOnPropReadOnly(Self, Prop, Result);
 end;
end;

function TPropList.GetComplexPropInstance(Prop: TCustomProp;
  const PropName: string): TCustomProp;
var PropInfo: PPropInfo;
    Value: TObject;
begin
 Result := Nil;
 // Test complex prop name
 if Prop.PublishedPropType[PropName] <> ptComplex then exit;
 PropInfo := GetPropInfo(PTypeInfo(Prop.ClassInfo), PropName);
 if not Assigned(PropInfo) or (PropInfo^.PropType^^.Kind <> tkClass) then exit;
 // Get complex prop instance
 {$IFDEF FG_D5}
 Value := TObject(integer(TypInfo.GetPropValue(Prop, PropName, true)));
 {$ELSE}
 Value := TObject(integer(FlexUtils.GetPropValue(Prop, PropName, true)));
 {$ENDIF}
 if not (Value is TCustomProp) then exit;
 Result := TCustomProp(Value);
end;

procedure TPropList.SavePropValue(Filer: TFlexFiler; const Indent: string;
  Prop: TCustomProp; const PropName: string; IsComplex: boolean);
const HexBlockSize = 32;
var PType: TPropType;
    VarValue: Variant;
    VarValueType: TVarType;
    s: string;
    Buffer, Data: PAnsiChar;
    BufSize, BufPos, DataPos, DataBlockSize: integer;
    StrCount, FullCount, ShortStrExists : integer;
    IndentLen: integer;
    i,j, Last, Size: integer;
    ComplexProp: TCustomProp;
begin
 if IsComplex then begin
  PType := Prop.PublishedPropType[PropName];
  if PType <> ptComplex
   then VarValue := Prop.PropData[PropName]
   else VarValue := Null;
 end else begin
  VarValue := Prop.PropData[''];
  PType := Prop.PublishedPropType[''];
 end;
 case PType of
  ptSimple:
    if not (VarIsEmpty(VarValue) or VarIsNull(VarValue)) then
     if Filer.Binary then begin
      // Save As Binary
      if IsComplex and (PropType(Prop.ClassType, PropName) = tkSet) then
       VarValue := '[' + VarValue + ']';
      with Filer.BinaryData do
       WritePropertyCommand(PropName, GetPropertyType(VarValue), VarValue);
     end else begin
      // Save As String
      VarValueType := VarType(VarValue);
      case VarValueType of
       varSingle,
       varDouble,
       varCurrency,
       varDate:
         s := DotFloatToStr(VarValue);
       varBoolean:
         s := BooleanWords[boolean(VarValue)];
       else
         s := VarAsType(VarValue, varString);
      end;
      if IsComplex and (PropType(Prop.ClassType, PropName) = tkSet) then begin
       s := '[' + s + ']';
      end else
      if FlexStrNeedQuote(s) then
       s := '''' + s + '''';
      Filer.SaveStr(Indent + PropName + ' = ' + s);
     end;
  ptStrList:
    if Filer.Binary then
     Filer.BinaryData.WritePropertyCommand(PropName, fbpStrList, VarValue)
    else 
    if VarIsEmpty(VarValue) or VarIsNull(VarValue) then
     Filer.SaveStr(Indent + PropName + ' = ( )')
    else
    if VarType(VarValue) and varArray <> 0 then begin
     Filer.SaveStr(Indent + PropName + ' = (');
     s := '';
     Last := VarArrayHighBound(VarValue, 1);
     for i:=0 to Last do begin
      if i = Last then s := ' )';
      Filer.SaveStr(Indent + IndentStep +
        '''' + VarAsType(VarValue[i], varString) + '''' + s);
     end;
    end;
  ptHexData:
    if Filer.Binary then
     Filer.BinaryData.WritePropertyCommand(PropName, fbpHexData, VarValue)
    else
    if VarIsEmpty(VarValue) or VarIsNull(VarValue) then
     Filer.SaveStr(Indent + PropName + ' = { }')
    else
    if VarType(VarValue) and varArray <> 0 then begin
     Size := VarArrayHighBound(VarValue, 1)+1;
     if Size = 0 then begin
      Filer.SaveStr(Indent + PropName + ' = { }')
     end else begin
      Filer.SaveStr(Indent + PropName + ' = {');
      IndentLen := Length(Indent) + Length(IndentStep);
      FullCount := Size div HexBlockSize;
      ShortStrExists := Byte(Size mod HexBlockSize <> 0);
      StrCount := FullCount + ShortStrExists;
      BufSize := StrCount * IndentLen +
                 FullCount * (2*HexBlockSize + 2{EOLN}) +
                 ShortStrExists * (2 * (Size mod HexBlockSize) + 2{EOLN}) +
                 2{end of hex-block};
      Buffer := Nil;
      Data := VarArrayLock(VarValue);
      try
       GetMem(Buffer, BufSize);
       BufPos := 0;
       DataPos := 0;
       for i:=0 to StrCount do begin
        FillChar(Buffer[BufPos], IndentLen, IndentStep[1]);
        inc(BufPos, IndentLen);
        DataBlockSize := HexBlockSize;
        if DataPos + DataBlockSize > Size then
         DataBlockSize := Size - DataPos;
        if DataBlockSize = 0 then break;
        for j:=0 to DataBlockSize-1 do
         pword(@Buffer[BufPos + 2*j])^ := ByteToHexChars(Byte(Data[DataPos + j]));
        inc(BufPos, 2*DataBlockSize);
        inc(DataPos, DataBlockSize);
        if DataPos = Size then break;
        Buffer[BufPos+0] := #$0D;
        Buffer[BufPos+1] := #$0A;
        inc(BufPos, 2);
       end;
       Buffer[BufPos+0] := ' ';
       Buffer[BufPos+1] := '}';
       Buffer[BufPos+2] := #$0D;
       Buffer[BufPos+3] := #$0A;
       Filer.SaveBuf(Buffer, BufSize);
      finally
       if Assigned(Buffer) then FreeMem(Buffer);
       VarArrayUnlock(VarValue);
      end;
     end;
    end;
  ptComplex:
    begin
     // Get complex prop instance
     ComplexProp := GetComplexPropInstance(Prop, PropName);
     if not Assigned(ComplexProp) then exit;
     SaveComplexProp(Filer, ComplexProp, PropName, Indent);
    end;
 end;
end;

procedure TPropList.SaveComplexProp(Filer: TFlexFiler; Prop: TCustomProp;
  const PropName, Indent: string; ForceSave: boolean = false);
var i: integer;
    StrList: TStringList;
    Stored: boolean;
begin
 if Prop.PropType <> ptComplex then exit;
 StrList := TStringList.Create;
 try
  Prop.GetPropNames(StrList);
  Stored := False;
  for i:=0 to StrList.Count-1 do begin
   if not ForceSave and not DoPropIsStored(Prop, StrList[i]) then continue;
   if not Stored then
    if Filer.Binary
     then Filer.BinaryData.WriteSimpleCommand(fbcComplexProperty, 1, PropName)
     else Filer.SaveStr(Indent + fcProperty + ' ' + PropName);
   SavePropValue(Filer, Indent + IndentStep, Prop, StrList[i], True);
   Stored := true;
  end;
  if Stored then
   if Filer.Binary
    then Filer.BinaryData.WriteSimpleCommand(fbcEnd)
    else Filer.SaveStr(Indent + fcEnd);
 finally
  StrList.Free;
 end;
end;

function TPropList.SaveProperty(Filer: TFlexFiler; Prop: TCustomProp;
  const Indent: string; PropName: string = '';
  ForceSave: boolean = false): boolean;
begin
 BeginSaving;
 try
  Result := ForceSave or
    (not (psDontStore in Prop.Style) and DoPropIsStored(Prop));
  if not Result then exit;
  if PropName = '' then PropName := Prop.Name;
  if Prop.PropType = ptComplex
   then SaveComplexProp(Filer, Prop, PropName, Indent, ForceSave)
   else SavePropValue(Filer, Indent, Prop, PropName, False);
 finally
  EndLoadSave;
 end;
end;

function TPropList.SaveToFiler(Filer: TFlexFiler;
  const Indent: string; ForceSave: boolean = false): boolean;
var i: integer;
begin
 BeginSaving;
 try
  for i:=0 to FPropList.Count-1 do
   SaveProperty(Filer, ByIndex[i], Indent, FPropList[i], ForceSave);
 finally
  EndLoadSave;
 end;
 Result := true;
end;

function TPropList.CompareTranslatedStrings(List: TStringList; Index1, Index2: Integer): Integer;
var s1, s2: string;
begin
  s1 := FPropNameTransation.Values[List[Index1]];
  if s1 = '' then
    s1 := List[Index1];

  s2 := FPropNameTransation.Values[List[Index2]];
  if s2 = '' then
    s2 := List[Index2];

  Result := AnsiCompareText(s1, s2);
end;

procedure TPropList.Sort;
begin
  FPropList.FlexSort(CompareTranslatedStrings);
end;

function TPropList.LoadPropValue(Filer: TFlexFiler; ComplexProp: TCustomProp;
  const First: string): TCustomProp;
var PropName, Value: string;
    a: AnsiString;
    VarValue: PVariant;
    Prop: TCustomProp;
    Finish: boolean;
    HexCount: integer;
    Buffer: PAnsiChar;
    BufSize, BufPos: integer;
    i, Index: integer;
    ArrayPtr: Pointer;
    IsBinary: boolean;
begin
 Result := Nil;
 IsBinary := Filer.Binary and (First = '');
 if IsBinary then begin
  if Filer.BinaryData.ReadCmdCommand <> fbcProperty then exit;
  PropName := Filer.BinaryData.ReadCmdKeys[0]; // Prop name
 end else
  PropName := Trim(ExtractWord(First, 1, '='));
 if not Assigned(ComplexProp) then begin
  Prop := Props[PropName];
  if not Assigned(Prop) then exit;
 end else
  Prop := Nil;
 New(VarValue);
 try
  FPropValueLoading := true;
  VarClear(VarValue^);
  if IsBinary then begin
   if Filer.BinaryData.ReadCommandData(VarValue^) < 0 then exit;
  end else begin
   Value := Trim(ExtractWord(First, 2, '='));
   if Value <> '' then
   case Value[1] of
    '('  :
      if Value[Length(Value)] <> ')' then begin
       Finish := False;
       repeat
        if not Filer.LoadStrCheck(Value) then break;
        //Value := Trim(Value);
        if Value = '' then continue;
        if Value[Length(Value)] = ')' then begin
         Finish := true;
         SetLength(Value, Length(Value)-1);
         Value := TrimRight(Value);
        end;
        if (Value[1] = '''') and (Value[Length(Value)] = '''') then begin
         if VarIsEmpty(VarValue^) then begin
          VarValue^ := VarArrayCreate([0,0], varVariant);
          Index := 0;
         end else begin
          Index := VarArrayHighBound(VarValue^, 1)+1;
          VarArrayRedim(VarValue^, Index);
         end;
         VarValue^[Index] := copy(Value, 2, Length(Value)-2);
        end;
       until Finish;
      end;
    '{'  :
      if Value[Length(Value)] <> '}' then begin
       BufSize := 0;
       BufPos := 0;
       Buffer := Nil;
       try
        Finish := False;
        repeat
         if not Filer.LoadStrCheck(Value) then break;
         if Value = '' then continue;
         if Value[Length(Value)] = '}' then begin
          Finish := true;
          SetLength(Value, Length(Value)-1);
          Value := TrimRight(Value);
         end;
         a := AnsiString(Value);
         HexCount := Length(a) div 2;
         if BufPos + HexCount >= BufSize then begin
          inc(BufSize, 16384);
          ReallocMem(Buffer, BufSize);
         end;
         for i:=0 to HexCount-1 do
          Buffer[BufPos+i] := AnsiChar(
            HexCharsToByte(Byte(a[2*i+1]) shl 8 or Byte(a[2*i+2]))
          );
         inc(BufPos, HexCount);
        until Finish;
        BufSize := BufPos;
        ReallocMem(Buffer, BufSize);
       except
        if Assigned(Buffer) then begin
         FreeMem(Buffer);
         Buffer := Nil;
        end;
       end;
       if Assigned(Buffer) then
       try
        VarValue^ := VarArrayCreate([0,BufSize-1], varByte);
        ArrayPtr := VarArrayLock(VarValue^);
        try
         System.Move(Buffer^, ArrayPtr^, BufSize);
        finally
         VarArrayUnlock(VarValue^);
        end;
       finally
        FreeMem(Buffer);
       end;
      end;
    else
      if ((Value[1] = '''') and (Value[Length(Value)] = '''')){ or
         ((Value[1] = '[') and (Value[Length(Value)] = ']')) }
       then VarValue^ := copy(Value, 2, Length(Value)-2)
       else VarValue^ := Value;
   end;
  end;
  if Assigned(ComplexProp) then begin
   Prop := ComplexProp;
   if not Filer.Binary and
      (PropType(Prop.ClassType, PropName) = tkFloat) and
      (VarType(VarValue^) = varString) then
    // Convert dot string to float
    VarValue^ := DotStrToFloat(VarValue^);
  end else
   PropName := '';
  if not Resolving and (psRef in Prop.FStyle) and Assigned(FRefList) then begin
   FRefList.AddRef(Prop, ShortString(PropName), VarValue);
   VarValue := Nil;
  end else
   Prop.PropData[PropName] := VarValue^;
 finally
  FPropValueLoading := False;
  if Assigned(VarValue) then Dispose(VarValue);
 end;
 Result := Prop;
end;

function TPropList.LoadComplexProp(Filer: TFlexFiler; const First: string;
  Prop: TCustomProp): boolean;
var PropName, s: string;
    ComplexProp: TCustomProp;
begin
 Result := false;
 try
  // Check parent property (for to be loaded) type
  if Prop.PropType <> ptComplex then exit;
  if Filer.Binary and (First = '') then begin
   if Filer.BinaryData.ReadCmdCommand <> fbcComplexProperty then exit;
   PropName := Filer.BinaryData.ReadCmdKeys[0];
   ComplexProp := GetComplexPropInstance(Prop, PropName);
   if not Assigned(ComplexProp) then exit;
   // Load complex prop published properties
   while Filer.BinaryData.ReadCommand do
    case Filer.BinaryData.ReadCmdCommand of
     fbcEnd:
       break;
     fbcComplexProperty:
       LoadComplexProp(Filer, '', ComplexProp);
     else
       LoadPropValue(Filer, ComplexProp, '');
    end; 
  end else begin
   // Define complex prop instance
   PropName := TrimRight(ExtractWord(First, 2, ' '));
   ComplexProp := GetComplexPropInstance(Prop, PropName);
   if not Assigned(ComplexProp) then exit;
   // Load complex prop published properties
   while Filer.LoadStrCheck(s) do
    if StrBeginsFrom(s, fcEnd) then
     break
    else
    if StrBeginsFrom(s, fcProperty) then
     LoadComplexProp(Filer, s, ComplexProp)
    else
     LoadPropValue(Filer, ComplexProp, s);
  end;
  // Complex prop loaded successfully
  Result := true;
 finally
  if not Result then Filer.LoadSkipToEnd;
 end;
end;

function TPropList.LoadProperty(Filer: TFlexFiler;
  const First: string; WithoutBeginLoading: boolean = false): TCustomProp;
var s: string;
begin
 if not WithoutBeginLoading then BeginLoading;
 try
  if Filer.Binary and (First = '') then begin
   // Ignore "First" param
   if Filer.BinaryData.ReadCmdCommand = fbcComplexProperty then begin
    s := Filer.BinaryData.ReadCmdKeys[0];
    Result := Props[s];
    if not Assigned(Result) then begin;
     Filer.LoadSkipToEnd;
     exit;
    end;
    while Filer.BinaryData.ReadCommand do
     case Filer.BinaryData.ReadCmdCommand of
      fbcComplexProperty:
        LoadComplexProp(Filer, '', Result);
      fbcProperty:
        LoadPropValue(Filer, Result, '');
      fbcEnd:
        break;
     end;
   end else
    Result := LoadPropValue(Filer, Nil, '');
  end else
  if StrBeginsFrom(First, fcProperty) then begin
   s := TrimRight(ExtractWord(First, 2, ' '));
   Result := Props[s];
   if not Assigned(Result) then
    Filer.LoadSkipToEnd
   else
   while Filer.LoadStrCheck(s) do
    if StrBeginsFrom(s, fcEnd) then
     break
    else
    if StrBeginsFrom(s, fcProperty) then
     LoadComplexProp(Filer, s, Result)
    else
     LoadPropValue(Filer, Result, s);
  end else
   Result := LoadPropValue(Filer, Nil, First);
 finally
  if not WithoutBeginLoading then EndLoadSave;
 end;
end;

procedure TPropList.LoadFromFiler(Filer: TFlexFiler; const First: string;
  RefList: TPropRefList);
begin
 FRefList := RefList;
 LoadProperty(Filer, First);
 FRefList := Nil;
end;

// TEnumProp //////////////////////////////////////////////////////////////////

constructor TEnumProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FEnumList := TStringList.Create;
 FIsEnum := true;
end;

destructor TEnumProp.Destroy;
begin
 FEnumList.Free;
 inherited;
end;

function TEnumProp.AddItem(const s: string): integer;
begin
 Result := FEnumList.Add(s);
end;

procedure TEnumProp.SetItem(EnumIndex: integer; const s: string);
begin
 if EnumIndex < 0 then exit;
 while FEnumList.Count <= EnumIndex do FEnumList.Add('');
 FEnumList[EnumIndex] := s;
end;

function TEnumProp.SetEnumType(TypeInfo: PTypeInfo): boolean;
var i: integer;
    T: PTypeData;
begin
 Result := False;
 if TypeInfo.Kind <> tkEnumeration then exit;
 FEnumList.Clear;
 T := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
 if T.MinValue < 0 then exit;
 for i:=T.MinValue to T.MaxValue do
  FEnumList.Add(GetEnumName(TypeInfo, i));
 Result := True;
end;

procedure TEnumProp.GetEnumList(List: TStrings);
begin
 List.BeginUpdate;
 try
  if Assigned(FEnumList)
   then List.Assign(FEnumList)
   else List.Clear;
 finally
  List.EndUpdate;
 end;
end;

function TEnumProp.GetDisplayValue: string;
begin
 Result := FEnumList[FEnumIndex];
end;

function TEnumProp.GetEnumCount: integer;
begin
 Result := FEnumList.Count;
end;

procedure TEnumProp.SetEnumIndex(Value: integer);
begin
 if (Value = FEnumIndex) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FEnumIndex := Value;
 DoChanged;
end;

function TEnumProp.GetPropValue(const PropName: string): Variant;
begin
 Result := FEnumList[FEnumIndex];
end;

procedure TEnumProp.SetPropValue(const PropName: string; Value: Variant);
var NewIndex: integer;
begin
 NewIndex := FEnumList.IndexOf(VarAsType(Value, varString));
 if NewIndex >= 0 then EnumIndex := NewIndex;
end;

// TBoolProp //////////////////////////////////////////////////////////////////

constructor TBoolProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 AddItem(BooleanWords[false]{'False'});
 AddItem(BooleanWords[true]{'True'});
end;

function TBoolProp.GetPropValue(const PropName: string): Variant;
begin
 Result := FEnumIndex <> 0;
end;

function TBoolProp.GetValue: boolean;
begin
 Result := Boolean(FEnumIndex);
end;

procedure TBoolProp.SetPropValue(const PropName: string; Value: Variant);
begin
 if VarType(Value) = varBoolean
  then EnumIndex := byte(boolean(Value))
  else inherited;
end;

procedure TBoolProp.SetValue(Value: boolean);
begin
 EnumIndex := integer(Value);
end;

// TIntProp ///////////////////////////////////////////////////////////////////

constructor TIntProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FValue := 0;
end;

function TIntProp.GetValue: integer;
begin
 if Assigned(FOnGetValue) then
  FOnGetValue(Self, Result)
 else
 if Assigned(FSource) then
  Result := FSource^
 else
  Result := FValue;
end;

procedure TIntProp.SetValue(Value: integer);
begin
 FBeforeValue := Value;
 if Assigned(FSource) then begin
  if (FSource^ = FBeforeValue) or Owner.IsReadOnly(Self) then exit;
 end else
  if (FValue = FBeforeValue) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if Assigned(FSource) then FSource^ := FBeforeValue;
 FValue := FBeforeValue;
 DoChanged;
end;

function TIntProp.GetDisplayValue: string;
begin
 if psScalable in FStyle
  then Result := Format(FloatDisplayFormat, [GetValue / PixelScaleFactor])
  else Result := IntToStr(GetValue);
end;

procedure TIntProp.SetDisplayValue(const Value: string);
begin
 if psScalable in FStyle then
  SetPropValue('', integer(Round(StrToFloat(Value) * PixelScaleFactor)) )
 else
  inherited;
end;

function TIntProp.GetPropValue(const PropName: string): Variant;
begin
 Result := Value;
end;

procedure TIntProp.SetPropValue(const PropName: string; Value: Variant);
begin
 Self.Value := VarAsType(Value, varInteger);
end;

// TLongWordProp //////////////////////////////////////////////////////////////

constructor TLongWordProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FValue := 0;
end;

function TLongWordProp.GetDisplayValue: string;
begin
 if psScalable in FStyle
  then Result := Format(FloatDisplayFormat, [FValue / PixelScaleFactor])
  else Result := IntToStr(FValue);
end;

procedure TLongWordProp.SetDisplayValue(const Value: string);
begin
 if psScalable in FStyle then
  SetPropValue('', integer(Round(StrToFloat(Value) * PixelScaleFactor)) )
 else
  inherited;
end;

procedure TLongWordProp.SetValue(Value: LongWord);
begin
 if (FValue = Value) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FValue := Value;
 DoChanged;
end;

function TLongWordProp.GetPropValue(const PropName: string): Variant;
begin
 Result := Variant(FValue);
end;

procedure TLongWordProp.SetPropValue(const PropName: string; Value: Variant);
begin
 Self.Value := LongWord(VarAsType(Value, varInteger));
end;

// TDateTimeProp //////////////////////////////////////////////////////////////

constructor TDateTimeProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FValue := 0;
end;

function TDateTimeProp.GetDisplayValue: string;
begin
 Result := DateTimeToStr(FValue);
end;

procedure TDateTimeProp.SetDisplayValue(const Value: string);
begin
 SetPropValue('', StrToDateTime(Value));
end;

function TDateTimeProp.GetPropValue(const PropName: string): Variant;
begin
 // Result := DotFloatToStr(FValue);
 Result := FValue;
end;

procedure TDateTimeProp.SetPropValue(const PropName: string;
  Value: Variant);
begin
 // Self.Value := DotStrToFloat(Value); // TDateTime(VarAsType(Value, varDouble));
 case VarType(Value) of
  varSingle,
  varDouble,
  varCurrency,
  varDate:
    Self.Value := Value;
  else
    Self.Value := DotStrToFloat(Value);
 end;
end;

procedure TDateTimeProp.SetValue(Value: TDateTime);
begin
 if (FValue = Value) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FValue := Value;
 DoChanged;
end;

// TDoubleProp //////////////////////////////////////////////////////////////

constructor TDoubleProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FValue := 0;
end;

function TDoubleProp.GetDisplayValue: string;
begin
 Result := FloatToStr(FValue); // DateTimeToStr(FValue);
end;

procedure TDoubleProp.SetDisplayValue(const Value: string);
begin
 SetPropValue('', StrToFloat(Value));
end;

function TDoubleProp.GetPropValue(const PropName: string): Variant;
begin
 // Result := DotFloatToStr(FValue);
 Result := FValue;
end;

procedure TDoubleProp.SetPropValue(const PropName: string;
  Value: Variant);
begin
 // Self.Value := DotStrToFloat(Value); // TDateTime(VarAsType(Value, varDouble));
 case VarType(Value) of
  varSingle,
  varDouble,
  varCurrency,
  varDate:
    Self.Value := Value;
  else
    Self.Value := DotStrToFloat(Value);
 end;
end;

procedure TDoubleProp.SetValue(Value: Double);
begin
 if (FValue = Value) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FValue := Value;
 DoChanged;
end;

// TStrProp ///////////////////////////////////////////////////////////////////

constructor TStrProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FValue := '';
end;

function TStrProp.GetDisplayValue: string;
begin
 Result := GetValue;
end;

function TStrProp.GetValue: string;
begin
 if Assigned(FOnGetString)
  then FOnGetString(Self, Result)
  else Result := FValue;
end;

procedure TStrProp.SetValue(const Value: string);
begin
 if (Value = FValue) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FValue := Value;
 DoChanged;
end;

function TStrProp.GetPropValue(const PropName: string): Variant;
begin
 Result := Value;
end;

procedure TStrProp.SetPropValue(const PropName: string; Value: Variant);
begin
 Self.Value := VarAsType(Value, varString);
end;

// TStrListProp ///////////////////////////////////////////////////////////////

constructor TStrListProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FStrList := TStringList.Create;
 FPropType := ptStrList;
end;

destructor TStrListProp.Destroy;
begin
 FStrList.Free;
 inherited;
end;

function TStrListProp.GetDisplayValue: string;
begin
 Result := '(Text)'; 
end;

procedure TStrListProp.Assign(Source: TStrings);
begin
 if Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FStrList.Assign(Source);
 DoChanged;
end;

procedure TStrListProp.AssignTo(Dest: TStrings);
begin
 Dest.Assign(FStrList);
end;

function TStrListProp.Add(const s: string): integer;
begin
 Result := FStrList.Count;
 if not Insert(FStrList.Count, s) then Result := -1;
end;

function TStrListProp.Insert(Index: Integer; const S: string): boolean;
begin
 if Owner.IsReadOnly(Self) then begin
  Result := False;
  exit;
 end;
 DoBeforeChanged;
 FStrList.Insert(Index, s);
 DoChanged;
 Result := true;
end;

procedure TStrListProp.Clear;
begin
 if Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FStrList.Clear;
 DoChanged;
end;

procedure TStrListProp.Delete(Index: integer);
begin
 if Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FStrList.Delete(Index);
 DoChanged;
end;

procedure TStrListProp.RemoveByName(const Name: string);
begin
 Delete(IndexOfName(Name));
end;

function TStrListProp.GetLine(Index: integer): string;
begin
 Result := FStrList[Index];
end;

function TStrListProp.GetLinesCount: integer;
begin
 Result := FStrList.Count;
end;

function TStrListProp.GetName(Index: integer): string;
begin
 Result := Trim(FStrList.Names[Index]);
end;

procedure TStrListProp.SetName(Index: integer; const Value: string);
var Line: string;
    i: integer;
begin
 Line := FStrList[Index];
 i := Pos('=', Line);
 if i < 0 then exit;
 FStrList[Index] := Trim(Value) + '='+ copy(Line, i+1, MaxInt);
end;

function TStrListProp.GetValue(const Name: string): string;
var i: integer;
begin
 i := FStrList.IndexOfName(Name);
 if i >= 0
  then Result := GetValueByIndex(i)
  else Result := '';
end;

function TStrListProp.GetValueByIndex(Index: integer): string;
var i, j, Len: integer;
begin
 Result := FStrList[Index];
 i := Pos('=', Result);
 if i > 0 then begin
  j := i+1;
  Len := Length(Result);
  while (j <= Len) and (Result[j] = ' ') do inc(j);
  if j > Len
   then Result := ''
   else Result := copy(Result, j, MaxInt);
 end else
  Result := '';
end;

procedure TStrListProp.SetValue(const Name, Value: string);
var i: integer;
begin
 if Owner.IsReadOnly(Self) then exit;
 i := FStrList.IndexOfName(Name);
 if i < 0 then i := FStrList.Add(Name + '=');
 SetValueByIndex(i, Value)
end;

procedure TStrListProp.SetValueByIndex(Index: integer; const Value: string);
var Line: string;
begin
 if Owner.IsReadOnly(Self) then exit;
 Line := Trim(FStrList.Names[Index]);
 if Line <> '' then begin
  DoBeforeChanged;
  FStrList[Index] := Line + '=' + Value;
  DoChanged;
 end;
end;

function TStrListProp.GetText: TCaption;
var Len: integer;
begin
 Result := FStrList.Text;
 Len := Length(Result);
 if Len > 1 then SetLength(Result, Len-2);
end;

function TStrListProp.IndexOf(const s: string): integer;
begin
 Result := FStrList.IndexOf(s);
end;

function TStrListProp.IndexOfName(const s: string): integer;
begin
 Result := FStrList.IndexOfName(s);
end;

procedure TStrListProp.SetLine(Index: integer; const Value: string);
begin
 if Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FStrList[Index] := Value;
 DoChanged;
end;

procedure TStrListProp.SetText(const Value: TCaption);
begin
 if Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FStrList.Text := Value;
 DoChanged;
end;

function TStrListProp.GetPropValue(const PropName: string): Variant;
var i: integer;
begin
 if FStrList.Count = 0 then
  VarClear(Result)
 else begin
  Result := VarArrayCreate([0, FStrList.Count-1], varVariant);
  for i:=0 to FStrList.Count-1 do
   Result[i] := FStrList[i];
 end;
end;

procedure TStrListProp.SetPropValue(const PropName: string; Value: Variant);
var i: integer;
begin
 if Owner.IsReadOnly(Self) then exit;
 if VarIsEmpty(Value) then
  SetText('')
 else
 if VarType(Value) and varArray = 0 then
  exit
 else begin
  DoBeforeChanged;
  FStrList.Clear;
  for i:=0 to VarArrayHighBound(Value, 1) do FStrList.Add(Value[i]);
  DoChanged;
 end;
end;

// TUserDataProp //////////////////////////////////////////////////////////////

// No methods

// TDataProp //////////////////////////////////////////////////////////////////

constructor TDataProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FPropType := ptHexData;
 Exclude(FStyle, psVisible);
end;

function TDataProp.GetDisplayValue: string;
begin
 Result := '(Data)';
end;

function TDataProp.GetPropValue(const PropName: string): Variant;
begin
 if Assigned(FOnGetPropData)
  then FOnGetPropData(Self, Result)
  else VarClear(Result);
end;

procedure TDataProp.SetPropValue(const PropName: string; Value: Variant);
begin
 if Assigned(FOnSetPropData) then FOnSetPropData(Self, Value);
end;

// TBrushProp /////////////////////////////////////////////////////////////////

constructor TBrushProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FBrush := TBrush.Create;
 FBrush.Color := clNone;
 FPropType := ptComplex;
 FBitmapDisplay := bdTile;
end;

destructor TBrushProp.Destroy;
begin
 inherited;
 ResetCache;
 FBrush.Free;
 FBitmap.Free;
end;

procedure TBrushProp.Setup(Canvas: TCanvas; Scale: integer = 100);
begin
 Canvas.Brush.Assign(FBrush);
 if (FMethod <> bmHatch) or (FBrush.Color = clNone) then
  Canvas.Brush.Style := bsClear;
end;

procedure TBrushProp.Translate(const TranslateInfo: TTranslateInfo);
const
 CCWStyles: array[0..3] of TGradientStyle = (
   gsTopLeft, gsBottomLeft, gsBottomRight, gsTopRight );
var
 Rotate, i, Idx: integer;

 procedure XchgStyle;
 begin
  if FGradStyle = gsHorizontal
   then GradStyle := gsVertical
   else GradStyle := gsHorizontal;
 end;

 procedure XchgColors;
 var RColor: TColor;
 begin
  RColor := FGradBeginColor;
  GradBeginColor := FGradEndColor;
  GradEndColor := RColor;
 end;

begin
 if FMethod <> bmGradient then exit;
 Rotate := TranslateInfo.Rotate;
 //if TranslateInfo.Mirror then inc(Rotate, 180);
 Rotate := Round((Rotate mod 360) / 90);
 if Rotate < 0 then Rotate := 4 + Rotate;
 // "Rotate" gradient style
 case FGradStyle of
  gsHorizontal:
    begin
     if TranslateInfo.Mirror then XchgColors;
     case Rotate of
      1: begin XchgStyle; XchgColors; end;  // 90
      2: begin XchgColors; end;             // 180
      3: begin XchgStyle; end;              // 270
     end;
    end;
  gsVertical:
    case Rotate of
     1: begin XchgStyle; end;              // 90
     2: begin XchgColors; end;             // 180
     3: begin XchgStyle; XchgColors; end;  // 270
    end;
  gsTopLeft,
  gsTopRight,
  gsBottomLeft,
  gsBottomRight:
    begin
     Idx := -1;
     for i:=0 to High(CCWStyles) do
      if CCWStyles[i] = FGradStyle then begin
       Idx := i;
       break;
      end;
     if Idx < 0 then exit;
     if TranslateInfo.Mirror then Idx := High(CCWStyles) - Idx;
     Idx := (Idx + Rotate) mod (High(CCWStyles)+1);
     GradStyle := CCWStyles[Idx];
    end;
 end;
end;

function TBrushProp.GetIsPaintAlternate: boolean;
begin
 Result := FMethod <> bmHatch;
end;

function TBrushProp.GetIsClear: boolean;
begin
 case FMethod of
  bmHatch    : Result := (FBrush.Color = clNone) or (FBrush.Style = bsClear);
  bmGradient : Result := false;
  bmBitmap   : if Assigned(FBitmap)
                then Result := FBitmap.Empty
                else Result := true;
  else         Result := false;
 end;
end;

procedure TBrushProp.PaintAlternate(Canvas: TCanvas; var PaintRect: TRect;
  const RefreshRect: TRect; Scale: integer = 100;
  ClipTransparent: boolean = false);
var Cache: PTiledBitmapCache;
begin
 case FMethod of
  bmGradient:
    PaintGradient(Canvas, PaintRect, FGradStyle, FGradBeginColor, FGradEndColor,
      Canvas.Pen.Mode);
  bmBitmap:
    if Assigned(FBitmap) then begin
     if not FBitmapCache
      then Cache := Nil
      else Cache := @FCache;
     //PaintTailed(Canvas, PaintRect, RefreshRect, FBitmap, Cache);
     PaintBitmap(Canvas, PaintRect, RefreshRect, FBitmap, FBitmapDisplay,
       Cache, Scale, ClipTransparent);
    end;
 end;
end;

function TBrushProp.GetDisplayValue: string;
begin
 Result := '(Brush)';
end;

function TBrushProp.GetPropType(const PropName: string): TPropType;
begin
 if CompareText(PropName, 'Bitmap') = 0
  then Result := ptHexData
  else Result := ptSimple;
end;

function TBrushProp.GetColor: TColor;
begin
 Result := FBrush.Color;
end;

function TBrushProp.GetStyle: TBrushStyle;
begin
 Result := FBrush.Style;
end;

procedure TBrushProp.SetMethod(const Value: TBrushMethod);
begin
 if (Value = FMethod) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FMethod := Value;
 DoChanged;
end;

procedure TBrushProp.ResetCache;
begin
 if FBitmapCache then begin
  // Reset cache
  DeleteObject(FCache.Handle);
  FCache.Handle := 0;
  FCache.Width := 0;
  FCache.Height := 0;
 end;
end;

procedure TBrushProp.BitmapChange(Sender: TObject);
begin
 if not Assigned(FBitmap) then exit;
 FBitmapMaskColor := FBitmap.TransparentColor;
 FBitmapMasked := FBitmap.Transparent;
 ResetCache;
 DoChanged;
end;

procedure TBrushProp.InitBitmap(Width: integer = 0; Height: integer = 0);
var Bmp: TBitmap;
begin
 Bmp := TBitmap.Create;
 try
  Bmp.Width := Width;
  Bmp.Height := Height;
  SetBitmap(Bmp);
 finally
  Bmp.Free;
 end;
end;

procedure TBrushProp.FreeBitmap;
begin
 if not Assigned(FBitmap) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FBitmap.OnChange := Nil;
 FreeAndNil(FBitmap);
 FBitmapLinkName := '';
 DoChanged;
end;

function TBrushProp.UpdateBitmapLink: boolean;
var LastDir: string;
begin
 Result := false;
 if FBitmapLinkName = '' then exit;
 if Assigned(FBitmap)
  then FBitmap.OnChange := Nil
  else InitBitmap;
 if Assigned(ResolveBitmapLink) then begin
  ResolveBitmapLink(Self, FBitmapLinkName, FBitmap);
  Result := true;
 end else begin
  LastDir := GetCurrentDir;
  try
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
   if FileExists(ExpandFilename(FBitmapLinkName)) then begin
    FBitmap.LoadFromFile(ExpandFilename(FBitmapLinkName));
    Result := true;
   end else
    //FPicture.Graphic := Nil;
    FreeBitmap;
  finally
   SetCurrentDir(LastDir);
  end;
 end;
 BitmapChange(FBitmap);
end;

procedure TBrushProp.SetBitmap(const Value: TBitmap);
begin
 if (Value = FBitmap) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if not Assigned(FBitmap)
  then FBitmap := TBitmap.Create
  else FBitmap.OnChange := Nil;
 ResetCache;
 if Assigned(Value) then begin
  FBitmap.Assign(Value);
  FBitmap.TransparentColor := FBitmapMaskColor;
  FBitmap.Transparent := FBitmapMasked;
  FBitmap.OnChange := BitmapChange;
 end else begin
  FBitmap.Free;
  FBitmap := Nil;
 end;
 DoChanged;
end;

procedure TBrushProp.SetBitmapMaskColor(const Value: TColor);
begin
 if (Value = FBitmapMaskColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if Assigned(FBitmap) then
  FBitmap.TransparentColor := Value
 else begin
  FBitmapMaskColor := Value;
  DoChanged;
 end;
end;

procedure TBrushProp.SetBitmapMasked(const Value: boolean);
begin
 if (Value = FBitmapMasked) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if Assigned(FBitmap) then
  FBitmap.Transparent := Value
 else begin
  FBitmapMasked := Value;
  DoChanged;
 end;
end;

procedure TBrushProp.SetBitmapCache(const Value: boolean);
begin
 if (Value = FBitmapCache) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FBitmapCache := Value;
 ResetCache;
 DoChanged;
end;

procedure TBrushProp.SetBitmapDisplay(const Value: TBitmapDisplay);
begin
 if (Value = FBitmapDisplay) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FBitmapDisplay := Value;
 DoChanged;
end;

procedure TBrushProp.SetBitmapLinkName(const Value: string);
begin
 if (Value = FBitmapLinkName) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FBitmapLinkName := Value;
 if FBitmapLinkName = ''
  then FreeBitmap
  else UpdateBitmapLink;
 DoChanged;
end;

procedure TBrushProp.SetColor(const Value: TColor);
begin
 if (Value = FBrush.Color) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FBrush.Color := Value;
 DoChanged;
end;

procedure TBrushProp.SetStyle(const Value: TBrushStyle);
begin
 if (Value = FBrush.Style) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FBrush.Style := Value;
 DoChanged;
end;

procedure TBrushProp.SetGradBeginColor(const Value: TColor);
begin
 if (Value = FGradBeginColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FGradBeginColor := Value;
 DoChanged;
end;

procedure TBrushProp.SetGradEndColor(const Value: TColor);
begin
 if (Value = FGradEndColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FGradEndColor := Value;
 DoChanged;
end;

procedure TBrushProp.SetGradStyle(const Value: TGradientStyle);
begin
 if (Value = FGradStyle) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FGradStyle := Value;
 DoChanged;
end;

function TBrushProp.GetPropValue(const PropName: string): Variant;
var HexStream: TMemoryStream;
    VarArray: Pointer;
    Count: integer;
begin
 if CompareText(PropName, 'Bitmap') = 0 then begin
  if not Assigned(FBitmap) then begin
   VarClear(Result);
   exit;
  end;
  HexStream := TMemoryStream.Create;
  try
   FBitmap.SaveToStream(HexStream);
   Count := HexStream.Size;
   Result := VarArrayCreate([0, Count-1], varByte);
   VarArray := VarArrayLock(Result);
   try
    Move(HexStream.Memory^, VarArray^, Count);
   finally
    VarArrayUnlock(Result);
   end;
  finally
   HexStream.Free;
  end;
 end else
  Result := inherited GetPropValue(PropName);
end;

procedure TBrushProp.SetPropValue(const PropName: string; Value: Variant);
var VArray: Pointer;
    HexStream: TMemoryStream;
    Count: integer;
    B: TBitmap;
begin
 if CompareText(PropName, 'Bitmap') = 0 then begin
  Bitmap := Nil;
  if not VarIsEmpty(Value) and ((VarType(Value) and varArray) <> 0) then begin
   HexStream := Nil;
   Count := VarArrayHighBound(Value, 1) + 1;
   B := Nil;
   VArray := VarArrayLock(Value);
   try
    HexStream := TMemoryStream.Create;
    HexStream.Write(VArray^, Count);
    HexStream.Position := 0;
    B := TBitmap.Create;
    B.LoadFromStream(HexStream);
    Bitmap := B;
   finally
    VarArrayUnlock(Value);
    HexStream.Free;
    B.Free;
   end;
  end;
 end else
  inherited;
end;

function TBrushProp.StoreBitmap: Boolean;
begin
 Result := Assigned(FBitmap) and (FBitmapLinkName = '');
end;

function TBrushProp.StoreBitmapLinkName: Boolean;
begin
 Result := FBitmapLinkName <> '';
end;

// TLineCapProp ////////////////////////////////////////////////////////////////

constructor TLineCapProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FPropType := ptComplex;
 FCapStyle := psNoCap;
 FSize := DefaultLineCapSize;
 FOutlineColor := clBlack;
 FFillColor := clWhite;
end;

destructor TLineCapProp.Destroy;
begin
 inherited;
end;

function TLineCapProp.GetActiveSize(PenWidth: integer;
  PixelSize: integer = PixelScaleFactor): integer;
begin
 if FFixedSize then begin
  if PixelSize = PixelScaleFactor
   then Result := FSize
   else Result := MulDiv(FSize, PixelSize, PixelScaleFactor);
 end else
  Result := MulDiv(DefaultLineCapSize, PixelSize, PixelScaleFactor) +
    2 * (PenWidth - PixelSize);
end;

function TLineCapProp.GetDisplayValue: string;
begin
 Result := '(LineCap)';
end;

procedure TLineCapProp.SetCapStyle(const Value: integer);
begin
 if (Value = FCapStyle) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FCapStyle := Value;
 DoChanged;
end;

procedure TLineCapProp.SetFillColor(const Value: TColor);
begin
 if (Value = FFillColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFillColor := Value;
 DoChanged;
end;

procedure TLineCapProp.SetFixedFillColor(const Value: boolean);
begin
 if (Value = FFixedFillColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFixedFillColor := Value;
 DoChanged;
end;

procedure TLineCapProp.SetFixedOutlineColor(const Value: boolean);
begin
 if (Value = FFixedOutlineColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFixedOutlineColor := Value;
 DoChanged;
end;

procedure TLineCapProp.SetFixedSize(const Value: boolean);
begin
 if (Value = FFixedSize) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFixedSize := Value;
 DoChanged;
end;

procedure TLineCapProp.SetOutlineColor(const Value: TColor);
begin
 if (Value = FOutlineColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FOutlineColor := Value;
 DoChanged;
end;

procedure TLineCapProp.SetSize(const Value: integer);
begin
 if (Value = FSize) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FSize := Value;
 DoChanged;
end;

// TPenProp ///////////////////////////////////////////////////////////////////

constructor TPenProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FPropType := ptComplex;
 FWidth := PixelScaleFactor; // 1 pixel
 FStyle := psSolid;
 FColor := clBlack;
 FMode := pmCopy;
 FEndCap := pecRound;
 FJoin := pjRound;
 FSolidAsInside := True;
end;

function TPenProp.GetDisplayValue: string;
begin
 Result := '(Pen)';
end;

procedure TPenProp.SetWidth(const Value: integer);
begin
 if (Value = FWidth) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FWidth := Value;
 DoChanged;
end;

procedure TPenProp.SetMode(const Value: TPenMode);
begin
 if (Value = FMode) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FMode := Value;
 DoChanged;
end;

procedure TPenProp.SetColor(const Value: TColor);
begin
 if (Value = FColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FColor := Value;
 DoChanged;
end;

procedure TPenProp.SetEndCap(const Value: TPenEndCap);
begin
 if (Value = FEndCap) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FEndCap := Value;
 DoChanged;
end;

procedure TPenProp.SetJoin(const Value: TPenJoin);
begin
 if (Value = FJoin) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FJoin := Value;
 DoChanged;
end;

procedure TPenProp.SetSolidAsInside(const Value: boolean);
begin
 if (Value = FSolidAsInside) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FSolidAsInside := Value;
 DoChanged;
end;

procedure TPenProp.SetStyle(const Value: TPenStyle);
begin
 if (Value = FStyle) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FStyle := Value;
 DoChanged;
end;

function TPenProp.GetActiveWidth: integer;
var PenWidth: integer;
    PenStyle: TPenStyle;
    IsGeometricPen: boolean;
begin
 if (FColor = clNone) or (FStyle = psClear) then
  Result := 0
 else begin
  GetPaintData(PenWidth, PenStyle, IsGeometricPen);
  if PenWidth = 0 then
   Result := PixelScaleFactor
  else
   Result := FWidth;
 end;
end;

procedure TPenProp.GetPaintData(var AWidth: integer; var AStyle: TPenStyle;
  var IsGeometricPen: boolean; Scale: integer = 100);
begin
 if FWidth <= 0 then begin
  AWidth := 0;
  AStyle := psClear;
  IsGeometricPen := False;
  Exit;
 end;
 // Scale pen width
 AWidth := ScaleValue(FWidth, Scale);
 if AWidth < 1 then AWidth := 1;
 // Define pen style
 AStyle := FStyle;
 if FColor = clNone then
  AStyle := psClear
 else
 // Check style for InsideFrame
 if (AStyle = psSolid) and (AWidth > 1) and FSolidAsInside then
  AStyle := psInsideFrame;
 // Define is pen geometric
 IsGeometricPen := False;
 if AStyle <> psClear then begin
  if ((FStyle <> psSolid) and (AWidth > 1)) or
     (FEndCap <> pecRound) or (FJoin <> pjRound) then begin
   if not GeometricPenChecked then CheckGeometricPen;
   IsGeometricPen := GeometricPenEnabled;
  end;
 end else
  AWidth := 0;
 // Reset width for stadard pen width non solid style
 if not IsGeometricPen and (FStyle <> psSolid) then
  // Styled pens have no width
  AWidth := 0; // was 1 - !!
end;

type
  {$HINTS OFF}
  TPenPrivate = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FResource: PResource;
    FOwnerLock: PRTLCriticalSection;
  end;
  {$HINTS ON}

procedure TPenProp.Setup(Canvas: TCanvas; Scale: integer = 100);
const
  {$IFNDEF FG_D10}
  PenStyles: array[TPenStyle] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME);
  {$ELSE}
  PenStyles: array[TPenStyle] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME, PS_SOLID, PS_SOLID);
  {$ENDIF}
  PenEndCaps: array[TPenEndCap] of Word =
    (PS_ENDCAP_FLAT, PS_ENDCAP_SQUARE, PS_ENDCAP_ROUND);
  PenJoins: array[TPenJoin] of Word =
    (PS_JOIN_BEVEL, PS_JOIN_MITER, PS_JOIN_ROUND);

var PenWidth: integer;
    PenStyle: TPenStyle;
    LogBrush: TLogBrush;
    NewPen: cardinal;
    IsGeometricPen: boolean;
begin
 GetPaintData(PenWidth, PenStyle, IsGeometricPen, Scale);
 // Check is geometric pen necessry and available
 if IsGeometricPen then begin
  // Create geometric pen
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := ColorToRGB(FColor);
  NewPen := ExtCreatePen(
    PS_GEOMETRIC or PenStyles[PenStyle] or PenEndCaps[FEndCap] or PenJoins[FJoin],
    PenWidth, LogBrush, 0, Nil);
 end else
  NewPen := CreatePen(PenStyles[PenStyle], PenWidth, FColor);
 // Check existence of new pen handle.
 // In some cases GDI can generate same handle id for pens with equal parameters.
 // In such cases PenManager in Graphics.pas already contain pen with such handle
 // and just increase RefCount for it. And when pen needs to be realeasing it
 // just decrease RefCount rather then call to DeleteObject and we got a HPen leak.
 // To avoid it we need to examine the pen RefCount after setting the new handle.
 // To do this we need an access to private FResource field of the TPen class.
 with TPenPrivate(Canvas.Pen) do
  if not Assigned(FResource) or (FResource.Handle <> NewPen) then begin
   Canvas.Pen.Handle := NewPen;
   if FResource.RefCount > 1 then
    // Decrease internal GDI reference count since this HPen was allocated earlier.
    DeleteObject(NewPen);
  end else
   // This handle already in Canvas.Pen so just decrease internal GDI reference count.
   DeleteObject(NewPen);
 Canvas.Pen.Mode := FMode;
end;

// TFontProp //////////////////////////////////////////////////////////////////

constructor TFontProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FFont := TFont.Create;
 FFont.Name := 'Arial';
 FFont.Color := clBlack;
 FFontHeight := FFont.Height * PixelScaleFactor;
 FPropType := ptComplex;
 Include(FStyle, psEditForm);
end;

destructor TFontProp.Destroy;
begin
 FFont.Free;
 inherited;
end;

function TFontProp.Edit: boolean;
var FontDlg: TFontDialog;
begin
 if Assigned(FEditFormClass) then
  Result := inherited Edit
 else begin
  FontDlg := TFontDialog.Create(Nil);
  try
   FontDlg.Font.Assign(FFont);
   Result := FontDlg.Execute;
   if Result then SetFont(FontDlg.Font);
  finally
   FontDlg.Free;
  end;
 end;
end;

function TFontProp.GetCharset: TFontCharset;
begin
 Result := FFont.Charset;
end;

function TFontProp.GetColor: TColor;
begin
 Result := FFont.Color;
end;

function TFontProp.GetDisplayValue: string;
begin
 Result := '(Font)';
end;

procedure TFontProp.GetFont(var AFont: TFont);
begin
 if Assigned(AFont) then begin
  AFont.Assign(FFont);
 end;
end;

function TFontProp.GetHandle: HFont;
begin
 Result := FFont.Handle;
end;

function TFontProp.GetName: TFontName;
begin
 Result := FFont.Name;
end;

function TFontProp.GetPitch: TFontPitch;
begin
 Result := FFont.Pitch;
end;

function TFontProp.GetPixelsPerInch: Integer;
begin
 Result := FFont.PixelsPerInch;
end;

function TFontProp.GetSize: Integer;
begin
 Result := -MulDiv(Height, 72, FFont.PixelsPerInch);
end;

function TFontProp.GetStyle: TFontStyles;
begin
 Result := FFont.Style;
end;

procedure TFontProp.SetCharset(const Value: TFontCharset);
begin
 if (Value = FFont.Charset) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.Charset := Value;
 DoChanged;
end;

procedure TFontProp.SetColor(const Value: TColor);
begin
 if (Value = FFont.Color) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.Color := Value;
 DoChanged;
end;

procedure TFontProp.SetFont(const AFont: TFont);
begin
 if not Assigned(AFont) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.Assign(AFont);
 FFontHeight := -(AFont.Size * AFont.PixelsPerInch * PixelScaleFactor div 72);{AFont.Height} 
 DoChanged;
end;

procedure TFontProp.SetHandle(const Value: HFont);
begin
 if (Value = FFont.Handle) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.Handle := Value;
 FFontHeight := FFont.Height * PixelScaleFactor;
 DoChanged;
end;

procedure TFontProp.SetHeight(const Value: Integer);
begin
 if (Value = FFontHeight) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFontHeight := Value;
 FFont.Height := FFontHeight div PixelScaleFactor;
 DoChanged;
end;

procedure TFontProp.SetName(const Value: TFontName);
begin
 if (Value = FFont.Name) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.Name := Value;
 DoChanged;
end;

procedure TFontProp.SetPitch(const Value: TFontPitch);
begin
 if (Value = FFont.Pitch) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.Pitch := Value;
 DoChanged;
end;

procedure TFontProp.SetPixelsPerInch(const Value: Integer);
begin
 if (Value = FFont.PixelsPerInch) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.PixelsPerInch := Value;
 DoChanged;
end;

procedure TFontProp.SetSize(const Value: Integer);
begin
 Height := -MulDiv(Value, FFont.PixelsPerInch, 72);
end;

procedure TFontProp.SetStyle(const Value: TFontStyles);
begin
 if (Value = FFont.Style) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FFont.Style := Value;
 DoChanged;
end;

procedure TFontProp.Setup(Canvas: TCanvas; Scale: integer = 100);
begin
 Canvas.Font.Assign(FFont);
 Canvas.Font.Height := ScaleValue(FFontHeight, Scale);
end;

// TPictureProp ///////////////////////////////////////////////////////////////

constructor TPictureProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FEmpty := true;
 FColumns := 1;
 FRows := 1;
 FPicture := TPicture.Create;
 FPicture.OnChange := PictureChange;
 FPropType := ptComplex;
end;

destructor TPictureProp.Destroy;
begin
 FPicture.Free;
 FPictureBuffer.Free;
 FPaintBuffer.Free;
 inherited;
end;

function TPictureProp.GetDisplayValue: string;
begin
 Result := '(Picture)';
end;

function TPictureProp.GetIsLoaded: boolean;
begin
 Result := not FEmpty;
{ if FBuffered
  then Result := not FEmpty
  else Result := Assigned(FPicture.Graphic) and not FPicture.Graphic.Empty; }
end;

function TPictureProp.GetIsRaster: boolean;
begin
 Result := IsClassParent(GraphicClass, TBitmap);
end;

function TPictureProp.GetGraphic: TGraphic;
begin
 Result := FPicture.Graphic;
end;

procedure TPictureProp.SetGraphic(const Value: TGraphic);
begin
 if (Value = FPicture.Graphic) or Owner.IsReadOnly(Self) then exit;
 if FBuffered then raise Exception.Create(SBufferedPictureChangeError);
 DoBeforeChanged;
 FPicture.Graphic := Value;
end;

procedure TPictureProp.PictureChange(Sender: TObject);
begin
 if FDrawing then exit;
 if FBuffered then
  if Assigned(FPicture.Graphic) then begin
   FPicture.Graphic := Nil;
   raise Exception.Create(SBufferedPictureChangeError);
  end else
   exit;
 if not IsRaster then begin
  FColumns := 1;
  FRows := 1;
 end;
 if Assigned(FPicture.Graphic) then with FPicture.Graphic do begin
  FGraphicClass := TGraphicClass(ClassType);
  FEmpty := Empty;
  FWidth := Width;
  FHeight := Height;
  FMasked := Transparent;
  if FPicture.Graphic is TBitmap then
   FMaskColor :=
     ColorToRGB(TBitmap(FPicture.Graphic).TransparentColor) and $FFFFFF;
 end else begin
  FGraphicClass := Nil;
  FEmpty := true;
  FWidth := 0;
  FHeight := 0;
 end;
 if FUsePaintBuffer then RefreshPaintBuffer;
 DoChanged;
end;

function TPictureProp.GetCellSizeRect: TRect;
begin
 if not IsLoaded then
  Result := Rect(0, 0, 0, 0)
 else
 if IsRaster or FUsePaintBuffer
  then Result := Rect(0, 0, Width div FColumns, Height div FRows)
  else Result := Rect(0, 0, Width, Height);
end;

function TPictureProp.GetImgRect(Index: integer): TRect;
var Col, Row: integer;
begin
 if not IsLoaded then begin
  Result := Rect(0, 0, 0, 0);
  exit;
 end;
 Result := Rect(0, 0, Width, Height);
 if IsRaster or FUsePaintBuffer then begin
  Col := Index mod FColumns;
  Row := Index div FColumns;
  if Row >= FRows then exit;
  Result.Right := Result.Right div FColumns;
  Result.Bottom := Result.Bottom div FRows;
  OffsetRect(Result, Col*Result.Right, Row*Result.Bottom);
 end;
end;

procedure TPictureProp.SetColumns(const Value: integer);
begin
 if (Value = FColumns) or (Value < 1) or
   (not IsRaster and not FUsePaintBuffer) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FColumns := Value;
 DoChanged;
end;

procedure TPictureProp.SetRows(const Value: integer);
begin
 if (Value = FRows) or (Value < 1) or
   (not IsRaster and not FUsePaintBuffer) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FRows := Value;
 DoChanged;
end;

procedure TPictureProp.SetMasked(const Value: boolean);
begin
 if (Value = FMasked) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if FBuffered or not IsLoaded then begin
  FMasked := Value;
  DoChanged;
 end else
  FPicture.Graphic.Transparent := Value;
end;

procedure TPictureProp.SetMaskColor(const Value: TColor);
begin
 if (Value = FMaskColor) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if not FBuffered and IsLoaded and (FPicture.Graphic is TBitmap) then
  TBitmap(FPicture.Graphic).TransparentColor := Value
 else begin
  FMaskColor := Value;
  DoChanged;
 end;
end;

procedure TPictureProp.SetLinkName(const Value: string);
begin
 if (Value = FLinkName) or Owner.IsReadOnly(Self) then exit;
 if FBuffered then raise Exception.Create(SBufferedPictureChangeError);
 DoBeforeChanged;
 FLinkName := Value;
 if FLinkName = ''
  then Clear
  else UpdateImageLink;
 DoChanged;
end;

procedure TPictureProp.SetFastBuffer(const Value: boolean);
begin
 if (Value = FUsePaintBuffer) or Owner.IsReadOnly(Self) or
    (Value and FBuffered) then exit;
 DoBeforeChanged;
 FUsePaintBuffer := Value;
 FreeAndNil(FPaintBuffer);
 if FUsePaintBuffer then begin
  FPaintBuffer := TAlphaBuffer.Create;
  RefreshPaintBuffer;
 end;
 DoChanged;
end;

procedure TPictureProp.RefreshPaintBuffer;
begin
 if Assigned(FPaintBuffer) then
  FPaintBuffer.StoreFromGraphic(FPicture.Graphic, PaintBufferStore);
end;

procedure TPictureProp.PaintBufferStore(Sender: TObject; Canvas: TCanvas;
  Graphic: TGraphic);
var R: TRect;
    DrawDefault: boolean;
begin
 if Assigned(FOnPaintFastBuffer) then begin
  DrawDefault := true;
  FOnPaintFastBuffer(Self, FPaintBuffer, Canvas, Graphic, DrawDefault);
  if not DrawDefault then exit;
 end;
 R := Rect(0, 0, Graphic.Width, Graphic.Height);
 Draw(Canvas, R, -1, true);
end;

procedure TPictureProp.SetBuffered(const Value: boolean);
var RProc, WProc: TStreamProc;
    StoredMasked: boolean;
    StoredMaskColor: TColor;
begin
 if Value = FBuffered then exit;
 FBuffered := Value;
 if FBuffered then begin
  // Reset fast buffer
  FastBuffer := false;
  // Store picture in buffer
  if not Assigned(FPictureBuffer)
   then FPictureBuffer := TMemoryStream.Create
   else FPictureBuffer.Clear;
  GetPicReadWrite(FPicture, RProc, WProc);
  WProc(FPictureBuffer);
  // Clear
  FPicture.Graphic := Nil;
 end else begin
  // Restore picture from buffer
  StoredMasked := FMasked;
  StoredMaskColor := FMaskColor;
  GetPicReadWrite(FPicture, RProc, WProc);
  FPictureBuffer.Position := 0;
  RProc(FPictureBuffer);
  if Assigned(FPicture.Graphic) then begin
   // Restore Mask props
   FPicture.Graphic.Transparent := StoredMasked;
   if FPicture.Graphic is TBitmap then
    TBitmap(FPicture.Graphic).TransparentColor := StoredMaskColor;
  end;
  FreeAndNil(FPictureBuffer);
 end;
end;

procedure TPictureProp.Clear(RemoveLink: boolean = false);
begin
 if FBuffered then raise Exception.Create(SBufferedPictureChangeError);
 FPicture.Graphic := Nil;
 if RemoveLink then LinkName := '';
end;

function TPictureProp.UpdateImageLink: boolean;
var LastDir: string;
    PreserveMaskColor: boolean;
    StoredMaskColor: TColor;
begin
 Result := false;
 if FBuffered then raise Exception.Create(SBufferedPictureChangeError);
 if FLinkName = '' then exit;
 // Preserve MaskColor for Bitmaps
 PreserveMaskColor :=
   Assigned(FPicture.Graphic) and (FPicture.Graphic is TBitmap) and
   (TBitmap(FPicture.Graphic).TransparentMode <> tmAuto);
 if PreserveMaskColor 
  then StoredMaskColor := FMaskColor
  else StoredMaskColor := 0; // Avoid compiler warning
 if Assigned(ResolvePictureLink) then begin
  ResolvePictureLink(Self, FLinkName, FPicture);
  Result := true;
 end else begin
  LastDir := GetCurrentDir;
  try
   SetCurrentDir(ExtractFilePath(ParamStr(0)));
   if FileExists(ExpandFilename(FLinkName)) then begin
    FPicture.LoadFromFile(ExpandFilename(FLinkName));
    Result := true;
   end else
    FPicture.Graphic := Nil;
  finally
   SetCurrentDir(LastDir);
  end;
 end;
 // Restore MaskColor if necessary
 if PreserveMaskColor and
    Assigned(FPicture.Graphic) and (FPicture.Graphic is TBitmap) then
  TBitmap(FPicture.Graphic).TransparentColor := StoredMaskColor;
end;

function TPictureProp.GetFrameBitmap(Graphic: TGraphic;
  FrameIndex: integer): TBitmap;
var R: TRect;
begin
 if not Assigned(Graphic) or not (Graphic is TBitmap) then begin
  Result := Nil;
  exit;
 end;
 Result := TBitmap.Create;
 with Result do
 try
  R := ImgRect[FrameIndex];
  Width := R.Right - R.Left;
  Height := R.Bottom - R.Top;
  Canvas.CopyRect(Rect(0, 0, Width, Height), TBitmap(Graphic).Canvas, R);
  Result.Transparent := Graphic.Transparent;
  if Graphic is TBitmap then
   Result.TransparentColor := TBitmap(Graphic).TransparentColor;
 except
  Result.Free;
  raise;
 end;
end;

function TPictureProp.CreatePictureFromBuffer: TPicture;
var RProc, WProc: TStreamProc;
begin
 if not Buffered then begin
  Result := Nil;
  exit;
 end;
 Result := TPicture.Create;
 GetPicReadWrite(Result, RProc, WProc);
 FPictureBuffer.Position := 0;
 RProc(FPictureBuffer);
 if Assigned(Result.Graphic) then begin
  Result.Graphic.Transparent := FMasked;
  if Result.Graphic is TBitmap then
   TBitmap(Result.Graphic).TransparentColor := FMaskColor;
 end;
end;

procedure TPictureProp.LoadFromStream(Stream: TStream);
var ReadProc, WriteProc: TStreamProc;
begin
 GetPicReadWrite(FPicture, ReadProc, WriteProc);
 ReadProc(Stream);
end;

procedure TPictureProp.SaveToStream(Stream: TStream);
var ReadProc, WriteProc: TStreamProc;
begin
 GetPicReadWrite(FPicture, ReadProc, WriteProc);
 WriteProc(Stream);
end;

procedure TPictureProp.LoadFromFile(const FileName: string);
begin
 FPicture.LoadFromFile(FileName);
end;

procedure TPictureProp.SaveToFile(const FileName: string);
begin
 FPicture.SaveToFile(FileName);
end;

procedure TPictureProp.Draw(Canvas: TCanvas; var R: TRect; FrameIndex: integer;
  ClipTransparent: boolean = false; DestBuffer: TAlphaBuffer = Nil);
var PrevRgn, ClipRgn: HRGN;
    PicGraphic, Graphic: TGraphic;
    BufPicture: TPicture;
    NeedFree: boolean;
    FrameRect: TRect;
begin
 if FUsePaintBuffer and Assigned(FPaintBuffer) and Assigned(DestBuffer) then begin
  if ((FColumns > 1) or (FRows > 1)) and (FrameIndex >= 0) 
   then FrameRect := ImgRect[FrameIndex]
   else FrameRect := Rect(0, 0, FWidth, FHeight);
  FPaintBuffer.PaintStored(DestBuffer, R, FrameRect);
 end else begin
  NeedFree := false;
  Graphic := Nil;
  PrevRgn := 0;
  BufPicture := Nil;
  ClipRgn := CreateRectRgnIndirect(R);
  try
   if Buffered then begin
    BufPicture := CreatePictureFromBuffer;
    PicGraphic := BufPicture.Graphic;
   end else
    PicGraphic := FPicture.Graphic;
   if ((FColumns > 1) or (FRows > 1)) and (FrameIndex >= 0) then begin
    Graphic := GetFrameBitmap(PicGraphic, FrameIndex);
    NeedFree := true;
   end else
    Graphic := PicGraphic;
   if not Assigned(Graphic) then exit;
   if ClipTransparent and Graphic.Transparent and (Graphic is TBitmap) then
    ExcludeBitmapTransparency(Graphic as TBitmap, R, ClipRgn);
   PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
   FDrawing := True;
   Canvas.StretchDraw(R, Graphic);
  finally
   FDrawing := False;
   if Assigned(Graphic) then SelectClipRgn(Canvas.Handle, PrevRgn);
   DeleteObject(PrevRgn);
   DeleteObject(ClipRgn);
   if NeedFree then Graphic.Free;
   BufPicture.Free;
  end;
 end;
end;

function TPictureProp.GetPropType(const PropName: string): TPropType;
begin
 if CompareText(PropName, 'Graphic') = 0
  then Result := ptHexData
  else Result := ptSimple;
end;

function TPictureProp.GetPropValue(const PropName: string): Variant;
var HexStream: TMemoryStream;
    VarArray: Pointer;
    Count: integer;
    RProc, WProc: TStreamProc;
begin
 if CompareText(PropName, 'Graphic') = 0 then begin
  if not IsLoaded or (FLinkName <> '') then begin
   VarClear(Result);
   exit;
  end;
  HexStream := TMemoryStream.Create;
  try
   if FBuffered then begin
    // Read data from FPictureBuffer
    HexStream.Write(FPictureBuffer.Memory^, FPictureBuffer.Size);
   end else begin
    // Read data from FPicture
    GetPicReadWrite(FPicture, RProc, WProc);
    WProc(HexStream);
   end;
   Count := HexStream.Size;
   Result := VarArrayCreate([0, Count-1], varByte);
   VarArray := VarArrayLock(Result);
   try
    Move(HexStream.Memory^, VarArray^, Count);
   finally
    VarArrayUnlock(Result);
   end;
  finally
   HexStream.Free;
  end;
 end else
  Result := inherited GetPropValue(PropName);
end;

procedure TPictureProp.SetPropValue(const PropName: string; Value: Variant);
var VArray: Pointer;
    HexStream: TMemoryStream;
    Count: integer;
    RProc, WProc: TStreamProc;
    IsBuffered: boolean;
begin
 if CompareText(PropName, 'Graphic') = 0 then begin
  if FLinkName <> '' then exit;
  if VarIsEmpty(Value) then
   FPicture.Graphic := Nil
  else
  if (VarType(Value) and varArray) <> 0 then begin
   HexStream := Nil;
   IsBuffered := FBuffered;
   Count := VarArrayHighBound(Value, 1) + 1;
   VArray := VarArrayLock(Value);
   try
    if IsBuffered then Buffered := false;
    HexStream := TMemoryStream.Create;
    HexStream.Write(VArray^, Count);
    HexStream.Position := 0;
    GetPicReadWrite(FPicture, RProc, WProc);
    RProc(HexStream);
   finally
    VarArrayUnlock(Value);
    HexStream.Free;
    if IsBuffered then Buffered := true;
   end;
  end;
 end else
  inherited;
end;

function TPictureProp.StoreGraphic: Boolean;
begin
 Result := GetIsLoaded;
end;

function TPictureProp.StoreLinkName: Boolean;
begin
 Result := FLinkName <> '';
end;

// TBackgroundOptionsProp /////////////////////////////////////////////////////

constructor TBackgroundOptionsProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FPropType := ptComplex;
 // defaults
 FPictureOptions := [bpStretchHoriz, bpStretchVert, bpScaledSize];
 FPictureEnabled := True;
 FBrushEnabled := True;
end;

function TBackgroundOptionsProp.GetDisplayValue: string;
begin
 Result := '(BackgroundOptions)';
end;

function TBackgroundOptionsProp.GetCenter: Boolean;
begin
 Result :=
   (bpCenterHoriz in FPictureOptions) and (bpCenterVert in FPictureOptions);
end;

function TBackgroundOptionsProp.GetStretch: Boolean;
begin
 Result :=
   (bpStretchHoriz in FPictureOptions) and (bpStretchVert in FPictureOptions);
end;

procedure TBackgroundOptionsProp.SetPictureOptions(
  const Value: TBackgroundPictureOptions);
begin
 if (Value = FPictureOptions) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FPictureOptions := Value;
 DoChanged;
end;

procedure TBackgroundOptionsProp.SetBrushEnabled(const Value: boolean);
begin
 if (Value = FBrushEnabled) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FBrushEnabled := Value;
 DoChanged;
end;

procedure TBackgroundOptionsProp.SetPictureEnabled(const Value: boolean);
begin
 if (Value = FPictureEnabled) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FPictureEnabled := Value;
 DoChanged;
end;

procedure TBackgroundOptionsProp.SetTop(Value: Integer);
begin
 if (Value = FTop) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FTop := Value;
 DoChanged;
end;

procedure TBackgroundOptionsProp.SetLeft(Value: Integer);
begin
 if (Value = FLeft) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FLeft := Value;
 DoChanged;
end;

procedure TBackgroundOptionsProp.SetWidth(Value: Integer);
begin
 if (Value = FWidth) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FWidth := Value;
 DoChanged;
end;

procedure TBackgroundOptionsProp.SetHeight(Value: Integer);
begin
 if (Value = FHeight) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 FHeight := Value;
 DoChanged;
end;

procedure TBackgroundOptionsProp.SetStretch(Value: Boolean);
begin
 if Value
  then PictureOptions := PictureOptions + [bpStretchHoriz, bpStretchVert]
  else PictureOptions := PictureOptions - [bpStretchHoriz, bpStretchVert];
end;

procedure TBackgroundOptionsProp.SetCenter(Value: Boolean);
begin
 if Value
  then PictureOptions := PictureOptions + [bpCenterHoriz, bpCenterVert]
  else PictureOptions := PictureOptions - [bpCenterHoriz, bpCenterVert];
end;

function TBackgroundOptionsProp.GetPicturePaintRect(Picture: TPictureProp;
  const BackgroundRect: TRect; Scale: integer): TRect;
begin
 if not Assigned(Picture) or not Picture.IsLoaded then
  SetRectEmpty(Result)
 else
 with BackgroundRect do begin
  Result := Rect(0, 0, Picture.Width, Picture.Height);
  // Adjust width
  if bpStretchHoriz in FPictureOptions then
   Result.Right := Right - Left
  else begin
   if bpNewWidth in FPictureOptions then
    Result.Right := ScaleValue(FWidth, Scale);
   if bpScaledSize in FPictureOptions then
    Result.Right := MulDiv(Result.Right, Scale, 100);
  end;
  // Adjust height
  if bpStretchVert in FPictureOptions then
   Result.Bottom := Bottom - Top
  else begin
   if bpNewHeight in FPictureOptions then
    Result.Bottom := ScaleValue(FHeight, Scale);
   if bpScaledSize in FPictureOptions then
    Result.Bottom := MulDiv(Result.Bottom, Scale, 100);
  end;  
  // Align horizontally
  if bpCenterHoriz in FPictureOptions then
   OffsetRect(Result, (Right - Left - Result.Right) div 2, 0)
  else
  if bpAlignRight in FPictureOptions then
   OffsetRect(Result, Right - Left - Result.Right, 0);
  // Align vertically
  if bpCenterVert in FPictureOptions then
   OffsetRect(Result, 0, (Bottom - Top - Result.Bottom) div 2)
  else
  if bpAlignBottom in FPictureOptions then
   OffsetRect(Result, 0, Bottom - Top - Result.Bottom);
  // Offset left, top
  if (bpOffsetLeft in FPictureOptions) and
     not (bpStretchHoriz in FPictureOptions) then
   OffsetRect(Result, ScaleValue(FLeft, Scale), 0);
  if (bpOffsetTop in FPictureOptions) and
     not (bpStretchVert in FPictureOptions) then
   OffsetRect(Result, 0, ScaleValue(FTop, Scale));
  // Get in background coordinate system
  OffsetRect(Result, Left, Top);
 end;
end;

procedure TBackgroundOptionsProp.Draw(Canvas: TCanvas; var R: TRect;
  Picture: TPictureProp; Brush: TBrushProp; Scale: integer;
  const PanelRefreshRect: TRect; ClipTransparent: boolean = false);
var PrevRgn, ClipRgn: HRGN;
    PicRect: TRect;
begin
 // Brush filling
 if FBrushEnabled and Assigned(Brush) then begin
  Brush.Setup(Canvas, Scale);
  Canvas.Pen.Style := psClear;
  Canvas.Pen.Mode := pmCopy;
  if Brush.IsPaintAlternate then begin
   PrevRgn := 0;
   ClipRgn := CreateRectRgnIndirect(R);
   try
    PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
    Brush.PaintAlternate(Canvas, R, PanelRefreshRect, Scale, ClipTransparent);
   finally
    SelectClipRgn(Canvas.Handle, PrevRgn);
    DeleteObject(ClipRgn);
    DeleteObject(PrevRgn);
   end;
  end else
   Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
 end;
 // Picture drawing
 if FPictureEnabled and Assigned(Picture) and Picture.IsLoaded then begin
  PicRect := GetPicturePaintRect(Picture, R, Scale);
  Picture.Draw(Canvas, PicRect, 0, ClipTransparent);
 end;
end;

{ TPropNameList }

procedure TPropNameList.FlexSort(compare: TFlexStringListSortCompare);
begin
  if not Sorted and (Count > 1) then
  begin
    Changing;
    FlexQuickSort(0, Count - 1, Compare);
    Changed;
  end;
end;

procedure TPropNameList.FlexQuickSort(L, R: Integer; SCompare: TFlexStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
          Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then FlexQuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

{ TControlObjectProp }

function TControlObjectProp.GetDisplayValue: string;
var obj: TGMObject;
begin
  Result := '';
  obj := GMObjects.ObjByID(Value);
  if obj <> nil then
    Result := obj.Name;
end;

initialization
  THistory.RegisterAction(TPropHistoryAction);
  THistory.RegisterAction(TPropHistoryGroup);

finalization
  ClearRegEditForms;

end.
