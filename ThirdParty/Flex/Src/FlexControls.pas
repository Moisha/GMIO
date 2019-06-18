/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    Common controls classes                          //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexControls;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes, Graphics, Controls, StdCtrls, SysUtils, 
  {$IFDEF FG_D6} Variants, {$ENDIF}
  FlexBase, FlexProps, FlexUtils, FlexPath, FlexHistory, FlexActions,
  UITypes;

type
  TFlexControlWithPenAndBrush = class(TFlexControl)
  private
   FBrushProp: TBrushProp;
   FPenProp: TPenProp;
  protected
   procedure CreateProperties; override;
  public
   property  BrushProp: TBrushProp read FBrushProp;
   property  PenProp: TPenProp read FPenProp;
  end;

  TFlexBox = class(TFlexControlWithPenAndBrush)
  private
   FAlwaysFilled: boolean;
   FRoundnessProp: TIntProp;
  protected
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   function  CreateCurveControl: TFlexControl; override;
   function  CreateBoxRegion(const PaintRect: TRect;
     Inflate: boolean = false): HRGN;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   function  GetAnchorPoint: TPoint; override;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   function  GetDefaultLinkPoint(Index: integer): TPoint; override;
   function  GetDefaultLinkPointCount: integer; override;
  public
   class function CursorInCreate: TCursor; override;
   function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   property  RoundnessProp: TIntProp read FRoundnessProp;
  end;

  TFlexEllipse = class(TFlexControlWithPenAndBrush)
  private
   function  GetCeneter: TPoint;
  protected
   FBeginAngleProp: TIntProp;
   FEndAngleProp: TIntProp;
   FArcAxesProp: TStrProp;
   FPoints: TPointArray;
   FEditing: boolean;
   FPointTypes: TPointTypeArray;
   FPathInfo: TPathInfo;
   FUseAxes: boolean;
   FArcOldFormatNeedCheck: boolean;
   FArcOldFormatNeedReset: boolean;
   FArcOldFormat: boolean;
   FArcOfs: TPoint;
   FArcSize: TPoint;
   FArcResizing: boolean;
   FArcResizingBounds: TRect;
   function  CreateEllipseRegion(const PaintRect: TRect): HRGN;
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   procedure MirrorInResize(HMirror, VMirror: boolean); override;
   function  CreateCurveControl: TFlexControl; override;
   function  GetPoint(Index: integer): TPoint; override;
   function  GetPointCount: integer; override;
   function  GetPointsInfo: PPathInfo; override;
   procedure SetPoint(Index: integer; const Value: TPoint); override;
   procedure MakeArc(DC: HDC; const R: TRect; const Pt: TPointArray);
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
   function  GetAnchorPoint: TPoint; override;
   function  GetDefaultLinkPoint(Index: integer): TPoint; override;
   function  GetDefaultLinkPointCount: integer; override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropHistoryAction(Sender: TObject; Prop: TCustomProp;
     var ActionClass: THistoryActionClass); override;
   procedure DoNotify(Notify: TFlexNotify); override;
   procedure StartResizing(const SelRect: TRect); override;
   procedure SetPointsByAngles;
   procedure SetAnglesByPoints;
   function  GetIsPie: boolean;
   function  IsPropUpdatePoints(Prop: TCustomProp): boolean; virtual;
   function  GetIsNeedHistoryPointsAction: boolean;
   procedure UpdateArcBoundsByPoints; virtual;
   procedure SetUseAxes(Value: boolean); virtual;
   procedure GetArcAxes(Sender: TObject; out s: string); virtual;
   procedure SetArcAxes(const s: string); virtual;
   property  UseAxes: boolean read FUseAxes write SetUseAxes;
  public
   class function CursorInCreate: TCursor; override;
   function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   function  EditPoints(Func: TPathEditFunc; const Selected: TSelectedArray;
     Params: PPathEditParams = Nil): boolean; override;
   function  EditPointsCaps(const Selected: TSelectedArray): TPathEditFuncs;
     override;
   property  Center: TPoint read GetCeneter;
   property  IsPie: boolean read GetIsPie;
   property  IsNeedHistoryPointsAction: boolean
     read GetIsNeedHistoryPointsAction;
   property  BeginAngleProp: TIntProp read FBeginAngleProp;
   property  EndAngleProp: TIntProp read FEndAngleProp;
  end;

  TFlexArc = class(TFlexEllipse);

  TFlexPicture = class(TFlexControl)
  private
   FAutoSizeProp: TBoolProp;
   FPictureProp: TPictureProp;
   FFrameIndexProp: TIntProp;
  protected
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   procedure PaintAll(Canvas: TCanvas; PaintX, PaintY: integer); override;
   function  GetDefaultLinkPoint(Index: integer): TPoint; override;
   function  GetDefaultLinkPointCount: integer; override;
  public
   class function CursorInCreate: TCursor; override;
   function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   property  PictureProp: TPictureProp read FPictureProp;
   property  FrameIndexProp: TIntProp read FFrameIndexProp;
   property  AutoSizeProp: TBoolProp read FAutoSizeProp;
  end;

  TFlexCurve = class(TFlexControlWithPenAndBrush)
  private
   FBeginCapProp: TLineCapProp;
   FEndCapProp: TLineCapProp;
   FPointsProp: TDataProp;
   FPathPointsProp: TDataProp; // Points in new format (coords + types)
   FIsSolidProp: TBoolProp;
  protected
   FPoints: TPointArray;
   FZeroPoints: boolean;
   FPointTypes: TPointTypeArray;
   FChanging: boolean;
   FCurveInfo: TPathInfo;
   FCurveInfoChanged: boolean;
   FResizePoints: TPointArray;
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlDestroy; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   procedure CreateInDesign(var Info: TFlexCreateInDesignInfo); override;
   procedure MirrorInResize(HMirror, VMirror: boolean); override;
   function  MovePathPoints(PointIndex: integer; var Delta: TPoint;
    Selected: TSelectedArray; Smooth: boolean = false;
    Symmetric: boolean = false): boolean; override;
   function  MovePathSegment(FirstIndex, NextIndex: integer;
     var Delta: TPoint; const SegmentCurvePos: double): boolean; override;
   function  RecordPointsAction: TPointsHistoryAction; virtual;
   function  GetPoint(Index: integer): TPoint; override;
   function  GetPointCount: integer; override;
   procedure SetPoint(Index: integer; const Value: TPoint); override;
   function  GetPointType(Index: integer): TPointType; override;
   procedure SetPointType(Index: integer; const Value: TPointType); override;
   procedure PointsChanged; virtual;
   function  GetAnchorPoint: TPoint; override;
   procedure StartResizing(const SelRect: TRect); override;
   procedure FinishResizing; override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   procedure DoNotify(Notify: TFlexNotify); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   procedure GetPointsData(Sender: TObject; var Value: Variant);
   procedure SetPointsData(Sender: TObject; var Value: Variant);
   procedure GetPathPointsData(Sender: TObject; var Value: Variant);
   procedure SetPathPointsData(Sender: TObject; var Value: Variant);
   function  GetIsPointsClosed: boolean; override;
   procedure SetIsPointsClosed(Value: boolean); override;
   //procedure SetDocRect(Value: TRect); override;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
   function  InternalInsertPoints(Index, Count: integer): integer;
   procedure InternalDeletePoints(Index, Count: integer);
   function  GetPointsInfo: PPathInfo; override;
   function  GetDefaultLinkPoint(Index: integer): TPoint; override;
   function  GetDefaultLinkPointCount: integer; override;
  public
   function  EndUpdate: boolean; override;
   class function CursorInCreate: TCursor; override;
   function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   function  InsertPoint(Index: integer; const Point: TPoint): integer; override;
   function  InsertNearestPoint(const Point: TPoint): integer; override;
   function  InsertCurvePoints(Index: integer; const Point,
     CtrlPointA, CtrlPointB: TPoint): integer; override;
   procedure DeletePoint(Index: integer); override;
   procedure SetPointsEx(const APoints: TPointArray;
     const ATypes: TPointTypeArray); override;
   procedure GetPointsEx(out APoints: TPointArray;
     out ATypes: TPointTypeArray); override;
   function  FlattenPoints(const Curvature: single): boolean; override;
   function  FindNearestPoint(const Point: TPoint;
     var Nearest: TNearestPoint): boolean; override;
   function  FindNearestPathSegment(const Point: TPoint; var FirstIndex,
     NextIndex: integer; Nearest: PNearestPoint = Nil;
     ForInsert: boolean = true; ForSelect: boolean = false): boolean; override;
   function  EditPoints(Func: TPathEditFunc; const Selected: TSelectedArray;
     Params: PPathEditParams = Nil): boolean; override;
   function  EditPointsCaps(const Selected: TSelectedArray): TPathEditFuncs;
     override;
   property  BeginCapProp: TLineCapProp read FBeginCapProp;
   property  EndCapProp: TLineCapProp read FEndCapProp;
   property  IsSolidProp: TBoolProp read FIsSolidProp;
  end;

  TTextCharWidth = array[0..255] of integer;
  TTextCharABCs = array[0..255] of TABC;

  PTextWordInfo = ^TTextWordInfo;
  TTextWordInfo = record
   WordBegin, WordEnd: integer;
   Size: integer;
  end;

  PTextLine = ^TTextLine;
  TTextLine = record
   Text: PChar;
   Length: integer;
   Pos: integer;
  end;

  TTextWordInfoArray = array of TTextWordInfo;

  TTextFormator = class
  private
   FLogFont: TLogFont;
   FLogFontValid: boolean;
   FCharWidth: TTextCharWidth;
   FCharABC: TTextCharABCs;
   FEMSquare: integer;
   FPixelSize: double;
   FHeight: integer;
   FLineSpace: integer;
   // Current line
   FDC: HDC;
   function  GetCharABC(AChar: Char): TABC;
   function  GetCharWidth(AChar: Char): integer;
  protected
   FLineSize: TSize;
   FLineLeftSpace: integer;
   FLineRightSpace: integer;
   function  CheckFontIdentical(DC: HDC): boolean;
   function  SkipWhite(var Line: TTextLine; var NeedBreak: boolean): boolean;
   procedure GetWord(var Line: TTextLine);
   function  GetLine(var Line: TTextLine; LineWidth: integer;
     var LineBegin, LineEnd: integer; WordWrap: boolean): boolean;
   procedure LineExtent(ALine: PChar; ACount: integer);
  public
   function  Setup(DC: HDC; const PixelSize: double;
     DivideOnEMSquare: boolean = false; RoundHeight: boolean = true): boolean;
   function  CharExtent(Text: PChar; CharCount: integer;
     LineBreaks: boolean = false; WordWrap: boolean = false;
     PaintWidth: integer = 0; InLogicalUnits: boolean = false;
     LineCount: PInteger = Nil): TSize;
   procedure CharWordInfos(Text: PChar; CharCount: integer;
     var WordInfos: TTextWordInfoArray; var WordCount, WordWidth: integer;
     InLogicalUnits: boolean = false);
   function  TextExtent(const Text: string): TSize;
   function  TextHeight(const Text: string): integer;
   function  TextWidth(const Text: string): integer;
   procedure TextOut(X, Y: Integer; const Text: string);
   procedure CharOut(X, Y: Integer; Text: PChar; CharCount: integer);
   procedure TextRect(Rect: TRect; const Text: string; WordWrap: boolean;
     Align: TAlignment = taLeftJustify; ALayout: TTextLayout = tlTop;
     WidthJustify: boolean = false; LogicalWidth: integer = 0;
     Rotation: integer = 0);
   property  Height: integer read FHeight;
   property  LineSpace: integer read FLineSpace;
   property  EMSquare: integer read FEMSquare;
   property  PixelSize: double read FPixelSize;
   property  CharABC[AChar: Char]: TABC read GetCharABC;
   property  CharWidth[AChar: Char]: integer read GetCharWidth;
  end;

  TFlexText = class(TFlexBox)
  private
   FAutoSizeProp: TBoolProp;
   FTextProp: TStrListProp;
   FFontProp: TFontProp;
   FWordWrapProp: TBoolProp;
   FGrayedProp: TBoolProp;
   FAlignmentProp: TEnumProp;
   FLayoutProp: TEnumProp;
   FAngleProp: TIntProp;
   FPreciseProp: TBoolProp;
   FPreciseJustifyProp: TBoolProp;
   FAutoScaleFontSizeProp: TBoolProp;
   FMaxFontSizeProp: TIntProp;
   FAutoSizeChanging: boolean;
   function  GetAlignment: TAlignment;
   function  GetLayout: TTextLayout;
   function  GetWordWrap: boolean;
   procedure SetAlignment(const Value: TAlignment);
   procedure SetLayout(const Value: TTextLayout);
   procedure SetWordWrap(const Value: boolean);
   function  GetGrayed: boolean;
   procedure SetGrayed(const Value: boolean);
   function  GetTextSize: TSize;
  protected
   FFormator: TTextFormator;
   FRefreshRect: TRect;
   FRefreshScale: integer;
   procedure GetLeftRightExtra(DC: HDC; var Left, Right: integer);
   procedure DoDrawText(Canvas: TCanvas; var Rect: TRect; Flags: Longint;
     const Text: string);
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlDestroy; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   function  CreateCurveControl: TFlexControl; override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
   procedure AutoSizeChanged;
   property  TextSize: TSize read GetTextSize;
  public
   class function CursorInCreate: TCursor; override;
   procedure DrawTextEx(Canvas: TCanvas; var R: TRect; CalcOnly, Scaled: boolean;
      const AText: string; AAlignment: TAlignment; ALayout: TTextLayout;
      AWordWrap: boolean; APrecise, APreciseJustify: boolean;
      LogSize: integer = 0; AFontHeight: integer = 0);
   procedure {$IFDEF FG_CBUILDER}DrawTextCpp{$ELSE}DrawText{$ENDIF}(
     Canvas: TCanvas; var R: TRect; CalcOnly, Scaled: boolean); virtual;
   //function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   property  Formator: TTextFormator read FFormator;
   property  AutoSizeProp: TBoolProp read FAutoSizeProp;
   property  TextProp: TStrListProp read FTextProp;
   property  FontProp: TFontProp read FFontProp;
   property  WordWrapProp: TBoolProp read FWordWrapProp;
   property  AlignmentProp: TEnumProp read FAlignmentProp;
   property  LayoutProp: TEnumProp read FLayoutProp;
   property  AngleProp: TIntProp read FAngleProp;
   property  PreciseProp: TBoolProp read FPreciseProp;
   property  PreciseJustifyProp: TBoolProp read FPreciseJustifyProp;
   property  AutoScaleFontSizeProp: TBoolProp read FAutoScaleFontSizeProp;
   property  MaxFontSizeProp: TIntProp read FMaxFontSizeProp;
   property  Grayed: boolean read GetGrayed write SetGrayed;
   property  WordWrap: boolean read GetWordWrap write SetWordWrap;
   property  Alignment: TAlignment read GetAlignment write SetAlignment;
   property  Layout: TTextLayout read GetLayout write SetLayout;
  end;

  TFlexConnector = class(TFlexCurve)
  private
   FLinkAProp: TLinkPointProp;
   FLinkBProp: TLinkPointProp;
   FRerouteModeProp: TEnumProp;
   FOrtogonalProp: TBoolProp;
   FMinimalGapProp: TIntProp;
   FBlocked: boolean;
   FLinkUpdateCount: integer;
   FLinkPointsIniting: boolean;
   procedure SetBlocked(Value: boolean);
   function  GetLinked: boolean;
   function  GetOrtogonal: boolean;
   procedure SetOrtogonal(const Value: boolean);
   function  GetRerouteMode: TRerouteMode;
   procedure SetRerouteMode(const Value: TRerouteMode);
  protected
   FDesignMoving: boolean;
   FDesignMovedFirst: integer;
   FDesignMovedNext: integer;
   FInTransformation: boolean;
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlDestroy; override;
   procedure CreateInDesign(var Info: TFlexCreateInDesignInfo); override;
   procedure BeginSelectionTransformation; override;
   procedure EndSelectionTransformation; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   procedure CreateCurveGuide(const NewPoint: TPoint;
     var Guide: TFlexEditPointGuide); override;
   procedure SetDocRect(Value: TRect); override;
   procedure MirrorInResize(HMirror, VMirror: boolean); override;
   function  MovePathPoints(PointIndex: integer; var Delta: TPoint;
    Selected: TSelectedArray; Smooth: boolean = false;
    Symmetric: boolean = false): boolean; override;
   function  MovePathSegment(FirstIndex, NextIndex: integer;
     var Delta: TPoint; const SegmentCurvePos: double): boolean; override;
   procedure PointsChanged; override;
   procedure DoNotify(Notify: TFlexNotify); override;
   procedure LinkedNotify(Sender: TObject; Source: TNotifyLink;
     const Info: TNotifyLinkInfo);
   procedure GetLinkProps(var LinkFirst, LinkLast: TLinkPointProp); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   procedure ConnectorMinGapChanged; override;
   procedure Reroute;
   property  Blocked: boolean read FBlocked write SetBlocked;
   property  LinkUpdateCount: integer read FLinkUpdateCount;
   property  Linked: boolean read GetLinked;
  public
   class function IsConnectorControl: boolean; override;
   function  InsertPoint(Index: integer;
     const Point: TPoint): integer; override;
   function  InsertNearestPoint(const Point: TPoint): integer; override;
   procedure DeletePoint(Index: integer); override;
   procedure EndPointsDesigning; override;
   procedure BeginLinkUpdate;
   procedure EndLinkUpdate;
   property  LinkAProp: TLinkPointProp read FLinkAProp;
   property  LinkBProp: TLinkPointProp read FLinkBProp;
   property  RerouteModeProp: TEnumProp read FRerouteModeProp;
   property  OrtogonalProp: TBoolProp read FOrtogonalProp;
   property  MinimalGapProp: TIntProp read FMinimalGapProp;
   property  RerouteMode: TRerouteMode read GetRerouteMode write SetRerouteMode;
   property  Ortogonal: boolean read GetOrtogonal write SetOrtogonal;
  end;

  TFlexRegularPolygon = class(TFlexControlWithPenAndBrush)
  private
   FAngleProp: TIntProp;
   FSidesProp: TIntProp;
  protected
   FEtalon: array of record X, Y: double; end;
   FEtalonDX: double;
   FEtalonDY: double;
   FEtalonDXDY: double;
   FPoints: TPointArray;
   FPointTypes: TPointTypeArray;
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   function  CreateCurveControl: TFlexControl; override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   function  GetAnchorPoint: TPoint; override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   function  CreatePolygonRegion(const PaintRect: TRect; Inflate: boolean = false): HRGN;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
   function  GetDefaultLinkPoint(Index: integer): TPoint; override;
   function  GetDefaultLinkPointCount: integer; override;
   procedure RebuildEtalon;
   procedure BuildPoints(var Points: TPointArray; const R: TRect);
  public
   function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   property  AngleProp: TIntProp read FAngleProp;
   property  SidesProp: TIntProp read FSidesProp;
  end;

  PLineCapInfo = ^TLineCapInfo;
  TLineCapInfo = record
   Size: integer;
   LineLength: integer;
   Bounds: TRect;
  end;

  PLineCapData = ^TLineCapData;
  TLineCapData = record
   Info: TLineCapInfo;
   p0: TPoint;
   p1: TPoint;
   Points: TPointArray;
   PointTypes: TPointTypeArray;
  end;

  PRenderCapParams = ^TRenderCapParams;
  TRenderCapParams = record
   Style: integer;
   OutlineColor: TColor;
   FillColor: TColor;
   CapSize: integer;
  end;

  TLineCapStyleResolve = procedure(LineCap: integer; const p0, p1: TPoint;
    var Data: TLineCapData; CapSize: integer; var Resolved: boolean);

var
  ResolveLineCapStyle: TLineCapStyleResolve;

function  GetLineCapInfo(LineCap: integer; var Info: TLineCapInfo;
  CapSize: integer): boolean;

function  GetLineCapData(LineCap: integer; const p0, p1: TPoint;
  var Data: TLineCapData; CapSize: integer): boolean;

procedure RenderCap(DC: HDC; LineCap: integer; const p0, p1: TPoint;
  CapSize: integer);

procedure RenderCaps(DC: HDC; PenWidth: integer;
  const BeginCap, EndCap: TRenderCapParams; const Points: TPointArray;
  const PointTypes: TPointTypeArray; Info: PPathInfo = Nil);

procedure RegisterStdControls;

implementation

uses
  {$IFDEF FG_D12} Types, {$ENDIF} Math;

const
  BezierCircleCoeff = 0.55228474983;

function GetLineCapInfo(LineCap: integer; var Info: TLineCapInfo;
  CapSize: integer): boolean;
var Half: integer;
    Data: TLineCapData;
begin
 Result := true;
 with Info do begin
  Size := CapSize;
  Half := Size div 2;
  case LineCap of
   psArrow0:
     begin
      LineLength := 0;
      Bounds.Left := -Half;
      Bounds.Top := -Half;
      Bounds.Right := 0;
      Bounds.Bottom := Half;
     end;
   psDblArrow0:
     begin
      LineLength := 0;
      Bounds.Left := -Size;
      Bounds.Top := -Half;
      Bounds.Right := 0;
      Bounds.Bottom := Half;
     end;
   psBackArrow0:
     begin
      LineLength := 0;
      Bounds.Left := 0;
      Bounds.Top := -Half;
      Bounds.Right := Half;
      Bounds.Bottom := Half;
     end;
   psBackDblArrow0:
     begin
      LineLength := 0;
      Bounds.Left := -Half;
      Bounds.Top := -Half;
      Bounds.Right := Half;
      Bounds.Bottom := Half;
     end;
   psArrow1:
     begin
      LineLength := 0;
      Bounds.Left := -Size;
      Bounds.Top := -Half;
      Bounds.Right := 0;
      Bounds.Bottom := Half;
     end;
   psDblArrow1:
     begin
      LineLength := 0;
      Bounds.Left := -Size -Half;
      Bounds.Top := -Half;
      Bounds.Right := 0;
      Bounds.Bottom := Half;
     end;
   psBackArrow1:
     begin
      LineLength := 0;
      Bounds.Left := 0;
      Bounds.Top := -Half;
      Bounds.Right := Size;
      Bounds.Bottom := Half;
     end;
   psBackDblArrow1:
     begin
      LineLength := 0;
      Bounds.Left := -Half;
      Bounds.Top := -Half;
      Bounds.Right := Size;
      Bounds.Bottom := Half;
     end;
   psArrow:
     begin
      LineLength := Size; // ????
      Bounds.Left := -Size;
      Bounds.Top := -Half;
      Bounds.Right := 0;
      Bounds.Bottom := Half;
     end;
   psTriangle,
   psDblTriangle,
   psBackTriangle,
   psBackDblTriangle:
     begin
      LineLength := Size;
      Bounds.Left := -Size;
      Bounds.Top := -Half;
      Bounds.Right := 0;
      Bounds.Bottom := Half;
     end;
   psSquare,
   psCircle,
   psRhombus:
     begin
      LineLength := Half;
      Bounds.Left := -Half;
      Bounds.Top := -Half;
      Bounds.Right := Half;
      Bounds.Bottom := Half;
     end;
   psDotCircle:
     begin
      Half := Size div 4;
      LineLength := Half;
      Bounds.Left := -Half;
      Bounds.Top := -Half;
      Bounds.Right := Half;
      Bounds.Bottom := Half;
     end;
   psThinRect:
     begin
      LineLength := Size;
      Bounds.Left := -Size;
      Bounds.Top := -Half;
      Bounds.Right := 0;
      Bounds.Bottom := Half;
     end;
   else
     begin
      // Unknown pen cap
      LineLength := 0;
      Bounds.Left := 0;
      Bounds.Top := 0;
      Bounds.Right := 0;
      Bounds.Bottom := 0;
      Result := false;
     end;
  end;
 end;
 if Assigned(ResolveLineCapStyle) then begin
  ResolveLineCapStyle(LineCap, Point(0, 0), Point(0, 0), Data, CapSize, Result);
  if Result then Info := Data.Info;
 end; 
end;

function GetLineCapData(LineCap: integer; const p0, p1: TPoint;
  var Data: TLineCapData; CapSize: integer): boolean;
var Coeff: double;
    LineDist, DistX, DistY: double;
    Temp1, Temp2, Temp3, Temp4: TPoint;
    TempDist, TempDist1, TempDist2: double;
    a, b, c, d: double;

 procedure SetCount(ACount: integer);
 var i: integer;
 begin
  SetLength(Data.Points, ACount);
  SetLength(Data.PointTypes, ACount);
  for i:=0 to ACount-2 do Data.PointTypes[i] := ptNode;
  Data.PointTypes[ACount-1] := ptEndNode;
  //Count := ACount;
 end;

 procedure GetPointOnLine(const Dist: double; var p: TPoint);
 var Coeff: double;
 begin
  if LineDist <> 0 then begin
   Coeff := Dist / LineDist;
   p.x := p1.x - Round(DistX * Coeff);
   p.y := p1.y - Round(DistY * Coeff);
  end else
   p := p1;
 end;

 procedure SetArrow0(const p0, p1: TPoint; Index: integer);
 begin
  Data.Points[Index+0] := p1;
  Data.Points[Index+1].x := p0.x - (p0.y - p1.y);
  Data.Points[Index+1].y := p0.y - (p1.x - p0.x);
  Data.PointTypes[Index+1] := ptEndNode;
  Data.Points[Index+2] := p1;
  Data.Points[Index+3].x := p0.x + (p0.y - p1.y);
  Data.Points[Index+3].y := p0.y + (p1.x - p0.x);
  Data.PointTypes[Index+3] := ptEndNode;
 end;

 procedure SetArrow1(const p0, p1: TPoint; Index: integer);
 begin
  Data.Points[Index+0] := p1;
  Data.Points[Index+1].x := p0.x - (p0.y - p1.y) div 2;
  Data.Points[Index+1].y := p0.y - (p1.x - p0.x) div 2;
  Data.PointTypes[Index+1] := ptEndNode;
  Data.Points[Index+2] := p1;
  Data.Points[Index+3].x := p0.x + (p0.y - p1.y) div 2;
  Data.Points[Index+3].y := p0.y + (p1.x - p0.x) div 2;
  Data.PointTypes[Index+3] := ptEndNode;
 end;

 procedure SetTriangle0(const p0, p1: TPoint; Index: integer);
 begin
  Data.Points[Index+0] := p1;
  Data.Points[Index+1].x := p0.x - (p0.y - p1.y);
  Data.Points[Index+1].y := p0.y - (p1.x - p0.x);
  Data.Points[Index+2].x := p0.x + (p0.y - p1.y);
  Data.Points[Index+2].y := p0.y + (p1.x - p0.x);
  Data.PointTypes[Index+2] := ptEndNodeClose;
 end;

 procedure SetTriangle1(const p0, p1: TPoint; Index: integer);
 begin
  Data.Points[Index+0] := p1;
  Data.Points[Index+1].x := p0.x - (p0.y - p1.y) div 2;
  Data.Points[Index+1].y := p0.y - (p1.x - p0.x) div 2;
  Data.Points[Index+2].x := p0.x + (p0.y - p1.y) div 2;
  Data.Points[Index+2].y := p0.y + (p1.x - p0.x) div 2;
  Data.PointTypes[Index+2] := ptEndNodeClose;
 end;

begin
 Result := GetLineCapInfo(LineCap, Data.Info, CapSize);
 if not Result then exit;
 DistX := p1.x - p0.x;
 DistY := p1.y - p0.y;
 LineDist := sqrt(DistX*DistX + DistY*DistY);
 case LineCap of
   psArrow0:
     begin
      GetPointOnLine(Data.Info.Size / 2, Temp1);
      SetCount(4);
      SetArrow0(Temp1, p1, 0);
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   psDblArrow0:
     begin
      GetPointOnLine(Data.Info.Size / 2, Temp1);
      GetPointOnLine(Data.Info.Size, Temp2);
      SetCount(8);
      SetArrow0(Temp1, p1, 0);
      SetArrow0(Temp2, Temp1, 4);
      Data.p0 := Temp2;
      Data.p1 := p1;
     end;
   psBackArrow0:
     begin
      GetPointOnLine(-Data.Info.Size / 2, Temp1);
      SetCount(4);
      SetArrow0(Temp1, p1, 0);
      Data.p0 := p1;
      Data.p1 := Temp1;
     end;
   psBackDblArrow0:
     begin
      GetPointOnLine(-Data.Info.Size / 2, Temp1);
      GetPointOnLine( Data.Info.Size / 2, Temp2);
      SetCount(8);
      SetArrow0(Temp1, p1, 0);
      SetArrow0(p1, Temp2, 4);
      Data.p0 := Temp2;
      Data.p1 := p1;
     end;
   psArrow1:
     begin
      GetPointOnLine(Data.Info.Size, Temp1);
      SetCount(4);
      SetArrow1(Temp1, p1, 0);
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   psDblArrow1:
     begin
      TempDist := Data.Info.Size * 0.7;
      GetPointOnLine(Data.Info.Size, Temp1);
      GetPointOnLine(TempDist, Temp2);
      GetPointOnLine(Data.Info.Size + TempDist, Temp3);
      SetCount(8);
      SetArrow1(Temp1, p1, 0);
      SetArrow1(Temp3, Temp2, 4);
      Data.p0 := Temp3;
      Data.p1 := p1;
     end;
   psBackArrow1:
     begin
      GetPointOnLine(-Data.Info.Size, Temp1);
      SetCount(4);
      SetArrow1(Temp1, p1, 0);
      Data.p0 := p1;
      Data.p1 := Temp1;
     end;
   psBackDblArrow1:
     begin
      TempDist := Data.Info.Size * 0.7;
      GetPointOnLine(-Data.Info.Size, Temp1);
      GetPointOnLine(TempDist, Temp2);
      GetPointOnLine(-Data.Info.Size + TempDist, Temp3);
      SetCount(8);
      SetArrow1(Temp1, p1, 0);
      SetArrow1(Temp3, Temp2, 4);
      Data.p0 := p1;
      Data.p1 := Temp3;
     end;
   psArrow:
     begin
      a := Data.Info.Size * 0.1875;
      b := Data.Info.Size * 0.40;
      c := Data.Info.Size * 0.1144;
      d := Data.Info.Size * 0.0731;
      GetPointOnLine(Data.Info.Size, Temp1);
      GetPointOnLine(Data.Info.Size - d, Temp2);
      GetPointOnLine(Data.Info.Size - c, Temp3);
      SetCount(8);
      // Calc nodes
      Data.Points[0] := p1;
      Data.Points[1].x := Temp1.x - (Temp1.y - p1.y) div 2;
      Data.Points[1].y := Temp1.y - (p1.x - Temp1.x) div 2;
      Data.Points[4] := Temp3;
      Data.Points[7].x := Temp1.x + (Temp1.y - p1.y) div 2;
      Data.Points[7].y := Temp1.y + (p1.x - Temp1.x) div 2;
      Data.PointTypes[7] := ptEndNodeClose;
      // Calc control points

      TempDist1 := p1.x - Temp2.x;
      TempDist2 := Temp2.y - p1.y;
      TempDist := sqrt(TempDist1*TempDist1 + TempDist2*TempDist2);
      if TempDist <> 0
       then Coeff := b / TempDist
       else Coeff := 0;
      Temp4.x := Round((Temp2.y - p1.y) * Coeff);
      Temp4.y := Round((p1.x - Temp2.x) * Coeff);
      Data.Points[2].x := Temp2.x - Temp4.x;
      Data.Points[2].y := Temp2.y - Temp4.y;
      Data.PointTypes[2] := ptControl;
      Data.Points[6].x := Temp2.x + Temp4.x;
      Data.Points[6].y := Temp2.y + Temp4.y;
      Data.PointTypes[6] := ptControl;

      TempDist1 := p1.x - Temp3.x;
      TempDist2 := Temp3.y - p1.y;
      TempDist := sqrt(TempDist1*TempDist1 + TempDist2*TempDist2);
      if TempDist <> 0
       then Coeff := a / TempDist
       else Coeff := 0;
      Temp4.x := Round((Temp3.y - p1.y) * Coeff);
      Temp4.y := Round((p1.x - Temp3.x) * Coeff);
      Data.Points[3].x := Temp3.x - Temp4.x;
      Data.Points[3].y := Temp3.y - Temp4.y;
      Data.PointTypes[3] := ptControl;
      Data.Points[5].x := Temp3.x + Temp4.x;
      Data.Points[5].y := Temp3.y + Temp4.y;
      Data.PointTypes[5] := ptControl;
     end;
   psTriangle:
     begin
      GetPointOnLine(Data.Info.Size, Temp1);
      SetCount(3);
      SetTriangle1(Temp1, p1, 0);
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   psDblTriangle:
     begin
      GetPointOnLine(Data.Info.Size / 2, Temp1);
      GetPointOnLine(Data.Info.Size, Temp2);
      SetCount(6);
      SetTriangle0(Temp1, p1, 0);
      SetTriangle0(Temp2, Temp1, 3);
      Data.p0 := Temp2;
      Data.p1 := p1;
     end;
   psBackTriangle:
     begin
      GetPointOnLine(Data.Info.Size, Temp1);
      SetCount(3);
      SetTriangle1(p1, Temp1, 0);
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   psBackDblTriangle:
     begin
      GetPointOnLine(Data.Info.Size / 2, Temp1);
      GetPointOnLine(Data.Info.Size, Temp2);
      SetCount(6);
      SetTriangle0(p1, Temp1, 0);
      SetTriangle0(Temp1, Temp2, 3);
      Data.p0 := Temp2;
      Data.p1 := p1;
     end;
   psSquare:
     begin
      GetPointOnLine(Data.Info.Size / 2, Temp1);
      SetCount(4);
      Data.Points[0].x := Temp1.x - (Temp1.y - p1.y);
      Data.Points[0].y := Temp1.y - (p1.x - Temp1.x);
      Data.Points[1].x := Data.Points[0].x + 2 * (p1.x - Temp1.x);
      Data.Points[1].y := Data.Points[0].y + 2 * (p1.y - Temp1.y);
      Data.Points[3].x := Temp1.x + (Temp1.y - p1.y);
      Data.Points[3].y := Temp1.y + (p1.x - Temp1.x);
      Data.Points[2].x := Data.Points[3].x + 2 * (p1.x - Temp1.x);
      Data.Points[2].y := Data.Points[3].y + 2 * (p1.y - Temp1.y);
      Data.PointTypes[3] := ptEndNodeClose;
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   psCircle,
   psDotCircle:
     begin
      if LineCap = psCircle
       then GetPointOnLine(Data.Info.Size / 2, Temp1)
       else GetPointOnLine(Data.Info.Size / 4, Temp1);
      SetCount(12);
      // Calc circle nodes
      Data.Points[0] := Temp1;
      Data.Points[3].x := p1.x - (Temp1.y - p1.y);
      Data.Points[3].y := p1.y - (p1.x - Temp1.x);
      Data.Points[6].x := p1.x + (p1.x - Temp1.x);
      Data.Points[6].y := p1.y + (p1.y - Temp1.y);
      Data.Points[9].x := p1.x + (Temp1.y - p1.y);
      Data.Points[9].y := p1.y + (p1.x - Temp1.x);
      // Calc circle control points
      Temp2.x := Round((Temp1.y - p1.y) * BezierCircleCoeff);
      Temp2.y := Round((p1.x - Temp1.x) * BezierCircleCoeff);
      Data.Points[1].x := Temp1.x - Temp2.x;
      Data.Points[1].y := Temp1.y - Temp2.y;
      Data.PointTypes[1] := ptControl;
      Data.Points[2].x := Data.Points[3].x - Temp2.y;
      Data.Points[2].y := Data.Points[3].y + Temp2.x;
      Data.PointTypes[2] := ptControl;
      Data.Points[4].x := Data.Points[3].x + Temp2.y;
      Data.Points[4].y := Data.Points[3].y - Temp2.x;
      Data.PointTypes[4] := ptControl;
      Data.Points[5].x := Data.Points[6].x - Temp2.x;
      Data.Points[5].y := Data.Points[6].y - Temp2.y;
      Data.PointTypes[5] := ptControl;
      Data.Points[7].x := Data.Points[6].x + Temp2.x;
      Data.Points[7].y := Data.Points[6].y + Temp2.y;
      Data.PointTypes[7] := ptControl;
      Data.Points[8].x := Data.Points[9].x + Temp2.y;
      Data.Points[8].y := Data.Points[9].y - Temp2.x;
      Data.PointTypes[8] := ptControl;
      Data.Points[10].x := Data.Points[9].x - Temp2.y;
      Data.Points[10].y := Data.Points[9].y + Temp2.x;
      Data.PointTypes[10] := ptControl;
      Data.Points[11].x := Data.Points[0].x + Temp2.x;
      Data.Points[11].y := Data.Points[0].y + Temp2.y;
      Data.PointTypes[11] := ptControl;
      Data.PointTypes[9] := ptEndNodeClose;
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   psRhombus:
     begin
      GetPointOnLine(Data.Info.Size / 2, Temp1);
      SetCount(4);
      Data.Points[0] := Temp1;
      Data.Points[1].x := p1.x - (Temp1.y - p1.y);
      Data.Points[1].y := p1.y - (p1.x - Temp1.x);
      Data.Points[2].x := p1.x + (p1.x - Temp1.x);
      Data.Points[2].y := p1.y + (p1.y - Temp1.y);
      Data.Points[3].x := p1.x + (Temp1.y - p1.y);
      Data.Points[3].y := p1.y + (p1.x - Temp1.x);
      Data.PointTypes[3] := ptEndNodeClose;
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   psThinRect:
     begin
      GetPointOnLine(Data.Info.Size / 3, Temp1);
      GetPointOnLine(Data.Info.Size / 2, Temp2);
      SetCount(4);
      Data.Points[0].x := Temp1.x - (Temp2.y - p1.y);
      Data.Points[0].y := Temp1.y - (p1.x - Temp2.x);
      Data.Points[1].x := p1.x - (Temp2.y - p1.y);
      Data.Points[1].y := p1.y - (p1.x - Temp2.x);
      Data.Points[2].x := p1.x + (Temp2.y - p1.y);
      Data.Points[2].y := p1.y + (p1.x - Temp2.x);
      Data.Points[3].x := Temp1.x + (Temp2.y - p1.y);
      Data.Points[3].y := Temp1.y + (p1.x - Temp2.x);
      Data.p0 := Temp1;
      Data.p1 := p1;
     end;
   else
     begin
      Result := false;
     end;
 end;
 if Assigned(ResolveLineCapStyle) then
   ResolveLineCapStyle(LineCap, p0, p1, Data, CapSize, Result); 
end;

procedure RenderCap(DC: HDC; LineCap: integer; const p0, p1: TPoint;
  CapSize: integer);
var Data: TLineCapData;
    Complete: boolean;
    Half1, Half2: integer;
begin
 if (LineCap = psCircle) or (LineCap = psDotCircle) then begin
  if LineCap = psCircle
   then Half1 := CapSize div 2
   else Half1 := CapSize div 4;
  Half2 := Half1 + 1; //CapSize - Half1;
  Ellipse(DC, p1.x - Half1, p1.y - Half1, p1.x + Half2, p1.y + Half2);
 end else begin
  if not GetLineCapData(LineCap, p0, p1, Data, CapSize) then exit;
  if CreatePath(DC, Data.Points, Data.PointTypes, true, true, Complete) then
    StrokeAndFillPath(DC);
 end;
end;

procedure RenderCaps(DC: HDC; PenWidth: integer;
  const BeginCap, EndCap: TRenderCapParams; const Points: TPointArray;
  const PointTypes: TPointTypeArray; Info: PPathInfo = Nil);
var i: integer;
    DefaultInfo, BegInfo, EndInfo: record
     Brush: HBrush;
     Pen: HPen;
    end;
    InternalPathInfo: boolean;
begin
 InternalPathInfo := false;
 FillChar(DefaultInfo, SizeOf(DefaultInfo), 0);
 FillChar(BegInfo, SizeOf(BegInfo), 0);
 FillChar(EndInfo, SizeOf(EndInfo), 0);
 try
  // Create pens and brushes
  if BeginCap.FillColor = clNone
   then BegInfo.Brush := GetStockObject(NULL_BRUSH)
   else BegInfo.Brush := CreateSolidBrush(BeginCap.FillColor);
  if BeginCap.OutlineColor = clNone
   then BegInfo.Pen := GetStockObject(NULL_PEN)
   else BegInfo.Pen := CreatePen(PS_INSIDEFRAME, PenWidth, BeginCap.OutlineColor);
  if EndCap.FillColor = clNone
   then EndInfo.Brush := GetStockObject(NULL_BRUSH)
   else EndInfo.Brush := CreateSolidBrush(EndCap.FillColor);
  if EndCap.OutlineColor = clNone
   then EndInfo.Pen := GetStockObject(NULL_PEN)
   else EndInfo.Pen := CreatePen(PS_INSIDEFRAME, PenWidth, EndCap.OutlineColor);
  // Init DC
  DefaultInfo.Brush := SelectObject(DC, BegInfo.Brush);
  DefaultInfo.Pen := SelectObject(DC, BegInfo.Pen);
  SetROP2(DC, R2_COPYPEN);
  // Check path info
  if not Assigned(Info) then begin
   New(Info);
   InternalPathInfo := true;
   GetPathInfo(Points, PointTypes, Info^);
  end;
  // For all figures in curve
  for i:=0 to Length(Info.Figures)-1 do with Info.Figures[i] do begin
   if IsClosed or (LastNode - FirstNode < 1) then continue;
   // Begin cap
   if BeginCap.Style <> psNoCap then begin
    SelectObject(DC, BegInfo.Brush);
    SelectObject(DC, BegInfo.Pen);
    RenderCap(DC, BeginCap.Style, Points[FirstNode+1], Points[FirstNode],
      BeginCap.CapSize);
   end;
   // End cap
   if EndCap.Style <> psNoCap then begin
    SelectObject(DC, EndInfo.Brush);
    SelectObject(DC, EndInfo.Pen);
    RenderCap(DC, EndCap.Style, Points[LastNode-1], Points[LastNode],
      EndCap.CapSize);
   end;
  end;
 finally
  if DefaultInfo.Brush <> 0 then SelectObject(DC, DefaultInfo.Brush);
  if DefaultInfo.Pen <> 0 then SelectObject(DC, DefaultInfo.Pen);
  DeleteObject(BegInfo.Brush);
  DeleteObject(BegInfo.Pen);
  DeleteObject(EndInfo.Brush);
  DeleteObject(EndInfo.Pen);
  if InternalPathInfo then Dispose(Info);
 end;
end;

// TFlexBox //////////////////////////////////////////////////////////////////

procedure TFlexBox.ControlCreate;
begin
 Width := 1;
 Height := 1;
 inherited;
 Visible := True;
end;

procedure TFlexBox.CreateProperties;
begin
 inherited;
 FRoundnessProp := TIntProp.Create(Props, 'Roundness');
 FRoundnessProp.Style := FRoundnessProp.Style + [ psScalable ]; 
end;

procedure TFlexBox.ControlTranslate(const TranslateInfo: TTranslateInfo);
begin
 inherited;
 FBrushProp.Translate(TranslateInfo);
end;

function TFlexBox.CreateCurveControl: TFlexControl;
var Right, Bottom, W, H: integer;
    dx, dy: integer;
    RoundHalf: integer;
begin
 Result := TFlexCurve.Create(Owner, Parent, Layer);
 try
  Result.BeginUpdate;
  try
   W := Width;
   H := Height;
   Right := W;
   Bottom := H;
   // Copy properties
   FlexControlCopy(Self, Result);
   // Make points data
   with TFlexCurve(Result) do begin
    // Delete all points
    while PointCount > 0 do DeletePoint(0);
    if FRoundnessProp.Value = 0 then begin
     // Make simple rectangle (with 4 points and CloseFigure)
     AddPoint(Point(0, 0));
     AddPoint(Point(Right, 0));
     AddPoint(Point(Right, Bottom));
     AddPoint(Point(0, Bottom));
     EndFigure;
    end else begin
     // Make rounded rectangle
     dx := Round(FRoundnessProp.Value * (1 - BezierCircleCoeff) / 2);
     dy := dx;
     RoundHalf := FRoundnessProp.Value div 2;
     AddPoint(Point(RoundHalf, 0));
     AddCurvePoints(
       Point(Right-RoundHalf, 0),
       Point(Right-dx, 0),
       Point(Right, dy) );
     AddPoint(Point(Right, RoundHalf));
     AddCurvePoints(
       Point(Right, Bottom-RoundHalf),
       Point(Right, Bottom-dy),
       Point(Right-dx, Bottom) );
     AddPoint(Point(Right-RoundHalf, Bottom));
     AddCurvePoints(
       Point(RoundHalf, Bottom),
       Point(dx, Bottom),
       Point(0, Bottom-dy) );
     AddPoint(Point(0, Bottom-RoundHalf));
     AddCurvePoints(
       Point(0, RoundHalf),
       Point(0, dy),
       Point(dx, 0) );
     EndFigure;
    end;
   end;
  finally
   Result.EndUpdate;
  end;
 except
  Result.Free;
  raise;
 end;
end;

function TFlexBox.CreateBoxRegion(const PaintRect: TRect;
  Inflate: boolean = false): HRGN;
var R: TRect;
    PenWidth: integer;
    PenStyle: TPenStyle;
    IsGeometricPen: boolean;
    Round: integer;
    Rgn: HRGN;
    i, InflateSize: integer;
begin
 R := PaintRect;
 FPenProp.GetPaintData(PenWidth, PenStyle, IsGeometricPen, Owner.Scale);
 Round := ScaleValue(FRoundnessProp.Value, Owner.Scale);
 InflateSize := PenWidth;
 if Inflate then begin
  if InflateSize > 2 then InflateSize := 2;
  i := InflateSize;
  if (PenStyle <> psInsideFrame) and (PenStyle <> psClear) and
     (PenWidth > 1) then
   inc(i, PenWidth div 2);
  InflateRect(R, i, i);
 end;
 if not Assigned(FRoundnessProp) or (FRoundnessProp.Value = 0) then
  Result := CreateRectRgnIndirect(R)
 else
 if PenWidth < 2 then
  Result := CreateRoundRectRgn(
                     R.Left, R.Top, R.Right+1, R.Bottom+1, Round, Round)
                //   R.Left, R.Top, R.Right, R.Bottom, Round, Round)
 else
  Result := CreateRoundRectRgn(
                     R.Left+1, R.Top+1, R.Right, R.Bottom, Round, Round);
 if not FAlwaysFilled and not (Assigned(Owner) and Owner.SelectAsFilled) and
   Inflate and FBrushProp.IsClear then begin
  if PenWidth < 3 then PenWidth := 3;
  InflateRect(R, -InflateSize -PenWidth, -InflateSize -PenWidth);
  if Round > 0
   then Rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right+1, R.Bottom+1,
                 Round-PenWidth, Round-PenWidth)
   else Rgn := CreateRectRgn(R.Left, R.Top, R.Right-1, R.Bottom-1);
  CombineRgn(Result, Result, Rgn, RGN_XOR);
  DeleteObject(Rgn);
 end;
end;

class function TFlexBox.CursorInCreate: TCursor;
begin
 Result := crCreateRectCursor;
end;

function TFlexBox.GetAnchorPoint: TPoint;
var Round: integer;
begin
 if FRoundnessProp.Value > WidthProp.Value
  then Round := WidthProp.Value
  else Round := FRoundnessProp.Value;
 with DocRect do begin
  Result.X := Left + Round div 2;
  Result.Y := Top;
 end;
 Owner.TransformPointIndirect(Result);
end;

function TFlexBox.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
var PenWidth: integer;
    PenStyle: TPenStyle;
    IsGeometricPen: boolean;
    InflateSize: integer;
begin
 Result := Rect(RefreshX, RefreshY,
                RefreshX + WidthProp.Value, RefreshY + HeightProp.Value);
 FPenProp.GetPaintData(PenWidth, PenStyle, IsGeometricPen, Owner.Scale);
 if (PenStyle <> psInsideFrame) and (PenStyle <> psClear) and
    (PenWidth > 1) then begin
  if (FPenProp.EndCap = pecSquare) and IsGeometricPen then
    InflateSize := FPenProp.Width
  else
    InflateSize := FPenProp.Width div 2;
  InflateRect(Result, InflateSize, InflateSize);
 end;
end;

function TFlexBox.GetDefaultLinkPoint(Index: integer): TPoint;
var Count, Round: integer;
    Half: TPoint;
    R: TRect;
    XMax, YMax: boolean;
begin
 Count := DefaultLinkPointCount;
 Round := FRoundnessProp.Value;
 XMax := Round > WidthProp.Value;
 YMax := Round > HeightProp.Value;
 Round := Round div 2;
 R := Rect(0, 0, WidthProp.Value, HeightProp.Value);
 Half.X := R.Right div 2;
 Half.Y := R.Bottom div 2;
 with R do
 if Count = 9 then begin
  // 9 points
  case Index of
   0: Result := TopLeft;
   1: Result := Point(Half.X, 0);
   2: Result := Point(R.Right, 0);
   3: Result := Point(R.Right, Half.Y);
   4: Result := BottomRight;
   5: Result := Point(Half.X, R.Bottom);
   6: Result := Point(0, R.Bottom);
   7: Result := Point(0, Half.Y);
   8: Result := Point(Half.X, Half.Y);
  end;
 end else begin
  // 13 points
  if XMax then
   if (Index = 0) or (Index = 2) then Index := 1 else
   if (Index = 6) or (Index = 8) then Index := 7;
  if YMax then
   if (Index = 3) or (Index = 5) then Index := 4 else
   if (Index = 9) or (Index = 11) then Index := 10;
  case Index of
    0: Result := Point(Round, 0);
    1: Result := Point(Half.X, 0);
    2: Result := Point(R.Right-Round, 0);
    3: Result := Point(R.Right, Round);
    4: Result := Point(R.Right, Half.Y);
    5: Result := Point(R.Right, R.Bottom-Round);
    6: Result := Point(R.Right-Round, R.Bottom);
    7: Result := Point(Half.X, R.Bottom);
    8: Result := Point(Round, R.Bottom);
    9: Result := Point(0, R.Bottom-Round);
   10: Result := Point(0, Half.Y);
   11: Result := Point(0, Round);
   12: Result := Point(Half.X, Half.Y);
  end;
 end;
end;

function TFlexBox.GetDefaultLinkPointCount: integer;
begin
 if FRoundnessProp.Value = 0
  then Result := 9
  else Result := 13;
end;

function TFlexBox.IsPointInside(PaintX, PaintY: integer): boolean;
var Rgn: HRGN;
    R: TRect;
begin
 if (FRoundnessProp.Value = 0) and not FBrushProp.IsClear then begin
  with DocRect do R := GetRefreshRect(Left, Top);
  Owner.TransformRect(R);
  with R do
   Result := (PaintX >= Left) and (PaintY >= Top) and
             (PaintX < Right) and (PaintY < Bottom);
 end else begin
  Rgn := CreateBoxRegion(PaintRect, True);
  try
   Result := PtInRegion(Rgn, PaintX, PaintY);
  finally
   DeleteObject(Rgn);
  end
 end;
end;

procedure TFlexBox.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 inherited;
 if Prop = FRoundnessProp then begin
  DoNotify(fnAnchorPoints);
 end;
end;

procedure TFlexBox.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = FRoundnessProp then
  IsStored := FRoundnessProp.Value <> 0
 else
  inherited;
end;

procedure TFlexBox.Paint(Canvas: TCanvas; var PaintRect: TRect);
var PrevRgn, ClipRgn: HRGN;
    Round: integer;

 procedure CanvasSetup;
 begin
  FPenProp.Setup(Canvas, Owner.Scale);
  FBrushProp.Setup(Canvas, Owner.Scale);
 end;

begin
 with Canvas do begin
  CanvasSetup;
  if FBrushProp.IsPaintAlternate then begin
   PrevRgn := 0;
   ClipRgn := CreateBoxRegion(PaintRect);
   try
    PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
    FBrushProp.PaintAlternate(Canvas, PaintRect, Owner.PaintRect,
      Owner.Scale, Owner.UseImageClipTransparent);
   finally
    SelectClipRgn(Canvas.Handle, PrevRgn);
    DeleteObject(ClipRgn);
    DeleteObject(PrevRgn);
   end;
   CanvasSetup;
  end;
  if FPenProp.ActiveWidth = 0 then begin
   inc(PaintRect.Right);
   inc(PaintRect.Bottom);
  end;
  if FRoundnessProp.Value = 0 then
   with PaintRect do Rectangle(Left, Top, Right, Bottom)
  else begin
   Round := ScaleValue(FRoundnessProp.Value, Owner.Scale);
   RoundRect(PaintRect.Left, PaintRect.Top, PaintRect.Right, PaintRect.Bottom,
         Round, Round);
  end;
 end;
end;

// TFlexEllipse //////////////////////////////////////////////////////////////

procedure TFlexEllipse.ControlCreate;
begin
 FArcOldFormatNeedCheck := true;
 Width := 1;
 Height := 1;
 inherited;
 Visible := True;
end;

procedure TFlexEllipse.CreateProperties;
begin
 inherited;
 SetLength(FPoints, 2);
 SetLength(FPointTypes, 2);
 FPointTypes[0] := ptNode;
 FPointTypes[1] := ptEndNode;
 GetPathInfo(FPoints, FPointTypes, FPathInfo);
 FBeginAngleProp := TIntProp.Create(Props, 'BeginAngle');
 FBeginAngleProp.Style := FBeginAngleProp.Style + [ psScalable ];
 FEndAngleProp := TIntProp.Create(Props, 'EndAngle');
 FEndAngleProp.Style := FEndAngleProp.Style + [ psScalable ];
 FArcAxesProp := TStrProp.Create(Props, 'ArcAxes');
 FArcAxesProp.Style := FArcAxesProp.Style - [ psVisible ];
 FArcAxesProp.OnGetString := GetArcAxes;
 SetPointsByAngles;
end;

class function TFlexEllipse.CursorInCreate: TCursor;
begin
 Result := crCreateEllipseCursor;
end;

procedure TFlexEllipse.ControlTranslate(
  const TranslateInfo: TTranslateInfo);
var NewBeg, NewEnd, NewTmp: integer;
    R, DR: TRect;

 procedure TranslateAngle(var Angle: integer);
 begin
  if TranslateInfo.Mirror then Angle := 180 * PixelScaleFactor - Angle;
  inc(Angle, TranslateInfo.Rotate * PixelScaleFactor);
  if Angle < 0 then Angle := 360 * PixelScaleFactor + Angle else
  if Angle >= 360 * PixelScaleFactor then
   Angle :=
     Angle - (Angle div (360 * PixelScaleFactor)) * (360 * PixelScaleFactor);
 end;

begin
 FEditing := true;
 try
  DR := DocRect;
  inherited;
  FBrushProp.Translate(TranslateInfo);
  // Translate angles
  NewBeg := FBeginAngleProp.Value;
  NewEnd := FEndAngleProp.Value;
  TranslateAngle(NewBeg);
  TranslateAngle(NewEnd);
  if TranslateInfo.Mirror then begin
   // Exchange angles
   NewTmp := NewBeg;
   NewBeg := NewEnd;
   NewEnd := NewTmp;
  end;
  if FUseAxes then begin
   R := Rect(FArcOfs.X, FArcOfs.Y, FArcOfs.X + FArcSize.X,
     FArcOfs.Y + FArcSize.Y);
   with DR do OffsetRect(R, Left, Top);
   R := TranslateRect(R, TranslateInfo);
   with DocRect do begin
    FArcOfs.X := R.Left - Left;
    FArcOfs.Y := R.Top - Top;
   end;
   FArcSize.X := R.Right - R.Left;
   FArcSize.Y := R.Bottom - R.Top;
  end;
  FBeginAngleProp.Value := NewBeg;
  FEndAngleProp.Value := NewEnd;
 finally
  FEditing := false;
 end;
 if FUseAxes then begin
  FArcResizing := true;
  try
   SetPointsByAngles;
  finally
   FArcResizing := false;
  end;
 end;
end;

procedure TFlexEllipse.MirrorInResize(HMirror, VMirror: boolean);
begin
 inherited;
 if FUseAxes then with FArcResizingBounds do begin
  if HMirror then
   Left := (FResizingRect.Right - FResizingRect.Left) - (Right + Left);
  if VMirror then
   Top := (FResizingRect.Bottom - FResizingRect.Top) - (Bottom + Top);
 end;
end;

function TFlexEllipse.CreateCurveControl: TFlexControl;
var Right, Bottom, W, H: integer;
    dx, dy: integer;
    XHalf, YHalf: integer;
    A, B, C, X, Y, SinVal, CosVal, RX, RY, OfsX, OfsY: double;
    StartRad, EndRad, SweepRad: double;
    SecStart, SecEnd: integer;
    XY: array[0..7] of double;
    ArcPoints: array[0..3] of TPoint;
    i: integer;
begin
 Result := TFlexCurve.Create(Owner, Parent, Layer);
 try
  Result.BeginUpdate;
  try
   TFlexCurve(Result).FZeroPoints := false;
   // Copy properties
   FlexControlCopy(Self, Result);
   // Make points data
   with TFlexCurve(Result) do begin
    // Delete all points
    while PointCount > 0 do DeletePoint(0);
    if FBeginAngleProp.Value = FEndAngleProp.Value then begin
     // Make Ellipse
     W := Width;
     H := Height;
     Right := W;
     Bottom := H;
     XHalf := W div 2;
     YHalf := H div 2;

     dx := Round(W * (1 - BezierCircleCoeff) / 2);
     dy := Round(H * (1 - BezierCircleCoeff) / 2);
     AddCurvePoints(
       Point(XHalf, 0),
       Point(Right-dx, 0),
       Point(Right, dy) );
     AddCurvePoints(
       Point(Right, YHalf),
       Point(Right, Bottom-dy),
       Point(Right-dx, Bottom) );
     AddCurvePoints(
       Point(XHalf, Bottom),
       Point(dx, Bottom),
       Point(0, Bottom-dy) );
     AddCurvePoints(
       Point(0, YHalf),
       Point(0, dy),
       Point(dx, 0) );
     EndFigure;
    end else begin
     // Make Arc
     XHalf := FArcOfs.X + FArcSize.X div 2;
     YHalf := FArcOfs.Y + FArcSize.Y div 2;
     RX := FArcSize.X / 2;
     RY := FArcSize.Y / 2;
     OfsX := FArcOfs.X + RX;
     OfsY := FArcOfs.Y + RY;
     SecStart := FBeginAngleProp.Value;
     SecEnd :=
       (SecStart div (90 * PixelScaleFactor) + 1) * (90 * PixelScaleFactor);
     repeat
      if (FEndAngleProp.Value > SecStart) and
         (FEndAngleProp.Value < SecEnd) then
       SecEnd := FEndAngleProp.Value;
      if SecEnd >= 360 * PixelScaleFactor then
       SecEnd := SecEnd - 360 * PixelScaleFactor;
      // Calculate arc
      StartRad := SecStart * pi / (180 * PixelScaleFactor);
      EndRad := SecEnd * pi / (180 * PixelScaleFactor);
      SweepRad := EndRad - StartRad;
      if SweepRad < 0 then SweepRad := 2*pi + SweepRad;
      // Compute bezier curve for arc centered along y axis
      // Anticlockwise: (0,-B), (x,-y), (x,y), (0,B)
      B := sin(SweepRad / 2);
      C := cos(SweepRad / 2);
      A := 1 - C;
      X := A * 4 / 3;
      Y := B - X * (1 - A) / B;
      XY[0] := C;
      XY[1] := -B;
      XY[2] := C + X;
      XY[3] := -Y;
      XY[4] := C + X;
      XY[5] := Y;
      XY[6] := C;
      XY[7] := B;
      // rotate to the original angle
      SinVal := sin(StartRad + SweepRad / 2);
      CosVal := cos(StartRad + SweepRad / 2);
      for i:=0 to 3 do begin
       ArcPoints[i].x :=
         // Round(RX + (XY[i*2] * CosVal - XY[i*2 + 1] * SinVal) * RX);
         Round(OfsX + (XY[i*2] * CosVal - XY[i*2 + 1] * SinVal) * RX);
       ArcPoints[i].y :=
         // Round(RY - (XY[i*2] * SinVal + XY[i*2 + 1] * CosVal) * RY);
         Round(OfsY - (XY[i*2] * SinVal + XY[i*2 + 1] * CosVal) * RY);
      end;
      AddCurvePoints(ArcPoints[0], ArcPoints[1], ArcPoints[2]);
      if SecEnd = FEndAngleProp.Value then begin
       // Set last node
       AddPoint(ArcPoints[3]);
       break;
      end;
      SecStart := SecEnd;
      SecEnd := SecStart + (90 * PixelScaleFactor);
     until false;
     if not BrushProp.IsClear then begin
      // Close sector
      AddPoint(Point(XHalf, YHalf));
      EndFigure;
     end else
      EndFigure(false);
    end;
   end;
  finally
   Result.EndUpdate;
  end;
 except
  Result.Free;
  raise;
 end;
end;

function TFlexEllipse.CreateEllipseRegion(const PaintRect: TRect): HRGN;
var PenWidth: integer;
    PenStyle: TPenStyle;
    IsGeometricPen: boolean;
    R: TRect;
begin
 R := PaintRect;
 FPenProp.GetPaintData(PenWidth, PenStyle, IsGeometricPen, Owner.Scale);
 if PenWidth < 2 then
  Result := CreateEllipticRgn(R.Left, R.Top, R.Right+1, R.Bottom+1)
 else
  Result := CreateEllipticRgn(R.Left+1, R.Top+1, R.Right, R.Bottom);
end;

function TFlexEllipse.IsPointInside(PaintX, PaintY: integer): boolean;
var PenWidth: integer;
    PenStyle: TPenStyle;
    IsGeometricPen: boolean;
    R, OrigDocRect: TRect;
    py, px, c1, c2: double;
    InflateSize: integer;

 function PtOnEllipse(const R: TRect; NeedLeft: boolean): double;
 var dx, dy, c: double;
 begin
  dx := (R.Right - R.Left) div 2;
  dy := (R.Bottom - R.Top) div 2;
  if dy = 0 then
   c := dx
  else begin
   c := dy * dy - py * py;
   if c > 0
    then c := (dx / dy) * sqrt(c)
    else c := 0;
  end;
  if NeedLeft then Result := -c else Result := c;
 end;

begin
 OrigDocRect := DocRect;
 R := OrigDocRect;
 if FUseAxes then begin
  OffsetRect(R, FArcOfs.X, FArcOfs.Y);
  R.Right := R.Left + FArcSize.X;
  R.Bottom := R.Top + FArcSize.Y;
 end;
 Owner.TransformRect(R);
 FPenProp.GetPaintData(PenWidth, PenStyle, IsGeometricPen, Owner.Scale);
 InflateSize := SelectionThreshold;
 if (PenStyle <> psInsideFrame) and (PenStyle <> psClear) and
    (PenWidth > 1) then
  inc(InflateSize, PenWidth div 2);
 InflateRect(R, InflateSize, InflateSize);
 Result :=
   (PaintX >= R.Left) and (PaintX <= R.Right) and
   (PaintY >= R.Top) and (PaintY <= R.Bottom);
 if not Result then exit;
 px := PaintX - (R.Left + R.Right) div 2;
 py := PaintY - (R.Top + R.Bottom) div 2;
 c1 := PtOnEllipse(R, px < 0);
 Result := c1 <> 0;
 if not Result then exit;
 if not (Assigned(Owner) and Owner.SelectAsFilled) and
    FBrushProp.IsClear then begin
  inc(PenWidth, SelectionThreshold + SelectionThreshold);
  InflateRect(R, -PenWidth, -PenWidth);
  c2 := PtOnEllipse(R, px < 0);
 end else
  c2 := 0;
 if c1 < c2
  then Result := (px >= c1) and (px <= c2)
  else Result := (px >= c2) and (px <= c1);
 if not Result then exit;
 if FBeginAngleProp.Value <> FEndAngleProp.Value then begin
  // Just check that point PaintX, PaintY in inflated OrigDocRect
  R := OrigDocRect;
  Owner.TransformRect(R);
  InflateRect(R, SelectionThreshold, SelectionThreshold);
  Result :=
   (PaintX >= R.Left) and (PaintX <= R.Right) and
   (PaintY >= R.Top) and (PaintY <= R.Bottom);
 end;
end;

function TFlexEllipse.GetCeneter: TPoint;
begin
 if FUseAxes then begin
  Result.X := FArcOfs.X + FArcSize.X div 2;
  Result.Y := FArcOfs.Y + FArcSize.Y div 2;
 end else begin
  Result.X := WidthProp.Value div 2;
  Result.Y := HeightProp.Value div 2;
 end;
end;

function TFlexEllipse.GetPoint(Index: integer): TPoint;
begin
 Result := FPoints[Index];
end;

function TFlexEllipse.GetPointCount: integer;
begin
 Result := Length(FPoints);
end;

procedure TFlexEllipse.SetPoint(Index: integer; const Value: TPoint);
begin
 if (Value.X = FPoints[Index].X) and (Value.Y = FPoints[Index].Y) then exit;
 FPoints[Index] := Value;
 SetAnglesByPoints;
 DoNotify(fnEditPoints);
end;

function TFlexEllipse.GetPointsInfo: PPathInfo;
begin
 Result := @FPathInfo;
end;

function TFlexEllipse.EditPoints(Func: TPathEditFunc;
  const Selected: TSelectedArray; Params: PPathEditParams = Nil): boolean;
begin
 Result := EditPath(FPoints, FPointTypes, Selected, Func, Params);
 if Result then begin
  SetAnglesByPoints;
  // Recalc points
  SetPointsByAngles;
 end;
end;

function TFlexEllipse.EditPointsCaps(
  const Selected: TSelectedArray): TPathEditFuncs;
begin
 Result := [ pfOffset ];
end;

function TFlexEllipse.GetAnchorPoint: TPoint;
begin
 if (Length(FPoints) = 2) and
    (FBeginAngleProp.Value <> FEndAngleProp.Value) then
  with DocRect do begin
   Result.X := Left + FPoints[0].X;
   Result.Y := Top + FPoints[0].Y;
   Owner.TransformPoint(Result.X, Result.Y);
  end
 else begin
  with DocRect do begin
   Result.X := Left + Width div 2;
   Result.Y := Top;
  end;
  Owner.TransformPointIndirect(Result);
 end;
end;

function TFlexEllipse.GetDefaultLinkPoint(Index: integer): TPoint;
var Half: TPoint;
    R: TRect;
begin
 R := Rect(0, 0, WidthProp.Value, HeightProp.Value);
 Half.X := R.Right div 2;
 Half.Y := R.Bottom div 2;
 case Index of
  0: Result := Point(Half.X, 0);
  1: Result := Point(R.Right, Half.Y);
  2: Result := Point(Half.X, R.Bottom);
  3: Result := Point(0, Half.Y);
  4: Result := Point(Half.X, Half.Y);
 end;
end;

function TFlexEllipse.GetDefaultLinkPointCount: integer;
begin
 Result := 5;
end;

procedure TFlexEllipse.SetUseAxes(Value: boolean);
begin
 if Value = FUseAxes then exit;
 FUseAxes := Value;
 if FUseAxes then begin
  FArcOfs.X := 0;
  FArcOfs.Y := 0;
  FArcSize.X := WidthProp.Value;
  FArcSize.Y := HeightProp.Value;
 end else begin
  BeginUpdate;
  try
   LeftProp.Value := LeftProp.Value + FArcOfs.X;
   TopProp.Value := TopProp.Value + FArcOfs.Y;
   WidthProp.Value := FArcSize.X;
   HeightProp.Value := FArcSize.Y;
  finally
   EndUpdate;
  end;
  FArcOfs.X := 0;
  FArcOfs.Y := 0;
  FArcSize.X := 0;
  FArcSize.Y := 0;
 end;
end;

function TFlexEllipse.GetIsPie: boolean;
begin
 Result := (FBeginAngleProp.Value <> FEndAngleProp.Value) and
   not BrushProp.IsClear;
end;

procedure TFlexEllipse.GetArcAxes(Sender: TObject; out s: string);
var R: TRect;
begin
 if UseAxes then begin
  R.TopLeft := FArcOfs;
  R.Right := R.Left + FArcSize.X;
  R.Bottom := R.Top + FArcSize.Y;
  if UpdateCounter  > 0
   then with FSavedDocRect do OffsetRect(R, Left, Top)
   else with DocRect do OffsetRect(R, Left, Top);
 end else
 if UpdateCounter  > 0
  then R := FSavedDocRect
  else R := DocRect;
 s := Format('%d;%d;%d;%d;%d;%d',
   [ R.Left, R.Top, R.Right, R.Bottom,
     FBeginAngleProp.Value, FEndAngleProp.Value ] )
end;

procedure TFlexEllipse.SetArcAxes(const s: string);
var Numbers: array[0..5] of integer;
    i, Start, Len, Index: integer;
begin
 if FEditing or (s = '') then exit;
 FArcOldFormat := false;
 FArcOldFormatNeedCheck := false;
 // Extract numbers
 Len := Length(s);
 Index := 0;
 i := 1;
 try
  while (i <= Len) and (Index < Length(Numbers)) do begin
   Start := i;
   while (i <= Len) and (s[i] <> ';') do inc(i);
   Numbers[Index] := StrToInt(copy(s, Start, i - Start));
   inc(Index);
   inc(i);
  end;
 except
 end;
 // Check all numbers extracted
 if Index <> Length(Numbers) then exit;
 // Set arc params
 BeginUpdate;
 try
  FEditing := true;
  FUseAxes := false;
  {LeftProp.Value := Numbers[0];
  TopProp.Value := Numbers[1];
  WidthProp.Value := Numbers[2];
  HeightProp.Value := Numbers[3]; }
  DocRect := Rect(Numbers[0], Numbers[1], Numbers[2], Numbers[3]);
  FBeginAngleProp.Value := Numbers[4];
  FEndAngleProp.Value := Numbers[5];
  FEditing := false;
  SetPointsByAngles;
 finally
  FEditing := true;
  EndUpdate;
  FEditing := false;
  FArcAxesProp.SavedValue := '';
 end;
end;

procedure TFlexEllipse.UpdateArcBoundsByPoints;
var NewBounds: TRect;
    OldOfs, Center: TPoint;
    QStart, QEnd: integer;
    ChangeEditing: boolean;
    DoResize: boolean;
begin
 if not UseAxes or FArcResizing or FArcOldFormat then exit;
 Owner.History.RecordAction(TPropHistoryAction, FArcAxesProp);
 QStart := FBeginAngleProp.Value div (90 * PixelScaleFactor) mod 4;
 if QStart < 0 then QStart := 4 - QStart;
 QEnd := FEndAngleProp.Value div (90 * PixelScaleFactor) mod 4;
 if QEnd < 0 then QEnd := 4 - QEnd;
 // Calculate arc occuped rectangle
 with NewBounds do
  if (QStart = QEnd) and
     (FBeginAngleProp.Value > FEndAngleProp.Value) then begin
   // Ellipse arc lies in all 4 quadrants
   TopLeft := FArcOfs;
   Right := Left + FArcSize.X;
   Bottom := Top + FArcSize.Y;
  end else begin
   // Points not in all quadrants
   TopLeft := FPoints[0];
   BottomRight := TopLeft;
   repeat
    if QStart = QEnd then begin
     if FPoints[1].X < Left then Left := FPoints[1].X else
     if FPoints[1].X > Right then Right := FPoints[1].X;
     if FPoints[1].Y < Top then Top := FPoints[1].Y else
     if FPoints[1].Y > Bottom then Bottom := FPoints[1].Y;
     break;
    end else
     case QStart of
      0: Top := FArcOfs.Y;
      1: Left := FArcOfs.X;
      2: Bottom := FArcOfs.Y + FArcSize.Y;
      3: Right := FArcOfs.X + FArcSize.X;
     end;
    inc(QStart);
    if QStart > 3 then QStart := 0;
   until false;
   // Check center point
   if IsPie then begin
    Center.X := FArcOfs.X + FArcSize.X div 2;
    Center.Y := FArcOfs.Y + FArcSize.Y div 2;
    if Center.X < Left then Left := Center.X else
    if Center.X > Right then Right := Center.X;
    if Center.Y < Top then Top := Center.Y else
    if Center.Y > Bottom then Bottom := Center.Y;
   end;
  end;
 // Check changing
// if EqualRect(NewBounds, FArcBounds) then exit;
 // Offset ArcBounds and resize control
 ChangeEditing := not FEditing;
 if ChangeEditing then FEditing := true;
 try
  OldOfs := FArcOfs;
  if (NewBounds.Left <> 0) or (NewBounds.Top <> 0) then begin
   // Do offset
   dec(FArcOfs.X, NewBounds.Left);
   dec(FArcOfs.Y, NewBounds.Top);
   dec(FPoints[0].X, NewBounds.Left);
   dec(FPoints[0].Y, NewBounds.Top);
   dec(FPoints[1].X, NewBounds.Left);
   dec(FPoints[1].Y, NewBounds.Top);
   OffsetRect(NewBounds, -NewBounds.Left, -NewBounds.Top);
   DoResize := true;
  end else
   DoResize :=
     (WidthProp.Value <> RectWidth(NewBounds)) or
     (HeightProp.Value <> RectHeight(NewBounds));
  if DoResize then begin
   // Do resize
   with DocRect do
    OffsetRect(NewBounds,
      Left - (FArcOfs.X - OldOfs.X), Top - (FArcOfs.Y - OldOfs.Y));
   BeginUpdate;
   try
    DocRect := NewBounds;
    // Owner.History.RecordAction(TPropHistoryAction, FArcAxesProp);
   finally
    EndUpdate;
   end;
  end;
 finally
  if ChangeEditing then FEditing := false;
 end;
end;

procedure TFlexEllipse.SetPointsByAngles;
var HR, WR: double;
    ArcBounds: TRect;

 function CalcPoint(IntAngle: integer): TPoint;
 var PX, PY: double;
     Angle, Coeff: double;
     Scaled180: integer;
 begin
  Scaled180 := 180 * PixelScaleFactor;
  if IntAngle = 0 then with ArcBounds do begin
   Result.X :=  Right;
   Result.Y := (Top + Bottom) div 2;
  end else
  if IntAngle = Scaled180 then with ArcBounds do begin
   Result.X := Left;
   Result.Y := (Top + Bottom) div 2;
  end else begin
   Angle := (IntAngle * pi) / Scaled180;
   Coeff := cos(Angle) / sin(Angle);
   PY := 1 / sqrt(Coeff * Coeff + 1);
   if IntAngle > Scaled180 then PY := -PY;
   PX := PY * Coeff;
   // Save result
   Result.X := ArcBounds.Left + Round(WR + PX * WR);
   Result.Y := ArcBounds.Top + Round(HR - PY * HR);
  end;
 end;

begin
 if FEditing or (Length(FPoints) < 2) then exit;
 FEditing := true;
 try
  if (UpdateCounter = 0) and FArcOldFormatNeedReset then begin
   FArcOldFormat := false;
   FArcOldFormatNeedReset := false;
  end;
  UseAxes :=
    not FArcOldFormat and (FBeginAngleProp.Value <> FEndAngleProp.Value);
  // Define ellipse bounds
  if UseAxes then
   ArcBounds :=
     Rect(FArcOfs.X, FArcOfs.Y, FArcOfs.X + FArcSize.X, FArcOfs.Y + FArcSize.Y)
  else
   ArcBounds := Rect(0, 0, WidthProp.Value, HeightProp.Value);
  WR := RectWidth(ArcBounds) / 2;
  HR := RectHeight(ArcBounds) / 2;
  // Calculate points by angles
  FPoints[0] := CalcPoint(FBeginAngleProp.Value);
  if FBeginAngleProp.Value <> FEndAngleProp.Value then begin
   FPoints[1] := CalcPoint(FEndAngleProp.Value);
   // Update ArcBounds and Control bounds
   UpdateArcBoundsByPoints;
  end else
   FPoints[1] := FPoints[0];
  DoNotify(fnEditPoints);
 finally
  FEditing := false;
 end;
end;

procedure TFlexEllipse.SetAnglesByPoints;
var HR, WR: double;
    Coeff: double;

 function CalcAngle(X, Y: double): integer;
 var Angle: double;
 begin
  Y := Y * Coeff;
  if X = 0 then begin
   if Y > 0
    then Result := 90 * PixelScaleFactor
    else Result := 270 * PixelScaleFactor;
  end else begin
   Angle := 180.0 * ArcTan2(Y, X) / pi;
   if Angle < 0 then Angle := 360.0 + Angle;
   Result := Round(Angle * PixelScaleFactor);
  end;
 end;

begin
 if FEditing then exit;
 BeginUpdate;
 try
  FEditing := true;
  if FUseAxes then begin
   WR := FArcSize.X / 2;
   HR := FArcSize.Y / 2;
  end else begin
   WR := WidthProp.Value / 2;
   HR := HeightProp.Value / 2;
  end;
  if HR = 0
   then Coeff := 0
   else Coeff := WR / HR;
  if FUseAxes then begin
   WR := WR + FArcOfs.X;
   HR := HR + FArcOfs.Y;
  end;
  FBeginAngleProp.Value := CalcAngle(FPoints[0].X - WR, HR - FPoints[0].Y);
  FEndAngleProp.Value   := CalcAngle(FPoints[1].X - WR, HR - FPoints[1].Y);
 finally
  EndUpdate;
  FEditing := false;
 end;
end;

procedure TFlexEllipse.MakeArc(DC: HDC; const R: TRect; const Pt: TPointArray);
begin
 BeginPath(DC);
 with R do begin
  MoveToEx(DC, (Left + Right) div 2, (Top + Bottom) div 2, Nil);
  ArcTo(DC, Left, Top, Right, Bottom, Pt[0].X, Pt[0].Y, Pt[1].X, Pt[1].Y);
  //LineTo(DC, (Left + Right) div 2, (Top + Bottom) div 2);
 end;
 CloseFigure(DC);
 EndPath(DC);
end;

procedure TFlexEllipse.Paint(Canvas: TCanvas; var PaintRect: TRect);
var PrevRgn, ClipRgn: HRGN;
    R, ArcPaintRect: TRect;
    Pt: TPointArray;
    DC: HDC;
    Center, Ofs: TPoint;
    Dist, Coeff: double;
    AP, BP, CP, DP: record X, Y: double; end;

 procedure CanvasSetup;
 begin
  FPenProp.Setup(Canvas, Owner.Scale);
  FBrushProp.Setup(Canvas, Owner.Scale);
 end;

begin
 Pt := Nil;
 if FBeginAngleProp.Value = FEndAngleProp.Value then with Canvas do begin
  // Draw Ellipse
  CanvasSetup;
  if FBrushProp.IsPaintAlternate then begin
   PrevRgn := 0;
   ClipRgn := CreateEllipseRegion(PaintRect);
   try
    PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
    FBrushProp.PaintAlternate(Canvas, PaintRect, Owner.PaintRect,
      Owner.Scale, Owner.UseImageClipTransparent);
   finally
    SelectClipRgn(Canvas.Handle, PrevRgn);
    DeleteObject(PrevRgn);
    DeleteObject(ClipRgn);
   end;
   CanvasSetup;
  end;
  R := PaintRect;
  if FPenProp.ActiveWidth = 0 then begin
   inc(R.Right);
   inc(R.Bottom);
  end;
  with R do Ellipse(Left, Top, Right, Bottom)
 end else begin
  // Draw Arc
  BrushProp.Setup(Canvas, Owner.Scale);
  PenProp.Setup(Canvas, Owner.Scale);
  with Canvas, ArcPaintRect do begin
   // Get paint points
   Pt := GetTransformPoints(PaintRect.Left, PaintRect.Top, Owner.Scale);
   // Calculate real paint rect (for whole ellipse)
   if UseAxes then begin
    TopLeft := FArcOfs;
    Right := Left + FArcSize.X;
    Bottom := Top + FArcSize.Y;
    with DocRect do OffsetRect(ArcPaintRect, Left, Top);
    Owner.TransformRect(ArcPaintRect);
    with Self.PaintRect do begin
     Ofs.X := PaintRect.Left - Left;
     Ofs.Y := PaintRect.Top - Top;
    end;
    OffsetRect(ArcPaintRect, Ofs.X, Ofs.Y);
   end else
    ArcPaintRect := PaintRect;
   // Test points distantce to avoid error
   if (Abs(Pt[0].X - Pt[1].X) < 2) and (Abs(Pt[0].Y - Pt[1].Y) < 2) and
     ((FPoints[0].X <> FPoints[1].X) or (FPoints[0].Y <> FPoints[1].Y)) then begin
    // Enlarge points distnace from ellipse center
    Dist := 2{pixels} * 100 * PixelScaleFactor / Owner.Scale;
    // Get initial points with coordinates system in ellipse center
    Center.X := FArcOfs.X + FArcSize.X div 2;
    Center.Y := FArcOfs.Y + FArcSize.Y div 2;
    AP.X := FPoints[0].X - Center.X;
    AP.Y := FPoints[0].Y - Center.Y;
    BP.X := FPoints[1].X - Center.X;
    BP.Y := FPoints[1].Y - Center.Y;
    // Select max distance
    if Abs(BP.X - AP.X) > Abs(BP.Y - AP.Y) then begin
     // Horizontal distance
     if BP.X < AP.X then Dist := -Dist;
     DP.X := (Dist * BP.X) / (BP.X - AP.X);
     CP.X := DP.X - Dist;
     if AP.X = 0
      then Coeff := Abs(DP.X / BP.X)
      else Coeff := Abs(CP.X / AP.X);
     DP.Y := BP.Y * Coeff;
     CP.Y := AP.Y * Coeff;
    end else begin
     // Verical distance
     if BP.Y < AP.Y then Dist := -Dist;
     DP.Y := (Dist * BP.Y) / (BP.Y - AP.Y);
     CP.Y := DP.Y - Dist;
     if AP.Y = 0
      then Coeff := Abs(DP.Y / BP.Y)
      else Coeff := Abs(CP.Y / AP.Y);
     DP.X := BP.X * Coeff;
     CP.X := AP.X * Coeff;
    end;
    // Convert to paint coords
    with PaintRect do begin
     Coeff := Owner.Scale / (100 * PixelScaleFactor);
     Pt[0].X := Left + Round((CP.X + Center.X) * Coeff);
     Pt[0].Y := Top + Round((CP.Y + Center.Y) * Coeff);
     Pt[1].X := Left + Round((DP.X + Center.X) * Coeff);
     Pt[1].Y := Top + Round((DP.Y + Center.Y) * Coeff);
    end;
   end;
   CanvasSetup;
   if BrushProp.IsPaintAlternate then begin
    PrevRgn := 0;
    // Make arc clip path
    DC := Canvas.Handle;
    MakeArc(DC, ArcPaintRect, Pt);
    ClipRgn := PathToRegion(DC);
    try
     PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
     FBrushProp.PaintAlternate(Canvas, PaintRect, Owner.PaintRect,
       Owner.Scale, Owner.UseImageClipTransparent);
    finally
     SelectClipRgn(Canvas.Handle, PrevRgn);
     DeleteObject(ClipRgn);
     DeleteObject(PrevRgn);
    end;
    // Stroke path
    CanvasSetup;
    DC := Canvas.Handle;
    MakeArc(DC, ArcPaintRect, Pt);
    StrokePath(DC);
   end else
   if IsPie then begin
    // Fill standard brush
    DC := Canvas.Handle;
    MakeArc(DC, ArcPaintRect, Pt);
    //FillPath(DC);
    StrokeAndFillPath(DC);
   end else
    // Non closed arc
    Canvas.Arc(Left, Top, Right, Bottom, Pt[0].X, Pt[0].Y, Pt[1].X, Pt[1].Y);
  end;
 end;
end;

function TFlexEllipse.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
var PenWidth: integer;
    PenStyle: TPenStyle;
    IsGeometricPen: boolean;
    InflateSize: integer;
begin
 Result := Rect(RefreshX, RefreshY,
                RefreshX + WidthProp.Value, RefreshY + HeightProp.Value);
 FPenProp.GetPaintData(PenWidth, PenStyle, IsGeometricPen, Owner.Scale);
 if UseAxes then
  // Fix GDI arc paiting bug
  // (sector lines always draws as psSolid, not psInsideFrame)
  InflateSize := FPenProp.Width + 2*PixelScaleFactor
 else
 if (PenStyle <> psInsideFrame) and (PenStyle <> psClear) and
    (PenWidth > 1) then
  InflateSize := FPenProp.Width div 2 + PixelScaleFactor
 else
  InflateSize := 0;
 if InflateSize <> 0 then
  InflateRect(Result, InflateSize, InflateSize);
end;

procedure TFlexEllipse.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string);
begin
 if Prop = FBeginAngleProp then
  IsStored := FBeginAngleProp.Value <> 0
 else
 if Prop = FEndAngleProp then
  IsStored := FEndAngleProp.Value <> 0
 else
 if Prop = FArcAxesProp then
  IsStored := UseAxes and not FArcOldFormat
 else
  inherited;
end;

procedure TFlexEllipse.StartResizing(const SelRect: TRect);
begin
 inherited;
 FArcResizingBounds.TopLeft := FArcOfs;
 FArcResizingBounds.BottomRight := FArcSize;
end;

procedure TFlexEllipse.DoNotify(Notify: TFlexNotify);
var R, WorkRect: TRect;
    ScaleX, ScaleY: Double;
begin
 case Notify of
  fnRect:
    if not FEditing and (UpdateCounter = 0) then begin
     {if FArcOldFormatNeedReset then begin
      FArcOldFormat := false;
      FArcOldFormatNeedReset := false;
      UseAxes := FBeginAngleProp.Value <> FEndAngleProp.Value;
      if UseAxes then begin
       FArcResizingBounds.TopLeft := FArcOfs;
       FArcResizingBounds.BottomRight := FArcSize;
      end;
     end; }
     if FUseAxes{ and
        (Owner.History.InProcessSource <> Self) }then begin
      // Calculate scale coeffs
      R := DocRect;
      if fsResizing in State
       then WorkRect := FResizingRect
       else WorkRect := FSavedDocRect;
      if not EqualRect(R, WorkRect) then begin
       with WorkRect do begin
        if Right - Left > 0
         then ScaleX := (R.Right - R.Left) / (Right - Left)
         else ScaleX := 0;
        if Bottom - Top > 0
         then ScaleY := (R.Bottom - R.Top) / (Bottom - Top)
         else ScaleY := 0;
       end;
       // Scale arc axes
       if (ScaleX <> 1.0) or (ScaleY <> 1.0) then
        if fsResizing in State then begin
         FArcOfs.X := Round(FArcResizingBounds.Left * ScaleX);
         FArcOfs.Y := Round(FArcResizingBounds.Top * ScaleY);
         FArcSize.X := Round(FArcResizingBounds.Right * ScaleX);
         FArcSize.Y := Round(FArcResizingBounds.Bottom * ScaleY);
        end else begin
         FArcOfs.X := Round(FArcOfs.X * ScaleX);
         FArcOfs.Y := Round(FArcOfs.Y * ScaleY);
         FArcSize.X := Round(FArcSize.X * ScaleX);
         FArcSize.Y := Round(FArcSize.Y * ScaleY);
        end;
      end;
      FArcResizing := true;
      try
       SetPointsByAngles;
      finally
       FArcResizing := false;
      end;
     end;
    end;
  fnLoaded:
    // Reset old format after loading
    FArcOldFormatNeedReset := true;
 end;
 inherited;
end;

function TFlexEllipse.IsPropUpdatePoints(Prop: TCustomProp): boolean;
begin
 Result :=
   // Width and Height test
   ( ((Prop = WidthProp) or (Prop = HeightProp)) and not FUseAxes
    //(UpdateCounter = 0) and (fsLoading in State);
   ) or
   // Angles test
   ( (Prop = FBeginAngleProp) or (Prop = FEndAngleProp) )
   or
   // Brush test
   ( (Prop = BrushProp) and UseAxes );
end;

function TFlexEllipse.GetIsNeedHistoryPointsAction: boolean;
begin
 Result := not FUseAxes ;
end;

procedure TFlexEllipse.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 inherited;
 if FArcOldFormatNeedCheck and (Prop.Owner.DataMode = plmLoading) then begin
  // Initially set arc old format
  FArcOldFormat := true;
  FArcOldFormatNeedCheck := false;
 end;
 if FEditing then exit;
 if Prop = FArcAxesProp then
  SetArcAxes(FArcAxesProp.SavedValue)
 else
 if IsPropUpdatePoints(Prop)
   { and (Owner.History.InProcessSource <> Self)} then begin
  if (Prop.Owner.DataMode <> plmLoading) and
     ( (Prop = FBeginAngleProp) or (Prop = FEndAngleProp) ) then
   FArcOldFormat := false;
  SetPointsByAngles;
 end;
end;

procedure TFlexEllipse.PropHistoryAction(Sender: TObject; Prop: TCustomProp;
  var ActionClass: THistoryActionClass);
begin
 if (Prop = FBeginAngleProp) or (Prop = FEndAngleProp) or (
    FUseAxes and ((Prop = WidthProp) or (Prop = HeightProp) ) ) then begin
  ActionClass := Nil;
  Owner.History.RecordAction(TPropHistoryAction, FArcAxesProp);
 end;
end;

// TFlexPicture ///////////////////////////////////////////////////////////////

procedure TFlexPicture.ControlCreate;
begin
 Width := 1;
 Height := 1;
 inherited;
 Visible := True;
end;

procedure TFlexPicture.CreateProperties;
begin
 inherited;
 FAutoSizeProp := TBoolProp.Create(Props, 'AutoSize');
 FPictureProp := TPictureProp.Create(Props, 'Picture');
 FFrameIndexProp := TIntProp.Create(Props, 'FrameIndex');
end;

class function TFlexPicture.CursorInCreate: TCursor;
begin
 Result := crCreatePicCursor;
end;

function TFlexPicture.IsPointInside(PaintX, PaintY: integer): boolean;
begin
 if Owner.InDesign or FPictureProp.IsLoaded
  then Result := inherited IsPointInside(PaintX, PaintY)
  else Result := false;
end;

function TFlexPicture.GetDefaultLinkPoint(Index: integer): TPoint;
var Half: TPoint;
    R: TRect;
begin
 R := Rect(0, 0, WidthProp.Value-PixelScaleFactor, HeightProp.Value-PixelScaleFactor);
 Half.X := R.Right div 2;
 Half.Y := R.Bottom div 2;
 case Index of
  0: Result := R.TopLeft;
  1: Result := Point(Half.X, 0);
  2: Result := Point(R.Right, 0);
  3: Result := Point(R.Right, Half.Y);
  4: Result := R.BottomRight;
  5: Result := Point(Half.X, R.Bottom);
  6: Result := Point(0, R.Bottom);
  7: Result := Point(0, Half.Y);
  8: Result := Point(Half.X, Half.Y);
 end;
end;

function TFlexPicture.GetDefaultLinkPointCount: integer;
begin
 Result := 9;
end;

procedure TFlexPicture.PaintAll(Canvas: TCanvas; PaintX, PaintY: integer);
begin
 if FPictureProp.FastBuffer then FPaintAlphaBufferMode := amRequired;
 inherited;
end;

procedure TFlexPicture.Paint(Canvas: TCanvas; var PaintRect: TRect);
begin
 if FPictureProp.IsLoaded then
  FPictureProp.Draw(Canvas, PaintRect, FFrameIndexProp.Value,
    Owner.UseImageClipTransparent or (TransparencyProp.Value > 0),
    FPaintAlphaBuffer)
 else
 if not Owner.PaintForExport and Owner.InDesign then
  Owner.PaintEmptyPicture(Canvas, Self);
end;

procedure TFlexPicture.PropChanged(Sender: TObject; Prop: TCustomProp);

 procedure AdjustSize;
 var Size: TRect;
 begin
  Size := FPictureProp.CellSizeRect;
  Size.Right := ScalePixels(Size.Right);
  Size.Bottom := ScalePixels(Size.Bottom);
  if IsRectEmpty(Size) then
   Exit;
  if FAutoSizeProp.Value then begin
   with WidthProp do Style := Style - [psReadOnly];
   with HeightProp do Style := Style - [psReadOnly];
  end;
  WidthProp.Value := Size.Right;
  HeightProp.Value := Size.Bottom;
  if FAutoSizeProp.Value then begin
   with WidthProp do Style := Style + [psReadOnly];
   with HeightProp do Style := Style + [psReadOnly];
  end;
 end;

begin
 inherited;
 if Prop = FAutoSizeProp then begin
  if FAutoSizeProp.Value then
   AdjustSize
  else begin
   with WidthProp do Style := Style - [psReadOnly];
   with HeightProp do Style := Style - [psReadOnly];
  end;
 end else
 if (Prop = FPictureProp) and FPictureProp.IsLoaded and FAutoSizeProp.Value then
   AdjustSize;
end;

procedure TFlexPicture.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = FAutoSizeProp then
  IsStored := FAutoSizeProp.Value
 else
 if Prop = FFrameIndexProp then
  IsStored := FFrameIndexProp.Value <> 0
 else
  inherited;
end;

// TFlexCurve /////////////////////////////////////////////////////////////

procedure TFlexCurve.ControlCreate;
begin
 FCurveInfoChanged := true;
 SetLength(FPoints, 2);
 FPoints[0] := Point(0, 0);
 FPoints[1] := Point(0, 0);
 SetLength(FPointTypes, 2);
 FPointTypes[0] := ptNode; //PT_MOVETO;
 FPointTypes[1] := ptEndNode; //PT_LINETO; 
 FZeroPoints := true;
 inherited;
 Visible := True;
end;

procedure TFlexCurve.CreateProperties;
begin
 inherited;
 FBeginCapProp := TLineCapProp.Create(Props, 'BeginCap');
 FEndCapProp := TLineCapProp.Create(Props, 'EndCap');
 FIsSolidProp := TBoolProp.Create(Props, 'IsSolid');
 FPointsProp := TDataProp.Create(Props, 'Points');
 FPointsProp.OnGetPropData := GetPointsData;
 FPointsProp.OnSetPropData := SetPointsData;
 FPointsProp.Style :=  FPointsProp.Style - [ psVisible ];
 FPathPointsProp := TDataProp.Create(Props, 'PathPoints');
 FPathPointsProp.OnGetPropData := GetPathPointsData;
 FPathPointsProp.OnSetPropData := SetPathPointsData;
 FPathPointsProp.Style :=  FPathPointsProp.Style - [ psVisible ]; 
end;

procedure TFlexCurve.ControlDestroy;
begin
 inherited;
 SetLength(FPoints, 0);
 SetLength(FResizePoints, 0);
end;

function TFlexCurve.EndUpdate: boolean;
begin
 Result := inherited EndUpdate;
 if Result and not FChanging then PointsChanged;
end;

class function TFlexCurve.CursorInCreate: TCursor;
begin
 Result := crCreatePolyCursor;
end;

function TFlexCurve.GetAnchorPoint: TPoint;
var R: TRect;
begin
 R := DocRect;
 Result.X := R.Left;
 Result.Y := R.Top;
 if Length(FPoints) > 0 then begin
  inc(Result.X, FPoints[0].X);
  inc(Result.Y, FPoints[0].Y);
 end;
 Owner.TransformPointIndirect(Result);
end;

function TFlexCurve.GetDefaultLinkPoint(Index: integer): TPoint;
begin
 Result := Nodes[Index];
end;

function TFlexCurve.GetDefaultLinkPointCount: integer;
begin
 Result := NodeCount;
end;

function TFlexCurve.GetIsPointsClosed: boolean;
begin
 Result := FIsSolidProp.Value;
end;

procedure TFlexCurve.SetIsPointsClosed(Value: boolean);
begin
 FIsSolidProp.Value := Value;
end;

function TFlexCurve.GetPoint(Index: integer): TPoint;
begin
 Result := FPoints[Index];
end;

procedure TFlexCurve.SetPoint(Index: integer; const Value: TPoint);
begin
 if (Assigned(Layer) and not Layer.Moveable) or
   ((Value.X = FPoints[Index].X) and (Value.Y = FPoints[Index].Y)) then exit;
 RecordPointsAction;
 FPoints[Index] := Value;
 PointsChanged;
end;

function TFlexCurve.GetPointType(Index: integer): TPointType;
begin
 Result := FPointTypes[Index];
end;

procedure TFlexCurve.SetPointType(Index: integer; const Value: TPointType);
begin
 if Assigned(Layer) and not Layer.Moveable then exit;
 RecordPointsAction;
 FPointTypes[Index] := Value;
 PointsChanged;
end;

function TFlexCurve.GetPointCount: integer;
begin
 Result := Length(FPoints);
end;

function TFlexCurve.IsPointInside(PaintX, PaintY: integer): boolean;
var Pt: TPoint;
    ActWidth, PenWidth: integer;
begin
 Pt.x := PaintX;
 Pt.y := PaintY;
 Pt := OwnerToClient(Pt);
 ActWidth := FPenProp.ActiveWidth;
 PenWidth := ActWidth div 2 + UnscaleValue(SelectionThreshold, Owner.Scale);
 Result := (Pt.x >= -PenWidth) and (Pt.x <= WidthProp.Value + PenWidth) and
           (Pt.y >= -PenWidth) and (Pt.y <= HeightProp.Value + PenWidth);
 if not Result then exit;
 Result := PointOnPath(FPoints, FPointTypes, Pt, FPenProp.ActiveWidth > 0,
   not FBrushProp.IsClear or (Assigned(Owner) and Owner.SelectAsFilled),
   PenWidth, Nil, PointsInfo);
end;

procedure TFlexCurve.DoNotify(Notify: TFlexNotify);
var DesignInfo: TFlexCreateInDesignInfo;
    Delta: TPoint;
    R: TRect;
    Size: TPoint;
    ScaleX, ScaleY: Double;
    i: integer;
begin
 case Notify of
  fnRect:
    if Owner.History.InProcessSource <> Self then
    if (Length(FPoints) > 0) and (UpdateCounter = 0) and
       not (fsLoading in State) and not FChanging then
     if FZeroPoints and
        (WidthProp.Value > 0) and (HeightProp.Value > 0) then
      with DesignInfo do begin
       // Try change control last point
       IsPointEdit := false;
       PointEditIndex := Length(FPoints) - 1;
       CreateInDesign(DesignInfo);
       if IsPointEdit and (PointEditIndex >= 0) and
         (PointEditIndex < Length(FPoints)) and
         (FPoints[PointEditIndex].X = 0) and
         (FPoints[PointEditIndex].Y = 0) then begin
        // Just move DesignInfo.PointEditIndex point
        Delta.X := WidthProp.Value;
        Delta.Y := HeightProp.Value;
        MovePathPoints(PointEditIndex, Delta, Nil);
       end;
       FZeroPoints := false;
       {FLastDocRect := DocRect;}
      end
     else
     if not FZeroPoints then begin
      R := DocRect;
      if fsResizing in State then begin
       with FResizingRect do begin
        if Right - Left > 0
         then ScaleX := (R.Right - R.Left) / (Right - Left)
         else ScaleX := 0;
        if Bottom - Top > 0
         then ScaleY := (R.Bottom - R.Top) / (Bottom - Top)
         else ScaleY := 0;
       end;
       //Ofs.X := 0; //R.Left - FSavedDocRect.Left;
       //Ofs.Y := 0; //R.Top - FSavedDocRect.Top;
       for i:=0 to High(FPoints) do begin
        FPoints[i].X := Round(FResizePoints[i].X * ScaleX);
        FPoints[i].Y := Round(FResizePoints[i].Y * ScaleY);
       end;
       PointsChanged;
      end else 
      if not IsRectEmpty(FSavedDocRect) and
         not EqualRect(R, FSavedDocRect) then begin
       Size.X := FSavedDocRect.Right - FSavedDocRect.Left;
       Size.Y := FSavedDocRect.Bottom - FSavedDocRect.Top;
       if Size.X <> 0
        then ScaleX := (R.Right - R.Left) / Size.X
        else ScaleX := 0;
       if Size.Y <> 0
        then ScaleY := (R.Bottom - R.Top) / Size.Y
        else ScaleY := 0;
       if (ScaleX <> 1.0) or (ScaleY <> 1.0) then begin
        for i:=0 to High(FPoints) do begin
         FPoints[i].X := Round(FPoints[i].X * ScaleX);
         FPoints[i].Y := Round(FPoints[i].Y * ScaleY);
        end;
        PointsChanged;
       end;
      end;
     end;
 end;
 inherited;
end;

procedure TFlexCurve.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 inherited;
 if FChanging or (Length(FPoints) = 0) then exit;
 if Prop = IsSolidProp then with PointsInfo^ do
  if Length(Figures) > 0 then with Figures[High(Figures)] do begin
   Invalidate;
   if IsClosed
    then FPointTypes[LastNode] := ptEndNode
    else FPointTypes[LastNode] := ptEndNodeClose;
   PointsChanged;
  end;
end;

procedure TFlexCurve.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = FPointsProp then
  //IsStored := Length(FPoints) > 0
  IsStored := false // not supported 
 else
 if Prop = FPathPointsProp then
  IsStored := Length(FPoints) > 0
 else
 if Prop = FIsSolidProp then
  IsStored := FIsSolidProp.Value 
 else
  inherited;
end;

procedure TFlexCurve.GetPointsData(Sender: TObject; var Value: Variant);
var Size: integer;
    VArray: pointer;
begin
 Size := Length(FPoints) * SizeOf(FPoints[0]);
 if Size = 0 then begin
  VarClear(Value);
  exit;
 end;
 Value := VarArrayCreate([0, Size-1], varByte);
 VArray := VarArrayLock(Value);
 try
  Move(FPoints[0], VArray^, Size);
 finally
  VarArrayUnlock(Value);
 end;
end;

procedure TFlexCurve.SetPointsData(Sender: TObject; var Value: Variant);
var Size: integer;
    VArray: pointer;
    i, Count: integer;
begin
 if VarIsEmpty(Value) or VarIsNull(Value) or
    (VarType(Value) and varArray = 0) then exit;
 Size := VarArrayHighBound(Value, 1)+1;
 if Size mod SizeOf(FPoints[0]) <> 0 then exit;
 SetLength(FPoints, Size div SizeOf(FPoints[0]));
 VArray := VarArrayLock(Value);
 try
  Move(Varray^, FPoints[0], Size);
 finally
  VarArrayUnlock(Value);
 end;
 Count := Length(FPoints);
 SetLength(FPointTypes, Count);
 if Count > 0 then begin
  for i:=0 to Count-2 do FPointTypes[i] := ptNode;
  if IsPointsClosed
   then FPointTypes[Count-1] := ptEndNodeClose
   else FPointTypes[Count-1] := ptEndNode;
 end;
 FZeroPoints := false;
 PointsChanged;
end;

procedure TFlexCurve.GetPathPointsData(Sender: TObject; var Value: Variant);
var PointsSize, TypesSize, Size, Count: integer;
    VArray: pointer;
begin
 PointsSize := Length(FPoints)*(SizeOf(FPoints[0]));
 TypesSize := Length(FPointTypes)*(SizeOf(FPointTypes[0]));
 Size := PointsSize + TypesSize;
 if Size = 0 then begin
  VarClear(Value);
  exit;
 end;
 Value := VarArrayCreate([0, Size+SizeOf(Count) -1], varByte);
 VArray := VarArrayLock(Value);
 try
  // Save points count
  Count := Length(FPoints);
  Move(Count, VArray^, SizeOf(Count));
  VArray := pointer(PAnsiChar(VArray) + SizeOf(Count));
  // Save point coordinates
  Move(FPoints[0], VArray^, PointsSize);
  VArray := pointer(PAnsiChar(VArray) + PointsSize);
  // Save point types
  Move(FPointTypes[0], VArray^, TypesSize);
 finally
  VarArrayUnlock(Value);
 end;
end;

procedure TFlexCurve.SetPathPointsData(Sender: TObject; var Value: Variant);
var PointsSize, TypesSize, Size, Count: integer;
    VArray: pointer;
begin
 if VarIsEmpty(Value) or VarIsNull(Value) or
    (VarType(Value) and varArray = 0) then exit;
 Size := VarArrayHighBound(Value, 1)+1;
 if Size < SizeOf(Integer) then exit;
 VArray := VarArrayLock(Value);
 try
  // Read count
  Move(Varray^, Count, SizeOf(Count));
  VArray := pointer(PAnsiChar(VArray) + SizeOf(Count));
  // Calculate sizes and check data size
  PointsSize := Count*SizeOf(FPoints[0]);
  TypesSize := Count*SizeOf(FPointTypes[0]);
  if Size <> SizeOf(Count) + PointsSize + TypesSize then exit;
  // Read point coordinates
  if PointsSize > 0 then begin
   SetLength(FPoints, Count);
   Move(VArray^, FPoints[0], PointsSize);
   VArray := pointer(PAnsiChar(VArray) + PointsSize);
  end else
   SetLength(FPoints, 0);
  // Read point types
  if TypesSize > 0 then begin
   SetLength(FPointTypes, Count);
   Move(VArray^, FPointTypes[0], TypesSize);
  end else
   SetLength(FPointTypes, 0);
 finally
  VarArrayUnlock(Value);
 end;
 FZeroPoints := false;
 PointsChanged;
end;

function TFlexCurve.GetPointsInfo: PPathInfo;
begin
 Result := @FCurveInfo;
 if not FCurveInfoChanged and (FCurveInfo.PointCount = Length(FPoints)) then exit;
 GetPathInfo(FPoints, FPointTypes, FCurveInfo);
 FCurveInfoChanged := false;  
end;

procedure TFlexCurve.StartResizing;
begin
 inherited;
 SetLength(FResizePoints, Length(FPoints));
 Move(FPoints[0], FResizePoints[0], Length(FPoints)*SizeOf(FPoints[0]));
end;

procedure TFlexCurve.FinishResizing;
begin
 inherited;
 SetLength(FResizePoints, 0);
end;
 {
procedure TFlexCurve.SetDocRect(Value: TRect);
var R: TRect;
    ScaleX, ScaleY: Double;
    Ofs: TPoint;
    i: integer;
begin
{ if fsResizing in State then begin
  R := DocRect;
  with FResizingRect do begin
   if Right - Left > 0
    then ScaleX := (Value.Right - Value.Left) / (Right - Left)
    else ScaleX := 0;
   if Bottom - Top > 0
    then ScaleY := (Value.Bottom - Value.Top) / (Bottom - Top)
    else ScaleY := 0;
  end;
  Ofs.X := Value.Left - R.Left;
  Ofs.Y := Value.Top - R.Top;
  for i:=0 to High(FPoints) do begin
   FPoints[i].X := Round(FResizePoints[i].X * ScaleX) + Ofs.X;
   FPoints[i].Y := Round(FResizePoints[i].Y * ScaleY) + Ofs.Y;
  end;
  PointsChanged;
 end else
  inherited;
end;
 }
function TFlexCurve.RecordPointsAction: TPointsHistoryAction;
{var ActionUndo: TPointsHistoryActionInfo;
    R: TRect;
    i: integer; }
begin
 Result := Nil;
 if not Assigned(Owner) then exit;
 Result := TPointsHistoryAction(
   Owner.History.RecordAction(TPointsHistoryAction, Self) );
 {if Assigned(Result) and (fsResizing in State) then begin
  // Get undo info
  ActionUndo := Result.UndoInfo;
  // Copy original points
  SetLength(ActionUndo.Points, Length(FResizePoints));
  for i:=0 to Length(FResizePoints)-1 do
   ActionUndo.Points[i] := FResizePoints[i];
  // Set original size and pos
  R := FResizingRect;
  OffsetRect(R, FResizingTopLeft.X, FResizingTopLeft.Y);
  ActionUndo.DocRect := R;
  // Set undo info
  Result.UndoInfo := ActionUndo;
 end; }
end;

procedure TFlexCurve.ControlTranslate(const TranslateInfo: TTranslateInfo);
var NewTranslate: TTranslateInfo;
begin
 BeginTranslate;
 try
  // Translate link points
  LinkPointsTranslate(TranslateInfo);
  // Translate curve points
  if Length(FPoints) > 0 then begin
   RecordPointsAction;
   NewTranslate := TranslateInfo;
   with DocRect do begin
    dec(NewTranslate.Center.x, Left);
    dec(NewTranslate.Center.y, Top);
   end;
   TranslatePoints(FPoints, NewTranslate);
   PointsChanged;
  end;
  // Translate brush
  FBrushProp.Translate(TranslateInfo);
 finally
  EndTranslate;
 end;
end;

procedure TFlexCurve.CreateInDesign(var Info: TFlexCreateInDesignInfo);
begin
 // Do point edit after control created in design mode
 inherited;
 Info.IsPointEdit := true;
 Info.PointEditIndex := Length(FPoints) - 1;
 Info.IsContinueAvail := true;
 // Leave Info.PointContinueIndex as default
end;

procedure TFlexCurve.MirrorInResize(HMirror, VMirror: boolean);
var i, H, W: integer;
begin
 //RecordPointsAction;
 inherited;
 H := RectHeight(FResizingRect);
 W := RectWidth(FResizingRect);
 // Mirror FResizePoints
 for i:=0 to Length(FResizePoints)-1 do begin
  if HMirror then FResizePoints[i].X := W - FResizePoints[i].X;
  if VMirror then FResizePoints[i].Y := H - FResizePoints[i].Y;
 end;
end;

function  TFlexCurve.MovePathPoints(PointIndex: integer; var Delta: TPoint;
  Selected: TSelectedArray; Smooth: boolean = false;
  Symmetric: boolean = false): boolean;
begin
 RecordPointsAction;
 Result := inherited MovePathPoints(PointIndex, Delta,
   Selected, Smooth, Symmetric);
end;

function  TFlexCurve.MovePathSegment(FirstIndex, NextIndex: integer;
  var Delta: TPoint; const SegmentCurvePos: double): boolean;
begin
 RecordPointsAction;
 Result := inherited MovePathSegment(FirstIndex, NextIndex, Delta,
   SegmentCurvePos);
end;

procedure TFlexCurve.PointsChanged;
var Bounds: TRect;
    i: integer;
begin
 FCurveInfoChanged := true;
 if (UpdateCounter > 0) or FChanging or Owner.IsLoading then exit;
 FChanging := true;
 try
  if Owner.History.InProcessSource <> Self then begin
   if Length(FPoints) = 0 then begin
    Width := 0;
    Height := 0;
    exit;
   end;
   Bounds.Left := FPoints[0].X;
   Bounds.Top := FPoints[0].Y;
   Bounds.Right := FPoints[0].X;
   Bounds.Bottom := FPoints[0].Y;
   if PointsInfo.IsCurve then begin
    // Calc curve bounds
    CalcPath(FPoints, FPointTypes, Bounds, PointsInfo);
   end else
   with Bounds do
    // Calc polyline bounds
    for i:=1 to High(FPoints) do begin
     if Left > FPoints[i].x then Left := FPoints[i].x else
     if Right < FPoints[i].x then Right := FPoints[i].x;
     if Top > FPoints[i].y then Top := FPoints[i].y else
     if Bottom < FPoints[i].y then Bottom := FPoints[i].y;
    end;
   // Check bounds
   if Bounds.Right = Bounds.Left then inc(Bounds.Right);
   if Bounds.Bottom = Bounds.Top then inc(Bounds.Bottom);
   // Offset curve points
   if (Bounds.Left <> 0) or (Bounds.Top <> 0) then
    for i:=0 to High(FPoints) do begin
     dec(FPoints[i].X, Bounds.Left);
     dec(FPoints[i].Y, Bounds.Top);
     if (FPoints[i].X <> 0) or (FPoints[i].Y <> 0) then FZeroPoints := false;
    end;
   // Change curve control position and size
   BeginUpdate;
   try
    LeftProp.Value := Left + Bounds.Left;
    TopProp.Value := Top + Bounds.Top;
    WidthProp.Value := Bounds.Right - Bounds.Left;
    HeightProp.Value := Bounds.Bottom - Bounds.Top;
   finally
    EndUpdate;
   end;
  end;
  // FLastDocRect := DocRect;
  // Define IsSolidProp value
  with PointsInfo^ do
   IsSolidProp.Value :=
     (Length(Figures) > 0) and Figures[High(Figures)].IsClosed;
 finally
  FChanging := False;
 end;
 DoNotify(fnEditPoints);
end;

function TFlexCurve.FlattenPoints(const Curvature: single): boolean;
begin
 RecordPointsAction;
 Result := FlattenPath(FPoints, FPointTypes, Curvature, PointsInfo);
 if Result then PointsChanged;
end;

function TFlexCurve.FindNearestPoint(const Point: TPoint;
  var Nearest: TNearestPoint): boolean;
begin
 PointOnPath(FPoints, FPointTypes, Point, True, False, 0, @Nearest,
   PointsInfo);
 Result := true;
end;

function TFlexCurve.FindNearestPathSegment(const Point: TPoint;
  var FirstIndex, NextIndex: integer; Nearest: PNearestPoint = Nil;
  ForInsert: boolean = true; ForSelect: boolean = false): boolean;
var
  Dist: single;
begin
 Result := FlexPath.FindNearestPathSegment(FPoints, FPointTypes, Point,
  FirstIndex, NextIndex, Nearest, PointsInfo, ForInsert);
 if Result and ForSelect and Assigned(Nearest) then begin
   Dist := (FPenProp.ActiveWidth div 2) +
     UnScaleValue(SelectionThreshold, Owner.Scale);
   Result := Nearest.MinSqrDist <= Dist*Dist;
 end;
end;

function TFlexCurve.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
var PenWidth: integer;
    PenStyle: TPenStyle;
    IsGeometricPen: boolean;
    Inflate_: integer;
    MaxSize: integer;
    Size1, Size2: integer;
    Info: TLineCapInfo;
begin
 Inflate_ := 0;
 if Assigned(FPenProp) then begin
  FPenProp.GetPaintData(PenWidth, PenStyle, IsGeometricPen);
  if PenWidth > 0 then begin
   PenWidth := FPenProp.Width;
   if IsGeometricPen and (FPenProp.Join = pjMiter) then
    // Lets miter limit equals 10.0
    Inflate_ := 5 * PenWidth
   else
    Inflate_ := PenWidth + 2*PixelScaleFactor;
  end else
   Inflate_ := PixelScaleFactor;
 end else
  PenWidth := 0;
 if (FBeginCapProp.CapStyle <> psNoCap) or
    (FEndCapProp.CapStyle <> psNoCap) then begin
  MaxSize := 0;
  // Check begin cap
  if GetLineCapInfo(FBeginCapProp.CapStyle, Info,
    FBeginCapProp.GetActiveSize(PenWidth)) then begin
   Size1 := RectWidth(Info.Bounds);
   Size2 := RectHeight(Info.Bounds);
   if Size1 > MaxSize then MaxSize := Size1;
   if Size2 > MaxSize then MaxSize := Size2;
  end;
  // Check end cap
  if GetLineCapInfo(FEndCapProp.CapStyle, Info,
    FEndCapProp.GetActiveSize(PenWidth)) then begin
   Size1 := RectWidth(Info.Bounds);
   Size2 := RectHeight(Info.Bounds);
   if Size1 > MaxSize then MaxSize := Size1;
   if Size2 > MaxSize then MaxSize := Size2;
  end;
  inc(Inflate_, MaxSize);
 end;
 with Result do begin
  Left := RefreshX - Inflate_;
  Top := RefreshY - Inflate_;
  Right := RefreshX + WidthProp.Value + Inflate_;
  Bottom := RefreshY + HeightProp.Value + Inflate_;
 end;
end;

procedure TFlexCurve.Paint(Canvas: TCanvas; var PaintRect: TRect);
var ScrPoints: TPointArray;
    PrevRgn, ClipRgn: HRGN;
    DC: HDC;
    SavedDC: integer;
    Complete, IsFilled: boolean;
    PenWidth: integer;
    PenColor: TColor;
    BeginCap, EndCap: TRenderCapParams;
begin
 ScrPoints := GetTransformPoints(PaintRect.Left, PaintRect.Top, Owner.Scale);
 if Length(ScrPoints) = 0 then exit;
 FPenProp.Setup(Canvas, Owner.Scale);
 PenWidth := Canvas.Pen.Width;
 PenColor := Canvas.Pen.Color;
 FBrushProp.Setup(Canvas, Owner.Scale);
 if PointsInfo.IsCurve or (Length(PointsInfo.Figures) > 1) then begin
  // Draw curve using CreatePath
  DC := Canvas.Handle;
  if not FBrushProp.IsClear then begin
   IsFilled := CreatePath(DC, ScrPoints, FPointTypes, True, False,
     Complete, Owner.UseOriginalBezier, PointsInfo);
   if FBrushProp.IsPaintAlternate then begin
    SavedDC := 0; 
    if IsFilled then begin
     if Complete then SavedDC := SaveDC(DC);
     PrevRgn := 0;
     try
      PrevRgn := IntersectClipPath(Canvas.Handle);
      FBrushProp.PaintAlternate(Canvas, PaintRect, Owner.PaintRect,
        Owner.Scale, Owner.UseImageClipTransparent);
     finally
      SelectClipRgn(Canvas.Handle, PrevRgn);
      DeleteObject(PrevRgn);
     end;
    end;
    if Complete and IsFilled then
     // Restore canvas and created path
     RestoreDC(DC, SavedDC)
    else begin
     // Setup canvas and create path again (for closed and not closed figures)
     FPenProp.Setup(Canvas, Owner.Scale);
     FBrushProp.Setup(Canvas, Owner.Scale);
     DC := Canvas.Handle;
     if not CreatePath(DC, ScrPoints, FPointTypes, True, True,
       Complete, False, PointsInfo) then exit;
    end;
    // Stroke path
    StrokePath(DC);
   end else begin
    if IsFilled then
      StrokeAndFillPath(DC);
    if not Complete and
      CreatePath(DC, ScrPoints, FPointTypes, False, True,
        Complete, Owner.UseOriginalBezier, PointsInfo) then
     StrokePath(DC);
   end;
  end else
  if CreatePath(DC, ScrPoints, FPointTypes, True, True,
    Complete, False, PointsInfo) then StrokePath(DC);
 end else begin
  // Draw curve as polyline
  if FBrushProp.IsPaintAlternate and PointsInfo.Figures[0].IsClosed then begin
   PrevRgn := 0;
   ClipRgn := CreatePolygonRgn(ScrPoints[0], Length(ScrPoints), ALTERNATE);
   try
    PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
    FBrushProp.PaintAlternate(Canvas, PaintRect, Owner.PaintRect,
      Owner.Scale, Owner.UseImageClipTransparent);
   finally
    SelectClipRgn(Canvas.Handle, PrevRgn);
    DeleteObject(PrevRgn);
    DeleteObject(ClipRgn);
   end;
   FPenProp.Setup(Canvas, Owner.Scale);
   FBrushProp.Setup(Canvas, Owner.Scale);
  end;
  if PointsInfo.Figures[0].IsClosed
   then Canvas.Polygon(ScrPoints)
   else Canvas.PolyLine(ScrPoints);
  DC := Canvas.Handle;
 end;
 if (FBeginCapProp.CapStyle <> psNoCap) or
    (FEndCapProp.CapStyle <> psNoCap) then begin
  // Init begin cap params
  with BeginCap do begin
   Style := FBeginCapProp.CapStyle;
   if FBeginCapProp.FixedOutlineColor
    then OutlineColor := FBeginCapProp.OutlineColor
    else OutlineColor := PenColor;
   if FBeginCapProp.FixedFillColor
    then FillColor := FBeginCapProp.FillColor
    else FillColor := PenColor;
   CapSize := ScaleValue(FBeginCapProp.GetActiveSize(FPenProp.ActiveWidth),
     Owner.Scale);
  end;
  // Init end cap params
  with EndCap do begin
   Style := FEndCapProp.CapStyle;
   if FEndCapProp.FixedOutlineColor
    then OutlineColor := FEndCapProp.OutlineColor
    else OutlineColor := PenColor;
   if FEndCapProp.FixedFillColor
    then FillColor := FEndCapProp.FillColor
    else FillColor := PenColor;
   CapSize := ScaleValue(FEndCapProp.GetActiveSize(FPenProp.ActiveWidth),
     Owner.Scale);
  end;
  // Render caps
  RenderCaps(DC, PenWidth, BeginCap, EndCap, ScrPoints, FPointTypes,
    PointsInfo);
 end;
 // Restore real Pen/Brush handles since we may call RestoreDC before 
 Canvas.Refresh;
end;

function TFlexCurve.InternalInsertPoints(Index, Count: integer): integer;
begin
 if ChangePathCount(FPoints, FPointTypes, Index, +Count)
  then Result := Index
  else Result := -1;
end;

procedure TFlexCurve.InternalDeletePoints(Index, Count: integer);
begin
 ChangePathCount(FPoints, FPointTypes, Index, -Count);
end;

procedure TFlexCurve.DeletePoint(Index: integer);
var FigIndex, PrevNode, Count: integer;
    PrevCurve, NextCurve: boolean;
begin
 if FPointTypes[Index] = ptControl then exit;
 //if Length(FPoints) < 3 then exit;
 FigIndex := GetFigureIndex(PointsInfo^, Index);
 if FigIndex < 0 then exit;
 Count := Length(FPoints);
 with PointsInfo.Figures[FigIndex] do begin
  // Define previous node
  if Index = FirstNode then
   PrevNode := LastNode
  else
  if FPointTypes[Index-1] = ptControl
   then PrevNode := Index-3
   else PrevNode := Index-1;
  PrevCurve := (LastNode < Count-2) and (FPointTypes[PrevNode+1] = ptControl);
  NextCurve := (Index < Count-2) and (FPointTypes[Index+1] = ptControl);
  // Change types
  if Index = LastNode then
   if IsClosed
    then FPointTypes[PrevNode] := ptEndNodeClose
    else FPointTypes[PrevNode] := ptEndNode;
 end;
 Invalidate;
 if PrevCurve and NextCurve then FPoints[PrevNode+2] := FPoints[Index+2];
 if NextCurve
  then InternalDeletePoints(Index, 3)
  else InternalDeletePoints(Index, 1);
 PointsChanged;
end;

function TFlexCurve.InsertPoint(Index: integer;
  const Point: TPoint): integer;
begin
 if (Index < Length(FPoints)) and (FPointTypes[Index] = ptControl) then
  Result := -1
 else begin
  Invalidate;
  Result := InternalInsertPoints(Index, 1);
  if Result >= 0 then begin
   FPoints[Result] := Point;
   FPointTypes[Result] := ptNode;
   PointsChanged;
  end;
 end;
end;

function TFlexCurve.InsertNearestPoint(const Point: TPoint): integer;
begin
 Result := FlexPath.InsertNearestPoint(FPoints, FPointTypes,  
   Point, ScaleValue(SelectionThreshold, Owner.Scale), PointsInfo);
 if Result >= 0 then PointsChanged;
end;

function TFlexCurve.InsertCurvePoints(Index: integer; const Point,
  CtrlPointA, CtrlPointB: TPoint): integer;
begin
 if (Index < Length(FPoints)) and (FPointTypes[Index] = ptControl) then
  Result := -1
 else begin
  Invalidate;
  Result := InternalInsertPoints(Index, 3);
  if Result >= 0 then begin
   FPoints[Result+0] := Point;
   FPoints[Result+1] := CtrlPointA;
   FPoints[Result+2] := CtrlPointB;
   FPointTypes[Result+0] := ptNode;
   FPointTypes[Result+1] := ptControl;
   FPointTypes[Result+2] := ptControl;
   PointsChanged;
  end;
 end;
end;

procedure TFlexCurve.SetPointsEx(const APoints: TPointArray;
  const ATypes: TPointTypeArray);
begin
 //if (Length(APoints) < 2) or (Length(APoints) <> Length(ATypes)) then exit;
 Invalidate;
 RecordPointsAction;
 SetLength(FPoints, Length(APoints));
 SetLength(FPointTypes, Length(ATypes));
 if Length(FPoints) > 0 then begin
  Move(APoints[0], FPoints[0], Length(APoints)*SizeOf(APoints[0]));
  Move(ATypes[0], FPointTypes[0], Length(ATypes)*SizeOf(ATypes[0]));
 end;
 PointsChanged;
end;

procedure TFlexCurve.GetPointsEx(out APoints: TPointArray;
  out ATypes: TPointTypeArray);
begin
 SetLength(APoints, Length(FPoints));
 SetLength(ATypes, Length(FPointTypes));
 if Length(APoints) > 0 then begin
  Move(FPoints[0], APoints[0], Length(APoints)*SizeOf(APoints[0]));
  Move(FPointTypes[0], ATypes[0], Length(ATypes)*SizeOf(ATypes[0]));
 end;
end;

function TFlexCurve.EditPoints(Func: TPathEditFunc;
  const Selected: TSelectedArray; Params: PPathEditParams = Nil): boolean;
begin
 RecordPointsAction;
 Result := EditPath(FPoints, FPointTypes, Selected, Func, Params);
 if Result then PointsChanged;
end;

function TFlexCurve.EditPointsCaps(
  const Selected: TSelectedArray): TPathEditFuncs;
begin
 Result := GetEditPathCaps(FPoints, FPointTypes, Selected);
end;

// TTextFormator //////////////////////////////////////////////////////////////

function TTextFormator.Setup(DC: HDC; const PixelSize: double;
  DivideOnEMSquare: boolean = false; RoundHeight: boolean = true): boolean;
var Size: integer;
    Otm: POutlineTextMetric;
    LogFont: TLogFont;
    RefFont: HFont;
    RefDC: HDC;
    RefOld: HFont;
begin
 Result := false;
 FDC := 0;
 if DC = 0 then exit;
 // Check font changed
 if not CheckFontIdentical(DC) then begin
  if not FLogFontValid then exit;
  Size := GetOutlineTextMetrics(DC, 0, Nil);
  if Size = 0 then exit;
  GetMem(Otm, Size);
  try
   if GetOutlineTextMetrics(DC, Size, Otm) = 0 then exit;
   FEmSquare := Otm.otmEMSquare;    // get EM square size
   // Create original font
   Move(FLogFont, LogFont, SizeOf(LogFont));
   LogFont.lfEscapement := 0;
   LogFont.lfHeight := -FEmSquare;  // font size for 1:1 mapping
   LogFont.lfWidth  := 0;          // original proportion
   // Get font chars width and outline text metrics
   RefFont := CreateFontIndirect(LogFont);
   RefDC := CreateCompatibleDC(DC);
   RefOld := SelectObject(RefDC, RefFont);
   Windows.GetCharWidth(RefDC, 0, 255, FCharWidth);
   Windows.GetCharABCWidths(RefDC, 0, 255, FCharABC);
   Result := GetOutlineTextMetrics(RefDC, Size, Otm) <> 0;
   SelectObject(RefDC, RefOld);
   DeleteObject(RefDC);
   DeleteObject(RefFont);
   if not Result then exit;
   // Calculate font line height and line space
   FHeight := Otm.otmTextMetrics.tmHeight;
   FLinespace := FHeight + Otm.otmTextMetrics.tmExternalLeading;
   if RoundHeight then begin
    if DivideOnEMSquare then FPixelSize := PixelSize / FEMSquare;
    FHeight := Round(Trunc((FHeight * FPixelSize) + 1) / FPixelSize);
    FLinespace := Round(Trunc((FLinespace * FPixelSize) + 1) / FPixelSize);
   end;
  finally
   FreeMem(Otm);
  end;
 end;
 // All ok
 FPixelSize := PixelSize;
 if DivideOnEMSquare then FPixelSize := FPixelSize / FEMSquare;
 FDC := DC;
 Result := true;
end;

function TTextFormator.CheckFontIdentical(DC: HDC): boolean;
var LogFont: TLogFont;
    CurFont: HFont;
begin
 Result := false;
 CurFont := GetCurrentObject(DC, OBJ_FONT);
 if (CurFont = 0) or
    (GetObject(CurFont, SizeOf(LogFont), @LogFont) = 0) then begin
  FLogFontValid := false;
  exit;
 end;
 if FLogFontValid then begin
  with LogFont do
   Result :=
    // Compare numeric parameters
    //(FLogFont.lfHeight           = lfHeight) and  
    (FLogFont.lfWidth            = lfWidth) and
    (FLogFont.lfEscapement       = lfEscapement) and
    (FLogFont.lfOrientation      = lfOrientation) and
    (FLogFont.lfWeight           = lfWeight) and
    (FLogFont.lfItalic           = lfItalic) and
    (FLogFont.lfUnderline        = lfUnderline) and
    (FLogFont.lfStrikeOut        = lfStrikeOut) and
    (FLogFont.lfCharSet          = lfCharSet) and
    (FLogFont.lfOutPrecision     = lfOutPrecision) and
    (FLogFont.lfClipPrecision    = lfClipPrecision) and
    (FLogFont.lfQuality          = lfQuality) and
    (FLogFont.lfPitchAndFamily   = lfPitchAndFamily) and
    // Compare face names
    (StrComp(FLogFont.lfFaceName, LogFont.lfFaceName) = 0);
 end else
  Result := false;
 if not Result then begin
  // Store LogFont
  Move(LogFont, FLogFont, SizeOf(FLogFont));
  FLogFontValid := true;
 end;
end;

function TTextFormator.GetCharABC(AChar: Char): TABC;
begin
 Result := FCharABC[byte(AChar)];
end;

function TTextFormator.GetCharWidth(AChar: Char): integer;
begin
 Result := FCharWidth[byte(AChar)];
end;

//procedure GetWord;
procedure TTextFormator.GetWord(var Line: TTextLine);
begin
 // FLine[FPos] is the starting of a word, find its end
 //while (FPos < FLength) and (FLine[FPos] > ' ') do inc(FPos);
 with Line do
  while (Pos < Length) and (Text[Pos] > ' ') do inc(Pos);
end;

function TTextFormator.SkipWhite(var Line: TTextLine;
  var NeedBreak: boolean): boolean;
begin
 NeedBreak := false;
 with Line do begin
  // skip white space
  while (Pos < Length) and (Text[Pos] <= ' ') do begin
   if (Text[Pos] = #13) or (Text[Pos] = #10) then begin
    NeedBreak := true;
    inc(Pos);
    if (Pos < Length) and
       (((Text[Pos-1] = #13) and (Text[Pos] = #10)) or
        ((Text[Pos-1] = #10) and (Text[Pos] = #13))) then
     // Skip second break char
     inc(Pos);
    break;
   end;
   inc(Pos);
  end;
  Result := Pos < Length;
 end;
end;

procedure TTextFormator.LineExtent(ALine: PChar; ACount: integer);
var i: integer;
    FirstChar: Char;
    LastChar: Char;
begin
 FLineSize.cx := 0;
 FirstChar := #0;
 LastChar := #0;
 for i:=0 to ACount-1 do
  if ALine[i] >= ' ' then begin
   LastChar := ALine[i];
   if FirstChar = #0 then FirstChar := LastChar;
   FLineSize.cx := FLineSize.cx + FCharWidth[byte(LastChar)];
  end;
 if FirstChar > #0
  then FLineLeftSpace := FCharABC[byte(FirstChar)].abcA
  else FLineLeftSpace := 0;
 if LastChar > #0
  then FLineRightSpace := FCharABC[byte(LastChar)].abcC
  else FLineRightSpace := 0;
 inc(FLineSize.cx, FLineRightSpace);
 FLineSize.cy := FLinespace; //FHeight;
end;

function TTextFormator.GetLine(var Line: TTextLine; LineWidth: integer;
  var LineBegin, LineEnd: integer; WordWrap: boolean): boolean;
var
  WordPos: integer;
  NeedBreak: boolean;
  EndOfLine: boolean;
begin
 Result := false;
 if Line.Pos >= Line.Length then exit;
 with Line do
 if WordWrap then begin
  // test leading white spaces
  LineBegin := Line.Pos;
  EndOfLine := not SkipWhite(Line, NeedBreak);
  if EndOfLine then begin
   if not NeedBreak then exit;
   // here is only empty line
   LineEnd := LineBegin;
   Result := true;
   exit;
  end;
  // first no white space to display
  LineBegin := Pos;
  // add words until the line is too long
  while SkipWhite(Line, NeedBreak) and not NeedBreak do begin
   // first end of word
   GetWord(Line);
   //if NeedBreak then break;
   // Get extent
   LineExtent(Line.Text + LineBegin, Line.Pos - LineBegin);
   // break out if it's too long
   NeedBreak := FLineSize.cx >= LineWidth;
   if NeedBreak then break;
  end;
  if NeedBreak and (FLineSize.cx > LineWidth) then begin
   WordPos := Pos - 1;
   // find a place to break a word into two
   while WordPos > LineBegin do begin
    if // space character is breakable
      (Text[WordPos-1] <= ' ') or
       // hypen character is breakable
      (Text[WordPos-1] = '-') then begin
     // skip trailing white space
     while (WordPos > LineBegin) and (Text[WordPos-1] = ' ') do dec(WordPos);
     // can we fit now ?
     LineExtent(Text + LineBegin, WordPos - LineBegin);
     if FLineSize.cx <= LineWidth then begin
      Pos := WordPos;
      break;
     end;
    end;
    dec(WordPos);
   end;
  end;
  LineEnd := Pos;
  while LineEnd > LineBegin do
   if Text[LineEnd - 1] = ' ' then
    dec(LineEnd)
   else
   if (Text[LineEnd - 1] = #13) or (Text[LineEnd - 1] = #10) then begin
    dec(LineEnd);
    if (LineEnd > LineBegin) and
       (((Text[LineEnd] = #13) and (Text[LineEnd - 1] = #10)) or
        ((Text[LineEnd] = #10) and (Text[LineEnd - 1] = #13))) then
    // Skip second break char
    dec(Pos);
   end else
    break;
 end else begin
  // No WordWrap
  LineBegin := Pos;
  while (Pos < Length) and (Text[Pos] <> #13) and (Text[Pos] <> #10) do
   inc(Line.Pos);
  LineEnd := Pos;
  if LineEnd < Length then begin
   // It is line break
   inc(Pos);
   if (Pos < Length) and
      (((Text[Pos-1] = #13) and (Text[Pos] = #10)) or
       ((Text[Pos-1] = #10) and (Text[Pos] = #13))) then
    // Skip second break char
    inc(Pos);
  end;
  LineExtent(Line.Text + LineBegin, LineEnd - LineBegin);
 end;
 Result := true;
end;

function TTextFormator.CharExtent(Text: PChar;
  CharCount: integer; LineBreaks: boolean = false; WordWrap: boolean = false;
  PaintWidth: integer = 0; InLogicalUnits: boolean = false;
  LineCount: PInteger = Nil): TSize;
var //i: integer;
    LineBegin, LineEnd: integer;
    Line: TTextLine;
begin
 Result.cx := 0;
 if Assigned(LineCount) then LineCount^ := 0;
 if not InLogicalUnits and (PaintWidth > 0) then begin
  // Convert PaintWidth to logical units
  PaintWidth := Round(PaintWidth / FPixelSize);
 end;
 if LineBreaks then begin
  Result.cy := 0;
  if (FDC = 0) or (Text = '') then exit;
  Line.Length := Length(Text);
  Line.Text := PChar(Text);
  Line.Pos := 0;
  while GetLine(Line, PaintWidth, LineBegin, LineEnd, WordWrap) do begin
   if FLineSize.cx > Result.cx then Result.cx := FLineSize.cx;
   //if Result.cy > 0 then inc(Result.cy, FLinespace - FHeight);
   Result.cy := Result.cy + FLineSize.cy;
   if Assigned(LineCount) then inc(LineCount^);
  end;
  if Result.cy = 0
   then Result.cy := FLinespace; //FHeight;
   //else inc(Result.cy, FHeight - FLinespace);
  //inc(Result.cy, Round(1 / FPixelSize));
 end else begin
  //for i:=0 to CharCount-1 do Result.cx := Result.cx + FCharWidth[byte(Text[i])];
  LineExtent(PChar(Text), CharCount);
  Result.cx := FLineSize.cx;
  Result.cy := FLineSize.cy; //FHeight;
 end;
 if not InLogicalUnits then begin
  Result.cx := Round(Result.cx * FPixelSize);
  Result.cy := Round(Result.cy * FPixelSize);
 end;
end;

procedure TTextFormator.CharWordInfos(Text: PChar; CharCount: integer;
  var WordInfos: TTextWordInfoArray; var WordCount, WordWidth: integer;
  InLogicalUnits: boolean = false);
var Line: TTextLine;
    NeedBreak: boolean;
begin
 Line.Text := Text;
 Line.Length := CharCount;
 Line.Pos := 0;
 // Resize word info array if need
 WordCount := CharCount div 2; // maximum words in line
 if Length(WordInfos) < WordCount then SetLength(WordInfos, WordCount);
 // Iterate words in line
 WordCount := 0;
 WordWidth := 0;
 if not SkipWhite(Line, NeedBreak) or NeedBreak then exit;
 while Line.Pos <= CharCount do begin
  with WordInfos[WordCount] do begin
   WordBegin := Line.Pos;
   GetWord(Line);
   WordEnd := Line.Pos;
   // Calc word size in logical units
   LineExtent(Line.Text + WordBegin, WordEnd - WordBegin);
   Size := FLineSize.cx;
   if not InLogicalUnits then Size := Round(Size * FPixelSize);
   WordWidth := WordWidth + Size;
  end;
  inc(WordCount);
  if not SkipWhite(Line, NeedBreak) then break;
 end;
 if not InLogicalUnits then WordWidth := Round(WordWidth * FPixelSize);
end;

function TTextFormator.TextExtent(const Text: string): TSize;
begin
 Result := CharExtent(PChar(Text), Length(Text));
end;

function TTextFormator.TextHeight(const Text: string): integer;
begin
 Result := CharExtent(PChar(Text), Length(Text), true).cy;
end;

function TTextFormator.TextWidth(const Text: string): integer;
begin
 Result := CharExtent(PChar(Text), Length(Text), true).cx;
end;

procedure TTextFormator.CharOut(X, Y: Integer; Text: PChar; CharCount: integer);
type
  PIntegers = ^TIntegers;
  TIntegers = array[0..MaxInt div SizeOf(integer) -1] of integer;
var Dx: PIntegers;
    LastX: integer;
    Sum: double;
    {SumDx, }NewX: integer;
    i: integer;
begin
 if (FDC = 0) or (CharCount <= 0) then exit;
 LastX := 0;
 Sum := 0;
 //SumDx := 0;
 GetMem(Dx, CharCount * SizeOf(Dx[0]));
 try
  for i:=0 to CharCount-1 do begin
   Sum := Sum + FCharWidth[byte(Text[i])];
   NewX := Round(Sum * FPixelSize);
   Dx[i] := NewX - LastX;
   LastX := NewX;
   //inc(SumDx, Dx[i]);
  end;
  ExtTextOut(FDC, X, Y, 0, Nil, Text, CharCount, PInteger(Dx));
 finally
  FreeMem(Dx);
 end;
end;

procedure TTextFormator.TextOut(X, Y: Integer; const Text: string);
begin
 CharOut(X, Y, PChar(Text), Length(Text));
end;

procedure TTextFormator.TextRect(Rect: TRect; const Text: string;
  WordWrap: boolean; Align: TAlignment = taLeftJustify;
  ALayout: TTextLayout = tlTop; WidthJustify: boolean = false;
  LogicalWidth: integer = 0; Rotation: integer = 0);
type
  TDPoint = record
    x, y: double;
  end;
var Line: TTextLine;
    LogOutX: double;
    LogOutY: double;
    Width: integer;
    LineBegin, LineEnd: integer;
    //NeedBreak: boolean;
    WordInfos: TTextWordInfoArray;
    WordCount: integer;
    WordWidth: integer;
    WordStep: double;
    i: integer;
    LineStep: TDPoint;
    LineStart: TPoint;
    //
    c, d, min, max, LogStart: TDPoint;
    Angle, rsin, rcos, k: double;
    pt: array[0..3] of TDPoint;
    RotOfs: TPoint;
    RotPoints: array[0..3] of TPoint;
    RotMin, RotMax: TPoint;
    TextSize: TSize;
begin
 if (FDC = 0) or (Text = '') then exit;
 Line.Text := PChar(Text);
 Line.Length := Length(Text);
 Line.Pos := 0;
 LogOutX := 0;
 LogOutY := 0;
 Width := Rect.Right - Rect.Left;
 if LogicalWidth <= 0 then
  LogicalWidth := Round(Width / FPixelSize);
 if Rotation <> 0 then begin
  Rotation := -Rotation mod 360;
  if Rotation < 0 then Rotation := 360 + Rotation;

  Angle := (Rotation * Pi) / 180;
  rsin := sin(Angle);
  rcos := cos(Angle);

  LineStep.X := -FLinespace * rsin;
  LineStep.Y := FLinespace * rcos;

  with Rect do begin
   c.x := (Right - Left) / 2;
   c.y := (Bottom - Top) / 2;
  end;

  pt[0].x := (-c.x) * rcos - (-c.y) * rsin;
  pt[0].y := (-c.x) * rsin + (-c.y) * rcos;
  pt[1].x := (c.x) * rcos - (-c.y) * rsin;
  pt[1].y := (c.x) * rsin + (-c.y) * rcos;
  pt[2].x := (c.x) * rcos - (c.y) * rsin;
  pt[2].y := (c.x) * rsin + (c.y) * rcos;
  pt[3].x := (-c.x) * rcos - (c.y) * rsin;
  pt[3].y := (-c.x) * rsin + (c.y) * rcos;

  if Rotation < 90 then begin
   min.x := pt[3].x;
   min.y := pt[0].y;
   max.x := pt[1].x;
   max.y := pt[2].y;
  end else
  if Rotation < 180 then begin
   min.x := pt[2].x;
   min.y := pt[3].y;
   max.x := pt[0].x;
   max.y := pt[1].y;
  end else
  if Rotation < 270 then begin
   min.x := pt[1].x;
   min.y := pt[2].y;
   max.x := pt[3].x;
   max.y := pt[0].y;
  end else begin
   min.x := pt[0].x;
   min.y := pt[1].y;
   max.x := pt[2].x;
   max.y := pt[3].y;
  end;

  d.x := c.x / (max.x - min.x);
  d.y := c.y / (max.y - min.y);
  if d.x < d.y then k := 2*d.x else k := 2*d.y;
  for i:=Low(pt) to High(pt) do begin
   RotPoints[i].x := Rect.Left + Round(c.x + pt[i].x * k);
   RotPoints[i].y := Rect.Top + Round(c.y + pt[i].y * k);
  end;

  if Rotation < 90 then begin
   RotMin.x := RotPoints[3].x;
   RotMin.y := RotPoints[0].y;
   RotMax.x := RotPoints[1].x;
   RotMax.y := RotPoints[2].y;
  end else
  if Rotation < 180 then begin
   RotMin.x := RotPoints[2].x;
   RotMin.y := RotPoints[3].y;
   RotMax.x := RotPoints[0].x;
   RotMax.y := RotPoints[1].y;
  end else
  if Rotation < 270 then begin
   RotMin.x := RotPoints[1].x;
   RotMin.y := RotPoints[2].y;
   RotMax.x := RotPoints[3].x;
   RotMax.y := RotPoints[0].y;
  end else begin
   RotMin.x := RotPoints[0].x;
   RotMin.y := RotPoints[1].y;
   RotMax.x := RotPoints[2].x;
   RotMax.y := RotPoints[3].y;
  end;

  if ALayout <> tlCenter then begin
   if ALayout = tlBottom
    then i := (Rotation + 180) mod 360
    else i := Rotation;
   with Rect do
    if i < 90 then begin
     RotOfs.X := Right - RotMax.X;
     RotOfs.Y := Bottom - RotMax.Y;
    end else
    if i < 180 then begin
     RotOfs.X := Right - RotMax.X;
     RotOfs.Y := Bottom - RotMax.Y;
    end else
    if i < 270 then begin
     RotOfs.X := Left - RotMin.X;
     RotOfs.Y := Top - RotMin.Y;
    end else begin
     RotOfs.X := Left - RotMin.X;
     RotOfs.Y := Top - RotMin.Y;
    end;

   for i:=Low(pt) to High(pt) do begin
    inc(RotPoints[i].x, RotOfs.X);
    inc(RotPoints[i].y, RotOfs.Y);
   end;
  end;

  //Windows.Polygon(FDC, RotPoints[0], Length(RotPoints));

  LogicalWidth := Round(LogicalWidth * k);

  LineStart := RotPoints[0];
 end else begin
  rsin := 0.0;
  rcos := 1.0;
  LineStep.X := 0;
  LineStep.Y := FLinespace;
  LineStart := Rect.TopLeft;
 end;

 if ALayout <> tlTop then begin
  // Calc text size
  TextSize := CharExtent(
    PChar(Text), Length(Text), true, WordWrap, LogicalWidth, True);
  // Calc Rect height in the font logical units
  if Rotation <> 0 then begin
   LogOutX := (RotPoints[3].x - RotPoints[0].x) / FPixelSize;
   LogOutY := (RotPoints[3].y - RotPoints[0].y) / FPixelSize;
  end else begin
   LogOutX := 0;
   LogOutY := (Rect.Bottom - Rect.Top) / FPixelSize;
  end;
  // Calc text offset for vertical alignment
  d.y := TextSize.cy;
  case ALayout of
   tlCenter:
     begin
      LogOutX := (LogOutX + d.y * rsin) / 2;
      LogOutY := (LogOutY - d.y * rcos) / 2;
     end;
   tlBottom:
     begin
      LogOutX := LogOutX + d.y * rsin;
      LogOutY := LogOutY - d.y * rcos;
     end;
  end;
 end;
 d.x := 0.0;
 d.y := 0.0;

 while GetLine(Line, LogicalWidth, LineBegin, LineEnd, WordWrap) do begin
  if WidthJustify then begin
   CharWordInfos(Line.Text + LineBegin, LineEnd - LineBegin,
     WordInfos, WordCount, WordWidth, true);
   // Calc extra size
   if WordCount > 1
    then WordStep := (LogicalWidth - WordWidth) / (WordCount - 1)
    else WordStep := 0;
   LogStart.x := LogOutX;
   LogStart.y := LogOutY;
   for i:=0 to WordCount-1 do with WordInfos[i] do begin
    CharOut(
      LineStart.X + Round(LogOutX * FPixelSize), // Rect.Left + Round(LogOutX * FPixelSize),
      LineStart.Y + Round(LogOutY * FPixelSize), // LineStart.Y + Round(LogOutY * FPixelSize),
      Line.Text + LineBegin + WordBegin, WordEnd - WordBegin);
    if i = WordCount-2 then begin
     //LogOutX := Width / FPixelSize - WordInfos[i+1].Size 
     LogOutX := LogStart.x + (LogicalWidth - WordInfos[i+1].Size) * rcos;
     LogOutY := LogStart.y + (LogicalWidth - WordInfos[i+1].Size) * rsin;
    end else begin
     LogOutX := LogOutX + (Size + WordStep) * rcos;
     LogOutY := LogOutY + (Size + WordStep) * rsin;
    end;
   end;
   LogOutX := LogStart.x;
   LogOutY := LogStart.y;
  end else begin
   if Align <> taLeftJustify then begin
    case Align of
     taCenter:
       d.x := (LogicalWidth - FLineSize.cx) / 2;
     taRightJustify:
       d.x := LogicalWidth - FLineSize.cx;
    end;
    if Rotation <> 0 then begin
     d.y := d.x * rsin;
     d.x := d.x * rcos;
    end;
   end;
   CharOut(
     LineStart.X + Round((LogOutX + d.x) * FPixelSize),
     LineStart.Y + Round((LogOutY + d.y) * FPixelSize),
     Line.Text + LineBegin, LineEnd - LineBegin);
  end;
  LogOutX := LogOutX + LineStep.X;
  LogOutY := LogOutY + LineStep.Y;
 end;
end;

// TFlexText /////////////////////////////////////////////////////////////////

procedure TFlexText.ControlCreate;
begin
 inherited;
 FTextProp.Text := Name;
 FPenProp.Style := psClear;
 FAlwaysFilled := true;
 //FAutoSizeProp.Value := true;
end;

procedure TFlexText.ControlDestroy;
begin
 inherited;
 FFormator.Free;
 FFormator := Nil;
end;

procedure TFlexText.CreateProperties;
var A: TAlignment;
    L: TTextLayout;
begin
 inherited;
 FAutoSizeProp := TBoolProp.Create(Props, 'AutoSize');
 FTextProp := TStrListProp.Create(Props, 'Text');
 FFontProp := TFontProp.Create(Props, 'Font');
 FWordWrapProp := TBoolProp.Create(Props, 'WordWrap');
 FGrayedProp := TBoolProp.Create(Props, 'Grayed');
 FAngleProp := TIntProp.Create(Props, 'Angle');
 FAlignmentProp := TEnumProp.Create(Props, 'Alignment');
 with FAlignmentProp do
 for A:=Low(A) to High(A) do
  case A of
   taLeftJustify  : AddItem('LeftJustify');
   taRightJustify : AddItem('RightJustify');
   taCenter       : AddItem('Center');
  end;
 FLayoutProp := TEnumProp.Create(Props, 'Layout');
 with FLayoutProp do
 for L:=Low(L) to High(L) do
  case L of
   tlTop    : AddItem('Top');
   tlCenter : AddItem('Center');
   tlBottom : AddItem('Bottom');
  end;
 FPreciseProp := TBoolProp.Create(Props, 'Precise');
 FPreciseJustifyProp := TBoolProp.Create(Props, 'PreciseJustify');
 FAutoScaleFontSizeProp := TBoolProp.Create(Props, 'AutoScaleFontSize');
 FAutoScaleFontSizeProp.Style := FAutoScaleFontSizeProp.Style + [ psScalable ]; 
 FMaxFontSizeProp := TIntProp.Create(Props, 'MaxFontSize');
end;

class function TFlexText.CursorInCreate: TCursor;
begin
 Result := crCreateTextCursor;
end;

procedure TFlexText.ControlTranslate(const TranslateInfo: TTranslateInfo);
var Degree: integer;
begin
 Degree := (FAngleProp.Value + TranslateInfo.Rotate) mod 360;
 if Degree < 0 then Degree := 360 + Degree;
 if Degree <> 0 then begin
//  FAutoSizeProp.Value := False;
//  FAutoScaleFontSizeProp.Value := False;
 end;
 FAngleProp.Value := Degree;
 inherited;
end;

function TFlexText.CreateCurveControl: TFlexControl;
begin
 // Not supported in current version
 Result := Nil;
end;

procedure TFlexText.PropChanged(Sender: TObject; Prop: TCustomProp);
var Size: TSize;
    PenWidth: integer;
    ForceFontSize: boolean;
    SizeProp: TIntProp;
begin
 inherited;
 ForceFontSize := Owner.InDesign and
   (Owner.ToolMode in [ftmResizing, ftmCreating]) and
   (GetKeyState(VK_SHIFT) and $8000 {SHIFTED} <> 0);
 // Select size property for auto scale font size by current angle
 case FAngleProp.Value of
  0,  180: SizeProp := HeightProp;
  90, 270: SizeProp := WidthProp;
  else     SizeProp := Nil;
 end;
 if not FAutoSizeChanging and (
    ((Prop = FAutoScaleFontSizeProp) and FAutoScaleFontSizeProp.Value) or
    (Assigned(SizeProp) and (Prop = SizeProp) and
     (ForceFontSize or FAutoScaleFontSizeProp.Value)) ) then begin
  // Change font size
  FAutoSizeChanging := true;
  try
   if (FMaxFontSizeProp.Value > 0) and
      (HeightProp.Value > FMaxFontSizeProp.Value) then
    FFontProp.Height := -FMaxFontSizeProp.Value
   else
   if Assigned(SizeProp) then
    FFontProp.Height := -SizeProp.Value;
  finally
   FAutoSizeChanging := false;
  end;
 end;
 if (Prop = FAutoSizeProp) and (FAngleProp.Value = 0){ or (Prop = FAngleProp) }then
  AutoSizeChanged;
 if FAutoSizeProp.Value and not FAutoSizeChanging and
    (Prop <> LeftProp) and (Prop <> TopProp) then begin
  if FAngleProp.Value <> 0 then begin
   with WidthProp do Style := Style - [psReadOnly];
   with HeightProp do Style := Style - [psReadOnly];
  end else begin
   FRefreshScale := 0;
   Size := TextSize;
   PenWidth := FPenProp.ActiveWidth;
   inc(Size.cx, 2*PenWidth);
   inc(Size.cy, 2*PenWidth);
   if (Size.cx = 0) and (Size.cy = 0) then exit;
   if Size.cx < 1 then Size.cx := 1;
   if Size.cy < 1 then Size.cy := 1;
   if (Size.cx <> Width) or (Size.cy <> Height) then begin
    FAutoSizeChanging := true;
    try
     with WidthProp do Style := Style - [psReadOnly];
     with HeightProp do Style := Style - [psReadOnly];
     Height := Size.cy;
     if not FWordWrapProp.Value and not Owner.IsLoading then begin
      Width := Size.cx;
      with WidthProp do Style := Style + [psReadOnly];
     end;
     if FAutoScaleFontSizeProp.Value then
      with HeightProp do Style := Style - [psReadOnly]
     else
      with HeightProp do Style := Style + [psReadOnly];
    finally
     FAutoSizeChanging := false;
    end;
   end;
  end;
  // Unlbock width in WordWrap mode
  if FWordWrapProp.Value then
   with WidthProp do Style := Style - [psReadOnly];
 end;
end;

procedure TFlexText.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = FAutoSizeProp then
  IsStored := FAutoSizeProp.Value
 else
 if Prop = FTextProp then
  IsStored := FTextProp.LinesCount > 0
 else
 if Prop = FWordWrapProp then
  IsStored := FWordWrapProp.Value
 else
 if Prop = FGrayedProp then
  IsStored := FGrayedProp.Value
 else
 if Prop = FAlignmentProp then
  IsStored := FAlignmentProp.EnumIndex <> 0
 else
 if Prop = FLayoutProp then
  IsStored := FLayoutProp.EnumIndex <> 0
 else
 if Prop = FAngleProp then
  IsStored := FAngleProp.Value <> 0
 else
 if Prop = FPreciseProp then
  IsStored := FPreciseProp.Value
 else
 if Prop = FPreciseJustifyProp then
  IsStored := FPreciseJustifyProp.Value
 else
 if (Prop = FPenProp) and (PropName = 'Style') then
  IsStored := FPenProp.Style <> psClear
 else
 if Prop= FAutoScaleFontSizeProp then
   IsStored := FAutoScaleFontSizeProp.Value
 else
 if Prop = FMaxFontSizeProp then
  IsStored := FMaxFontSizeProp.Value <> 0
 else
  inherited;
end;

procedure TFlexText.AutoSizeChanged;
var Size: TSize;
    PenWidth: integer;
begin
 FAutoSizeChanging := true;
 try
  // AutoSize works only when Angle = 0.
  if (FAutoSizeProp.Value) and (FAngleProp.Value = 0) then begin
   if FAngleProp.Value = 0 then FRefreshScale := 0;
   Size := TextSize;
   if (Size.cx = 0) and (Size.cy = 0) then exit;
   PenWidth := FPenProp.ActiveWidth;
   Height := Size.cy + 2*PenWidth;
   if not FWordWrapProp.Value and not Owner.IsLoading then begin
    Width := Size.cx + 2*PenWidth;
    with WidthProp do Style := Style + [psReadOnly];
   end else
    with WidthProp do Style := Style - [psReadOnly];
   if FAutoScaleFontSizeProp.Value then
    with HeightProp do Style := Style - [psReadOnly]
   else
    with HeightProp do Style := Style + [psReadOnly];
  end else begin
   // Unblock width and height
   with WidthProp do Style := Style - [psReadOnly];
   with HeightProp do Style := Style - [psReadOnly];
  end;
 finally
  FAutoSizeChanging := false;
 end;
end;

procedure TFlexText.GetLeftRightExtra(DC: HDC; var Left, Right: integer);
var ABCs: array[0..255] of TABC;
    i: integer;
begin
 Left := 0;
 Right := 0;
 if not GetCharABCWidths(DC, 0, 255, ABCs) then exit;
 for i:=0 to High(ABCs) do begin
  if ABCs[i].abcA < Left then Left := ABCs[i].abcA;
  if ABCs[i].abcC < Right then Right := ABCs[i].abcC;  
 end;
 Left := -Left;
 Right := -Right;
end;

procedure TFlexText.DoDrawText(Canvas: TCanvas; var Rect: TRect;
  Flags: Longint; const Text: string);
//var ABC: TABC;
begin
 if (Flags and DT_CALCRECT <> 0) and (Canvas.Font.Size = 0) then begin
  SetRectEmpty(Rect);
  exit;
 end;
 Flags := Flags or DT_NOPREFIX;
 if Grayed then begin
  OffsetRect(Rect, 1, 1);
  Canvas.Font.Color := clBtnHighlight;
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  OffsetRect(Rect, -1, -1);
  Canvas.Font.Color := clBtnShadow;
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  if Flags and DT_CALCRECT <> 0 then begin
   inc(Rect.Right);
   inc(Rect.Bottom);
  end;
 end else
  Windows.DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
{ if Flags and DT_CALCRECT <> 0 then begin
  if Text <> '' then begin
   FillChar(ABC, SizeOf(ABC), 0);
   if GetCharABCWidths(Canvas.Handle,
        Cardinal(Text[Length(Text)]), Cardinal(Text[Length(Text)]), ABC) then
    inc(Rect.Right, abs(ABC.abcC));
  end;
 end;  }
end;

{$IFDEF FG_CBUILDER}
procedure TFlexText.DrawTextCpp(
{$ELSE}
procedure TFlexText.DrawText(
{$ENDIF}
  Canvas: TCanvas; var R: TRect; CalcOnly, Scaled: boolean);
begin
 DrawTextEx(Canvas, R, CalcOnly, Scaled, FTextProp.Text, 
   Alignment, Layout, WordWrap, PreciseProp.Value, PreciseJustifyProp.Value);
end;

procedure TFlexText.DrawTextEx(Canvas: TCanvas; var R: TRect;
  CalcOnly, Scaled: boolean; const AText: string; AAlignment: TAlignment;
  ALayout: TTextLayout; AWordWrap: boolean; APrecise, APreciseJustify: boolean;
  LogSize: integer = 0; AFontHeight: integer = 0);
type
  TDPoint = record
    x, y: double;
  end;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  CalcRect: TRect;
  DrawStyle: Longint;
  Offset: integer;
  PrevRgn, ClipRgn: HRGN;
  LogFont: TLogFont;
  PenWidth: integer;
  angle: double;
  NeedRotate: boolean;
  TM: TTextMetric;
  TextSize: TSize;
  RotOrg: TPoint;
  RotFont, OldFont: HFont;
  DC: HDC;

 function DMin(const a, b: double): double;
 begin
   if a < b then
     Result := a
   else
     Result := b;
 end;

 function DMax(const a, b: double): double;
 begin
   if a < b then
     Result := b
   else
     Result := a;
 end;

 function SetClipRgn: boolean;
 var Round: integer;
     ClipRect: TRect;
 begin
  if FAutoSizeProp.Value and (FRefreshScale > 0) and (FAngleProp.Value = 0) and
     not APrecise and not AWordWrap then begin
   // Do not use clip region
   ClipRgn := 0;
   PrevRgn := 0;
   DrawStyle := DrawStyle or DT_NOCLIP;
   Result := true;
  end else begin
   ClipRect := R;
   if FRoundnessProp.Value = 0 then begin
    // Create rectangle region
 {   if PenWidth = 0 then begin
     dec(ClipRect.Right);
     dec(ClipRect.Bottom);
    end; }
    Result := not IsRectEmpty(ClipRect);
    if not Result then exit;
    ClipRgn := CreateRectRgnIndirect(ClipRect);
   end else begin
    Round := ScaleValue(FRoundnessProp.Value - 3*PenWidth div 2, Owner.Scale);
    if Round < 0 then begin
     // Roundness to small. Create rectangle region
     Result := not IsRectEmpty(ClipRect);
     if not Result then exit;
     ClipRgn := CreateRectRgnIndirect(ClipRect);
    end else begin
     // Create rounded rectangle region
     if PenWidth > 0 then begin
      inc(ClipRect.Right);
      inc(ClipRect.Bottom);
     end;
     Result := not IsRectEmpty(ClipRect);
     if not Result then exit;
     with ClipRect do
      ClipRgn := CreateRoundRectRgn(Left, Top, Right, Bottom, Round, Round);
    end;
   end;
   PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
   {Canvas.Brush.Color := clLime; // DEBUG
   Canvas.FillRect(ClipRect);}
  end;
 end;

begin
 if (AText = '') and not CalcOnly then exit;
 PrevRgn := 0;
 ClipRgn := 0;
 PenWidth := FPenProp.ActiveWidth;
 with Canvas do
 try
  if AFontHeight >= 0 then begin
   if Scaled
    then FFontProp.Setup(Canvas, Owner.Scale)
    else FFontProp.Setup(Canvas);
   AFontHeight := FFontProp.Height;
  end;
  // SetBkColor(Handle, RGB($FF, $FF, 0)); // DEBUG
  Canvas.Brush.Style := bsClear;
  NeedRotate := FAngleProp.Value <> 0;
  if NeedRotate then begin
   GetTextMetrics(Handle, TM);
   NeedRotate := TM.tmPitchAndFamily and TMPF_TRUETYPE <> 0;
  end;

  if APrecise then begin
   if not Assigned(FFormator) then FFormator := TTextFormator.Create;
   FFormator.Setup(Canvas.Handle,
     -AFontHeight * Owner.Scale / (100 * PixelScaleFactor), true);
   if LogSize = 0 then
    LogSize := Round(
      (WidthProp.Value - 2*PenWidth) * FFormator.EMSquare /
      (-AFontHeight) );
   if NeedRotate then begin
    if CalcOnly then begin
     SetRectEmpty(R);
     Exit;
    end;
    if not SetClipRgn then exit; // Nothing to paint
    DC := Canvas.Handle;
    OldFont := GetCurrentObject(DC, OBJ_FONT);
    GetObject(OldFont, SizeOf(LogFont), @LogFont);
    LogFont.lfEscapement := FAngleProp.Value * 10;
    RotFont := CreateFontIndirect(LogFont);
    OldFont := SelectObject(DC, RotFont);
    try
     FFormator.TextRect(R, AText, AWordWrap,
       AAlignment, ALayout, APreciseJustify, LogSize, FAngleProp.Value);
    finally
     SelectObject(DC, OldFont);
     DeleteObject(RotFont);
    end;
   end else begin
    // Without rotation
    if CalcOnly or (ALayout <> tlTop) then begin
     // Calc text size
     TextSize := FFormator.CharExtent(
       PChar(AText), Length(AText), true, AWordWrap, LogSize, True);
     TextSize.cx :=
       Round(TextSize.cx * (-AFontHeight) / FFormator.EMSquare);
     TextSize.cy :=
       Round(TextSize.cy * (-AFontHeight) / FFormator.EMSquare);
    end;
    if CalcOnly then begin
     R.Right := TextSize.cx;
     R.Bottom := TextSize.cy;
    end else begin
     if not SetClipRgn then exit; // Nothing to paint
     if ALayout <> tlTop then begin
      CalcRect := R;
      CalcRect.Right := R.Left + Round(ScaleValue(TextSize.cx, Owner.Scale));
      CalcRect.Bottom := R.Top + Round(ScaleValue(TextSize.cy, Owner.Scale));
      Offset := (R.Bottom - R.Top) - (CalcRect.Bottom - CalcRect.Top);
      if ALayout = tlBottom
       then OffsetRect(R, 0, Offset)
       else OffsetRect(R, 0, Offset div 2);
      R.Bottom := R.Top + (CalcRect.Bottom - CalcRect.Top);
     end;
     FFormator.TextRect(R, AText, AWordWrap,
       AAlignment, ALayout, APreciseJustify, LogSize, FAngleProp.Value);
    end;
   end;
  end else begin
   // Windows GDI TextOut
   if NeedRotate then begin
    Angle := (FAngleProp.Value * Pi) / 180;
    with RotOrg do begin
     if (FAngleProp.Value >= 0) and (FAngleProp.Value <= 90) then begin
      x := r.Left;
      y := r.Top + Round((r.Bottom - r.Top) * sin(angle));
     end else
     if (FAngleProp.Value > 90) and (FAngleProp.Value <= 180) then begin
      x := r.Left + Round((r.Right - r.Left) * Abs(cos(angle)));
      y := r.Bottom;
     end else
     if (FAngleProp.Value > 180) and (FAngleProp.Value <= 270) then begin
      x := r.Right;
      y := r.Bottom + Round((r.Bottom - r.Top) * sin(angle));
     end else
     if (FAngleProp.Value > 270) and (FAngleProp.Value < 360) then begin
      x := r.Right - Round((r.Right - r.Left) * Abs(cos(angle)));
      y := r.Top;
     end else begin
      x := r.Left;
      y := r.Top;
     end;
    end;
    if CalcOnly then
     SetRectEmpty(R)
    else begin
     // Ready to paint
     if not SetClipRgn then exit; // Nothing to paint
     //SetTextAlign(Handle, TA_LEFT or TA_TOP);
     GetObject(Font.Handle, SizeOf(TLogFont), @LogFont);
     LogFont.lfEscapement := FAngleProp.Value * 10;
     //LogFont.lfOrientation := LogFont.lfEscapement;
     Font.Handle := CreateFontIndirect(LogFont);
     TextOut(RotOrg.X, RotOrg.Y, AText);
    end;
   end else begin
    { DoDrawText takes care of BiDi alignments }
    DrawStyle := DT_EXPANDTABS or
                 WordWraps[AWordWrap] or
                 Alignments[AAlignment];
    if CalcOnly then begin
     { Calculate only }
     DrawStyle := DrawStyle or DT_CALCRECT;
    end else begin
     { Calculate vertical layout }
     if not SetClipRgn then exit; // Nothing to paint
     if ALayout <> tlTop then begin
      CalcRect := R;
      DoDrawText(Canvas, CalcRect, DrawStyle or DT_CALCRECT, AText);
      Offset := (R.Bottom - R.Top) - (CalcRect.Bottom - CalcRect.Top);
      //DrawStyle := DrawStyle or DT_NOCLIP;
      if ALayout = tlBottom
       then OffsetRect(R, 0, Offset)
       else OffsetRect(R, 0, Offset div 2);
      R.Bottom := R.Top + (CalcRect.Bottom - CalcRect.Top);
     end;
    end;
    DoDrawText(Canvas, R, DrawStyle, AText);
    if CalcOnly then begin
     R.Right := R.Right * PixelScaleFactor;
     R.Bottom := R.Bottom * PixelScaleFactor;
    end;
   end;
  end;
 finally
  if (ClipRgn <> 0) or (PrevRgn <> 0) then begin
   SelectClipRgn(Canvas.Handle, PrevRgn);
   DeleteObject(PrevRgn);
   DeleteObject(ClipRgn);
  end;
  Font.Color := clBlack;
 end;
end;

procedure TFlexText.Paint(Canvas: TCanvas; var PaintRect: TRect);
var PenWidth: integer;
begin
 inherited;
 PenWidth := ScaleValue(FPenProp.ActiveWidth, Owner.Scale);
 if PenWidth = 0 then begin
  if FRoundnessProp.Value = 0 then begin
   dec(PaintRect.Right);
   dec(PaintRect.Bottom);
  end;
 end else
  InflateRect(PaintRect, -PenWidth, -PenWidth);

 {
  // SHADOW
  FFontProp.Setup(Canvas, Owner.Scale);
  Canvas.Font.Color := clSilver;
  PenWidth := -(Canvas.Font.Height div 10);
  inc(PaintRect.Left, PenWidth);
  inc(PaintRect.Top, PenWidth);
  DrawTextEx(Canvas, PaintRect, False, True, FTextProp.Text,
    TAlignment(FAlignmentProp.EnumIndex), TTextLayout(FLayoutProp.EnumIndex),
    FWordWrapProp.Value, PreciseProp.Value, PreciseJustifyProp.Value, 0,
    FFontProp.Height);
  dec(PaintRect.Left, PenWidth);
  dec(PaintRect.Top, PenWidth);
  Canvas.Font.Color := FFontProp.Color;
 }
    
 //DrawText(Canvas, PaintRect, False, True);
 DrawTextEx(Canvas, PaintRect, False, True, FTextProp.Text, 
   TAlignment(FAlignmentProp.EnumIndex), TTextLayout(FLayoutProp.EnumIndex),
   FWordWrapProp.Value, PreciseProp.Value, PreciseJustifyProp.Value);
end;

function TFlexText.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
var Bmp: TBitmap;
    R: TRect;
    PenWidth: integer;
    LeftExtra: integer;
    RightExtra: integer;
begin
 Result := inherited GetRefreshRect(RefreshX, RefreshY);
 if Assigned(Owner) and not Owner.PaintForExport and (FAngleProp.Value = 0) and
    FAutoSizeProp.Value and not FPreciseProp.Value then begin
  if (FRefreshScale <> Owner.Scale) and not FAutoSizeChanging then begin
   // Update text visible bounds
   Bmp := TBitmap.Create;
   try
    SetRectEmpty(FRefreshRect);
    PenWidth := FPenProp.ActiveWidth;
    FRefreshRect.Right :=
      ScaleValue(WidthProp.Value - 2*PenWidth, Owner.Scale);
    {$IFDEF FG_CBUILDER}DrawTextCpp{$ELSE}DrawText{$ENDIF}(Bmp.Canvas,
      FRefreshRect, True, True);
    with FRefreshRect do begin
     Right := Round((Right + 2*PenWidth) * 100 / Owner.Scale);
     Bottom := Round((Bottom + 2*PenWidth) * 100 / Owner.Scale);
     // In ClearType font smoothing we can lost 1 pixel on the right and
     // on the bottom  
     if Right > 0 then inc(Right, PixelScaleFactor);
     if Bottom > 0 then inc(Right, PixelScaleFactor);
     // Add left/right extra space based on ABC metrics
     GetLeftRightExtra(Bmp.Canvas.Handle, LeftExtra, RightExtra);
     dec(Left, LeftExtra * PixelScaleFactor);
     inc(Right, RightExtra * PixelScaleFactor);
    end;
    FRefreshScale := Owner.Scale;
   finally
    Bmp.Free;
   end;
   with FRefreshRect do begin
    if Right - Left > WidthProp.Value then
     case TAlignment(FAlignmentProp.EnumIndex) of
      taRightJustify:
        dec(Left, (Right - Left) - WidthProp.Value);
      taCenter:
        InflateRect(FRefreshRect, ((Right - Left) - WidthProp.Value) div 2, 0);
     end;
    if Bottom - Top > HeightProp.Value then
     case TTextLayout(FLayoutProp.EnumIndex) of
      tlCenter:
        InflateRect(FRefreshRect, 0, ((Bottom - Top) - HeightProp.Value) div 2);
      tlBottom:
        dec(Top, (Bottom - Top) - HeightProp.Value);
     end;
    if Right < WidthProp.Value then Right := WidthProp.Value;
    if Bottom < HeightProp.Value then Bottom := HeightProp.Value;
   end;
  end;
  R := FRefreshRect;
  OffsetRect(R, RefreshX, RefreshY);
  with R do begin
   if Left < Result.Left then Result.Left := Left;
   if Right > Result.Right then Result.Right := Right;
   if Top < Result.Top then Result.Top := Top;
   if Bottom > Result.Bottom then Result.Bottom := Bottom;
  end;
 end;
end;

function TFlexText.GetAlignment: TAlignment;
begin
 Result := TAlignment(FAlignmentProp.EnumIndex);
end;

function TFlexText.GetGrayed: boolean;
begin
 Result := FGrayedProp.Value;
end;

function TFlexText.GetLayout: TTextLayout;
begin
 Result := TTextLayout(FLayoutProp.EnumIndex);
end;

function TFlexText.GetWordWrap: boolean;
begin
 Result := FWordWrapProp.Value;
end;

procedure TFlexText.SetAlignment(const Value: TAlignment);
begin
  FAlignmentProp.EnumIndex := integer(Value);
end;

procedure TFlexText.SetGrayed(const Value: boolean);
begin
 FGrayedProp.Value := Value;
end;

procedure TFlexText.SetLayout(const Value: TTextLayout);
begin
 FLayoutProp.EnumIndex := integer(Value);
end;

procedure TFlexText.SetWordWrap(const Value: boolean);
begin
 FWordWrapProp.Value := Value;
end;

function TFlexText.GetTextSize: TSize;
var B: TBitmap;
    R: TRect;
begin
 B := TBitmap.Create;
 try
  SetRectEmpty(R);
  R.Right := (WidthProp.Value - 2*FPenProp.ActiveWidth) div PixelScaleFactor;
  {$IFDEF FG_CBUILDER}DrawTextCpp{$ELSE}DrawText{$ENDIF}(B.Canvas, R,
    True, False);
  Result.cx := R.Right;
  Result.cy := R.Bottom;
 finally
  B.Free;
 end;
end;

// TFlexConnector /////////////////////////////////////////////////////////////

procedure TFlexConnector.ControlCreate;
begin
 inherited;
 SetLength(FPoints, 3);
 FPoints[0] := Point(0, 0);
 FPoints[1] := FPoints[0];
 FPoints[2] := FPoints[0];
 SetLength(FPointTypes, 3);
 FPointTypes[0] := ptNode; //PT_MOVETO;
 FPointTypes[1] := ptNode; //PT_MOVETO;
 FPointTypes[2] := ptEndNode; //PT_LINETO;
 FOrtogonalProp.Value := true;
end;

procedure TFlexConnector.ControlDestroy;
begin
 FLinkAProp.LinkedControl := Nil;
 FLinkBProp.LinkedControl := Nil;
 inherited;
end;

procedure TFlexConnector.CreateProperties;
begin
 inherited;
 FRerouteModeProp := TEnumProp.Create(Props, 'RerouteMode');
 FRerouteModeProp.AddItem('Always');
 FRerouteModeProp.AddItem('AsNeeded');
 FRerouteModeProp.AddItem('Never');
 FOrtogonalProp := TBoolProp.Create(Props, 'Ortogonal');
 FLinkAProp := TLinkPointProp.Create(Props, 'LinkA');
 FLinkAProp.OnLinkedNotify := LinkedNotify;
 FLinkBProp := TLinkPointProp.Create(Props, 'LinkB');
 FLinkBProp.OnLinkedNotify := LinkedNotify;
 FMinimalGapProp := TIntProp.Create(Props, 'MinimalGap');
 FMinimalGapProp.Style := FMinimalGapProp.Style + [ psNonVisual, psScalable ]; 
end;

procedure TFlexConnector.CreateInDesign(var Info: TFlexCreateInDesignInfo);
var Avail: boolean;
begin
 inherited;
 if Info.PointContinueIndex = 0 then
  Avail := not Assigned(LinkAProp.LinkedControl)
 else
 if Info.PointContinueIndex = PointCount-1 then
  Avail := not Assigned(LinkBProp.LinkedControl)
 else
  Avail := false;
 Info.IsContinueAvail := Avail or
   (TRerouteMode(FRerouteModeProp.EnumIndex) <> rmAlways);
   // false; // Can't continue connector
end;

procedure TFlexConnector.CreateCurveGuide(const NewPoint: TPoint;
  var Guide: TFlexEditPointGuide);
var FirstIndex, NextIndex: integer;
    FigIndex, ActiveIndex, i: integer;
    Nearest: TNearestPoint;
begin
 Guide.Count := 0;
 if PointCount = 0 then exit;
 if not FindNearestPathSegment(NewPoint, FirstIndex, NextIndex, @Nearest) then
  exit;
 // Update curve guide
 FigIndex := GetFigureIndex(PointsInfo^, FirstIndex);
 if FigIndex < 0 then exit;
 with PointsInfo.Figures[FigIndex], Guide do begin
  // Builde guide points
  if (NextIndex >= FirstNode) and (NextIndex <= LastNode) then begin
   // Check curve insert
   if (FirstIndex < LastPoint) and
      (Self.PointTypes[FirstIndex+1] = ptControl) then begin
    // Insert curve point
    SetLength(Points, 4);
    SetLength(Types, 4);
    Points[0] := Self.Points[FirstIndex];
    Types[0] := ptNode;
    Points[1] := Self.Points[FirstIndex+1];
    Types[1] := ptControl;
    Points[2] := Self.Points[FirstIndex+2];
    Types[2] := ptControl;
    Points[3] := Self.Points[NextIndex];
    Types[3] := ptEndNode;
    ActiveIndex := 3;
   end else begin
    // Line segment
    SetLength(Points, 2);
    SetLength(Types, 2);
    Points[0] := Self.Points[FirstIndex];
    Types[0] := ptNode;
    Points[1] := Self.Points[NextIndex];
    Types[1] := ptEndNode;
    ActiveIndex := 1;
   end;
   // Check point position
   with Nearest.Point do
    if ((x = Points[0].x) and (y = Points[0].y)) or
       ((x = Points[ActiveIndex].x) and (y = Points[ActiveIndex].y)) then
     exit;
   // Insert new point
   ActiveIndex := InsertPathPoint(Points, Types, 0, ActiveIndex, Nearest.Point);
   if ActiveIndex < 0 then begin
    SetLength(Points, 0);
    SetLength(Types, 0);
   end else begin
    Count := Length(Points);
    // Set Visible flags
    SetLength(Visible, Count);
    for i:=0 to Count-1 do Visible[i] := i = ActiveIndex;
   end;
   // Update NewPoint
   NewPoint := Points[ActiveIndex];
  end;
 end;
end;

function TFlexConnector.GetOrtogonal: boolean;
begin
 Result := FOrtogonalProp.Value;
end;

procedure TFlexConnector.SetOrtogonal(const Value: boolean);
begin
 FOrtogonalProp.Value := Value;
end;

function TFlexConnector.GetRerouteMode: TRerouteMode;
begin
 Result := TRerouteMode(FRerouteModeProp.EnumIndex);
end;

procedure TFlexConnector.SetRerouteMode(const Value: TRerouteMode);
begin
 FRerouteModeProp.EnumIndex := integer(Value);
end;

class function TFlexConnector.IsConnectorControl: boolean;
begin
 Result := true;
end;

procedure TFlexConnector.GetLinkProps(var LinkFirst, LinkLast: TLinkPointProp);
begin
 LinkFirst := FLinkAProp;
 LinkLast := FLinkBProp;
end;

function TFlexConnector.GetLinked: boolean;
begin
 Result := Assigned(LinkAProp.LinkedControl) or
           Assigned(LinkBProp.LinkedControl); 
end;

procedure TFlexConnector.SetBlocked(Value: boolean);
begin
 if FInTransformation then Value := false;
 if Value = FBlocked then exit;
 FBlocked := Value;
 if FBlocked then begin
  // Block pos and size
  LeftProp.Style := LeftProp.Style + [ psReadOnly ];
  TopProp.Style := TopProp.Style + [ psReadOnly ];
  WidthProp.Style := WidthProp.Style + [ psReadOnly ];
  HeightProp.Style := HeightProp.Style + [ psReadOnly ];
 end else begin
  // Unblock pos and size
  LeftProp.Style := LeftProp.Style - [ psReadOnly ];
  TopProp.Style := TopProp.Style - [ psReadOnly ];
  WidthProp.Style := WidthProp.Style - [ psReadOnly ];
  HeightProp.Style := HeightProp.Style - [ psReadOnly ];
 end; 
end;

procedure TFlexConnector.BeginLinkUpdate;
begin
 if (FLinkUpdateCount = 0) and Linked then Blocked := false;
 inc(FLinkUpdateCount);
end;

procedure TFlexConnector.EndLinkUpdate;
begin
 if FLinkUpdateCount = 0 then exit;
 dec(FLinkUpdateCount);
 if (FLinkUpdateCount = 0) and Linked then begin
  Blocked := true;
  
 end;
end;

procedure TFlexConnector.PointsChanged;
begin
 BeginLinkUpdate;
 try
  inherited;
 finally
  EndLinkUpdate;
 end;
end;

procedure TFlexConnector.DoNotify(Notify: TFlexNotify);
var R: TRect;
begin
 case Notify of
  fnLoaded,
  fnEditPoints:
    if not FInTransformation and (Length(FPoints) > 1) then begin
     // Change linked points
     R := DocRect;
     FLinkPointsIniting := true;
     try
      with Points[0] do
       FLinkAProp.LinkPoint := Point(X + R.Left, Y + R.Top);
      with Points[Length(FPoints)-1] do
       FLinkBProp.LinkPoint := Point(X + R.Left, Y + R.Top);
     finally
      FLinkPointsIniting := false;
     end;
    end;
 end;
 inherited;
end;

procedure TFlexConnector.ConnectorMinGapChanged;
begin
 if (FMinimalGapProp.Value = 0) and (RerouteMode <> rmNever) then Reroute;
end;

procedure TFlexConnector.Reroute;
var R: TRect;
    PointA, PointB: TPoint;
    ExistA, ExistB: boolean;
    Params: TRerouteParams;
    InProcess: TObject;
    Action: THistoryAction;
begin
 if Assigned(Owner) then begin
  InProcess := Owner.History.InProcessSource;
  if Assigned(InProcess) and
     ((InProcess = FLinkAProp.LinkedControl) or
      (InProcess = FLinkBProp.LinkedControl)) then exit;
 end; 
 if Assigned(Owner) then begin
  //if Owner.History.State in [hsUndo, hsRedo] then exit;
  Action := Owner.History.BeginPanelGroup(TPanelRerouteHistoryGroup);
 end else
  Action := Nil;
 with Params do
 try
  SelfLink := FLinkAProp.LinkedControl = FLinkBProp.LinkedControl;
  if FMinimalGapProp.Value = 0
   then LinkMinGap := Owner.ConnectorsMinGap
   else LinkMinGap := FMinimalGapProp.Value;
  Mode := RerouteMode;
  Ortogonal := Self.Ortogonal;
  R := DocRect;
  with FLinkAProp do begin
   // Get link point in self coords
   with LinkPoint do begin
    PointA.x := x - R.Left;
    PointA.y := y - R.Top;
   end;
   // Get LinkA control rect
   ExistA := Assigned(LinkedControl);
   if ExistA then begin
    RangeA := LinkedControl.DocRect;
    OffsetRect(RangeA, -R.Left, -R.Top);
   end else
    RangeA := Rect(PointA.x, PointA.y, PointA.x, PointA.y);
  end;
  with FLinkBProp do begin
   with LinkPoint do begin
    PointB.x := x - R.Left;
    PointB.y := y - R.Top;
   end;
   // Get LinkB control rect
   ExistB := Assigned(LinkedControl);
   if ExistB then begin
    RangeB := LinkedControl.DocRect;
    OffsetRect(RangeB, -R.Left, -R.Top);
   end else
    RangeB := Rect(PointB.x, PointB.y, PointB.x, PointB.y);
  end;
  // Save action
  RecordPointsAction;
  // Do reroute func
  if ExistA or ExistB then begin
   LinkPointA := PointA;
   LinkPointB := PointB;
   if ConnectorReroute(FPoints, FPointTypes, Params) then
    PointsChanged;
  end else begin
   // Replace link points
   if Length(FPoints) < 2 then begin
    SetLength(FPoints, 2);
    SetLength(FPointTypes, 2);
    FPointTypes[0] := ptNode;
    FPointTypes[1] := ptEndNode;
   end;
   FPoints[0] := PointA;
   // Check first start
   if (Length(FPoints) = 3) and
      (WidthProp.Value = 0) and (HeightProp.Value = 0) then
    FPoints[1] := PointA;
   FPoints[Length(FPoints)-1] := PointB;
   PointsChanged;
  end;
 finally
  if Assigned(Action) then
   Owner.History.EndPanelGroup(TPanelRerouteHistoryGroup);
 end;
end;

procedure TFlexConnector.LinkedNotify(Sender: TObject; Source: TNotifyLink;
  const Info: TNotifyLinkInfo);
begin
 case Info.Code of
  ncControlNotify:
    if not FInTransformation and not FLinkPointsIniting and
       (Length(FPoints) > 1) and
      ((Info.ControlNotify = fnLinkPoints) or
       (Info.ControlNotify = fnRect)) then begin
     //if Assigned(FLinkAProp.LinkedControl) and
     //   Assigned(FLinkBProp.LinkedControl) then FFirstDrawing := false;
     Reroute;
    end;
 end;
end;

procedure TFlexConnector.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 if Prop = FMinimalGapProp then begin
  if FMinimalGapProp.Value < 0 then
   FMinimalGapProp.Value := 0
  else
  if (RerouteMode <> rmNever) and not (fsLoading in State) then
   Reroute;
 end else
 if not FInTransformation then begin
  if (Prop = FLinkAProp) or (Prop = FLinkBProp) then
   Blocked := Linked
  else
  if (Prop = FRerouteModeProp) and not
     (Assigned(Owner) and Owner.IsLoading) then begin
   if RerouteMode <> rmNever then Reroute;
  end else
  if (Prop = FOrtogonalProp) then
   if RerouteMode <> rmNever then Reroute;
 end;
 inherited;
end;

procedure TFlexConnector.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = FOrtogonalProp then
  IsStored := not FOrtogonalProp.Value
 else
 if Prop = FRerouteModeProp then
  IsStored := FRerouteModeProp.EnumIndex <> integer(rmAlways)
 else
 if (Prop = FLinkAProp) or (Prop = FLinkBProp) then
  IsStored := Assigned(TLinkPointProp(Prop).LinkedControl)
 else
 if Prop = FMinimalGapProp then
  IsStored := FMinimalGapProp.Value > 0
 else
  inherited;
end;

procedure TFlexConnector.SetDocRect(Value: TRect);
begin
 if not Blocked then
  inherited;
end;

procedure TFlexConnector.MirrorInResize(HMirror, VMirror: boolean);
begin
 if not Blocked then
  inherited;
end;

procedure TFlexConnector.ControlTranslate(const TranslateInfo: TTranslateInfo);
begin
 if not Blocked then
  inherited
end;

procedure TFlexConnector.BeginSelectionTransformation;
begin
 if FInTransformation then exit;
 // Check full movement (not via linked points)
 FInTransformation :=
  (not Assigned(FLinkAProp.LinkedControl) or
   Owner.IsSelected(FLinkAProp.LinkedControl)) and
  (not Assigned(FLinkBProp.LinkedControl) or
   Owner.IsSelected(FLinkBProp.LinkedControl));
 if FInTransformation then
  // Unblock properties for free trasformation
  Blocked := false;
end;

procedure TFlexConnector.EndSelectionTransformation;
begin
 if FInTransformation then begin
  FInTransformation := false;
  // Restore blocking
  Blocked := Linked;
 end;
end;

function TFlexConnector.MovePathPoints(PointIndex: integer; var Delta: TPoint;
  Selected: TSelectedArray; Smooth: boolean = false;
  Symmetric: boolean = false): boolean;
var MovePoint: TPoint;
    PrevHoriz: boolean;
    NextHoriz: boolean;
    Threshold: integer;
    i: integer;

 function IsNear(Index1, Index2: integer; IsPrev: boolean): boolean;
 var Dist: TPoint;
 begin
  Dist.x := Abs(FPoints[Index1].x - FPoints[Index2].x + Delta.x);
  Dist.y := Abs(FPoints[Index1].y - FPoints[Index2].y + Delta.y);
  if PrevHoriz = IsPrev
   then Result := Dist.x < Threshold
   else Result := Dist.y < Threshold;
 end;

begin
 if //FFirstDrawing or
   not Ortogonal or (PointIndex < 0) or (Length(FPoints) < 2) then begin
  // Free move
  Result := inherited MovePathPoints(PointIndex, Delta, Selected, Smooth,
    Symmetric);
 end else begin
  RecordPointsAction;
  // Ortogonal move only
  Result := false;
  i := 1;
  PrevHoriz := true;
  repeat
   if (FPoints[i-1].x <> FPoints[i].x) or
      (FPoints[i-1].y <> FPoints[i].y) then begin
    // Define segment position
    PrevHoriz := FPoints[i-1].y = FPoints[i].y;
    if (i and 1) <> (PointIndex and 1) then PrevHoriz := not PrevHoriz;
    break;
   end;
   inc(i);
  until i = Length(FPoints);
  NextHoriz := not PrevHoriz;
  if (Length(FPoints) = 3) and (PointIndex = 1) then begin
   // Disable movement
   Delta.y := 0;
   Delta.x := 0;
   exit;
  end;
  if PointIndex = 1 then begin
   // Fix movement for second point
   if PrevHoriz
    then Delta.y := 0
    else Delta.x := 0;
  end else
  if PointIndex = Length(FPoints)-2 then begin
   // Fix movement for penultimate point
   if NextHoriz
    then Delta.y := 0
    else Delta.x := 0;
  end;
  if (PointIndex > 0) and (PointIndex < Length(FPoints)-1) and
    (RerouteMode = rmAlways) then
   RerouteMode := rmNever; 
  // Calc new point position
  MovePoint := FPoints[PointIndex];
  // Check nearest
  Threshold := UnscaleValue(SelectionThreshold, Owner.Scale);
  if (PointIndex > 1) and IsNear(PointIndex, PointIndex-1, true) then begin
   // "Stick"
   if PrevHoriz
    then Delta.x := FPoints[PointIndex-1].x - FPoints[PointIndex].x
    else Delta.y := FPoints[PointIndex-1].y - FPoints[PointIndex].y;
  end;
  if (PointIndex < Length(FPoints)-2) and
    IsNear(PointIndex, PointIndex+1, false) then begin
   // "Stick"
   if NextHoriz
    then Delta.x := FPoints[PointIndex+1].x - FPoints[PointIndex].x
    else Delta.y := FPoints[PointIndex+1].y - FPoints[PointIndex].y;
  end; 
  inc(MovePoint.X, Delta.X);
  inc(MovePoint.Y, Delta.Y);
  // Move previous and next points
  if PrevHoriz then begin
   // Previous segment horizontal and next vertical
   if PointIndex > 0 then FPoints[PointIndex-1].y := MovePoint.y;
   if PointIndex < Length(FPoints)-1 then FPoints[PointIndex+1].x := MovePoint.x;
  end else begin
   // Previous segment vertical and next horizontal
   if PointIndex > 0 then FPoints[PointIndex-1].x := MovePoint.x;
   if PointIndex < Length(FPoints)-1 then FPoints[PointIndex+1].y := MovePoint.y;
  end;
  // Set new position
  FPoints[PointIndex] := MovePoint;
  PointsChanged;
  Result := true;
 end;
 if Result and (PointsDesigning > 0) then begin
  FDesignMoving := true;
  FDesignMovedFirst := PointIndex;
  FDesignMovedNext := PointIndex;
  if not Owner.Schemes.ConnectorsKeepLinkProp.Value and
     (Owner.DefaultLinkPoint.Index < 0) then
   // Reset link
   if PointIndex = 0 then
    LinkAProp.LinkedControl := Nil
   else
   if PointIndex = Length(FPoints)-1 then
    LinkBProp.LinkedControl := Nil;
 end;
end;

function TFlexConnector.MovePathSegment(FirstIndex, NextIndex: integer;
  var Delta: TPoint; const SegmentCurvePos: double): boolean;
var Threshold: integer;
    HorizSeg: boolean;

 function IsNear(Index1, Index2: integer): boolean;
 var Dist: TPoint;
 begin
  Dist.x := Abs(FPoints[Index1].x - FPoints[Index2].x + Delta.x);
  Dist.y := Abs(FPoints[Index1].y - FPoints[Index2].y + Delta.y);
  if HorizSeg
   then Result := Dist.y < Threshold
   else Result := Dist.x < Threshold;
 end;

begin
 if (FirstIndex = 0) and Assigned(LinkAProp.LinkedControl) or
    (NextIndex = PointCount-1) and Assigned(LinkBProp.LinkedControl) then begin
  // Last and first segments can't move
  Delta.x := 0;
  Delta.y := 0;
  Result := false;
  exit;
 end;
 if not Ortogonal then begin
  // Free move
  Result := inherited MovePathSegment(FirstIndex, NextIndex, Delta,
    SegmentCurvePos);
 end else begin
  // Ortogonal move only 
  RecordPointsAction;
  // Fix ortogonal movement
  HorizSeg := FPoints[FirstIndex].y = FPoints[NextIndex].y;
  if HorizSeg
   then Delta.x := 0
   else Delta.y := 0;
  // Check nearest
  Threshold := UnscaleValue(SelectionThreshold, Owner.Scale);
  if (FirstIndex > 1) and IsNear(FirstIndex, FirstIndex-1) then begin
   // "Stick"
   if not HorizSeg
    then Delta.x := FPoints[FirstIndex-1].x - FPoints[FirstIndex].x
    else Delta.y := FPoints[FirstIndex-1].y - FPoints[FirstIndex].y;
  end;
  if (NextIndex < Length(FPoints)-2) and IsNear(NextIndex, NextIndex+1) then begin
   // "Stick"
   if not HorizSeg
    then Delta.x := FPoints[NextIndex+1].x - FPoints[NextIndex].x
    else Delta.y := FPoints[NextIndex+1].y - FPoints[NextIndex].y;
  end;
  Result := inherited MovePathSegment(FirstIndex, NextIndex,
    Delta, SegmentCurvePos);
 end;
 if Result and (PointsDesigning > 0) then begin
  FDesignMoving := true;
  FDesignMovedFirst := FirstIndex;
  FDesignMovedNext := NextIndex;
  if RerouteMode = rmAlways then RerouteMode := rmNever;
 end;
end;

function TFlexConnector.InsertNearestPoint(const Point: TPoint): integer;
var FirstIndex, NextIndex: integer;
begin
 Result := -1;
 if Ortogonal then begin
  // Find path segment for insert
  if not FindNearestPathSegment(Point, FirstIndex, NextIndex) then exit;
  Result := InternalInsertPoints(NextIndex, 2);
  if Result < 0 then exit;
  FPoints[Result+0] := Point;
  FPointTypes[Result+0] := ptNode;
  FPoints[Result+1] := Point;
  FPointTypes[Result+1] := ptNode;
  PointsChanged;
  if RerouteMode = rmAlways then RerouteMode := rmNever;
 end else begin
  Result := inherited InsertNearestPoint(Point);
  RerouteMode := rmNever;
 end;
end;

function TFlexConnector.InsertPoint(Index: integer;
  const Point: TPoint): integer;
begin
 Result := inherited InsertPoint(Index, Point);
 if Assigned(Owner) and (Owner.ToolMode = ftmCurveContinue) then begin
  if RerouteMode = rmAlways then RerouteMode := rmNever;
 end;
end;

procedure TFlexConnector.DeletePoint(Index: integer);
begin
 // Check link breaking
 if not Owner.Schemes.ConnectorsKeepLinkProp.Value and
   (FPointTypes[Index] <> ptControl) and (Length(FPoints) > 2) and
    ((Index = 0) or (Index = Length(FPoints)-1)) then begin
  if Index = 0 then
   FLinkAProp.LinkedControl := Nil
  else
   FLinkBProp.LinkedControl := Nil;
 end;
 if Ortogonal and (FPointTypes[Index] <> ptControl) then
  with FPoints[Index] do begin
   // Check previous point
   if ((Index = 0) or (Index = Length(FPoints)-1)) and
     (Length(FPoints) > 2) then begin
    // Delete first or last point in connector path
    inherited DeletePoint(Index);
    if RerouteMode = rmAlways then RerouteMode := rmNever;
   end else
   if (Index > 0) and (FPointTypes[Index-1] = ptNode) and
      (FPoints[Index-1].x = x) and (FPoints[Index-1].y = y) then begin
    // Delete two nodes
    inherited DeletePoint(Index-1);
    inherited DeletePoint(Index-1);
    if RerouteMode = rmAlways then RerouteMode := rmNever;
   end else
   // Check next point
   if (Index < Length(FPoints)-1) and (FPointTypes[Index+1] = ptNode) and
      (FPoints[Index+1].x = x) and (FPoints[Index+1].y = y) then begin
    // Delete two nodes
    inherited DeletePoint(Index);
    inherited DeletePoint(Index);
    if RerouteMode = rmAlways then RerouteMode := rmNever;
   end;
  end
 else
  inherited;
end;

procedure TFlexConnector.EndPointsDesigning;

 function IsSame(Index1, Index2: integer): boolean;
 begin
  Result := (FPoints[Index1].x = FPoints[Index2].x) and
            (FPoints[Index1].y = FPoints[Index2].y);
 end;

begin
 if FDesignMoving and (PointsDesigning = 1) and Ortogonal then begin
  if PointCount > 4 then begin
   // Check equal points deleting
   FDesignMoving := false;
   // Check previous point
   if (FDesignMovedFirst > 1) and
     IsSame(FDesignMovedFirst, FDesignMovedFirst-1) then begin
    // Delete two points
    inherited DeletePoint(FDesignMovedFirst-1);
    inherited DeletePoint(FDesignMovedFirst-1);
    dec(FDesignMovedNext, 2);
   end;
   if (FDesignMovedNext < Length(FPoints)-2) and
     IsSame(FDesignMovedNext, FDesignMovedNext+1) then begin
    // Delete two points
    inherited DeletePoint(FDesignMovedNext);
    inherited DeletePoint(FDesignMovedNext);
   end;
  end;
  // Check rerouting
  if RerouteMode <> rmNever then Reroute;
 end;
 inherited;
end;

// TFlexRegularPolygon ////////////////////////////////////////////////////////

procedure TFlexRegularPolygon.ControlCreate;
begin
 Width := 1;
 Height := 1;
 FBrushProp.Color := clNone;
 FBrushProp.Style := bsSolid;
 FSidesProp.Value := 4;
 inherited;
 Visible := True;
end;

procedure TFlexRegularPolygon.CreateProperties;
begin
 inherited;
 FAngleProp := TIntProp.Create(Props, 'Angle');
 FSidesProp := TIntProp.Create(Props, 'Sides');
end;

procedure TFlexRegularPolygon.ControlTranslate(const TranslateInfo: TTranslateInfo);
var NewAngle, StartAngle: integer;
begin
 inherited;
 NewAngle := FAngleProp.Value mod 360;
 if NewAngle < 0 then inc(NewAngle, 360);
 StartAngle := NewAngle;
 if TranslateInfo.Mirror then NewAngle := -NewAngle;
 inc(NewAngle, TranslateInfo.Rotate);
 NewAngle := NewAngle mod 360;
 if NewAngle < 0 then inc(NewAngle, 360);
 if NewAngle <> StartAngle then FAngleProp.Value := NewAngle;
 FBrushProp.Translate(TranslateInfo);
end;

function TFlexRegularPolygon.CreateCurveControl: TFlexControl;
begin
 Result := TFlexCurve.Create(Owner, Parent, Layer);
 try
  Result.BeginUpdate;
  try
   Result.DocRect := DocRect;
   // Copy properties
   FlexControlCopy(Self, Result);
   // Make points data
   Result.SetPointsEx(FPoints, FPointTypes);
  finally
   Result.EndUpdate;
  end;
 except
  Result.Free;
  raise;
 end;
end;

function TFlexRegularPolygon.CreatePolygonRegion(const PaintRect: TRect; Inflate: boolean = false): HRGN;
var R: TRect;
    PenWidth: integer;
    Rgn: HRGN;
    PaintPoints: TPointArray;
begin
 R := PaintRect;
 PenWidth := ScaleValue(FPenProp.ActiveWidth, Owner.Scale);
 if Inflate then begin
  if (PenWidth > SelectionThreshold) then PenWidth := SelectionThreshold;
  InflateRect(R, PenWidth, PenWidth);
 end;
 // Create polygon region
 BuildPoints(PaintPoints, R);
 Result := CreatePolygonRgn(PaintPoints[0], Length(PaintPoints), WINDING);
 if Inflate and FBrushProp.IsClear and
   not (Assigned(Owner) and Owner.SelectAsFilled) then begin
  // Restore paint rect
  InflateRect(R, -PenWidth, -PenWidth);
  // Erase polygon "hole"
  PenWidth := ScaleValue(FPenProp.ActiveWidth, Owner.Scale);
  if PenWidth < SelectionThreshold+1 then PenWidth := SelectionThreshold+1;
  InflateRect(R, -PenWidth, -PenWidth);
  // Create polygon region
  BuildPoints(PaintPoints, R);
  Rgn := CreatePolygonRgn(PaintPoints[0], Length(PaintPoints), WINDING);
  // Create "hole"
  CombineRgn(Result, Result, Rgn, RGN_XOR);
  DeleteObject(Rgn);
 end;
end;

function TFlexRegularPolygon.GetAnchorPoint: TPoint;
begin
 Result := ClientToOwner(FPoints[0]);
end;

function TFlexRegularPolygon.GetDefaultLinkPoint(Index: integer): TPoint;
begin
 Result := FPoints[Index];
end;

function TFlexRegularPolygon.GetDefaultLinkPointCount: integer;
begin
 Result := Length(FPoints);
end;
                       
function TFlexRegularPolygon.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
begin
 Result := inherited GetRefreshRect(RefreshX, RefreshY);
 // Check inflate refresh area to pen width
 if Assigned(FPenProp) and (FPenProp.ActiveWidth > 0) then
  InflateRect(Result,
    FPenProp.ActiveWidth div 2 + 2*PixelScaleFactor,
    FPenProp.ActiveWidth div 2 + 2*PixelScaleFactor );
end;

function TFlexRegularPolygon.IsPointInside(PaintX, PaintY: integer): boolean;
var Pt: TPoint;
    ActWidth, PenWidth: integer;
begin
 Pt.x := PaintX;
 Pt.y := PaintY;
 Pt := OwnerToClient(Pt);
 ActWidth := FPenProp.ActiveWidth;
 PenWidth := ActWidth div 2 + UnscaleValue(SelectionThreshold, Owner.Scale);
 Result := (Pt.x >= -PenWidth) and (Pt.x <= WidthProp.Value + PenWidth) and
           (Pt.y >= -PenWidth) and (Pt.y <= HeightProp.Value + PenWidth);
 if not Result then exit;
 Result := PointOnPath(FPoints, FPointTypes, Pt, ActWidth > 0,
   not FBrushProp.IsClear or (Assigned(Owner) and Owner.SelectAsFilled),
   PenWidth, Nil, PointsInfo);
end;

procedure TFlexRegularPolygon.Paint(Canvas: TCanvas; var PaintRect: TRect);
var PrevRgn, ClipRgn: HRGN;
    PaintPoints: TPointArray;

 procedure CanvasSetup;
 begin
  FPenProp.Setup(Canvas, Owner.Scale);
  FBrushProp.Setup(Canvas, Owner.Scale);
 end;
    
begin
 BuildPoints(PaintPoints, PaintRect);
 CanvasSetup;
 if FBrushProp.IsPaintAlternate then begin
  PrevRgn := 0;
  ClipRgn := CreatePolygonRegion(PaintRect);
  try
   PrevRgn := IntersectClipRgn(Canvas, ClipRgn);
   FBrushProp.PaintAlternate(Canvas, PaintRect, Owner.PaintRect,
     Owner.Scale, Owner.UseImageClipTransparent);
  finally
   SelectClipRgn(Canvas.Handle, PrevRgn);
   DeleteObject(PrevRgn);
   DeleteObject(ClipRgn);
  end;
  CanvasSetup;
 end;
 Canvas.Polygon(PaintPoints);
end;

procedure TFlexRegularPolygon.PropChanged(Sender: TObject; Prop: TCustomProp);
var PrevDX, PrevDY: double;
    NeedRebuildPoints: boolean;
    i, Count: integer;
begin
 NeedRebuildPoints := False;
 if (Prop = FSidesProp) or (Prop = FAngleProp) then begin
  if (Prop = FSidesProp) and (FSidesProp.Value < 3) then begin
   FSidesProp.Value := 3;
   Exit;
  end;
  PrevDX := FEtalonDX;
  PrevDY := FEtalonDY;
  RebuildEtalon;
  if (Prop <> FAngleProp) and
     (FEtalonDX <> 0) and (FEtalonDY <> 0) and
     (PrevDX <> 0) and (PrevDY <> 0) then begin
   // Fix height to preserve polygon proportions
   Owner.History.DisableRecording;
   try
    HeightProp.Value := Round((PrevDX / PrevDY) * (FEtalonDY / FEtalonDX) * HeightProp.Value);
   finally
    Owner.History.EnableRecording;
   end;
  end;
  NeedRebuildPoints := True;
 end else
 if (Prop = WidthProp) or (Prop = HeightProp) then
  NeedRebuildPoints := True;
 if NeedRebuildPoints then begin
  BuildPoints(FPoints, Rect(0, 0, WidthProp.Value, HeightProp.Value));
  Count := Length(FPoints);
  SetLength(FPointTypes, Count);
  if Count > 0 then begin
   for i:=0 to Count-1 do FPointTypes[i] := ptNode;
   FPointTypes[Count-1] := ptEndNodeClose;
  end;
 end;
 inherited;
end;

procedure TFlexRegularPolygon.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string);
begin
 if Prop = FSidesProp then
  IsStored := FSidesProp.Value <> 4
 else
 if Prop = FAngleProp then
  IsStored := FAngleProp.Value <> 0
 else
  inherited;
end;

procedure TFlexRegularPolygon.RebuildEtalon;
var Bounds: record Left, Top, Right, Bottom: double; end;
    PointAngle, StepAngle: double;
    i: integer;
begin
 if FSidesProp.Value < 3 then begin
  SetLength(FEtalon, 0);
  FEtalonDX := 0;
  FEtalonDY := 0;
 end;
 SetLength(FEtalon, FSidesProp.Value);
 StepAngle := 2 * pi / FSidesProp.Value;
 PointAngle := pi * (90 + FAngleProp.Value) / 180;
 with Bounds do begin
  Left := 0.0;
  Top := 0.0;
  Right := 0.0;
  Bottom := 0.0;
  for i:=0 to FSidesProp.Value-1 do begin
   FEtalon[i].X := cos(PointAngle);
   if Left > FEtalon[i].X then Left := FEtalon[i].X else
   if Right < FEtalon[i].X then Right := FEtalon[i].X;
   FEtalon[i].Y := -sin(PointAngle);
   if Top > FEtalon[i].Y then Top := FEtalon[i].Y else
   if Bottom < FEtalon[i].Y then Bottom := FEtalon[i].Y;
   PointAngle := PointAngle + StepAngle;
  end;
  for i:=0 to FSidesProp.Value-1 do begin
   FEtalon[i].X := FEtalon[i].X - Left;
   FEtalon[i].Y := FEtalon[i].Y - Top;
  end;
  FEtalonDX := Right - Left;
  FEtalonDY := Bottom - Top;
  FEtalonDXDY := FEtalonDX / FEtalonDY;
 end;
end;

procedure TFlexRegularPolygon.BuildPoints(var Points: TPointArray;
  const R: TRect);
var Count, i: integer;
    ScaleX: double;
    ScaleY: double;
begin
 Count := Length(FEtalon);
 SetLength(Points, Count);
 if FEtalonDX > 0
  then ScaleX := (R.Right - R.Left) / FEtalonDX
  else ScaleX := 0;
 if FEtalonDY > 0
  then ScaleY := (R.Bottom - R.Top) / FEtalonDY
  else ScaleY := 0;
 for i:=0 to Count-1 do begin
  Points[i].X := R.Left + Round(ScaleX * FEtalon[i].X);
  Points[i].Y := R.Top + Round(ScaleY * FEtalon[i].Y);
 end;
end;

///////////////////////////////////////////////////////////////////////////////

procedure RegisterStdControls;
begin
 RegisterFlexControl(TFlexBox);
 RegisterFlexControl(TFlexEllipse);
 RegisterFlexControl(TFlexArc);
 RegisterFlexControl(TFlexPicture);
 RegisterFlexControl(TFlexText);
 RegisterFlexControl(TFlexCurve);
 RegisterFlexControl(TFlexConnector);
 RegisterFlexControl(TFlexRegularPolygon);
end;

{ TFlexControlWithPenAndBrush }

procedure TFlexControlWithPenAndBrush.CreateProperties;
begin
  inherited;
  FBrushProp := TBrushProp.Create(Props, 'Brush');
  FPenProp := TPenProp.Create(Props, 'Pen');
end;

initialization
  RegisterStdControls;

end.
