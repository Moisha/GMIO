/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    FlexGraphics library base classes                //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexBase;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes, Messages, Forms, Controls, SysUtils, ExtCtrls, Graphics,
  Dialogs, Menus, Printers, Consts, ClipBrd,
  {$IFDEF FG_D6} Variants, RTLConsts, {$ENDIF}
  FlexProps, FlexUtils, FlexPath, FlexAlpha, FlexHistory;

const
  FlexGraphicsVersion = 170;

  ClassNamePrefix  = 'TFlex';

  MinScale = 1;
  MaxScale = 100 * PixelScaleFactor div 2;

  StdConnectorsMinGap = 8 * PixelScaleFactor;

  // Constants in pixels
  AnchorPointSize = 5;
  SelectionMarkerSize = 6;
  SelectionInflate = 8;
  SelectionPointInflate = 2;
  SelectionThreshold = 4;
  ConnectorStickThreshold = 16;
  DefaultLinkPointSize = 7;

  // Perfomance params
  MaxSinglePointsInvalidation = 20;

type
  TResizeCursor   = ( rcrNone,
                      rcrTopLeft, rcrTop, rcrTopRight, rcrLeft, rcrRight,
                      rcrBottomLeft, rcrBottom, rcrBottomRight );
  TFlexToolMode   = ( ftmSelect, ftmSelecting, ftmZoom, ftmZooming,
                      ftmMove, ftmMoving, ftmResize, ftmResizing, ftmCreating,
                      ftmPointEdit, ftmPointSelecting, ftmPointEditing,
                      ftmCurveMoving, ftmCurveContinue,
                      ftmPan, ftmPanning,
                      ftmUser );
  TFlexAlign      = ( faLeft, faHCenter, faRight,
                      faTop, faVCenter, faBottom, faCenter );
  TFlexSnap       = ( snLeft, snTop, snRight, snBottom, snCenter, snAll );
  TFlexSnaps      = set of TFlexSnap;
  TFlexStateItem  = ( fsCreating, fsDestroying, fsAdding, fsExtracting,
                      fsResizing, fsEndUpdating, fsRecording, fsLoading );
  TFlexState      = set of TFlexStateItem;

  TFlexLoadFunc   = ( lfNew, lfAdd );

  TFlexGridStyle  = ( gsLines, gsDots );

  TFlexAlphaBufferMode = ( amAuto, amRequired, amNotRequired );

  PFlexEditPointGuide = ^TFlexEditPointGuide;
  TFlexEditPointGuide = record
   Count: integer;
   NewPoint: TPoint;
   Points: TPointArray;
   PaintPoints: TPointArray;
   Types: TPointTypeArray;
   Visible: TSelectedArray;
  end;

  TFlexPanel = class;
  TFlexLayer = class;
  TFlexControl = class;
  TFlexServiceControl = class;
  TFlexCustomScheme = class;

  TFlexNotifyEvent = procedure(Sender: TObject; Control: TFlexControl;
    Notify: TFlexNotify) of object;

  TFlexControlCreateEvent = procedure(Sender: TObject; Control: TFlexControl;
    var ControlDocRect: TRect) of object;

  TFlexControlClass = class of TFlexControl;
  TFlexControlClasses = array of TFlexControlClass;

  PFlexCreateInDesignInfo = ^TFlexCreateInDesignInfo;
  TFlexCreateInDesignInfo = record
   IsPointEdit: boolean;
   PointEditIndex: integer;
   IsContinueAvail: boolean;
   PointContinueIndex: integer;
  end;

  PPassControlRec = ^TPassControlRec;
  TPassControlRec = record
   Control: TFlexControl;
   FirstControl: TFlexControl;
   Indexes: array of integer;
  end;

  PLinkPointInfo = ^TLinkPointInfo;
  TLinkPointInfo = record
   Control: TFlexControl;
   Index: integer;
   DocPos: TPoint;
   Size: integer; // DefaultLinkPointSize
  end;

  TSchemeRefProp = class(TCustomProp)
  private
   FValue: TFlexControl;
   procedure SetValue(const Value: TFlexControl);
  protected
   function  GetDisplayValue: string; override;
   function  GetNamesControl: TFlexControl; virtual;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   procedure GetEnumList(List: TStrings); override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
   property  Value: TFlexControl read FValue write SetValue;
  end;

  TLinkProp = class(TCustomProp)
  private
   FValue: string;
   FLinkChanged: boolean;
   FOnLinkedNotify: TNotifyLinkEvent;
   FOnLinkedFreeNotify: TNotifyLinkEvent;
   function  GetValue: string;
   procedure SetValue(const Value: string);
   procedure SetControl(const Value: TFlexControl);
   procedure SetProp(const Value: TCustomProp);
  protected
   FControl: TFlexControl;
   FProp: TCustomProp;
   FLinkToOtherOwner: boolean;
   function  GetDisplayValue: string; override;
   procedure SetDisplayValue(const Value: string); override;
   function  SetLink(AControl: TFlexControl; AProp: TCustomProp): boolean;
     virtual;
   procedure LinkedNotify(Sender: TObject; Source: TNotifyLink;
     const Info: TNotifyLinkInfo); virtual;
   procedure LinkedFreeNotify(Sender: TObject; Source: TNotifyLink;
     const Info: TNotifyLinkInfo); virtual;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   destructor Destroy; override;
   procedure SetPropValue(const PropName: string; Value: Variant); override;
   function  GetPropValue(const PropName: string): Variant; override;
   property  LinkedControl: TFlexControl read FControl write SetControl;
   property  LinkedProp: TCustomProp read FProp write SetProp;
   property  Value: string read GetValue write SetValue;
   property  NotifyLink read FNotifyLink;
   property  OnLinkedNotify: TNotifyLinkEvent read FOnLinkedNotify
     write FOnLinkedNotify;
   property  OnLinkedFreeNotify: TNotifyLinkEvent read FOnLinkedFreeNotify
     write FOnLinkedFreeNotify;
  end;

  TLinkPointProp = class(TLinkProp)
  private
   FConnector: TFlexControl;
   FLinkPoint: TPoint;
   FSavedPoint: TPoint;
   FSavedRect: TRect;
   procedure SetLinkPoint(const Value: TPoint);
  protected
   function  SetLink(AControl: TFlexControl; AProp: TCustomProp): boolean;
     override;
   procedure LinkedNotify(Sender: TObject; Source: TNotifyLink;
     const Info: TNotifyLinkInfo); override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   function  IndexOfLinkPoint: integer;
   property  LinkedProp: TCustomProp read FProp;
   property  LinkPoint: TPoint read FLinkPoint write SetLinkPoint;
  end;

  TLayerProp = class(TStrProp)
  private
   function  GetControl: TFlexLayer;
   procedure SetControl(const Value: TFlexLayer);
  protected
   procedure SetValue(const Value: string); override;
  public
   constructor Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
   procedure GetEnumList(List: TStrings); override;
   property  Control: TFlexLayer read GetControl write SetControl;
  end;

  TFlexControl = class(TPersistent)
  private
   FOwner: TFlexPanel;
   FParent: TFlexControl;
   FLayer: TFlexLayer;
   FState: TFlexState;
   FControls: TList;
   FProps: TPropList;
   FCheckID: LongWord;
   FNonVisual: boolean;
   FNoPaintRectCheck: boolean;
   FUpdateCounter: integer;
   FPointsDesigning: integer;
   FNotifyLink: TNotifyLink;
   FLinkPoints: TList;
   FLinkPointsChanged: boolean;
   FRectChanged: boolean;
   FTranslateCount: integer;
   FNameProp: TStrProp;
   FLeftProp: TIntProp;
   FTopProp: TIntProp;
   FWidthProp: TIntProp;
   FHeightProp: TIntProp;
   FIdProp: TLongWordProp;
   FTagProp: TIntProp;
   FVisibleProp: TBoolProp;
   FLayerProp: TLayerProp; //TStrProp;
   FHintProp: TStrListProp;
   FShowHintProp: TBoolProp;
   FReferenceProp: TSchemeRefProp;
   FUserDataProp: TUserDataProp;
   FTransparencyProp: TIntProp;
   FSelectableProp: TBoolProp;
   FChannelsProp: TChannelProp;
   FControlObjectProp: TControlObjectProp;
   FClickActionProp: TScriptProp;
   function  GetControl(Index: integer): TFlexControl;
   function  GetCount: integer;
   function  GetIsSelected: boolean;
   function  GetName: string;
   function  GetVisual: boolean;
   function  GetID: LongWord;
   function  GetHeight: integer;
   function  GetLeft: integer;
   function  GetTop: integer;
   function  GetWidth: integer;
   function  GetHint: string;
   function  GetShowHint: boolean;
   function  GetTag: integer;
   function  GetVisible: boolean;
   procedure SetParent(const Value: TFlexControl);
   procedure SetName(const Value: string);
   procedure SetLeft(Value: integer);
   procedure SetTop(Value: integer);
   procedure SetWidth(Value: integer);
   procedure SetHeight(Value: integer);
   procedure SetID(Value: LongWord);
   procedure SetLayer(Value: TFlexLayer);
   procedure SetHint(const Value: string);
   procedure SetShowHint(Value: boolean);
   procedure SetTag(Value: integer);
   procedure SetVisible(Value: boolean);
   procedure SetIsSelected(Value: boolean);
   function  GetDocRect: TRect;
   function  GetPaintRect: TRect;
   procedure GetLayerStrProp(Sender: TObject; out Value: string);
   function  GetByName(const Name: string): TFlexControl;
   function  GetParentScheme: TFlexCustomScheme;
   procedure SetOwner(Value: TFlexPanel);
   function  GetRef: TFlexCustomScheme;
   procedure SetRef(Value: TFlexCustomScheme);
   function  GetNode(NodeIndex: integer): TPoint;
   procedure SetNode(NodeIndex: integer; const Value: TPoint);
   function  GetNodeCount: integer;
   function  GetNodeType(NodeIndex: integer): TPointType;
   procedure SetNodeType(NodeIndex: integer; const Value: TPointType);
   function  GetLinkPoint(Index: integer): TPoint;
   function  GetLinkPointCount: integer;
   procedure SetLinkPoint(Index: integer; const Value: TPoint);
   function  GetTransparency: integer;
   procedure SetTransparency(const Value: integer);
   function  GetSelectable: boolean;
   procedure SetSelectable(const Value: boolean);
  protected
   FAnchorEnabled: boolean;
   FSavedDocRect: TRect;
   FLinkSavedBounds: TRect;
   FResizingTopLeft: TPoint;
   FResizingRect: TRect;
   FResizingLinkPoints: TPointArray;
   FUserObject: TObject;
   FPaintAlphaBufferMode: TFlexAlphaBufferMode;
   FPaintAlphaBuffer: TAlphaBuffer;
   FGroupUngroupLevel: integer;
   procedure InitializeControl(AParent: TFlexControl;
     ALayer: TFlexLayer); virtual;
   procedure FinalizeControl; virtual;
   procedure CreateProperties; virtual;
   procedure ControlCreate; virtual;
   procedure ControlDestroy; virtual;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); virtual;
   procedure BeginTranslate;
   procedure EndTranslate;
   procedure BeginSelectionTransformation; virtual;
   procedure EndSelectionTransformation; virtual;
   procedure MirrorInResize(HMirror, VMirror: boolean); virtual;
   procedure ConnectorMinGapChanged; virtual;
   procedure CreateInDesign(var Info: TFlexCreateInDesignInfo); virtual;
   function  CreateCurveControl: TFlexControl; virtual;
   procedure CreateCurveGuide(const NewPoint: TPoint;
     var Guide: TFlexEditPointGuide); virtual;
   function  MovePathPoints(PointIndex: integer; var Delta: TPoint;
    Selected: TSelectedArray; Smooth: boolean = false;
    Symmetric: boolean = false): boolean; virtual;
   function  MovePathSegment(FirstIndex, NextIndex: integer;
     var Delta: TPoint; const SegmentCurvePos: double): boolean; virtual;
   procedure DoNotify(Notify: TFlexNotify); virtual;
   procedure DoNeedHint(var HintInfo: THintInfo;
     var IsShowHint: boolean); virtual;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); virtual;
   procedure PaintAll(Canvas: TCanvas; PaintX, PaintY: integer); virtual;
   procedure StartResizing(const SelRect: TRect); virtual;
   procedure FinishResizing; virtual;
   procedure PropBeforeChanged(Sender: TObject; Prop: TCustomProp); virtual;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); virtual;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); virtual;
   procedure PropReadOnly(Sender: TObject; Prop: TCustomProp;
     var IsReadOnly: boolean); virtual;
   procedure PropHistoryAction(Sender: TObject; Prop: TCustomProp;
     var ActionClass: THistoryActionClass); virtual;
   function  GetAnchorPoint: TPoint; virtual;
   procedure GetLinkProps(var LinkFirst, LinkLast: TLinkPointProp); virtual;
   function  GetDefaultLinkPoint(Index: integer): TPoint; virtual;
   function  GetDefaultLinkPointCount: integer; virtual;
   function  GetPoint(Index: integer): TPoint; virtual;
   procedure SetPoint(Index: integer; const Value: TPoint); virtual;
   function  GetPointType(Index: integer): TPointType; virtual;
   procedure SetPointType(Index: integer; const Value: TPointType); virtual;
   function  GetPointCount: integer; virtual;
   function  GetIsPointsClosed: boolean; virtual;
   procedure SetIsPointsClosed(Value: boolean); virtual;
   function  GetPointsInfo: PPathInfo; virtual;
   procedure SetDocRect(Value: TRect); virtual;
   function  ValidateName(CheckEmpty, CheckUnique: boolean): boolean; virtual;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; virtual;
   function  GetNotifyLink: TNotifyLink; virtual;
   procedure LinkPointsTranslate(const TranslateInfo: TTranslateInfo); virtual;
   procedure LinkPointsResize(const OldDocRect, NewDocRect: TRect); virtual;
   function  GetIsSelectable: boolean; virtual;
   function  GetIsUngroupable: boolean; virtual;
   property  NonVisual: boolean read FNonVisual write FNonVisual;
   property  NoPaintRectCheck: boolean read FNoPaintRectCheck
     write FNoPaintRectCheck;
   property  TranslateCount: integer read FTranslateCount;
  public
   constructor Create(AOwner: TFlexPanel; AParent: TFlexControl;
     ALayer: TFlexLayer); virtual;
   destructor Destroy; override;
   {$IFDEF FG_CBUILDER}
   procedure AfterConstruction; override;
   // We strongly recommend to use these routine to create flex-control
   // in C++Builder environment instead of native object creation via
   // Control = new TFlexControl(...). It prevents from some incompatibility in
   // object initialization section in Delphi and C++Builder. You also can use
   // TFlexPanel::CreateControl() method to create flex-controls in definite flex
   // panel.
   class function CppCreate(AOwner: TFlexPanel; AParent: TFlexControl;
     ALayer: TFlexLayer): TFlexControl;
   {$ENDIF}
   class function CursorInCreate: TCursor; virtual;
   class function GetToolInfo(ToolIcon: TBitmap; var Hint: string): boolean;
     virtual;
   class function IsConnectorControl: boolean; virtual;
   function  BeginUpdate: boolean; virtual;
   function  EndUpdate: boolean; virtual;
   function  BeginGroupUngroup: boolean; virtual;
   function  EndGroupUngroup: boolean; virtual;
   procedure Invalidate; virtual;
   function  Add(AControl: TFlexControl): integer; virtual;
   function  IndexOf(AControl: TFlexControl): integer;
   procedure Delete(Index: integer); virtual;
   procedure Remove(AControl: TFlexControl);
   procedure Extract(AControl: TFlexControl); virtual;
   procedure Clear; virtual;
   procedure ChangeOrder(CurIndex, NewIndex: integer); virtual;
   procedure ToFront;
   procedure ToBack;
   procedure ForwardOne;
   procedure BackOne;
   function  FindControlAtPoint(x, y: integer): TFlexControl; virtual;
   function  FindByID(ControlID: LongWord): TFlexControl;
   function  FindByName(const AName: string): TFlexControl;
   procedure BeginPointsDesigning; virtual;
   procedure EndPointsDesigning; virtual;
   function  CreatePointsPath(DC: HDC): boolean;
   function  GetNodeIndex(Index: integer): integer; virtual;
   function  GetPointIndex(NodeIndex: integer): integer; virtual;
   function  InsertPoint(Index: integer; const Point: TPoint): integer; virtual;
   function  InsertCurvePoints(Index: integer; const Point, CtrlPointA,
     CtrlPointB: TPoint): integer; virtual;
   procedure DeletePoint(Index: integer); virtual;
   function  AddCurvePoints(const Point, CtrlPointA, CtrlPointB: TPoint): integer;
   function  AddPoint(const Point: TPoint): integer;
   function  InsertCurveNode(NodeIndex: integer; const Point,
     CtrlPointA, CtrlPointB: TPoint): integer;
   procedure DeleteNode(NodeIndex: integer);
   function  InsertNearestPoint(const Point: TPoint): integer; virtual;
   procedure EndFigure(Close: boolean = true);
   procedure SetPoints(const APoints: TPointArray);
   procedure SetPointsEx(const APoints: TPointArray;
     const ATypes: TPointTypeArray); virtual;
   procedure GetPointsEx(out APoints: TPointArray;
     out ATypes: TPointTypeArray); virtual;
   function  EditPoints(Func: TPathEditFunc; const Selected: TSelectedArray;
     Params: PPathEditParams = Nil): boolean; virtual;
   function  EditPointsCaps(const Selected: TSelectedArray): TPathEditFuncs;
     virtual;
   function  FlattenPoints(const Curvature: single): boolean; virtual;
   function  FindNearestPoint(const Point: TPoint;
     var Nearest: TNearestPoint): boolean; virtual;
   function  FindNearestPathSegment(const Point: TPoint; var FirstIndex,
     NextIndex: integer; Nearest: PNearestPoint = Nil;
     ForInsert: boolean = true; ForSelect: boolean = false): boolean; virtual;
   function  GetTransformPoints(OfsX, OfsY, Scale: integer): TPointArray;
   function  IsPointInside(PaintX, PaintY: integer): boolean; virtual;
   function  AddLinkPoint(Point: PPoint): integer; virtual;
   function  IndexOfLinkPoint(Point: PPoint): integer; virtual;
   procedure DeleteLinkPoint(Index: integer); virtual;
   procedure RemoveLinkPoint(Point: PPoint);
   function  ClientToOwner(const Point: TPoint): TPoint;
   function  OwnerToClient(const Point: TPoint): TPoint;
   procedure Translate(const TranslateInfo: TTranslateInfo); virtual;
   procedure SaveToFiler(Filer: TFlexFiler; const Indent: string); virtual;
   procedure LoadFromFiler(Filer: TFlexFiler); virtual;
   property  Owner: TFlexPanel read FOwner write SetOwner;
   property  Parent: TFlexControl read FParent write SetParent;
   property  ParentScheme: TFlexCustomScheme read GetParentScheme;
   property  State: TFlexState read FState;
   property  Layer: TFlexLayer read FLayer write SetLayer;
   property  Visual: boolean read GetVisual;
   property  UpdateCounter: integer read FUpdateCounter;
   property  PointsDesigning: integer read FPointsDesigning;
   property  DocRect: TRect read GetDocRect write SetDocRect;
   property  PaintRect: TRect read GetPaintRect;
   property  IsSelected: boolean read GetIsSelected write SetIsSelected;
   property  IsSelectable: boolean read GetIsSelectable;
   property  IsUngroupable: boolean read GetIsUngroupable;
   property  Count: integer read GetCount;
   property  Controls[Index: integer]: TFlexControl read GetControl; default;
   property  ByName[const Name: string]: TFlexControl read GetByName;
   property  Props: TPropList read FProps;
   property  AnchorEnabled: boolean read FAnchorEnabled;
   property  AnchorPoint: TPoint read GetAnchorPoint;
   property  PointCount: integer read GetPointCount;
   property  Points[Index: integer]: TPoint read GetPoint write SetPoint;
   property  PointTypes[Index: integer]: TPointType read GetPointType write
     SetPointType;
   property  PointsInfo: PPathInfo read GetPointsInfo;
   property  NodeCount: integer read GetNodeCount;
   property  Nodes[NodeIndex: integer]: TPoint read GetNode write SetNode;
   property  NodeTypes[NodeIndex: integer]: TPointType read GetNodeType
     write SetNodeType;
   property  IsPointsClosed: boolean read GetIsPointsClosed write
     SetIsPointsClosed;
   property  LinkPointCount: integer read GetLinkPointCount;
   property  LinkPoints[Index: integer]: TPoint read GetLinkPoint
     write SetLinkPoint;
   property  DefaultLinkPointCount: integer read GetDefaultLinkPointCount;
   property  DefaultLinkPoints[Index: integer]: TPoint read GetDefaultLinkPoint;
   property  NotifyLink: TNotifyLink read GetNotifyLink write FNotifyLink;
   property  NameProp: TStrProp read FNameProp;
   property  LeftProp: TIntProp read FLeftProp;
   property  TopProp: TIntProp read FTopProp;
   property  WidthProp: TIntProp read FWidthProp;
   property  HeightProp: TIntProp read FHeightProp;
   property  IdProp: TLongWordProp read FIdProp;
   property  TagProp: TIntProp read FTagProp;
   property  VisibleProp: TBoolProp read FVisibleProp;
   property  LayerProp: TLayerProp read FLayerProp;
   property  HintProp: TStrListProp read FHintProp;
   property  ShowHintProp: TBoolProp read FShowHintProp;
   property  ReferenceProp: TSchemeRefProp read FReferenceProp;
   property  TransparencyProp: TIntProp read FTransparencyProp;
   property  SelectableProp: TBoolProp read FSelectableProp;
   //property  UserDataProp: TUserDataProp read FUserDataProp;
   property  UserData: TUserDataProp read FUserDataProp;
   property  UserObject: TObject read FUserObject write FUserObject;
  public
   procedure BeforeDestruction; override;
   property  Name: string read GetName write SetName;
   property  ID: LongWord read GetID write SetID;
   property  Tag: integer read GetTag write SetTag;
   property  Left: integer read GetLeft write SetLeft;
   property  Top: integer read GetTop write SetTop;
   property  Width: integer read GetWidth write SetWidth;
   property  Height: integer read GetHeight write SetHeight;
   property  Visible: boolean read GetVisible write SetVisible;
   property  Hint: string read GetHint write SetHint;
   property  ShowHint: boolean read GetShowHint write SetShowHint;
   property  Reference: TFlexCustomScheme read GetRef write SetRef;
   property  Transparency: integer read GetTransparency write SetTransparency;
   property  Selectable: boolean read GetSelectable write SetSelectable;
   property  ChannelsProp: TChannelProp read FChannelsProp;
   property  ControlObjectProp: TControlObjectProp read FControlObjectProp;
   property  ActionProp: TScriptProp read FClickActionProp;
  end;

  // Use TFlexServiceControl to identify "structure" controls that destroys
  // manualy in TFlexPanel.
  // All that controls is derived from TFlexServiceControl:
  // TFlexCustomScheme, TFlexSchemes, TFlexLayer, TFlexLayers, TFlexGrid
  TFlexServiceControl = class(TFlexControl);

  TFlexGroupClass = class of TFlexGroup;

  TFlexGroup = class(TFlexControl)
  private
   FGroupResize: boolean;
   FGroupMove: boolean;
   FBoundsChanging: boolean;
   FOldDocRect: TRect;
   FNeedRefreshBounds: boolean;
  protected
   procedure ControlCreate; override;
   procedure ControlTranslate(const TranslateInfo: TTranslateInfo); override;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
   procedure PropBeforeChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure RefreshBounds;
   function  GetIsUngroupable: boolean; override;
  public
   function  EndGroupUngroup: boolean; override;
   function  IsPointInside(PaintX, PaintY: integer): boolean; override;
   procedure Translate(const TranslateInfo: TTranslateInfo); override;
  end;

  TFlexCloneClass = class of TFlexClone;

  TFlexClone = class(TFlexControl)
  private
   FSourceProp: TLinkProp;
   FAutoSizeProp: TBoolProp;
   FSizeLock: boolean;
   procedure SetSizeLock(Value: boolean);
  protected
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlDestroy; override;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   procedure SourceLinkNotify(Sender: TObject; Source: TNotifyLink;
     const Info: TNotifyLinkInfo);
   procedure SetSizeFromLinkedControl;
   function  OpenMetafile(const Bounds: TRect): TMetafile;
   procedure CloseMetafile(Metafile: TMetafile);
   property  SizeLock: boolean read FSizeLock write SetSizeLock;
  public
   property  SourceProp: TLinkProp read FSourceProp;
   property  AutoSizeProp: TBoolProp read FAutoSizeProp;
  end;

  TFlexGrid = class(TFlexServiceControl)
  private
   FSnap: boolean;
   FSnapTo: TFlexSnaps;
   FShowPixGrid: boolean;
   FStyle: TFlexGridStyle;
   FColor: TColor;
   FPixColor: TColor;
   FHSize: integer;
   FVSize: integer;
   FHOffset: integer;
   FVOffset: integer;
   procedure GetOwnerWidth(Sender: TObject; out Value: integer);
   procedure GetOwnerHeight(Sender: TObject; out Value: integer);
   procedure SetColor(const Value: TColor);
   procedure SetShowPixGrid(const Value: boolean);
   procedure SetStyle(const Value: TFlexGridStyle);
   procedure SetPixColor(const Value: TColor);
   procedure SetHSize(const Value: integer);
   procedure SetVSize(const Value: integer);
   procedure SetHOffset(const Value: integer);
   procedure SetVOffset(const Value: integer);
  protected
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
  public
   procedure DoSnap(const SnapRect: TRect; var Delta: TPoint;
     ResizeCursor: TResizeCursor = rcrNone);
   procedure DoCustomSnap(const SnapRect: TRect; var Delta: TPoint;
     HStep, VStep, HOffset, VOffset: integer;
     ResizeCursor: TResizeCursor = rcrNone;
     SnapTo: TFlexSnaps = [snAll]); virtual;
   property  Snap: boolean read FSnap write FSnap;
   property  SnapTo: TFlexSnaps read FSnapTo write FSnapTo;
   property  ShowPixGrid: boolean read FShowPixGrid write SetShowPixGrid;
   property  Style: TFlexGridStyle read FStyle write SetStyle;
   property  Color: TColor read FColor write SetColor;
   property  PixColor: TColor read FPixColor write SetPixColor;
   property  HSize: integer read FHSize write SetHSize;
   property  VSize: integer read FVSize write SetVSize;
   property  HOffset: integer read FHOffset write SetHOffset;
   property  VOffset: integer read FVOffset write SetVOffset;
  end;

  TFlexLayer = class(TFlexServiceControl)
  private
   //FSelectableProp: TBoolProp;
   FMoveableProp: TBoolProp;
   FResizableProp: TBoolProp;
   FReadOnlyProp: TBoolProp;
   //function  GetSelectable: boolean;
   //procedure SetSelectable(Value: boolean);
   function  GetReadOnly: boolean;
   procedure SetReadOnly(Value: boolean);
   function  GetMoveable: boolean;
   procedure SetMoveable(const Value: boolean);
   function  GetResizable: boolean;
   procedure SetResizable(const Value: boolean);
  protected
   FCheckName: string;
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlDestroy; override;
   procedure PropBeforeChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   property  ReadOnlyProp: TBoolProp read FReadOnlyProp;
   //property  SelectableProp: TBoolProp read FSelectableProp;
   property  MoveableProp: TBoolProp read FMoveableProp;
   property  ResizableProp: TBoolProp read FResizableProp;
  public
   procedure Invalidate; override;
   property  Moveable: boolean read GetMoveable write SetMoveable;
   property  Resizable: boolean read GetResizable write SetResizable;
   //property  Selectable: boolean read GetSelectable write SetSelectable;
   property  ReadOnly: boolean read GetReadOnly write SetReadOnly;
  end;

  TFlexLayers = class(TFlexServiceControl)
  private
   function  GetLayer(Index: integer): TFlexLayer;
   function  GetByName(const Name: string): TFlexLayer;
  protected
   procedure ControlCreate; override;
  public
   function  New: TFlexLayer;
   function  Add(AControl: TFlexControl): integer; override;
   procedure Delete(Index: integer); override;
   procedure ChangeOrder(CurIndex, NewIndex: integer); override;
   property  Controls[Index: integer]: TFlexLayer read GetLayer; default;
   property  ByName[const Name: string]: TFlexLayer read GetByName;
  end;

  TPaintOrder = record
   LayerRefs:   array of record
                 First: integer;
                 Last: integer;
                end;
   ControlRefs: array of record
                 Next: integer;
                 Prev: integer;
                 Control: TFlexControl;
                end;
  end;

  TFlexCustomScheme = class(TFlexServiceControl)
  private
   function  GetDefault: boolean;
   procedure SetDefault(const Value: boolean);
   procedure GetOwnerWidth(Sender: TObject; out Value: integer);
   procedure GetOwnerHeight(Sender: TObject; out Value: integer);
  protected
   FDefaultProp: TBoolProp;
   procedure CreateProperties; override;
   procedure ControlCreate; override;
   procedure ControlDestroy; override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   procedure PaintAll(Canvas: TCanvas; PaintX, PaintY: integer); override;
   property  Default: boolean read GetDefault write SetDefault;
  public
   function  FindControlAtPoint(x, y: integer): TFlexControl; override;
   function  CreatePaintOrder(var PaintOrder: TPaintOrder): boolean;
  end;

  TFlexScheme = class(TFlexCustomScheme)
  private
   FBrushProp: TBrushProp;
   FPictureProp: TPictureProp;
   FBackgroundProp: TBackgroundOptionsProp;
  protected
   FCheckName: string;
   procedure ControlCreate; override;
   procedure CreateProperties; override;
   procedure Paint(Canvas: TCanvas; var PaintRect: TRect); override;
   procedure PropBeforeChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
   function  GetRefreshRect(RefreshX, RefreshY: integer): TRect; override;
  public
   property  BrushProp: TBrushProp read FBrushProp;
   property  PictureProp: TPictureProp read FPictureProp;
   property  DefaultProp: TBoolProp read FDefaultProp;
   property  BackgroundProp: TBackgroundOptionsProp read FBackgroundProp;
   property  Default;
  end;

  TFlexSchemes = class(TFlexServiceControl)
  private
   FVersionProp: TIntProp;
   FConnectorsMinGapProp: TIntProp;
   FConnectorsKeepLinkProp: TBoolProp;
   FScriptProp: TStrListProp;
   function  GetScheme(Index: integer): TFlexCustomScheme;
   function  GetByName(const Name: string): TFlexCustomScheme;
  protected
   procedure ControlCreate; override;
   procedure CreateProperties; override;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); override;
   procedure PropStored(Sender: TObject; Prop: TCustomProp;
     var IsStored: boolean; const PropName: string = ''); override;
  public
   function  Add(AControl: TFlexControl): integer; override;
   procedure Delete(Index: integer); override;
   procedure ChangeOrder(CurIndex, NewIndex: integer); override;
   property  Controls[Index: integer]: TFlexCustomScheme read GetScheme; default;
   property  ByName[const Name: string]: TFlexCustomScheme read GetByName;
   property  VersionProp: TIntProp read FVersionProp;
   property  ConnectorsMinGapProp: TIntProp read FConnectorsMinGapProp;
   property  ConnectorsKeepLinkProp: TBoolProp read FConnectorsKeepLinkProp;
  end;

  TFlexDragObject = class(TDragObject)
  private
   FDragGroup: TFlexGroup;
   FMouseDragDelta: TPoint;
   FDefaultFlex: TFlexPanel;
  protected
   procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
     override;
  public
   destructor Destroy; override;
   procedure UpdateLink(Flex: TFlexPanel; MouseX, MouseY: integer); virtual;
   procedure Unlink; virtual;
   property  DragGroup: TFlexGroup read FDragGroup write FDragGroup;
   property  MouseDragDelta: TPoint read FMouseDragDelta write FMouseDragDelta;
   property  DefaultFlex: TFlexPanel read FDefaultFlex write FDefaultFlex;
  end;

  PFlexPaintStruct = ^TFlexPaintStruct;
  TFlexPaintStruct = record
   IsFirstPaint: boolean;
   Canvas: TCanvas;
   Origin: TPoint;
   Scale: integer;
   PaintRect: TRect;
   PaintWidth: integer;
   PaintHeight: integer;
   PaintForExport: boolean;
   UseOriginalBezier: boolean;
   UseImageClipTransparent: boolean;
  end;

  TFlexPaintList = class
  private
   FOwner: TFlexPanel;
   FList: TList;
   FFirstInUse: boolean;
  protected
   procedure Clear;
   procedure StoreParams(PaintStruct: PFlexPaintStruct; ACanvas: TCanvas);
   procedure RestoreParams(PaintStruct: PFlexPaintStruct);
  public
   constructor Create(AOwner: TFlexPanel);
   destructor Destroy; override;
   function  BeginPaint(ACanvas: TCanvas): PFlexPaintStruct;
   procedure EndPaint(PaintStruct: PFlexPaintStruct);
   property  Owner: TFlexPanel read FOwner;
  end;

  PFlexPaintCache = ^TFlexPaintCache;
  TFlexPaintCache = record
   DC: HDC;
   Bitmap: HBitmap;
   BitsPixel: integer;
   Width: integer;
   Height: integer;
   rcPaint: TRect;
  end;

  TFlexPanelHistory = class(THistory)
  private
   FPanel: TFlexPanel;
  protected
   procedure DoChange; override;
   function  DoGetActionSource(Action: THistoryAction;
     var Enabled: boolean): TObject; override;
   procedure DoSetActionSource(Action: THistoryAction;
     const Source: TObject); override;
   function  DoGetActionCaption(Action: THistoryAction;
     const Source: TObject): string; override;
   function  DoIsRecordable(var ActionClass: THistoryActionClass;
     var Source: TObject; Parent: THistoryGroup): boolean; override;
   function  DoIsSame(ExistedAction: THistoryAction;
     NewActionClass: THistoryActionClass;
     NewSource: TObject): boolean; override;
  public
   constructor Create(AOwner: TFlexPanel);
   function  Undo: boolean; override;
   function  Redo: boolean; override;
   function  SourceControl(Source: TObject): TFlexControl;
   procedure SourceInfo(Source: TObject;
     out Id: LongWord; out PropName: string);
   function  BeginPanelGroup(GroupClass: THistoryGroupClass): THistoryGroup;
   function  EndPanelGroup(GroupClass: THistoryGroupClass): boolean;
   function  RecordSelectedAsNew: boolean;
   property  Owner: TFlexPanel read FPanel;
  end;

  TFlexPaintEvent = procedure(ACanvas: TCanvas; AControl: TFlexControl;
     ChildrenOnly, SelectedOnly: boolean) of object;

  TFlexUpdateCursorEvent = procedure(Sender: TObject; var ACursor: TCursor)
    of object;
  TFlexNeedHintEvent = procedure(Sender: TObject; var IsNeedHint: boolean)
    of object;
  TFlexShowHintEvent = procedure(Sender: TObject; HintControl: TFlexControl;
    var HintInfo: THintInfo; var IsShowHint: boolean) of object;

  TFlexPanel = class(TScrollingWinControl)
  private
   FFileName: string;
   FBorderStyle: TBorderStyle;
   FCanvas: TCanvas;
   FDocWidth: integer;
   FDocHeight: integer;
   FDocFrameColor: TColor;
   FDocShadowColor: TColor;
   FDocSpaceBrush: TBrush;
   FDocSpaceFill: boolean;
   FDocClipping: boolean;
   FSchemeBkStretch: boolean;
   FShowDocFrame: boolean;
   FAutoZoom: boolean;
   FAutoNames: boolean;
   FAutoNameNumbs: boolean;
   FSelectAsFilled: boolean;
   FSelectPartialOverlapped: boolean;
   FGridControl: TFlexGrid;
   FToolMode: TFlexToolMode;
   FDefaultToolMode: TFlexToolMode;
   FAutoDragEnabled: boolean;
   FLayers: TFlexLayers;
   FActiveLayer: TFlexLayer;
   FSchemes: TFlexSchemes;
   FActiveScheme: TFlexCustomScheme;
   FSelList: TList;
   FInDesign: boolean;
   FViewing: boolean;
   FModified: boolean;
   FIdPool: TIdPool;
   FIdChangeControl: TFlexControl;
   FConnectorsMinGap: integer;
   FMouseAnchor: TPoint;
   FMoveStart: TPoint;
   FResizeRect: TRect;
   FMarqueeRect: TRect;
   FCreatingControlClass: TFlexControlClass;
   FShowEditPointGuide: boolean;
   FEditPointGuide: TFlexEditPointGuide;
   FEditPointControl: TFlexControl;
   FEditPointSelected: TSelectedArray;
   FEditPointSelCount: integer;
   FUseOriginalBezier: boolean;
   FUseImageClipTransparent: boolean;
   FWMPaintProcessing: boolean;
   FNotifyLink: TNotifyLink;
   FAlphaBuffer: TAlphaBuffer;
   FFrostPan: boolean;
   FFrostPanFullDoc: boolean;
   FOnControlCreate: TFlexControlCreateEvent;
   FOnUpdateCursor: TFlexUpdateCursorEvent;
   FOnPropBeforeChanged: TPropChangedEvent;
   FOnPropChanged: TPropChangedEvent;
   FOnPaintScheme: TFlexPaintEvent;
   FOnPaintOver: TFlexPaintEvent;
   FOnNotify: TFlexNotifyEvent;
   FOnBeginSelectionUpdate: TNotifyEvent;
   FOnEndSelectionUpdate: TNotifyEvent;
   FOnProgress: TFlexProgressEvent;
   FOnToolMode: TNotifyEvent;
   FOnNeedHint: TFlexNeedHintEvent;
   FOnShowHint: TFlexShowHintEvent;
   FOnMouseControlChange: TNotifyEvent;
   FModalDialogMode: bool;
   procedure SetBorderStyle(Value: TBorderStyle);
   procedure SetInDesign(const Value: boolean);
   procedure SetActiveLayer(const Value: TFlexLayer);
   procedure SetActiveScheme(const Value: TFlexCustomScheme);
   function  GetSelected(Index: integer): TFlexControl;
   function  GetSelectedCount: integer;
   procedure SetToolMode(const Value: TFlexToolMode);
   procedure SetCreatingControlClass(const Value: TFlexControlClass);
   function  GetCppCreatingControlClass: cardinal;
   procedure SetCppCreatingControlClass(const Value: cardinal);
   procedure SetDocHeight(const Value: integer);
   procedure SetDocWidth(const Value: integer);
   procedure SetDocFrameColor(const Value: TColor);
   procedure SetDocShadowColor(const Value: TColor);
   procedure SetDocSpaceFill(const Value: boolean);
   function  GetDocFrameVisible: boolean;
   procedure SetDocClipping(const Value: boolean);
   procedure SetShowDocFrame(const Value: boolean);
   procedure SetScale(Value: integer);
   function  GetShowGrid: boolean;
   procedure SetShowGrid(const Value: boolean);
   function  GetShowPixGrid: boolean;
   procedure SetShowPixGrid(const Value: boolean);
   function  GetGridHorizSize: integer;
   function  GetGridVertSize: integer;
   procedure SetGridHorizSize(const Value: integer);
   procedure SetGridVertSize(const Value: integer);
   function  GetGridStyle: TFlexGridStyle;
   procedure SetGridStyle(const Value: TFlexGridStyle);
   function  GetSnapToGrid: boolean;
   procedure SetSnapToGrid(const Value: boolean);
   function  GetSnapStyle: TFlexSnaps;
   procedure SetSnapStyle(const Value: TFlexSnaps);
   function  GetGridColor: TColor;
   procedure SetGridColor(const Value: TColor);
   procedure SetMarqueeRect(const Value: TRect);
   procedure SetEditPointControl(Value: TFlexControl);
   function  GetDefaultScheme: TFlexCustomScheme;
   function  GetGridPixColor: TColor;
   procedure SetGridPixColor(const Value: TColor);
   procedure SetHorzExtraSpace(const Value: integer);
   procedure SetVertExtraSpace(const Value: integer);
   procedure DocSpaceBrushChanged(Sender: TObject);
   procedure SetSchemeBkStretch(const Value: boolean);
   procedure FilerProgress(Sender: TObject; Progress: integer;
     Process: TFlexFilerProcess);
   procedure SetDocSpaceBrush(const Value: TBrush);
   function  GetControlCount(Control: TFlexControl): integer;
   function  GetEditPointSelected(Index: integer): boolean;
   procedure SetEditPointSelected(Index: integer; Value: boolean);
   function  GetEditPointsCaps: TPathEditFuncs;
   function  GetIsLoading: boolean;
   procedure SetHideSelection(const Value: boolean);
   procedure UpdateMouseData;
   function  IsEditCloseFigure: boolean;
   function  CloseFigureByPoint(Index: integer): boolean;
   procedure SetShowEditPointGuide(const Value: boolean);
   function  GetNotifyLink: TNotifyLink;
   procedure SetAutoZoom(const Value: boolean);
   procedure SetGridControl(const Value: TFlexGrid);
   procedure SetFrostPan(const Value: boolean);
   procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
   procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
   procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
   procedure WMSize(var Message: TWMSize); message WM_SIZE;
   procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
   procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
   procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
   procedure WMNCCalcSize(var Message: TMessage); message WM_NCCALCSIZE;
    function GetScriptProp: TStrListProp;
    procedure SetModified(const Value: boolean);
  protected
   FHistory: TFlexPanelHistory;
   FCreateInDesignAction: THistoryGroup;
   FWithoutParent: TList;
   FFlexCursor: TCursor;
   FOrigin: TPoint;
   FClientDocRect: TRect;
   FFilerProcess: TFlexFilerProcess;
   FPaintCache: TFlexPaintCache;   
   FPaintWidth: integer;
   FPaintHeight: integer;
   FPaintRect: TRect;
   FPaintForExport: boolean;
   FPaintSchemeBackground: boolean;
   FInPaintOrigin: TPoint;
   FInPaintScale: integer;
   FScale: integer; { in percents MinScale..MaxScale}
   FZoomingIn: boolean;
   FZoomMouseStart: TPoint;
   FIsPointAlter: boolean;
   FIsOverPoint: boolean;
   FIsOverCurveSegment: boolean;
   FIsOverSegment: boolean;
   FOverCurveNearest: TNearestPoint;
   FMouseControl: TFlexControl;
   FMouseSubControl: TFlexControl;
   FHideSelection: boolean;
   FHintControl: TFlexControl;
   FEditPointIndex: integer;
   FIsLoading: integer;
   FLoadFunc: TFlexLoadFunc;
   FLastMousePos: TPoint;
   FLastKeyShift: TShiftState;
   FSelRect: TRect;
   FSelUpdateCounter: integer;
   FSelNeedUpdate: boolean;
   FResizeCursor: TResizeCursor;
   FPointsRgn: HRGN;
   FPointsRgnExact: boolean;
   FDragObject: TFlexDragObject;
   FRefPropsList: TPropRefList;
   FDefaultLinkPoint: TLinkPointInfo;
   FLastMovedPos: TPoint;
   FPaintList: TFlexPaintList;
   FHorzExtraSpace: integer;
   FVertExtraSpace: integer;
   FSaveAsBinary: boolean;
   FFrostPanImage: TBitmap;
   function  AddWithoutParent(Control: TFlexControl): integer;
   function  RemoveWithoutParent(Control: TFlexControl): boolean;
   function  WithoutParentFindById(ControlID: LongWord): TFlexControl;
   procedure CreateParams(var Params: TCreateParams); override;
   procedure DoNotify(Control: TFlexControl; Notify: TFlexNotify); virtual;
   function  IsInternalControl(Control: TFlexControl): boolean;
   procedure UpdateOrigin;
   procedure UpdateScrollBars;
   procedure Paint(ACanvas: TCanvas; AControl: TFlexControl;
     ChildrenOnly, SelectedOnly: boolean); virtual;
   procedure PaintPoints(ACanvas: TCanvas); virtual;
   procedure PaintLinkPoint(ACanvas: TCanvas); virtual;
   function  GetAnchorRect(AControl: TFlexControl; Inflate: integer = 0): TRect;
   function  GetEditPointRect(Index: integer; Inflate: integer = 0): TRect;
   function  DoPointEdit(Control: TFlexControl;
     Index, DocX, DocY: integer): boolean;
   procedure DoEnter; override;
   procedure DoExit; override;
   procedure DoMouseControlChange; virtual;
   procedure UpdatePoints;
   function  UpdatePointGuide(Region: HRGN = 0): boolean;
   function  UpdatePointTangents(Region: HRGN = 0): boolean;
   procedure UpdateEditPointIndex;
   procedure FindControlPoints(PointIndex: integer; out PrevNode,
     NextNode: integer);
   function  FindFirstOrLastNodeIfEqual(Index: integer;
     AControl: TFlexControl): integer;
   procedure InvalidateSelection;
   procedure PaintSelection(ACanvas: TCanvas); virtual;
   procedure PaintMarquee(ACanvas: TCanvas); virtual;
   procedure DoNeedHint(var IsNeedHint: boolean); virtual;
   procedure DoShowHint(var HintInfo: THintInfo; var IsShowHint: boolean); virtual;
   procedure DoStartDrag(var DragObject: TDragObject); override;
   procedure DragOver(Source: TObject; X, Y: Integer;
     State: TDragState; var Accept: Boolean); override;
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
     X, Y: Integer); override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
     override;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
   procedure UpdateToolMode(Shift: TShiftState; X, Y: Integer);
   procedure KeyDown(var Key: Word; Shift: TShiftState); override;
   procedure KeyUp(var Key: Word; Shift: TShiftState); override;
   function  ValidateID(Control: TFlexControl; LastID: LongWord): boolean;
   procedure PropBeforeChanged(Sender: TObject; Prop: TCustomProp); virtual;
   procedure PropChanged(Sender: TObject; Prop: TCustomProp); virtual;
   function  CreateSelMarkersRgn: HRGN; virtual;
   function  GetSelResizeCursor(PaintX,
     PaintY: integer): TResizeCursor; virtual;
   procedure UpdateSelection(Control: TFlexControl);
   procedure SetConnectorsMinGap(const Value: integer); virtual;
   procedure SetMouseControl(Control: TFlexControl;
     SubControl: TFlexControl = Nil); virtual;
   property  Canvas: TCanvas read FCanvas;
   property  MarqueeRect: TRect read FMarqueeRect write SetMarqueeRect;
   property  AlphaBuffer: TAlphaBuffer read FAlphaBuffer;
  public
   DataArray: array[0..1000] of Variant;
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Assign(Source: TPersistent); override;
   procedure Click; override;
   procedure AutoSize;
   procedure BeginLoading; virtual;
   procedure EndLoading; virtual;
   function  CreateControl(ControlClass: TFlexControlClass;
     ALeft, ATop, AWidth, AHeight: integer;
     Scheme: TFlexCustomScheme = Nil; Layer: TFlexLayer = Nil;
     UseGrid: boolean = true): TFlexControl; virtual;
   procedure PaintTo(ACanvas: TCanvas; const APaintRect: TRect;
    const AOrigin: TPoint; AScale: integer; AControl: TFlexControl;
    Clipping, ChildrenOnly, SelectedOnly, ForExport: boolean;
    AUseOriginalBezier: boolean = false;
    AUseImageClipTransparent: boolean = false);
   procedure PaintEmptyPicture(ACanvas: TCanvas;
     APicture: TFlexControl); virtual;
   procedure InvalidateDocRect(const DocRect: TRect);
   // Return TRUE if AControl invalidated and child controls also needs to invalidate
   function  InvalidateControl(AControl: TFlexControl): boolean; virtual;
   procedure EmptyDocument;
   procedure NewDocument;
   procedure UpdateCursor; virtual;
   function  FindControl(const AName: string;
     From: TFlexControl = Nil): TFlexControl;
   function  FindControlByID(ControlID: LongWord;
     From: TFlexControl = Nil): TFlexControl;
   function  FindControlAtPoint(x, y: integer): TFlexControl; virtual;
   function  GetDefaultNewName(Control: TFlexControl;
     RootControl: TFlexControl = Nil; GenerateNumb: boolean = False): string;
   procedure GenerateID(Control: TFlexControl);
   procedure DoSnapToGrid(HStep, VStep: integer; const SnapRect: TRect;
     var Delta: TPoint; SnapTo: TFlexSnaps = [snAll];
     ResizeCursor: TResizeCursor = rcrNone;
     HOffset: integer = 0; VOffset: integer = 0); virtual;
   function  StartDrag(AControl: TFlexControl;
     ChildrenOnly, SelectedOnly: boolean; const MousePos, AOrigin: TPoint;
     AScale: integer; const DragName: string = ''): TFlexDragObject;
   function  CreateDragObject(AControl: TFlexControl;
     ChildrenOnly, SelectedOnly: boolean; DragName: string = ''): TFlexDragObject;
   procedure DragDrop(Source: TObject; X, Y: Integer); override;
   function  CreateFlexFiler(AStream: TStream;
     Process: TFlexFilerProcess; ABinary: boolean = False): TFlexFiler;
   function  LoadFromStream(Stream: TStream): boolean;
   function  LoadFromFile(const AFilename: string): boolean;
   function  LoadFromFiler(Filer: TFlexFiler;
     LoadFunc: TFlexLoadFunc): boolean; virtual;
   function  LoadFlexControl(Filer: TFlexFiler; AParent: TFlexControl;
     const First: string; WithoutParent: boolean = false): TFlexControl; virtual;
   function  SaveToFile(const Filename: string): boolean;
   function  SaveToStream(Stream: TStream): boolean;
   function  SaveToFiler(Filer: TFlexFiler; SelectedOnly: boolean = false;
     AControl: TFlexControl = Nil; ChildrenOnly: boolean = false): boolean;
   procedure Print(APrinter: TPrinter; PrintBackground, SelectedOnly: boolean);
   procedure TransformRect(var R: TRect);
   procedure TransformPoint(var px, py: integer);
   procedure TransformPointIndirect(var P: TPoint);
   procedure UnTransformPoint(var px, py: integer);
   procedure BeginSelectionUpdate(IsTransformation: boolean = false;
     HistoryGroup: THistoryGroupClass = Nil);
   procedure EndSelectionUpdate(IsTransformation: boolean = false;
     HistoryGroup: THistoryGroupClass = Nil);
   function  IsSelected(AControl: TFlexControl): boolean;
   function  Select(AControl: TFlexControl): boolean;
   function  SelectNext: TFlexControl;
   function  SelectPrev: TFlexControl;
   function  Unselect(AControl: TFlexControl): boolean;
   procedure UnselectAll;
   procedure CopyToClipboard;
   procedure CutToClipboard;
   procedure PasteFromClipboard(FlexDocOnly: boolean = false); virtual;
   function  PasteAvailable(FlexDocOnly: boolean = false): boolean; virtual;
   procedure DeleteSelected;
   procedure BackOne;
   procedure ForwardOne;
   procedure ToBack;
   procedure ToFront;
   function  CloneSelected(ShiftX: integer = 0; ShiftY: integer = 0;
     CloneClass: TFlexCloneClass = nil): TFlexClone;
   function  Group(GroupClass: TFlexGroupClass = nil): TFlexGroup;
   function  Ungroup: boolean;
   procedure Zoom(AScale: integer; ZoomRect: PRect);
   procedure Rotate(ADegree: integer; AMirror: boolean);
   procedure Translate(var TranslateInfo: TTranslateInfo);
   procedure MoveSelected(ShiftX, ShiftY: integer);
   procedure ResizeSelected(DeltaX, DeltaY: integer;
     Proportional: boolean = false; UseGrid: boolean = true;
     ResizeCursor: TResizeCursor = rcrBottomRight;
     UseDefaultSnap: boolean = true;
     ProportionalToMax: boolean = false); virtual;
   procedure Duplicate(ShiftX, ShiftY: integer);
   procedure AlignSelected(Align: TFlexAlign);
   function  IsEditPointsVisible: boolean;
   function  EditPoints(Func: TPathEditFunc;
     Params: PPathEditParams = Nil): boolean;
   function  BreakApartSelected: boolean;
   function  CombineSelected: boolean;
   function  FlattenSelected(const Curvature: single): boolean;
   function  ConvertSelectedToCurves: boolean;
   procedure SelectPoint(Index: integer);
   procedure UnselectPoint(Index: integer);
   procedure UnselectAllPoints;
   procedure DeleteSelectedPoints;
   property  ToolMode: TFlexToolMode read FToolMode write SetToolMode;
   property  History: TFlexPanelHistory read FHistory;
   property  Origin: TPoint read FOrigin;
   property  ClientDocRect: TRect read FClientDocRect;
   property  Layers: TFlexLayers read FLayers;
   property  ActiveLayer: TFlexLayer read FActiveLayer write SetActiveLayer;
   property  Schemes: TFlexSchemes read FSchemes;
   property  ActiveScheme: TFlexCustomScheme read FActiveScheme
     write SetActiveScheme;
   property  DefaultScheme: TFlexCustomScheme read GetDefaultScheme;
   property  SelectedCount: integer read GetSelectedCount;
   property  Selected[Index: integer]: TFlexControl read GetSelected;
   property  SelectedRange: TRect read FSelRect;
   property  SelectionUpdateCounter: integer read FSelUpdateCounter;
   property  InDesign: boolean read FInDesign write SetInDesign;
   property  Modified: boolean read FModified write SetModified;
   property  IsLoading: boolean read GetIsLoading;
   property  GridControl: TFlexGrid read FGridControl write SetGridControl;
   property  PaintWidth: integer read FPaintWidth;
   property  PaintHeight: integer read FPaintHeight;
   property  PaintRect: TRect read FPaintRect;
   property  PaintForExport: boolean read FPaintForExport;
   property  PaintSchemeBackground: boolean read FPaintSchemeBackground;
   property  DocFrameVisible: boolean read GetDocFrameVisible;
   property  PropRefList: TPropRefList read FRefPropsList;
   property  CreatingControlClass: TFlexControlClass
     read FCreatingControlClass write SetCreatingControlClass;
   property  CppCreatingControlClass: cardinal
     read GetCppCreatingControlClass write SetCppCreatingControlClass;
   property  MouseControl: TFlexControl read FMouseControl;
   property  MouseSubControl: TFlexControl read FMouseSubControl;
   property  EditPointControl: TFlexControl read FEditPointControl
     write SetEditPointControl;
   property  EditPointsCaps: TPathEditFuncs read GetEditPointsCaps;
   property  EditPointSelected[Index: integer]: boolean
     read GetEditPointSelected write SetEditPointSelected;
   property  EditPointSelectedTotal: integer read FEditPointSelCount;
   property  DefaultLinkPoint: TLinkPointInfo read FDefaultLinkPoint;
   property  UseOriginalBezier: boolean read FUseOriginalBezier;
   property  UseImageClipTransparent: boolean read FUseImageClipTransparent;
   property  NotifyLink: TNotifyLink read GetNotifyLink write FNotifyLink;
   property Script: TStrListProp read GetScriptProp;
   property FileName: string read FFileName write FFileName;
   property ModalDialogMode: bool read FModalDialogMode write FModalDialogMode;
  published
   property  AutoDragEnabled: boolean read FAutoDragEnabled write FAutoDragEnabled
     default false;
   property  AutoZoom: boolean read FAutoZoom write SetAutoZoom default false;
   property  Scale: integer read FScale write SetScale default 100;
   //property  Viewing: boolean read FViewing write FViewing;
   property  DocWidth: integer read FDocWidth write SetDocWidth;
   property  DocHeight: integer read FDocHeight write SetDocHeight;
   property  DocFrameColor: TColor read FDocFrameColor write SetDocFrameColor;
   property  DocShadowColor: TColor read FDocShadowColor write SetDocShadowColor;
   property  DocSpaceBrush: TBrush read FDocSpaceBrush write SetDocSpaceBrush;
   property  DocSpaceFill: boolean read FDocSpaceFill write SetDocSpaceFill;
   property  DocClipping: boolean read FDocClipping write SetDocClipping
     default false;
   property  SaveAsBinary: boolean read FSaveAsBinary write FSaveAsBinary
     default false;
   property  HideSelection: boolean read FHideSelection write SetHideSelection
     default false;
   property  HorzExtraSpace: integer read FHorzExtraSpace
     write SetHorzExtraSpace default 0;
   property  VertExtraSpace: integer read FVertExtraSpace
     write SetVertExtraSpace default 0;
   property  SchemeBkStretch: boolean read FSchemeBkStretch
     write SetSchemeBkStretch default false;
   property  AutoNames: boolean read FAutoNames write FAutoNames
     default True;
   property  ConnectorsMinGap: integer read FConnectorsMinGap
     write SetConnectorsMinGap default StdConnectorsMinGap;
   property  AutoNameNumbs: boolean read FAutoNameNumbs write FAutoNameNumbs
     default False;
   property  FrostPan: boolean read FFrostPan write SetFrostPan default False;
   property  FrostPanFullDoc: boolean read FFrostPanFullDoc
     write FFrostPanFullDoc default False;
   property  SelectAsFilled: boolean read FSelectAsFilled write FSelectAsFilled
     default False;
   property  SelectPartialOverlapped: boolean read FSelectPartialOverlapped
     write FSelectPartialOverlapped default false;
   property  ShowDocFrame: boolean read FShowDocFrame write SetShowDocFrame
     default True;
   property  ShowGrid: boolean read GetShowGrid write SetShowGrid
     default false;
   property  ShowPixGrid: boolean read GetShowPixGrid write SetShowPixGrid
     default false;
   property  SnapToGrid: boolean read GetSnapToGrid write SetSnapToGrid
     default false;
   property  SnapStyle: TFlexSnaps read GetSnapStyle write SetSnapStyle
     default [snLeft, snTop];
   property  GridStyle: TFlexGridStyle read GetGridStyle write SetGridStyle;
   property  GridColor: TColor read GetGridColor write SetGridColor;
   property  GridPixColor: TColor read GetGridPixColor write SetGridPixColor;
   property  GridHorizSize: integer read GetGridHorizSize
     write SetGridHorizSize;
   property  GridVertSize: integer read GetGridVertSize write SetGridVertSize;
   property  ShowEditPointGuide: boolean read FShowEditPointGuide write
    SetShowEditPointGuide default true;
   property  OnControlCreate: TFlexControlCreateEvent read FOnControlCreate
     write FOnControlCreate;
   property  OnUpdateCursor: TFlexUpdateCursorEvent read FOnUpdateCursor
     write FOnUpdateCursor;
   property  OnPropBeforeChanged: TPropChangedEvent read FOnPropBeforeChanged
     write FOnPropBeforeChanged;
   property  OnPropChanged: TPropChangedEvent read FOnPropChanged
     write FOnPropChanged;
   property  OnNotify: TFlexNotifyEvent read FOnNotify write FOnNotify;
   property  OnMouseControlChange: TNotifyEvent read FOnMouseControlChange
     write FOnMouseControlChange;
   property  OnPaintScheme: TFlexPaintEvent read FOnPaintScheme
     write FOnPaintScheme;
   property  OnPaintOver: TFlexPaintEvent read FOnPaintOver
     write FOnPaintOver;
   property  OnBeginSelectionUpdate: TNotifyEvent read FOnBeginSelectionUpdate
     write FOnBeginSelectionUpdate;
   property  OnEndSelectionUpdate: TNotifyEvent read FOnEndSelectionUpdate
     write FOnEndSelectionUpdate;
   property  OnProgress: TFlexProgressEvent read FOnProgress write FOnProgress;
   property  OnToolMode: TNotifyEvent read FOnToolMode write FOnToolMode;
   property  OnNeedHint: TFlexNeedHintEvent read FOnNeedHint write FOnNeedHint;
   property  OnShowHint: TFlexShowHintEvent read FOnShowHint write FOnShowHint;
   // inherited
   property  Align;
   property  Anchors;
   property  BiDiMode;
   property  BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
     default bsSingle;
   property  Constraints;
   property  DockSite;
   property  DragCursor;
   property  DragKind;
   property  DragMode;
   property  Enabled;
   property  Color nodefault;
   property  Ctl3D;
   property  Font;
   property  ParentBiDiMode;
   property  ParentColor;
   property  ParentCtl3D;
   property  ParentFont;
   property  ParentShowHint;
   property  PopupMenu;
   property  ShowHint;
   property  TabOrder;
   property  TabStop;
   property  Visible;
   property  OnCanResize;
   property  OnClick;
   property  OnConstrainedResize;
   {$IFDEF FG_D5}
   property  OnContextPopup;
   {$ENDIF}
   property  OnDblClick;
   property  OnDockDrop;
   property  OnDockOver;
   property  OnDragDrop;
   property  OnDragOver;
   property  OnEndDock;
   property  OnEndDrag;
   property  OnEnter;
   property  OnExit;
   property  OnGetSiteInfo;
   property  OnMouseDown;
   property  OnMouseMove;
   property  OnMouseUp;
   property  OnMouseWheel;
   property  OnMouseWheelDown;
   property  OnMouseWheelUp;
   property  OnKeyPress;
   property  OnKeyDown;
   property  OnKeyUp;
   property  OnResize;
   property  OnStartDock;
   property  OnStartDrag;
   property  OnUnDock;
  end;

  TGetNameLinkByFlexPanelEvent = procedure(FlexPanel: TFlexPanel;
    var NameLink: string);

  TGetFlexPanelByNameLinkEvent = procedure(NameLink: string;
    var FlexPanel: TFlexPanel);

var
  GetNameLinkByFlexPanel: TGetNameLinkByFlexPanelEvent;
  GetFlexPanelByNameLink: TGetFlexPanelByNameLinkEvent;

  RegisteredFlexControls: TFlexControlClasses;

{$IFDEF FG_CBUILDER}
// We strongly recommend to use these routine to destroy flex-controls
// in C++Builder environment. It prevents from some incompatibility in
// object finalization section in Delphi and C++Builder.
procedure CppFreeControl(Control: TFlexControl);
{$ENDIF}

function  FirstControl(AControl: TFlexControl;
  var PassRec: TPassControlRec): TFlexControl;
function  NextControl(var PassRec: TPassControlRec;
  Sibling: boolean = false): TFlexControl;
procedure ClosePassRec(var PassRec: TPassControlRec);

procedure RegisterFlexControl(ControlClass: TFlexControlClass);
function  IndexOfRegisteredFlexControlClass(const ClassName: string): integer;

procedure InitPaintOrder(var PaintOrder: TPaintOrder);
procedure ClearPaintOrder(var PaintOrder: TPaintOrder);

function  TranslateRect(const R: TRect;
  const TranslateInfo: TTranslateInfo): TRect;
function  TranslateBounds(const R: TRect;
  const TranslateInfo: TTranslateInfo): TRect;
function  TranslatePoint(const Point: TPoint;
  const TranslateInfo: TTranslateInfo): TPoint;
procedure TranslatePoints(var Points: TPointArray;
  const TranslateInfo: TTranslateInfo);

function  FlexControlCopy(Source, Destination: TFlexControl;
  Filer: TFlexFiler = Nil; ChildrenOnly: boolean = false): boolean;
function  FlexDuplicate(Source: TFlexControl; DestParent: TFlexControl;
  Filer: TFlexFiler = Nil; ChildrenOnly: boolean = false;
  SelectNewControls: boolean = false): TFlexControl;

function  CreateLinkString(Control: TFlexControl; Prop: TCustomProp;
  UseNameLinkPrefix: boolean = false): string;
function  ResolveLinkString(const LinkString: string; FlexPanel: TFlexPanel;
  var Control: TFlexControl; var Prop: TCustomProp;
  RecodeId: TGetIDAlias = Nil; SourceControl: TFlexControl = Nil): boolean;

procedure DefaultGetNameLinkByFlexPanel(FlexPanel: TFlexPanel;
  var NameLink: string);
procedure DefaultGetFlexPanelByNameLink(NameLink: string;
  var FlexPanel: TFlexPanel);

implementation

uses
  {$IFDEF FG_D12}
  Types,
  {$ENDIF}
  {$IFDEF STDFLEXCTRLS}
  FlexControls, // for register flex controls only
  {$ENDIF}
  {$IFDEF USE_FLEXPLUS}
  FlexPlus, // for register flex controls only
  {$ENDIF}
  FlexActions;

{$IFDEF FG_CBUILDER}
procedure CppFreeControl(Control: TFlexControl);
begin
 Control.Free;
end;
{$ENDIF}

function FirstControl(AControl: TFlexControl;
  var PassRec: TPassControlRec): TFlexControl;
begin
 PassRec.Control := AControl;
 if Assigned(PassRec.Control) then begin
  SetLength(PassRec.Indexes, 1);
  PassRec.Indexes[0] := 0;
 end;
 Result := AControl;
end;

function NextControl(var PassRec: TPassControlRec;
  Sibling: boolean = false): TFlexControl;
var Idx: integer;
begin
 Result := Nil;
 with PassRec do
 try
  if not Sibling and (Control.Count > 0) then begin
   Result := Control[0];
   Idx := Length(Indexes);
   SetLength(Indexes, Idx + 1);
   Indexes[Idx] := 0;
  end else
  repeat
   if Length(Indexes) = 1 then begin
    Result := Nil;
    break;
   end;
   Result := Control.Parent;
   if not Assigned(Result) then break;
   Idx := Indexes[High(Indexes)];
   inc(Idx);
   if Idx >= Result.Count then begin
    SetLength(Indexes, Length(Indexes)-1);
    Control := Result;
   end else begin
    Result := Result[Idx];
    Indexes[High(Indexes)] := Idx;
    break;
   end;
  until false;
 finally
  Control := Result;
  if not Assigned(Result) then SetLength(Indexes, 0);
 end;
end;

procedure ClosePassRec(var PassRec: TPassControlRec);
begin
 PassRec.Control := Nil;
 SetLength(PassRec.Indexes, 0);
end;

procedure InitPaintOrder(var PaintOrder: TPaintOrder);
begin
 PaintOrder.LayerRefs := Nil;
 PaintOrder.ControlRefs := Nil;
end;

procedure ClearPaintOrder(var PaintOrder: TPaintOrder);
begin
 SetLength(PaintOrder.LayerRefs, 0);
 SetLength(PaintOrder.ControlRefs, 0);
end;

function TranslateRect(const R: TRect;
  const TranslateInfo: TTranslateInfo): TRect;
var ASin, ACos: extended;
    P: TPoint;
begin
 Result := R;
 with TranslateInfo do begin
  OffsetRect(Result, -Center.X, -Center.Y);
  ASin := sin(-Rotate * pi / 180);
  ACos := cos(-Rotate * pi / 180);
  P := Result.TopLeft;
  if Mirror then P.X := -P.X;
  Result.Left := Round((P.X * ACos) - (P.Y * ASin));
  Result.Top  := Round((P.X * ASin) + (P.Y * ACos));
  P := Result.BottomRight;
  if Mirror then P.X := -P.X;
  Result.Right  := Round((P.X * ACos) - (P.Y * ASin));
  Result.Bottom := Round((P.X * ASin) + (P.Y * ACos));
  OffsetRect(Result, Center.X, Center.Y);
  Result := FlexUtils.NormalizeRect(Result);
 end;
end;

function TranslateBounds(const R: TRect;
  const TranslateInfo: TTranslateInfo): TRect;
var Points: array[0..3] of TPoint;
    Point: TPoint;
    ASin, ACos: extended;
    i: integer;
begin
 Result := R;
 with TranslateInfo do begin
  OffsetRect(Result, -Center.X, -Center.Y);
  // Left-top
  Points[0].X := Result.Left;
  Points[0].Y := Result.Top;
  // Right-top
  Points[1].X := Result.Right;
  Points[1].Y := Result.Top;
  // Right-bottom
  Points[2].X := Result.Right;
  Points[2].Y := Result.Bottom;
  // Left-bottom
  Points[3].X := Result.Left;
  Points[3].Y := Result.Bottom;
  // Calculate sin/cos
  ASin := sin(-Rotate * pi / 180);
  ACos := cos(-Rotate * pi / 180);
  for i:=Low(Points) to High(Points) do begin
   if Mirror then Points[i].X := -Points[i].X;
   Point.X := Round((Points[i].X * ACos) - (Points[i].Y * ASin));
   Point.Y := Round((Points[i].X * ASin) + (Points[i].Y * ACos));
   if i=Low(Points) then begin
    Result.Left := Point.X; Result.Right  := Point.X;
    Result.Top  := Point.Y; Result.Bottom := Point.Y;
   end else begin
    if Point.X < Result.Left  then Result.Left  := Point.X else
    if Point.X > Result.Right then Result.Right := Point.X;
    if Point.Y < Result.Top    then Result.Top   := Point.Y else
    if Point.Y > Result.Bottom then Result.Bottom := Point.Y;
   end;
  end;
  OffsetRect(Result, Center.X, Center.Y);
 end;
end;

function TranslatePoint(const Point: TPoint;
  const TranslateInfo: TTranslateInfo): TPoint;
var PointX, PointY, ASin, ACos: extended;
begin
 with TranslateInfo do begin
  // Calculate sin/cos
  ASin := sin(-Rotate * pi / 180);
  ACos := cos(-Rotate * pi / 180);
  if Mirror
   then PointX := Center.X - Point.X
   else PointX := Point.X - Center.X;
  PointY := Point.Y - Center.Y;
  Result.X := Round(PointX * ACos - PointY * ASin);
  Result.Y := Round(PointX * ASin + PointY * ACos);
  inc(Result.X, Center.X);
  inc(Result.Y, Center.Y);
 end;
end;

procedure TranslatePoints(var Points: TPointArray;
  const TranslateInfo: TTranslateInfo);
var Point: TPoint;
    ASin, ACos: extended;
    i: integer;
begin
 with TranslateInfo do begin
  // Calculate sin/cos
  ASin := sin(-Rotate * pi / 180);
  ACos := cos(-Rotate * pi / 180);
  for i:=0 to Length(Points)-1 do begin
   dec(Points[i].X, Center.X);
   dec(Points[i].Y, Center.Y);
   if Mirror then Points[i].X := -Points[i].X;
   Point.X := Round((Points[i].X * ACos) - (Points[i].Y * ASin));
   Point.Y := Round((Points[i].X * ASin) + (Points[i].Y * ACos));
   inc(Point.X, Center.X);
   inc(Point.Y, Center.Y);
   Points[i] := Point;
  end;
 end;
end;

function FlexControlCopy(Source, Destination: TFlexControl;
  Filer: TFlexFiler = Nil; ChildrenOnly: boolean = false): boolean;
var MS: TMemoryStream;
    InternalCreate: boolean;
    i: integer;
begin
 Result := false;
 if not Assigned(Source) or not Assigned(Destination) then exit;
 MS := Nil;
 InternalCreate := not Assigned(Filer);
 try
  if InternalCreate then begin
   MS := TMemoryStream.Create;
   Filer := TFlexFiler.Create(MS);
  end;
  // Save source object to filer
  if not ChildrenOnly then
   Source.SaveToFiler(Filer, '')
  else
   for i:=0 to Source.Count-1 do
    Source[i].SaveToFiler(Filer, '');
  // Rewind stream to start
  Filer.Rewind;
  // Skip object struct head line
  if not ChildrenOnly then Filer.LoadStr;
  // Load object from filer to destination
  Destination.LoadFromFiler(Filer);
  // Resolve refs
  if Assigned(Destination.Owner) then
   Destination.Owner.PropRefList.ResolveAllRefs;
  // Success
  Result := true;
 finally
  if InternalCreate then begin
   Filer.Free;
   MS.Free;
  end;
 end;
end;

function FlexDuplicate(Source: TFlexControl; DestParent: TFlexControl;
  Filer: TFlexFiler = Nil; ChildrenOnly: boolean = false;
  SelectNewControls: boolean = false): TFlexControl;
var MS: TMemoryStream;
    InternalCreate: boolean;
    i: integer;
    s: string;
    Control: TFlexControl;
begin
 Result := Nil;
 if not Assigned(Source) or not Assigned(DestParent) then exit;	  	
 MS := Nil;
 InternalCreate := not Assigned(Filer);
 try
  if InternalCreate then begin
   MS := TMemoryStream.Create;
   Filer := TFlexFiler.Create(MS);
  end;
  // Save source object to filer
  if ChildrenOnly then begin
   for i:=0 to Source.Count-1 do Source[i].SaveToFiler(Filer, '');
  end else
   Source.SaveToFiler(Filer, '');
  // Rewind stream to start
  Filer.Rewind;
  if SelectNewControls then DestParent.Owner.UnselectAll;
  if ChildrenOnly then begin
   // Load controls from stream
   while Filer.LoadStrCheck(s) do
    if StrBeginsFrom(s, fcObject) then begin
     Control := DestParent.Owner.LoadFlexControl(Filer, DestParent, s);
     if not Assigned(Result) then Result := Control;
     if SelectNewControls then Control.IsSelected := true;
    end else
     // Skip all other
     Filer.CheckLoadSkipToEnd(s);
  end else begin
   // Create and load object from filer
   Result := DestParent.Owner.LoadFlexControl(Filer, DestParent, Filer.LoadStr);
   if SelectNewControls then Result.IsSelected := true;
  end;
  // Resolve refs
  if Assigned(Result) then
   Result.Owner.PropRefList.ResolveAllRefs;
 finally
  if InternalCreate then begin
   Filer.Free;
   MS.Free;
  end;
 end;
end;

procedure RegisterFlexControl(ControlClass: TFlexControlClass);
var i: integer;
begin
 if Length(RegisteredFlexControls) > 0 then
  for i:=0 to High(RegisteredFlexControls) do
   if RegisteredFlexControls[i] = ControlClass then exit;
 SetLength(RegisteredFlexControls, Length(RegisteredFlexControls)+1);
 RegisteredFlexControls[High(RegisteredFlexControls)] := ControlClass;
end;

function IndexOfRegisteredFlexControlClass(const ClassName: string): integer;
var i: integer;
begin
 Result := -1;
 for i:=0 to High(RegisteredFlexControls) do
  if CompareText(RegisteredFlexControls[i].ClassName, ClassName) = 0 then begin
   Result := i;
   break;
  end;
end;

function CreateLinkString(Control: TFlexControl; Prop: TCustomProp;
  UseNameLinkPrefix: boolean = false): string;
var i, Len: integer;
    Quoted: boolean;
    NameLinkPrefix: string;
begin
 if not Assigned(Control) then begin
  Result := '';
  exit;
 end;
 // "Filter" control name
 Result := Control.Name;
 for i:=Length(Result) downto 1 do
  if (Result[i] < ' ') or (Result[i] = '\') then Delete(Result, i, 1);
 // Format link string
 if Assigned(Prop) and (Prop.Owner.Owner = Control) then begin
  i := Prop.Owner.IndexOf(Prop);
  Result := Format('\%d:%s\%s', [Control.Id, Result, Prop.Owner.PropNames[i]]);
 end else
  Result := Format('\%d:%s', [Control.Id, Result]);
 if UseNameLinkPrefix then begin
  // Get owner name link
  GetNameLinkByFlexPanel(Control.Owner, NameLinkPrefix);
  // Check chars
  Quoted := false;
  Len := Length(NameLinkPrefix);
  i := 1;
  while i <= Len do begin
   if (NameLinkPrefix[i] <= ' ') or (NameLinkPrefix[i] = '"') or (NameLinkPrefix[i] = '\') then begin
    Quoted := true;
    if NameLinkPrefix[i] = '"' then begin
     Insert('"', NameLinkPrefix, i);
     inc(i);
    end;
   end;
   inc(i);
  end;
  if Quoted
   then Result := Format('"%s"%s', [NameLinkPrefix, Result])
   else Result := NameLinkPrefix + Result;
 end;
end;

function ResolveLinkString(const LinkString: string; FlexPanel: TFlexPanel;
  var Control: TFlexControl; var Prop: TCustomProp;
  RecodeId: TGetIDAlias = Nil; SourceControl: TFlexControl = Nil): boolean;
var Start, i, Len, CopyLen, ControlID, NewID: integer;
    NameLink: string;
    IsContinue: boolean;
begin
 Result := false;
 Control := Nil;
 Prop := Nil;
 Len := Length(LinkString);
 if Len = 0 then begin
  Result := true;
  exit;
 end;
 if not Assigned(FlexPanel) {or (LinkString[1] <> '\') }then exit;
 i := 2;
 if LinkString[1] <> '\' then begin
  // Try extract FlexPanel link name
  if LinkString[1] = '"' then begin
   // Link string in qoutes
   NameLink := '';
   Start := i;
   while i <= Len do
    if LinkString[i] = '"' then begin
     IsContinue := (i < Len) and (LinkString[i+1] = '"');
     CopyLen := i - Start;
     if IsContinue then inc(CopyLen);
     NameLink := NameLink + copy(LinkString, Start, CopyLen);
     i := Start + CopyLen + 1;
     if not IsContinue then break;
     Start := i;
    end else
     inc(i);
   // Check name delimiter
   if (i >= Len) or (LinkString[i] <> '\') then exit;
  end else begin
   // just find first '\' symbol
   i := pos('\', LinkString);
   if i = 0 then exit;
   NameLink := copy(LinkString, 1, i-1);
   inc(i);
  end;
  // Resolve name link
  if NameLink <> '' then begin
   GetFlexPanelByNameLink(NameLink, FlexPanel);
   if not Assigned(FlexPanel) then exit;
  end;
 end;
 // Find control id
 Start := i;
 while (i <= Len) and
       (LinkString[i] <> ':') and (LinkString[i] <> '\') do inc(i);
 ControlID := StrToIntDef(copy(LinkString, Start, i-Start), 0);
 if Assigned(RecodeId) and (ControlID <> 0) then begin
  // Check resolve ID alias
  NewID := RecodeId(ControlID);
  if NewID <> 0 then ControlID := NewID;
 end;
 // Find control
 Control := FlexPanel.FindControlByID(ControlID);
 if not Assigned(Control) and Assigned(SourceControl) then begin
  while Assigned(SourceControl.Parent) do SourceControl := SourceControl.Parent;
  Control := SourceControl.FindByID(ControlID);
 end;
 if not Assigned(Control) then exit;
 Result := true;
 // Check find property
 if i > Len then exit;
 if LinkString[i] = ':' then
  // Skip control name
  while i <= Len do
   if LinkString[i] = '\' then begin
    inc(i);
    break;
   end else
    inc(i);
 if i > Len then exit;
 // Find property
 Prop := Control.Props[copy(LinkString, i, Len-i +1)];
 Result := Assigned(Prop);
end;

procedure DefaultGetNameLinkByFlexPanel(FlexPanel: TFlexPanel;
  var NameLink: string);
begin
 // As default return just FlexPanel's name
 if Assigned(FlexPanel)
  then NameLink := FlexPanel.Name
  else NameLink := '';
end;

procedure DefaultGetFlexPanelByNameLink(NameLink: string;
  var FlexPanel: TFlexPanel);
begin
 // We can't resolve NameLink's as default since there is not TFlexPanel list
 FlexPanel := Nil;
end;

// TLayerProp /////////////////////////////////////////////////////////////////

constructor TLayerProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FIsEnum := true;
end;

procedure TLayerProp.SetValue(const Value: string);
var Control: TFlexControl;
    Panel: TFlexPanel;
    NewLayer: TFlexLayer;
begin
 if Owner.Owner is TFlexControl then begin
  Control := TFlexControl(Owner.Owner);
  Panel := Control.Owner;
  if not Assigned(Panel) then exit; // Can't change layer without control owner
  if Value = '' then
   NewLayer := Nil
  else begin
   NewLayer := TFlexLayer(Panel.Layers.ByName[Value]);
   if not Assigned(NewLayer) then
    // Wrong layer name
    if not Assigned(Control.Layer) and Assigned(Control.ParentScheme) then
     NewLayer := Panel.ActiveLayer
    else
     exit;
  end;
  // Make layer changing in a control
  Control.Layer := NewLayer;
 end else
  inherited;
end;

function TLayerProp.GetControl: TFlexLayer;
begin
 if Owner.Owner is TFlexControl
  then Result := TFlexControl(Owner.Owner).Layer
  else Result := Nil;
end;

procedure TLayerProp.GetEnumList(List: TStrings);
var Panel: TFlexPanel;
    i: integer;
begin
 List.BeginUpdate;
 try
  List.Clear;
  if not (Owner.Owner is TFlexControl) then exit;
  Panel := TFlexControl(Owner.Owner).Owner;
  for i:=0 to Panel.Layers.Count-1 do
   List.Add(Panel.Layers[i].NameProp.Value);
 finally
  List.EndUpdate;
 end;
end;

procedure TLayerProp.SetControl(const Value: TFlexLayer);
begin
 if Assigned(Value)  
  then Self.Value := Value.NameProp.Value
  else Self.Value := '';
end;

// TSchemeRefProp /////////////////////////////////////////////////////////////

constructor TSchemeRefProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 FIsEnum := true;
end;

function TSchemeRefProp.GetDisplayValue: string;
begin
 if Assigned(FValue)
  then Result := FValue.Name
  else Result := '';
end;

procedure TSchemeRefProp.SetValue(const Value: TFlexControl);
begin
 if (psReadOnly in Style) or (FValue = Value) then exit;
 DoBeforeChanged;
 FValue := Value;
 DoChanged;
end;

function TSchemeRefProp.GetNamesControl: TFlexControl;
begin
 Result := Nil;
 if Assigned(Owner.Owner) and (Owner.Owner is TFlexControl) then begin
  Result := TFlexControl(Owner.Owner).ParentScheme;
  if Assigned(Result) then Result := Result.Parent;
 end;
end;

procedure TSchemeRefProp.GetEnumList(List: TStrings);
var i: integer;
    Control: TFlexControl;
begin
 List.BeginUpdate;
 try
  List.Clear;
  List.Add('');
  Control := GetNamesControl;
  if Assigned(Control) then with Control do
   for i:=0 to Count-1 do List.AddObject(Controls[i].Name, Controls[i]);
 finally
  List.EndUpdate;
 end;
end;

function TSchemeRefProp.GetPropValue(const PropName: string): Variant;
begin
 if Assigned(FValue)
  then Result := FValue.Name
  else Result := '';
end;

procedure TSchemeRefProp.SetPropValue(const PropName: string; Value: Variant);
var AName: string;
    i: integer;
    Control: TFlexControl;
begin
 Control := GetNamesControl;
 if not Assigned(Control) then exit;
 try
  AName := VarAsType(Value, varString);
  if AName = '' then
   Self.Value := Nil
  else
  for i:=0 to Control.Count-1 do
   if CompareStr(Control[i].Name, AName) = 0 then begin
    Self.Value := Control[i];
    break;
   end;
 except
 end;
end;

// TLinkProp //////////////////////////////////////////////////////////////////

constructor TLinkProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 Style := Style + [ psRef ];
 FNotifyLink := TNotifyLink.Create(Self);
 FNotifyLink.OnNotify := LinkedNotify;
 FNotifyLink.OnFreeNotify := LinkedFreeNotify;
end;

destructor TLinkProp.Destroy;
begin
 LinkedControl := Nil;
 inherited; 
end;

function TLinkProp.GetValue: string;
var NewOtherOwner: boolean;
begin
 if Assigned(FControl) and (Owner.Owner is TFlexControl)
  then NewOtherOwner := TFlexControl(Owner.Owner).Owner <> FControl.Owner
  else NewOtherOwner := false;
 if FLinkChanged or (NewOtherOwner <> FLinkToOtherOwner) then begin
  FLinkToOtherOwner := NewOtherOwner;
  FValue := CreateLinkString(FControl, FProp, FLinkToOtherOwner);
  FLinkChanged := false;
 end;
 Result := FValue;
end;

function TLinkProp.SetLink(AControl: TFlexControl; AProp: TCustomProp): boolean;
begin
 // Check unsubscribe from FProp
 if (AProp <> FProp) and Assigned(FProp) then
  FProp.NotifyLink.Unsubscribe(FNotifyLink)
 else
 // Check unsubscribe from FControl
 if (AControl <> FControl) and Assigned(FControl) then
  FControl.NotifyLink.Unsubscribe(FNotifyLink);
 // Set new values
 FControl := AControl;
 FProp := AProp;
 if Assigned(FControl) and (Owner.Owner is TFlexControl)
  then FLinkToOtherOwner := TFlexControl(Owner.Owner).Owner <> FControl.Owner
  else FLinkToOtherOwner := false;
 // Check subscribe to new FProp
 if Assigned(FProp) then
  FProp.NotifyLink.Subscribe(FNotifyLink)
 else
 // Check subscribe to new FControl
 if Assigned(FControl) then
  FControl.NotifyLink.Subscribe(FNotifyLink);
 Result := true;
end;

procedure TLinkProp.LinkedNotify(Sender: TObject; Source: TNotifyLink;
  const Info: TNotifyLinkInfo);
begin
 // Check ID changing
 if (Info.Code = ncControlNotify) and (Info.ControlNotify = fnID) then
  FLinkChanged := true;
 if Assigned(FOnLinkedNotify) then FOnLinkedNotify(Sender, Source, Info);
end;

procedure TLinkProp.LinkedFreeNotify(Sender: TObject; Source: TNotifyLink;
  const Info: TNotifyLinkInfo);
begin
 // Reset link
 if (FControl <> Nil) or (FProp <> Nil) then begin
  DoBeforeChanged;
  FControl := Nil;
  FProp := Nil;
  FLinkChanged := true;
  DoChanged;
 end;
 // Handle event
 if Assigned(FOnLinkedFreeNotify) then
  FOnLinkedFreeNotify(Sender, Source, Info);
end;

procedure TLinkProp.SetValue(const Value: string);
var NewControl: TFlexControl;
    NewProp: TCustomProp;
    Control: TFlexControl;
    RecodeId: TGetIDAlias;
begin
 if not (Owner.Owner is TFlexControl) then exit;
 Control := TFlexControl(Owner.Owner);
 if Assigned(Owner.RefList)
  then RecodeId := Owner.RefList.GetIDAlias
  else RecodeId := Nil;
 if not ResolveLinkString(Value, Control.Owner, NewControl, NewProp, RecodeId,
   Control) then exit;
 if ((NewControl = FControl) and (NewProp = FProp)) or
   Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if SetLink(NewControl, NewProp) then begin
  FValue := Value;
  FLinkChanged := true; //Assigned(OldIds) and Assigned(NewIds);
 end;
 DoChanged;
end;

procedure TLinkProp.SetControl(const Value: TFlexControl);
begin
 if (Value = FControl) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 SetLink(Value, Nil);
 FLinkChanged := true;
 DoChanged;
end;

procedure TLinkProp.SetProp(const Value: TCustomProp);
var NewControl: TFlexControl;
begin
 if (Value = FProp) or Owner.IsReadOnly(Self) then exit;
 DoBeforeChanged;
 if Assigned(Value) then begin
  if Value.Owner.Owner is TFlexControl
   then NewControl := TFlexControl(Value.Owner.Owner)
   else NewControl := Nil;
 end else
  NewControl := FControl;
 SetLink(NewControl, Value);
 FLinkChanged := true;
 DoChanged;
end;

function TLinkProp.GetDisplayValue: string;
begin
 Result := Value;
end;

procedure TLinkProp.SetDisplayValue(const Value: string);
begin
 if (Length(Value) > 0) and (pos('\', Value) >= pos(':', Value)) // Value[1] <> '\')
  then inherited SetDisplayValue('\' + Value)
  else inherited;
end;

function TLinkProp.GetPropValue(const PropName: string): Variant;
begin
 Result := Value;
end;

procedure TLinkProp.SetPropValue(const PropName: string; Value: Variant);
begin
 Self.Value := VarAsType(Value, varString);
end;

// TLinkPointPorp /////////////////////////////////////////////////////////////

constructor TLinkPointProp.Create(AOwner: TPropList; const AName: string; const ATranslatedName: string = '');
begin
 inherited;
 if Owner.Owner is TFlexControl
  then FConnector := TFlexControl(Owner.Owner)
  else raise Exception.Create('Can''t create link prop');
end;

procedure TLinkPointProp.LinkedNotify(Sender: TObject; Source: TNotifyLink;
  const Info: TNotifyLinkInfo);
//var R: TRect;
//    Coeff: extended;
begin
 if Info.Code = ncControlNotify then
  case Info.ControlNotify of
   fnLinkPoints:
     // Check point changing
     if (FLinkPoint.X = FSavedPoint.X) and (FLinkPoint.Y = FSavedPoint.Y) then
      // No point changing. Don't generate notify
      exit
     else
      // Save changes
      FSavedPoint := FLinkPoint;
  end;
 // Generate notify
 inherited;
end;

function TLinkPointProp.SetLink(AControl: TFlexControl;
  AProp: TCustomProp): boolean;
begin
 Result := false;
 if (AControl = FControl) or (AControl = FConnector) then exit; // nothing to do
 // Remove reference to link point from FControl
 if Assigned(FControl) then FControl.RemoveLinkPoint(@FLinkPoint);
 // Ignore AProp (can't reference to prop)
 AProp := Nil;
 // Update FControl and FProp
 Result := inherited SetLink(AControl, AProp);
 if Assigned(FControl) then begin
  // Add reference to new FControl
  FControl.AddLinkPoint(@FLinkPoint);
  // Save FControl DocRect
  FSavedRect := FControl.DocRect;
 end;
end;

procedure TLinkPointProp.SetLinkPoint(const Value: TPoint);
var Info: TNotifyLinkInfo;
begin
 if ((Value.x = FLinkPoint.x) and (Value.y = FLinkPoint.y)) or
    Owner.IsReadOnly(Self) then exit;
 FSkipHistoryAction := true;
 try
  DoBeforeChanged;
 finally
  FSkipHistoryAction := false;
 end;
 FLinkPoint := Value;
 if Assigned(FControl) then
  FControl.DoNotify(fnLinkPoints)
  //FControl.NotifyLink.ControlNotify(FControl, fnLinkPoints)
 else begin
  Info.Code := ncControlNotify;
  Info.Control := FConnector;
  Info.ControlNotify := fnLinkPoints;
  LinkedNotify(FNotifyLink{Self}, FNotifyLink, Info);
 end;
 DoChanged;
end;

function TLinkPointProp.IndexOfLinkPoint: integer;
begin
 if Assigned(FControl)
  then Result := FControl.IndexOfLinkPoint(@FLinkPoint)
  else Result := -1;
end;

// TFlexControl //////////////////////////////////////////////////////////////

constructor TFlexControl.Create(AOwner: TFlexPanel; AParent: TFlexControl;
  ALayer: TFlexLayer);
begin
 inherited Create;
 FState := [ fsCreating ];
 FOwner := AOwner;
 {$IFNDEF FG_CBUILDER}
 InitializeControl(AParent, ALayer);
 {$ELSE}
 // Initialize control in AfterConstruction.
 // (Else BCB reinits any dynamic arrays allocated in ControlCreate)
 FParent := AParent;
 FLayer := ALayer;
 {$ENDIF}
end;

{$IFDEF FG_CBUILDER}
class function TFlexControl.CppCreate(AOwner: TFlexPanel; AParent: TFlexControl;
  ALayer: TFlexLayer): TFlexControl;
// We strongly recommend to use these routine to create flex-control
// in C++Builder environment instead of native object creation via
// Control = new TFlexControl(...). It prevents from some incompatibility in
// object initialization section in Delphi and C++Builder. You also can use
// TFlexPanel::CreateControl() method to create flex-controls in definite flex
// panel.
begin
 Result := Self.Create(AOwner, AParent, ALayer);
end;

procedure TFlexControl.AfterConstruction;
var AParent: TFlexControl;
    ALayer: TFlexLayer;
begin
 AParent := FParent;
 ALayer := FLayer;
 FParent := Nil;
 FLayer := Nil;
 InitializeControl(AParent, ALayer);
end;
{$ENDIF}

destructor TFlexControl.Destroy;
begin
 {$IFNDEF FG_CBUILDER}
 FinalizeControl;
 {$ENDIF}
 inherited;
end;

procedure TFlexControl.BeforeDestruction;
begin
 Include(FState, fsDestroying);
 inherited;
 {$IFDEF FG_CBUILDER}
 FinalizeControl;
 {$ENDIF}
end;

procedure TFlexControl.InitializeControl(AParent: TFlexControl;
  ALayer: TFlexLayer);
var AOwner: TFlexPanel;
    OnAction: TPropHistoryActionEvent;
begin
 AOwner := FOwner;
 FControls := TList.Create;
 FPaintAlphaBufferMode := amAuto;
 FProps := TPropList.Create(Self);
 FProps.OnPropChanged := PropChanged;
 FProps.OnPropBeforeChanged := PropBeforeChanged;
 FProps.OnPropStored := PropStored;
 FProps.OnPropReadOnly := PropReadOnly;
 // Set PropHistoryAction event handler only if necessary (if overrided)
 OnAction := PropHistoryAction;
 if Addr(TFlexControl.PropHistoryAction) <> TMethod(OnAction).Code then
  FProps.OnPropHistoryAction := PropHistoryAction;
 // Create user properties
 CreateProperties;
 FAnchorEnabled := true;
 Parent := AParent;
 if not Assigned(Parent) and Assigned(FOwner) then
  FOwner.AddWithoutParent(Self);
 FLayer := ALayer;
 try
  ControlCreate;
  Exclude(FState, fsCreating);
  if Assigned(FOwner) then FProps.History := FOwner.History;
  if fsRecording in FState then begin
   if FOwner = AOwner then FOwner.History.EndAction;
   Exclude(FState, fsRecording);
  end;
 except
  if fsRecording in FState then begin
   if FOwner = AOwner then FOwner.History.CancelAction;
   Exclude(FState, fsRecording);
  end;
  raise;
 end;
end;

procedure TFlexControl.FinalizeControl;
begin
 if not (fsDestroying in FState) then exit;
 if Assigned(FNotifyLink) then FNotifyLink.DestroyNotify;
 try
  ControlDestroy;
  Clear;
  Parent := Nil;
  if Assigned(FOwner) then begin
   FOwner.FIdPool.Release(FIdProp.Value);
   FOwner.RemoveWithoutParent(Self);
  end;
  if fsRecording in FState then begin
   Exclude(FState, fsRecording);
   FOwner.History.EndAction;
  end;
 except
  if fsRecording in FState then begin
   Exclude(FState, fsRecording);
   FOwner.History.CancelAction;
  end;
  raise;
 end;
 FProps.Free;
 FControls.Free;
 FNotifyLink.Free;
end;

procedure TFlexControl.CreateProperties;
begin
 FIdProp := TLongWordProp.Create(FProps, 'ID');
 FIdProp.Style := FIdProp.Style + [ psNonVisual ];
 FTagProp := TIntProp.Create(FProps, 'Tag');
 FTagProp.Style := FTagProp.Style + [ psNonVisual ];
 FNameProp := TStrProp.Create(FProps, 'Name', 'Имя');
 FNameProp.Style := FNameProp.Style + [ psDontStore, psNonVisual ];
 FLeftProp := TIntProp.Create(FProps, 'Left');
 FLeftProp.Style := FLeftProp.Style + [ psScalable ];
 FTopProp := TIntProp.Create(FProps, 'Top');
 FTopProp.Style := FTopProp.Style + [ psScalable ];
 FWidthProp := TIntProp.Create(FProps, 'Width');
 FWidthProp.Style := FWidthProp.Style + [ psScalable ];
 FHeightProp := TIntProp.Create(FProps, 'Height');
 FHeightProp.Style := FHeightProp.Style + [ psScalable ];
 FVisibleProp := TBoolProp.Create(FProps, 'Visible');
 FVisibleProp.Style := FVisibleProp.Style - [ psVisible ] + [ psDontStore ];
 FHintProp := TStrListProp.Create(FProps, 'Hint');
 FHintProp.Style := FHintProp.Style + [ psNonVisual ];
 FShowHintProp := TBoolProp.Create(FProps, 'ShowHint');
 FShowHintProp.Style := FShowHintProp.Style + [ psNonVisual ];
 FLayerProp := TLayerProp.Create(FProps, 'Layer');
 FLayerProp.Style := FLayerProp.Style + [ psRef ];
 FLayerProp.OnGetString := GetLayerStrProp;
 //FLayerProp.Style := [ psVisible ];
 FReferenceProp := TSchemeRefProp.Create(FProps, 'Reference');
 FReferenceProp.Style := FReferenceProp.Style + [ psNonVisual, psRef ];
 FUserDataProp := TUserDataProp.Create(FProps, 'UserData');
 FTransparencyProp := TIntProp.Create(FProps, 'Transparency');
 FSelectableProp := TBoolProp.Create(FProps, 'Selectable');
 FSelectableProp.Style := FSelectableProp.Style
   + [ psNonVisual ] - [ psVisible ];
 FChannelsProp := TChannelProp.Create(FProps, 'Channels', 'Каналы');
 FControlObjectProp := TControlObjectProp.Create(FProps, 'ControlObject', 'Управление');
 FClickActionProp := TScriptProp.Create(FProps, 'ClickAction', 'Действие');
end;

procedure TFlexControl.ControlCreate;
begin
 if Assigned(FOwner) then begin
  if not FOwner.IsLoading then FOwner.GenerateID(Self);
  if FOwner.AutoNames and (NameProp.Value = '') and not FOwner.IsLoading then
   Name := FOwner.GetDefaultNewName(Self, Nil, FOwner.AutoNameNumbs);
  // Record control create
  if Assigned(FOwner.History.BeginAction(TControlCreateHistoryGroup, Self)) then
   FState := [ fsRecording ];
 end;
 FShowHintProp.Value := true;
 FSelectableProp.Value := true;
 DoNotify(fnCreated);
end;

procedure TFlexControl.ControlDestroy;
begin
 if Assigned(FOwner) and
    Assigned(FOwner.History.BeginAction(TControlDestroyHistoryGroup, Self)) then
  Include(FState, fsRecording);
 DoNotify(fnDestroyed);
end;

class function TFlexControl.CursorInCreate: TCursor;
begin
 Result := crCreateControlCursor;
end;

function TFlexControl.GetNotifyLink: TNotifyLink;
begin
 if not Assigned(FNotifyLink) and not (fsDestroying in FState) then
  FNotifyLink := TNotifyLink.Create(Self);
 Result := FNotifyLink;
end;

class function TFlexControl.GetToolInfo(ToolIcon: TBitmap;
  var Hint: string): boolean;
begin
 Result := False;
end;

class function TFlexControl.IsConnectorControl: boolean;
begin
 Result := False;
end;

procedure TFlexControl.ConnectorMinGapChanged;
begin
 // Called when TFlexPanel(Owner).ConnectorsMinGap changed
end;

procedure TFlexControl.DoNotify(Notify: TFlexNotify);
var NewRect, OldRect, LinkRect: TRect;

 procedure RecordDocRect(Control: TFlexControl);
 var Group: THistoryGroup;
     Action: THistoryAction;
     Diff: TPoint;
     NowDocRect: TRect;
     i: integer;
 begin
  if not FOwner.History.IsRecordable then exit;
  Group := Nil;
  try
   if Control.FControls.Count > 0 then begin
    // First record all children
    if FOwner.History.GroupLevel = 0 then
     Group := FOwner.History.BeginPanelGroup(TPanelSizeMoveHistoryGroup);
    // Self DocRect changes, so calc differences
    NowDocRect := Control.DocRect;
    Diff.X := Control.FSavedDocRect.Left - NowDocRect.Left;
    Diff.Y := Control.FSavedDocRect.Top - NowDocRect.Top;
    // Iterate self children
    for i:=0 to Control.FControls.Count-1 do
     with TFlexControl(Control.FControls[i]) do begin
      BeginUpdate;
      try
       if FControls.Count > 0 then
        RecordDocRect(TFlexControl(Control.FControls[i]))
       else begin
        // Save control action
        if Control.PointCount = 0 then begin
         Action := Owner.History.RecordAction(TDocRectHistoryAction,
             TFlexControl(Control.FControls[i]));
         // Correct recorded undo DocRect
         if Assigned(Action) then TDocRectHistoryAction(Action).OffsetUndo(Diff);
        end else begin
         Action := Owner.History.RecordAction(TPointsHistoryAction,
           TFlexControl(Control.FControls[i]));
         // Correct recorded undo DocRect
         if Assigned(Action) then with TPointsHistoryAction(Action) do begin
          SetUndoSavedDocRect(Control);
          OffsetUndo(Diff);
         end;
        end;
       end;
      finally
       EndUpdate;
      end;
     end;
   end;
   // Record self
   if Control.PointCount > 0 then begin
    Action := FOwner.History.RecordAction(TPointsHistoryAction, Control);
    if Assigned(Action) then
     TPointsHistoryAction(Action).SetUndoSavedDocRect(Control);
   end else
    FOwner.History.RecordAction(TDocRectHistoryAction, Control);
  finally
   if Assigned(Group) then
    FOwner.History.EndPanelGroup(TPanelSizeMoveHistoryGroup);
  end;
 end;

begin
 case Notify of
  fnLinkPoints:
    if FUpdateCounter > 0 then begin
     // Flex-control in update mode - skip notify
     FLinkPointsChanged := true;
     exit;
    end;
  fnParent,
  fnRect:
    if (Notify = fnRect) and (FUpdateCounter > 0) then begin
     FRectChanged := true;
     exit;
    end else begin
     // Save history action
     if (Notify = fnRect) and Assigned(FOwner) then RecordDocRect(Self);
     // Check group
     LinkRect := FLinkSavedBounds;
     if Assigned(FParent) and (FParent is TFlexGroup) and
       not (fsResizing in FState) and not FOwner.IsLoading then begin
      // Refresh parent bounds
      TFlexGroup(FParent).RefreshBounds;
     end;
     // Check link points
     if (FTranslateCount = 0) and
       Assigned(FLinkPoints) and (FLinkPoints.Count > 0) then begin
      // if Assigned(FParent) and (FParent.FGroupUngroupLevel > 0) then exit;
      NewRect := DocRect;
      // We must use OldRect since LinkPointsResize call BeginUpdate
      // where FLinkSavedBounds changes
      OldRect := LinkRect;
      LinkPointsResize(OldRect, NewRect);
      FLinkSavedBounds := NewRect;
     end;
    end;
 end;
 if Assigned(FOwner) then FOwner.DoNotify(Self, Notify);
 if Assigned(FNotifyLink) then FNotifyLink.ControlNotify(Self, Notify);
end;

procedure TFlexControl.DoNeedHint(var HintInfo: THintInfo;
  var IsShowHint: boolean); 
begin
 if IsShowHint and FShowHintProp.Value then
  HintInfo.HintStr := FHintProp.Text;
end;

procedure TFlexControl.SetParent(const Value: TFlexControl);
begin
 if Value = FParent then exit;
 if Assigned(Value) then
  Value.Add(Self)
 else
 if Assigned(FParent) then
  FParent.Extract(Self);
end;

function TFlexControl.Add(AControl: TFlexControl): integer;
var {PassRec: TPassControlRec;
    Control: TFlexControl;}
    IsSameOwner: boolean;
    SrcAction: THistoryAction;
    DstAction: THistoryAction;
    NeedDisable: boolean;
    OrigLayer: TFlexLayer;
begin
 Result := FControls.IndexOf(AControl);
 if Result >= 0 then exit;
 SrcAction := Nil;
 DstAction := Nil;
 try
  // Create redo/undo actions
  IsSameOwner := AControl.Owner = FOwner;
  NeedDisable := false;
  if IsSameOwner then begin
   OrigLayer := AControl.Layer;
   // Create just change order action
   if Assigned(FOwner) then
    SrcAction := FOwner.History.BeginAction(TOrderHistoryAction, AControl);
  end else begin
   OrigLayer := Nil;
   // Create action AControl owner
   if Assigned(AControl.Owner) then
    DstAction := AControl.Owner.History.BeginAction(
      TControlDestroyHistoryGroup, AControl);
   // Disable recording in self owner
   NeedDisable := true;
  end;
  if NeedDisable then FOwner.History.DisableRecording;
  try
   // Begin Add phase
   Include(AControl.FState, fsAdding);
   try
    // Extract from old parent
    if Assigned(AControl.Parent) then AControl.Parent.Extract(AControl);
    // Change owner if necessary
    if not IsSameOwner then AControl.Owner := FOwner;
    Result := FControls.Add(AControl);
    if not Assigned(AControl.FParent) and Assigned(FOwner) then
     FOwner.RemoveWithoutParent(AControl);
    AControl.FParent := Self;
   finally
    Exclude(AControl.FState, fsAdding);
   end;
   if Assigned(FLayer) and (AControl.Layer <> FLayer) then
    AControl.Layer := FLayer
   else
   // Check insertion in scheme
   if Assigned(OrigLayer) and (Self is TFlexCustomScheme) then
    AControl.Layer := OrigLayer
   else
    AControl.Invalidate;
  finally
   if NeedDisable then FOwner.History.EnableRecording;
  end;
  // Record self action if necessary
  if not IsSameOwner then
   SrcAction := FOwner.History.BeginAction(
     TControlCreateHistoryGroup, AControl);
 finally
  if Assigned(SrcAction) then SrcAction.Owner.EndAction;
  if Assigned(DstAction) then DstAction.Owner.EndAction;
 end;
 AControl.DoNotify(fnParent);
 DoNotify(fnOrder);
end;

procedure TFlexControl.Clear;
var i: integer;
begin
 i := FControls.Count-1;
 while i >= 0 do begin
  Delete(i);
  dec(i);
 end;
end;

procedure TFlexControl.Delete(Index: integer);
var Control: TFlexControl;
begin
 Control := TFlexControl(FControls[Index]);
 with Control do
  if not ((fsDestroying in FState) or (fsExtracting in Self.FState)) then
   Free
  else begin
   FParent := Nil;
   if not (fsDestroying in FState) and Assigned(FOwner) then
    FOwner.AddWithoutParent(Control);
   Self.FOwner.Unselect(Control);
   Self.FControls.Delete(Index);
   if not (fsAdding in FState) then begin
    DoNotify(fnParent);
    Self.DoNotify(fnOrder);
   end;
  end;
end;

procedure TFlexControl.Remove(AControl: TFlexControl);
var Index: integer;
begin
 Index := FControls.IndexOf(AControl);
 if Index >= 0 then Delete(Index);
end;

procedure TFlexControl.Extract(AControl: TFlexControl);
begin
 Include(FState, fsExtracting);
 try
  AControl.IsSelected := False;
  AControl.Invalidate;
  Remove(AControl);
  if FControls.IndexOf(AControl) < 0 then begin
   AControl.FParent := Nil;
   AControl.FLayer := Nil;
  end;
 finally
  Exclude(FState, fsExtracting);
 end;
end;

procedure TFlexControl.ChangeOrder(CurIndex, NewIndex: integer);
begin
 if CurIndex = NewIndex then exit;
 if Assigned(FOwner) and (CurIndex >= 0) and (CurIndex < FControls.Count) then
  FOwner.History.RecordAction(TOrderHistoryAction, FControls[CurIndex]);
 FControls.Move(CurIndex, NewIndex);
 Controls[NewIndex].Invalidate;
 DoNotify(fnOrder);
end;

procedure TFlexControl.ToFront;
var Index: integer;
begin
 if not Assigned(FParent) then exit;
 Index := FParent.FControls.IndexOf(Self);
 if Index < 0 then exit;
 FParent.ChangeOrder(Index, FParent.FControls.Count-1);
end;

procedure TFlexControl.ToBack;
var Index: integer;
begin
 if not Assigned(FParent) then exit;
 Index := FParent.FControls.IndexOf(Self);
 if Index < 0 then exit;
 FParent.ChangeOrder(Index, 0);
end;

procedure TFlexControl.ForwardOne;
var Index: integer;
begin
 if not Assigned(FParent) then exit;
 Index := FParent.FControls.IndexOf(Self);
 if (Index < 0) or (Index = FParent.FControls.Count-1) then exit;
 FParent.ChangeOrder(Index, Index+1);
end;

procedure TFlexControl.BackOne;
var Index: integer;
begin
 if not Assigned(FParent) then exit;
 Index := FParent.FControls.IndexOf(Self);
 if Index <= 0 then exit;
 FParent.ChangeOrder(Index, Index-1);
end;

function TFlexControl.IndexOf(AControl: TFlexControl): integer;
begin
 Result := FControls.IndexOf(AControl);
end;

function TFlexControl.FindControlAtPoint(x, y: integer): TFlexControl;
var i: integer;
begin
 Result := Nil;
 for i:=FControls.Count-1 downto 0 do
  with TFlexControl(FControls[i]) do
   if FVisibleProp.Value and IsSelectable and IsPointInside(x, y) then begin
    Result := TFlexControl(Self.FControls[i]);
    break;
   end;
end;

function TFlexControl.ValidateName(CheckEmpty, CheckUnique: boolean): boolean;
var i: integer;
begin
 Result := not (CheckEmpty and (FNameProp.Value = ''));	  	
 if Result and CheckUnique and Assigned(FParent) then begin
  for i:=0 to FParent.Count-1 do
   if (FParent[i] <> Self) and
      (CompareStr(FNameProp.Value, FParent[i].Name) = 0) then begin
    Result := False;
    break;
   end;
 end;
end;

function TFlexControl.BeginUpdate: boolean;
begin
 Result := FUpdateCounter = 0;
 if Result then begin
  Invalidate;
  FLinkPointsChanged := false;
  FRectChanged := false;
  FSavedDocRect := DocRect;
  if not (fsEndUpdating in FState) then
   FLinkSavedBounds := FSavedDocRect;
 end;
 inc(FUpdateCounter);
end;

function TFlexControl.EndUpdate: boolean;
begin
 if FUpdateCounter = 0 then
  Result := False
 else begin
  Include(FState, fsEndUpdating);
  dec(FUpdateCounter);
  Result := FUpdateCounter = 0;
  if Result then begin
   if FRectChanged then begin
    FRectChanged := false;
    DoNotify(fnRect);
   end;
   if FLinkPointsChanged then begin
    FLinkPointsChanged := false;
    DoNotify(fnLinkPoints);
   end;
   Invalidate;
  end;
  Exclude(FState, fsEndUpdating);
 end;
end;

function TFlexControl.BeginGroupUngroup: boolean;
begin
 Result := FGroupUngroupLevel = 0;
 inc(FGroupUngroupLevel);
end;

function TFlexControl.EndGroupUngroup: boolean;
begin
 if FGroupUngroupLevel = 0 then begin
  Result := false;
  exit;
 end;
 dec(FGroupUngroupLevel);
 Result := FGroupUngroupLevel = 0;
end;

procedure TFlexControl.BeginPointsDesigning;
begin
 inc(FPointsDesigning);
end;

procedure TFlexControl.EndPointsDesigning;
begin
 if FPointsDesigning > 0 then dec(FPointsDesigning);
end;

procedure TFlexControl.GetLinkProps(var LinkFirst, LinkLast: TLinkPointProp); 
begin
 // Default - not link point props
 LinkFirst := Nil;
 LinkLast  := Nil;
end;

function TFlexControl.GetDefaultLinkPoint(Index: integer): TPoint;
begin
 Result.x := 0;
 Result.y := 0;
end;

function TFlexControl.GetDefaultLinkPointCount: integer;
begin
 // Default - no link points
 Result := 0;
end;

function TFlexControl.IndexOfLinkPoint(Point: PPoint): integer;
begin
 if not Assigned(FLinkPoints)
  then Result := -1
  else Result := FLinkPoints.IndexOf(Point);
end;

function TFlexControl.AddLinkPoint(Point: PPoint): integer;
begin
 if not Assigned(FLinkPoints) then
  FLinkPoints := TList.Create
 else begin
  Result := FLinkPoints.IndexOf(Point);
  if Result >= 0 then exit;
 end;
 Result := FLinkPoints.Add(Point);
 DoNotify(fnLinkPoints);
end;

procedure TFlexControl.DeleteLinkPoint(Index: integer);
begin
 if not Assigned(FLinkPoints) then
  {$IFDEF FG_D4}
  TList.Error(SListIndexError, Index)
  {$ELSE}
  TList.Error(@SListIndexError, Index)
  {$ENDIF}
 else begin
  FLinkPoints.Delete(Index);
  if FLinkPoints.Count = 0 then FreeAndNil(FLinkPoints);
  DoNotify(fnLinkPoints);
 end;
end;

procedure TFlexControl.RemoveLinkPoint(Point: PPoint);
var Index: integer;
begin
 Index := IndexOfLinkPoint(Point);
 if Index >= 0 then DeleteLinkPoint(Index);
end;

function TFlexControl.GetLinkPointCount: integer;
begin
 if not Assigned(FLinkPoints)
  then Result := 0
  else Result := FLinkPoints.Count;
end;

function TFlexControl.GetLinkPoint(Index: integer): TPoint;
begin
 if not Assigned(FLinkPoints) then
  {$IFDEF FG_D4}
  TList.Error(SListIndexError, Index)
  {$ELSE}
  TList.Error(@SListIndexError, Index)
  {$ENDIF}
 else
  Result := PPoint(FLinkPoints[Index])^;
end;

procedure TFlexControl.SetLinkPoint(Index: integer; const Value: TPoint);
begin
 if not Assigned(FLinkPoints) then
  {$IFDEF FG_D4}
  TList.Error(SListIndexError, Index)
  {$ELSE}
  TList.Error(@SListIndexError, Index)
  {$ENDIF}
 else begin
  PPoint(FLinkPoints[Index])^ := Value;
  DoNotify(fnLinkPoints);
 end;
end;

function TFlexControl.GetAnchorPoint: TPoint;
begin
 with DocRect do begin
  Result.X := Left;
  Result.Y := Top;
 end;
 FOwner.TransformPointIndirect(Result);
end;

function TFlexControl.CreatePointsPath(DC: HDC): boolean;
begin
 Result := false;
end;

function TFlexControl.GetTransformPoints(OfsX, OfsY,
  Scale: integer): TPointArray;
var i, AddX, AddY: integer;
    Denominator: integer;
begin
 SetLength(Result, PointCount);
 if PointCount = 0 then exit;
 with DocRect do begin
  AddX := Left;
  AddY := Top;
 end;
 dec(OfsX, ScaleValue(AddX, Scale));
 dec(OfsY, ScaleValue(AddY, Scale));
 Denominator := 100 * PixelScaleFactor;
 for i:=0 to PointCount-1 do with Points[i] do begin
  Result[i].X := MulDiv(X+AddX, Scale, Denominator) + OfsX;
  Result[i].Y := MulDiv(Y+Addy, Scale, Denominator) + OfsY;
 end;
end;

function TFlexControl.GetPoint(Index: integer): TPoint;
begin
 Result.x := 0;
 Result.y := 0;
end;

procedure TFlexControl.SetPoint(Index: integer; const Value: TPoint);
begin
end;

procedure TFlexControl.SetPoints(const APoints: TPointArray);
var Types: TPointTypeArray;
    i, Count: integer;
begin
 Count := Length(APoints);
 SetLength(Types, Count);
 if Count > 0 then begin
  for i:=0 to Count-2 do Types[i] := ptNode;
  if IsPointsClosed
   then Types[Count-1] := ptEndNodeClose
   else Types[Count-1] := ptEndNode;
 end;
 SetPointsEx(APoints, Types);
end;

procedure TFlexControl.SetPointsEx(const APoints: TPointArray;
  const ATypes: TPointTypeArray);
begin
end;

procedure TFlexControl.GetPointsEx(out APoints: TPointArray;
  out ATypes: TPointTypeArray);
begin
end;

function TFlexControl.GetPointType(Index: integer): TPointType;
begin
 if Index = PointCount-1 then begin
  if IsPointsClosed
   then Result := ptEndNodeClose
   else Result := ptEndNode;
 end else
  Result := ptNode;
end;

procedure TFlexControl.SetPointType(Index: integer;
  const Value: TPointType);
begin
end;

function TFlexControl.GetIsPointsClosed: boolean;
begin
 Result := True;
end;

procedure TFlexControl.SetIsPointsClosed(Value: boolean);
begin
end;

function TFlexControl.FlattenPoints(const Curvature: single): boolean;
begin
 Result := false;
end;

function TFlexControl.FindNearestPoint(const Point: TPoint;
  var Nearest: TNearestPoint): boolean;
begin
 Result := false;
end;

function TFlexControl.FindNearestPathSegment(const Point: TPoint;
  var FirstIndex, NextIndex: integer; Nearest: PNearestPoint = Nil;
  ForInsert: boolean = true; ForSelect: boolean = false): boolean;
begin
 Result := false;
end;

function TFlexControl.EditPoints(Func: TPathEditFunc;
  const Selected: TSelectedArray; Params: PPathEditParams = Nil): boolean;
begin
 Result := false;
end;

function TFlexControl.EditPointsCaps(
  const Selected: TSelectedArray): TPathEditFuncs;
begin
 Result := [];
end;

function TFlexControl.InsertPoint(Index: integer; const Point: TPoint): integer;
begin
 Result := -1;
end;

function TFlexControl.InsertCurvePoints(Index: integer; const Point,
  CtrlPointA, CtrlPointB: TPoint): integer;
begin
 Result := -1;
end;

procedure TFlexControl.DeletePoint(Index: integer);
begin
end;

function TFlexControl.AddCurvePoints(const Point, CtrlPointA,
  CtrlPointB: TPoint): integer;
begin
 Result := InsertCurvePoints(PointCount, Point, CtrlPointA, CtrlPointB);
end;

function TFlexControl.AddPoint(const Point: TPoint): integer;
begin
 Result := InsertPoint(PointCount, Point);
end;

function TFlexControl.InsertCurveNode(NodeIndex: integer; const Point,
  CtrlPointA, CtrlPointB: TPoint): integer;
var Index: integer;
begin
 Result := -1;
 if NodeIndex = NodeCount
  then Index := PointCount
  else Index := GetPointIndex(NodeIndex);
 if InsertCurvePoints(Index, Point, CtrlPointA, CtrlPointB) >= 0 then
  Result := NodeIndex;
end;

procedure TFlexControl.DeleteNode(NodeIndex: integer);
begin
 DeletePoint(GetPointIndex(NodeIndex));
end;

function TFlexControl.InsertNearestPoint(const Point: TPoint): integer;
begin
 Result := -1;
end;

procedure TFlexControl.EndFigure(Close: boolean);
var i: integer;
begin
 i := PointCount-1;
 while (i >= 0) and (PointTypes[i] = ptControl) do dec(i);
 if i >= 0 then
  if Close
   then PointTypes[i] := ptEndNodeClose
   else PointTypes[i] := ptEndNode;
end;

function TFlexControl.GetNode(NodeIndex: integer): TPoint;
begin
 Result := GetPoint(GetPointIndex(NodeIndex));
end;

procedure TFlexControl.SetNode(NodeIndex: integer; const Value: TPoint);
begin
 SetPoint(GetPointIndex(NodeIndex), Value);
end;

function TFlexControl.GetNodeCount: integer;
begin
 Result := PointCount;
 if Result > 0 then Result := GetNodeIndex(Result);
end;

function TFlexControl.GetNodeType(NodeIndex: integer): TPointType;
begin
 Result := GetPointType(GetPointIndex(NodeIndex));
end;

procedure TFlexControl.SetNodeType(NodeIndex: integer;
  const Value: TPointType);
begin
 SetPointType(GetPointIndex(NodeIndex), Value);
end;

function TFlexControl.GetPointCount: integer;
begin
 Result := 0;
end;

function TFlexControl.GetNodeIndex(Index: integer): integer;
var i: integer;
begin
 // Must be overriden for direct access to point arrays
 Result := -1;
 if (Index < PointCount) and
    (PointTypes[Index] = ptControl) then exit;
 inc(Result);
 for i:=0 to Index-1 do
  if PointTypes[i] <> ptControl then inc(Result);
end;

function TFlexControl.GetPointIndex(NodeIndex: integer): integer;
var i: integer;
begin
 // Must be overriden for direct access to point arrays
 Result := -1;
 for i:=0 to PointCount-1 do
  if PointTypes[i] <> ptControl then begin
   dec(NodeIndex);
   if NodeIndex < 0 then begin
    Result := i;
    break;
   end;
  end;
end;

function TFlexControl.GetPointsInfo: PPathInfo;
begin
 Result := Nil;
end;

procedure TFlexControl.BeginSelectionTransformation;
begin
 // Default - do nothing.
 // Call from TFlexPanel methods
 FSavedDocRect := DocRect;
end;

procedure TFlexControl.EndSelectionTransformation;
begin
 // Default - do nothing
 // Call from TFlexPanel methods
end;

procedure TFlexControl.BeginTranslate;
begin
 inc(FTranslateCount);
end;

procedure TFlexControl.EndTranslate;
begin
 if FTranslateCount > 0 then dec(FTranslateCount);
end;

procedure TFlexControl.ControlTranslate(const TranslateInfo: TTranslateInfo);
var R: TRect;
begin
 BeginTranslate;
 try
  // Default translation
  R := DocRect;
  DocRect := TranslateRect(R, TranslateInfo);
  // Translate link points
  LinkPointsTranslate(TranslateInfo);
 finally
  EndTranslate;
 end;
end;

procedure TFlexControl.LinkPointsTranslate(const TranslateInfo: TTranslateInfo);
var i: integer;
begin
 if (LinkPointCount = 0) or (Owner.History.InProcessSource = Self) then exit;
 BeginUpdate;
 try
  for i:=0 to LinkPointCount-1 do
   LinkPoints[i] := TranslatePoint(LinkPoints[i], TranslateInfo);
 finally
  EndUpdate;
 end;
end;

procedure TFlexControl.LinkPointsResize(const OldDocRect, NewDocRect: TRect);
var i: integer;
    CoeffX, CoeffY: double;
begin
 if not Assigned(FLinkPoints) or (FLinkPoints.Count = 0) or
   EqualRect(OldDocRect, NewDocRect) or
   (Owner.History.InProcessSource = Self) then exit;
 BeginUpdate;
 try
  if (fsResizing in FState) and
    (Length(FResizingLinkPoints) = FLinkPoints.Count) then begin
   with FResizingRect do begin
    if Right <> Left
     then CoeffX := (NewDocRect.Right - NewDocRect.Left) / (Right - Left)
     else CoeffX := 0;
    if Bottom <> Top
     then CoeffY := (NewDocRect.Bottom - NewDocRect.Top) / (Bottom - Top)
     else CoeffY := 0;
   end;
   for i:=0 to FLinkPoints.Count-1 do with PPoint(FLinkPoints[i])^ do begin
    X := NewDocRect.Left +
      Round(CoeffX * (FResizingLinkPoints[i].X - FResizingRect.Left));
    Y := NewDocRect.Top +
      Round(CoeffY * (FResizingLinkPoints[i].Y - FResizingRect.Top));
   end;
  end else begin
   if OldDocRect.Right <> OldDocRect.Left
    then CoeffX := (NewDocRect.Right - NewDocRect.Left) /
                   (OldDocRect.Right - OldDocRect.Left)
    else CoeffX := 0;
   if OldDocRect.Bottom <> OldDocRect.Top
    then CoeffY := (NewDocRect.Bottom - NewDocRect.Top) /
                   (OldDocRect.Bottom - OldDocRect.Top)
    else CoeffY := 0;
   for i:=0 to FLinkPoints.Count-1 do with PPoint(FLinkPoints[i])^ do begin
    X := NewDocRect.Left + Round(CoeffX * (X - OldDocRect.Left));
    Y := NewDocRect.Top + Round(CoeffY * (Y - OldDocRect.Top));
   end;
  end;
  FLinkPointsChanged := true;
 finally
  EndUpdate;
 end;
end;

procedure TFlexControl.MirrorInResize(HMirror, VMirror: boolean);
var i, H, W: integer;
begin
 if fsResizing in FState then begin
  // Mirror resizing link points
  H := RectHeight(FResizingRect);
  W := RectWidth(FResizingRect);
  // Mirror FResizePoints
  for i:=0 to Length(FResizingLinkPoints)-1 do
   with FResizingLinkPoints[i] do begin
    if HMirror then X := W - X + 2 * FResizingRect.Left;
    if VMirror then Y := H - Y + 2 * FResizingRect.Top;
   end;
 end;
end;

function TFlexControl.CreateCurveControl: TFlexControl;
begin
 // Default - convert not available
 Result := Nil;
end;

procedure TFlexControl.CreateInDesign(var Info: TFlexCreateInDesignInfo);
begin
 // Default - transform whole object after create in design mode
 Info.IsPointEdit := false;
 Info.IsContinueAvail := false;
end;

procedure TFlexControl.CreateCurveGuide(const NewPoint: TPoint;
  var Guide: TFlexEditPointGuide);
var FirstIndex, NextIndex: integer;
    FigIndex, ActiveIndex, i: integer;
begin
 Guide.Count := 0;
 Guide.NewPoint := NewPoint;
 if PointCount = 0 then exit;
 if not FindNearestPathSegment(NewPoint, FirstIndex, NextIndex) then exit;
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
   ActiveIndex := InsertPathPoint(Points, Types, 0, ActiveIndex, NewPoint,
     UnScaleValue(SelectionThreshold, FOwner.Scale), // "Stickness"
     Nil);
   if ActiveIndex < 0 then begin
    SetLength(Points, 0);
    SetLength(Types, 0);
   end;
  end else begin
   SetLength(Points, 2);
   SetLength(Types, 2);
   Points[0] := Self.Points[FirstIndex];
   Types[0] := ptNode;
   Points[1] := NewPoint;
   Types[1] := ptEndNode;
   ActiveIndex := 1;
  end;
  Count := Length(Points);
  // Set Visible flags
  SetLength(Visible, Count);
  for i:=0 to Count-1 do Visible[i] := i = ActiveIndex;
  // Update NewPoint
  NewPoint := Points[ActiveIndex];
 end;
end;

function TFlexControl.MovePathPoints(PointIndex: integer; var Delta: TPoint;
  Selected: TSelectedArray; Smooth: boolean = false;
  Symmetric: boolean = false): boolean;
var MovePoint: TPoint;
    TempPoint: TPoint;             
    Index: integer;
    NodeIndex: integer;
    FigIndex: integer;
    PathEdit: TPathEditParams;
    Coeff, x0, y0, x1, y1, x2, y2: Double;
begin
 Result := false;
 if PointIndex >= PointCount then exit;
 if (PointIndex >= 0) and (PointTypes[PointIndex] = ptControl) then begin
  // Bezier control point moving
  MovePoint := Points[PointIndex];
  inc(MovePoint.X, Delta.X);
  inc(MovePoint.Y, Delta.Y);
  Points[PointIndex] := MovePoint;
  if Smooth or Symmetric then begin
   // Find second control point of the selected node
   Index := -1;
   NodeIndex := -1;
   FigIndex := GetFigureIndex(PointsInfo^, PointIndex);
   if FigIndex >= 0 then with PointsInfo.Figures[FigIndex] do
    if PointTypes[PointIndex-1] <> ptControl then begin
     // Is moving first control point
     NodeIndex := PointIndex-1;
     if NodeIndex > FirstNode then
      Index := NodeIndex - 1
     else
     if IsClosed then
      Index := LastPoint
    end else begin
     // Is moving second control point
     NodeIndex := PointIndex + 1;
     if NodeIndex > LastNode then begin
      NodeIndex := FirstNode;
      Index := NodeIndex + 1
     end else
     if NodeIndex < LastPoint then
      Index := NodeIndex + 1;
     //if IsClosed and (LastPoint > LastNode) then
     // Index := FirstNode + 1
    end;
   if (Index > 0) and (PointTypes[Index] = ptControl) then
   with Points[NodeIndex] do begin
    // Move second control point
    MovePoint.x := 2*x - Points[PointIndex].x;
    MovePoint.y := 2*y - Points[PointIndex].y;
    if not Symmetric and Smooth then begin
     // Calc length coeff
     TempPoint := Points[Index];
     if (TempPoint.x <> x) or (TempPoint.y <> y) then begin
      x0 := x;
      y0 := y;
      x1 := TempPoint.x;
      y1 := TempPoint.y;
      x2 := MovePoint.x;
      y2 := MovePoint.y;
      Coeff := Sqrt(
        ((x0 - x1)*(x0 - x1) + (y0 - y1)*(y0 - y1)) /
        ((x0 - x2)*(x0 - x2) + (y0 - y2)*(y0 - y2)) );
     end else
      Coeff := 0;
     // Calc new point
     MovePoint.x := x - Round((x - MovePoint.x) * Coeff);
     MovePoint.y := y - Round((y - MovePoint.y) * Coeff);
    end;
    Points[Index] := MovePoint;
   end;
  end;
  Result := true;
 end else
 if Assigned(Selected) then begin
  // Move selected points
  PathEdit.Offset := Delta;
  PathEdit.MoveControls := true;
  EditPoints(pfOffset, Selected, @PathEdit);
  Result := true;
 end else
 if PointIndex >= 0 then begin
  // Move Points[PointIndex] point
  MovePoint := Points[PointIndex];
  inc(MovePoint.X, Delta.X);
  inc(MovePoint.Y, Delta.Y);
  Points[PointIndex] := MovePoint;
  Result := true;
 end;
end;

function TFlexControl.MovePathSegment(FirstIndex, NextIndex: integer;
  var Delta: TPoint; const SegmentCurvePos: double): boolean;
const Epsilon = 0.00001;
var PtCount: integer;
    Coeff: double;
    CtrlDisp: TPoint;
begin
 Result := false;
 PtCount := PointCount;
 if (FirstIndex = NextIndex) or (FirstIndex < 0) or (NextIndex < 0) or
    (FirstIndex >= PtCount) or (NextIndex >= PtCount) or
   ((Delta.x = 0) and (Delta.y = 0)) then exit;
 if (FirstIndex < PtCount-1) and
   (PointTypes[FirstIndex+1] = ptControl) then begin
  if (SegmentCurvePos < Epsilon) or (SegmentCurvePos > 1 - Epsilon) then
    Exit;
  // Move bezier curve segment
  Coeff := 1 / (3 * (1 - SegmentCurvePos) * SegmentCurvePos);
  CtrlDisp.x := Round(Delta.x * Coeff);
  CtrlDisp.y := Round(Delta.y * Coeff);
  BeginUpdate;
  try
   with Points[FirstIndex + 1] do
    Points[FirstIndex + 1] := Classes.Point(x + CtrlDisp.x, y + CtrlDisp.y);
   with Points[FirstIndex + 2] do
    Points[FirstIndex + 2] := Classes.Point(x + CtrlDisp.x, y + CtrlDisp.y);
  finally
   EndUpdate;
  end;
 end else begin
  // Move line segment
  BeginUpdate;
  try
   with Points[FirstIndex] do
    Points[FirstIndex] := Classes.Point(x + Delta.x, y + Delta.y);
   with Points[NextIndex] do
    Points[NextIndex] := Classes.Point(x + Delta.x, y + Delta.y);
  finally
   EndUpdate;
  end;
 end;
 Result := true;
end;

procedure TFlexControl.Translate(const TranslateInfo: TTranslateInfo);
var i: integer;
begin
 Invalidate;
 // Translate all sub controls
 for i:=0 to FControls.Count-1 do
  TFlexControl(FControls[i]).Translate(TranslateInfo);
 // Do translate
 ControlTranslate(TranslateInfo);
end;

function TFlexControl.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
begin
 Result := Rect(RefreshX, RefreshY,
                RefreshX+FWidthProp.Value, RefreshY+FHeightProp.Value);
end;

function TFlexControl.ClientToOwner(const Point: TPoint): TPoint;
var Control: TFlexControl;
begin
 Result.X := Point.X;
 Result.Y := Point.Y;
 Control := Self;
 repeat
  inc(Result.X, Control.FLeftProp.Value);
  inc(Result.Y, Control.FTopProp.Value);
  Control := Control.Parent;
 until not Assigned(Control);
 FOwner.TransformPointIndirect(Result);
end;

function TFlexControl.OwnerToClient(const Point: TPoint): TPoint;
var Control: TFlexControl;
begin
 Result.X := Point.X;
 Result.Y := Point.Y;
 FOwner.UnTransformPoint(Result.X, Result.Y);
 Control := Self;
 repeat
  dec(Result.X, Control.FLeftProp.Value);
  dec(Result.Y, Control.FTopProp.Value);
  Control := Control.Parent;
 until not Assigned(Control);
end;

procedure TFlexControl.Invalidate;
var i: integer;
begin
 if Assigned(FOwner) and FOwner.InvalidateControl(Self) then
  for i:=0 to FControls.Count-1 do Controls[i].Invalidate;
end;

procedure TFlexControl.PaintAll(Canvas: TCanvas; PaintX, PaintY: integer);
var i: integer;
    R: TRect;
    PaintCanvas: TCanvas;
    AlphaBlend: boolean;
    UseMainAlpha: boolean;
    TranspValue: integer;
begin
 // Check control visible
 if not Visible or Assigned(FLayer) and not FLayer.Visible then exit;
 if not FNoPaintRectCheck then begin
  // Check control intersection with canvas PaintRect
  R := GetRefreshRect(PaintX, PaintY);
  FOwner.TransformRect(R);
  with FOwner.PaintRect do
  if // not IntersectRect(iRect, R, FOwner.PaintRect^) then exit;
    {(R.Left >= FOwner.PaintWidth) or (R.Top >= FOwner.PaintHeight) or
    (R.Right < 0) or (R.Bottom < 0) then exit; }
    (R.Left > Right) or (R.Top > Bottom) or
    (R.Right < Left) or (R.Bottom < Top) then exit;
 end;
 PaintCanvas := Canvas;
 AlphaBlend := false;
 UseMainAlpha := false;
 FPaintAlphaBuffer := Nil;
 try
  TranspValue := FTransparencyProp.Value;
  if Assigned(FLayer) and (FLayer.TransparencyProp.Value > 0) then
   TranspValue := FLayer.TransparencyProp.Value +
     (100 - FLayer.TransparencyProp.Value) * TranspValue div 100;
  if (FPaintAlphaBufferMode = amRequired) or
    ((FPaintAlphaBufferMode = amAuto) and (TranspValue > 0) and not
      FOwner.PaintForExport) then begin
   // Semi-transparent control
   if FOwner.AlphaBuffer.SourceDC <> 0 then
    // Default alpha buffer is busy. Create auxiliary
    FPaintAlphaBuffer := TAlphaBuffer.Create(FOwner.AlphaBuffer)
   else begin
    // Use main alpha buffer
    FPaintAlphaBuffer := FOwner.AlphaBuffer;
    UseMainAlpha := true;
   end;
   // Init alpha buffer
   if FPaintAlphaBuffer.BeginPaint(Canvas.Handle, FOwner.PaintRect) then begin
    PaintCanvas := FOwner.AlphaBuffer.Canvas;
    AlphaBlend := true;
   end;
  end;
  // Default painting
  if not FNonVisual then begin
   // Paint self
   R := Rect(PaintX, PaintY,
     PaintX + FWidthProp.Value, PaintY + FHeightProp.Value);
   FOwner.TransformRect(R);
   Paint(PaintCanvas, R);
  end;
  // Paint sub-controls
  for i:=0 to FControls.Count-1 do with Controls[i] do
   PaintAll(PaintCanvas, PaintX + FLeftProp.Value, PaintY + FTopProp.Value);
  if AlphaBlend then
   // End semi-transparent paint
   FPaintAlphaBuffer.EndPaint(TranspValue);
 finally
  if not UseMainAlpha then FPaintAlphaBuffer.Free;
 end;
end;

procedure TFlexControl.Paint(Canvas: TCanvas; var PaintRect: TRect);
begin
end;

function TFlexControl.GetDocRect: TRect;
var Control: TFlexControl;
begin
 Result := Rect(FLeftProp.Value, FTopProp.Value,
   FLeftProp.Value + FWidthProp.Value, FTopProp.Value + FHeightProp.Value);
 Control := FParent;
 while Assigned(Control) do begin
  OffsetRect(Result, Control.FLeftProp.Value, Control.FTopProp.Value);
  Control := Control.FParent;
 end;
end;

function TFlexControl.GetPaintRect: TRect;
begin
 Result := GetDocRect;
 FOwner.TransformRect(Result);
end;

procedure TFlexControl.SetDocRect(Value: TRect);
var Control: TFlexControl;
begin
 Control := FParent;
 while Assigned(Control) do begin
  OffsetRect(Value, -Control.FLeftProp.Value, -Control.FTopProp.Value);
  Control := Control.FParent;
 end;
 BeginUpdate;
 try
  FLeftProp.Value := Value.Left;
  FTopProp.Value := Value.Top;
  FWidthProp.Value := Value.Right - Value.Left;
  FHeightProp.Value := Value.Bottom - Value.Top;
 finally
  EndUpdate;
 end;
end;

function TFlexControl.GetParentScheme: TFlexCustomScheme;
var Control: TFlexControl;
begin
 Result := Nil;
 Control := Parent;
 while Assigned(Control) do begin
  if Control is TFlexCustomScheme then begin
   Result := TFlexCustomScheme(Control);
   break;
  end;
  Control := Control.Parent;
 end;
end;

procedure TFlexControl.StartResizing(const SelRect: TRect);
var i: integer;
begin
 if fsResizing in FState then exit;
 Include(FState, fsResizing);
 FResizingRect := DocRect;
 FResizingTopLeft := SelRect.TopLeft;
 OffsetRect(FResizingRect, -SelRect.Left, -SelRect.Top);
 if Assigned(FLinkPoints) then begin
  SetLength(FResizingLinkPoints, FLinkPoints.Count);
  for i:=0 to FLinkPoints.Count-1 do with PPoint(FLinkPoints[i])^ do begin
   FResizingLinkPoints[i].X := X - SelRect.Left;
   FResizingLinkPoints[i].Y := Y - SelRect.Top;
  end;
 end;
 for i:=0 to FControls.Count-1 do Controls[i].StartResizing(SelRect);
end;

procedure TFlexControl.FinishResizing;
var i: integer;
begin
 if not (fsResizing in FState) then exit;
 SetLength(FResizingLinkPoints, 0);
 for i:=0 to FControls.Count-1 do Controls[i].FinishResizing;
 Exclude(FState, fsResizing);
end;

function TFlexControl.GetVisual: boolean;
begin
 Result := not FNonVisual;
end;

function TFlexControl.GetControl(Index: integer): TFlexControl;
begin
 Result := TFlexControl(FControls[Index]);
end;

function TFlexControl.GetByName(const Name: string): TFlexControl;
var i: integer;
begin
 Result := Nil;
 for i:=0 to FControls.Count-1 do
  if CompareStr(Name, TFlexControl(FControls[i]).Name) = 0 then begin
   Result := TFlexControl(FControls[i]);
   break;
  end;
end;

function TFlexControl.FindByID(ControlID: LongWord): TFlexControl;
var PassRec: TPassControlRec;
    Control: TFlexControl;
begin
 Result := Nil;
 if ControlID = 0 then exit;
 Control := Self;
 FirstControl(Control, PassRec);
 try
  while Assigned(Control) do begin
   if Control.ID = ControlID then begin
    Result := Control;
    break;
   end;
   Control := NextControl(PassRec);
  end;
 finally
  ClosePassRec(PassRec);
 end;
end;

function TFlexControl.FindByName(const AName: string): TFlexControl;
var PassRec: TPassControlRec;
    Control: TFlexControl;
begin
 Result := Nil;
 if AName = '' then exit;
 Control := Self;
 FirstControl(Control, PassRec);
 try
  while Assigned(Control) do begin
   if CompareStr(AName, Control.Name) = 0 then begin
    Result := Control;
    break;
   end;
   Control := NextControl(PassRec);
  end;
 finally
  ClosePassRec(PassRec);
 end;
end;

function TFlexControl.GetCount: integer;
begin
 Result := FControls.Count;
end;

function TFlexControl.GetID: LongWord;
begin
 Result := FIdProp.Value;
end;

procedure TFlexControl.SetID(Value: LongWord);
begin
 FIdProp.Value := Value;
end;

function TFlexControl.GetName: string;
begin
 Result := FNameProp.Value;
end;

procedure TFlexControl.SetName(const Value: string);
begin
 FNameProp.Value := Value;
end;

function TFlexControl.GetLeft: integer;
begin
 Result := FLeftProp.Value;
end;

function TFlexControl.GetTop: integer;
begin
 Result := FTopProp.Value;
end;

function TFlexControl.GetHeight: integer;
begin
 Result := FHeightProp.Value;
end;

function TFlexControl.GetWidth: integer;
begin
 Result := FWidthProp.Value;
end;

procedure TFlexControl.SetLeft(Value: integer);
begin
 FLeftProp.Value := Value;
end;

procedure TFlexControl.SetTop(Value: integer);
begin
 FTopProp.Value := Value;
end;

procedure TFlexControl.SetHeight(Value: integer);
begin
 FHeightProp.Value := Value;
end;

procedure TFlexControl.SetWidth(Value: integer);
begin
 FWidthProp.Value := Value;
end;

procedure TFlexControl.SetVisible(Value: boolean);
begin
 FVisibleProp.Value := Value;
end;

function TFlexControl.GetTransparency: integer;
begin
 Result := FTransparencyProp.Value;
end;

procedure TFlexControl.SetTransparency(const Value: integer);
begin
 FTransparencyProp.Value := Value;
end;

procedure TFlexControl.GetLayerStrProp(Sender: TObject; out Value: string);
begin
 if Assigned(FLayer)
  then Value := FLayer.Name
  else Value := '';
end;

procedure TFlexControl.SetLayer(Value: TFlexLayer);
var i: integer;
begin
 if (Value = FLayer) or not Assigned(FOwner) or
    (Assigned(Value) and (Value.Owner <> FOwner)) or
    Props.IsReadOnly(FLayerProp) then exit;
 {FLayerProp.FDirectChange := true;
 try}
  FLayerProp.DoBeforeChanged;
  {if Assigned(Value)
   then FLayerProp.SavedValue := Value.NameProp.Value
   else FLayerProp.SavedValue := '';}
  FLayer := Value;
  FLayerProp.DoChanged;
  if not IsSelectable then FOwner.Unselect(Self);
 {finally
  FLayerProp.FDirectChange := false;
 end;}
 for i:=0 to FControls.Count-1 do Controls[i].Layer := FLayer;
 Invalidate;
end;

procedure TFlexControl.SetOwner(Value: TFlexPanel);
var i: integer;
    SrcAction: THistoryAction;
    DstAction: THistoryAction;
begin
 if Value = FOwner then exit;
 Invalidate;
 SrcAction := Nil;
 DstAction := Nil;
 try
  if Assigned(FOwner) then
   SrcAction := FOwner.History.BeginAction(TControlDestroyHistoryGroup, Self);
  if Assigned(Parent) then begin
   if Parent.Owner <> Value then Parent.Extract(Self);
  end else
  if Assigned(FOwner) then 
   FOwner.RemoveWithoutParent(Self);
  if Assigned(FOwner) then begin
   FOwner.FIdPool.Release(FIdProp.Value);
   FProps.History := Nil;
  end;
  FOwner := Value;
  if Assigned(FOwner) then begin
   if not FOwner.FIdPool.Use(FIdProp.Value) then FOwner.GenerateID(Self);
   FProps.History := FOwner.History;
   if not Assigned(Parent) then FOwner.AddWithoutParent(Self);
   DstAction := FOwner.History.BeginAction(TControlCreateHistoryGroup, Self);
  end;
  for i:=0 to FControls.Count-1 do Controls[i].Owner := FOwner;
  Invalidate;
 finally
  if Assigned(SrcAction) then SrcAction.Owner.EndAction;
  if Assigned(DstAction) then DstAction.Owner.EndAction;
 end;
end;

function TFlexControl.GetHint: string;
begin
 Result := FHintProp.Text;
end;

function TFlexControl.GetShowHint: boolean;
begin
 Result := FShowHintProp.Value;
end;

function TFlexControl.GetTag: integer;
begin
 Result := FTagProp.Value;
end;

function TFlexControl.GetVisible: boolean;
begin
 Result := FVisibleProp.Value;
end;

procedure TFlexControl.SetHint(const Value: string);
begin
 FHintProp.Text := Value;
end;

procedure TFlexControl.SetShowHint(Value: boolean);
begin
 FShowHintProp.Value := Value;
end;

procedure TFlexControl.SetTag(Value: integer);
begin
 FTagProp.Value := Value;
end;

function TFlexControl.GetIsSelected: boolean;
begin
 if Assigned(FOwner)
  then Result := FOwner.IsSelected(Self)
  else Result := False;
end;

procedure TFlexControl.SetIsSelected(Value: boolean);
begin
 if Value
  then FOwner.Select(Self)
  else FOwner.UnSelect(Self);
end;

function TFlexControl.GetIsSelectable: boolean;
var Control: TFlexControl;
begin
 Result := FSelectableProp.Value;
 if Result and Assigned(FLayer) then Result := FLayer.SelectableProp.Value;
 if not Result then exit;
 Control := Parent;
 while Assigned(Control) do begin
  Result := Control.SelectableProp.Value;
  if not Result then break;
  Control := Control.Parent;
 end;
end;

function TFlexControl.GetIsUngroupable: boolean;
begin
 Result := false;
end;

function TFlexControl.GetRef: TFlexCustomScheme;
begin
 Result := TFlexCustomScheme(FReferenceProp.Value);
end;

procedure TFlexControl.SetRef(Value: TFlexCustomScheme);
begin
 FReferenceProp.Value := Value;
end;

function TFlexControl.GetSelectable: boolean;
begin
 Result := FSelectableProp.Value;
end;

procedure TFlexControl.SetSelectable(const Value: boolean);
begin
 FSelectableProp.Value := Value;
end;

procedure TFlexControl.PropBeforeChanged(Sender: TObject; Prop: TCustomProp);
begin
 if (Prop = FLeftProp) or (Prop = FTopProp) or
    (Prop = FWidthProp) or (Prop = FHeightProp) then begin
  if FUpdateCounter = 0 then begin
   FSavedDocRect := DocRect;
   if Assigned(FLinkPoints) and (FLinkPoints.Count > 0) then
    FLinkSavedBounds := FSavedDocRect;
  { FLinkSavedBounds.Left   := FLeftProp.Value;
   FLinkSavedBounds.Top    := FTopProp.Value;
   FLinkSavedBounds.Right  := FWidthProp.Value;
   FLinkSavedBounds.Bottom := FHeightProp.Value; }
  end;
 end else
 if Prop = FIdProp then begin
  FCheckID := FIdProp.Value;
  //FOwner.FIdPool.Release(FCheckID);
 end;
 if not (psNonVisual in Prop.Style) then Invalidate;
 if Assigned(FOwner) then FOwner.PropBeforeChanged(Self, Prop);
 if Assigned(FNotifyLink) then FNotifyLink.PropNotify(Prop, True);
end;

procedure TFlexControl.PropChanged(Sender: TObject; Prop: TCustomProp);
//var NewLayer: TFlexLayer;
begin
 if (Prop = FLeftProp) or (Prop = FTopProp) or
    (Prop = FWidthProp) or (Prop = FHeightProp) then begin
  // Check min values
  if (Prop = FWidthProp) and (FWidthProp.Value <= 0) then
   FWidthProp.Value := 1
  else
  if (Prop = FHeightProp) and (FHeightProp.Value <= 0) then
   FHeightProp.Value := 1
  else begin

   DoNotify(fnRect);
   //Invalidate;
  end;
 end else
 if (Prop = FNameProp) then
  DoNotify(fnName)
 else
 if (Prop = FIdProp) and (FCheckID <> FIdProp.Value) and Assigned(FOwner) then begin
  if FOwner.ValidateID(Self, FCheckID) then DoNotify(fnID);
 end else (*
 if Prop = FLayerProp then begin
 { NewLayer := Nil;
  if Assigned(FOwner) then begin
   if not FOwner.LoadOnActiveScheme then
    NewScheme := FOwner.SchemesByName[FSchemeProp.SavedValue];
   if not Assigned(NewScheme) then NewScheme := FOwner.ActiveScheme;
  end; }
  if not FLayerProp.DirectChange then begin
   // Check changed layer name
   NewLayer := TFlexLayer(FOwner.Layers.ByName[FLayerProp.SavedValue]);
   if not Assigned(NewLayer) and (FLayerProp.SavedValue <> '') and
      Assigned(ParentScheme) then
    NewLayer := Owner.ActiveLayer;
   Layer := NewLayer;
  end else
   NewLayer := Nil;
  // Check selectable
  if (NewLayer <> Layer) and Assigned(FOwner) and
    FOwner.IsSelected(Self) and not IsSelectable then FOwner.Unselect(Self);
 end else *)
 if (Prop = FSelectableProp) and not FSelectableProp.Value and
    Assigned(FOwner) and FOwner.IsSelected(Self) then begin
  FOwner.Unselect(Self);
 end;
 if not (psNonVisual in Prop.Style) then Invalidate;
 if Assigned(FOwner) then FOwner.PropChanged(Self, Prop);
 if Assigned(FNotifyLink) then FNotifyLink.PropNotify(Prop, False); 
end;

procedure TFlexControl.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = TagProp then
  IsStored := TagProp.Value <> 0
 else
 if Prop = ReferenceProp then
  IsStored := Assigned(ReferenceProp.Value)
 else
 if Prop = HintProp then
  IsStored := HintProp.LinesCount > 0
 else
 if Prop = ShowHintProp then
  IsStored := not ShowHintProp.Value
 else
 if Prop = UserData then
  IsStored := UserData.LinesCount > 0
 else
 if Prop = TransparencyProp then
  IsStored := TransparencyProp.Value > 0
 else
 if Prop = SelectableProp then
  IsStored := not SelectableProp.Value 
 else
 if (Prop.PropType = ptComplex) and (PropName <> '') then
  IsStored := Prop.IsComplexStored(PropName);
end;

procedure TFlexControl.PropReadOnly(Sender: TObject; Prop: TCustomProp;
  var IsReadOnly: boolean);
begin
 IsReadOnly := false;
 if Assigned(FLayer) then begin
  IsReadOnly := FLayer.ReadOnly;
  if not IsReadOnly then
   if not FLayer.MoveableProp.Value then
    IsReadOnly := (Prop = FLeftProp) or (Prop = FTopProp)
   else
   if not FLayer.ResizableProp.Value then
    IsReadOnly := (Prop = FWidthProp) or (Prop = FHeightProp);
 end;
end;

procedure TFlexControl.PropHistoryAction(Sender: TObject; Prop: TCustomProp;
  var ActionClass: THistoryActionClass);
begin
 // Abstract
end;

function TFlexControl.IsPointInside(PaintX, PaintY: integer): boolean;
begin
 with PaintRect do
  Result := (PaintX >= Left) and (PaintY >= Top) and
            (PaintX < Right) and (PaintY < Bottom);
end;

procedure TFlexControl.SaveToFiler(Filer: TFlexFiler; const Indent: string);
var i: integer;
begin
 if Filer.Binary then
  Filer.BinaryData.WriteSimpleCommand(fbcObject, 2, NameProp.Value, ClassName)
 else
  Filer.SaveStr(Indent + fcObject + ' ' + Name + ': '+ClassName);
 FProps.SaveToFiler(Filer, Indent + IndentStep);
 for i:=0 to FControls.Count-1 do
  Controls[i].SaveToFiler(Filer, Indent + IndentStep);
 if Filer.Binary
  then Filer.BinaryData.WriteSimpleCommand(fbcEnd)
  else Filer.SaveStr(Indent + fcEnd);
 Filer.Saved := Filer.Saved + 1;
end;

procedure TFlexControl.LoadFromFiler(Filer: TFlexFiler);
var s: string;
begin
 FOwner.BeginLoading;
 try
  Include(FState, fsLoading);
  repeat
   if Filer.Binary then begin
    if not Filer.BinaryData.ReadCommand then break;
    case Filer.BinaryData.ReadCmdCommand of
     fbcEnd    : break;
     fbcObject : FOwner.LoadFlexControl(Filer, Self, '');
     else        Props.LoadFromFiler(Filer, '', Owner.PropRefList);
    end;
   end else begin
    if not Filer.LoadStrCheck(s) then break;
    if StrBeginsFrom(s, fcEnd) then
     break
    else
    if StrBeginsFrom(s, fcObject) then
     FOwner.LoadFlexControl(Filer, Self, s)
    else
     Props.LoadFromFiler(Filer, s, Owner.PropRefList);
   end;
  until false;
 finally
  Exclude(FState, fsLoading);
  FOwner.EndLoading;
 end;
 DoNotify(fnLoaded);
end;

// TFlexGroup ////////////////////////////////////////////////////////////////

procedure TFlexGroup.ControlCreate;
begin
 FNonVisual := True;
 inherited;
 Visible := True;
end;

procedure TFlexGroup.ControlTranslate(const TranslateInfo: TTranslateInfo);
var Range: TRect;
    i: integer;
begin
 for i:=0 to FControls.Count-1 do with TFlexControl(FControls[i]) do
  if i = 0
   then Range := Rect(LeftProp.Value, TopProp.Value,
                      LeftProp.Value + WidthProp.Value,
                      TopProp.Value + HeightProp.Value)
   else UnionRect(Range, Range,
          Rect(LeftProp.Value, TopProp.Value,
               LeftProp.Value + WidthProp.Value,
               TopProp.Value + HeightProp.Value) );
 for i:=0 to FControls.Count-1 do with TFlexControl(FControls[i]) do begin
  BeginUpdate;
  try
   LeftProp.Value := LeftProp.Value - Range.Left;
   TopProp.Value := TopProp.Value - Range.Top;
  finally
   EndUpdate;
  end;
 end;
 with DocRect do OffsetRect(Range, Left, Top);
 BeginTranslate;
 try
  // Group control translation
  DocRect := Range;
  // Translate link points
  LinkPointsTranslate(TranslateInfo);
 finally
  EndTranslate;
 end;
end;

procedure TFlexGroup.Translate(const TranslateInfo: TTranslateInfo);
var IsRectChanged: boolean;
    SavedDocRect: TRect;
begin
 if Owner.History.InProcessSource = Self then exit;
 if TranslateInfo.Mirror and (psReadOnly in WidthProp.Style) then exit;
 if (TranslateInfo.Rotate = 180) and (psReadOnly in HeightProp.Style) then exit;
 BeginUpdate;
 try
  SavedDocRect := FSavedDocRect;
  inherited;
 finally
  IsRectChanged := FRectChanged;
  FRectChanged := false;
  EndUpdate;
 end;
 if IsRectChanged then begin
  //DoNotify(fnRect);
  BeginUpdate;
  FSavedDocRect := SavedDocRect;
  FRectChanged := true;
  EndUpdate;
 end;
end;

procedure TFlexGroup.PropBeforeChanged(Sender: TObject; Prop: TCustomProp);
begin
 if Owner.History.InProcessSource <> Self then begin
  if ((Prop = FLeftProp) or (Prop = FTopProp)) and not FGroupMove then begin
   FOldDocRect := DocRect;
  end else
  if ((Prop = FWidthProp) or (Prop = FHeightProp)) and
     not (fsResizing in FState) and not FBoundsChanging then begin
   FGroupResize := true;
   StartResizing(DocRect);
  end;
 end;
 inherited;
end;

procedure TFlexGroup.PropChanged(Sender: TObject; Prop: TCustomProp);
var PassRec: TPassControlRec;
    Control: TFlexControl;
    CoeffX, CoeffY: double;
    R: TRect;
    GroupDocRect: TRect;
begin
 if Owner.History.InProcessSource <> Self then begin
  if ((Prop = FLeftProp) or (Prop = FTopProp)) and not FGroupMove and
     (not Assigned(Parent) or (Parent.FGroupUngroupLevel = 0)) then begin
   // Send notify from all controls containing link point(s)
   FGroupMove := true;
   try
    GroupDocRect := DocRect;
    Control := Self;
    FirstControl(Control, PassRec);
    Control := NextControl(PassRec); // skip self
    while Assigned(Control) do begin
     if Assigned(Control.FLinkPoints) and
        (Control.FLinkPoints.Count > 0) then begin
      Control.BeginUpdate;
      try
       OffsetRect(Control.FLinkSavedBounds,
         FOldDocRect.Left - GroupDocRect.Left,
         FOldDocRect.Top - GroupDocRect.Top);
       Control.DoNotify(fnRect);
      finally
       Control.EndUpdate;
      end;
     end;
     Control := NextControl(PassRec);
    end;
   finally
    FGroupMove := false;
   end;
  end else
  if ((Prop = FWidthProp) or (Prop = FHeightProp)) and FGroupResize then begin
   if (FControls.Count > 0) and not IsRectEmpty(FResizingRect) then begin
    // Calc resize coeff's
    CoeffX := FWidthProp.Value / RectWidth(FResizingRect);
    CoeffY := FHeightProp.Value / RectHeight(FResizingRect);
    // Resize sub-controls
    Control := Self;
    FirstControl(Control, PassRec);
    Control := NextControl(PassRec); // skip self
    while Assigned(Control) do begin
     // Resize
     with Control.FResizingRect, Control.FResizingTopLeft do begin
      R.Left := Round(Left * CoeffX);
      R.Top := Round(Top * CoeffY);
      R.Right := Round(Right * CoeffX);
      R.Bottom := Round(Bottom * CoeffY);
      OffsetRect(R, X, Y);
     end;
     Control.DocRect := R;
     // Next sub-control
     Control := NextControl(PassRec);
    end;
   end;
   FGroupResize := false;
   FinishResizing;
   //if Assigned(FParent) and (FParent is TFlexGroup) then
   // TFlexGroup(FParent).RefreshBounds;
  end;
 end;
 inherited;
end;

function TFlexGroup.EndGroupUngroup: boolean;
begin
 Result := inherited EndGroupUngroup;
 if not Result then exit;
 if FNeedRefreshBounds then begin
  FNeedRefreshBounds := False;
  RefreshBounds;
 end;
end;

procedure TFlexGroup.RefreshBounds;
var i: integer;
    Bounds, R: TRect;
    InProcessSource: TObject;
begin
 if FBoundsChanging or FGroupMove then exit;
 if FGroupUngroupLevel > 0 then begin
  FNeedRefreshBounds := true;
  exit;
 end;
 FBoundsChanging := true;
 try
  // Calc union of sub-controls bounds
  InProcessSource := Owner.History.InProcessSource;
  for i:=0 to FControls.Count-1 do
   if Self.FControls[i] = InProcessSource then
    // Do not change bounds in redo/undo phase
    exit
   else
   with TFlexControl(FControls[i]) do begin
    R.Left := LeftProp.Value;
    R.Top := TopProp.Value;
    R.Right := R.Left + WidthProp.Value;
    R.Bottom := R.Top + HeightProp.Value;
    if i=0 then
     Bounds := R
    else begin
     if R.Right = R.Left then inc(R.Right);
     if R.Bottom = R.Top then inc(R.Bottom);
     UnionRect(Bounds, Bounds, R);
    end;
   end;
  R := DocRect;
  // Correct pos of sub-controls
  if (Bounds.Left <> 0) or (Bounds.Top <> 0) then begin
   for i:=0 to FControls.Count-1 do with TFlexControl(FControls[i]) do begin
    BeginUpdate;
    try
     LeftProp.Value := LeftProp.Value - Bounds.Left;
     TopProp.Value := TopProp.Value - Bounds.Top;
    finally
     EndUpdate;
    end;
   end;
   OffsetRect(R, Bounds.Left, Bounds.Top);
   OffsetRect(Bounds, -Bounds.Left, -Bounds.Top);
  end;
  // Correct size
  R.Right := R.Left + Bounds.Right;
  R.Bottom := R.Top + Bounds.Bottom;
  DocRect := R;
 finally
  FBoundsChanging := false;
 end;
end;

function TFlexGroup.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
var i: integer;
    R: TRect;
begin
 Result := inherited GetRefreshRect(RefreshX, RefreshY);
 for i:=0 to FControls.Count-1 do with TFlexControl(FControls[i]) do begin
  R := GetRefreshRect(RefreshX + FLeftProp.Value, RefreshY + FTopProp.Value);
  UnionRect(Result, Result, R);
 end;
end;

function TFlexGroup.IsPointInside(PaintX, PaintY: integer): boolean;
var i: integer;
begin
 Result := false;
 for i:=FControls.Count-1 downto 0 do with Controls[i] do
  if IsPointInside(PaintX, PaintY) then begin
   Result := true;
   break;
  end;
end;

function TFlexGroup.GetIsUngroupable: boolean; 
begin
 Result := true;
end;

// TFlexClone /////////////////////////////////////////////////////////////////

procedure TFlexClone.ControlCreate;
begin
 Width := 1;
 Height := 1;
 AutoSizeProp.Value := true;
 inherited;
 Visible := True;
end;

procedure TFlexClone.ControlDestroy;
begin
 FSourceProp.LinkedControl := Nil;
 inherited;
end;

procedure TFlexClone.CreateProperties;
begin
 inherited;
 FSourceProp := TLinkProp.Create(FProps, 'Source');
 FSourceProp.OnLinkedNotify := SourceLinkNotify;
 FAutoSizeProp := TBoolProp.Create(FProps, 'AutoSize');
end;

procedure TFlexClone.SetSizeLock(Value: boolean);
begin
 if not FAutoSizeProp.Value then Value := false;
 if Value = FSizeLock then exit;
 FSizeLock := Value;
 if FSizeLock then begin
  FWidthProp.Style := FWidthProp.Style   + [ psReadOnly ];
  FHeightProp.Style := FHeightProp.Style + [ psReadOnly ];
 end else begin
  FWidthProp.Style := FWidthProp.Style   - [ psReadOnly ];
  FHeightProp.Style := FHeightProp.Style - [ psReadOnly ];
 end;
end;

function TFlexClone.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
var CoeffX, CoeffY: double;
    LinkedRect: TRect;
begin
 Result := inherited GetRefreshRect(RefreshX, RefreshY);
 if not Assigned(FSourceProp.LinkedControl) then exit;
 with FSourceProp.LinkedControl, LinkedRect do begin
  LinkedRect := GetRefreshRect(0, 0);
  dec(Right, WidthProp.Value);
  dec(Bottom, HeightProp.Value);
 end;
 if FAutoSizeProp.Value then begin
  inc(Result.Left, LinkedRect.Left);
  inc(Result.Top, LinkedRect.Top);
  inc(Result.Right, LinkedRect.Right);
  inc(Result.Bottom, LinkedRect.Bottom);
 end else begin
  CoeffX := WidthProp.Value / FSourceProp.LinkedControl.WidthProp.Value;
  CoeffY := HeightProp.Value / FSourceProp.LinkedControl.HeightProp.Value;
  inc(Result.Left, Round(LinkedRect.Left * CoeffX));
  inc(Result.Top, Round(LinkedRect.Top * CoeffY));
  inc(Result.Right, Round(LinkedRect.Right * CoeffX));
  inc(Result.Bottom, Round(LinkedRect.Bottom * CoeffY));
 end;
end;

procedure TFlexClone.Paint(Canvas: TCanvas; var PaintRect: TRect);
var DefBrush: TBrush;
    DefPen: TPen;
    Control: TFlexControl;
    ControlRect: TRect;
    ControlOrigin: TPoint;
    Metafile: TMetafile;
    MetaCanvas: TMetafileCanvas;
begin
 Control := FSourceProp.LinkedControl;
 if Assigned(Control) then begin
  Metafile := Nil;
  MetaCanvas := Nil;
  try
   if not FAutoSizeProp.Value then with PaintRect do begin
    ControlRect := Control.PaintRect;
    if ((Right - Left - ControlRect.Right + ControlRect.Left) <> 0) or
       ((Bottom - Top - ControlRect.Bottom + ControlRect.Top) <> 0) then
     Metafile := OpenMetafile(ControlRect);
   end;
   if Assigned(Metafile) then begin
    // Paint on metafile
    with Control.DocRect do begin
     ControlOrigin.X := ScaleValue(Left, Owner.Scale);
     ControlOrigin.Y := ScaleValue(Top, Owner.Scale);
    end;
    ControlRect := Rect(0, 0, Metafile.Width, Metafile.Height);
    MetaCanvas := TMetafileCanvas.Create(Metafile, Canvas.Handle);
    Control.Owner.PaintTo(MetaCanvas, ControlRect, ControlOrigin,
      Control.Owner.Scale, Control, false, false, false, true);
    MetaCanvas.Free;
    MetaCanvas := Nil;
    // Draw metafile on canvas
    Canvas.StretchDraw(PaintRect, Metafile);
   end else
   // Paint without stretching
   if Control.Owner <> FOwner then begin
    with Control.DocRect do begin
     ControlOrigin.X := ScaleValue(Left, Owner.Scale);
     ControlOrigin.Y := ScaleValue(Top, Owner.Scale);
    end;
    Control.Owner.PaintTo(Canvas, PaintRect, ControlOrigin,
      Owner.Scale, Control, false, false, false, true)
   end else
    with DocRect do Control.PaintAll(Canvas, Left, Top);
  finally
   MetaCanvas.Free;
   CloseMetafile(Metafile);
  end;
 end else
 if not Owner.PaintForExport and Owner.InDesign then
 with Canvas do begin
  DefBrush := Nil;
  DefPen := Nil;
  try
   DefBrush := TBrush.Create;
   DefPen := TPen.Create;
   Brush.Assign(DefBrush);
   Pen.Assign(DefPen);
  finally
   DefBrush.Free;
   DefPen.Free;
  end;
  Brush.Style := bsClear;
  Pen.Style := psDash;
  with PaintRect do Rectangle(Left, Top, Right, Bottom);
 end;
end;

procedure TFlexClone.SetSizeFromLinkedControl;
begin
 if not Assigned(FSourceProp.LinkedControl) then exit;
 SizeLock := false;
 try
  BeginUpdate;
  try
   FWidthProp.Value := FSourceProp.LinkedControl.WidthProp.Value;
   FHeightProp.Value := FSourceProp.LinkedControl.HeightProp.Value;
  finally
   EndUpdate;
  end;
 finally
  SizeLock := true;
 end;
end;

procedure TFlexClone.SourceLinkNotify(Sender: TObject; Source: TNotifyLink;
  const Info: TNotifyLinkInfo);
var Control: TFlexControl;
begin
 Control := FSourceProp.LinkedControl;
 case Info.Code of
{  ncControlNotify:
    if (Info.ControlNotify = fnRect) and
       FAutoSizeProp.Value then SetSizeFromLinkedControl; }
  ncPropChanged:
    if ((Info.Prop = Control.WidthProp) or
        (Info.Prop = Control.HeightProp)) and FAutoSizeProp.Value then
     SetSizeFromLinkedControl
    else
    if not (psNonVisual in TCustomProp(Info.Prop).Style) then Invalidate;
 end;
end;

procedure TFlexClone.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 if Prop = FSourceProp then begin
  if Assigned(FSourceProp.LinkedControl) then begin
   // Check Linked Control
   if FSourceProp.LinkedControl is TFlexClone then
    // Can't clone another clone
    FSourceProp.LinkedControl := Nil
   else
   if FAutoSizeProp.Value then
    SetSizeFromLinkedControl;
  end else
   SizeLock := false;
 end else
 if Prop = FAutoSizeProp then begin
  if FAutoSizeProp.Value
   then SetSizeFromLinkedControl
   else SizeLock := false;
 end;
 inherited;
end;

procedure TFlexClone.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = FSourceProp then
  IsStored := Assigned(FSourceProp.LinkedControl)
 else
 if Prop = FAutoSizeProp then
  IsStored := not FAutoSizeProp.Value
 else
  inherited;
end;

function TFlexClone.OpenMetafile(const Bounds: TRect): TMetafile;
begin
 Result := TMetafile.Create;
 Result.Width := Bounds.Right - Bounds.Left;
 Result.Height := Bounds.Bottom - Bounds.Top;
end;

procedure TFlexClone.CloseMetafile(Metafile: TMetafile);
begin
 Metafile.Free;
end;

// TFlexLayer ////////////////////////////////////////////////////////////////

procedure TFlexLayer.ControlCreate;
begin
 FSelectableProp.Value := True;
 FMoveableProp.Value := True;
 FResizableProp.Value := True;
 if Assigned(Owner) then Name := Owner.GetDefaultNewName(Self, Nil, True); 
 inherited;
 Visible := true;
end;

procedure TFlexLayer.CreateProperties;
const HideSet: TPropStyle = [ psReadOnly, psDontStore, psNonVisual ];
begin
 inherited;
 FMoveableProp := TBoolProp.Create(FProps, 'Moveable');
 FMoveableProp.Style := [ psVisible, psNonVisual ];
 FResizableProp := TBoolProp.Create(FProps, 'Resizable');
 FResizableProp.Style := [ psVisible, psNonVisual ];
 {FSelectableProp := TBoolProp.Create(FProps, 'Selectable');
 FSelectableProp.Style := [ psVisible, psNonVisual ];}
 FReadOnlyProp := TBoolProp.Create(FProps, 'ReadOnly');
 FReadOnlyProp.Style := [ psVisible, psNonVisual ];
 FLeftProp.Style := HideSet;
 FTopProp.Style := HideSet;
 FWidthProp.Style := HideSet;
 FHeightProp.Style := HideSet;
 FLayerProp.Style := HideSet;
 FVisibleProp.Style := [ psVisible ];
 FLayerProp.Style := HideSet;
 FReferenceProp.Style := HideSet;
 //FTransparencyProp.Style := HideSet;
end;

procedure TFlexLayer.ControlDestroy;
var i,j: integer;
begin
 if Assigned(FOwner.Schemes) then 
 for i:=0 to FOwner.Schemes.Count-1 do begin
  j := FOwner.Schemes[i].Count-1;
  while j >= 0 do with FOwner.Schemes[i][j] do begin
   if Layer = Self then Free;
   dec(j);
  end;
 end;
 inherited;
end;

{function TFlexLayer.GetSelectable: boolean;
begin
 Result := FSelectableProp.Value;
end;

procedure TFlexLayer.SetSelectable(Value: boolean);
begin
 FSelectableProp.Value := Value;
end;}

function TFlexLayer.GetReadOnly: boolean;
begin
 Result := FReadOnlyProp.Value;
end;

procedure TFlexLayer.SetReadOnly(Value: boolean);
begin
 FReadOnlyProp.Value := Value;
end;

function TFlexLayer.GetMoveable: boolean;
begin
 Result := FMoveableProp.Value;
end;

procedure TFlexLayer.SetMoveable(const Value: boolean);
begin
 FMoveableProp.Value := Value;
end;

function TFlexLayer.GetResizable: boolean;
begin
 Result := FResizableProp.Value;
end;

procedure TFlexLayer.SetResizable(const Value: boolean);
begin
 FResizableProp.Value := Value;
end;

procedure TFlexLayer.Invalidate;
begin
 if not Assigned(FOwner.Parent) or (FUpdateCounter > 0) then exit;
 FOwner.Invalidate;
end;

procedure TFlexLayer.PropBeforeChanged(Sender: TObject; Prop: TCustomProp);
begin
 if (Prop = FNameProp) and (FCheckName = '') then FCheckName := FNameProp.Value;
 inherited;
end;

procedure TFlexLayer.PropChanged(Sender: TObject; Prop: TCustomProp);
var i: integer;
begin
 if Prop = FNameProp then begin
  if not ValidateName(True, True) then
   if FNameProp.Value <> FCheckName
    then FNameProp.Value := FCheckName
    else FNameProp.Value := '';
  FCheckName := '';
 end else
 if Prop = FSelectableProp then begin
  if not FSelectableProp.Value then
   for i:=FOwner.SelectedCount-1 downto 0 do
    if FOwner.Selected[i].Layer = Self then FOwner.Unselect(FOwner.Selected[i]);
 end;
 inherited;
end;

procedure TFlexLayer.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = MoveableProp then
  IsStored := not MoveableProp.Value
 else
 if Prop = ResizableProp then
  IsStored := not ResizableProp.Value
 else
 if Prop = SelectableProp then
  IsStored := not SelectableProp.Value
 else
 if Prop = ReadOnlyProp then
  IsStored := ReadOnlyProp.Value
 else
  inherited;
end;


// TFlexLayers ///////////////////////////////////////////////////////////////

procedure TFlexLayers.ControlCreate;
begin
 FNonVisual := true;
 Visible := false;
 Name := 'Layers';
end;

function TFlexLayers.Add(AControl: TFlexControl): integer;
begin
 if AControl is TFlexLayer then begin
  Result := inherited Add(AControl);
  if Result >=0 then begin
   DoNotify(fnLayers);
   if not Assigned(FOwner.ActiveLayer) then
    FOwner.ActiveLayer := TFlexLayer(AControl);
  end;
 end else
  Result := -1;
end;

function TFlexLayers.New: TFlexLayer;
begin
 Result := TFlexLayer.Create(FOwner, Self, Nil);
end;

procedure TFlexLayers.Delete(Index: integer);
var Layer: TFlexLayer;
begin
 Layer := TFlexLayer(FControls[Index]);
 inherited;
 if FControls.IndexOf(Layer) >= 0 then exit; 
 if FOwner.ActiveLayer = Layer then begin
  if Index = FControls.Count then dec(Index);
  if Index >= 0
   then FOwner.ActiveLayer := Controls[Index]
   else FOwner.ActiveLayer := Nil;
 end;
 DoNotify(fnLayers);
end;

procedure TFlexLayers.ChangeOrder(CurIndex, NewIndex: integer);
begin
 inherited;
 DoNotify(fnLayers);
end;

function TFlexLayers.GetLayer(Index: integer): TFlexLayer;
begin
 Result := TFlexLayer(FControls[Index]);
end;

function TFlexLayers.GetByName(const Name: string): TFlexLayer;
begin
 Result := TFlexLayer( inherited ByName[Name] );
end;

// TFlexCustomScheme /////////////////////////////////////////////////////////

procedure TFlexCustomScheme.ControlCreate;
begin
 if Assigned(Parent) and (Parent.Count = 1) then Default := True;
 if Assigned(Owner) then Name := Owner.GetDefaultNewName(Self, Nil, True);
 FNoPaintRectCheck := true;
 inherited;
 FNonVisual := True;
 Visible := True;
end;

procedure TFlexCustomScheme.CreateProperties;
const HideSet: TPropStyle = [ psReadOnly, psDontStore, psNonVisual ];
begin
 inherited;
 FDefaultProp := TBoolProp.Create(FProps, 'Default');
 FDefaultProp.Style := [ psVisible, psNonVisual ];
 FLeftProp.Style := HideSet;
 FTopProp.Style := HideSet;
 FWidthProp.Style := HideSet;
 FWidthProp.OnGetValue := GetOwnerWidth;
 FHeightProp.Style := HideSet;
 FHeightProp.OnGetValue := GetOwnerHeight;
 FLayerProp.Style := HideSet;
 FTransparencyProp.Style := HideSet;
end;

procedure TFlexCustomScheme.ControlDestroy;
var i,j: integer;
begin
 if Assigned(FOwner.Schemes) then
  for i:=0 to FOwner.Schemes.Count-1 do 
   for j:=0 to FOwner.Schemes[i].Count-1 do with FOwner.Schemes[i][j] do
    if Reference = Self then Reference := Nil;
 inherited;
end;

function TFlexCustomScheme.CreatePaintOrder(var PaintOrder: TPaintOrder): boolean;
var i, LayerIdx: integer;
begin
 with PaintOrder do begin
  SetLength(LayerRefs, FOwner.Layers.Count);
  SetLength(ControlRefs, FControls.Count);
  for i:=0 to High(LayerRefs) do begin
   LayerRefs[i].First := -1;
   LayerRefs[i].Last := -1
  end;
  for i:=0 to FControls.Count-1 do begin
   LayerIdx := FOwner.Layers.IndexOf(TFlexControl(FControls[i]).Layer);
   if LayerIdx < 0 then continue;
   ControlRefs[i].Control := FControls[i];
   ControlRefs[i].Next := -1;
   ControlRefs[i].Prev := -1;
   if LayerRefs[LayerIdx].Last >= 0 then begin
    ControlRefs[LayerRefs[LayerIdx].Last].Next := i;
    ControlRefs[i].Prev := LayerRefs[LayerIdx].Last;
    LayerRefs[LayerIdx].Last := i;
   end else begin
    LayerRefs[LayerIdx].First := i;
    LayerRefs[LayerIdx].Last := i;
   end;
  end;
 end;
 Result := true;
end;

procedure TFlexCustomScheme.PropChanged(Sender: TObject; Prop: TCustomProp);
var i: integer;
    Scheme: TFlexCustomScheme;
    Present: boolean;
begin
 if Prop = FDefaultProp then 
  if FDefaultProp.Value then begin
   for i:=0 to FOwner.Schemes.Count-1 do begin
    Scheme := FOwner.Schemes[i];
    if (Scheme <> Self) and Scheme.Default then Scheme.Default := false;
   end;
   if not Assigned(FOwner.ActiveScheme) then
    FOwner.ActiveScheme := Self;
  end else begin
   Present := False;
   for i:=0 to FOwner.Schemes.Count-1 do begin
    Scheme := FOwner.Schemes[i];
    if (Scheme <> Self) and Scheme.Default then begin
     Present := True;
     break;
    end;
   end;
   if not Present then Default := true;
  end;
 inherited;
end;

procedure TFlexCustomScheme.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if Prop = FDefaultProp then
  IsStored := FDefaultProp.Value
 else
  inherited;
end;

procedure TFlexCustomScheme.PaintAll(Canvas: TCanvas; PaintX, PaintY: integer);
var PO: TPaintOrder;
    i, LayerIdx: integer;
    R{, PaintR}: TRect;
    Size: TPoint;
begin
 if not Visible then exit;
 if not FNonVisual and
   (not FOwner.PaintForExport or FOwner.PaintSchemeBackground) then begin
  R := Rect(PaintX, PaintY, PaintX + Width, PaintY + Height);
  FOwner.TransformRect(R);
  if FOwner.SchemeBkStretch and FOwner.FWMPaintProcessing then begin
   {PaintR := FOwner.PaintRect;
   if FOwner.FInPaintOrigin.X < 0 then begin
    R.Left := (FOwner.FInPaintOrigin.X - FOwner.FOrigin.X);
    R.Right := PaintR.Right;
   end;
   if FOwner.FInPaintOrigin.Y < 0 then begin
    R.Top := (FOwner.FInPaintOrigin.Y - FOwner.FOrigin.Y);
    R.Bottom := PaintR.Bottom;
   end;}
   Size.X := RectWidth(R);
   Size.Y := RectHeight(R);
   if Size.X < FOwner.ClientWidth then begin
    dec(R.Left, (FOwner.ClientWidth - Size.X) div 2);
    R.Right := R.Left + FOwner.ClientWidth;
   end;
   if Size.Y < FOwner.ClientHeight then begin
    dec(R.Top, (FOwner.ClientHeight - Size.Y) div 2);
    R.Bottom := R.Top + FOwner.ClientHeight;
   end;
  end;
  Paint(Canvas, R);
 end;
 if Assigned(FOwner.OnPaintScheme) then
  FOwner.OnPaintScheme(Canvas, Self, False, False);
 if not FOwner.PaintForExport then
  FOwner.FGridControl.PaintAll(Canvas, PaintX, PaintY);
 if (FControls.Count = 0) or (FOwner.Layers.Count = 0) then exit;
 InitPaintOrder(PO);
 try
  if CreatePaintOrder(PO) then with PO do
   for LayerIdx:=0 to High(LayerRefs) do begin
    i := LayerRefs[LayerIdx].First;
    while i >= 0 do begin
     with ControlRefs[i].Control do PaintAll(Canvas, PaintX + Left, PaintY + Top);
     i := ControlRefs[i].Next;
    end;
   end;
 finally
  ClearPaintOrder(PO);
 end;
end;

function TFlexCustomScheme.FindControlAtPoint(x, y: integer): TFlexControl;
var i, Index: integer;
    PO: TPaintOrder;
begin
 Result := Nil;
 InitPaintOrder(PO);
 try
  if not CreatePaintOrder(PO) or (Length(PO.LayerRefs) = 0) then exit;
  for i:=High(PO.LayerRefs) downto 0 do begin
   if not FOwner.Layers[i].Selectable then continue;
   Index := PO.LayerRefs[i].Last;
   while Index >= 0 do with PO.ControlRefs[Index].Control do begin
    if FVisibleProp.Value and IsPointInside(x, y) then begin
     Result := PO.ControlRefs[Index].Control;
     break;
    end;
    Index := PO.ControlRefs[Index].Prev;
   end;
   if Assigned(Result) then break;
  end;
 finally
  ClearPaintOrder(PO);
 end;
end;

procedure TFlexCustomScheme.GetOwnerHeight(Sender: TObject; out Value: integer);
begin
 Value := FOwner.DocHeight;
end;

procedure TFlexCustomScheme.GetOwnerWidth(Sender: TObject; out Value: integer);
begin
 Value := FOwner.DocWidth;
end;

function TFlexCustomScheme.GetDefault: boolean;
begin
 Result := FDefaultProp.Value;
end;

procedure TFlexCustomScheme.SetDefault(const Value: boolean);
begin
 FDefaultProp.Value := Value;
end;

// TFlexScheme ///////////////////////////////////////////////////////////////

procedure TFlexScheme.ControlCreate;
begin
 inherited;
 FBrushProp.Color := clWhite; 
 FNonVisual := False;
end;

procedure TFlexScheme.CreateProperties;
begin
 inherited;
 FBrushProp := TBrushProp.Create(FProps, 'Brush');
 FPictureProp := TPictureProp.Create(FProps, 'Picture');
 FBackgroundProp := TBackgroundOptionsProp.Create(FProps, 'Background');
end;

procedure TFlexScheme.Paint(Canvas: TCanvas; var PaintRect: TRect);
begin
 FBackgroundProp.Draw(Canvas, PaintRect, FPictureProp, FBrushProp,
   Owner.Scale, Owner.PaintRect, Owner.UseImageClipTransparent);
end;

function TFlexScheme.GetRefreshRect(RefreshX, RefreshY: integer): TRect;
begin
 if Owner.SchemeBkStretch then begin
  Result := Owner.ClientRect;
  Result.TopLeft := OwnerToClient(Result.TopLeft);
  Result.BottomRight := OwnerToClient(Result.BottomRight);
 end else
  Result := inherited GetRefreshRect(RefreshX, RefreshY);
end;

procedure TFlexScheme.PropBeforeChanged(Sender: TObject;
  Prop: TCustomProp);
begin
 if (Prop = FNameProp) and (FCheckName = '') then FCheckName := FNameProp.Value;
 inherited;
end;

procedure TFlexScheme.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 if Prop = FNameProp then begin
  if not ValidateName(True, True) then
   if FNameProp.Value <> FCheckName
    then FNameProp.Value := FCheckName
    else FNameProp.Value := '';
  FCheckName := '';
 end;
 inherited;
end;

procedure TFlexScheme.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if (Prop = FBrushProp) and (PropName = 'Color') then
  IsStored := FBrushProp.Color <> clWhite
 else
  inherited; 
end;

// TFlexSchemes //////////////////////////////////////////////////////////////

procedure TFlexSchemes.ControlCreate;
begin
 FNoPaintRectCheck := true;
 Name := 'Document';
 ID := 1;
 Visible := true;
 ShowHint := true;
 SelectableProp.Value := true;
 ConnectorsMinGapProp.Value := StdConnectorsMinGap;
 DoNotify(fnCreated);
end;

procedure TFlexSchemes.CreateProperties;
const HideSet: TPropStyle = [ psReadOnly, psDontStore, psNonVisual ];
begin
 inherited;
 FVersionProp := TIntProp.Create(Props, 'Version');
 FVersionProp.Value := FlexGraphicsVersion;
 FVersionProp.Style := [ psReadOnly, psNonVisual ];
 FLeftProp.Style := HideSet;
 FTopProp.Style := HideSet;
 FVisibleProp.Style := HideSet;
 FLayerProp.Style := HideSet;
 FReferenceProp.Style := HideSet;
 FTransparencyProp.Style := HideSet;
 FConnectorsMinGapProp := TIntProp.Create(Props, 'ConnectorsMinGap');
 FConnectorsMinGapProp.Style := FConnectorsMinGapProp.Style +
   [ psNonVisual, psScalable ];
 FConnectorsKeepLinkProp := TBoolProp.Create(Props, 'ConnectorLinkToPoint');
 FConnectorsKeepLinkProp.Style := FConnectorsKeepLinkProp.Style +
   [ psNonVisual ];
 FScriptProp := TStrListProp.Create(Props, 'Script');
 FScriptProp.Style := FScriptProp.Style + [psNonVisual];
end;

procedure TFlexSchemes.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 inherited;
 if (Prop = FWidthProp) and (FOwner.Width <> FWidthProp.Value) then
  FOwner.DocWidth := FWidthProp.Value
 else
 if (Prop = FHeightProp) and (FOwner.Height <> FHeightProp.Value) then
  FOwner.DocHeight := FHeightProp.Value
 else
 if (Prop = FConnectorsMinGapProp) and
    (FOwner.ConnectorsMinGap <> FConnectorsMinGapProp.Value) then begin
  if FConnectorsMinGapProp.Value <= 0 then
   FConnectorsMinGapProp.Value := 1
  else
   FOwner.ConnectorsMinGap := FConnectorsMinGapProp.Value;
 end;
end;

procedure TFlexSchemes.PropStored(Sender: TObject; Prop: TCustomProp;
  var IsStored: boolean; const PropName: string = '');
begin
 if (Prop = FWidthProp) or (Prop = FHeightProp) then
  IsStored := true
 else
 if Prop = FConnectorsMinGapProp then
  IsStored := FConnectorsMinGapProp.Value <> StdConnectorsMinGap
 else
 if Prop = FConnectorsKeepLinkProp then
  IsStored := FConnectorsKeepLinkProp.Value
 else
  inherited;
end;

function TFlexSchemes.Add(AControl: TFlexControl): integer;
begin
 if AControl is TFlexCustomScheme then begin
  Result := inherited Add(AControl);
  if Result >=0 then begin
   DoNotify(fnSchemes);
   if not Assigned(FOwner.ActiveScheme) then
    FOwner.ActiveScheme := TFlexCustomScheme(AControl);
  end;
 end else
  Result := -1;
end;

procedure TFlexSchemes.Delete(Index: integer);
var Scheme: TFlexScheme;
begin
 Scheme := TFlexScheme(FControls[Index]);
 inherited;
 if FControls.IndexOf(Scheme) >= 0 then exit;
 if FOwner.ActiveScheme = Scheme then begin
  if Index = FControls.Count then dec(Index);
  if Index >= 0
   then FOwner.ActiveScheme := TFlexScheme(FControls[Index])
   else FOwner.ActiveScheme := Nil;
 end;
 DoNotify(fnSchemes);
end;

procedure TFlexSchemes.ChangeOrder(CurIndex, NewIndex: integer);
begin
 inherited;
 DoNotify(fnSchemes);
end;

function TFlexSchemes.GetScheme(Index: integer): TFlexCustomScheme;
begin
 Result := TFlexScheme(FControls[Index]);
end;

function TFlexSchemes.GetByName(const Name: string): TFlexCustomScheme;
begin
 Result := TFlexCustomScheme ( inherited ByName[Name] );
end;

// TFlexDragObject ///////////////////////////////////////////////////////////

destructor TFlexDragObject.Destroy;
var Flex: TFlexPanel;
begin
 if Assigned(FDragGroup) and (FDragGroup.Parent = Nil) then begin
  Flex := FDragGroup.Owner;
  if Assigned(Flex) then Flex.History.DisableRecording;
  try
   FDragGroup.Free;
   FDragGroup := Nil;
  finally
   if Assigned(Flex) then Flex.History.EnableRecording;
  end;
 end;
 inherited;
end;

procedure TFlexDragObject.Finished(Target: TObject; X, Y: Integer;
  Accepted: Boolean);
begin
 Free;
end;

procedure TFlexDragObject.UpdateLink(Flex: TFlexPanel;
  MouseX, MouseY: integer);
begin
 Flex.History.DisableRecording;
 try
  with FDragGroup do begin
   Flex.ActiveScheme.Add(DragGroup);
   Layer := Flex.ActiveLayer;
   Flex.UnTransformPoint(MouseX, MouseY);
   BeginUpdate;
   try
    Left := MouseX - FMouseDragDelta.X;
    Top  := MouseY - FMouseDragDelta.Y;
   finally
    EndUpdate;
   end;
  end;
 finally
  Flex.History.EnableRecording;
 end;
end;

procedure TFlexDragObject.Unlink;
var PassRec: TPassControlRec;
    Control: TFlexControl;
    GroupOwner: TFlexPanel;
begin
 with FDragGroup do begin
  GroupOwner := Owner;
  GroupOwner.History.DisableRecording;
  try
   FDefaultFlex.History.DisableRecording;
   try
    Owner := FDefaultFlex;
    IsSelected := False;
    Layer := Nil;
    // Reset Id's
    Control := FDragGroup;
    FirstControl(Control, PassRec);
    try
     while Assigned(Control) do begin
      Control.ID := 0;
      Control := NextControl(PassRec);
     end;
    finally
     ClosePassRec(PassRec);
    end;
    Left := 0;
    Top := 0;
   finally
    FDefaultFlex.History.EnableRecording;
   end;
  finally
   GroupOwner.History.EnableRecording;
  end;
 end;
end;

// TFlexGrid /////////////////////////////////////////////////////////////////

procedure TFlexGrid.ControlCreate;
begin
 FNoPaintRectCheck := true;
 FSnapTo := [ snLeft, snTop ];
 FColor := clGray;
 FStyle := gsDots;
 FPixColor := clSilver;
 Name := 'Grid';
 Width := FOwner.DocWidth;
 Height := FOwner.DocHeight;
end;

procedure TFlexGrid.CreateProperties;
const HideSet: TPropStyle = [ psReadOnly, psDontStore, psNonVisual ];
begin
 inherited;
 FLeftProp.Style := HideSet;
 FTopProp.Style := HideSet;
 FWidthProp.Style := HideSet;
 FWidthProp.OnGetValue := GetOwnerWidth;
 FHeightProp.Style := HideSet;
 FHeightProp.OnGetValue := GetOwnerHeight;
 FLayerProp.Style := HideSet;
 FReferenceProp.Style := HideSet;
end;

procedure TFlexGrid.DoCustomSnap(const SnapRect: TRect;
  var Delta: TPoint; HStep, VStep, HOffset, VOffset: integer;
  ResizeCursor: TResizeCursor; SnapTo: TFlexSnaps);
type
  TParams = record
   Dist: integer;
   Offset: integer;
   Step: integer;
   PosSum: integer;
   Use: boolean;
  end;
var
 HParams, VParams: TParams;
 Snap: TFlexSnap;
 Snaps: TFlexSnaps;
 R: TRect;
 NotAll: boolean;

 procedure CalcPos(RectPos: integer; var Params: TParams);
 var TempDist: integer;
 begin
  with Params do
  if Step > 0 then begin
   TempDist := Step * Round((RectPos - Offset) / Step) + Offset - RectPos;
   if (ResizeCursor <> rcrNone) and (Snap = snCenter) then 
    // Recalc dist for center snap in resize mode
    TempDist := 2*(RectPos + TempDist) - PosSum;
   if not Use or (Abs(TempDist) < Abs(Dist)) then begin
    Dist := TempDist;
    Use := true;
   end;
  end;
 end;

begin
 HParams.Use := false;
 HParams.Offset := HOffset;
 HParams.Step := HStep;
 VParams.Use := false;
 VParams.Offset := VOffset;
 VParams.Step := VStep;
 R := SnapRect;
 NotAll := not (snAll in SnapTo);
 if ResizeCursor <> rcrNone then begin
  Snaps := [];
  // Resize rect
  if ResizeCursor in [ rcrTopLeft, rcrLeft, rcrBottomLeft] then begin
   inc(R.Left, Delta.x);
   Include(Snaps, snLeft);
  end;
  if ResizeCursor in [ rcrTopLeft, rcrTop, rcrTopRight ] then begin
   inc(R.Top, Delta.y);
   Include(Snaps, snTop);
  end;
  if ResizeCursor in [ rcrTopRight, rcrRight, rcrBottomRight] then begin
   inc(R.Right, Delta.x);
   Include(Snaps, snRight);
  end;
  if ResizeCursor in [ rcrBottomLeft, rcrBottom, rcrBottomRight] then begin
   inc(R.Bottom, Delta.y);
   Include(Snaps, snBottom);
  end;
  if snCenter in SnapTo then Include(Snaps, snCenter);
  if NotAll then Snaps := Snaps * SnapTo;
  HParams.PosSum := R.Left + R.Right;
  VParams.PosSum := R.Top + R.Bottom;
 end else begin
  // Offset rect
  OffsetRect(R, Delta.x, Delta.y);
  Snaps := SnapTo;
 end;
 // Calc nearest grid node
 for Snap:=Low(Snap) to High(Snap) do begin
  if NotAll and not (Snap in Snaps) then continue;
  case Snap of
   snLeft   : CalcPos(R.Left,   HParams);
   snTop    : CalcPos(R.Top,    VParams);
   snRight  : CalcPos(R.Right,  HParams);
   snBottom : CalcPos(R.Bottom, VParams);
   snCenter : begin
               CalcPos((R.Right + R.Left) div 2, HParams);
               CalcPos((R.Bottom + R.Top) div 2, VParams);
              end;
  end;
 end;
 // Calc new delta
 if HParams.Use then inc(Delta.X, HParams.Dist);
 if VParams.Use then inc(Delta.Y, VParams.Dist);
end;

procedure TFlexGrid.DoSnap(const SnapRect: TRect; var Delta: TPoint;
  ResizeCursor: TResizeCursor = rcrNone);
begin
 DoCustomSnap(SnapRect, Delta, FHSize, FVSize, FHOffset, FVOffset,
   ResizeCursor, FSnapTo);
end;

procedure TFlexGrid.GetOwnerHeight(Sender: TObject; out Value: integer);
begin
 Value := FOwner.DocHeight;
end;

procedure TFlexGrid.GetOwnerWidth(Sender: TObject; out Value: integer);
begin
 Value := FOwner.DocWidth;
end;

procedure TFlexGrid.Paint(Canvas: TCanvas; var PaintRect: TRect);
var Step, Coeff, HStep, VStep, HOfs, VOfs: double;
    GridRect: TRect;
    // GridSize: TPoint;

 procedure DrawGrid(Vert: boolean);
 var R: TRect;
     Last: integer;
     x, y: double;
 begin
  if Vert then begin
   Last := GridRect.Right;
   if FOwner.ShowDocFrame then dec(Last);
   x := HOfs + GridRect.Left;
   R := Rect(0, GridRect.Top, 0, GridRect.Bottom);
   repeat
    R.Left := Round(x);
    if R.Left >= Last then break;
    R.Right := R.Left+1;
    Canvas.FillRect(R);
    x := x + Step;
   until false;
  end else begin
   Last := GridRect.Bottom;
   if FOwner.ShowDocFrame then dec(Last);
   y := VOfs + GridRect.Top;
   R := Rect(GridRect.Left, 0, GridRect.Right, 0);
   repeat
    R.Top := Round(y);
    if R.Top >= Last then break;
    R.Bottom := R.Top+1;
    Canvas.FillRect(R);
    y := y + Step;
   until false;
  end;
 end;

 procedure DrawDots(HStep, VStep: double);
 var R: TRect;
     x, y: double;
 begin
  y := VOfs + GridRect.Top;
  repeat
   R.Top := Round(y);
   if R.Top > GridRect.Bottom then break;
   x := HOfs + GridRect.Left;
   repeat
    R.Left := Round(x);
    if R.Left > GridRect.Right then break;
    R.Bottom := R.Top+1;
    R.Right := R.Left+1;
    Canvas.FillRect(R);
    x := x + HStep;
   until false;
   y := y + VStep;
  until false;
 end;

begin
 with Canvas do begin
  GridRect := PaintRect;
  // TODO:
{  GridSize.X := ScaleValue(FHSize, Owner.Scale);
  GridSize.Y := ScaleValue(FVSize, Owner.Scale);
  with Owner.PaintRect do begin
   if Left - GridRect.Left > GridSize.X then
    GridRect.Left :=
      GridRect.Left + ((Left - GridRect.Left) div GridSize.X -1) * GridSize.X;
   if Top - GridRect.Top > GridSize.Y then
    GridRect.Top :=
      GridRect.Top + ((Top - GridRect.Top) div GridSize.Y -1) * GridSize.Y;
   if GridRect.Right > Right then GridRect.Right := Right;
   if GridRect.Bottom > Bottom then GridRect.Bottom := Bottom;
  end;  }
  if FShowPixGrid and (FOwner.Scale >= 400) then begin
   Brush.Color := FPixColor;
   Brush.Style := bsSolid;
   Step := FOwner.Scale / 100;
   DrawGrid(True);
   DrawGrid(False);
  end;
  if (FHSize > 1) and (FVSize > 1) and (FColor <> clNone) then begin
   Brush.Color := FColor;
   Brush.Style := bsSolid;
   Coeff := FOwner.Scale / (100 * PixelScaleFactor);
   HOfs := (FHOffset mod FHSize) * Coeff;
   HStep := FHSize;
   HStep := HStep * Coeff;
   while HStep < 4 do HStep := 2*HStep;
   VOfs := (FVOffset mod FVSize) * Coeff;
   VStep := FVSize;
   VStep := VStep * Coeff;
   while VStep < 4 do VStep := 2*VStep;
   case FOwner.FGridControl.Style of
    gsLines:
      begin
       Step := HStep;
       DrawGrid(True);
       Step := VStep;
       DrawGrid(False);
      end;
    gsDots:
      begin
       DrawDots(HStep, VStep);
      end;
   end;
  end;
 end;
end;

procedure TFlexGrid.SetColor(const Value: TColor);
begin
 if Value = FColor then exit;
 FColor := Value;
 if Visible then Invalidate;
end;

procedure TFlexGrid.SetHOffset(const Value: integer);
begin
 if Value = FHOffset then exit;
 FHOffset := Value;
 if Visible then Invalidate;
end;

procedure TFlexGrid.SetHSize(const Value: integer);
begin
 if Value = FHSize then exit;
 FHSize := Value;
 if Visible then Invalidate;
end;

procedure TFlexGrid.SetPixColor(const Value: TColor);
begin
 if Value = FPixColor then exit;
 FPixColor := Value;
 if Visible and FShowPixGrid then Invalidate;  
end;

procedure TFlexGrid.SetShowPixGrid(const Value: boolean);
begin
 if Value = FShowPixGrid then exit;
 FShowPixGrid := Value;
 if Visible then Invalidate;
end;

procedure TFlexGrid.SetStyle(const Value: TFlexGridStyle);
begin
 if Value = FStyle then exit;
 FStyle := Value;
 if Visible then Invalidate;
end;

procedure TFlexGrid.SetVOffset(const Value: integer);
begin
 if Value = FVOffset then exit;
 FVOffset := Value;
 if Visible then Invalidate;
end;

procedure TFlexGrid.SetVSize(const Value: integer);
begin
 if Value = FVSize then exit;
 FVSize := Value;
 if Visible then Invalidate;
end;

// TFlexPaintList /////////////////////////////////////////////////////////////

constructor TFlexPaintList.Create(AOwner: TFlexPanel);
begin
 FOwner := AOwner;
 FList := TList.Create;
end;

destructor TFlexPaintList.Destroy;
begin
 inherited;
 Clear;
 FList.Free;
end;

procedure TFlexPaintList.Clear;
var i: integer;
begin
 for i:=0 to FList.Count-1 do Dispose(PFlexPaintStruct(FList[i]));
 FList.Clear;
end;

procedure TFlexPaintList.StoreParams(PaintStruct: PFlexPaintStruct;
  ACanvas: TCanvas);
begin
 if not Assigned(PaintStruct) then exit;
 with FOwner, PaintStruct^ do begin
  IsFirstPaint := FList.Count = 1;
  Canvas := ACanvas;
  PaintForExport := FPaintForExport;
  PaintRect := FPaintRect;
  PaintWidth := FPaintWidth;
  PaintHeight := FPaintHeight;
  UseOriginalBezier := FUseOriginalBezier;
  UseImageClipTransparent := FUseImageClipTransparent;
  Origin := FOrigin;
  Scale := FScale;
 end;
end;

procedure TFlexPaintList.RestoreParams(PaintStruct: PFlexPaintStruct);
begin
 if not Assigned(PaintStruct) then exit;
 with FOwner, PaintStruct^ do begin
  FPaintForExport := PaintForExport;
  FPaintRect := PaintRect;
  FPaintWidth := PaintWidth;
  FPaintHeight := PaintHeight;
  FUseOriginalBezier := UseOriginalBezier;
  FUseImageClipTransparent := UseImageClipTransparent;
  FOrigin := Origin;
  FScale := Scale;
 end;
end;

function TFlexPaintList.BeginPaint(ACanvas: TCanvas): PFlexPaintStruct;
begin
 if FFirstInUse or (FList.Count = 0) then begin
  New(Result);
  FList.Add(Result);
 end else
  Result := PFlexPaintStruct(FList[0]);
 FFirstInUse := true;
 StoreParams(Result, ACanvas);
end;

procedure TFlexPaintList.EndPaint(PaintStruct: PFlexPaintStruct);
var Index: integer;
begin
 if not Assigned(PaintStruct) then exit;
 RestoreParams(PaintStruct);
 Index := FList.IndexOf(PaintStruct);
 if Index = 0 then
  FFirstInUse := false
 else begin
  FList.Delete(Index);
  Dispose(PaintStruct);
 end;
end;

// TFlexPanelHistory //////////////////////////////////////////////////////////

constructor TFlexPanelHistory.Create(AOwner: TFlexPanel);
begin
 inherited Create(AOwner);
 FPanel := AOwner;
end;

procedure TFlexPanelHistory.DoChange;
begin
 if not (csDestroying in FPanel.ComponentState) then
  inherited;
end;

function  TFlexPanelHistory.Undo: boolean;
begin
 FPanel.BeginSelectionUpdate;
 try
  Result := inherited Undo;
 finally
  FPanel.EndSelectionUpdate;
 end;
end;

function  TFlexPanelHistory.Redo: boolean;
begin
 FPanel.BeginSelectionUpdate;
 try
  Result := inherited Redo;
 finally
  FPanel.EndSelectionUpdate;
 end;
end;

function TFlexPanelHistory.SourceControl(Source: TObject): TFlexControl;
var AOwner: TObject;
begin
 Result := Nil;
 if not Assigned(Source) then exit;
 if Source is TCustomProp then begin
  AOwner := TCustomProp(Source).Owner.Owner;
  if AOwner is TFlexControl then Result := TFlexControl(AOwner);
 end else
 if Source is TFlexControl then
  Result := TFlexControl(Source);
end;

procedure TFlexPanelHistory.SourceInfo(Source: TObject;
  out Id: LongWord; out PropName: string);
var Control: TFlexControl;
begin
 Control := SourceControl(Source);
 if Assigned(Control) then begin
  Id := Control.IdProp.Value;
  if Source is TCustomProp
   then PropName := TCustomProp(Source).Name
   else PropName := '';
 end else begin
  Id := 0;
  PropName := ''
 end;
end;

function TFlexPanelHistory.DoGetActionCaption(Action: THistoryAction;
  const Source: TObject): string;
var Control: TFlexControl;
    Prop: TCustomProp;
    PropList: TPropList;
    PropIndex: integer;
    s: string;

 function ControlName: string;
 begin
  Result := '';
  if not Assigned(Control) then exit;
  if Assigned(Control.NameProp) then
   Result := Control.NameProp.Value;
  if Result = '' then Result := '<' + Control.ClassName + '>';
 end;

begin
 Result := inherited DoGetActionCaption(Action, Source);
 if Result <> '' then exit;
 if Source is TFlexControl
  then Control := TFlexControl(Source)
  else Control := Nil;
 if Source is TCustomProp
  then Prop := TCustomProp(Source)
  else Prop := Nil;
 if (Action is TPropHistoryAction) and Assigned(Prop) then begin
  PropList := Prop.Owner;
  PropIndex := PropList.IndexOf(Prop);
  if PropIndex >= 0 then begin
   s := PropList.PropNames[PropIndex];
   if (PropList.Owner is TFlexControl) then begin
    Control := TFlexControl(PropList.Owner);
    s := ControlName + '.' + s;
   end;
   Result := 'Change ' + s + ' property';
  end;
 end else
 if Action is TPropHistoryGroup then
  Result := 'Change control(s) properties'
 else
 if (Action is TDocRectHistoryAction) and Assigned(Control) then
  Result := 'Change ' + ControlName + ' size/position'
 else
 if (Action is TOrderHistoryAction) and Assigned(Control) then
  Result := 'Change ' + ControlName + ' order'
 else
 if (Action is TPointsHistoryAction) and Assigned(Control) then
  Result := 'Change ' + ControlName + ' points'
 else
 if Action is TPanelSizeMoveHistoryGroup then
  Result := 'Size/move control(s)'
 else
 if Action is TPanelAlignHistoryGroup then
  Result := 'Align control(s)'
 else
 if Action is TPanelCreateControlsHistoryGroup then
  Result := 'Create control(s)'
 else
 if Action is TPanelDestroyControlsHistoryGroup then
  Result := 'Delete control(s)'
 else
 if Action is TControlGroupHistoryAction then
  Result := 'Group controls'
 else
 if Action is TControlUngroupHistoryAction then
  Result := 'Ungroup control'
 else
 if Action is TPanelUngroupControlsHistoryGroup then
  Result := 'Ungroup control(s)'
 else
 if Action is TPanelDuplicateHistoryGroup then
  Result := 'Duplicate control(s)'
 else
 if Action is TPanelCloneHistoryGroup then
  Result := 'Clone control(s)'
 else
 if Action is TPanelOrderHistoryGroup then
  Result := 'Change control(s) order'
 else
 if Action is TPanelPointsHistoryGroup then
  Result := 'Change control(s) points'
 else
 if Action is TPanelRerouteHistoryGroup then
  Result := 'Reroute control(s)'
 else
 if Action is TPanelColorHistoryGroup then
  Result := 'Change control(s) color'
 else
 if Action is TPanelPropsHistoryGroup then
  Result := 'Change control(s) properties'
 else
 if (Action is TControlCreateHistoryGroup) and Assigned(Control) then
  Result := 'Create ' + ControlName + ' control'
 else
 if (Action is TControlDestroyHistoryGroup) and Assigned(Control) then
  Result := 'Destroy ' + ControlName + ' control';
end;

function TFlexPanelHistory.DoGetActionSource(Action: THistoryAction;
  var Enabled: boolean): TObject;
var Control: TFlexControl;
begin
 Enabled := false;
 Result := inherited DoGetActionSource(Action, Enabled);
 if Enabled then exit;
 if Action is TPropHistoryGroup then begin
  Result := Nil;
  Enabled := true;
  exit;
 end else
 if Action is TIdHistoryAction then begin
  Control := FPanel.FindControlByID(TPropHistoryAction(Action).SourceId);
  if (Action is TPropHistoryAction) and Assigned(Control)
   then Result := Control.Props[TPropHistoryAction(Action).SourcePropName]
   else Result := Control;
 end else
 if Action is TCreateDestroyHistoryGroup then begin
  with TCreateDestroyHistoryGroup(Action) do
   Result := SourcePanel.FindControlByID(SourceId);
  Enabled := true;
  exit;
 end else
 if Action is TFlexPanelHistoryGroup then
  Result := TFlexPanelHistoryGroup(Action).SourcePanel;
 // Check group/ungroup actions
 if Action is TGroupUngroupHistoryAction then
  Enabled := true // source can be nil
 else
  Enabled := Assigned(Result);
end;

procedure TFlexPanelHistory.DoSetActionSource(Action: THistoryAction;
  const Source: TObject);
var AId: LongWord;
    APropName: string;
begin
 if Action is TIdHistoryAction then with TIdHistoryAction(Action) do begin
  SourceInfo(Source, AId, APropName);
  SourceId := AId;
  SourcePropName := APropName;
 end else
 if (Action is TFlexPanelHistoryGroup) and (Source is TFlexPanel) then begin
  TFlexPanelHistoryGroup(Action).SourcePanel := TFlexPanel(Source);
 end;
 inherited;
end;

function TFlexPanelHistory.DoIsRecordable(var ActionClass: THistoryActionClass;
  var Source: TObject; Parent: THistoryGroup): boolean;
var Control: TFlexControl;
begin
 Result := inherited DoIsRecordable(ActionClass, Source, Parent);
 if not Result then exit;
 if ActionClass.InheritsFrom(TIdHistoryAction) and (Source is TFlexControl) and
    (TFlexControl(Source).State * [fsCreating, fsDestroying] <> []) then begin
  // No actions while control is creating or destroying
  Result := false;
  exit;
 end;
 if ActionClass.InheritsFrom(TPropHistoryAction) and
    (Source is TCustomProp) then begin
  Control := SourceControl(Source);
  if Assigned(Control) then
   if fsLoading in Control.State then
    Result := false
   else
   if (Control.UpdateCounter > 0) and (
     (Source = Control.LeftProp) or (Source = Control.TopProp) or
     (Source = Control.WidthProp) or (Source = Control.HeightProp)) then
   // Store action via TDocRectHistoryAction in TFlexControl.DoNotify(fnRect)
   Result := false;
 end;
end;

function TFlexPanelHistory.DoIsSame(ExistedAction: THistoryAction;
  NewActionClass: THistoryActionClass; NewSource: TObject): boolean;
var NewId: LongWord;
    NewPropName: string;
begin
 Result := inherited DoIsSame(ExistedAction, NewActionClass, NewSource);
 if Result then exit;
 if (ExistedAction is TIdHistoryAction) and
    (ExistedAction.ClassType = NewActionClass) then begin
  SourceInfo(NewSource, NewId, NewPropName);
  with TIdHistoryAction(ExistedAction) do
   Result := (SourceId = NewId) and (SourcePropName = NewPropName);
 end;
end;

function TFlexPanelHistory.BeginPanelGroup(
  GroupClass: THistoryGroupClass): THistoryGroup;
begin
 if GroupLevel = 0
  then Result := THistoryGroup(BeginAction(GroupClass, FPanel))
  else Result := Nil;
end;

function TFlexPanelHistory.EndPanelGroup(
  GroupClass: THistoryGroupClass): boolean;
begin
 Result :=
   {(GroupLevel = 1) and }Assigned(GroupClass) and
   (ActionIndex >= 0) and (Actions[ActionIndex] is GroupClass);
 if Result then Result := EndAction;
end;

function TFlexPanelHistory.RecordSelectedAsNew: boolean;
var i, LayerIdx: integer;
    Scheme: TFlexCustomScheme;
    PO: TPaintOrder;
begin
 Result := false;
 if FPanel.SelectedCount = 0 then exit;  
 BeginPanelGroup(TPanelCreateControlsHistoryGroup);
 with FPanel do
 try
  Scheme := Selected[0].ParentScheme;
  if Assigned(Scheme) then begin
   InitPaintOrder(PO);
   try
    Scheme.CreatePaintOrder(PO);
    for LayerIdx:=0 to High(PO.LayerRefs) do begin
     i := PO.LayerRefs[LayerIdx].First;
     while i >= 0 do with PO.ControlRefs[i] do begin
      if Control.IsSelected then
       RecordAction(TControlCreateHistoryGroup, Control);
      i := PO.ControlRefs[i].Next;
     end;
    end;
   finally
    ClearPaintOrder(PO);
   end;
  end;
 finally
  EndPanelGroup(TPanelCreateControlsHistoryGroup);
 end;
 Result := true;
end;

// TFlexPanel ////////////////////////////////////////////////////////////////

constructor TFlexPanel.Create(AOwner: TComponent);
var i: integer;
begin
 inherited;
 // ScrollBox init
 FModalDialogMode := false;
 ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
   csSetCaption, csDoubleClicks];
 Width := 185;
 Height := 41;
 FShowEditPointGuide := True;
 FBorderStyle := bsSingle;
 FInDesign := csDesigning in ComponentState;
 FAutoNames := true;
 FDefaultToolMode := ftmSelect;
 FToolMode := FDefaultToolMode;
 FDefaultLinkPoint.Index := -1;
 FDefaultLinkPoint.Size := DefaultLinkPointSize;
 FConnectorsMinGap := StdConnectorsMinGap;
 // Create history
 FHistory := TFlexPanelHistory.Create(Self);
 //
 FWithoutParent := TList.Create;
 FPaintList := TFlexPaintList.Create(Self);
 FAlphaBuffer := TAlphaBuffer.Create;
 FCanvas := TControlCanvas.Create;
 TControlCanvas(FCanvas).Control := Self;
 FIdPool := TIdPool.Create;
 FDocWidth := ScalePixels(320);
 FDocHeight := ScalePixels(240);
 FDocSpaceBrush := TBrush.Create;
 FDocSpaceBrush.Color := clBtnFace; // clWindow;
 FDocSpaceBrush.Style := bsBDiagonal;
 FDocSpaceBrush.OnChange := DocSpaceBrushChanged;
 FDocFrameColor := clBlack;
 FDocShadowColor := clGray;
 FScale := 100;
 FShowDocFrame := true;
 FPaintSchemeBackground := true;
 AutoScroll := False;
 FRefPropsList := TPropRefList.Create;
 FSelList := TList.Create;
 FLayers := TFlexLayers.Create(Self, Nil, Nil);
 FSchemes := TFlexSchemes.Create(Self, Nil, Nil);
 FGridControl := TFlexGrid.Create(Self, Nil, Nil);
 GridHorizSize := ScalePixels(10);
 GridVertSize := ScalePixels(10);
 DoubleBuffered := True;
 Color := clWindow;
 NewDocument;
 UpdateScrollBars;
 for i := 0 to High(DataArray) do
   DataArray[i] := null;
end;

destructor TFlexPanel.Destroy;
begin
 if Assigned(FNotifyLink) then FNotifyLink.DestroyNotify;
 EmptyDocument;
 FHistory.Active := false;
 FOnNotify := Nil;
 FSchemes.Free;
 FSchemes := Nil;
 FLayers.Free;
 FLayers := Nil;
 FGridControl.Free;
 FGridControl := Nil;
 FHistory.Free;
 FHistory := Nil;
 if FPointsRgn <> 0 then DeleteObject(FPointsRgn);
 FCanvas.Free;
 FDocSpaceBrush.Free;
 FRefPropsList.Free;
 FSelList.Free;
 FIdPool.Free;
 FNotifyLink.Free;
 with FPaintCache do begin
  if Bitmap <> 0 then DeleteObject(Bitmap);
  if DC <> 0 then DeleteDC(DC);
 end;
 FAlphaBuffer.Free;
 FPaintList.Free;
 FWithoutParent.Free;
 FFrostPanImage.Free;
 inherited;
end;

procedure TFlexPanel.Assign(Source: TPersistent);
var MS: TMemoryStream;
    Filer: TFlexFiler;
begin
 if Source is TFlexPanel then begin
  MS := Nil;
  Filer := Nil;
  try
   MS := TMemoryStream.Create;
   Filer := CreateFlexFiler(MS, ppCopy);
   TFlexPanel(Source).SaveToFiler(Filer);
   Filer.Rewind;
   LoadFromFiler(Filer, lfNew);
  finally
   Filer.Free;
   MS.Free;
  end;
 end else
  inherited;
end;

procedure TFlexPanel.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TFlexPanel.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TFlexPanel.SetConnectorsMinGap(const Value: integer);
var PassRec: TPassControlRec;
    Control: TFlexControl;
begin
 if (Value = FConnectorsMinGap) or (Value <= 0) then exit;
 FConnectorsMinGap := Value;
 if Assigned(FSchemes) then FSchemes.ConnectorsMinGapProp.Value := Value;
 // Send notification to all IsConnector controls in document
 Control := FirstControl(FSchemes, PassRec);
 while Assigned(Control) do begin
  if Control.IsConnectorControl then Control.ConnectorMinGapChanged;
  Control := NextControl(PassRec);
 end;
 ClosePassRec(PassRec);
end;

function TFlexPanel.GetNotifyLink: TNotifyLink;
begin
 if not Assigned(FNotifyLink) and not (csDestroying in ComponentState) then
  FNotifyLink := TNotifyLink.Create(Self);
 Result := FNotifyLink;
end;

procedure TFlexPanel.WMNCHitTest(var Message: TMessage);
begin
 DefaultHandler(Message);
end;

procedure TFlexPanel.WMNCCalcSize(var Message: TMessage);
begin
 inherited;
 Message.Result := Message.Result or WVR_REDRAW;
end;

procedure TFlexPanel.CMCtl3DChanged(var Message: TMessage);
begin
 if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
 inherited;
end;

procedure TFlexPanel.WMSize(var Message: TWMSize);
begin
 inherited;
 if FAutoZoom then Scale := 0; // change scale
 UpdateOrigin;
end;

procedure TFlexPanel.WMHScroll(var Message: TWMHScroll);
begin
 inherited;
 UpdateOrigin;
end;

procedure TFlexPanel.WMVScroll(var Message: TWMVScroll);
begin
 inherited;
 UpdateOrigin;
end;

procedure TFlexPanel.SetHorzExtraSpace(const Value: integer);
begin
 if Value = FHorzExtraSpace then exit;
 FHorzExtraSpace := Value;
 UpdateScrollBars;
end;

procedure TFlexPanel.SetVertExtraSpace(const Value: integer);
begin
 if Value = FVertExtraSpace then exit;
 FVertExtraSpace := Value;
 UpdateScrollBars; 
end;

procedure TFlexPanel.UpdateScrollBars;
var i: integer;
begin
 if FCanvas.LockCount <> 0 then exit;
 i := 10; //FScale div 100; if i = 0 then i := 1;
 HorzScrollBar.Range := ScaleValue(FDocWidth, FScale) + 2*FHorzExtraSpace;
 HorzScrollBar.Increment := i;
 VertScrollBar.Range := ScaleValue(FDocHeight, FScale) + 2*FVertExtraSpace;
 VertScrollBar.Increment := i;
 UpdateOrigin;
end;

procedure TFlexPanel.UpdateOrigin;
var Client: TSize;
    NewRect: TRect;
begin
 if (FCanvas.LockCount <> 0) or not Assigned(Parent) then exit;
 Client.cx := ClientWidth;
 Client.cy := ClientHeight;
 if HorzScrollBar.IsScrollBarVisible then
  FOrigin.X := HorzScrollBar.Position - FHorzExtraSpace
 else
  FOrigin.X := -(Client.cx - ScaleValue(FDocWidth, FScale) ) div 2;
 if VertScrollBar.IsScrollBarVisible then
  FOrigin.Y := VertScrollBar.Position - FVertExtraSpace
 else
  FOrigin.Y := -(Client.cy - ScaleValue(FDocHeight, FScale) ) div 2;
 // Update edit points
 UpdatePoints;
 // Update client doc rect
 with NewRect do begin
  Left := MulDiv(FOrigin.X, 100 * PixelScaleFactor, Scale);
  Top := MulDiv(FOrigin.Y, 100 * PixelScaleFactor, Scale);
  Right := Left + MulDiv(Client.cx, 100 * PixelScaleFactor, Scale);
  Bottom := Top + MulDiv(Client.cy, 100 * PixelScaleFactor, Scale);
 end;
 // Send notify if needed
 if not EqualRect(NewRect, FClientDocRect) then begin
  FClientDocRect := NewRect;
  DoNotify(Nil, fnPan);
 end;
end;

procedure TFlexPanel.SetAutoZoom(const Value: boolean);
begin
 if Value = FAutoZoom then exit;
 FAutoZoom := Value;
 if FAutoZoom then Scale := Scale - 1; // just change scale to any value
end;

procedure TFlexPanel.SetGridControl(const Value: TFlexGrid);
var GridId: LongWord;
begin
 if (Value = FGridControl) or
    not Assigned(Value) or (Value.Owner <> Self) then exit;
 GridId := FGridControl.IdProp.Value;
 FGridControl.Free;
 FGridControl := Value;
 Value.IdProp.Value := GridId;
end;

procedure TFlexPanel.SetHideSelection(const Value: boolean);
begin
 if Value = FHideSelection then exit;
 FHideSelection := Value;
end;

procedure TFlexPanel.SetDocHeight(const Value: integer);
begin
 if FCanvas.LockCount <> 0 then exit;
 FDocHeight := Value;
 UpdateScrollBars;
 if Assigned(FSchemes) then FSchemes.Height := FDocHeight;
 Invalidate;
end;

procedure TFlexPanel.SetDocWidth(const Value: integer);
begin
 if FCanvas.LockCount <> 0 then exit;
 FDocWidth := Value;
 UpdateScrollBars;
 if Assigned(FSchemes) then FSchemes.Width := FDocWidth;
 Invalidate;
end;

procedure TFlexPanel.SetDocFrameColor(const Value: TColor);
begin
 if Value = FDocFrameColor then exit;
 FDocFrameColor := Value;
 if FShowDocFrame then Invalidate;
end;

procedure TFlexPanel.SetDocShadowColor(const Value: TColor);
begin
 if Value = FDocFrameColor then exit;
 FDocShadowColor := Value;
 if FShowDocFrame then Invalidate;
end;

procedure TFlexPanel.SetDocSpaceFill(const Value: boolean);
begin
 if Value = FDocSpaceFill then exit;
 FDocSpaceFill := Value;
 Invalidate;
end;

procedure TFlexPanel.SetDocSpaceBrush(const Value: TBrush);
begin
 FDocSpaceBrush.Assign(Value);
 //FDocSpaceBrush := Value;
 Invalidate;
end;

procedure TFlexPanel.DocSpaceBrushChanged(Sender: TObject);
begin
 Invalidate;
end;

procedure TFlexPanel.SetDocClipping(const Value: boolean);
begin
 if Value = FDocClipping then exit;
 FDocClipping := Value;
 Invalidate;
end;

procedure TFlexPanel.SetShowDocFrame(const Value: boolean);
begin
 if Value = FShowDocFrame then exit;
 FShowDocFrame := Value;
 Invalidate;
end;

procedure TFlexPanel.SetShowEditPointGuide(const Value: boolean);
begin
 if Value = FShowEditPointGuide then exit;
 FShowEditPointGuide := Value;
 UpdatePoints;
end;

function TFlexPanel.GetDocFrameVisible: boolean;
begin
 if not FShowDocFrame then
  Result := False
 else
 if not FWMPaintProcessing then
  Result := True
 else
 if (FHorzExtraSpace > 0) or (FVertExtraSpace > 0) then
  Result := true
 else
 if csCustomPaint in ControlState
  then Result := (FInPaintOrigin.X < 0) or (FInPaintOrigin.Y < 0)
  else Result := (FOrigin.X < 0) or (FOrigin.Y < 0);
end;

procedure TFlexPanel.SetSchemeBkStretch(const Value: boolean);
begin
 if Value = FSchemeBkStretch then exit;
 FSchemeBkStretch := Value;
 Invalidate;
end;

procedure TFlexPanel.SetScale(Value: integer);
var AScale: TPoint;
begin
 if FCanvas.LockCount <> 0 then exit;
 if FAutoZoom then
  try
   // Calculate scale
   AScale.X := ClientWidth * 100 div UnscalePixels(DocWidth);
   AScale.Y := ClientHeight * 100 div UnscalePixels(DocHeight);
   if AScale.X < AScale.Y
    then Value := AScale.X
    else Value := AScale.Y;
  except
   // Skip exceptions
  end;
 if Value < MinScale then Value := MinScale else
 if Value > MaxScale then Value := MaxScale;
 if Value = FScale then exit;
 FScale := Value;
 UpdateScrollBars;
 UpdateSelection(Nil);
 Invalidate;
 DoNotify(Nil, fnScale);
end;

function NumbCompare(Item1, Item2: Pointer): Integer; far;
begin
 Result := integer(Item1) - integer(Item2);
end;

function TFlexPanel.FindControl(const AName: string;
  From: TFlexControl = Nil): TFlexControl;
var Check: TFlexControl;
    PassRec: TPassControlRec;
    Root: integer;
begin
 Result := Nil;
 for Root:=0 to 1 do begin
  if Assigned(From) then
   Check := From
  else
  if Root = 0
   then Check := FSchemes
   else Check := FLayers;
  FirstControl(Check, PassRec);
  while Assigned(Check) do begin
   if CompareStr(AName, Check.Name) = 0 then begin
    Result := Check;
    break;
   end;
   Check := NextControl(PassRec);
  end;
  if Assigned(From) or Assigned(Result) then break;
 end;
end;

function TFlexPanel.FindControlByID(ControlID: LongWord;
  From: TFlexControl = Nil): TFlexControl;
begin
 if not FIdPool.Used[ControlID] then
  Result := Nil
 else begin
  if not Assigned(From)
   then Result := FSchemes.FindByID(ControlID)
   else Result := From.FindByID(ControlID);
  if not Assigned(Result) and not Assigned(From) then begin
   Result := FLayers.FindByID(ControlID);
   if not Assigned(Result) then Result := WithoutParentFindByID(ControlID);
  end;
 end;
end;

function TFlexPanel.FindControlAtPoint(x, y: integer): TFlexControl;
begin
 if Assigned(FActiveScheme)
  then Result := FActiveScheme.FindControlAtPoint(x, y)
  else Result := Nil;
end;

function TFlexPanel.AddWithoutParent(Control: TFlexControl): integer;
begin
 if Assigned(Control) and not
    (Control is TFlexSchemes) and not
    (Control is TFlexLayers) and not
    (Control is TFlexGrid) and
    (FWithoutParent.IndexOf(Control) < 0)
  then Result := FWithoutParent.Add(Control)
  else Result := -1;
end;

function TFlexPanel.RemoveWithoutParent(Control: TFlexControl): boolean;
var Index: integer;
begin
 Result := false;
 if not Assigned(Control) or (fsCreating in Control.State) then exit;
 Result := FWithoutParent.Count > 0;
 if not Result then exit;
 Index := FWithoutParent.IndexOf(Control);
 Result := Index >= 0;
 if Result then FWithoutParent.Delete(Index);
end;

function TFlexPanel.WithoutParentFindById(ControlID: LongWord): TFlexControl;
var i: integer;
begin
 Result := Nil;
 for i:=0 to FWithoutParent.Count-1 do
  with TFlexControl(FWithoutParent[i]) do begin
   if IdProp.Value = ControlID then
    Result := TFlexControl(FWithoutParent[i])
   else
   if Count > 0 then
    Result := FindByID(ControlID);
   if Assigned(Result) then break;
  end;
end;

function TFlexPanel.GetDefaultNewName(Control: TFlexControl;
  RootControl: TFlexControl = Nil; GenerateNumb: boolean = False): string;
var Check: TFlexControl;
    PassRec: TPassControlRec;
    Root, Numb, i: integer;
    NumbList: TList;
    ControlName: string;
begin
 Result := Control.ClassName;
 if (Length(Result) > Length(ClassNamePrefix)) and
    StrBeginsFrom(Result, ClassNamePrefix) then
  Delete(Result, 1, Length(ClassNamePrefix))
 else
 if Result[1] = 'T' then
  Delete(Result, 1, 1);
 if not GenerateNumb or (FIsLoading > 0) then exit;
 NumbList := TList.Create;
 try
  for Root:=0 to 1 do begin
   if Root = 0 then begin
    if Assigned(RootControl)
     then Check := RootControl
     else Check := FSchemes;
   end else
   if Assigned(RootControl)
    then break
    else Check := FLayers;
   FirstControl(Check, PassRec);
   while Assigned(Check) do begin
    if Check <> Control then begin
     ControlName := Check.Name;
     if StrBeginsFrom(ControlName, Result) then begin
      Numb := StrToIntDef(copy(ControlName, Length(Result)+1, MaxInt), 0);
      if (Numb > 0) and (NumbList.IndexOf(pointer(Numb)) < 0) then
       NumbList.Add(pointer(Numb));
     end;
    end;
    Check := NextControl(PassRec);
   end;
  end;
  NumbList.Sort(NumbCompare);
  Numb := 1;
  for i:=0 to NumbList.Count-1 do
   if Numb = integer(NumbList[i])
    then inc(Numb)
    else break;
  Result := Result + IntToStr(Numb);
 finally
  NumbList.Free;
 end;
end;

procedure TFlexPanel.GenerateID(Control: TFlexControl);
begin
 if Assigned(FIdChangeControl) or
    (psReadOnly in Control.FIdProp.Style) then exit;
 FIdChangeControl := Control;
 try
  Control.ID := FIdPool.Generate;
 finally
  FIdChangeControl := Nil;
 end;
end;

function TFlexPanel.ValidateID(Control: TFlexControl; LastID: LongWord): boolean;
var OldID: integer;
begin
 Result := false;
 if not Assigned(Control) or (Control = FIdChangeControl) then exit;
 OldID := Control.ID;
 if OldID = 0 then begin
  FIdPool.Release(LastID);
  Result := true;
 end else
 if psReadOnly in Control.FIdProp.Style then begin
  Result := not FIdPool.Used[OldID];
 end else begin
  FIdChangeControl := Control;
  try
   Result := (OldID = 0) or not FIdPool.Used[OldID];
   if not Result then begin
    // This Id already exist
    if LastID = 0 then LastID := FIdPool.Generate;
    // Set new generated Id
    Control.ID := LastID;
    Result := true;
   end else begin
    // Use Id and realease last Id
    FIdPool.Release(LastID);
    FIdPool.Use(OldID);
   end;
  finally
   FIdChangeControl := Nil;
  end;
 end;
 if (FIsLoading > 0) and (OldId <> 0) then 
  // Save used id in ref-list
  Control.Props.AddLoadedIDAlias(OldID, Control.ID);
end;

procedure TFlexPanel.SetCreatingControlClass(const Value: TFlexControlClass);
var i: integer;
    Found: boolean;
begin
 if Value = FCreatingControlClass then exit;
 if Assigned(Value) then begin
  if Length(RegisteredFlexControls) = 0 then exit;
  Found := false;
  for i:=0 to High(RegisteredFlexControls) do
   if Value = RegisteredFlexControls[i] then begin
    Found := true;
    break;
   end;
  if not Found then exit;
 end;
 FCreatingControlClass := Value;
 UpdateCursor;
end;

function TFlexPanel.GetCppCreatingControlClass: cardinal;
begin
 Result := cardinal(FCreatingControlClass);
end;

procedure TFlexPanel.SetCppCreatingControlClass(const Value: cardinal);
begin
 SetCreatingControlClass(TFlexControlClass(Value));
end;

procedure TFlexPanel.SetActiveLayer(const Value: TFlexLayer);
begin
 if Value = FActiveLayer then exit;
 FActiveLayer := Value;
 DoNotify(Nil, fnLayers);
end;

procedure TFlexPanel.SetActiveScheme(const Value: TFlexCustomScheme);
begin
 if Value = FActiveScheme then exit;
 UnselectAll;
 FActiveScheme := Value;
 DoNotify(Nil, fnSchemes);
 Invalidate;
end;

function TFlexPanel.GetDefaultScheme: TFlexCustomScheme;
var i: integer;
begin
 if FSchemes.Count = 0 then begin
  Result := Nil;
  exit;
 end;
 Result := FSchemes[0];
 for i:=0 to FSchemes.Count-1 do
  if FSchemes[i].Default then begin	  	
   Result := FSchemes[i];
   break;
  end;
end;

function TFlexPanel.GetScriptProp: TStrListProp;
begin
  Result := Schemes.FScriptProp;
end;

function TFlexPanel.GetSelected(Index: integer): TFlexControl;
begin
 Result := TFlexControl(FSelList[Index]);
end;

function TFlexPanel.GetSelectedCount: integer;
begin
 Result := FSelList.Count;
end;

procedure TFlexPanel.EmptyDocument;
begin
 FHistory.DisableRecording;
 try
  FHistory.Clear;
  UnselectAll;
  ActiveScheme := Nil;
  FSchemes.Clear;
  ActiveLayer := Nil;
  FLayers.Clear;
 finally
  FHistory.EnableRecording;
 end;
end;

procedure TFlexPanel.NewDocument;
begin
 FHistory.DisableRecording;
 try
  EmptyDocument;
  if FSchemes.Count = 0
   then FActiveScheme := TFlexScheme.Create(Self, FSchemes, Nil) //FSchemes.New
   else FActiveScheme := FSchemes[0];
  if FLayers.Count = 0
   then FActiveLayer := FLayers.New
   else FActiveLayer := FLayers[0];
 finally
  FHistory.EnableRecording;
 end;
end;

procedure TFlexPanel.TransformRect(var R: TRect);
var Denominator: integer;
begin
 Denominator := 100 * PixelScaleFactor;
 if FScale <> Denominator then with R do begin
  Left := MulDiv(Left, FScale, Denominator);
  Top := MulDiv(Top, FScale, Denominator);
  Right := MulDiv(Right, FScale, Denominator);
  Bottom := MulDiv(Bottom, FScale, Denominator);
 end;
 OffsetRect(R, -FOrigin.X, -FOrigin.Y);
end;

procedure TFlexPanel.TransformPoint(var px, py: integer);
var Denominator: integer;
begin
 Denominator := 100 * PixelScaleFactor;
 if FScale <> Denominator then begin
  px := MulDiv(px, FScale, Denominator);
  py := MulDiv(py, FScale, Denominator);
 end;
 dec(px, FOrigin.X);
 dec(py, FOrigin.Y);
end;

procedure TFlexPanel.TransformPointIndirect(var P: TPoint);
begin
 TransformPoint(P.X, P.Y);
end;

procedure TFlexPanel.UnTransformPoint(var px, py: integer);
var Numerator: integer;
begin
 inc(px, FOrigin.X);
 inc(py, FOrigin.Y);
 Numerator := 100 * PixelScaleFactor;
 if FScale <> Numerator then begin
  px := MulDiv(px, Numerator, FScale);
  py := MulDiv(py, Numerator, FScale);
 end;
end;

function TFlexPanel.IsInternalControl(Control: TFlexControl): boolean;
var AParent: TFlexControl;
begin
 AParent := Control;
 if Assigned(AParent) then
  while Assigned(AParent.Parent) do AParent := AParent.Parent;
 Result := (AParent <> FSchemes) and (AParent <> FLayers) and Assigned(AParent);
end;

procedure TFlexPanel.Click;
begin
 if not FInDesign and (FToolMode = ftmSelect) and 
    Assigned(FMouseControl) and Assigned(FMouseControl.Reference) then
  ActiveScheme := FMouseControl.Reference;
 inherited;
end;

procedure TFlexPanel.AutoSize;
var HVisible, VVisible: boolean;
begin
 if not Assigned(Parent) then exit;
 HVisible := HorzScrollBar.Visible;
 VVisible := VertScrollBar.Visible;
 try
  HorzScrollBar.Visible := False;
  VertScrollBar.Visible := False;
  ClientWidth := UnScalePixels(DocWidth);
  ClientHeight := UnScalePixels(DocHeight);
 finally
  HorzScrollBar.Visible := HVisible;
  VertScrollBar.Visible := VVisible;
 end;
end;

function TFlexPanel.GetIsLoading: boolean;
begin
 Result := FIsLoading > 0;
end;

procedure TFlexPanel.BeginLoading;
begin
 inc(FIsLoading);
end;

procedure TFlexPanel.EndLoading;
begin
 if FIsLoading = 0 then exit;
 dec(FIsLoading);
 if FIsLoading = 0 then begin
  FRefPropsList.ResolveAllRefs;
 end;
end;

procedure TFlexPanel.DoNotify(Control: TFlexControl; Notify: TFlexNotify);
begin
 if Notify in [fnCreated, fnDestroyed, fnOrder, fnLayers, fnSchemes] then
  FModified := true;
 case Notify of
  fnRect:
   if ListScan(Control, FSelList.List, FSelList.Count) >= 0 then
    UpdateSelection(Control);
  fnAnchorPoints:
   if ListScan(Control, FSelList.List, FSelList.Count) >= 0 then
    UpdatePoints;
  fnEditPoints:
   if ListScan(Control, FSelList.List, FSelList.Count) >= 0 then
    UpdateSelection(Nil);
  fnSelect:
   UpdateSelection(Control);
  fnDestroyed:
   begin
    if Control = FMouseControl then SetMouseControl(Nil);
    if Control = FMouseSubControl then SetMouseControl(FMouseControl);
    if Control = FEditPointControl then EditPointControl := Nil;
   end;
 end;
 if Assigned(FOnNotify) and
    ((Notify = fnDestroyed) or not IsInternalControl(Control)) then
  FOnNotify(Self, Control, Notify);
 if Assigned(FNotifyLink) then FNotifyLink.ControlNotify(Control, Notify);
end;

procedure TFlexPanel.DoNeedHint(var IsNeedHint: boolean);
begin
 if Assigned(FOnNeedHint) then FOnNeedHint(Self, IsNeedHint);
end;

procedure TFlexPanel.DoShowHint(var HintInfo: THintInfo;
  var IsShowHint: boolean);
begin
 if Assigned(FOnShowHint) then
  FOnShowHint(Self, FHintControl, HintInfo, IsShowHint);
end;

procedure TFlexPanel.CMHintShow(var Message: TMessage);
var IsShowHint: boolean;
    HintInfo: PHintInfo;
begin
// if not FInDesign then begin
  IsShowHint := Self.ShowHint;
  HintInfo := TCMHintShow(Message).HintInfo;
  if Assigned(FHintControl) then
   FHintControl.DoNeedHint(HintInfo^, IsShowHint);
  DoShowHint(HintInfo^, IsShowHint);
  if IsShowHint
   then Message.Result := 0 // Must show
   else Message.Result := 1;
{ end else
  inherited; }
end;

procedure TFlexPanel.WMPaint(var Message: TWMPaint);
var
 ScrDC, TmpDC: HDC;
 AOrigin: TPoint;
 APaintRect: TRect;
 FWMPaintStruct: TPaintStruct;
 PaintWidth, PaintHeight: integer;
 ClipRgn: HRGN;
 ForceRecreate: boolean;
begin
 // Check DC
 if Message.DC <> 0 then begin
  // Output to given device (for export)
  FCanvas.Lock;
  try
   FWMPaintProcessing := true;
   // Setup FCanvas
   FCanvas.Handle := Message.DC;
   TControlCanvas(FCanvas).UpdateTextFlags;
   // Paint
   PaintTo(FCanvas, ClientRect, FOrigin, FScale, Nil,
      True, False, False, True, True, True);
  finally
   // End paint
   FCanvas.Unlock;
   FWMPaintProcessing := false;
  end;
 end else begin
  // Buffered paint
  ScrDC := BeginPaint(Handle, FWMPaintStruct);
  try
   FWMPaintProcessing := true;
   // Calc paint size
   with FWMPaintStruct.rcPaint do begin
    PaintWidth := Right - Left;
    PaintHeight := Bottom - Top;
   end;
   with FPaintCache do begin
    // Save rcPaint
    rcPaint := FWMPaintStruct.rcPaint;
    // Check new display resolution
    ForceRecreate := BitsPixel <> GetDeviceCaps(ScrDC, Windows.BITSPIXEL);
    // Check recreate buffer bitmap
    if ForceRecreate or (Bitmap = 0) or
       (PaintWidth > Width) or (PaintHeight > Height) then begin
     if Bitmap <> 0 then DeleteObject(Bitmap);
     // Check enlarge paint size
     if Width < PaintWidth then Width := PaintWidth;
     if Height < PaintHeight then Height := PaintHeight;
     // Recreate
     TmpDC := GetDC(0);
     //Bitmap := CreateBitmap(Width, Height, 1, 32, Nil);
     Bitmap := CreateCompatibleBitmap(TmpDC, Width, Height);
     BitsPixel := GetDeviceCaps(TmpDC, Windows.BITSPIXEL);
     ReleaseDC(0, TmpDC);
    end;
    // Check create buffer DC
    if DC = 0 then DC := CreateCompatibleDC(0);
    // Select bitmap in DC
    SelectObject(DC, Bitmap);
   end;
   FCanvas.Lock;
   try
    // Setup clip rect
    ClipRgn := CreateRectRgn(0, 0, PaintWidth, PaintHeight);
    SelectClipRgn(FPaintCache.DC, ClipRgn);
    DeleteObject(ClipRgn);
    // Setup FCanvas
    FCanvas.Handle := FPaintCache.DC;
    TControlCanvas(FCanvas).UpdateTextFlags;
    // Paint
    AOrigin.x := FOrigin.x + FWMPaintStruct.rcPaint.Left;
    AOrigin.y := FOrigin.y + FWMPaintStruct.rcPaint.Top;
    APaintRect := Rect(0, 0, PaintWidth, PaintHeight);
    //PaintTo(FCanvas, FWMPaintStruct.rcPaint, FOrigin, FScale, Nil,
    //  False, False, False, False);
    PaintTo(FCanvas, APaintRect, AOrigin, FScale, Nil,
      False, False, False, False);
   finally
    //FCanvas.Handle := 0;
    FCanvas.Unlock;
    SetRectEmpty(FPaintCache.rcPaint);
   end;
   with FWMPaintStruct.rcPaint do
    BitBlt(ScrDC, Left, Top, Right, Bottom, FPaintCache.DC, 0, 0, SRCCOPY);
  finally
   // End paint
   FWMPaintProcessing := false;
   if ScrDC <> 0 then EndPaint(Handle, FWMPaintStruct);
  end;
 end;
 Message.Result := 0;
end;

procedure TFlexPanel.PaintTo(ACanvas: TCanvas; const APaintRect: TRect;
  const AOrigin: TPoint; AScale: integer; AControl: TFlexControl;
  Clipping, ChildrenOnly, SelectedOnly, ForExport: boolean;
  AUseOriginalBezier: boolean = false; AUseImageClipTransparent: boolean = false);
var
  R: TRect;
  ClipRgn, OldClipRgn: HRGN;
  NeedPaintFrame, IsClipped: boolean;
  DocScaledSize: TPoint;
  OldBrushOrigin: TPoint;
  RestoreBrushOrigin: boolean;
  PS: PFlexPaintStruct;
  IsFirstPaint: boolean;
begin
 if csDestroying in ComponentState then exit;
 RestoreBrushOrigin := False;
 PS := FPaintList.BeginPaint(ACanvas);
 try
  ACanvas.Lock;
  FPaintForExport := ForExport;
  if PS.IsFirstPaint then begin
   FInPaintOrigin := FOrigin;
   FInPaintScale := FScale;
  end;
  FPaintRect := APaintRect;
  with FPaintRect do begin
   FPaintWidth := Right - Left;
   FPaintHeight := Bottom - Top;
  end;
  FUseOriginalBezier := AUseOriginalBezier;
  FUseImageClipTransparent := AUseImageClipTransparent;
  //CanvasRect := Rect(0, 0, FPaintWidth, FPaintHeight);
  //OffsetRect(CanvasRect, APaintRect.Left, APaintRect.Top);
  ControlState := ControlState + [csCustomPaint];
  FOrigin.X := AOrigin.X - FPaintRect.Left;
  FOrigin.Y := AOrigin.Y - FPaintRect.Top;
  FScale := AScale;
  SetBrushOrgEx(ACanvas.Handle, -FOrigin.X, -FOrigin.Y, @OldBrushOrigin);
  RestoreBrushOrigin := True;
  DocScaledSize.X := ScaleValue(FDocWidth, FScale);
  DocScaledSize.Y := ScaleValue(FDocHeight, FScale);
  if not ForExport and PS.IsFirstPaint and (FToolMode = ftmPanning) and
    Assigned(FFrostPanImage) then begin
   R := Rect(0, 0, FFrostPanImage.Width, FFrostPanImage.Height);
   OffsetRect(R, -FOrigin.X - FHorzExtraSpace, -FOrigin.Y - FVertExtraSpace);
   if not FFrostPanFullDoc then
    OffsetRect(R, FMouseAnchor.X, FMouseAnchor.Y);
   with ACanvas do begin
    Draw(R.Left, R.Top, FFrostPanImage);
    if not FFrostPanFullDoc then begin
     // Fill non-invalidated space
     Brush.Assign(Self.Brush);
     with FPaintRect do begin
      if Top < R.Top then FillRect(Rect(Left, Top, Right, R.Top));
      if Left < R.Left then FillRect(Rect(Left, R.Top, R.Left, Bottom));
      if Right > R.Right then FillRect(Rect(R.Right, R.Top, Right, Bottom));
      if Bottom > R.Bottom then
       FillRect(Rect(R.Left, R.Bottom, R.Right, Bottom));
     end;
    end;
   end;
   Exit;
  end;
  with ACanvas do begin
   NeedPaintFrame := not FSchemeBkStretch and DocFrameVisible;
   OldClipRgn := 0;
   ClipRgn := 0;
   IsClipped := False;
   try
    // Fill background first
    if not ForExport then begin
     //Brush.Color := Self.Color;
     //Brush.Style := bsSolid;
     Brush.Assign(Self.Brush);
     FillRect(FPaintRect)
    end;
    // Setup clip region
    if FDocClipping then begin
     R := Rect(0, 0, DocScaledSize.X, DocScaledSize.Y);
     OffsetRect(R, -FOrigin.X, -FOrigin.Y);
     IsClipped := true;
    end;
    if Clipping then begin
     if IsClipped then
      IntersectRect(R, R, FPaintRect)
     else begin
      R := FPaintRect;
      IsClipped := true;
     end;
    end;
    if IsClipped then begin
     OldClipRgn := CreateRectRgnIndirect(R);
     if GetClipRgn(ACanvas.Handle, OldClipRgn) <> 1 then begin
      DeleteObject(OldClipRgn);
      OldClipRgn := 0;
     end;
     ClipRgn := CreateRectRgnIndirect(R);
     SelectClipRgn(ACanvas.Handle, ClipRgn);
    end;
    // Paint document frame
    if not ForExport then begin
     if NeedPaintFrame then begin
      if FDocShadowColor <> clNone then begin
       Brush.Color := FDocShadowColor;
       R := Rect(5, DocScaledSize.Y+1, DocScaledSize.X+6, DocScaledSize.Y+6);
       OffsetRect(R, -FOrigin.X, -FOrigin.Y);
       FillRect(R);
       R := Rect(DocScaledSize.X+1, 5, DocScaledSize.X+6, DocScaledSize.Y+6);
       OffsetRect(R, -FOrigin.X, -FOrigin.Y);
       FillRect(R);
      end;
      if FDocFrameColor <> clNone then begin
       Pen.Color := FDocFrameColor;
       Pen.Style := psSolid;
       Pen.Width := 1;
       Pen.Mode := pmCopy;
       Brush.Style := bsClear;
       R := Rect(-1, -1, DocScaledSize.X+1, DocScaledSize.Y+1);
       OffsetRect(R, -FOrigin.X, -FOrigin.Y);
       Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;
     end;
    end;
    // Paint document controls
    Paint(ACanvas, AControl, ChildrenOnly, SelectedOnly);
    // Paint document outside area
    if not ForExport and FDocSpaceFill {and NeedPaintFrame} then begin
     Brush.Assign(FDocSpaceBrush);
     Pen.Style := psClear;
     Pen.Mode := pmCopy;
     with FOrigin, FPaintRect do begin
      R.Right := -X + DocScaledSize.X;
      R.Bottom := -Y + DocScaledSize.Y;
      Rectangle(Left, Top, Right+1, -Y+1);
      Rectangle(Left, -Y, -X+1, R.Bottom+1);
      Rectangle(R.Right, -Y, Right+1, R.Bottom+1);
      Rectangle(Left, R.Bottom, Right+1, Bottom+1);
     end;
    end;
   finally
    if IsClipped then begin
     if OldClipRgn <> 0 then begin
      SelectClipRgn(Handle, OldClipRgn);
      DeleteObject(OldClipRgn);
     end else
      SelectClipRgn(Handle, 0);
     DeleteObject(ClipRgn);
    end;
   end;
  end;
 finally
  if RestoreBrushOrigin then
   SetBrushOrgEx(ACanvas.Handle, OldBrushOrigin.X, OldBrushOrigin.Y, Nil);
  ACanvas.Unlock;
  IsFirstPaint := PS.IsFirstPaint;
  FPaintList.EndPaint(PS);
  if IsFirstPaint then begin
   FOrigin := FInPaintOrigin;
   FScale := FInPaintScale;
   FPaintWidth := 0;
   FPaintHeight := 0;
   SetRectEmpty(FPaintRect);
   FPaintForExport := False;
   FUseOriginalBezier := False;
   FUseImageClipTransparent := false;
  end;
  ControlState := ControlState - [csCustomPaint];
 end;
end;

procedure TFlexPanel.PaintEmptyPicture(ACanvas: TCanvas; 
  APicture: TFlexControl);
var R: TRect;
begin
 with ACanvas do begin
  Pen.Color := clBlack;
  Pen.Style := psSolid;
  Pen.Mode := pmCopy;
  Brush.Style := bsClear;
  R := APicture.PaintRect;
  with R do Rectangle(Left, Top, Right, Bottom);
  if (RectWidth(R) > 1) and (RectHeight(R) > 1) then begin
   MoveTo(R.Left, R.Top);
   LineTo(R.Right-1, R.Bottom-1);
   MoveTo(R.Right-1, R.Top);
   LineTo(R.Left, R.Bottom-1);
  end;
 end;
end;

procedure TFlexPanel.InvalidateDocRect(const DocRect: TRect);
var R, OldRect: TRect;
begin
 if not Assigned(Parent) or IsRectEmpty(DocRect) then exit;
 CopyRect(R, DocRect);
 TransformRect(R);
 // Union with previous rect
 GetUpdateRect(Handle, OldRect, False);
 if not IsRectEmpty(OldRect) then UnionRect(R, R, OldRect);
 // Do invalidate
 InvalidateRect(Handle, @R, False);
end;

function TFlexPanel.InvalidateControl(AControl: TFlexControl): boolean;
var R, OldRect: TRect;
begin
 Result :=
   Assigned(Parent) and (AControl.UpdateCounter = 0) and
   ( (AControl.Parent = Schemes) or
     (Assigned(ActiveScheme) and (AControl.ParentScheme = ActiveScheme)) );
 if Result and not AControl.NonVisual then begin
  with AControl.DocRect do R := AControl.GetRefreshRect(Left, Top);
  TransformRect(R);
  // Union with previous rect
  GetUpdateRect(Handle, OldRect, False);
  if not IsRectEmpty(OldRect) then UnionRect(R, R, OldRect);
  // Invalidate rect
  InvalidateRect(Handle, @R, False)
 end;
end;

procedure TFlexPanel.InvalidateSelection;
var R: TRect;
begin
 if IsRectEmpty(FSelRect) or not HandleAllocated then exit;
 R := FSelRect;
 TransformRect(R);
 InflateRect(R, SelectionInflate, SelectionInflate);
 InvalidateRect(Handle, @R, false);
end;

function TFlexPanel.GetSelResizeCursor(PaintX, PaintY: integer): TResizeCursor;
var R: TRect;
    Rgn: HRGN;
    W3, H3: integer;
begin
 Result := rcrNone;
 Rgn := CreateSelMarkersRgn;
 if Rgn <> 0 then
 try
  if not PtInRegion(Rgn, PaintX, PaintY) then exit;
  CopyRect(R, FSelRect);
  TransformRect(R);
  InflateRect(R, SelectionMarkerSize, SelectionMarkerSize);
  with R do begin
   W3 := (Right - Left) div 3;
   H3 := (Bottom - Top) div 3;
   if PaintY < Top + H3 then begin
    if PaintX < Left + W3  then Result := rcrTopLeft else
    if PaintX > Right - W3 then Result := rcrTopRight
                           else Result := rcrTop;
   end else
   if PaintY > Bottom - H3 then begin
    if PaintX < Left + W3  then Result := rcrBottomLeft else
    if PaintX > Right - W3 then Result := rcrBottomRight
                           else Result := rcrBottom;
   end else
   begin
    if PaintX < Left + W3  then Result := rcrLeft else
    if PaintX > Right - W3 then Result := rcrRight;
   end;
  end;
 finally
  DeleteObject(Rgn);
 end;
end;

function TFlexPanel.GetAnchorRect(AControl: TFlexControl;
  Inflate: integer = 0): TRect;
begin
 if AControl.AnchorEnabled then with AControl.AnchorPoint do begin
  Result.Left := X - AnchorPointSize div 2 -Inflate;
  Result.Top := Y - AnchorPointSize div 2 -Inflate;
  Result.Right := Result.Left + AnchorPointSize +2*Inflate;
  Result.Bottom := Result.Top + AnchorPointSize +2*Inflate;
 end else
  SetRectEmpty(Result);
end;

function TFlexPanel.GetEditPointRect(Index: integer;
  Inflate: integer = 0): TRect;
var P: TPoint;
begin
 if Assigned(FEditPointControl) then begin
  with FEditPointControl.DocRect do begin
   P := FEditPointControl.Points[Index];
   inc(P.X, Left);
   inc(P.Y, Top);
  end;
  TransformPoint(P.X, P.Y);
  with P do begin
   Result.Left := X - AnchorPointSize div 2 -Inflate;
   Result.Top := Y - AnchorPointSize div 2 -Inflate;
   Result.Right := Result.Left + AnchorPointSize +2*Inflate;
   Result.Bottom := Result.Top + AnchorPointSize +2*Inflate;
  end
 end else
   SetRectEmpty(Result);
end;

procedure TFlexPanel.SetEditPointControl(Value: TFlexControl);
begin
 if Assigned(Value) and (Value.PointCount = 0) then Value := Nil;
 if Value = FEditPointControl then exit;
{ if not Assigned(FEditPointControl) or not Assigned(Value) then begin
  if FPointsRgn <> 0 then begin
   if Assigned(Parent) then InvalidateRgn(Handle, FPointsRgn, False);
   DeleteObject(FPointsRgn);
   FPointsRgn := 0;
  end;
 end; }
 FEditPointControl := Value;
 UpdatePoints;
end;

function TFlexPanel.IsEditPointsVisible: boolean;
begin
 Result := Assigned(FEditPointControl) and (FDefaultToolMode = ftmPointEdit);
end;

function TFlexPanel.DoPointEdit(Control: TFlexControl;
  Index, DocX, DocY: integer): boolean;
begin
 Result := False;
 if not Assigned(Control) then exit;
 FEditPointControl := Control;
 with Control.DocRect, Control.Points[Index] do begin
  FMoveStart.X := Left + X;
  FMoveStart.Y := Top + Y;
  FMouseAnchor.X := DocX - FMoveStart.X;
  FMouseAnchor.Y := DocY - FMoveStart.Y;
  FEditPointIndex := Index;
  if FEditPointControl.PointTypes[Index] <> ptControl then SelectPoint(Index);
  ToolMode := ftmPointEditing;
 end;
 Result := True;
end;

function TFlexPanel.UpdatePointGuide(Region: HRGN = 0): boolean;
var Range: TRect;
    AnchorHalf: integer;
    Rgn: HRGN;
begin
 Result := FShowEditPointGuide and (FEditPointGuide.Count > 0);
 if not Result then exit;
 with FEditPointGuide do begin
  if not CalcPath(Points, Types, Range) then exit;
  with Range do begin
   TopLeft := FEditPointControl.ClientToOwner(TopLeft);
   BottomRight := FEditPointControl.ClientToOwner(BottomRight);
  end;
  AnchorHalf := AnchorPointSize div 2 + AnchorPointSize and 1;
  InflateRect(Range, AnchorHalf, AnchorHalf);
  if Region = 0 then begin
   if Assigned(Parent) then InvalidateRect(Handle, @Range, False);
  end else begin
   Rgn := CreateRectRgnIndirect(Range);
   CombineRgn(Region, Region, Rgn, RGN_OR);
   DeleteObject(Rgn);
  end;
 end;
end;

function TFlexPanel.UpdatePointTangents(Region: HRGN = 0): boolean;
var Rects: TRectArray;
    APointsRgn: HRGN;
    i, Index: integer;
    PrevNode, NextNode: integer;
    Ofs: TPoint;

 procedure Combine(const p0, p1: TPoint);
 var TempRgn: HRGN;
     pt0, pt1: TPoint;
     R: TRect;
 begin
  // Transform points
  pt0.x := p0.x + Ofs.x;
  pt0.y := p0.y + Ofs.y;
  TransformPoint(pt0.x, pt0.y);
  pt1.x := p1.x + Ofs.x;
  pt1.y := p1.y + Ofs.y;
  TransformPoint(pt1.x, pt1.y);
  // Normalize points coords
  if pt0.x < pt1.x then begin
   R.Left := pt0.x - AnchorPointSize div 2;
   R.Right := pt1.x + AnchorPointSize div 2 + AnchorPointSize and 1;
  end else begin
   R.Left := pt1.x - AnchorPointSize div 2;
   R.Right := pt0.x + AnchorPointSize div 2 + AnchorPointSize and 1;
  end;
  if pt0.y < pt1.y then begin
   R.Top := pt0.y - AnchorPointSize div 2;
   R.Bottom := pt1.y + AnchorPointSize div 2 + AnchorPointSize and 1;
  end else begin
   R.Top := pt1.y - AnchorPointSize div 2;
   R.Bottom := pt0.y + AnchorPointSize div 2 + AnchorPointSize and 1;
  end;
  // Create tangent region
  TempRgn := CreateRectRgnIndirect(R);
  if APointsRgn = 0 then
   APointsRgn := TempRgn
  else begin
   CombineRgn(APointsRgn, APointsRgn, TempRgn, RGN_OR);
   DeleteObject(TempRgn);
  end;
  Result := true;
 end;

begin
 Result := false;
 Rects := Nil;
 if FIsPointAlter or (FEditPointSelCount <> 1) then exit;
 APointsRgn := Region;
 try
  // Find selected point
  Index := -1;
  for i:=0 to Length(FEditPointSelected)-1 do
   if FEditPointSelected[i] then begin
    Index := i;
    break;
   end;
  if Index < 0 then exit;
  FindControlPoints(Index, PrevNode, NextNode);
  with FEditPointControl.DocRect do begin
   Ofs.X := Left;
   Ofs.Y := Top;
  end;
  with FEditPointControl do begin
   if PrevNode >= 0 then begin
    Combine(Points[PrevNode], Points[PrevNode+1]);
    Combine(Points[PrevNode+2], Points[Index]);
   end;
   if NextNode >= 0 then begin
    Combine(Points[Index], Points[Index+1]);
    Combine(Points[Index+2], Points[NextNode]);
   end;
  end;
 finally
  if (APointsRgn <> 0) and (Region = 0) then DeleteObject(APointsRgn);
 end;
end;

procedure TFlexPanel.UpdatePoints;
var APointsRgn, TempRgn: HRGN;
    i, Count: integer;
    R: TRect;
    UseAnchor: boolean;
    GroupRect: TRect;
begin
 APointsRgn := 0;
 try
  Count := 0;
  UseAnchor := true;
  // Check selected edit points
  if Assigned(FEditPointControl) then begin
   if FEditPointControl.PointCount <> Length(FEditPointSelected) then begin
    SetLength(FEditPointSelected, FEditPointControl.PointCount);
    FillChar(FEditPointSelected[0], Length(FEditPointSelected), 0);
    FEditPointSelCount := 0;
   end;
   if IsEditPointsVisible then begin
    // Create region for edit points
    UseAnchor := false;
    Count := FEditPointControl.PointCount;
   end;
  end else begin
   // Reset selected points array
   SetLength(FEditPointSelected, 0);
   FEditPointSelCount := 0;
  end;
  // Check use anchor points for region
  if UseAnchor then Count := FSelList.Count;
  FPointsRgnExact := Count <= MaxSinglePointsInvalidation;
  FillChar(GroupRect, SizeOf(GroupRect), 0);
  // Create points region
  for i:=0 to Count-1 do begin
   if UseAnchor then
    R := GetAnchorRect(TFlexControl(FSelList[i]), SelectionPointInflate)
   else
   if FEditPointControl.PointTypes[i] <> ptControl then
    R := GetEditPointRect(i, SelectionPointInflate)
   else
    continue;
   if IsRectEmpty(R) then continue;
   if not FPointsRgnExact then begin
    if i = 0 then
     // Init GroupRect
     GroupRect := R
    else
     // Union rects
     UnionRect(GroupRect, GroupRect, R);
   end else begin
    TempRgn := CreateRectRgnIndirect(R);
    if APointsRgn = 0 then
     APointsRgn := TempRgn
    else begin
     CombineRgn(APointsRgn, APointsRgn, TempRgn, RGN_OR);
     DeleteObject(TempRgn);
    end;
   end;
  end;
  if not FPointsRgnExact then APointsRgn := CreateRectRgnIndirect(GroupRect);
  if not UseAnchor then begin
   // Guide and/or Tangents included in APointsRgn
   if UpdatePointGuide(APointsRgn) then FPointsRgnExact := false;
   if UpdatePointTangents(APointsRgn) then FPointsRgnExact := false;
  end;
  if FPointsRgnExact and EqualRgn(FPointsRgn, APointsRgn) then exit;
  if FPointsRgn <> 0 then begin
   if Assigned(Parent) then InvalidateRgn(Handle, FPointsRgn, False);
   DeleteObject(FPointsRgn);
   FPointsRgn := 0;
  end;
  if APointsRgn <> 0 then begin
   FPointsRgn := APointsRgn;
   APointsRgn := 0;
   if Assigned(Parent) then InvalidateRgn(Handle, FPointsRgn, False);
   { //// DEBUG /////
   i := GetDC(Handle);
   InvertRgn(i, FPointsRgn);
   ReleaseDC(Handle, i);
   { //////////////// }
  end;
 finally
  if APointsRgn <> 0 then DeleteObject(APointsRgn);
 end;
end;

function TFlexPanel.GetEditPointsCaps: TPathEditFuncs;
begin
 if Assigned(FEditPointControl)
  then Result := FEditPointControl.EditPointsCaps(FEditPointSelected)
  else Result := [];
end;

function TFlexPanel.EditPoints(Func: TPathEditFunc;
  Params: PPathEditParams = Nil): boolean;
begin
// Result := Func in EditPointsCaps;
// if not Result then exit;
 FEditPointControl.BeginUpdate;
 try
  Result := FEditPointControl.EditPoints(Func, FEditPointSelected);
 finally
  FEditPointControl.EndUpdate;
 end;
end;

function TFlexPanel.BreakApartSelected: boolean;
var i, j, NewCount: integer;
    Info: TPathInfo;
    Control: TFlexControl;
    OrigPoints, NewPoints: TPointArray;
    OrigTypes, NewTypes: TPointTypeArray;
    MS: TMemoryStream;
    Filer: TFlexFiler;
begin
 Result := false;
 MS := Nil;
 Filer := Nil;
 BeginSelectionUpdate(false, TPanelPointsHistoryGroup);
 try
  for i:=0 to FSelList.Count-1 do with TFlexControl(FSelList[i]) do begin
   if (PointCount = 0) or (Length(PointsInfo.Figures) < 2) then continue;
   if not Assigned(MS) then MS := TMemoryStream.Create;
   if not Assigned(Filer) then Filer := TFlexFiler.Create(MS);
   SaveToFiler(Filer, '');
   Info := PointsInfo^;
   GetPointsEx(OrigPoints, OrigTypes);
   for j:=0 to Length(Info.Figures)-1 do with Info.Figures[j] do begin
    if j = 0 then
     Control := TFlexControl(FSelList[i])
    else begin
     Control := TFlexControlClass(ClassType).Create(Owner, Parent, Layer);
     // Rewind filer
     Filer.Rewind;
     // Skip object name
     Filer.LoadStr;
     // Load control properties
     Control.LoadFromFiler(Filer);
     // Resolve all ref props
     Control.Owner.PropRefList.ResolveAllRefs;
    end;
    NewCount := LastPoint - FirstNode +1;
    SetLength(NewPoints, NewCount);
    Move(OrigPoints[FirstNode], NewPoints[0], NewCount*SizeOf(NewPoints[0]));
    SetLength(NewTypes, NewCount);
    Move(OrigTypes[FirstNode], NewTypes[0], NewCount*SizeOf(NewTypes[0]));
    Control.SetPointsEx(NewPoints, NewTypes);
    Select(Control);
    Result := true;
   end;
  end;
 finally
  EndSelectionUpdate(false, TPanelPointsHistoryGroup);
  MS.Free;
  Filer.Free;
 end;
end;

function TFlexPanel.CombineSelected: boolean;
var i, j, NewCount: integer;
    Dest: TFlexControl;
    NewPoints: TPointArray;
    NewTypes: TPointTypeArray;
    Offset_: TPoint;
begin
 Result := false;
 BeginSelectionUpdate(false, TPanelPointsHistoryGroup);
 try
  NewCount := 0;
  Dest := Nil;
  for i:=0 to FSelList.Count-1 do with TFlexControl(FSelList[i]) do begin
   if PointCount = 0 then continue;
   if not Assigned(Dest) then begin
    // First selected curve - destination of combine
    Dest := TFlexControl(FSelList[i]);
    GetPointsEx(NewPoints, NewTypes);
    with DocRect do begin
     Offset_.X := Left;
     Offset_.Y := Top;
    end;
    NewCount := PointCount;
   end else begin
    // Add next selected curve to dest curve points
    SetLength(NewPoints, NewCount+PointCount);
    SetLength(NewTypes, NewCount+PointCount);
    with DocRect do
     for j:=0 to PointCount-1 do with Points[j] do begin
      NewPoints[NewCount+j].x := x + Left - Offset_.x;
      NewPoints[NewCount+j].y := y + Top - Offset_.y;
      NewTypes[NewCount+j] := PointTypes[j];
     end;
    NewCount := Length(NewPoints);
    Result := true;
   end;
  end;
  if Result then begin
   Unselect(Dest);
   DeleteSelected;
   Dest.SetPointsEx(NewPoints, NewTypes);
   Select(Dest);
  end;
 finally
  EndSelectionUpdate(false, TPanelPointsHistoryGroup);
 end;
end;

function TFlexPanel.FlattenSelected(const Curvature: single): boolean;
var i: integer;
begin
 Result := false;
 BeginSelectionUpdate;
 try
  for i:=0 to FSelList.Count-1 do with TFlexControl(FSelList[i]) do begin
   if PointCount = 0 then continue;
   if not FlattenPoints(Curvature) then break;
   Result := true;
  end;
 finally
  EndSelectionUpdate;
 end;
end;

procedure TFlexPanel.DeleteSelectedPoints;
var i: integer;
begin
 if FEditPointSelCount = 0 then exit;
 BeginSelectionUpdate;
 try
  FEditPointControl.BeginUpdate;
  try
   for i:=FEditPointControl.PointCount-1 downto 0 do
    if EditPointSelected[i] then FEditPointControl.DeletePoint(i);
   // Delete all single point figures in control
   with FEditPointControl.PointsInfo^ do
    if PointCount > 1 then
    for i:=Length(Figures)-1 downto 0 do
     with Figures[i] do
      if FirstNode = LastNode then
       FEditPointControl.DeletePoint(FirstNode);
  finally
   FEditPointControl.EndUpdate;
  end;
 finally
  EndSelectionUpdate;
 end;
end;

function TFlexPanel.ConvertSelectedToCurves: boolean;
var i: integer;
    Curve: TFlexControl;
    CurveIndex, SelIndex: integer;
begin
 Result := false;
 BeginSelectionUpdate(false, TPanelCreateControlsHistoryGroup);
 try
  for i:=0 to FSelList.Count-1 do with TFlexControl(FSelList[i]) do begin
   // Create curve
   Curve := CreateCurveControl;
   if not Assigned(Curve) then continue;
   // Change curve order
   if Curve.Parent = Parent then begin
    CurveIndex := Parent.IndexOf(Curve);
    SelIndex := Parent.IndexOf(TFlexControl(FSelList[i]));
    Parent.ChangeOrder(CurveIndex, SelIndex);
   end;
   // Select new curve and delete source control
   FSelList[i] := Curve;
   FSelNeedUpdate := true;
   Free;
  end;
 finally
  EndSelectionUpdate(false, TPanelCreateControlsHistoryGroup);
 end;
end;

function TFlexPanel.GetEditPointSelected(Index: integer): boolean;
begin
 if Assigned(FEditPointSelected)
  then Result := FEditPointSelected[Index]
  else Result := false;
end;

procedure TFlexPanel.SetEditPointSelected(Index: integer; Value: boolean);
var PointRect: TRect;
begin
 if not Assigned(FEditPointControl) then exit;
 //if FEditPointControl.PointTypes[Index] = ptControl then Value := false;
 if not Assigned(FEditPointSelected) or
   (FEditPointSelected[Index] = Value) then exit;
 // Check for connector control - can't select more one point
 if Value and FEditPointControl.IsConnectorControl and
    (FEditPointSelCount <> 0) then exit;
 FEditPointSelected[Index] := Value;
 if Value
  then inc(FEditPointSelCount)
  else dec(FEditPointSelCount);
 if Assigned(Parent) then begin
  PointRect := GetEditPointRect(Index);
  if not IsRectEmpty(PointRect) then InvalidateRect(Handle, @PointRect, False);
  UpdatePointTangents;
 end;
 DoNotify(FEditPointControl, fnSelectPoint);
end;

procedure TFlexPanel.SelectPoint(Index: integer);
begin
 EditPointSelected[Index] := true;
end;

procedure TFlexPanel.UnselectPoint(Index: integer);
begin
 EditPointSelected[Index] := false;
end;

procedure TFlexPanel.UnselectAllPoints;
var i: integer;
begin
 for i:=0 to Length(FEditPointSelected)-1 do EditPointSelected[i] := false;
end;

procedure TFlexPanel.FindControlPoints(PointIndex: integer; out PrevNode,
  NextNode: integer);
var Info: PPathInfo;
    FigIndex: integer;
begin
 PrevNode := -1;
 NextNode := -1;
 if not Assigned(FEditPointControl) or
    (PointIndex < 0) or (PointIndex >= FEditPointControl.PointCount) or
    (FEditPointControl.PointTypes[PointIndex] = ptControl) then exit;
 Info := FEditPointControl.PointsInfo;
 FigIndex := 0;
 while FigIndex < Length(Info.Figures) do with Info.Figures[FigIndex] do
  if (PointIndex >= FirstNode) and (PointIndex <= LastNode)
   then break
   else inc(FigIndex);
 if FigIndex = Length(Info.Figures) then exit;
 with Info.Figures[FigIndex] do begin
  // Define prev index
  if PointIndex = FirstNode then begin
   if IsClosed and (LastNode < LastPoint) then PrevNode := LastNode;
  end else
  if FEditPointControl.PointTypes[PointIndex-1] = ptControl then
   PrevNode := PointIndex-3;
  // Define next index
  if PointIndex = LastNode then begin
   if IsClosed and (LastNode < LastPoint) then NextNode := FirstNode;
  end else
  if FEditPointControl.PointTypes[PointIndex+1] = ptControl then
   NextNode := PointIndex+3;
 end;
end;

function TFlexPanel.FindFirstOrLastNodeIfEqual(Index: integer;
  AControl: TFlexControl): integer;
var FigIndex: integer;
begin
 Result := -1;
 FigIndex := GetFigureIndex(AControl.PointsInfo^, Index);
 if FigIndex < 0 then exit;
 with AControl, PointsInfo.Figures[FigIndex] do begin
  if IsClosed then exit;
  // Find first point (if there several same points on start)
  Result := Index;
  while
    (Result > FirstNode) and (PointTypes[Result-1] = ptNode) and
    (Points[Result].X = Points[Result-1].X) and
    (Points[Result].Y = Points[Result-1].Y) do
   dec(Result);
  if Result = FirstNode then exit;
  // Find last point (if there several same points at the end)
  Result := Index;
  while
    (Result < LastNode) and (PointTypes[Result+1] <> ptControl) and
    (Points[Result].X = Points[Result+1].X) and
    (Points[Result].Y = Points[Result+1].Y) do
   inc(Result);
  if Result < LastNode then Result := -1;
 end;
end;

procedure TFlexPanel.PaintPoints(ACanvas: TCanvas);
var i, Index, Count, OldMode: integer;
    PrevNode, NextNode: integer;
    R: TRect;
    DC: HDC;
    CurvePen: HPen;
    TransPoints: TPointArray;
    Complete: boolean;

 procedure DrawPoint(Index: integer);
 var R: TRect;
     i: integer;
     DoInflate: boolean;
 begin
  // Check for overriden points
  DoInflate := false;
  with TransPoints[Index] do
   for i:=Index-1 downto 0 do
    if (TransPoints[i].x = x) and (TransPoints[i].y = y) then begin
     // Point already drawn
     if FEditPointSelected[i] or not FEditPointSelected[Index] then exit;
     // Need paint selected point with already drawn non-selected point
     DoInflate := true;
     break;
    end;
  // Calc point rectangle
  with TransPoints[Index] do begin
   R.Left := X - AnchorPointSize div 2;
   R.Top := Y - AnchorPointSize div 2;
   R.Right := R.Left + AnchorPointSize;
   R.Bottom := R.Top + AnchorPointSize;
  end;
  // Contarct border for 1 pixel if needed
  if DoInflate then InflateRect(R, -2, -2);
  // Draw point
  if FEditPointSelected[Index]
   then Windows.InvertRect(DC, R)
   else Windows.Rectangle(DC, R.Left, R.Top, R.Right, R.Bottom);
 end;

begin
 TransPoints := Nil;
 DC := ACanvas.Handle;
 OldMode := SetROP2(DC, R2_NOTXORPEN);
 try
  SetBkMode(DC, TRANSPARENT);
  SelectObject(DC, GetStockObject(NULL_BRUSH));
  SetBkColor(DC, clWhite);
  if IsEditPointsVisible then begin
   Count := FEditPointControl.PointCount;
   with FEditPointControl.PaintRect do
    TransPoints :=
      FEditPointControl.GetTransformPoints(Left, Top, FScale);
   if not FIsPointAlter and (FEditPointSelCount = 1) then begin
    // Find selected point
    Index := -1;
    for i:=0 to Length(FEditPointSelected)-1 do
     if FEditPointSelected[i] then begin
      Index := i;
      break;
     end;
    // Process selected point
    FindControlPoints(Index, PrevNode, NextNode);
    // Draw curve tangents
    CurvePen := CreatePen(PS_DOT, 1, clBlue);
    SelectObject(DC, CurvePen);
    if PrevNode >= 0 then begin
     with TransPoints[PrevNode] do MoveToEx(DC, x, y, Nil);
     with TransPoints[PrevNode+1] do LineTo(DC, x, y);
     with TransPoints[PrevNode+2] do MoveToEx(DC, x, y, Nil);
     with TransPoints[Index] do LineTo(DC, x, y);
    end;
    if NextNode >= 0 then begin
     with TransPoints[Index] do MoveToEx(DC, x, y, Nil);
     with TransPoints[Index+1] do LineTo(DC, x, y);
     with TransPoints[Index+2] do MoveToEx(DC, x, y, Nil);
     with TransPoints[NextNode] do LineTo(DC, x, y);
    end;
    SelectObject(DC, GetStockObject(BLACK_PEN));
    DeleteObject(CurvePen);
    // Draw control points
    if PrevNode >= 0 then begin
     DrawPoint(PrevNode+1);
     DrawPoint(PrevNode+2);
    end;
    if NextNode >= 0 then begin
     DrawPoint(Index+1);
     DrawPoint(Index+2);
    end;
   end;
   // Draw points
   SelectObject(DC, GetStockObject(BLACK_PEN));
   for i:=0 to Count-1 do begin
    if FEditPointControl.PointTypes[i] = ptControl then continue;
    // Calc point rect
    DrawPoint(i);
   end;
   with FEditPointGuide do
   if FShowEditPointGuide and (Count > 0) then begin
    // Transform points
    SetLength(PaintPoints, Count);
    for i:=0 to Count-1 do
     PaintPoints[i] := FEditPointControl.ClientToOwner(Points[i]);
    // Draw point guide
    CurvePen := CreatePen(PS_DOT, 1, clBlue);
    try
     SelectObject(DC, CurvePen);
     // Draw guide path
     if CreatePath(DC, PaintPoints, Types, False, True, Complete) then
      StrokePath(DC);
     // Draw guide visible points
     CurvePen := CreatePen(PS_SOLID, 1, clBlue);
     DeleteObject( SelectObject(DC, CurvePen) );
     for i:=0 to Count-1 do
      if Visible[i] then with PaintPoints[i] do begin
       R.Left := X - AnchorPointSize div 2;
       R.Top := Y - AnchorPointSize div 2;
       R.Right := R.Left + AnchorPointSize;
       R.Bottom := R.Top + AnchorPointSize;
       Windows.Rectangle(DC, R.Left, R.Top, R.Right, R.Bottom);
      end;
    finally
     SelectObject(DC, GetStockObject(BLACK_PEN));
     DeleteObject(CurvePen);
    end;
   end;
  end else begin
   // Draw anchor points for selected controls
   Count := FSelList.Count;
   SelectObject(DC, GetStockObject(BLACK_PEN));
   for i:=0 to Count-1 do begin
    R := GetAnchorRect(TFlexControl(FSelList[i]));
    if IsRectEmpty(R) then continue;
    Windows.Rectangle(DC, R.Left, R.Top, R.Right, R.Bottom);
   end;
  end;
 finally
  // Restore canvas
  SetROP2(DC, OldMode);
  ACanvas.Pen.Handle := GetStockObject(BLACK_PEN);
  ACanvas.Brush.Handle := GetStockObject(NULL_BRUSH);
  TransPoints := Nil;
 end;
end;

procedure TFlexPanel.PaintSelection(ACanvas: TCanvas);
var Rgn: HRGN;
begin
 Rgn := CreateSelMarkersRgn;
 if Rgn <> 0 then
 try
  InvertRgn(ACanvas.Handle, Rgn);
 finally
  DeleteObject(Rgn);
 end;
end;

procedure TFlexPanel.SetMarqueeRect(const Value: TRect);
begin
 if EqualRect(Value, FMarqueeRect) then exit;
 InvalidateDocRect(FMarqueeRect);
 CopyRect(FMarqueeRect, Value);
 InvalidateDocRect(FMarqueeRect);
end;

procedure TFlexPanel.PaintMarquee(ACanvas: TCanvas);
var R: TRect;
    DC: HDC;
    Brush: HBrush;
begin
 CopyRect(R, FMarqueeRect);
 TransformRect(R);
 with FMarqueeRect do if (Right - Left < 2) or (Bottom - Top < 2) then exit;
 DC := ACanvas.Handle;
 Brush := SelectObject(DC, GetStockObject(WHITE_BRUSH));
 SetBkColor(DC, clWhite);
 DrawFocusRect(DC, R);
 SelectObject(DC, Brush);
end;

procedure TFlexPanel.PaintLinkPoint(ACanvas: TCanvas);
var DC: HDC;
    OldMode: integer;
    PointsPen, OldPen: HPen;
    TopLeft: TPoint;
    Half: integer;
    Size: integer;
begin
 if FDefaultLinkPoint.Index < 0 then exit;
 ACanvas.Brush.Style := bsClear;
 DC := ACanvas.Handle;
 OldMode := SetROP2(DC, R2_NOTXORPEN);
 try
  PointsPen := CreatePen(PS_SOLID, 1, clBlue);
  OldPen := SelectObject(DC, PointsPen);
  // Draw nearest link point
  TopLeft := Schemes.ClientToOwner(FDefaultLinkPoint.DocPos);
  Size := FDefaultLinkPoint.Size;
  Half := Size div 2;
  //TopLeft := Point(TopLeft.X-Half, TopLeft.Y-Half);
  dec(TopLeft.x, Half);
  dec(TopLeft.y, Half);
  with TopLeft do begin
   MoveToEx(DC, X+1, Y, Nil);
   LineTo(DC, X+Size-1, Y);
   MoveToEx(DC, X+Size-1, Y+1, Nil);
   LineTo(DC, X+Size-1, Y+Size-1);
   MoveToEx(DC, X+1, Y+Size-1, Nil);
   LineTo(DC, X+Size-1, Y+Size-1);
   MoveToEx(DC, X, Y+1, Nil);
   LineTo(DC, X, Y+Size-1);
  end;
  SelectObject(DC, OldPen);
  DeleteObject(PointsPen);
 finally
  // Restore canvas
  SetROP2(DC, OldMode);
 end;
end;

procedure TFlexPanel.Paint(ACanvas: TCanvas; AControl: TFlexControl;
  ChildrenOnly, SelectedOnly: boolean);
var R: TRect;
    i, LayerIdx: integer;
    PO: TPaintOrder;
    Scheme: TFlexCustomScheme;
begin
 if Assigned(FOnPaintScheme) then
  FOnPaintScheme(ACanvas, AControl, ChildrenOnly, SelectedOnly);
 if SelectedOnly then begin
  InitPaintOrder(PO);
  try
   if not Assigned(AControl) then
    Scheme := FActiveScheme
   else
   if AControl is TFlexCustomScheme then
    Scheme := TFlexCustomScheme(AControl)
   else begin
    Scheme := AControl.ParentScheme;
    if not Assigned(Scheme) then Scheme := FActiveScheme;
   end;
   Scheme.CreatePaintOrder(PO);
   for LayerIdx:=0 to High(PO.LayerRefs) do begin
    i := PO.LayerRefs[LayerIdx].First;
    while i >= 0 do with PO.ControlRefs[i] do begin
     if Control.IsSelected then begin
      R := Control.DocRect;
      Control.PaintAll(ACanvas, R.Left, R.Top);
     end;
     i := PO.ControlRefs[i].Next;
    end;
   end;
  finally
   ClearPaintOrder(PO);
  end;
 end else begin
  if not Assigned(AControl) or (AControl.Owner <> Self) then
   AControl := FActiveScheme;
  if not Assigned(AControl) then
   if FSchemes.Count > 0
    then AControl := FSchemes[0]
    else exit;
  R := AControl.DocRect;
  if ChildrenOnly then begin
   // Paint children only
   if AControl is TFlexCustomScheme then begin
    // From scheme. Creation of paint order is necessary
    InitPaintOrder(PO);
    try
     TFlexCustomScheme(AControl).CreatePaintOrder(PO);
     for LayerIdx:=0 to High(PO.LayerRefs) do begin
      i := PO.LayerRefs[LayerIdx].First;
      while i >= 0 do with PO.ControlRefs[i] do begin
       R := Control.DocRect;
       Control.PaintAll(ACanvas, R.Left, R.Top);
       i := PO.ControlRefs[i].Next;
      end;
     end;
    finally
     ClearPaintOrder(PO);
    end;
   end else
    // From some flex-control. All sub controls are located in same layer
    for i:=0 to AControl.Count-1 do with AControl[i] do
     PaintAll(ACanvas, R.Left + Left, R.Top + Top);
  end else
   AControl.PaintAll(ACanvas, R.Left, R.Top);
  if not FPaintForExport then begin
   if FInDesign then begin
    PaintPoints(ACanvas);
    PaintLinkPoint(ACanvas);
    PaintSelection(ACanvas);
   end;
   PaintMarquee(ACanvas);
  end;
 end;
 if Assigned(FOnPaintOver) then
  FOnPaintOver(ACanvas, AControl, ChildrenOnly, SelectedOnly);
end;

procedure TFlexPanel.SetToolMode(const Value: TFlexToolMode);
var i, NewScale, MinSize: integer;
    SelRect, ZoomRect: TRect;
    Size_, Trans, ZoomDelta, Disp: TPoint;
    CoeffX, CoeffY: double;
    PanImage: TBitmap;
begin
 if FToolMode = Value then exit;
 // Check new tool mode
 case Value of
  ftmCurveMoving:
    if not FIsOverCurveSegment and not FIsOverSegment then exit;
 end;
 // Defing current mouse position
 Trans := FLastMousePos;
 UnTransformPoint(Trans.X, Trans.Y);
 // Finit last tool mod
 case FToolMode of
  ftmSelecting:
    if not Assigned(FEditPointControl) then begin
     SelRect := MarqueeRect;
     BeginSelectionUpdate;
     try
      if Assigned(FActiveScheme) then
      for i:=0 to FActiveScheme.Count-1 do
       with FActiveScheme[i], DocRect do
        if ((Left >= SelRect.Left) and (Top >= SelRect.Top) and
            (Right <= SelRect.Right) and (Bottom <= SelRect.Bottom)) or
           (FSelectPartialOverlapped and
            (Left < SelRect.Right) and (Right > SelRect.Left) and
            (Top < SelRect.Bottom) and (Bottom > SelRect.Top)) then
         FActiveScheme[i].IsSelected := not FActiveScheme[i].IsSelected;
     finally
      EndSelectionUpdate;
     end;
    end;
  ftmPointSelecting:
    begin
     SelRect := MarqueeRect;
     MinSize := UnScaleValue(SelectionThreshold, FScale);
     if (RectWidth(SelRect) < MinSize) and
        (RectHeight(SelRect) < MinSize) then begin
      // Check select new curve control
      if FEditPointControl <> FMouseControl then begin
       UnselectAll;
       Select(FMouseControl);
      end;
     end else
     if Assigned(FEditPointControl) then begin
      // Invert selected points
      with FEditPointControl.DocRect do OffsetRect(SelRect, -Left, -Top);
      BeginSelectionUpdate;
      try
       for i:=0 to FEditPointControl.PointCount-1 do
        if FEditPointControl.PointTypes[i] <> ptControl then
        with FEditPointControl.Points[i] do
         if (X >= SelRect.Left) and (Y >= SelRect.Top) and
            (X <= SelRect.Right) and (Y <= SelRect.Bottom) then
           EditPointSelected[i] := not EditPointSelected[i];
      finally
       EndSelectionUpdate;
      end;
     end;
    end;
  ftmCreating,
  ftmResizing:
    begin
     for i:=0 to FSelList.Count-1 do
      TFlexControl(FSelList[i]).FinishResizing;
     if Assigned(FCreateInDesignAction) then begin
      FHistory.EndPanelGroup(
        THistoryGroupClass(FCreateInDesignAction.ClassType));
      FCreateInDesignAction := Nil;
     end else
      FHistory.EndPanelGroup(TPanelSizeMoveHistoryGroup);
    end;
  ftmMoving:
    if Assigned(FCreateInDesignAction) then begin
     FHistory.EndPanelGroup(
       THistoryGroupClass(FCreateInDesignAction.ClassType));
     FCreateInDesignAction := Nil;
    end else
     FHistory.EndPanelGroup(TPanelSizeMoveHistoryGroup);
  ftmCurveMoving,
  ftmPointEditing:
    begin
     if Assigned(FEditPointControl) then FEditPointControl.EndPointsDesigning;
     if FToolMode = ftmPointEditing then FEditPointIndex := -1;
     if Assigned(FCreateInDesignAction) then begin
      FHistory.EndPanelGroup(
        THistoryGroupClass(FCreateInDesignAction.ClassType));
      FCreateInDesignAction := Nil;
     end else
      FHistory.EndPanelGroup(TPanelPointsHistoryGroup);
    end;
  ftmZooming:
    begin
     ZoomRect := MarqueeRect;
     with ZoomRect do begin
      Size_.X := ScaleValue(Right - Left, FScale);
      Size_.Y := ScaleValue(Bottom - Top, FScale);
     end;
     ZoomDelta.X := Abs(FLastMousePos.X - FZoomMouseStart.X);
     ZoomDelta.Y := Abs(FLastMousePos.Y - FZoomMouseStart.Y);
     if ((ZoomDelta.X < 3) and (ZoomDelta.Y < 3)) or not FZoomingIn then begin
      if FZoomingIn
        then NewScale := FScale * 2
        else NewScale := FScale div 2;
     end else begin
      if Size_.X > 0
       then CoeffX := ClientWidth / Size_.X
       else CoeffX := 1;
      if Size_.Y > 0
       then CoeffY := ClientHeight / Size_.Y
       else CoeffY := 1;
      if CoeffX < CoeffY
       then NewScale := Round(FScale * CoeffX)
       else NewScale := Round(FScale * CoeffY);
     end;
     Zoom(NewScale, @ZoomRect);
    end;
  ftmPanning:
    if Assigned(FFrostPanImage) then begin
      FreeAndNil(FFrostPanImage);
      Invalidate;
    end;
 end;
 FToolMode := Value;
 // Init new tool mode
 case FToolMode of
  ftmPointEdit:
    begin
     MarqueeRect := Rect(0, 0, 0, 0);
     FDefaultToolMode := ftmPointEdit;
     if FSelList.Count = 1 then begin
      if FEditPointControl <> FSelList[0] then begin
       EditPointControl := TFlexControl(FSelList[0]);
       //if FEditPointControl <> TFlexControl(FSelList[0]) then UnselectAll;
      end else
       UpdatePoints;
     end else
      UnselectAll;
    end;
  ftmCurveMoving:
    begin
     FMoveStart := Trans;
     with FOverCurveNearest do begin
      FMouseAnchor.X := Trans.x - Point.x;
      FMouseAnchor.Y := Trans.y - Point.y;
     end;
     if Assigned(FEditPointControl) then begin
      FEditPointControl.BeginPointsDesigning;
      FHistory.BeginPanelGroup(TPanelPointsHistoryGroup);
     end;
    end;
  ftmPointEditing:
    if Assigned(FEditPointControl) then begin
     FEditPointControl.BeginPointsDesigning;
     if not Assigned(FCreateInDesignAction) then
      FHistory.BeginPanelGroup(TPanelPointsHistoryGroup);
    end;
  ftmPointSelecting,
  ftmSelecting,
  ftmMoving:
    begin
     if FToolMode = ftmMoving then begin
      Disp.X := FSelRect.Left;
      Disp.Y := FSelRect.Top;
      FLastMovedPos := Trans;
      FHistory.BeginPanelGroup(TPanelSizeMoveHistoryGroup);
     end else begin
      Disp.X := 0;
      Disp.Y := 0;
     end;
     FMouseAnchor.X := Trans.x - Disp.X;
     FMouseAnchor.Y := Trans.y - Disp.Y;
     FMoveStart.X := Disp.X;
     FMoveStart.Y := Disp.Y;
    end;
  ftmZooming:
    begin
     FMouseAnchor.X := Trans.x;
     FMouseAnchor.Y := Trans.y;
     FZoomMouseStart := Point(FLastMousePos.X, FLastMousePos.Y);
     MarqueeRect := Rect(Trans.x, Trans.y, Trans.x, Trans.y);
    end;
  ftmPanning:
    begin
     FMouseAnchor := FOrigin;
     inc(FMouseAnchor.X, FHorzExtraSpace);
     inc(FMouseAnchor.Y, FVertExtraSpace);
     FMoveStart := FLastMousePos;
     if FFrostPan then begin
      FreeAndNil(FFrostPanImage);
      PanImage := TBitmap.Create;
      try
       if FFrostPanFullDoc then begin
        SelRect.Left := 0;
        SelRect.Top := 0;
        SelRect.Right := ScaleValue(DocWidth, FScale) + 2 * FHorzExtraSpace;
        SelRect.Bottom := ScaleValue(DocHeight, FScale) + 2 * FVertExtraSpace;
        Disp := Point(-FHorzExtraSpace, -FVertExtraSpace);
       end else begin
        SelRect := ClientRect;
        Disp := FOrigin;
       end;
       with SelRect do begin
        PanImage.Width := Right - Left;
        PanImage.Height := Bottom - Top;
       end;
       PaintTo(PanImage.Canvas, SelRect, Disp, FScale, Nil,
         False, False, False, False, False, False);
       FFrostPanImage := PanImage;
      except
       PanImage.Free;
       raise;
      end;
     end;
    end;
  ftmZoom,
  ftmSelect:
    begin
     MarqueeRect := Rect(0, 0, 0, 0);
     if FToolMode = ftmSelect then begin
      FDefaultToolMode := ftmSelect;
      UpdatePoints;
     end;
    end;
  ftmCreating,
  ftmResizing:
    begin
     for i:=0 to FSelList.Count-1 do
      TFlexControl(FSelList[i]).StartResizing(FSelRect);	  	
     FResizeRect := FSelRect;
     if FToolMode = ftmCreating then begin
      FMouseAnchor.X := Trans.x + (FSelRect.Right - FSelRect.Left);
      FMouseAnchor.Y := Trans.y + (FSelRect.Bottom - FSelRect.Top);
     end else 
      FMouseAnchor := Trans;
     if not Assigned(FCreateInDesignAction) then
      FHistory.BeginPanelGroup(TPanelSizeMoveHistoryGroup);
    end;
 end;
 UpdateCursor;
 if Assigned(FOnToolMode) then FOnToolMode(Self);
end;

procedure TFlexPanel.UpdateCursor;
var NewCursor: TCursor;
begin
 NewCursor := crDefault;
 case FToolMode of
  ftmCreating,
  ftmSelect:
    {if Assigned(FEditPointControl) then
     NewCursor := crShapeCursor
    else                                }
    if Assigned(FCreatingControlClass) then
     NewCursor := FCreatingControlClass.CursorInCreate
    else
    if not FInDesign and
       Assigned(FMouseControl) and Assigned(FMouseControl.Reference) then
     NewCursor := crHandPoint
    else
     NewCursor := crDefault;
  ftmZoom,
  ftmZooming:
    begin
     FZoomingIn := GetAsyncKeyState(VK_CONTROL) = 0;
     if FZoomingIn
      then NewCursor := crZoomInCursor
      else NewCursor := crZoomOutCursor;
    end;
  ftmPan: NewCursor := crPanCursor;
  ftmPanning: NewCursor := crPanningCursor;
  ftmMove,
  ftmMoving: NewCursor := crSizeAll;
  ftmResize,
  ftmResizing:
    case FResizeCursor of
     rcrTopLeft,
     rcrBottomRight : NewCursor := crSizeNWSE;
     rcrTop,
     rcrBottom      : NewCursor := crSizeNS;
     rcrTopRight,
     rcrBottomLeft  : NewCursor := crSizeNESW;
     rcrLeft,
     rcrRight       : NewCursor := crSizeWE;
     else             NewCursor := crDefault;
    end;
 // ftmCreating: NewCursor := crSizeNWSE;
  ftmPointSelecting:
    NewCursor := crShapeCursor;
  ftmPointEdit:
    begin
     if not Assigned(FEditPointControl) then
      NewCursor := crShapeCursor
     else
     if FIsPointAlter and not
       (Assigned(FEditPointControl.Layer) and not
        FEditPointControl.Layer.Moveable) then begin
      // Inserting or deleting point
      if FIsOverPoint then begin
       UpdateEditPointIndex;
       if IsEditCloseFigure
        then NewCursor := crShapeCloseCursor
        else NewCursor := crShapeDelCursor;
      end else
      if FEditPointGuide.Count > 0
       then NewCursor := crShapeAddCursor
       else NewCursor := crShapeCursor;
     end else
     if FIsOverPoint then
      NewCursor := crShapeMoveCursor
     else
     if FIsOverCurveSegment then
      NewCursor := crShapeMoveCurveCursor
     else
     if FIsOverSegment then
      NewCursor := crShapeMoveLineCursor // crShapeMoveCursor
     else
      NewCursor := crShapeCursor;
    end;
  ftmPointEditing:
    // Check "creating" phase
    if (FDefaultToolMode <> ftmPointEdit) and
        Assigned(FCreatingControlClass) then
     NewCursor := FCreatingControlClass.CursorInCreate
    else
     NewCursor := crShapeMoveCursor; //crHandPoint;
  ftmCurveMoving:
    if FIsOverCurveSegment
     then NewCursor := crShapeMoveCurveCursor
     else NewCursor := crShapeMoveCursor;
  ftmCurveContinue:
    NewCursor := crShapeContinueCursor;
  else
    NewCursor := crDefault;
 end;
 if Assigned(FOnUpdateCursor) then FOnUpdateCursor(Self, NewCursor);
 if Cursor <> NewCursor then begin
  Cursor := NewCursor;
  if Assigned(Parent) then Perform(WM_SETCURSOR, Handle, HTCLIENT);
 end;
end;

procedure TFlexPanel.UpdateEditPointIndex;
var i: integer;
    R: TRect;
begin
 FEditPointIndex := -1;
 if not Assigned(FEditPointControl) then exit;
 for i:=0 to FEditPointControl.PointCount-1 do begin
  R := GetEditPointRect(i, SelectionPointInflate);
  if PtInRect(R, FLastMousePos) then begin
   FEditPointIndex := i;
   break;
  end;
 end;
end;

function TFlexPanel.IsEditCloseFigure: boolean;
var FigIndex: integer;
begin
 Result := false;
 if not Assigned(FEditPointControl) or
   FEditPointControl.IsConnectorControl then exit;
 FigIndex := GetFigureIndex(FEditPointControl.PointsInfo^, FEditPointIndex);
 if FigIndex < 0 then exit;
 with FEditPointControl.PointsInfo.Figures[FigIndex] do
  Result := not IsClosed and
    ((FEditPointIndex = FirstNode) or (FEditPointIndex = LastNode))
end;

function TFlexPanel.CloseFigureByPoint(Index: integer): boolean;
var FigIndex: integer;
begin
 Result := false;
 if not Assigned(FEditPointControl) then exit;
 FigIndex := GetFigureIndex(FEditPointControl.PointsInfo^, Index);
 if FigIndex < 0 then exit;
 with FEditPointControl, PointsInfo.Figures[FigIndex] do
  if PointTypes[LastNode] = ptEndNode then
   PointTypes[LastNode] := ptEndNodeClose;
end;

function TFlexPanel.CreateControl(ControlClass: TFlexControlClass;
  ALeft, ATop, AWidth, AHeight: integer; Scheme: TFlexCustomScheme = Nil;
  Layer: TFlexLayer = Nil; UseGrid: boolean = true): TFlexControl;
var R: TRect;
    Delta: TPoint;
    Group: THistoryGroup;
begin
 if not Assigned(Scheme) then Scheme := FActiveScheme;
 if not Assigned(Layer) then Layer := FActiveLayer;
 if not Assigned(Scheme) then raise Exception.Create('Scheme must exist');
 if not Assigned(Layer) then raise Exception.Create('Layer must exist');
 if not Layer.Visible or not Layer.Selectable or Layer.ReadOnly or
    not Layer.Moveable then
  //MessageDlg('Can''t create control on invisible or non selectable layer',
  //  mtError, [mbOk], 0);
  //exit;
  raise Exception.Create(
    'Can''t create control on invisible or disabled layer');
 Result := ControlClass.Create(Self, Scheme, Layer);
 Group := FHistory.OpenLastGroup;
 try
  R.Left := ALeft;
  R.Top := ATop;
  R.BottomRight := R.TopLeft;
  if AWidth <> 0
   then inc(R.Right, AWidth)
   else inc(R.Right, Result.WidthProp.Value);
  if AHeight <> 0
   then inc(R.Bottom, AHeight)
   else inc(R.Bottom, Result.HeightProp.Value);
  if UseGrid and FGridControl.Snap then begin
   Delta.X := 0;
   Delta.Y := 0;
   FGridControl.DoSnap(R, Delta); 
   OffsetRect(R, Delta.x, Delta.y);
  end;
  // Fire OnControlCreate event that can modify DocRect
  if Assigned(FOnControlCreate) then FOnControlCreate(Self, Result, R);
  // Change control size and position
  Result.DocRect := R;
 finally
  if Assigned(Group) then FHistory.EndAction;
 end;
end;

procedure TFlexPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var Control: TFlexControl;
    ShiftPressed: boolean;
    CtrlPressed: boolean;
    NextToolMode: TFlexToolMode;
    Trans, CreatePt: TPoint;
    R: TRect;
    LinkA, LinkB: TLinkPointProp;
    DesignInfo: TFlexCreateInDesignInfo;
    Index, OrigIndex: integer;
begin
 if TabStop then SetFocus;
 FLastMousePos := Point(x, y);
 if (ssDouble in Shift) or (Button = mbMiddle) then begin
  inherited;
  exit;
 end;
 Trans := FLastMousePos;
 UnTransformPoint(Trans.X, Trans.Y);
 case FToolMode of
  ftmSelect:
    begin
     if {(Button = mbLeft) and }FInDesign then
      if (Button = mbLeft) and Assigned(FCreatingControlClass) and
         Assigned(FActiveScheme) and Assigned(FActiveLayer) then begin
       // Creating
       FCreateInDesignAction :=
         FHistory.BeginPanelGroup(TPanelCreateControlsHistoryGroup); 
       UnselectAll;
       Control := CreateControl(FCreatingControlClass, Trans.x, Trans.y, 0, 0);
       Select(Control);
       // Check connector control
       Control.GetLinkProps(LinkA, LinkB);
       if Assigned(LinkA) and (FDefaultLinkPoint.Index >= 0) then begin
        // Link to nearest linking site
        LinkA.LinkPoint := FDefaultLinkPoint.DocPos;
        LinkA.LinkedControl := FDefaultLinkPoint.Control;
       end;
       DesignInfo.IsPointEdit := false;
       if Assigned(FEditPointControl) then with DesignInfo do begin
        DesignInfo.PointEditIndex := FEditPointControl.PointCount - 1;
        FEditPointControl.CreateInDesign(DesignInfo);
        IsPointEdit := IsPointEdit and (PointEditIndex >= 0) and
          (PointEditIndex < FEditPointControl.PointCount);
       end;
       if DesignInfo.IsPointEdit then
        DoPointEdit(FEditPointControl, DesignInfo.PointEditIndex,
          Trans.X, Trans.Y)
       else
       if (psReadOnly in Control.WidthProp.Style) and
          (psReadOnly in Control.HeightProp.Style) then
        ToolMode := ftmMoving
       else
        ToolMode := ftmCreating;
      end else
      if not FViewing then begin
       // Selecting
       NextToolMode := FToolMode;
       ShiftPressed := ssShift in Shift;
       CtrlPressed := ssCtrl in Shift;
       if not CtrlPressed
        then Control := FindControlAtPoint(X, Y)
        else Control := Nil;
       if ShiftPressed then begin
        if Assigned(Control) then
         // Inverse control selection
         Control.IsSelected := not Control.IsSelected
        else
         NextToolMode := ftmSelecting
       end else
       if CtrlPressed then begin
        if not ShiftPressed then UnselectAll;
        NextToolMode := ftmSelecting;
       end else
       if Assigned(Control) then begin
        if FSelList.IndexOf(Control) < 0 then begin
         UnselectAll;
         Select(Control);
        end;
        NextToolMode := ftmMoving
       end else begin
        UnselectAll;
        NextToolMode := ftmSelecting;
       end;
       if (NextToolMode <> ftmMoving) or (FSelList.Count > 0) then
        ToolMode := NextToolMode;
      end;
     // Check viewing mode
     if FViewing and (ToolMode = ftmSelect) and
        (FInDesign or not
         (Assigned(FMouseControl) and Assigned(FMouseControl.Reference))) then
      case Button of
       mbLeft  : ToolMode := ftmPanning;
       //mbRight : ToolMode := ftmZooming;
      end;
      //exit;
    end;
  ftmZoom:
    if Button = mbLeft then ToolMode := ftmZooming;
  ftmPan:
    if Button = mbLeft then ToolMode := ftmPanning;
  ftmMove:
    if (Button = mbLeft) and (FSelList.Count > 0) then ToolMode := ftmMoving;
  ftmResize:
    if (Button = mbLeft) and (FSelList.Count > 0) then ToolMode := ftmResizing;
  ftmPointEdit:
    if (Button = mbLeft) then begin
     if Assigned(FEditPointControl) then begin
      // Check curve edit
      if not (
         Assigned(FEditPointControl.Layer) and not
         FEditPointControl.Layer.Moveable ) then begin
       // Curve editable
       if FIsPointAlter then begin
        // Inserting or deleting point
        if FIsOverPoint then begin
         // Delete point or close figure
         UpdateEditPointIndex;
         if IsEditCloseFigure then begin
          FEditPointControl.BeginPointsDesigning;
          try
           CloseFigureByPoint(FEditPointIndex)
          finally
           FEditPointControl.EndPointsDesigning;
          end;
         end else
         if FEditPointControl.NodeCount > 2 then
          with FEditPointControl do begin
           BeginPointsDesigning;
           try
            FEditPointControl.DeletePoint(FEditPointIndex);
           finally
            FEditPointControl.EndPointsDesigning;
           end;
          end;
        end else begin
         // Insert nearest point
         if FEditPointGuide.Count = 0 then with FGridControl do begin
          if Snap then begin
           // Snap point
           R.TopLeft := Trans;
           R.BottomRight := Trans;
           CreatePt.X := 0;
           CreatePt.Y := 0;
           DoSnap(R, CreatePt);
           inc(CreatePt.X, Trans.X);
           inc(CreatePt.Y, Trans.Y);
          end else
           CreatePt := Trans;
          with FEditPointControl.DocRect do begin
           dec(CreatePt.X, Left);
           dec(CreatePt.Y, Top);
          end;
         end else
          CreatePt := FEditPointGuide.NewPoint;
         // Insert point
         with FEditPointControl do begin
          BeginPointsDesigning;
          try
           InsertNearestPoint(CreatePt);
          finally
           EndPointsDesigning;
          end;
         end;
        end;
        UpdatePoints;
       end else begin
        // Check select point(s) or curve moving
        ShiftPressed := ssShift in Shift;
        UpdateEditPointIndex;
        if FEditPointIndex >= 0 then begin
         // Mouse over point
         if not FEditPointSelected[FEditPointIndex] and
           (FEditPointControl.PointTypes[FEditPointIndex] <> ptControl) then begin
          if not ShiftPressed then UnselectAllPoints;
          EditPointSelected[FEditPointIndex] := true;
         end;
         DoPointEdit(FEditPointControl, FEditPointIndex, Trans.X, Trans.Y)
        end else
        if FIsOverCurveSegment or FIsOverSegment then
         // Curve moving
         ToolMode := ftmCurveMoving
        else begin
         // Start rectangle points selection
         if not ShiftPressed then UnselectAllPoints;
         ToolMode := ftmPointSelecting;
        end;
       end;
      end;
     end else begin
      // Check selecting curve control for edit
      UnselectAll;
      Select(FMouseControl);
     end;
    end;
  ftmCurveContinue:
    if Assigned(FEditPointControl) and (FEditPointIndex >= 0) then begin
     CreatePt := Trans;
     with FEditPointControl.DocRect do begin
      dec(CreatePt.X, Left);
      dec(CreatePt.Y, Top);
     end;
     Index := GetFigureIndex(FEditPointControl.PointsInfo^, FEditPointIndex);
     repeat
      if Index < 0 then break;
      // if IsClosed then break;
      // Find first point (if there several same points on start)
      Index := FindFirstOrLastNodeIfEqual(FEditPointIndex, FEditPointControl);
      if Index < 0 then break;
      OrigIndex := Index;
      // Check point insert to the end of the figure
      if (Index < FEditPointControl.PointCount) and
         (FEditPointControl.PointTypes[Index] <> ptNode) then inc(Index);
      // Insert new point
      FEditPointControl.BeginUpdate;
      try
       FEditPointControl.InsertPoint(Index, CreatePt);
       if Index < 0 then exit;
       if Index > FEditPointIndex then begin
        FEditPointControl.PointTypes[Index] :=
          FEditPointControl.PointTypes[FEditPointIndex];
        FEditPointControl.PointTypes[FEditPointIndex] := ptNode;
       end;
      finally
       FEditPointControl.EndUpdate;
      end;
      // Get Start node coords for right aligning
      CreatePt := FEditPointControl.Points[OrigIndex];
      with FEditPointControl.DocRect do begin
       inc(CreatePt.X, Left);
       inc(CreatePt.Y, Top);
      end;
      // Move to point edit mode
      DoPointEdit(FEditPointControl, Index, Trans.X, Trans.Y);
      FMoveStart := CreatePt;
      break;
     until false;
    end;
 end;
 UpdateMouseData;
 inherited;
end;

procedure TFlexPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Trans: TPoint;
    Link, LinkA, LinkB: TLinkPointProp;
    Deleted: boolean;
    i: integer;
begin
 FLastMousePos := Point(x, y);
 Trans := FLastMousePos;
 UnTransformPoint(Trans.X, Trans.Y);
 case FToolMode of
  ftmCreating:
    if FSelList.Count = 1 then with TFlexControl(FSelList[0]) do
     if (Width <= UnScaleValue(2, FOwner.Scale)) or
        (Height <= UnScaleValue(2, FOwner.Scale)) then
      Self.DeleteSelected;
  ftmPointEditing:
    if Assigned(FEditPointControl) and (FSelList.Count = 1) then
    with FEditPointControl do begin
     // Check deletion
     Deleted := true;
     for i:=0 to PointCount-1 do with Points[i] do
      if (x <>  0) or (y <> 0) then begin
       Deleted := false;
       break;
      end;
     if Deleted then
      Self.DeleteSelected
     else
     if IsConnectorControl then begin
      // Check connector control
      Link := Nil;
      GetLinkProps(LinkA, LinkB);
      if Assigned(FEditPointSelected) then
       if FEditPointSelected[0] then Link := LinkA else
       if FEditPointSelected[High(FEditPointSelected)] then Link := LinkB;
      if Assigned(Link) and (FDefaultLinkPoint.Index >= 0) then begin
       // Link to nearest linking site
       Link.LinkedControl := FDefaultLinkPoint.Control;
       Link.LinkPoint := FDefaultLinkPoint.DocPos;
      end;
     end;
    end;
 end;
 case FToolMode of
  ftmSelecting,
  ftmMoving,
  ftmCreating,
  ftmPointEdit,
  ftmPointSelecting,
  ftmPointEditing,
  ftmCurveMoving,
  ftmResize,
  ftmResizing:
    begin
     ToolMode := FDefaultToolMode;
    end;
  ftmZooming:
    begin
     if FViewing
      then ToolMode := FDefaultToolMode
      else ToolMode := ftmZoom;
    end;
  ftmPanning:
    begin
     if FViewing
      then ToolMode := FDefaultToolMode
      else ToolMode := ftmPan;
    end;
 end;
 UpdateMouseData;
 inherited;
end;

procedure TFlexPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 UpdateToolMode(Shift, X, Y);
 inherited;
end;

procedure TFlexPanel.UpdateMouseData;
var i, j, Size: integer;
    FirstIndex, NextIndex: integer;
    Trans, TopLeft, NewPoint: TPoint;
    R, TestRect: TRect;
    LinkPointExist: boolean;
    LinkPoint: TLinkPointInfo;
    Dist, Temp, MinDistSqr: double;
    NewControl, NewSubControl: TFlexControl;

 procedure PointToRect(const Point: TPoint; Size_: integer; var Rect: TRect);
 var Half: integer;
 begin
  Half := Size_ div 2;
  with Rect, Point do begin
   Left := X - Half;
   Top := Y - Half;
   Right := Left + Size_;
   Bottom := Top + Size_;
  end;
 end;

begin
 if not (FToolMode in [ftmZooming, ftmSelecting, ftmPanning, ftmPointEditing,
   ftmMoving, ftmCreating, ftmResizing, ftmCurveMoving]) then
 with FLastMousePos do begin
  // Reset curve guide
  FEditPointGuide.Count := 0;
  // Check edit point
  FIsOverPoint := (FPointsRgn <> 0) and PtInRegion(FPointsRgn, X, Y);
  if Assigned(FEditPointControl) then begin
   Trans := FEditPointControl.OwnerToClient(FLastMousePos);
   if not FPointsRgnExact and FIsOverPoint then begin
    // Find point
    Size:= UnScaleValue(AnchorPointSize, FScale);
    for i:=0 to FEditPointControl.PointCount-1 do begin
     PointToRect(FEditPointControl.Points[i], Size, R);
     FIsOverPoint :=
       (Trans.X >= R.Left) and (Trans.X <= R.Right) and
       (Trans.Y >= R.Top) and (Trans.Y <= R.Bottom);
     if FIsOverPoint then break;
    end;
   end;
   // Rebuid curve guide
   if FShowEditPointGuide and not FIsOverPoint and FIsPointAlter then begin
    with FGridControl do
     if Snap then begin
      // Snap point
      R.TopLeft := Trans;
      R.BottomRight := Trans;
      NewPoint.X := 0;
      NewPoint.Y := 0;
      DoSnap(R, NewPoint);
      inc(NewPoint.X, Trans.X);
      inc(NewPoint.Y, Trans.Y);
     end else
      NewPoint := Trans;
    FEditPointControl.CreateCurveGuide(NewPoint, FEditPointGuide);
   end;
   // Check on curve segment
   if FToolMode = ftmPointEdit then begin
    FIsOverSegment := FEditPointControl.FindNearestPathSegment(Trans,
      FirstIndex, NextIndex, @FOverCurveNearest, false, true);
    FIsOverCurveSegment := FIsOverSegment and
      (FirstIndex < FEditPointControl.PointCount-1) and
      (FEditPointControl.PointTypes[FirstIndex+1] = ptControl);
    if FIsOverSegment then begin
     with FEditPointControl.DocRect do begin
      inc(FOverCurveNearest.Point.x, Left);
      inc(FOverCurveNearest.Point.y, Top);
     end;
     FOverCurveNearest.Index0 := FirstIndex;
     FOverCurveNearest.Index1 := NextIndex;
    end;
   end else begin
    FIsOverSegment := false;
    FIsOverCurveSegment := false;
   end;
  end else
  if not FPointsRgnExact and FIsOverPoint then begin
   FIsOverPoint := false;
   // Find anchor point
   for i:=0 to FSelList.Count-1 do begin
    R := GetAnchorRect(TFlexControl(FSelList[i]));
    FIsOverPoint :=
      (X >= R.Left) and (X <= R.Right) and
      (Y >= R.Top) and (Y <= R.Bottom);
    if FIsOverPoint then break;
   end;
  end;
  // Check mouse control
  NewControl := FindControlAtPoint(x, y);
  NewSubControl := NewControl;
  if Assigned(NewSubControl) and (NewSubControl.Count > 0) then
    for i:=NewSubControl.Count-1 downto 0 do with NewSubControl[i] do
     if FVisibleProp.Value and IsPointInside(x, y) then begin
      NewSubControl := NewSubControl[i];
      break;
     end;
  SetMouseControl(NewControl, NewSubControl);
 end;
 // Check link point search
 LinkPoint.Index := -1;
 LinkPointExist :=
   ( Assigned(FCreatingControlClass) and
     FCreatingControlClass.IsConnectorControl ) or
   ( (FToolMode = ftmCreating) and (FSelList.Count > 0) and 
     TFlexControl(FSelList[0]).IsConnectorControl ) or
   ( (FToolMode = ftmPointEditing) and Assigned(FEditPointControl) and
     FEditPointControl.IsConnectorControl and (FEditPointSelCount = 1) and
     (FEditPointSelected[0] or FEditPointSelected[High(FEditPointSelected)]) );
 if LinkPointExist then begin
  // Find Nearest link point
  MinDistSqr := 0;
  Trans := FActiveScheme.OwnerToClient(FLastMousePos);
  // Calc test rectangle area
  Size := UnscaleValue(ConnectorStickThreshold, FScale);
  PointToRect(Trans, Size, TestRect);
  // Check all non-grouped controls
  for i:=0 to FActiveScheme.Count-1 do with FActiveScheme[i] do begin
   if FActiveScheme[i].IsConnectorControl or
      (FActiveScheme[i] = FEditPointControl) or
      (DefaultLinkPointCount = 0) then continue;
   TopLeft.X := LeftProp.Value;
   TopLeft.Y := TopProp.Value;
   // Test intersection
   R := GetRefreshRect(TopLeft.X, TopLeft.Y);
   with TestRect do
    if (R.Left > Right) or (R.Top > Bottom) or
       (R.Right < Left) or (R.Bottom < Top) then continue;
   // Test default link points
   for j:=0 to DefaultLinkPointCount-1 do begin
    NewPoint := DefaultLinkPoints[j];
    inc(NewPoint.X, TopLeft.X);
    inc(NewPoint.Y, TopLeft.Y);
    // Test point inside TestRect
    with TestRect, NewPoint do
     if (X < Left) or (X > Right) or (Y < Top) or (Y > Bottom) then continue;
    // Calc distance between NewPoint and Trans
    Temp := NewPoint.X - Trans.X;
    Dist := Temp * Temp;
    Temp := NewPoint.Y - Trans.Y;
    Dist := Dist + Temp * Temp;
    // Check distance
    with LinkPoint do
     if (Index < 0) or (Dist < MinDistSqr) then begin
      Control := FActiveScheme[i];
      Index := j;
      DocPos := NewPoint;
      MinDistSqr := Dist;
     end;
   end;
  end;
 end else begin
  // Hide link point
  LinkPoint.Index := -1;
 end;
 // Check link point invalidate
 if LinkPoint.Index <> FDefaultLinkPoint.Index then begin
  // Invalidate old link point rect
  if Assigned(Parent) then begin
   NewPoint := FDefaultLinkPoint.DocPos;
   NewPoint := FActiveScheme.ClientToOwner(NewPoint);
   PointToRect(NewPoint, FDefaultLinkPoint.Size, R);
   InvalidateRect(Handle, @R, False);
  end;
  LinkPoint.Size := FDefaultLinkPoint.Size;
  FDefaultLinkPoint := LinkPoint;
  // Invalidate new link point rect
  if Assigned(Parent) then begin
   NewPoint := FDefaultLinkPoint.DocPos;
   NewPoint := FActiveScheme.ClientToOwner(NewPoint);
   PointToRect(NewPoint, FDefaultLinkPoint.Size, R);
   InvalidateRect(Handle, @R, False);
  end;
 end;
end;

procedure TFlexPanel.UpdateToolMode(Shift: TShiftState; X, Y: Integer);
var Control: TFlexControl;
    Delta, Base: TPoint;
    ResizeCursor: TResizeCursor;
    Ortogonal, Smooth, Symmetric: boolean;
    Trans: TPoint;
    PointAlterChange: boolean;
    R: TRect;
    IsNeedHint: boolean;

 function NeedCurveContinueMode: boolean;
 var Point: TPoint;
     PointRect: TRect;
     PointRectSize: integer;
     i, NearestIndex: integer;
     DesignInfo: TFlexCreateInDesignInfo;
 begin
  Result := false;
  FEditPointIndex := -1;
  if Assigned(FCreatingControlClass) and
     Assigned(FEditPointControl) and
     (FEditPointControl.ClassType = FCreatingControlClass) and
     (FMouseControl = FEditPointControl) then begin
   Point := Trans;
   with FEditPointControl.DocRect do begin
    dec(Point.X, Left);
    dec(Point.Y, Top);
   end;
   PointRectSize := UnscaleValue(AnchorPointSize {SelectionThreshold}, FScale);
   with Point do begin
    PointRect.Left := X - PointRectSize div 2;
    PointRect.Top := Y - PointRectSize div 2;
    PointRect.Right := PointRect.Left + PointRectSize;
    PointRect.Bottom := PointRect.Top + PointRectSize;
   end;
   // Find point
   NearestIndex := -1;
   for i:=0 to FEditPointControl.PointCount-1 do
    if FEditPointControl.PointTypes[i] <> ptControl then
     with FEditPointControl.Points[i] do
      if (X >= PointRect.Left) and (X <= PointRect.Right) and
         (Y >= PointRect.Top) and (Y <= PointRect.Bottom) then begin
       NearestIndex := i;
       break;
      end;
   if NearestIndex < 0 then exit;
   NearestIndex := FindFirstOrLastNodeIfEqual(NearestIndex, FEditPointControl);
   if NearestIndex >= 0 then begin
    DesignInfo.IsContinueAvail := false;
    DesignInfo.PointContinueIndex := NearestIndex;
    FEditPointControl.CreateInDesign(DesignInfo);
    Result := DesignInfo.IsContinueAvail;
    if Result then FEditPointIndex := NearestIndex;
   end;
  end;
 end;

begin
 FLastMousePos := Point(x, y);
 Trans := FLastMousePos;
 UnTransformPoint(Trans.X, Trans.Y);
 PointAlterChange :=
   ((FToolMode = ftmPointEdit) and (ssCtrl in Shift)) <> FIsPointAlter;
 if PointAlterChange then FIsPointAlter := not FIsPointAlter;
 UpdateMouseData;
 // Update hint
 IsNeedHint := (FToolMode = ftmSelect);
 DoNeedHint(IsNeedHint);
 if IsNeedHint then begin
  // Check hint control
  Control := FHintControl;
  FHintControl := FMouseSubControl;
  while (FHintControl <> FMouseControl) and (FHintControl.Hint = '') do
   FHintControl := FHintControl.Parent;
  if not Assigned(FHintControl) and Assigned(FActiveScheme) and
     FActiveScheme.IsPointInside(x, y) then FHintControl := FActiveScheme;
  if Assigned(FHintControl) and not FHintControl.ShowHint then FHintControl := Nil;
  if (Control <> FHintControl) {and not FInDesign} then Application.CancelHint;
 end else
  Application.CancelHint;
 // Process mouse movement for current tool mode
 repeat
  case FToolMode of
   ftmSelect:
     if FInDesign then begin
      FResizeCursor := GetSelResizeCursor(X, Y);
      if FResizeCursor <> rcrNone then
       ToolMode := ftmResize
      else
    { if Assigned(FEditPointControl) then begin
       FIsPointAlter := ssCtrl in Shift;
       if FIsOverPoint or FIsPointAlter then ToolMode := ftmPointEdit;
      end else  }
      if not Assigned(FEditPointControl) and FIsOverPoint then
       ToolMode := ftmMove
      else
      if NeedCurveContinueMode then
       ToolMode := ftmCurveContinue;
     end;
   ftmCurveContinue:
     if not NeedCurveContinueMode then
      ToolMode := FDefaultToolMode; //ftmSelect;
   ftmZooming,
   ftmPointSelecting,
   ftmSelecting:
     begin
      if Trans.X > FMouseAnchor.X then begin
       R.Left := FMouseAnchor.X;
       R.Right := Trans.X+1;
      end else begin
       R.Left := Trans.X;
       R.Right := FMouseAnchor.X+1;
      end;
      if Trans.Y > FMouseAnchor.Y then begin
       R.Top := FMouseAnchor.Y;
       R.Bottom := Trans.Y+1;
      end else begin
       R.Top := Trans.Y;
       R.Bottom := FMouseAnchor.Y+1;
      end;
      //inc(R.Right, PixelScaleFactor);
      //inc(R.Bottom, PixelScaleFactor);
      MarqueeRect := R;
     end;
   ftmResize:
     begin
      FResizeCursor := GetSelResizeCursor(X, Y);
      if FResizeCursor = rcrNone then
       ToolMode := FDefaultToolMode; //ftmSelect;
     end;
   ftmPointEdit:
     begin
      FResizeCursor := GetSelResizeCursor(X, Y);
      if FResizeCursor <> rcrNone then
       ToolMode := ftmResize
      else
      if FShowEditPointGuide and (FIsPointAlter or PointAlterChange) then
       UpdatePoints;
     end;
   ftmMove:
     begin
      if not FIsOverPoint then ToolMode := ftmSelect;
     end;
   ftmCurveMoving,  
   ftmPointEditing,
   ftmMoving:
     begin
      Ortogonal := ssCtrl in Shift;
      Smooth := ssShift in Shift;
      Symmetric := Smooth and (ssAlt in Shift);
      case FToolMode of
       ftmPointEditing:
         begin
          Base := FEditPointControl.Points[FEditPointIndex];
          with FEditPointControl.DocRect do begin
           inc(Base.X, Left);
           inc(Base.Y, Top);
          end;
         end;
       ftmCurveMoving:
         Base := FMoveStart;
       else
         Base := Point(FSelRect.Left, FSelRect.Top);
      end;
      if FToolMode = ftmMoving then begin
       Delta.X := Trans.X - FLastMovedPos.X;
       Delta.Y := Trans.Y - FLastMovedPos.Y;
      end else begin
       Delta.X := Trans.X - (FMouseAnchor.X + Base.X);
       Delta.Y := Trans.Y - (FMouseAnchor.Y + Base.Y);
      end;
      with FGridControl do
       if Snap then begin
        // Calc snap rect
        if (FToolMode = ftmPointEditing) or (FToolMode = ftmCurveMoving)
         then R := Rect(Base.x, Base.y, Base.x, Base.y)
         else R := FSelRect;
        // Do snap
        DoSnap(R, Delta);
       end;
      if Ortogonal then
       if Abs(Delta.X + Base.X - FMoveStart.X) >
          Abs(Delta.Y + Base.Y - FMoveStart.Y)
        then Delta.Y := FMoveStart.Y - Base.Y
        else Delta.X := FMoveStart.X - Base.X;
      if (Delta.X <> 0) or (Delta.Y <> 0) then
      case FToolMode of
       ftmPointEditing:
         FEditPointControl.MovePathPoints(FEditPointIndex, Delta,
           FEditPointSelected, Smooth, Symmetric);
       ftmCurveMoving:
         with FOverCurveNearest do begin
          dec(Delta.x, Point.x - FMoveStart.x);
          dec(Delta.y, Point.y - FMoveStart.y);
          FEditPointControl.MovePathSegment(Index0, Index1, Delta, CurvePos);
          inc(Point.x, Delta.x);
          inc(Point.y, Delta.y);
         end;
       else begin
        // ftmMoving - Move selected controls
        MoveSelected(Delta.X, Delta.Y);
        inc(FLastMovedPos.X, Delta.X);
        inc(FLastMovedPos.Y, Delta.Y);
        // Check begin drag
        if FAutoDragEnabled then with FLastMousePos do
         if (x < 0) or (x > ClientWidth) or
            (y < 0) or (y > ClientHeight) then begin
          BeginDrag(False);
          DeleteSelected;
         end;
       end;
      end;
     end;
   ftmCreating,
   ftmResizing:
     if ssLeft in Shift then begin
      Ortogonal := ssCtrl in Shift;
      if FToolMode = ftmCreating
       then ResizeCursor := rcrBottomRight
       else ResizeCursor := FResizeCursor;
      Delta.X := Trans.X - FMouseAnchor.X;
      if Delta.X > 0 then inc(Delta.X, PixelScaleFactor);
      Delta.Y := Trans.Y - FMouseAnchor.Y;
      if Delta.Y > 0 then inc(Delta.Y, PixelScaleFactor);
      ResizeSelected(Delta.X, Delta.Y, Ortogonal, True, ResizeCursor);
     end;
   ftmPanning:
     if HorzScrollBar.IsScrollBarVisible or
        VertScrollBar.IsScrollBarVisible then begin
      Ortogonal := ssCtrl in Shift;
      Delta.X := FLastMousePos.X - FMoveStart.X;
      Delta.Y := FLastMousePos.Y - FMoveStart.Y;
      if Ortogonal then
       if Abs(Delta.X) > Abs(Delta.Y)
        then Delta.Y := 0
        else Delta.X := 0;
      HorzScrollBar.Position := FMouseAnchor.X - Delta.X;
      VertScrollBar.Position := FMouseAnchor.Y - Delta.Y;
      UpdateOrigin;
     end;
  end;
  break;
 until false;
 UpdateCursor;
end;

procedure TFlexPanel.SetModified(const Value: boolean);
begin
  FModified := Value;
  DoNotify(nil, fnChange);
end;

procedure TFlexPanel.SetMouseControl(Control: TFlexControl;
  SubControl: TFlexControl = Nil);
begin
 if (Control = FMouseControl)
    and
    ( (SubControl = FMouseSubControl)
      or
      (not Assigned(SubControl) and (Control = FMouseSubControl)) )
 then
  exit;
 FMouseControl := Control;
 if Assigned(SubControl) and Assigned(Control)
  then FMouseSubControl := SubControl
  else FMouseSubControl := Control;                                
 DoMouseControlChange;
end;

procedure TFlexPanel.DoMouseControlChange;
begin
 if Assigned(FOnMouseControlChange) then
   FOnMouseControlChange(Self);
end;

procedure TFlexPanel.DoEnter;
begin
 InvalidateSelection;
 inherited;
end;

procedure TFlexPanel.DoExit;
begin
 InvalidateSelection;
 inherited;
end;

procedure TFlexPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
 if FLastKeyShift <> Shift then begin
  FLastKeyShift := Shift;
  UpdateToolMode(Shift, FLastMousePos.X, FLastMousePos.Y);
 end;
 inherited;
end;

procedure TFlexPanel.KeyUp(var Key: Word; Shift: TShiftState);
begin
 if FLastKeyShift <> Shift then begin
  FLastKeyShift := Shift;
  UpdateToolMode(Shift, FLastMousePos.X, FLastMousePos.Y);
 end;
 inherited;
end;

function TFlexPanel.CreateDragObject(AControl: TFlexControl;
  ChildrenOnly, SelectedOnly: boolean; DragName: string = ''): TFlexDragObject;
var MS: TMemoryStream;
    Filer: TFlexFiler;
    //i: integer;
    s: string;
    Control: TFlexControl;
begin
 //Result := Nil;
 MS := Nil;
 Filer := Nil;
 try
  MS := TMemoryStream.Create;
  Filer := CreateFlexFiler(MS, ppCopy);
  SaveToFiler(Filer, SelectedOnly, AControl, ChildrenOnly);
  Filer.Rewind;
  Filer.Total := Filer.StreamSize;
  FHistory.DisableRecording;
  try
   // Create TFlexDragObject
   Result := TFlexDragObject.Create;
   with Result do
   try
    //FIsLoading := true;
    BeginLoading;
    try
     // Skip clipboard word
     Filer.LoadStrCheck(s);
     // Create DragGroup and read controls into it
     DragGroup := TFlexGroup.Create(Self, Nil, Nil);
     DragGroup.ID := 0;
     DragGroup.IdProp.Style := DragGroup.IdProp.Style + [ psReadOnly ];
     while Filer.LoadStrCheck(s) do
      if StrBeginsFrom(s, fcObject) then begin
       Control := LoadFlexControl(Filer, DragGroup, s);
       //Control.ID := 0;
       if SelectedOnly then begin
        Control.Left := Control.Left - FSelRect.Left;
        Control.Top := Control.Top - FSelRect.Top;
       end;
      end else
       Filer.CheckLoadSkipToEnd(s);
     // Initialize properties of DragGroup
     if Assigned(AControl) and (DragName = '') then DragName := AControl.Name;
     DragGroup.Name := DragName;
     if SelectedOnly then begin
      DragGroup.Left := FSelRect.Left;
      DragGroup.Top := FSelRect.Top;
      DragGroup.Width := FSelRect.Right - FSelRect.Left;
      DragGroup.Height := FSelRect.Bottom - FSelRect.Top;
     end else begin
      DragGroup.Left := AControl.Left;
      DragGroup.Top := AControl.Top;
      DragGroup.Width := AControl.Width;
      DragGroup.Height := AControl.Height;
     end;
     DefaultFlex := Self;
     MouseDragDelta := Point(0, 0);
    finally
     EndLoading;
    end;
   except
    Result.Free;
    raise;
   end;
  finally
   FHistory.EnableRecording;
  end;
 finally
  Filer.Free;
  MS.Free;
  //FIsLoading := False;
 end;
end;

function TFlexPanel.StartDrag(AControl: TFlexControl;
  ChildrenOnly, SelectedOnly: boolean; const MousePos, AOrigin: TPoint;
  AScale: integer; const DragName: string = ''): TFlexDragObject;
var DragDelta: TPoint;
begin
 Screen.Cursor := crHourGlass;
 try
  Result := CreateDragObject(AControl, ChildrenOnly, SelectedOnly, DragName);
  if not Assigned(Result) then exit;
  try
   with DragDelta do begin
    X := UnScaleValue(MousePos.X + AOrigin.X, AScale) - Result.DragGroup.Left;
    Y := UnScaleValue(MousePos.Y + AOrigin.Y, AScale) - Result.DragGroup.Top;
   end;
   Result.MouseDragDelta := DragDelta;
  except
   Result.Free;
   raise;
  end;
 finally
  Screen.Cursor := crDefault;
 end;
end;

procedure TFlexPanel.DoStartDrag(var DragObject: TDragObject);
begin	  	
 if Assigned(OnStartDrag) then
  inherited
 else
  DragObject := StartDrag(Nil, False, True, FLastMousePos,
    FOrigin, FScale, 'DragGroup');
end;

procedure TFlexPanel.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 Accept := Source is TFlexDragObject;
 if Accept and (not Assigned(FDragObject) or (FDragObject = Source)) then
 with TFlexDragObject(Source) do begin
  if Assigned(Parent) then Windows.SetFocus(Handle);
  case State of
   dsDragEnter:
     begin
      if Assigned(FDragObject) and (FDragObject <> Source) then begin
       FDragObject.Unlink;
       FDragObject := Nil;
      end;
      FDragObject := TFlexDragObject(Source);
      FDragObject.UpdateLink(Self, X, Y);
     end;
   dsDragLeave:
     begin
      if Assigned(FDragObject) then begin
       FDragObject.Unlink;
       FDragObject := Nil;
      end;
     end;
   dsDragMove:
     begin
      if Assigned(FDragObject) then FDragObject.UpdateLink(Self, X, Y);
     end;
  end;
  if Assigned(FDragObject) and
     ((FSelList.Count <> 1) or (Selected[0] <> FDragObject.DragGroup)) then begin
   UnselectAll;
   Select(FDragObject.DragGroup);
  end;
  // Call event handler manually since standard inherited method ignore current
  // Accept value and always initialize it to True if OnDragOver handler exist
  // and to False in other case.
  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
 end else begin
  if Assigned(FDragObject) then begin
   FDragObject.Unlink;
   FDragObject := Nil;
  end;
  inherited;
 end;
end;

procedure TFlexPanel.DragDrop(Source: TObject; X, Y: Integer);
var PassRec: TPassControlRec;
    Control: TFlexControl;
begin
 if Source is TFlexDragObject then begin
  if FDragObject <> Source  then begin
   if Assigned(FDragObject) then FDragObject.Unlink;
   FDragObject := TFlexDragObject(Source);
  end;
  FDragObject.UpdateLink(Self, X, Y);
  FHistory.DisableRecording;
  try
   // Validate id's
   Control := FDragObject.DragGroup;
   FirstControl(Control, PassRec);
   try
    Control := NextControl(PassRec); // Skip drag group
    while Assigned(Control) do begin
     if Control.IdProp.Value = 0 then GenerateID(Control);
     Control := NextControl(PassRec);
    end;
   finally
    ClosePassRec(PassRec);
   end;
   // Select drag group
   if (FSelList.Count <> 1) or (Selected[0] <> FDragObject.DragGroup) then begin
    UnselectAll;
    Select(FDragObject.DragGroup);
   end;
   // Ungroup
   Ungroup;
   FDragObject.DragGroup := Nil;
   FDragObject := Nil;
  finally
   FHistory.EnableRecording;
  end;
  // Record selected controls
  FHistory.RecordSelectedAsNew;
 end;
 inherited;
end;

procedure TFlexPanel.PropBeforeChanged(Sender: TObject;
  Prop: TCustomProp);
begin
 if Assigned(FOnPropBeforeChanged) then FOnPropBeforeChanged(Sender, Prop);
end;

procedure TFlexPanel.PropChanged(Sender: TObject; Prop: TCustomProp);
begin
 FModified := true;
 if Assigned(FOnPropChanged) then FOnPropChanged(Sender, Prop);
end;

function TFlexPanel.IsSelected(AControl: TFlexControl): boolean;
begin
 Result := FSelList.IndexOf(AControl) >= 0;
end;

procedure TFlexPanel.SetInDesign(const Value: boolean);
begin
 FInDesign := Value;
end;

function TFlexPanel.GetShowGrid: boolean;
begin
 Result := FGridControl.Visible;
end;

procedure TFlexPanel.SetShowGrid(const Value: boolean);
begin
 FGridControl.Visible := Value;
end;

function TFlexPanel.GetShowPixGrid: boolean;
begin
 Result := FGridControl.ShowPixGrid;
end;

procedure TFlexPanel.SetShowPixGrid(const Value: boolean);
begin
 FGridControl.ShowPixGrid := Value;
end;

function TFlexPanel.GetGridHorizSize: integer;
begin
 Result := FGridControl.HSize;
end;

function TFlexPanel.GetGridVertSize: integer;
begin
 Result := FGridControl.VSize;
end;

procedure TFlexPanel.SetGridHorizSize(const Value: integer);
begin
 FGridControl.HSize := Value;
end;

procedure TFlexPanel.SetGridVertSize(const Value: integer);
begin
 FGridControl.VSize := Value;
end;

function TFlexPanel.GetGridStyle: TFlexGridStyle;
begin
 Result := FGridControl.Style;
end;

procedure TFlexPanel.SetGridStyle(const Value: TFlexGridStyle);
begin
 FGridControl.Style := Value;
end;

function TFlexPanel.GetSnapToGrid: boolean;
begin
 Result := FGridControl.FSnap;
end;

procedure TFlexPanel.SetSnapToGrid(const Value: boolean);
begin
 FGridControl.FSnap := Value;
end;

function TFlexPanel.GetSnapStyle: TFlexSnaps;
begin
 Result := FGridControl.SnapTo;
end;

procedure TFlexPanel.SetSnapStyle(const Value: TFlexSnaps);
begin
 FGridControl.SnapTo := Value;
end;

function TFlexPanel.GetGridColor: TColor;
begin
 Result := FGridControl.Color;
end;

procedure TFlexPanel.SetGridColor(const Value: TColor);
begin
 FGridControl.Color := Value;
end;

function TFlexPanel.GetGridPixColor: TColor;
begin
 Result := FGridControl.PixColor;
end;

procedure TFlexPanel.SetGridPixColor(const Value: TColor);
begin
 FGridControl.PixColor := Value;
end;

procedure TFlexPanel.SetFrostPan(const Value: boolean);
begin
 if (Value = FFrostPan) or (FToolMode = ftmPanning) then
   Exit;
 FFrostPan := Value;
end;

procedure TFlexPanel.BeginSelectionUpdate(IsTransformation: boolean = false;
  HistoryGroup: THistoryGroupClass = Nil);
var i: integer;
begin
 if FSelUpdateCounter = 0 then begin
  FSelNeedUpdate := False;
  if IsTransformation then
   // Call BeginSelectionTransformation for all selected flex-controls
   for i:=0 to FSelList.Count-1 do
    TFlexControl(FSelList[i]).BeginSelectionTransformation;
 end;
 inc(FSelUpdateCounter);
 if (FSelUpdateCounter = 1) and Assigned(HistoryGroup) then
  FHistory.BeginPanelGroup(HistoryGroup);
 if Assigned(FOnBeginSelectionUpdate) then FOnBeginSelectionUpdate(Self);
end;

procedure TFlexPanel.EndSelectionUpdate(IsTransformation: boolean = false;
  HistoryGroup: THistoryGroupClass = Nil);
var i: integer;
begin
 if FSelUpdateCounter = 0 then exit;
 dec(FSelUpdateCounter);
 if FSelUpdateCounter = 0 then begin
  // Call EndSelectionTransformation for all selected flex-controls
  if IsTransformation then
   for i:=0 to FSelList.Count-1 do
    TFlexControl(FSelList[i]).EndSelectionTransformation;
  // Update selection if needed
  if FSelNeedUpdate then begin
   UpdateSelection(Nil);
   FSelNeedUpdate := False;
  end;
 end;
 if Assigned(FOnEndSelectionUpdate) then FOnEndSelectionUpdate(Self);
 if (FSelUpdateCounter = 0) and Assigned(HistoryGroup) then
  FHistory.EndPanelGroup(HistoryGroup);
end;

function TFlexPanel.CreateSelMarkersRgn: HRGN;
var R: TRect;
    HalfX, HalfY: integer;

 procedure AddRect(Left, Top: integer);
 var R: TRect;
     Rgn: HRGN;
     //Rgn2: HRGN;
 begin
  R.Left := Left;
  R.Top := Top;
  R.Right := Left+SelectionMarkerSize;
  R.Bottom := Top+SelectionMarkerSize;
  Rgn := CreateRectRgnIndirect(R);
  try
   {if TabStop and not Focused then begin
    InflateRect(R, -2, -2);
    if not IsRectEmpty(R) then begin
     Rgn2 := CreateRectRgnIndirect(R);
     CombineRgn(Rgn, Rgn, Rgn2, RGN_XOR);
     DeleteObject(Rgn2);
    end;
   end; }
   if Result = 0 then begin
    Result := Rgn;
    Rgn := 0;
   end else
    CombineRgn(Result, Result, Rgn, RGN_XOR);
  finally
   if Rgn <> 0 then DeleteObject(Rgn);
  end;
 end;

begin
 Result := 0;
 if IsRectEmpty(FSelRect) or
    (FHideSelection and TabStop and not Focused) then exit;
 try
  CopyRect(R, FSelRect);
  TransformRect(R);
  InflateRect(R, SelectionInflate, SelectionInflate);
  with R do begin
   HalfX := ((Right-Left)-SelectionMarkerSize) div 2;
   HalfY := ((Bottom-Top)-SelectionMarkerSize) div 2;
   AddRect(Left, Top);
   AddRect(Left+HalfX, Top);
   AddRect(Right - SelectionMarkerSize, Top);
   AddRect(Left, Top+HalfY);
   AddRect(Right-SelectionMarkerSize, Top+HalfY);
   AddRect(Right - SelectionMarkerSize, Bottom - SelectionMarkerSize);
   AddRect(Left+HalfX, Bottom - SelectionMarkerSize);
   AddRect(Left, Bottom - SelectionMarkerSize);
  end;
 except
  if Result <> 0 then DeleteObject(Result);
  raise;
 end;
end;

procedure TFlexPanel.UpdateSelection(Control: TFlexControl);
var GroupRect, ControlRect:TRect;
    i: integer;
    Rgn: HRGN;
begin
 if FSelUpdateCounter > 0 then begin
  FSelNeedUpdate := True;
  exit;
 end;
 // Calculate bounds of selected area
 if FSelList.Count > 0 then begin
  GroupRect := TFlexControl(FSelList[0]).DocRect;
  with GroupRect do
  for i:=1 to FSelList.Count-1 do begin
   //UnionRect(GroupRect, GroupRect, TFlexControl(FSelList[i]).DocRect);
   ControlRect := TFlexControl(FSelList[i]).DocRect;
   if Left > ControlRect.Left then Left := ControlRect.Left;
   if Top > ControlRect.Top then Top := ControlRect.Top;
   if Right < ControlRect.Right then Right := ControlRect.Right;
   if Bottom < ControlRect.Bottom then Bottom := ControlRect.Bottom;
  end;
 end else
  SetRectEmpty(GroupRect);
 if not EqualRect(FSelRect, GroupRect) then begin
  // Update selection markers
  if Assigned(Parent) and not IsRectEmpty(FSelRect) then begin
   Rgn := CreateSelMarkersRgn;
   InvalidateRgn(Handle, Rgn, False);
   DeleteObject(Rgn);
  end;
  FSelRect := GroupRect;
  if Assigned(Parent) and not IsRectEmpty(FSelRect) then begin
   Rgn := CreateSelMarkersRgn;
   InvalidateRgn(Handle, Rgn, False);
   DeleteObject(Rgn);
  end;
 end;
 // Update edit/anchor points
 if FSelList.Count = 1 then begin
  if FEditPointControl <> Selected[0] then begin
   EditPointControl := Selected[0];
   if FEditPointControl <> Selected[0] then UpdatePoints;
  end else
   UpdatePoints;
 end else
 if Assigned(FEditPointControl)
  then EditPointControl := Nil
  else UpdatePoints;
end;

procedure TFlexPanel.DoSnapToGrid(HStep, VStep: integer;
  const SnapRect: TRect; var Delta: TPoint; SnapTo: TFlexSnaps = [snAll];
  ResizeCursor: TResizeCursor = rcrNone;
  HOffset: integer = 0; VOffset: integer = 0);
begin
 FGridControl.DoCustomSnap(SnapRect, Delta, HStep, VStep, HOffset, VOffset,
   ResizeCursor, SnapTo);
end;

function TFlexPanel.GetControlCount(Control: TFlexControl): integer;
var PassRec: TPassControlRec;
begin
 Result := 0;
 FirstControl(Control, PassRec);
 while Assigned(Control) do begin
  inc(Result);
  Control := NextControl(PassRec);
 end;
end;

function TFlexPanel.CreateFlexFiler(AStream: TStream;
  Process: TFlexFilerProcess; ABinary: boolean = False): TFlexFiler;
begin
 Result := TFlexFiler.Create(AStream);
 if Process in [ppSave, ppCopy] then Result.Binary := ABinary;
 Result.OnProgress := FilerProgress;
 FFilerProcess := Process;
end;

procedure TFlexPanel.FilerProgress(Sender: TObject; Progress: integer;
  Process: TFlexFilerProcess);
begin
 if not Assigned(FOnProgress) then exit;
 if Process = FFilerProcess then
  FOnProgress(Self, Progress, Process)
 else
 case FFilerProcess of
  ppCopy:
    case Process of
     ppLoad:
       FOnProgress(Self, 50 + Progress div 2, ppCopy);
     ppSave:
       FOnProgress(Self, Progress div 2, ppCopy);
    end;
 end;
end;

function TFlexPanel.LoadFlexControl(Filer: TFlexFiler; AParent: TFlexControl;
  const First: string; WithoutParent: boolean = false): TFlexControl;
var i, Delim: integer;
    s: string;
    IsError: boolean;
    ControlClass: TFlexControlClass;
    IsBinary: boolean;
begin
 Result := Nil;
 if Length(RegisteredFlexControls) = 0 then exit;
 IsBinary := Filer.Binary and (First = ''); 
 FHistory.DisableRecording;
 try
  BeginLoading;
  try
   // Extract class name
   if IsBinary then begin
    s := Filer.BinaryData.ReadCmdKeys[1];
    Delim := 0;
   end else begin
    Delim := Length(First);
    while (Delim > 1) and (First[Delim] <> ':') do dec(Delim);
    if Delim = 1 then begin
     Filer.LoadSkipToEnd;
     exit;
    end;
    s := Trim(copy(First, Delim+1, MaxInt));
   end;
   // Find class type by class name
   ControlClass := Nil;
   for i:=0 to High(RegisteredFlexControls) do
    if CompareText(RegisteredFlexControls[i].ClassName, s) = 0 then begin
     ControlClass := RegisteredFlexControls[i];
     break;
    end;
   if not Assigned(ControlClass) then begin
    Filer.LoadSkipToEnd;
    exit;
   end;
   try
    // Extract object name
    if IsBinary
     then s := Filer.BinaryData.ReadCmdKeys[0]
     else s := Trim(copy(First, Length(fcObject)+1, Delim-Length(fcObject)-1));
    {if OverrideExisting and Assigned(AParent) then begin
     // Find control in parent list
     Result := AParent.ByName[s];
     if Assigned(Result) and not (Result is ControlClass) then Result := Nil;
    end;}
    if not Assigned(Result) then
     // Create new control
     Result := ControlClass.Create(Self, AParent, Nil);

  {  if FLoadFunc = lfAdd then begin
     if FindControl(s) = Nil then Result.Name := s;
    end else   }
     Result.Name := s;
    IsError := false;
    if Result is TFlexLayer then begin
     if Assigned(Result.Parent) then begin
      if Result.Parent <> FLayers then IsError := true;
     end else
      Layers.Add(Result);
    end else
    if Result is TFlexCustomScheme then begin
     if Assigned(Result.Parent) then begin
      if Result.Parent <> FSchemes then IsError := true
     end else
      Schemes.Add(Result);
    end else
    if not Assigned(Result.Parent) and not WithoutParent then IsError := true;
    if IsError then begin
     Result.Free;
     Result := Nil;
     Filer.LoadSkipToEnd;
     exit;
    end;
    Result.LoadFromFiler(Filer);
    if FLoadFunc = lfAdd then begin
     if not Assigned(Result.Layer) and
        not (psDontStore in Result.FLayerProp.Style) then
      Result.Layer := FActiveLayer;
     if Assigned(Result.Parent) and (Result.Parent is TFlexCustomScheme) then
      Select(Result);
    end;
   except
    Result.Free;
    raise;
   end;
  finally
   EndLoading;
  end;
 finally
  FHistory.EnableRecording;
 end;
 FHistory.RecordAction(TControlCreateHistoryGroup, Result);
end;

function TFlexPanel.LoadFromStream(Stream: TStream): boolean;
var Filer: TFlexFiler;
begin
 Filer := CreateFlexFiler(Stream, ppLoad);
 try
  Result := LoadFromFiler(Filer, lfNew);
 finally
  Filer.Free;
 end;
end;

function TFlexPanel.LoadFromFile(const AFilename: string): boolean;
var FS: TFileStream;
    Filer: TFlexFiler;
begin
 FFileName := AFilename;
 FS := Nil;
 Filer := Nil;
 try
  FS := TFileStream.Create(AFilename, fmOpenRead);
  Filer := CreateFlexFiler(FS, ppLoad);
  Result := LoadFromFiler(Filer, lfNew);
 finally
  Filer.Free;
  FS.Free;
 end;
end;

function TFlexPanel.LoadFromFiler(Filer: TFlexFiler;
  LoadFunc: TFlexLoadFunc): boolean;
var s: string;
    Root: TFlexControl;
    i: integer;
begin
 BeginSelectionUpdate;
 try
  if LoadFunc = lfNew then FHistory.DisableRecording;
  BeginLoading;
  FLoadFunc := LoadFunc;
  Filer.Total := Filer.StreamSize;
  Root := Nil;
  case LoadFunc of
   lfNew : begin
            EmptyDocument;
           end;
   lfAdd : begin
            Root := FActiveScheme;
            UnselectAll;
           end;
  end;
  try
   s := Filer.LoadStr;
   if StrBeginsFrom(s, fcBinary) then s := Filer.LoadStr;
   FSaveAsBinary := Filer.Binary; 
   if not StrBeginsFrom(s, fcDocument) and
      not ((LoadFunc = lfAdd) and StrBeginsFrom(s, fcClipboard)) then
    raise Exception.Create('Data format error');
   i := Pos(' ', s);
   if i > 0 then FSchemes.Name := Trim(copy(s, i+1, MaxInt));
   repeat
    if Filer.Binary then begin
     if not Filer.BinaryData.ReadCommand then break;
     case Filer.BinaryData.ReadCmdCommand of
      fbcEnd    : break;
      fbcObject : LoadFlexControl(Filer, Root, '');
      else        FSchemes.Props.LoadFromFiler(Filer, '', PropRefList);
     end;
    end else begin
     if not Filer.LoadStrCheck(s) then break;
     if StrBeginsFrom(s, fcEnd) then
      break
     else
     if StrBeginsFrom(s, fcObject)
      then LoadFlexControl(Filer, Root, s)
      else FSchemes.Props.LoadFromFiler(Filer, s, PropRefList);
    end;
   until false;
   LoadFunc := lfNew;
  except
   if LoadFunc = lfNew then NewDocument;
   raise;
  end;
  //FRefPropsList.ResolveAllRefs;
 finally
  EndSelectionUpdate;
  EndLoading;
  if LoadFunc = lfNew then FHistory.EnableRecording;
 end;
 if FLoadFunc = lfNew then FModified := false;
 Result := true;
end;

function TFlexPanel.SaveToFile(const Filename: string): boolean;
var FS: TFileStream;
    Filer: TFlexFiler;
begin
 FS := Nil;
 Filer := Nil;
 try
  FS := TFileStream.Create(Filename, fmCreate);
  Filer := CreateFlexFiler(FS, ppSave);
  Result := SaveToFiler(Filer);
 finally
  Filer.Free;
  FS.Free;
 end;
end;

function TFlexPanel.SaveToStream(Stream: TStream): boolean;
var Filer: TFlexFiler;
begin
 Filer := CreateFlexFiler(Stream, ppSave);
 try
  Result := SaveToFiler(Filer);
 finally
  Filer.Free;
 end;
end;

function TFlexPanel.SaveToFiler(Filer: TFlexFiler; SelectedOnly: boolean = false;
  AControl: TFlexControl = Nil; ChildrenOnly: boolean = false): boolean;
var i, LayerIdx: integer;
    Scheme: TFlexCustomScheme;
    PO: TPaintOrder;
begin
 Filer.Binary := FSaveAsBinary;
 if not SelectedOnly then begin
  // Save flex-document completly
  Filer.SaveStr(fcDocument + ' ' + FSchemes.Name);
  if not Assigned(AControl) then begin
   AControl := FSchemes;
   ChildrenOnly := true;
  end;
  // Initialize control counters
  Filer.Total := GetControlCount(FLayers) - 1 +
                 GetControlCount(AControl) - Byte(ChildrenOnly);
  Filer.Saved := 0;
  // Save document properties and layers
  FSchemes.Props.SaveToFiler(Filer, IndentStep);
  for i:=0 to FLayers.Count-1 do
   Layers[i].SaveToFiler(Filer, IndentStep);
  // Save controls
  if ChildrenOnly then begin
   for i:=0 to AControl.Count-1 do
    AControl[i].SaveToFiler(Filer, IndentStep);
  end else
   AControl.SaveToFiler(Filer, IndentStep);
  // Reset modified flag if save whole document
  if AControl = FSchemes then FModified := false;
  Result := true;
 end else
 if FSelList.Count > 0 then begin
  // Save selected flex controls only
  Filer.SaveStr(fcClipboard);
  Filer.Total := 0;
  Scheme := Selected[0].ParentScheme;
  if Assigned(Scheme) then begin
   InitPaintOrder(PO);
   try
    Scheme.CreatePaintOrder(PO);
    for LayerIdx:=0 to High(PO.LayerRefs) do begin
     i := PO.LayerRefs[LayerIdx].First;
     while i >= 0 do with PO.ControlRefs[i] do begin
      if Control.IsSelected then Control.SaveToFiler(Filer, IndentStep);
      i := PO.ControlRefs[i].Next;
     end;
    end;
   finally
    ClearPaintOrder(PO);
   end;
  end;
  Result := true;
 end else
  // The selected list is empty
  Result := false;
 // Save "end-of-document"
 if Result then Filer.SaveStr(fcEnd);
end;

procedure TFlexPanel.Print(APrinter: TPrinter;
  PrintBackground, SelectedOnly: boolean);
var MF: TMetafile;
    MFCanvas: TMetafileCanvas;
    CoeffX, CoeffY: double;
    R: TRect;
begin
 MF := Nil;
 APrinter.BeginDoc;
 try
  FPaintSchemeBackground := PrintBackground;
  MF := TMetafile.Create;
  MF.Width := UnScalePixels(DocWidth);
  MF.Height := UnScalePixels(DocHeight);
  MFCanvas := TMetafileCanvas.Create(MF, APrinter.Handle);
  try
   PaintTo(MFCanvas,
     Rect(0, 0, UnScalePixels(DocWidth), UnScalePixels(DocHeight)),
     Point(0, 0), 100, Nil, True, False, SelectedOnly, True, True, True);
  finally
   MFCanvas.Free;
  end;
  R := Rect(0, 0, APrinter.PageWidth, APrinter.PageHeight);
  CoeffX := MF.Width / APrinter.PageWidth;
  CoeffY := MF.Height / APrinter.PageHeight;
  if CoeffX < CoeffY then begin
   R.Right := Round(MF.Width / CoeffY);
   OffsetRect(R, (APrinter.PageWidth - R.Right) div 2, 0);
  end else begin
   R.Bottom := Round(MF.Height / CoeffX);
   OffsetRect(R, 0, (APrinter.PageHeight - R.Bottom) div 2);
  end;
  APrinter.Canvas.StretchDraw(R, MF);
 finally
  FPaintSchemeBackground := True;
  MF.Free;
  APrinter.EndDoc;
 end;
end;

function TFlexPanel.Select(AControl: TFlexControl): boolean;
var i: integer;
    Found: boolean;
begin
 Result := false;
 if not Assigned(AControl) or (FSelList.IndexOf(AControl) >= 0) or
    not AControl.IsSelectable then exit;
 Found := false;
 for i:=0 to FSchemes.Count-1 do
  if AControl.Parent = Schemes[i] then begin
   Found := true;
   break;
  end;
 if not Found then exit;
 FSelList.Add(AControl);
 if (FSelList.Count <> 1) or (AControl <> FEditPointControl) then
  FEditPointControl := Nil;
 DoNotify(AControl, fnSelect);
 Result := true;
end;

function TFlexPanel.SelectNext: TFlexControl;
var EndIndex, Index: integer;
    Control, SelControl: TFlexControl;
begin
 Result := Nil;
 if not Assigned(FActiveScheme) or (FActiveScheme.Count = 0) then exit;
 Index := 0;
 SelControl := Nil;
 if FSelList.Count > 0 then begin
  SelControl := TFlexControl(FSelList[0]);
  Index := FActiveScheme.IndexOf(SelControl);
  if Index >= 0 then begin
   inc(Index);
   if Index = FActiveScheme.Count then Index := 0;
  end else
   Index := 0;
 end;
 EndIndex := Index;
 repeat
  Control := TFlexControl(FActiveScheme[Index]);
  if (Control <> SelControl) and Control.IsSelectable then begin
   // Next selectable control found
   UnselectAll;
   if Select(Control) then Result := Control;
   break;
  end;
  inc(Index);
  if Index = FActiveScheme.Count then Index := 0;
 until Index = EndIndex;
end;

function TFlexPanel.SelectPrev: TFlexControl;
var EndIndex, Index: integer;
    Control, SelControl: TFlexControl;
begin
 Result := Nil;
 if not Assigned(FActiveScheme) or (FActiveScheme.Count = 0) then exit;
 Index := 0;
 SelControl := Nil;
 if FSelList.Count > 0 then begin
  SelControl := TFlexControl(FSelList[0]);
  Index := FActiveScheme.IndexOf(SelControl);
  if Index <= 0
   then Index := FActiveScheme.Count-1
   else dec(Index);
 end;
 EndIndex := Index;
 repeat
  Control := TFlexControl(FActiveScheme[Index]);
  if (Control <> SelControl) and Control.IsSelectable then begin
   // Next selectable control found
   UnselectAll;
   if Select(Control) then Result := Control;
   break;
  end;
  dec(Index);
  if Index < 0 then Index := FActiveScheme.Count - 1;
 until Index = EndIndex;
end;

function TFlexPanel.Unselect(AControl: TFlexControl): boolean;
var Index: integer;
begin
 Result := false;
 Index := FSelList.IndexOf(AControl);
 if Index < 0 then exit;
 FSelList.Delete(Index{AControl});
 if (FSelList.Count <> 1) or (FSelList[0] <> FEditPointControl) then
  FEditPointControl := Nil;
 DoNotify(AControl, fnSelect);
 if FToolMode in [ ftmMove, ftmMoving, ftmResize, ftmResizing, ftmCreating,
                   ftmPointEdit, ftmPointEditing ] then
  ToolMode := FDefaultToolMode;
 Result := true;
end;

procedure TFlexPanel.UnselectAll;
var i: integer;
begin
 BeginSelectionUpdate;
 try
  for i:=FSelList.Count-1 downto 0 do
   Unselect(TFlexControl(FSelList[i]));
 finally
  EndSelectionUpdate;
 end;
end;

procedure TFlexPanel.BackOne;
var i, Index: integer;
    Control: TFlexControl;
begin
 FHistory.BeginPanelGroup(TPanelOrderHistoryGroup);
 try
  for i:=0 to FSelList.Count-1 do begin
   Control := Selected[i];
   if not Assigned(Control.Parent) then continue;
   Index := Control.Parent.IndexOf(Control);
   if Index > 0 then Control.Parent.ChangeOrder(Index, Index-1);
  end;
 finally
  FHistory.EndPanelGroup(TPanelOrderHistoryGroup);
 end;
end;

procedure TFlexPanel.ForwardOne;
var i, Index: integer;
    Control: TFlexControl;
begin
 FHistory.BeginPanelGroup(TPanelOrderHistoryGroup);
 try
  for i:=0 to FSelList.Count-1 do begin
   Control := Selected[i];
   if not Assigned(Control.Parent) then continue;
   Index := Control.Parent.IndexOf(Control);
   if (Index >= 0) and (Index < Control.Parent.Count-1) then
    Control.Parent.ChangeOrder(Index, Index+1);
  end;
 finally
  FHistory.EndPanelGroup(TPanelOrderHistoryGroup);
 end;
end;

procedure TFlexPanel.ToBack;
var i, Index: integer;
    Control: TFlexControl;
begin
 if FSelList.Count = 0 then exit;
 Control := TFlexControl(FSelList[0]);
 while Assigned(Control) and not (Control is TFlexCustomScheme) do
  Control := Control.Parent;
 if not Assigned(Control) then exit;
 FHistory.BeginPanelGroup(TPanelOrderHistoryGroup);
 try
  Index := 0;
  for i:=0 to Control.Count-1 do
   if FSelList.IndexOf(Control.Controls[i]) >= 0 then begin
    Control.ChangeOrder(i, Index);
    inc(Index);
   end;
 finally
  FHistory.EndPanelGroup(TPanelOrderHistoryGroup);
 end;
end;

procedure TFlexPanel.ToFront;
var i, Index: integer;
    Control: TFlexControl;
begin
 if FSelList.Count = 0 then exit;
 Control := TFlexControl(FSelList[0]);
 while Assigned(Control) and not (Control is TFlexCustomScheme) do
  Control := Control.Parent;
 if not Assigned(Control) then exit;
 FHistory.BeginPanelGroup(TPanelOrderHistoryGroup);
 try
  Index := Control.Count-1;
  for i:=Control.Count-1 downto 0 do
   if FSelList.IndexOf(Control.Controls[i]) >= 0 then begin
    Control.ChangeOrder(i, Index);
    dec(Index);
   end;
 finally
  FHistory.EndPanelGroup(TPanelOrderHistoryGroup);
 end;   
end;

function TFlexPanel.CloneSelected(ShiftX: integer = 0; ShiftY: integer = 0;
     CloneClass: TFlexCloneClass = nil): TFlexClone;
var i, Index, MaxIndex: integer;
    Control, Parent: TFlexControl;
    Clone: TFlexClone;
    Layer: TFlexLayer;
    Sources: TList;
    Source: TFlexControl;
begin
 Result := Nil;
 if FSelList.Count < 1 then exit;
 Sources := Nil;
 BeginSelectionUpdate(false, TPanelCloneHistoryGroup);
 try
  Sources := TList.Create;
  for i:=0 to FSelList.Count-1 do Sources.Add(FSelList[i]);
  UnselectAll;
  if not Assigned(CloneClass) then CloneClass := TFlexClone;
  for i:=0 to Sources.Count-1 do begin
   Control := TFlexControl(Sources[i]);
   if Control is TFlexClone
    then Source := TFlexClone(Control).SourceProp.LinkedControl    //continue;
    else Source := Control;
   Layer := Control.Layer;
   if not Assigned(Layer) or Layer.ReadOnly then continue;
   Parent := Control.Parent;
   if not Assigned(Parent) then continue;
   MaxIndex := Parent.IndexOf(Control);
   Clone := CloneClass.Create(Self, Parent, Layer);
   with Clone do begin
    BeginUpdate;
    try
     Left := Control.Left + ShiftX;
     Top := Control.Top + ShiftY;
     Width := Control.Width;
     Height := Control.Height;
    finally
     EndUpdate;
    end;
   end;
   Clone.Layer := Layer;
   Index := Parent.IndexOf(Clone);
   Parent.ChangeOrder(Index, MaxIndex + 1);
   Clone.SourceProp.LinkedControl := Source; //Control;
   Select(Clone);
   if not Assigned(Result) then Result := Clone;
  end;
 finally
  Sources.Free;
  EndSelectionUpdate(false, TPanelCloneHistoryGroup);
 end;
end;

function TFlexPanel.Group(GroupClass: TFlexGroupClass = nil): TFlexGroup;
var i, Index, MaxIndex, Count: integer;
    Control, Parent: TFlexControl;
    Layer: TFlexLayer;
    //SelRect: TRect;
begin
 Result := Nil;
 if FSelList.Count < 2 then exit;
 Control := Selected[0];
 Layer := Control.Layer;
 if not Assigned(Layer) or Layer.ReadOnly then exit;
 Parent := Control.Parent;
 if not Assigned(Parent) then exit;
 MaxIndex := Parent.IndexOf(Control);
 Count := FSelList.Count;
 for i:=1 to FSelList.Count-1 do begin
  Control := Selected[i];
  if (Control.Layer <> Layer) or (Control.Parent <> Parent) then exit;
  Index := Parent.IndexOf(Control);
  if Index > MaxIndex then MaxIndex := Index;
  // Can't group connectors
  if Control.IsConnectorControl then dec(Count);
 end;
 if Count < 2 then exit;
 if not Assigned(GroupClass) then GroupClass := TFlexGroup;
 FHistory.DisableRecording;
 try
  Result := GroupClass.Create(Self, Parent, Layer);
  with Result do begin
   Left := FSelRect.Left;
   Top := FSelRect.Top;
   Width := FSelRect.Right - FSelRect.Left;
   Height := FSelRect.Bottom - FSelRect.Top;
  end;
  Result.Layer := Layer;
  Index := Parent.IndexOf(Result);
  Parent.ChangeOrder(Index, MaxIndex);
  BeginSelectionUpdate;
  try
   Select(Result);
   Result.BeginGroupUngroup;
   try
    i := 0;
    while i < Parent.Count do begin
     Control := Parent[i];
     if (Control = Result) or (FSelList.IndexOf(Control) < 0) or
        // Can't group connectors
        Control.IsConnectorControl then begin
      inc(i);
      continue;
     end;
     Control.Parent := Result;
     Control.Left := Control.Left - Result.Left;
     Control.Top := Control.Top - Result.Top;
    end;
   finally
    Result.EndGroupUngroup;
   end;
   Result.Visible := True;
  finally
   EndSelectionUpdate;
  end;
 finally
  FHistory.EnableRecording;
 end;
 FHistory.RecordAction(TControlGroupHistoryAction, Result);
end;

function TFlexPanel.Ungroup: boolean;
var i, Index: integer;
    Control, AParent: TFlexControl;
    GroupControl: TFlexControl;
    ALayer: TFlexLayer;
    Groups: TList;
begin
 //if (FSelList.Count <> 1) or not (Selected[0] is TFlexGroup) then exit;
 Result := false;
 Groups := Nil;
 FHistory.BeginPanelGroup(TPanelUngroupControlsHistoryGroup);
 try
  BeginSelectionUpdate;
  try
   Groups := TList.Create;
   for i:=0 to FSelList.Count-1 do
    if TFlexControl(FSelList[i]).IsUngroupable then Groups.Add(FSelList[i]);
   for i:=0 to Groups.Count-1 do begin
    GroupControl := TFlexControl(Groups[i]);
    FHistory.RecordAction(TControlUngroupHistoryAction, GroupControl);
    FHistory.DisableRecording;
    try
     ALayer := GroupControl.Layer;
     AParent := GroupControl.Parent;
     if not Assigned(ALayer) or not Assigned(AParent) then continue;
     Index := AParent.IndexOf(GroupControl);
     if Index < 0 then continue;
     GroupControl.BeginGroupUngroup;
     try
      while GroupControl.Count > 0 do begin
       Control := GroupControl[GroupControl.Count-1];
       Control.Left := Control.Left + GroupControl.Left;
       Control.Top := Control.Top + GroupControl.Top;
       Control.Parent := AParent;
       Control.Layer := ALayer;
       AParent.ChangeOrder(AParent.Count-1, Index);
       Select(Control);
      end;
     finally
      GroupControl.EndGroupUngroup;
     end;
     GroupControl.Free;
     Result := true;
    finally
     FHistory.EnableRecording;
    end;
   end;
  finally
   Groups.Free;
   EndSelectionUpdate;
  end;
 finally
  FHistory.EndPanelGroup(TPanelUngroupControlsHistoryGroup);
 end;
end;

procedure TFlexPanel.AlignSelected(Align: TFlexAlign);
var i: integer;
    R, Base: TRect;
begin
 if FSelList.Count < 2 then exit;
 BeginSelectionUpdate(false, TPanelAlignHistoryGroup);
 try
  Base := Selected[FSelList.Count-1].DocRect;
  for i:=0 to FSelList.Count-2 do with Selected[i] do begin
   R := DocRect;
   if (Align = faHCenter) or (Align = faCenter) then
    OffsetRect(R, Base.Left + (Base.Right - Base.Left) div 2 -
                    (R.Left + (R.Right - R.Left) div 2), 0);
   if (Align = faVCenter) or (Align = faCenter) then
    OffsetRect(R, 0, Base.Top + (Base.Bottom - Base.Top) div 2 -
                      (R.Top + (R.Bottom - R.Top) div 2));
   case Align of
    faLeft:
      OffsetRect(R, Base.Left - R.Left, 0);
    faRight:
      OffsetRect(R, Base.Right - R.Right, 0);
    faTop:
      OffsetRect(R, 0, Base.Top - R.Top);
    faBottom:
      OffsetRect(R, 0, Base.Bottom - R.Bottom);
   end;
   DocRect := R;
  end;
 finally
  EndSelectionUpdate(false, TPanelAlignHistoryGroup);
 end;
end;

procedure TFlexPanel.Rotate(ADegree: integer; AMirror: boolean);
var TranslateInfo: TTranslateInfo;
begin
 if FSelList.Count = 0 then exit;
 with TranslateInfo, FSelRect do begin
  Center.X := Left + (Right - Left {- PixelScaleFactor}) div 2;
  Center.Y := Top  + (Bottom - Top {- PixelScaleFactor}) div 2;
  Rotate := ADegree;
  Mirror := AMirror;
 end;
 Translate(TranslateInfo);
end;

procedure TFlexPanel.Translate(var TranslateInfo: TTranslateInfo);
var i: integer;
begin
 if FSelList.Count = 0 then exit;
 with TranslateInfo do
  Rotate := Round((Rotate mod 360) / 90) * 90;
 BeginSelectionUpdate(true, TPanelSizeMoveHistoryGroup);
 try
  for i:=0 to FSelList.Count-1 do with TFlexControl(FSelList[i]) do begin
   if Assigned(Layer) and Layer.ReadOnly then continue;
   Translate(TranslateInfo);
  end;
 finally
  EndSelectionUpdate(true, TPanelSizeMoveHistoryGroup);
 end;
end;

procedure TFlexPanel.Zoom(AScale: integer; ZoomRect: PRect);
var Center: TPoint;
begin
 if not Assigned(ZoomRect) then begin
  Center.X := ClientWidth div 2;
  Center.Y := ClientHeight div 2;
  Center := Schemes.OwnerToClient(Center);
  //UnTransformPoint(Center.X, Center.Y);
 end else
 with ZoomRect^ do begin
  Center.X := Left + (Right - Left) div 2;
  Center.Y := Top + (Bottom - Top) div 2;
 end;
 Scale := AScale;
 //TransformPoint(Center);
 Center.X := ScaleValue(Center.X, Scale);
 Center.Y := ScaleValue(Center.Y, Scale);
 HorzScrollBar.Position := HorzExtraSpace + Center.X - ClientWidth div 2;
 VertScrollBar.Position := VertExtraSpace + Center.Y - ClientHeight div 2;
 UpdateOrigin;
 UpdateSelection(Nil);
 Invalidate;
end;

procedure TFlexPanel.MoveSelected(ShiftX, ShiftY: integer);
var i: integer;
    Action: THistoryGroupClass;
begin
 if ((ShiftX = 0) and (ShiftY = 0)) or (FSelList.Count = 0) then exit;
 if (FToolMode in [ftmCreating, ftmMoving, ftmResizing]) and
    (FHistory.State = hsRecord)
  then Action := Nil // Already recording
  else Action := TPanelSizeMoveHistoryGroup;
 BeginSelectionUpdate(true, Action);
 try
  for i:=0 to FSelList.Count-1 do
   with TFlexControl(FSelList[i]) do
   if Assigned(Parent) and (Parent is TFlexCustomScheme) then begin
    BeginUpdate;
    try
     Left := Left + ShiftX;
     Top := Top + ShiftY;
    finally
     EndUpdate;
    end;
   end;
 finally
  EndSelectionUpdate(true, Action);
 end;
end;

procedure TFlexPanel.ResizeSelected(DeltaX, DeltaY: integer;
  Proportional: boolean = false; UseGrid: boolean = true;
  ResizeCursor: TResizeCursor = rcrBottomRight; UseDefaultSnap: boolean = true;
  ProportionalToMax: boolean = false);
var Size_, Dir: TPoint;
    OrigTool: TFlexToolMode;
    CoeffX, CoeffY: extended;
    SelRect, DestRect, OrigRect, R: TRect;
    i, NewWidth, NewHeight: integer;
    Control, TestControl: TFlexControl;
    PassRec: TPassControlRec;
    Delta: TPoint;
    Info: TTranslateInfo;
    HMirror, VMirror: boolean;
    SnapStyle: TFlexSnaps;
    Sibling: boolean;
begin
 OrigTool := FToolMode;
 if not (OrigTool in [ftmResizing, ftmCreating]) then ToolMode := ftmResizing;
 try
  if UseGrid then with FGridControl do
   if Snap then begin
    Delta := Point(DeltaX, DeltaY);
    if UseDefaultSnap then begin
     SnapStyle := [ snLeft, snTop, snRight, snBottom ];
     if SnapStyle * SnapTo = [] then
      SnapStyle := SnapTo
     else
     if snCenter in SnapTo then Include(SnapStyle, snCenter);
    end else
     SnapStyle := SnapTo;
    DoCustomSnap(FResizeRect, Delta, HSize, VSize, HOffset, VOffset,
      ResizeCursor, SnapStyle);
    DeltaX := Delta.x;
    DeltaY := Delta.y;
   end;
  if ResizeCursor in [rcrTop, rcrBottom] then DeltaX := 0;
  if ResizeCursor in [rcrLeft, rcrRight] then DeltaY := 0;
  with FSelRect do begin
   Info.Center.X := Left + (Right - Left) div 2;
   Info.Center.Y := Top + (Bottom - Top) div 2;
  end;
  with FResizeRect do begin
   Size_.X := Right - Left;
   Size_.Y := Bottom - Top;
  end;
  // Calc directions
  if ResizeCursor in [rcrTop, rcrTopRight, rcrRight, rcrBottomRight, rcrBottom]
   then Dir.X := +1
   else Dir.X := -1;
  if ResizeCursor in [rcrLeft, rcrBottomLeft, rcrBottom, rcrBottomRight, rcrRight]
   then Dir.Y := +1
   else Dir.Y := -1;
  // Calc Size_
  SetRectEmpty(DestRect);
  DestRect.Right := Abs(Size_.X) + Dir.X * DeltaX;
  DestRect.Bottom := Abs(Size_.Y) + Dir.Y * DeltaY;
  // Check proportional resizing
  if Proportional then begin
   if Size_.Y = 0
    then NewWidth := DestRect.Bottom
    else NewWidth := Abs(Round(Size_.X / Size_.Y * DestRect.Bottom));
   if Size_.X = 0
    then NewHeight := DestRect.Right
    else NewHeight := Abs(Round(Size_.Y / Size_.X * DestRect.Right));
   if (Abs(DestRect.Bottom) - NewHeight < 0) = ProportionalToMax then begin
    if DestRect.Bottom < 0
     then DestRect.Bottom := -NewHeight
     else DestRect.Bottom := NewHeight;
    DeltaY := (DestRect.Bottom - Abs(Size_.Y)) * Dir.Y;
   end else begin
    if DestRect.Right < 0
     then DestRect.Right := -NewWidth
     else DestRect.Right := NewWidth;
    DeltaX := (DestRect.Right - Abs(Size_.X)) * Dir.X;
   end;
  end;
  BeginSelectionUpdate(true);
  try
   // Process mirroring
   HMirror := (DestRect.Right >= 0) <> (Size_.X >= 0);
   if HMirror then begin
    // Horizontal mirror
    Info.Mirror := true;
    Info.Rotate := 0;
    Translate(Info);
    // Swap X-coords
    i := FResizeRect.Left;
    FResizeRect.Left := FResizeRect.Right;
    FResizeRect.Right := i;
    Size_.X := -Size_.X;
   end;
   VMirror := (DestRect.Bottom >= 0) <> (Size_.Y >= 0);
   if VMirror then begin
    // Vertical mirror
    Info.Mirror := true;
    Info.Rotate := 180;
    Translate(Info);
    // Swap Y-coords
    i := FResizeRect.Top;
    FResizeRect.Top := FResizeRect.Bottom;
    FResizeRect.Bottom := i;
    Size_.Y := -Size_.Y;
   end;
   // Calc destination rect
   if DestRect.Right >= 0 then begin
    DestRect.Left := FResizeRect.Left;
    if Dir.X < 0 then inc(DestRect.Left, DeltaX);
   end else begin
    DestRect.Right := -DestRect.Right;
    DestRect.Left := FResizeRect.Right;
    if Dir.X < 0
     then inc(DestRect.Left, Abs(Size_.X))
     else dec(DestRect.Left, DestRect.Right);
   end;
   inc(DestRect.Right, DestRect.Left);
   if DestRect.Bottom >= 0 then begin
    DestRect.Top := FResizeRect.Top;
    if Dir.Y < 0 then inc(DestRect.Top, DeltaY);
   end else begin
    DestRect.Bottom := -DestRect.Bottom;
    DestRect.Top := FResizeRect.Bottom;
    if Dir.Y < 0
     then inc(DestRect.Top, Abs(Size_.Y))
     else dec(DestRect.Top, DestRect.Bottom);
   end;
   inc(DestRect.Bottom, DestRect.Top);
   // Calc scale coeff's
   if Size_.X = 0
    then CoeffX := 1
    else CoeffX := RectWidth(DestRect) / Abs(Size_.X);
   if Size_.Y = 0
    then CoeffY := 1
    else CoeffY := RectHeight(DestRect) / Abs(Size_.Y);
   // Resize all selected controls
   SelRect := FSelRect;
   //for i:=0 to FSelList.Count-1 do begin
   i := FSelList.Count-1;
   while i >= 0 do begin
    Control := Selected[i];
    FirstControl(Control, PassRec);
    try
     while Assigned(Control) do with Control do begin
      if not Assigned(Layer) or not Layer.ReadOnlyProp.Value then begin
       R := FResizingRect;
       // Check mirroring
       if HMirror or VMirror then MirrorInResize(HMirror, VMirror);
       if Size_.X < 0 then begin
        R.Right := Abs(RectWidth(FResizeRect)) - R.Left;
        R.Left := R.Right - RectWidth(FResizingRect);
       end;
       if Size_.Y < 0 then begin
        R.Bottom := Abs(RectHeight(FResizeRect)) - R.Top;
        R.Top := R.Bottom - RectHeight(FResizingRect);
       end;
       // Scale
       R.Left := Round(R.Left * CoeffX);
       R.Right := Round(R.Right * CoeffX);
       R.Top := Round(R.Top * CoeffY);
       R.Bottom := Round(R.Bottom * CoeffY);
       // Offset
       OffsetRect(R, DestRect.Left, DestRect.Top);
       // Check parent Read-Only
       OrigRect := DocRect;
       TestControl := Control.Parent;
       while Assigned(TestControl) and not
         (TestControl is TFlexCustomScheme) do begin
        if psReadOnly in TestControl.WidthProp.Style then begin
         R.Left := OrigRect.Left;
         R.Right := OrigRect.Right;
        end;
        if psReadOnly in TestControl.HeightProp.Style then begin
         R.Top := OrigRect.Top;
         R.Bottom := OrigRect.Bottom;
        end;
        TestControl := TestControl.Parent;
       end;
       // Setup new coord
       DocRect := R;
       with DocRect do
        Sibling :=
          ((Right - Left) = (OrigRect.Right - OrigRect.Left)) and
          ((Bottom - Top) = (OrigRect.Bottom - OrigRect.Top));
      end else
       Sibling := false;
      // Next child control
      Control := NextControl(PassRec, Sibling);
     end;
    finally
     ClosePassRec(PassRec);
    end;
    dec(i);
   end; { while //for }
  finally
   EndSelectionUpdate(true);
  end;
 finally
  ToolMode := OrigTool;
 end;
end;

procedure TFlexPanel.Duplicate(ShiftX, ShiftY: integer);
var MS: TMemoryStream;
    Filer: TFlexFiler;
begin
 if FSelList.Count = 0 then exit;
 MS := Nil;
 Filer := Nil;
 FHistory.BeginPanelGroup(TPanelDuplicateHistoryGroup);
 try
  MS := TMemoryStream.Create;
  Filer := CreateFlexFiler(MS, ppCopy);
  SaveToFiler(Filer, True);
  Filer.Rewind;
  LoadFromFiler(Filer, lfAdd);
  MoveSelected(ShiftX, ShiftY);
 finally
  FHistory.EndPanelGroup(TPanelDuplicateHistoryGroup);
  MS.Free;
  Filer.Free;
 end;
end;

procedure TFlexPanel.CopyToClipboard;
var R: TRect;
    MF: TMetafile;
    MFCanvas: TMetafileCanvas;
    //B: TBitmap;
    MS: TMemoryStream;
    Filer: TFlexFiler;
    MemPtr: pointer;
    AHandle: Cardinal;
begin
 if FSelList.Count = 0 then exit;
 with Clipboard do begin
  Open;
  try
   Clear;
   // Create Metafile clipboard data
   MF := TMetafile.Create;
   try
    MF.Width := UnscalePixels(FSelRect.Right - FSelRect.Left);
    MF.Height := UnscalePixels(FSelRect.Bottom - FSelRect.Top);
    MFCanvas := TMetafileCanvas.Create(MF, 0);
    try
     R.Left := UnscalePixels(FSelRect.Left);
     R.Right := UnscalePixels(FSelRect.Right);
     R.Top := UnscalePixels(FSelRect.Top);
     R.Bottom := UnscalePixels(FSelRect.Bottom);
     //PaintTo(MFCanvas, R, Point(0, 0), 100, Nil, False, False, True, True, True);
     PaintTo(MFCanvas, Rect(0, 0, R.Right - R.Left, R.Bottom - R.Top),
       R.TopLeft, 100, Nil, False, False, True, True, True);
    finally
     MFCanvas.Free;
    end;
    AHandle := CopyEnhMetafile(MF.Handle, Nil);
    SetAsHandle(CF_ENHMETAFILE, AHandle);
   finally
    MF.Free;
   end;
   // Create FlexGraphics clipboard data
   MS := Nil;
   Filer := Nil;
   try
    MS := TMemoryStream.Create;
    Filer := CreateFlexFiler(MS, ppSave);
    SaveToFiler(Filer, True);
    AHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, MS.Size);
    try
     MemPtr := GlobalLock(AHandle);
     Move(MS.Memory^, MemPtr^, MS.Size);
     GlobalUnlock(AHandle);
     SetClipboardData(CF_FLEXDOC, AHandle);
    except
     GlobalFree(AHandle);
     raise;
    end;
   finally
    MS.Free;
    Filer.Free;
   end;
  finally
   Close;
  end;
 end;
end;

procedure TFlexPanel.CutToClipboard;
begin
 CopyToClipboard;
 DeleteSelected;
end;

function TFlexPanel.PasteAvailable(FlexDocOnly: boolean = false): boolean;
begin
 Result := Clipboard.HasFormat(CF_FLEXDOC);
 {$IFDEF STDFLEXCTRLS}
 if Result or FlexDocOnly then exit;
 // Check other supported formats
 Result := Clipboard.HasFormat(CF_PICTURE) or Clipboard.HasFormat(CF_TEXT);
 {$ENDIF}
end;

procedure TFlexPanel.PasteFromClipboard(FlexDocOnly: boolean = false);
var AHandle: Cardinal;
    AFormat: word;
    MS: TMemoryStream;
    Filer: TFlexFiler;
    MemPtr: pointer;
    Size: integer;
    {$IFDEF STDFLEXCTRLS}
    Picture: TPicture;
    AControl: TFlexControl;
    R: TRect;
    {$ENDIF}

 function FindPictureFormat: word;
 begin
  Result := EnumClipboardFormats(0);
  while (Result <> 0) and not TPicture.SupportsClipboardFormat(Result) do
   Result := EnumClipboardFormats(Result);
 end;

begin
 FHistory.BeginPanelGroup(TPanelCreateControlsHistoryGroup);
 try
  with Clipboard do begin
   Open;
   try
    AHandle := GetAsHandle(CF_FLEXDOC);
    if AHandle <> 0 then begin
     // Paste flex document
     MS := Nil;
     Filer := Nil;
     try
      MS := TMemoryStream.Create;
      Size := GlobalSize(AHandle);
      MemPtr := GlobalLock(AHandle);
      try
       MS.Write(MemPtr^, Size);
      finally
       GlobalUnlock(AHandle);
      end;
      MS.Position := 0;
      Filer := CreateFlexFiler(MS, ppLoad);
      LoadFromFiler(Filer, lfAdd);
     finally
      MS.Free;
      Filer.Free;
     end;
    end
    {$IFDEF STDFLEXCTRLS}
    else
    if not FlexDocOnly then begin
     // Test other formats in "desirable" order
     AControl := Nil;
     if Clipboard.HasFormat(CF_PICTURE) then begin
      // Formats CF_ENHMETAFILE, CF_METAFILEPICT, CF_BITMAP supports by TPicture
      Picture := TPicture.Create;
      try
       // Load picture from clipboard
       AFormat := FindPictureFormat;
       if AFormat = 0 then exit;
       AHandle := GetAsHandle(AFormat);
       Picture.LoadFromClipboardFormat(AFormat, AHandle, 0);
       AControl := TFlexPicture.Create(Self, ActiveScheme, ActiveLayer);
       with TFlexPicture(AControl) do begin
        PictureProp.Graphic := Picture.Graphic;
        AutoSizeProp.Value := true;
        AutoSizeProp.Value := false;
       end;
      finally
       Picture.Free;
      end;
     end else
     if Clipboard.HasFormat(CF_TEXT) then begin
      // Create stadard text control
      AControl := TFlexText.Create(Self, ActiveScheme, ActiveLayer);
      with TFlexText(AControl) do begin
       TextProp.Text := Clipboard.AsText;
       AutoSizeProp.Value := true;
       AutoSizeProp.Value := false;
      end;
     end;
     if Assigned(AControl) then begin
      // Move AControl in center of currently visible panel area
      R := Rect(0, 0, ClientWidth, ClientHeight);
      UnTransformPoint(R.Left, R.Top);
      UnTransformPoint(R.Right, R.Bottom);
      AControl.BeginUpdate;
      try
       AControl.Left := (R.Left + R.Right - AControl.WidthProp.Value) div 2;
       AControl.Top := (R.Top + R.Bottom - AControl.HeightProp.Value) div 2;
      finally
       AControl.EndUpdate;
      end;
     end;
    end;
    {$ENDIF}
   finally
    Close;
   end;
  end;
 finally
  FHistory.EndPanelGroup(TPanelCreateControlsHistoryGroup);
 end;
end;

procedure TFlexPanel.DeleteSelected;
var i: integer;
begin
 BeginSelectionUpdate(false, TPanelDestroyControlsHistoryGroup);
 try
  i := FSelList.Count-1;
  while i >= 0 do begin
   Selected[i].Free;
   dec(i);
  end;
 finally
  EndSelectionUpdate(false, TPanelDestroyControlsHistoryGroup);
 end;
end;

///////////////////////////////////////////////////////////////////////////////

procedure RegisterControls;
begin
 RegisterFlexControl(TFlexLayer);
 RegisterFlexControl(TFlexScheme);
 RegisterFlexControl(TFlexControl);
 RegisterFlexControl(TFlexServiceControl);
 RegisterFlexControl(TFlexGroup);
 RegisterFlexControl(TFlexClone);
end;

initialization
 RegisterControls;
 // Set default FlexPanel's name resolving routies
 GetNameLinkByFlexPanel := DefaultGetNameLinkByFlexPanel;
 GetFlexPanelByNameLink := DefaultGetFlexPanelByNameLink;

end.
