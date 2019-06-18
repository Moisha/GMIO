/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    Path procedures and functions                    //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexPath;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes, FlexUtils;

type
  TPointType = ( ptNode, ptEndNode, ptEndNodeClose, ptControl );

  TPointArray = packed array of TPoint;
  TPointTypeArray = packed array of TPointType;
  TSelectedArray = packed array of boolean;
  TRectArray = packed array of TRect;

  TPathEditFunc   = ( pfOffset, {pfDelete, }pfJoin, pfBreak, pfClose,
    pfToLine, pfToCurve );
  TPathEditFuncs  = set of TPathEditFunc;

  PPathEditParams = ^TPathEditParams;
  TPathEditParams = record
   {$IFNDEF FG_C10}
   case TPathEditFunc of
    pfOffset: (
   {$ENDIF}
      Offset: TPoint;
      MoveControls: boolean;
    {$IFNDEF FG_C10}
    );
    {$ENDIF}
  end;

  PNearestPoint = ^TNearestPoint;
  TNearestPoint = record
   Point: TPoint;
   Index0: integer;
   Index1: integer;
   MinIndex: integer;
   MinSqrDist: single;
   CurvePos: single;
   IsNewMin: boolean;
  end;

  PPathFigureInfo = ^TPathFigureInfo;
  TPathFigureInfo = record
   FirstNode: integer;
   LastNode: integer;
   LastPoint: integer;
   IsClosed: boolean;
   IsCurve: boolean;
  end;

  TPathFigureInfos = array of TPathFigureInfo;

  PPathInfo = ^TPathInfo;
  TPathInfo = record
   PointCount: integer;
   IsCurve: boolean;
   Figures: TPathFigureInfos;
  end;

  TRerouteMode = (
   rmAlways,
   rmAsNeeded,
   rmNever
  );

  PRerouteParams = ^TRerouteParams;
  TRerouteParams = record
   RangeA: TRect;
   RangeB: TRect;
   Mode: TRerouteMode;
   Ortogonal: boolean;
   LinkMinGap: integer;
   SelfLink: boolean;
   LinkPointA: TPoint;
   LinkPointB: TPoint;
  end;

function  CalcPath(const Points: TPointArray; const Types: TPointTypeArray;
  var Range: TRect; Info: PPathInfo = Nil): boolean;
function  CreatePath(DC: HDC; const Points: TPointArray;
  const Types: TPointTypeArray; UseClosed, UseNotClosed: boolean;
  var Complete: boolean; OriginalBezier: boolean = false;
  Info: PPathInfo = Nil): boolean;
function  PointOnLine(const p, p0, p1: TPoint; StrokeWidth: integer;
  ScanLinePoints: TList = Nil; Nearest: PNearestPoint = Nil): boolean;
function  PointOnPath(const Points: TPointArray; const Types: TPointTypeArray;
  const Point: TPoint; Stroked, Filled: boolean; StrokeWidth: integer = 0;
  Nearest: PNearestPoint = Nil; Info: PPathInfo = Nil;
  LengthPos: PSingle = Nil): boolean;
function  FlattenPath(var Points: TPointArray; var Types: TPointTypeArray;
  Curvature: single; Info: PPathInfo = Nil): boolean;
function  ConnectorReroute(var Points: TPointArray;
  var PointTypes: TPointTypeArray; const Params: TRerouteParams): boolean;
function  PathLength(const Points: TPointArray; const Types: TPointTypeArray;
  Info: PPathInfo = Nil): single;
function  PathLengthPos(const Points: TPointArray; const Types: TPointTypeArray;
  PathPos: single; Nearest: PNearestPoint; Info: PPathInfo = Nil): boolean;

function FindNearestPathSegment(const Points: TPointArray;
  const Types: TPointTypeArray; const Point: TPoint;
  var FirstIndex, NextIndex: integer; Nearest: PNearestPoint = Nil;
  Info: PPathInfo = Nil; ForInsert: boolean = true): boolean;
function  InsertPathPoint(var Points: TPointArray; var Types: TPointTypeArray;
  FirstIndex, NextIndex: integer; const Point: TPoint;
  StickThreshold: integer = 0; Info: PPathInfo = Nil): integer;
function  InsertNearestPoint(var Points: TPointArray;
  var Types: TPointTypeArray; const Point: TPoint;
  StickThreshold: integer = 0; Info: PPathInfo = Nil): integer;

function  GetEditPathCaps(const Points: TPointArray;
  const Types: TPointTypeArray; const Selected: TSelectedArray): TPathEditFuncs;
function  EditPath(var Points: TPointArray; var Types: TPointTypeArray;
  const Selected: TSelectedArray; Func: TPathEditFunc;
  Params: PPathEditParams = Nil): boolean; overload;
function  EditPath(var Points: TPointArray; var Types: TPointTypeArray;
  const Indexes: array of integer; Func: TPathEditFunc;
  Params: PPathEditParams = Nil): boolean; overload;

procedure GetPathInfo(const Points: TPointArray; const Types: TPointTypeArray;
  var Info: TPathInfo);
function  GetFigureIndex(const Info: TPathInfo; PointIndex: integer): integer;
function  ChangePathCount(var Points: TPointArray; var Types: TPointTypeArray;
  Index, Delta: integer): boolean;

implementation

type
  TPathFunc = ( pfFlatten, pfOnLine, pfRange, pfPaint, pfLength );

  TListPoint = packed record
   Point: TPoint;
   PointType: TPointType;
  end;

  PListPoints = ^TListPoints;
  TListPoints = array[0..(MaxInt div SizeOf(TListPoint))-1] of TListPoint;

  TPointList = class
   Data: PListPoints;
   Count: integer;
   Capacity: integer;
   Delta: integer;
   constructor Create(ACapacity: integer = 32);
   destructor Destroy; override;
   procedure Grow;
   procedure Clear;
  end;

  PBezierParams = ^TBezierParams;
  TBezierParams = record
   case Boolean {OriginalBezier} of
    False: (
      x0, y0: single;
      x1, y1: single;
      x2, y2: single;
      x3, y3: single;
      p0pos, p3pos: single;
     );
    True: (
      OrigPoints: array[0..3] of TPoint;
     );
  end;

  PPathParams = ^TPathParams;
  TPathParams = record
   UseClosed: boolean;
   UseNotClosed: boolean;
   Complete: boolean;
   OriginalBezier: boolean;
   IsFigureClosed: boolean;
   p0, p1: TPoint; // points of the current line segment
   Curvature: single; // [0.0 .. 1.0]
   Info: PPathInfo;
   FigureCount: integer; // Total processed figures in ProcessPath
   case Func: TPathFunc of
    pfFlatten: (
      PointList: TPointList );
    pfOnLine: (
      CheckPoint: TPoint;
      StrokeWidth: integer;
      ScanLine: TList;
      Nearest: PNearestPoint;
      IsOnLine: boolean;
      CurLength: single;
      LengthPos: PSingle; );
    pfLength: (
      PathLength: single;
      PathPos: single;
      LengthNearest: PNearestPoint );
    pfRange: (
      Range: PRect );
    pfPaint: (
      PaintDC: HDC );
  end;

// TPointList /////////////////////////////////////////////////////////////////

constructor TPointList.Create(ACapacity: integer);
begin
 Capacity := ACapacity;
 Delta := ACapacity;
 if Capacity > 0 then GetMem(Data, Capacity * SizeOf(TListPoint));
end;

destructor TPointList.Destroy;
begin
 Clear;
end;

procedure TPointList.Clear;
begin
 FreeMem(Data);
 Data := Nil;
 Capacity := 0;
end;

procedure TPointList.Grow;
begin
 inc(Capacity, Delta);
 ReallocMem(Data, Capacity*SizeOf(TListPoint));
 inc(Delta, Delta);
end;

// Bezier curve and path routines /////////////////////////////////////////////

function PointOnLine(const p, p0, p1: TPoint; StrokeWidth: integer;
  ScanLinePoints: TList = Nil; Nearest: PNearestPoint = Nil): boolean;
var SqrDist, SqrLength, SqrDist0, SqrDist1, Coeff: single;
    px, py, p0x, p0y, p1x, p1y: single;
    R: TRect;
    InRange: boolean;
    Index: integer;
begin
 Result := StrokeWidth > 0;
 if Result then begin
  // Calculate normalized rect [p0, p1] inflated on StrokeWidth
  if p0.x < p1.x
   then begin R.Left  := p0.x - StrokeWidth; R.Right := p1.x + StrokeWidth end
   else begin R.Right := p0.x + StrokeWidth; R.Left  := p1.x - StrokeWidth end;
  if p0.y < p1.y
   then begin R.Top    := p0.y - StrokeWidth; R.Bottom := p1.y + StrokeWidth end
   else begin R.Bottom := p0.y + StrokeWidth; R.Top    := p1.y - StrokeWidth end;
  // Check that p in R
  Result := (p.x >= R.Left) and (p.x <= R.Right) and
            (p.y >= R.Top) and (p.y <= R.Bottom);
 end;
 if Result or Assigned(Nearest) then begin
  // Convert to float values and calculate length
  px := p.x;
  py := p.y;
  p0x := p0.x;
  p0y := p0.y;
  p1x := p1.x;
  p1y := p1.y;
  SqrLength := (p0x - p1x)*(p0x - p1x) + (p0y - p1y)*(p0y - p1y);
  SqrDist0 := (p0x - px)*(p0x - px) + (p0y - py)*(p0y - py);
  SqrDist1 := (p1x - px)*(p1x - px) + (p1y - py)*(p1y - py);
  // Check point-on-line
  if SqrLength < 1 then begin
   // Lenght too small
   SqrDist := SqrDist0; // There must be SqrDist0 = SqrDist1
   Result := Result and (Abs(p.x - p0.x) <= StrokeWidth)
                    and (Abs(p.y - p0.y) <= StrokeWidth);
  end else begin
   // Check distance
   Coeff := SqrLength - SqrDist0 - SqrDist1;
   SqrDist := Abs(4*SqrDist0*SqrDist1 - Coeff * Coeff) / (4*SqrLength);
   Result := Result and (SqrDist <= StrokeWidth*StrokeWidth);
  end;
  if Assigned(Nearest) then begin
   Nearest.IsNewMin := false;
   Index := Nearest.Index0;
   if (Nearest.MinIndex < 0) or (Nearest.MinSqrDist > SqrDist) then begin
    // Calculate nearest point
    if SqrLength = 0
     then Coeff := 0
     else Coeff := (SqrDist0 - SqrDist1 + SqrLength) / (2*SqrLength);
    R.Left := p0.x + Round((p1.x - p0.x) * Coeff);
    R.Top := p0.y + Round((p1.y - p0.y) * Coeff);
    if p0.x < p1.x
     then InRange := (R.Left >= p0.x) and (R.Left <= p1.x)
     else InRange := (R.Left >= p1.x) and (R.Left <= p0.x);
    if p0.y < p1.y
     then InRange := InRange and (R.Top >= p0.y) and (R.Top <= p1.y)
     else InRange := InRange and (R.Top >= p1.y) and (R.Top <= p0.y);
    if not InRange then begin
     if SqrDist0 < SqrDist1 then begin
      SqrDist := SqrDist0;
      R.Left := p0.x;
      R.Top := p0.y
     end else begin
      SqrDist := SqrDist1;
      R.Left := p1.x;
      R.Top := p1.y;
      Index := Nearest.Index1;
     end;
     InRange := (Nearest.MinIndex < 0) or (Nearest.MinSqrDist > SqrDist);
    end;
    if InRange then begin
     Nearest.Point.x := R.Left;
     Nearest.Point.y := R.Top;
     Nearest.MinIndex := Index;
     Nearest.MinSqrDist := SqrDist;
     Nearest.IsNewMin := true;
    end;
   end;
  end;
 end;
 if Assigned(ScanLinePoints) then begin
  if p1.y < p0.y
   then InRange := (p.y >= p1.y) and (p.y < p0.y)
   else InRange := (p.y >= p0.y) and (p.y < p1.y);
  if InRange then
   // Add point(s) to ScanList
   if p0.y = p1.y then begin
    // Add edge points
    ScanLinePoints.Insert(
      ListScanLess(pointer(p0.x), ScanLinePoints.List, ScanLinePoints.Count),
      pointer(p0.x)
    );
    ScanLinePoints.Insert(
      ListScanLess(pointer(p1.x), ScanLinePoints.List, ScanLinePoints.Count),
      pointer(p1.x)
    );
   end else begin
    // Add intersection point
    R.Left := p0.x + integer(int64(p1.x - p0.x)*(p.y - p0.y) div (p1.y - p0.y));
    ScanLinePoints.Insert(
      ListScanLess(pointer(R.Left), ScanLinePoints.List, ScanLinePoints.Count),
      pointer(R.Left)
    );
   end;
 end;
end;

procedure ProcessSegment(var Path: TPathParams; Bezier: PBezierParams);
var
 TempBezier: packed record
  xm, ym: single;
  case byte of
   1: (
     A, B, C, AB: single;
     CheckAB1, CheckAB2: single );
   2: (
     Next: TBezierParams );
   3: (
     px, py: single;
     Length, NewLength: single;
     Coeff: single;
      );
 end;
 TempList: TList;

begin
 if Assigned(Bezier) then with TempBezier, Bezier^ do
  if Path.OriginalBezier and (Path.Func = pfPaint) then begin
   // Draw original bezier curve (GDI)
   PolyBezierTo(Path.PaintDC, OrigPoints[1], 3);
   Path.p1 := OrigPoints[3];
   Path.p0 := OrigPoints[3];
   exit;
  end else begin
   // Draw recoursive bezier curve
   A := y3 - y0;
   B := x0 - x3;
   C := A*x0 + B*y0;
   // Ax + By - C = 0  is line (x0,y0) - (x3,y3)
   AB := A*A + B*B;
   CheckAB1 := (A*x1 + B*y1 - C) * Path.Curvature;
   CheckAB2 := (A*x2 + B*y2 - C) * Path.Curvature;
   // Check subdivide
   if (CheckAB1*CheckAB1 >= AB) or (CheckAB2*CheckAB2 >= AB) then begin
    xm := (x0 + 3*x1 + 3*x2 + x3) * 0.125;
    ym := (y0 + 3*y1 + 3*y2 + y3) * 0.125;
    // Check small length
    if (xm <> x0) or (ym <> y0) then begin
     // Subdivide
     Next.x0 := x0;
     Next.y0 := y0;
     Next.x1 := (x0 + x1) * 0.5;
     Next.y1 := (y0 + y1) * 0.5;
     Next.x2 := (x0 + 2*x1 + x2) * 0.25;
     Next.y2 := (y0 + 2*y1 + y2) * 0.25;
     Next.x3 := xm;
     Next.y3 := ym;
     if Path.Func = pfOnLine then begin
      Next.p0pos := p0pos;
      Next.p3pos := p0pos + (p3pos - p0pos) * 0.5;
     end;
     ProcessSegment(Path, @Next);
     Next.x0 := xm;
     Next.y0 := ym;
     Next.x1 := (x1 + 2*x2 + x3) * 0.25;
     Next.y1 := (y1 + 2*y2 + y3) * 0.25;
     Next.x2 := (x2 + x3) * 0.5;
     Next.y2 := (y2 + y3) * 0.5;
     Next.x3 := x3;
     Next.y3 := y3;
     if Path.Func = pfOnLine then begin
      Next.p0pos := Next.p3pos;
      Next.p3pos := p3pos;
     end;
     ProcessSegment(Path, @Next);
     exit;
    end;
   end;
   // distance from (x1,y1) to the line is less than 1
   // distance from (x2,y2) to the line is less than 1
   Path.p1.x := Round(x3);
   Path.p1.y := Round(y3);
  end;
 with Path do begin
  case Func of
   pfFlatten:
     with PointList do begin
      if Count = Capacity then Grow;
      Data[Count].Point := p1;
      Data[Count].PointType := ptNode;
      inc(Count);
     end;
   pfOnLine:
     if not IsOnLine or Assigned(Nearest) then begin
      if IsFigureClosed
       then TempList := ScanLine
       else TempList := Nil;
      IsOnLine :=
        PointOnLine(CheckPoint, p0, p1, StrokeWidth, TempList, Nearest) or
        IsOnLine;
      if Assigned(LengthPos) then with TempBezier do begin
       px := p0.x - p1.x;
       py := p0.y - p1.y;
       if p0.x = p1.x then
        Length := abs(py)
       else
       if p0.y = p1.y then
        Length := abs(px)
       else
        Length := sqrt(px*px + py*py);
       NewLength := CurLength + Length;
      end;
      if Assigned(Nearest) and Nearest.IsNewMin then with Nearest^ do
       if Assigned(Bezier) then begin
        // Correct index
        if Bezier.p3pos < 1 then MinIndex := Index0;
        // Calc bezier curve position (t)
        if p1.x <> p0.x then
         CurvePos := Bezier.p0pos + (Bezier.p3pos - Bezier.p0pos) *
           ( (Point.x - p0.x) / (p1.x - p0.x) )
        else
         CurvePos := Bezier.p0pos;
        if Assigned(LengthPos) then
         LengthPos^ := CurLength + TempBezier.Length * CurvePos;
       end else
       if Assigned(LengthPos) then begin
        if p1.x <> p0.x then
         LengthPos^ := CurLength +
           TempBezier.Length * (Point.x - p0.x) / (p1.x - p0.x)
        else
        if p1.y <> p0.y then
         LengthPos^ := CurLength +
           TempBezier.Length * (Point.y - p0.y) / (p1.y - p0.y)
        else
         LengthPos^ := CurLength;
       end;
      if Assigned(LengthPos) then CurLength := TempBezier.NewLength;
     end;
   pfLength:
     with TempBezier do begin
      // Increase Nearest.CurvePos (total path length) to current segment length
      px := p0.x - p1.x;
      py := p0.y - p1.y;
      if p0.x = p1.x then
       Length := abs(py)
      else
      if p0.y = p1.y then
       Length := abs(px)
      else
       Length := sqrt(px*px + py*py);
      NewLength := PathLength + Length;
      // Check length stop
      if Assigned(LengthNearest) and (LengthNearest.MinIndex < 0) and
         (PathPos >= 0) and (PathPos <= NewLength) and (Length > 0) then
       with LengthNearest^ do begin
        // Calc point on segment
        Coeff := (PathPos - PathLength) / Length;
        Point.X := p0.x - Round(px * Coeff);
        Point.Y := p0.y - Round(py * Coeff);
        if (PathPos = NewLength) or
           (Assigned(Bezier) and (Bezier.p3pos = 1)) then begin
         MinIndex := Index1;
         CurvePos := 0;
        end else begin
         MinIndex := Index0;
         if Assigned(Bezier) then begin
          // Calc bezier curve position (t)
          if p1.x <> p0.x then
           CurvePos := Bezier.p0pos + (Bezier.p3pos - Bezier.p0pos) *
             ( (Point.x - p0.x) / (p1.x - p0.x) )
           else
            CurvePos := Bezier.p0pos;
         end else
          CurvePos := Coeff;
        end;
       end
      else
       PathLength := NewLength;
     end;
   pfRange:
     with Range^ do begin
      if Left > p1.x then Left := p1.x else
      if Right < p1.x then Right := p1.x;
      if Top > p1.y then Top := p1.y else
      if Bottom < p1.y then Bottom := p1.y;
     end;
   pfPaint:
     LineTo(PaintDC, p1.x, p1.y);
  end;
  p0 := p1;
 end;
end;

function ProcessPath(const Points: TPointArray; const Types: TPointTypeArray;
  var Params: TPathParams): boolean;
var FigIndex, FigEndIndex, Index, Count: integer;
    Bezier: TBezierParams;
    UseBezier: PBezierParams;
    InternalInfo: boolean;
    PathNearest: PNearestPoint;
begin
 Result := true;
 Params.FigureCount := 0;
 Count := Length(Points);
 if Count <> Length(Types) then begin
  Result := false;
  exit;
 end;
 if (Count = 0) or (not Params.UseClosed and not Params.UseNotClosed) then
  // Empty path - do nothing
  exit;
 InternalInfo := not Assigned(Params.Info);
 with Params do
 try
  if InternalInfo then begin
   New(Info);
   GetPathInfo(Points, Types, Info^);
  end;
  if (Count <> Info.PointCount) then begin
   Result := false;
   exit;
  end;
  case Func of
   pfOnLine:  PathNearest := Params.Nearest;
   pfLength:  PathNearest := Params.LengthNearest;
   else       PathNearest := Nil;
  end;
  // Process all figures
  for FigIndex:=0 to Length(Info.Figures)-1 do
   with Info.Figures[FigIndex] do begin
    if (IsClosed and not UseClosed) or (not IsClosed and not UseNotClosed) then
    begin
     // Skip figure
     Complete := false;
     continue;
    end;
    p0 := Points[FirstNode];
    // BEGIN FIGURE
    inc(Params.FigureCount);
    case Func of
     pfFlatten:
       with PointList do begin
        if Count = Capacity then Grow;
        Data[Count].Point := p0;
        Data[Count].PointType := ptNode;
        inc(Count);
       end;
     pfRange:
       with Range^ do begin
        if Left > p0.x then Left := p0.x else
        if Right < p0.x then Right := p0.x;
        if Top > p0.y then Top := p0.y else
        if Bottom < p0.y then Bottom := p0.y;
       end;
     pfPaint:
       MoveToEx(PaintDC, p0.x, p0.y, Nil);
    end;
    IsFigureClosed := IsClosed;
    // PROCESS FIGURE POINTS
    Index := FirstNode;
    if IsClosed
     then FigEndIndex := LastNode
     else FigEndIndex := LastNode-1;
    while Index <= FigEndIndex do begin
     if Types[Index] = ptControl then begin
      // error
      Result := false;
      break;
     end;
     if Assigned(PathNearest) then PathNearest.Index0 := Index;
     if (Index < Count-2) and (Types[Index+1] = ptControl) then begin
      // Bezier curve segment
      UseBezier := @Bezier;
      with Bezier do
       if Params.OriginalBezier then begin
        OrigPoints[0] := Points[Index+0];
        OrigPoints[1] := Points[Index+1];
        OrigPoints[2] := Points[Index+2];
        if Types[Index] = ptEndNodeClose
         then OrigPoints[3] := Points[FirstNode]
         else OrigPoints[3] := Points[Index+3];
       end else begin
        // Init bezier parameters (convert to float values)
        p0pos := 0;
        p3pos := 1;
        x0 := Points[Index+0].x;
        y0 := Points[Index+0].y;
        x1 := Points[Index+1].x;
        y1 := Points[Index+1].y;
        x2 := Points[Index+2].x;
        y2 := Points[Index+2].y;
        if Types[Index] = ptEndNodeClose then begin
         // Closed curve - use first point of the figure
         x3 := Points[FirstNode].x;
         y3 := Points[FirstNode].y;
         if Assigned(PathNearest) then PathNearest.Index1 := FirstNode;
        end else begin
         // Use next node as end point of the bezier curve
         x3 := Points[Index+3].x;
         y3 := Points[Index+3].y;
         if Assigned(PathNearest) then PathNearest.Index1 := Index+3; 
        end;
       end;
      inc(Index, 3);
     end else begin
      // Line segment
      UseBezier := Nil;
      if Types[Index] = ptEndNodeClose then begin
       p1 := Points[FirstNode];
       if Assigned(PathNearest) then PathNearest.Index1 := FirstNode;
      end else begin
       p1 := Points[Index+1];
       if Assigned(PathNearest) then PathNearest.Index1 := Index+1;
      end;
      inc(Index);
     end;
     // Process path segment
     ProcessSegment(Params, UseBezier);
     case Func of
      pfOnLine:
        if IsOnLine and not Assigned(Nearest) then break;
      pfLength:
        if Assigned(PathNearest) and (PathNearest.MinIndex >= 0) then break;
     end;
    end;
    // Check error
    if not Result then break;
    // END FIGURE
    case Func of
     pfFlatten:
       with PointList do
        if IsClosed then begin
         dec(Count);
         Data[Count-1].PointType := ptEndNodeClose
        end else
         Data[Count-1].PointType := ptEndNode;
     pfOnLine:
       if IsOnLine and not Assigned(Nearest) then break;
     pfLength:
       if Assigned(PathNearest) and (PathNearest.MinIndex >= 0) then break;
     pfPaint:
       if IsClosed then CloseFigure(PaintDC);
    end;
   end;
 finally
  if InternalInfo and Assigned(Info) then begin
   Dispose(Info);
   Info := Nil;
  end;
 end;
end;

// Interface routines /////////////////////////////////////////////////////////

function CalcPath(const Points: TPointArray; const Types: TPointTypeArray;
  var Range: TRect; Info: PPathInfo = Nil): boolean;
var Path: TPathParams;
begin
 if Length(Points) = 0 then begin
  SetRectEmpty(Range);
  Result := false;
 end else begin
  with Range do begin
   Left := Points[0].x;
   Top := Points[0].y;
   Right := Points[0].x;
   Bottom := Points[0].y;
  end;
  Path.UseClosed := true;
  Path.UseNotClosed := true;
  Path.OriginalBezier := false;
  Path.Curvature := 1;
  Path.Info := Info;
  Path.Func := pfRange;
  Path.Range := @Range;
  Result := ProcessPath(Points, Types, Path);
 end;
end;

function CreatePath(DC: HDC; const Points: TPointArray;
  const Types: TPointTypeArray; UseClosed, UseNotClosed: boolean;
  var Complete: boolean; OriginalBezier: boolean = false;
  Info: PPathInfo = Nil): boolean;
var Path: TPathParams;
begin
 Path.UseClosed := UseClosed;
 Path.UseNotClosed := UseNotClosed;
 Path.OriginalBezier := OriginalBezier;
 Path.Curvature := 1;
 Path.Info := Info;
 Path.Func := pfPaint;
 Path.PaintDC := DC;
 Result := BeginPath(DC);
 Result := Result and ProcessPath(Points, Types, Path);
 Result := Result and (Path.FigureCount > 0);
 if Result 
  then EndPath(DC)
  else AbortPath(DC);
 Complete := Result and Path.Complete;
end;

function PointOnPath(const Points: TPointArray; const Types: TPointTypeArray;
  const Point: TPoint; Stroked, Filled: boolean; StrokeWidth: integer = 0;
  Nearest: PNearestPoint = Nil; Info: PPathInfo = Nil;
  LengthPos: PSingle = Nil): boolean;
var ScanList: TList;
    Index: integer;
    Path: TPathParams;
begin
 ScanList := Nil;
 try
  if Assigned(Nearest) then begin
   FillChar(Nearest^, SizeOf(Nearest^), 0);
   Nearest.MinIndex := -1;
  end;
  if Assigned(LengthPos) then LengthPos^ := 0.0;
  if Filled then begin
   ScanList := TList.Create;
   ScanList.Capacity := 64;
  end;
  if not Stroked then StrokeWidth := 0;
  Path.UseClosed := Stroked or Filled;
  Path.UseNotClosed := Stroked;
  Path.OriginalBezier := false;
  Path.Curvature := 1.0;
  Path.Info := Info;
  Path.Func := pfOnLine;
  Path.CheckPoint := Point;
  Path.StrokeWidth := StrokeWidth;
  Path.IsOnLine := false;
  Path.ScanLine := ScanList;
  Path.Nearest := Nearest;
  Path.CurLength := 0.0;
  Path.LengthPos := LengthPos;
  if ProcessPath(Points, Types, Path) then begin
   Result := Path.IsOnLine;
   if not Result and Filled then begin
    // Check ScanList
    Index := ListScanLess(pointer(Point.x), ScanList.List, ScanList.Count);
    Result := (Index < ScanList.Count) and
      ((Index and 1 <> 0) or (integer(ScanList[Index]) = Point.x));
   end;
  end else
   Result := false;
 finally
  ScanList.Free;
 end;
end;

function PathLength(const Points: TPointArray; const Types: TPointTypeArray;
  Info: PPathInfo = Nil): single;
var Path: TPathParams;
begin
 Path.UseClosed := true;
 Path.UseNotClosed := true;
 Path.OriginalBezier := false;
 Path.Curvature := 1;
 Path.Info := Info;
 Path.Func := pfLength;
 Path.LengthNearest := Nil;
 Path.PathLength := 0;
 Path.PathPos := -1.0;
 if ProcessPath(Points, Types, Path)
  then Result := Path.PathLength
  else Result := -1.0;
end;

function PathLengthPos(const Points: TPointArray; const Types: TPointTypeArray;
  PathPos: single; Nearest: PNearestPoint; Info: PPathInfo = Nil): boolean;
var Path: TPathParams;
begin
 if Assigned(Nearest) then begin
  FillChar(Nearest^, SizeOf(Nearest^), 0);
  Nearest.MinIndex := -1;
 end;
 Path.UseClosed := true;
 Path.UseNotClosed := true;
 Path.OriginalBezier := false;
 Path.Curvature := 1;
 Path.Info := Info;
 Path.Func := pfLength;
 Path.LengthNearest := Nearest;
 Path.PathLength := 0;
 Path.PathPos := PathPos;
 Result := ProcessPath(Points, Types, Path);
end;

function FlattenPath(var Points: TPointArray; var Types: TPointTypeArray;
  Curvature: single; Info: PPathInfo = Nil): boolean;
var Path: TPathParams;
    PointList: TPointList;
    i: integer;
begin
 PointList := Nil;
 try
  Path.UseClosed := true;
  Path.UseNotClosed := true;
  Path.OriginalBezier := false;
  Path.Curvature := Curvature;
  Path.Info := Info;
  Path.Func := pfFlatten;
  PointList := TPointList.Create;
  Path.PointList := PointList;
  Result := ProcessPath(Points, Types, Path);
  if Result then begin
   SetLength(Points, PointList.Count);
   SetLength(Types, PointList.Count);
   for i:=0 to PointList.Count-1 do begin
    Points[i] := PointList.Data[i].Point;
    Types[i] := PointList.Data[i].PointType;
   end;
  end;
 finally
  PointList.Free;
 end;
end;

function InsertPathPoint(var Points: TPointArray; var Types: TPointTypeArray;
  FirstIndex, NextIndex: integer; const Point: TPoint;
  StickThreshold: integer = 0; Info: PPathInfo = Nil): integer;
var Count: integer;
    InternalInfo: boolean;
    IsOutOfFigure, IsLastFirst, IsSegmentCurve: boolean;
    FigIndex, Index: integer;
    Path: TPathParams;
    Bezier: TBezierParams;
    Nearest: TNearestPoint;
    Ofs, Delta: TPoint;
    Coeff: single;

 function CalcBezier(a, b, c, t: single): single;
 var mt: single;
 begin
  mt := 1 - t;
  Result := mt*mt*a + 2*t*mt*b + t*t*c;
 end;

begin
 Result := -1;
 Count := Length(Points);
 // Check indexes values
 if (Count <> Length(Types)) or (FirstIndex = NextIndex) or
    (FirstIndex < 0) or (FirstIndex > Count-1) or
    (NextIndex < -1) or (NextIndex > Count) then exit;
 // Check points types
 if (Types[FirstIndex] = ptControl) or
  ( (NextIndex >= 0) and (NextIndex < Count) and
    (Types[NextIndex] = ptControl) ) then exit;
 InternalInfo := not Assigned(Info);
 try
  // Check path info
  if InternalInfo then begin
   New(Info);
   GetPathInfo(Points, Types, Info^);
  end;
  if Info.PointCount <> Count then exit;
  // Find figure
  FigIndex := GetFigureIndex(Info^, FirstIndex);
  if FigIndex < 0 then exit;
  // Validate indexes
  with Info.Figures[FigIndex] do begin
   IsOutOfFigure := (NextIndex < FirstNode) or (NextIndex > LastNode);
   IsLastFirst := (NextIndex = FirstNode) and (FirstIndex = LastNode);
   if IsOutOfFigure then begin
    // NextIndex is out of figure
    if IsClosed or
       ((FirstIndex <> FirstNode) and (NextIndex < FirstNode)) or
       ((FirstIndex <> LastNode) and (NextIndex > LastNode)) then exit;
   end else begin
    if IsLastFirst and not IsClosed then exit;
    // Check index order
    if (NextIndex < FirstIndex) <> IsLastFirst then begin
     // Exchange indexes
     Index := FirstIndex;
     FirstIndex := NextIndex;
     NextIndex := Index;
     // Redefine IsLastFirst
     IsLastFirst := (NextIndex = FirstNode) and (FirstIndex = LastNode);
    end;
   end;
   // Define curve flag
   IsSegmentCurve := (FirstIndex < LastPoint) and
     (Types[FirstIndex+1] = ptControl);
   // Check index distance
   if not IsLastFirst and (Abs(NextIndex-FirstIndex) > 1) then
    if ((FirstIndex = FirstNode) and IsOutOfFigure) or
       not IsSegmentCurve or (NextIndex <> FirstIndex+3) then exit;
   // Calculate index to insert
   if IsOutOfFigure then begin
    if NextIndex < FirstIndex
     then Index := FirstIndex
     else Index := NextIndex;
   end else
   if IsLastFirst then
    Index := LastPoint+1
   else
    Index := NextIndex;
  end;
  FillChar(Nearest, SizeOf(Nearest), 0);
  Nearest.MinIndex := -1;
  if IsSegmentCurve then begin
   // Insert bezier curve node
   FillChar(Path, SizeOf(Path), 0);
   Path.Curvature := 1;
   Path.Func := pfOnLine;
   Path.CheckPoint := Point;
   Path.StrokeWidth := 0;
   Path.IsOnLine := false;
   Path.ScanLine := Nil;
   Path.Nearest := @Nearest;
   with Bezier do begin
    x0 := Points[FirstIndex+0].x;
    y0 := Points[FirstIndex+0].y;
    x1 := Points[FirstIndex+1].x;
    y1 := Points[FirstIndex+1].y;
    x2 := Points[FirstIndex+2].x;
    y2 := Points[FirstIndex+2].y;
    x3 := Points[NextIndex].x;
    y3 := Points[NextIndex].y;
    p0pos := 0;
    p3pos := 1;
   end;
   Path.p0 := Points[FirstIndex+0];
   ProcessSegment(Path, @Bezier);
   ChangePathCount(Points, Types, Index, 3);
   if Index < FirstIndex then inc(FirstIndex, 3);
   with Bezier do begin
    if Nearest.MinSqrDist <= StickThreshold*StickThreshold then begin
     Points[Index] := Nearest.Point;
     Ofs.x := 0;
     Ofs.y := 0;
    end else begin
     Points[Index] := Point;
     Ofs.x := Point.x - Nearest.Point.x;
     Ofs.y := Point.y - Nearest.Point.y;
    end;
    // Control points of FirstIndex
{    Points[FirstIndex+1].x :=
      Round( (1-Nearest.CurvePos) * x0 + Nearest.CurvePos * x1 );
    Points[FirstIndex+1].y :=
      Round( (1-Nearest.CurvePos) * y0 + Nearest.CurvePos * y1 );
    Points[FirstIndex+2].x :=
      Round( CalcBezier(x0, x1, x2, Nearest.CurvePos) ) + Ofs.x;
    Points[FirstIndex+2].y :=
      Round( CalcBezier(y0, y1, y2, Nearest.CurvePos) ) + Ofs.y;
    // Control points of inserted point
    Points[Index+1].x :=
      Round( CalcBezier(x1, x2, x3, Nearest.CurvePos) ) + Ofs.x;
    Points[Index+1].y :=
      Round( CalcBezier(y1, y2, y3, Nearest.CurvePos) ) + Ofs.y;
    Points[Index+2].x :=
      Round( (1-Nearest.CurvePos) * x2 + Nearest.CurvePos * x3 );
    Points[Index+2].y :=
      Round( (1-Nearest.CurvePos) * y2 + Nearest.CurvePos * y3 ); }
    // Recalc
    Points[Index+2] := Points[FirstIndex+2];
     if (Nearest.CurvePos = 0) or (Nearest.CurvePos = 1)
     then Coeff := 0 {must be infinity}
     else Coeff := 1 / (3 * (1 - Nearest.CurvePos) * Nearest.CurvePos);
    Delta.x := Round(Ofs.x * Coeff);
    Delta.y := Round(Ofs.y * Coeff);
    if (Delta.x <> 0) or (Delta.y <> 0) then begin
     // Change control points
     inc(Points[FirstIndex+1].x, Delta.x);
     inc(Points[FirstIndex+1].y, Delta.y);
     inc(Points[Index+2].x, Delta.x);
     inc(Points[Index+2].y, Delta.y);
     // Change bezier params
     x1 := Points[FirstIndex+1].x;
     y1 := Points[FirstIndex+1].y;
     x2 := Points[Index+2].x;
     y2 := Points[Index+2].y;
    end;
    // Calc new position of existing control points
    Points[FirstIndex+1].x :=
      Round( (1-Nearest.CurvePos) * x0 + Nearest.CurvePos * x1 );
    Points[FirstIndex+1].y :=
      Round( (1-Nearest.CurvePos) * y0 + Nearest.CurvePos * y1 );
    Points[Index+2].x :=
      Round( (1-Nearest.CurvePos) * x2 + Nearest.CurvePos * x3 );
    Points[Index+2].y :=
      Round( (1-Nearest.CurvePos) * y2 + Nearest.CurvePos * y3 );
    // Calc control points for inserted node
    Points[FirstIndex+2].x := Round(CalcBezier(x0, x1, x2, Nearest.CurvePos));
    Points[FirstIndex+2].y := Round(CalcBezier(y0, y1, y2, Nearest.CurvePos));
    Points[Index+1].x := Round(CalcBezier(x1, x2, x3, Nearest.CurvePos));
    Points[Index+1].y := Round(CalcBezier(y1, y2, y3, Nearest.CurvePos));
    // Set types of inserted control points
    Types[Index+1] := ptControl;
    Types[Index+2] := ptControl;
   end;
  end else begin
   // Insert line node
   if not IsOutOfFigure then
    PointOnLine(Point, Points[FirstIndex], Points[NextIndex], 0, Nil, @Nearest);
   ChangePathCount(Points, Types, Index, 1);
   if not IsOutOfFigure and
     (Nearest.MinSqrDist <= StickThreshold*PixelScaleFactor)
    then Points[Index] := Nearest.Point
    else Points[Index] := Point;
   if Index < FirstIndex then inc(FirstIndex);
  end;
  // Set type of inserted node
  if IsOutOfFigure and (Index > FirstIndex) then begin
   Types[FirstIndex] := ptNode;
   Types[Index] := ptEndNode;
  end else
  if IsLastFirst then begin
   Types[FirstIndex] := ptNode;
   Types[Index] := ptEndNodeClose;
  end else
   Types[Index] := ptNode;
  Result := Index;
 finally
  if InternalInfo and Assigned(Info) then Dispose(Info);
 end;
end;

function FindNearestPathSegment(const Points: TPointArray;
  const Types: TPointTypeArray; const Point: TPoint;
  var FirstIndex, NextIndex: integer; Nearest: PNearestPoint = Nil;
  Info: PPathInfo = Nil; ForInsert: boolean = true): boolean;
var MemNearest: TNearestPoint;
    FigIndex: integer;
    InternalInfo: boolean;
begin
 Result := false;
 InternalInfo := not Assigned(Info);
 try
  // Check nearest
  if not Assigned(Nearest) then Nearest := @MemNearest;
  // Check path info
  if InternalInfo then begin
   New(Info);
   GetPathInfo(Points, Types, Info^);
  end;
  // Find nearest point
  FillChar(Nearest^, SizeOf(Nearest^), 0);
  Nearest.MinIndex := -1;
  PointOnPath(Points, Types, Point, True, False, 0, Nearest, Info);
  if Nearest.MinIndex < 0 then exit;
  // Find figure
  FigIndex := GetFigureIndex(Info^, Nearest.MinIndex);
  if FigIndex < 0 then exit;
  // Define closest path segment
  with Info.Figures[FigIndex] do begin
   FirstIndex := Nearest.MinIndex;
   NextIndex := -1;
   if IsClosed then begin
    // Figure is closed
    if FirstIndex = LastNode then
     NextIndex := FirstNode
    else
    if Types[FirstIndex+1] = ptControl
     then NextIndex := FirstIndex+3
     else NextIndex := FirstIndex+1;
   end else begin
    // Figure is non-closed
    if FirstIndex = LastNode then begin
     if FirstIndex < LastPoint
      then NextIndex := FirstIndex+3
      else NextIndex := FirstIndex+1;
    end else
    if (FirstIndex = FirstNode) and
       (Nearest.Point.x = Points[FirstNode].x) and
       (Nearest.Point.y = Points[FirstNode].y) then
     NextIndex := FirstIndex-1
    else
    if Types[FirstIndex+1] = ptControl
     then NextIndex := FirstIndex+3
     else NextIndex := FirstIndex+1;
   end;
   Result := ForInsert or
     ((NextIndex >= FirstNode) and (NextIndex <= LastNode));
  end;
 finally
  if InternalInfo and Assigned(Info) then Dispose(Info);
 end;
end;

function InsertNearestPoint(var Points: TPointArray;
  var Types: TPointTypeArray; const Point: TPoint;
  StickThreshold: integer = 0; Info: PPathInfo = Nil): integer;
var FirstIndex, NextIndex: integer;
    InternalInfo: boolean;
begin
 Result := -1;
 InternalInfo := not Assigned(Info);
 try
  // Check path info
  if InternalInfo then begin
   New(Info);
   GetPathInfo(Points, Types, Info^);
  end;
  // Find path segment
  if not FindNearestPathSegment(Points, Types, Point,
    FirstIndex, NextIndex, Nil, Info) then exit;
  // Insert new point
  Result := InsertPathPoint(Points, Types, FirstIndex, NextIndex, Point,
    StickThreshold, Info);
 finally
  if InternalInfo and Assigned(Info) then Dispose(Info);
 end;
end;

function GetEditPathCaps(const Points: TPointArray;
  const Types: TPointTypeArray; const Selected: TSelectedArray): TPathEditFuncs;
var i, Count, SelCount: integer;
    First, Second: record
     IsFirst: boolean;
     PointType: TPointType;
     Figure: integer;
    end;
    Figures: integer;
    IsPrevEnd: boolean;
begin
 Result := [];
 Count := Length(Selected);
 if (Count <> Length(Points)) or (Count <> Length(Types)) then exit;
 SelCount := 0;
 Figures := 0;
 IsPrevEnd := true;
 for i:=0 to Count-1 do begin
  if Selected[i] then begin
   // Check selected point
   inc(SelCount);
   if Types[i] = ptControl then begin
    Result := [pfOffset];
    exit;
   end;
   case SelCount of
    1: with First do begin
        IsFirst := IsPrevEnd;
        PointType := Types[i];
        Figure := Figures;
       end;
    2: with Second do begin
        IsFirst := IsPrevEnd;
        PointType := Types[i];
        Figure := Figures;
       end;
   end;
   if (i < Length(Points)-1) and (Types[i+1] = ptControl)
    then Include(Result, pfToLine)
    else Include(Result, pfToCurve);
   if Types[i] = ptNode then Include(Result, pfBreak);
  end;
  // Check end of figure in path
  case Types[i] of
   ptNode:
     IsPrevEnd := false;
   ptEndNode,
   ptEndNodeClose:
     begin
      IsPrevEnd := true;
      inc(Figures);
     end;
  end;
 end;
 // Define pfJoin and pfClose capabilities
 if (SelCount = 2) and
     ( (First.IsFirst and (Second.PointType = ptEndNode)) or
       ((First.PointType = ptEndNode) or Second.IsFirst) ) then begin
  Include(Result, pfJoin);
  Include(Result, pfClose);
 end;
 if SelCount > 0 then begin
  Include(Result, pfOffset);
  //Include(Result, pfDelete);
 end;
end;

function EditPath(var Points: TPointArray; var Types: TPointTypeArray;
  const Selected: TSelectedArray; Func: TPathEditFunc;
  Params: PPathEditParams = Nil): boolean;
type
 TFigure = record
  FirstNode: integer;
  EndNode: integer;
  LastPoint: integer;
 end;
var
 Index, BreakIndex, NodeIndex, LastIndex, FirstIndex: integer;
 First, Second: TFigure;
 Temp: array of byte;
 Count, DeltaCount, MoveCount, Size: integer;
 Error, SameFigure, NeedMove: boolean;
 p0, p1, CtrlPointA, CtrlPointB: TPoint;

 function CalcFigure(Index: integer; var Figure: TFigure): boolean;
 begin
  with Figure do begin
   if Types[Index] = ptEndNode then begin
    // It is last point in figure. Find first point
    EndNode := Index;
    FirstNode := Index;
    repeat
     dec(Index);
     if Index < 0 then break;
     if Types[Index] = ptNode then FirstNode := Index;
    until Types[Index] in [ptEndNode, ptEndNodeClose];
    Result := true;
   end else begin
    // It is first point in figure. Find last point
    FirstNode := Index;
    EndNode := Index;
    repeat
     inc(EndNode);
    until (EndNode = Count) or (Types[EndNode] in [ptEndNode, ptEndNodeClose]);
    Result := EndNode < Count; // check error in point types
   end;
   if not Result then exit;
   LastPoint := EndNode +1;
   while (LastPoint < Count) and (Types[LastPoint] = ptControl) do
    inc(LastPoint);
   dec(LastPoint);
  end;
 end;

 procedure ReverseFigureDirection(var Figure: TFigure);
 var i: integer;
     TempPoint: TPoint;
     TempType: TPointType;
     Count: integer;
 begin
  with Figure do begin
   Count := LastPoint - FirstNode +1;
   if Count <= 1 then exit;
   for i:=0 to (Count div 2)-1 do begin
    TempPoint := Points[FirstNode+i];
    Points[FirstNode+i] := Points[LastPoint-i];
    Points[LastPoint-i] := TempPoint;
    TempType := Types[FirstNode+i];
    Types[FirstNode+i] := Types[LastPoint-i];
    Types[LastPoint-i] := TempType;
   end;
   Types[EndNode] := Types[FirstNode];
   Types[FirstNode] := ptNode;
   Result := true; // Points changed
  end;
 end;

 procedure ChangeCount(Index, Delta: integer);
 begin
  if Delta > 0 then begin
   SetLength(Points, Count + Delta);
   SetLength(Types, Count + Delta);
  end else
   dec(Index, Delta);
  Move(Points[Index], Points[Index+Delta], (Count - Index) * SizeOf(Points[0]));
  Move(Types[Index], Types[Index+Delta], (Count - Index) * SizeOf(Types[0]));
  if Delta < 0 then begin
   SetLength(Points, Count + Delta);
   SetLength(Types, Count + Delta);
  end;
  inc(Count, Delta);
  Result := true; // Points changed
 end;

begin
 Temp := Nil;
 Result := Func in GetEditPathCaps(Points, Types, Selected);
 if not Result then exit;
 Result := false;
 Count := Length(Selected);
 if (Count <> Length(Points)) or (Count <> Length(Types)) then exit;
 case Func of
  pfOffset:
    if Assigned(Params) then with Params^ do
     if not Params.MoveControls then begin
      for Index:=0 to Count-1 do
       if Selected[Index] then begin
        inc(Points[Index].x, Offset.x);
        inc(Points[Index].y, Offset.y);
        Result := true; // Points changed
       end;
     end else begin
      BreakIndex := 0;
      FirstIndex := -1;
      NodeIndex := -1;
      for Index:=0 to Count-1 do begin
       if Index = BreakIndex then FirstIndex := Index;
       if Types[Index] <> ptControl then begin
        NodeIndex := Index;
        if Types[Index] in [ptEndNode, ptEndNodeClose] then
         if (Index < Count-1) and (Types[Index+1] = ptControl)
           then BreakIndex := Index + 3
           else BreakIndex := Index + 1;
       end;
       NeedMove := Selected[Index];
       if not NeedMove and (Index > NodeIndex) then
        // Check control point
        if Index - NodeIndex = 1 then begin
         // First control point - NodeIndex is owner
         NeedMove := Selected[NodeIndex];
        end else begin
         // Second control point - owner is next (or first in figure) node
         if Types[NodeIndex] = ptEndNodeClose then
          // Last node and closed figure - check first node
          NeedMove := Selected[FirstIndex]
         else
         if Types[NodeIndex] <> ptEndNode then
          // Check next node in figure
          NeedMove := Selected[NodeIndex+3];
        end;
       if NeedMove then begin
        inc(Points[Index].x, Offset.x);
        inc(Points[Index].y, Offset.y);
        Result := true; // Points changed
       end;
      end;
     end;
{  pfDelete:
    begin
     DeltaCount := 0;
     Index := 0;
     BreakIndex := 0;
     FirstIndex := -1;
     NodeIndex := -1;
     while Index < Count do begin
      if Index = BreakIndex then FirstIndex := Index;
      if Types[Index] <> ptControl then begin
       NodeIndex := Index;
       if Types[Index] in [ptEndNode, ptEndNodeClose] then
        if (Index < Count-1) and (Types[Index+1] = ptControl)
          then BreakIndex := Index + 3
          else BreakIndex := Index + 1;
       if Selected[Index+DeltaCount] then begin

       end
      end else
       // Skip control points
       inc(Index);
     end;
    end;  }
  pfJoin,
  pfClose:
    begin
     FirstIndex := -1;
     LastIndex := -1;
     SameFigure := true;
     for Index := 0 to Count-1 do begin
      if Selected[Index] then
       if FirstIndex < 0 then
        FirstIndex := Index
       else
       if LastIndex < 0 then begin
        LastIndex := Index;
        break;
       end;
      if (FirstIndex >= 0) and
         (Types[Index] in [ptEndNode, ptEndNodeClose]) then
       SameFigure := false;
     end;
     if (FirstIndex < 0) or (LastIndex < 0) then exit; // can't complete
     if SameFigure then begin
      case Func of
       pfJoin:
         // Join edge points of the figure
         if Types[LastIndex] = ptEndNode then begin
          // Find previous node
          Index := LastIndex -1;
          while (Index >= FirstIndex) and (Types[Index] = ptControl) do
           dec(Index);
          if Index < FirstIndex then exit; // error
          // Close figure and delete last point (join to first)
          Types[Index] := ptEndNodeClose;
          // Calculate middle point
          with Points[FirstIndex] do begin
           p0.x := x + abs(x - Points[LastIndex].x) div 2;
           p0.y := y + abs(y - Points[LastIndex].y) div 2;
          end;
          Points[FirstIndex] := p0;
          ChangeCount(LastIndex, -1);
         end;
       pfClose:
         // Close figure
         if Types[LastIndex] = ptEndNode then begin
          Types[LastIndex] := ptEndNodeClose;
          Result := true; // Points changed
         end;
      end;
      exit;
     end;
     // Define first and end nodes of selected figures
     if not CalcFigure(FirstIndex, First) then exit;
     if not CalcFigure(LastIndex, Second) then exit;
     // Calculate point count between first and second figures
     MoveCount := Second.FirstNode - First.LastPoint -1;
     if MoveCount > 0 then begin
      // Move Second figure after First figure
      DeltaCount := Second.LastPoint - Second.FirstNode +1;
      // Move points
      Size := DeltaCount * SizeOf(Points[0]);
      SetLength(Temp, Size);
      Move(Points[Second.FirstNode], Temp[0], Size);
      Move(Points[First.LastPoint+1], Points[First.LastPoint+1 +DeltaCount],
        MoveCount * SizeOf(Points[0]));
      Move(Temp[0], Points[First.LastPoint+1], Size);
      // Move types
      Size := DeltaCount * SizeOf(Types[0]);
      if Size > Length(Temp) then SetLength(Temp, Size);
      Move(Types[Second.FirstNode], Temp[0], Size);
      Move(Types[First.LastPoint+1], Types[First.LastPoint+1 +DeltaCount],
        MoveCount * SizeOf(Types[0]));
      Move(Temp[0], Types[First.LastPoint+1], Size);
      with Second do begin
       dec(FirstNode, MoveCount);
       dec(EndNode, MoveCount);
       dec(LastPoint, MoveCount);
       dec(LastIndex, MoveCount);
      end;
      Result := true; // Points changed
     end;
     // Check direction of First and Second figures
     if FirstIndex <> First.EndNode then ReverseFigureDirection(First);
     if LastIndex <> Second.FirstNode then ReverseFigureDirection(Second);
     // Complete function
     case Func of
      pfJoin:
        begin
         // Calculate middle point
         with Points[First.EndNode] do begin
          p0.x := x + abs(x - Points[Second.FirstNode].x) div 2;
          p0.y := y + abs(y - Points[Second.FirstNode].y) div 2;
         end;
         // Delete First.EndNode point
         if (First.EndNode < Count-1) and (Types[First.EndNode+1] = ptControl)
          then ChangeCount(First.EndNode, -3)
          else ChangeCount(First.EndNode, -1);
         // Set calculated middle point as result of join
         Points[First.EndNode] := p0;
        end;
      pfClose:
        // Replace ptEndNode type to ptNode (link figures)
        begin
         Types[First.EndNode] := ptNode;
         Result := true; // Points changed
        end;
     end;
    end;
  pfBreak:
    begin
     Index := 0;
     DeltaCount := 0;
     FirstIndex := 0;
     while Index < Count do begin
      BreakIndex := -1;
      // Define last point in figure
      LastIndex := FirstIndex;
      while (LastIndex < Count) and not
            (Types[LastIndex] in [ptEndNode, ptEndNodeClose]) do
       inc(LastIndex);
      if LastIndex = Count then break; // error in point types
      Index := FirstIndex;
      while Index <= LastIndex do begin
       if Selected[Index+DeltaCount] then
        if Types[Index] <> ptControl then begin
         // Break selected point
         ChangeCount(Index, 1);
         Types[Index] := ptEndNode;
         inc(LastIndex);
         inc(Index);
         dec(DeltaCount);
         BreakIndex := Index;
        end;
       inc(Index);
      end;
      // Check figure breaking
      if (BreakIndex >= 0) and (Types[LastIndex] = ptEndNodeClose) then begin
       Types[LastIndex] := ptNode;
       // Define last point of the figure
       if (LastIndex < Count-1) and (Types[LastIndex+1] = ptControl) then
        inc(LastIndex, 2);
       // Move points after last breaked before first point of the figure
       MoveCount := LastIndex - BreakIndex +1;
       // Move points
       Size := MoveCount * SizeOf(Points[0]);
       SetLength(Temp, Size);
       Move(Points[BreakIndex], Temp[0], Size);
       Move(Points[FirstIndex], Points[FirstIndex+MoveCount],
         (BreakIndex - FirstIndex) * SizeOf(Points[0]));
       Move(Temp[0], Points[FirstIndex], Size);
       // Move types
       Size := MoveCount * SizeOf(Types[0]);
       if Size > Length(Temp) then SetLength(Temp, Size);
       Move(Types[BreakIndex], Temp[0], Size);
       Move(Types[FirstIndex], Types[FirstIndex+MoveCount],
         (BreakIndex - FirstIndex) * SizeOf(Types[0]));
       Move(Temp[0], Types[FirstIndex], Size);
      end;
      FirstIndex := Index;
     end;
    end;
  pfToCurve:
    begin
     Index := 0;
     DeltaCount := 0;
     FirstIndex := 0;
     while Index < Count do begin
      // Define last point in figure
      LastIndex := FirstIndex;
      while (LastIndex < Count) and not
            (Types[LastIndex] in [ptEndNode, ptEndNodeClose]) do
       inc(LastIndex);
      if LastIndex = Count then break; // error in point types
      Index := FirstIndex;
      while Index <= LastIndex do begin
       if Selected[Index+DeltaCount] and
          ((Index >= Count-1) or (Types[Index+1] <> ptControl)) then begin
        // Define line points
        Error := false;
        p0 := Points[Index];
        if Types[Index] = ptEndNodeClose then
         p1 := Points[FirstIndex]
        else
        if Index < LastIndex then
         p1 := Points[Index+1]
        else
         // Can't convert point
         Error := true;
        if not Error then begin
         // Calculate control points
         CtrlPointA.x := p0.x + (p1.x - p0.x) div 3;
         CtrlPointA.y := p0.y + (p1.y - p0.y) div 3;
         CtrlPointB.x := p1.x - (p1.x - p0.x) div 3;
         CtrlPointB.y := p1.y - (p1.y - p0.y) div 3;
         // Convert to curve point
         ChangeCount(Index+1, 2);
         Points[Index+1] := CtrlPointA;
         Types[Index+1] := ptControl;
         Points[Index+2] := CtrlPointB;
         Types[Index+2] := ptControl;
         inc(Index, 2);
         inc(LastIndex, 2);
         dec(DeltaCount, 2);
        end;
       end;
       inc(Index);
      end;
      FirstIndex := Index;
     end;
    end;
  pfToLine:
    begin
     Index := 0;
     DeltaCount := 0;
     while Index < Count do begin
      if Selected[Index+DeltaCount] and
        (Index < Count-1) and (Types[Index+1] = ptControl) then begin
       // Convert curve point to line point
       ChangeCount(Index+1, -2);
       inc(DeltaCount, 2);
      end;
      inc(Index);
     end;
    end;
 end;
end;

function  EditPath(var Points: TPointArray; var Types: TPointTypeArray;
  const Indexes: array of integer; Func: TPathEditFunc;
  Params: PPathEditParams = Nil): boolean; overload;
var Selected: TSelectedArray;
    i: integer;
begin
 SetLength(Selected, Length(Points));
 for i:=0 to Length(Selected)-1 do Selected[i] := false;
 for i:=0 to Length(Indexes)-1 do
  if Indexes[i] < Length(Selected) then Selected[Indexes[i]] := true;
 Result := EditPath(Points, Types, Selected, Func, Params);
end;

function  ConnectorReroute(var Points: TPointArray;
  var PointTypes: TPointTypeArray; const Params: TRerouteParams): boolean;
type
  TSide = ( sdNorth, sdEast, sdSouth, sdWest );

  TSides = set of TSide;

var
  Count: integer;
  RangeGapA, RangeGapB: TRect;
  RangeGapExist: boolean;

 procedure SetCount(ACount: integer);
 var i: integer;
 begin
  SetLength(Points, ACount);
  SetLength(PointTypes, ACount);
  for i:=0 to ACount-2 do PointTypes[i] := ptNode;
  PointTypes[ACount-1] := ptEndNode;
  Count := ACount;
 end;

 function CalcSides(const LinkPoint: TPoint; const Range: TRect): TSides;
 var OutsideX, OutsideY: boolean;
     Side: TSide;
     Dist: array[TSide] of integer;
     MinDist: integer;
 begin
  Result := [];
  with LinkPoint, Range do begin
   // Check link point outside
   OutsideX := (x < Left) or (x > Right);
   OutsideY := (y < Top) or (y > Bottom);
   if OutsideX or OutsideY then begin
    // LinkPoint outside Range
    if OutsideX then
     if x < Left
      then Include(Result, sdWest)  // Outside left
      else Include(Result, sdEast); // Outside right
    if OutsideY then
     if y < Top
      then Include(Result, sdNorth)  // Outside top
      else Include(Result, sdSouth); // Outside bottom
   end else begin
    // LinkPoint inside Range
    // Calc distances to edges
    Dist[sdNorth] := y - Top;
    Dist[sdEast]  := Right - x;
    Dist[sdSouth] := Bottom - y;
    Dist[sdWest]  := x - Left;
    // Find min distance
    MinDist := 0;
    for Side:=Low(Side) to High(Side) do
     if Side=Low(Side) then
      MinDist := Dist[Side]
     else
     if Dist[Side] < MinDist then
      MinDist := Dist[Side];
    // Define active sides
    for Side:=Low(Side) to High(Side) do
     if Dist[Side] = MinDist then Include(Result, Side);
   end;
  end;
 end;

 function TryLinkOrtogonal(PtCount: integer; SideA, SideB: TSide): boolean;
 var Codirectional: boolean;
     IsHorizA, IsHorizB: boolean;
     Temp: TPoint;
     TempR: TRect;
     Exist: boolean;
     Size1, Size2{, Max1, Max2}: integer;
     FlagA, FlagB: boolean;

  function PointsOnGapInRange: boolean;
  var Temp: TRect;
  begin
   Result := true;
   // Test LinkA distribution
   Temp.TopLeft := Params.LinkPointA;
   Temp.BottomRight := Params.LinkPointA;
   inc(Temp.Right);
   inc(Temp.Bottom);
   case SideA of
    sdNorth : Temp.Top    := RangeGapA.Top;
    sdEast  : Temp.Right  := RangeGapA.Right;
    sdSouth : Temp.Bottom := RangeGapA.Bottom;
    sdWest  : Temp.Left   := RangeGapA.Left;
   end;
   // Test temp intersects with RangeGapB
   if IntersectRect(Temp, Temp, RangeGapB) then exit;
   // Test LinkB distribution
   Temp.TopLeft := Params.LinkPointB;
   Temp.BottomRight := Params.LinkPointB;
   inc(Temp.Right);
   inc(Temp.Bottom);
   case SideB of
    sdNorth : Temp.Top    := RangeGapB.Top;
    sdEast  : Temp.Right  := RangeGapB.Right;
    sdSouth : Temp.Bottom := RangeGapB.Bottom;
    sdWest  : Temp.Left   := RangeGapB.Left;
   end;
   // Test temp intersects with RangeGapA
   if IntersectRect(Temp, Temp, RangeGapA) then exit;
   Result := false;
  end;

 begin
  Result := false;
  // Define side type (horizontal or vertical)
  IsHorizA := SideA in [sdEast, sdWest];
  IsHorizB := SideB in [sdEast, sdWest];
  // Define codirectional sides (both horizontal or both vertical)
  Codirectional := (IsHorizA and IsHorizB) or (not IsHorizA and not IsHorizB);
  // Test incompatible imtermediate point count
  if ((PtCount = 1) or (PtCount = 3)) = Codirectional then exit;
  with Params do
  case PtCount of
   0: begin
       // Straight line (no intermediate points)
       // Check link points lie on horiz or vert line
       if IsHorizA then begin
        if LinkPointA.y <> LinkPointB.y then exit;
       end else
        if LinkPointA.x <> LinkPointB.x then exit;
       // Check distribution
       if SideA = SideB then exit;
       if IsHorizA then begin
        // Test horizontal distribution
        if (SideA = sdEast) = (LinkPointA.x > LinkPointB.x) then exit;
       end else
        // Test vertical distribution
        if (SideA = sdNorth) = (LinkPointA.y < LinkPointB.y) then exit;
       // All conditions completed. Create link
       SetCount(2);
      end;
   1: begin
       // Corner link (one intermediate point)
       // Check distribution
       if IsHorizA then begin
        // Test horizontal distribution
        case SideA of
         sdEast: if RangeGapA.Right > LinkPointB.x then exit;
         sdWest: if RangeGapA.Left  < LinkPointB.x then exit;
        end;
        case SideB of
         sdNorth: if LinkPointA.y > RangeGapB.Top then exit;
         sdSouth: if LinkPointA.y < RangeGapB.Bottom then exit;
        end;
       end else begin
        // Test vertical distribution
        case SideA of
         sdNorth: if RangeGapA.Top    < LinkPointB.y then exit;
         sdSouth: if RangeGapA.Bottom > LinkPointB.y then exit;
        end;
        case SideB of
         sdEast: if LinkPointA.x < RangeGapB.Right then exit;
         sdWest: if LinkPointA.x > RangeGapB.Left then exit;
        end;
       end;
       // All conditions completed. Create link
       SetCount(3);
       if IsHorizA then begin
        Points[1].x := LinkPointB.x;
        Points[1].y := LinkPointA.y;
       end else begin
        Points[1].x := LinkPointA.x;
        Points[1].y := LinkPointB.y;
       end;
      end;
   2: begin
       // Step link (two intermediate points)
       // Check distribution
       if IsHorizA then begin
        // Test horizontal distribution
        if SideA <> SideB then begin
         // Check horizontal gap
         case SideA of
          sdEast: begin
                   if RangeGapA.Right > RangeGapB.Left then exit;
                   Temp.x := (RangeGapA.Right + RangeGapB.Left) div 2;
                  end;
          sdWest: begin
                   if RangeGapA.Left < RangeGapB.Right then exit;
                   Temp.x := (RangeGapB.Right + RangeGapA.Left) div 2;
                  end;
         end;
        end else begin
         // Check y position
         if SideA = sdEast then begin
          // Check LinkA
          if (LinkPointA.x < RangeGapB.Right) and
             (LinkPointA.y > RangeGapB.Top) and
             (LinkPointA.y < RangeGapB.Bottom) then exit;
          // Check LinkB
          if (LinkPointB.x < RangeGapA.Right) and
             (LinkPointB.y > RangeGapA.Top) and
             (LinkPointB.y < RangeGapA.Bottom) then exit;
         end else begin
          // Check LinkA
          if (LinkPointA.x > RangeGapB.Left) and
             (LinkPointA.y > RangeGapB.Top) and
             (LinkPointA.y < RangeGapB.Bottom) then exit;
          // Check LinkB
          if (LinkPointB.x > RangeGapA.Left) and
             (LinkPointB.y > RangeGapA.Top) and
             (LinkPointB.y < RangeGapA.Bottom) then exit;
         end;
         // Calc edge
         if SideA = sdEast then begin
          if RangeGapA.Right > RangeGapB.Right
           then Temp.x := RangeGapA.Right
           else Temp.x := RangeGapB.Right;
         end else
          if RangeGapA.Left < RangeGapB.Left
           then Temp.x := RangeGapA.Left
           else Temp.x := RangeGapB.Left;
        end;
        // All OK. Create link
        SetCount(4);
        Points[1].x := Temp.x;
        Points[1].y := LinkPointA.y;
        Points[2].x := Temp.x;
        Points[2].y := LinkPointB.y;
       end else begin
        // Test vertical distribution
        if SideA <> SideB then begin
         // Check vertical gap
         case SideA of
          sdNorth: begin
                    if RangeGapA.Top < RangeGapB.Bottom then exit;
                    Temp.y := (RangeGapA.Top + RangeGapB.Bottom) div 2;
                   end;
          sdSouth: begin
                    if RangeGapA.Bottom > RangeGapB.Top then exit;
                    Temp.y := (RangeGapA.Bottom + RangeGapB.Top) div 2;
                   end;
         end;
        end else begin
         // Check x position
         if SideA = sdNorth then begin
          // Check LinkA
          if (LinkPointA.y > RangeGapB.Top) and
             (LinkPointA.x > RangeGapB.Left) and
             (LinkPointA.x < RangeGapB.Right) then exit;
          // Check LinkB
          if (LinkPointB.y > RangeGapA.Top) and
             (LinkPointB.x > RangeGapA.Left) and
             (LinkPointB.x < RangeGapA.Right) then exit;
         end else begin
          // Check LinkA
          if (LinkPointA.y < RangeGapB.Bottom) and
             (LinkPointA.x > RangeGapB.Left) and
             (LinkPointA.x < RangeGapB.Right) then exit;
          // Check LinkB
          if (LinkPointB.y < RangeGapA.Bottom) and
             (LinkPointB.x > RangeGapA.Left) and
             (LinkPointB.x < RangeGapA.Right) then exit;
         end;
         // Calc edge
         if SideA = sdNorth then begin
          if RangeGapA.Top < RangeGapB.Top
           then Temp.y := RangeGapA.Top
           else Temp.y := RangeGapB.Top;
         end else
          if RangeGapA.Bottom > RangeGapB.Bottom
           then Temp.y := RangeGapA.Bottom
           else Temp.y := RangeGapB.Bottom;
        end;
        // All OK. Create link
        SetCount(4);
        Points[1].x := LinkPointA.x;
        Points[1].y := Temp.y;
        Points[2].x := LinkPointB.x;
        Points[2].y := Temp.y;
       end;
      end;
   3: begin
       // Zigzag link (three intermediate points)
       // Check distribution
       if PointsOnGapInRange then exit;
       // Linking possible
       SetCount(5);
       if IsHorizA then begin
        // LinkA is West or East and LinkB is North or South
        Points[1].y := LinkPointA.y;
        // Test center (horizontal gap)
        if SideA = sdEast then begin
         Exist := RangeGapB.Left >= RangeGapA.Right;
         Temp.x := (RangeGapB.Left + RangeGapA.Right) div 2;
        end else begin
         Exist := RangeGapB.Right <= RangeGapA.Left;
         Temp.x := (RangeGapB.Right + RangeGapA.Left) div 2;
        end;
        if Exist then begin
         // Exist horizontal gap. Use it.
         Points[1].x := Temp.x;
         Points[2].x := Temp.x;
         if SideB = sdNorth
          then Points[2].y := RangeGapB.Top
          else Points[2].y := RangeGapB.Bottom;
         Points[3].x := LinkPointB.x;
         Points[3].y := Points[2].y;
        end else begin
         // Find vertical gap
         if SideB = sdNorth then begin
          // Check up gap
          Exist := RangeGapA.Bottom <= RangeGapB.Top;
          Temp.y := (RangeGapA.Bottom + RangeGapB.Top) div 2;
         end else begin
          // Check down gap
          Exist := RangeGapA.Top >= RangeGapB.Bottom;
          Temp.y := (RangeGapA.Top + RangeGapB.Bottom) div 2;
         end;
         if Exist then begin
          // Exist vertical gap. Use it.
          if SideA = sdEast
           then Points[1].x := RangeGapA.Right
           else Points[1].x := RangeGapA.Left;
          Points[2].x := Points[1].x;
          Points[2].y := Temp.y;
          Points[3].x := LinkPointB.x;
          Points[3].y := Temp.y;
         end else begin
          // No gaps. Make path around
          if SideA = sdEast then begin
           if RangeGapA.Right > RangeGapB.Right
            then Points[1].x := RangeGapA.Right
            else Points[1].x := RangeGapB.Right;
          end else
           if RangeGapA.Left < RangeGapB.Left
            then Points[1].x := RangeGapA.Left
            else Points[1].x := RangeGapB.Left;
          Points[2].x := Points[1].x;
          if SideB = sdNorth then begin
           if RangeGapA.Top < RangeGapB.Top
            then Points[2].y := RangeGapA.Top
            else Points[2].y := RangeGapB.Top;
          end else
           if RangeGapA.Bottom > RangeGapB.Bottom
            then Points[2].y := RangeGapA.Bottom
            else Points[2].y := RangeGapB.Bottom;
          Points[3].x := LinkPointB.x;
          Points[3].y := Points[2].y;
         end;
        end;
       end else begin
        // LinkA is North or South and LinkB is West or East
        Points[1].x := LinkPointA.x;
        // Test center (vertical gap)
        if SideA = sdNorth then begin
         Exist := RangeGapB.Bottom <= RangeGapA.Top;
         Temp.y := (RangeGapB.Bottom + RangeGapA.Top) div 2;
        end else begin
         Exist := RangeGapB.Top >= RangeGapA.Bottom;
         Temp.y := (RangeGapB.Top + RangeGapA.Bottom) div 2;
        end;
        if Exist then begin
         // Exist vertical gap. Use it.
         Points[1].y := Temp.y;
         Points[2].y := Temp.y;
         if SideB = sdEast
          then Points[2].x := RangeGapB.Right
          else Points[2].x := RangeGapB.Left;
         Points[3].x := Points[2].x;
         Points[3].y := LinkPointB.y;
        end else begin
         // Find horizontal gap
         if SideB = sdEast then begin
          // Check left gap
          Exist := RangeGapA.Left >= RangeGapB.Right;
          Temp.x := (RangeGapA.Left + RangeGapB.Right) div 2;
         end else begin
          // Check right gap
          Exist := RangeGapA.Right <= RangeGapB.Left;
          Temp.x := (RangeGapA.Right + RangeGapB.Left) div 2;
         end;
         if Exist then begin
          // Exist horizontal gap. Use it.
          if SideA = sdNorth
           then Points[1].y := RangeGapA.Top
           else Points[1].y := RangeGapA.Bottom;
          Points[2].x := Temp.x;
          Points[2].y := Points[1].y;
          Points[3].x := Temp.x;
          Points[3].y := LinkPointB.y;
         end else begin
          // No gaps. Make path around
          if SideA = sdNorth then begin
           if RangeGapA.Top < RangeGapB.Top
            then Points[1].y := RangeGapA.Top
            else Points[1].y := RangeGapB.Top;
          end else
           if RangeGapA.Bottom > RangeGapB.Bottom
            then Points[1].y := RangeGapA.Bottom
            else Points[1].y := RangeGapB.Bottom;
          Points[2].y := Points[1].y;
          if SideB = sdEast then begin
           if RangeGapA.Right > RangeGapB.Right
            then Points[2].x := RangeGapA.Right
            else Points[2].x := RangeGapB.Right;
          end else
           if RangeGapA.Left < RangeGapB.Left
            then Points[2].x := RangeGapA.Left
            else Points[2].x := RangeGapB.Left;
          Points[3].x := Points[2].x;
          Points[3].y := LinkPointB.y;
         end;
        end;
       end;
      end;
   4: begin
       // Zigzag link (four intermediate points)
       // Check distribution
       if PointsOnGapInRange then exit;
       // Linking possible
       SetCount(6);
       if IsHorizA then begin
        // LinkA and LinkB is West or East
        Points[1].y := LinkPointA.y;
        // Calc vertical direction (Temp.y)
        // Test sides
        if SideA = SideB then begin
         // Unidirectional
         // Calc horizontal gap center
         if RangeGapA.Left >= RangeGapB.Right
          then Temp.x := (RangeGapA.Left + RangeGapB.Right) div 2  // left gap
          else Temp.x := (RangeGapA.Right + RangeGapB.Left) div 2; // right gap
         if ((SideA = sdEast) and (RangeGapA.Right < RangeGapB.Left)) or
            ((SideA = sdWest) and (RangeGapA.Left > RangeGapB.Right)) then begin
          // Step near LinkA
          // Calc vertical position by LinkB
          Size1 := Abs(LinkPointA.y - RangeGapB.Top) +
                   Abs(LinkPointB.y - RangeGapB.Top);
          Size2 := Abs(LinkPointA.y - RangeGapB.Bottom) +
                   Abs(LinkPointB.y - RangeGapB.Bottom);
          if Size1 < Size2
           then Temp.y := RangeGapB.Top
           else Temp.y := RangeGapB.Bottom;
          // Build path
          Points[1].x := Temp.x;
          Points[2].x := Temp.x;
          Points[2].y := Temp.y;
          if SideA = sdEast
           then Points[3].x := RangeGapB.Right
           else Points[3].x := RangeGapB.Left;
          Points[3].y := Temp.y;
          Points[4].x := Points[3].x;
          Points[4].y := LinkPointB.y;
         end else begin
          // Step near LinkB
          // Calc vertical position by LinkA
          Size1 := Abs(LinkPointA.y - RangeGapA.Top) +
                   Abs(LinkPointB.y - RangeGapA.Top);
          Size2 := Abs(LinkPointA.y - RangeGapA.Bottom) +
                   Abs(LinkPointB.y - RangeGapA.Bottom);
          if Size1 < Size2
           then Temp.y := RangeGapA.Top
           else Temp.y := RangeGapA.Bottom;
          if SideA = sdEast
           then Points[1].x := RangeGapA.Right
           else Points[1].x := RangeGapA.Left;
          Points[2].x := Points[1].x;
          Points[2].y := Temp.y;
          Points[3].x := Temp.x;
          Points[3].y := Temp.y;
          Points[4].x := Temp.x;
          Points[4].y := LinkPointB.y;
         end;
        end else begin
         // Contradirectional
         FlagA := true;
         FlagB := true;
         // Test vertical gap
         if RangeGapA.Top >= RangeGapB.Bottom then
          Temp.y := (RangeGapA.Top + RangeGapB.Bottom) div 2
         else
         if RangeGapA.Bottom <= RangeGapB.Top then
          Temp.y := (RangeGapA.Bottom + RangeGapB.Top) div 2
         else begin
          // No gaps. Make path around
          UnionRect(TempR, RangeGapA, RangeGapB);
          // Test links on TempR
          if SideA = sdEast
           then FlagA := TempR.Right - LinkPointA.x = LinkMinGap
           else FlagA := LinkPointA.x - TempR.Left = LinkMinGap;
          if SideB = sdEast
           then FlagB := TempR.Right - LinkPointB.x = LinkMinGap
           else FlagB := LinkPointB.x - TempR.Left = LinkMinGap;
          if FlagA and FlagB then begin
           // Make path around
           // Calc vertical pos (Temp.y)
           Size1 := Abs(LinkPointA.y - TempR.Top) +
             Abs(LinkPointB.y - TempR.Top);
           Size2 := Abs(LinkPointA.y - TempR.Bottom) +
             Abs(LinkPointB.y - TempR.Bottom);
           if Size1 < Size2 then Temp.y := TempR.Top else Temp.y := TempR.Bottom;
          end else begin
           // Calc points for long line from LinkPointA
           if SideA = sdEast 
            then Points[1].x := TempR.Right
            else Points[1].x := TempR.Left;
           Points[2].x := Points[1].x;
           if LinkPointB.y < LinkPointA.y
            then Points[2].y := TempR.Top
            else Points[2].y := TempR.Bottom;
           Points[3].y := Points[2].y;
           if SideB = sdEast
            then Points[3].x := RangeGapB.Right
            else Points[3].x := RangeGapB.Left;
           Points[4].x := Points[3].x;
           Points[4].y := LinkPointB.y;
           // Calc distance
           Size1 :=
             Abs(LinkPointA.x - Points[1].x) +
             Abs(Points[2].y - Points[1].y) +
             Abs(Points[3].x - Points[2].x) +
             Abs(Points[4].y - Points[3].y);
           // Calc points for long line from LinkPointA
           if SideA = sdEast
            then Points[1].x := RangeGapA.Right
            else Points[1].x := RangeGapA.Left;
           Points[2].x := Points[1].x;
           if LinkPointA.y < LinkPointB.y
            then Points[2].y := TempR.Top
            else Points[2].y := TempR.Bottom;
           Points[3].y := Points[2].y;
           if SideA = sdEast
            then Points[3].x := TempR.Left
            else Points[3].x := TempR.Right;
           Points[4].x := Points[3].x;
           Points[4].y := LinkPointB.y;
           // Calc distance
           Size2 :=
             Abs(Points[2].y - Points[1].y) +
             Abs(Points[3].x - Points[2].x) +
             Abs(Points[4].y - Points[3].y) +
             Abs(LinkPointB.x - Points[4].x);
           if Size1 < Size2 then begin
            // Reconstruct link connection for long line from LinkPointA
            if SideA = sdEast
             then Points[1].x := TempR.Right
             else Points[1].x := TempR.Left;
            Points[2].x := Points[1].x;
            if LinkPointB.y < LinkPointA.y
             then Points[2].y := TempR.Top
             else Points[2].y := TempR.Bottom;
            Points[3].y := Points[2].y;
            if SideB = sdEast
             then Points[3].x := RangeGapB.Right
             else Points[3].x := RangeGapB.Left;
            Points[4].x := Points[3].x;
            Points[4].y := LinkPointB.y;
           end;
          end;
         end;
         // Build path
         if FlagA and FlagB then begin
          if SideA = sdEast
           then Points[1].x := RangeGapA.Right
           else Points[1].x := RangeGapA.Left;
          Points[2].x := Points[1].x;
          Points[2].y := Temp.y;
          if SideB = sdEast
           then Points[3].x := RangeGapB.Right
           else Points[3].x := RangeGapB.Left;
          Points[3].y := Temp.y;
          Points[4].x := Points[3].x;
          Points[4].y := LinkPointB.y;
         end;
        end;
       end else begin
        // LinkA and LinkB is North or South
        Points[1].x := LinkPointA.x;
        // Calc horizontal direction (Temp.x)
        // Test sides
        if SideA = SideB then begin
         // Unidirectional
         // Calc vertical gap center
         if RangeGapA.Top >= RangeGapB.Bottom
          then Temp.y := (RangeGapA.Top + RangeGapB.Bottom) div 2  // top gap
          else Temp.y := (RangeGapA.Bottom + RangeGapB.Top) div 2; // bottom gap
         if ((SideA = sdNorth) and (RangeGapA.Top > RangeGapB.Bottom)) or
            ((SideA = sdSouth) and (RangeGapA.Bottom < RangeGapB.Top)) then begin
          // Step near LinkA
          // Calc horizontal position by LinkB
          Size1 := Abs(LinkPointA.x - RangeGapB.Left) +
                   Abs(LinkPointB.x - RangeGapB.Left);
          Size2 := Abs(LinkPointA.x - RangeGapB.Right) +
                   Abs(LinkPointB.x - RangeGapB.Right);
          if Size1 < Size2
           then Temp.x := RangeGapB.Left
           else Temp.x := RangeGapB.Right;
          // Build path
          Points[1].y := Temp.y;
          Points[2].y := Temp.y;
          Points[2].x := Temp.x;
          if SideA = sdNorth
           then Points[3].y := RangeGapB.Top
           else Points[3].y := RangeGapB.Bottom;
          Points[3].x := Temp.x;
          Points[4].y := Points[3].y;
          Points[4].x := LinkPointB.x;
         end else begin
          // Step near LinkB
          // Calc vertical position by LinkA
          Size1 := Abs(LinkPointA.x - RangeGapA.Left) +
                   Abs(LinkPointB.x - RangeGapA.Left);
          Size2 := Abs(LinkPointA.x - RangeGapA.Right) +
                   Abs(LinkPointB.x - RangeGapA.Right);
          if Size1 < Size2
           then Temp.x := RangeGapA.Left
           else Temp.x := RangeGapA.Right;
          // Build path
          if SideA = sdNorth
           then Points[1].y := RangeGapA.Top
           else Points[1].y := RangeGapA.Bottom;
          Points[2].y := Points[1].y;
          Points[2].x := Temp.x;
          Points[3].y := Temp.y;
          Points[3].x := Temp.x;
          Points[4].y := Temp.y;
          Points[4].x := LinkPointB.x;
         end;
        end else begin
         // Contradirectional
         FlagA := true;
         FlagB := true;
         // Test horizontal gap
         if RangeGapA.Left >= RangeGapB.Right then
          Temp.x := (RangeGapA.Left + RangeGapB.Right) div 2
         else
         if RangeGapA.Right <= RangeGapB.Left then
          Temp.x := (RangeGapA.Right + RangeGapB.Left) div 2
         else begin
          // No gaps. Make path around
          UnionRect(TempR, RangeGapA, RangeGapB);
          // Test links on TempR
          if SideA = sdNorth
           then FlagA := LinkPointA.y - TempR.Top = LinkMinGap
           else FlagA := TempR.Bottom - LinkPointA.y = LinkMinGap;
          if SideB = sdNorth
           then FlagB := LinkPointB.y - TempR.Top = LinkMinGap
           else FlagB := TempR.Bottom - LinkPointB.y = LinkMinGap;
          if FlagA and FlagB then begin
           // Make path around
           // Calc horizontal pos (Temp.x)
           Size1 := Abs(LinkPointA.x - TempR.Left) +
             Abs(LinkPointB.x - TempR.Left);
           Size2 := Abs(LinkPointA.x - TempR.Right) +
             Abs(LinkPointB.x - TempR.Right);
           if Size1 < Size2 then Temp.x := TempR.Left else Temp.x := TempR.Right;
          end else begin
           // Calc points for long line from LinkPointA
           if SideA = sdNorth 
            then Points[1].y := TempR.Top
            else Points[1].y := TempR.Bottom;
           Points[2].y := Points[1].y;
           if LinkPointB.x < LinkPointA.x
            then Points[2].x := TempR.Left
            else Points[2].x := TempR.Right;
           Points[3].x := Points[2].x;
           if SideB = sdNorth
            then Points[3].y := RangeGapB.Top
            else Points[3].y := RangeGapB.Bottom;
           Points[4].y := Points[3].y;
           Points[4].x := LinkPointB.x;
           // Calc distance
           Size1 :=
             Abs(LinkPointA.y - Points[1].y) +
             Abs(Points[2].x - Points[1].x) +
             Abs(Points[3].y - Points[2].y) +
             Abs(Points[4].x - Points[3].x);
           // Calc points for long line from LinkPointA
           if SideA = sdNorth
            then Points[1].y := RangeGapA.Top
            else Points[1].y := RangeGapA.Bottom;
           Points[2].y := Points[1].y;
           if LinkPointA.x < LinkPointB.x
            then Points[2].x := TempR.Left
            else Points[2].x := TempR.Right;
           Points[3].x := Points[2].x;
           if SideB = sdNorth
            then Points[3].y := TempR.Top
            else Points[3].y := TempR.Bottom;
           Points[4].y := Points[3].y;
           Points[4].x := LinkPointB.x;
           // Calc distance
           Size2 :=
             Abs(Points[2].x - Points[1].x) +
             Abs(Points[3].y - Points[2].y) +
             Abs(Points[4].x - Points[3].x) +
             Abs(LinkPointB.y - Points[4].y);
           if Size1 < Size2 then begin
            // Reconstruct link connection for long line from LinkPointA
            if SideA = sdNorth
             then Points[1].y := TempR.Top
             else Points[1].y := TempR.Bottom;
            Points[2].y := Points[1].y;
            if LinkPointB.x < LinkPointA.x
             then Points[2].x := TempR.Left
             else Points[2].x := TempR.Right;
            Points[3].x := Points[2].x;
            if SideB = sdNorth
             then Points[3].y := RangeGapB.Top
             else Points[3].y := RangeGapB.Bottom;
            Points[4].y := Points[3].y;
            Points[4].x := LinkPointB.x;
           end;
          end;
         end;
         // Build path
         if FlagA and FlagB then begin
          if SideA = sdNorth
           then Points[1].y := RangeGapA.Top
           else Points[1].y := RangeGapA.Bottom;
          Points[2].y := Points[1].y;
          Points[2].x := Temp.x;
          if SideB = sdNorth
           then Points[3].y := RangeGapB.Top
           else Points[3].y := RangeGapB.Bottom;
          Points[3].x := Temp.x;
          Points[4].y := Points[3].y;
          Points[4].x := LinkPointB.x;
         end;
        end;
       end;
      end;
  end;
  Points[0] := Params.LinkPointA;
  Points[Count-1] := Params.LinkPointB;
  Result := true;
 end;

 function TrySelfLinkOrtogonal(PtCount: integer; SideA, SideB: TSide): boolean;
 var Codirectional: boolean;
     IsHorizA, IsHorizB: boolean;
     Dest1, Dest2: integer;
     Temp: TPoint;
 begin
  Result := false;
  // Define side type (horizontal or vertical)
  IsHorizA := SideA in [sdEast, sdWest];
  IsHorizB := SideB in [sdEast, sdWest];
  // Define codirectional sides (both horizontal or both vertical)
  Codirectional := (IsHorizA and IsHorizB) or (not IsHorizA and not IsHorizB);
  with Params do begin
   if Codirectional then begin
    // Check point count
    if PtCount <> 4 then exit;
    // Linking possible
    SetCount(6);
    if IsHorizA then begin
     // SideA is sdEast or sdWest
     Dest1 := (LinkPointA.y - RangeGapA.Top) + (LinkPointB.y - RangeGapA.Top);
     Dest2 := (RangeGapA.Bottom - LinkPointA.y) + (RangeGapA.Bottom - LinkPointB.y);
     if Dest1 < Dest2
      then Temp.y := RangeGapA.Top
      else Temp.y := RangeGapA.Bottom;
     if SideA = sdEast
      then Points[1].x := RangeGapA.Right
      else Points[1].x := RangeGapA.Left;
     Points[1].y := LinkPointA.y;
     Points[2].x := Points[1].x;
     Points[2].y := Temp.y;
     if SideB = sdEast
      then Points[3].x := RangeGapA.Right
      else Points[3].x := RangeGapA.Left;
     Points[3].y := Temp.y;
     Points[4].x := Points[3].x;
     Points[4].y := LinkPointB.y;
    end else begin
     // SideA is sdNorth or sdSouth
     Dest1 := (LinkPointA.x - RangeGapA.Left) + (LinkPointB.x - RangeGapA.Left);
     Dest2 := (RangeGapA.Right - LinkPointA.x) + (RangeGapA.Right - LinkPointB.x);
     if Dest1 < Dest2
      then Temp.x := RangeGapA.Left
      else Temp.x := RangeGapA.Right;
     Points[1].x := LinkPointA.x;
     if SideA = sdNorth
      then Points[1].y := RangeGapA.Top
      else Points[1].y := RangeGapA.Bottom;
     Points[2].x := Temp.x;
     Points[2].y := Points[1].y;
     Points[3].x := Temp.x;
     if SideB = sdNorth
      then Points[3].y := RangeGapA.Top
      else Points[3].y := RangeGapA.Bottom;
     Points[4].x := LinkPointB.x;
     Points[4].y := Points[3].y;
    end;
   end else begin
    // Check point count
    if PtCount <> 3 then exit;
    // Linking possible
    SetCount(5);
    if IsHorizA then begin
     // SideA is sdEast or sdWest
     if SideA = sdEast
      then Points[1].x := RangeGapA.Right
      else Points[1].x := RangeGapA.Left;
     Points[1].y := LinkPointA.y;
     Points[2].x := Points[1].x;
     if SideB = sdNorth
      then Points[2].y := RangeGapA.Top
      else Points[2].y := RangeGapA.Bottom;
     Points[3].x := LinkPointB.x;
     Points[3].y := Points[2].y;
    end else begin
     // SideA is sdNorth or sdSouth
     Points[1].x := LinkPointA.x;
     if SideA = sdNorth
      then Points[1].y := RangeGapA.Top
      else Points[1].y := RangeGapA.Bottom;
     if SideB = sdEast
      then Points[2].x := RangeGapA.Right
      else Points[2].x := RangeGapA.Left;
     Points[2].y := Points[1].y;
     Points[3].x := Points[2].x;
     Points[3].y := LinkPointB.y;
    end;
   end;
   Points[0] := LinkPointA;
   Points[Count-1] := LinkPointB;
  end;
  Result := true;
 end;

 function LinkAnyCase(SideA, SideB: TSide): boolean;
 var Codirectional: boolean;
     IsHorizA, IsHorizB: boolean;
     Straight: boolean;
     Dist: integer;
 begin
  // Define side type (horizontal or vertical)
  IsHorizA := SideA in [sdEast, sdWest];
  IsHorizB := SideB in [sdEast, sdWest];
  // Define codirectional sides (both horizontal or both vertical)
  Codirectional := (IsHorizA and IsHorizB) or (not IsHorizA and not IsHorizB);
  with Params do begin
   if Codirectional then begin
    // 0 or 2 intermediate points
    if IsHorizA
     then Straight := LinkPointA.y = LinkPointB.y
     else Straight := LinkPointA.x = LinkPointB.x;
    if Straight then
     // 0 intermediate points - straight line
     SetCount(2)
    else begin
     // 2 intermediate points - zigzig
     SetCount(4);
     if IsHorizA then begin
      Dist := (LinkPointA.x + LinkPointB.x) div 2;
      Points[1].x := Dist;
      Points[1].y := LinkPointA.y;
      Points[2].x := Dist;
      Points[2].y := LinkPointB.y;
     end else begin
      Dist := (LinkPointA.y + LinkPointB.y) div 2;
      Points[1].x := LinkPointA.x;
      Points[1].y := Dist;
      Points[2].x := LinkPointB.x;
      Points[2].y := Dist;
     end;
    end;
   end else begin
    // 1 intermediate point
    SetCount(3);
    if IsHorizA then begin
     Points[1].x := LinkPointB.x;
     Points[1].y := LinkPointA.y;
    end else begin
     Points[1].x := LinkPointA.x;
     Points[1].y := LinkPointB.y;
    end;
   end;
   Points[0] := LinkPointA;
   Points[Count-1] := LinkPointB;
  end;
  Result := true;
 end;

 procedure CalcRangeGaps;
 begin
  if RangeGapExist then exit;
  with Params do
  if SelfLink then begin
   // Include in RangeGapA LinkPoints coors and inflate by LinkMinGap
   RangeGapA := RangeA;
   if LinkPointA.x < RangeGapA.Left   then RangeGapA.Left   := LinkPointA.x else
   if LinkPointA.x > RangeGapA.Right  then RangeGapA.Right  := LinkPointA.x;
   if LinkPointA.y < RangeGapA.Top    then RangeGapA.Top    := LinkPointA.y else
   if LinkPointA.y > RangeGapA.Bottom then RangeGapA.Bottom := LinkPointA.y;
   if LinkPointB.x < RangeGapA.Left   then RangeGapA.Left   := LinkPointB.x else
   if LinkPointB.x > RangeGapA.Right  then RangeGapA.Right  := LinkPointB.x;
   if LinkPointB.y < RangeGapA.Top    then RangeGapA.Top    := LinkPointB.y else
   if LinkPointB.y > RangeGapA.Bottom then RangeGapA.Bottom := LinkPointB.y;
   InflateRect(RangeGapA, LinkMinGap, LinkMinGap);
   // Set RangeGapB equal to RangeGapA
   RangeGapB := RangeGapA;
  end else begin
   // Calc ranges with min gap
   RangeGapA := RangeA;
   if LinkPointA.x < RangeGapA.Left   then RangeGapA.Left   := LinkPointA.x else
   if LinkPointA.x > RangeGapA.Right  then RangeGapA.Right  := LinkPointA.x;
   if LinkPointA.y < RangeGapA.Top    then RangeGapA.Top    := LinkPointA.y else
   if LinkPointA.y > RangeGapA.Bottom then RangeGapA.Bottom := LinkPointA.y;
   InflateRect(RangeGapA, LinkMinGap, LinkMinGap);
   RangeGapB := RangeB;
   if LinkPointB.x < RangeGapB.Left   then RangeGapB.Left   := LinkPointB.x else
   if LinkPointB.x > RangeGapB.Right  then RangeGapB.Right  := LinkPointB.x;
   if LinkPointB.y < RangeGapB.Top    then RangeGapB.Top    := LinkPointB.y else
   if LinkPointB.y > RangeGapB.Bottom then RangeGapB.Bottom := LinkPointB.y;
   InflateRect(RangeGapB, LinkMinGap, LinkMinGap);
  end;
  RangeGapExist := true;
 end;

 function IsLineIntersectRect(const PointA, PointB: TPoint;
   const R: TRect): boolean;
 var CodeA, CodeB: cardinal;
 begin
  CodeA :=
    byte(PointA.y <= R.Top) shl 3 or
    byte(PointA.y >= R.Bottom) shl 2 or
    byte(PointA.x >= R.Right) shl 1 or
    byte(PointA.x <= R.Left);
  CodeB :=
    byte(PointB.y <= R.Top) shl 3 or
    byte(PointB.y >= R.Bottom) shl 2 or
    byte(PointB.x >= R.Right) shl 1 or
    byte(PointB.x <= R.Left);
  Result := CodeA and CodeB = 0;
 end;

var
  i: integer;
  Method: integer;
  SidesA, SidesB: TSides;
  SideA, SideB: TSide;
  NeedFull: boolean;
  IsHoriz: boolean;

begin
 Result := false;
 // Test counts
 Count := Length(Points);
 if Count <> Length(PointTypes) then exit;
 // Test point types (support line's only and only not closed and with one figure)
 for i:=0 to Count-2 do if PointTypes[i] <> ptNode then exit;
 if (Count > 0) and (PointTypes[Count-1] <> ptEndNode) then exit;
 with Params do begin
  if Mode = rmAsNeeded then begin
   // Test count for full reroute
   NeedFull := false;
   if Count < 2 then
    NeedFull := true
   else
   // Test ortogonal for full reroute
   if Ortogonal then
    // Check all ortogonal segments
    for i:=0 to Count-2 do
     if (Points[i].x <> Points[i+1].x) and
        (Points[i].y <> Points[i+1].y) then begin
      // Slant line
      NeedFull := true;
      break;
     end;
  end else
   NeedFull := Mode = rmAlways;
  RangeGapExist := false;
  repeat
   // Reroute
   if NeedFull then begin
    if Ortogonal then begin
     CalcRangeGaps;
     // Calc active sides
     SidesA := CalcSides(LinkPointA, RangeGapA);
     SidesB := CalcSides(LinkPointB, RangeGapB);
     // Check error
     if (SidesA = []) or (SidesB = []) then exit;
     // Find link method (count of intermediate points)
     for Method:=0 to 4 do begin
      for SideA:=Low(SideA) to High(SideA) do begin
       if not (SideA in SidesA) then continue;
       for SideB:=Low(SideB) to High(SideB) do begin
        if not (SideB in SidesB) then continue;
        if SelfLink
         then Result := TrySelfLinkOrtogonal(Method, SideA, SideB)
         else Result := TryLinkOrtogonal(Method, SideA, SideB);
        if Result then exit;
       end;
      end;
     end;
     { // Method not found - link straight line
     SetCount(2);
     Points[0] := LinkPointA;
     Points[Count-1] := LinkPointB; }
     // Method not found - link with minimal points
     for SideA:=Low(SideA) to High(SideA) do begin
      if not (SideA in SidesA) then continue;
      for SideB:=Low(SideB) to High(SideB) do begin
       if not (SideB in SidesB) then continue;
       if LinkAnyCase(SideA, SideB) then break;
      end;
     end;
    end else begin
     // Move end points to link points
     {if Count < 2 then }SetCount(2);
     Points[0] := LinkPointA;
     Points[Count-1] := LinkPointB;
    end;
    break; // connector rerouted
   end else begin
    // Don't rerout
    if Ortogonal then begin
     // Check count and LinkPoints position
     if (Count >= 2) and (
         (LinkPointA.x <> Points[0].x) or
         (LinkPointA.y <> Points[0].y) or
         (LinkPointB.x <> Points[Count-1].x) or
         (LinkPointB.y <> Points[Count-1].y)) then begin
      if Count = 2 then begin
       // Can't save ortogonal. Just move first and end points to link points
       Points[0] := LinkPointA;
       Points[1] := LinkPointB;
      end else begin
       // Move first point to LinkPointA
       // IsHoriz := Points[0].y = Points[1].y;
       i := 1;
       IsHoriz := true;
       repeat
        if (Points[i-1].x <> Points[i].x) or
           (Points[i-1].y <> Points[i].y) then begin
         // Define segment position
         IsHoriz := Points[i-1].y = Points[i].y;
         if i and 1 = 0 then IsHoriz := not IsHoriz;
         break;
        end;
        inc(i);
       until i = Length(Points);
       Points[0] := LinkPointA;
       if IsHoriz
        then Points[1].y := LinkPointA.y
        else Points[1].x := LinkPointA.x;
       // Move last point to LinkPointA
       // IsHoriz := Points[Count-1].y = Points[Count-2].y;
       i := Length(Points)-1;
       IsHoriz := true;
       repeat
        if (Points[i-1].x <> Points[i].x) or
           (Points[i-1].y <> Points[i].y) then begin
         // Define segment position
         IsHoriz := Points[i-1].y = Points[i].y;
         if i and 1 <> (Length(Points)-1) and 1 then IsHoriz := not IsHoriz;
         break;
        end;
        dec(i);
       until i = 0;
       Points[Count-1] := LinkPointB;
       if IsHoriz
        then Points[Count-2].y := LinkPointB.y
        else Points[Count-2].x := LinkPointB.x;
      end;
     end else begin
      // Just move first and end points to link points
      Points[0] := LinkPointA;
      Points[Count-1] := LinkPointB;
     end;
    end else begin
     // Non ortogonal link without reroute
     Points[0] := LinkPointA;
     Points[Count-1] := LinkPointB;
    end;
    if Mode = rmAsNeeded then begin
     // Check overlaping (connector with connected objects)
     CalcRangeGaps;
     for i:=0 to Count-2 do begin
      // Test segment for intersection with object ranges
      if i > 0
       then NeedFull := IsLineIntersectRect(Points[i], Points[i+1], RangeGapA)
       else NeedFull := IsLineIntersectRect(Points[i], Points[i+1], RangeA);
      if NeedFull then break;
      if i < Count-2
       then NeedFull := IsLineIntersectRect(Points[i], Points[i+1], RangeGapB)
       else NeedFull := IsLineIntersectRect(Points[i], Points[i+1], RangeB);
      if NeedFull then break;
     end;
    end;
   end;
   // Repeat if NeedFull changed to true
  until not NeedFull;
 end;
 Result := true;
end;

procedure GetPathInfo(const Points: TPointArray; const Types: TPointTypeArray;
  var Info: TPathInfo);
var i, Count, LastCount: integer;
    FigIndex, FigCount: integer;
    FirstNodeIndex: integer;
begin
 Info.IsCurve := false;
 Count := Length(Points);
 if (Count = 0) or (Count <> Length(Types)) then begin
  Info.Figures := Nil;
  exit;
 end;
 FigCount := Length(Info.Figures);
 FigIndex := -1;
 FirstNodeIndex := 0;
 with Info do begin
  PointCount := Count;
  for i:=0 to Count-1 do begin
   if i = FirstNodeIndex then begin
    // Start of figure
    inc(FigIndex);
    if FigIndex >= FigCount then begin
     FigCount := FigIndex+1;
     SetLength(Figures, FigCount);
    end;
    with Figures[FigIndex] do begin
     FirstNode := FirstNodeIndex;
     LastNode := -1;
     LastPoint := -1;
     IsClosed := false;
     IsCurve := false;
    end;
   end;
   case Types[i] of
    ptControl:
      // Curve point in figure
      Figures[FigIndex].IsCurve := true;
    ptEndNode,
    ptEndNodeClose:
      with Figures[FigIndex] do begin
       // End of figure
       LastNode := i;
       IsClosed := Types[i] = ptEndNodeClose;
       if (i < Count-2) and (Types[i+1] = ptControl) then begin
        LastCount := 3;
        IsCurve := true;
       end else
        LastCount := 1;
       FirstNodeIndex := i + LastCount;
       LastPoint := i + LastCount-1;
       Info.IsCurve := Info.IsCurve or IsCurve;
      end;
   end;
  end;
 end;
 if FigIndex < FigCount-1 then SetLength(Info.Figures, FigIndex+1);
end;

function GetFigureIndex(const Info: TPathInfo; PointIndex: integer): integer;
var i: integer;
begin
 Result := -1;
 for i:=0 to Length(Info.Figures)-1 do with Info.Figures[i] do
  if (PointIndex >= FirstNode) and (PointIndex <= LastPoint) then begin
   Result := i;
   break;
  end;
end;

function ChangePathCount(var Points: TPointArray; var Types: TPointTypeArray;
  Index, Delta: integer): boolean;
var Count: integer;
begin
 Count := Length(Points);
 Result := (Count = Length(Types)) and (Index >= 0) and (Index <= Count);
 if not Result then exit;
 if Delta > 0 then begin
  SetLength(Points, Count + Delta);
  SetLength(Types, Count + Delta);
 end else
  dec(Index, Delta);
 Move(Points[Index], Points[Index+Delta], (Count - Index) * SizeOf(Points[0]));
 Move(Types[Index], Types[Index+Delta], (Count - Index) * SizeOf(Types[0]));
 if Delta < 0 then begin
  SetLength(Points, Count + Delta);
  SetLength(Types, Count + Delta);
 end;
end;

end.
