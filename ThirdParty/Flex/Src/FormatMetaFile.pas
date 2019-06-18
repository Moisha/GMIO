/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    FlexGraphics library file formats support        //
//    Metafiles (wmf/emf) files                        //
//                                                     //
/////////////////////////////////////////////////////////

unit FormatMetaFile;

{$I FlexDefs.inc}

interface

uses
  Windows, Types, UITypes, Classes, Graphics, FlexBase, FlexFileFormats;

resourcestring
  sWmfFileDescription = 'Windows Metafiles';
  sEmfFileDescription = 'Windows Enhanced Metafiles';

type
  TDoublePoint = record
    X, Y: double;
  end;

  TFlexMetafileFormat = class(TFlexFileFormat)
  protected
    // Internal fields
    FMetafile: TMetafile;
    FFlexPanel: TFlexPanel;
    FCanvas: TCanvas;
    FXForm: TXForm;
    FWExt: TDoublePoint;
    FWOrg: TDoublePoint;
    FVExt: TDoublePoint;
    FVOrg: TDoublePoint;
    procedure RegisterSupportedExtensions; override;
    procedure MetaToFlex(var Points; PointCount: integer);
    procedure ConvertEmr(DC: HDC; var lpEmr: TEMR);
  public
    procedure ImportFromStream(AStream: TStream; AFlexPanel: TFlexPanel;
      const Extension: TFlexFileExtension; const AFileName: string); override;
    procedure ExportToStream(AStream: TStream; AFlexPanel: TFlexPanel;
      const Extension: TFlexFileExtension; const AFileName: string); override;
  end;

implementation

uses
  SysUtils, Math, FlexUtils, FlexControls, FlexPath, FlexProps;

function EnumEMFCallback(
  DC: HDC;                      // handle to device context
  var lpHTable: THandleTable;   // pointer to metafile handle table
  var lpEMR: TEMR;              // pointer to metafile record
  nObj: integer;                // count of objects
  lpClientData: integer         // pointer to optional data
): integer; far; stdcall;
begin
  if not PlayEnhMetafileRecord(DC, lpHTable, PEnhMetaRecord(@lpEMR)^, nObj) then
    Result := 0
  else begin
    Result := 1;
    TFlexMetafileFormat(lpClientData).ConvertEmr(DC, lpEmr);
  end;
end;

// Transformation to map recording device's device coordinate space
// to destination device's logical coordinate space
function GetPlayTransformation(hEmf: HENHMETAFILE; const rcPic: TRect;
  var xformPlay: TXForm): boolean;
var
  Header: TEnhMetaHeader;
  sx0, sy0, sx1, sy1: double;
  rx, ry: double;
begin
  Result := False;
  if GetEnhMetaFileHeader(hEmf, sizeof(Header), @Header) <= 0 then
    Exit;
  try
    // frame rectangle size in 0.01 mm -> 1 mm -> percentage -> device pixels
    with Header do begin
      sx0 := rclFrame.left   / 100.0 / szlMillimeters.cx * szlDevice.cx;
      sy0 := rclFrame.top    / 100.0 / szlMillimeters.cy * szlDevice.cy;
      sx1 := rclFrame.right  / 100.0 / szlMillimeters.cx * szlDevice.cx;
      sy1 := rclFrame.bottom / 100.0 / szlMillimeters.cy * szlDevice.cy;
    end;
    // source to destination ratio
    with rcPic do begin
      rx := (right  - left) / (sx1 - sx0);
      ry := (bottom - top)  / (sy1 - sy0);
    end;
    with xformPlay do begin
      // x' = x * eM11 + y * eM21 + eDx
      // y' = x * eM12 + y * eM22 + eDy
      eM11 := rx;
      eM21 := 0;
      eDx  := (-sx0*rx + rcPic.left);
      eM12 := 0;
      eM22 := ry;
      eDy  := (-sy0*ry + rcPic.top);
    end;
    Result := True;
  except
    // Skip exceptions
  end;
end;

procedure PointsToAngles(const R: TRect; const ptStart, ptEnd: TPoint;
  var angStart, angEnd: double);
var
  HR, WR: double;
  Coeff: double;

  function CalcAngle(X, Y: double): double;
  begin
    Y := Y * Coeff;
    if X = 0 then begin
      if Y > 0
        then Result := 90
        else Result := 270;
    end else begin
      Result := 180.0 * ArcTan2(Y, X) / pi;
      if Result < 0 then
        Result := 360.0 + Result;
    end;
  end;

begin
  WR := (R.Right - R.Left) / 2;
  HR := (R.Bottom - R.Top) / 2;
  if HR = 0
    then Coeff := 0
    else Coeff := WR / HR;
  angStart := CalcAngle(ptStart.X - R.Left - WR, HR - (ptStart.Y - R.Top));
  angEnd := CalcAngle(ptEnd.X - R.Left - WR, HR - (ptEnd.Y - R.Top));
end;

// TFlexMetafileFormat ////////////////////////////////////////////////////////

procedure TFlexMetafileFormat.RegisterSupportedExtensions;
begin
  RegisterExtension('wmf', sWmfFileDescription, [skImport, skExport]);
  RegisterExtension('emf', sEmfFileDescription, [skImport, skExport]);
end;

procedure TFlexMetafileFormat.ImportFromStream(AStream: TStream;
  AFlexPanel: TFlexPanel; const Extension: TFlexFileExtension;
  const AFileName: string);
var
  Bmp: TBitmap;
  R: TRect;
begin
  FMetafile := Nil;
  Bmp := TBitmap.Create;
  try
    FMetafile := TMetafile.Create;
    FMetafile.LoadFromStream(AStream);

    FCanvas := Bmp.Canvas;
    FFlexPanel := AFlexPanel;

    FVOrg.X := 0;
    FVOrg.Y := 0;
    FVExt.X := 1;
    FVExt.Y := 1;

    FWOrg.X := 0;
    FWOrg.Y := 0;
    FWExt.X := 1;
    FWExt.Y := 1;

    R := Rect(0, 0, FMetafile.Width, FMetafile.Height);
    Bmp.Width := R.Right;
    Bmp.Height := R.Bottom;

    FFlexPanel.DocWidth := R.Right * PixelScaleFactor;
    FFlexPanel.DocHeight := R.Bottom * PixelScaleFactor;

    GetPlayTransformation(FMetafile.Handle, R, FXForm);

    // Enumerate
    EnumEnhMetafile(FCanvas.Handle, FMetafile.Handle,
      @EnumEMFCallback, Self, R);
  finally
    FCanvas := Nil;
    FMetafile.Free;
    FMetafile := Nil;
    FFlexPanel := Nil;
    Bmp.Free;
  end;
end;

procedure TFlexMetafileFormat.ExportToStream(AStream: TStream;
  AFlexPanel: TFlexPanel; const Extension: TFlexFileExtension;
  const AFileName: string);
begin
  FMetafile := TMetafile.Create;
  try
    FFlexPanel := AFlexPanel;
    FMetafile.Width := ScaleValue(FFlexPanel.DocWidth, 100);
    FMetafile.Height := ScaleValue(FFlexPanel.DocHeight, 100);
    // Paint to metafile canvas
    FCanvas := TMetafileCanvas.Create(FMetafile, 0);
    try
      with FFlexPanel do
        PaintTo(FCanvas,
          Rect(0, 0, ScaleValue(DocWidth, 100), ScaleValue(DocHeight, 100)),
          Point(0, 0), 100, ActiveScheme, False, False, False, True, True);
    finally
      FCanvas.Free;
      FCanvas := Nil;
    end;
    FMetafile.Enhanced := CompareText(Extension.Extension, 'wmf') <> 0;
    FMetafile.SaveToStream(AStream);
  finally
    FFlexPanel := Nil;
    FMetafile.Free;
    FMetafile := Nil;
  end;
end;

procedure TFlexMetafileFormat.MetaToFlex(var Points; PointCount: integer);
var
  i: integer;
  px, py: double;
  pt: PPoint;
begin
  pt := @Points;
  for i:=0 to PointCount-1 do begin
    px := (pt.x - FWOrg.X) * FVExt.X / FWExt.X + FVOrg.X;
    py := (pt.y - FWOrg.Y) * FVExt.Y / FWExt.Y + FVOrg.Y;
    pt.X := Round(
      (px * FXForm.eM11 + px * FXForm.eM12 + FXForm.eDx) * PixelScaleFactor);
    pt.Y := Round(
      (py * FXForm.eM21 + py * FXForm.eM22 + FXForm.eDy) * PixelScaleFactor);
    inc(pt);
  end;
end;

procedure TFlexMetafileFormat.ConvertEmr(DC: HDC; var lpEmr: TEMR);
type
  TSetupProp = (spPen, spBrush, spFont);
  TSetupProps = set of TSetupProp;

var
  i, j: integer;
  Control: TFlexControl;
  Curve: TFlexCurve;
  Text: TFlexText;
  TextMetric: TTextMetric;
  TextStr: string;
  Points: TPointArray;
  PointTypes: TPointTypeArray;
  PolyPointCount: array of Dword;
  IsSolid: boolean;
  pt: TPoint;
  R: TRect;
  Bmp: TBitmap;
  AngStart, AngEnd: double;

  function Scale(const Value: integer;
    NoPixelScaleFactor: boolean = False): integer;
  begin
    if NoPixelScaleFactor then
      Result := Round(Value * FVExt.X / FWExt.Y * FXForm.eM11)
    else
      Result :=
        Round(Value * FVExt.X / FWExt.Y * FXForm.eM11 * PixelScaleFactor);
  end;

  procedure SetupProps(Control: TFlexControl; Props: TSetupProps);
  var
    PenProp: TPenProp;
    BrushProp: TBrushProp;
    FontProp: TFontProp;
    LogObj: array of byte;
  begin
    if spPen in Props then begin
      PenProp := TPenProp(Control.Props['Pen']);
      if Assigned(PenProp) and (TObject(PenProp) is TPenProp) then begin
        SetLength(LogObj, SizeOf(TLogPen));
        GetObject(GetCurrentObject(DC, OBJ_PEN), Length(LogObj), @LogObj[0]);
        with PenProp, PLogPen(@LogObj[0])^ do begin
          Style := TPenStyle(lopnStyle);
          Width := Scale(lopnWidth.X);
          if Width = 0 then
            Width := 1;
          Color := lopnColor;
        end;
      end;
    end;
    if spBrush in Props then begin
      BrushProp := TBrushProp(Control.Props['Brush']);
      if Assigned(BrushProp) and (TObject(BrushProp) is TBrushProp) then begin
        SetLength(LogObj, SizeOf(TLogBrush));
        GetObject(GetCurrentObject(DC, OBJ_BRUSH), Length(LogObj), @LogObj[0]);
        with BrushProp, PLogBrush(@LogObj[0])^ do begin
          Color := lbColor;
          case lbStyle of
           BS_SOLID:
             Style := bsSolid;
           BS_HATCHED:
             case lbHatch of
               HS_BDIAGONAL  : Style := bsBDiagonal;
               HS_CROSS      : Style := bsCross;
               HS_DIAGCROSS  : Style := bsDiagCross;
               HS_FDIAGONAL  : Style := bsFDiagonal;
               HS_HORIZONTAL : Style := bsHorizontal;
               HS_VERTICAL   : Style := bsVertical;
             end;
           BS_DIBPATTERN,
           BS_DIBPATTERN8X8,
           BS_DIBPATTERNPT:
             begin
               Bmp := TBitmap.Create;
               try
                 with PBitmapInfo(lbHatch)^.bmiHeader do begin
                   Bmp.Width := biWidth;
                   Bmp.Height := biHeight;
                 end;
                 StretchDIBits(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height,
                   0, 0, Bmp.Width, Bmp.Height,
                   PChar(lbHatch) + PBitmapInfo(lbHatch).bmiHeader.biSize,
                   PBitmapInfo(lbHatch)^, lbColor and $FFFF, SRCCOPY);
                 Bitmap := Bmp;
                 Method := bmBitmap;
               finally
                 Bmp.Free;
               end;
             end;
           else
             Style := bsClear;
          end;
        end;
      end;
    end;
    if spFont in Props then begin
      FontProp := TFontProp(Control.Props['Font']);
      if Assigned(FontProp) and (TObject(FontProp) is TFontProp) then begin
        SetLength(LogObj, SizeOf(TLogFont));
        GetObject(GetCurrentObject(DC, OBJ_FONT), Length(LogObj), @LogObj[0]);
        with PLogFont(@LogObj[0])^ do begin
          lfHeight := Scale(lfHeight, True);
        end;
        FontProp.Handle := CreateFontIndirect(PLogFont(@LogObj[0])^);
        FontProp.Color := GetTextColor(DC);
      end;
    end;
  end;

  procedure AssignSmallPoints(SrcPoints: PSmallPoint; PointCount: integer);
  var
    i: integer;
  begin
    SetLength(Points, PointCount);
    for i:=0 to PointCount-1 do begin
      Points[i].X := SrcPoints.x;
      Points[i].Y := SrcPoints.y;
      inc(SrcPoints);
    end;
  end;

  procedure AssignPoints(SrcPoints: PPoint; PointCount: integer);
  var
    i: integer;
  begin
    SetLength(Points, PointCount);
    for i:=0 to PointCount-1 do begin
      Points[i].X := SrcPoints.x;
      Points[i].Y := SrcPoints.y;
      inc(SrcPoints);
    end;
  end;

  function PictureFromEMR(Emr: Pointer;
    offBmiSrc, cbBmiSrc, offBitsSrc, cbBitsSrc: integer): TFlexControl;
  var
    BitmapInfo: PBitmapInfo;
    Bits: Pointer;
    Bmp: TBitmap;
  begin
    if cbBitsSrc = 0 then begin
      Result := TFlexBox.Create(FFlexPanel, FFlexPanel.ActiveScheme,
        FFlexPanel.ActiveLayer);
      SetupProps(Result, [spBrush]);
      TFlexBox(Result).PenProp.Style := psClear;
      Exit;
    end;
    BitmapInfo := PBitmapInfo(PChar(Emr) + offBmiSrc);
    Bits := PChar(Emr) + offBitsSrc;
    Bmp := TBitmap.Create;
    try
      Bmp.Width := BitmapInfo.bmiHeader.biWidth;
      Bmp.Height := Abs(BitmapInfo.bmiHeader.biHeight);
      StretchDIBits(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0,
        Bmp.Width, Bmp.Height, Bits, BitmapInfo^, DIB_RGB_COLORS, SRCCOPY);
      Result := TFlexPicture.Create(FFlexPanel, FFlexPanel.ActiveScheme,
        FFlexPanel.ActiveLayer);
      with TFlexPicture(Result) do begin
        PictureProp.Graphic := Bmp;
        AutoSizeProp.Value := True;
        AutoSizeProp.Value := False;
      end;
    finally
      Bmp.Free;
    end;
  end;

begin
  case lpEMR.iType of
    EMR_SETVIEWPORTEXTEX:
      with PEMRSetViewPortExtEx(@lpEMR).szlExtent do begin
        FVExt.X := cx;
        FVExt.Y := cy;
      end;
    EMR_SETVIEWPORTORGEX:
      with PEMRSetViewPortOrgEx(@lpEMR).ptlOrigin do begin
        FVOrg.X := X;
        FVOrg.Y := Y;
      end;
    EMR_SETWINDOWEXTEX:
      with PEMRSetViewPortExtEx(@lpEMR).szlExtent do begin
        FWExt.X := cx;
        FWExt.Y := cy;
      end;
    EMR_SETWINDOWORGEX:
      with PEMRSetViewPortOrgEx(@lpEMR).ptlOrigin do begin
        FWOrg.X := X;
        FWOrg.Y := Y;
      end;
    EMR_RECTANGLE,
    EMR_ROUNDRECT:
      begin
        if lpEMR.iType = EMR_RECTANGLE then begin
          R := PEmrRectangle(@lpEMR).rclBox;
          j := 0;
        end else begin
          R := PEmrRoundRect(@lpEMR).rclBox;
          j := Scale(PEmrRoundRect(@lpEMR).szlCorner.cx);
        end;
        MetaToFlex(R, 2);
        Control := TFlexBox.Create(FFlexPanel, FFlexPanel.ActiveScheme,
          FFlexPanel.ActiveLayer);
        Control.DocRect := R;
        TFlexBox(Control).RoundnessProp.Value := j;
        SetupProps(Control, [spPen, spBrush]);
      end;
    EMR_ARC,
    EMR_ELLIPSE:
      begin
        Control := TFlexEllipse.Create(FFlexPanel, FFlexPanel.ActiveScheme,
          FFlexPanel.ActiveLayer);
        if lpEMR.iType = EMR_ARC
          then R := PEmrArc(@lpEMR).rclBox
          else R := PEmrEllipse(@lpEMR).rclBox;
        MetaToFlex(R, 2);
        Control.DocRect := R;
        if lpEMR.iType = EMR_ARC then begin
          with PEmrArc(@lpEMR)^ do
            PointsToAngles(rclBox, ptlStart, ptlEnd, AngStart, AngEnd);
          TFlexEllipse(Control).BeginAngleProp.Value :=
            Round(AngStart * PixelScaleFactor);
          TFlexEllipse(Control).EndAngleProp.Value :=
            Round(AngEnd * PixelScaleFactor);
        end;
        SetupProps(Control, [spPen, spBrush]);
      end;
    EMR_EXTTEXTOUTW:
      begin
       with PEMRExtTextOut(@lpEMR)^ do begin
         TextStr := WideCharLenToString(
           PWideChar(PChar(@lpEMR) + emrtext.offString), emrtext.nChars);
         pt := emrtext.ptlReference;
         MetaToFlex(pt, 1);
       end;
       Text := TFlexText.Create(FFlexPanel, FFlexPanel.ActiveScheme,
         FFlexPanel.ActiveLayer);
       Text.Left := pt.X;
       Text.Top := pt.Y;
       Text.TextProp.Text := TextStr;
       SetupProps(Text, [spFont]);
       Text.AutoSizeProp.Value := true;
       j := GetTextAlign(DC);
       case j and (TA_LEFT or TA_RIGHT or TA_CENTER) of
         TA_CENTER : Text.Left := Text.Left - Text.Width div 2;
         TA_RIGHT  : Text.Left := Text.Left - Text.Width;
       end;
       case j and (TA_TOP or TA_BOTTOM or TA_BASELINE) of
         TA_BASELINE:
           begin
             GetTextMetrics(DC, TextMetric);
             Text.Top := Text.Top - Scale(TextMetric.tmAscent);
           end;
         TA_BOTTOM:
           Text.Top := Text.Top - Text.Height;
       end;
      end;
    EMR_POLYLINE,
    EMR_POLYLINE16,
    EMR_POLYGON,
    EMR_POLYGON16:
      begin
        if (lpEMR.iType = EMR_POLYLINE16) or
           (lpEMR.iType = EMR_POLYGON16) then begin
          with PEMRPolyline16(@lpEmr)^ do
            AssignSmallPoints(@apts, cpts);
        end else
          with PEMRPolyline(@lpEmr)^ do
            AssignPoints(@aptl, cptl);

        Curve := TFlexCurve.Create(FFlexPanel, FFlexPanel.ActiveScheme,
          FFlexPanel.ActiveLayer);
        MetaToFlex(Points[0], Length(Points));
        Curve.SetPoints(Points);
        if (lpEMR.iType = EMR_POLYGON16) or (lpEMR.iType = EMR_POLYGON) then begin
          Curve.IsSolidProp.Value := true;
          SetupProps(Curve, [spPen, spBrush]);
        end else
          SetupProps(Curve, [spPen]);
      end;
    EMR_POLYPOLYGON16,
    EMR_POLYPOLYGON,
    EMR_POLYPOLYLINE16,
    EMR_POLYPOLYLINE:
      begin
        if (lpEMR.iType = EMR_POLYPOLYLINE16) or
           (lpEMR.iType = EMR_POLYPOLYGON16) then
          with PEMRPolyPolyline16(@lpEmr)^ do begin
            SetLength(PolyPointCount, nPolys);
            for i:=0 to Length(PolyPointCount)-1 do
              PolyPointCount[i] := aPolyCounts[i];
            AssignSmallPoints(@aPolyCounts[nPolys], cpts);
          end
        else
          with PEMRPolyPolyline(@lpEmr)^ do begin
            SetLength(PolyPointCount, nPolys);
            for i:=0 to Length(PolyPointCount)-1 do
              PolyPointCount[i] := aPolyCounts[i];
            AssignPoints(@aPolyCounts[nPolys], cptl);
          end;
        IsSolid :=
          (lpEMR.iType = EMR_POLYPOLYGON16) or (lpEMR.iType = EMR_POLYPOLYGON);

        SetLength(PointTypes, Length(Points));
        for i:=0 to Length(PointTypes)-1 do
          PointTypes[i] := ptNode;

        j := 0;
        for i:=0 to Length(PolyPointCount)-1 do begin
          inc(j, PolyPointCount[i]);
          if IsSolid
            then PointTypes[j-1] := ptEndNodeClose
            else PointTypes[j-1] := ptEndNode;
        end;

        Curve := TFlexCurve.Create(FFlexPanel, FFlexPanel.ActiveScheme,
          FFlexPanel.ActiveLayer);
        MetaToFlex(Points[0], Length(Points));
        Curve.SetPointsEx(Points, PointTypes);
        if IsSolid
          then SetupProps(Curve, [spPen, spBrush])
          else SetupProps(Curve, [spPen]);
      end;
    EMR_STRETCHDIBITS:
      begin
        with PEmrStretchDIBits(@lpEMR)^ do begin
          Control :=
            PictureFromEMR(@lpEmr, offBmiSrc, cbBmiSrc, offBitsSrc, cbBitsSrc);
          R := Rect(xDest, yDest, xDest + cxDest, yDest + cyDest);
          MetaToFlex(R, 2);
          Control.DocRect := R;
        end;
      end;
    EMR_BITBLT:
      begin
        with PEmrBitBlt(@lpEMR)^ do begin
          Control :=
            PictureFromEMR(@lpEmr, offBmiSrc, cbBmiSrc, offBitsSrc, cbBitsSrc);
          R := Rect(xDest, yDest, xDest + cxDest, yDest + cyDest);
          MetaToFlex(R, 2);
          Control.DocRect := R;
        end;
      end;
   EMR_STRETCHBLT:
      begin
        with PEmrStretchBlt(@lpEMR)^ do begin
          Control :=
            PictureFromEMR(@lpEmr, offBmiSrc, cbBmiSrc, offBitsSrc, cbBitsSrc);
          R := Rect(xDest, yDest, xDest + cxDest, yDest + cyDest);
          MetaToFlex(R, 2);
          Control.DocRect := R;
        end;
      end;
   EMR_MASKBLT:
      begin
        with PEmrMaskBlt(@lpEMR)^ do begin
          Control :=
            PictureFromEMR(@lpEmr, offBmiSrc, cbBmiSrc, offBitsSrc, cbBitsSrc);
          R := Rect(xDest, yDest, xDest + cxDest, yDest + cyDest);
          MetaToFlex(R, 2);
          Control.DocRect := R;
          if Control is TFlexPicture then
            TFlexPicture(Control).PictureProp.Masked := True;
        end;
      end;
  end;
end;

initialization
  RegisteredFlexFileFormats.RegisterFormat(TFlexMetafileFormat);

end.
