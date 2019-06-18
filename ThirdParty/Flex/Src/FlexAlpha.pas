/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    Alpha blending buffer support                    //
//                                                     //
/////////////////////////////////////////////////////////

unit FlexAlpha;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes, Graphics;

type
  PAlphaLine = ^TAlphaLine;
  TAlphaLine = array[0..MaxInt div SizeOf(TRGBQuad)-1] of TRGBQuad;

  TStoreDrawEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Graphic: TGraphic) of object;

  TAlphaBuffer = class
  private
   FParent: TAlphaBuffer;
   FSourceDC: HDC;
   FCanvas: TCanvas;
   FCanvasDC: HDC;
   FCanvasDCBitmap: HBitmap;
   FPaintRect: TRect;
   FPaintWidth: integer;
   FPaintHeight: integer;
   FWidth: integer;
   FHeight: integer;
   FBits: PAlphaLine;
   FBytesPerLine: integer;
   FInfo: TBitmapInfo;
   FHandle: HBitmap;
   FStored: boolean;
  protected
   procedure ResetCanvas(DeleteDIB: boolean = false);
   procedure RecreateDIB(AWidth, AHeight: integer;
     ForceRecreate: boolean = false; FillColor: cardinal = $FF000000);
  public
   constructor Create(AParent: TAlphaBuffer = Nil);
   destructor Destroy; override;
   function  BeginPaint(ASourceDC: HDC;
     const APaintRect: TRect): boolean; virtual;
   function  EndPaint(Transparency: integer): boolean; virtual;
   function  StoreFromGraphic(Graphic: TGraphic;
     OnDraw: TStoreDrawEvent = Nil): boolean;
   function  PaintStored(Dest: TAlphaBuffer; const APaintRect,
     ASrcRect: TRect): boolean;
   property  Canvas: TCanvas read FCanvas;
   property  SourceDC: HDC read FSourceDC;
   property  Handle: HBitmap read FHandle;
   property  Width: integer read FWidth;
   property  Height: integer read FHeight;
  end;

implementation

// TFlexAlphaBuffer ///////////////////////////////////////////////////////////

constructor TAlphaBuffer.Create(AParent: TAlphaBuffer = Nil);
begin
 FParent := AParent;
 FCanvas := TCanvas.Create;
end;

destructor TAlphaBuffer.Destroy;
begin
 inherited;
 ResetCanvas(true);
 FCanvas.Free;
end;

procedure TAlphaBuffer.ResetCanvas(DeleteDIB: boolean = false);
begin
 if FCanvasDC <> 0 then begin
  // Reset canvas DC
  FCanvas.Handle := 0;
  // Select old canvas bitmap
  SelectObject(FCanvasDC, FCanvasDCBitmap);
  FCanvasDCBitmap := 0;
  // Delete Canvas DC
  DeleteDC(FCanvasDC);
  FCanvasDC := 0;
 end;
 // Delete DIB-section
 if DeleteDIB then begin
  if FHandle <> 0 then begin
   DeleteObject(FHandle);
   FHandle := 0;
  end;
  FWidth := 0;
  FHeight := 0;
 end;
 FStored := false;
end;

procedure TAlphaBuffer.RecreateDIB(AWidth, AHeight: integer;
  ForceRecreate: boolean = false; FillColor: cardinal = $FF000000);
var BitLine: PAlphaLine;
    x, y: integer;
begin
 if ForceRecreate or (AWidth > FWidth) or (AHeight > FHeight) then begin
  // Source rect larger then existing. Enlarge DIB-section
  ResetCanvas(true);
  FWidth := AWidth;
  FHeight := AHeight;
  FillChar(FInfo, sizeof(FInfo), 0);
  with FInfo.bmiHeader do begin
   biSize := SizeOf(TBitmapInfoHeader);
   biWidth := FWidth;
   biHeight := -FHeight;
   biPlanes := 1;
   biBitCount := 32;
   biCompression := BI_RGB;
   // Calc bytes per line
   FBytesPerLine := (((biBitCount * biWidth) + 31) and not 31) div 8;
  end;
 end;
 if FCanvasDC = 0 then begin
  // Create paint DC (for alpha blend)
  FCanvasDC := CreateCompatibleDC(0);
  // Recreate DIB section
  if (FHandle = 0) and (FWidth > 0) and (FHeight > 0) then begin
   FHandle := CreateDIBSection(FCanvasDC, FInfo, DIB_RGB_COLORS,
      pointer(FBits), 0, 0);
   if FHandle = 0 then begin
    // Error in DIB-section create
    ResetCanvas(true);
    exit;
   end;
  end;
  // Select DIB-section in FCanvasDC
  if FHandle <> 0 then
   FCanvasDCBitmap := SelectObject(FCanvasDC, FHandle);
 end;
 // set canvas bitmap completely transparent
 BitLine := FBits;
 for y:=0 to FHeight-1 do begin
  for x:=0 to FWidth-1 do cardinal(BitLine[x]) := FillColor;
  inc(integer(BitLine), FBytesPerLine);
 end;
end;

function TAlphaBuffer.BeginPaint(ASourceDC: HDC;
  const APaintRect: TRect): boolean;
begin
 Result := false;
 if (FSourceDC <> 0) or (ASourceDC = 0) then exit;
 with APaintRect do begin
  FPaintWidth := Right - Left;
  FPaintHeight := Bottom - Top;
 end;
 if (FPaintWidth <= 0) or (FPaintHeight <= 0) then exit;
 FPaintRect := APaintRect;
 // Check recreate DIB
 RecreateDIB(FPaintWidth, FPaintHeight);
 FSourceDC := ASourceDC;
 // Select FCanvasDC in FCanvas
 FCanvas.Handle := FCanvasDC;
 Result := true;
end;

function TAlphaBuffer.EndPaint(Transparency: integer): boolean;
var AlphaDC: HDC;
    AlphaDCBitmap: HBitmap;
    AlphaBitmap: HBitmap;
    AlphaBits: PAlphaLine;
    SrcLine: PAlphaLine;
    SrcPixel: PRGBQuad;
    SrcLineInc: integer;
    DestLine: PAlphaLine;
    DestLineInc: integer;
    SrcAlpha: integer;
    DstAlpha: integer;
    Tmp, A, C: integer;
    x, y: integer;
begin
 Result := false;
 AlphaDC := 0;
 AlphaDCBitmap := 0;
 AlphaBitmap := 0;
 try
  if (FSourceDC = 0) or (FCanvasDC = 0) or (FHandle = 0) or
     (Transparency < 0) then exit;
  if Transparency >= 100 then begin
   // Image completely transparent. Not necessary  to paint something
   Result := true;
   exit;
  end;
  if Assigned(FParent) and (FSourceDC = FParent.FCanvasDC) and
     EqualRect(FPaintRect, FParent.FPaintRect) then begin
   // Is child alpha buffer. No need to create temp alpha bitmap
   DestLine := FParent.FBits;
   DestLineInc := FParent.FBytesPerLine;
  end else begin
   // Create alpha DC
   AlphaDC := CreateCompatibleDC(0);
   if AlphaDC = 0 then exit;
   // Create alpha bitmap
   AlphaBitmap := CreateDIBSection(AlphaDC, FInfo, DIB_RGB_COLORS,
     pointer(AlphaBits), 0, 0);
   // Select alpha bitmap in alpha DC
   AlphaDCBitmap := SelectObject(AlphaDC, AlphaBitmap);
   // Copy current image from SourceDC to AlphaDC
   StretchBlt(AlphaDC, 0, 0, FPaintWidth, FPaintHeight, FSourceDC,
     FPaintRect.Left, FPaintRect.Top, FPaintWidth, FPaintHeight, SRCCOPY);
   // Set destination start
   DestLine := AlphaBits;
   DestLineInc := FBytesPerLine;
  end;
  // Alpha blend. Mix AlphaDC with FCanvasDC
  SrcLine := FBits;
  SrcLineInc := FBytesPerLine;
  for y:=0 to FPaintHeight-1 do begin
   for x:=0 to FPaintWidth-1 do with DestLine[x] do begin
    SrcPixel := @SrcLine[x];
    SrcAlpha := 255 - SrcPixel.rgbReserved;
    if SrcAlpha = 0 then continue;
    if Transparency > 0 then SrcAlpha := (100 - Transparency) * SrcAlpha div 100;
    DstAlpha := 255 - rgbReserved;
    if (SrcAlpha = 255) and (DstAlpha = 0) then
     DestLine[x] := SrcLine[x]
    else begin
     Tmp := (255 - SrcAlpha) * (255 - DstAlpha) + $80;
     A := 255 - ((Tmp + (Tmp shr 8)) shr 8);
     C := ((SrcAlpha shl 16) + (A shr 1)) div A;
     rgbRed := rgbRed +
       (((SrcPixel.rgbRed - rgbRed) * C + $8000) shr 16);
     rgbGreen := rgbGreen +
       (((SrcPixel.rgbGreen - rgbGreen) * C + $8000) shr 16);
     rgbBlue := rgbBlue +
       (((SrcPixel.rgbBlue - rgbBlue) * C + $8000) shr 16);
     rgbReserved := 255 - A;
    end;
   end;
   inc(integer(SrcLine), SrcLineInc);
   inc(integer(DestLine), DestLineInc);
  end;
  if AlphaDC <> 0 then
   // Draws alpha bitmap on the foreground
   BitBlt(FSourceDC, FPaintRect.Left, FPaintRect.Top, FPaintWidth, FPaintHeight,
     AlphaDC, 0, 0, SRCCOPY);
  // Alpha blend complete
  Result := true;
 finally
  // Delete alpha buffer
  if AlphaDC <> 0 then begin
   SelectObject(AlphaDC, AlphaDCBitmap);
   DeleteDC(AlphaDC);
   if AlphaBitmap <> 0 then DeleteObject(AlphaBitmap);
  end;
  // End paint
  FSourceDC := 0;
  ResetCanvas;
 end;
end;

function TAlphaBuffer.StoreFromGraphic(Graphic: TGraphic;
  OnDraw: TStoreDrawEvent = Nil): boolean;
begin
 Result := false;
 if (FSourceDC <> 0) then exit;
 if not Assigned(Graphic) then begin
  ResetCanvas(true);
  Result := true;
  exit;
 end;
 // Create DIB
 RecreateDIB(Graphic.Width, Graphic.Height, true);
 // Select FCanvasDC in FCanvas
 FCanvas.Handle := FCanvasDC;
 // Copy graphic to DIB
 if Assigned(OnDraw)
  then OnDraw(Self, FCanvas, Graphic)
  else FCanvas.Draw(0, 0, Graphic);
 // All done
 FStored := true;
 Result := true;
end;

function TAlphaBuffer.PaintStored(Dest: TAlphaBuffer;
  const APaintRect, ASrcRect: TRect): boolean;
var DestRect: TRect;
    Size, PaintSize, SrcSize: TPoint;
    XPoints: array of integer;
    x, y, YLine: integer;
    SrcLine, DestLine: PAlphaLine;
    XCoeff, YCoeff: double;
begin
 Result := false;
 if not FStored or (FWidth <= 0) or (FHeight <= 0) or
   (ASrcRect.Left < 0) or (ASrcRect.Top < 0) or
   (ASrcRect.Right > FWidth) or (ASrcRect.Bottom > FHeight) then exit;
 if not IntersectRect(DestRect, Rect(0, 0, Dest.Width, Dest.Height),
   APaintRect) then begin
  // Picture is completely invisible
  Result := true;
  exit;
 end;
 // Calculate size
 with Size, DestRect do begin
  X := Right - Left;
  Y := Bottom - Top;
 end;
 with PaintSize, APaintRect do begin
  X := Right - Left;
  Y := Bottom - Top;
 end;
 with SrcSize, ASrcRect do begin
  X := Right - Left;
  Y := Bottom - Top;
 end;
 // Calculate x points
 XCoeff := SrcSize.X / PaintSize.X;
 SetLength(XPoints, Size.X);
 for x:=0 to Size.X-1 do begin
  XPoints[x] := ASrcRect.Left +
    Trunc((x + (DestRect.Left - APaintRect.Left)) * XCoeff);
  if XPoints[x] < 0 then XPoints[x] := 0 else
  if XPoints[x] >= FWidth then XPoints[x] := FWidth;
 end;
 // Cycle for every visible scanline
 YCoeff := SrcSize.Y / PaintSize.Y;
 DestLine := PAlphaLine(PAnsiChar(Dest.FBits) + DestRect.Top * Dest.FBytesPerLine);
 for y:=DestRect.Top to DestRect.Bottom-1 do begin
  YLine := ASrcRect.Top + Trunc((y - APaintRect.Top) * YCoeff);
  if YLine >= FHeight then YLine := FHeight-1 else
  if YLine < 0 then YLine := 0;
  Srcline := PAlphaLine(PAnsiChar(FBits) + YLine * FBytesPerLine);
  for x:=0 to Size.X-1 do
   DestLine[x + DestRect.Left] := SrcLine[XPoints[x]];
  inc(PAnsiChar(DestLine), Dest.FBytesPerLine);
 end;
 // All ok
 Result := true;
end;

end.
