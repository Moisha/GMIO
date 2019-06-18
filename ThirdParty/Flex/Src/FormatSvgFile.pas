/////////////////////////////////////////////////////////
//                                                     //
//    FlexGraphics library                             //
//    Copyright (c) 2002-2009, FlexGraphics software.  //
//                                                     //
//    FlexGraphics library file formats support        //
//    Scalable Vector Graphics (svg) files             //
//                                                     //
/////////////////////////////////////////////////////////

unit FormatSvgFile;

{$I FlexDefs.inc}

interface

uses
  Windows, Classes, Graphics, FlexBase, FlexFileFormats;

resourcestring
  sSvgFileDescription = 'Scalable Vector Graphics';

type
  TFlexSvgFormat = class(TFlexFileFormat)
  protected
    procedure RegisterSupportedExtensions; override;
    procedure ExportSVG(AFlexPanel: TFlexPanel; AStream: TStream);
  public
    procedure ExportToStream(AStream: TStream; AFlexPanel: TFlexPanel;
      const Extension: TFlexFileExtension; const AFileName: string); override;
  end;

implementation

uses
  SysUtils, StdCtrls, FlexProps, FlexPath, FlexControls, FlexUtils;

function FormatCoord(Coord: Integer): string;
var Module: integer;
begin
  Result := IntToStr(Coord div PixelScaleFactor);
  Module := Coord mod PixelScaleFactor;
  if Module <> 0 then Result := Result + '.' + IntToStr(Module);
end;

function SVGColor(AColor: TColor; Braces: boolean = True): string;
begin
  AColor:=ColorToRGB(AColor);
  case AColor of
    clBlack: Result := 'black';
    clWhite: Result := 'white';
    clRed: Result := 'red';
    clGreen: Result := 'green';
    clBlue: Result := 'blue';
    clYellow: Result := 'yellow';
    clGray: Result := 'gray';
    clNavy: Result := 'navy';
    clOlive: Result := 'olive';
    clLime: Result := 'lime';
    clTeal:  Result := 'teal';
    clSilver:  Result := 'silver';
    clPurple:  Result := 'purple';
    clFuchsia:  Result := 'fuchsia';
    clMaroon:  Result := 'maroon';
    else
      Result :=  'rgb(' +
        IntToStr(GetRValue(AColor)) + ',' +
        IntToStr(GetGValue(AColor)) + ',' +
        IntToStr(GetBValue(AColor)) + ')';
  end;
  if Braces then result := '"' + result + '"';
end;

function PointToStr(X, Y: Integer): string;
begin
  Result := FormatCoord(X) + ',' + FormatCoord(Y);
end;

function SVGPoints(const Left, Top: integer; const Points: TPointArray): string;
var t: Integer;
begin
  Result := ' points="';
  for t:=Low(Points) to High(Points) do
    Result := Result +
      PointToStr((Left + Points[t].X), (Top + Points[t].Y)) + ' ';
  Result := Result + '"';
end;

function SVGPen(Pen: TPenProp; Braces: boolean = true): string;
var bc: string;

  function PenStyle: string;
  begin
    case Pen.Style of
      psDash       : Result := '4, 2';
      psDot        : Result := '2, 2';
      psDashDot    : Result := '4, 2, 2, 2';
      psDashDotDot : Result := '4, 2, 2, 2, 2, 2';
      else           Result := '';
    end;
  end;

begin
  if Braces
    then bc := '"'
    else bc := '';
  if Pen.Style = psClear then
    Result := ' stroke=' + bc + 'none' + bc
  else begin
    Result := ' stroke=' + SVGColor(Pen.Color, Braces);
    if Pen.Width > 1 then
      Result := Result + ' stroke-width='+ bc + FormatCoord(Pen.Width) + bc;
    if Pen.Style <> psSolid then
      Result := Result + ' stroke-dasharray=' + bc + PenStyle + bc + ' ';  // fill="none" breaks brush ??
  end;
end;

function SVGBrushPen(Brush: TBrushProp; Pen: TPenProp = Nil;
  Brushes: TStrings = Nil): string;
var
  Control: TFlexControl;
  Index: integer;
begin
  if Assigned(Brushes) then begin
    // Try to find brush style in list
    Control := Brush.Owner.Owner as TFlexControl;
    Index := Brushes.IndexOfObject(pointer(Control.IdProp.Value));
    if Index >= 0 then begin
      Result := 'fill:url(#' + Brushes[Index] +')';
      if Result <> '' then Result := Format(' style="%s"', [Result]);
    end;
  end else
    Index := -1;
  if Index < 0 then
    if Brush.Style <> bsClear then begin
      Result := ' fill=' + SVGColor(Brush.Color);
      if Brush.Color = clNone then
        Result := Result + ' fill-opacity="0"';
    end else
      Result := ' fill="none"';
  if Assigned(Pen) then Result := Result + SVGPen(Pen);
end;

function SVGFont(Font: TFontProp): string;

  function GetFontName(const aName: string): string;
  begin
    if aName = '宋体' then
      Result := 'SimSun'
    else if aName = '黑体' then
      Result := 'SimHei'
    else if aName = '仿宋_GB2312' then
      Result := 'FangSong_GB2312'
    else if aName = '楷体_GB2312' then
      Result := 'KaiTi_GB2312'
    else if aName = '幼圆' then
      Result := 'YouYuan'
    else if aName = '华文宋体' then
      Result := 'STSong'
    else if aName = '华文中宋' then
      Result := 'STZhongsong'
    else if aName = '华文楷体' then
      Result := 'STKaiti'
    else if aName = '华文仿宋'  then
      Result := 'STFangsong'
    else if aName = '华文细黑' then
      Result := 'STXihei'
    else if aName = '华文隶书' then
      Result := 'STLiti'
    else if aName = '华文行楷' then
      Result := 'STXingkai'
    else if aName = '华文新魏' then
      Result := 'STXinwei'
    else if aName = '华文琥珀' then
      Result := 'STHupo'
    else if aName = '华文彩云' then
      Result := 'STCaiyun'
    else if aName = '方正姚体简体' then
      Result := 'FZYaoTi'
    else if aName = '方正舒体简体' then
      Result := 'FZShuTi'
    else if aName = '新宋体' then
      Result := 'NSimSun'
    else if aName = '隶书' then
      Result := 'LiSu'
    else
      Result := aName;
  end;
  
begin
  Result :=
    ' font-family="' + GetFontName(Font.Name) +
    '" font-size="' + FormatCoord(Abs(Font.Size)) + 'pt" ';

  if fsItalic in Font.Style then
    Result := Result + ' font-style="italic"';

  if fsBold in Font.Style then
    Result := Result + ' font-weight="bold"';

  if fsUnderline in Font.Style then
    Result := Result + ' text-decoration="underline"'
  else
  if fsStrikeOut in Font.Style then
    Result := Result + ' text-decoration="line-through"';

  Result := Result + ' fill=' + SVGColor(Font.Color);
end;

function GetTextWidth(aFont: TFontProp; const aText: string): integer;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Canvas.Font.Size := aFont.Size div 1000;
    Bmp.Canvas.Font.Name := aFont.Name;
    Bmp.Canvas.Font.Style := aFont.Style;
    Bmp.Canvas.Font.Charset := aFont.Charset;
    Result := Bmp.Canvas.TextWidth(aText);
  finally
    Bmp.Free;
  end;
end;

function GetTextHeight(aFont: TFontProp; const aText: string): integer;
var
  Bmp: TBitmap;
  TM: TTextMetric;  
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Canvas.Font.Size := aFont.Size div 1000;
    Bmp.Canvas.Font.Name := aFont.Name;
    Bmp.Canvas.Font.Style := aFont.Style;
    Bmp.Canvas.Font.Charset := aFont.Charset;
    Result := Bmp.Canvas.TextHeight(aText);
    if GetTextMetrics(Bmp.Canvas.Handle, TM) then
      dec(Result, TM.tmDescent); // Move to baseline
  finally
    Bmp.Free;
  end;
end;

// TFlexSvgFormat /////////////////////////////////////////////////////////////

procedure TFlexSvgFormat.RegisterSupportedExtensions;
begin
  RegisterExtension('svg', sSvgFileDescription, [skExport]);
end;

procedure TFlexSvgFormat.ExportToStream(AStream: TStream;
  AFlexPanel: TFlexPanel; const Extension: TFlexFileExtension;
  const AFileName: string);
begin
  if CompareText(Extension.Extension, 'svg') <> 0 then begin
    inherited;
    exit;
  end;
  ExportSVG(AFlexPanel, AStream);
end;

procedure TFlexSvgFormat.ExportSVG;
var
  List: TStrings;
  str,LStr, StyleName: string;
  Control: TFlexControl;
  PassRec: TPassControlRec;
  PO: TPaintOrder;
  LayerIdx, ControlIdx: integer;
  i,x,y,w,h,r: integer;
  CurvePoints: TPointArray;
  Prop: TCustomProp;
  Brushes: TStringList;
  StartColor, EndColor: TColor;
begin
  Brushes := Nil;
  List := TStringList.Create;
  try
    List.Add('<?xml version="1.0" encoding="UTF-8"?>');
    List.Add('<svg width="' + IntToStr(AFlexPanel.DocWidth div 1000) +
      '" height="' + IntToStr(AFlexPanel.DocHeight div 1000) + '">');
    // SVG defs section
    List.Add('<defs>');
    try
      Brushes := TStringList.Create;
      Control := AFlexPanel.ActiveScheme;
      FirstControl(Control, PassRec);
      while Assigned(Control) do begin
        // Check brush
        Prop := Control.Props['Brush'];
        if Assigned(Prop) and (Prop is TBrushProp) then
          with TBrushProp(Prop) do
            if not IsClear and IsPaintAlternate then begin
              // Try add brush style
              if (Method = bmGradient) and
                 (Control.IdProp.Value <> 0) then begin
                // Create gradient brush
                if GradStyle = gsElliptic
                  then LStr := 'radialGradient'
                  else LStr := 'linearGradient';
                StyleName :=
                  Control.NameProp.Value +
                  IntToStr(Control.IdProp.Value) +
                  '_Brush';
                str :=
                  'id="' + StyleName + '" gradientUnits="objectBoundingBox" ';
                case GradStyle of
                  gsHorizontal  : str := str + 'x1="0" y1="0" x2="1" y2="0"';
                  gsVertical    : str := str + 'x1="0" y1="0" x2="0" y2="1"';
                  gsSquare      : str := str + 'x1="0" y1="0" x2="0" y2="0"'; // NOT IMPLEMENTED
                  gsElliptic    : str := str + 'cx="0.5" cy="0.5" r="0.5" fx="0.5" fy="0.5"';
                  gsTopLeft     : str := str + 'x1="0" y1="0" x2="1" y2="1"';
                  gsTopRight    : str := str + 'x1="1" y1="0" x2="0" y2="1"';
                  gsBottomLeft  : str := str + 'x1="0" y1="1" x2="1" y2="0"';
                  gsBottomRight : str := str + 'x1="1" y1="1" x2="0" y2="0"';
                end;
                List.Add('<' + LStr + ' ' + str + '>');
                if GradStyle = gsElliptic then begin
                  EndColor := GradBeginColor;
                  StartColor := GradEndColor;
                end else begin
                  StartColor := GradBeginColor;
                  EndColor := GradEndColor;
                end;
                List.Add('<stop offset="0%" style="stop-color:' +
                  SVGColor(StartColor,false) + '; stop-opacity:1"/>');
                List.Add('<stop offset="100%" style="stop-color:' +
                  SVGColor(EndColor,false) + '; stop-opacity:1"/>');
                List.Add('</' + LStr + '>');
                Brushes.AddObject(StyleName, pointer(Control.IdProp.Value));
              end;
            end;
        // Next control
        Control := NextControl(PassRec);
      end;
      ClosePassRec(PassRec);
    finally
      List.Add('</defs>');
    end;
    // Scheme background
    List.Add(
      '<rect x="0" y="0" width="'  + IntToStr(AFlexPanel.DocWidth div 1000) +
      '" height="'  + IntToStr(AFlexPanel.DocHeight div 1000) + '"' +
      SVGBrushPen(TFlexScheme(AFlexPanel.ActiveScheme).BrushProp, Nil, Brushes) +
      //'" fill=' + SVGColor(TFlexScheme(Flex.ActiveScheme).BrushProp.Color) +
      //' stroke=' + SVGColor(TFlexScheme(Flex.ActiveScheme).BrushProp.Color) +
      ' stroke="none" stroke-width="0" />');
    // SVG graphics section
    InitPaintOrder(PO);
    try
      AFlexPanel.ActiveScheme.CreatePaintOrder(PO);
      for LayerIdx:=0 to High(PO.LayerRefs) do begin
        ControlIdx := PO.LayerRefs[LayerIdx].First;
        while ControlIdx >= 0 do begin
          Control := PO.ControlRefs[ControlIdx].Control;
          FirstControl(Control, PassRec);
          while Assigned(Control) do begin
            if Control is TFlexBox then with TFlexBox(Control) do begin
              str :=
                '<rect x = "' + IntToStr(DocRect.Left div 1000) +
                '" y="' + IntToStr(DocRect.Top div 1000) +
                '" width="' + IntToStr(Width div 1000) +
                '" height="' + IntToStr(Height div 1000) + '" ' +
                SVGBrushPen(BrushProp, PenProp, Brushes);
              if RoundnessProp.Value > 0 then begin
                r := RoundnessProp.Value div (2*PixelScaleFactor);
                str := Format('%s rx="%d" ry="%d"', [str, r, r]);
              end;
              str := str + '/>';
              List.Add(str);
              if Control is TFlexText then with TFlexText(Control) do begin
                x := DocRect.Left div 1000;
                y := DocRect.Bottom div 1000;
                w := Width div 1000 - GetTextWidth(FontProp, TextProp.Text);
                h := Height div 1000 - GetTextHeight(FontProp, ' ');
                case Alignment of
                  taRightJustify : x := x + w;
                  taCenter       : x := x + w div 2;
                end;
                case Layout of
                  tlTop    : y := y - h;
                  tlCenter : y := y - h div 2;
                end;
                str :=
                  '<text x="' + IntToStr(x) + '" y="' + IntToStr(y) + '"' +
                  SVGFont(FontProp) + '> ' + TextProp.Text + ' </text>';
                List.Add(str);
              end;
            end else
            if Control is TFlexEllipse then with TFlexEllipse(Control) do begin
              str :=
                '<ellipse cx = "' +
                IntToStr((DocRect.Right - Width div 2) div 1000) +
                '" cy="' + IntToStr((DocRect.Bottom - Height div 2) div 1000) +
                '" rx="' + IntToStr(Width div 2000) +
                '" ry="' + IntToStr(Height div 2000) +  '" ' +
                SVGBrushPen(BrushProp, PenProp, Brushes) + '/>';
              List.Add(str);
            end else
            if Control is TFlexCurve then with TFlexCurve(Control) do begin
              SetLength(CurvePoints, PointCount);
              for i := 0 to PointCount-1 do
                CurvePoints[i] := Points[i];
              if IsSolidProp.Value then
                Str := '<polygon ' + SVGBrushPen(BrushProp, PenProp, Brushes)
              else
                Str := '<polyline fill="none" marker-start="url(#arrow)"' +
                  SVGPen(PenProp);
              str :=
                str + SVGPoints(DocRect.Left, DocRect.Top, CurvePoints) + '/>';
              List.Add(str);
            end else
            if Control is TFlexPicture then with TFlexPicture(Control) do begin
              // TODO: Not implemented
            end;
            Control := NextControl(PassRec);
          end;
          ClosePassRec(PassRec);
          ControlIdx := PO.ControlRefs[ControlIdx].Next;
        end;
      end;
    finally
      ClearPaintOrder(PO);
    end;
    List.Add('</svg>');
    {$IFNDEF FG_D12}
    List.Text := AnsiToUtf8(List.Text);
    List.SaveToStream(AStream);
    {$ELSE}
    List.SaveToStream(AStream, TEncoding.UTF8);
    {$ENDIF}
  finally
    List.Free;
    Brushes.Free;
  end;
end;

initialization
  RegisteredFlexFileFormats.RegisterFormat(TFlexSvgFormat);

end.
