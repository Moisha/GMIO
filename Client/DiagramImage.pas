////////////////////////////////////////////
// Заготовка под график
////////////////////////////////////////////
unit DiagramImage;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, Graphics, GMGlobals, Windows, Math;

type
  TOnMarkerMoved = procedure (Sender: TObject; MarkerPos: Int; MarkerDT: LongWord) of object;

  TDiagramImage = class(TImage)
  private
    { Private declarations }
    FOnMarkerMoved: TOnMarkerMoved;
    function GeMarkerDT: LongWord;
    function GetRightOffset: int;
  published
    { Published declarations }
    property OnMarkerMoved: TOnMarkerMoved read FOnMarkerMoved write FOnMarkerMoved;
  protected
    { Protected declarations }
    MarkerPos: int;
    FDStart, FDEnd: LongWord;

    procedure Click; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawMarker();
  public
    { Public declarations }
    DiagramColor: TColor;
    iDiagramTicks: int;
    iDiagramVrtTicks: int;
    bMarker: bool;

    property RightOffset: int read GetRightOffset;
    property DStart: LongWord read FDStart;
    property DEnd: LongWord read FDEnd;
    procedure PrepareDiagramBackground(AgmDStart: LongWord = 0; AgmDEnd: LongWord = 0);
    procedure PrintLastRefreshTime(bNodata: bool; LastDT: TDateTime);
    procedure StretchBitmap();
    procedure FinalizeDraw();
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TDiagramImage]);
end;

procedure TDiagramImage.StretchBitmap;
var h, w: int;
begin
  w := Width;
  Picture.Bitmap.Width := w;
  h := Height;
  Picture.Bitmap.Height := h;
end;

procedure TDiagramImage.PrepareDiagramBackground(AgmDStart: LongWord = 0; AgmDEnd: LongWord = 0);
var cnv: TCanvas;
    xTick: Int64;
    x0, y0, h, w: int;
begin
  if (AgmDStart = 0) or (AgmDEnd = 0) then
  begin
    FDEnd := NowGM();
    FDStart := FDEnd - iDiagramHrs*3600;
  end
  else
  begin
    FDEnd := AgmDEnd;
    FDStart := AgmDStart;
  end;

  StretchBitmap();
  
  cnv := Picture.Bitmap.Canvas;

  w := Width;
  h := Height;

  // фон
  cnv.Brush.Color := DiagramColor;
  cnv.FillRect(Rect(0, 0, w, h));

  // риски по времени
  // iDiagramTicks - промежуток в минутах
  if (iDiagramTicks > 0) and (FDEnd > FDStart) then
  begin
    cnv.Pen.Color := clBlack;
    cnv.Pen.Width := 1;
    xTick := (int64(FDStart) div iDiagramTicks) * iDiagramTicks;
    while xTick <= FDEnd do
    begin
      x0 := Round((xTick - FDStart) / (FDEnd - FDStart) * w);
      cnv.MoveTo(x0, h);
      cnv.LineTo(x0, h - 4);

      xTick := xTick + iDiagramTicks * 60;
    end;
  end;

  // риски по горизонтали
  // iDiagramVrtTicks - промежуток в процентах
  if iDiagramVrtTicks > 0 then
  begin
    cnv.Pen.Color := RGB(180, 180, 180);
    cnv.Pen.Width := 1;
    cnv.Pen.Style := psSolid;
    xTick := iDiagramVrtTicks;
    while xTick < 100 do
    begin
      y0 := Round(xTick / 100 * h);
      cnv.MoveTo(1, y0);
      cnv.LineTo(w - 1, y0);

      xTick := xTick + iDiagramVrtTicks;
    end;
  end;

  // рамка
  cnv.Brush.Color:=clBlack;
  cnv.FrameRect(Rect(0, 0, w, h));
end;

procedure TDiagramImage.PrintLastRefreshTime(bNodata: bool; LastDT: TDateTime);
begin
  // последнее время опроса
  Picture.Bitmap.Canvas.Font.Color := clBlack;
  Picture.Bitmap.Canvas.Brush.Color := IfThen(bNoData, clRed, DiagramColor);
  Picture.Bitmap.Canvas.TextOut(4, 2, FormatDateTime('dd.mm hh:nn', LastDT));
end;

procedure TDiagramImage.Click;
begin
  inherited;
end;

procedure TDiagramImage.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

function TDiagramImage.GeMarkerDT(): LongWord;
var w: int;
begin
  w := Width - RightOffset;
  if FDStart < FDEnd then
    Result := FDStart + Round((MarkerPos / w) * (FDEnd - FDStart))
  else
    Result := FDStart;
end;

procedure TDiagramImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if bMarker then
  begin
    MarkerPos := X;
    DrawMarker();
    if Assigned(FOnMarkerMoved) then
      FOnMarkerMoved(self, MarkerPos, GeMarkerDT());
  end;

  inherited;
end;

constructor TDiagramImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  bMarker := false;
  MarkerPos := 0;
end;

procedure TDiagramImage.DrawMarker;
begin
  if bMarker and (MarkerPos >= 0) and (MarkerPos < Width) then
  begin
    Picture.Bitmap.Canvas.Pen.Color := clGreen;
    Picture.Bitmap.Canvas.Pen.Width := 1;
    Picture.Bitmap.Canvas.MoveTo(MarkerPos, 0);
    Picture.Bitmap.Canvas.LineTo(MarkerPos, Height);
  end;
end;

procedure TDiagramImage.FinalizeDraw;
begin
  DrawMarker();
end;

function TDiagramImage.GetRightOffset: int;
begin
  Result := Round(Min(Width / 50.0, 10));
end;

end.
