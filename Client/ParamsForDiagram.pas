////////////////////////////////////////////
// Класс для хранения графиков
////////////////////////////////////////////
unit ParamsForDiagram;

interface

uses Classes, Windows, GMGlobals, ExtCtrls, Graphics, Math, Threads.GMClient,
     SysUtils, GMDBClasses, DiagramImage, Controls, GMConst;

const DefEditWidth = 33;
      LEditVerticalStep = 27;

type
  TParamForDraw = class(TCollectionItem)
  private
    function GetDiagram: TDiagramImage;
  private
    FShowType: int;
    property ImgDiagram: TDiagramImage read GetDiagram;
    function GetLastVal: double;
    function GetDiagramCoeff: double;
    procedure SetShowType(const Value: int);
  public
    prm: TGMBaseParam;
    DiagramMin, DiagramMax: double;
    ColWidth: int;
    bDiagram: bool;
    Title: string;
    iDigitsAfterPoint: int;

    bMeander: bool;
    bActive: bool;

    PumpDown_ID_Prm: int;
    PumpDownSignal: double;
    PumpDownVal: TValueFromBase;

    lDiagram: array [0..3000] of TValueFromBase;
    fTrackBarFloating: double;

    pLEdit: TLabeledEdit;
    pImage: TImage;

    sPrefix, sPostfix: string;

    procedure SetControlsVisible(v: bool);
    property ShowType: int read FShowType write SetShowType;
    procedure SetCtrlLeft(Left: int);
    function ControlsWidth(iSpace: int): int;
    function CheckPump(): bool;
    function ControlsLeft(): int;
    property LastVal: double read GetLastVal;
    function LastValToString(): string;
    procedure ShowLastVal();
    procedure DrawPump(bmpPumpOff, bmpPumpOn, bmpPumpDown: TBitMap);
    procedure SetPenForDiagram(Pen: TPen);
    property DiagramCoeff: double read GetDiagramCoeff;
    procedure DrawDiagram(gmDStart: LongWord = 0; gmDEnd: LongWord = 0);
    constructor Create(Collection: TCollection); override;
    function CalcAvg(UT1, UT2: LongWord; var Summ: double): bool;
    procedure SaveToDebugFile(const fn: string);
    procedure InitControls(Parent: TWinControl; row: int);
    procedure ResizeControls(row: int);
    procedure ClearDiagram;
  end;

  TParamForDrawList = class (TCollection)
  private
    FObjectName: string;
    function GetParam(Index: int): TParamForDraw;
    function GetLastDT: LongWord;
    function GetFirstDT: LongWord;
    function GetAlarmMeanderParam: TParamForDraw;
  public
    ImgDiagram: TDiagramImage;

    constructor Create();

    function VisibleControlsCount: int;
    property LastDT: LongWord read GetLastDT;
    property FirstDT: LongWord read GetFirstDT;
    function Add(): TParamForDraw;
    property Params[Index: int]: TParamForDraw read GetParam; default;
    property AlarmMeander: TParamForDraw read GetAlarmMeanderParam;
    function CalcVisibleControlsTotalWidth(iSpace: int): int;
    function AddValue(val: TValueFromBase; bArchive: bool): bool;
    function CheckLastDataTimeout(): bool;
    procedure RequestInitialDiagrams(Handle: HWnd; nStep: int);
    procedure RequestInitialDiagramsBunch(Handle: HWnd; nStep: int; maxBunch: int = -1);
    procedure ClearDiagrams();
    function FindParam(Chn: TGMBaseParam): TParamForDraw;
    property ObjectName: string read FObjectName write FObjectName;
  end;

implementation

{ TParamsForDraw }

function TParamForDrawList.Add(): TParamForDraw;
begin
  Result := TParamForDraw(inherited Add());
end;

function TParamForDrawList.VisibleControlsCount(): int;
begin
  if Params[4].prm <> nil then Result := 4
  else if Params[3].prm <> nil then Result := 3
  else if Params[2].prm <> nil then Result := 2
  else if Params[1].prm <> nil then Result := 1
  else Result := 0;
end;

function TParamForDrawList.AddValue(val: TValueFromBase; bArchive: bool): bool;
var i, j, iStart: int;
    gmn: Int64;
    p: TParamForDraw;
    bFoundParams: bool;
begin
  bFoundParams := false;

  for i := 0 to Count - 1 do
  begin
    p := Params[i];
    if (p <> nil) and (p.prm <> nil) and p.prm.Equals(val.Chn) then
    begin
      // если прислали старую точку по текущим, то ничего не делаем
      if bArchive  then
      begin
        // архив, поставляется сверху вниз, так же и строится
        for j := High(p.lDiagram) downto 0 do
        begin
          if p.lDiagram[j].UTime = 0 then
          begin
            p.lDiagram[j] := val;
            break;
          end;
        end;
      end
      else
      if p.lDiagram[High(p.lDiagram)].UTime < val.UTime then
      begin
        bFoundParams := true;

        // сдвинем старый архив значений
        gmn := NowGM();
        for iStart := 0 to High(p.lDiagram) do
        begin
          // ищем первое показание текущих суток
          if gmn - p.lDiagram[iStart].UTime < 24*60*60 then
            break
          else
            p.lDiagram[iStart].UTime := 0;
        end;

        if iStart > 0 then dec(iStart);

        // первое показание, не вошедшее в интересующие сутки
        if iStart > 0 then dec(iStart);

        // сдвинем точки назад
        for j := iStart to High(p.lDiagram) - 1 do
          p.lDiagram[j] := p.lDiagram[j + 1];

        // установим новую точку
        p.lDiagram[High(p.lDiagram)] := val;
      end;
    end;

    if not bArchive
       and (p <> nil)
       and (GMParams.ByID(p.PumpDown_ID_Prm) <> nil)
       and (val.Chn is TGMParam)
       and (p.PumpDown_ID_Prm = TGMParam(val.Chn).ID_Prm)
       and (val.UTime > p.PumpDownVal.UTime) then
    begin
      p.PumpDownVal := val;
    end;
  end;

  Result :=  bFoundParams and not bArchive;
end;

function TParamForDrawList.CalcVisibleControlsTotalWidth(iSpace: int): int;
var nEdits: int;
begin
  // iSpace - расстояние между Label и Edit, а также отступ от края
  nEdits := VisibleControlsCount();

  Result :=0;

  // ширина всех объектов в сумме
  if nEdits >= 1 then Result :=          Params[1].ControlsWidth(iSpace);
  if nEdits >= 2 then Result := Result + Params[2].ControlsWidth(iSpace);
  if nEdits >= 3 then Result := Result + Params[3].ControlsWidth(iSpace);
  if nEdits >= 4 then Result := Result + Params[4].ControlsWidth(iSpace);
end;

constructor TParamForDrawList.Create;
begin
  inherited Create(TParamForDraw);
end;

function TParamForDrawList.FindParam(Chn: TGMBaseParam): TParamForDraw;
var i: int;
begin
  Result := nil;
  if Chn = nil then Exit;
  
  for i := 0 to Count - 1 do
  begin
    if (Params[i].prm <> nil) and Params[i].prm.Equals(Chn) then
    begin
      Result := Params[i];
      Exit;
    end;
  end;
end;

function TParamForDrawList.GetAlarmMeanderParam: TParamForDraw;
var i: int;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Params[i].bMeander then
    begin
      Result := Params[5];
      break;
    end;
end;

function TParamForDrawList.GetLastDT: LongWord;
var i: int;
begin
  Result:=0;

  for i := 0 to Count - 1 do
  if Params[i] <> AlarmMeander then
    Result := max(Result, Params[i].lDiagram[high(Params[i].lDiagram)].UTime);
end;

function TParamForDrawList.GetFirstDT: LongWord;
var i, j: int;
begin
  Result := 0;

  for i := 0 to Count - 1 do
  if Params[i] <> AlarmMeander then
  begin
    for j := 0 to high(Params[i].lDiagram) do
      if Params[i].lDiagram[j].UTime > 0 then
      begin
        if Result = 0 then
          Result := Params[i].lDiagram[j].UTime
        else
          Result := min(Result, Params[i].lDiagram[j].UTime);
        break; // даты отсортированы, дальше можно не смотреть
      end;
  end;
end;

function TParamForDrawList.GetParam(Index: int): TParamForDraw;
begin
  Result:=TParamForDraw(Items[Index]);
end;

function TParamForDrawList.CheckLastDataTimeout: bool;
var i: int;
    ut: LongWord;
begin
  Result := true;
  try
  for i := 0 to Count - 1 do
    if not Params[i].bMeander then
    begin
      ut := Params[i].lDiagram[High(Params[i].lDiagram)].UTime;
      if Abs(Int64(NowGM()) + TGMCOMThreadPool.ServerTimeDiffSec - ut) <= iNoRefreshAlarm * 60 then
        Result := false; // данных есть
    end;
  except end;
end;

procedure TParamForDrawList.ClearDiagrams;
var i: int;
begin
  for i := 0 to Count - 1 do
    Params[i].ClearDiagram();
end;

procedure TParamForDrawList.RequestInitialDiagrams(Handle: HWnd; nStep: int);
begin
  RequestInitialDiagramsBunch(Handle, nStep, 1);
end;

procedure TParamForDrawList.RequestInitialDiagramsBunch(Handle: HWnd; nStep: int; maxBunch: int = -1);
var
  i, n: int;
  order: TDataOrder;
  ut1: LongWord;
begin
  n := 0;
  order := nil; // make compiler happy
  DataThreadSynch.BeginWrite();
  try
    for i := 0 to Count - 1 do
      if Params[i].prm <> nil then
      begin
        TGMCOMThreadPool.AddReqCurrentsPrm(Params[i].prm);

        if Params[i].bDiagram then
        begin
          if n = 0 then
          begin
            order := TDataOrders.AddDiagram(nil, 0, 0, nStep, Handle);
            order.req.Comment := ObjectName;
          end;

          ut1 := Params[i].lDiagram[High(Params[i].lDiagram)].UTime;
          ut1 := Max(ut1, NowGM() - 24 * 3600);
          order.AddDiagramRequestDataSource(Params[i].prm, ut1, NowGM(), nStep);
          inc(n);
        end;

        if (maxBunch > 0) and (n >= maxBunch) then
        begin
          order.State := dosWaiting;
          n := 0;
        end;
      end;

    if n > 0 then
      order.State := dosWaiting;
  except end;
  DataThreadSynch.EndWrite();
end;

{ TParamForDraw }

function TParamForDraw.CalcAvg(UT1, UT2: LongWord; var Summ: double): bool;
var i, n: int;
    bMTR: bool;
begin
  Result := false;
  Summ := 0;
  n := 0;
  bMTR := (prm <> nil) and prm.IsMeterNI;
  for i := 0 to High(lDiagram) do
  begin
    if (lDiagram[i].UTime >= UT1) and (lDiagram[i].UTime < UT2) then
    begin
      if bMTR then
      begin
        if n = 0 then
          Summ := lDiagram[i].Val
        else
          Summ := Max(Summ, lDiagram[i].Val)
      end
      else
      begin
        Summ := Summ + lDiagram[i].Val;
      end;

      inc(n);
    end
    else
    if lDiagram[i].UTime >= UT2 then
      break;
  end;

  if n > 0 then
  begin
    Result := true;
    if not bMTR then
      Summ := Summ / n;
  end;
end;

function TParamForDraw.CheckPump: bool;
begin
  Result := false;
  if prm <> nil then
    Result := (GetLastVal() - DiagramMin) > 0.2 * (DiagramMax - DiagramMin);
end;

function TParamForDraw.ControlsLeft: int;
begin
  Result:=pLEdit.EditLabel.Left;
end;

function TParamForDraw.ControlsWidth(iSpace: int): int;
begin
  pLEdit.LabelSpacing:=iSpace;
  Result:=iSpace;
  Result:=Result+pLEdit.Width+pLEdit.EditLabel.Width;

  if ShowType=1 then
    Result:=Result+pImage.Width;
end;

procedure TParamForDraw.ResizeControls(row: int);
begin
  if pLEdit <> nil then
  begin
    pLEdit.Name := 'le' + IntToStr(row) + IntToStr(Index);
    pLEdit.Top := pLEdit.Parent.Height - LEditVerticalStep * (row + 1);
    pLEdit.Left := Index * 50;
    pLEdit.Width := DefEditWidth;

    pLEdit.EditLabel.Font.Name := 'Times New Roman';
    pLEdit.EditLabel.Font.Size := 12;

    pLEdit.AutoSize := false;
    pLEdit.Font.Name := 'Times New Roman';
    pLEdit.Font.Size := 14;
    pLEdit.Height := 23;

    pLEdit.EditLabel.Caption := '';
    pLEdit.LabelPosition := lpLeft;
    pLEdit.Visible := false;
    pLEdit.Text := '';
  end;

  if pImage <> nil then
  begin
    pImage.Name := 'img' + IntToStr(row) + IntToStr(Index);
    pImage.Top := pImage.Parent.Height - LEditVerticalStep * (row + 1) + 2;
    pImage.Left := Index * 50;
    pImage.Width := 30;
    pImage.Height := 19;
    pImage.AutoSize := true;
    pImage.Visible := false;
  end;
end;

procedure TParamForDraw.InitControls(Parent: TWinControl; row: int);
begin
  pLEdit := TLabeledEdit.Create(Parent);
  pLEdit.Parent := Parent;
  pLEdit.ReadOnly := True;

  pImage := TImage.Create(Parent);
  pImage.Parent := Parent;

  ResizeControls(row);
end;

procedure TParamForDraw.ClearDiagram();
var i: int;
begin
  for i:=0 to High(lDiagram) do
  begin
    lDiagram[i].UTime:=0;
    lDiagram[i].Val:=0;
  end;
end;

constructor TParamForDraw.Create(Collection: TCollection);
begin
  inherited;

  ColWidth := 0;
  bMeander := false;
  bActive := true;

  prm := nil;
  ClearDiagram();
end;

procedure TParamForDraw.DrawDiagram(gmDStart: LongWord = 0; gmDEnd: LongWord = 0);
var cnv: TCanvas;
    x0, y0, x1, y1, h, w, j: int;
begin
  if not bActive then Exit;

  if (gmDStart = 0) or (gmDEnd = 0) then
  begin
    gmDEnd:=NowGM();
    gmDStart:=gmDEnd-iDiagramHrs*3600;
  end;

  cnv := imgDiagram.Picture.Bitmap.Canvas;

  w := imgDiagram.Width - imgDiagram.RightOffset; // с небольшим отступом от края
  h := imgDiagram.Height;

  SetPenForDiagram(cnv.Pen);

  if (prm <> nil) and bDiagram and (DiagramMax - DiagramMin > 0) and (gmDEnd - gmDStart > 0) then
  begin
    x0 := 0;
    y0 := 0;
    //рисуем график
    for j := High(lDiagram) downto 0 do
    begin
      if lDiagram[j].UTime <= 0 then break;
      
      if Abs(lDiagram[j].Val) < MaxInt/2 then
      begin
        x1 := Round((w * (int64(lDiagram[j].UTime) - gmDStart)) / (gmDEnd - gmDStart));
        if (x1 <= 0) and (x0 <=0) then break; // прекращаем рисование, когда оба вылетели за экран
        y1 := h - Floor(h * (lDiagram[j].Val - DiagramMin) / (DiagramMax - DiagramMin));
        if j < High(lDiagram) then
        begin
          cnv.MoveTo(x0, y0);
          cnv.LineTo(x1, y1);
        end;

        x0 := x1;
        y0 := y1;
      end;
    end;
  end;
end;

procedure TParamForDraw.DrawPump(bmpPumpOff, bmpPumpOn, bmpPumpDown: TBitMap);
var bPump, bPumpDown: bool;
begin
  if ShowType <> 1 then Exit;

  bPump := CheckPump();
  bPumpDown := (PumpDown_ID_Prm > 0)
               and (PumpDownVal.UTime > 0)
               and (PumpDownVal.Val = PumpDownSignal);

  pImage.Transparent:=true;
  if bPumpDown then
    pImage.Picture.Bitmap.Assign(bmpPumpDown)
  else
  if bPump then
    pImage.Picture.Bitmap.Assign(bmpPumpOn)
  else
    pImage.Picture.Bitmap.Assign(bmpPumpOff);

  pImage.Picture.Bitmap.TransparentMode := tmAuto;
  pImage.Picture.Bitmap.Transparent := true;
  pImage.Transparent:=true;
end;

function TParamForDraw.GetDiagram: TDiagramImage;
begin
  Result := TParamForDrawList(Collection).ImgDiagram;
end;

function TParamForDraw.GetDiagramCoeff: double;
begin
  if DiagramMax - DiagramMin > 0.01 then
    Result := (LastVal - DiagramMin) / (DiagramMax - DiagramMin)
  else
    Result:=1;
end;

function TParamForDraw.GetLastVal: double;
begin
  Result:=lDiagram[High(lDiagram)].Val;
end;

function TParamForDraw.LastValToString: string;
begin
  Result := FormatFloatToShow(LastVal, iDigitsAfterPoint);
end;

procedure TParamForDraw.SaveToDebugFile(const fn: string);
var i, n: int;
    s: string;
begin
  for n := 0 to High(lDiagram) do
    if lDiagram[n].UTime > 0 then break;

  if n > 0 then dec(n);
  s := '';

  for i := n to High(lDiagram) do
    s := s + IntToStr(lDiagram[i].UTime) + #9 + DateTimeToStr(UTCToLocal(lDiagram[i].UTime)) + #13#10;

  SaveStringToFile(s, fn);
end;

procedure TParamForDraw.SetControlsVisible(v: bool);
begin
  if not v then
  begin
    if pLEdit<>nil then pLEdit.Visible:=false;
    if pImage<>nil then pImage.Visible:=false;
  end
  else
  begin
    if pLEdit<>nil then pLEdit.Visible:=true;
    if pImage<>nil then pImage.Visible:=(ShowType=1);
  end;
end;

procedure TParamForDraw.SetCtrlLeft(Left: int);
begin
  pLEdit.Left:=Left+pLEdit.LabelSpacing+pLEdit.EditLabel.Width;
  pImage.Left:=pLEdit.Left+pLEdit.Width+pLEdit.LabelSpacing;
end;

procedure TParamForDraw.SetPenForDiagram(Pen: TPen);
begin
  case Index mod 6 of
    0: Pen.Color:=clGreen;
    1: Pen.Color:=clPurple;
    2: Pen.Color:=clNavy;
    3: Pen.Color:=clAqua;
    4: Pen.Color:=clFuchsia;
    5: Pen.Color:=clRed;
  end;

  Pen.Width := 2;
  Pen.Style := psSolid;
end;

procedure TParamForDraw.SetShowType(const Value: int);
begin
  FShowType := Value;
end;

procedure TParamForDraw.ShowLastVal;
var s: string;
    w: int;
    sz: SIZE;
    h: HDC;
begin
  s := LastValToString();

  if Length(s) >= 4 then
  begin
    w := 0;
    h := GetWindowDC(pLEdit.Handle);
    if h <> 0 then
    begin
      GetTextExtentPoint32(h, PChar(s), Length(s), sz);
      w := sz.cx + 8;
      ReleaseDC(pLEdit.Handle, h);
    end;

    pLEdit.ClientWidth := Max(DefEditWidth, w);
  end
  else
    pLEdit.ClientWidth := DefEditWidth;

  pLEdit.Text := s;
end;

end.
