////////////////////////////////////////////
// График с таблицей, т.н. "Большое окно"
////////////////////////////////////////////
unit Frame.DiagramAndTable;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, ExtCtrls, GMGlobals, ParamsForDiagram, INIFiles,
  DiagramImage, Threads.GMClient, StdCtrls, GMDBClasses, GMConst, StrUtils,
  ConfigXml, Math, BigWindowData, dxStandardControlsSkinHelper,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  TFrmDiagramAndTable = class(TFrame)
    Panel1: TcxGroupBox;
    Splitter1: TcxSplitter;
    vstTable: TVirtualStringTree;
    imgDiagram: TDiagramImage;
    Timer1: TTimer;
    procedure vstTableHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure vstTableAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo;
      const Elements: THeaderPaintElements);
    procedure vstTableGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstTableGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure Timer1Timer(Sender: TObject);
    procedure imgDiagramMarkerMoved(Sender: TObject; MarkerPos: Integer;
      MarkerDT: Cardinal);
    procedure rbDiagramLenHrsClick(Sender: TObject);
    procedure vstTableHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstTableColumnResize(Sender: TVTHeader;
      Column: TColumnIndex);
  private
    FDiagramLenHrs: int;
    FActiveBW: TBigWindow;
    FClearArchivesOnRequest: bool;
    FTblTimeStep: int;

    procedure PrepareTableHeader;
    procedure AdjustColCount;
    procedure SetColumnNames;
    function GetNodeData(nd: PVirtualNode): PBWNodeData;
    procedure InitTableDates;
    procedure RepaintDiagram;
    function GetDiagramLenHrs(): int;
    procedure AdjustColWidth;

    procedure WMRequestDiagrams(var Msg: TMessage); message WM_REQUEST_DIAGRAMS;
    procedure WMRefreshObjectFrame(var Msg: TMessage); message WM_REFRESH_OBJECT_FRAME;
    procedure WMOrderReady(var Msg: TMessage); message WM_ORDER_READY;
    procedure SetDiagramLenHrs(const Value: int);
    function CalcLastDiagramUTime: LongWord;
    procedure CheckTableDates;
    procedure SetActiveBW(const Value: TBigWindow);
    procedure SetTblTimeStep(const Value: int);
    procedure ClearDiagrams(lDiagram: array of TValueFromBase);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckDiagramSize;
    property DiagramLenHrs: int read GetDiagramLenHrs write SetDiagramLenHrs;
    property ClearArchivesOnRequest: bool read FClearArchivesOnRequest write FClearArchivesOnRequest;
    property ActiveBW: TBigWindow read FActiveBW write SetActiveBW;
    property TblTimeStep: int read FTblTimeStep write SetTblTimeStep;
  end;

implementation

{$R *.dfm}

{ TFrmDiagramAndTable }

constructor TFrmDiagramAndTable.Create(AOwner: TComponent);
begin
  inherited;

  imgDiagram.DiagramColor := clWhite;
  imgDiagram.iDiagramTicks := 10;
  imgDiagram.iDiagramVrtTicks := 25;
  imgDiagram.bMarker := true;

  FDiagramLenHrs := 24;

  FTblTimeStep := 5 * 60; // шаг в таблице 5 минут
end;

destructor TFrmDiagramAndTable.Destroy;
begin
  inherited;
end;

procedure TFrmDiagramAndTable.ClearDiagrams(lDiagram: array of TValueFromBase);
var
  i: int;
  p: TParamForDraw;
  chn: TGMBaseParam;
begin
    chn := nil;
    for i := 0 to High(lDiagram) do
    begin
      p := FActiveBW.pfd.FindParam(TGMBaseParam(lDiagram[0].Chn));
      if (p.prm <> chn) and (p <> nil) then
      begin
        p.ClearDiagram();
        chn := p.prm;
      end;
    end;
end;

procedure TFrmDiagramAndTable.WMOrderReady(var Msg: TMessage);
var order: TDataOrder;
    i: int;
begin
  order := TDataOrder(Msg.WParam);
  try
    if FActiveBW = nil then
      Exit;

    if FClearArchivesOnRequest and (Length(order.lDiagram) > 0) then
      ClearDiagrams(order.lDiagram);

    for i := 0 to High(order.lDiagram) do
      FActiveBW.AddValue(order.lDiagram[i], false);

    InitTableDates();
    PostMessage(Handle, WM_REFRESH_OBJECT_FRAME, 0, 0);
  finally
    order.State := dosDelete;
  end;
end;

function TFrmDiagramAndTable.GetDiagramLenHrs(): int;
begin
  Result := FDiagramLenHrs;
end;

procedure TFrmDiagramAndTable.RepaintDiagram();
begin
  imgDiagram.PrepareDiagramBackground(int64(NowGM()) - GetDiagramLenHrs() * 3600, NowGM());

  if FActiveBW <> nil then
  begin
    FActiveBW.DiagramLenHrs := GetDiagramLenHrs();
    FActiveBW.DrawDiagram();
  end;

  imgDiagram.FinalizeDraw();
end;

procedure TFrmDiagramAndTable.WMRefreshObjectFrame(var Msg: TMessage);
begin
  RepaintDiagram();
end;

procedure TFrmDiagramAndTable.WMRequestDiagrams(var Msg: TMessage);
begin
  if FActiveBW <> nil then
  begin
    FActiveBW.pfd.ObjectName := FActiveBW.Caption;
    FActiveBW.pfd.RequestInitialDiagramsBunch(Handle, 1, 5);
  end;
end;

procedure TFrmDiagramAndTable.AdjustColCount();
var cnt: int;
begin
  if FActiveBW <> nil then
    cnt := FActiveBW.pfd.Count
  else
    cnt := 0;

  while vstTable.Header.Columns.Count < cnt + 1 do
    with vstTable.Header.Columns.Add() do
    begin
      Width := 150;
    end;

  while vstTable.Header.Columns.Count > cnt + 1 do
    vstTable.Header.Columns.Delete(vstTable.Header.Columns.Count - 1);
end;

procedure TFrmDiagramAndTable.SetColumnNames();
var i: int;
begin
  vstTable.Header.Columns[0].Text := 'Дата';

  if FActiveBW <> nil then
  begin
    for i := 0 to FActiveBW.pfd.Count - 1 do
    begin
      vstTable.Header.Columns[i + 1].Text := SplitToThreeLines(FActiveBW.pfd[i].Title);

      if (Trim(FActiveBW.pfd[i].Title) = '') and (FActiveBW.pfd[i].prm <> nil) then
      begin
        if Trim(FActiveBW.pfd[i].prm.PrmName) <> '' then
          vstTable.Header.Columns[i + 1].Text := SplitToThreeLines(FActiveBW.pfd[i].prm.PrmName)
        else
          vstTable.Header.Columns[i + 1].Text := FActiveBW.pfd[i].prm.GetNameWithPath([ppcObj, ppcDev, ppcPT], #13#10);
      end;
    end;
  end;
end;

procedure TFrmDiagramAndTable.SetDiagramLenHrs(const Value: int);
begin
  FDiagramLenHrs := Value;

  RepaintDiagram();
  InitTableDates();
end;

procedure TFrmDiagramAndTable.SetTblTimeStep(const Value: int);
begin
  FTblTimeStep := Value;

  RepaintDiagram();
  InitTableDates();
end;

procedure TFrmDiagramAndTable.AdjustColWidth();
var i: int;
begin
  try
    vstTable.OnColumnResize := nil;
    if FActiveBW <> nil then
    begin
      for i := 0 to FActiveBW.pfd.Count - 1 do
        if FActiveBW.pfd[i].ColWidth <= 0 then
        begin
          vstTable.Header.Columns[i + 1].Width := MultilineTextWidth(vstTable.Canvas, vstTable.Header.Columns[i + 1].Text) + 10;
          FActiveBW.pfd[i].ColWidth := vstTable.Header.Columns[i + 1].Width;
        end
        else
          vstTable.Header.Columns[i + 1].Width := FActiveBW.pfd[i].ColWidth;
    end;
  finally
    vstTable.OnColumnResize := vstTableColumnResize;
  end;
end;

procedure TFrmDiagramAndTable.PrepareTableHeader();
begin
  AdjustColCount();
  SetColumnNames();
  AdjustColWidth();
end;

procedure TFrmDiagramAndTable.SetActiveBW(const Value: TBigWindow);
begin
  if FActiveBW = Value then Exit;

  FActiveBW := Value;
  Value.pfd.ImgDiagram := imgDiagram;

  PrepareTableHeader();
  InitTableDates();
  PostMessage(Handle, WM_REFRESH_OBJECT_FRAME, 0, 0);
end;

procedure TFrmDiagramAndTable.vstTableHeaderDrawQueryElements(
  Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  var Elements: THeaderPaintElements);
begin
  Elements := [hpeText];
end;

procedure TFrmDiagramAndTable.vstTableAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var r: TRect;
    i: int;
    sl: TStringList;
begin
  if csDestroying in ComponentState then Exit;

  if hpeText in Elements then
  begin
    if PaintInfo.Column.Index > 0 then
    begin
      sl := TStringList.Create();
      sl.Text := PaintInfo.Column.Text;

      if FActiveBW.pfd[PaintInfo.Column.Index - 1].bDiagram then
        PaintInfo.TargetCanvas.Font.Color := clWindowText
      else
        PaintInfo.TargetCanvas.Font.Color := clInactiveCaption;

      for i := 0 to sl.Count - 1 do
      begin
        r := PaintInfo.PaintRectangle;
        r.Top :=  r.Top + i * 17 + 2;
        PaintInfo.TargetCanvas.TextRect(r, r.Left + 2, r.Top, sl[i]);
      end;
      sl.Free();

      if FActiveBW.pfd[PaintInfo.Column.Index - 1].bDiagram then
      begin
        FActiveBW.pfd[PaintInfo.Column.Index - 1].SetPenForDiagram(PaintInfo.TargetCanvas.Pen);
        PaintInfo.TargetCanvas.Pen.Width := 2;
        PaintInfo.TargetCanvas.Brush.Style := bsClear;
        PaintInfo.TargetCanvas.Rectangle(PaintInfo.PaintRectangle);
      end;

      PaintInfo.TargetCanvas.Brush.Style := bsSolid;
      PaintInfo.TargetCanvas.Font.Color := clWindowText
    end
    else
    begin
      r := PaintInfo.TextRectangle;
      PaintInfo.TargetCanvas.TextRect(r, r.Left, r.Top, PaintInfo.Column.Text);
    end;
  end;
end;

procedure TFrmDiagramAndTable.vstTableGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TBWNodeData);
end;

function TFrmDiagramAndTable.GetNodeData(nd: PVirtualNode): PBWNodeData;
begin
  if nd = nil then
    Result := nil
  else
    Result := PBWNodeData(vstTable.GetNodeData(nd));
end;

function TFrmDiagramAndTable.CalcLastDiagramUTime(): LongWord;
begin
  Result := (Min(imgDiagram.DEnd, NowGM()) div FTblTimeStep) * FTblTimeStep
end;

procedure TFrmDiagramAndTable.InitTableDates();
var ud1, ud2: Longword;
    i, n: int;
    nd: PVirtualNode;
begin
  if FActiveBW = nil then
  begin
    vstTable.Clear();
    Exit;
  end;

  vstTable.BeginUpdate();
  try
    ud1 := (int64(imgDiagram.DStart) div FTblTimeStep) * FTblTimeStep;
    ud2 := CalcLastDiagramUTime();

    n := int64(ud2 - ud1) div FTblTimeStep;
    vstTable.RootNodeCount := n;

    i := 0;
    nd := vstTable.RootNode.LastChild;
    while nd <> nil do
    begin
      GetNodeData(nd).UT1 := int64(ud1) + i * FTblTimeStep;
      GetNodeData(nd).UT2 := int64(ud1) + (i + 1) * FTblTimeStep;

      inc(i);
      nd := nd.PrevSibling;
    end;
  finally
    vstTable.EndUpdate();
  end;
end;

procedure TFrmDiagramAndTable.vstTableGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var vnd: PBWNodeData;
    v: double;
begin
  CellText := '';
  vnd := GetNodeData(Node);
  if vnd = nil then Exit;

  if Column = 0 then
  begin
    CellText := FormatDateTime('dd.mm hh:nn', UTCtoLocal(vnd.UT1));

     if vnd.UT2 - vnd.UT1 > 60 then // дописываем время окончания, только если интервал усреденения болье минуты 
       CellText := CellText + FormatDateTime(' - hh:nn', UTCtoLocal(vnd.UT2));
  end
  else
  if FActiveBW <> nil then
  begin
    if FActiveBW.pfd.Params[Column - 1].CalcAvg(vnd.UT1, vnd.UT2, v) then
      CellText := FormatFloatToShow(v, FActiveBW.pfd.Params[Column - 1].iDigitsAfterPoint);
  end;
end;

procedure TFrmDiagramAndTable.CheckDiagramSize;
begin  // любимый глюк фреймов с корявым Align
  if (Abs(Panel1.Width - imgDiagram.Width) > 4)
     or (Abs(Panel1.Height - imgDiagram.Height) > 4) then
  begin
    imgDiagram.Width := Panel1.Width - 2;
    imgDiagram.Height := Panel1.Height - 2;
    RepaintDiagram();
  end;
end;

procedure TFrmDiagramAndTable.CheckTableDates();
begin
  if int64(NowGM() - CalcLastDiagramUTime()) > FTblTimeStep then
  begin
    RepaintDiagram();
    InitTableDates();
  end;
end;

procedure TFrmDiagramAndTable.Timer1Timer(Sender: TObject);
begin
  CheckDiagramSize();
  CheckTableDates();
end;

procedure TFrmDiagramAndTable.imgDiagramMarkerMoved(Sender: TObject;
  MarkerPos: Integer; MarkerDT: Cardinal);
var nd: PVirtualNode;
    vnd: PBWNodeData;
begin
  RepaintDiagram();

  nd := vstTable.RootNode.FirstChild;
  if nd <> nil then
  begin
    try
      while nd <> nil do
      begin
        vnd := vstTable.GetNodeData(nd);
        if (vnd.UT1 <= MarkerDT) and (vnd.UT2 > MarkerDT) then
        begin
          vstTable.Selected[nd] := true;
          Exit;
        end;

        nd := nd.NextSibling;
      end;

      vnd := vstTable.GetNodeData(vstTable.RootNode.FirstChild);
      if vnd.UT1 > MarkerDT then
        vstTable.Selected[vstTable.RootNode.FirstChild] := true
      else
        vstTable.Selected[vstTable.RootNode.LastChild] := true;
    finally
      vstTable.ScrollIntoView(vstTable.GetFirstSelected(), true);
    end;
  end;
end;

procedure TFrmDiagramAndTable.rbDiagramLenHrsClick(Sender: TObject);
begin
  RepaintDiagram();
  InitTableDates();
end;

procedure TFrmDiagramAndTable.vstTableHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var xml: IGMConfigConfigType;
begin
  if HitInfo.Column > 0 then
  begin
    FActiveBW.pfd[HitInfo.Column - 1].bDiagram := not FActiveBW.pfd[HitInfo.Column - 1].bDiagram;

    xml := ReadConfigXML();
    xml.Bigwindows[BigWindowList.IndexOf(FActiveBW)].Diagrams[HitInfo.Column - 1].Diagram := IfThen(FActiveBW.pfd[HitInfo.Column - 1].bDiagram, 1, 0);
    SaveConfigXML(xml);

    RepaintDiagram();
    vstTable.Refresh();
  end;
end;

procedure TFrmDiagramAndTable.vstTableColumnResize(Sender: TVTHeader; Column: TColumnIndex);
var xml: IGMConfigConfigType;
begin
  if (FActiveBW <> nil) and (Column > 0) and (FActiveBW.pfd[Column - 1].ColWidth <> Sender.Columns[Column].Width) then
  begin
    xml := ReadConfigXML();
    xml.Bigwindows[BigWindowList.IndexOf(FActiveBW)].Diagrams[Column - 1].ColWidth := Sender.Columns[Column].Width;
    SaveConfigXML(xml);
  end;
end;

end.



