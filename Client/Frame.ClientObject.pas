////////////////////////////////////////////
// Обеъкт основного окна
////////////////////////////////////////////
unit Frame.ClientObject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GMGlobals, IniFiles, Math, ComCtrls, Threads.GMClient,
  ImgList, ParamsForDiagram, DiagramImage, GMDBClasses, GMConst, ConfigXML,
  GMGenerics, Frame.CurrentsDiagramBase,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver, cxTrackBar;

type
  TFrmObject = class(TCurrentsDiagramBaseFrame)
    gbFrame: TcxGroupBox;
    ilPump: TImageList;
    imgDiagram: TDiagramImage;
    tbBar: TcxTrackBar;
    procedure tbBarChange(Sender: TObject);
    procedure imgDiagramMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgDiagramClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure tbBarPropertiesGetPositionHint(Sender: TObject; const APosition: Integer; var AHintText: string; var ACanShow,
      AIsHintMultiLine: Boolean);
  private
    { Private declarations }
    FID_Obj: int; // ID_Obj из базы
    InitialWidth, InitialHeight: int;
    bNoData: bool;
    bmpPumpOn, bmpPumpOff, bmpPumpDown: TBitmap;

    procedure SetNColor(const Value: int);
    procedure CheckBarAlarmThreshold(pfdParams: TParamForDrawList);
    procedure CheckLastData;
    procedure WMRefreshObjectFrame(var Msg: TMessage); message WM_REFRESH_OBJECT_FRAME;
    procedure WMOrderReady(var Msg: TMessage); message WM_ORDER_READY;
    procedure WMCheckLastData(var Msg: TMessage); message WM_CHECK_LAST_DATA;
    procedure WMRequestDiagrams(var Msg: TMessage); message WM_REQUEST_DIAGRAMS;
    procedure CheckAlarmsDiagram;
    procedure SetTransparent(bmp: TBitMap);
    procedure PlaceLEdits(pfdParams: TParamForDrawList);
    procedure DrawParam(p: TParamForDraw);
    function CheckID_Obj(prm: TGMBaseParam): bool;
    procedure AddMeanderPrm(pfdParams: TParamForDrawList);
    procedure ReadCommon;
    procedure ReadObjectCommon(XmlNode: IGMConfigObjectType);
    procedure ReadChannel(pfdParams: TParamForDrawList; channel: IGMConfigChannelType; row: int; bAddEditBox: bool);
    function MakeTransparentBitmap(Index: int): TBitmap;
    function AddOneEditLine(): TParamForDrawList;
    function LastDT: LongWord;
    function RowForParamsLine(n: int): int;
    function TrackBarValue(position: int): double;
  public
    { Public declarations }
    ParamsForDrawRows: TGMCollection<TParamForDrawList>;

    property NoData: bool read bNoData;
    procedure ReadINI(XmlNode: IGMConfigObjectType);
    property NColor: int write SetNColor;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ID_Obj: int read FID_Obj;
    procedure AddValue(val: TValueFromBase; bArchive: bool);
  end;

implementation

{$R *.dfm}

uses AppConfigFile, dxStandardControlsSkinHelper, EsLogging;

type FrameColorAndBitmaps = record
  color: TColor;
  bmpPumpOn, bmpPumpOff, bmpPumpDown: TBitmap;
end;

// синий, зеленый, коричневый
var FrameColors: array [1..3] of FrameColorAndBitmaps;

procedure TFrmObject.AddValue(val: TValueFromBase; bArchive: bool);
var pfdParams: TParamForDrawList;
begin
  for pfdParams in ParamsForDrawRows do
  begin
    if pfdParams.AddValue(val, bArchive) then
      CheckLastData(); // форма отрефрешится там же
  end;
end;

function TFrmObject.AddOneEditLine(): TParamForDrawList;
begin
  Result := ParamsForDrawRows.Add();
  Result.ImgDiagram := imgDiagram;
end;

constructor TFrmObject.Create(AOwner: TComponent);
begin
  inherited;

  ParamsForDrawRows := TGMCollection<TParamForDrawList>.Create();

  InitialWidth := Width;
  InitialHeight := Height;

  imgDiagram.DiagramColor := GetSkinFormContentColor();
  bNoData := true;
  imgDiagram.iDiagramVrtTicks := 25;
end;

procedure TFrmObject.AddMeanderPrm(pfdParams: TParamForDrawList);
var LabelP: TParamForDraw;
begin
  // 5й параметр для меандра тревог
  LabelP := pfdParams.Add();
  LabelP.bMeander := true;
  LabelP.bDiagram := true;
  LabelP.DiagramMin := 0.5;
  LabelP.DiagramMax := 1.1;
end;

procedure TFrmObject.ReadCommon();
var xml: IGMConfigConfigType;
begin
  xml := ReadConfigXML();
  iDiagramHrs := xml.Common.Diagram_time;
end;

procedure TFrmObject.ReadObjectCommon(XmlNode: IGMConfigObjectType);
begin
  gbFrame.Caption := XmlNode.Name;
  NColor := XmlNode.Color; // f.ReadInteger(sect, 'COLOR', 1);
  Width := Round( InitialWidth * XmlNode.Squeeze / 100.0 );
  Height := Round( InitialHeight * XmlNode.SqueezeY / 100.0 );
  FID_Obj := XmlNode.Id_obj;
  imgDiagram.iDiagramTicks := XmlNode.Ticks;
end;

procedure TFrmObject.ReadChannel(pfdParams: TParamForDrawList; channel: IGMConfigChannelType; row: int; bAddEditBox: bool);
var ID_Prm: int;
    LabelP: TParamForDraw;
    AchtungP: TGMAlarm;
begin
  LabelP := pfdParams.Add();
  if bAddEditBox then
    LabelP.InitControls(gbFrame, row);

  if channel <> nil then
  begin
    LabelP.sPrefix := channel.Prefix;
    LabelP.sPostfix := channel.Postfix;
    LabelP.ShowType := channel.Showtype;
    LabelP.fTrackBarFloating := MyStrToFloatDef(channel.Barfloat);

    ID_Prm := channel.Id_prm;
    if ID_Prm > 0 then
    begin
      LabelP.prm := ParamOrNodeChannel(channel.DataChnType, ID_Prm);
      LabelP.bDiagram := channel.Diagram > 0;

      LabelP.DiagramMin := channel.Dmin;
      LabelP.DiagramMax := channel.Dmax;
      LabelP.iDigitsAfterPoint := channel.Digits;

      LabelP.PumpDown_ID_Prm := channel.Pump_prm;
      LabelP.PumpDownSignal := channel.Pump_signal;

      if not bAddEditBox and (LabelP.prm <> nil) then // Алярм для бара
      begin
        AchtungP := acAlarms.Add();
        AchtungP.prm := LabelP.prm;

        case channel.Showtype of
          BAR_ALARM_TYPE_DOWN: AchtungP.AlarmType := atpBarThresholdDown;
          BAR_ALARM_TYPE_HYSTERESIS:
            begin
              AchtungP.AlarmType := atpBarHisteresis;
              AchtungP.fBarHisteresis := MyStrToFloatDef(channel.Barfloat);
            end;
          else AchtungP.AlarmType := atpBarThresholdUp;
        end;
      end;
    end;
  end;
end;

procedure TFrmObject.ReadINI(XmlNode: IGMConfigObjectType);
var i, j, linecount, row: int;
    pfdParams: TParamForDrawList;
begin
  try
    ReadCommon();
    ReadObjectCommon(XmlNode);

    linecount := max(XmlNode.Tabs.Count, 1);
    for i := 0 to linecount - 1 do
    begin
      pfdParams := AddOneEditLine();

      row := linecount - i - 1;
      for j := 0 to NominalEditBoxOnLineCount - 1 do
        ReadChannel(pfdParams, XmlNode.Tabs[i].Channels.ChannelByNum[j], row, j > 0);

      AddMeanderPrm(pfdParams);
      CheckBarAlarmThreshold(pfdParams);
    end;
  except
    on e: Exception do
    begin
      ShowMessageBox('Ошибка чтения файла ' + GMMainConfigFile.GetMainINIFileName() + #13#10 + e.Message, MB_ICONSTOP);
      Application.Terminate();
    end;
  end;
end;

procedure TFrmObject.SetTransparent(bmp: TBitMap);
var k, j: int;
    clr: TColor;
begin
  // сам себе транспарент
  clr := bmp.Canvas.Pixels[0, 0];
  for k := 0 to bmp.Width do
    for j := 0 to bmp.Height do
    if bmp.Canvas.Pixels[k, j] = clr then
      bmp.Canvas.Pixels[k, j] := Color;
end;

function TFrmObject.MakeTransparentBitmap(Index: int): TBitmap;
begin
  Result := TBitmap.Create();
  ilPump.GetBitmap(Index, Result);
  SetTransparent(Result);
end;

procedure TFrmObject.SetNColor(const Value: int);
var n: int;
begin
  if Value in [Low(FrameColors)..High(FrameColors)] then
    n := Value
  else
    n := Low(FrameColors); // раз неясный цвет, то берем первый попавшийся

  Color := FrameColors[n].color;
  gbFrame.Style.Color := Color;

  if FrameColors[n].bmpPumpOff = nil then
    FrameColors[n].bmpPumpOff := MakeTransparentBitmap(0);

  if FrameColors[n].bmpPumpOn = nil then
    FrameColors[n].bmpPumpOn := MakeTransparentBitmap(1);

  if FrameColors[n].bmpPumpDown = nil then
    FrameColors[n].bmpPumpDown := MakeTransparentBitmap(2);

  bmpPumpOff := FrameColors[n].bmpPumpOff;
  bmpPumpOn := FrameColors[n].bmpPumpOn;
  bmpPumpDown := FrameColors[n].bmpPumpDown;
end;

procedure TFrmObject.WMRequestDiagrams(var Msg: TMessage);
var pfdParams: TParamForDrawList;
begin
  for pfdParams in ParamsForDrawRows do
    pfdParams.RequestInitialDiagrams(Handle, 5);
end;

function TFrmObject.CheckID_Obj(prm: TGMBaseParam): bool;
var pfdParams: TParamForDrawList;
    i: TCollectionItem;
    p: TParamForDraw;
begin
  Result := false;
  if (prm = nil) or not (prm is TGMParam) then Exit;

  for pfdParams in ParamsForDrawRows do
  begin
    for i in pfdParams do
    begin
      p := TParamForDraw(i);
      if (p.prm <> nil)
         and (p.prm is TGMParam)
         and (TGMParam(p.prm).Device.Obj.ID_Obj = TGMParam(prm).Device.Obj.ID_Obj) then
      begin
        Result := true;
        Exit;
      end;
    end;
  end;
end;

procedure TFrmObject.CheckAlarmsDiagram();
var i: int;
    bAlarm: bool;
    pfdParams: TParamForDrawList;
begin
  bAlarm := false;
  for i := 0 to acAlarms.Count - 1 do
  begin
    if CheckID_Obj(acAlarms[i].prm)
        and (acAlarms[i].alCurrState in [asSetTo1, as1AndQuoted]) then
    begin
      bAlarm := true;
      break;
    end;
  end;

  for pfdParams in ParamsForDrawRows do
  begin
    if pfdParams.AlarmMeander <> nil then
    begin
      // если в графике пусто, то поставим изначальный 0
      if pfdParams.AlarmMeander.lDiagram[High(pfdParams.AlarmMeander.lDiagram)].UTime = 0 then
        pfdParams.AlarmMeander.lDiagram[High(pfdParams.AlarmMeander.lDiagram)].UTime := NowGM() - 1;

      // сдвинем точки назад
      for i := 0 to High(pfdParams.AlarmMeander.lDiagram) - 1 do
        pfdParams.AlarmMeander.lDiagram[i] := pfdParams.AlarmMeander.lDiagram[i + 1];

      pfdParams.AlarmMeander.lDiagram[High(pfdParams.AlarmMeander.lDiagram)].Val := IfThen(bAlarm, 1, 0);
      pfdParams.AlarmMeander.lDiagram[High(pfdParams.AlarmMeander.lDiagram)].UTime := NowGM();
    end;
  end;
end;

procedure TFrmObject.PlaceLEdits(pfdParams: TParamForDrawList);
var cWidth, iSpace, wSpace: int;
begin
  // расстояние между Label и Edit, а также отступ от края
  iSpace := 4;

  // общая ширина
  cWidth := pfdParams.CalcVisibleControlsTotalWidth(iSpace);

  // остатки места
  wSpace := Max(gbFrame.Width - iSpace * 2 - cWidth, 0);

  case pfdParams.VisibleControlsCount() of

    1: begin // посередине
         pfdParams[1].SetCtrlLeft(iSpace + wSpace div 2);
       end;

    2: begin // на три части, едиты с лабелами как границы
         pfdParams[1].SetCtrlLeft(iSpace + wSpace div 3);
         pfdParams[2].SetCtrlLeft(pfdParams[1].ControlsLeft()
                                     + pfdParams[1].ControlsWidth(iSpace)
                                     + wSpace div 3);
       end;

    3: begin // первый и третий по углам, второй посередине
         pfdParams[1].SetCtrlLeft(iSpace);
         pfdParams[2].SetCtrlLeft(pfdParams[1].ControlsLeft()
                                  + pfdParams[1].ControlsWidth(iSpace)
                                  + wSpace div 2);
         pfdParams[3].SetCtrlLeft(pfdParams[2].ControlsLeft()
                                  + pfdParams[2].ControlsWidth(iSpace)
                                  + wSpace div 2);
       end;

    4: begin // первый и четвертый по углам, второй и третий равномерно
         pfdParams[1].SetCtrlLeft(iSpace);

         pfdParams[2].SetCtrlLeft(pfdParams[1].ControlsLeft()
                                  + pfdParams[1].ControlsWidth(iSpace)
                                  + wSpace div 3);

         pfdParams[3].SetCtrlLeft(pfdParams[2].ControlsLeft()
                                  + pfdParams[2].ControlsWidth(iSpace)
                                  + wSpace div 3);

         pfdParams[4].SetCtrlLeft(pfdParams[3].ControlsLeft()
                                  + pfdParams[3].ControlsWidth(iSpace)
                                  + wSpace div 3);
       end;
  end;
end;

procedure TFrmObject.DrawParam(p: TParamForDraw);
begin
  if (p.prm = nil) then
    p.SetControlsVisible(false)
  else
  if p.pLEdit <> nil then
  begin
    p.SetControlsVisible(true);
    p.pLEdit.EditLabel.Caption:=p.sPrefix;

    // корявко в данных
    if Abs(p.LastVal) < MaxInt / 2 then
    begin
      p.ShowLastVal();
      p.DrawPump(bmpPumpOff, bmpPumpOn, bmpPumpDown);
    end;
  end;

  p.DrawDiagram();
end;

function TFrmObject.LastDT(): LongWord;
var pfdParams: TParamForDrawList;
begin
  Result := 0;
  for pfdParams in ParamsForDrawRows do
    Result := Max(Result, pfdParams.LastDT);
end;

procedure TFrmObject.WMRefreshObjectFrame(var Msg: TMessage);
var pfdParams: TParamForDrawList;
    p: TParamForDraw;
    i: int;
begin
  try
    CheckAlarmsDiagram();

    tbBar.Visible := ParamsForDrawRows[0][0].prm <> nil;
    if tbBar.Visible then
    begin
      p:=ParamsForDrawRows[0][0];

      if Abs(p.DiagramMax - p.DiagramMin) > 0.01 then
      begin
        tbBar.Properties.SelectionStart := 100 - Round((p.LastVal - p.DiagramMin) / (p.DiagramMax - p.DiagramMin) * 100);
        tbBar.Properties.SelectionEnd := 100;
      end;
    end;

    imgDiagram.PrepareDiagramBackground();
    imgDiagram.PrintLastRefreshTime(bNoData, UTCToLocal(LastDT()));

    for pfdParams in ParamsForDrawRows do
    begin
      // Нарисовать линии
      for i := 0 to pfdParams.Count - 1 do
        DrawParam(pfdParams[i]);

      // распихать едиты ровно
      PlaceLEdits(pfdParams);
    end;

    imgDiagram.FinalizeDraw();

  except end;
end;

procedure TFrmObject.tbBarChange(Sender: TObject);
var pfdParams: TParamForDrawList;
begin
  for pfdParams in ParamsForDrawRows do
    CheckBarAlarmThreshold(pfdParams);
end;

procedure TFrmObject.tbBarPropertiesGetPositionHint(Sender: TObject; const APosition: Integer; var AHintText: string; var ACanShow,
  AIsHintMultiLine: Boolean);
begin
  AHintText := MyFloatToStr(RoundVal(TrackBarValue(APosition), 1));
  ACanShow := true;
  AIsHintMultiLine := false;
end;

procedure TFrmObject.CheckLastData();
var pfdParams: TParamForDrawList;
begin
  bNoData := true;
  for pfdParams in ParamsForDrawRows do
    bNoData := bNoData and pfdParams.CheckLastDataTimeout();

  PostMessage(Handle, WM_REFRESH_OBJECT_FRAME, 0, 0);
end;

procedure TFrmObject.WMCheckLastData(var Msg: TMessage);
begin
  CheckLastData();
end;

procedure TFrmObject.WMOrderReady(var Msg: TMessage);
var order: TDataOrder;
    i: int;
begin
  order := TDataOrder(Msg.WParam);

  for i := High(order.lDiagram) downto 0 do
    AddValue(order.lDiagram[i], true);

  order.State := dosDelete;
  PostMessage(Handle, WM_REFRESH_OBJECT_FRAME, 0, 0);
end;

function TFrmObject.TrackBarValue(position: int): double;
begin
  Result := ParamsForDrawRows[0][0].DiagramMin + (100 - position) * (ParamsForDrawRows[0][0].DiagramMax - ParamsForDrawRows[0][0].DiagramMin) / 100;
end;

procedure TFrmObject.CheckBarAlarmThreshold(pfdParams: TParamForDrawList);
var f: double;
    i: int;
begin
  f := TrackBarValue(tbBar.Position);

  for i := 0 to acAlarms.Count - 1 do
  if (pfdParams[0].prm is TGMParam)
     and (acAlarms[i].ID_Prm = pfdParams[0].prm.ID_Prm)
     and (acAlarms[i].AlarmType in [atpBarThresholdUp, atpBarThresholdDown, atpBarHisteresis]) then
  begin
    if acAlarms[i].AlarmType = atpBarThresholdDown then
      acAlarms[i].Enabled := tbBar.Position < tbBar.Properties.Max
    else
      acAlarms[i].Enabled := tbBar.Position > 0;
      
    acAlarms[i].fBarTreshold := f;
    Exit;
  end;
end;

destructor TFrmObject.Destroy;
begin
  ParamsForDrawRows.Free();

  inherited;
end;

procedure TFrmObject.imgDiagramMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if GMAggregates.IsObjectControllable(FID_Obj) then
    imgDiagram.Cursor := crHandPoint
  else
    imgDiagram.Cursor := crDefault;
end;

procedure TFrmObject.imgDiagramClick(Sender: TObject);
begin
  GMPostMessage(WM_SHOW_CONTROL_PANEL, FID_Obj, 1, DefaultLogger);
end;

procedure FinalizeModule();
var i: int;
begin
  for i := Low(FrameColors) to High(FrameColors) do
  begin
    FrameColors[i].bmpPumpOn.Free();
    FrameColors[i].bmpPumpOff.Free();
    FrameColors[i].bmpPumpDown.Free();
  end;
end;

function TFrmObject.RowForParamsLine(n: int): int;
begin
  Result := ParamsForDrawRows.Count - n - 1;
end;

procedure TFrmObject.FrameResize(Sender: TObject);
var i, j, diagramHeight: int;
begin
  for i := 0 to ParamsForDrawRows.Count - 1 do
  begin
    for j := 0 to ParamsForDrawRows[i].Count - 1 do
      ParamsForDrawRows[i][j].ResizeControls(RowForParamsLine(i));
  end;

  diagramHeight := gbFrame.Height - imgDiagram.Top - LEditVerticalStep * (ParamsForDrawRows.Count) - 4;
  imgDiagram.Height := max(diagramHeight, 0);
  imgDiagram.Width := Max(gbFrame.Width - imgDiagram.Left - 4, 0);
  tbBar.Height := Height - 24;
end;

initialization
  FrameColors[1].color := $00F8E6D6;
  FrameColors[2].color := $00BDD7C8;
  FrameColors[3].color := $00BBD0DF;

finalization
  FinalizeModule();
end.



