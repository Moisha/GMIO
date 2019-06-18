////////////////////////////////////////////
// ќтчет гидрогеолога
////////////////////////////////////////////
unit HydroGeoRep;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Frame.ReportBlank, GMGlobals, VirtualTrees, GMConst, GMDBClasses,
  Threads.GMClient, ActiveX, ConfigXml,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  THydroDayData = class(TCollectionItem)
  private
    function LookForValue(dgrm: array of TValueFromBase;
      UTime: LongWord): TValueFromBase;
    function CalcSumFromMidnight(dgrm: array of TValueFromBase;
      UTime: LongWord): TValueFromBase;
    function CalcLdivVFromMidnight(UTime: LongWord): TValueFromBase;
  public
    UDay: LongWord;
    lDiagramL: array [0..47] of TValueFromBase;
    lDiagramV: array [0..47] of TValueFromBase;
    Lmin: TValueFromBase;
    Lmax: TValueFromBase;
    Vmonth: TValueFromBase;

    procedure AfterConstruction; override;
    procedure ProcessDay(sh: Variant; nStartRow: int);
  end;

  THydroDayDataCollection = class(TCollection)
  private
    function GetDay(n: int): THydroDayData;
  public
    property Days[n: int]: THydroDayData read GetDay; default;
    function Add(): THydroDayData;
    function GetDayByDT(UDT: LongWord; bCreate: bool = true): THydroDayData;
  end;

  THydroGeoRepDlg = class(TGMEnhancedScrollForm)
    frmRep: TFrmReportBlank;
    vstHydroGeoRep: TVirtualStringTree;
    procedure frmRepbtGoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vstHydroGeoRepGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vstHydroGeoRepGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
  private
    { Private declarations }
    cDays: THydroDayDataCollection;
    FirstDT, LastDT: LongWord;
    prmL, prmV: TGMParam;
    lstObj: TList;
    xml: IGMConfigConfigType;
    procedure ProcessReport();
    procedure AddSelectedObjecs(nd: PVirtualNode; CycleObject: pointer);
    procedure AddDataOrder(prm: TGMParam; UDT1, UDT2: LongWord; DiagramType: TGMDiagramType);
    procedure AddSQLOrder(prm: TGMParam; UDT1, UDT2: LongWord);
    procedure AddOrders();
    procedure InitReport;
    procedure ProcessOrders;
    procedure CommitReport;
    function LookForParams(nReport: int): bool;
    procedure ParseDataOrder(order: TDataOrder);
    procedure ParseSQLOrder(order: TDataOrder);
    procedure CreateXLS(nReport: int);
    procedure XLSHeader(sh: Variant; FirstRow: int);
  public
    { Public declarations }
    function ShowModal: TModalResult; reintroduce;
    procedure AfterConstruction; override;
  end;

var
  HydroGeoRepDlg: THydroGeoRepDlg;

implementation

uses Math, ComObj, DateUtils, StrUtils, VT_Utils;

{$R *.dfm}

{ THydroDayData }

function THydroDayDataCollection.Add: THydroDayData;
begin
  Result := THydroDayData(inherited Add());
end;

function THydroDayDataCollection.GetDay(n: int): THydroDayData;
begin
  Result := THydroDayData(Items[n]);
end;

function THydroDayDataCollection.GetDayByDT(UDT: LongWord; bCreate: bool = true): THydroDayData;
var i: int;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    if Days[i].UDay = UDT then
    begin
      Result := Days[i];
      Exit;
    end;
  end;

  if bCreate then
  begin
    Result := Add();
    Result.UDay := UDT;
  end;
end;

{ THydroDayData }

procedure THydroDayData.AfterConstruction;
var i: int;
begin
  inherited;

  for i := 0 to High(lDiagramL) do
    lDiagramL[i].UTime := 0;

  for i := 0 to High(lDiagramV) do
    lDiagramV[i].UTime := 0;

  Lmin.UTime := 0;
  Lmax.UTime := 0;

  Vmonth.Val := 0;
  Vmonth.UTime := 0;
end;

function THydroDayData.LookForValue(dgrm: array of TValueFromBase; UTime: LongWord): TValueFromBase;
var i: int;
begin
  Result.UTime := 0;

  for i := 0 to High(dgrm) do
    if dgrm[i].UTime = UTime then
    begin
      Result := dgrm[i];
      Exit;
    end;
end;

function THydroDayData.CalcSumFromMidnight(dgrm: array of TValueFromBase; UTime: LongWord): TValueFromBase;
var i: int;
begin
  Result.UTime := 0;
  Result.Val := 0;

  for i := 0 to High(dgrm) do
    if (dgrm[i].UTime > 0) and (dgrm[i].UTime <= UTime) then
    begin
      Result.UTime := Result.UTime + 1;
      Result.Val := Result.Val + dgrm[i].Val;
    end;
end;

function THydroDayData.CalcLdivVFromMidnight(UTime: LongWord): TValueFromBase;
var i: int;
    v: TValueFromBase;
begin
  Result.UTime := 0;
  Result.Val := 0;

  for i := 0 to High(lDiagramL) do
    if (lDiagramL[i].UTime > 0) and (lDiagramL[i].UTime <= UTime) then
    begin
      v := LookForValue(lDiagramV, lDiagramL[i].UTime);
      if (v.UTime > 0) and (CompareValue(v.Val, 0, 1e-6) <> 0) then
      begin
        Result.UTime := Result.UTime + 1;
        Result.Val := Result.Val + lDiagramL[i].Val / v.Val;
      end;
    end;

  if Result.UTime > 0 then
    Result.Val := result.Val / Result.UTime;
end;

procedure THydroDayData.ProcessDay(sh: Variant; nStartRow: int);
var dt: TDateTime;
    udt: LongWord;
    row, LastRow: int;
    l, v, sumV, avgLdivV: TValueFromBase;
begin
  dt := UTCtoLocal(UDay);
  udt := UDay;
  LastRow := nStartRow + 47;
  for row := nStartRow to LastRow do
  begin
    dt := dt + 30 * OneMinute;

    l := LookForValue(lDiagramL, udt);
    v := LookForValue(lDiagramV, udt);
    sumV := CalcSumFromMidnight(lDiagramV, udt);
    avgLdivV := CalcLdivVFromMidnight(udt);

    sh.Cells[row, 1] := FormatDateTime('mmmm yyyy', dt - IfThen(row = LastRow, 1));
    sh.Cells[row, 2] := DayOf(dt - IfThen(row = LastRow, 1));
    sh.Cells[row, 3] := '''' + IfThen(row = LastRow, '23:59', FormatDateTime('hh:nn', dt));
    if l.UTime > 0 then sh.Cells[row, 4] := RoundVal(l.Val, 2); // '”ровень'
    if v.UTime > 0 then  sh.Cells[row, 5] := RoundVal(v.Val, 2); // 'V/30 мин';
    if (v.UTime > 0) and (sumV.UTime > 0) then sh.Cells[row, 6] := RoundVal(sumV.Val, 2); // 'V с нач. сут';
    if (v.UTime > 0) and (l.UTime > 0) and (CompareValue(v.Val, 0, 1e-6) <> 0) then sh.Cells[row, 7] := RoundVal(l.Val / v.Val, 2); // 'L/V ср.';
    if row = LastRow then
    begin
      if avgLdivV.UTime > 0 then sh.Cells[row, 8] := RoundVal(avgLdivV.Val, 2) ; //'L/V ср. сут.';
      if Lmax.UTime > 0 then
      begin
        sh.Cells[row, 9] := RoundVal(Lmax.Val, 2); // 'L max';
        sh.Cells[row, 10] := '''' + FormatDateTime('hh:nn', UTCtoLocal(Lmax.UTime)); //'¬рем€ L max';
      end;
      if Lmin.UTime > 0 then
      begin
        sh.Cells[row, 11] := RoundVal(Lmin.Val, 2); // 'L min';
        sh.Cells[row, 12] := '''' + FormatDateTime('hh:nn', UTCtoLocal(Lmin.UTime));; // '¬рем€ L min';
      end;
    end;
    if (v.UTime > 0) and (sumV.UTime > 0) then sh.Cells[row, 13] := RoundVal(Vmonth.Val + sumV.Val, 2); // 'V с нач. мес.';

    udt := udt + 30 * UTC_MINUTE;
  end;
end;

{ THydroGeoRepDlg }

function THydroGeoRepDlg.ShowModal: TModalResult;
var nd: PVirtualNode;
begin
  frmRep.HidePeriod();
  frmRep.frmTree.bHighLightObjects := false;
  frmRep.frmTree.Visible := false;

  xml := ReadConfigXML();
  vstHydroGeoRep.RootNodeCount := xml.Hydrogeoreps.Count;

  nd := vstHydroGeoRep.RootNode.FirstChild;
  while nd <> nil do
  begin
    vstHydroGeoRep.CheckType[nd] := ctCheckBox;
    nd := nd.NextSibling;
  end;

  Result := inherited ShowModal();

  xml := nil;
end;

procedure THydroGeoRepDlg.AddDataOrder(prm: TGMParam; UDT1, UDT2: LongWord; DiagramType: TGMDiagramType);
var order: TDataOrder;
begin
  order := frmRep.AddOrder();

  order.ID_Obj := 0;
  order.prm := prm;

  order.nTimeStep := 30; // отчет гидрогеолога всегда получасовки
  order.UD1 := UDT1;
  order.UD2 := UDT2;
  order.DiagramType := DiagramType;

  order.State := dosWaiting;
end;

procedure THydroGeoRepDlg.AddSQLOrder(prm: TGMParam; UDT1, UDT2: LongWord);
var sPrm, sWhere: string;
    order: TDataOrder;
begin
  sPrm := IntToStr(prm.ID_Prm);
  sWhere :=  ' from Vals ' +
             '  where ID_Prm = ' + sPrm +
             '        and UTime >= ' + IntToStr(UDT1) +
             '        and UTime < ' + IntToStr(UDT2);

  order := frmRep.AddOrder();

  order.ID_Obj := 0;
  order.prm := prm;

  order.UD1 := UDT1;
  order.UD2 := UDT2;

  order.SQL := ' declare @MinL float, @MaxL float, @MinT int, @MaxT int ' +
               ' select @MinL = Min(Val), @MaxL = Max(Val) ' + sWhere +
               ' select top 1 @MinT = UTime ' + sWhere + ' and Val <= @MinL ' +
               ' select top 1 @MaxT = UTime ' + sWhere + ' and Val >= @MaxL ' +
               ' select @MinT, PrmRealVal(' + sPrm + ', @MinT, @MinL), @MaxT, PrmRealVal(' + sPrm + ', @MaxT, @MaxL)';

  order.OrderType := dotSQL;

  order.State := dosWaiting;
end;

procedure THydroGeoRepDlg.AddOrders();
var d: LongWord;
begin
  d := FirstDT;

  while d < LastDT do
  begin
    AddDataOrder(prmL, d, d + UTC_DAY, DIAGRAMTYPE_DEFAULT);
    AddDataOrder(prmV, d, d + UTC_DAY, DIAGRAMTYPE_INTEGRAL);
    AddDataOrder(prmV, int64(d) - UTC_DAY * (DayOf(UTCToLocal(d)) - 1), d, DIAGRAMTYPE_TOTAL);
    AddSQLOrder(prmL, d, d + UTC_DAY);

    frmRep.MaxProgress := frmRep.MaxProgress + 3;
    d := d + UTC_DAY;
  end;
end;

procedure THydroGeoRepDlg.InitReport();
begin
  frmRep.btGo.Enabled := false;
  Enabled := false;
  frmRep.pbProgress.Visible := true;
  cDays.Clear();
end;

procedure THydroGeoRepDlg.CommitReport();
begin
  frmRep.btGo.Enabled := true;
  Enabled := true;
  frmRep.pbProgress.Visible := false;
  cDays.Clear();
end;

procedure THydroGeoRepDlg.ParseDataOrder(order: TDataOrder);
var hdt: THydroDayData;
    prm: TGMBaseParam;
    i: int;
begin
  prm := order.prm;
  hdt := cDays.GetDayByDT(order.UD1, true);

  if prm = prmL then
  begin
    for i := 0 to Min(High(order.lDiagram), High(hdt.lDiagramL)) do
      hdt.lDiagramL[i] := order.lDiagram[i];
  end;

  if prm = prmV then
  begin
    if order.DiagramType = DIAGRAMTYPE_TOTAL then
    begin
      hdt := cDays.GetDayByDT(order.UD2, true); // по последнему, т.к. это сумма с начала мес€ца по первый день
      if Length(order.lDiagram) > 0 then
        hdt.Vmonth := order.lDiagram[0];
    end
    else
    begin
      for i := 0 to Min(High(order.lDiagram), High(hdt.lDiagramV)) do
        hdt.lDiagramV[i] := order.lDiagram[i];
    end;
  end;
end;

procedure THydroGeoRepDlg.ParseSQLOrder(order: TDataOrder);
var hdt: THydroDayData;
begin
  if order.slSQLRes.Count = 4 then
  begin
    hdt := cDays.GetDayByDT(order.UD1);

    hdt.Lmin.UTime := StrToIntDef(order.slSQLRes[0], 0);
    hdt.Lmin.Val := MyStrToFloatDef(order.slSQLRes[1]);

    hdt.Lmax.UTime := StrToIntDef(order.slSQLRes[2], 0);
    hdt.Lmax.Val := MyStrToFloatDef(order.slSQLRes[3]);
  end;
end;

procedure THydroGeoRepDlg.ProcessOrders();
var order: TDataOrder;
    i: int;
begin
  for i := frmRep.OrderCount() - 1 downto 0 do
  begin
    order := frmRep.Order[i];

    if order.OrderType = dotXLS then
    begin
       if order.State = dosDone then
         ParseDataOrder(order);

       order.State := dosDelete;
    end;

    if order.OrderType = dotSQL then
    begin
       if order.State = dosDone then
         ParseSQLOrder(order);

       order.State := dosDelete;
    end;
  end;
end;

function THydroGeoRepDlg.LookForParams(nReport: int): bool;
begin
  prmL := GMParams.ByID(xml.Hydrogeoreps[nReport].LevelPrm);
  prmV := GMParams.ByID(xml.Hydrogeoreps[nReport].FlowPrm);

  Result := (prmL <> nil) and (prmV <> nil);
end;

procedure THydroGeoRepDlg.XLSHeader(sh: Variant; FirstRow: int);
begin
  sh.Cells[FirstRow, 1] := 'ћес€ц';
  sh.Cells[FirstRow, 2] := 'ƒата';
  sh.Cells[FirstRow, 3] := '¬рем€';
  sh.Cells[FirstRow, 4] := '”ровень';
  sh.Cells[FirstRow, 5] := 'V/30 мин';
  sh.Cells[FirstRow, 6] := 'V с нач. сут';
  sh.Cells[FirstRow, 7] := 'L/V ср.';
  sh.Cells[FirstRow, 8] := 'L/V ср. сут.';
  sh.Cells[FirstRow, 9] := 'L max';
  sh.Cells[FirstRow, 10] := '¬рем€ L max';
  sh.Cells[FirstRow, 11] := 'L min';
  sh.Cells[FirstRow, 12] := '¬рем€ L min';
  sh.Cells[FirstRow, 13] := 'V с нач. мес.';
end;

procedure THydroGeoRepDlg.CreateXLS(nReport: int);
var xl, sh: Variant;
    dDay: LongWord;
    i, nRow: int;
    hdt: THydroDayData;
begin
  try
    xl := CreateOleObject('Excel.Application');
    sh := xl.WorkBooks.Add.Sheets[1];

    XLSHeader(sh, 3);
    dDay := FirstDT;
    nRow := 4;
    while dDay < LastDT do
    begin
      hdt := cDays.GetDayByDT(dDay, false);
      if hdt <> nil then
      begin
        hdt.ProcessDay(sh, nRow);
        inc(nRow, 48);
      end;

      dDay := dDay + UTC_Day;
    end;

    for i := 1 to 13 do
      sh.Columns[i].AutoFit;

    sh.Cells[1, 1] := xml.Hydrogeoreps[nReport].Name;
  finally
    if not VarIsNull(xl) and not VarIsEmpty(xl) then
    begin
      xl.WorkBooks[1].Saved := true;
      xl.Visible := true;

      sh := null;
      xl := null;
    end;
  end;
end;

procedure THydroGeoRepDlg.ProcessReport();
var i: int;
begin
  InitReport();

  try
    for i := 0 to lstObj.Count - 1 do
    begin
      frmRep.SetProgress(0);
      frmRep.MaxProgress := 0;

      if LookForParams(int(lstObj[i])) then
      begin
        AddOrders();
        frmRep.WaitForOrders();
        ProcessOrders();
        CreateXLS(int(lstObj[i]));
      end;
    end;
  finally
    CommitReport();
  end;
end;

procedure THydroGeoRepDlg.AddSelectedObjecs(nd: PVirtualNode; CycleObject: pointer);
begin
  if (TObject(CycleObject) is TList) and (TreeFromNode(nd).CheckState[nd] = csCheckedNormal) then
  begin
    TList(CycleObject).Add(TObject(nd.Index));
  end;
end;

procedure THydroGeoRepDlg.frmRepbtGoClick(Sender: TObject);
begin
  lstObj.Clear();
  FirstDT := frmRep.UTCDate1;
  LastDT := frmRep.UTCDate2;
  frmRep.ClearOrders();

  CycleTree(vstHydroGeoRep.RootNode, AddSelectedObjecs, lstObj);

  if frmRep.CommonCheck(lstObj) then ProcessReport();
end;

procedure THydroGeoRepDlg.AfterConstruction;
begin
  inherited;

  cDays := THydroDayDataCollection.Create(THydroDayData);
  lstObj := TList.Create();
end;

procedure THydroGeoRepDlg.FormDestroy(Sender: TObject);
begin
  cDays.Free();
  lstObj.free();
end;

procedure THydroGeoRepDlg.vstHydroGeoRepGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := 0;
end;

procedure THydroGeoRepDlg.vstHydroGeoRepGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var prm: TGMParam;
begin
  CellText := '';
  case Column of
    0: CellText := xml.HydroGeoReps[Node.Index].Name;
    1: begin
         prm := GMParams.ByID(xml.HydroGeoReps[Node.Index].LevelPrm);
         if prm <> nil then
           CellText := prm.GetNameWithPath([ppcObj, ppcDev]);
       end;
    2: begin
         prm := GMParams.ByID(xml.HydroGeoReps[Node.Index].FlowPrm);
         if prm <> nil then
           CellText := prm.GetNameWithPath([ppcObj, ppcDev]);
       end;
  end;
end;

end.
