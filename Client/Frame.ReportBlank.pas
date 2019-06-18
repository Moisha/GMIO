/// /////////////////////////////////////////
// «аготовка под разные отчеты
/// /////////////////////////////////////////
unit Frame.ReportBlank;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Spin, ExtCtrls, Frame.ObjectTree, GMGlobals,
  VirtualTrees, ActiveX, GMConst, Threads.GMClient, ComObj, Excel2000,
  DateComboBox, GMDBClasses,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus, dxCore, cxDateUtils, Generics.Collections, dxSkinOffice2010Silver;

type
  TReportOption = (roDaySumm, roTemplate);
  TReportOptions = set of TReportOption;

  TRepPeriodType = (rptMin, rptHour, rptDay, rptMonth, rptYear);

  TReportCheck = (cctXLS, cctDates, cctList);
  TReportCheckList = set of TReportCheck;

const
  cctAll = [cctXLS, cctDates, cctList];

type
  TReportData = class
  private
    XL, Sheet: Variant;
  public
    lstChannels: TList;
    Options: TReportOptions;

    UTCDate1, UTCDate2: LongWord;

    PeriodType: TRepPeriodType;
    PeriodLen: int;
    DataType: int;

    Template: string;

    function OpenXL: bool;
    procedure CloseXL();
    procedure FinalizeXLS();

    function AddPeriod(udt: UINT): UINT;
    function AddStep(udt: UINT): UINT;
    function IntervalNumber(udt: UINT): int;

    constructor Create();
    destructor Destroy(); override;
  end;

type
  TFrmReportBlank = class(TFrame)
    frmTree: TObjTreeFrame;
    Panel2: TcxGroupBox;
    Label1: TcxLabel;
    Label2: TcxLabel;
    btGo: TcxButton;
    pbProgress: TcxProgressBar;
    btStop: TcxButton;
    cmbPeriod: TcxComboBox;
    pnPeriod: TcxGroupBox;
    Label3: TcxLabel;
    seInterval: TcxSpinEdit;
    cmbIntervalType: TcxComboBox;
    dtpDT: TcxDateEdit;
    dtpDT2: TcxDateEdit;
    procedure dtpDTChange(Sender: TObject);
    procedure cmbPeriodChange(Sender: TObject);
    procedure cmbIntervalTypeChange(Sender: TObject);
    procedure btStopClick(Sender: TObject);
  private
    { Private declarations }
    lOrders: TList<TDataOrder>;
    FCurrentRep: TReportData;

    function GetUTCDate1: LongWord;
    function GetUTCDate2: LongWord;
    function GetPeriod: int;
    function GetMaxProgress: int;
    procedure SetMaxProgress(const Value: int);
    function IsXLInstalled: bool;
    function CheckDates: bool;
    function CheckLst(lst: TList): bool;
    function GetOrder(n: int): TDataOrder;
    procedure AddDataOrder(p: TGMBaseParam; d1, d2: LongWord);
    procedure AddSQLOrder(prm: TGMBaseParam; d1, d2: LongWord);
    procedure AddOrders(d1, d2: LongWord);
    procedure FillXLS(d1, d2: LongWord);
    function AddCommonDataOrder(prm: TGMBaseParam; d1, d2: LongWord): TDataOrder;
    function GetDataType: int;
  protected
    FStop: bool;
    FInReport: bool;
  public
    { Public declarations }
    property Stop: bool read FStop;
    property InReport: bool read FInReport;
    property UTCDate1: LongWord read GetUTCDate1;
    property UTCDate2: LongWord read GetUTCDate2;
    property Period: int read GetPeriod;
    property DataType: int read GetDataType;
    property MaxProgress: int read GetMaxProgress write SetMaxProgress;
    procedure SetProgress(n: int);
    procedure ShowObjectsOnly();
    procedure HidePeriod();
    function CommonCheck(lst: TList; CheckContents: TReportCheckList = cctAll): bool;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure FinishReport;
    procedure StartReport;
    procedure ClearOrders();
    function AddOrder(): TDataOrder;
    procedure WaitForOrders(NeedUpdateProgressBar: bool = true);
    property Order[n: int]: TDataOrder read GetOrder;
    function OrderCount(): int;
    procedure ProcessCommonReport(rep: TReportData);
    function CreateReportData(): TReportData;
    procedure SetDataLength(DataLength: int);
    procedure EnableOpenInterval(AEnable: bool);
  end;

implementation

uses Math, Frame.ClientObject, DateUtils, VT_Utils, ProgramLogFile, System.IOUtils;

{$R *.dfm}
{ TFrmReportBlank }

procedure TFrmReportBlank.AfterConstruction;
begin
  inherited;

  lOrders := TList<TDataOrder>.Create();
  seInterval.Value := 10;
  dtpDT.Date := Floor(Now() - 1);
  dtpDT2.Date := Floor(Now());
  cmbIntervalType.ItemIndex := 2;
  cmbIntervalTypeChange(nil);
  cmbPeriodChange(nil);
end;

function TFrmReportBlank.GetPeriod: int;
begin
  if FCurrentRep <> nil then
  begin
    case FCurrentRep.PeriodType of
      rptMin:
        Result := FCurrentRep.PeriodLen;
      rptHour:
        Result := FCurrentRep.PeriodLen * 60;
      rptDay:
        Result := FCurrentRep.PeriodLen * 60 * 24;
    else
      Result := FCurrentRep.PeriodLen;
    end;
  end
  else
  begin
    case cmbIntervalType.ItemIndex of
      0:
        Result := 60 * 24;
      1:
        Result := 60;
    else
      Result := seInterval.Value;
    end;
  end;
end;

function TFrmReportBlank.GetUTCDate1: LongWord;
begin
  Result := LocalToUTC(dtpDT.Date);
end;

function TFrmReportBlank.GetUTCDate2: LongWord;
begin
  Result := LocalToUTC(dtpDT2.Date);
end;

procedure TFrmReportBlank.SetProgress(n: int);
begin
  pbProgress.Position := Max(0, n);
end;

function TFrmReportBlank.GetDataType: int;
begin
  if FCurrentRep <> nil then
    Result := FCurrentRep.DataType
  else
    Result := DATATYPE_CURRENT_DIAGRAM;
end;

function TFrmReportBlank.GetMaxProgress: int;
begin
  Result := Round(pbProgress.Properties.Max);
end;

procedure TFrmReportBlank.SetMaxProgress(const Value: int);
begin
  pbProgress.Properties.Max := Value;
end;

procedure TFrmReportBlank.ShowObjectsOnly();
begin
  if frmTree.Tree.RootNode.ChildCount = 0 then
    frmTree.Init([NODE_LEVEL_OBJECT]);

  frmTree.SetDeviceSystemViewMode(dsvmObjects);
  frmTree.ShowObjects(fotAll);
end;

procedure TFrmReportBlank.dtpDTChange(Sender: TObject);
begin
  if cmbPeriod.ItemIndex = 1 then
    dtpDT2.Date := dtpDT.Date + 1;
end;

procedure TFrmReportBlank.EnableOpenInterval(AEnable: bool);
begin
  if AEnable then
  begin
    Label2.Enabled := true;
    dtpDT2.Enabled := true;

    dtpDT.Properties.Kind := ckDateTime;
    dtpDT2.Properties.Kind := ckDateTime;
  end
  else
  begin
    Label2.Enabled := false;
    dtpDT2.Enabled := false;

    dtpDT.Properties.Kind := ckDate;
    dtpDT2.Properties.Kind := ckDate;

    dtpDT.Date := Floor(dtpDT.Date);
    dtpDT2.Date := Floor(dtpDT2.Date)
  end;
end;

procedure TFrmReportBlank.cmbPeriodChange(Sender: TObject);
begin
  EnableOpenInterval(cmbPeriod.ItemIndex = 0);
  dtpDTChange(dtpDT);
end;

procedure TFrmReportBlank.HidePeriod;
begin
  pnPeriod.Visible := false;
end;

function TFrmReportBlank.IsXLInstalled(): bool;
var
  g: TGUID;
begin
  Result := false;
  try
    Result := (CLSIDFromProgID('Excel.Application', g) = S_OK);
    if not Result then
      ShowMessageBox('Excel не установлен', MB_ICONERROR);
  except
    ShowMessageBox('Excel не установлен', MB_ICONERROR);
  end
end;

function TFrmReportBlank.CheckDates(): bool;
begin
  Result := CompareDateTime(dtpDT.Date, dtpDT2.Date) < 0;
  if not Result then
    ShowMessageBox('Ќачальна€ дата должна быть меньше конечной', MB_ICONERROR);
end;

function TFrmReportBlank.CheckLst(lst: TList): bool;
begin
  Result := lst.Count > 0;

  if not Result then
    ShowMessageBox('Ќе выбрано ни одного объекта', MB_ICONERROR);
end;

function TFrmReportBlank.CommonCheck(lst: TList; CheckContents: TReportCheckList = cctAll): bool;
var
  bTrouble: bool;
begin
  bTrouble := ((cctXLS in CheckContents) and not IsXLInstalled()) or ((cctDates in CheckContents) and not CheckDates())
    or ((cctList in CheckContents) and not CheckLst(lst));

  Result := not bTrouble;
end;

procedure TFrmReportBlank.cmbIntervalTypeChange(Sender: TObject);
begin
  seInterval.Visible := cmbIntervalType.ItemIndex = 2;
  Label3.Visible := seInterval.Visible;
end;

procedure TFrmReportBlank.BeforeDestruction;
begin
  lOrders.Free();
  inherited;
end;

procedure TFrmReportBlank.btStopClick(Sender: TObject);
begin
  FStop := true;
end;

procedure TFrmReportBlank.WaitForOrders(NeedUpdateProgressBar: bool = true);
var
  i, n: int;
  Order: TDataOrder;
begin
  if NeedUpdateProgressBar then
    MaxProgress := lOrders.Count;

  repeat
    n := 0;
    Sleep(100);

    for i := 0 to lOrders.Count - 1 do
    begin
      Order := lOrders[i];
      if Order.State in [dosWaiting, dosProcessing] then
        inc(n);
    end;

    if NeedUpdateProgressBar then
      SetProgress(MaxProgress - n);

    Application.ProcessMessages();
  until FStop or (n = 0);

  ProgramLog.AddMessage(ClassName + '.Orders received');
end;

procedure TFrmReportBlank.ClearOrders;
begin
  lOrders.Clear();
end;

function TFrmReportBlank.GetOrder(n: int): TDataOrder;
begin
  Result := lOrders[n];
end;

function TFrmReportBlank.OrderCount: int;
begin
  Result := lOrders.Count;
end;

function TFrmReportBlank.AddCommonDataOrder(prm: TGMBaseParam; d1, d2: LongWord): TDataOrder;
begin
  Result := AddOrder();

  Result.prm := prm;
  Result.UD1 := d1;
  Result.UD2 := d2;
  Result.nTimeStep := Period;
  Result.DataType := DataType;

  ProgramLog().AddMessage(ClassName() + Format('.AddOrder %d, %d, %d', [prm.ID_Prm, d1, d2]));
end;

procedure TFrmReportBlank.AddSQLOrder(prm: TGMBaseParam; d1, d2: LongWord);
begin
  with AddCommonDataOrder(prm, d1, d2) do
  begin
    AggrType := DATA_AGGREGATION_TYPE_DAYSUMM;
    State := dosWaiting;
  end;
end;

procedure TFrmReportBlank.AddDataOrder(p: TGMBaseParam; d1, d2: LongWord);
begin
  AddCommonDataOrder(p, d1, d2).State := dosWaiting;
end;

function TFrmReportBlank.AddOrder: TDataOrder;
begin
  Result := TDataOrders.Add();
  Result.OrderType := dotXLS;
  lOrders.Add(Result);
end;

procedure TFrmReportBlank.AddOrders(d1, d2: LongWord);
var
  i: int;
  p: TGMBaseParam;
begin
  ClearOrders();

  for i := 0 to FCurrentRep.lstChannels.Count - 1 do
  begin
    p := FCurrentRep.lstChannels[i];
    if roDaySumm in FCurrentRep.Options then
      AddSQLOrder(p, d1, d2)
    else
      AddDataOrder(p, d1, d2);
  end;
end;

procedure TFrmReportBlank.FillXLS(d1, d2: LongWord);
var
  j, k, col, row: int;
  Order: TDataOrder;
  uOrderDT, dt: LongWord;
  val: Variant;
  iDataFromCol: int;
  f: double;
  p: TGMBaseParam;
begin
  col := 2; // в первую колонку нахерачить дату
  iDataFromCol := 0; // признак, что дату берем при проходе определенной колонки
  // пока 0 - даты нет
  // потом по€витс€ целое число, если обрабатываема€ колонка равна этому числу или 0, то дату пишем

  for k := 0 to lOrders.Count - 1 do
  begin
    row := 3 + FCurrentRep.IntervalNumber(d1);

    p := FCurrentRep.lstChannels[k];
    if p is TGMParam then
      FCurrentRep.Sheet.Cells[1, col] := TGMParam(p).Device.Obj.Name;
    FCurrentRep.Sheet.Cells[2, col] := p.GetNameWithPath([]);

    Order := lOrders[k];
    if Order.OrderType = dotSQL then
    begin
      if Order <> nil then
      begin
        FCurrentRep.Sheet.Cells[row, 1] := '''' + FormatDateTime('dd.mm.yyyy', UTCtoLocal(d1));
        val := Null;

        if Order.slSQLRes.Count >= 1 then
        begin
          f := MyStrToFloatDef(Order.slSQLRes[0]);
          if f <> INCORRECT_VALUE then
            val := f;
        end;

        FCurrentRep.Sheet.Cells[row, col] := val;
      end;
    end
    else
    begin
      if Order <> nil then
      begin
        dt := d1;
        while dt < d2 do
        begin
          if (col >= 2) and (iDataFromCol in [0, col]) then
          begin
            FCurrentRep.Sheet.Cells[row, 1] := '''' + FormatDateTime('dd.mm.yyyy hh:nn', UTCtoLocal(dt));
            iDataFromCol := col;
          end;

          val := Null;
          for j := 0 to high(Order.lDiagram) do
          begin
            uOrderDT := Order.lDiagram[j].UTime;
            if uOrderDT = dt then
            begin
              val := Order.lDiagram[j].val;
              break;
            end;
          end;

          FCurrentRep.Sheet.Cells[row, col] := val;

          dt := FCurrentRep.AddPeriod(dt);
          inc(row);
        end;
      end;
    end;

    Order.State := dosDelete;
    inc(col);
  end;
end;

procedure TFrmReportBlank.ProcessCommonReport(rep: TReportData);
var
  d1, d2, MainDT, LastDT: LongWord;
begin
  FCurrentRep := rep;

  rep.XL := Null;
  rep.Sheet := Null;

  btGo.Enabled := false;
  pbProgress.Visible := true;
  MaxProgress := 100;

  try
    if not FCurrentRep.OpenXL() then Exit;

    MainDT := rep.UTCDate1;
    LastDT := rep.UTCDate2;

    d1 := MainDT;
    if FCurrentRep.DataType <= DATATYPE_CURRENT_DIAGRAM then
      d2 := min(rep.AddStep(d1), LastDT)
    else
      d2 := LastDT;

    while d1 < LastDT do
    begin
      SetProgress(Floor((d2 - MainDT) / (LastDT - MainDT) * 100));

      AddOrders(d1, d2);
      if FStop then Exit;

      WaitForOrders(false);
      if FStop then Exit;

      FillXLS(d1, d2);
      if FStop then Exit;

      d1 := d2;
      d2 := min(rep.AddStep(d1), LastDT);
    end;

  finally
    try
      if not FStop then
        FCurrentRep.FinalizeXLS()
      else
        FCurrentRep.CloseXL();
    finally
      FCurrentRep := nil;
      btGo.Enabled := true;
      pbProgress.Visible := false;
    end;
  end;
end;

function TFrmReportBlank.CreateReportData: TReportData;
begin
  Result := TReportData.Create();

  Result.DataType := DATATYPE_CURRENT_DIAGRAM;

  Result.UTCDate1 := UTCDate1;
  Result.UTCDate2 := UTCDate2;

  case cmbIntervalType.ItemIndex of
    0:
      begin
        Result.PeriodType := rptDay;
        Result.PeriodLen := 1;
      end;
    1:
      begin
        Result.PeriodType := rptHour;
        Result.PeriodLen := 1;
      end;
  else
    begin
      Result.PeriodType := rptMin;
      Result.PeriodLen := seInterval.Value;
    end;
  end;
end;

procedure TFrmReportBlank.StartReport;
begin
  FStop := false;
  FInReport := true;
  RecoursiveEnableControls(self, false);
  btStop.Visible := true;
  EnableWithParents(btStop)
end;

procedure TFrmReportBlank.FinishReport;
begin
  FStop := false;
  FInReport := false;
  btStop.Visible := false;
  RecoursiveEnableControls(self, true);
  cmbPeriodChange(cmbPeriod);
end;

procedure TFrmReportBlank.SetDataLength(DataLength: int);
var
  y, m, d: WORD;
begin
  DecodeDate(dtpDT.Date, y, m, d);

  case DataLength of
    REPORTLENGTH_DAY:
      dtpDT2.Date := dtpDT.Date + 1;
    REPORTLENGTH_MONTH:
      begin
        dtpDT.Date := EncodeDate(y, m, 1);
        dtpDT2.Date := IncMonth(dtpDT.Date);
      end;
    REPORTLENGTH_YEAR:
      begin
        dtpDT.Date := EncodeDate(y, 1, 1);
        dtpDT2.Date := EncodeDate(y + 1, 1, 1);
      end;
  end;

  if dtpDT2.Date <= dtpDT.Date then
    dtpDT2.Date := dtpDT.Date + 1;
end;

{ TReportData }

function TReportData.AddPeriod(udt: UINT): UINT;
var
  dt, res: TDateTime;
begin
  dt := UTCtoLocal(udt);

  case PeriodType of
    rptMin:
      res := dt + PeriodLen * OneMinute;
    rptHour:
      res := dt + PeriodLen * OneHour;
    rptDay:
      res := dt + PeriodLen * 1;
    rptMonth:
      res := IncMonth(dt, PeriodLen);
    rptYear:
      res := IncMonth(dt, PeriodLen * 12);
  else
    res := UTCtoLocal(UTCDate2);
  end;

  Result := LocalToUTC(res);
end;

function TReportData.AddStep(udt: UINT): UINT;
begin
  if PeriodType >= rptDay then
    Result := AddPeriod(udt)
  else
    Result := udt + UTC_DAY;
end;

procedure TReportData.CloseXL;
begin
  XL.Application.Quit;
end;

constructor TReportData.Create;
begin
  lstChannels := TList.Create();
  XL := Null;
  Sheet := Null;
  Options := [];
end;

destructor TReportData.Destroy;
begin
  lstChannels.Free();
  XL := Null;
  Sheet := Null;

  inherited;
end;

procedure TReportData.FinalizeXLS;
var i: int;
begin
   for i := 0 to lstChannels.Count + 1 do // на одну колонку больше, это дата
     Sheet.Columns[i + 1].AutoFit;

  if not VarIsNull(XL) and not VarIsEmpty(XL) then
  begin
    XL.WorkBooks[1].Saved := true;
    XL.Visible := true;
  end;
end;

function OpenDoc(eaSGrid: Variant; const AFileName: String; AReadOnly: Boolean; bOpenCopy: bool = false): bool;
begin
  Result := false;

  if VarIsEmpty(eaSGrid) then
    eaSGrid := CreateOLEObject('Excel.Application');

  try
    ProgramLog.AddMessage('OpenDoc WorkBooks.Add');
    eaSGrid.WorkBooks.Open(AFileName, EmptyParam, AReadOnly, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
    EmptyParam, EmptyParam, EmptyParam);
    Result := true;
    ProgramLog.AddMessage('OpenDoc WorkBooks.Add Done Workbooks: ' + IntToStr(eaSGrid.WorkBooks.Count));
  except
    on e: Exception do
      ProgramLog.AddException('OpenDoc ' + e.Message);
  end;
end;

function TReportData.IntervalNumber(udt: UINT): int;
var
  dt: UINT;
begin
  Result := 0;

  if (udt >= UTCDate1) and (udt <= UTCDate2) then
  begin
    dt := UTCDate1;
    while dt < udt do
    begin
      inc(Result);
      dt := AddPeriod(dt);
    end;
  end;
end;

function TReportData.OpenXL: bool;
var
  dir, fn, fnTemp: string;
  bTemplateFound: bool;
  res, n: int;
begin
  ProgramLog.AddMessage(ClassName + '.OpenXL');
  Result := not VarIsEmpty(XL) and not VarIsNull(Sheet);
  if Result then Exit;

  bTemplateFound := false;
  if (roTemplate in Options) and (Trim(Template) <> '') then
  begin
    dir := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));
    fn := dir + Template;

    bTemplateFound := FileExists(fn);
    if not bTemplateFound then
    begin
      fn := dir + 'Reports\' + Template;
      bTemplateFound := FileExists(fn);
      if not bTemplateFound then
      begin
        res := ShowMessageBox('Ўаблон отчета "' + Template + '" не найден!'#13#10'—формировать отчет на чистом листе?',
          MB_ICONEXCLAMATION or MB_YESNO);
        if res = IDNO then
          Exit;
      end;
    end;
  end;

  ProgramLog.AddMessage(ClassName + '.OpenXL CreateOLEObject');
  XL := CreateOLEObject('Excel.Application');
  XL.Application.DisplayAlerts := false;

  if bTemplateFound then
  begin
    fnTemp := GetTemporaryFileName(TPath.Combine(GetTempDir(), 'repTmp.xls'), 'repTmp.xls', true);
    ProgramLog.AddMessage(ClassName + '.OpenXL TempFileName = ' + fnTemp);

    if not CopyFile(PChar(fn), PChar(fnTemp), false) then
    begin
      ProgramLog.AddError(ClassName + '.OpenXL can not copy TempFileName = ' + fnTemp);
      Exit;
    end;

    ProgramLog.AddMessage(ClassName + '.OpenXL OpenDoc');
    OpenDoc(XL, fnTemp, false, false);
    ProgramLog.AddMessage(ClassName + '.OpenXL SheetCount');
    n := XL.ActiveWorkbook.Sheets.Count;
    ProgramLog.AddMessage(ClassName + '.OpenXL SelectSheet');
    Sheet := XL.ActiveWorkbook.Sheets[n];
    Result := true;
  end
  else
  begin
    Sheet := XL.WorkBooks.Add.Sheets[1];
    Result := true;
  end;
end;

end.
