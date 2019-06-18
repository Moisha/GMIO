////////////////////////////////////////////
// Отчет об авариях
////////////////////////////////////////////
unit AlarmRep;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, CheckLst, ExtCtrls, GMGlobals, Math,
  Threads.GMClient, Frame.ObjectTree, VirtualTrees, Frame.ReportBlank, GMConst,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  TAlarmRepDlg = class(TGMEnhancedScrollForm)
    mArch: TcxMemo;
    Splitter2: TcxSplitter;
    frmAlarms: TFrmReportBlank;
    procedure frmAlarmsbtGoClick(Sender: TObject);
  private
    lstParams: TList;
    procedure ProcessReport();
    procedure AddSelectedObjectAlarms(nd: PVirtualNode; CycleObject: pointer);
    procedure AddOrders;
    { Private declarations }
  public
    { Public declarations }
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    function ShowModal(): TModalResult; reintroduce;
  end;

var
  AlarmRepDlg: TAlarmRepDlg;

implementation

uses GMClMain, StrUtils, GMDBClasses, VT_Utils, Devices.UBZ.Common;

{$R *.dfm}

procedure TAlarmRepDlg.AddOrders();
var i: int;
begin
  for i := 0 to  lstParams.Count - 1 do
  begin
    with frmAlarms.AddOrder() do
    begin
      ID_Obj := 0;
      prm := lstParams[i];

      nTimeStep := 0;  // только изменения
      UD1 := frmAlarms.UTCDate1;
      UD2 := frmAlarms.UTCDate2;
      iRetryCount := 3;

      State := dosWaiting;
    end;
  end;
end;

procedure TAlarmRepDlg.ProcessReport();
var j, i: int;
    prm: TGMParam;
    order: TDataOrder;
    s: string;
    alarm: TGMAlarm;
    AlarmSignal: int;
    AlarmTxt: string;
begin
  frmAlarms.btGo.Enabled:=false;
  Enabled:=false;
  frmAlarms.pbProgress.Visible:=true;
  mArch.Clear();
  mArch.Lines.BeginUpdate();
  frmAlarms.ClearOrders();

  try
    frmAlarms.SetProgress(0);
    frmAlarms.MaxProgress := 0;

    AddOrders();
    frmAlarms.WaitForOrders();

    for i := 0 to frmAlarms.OrderCount() - 1 do
    begin
      order := frmAlarms.Order[i];
      prm := nil;
      if order.prm is TGMParam then
        prm := TGMParam(order.prm);

      if prm = nil then continue;

      Alarm := acAlarms.AlarmByID[prm.ID_Prm];

      if Alarm <> nil then
      begin
        AlarmSignal := Alarm.AlarmSignal;
        AlarmTxt := Alarm.GetAlarmTxt() + ' - ';
      end
      else
      begin
        AlarmSignal := prm.AlarmSignal;
        AlarmTxt := prm.GetNameWithPath([ppcObj, ppcDev], '.') + ' - ';
      end;

      for j := 0 to High(order.lDiagram) do
      begin
        s := FormatDateTime('dd.mm.yyyy hh:nn:ss ', UTCtoLocal(order.lDiagram[j].UTime));
        s := s + AlarmTxt;
        if prm.Device.ID_DevType = DEVTYPE_UBZ then
          s := s + DecodeUBZAlarms(Round(order.lDiagram[j].Val), ', ')
        else
        if prm.Device.ID_DevType = DEVTYPE_TR101 then
          s := s + DecodeTR101Alarms(Round(order.lDiagram[j].Val), ', ')
        else
        begin
          s := s + IfThen(order.lDiagram[j].Val = AlarmSignal, 'Сработка датчика', 'Отключение датчика');
        end;

        mArch.Lines.Add(s);
      end;

      order.State := dosDelete;
    end;

    if mArch.Lines.Count = 0 then
      mArch.Lines.Add('Аварий не зафиксировано');

  finally
    mArch.Lines.EndUpdate();
    frmAlarms.btGo.Enabled := true;
    Enabled := true;
    frmAlarms.pbProgress.Visible := false;
  end;
end;
     
function TAlarmRepDlg.ShowModal: TModalResult;
begin
  frmAlarms.frmTree.bHighLightObjects := false;
  frmAlarms.ShowObjectsOnly();
  frmAlarms.HidePeriod();

  frmAlarms.frmTree.frmNodes.Visible := false;
  frmAlarms.frmTree.Splitter1.Visible := false;

  Result := inherited ShowModal();
end;

procedure TAlarmRepDlg.AddSelectedObjectAlarms(nd: PVirtualNode; CycleObject: pointer);
var ID_Obj: int;
    prm: TGMParam;
begin
  if TreeFromNode(nd).CheckState[nd] = csCheckedNormal then
  begin
    ID_Obj := PVTNodeData(TreeFromNode(nd).GetNodeData(nd)).ID_Obj;
    for prm in GMParams do
    begin
      if prm.IsAlarm and (prm.Device.Obj.ID_Obj = ID_Obj) then
      begin
        if lstParams.IndexOf(prm) < 0 then
          lstParams.Add(prm);
      end;
    end;
  end;
end;

procedure TAlarmRepDlg.frmAlarmsbtGoClick(Sender: TObject);
begin
  lstParams.Clear();
  CycleTree(frmAlarms.frmTree.Tree.RootNode, AddSelectedObjectAlarms, nil);

  if frmAlarms.CommonCheck(lstParams, [cctDates, cctList]) then
    ProcessReport();
end;

procedure TAlarmRepDlg.AfterConstruction;
begin
  inherited;
  lstParams := TList.Create();
end;

procedure TAlarmRepDlg.BeforeDestruction;
begin
  lstParams.Free();
  inherited;       
end;

end.
