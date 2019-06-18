////////////////////////////////////////////
// Один агрегат с панели управления
////////////////////////////////////////////
unit Frame.ControlOneAggregate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, Buttons, StdCtrls, IniFiles, GMGlobals, GMDBClasses,
  GMConst, Threads.GMClient,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus, dxBevel, dxSkinOffice2010Silver;

type
  TControlOneAggregateFrame = class(TFrame)
    btnStart: TcxButton;
    btnStop: TcxButton;
    le1: TcxTextEdit;
    le2: TcxTextEdit;
    le3: TcxTextEdit;
    le4: TcxTextEdit;
    lbName: TcxLabel;
    LabelParams: TcxLabel;
    btnPreset: TcxButton;
    Bevel1: TdxBevel;
    btnResetAlarm: TcxButton;
    Label1: TcxLabel;
    Label2: TcxLabel;
    Label3: TcxLabel;
    Label4: TcxLabel;
    procedure btnManualClick(Sender: TObject);
  private
    { Private declarations }
    FAggregate: TGMAggregate;

    Prm_Manual, Prm_Control, Prm_Preset, Prm_ResetAlarm: TGMAggregateParam;
    function HandleOrderState(order: TDataOrder): bool;
  public
    { Public declarations }
    property Aggregate: TGMAggregate read FAggregate write FAggregate;
    procedure Load();
    procedure RefreshData();
    procedure EnableRemoteControl(bEnable: bool);
  end;

  procedure UpdateEditWithIDinTag(le: TcxTextEdit);

implementation

uses Math;

{$R *.dfm}

{ TControlOneAggregateFrame }

procedure UpdateEditWithIDinTag(le: TcxTextEdit);
var prm: TGMParam;
begin
  if not le.Visible then Exit;

  prm := GMParams.ByID(le.Tag);
  if (prm <> nil) and (prm.LastVal.UTime > 0) then
    le.Text := FormatFloatToShow(prm.LastVal.Val)
  else
    le.Text := '';
end;

procedure TControlOneAggregateFrame.Load();

  function GetID_Prm(PrmKind: int): int;
  var prm: TGMAggregateParam;
  begin
    Result := 0;
    prm := FAggregate.ParamByKind(PrmKind);
    if prm <> nil then
      Result := prm.prm.ID_Prm;
  end;

  procedure ReadEngine();
  begin
    Visible := true;

    le1.Visible := true;
    Label1.Caption := 'U, B';
    le1.Tag := GetID_Prm(CONTROL_PRM_ENG_U);

    le2.Visible := true;
    Label2.Caption := 'I, A';
    le2.Tag := GetID_Prm(CONTROL_PRM_ENG_I);

    le3.Visible := true;
    Label3.Caption := 'P, кВт';
    le3.Tag := GetID_Prm(CONTROL_PRM_ENG_P);

    le4.Visible := true;
    Label4.Caption := 'А, мч';
    le4.Tag := GetID_Prm(CONTROL_PRM_ENG_A);

    btnStart.Caption := 'ПУСК';
    btnStop.Caption := 'СТОП';
  end;

  procedure ReadValve();
  begin
    Visible := true;

    le1.Visible := true;
    Label1.Caption := 'P1, атм';
    le1.Tag := GetID_Prm(CONTROL_PRM_VALVE_P1);

    le2.Visible := true;
    Label2.Caption := 'P2, атм';
    le2.Tag := GetID_Prm(CONTROL_PRM_VALVE_P2);

    le3.Visible := true;
    Label3.Caption := 'Q, м3/ч';
    le3.Tag := GetID_Prm(CONTROL_PRM_VALVE_Q);

    le4.Visible := false;
    Label4.Visible := false;

    btnStart.Caption := 'ОТКР';
    btnStop.Caption := 'ЗАКР';
  end;

  procedure ReadHeater();
  begin
    Visible := true;

    le1.Visible := true;
    Label1.Caption := 'T, C';
    le1.Tag := GetID_Prm(CONTROL_PRM_HEAT_T);

    le2.Visible := false;
    Label2.Visible := false;
    le3.Visible := false;
    Label3.Visible := false;
    le4.Visible := false;
    Label4.Visible := false;

    btnStart.Caption := 'ВКЛ';
    btnStop.Caption := 'ВЫКЛ';
  end;

begin
  if FAggregate = nil then
  begin
    Visible := false;
    Exit;
  end;

  Visible := true;

  Prm_Manual := FAggregate.ParamByKind(CONTROL_PRM_MANUAL);
  Prm_Control := FAggregate.ParamByKind(CONTROL_PRM_RUN);
  Prm_Preset := FAggregate.ParamByKind(CONTROL_PRM_PRESET);
  Prm_ResetAlarm := FAggregate.ParamByKind(CONTROL_PRM_RESETALARM);

  lbName.Caption := FAggregate.AggrName;

  case FAggregate.AggrType of
    AGGREGATE_TYPE_ENGINE: ReadEngine();
    AGGREGATE_TYPE_VALVE: ReadValve();
    AGGREGATE_TYPE_HEAT: ReadHeater();
    else Visible := false;
  end;

  RefreshData();
end;

function TControlOneAggregateFrame.HandleOrderState(order: TDataOrder): bool;
var
  err: string;
begin
  err := order.CommandOrderError();
  Result := err = '';
  order.State := dosDelete;

  if not Result then
    ShowMessageBox(err, MB_ICONERROR);
end;

procedure TControlOneAggregateFrame.btnManualClick(Sender: TObject);
label AskPreset;
var order: TDataOrder;
    id_prm: int;
    fVal: double;
    sVal: string;
    c: int;
    res: bool;
begin
  id_prm := 0;
  if Sender = btnPreset then
  begin
    if Prm_Preset <> nil then
      id_prm := Prm_Preset.prm.ID_Prm
  end
  else
  if Sender = btnResetAlarm then
  begin
    if Prm_ResetAlarm <> nil then
      id_prm := Prm_ResetAlarm.prm.ID_Prm
  end
  else
  if (Sender = btnStart) or (Sender = btnStop) then
  begin
    if Prm_Control <> nil then
      id_prm := Prm_Control.prm.ID_Prm;
  end;

  if id_prm <= 0 then Exit;

  if Sender = btnResetAlarm then
    fVal := 1
  else
  if Sender = btnPreset then
  begin
    AskPreset:
    if not InputQuery('Ввод уставки', '', sVal) then Exit;
    Val(sVal, fVal, c);
    if c > 0 then goto AskPreset;
  end
  else
    fVal := IfThen( Sender = btnStart, 1, 0);

  order := TDataOrders.AddSendCommandOrder(GMParams.ByID(id_prm), fVal);
  order.TimeHold := IfThen(Sender = btnResetAlarm, 1, 0); // сброс аварии держит выход секунду, осталные замыкают до отмены
  order.Exec();

  order.WaitFor(20000);
  res := HandleOrderState(order);

  if res and ((Sender = btnStart) or (Sender = btnStop)) and (Prm_Manual <> nil) then
  begin
    order := TDataOrders.AddSendCommandOrder(Prm_Manual.prm, fVal);
    order.Exec();
    order.WaitFor(20000);
    HandleOrderState(order);
  end;
end;

procedure TControlOneAggregateFrame.RefreshData();
begin
  if FAggregate = nil then Exit;

  btnPreset.Visible := (Prm_Preset <> nil);
  btnResetAlarm.Visible := (Prm_ResetAlarm <> nil);

  btnStart.Visible := (Prm_Control <> nil) and (FAggregate.AggrType = AGGREGATE_TYPE_ENGINE);
  btnStop.Visible := btnStart.Visible;

  UpdateEditWithIDinTag(le1);
  UpdateEditWithIDinTag(le2);
  UpdateEditWithIDinTag(le3);
  UpdateEditWithIDinTag(le4);
end;

procedure TControlOneAggregateFrame.EnableRemoteControl(bEnable: bool);
begin
  btnStart.Enabled := bEnable;
  btnStop.Enabled := bEnable;
  btnPreset.Enabled := bEnable;
  btnResetAlarm.Enabled := bEnable;
end;

end.
