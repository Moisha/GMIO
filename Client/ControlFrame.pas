////////////////////////////////////////////
// Панель управления
////////////////////////////////////////////
unit ControlFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, Frame.ControlOneAggregate, Buttons, GMGlobals, StdCtrls,
  GMDBClasses, GMConst,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus, dxSkinOffice2010Silver;

type
  TControlFrm = class;

  TGMForm = class(TGMEnhancedScrollForm)
  private
    procedure WMShowControlPanel(var Msg: TMessage); message WM_SHOW_CONTROL_PANEL;
  protected
    function GetControlFrame(): TControlFrm; virtual;
  public
    property ControlFrame: TControlFrm read GetControlFrame;
    function GetMinWidth: int;
    function GetMinHeight: int;
  end;
  TGMFormClass = class of TGMForm;

  TGMControlMode = (cmEdit, cmView, cmControl);
  TControlFrm = class(TFrame)
    Panel1: TcxGroupBox;
    pnAggregates: TcxGroupBox;
    pnAlarm: TcxGroupBox;
    Aggregate1: TControlOneAggregateFrame;
    Aggregate2: TControlOneAggregateFrame;
    Aggregate3: TControlOneAggregateFrame;
    sbClose: TcxButton;
    mAlarms: TcxMemo;
    lObjName: TcxLabel;
    lAddPrm1: TcxLabel;
    eAddPrm1: TcxTextEdit;
    lAddPrm2: TcxLabel;
    eAddPrm2: TcxTextEdit;
    lAddPrm3: TcxLabel;
    eAddPrm3: TcxTextEdit;
    procedure sbCloseClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
    FHolder: TGMAggregate;
    FID_Obj: int;
    FControlMode: TGMControlMode;
    procedure SetObj(const Value: int);
    procedure Load;
    procedure UpdateAlarms;
    procedure HideTitleParams;
    procedure SetControlMode(const Value: TGMControlMode);
  public
    { Public declarations }
    property ControlMode: TGMControlMode read FControlMode write SetControlMode;
    property ID_Obj: int read FID_Obj write SetObj;
    procedure RefreshData();
    constructor Create(AOWner: TComponent); override;
  end;

implementation

uses Devices.UBZ.Common, Devices.Vacon.Common, EsLogging;

{$R *.dfm}

procedure TControlFrm.HideTitleParams();
begin
  lAddPrm1.Caption := '';
  lAddPrm2.Caption := '';
  lAddPrm3.Caption := '';

  eAddPrm1.Visible := false;
  eAddPrm2.Visible := false;
  eAddPrm3.Visible := false;

  eAddPrm1.Tag := 0;
  eAddPrm2.Tag := 0;
  eAddPrm3.Tag := 0;
end;

procedure TControlFrm.Load();
var i: int;
    e: TcxTextEdit;
    l: TcxLabel;
begin
  Aggregate1.Load();
  Aggregate2.Load();
  Aggregate3.Load();

  lObjName.Caption := GMObjects.ObjByID(FID_Obj).Name;

  HideTitleParams();
  if FHolder <> nil then
  begin
    for i := 0 to FHolder.ParamCount - 1 do
    begin
      case FHolder.Params[i].PrmKind of
        CONTROL_PRM_ADD1: begin l := lAddPrm1; e := eAddPrm1; end;
        CONTROL_PRM_ADD2: begin l := lAddPrm2; e := eAddPrm2; end;
        CONTROL_PRM_ADD3: begin l := lAddPrm3; e := eAddPrm3; end;
        else begin l := nil; e := nil; end;
      end;

      if e <> nil then
      begin
        e.Visible := true;
        e.Tag := FHolder.Params[i].prm.ID_Prm;
        l.Caption := FHolder.Params[i].Caption;
      end;
    end;
  end;

  RefreshData();
end;

procedure TControlFrm.UpdateAlarms();
var i: int;
    sl: TStringList;
    bHasAlarms: bool;
begin
  mAlarms.Clear();
  sl := TStringList.Create();
  for i := 0 to GMParams.Count - 1 do
  begin
    if (GMParams[i].Device.Obj.ID_Obj = FID_Obj) and (GMParams[i].LastVal.UTime > 0)  then
    begin
      sl.Clear();
      bHasAlarms := false;

      if (GMParams[i].Device.ID_DevType in [DEVTYPE_UBZ, DEVTYPE_GM_UBZ]) and (GMParams[i].PType.ID_PT = ID_PT_UBZ_ALARM) then
      begin
        bHasAlarms := true;
        DecodeUBZAlarms(Round(GMParams[i].LastVal.Val), sl);
      end;

      if (GMParams[i].Device.ID_DevType = DEVTYPE_TR101) and (GMParams[i].PType.ID_PT = ID_PT_TR101_ALARM) then
      begin
        bHasAlarms := true;
        DecodeTR101Alarms(Round(GMParams[i].LastVal.Val), sl);
      end;

      if (GMParams[i].Device.ID_DevType = DEVTYPE_VACON_NXL) and (GMParams[i].PType.ID_PT = ID_PT_COMMON_ALARM) then
      begin
        bHasAlarms := true;
        DecodeVaconAlarms(Round(GMParams[i].LastVal.Val), sl);
      end;

      if bHasAlarms then
      begin
        mAlarms.Lines.Add(GMParams[i].Device.Name + ':');

        if sl.Count > 0 then
          mAlarms.Lines.AddStrings(sl)
        else
          mAlarms.Lines[mAlarms.Lines.Count - 1] := mAlarms.Lines[mAlarms.Lines.Count - 1] + ' нет аварий';
      end;
    end;
  end;

  sl.Free();
end;

procedure TControlFrm.RefreshData();
begin
  Aggregate1.RefreshData();
  Aggregate2.RefreshData();
  Aggregate3.RefreshData();

  UpdateAlarms();
  UpdateEditWithIDinTag(eAddPrm1);
  UpdateEditWithIDinTag(eAddPrm2);
  UpdateEditWithIDinTag(eAddPrm3);
end;

procedure TControlFrm.SetObj(const Value: int);
var i: int;
begin
  FID_Obj := Value;

  Aggregate1.Aggregate := nil;
  Aggregate2.Aggregate := nil;
  Aggregate3.Aggregate := nil;
  FHolder := nil;

  for i := 0 to GMAggregates.Count - 1 do
    if GMAggregates[i].Obj.ID_Obj = FID_Obj then
    begin
      if GMAggregates[i].AggrType = AGGREGATE_TYPE_PARAM_HOLDER then
        FHolder := GMAggregates[i]
      else
      if Aggregate1.Aggregate = nil then
        Aggregate1.Aggregate := GMAggregates[i]
      else
      if Aggregate2.Aggregate = nil then
        Aggregate2.Aggregate := GMAggregates[i]
      else
      if Aggregate3.Aggregate = nil then
        Aggregate3.Aggregate := GMAggregates[i];
    end;

  Load();
end;

procedure TControlFrm.sbCloseClick(Sender: TObject);
begin
  GMPostMessage(WM_SHOW_CONTROL_PANEL, 0, 0, DefaultLogger);
end;

constructor TControlFrm.Create(AOWner: TComponent);
begin
  inherited;

  SetControlMode(cmView);

  Aggregate1.Width := 100;
  Aggregate2.Width := 100;
  Aggregate3.Width := 100;
  Width := 270;
end;

procedure TControlFrm.FrameResize(Sender: TObject);
begin
  sbClose.Left := Width - sbClose.Width - 4;
  eAddPrm1.Left := Width - eAddPrm1.Width - 4;
  eAddPrm2.Left := Width - eAddPrm2.Width - 4;
  eAddPrm3.Left := Width - eAddPrm3.Width - 4;
  lObjName.Left := 1;
  lObjName.Width := sbClose.Left - lObjName.Width - 5; 
end;

procedure TControlFrm.SetControlMode(const Value: TGMControlMode);
begin
  FControlMode := Value;

  Aggregate1.EnableRemoteControl(FControlMode = cmControl);
  Aggregate2.EnableRemoteControl(FControlMode = cmControl);
  Aggregate3.EnableRemoteControl(FControlMode = cmControl);
end;

{ TGMForm }

function TGMForm.GetControlFrame: TControlFrm;
begin
  Result := nil;
end;

function TGMForm.GetMinHeight: int;
begin
  Result := Constraints.MinHeight;
end;

function TGMForm.GetMinWidth: int;
begin
  Result := Constraints.MinWidth;
end;

procedure TGMForm.WMShowControlPanel(var Msg: TMessage);
begin
  if GetControlFrame() = nil then Exit;

  if (Msg.LParam <= 0) or not GMAggregates.IsObjectControllable(Msg.WParam) then
    GetControlFrame().Visible := false
  else
  begin
    GetControlFrame().Visible := true;
    GetControlFrame().ID_Obj := Msg.WParam;
  end;
end;

end.
