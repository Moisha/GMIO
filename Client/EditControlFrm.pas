unit EditControlFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, EditSmthFrm, StdCtrls, ExtCtrls, ComCtrls, GMGlobals,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage;

type
  TEditControlFrame = class(TEditSomethingFrame)
    cmbControlAggregate: TcxComboBox;
    Label1: TcxLabel;
    leManual: TLabeledEdit;
    leControl: TLabeledEdit;
    pcChannels: TcxPageControl;
    TabSheet1: TcxTabSheet;
    TabSheet2: TcxTabSheet;
    TabSheet3: TcxTabSheet;
    leU1: TLabeledEdit;
    leI1: TLabeledEdit;
    leP1: TLabeledEdit;
    leA1: TLabeledEdit;
    leP12: TLabeledEdit;
    leP22: TLabeledEdit;
    leQ2: TLabeledEdit;
    leT3: TLabeledEdit;
    procedure leManualDblClick(Sender: TObject);
    procedure cmbControlAggregateChange(Sender: TObject);
    procedure leA1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure leA1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure leManualKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    function GetPrefix: string; override;
    procedure LoadFromINI(); override;
    procedure SaveControlToINI(Sender: TObject); override;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

procedure TEditControlFrame.leManualDblClick(Sender: TObject);
begin
  frmTree.FindID_Prm(TCustomEdit(Sender).Tag);
end;

procedure TEditControlFrame.cmbControlAggregateChange(Sender: TObject);
begin
  if cmbControlAggregate.ItemIndex > 0 then
    pcChannels.ActivePageIndex := cmbControlAggregate.ItemIndex - 1;

  SaveControlToINI(Sender);
end;

procedure TEditControlFrame.leA1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := AcceptOnlyParams();
end;

procedure TEditControlFrame.leA1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  AssignParamWithEdit(TEdit(Sender));
end;

function TEditControlFrame.GetPrefix: string;
begin
  Result := 'CONTROL' + IntToStr(FNPrm) + '_';
end;

constructor TEditControlFrame.Create(AOwner: TComponent);
var i: int;
begin
  inherited;

  for i := 0 to pcChannels.PageCount - 1 do
    pcChannels.Pages[i].TabVisible := false;

  pcChannels.ActivePageIndex := 0;
  cmbControlAggregate.ItemIndex := 0;
  FNObj := 0;
  FNPrm := 0;
end;

procedure TEditControlFrame.LoadFromINI;
begin
  if INISection='OBJECT0' then Exit;

  cmbControlAggregate.ItemIndex := fINI.ReadInteger(INISection, Prefix + 'AGGREGATETYPE', 0);

  UpdatePrmEditFromINI(leManual, 'ID_MANUAL');
  UpdatePrmEditFromINI(leControl, 'ID_CONTROL');

  UpdatePrmEditFromINI(leU1, 'ID_U1');
  UpdatePrmEditFromINI(leI1, 'ID_I1');
  UpdatePrmEditFromINI(leP1, 'ID_P1');
  UpdatePrmEditFromINI(leA1, 'ID_A1');

  UpdatePrmEditFromINI(leP12, 'ID_P12');
  UpdatePrmEditFromINI(leP22, 'ID_P22');
  UpdatePrmEditFromINI(leQ2, 'ID_Q2');

  UpdatePrmEditFromINI(leT3, 'ID_T3');
end;

procedure TEditControlFrame.SaveControlToINI(Sender: TObject);
var s, sINI: string;
begin
  if Sender is TCustomEdit then
  begin
    s := IntToStr(TCustomEdit(Sender).Tag);

         if Sender = leManual then sINI := 'ID_MANUAL'
    else if Sender = leControl then sINI := 'ID_CONTROL'
    else if Sender = leU1 then sINI := 'ID_U1'
    else if Sender = leI1 then sINI := 'ID_I1'
    else if Sender = leP1 then sINI := 'ID_P1'
    else if Sender = leA1 then sINI := 'ID_A1'

    else if Sender = leP12 then sINI := 'ID_P12'
    else if Sender = leP22 then sINI := 'ID_P22'
    else if Sender = leQ2 then sINI := 'ID_Q2'

    else if Sender = leT3 then sINI := 'ID_T3';

    fINI.WriteString(INISection, Prefix + sINI, s);
  end;

  if Sender = cmbControlAggregate then
    fINI.WriteInteger(INISection, Prefix + 'AGGREGATETYPE', cmbControlAggregate.ItemIndex);
end;
  
procedure TEditControlFrame.leManualKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ChekDelParam(Sender, Key);
end;

end.
