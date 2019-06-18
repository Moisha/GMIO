////////////////////////////////////////////
// Вспомогательные компоненты для настройки управления
////////////////////////////////////////////
unit Frame.EditAggrControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, GMGlobals, DB, ADODB,
  GMSqlQuery, Frame.ControlOneAddPrm;

type
  TEditAggrFrame = class(TFrame)
    pcMain: TPageControl;
    tsCommon: TTabSheet;
    Label1: TLabel;
    cmbControlAggregate: TComboBox;
    leManual: TLabeledEdit;
    leControl: TLabeledEdit;
    pcChannels: TPageControl;
    TabSheet2: TTabSheet;
    leU1: TLabeledEdit;
    leI1: TLabeledEdit;
    leP1: TLabeledEdit;
    leA1: TLabeledEdit;
    TabSheet3: TTabSheet;
    leP12: TLabeledEdit;
    leP22: TLabeledEdit;
    leQ2: TLabeledEdit;
    TabSheet4: TTabSheet;
    leT3: TLabeledEdit;
    leName: TLabeledEdit;
    tsAddParams: TTabSheet;
    ControlOneAddPrmFrame1: TControlOneAddPrmFrame;
    ControlOneAddPrmFrame2: TControlOneAddPrmFrame;
    ControlOneAddPrmFrame3: TControlOneAddPrmFrame;
    lePreset: TLabeledEdit;
    leResetAlarm: TLabeledEdit;
    procedure leManualKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure leManualDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure leManualDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cmbControlAggregateChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure leNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure leNameExit(Sender: TObject);
    procedure ControlOneAddPrmFrame1eCaptionExit(Sender: TObject);
  private
    { Private declarations }
    q: TGMSqlQuery;

    procedure ChekDelParam(Sender: TObject; Key: Word);
    function GetKind(le: TObject): int;
    procedure SetID_Prm(ID_Kind, ID_Prm: int);
    function GetCaptionByKind(ID_Kind: int): string;
    function LookForLabeledEdit(Parent: TWinControl; ID_Kind: int): TLabeledEdit;
    procedure SetAggrName(const s: string);
  public
    { Public declarations }
    ID_Aggr: int;
    OnGetSelectedParam: procedure (var ID_Prm: int) of object;
    OnGetTextForPrm: function(ID_Prm: int): string of object;

    procedure LoadFromSQL();
    constructor Create(ASource: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses GMConst, StrUtils;

{$R *.dfm}

function TEditAggrFrame.GetCaptionByKind(ID_Kind: int): string;
begin
  Result := '';

  case ID_Kind of
    CONTROL_PRM_ENG_I: Result := 'I, A';
    CONTROL_PRM_ENG_U: Result := 'U, B';
    CONTROL_PRM_ENG_P: Result := 'P, кВт';
    CONTROL_PRM_ENG_A: Result := 'А, мч';

    CONTROL_PRM_VALVE_P1: Result := 'P1, атм';
    CONTROL_PRM_VALVE_P2: Result := 'P2, атм';
    CONTROL_PRM_VALVE_Q: Result := 'Q, м3/ч';

    CONTROL_PRM_HEAT_T: Result := 'T, C';

    CONTROL_PRM_MANUAL: Result := 'Ручн.';
    CONTROL_PRM_RUN: Result := 'Старт';

    CONTROL_PRM_ADD1: Result := ControlOneAddPrmFrame1.eCaption.Text;
    CONTROL_PRM_ADD2: Result := ControlOneAddPrmFrame2.eCaption.Text;
    CONTROL_PRM_ADD3: Result := ControlOneAddPrmFrame3.eCaption.Text;

    CONTROL_PRM_RESETALARM: Result := 'Сброс аварии';

    CONTROL_PRM_PRESET: Result := 'Уставка';
  end;
end;

function TEditAggrFrame.GetKind(le: TObject): int;
begin
  Result := 0;

  if le = leManual then Result := CONTROL_PRM_MANUAL;
  if le = leControl then Result := CONTROL_PRM_RUN;
  if le = lePreset then Result := CONTROL_PRM_PRESET;
  if le = leResetAlarm then Result := CONTROL_PRM_RESETALARM;

  if le = leI1 then Result := CONTROL_PRM_ENG_I;
  if le = leU1 then Result := CONTROL_PRM_ENG_U;
  if le = leP1 then Result := CONTROL_PRM_ENG_P;
  if le = leA1 then Result := CONTROL_PRM_ENG_A;

  if le = leP12 then Result := CONTROL_PRM_VALVE_P1;
  if le = leP22 then Result := CONTROL_PRM_VALVE_P2;
  if le = leQ2 then Result := CONTROL_PRM_VALVE_Q;

  if le = leT3 then Result := CONTROL_PRM_HEAT_T;

  // Дополнительные
  if le = ControlOneAddPrmFrame1.lePrm then Result := CONTROL_PRM_ADD1;
  if le = ControlOneAddPrmFrame2.lePrm then Result := CONTROL_PRM_ADD2;
  if le = ControlOneAddPrmFrame3.lePrm then Result := CONTROL_PRM_ADD3;

  if le = ControlOneAddPrmFrame1.eCaption then Result := CONTROL_PRM_ADD1;
  if le = ControlOneAddPrmFrame2.eCaption then Result := CONTROL_PRM_ADD2;
  if le = ControlOneAddPrmFrame3.eCaption then Result := CONTROL_PRM_ADD3;
end;

function TEditAggrFrame.LookForLabeledEdit(Parent: TWinControl; ID_Kind: int): TLabeledEdit;
var i: int;
    c: TControl;
begin
  Result := nil;
  for i := 0 to Parent.ControlCount - 1 do
  begin
    c := Parent.Controls[i];
    if (c is TLabeledEdit) and (GetKind(c) = ID_Kind) then
      Result := TLabeledEdit(c)
    else
    if c is TWinControl then
      Result := LookForLabeledEdit(TWinControl(c), ID_Kind);

    if Result <> nil then Exit;
  end;
end;

procedure TEditAggrFrame.LoadFromSQL();
var i: int;
    le: TLabeledEdit;
    n: int;
begin
  for i := 0 to CONTROL_PRM_MAX do
  begin
    le := LookForLabeledEdit(self, i);
    if le <> nil then
    begin
      le.Text := '';
      le.Tag := 0;
    end;
  end;

  ControlOneAddPrmFrame1.eCaption.Text := '';
  ControlOneAddPrmFrame2.eCaption.Text := '';
  ControlOneAddPrmFrame3.eCaption.Text := '';

  cmbControlAggregate.ItemIndex := 0;
  q.Close();
  q.SQL.Text := 'select * from Aggregates where ID_Aggregate = ' + IntToStr(ID_Aggr);
  q.Open();
  if not q.Eof then
  begin
    n := q.FieldByName('ID_AggrType').AsInteger;

    if n = AGGREGATE_TYPE_PARAM_HOLDER then
    begin
      pcMain.ActivePage := tsAddParams;
    end
    else
    begin
      pcMain.ActivePage := tsCommon;

      if (n >= 0) and (n < cmbControlAggregate.Items.Count) then
        cmbControlAggregate.ItemIndex := n
      else
        cmbControlAggregate.ItemIndex := 0;
    end;

    leName.Text := q.FieldByName('Name').AsString;
  end;

  q.Close();
  q.SQL.Text := 'select * from AggregateParams where ID_Aggregate = ' + IntToStr(ID_Aggr);
  q.Open();
  while not q.Eof do
  begin
    le := LookForLabeledEdit(self, q.FieldByName('PrmKind').AsInteger);
    if le <> nil then
    begin
      le.Tag := q.FieldByName('ID_Prm').AsInteger;
        le.Text := OnGetTextForPrm(le.Tag);

      if q.FieldByName('PrmKind').AsInteger >= CONTROL_PRM_ADD1 then
          TControlOneAddPrmFrame(le.Parent.Parent).eCaption.Text := q.FieldByName('Caption').AsString;
    end;

    q.Next();
  end;

  q.Close();
end;

procedure TEditAggrFrame.SetID_Prm(ID_Kind, ID_Prm: int);
begin
  q.Close();
  q.SQL.Text := Format(' do $$ ' +
                       ' declare _id int; ' +
                       ' begin ' +
                       ' select ID_AggregatePrm into _id from AggregateParams ' +
                       '  where ID_Aggregate = %d and PrmKind = %d; ' +
                       ' if _id is null then' +
                       '   insert into AggregateParams ( ID_Aggregate, Caption, ID_Prm, PrmKind ) ' +
                       '     select %d, ''%s'', %d, %d ;',
                       [ ID_Aggr, ID_Kind,
                         ID_Aggr, GetCaptionByKind(ID_Kind), ID_Prm, ID_Kind
                       ] );

  q.SQL.Add(' else ');
  if ID_Prm >= 0 then // задаем или убиваем парметр
    q.SQL.Add(Format( ' update AggregateParams set ID_Prm = %s, Caption = %s where ID_AggregatePrm = _id;',
                      [IfThen(ID_Prm = 0, 'null', IntToStr(ID_Prm)), QuotedStr(GetCaptionByKind(ID_Kind))]) )
  else
    q.SQL.Add(Format( ' update AggregateParams set Caption = %s where ID_AggregatePrm = _id;',
                          [QuotedStr(GetCaptionByKind(ID_Kind))]) );

  q.SQL.Add(' end if; ');
  q.SQL.Add(' end $$; ');

  q.ExecSQL();
end;

procedure TEditAggrFrame.ChekDelParam(Sender: TObject; Key: Word);
begin
  if Key = VK_DELETE then
  begin
    TCustomEdit(Sender).Text := '';
    SetID_Prm(GetKind(Sender), 0);
  end;
end;

constructor TEditAggrFrame.Create(ASource: TComponent);
begin
  inherited;

  q := TGMSqlQuery.Create();

  PageControl_HideAllTabs(pcChannels);
  PageControl_HideAllTabs(pcMain);

  ControlOneAddPrmFrame1.ControlPrmType := CONTROL_PRM_ADD1;
  ControlOneAddPrmFrame2.ControlPrmType := CONTROL_PRM_ADD2;
  ControlOneAddPrmFrame3.ControlPrmType := CONTROL_PRM_ADD3;
end;

procedure TEditAggrFrame.leManualKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ChekDelParam(Sender, Key);
end;

procedure TEditAggrFrame.leManualDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TEditAggrFrame.leManualDragDrop(Sender, Source: TObject; X, Y: Integer);
var ID_Prm: int;
begin
  OnGetSelectedParam(ID_Prm);
  TLabeledEdit(Sender).Tag := ID_Prm;
  TLabeledEdit(Sender).Text := OnGetTextForPrm(ID_Prm);

  SetID_Prm(GetKind(Sender), ID_Prm);
end;

procedure TEditAggrFrame.cmbControlAggregateChange(Sender: TObject);
begin
  if cmbControlAggregate.ItemIndex > 0 then
    pcChannels.ActivePageIndex := cmbControlAggregate.ItemIndex - 1;

  q.Close();

  q.SQL.Text := 'update Aggregates set ID_AggrType = ' + IntToStr(cmbControlAggregate.ItemIndex) +
                ' where ID_Aggregate = ' + IntToStr(ID_Aggr);
  q.ExecSQL();
end;

procedure TEditAggrFrame.FrameResize(Sender: TObject);
var i: int;
    le: TLabeledEdit;
begin
  for i := 0 to CONTROL_PRM_MAX do
  begin
    le := LookForLabeledEdit(self, i);
    if le <> nil then
      le.Width := le.Parent.Width - le.Left - 4;
  end;
end;

procedure TEditAggrFrame.SetAggrName(const s: string);
begin
  q.Close();
  q.SQL.Text := 'update Aggregates set Name = ''' + leName.Text +''' where ID_Aggregate = ' + IntToStr(ID_Aggr);
  q.ExecSQL();
end;

procedure TEditAggrFrame.leNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    SetAggrName(leName.Text);
end;

procedure TEditAggrFrame.leNameExit(Sender: TObject);
begin
  SetAggrName(leName.Text);
end;

procedure TEditAggrFrame.ControlOneAddPrmFrame1eCaptionExit(
  Sender: TObject);
begin
  SetID_Prm(GetKind(Sender), -1);
end;

destructor TEditAggrFrame.Destroy;
begin
  q.Free();
  
  inherited;
end;

end.
