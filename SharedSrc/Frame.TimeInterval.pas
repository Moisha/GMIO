unit Frame.TimeInterval;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateComboBox, Buttons, GMGlobals,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus, Vcl.ComCtrls, dxCore, cxDateUtils, dxSkinOffice2010Silver;

type
  TfrmSelArchTime = class(TFrame)
    dcbDate1: TcxDateEdit;
    Label2: TcxLabel;
    dcbDate2: TcxDateEdit;
    cmbTimeStamp: TcxComboBox;
    Label1: TcxLabel;
    bPrev: TcxButton;
    bNext: TcxButton;
    procedure dcbDate1Change(Sender: TObject);
    procedure bPrevClick(Sender: TObject);
  private
    { Private declarations }
    function GetD1(): TDateTime;
    function GetD2(): TDateTime;
    procedure SetD1(const Value: TDateTime);
    procedure SetD2(const Value: TDateTime);

    procedure DoDataChanged;
  public
    { Public declarations }
    OnDateChanged: TNotifyEvent;

    property D1: TDateTime read GetD1 write SetD1;
    property D2: TDateTime read GetD2 write SetD2;

    procedure DelInterval(Interval: int);
    function GetInterval(): int;
    procedure SetIntervalList(intrv: SetOfInt);

    constructor Create(AOwner: TComponent); override;
  end;

const
  SELINTERVAL_OPEN = 0;
  SELINTERVAL_HOUR = 1;
  SELINTERVAL_DAY = 2;
  SELINTERVAL_WEEK = 3;
  SELINTERVAL_MONTH = 4;
  SELINTERVAL_QUARTER = 5;
  SELINTERVAL_HALFYEAR = 6;
  SELINTERVAL_YEAR = 7;
  SELINTERVAL_ALWAYS = 8;

  SELINTERVAL_ALL: SetOfInt = [SELINTERVAL_OPEN .. SELINTERVAL_ALWAYS];

implementation

uses Math, DateUtils;

{$R *.dfm}

function GetFirstDayOfMonth(T: TDateTime): TDateTime;
var
  D, M, Y: WORD;
begin
  DecodeDate(T, Y, M, D);
  Result := EncodeDate(Y, M, 1);
end;

constructor TfrmSelArchTime.Create(AOwner: TComponent);
begin
  inherited;

  dcbDate1.Date := Floor(Now());
  dcbDate2.Date := dcbDate1.Date + 1;

  SetIntervalList([SELINTERVAL_OPEN, SELINTERVAL_DAY, SELINTERVAL_WEEK, SELINTERVAL_MONTH, SELINTERVAL_QUARTER,
    SELINTERVAL_HALFYEAR, SELINTERVAL_YEAR, SELINTERVAL_ALWAYS]);

  cmbTimeStamp.ItemIndex := 1;
end;

procedure TfrmSelArchTime.dcbDate1Change(Sender: TObject);
var
  D: TDateTime;
  DD, MM, YY: WORD;
begin
  dcbDate1.Enabled := (CmbSelectedObj(cmbTimeStamp) <> SELINTERVAL_ALWAYS);
  dcbDate2.Enabled := (CmbSelectedObj(cmbTimeStamp) <> SELINTERVAL_ALWAYS);

  if Sender is TDateComboBox then
    D := TDateComboBox(Sender).DateTime
  else
    D := dcbDate1.Date;

  case CmbSelectedObj(cmbTimeStamp) of
    SELINTERVAL_OPEN:
      begin // указанный период
        if (Sender = dcbDate1) and (D > dcbDate2.Date) then
          dcbDate2.Date := D + 1;

        if (Sender = dcbDate2) and (D < dcbDate1.Date) then
          dcbDate1.Date := D - 1;
      end;
    SELINTERVAL_HOUR:
      begin // День
        if Sender = dcbDate2 then
        begin
          dcbDate2.Date := Ceil(D / OneHour) * OneHour;
          dcbDate1.Date := dcbDate2.Date - OneHour;
        end
        else
        begin
          dcbDate1.Date := Floor(D / OneHour) * OneHour;
          dcbDate2.Date := dcbDate1.Date + OneHour;
        end;
      end;
    SELINTERVAL_DAY:
      begin // День
        dcbDate1.Date := Math.Floor(D);
        dcbDate2.Date := dcbDate1.Date + 1;
      end;
    SELINTERVAL_WEEK:
      begin // Неделя
        dcbDate1.Date := Math.Floor(D) - DayOfTheWeek(D) + 1;
        dcbDate2.Date := dcbDate1.Date + 7;
      end;
    SELINTERVAL_MONTH:
      begin // Месяц
        dcbDate1.Date := GetFirstDayOfMonth(D);
        dcbDate2.Date := IncMonth(dcbDate1.Date);
      end;
    SELINTERVAL_QUARTER:
      begin // Квартал
        DecodeDate(D, YY, MM, DD);
        dcbDate1.Date := EncodeDate(YY, 3 * ((MM - 1) div 3) + 1, 1);
        dcbDate2.Date := IncMonth(dcbDate1.Date, 3);
      end;
    SELINTERVAL_HALFYEAR:
      begin // Полгода
        DecodeDate(D, YY, MM, DD);
        if MM < 7 then
          MM := 1
        else
          MM := 7;
        dcbDate1.Date := EncodeDate(YY, MM, 1);
        dcbDate2.Date := IncMonth(dcbDate1.Date, 6);
      end;
    SELINTERVAL_YEAR:
      begin // Год
        DecodeDate(D, YY, MM, DD);
        dcbDate1.Date := EncodeDate(YY, 1, 1);
        dcbDate2.Date := EncodeDate(YY + 1, 1, 1);
      end;
  end;

  DoDataChanged();
end;

function TfrmSelArchTime.GetD1: TDateTime;
begin
  if CmbSelectedObj(cmbTimeStamp) <> SELINTERVAL_ALWAYS then
    Result := dcbDate1.Date
  else
    Result := EncodeDate(1970, 1, 1);

  if not(CmbSelectedObj(cmbTimeStamp) in [SELINTERVAL_OPEN, SELINTERVAL_HOUR]) then
    Result := Floor(Result);
end;

function TfrmSelArchTime.GetD2: TDateTime;
begin
  if CmbSelectedObj(cmbTimeStamp) <> SELINTERVAL_ALWAYS then
    Result := dcbDate2.Date
  else
    Result := EncodeDate(2050, 1, 1);

  if not(CmbSelectedObj(cmbTimeStamp) in [SELINTERVAL_OPEN, SELINTERVAL_HOUR]) then
    Result := Floor(Result);
end;

procedure TfrmSelArchTime.bPrevClick(Sender: TObject);
var
  D: int;
  diff: double;
begin
  D := IfThen(Sender = bPrev, -1, 1);

  case CmbSelectedObj(cmbTimeStamp) of
    SELINTERVAL_OPEN:
      begin
        diff := D * (D2 - D1);
        dcbDate1.Date := dcbDate1.Date + diff;
        dcbDate2.Date := dcbDate1.Date + diff;
      end;
    SELINTERVAL_DAY:
      dcbDate1.Date := dcbDate1.Date + D;
    SELINTERVAL_WEEK:
      dcbDate1.Date := dcbDate1.Date + D * 7;
    SELINTERVAL_MONTH:
      dcbDate1.Date := IncMonth(dcbDate1.Date, D);
    SELINTERVAL_QUARTER:
      dcbDate1.Date := IncMonth(dcbDate1.Date, D * 3);
    SELINTERVAL_HALFYEAR:
      dcbDate1.Date := IncMonth(dcbDate1.Date, D * 6);
    SELINTERVAL_YEAR:
      dcbDate1.Date := IncMonth(dcbDate1.Date, D * 12);
  end;

  if Assigned(cmbTimeStamp.Properties.OnChange) then
    cmbTimeStamp.Properties.OnChange(cmbTimeStamp);
end;

procedure TfrmSelArchTime.DelInterval(Interval: int);
var
  n: int;
begin
  n := cmbTimeStamp.Properties.Items.IndexOfObject(TObject(Interval));
  if n >= 0 then
    cmbTimeStamp.Properties.Items.Delete(n);
end;

function TfrmSelArchTime.GetInterval: int;
begin
  Result := CmbSelectedObj(cmbTimeStamp);
end;

procedure TfrmSelArchTime.SetIntervalList(intrv: SetOfInt);
var
  n, n1: int;
begin
  n := GetInterval();

  cmbTimeStamp.Properties.Items.Clear();
  if (SELINTERVAL_OPEN in intrv) or (intrv = []) then
    cmbTimeStamp.Properties.Items.AddObject('Указать ...', TObject(SELINTERVAL_OPEN));
  if SELINTERVAL_DAY in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Час', TObject(SELINTERVAL_HOUR));
  if SELINTERVAL_DAY in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Сутки', TObject(SELINTERVAL_DAY));
  if SELINTERVAL_WEEK in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Неделя', TObject(SELINTERVAL_WEEK));
  if SELINTERVAL_MONTH in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Месяц', TObject(SELINTERVAL_MONTH));
  if SELINTERVAL_QUARTER in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Квартал', TObject(SELINTERVAL_QUARTER));
  if SELINTERVAL_HALFYEAR in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Полгода', TObject(SELINTERVAL_HALFYEAR));
  if SELINTERVAL_YEAR in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Год', TObject(SELINTERVAL_YEAR));
  if SELINTERVAL_ALWAYS in intrv then
    cmbTimeStamp.Properties.Items.AddObject('Всегда', TObject(SELINTERVAL_ALWAYS));

  n1 := cmbTimeStamp.Properties.Items.IndexOfObject(TObject(n));
  if n1 >= 0 then
    cmbTimeStamp.ItemIndex := n1
  else
    cmbTimeStamp.ItemIndex := 0;

  if Assigned(cmbTimeStamp.Properties.OnChange) then
    cmbTimeStamp.Properties.OnChange(cmbTimeStamp);
end;

procedure TfrmSelArchTime.SetD1(const Value: TDateTime);
begin
  dcbDate1.Date := Value;
  dcbDate1Change(dcbDate1);
end;

procedure TfrmSelArchTime.SetD2(const Value: TDateTime);
begin
  dcbDate2.Date := Value;
  dcbDate1Change(dcbDate2);
end;

procedure TfrmSelArchTime.DoDataChanged;
begin
  if Assigned(OnDateChanged) then
    OnDateChanged(self);
end;

end.
