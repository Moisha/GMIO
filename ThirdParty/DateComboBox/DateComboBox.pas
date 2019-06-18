// (c) Alex Konshin mailto:alexk@msmt.spb.su 05.01.99

unit DateComboBox;

// замена для TDateTimePicker

interface

uses
	SysUtils, Windows, Classes, Messages, Graphics, Controls, ComCtrls, CommCtrl, Types;

type

{ Calendar common control support }

  TCustomCalendar = class;

  ECommonCalendarError = class(Exception);

  TCalendarColors = class(TPersistent)
  private
    Owner: TCustomCalendar;
    FBackColor: TColor;
    FTextColor: TColor;
    FTitleBackColor: TColor;
    FTitleTextColor: TColor;
    FMonthBackColor: TColor;
    FTrailingTextColor: TColor;
    procedure SetColor(Index: Integer; Value: TColor);
    procedure SetAllColors;
  public
    constructor Create(AOwner: TCustomCalendar);
    procedure Assign(Source: TPersistent); override;
  published
    property BackColor: TColor index 0 read FBackColor write SetColor default clWindow;
    property TextColor: TColor index 1 read FTextColor write SetColor default clWindowText;
    property TitleBackColor: TColor index 2 read FTitleBackColor write SetColor default clActiveCaption;
    property TitleTextColor: TColor index 3 read FTitleTextColor write SetColor default clWhite;
    property MonthBackColor: TColor index 4 read FMonthBackColor write SetColor default clWhite;
    property TrailingTextColor: TColor index 5 read FTrailingTextColor
      write SetColor default clInactiveCaptionText;
  end;

  TCalDayOfWeek = (dowMonday, dowTuesday, dowWednesday, dowThursday,
    dowFriday, dowSaturday, dowSunday, dowLocaleDefault);

  TOnGetMonthInfoEvent = procedure(Sender: TObject; Month: LongWord;
    var MonthBoldInfo: LongWord) of object;


  TCustomCalendar = class(TWinControl)
  protected
    FMaxSelectRange: Integer;
    FMonthDelta, FInitMonthDelta: Integer;
    FCalExceptionClass: ExceptClass;
    FEndDate: TDate;
    FMaxDate: TDate;
    FMinDate: TDate;
    FDateTime: TDateTime;
    FOldDateTime: TDateTime;
    FCalColors: TCalendarColors;
    FOnGetMonthInfo: TOnGetMonthInfoEvent;
    FFirstDayOfWeek: TCalDayOfWeek;
    FOnChange: TNotifyEvent;
    FMultiSelect: Boolean;
    FShowToday: Boolean;
    FShowTodayCircle: Boolean;
    FWeekNumbers: Boolean;
    function DoStoreEndDate: Boolean;
    function DoStoreMaxDate: Boolean;
    function DoStoreMinDate: Boolean;
    function GetDate: TDate;
		function GetIsChanged : Boolean; virtual;
		procedure SetInitMonthDelta( Value : Integer ); virtual;
    procedure SetCalColors(Value: TCalendarColors);
    procedure SetDate(Value: TDate); virtual;
    procedure SetDateTime(Value: TDateTime); virtual;
    procedure SetEndDate(Value: TDate);
    procedure SetFirstDayOfWeek(Value: TCalDayOfWeek);
    procedure SetMaxDate(Value: TDate);
    procedure SetMaxSelectRange(Value: Integer);
    procedure SetMinDate(Value: TDate);
    procedure SetMonthDelta(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetRange(MinVal, MaxVal: TDate);
    procedure SetSelectedRange(Date, EndDate: TDate);
    procedure SetShowToday(Value: Boolean);
    procedure SetShowTodayCircle(Value: Boolean);
    procedure SetWeekNumbers(Value: Boolean);
    procedure CheckEmptyDate; virtual;
    procedure CheckValidDate(Value: TDate); virtual;
    procedure Change; virtual;
    procedure ChangeDate(Value: TDate);
    procedure ChangeDateTime(Value: TDateTime);
    procedure CreateWnd; override;
    function GetCalendarHandle: HWND; virtual; abstract;
    function GetCalStyles: DWORD; virtual;
    function MsgSetCalColors(ColorIndex: Integer; ColorValue: TColor): Boolean; virtual; abstract;
    function MsgSetDateTime(Value: TSystemTime): Boolean; virtual; abstract;
    function MsgSetRange(Flags: Integer; SysTime: PSystemTime): Boolean; virtual; abstract;
    property CalColors: TCalendarColors read FCalColors write SetCalColors;
    property CalendarHandle: HWND read GetCalendarHandle;
    property CalExceptionClass: ExceptClass read FCalExceptionClass write FCalExceptionClass;
    property Date: TDate read GetDate write SetDate;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property EndDate: TDate read FEndDate write SetEndDate stored DoStoreEndDate;
    property FirstDayOfWeek: TCalDayOfWeek read FFirstDayOfWeek write SetFirstDayOfWeek default dowLocaleDefault;
    property MaxDate: TDate read FMaxDate write SetMaxDate stored DoStoreMaxDate;
    property MaxSelectRange: Integer read FMaxSelectRange write SetMaxSelectRange default 31;
    property MinDate: TDate read FMinDate write SetMinDate stored DoStoreMinDate;
    property MonthDelta: Integer read FMonthDelta write SetMonthDelta default 1;
		property InitMonthDelta : Integer read FInitMonthDelta write SetInitMonthDelta default 0;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ShowToday: Boolean read FShowToday write SetShowToday default True;
    property ShowTodayCircle: Boolean read FShowTodayCircle write SetShowTodayCircle default True;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default False;
    property OnGetMonthInfo: TOnGetMonthInfoEvent read FOnGetMonthInfo write FOnGetMonthInfo;
		property IsChanged : Boolean read GetIsChanged;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BoldDays(Days: array of LongWord; var MonthBoldInfo: LongWord);
  end;

	{ TDateComboBox }

  EDateTimeError = class(ECommonCalendarError);

{ TDTDateMode = (dmComboBox, dmUpDown);
  TDTCalAlignment = (dtaLeft, dtaRight);}

{  TDTParseInputEvent = procedure(Sender: TObject; const UserString: string; var DateAndTime: TDateTime; var AllowChange: Boolean) of object;}

	TDTPKind = (dtkShortDate,dtkLongDate,dtkTime,dtkShortDateTime,dtkLongDateTime,dtkCustom);

  TDateTimeColors = TCalendarColors;  // for backward compatibility

  TDateComboBox = class(TCustomCalendar)
  protected
		FCustomFormat : String;
		FButtonWidth : LongInt;
    FOnUserInput: TDTParseInputEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FCalAlignment: TDTCalAlignment;
    FDateMode: TDTDateMode;
    FKind: TDTPKind;
    FLastChange: TSystemTime;
    FDroppedDown,FButtonPressed: Boolean;
    FParseInput: Boolean;
    FShowCheckbox: Boolean;
    FChanging: Boolean;
    FChecked: Boolean;
    FOldChecked: Boolean;
		FOnClick: TNotifyEvent;
    procedure AdjustHeight;
    function GetTime: TTime;
    procedure SetCalAlignment(Value: TDTCalAlignment);
    procedure SetChecked(Value: Boolean);
    procedure SetDateMode(Value: TDTDateMode);
		procedure SetCustomFormat( Value : String );
		procedure SetInitMonthDelta( Value : Integer ); override;
    procedure SetKind(Value: TDTPKind);
    procedure SetParseInput(Value: Boolean);
    procedure SetShowCheckbox(Value: Boolean);
    procedure ChangeTime(Value: TTime);
    procedure SetTime(Value: TTime); virtual;
		procedure SetDate(Value: TDate); override;
    procedure SetDateTime(Value: TDateTime); override;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CheckEmptyDate; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function GetCalendarHandle: HWND; override;
    function MsgSetCalColors(ColorIndex: Integer; ColorValue: TColor): Boolean; override;
    function MsgSetDateTime( ASystemTime: TSystemTime ): Boolean; override;
		function MsgSetDateTimeAndChecked( ASystemTime: TSystemTime; const AChecked : Boolean ): Boolean;
    function MsgSetRange(Flags: Integer; SysTime: PSystemTime): Boolean; override;
		procedure DoExit; override;
		function GetIsChanged : Boolean; override;
		function StoreCustomFormat : Boolean;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  public
    constructor Create(AOwner: TComponent); override;
		procedure SetDateTimeAndChecked( ADateTime: TDateTime; const AChecked : Boolean );
    property DateTime;
    property DroppedDown: Boolean read FDroppedDown;
    property Date stored False;
    property Time: TTime read GetTime write SetTime stored False;
  published
    property Anchors;
    property BiDiMode;
    property CalAlignment: TDTCalAlignment read FCalAlignment write SetCalAlignment;
    property CalColors;
    property Constraints;
    // The Date, Time, ShowCheckbox, and Checked properties must be in this order:
    property ShowCheckbox: Boolean read FShowCheckbox write SetShowCheckbox default False;
    property Checked: Boolean read FChecked write SetChecked default True;
    property Color stored True default clWindow;
		property CustomFormat : String read FCustomFormat write SetCustomFormat stored StoreCustomFormat;
    property DateMode: TDTDateMode read FDateMode write SetDateMode;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property Kind: TDTPKind read FKind write SetKind;
    property MaxDate;
    property MinDate;
    property ParseInput: Boolean read FParseInput write SetParseInput;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUserInput: TDTParseInputEvent read FOnUserInput write FOnUserInput;
		property InitMonthDelta;
  end;

procedure Register;

//=============================================================
implementation
//=============================================================

uses
	RtlConsts,
	ComStrs{, StringConv};

const
  ColorIndex: array[0..5] of Integer = (MCSC_BACKGROUND, MCSC_TEXT, MCSC_TITLEBK, MCSC_TITLETEXT, MCSC_MONTHBK, MCSC_TRAILINGTEXT);
	eCustomFormats = [dtkShortDateTime,dtkLongDateTime,dtkCustom];
	sShortDateTimeFormat = 'dd''.''MM''.''yyy  HH'':''mm';
	sLongDateTimeFormat = 'dd MMMM yyy HH'':''mm'':''ss';

//=============================================================
procedure Register;
begin
  RegisterComponents('Additional', [TDateComboBox]);
end;

//=============================================================
procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean);
var Style: Integer;
begin
  if Ctl.HandleAllocated then
  begin
    Style := GetWindowLong(Ctl.Handle, GWL_STYLE);
    if not UseStyle then Style := Style and not Value else Style := Style or Value;
    SetWindowLong(Ctl.Handle, GWL_STYLE, Style);
  end;
end;

//==TCalendarColors===========================================================
constructor TCalendarColors.Create(AOwner: TCustomCalendar);
begin
  Owner := AOwner;
  FBackColor := clWindow;
  FTextColor := clWindowText;
  FTitleBackColor := clActiveCaption;
  FTitleTextColor := clWhite;
  FMonthBackColor := clWhite;
  FTrailingTextColor := clInactiveCaptionText;
end;
//-------------------------------------------------------------
procedure TCalendarColors.Assign(Source: TPersistent);
var SourceName: string;
begin
  if Source = nil then SourceName := 'nil'
  else SourceName := Source.ClassName;
  if (Source = nil) or not (Source is TCalendarColors) then
    raise EConvertError.CreateFmt(SAssignError, [SourceName, ClassName]);
  FBackColor := TCalendarColors(Source).BackColor;
  FTextColor := TCalendarColors(Source).TextColor;
  FTitleBackColor := TCalendarColors(Source).TitleBackColor;
  FTitleTextColor := TCalendarColors(Source).TitleTextColor;
  FMonthBackColor := TCalendarColors(Source).MonthBackColor;
  FTrailingTextColor := TCalendarColors(Source).TrailingTextColor;
end;
//-------------------------------------------------------------
procedure TCalendarColors.SetColor(Index: Integer; Value: TColor);
begin
  case Index of
    0: FBackColor := Value;
    1: FTextColor := Value;
    2: FTitleBackColor := Value;
    3: FTitleTextColor := Value;
    4: FMonthBackColor := Value;
    5: FTrailingTextColor := Value;
  end;
  if Owner.HandleAllocated then
    Owner.MsgSetCalColors(ColorIndex[Index], ColorToRGB(Value));
end;
//-------------------------------------------------------------
procedure TCalendarColors.SetAllColors;
begin
  SetColor(0, FBackColor);
  SetColor(1, FTextColor);
  SetColor(2, FTitleBackColor);
  SetColor(3, FTitleTextColor);
  SetColor(4, FMonthBackColor);
  SetColor(5, FTrailingTextColor);
end;


//==TCustomCalendar===========================================================
constructor TCustomCalendar.Create(AOwner: TComponent);
begin
  CheckCommonControl(ICC_DATE_CLASSES);
  inherited Create(AOwner);
  FShowToday := True;
  FShowTodayCircle := True;
  ControlStyle := [csOpaque, csClickEvents, csDoubleClicks, csReflector];
  FCalColors := TDateTimeColors.Create(Self);
  FDateTime := Now;
  FOldDateTime := FDateTime;
  FFirstDayOfWeek := dowLocaleDefault;
  FMaxSelectRange := 31;
  FMonthDelta := 1;
//	FInitMonthDelta := 0;
end;
//-------------------------------------------------------------
destructor TCustomCalendar.Destroy;
begin
  inherited Destroy;
  FCalColors.Free;
end;
//-------------------------------------------------------------
procedure TCustomCalendar.BoldDays(Days: array of LongWord; var MonthBoldInfo: LongWord);
var I: LongWord;
begin
  MonthBoldInfo := 0;
  for I := Low(Days) to High(Days) do
    if (Days[I] > 0) and (Days[I] < 32) then
      MonthBoldInfo := MonthBoldInfo or ($00000001 shl (Days[I] - 1));
end;
//-------------------------------------------------------------
procedure TCustomCalendar.CheckEmptyDate;
begin
  // do nothing
end;
//-------------------------------------------------------------
procedure TCustomCalendar.CheckValidDate(Value: TDate);
begin
  if (FMaxDate <> 0.0) and (Value > FMaxDate) then
    raise CalExceptionClass.CreateFmt(SDateTimeMax, [DateToStr(FMaxDate)]);
  if (FMinDate <> 0.0) and (Value < FMinDate) then
    raise CalExceptionClass.CreateFmt(SDateTimeMin, [DateToStr(FMinDate)]);
end;
//-------------------------------------------------------------
procedure TCustomCalendar.CreateWnd;
begin
  inherited CreateWnd;
  FCalColors.SetAllColors;
  SetRange(FMinDate, FMaxDate);
  SetMaxSelectRange(FMaxSelectRange);
  SetFirstDayOfWeek(FFirstDayOfWeek);
	if not (Self is TDateComboBox) then
		begin
			if FMultiSelect then SetSelectedRange(FDateTime, FEndDate) else SetDateTime(FDateTime);
  		SetMonthDelta(FMonthDelta);
		end
	else if FMultiSelect then SetSelectedRange(FDateTime, FEndDate);
end;
//-------------------------------------------------------------
procedure TCustomCalendar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;
//-------------------------------------------------------------
procedure TCustomCalendar.ChangeDate(Value: TDate);
var TruncValue: TDate;
begin
  TruncValue := Trunc(Value);
	if Trunc(FDateTime)=TruncValue then Exit;
  Value := TruncValue + Frac(FDateTime);
  if Value = 0.0 then CheckEmptyDate;
  try
    CheckValidDate(TruncValue);
    SetDateTime(Value);
  except
    SetDateTime(FDateTime);
    raise;
  end;
	Changed;
end;
//-------------------------------------------------------------
procedure TCustomCalendar.ChangeDateTime(Value: TDateTime);
var ST: TSystemTime;
begin
  if FMultiSelect then
		begin
			SetSelectedRange(Value,FEndDate);
			FOldDateTime := 0.0;
		end
  else if FDateTime<>Value then
		begin
			CheckValidDate(Value);
    	if HandleAllocated then
			begin
  			DateTimeToSystemTime(Value,ST);
				if not MsgSetDateTime(ST) then raise ECommonCalendarError.Create(sFailSetCalDateTime);
			end;
    	FDateTime := Value;
		end;
	Changed;
end;
//-------------------------------------------------------------
function TCustomCalendar.DoStoreEndDate: Boolean;
begin
  Result := FMultiSelect;
end;
//-------------------------------------------------------------
function TCustomCalendar.DoStoreMaxDate: Boolean;
begin
  Result := FMaxDate <> 0.0;
end;
//-------------------------------------------------------------
function TCustomCalendar.DoStoreMinDate: Boolean;
begin
  Result := FMinDate <> 0.0;
end;
//-------------------------------------------------------------
function TCustomCalendar.GetCalStyles: DWORD;
const
  ShowTodayFlags: array[Boolean] of DWORD = (MCS_NOTODAY, 0);
  ShowTodayCircleFlags: array[Boolean] of DWORD = (MCS_NOTODAYCIRCLE, 0);
  WeekNumFlags: array[Boolean] of DWORD = (0, MCS_WEEKNUMBERS);
  MultiSelFlags: array[Boolean] of DWORD = (0, MCS_MULTISELECT);
begin
  Result := MCS_DAYSTATE or ShowTodayFlags[FShowToday] or
    ShowTodayCircleFlags[FShowTodayCircle] or WeekNumFlags[FWeekNumbers] or
    MultiSelFlags[FMultiSelect];
end;
//-------------------------------------------------------------
function TCustomCalendar.GetIsChanged : Boolean;
begin
	Result := FOldDateTime<>FDateTime;
end;
//-------------------------------------------------------------
function TCustomCalendar.GetDate: TDate;
begin
  Result := TDate(FDateTime);
end;
//-------------------------------------------------------------
procedure TCustomCalendar.SetDate(Value: TDate);
var TruncValue: TDate;
begin
  TruncValue := Trunc(Value);
	if Trunc(FDateTime)=TruncValue then Exit;
  Value := TruncValue + Frac(FDateTime);
  if Value = 0.0 then CheckEmptyDate;
  try
    CheckValidDate(TruncValue);
    SetDateTime(Value);
  except
    SetDateTime(FDateTime);
    raise;
  end;
	FOldDateTime := Value;
end;
//-------------------------------------------------------------
procedure TCustomCalendar.SetDateTime(Value: TDateTime);
var ST: TSystemTime;
begin
  if FMultiSelect then SetSelectedRange(Value, FEndDate)
  else if FDateTime<>Value then
		begin
    	if HandleAllocated then
			begin
  			DateTimeToSystemTime(Value,ST);
				if not MsgSetDateTime(ST) then raise ECommonCalendarError.Create(sFailSetCalDateTime);
			end;
    	FDateTime := Value;
			FOldDateTime := Value;
		end;
end;
//-------------------------------------------------------------
procedure TCustomCalendar.SetInitMonthDelta( Value : Integer );
begin
//	if FInitMonthDelta=Value then Exit;
	FInitMonthDelta := Value;
	if Value<>0 then SetDateTime(IncMonth(Now,Value));
end; {TCustomCalendar.SetInitMonthDelta}
//-------------------------------------------------------------
procedure TCustomCalendar.SetCalColors(Value: TDateTimeColors);
begin
  if FCalColors <> Value then FCalColors.Assign(Value);
end;
//-------------------------------------------------------------
procedure TCustomCalendar.SetEndDate(Value: TDate);
var
  TruncValue: TDate;
begin
  TruncValue := Trunc(Value);
  if Trunc(FEndDate) <> TruncValue then
  begin
    Value := TruncValue + 0.0;
    if Value = 0.0 then CheckEmptyDate;
    CheckValidDate(TruncValue);
    SetSelectedRange(Date, TruncValue);
  end;
end;
//-------------------------------------------------------------
procedure TCustomCalendar.SetFirstDayOfWeek(Value: TCalDayOfWeek);
var
  DOWFlag: Integer;
  A: array[0..1] of char;
begin
  if HandleAllocated then
  begin
    if Value = dowLocaleDefault then
    begin
      GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, A, SizeOf(A));
      DOWFlag := Ord(A[0]) - Ord('0');
    end
    else
      DOWFlag := Ord(Value);
    if CalendarHandle <> 0 then
      MonthCal_SetFirstDayOfWeek(CalendarHandle, DOWFlag);
  end;
  FFirstDayOfWeek := Value;
end;
//-------------------------------------------------------------
procedure TCustomCalendar.SetMaxDate(Value: TDate);
begin
  if (FMinDate <> 0.0) and (Value < FMinDate) then
    raise CalExceptionClass.CreateFmt(SDateTimeMin, [DateToStr(FMinDate)]);
  if FMaxDate <> Value then
  begin
    SetRange(FMinDate, Value);
    FMaxDate := Value;
  end;
end;

procedure TCustomCalendar.SetMaxSelectRange(Value: Integer);
begin
  if FMultiSelect and HandleAllocated then
    if not MonthCal_SetMaxSelCount(CalendarHandle, Value) then
      raise ECommonCalendarError.Create(sFailSetCalMaxSelRange);
  FMaxSelectRange := Value;
end;

procedure TCustomCalendar.SetMinDate(Value: TDate);
begin
  if (FMaxDate <> 0.0) and (Value > FMaxDate) then
    raise CalExceptionClass.CreateFmt(SDateTimeMax, [DateToStr(FMaxDate)]);
  if FMinDate <> Value then
  begin
    SetRange(Value, FMaxDate);
    FMinDate := Value;
  end;
end;
//-------------------------------------------------------------
procedure TCustomCalendar.SetMonthDelta( Value: Integer );
begin
	if FMonthDelta<>Value then
	begin
  	if HandleAllocated and (CalendarHandle<>0) then MonthCal_SetMonthDelta(CalendarHandle,Value);
  	FMonthDelta := Value;
	end;
end;

procedure TCustomCalendar.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if Value then FEndDate := FDateTime else FEndDate := 0.0;
    RecreateWnd;
  end;
end;

procedure TCustomCalendar.SetRange(MinVal, MaxVal: TDate);
var
  STA: packed array[1..2] of TSystemTime;
  Flags: DWORD;
  TruncDate, TruncEnd, TruncMin, TruncMax: Int64;
begin
  Flags := 0;
  TruncMin := Trunc(MinVal);
  TruncMax := Trunc(MaxVal);
  TruncDate := Trunc(FDateTime);
  TruncEnd := Trunc(FEndDate);
  if TruncMin <> 0 then
  begin
    if TruncDate < TruncMin then ChangeDate(MinVal);
    if TruncEnd < TruncMin then SetEndDate(MinVal);
    Flags := Flags or GDTR_MIN;
    DateTimeToSystemTime(MinVal, STA[1]);
  end;
  if TruncMax <> 0 then
  begin
    if TruncDate > TruncMax then ChangeDate(MaxVal);
    if TruncEnd > TruncMax then SetEndDate(MaxVal);
    Flags := Flags or GDTR_MAX;
    DateTimeToSystemTime(MaxVal, STA[2]);
  end;
  if HandleAllocated then
    if not MsgSetRange(Flags, @STA[1]) then
      raise ECommonCalendarError.Create(sFailSetCalMinMaxRange);
end;

procedure TCustomCalendar.SetSelectedRange(Date, EndDate: TDate);
var
  DateArray: array[1..2] of TSystemTime;
begin
  if not FMultiSelect then SetDateTime(Date)
  else
		begin
			DateTimeToSystemTime(Date, DateArray[1]);
			DateTimeToSystemTime(EndDate, DateArray[2]);
			if HandleAllocated then
  			if not MonthCal_SetSelRange(Handle, @DateArray[1]) then
    			raise ECommonCalendarError.Create(sFailsetCalSelRange);
			FDateTime := Date;
			FEndDate := EndDate;
  	end;
end;

procedure TCustomCalendar.SetShowToday(Value: Boolean);
begin
  if FShowToday <> Value then
  begin
    FShowToday := Value;
    SetComCtlStyle(Self, MCS_NOTODAY, not Value);
  end;
end;

procedure TCustomCalendar.SetShowTodayCircle(Value: Boolean);
begin
  if FShowTodayCircle <> Value then
  begin
    FShowTodayCircle := Value;
    SetComCtlStyle(Self, MCS_NOTODAYCIRCLE, not Value);
  end;
end;

procedure TCustomCalendar.SetWeekNumbers(Value: Boolean);
begin
  if FWeekNumbers <> Value then
  begin
    FWeekNumbers := Value;
    SetComCtlStyle(Self, MCS_WEEKNUMBERS, Value);
  end;
end;

function IsBlankSysTime(const ST: TSystemTime): Boolean;
begin
  with ST do
    Result := (wYear = 0) and (wMonth = 0) and (wDayOfWeek = 0) and
      (wDay = 0) and (wHour = 0) and (wMinute = 0) and (wSecond = 0) and
      (wMilliseconds = 0);
end;


//==TDateComboBox===========================================================
constructor TDateComboBox.Create(AOwner: TComponent);
begin
  FCalExceptionClass := EDateTimeError;
  FChanging := False;
  inherited Create(AOwner);
  DateTimeToSystemTime(FDateTime,FLastChange);
  FShowCheckbox := False;
  FChecked := True;
  ControlStyle := ControlStyle + [csFixedHeight, csReflector];
  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  Width := 186;
  AdjustHeight;
end;
//-------------------------------------------------------------
procedure TDateComboBox.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
  Height := Metrics.tmHeight + (GetSystemMetrics(SM_CYBORDER) * 8);
end;
//-------------------------------------------------------------
procedure TDateComboBox.CheckEmptyDate;
begin
  if not FShowCheckbox then raise EDateTimeError.Create(SNeedAllowNone);
  FChecked := False;
  Invalidate;
end;
//-------------------------------------------------------------
procedure TDateComboBox.CreateParams(var Params: TCreateParams);
const
	aStyles: array[TDTPKind] of DWORD = (DTS_SHORTDATEFORMAT,DTS_LONGDATEFORMAT,DTS_TIMEFORMAT,DTS_SHORTDATEFORMAT,DTS_LONGDATEFORMAT,0);
var ACalAlignment: TDTCalAlignment;
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, DATETIMEPICK_CLASS);
  with Params do
  begin
    if FDateMode = dmUpDown then Style := Style or DTS_UPDOWN;
		if (FKind=dtkCustom)and(FCustomFormat='') then FKind := dtkShortDate;
		if FKind=dtkShortDateTime then FCustomFormat := sShortDateTimeFormat
		else if FKind=dtkLongDateTime then FCustomFormat := sLongDateTimeFormat
		else Style := Style or aStyles[FKind];
		ACalAlignment := FCalAlignment;
    if UseRightToLeftAlignment then
      if ACalAlignment = dtaLeft then ACalAlignment := dtaRight else ACalAlignment := dtaLeft;
    if ACalAlignment = dtaRight then Style := Style or DTS_RIGHTALIGN;
    if FParseInput then Style := Style or DTS_APPCANPARSE;
    if FShowCheckbox then Style := Style or DTS_SHOWNONE;
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.CreateWnd;
begin
  inherited CreateWnd;
	if (FKind in eCustomFormats)and(FCustomFormat<>'') then DateTime_SetFormat(Handle,PChar(FCustomFormat));
	if FInitMonthDelta<>0 then
	begin
		FDateTime := IncMonth(Now,FInitMonthDelta);
		FOldDateTime := FDateTime;
	end;
	DateTimeToSystemTime(FDateTime,FLastChange);
	if not MsgSetDateTimeAndChecked( FLastChange, FChecked or not FShowCheckbox ) then raise ECommonCalendarError.Create(sFailSetCalDateTime);
	FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
	FButtonPressed := False;
//	Invalidate;
end;
//-------------------------------------------------------------
procedure TDateComboBox.CMColorChanged(var Message: TMessage);
begin
  inherited;
  InvalidateRect(Handle, nil, True);
end;
//-------------------------------------------------------------
procedure TDateComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustHeight;
  InvalidateRect(Handle, nil, True);
end;
//-------------------------------------------------------------
procedure TDateComboBox.CNNotify(var Message: TWMNotify);
var dtNew : TDateTime;
		bAllowChange: Boolean;
begin
  with Message, NMHdr^ do
  begin
    Result := 0;
    case code of
      DTN_CLOSEUP:
        begin
          FDroppedDown := False;
					FButtonPressed := False;
//          SetDate(SystemTimeToDateTime(FLastChange));
          if Assigned(FOnCloseUp) then FOnCloseUp(Self);
          if Assigned(FOnClick) and ((FDateTime<>FOldDateTime)or(FShowCheckbox and not FChecked)) then
						begin
							FChecked := True;
							FOnClick(Self);
							FOldChecked := True;
							FOldDateTime := FDateTime;
						end
					else FChecked := True;
        end;
      DTN_DATETIMECHANGE:
        begin
          with PNMDateTimeChange(NMHdr)^ do
          begin
            if FDroppedDown and (dwFlags = GDT_VALID) then
          		begin
            		FLastChange := st;
            		FDateTime := SystemTimeToDateTime(FLastChange);
            		Change;
          		end
            else
            	if (dwFlags = GDT_NONE) or IsBlankSysTime(st) then
								if FShowCheckbox and FChecked and Assigned(FOnClick) then
									begin
										FChecked := False;
										FOnClick(Self);
										FOldChecked := False;
										FOldDateTime := FDateTime;
									end
								else FChecked := False
            	else if dwFlags = GDT_VALID then
            		begin
              		FLastChange := st;
              		dtNew := SystemTimeToDateTime(st);
									if not FButtonPressed and FShowCheckbox and not FChecked and Assigned(FOnClick) then
										begin
											if dtNew<>FDateTime then
              					if (FKind=dtkShortDate)or(FKind=dtkLongDate) then FDateTime := Trunc(dtNew)+Frac(FDateTime)
												else if FKind=dtkTime then FDateTime := Trunc(FDateTime)+Frac(dtNew)
												else FDateTime := dtNew;
											FChecked := True;
											FOnClick(Self);
											FOldChecked := True;
											FOldDateTime := dtNew;
										end
									else if dtNew<>FDateTime then
										begin
              				if (FKind=dtkShortDate)or(FKind=dtkLongDate) then FDateTime := Trunc(dtNew)+Frac(FDateTime)
											else if FKind=dtkTime then FDateTime := Trunc(FDateTime)+Frac(dtNew)
											else FDateTime := dtNew;
											FChecked := True;
											Change;
										end
									else FChecked := not FButtonPressed;
            		end;
          end;
        end;
      DTN_DROPDOWN:
        begin
          DateTimeToSystemTime(Date,FLastChange);
          FDroppedDown := True;
					FButtonPressed := False;
          if Assigned(FOnDropDown) then FOnDropDown(Self);
        end;
      DTN_USERSTRING:
        begin
          bAllowChange := Assigned(FOnUserInput);
          with PNMDateTimeString(NMHdr)^ do
          begin
            if bAllowChange then
            begin
              dtNew := 0.0;
              FOnUserInput(Self, pszUserString, dtNew, bAllowChange);
              DateTimeToSystemTime(dtNew, st);
            end;
            dwFlags := Ord(not bAllowChange);
          end;
        end;
    else
      inherited;
    end;
  end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
	FButtonPressed := PtInRect( Rect(ClientWidth-FButtonWidth,0,ClientWidth,ClientHeight), Point(Message.XPos,Message.YPos) );
	inherited;
end;
//-------------------------------------------------------------
function TDateComboBox.GetCalendarHandle: HWND;
begin
  Result := DateTime_GetMonthCal(Handle);
end;
//-------------------------------------------------------------
function TDateComboBox.GetTime: TTime;
begin
  Result := TTime(FDateTime);
end;
//-------------------------------------------------------------
function TDateComboBox.MsgSetCalColors(ColorIndex: Integer; ColorValue: TColor): Boolean;
begin
  Result := True;
  if HandleAllocated then
    Result := DateTime_SetMonthCalColor(Handle, ColorIndex, ColorValue) <> DWORD($FFFFFFFF);
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetDateTimeAndChecked( ADateTime: TDateTime; const AChecked : Boolean );
begin
  if not FShowCheckbox then raise EDateTimeError.Create(SNeedAllowNone);
	if (AChecked<>FChecked)or(AChecked and (FDateTime=ADateTime)) then
	begin
		FDateTime := ADateTime;
 		if HandleAllocated then
  	begin
			DateTimeToSystemTime(FDateTime,FLastChange);
			if not MsgSetDateTimeAndChecked(FLastChange,AChecked) then raise ECommonCalendarError.Create(sFailSetCalDateTime);
    	Invalidate;
  	end;
		FOldDateTime := FDateTime;
  	FChecked := AChecked;
		FOldChecked := AChecked;
	end;
end; {TDateComboBox.SetDateTimeAndChecked}
//-------------------------------------------------------------
function TDateComboBox.MsgSetDateTime( ASystemTime: TSystemTime{; const AChecked : Boolean} ): Boolean;
begin
  Result := MsgSetDateTimeAndChecked( ASystemTime, True );
end;
//-------------------------------------------------------------
function TDateComboBox.MsgSetDateTimeAndChecked( ASystemTime: TSystemTime; const AChecked : Boolean ): Boolean;
const aGDT_Flag : array [Boolean] of DWORD = (GDT_NONE,GDT_VALID);
begin
  Result := True;
  if HandleAllocated then
    if not FChanging then
    begin
      FChanging := True;
      try
        Result := DateTime_SetSystemTime(Handle,aGDT_Flag[AChecked],ASystemTime);
      finally
        FChanging := False;
      end;
    end;
end;
//-------------------------------------------------------------
function TDateComboBox.MsgSetRange(Flags: Integer; SysTime: PSystemTime): Boolean;
begin
  Result := True;
  if HandleAllocated then
    if Flags <> 0 then Result := DateTime_SetRange(Handle, Flags, SysTime);
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetCalAlignment(Value: TDTCalAlignment);
begin
  if FCalAlignment <> Value then
  begin
    FCalAlignment := Value;
    if not (csDesigning in ComponentState) then
      SetComCtlStyle(Self, DTS_RIGHTALIGN, Value = dtaRight);
  end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetChecked(Value: Boolean);
begin
	if FChecked=Value then Exit;
 	if FShowCheckbox and HandleAllocated then
  begin
		DateTimeToSystemTime(FDateTime,FLastChange);
		if not MsgSetDateTimeAndChecked(FLastChange,Value) then raise ECommonCalendarError.Create(sFailSetCalDateTime);
    Invalidate;
  end;
  FChecked := Value;
	FOldChecked := Value;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetCustomFormat( Value : String );
begin
	if FCustomFormat<>Value then
	begin
		FCustomFormat := Value;
		if Value<>'' then FKind := dtkCustom;
    RecreateWnd;
	end;
end; {TDateComboBox.SetCustomFormat}
//-------------------------------------------------------------
function TDateComboBox.StoreCustomFormat : Boolean;
begin
	Result := FKind=dtkCustom;
end; {TDateComboBox.StoreCustomFormat}
//-------------------------------------------------------------
procedure TDateComboBox.SetDateMode(Value: TDTDateMode);
begin
  if FDateMode <> Value then
  begin
    FDateMode := Value;
    RecreateWnd;
  end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetKind(Value: TDTPKind);
//	TDTPKind = (dtkShortDate,dtkLongDate,dtkTime,dtkShortDateTime,dtkLongDateTime,dtkCustom);
const
	aStyles: array[TDTPKind] of DWORD = (DTS_SHORTDATEFORMAT,DTS_LONGDATEFORMAT,DTS_TIMEFORMAT,DTS_SHORTDATEFORMAT,DTS_LONGDATEFORMAT,0);
	DropFormats = not(DTS_SHORTDATEFORMAT or DTS_LONGDATEFORMAT or DTS_TIMEFORMAT);
var Style: DWORD;
begin
	if (Value=dtkCustom)and(FCustomFormat='') then Value := dtkShortDate;
  if FKind <> Value then
	begin
		if Value=dtkShortDateTime then FCustomFormat := sShortDateTimeFormat
		else if Value=dtkLongDateTime then FCustomFormat := sLongDateTimeFormat;
		if HandleAllocated then
		begin
			if Value in eCustomFormats then DateTime_SetFormat(Handle,PChar(FCustomFormat))
  		else if not (FKind in eCustomFormats) then
  			begin
    			Style := GetWindowLong(Handle,GWL_STYLE) and DropFormats;
    			SetWindowLong(Handle,GWL_STYLE,Style or aStyles[Value]);
  			end
			else
				begin
					FKind := Value;
    			RecreateWnd;
					Exit;
				end;
		end;
		FKind := Value;
	end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetParseInput(Value: Boolean);
begin
  if FParseInput <> Value then
  begin
    FParseInput := Value;
    if not (csDesigning in ComponentState) then SetComCtlStyle(Self, DTS_APPCANPARSE, Value);
  end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetShowCheckbox(Value: Boolean);
begin
  if FShowCheckbox <> Value then
  begin
    FShowCheckbox := Value;
    RecreateWnd;
  end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetInitMonthDelta( Value : Integer );
var dt : TDateTime;
begin
//	if FInitMonthDelta=Value then Exit;
	FInitMonthDelta := Value;
	if Value=0 then dt := Now else dt := IncMonth(Now,Value);
	if ShowCheckbox then SetDateTimeAndChecked(dt,FChecked) else SetDateTime(dt);
end; {TDateComboBox.SetInitMonthDelta}
//-------------------------------------------------------------
procedure TDateComboBox.SetDate(Value: TDate);
var TruncValue: TDate;
begin
  TruncValue := Trunc(Value);
	if (Trunc(FDateTime)=TruncValue)and FChecked then Exit;
  Value := TruncValue + Frac(FDateTime);
  if Value = 0.0 then CheckEmptyDate;
  try
    CheckValidDate(TruncValue);
    SetDateTime(Value);
  except
    SetDateTime(FDateTime);
    raise;
  end;
	FOldDateTime := Value;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetDateTime(Value: TDateTime);
begin
  if FMultiSelect then SetSelectedRange(Value, FEndDate)
  else if (FDateTime<>Value)or not FChecked then
		begin
			FChecked := True;
			FOldChecked := True;
    	if HandleAllocated then
			begin
  			DateTimeToSystemTime(Value,FLastChange);
				if not MsgSetDateTimeAndChecked(FLastChange,True) then raise ECommonCalendarError.Create(sFailSetCalDateTime);
				FChecked := True;
				FOldChecked := True;
				Invalidate;
			end;
    	FDateTime := Value;
			FOldDateTime := Value;
		end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.SetTime(Value: TTime);
begin
  if (Frac(FDateTime)<>Frac(Value))or not Checked then
  begin
    Value := Trunc(FDateTime) + Frac(Value);
    if Value = 0.0 then
  		begin
    		if not FShowCheckbox then raise EDateTimeError.Create(SNeedAllowNone);
    		Checked := False;
  		end
    else SetDateTime(Value);
  end;
end;
//-------------------------------------------------------------
function TDateComboBox.GetIsChanged : Boolean;
begin
	Result := (FOldChecked<>FChecked)or(FOldDateTime<>FDateTime);
end;
//-------------------------------------------------------------
procedure TDateComboBox.ChangeTime(Value: TTime);
begin
  if (Frac(FDateTime)<>Frac(Value))or not Checked then
  begin
    Value := Trunc(FDateTime) + Frac(Value);
    if Value = 0.0 then
  		begin
    		if not FShowCheckbox then raise EDateTimeError.Create(SNeedAllowNone);
    		Checked := False;
  		end
    else ChangeDateTime(Value);
  end;
end;
//-------------------------------------------------------------
procedure TDateComboBox.DoExit;
begin
	if Assigned(FOnClick) and not FMultiSelect and IsChanged then	FOnClick(Self);
	inherited;
	FOldDateTime := FDateTime;
	FOldChecked := FChecked;
end;

end.