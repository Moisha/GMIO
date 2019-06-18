////////////////////////////////////////////
// Настройки связи с сервером
////////////////////////////////////////////
unit Opts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Spin, IniFiles, Math, GMGlobals,
  ConfigXml,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus;

type
  TOptsDlg = class(TGMEnhancedScrollForm)
    btOk: TcxButton;
    btCancel: TcxButton;
    GroupBox1: TcxGroupBox;
    Label1: TcxLabel;
    seDiagramTime: TcxSpinEdit;
    Label7: TcxLabel;
    seAlarmsHeight: TcxSpinEdit;
    Label8: TcxLabel;
    seNoRefreshTimeout: TcxSpinEdit;
    Label9: TcxLabel;
    cmbReqMethod: TcxComboBox;
    GroupBox2: TcxGroupBox;
    leSrvPort: TcxTextEdit;
    leSrv: TcxTextEdit;
    leLogin: TcxTextEdit;
    lePassword: TcxTextEdit;
    pnTimeZone: TcxGroupBox;
    Label10: TcxLabel;
    cmbTimeZone: TcxComboBox;
    leTimeOut: TcxTextEdit;
    eTimeout2: TcxTextEdit;
    Label2: TcxLabel;
    Label3: TcxLabel;
    Label4: TcxLabel;
    Label5: TcxLabel;
    Label6: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure lePasswordChange(Sender: TObject);
  private
    FPasswordChanged: bool;
    function ReadConfig: IGMConfigConfigType;
    { Private declarations }
  public
    { Public declarations }
    procedure ShowTimeZone(bShow: bool = true);
    function ShowModal(): TModalResult; reintroduce;
  end;

var
  OptsDlg: TOptsDlg;

implementation

uses TimeZone, AppConfigFile, StrUtils;

{$R *.dfm}

procedure ReadWinCOMs(TS: TStrings);       // поищем порты в реестре виндов
var h: HKEY;
    s: string;
    a, b:array[0..200] of char;
    lena, lenb: cardinal;
    i, k, tp: DWORD;
    n, m:integer;
begin
  TS.Clear();

  try
    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'HARDWARE\DEVICEMAP\SERIALCOMM', 0, KEY_ENUMERATE_SUB_KEYS or KEY_QUERY_VALUE, h) = ERROR_SUCCESS then
    begin
      i:=0;
      while 1<2 do
      begin
        lena := 200;
        lenb := 200;
        k := RegEnumValue(h, i, @a, lena, nil, @tp, @b, @lenb);
        if (k <> ERROR_SUCCESS) then break;

        Inc(i);
        if (lenb > 0) then
        begin
          s := Trim(UpperCase(Copy(b, 0, lenb)));
          if (Length(s) > 3) and (Pos('COM', s) = 1) then
          try
            n := StrToInt(Copy(s, 4, Length(s)-3));
            m := 0;
            while m < TS.Count do
              if Integer(TS.Objects[m]) > n then
                break
              else
                Inc(m);

            TS.InsertObject(m, s, Pointer(n));
          except
          end;
        end;
      end;

      RegCloseKey(h);
    end;
  except
  end;

  if TS.Count = 0 then
    for i := 1 to 40 do
      TS.AddObject('COM'+IntToStr(i), Pointer(i));

  TS.Insert(0, '< Нет >');
end;

procedure TOptsDlg.FormCreate(Sender: TObject);
begin
  ReadRegTimeZones(cmbTimeZone.Properties.Items);
end;

procedure SetCmbItemIndexWithDef(cmb: TcxComboBox; MainIdx, DefIdx: int);
begin
  if MainIdx in [0..cmb.Properties.Items.Count] then
    cmb.ItemIndex := MainIdx
  else
    cmb.ItemIndex := DefIdx;
end;

procedure TOptsDlg.lePasswordChange(Sender: TObject);
begin
  FPasswordChanged := true;
end;

function TOptsDlg.ReadConfig(): IGMConfigConfigType;
begin
  if FileExists(GMMainConfigFile.GetMainINIFileName()) then
  begin
    Result := ReadConfigXML();

    // TCP
    leSrv.Text := Result.Common.Host;
    leSrvPort.Text := IntToStr(Result.Common.Port);
    leTimeout.Text := IntToStr(Result.Common.Timeout);
    eTimeout2.Text := IntToStr(Result.Common.Timeout2);
    leLogin.Text := Result.Common.Login;
    lePassword.Text := IfThen(Result.Common.Password = '', '', '***');

    // Opts
    seDiagramTime.Value := Result.Common.Diagram_time;
    seAlarmsHeight.Value := Result.Common.Alarm_window_height;
    seNoRefreshTimeout.Value := Result.Common.Norefresh_alarm;
    SetCmbItemIndexWithDef(cmbReqMethod, Result.Common.Req_system, 0);
    SetCmbItemIndexWithDef(cmbTimeZone, Result.Common.Timezone, 6);
  end
  else
  begin
    Result := Newconfig();
  end;
end;

function TOptsDlg.ShowModal: TModalResult;
var xml: IGMConfigConfigType;
begin
  xml := ReadConfig();
  FPasswordChanged := false;

  Result := inherited ShowModal();
  
  if Result = mrOk then
  begin
    // TCP
    xml.Common.Host := leSrv.Text;
    xml.Common.Port := StrToInt(leSrvPort.Text);
    if Trim(leTimeOut.Text) <> '' then
      xml.Common.Timeout := StrToInt(leTimeOut.Text);
    if Trim(eTimeOut2.Text) <> '' then
      xml.Common.Timeout2 := StrToInt(eTimeOut2.Text);
    xml.Common.Login := Trim(leLogin.Text);
    if FPasswordChanged then
      xml.Common.Password := IfThen(lePassword.Text = '', '', CalcMD5(lePassword.Text));

    // Opts
    xml.Common.Diagram_time := seDiagramTime.Value;
    xml.Common.Alarm_window_height := seAlarmsHeight.Value;
    xml.Common.Norefresh_alarm := seNoRefreshTimeout.Value;
    xml.Common.Req_system := cmbReqMethod.ItemIndex;
    xml.Common.Timezone := CmbSelectedObj(cmbTimeZone);

    SaveConfigXML(xml);
  end;
end;

procedure TOptsDlg.ShowTimeZone(bShow: bool);
begin
  pnTimeZone.Visible := bShow;
end;

end.
