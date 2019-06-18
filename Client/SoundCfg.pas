unit SoundCfg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ConfigXml, StdCtrls, ExtCtrls, Math,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus;

type
  TSoundCfgDlg = class(TForm)
    Button2: TcxButton;
    Button3: TcxButton;
    odMP3: TOpenDialog;
    GroupBox1: TcxGroupBox;
    cbNeedAlarm: TcxCheckBox;
    cbNeedNodata: TcxCheckBox;
    leFilename: TcxTextEdit;
    Button1: TcxButton;
    GroupBox2: TcxGroupBox;
    leBlinkingPanelWidth: TcxTextEdit;
    Label1: TcxLabel;
    Label2: TcxLabel;
    procedure Button1Click(Sender: TObject);
  private
    procedure ObjectToControls(xml: IGMConfigConfigType);
    procedure ControlsToObject(xml: IGMConfigConfigType);
    { Private declarations }
  public
    { Public declarations }
    function ShowModal(): TModalResult; reintroduce;
  end;

var
  SoundCfgDlg: TSoundCfgDlg;

implementation

{$R *.dfm}

procedure TSoundCfgDlg.ObjectToControls(xml: IGMConfigConfigType);
begin
  cbNeedAlarm.Checked := xml.Sound.NeedAlarmSound > 0;
  cbNeedNodata.Checked := xml.Sound.NeedNoDataSound > 0;
  leFilename.Text := xml.Sound.AlarmSoundFile;
  leBlinkingPanelWidth.Text := IntToStr(xml.Sound.BlinkingPanelWidth);
end;

procedure TSoundCfgDlg.ControlsToObject(xml: IGMConfigConfigType);
begin
  xml.Sound.NeedAlarmSound := IfThen(cbNeedAlarm.Checked, 1, 0);
  xml.Sound.NeedNoDataSound := IfThen(cbNeedNodata.Checked, 1, 0);
  xml.Sound.AlarmSoundFile := leFilename.Text;
  xml.Sound.BlinkingPanelWidth := StrToIntDef(leBlinkingPanelWidth.Text, 0);
end;

function TSoundCfgDlg.ShowModal(): TModalResult;
var xml: IGMConfigConfigType;
begin
  xml := ReadConfigXML();
  ObjectToControls(xml);

  Result := inherited ShowModal();
  if Result = mrOk then
  begin
    ControlsToObject(xml);
    SaveConfigXML(xml);
  end;
end;

procedure TSoundCfgDlg.Button1Click(Sender: TObject);
begin
  odMP3.InitialDir := ExtractFileDir(leFilename.Text);
  if odMP3.InitialDir = '' then
    odMP3.InitialDir := ExtractFileDir(Application.ExeName);

  odMP3.FileName := ExtractFileName(leFilename.Text);

  if odMP3.Execute() then
  begin
    leFilename.Text := odMP3.FileName;
  end;
end;

end.
