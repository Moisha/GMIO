unit TemplatedRep;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Frame.ReportBlank, ConfigXml, GMGlobals, GMClXL,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  TTemplatedRepDlg = class(TGMEnhancedScrollForm)
    lbReports: TcxListBox;
    frmRep: TFrmReportBlank;
    procedure frmRepbtGoClick(Sender: TObject);
    procedure frmRepdtpDTChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbReportsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    xml: IGMConfigConfigType;
    procedure ReadReportChnList(rep: TReportData);
  public
    { Public declarations }
    function ShowModal: TModalResult; reintroduce;
  end;

var
  TemplatedRepDlg: TTemplatedRepDlg;

implementation

uses Math, GMConst, GMDBClasses;

{$R *.dfm}

function TTemplatedRepDlg.ShowModal: TModalResult;
var i: int;
    s: string;
begin
  frmRep.HidePeriod();
  frmRep.frmTree.bHighLightObjects := false;
  frmRep.frmTree.Visible := false;

  xml := ReadConfigXML();

  s := '';
  if lbReports.ItemIndex >= 0 then
    s := lbReports.Items[lbReports.ItemIndex];

  lbReports.Items.Clear();
  for i := 0 to xml.Reports.Count - 1 do
    lbReports.Items.Add(xml.Reports[i].Name);

  lbReports.ItemIndex := Max(0, lbReports.Items.IndexOf(s));
  lbReportsClick(nil);

  Result := inherited ShowModal();

  xml := nil;
end;

procedure TTemplatedRepDlg.ReadReportChnList(rep: TReportData);
var i, n: int;
    prm: TGMBaseParam;
begin
  n := lbReports.ItemIndex;
  if n < 0 then Exit;

  for i := 0 to xml.Reports[n].ReportColumns.Count - 1 do
  begin
    prm := ParamOrNodeChannel(xml.Reports[n].ReportColumns[i].DataChnType, xml.Reports[n].ReportColumns[i].ID_Prm);
    if (prm <> nil) and (rep.lstChannels.IndexOf(prm) < 0) then
      rep.lstChannels.Add(prm);
  end;
end;

procedure TTemplatedRepDlg.frmRepbtGoClick(Sender: TObject);
var rep: TReportData;
begin
  if lbReports.ItemIndex < 0 then Exit;

  rep := frmRep.CreateReportData();
  rep.Template := xml.Reports[lbReports.ItemIndex].Template;
  rep.Options := [roTemplate];

  rep.PeriodLen := 1;
  case xml.Reports[lbReports.ItemIndex].DataGroup of
    REPORTLENGTH_USER_SEFINE:
      begin
        rep.PeriodType := rptMin;
        rep.PeriodLen := Max(xml.Reports[lbReports.ItemIndex].DataGroupLen, 1);
      end;
    REPORTLENGTH_HALFHOUR:
      begin
        rep.PeriodType := rptHour;
        rep.PeriodLen := 30;
      end;
    REPORTLENGTH_HOUR:
      rep.PeriodType := rptHour;
    REPORTLENGTH_MONTH:
      rep.PeriodType := rptMonth;
    else
      rep.PeriodType := rptDay;
  end;

  rep.DataType := xml.Reports[lbReports.ItemIndex].DataType;

  try
    ReadReportChnList(rep);
    if frmRep.CommonCheck(rep.lstChannels) then
    begin
      frmRep.StartReport();
      frmRep.ProcessCommonReport(rep);
    end;
  finally
    rep.Free();
    frmRep.FinishReport();
  end;
end;

procedure TTemplatedRepDlg.frmRepdtpDTChange(Sender: TObject);
var dl: int;
begin
  if (lbReports.ItemIndex < 0) or (lbReports.ItemIndex >= xml.Reports.Count) then Exit;

  dl := xml.Reports[lbReports.ItemIndex].DataLength;
  if dl < 0 then
    dl := REPORTLENGTH_DAY;

  frmRep.EnableOpenInterval(dl = REPORTLENGTH_USER_SEFINE);

  frmRep.SetDataLength(dl);
end;

procedure TTemplatedRepDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not frmRep.InReport;
end;

procedure TTemplatedRepDlg.FormCreate(Sender: TObject);
begin
  frmRep.cmbPeriod.ItemIndex := 0;
  frmRep.cmbPeriod.Visible := false;
  frmRep.dtpDT2.Enabled := false;
end;

procedure TTemplatedRepDlg.lbReportsClick(Sender: TObject);
begin
  frmRepdtpDTChange(nil);
end;

end.
