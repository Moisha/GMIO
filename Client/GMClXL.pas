////////////////////////////////////////////
// Таблицы XLS, отчет по данным
////////////////////////////////////////////
unit GMClXL;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, GMGlobals, INIFiles, Spin,
  ComCtrls, Frame.ObjectTree, VirtualTrees, GMDBClasses, Frame.ReportBlank,
  GMConst,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  TFrmXL = class(TGMEnhancedScrollForm)
    frmXLS: TFrmReportBlank;
    cbDaySumm: TcxCheckBox;
    procedure frmXLSbtGoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    procedure WMRefreshObjectsTree(var Msg: TMessage); message WM_REFRESH_OBJECTS_TREE;
  public
    { Public declarations }
    function ShowModal: TModalResult; reintroduce;
  end;

var FrmXL: TFrmXL;

implementation

uses Threads.GMClient, DateUtils, Math, ComObj, GMClMain;

{$R *.dfm}

function TFrmXL.ShowModal: TModalResult;
begin
  if frmXLS.frmTree.Tree.RootNode.ChildCount = 0 then
    frmXLS.frmTree.Init([NODE_LEVEL_PARAM]);

  frmXLS.frmTree.ShowObjects(fotAll);
  Result:=inherited ShowModal();
end;

procedure TFrmXL.WMRefreshObjectsTree(var Msg: TMessage);
begin
  frmXLS.frmTree.Init([NODE_LEVEL_PARAM]);
end;

procedure TFrmXL.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not frmXLS.InReport;
end;

procedure TFrmXL.frmXLSbtGoClick(Sender: TObject);
var rep: TReportData;
begin
  rep := frmXLS.CreateReportData();

  if cbDaySumm.Checked then
    rep.Options := [roDaySumm];

  frmXLS.frmTree.GetCheckedParamsList(rep.lstChannels);
  try
    if frmXLS.CommonCheck(rep.lstChannels) then
    begin
      frmXLS.StartReport();
      frmXLS.ProcessCommonReport(rep);
    end;
  finally
    rep.Free();
    frmXLS.FinishReport();
  end;
end;


end.
