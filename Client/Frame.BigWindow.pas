unit Frame.BigWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Frame.DiagramAndTable, GMGlobals, GMConst, Frame.CurrentsDiagramBase,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, dxSkinOffice2010Silver;

type
  TBigWindowFrame = class(TCurrentsDiagramBaseFrame)
    pdDiagramLen: TcxGroupBox;
    rbDiagramLenHrs: TcxRadioGroup;
    DiagramAndTable: TFrmDiagramAndTable;
    tmrRequestGraph: TTimer;
    cxLabel1: TcxLabel;
    cmbTimeStep: TcxComboBox;
    procedure rbDiagramLenHrsClick(Sender: TObject);
    procedure tmrRequestGraphTimer(Sender: TObject);
    procedure cmbTimeStepPropertiesChange(Sender: TObject);
  private
    { Private declarations }
    tcLastUpdate: int64;
    procedure SetActiveBigWindow(n: int);
    function GetActiveBigWindow: int;
  protected
    function RealHandle(): HWND; override;
  public
    { Public declarations }
    property BigWindowIndex: int read GetActiveBigWindow write SetActiveBigWindow;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses BigWindowData, Threads.GMClient;

procedure TBigWindowFrame.cmbTimeStepPropertiesChange(Sender: TObject);
begin
  DiagramAndTable.TblTimeStep := StrToInt(Trim(cmbTimeStep.Text)) * 60;
end;

constructor TBigWindowFrame.Create(AOwner: TComponent);
begin
  inherited;
  tcLastUpdate := 0;
end;

function TBigWindowFrame.GetActiveBigWindow: int;
begin
  Result := BigWindowList.IndexOf(DiagramAndTable.ActiveBW);
end;

procedure TBigWindowFrame.rbDiagramLenHrsClick(Sender: TObject);
begin
  case rbDiagramLenHrs.ItemIndex of
    0: DiagramAndTable.DiagramLenHrs := 1;
    1: DiagramAndTable.DiagramLenHrs := 3;
    2: DiagramAndTable.DiagramLenHrs := 8;
    3: DiagramAndTable.DiagramLenHrs := 12;
    4: DiagramAndTable.DiagramLenHrs := 24;
  end;
end;

function TBigWindowFrame.RealHandle: HWND;
begin
  Result := DiagramAndTable.Handle;
end;

procedure TBigWindowFrame.SetActiveBigWindow(n: int);
begin
  if (n < 0) or (n >= BigWindowList.Count) then Exit;

  DiagramAndTable.ActiveBW := BigWindowList[n];
end;

procedure TBigWindowFrame.tmrRequestGraphTimer(Sender: TObject);
begin
  if not HandleAllocated() or (Abs(tcLastUpdate - GetTickCount()) < 20000) or (TDataOrders.QueueForHandle(RealHandle()) > 0) then
    Exit;

  tcLastUpdate := GetTickCount();
  DiagramAndTable.ClearArchivesOnRequest := false;
  RequestDiagrams();
end;

end.
