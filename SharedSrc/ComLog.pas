/// /////////////////////////////////////////
// Журнал запросов по СОМ-порту
/// /////////////////////////////////////////
unit ComLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GMGlobals, StdCtrls, Clipbrd, GMConst, ExtCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxScrollBox,
  dxSkinsCore, cxLabel, cxTextEdit, cxMaskEdit, cxSplitter, cxColorComboBox, cxMemo, cxProgressBar,
  cxDropDownEdit, cxCalendar, dxSkinscxPCPainter, dxBarBuiltInMenu, cxPC, cxTimeEdit, cxRadioGroup,
  cxGroupBox, cxSpinEdit, cxCheckBox, cxButtonEdit, cxButtons, cxCheckListBox, cxListBox, cxListView,
  cxImage, Vcl.Menus;

type
  TComLogFrm = class(TForm)
    mCOMLog: TcxMemo;
    Panel1: TcxGroupBox;
    btnCopy: TcxButton;
    Timer1: TTimer;
    procedure mCOMLogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCopyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    slBuffer: TSTringList;
    tLastRefrMemo, tLastRefrBuffer: int64;
    synch: TMultiReadExclusiveWriteSynchronizer;
    procedure WMUpdateCOMLog(var Msg: TMessage); message WM_UPDATE_COM_LOG;
  public
    { Public declarations }
  end;

var
  ComLogFrm: TComLogFrm;

implementation

{$R *.dfm}

uses ProgramLogFile;

{ TComLogFrm }

procedure TComLogFrm.WMUpdateCOMLog(var Msg: TMessage);
var
  s: string;
begin
  synch.BeginWrite();
  try
    while slBuffer.Count > 1000 do
      slBuffer.Delete(0);

    try
      s := FormatDateTime('hh:nn:ss.zzz ', Now()) + #9 + TStringClass(Msg.WParam).s;
    except
      on e: Exception do
        ProgramLog.AddException('WMUpdateCOMLog - ' + e.Message);
    end;
    slBuffer.Add(s);
  finally
    synch.EndWrite();
    try
      TStringClass(Msg.WParam).Free();
    except
    end;
    tLastRefrBuffer := GetTickCount();
  end;
end;

procedure TComLogFrm.mCOMLogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl, ssAlt]) and (Key = Ord('C')) then
    Clipboard().SetTextBuf(PChar(mCOMLog.Text));
end;

procedure TComLogFrm.btnCopyClick(Sender: TObject);
begin
  mCOMLog.SelectAll();
  mCOMLog.CopyToClipboard();
end;

procedure TComLogFrm.FormCreate(Sender: TObject);
begin
  slBuffer := TSTringList.Create();
  tLastRefrMemo := 0;
  tLastRefrBuffer := 0;
  synch := TMultiReadExclusiveWriteSynchronizer.Create();
end;

procedure TComLogFrm.FormDestroy(Sender: TObject);
begin
  slBuffer.Free();
  synch.Free();
end;

procedure TComLogFrm.Timer1Timer(Sender: TObject);
begin
  if not Visible or (tLastRefrMemo > tLastRefrBuffer) then
    Exit;

  synch.BeginRead();
  try
    tLastRefrMemo := GetTickCount();
    mCOMLog.Lines.Assign(slBuffer);
  finally
    synch.EndRead();
  end;

  mCOMLog.SelStart := mCOMLog.Perform(EM_LINEINDEX, mCOMLog.Lines.Count, 0);
  mCOMLog.Perform(EM_SCROLLCARET, 0, 0);
end;

end.
