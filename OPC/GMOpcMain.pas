unit GMOpcMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OPCDataThread, GMGlobals, GMConst, XMLDoc, XMLIntf, ExtCtrls,
  StdCtrls, VirtualTrees;

type
  TGMOpcMainForm = class(TForm)
    Panel1: TPanel;
    lState: TLabel;
    vstOpc: TVirtualStringTree;
    Timer1: TTimer;
    btnOpts: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure vstOpcGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure Timer1Timer(Sender: TObject);
    procedure btnOptsClick(Sender: TObject);
    procedure vstOpcPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    tLastRefresh: TDatetime;
    DataThread: TGMOPCDataThread;
    procedure WMUpdateCOMLog(var Msg: TMessage); message WM_UPDATE_COM_LOG;
    procedure WMOpcUpdateChannels(var Msg: TMessage); message WM_OPC_UPDATE_CHANNELS;
    procedure ParseChn(chn: IXMLNode);
    procedure RestartDataThread;
    procedure Init;
    procedure StopDataThread;
    function ShowOptsDlg: bool;
  public
    { Public declarations }
  end;

var
  GMOpcMainForm: TGMOpcMainForm;

implementation

uses GMOPCSrv, DateUtils, Opts, ComLog, AppConfigFile;

{$R *.dfm}

procedure TGMOpcMainForm.Init();
begin
  DataThread := TGMOPCDataThread.Create(Handle);
  lState.Caption := 'Ожидание ответа сервера опроса';
  tLastRefresh := 0;
end;

procedure TGMOpcMainForm.StopDataThread();
begin
  if DataThread <> nil then
  begin
    DataThread.Terminate();
    WaitForSingleObject(DataThread.Handle, 10000);
    TryFreeAndNil(DataThread);
  end;
end;

procedure TGMOpcMainForm.FormCreate(Sender: TObject);
begin
  SetMainConfigFileClass(TGMClientMainConfigFile);
end;

procedure TGMOpcMainForm.FormDestroy(Sender: TObject);
begin
  StopDataThread();
end;

procedure TGMOpcMainForm.ParseChn(chn: IXMLNode);
var c: TChannelItem;
begin
  c := lstChannels.FindOrAdd(chn.Attributes['id']);
  c.Tag := Trim(chn.Attributes['opctag']);
  c.UTime := chn.Attributes['utime'];
  c.Val := MyStrToFloat(chn.Attributes['val']);
end;

procedure TGMOpcMainForm.WMOpcUpdateChannels(var Msg: TMessage);
var xml: IXMLDocument;
    sc: TStringClass;
    chnList, chn: IXMLNode;
begin
  sc := TStringClass(Msg.WParam);
  xml := NewXMLDocument();
  lstChannels.synch.BeginWrite();
  try
    xml.LoadFromXML(sc.s);
    chnList := xml.ChildNodes.FindNode('channels');
    if chnList <> nil then
    begin
      chn := chnList.ChildNodes.First();
      while chn <> nil do
      begin
        if chn.NodeName = 'chn' then
          ParseChn(chn);

        chn := chn.NextSibling();
      end;
    end;
  except end;
  vstOpc.RootNodeCount := lstChannels.Count;
  lstChannels.synch.EndWrite();

  sc.Free();
  tLastRefresh := Now();
  lState.Caption := 'Последний обмен с сервером: ' + DateTimeToStr(tLastRefresh);
end;

procedure TGMOpcMainForm.WMUpdateCOMLog(var Msg: TMessage);
begin
  PostMessage(ComLogFrm.Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TGMOpcMainForm.vstOpcGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  lstChannels.synch.BeginRead();
  try
    if int(Node.Index) < lstChannels.Count then
    begin
      case Column of
        0: CellText := lstChannels[Node.Index].Tag;
        1: begin
             if lstChannels[Node.Index].Utime > 0 then
               CellText := DateTimeToStr(UTCtoLocal(lstChannels[Node.Index].Utime))
             else
               CellText := '???';
           end;
        2: begin
             if lstChannels[Node.Index].Utime > 0 then
               CellText := Format('%10.3f', [lstChannels[Node.Index].Val])
             else
               CellText := '???';
           end;
      end;
    end;
  except end;
end;

procedure TGMOpcMainForm.Timer1Timer(Sender: TObject);
begin
  if Now() - tLastRefresh > OPC_NO_REFRESH_INTERVAL * OneSecond then
    lState.Font.Color := clRed
  else
    lState.Font.Color := clWindowText;

  vstOpc.RootNodeCount := lstChannels.Count;
  vstOpc.Refresh();
end;

procedure TGMOpcMainForm.RestartDataThread();
begin
  StopDataThread();
  lstChannels.Clear();
  vstOpc.RootNodeCount := 0;
  
  Init();
end;

function TGMOpcMainForm.ShowOptsDlg(): bool;
begin
  if OptsDlg = nil then
    Application.CreateForm(TOptsDlg, OptsDlg);

  OptsDlg.Width := OptsDlg.GroupBox1.Width + 2 * OptsDlg.GroupBox1.Left;
  OptsDlg.ShowTimeZone(true);

  Result := OptsDlg.ShowModal() = mrOk;
end;

procedure TGMOpcMainForm.btnOptsClick(Sender: TObject);
begin
  if ShowOptsDlg() then
    RestartDataThread();
end;

procedure TGMOpcMainForm.vstOpcPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if int64(Node.Index) < lstChannels.Count then
  begin
    if Abs(int64(lstChannels[Node.Index].UTime) - NowGM(TimeZone)) > OPC_NO_REFRESH_INTERVAL then
    TargetCanvas.Font.Color := clRed
    else
      TargetCanvas.Font.Color := clWindowText;
  end
  else
    TargetCanvas.Font.Color := clWindowText;
end;

procedure TGMOpcMainForm.FormShow(Sender: TObject);
begin
  if not FileExists(GMMainConfigFile.GetMainINIFileName()) and not ShowOptsDlg() then
  begin
    Application.Terminate();
    Exit;
  end;

  Init();
end;

procedure TGMOpcMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl, ssAlt]) and (Key = VK_F9) then
    ComLogFrm.Show();
end;

end.
