unit GMTermMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, GMGlobals, IniFiles, Devices.ModbusBase, GMConst,
  Vcl.ComCtrls, Math, Devices.Tecon.Common, Connection.Base, Connection.COM, WinSock,
  Connection.TCP, Connection.UDP, Threads.Base, StrUtils, Devices.Logica.Base, System.Win.ScktComp;

type
  TListenComThread = class(TGMThread)
  protected
    FConnectionObject: TConnectionObjectBase;
    procedure SafeExecute(); override;
    constructor Create();
  end;

  TForm1 = class(TForm)
    btGo: TButton;
    Memo1: TMemo;
    cmbTxt: TComboBox;
    PageControl1: TPageControl;
    tsCOM: TTabSheet;
    tsEthernet: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lePort: TLabeledEdit;
    cmbBaud: TComboBox;
    leLen: TLabeledEdit;
    cmbStopBits: TComboBox;
    cmbParity: TComboBox;
    Label1: TLabel;
    cmbMode: TComboBox;
    cmbTrailing: TComboBox;
    cbListen: TCheckBox;
    Label2: TLabel;
    rgProtocol: TRadioGroup;
    leIP: TLabeledEdit;
    leTCPPort: TLabeledEdit;
    leWaitFirst: TLabeledEdit;
    leWaitNext: TLabeledEdit;
    ssListen: TServerSocket;
    cbRepeat: TCheckBox;
    Timer1: TTimer;
    btBreak: TButton;
    procedure btGoClick(Sender: TObject);
    procedure cbListenClick(Sender: TObject);
    procedure lePortChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageControl1Change(Sender: TObject);
    procedure cmbTxtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgProtocolClick(Sender: TObject);
    procedure ssListenClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ssListenAccept(Sender: TObject; Socket: TCustomWinSocket);
    procedure ssListenGetSocket(Sender: TObject; Socket: NativeInt; var ClientSocket: TServerClientWinSocket);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbRepeatClick(Sender: TObject);
    procedure btBreakClick(Sender: TObject);
  private
    ListenPortThread: TGMThread;
    procedure SaveParams;
    procedure LoadParams;
    procedure SaveHistory(const s: string);
    procedure ListenPort(bListen: bool);
    function CreateConnectionObject: TConnectionObjectBase;
    procedure ApplyTrailing(obj: TConnectionObjectBase; trailingIndex: int);
    procedure ProcessExchangeBlockData(thr: TConnectionObjectBase; et: TExchangeType);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TListenComThread }

constructor TListenComThread.Create;
begin
  inherited Create();
  FConnectionObject := Form1.CreateConnectionObject();
  FConnectionObject.InitEquipmentBeforeExchange := false;
end;

procedure TListenComThread.SafeExecute;
begin
  while not Terminated do
  begin
    case FConnectionObject.ExchangeBlockData(etRec) of
      ccrBytes:
        begin
          Form1.Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + ' < ' + ArrayToString(FConnectionObject.buffers.BufRec, FConnectionObject.buffers.NumberOfBytesRead, true, true));
        end;

      ccrError:
        begin
          Form1.Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + ' Error');
        end;

      ccrEmpty:
        begin
          //Form1.Memo1.Lines.Add('No responce');
        end;
    end;

    sleep(200);
  end;
end;

procedure TForm1.SaveHistory(const s: string);
var n: int;
begin
  if s = '' then Exit;

  n := cmbTxt.Items.IndexOf(s);
  if n = 0 then Exit;

  if n > 0 then
    cmbTxt.Items.Delete(n);

  cmbTxt.Items.Insert(0, s);
  cmbTxt.Text := s;

  SaveParams();
end;

function TForm1.CreateConnectionObject(): TConnectionObjectBase;
begin
  if PageControl1.ActivePageIndex = 0 then
  begin
    Result := TConnectionObjectCOM.Create();

    TConnectionObjectCOM(Result).nPort := StrToInt(lePort.Text);
    TConnectionObjectCOM(Result).nBaudRate := StrToInt(cmbBaud.Text);
    TConnectionObjectCOM(Result).nParity := cmbParity.ItemIndex;
    TConnectionObjectCOM(Result).nStopBits := cmbStopBits.ItemIndex;
    TConnectionObjectCOM(Result).nWordLen := StrToInt(leLen.Text);
  end
  else
  begin
    if rgProtocol.ItemIndex = 0 then
    begin
      Result := TConnectionObjectTCP_OwnSocket.Create();
      TConnectionObjectTCP_OwnSocket(Result).Host := leIP.Text;
      TConnectionObjectTCP_OwnSocket(Result).Port := StrToInt(leTCPPort.Text);
    end
    else
    begin
      Result := TConnectionObjectUDP.Create();
      TConnectionObjectUDP(Result).UDPObject.Host := leIP.Text;
      TConnectionObjectUDP(Result).UDPObject.Port := StrToInt(leTCPPort.Text);
      TConnectionObjectUDP(Result).UDPObject.ReceiveTimeout := StrToIntDef(leWaitFirst.Text, 1000);
    end;
  end;

  Result.WaitFirst := StrToIntDef(leWaitFirst.Text, 1000);
  Result.WaitNext := StrToIntDef(leWaitNext.Text, 100);
end;

procedure TForm1.ApplyTrailing(obj: TConnectionObjectBase; trailingIndex: int);
begin
    case cmbTrailing.ItemIndex of
      1:
        begin
          obj.buffers.BufSend[obj.buffers.LengthSend] := 13;
          inc(obj.buffers.LengthSend);
        end;
      2:
        begin
          obj.buffers.BufSend[obj.buffers.LengthSend] := 26;
          inc(obj.buffers.LengthSend);
        end;
      3:
        begin
          obj.buffers.BufSend[obj.buffers.LengthSend] := 13;
          obj.buffers.BufSend[obj.buffers.LengthSend + 1] := 10;
          inc(obj.buffers.LengthSend, 2);
        end;
      4:
        begin
          Modbus_CRC(obj.buffers.BufSend, obj.buffers.LengthSend);
          inc(obj.buffers.LengthSend, 2);
        end;
      5: // Tecon_CRC + 0x16
        begin
          Tecon_CRC(obj.buffers.BufSend, obj.buffers.LengthSend);
          inc(obj.buffers.LengthSend, 2);
        end;
      6: // ÀÓ„ËÍ‡ —œ“941, 943
        begin
          LogicaM4_CRC(obj.buffers.BufSend, obj.buffers.LengthSend);
          inc(obj.buffers.LengthSend, 2);
        end;
      7: // ÀÓ„ËÍ‡ —œ“961
        begin
          LogicaSPBUS_CRC(obj.buffers.BufSend, obj.buffers.LengthSend);
          inc(obj.buffers.LengthSend, 2);
        end;
      8:
        begin
          Modbus_LRC(obj.buffers.BufSend, obj.buffers.LengthSend);
          obj.buffers.BufSend[obj.buffers.LengthSend + 2] := 13;
          obj.buffers.BufSend[obj.buffers.LengthSend + 3] := 10;
          inc(obj.buffers.LengthSend, 4);
        end;
    end;
end;

procedure TForm1.ProcessExchangeBlockData(thr: TConnectionObjectBase; et: TExchangeType);
begin
  case thr.ExchangeBlockData(etSenRec) of
    ccrBytes:
      begin
        Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + ' < ' + ArrayToString(thr.buffers.BufRec, thr.buffers.NumberOfBytesRead, true, true));
      end;

    ccrError:
      begin
        Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + ' Error ' + thr.LastError);
      end;

    ccrEmpty:
      begin
        Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + ' No responce');
      end;
  end;
end;

procedure TForm1.btGoClick(Sender: TObject);
var l: TStringList;
    i, n: int;
    thr: TConnectionObjectBase;
    s, v: string;
begin
  Timer1.Enabled := false;

  s := Trim(cmbTxt.Text);
  SaveHistory(s);

  if cbListen.Checked then
  begin
    ListenPort(false);
  end;

  thr := CreateConnectionObject();
  Timer1.Interval := thr.WaitFirst;

  l := TStringList.Create();

  try
    case cmbMode.ItemIndex of
      0, 2:
        begin
          l.CommaText := s;
          n := l.Count;
          for i := 0 to l.Count - 1 do
          begin
            v := Trim(l[i]);
            if v = '' then
              dec(n)
            else
              thr.buffers.BufSend[i] := StrToInt(IfThen((cmbMode.ItemIndex = 0) and  (v[1] <> '$'), '$') + v);
          end;
        end;

      1:
        begin
          n := Length(s);
          for i := 1 to n do
            thr.buffers.BufSend[i - 1] := Ord(AnsiString(s)[i]);
        end;

      else Exit;
    end;

    thr.buffers.LengthSend := n;
    ApplyTrailing(thr, cmbTrailing.ItemIndex);

    Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + ' ? ' + ArrayToString(thr.buffers.BufSend, thr.buffers.LengthSend, true, true));

    ProcessExchangeBlockData(thr, etSenRec);
  finally
    l.Free();
    thr.Free();
    ListenPort(cbListen.Visible and cbListen.Checked);
    if cbRepeat.Checked then
    begin
      Timer1.Enabled := true;
    end;
  end;
end;

procedure TForm1.btBreakClick(Sender: TObject);
var
  thr: TConnectionObjectBase;
begin
  thr := CreateConnectionObject();
  try
    if TConnectionObjectCOM(thr).SendBreak() then
    begin
      thr.FreePort();
      ProcessExchangeBlockData(thr, etRec)
    end
    else
      ShowMessageBox('Break failed ' + thr.LastError);
  finally
    thr.Free();
  end;
end;

procedure TForm1.ListenPort(bListen: bool);
begin
  cbListen.Checked := bListen;

  if bListen then
  begin
    if PageControl1.ActivePageIndex = 0 then
    begin
      ListenPortThread := TListenComThread.Create()
    end
    else
    begin
      ssListen.Port := StrToIntDef(leTCPPort.Text, 0);
      try
        ssListen.Active := true;
      except
        cbListen.Checked := false;
        raise;
      end;
    end;
  end
  else
  begin
    TryFreeAndNil(ListenPortThread);
    ssListen.Active := false;
  end;
end;

procedure TForm1.cbListenClick(Sender: TObject);
begin
  if cbListen.Checked then
    cbRepeat.Checked := false;

  ListenPort(cbListen.Checked);
end;

procedure TForm1.cbRepeatClick(Sender: TObject);
begin
  if cbRepeat.Checked then
    cbListen.Checked := false;
end;

procedure TForm1.cmbTxtKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    btGoClick(nil);
end;

procedure TForm1.SaveParams();
var f: TIniFile;
    i: int;
begin
  try
    f := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

    f.WriteInteger('COMMON', 'COM', StrToInt(lePort.Text));
    f.WriteInteger('COMMON', 'BAUD', StrToInt(cmbBaud.Text));

    f.WriteInteger('COMMON', 'PARITY', cmbParity.ItemIndex);
    f.WriteInteger('COMMON', 'STOPBITS', cmbStopBits.ItemIndex);
    f.WriteInteger('COMMON', 'WORDLEN', StrToInt(leLen.Text));

    f.WriteInteger('COMMON', 'MODE', cmbMode.ItemIndex);
    f.WriteInteger('COMMON', 'TRAILING', cmbTrailing.ItemIndex);

    f.WriteString('COMMON', 'WAITFIRST', leWaitFirst.Text);
    f.WriteString('COMMON', 'WAITNEXT', leWaitNext.Text);

    f.WriteInteger('COMMON', 'PAGE', PageControl1.ActivePageIndex);
    f.WriteInteger('COMMON', 'PROTOCOL', rgProtocol.ItemIndex);
    f.WriteString('COMMON', 'IP', leIP.Text);
    f.WriteString('COMMON', 'TCPPORT', leTCPPort.Text);

    f.WriteInteger('HISTORY', 'Count', cmbTxt.Items.Count);
    for i := 0 to cmbTxt.Items.Count - 1 do
      f.WriteString('HISTORY', 'Record_' + IntToStr(i), cmbTxt.Items[i]);

    f.UpdateFile();
    f.Free();
  except
  end;
end;

procedure TForm1.ssListenAccept(Sender: TObject; Socket: TCustomWinSocket);
var opt: integer;
begin
  opt := 1;
  SetSockOpt(Socket.SocketHandle, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@opt), SizeOf(opt));
end;

procedure TForm1.ssListenClientRead(Sender: TObject; Socket: TCustomWinSocket);
var cnt: int;
    buf: array[0..1000] of byte;
    action: string;
begin
  try
    action := 'CreateConn';
    cnt := Socket.ReceiveBuf(buf, Length(buf));
    Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + ' TCP > ' + ArrayToString(buf, cnt, false, true));
  except
    on e: Exception do
      Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now()) + 'TCP error: ' + action + ' - ' + e.Message);
  end;
end;

procedure TForm1.ssListenGetSocket(Sender: TObject; Socket: NativeInt; var ClientSocket: TServerClientWinSocket);
begin
  ClientSocket := TServerClientWinSocket.Create(Socket, Sender as TServerWinSocket);
end;

procedure TForm1.LoadParams();
var f: TIniFile;
    i, n: int;
    s: string;
begin
  try
    f := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

    lePort.Text := f.ReadString('COMMON', 'COM', lePort.Text);
    n := cmbBaud.ItemIndex;
    cmbBaud.ItemIndex := cmbBaud.Items.IndexOf(f.ReadString('COMMON', 'BAUD', '9600'));
    if cmbBaud.ItemIndex < 0 then
      cmbBaud.ItemIndex := n;

    cmbParity.ItemIndex := f.ReadInteger('COMMON', 'PARITY', cmbParity.ItemIndex);
    cmbStopBits.ItemIndex := f.ReadInteger('COMMON', 'STOPBITS', cmbStopBits.ItemIndex);
    leLen.Text := f.ReadString('COMMON', 'WORDLEN', leLen.Text);

    cmbMode.ItemIndex := f.ReadInteger('COMMON', 'MODE', cmbMode.ItemIndex);
    cmbTrailing.ItemIndex := f.ReadInteger('COMMON', 'TRAILING', cmbTrailing.ItemIndex);

    leWaitFirst.Text := f.ReadString('COMMON', 'WAITFIRST', leWaitFirst.Text);
    leWaitNext.Text := f.ReadString('COMMON', 'WAITNEXT', leWaitNext.Text);

    PageControl1.ActivePageIndex := IfThen(f.ReadInteger('COMMON', 'PAGE', 1) = 1, 1, 0);
    PageControl1Change(nil);
    rgProtocol.ItemIndex := IfThen(f.ReadInteger('COMMON', 'PROTOCOL', 1) = 1, 1, 0);
    leIP.Text := f.ReadString('COMMON', 'IP', '');
    leTCPPort.Text := f.ReadString('COMMON', 'TCPPORT', '');

    n := f.ReadInteger('HISTORY', 'Count', 0);
    for i := 0 to n - 1 do
    begin
      s := Trim(f.ReadString('HISTORY', 'Record_' + IntToStr(i), ''));
      if s <> '' then
        cmbTxt.Items.Add(s);
    end;

    f.Free();
  except
  end;
end;

procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = ord('A')) then
    Memo1.SelectAll();
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  ListenPort(false);
end;

procedure TForm1.rgProtocolClick(Sender: TObject);
begin
  ListenPort(false);
end;

procedure TForm1.lePortChange(Sender: TObject);
begin
  SaveParams();
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LoadParams();
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveParams();
end;

end.


