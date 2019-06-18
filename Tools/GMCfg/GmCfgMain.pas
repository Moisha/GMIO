unit GmCfgMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ScktComp, GMGlobals, WinSock, StdCtrls, ExtCtrls, IniFiles,
  GMSocket, GMConst, GeomerLastValue, Threads.GMSocket;

const WM_NCAR_FOUND = WM_USER + 1;
      WM_NCAR_DELETED = WM_USER + 2;
      WM_LOG_UPDATED = WM_USER + 3;

type
  TCfgGeomerSocket = class (TGeomerSocket)
  private
    procedure WrapAndSend485Command(buf: array of byte; cnt: int);
    function MakeGM485Buf_OneRequest(var buf: array of Byte; bufSrc: array of byte; nCnt, Id485: int): int;
  protected
    procedure SetNCar(const Value: int); override;
    procedure slChanged(Sender: TObject);
    function DecodeGMDataBlock(buf: array of byte; cnt: int): int; override;
    procedure BuildReqList; override;
  public
    slProtocol: TStringList;
    constructor Create(Socket: TSocket; ServerWinSocket: TServerWinSocket; glvBuffer: TGeomerLastValuesBuffer);
    destructor Destroy; override;
  end;

  TGMCfgMainFrm = class(TForm)
    ssGM: TServerSocket;
    lCars: TListBox;
    Panel1: TPanel;
    mLog: TMemo;
    Panel2: TPanel;
    Button1: TButton;
    eFilename: TEdit;
    Button2: TButton;
    odProgram: TOpenDialog;
    eStrForSend: TEdit;
    Panel3: TPanel;
    Button3: TButton;
    Button4: TButton;
    eBlock: TEdit;
    Button5: TButton;
    btUBZ: TButton;
    Button6: TButton;
    procedure ssGMClientError(Sender: TObject;
      Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer);
    procedure ssGMAccept(Sender: TObject; Socket: TCustomWinSocket);
    procedure ssGMClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ssGMClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure lCarsClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ssGMClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ssGMThreadEnd(Sender: TObject; Thread: TServerClientThread);
    procedure FormShow(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure eStrForSendKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btUBZClick(Sender: TObject);
    procedure ssGMGetSocket(Sender: TObject; Socket: NativeInt; var ClientSocket: TServerClientWinSocket);
    procedure Button6Click(Sender: TObject);
    procedure ssGMGetThread(Sender: TObject;
      ClientSocket: TServerClientWinSocket;
      var SocketThread: TServerClientThread);
  private
    { Private declarations }
    LastWriteVer: WORD;
    glvBuffer: TGeomerLastValuesBuffer;

    procedure WMNCarFound(var Msg: TMessage); message WM_NCAR_FOUND;
    procedure WMNCarDeleted(var Msg: TMessage); message WM_NCAR_DELETED;
    procedure WMLogUpdated(var Msg: TMessage); message WM_LOG_UPDATED;
    procedure ReadINI;
    procedure SendTextToSocket(Name: string; Sckt: TCfgGeomerSocket; txt: string);
  public
    { Public declarations }
  end;

var
  GMCfgMainFrm: TGMCfgMainFrm;

implementation

{$R *.dfm}

procedure TGMCfgMainFrm.ssGMClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  ErrorCode:=0;
  Socket.Close();
end;

procedure WriteLnString(const s: string; bForceRewrite: bool = false);
var f: TextFile;
begin
  AssignFile(f, 'CfgLog.log');
  if bForceRewrite or not FileExists('CfgLog.log') then
    Rewrite(f)
  else
    Append(f);
  Writeln(f, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz ', Now()) + s);
  CloseFile(f);
end;

procedure TGMCfgMainFrm.ssGMGetSocket(Sender: TObject; Socket: NativeInt; var ClientSocket: TServerClientWinSocket);
var //opt: integer;
    buf: array[0..100] of byte;
    s: string;
begin
  ClientSocket:=TCfgGeomerSocket.Create(Socket, Sender as TServerWinSocket, glvBuffer);

  try
  TCfgGeomerSocket(ClientSocket).slProtocol.Add('Host - ' + ClientSocket.RemoteHost);
  except
    TCfgGeomerSocket(ClientSocket).slProtocol.Add('Host - ?');
  end;

  try
  TCfgGeomerSocket(ClientSocket).slProtocol.Add('Address - ' + ClientSocket.RemoteAddress);
  except
    TCfgGeomerSocket(ClientSocket).slProtocol.Add('Address - ?');
  end;

  // установим на вс€кий случай врем€, вдруг спутники не нашел
  s:=#13#10'TIME:'+FormatDateTime('ddmmyy,hhnnss', NowUTC())+#13#10;
  WriteString(buf, 0, s);
  ClientSocket.SendBuf(buf, Length(s));

  WriteLnString('Get Socket');

{  Sleep(1000);

  s:=#13#10'INFO:1'#13#10;
  WriteString(buf, 0, s);
  ClientSocket.SendBuf(buf, Length(s));}
end;

procedure TGMCfgMainFrm.ssGMGetThread(Sender: TObject;
  ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);
begin
  SocketThread := TGMSocketThread.Create(false, ClientSocket);
end;

{ TCfgGeomerSocket }

procedure TCfgGeomerSocket.BuildReqList;
begin
  // Do nothing
end;

constructor TCfgGeomerSocket.Create(Socket: TSocket; ServerWinSocket: TServerWinSocket; glvBuffer: TGeomerLastValuesBuffer);
begin
  inherited Create(Socket, ServerWinSocket, glvBuffer);

  slProtocol:=TStringList.Create();
  slProtocol.OnChange:=slChanged;
end;

destructor TCfgGeomerSocket.Destroy;
begin
  PostMessage(Application.MainForm.Handle, WM_NCAR_DELETED, WParam(self), 0);
  slProtocol.Free();

  inherited;
end;

function TCfgGeomerSocket.MakeGM485Buf_OneRequest(var buf: array of Byte; bufSrc: array of byte; nCnt: int; Id485: int): int;
begin
  Result := WriteString(buf, 0,  #13#10'RS485:P');  // заголовок пакетной передачи
  Result := WriteByte(buf, Result, 1); // к-во запросов
  Result := WriteByte(buf, Result, Id485);  // ID первого запроса
  Result := WriteByte(buf, Result, nCnt and $FF);   // длина первого запроса
  // ... ID и длины остальных запросов ...
  Result := WriteBuf(buf, Result, bufSrc, nCnt); // первый запрос
  // ... остальные запросы ...
  Result := WriteString(buf, Result, #13#10);
end;

procedure TCfgGeomerSocket.WrapAndSend485Command(buf: array of byte; cnt: int);
var nPos: int;
    bufSend: array [0..100] of byte;
begin
  if FSocketObjectType = OBJ_TYPE_GM then
  begin
    nPos := MakeGM485Buf_OneRequest(bufSend, buf, cnt, 0);
    SendBuf(bufSend, nPos);
  end
  else
  begin
    SendBuf(buf, cnt);
  end;
end;

function TCfgGeomerSocket.DecodeGMDataBlock(buf: array of byte; cnt: int): int;
var i, j, n, c485: int;
    s: string;
    f: TFileStream;
    bufFLK: array[0..37] of Byte;
begin
  WriteLnString('DecodeGMDataBlock cnt = ' + IntToStr(cnt));
  if (cnt>=37) and (buf[0]=$55) and (buf[1]=$FF) then
  begin
    WriteLnString('DecodeGMDataBlock.FLK');
    Result:=37;
    FSocketObjectType := OBJ_TYPE_GM;
    
    try
      f:=TFileStream.Create(ChangeFileExt(GMCfgMainFrm.eFilename.Text, '.flk'), fmOpenRead);
      n:=f.Read(bufFLK, 38);
      s:='Ќехватка блоков: ';
      if n=38 then
      begin
        for i:=0 to 31 do
          for j:=0 to 7 do
          begin
            if (buf[i+5] and (1 shl j)=0)
               and (bufFlk[i+2] and (1 shl j)>0) then
            begin
              s:=s+IntToStr(i*8+j)+', ';
            end;
          end;
        slProtocol.Add(s);
      end;
    except
    end;
    TryFreeAndNil(f);
  end
  else
  if CheckGMWithUBZBlock(buf, cnt, Result) then
  begin
    // длинный блок c ”Ѕ«
    WriteLnString('DecodeGMDataBlock.длинный блок c ”Ѕ«');
    FSocketObjectType := OBJ_TYPE_GM;
    N_Car:=ReadWord(buf, 2);
  end
  else
  if (cnt>=90) and (buf[0]=$55) and (buf[1]=$19) then
  begin
    // длинный блок c ISCO
    WriteLnString('DecodeGMDataBlock.длинный блок c ISCO');
    Result:=90;
    FSocketObjectType := OBJ_TYPE_GM;
    N_Car:=ReadWord(buf, 2);
  end
  else
  if (cnt>=46) and (buf[0]=$55) and (buf[1]=$18) then
  begin
    // длинный блок
    WriteLnString('DecodeGMDataBlock.длинный блок');
    Result:=46;
    FSocketObjectType := OBJ_TYPE_GM;
    N_Car:=ReadWord(buf, 2);
  end
  else
  if (cnt>=24) and (buf[0]=$55) and (buf[1]=$17) then
  begin
    // короткий блок
    WriteLnString('DecodeGMDataBlock.короткий блок');
    Result:=24;
    FSocketObjectType := OBJ_TYPE_GM;
    N_Car:=ReadWord(buf, 2);
  end
  else
  if (cnt>=4) and (buf[0]=$55) and (buf[1]=$85) then
  begin
    //485
    WriteLnString('DecodeGMDataBlock.485');
    c485:=buf[3];
    FSocketObjectType := OBJ_TYPE_GM;
    Result:=c485+4;
  end
  else
  // INFO:1
  if (cnt>=123) and (buf[0]=$55) and (buf[1]=1) then
  begin
    WriteLnString('DecodeGMDataBlock.INFO:1');
    N_Car:=ReadWORD(buf, 108);
    FSocketObjectType := OBJ_TYPE_GM;
    Result:=123;
  end
  else
  if (cnt>=27) and (buf[0]=$55) and (buf[1]=0) then
  begin
    WriteLnString('DecodeGMDataBlock.X3');
    Result:=27;
    FSocketObjectType := OBJ_TYPE_GM;
    
    s:='';

    for i:=2 to 9 do
      s:=s+Chr(buf[i]);
    s:=s+' ';

    for i:=11 to 26 do
      s:=s+Chr(buf[i]);

    slProtocol.Add(s);
    WriteLnString('DecodeGMDataBlock.Done');

    Exit;
  end
  else
  begin
    WriteLnString('DecodeGMDataBlock.Unrecognized');
    Result:=cnt;
  end;

  WriteLnString('DecodeGMDataBlock.ToProtocol');
  slProtocol.Add(ArrayToString(buf, Result, true));
  WriteLnString('DecodeGMDataBlock.Done');
end;

procedure TCfgGeomerSocket.SetNCar(const Value: int);
begin
  inherited SetNCar(Value);
  if Value >= 0 then
    PostMessage(Application.MainForm.Handle, WM_NCAR_FOUND, WParam(self), N_Car);
end;

procedure TGMCfgMainFrm.ssGMAccept(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  //opt:=1;
  //SetSockOpt(Socket.SocketHandle,SOL_SOCKET,SO_KEEPALIVE,PChar(@opt),SizeOf(opt));

  // установим на вс€кий случай врем€, вдруг спутники не нашел
end;

procedure TGMCfgMainFrm.ssGMClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Socket.OnErrorEvent:=ssGMClientError;
end;

procedure TGMCfgMainFrm.ssGMClientRead(Sender: TObject;
  Socket: TCustomWinSocket);

var Sckt: TCfgGeomerSocket;
    cnt: int;
    buf: array [0..102400] of byte;
    iProcessed: int;

begin
  WriteLnString('ssGMClientRead');
  Sckt:=TCfgGeomerSocket(Socket);
  cnt:=Sckt.ReceiveBuf(buf, High(Buf));
  WriteLnString(ArrayToString(buf, cnt));

  if cnt > 0 then
  try
    repeat
      iProcessed := Sckt.DecodeGMDataBlock(buf, cnt);
      if (iProcessed=0) or (iProcessed>cnt)  then break;

      CopyMemory(@buf, @buf[iProcessed], cnt-iProcessed);
      cnt:=cnt-iProcessed;
    until (cnt<=0) or (iProcessed<=0);
  except
    on e: Exception do
    begin
      WriteLnString('Sckt.DecodeGMDataBlock ' + e.Message);
    end;
  end;
  WriteLnString('ssGMClientRead Done');
end;

procedure TGMCfgMainFrm.WMNCarFound(var Msg: TMessage);
var i: int;
begin
  WriteLnString('WMNCarFound');
  for i:=lCars.Count-1 downto 0 do
  begin
    if lCars.Items.Objects[i]=TObject(Msg.WParam) then
      Exit
    else
    if lCars.Items[i]=IntToStr(Msg.LParam) then
      lCars.Items.Delete(i);
  end;

  lCars.Items.AddObject(IntToStr(Msg.LParam), TObject(Msg.WParam));

  WriteLnString('WMNCarFound Done');
end;

procedure TGMCfgMainFrm.lCarsClick(Sender: TObject);
begin
  try
    mLog.Lines.BeginUpdate();
    mLog.Lines.Assign(TCfgGeomerSocket(lCars.Items.Objects[lCars.ItemIndex]).slProtocol);
  finally
    mLog.Lines.EndUpdate();
  end;
end;

procedure TGMCfgMainFrm.Button2Click(Sender: TObject);
begin
  if odProgram.Execute() then
    eFilename.Text:=odProgram.FileName;
end;

procedure TGMCfgMainFrm.Button1Click(Sender: TObject);
var Sckt: TCfgGeomerSocket;
    fs: TFileStream;
    nPos, cnt, nBlock: int;
    buf, bufSend: array [0..600] of byte;
begin
  if lCars.ItemIndex<0 then Exit;

  Sckt:=TCfgGeomerSocket(lCars.Items.Objects[lCars.ItemIndex]);
  if not Sckt.Connected then
  begin
    ShowMessageBox('Not connected', MB_ICONERROR);
    Exit;
  end;

  if not FileExists(eFilename.Text) then
  begin
    ShowMessageBox('File Not Found', MB_ICONERROR);
    Exit;
  end;

  lCars.Enabled:=false;
  fs:=TFileStream.Create(eFilename.Text, fmOpenRead);
  try
    repeat
      cnt:=fs.Read(buf, 520);
      if cnt<>520 then break;

      nBlock:=ReadWORD(buf, 0);
      LastWriteVer:=ReadWORD(buf, 2);
      nPos:=0;
      nPos:=WriteString(bufSend, nPos, #13#10'ROM:');
      nPos:=WriteBuf(bufSend, nPos, buf, 520);

      if (Trim(eBlock.Text)='') or (nBlock=StrToInt(eBlock.Text)) then
      begin
        Sckt.slProtocol.Add('ROM '+IntToStr(nBlock));
        Sckt.SendBuf(bufSend, nPos);
        Sleep(1000);
      end;

      Application.ProcessMessages();
    until false;

    // теперь печать
    TryFreeAndNil(fs);
    fs:=TFileStream.Create(ChangeFileExt(eFilename.Text, '.flk'), fmOpenRead);
    fs.Read(buf, 38);
    TryFreeAndNil(fs);

    nPos:=WriteString(bufSend, 0, #13#10'ROM KEY:');
    nPos:=WriteBuf(bufSend, nPos, buf, 38);
    Sckt.SendBuf(bufSend, nPos);
  finally
    lCars.Enabled:=true;
    if fs<>nil then TryFreeAndNil(fs);
    eBlock.Text := '';
  end;
end;

procedure TGMCfgMainFrm.WMNCarDeleted(var Msg: TMessage);
var i: int;
begin
  for i:=0 to lCars.Count-1 do
    if lCars.Items.Objects[i]=TObject(Msg.WParam) then
    begin
      lCars.Items.Delete(i);
      Exit;
    end;
end;

procedure TCfgGeomerSocket.slChanged(Sender: TObject);
begin
  SendMessage(Application.MainForm.Handle, WM_LOG_UPDATED, WParam(self), 0);
end;

procedure TGMCfgMainFrm.WMLogUpdated(var Msg: TMessage);
var i: int;
begin
  WriteLnString('WMLogUpdated');
  if (lCars.ItemIndex>=0) and
     (lCars.Items.Objects[lCars.ItemIndex]=TObject(Msg.WParam)) then
  begin
    for i:=mLog.Lines.Count to TCfgGeomerSocket(Msg.WParam).slProtocol.Count-1 do
      mLog.Lines.Add(TCfgGeomerSocket(Msg.WParam).slProtocol[i]);
  end;
  WriteLnString('WMLogUpdated Done');
end;

procedure TGMCfgMainFrm.SendTextToSocket(Name: string; Sckt: TCfgGeomerSocket; txt: string);
var bufSend: array [0..600] of byte;
    nPos: int;
begin
  if not Sckt.Connected then
  begin
    ShowMessageBox(Name+' Not connected', MB_ICONERROR);
    Exit;
  end;

  txt := StringReplace(txt, '|', #13, [rfReplaceAll]);
  nPos := WriteString(bufSend, 0, #13#10+txt+#13#10);
  Sckt.SendBuf(bufSend, nPos);
end;

procedure TGMCfgMainFrm.Button3Click(Sender: TObject);
begin
  if lCars.ItemIndex<0 then Exit;

  SendTextToSocket(
    lCars.Items[lCars.ItemIndex],
    TCfgGeomerSocket(lCars.Items.Objects[lCars.ItemIndex]),
    eStrForSend.Text);
end;

procedure TGMCfgMainFrm.Button5Click(Sender: TObject);
var i: int;
begin
  for i:=0 to lCars.Count-1 do
    SendTextToSocket(
      lCars.Items[i],
      TCfgGeomerSocket(lCars.Items.Objects[i]),
      eStrForSend.Text);
end;

procedure TGMCfgMainFrm.Button6Click(Sender: TObject);
var buf: ArrayOfByte;
begin
  if lCars.ItemIndex<0 then Exit;

  buf := TextNumbersStringToArray(eStrForSend.Text);
  TCfgGeomerSocket(lCars.Items.Objects[lCars.ItemIndex]).WrapAndSend485Command(buf, Length(buf));
end;

procedure TGMCfgMainFrm.Button4Click(Sender: TObject);
var bufSend: array [0..600] of byte;
    nPos: int;
    Sckt: TCfgGeomerSocket;
begin
  if lCars.ItemIndex<0 then Exit;

  Sckt:=TCfgGeomerSocket(lCars.Items.Objects[lCars.ItemIndex]);
  if not Sckt.Connected then
  begin
    ShowMessageBox('Not connected', MB_ICONERROR);
    Exit;
  end;

  nPos:=WriteString(bufSend, 0, #13#10'ROM TEST:');
  nPos:=WriteWORD(bufSend, nPos, {LastWriteVer}78);
  Sckt.SendBuf(bufSend, nPos);
end;


procedure TGMCfgMainFrm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ssGM.Active:=false;
end;

procedure TGMCfgMainFrm.FormCreate(Sender: TObject);
begin
  LastWriteVer:=0;
  glvBuffer := TGeomerLastValuesBuffer.Create();
  lstSockets := TSocketList.Create(ssGM.Socket);
end;

procedure TGMCfgMainFrm.ssGMClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  PostMessage(Application.MainForm.Handle, WM_NCAR_DELETED, WPARAM(Socket), 0);
end;

procedure TGMCfgMainFrm.ssGMThreadEnd(Sender: TObject;
  Thread: TServerClientThread);
begin
  PostMessage(Application.MainForm.Handle, WM_NCAR_DELETED, WPARAM(nil), 0);
end;

procedure TGMCfgMainFrm.ReadINI();
var f: TINIFile;
    s, fn: string;
    nPort, c: int;
begin
  f:=nil;
  fn:=ExpandFileName('GMIOSrv.ini');
  nPort:=0;

  try
    if not FileExists(fn) then
    begin
    end
    else
    try
      f:=TIniFile.Create(fn);
      nPort:=f.ReadInteger('COMMON', 'PORT', 0);
    finally
      TryFreeAndNil(f);
    end;
  except
    on e: Exception do
    begin
    end;
  end;

  repeat
    if nPort=0 then
    begin
      s:='65500';
      repeat
        if not InputQuery('ѕорт', Application.Title, s) then
        begin
          Application.Terminate;
          Exit;
        end;
        Val(s, nPort, c);
      until (c<=0) and (nPort>0);
    end;

    try
      ssGM.Port:=nPort;
      ssGM.Open();
    except
      on e: exception do
      begin
        nPort:=0;
        ShowMessageBox(e.Message, MB_ICONSTOP);
      end;
    end;
  until ssGM.Active;

end;

procedure TGMCfgMainFrm.FormShow(Sender: TObject);
begin
  ReadINI();
  WriteLnString('CfgStart', true);
end;

procedure TGMCfgMainFrm.eStrForSendKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then Button3Click(Button3);
end;

procedure TGMCfgMainFrm.btUBZClick(Sender: TObject);
var Num, Addr, Cmd, n: int;
    s: string;
begin
  s := Trim(eStrForSend.Text);

  n := Pos(' ', s);

  if n <= 0 then Exit;
  Num := StrToInt(Trim(Copy(s, 1, n)));
  Delete(s, 1, n);

  n := Pos(' ', s);
  if n <= 0 then Exit;
  Addr := StrToInt(Trim(Copy(s, 1, n)));
  Delete(s, 1, n);

  if Trim(s) = '' then Exit;
  Cmd := StrToInt(Trim(Copy(s, 1, n)));
  Delete(s, 1, n);

  // !!!!!! TCfgGeomerSocket(lCars.Items.Objects[lCars.ItemIndex]).SendCommandToUBZ(Num, Addr, Cmd);
  raise Exception.CreateFmt('Not supported ! %d, %d, %d', [Num, Addr, Cmd]);
end;

end.
