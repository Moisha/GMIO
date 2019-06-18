unit Tecon_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ScktComp, StdCtrls, Sockets, IdBaseComponent, IdComponent,
  IdUDPBase, IdUDPServer, IdSocketHandle, IdUDPClient, IdGlobal;

type                    
  TForm1 = class(TForm)
    Memo1: TMemo;
    IdUDPServer1: TIdUDPServer;
    Button1: TButton;
    IdUDPClient1: TIdUDPClient;
    CheckBox1: TCheckBox;
    Button2: TButton;
    Edit1: TEdit;
    procedure UdpSocket1Connect(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ServerSocket1Accept(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocket1ClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocket1ClientRead(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocket1GetSocket(Sender: TObject; Socket: Integer;
      var ClientSocket: TServerClientWinSocket);
    procedure IdUDPServer2Status(ASender: TObject;
      const AStatus: TIdStatus; const AStatusText: String);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IdUDPServer1UDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses GMGlobals, Devices.Tecon, StrUtils;

{$R *.dfm}

procedure TForm1.UdpSocket1Connect(Sender: TObject);
begin
  Memo1.Lines.Add('UPD Connected');
end;

procedure TForm1.IdUDPServer1UDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle);
var buf: TIdBytes;
    bCRC: bool;
    n: int;
begin
  n := Length(AData);
  bCRC := Tecon_CheckCRC(AData, n);
  Memo1.Lines.Add('UPD < ' + ArrayToString(AData, n, false, true) + ' - ' + IfThen(bCRC, 'Ok', 'Bad CRC'));
  // 68 08 08 68 41 25 1B 02 C0 06 36 06 85 16

  if n >= 14 then
  begin
    SetLength(buf, 9);
    buf[0] := $10;
    buf[1] := $40;
    buf[2] := AData[7];
    buf[3] := $1B;
    buf[4] := $00;
    buf[5] := $00;
    buf[6] := $00;
    Tecon_CRC(buf, 7);

    Memo1.Lines.Add('UPD > ' + ArrayToString(buf, 9, false, true));
    ABinding.SendTo(ABinding.PeerIP, ABinding.PeerPort, buf);
  end
  else
  if AData[0] <> $10 then
  begin
    SetLength(buf, 9);
    buf[0] := $10;
    buf[1] := $40;
    buf[2] := $08;
    buf[3] := $01;
    buf[4] := $00;
    buf[5] := $F0;
    buf[6] := $00;
    Tecon_CRC(buf, 7);

    Memo1.Lines.Add('UPD > ' + ArrayToString(buf, 9, false, true));
    ABinding.SendTo(ABinding.PeerIP, ABinding.PeerPort, buf);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var buf: array [0..100] of byte;
    bufSend, bufRec: TIdBytes;
    i, n: int;
    l: TStringList;
    b: bool;
    f: double;
//    v: int64;
begin
{  buf[0] := $10;
  buf[1] := $40;
  buf[2] := $08;
  buf[3] := $01;
  buf[4] := $00;
  buf[5] := $F0;
  buf[6] := $00;
  Tecon_CRC(buf, 7);

  IdUDPClient1.SendBuffer(buf, 9);
  Memo1.Lines.Add('UPD > ' + ArrayToString(buf, 9, false, true));
  n := IdUDPClient1.ReceiveBuffer(bufRec, Length(bufRec));

  Memo1.Lines.Add('UPD < ' + ArrayToString(bufRec, n, false, true) + ' - ' + IfThen(Tecon_CheckCRC(bufRec, n), 'Ok', 'Bad CRC'));
}
  l := TStringList.Create();
  l.CommaText := Edit1.Text;
  n := l.Count;
  for i := 0 to n - 1 do
    buf[i] := StrToInt('$' + l[i]);

  Tecon_CRC(buf, n);

  SetLength(bufSend, n + 2);
  WriteBuf(bufSend, 0, buf, n + 2);

  IdUDPClient1.SendBuffer(bufSend);
  Memo1.Lines.Add('UPD > ' + ArrayToString(buf, n + 2, false, true));


  SetLength(bufRec, 1024);
  n := IdUDPClient1.ReceiveBuffer(bufRec, Length(bufRec));
  b := Tecon_CheckCRC(bufRec, n);
  Memo1.Lines.Add('UPD < ' + ArrayToString(bufRec, n, false, true) + ' - ' + IfThen(b, 'Ok', 'Bad CRC'));

  if b then
  begin
    f := ReadSingle(bufRec, 6);
    Memo1.Lines.Add('Decoding float: ' + FormatFloatToShow(f, 3));

{    f := ReadSingleInv(bufRec, 6);
    Memo1.Lines.Add('Decoding float_Inv: ' + FormatFloatToShow(f, 3));

    v := ReadUINT(bufRec, 6);
    Memo1.Lines.Add('Decoding UINT: ' + IntToStr(v));

    v := ReadUINTInv(bufRec, 6);
    Memo1.Lines.Add('Decoding UINT_Inv: ' + IntToStr(v));

    v := ReadLongInt(bufRec, 6);
    Memo1.Lines.Add('Decoding LongInt: ' + IntToStr(v));

    v := ReadLongIntInv(bufRec, 6);
    Memo1.Lines.Add('Decoding LongInt_Inv: ' + IntToStr(v)); }
  end;

  Memo1.Lines.Add('');

  l.Free();
end;

procedure TForm1.ServerSocket1Accept(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Memo1.Lines.Add('TCP Accept');
end;

procedure TForm1.ServerSocket1ClientConnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Memo1.Lines.Add('TCP Connect');
end;

procedure TForm1.ServerSocket1ClientRead(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Memo1.Lines.Add('TCP Read');
end;

procedure TForm1.ServerSocket1GetSocket(Sender: TObject; Socket: Integer;
  var ClientSocket: TServerClientWinSocket);
begin
  Memo1.Lines.Add('TCP Get');
end;

procedure TForm1.IdUDPServer2Status(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: String);
begin
  Memo1.Lines.Add('UDP State');
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  IdUDPServer1.Active := CheckBox1.Checked;
end;

procedure TForm1.Button2Click(Sender: TObject);
var l: TStringList;
    r: TSearchRec;
    n, nPas, nDfm: int;
begin
  l := TStringList.Create();
  nPas := 0;
  nDfm := 0;
  n := FindFirst('D:\Programs\Delphi\Geomer\GMIO\*.*', faAnyFile, r);
  while n = 0 do
  begin
    if ExtractFileExt(r.Name) = '.pas' then
    begin
      l.LoadFromFile('D:\Programs\Delphi\Geomer\GMIO\' + r.Name);
      nPas := nPas + l.Count;
    end;

    if ExtractFileExt(r.Name) = '.dfm' then
    begin
      l.LoadFromFile('D:\Programs\Delphi\Geomer\GMIO\' + r.Name);
      nDfm := nDfm + l.Count;
    end;

    n := FindNext(r);
  end;

  Memo1.Lines.Add('pas - ' + IntToStr(nPas));
  Memo1.Lines.Add('dfm - ' + IntToStr(nDfm));
  Memo1.Lines.Add('all - ' + IntToStr(nDfm + nPas));

  FindClose(r);
  l.Free();
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Button1Click(nil);
end;

procedure TForm1.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then Memo1.Lines.Clear();
end;

end.
