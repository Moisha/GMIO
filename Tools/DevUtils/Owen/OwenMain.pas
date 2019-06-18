unit OwenMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GMGlobals;

type
  TForm1 = class(TForm)
    mAnswer: TMemo;
    Button1: TButton;
    Button2: TButton;
    eStrForCRC: TEdit;
    mDecode: TMemo;
    Button3: TButton;
    Edit1: TEdit;
    eNumber: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses SOThread, Math;

{$R *.dfm}
      
procedure TForm1.Button1Click(Sender: TObject);
var so: TSOThread;
    buf: array[0..50] of byte;
    sReq: string;
    i: int;
    t: Single;
    s, e, m: int;
    p: PDWORD;
    dw: DWORD;
begin
  so := TSOThread.Create(true);

  buf[0] := 104 + StrToInt(eNumber.Text) - 1;
  buf[1] := 0 + 0 + 0 + 16 + 0 + 0 + 0 + 0;
  WriteWORDInv(buf, 2, OwenIdToHashStr('rEAd'));
  WriteWORDInv(buf, 4, OwenCRC(buf, 4));
       
  sReq := '#' + OwenEncodeBufToASCII(buf, 6) + #13;
  mAnswer.Lines.Text := mAnswer.Lines.Text + sReq;
  WriteString(so.bufSend, 0, sReq);
  so.LengthSend := Length(sReq);

  so.SetCommParam();
  so.ExchangeBlockData(etSenRec);
  if so.NumberOfBytesRead > 0 then
  begin
    for i := 0 to so.NumberOfBytesRead - 1 do
      mAnswer.Lines.Text := mAnswer.Lines.Text + Chr(so.bufRec[i]);

    OwenDecodeBuf(so.BufRec, so.NumberOfBytesRead, buf);
    t := ReadSingleInv(buf, 4);
    Edit1.Text := FloatToStr(t);

    Button3Click(nil);
  end;

  so.Free();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessageBox(IntToHex(OwenIdToHashStr(eStrForCRC.Text), 4));
end;

procedure TForm1.Button3Click(Sender: TObject);
var i, j: int;
    n1, n2: byte;
    s: string;
begin
  mDecode.Lines.Clear();
  for j := 0 to mAnswer.Lines.Count - 1 do
  begin
    mDecode.Lines.Add('');
    s := mAnswer.Lines[j];
    i := 2;
    while i < Length(mAnswer.Lines[j]) do
    begin
      n1 := ord(s[i]) - ord('G');
      n2 := ord(s[i + 1]) - ord('G');
      mDecode.Lines[j] := mDecode.Lines[j] + ' ' + IntToHex(n1 shl 4 + n2, 2);

      inc(i, 2)
    end;
  end;
end;

end.

