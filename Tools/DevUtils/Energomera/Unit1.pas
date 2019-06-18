unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GMGlobals, IEC_2061107;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var buf: ArrayOfByte;
    nPos: int;
begin
  // /?!<SOH>R1<STX>NAME()<ETX><BCC>

  SetLength(buf, 50);
  nPos := WriteString(buf, 0, '/?!');
  nPos := WriteByte(buf, nPos, IEC_SOH);
  nPos := WriteString(buf, nPos, 'R1');
  nPos := WriteByte(buf, nPos, IEC_STX);
  // nPos := WriteString(buf, nPos, 'EADPE()');
  nPos := WriteString(buf, nPos, Edit1.Text);
  nPos := WriteByte(buf, nPos, IEC_ETX);
  IEC2061107_CRC(buf, nPos);
  Edit2.Text := ArrayToString(buf, nPos + 1, false, true);
end;


end.
