unit XmlPreview;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxMemo, xmlDoc;

type
  TXmlPreviewDlg = class(TForm)
    cxMemo1: TcxMemo;
    procedure cxMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure ToDelphiClipboard;
    { Private declarations }
  public
    { Public declarations }
    procedure SetString(const s: string);
  end;

var
  XmlPreviewDlg: TXmlPreviewDlg;

implementation

uses
  GMGlobals, Vcl.Clipbrd;

{$R *.dfm}

{ TForm1 }

procedure TXmlPreviewDlg.ToDelphiClipboard();
var res, s: string;
    i, n: int;
    buf: ArrayOfByte;
begin
  s := StringReplace(cxMemo1.Lines.Text, #13#10, '', [rfReplaceAll]);
  n := Pos('(', s);
  if n <= 0 then Exit;

  buf := TextNumbersStringToArray(Copy(s, 1, n - 1));
  res := '''';
  for i := 0 to High(buf) do
  begin
    res := res + IntToHex(buf[i], 2) + ' ';
    if (i + 1) mod 40 = 0 then
      res := res + ''' +'#13#10'''';
  end;

  res := res + '''';

  Clipboard.AsText := res;
end;

procedure TXmlPreviewDlg.cxMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl, ssShift]) and (Key = Ord('C')) then
    ToDelphiClipboard();
end;

procedure TXmlPreviewDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close();
end;

procedure TXmlPreviewDlg.SetString(const s: string);
var xml: string;
begin
  xml := Trim(s);
  if (xml <> '') and (xml[1] = '<') then
    xml := FormatXMLData(xml);

  cxMemo1.Lines.Text := xml;
end;

end.
