unit Logica_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GMGlobals, SOThread, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    CommonPrm: TButton;
    leDev: TLabeledEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CommonPrmClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure LoadParams;
    procedure SaveParams;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Logica_Utils, StrUtils, Math, IniFiles;

{$R *.dfm}

var req: TSPT961RequestCreator;

type TSimpleComThread = class(TSOThread)
protected
  procedure ReadINI(); override;
  procedure Execute(); override;
end;

{ TSimpleComThread }

procedure TSimpleComThread.ReadINI;
begin

end;

procedure TSimpleComThread.Execute();
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
var l: TStringList;
    i, n: int;
    thr: TSimpleComThread;
begin
  SaveParams();
  thr := TSimpleComThread.Create(true);

  thr.nPort := 1;
  thr.nBaudRate := 38400;
  thr.WaitFirst := 1000;
  thr.WaitNext := 100;

  l := TStringList.Create();

  try
    l.CommaText := Edit1.Text;
    n := l.Count;
    for i := 0 to n - 1 do
      thr.BufSend[i] := StrToInt('$' + l[i]);

    Logica_CRC(thr.BufSend, n);
    thr.LengthSend := n + 2;

    Memo1.Lines.Add('? ' +ArrayToString(thr.BufSend, thr.LengthSend, true, true));

    thr.SetCommParam();
    case thr.ExchangeBlockData(etSenRec) of
      ccrBytes:
        begin
          Memo1.Lines.Add('< ' + ArrayToString(thr.BufRec, thr.NumberOfBytesRead, true, true) + ' - ' +
                            IfThen(Logica_CheckCRC(thr.BufRec, thr.NumberOfBytesRead), 'Ok', 'BadCRC'));
        end;

      ccrError:
        begin
          Memo1.Lines.Add('Error');
        end;

      ccrEmpty:
        begin
          Memo1.Lines.Add('No responce');
        end;
    end;
  finally
    l.Free();
    thr.Free();
  end;
end;

const
  SOH = ' 01 ';
  ISI = ' 1F ';
  STX = ' 02 ';
  ETX = ' 03 ';
  DAD = ' 00 ';
  SAD = ' 00 ';
  DLE = ' 10 ';
  HT  = ' 09 ';
  FF  = ' 0C ';

function EncodeStr(const s: string): string;
var i: int;
begin
  for i := 1 to Length(s) do
    Result := Result + IntToHex(Ord(s[i]), 2) + ' ';

  Result := Trim(Result);
end;

procedure TForm1.FormCreate(Sender: TObject);
var FNC, DataDLEHead, DataDLESet, dt, prm: string;

  function FormatString(): string;
  begin
    Result := DLE + SOH + DAD + SAD + DLE + ISI + FNC + DataDLEHead;
  end;

begin
  req := TSPT961RequestCreator.Create();

  Edit1.Text := IntToHex(Ord('1'), 2) + ' ' +
                IntToHex(Ord('2'), 2) + ' ' +
                IntToHex(Ord('3'), 2) + ' ' +
                IntToHex(Ord('4'), 2) + ' ' +
                IntToHex(Ord('5'), 2);

  // В номерах пареметров применяется следующий формат нумерации:
  // номера передаются в текстовом формате
  // Например, 0 - 56 передается как HT 00 HT 05 06 HT FF
  // схема общая как для индексных, так и для обычных

  FNC := '1D';  // 1D - чтение обычного параметра
  DataDLESet := HT + EncodeStr('1') + HT + EncodeStr('160') + FF;
  DataDLEHead := DLE + STX + DataDLESet + DLE + ETX;

  Edit1.Text := FormatString();
  // пример - 10 01 00 00 10 1F 1D 10 02 09 00 09 00 03 0C 10 03



  FNC := '0C';  // 0C - чтение индексного параметра
  DataDLESet := HT + EncodeStr('0') + HT + EncodeStr('96') + HT + EncodeStr('1') + HT + EncodeStr('9') + FF;
  DataDLEHead := DLE + STX + DataDLESet + DLE + ETX;

  Edit1.Text := FormatString();
  // пример - 10 01 00 00 10 1F 0C 10 02 09 00 09 02 02 09 00 09 03 0C 10 03


  FNC := '18';  // 18 - чтение архива
  prm := HT + EncodeStr('0') + HT + EncodeStr('65532') + FF; // 0 - 65532 - суточный архив
  dt := HT + EncodeStr('14') + HT + EncodeStr('10') + HT + EncodeStr('2013') +
        HT + EncodeStr('0') + HT + EncodeStr('0')+ HT + EncodeStr('0') + FF ; // dd mm yy hh nn ss
  DataDLESet := prm + dt;
  DataDLEHead := DLE + STX + DataDLESet + DLE + ETX;

  Edit1.Text := FormatString();

  // пример - 10 01 00 00 10 1F 18 10 02 09 30 09 36 35 35 33 32 0C 09 31 34 09 31 30 09 31 33 09 30 09 30 09 30 0C 10 03 24 EE
  { Ответ:
  FF FF 10 01 00 00 10 1F 20 10 02
  09 00 09 06 05 05 03 02 0C
  09 31 34 09 31 30 09 31 33 09 30 09 30 09 30 0C
  09 31 34 09 31 30 09 31 33 09 30 09 30 30 09 30 30 0C
  09 31 33 09 31 30 09 31 33 09 30 09 30 09 30 30 0C
  09 32 34 0C 09 30 0C 09 30 2E 32 0C 09 30 2E 31 30 31 33 32 0C 09 32 30 0C 09 32 34 0C 09 30 0C 09 31 38 2E 33 36 35 0C 09 30 0C 09 30 0C 09 30 0C 09 30 2E 30 30 0C 09 30 2E 30 30 0C 09 30 2E 30 30 30 0C 09 30 30 30 30 31 31 31 30 0C 09 30 2E 30 30 0C 09 30 2E 30 30 0C 09 32 34 0C 09 30 0C 09 31 33 2E 38 31 31 0C 09 30 0C 09 30 0C 09 30 0C 09 30 2E 30 30 0C 09 30 2E 30 30 0C 09 30 2E 30 30 30 0C 09 30 30 30 30 31 31 31 30 0C 10 03 E1 79 (
    ||||||||||||||||| - сам запрос и указатель 1
  |14|10|13|0|0|0| - указатель 2 из запроса
  |14|10|13|0|00|00| - метка времени архива
  |13|10|13|0|0|00|  - ближайшая метка назад
  |24||0||0.2||0.10132||20||24||0||18.365||0||0||0||0.00||0.00||0.000||00001110||0.00||0.00||24||0||13.811||0||0||0||0.00||0.00||0.000||00001110|||ny) - Ok
  }
  FNC := '19';  // 19 - структура архива
  prm := HT + EncodeStr('0') + HT + EncodeStr('65532') + FF; // 0 - 65532 - суточный архив
  //prm := HT + EncodeStr('0') + HT + EncodeStr('65530') + FF; // 0 - 65530 - часовой архив
  DataDLESet := prm;
  DataDLEHead := DLE + STX + DataDLESet + DLE + ETX;

  Edit1.Text := FormatString();

  // пример - месячный  10 01 00 00 10 1F 19 10 02 09 30 09 36 35 35 33 32 0C 09 31 34 09 31 30 09 31 33 09 30 09 30 09 30 0C 10 03 2D F1
  { Ответ
  FF FF 10 01 00 00 10 1F 21 10 02 09 00 09 06 05 05 03 02 0C 09 74 A8 09 E7 09 30 09 39 31 0C 09 92 E5 A2 09 27 43 09 30 09 37 32 0C 09 90 E5 A2 09 8C
  8F A0 09 30 09 37 35 0C 09 90 A1 09 8C 8F A0 09 30 09 37 39 0C 09 92 AD A2 09 27 43 09 30 09 38 33 0C 09 74 6F E2 30 31 09 E7 09 31 09 32 33 35 0C 09
  51 6F E2 30 31 09 AC 33 2F E7 09 31 09 31 39 36 0C 09 92 E2 30 31 09 27 43 09 31 09 32 30 31 0C 09 50 E2 30 31 09 8C 8F A0 09 31 09 32 30 36 0C 09 84
  31 E2 30 31 09 20 09 31 09 32 33 39 0C 09 84 32 E2 30 31 09 20 09 31 09 32 34 33 0C 09 4D E2 30 31 09 E2 09 31 09 32 31 31 0C 09 57 E2 30 31 09 83 84
  A6 09 31 09 32 31 36 0C 09 56 6F E2 30 31 09 AC 33 09 31 09 32 32 31 0C 09 8D 91 AE E2 30 31 09 20 09 31 09 32 34 36 0C 09 64 4D AF 31 09 E2 09 31 09
  34 30 31 0C 09 64 57 AF 31 09 83 84 A6 09 31 09 34 30 36 0C 09 74 6F E2 30 32 09 E7 09 32 09 32 33 35 0C 09 51 6F E2 30 32 09 AC 33 2F E7 09 32 09 31
  39 36 0C 09 92 E2 30 32 09 27 43 09 32 09 32 30 31 0C 09 50 E2 30 32 09 8C 8F A0 09 32 09 32 30 36 0C 09 84 31 E2 30 32 09 20 09 32 09 32 33 39 0C 09
  84 32 E2 30 32 09 20 09 32 09 32 34 33 0C 09 4D E2 30 32 09 E2 09 32 09 32 31 31 0C 09 57 E2 30 32 09 83 84 A6 09 32 09 32 31 36 0C 09 56 6F E2 30 32
  09 AC 33 09 32 09 32 32 31 0C 09 8D 91 AE E2 30 32 09 20 09 32 09 32 34 36 0C 10 03 06 37 
   (  ||||||!|||||||||||
  |tи|ч|0|91|
  |Тхв|'C|0|72|
  |Рхв|МПа|0|75|
  |Рб|МПа|0|79|
  |Тнв|'C|0|83|
  |toт01|ч|1|235|
  |Qoт01|м3/ч|1|196|
  |Тт01|'C|1|201|
  |Pт01|МПа|1|206|
  |Д1т01| |1|239|
  |Д2т01| |1|243|
  |Mт01|т|1|211|
  |Wт01|ГДж|1|216|
  |Voт01|м3|1|221|
  |НСот01| |1|246|
  |dMп1|т|1|401|
  |dWп1|ГДж|1|406|
  |toт02|ч|2|235|
  |Qoт02|м3/ч|2|196|
  |Тт02|'C|2|201|
  |Pт02|МПа|2|206|
  |Д1т02| |2|239|
  |Д2т02| |2|243|
  |Mт02|т|2|211|
  |Wт02|ГДж|2|216|
  |Voт02|м3|2|221|
  |НСот02| |2|246|
  |||7) - Ok
  }

  // часовой -  10 01 00 00 10 1F 19 10 02 09 30 09 36 35 35 33 32 0C 09 31 34 09 31 30 09 31 33 09 30 09 30 09 30 0C 10 03 2D F1

  FNC := '0E';  // OE - временной массив
  prm := HT + EncodeStr('0') + HT + EncodeStr('72') + FF; // 0 - 65532 - суточный архив

  dt := HT + EncodeStr('21') + HT + EncodeStr('09') + HT + EncodeStr('2013') +
        HT + EncodeStr('0') + HT + EncodeStr('0')+ HT + EncodeStr('0') + FF;

  DataDLESet := prm + dt;
  dt := HT + EncodeStr('1') + HT + EncodeStr('09') + HT + EncodeStr('2013') +
        HT + EncodeStr('0') + HT + EncodeStr('0')+ HT + EncodeStr('0') + FF;


  DataDLESet := DataDLESet + dt;
  DataDLEHead := DLE + STX + DataDLESet + DLE + ETX;

  Edit1.Text := FormatString();  
end;

procedure TForm1.CommonPrmClick(Sender: TObject);
begin
  req.DevNumber := StrToInt(leDev.Text);
  req.Chn := StrToInt(LabeledEdit1.Text);
  req.Prm := StrToInt(LabeledEdit2.Text);
  req.CommonPrm();
  Edit1.Text := req.ResultString();
  Button1.Click();
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  req.DevNumber := StrToInt(leDev.Text);
  req.Chn := StrToInt(LabeledEdit1.Text);
  req.Prm := StrToInt(LabeledEdit2.Text);
  req.archDT1 := Floor(Now());
  req.archDT2 := Now();
  req.TimeArray();
  Edit1.Text := req.ResultString();
  Button1.Click();
end;

procedure TForm1.SaveParams();
var f: TIniFile;
begin
  try
    f := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

    f.WriteInteger('COMMON', 'DEVNUM', StrToInt(leDev.Text));
    f.WriteInteger('COMMON', 'CHANNEL', StrToInt(LabeledEdit1.Text));
    f.WriteInteger('COMMON', 'PRM', StrToInt(LabeledEdit2.Text));

    f.UpdateFile();
    f.Free();
  except
  end;
end;

procedure TForm1.LoadParams();
var f: TIniFile;
begin
  try
    f := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));

    leDev.Text := f.ReadString('COMMON', 'DEVNUM', leDev.Text);
    LabeledEdit1.Text := f.ReadString('COMMON', 'CHANNEL', LabeledEdit1.Text);
    LabeledEdit2.Text := f.ReadString('COMMON', 'PRM', LabeledEdit2.Text);

    f.Free();
  except
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LoadParams();
end;

end.
