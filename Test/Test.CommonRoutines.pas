unit Test.CommonRoutines;

interface

uses Windows, TestFrameWork, GMGlobals;

type
  TCommonRoutinesTest = class(TTestCase)
  private
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MultiLineSplit;
    procedure Zip;
    procedure Md5;
    procedure Buf_WriteStringMd5;
    procedure Buf_ReadMd5String;
    procedure Float_FormatToShow();
    procedure ExtractPrefix;
    procedure EncodedSingle;
    procedure EnumName;
  end;

implementation

uses Math, Classes, SysUtils, GMConst, System.TypInfo;

{ TCommonRoutinesTest }

procedure TCommonRoutinesTest.SetUp;
begin
  inherited;

end;

procedure TCommonRoutinesTest.TearDown;
begin
  inherited;

end;

procedure TCommonRoutinesTest.Buf_ReadMd5String;
var buf: arrayofByte;
    example, s: string;
begin
  example := '12 2a 34 4e 56 67 89 00 08 07 06 12 33 34 56 78 aa ab dd';
  buf := TextNumbersStringToArray(example);
  s := lowerCase(ReadMd5String(buf, 1));
  Check(s = StringReplace(Copy(example, 3, 16 * 3), ' ', '', [rfReplaceAll]));
end;

procedure TCommonRoutinesTest.Buf_WriteStringMd5;
var buf: array [0..20]of byte;
    i: int;
    s: string;
begin
  s := '11111 2222222222222222222 33 44 55 66 77 88  AbdbvfhiruhJJJJJ';
  ZeroMemory(@buf[0], 20);
  Check(WriteStringMd5(buf, 1, s) = 17, 'Len');

  Check(buf[0] = 0);
  for i := 1 to 16 do
    Check(buf[i] <> 0, 'buf ' + IntToStr(i) + ' = 0');
end;

procedure TCommonRoutinesTest.EncodedSingle;
{var buf: ArrayOfByte;
    buf2: array[0..3] of byte;
    i, j, k, l: int;
    sl: TStringList;
    res: double;
}
begin
{  buf := TextNumbersStringToArray('1A 2A 43 5E');
  sl := TStringList.Create();

  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      if i = j then continue;
      for k := 0 to 3 do
      begin
        if k in [i, j] then continue;
        for l := 0 to 3 do
        begin
          if l in [i, j, k] then continue;
          buf2[0] := buf[i];
          buf2[1] := buf[j];
          buf2[2] := buf[k];
          buf2[3] := buf[l];

          try
            res := ReadSingleInv(buf2, 0);
            sl.Add(Format('%d %d %d %d: %s - %g', [i, j, k, l, ArrayToString(buf2, 4, false, true), res]));
          except

          end;
        end;
      end;
    end;

  ShowMessageBox(sl.Text);
  sl.Free();
}
  Check(true);
end;

procedure TCommonRoutinesTest.EnumName;
var
  s: string;
begin
  s := GetEnumName(TypeInfo(T485RequestType), ord(T485RequestType.rqtUBZ_ALARMS));
  CheckEquals('rqtUBZ_ALARMS', s);
  s := T485RequestType.rqtTR101_SP4.ToString();
  CheckEquals('rqtTR101_SP4', s);
end;

procedure TCommonRoutinesTest.ExtractPrefix;
var name: string;
    number: int;
begin
  GetNamePrefix('curve-1', name, number);
  Check((name = 'curve-') and (number = 1));

  GetNamePrefix('curve', name, number);
  Check((name = 'curve') and (number = -1));

  GetNamePrefix('10', name, number);
  Check((name = '') and (number = 10));
end;

procedure TCommonRoutinesTest.Float_FormatToShow;
begin
  Check(FormatFloatToShow(1.1) = '1.1');
  Check(FormatFloatToShow(10.1) = '10');
end;

procedure TCommonRoutinesTest.Md5;
var s: string;
begin
  s := '11111 2222222222222222222 33 44 55 66 77 88  AbdbvfhiruhJJJJJ';
  Check(CalcMD5(s) <> s);
  Check(CalcMD5(s) <> '');
  Check(CalcMD5(s) = CalcMD5(s));
  Check(CalcMD5(s) <> CalcMD5(s + 'A'));
  Check(CalcMD5(s) <> CalcMD5(UpperCase(s)));
end;

procedure TCommonRoutinesTest.MultiLineSplit;
begin
  Check(SplitToThreeLines('') = '', '1');
  Check(SplitToThreeLines('11111 222222222') = '11111'#13#10'222222222', '2');
  Check(SplitToThreeLines('11111 ,.\ 222222222') = '11111 ,.\'#13#10'222222222', '2_1');
  Check(SplitToThreeLines('11111 22222222222222 33') = '11111'#13#10'22222222222222'#13#10'33', '3');
  Check(SplitToThreeLines('11111 2222222222222222222 33 44') = '11111'#13#10'2222222222222222222'#13#10'33 44', '4');
  Check(SplitToThreeLines('11111 2222222222222222222 33 44 55 66 77 88') = '11111'#13#10'2222222222222222222'#13#10'33 44 55 66 77 88', '5');
  Check(SplitToThreeLines('Т1 Давление теплоносителя') = 'Т1'#13#10'Давление'#13#10'теплоносителя', '6');
end;

procedure TCommonRoutinesTest.Zip;
{var s, s2: TStringStream;
    ms, ms2: TMemoryStream;
    i: int;}
begin
  Check(true);
{  try
    s := TSTringStream.Create('');
    for i := 1 to 1000 do
      s.WriteString(Chr(Ord('A') + RandomRange(0, 25)));

    ms := CompressStream(s, '12345');

    Check(ms.Size > 0, 'ms.Size > 0');
    Check(ms.Size < s.Size, 'Size < s.Size');

    ms2 := DeCompressStream(ms);
    Check(ms2 = nil, 'ms2 password failed');

    ms2 := DeCompressStream(ms, '12345');

    s2 := TSTringStream.Create('');
    ms2.Seek(0, soFromBeginning);
    s2.CopyFrom(ms2, ms2.Size);

    Check(s.DataString = s2.DataString, 'DataString');
  finally
    TryFreeAndNil(s);
    TryFreeAndNil(s2);
    TryFreeAndNil(ms);
    TryFreeAndNil(ms2);
  end;}
end;

initialization
  RegisterTest('CommonRoutines', TCommonRoutinesTest.Suite);
end.
