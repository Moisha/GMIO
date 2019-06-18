unit TimeZone;
//  ******************************************************************
//  ������ ������� ������ ���� ������� � �������� �������:
//  1) ������ ������� ������ � ������ � ��������� ���� ������ ��� ���� �� ����.
//     ������� ������������� �� ����� � ���� ��� ��� �� ������������.
//  2) ������, ��� ������ ��������� �� ���� ��� ������ 2011. ������ ���������
//     ��������� ���������� ������� ��� � ���������� (�� ����� live update),
//     � ��������� - � ����������� (������� ������ ������ ����).
//  ����� - ���������� �� ������ �� ������� Windows, ����� �� ������� ����
//  �� ������������� ����, ��� ���� ����� � ��������� ��������. �� ��������
//   �) �������� ������ ����� (� ������� � ������ � ��� ���� �������� ���)
//   �) ��������� ����������� (��������� ������������ ������ ������� ������,
//      �� � ������ ��� �������� ����� ������ � 2000/XP)
//   �) ������������� ����������, ����� ������ ����� ���� ���������, �� � ��������.
//      ���� ������ �� �����, � ��� ������� ����� ����� � ������ �.�.�����������)
//  ��� ��� ������ procedure ReadRegTimeZones. ������ �������������:
//      ...
//      ReadRegTimeZones(ComboBox1.Items);
//      ComboBox1.ItemIndex := ComboBox1.Items.IndexOfObject(Pointer(300));
//  ******************************************************************
interface

uses Windows, Classes;

procedure ReadRegTimeZones( // ������� ������ ������� ������ �� �������
            Target: TStrings; // ������� ����, ������ � Objects - Pointer(Integer(�����_��_UTC_�_�������))
            BestLen:integer = 61; // ����� ���������� ����� �������� ������, �� ��� �������� - ������ ���� ���������
            OurFilter:boolean = True // ������ ���������� ������ ����� �� ���������� [0..12] ����� � ��� ���� �� ����������
          );

implementation

uses Registry, Math, StrUtils, SysUtils;

const  // ��� ������ ������ ���� ������� � ����� �������
  KnownCity: array [1..32] of string = (
    '���������',    'Reykjavik',
    '������',       'London',
    '�������',      'Belgrade',
    '�������������','Petropavlovsk',
    '�������',      'Magadan',
    '�����������',  'Vladivostok',
    '������',       'Yakutsk',
    '�������',      'Irkutsk',
    '����������',   'Krasnoyarsk',
    '����',         'Baku',
    '����',         'Kiev',
    '�����',        'Minsk',
    '������',       'Astana',
    '�����������',  'Novosibirsk',
    '������',       'Moscow',
    '������������', 'katerinburg');

function HasKnownCity(s:string): integer;
begin
  Result := High(KnownCity);
  while Result >= Low(KnownCity) do
    if Pos(KnownCity[Result], s) > 0 then
      exit
    else
      Dec(Result);
end;

function IntSort(List: TStringList; Index1, Index2: Integer): Integer;
begin  // ���������� � ������ ������� - �� ������ �� UTC
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
  if Result = 0 then   // ����� - �� '(UTC' ��� '(GMT' � ������ ��������
    Result := Pos('(', List[Index2]) - Pos('(', List[Index1]);
  if Result = 0 then   // ����� - �� ������� ������������ ��������
    Result := HasKnownCity(List[Index2]) - HasKnownCity(List[Index1]);
end;

procedure GroupTimeZones(SL:TStringList; BestLen:integer);
var i,k,lst,len:integer; Tmp:TStringList; s:string;
begin
  Tmp := TStringList.Create;
  try
    lst := MaxInt;
    i := 0;
    k := 0;
    while i < SL.Count do begin
      if lst <> Integer(SL.Objects[i]) then begin
        lst := Integer(SL.Objects[i]);
        k := Tmp.AddObject(Trim(SL[i]),Pointer(lst));
      end else begin
        s := Trim(SL[i]);
        if Pos('(', s) = 1 then begin
          len := Pos(')', s);
          if len > 0 then
            Delete(s, 1, len);
        end;
        s := Trim(s);
        if (s <> '') then begin
          s := Tmp.Strings[k] + ', ' + s;
          if Length(s) <= BestLen then
            Tmp.Strings[k] := s;
        end;
      end;
      Inc(i);
    end;
    SL.Assign(Tmp);
  finally
    Tmp.Free;
  end;
end;

// � ������ ������ ���� ��� ����� �� UTC �� UTC+12 - ���� ���� � ������� �� ���
procedure AddFromFixZones(S:TStringList);
var i:integer;
begin
  for i:=0 to 12 do
    if S.IndexOfObject(Pointer(i*60)) < 0 then
      S.AddObject('(UTC' + IfThen(i>0, '+'+IntToStr(i)) + ')', Pointer(i*60));
  S.CustomSort(IntSort);
end;

const TZKey = 'Software\Microsoft\Windows NT\CurrentVersion\Time Zones';

procedure ReadRegTimeZonesPrim(S:TStringList; BestLen:integer; OurFilter:boolean);
var R:TRegistry; i:integer; disp:string;
    TZI: record Bias:integer; Dummy:array[0..15] of integer; end;

  function GoodZone: boolean;
  begin
    Result := False;
    if disp = '' then exit;  // ���� ��� ����� ��� �� �����
    try
      Result := (R.ReadBinaryData('TZI', TZI, SizeOf(TZI)) >= SizeOf(TZI.Bias))
         and ((not OurFilter)
                 or (InRange(TZI.Bias, -720, 0)  // �� �������� �� GMT
                     and (TZI.Bias mod 60 = 0))); // � ��� ��� ���� �����, � ������ ����� � ������� �������
    except
      // ��� ����� TZI ��� �� ������������ �������
    end;
  end;

begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.OpenKeyReadOnly(TZKey) then begin
      R.GetKeyNames(S);
      R.CloseKey;
      for i:=S.Count-1 downto 0 do begin
        if R.OpenKeyReadOnly(TZKey + '\' + S[i]) then begin
          disp := R.ReadString('Display');
          if GoodZone() then begin
            S[i] := AnsiReplaceStr(disp, ':00', '');
            S.Objects[i] := Pointer(-TZI.Bias);
          end else
            S.Delete(i);
          R.CloseKey;
        end else  // ������-�� ��� ���� �� ������, �� ��� ���� �� �����...
          S.Delete(i);
      end;
    end;
    S.CustomSort(IntSort);
    if OurFilter then
      GroupTimeZones(S, BestLen);
    AddFromFixZones(S);  
  finally
    R.Free;
  end;
end;

procedure ReadRegTimeZones(Target: TStrings; BestLen:integer = 61; OurFilter:boolean = True);
var SL: TStringList;
    i: integer;
begin
  SL := TStringList.Create;
  try
    ReadRegTimeZonesPrim(SL, BestLen, OurFilter);

    for i := 0 to SL.Count - 1 do
      SL.Objects[i] := TObject(integer(SL.Objects[i]) div 60);

    Target.Assign(SL);
  finally
    SL.Free;
  end;    
end;

end.
