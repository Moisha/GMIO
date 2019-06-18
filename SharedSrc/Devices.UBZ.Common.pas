////////////////////////////////////////////
// ��������� � ������ CRC ��� �������� ���� ���-302 � ��-101
////////////////////////////////////////////
unit Devices.UBZ.Common;

interface

uses GMGlobals, Windows, Classes, StrUtils;

procedure DecodeUBZAlarms(dwErr: DWORD; sl: TSTrings); overload;
function DecodeUBZAlarms(dwErr: DWORD; const Separator: string): string; overload;

procedure DecodeTR101Alarms(dwErr: DWORD; sl: TSTrings); overload;
function DecodeTR101Alarms(dwErr: DWORD; const Separator: string): string; overload;

implementation


procedure DecodeUBZAlarms(dwErr: DWORD; sl: TSTrings);
  const UBZ_AlarmStr: array [0..23] of String = (
     '������������ ������� � �����' // 241:0
    ,'�� �������� ����������' // 241:1
    ,'�� ��������� �� ����� (�� ���� ������� ������������������)' // 241:2
    ,'�� ���������� ��������� �������� ������������������ �� ���� � �������� ������������������ �� ����������' // 241:3
    ,'�� �������� ������������������ �� ����' // 241:4
    ,'����������� ������� � �����' // 241:5
    ,'��������� ����' // 241:6
    ,'���������� ������' // 241:7
    ,'�� ���������� ������ ����������� ������� �������' // 241:8
    ,'�� ���������� ������ ����������� ������� �������' // 241:9
    ,'�� ������� ����������� ���' // 241:10
    ,'�� ������� ����� ��� ����������� ���� �������� (������ ����������)' //241:11
    ,'�� ������������ ��������� ����������' // 241:12
    ,'�� ������������� ��������� ����������' // 241:13
    ,'�� �������� ���' // 241:14
    ,'�� ������������ ������������� �������� ������� ���������' // 241:15
    ,'�� ������ ������ �������������� ����������' // 242:0
    ,'��������� ������� ��������� ��� ����������� ���������� �����' // 242:1
    ,'��������� ������� ��������� � ������������ ���������� ����� ������������� �������� ������ ����� � ����' //	242:2
    ,'�� �.�. ������� ����������� 1' // 242:3
    ,'�� ������ ������� ����������� 1' // 242:4
    ,'�� �.�. ������� ����������� 2' // 242:5
    ,'�� ������ ������� ����������� 2' // 242:6
    ,'�� ������ ����' // 242:7
  );
var i: int;
begin
  if dwErr = $FFFF then
    sl.Add('����� ����� � ��������')
  else
  for i := 0 to High(UBZ_AlarmStr) do
  if (dwErr shr i) and 1 > 0 then
    sl.Add(UBZ_AlarmStr[i]);
end;

function DelimitedSL(sl: TStrings; const Separator: string): string;
var i: int;
begin
  if sl.Count = 0 then
    Result := '��� ������'
  else
  begin
    Result := '';
    for i := 0 to sl.Count - 1 do
      Result := Result + IfThen(i > 0, Separator) + sl[i];
  end;
end;

function DecodeUBZAlarms(dwErr: DWORD; const Separator: string): string;
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    DecodeUBZAlarms(dwErr, sl);
    Result := DelimitedSL(sl, Separator);
  finally
    sl.Free();
  end;
end;

procedure DecodeTR101Alarms(dwErr: DWORD; sl: TSTrings);
  const TR101_AlarmStr: array [0..9] of String = (
    '����� EEPROM',
    '������ ���������',
    '��������� ������� 1',
    '��������� ������� 2',
    '��������� ������� 3',
    '��������� ������� 4',
    '����� ������� 1',
    '����� ������� 2',
    '����� ������� 3',
    '����� ������� 4'
    );

var i: int;
begin
  if dwErr = $FFFF then
    sl.Add('����� ����� � ��������')
  else
  for i := 0 to High(TR101_AlarmStr) do
  if (dwErr shr i) and 1 > 0 then
    sl.Add(TR101_AlarmStr[i]);
end;

function DecodeTR101Alarms(dwErr: DWORD; const Separator: string): string;
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    DecodeUBZAlarms(dwErr, sl);
    Result := DelimitedSL(sl, Separator);
  finally
    sl.Free();
  end;
end;

end.
