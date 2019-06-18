////////////////////////////////////////////
// Кодировка и расчет CRC для приборов типа УБЗ-302 и ТР-101
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
     'максимальная токовая в фазах' // 241:0
    ,'по тепловой перегрузке' // 241:1
    ,'от замыкания на землю (по току нулевой последовательности)' // 241:2
    ,'по превышению кратности обратной последовательности по току к обратной последовательности по напряжению' // 241:3
    ,'по обратной последовательности по току' // 241:4
    ,'минимальная токовая в фазах' // 241:5
    ,'затянутый пуск' // 241:6
    ,'блокировка ротора' // 241:7
    ,'по достижению порога температуры первого датчика' // 241:8
    ,'по достижению порога температуры второго датчика' // 241:9
    ,'по порядку чередования фаз' // 241:10
    ,'по наличию токов при отключенном реле нагрузки (авария контактора)' //241:11
    ,'по минимальному линейному напряжению' // 241:12
    ,'по максимальному линейному напряжению' // 241:13
    ,'по перекосу фаз' // 241:14
    ,'по минимальному сопротивлению изоляции обмоток двигателя' // 241:15
    ,'по аварии канала дистанционного управления' // 242:0
    ,'аварийный останов двигателя без возможности повторного пуска' // 242:1
    ,'аварийный останов двигателя с возможностью повторного пуска одновременным нажатием кнопок ВВЕРХ и ВНИЗ' //	242:2
    ,'по к.з. датчика температуры 1' // 242:3
    ,'по обрыву датчика температуры 1' // 242:4
    ,'по к.з. датчика температуры 2' // 242:5
    ,'по обрыву датчика температуры 2' // 242:6
    ,'по обрыву фазы' // 242:7
  );
var i: int;
begin
  if dwErr = $FFFF then
    sl.Add('Обрыв связи с прибором')
  else
  for i := 0 to High(UBZ_AlarmStr) do
  if (dwErr shr i) and 1 > 0 then
    sl.Add(UBZ_AlarmStr[i]);
end;

function DelimitedSL(sl: TStrings; const Separator: string): string;
var i: int;
begin
  if sl.Count = 0 then
    Result := 'Нет аварий'
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
    'отказ EEPROM',
    'ошибка параметра',
    'замыкание датчика 1',
    'замыкание датчика 2',
    'замыкание датчика 3',
    'замыкание датчика 4',
    'обрыв датчика 1',
    'обрыв датчика 2',
    'обрыв датчика 3',
    'обрыв датчика 4'
    );

var i: int;
begin
  if dwErr = $FFFF then
    sl.Add('Обрыв связи с прибором')
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
