unit TimeZone;
//  ******************************************************************
//  Модуль призван решить пару проблем с часовыми поясами:
//  1) Список часовых поясов в России в последние годы меняют все кому не лень.
//     Области перепрыгивают из пояса в пояс так что не предугадаешь.
//  2) Похоже, вся Россия сдвинется на один час осенью 2011. Причем наверняка
//     некоторые компьютеры сделают это с опозданием (не очень live update),
//     а некоторые - с опережением (вручную выбрав другой пояс).
//  Выход - зацепиться за список из реестра Windows, чтобы по крайней мере
//  не противоречить тому, что юзер видит в системных утилитах. Но придется
//   а) выкинуть лишние пояса (в Уругвае и Непале у нас пока клиентов нет)
//   б) дополнить недостающие (некоторые оптимизаторы сильно сжимают реестр,
//      да и список там появился вроде только в 2000/XP)
//   в) сгруппировать оставшиеся, иначе список может быть длинноват, да и неудобен.
//      Ведь храним мы число, и для каждого числа выбор в списке д.б.однозначным)
//  Все это делает procedure ReadRegTimeZones. Пример использования:
//      ...
//      ReadRegTimeZones(ComboBox1.Items);
//      ComboBox1.ItemIndex := ComboBox1.Items.IndexOfObject(Pointer(300));
//  ******************************************************************
interface

uses Windows, Classes;

procedure ReadRegTimeZones( // достает список часовых поясов из реестра
            Target: TStrings; // заносит сюда, причем в Objects - Pointer(Integer(Сдвиг_от_UTC_в_минутах))
            BestLen:integer = 61; // можно ограничить длину названий поясов, но без гарантий - только если получится
            OurFilter:boolean = True // фильтр пропускает только пояса со смещениями [0..12] часов и при этом их группирует
          );

implementation

uses Registry, Math, StrUtils, SysUtils;

const  // эти города должны быть первыми в своих группах
  KnownCity: array [1..32] of string = (
    'Рейкьявик',    'Reykjavik',
    'Лондон',       'London',
    'Белград',      'Belgrade',
    'Петропавловск','Petropavlovsk',
    'Магадан',      'Magadan',
    'Владивосток',  'Vladivostok',
    'Якутск',       'Yakutsk',
    'Иркутск',      'Irkutsk',
    'Красноярск',   'Krasnoyarsk',
    'Баку',         'Baku',
    'Киев',         'Kiev',
    'Минск',        'Minsk',
    'Астана',       'Astana',
    'Новосибирск',  'Novosibirsk',
    'Москва',       'Moscow',
    'Екатеринбург', 'katerinburg');

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
begin  // сортировка в первую очередь - по сдвигу от UTC
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
  if Result = 0 then   // затем - по '(UTC' или '(GMT' в начале названия
    Result := Pos('(', List[Index2]) - Pos('(', List[Index1]);
  if Result = 0 then   // затем - по степени узнаваемости названия
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

// в списке должны быть все пояса от UTC до UTC+12 - даже если в реестре их нет
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
    if disp = '' then exit;  // пояс без имени нам не нужен
    try
      Result := (R.ReadBinaryData('TZI', TZI, SizeOf(TZI)) >= SizeOf(TZI.Bias))
         and ((not OurFilter)
                 or (InRange(TZI.Bias, -720, 0)  // от Камчатки до GMT
                     and (TZI.Bias mod 60 = 0))); // у нас все часы целые, а всякие Ираны с Непалом выкинем
    except
      // нет ключа TZI или он неизвестного формата
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
        end else  // вообще-то так быть не должно, но чем Билл не шутит...
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
