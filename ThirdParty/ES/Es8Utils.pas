unit Es8Utils;

interface

Uses
  Windows, SysUtils, Classes, System.Variants, System.Generics.Defaults, StrUtils, Registry, Menus,
  System.DateUtils, System.RegularExpressions, System.Generics.Collections,
{$IFNDEF ServerOprosa} //PSO не использует JCL
  JclSysInfo,
{$ENDIF}
  esExceptions;

const
   TRUE_AS_STRING = 'True';
   FALSE_AS_STRING = 'False';

type
  TEsStringList = class(TStringList)
  public
    constructor CreateFromArray(const source: array of string);
    function ToStringWithDelimiter(const delimiter: string): string;
  end;

  {{
  Аналог (обертка над) TStringList, TEsStringList
  Предназначен для утилитных операций со строками.
  Позволяет не писать Free для временного объекта}
  IStringListInterface = interface
  ['{4772449B-8E49-430E-B19F-BFC6DA2185BA}']
    function  OriginalStrings(): TStrings;
    procedure SetValue(const Name, Value: string);
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    function GetCommaText: string;
    procedure SetCommaText(const Value: string);
    procedure SetText(const Value: string);
    function  GetText: string;
    function  GetCount(): Integer;
    function  GetItem(Index: Integer): string;
    function  GetObject(Index: Integer): TObject;
    function  ToStringWithDelimiter(const delimiter: string): string;
    procedure Add(const s: string);overload;
    procedure AddIfNotEmpty(const s: string);
    procedure AddObject(const s: string; obj: TObject); overload;
    procedure AddObject(const s: string; obj: integer); overload;
    procedure Add(const FormatStr: string; const Args: array of const);overload;
    procedure AddStrings(Strings: IStringListInterface);overload;
    procedure AddStrings(Strings: TStrings);overload;
    procedure AddTo(dest: TStrings);overload;
    procedure AddTo(dest: ExceptionWithDetails);overload;
    function  GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function  GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    function  GetDelimitedText: string;
    procedure SetDelimitedText(const Value: string);
    procedure Delete(Index: Integer);
    procedure SaveToFile(const FileName: string);
    function  GetEnumerator: TStringsEnumerator;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear();
    function  Contains(const str: string): boolean;
    function  ContainsValue(const valueName: string): boolean;
    property  Count: Integer read GetCount;
    property  Text: string read GetText write SetText;
    property  CommaText: string read GetCommaText write SetCommaText;
    property  Values[const Name: string]: string read GetValue write SetValue;
    property  Items[Index: Integer]: string read GetItem;default;
    property  Objects[Index: Integer]: TObject read GetObject;
    property  Delimiter: Char read GetDelimiter write SetDelimiter;
    property Names[Index: Integer]: string read GetName;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
  end;

  {{См. IStringListInterface}
  TStringListInterfaced = class(TInterfacedObject, IStringListInterface)
  private
    FStrings: TEsStringList;
    function GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    function GetDelimitedText: string;
    procedure SetDelimitedText(const Value: string);
  protected
    function  OriginalStrings(): TStrings;
    procedure SetValue(const Name, Value: string);
    function GetValue(const Name: string): string;
    function GetName(Index: Integer): string;
    function GetCommaText: string;
    procedure SetCommaText(const Value: string);
    procedure SetText(const Value: string);
    function GetText: string;
    function  GetItem(Index: Integer): string;
    function  GetObject(Index: Integer): TObject;
    function GetCount(): Integer;
    function ToStringWithDelimiter(const delimiter: string): string;
    procedure Add(const s: string);overload;
    procedure AddIfNotEmpty(const s: string);
    procedure Add(const FormatStr: string; const Args: array of const);overload;
    procedure AddObject(const s: string; obj: TObject); overload;
    procedure AddObject(const s: string; obj: integer); overload;
    procedure AddStrings(Strings: IStringListInterface);overload;
    procedure AddStrings(Strings: TStrings);overload;
    procedure AddTo(dest: TStrings);overload;
    procedure AddTo(dest: ExceptionWithDetails);overload;
    procedure Delete(Index: Integer);
    procedure SaveToFile(const FileName: string);
    function GetEnumerator: TStringsEnumerator;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear();
    function  Contains(const str: string): boolean;
    function  ContainsValue(const valueName: string): boolean;
    property  Count: Integer read GetCount;
    property  Text: string read GetText write SetText;
    property Names[Index: Integer]: string read GetName;
    property  Values[const Name: string]: string read GetValue write SetValue;
    property  CommaText: string read GetCommaText write SetCommaText;
    property  Items[Index: Integer]: string read GetItem;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
  public
    constructor Create(const aText: string = '');
    constructor CreateWithDelimiter(const aTextToDelimit: string; aDelimiter: Char);
    destructor Destroy;override;
  end;

  {{Обертка над реестром для упрощения вызовов}
  TEsRegistry = class
  private
    FRegistry: TRegistry;
    FKeyOpened: boolean;
    function NormalizeName(const name: string): string;
  protected
    function  TryReadInteger(const Name: string; out value: Integer): boolean;
    function  TryReadBoolean(const Name: string; out value: boolean): boolean;
  public
    constructor Create(const rootKeyName: string; canWrite: boolean);
    destructor  Destroy();override;
    procedure WriteString(const Name, Value: string);
    procedure WriteAsString(const Name: string; Value: Integer);overload;
    procedure WriteAsString(const Name: string; Value: boolean);overload;
    function  TryReadString(const Name: string; out value: string): boolean;
    function  ReadString(const Name, defaultValue: string): string;
    function  ReadInteger(const Name: string; aMin, aMax, defaultValue: Integer): Integer;overload;
    function  ReadInteger(const Name: string; defaultValue: Integer): Integer;overload;
    function  ReadBoolean(const Name: string; defaultValue: boolean): boolean;

    {{Пишет флаг "Checked" у item}
    procedure WriteChecked(item: TMenuItem);

    {{Читает значение флага "Checked" для item}
    procedure ReadChecked(item: TMenuItem);

    procedure WriteStringsByComma(const name: string; Value: IStringListInterface);
    function  TryReadStringsByComma(const name: string): IStringListInterface;

    property KeyOpened: boolean read FKeyOpened;
  end;

  {{Источник "астрономического" времени. Астрономическое время не имеет переходов зима/лето. Время всегда только увеличивается.}
  TDateTimeProviderSimple = class
    function  GetNowAstroTime(): TDateTime;virtual; //Текущее время компьютера, на котором работаем
  end;

  TExceptionHelper = class helper for Exception
    function MessageWithNestedExceptions(): string;
  end;

  {{
  Умный указатель, не требует каноничного блока Create try .. finally Free end
  Пример использовния:
  var
    sl: ISmartPointer<TStringList>;
  begin
    sl := TSmartPointer<TStringList>.Create(); // конструктор по умолчанию
    sl := TSmartPointer<TStringList>.Create(TStringList.Create(true)); // конструктор с параметрами
    sl.Add('За мной не надо чистить память');
  end;
  }
  ISmartPointer<T> = reference to function: T;

  TSmartPointer2<T: class> = class(TInterfacedObject, ISmartPointer<T>)
  private
    FValue: T;
    function Invoke: T;
  public
    constructor Create(AValue: T); overload;
    destructor Destroy; override;
  end;

  TSmartPointer<T: class, constructor> = class(TSmartPointer2<T>, ISmartPointer<T>)
  public
    constructor Create; overload;
  end;

  TFakeInterfaceObject = class(TObject, IInterface)
  private
    class var FInstance: TFakeInterfaceObject;
    class constructor ClassCreate();
    class procedure UnitFinalizationWithoutThreads();
  protected
  type
    TOnUnitFinalization = reference to procedure;
    class var OnUnitFinalization: TOnUnitFinalization;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    class function GetInstance(): TFakeInterfaceObject;inline;
  end;

  ENullableNotAssignedException = class(ECheckException);

  Nullable<T> = record
  {{Inspired by
    https://community.embarcadero.com/blogs/entry/a-andquotnullableandquot-post-38869}
  private
    FValue: T;
    FHasValue: IInterface;//поле интерфейсного типа будет обнулено компиляторм при размещении структуры на стэке. Булевский флаг так бы не смог быть инициализирован
    function GetValue: T;
  public
    constructor Create(AValue: T);
    class function CreateEmpty(): Nullable<T>;static;
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(Default: T): T; overload;
    function Assigned(): boolean;
    property HasValue: Boolean read Assigned;
    property Value: T read GetValue;
    class operator NotEqual(const ALeft, ARight: Nullable<T>): Boolean;
    class operator Equal(const ALeft, ARight: Nullable<T>): Boolean;
    class operator Implicit(Value: Nullable<T>): T;
    class operator Implicit(Value: T): Nullable<T>;
    class operator Explicit(Value: Nullable<T>): T;
  end;

  INamedFormat = interface
  ['{C2627E6E-481F-40CB-94DD-F027F204E065}']
    function AddNull(const name: string): INamedFormat;
    function Add(const name: string; Value: Integer): INamedFormat;overload;
    function Add(const name: string; Value: TDateTime): INamedFormat;overload;
    function Add(const name, Value: string): INamedFormat;overload;
    function Format(): string;
  end;

  TNamedFormat = class(TInterfacedObject, INamedFormat)
  public
  type
    TFormatMode = (fmDefault, fmSql);
    TFormatModeHelper = record helper for TFormatMode
    public
      function GetDefaultPrefix(): string;
      function GetReplaceFlags(): TReplaceFlags;
    end;
    TFormatParameter = record
    private
      FName: string;
      FIntValue: Nullable<Integer>;
      FDateTimeValue: Nullable<TDateTime>;
      FStringValue: Nullable<string>;
    public
      class function Int(const aName: string; const Value: Integer): TFormatParameter;static;
      class function DT(const aName: string; const Value: TDateTime): TFormatParameter;static;
      class function Str(const aName, Value: string): TFormatParameter;static;
      class function Null(const aName: string): TFormatParameter;static;
      function Format(parent: TNamedFormat; const format: string): string;
      property Name: string read FName;
    end;
  private
    FParameters: array of TFormatParameter;
    FFormatMode: TFormatMode;
    FFormatString: string;
  protected
    function Format(): string;
    constructor DoCreate(aMode: TFormatMode; const aformatStr: string);
    function DoAdd(var parameter: TFormatParameter): INamedFormat;
  public
    constructor ForSql(const resourceName: string);
    constructor Create(const formatStr: string; mode: TFormatMode = fmDefault);
    constructor FromResource(const resourceName: string; mode: TFormatMode = fmDefault);
    function Add(const name: string; Value: Integer): INamedFormat;overload;
    function Add(const name, Value: string): INamedFormat;overload;
    function Add(const name: string; Value: TDateTime): INamedFormat;overload;
    function AddNull(const name: string): INamedFormat;
    destructor Destroy;override;
    property FormatMode: TFormatMode read FFormatMode;
  end;

  {{Сортировка строк с адресами, применяемая во всей Энергосфере. Надо, чтобы было: Волгоградская 2 < Волгоградская 194}
  TStringNormalizer = class
  private
    class var AnyNumber: TRegEx;
    class var WhiteSpaces: TRegEx;
  private
    class constructor ClassCreate();
    class function DoReplace(const Match: TMatch): string;
  public
    class function NormalizeStringForCompare(const value: string): string;
    class function CompareStrings(const str1, str2: string): Integer;
  end;

{{Returns true if v is not NULL or EMPTY}
function VarIsAssigned(const v: Variant): boolean; inline;

{{Returns true if v IS NULL or EMPTY}
function VarIsNullOrEmpty(const v: Variant): boolean; inline;

{{Если в конце msg нет точки/воскл. знака, добавляет в конец msg точку}
function AppendDotIfNeeded(const msg: string): string;

{{ Выдергивает полный текст сообщения об ошибке, включая InnerException's (если есть).
  В простейшем случае результат == E.Message
  Если в конце сообщения об ощибке нет точки/воскл. знака, добавляет в конец сообщения точку}
function FullMessage(const E: Exception): string;

function LoadStringFromRcData(const resourceName: string): string;

{$IFNDEF ServerOprosa}
{{"PROSOFT-E\petrov" for domain user, or just "petrov", if can not get info from domain}
function GetDomainAndUserName(): string;
{$ENDIF}

{{Same as GetTimeZoneInformation but throws an error if can not read time zone info}
function GetTimeZoneInformationWithException(var lpTimeZoneInformation: TTimeZoneInformation): DWORD;

{{'True' or 'False'}
function BooleanToDebugStr(b: boolean): string;

function GET_X_LPARAM(lParam: DWORD): SmallInt;
function GET_Y_LPARAM(lParam: DWORD): SmallInt;

implementation

uses
  System.Math;

function BooleanToDebugStr(b: boolean): string;
begin
  Result := IfThen(b, TRUE_AS_STRING, FALSE_AS_STRING);
end;

function GET_X_LPARAM(lParam: DWORD): SmallInt;
begin
  Result := SmallInt( lParam and $FFFF);
end;

function GET_Y_LPARAM(lParam: DWORD): SmallInt;
begin
  Result := SmallInt( (lParam shr 16) and $FFFF);
end;

function VarIsAssigned(const v: Variant): boolean; inline;
begin
  Result := (v <> System.Variants.Null) and (not VarIsNull(v)) and (not VarIsEmpty(v));
end;

function VarIsNullOrEmpty(const v: Variant): boolean; inline;
begin
  Result := VarIsNull(v) or VarIsEmpty(v) or (v = System.Variants.Null);
end;

{$IFNDEF ServerOprosa}
function GetDomainAndUserName(): string;
begin
  try
    Result := Trim(GetDomainName());
  except
    on E: EOSError do
      Result := '';
  end;
  if Result = '' then
    Result := GetLocalUserName()
  else
    Result := Format('%s\%s', [Result, GetLocalUserName()]);
end;
{$ENDIF}

function GetTimeZoneInformationWithException(var lpTimeZoneInformation: TTimeZoneInformation): DWORD;
begin
  Result:= GetTimeZoneInformation(lpTimeZoneInformation);
  if (Result = $FFFFFFFF) then
    RaiseLastOSError();
end;

function AppendDotIfNeeded(const msg: string): string;
var
  trimmedMessage: string;
begin
  trimmedMessage := Trim(msg);
  Result := msg;
  if not (trimmedMessage.EndsWith('.') or trimmedMessage.EndsWith('!')) then
    Result := Result + '.';
end;

function FullMessage(const E: Exception): string;
var
  Ex: Exception;
  text: IStringListInterface;
  aMessage: string;
begin
  if E = nil then
  begin
    Result := '[No Exception!]';
    exit;
  end;
  if not Assigned(E.InnerException) then
  begin
    Result := AppendDotIfNeeded(E.Message);
    exit;
  end;

  Ex := E;
  text:= TStringListInterfaced.Create();
  repeat
    if Trim(Ex.Message) <> '' then
    begin
      aMessage := AppendDotIfNeeded(Ex.Message);
      if text.Count = 0 then
        text.Add(aMessage)
      else
        text.Add('Причина: %s', [aMessage]);
    end;
    Ex := Ex.InnerException;
  until (Ex = nil);
  if text.Count  = 0 then
    Result := '[Исключение без текста]'
  else
    Result := Text.ToStringWithDelimiter(sLineBreak);
end;

{ TExceptionHelper }

function TExceptionHelper.MessageWithNestedExceptions: string;
begin
  Result := FullMessage(Self);
end;

function LoadStringFromRcData(const resourceName: string): string;
var
  ResStream: TResourceStream;
  dest: TStringStream;
begin
  ResStream := TResourceStream.Create(HInstance, resourceName, RT_RCDATA);
  try
    dest:= TStringStream.Create();
    try
      ResStream.SaveToStream(dest);
      Result := dest.DataString;
    finally
      dest.Free();
    end;
  finally
    ResStream.Free();
  end;
end;

{ TEsStringList }

constructor TEsStringList.CreateFromArray(const source: array of string);
var
  I: Integer;
begin
  inherited Create();
  for I:= Low(source) to High(source) do
    Add(source[I]);
end;

function TEsStringList.ToStringWithDelimiter(const delimiter: string): string;
var
  old: string;
begin
  if delimiter = '' then
    raise ECheckException.Create('ToStringWithDelimiter not tested with empty delimiter');
  old := LineBreak;
  try
    LineBreak:= delimiter;
    Result:= Text;
    if EndsStr(delimiter, Result) then
      System.Delete(Result, Length(Result) - Length(delimiter) + 1, Length(delimiter));
  finally
    LineBreak:= old;
  end;
end;

{ TStringListInterfaced }

constructor TStringListInterfaced.Create(const aText: string = '');
begin
  inherited Create();
  FStrings:= TEsStringList.Create();
  if Length(aText) > 0 then
   Text := aText;
end;

constructor TStringListInterfaced.CreateWithDelimiter(const aTextToDelimit: string; aDelimiter: Char);
begin
  Create();
  Delimiter := aDelimiter;
  StrictDelimiter := True;
  DelimitedText:= aTextToDelimit;
end;

destructor TStringListInterfaced.Destroy;
begin
  FStrings.Free();
  inherited;
end;

procedure TStringListInterfaced.EndUpdate;
begin
  FStrings.EndUpdate();
end;

procedure TStringListInterfaced.Clear;
begin
  FStrings.Clear();
end;

function TStringListInterfaced.GetCommaText: string;
begin
  Result:= FStrings.CommaText;
end;

function TStringListInterfaced.GetCount: Integer;
begin
  Result:= FStrings.Count;
end;

procedure TStringListInterfaced.Delete(Index: Integer);
begin
  FStrings.Delete(Index);
end;

function TStringListInterfaced.GetDelimitedText: string;
begin
  Result := FStrings.DelimitedText;
end;

function TStringListInterfaced.GetDelimiter: Char;
begin
  Result := FStrings.Delimiter;
end;

function TStringListInterfaced.GetEnumerator: TStringsEnumerator;
begin
  Result := FStrings.GetEnumerator;
end;

function TStringListInterfaced.GetItem(Index: Integer): string;
begin
  Result:= FStrings[Index];
end;

function TStringListInterfaced.GetObject(Index: Integer): TObject;
begin
  REsult := FStrings.Objects[Index];
end;

function TStringListInterfaced.GetStrictDelimiter: Boolean;
begin
  Result := FStrings.StrictDelimiter;
end;

function TStringListInterfaced.GetText: string;
begin
  Result:= FStrings.Text;
end;

function TStringListInterfaced.GetValue(const Name: string): string;
begin
  Result:= FStrings.Values[Name];
end;

function TStringListInterfaced.OriginalStrings: TStrings;
begin
  Result:= FStrings;
end;

procedure TStringListInterfaced.SaveToFile(const FileName: string);
begin
  FStrings.SaveToFile(FileName);
end;

procedure TStringListInterfaced.SetCommaText(const Value: string);
begin
  FStrings.CommaText := Value;
end;

procedure TStringListInterfaced.SetDelimitedText(const Value: string);
begin
  FStrings.DelimitedText := Value;
end;

procedure TStringListInterfaced.SetDelimiter(const Value: Char);
begin
  FStrings.Delimiter := Value;
end;

procedure TStringListInterfaced.SetStrictDelimiter(const Value: Boolean);
begin
  FStrings.StrictDelimiter := Value;
end;

procedure TStringListInterfaced.SetText(const Value: string);
begin
  FStrings.Text := Value;
end;

procedure TStringListInterfaced.SetValue(const Name, Value: string);
begin
  FStrings.Values[Name] := Value;
end;

procedure TStringListInterfaced.Add(const s: string);
begin
  FStrings.Add(s);
end;

procedure TStringListInterfaced.AddIfNotEmpty(const s: string);
begin
  if Trim(s) <> '' then
    FStrings.Add(s);
end;

procedure TStringListInterfaced.AddObject(const s: string; obj: integer);
begin
  AddObject(s, TObject(obj));
end;

procedure TStringListInterfaced.AddObject(const s: string; obj: TObject);
begin
  FStrings.AddObject(s, obj);
end;

procedure TStringListInterfaced.Add(const FormatStr: string; const Args: array of const);
begin
  FStrings.Add(Format(FormatStr, Args));
end;

procedure TStringListInterfaced.AddStrings(Strings: TStrings);
begin
  FStrings.AddStrings(Strings);
end;

procedure TStringListInterfaced.AddTo(dest: ExceptionWithDetails);
begin
  EnsureNotNull(dest, 'dest');
  dest.AddDetails(FStrings);
end;

procedure TStringListInterfaced.BeginUpdate;
begin
  FStrings.BeginUpdate();
end;

procedure TStringListInterfaced.AddTo(dest: TStrings);
begin
  EnsureNotNull(dest, 'dest');
  dest.AddStrings(FStrings);
end;

procedure TStringListInterfaced.AddStrings(Strings: IStringListInterface);
begin
  FStrings.AddStrings(Strings.OriginalStrings);
end;

function TStringListInterfaced.ToStringWithDelimiter(const delimiter: string): string;
begin
  Result:= FStrings.ToStringWithDelimiter(delimiter);
end;

function TStringListInterfaced.Contains(const str: string): boolean;
begin
  Result := FStrings.IndexOf(str) >= 0;
end;

function TStringListInterfaced.ContainsValue(const valueName: string): boolean;
begin
  if Trim(valueName)= '' then
    Result := False
  else
    Result := FStrings.IndexOfName(valueName) >= 0;
end;

function TStringListInterfaced.GetName(Index: Integer): string;
begin
  Result := FStrings.Names[Index];
end;

{ TEsRegistry }

constructor TEsRegistry.Create(const rootKeyName: string; canWrite: boolean);
begin
  FRegistry:= TRegistry.Create();
  FRegistry.RootKey:= HKEY_CURRENT_USER;
  FKeyOpened := FRegistry.OpenKey(rootKeyName, canWrite);
end;

destructor TEsRegistry.Destroy;
begin
  if FKeyOpened then
    FRegistry.CloseKey();
  FRegistry.Free();
  inherited;
end;

function TEsRegistry.NormalizeName(const name: string): string;
begin
  EnsureNotEmpty(name, 'name');
  Result:= ReplaceText(Name, '.', '_');
end;

function TEsRegistry.TryReadBoolean(const Name: string; out value: boolean): boolean;
var
  tmp: string;
begin
  value:= False;
  Result:= TryReadString(Name, tmp);
  if Result then
    Result:= TryStrToBool(tmp, value);
end;

function TEsRegistry.ReadBoolean(const Name: string; defaultValue: boolean): boolean;
begin
  if not TryReadBoolean(Name, Result) then
    Result:= defaultValue;
end;

function TEsRegistry.TryReadInteger(const Name: string; out value: Integer): boolean;
var
  tmp: string;
begin
  value:= 0;
  Result:= TryReadString(Name, tmp);
  if Result then
    Result:= TryStrToInt(tmp, value);
end;

function TEsRegistry.ReadInteger(const Name: string; aMin, aMax, defaultValue: Integer): Integer;
var
  tmp: Integer;
begin
  if TryReadInteger(name, Result) then
  begin
    if aMin > aMax then
    begin
      tmp:= aMin;
      aMin:= aMax;
      aMax:= tmp;
    end;
    if (Result< aMin) or (Result > aMax) then
      Result:= defaultValue;
  end
  else
    Result:= defaultValue;
end;

function TEsRegistry.ReadInteger(const Name: string; defaultValue: Integer): Integer;
begin
  if not TryReadInteger(name, Result) then
    Result:= defaultValue;
end;

function TEsRegistry.ReadString(const Name, defaultValue: string): string;
begin
  if not TryReadString(Name, Result) then
    Result:= defaultValue;
end;

function TEsRegistry.TryReadString(const Name: string; out value: string): boolean;
var
  newName: string;
begin
  newName:= NormalizeName(Name);
  Result:= FRegistry.ValueExists(newName);
  if (Result) then
    try
      value:= FRegistry.ReadString(newName);
    except on ERegistryException do
      value := ''
    end
  else
    value:= '';
end;

procedure TEsRegistry.WriteString(const Name, Value: string);
var
  newName: string;
begin
  newName:= NormalizeName(Name);
  FRegistry.WriteString(newName, Value);
end;

procedure TEsRegistry.WriteStringsByComma(const name: string; Value: IStringListInterface);
begin
  EnsureNotNull(Value, 'Value');
  WriteString(name, Value.CommaText);
end;

function TEsRegistry.TryReadStringsByComma(const name: string): IStringListInterface;
var
  tmp: string;
begin
  tmp := '';
  if TryReadString(name, tmp) then
  begin
    Result:= TStringListInterfaced.Create();
    Result.CommaText := tmp;
  end
  else
    Result:= nil;
end;

procedure TEsRegistry.WriteAsString(const Name: string; Value: Integer);
begin
  WriteString(Name, IntToStr(Value));
end;

procedure TEsRegistry.WriteAsString(const Name: string; Value: boolean);
begin
  WriteString(Name, BoolToStr(Value, True));
end;

procedure TEsRegistry.ReadChecked(item: TMenuItem);
var
  b: boolean;
begin
  EnsureNotNull(item, 'item = nil');
  EnsureNotEmpty(item.Name, 'item.Name is empty');
  if TryReadBoolean(Format('%s.Checked', [item.Name]), b) then
    item.Checked:= b;
end;

procedure TEsRegistry.WriteChecked(item: TMenuItem);
begin
  EnsureNotNull(item, 'item = nil');
  EnsureNotEmpty(item.Name, 'item.Name is empty');
  WriteAsString(Format('%s.Checked', [item.Name]), item.Checked);
end;

{ TDateTimeProviderSimple }

function TDateTimeProviderSimple.GetNowAstroTime: TDateTime;
var
  TZ: TTimeZoneInformation;
  currentDaylight: DWORD;
  daylightChangeAllowed: boolean;
begin
  currentDaylight:= GetTimeZoneInformationWithException(TZ);
  daylightChangeAllowed := (currentDaylight in [TIME_ZONE_ID_STANDARD, TIME_ZONE_ID_DAYLIGHT])
            and (TZ.DaylightBias <> 0)
            and (TZ.StandardDate.wMonth > 0)
            and (TZ.DaylightDate.wMonth > 0);
  Result := Now;
  if (daylightChangeAllowed) and (currentDaylight = TIME_ZONE_ID_DAYLIGHT) then
    Result := IncMinute(Result, TZ.DaylightBias);//DaylightBias в Windows отрицательный, потому +
end;

{ TSmartPointer<T> }

constructor TSmartPointer<T>.Create;
begin
  inherited Create;
  FValue := T.Create;
end;

{ TSmartPointer2<T> }

constructor TSmartPointer2<T>.Create(AValue: T);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TSmartPointer2<T>.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TSmartPointer2<T>.Invoke: T;
begin
  Result := FValue;
end;

{ TFakeInterfaceObject }

class constructor TFakeInterfaceObject.ClassCreate;
begin
  FInstance:= TFakeInterfaceObject.Create();
  OnUnitFinalization := UnitFinalizationWithoutThreads;
end;

class function TFakeInterfaceObject.GetInstance: TFakeInterfaceObject;
begin
  Result := FInstance;
end;

class procedure TFakeInterfaceObject.UnitFinalizationWithoutThreads;
begin
  FreeAndNil(FInstance);
end;

function TFakeInterfaceObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;

function TFakeInterfaceObject._AddRef: Integer;
begin
  Result := -1;
end;

function TFakeInterfaceObject._Release: Integer;
begin
  Result := -1;
end;

{ Nullable<T> }

constructor Nullable<T>.Create(AValue: T);
begin
  FValue := AValue;
  FHasValue := TFakeInterfaceObject.GetInstance();
end;

class function Nullable<T>.CreateEmpty(): Nullable<T>;
begin
  Result.FHasValue := nil;
end;

class operator Nullable<T>.Equal(const ALeft, ARight: Nullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := Comparer.Equals(ALeft.Value, ARight.Value);
  end
  else
    Result := ALeft.HasValue = ARight.HasValue;
end;

class operator Nullable<T>.NotEqual(const ALeft, ARight: Nullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := not Comparer.Equals(ALeft.Value, ARight.Value);
  end
  else
    Result := ALeft.HasValue <> ARight.HasValue;
end;

class operator Nullable<T>.Explicit(Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

function Nullable<T>.Assigned: boolean;
begin
  Result := FHasValue <> nil;
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
    raise ENullableNotAssignedException.Create('Invalid operation, Nullable type has no value');
  Result := FValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default(T);
end;

function Nullable<T>.GetValueOrDefault(Default: T): T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default
end;

class operator Nullable<T>.Implicit(Value: Nullable<T>): T;
begin
  Result := Value.Value;
end;

class operator Nullable<T>.Implicit(Value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(Value);
end;

{ TNamedFormat }

constructor TNamedFormat.DoCreate(aMode: TFormatMode; const aformatStr: string);
begin
  inherited Create();
  EnsureNotEmpty(aformatStr, 'aformatStr');
  Self.FFormatMode := aMode;
  Self.FFormatString := aformatStr;
end;

constructor TNamedFormat.Create(const formatStr: string; mode: TFormatMode = fmDefault);
begin
  DoCreate(mode, formatStr);
end;

constructor TNamedFormat.ForSql(const resourceName: string);
begin
  FromResource(resourceName, fmSql);
end;

constructor TNamedFormat.FromResource(const resourceName: string; mode: TFormatMode = fmDefault);
begin
  DoCreate(mode, LoadStringFromRcData(resourceName));
end;

destructor TNamedFormat.Destroy;
begin
  SetLength(FParameters, 0);
  inherited;
end;

function TNamedFormat.Format(): string;
var
  f: TFormatParameter;
  params: ISmartPointer<TList<TFormatParameter>>;
begin
  Result := Self.FFormatString;

  // параметры надо обрабатывать в порядке уменьшения длины,
  // в противном случае возможно коллизии при вхождении имени одного параметра в другой,
  // например сначала отработает ID_PP, а потом ID_PP_TMP обработается неправильно
  params := TSmartPointer<TList<TFormatParameter>>.Create();
  params.AddRange(Self.FParameters);
  params.Sort(TComparer<TFormatParameter>.Construct(
    function(const Left, Right: TFormatParameter): Integer
    begin
      Result := -CompareValue(Length(Left.Name), Length(Right.Name));
    end));

  for f in params do
    Result := f.Format(Self, Result);
end;

function TNamedFormat.DoAdd(var parameter: TFormatParameter): INamedFormat;
var
  f: TFormatParameter;
begin
  Result := Self;
  for f in FParameters do
    if LowerCase(f.Name) = LowerCase(parameter.Name) then
       raise EInternalException.CreateFmt('param with name = %s is already added', [parameter.Name]);
  SetLength(FParameters, Length(FParameters) + 1);
  FParameters[High(FParameters)] := parameter;
end;

function TNamedFormat.Add(const name: string; Value: Integer): INamedFormat;
var
  tmp: TFormatParameter;
begin
  tmp := TFormatParameter.Int(name, Value);
  Result := DoAdd(tmp);
end;

function TNamedFormat.Add(const name, Value: string): INamedFormat;
var
  tmp: TFormatParameter;
begin
  tmp := TFormatParameter.Str(name, Value);
  Result := DoAdd(tmp);
end;

function TNamedFormat.Add(const name: string; Value: TDateTime): INamedFormat;
var
  tmp: TFormatParameter;
begin
  tmp := TFormatParameter.DT(name, Value);
  Result := DoAdd(tmp);
end;

function TNamedFormat.AddNull(const name: string): INamedFormat;
var
  tmp: TFormatParameter;
begin
  tmp := TFormatParameter.Null(name);
  Result := DoAdd(tmp);
end;

{ TNamedFormat.TFormatParameter }

class function TNamedFormat.TFormatParameter.DT(const aName: string; const Value: TDateTime): TFormatParameter;
begin
  EnsureNotEmpty(aName, 'aName');
  Result.FName := aName;
  Result.FDateTimeValue := Value;
end;

function TNamedFormat.TFormatParameter.Format(parent: TNamedFormat; const format: string): string;
var
  oldValue: string;
  newValue: Nullable<string>;
begin
  if FIntValue.HasValue then
  begin
    newValue := IntToStr(FIntValue.Value);
    if FIntValue.Value < 0 then
      newValue := '(' + newValue.Value + ')';
  end
  else
  if FStringValue.HasValue then
  begin
    if parent.FormatMode = fmSql then
      newValue := AnsiQuotedStr(FStringValue.Value, '''')
    else
      newValue := FStringValue.Value;
  end
  else
  if FDateTimeValue.HasValue then
    newValue := QuotedStr(FormatDateTime('YYYYMMDD HH:NN:SS.ZZZ', FDateTimeValue.Value));

  if not newValue.HasValue then
  begin
    if parent.FormatMode = fmSql then
      newValue := 'NULL'
    else
      newValue := '';
  end;
  oldValue := parent.FormatMode.GetDefaultPrefix() + Name;
  Result := StringReplace(format, oldValue, newValue.Value, parent.FormatMode.GetReplaceFlags());
end;

class function TNamedFormat.TFormatParameter.Int(const aName: string; const Value: Integer): TFormatParameter;
begin
  EnsureNotEmpty(aName, 'aName');
  Result.FName := aName;
  Result.FIntValue := Value;
end;

class function TNamedFormat.TFormatParameter.Null(const aName: string): TFormatParameter;
begin
  EnsureNotEmpty(aName, 'aName');
  Result.FName := aName;
end;

class function TNamedFormat.TFormatParameter.Str(const aName, Value: string): TFormatParameter;
begin
  EnsureNotEmpty(aName, 'aName');
  Result.FName := aName;
  Result.FStringValue := Value;
end;

{ TNamedFormat.TFormatModeHelper }

function TNamedFormat.TFormatModeHelper.GetDefaultPrefix: string;
begin
  case Self of
    fmSql: Result := '@';
    else
      Result := '$';
  end;
end;

function TNamedFormat.TFormatModeHelper.GetReplaceFlags: TReplaceFlags;
begin
  case Self of
    fmSql: Result := [rfReplaceAll, rfIgnoreCase];
    else
      Result := [rfReplaceAll];
  end;
end;

{ TStringNormalizer }

class constructor TStringNormalizer.ClassCreate;
begin
  AnyNumber:= TRegEx.Create('\d+', [roCompiled]);
  WhiteSpaces:= TRegEx.Create('\s+', [roCompiled]);
end;

class function TStringNormalizer.CompareStrings(const str1, str2: string): Integer;
begin
  Result := AnsiCompareText(NormalizeStringForCompare(str1), NormalizeStringForCompare(str2));
end;

class function TStringNormalizer.DoReplace(const Match: TMatch): string;
var
  tmp: Int64;
begin
  Result := Match.Value;
  if TryStrToInt64(Result, tmp) then
    Result := Format('%.10d', [tmp]);
end;

class function TStringNormalizer.NormalizeStringForCompare(const value: string): string;
begin
  if Trim(value) = '' then
  begin
    Result := value;
    exit;
  end;
  Result := AnyNumber.Replace(value, DoReplace);
  Result := WhiteSpaces.Replace(Result, '');
end;

initialization
finalization
  if Assigned(TFakeInterfaceObject.OnUnitFinalization) then
    TFakeInterfaceObject.OnUnitFinalization();
end.
