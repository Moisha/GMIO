unit esExceptions;

interface

{$I Compilers.inc}

Uses
  SysUtils, Classes;

type
  EsException = class(Exception)
  {{� ����� �������� ��������� Inner Exception, ����� ������� ��� � Delphi.
    �������� ������� � XE4
    ��� ����, ����� �������� ����������, ���� ������:
    try
      DoSomething();
    except
      on E: Exception1 do
        raise EsException.Create(E, '����� ��������������� ��������');
    end;
  }
  public
    constructor Create(aInnerException: Exception);overload;//���� � ������ ����������� ���������� �������� ������, ������������ ���� �����������
    constructor Create(aInnerException: Exception; const msg: string);overload;
    constructor CreateFmt(aInnerException: Exception; const msgFormat: string; const args: array of const);overload;
{$IFDEF COMPILER_18_UP}
    function Message(): string;reintroduce;//�������������� ��� ����, ����� ��� ����������� ���������� ��� ���������� ������ by Create(aInnerException: Exception)
    //����� ���� �� ����� ����������� �� ������ ���������� ����������
{$ENDIF}
  end;

  {{���������� � �������������.
  ����������� ����� �������� �� �������� ������}
  ExceptionWithDetails = class(EsException)
  private
    FDetails: TStrings;
    function GetDetailsAsText: string;
    function GetDetails: TStrings;
  protected
    property   Details: TStrings read GetDetails;
  public
    constructor CreateWithDetails(aInnerException: Exception; const aDetails, aMessage: string; const Args: array of const);overload;
    constructor CreateWithDetails(const aDetails, aMessage: string; const Args: array of const);overload;
    constructor CreateWithDetails(const aMessage: string; const Args: array of const; const details: TStrings);overload;
    constructor CreateWithDetails(const aMessage: string; const details: TStrings);overload;
    procedure  AfterConstruction();override;
    destructor Destroy;override;
    function AddDetail(const detail: string): ExceptionWithDetails;overload;
    function AddDetail(const msgFormat: string; const Args: array of const): ExceptionWithDetails;overload;
    procedure  AddDetails(source: TStrings);
    procedure  StoreDetailsTo(dest: TStrings);
    property   DetailsAsText: string read GetDetailsAsText;
  End;

  {{������ � ���� ���������. ����� ������ � ������ �� ������ ���������.
    ���������� ���������� ������ ����� ���� ��������� ������������ - �� �� ����� ������ ������� ������,
    �� �������� ��� ����� ���������.
    ��. ����� ECorrectException}
  EInternalException = class(EsException);

  {{����������, ��������� � ���������� ������ � ���������, �������� �� ������� ��������� �������� � ���������.
  ��. EnsureNotNull}
  ECheckException = class(EInternalException);

  {{"����������" ����������.
   ����� ��������� ��� ������� ������ ������ ���������.
   ��������, ������������ ���� ������������ ��������, ���
   ��� �� �������� ����� �� �������� � �� �� ����� ��� ����������.
   ��� ������� ���� "����������" ������ ����� �������� ���� �������� -
   ��� �������� ������ ����������� ����������� ������.
   ��. ����� EInternalException.
   ��� ����� ���������� ����� �� ���������� StackTrace - ��� ������ ������ ����������� �������������}
  ECorrectException = class(ExceptionWithDetails);

  {{������ �������� ���������. ��������, � �� ��� �������, ������� ���������� ��� ������ ���������}
  EnvironmentException = class(ExceptionWithDetails);

  EUserInputException = class(ECorrectException);

{{Raises an ECheckException if obj is null, with given msg.
  ���� msg �� �������� ��������, ���������������, ��� ��� ��� ��������� ���������.
  ��������� �� ������ ����� '"msg" is null'}
procedure EnsureNotNull(obj: TObject; const msg: string);overload;{$IFDEF COMPILER_18_UP}inline;{$ENDIF}
procedure EnsureNotNull(obj: IInterface; const msg: string);overload;{$IFDEF COMPILER_18_UP}inline;{$ENDIF}
procedure EnsureNotNull(obj: TObject; const msg: string; const Args: array of const);overload;

{{Raises an ECheckException if Trim(s) == '', with given msg.
  ���� msg �� �������� ��������, ���������������, ��� ��� ��� ��������� ���������.
  ��������� �� ������ ����� '"msg" is empty'}
procedure EnsureNotEmpty(const s: string; const msg: string);overload;{$IFDEF COMPILER_18_UP}inline;{$ENDIF}
procedure EnsureNotEmpty(const s: string; const msg: string; const Args: array of const);overload;

{{����������� ECheckException if not condtion}
procedure mustBeTrue(condition: boolean; const msg: string);{$IFDEF COMPILER_18_UP}inline;{$ENDIF}
{{����������� ECheckException if not condtion}
procedure mustBeTrueFmt(condition: boolean; const msg: string; const Args: array of const);


implementation

{$IFDEF COMPILER_18_UP}
type
  EsDummyException = class(Exception);//��� �� ������, ���� ������� Exception ���������� �� ����������� � EsException � �������� InnerException

  ExceptionHelper = class helper for Exception
    procedure AssignInnerException(E: Exception);
  end;
{$ENDIF}

procedure mustBeTrue(condition: boolean; const msg: string);
begin
  if (not(condition)) then
    raise ECheckException.Create(msg);
end;

procedure mustBeTrueFmt(condition: boolean; const msg: string; const Args: array of const);
begin
  if (not(condition)) then
    raise ECheckException.CreateFmt(msg, Args);
end;

procedure EnsureNotEmpty(const s: string; const msg: string);
begin
  if Trim(s) = '' then
  begin
    if Pos(' ', msg) > 0 then
      raise ECheckException.Create(msg);
    raise ECheckException.CreateFmt('"%s" is empty', [msg]);
  end;
end;

procedure EnsureNotEmpty(const s: string; const msg: string; const Args: array of const);
begin
  if Trim(s) = '' then
    raise ECheckException.CreateFmt(msg, Args);
end;

procedure EnsureNotNull(obj: TObject; const msg: string);
begin
  if obj = nil then
  begin
    if Pos(' ', msg) > 0 then
      raise ECheckException.Create(msg);
    raise ECheckException.CreateFmt('"%s" is null', [msg]);
  end;
end;

procedure EnsureNotNull(obj: IInterface; const msg: string);overload;
begin
  if not Assigned(obj) then
  begin
    if Pos(' ', msg) > 0 then
      raise ECheckException.Create(msg);
    raise ECheckException.CreateFmt('"%s" is null', [msg]);
  end;
end;

procedure EnsureNotNull(obj: TObject; const msg: string; const Args: array of const);
begin
  if obj = nil then
    raise ECheckException.CreateFmt(msg, Args);
end;

{ ExceptionWithDetails }

procedure ExceptionWithDetails.AfterConstruction;
begin
  inherited;
  GetDetails();//just create details if not created already
end;

constructor ExceptionWithDetails.CreateWithDetails(aInnerException: Exception; const aDetails, aMessage: string; const Args: array of const);
begin
  inherited CreateFmt(aInnerException, aMessage, Args);
  GetDetails.Add(aDetails);
end;

constructor ExceptionWithDetails.CreateWithDetails(const aMessage: string; const Args: array of const; const details: TStrings);
begin
  inherited CreateFmt(aMessage, Args);
  AddDetails(details);
end;

constructor ExceptionWithDetails.CreateWithDetails(const aMessage: string; const details: TStrings);
begin
  inherited Create(aMessage);
  AddDetails(details);
end;

constructor ExceptionWithDetails.CreateWithDetails(const aDetails, aMessage: string; const Args: array of const);
begin
  inherited CreateFmt(aMessage, Args);
  GetDetails.Add(aDetails);
end;

destructor ExceptionWithDetails.Destroy;
begin
  FreeAndNil(FDetails);
  inherited;
end;

function ExceptionWithDetails.GetDetails: TStrings;
begin
  if FDetails = nil then
    FDetails := TStringList.Create();
  Result := FDetails;
end;

function ExceptionWithDetails.GetDetailsAsText: string;
begin
  Result:= Details.Text;
end;

procedure ExceptionWithDetails.StoreDetailsTo(dest: TStrings);
begin
  if Assigned(dest) then
    dest.AddStrings(Details);
end;

function ExceptionWithDetails.AddDetail(const detail: string): ExceptionWithDetails;
begin
  Details.Add(detail);
  Result := Self;
end;

function ExceptionWithDetails.AddDetail(const msgFormat: string;
  const Args: array of const): ExceptionWithDetails;
begin
  Result := AddDetail(Format(msgFormat, Args));
end;

procedure ExceptionWithDetails.AddDetails(source: TStrings);
begin
  if Assigned(source) then
    Details.AddStrings(source);
end;

{ EsException }

constructor EsException.Create(aInnerException: Exception; const msg: string);
begin
  inherited Create(msg);
{$IFDEF COMPILER_18_UP}
  AssignInnerException(aInnerException);
{$ENDIF}
end;

constructor EsException.Create(aInnerException: Exception);
begin
  inherited Create('');
{$IFDEF COMPILER_18_UP}
  AssignInnerException(aInnerException);
{$ENDIF}
end;

constructor EsException.CreateFmt(aInnerException: Exception; const msgFormat: string; const args: array of const);
begin
  inherited CreateFmt(msgFormat, args);
{$IFDEF COMPILER_18_UP}
  AssignInnerException(aInnerException);
{$ENDIF}
end;

{$IFDEF COMPILER_18_UP}

function EsException.Message: string;
begin
  Result := inherited Message;
  if Result = '' then
    if Assigned(InnerException) then
       Result := InnerException.Message;
end;

{ ExceptionHelper }

procedure ExceptionHelper.AssignInnerException(E: Exception);
var
  tmp: TObject;
begin
  tmp := AcquireExceptionObject();
  if (tmp = E) then
    with Self do
      FInnerException := E
  else begin
    ReleaseExceptionObject();
    with Self do
      FInnerException := EsDummyException.CreateFmt('%s: %s (%s)', [E.ClassName, E.Message, E.ToString]);
  end;
end;
{$ENDIF}

end.
