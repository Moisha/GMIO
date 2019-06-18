unit GMGenerics;

interface

uses Generics.Collections, SysUtils;

type
  // ������-�� ���� ������ � ������� ����������, �� ��� ���������.
  // �������� �������� �������������
  TGMCollection<T: class, constructor> = class(TList<T>)
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    function Add(): T; virtual;
  end;

  TGMThreadSafeCollection<T: class, constructor> = class(TGMCollection<T>)
  private
    FLock: TMultiReadExclusiveWriteSynchronizer;
  protected
    property Lock: TMultiReadExclusiveWriteSynchronizer read FLock;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

{ TGMCollection<T> }

function TGMCollection<T>.Add: T;
begin
  Result := T.Create();
  inherited Add(Result);
end;

procedure TGMCollection<T>.Notify(const Value: T; Action: TCollectionNotification);
begin
  inherited;
  if Action = cnRemoved then
    Value.DisposeOf;
end;

{ TGMThreadSafeCollection<T> }

constructor TGMThreadSafeCollection<T>.Create;
begin
  inherited;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create();
end;

destructor TGMThreadSafeCollection<T>.Destroy;
begin
  FLock.Free();
  inherited;
end;

end.
