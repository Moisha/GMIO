{
  Библиотека дополнительных компонентов

  Регистрация компонента TGridView в Delphi IDE

  © Роман М. Мочалов, 1997-2001
  E-mail: roman@sar.nnov.ru
}

unit Ex_RegGrid;

{$I EX.INC}

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, TypInfo,
  {$IFDEF EX_D6_UP} DesignIntf, DesignEditors {$ELSE} DsgnIntf {$ENDIF};

type

{ TGridEditor }

  TGridEditor = class(TDefaultEditor)
  private
    FCollection: TCollection;
    {$IFDEF EX_D6_UP}
    procedure FindCollectionEditor(const PropertyEditor: IProperty);
    {$ELSE}
    procedure FindCollectionEditor(PropertyEditor: TPropertyEditor);
    {$ENDIF}
  protected
    {$IFDEF EX_D6_UP}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF}
    procedure ShowCollectionEditor(ACollection: TCollection);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TGridColumnsProperty }

  TGridColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TGridHeaderProperty }

  TGridHeaderProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TDBGridFieldNameProperty }

  TDBGridFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Ex_Grid, Ex_GridC, Ex_GridH, Ex_Inspector, Ex_DBGrid;

{ TGridEditor }

{$IFDEF EX_D6_UP}
procedure TGridEditor.FindCollectionEditor(const PropertyEditor: IProperty);
{$ELSE}
procedure TGridEditor.FindCollectionEditor(PropertyEditor: TPropertyEditor);
{$ENDIF}
var
  P: PTypeInfo;
begin
  { если редактор свойства найден, то значение FCollection будет
    сброшено в nil }
  if FCollection <> nil then
  begin
    P := PropertyEditor.GetPropType;
    { проверяем тип и название свойства }
    if (P <> nil) and (P.Kind = tkClass) and (CompareText(P.Name, FCollection.ClassName) = 0) then
    begin
      PropertyEditor.Edit;
      FCollection := nil;
    end;
  end;
  {$IFNDEF EX_D6_UP}
  { об освобождении редактора свойства необходимо заботится самим }
  PropertyEditor.Free;
  {$ENDIF}
end;

{$IFDEF EX_D6_UP}
procedure TGridEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}
procedure TGridEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
begin
  inherited;
  { Двойной щелчок на компоненте в дизайнере Delphi приводит к вызову
    метода Edit у TDefaultEditor. TDefaultEditor перебирает внутри Edit
    все свойства компонента, ищет свойство-метод с именем OnCreate,
    OnChange или OnClick (или первый попавшийся, если указанных нет) и
    вызывает Edit соответствующего редактор свойства. Редактор свойтва в
    свою очередь ставит курсор на обработчик данного метода в тексте.
    Поэтому, если мы хотим, чтобы двойной щелчок на таблице автоматически
    ставил курсор на OnGetCellText, то нам необходимо самим найти редактор
    свойства этого свойства и "выполнить" его. }
  if CompareText(PropertyEditor.GetName, 'ONGETCELLTEXT') = 0 then
  begin
    PropertyEditor.Edit;
    { говорим стандартному обработчику, что исполнять найденный им
      редактор свойства не надо }
    Continue := False;
  end;
end;

procedure TGridEditor.ShowCollectionEditor(ACollection: TCollection);
var
{$IFDEF EX_D6_UP}
  List: IDesignerSelections;
{$ELSE}
 {$IFDEF EX_D5_UP}
  List: TDesignerSelectionList;
 {$ELSE}
  List: TComponentList;
 {$ENDIF}
{$ENDIF}
begin
  FCollection := ACollection;
  { перебираем список свойств, ищем и показываем стандартный редактор
    для указанной коллекции }
  {$IFDEF EX_D6_UP}
  List := TDesignerSelections.Create;
  {$ELSE}
   {$IFDEF EX_D5_UP}
  List := TDesignerSelectionList.Create;
   {$ELSE}
  List := TComponentList.Create;
   {$ENDIF}
  {$ENDIF}
  try
    List.Add(Self.Component);
    GetComponentProperties(List, [tkClass], Self.Designer, FindCollectionEditor);
  finally
    {$IFNDEF EX_D6_UP}
    List.Free;
    {$ENDIF}
  end;
end;

procedure TGridEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
{$IFDEF VER90}
    0: if EditGridColumns(TCustomGridView(Component)) then Designer.Modified;
{$ELSE}
    0: ShowCollectionEditor(TCustomGridView(Component).Columns);
{$ENDIF}
    1: if EditGridHeader(TCustomGridView(Component)) then Designer.Modified;
  end;
end;

function TGridEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Columns Editor...';
    1: Result := 'Header Editor...';
  end;
end;

function TGridEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TGridColumnsProperty }

procedure TGridColumnsProperty.Edit;
begin
  if EditGridColumns(TGridColumns(GetOrdValue).Grid) then Modified;
end;

function TGridColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TGridHeaderProperty }

procedure TGridHeaderProperty.Edit;
begin
  if EditGridHeader(TGridHeaderSections(GetOrdValue).Header.Grid) then Modified;
end;

function TGridHeaderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TDBGridFieldNameProperty }

function TDBGridFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBGridFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  Grid: TCustomDBGridView;
  Values: TStringList;
  I: Integer;
begin
  Grid := TDBGridColumn(GetComponent(0)).Grid;
  if (Grid <> nil) and (Grid.DataLink.DataSet <> nil) then
  begin
    Values := TStringList.Create;
    try
      Grid.DataLink.DataSet.GetFieldNames(Values);
      for I := 0 to Values.Count - 1 do Proc(Values[I]);
    finally
      Values.Free;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Extended', [TGridView]);
  RegisterComponents('Extended', [TExInspector]);

  RegisterComponents('Extended', [TDBGridView]);
  
  RegisterComponentEditor(TGridView, TGridEditor);
  RegisterPropertyEditor(TypeInfo(TGridHeaderSections), TGridHeader, 'Sections', TGridHeaderProperty);

{$IFDEF VER90}
  RegisterPropertyEditor(TypeInfo(TGridColumns), TGridView, 'Columns', TGridColumnsProperty);
{$ENDIF}

  RegisterComponentEditor(TDBGridView, TGridEditor);
  RegisterPropertyEditor(TypeInfo(string), TDBGridColumn, 'FieldName', TDBGridFieldNameProperty);
end;

end.
