unit dxStandardControlsSkinHelper;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxSkinsCore, dxSkinsDefaultPainters, cxLookAndFeels, dxSkinsForm,
  cxControls, cxContainer, cxEdit, cxGroupBox, cxRadioGroup, Menus, cxScrollBox,
  cxLookAndFeelPainters, StdCtrls, cxButtons, ExtCtrls, dxSkinsLookAndFeelPainter,
  cxTextEdit, cxMemo, dxDockControl, dxSkinInfo, cxPC, cxButtonEdit, cxLabel;

type
  {{Внутренняя ошибка приложения.
  Проблема при чтении свойств скина}
  ESkinHelperException = class(Exception);

  TdxStandardControlsSkinHelper = class(TObject)
  private
    FPrevRootLookAndFeelChanged: TcxLookAndFeelChangedEvent;
    FLeftOffsetForDxEdit: Integer;
    constructor PrivateCreate();
    function  NeedProcess: Boolean;
    procedure SetLeftOffsetForDxEdit(const Value: Integer);
  protected
    procedure ProcessScreen;
    procedure ProcessForm(AForm: TCustomForm);
    procedure ProcessControl(AControl: TControl); virtual;
    procedure ActiveFormChanged(Sender: TObject);
    procedure RootLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
  public
    constructor Create();
    destructor  Destroy;override;
    procedure AssignEventHandlers;
    procedure ResetEventHandlers;

    {{Singleton pattern}
    class function GetInstance(): TdxStandardControlsSkinHelper;

    {{ Настройка в соответствии со скином
     Если контрол содержит в себе другие контролы, то настраиваем и их тоже}
    procedure ProcessControls(AControl: TControl);

    {{Отступ слева, между границей контрола и текстом, который будет задан для всех наследников от TcxCustomEdit.
    Можно задать это св-во в -1, чтобы не присваивался}
    property  LeftOffsetForDxEdit: Integer read FLeftOffsetForDxEdit write SetLeftOffsetForDxEdit;
  end;

  ECantGetSkinElementEsException = class(ESkinHelperException);

  {{Позволяет считывать элементы из скина}
  TesSkinElementGetter = class
    {{groupName - имя группы в скине.
      elementName - имя элемента в группе.
      Если не удалось найти группу/элемент, выбрасывает ECantGetColorEsException}
    class function GetElement(const groupName, elementName: string): TdxSkinElement;
  end;

  {{Позволяет считывать цвета из скина}
  TesSkinColorGetter = class(TesSkinElementGetter)
  public
    {{groupName - имя группы в скине.
      elementName - имя элемента в группе.
      Если не удалось найти группу/элемент, выбрасывает ECantGetColorEsException}
    class function GetColor(const groupName, elementName: string): TColor;
    class function ColorByName(const colorName: string): TColor;
  end;

  function GetSkinFormContentColor(): TColor;

implementation

var
  StandardControlsSkinHelper: TdxStandardControlsSkinHelper;

procedure RegisterStandardControlsSkinHelper;
begin
  StandardControlsSkinHelper := TdxStandardControlsSkinHelper.PrivateCreate();
  StandardControlsSkinHelper.AssignEventHandlers;
end;

procedure UnregisterStandardControlsSkinHelper;
begin
  if StandardControlsSkinHelper <> nil then
  begin
    StandardControlsSkinHelper.ResetEventHandlers;
    FreeAndNil(StandardControlsSkinHelper);
  end;
end;

var FFormColor: TColor = clNone;

function GetSkinFormContentColor(): TColor;
begin
  if FFormColor = clNone then
    FFormColor := TesSkinColorGetter.GetColor('Form', 'FormContent');

  Result := FFormColor;
end;

{ TesSkinColorGetter }

class function TesSkinColorGetter.GetColor(const groupName, elementName: string): TColor;
begin
  Result := GetElement(groupName, elementName).Color;
end;

class function TesSkinColorGetter.ColorByName(const colorName: string): TColor;
var
  ASkinInfo: TdxSkinInfo;
  skinColor: TdxSkinColor;
begin
  if Trim(colorName) = '' then
    raise ECantGetSkinElementEsException.Create('Empty colorName');

  if not RootLookAndFeel.SkinPainter.GetPainterData(ASkinInfo) then
    raise ECantGetSkinElementEsException.Create('not RootLookAndFeel.SkinPainter.GetPainterData(ASkinInfo)');

  skinColor := ASkinInfo.Skin.GetColorByName(colorName);
  if skinColor = nil then
     raise ECantGetSkinElementEsException.CreateFmt('skinColor = nil for colorName= "%s"', [colorName]);

  Result:= skinColor.Value;
end;

{ TdxStandardControlsSkinHelper }

constructor TdxStandardControlsSkinHelper.PrivateCreate;
begin
  inherited Create();
  LeftOffsetForDxEdit:= 4;
end;

constructor TdxStandardControlsSkinHelper.Create;
begin
  PrivateCreate();
  raise ESkinHelperException.Create('Предполагается, что это Singleton. Use TdxStandardControlsSkinHelper.GetInstance() instead');
end;

destructor TdxStandardControlsSkinHelper.Destroy;
begin
  inherited;
end;

class function TdxStandardControlsSkinHelper.GetInstance: TdxStandardControlsSkinHelper;
begin
  Result:= StandardControlsSkinHelper;
end;

procedure TdxStandardControlsSkinHelper.ActiveFormChanged(Sender: TObject);
begin
  if NeedProcess and Assigned(Sender) then
    ProcessForm((Sender as TScreen).ActiveCustomForm)
end;

function TdxStandardControlsSkinHelper.NeedProcess: Boolean;
begin
  Result := RootLookAndFeel.Painter.InheritsFrom(TdxSkinLookAndFeelPainter);
end;

type
  THackcxCustomEdit = class(TcxCustomEdit);

procedure TdxStandardControlsSkinHelper.ProcessControl(AControl: TControl);
  procedure TuneCxCustomEdit(edit: THackcxCustomEdit);
  begin
    if (edit = nil) or (edit.InnerControl = nil) then exit;
    if LeftOffsetForDxEdit> 0 then  // а не поплывут ли вычисляемые контролом размеры?
      SendMessage(edit.InnerControl.Handle, EM_SETMARGINS, EC_LEFTMARGIN + EC_RIGHTMARGIN, LeftOffsetForDxEdit*$20001+$10000);
    if edit.Properties.ReadOnly
       and not (svColor in edit.Style.AssignedValues) then
      edit.Style.Color := GetSkinFormContentColor();
  end;
begin
  if AControl is TcxLabel then
    TcxLabel(AControl).Transparent := True;

  if (AControl is TcxCustomEdit) then
    TuneCxCustomEdit(THackcxCustomEdit(aControl));

  if (AControl.Parent is TForm) or (AControl.Parent is TFrame) then
  begin
    if (AControl is TPanel) and TPanel(AControl).ParentColor then
      TPanel(AControl).Color := GetSkinFormContentColor();

    if (AControl is TcxGroupBox) and TcxGroupBox(AControl).ParentColor then
      TcxGroupBox(AControl).Style.Color := GetSkinFormContentColor();

    if (AControl is TcxScrollBox) and TcxScrollBox(AControl).ParentColor then
      TcxScrollBox(AControl).Color := GetSkinFormContentColor();

    if (AControl is TcxRadioButton) and TcxRadioButton(AControl).ParentColor then
      TcxRadioButton(AControl).Color := GetSkinFormContentColor();

    if (AControl is TFrame) and TFrame(AControl).ParentColor then
      TFrame(AControl).Color := GetSkinFormContentColor();
  end;
end;

procedure TdxStandardControlsSkinHelper.ProcessControls(AControl: TControl);
var i: integer;
begin
  if AControl = nil then
    Exit;

  ProcessControl(AControl);
  if AControl is TWinControl then
  begin
    for i := 0 to TWinControl(AControl).ControlCount - 1 do
      ProcessControls(TWinControl(AControl).Controls[i]);
  end;

  if AControl is TdxCustomDockControl then
  begin
    for i := 0 to TdxCustomDockControl(AControl).ControlCount - 1 do
      ProcessControls(TdxCustomDockControl(AControl).Controls[i]);
  end;
end;

procedure TdxStandardControlsSkinHelper.ProcessForm(AForm: TCustomForm);
begin
  if AForm = nil then
    Exit;
  ProcessControls(AForm);
end;

procedure TdxStandardControlsSkinHelper.ProcessScreen;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    ProcessForm(Screen.Forms[I]);
end;

procedure TdxStandardControlsSkinHelper.RootLookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  if NeedProcess then
    ProcessScreen;
  if Assigned(FPrevRootLookAndFeelChanged) then
    FPrevRootLookAndFeelChanged(Sender, AChangedValues);
end;

procedure TdxStandardControlsSkinHelper.SetLeftOffsetForDxEdit(const Value: Integer);
begin
  if Value > 20 then
    raise ESkinHelperException.CreateFmt('Value is too big (%d). 20 is max', [Value]);
  FLeftOffsetForDxEdit := Value;
end;

procedure TdxStandardControlsSkinHelper.ResetEventHandlers;
begin
  RootLookAndFeel.OnChanged := FPrevRootLookAndFeelChanged;
end;

procedure TdxStandardControlsSkinHelper.AssignEventHandlers;
begin
  FPrevRootLookAndFeelChanged := RootLookAndFeel.OnChanged;
  RootLookAndFeel.OnChanged := RootLookAndFeelChanged;
  Screen.OnActiveFormChange := ActiveFormChanged;
end;

{ TesSkinElementGetter }

class function TesSkinElementGetter.GetElement(const groupName, elementName: string): TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
  group: TdxSkinControlGroup;
  element: TdxSkinElement;
begin
  if Trim(groupName) = '' then
    raise ECantGetSkinElementEsException.Create('Empty groupName');
  if Trim(elementName) = '' then
    raise ECantGetSkinElementEsException.Create('Empty elementName');

  if not RootLookAndFeel.SkinPainter.GetPainterData(ASkinInfo) then
    raise ECantGetSkinElementEsException.Create('not RootLookAndFeel.SkinPainter.GetPainterData(ASkinInfo)');

  group:= ASkinInfo.Skin.GetGroupByName(groupName);
  if group = nil then
     raise ECantGetSkinElementEsException.CreateFmt('group = nil for groupName= "%s"', [groupName]);
  element:= group.GetElementByName(elementName);
  if element = nil then
     raise ECantGetSkinElementEsException.CreateFmt('element = nil for elementName= "%s"', [elementName]);
  Result:= Element;
end;

initialization
  RegisterStandardControlsSkinHelper;

finalization
  UnregisterStandardControlsSkinHelper;

end.
