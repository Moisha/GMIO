unit h_GMClient;

interface

uses Windows, JvInterpreter, Classes, FlexBase, FlexProps, FlexUtils, SysUtils, FlexControls, Graphics, UITypes, Variants,
     GMDBClasses, GMGlobals, JvInterpreter_all, Dialogs, GMConst, Threads.GMClient;

type
  TGlobalArrayContainer = class(TPersistent)
  public
    class function GetInstance(): TGlobalArrayContainer;
  end;

implementation

const SiriusUnit = 'Sirius';

var
  ScriptGlobalsDataArray: array [0 .. 1000] of Variant;
  GlobalArrayContainer: TGlobalArrayContainer;

{ TGlobalArrayContainer }

class function TGlobalArrayContainer.GetInstance: TGlobalArrayContainer;
begin
  Result := GlobalArrayContainer;
end;

procedure RegiterFlexClasses(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  FlexClassList: array[0..13] of TPersistentClass = (
                    TFlexControl,
                    TFlexControlWithPenAndBrush,
                    TFlexBox,
                    TFlexText,
                    TFlexEllipse,
                    TFlexArc,
                    TFlexPicture,
                    TFlexCurve,
                    TFlexConnector,
                    TFlexRegularPolygon,
                    TFlexPanel,

                    TBrushProp,
                    TPenProp,
                    TFontProp);
var
 i: int;
begin
  for i := 0 to High(FlexClassList) do
  begin
    Classes.RegisterClass(FlexClassList[i]);
    JvInterpreterAdapter.AddClass('FlexGraphics', FlexClassList[i], FlexClassList[i].ClassName());
  end;
end;

procedure RegiterGMClasses();
begin
  RegisterClasses([TGMParam]);
  RegisterClasses([TGlobalArrayContainer]);
end;

procedure AddColorConstants(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clBlack', TColors.Black);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clMaroon', TColors.Maroon);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clGreen', TColors.Green);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clOlive', TColors.Olive);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clNavy', TColors.Navy);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clPurple', TColors.Purple);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clTeal', TColors.Teal);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clGray', TColors.Gray);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clSilver', TColors.Silver);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clRed', TColors.Red);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clLime', TColors.Lime);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clYellow', TColors.Yellow);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clBlue', TColors.Blue);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clFuchsia', TColors.Fuchsia);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clAqua', TColors.Aqua);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clLtGray', TColors.LtGray);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clDkGray', TColors.DkGray);
  JvInterpreterAdapter.AddConst(SiriusUnit, 'clWhite', TColors.White);
end;

procedure TGlobalArrayContainer_get_Data(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := ScriptGlobalsDataArray[int(Args.Values[0])];
end;

procedure TGlobalArrayContainer_set_Data(const Value: Variant; Args: TJvInterpreterArgs);
begin
  ScriptGlobalsDataArray[int(Args.Values[0])] := Value;
end;

procedure TFlexControl_get_Left(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFlexControl(Args.Obj).Left div PixelScaleFactor;
End;

procedure TFlexControl_set_Left(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFlexControl(Args.Obj).Left := Value * PixelScaleFactor;
End;

procedure TFlexControl_get_Top(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFlexControl(Args.Obj).Top div PixelScaleFactor;
End;

procedure TFlexControl_set_Top(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFlexControl(Args.Obj).Top := Value * PixelScaleFactor;
End;

procedure TFlexControl_get_Width(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFlexControl(Args.Obj).Width div PixelScaleFactor;
End;

procedure TFlexControl_set_Width(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFlexControl(Args.Obj).Width := Value * PixelScaleFactor;
End;

procedure TFlexControl_get_Height(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFlexControl(Args.Obj).Height div PixelScaleFactor;
End;

procedure TFlexControl_set_Height(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFlexControl(Args.Obj).Height := Value * PixelScaleFactor;
End;

procedure TFlexControl_get_Visible(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFlexControl(Args.Obj).Visible;
End;

procedure TFlexControl_set_Visible(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFlexControl(Args.Obj).Visible := Value;
End;

procedure TFlexControl_get_Hint(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFlexControl(Args.Obj).Hint;
End;

procedure TFlexControl_set_Hint(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFlexControl(Args.Obj).Hint := Value;
End;

procedure TFlexText_get_Text(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFlexText(Args.Obj).TextProp.Text;
End;

procedure TFlexText_set_Text(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFlexText(Args.Obj).TextProp.Text := Value;
End;

procedure TFlexControlWithPenAndBrush_get_Pen(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := O2V(TFlexControlWithPenAndBrush(Args.Obj).PenProp);
End;

procedure TFlexControlWithPenAndBrush_get_Brush(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := O2V(TFlexControlWithPenAndBrush(Args.Obj).BrushProp);
End;

procedure TFlexControl_get_Channels(var Value: Variant; Args: TJvInterpreterArgs);
var s: string;
    prop: TChannelProp;
    i, n: int;
Begin
  Value := Null;
  prop := TFlexControl(Args.Obj).ChannelsProp;
  s := Args.Values[0];

  // сначала поищем по имени
  for i := 0 to prop.LinesCount - 1 do
  begin
    if Trim(UpperCase(prop.ValuesByIndex[i])) = Trim(UpperCase(s)) then
    begin
      Value := O2V(GMParamFromString(prop.Names[i]));
      Exit;
    end;
  end;

  // не нашли, возможно хотели по номеру
  n := StrToIntDef(s, -1);
  if (n >= 0) and (n < prop.LinesCount) then
    Value := O2V(GMParamFromString(prop.Names[n]));
End;

procedure TFlexControl_get_Channel(var Value: Variant; Args: TJvInterpreterArgs);
var prop: TChannelProp;
Begin
  prop := TFlexControl(Args.Obj).ChannelsProp;
  try
    Value := O2V(GMParamFromString(prop.Names[Args.Values[0]]));
  except
    on e: Exception do
      raise Exception.Create(TFlexControl(Args.Obj).Name + ', ' + e.Message);
  end;
End;

procedure TFlexText_get_Font(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFlexText(Args.Obj).FontProp);
end;

procedure TFontProp_get_Color(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TFontProp(Args.Obj).Color;
End;

procedure TFontProp_set_Color(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TFontProp(Args.Obj).Color := Value;
End;

procedure TPenProp_get_Color(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TPenProp(Args.Obj).Color;
End;

procedure TPenProp_set_Color(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TPenProp(Args.Obj).Color := Value;
End;

procedure TBrushProp_get_Color(var Value: Variant; Args: TJvInterpreterArgs);
Begin
  Value := TBrushProp(Args.Obj).Color;
End;

procedure TBrushProp_set_Color(const Value: Variant; Args: TJvInterpreterArgs);
Begin
  TBrushProp(Args.Obj).Color := Value;
End;

procedure AddFlexClasses_Props(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddGet(TFontProp, 'Color', TFontProp_get_Color, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFontProp, 'Color', TFontProp_set_Color, 0, [varEmpty]);

  JvInterpreterAdapter.AddGet(TPenProp, 'Color', TPenProp_get_Color, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TPenProp, 'Color', TPenProp_set_Color, 0, [varEmpty]);

  JvInterpreterAdapter.AddGet(TBrushProp, 'Color', TBrushProp_get_Color, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TBrushProp, 'Color', TBrushProp_set_Color, 0, [varEmpty]);
end;

procedure AddFlexClasses_Common(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddGet(TFlexControl, 'Left', TFlexControl_get_Left, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexControl, 'Left', TFlexControl_set_Left, 0, [varEmpty]);

  JvInterpreterAdapter.AddGet(TFlexControl, 'Top', TFlexControl_get_Top, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexControl, 'Top', TFlexControl_set_Top, 0, [varEmpty]);

  JvInterpreterAdapter.AddGet(TFlexControl, 'Width', TFlexControl_get_Width, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexControl, 'Width', TFlexControl_set_Width, 0, [varEmpty]);

  JvInterpreterAdapter.AddGet(TFlexControl, 'Height', TFlexControl_get_Height, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexControl, 'Height', TFlexControl_set_Height, 0, [varEmpty]);

  JvInterpreterAdapter.AddGet(TFlexControl, 'Visible', TFlexControl_get_Visible, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexControl, 'Visible', TFlexControl_set_Visible, 0, [varEmpty]);

  JvInterpreterAdapter.AddIGet(TFlexControl, 'Channels', TFlexControl_get_Channels, 1, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddGet(TFlexControl, 'Channel', TFlexControl_get_Channel, 0, [varEmpty], varEmpty);

  JvInterpreterAdapter.AddGet(TFlexControl, 'Hint', TFlexControl_get_Hint, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexControl, 'Hint', TFlexControl_set_Hint, 0, [varEmpty]);
end;

procedure AddFlexClasses_Text(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddGet(TFlexText, 'Text', TFlexText_get_Text, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexText, 'Text', TFlexText_set_Text, 0, [varEmpty]);
  JvInterpreterAdapter.AddGet(TFlexText, 'Font', TFlexText_get_Font, 0, [varEmpty], varEmpty);
end;

procedure AddFlexClasses_PenAndBrush(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddGet(TFlexControlWithPenAndBrush, 'Pen', TFlexControlWithPenAndBrush_get_Pen, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddGet(TFlexControlWithPenAndBrush, 'Brush', TFlexControlWithPenAndBrush_get_Brush, 0, [varEmpty], varEmpty);
end;

function CheckFloat(const Values: array of string): Boolean;
begin
  Result := MyStrToFloat(Values[0]) <> INCORRECT_VALUE;
  if not Result then
    ShowMessageBox('Введите число. "' + Values[0] + '" - не число.', MB_ICONSTOP);
end;

procedure TFlexPanel_InputFloat(var Value: Variant; Args: TJvInterpreterArgs);
var
  prompt, val: array [0..0] of string;
begin
  TFlexPanel(Args.Obj).ModalDialogMode := true;

  prompt[0] := Args.Values[1];
  Value := InputQuery(Args.Values[0], prompt, val, CheckFloat);
  if Value then
    Args.Values[2] := MyStrToFloat(val[0]);
end;

function CheckInt(const Values: array of string): Boolean;
var n, c: int;
begin
  Val(Values[0], n, c);
  Result := (c = 0) and (n = n) {это дебильное условие вставлено, чтобы убить warning о неиспользуемой переменной n};
  if not Result then
    ShowMessageBox('Введите целое число. "' + Values[0] + '" - не целое число.', MB_ICONSTOP);
end;

procedure TFlexPanel_InputInt(var Value: Variant; Args: TJvInterpreterArgs);
var
  prompt, val: array [0..0] of string;
begin
  TFlexPanel(Args.Obj).ModalDialogMode := true;

  prompt[0] := Args.Values[1];
  Value := InputQuery(Args.Values[0], prompt, val, CheckInt);
  if Value then
    Args.Values[2] := StrToInt(val[0]);
end;

procedure TFlexPanel_SendCommand(var Value: Variant; Args: TJvInterpreterArgs);
var order: TDataOrder;
begin
  Value := ''; // OK
  TFlexPanel(Args.Obj).ModalDialogMode := true;

  order := TDataOrders.AddSendCommandOrder(TGMParam(V2O(Args.Values[0])), Args.Values[1]);
  order.Exec();
  order.WaitFor(10000);
  Value := order.CommandOrderError();
  order.State := dosDelete;
end;

procedure TFlexPanel_SendAsyncCommand(var Value: Variant; Args: TJvInterpreterArgs);
var order: TDataOrder;
begin
  order := TDataOrders.AddSendCommandOrder(TGMParam(V2O(Args.Values[0])), Args.Values[1]);
  order.bNotify := false;
  order.Exec();
end;

procedure TFlexPanel_get_DataArray(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFlexPanel(Args.Obj).DataArray[int(Args.Values[0])];
end;

procedure TFlexPanel_set_DataArray(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFlexPanel(Args.Obj).DataArray[int(Args.Values[0])] := Value;
end;

procedure TFlexPanel_set_ActiveScheme(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFlexPanel(Args.Obj).ActiveScheme := TFlexCustomScheme(V2O(Value));
end;

procedure TFlexPanel_get_Schemes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TFlexPanel(Args.Obj).Schemes[int(Args.Values[0])]);
end;

procedure AddFlexClasses_Panel(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddGet(TFlexPanel, 'InputFloat', TFlexPanel_InputFloat, 3, [varEmpty, varEmpty, varByRef], varEmpty);
  JvInterpreterAdapter.AddGet(TFlexPanel, 'InputInt', TFlexPanel_InputInt, 3, [varEmpty, varEmpty, varByRef], varEmpty);
  JvInterpreterAdapter.AddGet(TFlexPanel, 'SendCommand', TFlexPanel_SendCommand, 2, [varEmpty, varEmpty], varEmpty);
  JvInterpreterAdapter.AddGet(TFlexPanel, 'SendAsyncCommand', TFlexPanel_SendAsyncCommand, 2, [varEmpty, varEmpty], varEmpty);
  JvInterpreterAdapter.AddIGet(TFlexPanel, 'DataArray', TFlexPanel_get_DataArray, 1, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddISet(TFlexPanel, 'DataArray', TFlexPanel_set_DataArray, 1, [varEmpty]);
  JvInterpreterAdapter.AddIGet(TFlexPanel, 'Schemes', TFlexPanel_get_Schemes, 1, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexPanel, 'ActiveScheme', TFlexPanel_set_ActiveScheme, 0, [varEmpty]);
end;

procedure TFlexPicture_get_FrameIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TFlexPicture(Args.Obj).FrameIndexProp.Value;
end;

procedure TFlexPicture_set_FrameIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TFlexPicture(Args.Obj).FrameIndexProp.Value := Value;
end;

procedure AddFlexClasses_Picture(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddGet(TFlexPicture, 'FrameIndex', TFlexPicture_get_FrameIndex, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddSet(TFlexPicture, 'FrameIndex', TFlexPicture_set_FrameIndex, 0, [varEmpty]);
end;

procedure AddFlexClasses(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  AddFlexClasses_Panel(JvInterpreterAdapter);
  AddFlexClasses_Props(JvInterpreterAdapter);
  AddFlexClasses_Common(JvInterpreterAdapter);
  AddFlexClasses_Text(JvInterpreterAdapter);
  AddFlexClasses_PenAndBrush(JvInterpreterAdapter);
  AddFlexClasses_Picture(JvInterpreterAdapter);
end;

procedure ParamByID(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(GMParams.ByID(Args.Values[0]));
end;

procedure TGMParam_get_LastVal(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TGMParam(Args.Obj).LastVal.Val;
end;

procedure TGMParam_get_AgeInSeconds(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := NowGM() - TGMParam(Args.Obj).LastVal.UTime;
end;

procedure AddGMClasses(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddGet(TGMParam, 'LastVal', TGMParam_get_LastVal, 0, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddGet(TGMParam, 'AgeInSeconds', TGMParam_get_AgeInSeconds, 0, [varEmpty], varEmpty);
end;

procedure h_FormatFloat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := FormatFloatToShow(Args.Values[0], Args.Values[1]);
end;

procedure RGB(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Windows.RGB(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

procedure RegisterCommonFunc(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  JvInterpreterAdapter.AddFunction(SiriusUnit, 'FormatFloat', h_FormatFloat, 2, [varEmpty, varEmpty], varEmpty);
  JvInterpreterAdapter.AddFunction(SiriusUnit, 'ChannelByID', ParamByID, 1, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddFunction(SiriusUnit, 'RGB', RGB, 3, [varEmpty, varEmpty, varEmpty], varEmpty);

  JvInterpreterAdapter.AddIDGet(TGlobalArrayContainer, TGlobalArrayContainer_get_Data, 1, [varEmpty], varEmpty);
  JvInterpreterAdapter.AddIDSet(TGlobalArrayContainer, TGlobalArrayContainer_set_Data, 1, [varEmpty]);
end;

procedure RegiterJvInterpreterFlexClasses(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  RegisterCommonFunc(JvInterpreterAdapter);
  RegiterFlexClasses(JvInterpreterAdapter);
  RegiterGMClasses();
  AddColorConstants(JvInterpreterAdapter);
  AddFlexClasses(JvInterpreterAdapter);
  AddGMClasses(JvInterpreterAdapter);
end;

initialization
  RegiterJvInterpreterFlexClasses(GlobalJvInterpreterAdapter());
  GlobalArrayContainer := TGlobalArrayContainer.Create();

finalization
  GlobalArrayContainer.Free();
end.
