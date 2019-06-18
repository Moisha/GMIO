object ToolContainer: TToolContainer
  Left = 317
  Top = 185
  ActiveControl = pgTools
  Align = alClient
  BorderStyle = bsNone
  Caption = 'ToolContainer'
  ClientHeight = 323
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    235
    323)
  PixelsPerInch = 96
  TextHeight = 13
  object pgTools: TcxPageControl
    Left = -1
    Top = 0
    Width = 238
    Height = 325
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object pmTools: TPopupMenu
    AutoPopup = False
    Left = 12
    Top = 16
  end
end
