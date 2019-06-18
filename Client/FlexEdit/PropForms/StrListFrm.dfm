object StrListPropForm: TStrListPropForm
  Left = 328
  Top = 144
  BorderStyle = bsDialog
  Caption = 'Edit text'
  ClientHeight = 249
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object mmText: TcxMemo
    Left = 8
    Top = 8
    Lines.Strings = (
      'mmText')
    Properties.ScrollBars = ssBoth
    TabOrder = 0
    Height = 197
    Width = 305
  end
  object BitBtn1: TcxButton
    Left = 156
    Top = 216
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    OptionsImage.NumGlyphs = 2
    TabOrder = 1
  end
  object BitBtn2: TcxButton
    Left = 240
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    OptionsImage.NumGlyphs = 2
    TabOrder = 2
  end
end
