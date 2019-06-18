object PropertyTextEditor: TPropertyTextEditor
  Left = 350
  Top = 313
  Width = 489
  Height = 209
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #1056#1077#1076#1072#1082#1090#1086#1088
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 141
    Width = 481
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      481
      41)
    object btnCancel: TButton
      Left = 401
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 0
    end
    object btnOk: TButton
      Left = 321
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 1
    end
  end
  object mEditor: TMemo
    Left = 0
    Top = 0
    Width = 481
    Height = 141
    Align = alClient
    Constraints.MinHeight = 100
    Constraints.MinWidth = 200
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'mEditor')
    ParentFont = False
    TabOrder = 1
    WantTabs = True
  end
end
