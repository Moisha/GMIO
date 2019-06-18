object ComLogFrm: TComLogFrm
  Left = 660
  Top = 373
  Caption = 'ComLogFrm'
  ClientHeight = 453
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object mCOMLog: TcxMemo
    Left = 0
    Top = 41
    Align = alClient
    Lines.Strings = (
      '')
    ParentFont = False
    Properties.ScrollBars = ssBoth
    Properties.WordWrap = False
    Style.Font.Charset = ANSI_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -12
    Style.Font.Name = 'Courier New'
    Style.Font.Style = []
    Style.IsFontAssigned = True
    TabOrder = 0
    OnKeyDown = mCOMLogKeyDown
    Height = 412
    Width = 729
  end
  object Panel1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 1
    DesignSize = (
      729
      41)
    Height = 41
    Width = 729
    object btnCopy: TcxButton
      Left = 633
      Top = 8
      Width = 89
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #1057#1082#1086#1087#1080#1088#1086#1074#1072#1090#1100
      TabOrder = 0
      OnClick = btnCopyClick
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 80
    Top = 64
  end
end
