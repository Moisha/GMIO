object PascalDlg: TPascalDlg
  Left = 643
  Top = 208
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #1057#1082#1088#1080#1087#1090
  ClientHeight = 512
  ClientWidth = 797
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 410
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object mScript: TSynEdit
    Left = 0
    Top = 0
    Width = 797
    Height = 471
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Gutter.Width = 0
    Highlighter = SynPasSyn1
    Lines.Strings = (
      'program k;'
      ''
      'function Main();'
      'begin'
      '  // comment'
      '  s := '#39'string'#39';'
      '  n := 10;'
      '  f := 1.1;'
      '  h := $FF;'
      'end;'
      ''
      'end.')
    RightEdge = 0
    FontSmoothing = fsmNone
  end
  object Panel1: TcxGroupBox
    Left = 0
    Top = 471
    Align = alBottom
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 1
    DesignSize = (
      797
      41)
    Height = 41
    Width = 797
    object BitBtn3: TcxButton
      Left = 8
      Top = 11
      Width = 165
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100' '#1089#1080#1085#1090#1072#1082#1089#1080#1089
      TabOrder = 0
      OnClick = BitBtn3Click
    end
    object BitBtn2: TcxButton
      Left = 636
      Top = 11
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object BitBtn1: TcxButton
      Left = 719
      Top = 11
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      OptionsImage.NumGlyphs = 2
      TabOrder = 2
    end
  end
  object SynPasSyn1: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clBlue
    KeyAttri.Style = []
    NumberAttri.Foreground = clMaroon
    FloatAttri.Foreground = clPurple
    HexAttri.Foreground = clPurple
    StringAttri.Foreground = clRed
    Left = 244
    Top = 115
  end
end
