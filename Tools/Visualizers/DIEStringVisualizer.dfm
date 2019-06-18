object StringViewerFrame: TStringViewerFrame
  Left = 0
  Top = 0
  Width = 900
  Height = 530
  TabOrder = 0
  DesignSize = (
    900
    530)
  object labelLength: TLabel
    Left = 16
    Top = 8
    Width = 55
    Height = 13
    Caption = 'labelLength'
  end
  object labelStrings: TLabel
    Left = 184
    Top = 8
    Width = 55
    Height = 13
    Caption = 'labelStrings'
  end
  object speedButtonCopy: TSpeedButton
    Left = 877
    Top = 3
    Width = 23
    Height = 22
    Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074' '#1073#1091#1092#1077#1088
    Anchors = [akTop, akRight]
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003333330B7FFF
      FFB0333333777F3333773333330B7FFFFFB0333333777F3333773333330B7FFF
      FFB0333333777F3333773333330B7FFFFFB03FFFFF777FFFFF77000000000077
      007077777777777777770FFFFFFFF00077B07F33333337FFFF770FFFFFFFF000
      7BB07F3FF3FFF77FF7770F00F000F00090077F77377737777F770FFFFFFFF039
      99337F3FFFF3F7F777FF0F0000F0F09999937F7777373777777F0FFFFFFFF999
      99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
      99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
      93337FFFF7737777733300000033333333337777773333333333}
    NumGlyphs = 2
    OnClick = speedButtonCopyClick
    ExplicitLeft = 547
  end
  object memoString: TMemo
    Left = 0
    Top = 27
    Width = 900
    Height = 503
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'memoString')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 570
    ExplicitHeight = 383
  end
end
