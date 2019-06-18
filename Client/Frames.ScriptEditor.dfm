object ScriptEditorFrame: TScriptEditorFrame
  Left = 0
  Top = 0
  Width = 645
  Height = 246
  TabOrder = 0
  object mScript: TJvHLEditor
    Left = 0
    Top = 0
    Width = 645
    Height = 246
    Cursor = crIBeam
    Lines.Strings = (
      'unit d;'
      ''
      'implementation'
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
    Completion.ItemHeight = 13
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    BracketHighlighting.StringEscape = #39#39
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Colors.Comment.Style = [fsItalic]
    Colors.Comment.ForeColor = clGreen
    Colors.Comment.BackColor = clWindow
    Colors.Number.ForeColor = clPurple
    Colors.Number.BackColor = clWindow
    Colors.Strings.ForeColor = clRed
    Colors.Strings.BackColor = clWindow
    Colors.Symbol.ForeColor = clWindowText
    Colors.Symbol.BackColor = clWindow
    Colors.Reserved.Style = [fsBold]
    Colors.Reserved.ForeColor = clBlue
    Colors.Reserved.BackColor = clWindow
    Colors.Identifier.ForeColor = clWindowText
    Colors.Identifier.BackColor = clWindow
    Colors.Preproc.ForeColor = clOlive
    Colors.Preproc.BackColor = clWindow
    Colors.FunctionCall.ForeColor = clWindowText
    Colors.FunctionCall.BackColor = clWindow
    Colors.Declaration.ForeColor = clWindowText
    Colors.Declaration.BackColor = clWindow
    Colors.Statement.Style = [fsBold]
    Colors.Statement.ForeColor = clWindowText
    Colors.Statement.BackColor = clWindow
    Colors.PlainText.ForeColor = clWindowText
    Colors.PlainText.BackColor = clWindow
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 456
    Top = 40
    DockControlHeights = (
      0
      0
      0
      0)
    object dxBarButton1: TdxBarButton
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Category = 0
      Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Visible = ivAlways
    end
    object dxBarButton2: TdxBarButton
      Caption = #1042#1099#1088#1077#1079#1072#1090#1100
      Category = 0
      Hint = #1042#1099#1088#1077#1079#1072#1090#1100
      Visible = ivAlways
    end
    object dxBarButton3: TdxBarButton
      Caption = #1042#1089#1090#1072#1074#1080#1090#1100
      Category = 0
      Hint = #1042#1089#1090#1072#1074#1080#1090#1100
      Visible = ivAlways
    end
    object dxBarButton4: TdxBarButton
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Category = 0
      Hint = #1059#1076#1072#1083#1080#1090#1100
      Visible = ivAlways
    end
    object dxBarButton5: TdxBarButton
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
      Category = 0
      Hint = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
      Visible = ivAlways
    end
  end
  object dxBarPopupMenu1: TdxBarPopupMenu
    BarManager = dxBarManager1
    ItemLinks = <
      item
        Visible = True
        ItemName = 'dxBarButton1'
      end
      item
        Visible = True
        ItemName = 'dxBarButton3'
      end
      item
        Visible = True
        ItemName = 'dxBarButton2'
      end
      item
        Visible = True
        ItemName = 'dxBarButton4'
      end
      item
        BeginGroup = True
        Visible = True
        ItemName = 'dxBarButton5'
      end>
    UseOwnFont = False
    Left = 424
    Top = 120
  end
end
