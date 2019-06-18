object FlexChildFrame: TFlexChildFrame
  Left = 0
  Top = 0
  Width = 721
  Height = 472
  HorzScrollBar.Tracking = True
  VertScrollBar.Tracking = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Splitter1: TcxSplitter
    Left = 0
    Top = 220
    Width = 721
    Height = 6
    Cursor = crVSplit
    AlignSplitter = salBottom
    ResizeUpdate = True
    ExplicitLeft = 3
    ExplicitTop = 208
  end
  object Flex: TFlexPanel
    Left = 0
    Top = 0
    Width = 721
    Height = 220
    HorzScrollBar.Increment = 10
    HorzScrollBar.Range = 320
    HorzScrollBar.Tracking = True
    VertScrollBar.Increment = 10
    VertScrollBar.Range = 240
    VertScrollBar.Tracking = True
    DocWidth = 320000
    DocHeight = 240000
    DocFrameColor = clBlack
    DocShadowColor = clGray
    DocSpaceBrush.Color = clBtnFace
    DocSpaceBrush.Style = bsBDiagonal
    DocSpaceFill = True
    GridStyle = gsDots
    GridColor = clGray
    GridPixColor = clSilver
    GridHorizSize = 10000
    GridVertSize = 10000
    Align = alClient
    TabOrder = 0
    OnMouseDown = FlexPanel1MouseDown
    OnKeyDown = FlexKeyDown
    ExplicitHeight = 466
  end
  inline ScriptEditorFrame1: TScriptEditorFrame
    Left = 0
    Top = 226
    Width = 721
    Height = 246
    Align = alBottom
    TabOrder = 2
    ExplicitLeft = 72
    ExplicitTop = 216
    inherited mScript: TJvHLEditor
      Width = 721
      OnChange = mScriptChange
      OnExit = mScriptExit
      ExplicitHeight = 246
    end
  end
end
