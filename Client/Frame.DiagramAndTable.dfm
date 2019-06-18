object FrmDiagramAndTable: TFrmDiagramAndTable
  Left = 0
  Top = 0
  Width = 799
  Height = 427
  TabOrder = 0
  object Splitter1: TcxSplitter
    Left = 0
    Top = 193
    Width = 799
    Height = 7
    Cursor = crVSplit
    AlignSplitter = salTop
  end
  object Panel1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 0
    Height = 193
    Width = 799
    object imgDiagram: TDiagramImage
      Left = 1
      Top = 1
      Width = 797
      Height = 191
      OnMarkerMoved = imgDiagramMarkerMoved
    end
  end
  object vstTable: TVirtualStringTree
    Left = 0
    Top = 200
    Width = 799
    Height = 227
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.Height = 56
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoOwnerDraw, hoRestrictDrag, hoShowImages, hoVisible]
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowRoot, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnAdvancedHeaderDraw = vstTableAdvancedHeaderDraw
    OnColumnResize = vstTableColumnResize
    OnGetText = vstTableGetText
    OnGetNodeDataSize = vstTableGetNodeDataSize
    OnHeaderClick = vstTableHeaderClick
    OnHeaderDrawQueryElements = vstTableHeaderDrawQueryElements
    Columns = <
      item
        Position = 0
        Width = 150
        WideText = #1044#1072#1090#1072
      end>
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 568
    Top = 48
  end
end
