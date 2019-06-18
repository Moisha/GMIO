object BigWindowFormForDocking: TBigWindowFormForDocking
  Left = 0
  Top = 0
  ClientHeight = 441
  ClientWidth = 803
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inline BigWindowFrame1: TBigWindowFrame
    Left = 0
    Top = 0
    Width = 803
    Height = 441
    Align = alClient
    TabOrder = 0
    ExplicitLeft = -44
    ExplicitTop = 25
    inherited pdDiagramLen: TcxGroupBox
      Width = 803
    end
    inherited DiagramAndTable: TFrmDiagramAndTable
      Width = 803
      Height = 384
      inherited Splitter1: TcxSplitter
        Width = 803
        ExplicitTop = 193
      end
      inherited Panel1: TcxGroupBox
        Width = 803
      end
      inherited vstTable: TVirtualStringTree
        Width = 803
        Height = 184
      end
    end
  end
end
