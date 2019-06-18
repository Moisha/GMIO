object ControlOneAddPrmFrame: TControlOneAddPrmFrame
  Left = 0
  Top = 0
  Width = 361
  Height = 71
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 361
    Height = 71
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 43
      Height = 13
      Caption = #1055#1086#1076#1087#1080#1089#1100
    end
    object eCaption: TEdit
      Left = 56
      Top = 4
      Width = 289
      Height = 24
      TabOrder = 0
      Text = 'eCaption'
    end
    object lePrm: TLabeledEdit
      Left = 8
      Top = 44
      Width = 337
      Height = 24
      EditLabel.Width = 31
      EditLabel.Height = 13
      EditLabel.Caption = #1050#1072#1085#1072#1083
      TabOrder = 1
    end
  end
end
