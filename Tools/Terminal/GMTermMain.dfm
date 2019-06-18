object Form1: TForm1
  Left = 750
  Top = 305
  Caption = 'COM '#1058#1077#1088#1084#1080#1085#1072#1083' SiriusATM'
  ClientHeight = 452
  ClientWidth = 954
  Color = clBtnFace
  Constraints.MinHeight = 246
  Constraints.MinWidth = 748
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    954
    452)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 536
    Top = 33
    Width = 35
    Height = 13
    Caption = #1056#1077#1078#1080#1084
  end
  object Label2: TLabel
    Left = 536
    Top = 76
    Width = 50
    Height = 13
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
  end
  object btGo: TButton
    Left = 861
    Top = 127
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Go'
    TabOrder = 1
    OnClick = btGoClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 158
    Width = 926
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
    OnKeyDown = Memo1KeyDown
  end
  object cmbTxt: TComboBox
    Left = 8
    Top = 127
    Width = 846
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnKeyDown = cmbTxtKeyDown
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 386
    Height = 121
    ActivePage = tsCOM
    TabOrder = 3
    OnChange = PageControl1Change
    object tsCOM: TTabSheet
      Caption = 'tsCOM'
      object Label3: TLabel
        Left = 144
        Top = 8
        Width = 48
        Height = 13
        Caption = #1057#1082#1086#1088#1086#1089#1090#1100
      end
      object Label4: TLabel
        Left = 96
        Top = 52
        Width = 52
        Height = 13
        Caption = #1057#1090#1086#1087'-'#1073#1080#1090#1099
      end
      object Label5: TLabel
        Left = 176
        Top = 52
        Width = 48
        Height = 13
        Caption = #1063#1077#1090#1085#1086#1089#1090#1100
      end
      object lePort: TLabeledEdit
        Left = 8
        Top = 24
        Width = 121
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = #1055#1086#1088#1090
        TabOrder = 0
        Text = '1'
        OnChange = lePortChange
      end
      object cmbBaud: TComboBox
        Left = 144
        Top = 24
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemIndex = 5
        TabOrder = 1
        Text = '9600'
        OnChange = lePortChange
        Items.Strings = (
          '300'
          '600'
          '1200'
          '2400'
          '4800'
          '9600'
          '19200'
          '38400'
          '57600'
          '115200')
      end
      object leLen: TLabeledEdit
        Left = 8
        Top = 68
        Width = 65
        Height = 21
        EditLabel.Width = 66
        EditLabel.Height = 13
        EditLabel.Caption = #1044#1083#1080#1085#1072' '#1089#1083#1086#1074#1072
        TabOrder = 2
        Text = '8'
        OnChange = lePortChange
      end
      object cmbStopBits: TComboBox
        Left = 96
        Top = 68
        Width = 57
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 3
        Text = '1'
        OnChange = lePortChange
        Items.Strings = (
          '1'
          '1.5'
          '2')
      end
      object cmbParity: TComboBox
        Left = 176
        Top = 68
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'NOPARITY'
        OnChange = lePortChange
        Items.Strings = (
          'NOPARITY'
          'ODDPARITY'
          'EVENPARITY'
          'MARKPARITY'
          'SPACEPARITY')
      end
      object btBreak: TButton
        Left = 288
        Top = 22
        Width = 75
        Height = 25
        Caption = 'Break'
        TabOrder = 5
        OnClick = btBreakClick
      end
    end
    object tsEthernet: TTabSheet
      Caption = 'tsEthernet'
      ImageIndex = 1
      object rgProtocol: TRadioGroup
        Left = 3
        Top = 3
        Width = 94
        Height = 87
        Caption = #1055#1088#1086#1090#1086#1082#1086#1083
        Items.Strings = (
          'TCP'
          'UDP')
        TabOrder = 0
        OnClick = rgProtocolClick
      end
      object leIP: TLabeledEdit
        Left = 136
        Top = 24
        Width = 121
        Height = 21
        EditLabel.Width = 10
        EditLabel.Height = 13
        EditLabel.Caption = 'IP'
        TabOrder = 1
      end
      object leTCPPort: TLabeledEdit
        Left = 136
        Top = 69
        Width = 121
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = #1055#1086#1088#1090
        TabOrder = 2
      end
    end
  end
  object cmbMode: TComboBox
    Left = 536
    Top = 49
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = 'HEX'
    OnChange = lePortChange
    Items.Strings = (
      'HEX'
      #1058#1077#1082#1089#1090
      'Dec')
  end
  object cmbTrailing: TComboBox
    Left = 536
    Top = 92
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 5
    OnChange = lePortChange
    Items.Strings = (
      ''
      '<CR>'
      '<CTRL-Z>'
      '<CR><LF>'
      'CRC 16'
      #1058#1101#1082#1086#1085' CRC + 0x16'
      'CRC '#1057#1055#1058'941'
      'CRC '#1057#1055#1058'961'
      'LRC <CR><LF>')
  end
  object cbListen: TCheckBox
    Left = 837
    Top = 94
    Width = 97
    Height = 17
    Anchors = [akTop, akRight]
    Caption = #1057#1083#1091#1096#1072#1090#1100' '#1087#1086#1088#1090
    TabOrder = 6
    OnClick = cbListenClick
  end
  object leWaitFirst: TLabeledEdit
    Left = 392
    Top = 48
    Width = 121
    Height = 21
    EditLabel.Width = 41
    EditLabel.Height = 13
    EditLabel.Caption = 'WaitFirst'
    TabOrder = 7
    Text = '1000'
  end
  object leWaitNext: TLabeledEdit
    Left = 392
    Top = 93
    Width = 121
    Height = 21
    EditLabel.Width = 44
    EditLabel.Height = 13
    EditLabel.Caption = 'WaitNext'
    TabOrder = 8
    Text = '100'
  end
  object cbRepeat: TCheckBox
    Left = 837
    Top = 71
    Width = 107
    Height = 17
    Anchors = [akTop, akRight]
    Caption = #1055#1086#1074#1090#1086#1088' '#1079#1072#1087#1088#1086#1089#1072
    TabOrder = 9
    OnClick = cbRepeatClick
  end
  object ssListen: TServerSocket
    Active = False
    Port = 0
    ServerType = stNonBlocking
    OnAccept = ssListenAccept
    OnGetSocket = ssListenGetSocket
    OnClientRead = ssListenClientRead
    Left = 760
    Top = 48
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = btGoClick
    Left = 672
    Top = 24
  end
end
