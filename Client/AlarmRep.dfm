object AlarmRepDlg: TAlarmRepDlg
  Left = 526
  Top = 290
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = #1040#1088#1093#1080#1074' '#1072#1074#1072#1088#1080#1081
  ClientHeight = 619
  ClientWidth = 977
  Color = clBtnFace
  Constraints.MinHeight = 550
  Constraints.MinWidth = 684
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TcxSplitter
    Left = 0
    Top = 377
    Width = 977
    Height = 7
    Cursor = crVSplit
    AlignSplitter = salTop
  end
  object mArch: TcxMemo
    Left = 0
    Top = 384
    Align = alClient
    ParentFont = False
    Properties.ReadOnly = True
    Properties.ScrollBars = ssBoth
    Style.Font.Charset = RUSSIAN_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -15
    Style.Font.Name = 'Courier New'
    Style.Font.Style = []
    Style.IsFontAssigned = True
    TabOrder = 0
    Height = 235
    Width = 977
  end
  inline frmAlarms: TFrmReportBlank
    Left = 0
    Top = 0
    Width = 977
    Height = 377
    Align = alTop
    Constraints.MinHeight = 210
    TabOrder = 1
    inherited frmTree: TObjTreeFrame
      Width = 977
      Height = 272
      inherited Splitter1: TcxSplitter
        Top = 42
        Width = 977
      end
      inherited Tree: TVirtualStringTree
        Width = 977
        Height = 42
      end
      inherited frmNodes: TfrmNodeTree
        Top = 46
        Width = 977
        inherited Tree: TVirtualStringTree
          Width = 977
        end
        inherited Panel1: TcxGroupBox
          Width = 977
        end
        inherited ilNodeTree: TImageList
          Bitmap = {
            494C01010A006800680010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
            0000000000003600000028000000400000003000000001002000000000000030
            0000000000000000000000000000000000000000000000000000000000000000
            0000FAF5EE12E6C7A069DBAF7697D8AA6E9FDAAD7598E4C49C6CFAF5EE120000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000F9F3
            EA17DFB5808FE7C79EAEF7EAD9C4FCF7EECCF7EAD9C5E6C49AB1DBAF7895F9F2
            E918000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000E9CB
            A566E9C79CAEF8E7CDCCF4D6ACCCF3CFA0CCF4D6ACCCF8E7CDCCE6C498B2E5C5
            9D6C000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000E3BA
            878BF4DAB8C4F5D6ABCCF3D3A6CCF3D3A6CCF3D3A6CCF5D6ABCCF3D9B6C5DDB2
            7B94000000000000000000000000000000000000000000000000000000000000
            0000656463FF666463FF666564FF666665FF666564FF646362FF686766FF6A69
            68FF000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000E4BB
            888BF6DDBACAF5D7AFCCF5D7AFCCF5D7AFCCFAEBD7CCF5D7AFCCF6DCBACBDEB2
            7A9600000000000000000000000000000000000000000000000000000000DCDC
            DC9FACABABFFFFFFFFFFFEFDFEFFFEFDFDFFFEFDFDFFFFFFFFFFFFFFFFFF6767
            67FBEAEBEAAB0000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000E9C4
            967CF4DCBCBDF9E6CCCCFBECDACCF9E6CCCCF7DDBBCCF7DDBBCCF3D9B9BFE3BC
            8B85000000000000000000000000000000006B6969FF757272FF747070FF9C9E
            9CFFAAA9AAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA4A3
            A2FFA4A4A3FF726F70FF757272FF6A6867FF0000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000F2DB
            BE4DEDCDA39DFCF2E4CCFFFFFFCCFBEFE0CCF7E2C6CCF9E6CCCCEAC79CA4EED6
            B75300000000000000000000000000000000E6E6E664CBCCCCFFBDBDBDFF4D4F
            4DFF8A8889FF525150FF5E5B5BFF5E5D5CFF5F5C5AFFC1C0C0FF555353FF5957
            57FF454545FFAAACABFFCACACAFFDEDFDF820000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000FCF8
            F30EEBC6977EF9EBD9BEFBF3E7CCFBEFDECCF9E8D1CCF6E5CEBEE7BF8E86FCF7
            F20F00000000000000000000000000000000000000000000000000000000B7B8
            B8C7A9A9A9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9492
            92FFC4C4C4D2F2F2F21000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000F5E2C942EFD0A792FBF1E3CCFBEDDCCCFBF1E3CCEDCCA297F3DEC4460000
            0000000000000000000000000000000000000000000000000000000000000000
            0000AEADADFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F8D
            8FB2000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000FEFBF907EFCDA372F8E8D5B5FCF3E6CCF7E8D4B6ECC99D77FCFBF80869A7
            6699378733CC69A7669900000000000000000000000000000000000000000000
            000000000000D4D4D4305B5A59FF5B5858FFDFDEDEFF00000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000F8E7D337F1D4AF8CFEFAF4CCF1D3AD8EF6E5D03A000000003C9B
            33CC2BDF1AFF3C9B33CC00000000000000000000000000000000000000000000
            00000000000000000000D6D6D644B4B3B4FFFFFFFF4500000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000FEFBF808F1D0A671FEFAF4C6F0CEA37387B766A13EA333CC3EA3
            33CC3DE22CFF3EA333CC3EA333CC6EBA66990000000000000000000000000000
            0000000000000000000000000000ABACACFF0000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000F7E2C747F5DBBA8AF6E1C6483FAA33CC52E741FF52E7
            41FF52E741FF52E741FF52E741FF3FAA33CC0000000000000000000000000000
            000000000000FAFAFA19F9F9F8CFB1B0AFFFEAEAE9E900000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000FBF0E225F3D3A96FFBF0E22570C3669941B033CC41B0
            33CC66EB55FF41B033CC41B033CC70C366990000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000FDF9F30FF4D5AC6CFDF9F30F000000000000000043B6
            33CC75EE64FF43B633CC00000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000FAEAD73500000000000000000000000072CB
            669943BA33CC72CB669900000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000CCC7
            C63A9E918E71C3C3C33CF4F4F40B000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000D9BEA45CC194679900000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000EFEFEF10E3E3E31CE4E4
            E41BE4E4E41BE4E4E41BE4E4E41BE4E4E41BE4E4E41BE4E4E41BE4E4E41BE4E4
            E41BE4E4E41BE4E4E41BE9E9E916FBFBFB040000000000000000000000000000
            0000000000000000000000000000000000000000000000000000E4DCD433EDA2
            6BF2DD7D56FF775855AAE2E2E21D000000000000000000000000000000000000
            000000000000DBAC71A3E3BF91BBF9EFE0CAF3E1C9C7D8A86BA8F0DFCA3A0000
            000000000000000000000000000000000000000000000000000000000000DBC0
            A55CAF7337CCAF7337CC00000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000EFEFEF10B0B0B04FA3A3
            A35CA6A6A659A5A5A55AA6A6A659A6A6A659A6A6A659A6A6A659A6A6A659A6A6
            A659A6A6A659A7A7A758E0E0E01FFEFEFE010000000000000000000000000000
            0000FEFEFE01F2F2F20DF0F0F00FF7F7F7080000000000000000FEEACD51FED8
            9FFFFCA96BFF83483FC2EAEAEA15000000000000000000000000000000000000
            0000F9F1E71BEDD0AAC1F8E3C9CCF3D2A5CCF5D7B0CCECCDA6C4D8A96CA60000
            0000000000000000000000000000000000000000000000000000DDC2A65CB377
            3ACCFFBF27FFB3773ACC00000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000001E5786FC1F58
            86FF1F5988FF417499FF3D4953FF343433FF434343FF4C5861FF165384FF1653
            84FF205A8AFF427398FDF4F4F40B00000000000000000000000000000000BEBC
            BC46796F6FA06B5F5FB6716868A59F9E9E62E0E0E01F00000000EAE6E220FEDA
            A8ECEBB89CFF9786857AFCFCFC03000000000000000000000000000000000000
            0000E2B8838EF4D5ACCCF3D2A6CCF3D2A6CCF3D2A6CCF3D2A6CCEECC9EC50000
            00000000000000000000000000000000000000000000DFC4A85CB87C3DCCFFC4
            35FFFFBB19FFB87C3DCCB87C3DCCB87C3DCCB87C3DCCB87C3DCCB97D3FCAC18D
            56B2D3AE877EF4EAE02000000000000000000000000000000000326992FD3268
            93FF386F98FF5D89ABFF778189FF878787FF797979FF63676AFF27638FFF2763
            8FFF336C96FF5582A4FE00000000000000000000000000000000A39A9A737D59
            59FB805A5AFF764F4FFF704747FF634242F1827D7D87C8C8C837DFDEDE21B589
            77CE985B52FF7769689FB5B5B54AF9F9F9060000000000000000000000000000
            0000E1B3799CF5D7AFCCF4D6ADCCF4D6ADCCFAEBD6CCF4D6ADCCF4D8B3CA0000
            000000000000000000000000000000000000E7D2BC48BD8141CCFFCA46FFFFB2
            00FFFFB70FFFFFBB1BFFFFBB19FFFFBA18FFFFBA16FFFFB914FFFEB813FFF4B0
            1BF8DC9C2DE7C58D4CC1EAD7C241000000000000000000000000DEDBD9E2DAD7
            D5FFDAD7D5FFD9D6D4FF6C6C6CFF757575FF6B6B6BFF696969FFCDCAC8FFCAC8
            C6FFC7C4C2FFCBC8C6EB000000000000000000000000C2BFBF45906D6DFD9275
            75FF836363FF7B5A5AFF6A4646FF643C3CFF684343F86C5656D66E6262AD8265
            67FD9B8383FF7A5555FF8E89897CFBFBFB040000000000000000000000000000
            0000EAC89D74FBF1E3CCFFFFFFCCF7DEBCCCF7DEBCCCF7DEBCCCEDCEA6BB0000
            000000000000000000000000000000000000E9D4BD48C38745CCFED161FFFBB0
            05FFFDBE2AFFFECA4BFFFEC845FFFEC53DFFFDC134FFFDBE2AFFFCBA1FFFFCB3
            0EFFFBB20AFFECAB28F3CA9250C1F5ECE2200000000000000000DDDAD8E27373
            73FF5A5A5AFFD4D2D0FF666666FF6C6C6CFF626262FF606060FFC6C3C1FF6A6A
            6AFF555555FFC2BFBEEB000000000000000000000000A592929CAA9292FFA086
            86FFA18787FF8B6C6CFF866767FF724E4EFF816060FF917272FF744F4FFF987D
            7DFFB3A0A0FF7B5858FF8F8A8A7BFBFBFB040000000000000000000000000000
            0000F3DEC347FCF3E7CCFBEFDFCCF7E2C4CCF7E2C4CCF8E4C9CCE7C293AE0000
            00000000000000000000000000000000000000000000E5CAAD5CC78B49CCFCD8
            7BFFF9C756FFC78B49CCC78B49CCC78B49CCC78B49CCC78B49CCCC9149D1DCA0
            43E3F1AF28FDF2AB18FFDCA13FE6DCB78E7E0000000000000000D9D7D5E26666
            66FF4E4E4EFFCFCCCAFF535252FF535353FF494949FF585757FFBCBAB9FF5C5C
            5CFF4C4C4CFFBAB8B7EB000000000000000000000000B4A1A1C6C2B1B1FFB6A2
            A2FFE2DADAFFD1C4C4FFBDAAAAFFAB9595FFC3B2B2FFD6CACAFFBCA9A9FFB19C
            9CFFBDAAAAFF846262FF918C8C79000000000000000000000000000000000000
            000000000000F2D8B8AEFCF3E9CCFAEAD5CCFAEAD5CCF0D5B2B2EAC79A770000
            0000000000000000000000000000000000000000000000000000E8CDAE5CCD92
            4ECCFDE092FFCD924ECC00000000000000000000000000000000F8F0E71BDDB5
            878ADBA24DE3E6A62DFFE5AA41F8D3A064B20000000000000000D6D3D1E2CFCC
            CAFFCDCAC7FFCAC7C5FFC6C3C1FFC3C0BEFFBEBBBAFFBAB7B6FFB7B4B3FFB3B0
            AFFFAFACABFFB5B2B1EB000000000000000000000000CDC1C1B6DBD0D0FFD0C3
            C3FFE1DADAFFF8F7F7FFF0EBEBFFC8B9B9FFDCD2D2FFFEFEFEFFF9F7F7FFE6DC
            DCFFDED3D3FF9B7F7FF7BDBBBB45000000000000000000000000000000000000
            000000000000EBC5948CFDF7F0CAFBEEDDCCFBEEDDCCE8BF8C94F8EEE0240000
            000000000000000000000000000000000000000000000000000000000000EBD0
            B05CD39752CCD39752CC0000000000000000000000000000000000000000F9F1
            E81BD69B53D1E0A84AFFE0A94CFFD49853CA0000000000000000074678FF0746
            78FF074678FF074678FF074679FF074678FF074679FF064679FF074678FF0746
            79FF074679FF114C7CFE000000000000000000000000E3DDDD78EDE7E7FFF0EB
            EBFFD0C3C3FFC2B1B1FFECE7E7FFF1ECECFFC0AFAFFFC2AFAFFFB6A5A5EDB3A5
            A5BCAD9696BEB3ABAB6300000000000000000000000000000000000000000000
            000000000000FAF1E520EEC99B86FDF6ECCCFEF9F3CCF9EFE322000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000EDD2B25CE1B480990000000000000000000000000000000000000000FAF2
            E81BDA9F59D1E1AD5EFFE7B465FFD89D57CA0000000000000000114F81FF114F
            81FF114F81FF114F81FF114F80FF114F81FF114F80FF124F80FF114F81FF114F
            80FF114F80FF1D5684FC000000000000000000000000FBFAFA14E8E3E3CDEDE9
            E9FFD7CBCBFFB6A2A2FFBEACACFFF9F8F8FFBCA8A8FF9A7C7CFF918C8C88F6F6
            F609000000000000000000000000000000000000000000000000000000000000
            00000000000000000000F0CEA374FEFBF7CCF9EAD7B700000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000FAF2E91BE8C0
            8F8AE9B56BE3F0B86CFFF1BF74F8E2AE6FB20000000000000000215D8CFF215D
            8CFF215D8CFF215D8CFF225E8CFF215D8CFF215D8CFF225E8CFF215E8CFF215D
            8BFF215D8CFF306893EE00000000000000000000000000000000FBFAFA1FEEE8
            E8A9E6DBDBF5D0BDBDF8D2C2C2FCF0ECECFFB7A4A4FF7D5858FF7A6E6EA5EFEF
            EF10000000000000000000000000000000000000000000000000000000000000
            00000000000000000000FEFBF808F9EBDAAEF1CEA37500000000000000000000
            0000000000000000000000000000000000000000000000000000EABD8699E3A7
            5DCCE3A75DCCE3A75DCCE3A75DCCE3A75DCCE3A75DCCE3A75DCCE4AB61D1ECBB
            6FE3F4C377FDF3BB6FFFEDBB70E6EDC89B7E00000000000000004A7CA3F34A7D
            A3F34A7CA3F34A7DA3F3497DA4F343779FFE41749DFF4A7DA3F343779FFE4074
            9DFF4A7CA3F35987ABDF0000000000000000000000000000000000000000FEFE
            FE01F1F0F016E7E4E420CCC5C375FEF6F1FFD8C2BDFF916968FF776868A5D9D9
            D926F7F7F7080000000000000000000000000000000000000000000000000000
            0000000000000000000000000000F2D1A879F6E0C54A00000000000000000000
            0000000000000000000000000000000000000000000000000000E7AB60CCFEDE
            92FFFCD68AFFFCD68AFFFCD68AFFFCD589FFFCD589FFFCD589FFFBD286FFF9CA
            7EFFF8C77BFFF6CB7FF3EAB46EC1FBF1E6200000000000000000000000000000
            00000000000000000000000000007497B4F2395472FB000000007497B4F23954
            72FB000000000000000000000000000000000000000000000000000000000000
            0000EFEFF0108BA1AE746786959A739DB6DB89BDD8FF5B93ABFC2377A3E46572
            7E9AE8E8E8170000000000000000000000000000000000000000000000000000
            0000000000000000000000000000F4D3AA6FFDF9F30F00000000000000000000
            0000000000000000000000000000000000000000000000000000EBAF63CCFFE0
            94FFFEDA8EFFFEDA8EFFFEDA8EFFFEDA8EFFFEDA8EFFFEDA8EFFFEDA8DFFFCD5
            89F8F5C679E7EDB771C1F8E5CD41000000000000000000000000000000000000
            000000000000000000000000000090B9D4F4405D7CFC0000000090B9D4F4405D
            7CFC000000000000000000000000000000000000000000000000000000000000
            00008BD4F47925E0FEFF17CFFEFF2AB9FBFF61D0FDFF16B8F3FF0FAAF2FF7288
            9B8DFEFEFE010000000000000000000000000000000000000000000000000000
            0000000000000000000000000000F5D5AD6C0000000000000000000000000000
            0000000000000000000000000000000000000000000000000000F1C48C99EDB1
            66CCEDB166CCEDB166CCEDB166CCEDB166CCEDB166CCEDB166CCEDB267CAEFBB
            79B2F4CFA07EFCF2E62000000000000000000000000000000000000000000000
            000000000000000000000000000065829DE6425C78ED0000000065829DE6425C
            78ED000000000000000000000000000000000000000000000000000000000000
            0000C3E9FD3C50C1F2B957B2DEAE84A8BE8081AECF99A6B2BD59C8CDD437F8F8
            F807000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000EEEEEE13A2A3A3A4B2B2B2AAEAEAEA19000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000F0F0F00FA2A1A16EA2A1A16EEFEFEF10000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000F8F8F80768696AC4626467FE7A7A7BFE6E6F6FD7EDEDED140000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000ECECEC1377797BA99C9EA1FF9B9EA2FF787A7DA3FBFBFB040000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000EAEAEA173B3E3FFC75787BFFAEAFAFFF818383FEDEDEDE250000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000005D78A7FF5D78A7FF8FA9CFFF7293C0FF5D78A7FF5D78A7FF0000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000FBE5E029EC7F
            64D0E86341FFEC7F64D0FBE5E029000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000E7E8E81A4D5051FD6F7072FF7A7A7AFF999999FEDCDCDC290000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000A4B3CD8F5D78A7FFB2D0FAFF90B4E5FF5D78A7FFA4B3CD8F0000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000E1795DD0E491
            7AFFE8BFB2FFE4917AFFE1795DD0000000000000000000000000000000000000
            0000FDF4F112F6B09E83F07C5FD9EE6A49F8F07C5FD9F6B09E83FDF4F1120000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000E1E0DF25A09C9A979B9793A99E9A96AAA6A3A096DAD9D82F0000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000005D78A7FF5177B4FF94AFD9FF7595C5FF5D78A7FF5D78A7FF0000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000B3B3B34D9B9B9B669B9B9B66CC5331FFF0C7
            BAFFF0C7BAFFF0C7BAFFCC5331FF00000000000000000000000000000000FDF3
            F112F18B71C1F3A28EFFF9DCD7FFFDF5F4FFF8DAD6FFF29F8CFFF18B71C1FDF3
            F112000000000000000000000000000000000000000000000000000000000000
            000000000000DEDCDB31E7E7E738DBD9D54BDBD9D64AEAE9E935DEDBDA340000
            0000000000000000000000000000000000000000000000000000000000000000
            0000FAFCFE06A0C9E56E77AACEB77EADD2C37BAACFC379A9CBBB8FBCDA82FAFC
            FE06000000000000000000000000000000000000000000000000000000000000
            00000000000000000000000000009F9F9F660000000000000000C96C50D0DB90
            78FFF7CEC1FFDB9078FFC96C50D000000000000000000000000000000000EDAB
            9A83E99E8BFFF0E6E6FFEDE2E2FFDD5D3BFFEDE2E2FFEEE3E2FFE69985FFEDAB
            9A83000000000000000000000000000000000000000000000000000000000000
            0000FBFBFB05D2CFCE4BEAEAEA36D7D3CF53D6D2CE54EBEBEB35D7D5D447F9F9
            F908000000000000000000000000000000000000000000000000000000000000
            0000CDE4F43B689FC1B4B5E4FB7C9FD5F2929ED6F293B5E5FB7B6BA0C3B2CCE4
            F43C000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000A3A3A3660000000000000000F2E1DB29BF67
            4BD0B14523FFBF674BD0F2E1DB2900000000000000000000000000000000CC69
            4CD9EDDCD8FFDFD9D9FFDFD9D9FFE8BFB3FFE8E3E3FFDFD9D9FFE2CFCAFFCC69
            4CD9000000000000000000000000000000000000000000000000000000000000
            0000DCDAD835E1E0DF41ECECEB38E2DEDB4BE3DFDC54EAE9E946E6E5E53FDFDD
            DB3400000000000000000000000000000000000000000000000000000000EEF6
            FB147EB4D89388B8D1ACA6EBFF8774D3EDB171D4EDB39EECFF8D84B9D0AE7EB4
            D893EEF6FB140000000000000000000000000000000000000000000000000000
            0000000000000000000000000000A9A9A9660000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000B24A
            29F8F6F4F3FFE5E4E4FFD7D5D5FFB04523FFD19C8BFFD3D0D0FFE0DCDBFFB24A
            29F800000000000000000000000000000000000000000000000000000000F5F4
            F40ECECCCA53E9E9E93AEAEAEA48E4E3E05DE6E5E267E9E8E861EAEAEA47D9D7
            D649F4F4F310000000000000000000000000000000000000000000000000B3D6
            ED5878ABC8AFA0E3F6917BECFDAF5EE7F4D75AE8F5DA6FEEFDB893E4F59A74AA
            C8B1B3D5ED580000000000000000000000000000000000000000000000000000
            0000000000000000000000000000AEAEAE660000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000B863
            46D9F0E4DFFFF1F1F1FFF6F6F6FFEEEEEEFFAC4826FFE0E0E0FFE8DAD6FFB863
            46D900000000000000000000000000000000000000000000000000000000D8D5
            D33ADFDEDE43E8E8E840E2E0DF7BE2DFDC7FE1DFDB80E5E2E174E6E5E466E7E6
            E65BDFDDDC3E0000000000000000000000000000000000000000F6FAFC0B84B6
            D88F97CAE19D94EEFF9A8BF5FFD5AAFCFEF8A7FDFEFA7FF8FFDF85F1FFA890CB
            E1A184B6D88FF6FAFC0B0000000000000000F3E3DE29C37256D0B65331FFC372
            56D0F3E3DE290000000000000000B3B3B3660000000000000000F3E3DE29C372
            56D0B65331FFC37256D0F3E3DE2900000000000000000000000000000000DDA9
            9883DBA18EFFFAF9F9FFCB8066FFBD5937FFCB8066FFFAF9F9FFDAA18EFFDDA9
            988300000000000000000000000000000000000000000000000000000000CECB
            C94AE6E6E66CE5E5E540E2E0DF64DEDBDA7DE0DEDD69E0DCDB84DAD6D48CEAE8
            E89DD7D5D36B0000000000000000000000000000000000000000EDF5FB157EB2
            D399A4DAF18E91ECFFA1A7F9FFE3EEFFFFFFEDFFFFFF9DFAFFEA87F1FFB19CDB
            F19480B2D498EDF5FB150000000000000000CE7E62D0D7927BFFE8BFB2FFD792
            7BFFCE7E62D00000000000000000B7B7B7660000000000000000CE7E62D0D792
            7BFFE8BFB2FFD7927BFFCE7E62D000000000000000000000000000000000FBF4
            F112DF9379C1EAB09DFFFAEDE9FFFEFCFBFFFAEDE9FFEAB09DFFDF9379C1FBF4
            F11200000000000000000000000000000000000000000000000000000000D0CD
            CC48E9E9E992DCDCDC49DDDDDD4FD3CECC8DCFC9C6A8CCC6C3A5C8C1BEB3E4DF
            DED6CDC8C67B0000000000000000000000000000000000000000F5F9FC0C82B6
            D791A1D4EC919EEBFF8D97F3FFC7B6FBFFEBB3FBFFED8EF5FFCF92EDFF9B9CD5
            EC9482B5D791F5F9FC0C0000000000000000D46F4DFFF0C7BAFFF0C7BAFFF0C7
            BAFFD46F4DFFBABABA66BABABA66BABABA66BABABA66BABABA66D46F4DFFF0C7
            BAFFF0C7BAFFF0C7BAFFD46F4DFF000000000000000000000000000000000000
            0000FCF5F312F4C0AE83ED977AD9EB8867F8ED977AD9F4C0AE83FCF5F3120000
            000000000000000000000000000000000000000000000000000000000000D7D3
            D14AD5D3D18EDBDADA57E0E0E046DEDDDC58CDC6C2A5C5BAB5C5C2B6B2CACAC3
            BFE1CBC6C35F000000000000000000000000000000000000000000000000AAD0
            EA6289B9D5A8C1EFFFA6A4EEFFAD93F0FFB990F1FFBA9BEFFFAFB7EEFF9E87BA
            D5A6AAD0EA62000000000000000000000000E79478D0EEA891FFF7CEC1FFEEA8
            91FFE79478D00000000000000000000000000000000000000000E79478D0EEA8
            91FFF7CEC1FFEEA891FFE79478D0000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000000000000000000000000000FBFB
            FA06A79C97B0BBB2AEADCFCBC97AD5D2D083DEDEDD62C6BEBBA9B2A59FD5998B
            85C8F6F5F40E000000000000000000000000000000000000000000000000E6F2
            FA1D82B8DC8DA5C6D9C4F2FAFCE1EFFFFFE6EAFEFFDFE3F7FBCCA1C7DBB782B8
            DC8DE6F2FA1D000000000000000000000000FBEBE529EF9C80D0EC8664FFEF9C
            80D0FBEBE5290000000000000000000000000000000000000000FBEBE529EF9C
            80D0EC8664FFEF9C80D0FBEBE529000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000ECE9E81FA29791ADD3CECCD8EAE9E8C3EBEAEABDDDD9D8DC9D928CB5E3E0
            DE2A000000000000000000000000000000000000000000000000000000000000
            0000DBECF72A8ABEE18181B5D4AB9CC6DEB99BC7DEB681B5D5A98ABFE181DBEC
            F72A000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000FAF9F907D9D6D34FDBD8D685DEDBD987DDDAD854F7F7F60A0000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000F0F7FB12C7E1F341AACFE962AACFE962C6E0F243F0F7FB120000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000424D3E000000000000003E000000
            2800000040000000300000000100010000000000800100000000000000000000
            000000000000000000000000FFFFFF00F01FFFFF00000000E00FFFFF00000000
            E00FFFFF00000000E00FF00F00000000E00FE00700000000E00F000000000000
            E00F000000000000E00FE00300000000F01FF00F00000000F003F87F00000000
            F823FC7F00000000F800FEFF00000000FC00F87F00000000FC00FFFF00000000
            FC63FFFF00000000FEE3FFFF00000000FFFFFFE1FFFFF3FF8000FFC1F81FE3FF
            8000F0C1F01FC3FFC001E041F01F8003C003C000F01F0001C0038000F01F0000
            C0038000F01F8000C0038001F81FC3C0C0038001F81FE3E0C0038003F83FF3E0
            C003800FFC7FFFC0C003C00FFC7FC000C003E007FE7FC000FE4FF007FE7FC001
            FE4FF007FEFFC003FE4FF00FFFFFFFFFFFFFFC3FFC3FFFFFFFFFF81FF81FFFFF
            FFFFF81FF81FFFC1FFFFF81FF81FFFC1F01FF81FF81FFE01E00FF81FF00FFEC1
            E00FF00FF00FFEC1E00FF00FE007FEFFE00FE007E007FEFFE00FE007C00306C1
            E00FE007C00306C1E00FE007C0030001F01FE007E00707C1FFFFE007E00707C1
            FFFFF00FF00FFFFFFFFFF81FF81FFFFF00000000000000000000000000000000
            000000000000}
        end
      end
    end
    inherited Panel2: TcxGroupBox
      Width = 977
      inherited btGo: TcxButton
        OnClick = frmAlarmsbtGoClick
      end
      inherited pbProgress: TcxProgressBar
        Width = 964
      end
    end
    inherited pnPeriod: TcxGroupBox
      Width = 977
      inherited cmbIntervalType: TcxComboBox
        Left = 212
      end
    end
  end
end
