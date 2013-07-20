object Form1: TForm1
  Left = 380
  Top = 178
  Width = 356
  Height = 336
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 155
    Top = 5
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label2: TLabel
    Left = 155
    Top = 20
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label3: TLabel
    Left = 155
    Top = 35
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label4: TLabel
    Left = 155
    Top = 50
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label5: TLabel
    Left = 155
    Top = 65
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label6: TLabel
    Left = 155
    Top = 80
    Width = 9
    Height = 13
    Caption = '...'
  end
  object Label7: TLabel
    Left = 85
    Top = 5
    Width = 43
    Height = 13
    Caption = 'last error:'
  end
  object Label8: TLabel
    Left = 85
    Top = 20
    Width = 65
    Height = 13
    Caption = 'last response:'
  end
  object Label9: TLabel
    Left = 85
    Top = 35
    Width = 56
    Height = 13
    Caption = 'readerstate:'
  end
  object Label10: TLabel
    Left = 85
    Top = 50
    Width = 43
    Height = 13
    Caption = 'ICC type:'
  end
  object Label11: TLabel
    Left = 85
    Top = 65
    Width = 65
    Height = 13
    Caption = 'vendor name:'
  end
  object Label12: TLabel
    Left = 85
    Top = 80
    Width = 42
    Height = 13
    Caption = 'serial nr.:'
  end
  object Label13: TLabel
    Left = 85
    Top = 95
    Width = 41
    Height = 13
    Caption = 'protocol:'
  end
  object Label14: TLabel
    Left = 155
    Top = 95
    Width = 9
    Height = 13
    Caption = '...'
  end
  object bt_Init: TButton
    Left = 5
    Top = 5
    Width = 62
    Height = 19
    Caption = 'Init'
    TabOrder = 0
    OnClick = bt_InitClick
  end
  object bt_Open: TButton
    Left = 5
    Top = 25
    Width = 62
    Height = 19
    Caption = 'Open'
    TabOrder = 1
    OnClick = bt_OpenClick
  end
  object bt_Connect: TButton
    Left = 5
    Top = 45
    Width = 62
    Height = 19
    Caption = 'Connect'
    TabOrder = 2
    OnClick = bt_ConnectClick
  end
  object bt_Close: TButton
    Left = 5
    Top = 65
    Width = 62
    Height = 19
    Caption = 'Close'
    TabOrder = 3
    OnClick = bt_CloseClick
  end
  object bt_Disconnect: TButton
    Left = 5
    Top = 85
    Width = 62
    Height = 19
    Caption = 'Disconnect'
    TabOrder = 4
    OnClick = bt_DisconnectClick
  end
  object bt_Send: TButton
    Left = 5
    Top = 105
    Width = 62
    Height = 19
    Caption = 'Send'
    TabOrder = 5
    OnClick = bt_SendClick
  end
  object Memo1: TMemo
    Left = 75
    Top = 130
    Width = 271
    Height = 176
    TabOrder = 6
  end
  object pcsc: TPCSCConnector
    OnCardInserted = pcscCardInserted
    OnCardActive = pcscCardActive
    OnCardRemoved = pcscCardRemoved
    OnCardInvalid = pcscCardInvalid
    OnReaderWaiting = pcscReaderWaiting
    OnReaderListChange = pcscReaderListChange
    OnError = pcscError
    Left = 315
    Top = 5
  end
end
