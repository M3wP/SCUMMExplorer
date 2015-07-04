object SCUMMExpSplashForm: TSCUMMExpSplashForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 193
  ClientWidth = 350
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 333
    Height = 33
    Alignment = taCenter
    AutoSize = False
    Caption = 'SCUMM Explorer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lblProgress: TLabel
    Left = 8
    Top = 124
    Width = 333
    Height = 13
    Alignment = taCenter
    AutoSize = False
  end
  object lblDetail: TLabel
    Left = 8
    Top = 60
    Width = 333
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Caption = 'Starting'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object prgbrProgress: TProgressBar
    Left = 8
    Top = 156
    Width = 333
    Height = 17
    DoubleBuffered = False
    ParentDoubleBuffered = False
    Smooth = True
    TabOrder = 0
  end
end
