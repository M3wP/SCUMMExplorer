object SCUMMExpMainForm: TSCUMMExpMainForm
  Left = 0
  Top = 0
  Caption = 'SCUMM Explorer'
  ClientHeight = 706
  ClientWidth = 724
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 724
    Height = 48
    AutoSize = True
    Bands = <
      item
        Control = ToolBar1
        ImageIndex = -1
        MinHeight = 21
        Width = 722
      end
      item
        Control = ToolBar2
        ImageIndex = -1
        Width = 722
      end>
    EdgeBorders = []
    ExplicitWidth = 923
    object ToolBar1: TToolBar
      Left = 11
      Top = 0
      Width = 713
      Height = 21
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 34
      Caption = 'ToolBar1'
      Menu = SCUMMExpMainDMod.MainMenu1
      ShowCaptions = True
      TabOrder = 0
      Wrapable = False
    end
    object ToolBar2: TToolBar
      Left = 11
      Top = 23
      Width = 713
      Height = 25
      Caption = 'ToolBar2'
      TabOrder = 1
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 687
    Width = 724
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitTop = 781
    ExplicitWidth = 923
  end
  inline SCUMMExpMainFrame1: TSCUMMExpMainFrame
    Left = 0
    Top = 48
    Width = 724
    Height = 639
    Align = alClient
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 2
    ExplicitTop = 48
    ExplicitWidth = 923
    ExplicitHeight = 733
    inherited Splitter1: TSplitter
      Left = 189
      Height = 639
      ExplicitHeight = 733
    end
    inherited Panel1: TPanel
      Left = 191
      Width = 533
      Height = 639
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 0
      Margins.Bottom = 2
      ExplicitWidth = 704
      ExplicitHeight = 733
      inherited Splitter2: TSplitter
        Top = 345
        Width = 533
        ExplicitWidth = 704
      end
      inherited ListView1: TListView
        Width = 533
        Height = 343
        ExplicitWidth = 704
        ExplicitHeight = 343
      end
    end
    inherited TreeView1: TTreeView
      Width = 189
      Height = 637
      ExplicitWidth = 189
      ExplicitHeight = 637
    end
  end
end
