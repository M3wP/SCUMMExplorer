object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'SCUMMv2 Room Reader'
  ClientHeight = 604
  ClientWidth = 738
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 177
    Top = 22
    Width = 2
    Height = 582
    Color = clGradientActiveCaption
    ParentColor = False
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 22
    Width = 177
    Height = 582
    Align = alLeft
    BorderStyle = bsNone
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = TreeView1Change
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 738
    Height = 22
    AutoSize = True
    Caption = 'ToolBar1'
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ToolButton1'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
  end
  object Panel1: TPanel
    Left = 179
    Top = 22
    Width = 559
    Height = 582
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 0
      Top = 342
      Width = 559
      Height = 2
      Cursor = crVSplit
      Align = alBottom
      Color = clGradientActiveCaption
      ParentColor = False
      ExplicitTop = 341
    end
    object RichEdit1: TRichEdit
      Left = 0
      Top = 0
      Width = 559
      Height = 342
      Align = alClient
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Zoom = 100
    end
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 344
      Width = 559
      Height = 238
      Align = alBottom
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 497
        Height = 209
        AutoSize = True
        Proportional = True
      end
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 508
  end
end
