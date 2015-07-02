object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'SCUMMv2 Object Reader'
  ClientHeight = 529
  ClientWidth = 576
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
    Height = 488
    Color = clGradientActiveCaption
    ParentColor = False
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 576
    Height = 22
    AutoSize = True
    Caption = 'ToolBar1'
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ToolButton1'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 510
    Width = 576
    Height = 19
    Panels = <>
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 22
    Width = 177
    Height = 488
    Align = alLeft
    BorderStyle = bsNone
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnChange = TreeView1Change
  end
  object Panel1: TPanel
    Left = 179
    Top = 22
    Width = 397
    Height = 488
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object Image1: TImage
      Left = 0
      Top = 324
      Width = 397
      Height = 164
      Align = alBottom
      Center = True
      ExplicitWidth = 399
    end
    object Splitter2: TSplitter
      Left = 0
      Top = 322
      Width = 397
      Height = 2
      Cursor = crVSplit
      Align = alBottom
      Color = clGradientActiveCaption
      ParentColor = False
      ExplicitTop = 321
    end
    object RichEdit1: TRichEdit
      Left = 0
      Top = 0
      Width = 397
      Height = 322
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
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 508
  end
end
