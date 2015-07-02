object SCUMMExpLogForm: TSCUMMExpLogForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Message Log'
  ClientHeight = 670
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object vsttrMessages: TVirtualStringTree
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 582
    Height = 662
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    DefaultNodeHeight = 19
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Segoe UI'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoVisible]
    Images = SCUMMExpMainDMod.imgLstLogMsgs
    Indent = 4
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toGridExtensions, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnBeforeCellPaint = vsttrMessagesBeforeCellPaint
    OnGetText = vsttrMessagesGetText
    OnGetImageIndex = vsttrMessagesGetImageIndex
    Columns = <
      item
        MaxWidth = 40
        Options = [coEnabled, coParentBidiMode, coParentColor, coVisible, coFixed, coUseCaptionAlignment]
        Position = 0
        Spacing = 8
        Width = 40
        WideText = 'Type'
      end
      item
        MaxWidth = 100
        MinWidth = 100
        Options = [coEnabled, coParentBidiMode, coParentColor, coVisible, coFixed]
        Position = 1
        Width = 100
        WideText = 'Time'
      end
      item
        MaxWidth = 110
        MinWidth = 110
        Options = [coEnabled, coParentBidiMode, coParentColor, coVisible]
        Position = 2
        Width = 110
        WideText = 'Source'
      end
      item
        Options = [coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible]
        Position = 3
        Width = 290
        WideText = 'Message'
      end>
  end
end
