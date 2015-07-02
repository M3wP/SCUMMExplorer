object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SCUMM V2 Costume Animator'
  ClientHeight = 527
  ClientWidth = 628
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 306
    Height = 527
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 301
    DesignSize = (
      306
      527)
    object Label9: TLabel
      Left = 96
      Top = 13
      Width = 33
      Height = 13
      Caption = 'Label9'
    end
    object Label10: TLabel
      Left = 8
      Top = 458
      Width = 35
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Speed:'
      ExplicitTop = 434
    end
    object Label11: TLabel
      Left = 8
      Top = 354
      Width = 76
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Utilised Layers:'
      ExplicitTop = 330
    end
    object Label8: TLabel
      Left = 8
      Top = 48
      Width = 64
      Height = 13
      Caption = 'Information:'
    end
    object Label14: TLabel
      Left = 196
      Top = 354
      Width = 57
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Additional:'
      ExplicitTop = 330
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Open...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 220
      Top = 494
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 220
      Top = 307
      Width = 75
      Height = 25
      Caption = 'Select'
      Enabled = False
      TabOrder = 2
      OnClick = Button3Click
    end
    object TrackBar8: TTrackBar
      Left = 60
      Top = 455
      Width = 241
      Height = 29
      Anchors = [akLeft, akBottom]
      Max = 2000
      Min = 20
      ParentShowHint = False
      PageSize = 20
      Position = 1860
      ShowHint = True
      ShowSelRange = False
      TabOrder = 3
      TickStyle = tsNone
      OnChange = TrackBar8Change
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 378
      Width = 85
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 9'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 401
      Width = 85
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 10'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 424
      Width = 85
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 11'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox5: TCheckBox
      Left = 112
      Top = 378
      Width = 85
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 12'
      Enabled = False
      TabOrder = 7
    end
    object CheckBox6: TCheckBox
      Left = 112
      Top = 401
      Width = 78
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 13'
      Enabled = False
      TabOrder = 8
    end
    object CheckBox7: TCheckBox
      Left = 112
      Top = 424
      Width = 85
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 14'
      Enabled = False
      TabOrder = 9
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 67
      Width = 77
      Height = 17
      Caption = 'No Mirror'
      Enabled = False
      TabOrder = 10
    end
    object CheckBox15: TCheckBox
      Left = 16
      Top = 90
      Width = 105
      Height = 17
      Caption = 'No Animations'
      Enabled = False
      TabOrder = 11
    end
    object CheckBox16: TCheckBox
      Left = 128
      Top = 67
      Width = 105
      Height = 17
      Caption = 'Other Layers'
      Enabled = False
      TabOrder = 12
    end
    object ListBox1: TListBox
      Left = 196
      Top = 378
      Width = 93
      Height = 63
      Anchors = [akLeft, akBottom]
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 13
    end
    object Button4: TButton
      Left = 139
      Top = 494
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Save...'
      TabOrder = 14
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 220
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Batch...'
      TabOrder = 15
      OnClick = Button5Click
    end
    object PageControl1: TPageControl
      Left = 6
      Top = 121
      Width = 295
      Height = 180
      ActivePage = TabSheet2
      Style = tsFlatButtons
      TabOrder = 16
      object TabSheet1: TTabSheet
        Caption = 'Standard'
        object RadioGroup2: TRadioGroup
          Left = 200
          Top = 0
          Width = 87
          Height = 148
          Caption = 'Facing  '
          ItemIndex = 1
          Items.Strings = (
            'Right'
            'Left'
            'Down'
            'Up')
          TabOrder = 0
        end
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 194
          Height = 148
          Caption = 'Animation Type  '
          TabOrder = 1
          object Label13: TLabel
            Left = 16
            Top = 116
            Width = 38
            Height = 13
            Caption = 'Mouth:'
          end
          object RadioButton1: TRadioButton
            Left = 16
            Top = 24
            Width = 113
            Height = 17
            Caption = 'Walking'
            Checked = True
            TabOrder = 0
            TabStop = True
          end
          object RadioButton2: TRadioButton
            Left = 16
            Top = 47
            Width = 113
            Height = 17
            Caption = 'Standing'
            TabOrder = 1
          end
          object CheckBox1: TCheckBox
            Left = 16
            Top = 80
            Width = 69
            Height = 17
            BiDiMode = bdLeftToRight
            Caption = 'Head'
            Checked = True
            ParentBiDiMode = False
            State = cbChecked
            TabOrder = 2
          end
          object ComboBox1: TComboBox
            Left = 72
            Top = 112
            Width = 110
            Height = 21
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 3
            Text = 'None'
            Items.Strings = (
              'None'
              'Open'
              'Closed'
              'Talking')
          end
          object CheckBox17: TCheckBox
            Left = 97
            Top = 24
            Width = 94
            Height = 17
            Caption = 'Use Near/Far'
            Checked = True
            State = cbChecked
            TabOrder = 4
            OnClick = CheckBox17Click
          end
          object CheckBox18: TCheckBox
            Left = 97
            Top = 47
            Width = 94
            Height = 17
            Caption = 'Near'
            TabOrder = 5
          end
          object CheckBox19: TCheckBox
            Left = 97
            Top = 80
            Width = 85
            Height = 17
            BiDiMode = bdLeftToRight
            Caption = 'No Body'
            ParentBiDiMode = False
            TabOrder = 6
          end
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Advanced'
        ImageIndex = 1
        object Label15: TLabel
          Left = 3
          Top = 6
          Width = 32
          Height = 13
          Caption = 'Slot 1:'
        end
        object Label16: TLabel
          Left = 3
          Top = 33
          Width = 32
          Height = 13
          Caption = 'Slot 2:'
        end
        object Label17: TLabel
          Left = 3
          Top = 60
          Width = 32
          Height = 13
          Caption = 'Slot 3:'
        end
        object Label18: TLabel
          Left = 3
          Top = 87
          Width = 32
          Height = 13
          Caption = 'Slot 4:'
        end
        object ComboBox2: TComboBox
          Left = 86
          Top = 3
          Width = 119
          Height = 21
          Style = csDropDownList
          TabOrder = 0
        end
        object ComboBox3: TComboBox
          Left = 86
          Top = 30
          Width = 119
          Height = 21
          Style = csDropDownList
          TabOrder = 1
        end
        object ComboBox4: TComboBox
          Left = 86
          Top = 57
          Width = 119
          Height = 21
          Style = csDropDownList
          TabOrder = 2
        end
        object ComboBox5: TComboBox
          Left = 86
          Top = 84
          Width = 119
          Height = 21
          Style = csDropDownList
          TabOrder = 3
        end
        object CheckBox20: TCheckBox
          Left = 3
          Top = 120
          Width = 97
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Mirrored:'
          TabOrder = 4
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 306
    Top = 0
    Width = 322
    Height = 527
    Align = alRight
    FullRepaint = False
    ParentBackground = False
    TabOrder = 1
    ExplicitLeft = 301
    DesignSize = (
      322
      527)
    object Label2: TLabel
      Left = 6
      Top = 300
      Width = 38
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 9:'
      ExplicitTop = 276
    end
    object Label3: TLabel
      Left = 6
      Top = 328
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 10:'
      ExplicitTop = 304
    end
    object Label4: TLabel
      Left = 6
      Top = 356
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 11:'
      ExplicitTop = 332
    end
    object Label5: TLabel
      Left = 6
      Top = 384
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 12:'
      ExplicitTop = 360
    end
    object Label6: TLabel
      Left = 6
      Top = 412
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 13:'
      ExplicitTop = 388
    end
    object Label7: TLabel
      Left = 6
      Top = 440
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 14:'
      ExplicitTop = 416
    end
    object Label1: TLabel
      Left = 6
      Top = 278
      Width = 114
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Animation Sequences:'
      ExplicitTop = 220
    end
    object PaintBox1: TPaintBox
      Left = 1
      Top = 1
      Width = 320
      Height = 240
      Align = alTop
      Color = clBtnFace
      ParentColor = False
      OnPaint = PaintBox1Paint
    end
    object Label12: TLabel
      Left = 6
      Top = 481
      Width = 29
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'State:'
      ExplicitTop = 423
    end
    object TrackBar2: TTrackBar
      Left = 57
      Top = 297
      Width = 265
      Height = 33
      Anchors = [akLeft, akBottom]
      Enabled = False
      ShowSelRange = False
      TabOrder = 0
    end
    object TrackBar1: TTrackBar
      Left = 57
      Top = 325
      Width = 265
      Height = 33
      Anchors = [akLeft, akBottom]
      Enabled = False
      ShowSelRange = False
      TabOrder = 5
    end
    object CheckBox9: TCheckBox
      Left = 82
      Top = 480
      Width = 69
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 9'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox10: TCheckBox
      Left = 82
      Top = 503
      Width = 69
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 10'
      Enabled = False
      TabOrder = 7
    end
    object CheckBox11: TCheckBox
      Left = 164
      Top = 480
      Width = 69
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 11'
      Enabled = False
      TabOrder = 8
    end
    object CheckBox12: TCheckBox
      Left = 164
      Top = 503
      Width = 69
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 12'
      Enabled = False
      TabOrder = 9
    end
    object CheckBox13: TCheckBox
      Left = 248
      Top = 480
      Width = 69
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 13'
      Enabled = False
      TabOrder = 10
    end
    object CheckBox14: TCheckBox
      Left = 248
      Top = 503
      Width = 69
      Height = 17
      AllowGrayed = True
      Anchors = [akLeft, akBottom]
      Caption = 'Layer 14'
      Enabled = False
      TabOrder = 11
    end
    object TrackBar3: TTrackBar
      Left = 57
      Top = 353
      Width = 265
      Height = 33
      Anchors = [akLeft, akBottom]
      Enabled = False
      ShowSelRange = False
      TabOrder = 1
    end
    object TrackBar4: TTrackBar
      Left = 57
      Top = 381
      Width = 265
      Height = 33
      Anchors = [akLeft, akBottom]
      Enabled = False
      ShowSelRange = False
      TabOrder = 2
    end
    object TrackBar5: TTrackBar
      Left = 57
      Top = 409
      Width = 265
      Height = 33
      Anchors = [akLeft, akBottom]
      Enabled = False
      ShowSelRange = False
      TabOrder = 3
    end
    object TrackBar6: TTrackBar
      Left = 57
      Top = 437
      Width = 265
      Height = 33
      Anchors = [akLeft, akBottom]
      Enabled = False
      ShowSelRange = False
      TabOrder = 4
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 248
    Top = 52
  end
  object OpenDialog1: TOpenDialog
    Left = 248
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'png'
    Left = 288
    Top = 8
  end
end
