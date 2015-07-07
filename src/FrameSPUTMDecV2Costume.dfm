object SPUTMDecV2CostumeFrame: TSPUTMDecV2CostumeFrame
  Left = 0
  Top = 0
  Height = 437
  Width = 593
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  object Panel2: TPanel
    Left = 271
    Top = 0
    Width = 322
    Height = 437
    Align = alRight
    FullRepaint = False
    ParentBackground = False
    TabOrder = 0
    object Label2: TLabel
      Left = 6
      Top = 269
      Width = 36
      Height = 13
      Caption = 'Limb 9:'
    end
    object Label3: TLabel
      Left = 6
      Top = 297
      Width = 42
      Height = 13
      Caption = 'Limb 10:'
    end
    object Label4: TLabel
      Left = 6
      Top = 325
      Width = 42
      Height = 13
      Caption = 'Limb 11:'
    end
    object Label5: TLabel
      Left = 6
      Top = 353
      Width = 42
      Height = 13
      Caption = 'Limb 12:'
    end
    object Label6: TLabel
      Left = 6
      Top = 381
      Width = 42
      Height = 13
      Caption = 'Limb 13:'
    end
    object Label7: TLabel
      Left = 6
      Top = 409
      Width = 42
      Height = 13
      Caption = 'Limb 14:'
    end
    object Label1: TLabel
      Left = 6
      Top = 247
      Width = 114
      Height = 13
      Caption = 'Animation Sequences:'
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
      ExplicitTop = 3
    end
    object Label12: TLabel
      Left = 6
      Top = 450
      Width = 29
      Height = 13
      Caption = 'State:'
    end
    object TrackBar2: TTrackBar
      Left = 57
      Top = 266
      Width = 265
      Height = 33
      Enabled = False
      ShowSelRange = False
      TabOrder = 0
    end
    object TrackBar1: TTrackBar
      Left = 57
      Top = 294
      Width = 265
      Height = 33
      Enabled = False
      ShowSelRange = False
      TabOrder = 5
    end
    object CheckBox9: TCheckBox
      Left = 82
      Top = 449
      Width = 69
      Height = 17
      AllowGrayed = True
      Caption = 'Limb 9'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox10: TCheckBox
      Left = 82
      Top = 472
      Width = 69
      Height = 17
      AllowGrayed = True
      Caption = 'Limb 10'
      Enabled = False
      TabOrder = 7
    end
    object CheckBox11: TCheckBox
      Left = 164
      Top = 449
      Width = 69
      Height = 17
      AllowGrayed = True
      Caption = 'Limb 11'
      Enabled = False
      TabOrder = 8
    end
    object CheckBox12: TCheckBox
      Left = 164
      Top = 472
      Width = 69
      Height = 17
      AllowGrayed = True
      Caption = 'Limb 12'
      Enabled = False
      TabOrder = 9
    end
    object CheckBox13: TCheckBox
      Left = 248
      Top = 449
      Width = 69
      Height = 17
      AllowGrayed = True
      Caption = 'Limb 13'
      Enabled = False
      TabOrder = 10
    end
    object CheckBox14: TCheckBox
      Left = 248
      Top = 472
      Width = 69
      Height = 17
      AllowGrayed = True
      Caption = 'Limb 14'
      Enabled = False
      TabOrder = 11
    end
    object TrackBar3: TTrackBar
      Left = 57
      Top = 322
      Width = 265
      Height = 33
      Enabled = False
      ShowSelRange = False
      TabOrder = 1
    end
    object TrackBar4: TTrackBar
      Left = 57
      Top = 350
      Width = 265
      Height = 33
      Enabled = False
      ShowSelRange = False
      TabOrder = 2
    end
    object TrackBar5: TTrackBar
      Left = 57
      Top = 378
      Width = 265
      Height = 33
      Enabled = False
      ShowSelRange = False
      TabOrder = 3
    end
    object TrackBar6: TTrackBar
      Left = 57
      Top = 406
      Width = 265
      Height = 33
      Enabled = False
      ShowSelRange = False
      TabOrder = 4
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 271
    Height = 437
    Align = alClient
    ParentBackground = False
    TabOrder = 1
    object Label10: TLabel
      Left = 8
      Top = 409
      Width = 35
      Height = 13
      Caption = 'Speed:'
    end
    object Label11: TLabel
      Left = 8
      Top = 305
      Width = 74
      Height = 13
      Caption = 'Utilised Limbs:'
    end
    object Label8: TLabel
      Left = 8
      Top = 6
      Width = 64
      Height = 13
      Caption = 'Information:'
    end
    object Label14: TLabel
      Left = 196
      Top = 305
      Width = 57
      Height = 13
      Caption = 'Additional:'
    end
    object Button2: TButton
      Left = 220
      Top = 445
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 220
      Top = 240
      Width = 75
      Height = 25
      Caption = 'Select'
      Enabled = False
      TabOrder = 1
      OnClick = Button3Click
    end
    object TrackBar8: TTrackBar
      Left = 60
      Top = 406
      Width = 241
      Height = 29
      Max = 2000
      Min = 20
      ParentShowHint = False
      PageSize = 20
      Position = 1860
      ShowHint = True
      ShowSelRange = False
      TabOrder = 2
      TickStyle = tsNone
      OnChange = TrackBar8Change
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 329
      Width = 85
      Height = 17
      Caption = 'Limb 9'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 352
      Width = 85
      Height = 17
      Caption = 'Limb 10'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 375
      Width = 85
      Height = 17
      Caption = 'Limb 11'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox5: TCheckBox
      Left = 112
      Top = 329
      Width = 85
      Height = 17
      Caption = 'Limb 12'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox6: TCheckBox
      Left = 112
      Top = 352
      Width = 78
      Height = 17
      Caption = 'Limb 13'
      Enabled = False
      TabOrder = 7
    end
    object CheckBox7: TCheckBox
      Left = 112
      Top = 375
      Width = 85
      Height = 17
      Caption = 'Limb 14'
      Enabled = False
      TabOrder = 8
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 25
      Width = 77
      Height = 17
      Caption = 'No Mirror'
      Enabled = False
      TabOrder = 9
    end
    object RadioGroup2: TRadioGroup
      Left = 208
      Top = 86
      Width = 87
      Height = 148
      Caption = 'Facing  '
      ItemIndex = 1
      Items.Strings = (
        'Right'
        'Left'
        'Down'
        'Up')
      TabOrder = 10
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 86
      Width = 194
      Height = 148
      Caption = 'Animation Type  '
      TabOrder = 11
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
    object CheckBox15: TCheckBox
      Left = 16
      Top = 48
      Width = 105
      Height = 17
      Caption = 'No Animations'
      Enabled = False
      TabOrder = 12
    end
    object CheckBox16: TCheckBox
      Left = 128
      Top = 25
      Width = 105
      Height = 17
      Caption = 'Other Limbs'
      Enabled = False
      TabOrder = 13
    end
    object ListBox1: TListBox
      Left = 196
      Top = 329
      Width = 93
      Height = 63
      Color = clBtnFace
      Enabled = False
      ItemHeight = 13
      TabOrder = 14
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 272
    Top = 8
  end
end
