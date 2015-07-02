object SCUMMExpAboutForm: TSCUMMExpAboutForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'SCUMM Explorer About'
  ClientHeight = 346
  ClientWidth = 437
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClick = FormClick
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  DesignSize = (
    437
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 4
    Top = 4
    Width = 128
    Height = 128
    Proportional = True
    Stretch = True
    OnClick = FormClick
  end
  object Label1: TLabel
    Left = 144
    Top = 8
    Width = 154
    Height = 25
    Caption = 'SCUMM Explorer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = FormClick
  end
  object Label2: TLabel
    Left = 144
    Top = 72
    Width = 179
    Height = 13
    Caption = 'Copyright '#169' 2015, Daniel England.'
    OnClick = FormClick
  end
  object Label3: TLabel
    Left = 144
    Top = 87
    Width = 101
    Height = 13
    Caption = 'All Rights Reserved.'
    OnClick = FormClick
  end
  object Label4: TLabel
    Left = 144
    Top = 110
    Width = 282
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Released under the terms of the GPL, version 3 or later.'
    WordWrap = True
    OnClick = FormClick
    ExplicitWidth = 297
  end
  object Label5: TLabel
    Left = 144
    Top = 44
    Width = 42
    Height = 13
    Caption = 'Version:'
    OnClick = FormClick
  end
  object Label6: TLabel
    Left = 200
    Top = 44
    Width = 151
    Height = 13
    Caption = '0.00.0397 - DEVELOPMENT - '#945
    OnClick = FormClick
  end
  object Label7: TLabel
    Left = 8
    Top = 142
    Width = 248
    Height = 13
    Caption = 'Portions translated from the SCUMMVM project.'
    OnClick = FormClick
  end
  object Label9: TLabel
    Left = 8
    Top = 232
    Width = 418
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This program is distributed in the hope that it will be useful, ' +
      'but WITHOUT ANY WARRANTY; without even the implied warranty of M' +
      'ERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU' +
      ' General Public License for more details.'
    WordWrap = True
    OnClick = FormClick
  end
  object Label10: TLabel
    Left = 8
    Top = 184
    Width = 418
    Height = 46
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This program is free software: you can redistribute it and/or mo' +
      'dify it under the terms of the GNU General Public License as pub' +
      'lished by the Free Software Foundation, either version 3 of the ' +
      'License, or (at your option) any later version.'
    WordWrap = True
    OnClick = FormClick
  end
  object Label8: TLabel
    Left = 8
    Top = 158
    Width = 412
    Height = 13
    Caption = 
      'Special thanks to the team for doing such a wonderful job (given' +
      ' that its in C'#8230').'
    OnClick = FormClick
  end
  object LinkLabel1: TLinkLabel
    Left = 8
    Top = 295
    Width = 418
    Height = 38
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'You should have received a copy of the GNU General Public Licens' +
      'e along with this program.  If not, see <a href="http://www.gnu.' +
      'org/licenses/">http://www.gnu.org/licenses/</a>.'
    TabOrder = 0
    OnClick = FormClick
  end
end
