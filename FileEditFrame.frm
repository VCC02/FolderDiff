object frFileEdit: TfrFileEdit
  Left = 0
  Height = 419
  Top = 0
  Width = 817
  ClientHeight = 419
  ClientWidth = 817
  TabOrder = 0
  OnResize = FrameResize
  DesignLeft = 86
  DesignTop = 85
  object pnlLeft: TPanel
    Left = 0
    Height = 393
    Top = 0
    Width = 400
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'pnlLeft'
    ClientHeight = 393
    ClientWidth = 400
    Color = 14286840
    Constraints.MinWidth = 400
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    object edtLeftPath: TEdit
      Left = 12
      Height = 23
      Top = 0
      Width = 386
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      TabOrder = 0
      OnChange = edtLeftPathChange
      OnKeyDown = edtLeftPathKeyDown
    end
    object lblModifiedLeft: TLabel
      Left = 0
      Height = 37
      Hint = 'Modified'#13#10'Can be saved with Ctrl-S.'
      Top = -4
      Width = 12
      Caption = '*'
      Font.CharSet = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -27
      Font.Pitch = fpVariable
      Font.Quality = fqDraft
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Visible = False
    end
  end
  object pnlRight: TPanel
    Left = 410
    Height = 393
    Top = 0
    Width = 401
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'pnlRight'
    ClientHeight = 393
    ClientWidth = 401
    Color = 14286840
    Constraints.MinWidth = 400
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    object edtRightPath: TEdit
      Left = 12
      Height = 23
      Top = 0
      Width = 384
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      TabOrder = 0
      OnChange = edtRightPathChange
      OnKeyDown = edtRightPathKeyDown
    end
    object lblModifiedRight: TLabel
      Left = 0
      Height = 37
      Hint = 'Modified'#13#10'Can be saved with Ctrl-S.'
      Top = -4
      Width = 12
      Caption = '*'
      Font.CharSet = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -27
      Font.Pitch = fpVariable
      Font.Quality = fqDraft
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Visible = False
    end
  end
  object pnlHorizSplitter: TPanel
    Cursor = crHSplit
    Left = 400
    Height = 393
    Top = 0
    Width = 10
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'Splitter'
    Color = 13041606
    Font.Color = 13041606
    Font.Height = -11
    Font.Name = 'Tahoma'
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    OnMouseDown = pnlHorizSplitterMouseDown
    OnMouseMove = pnlHorizSplitterMouseMove
    OnMouseUp = pnlHorizSplitterMouseUp
  end
  object lblLineSeparator: TLabel
    Left = 0
    Height = 15
    Top = 400
    Width = 74
    Anchors = [akLeft, akBottom]
    Caption = 'Line separator'
  end
  object cmbLineBreak: TComboBox
    Left = 96
    Height = 23
    Top = 393
    Width = 100
    Anchors = [akLeft, akBottom]
    ItemHeight = 15
    ItemIndex = 0
    Items.Strings = (
      'CRLF'
      'LF'
    )
    Style = csDropDownList
    TabOrder = 3
    Text = 'CRLF'
  end
  object pnlCurrentSection: TPanel
    Left = 216
    Height = 26
    Top = 393
    Width = 594
    Alignment = taLeftJustify
    Anchors = [akLeft, akRight, akBottom]
    Caption = '0 0'
    Color = 11593983
    Font.Style = [fsBold]
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    TabOrder = 4
  end
  object tmrRefreshHighlighter: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrRefreshHighlighterTimer
    Left = 257
    Top = 82
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrStartupTimer
    Left = 257
    Top = 147
  end
  object tmrSetRightFromLeftPosition: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrSetRightFromLeftPositionTimer
    Left = 304
    Top = 232
  end
  object tmrSetLeftFromRightPosition: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrSetLeftFromRightPositionTimer
    Left = 496
    Top = 232
  end
  object FindDialog1: TFindDialog
    OnFind = FindDialog1Find
    Left = 304
    Top = 304
  end
  object pmSynEdits: TPopupMenu
    Left = 496
    Top = 82
    object MenuItem_ShowSpecialChars: TMenuItem
      AutoCheck = True
      Caption = 'Show special chars'
      Checked = True
      OnClick = MenuItem_ShowSpecialCharsClick
    end
  end
end
