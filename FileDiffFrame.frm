object frFileDiff: TfrFileDiff
  Left = 0
  Height = 528
  Top = 0
  Width = 891
  ClientHeight = 528
  ClientWidth = 891
  TabOrder = 0
  OnResize = FrameResize
  DesignLeft = 86
  DesignTop = 85
  object pnlLeft: TPanel
    Left = 77
    Height = 526
    Top = 0
    Width = 400
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'pnlLeft'
    ClientHeight = 526
    ClientWidth = 400
    Color = 14286811
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    object edtLeftPath: TEdit
      Left = 8
      Height = 23
      Top = 0
      Width = 386
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      TabOrder = 0
    end
  end
  object pnlRight: TPanel
    Left = 485
    Height = 526
    Top = 0
    Width = 405
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'pnlRight'
    ClientHeight = 526
    ClientWidth = 405
    Color = 14286811
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    object edtRightPath: TEdit
      Left = 8
      Height = 23
      Top = 0
      Width = 388
      Anchors = [akTop, akLeft, akRight]
      Color = clBtnFace
      TabOrder = 0
    end
  end
  object pnlMiniMap: TPanel
    Left = 0
    Height = 477
    Top = 27
    Width = 78
    Anchors = [akTop, akLeft, akBottom]
    FullRepaint = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnMouseDown = pnlMiniMapMouseDown
    OnMouseMove = pnlMiniMapMouseMove
  end
  object spdbtnJumpToPrevSection: TSpeedButton
    Left = 8
    Height = 18
    Hint = 'Jump to previous section'
    Top = 3
    Width = 23
    Enabled = False
    Flat = True
    Font.CharSet = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Webdings'
    Font.Style = [fsBold]
    Glyph.Data = {
      56010000424D560100000000000036000000280000000F000000060000000100
      1800000000002001000000000000000000000000000000000000FFFFFF4CB122
      4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
      224CB122FFFFFF000000FFFFFFFFFFFF4CB1224CB1224CB1224CB1224CB1224C
      B1224CB1224CB1224CB1224CB1224CB122FFFFFFFFFFFF000000FFFFFFFFFFFF
      FFFFFF4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB122FFFF
      FFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF4CB1224CB1224CB1224C
      B1224CB1224CB1224CB122FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF4CB1224CB1224CB1224CB1224CB122FFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4CB1224C
      B1224CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
    }
    ShowHint = True
    ParentFont = False
    ParentShowHint = False
  end
  object spdbtnJumpToNextSection: TSpeedButton
    Left = 37
    Height = 18
    Hint = 'Jump to next section'
    Top = 3
    Width = 23
    Enabled = False
    Flat = True
    Font.CharSet = SYMBOL_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Webdings'
    Font.Style = [fsBold]
    Glyph.Data = {
      56010000424D560100000000000036000000280000000F000000060000000100
      1800000000002001000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF4CB1224CB1224CB122FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4CB1224CB1224C
      B1224CB1224CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
      FFFFFFFFFFFF4CB1224CB1224CB1224CB1224CB1224CB1224CB122FFFFFFFFFF
      FFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF4CB1224CB1224CB1224CB1224C
      B1224CB1224CB1224CB1224CB122FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF
      4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
      22FFFFFFFFFFFF000000FFFFFF4CB1224CB1224CB1224CB1224CB1224CB1224C
      B1224CB1224CB1224CB1224CB1224CB1224CB122FFFFFF000000
    }
    ShowHint = True
    ParentFont = False
    ParentShowHint = False
  end
  object lblInfo: TLabel
    Left = 0
    Height = 15
    Top = 511
    Width = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Info'
    ParentShowHint = False
    ShowHint = True
  end
  object tmrRepaintMinimapOnSelect: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrRepaintMinimapOnSelectTimer
    Left = 476
    Top = 244
  end
  object SynPasSyn1: TSynPasSyn
    Enabled = False
    CommentAttri.Foreground = clGray
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clGreen
    DirectiveAttri.Foreground = clTeal
    CompilerMode = pcmDelphi
    NestedComments = False
    TypeHelpers = True
    StringMultilineMode = []
    Left = 243
    Top = 81
  end
  object SynIniSyn1: TSynIniSyn
    DefaultFilter = 'INI Files (*.ini)|*.ini'
    Enabled = False
    Left = 304
    Top = 80
  end
  object SynLFMSyn1: TSynLFMSyn
    DefaultFilter = 'TyphonIDE Form Files (*.frm;*.frm)|*.frm;*.frm'
    Enabled = False
    Left = 376
    Top = 80
  end
  object SynXMLSyn1: TSynXMLSyn
    DefaultFilter = 'XML Document (*.xml,*.xsd,*.xsl,*.xslt,*.dtd)|*.xml;*.xsd;*.xsl;*.xslt;*.dtd'
    Enabled = False
    WantBracesParsed = False
    Left = 448
    Top = 81
  end
  object pmHighlighter: TPopupMenu
    Left = 114
    Top = 84
    object MenuItem_OpenInTextEditor: TMenuItem
      Caption = 'Open in text editor'
      OnClick = MenuItem_OpenInTextEditorClick
    end
  end
  object tmrSetRightFromLeftPosition: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrSetRightFromLeftPositionTimer
    Left = 376
    Top = 336
  end
  object tmrSetLeftFromRightPosition: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrSetLeftFromRightPositionTimer
    Left = 568
    Top = 336
  end
  object FindDialog1: TFindDialog
    OnFind = FindDialog1Find
    Left = 232
    Top = 336
  end
end
