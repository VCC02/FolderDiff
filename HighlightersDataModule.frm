object dmHighlighters: TdmHighlighters
  OldCreateOrder = False
  Height = 150
  HorizontalOffset = 86
  VerticalOffset = 85
  Width = 390
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
    Left = 48
    Top = 24
  end
  object SynIniSyn1: TSynIniSyn
    DefaultFilter = 'INI Files (*.ini)|*.ini'
    Enabled = False
    CommentAttri.Foreground = clGray
    SectionAttri.Background = 11593983
    NumberAttri.Foreground = clBlue
    Left = 48
    Top = 88
  end
  object SynLFMSyn1: TSynLFMSyn
    DefaultFilter = 'TyphonIDE Form Files (*.frm;*.frm)|*.frm;*.frm'
    Enabled = False
    Left = 136
    Top = 24
  end
  object SynXMLSyn1: TSynXMLSyn
    DefaultFilter = 'XML Document (*.xml,*.xsd,*.xsl,*.xslt,*.dtd)|*.xml;*.xsd;*.xsl;*.xslt;*.dtd'
    Enabled = False
    WantBracesParsed = False
    Left = 136
    Top = 88
  end
  object SynPythonSyn1: TSynPythonSyn
    DefaultFilter = 'Python Files (*.py)|*.py'
    Enabled = False
    Left = 216
    Top = 24
  end
end
