object frmFolderDiffMain: TfrmFolderDiffMain
  Left = 373
  Height = 345
  Top = 185
  Width = 982
  Caption = 'Folder Diff'
  ClientHeight = 345
  ClientWidth = 982
  Constraints.MinHeight = 345
  Constraints.MinWidth = 982
  LCLVersion = '8.4'
  OnClose = FormClose
  OnCreate = FormCreate
  object PageControlMain: TPageControl
    Left = 0
    Height = 336
    Top = 8
    Width = 982
    ActivePage = TabSheetFolderDiff
    Anchors = [akTop, akLeft, akRight, akBottom]
    Images = imglstVST
    PopupMenu = pmTabs
    TabIndex = 0
    TabOrder = 0
    OnChange = PageControlMainChange
    object TabSheetFolderDiff: TTabSheet
      Caption = 'Folder Diff'
      ClientHeight = 308
      ClientWidth = 974
      ImageIndex = 0
      OnResize = TabSheetFolderDiffResize
      object edtSearch: TEdit
        Left = 0
        Height = 23
        Top = 256
        Width = 480
        Anchors = [akLeft, akBottom]
        Color = 11795967
        TabOrder = 5
        TextHint = 'Search'
        OnChange = edtSearchChange
      end
      object pnlLeft: TPanel
        Left = 0
        Height = 32
        Top = 48
        Width = 480
        Caption = 'pnlLeft'
        ClientHeight = 32
        ClientWidth = 480
        TabOrder = 0
        object edtLeft: TEdit
          Left = 0
          Height = 23
          Top = 5
          Width = 448
          Anchors = [akTop, akLeft, akRight]
          PopupMenu = pmRecent
          TabOrder = 0
          OnKeyDown = edtLeftKeyDown
        end
        object spdbtnLeft: TSpeedButton
          Left = 452
          Height = 23
          Top = 5
          Width = 26
          Anchors = [akTop, akRight]
          Caption = '...'
          PopupMenu = pmRecent
          OnClick = spdbtnLeftClick
        end
      end
      object pnlRight: TPanel
        Left = 488
        Height = 32
        Top = 48
        Width = 480
        Caption = 'pnlRight'
        ClientHeight = 32
        ClientWidth = 480
        TabOrder = 1
        object edtRight: TEdit
          Left = 0
          Height = 23
          Top = 5
          Width = 448
          Anchors = [akTop, akLeft, akRight]
          PopupMenu = pmRecent
          TabOrder = 0
          OnKeyDown = edtRightKeyDown
        end
        object spdbtnRight: TSpeedButton
          Left = 452
          Height = 23
          Top = 5
          Width = 26
          Anchors = [akTop, akRight]
          Caption = '...'
          PopupMenu = pmRecent
          OnClick = spdbtnRightClick
        end
      end
      object vstDiff: TVirtualStringTree
        Left = 0
        Height = 199
        Top = 80
        Width = 976
        Anchors = [akTop, akLeft, akRight, akBottom]
        Colors.FocusedSelectionColor = 10419935
        Colors.SelectionTextColor = clGreen
        Colors.UnfocusedSelectionColor = 15859426
        DefaultText = 'Node'
        Header.AutoSizeIndex = 0
        Header.Columns = <        
          item
            MinWidth = 200
            Position = 0
            Text = 'Name'
            Width = 200
          end        
          item
            MinWidth = 100
            Position = 1
            Text = 'Size[B]'
            Width = 100
          end        
          item
            MinWidth = 150
            Position = 2
            Text = 'Timestamp'
            Width = 150
          end        
          item
            MinWidth = 40
            Position = 3
            Text = 'Diff'
            Width = 40
          end        
          item
            MinWidth = 200
            Position = 4
            Text = 'Name'
            Width = 200
          end        
          item
            MinWidth = 100
            Position = 5
            Text = 'Size[B]'
            Width = 100
          end        
          item
            MinWidth = 150
            Position = 6
            Text = 'Timestamp'
            Width = 150
          end>
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Header.Style = hsFlatButtons
        Indent = 8
        StateImages = imglstVST
        TabOrder = 2
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
        OnAfterCellPaint = vstDiffAfterCellPaint
        OnBeforeCellPaint = vstDiffBeforeCellPaint
        OnClick = vstDiffClick
        OnDblClick = vstDiffDblClick
        OnDrawText = vstDiffDrawText
        OnExpanded = vstDiffExpanded
        OnGetText = vstDiffGetText
        OnPaintText = vstDiffPaintText
        OnGetImageIndex = vstDiffGetImageIndex
        OnKeyUp = vstDiffKeyUp
        OnMouseDown = vstDiffMouseDown
      end
      object lblFilter: TLabel
        Left = 10
        Height = 15
        Top = 11
        Width = 38
        Caption = 'Display'
      end
      object cmbFilter: TComboBox
        Left = 64
        Height = 23
        Top = 8
        Width = 100
        ItemHeight = 15
        ItemIndex = 0
        Items.Strings = (
          'All files'
          'Differences'
        )
        Style = csDropDownList
        TabOrder = 3
        Text = 'All files'
        OnChange = cmbFilterChange
      end
      object spdbtnSyncToRight: TSpeedButton
        Left = 416
        Height = 22
        Hint = 'Copy/Updated files from left to right'
        Top = 0
        Width = 56
        Caption = 'Sync'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6C98FFCCDBFFFF
          FFFFF8FAFFFAFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF0048FF6F97FFE9EFFFFFFFFFFFFFFFFBFCFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1C60FF044FFF63
          91FFFFFFFFFFFFFFF8FAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF1F62FF1266FF1A6BFF5F8CFFD5DFFFFFFFFFF8FAFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1C60FF3282FF17
          76FF0557FF739BFFE7ECFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF1C60FF2373FF2D88FF277FFF0051FF6B95FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1C60FF2273FF2C
          87FF2A83FF1875FF2874FF467EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF1C60FF2273FF2C87FF2A83FF1875FF2874FF467EFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1C60FF2373FF2D
          88FF277FFF0051FF6B95FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF1C60FF3282FF1776FF0557FF739BFFE7ECFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F62FF1266FF1A
          6BFF5F8CFFD5DFFFFFFFFFF8FAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF1C60FF044FFF6391FFFFFFFFFFFFFFF8FAFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0048FF6F97FFE9
          EFFFFFFFFFFFFFFFFBFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF6C98FFCCDBFFFFFFFFF8FAFFFAFCFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        }
        ShowHint = True
        ParentShowHint = False
        OnClick = spdbtnSyncToRightClick
      end
      object spdbtnDiff: TSpeedButton
        Left = 480
        Height = 22
        Hint = 'Compare selected files'
        Top = 0
        Width = 23
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF241CED
          241CED241CEDFFFFFF241CEDFFFFFFFFFFFFFFFFFF241CEDFFFFFF241CEDFFFF
          FFFFFFFFFFFFFFFFFFFFC9AEFF241CEDFFFFFFFFFFFFC9AEFF241CEDFFFFFFFF
          FFFFFFFFFF241CEDFFFFFF241CEDFFFFFFFFFFFFFFFFFFFFFFFF241CEDFFFFFF
          FFFFFFFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFF241CEDFFFFFF241CEDFFFF
          FFFFFFFFFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFFFFFFFF241CEDFFFFFFFF
          FFFFFFFFFF241CEDFFFFFF241CED241CED241CED241CEDC9AEFF241CEDFFFFFF
          FFFFFFFFFFFFFFFFFF241CEDFFFFFFFFFFFFFFFFFF241CEDFFFFFF241CEDFFFF
          FFFFFFFFC9AEFF241CED241CEDFFFFFFFFFFFFFFFFFFFFFFFF241CEDC9AEFF24
          1CEDC9AEFF241CEDFFFFFF241CEDFFFFFFFFFFFFC9AEFF241CEDC9AEFF241CED
          FFFFFFFFFFFFC9AEFF241CED241CEDFFFFFF241CED241CEDFFFFFF241CEDFFFF
          FFFFFFFFC9AEFF241CEDFFFFFF241CED241CED241CEDFFFFFF241CEDFFFFFFFF
          FFFFFFFFFF241CEDFFFFFF241CED241CED241CED241CEDC9AEFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        }
        ShowHint = True
        ParentShowHint = False
        OnClick = spdbtnDiffClick
      end
      object spdbtnSwitchLeftAndRight: TSpeedButton
        Left = 480
        Height = 22
        Hint = 'Switch left and right views'
        Top = 24
        Width = 23
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4CB122FF4CB122FF4CB122FF4CB1
          22FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF4CB122FF4CB122FF1DE6B5FF1DE6B5FF1DE6B5FF1DE6
          B5FF4CB122FF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF4CB122FF1DE6B5FF1DE6B5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF1DE6B5FF1DE6B5FF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFF7F7F7FFF7F7F
          7FFF7F7F7FFF7F7F7FFF7F7F7FFFFFFFFFFFC9AEFFFF1E00C4FF1E00C4FFFFFF
          FFFF7F7F7FFF1DE6B5FF4CB122FF7F7F7FFF7F7F7FFF7F7F7FFF150088FF1500
          88FF150088FF150088FF7F7F7FFFFFFFFFFFC9AEFFFF150088FF1E00C4FFFFFF
          FFFF7F7F7FFF150088FF1DE6B5FF4CB122FF150088FF150088FF150088FF1500
          88FF1DE6B5FF150088FF7F7F7FFFFFFFFFFFC9AEFFFF150088FF1E00C4FFFFFF
          FFFF7F7F7FFF150088FF1DE6B5FF4CB122FF150088FF150088FF150088FF1DE6
          B5FF4CB122FF1DE6B5FF7F7F7FFFFFFFFFFFC9AEFFFF150088FF1E00C4FFFFFF
          FFFF7F7F7FFF1DE6B5FF4CB122FF4CB122FF4CB122FF1DE6B5FF1DE6B5FF4CB1
          22FF4CB122FF4CB122FF1DE6B5FFFFFFFFFFC9AEFFFF150088FF1E00C4FFFFFF
          FFFF7F7F7FFF150088FF1DE6B5FF4CB122FF1DE6B5FF150088FF150088FF1DE6
          B5FF4CB122FF150088FF7F7F7FFFFFFFFFFFC9AEFFFF150088FF1E00C4FFFFFF
          FFFF7F7F7FFF150088FF150088FF1DE6B5FF150088FF150088FF150088FF1DE6
          B5FF4CB122FF150088FF7F7F7FFFFFFFFFFFC9AEFFFF150088FF1E00C4FFFFFF
          FFFF7F7F7FFF150088FF150088FF150088FF150088FF150088FF7F7F7FFF7F7F
          7FFF1DE6B5FF4CB122FF7F7F7FFFFFFFFFFFC9AEFFFFC9AEFFFFC9AEFFFFFFFF
          FFFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFFFFFFFFFFFFFF
          FFFF1DE6B5FF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF1DE6B5FF1DE6B5FF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF1DE6B5FF4CB122FF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFF1DE6
          B5FF4CB122FF4CB122FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF1DE6B5FF1DE6B5FF4CB122FF4CB122FF4CB122FF4CB1
          22FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6B5FF1DE6B5FF1DE6B5FF1DE6
          B5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        }
        ShowHint = True
        ParentShowHint = False
        OnClick = spdbtnSwitchLeftAndRightClick
      end
      object spdbtnSyncToLeft: TSpeedButton
        Left = 512
        Height = 22
        Hint = 'Copy/Updated files from right to left'
        Top = 0
        Width = 56
        Caption = 'Sync'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFCFFF8
          FAFFFFFFFFCCDBFF6C98FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFBFCFFFFFFFFFFFFFFE9EFFF6F97FF0048FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8FAFFFFFFFFFF
          FFFF6391FF044FFF1C60FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFF8FAFFFFFFFFD5DFFF5F8CFF1A6BFF1266FF1F62FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7ECFF739BFF05
          57FF1776FF3282FF1C60FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFF6B95FF0051FF277FFF2D88FF2373FF1C60FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF467EFF2874FF1875FF2A
          83FF2C87FF2273FF1C60FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF467EFF2874FF1875FF2A83FF2C87FF2273FF1C60FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6B95FF0051FF27
          7FFF2D88FF2373FF1C60FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFE7ECFF739BFF0557FF1776FF3282FF1C60FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8FAFFFFFFFFD5DFFF5F
          8CFF1A6BFF1266FF1F62FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFF8FAFFFFFFFFFFFFFF6391FF044FFF1C60FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFCFFFFFFFFFF
          FFFFE9EFFF6F97FF0048FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFAFCFFF8FAFFFFFFFFCCDBFF6C98FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        }
        ShowHint = True
        ParentShowHint = False
        OnClick = spdbtnSyncToLeftClick
      end
      object spdbtnStopLoading: TSpeedButton
        Left = 608
        Height = 22
        Top = 0
        Width = 88
        Caption = 'Stop loading'
        Visible = False
        OnClick = spdbtnStopLoadingClick
      end
      object spdbtnStopSyncing: TSpeedButton
        Left = 712
        Height = 22
        Top = 0
        Width = 87
        Caption = 'Stop syncing'
        Visible = False
        OnClick = spdbtnStopSyncingClick
      end
      object StatusBar1: TStatusBar
        Left = 0
        Height = 23
        Top = 285
        Width = 974
        Panels = <        
          item
            Text = 'LeftFile'
            Width = 480
          end        
          item
            Text = 'RightFile'
            Width = 480
          end>
        SimplePanel = False
      end
      object chkSearchFilename: TCheckBox
        Left = 184
        Height = 19
        Top = 8
        Width = 90
        Caption = 'Search for file'
        TabOrder = 6
        OnChange = chkSearchFilenameChange
      end
      object chkFullFolderLoading: TCheckBox
        Left = 184
        Height = 19
        Top = 27
        Width = 114
        Caption = 'Full folder loading'
        Checked = True
        State = cbChecked
        TabOrder = 7
        Visible = False
      end
    end
  end
  object SelectDirectoryDialog1: TSelectDirectoryDialog
    Left = 288
    Top = 168
  end
  object imglstVST: TImageList
    Left = 432
    Top = 176
    Bitmap = {
      4C7A030000001000000010000000C90000000000000078DAFBFFFF3FC3FF4180
      1996DDFD8F8C49D6FBE93F029368064C3FDF49084637071BC6A71F19A3B80B87
      FBF0E9C786D5EBFF8FEAA7A2FE93EB207C9038311857DAC2975EC84DDB4305FB
      6C54FA8F0B13AB7FC393F758313166C0F4A3C7154C3F213308D94FC81DC4E8C7
      670631EEC717A6F8EC27263CA9A59F50DEA1B5FD43D9FDA4E0E158860C145691
      79FB1F86617C74797475200CAA374018593D8C8FAE07DD5C7475E8F6C1E491CD
      27453F2EF753AA1FDDFFE4DA8FEE0E5C7C42FA498DBFD1F48E8A01EF86A5EE
    }
  end
  object pmTabs: TPopupMenu
    Left = 368
    Top = 40
    object MenuItem_CloseActiveTab: TMenuItem
      Caption = 'Close active tab'
      OnClick = MenuItem_CloseActiveTabClick
    end
    object MenuItem_OpenInEditMode: TMenuItem
      Caption = 'Open in edit mode'
      OnClick = MenuItem_OpenInEditModeClick
    end
    object MenuItem_ShowInExplorer: TMenuItem
      Caption = 'Show in explorer'
      OnClick = MenuItem_ShowInExplorerClick
    end
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 520
    Top = 176
  end
  object tmrSearch: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrSearchTimer
    Left = 600
    Top = 176
  end
  object AsyncProcess1: TAsyncProcess
    Active = False
    Options = []
    Priority = ppNormal
    StartupOptions = []
    ShowWindow = swoNone
    WindowColumns = 0
    WindowHeight = 0
    WindowLeft = 0
    WindowRows = 0
    WindowTop = 0
    WindowWidth = 0
    FillAttribute = 0
    InputDescriptor.IOType = iotDefault
    InputDescriptor.FileWriteMode = fwmTruncate
    OutputDescriptor.IOType = iotDefault
    OutputDescriptor.FileWriteMode = fwmTruncate
    ErrorDescriptor.IOType = iotDefault
    ErrorDescriptor.FileWriteMode = fwmTruncate
    Left = 432
    Top = 233
  end
  object pmRecent: TPopupMenu
    Left = 80
    Top = 94
    object MenuItem_AddToRecent: TMenuItem
      Caption = 'Add to recent'
      OnClick = MenuItem_AddToRecentClick
    end
    object MenuItem_RemoveCurrentPathsFromRecent: TMenuItem
      Caption = 'Remove current paths from recent'
      OnClick = MenuItem_RemoveCurrentPathsFromRecentClick
    end
    object Separator1: TMenuItem
      Caption = '-'
    end
    object MenuItem_Recent: TMenuItem
      Caption = 'Recent'
    end
  end
end
