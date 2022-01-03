object FormMain: TFormMain
  Left = 0
  Top = 0
  ActiveControl = SudokuGrid
  Caption = 'Sudoku Helper'
  ClientHeight = 631
  ClientWidth = 784
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 600
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 23
  object StatusBar: TStatusBar
    Left = 0
    Top = 612
    Width = 784
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 100
      end
      item
        Width = 100
      end>
  end
  object SudokuPanel: TPanel
    Left = 40
    Top = 21
    Width = 473
    Height = 369
    Caption = 'SudokuPanel'
    Padding.Left = 6
    Padding.Top = 6
    Padding.Right = 6
    Padding.Bottom = 6
    ShowCaption = False
    TabOrder = 1
    object SudokuGrid: TDrawGrid
      Left = 95
      Top = 55
      Width = 298
      Height = 232
      ColCount = 9
      DefaultRowHeight = 64
      DoubleBuffered = True
      FixedCols = 0
      RowCount = 9
      FixedRows = 0
      Options = [goVertLine, goHorzLine, goTabs]
      ParentDoubleBuffered = False
      ScrollBars = ssNone
      TabOrder = 0
      StyleElements = [seClient, seBorder]
      OnClick = SudokuGridClick
      OnContextPopup = SudokuGridContextPopup
      OnKeyPress = SudokuGridKeyPress
      OnKeyUp = SudokuGridKeyUp
      OnMouseDown = SudokuGridMouseDown
      ColWidths = (
        64
        64
        64
        64
        64
        64
        64
        64
        64)
      RowHeights = (
        64
        64
        64
        64
        64
        64
        64
        64
        64)
    end
  end
  object ButtonsPanel: TPanel
    Left = 632
    Top = 0
    Width = 152
    Height = 612
    Align = alRight
    Caption = 'Panel1'
    Padding.Left = 6
    Padding.Top = 6
    Padding.Right = 6
    Padding.Bottom = 6
    ShowCaption = False
    TabOrder = 2
    object UndoButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 143
      Width = 138
      Height = 28
      Margins.Left = 0
      Margins.Top = 6
      Margins.Right = 0
      Margins.Bottom = 0
      Action = UndoAction
      Align = alTop
      DisabledImageIndex = 2
      HotImageIndex = 0
      ImageMargins.Left = 4
      TabOrder = 0
    end
    object ClearStackButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 109
      Width = 138
      Height = 28
      Margins.Left = 0
      Margins.Top = 6
      Margins.Right = 0
      Margins.Bottom = 0
      Action = ClearStackAction
      Align = alTop
      DisabledImageIndex = 2
      HotImageIndex = 0
      ImageMargins.Left = 4
      TabOrder = 1
    end
    object StartNewButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 7
      Width = 138
      Height = 28
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Action = StartNewAction
      Align = alTop
      DisabledImageIndex = 2
      HotImageIndex = 0
      ImageMargins.Left = 4
      TabOrder = 2
    end
    object MouseButtonsPanel: TPanel
      Left = 7
      Top = 498
      Width = 138
      Height = 107
      Align = alBottom
      AutoSize = True
      Caption = 'MouseButtonsPanel'
      ParentShowHint = False
      ShowCaption = False
      ShowHint = False
      TabOrder = 3
      object ToggleGosuButton: TSpeedButton
        Left = 1
        Top = 22
        Width = 136
        Height = 28
        Align = alTop
        AllowAllUp = True
        GroupIndex = 1
        Caption = '&Toggle Gosu'
        OnClick = SpeedButtonClick
        ExplicitLeft = 0
        ExplicitTop = 3
        ExplicitWidth = 121
      end
      object SetCandidatesButton: TSpeedButton
        Left = 1
        Top = 50
        Width = 136
        Height = 28
        Align = alTop
        AllowAllUp = True
        GroupIndex = 1
        Caption = '&Set candidate'
        Spacing = 0
        OnClick = SpeedButtonClick
        ExplicitLeft = -1
        ExplicitTop = 44
      end
      object UnsetCandidatesButton: TSpeedButton
        Left = 1
        Top = 78
        Width = 136
        Height = 28
        Align = alTop
        AllowAllUp = True
        GroupIndex = 1
        Caption = 'Clea&r candidate'
        Spacing = 0
        OnClick = SpeedButtonClick
        ExplicitLeft = 0
        ExplicitTop = 58
        ExplicitWidth = 121
      end
      object ActionsLabel: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 130
        Height = 17
        Margins.Bottom = 1
        Align = alTop
        Caption = 'Right-click actions:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 107
      end
    end
    object ValueButtonsPanel: TPanel
      AlignWithMargins = True
      Left = 16
      Top = 392
      Width = 117
      Height = 65
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 6
      Caption = 'ValueButtonsPanel'
      ShowCaption = False
      TabOrder = 4
      object ClearCellButton: TSpeedButton
        Left = 1
        Top = 36
        Width = 115
        Height = 28
        Align = alBottom
        GroupIndex = 2
        Caption = '0 (Clear cell)'
        Spacing = 0
        OnClick = SpeedButtonClick
        ExplicitLeft = -1
        ExplicitTop = 323
        ExplicitWidth = 121
      end
    end
    object RevertToMarkButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 211
      Width = 138
      Height = 28
      Margins.Left = 0
      Margins.Top = 6
      Margins.Right = 0
      Margins.Bottom = 0
      Action = RevertToMarkAction
      Align = alTop
      DisabledImageIndex = 2
      HotImageIndex = 0
      ImageMargins.Left = 4
      TabOrder = 5
    end
    object SetMarkButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 177
      Width = 138
      Height = 28
      Margins.Left = 0
      Margins.Top = 6
      Margins.Right = 0
      Margins.Bottom = 0
      Action = SetMarkAction
      Align = alTop
      DisabledImageIndex = 2
      HotImageIndex = 0
      ImageMargins.Left = 4
      TabOrder = 6
    end
    object LoadSudokuButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 75
      Width = 138
      Height = 28
      Margins.Left = 0
      Margins.Top = 6
      Margins.Right = 0
      Margins.Bottom = 0
      Action = LoadSudokuAction
      Align = alTop
      DisabledImageIndex = 3
      HotImageIndex = 2
      ImageMargins.Left = 4
      ImageMargins.Right = 4
      PressedImageIndex = 3
      SelectedImageIndex = 3
      TabOrder = 8
    end
    object SaveSudokuButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 41
      Width = 138
      Height = 28
      Margins.Left = 0
      Margins.Top = 6
      Margins.Right = 0
      Margins.Bottom = 0
      Action = SaveSudokuAction
      Align = alTop
      DisabledImageIndex = 4
      HotImageIndex = 4
      ImageMargins.Left = 4
      ImageMargins.Right = 4
      PressedImageIndex = 4
      SelectedImageIndex = 4
      TabOrder = 7
    end
    object ShowMemoButton: TButton
      AlignWithMargins = True
      Left = 7
      Top = 245
      Width = 138
      Height = 25
      Margins.Left = 0
      Margins.Top = 6
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Show Memo'
      TabOrder = 9
      OnClick = ShowMemoButtonClick
    end
  end
  object MessageTimer: TTimer
    Enabled = False
    OnTimer = MessageTimerTimer
    Left = 40
    Top = 440
  end
  object ActionList: TActionList
    Left = 164
    Top = 436
    object UndoAction: TAction
      Caption = '&Undo'
      ImageIndex = 1
      SecondaryShortCuts.Strings = (
        'Alt+BkSp')
      ShortCut = 16474
      OnExecute = UndoActionExecute
      OnUpdate = UndoActionUpdate
    end
    object ClearStackAction: TAction
      Caption = 'Clear stac&k'
      OnExecute = ClearStackActionExecute
      OnUpdate = UndoActionUpdate
    end
    object StartNewAction: TAction
      Caption = '&New Sudoku'
      OnExecute = StartNewActionExecute
    end
    object SetMarkAction: TAction
      Caption = 'Set &Mark'
      Hint = 'Define a mark for the current undo stack state'
      OnExecute = SetMarkActionExecute
    end
    object RevertToMarkAction: TAction
      Caption = 'Re&vert to Mark'
      Hint = 'Unroll the undo stack to a saved mark'
      OnExecute = RevertToMarkActionExecute
      OnUpdate = RevertToMarkActionUpdate
    end
    object LoadSudokuAction: TFileOpen
      Category = 'File'
      Caption = '&Load Sudoku...'
      Dialog.DefaultExt = 'sudoku'
      Dialog.Filter = 'Sudoku|*.sudoku'
      Dialog.Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
      Dialog.Title = 'Load Sudoku'
      Hint = 'Load|Load a saved Sudoku'
      ImageIndex = 3
      ShortCut = 16463
      BeforeExecute = LoadSudokuActionBeforeExecute
      OnAccept = LoadSudokuActionAccept
    end
    object SaveSudokuAction: TFileSaveAs
      Category = 'File'
      Caption = 'Save Sud&oku...'
      Dialog.DefaultExt = 'sudoku'
      Dialog.Filter = 'Sudoku|*.sudoku'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofNoDereferenceLinks, ofEnableSizing]
      Dialog.Title = 'Save Sudoku'
      Hint = 'Save|Saves the Sudoku to file'
      ImageIndex = 4
      ShortCut = 16467
      BeforeExecute = SaveSudokuActionBeforeExecute
      OnAccept = SaveSudokuActionAccept
    end
    object HelpAction: TAction
      Category = 'Help'
      Caption = '&Help...'
      ShortCut = 112
      OnExecute = HelpActionExecute
    end
  end
end
