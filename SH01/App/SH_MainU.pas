{!
<summary>
 SH_MainU
</summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2015-09-26<p>
 Version 2.0 created 2021-09-30<p>
 Last modified by PB 2021-11-16<p>
 Last modified by GS 2022-01-xx<p>
</history>
<copyright>Copyright 2021 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
 restrictions for use or redistribution. Just leave the copyright note
 above intact. The code carries no warranties whatsoever, use at your
 own risk!
</licence>
<remarks>
 This unit implements the main form of the SudokuHelper aqpplication.
 Its main features are a draw grid for the display of the Sudoku and
 a set of buttons on a panel to the right of the grid. The grid and the
 set of buttons - used to select values for mouse/touch input - are adapted
 to the requirements of a specific kind of Sudoku. This will also adjust
 the form size as needed.

 The program logic is completely handled by a set of classes the main form
 only accesses via as set of interfaces, and the form also implements an
 interface itself to allow the classes in question (especially the one
 handling mouse and keyboard input) to get some info on the state of the UI.
</remarks>
}
unit SH_MainU;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.ImageList,
  System.Types,
  System.Actions,
  Vcl.ActnList,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.ImgList,
  Vcl.StdActns,
  Vcl.StdCtrls,
  Forms,
  SH.Interfaces;

const
  UM_FOCUSGRID = WM_USER + 111;

type
  {! The application's main form, autocreated.
    The form "remembers" the  of Sudoku used in the last program run. }
  TFormMain = class(TForm, ISudokuHostform)
    UndoButton: TButton;
    MessageTimer: TTimer;
    SudokuPanel: TPanel;
    ButtonsPanel: TPanel;
    StatusBar: TStatusBar;
    SudokuGrid: TDrawGrid;
    ActionList: TActionList;
    UndoAction: TAction;
    ClearStackButton: TButton;
    ClearStackAction: TAction;
    StartNewButton: TButton;
    StartNewAction: TAction;
    MouseButtonsPanel: TPanel;
    ToggleGosuButton: TSpeedButton;
    SetCandidatesButton: TSpeedButton;
    UnsetCandidatesButton: TSpeedButton;
    ClearCellButton: TSpeedButton;
    ValueButtonsPanel: TPanel;
    ActionsLabel: TLabel;
    RevertToMarkButton: TButton;
    SetMarkButton: TButton;
    SetMarkAction: TAction;
    RevertToMarkAction: TAction;
    LoadSudokuAction: TFileOpen;
    SaveSudokuAction: TFileSaveAs;
    LoadSudokuButton: TButton;
    SaveSudokuButton: TButton;
    HelpAction: TAction;
    ShowMemoButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure ClearStackActionExecute(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure LoadSudokuActionAccept(Sender: TObject);
    procedure LoadSudokuActionBeforeExecute(Sender: TObject);
    procedure MessageTimerTimer(Sender: TObject);
    procedure RevertToMarkActionExecute(Sender: TObject);
    procedure RevertToMarkActionUpdate(Sender: TObject);
    procedure SaveSudokuActionAccept(Sender: TObject);
    procedure SaveSudokuActionBeforeExecute(Sender: TObject);
    procedure SetMarkActionExecute(Sender: TObject);
    procedure StartNewActionExecute(Sender: TObject);
    procedure SudokuGridClick(Sender: TObject);
    procedure SudokuGridContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure SudokuGridKeyPress(Sender: TObject; var Key: Char);
    procedure SudokuGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SudokuGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UndoActionExecute(Sender: TObject);
    procedure UndoActionUpdate(Sender: TObject);
    procedure ShowMemoButtonClick(Sender: TObject);
  private
    FLastMarkNum: Integer;
    FLastMouseButton: TMouseButton;
    FSudoku: ISudokuHelper;
    function GetCurrentCandidate: TSudokuValue;
    function GetCurrentValue: TSudokuValue;
    function GetDownValue(aParent: TWincontrol): TSudokuValue;
    procedure CreateSudokuHelper(const aName: string);
    procedure FocusGrid;
    procedure InitializeSudoku;
    procedure ShowHelpPrompt;
    function GetButtonsContainer: TWincontrol;
    function GetModifierkeys: TShiftstate;
    function GetRightClickAction: TRightClickAction;
    procedure UMFocusGrid(var M: TMessage); message UM_FOCUSGRID;
    procedure Display(const S: string; Timed: Boolean = false); overload;
    procedure Display(const Fmt: string; const A: array of const; Timed: Boolean = false); overload;
    property CurrentCandidate: TSudokuValue read GetCurrentCandidate;
    property CurrentValue: TSudokuValue read GetCurrentValue;
  protected
    procedure UpdateActions; override;
  public
    destructor Destroy; override;
    property Sudoku: ISudokuHelper read FSudoku;
  end;

var
  FormMain: TFormMain;

implementation

uses
  FrmMemo,
  System.Character,
  System.Generics.Collections,
  System.IOUtils,
  SH.SudokuHelper,
  SH.HelperBase,
  SH.SudokuFiler,
  SH.Memory,
  SH.Strings,
  SH_SelectSudokuDlgU,
  SH_SelectMarkDlgU;

{$R *.dfm}

const
  MessageTimeout = 30000; // 30 seconds

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ShowMemoButton.Margins.Top := 6;
  ShowMemoButton.AlignWithMargins := True;
  ShowMemoButton.Align := alTop;
  ShowMemoButton.Visible := True;

  MouseButtonsPanel.BevelOuter := TPanelBevel.bvNone;
  MouseButtonsPanel.Align := alBottom;

  ValueButtonsPanel.BevelOuter := TPanelBevel.bvNone;
  ValueButtonsPanel.Align := alClient;

  ClearCellButton.Align := alNone;
  ClearCellButton.Top := 200;

  SudokuPanel.Align := alClient;
  SudokuGrid.Align := alClient;
end;

destructor TFormMain.Destroy;
begin
  FSudoku := nil;
  inherited Destroy;
end;

{ This handler is used for all speedbuttons
  for which only the down state is relevant.
  The buttons are part of two button groups,
  which takes care of setting Down to false
    for all other buttons in the group. }
procedure TFormMain.SpeedButtonClick(Sender: TObject);
begin
  (Sender as TSpeedbutton).Down := true;
end;

procedure TFormMain.ClearStackActionExecute(Sender: TObject);
begin
  Sudoku.ClearUndostack;
  FocusGrid;
end;

procedure TFormMain.CreateSudokuHelper(const aName: string);
begin
  FSudoku := HelperRegistry.CreateInstance(aName);
  InitializeSudoku;
end;

procedure TFormMain.Display(const S: string; Timed: Boolean = false);
begin
  if Statusbar.SimplePanel then
    Statusbar.SimpleText := S
  else if Statusbar.Panels.Count > 0 then
    Statusbar.Panels[0].Text := S;
  if Timed then
  begin
    MessageTimer.Interval := MessageTimeout;
    MessageTimer.Enabled := true;
  end;
end;

procedure TFormMain.Display(const Fmt: string; const A: array of const; Timed:
  Boolean = false);
begin
  Display(Format(Fmt, A), Timed);
end;

procedure TFormMain.FocusGrid;
begin
//  SudokuGrid.SetFocus;
  PostMessage(Handle, UM_FOCUSGRID, 0, 0);
end;

procedure TFormMain.FormPaint(Sender: TObject);
begin
  { Note: FormPaint is called after FromShow }
  OnPaint := nil;

  { We need to delay the helper creation at launch until the form is
    completely displayed, to avoid a collision with startup state
    restoration done by the constructor. }

//  CreateSudokuHelper(AppMemory.LastSudoku);
  CreateSudokuHelper(CClassicSudoku9x9); // but I want the most basic, always

  { Display('Press F1 for a brief help overview'); }
  ShowHelpPrompt;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  Statusbar.Panels[0].Width :=
    Statusbar.ClientWidth -
    Statusbar.Panels[1].Width -
    Statusbar.Panels[2].Width -
    Statusbar.Height;
end;

{! Implements ISudokuHostform.GetButtonsContainer }
function TFormMain.GetButtonsContainer: TWinControl;
begin
  Result := ValueButtonsPanel;
end;

{! Implements ISudokuHostform.GetCurrentCandidate }
function TFormMain.GetCurrentCandidate: TSudokuValue;
begin
  Result := GetDownValue(ValueButtonsPanel);
end;

{! Implements ISudokuHostform.GetCurrentValue }
function TFormMain.GetCurrentValue: TSudokuValue;
begin
  Result := GetDownValue(ValueButtonsPanel);
end;

{! Get the Tag (encoding the value the button represents)
 of the speedbutton that is down. }
function TFormMain.GetDownValue(aParent: TWincontrol): TSudokuValue;
var
  Ctrl: TControl;
  I: Integer;
begin
  Result := 0;
  for I := 0 to aParent.ControlCount - 1 do
  begin
    Ctrl := aParent.Controls[I];
    if (Ctrl is TSpeedButton) and TSpeedButton(Ctrl).Down then
    begin
      Result := Ctrl.Tag;
      Break;
    end;
  end;
end;

{! Implements ISudokuHostform.GetModifierkeys }
function TFormMain.GetModifierkeys: TShiftstate;
begin
  Result := KeyboardStateToShiftState();
end;

{!
<summary>
 Implements ISudokuHostform.GetRightClickAction</summary>
<returns>
 The action to take on a right click or keyboard input</returns>
<remarks>
 To set a candidate the user can hold down the Shift key and just type
 the value, or right-click with the mouse. To clear a candidate he
 can use the Ctrl key instead. The right-click action is also controlled
 with a group of speedbuttons, but the modifier keys take precedence.
 </remarks>
}
function TFormMain.GetRightClickAction: TRightClickAction;
var
  LState: TShiftstate;
begin
  LState:= KeyboardStateToShiftState;
  if ssShift in LState then
    Result := TRightClickAction.SetCandidate
  else if ssCtrl in LState then
    Result := TRightClickAction.UnsetCandidate
  else if ToggleGosuButton.Enabled and ToggleGosuButton.Down then
    Result := TRightClickAction.ToggleGosu
  else if SetCandidatesButton.Down then
     Result := TRightClickAction.SetCandidate
  else if UnsetCandidatesButton.Down then
    Result := TRightClickAction.UnsetCandidate
  else  // default action is to set a candidate
    Result := TRightClickAction.SetCandidate
end;

procedure TFormMain.HelpActionExecute(Sender: TObject);
begin
  if FormMemo = nil then
  begin
    FormMemo := TFormMemo.Create(Self);
  end;
  FormMemo.HelpBtnClick(nil);
  FormMemo.Show;
end;

procedure TFormMain.InitializeSudoku;
begin
  Sudoku.Display.InitializeGrid(SudokuGrid);
  Sudoku.InputHandler.Initialize(self as ISudokuHostform);
  ToggleGosuButton.Enabled := Sudoku.IsGosu;
  AppMemory.LastSudoku := Sudoku.Displayname;
  Caption := Format(SMainformCaptionMask, [Sudoku.Displayname]);

  { Hack alert! Make sure the grid's OnClick handler can reliably distinguish
    a click fired by the left mouse button
      from one fired by cursor keys moving the selected cell.
    The startup value for FLastMouseButton is 0,
      which equals TMouseButton.mbLeft! }
  FLastMouseButton := TMouseButton.mbMiddle;
end;

procedure TFormMain.LoadSudokuActionAccept(Sender: TObject);
var
  LFilename: string;
  LSudoku: ISudokuHelper;
begin
  LFilename := LoadSudokuAction.Dialog.FileName;
  LSudoku := TSudokuFiler.LoadFromFile(LFilename);
  if Assigned(LSudoku) then
  begin
    FSudoku := LSudoku;
    InitializeSudoku;
    Sudoku.Display.Refresh;
    AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
  end;
  FocusGrid;
  ValueButtonsPanel.Invalidate; // needed to repaint the SpeedButtons
end;

procedure TFormMain.LoadSudokuActionBeforeExecute(Sender: TObject);
begin
  LoadSudokuAction.Dialog.InitialDir := AppMemory.LastFolder;
end;

procedure TFormMain.MessageTimerTimer(Sender: TObject);
begin
  MessageTimer.Enabled := false;
  ShowHelpPrompt;
end;

procedure TFormMain.RevertToMarkActionExecute(Sender: TObject);
var
  LMark: string;
begin
  if TSelectMarkDlg.Execute(LMark, Sudoku, RevertToMarkButton) then
    Sudoku.RevertToMark(LMark);
  FocusGrid;
end;

procedure TFormMain.RevertToMarkActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Sudoku.HasMarks;
end;

procedure TFormMain.SaveSudokuActionAccept(Sender: TObject);
var
  LFilename: string;
begin
  LFilename := SaveSudokuAction.Dialog.FileName;
  TSudokuFiler.SaveToFile(Sudoku, LFilename);
  AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
  FocusGrid;
  Display(SSaveFileMessageMask, [LFilename], true);
end;

procedure TFormMain.SaveSudokuActionBeforeExecute(Sender: TObject);
begin
  SaveSudokuAction.Dialog.InitialDir := AppMemory.LastFolder;
end;

procedure TFormMain.SetMarkActionExecute(Sender: TObject);
var
  LMark: string;
begin
  { Generate a proposed name }
  repeat
    Inc(FLastMarkNum);
    LMark := Format(SNewMarkMask, [FLastMarkNum]);
  until not Sudoku.MarkExists(LMark) ;

  if InputQuery(SNewStackMarkCaption, SNewStackMarkPrompt, LMark) then
  begin
    Sudoku.AddMark(LMark);
  end;
  FocusGrid;
end;

procedure TFormMain.ShowHelpPrompt;
begin
  Display(SHelpPrompt);
end;

procedure TFormMain.ShowMemoButtonClick(Sender: TObject);
begin
  if FormMemo = nil then
  begin
    FormMemo := TFormMemo.Create(Self);
  end;
  FormMemo.Show;
end;

procedure TFormMain.StartNewActionExecute(Sender: TObject);
var
  LSudokuName: string;
begin
  LSudokuName := CClassicSudoku9x9;
  if TSelectSudokuDlg.Execute(LSudokuName, StartNewButton) then
    CreateSudokuHelper(LSudokuName);
  FocusGrid;
  ValueButtonsPanel.Invalidate;
end;

procedure TFormMain.SudokuGridClick(Sender: TObject);
var
  aCol: Integer;
  aRow: Integer;
  Mousepos: TPoint;
begin
  if FLastMouseButton <> TMouseButton.mbLeft then
    Exit; // click event fired by cursor keys

  aCol := 0;
  aRow := 0;
  Mousepos := SudokuGrid.ScreenToClient(Mouse.CursorPos);
  SudokuGrid.MouseToCell(Mousepos.X, MousePos.Y, aCol, aRow);

  Sudoku.InputHandler.HandleCellClick(aCol, aRow);
  { Make sure the next click can identify a mouse click vs. keyboard "click". }
  FLastMouseButton := TMouseButton.mbMiddle;
end;

procedure TFormMain.SudokuGridContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  aCol: Integer;
  aRow: Integer;
begin
  Handled := True;
  Mousepos:= SudokuGrid.ScreenToClient(Mouse.CursorPos);
  aCol := 0;
  aRow := 0;
  SudokuGrid.MouseToCell(Mousepos.X, MousePos.Y, aCol, aRow);
  if (aCol >= 0) and (aRow >= 0) then
  begin
    SudokuGrid.Col := aCol;
    SudokuGrid.Row := aRow;
    Sudoku.InputHandler.HandleCellClick(aCol, aRow, true);
  end;
end;

procedure TFormMain.SudokuGridKeyPress(Sender: TObject; var Key: Char);
begin
  if (SudokuGrid.Col >= 0) and (SudokuGrid.Row >= 0) then
  begin
    Sudoku.InputHandler.HandleCharacter(SudokuGrid.Col, SudokuGrid.Row, Key);
    Key := #0;
  end;
end;

procedure TFormMain.SudokuGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  LChar: char;
begin
  { The purpose of this is to enable Shift-Number or Ctrl-Number
    to set or uset a candidate.

    This does NOT work with NumPad keys. ( I am not using the NumPad. )
    The original Alt key (instead of Shift key) version did work with NumPad,
      with NumPad in number mode.

    We are using Shift now instead of Alt to set candidates because Alt has problems:
    1. Alt + Number does not work in FMX project at all.
    2. Alt + Number (not NumPad) will issue unwanted warning sound.
    4. Alt or Shift alone can be used to toggle the right click mode (ok).
       But Alt alone will also activate the sytems menu,
       and if followed by an arrow key it will navigate the system menu
         instead of navigating the Sudoku-Grid.
       Pressing Alt (alone) twice would mitigate this but is not considered beautiful.

    ( There is a trade off between the Shift and Alt key 'solutions'. )
  }

  if (Shift * [ssCtrl, ssShift]) <> [] then
  begin
    { If Ctrl or Shift are down the OnKeyPress event will not fire!
      We have to figure out which character key was pressed ourself. }
    case Key of
      Ord('1')..Ord('9'): LChar := Char(Key);
      Ord('a')..Ord('g'): LChar := Char(Key);
    else
      LChar := #0;
    end;

    if LChar <> #0 then
    begin
      if (SudokuGrid.Col >= 0) and (SudokuGrid.Row >= 0) then
      begin
        Sudoku.InputHandler.HandleCharacter(SudokuGrid.Col, SudokuGrid.Row, LChar);
      end;
    end;
  end;
  FocusGrid;
end;

procedure TFormMain.SudokuGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseButton := Button;
end;

procedure TFormMain.UMFocusGrid(var M: TMessage);
begin
  M.Result := 1;
  SudokuGrid.SetFocus;
end;

procedure TFormMain.UndoActionExecute(Sender: TObject);
begin
  Sudoku.Undo;
  FocusGrid;
end;

procedure TFormMain.UndoActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Sudoku.CanUndo;
end;

procedure TFormMain.UpdateActions;
begin
  inherited;
  StatusBar.Panels[1].Text := Format(SLeftMask, [CurrentValue]);
  StatusBar.Panels[2].Text := Format(SRightMask, [CurrentCandidate]);
  if GetAsyncKeyState(VK_SHIFT) < 0 then
    SetCandidatesButton.Down := true
  else if GetAsyncKeyState(VK_CONTROL) < 0 then
    UnsetCandidatesButton.Down := true;
end;

end.
