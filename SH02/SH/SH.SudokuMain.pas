unit SH.SudokuMain;

interface

uses
  System.Classes,
  System.ImageList,
  System.SysUtils,
  System.Types,
  FMX.Types,
  FMX.Layouts,
  FMX.Controls,
  FMX.StdCtrls,
  SH.Interfaces,
  SH.SudokuGrid,
  RiggVar.App.Main0;

type
  TSudokuMain = class
  strict private
    FLastMarkNum: Integer;
    FSudoku: ISudokuHelper;
    function GetCurrentCandidate: TSudokuValue;
    function GetCurrentValue: TSudokuValue;
    function GetDownValue(aParent: TLayout): TSudokuValue;
    procedure CreateSudokuHelper(const aName: string);
    procedure FocusGrid;
    procedure InitializeSudoku;
    procedure RunTest;
    procedure ShowHelpPrompt;
  strict protected
    function GetButtonsContainer: TLayout;
    function GetModifierkeys: TShiftstate;
    function GetRightClickAction: TRightClickAction;
  protected
    procedure UpdateActions;
    property CurrentCandidate: TSudokuValue read GetCurrentCandidate;
    property CurrentValue: TSudokuValue read GetCurrentValue;
    property Sudoku: ISudokuHelper read FSudoku;
  public
    Caption: string;
    SudokuGrid: TSudokuGrid;

    ValueButtonsPanel: TLayout;
    ToggleGosuButton: TButton;
    SetCandidatesButton: TButton;
    UnsetCandidatesButton: TButton;

    constructor Create;
    destructor Destroy; override;

    procedure DoOnceAfterStartup;

    procedure SpeedButtonClick(Sender: TObject);
    procedure ClearStackActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
    procedure LoadSudokuActionAccept(fn: string); //(Sender: TObject);
    procedure LoadSudokuActionBeforeExecute(Sender: TObject);
    procedure RevertToMarkActionExecute(MarkName: string);
    procedure RevertToMarkActionUpdate(Sender: TObject);
    procedure SaveSudokuActionAccept(fn: string);
    procedure SaveSudokuActionBeforeExecute(Sender: TObject);
    procedure SetMarkActionExecute(Sender: TObject);
    procedure StartNewActionExecute(Sender: TObject);
    procedure SudokuGridClick(Sender: TObject);
    procedure SudokuGridContextPopup(Sender: TObject; MousePos: TPointF; var Handled: Boolean);
    procedure SudokuGridKeyPress(Sender: TObject; var Key: Char);
    procedure SudokuGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SudokuGridMouseDown(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TestButtonClick(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure UndoActionUpdate(Sender: TObject);

    procedure Display(const S: string; Timed: Boolean = false); overload;
    procedure Display(const Fmt: string; const A: array of const; Timed: Boolean = false); overload;
  end;

implementation

uses
  FrmMain,
  System.Character,
  System.Generics.Collections,
  System.IOUtils,
  SH.HelperBase,
  SH.SudokuHelper,
  SH.SudokuFiler,
  SH.Memory,
  SH.Strings;

constructor TSudokuMain.Create;
begin

  inherited;
//  AppMemory.RestoreFormState(self);
end;

destructor TSudokuMain.Destroy;
begin
  FSudoku := nil;
  inherited Destroy;
end;

{ This handler is used for all Buttons
  for which only the down state is relevant.
  The buttons are part of two button groups,
  which takes care of setting Down to false
    for all other buttons in the group. }
procedure TSudokuMain.SpeedButtonClick(Sender: TObject);
begin
  // (Sender as TButton).Down := true;
end;

procedure TSudokuMain.ClearStackActionExecute(Sender: TObject);
begin
  Sudoku.ClearUndostack;
  FocusGrid;
end;

procedure TSudokuMain.CreateSudokuHelper(const aName: string);
begin
  FSudoku := HelperRegistry.CreateInstance(aName);
  InitializeSudoku;
end;

procedure TSudokuMain.Display(const S: string; Timed: Boolean = false);
begin
//  if Statusbar.SimplePanel then
//    Statusbar.SimpleText := S
//  else if Statusbar.Panels.Count > 0 then
//    Statusbar.Panels[0].Text := S;
//  if Timed then
//  begin
//    MessageTimer.Interval := MessageTimeout;
//    MessageTimer.Enabled := true;
//  end;
end;

procedure TSudokuMain.Display(const Fmt: string; const A: array of const; Timed: Boolean = false);
begin
  Display(Format(Fmt, A), Timed);
end;

procedure TSudokuMain.FocusGrid;
begin

end;

procedure TSudokuMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//  AppMemory.SaveFormState(self);
end;

procedure TSudokuMain.DoOnceAfterStartup;
begin
  { We need to delay the helper creation at launch until the form is
    completely displayed, to avoid a collision with startup state
    restoration done by the constructor. }
  CreateSudokuHelper(AppMemory.LastSudoku);

  { Display('Press F1 for a brief help overview'); }
  ShowHelpPrompt;
end;

procedure TSudokuMain.FormResize(Sender: TObject);
begin
//  Statusbar.Panels[0].Width :=
//    Statusbar.ClientWidth -
//    Statusbar.Panels[1].Width -
//    Statusbar.Panels[2].Width -
//    Statusbar.Height;
end;

{! Implements ISudokuHostform.GetButtonsContainer }
function TSudokuMain.GetButtonsContainer: TLayout;
begin
  Result := ValueButtonsPanel;
end;

{! Implements ISudokuHostform.GetCurrentCandidate }
function TSudokuMain.GetCurrentCandidate: TSudokuValue;
begin
  Result := GetDownValue(ValueButtonsPanel);
end;

{! Implements ISudokuHostform.GetCurrentValue }
function TSudokuMain.GetCurrentValue: TSudokuValue;
begin
  Result := GetDownValue(ValueButtonsPanel);
end;

{! Get the Tag (encoding the value the button represents)
   of the speedbutton that is pressed. }
function TSudokuMain.GetDownValue(aParent: TLayout): TSudokuValue;
var
  Ctrl: TControl;
  I: Integer;
begin
  Result := 0;
  for I := 0 to aParent.ChildrenCount - 1 do
  begin
    Ctrl := aParent.Controls[I];
    if (Ctrl is TButton) and TButton(Ctrl).IsPressed then
    begin
      Result := Ctrl.Tag;
      Break;
    end;
  end;
end;

{! Implements ISudokuHostform.GetModifierkeys }
function TSudokuMain.GetModifierkeys: TShiftState;
begin
  Result := []; //KeyboardStateToShiftState();
end;

{!
<summary>
 Implements ISudokuHostform.GetRightClickAction</summary>
<returns>
 The action to take on a right click or keyboard input</returns>
<remarks>
 To set a candidate the user can hold down the Alt key and just type
 the value, or right-click with the mouse. To clear a candidate he
 can use the Ctrl key instead. The right-click action is also controlled
 with a group of speedbuttons, but the modifier keys take precedence.
 </remarks>
}
function TSudokuMain.GetRightClickAction: TRightClickAction;
var
  LState: TShiftstate;
begin
  LState:= GetModifierKeys; // KeyboardStateToShiftState;
  if ssAlt in LState then
    Result := TRightClickAction.SetCandidate
  else if ssCtrl in LState then
    Result := TRightClickAction.UnsetCandidate
  else if ToggleGosuButton.Enabled and ToggleGosuButton.IsPressed then
    Result := TRightClickAction.ToggleGosu
  else if SetCandidatesButton.IsPressed then
     Result := TRightClickAction.SetCandidate
  else if UnsetCandidatesButton.IsPressed then
    Result := TRightClickAction.UnsetCandidate
  else  // default action is to set a candidate
    Result := TRightClickAction.SetCandidate
end;

procedure TSudokuMain.HelpActionExecute(Sender: TObject);
begin
//  THelpViewerForm.Execute;
end;

procedure TSudokuMain.InitializeSudoku;
begin
  Sudoku.Display.InitializeGrid(SudokuGrid);
  Sudoku.InputHandler.Initialize(FormMain as ISudokuHostForm); //(self as ISudokuHostform);
  ToggleGosuButton.Enabled := Sudoku.IsGosu;
  AppMemory.LastSudoku := Sudoku.Displayname;
  Caption := Format(SMainformCaptionMask, [Sudoku.Displayname]);

  { Hack alert! Make sure the grid's OnClick handler can reliably distinguish
    a click fired by the left mouse button
      from one fired by cursor keys moving the selected cell.
    The startup value for FLastMouseButton is 0,
      which equals TMouseButton.mbLeft! }
//  FLastMouseButton := TMouseButton.mbMiddle;
end;

procedure TSudokuMain.LoadSudokuActionAccept(fn: string);
var
  LFilename: string;
  LSudoku: ISudokuHelper;
begin
//  LFilename := LoadSudokuAction.Dialog.FileName;
  LSudoku := TSudokuFiler.LoadFromFile(LFilename);
  if Assigned(LSudoku) then
  begin
    FSudoku := LSudoku;
    InitializeSudoku;
    Sudoku.Display.Refresh;
    AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
  end;
  FocusGrid;
end;

procedure TSudokuMain.LoadSudokuActionBeforeExecute(Sender: TObject);
begin
//  LoadSudokuAction.Dialog.InitialDir := AppMemory.LastFolder;
end;

procedure TSudokuMain.RevertToMarkActionExecute(MarkName: string);
begin
  Sudoku.RevertToMark(MarkName);
  FocusGrid;
end;

procedure TSudokuMain.RevertToMarkActionUpdate(Sender: TObject);
begin
//  (Sender as TAction).Enabled := Sudoku.HasMarks;
end;

procedure TSudokuMain.RunTest;
var
  LList: TStack<Integer>;
  I: Integer;
  LArray: TArray<Integer>;
  SB: TStringbuilder;
begin
  SB := TStringBuilder.Create(4096);
  try
    LList := TStack<Integer>.Create();
    try
      for I := 1 to 10 do
        LList.Push(I);
      SB.AppendLine('Enumerator sequence:');
      for I in LList do
        SB.AppendFormat('%d, ',[I]);
      SB.AppendLine;
      SB.AppendLine('ToArray sequence:');
      LArray:= LList.ToArray;
      for I := Low(LArray) to High(LArray) do
        SB.AppendFormat('%d, ',[LArray[I]]);
      SB.AppendLine;
      SB.AppendLine('Pop sequence:');
      while LLIst.Count >0 do
        SB.AppendFormat('%d, ',[LList.Pop]);
      SB.AppendLine;
//      ShowMessage(SB.ToString);
    finally
      LList.Free;
    end;
  finally
    SB.Free;
  end;
end;

procedure TSudokuMain.SaveSudokuActionAccept(fn: string);
var
  LFilename: string;
begin
  LFileName := fn;
//  LFilename := SaveSudokuAction.Dialog.FileName;
  TSudokuFiler.SaveToFile(Sudoku, LFilename);
  AppMemory.LastFolder := TPath.GetDirectoryName(LFilename);
  FocusGrid;
  Display(SSaveFileMessageMask, [LFilename], true);
end;

procedure TSudokuMain.SaveSudokuActionBeforeExecute(Sender: TObject);
begin
//  SaveSudokuAction.Dialog.InitialDir := AppMemory.LastFolder;
end;

procedure TSudokuMain.SetMarkActionExecute(Sender: TObject);
var
  LMark: string;
begin
  { Generate a proposed name }
  repeat
    Inc(FLastMarkNum);
    LMark := String.Format(SNewMarkMask, [FLastMarkNum]);
  until not Sudoku.MarkExists(LMark) ;

//  if InputQuery(SNewStackMarkCaption, SNewStackMarkPrompt, LMark) then
//  begin
    Sudoku.AddMark(LMark);
//  end;
  FocusGrid;
end;

procedure TSudokuMain.ShowHelpPrompt;
begin
  Display(SHelpPrompt);
end;

procedure TSudokuMain.StartNewActionExecute(Sender: TObject);
var
  LSudokuName: string;
begin
  LSudokuName := 'Classic Sudoku (9x9)';
//  if TSelectSudokuDlg.Execute(LSudokuName, StartNewButton) then
  CreateSudokuHelper(LSudokuName);
  FocusGrid;
end;

procedure TSudokuMain.SudokuGridClick(Sender: TObject);
//var
//  aCol: Integer;
//  aRow: Integer;
//  Mousepos: TPoint;
begin
//  if FLastMouseButton <> TMouseButton.mbLeft then
//    Exit; // click event fired by cursor keys
//
//  Mousepos:= SudokuGrid.ScreenToClient(Mouse.CursorPos);
//  SudokuGrid.MouseToCell(Mousepos.X, MousePos.Y, aCol, aRow);
//
//  Sudoku.InputHandler.HandleCellClick(aCol, aRow);
//  { Make sure the next click can identify a mouse click vs. keyboard "click". }
//  FLastMouseButton := TMouseButton.mbMiddle;
end;

procedure TSudokuMain.SudokuGridContextPopup(Sender: TObject; MousePos: TPointF; var Handled: Boolean);
//var
//  aCol: Integer;
//  aRow: Integer;
begin
//  Handled := true;
//  Mousepos:= SudokuGrid.ScreenToClient(Mouse.CursorPos);
//  SudokuGrid.MouseToCell(Mousepos.X, MousePos.Y, aCol, aRow);
//  if (aCol >= 0) and (aRow >= 0) then
//  begin
//    SudokuGrid.Col := aCol;
//    SudokuGrid.Row := aRow;
//    Sudoku.InputHandler.HandleCellClick(aCol, aRow, true);
//  end;
end;

procedure TSudokuMain.SudokuGridKeyPress(Sender: TObject; var Key: Char);
begin
  if (SudokuGrid.Col >= 0) and (SudokuGrid.Row >= 0) then
  begin
    Sudoku.InputHandler.HandleCharacter(SudokuGrid.Col, SudokuGrid.Row, Key.ToUpper);
    Key := #0;
  end;
end;

procedure TSudokuMain.SudokuGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
//var
//  LChar: Char;
begin
//  if (Shift * [ssCtrl, ssAlt]) <> [] then
//  begin
//    { If Ctrl or Alt are down the OnKeyPress event will not fire!
//      We have to figure out which character key was pressed ourself. }
//    case Key of
//      VK_NUMPAD1..VK_NUMPAD9: LChar := Char(Ord('0') + Key - VK_NUMPAD0);
//      Ord('1')..Ord('9'),
//      Ord('A')..Ord('G'): LChar := Char(Key);
//    else
//      LChar := #0;
//    end;
//
//    if LChar <> #0 then
//      SudokuGridKeyPress(Sender, LChar);
//  end;
//  FocusGrid;
end;

procedure TSudokuMain.SudokuGridMouseDown(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
//  FLastMouseButton := Button;
end;

procedure TSudokuMain.TestButtonClick(Sender: TObject);
begin
  RunTest;
end;

procedure TSudokuMain.UndoActionExecute(Sender: TObject);
begin
  Sudoku.Undo;
  FocusGrid;
end;

procedure TSudokuMain.UndoActionUpdate(Sender: TObject);
begin
//  (Sender as TAction).Enabled := Sudoku.CanUndo;
end;

procedure TSudokuMain.UpdateActions;
begin
  inherited;
//  StatusBar.Panels[1].Text := Format(SLeftMask,[CurrentValue]);
//  StatusBar.Panels[2].Text := Format(SRightMask,[CurrentCandidate]);
//  if GetAsyncKeyState(VK_MENU) < 0 then
//    SetCandidatesButton.Down := true
//  else
//    if GetAsyncKeyState(VK_CONTROL) < 0 then
//      UnsetCandidatesButton.Down := true;
end;

end.

