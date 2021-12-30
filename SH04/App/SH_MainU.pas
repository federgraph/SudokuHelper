{!
<summary>
 SH_MainU
</summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2015-09-26<p>
 Version 2.0 created 2021-09-30<p>
 Last modified       2021-11-16<p>
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
 set of buttons used to select values for mouse/touch input are adapted
 to the requirements of a specific kind of Sudoku. This will also adjust
 the form size as needed.

 The program logic is completely handled by a set of classes the main form
 only accesses via as set of interfaces, and the form also implements an
 interface itself to allow the classes in question (especially the one
 handling mouse and keyboard input) to get some info on the state of the
 UI.
</remarks>
}
unit SH_MainU;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LMessages, LCLIntf, LCLType,
  SysUtils,
  Classes,
  Types,
  Forms,
  ComCtrls,
  ExtCtrls,
  Grids,
  StdCtrls,
  Dialogs,
  Buttons,
  ActnList,
  StdActns,
  Controls,
  SH.Interfaces;

const
  UM_FOCUSGRID = WM_USER + 111;

type

  { TFormMain }

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
    TestButton: TButton;
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
    procedure FormShow(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure ClearStackActionExecute(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    procedure TestButtonClick(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure UndoActionUpdate(Sender: TObject);
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
    procedure RunTest;
    procedure ShowHelpPrompt;
  protected
    FormShown: Boolean;
    function GetButtonsContainer: TWincontrol;
    function GetModifierkeys: TShiftstate;
    function GetRightClickAction: TRightClickAction;
    procedure UpdateActions; override;
    procedure UMFocusGrid(var Message: TLMessage); message UM_FOCUSGRID;
    property CurrentCandidate: TSudokuValue read GetCurrentCandidate;
    property CurrentValue: TSudokuValue read GetCurrentValue;
    property Sudoku: ISudokuHelper read FSudoku;
  public
    destructor Destroy; override;
    procedure Display(const S: string; Timed: Boolean = false); overload;
    procedure Display(const Fmt: string; const A: array of const; Timed: Boolean = false); overload;
  end;

var
  FormMain: TFormMain;

implementation

uses
  Generics.Collections,
  SH.HelperBase,
  SH.SudokuHelper,
  SH.SudokuFiler,
  SH.Memory,
  SH.Strings,
  SH_SelectSudokuDlgU,
  SH_SelectMarkDlgU;

{$R *.lfm}

const
  MessageTimeout = 30000; // 30 seconds

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

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    CreateSudokuHelper(CClassicSudoku9x9);
  end;
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
  OnPaint := nil;
  { We need to delay the helper creation at launch until the form is
    completely displayed, to avoid a collision with startup state
    restoration done by the constructor. }
  CreateSudokuHelper(AppMemory.LastSudoku);

  { Display('Press F1 for a brief help overview'); }
  ShowHelpPrompt;
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
  //
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
    AppMemory.LastFolder := ExtractFileDir(LFilename);
  end;
  FocusGrid;
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
  LMark := 'M0';
  if TSelectMarkDlg.Execute(LMark, Sudoku, RevertToMarkButton) then
    Sudoku.RevertToMark(LMark);
  FocusGrid;
end;

procedure TFormMain.RevertToMarkActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Sudoku.HasMarks;
end;

procedure TFormMain.RunTest;
var
  LList: TStack<Integer>;
  I: Integer;
  LArray: TArray<Integer>;
  SB: TStringList;
begin
  SB := TStringList.Create;
  try
    LList := TStack<Integer>.Create();
    try
      for I := 1 to 10 do
        LList.Push(I);
      SB.Add('Enumerator sequence:');
      for I in LList do
        SB.Add(Format('%d, ', [I]));
      SB.Add('');
      SB.Add('ToArray sequence:');
      LArray := LList.ToArray;
      for I := Low(LArray) to High(LArray) do
        SB.Add(Format('%d, ', [LArray[I]]));
      SB.Add('');
      SB.Add('Pop sequence:');
      while LLIst.Count >0 do
        SB.Add(Format('%d, ',[LList.Pop]));
      SB.Add('');
      ShowMessage(SB.ToString);
    finally
      LList.Free;
    end;
  finally
    SB.Free;
  end;
end;

procedure TFormMain.SaveSudokuActionAccept(Sender: TObject);
var
  LFilename: string;
begin
  LFilename := SaveSudokuAction.Dialog.FileName;
  TSudokuFiler.SaveToFile(Sudoku, LFilename);
  AppMemory.LastFolder := ExtractFileDir(LFilename);
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

procedure TFormMain.StartNewActionExecute(Sender: TObject);
var
  LSudokuName: string;
begin
  LSudokuName := CClassicSudoku9x9;

  if TSelectSudokuDlg.Execute(LSudokuName, StartNewButton) then
    CreateSudokuHelper(LSudokuName);
  FocusGrid;
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
begin
  FocusGrid;
end;

procedure TFormMain.SudokuGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLastMouseButton := Button;
end;

procedure TFormMain.TestButtonClick(Sender: TObject);
begin
  RunTest;
end;

procedure TFormMain.UMFocusGrid(var Message: TLMessage);
begin
  Message.Result := 1;
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
  if GetKeyState(VK_SHIFT) < 0 then
    SetCandidatesButton.Down := true
  else if GetKeyState(VK_CONTROL) < 0 then
    UnsetCandidatesButton.Down := true;
end;

end.
