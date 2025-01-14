﻿unit RiggVar.FederModel.Action;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action;

type
  TFederActionHandler = class(TFederActionHandlerBase)
  public
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure Execute(fa: TFederAction); override;
    function GetChecked(fa: TFederAction): Boolean; override;
    function GetEnabled(fa: TFederAction): Boolean; override;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  SH.Interfaces;

procedure TFederActionHandler.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  fa: TFederAction;
begin
  fa := faNoop;
  if not Main.IsUp then
    Exit;

  if (fa = faNoop) and (Main.Keyboard <> nil) then
  begin
    fa := Main.Keyboard.KeyUpAction(Key, KeyChar, Shift);
  end;

  if fa <> faNoop then
  begin
    Execute(fa);
    Key := 0;
  end;
end;

function TFederActionHandler.GetEnabled(fa: TFederAction): Boolean;
var
  M: TMain;
begin
  result := True;

  M := Main;
  if M = nil then
    Exit;
  if not M.IsUp then
    Exit;
  if M.Sudoku = nil then
    Exit;

  case fa of
    faSelect10..faSelect12: result := M.Sudoku.Data.Bounds.MaxValue > 9;
    faSelect13..faSelect16: result := M.Sudoku.Data.Bounds.MaxValue > 12;

    faToggleGosu: result := M.Sudoku.IsGosu;
    faToggleGosuMode: result := M.Sudoku.IsGosu;

    faUndo: result := M.Sudoku.CanUndo;
    faClearStack: result := M.Sudoku.CanUndo;

    faRevertToMark: result := M.Sudoku.HasMarks;
  end;
end;

function TFederActionHandler.GetChecked(fa: TFederAction): Boolean;
var
  M: TMain;
begin
  result := False;

  M := Main;
  if M = nil then
    Exit;
  if not M.IsUp then
    Exit;
  if M.Sudoku = nil then
    Exit;

  case fa of
    faSelect0..faSelect16: result := M.CurrentValue = fa - faSelect0;

    faSetFocusMode: result := M.ClickAction = TClickAction.SetFocus;
    faSetValueMode: result := M.ClickAction = TClickAction.SetValue;
    faSetCandidateMode: result := M.ClickAction = TClickAction.SetCandidate;
    faUnsetCandidateMode: result := M.ClickAction = TClickAction.UnsetCandidate;
    faToggleGosuMode: result := M.ClickAction = TClickAction.ToggleGosu;

    faUndo: result := M.Sudoku.CanUndo;

    faTL02: result := M.FixedZOrder;
    faTL03: result := not M.FixedZOrder;
    faTL04: result := M.SudokuGraph.WantClearToRed;
  end;
end;

procedure TFederActionHandler.Execute(fa: TFederAction);
var
  M: TMain;
begin
  M := Main;
  if M = nil then
    Exit;
  if not M.IsUp then
    Exit;
  if M.Sudoku = nil then
    Exit;

  case fa of
    faActionPageM: M.CycleToolSet(-1);
    faActionPageP: M.CycleToolSet(1);
    faActionPage1: M.FederText.ActionPage := 1;
    faActionPage2: M.FederText.ActionPage := 2;
    faActionPageE: M.FederText.ActionPage := -1;
    faActionPageS: M.FederText.ActionPage := -3;

    faCycleColorSchemeM: M.CycleColorSchemeM;
    faCycleColorSchemeP: M.CycleColorSchemeP;

    faSelect0..faSelect16: M.CurrentValue := fa - faSelect0;

    faPlace0: M.HandleCharacter('0');
    faPlace1: M.HandleCharacter('1');
    faPlace2: M.HandleCharacter('2');
    faPlace3: M.HandleCharacter('3');
    faPlace4: M.HandleCharacter('4');
    faPlace5: M.HandleCharacter('5');
    faPlace6: M.HandleCharacter('6');
    faPlace7: M.HandleCharacter('7');
    faPlace8: M.HandleCharacter('8');
    faPlace9: M.HandleCharacter('9');
    faPlace10: M.HandleCharacter('a');
    faPlace11: M.HandleCharacter('b');
    faPlace12: M.HandleCharacter('c');
    faPlace13: M.HandleCharacter('d');
    faPlace14: M.HandleCharacter('e');
    faPlace15: M.HandleCharacter('f');
    faPlace16: M.HandleCharacter('g');

    faNavColM: M.SudokuGraph.NavCol(-1);
    faNavColP: M.SudokuGraph.NavCol(+1);
    faNavRowM: M.SudokuGraph.NavRow(-1);
    faNavRowP: M.SudokuGraph.NavRow(+1);

    faNavColFirst: M.SudokuGraph.NavCol(-99);
    faNavColLast: M.SudokuGraph.NavCol(99);
    faNavRowFirst: M.SudokuGraph.NavRow(-99);
    faNavRowLast: M.SudokuGraph.NavRow(99);

    faSudoku09A: M.StartNew(fa);
    faSudoku09B: M.StartNew(fa);

    faSudoku12A: M.StartNew(fa);
    faSudoku12B: M.StartNew(fa);
    faSudoku12C: M.StartNew(fa);
    faSudoku12D: M.StartNew(fa);

    faSudoku16A: M.StartNew(fa);
    faSudoku16B: M.StartNew(fa);
    faSudoku16C: M.StartNew(fa);
    faSudoku16D: M.StartNew(fa);

    faSetFocusMode: M.ClickAction := TClickAction.SetFocus;
    faSetValueMode: M.ClickAction := TClickAction.SetValue;
    faSetCandidateMode: M.ClickAction := TClickAction.SetCandidate;
    faUnsetCandidateMode: M.ClickAction := TClickAction.UnsetCandidate;
    faToggleGosuMode: M.ClickAction := TClickAction.ToggleGosu;

    faToggleGosu: M.HandleCharacter(' ');
    faUndo: M.Sudoku.Undo;
    faClearStack: M.Sudoku.ClearUndostack;

//    faSaveSudoku: M.SudokuMain.SaveSudokuBin;
//    faLoadSudoku: M.SudokuMain.LoadSudokuBin;

    faSaveSudoku: M.SudokuMain.SaveSudokuTxt;
    faLoadSudoku: M.SudokuMain.LoadSudokuTxt;

    faSetMark: M.SudokuMain.SetMark;
    faRevertToMark: M.SudokuMain.RevertToMark('M0');

    else
    begin
      M.HandleAction(fa);
    end;

  end;
  M.Sudoku.Display.Refresh;
  M.FederText.CheckState;
end;

end.
