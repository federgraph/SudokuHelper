unit RiggVar.FederModel.Action;

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
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main;

procedure TFederActionHandler.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
var
  fa: Integer;
begin
  fa := faNoop;
  if not Main.IsUp then
    Exit;

  if ssCtrl in Shift then
  begin
    if Key = 49 then
      fa := faNavColFirst
    else if Key = 50 then
      fa := faNavColLast
    else if Key = 51 then
      fa := faNavRowFirst
    else if Key = 52 then
      fa := faNavRowLast
    else if Key = 53 then
      fa := faNoop
  end;

  if (fa = faNoop) and (Main.Keyboard <> nil) then
  begin
{$ifdef MSWINDOWS}
    fa := Main.Keyboard.KeyUpAction(Key, KeyChar, Shift);
{$endif}
{$ifdef MACOS}
    fa := Main.Keyboard.KeyDownAction(Key, KeyChar, Shift);
{$endif}
  end;

  if fa <> faNoop then
  begin
    Execute(fa);
    Key := 0;
  end;
end;

function TFederActionHandler.GetChecked(fa: TFederAction): Boolean;
begin
  result := False;
  case fa of
    faSelect0..faSelect16: result := Main.CurrentValue = fa - faSelect0;
  end;
end;

procedure TFederActionHandler.Execute(fa: TFederAction);
var
  M: TMain;
begin
  M := Main;
  if M = nil then
    Exit;

  case fa of
    faActionPageM: M.CycleToolSet(-1);
    faActionPageP: M.CycleToolSet(1);

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
    faPlace10: M.HandleCharacter('A');
    faPlace11: M.HandleCharacter('B');
    faPlace12: M.HandleCharacter('C');
    faPlace13: M.HandleCharacter('D');
    faPlace14: M.HandleCharacter('E');
    faPlace15: M.HandleCharacter('F');
    faPlace16: M.HandleCharacter('G');

    faNavColM: M.SudokuGrid.NavCol(-1);
    faNavColP: M.SudokuGrid.NavCol(+1);
    faNavRowM: M.SudokuGrid.NavRow(-1);
    faNavRowP: M.SudokuGrid.NavRow(+1);

    faNavColFirst: M.SudokuGrid.NavCol(-99);
    faNavColLast: M.SudokuGrid.NavCol(99);
    faNavRowFirst: M.SudokuGrid.NavRow(-99);
    faNavRowLast: M.SudokuGrid.NavRow(99);

    faSudoku9: M.StartNew(9);
    faSudoku12: M.StartNew(12);
    faSudoku16: M.StartNew(16);

  end;
  if M.Sudoku <> nil then
    M.Sudoku.Display.Refresh;
  M.FederText.CheckState;
end;

end.
