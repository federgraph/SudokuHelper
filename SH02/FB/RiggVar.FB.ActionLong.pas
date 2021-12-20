unit RiggVar.FB.ActionLong;

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
  RiggVar.FB.ActionConst;

function GetFederActionLong(fa: TFederAction): string;

implementation

function GetFederActionLong(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    // --- generated code snippet ---

    { EmptyAction }
    faNoop: result := 'Noop';

    { Pages }
    faActionPageM: result := 'Action Page -';
    faActionPageP: result := 'Action Page +';
    faActionPageE: result := 'Action Page E';
    faActionPageS: result := 'Action Page S';
    faActionPage1: result := 'Action Page 1';
    faActionPage2: result := 'Action Page 2';

    { Forms }
    faShowMemo: result := 'Form Memo';
    faShowActions: result := 'Form Actions';
    faShowInfo: result := 'Form Info';

    { TouchLayout }
    faTouchTablet: result := 'Touch Tablet';
    faTouchPhone: result := 'Touch Phone';
    faTouchDesk: result := 'Touch Desk';

    { Wheel }
    faPlusOne: result := 'Plus One';
    faWheelLeft: result := 'Wheel -1';
    faWheelRight: result := 'Wheel +1';
    faWheelDown: result := 'Wheel +10';
    faWheelUp: result := 'Wheel -10';

    { ColorScheme }
    faCycleColorSchemeM: result := 'cycle color scheme -';
    faCycleColorSchemeP: result := 'cycle color scheme +';

    { FederText }
    faToggleTouchFrame: result := 'Toggle Touch Frame';

    { Format }
    faFormatLandscape: result := '[Landscape]';
    faFormatPortrait: result := '[Portrait]';
    faFormatIPhoneLandscape: result := '[IPhone Landscape]';
    faFormatIPhonePortrait: result := '[IPhone Portrait]';

    { TouchBarLegend }
    faTouchBarTop: result := 'TouchBar Top';
    faTouchBarBottom: result := 'TouchBar Bottom';
    faTouchBarLeft: result := 'TouchBar Left';
    faTouchBarRight: result := 'TouchBar Right';

    { Reset }
    faReset: result := 'Reset';

    { SudokuNavigation }
    faNavColM: result := 'Nav Col -';
    faNavColP: result := 'Nav Col +';
    faNavRowM: result := 'Nav Row -';
    faNavRowP: result := 'Nav Row +';
    faNavColFirst: result := 'Nav Col First';
    faNavColLast: result := 'Nav Col Last';
    faNavRowFirst: result := 'Nav Row First';
    faNavRowLast: result := 'Nav Row Last';

    { SudokuSelection }
    faSelect0: result := 'Select 0';
    faSelect1: result := 'Select 1';
    faSelect2: result := 'Select 2';
    faSelect3: result := 'Select 3';
    faSelect4: result := 'Select 4';
    faSelect5: result := 'Select 5';
    faSelect6: result := 'Select 6';
    faSelect7: result := 'Select 7';
    faSelect8: result := 'Select 8';
    faSelect9: result := 'Select 9';
    faSelect10: result := 'Select 10';
    faSelect11: result := 'Select 11';
    faSelect12: result := 'Select 12';
    faSelect13: result := 'Select 13';
    faSelect14: result := 'Select 14';
    faSelect15: result := 'Select 15';
    faSelect16: result := 'Select 16';

    { SudokuPlacing }
    faPlace0: result := 'Place 0';
    faPlace1: result := 'Place 1';
    faPlace2: result := 'Place 2';
    faPlace3: result := 'Place 3';
    faPlace4: result := 'Place 4';
    faPlace5: result := 'Place 5';
    faPlace6: result := 'Place 6';
    faPlace7: result := 'Place 7';
    faPlace8: result := 'Place 8';
    faPlace9: result := 'Place 9';
    faPlace10: result := 'Place 10';
    faPlace11: result := 'Place 11';
    faPlace12: result := 'Place 12';
    faPlace13: result := 'Place 13';
    faPlace14: result := 'Place 14';
    faPlace15: result := 'Place 15';
    faPlace16: result := 'Place 16';

    { SudokuScene }
    faSudoku09A: result := 'Classic Sudoku (9x9)';
    faSudoku09B: result := 'Classic Sudoku Gosu (9x9)';
    faSudoku12A: result := 'Sudoku 12x12';
    faSudoku12B: result := '12x12 Sudoku Gosu';
    faSudoku12C: result := '12x12 Sudoku (hexadecimal)';
    faSudoku12D: result := '12x12 Sudoku Gosu (hexadecimal)';
    faSudoku16A: result := 'Sudoku 16x16';
    faSudoku16B: result := '16x16 Sudoku Gosu';
    faSudoku16C: result := '16x16 Sudoku (heptadecimal)';
    faSudoku16D: result := '16x16 Sudoku Gosu (heptadecimal)';

    { SudokuMode }
    faSetFocusMode: result := 'Set Focus Mode';
    faSetValueMode: result := 'Set Value Mode';
    faSetCandidateMode: result := 'Set Candidate Mode';
    faUnsetCandidateMode: result := 'Unset Candidate Mode';
    faToggleGosuMode: result := 'Toggle Gosu Mode';

    { SudokuCommands }
    faToggleGosu: result := 'Toggle Gosu';
    faNewSudoku: result := 'New Sudoku';
    faSaveSudoku: result := 'Save Sudoku';
    faLoadSudoku: result := 'Load Sudoku';
    faClearStack: result := 'Clear Stack';
    faUndo: result := 'Undo';
    faSetMark: result := 'Set Mark';
    faRevertToMark: result := 'Revert to Mark';
  end;
end;

end.
