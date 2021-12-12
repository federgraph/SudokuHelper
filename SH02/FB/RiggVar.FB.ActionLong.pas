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
    faShowData: result := 'Data';
    faShowRepo: result := 'Repository';

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

    { Keyboard }
    faKeyboard01: result := 'Keyboard 1';
    faKeyboard02: result := 'Keyboard 2';
    faSetShift: result := '[Shift]';
    faSetCtrl: result := '[Control]';
    faClearShift: result := '[]';

    { FederText }
    faToggleAllText: result := 'Toggle All Text';
    faToggleTouchFrame: result := 'Toggle Touch Frame';

    { Format }
    faFormatLandscape: result := '[Landscape]';
    faFormatPortrait: result := '[Portrait]';
    faFormatIPhoneLandscape: result := '[IPhone Landscape]';
    faFormatIPhonePortrait: result := '[IPhone Portrait]';

    { Help }
    faCycleHelpM: result := 'cycle help text -';
    faCycleHelpP: result := 'cycle help text +';
    faHelpHome: result := 'Help home';

    { TouchBarLegend }
    faTouchBarTop: result := 'TouchBar Top';
    faTouchBarBottom: result := 'TouchBar Bottom';
    faTouchBarLeft: result := 'TouchBar Left';
    faTouchBarRight: result := 'TouchBar Right';

    { Reset }
    faReset: result := 'Reset';

    { DropTarget }
    faToggleDropTarget: result := 'Drop target';

    { CopyPaste }
    faSave: result := 'Save';
    faLoad: result := 'Load';
    faOpen: result := 'Open';
    faCopy: result := 'Copy';
    faPaste: result := 'Paste';
    faShare: result := 'Share';

    { ViewOptions }
    faToggleMoveMode: result := 'Toggle move mode';
    faLinearMove: result := 'Linear move';
    faExpoMove: result := 'Exponential move';

    { Select }
    faSelect0: result := 'Select Clear Value 0';
    faSelect1: result := 'Select Sudoku Value 1';
    faSelect2: result := 'Select Sudoku Value 2';
    faSelect3: result := 'Select Sudoku Value 3';
    faSelect4: result := 'Select Sudoku Value 4';
    faSelect5: result := 'Select Sudoku Value 5';
    faSelect6: result := 'Select Sudoku Value 6';
    faSelect7: result := 'Select Sudoku Value 7';
    faSelect8: result := 'Select Sudoku Value 8';
    faSelect9: result := 'Select Sudoku Value 9';
    faSelect10: result := 'Select Sudoku Value 10';
    faSelect11: result := 'Select Sudoku Value 11';
    faSelect12: result := 'Select Sudoku Value 12';
    faSelect13: result := 'Select Sudoku Value 13';
    faSelect14: result := 'Select Sudoku Value 14';
    faSelect15: result := 'Select Sudoku Value 15';
    faSelect16: result := 'Select Sudoku Value 16';

    { Plot }
    faPlace0: result := 'P0';
    faPlace1: result := 'P1';
    faPlace2: result := 'P2';
    faPlace3: result := 'P3';
    faPlace4: result := 'P4';
    faPlace5: result := 'P5';
    faPlace6: result := 'P6';
    faPlace7: result := 'P7';
    faPlace8: result := 'P8';
    faPlace9: result := 'P9';
    faPlace10: result := 'P10';
    faPlace11: result := 'P11';
    faPlace12: result := 'P12';
    faPlace13: result := 'P13';
    faPlace14: result := 'F1';
    faPlace15: result := 'F2';
    faPlace16: result := 'F3';

    { Navigation }
    faNavRowP: result := 'Nav Row +';
    faNavRowM: result := 'Nav Row +';
    faNavColM: result := 'Nav Col -';
    faNavColP: result := 'Nav Col +';

    faNavColFirst: result := 'Nav Col First';
    faNavColLast: result := 'Nav Col Last';
    faNavRowFirst: result := 'Nav Row First';
    faNavRowLast: result := 'Nav Row Last';

    { Scene }
    faSudoku9: result := 'Sudoku 9x9';
    faSudoku12: result := 'Sudoku 12x12';
    faSudoku16: result := 'Sudoku 16x16';

  end;
end;

end.
