unit RiggVar.FB.ActionName;

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

function GetFederActionName(fa: TFederAction): string;

implementation

function GetFederActionName(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    // --- generated code snippet ---

    { EmptyAction }
    faNoop: result := 'faNoop';

    { Pages }
    faActionPageM: result := 'faActionPageM';
    faActionPageP: result := 'faActionPageP';
    faActionPageE: result := 'faActionPageE';
    faActionPageS: result := 'faActionPageS';
    faActionPage1: result := 'faActionPage1';
    faActionPage2: result := 'faActionPage2';

    { Forms }
    faShowMemo: result := 'faShowMemo';
    faShowActions: result := 'faShowActions';
    faShowInfo: result := 'faShowInfo';

    { TouchLayout }
    faTouchTablet: result := 'faTouchTablet';
    faTouchPhone: result := 'faTouchPhone';
    faTouchDesk: result := 'faTouchDesk';

    { Wheel }
    faWheelLeft: result := 'faWheelLeft';
    faWheelRight: result := 'faWheelRight';
    faWheelDown: result := 'faWheelDown';
    faWheelUp: result := 'faWheelUp';

    { ColorScheme }
    faCycleColorSchemeM: result := 'faCycleColorSchemeM';
    faCycleColorSchemeP: result := 'faCycleColorSchemeP';

    { BtnLegendTablet }
    faTL01: result := 'faTL01';
    faTL02: result := 'faTL02';
    faTL03: result := 'faTL03';
    faTL04: result := 'faTL04';
    faTL05: result := 'faTL05';
    faTL06: result := 'faTL06';
    faTR01: result := 'faTR01';
    faTR02: result := 'faTR02';
    faTR03: result := 'faTR03';
    faTR04: result := 'faTR04';
    faTR05: result := 'faTR05';
    faTR06: result := 'faTR06';
    faTR07: result := 'faTR07';
    faTR08: result := 'faTR08';
    faBL01: result := 'faBL01';
    faBL02: result := 'faBL02';
    faBL03: result := 'faBL03';
    faBL04: result := 'faBL04';
    faBL05: result := 'faBL05';
    faBL06: result := 'faBL06';
    faBL07: result := 'faBL07';
    faBL08: result := 'faBL08';
    faBR01: result := 'faBR01';
    faBR02: result := 'faBR02';
    faBR03: result := 'faBR03';
    faBR04: result := 'faBR04';
    faBR05: result := 'faBR05';
    faBR06: result := 'faBR06';

    { BtnLegendPhone }
    faMB01: result := 'faMB01';
    faMB02: result := 'faMB02';
    faMB03: result := 'faMB03';
    faMB04: result := 'faMB04';
    faMB05: result := 'faMB05';
    faMB06: result := 'faMB06';
    faMB07: result := 'faMB07';
    faMB08: result := 'faMB08';

    { TouchBarLegend }
    faTouchBarTop: result := 'faTouchBarTop';
    faTouchBarBottom: result := 'faTouchBarBottom';
    faTouchBarLeft: result := 'faTouchBarLeft';
    faTouchBarRight: result := 'faTouchBarRight';

    { SudokuNavigation }
    faNavColM: result := 'faNavColM';
    faNavColP: result := 'faNavColP';
    faNavRowM: result := 'faNavRowM';
    faNavRowP: result := 'faNavRowP';
    faNavColFirst: result := 'faNavColFirst';
    faNavColLast: result := 'faNavColLast';
    faNavRowFirst: result := 'faNavRowFirst';
    faNavRowLast: result := 'faNavRowLast';

    { SudokuSelection }
    faSelect0: result := 'faSelect0';
    faSelect1: result := 'faSelect1';
    faSelect2: result := 'faSelect2';
    faSelect3: result := 'faSelect3';
    faSelect4: result := 'faSelect4';
    faSelect5: result := 'faSelect5';
    faSelect6: result := 'faSelect6';
    faSelect7: result := 'faSelect7';
    faSelect8: result := 'faSelect8';
    faSelect9: result := 'faSelect9';
    faSelect10: result := 'faSelect10';
    faSelect11: result := 'faSelect11';
    faSelect12: result := 'faSelect12';
    faSelect13: result := 'faSelect13';
    faSelect14: result := 'faSelect14';
    faSelect15: result := 'faSelect15';
    faSelect16: result := 'faSelect16';

    { SudokuPlacing }
    faPlace0: result := 'faPlace0';
    faPlace1: result := 'faPlace1';
    faPlace2: result := 'faPlace2';
    faPlace3: result := 'faPlace3';
    faPlace4: result := 'faPlace4';
    faPlace5: result := 'faPlace5';
    faPlace6: result := 'faPlace6';
    faPlace7: result := 'faPlace7';
    faPlace8: result := 'faPlace8';
    faPlace9: result := 'faPlace9';
    faPlace10: result := 'faPlace10';
    faPlace11: result := 'faPlace11';
    faPlace12: result := 'faPlace12';
    faPlace13: result := 'faPlace13';
    faPlace14: result := 'faPlace14';
    faPlace15: result := 'faPlace15';
    faPlace16: result := 'faPlace16';

    { SudokuScene }
    faSudoku09A: result := 'faSudoku09A';
    faSudoku09B: result := 'faSudoku09B';
    faSudoku12A: result := 'faSudoku12A';
    faSudoku12B: result := 'faSudoku12B';
    faSudoku12C: result := 'faSudoku12C';
    faSudoku12D: result := 'faSudoku12D';
    faSudoku16A: result := 'faSudoku16A';
    faSudoku16B: result := 'faSudoku16B';
    faSudoku16C: result := 'faSudoku16C';
    faSudoku16D: result := 'faSudoku16D';

    { SudokuMode }
    faSetFocusMode: result := 'faSetFocusMode';
    faSetValueMode: result := 'faSetValueMode';
    faSetCandidateMode: result := 'faSetCandidateMode';
    faUnsetCandidateMode: result := 'faUnsetCandidateMode';
    faToggleGosuMode: result := 'faToggleGosuMode';

    { SudokuCommands }
    faToggleGosu: result := 'faToggleGosu';
    faNewSudoku: result := 'faNewSudoku';
    faSaveSudoku: result := 'faSaveSudoku';
    faLoadSudoku: result := 'faLoadSudoku';
    faClearStack: result := 'faClearStack';
    faUndo: result := 'faUndo';
    faSetMark: result := 'faSetMark';
    faRevertToMark: result := 'faRevertToMark';
  end;
end;

end.
