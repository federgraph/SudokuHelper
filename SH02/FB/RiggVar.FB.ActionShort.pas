unit RiggVar.FB.ActionShort;

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

function GetFederActionShort(fa: TFederAction): string;

implementation

function GetFederActionShort(fa: TFederAction): string;
begin
  result := '??';
  case fa of
    // --- generated code snippet ---

    { EmptyAction }
    faNoop: result := '';

    { Pages }
    faActionPageM: result := 'P-';
    faActionPageP: result := 'P+';
    faActionPageE: result := 'PE';
    faActionPageS: result := 'PS';
    faActionPage1: result := 'HP';
    faActionPage2: result := 'SP';

    { Forms }
    faShowMemo: result := 'FM';
    faShowActions: result := 'FA';
    faShowInfo: result := 'FI';

    { TouchLayout }
    faTouchTablet: result := 'tab';
    faTouchPhone: result := 'pho';
    faTouchDesk: result := 'dsk';

    { Wheel }
    faWheelLeft: result := 'wl';
    faWheelRight: result := 'wr';
    faWheelDown: result := 'wd';
    faWheelUp: result := 'wu';

    { ColorScheme }
    faCycleColorSchemeM: result := 'c-';
    faCycleColorSchemeP: result := 'c+';

    { BtnLegendTablet }
    faTL01: result := '#1';
    faTL02: result := '#2';
    faTL03: result := '#3';
    faTL04: result := '#4';
    faTL05: result := '#5';
    faTL06: result := '#6';
    faTR01: result := '1#';
    faTR02: result := '2#';
    faTR03: result := '3#';
    faTR04: result := '4#';
    faTR05: result := '5#';
    faTR06: result := '6#';
    faTR07: result := '7#';
    faTR08: result := '8#';
    faBL01: result := '1*';
    faBL02: result := '2*';
    faBL03: result := '3*';
    faBL04: result := '4*';
    faBL05: result := '5*';
    faBL06: result := '6*';
    faBL07: result := '7*';
    faBL08: result := '8*';
    faBR01: result := '*1';
    faBR02: result := '*2';
    faBR03: result := '*3';
    faBR04: result := '*4';
    faBR05: result := '*5';
    faBR06: result := '*6';

    { BtnLegendPhone }
    faMB01: result := '_1';
    faMB02: result := '_2';
    faMB03: result := '_3';
    faMB04: result := '_4';
    faMB05: result := '_5';
    faMB06: result := '_6';
    faMB07: result := '_7';
    faMB08: result := '_8';

    { TouchBarLegend }
    faTouchBarTop: result := 'tbT';
    faTouchBarBottom: result := 'tbB';
    faTouchBarLeft: result := 'tbL';
    faTouchBarRight: result := 'tbR';

    { SudokuNavigation }
    faNavColM: result := 'nc-';
    faNavColP: result := 'nc+';
    faNavRowM: result := 'nr-';
    faNavRowP: result := 'nr+';
    faNavColFirst: result := 'ncF';
    faNavColLast: result := 'ncL';
    faNavRowFirst: result := 'nrF';
    faNavRowLast: result := 'nrL';

    { SudokuSelection }
    faSelect0: result := 's0';
    faSelect1: result := 's1';
    faSelect2: result := 's2';
    faSelect3: result := 's3';
    faSelect4: result := 's4';
    faSelect5: result := 's5';
    faSelect6: result := 's6';
    faSelect7: result := 's7';
    faSelect8: result := 's8';
    faSelect9: result := 's9';
    faSelect10: result := 's10';
    faSelect11: result := 's11';
    faSelect12: result := 's12';
    faSelect13: result := 's13';
    faSelect14: result := 's14';
    faSelect15: result := 's15';
    faSelect16: result := 's16';

    { SudokuPlacing }
    faPlace0: result := 'p0';
    faPlace1: result := 'p1';
    faPlace2: result := 'p2';
    faPlace3: result := 'P3';
    faPlace4: result := 'p4';
    faPlace5: result := 'p5';
    faPlace6: result := 'p6';
    faPlace7: result := 'p7';
    faPlace8: result := 'p8';
    faPlace9: result := 'p9';
    faPlace10: result := 'p10';
    faPlace11: result := 'p11';
    faPlace12: result := 'p12';
    faPlace13: result := 'p13';
    faPlace14: result := 'p14';
    faPlace15: result := 'p15';
    faPlace16: result := 'p16';

    { SudokuScene }
    faSudoku09A: result := '9A';
    faSudoku09B: result := '9B';
    faSudoku12A: result := '12A';
    faSudoku12B: result := '12B';
    faSudoku12C: result := '12C';
    faSudoku12D: result := '12D';
    faSudoku16A: result := '16A';
    faSudoku16B: result := '16B';
    faSudoku16C: result := '16C';
    faSudoku16D: result := '16D';

    { SudokuMode }
    faSetFocusMode: result := '.SF';
    faSetValueMode: result := '.SV';
    faSetCandidateMode: result := '.SC';
    faUnsetCandidateMode: result := '.UC';
    faToggleGosuMode: result := '.TG';

    { SudokuCommands }
    faToggleGosu: result := 'TG';
    faNewSudoku: result := 'NS';
    faSaveSudoku: result := 'SS';
    faLoadSudoku: result := 'LS';
    faClearStack: result := 'CS';
    faUndo: result := '^z';
    faSetMark: result := 'SM';
    faRevertToMark: result := 'RtM';
  end;
end;

end.
