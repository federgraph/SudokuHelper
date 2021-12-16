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
    faShowData: result := 'FD';
    faShowRepo: result := 'FR';

    { TouchLayout }
    faTouchTablet: result := 'tab';
    faTouchPhone: result := 'pho';
    faTouchDesk: result := 'dsk';

    { Wheel }
    faPlusOne: result := 'one';
    faWheelLeft: result := 'wl';
    faWheelRight: result := 'wr';
    faWheelDown: result := 'wd';
    faWheelUp: result := 'wu';

    { ColorScheme }
    faCycleColorSchemeM: result := 'c-';
    faCycleColorSchemeP: result := 'c+';

    { FederText }
    faToggleTouchFrame: result := 'fra';

    { Format }
    faFormatLandscape: result := '[fL]';
    faFormatPortrait: result := '[fP]';
    faFormatIPhoneLandscape: result := '[fLi]';
    faFormatIPhonePortrait: result := '[fPi]';

    { TouchBarLegend }
    faTouchBarTop: result := 'tbT';
    faTouchBarBottom: result := 'tbB';
    faTouchBarLeft: result := 'tbL';
    faTouchBarRight: result := 'tbR';

    { Reset }
    faReset: result := 'res';

    { DropTarget }
    faToggleDropTarget: result := 'tdt';

    { CopyPaste }
    faSave: result := 'sav';
    faLoad: result := 'loa';
    faOpen: result := 'ope';
    faCopy: result := '^c';
    faPaste: result := '^v';
    faShare: result := 'sha';

    { SudokuSelection }
    faSelect0: result := '0';
    faSelect1: result := '1';
    faSelect2: result := '2';
    faSelect3: result := '3';
    faSelect4: result := '4';
    faSelect5: result := '5';
    faSelect6: result := '6';
    faSelect7: result := '7';
    faSelect8: result := '8';
    faSelect9: result := '9';
    faSelect10: result := '10';
    faSelect11: result := '11';
    faSelect12: result := '12';
    faSelect13: result := '13';
    faSelect14: result := '14';
    faSelect15: result := '15';
    faSelect16: result := '16';

    { SudokuPlacing }
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
    faPlace14: result := 'P14';
    faPlace15: result := 'P15';
    faPlace16: result := 'P16';

    { SudokuNavigation }
    faNavRowM: result := 'nr-';
    faNavRowP: result := 'nr+';
    faNavColM: result := 'nc-';
    faNavColP: result := 'nc+';

    faNavColFirst: result := 'ncF';
    faNavColLast: result := 'ncL';
    faNavRowFirst: result := 'nrF';
    faNavRowLast: result := 'nrL';

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
