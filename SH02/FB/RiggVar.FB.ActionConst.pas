unit RiggVar.FB.ActionConst;

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

type
  TFederAction = Integer;

const
  // --- generated code snippet ---

  { EmptyAction }
  faNoop = 0;

  { Pages }
  faActionPageM = 1;
  faActionPageP = 2;
  faActionPageE = 3;
  faActionPageS = 4;
  faActionPage1 = 5;
  faActionPage2 = 6;

  { Forms }
  faShowMemo = 7;
  faShowActions = 8;
  faShowInfo = 9;

  { TouchLayout }
  faTouchTablet = 10;
  faTouchPhone = 11;
  faTouchDesk = 12;

  { Wheel }
  faWheelLeft = 13;
  faWheelRight = 14;
  faWheelDown = 15;
  faWheelUp = 16;

  { ColorScheme }
  faCycleColorSchemeM = 17;
  faCycleColorSchemeP = 18;

  { BtnLegendTablet }
  faTL01 = 19;
  faTL02 = 20;
  faTL03 = 21;
  faTL04 = 22;
  faTL05 = 23;
  faTL06 = 24;
  faTR01 = 25;
  faTR02 = 26;
  faTR03 = 27;
  faTR04 = 28;
  faTR05 = 29;
  faTR06 = 30;
  faTR07 = 31;
  faTR08 = 32;
  faBL01 = 33;
  faBL02 = 34;
  faBL03 = 35;
  faBL04 = 36;
  faBL05 = 37;
  faBL06 = 38;
  faBL07 = 39;
  faBL08 = 40;
  faBR01 = 41;
  faBR02 = 42;
  faBR03 = 43;
  faBR04 = 44;
  faBR05 = 45;
  faBR06 = 46;

  { BtnLegendPhone }
  faMB01 = 47;
  faMB02 = 48;
  faMB03 = 49;
  faMB04 = 50;
  faMB05 = 51;
  faMB06 = 52;
  faMB07 = 53;
  faMB08 = 54;

  { TouchBarLegend }
  faTouchBarTop = 55;
  faTouchBarBottom = 56;
  faTouchBarLeft = 57;
  faTouchBarRight = 58;

  { SudokuNavigation }
  faNavColM = 59;
  faNavColP = 60;
  faNavRowM = 61;
  faNavRowP = 62;
  faNavColFirst = 63;
  faNavColLast = 64;
  faNavRowFirst = 65;
  faNavRowLast = 66;

  { SudokuSelection }
  faSelect0 = 67;
  faSelect1 = 68;
  faSelect2 = 69;
  faSelect3 = 70;
  faSelect4 = 71;
  faSelect5 = 72;
  faSelect6 = 73;
  faSelect7 = 74;
  faSelect8 = 75;
  faSelect9 = 76;
  faSelect10 = 77;
  faSelect11 = 78;
  faSelect12 = 79;
  faSelect13 = 80;
  faSelect14 = 81;
  faSelect15 = 82;
  faSelect16 = 83;

  { SudokuPlacing }
  faPlace0 = 84;
  faPlace1 = 85;
  faPlace2 = 86;
  faPlace3 = 87;
  faPlace4 = 88;
  faPlace5 = 89;
  faPlace6 = 90;
  faPlace7 = 91;
  faPlace8 = 92;
  faPlace9 = 93;
  faPlace10 = 94;
  faPlace11 = 95;
  faPlace12 = 96;
  faPlace13 = 97;
  faPlace14 = 98;
  faPlace15 = 99;
  faPlace16 = 100;

  { SudokuScene }
  faSudoku09A = 101;
  faSudoku09B = 102;
  faSudoku12A = 103;
  faSudoku12B = 104;
  faSudoku12C = 105;
  faSudoku12D = 106;
  faSudoku16A = 107;
  faSudoku16B = 108;
  faSudoku16C = 109;
  faSudoku16D = 110;

  { SudokuMode }
  faSetFocusMode = 111;
  faSetValueMode = 112;
  faSetCandidateMode = 113;
  faUnsetCandidateMode = 114;
  faToggleGosuMode = 115;

  { SudokuCommands }
  faToggleGosu = 116;
  faNewSudoku = 117;
  faSaveSudoku = 118;
  faLoadSudoku = 119;
  faClearStack = 120;
  faUndo = 121;
  faSetMark = 122;
  faRevertToMark = 123;

  faMax = 124;

implementation

end.
