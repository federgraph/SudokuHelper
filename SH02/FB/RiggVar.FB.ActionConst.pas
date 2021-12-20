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
  faPlusOne = 13;
  faWheelLeft = 14;
  faWheelRight = 15;
  faWheelDown = 16;
  faWheelUp = 17;

  { ColorScheme }
  faCycleColorSchemeM = 18;
  faCycleColorSchemeP = 19;

  { FederText }
  faToggleTouchFrame = 20;

  { Format }
  faFormatLandscape = 21;
  faFormatPortrait = 22;
  faFormatIPhoneLandscape = 23;
  faFormatIPhonePortrait = 24;

  { TouchBarLegend }
  faTouchBarTop = 25;
  faTouchBarBottom = 26;
  faTouchBarLeft = 27;
  faTouchBarRight = 28;

  { Reset }
  faReset = 29;

  { SudokuNavigation }
  faNavColM = 30;
  faNavColP = 31;
  faNavRowM = 32;
  faNavRowP = 33;
  faNavColFirst = 34;
  faNavColLast = 35;
  faNavRowFirst = 36;
  faNavRowLast = 37;

  { SudokuSelection }
  faSelect0 = 38;
  faSelect1 = 39;
  faSelect2 = 40;
  faSelect3 = 41;
  faSelect4 = 42;
  faSelect5 = 43;
  faSelect6 = 44;
  faSelect7 = 45;
  faSelect8 = 46;
  faSelect9 = 47;
  faSelect10 = 48;
  faSelect11 = 49;
  faSelect12 = 50;
  faSelect13 = 51;
  faSelect14 = 52;
  faSelect15 = 53;
  faSelect16 = 54;

  { SudokuPlacing }
  faPlace0 = 55;
  faPlace1 = 56;
  faPlace2 = 57;
  faPlace3 = 58;
  faPlace4 = 59;
  faPlace5 = 60;
  faPlace6 = 61;
  faPlace7 = 62;
  faPlace8 = 63;
  faPlace9 = 64;
  faPlace10 = 65;
  faPlace11 = 66;
  faPlace12 = 67;
  faPlace13 = 68;
  faPlace14 = 69;
  faPlace15 = 70;
  faPlace16 = 71;

  { SudokuScene }
  faSudoku09A = 72;
  faSudoku09B = 73;
  faSudoku12A = 74;
  faSudoku12B = 75;
  faSudoku12C = 76;
  faSudoku12D = 77;
  faSudoku16A = 78;
  faSudoku16B = 79;
  faSudoku16C = 80;
  faSudoku16D = 81;

  { SudokuMode }
  faSetFocusMode = 82;
  faSetValueMode = 83;
  faSetCandidateMode = 84;
  faUnsetCandidateMode = 85;
  faToggleGosuMode = 86;

  { SudokuCommands }
  faToggleGosu = 87;
  faNewSudoku = 88;
  faSaveSudoku = 89;
  faLoadSudoku = 90;
  faClearStack = 91;
  faUndo = 92;
  faSetMark = 93;
  faRevertToMark = 94;

  faMax = 95;

implementation

end.
