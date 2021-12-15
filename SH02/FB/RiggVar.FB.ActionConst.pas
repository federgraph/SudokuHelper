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

// --- generated code snippet ---
// Note that some of the defined actions
//   may not be implemented in this version of the app.

const

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
  faShowData = 10;
  faShowRepo = 11;

  { TouchLayout }
  faTouchTablet = 12;
  faTouchPhone = 13;
  faTouchDesk = 14;

  { Wheel }
  faPlusOne = 15;
  faWheelLeft = 16;
  faWheelRight = 17;
  faWheelDown = 18;
  faWheelUp = 19;

  { ColorScheme }
  faCycleColorSchemeM = 20;
  faCycleColorSchemeP = 21;

  { FederText }
  faToggleTouchFrame = 22;

  { Format }
  faFormatLandscape = 23;
  faFormatPortrait = 24;
  faFormatIPhoneLandscape = 25;
  faFormatIPhonePortrait = 26;

  { TouchBarLegend }
  faTouchBarTop = 27;
  faTouchBarBottom = 28;
  faTouchBarLeft = 29;
  faTouchBarRight = 30;

  { Reset }
  faReset = 31;

  { DropTarget }
  faToggleDropTarget = 32;

  { CopyPaste }
  faSave = 33;
  faLoad = 34;
  faOpen = 35;
  faCopy = 36;
  faPaste = 37;
  faShare = 38;

  { SudokuNavigation }
  faNavColM = 39;
  faNavColP = 40;
  faNavRowM = 41;
  faNavRowP = 42;
  faNavColFirst = 43;
  faNavColLast = 44;
  faNavRowFirst = 45;
  faNavRowLast = 46;

  { SudokuSelection }
  faSelect0 = 47;
  faSelect1 = 48;
  faSelect2 = 49;
  faSelect3 = 50;
  faSelect4 = 51;
  faSelect5 = 52;
  faSelect6 = 53;
  faSelect7 = 54;
  faSelect8 = 55;
  faSelect9 = 56;
  faSelect10 = 57;
  faSelect11 = 58;
  faSelect12 = 59;
  faSelect13 = 60;
  faSelect14 = 61;
  faSelect15 = 62;
  faSelect16 = 63;

  { SudokuPlacing }
  faPlace0 = 64;
  faPlace1 = 65;
  faPlace2 = 66;
  faPlace3 = 67;
  faPlace4 = 68;
  faPlace5 = 69;
  faPlace6 = 70;
  faPlace7 = 71;
  faPlace8 = 72;
  faPlace9 = 73;
  faPlace10 = 74;
  faPlace11 = 75;
  faPlace12 = 76;
  faPlace13 = 77;
  faPlace14 = 78;
  faPlace15 = 79;
  faPlace16 = 80;

  { SudokuScene }
  faSudoku09A = 81;
  faSudoku09B = 82;
  faSudoku12A = 83;
  faSudoku12B = 84;
  faSudoku12C = 85;
  faSudoku12D = 86;
  faSudoku16A = 87;
  faSudoku16B = 88;
  faSudoku16C = 89;
  faSudoku16D = 90;

  faMax = 91;

implementation

end.

