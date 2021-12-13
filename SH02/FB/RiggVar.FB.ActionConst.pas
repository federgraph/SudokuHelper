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

  { SudokuScene }
  faSudoku9 = 39;
  faSudoku12 = 40;
  faSudoku16 = 41;

  { SudokuNavigation }
  faNavColM = 42;
  faNavColP = 43;
  faNavRowM = 44;
  faNavRowP = 45;
  faNavColFirst = 46;
  faNavColLast = 47;
  faNavRowFirst = 48;
  faNavRowLast = 49;

  { SudokuSelection }
  faSelect0 = 50;
  faSelect1 = 51;
  faSelect2 = 52;
  faSelect3 = 53;
  faSelect4 = 54;
  faSelect5 = 55;
  faSelect6 = 56;
  faSelect7 = 57;
  faSelect8 = 58;
  faSelect9 = 59;
  faSelect10 = 60;
  faSelect11 = 61;
  faSelect12 = 62;
  faSelect13 = 63;
  faSelect14 = 64;
  faSelect15 = 65;
  faSelect16 = 66;

  { SudokuPlacing }
  faPlace0 = 67;
  faPlace1 = 68;
  faPlace2 = 69;
  faPlace3 = 70;
  faPlace4 = 71;
  faPlace5 = 72;
  faPlace6 = 73;
  faPlace7 = 74;
  faPlace8 = 75;
  faPlace9 = 76;
  faPlace10 = 77;
  faPlace11 = 78;
  faPlace12 = 79;
  faPlace13 = 80;
  faPlace14 = 81;
  faPlace15 = 82;
  faPlace16 = 83;

  faMax = 84;

implementation

end.

