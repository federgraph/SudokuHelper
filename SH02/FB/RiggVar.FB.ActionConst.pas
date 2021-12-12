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
  faShowData = 17;
  faShowRepo = 18;

  { TouchLayout }
  faTouchTablet = 19;
  faTouchPhone = 20;
  faTouchDesk = 21;

  { Scene }
  faSudoku9 = 22;
  faSudoku12 = 23;
  faSudoku16 = 24;

  { Plot }
  faPlace0 = 27;
  faPlace1 = 28;
  faPlace2 = 29;
  faPlace3 = 30;
  faPlace4 = 31;
  faPlace5 = 32;
  faPlace6 = 33;
  faPlace7 = 34;
  faPlace8 = 35;
  faPlace9 = 36;
  faPlace10 = 37;
  faPlace11 = 38;
  faPlace12 = 39;
  faPlace13 = 40;

  faPlace14 = 41;
  faPlace15 = 42;
  faPlace16 = 43;

  { Graph }
  faNavColFirst = 47;
  faNavColLast = 48;
  faNavRowFirst = 49;
  faNavRowLast = 50;

  { Wheel }
  faPlusOne = 69;
  faWheelLeft = 70;
  faWheelRight = 71;
  faWheelDown = 72;
  faWheelUp = 73;

  { ColorScheme }
  faCycleColorSchemeM = 74;
  faCycleColorSchemeP = 75;

  { Keyboard }
  faKeyboard01 = 76;
  faKeyboard02 = 77;
  faSetShift = 78;
  faSetCtrl = 79;
  faClearShift = 80;

  { FederText }
  faToggleAllText = 94;
  faToggleTouchFrame = 95;

  { Format }
  faFormatLandscape = 122;
  faFormatPortrait = 123;
  faFormatIPhoneLandscape = 124;
  faFormatIPhonePortrait = 125;

  { Navigation }
  faNavRowP = 162;
  faNavRowM = 163;
  faNavColM = 164;
  faNavColP = 165;

  { Help }
  faCycleHelpM = 166;
  faCycleHelpP = 167;
  faHelpHome = 168;

  { TouchBarLegend }
  faTouchBarTop = 169;
  faTouchBarBottom = 170;
  faTouchBarLeft = 171;
  faTouchBarRight = 172;

  { Reset }
  faReset = 173;

  { DropTarget }
  faToggleDropTarget = 177;

  { CopyPaste }
  faSave = 178;
  faLoad = 179;
  faOpen = 180;
  faCopy = 181;
  faPaste = 182;
  faShare = 183;

  { ViewOptions }
  faToggleMoveMode = 184;
  faLinearMove = 185;
  faExpoMove = 186;

  { Selection }
  faSelect0 = 187;
  faSelect1 = 188;
  faSelect2 = 189;
  faSelect3 = 190;
  faSelect4 = 191;
  faSelect5 = 192;
  faSelect6 = 193;
  faSelect7 = 194;
  faSelect8 = 195;
  faSelect9 = 196;
  faSelect10 = 197;
  faSelect11 = 198;
  faSelect12 = 199;
  faSelect13 = 200;
  faSelect14 = 201;
  faSelect15 = 202;
  faSelect16 = 203;

  faMax = 220;

implementation

end.

