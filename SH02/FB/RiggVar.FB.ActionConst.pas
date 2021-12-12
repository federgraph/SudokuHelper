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

  { Scene }
  faSudoku9 = 15;
  faSudoku12 = 16;
  faSudoku16 = 17;

  { Placing }
  faPlace0 = 18;
  faPlace1 = 19;
  faPlace2 = 20;
  faPlace3 = 21;
  faPlace4 = 22;
  faPlace5 = 23;
  faPlace6 = 24;
  faPlace7 = 25;
  faPlace8 = 26;
  faPlace9 = 27;
  faPlace10 = 28;
  faPlace11 = 29;
  faPlace12 = 30;
  faPlace13 = 31;
  faPlace14 = 32;
  faPlace15 = 33;
  faPlace16 = 34;

  { Wheel }
  faPlusOne = 35;
  faWheelLeft = 36;
  faWheelRight = 37;
  faWheelDown = 38;
  faWheelUp = 39;

  { ColorScheme }
  faCycleColorSchemeM = 40;
  faCycleColorSchemeP = 41;

  { FederText }
  faToggleTouchFrame = 42;

  { Format }
  faFormatLandscape = 43;
  faFormatPortrait = 44;
  faFormatIPhoneLandscape = 45;
  faFormatIPhonePortrait = 46;

  { Navigation }
  faNavRowP = 47;
  faNavRowM = 48;
  faNavColM = 49;
  faNavColP = 59;

  faNavColFirst = 60;
  faNavColLast = 61;
  faNavRowFirst = 62;
  faNavRowLast = 63;

  { TouchBarLegend }
  faTouchBarTop = 64;
  faTouchBarBottom = 65;
  faTouchBarLeft = 66;
  faTouchBarRight = 67;

  { Reset }
  faReset = 68;

  { DropTarget }
  faToggleDropTarget = 69;

  { CopyPaste }
  faSave = 70;
  faLoad = 71;
  faOpen = 72;
  faCopy = 73;
  faPaste = 74;
  faShare = 75;

  { Selection }
  faSelect0 = 76;
  faSelect1 = 77;
  faSelect2 = 78;
  faSelect3 = 79;
  faSelect4 = 80;
  faSelect5 = 81;
  faSelect6 = 82;
  faSelect7 = 83;
  faSelect8 = 84;
  faSelect9 = 85;
  faSelect10 = 86;
  faSelect11 = 87;
  faSelect12 = 88;
  faSelect13 = 89;
  faSelect14 = 90;
  faSelect15 = 91;
  faSelect16 = 92;

  faMax = 93;

implementation

end.

