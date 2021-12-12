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
    faShowData: result := 'faShowData';
    faShowRepo: result := 'faShowRepo';

    { TouchLayout }
    faTouchTablet: result := 'faTouchTablet';
    faTouchPhone: result := 'faTouchPhone';
    faTouchDesk: result := 'faTouchDesk';

    { Wheel }
    faPlusOne: result := 'faPlusOne';
    faWheelLeft: result := 'faWheelLeft';
    faWheelRight: result := 'faWheelRight';
    faWheelDown: result := 'faWheelDown';
    faWheelUp: result := 'faWheelUp';

    { ColorScheme }
    faCycleColorSchemeM: result := 'faCycleColorSchemeM';
    faCycleColorSchemeP: result := 'faCycleColorSchemeP';

    { Keyboard }
    faKeyboard01: result := 'faKeyboard01';
    faKeyboard02: result := 'faKeyboard02';
    faSetShift: result := 'faSetShift';
    faSetCtrl: result := 'faSetCtrl';
    faClearShift: result := 'faClearShift';

    { FederText }
    faToggleAllText: result := 'faToggleAllText';
    faToggleTouchFrame: result := 'faToggleTouchFrame';

    { Format }
    faFormatLandscape: result := 'faFormatLandscape';
    faFormatPortrait: result := 'faFormatPortrait';
    faFormatIPhoneLandscape: result := 'faFormatIPhoneLandscape';
    faFormatIPhonePortrait: result := 'faFormatIPhonePortrait';

    { Help }
    faCycleHelpM: result := 'faCycleHelpM';
    faCycleHelpP: result := 'faCycleHelpP';
    faHelpHome: result := 'faHelpHome';

    { TouchBarLegend }
    faTouchBarTop: result := 'faTouchBarTop';
    faTouchBarBottom: result := 'faTouchBarBottom';
    faTouchBarLeft: result := 'faTouchBarLeft';
    faTouchBarRight: result := 'faTouchBarRight';

    { Reset }
    faReset: result := 'faReset';

    { DropTarget }
    faToggleDropTarget: result := 'faToggleDropTarget';

    { CopyPaste }
    faSave: result := 'faSave';
    faLoad: result := 'faLoad';
    faOpen: result := 'faOpen';
    faCopy: result := 'faCopy';
    faPaste: result := 'faPaste';
    faShare: result := 'faShare';

    { ViewOptions }
    faToggleMoveMode: result := 'faToggleMoveMode';
    faLinearMove: result := 'faLinearMove';
    faExpoMove: result := 'faExpoMove';

    { Scene }
    faSudoku9: result := 'faSudoku9';
    faSudoku12: result := 'faSudoku12';
    faSudoku16: result := 'faSudoku16';

    { Current Value Selection }
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

    { Value Placing }
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

    { Navigation }
    faNavColM: result := 'faNavColM';
    faNavColP: result := 'faNavColP';
    faNavRowM: result := 'faNavRowM';
    faNavRowP: result := 'faNavRowP';

    faNavColFirst: result := 'faNavColFirst';
    faNavColLast: result := 'faNavColLast';
    faNavRowFirst: result := 'faNavRowFirst';
    faNavRowLast: result := 'faNavRowLast';

  end;
end;

end.
