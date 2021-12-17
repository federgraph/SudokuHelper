unit RiggVar.FB.ActionGroup;

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

{$define WantAll}

uses
  RiggVar.FB.ActionConst;

type
  TActionGroup = array of Integer;

const

ActionGroupEmptyAction: TActionGroup = [
faNoop];

ActionGroupTouchLayout: TActionGroup = [
faTouchTablet,
faTouchPhone,
faTouchDesk];

ActionGroupPages: TActionGroup = [
faActionPageM,
faActionPageP,
faActionPageE,
faActionPageS,
faActionPage1,
faActionPage2];

ActionGroupColorScheme: TActionGroup = [
faCycleColorSchemeM,
faCycleColorSchemeP];

ActionGroupWheel: TActionGroup = [
faPlusOne,
//faPlusTen,
faWheelLeft,
faWheelRight,
faWheelDown,
faWheelUp
//faParamValuePlus1,
//faParamValueMinus1,
//faParamValuePlus10,
//faParamValueMinus10
];

ActionGroupForms: TActionGroup = [
faShowMemo,
faShowActions,
faShowInfo,
faShowData,
faShowRepo];

ActionGroupFormat: TActionGroup = [
faFormatLandscape,
faFormatPortrait,
//faFormatSquare,
faFormatIPhoneLandscape,
faFormatIPhonePortrait];

ActionGroupReset: TActionGroup = [
faReset];

ActionGroupFederText: TActionGroup = [
//faToggleAllText,
faToggleTouchFrame];

//ActionGroupBtnLegendTablet: TActionGroup = [
//faTL01,
//faTL02,
//faTL03,
//faTL04,
//faTL05,
//faTL06,
//faTR01,
//faTR02,
//faTR03,
//faTR04,
//faTR05,
//faTR06,
//faTR07,
//faTR08,
//faBL01,
//faBL02,
//faBL03,
//faBL04,
//faBL05,
//faBL06,
//faBL07,
//faBL08,
//faBR01,
//faBR02,
//faBR03,
//faBR04,
//faBR05,
//faBR06];

//ActionGroupBtnLegendPhone: TActionGroup = [
//faMB01,
//faMB02,
//faMB03,
//faMB04,
//faMB05,
//faMB06,
//faMB07,
//faMB08];

ActionGroupTouchBarLegend: TActionGroup = [
faTouchBarTop,
faTouchBarBottom,
faTouchBarLeft,
faTouchBarRight];

ActionGroupDropTarget: TActionGroup = [
faToggleDropTarget];

//ActionGroupLanguage: TActionGroup = [
//faToggleLanguage];

ActionGroupCopyPaste: TActionGroup = [
faSave,
faLoad,
faOpen,
faCopy,
faPaste,
faShare];

ActionGroupSudokuScene: TActionGroup = [
faSudoku09A,
faSudoku09B,
faSudoku12A,
faSudoku12B,
faSudoku12C,
faSudoku12D,
faSudoku16A,
faSudoku16B,
faSudoku16C,
faSudoku16D];

ActionGroupSudokuNavigation: TActionGroup = [
faNavColM,
faNavColP,
faNavRowM,
faNavRowP,
faNavColFirst,
faNavColLast,
faNavRowFirst,
faNavRowLast];

ActionGroupSudokuSelection: TActionGroup = [
faSelect0,
faSelect1,
faSelect2,
faSelect3,
faSelect4,
faSelect5,
faSelect6,
faSelect7,
faSelect8,
faSelect9,
faSelect10,
faSelect11,
faSelect12,
faSelect13,
faSelect14,
faSelect15,
faSelect16];

ActionGroupSudokuPlacing: TActionGroup = [
faPlace0,
faPlace1,
faPlace2,
faPlace3,
faPlace4,
faPlace5,
faPlace6,
faPlace7,
faPlace8,
faPlace9,
faPlace10,
faPlace11,
faPlace12,
faPlace13,
faPlace14,
faPlace15,
faPlace16];

ActionGroupSudokuMode: TActionGroup = [
faSetFocusMode,
faSetValueMode,
faSetCandidateMode,
faUnsetCandidateMode,
faToggleGosuMode];

ActionGroupSudokuCommands: TActionGroup = [
faToggleGosu,
faNewSudoku,
faSaveSudoku,
faLoadSudoku,
faClearStack,
faUndo,
faSetMark,
faRevertToMark];

implementation

end.
