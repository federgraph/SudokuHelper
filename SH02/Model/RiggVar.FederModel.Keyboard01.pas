unit RiggVar.FederModel.Keyboard01;

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
  System.Classes,
  System.UITypes,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionKeys;

type
  TFederKeyboard01 = class(TFederKeyboard)
  private
    function GetActionFromKey(Shift: TShiftState; Key: Word): TFederAction;
    function GetActionFromKeyChar(KeyChar: char): TFederAction;
  public
    constructor Create;
    function KeyUpAction(var Key: Word; var KeyChar: Char; Shift: TShiftState): TFederAction; override;
  end;

implementation

{ TFederKeyboard01 }

constructor TFederKeyboard01.Create;
begin
  inherited;
  TestName := 'Keyboard01';
end;

function TFederKeyboard01.GetActionFromKey(Shift: TShiftState; Key: Word): TFederAction;
var
  fa: TFederAction;
begin
  fa := faNoop;

  if ssCtrl in Shift then
  begin
    case Key of
      vkZ: fa := faUndo;
    end;
  end

  else if ssShift in Shift then
  begin
    case Key of
      vkLeft: fa := faWheelLeft;
      vkRight: fa := faWheelRight;
      vkUp: fa := faWheelUp;
      vkDown: fa := faWheelDown;
    end;
  end

  else case Key of
    vkLeft: fa := faNavColM;
    vkRight: fa := faNavColP;
    vkUp: fa := faNavRowM;
    vkDown: fa := faNavRowP;

    vkPrior: fa := faNavRowFirst;
    vkNext: fa := faNavRowLast;
    vkHome: fa := faNavColFirst;
    vkEnd: fa := faNavColLast;

    vkF1: fa := faShowMemo;
//    vkSpace: fa := faToggleGosu; // KeyChar ' ' is dominant
    vkEscape: fa := faActionPageE;
  end;

  result := fa;
end;

function TFederKeyboard01.GetActionFromKeyChar(KeyChar: char): TFederAction;
var
  fa: TFederAction;
begin
  fa := faNoop;
  case KeyChar of
    '0': fa := faPlace0;
    '1': fa := faPlace1;
    '2': fa := faPlace2;
    '3': fa := faPlace3;
    '4': fa := faPlace4;
    '5': fa := faPlace5;
    '6': fa := faPlace6;
    '7': fa := faPlace7;
    '8': fa := faPlace8;
    '9': fa := faPlace9;

    'a': fa := faPlace10;
    'A': fa := faPlace10;
    'b': fa := faPlace11;
    'B': fa := faPlace11;
    'c': fa := faPlace12;
    'C': fa := faPlace12;
    'd': fa := faPlace13;
    'D': fa := faPlace13;
    'e': fa := faPlace14;
    'E': fa := faPlace14;
    'f': fa := faPlace15;
    'F': fa := faPlace15;
    'g': fa := faPlace16;
    'G': fa := faPlace16;

    'i': fa := faWheelRight;
    'I': fa := faWheelUp;
    'j': fa := faWheelLeft;
    'J': fa := faWheelDown;

    'M': fa := faSetMark;
    'R': fa := faRevertToMark;

    's': fa := faCycleColorSchemeP;
    'S': fa := faCycleColorSchemeM;

    ' ': fa := faToggleGosu;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;
    '^': fa := faActionPageE;
  end;
  result := fa;
end;

function TFederKeyboard01.KeyUpAction(var Key: Word; var KeyChar: Char; Shift: TShiftState): TFederAction;
begin
  { only one mapping can be detected when testing, there may be more }
  result := GetActionFromKey(Shift, Key);
  if result = faNoop then
    result := GetActionFromKeyChar(KeyChar);
end;

end.
