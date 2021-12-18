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
    function GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
    function GetActionFromKeyChar(KeyChar: char): Integer;
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

function TFederKeyboard01.GetActionFromKey(Shift: TShiftState; Key: Word): Integer;
begin
  result := faNoop;

  if Key = vkC then
    result := faCopy
  else if Key = vkV then
    result := faPaste
  else if Key = vkL then
    result := faLoad
  else if Key = vkS then
    result := faSave
  else if Key = vkO then
    result := faOpen

  else if ssCtrl in Shift then
  begin
    if Key = vkZ then
      result := faUndo
  end

  else
  begin

    if Key = vkLeft then
    begin
      if ssShift in Shift then
        result := faWheelLeft
      else
        result := faNavColM
    end
    else if Key = vkRight then
    begin
      if ssShift in Shift then
        result := faWheelRight
      else
        result := faNavColP
    end

    else if Key = vkUp then
    begin
      if ssShift in Shift then
        result := faWheelUp
      else
        result := faNavRowM
    end
    else if Key = vkDown then
    begin
      if ssShift in Shift then
        result := faWheelDown
      else
        result := faNavRowP
    end

    else if Key = vkPrior then
      result := faNavRowFirst
    else if Key = vkNext then
      result := faNavRowLast

    else if Key = vkHome then
      result := faNavColFirst
    else if Key = vkEnd then
      result := faNavColLast

    else if Key = vkEscape then
    begin
      if ssShift in Shift then
        result := faReset
      else
        result := faNoop;
    end

    else if Key = vkF1 then
      result := faShowMemo

  end;

end;

function TFederKeyboard01.GetActionFromKeyChar(KeyChar: char): Integer;
var
  fa: Integer;
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

    's': fa := faCycleColorSchemeP;
    'S': fa := faCycleColorSchemeM;

    ' ': fa := faToggleGosu;

    '+': fa := faActionPageP;
    '*': fa := faActionPageM;
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
