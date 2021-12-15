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
  public
    constructor Create;
    function KeyDownAction(var Key: Word; var KeyChar: Char; Shift: TShiftState): TFederAction; override;
    function KeyUpAction(var Key: Word; var KeyChar: Char; Shift: TShiftState): TFederAction; override;
  end;

implementation

{ TFederKeyboard01 }

constructor TFederKeyboard01.Create;
begin
  inherited;
  TestName := 'Keyboard01';
end;

function TFederKeyboard01.KeyDownAction(var Key: Word; var KeyChar: Char; Shift: TShiftState): TFederAction;
begin
  result := faNoop;
end;

function TFederKeyboard01.KeyUpAction(var Key: Word; var KeyChar: Char; Shift: TShiftState): TFederAction;
begin
  { ok, only one mapping can be detected when testing, there may be more }

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
    if Key = vkLeft then

    else if Key = vkRight then

    else if Key = vkUp then

    else if Key = vkDown then

    else if KeyChar = ' ' then

    else if Key = vkSpace then

    else if Key = vkEscape then
    begin
      { this is a Windows 8 shortcut (toggle between Startscreen and Desktop) }
      result := faNoop;
    end

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
      result := faNavColFirst
    else if Key = vkNext then
      result := faNavColLast

    else if Key = vkHome then
      result := faNavRowFirst
    else if Key = vkEnd then
      result := faNavRowLast

    else if Key = vkEscape then
    begin
      if ssShift in Shift then
        result := faReset
      else
        result := faNoop;
    end

    else if KeyChar = '1' then
      result := faPlace1
    else if KeyChar = '2' then
      result := faPlace2
    else if KeyChar = '3' then
      result := faPlace3
    else if KeyChar = '4' then
      result := faPlace4
    else if KeyChar = '5' then
      result := faPlace5
    else if KeyChar = '6' then
      result := faPlace6
    else if KeyChar = '7' then
      result := faPlace7
    else if KeyChar = '8' then
      result := faPlace8
    else if KeyChar = '9' then
      result := faPlace9
    else if KeyChar = '0' then
      result := faPlace0

    else if KeyChar = '*' then
      result := faActionPageM
    else if KeyChar = '+' then
      result := faActionPageP
    else if KeyChar = '#' then

    else if KeyChar = '=' then

    else if KeyChar = '?' then

    else if KeyChar = '|' then

    else if KeyChar = '(' then
      result := faCycleColorSchemeM
    else if KeyChar = ')' then
      result := faCycleColorSchemeP

    else if KeyChar = 'a' then
      result := faPlace10
    else if KeyChar = 'A' then
      result := faPlace10
    else if KeyChar = 'b' then
      result := faPlace11
    else if KeyChar = 'B' then
      result := faPlace11
    else if KeyChar = 'c' then
      result := faPlace12
    else if KeyChar = 'C' then
      result := faPlace12

    else if KeyChar = 'd' then
      result := faPlace13
    else if KeyChar = 'D' then
      result := faPlace13

    else if KeyChar = 'e' then
      result := faPlace14
    else if KeyChar = 'E' then
      result := faPlace14

    else if KeyChar = 'f' then
      result := faPlace15
    else if KeyChar = 'F' then
      result := faPlace15

    else if KeyChar = 'g' then

    else if KeyChar = 'G' then

    else if KeyChar = 'h' then

    else if KeyChar = 'H' then

    else if KeyChar = 'i' then
      result := faWheelRight
    else if KeyChar = 'I' then
      result := faWheelUp

    else if KeyChar = 'j' then
      result := faWheelLeft
    else if KeyChar = 'J' then
      result := faWheelDown

    else if KeyChar = 'k' then

    else if KeyChar = 'K' then

    else if KeyChar = 'l' then

    else if KeyChar = 'L' then

    else if KeyChar = 'm' then

    else if KeyChar = 'M' then

    else if KeyChar = 'n' then

    else if KeyChar = 'N' then

    else if KeyChar = 'o' then

    else if KeyChar = 'O' then

    else if KeyChar = 'p' then

    else if KeyChar = 'P' then

    else if KeyChar = 'q' then

    else if KeyChar = 'Q' then

    else if KeyChar = 'r' then

    else if KeyChar = 'R' then

    else if KeyChar = 's' then

    else if KeyChar = 'S' then

    else if KeyChar = 't' then

    else if KeyChar = 'T' then

    else if KeyChar = 'u' then

    else if KeyChar = 'U' then

    else if KeyChar = 'v' then

    else if KeyChar = 'V' then

    else if KeyChar = 'w' then

    else if KeyChar = 'W' then

    else if KeyChar = 'x' then

    else if KeyChar = 'X' then

    else if KeyChar = 'y' then

    else if KeyChar = 'Y' then

    else if KeyChar = 'z' then

    else if KeyChar = 'Z' then

    else if KeyChar = ' ' then

    else if Key = vkSpace then

    else if Key = vkDelete then

    else if Key = vkReturn then

    else if Key = vkF1 then
      result := faShowMemo

  end;
end;

end.
