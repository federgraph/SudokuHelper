{!
<summary>
 This unit implements the base class for Sudoku input handlers.
 </summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2021-10-27<p>
 Last modified       2021-10-27<p>
</history>
<copyright>Copyright 2021 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>
<remarks>
 The input handler processes mouse and keyboard input coming from the user
 working on the main form, at least the part intended to change the content
 of the Sudoku's cells. The user can

 * Set the value of a Sudoku cell (0 clears the cell)
   * via keyboard by just typing a number. It will go into the currently
     selected cell of the display grid.
   * via mouse/touch by first selecting the value from a group of
     speedbuttons and then clicking on the cell the value is intended for.

 * Set or clear candidate values for a cell
   * via keyboard by typing the value while holding down Ctrl to clear
     a candidate and Alt to set it for the currently selected cell of the
     display grid..
   * via mouse/touch by first selecting the value from a group of
     speedbuttons, the action to perform (set/unset) from a second set
     of speedbuttons, and then clicking on the target cell.

 Setting values or candidates that would require two keystrokes is a bit
 of a problem. To make this a bit easier we accept heptadecimal input
 for values above 9, with A=10, B=11 etc. and G=16 to allow
 input with a single keystroke. Input is not case-sensitive.
</remarks>
}
unit SH.InputHandlerBase;


interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  SH.Interfaces;

type
  {!
  <summary>
   Base class for all Sudoku input handlers.</summary>
  <remarks>
   The class implements all functionality needed for a Sudoku to react to user
   input via mouse or keyboard. Descendants have to override the virtual
   methods CharToValue, IsAllowedValue, SetButtonSymbol and perhaps GetButtonSize
   if the defaults implement here do not fit their needs. The defaults
   should serve for Sudokus that display decimal numbers for values.

   The input handler will be created by the Sudoku helper and has to be
   initialized by the UI before it can do its work. During initialization
   the class will create the speedbuttons needed to select the values to
   input via mouse.

   Method comments are in the implementation section.  Properties are
   intended for the use of descendants only.
   </remarks>
  }
  TBaseSudokuInputHandler = class abstract(TInterfacedObject, ISudokuInputHandler)
  strict private
    FDataStorage: ISudokuData;
    FHost: ISudokuHostForm;
  strict protected
    function CharToValue(aChar: Char; var aValue: TSudokuValue): boolean; virtual;
    procedure HandleCellClick(aCol, aRow: Integer; aRightClick: Boolean = false);
    procedure HandleCharacter(aCol, aRow: Integer; aChar: Char);
    procedure HandleLeftClick(const aCol, aRow: TSudokuCellIndex);
    procedure HandleRightClick(const aCol, aRow: TSudokuCellIndex);
    procedure HandleValueOrCandidate(const LCell: ISudokuCell; aChar: Char); virtual;
    procedure Initialize(aHost: ISudokuHostForm);
    function IsAllowedValue(aChar: Char): Boolean; virtual;
    property Data: ISudokuData read FDataStorage;
    property Host: ISudokuHostForm read FHost;
  public
    constructor Create(const ADataStorage: ISudokuData); virtual;
  end;

implementation

uses
  System.Math,
  System.Character,
  PB.CommonTypesU,
  PB.CharactersU,
  SH.DataStorageBase;

constructor TBaseSudokuInputHandler.Create(const ADataStorage: ISudokuData);
begin
  inherited Create;
  FDataStorage := ADataStorage;
end;

{!
<summary>
 Deduce a Sudoku value from character input </summary>
<returns>
 true if a value could be determined and is valid for the Sudoku,
 false if not.</returns>
<param name="aChar">is the character entered</param>
<param name="aValue">returns the value</param>
<remarks>
 If the method returns false aValue should not be used by the
 caller.</remarks>
}
function TBaseSudokuInputHandler.CharToValue(aChar: Char; var aValue: TSudokuValue): Boolean;
begin
  Result := true;
  case aChar.ToUpper of
    '1'..'9': aValue := String.ToInteger(aChar);
    'A'..'G': aValue := Ord(aChar) - Ord('A') + 10;
  else
    Result := false;
  end;
  Result := Result and InRange(aValue, 1, Data.Bounds.MaxValue);
end;

{!
<summary>
 Implements ISudokuInputHandler.HandleCellClick </summary>
<param name="aCol">is the grid column of the clicked cell</param>
<param name="aRow">is the grid row of the clicked cell</param>
<param name="aRightClick">false indicates a click with the left
 mouse button, true a click with the right one.</param>
<remarks>
 Clicks with the left button set a cell's value, clicks with the right
 button set or unset candidates, if the cell is empty, or toggle the
 Gosu state of a cell. Which right-click action is taken depends on
 the state of some buttons on the main form. The value is also defined
 by a set of speed buttons on that form.
 </remarks>
}
procedure TBaseSudokuInputHandler.HandleCellClick(aCol, aRow: Integer;
    aRightClick: Boolean = false);
begin
  { Convert grid to Sudoku cell index first }
  Inc(aCol);
  Inc(aRow);
  if aRightClick then
    HandleRightClick(aCol, aRow)
  else
    HandleLeftClick(aCol, aRow);
end;

{!
<summary>
 Implements  ISudokuInputHandler.HandleCharacter
<param name="aCol">is the grid column of the active cell</param>
<param name="aRow">is the grid row of the active cell</param>
<param name="aChar">is the character entered</param>
<remarks>
 A character not valid for our purpose is ignored. The method will

 * set the cell value if aChar represents one of the valid Sudoku
   values (1..MaxValue) and neither Alt nor Ctrl are held down. If
   aChar is '1' and MaxValue is 10 or higher a delay mechanism is
   used to allow a second call to complete a two character sequence.
   Alternatively letters A to G can be used to input the values 10
   to 16.
 * clear the cell if aChar is '0'.
 * set a candidate if the cell is empty and Alt is down.
 * remove a candidate if the cell is empty and Ctrl is down.
 * toggle the Gosu state of the cell if aChar is a space.

</remarks>
}
procedure TBaseSudokuInputHandler.HandleCharacter(aCol, aRow: Integer; aChar: Char);
var
  LCell: ISudokuCell;
begin
  { Convert grid to Sudoku cell index first }
  Inc(aCol);
  Inc(aRow);
  LCell := Data.Cell[aCol, aRow];

  case aChar of
    Space: if Data.IsGosu then
             LCell.ToggleEvenOnly;
    '0': LCell.Clear;
  else
    if IsAllowedValue(aChar) then
      HandleValueOrCandidate(LCell, aChar);
  end;
end;

{!
<summary>
  Handle a left mouse click on a Sudoku cell  </summary>
<param name="aCol">is the column index of the cell</param>
<param name="aRow">is the row index of the  cell</param>
<remarks>
 A left click sets a cell's value, which is obtained from the host
 form. Setting a cell to the value it already contains clears the cell.
 </remarks>
}
procedure TBaseSudokuInputHandler.HandleLeftClick(const aCol, aRow: TSudokuCellIndex);
var
  LValue: TSudokuValue;
  LCell : ISudokuCell;
begin
  LCell := Data.Cell[aCol, aRow];
  LValue := Host.CurrentValue;
  if (LValue = 0) or (LValue = LCell.Value) then
    LCell.Clear
  else
    LCell.Value := LValue;
end;

{!
<summary>
  Handle a right mouse click on a Sudoku cell  </summary>
<param name="aCol">is the column index of the cell</param>
<param name="aRow">is the row index of the  cell</param>
<remarks>
 A right click is used to set or unset a candidate or toggle the
 Gosu state of a cell. The kind of action to take is obtained from the
 host form, where the user can select it via a group of speedbuttons.
 Note that trying to set or unset a candidate only does something
 if the cell is empty.
 </remarks>
}
procedure TBaseSudokuInputHandler.HandleRightClick(const aCol, aRow: TSudokuCellIndex);
var
  LAction: TRightClickAction;
  LValue: TSudokuValue;
  LCell: ISudokuCell;
begin
  LCell := Data.Cell[aCol, aRow];
  LValue := Host.CurrentCandidate;
  LAction := Host.RightClickAction;
  if (LValue = 0) and (LAction <> TRightClickAction.ToggleGosu)  then
    Exit; // 0 is not a valid candidate value!
  case LAction of
    TRightClickAction.SetCandidate:
      LCell.AddCandidate(LValue);
    TRightClickAction.UnsetCandidate:
      LCell.RemoveCandidate(LValue);
    TRightClickAction.ToggleGosu:
      LCell.ToggleEvenOnly;
  end;
end;

procedure TBaseSudokuInputHandler.HandleValueOrCandidate(const LCell: ISudokuCell; aChar: Char);
var
  LModifierKeys: TShiftstate;
  LValue: TSudokuValue;
begin
   if CharToValue(aChar, LValue) then begin
     LModifierKeys := Host.Modifierkeys;
     if ssAlt in LModifierKeys then
       LCell.AddCandidate(LValue)
     else
       if ssCtrl in LModifierKeys then
         LCell.RemoveCandidate(LValue)
       else
         LCell.Value := LValue
   end;
end;

{!
<summary>Implements ISudokuInputHandler.Initialize. </summary>
<param name="aHost">represents the main form, required</param>
<exception cref="EParameterCannotBeNil">is raised if aHost is nil </exception>
}
procedure TBaseSudokuInputHandler.Initialize(aHost: ISudokuHostForm);
begin
  if not Assigned(aHost) then
    raise EParameterCannotBeNil.Create(Classname + '.Initialize', 'aHost');
  FHost := aHost;
end;

{!
<summary>
 Check wheather the passed character is valid as input for a value of
 the current Sudoku.</summary>
<returns>
 true if the character is OK, false if not</returns>
<param name="aChar">is the character to check</param>
<remarks>
 Descendants can override this method to allow more characters as input.</remarks>
}
function TBaseSudokuInputHandler.IsAllowedValue(aChar: Char): Boolean;
begin
  Result := CharInSet(aChar, ['0'..'9']);
end;

end.
