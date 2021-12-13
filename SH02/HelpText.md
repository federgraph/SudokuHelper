# SudokuHelper

Copyright © 2021 by Dr. Peter Below

**SudokuHelper** is an application that acts like an electronic Sudoku grid.
It supports 9x9, 12x12, and 16x16 Sudokus, both in the classic and Gosu variant,
where cells can be marked to only accept even numbers.
The application neither creates Sudokus itself nor provides a solver for them;
it is just a more convenient way to solve a Sudoku from a magazine
or other external source than doing it on paper,
using pencil and eraser.

The application's main features are:
- Invalid cell values are marked in red. 
- Candidate values can be added and removed from a cell. Setting a cell's value
will automatically remove candidates no longer possible in other cells. 
- All actions can be undone, the undo stack is only limited by available memory. 
- Named marks can be set for the current undo stack state
and one can later restore the stack to such a named mark. 
- The complete Sudoku can be saved to file, including the undo stack,
and later loaded again from such a file. 

## Keyboard interface

### Navigating the Grid via keyboard

The active cell is marked in yellow (or blue for a Gosu cell).

To navigate around the grid use the cursor keys to move one cell up, down, left or right.

In addition:
- **HOME** moves to the first cell in the row.
- **END** moves to the last cell in the row.
- **PageUp** moves to the top cell in the column.
- **PageDown** moves to the bottom cell in the column.

### Setting cell values via keyboard

To set a cell's value just type the value.
- 0 will clear the cell,
- 1 to 9 set the cell value.

For 12x12 and 16x16 Sudokus the letters A to G will set the values 10 to 16,
these characters are not case-sensitive.

### How to toggle Even-Values-Only cell state

> Feature not yet implemented in FMX app.

For Gosu-type Sudokus **spacebar** should toggle the active cells *even values only* state. 

### Setting candidates

> Feature not yet implemented in FMX app.

To set a candidate hold down the Alt key while typing.
To remove a candidate use the Ctrl key instead.

Candidates can only be set on an empty cell.

## Mouse interface

SH01 VCL:
On the right of the Sudoku grid you see a panel with buttons.
Buttons near top of panel act on the whole Sudoku.
Button in the middle select normal values.
Buttons near bottom of panel are setting the mode, how a mouse click on a cell of the grid is interpreted.

SH02 FMX:
You should see a frame of buttons around the border of the main window.
Some buttons will select the value (to be used next),
some buttons will set the mode of operation,
and some buttons, probably located on page two, should act on the whole Sudoku.

### Setting values

Using the left mouse button,
first click on one of the numbered buttons to *select* the value,
then click on a cell in the grid to *place* the value.

Using 0 as value should clear the cell.

 ### Setting candidates

For *candidates* it should go like this:
- click the value as above,
- make sure the *mode of operation* is set appropriately,
- then click on an empty cell with the right mouse button.

Right mouse button clicks should work together with the modifier keys Alt (set a candidate) and Ctrl (remove a candidate).

The currently active mode should be reflected in the buttons down state.

## Control buttons

In the VCL app these *buttons* are located on the top part of the right-hand panel and perform the following functions:

- **New Sudoku** shows a list of the Sudoku types the application can handle.
Select the one you want and click OK.
A new empty Sudoku is created and both grid and value buttons are adjusted as needed. 
- **Save Sudoku** should bring up a File Save dialog.
It should remember the last folder you saved a Sudoku to, or loaded one from.
Enter a filename and click the dialog's Save button to store the current Sudoku,
including the Undo stack, to the file. 
- **Load Sudoku** should bring up a File Open dialog.
It should remember the last folder you saved a Sudoku to or loaded one from.
Pick a filename and click the dialog's Open button to replace the current Sudoku,
including the Undo stack, with the one saved to the file. 
- **Clear stack** action should discards all items on the Undo stack,
including all stack marks.
The action should only be enabled if the stack is not empty.
- **Undo** action should undo the last user action that changed the Sudoku's content,
including the candidates.
This action should be enabled only if the stack is not empty. 
- **Set Mark** should pop up a simple dialog where you can enter a name for the mark to create.
It then represents the current state of the undo stack. 
- **Revert to Mark** should pops up a list of the defined stack marks.
Select the one you want to revert to and click OK.
The application should then undo all changes done after the mark was set.
This action should only be enabled if there is at least one stack mark defined.