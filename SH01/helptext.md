# SudokuHelper

Copyright © 2021 by Dr. Peter Below

SudokuHelper is an application that acts like an electronic Sudoku grid. It supports 9x9, 12x12, and 16x16 Sudokus, both in the classic and Gosu variant, where cells can be marked to only accept even numbers. The application neither creates Sudokus itself nor provides a solver for them; it is just a more convenient way to solve a Sudoku from a magazine or other external source than doing it on paper, using pencil and eraser.

The application's main features are:
- Invalid cell values are marked in red.
- Candidate values can be added and removed from a cell. Setting a cell's value will automatically remove candidates no longer possible in other cells.
- All actions can be undone, the undo stack is only limited by available memory.
- Named marks can be set for the current undo stack state and one can later restore the stack to such a named mark.
- The complete Sudoku can be saved to file, including the undo stack, and later loaded again from such a file.

<a href="../images/SH01-01.png">*Screenshot of SH01*<br>
![SH01 screenshot](../images/SH01-01.png)</a>

## The keyboard interface

### Navigating the Sudoku grid

By default the input focus will be on the Sudoku grid; the active cell is marked in yellow (or blue for a Gosu cell). To navigate around the grid use the cursor keys to move one cell up, down, left or right. In addition:

- **[HOME]** moves to the first cell in the row
- **[END]** moves to the last cell in the row
- **[PageUp]** moves to the top cell in the column
- **[PageDown]** moves to the bottom cell in the column

### Setting values and candidates

To set a cell's value just type the value. 0 will clear the cell, 1 to 9 set the cell value. For 12x12 and 16x16 Sudokus the letters A to G will set the values 10 to 16, the characters are not case-sensitive. For a Gosu-type Sudoku the spacebar will toggle the active cells "even values only" state.

To set a candidate hold down the Alt key while typing; to remove a candidate use the Ctrl key instead. Candidates can only be set on an empty cell.

## The mouse interface

On the right of the Sudoku grid you see a panel with buttons. The buttons on the lower part of the panel control how a mouse click on a cell of the grid acts, the upper part of the panel holds buttons that act on the whole Sudoku.

### Setting values and candidates

Click on one of the numbered buttons in the lower part of the panel to select the value (the button stays "down"), then click on the cell of the grid you want to put the value into, using the left mouse button. Using 0 as value clears the cell.

To set or remove a candidate first click the value as above, then the action to perform (from the three bottommost buttons), then click on an empty cell using the right mouse button. Right clicks also work together with the modifier keys Alt (set a candidate) and Ctrl (remove a candidate). These also set the matching button to down and it stays down after you release the key!

## The control buttons

The buttons on the top part of the right-hand panel perform the following functions:

- **[New Sudoku]** Shows a list of the Sudoku types the application can handle. Select the one you want and click OK. A new empty Sudoku is created and both grid and value buttons are adjusted as needed.
- **[Save Sudoku]** A File Save dialog is shown. It remembers the last folder you saved a Sudoku to or loaded one from. Enter a filename and click the dialog's Save button to store the current Sudoku, including the Undo stack, to the file.
- **[Load Sudoku]** A File Open dialog is shown. It remembers the last folder you saved a Sudoku to or loaded one from. Pick a filename and click the dialog's Open button to replace the current Sudoku, including the Undo stack, with the one saved to the file.
- **[Clear stack]** Discards all items on the Undo stack, including all stack marks. The button is only enabled if the stack is not empty.
- **[Undo]** Undoes the last user action that changed the Sudoku's content, including the candidates. The button is only enabled if the stack is not empty.
- **[Set Mark]** Pops up a simple dialog where you can enter a name for the mark to create. It then represents the current state of the undo stack.
- **[Revert to Mark]** Pops up a list of the defined stack marks. Select the one you want to revert to and click OK. The application then undoes all changes done after the mark was set. The button is only enabled if there is at least one stack mark defined.