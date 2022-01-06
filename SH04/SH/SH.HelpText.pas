unit SH.HelpText;

{ This file need to be saved as UTF-8 with BOM, so that it can be managed (merged) with tool. }

interface

uses
  Classes;

procedure GetHelpText(ML: TStrings);

implementation

procedure GetHelpText(ML: TStrings);
begin
  ML.Clear;
  ML.Add('# SudokuHelper Readme');
  ML.Add('');
  ML.Add('Copyright © 2021 by Dr. Peter Below, see GitHub repository.');
  ML.Add('Adapted by Federgraph, see forked GitHub repository.');
  ML.Add('');
  ML.Add('SudokuHelper acts like an electronic Sudoku grid.');
  ML.Add('');
  ML.Add('It supports 9x9, 12x12, and 16x16 Sudokus,');
  ML.Add('  both in the classic and Gosu variant,');
  ML.Add('    where cells can be marked to only accept even numbers.');
  ML.Add('');
  ML.Add('The application''s main features are:');
  ML.Add('- Invalid cell values are marked in red. ');
  ML.Add('- Candidate values can be added and removed from a cell.');
  ML.Add('- Actions can be undone. ');
  ML.Add('- Marks can be set for the current undo stack state.');
  ML.Add('- The Sudoku can be saved to a binary file, including the undo stack and marks.');
  ML.Add('- The current state can be saved to text format, excluding the undo stack.');
  ML.Add('');
  ML.Add('## Basics of operation');
  ML.Add('');
  ML.Add('### Navigating the Grid');
  ML.Add('');
  ML.Add('The active cell is marked in yellow, or blue (aqua) for a Gosu cell.');
  ML.Add('');
  ML.Add('When keyboard is available:');
  ML.Add('- Use cursor keys to move one cell up, down, left or right.');
  ML.Add('- HOME moves to the first cell in the row.');
  ML.Add('- END moves to the last cell in the row.');
  ML.Add('- PageUp moves to the top cell in the column.');
  ML.Add('- PageDown moves to the bottom cell in the column.');
  ML.Add('');
  ML.Add('### Setting cell values with keyboard');
  ML.Add('');
  ML.Add('To set a cell''s value just type the value:');
  ML.Add('- 0 to clear the cell,');
  ML.Add('- 1 to 9 to set the cell value');
  ML.Add('- a to g for values 10 to 16 (for 12x12 and 16x16 Sudokus)');
  ML.Add('- Space should toggle the Gosu state if appropriate');
  ML.Add('');
  ML.Add('### Setting cell state with mouse');
  ML.Add('');
  ML.Add('To set a normal value:');
  ML.Add('- Click on one of the numbered buttons to SELECT the value,');
  ML.Add('- followed by a left click on a cell in the grid to PLACE the value.');
  ML.Add('');
  ML.Add('To set a candidate use right mouse button:');
  ML.Add('  1. Make sure the value is selected (button is down).');
  ML.Add('  2. Make sure the right click mode is set appropriately.');
  ML.Add('     You can use Shift or Control keys to set the right click mode.');
  ML.Add('  3. Then right click a cell.');
  ML.Add('');
  ML.Add('To toggle the Gosu state of a cell:');
  ML.Add('- Right click on a cell when toggle Gosu button is down and enabled.');
  ML.Add('');
  ML.Add('## Sudoku commands');
  ML.Add('');
  ML.Add('[Clear stack] should discard all items on the Undo stack,');
  ML.Add('  including all stack marks.');
  ML.Add('  This action should be enabled if the stack is not empty.');
  ML.Add('');
  ML.Add('[Undo] should undo the last user action that changed the Sudoku''s content,');
  ML.Add('  including the candidates.');
  ML.Add('  This action should be enabled if the stack is not empty.');
  ML.Add('');
  ML.Add('[Save Sudoku] should bring up a File Save dialog.');
  ML.Add('  It should remember the last folder you saved a Sudoku to, or loaded one from.');
  ML.Add('  Enter a filename and click the dialog''s Save button to store the current Sudoku,');
  ML.Add('  including the Undo stack, to the file. ');
  ML.Add('');
  ML.Add('[Load Sudoku] should bring up a File Open dialog.');
  ML.Add('  It should remember the last folder you saved a Sudoku to or loaded one from.');
  ML.Add('  Pick a filename and click the dialog''s Open button to replace the current Sudoku,');
  ML.Add('  including the Undo stack, with the one saved to the file. ');
  ML.Add('');
  ML.Add('[Set Mark] should set a mark to remember');
  ML.Add('  the current state of the undo stack. ');
  ML.Add('');
  ML.Add('[Revert to Mark] should undo all changes done after the mark was set.');
  ML.Add('  This action should be enabled if the stack mark was set.');
  ML.Add('');
  ML.Add('[Show Memo] should show a secondary form with save and load buttons.');
  ML.Add('  Save button will write the current state to the memo.');
  ML.Add('  It should be possible to edit the text and load again.');
  ML.Add('  You should be able to paste text via the clipboard and load.');
  ML.Add('');
  ML.Add('## Sudoku text format');
  ML.Add('');
  ML.Add('Expect one line for each non-default cell in the following format:');
  ML.Add('  (column, row) = Value;Valid;EvenOnly;[Candidates]');
  ML.Add('Lines for cells in default state can be omitted.');
//  ML.Add('The type of the Sudoku need to be given in first line');
//  ML.Add('  if the text is loaded from file.');
end;

end.
