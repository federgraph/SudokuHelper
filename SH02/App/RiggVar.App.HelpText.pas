unit RiggVar.App.HelpText;

{ This file need to be saved as UTF-8 with BOM, so that it can be managed (merged) with tool. }

interface

uses
  System.Classes;

procedure GetHelpText(ML: TStrings);

implementation

procedure GetHelpText(ML: TStrings);
begin
  ML.Clear;
  ML.Add('# SudokuHelper Readme');
  ML.Add('');
  ML.Add('Copyright © 2021 by Dr. Peter Below, see GitHub repository.');
  ML.Add('Adapted for SH02 by Federgraph, see forked GitHub repository.');
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
  ML.Add('- A mark can be set for the current undo stack state.');
  ML.Add('- The Sudoku can be saved to file, including the undo stack.');
  ML.Add('- The current state can be written to text format, excluding the undo stack.');
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
  ML.Add('When mouse is available you can use wheel and shift wheel,');
  ML.Add('  or use left button click when in click mode SetFocus.');
  ML.Add('');
  ML.Add('On a touch screen device you can use bottom and right touch bar,');
  ML.Add('  of the button frame instead of mouse wheel,');
  ML.Add('  or simply tap a cell when in click mode SetFocus.');
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
  ML.Add('First make sure to set the click mode with on of the yellow frame buttons,');
  ML.Add('  then click on one of the numbered buttons to SELECT the value,');
  ML.Add('  followed by a click on a cell in the grid to PLACE the value.');
  ML.Add('');
  ML.Add('Value 0 will clear the cell.');
  ML.Add('');
  ML.Add('Only empty cells can have candidates.');
  ML.Add('  To set candidates with the left mouse button');
  ML.Add('  make sure the SetCandidates click mode is active.');
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
end;

end.
