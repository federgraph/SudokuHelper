unit FrmMemo;

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
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls;

type
  TFormMemo = class(TForm)
    SaveBtn: TButton;
    LoadBtn: TButton;
    TestBtn: TButton;
    HelpBtn: TButton;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    Margin: Integer;
    ML: TStringList;
    procedure RunTest;
    procedure WriteHelpText;
  end;

var
  FormMemo: TFormMemo;

implementation

{$R *.dfm}

uses
  SH_MainU,
  Generics.Collections;

procedure TFormMemo.FormCreate(Sender: TObject);
begin
  Width := 800;
  Height := 800;

  Margin := 10;
  ML := TStringList.Create;

  SaveBtn.Top := Margin;
  SaveBtn.Left := 3 * Margin;
  SaveBtn.Caption := 'Save Text';

  LoadBtn.Top := Margin;
  LoadBtn.Left := SaveBtn.Left + SaveBtn.Width + Margin;
  LoadBtn.Caption := 'Load Text';

  TestBtn.Top := Margin;
  TestBtn.Left := LoadBtn.Left + LoadBtn.Width + Margin;
  TestBtn.Caption := 'Run Test';

  HelpBtn.Top := Margin;
  HelpBtn.Left := TestBtn.Left + TestBtn.Width + Margin;
  HelpBtn.Caption := 'Show Help';

  Memo.Left := Margin;
  Memo.Top := SaveBtn.Top + SaveBtn.Height + Margin;
  Memo.ScrollBars := System.UITypes.TScrollStyle.ssBoth;
  Memo.Width := ClientWidth - Memo.Left - Margin;
  Memo.Height := ClientHeight - Memo.Top - Margin;
  Memo.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
  Memo.Font.Name := 'Consolas';
  Memo.Font.Size := 12;
  Memo.Font.Color := clBlue;
end;

procedure TFormMemo.FormDestroy(Sender: TObject);
begin
  ML.Free;
end;

procedure TFormMemo.SaveBtnClick(Sender: TObject);
begin
  ML.Clear;
  FormMain.Sudoku.Data.SaveToML(ML);
  Memo.Lines.Text := ML.Text;
end;

procedure TFormMemo.TestBtnClick(Sender: TObject);
begin
  RunTest;
end;

procedure TFormMemo.LoadBtnClick(Sender: TObject);
begin
  ML.Text := Memo.Lines.Text;
  FormMain.Sudoku.Data.LoadFromML(ML);
  FormMain.Sudoku.Display.Refresh;
end;

procedure TFormMemo.RunTest;
var
  LList: TStack<Integer>;
  I: Integer;
  LArray: TArray<Integer>;
  SB: TStringbuilder;
begin
  SB := TStringBuilder.Create(4096);
  try
    LList := TStack<Integer>.Create();
    try
      for I := 1 to 10 do
        LList.Push(I);
      SB.AppendLine('Enumerator sequence:');
      for I in LList do
        SB.AppendFormat('%d, ',[I]);
      SB.AppendLine;
      SB.AppendLine('ToArray sequence:');
      LArray:= LList.ToArray;
      for I := Low(LArray) to High(LArray) do
        SB.AppendFormat('%d, ',[LArray[I]]);
      SB.AppendLine;
      SB.AppendLine('Pop sequence:');
      while LLIst.Count >0 do
        SB.AppendFormat('%d, ',[LList.Pop]);
      SB.AppendLine;
      Memo.Text := SB.ToString;
    finally
      LList.Free;
    end;
  finally
    SB.Free;
  end;
end;

procedure TFormMemo.HelpBtnClick(Sender: TObject);
begin
  WriteHelpText;
  Memo.Lines.Text := ML.Text;
end;

procedure TFormMemo.WriteHelpText;
begin
  ML.Clear;
  ML.Add('# SudokuHelper Readme');
  ML.Add('');
  ML.Add('Copyright 2021 by Dr. Peter Below, see GitHub repository.');
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
  ML.Add('- A mark can be set for the current undo stack state.');
  ML.Add('- The Sudoku can be saved to a binary file, including the undo stack.');
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
  ML.Add('- 1 to 9 to set the cell value.');
  ML.Add('- a to g for values 10 to 16 (for 12x12 and 16x16 Sudokus)');
  ML.Add('');
  ML.Add('### Setting values and candidates with mouse');
  ML.Add('');
  ML.Add('To set a normal value:');
  ML.Add('  Click on one of the numbered buttons to SELECT the value,');
  ML.Add('  followed by a left click on a cell in the grid to PLACE the value.');
  ML.Add('');
  ML.Add('To set a candidate use right mouse button:');
  ML.Add('  1. Make sure the value is selected (button is down).');
  ML.Add('  2. Make sure the right click mode is set appropriately.');
  ML.Add('     You can use Shift or Control keys to set the right click mode.');
  ML.Add('  3. Then right click a cell.');
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
  ML.Add('  Save Text button will write the current state to the memo control.');
  ML.Add('  It should be possible to edit the text and load again.');
  ML.Add('  Also you should be able to copy text via clipboard and load.');
  ML.Add('  There should be one line for each cell in the following format:');
  ML.Add('  (column, row) = Value;Valid;EvenOnly;[Candidates]');
end;

end.
