program SH01;

{$R 'helptext.res' 'helptext.rc'}

uses
  Forms,
  SH_MainU in 'App\SH_MainU.pas' {FormMain},
  SH_SelectFromListDlgU in 'App\SH_SelectFromListDlgU.pas' {SelectFromListDlg},
  SH_SelectMarkDlgU in 'App\SH_SelectMarkDlgU.pas' {SelectMarkDlg},
  SH_SelectSudokuDlgU in 'App\SH_SelectSudokuDlgU.pas' {SelectSudokuDlg},
  SH_HelpviewerU in 'App\SH_HelpviewerU.pas' {HelpViewerForm},
  PB.CharactersU in 'PB\PB.CharactersU.pas',
  PB.CommonTypesU in 'PB\PB.CommonTypesU.pas',
  PB.InterlockedOpsU in 'PB\PB.InterlockedOpsU.pas',
  PB.WinSet in 'PB\PB.WinSet.pas',
  SH.Memory in 'SH\SH.Memory.pas',
  SH.Strings in 'SH\SH.Strings.pas',
  SH.Interfaces in 'SH\SH.Interfaces.pas',
  SH.SudokuFiler in 'SH\SH.SudokuFiler.pas',
  SH.HelperBase in 'SH\SH.HelperBase.pas',
  SH.DataStorageBase in 'SH\SH.DataStorageBase.pas',
  SH.InputHandlerBase in 'SH\SH.InputHandlerBase.pas',
  SH.DisplayHandlerBase in 'SH\SH.DisplayHandlerBase.pas',
  SH.SudokuHelper in 'SH\SH.SudokuHelper.pas',
  SH.SudokuHelper12x12 in 'SH\SH.SudokuHelper12x12.pas',
  SH.SudokuHelper16x16 in 'SH\SH.SudokuHelper16x16.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Sudoku Helper';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
