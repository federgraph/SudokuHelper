program SH01;

uses
  Forms,
  SH_MainU in 'App\SH_MainU.pas' {FormMain},
  SH_SelectFromListDlgU in 'App\SH_SelectFromListDlgU.pas' {SelectFromListDlg},
  SH_SelectMarkDlgU in 'App\SH_SelectMarkDlgU.pas' {SelectMarkDlg},
  SH_SelectSudokuDlgU in 'App\SH_SelectSudokuDlgU.pas' {SelectSudokuDlg},
  FrmMemo in 'App\FrmMemo.pas' {FormMemo},
  SH.Exceptions in 'SH\SH.Exceptions.pas',
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
  SH.SudokuHelper16x16 in 'SH\SH.SudokuHelper16x16.pas',
  SH.HelpText in 'SH\SH.HelpText.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Sudoku Helper';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
