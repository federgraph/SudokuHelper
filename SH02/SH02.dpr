program SH02;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
  FrmAction in 'App\FrmAction.pas' {FormAction},
  FrmMemo in 'App\FrmMemo.pas' {FormMemo},
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Main0 in 'App\RiggVar.App.Main0.pas',
  PB.CharactersU in 'PB\PB.CharactersU.pas',
  PB.CommonTypesU in 'PB\PB.CommonTypesU.pas',
  PB.InterlockedOpsU in 'PB\PB.InterlockedOpsU.pas',
  SH.DataStorageBase in 'SH\SH.DataStorageBase.pas',
  SH.DisplayHandlerBase in 'SH\SH.DisplayHandlerBase.pas',
  SH.HelperBase in 'SH\SH.HelperBase.pas',
  SH.InputHandlerBase in 'SH\SH.InputHandlerBase.pas',
  SH.SudokuHelper in 'SH\SH.SudokuHelper.pas',
  SH.Memory in 'SH\SH.Memory.pas',
  SH.Strings in 'SH\SH.Strings.pas',
  SH.SudokuHelper12x12 in 'SH\SH.SudokuHelper12x12.pas',
  SH.SudokuHelper16x16 in 'SH\SH.SudokuHelper16x16.pas',
  SH.SudokuFiler in 'SH\SH.SudokuFiler.pas',
  SH.Interfaces in 'SH\SH.Interfaces.pas',
  SH.SudokuGraph in 'SH\SH.SudokuGraph.pas',
  SH.SudokuGrid in 'SH\SH.SudokuGrid.pas',
  SH.SudokuMain in 'SH\SH.SudokuMain.pas',
  RiggVar.FederModel.Touch in 'Model\RiggVar.FederModel.Touch.pas',
  RiggVar.FederModel.TouchBase in 'Model\RiggVar.FederModel.TouchBase.pas',
  RiggVar.FB.Action in 'FB\RiggVar.FB.Action.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.ActionGroup in 'FB\RiggVar.FB.ActionGroup.pas',
  RiggVar.FB.ActionGroups in 'FB\RiggVar.FB.ActionGroups.pas',
   RiggVar.FB.ActionHelper in 'FB\RiggVar.FB.ActionHelper.pas',
  RiggVar.FB.ActionItem in 'FB\RiggVar.FB.ActionItem.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionMap in 'FB\RiggVar.FB.ActionMap.pas',
  RiggVar.FB.ActionName in 'FB\RiggVar.FB.ActionName.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.ActionTest in 'FB\RiggVar.FB.ActionTest.pas',
  RiggVar.FB.ColorScheme in 'FB\RiggVar.FB.ColorScheme.pas',
  RiggVar.FB.Touch in 'FB\RiggVar.FB.Touch.pas',
  RiggVar.FD.Image in 'FD\RiggVar.FD.Image.pas',
  RiggVar.FB.ActionKeys in 'FB\RiggVar.FB.ActionKeys.pas',
  RiggVar.FederModel.TouchPhone in 'Model\RiggVar.FederModel.TouchPhone.pas',
  RiggVar.FederModel.Action in 'Model\RiggVar.FederModel.Action.pas',
  RiggVar.FederModel.ActionMapPhone in 'Model\RiggVar.FederModel.ActionMapPhone.pas',
  RiggVar.FederModel.ActionMapTablet in 'Model\RiggVar.FederModel.ActionMapTablet.pas',
  RiggVar.FederModel.Keyboard01 in 'Model\RiggVar.FederModel.Keyboard01.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
