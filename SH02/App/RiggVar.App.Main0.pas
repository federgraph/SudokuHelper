unit RiggVar.App.Main0;

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
  System.Math,
  FMX.Layouts,
  RiggVar.FB.Action,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionItem,
  RiggVar.FB.ActionMap,
  RiggVar.FB.ActionKeys,
  RiggVar.FB.ActionTest,
  RiggVar.FB.ActionGroups,
  RiggVar.FB.ActionHelper,
  RiggVar.FB.ActionLong,
  RiggVar.FB.Touch,
  RiggVar.FederModel.Keyboard01,
  RiggVar.FederModel.Action,
  RiggVar.FederModel.TouchBase,
  RiggVar.FederModel.TouchPhone,
  RiggVar.FederModel.Touch,
  SH.Memory,
  SH.Strings,
  SH.Interfaces,
  SH.HelperBase,
  SH.SudokuGrid,
  SH.SudokuGraph,
  SH.SudokuHelper,
  SH.SudokuMain;

type
  TSudokuHostForm = class(TInterfacedObject, ISudokuHostForm)
  private
    function GetCurrentCandidate: TSudokuValue;
    function GetCurrentValue: TSudokuValue;
    function GetClickAction: TClickAction;
    function GetModifierkeys: TShiftstate;
  public
    property CurrentCandidate: TSudokuValue read GetCurrentCandidate;
    property CurrentValue: TSudokuValue read GetCurrentValue;
    property Modifierkeys: TShiftstate read GetModifierkeys;
    property ClickAction: TClickAction read GetClickAction;
  end;

  TMain0 = class
  private
    FIsUp: Boolean;
    FTouch: Integer;
    FSudoku: ISudokuHelper;
    FCurrentValue: Integer;
    procedure InitFederText(ft: TFederTouch0);
    procedure InitRaster;
    function GetFederText: TFederTouchBase;
    function GetIsLandscape: Boolean;
    function GetIsPhone: Boolean;
    function GetIsPortrait: Boolean;
    function GetTouchbarLayout: Integer;
    function GetColorScheme: Integer;
    procedure InitText;
    procedure InitTouch;
    procedure SetIsUp(const Value: Boolean);
    procedure SetColorScheme(const Value: Integer);
    procedure SetTouch(const Value: Integer);
    procedure SetTouchbarLayout(const Value: Integer);
    procedure SetCurrentValue(const Value: Integer);
    function GetSetCandidatesButtonDown: Boolean;
    function GetToggleGosuButtonDown: Boolean;
    function GetUnsetCandidatesButtonDown: Boolean;
    procedure SetClickAction(const Value: TClickAction);
    function GetClickAction: TClickAction;
  public
    SudokuHostForm: ISudokuHostForm;
    BackgroundLock: Boolean;

    ActionHandler: TActionHelper;
    ActionGroupList: TActionGroupList;
    ActionTest: TActionTest;
    ActionItem: TActionItem;

    ActionMapTablet: TActionMap;
    ActionMapPhone: TActionMap;
    Keyboard: TFederKeyboard01;
    FederTextTablet: TFederTouch;
    FederTextPhone: TFederTouchPhone;

    SudokuGraph: TSudokuGraph; // injected, not owned
    SudokuMain: TSudokuMain;

    ToggleGosuButtonEnabled: Boolean;

    FClickAction: TClickAction;

    constructor Create;
    destructor Destroy; override;

    procedure CycleToolSet(i: Integer);
    procedure CycleColorSchemeM;
    procedure CycleColorSchemeP;

    procedure DoTouchbarLeft(Delta: single);
    procedure DoTouchbarTop(Delta: single);
    procedure DoTouchbarRight(Delta: single);
    procedure DoTouchbarBottom(Delta: single);

    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    procedure Init;
    procedure UpdateText;
    procedure UpdateTouch;
    procedure FederTextCheckState;
    procedure HandleAction(fa: TFederAction);
    procedure ExecuteAction(fa: Integer);
    function IsActionChecked(fa: Integer): Boolean;
    procedure CollectShortcuts(fa: Integer; ML: TStrings);
    procedure WriteHelpText(ML: TStrings);
    procedure RunTest01(ML: TStrings);

    procedure InitializeSudoku;
    procedure InitFirstSudoku;
    procedure CreateSudokuHelper(const aName: string);
    procedure StartNew(fa: TFederAction);
    procedure HandleCharacter(AChar: Char);

    property ColorScheme: Integer read GetColorScheme write SetColorScheme;
    property IsUp: Boolean read FIsUp write SetIsUp;
    property IsPhone: Boolean read GetIsPhone;
    property IsLandscape: Boolean read GetIsLandscape;
    property IsPortrait: Boolean read GetIsPortrait;
    property Touch: Integer read FTouch write SetTouch;
    property TouchbarLayout: Integer read GetTouchbarLayout write SetTouchbarLayout;
    property FederText: TFederTouchBase read GetFederText;

    property Sudoku: ISudokuHelper read FSudoku write FSudoku;
    property CurrentValue: Integer read FCurrentValue write SetCurrentValue;
    property ToggleGosuButtonDown: Boolean read GetToggleGosuButtonDown;
    property SetCandidatesButtonDown: Boolean read GetSetCandidatesButtonDown;
    property UnsetCandidatesButtonDown: Boolean read GetUnsetCandidatesButtonDown;
    property ClickAction: TClickAction read GetClickAction write SetClickAction;
  end;

implementation

uses
  System.Generics.Collections,
  FrmMain,
  RiggVar.App.Main,
  RiggVar.FederModel.ActionMapPhone,
  RiggVar.FederModel.ActionMapTablet;

{ TSudokuHostForm }

function TSudokuHostForm.GetCurrentCandidate: TSudokuValue;
begin
  result := 1;
  if Main.IsUp then
    result := Main.CurrentValue;
end;

function TSudokuHostForm.GetCurrentValue: TSudokuValue;
begin
  result := 1;
  if Main.IsUp then
    result := Main.CurrentValue;
end;

function TSudokuHostForm.GetModifierkeys: TShiftState;
begin
  result := MainVar.ShiftState;
end;

function TSudokuHostForm.GetClickAction: TClickAction;
begin
  Result := Main.ClickAction;
end;

{ TMain0 }

constructor TMain0.Create;
begin
  Main := self;
  SudokuHostForm := TSudokuHostForm.Create;
  SudokuMain := TSudokuMain.Create;

  FClickAction := TClickAction.SetValue;
  FCurrentValue := 1;
  FTouch := faTouchDesk;

  ActionMapTablet := TActionMapTablet.Create;
  ActionMapPhone := TActionMapPhone.Create;

  Keyboard := TFederKeyboard01.Create;

  InitRaster;

  TTouchBtn.WantHint := True;
  FederTextTablet := TFederTouch.Create(nil);
  FederTextPhone := TFederTouchPhone.Create(nil);

  ActionHandler := TActionHelper.Create;
  ActionGroupList := TActionGroupList.Create;
  ActionTest := TActionTest.Create;
  ActionItem := TActionItem.Create(faNoop);
end;

destructor TMain0.Destroy;
begin
  FederTextTablet.Free;
  FederTextPhone.Free;
  Keyboard.Free;
  ActionMapTablet.Free;
  ActionMapPhone.Free;
  ActionHandler.Free;
  ActionGroupList.Free;
  ActionTest.Free;
  ActionItem.Free;

  FSudoku := nil;
  SudokuHostForm := nil;
  SudokuMain.Free;

  inherited;
end;

procedure TMain0.HandleAction(fa: TFederAction);
begin
  case fa of
    faNoop: ;
    else
      FormMain.HandleAction(fa);
   end;
end;

procedure TMain0.ExecuteAction(fa: TFederAction);
begin
  ActionHandler.Execute(fa);
end;

procedure TMain0.FederTextCheckState;
begin
  FederText.CheckState;
end;

procedure TMain0.SetIsUp(const Value: Boolean);
begin
  FIsUp := Value;
end;

procedure TMain0.InitRaster;
begin
  MainVar.ClientWidth := FormMain.ClientWidth;
  MainVar.ClientHeight := FormMain.ClientHeight;
end;

procedure TMain0.Init;
begin
  InitText;
end;

procedure TMain0.InitText;
begin
  InitRaster;
  InitFederText(FederTextTablet);
  InitFederText(FederTextPhone);
end;

procedure TMain0.InitFederText(ft: TFederTouch0);
begin
  if ft is TLayout then
  begin
    ft.Parent := FormMain;
    TFederTouchBase.OwnerComponent := ft;
    TFederTouchBase.ParentObject := ft;
  end
  else
  begin
    TFederTouchBase.OwnerComponent := FormMain;
    TFederTouchBase.ParentObject := FormMain;
  end;

  ft.Position.X := 0;
  ft.Position.Y := 0;
  ft.Width := MainVar.ClientWidth;
  ft.Height := MainVar.ClientHeight;
  ft.Init;
end;

procedure TMain0.InitTouch;
begin
  InitRaster;
  FederTextTablet.Visible := not IsPhone;
  FederTextPhone.Visible := IsPhone;
end;

function TMain0.IsActionChecked(fa: Integer): Boolean;
begin
  result := ActionHandler.GetChecked(fa);
end;

procedure TMain0.UpdateText;
begin
  FederText.UpdateText;
end;

procedure TMain0.UpdateTouch;
begin
  if Assigned(FederText) and FederText.InitOK then
  begin
    InitRaster;
    InitTouch;
    FederText.UpdateShape;
  end;
end;

function TMain0.GetFederText: TFederTouchBase;
begin
  case FTouch of
    faTouchTablet: result := FederTextTablet;
    faTouchPhone: result := FederTextPhone;
    faTouchDesk:
    begin
      if IsPhone then
        result := FederTextPhone
      else
        result := FederTextTablet;
    end;
    else
      result := FederTextTablet;
  end;
end;

function TMain0.GetIsLandscape: Boolean;
begin
  result := MainVar.ClientWidth > MainVar.ClientHeight;
end;

function TMain0.GetIsPhone: Boolean;
var
  MinCount, MaxCount: Integer;
begin
  case FTouch of
    faTouchPhone: result := True;
    faTouchTablet: result := False;
    else
    begin
      MinCount := Min(MainVar.ClientHeight, MainVar.ClientWidth) div MainVar.Raster;
      MaxCount := Max(MainVar.ClientHeight, MainVar.ClientWidth) div MainVar.Raster;
      result  := (MinCount < 8) or (MaxCount < 12);
    end;
  end;
end;

function TMain0.GetIsPortrait: Boolean;
begin
  result := not IsLandscape;
end;

function TMain0.GetTouchbarLayout: Integer;
begin
  if Assigned(FederText) then
    result := FederText.TouchBarLayout
  else
    result := 1;
end;

function TMain0.GetClickAction: TClickAction;
begin
  { keyboard keys have priority }
  if ssAlt in MainVar.ShiftState then
    Result := TClickAction.SetCandidate
  else if ssCtrl in MainVar.ShiftState then
    Result := TClickAction.UnsetCandidate
  else
    Result := FClickAction;
end;

function TMain0.GetColorScheme: Integer;
begin
  result := MainVar.ColorScheme.CurrentScheme;
end;

procedure TMain0.SetClickAction(const Value: TClickAction);
begin
  FClickAction := Value;
  if not ToggleGosuButtonEnabled and (Value = TClickAction.ToggleGosu) then
  begin
    FClickAction := TClickAction.SetFocus;
  end
end;

procedure TMain0.SetColorScheme(const Value: Integer);
begin
  if not BackgroundLock then
  begin
    MainVar.ColorScheme.CurrentScheme := Value;
    MainVar.ColorScheme.Init(Value);
    FormMain.UpdateBackground;
    FederText.UpdateColorScheme;
  end;
end;

procedure TMain0.SetTouch(const Value: Integer);
begin
  FTouch := Value;
  if IsPhone then
    FederTextTablet.Visible := False
  else case FTouch of
    faTouchTablet: FederTextTablet.Visible := True;
    faTouchPhone: FederTextTablet.Visible := False;
  else
      FederTextTablet.Visible := not IsPhone;
  end;
  FederTextPhone.Visible := not FederTextTablet.Visible;

  FederText.UpdateShape;
end;

procedure TMain0.SetTouchbarLayout(const Value: Integer);
begin
  if Assigned(FederText) then
    FederText.TouchBarLayout := Value;
end;

procedure TMain0.DoTouchbarLeft(Delta: single);
begin

end;

procedure TMain0.DoTouchbarTop(Delta: single);
begin

end;

procedure TMain0.DoTouchbarRight(Delta: single);
var
  d: single;
begin
  if Delta > 0 then
    d := 1
  else
    d := -1;
  DoBigWheel(-d);
end;

procedure TMain0.DoTouchbarBottom(Delta: single);
var
  d: single;
begin
  if Delta > 0 then
    d := 1
  else
    d := -1;
  DoSmallWheel(-d);
end;

procedure TMain0.CycleToolSet(i: Integer);
begin
  FederText.UpdateToolSet(i);
end;

procedure TMain0.CycleColorSchemeM;
var
  i: Integer;
  l: Boolean;
begin
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Dec(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.DefaultScheme := i;
  ColorScheme := i;
  BackgroundLock := l;
end;

procedure TMain0.CycleColorSchemeP;
var
  i: Integer;
  l: Boolean;
begin
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme;
  Inc(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.DefaultScheme := i;
  ColorScheme := i;
  BackgroundLock := l;
end;

procedure TMain0.HandleCharacter(AChar: Char);
begin
  Sudoku.InputHandler.HandleCharacter(SudokuGraph.Col, SudokuGraph.Row, AChar);
end;

procedure TMain0.InitFirstSudoku;
begin
//  CreateSudokuHelper(AppMemory.LastSudoku);
  CreateSudokuHelper(GetFederActionLong(faSudoku09A));
end;

procedure TMain0.CreateSudokuHelper(const aName: string);
begin
  Sudoku := HelperRegistry.CreateInstance(aName);
  InitializeSudoku;
end;

procedure TMain0.InitializeSudoku;
begin
  SudokuGraph.DataStorage := Sudoku.Data;
  Sudoku.Display.InitializeGrid(SudokuGraph);
  Sudoku.InputHandler.Initialize(SudokuHostForm);
  ToggleGosuButtonEnabled := Sudoku.IsGosu;
  AppMemory.LastSudoku := Sudoku.Displayname;
  FormMain.Caption := string.Format(SMainformCaptionMask, [Sudoku.Displayname]);

  FCurrentValue := 1;
  FederText.ST00.Caption := IntToStr(FCurrentValue);
  FederText.ActionPage := 1;
end;

procedure TMain0.StartNew(fa: TFederAction);
var
  s: string;
begin
  s := ActionHandler.GetCaption(fa);
  CreateSudokuHelper(s);
end;

procedure TMain0.SetCurrentValue(const Value: Integer);
begin
  if Value <= SudokuGraph.ColCount then
  begin
    FCurrentValue := Value;
    FederText.ST00.Caption := IntToStr(Value);
  end;
end;

function TMain0.GetSetCandidatesButtonDown: Boolean;
begin
  result := ClickAction = TClickAction.SetCandidate;
end;

function TMain0.GetToggleGosuButtonDown: Boolean;
begin
  result := ClickAction = TClickAction.ToggleGosu;
end;

function TMain0.GetUnsetCandidatesButtonDown: Boolean;
begin
  result := ClickAction = TClickAction.UnsetCandidate;
end;

procedure TMain0.DoBigWheel(Delta: single);
begin
  SudokuGraph.NavRow(Round(Delta));
end;

procedure TMain0.DoSmallWheel(Delta: single);
begin
  SudokuGraph.NavCol(Round(Delta));
end;

procedure TMain0.CollectShortcuts(fa: Integer; ML: TStrings);
begin
  Keyboard.GetShortcuts(fa, ML);
  ActionMapTablet.CollectOne(fa, ML);
  ActionMapPhone.CollectOne(fa, ML);
//  FederMenu.CollectOne(fa, ML);
end;

procedure TMain0.WriteHelpText(ML: TStrings);
begin
  ML.Add('# SudokuHelper Readme');
  ML.Add('');
  ML.Add('Copyright © 2021 by Dr. Peter Below, see GitHub repository.');
  ML.Add('Adapted for SH02 by Federgraph, see forked GitHub repository.');
  ML.Add('');
  ML.Add('SudokuHelper is an application that acts like an electronic Sudoku grid.');
  ML.Add('');
  ML.Add('It supports 9x9, 12x12, and 16x16 Sudokus,');
  ML.Add('  both in the classic and Gosu variant,');
  ML.Add('  where cells can be marked to only accept even numbers.');
  ML.Add('');
  ML.Add('The application neither creates Sudokus itself');
  ML.Add('  nor provides a solver for them.');
  ML.Add('  It is just a more convenient way to solve a Sudoku');
  ML.Add('  from a magazine or other external source,');
  ML.Add('  than doing it on paper, using pencil and eraser.');
  ML.Add('');
  ML.Add('The application''s main features are:');
  ML.Add('- Invalid cell values are marked in red. ');
  ML.Add('- Candidate values can be added and removed from a cell.');
  ML.Add('- All actions can be undone, the undo stack is only limited by available memory. ');
//  ML.Add('- Named marks can be set for the current undo stack state.');
  ML.Add('- The Sudoku can be saved to file, including the undo stack.');
  ML.Add('');
  ML.Add('## Basics of operation');
  ML.Add('');
  ML.Add('### Navigating the Grid');
  ML.Add('');
  ML.Add('The active cell is marked in yellow, or blue (aqua) for a Gosu cell.');
  ML.Add('');
  ML.Add('When keyboard is available:');
  ML.Add('- Use cursor keys to move one cell up, down, left or right');
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
  ML.Add('To set a cell''s value just type the value.');
  ML.Add('- 0 to clear the cell,');
  ML.Add('- 1 to 9 to set the cell value.');
  ML.Add('');
  ML.Add('For 12x12 and 16x16 Sudokus');
  ML.Add('  the letters A to G will set the values 10 to 16.');
  ML.Add('');
  ML.Add('### Setting values and candidates with mouse');
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
  ML.Add('On the desktop you can alternatively make use of the right mouse button,');
  ML.Add('  it should work together with modifier keys Alt (set candidate)');
  ML.Add('  or Ctrl (remove candidate).');
  ML.Add('  This should work independent of the selected click mode.');
  ML.Add('');
  ML.Add('## Sudoku commands');
  ML.Add('');
  ML.Add('These commands affect the whole Sudoku, not just a single cell.');
  ML.Add('  Some actions have been implemented and should work already:');
  ML.Add('');
  ML.Add('[Clear stack] should discard all items on the Undo stack,');
  ML.Add('  including all stack marks.');
  ML.Add('//The action should only be enabled if the stack is not empty.');
  ML.Add('');
  ML.Add('[Undo] should undo the last user action that changed the Sudoku''s content,');
  ML.Add('  including the candidates.');
  ML.Add('//This action should be enabled only if the stack is not empty.');
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
  ML.Add('Not implemented yet:');
  ML.Add('');
  ML.Add('[New Sudoku] shows a list of the Sudoku types the application can handle.');
  ML.Add('  Select the one you want and click OK.');
  ML.Add('  A new empty Sudoku is created and both grid and value buttons are adjusted as needed. ');
  ML.Add('');
  ML.Add('[Set Mark] should pop up a simple dialog where you can enter a name for the mark to create.');
  ML.Add('  It then represents the current state of the undo stack. ');
  ML.Add('');
  ML.Add('[Revert to Mark] should pop up a list of the defined stack marks.');
  ML.Add('  Select the one you want to revert to and click OK.');
  ML.Add('  The application should then undo all changes done after the mark was set.');
  ML.Add('  This action should only be enabled if there is at least one stack mark defined.');
end;

procedure TMain0.RunTest01(ML: TStrings);
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
      ML.Text := SB.ToString;
    finally
      LList.Free;
    end;
  finally
    SB.Free;
  end;
end;

end.
