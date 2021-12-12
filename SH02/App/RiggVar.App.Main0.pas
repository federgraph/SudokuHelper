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
  RiggVar.FB.ActionMap,
  RiggVar.FB.ActionKeys,
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
  SH.SudokuGraph;

type
  TSudokuHostForm = class(TInterfacedObject, ISudokuHostForm)
  private
    function GetCurrentCandidate: TSudokuValue;
    function GetCurrentValue: TSudokuValue;
    function GetRightClickAction: TRightClickAction;
    function GetModifierkeys: TShiftstate;
  public
    property CurrentCandidate: TSudokuValue read GetCurrentCandidate;
    property CurrentValue: TSudokuValue read GetCurrentValue;
    property Modifierkeys: TShiftstate read GetModifierkeys;
    property RightClickAction: TRightClickAction read GetRightClickAction;
  end;

  TMain0 = class
  private
    FIsUp: Boolean;
    FTouch: Integer;
    FSudoku: ISudokuHelper;
    FCurrentValue: Integer;
    procedure SetIsUp(const Value: Boolean);
    procedure InitFederText(ft: TFederTouch0);
    procedure InitRaster;
    function GetFederText: TFederTouchBase;
    function GetIsLandscape: Boolean;
    function GetIsPhone: Boolean;
    function GetIsPortrait: Boolean;
    function GetTouchbarLayout: Integer;
    procedure InitText;
    procedure InitTouch;
    procedure SetColorScheme3D(const Value: Integer);
    procedure SetTouch(const Value: Integer);
    procedure SetTouchbarLayout(const Value: Integer);
    function GetColorScheme3D: Integer;
    procedure InitializeSudoku;
    procedure SetCurrentValue(const Value: Integer);
  public
    SudokuHostForm: ISudokuHostForm;
    BackgroundLock: Boolean;

    ActionHandler: TFederActionHandler;
    ActionMapTablet: TActionMap;
    ActionMapPhone: TActionMap;
    Keyboard: TFederKeyboard01;
    FederText1: TFederTouch;
    FederText2: TFederTouchPhone;

    SudokuGrid: TSudokuGrid;
    SudokuGraph: TSudokuGraph;

    constructor Create;
    destructor Destroy; override;

    procedure CycleToolSet(i: Integer);
    procedure CycleColorSchemeM;
    procedure CycleColorSchemeP;

    procedure DoTouchbarLeft(Delta: single);
    procedure DoTouchbarTop(Delta: single);
    procedure DoTouchbarRight(Delta: single);
    procedure DoTouchbarBottom(Delta: single);

    procedure Init;
    procedure UpdateText;
    procedure UpdateTouch;
    procedure FederTextCheckState;

    procedure ExecuteAction(fa: Integer);
    function IsActionChecked(fa: Integer): Boolean;

    procedure InitFirstSudoku;
    procedure CreateSudokuHelper(const aName: string);
    procedure StartNew(Value: Integer);
    procedure HandleCharacter(AChar: Char);

    property ColorScheme3D: Integer read GetColorScheme3D write SetColorScheme3D;
    property IsPhone: Boolean read GetIsPhone;
    property IsLandscape: Boolean read GetIsLandscape;
    property IsPortrait: Boolean read GetIsPortrait;
    property Touch: Integer read FTouch write SetTouch;
    property TouchbarLayout: Integer read GetTouchbarLayout write SetTouchbarLayout;
    property FederText: TFederTouchBase read GetFederText;
    property IsUp: Boolean read FIsUp write SetIsUp;

    property Sudoku: ISudokuHelper read FSudoku write FSudoku;

    property CurrentValue: Integer read FCurrentValue write SetCurrentValue;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  RiggVar.FederModel.ActionMapPhone,
  RiggVar.FederModel.ActionMapTablet,
  SH.SudokuHelper;

constructor TMain0.Create;
begin
  Main := self;
  SudokuHostForm := TSudokuHostForm.Create;

  FCurrentValue := 1;
  FTouch := faTouchDesk;

  ActionMapTablet := TActionMapTablet.Create;
  ActionMapPhone := TActionMapPhone.Create;

  Keyboard := TFederKeyboard01.Create;

  InitRaster;

  TTouchBtn.WantHint := True;
  FederText1 := TFederTouch.Create(nil);
  FederText2 := TFederTouchPhone.Create(nil);

  ActionHandler := TFederActionHandler.Create;
end;

destructor TMain0.Destroy;
begin
  FederText1.Free;
  FederText2.Free;
  Keyboard.Free;
  ActionMapTablet.Free;
  ActionMapPhone.Free;
  ActionHandler.Free;
  FSudoku := nil;
  SudokuHostForm := nil;
  inherited;
end;

procedure TMain0.ExecuteAction(fa: Integer);
begin
  ActionHandler.Execute(fa);
end;

procedure TMain0.FederTextCheckState;
begin
  //
end;

procedure TMain0.SetIsUp(const Value: Boolean);
begin
  FIsUp := Value;
end;

procedure TMain0.Init;
begin
  InitRaster;
  InitText;
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

procedure TMain0.InitRaster;
begin
  MainVar.ClientWidth := FormMain.ClientWidth;
  MainVar.ClientHeight := FormMain.ClientHeight;
end;

procedure TMain0.InitText;
begin
  MainVar.ClientWidth := FormMain.ClientWidth;
  MainVar.ClientHeight := FormMain.ClientHeight;
  InitFederText(FederText1);
  InitFederText(FederText2);
  Touch := faTouchDesk;
end;

procedure TMain0.InitTouch;
begin
  InitRaster;
  FederText1.Visible := not IsPhone;
  FederText2.Visible := IsPhone;
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
    MainVar.ClientWidth := FormMain.ClientWidth;
    MainVar.ClientHeight := FormMain.ClientHeight;
    InitTouch;
    FederText.UpdateShape;
  end;
end;

function TMain0.GetFederText: TFederTouchBase;
begin
  case FTouch of
    faTouchTablet: result := FederText1;
    faTouchPhone: result := FederText2;
    faTouchDesk:
    begin
      if IsPhone then
        result := FederText2
      else
        result := FederText1;
    end;
    else
      result := FederText1;
  end;
end;

function TMain0.GetIsLandscape: Boolean;
begin
  result := FormMain.ClientWidth > FormMain.ClientHeight;
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
      MinCount := Min(FormMain.ContentHeight, FormMain.ContentWidth) div MainVar.Raster;
      MaxCount := Max(FormMain.ContentHeight, FormMain.ContentWidth) div MainVar.Raster;
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

function TMain0.GetColorScheme3D: Integer;
begin
  result := MainVar.ColorScheme.CurrentScheme;
end;

procedure TMain0.SetColorScheme3D(const Value: Integer);
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
    FederText1.Visible := False
  else case FTouch of
    faTouchTablet: FederText1.Visible := True;
    faTouchPhone: FederText1.Visible := False;
  else
      FederText1.Visible := not IsPhone;
  end;
  FederText2.Visible := not FederText1.Visible;

  FederText.UpdateShape;
end;

procedure TMain0.SetTouchbarLayout(const Value: Integer);
begin
  if Assigned(FederText) then
    FederText.TouchBarLayout := Value;
end;

procedure TMain0.DoTouchbarLeft(Delta: single);
begin
//  DoBigWheel(Delta);
end;

procedure TMain0.DoTouchbarTop(Delta: single);
begin
//  DoMM(fmkRZ, Delta, 0);
end;

procedure TMain0.DoTouchbarRight(Delta: single);
begin
//  DoSmallWheel(Delta);
end;

procedure TMain0.DoTouchbarBottom(Delta: single);
begin
//  DoZoom(-Delta / 20);
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
  i := ColorScheme3D;
  Dec(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.DefaultScheme := i;
  ColorScheme3D := i;
  BackgroundLock := l;
end;

procedure TMain0.CycleColorSchemeP;
var
  i: Integer;
  l: Boolean;
begin
  l := BackgroundLock;
  BackgroundLock := false;
  i := ColorScheme3D;
  Inc(i);
  if (i < 1) then
    i := MainConst.ColorSchemeCount;
  if i > MainConst.ColorSchemeCount then
    i := 1;

  MainVar.ColorScheme.DefaultScheme := i;
  ColorScheme3D := i;
  BackgroundLock := l;
end;

procedure TMain0.HandleCharacter(AChar: Char);
begin
  Sudoku.InputHandler.HandleCharacter(SudokuGrid.Col, SudokuGrid.Row, AChar);
end;

procedure TMain0.InitFirstSudoku;
begin
//  CreateSudokuHelper(AppMemory.LastSudoku);
  CreateSudokuHelper(CClassicSudoku9x9);
end;

procedure TMain0.CreateSudokuHelper(const aName: string);
begin
  Main.Sudoku := HelperRegistry.CreateInstance(aName);
  InitializeSudoku;
end;

procedure TMain0.InitializeSudoku;
begin
  SudokuGraph.DataStorage := Main.Sudoku.Data;
  Main.Sudoku.Display.InitializeGrid(SudokuGraph);
  Main.Sudoku.InputHandler.Initialize(SudokuHostForm);
//  ToggleGosuButton.Enabled := Main.Sudoku.IsGosu;
  AppMemory.LastSudoku := Main.Sudoku.Displayname;
  FormMain.Caption := string.Format(SMainformCaptionMask, [Main.Sudoku.Displayname]);
end;

procedure TMain0.StartNew(Value: Integer);
var
  LSudokuName: string;
begin
  case Value of
    12: LSudokuName := '12x12 Sudoku';
    16: LSudokuName := '16x16 Sudoku';
    else
      LSudokuName := CClassicSudoku9x9;
  end;
  CreateSudokuHelper(LSudokuName);
end;

procedure TMain0.SetCurrentValue(const Value: Integer);
begin
  if Value <= SudokuGrid.ColCount then
  begin
    FCurrentValue := Value;
    FederText.ST00.Caption := IntToStr(Value);
  end;
end;

{ TSudokuHostForm }

function TSudokuHostForm.GetCurrentCandidate: TSudokuValue;
begin
  result := 1;
end;

function TSudokuHostForm.GetCurrentValue: TSudokuValue;
begin
  result := 1;
  if Main.IsUp then
    result := Main.CurrentValue;
end;

function TSudokuHostForm.GetModifierkeys: TShiftstate;
begin
  result := [];
end;

function TSudokuHostForm.GetRightClickAction: TRightClickAction;
begin
  result := TRightClickAction.SetCandidate;
end;

end.
