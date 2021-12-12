unit FrmMain;

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
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes,
  System.Math,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Platform,
  FMX.Objects,
  RiggVar.FB.ColorScheme,
  RiggVar.FD.Image,
  SH.SudokuGraph;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    procedure SudokuImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    WantInitTimer: Boolean;
    InitTimer: TTimer;

    Raster: single;
    Margin: single;
    ComponentsCreated: Boolean;
    SudokuImage: TOriginalImage;

{$ifdef WantHintText}
    HintText: TText;
{$endif}

    FormShown: Boolean;
    InitTimerCalled: Boolean;

    procedure HandleShowHint(Sender: TObject);

    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);

    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    function GetIsUp: Boolean;
    procedure Init;
    procedure InitTimerTimer(Sender: TObject);
    procedure SetIsUp(const Value: Boolean);
    procedure InitSudokuGraph;
    procedure LayoutImages;
    procedure UpdateSudokuGraph;
    procedure CreateComponents;
    procedure HandleAction(fa: Integer);
    procedure LayoutComponents;
    procedure SetupText(T: TText; fs: single);
  public
    SudokuGraph: TSudokuGraph;
    procedure UpdateBackground;

    property IsUp: Boolean read GetIsUp write SetIsUp;
    property ContentWidth: Integer read GetContentWidth;
    property ContentHeight: Integer read GetContentHeight;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Action,
  RiggVar.FederModel.TouchBase;

{ TFormMain }

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
end;

procedure TFormMain.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if IsUp then
    if Assigned(Main) then
  Done := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormMain := self;

  Application.OnException := ApplicationEventsException;
  Application.OnIdle := ApplicationEventsIdle;
  ReportMemoryLeaksOnShutdown := True;

  Fill.Kind := TBrushKind.Solid; // because it is still TBrushKind.None
  Self.Position := TFormPosition.ScreenCenter;

  ClientWidth := 1024;
  ClientHeight := 900;

  Caption := UpperCase(Application.Title);

  WantInitTimer := False;
{$ifdef IOS}
  WantInitTimer := True;
{$endif}
  if WantInitTimer then
  begin
    InitTimer := TTimer.Create(Self);
    InitTimer.Interval := 500;
    InitTimer.OnTimer := InitTimerTimer;
    InitTimer.Enabled := True;
  end
  else
    Init;

  CreateComponents;

  Application.OnHint := HandleShowHint;

  OnShow := FormShow;
end;

procedure TFormMain.Init;
begin
  Main := TMain.Create;

  Main.Init;

  Main.Keyboard.KeyMapping := 1;

  IsUp := True;

  Main.FederText.ActionPage := 1;

  Main.ColorScheme3D := 4;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  MainVar.AppIsClosing := True;

  SudokuGraph.Free;

  Main.Free;
  Main := nil;
end;

procedure TFormMain.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled := False;
  if not IsUp then
    Init;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if (InitTimer <> nil) and not InitTimerCalled then
    Exit;

  if not FormShown then
  begin
    FormShown := True;

    LayoutComponents;
    LayoutImages;

    Main.InitFirstSudoku;
  end;
end;

procedure TFormMain.SetIsUp(const Value: Boolean);
begin
  Main.IsUp := Value;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  MainVar.BatchStopRequested := True;
  if IsUp then
    Main.ActionHandler.FormKeyUp(Sender, Key, KeyChar, Shift);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if IsUp then
  begin
    Main.UpdateTouch;
    Main.UpdateText;
    SudokuGraph.Draw;
  end;
end;

function TFormMain.GetContentHeight: Integer;
begin
  result := ClientHeight;
end;

function TFormMain.GetContentWidth: Integer;
begin
  result := ClientWidth;
end;

function TFormMain.GetIsUp: Boolean;
begin
  if Assigned(Main) then
    result := Main.IsUp
  else
    result := False;
end;

procedure TFormMain.UpdateBackground;
begin
  Self.Fill.Color := MainVar.ColorScheme.claBackground3D;
end;

procedure TFormMain.InitSudokuGraph;
var
  t: Integer;
begin
  t := 1200;
  SudokuImage := TOriginalImage.Create(Self, t, t);
  SudokuImage.Name := 'SudokuImage';
  SudokuImage.Parent := Self;
  SudokuImage.HitTest := False;
  SudokuImage.Visible := False;
  SudokuImage.OnMouseUp := SudokuImageMouseUp;

  SudokuGraph := TSudokuGraph.Create;
  SudokuGraph.BackgroundColor := MainVar.ColorScheme.claBackground3D;
  SudokuGraph.ImageOpacity := 0.2;

  SudokuGraph.Image := SudokuImage;
  UpdateSudokuGraph;
end;

procedure TFormMain.UpdateSudokuGraph;
begin
  if IsUp and SudokuImage.Visible then
  begin
    SudokuGraph.Draw;
  end;
end;

procedure TFormMain.LayoutImages;
var
  PosX: single;
  PosY: single;
begin
  if not ComponentsCreated then
    Exit;

  PosX := Raster + Margin;
  PosY := Raster + Margin;

  SudokuImage.Position.X := PosX;
  SudokuImage.Position.Y := PosY;
end;

procedure TFormMain.HandleAction(fa: Integer);
begin
  case fa of
    faNoop: ;

    else
    begin
      { do nothing }
    end;

  end;
end;

procedure TFormMain.SetupText(T: TText; fs: single);
begin
  T.Parent := Self;
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := fs;
  T.AutoSize := True;
  T.HitTest := False;
end;

procedure TFormMain.CreateComponents;
begin
  Raster := MainVar.Raster;
  Margin := 10.0;

{$ifdef WantHintText}
  HintText := TText.Create(Self);
  HintText.Name := 'HintText';
  SetupText(HintText, 24.0);
  HintText.TextSettings.FontColor := claYellow;
{$endif}

  InitSudokuGraph;
  Main.SudokuGrid := SudokuGraph;
  Main.SudokuGraph := SudokuGraph;
  SudokuImage.Visible := True;
  UpdateSudokuGraph;

  ComponentsCreated := True;
end;

procedure TFormMain.LayoutComponents;
begin
  if not ComponentsCreated then
    Exit;

{$ifdef WantHintText}
  HintText.Position.X := 2 * Raster + Margin;
  HintText.Position.Y := 11 * Raster + Margin;
{$endif}
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  Main.FederText.SB00.Caption := Application.Hint;
{$ifdef WantHintText}
  HintText.Text := Application.Hint;
{$endif}
end;

procedure TFormMain.SudokuImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LCol: single;
  LRow: single;
begin
  LCol := X / (SudokuGraph.DefaultColWidth * SudokuGraph.Zoom);
  if LCol >= SudokuGraph.ColCount then
    Exit;

  LRow := Y / (SudokuGraph.DefaultRowHeight * SudokuGraph.Zoom);
  if LRow >= SudokuGraph.RowCount then
    Exit;

  SudokuGraph.Col := Floor(LCol);
  SudokuGraph.Row := Floor(LRow);
  Main.Sudoku.InputHandler.HandleCellClick(SudokuGraph.Col, SudokuGraph.Row, Button = TMouseButton.mbRight);
end;

procedure TFormMain.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  SudokuImageMouseUp(Sender, Button, Shift,
  X - SudokuImage.Position.X,
  Y - SudokuImage.Position.Y);
end;

end.
