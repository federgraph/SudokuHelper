﻿unit FrmMain;

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
  RiggVar.FB.ActionConst,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math,
  FMX.Forms,
  FMX.Graphics,
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
    Raster: single;
    Margin: single;
    ComponentsCreated: Boolean;
    FormShown: Boolean;

    SudokuImage: TOriginalImage;
    SudokuGraph: TSudokuGraph;

    procedure HandleShowHint(Sender: TObject);

    function GetIsUp: Boolean;
    procedure Init;
    procedure SetIsUp(const Value: Boolean);
    procedure InitSudokuGraph;
    procedure UpdateSudokuGraph;
    procedure CreateComponents;
    procedure HandleAction(fa: Integer);
  public
    procedure UpdateBackground;

    property IsUp: Boolean read GetIsUp write SetIsUp;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormMain := self;

  ReportMemoryLeaksOnShutdown := True;

  Fill.Kind := TBrushKind.Solid; // because it is still TBrushKind.None
  Self.Position := TFormPosition.ScreenCenter;

  ClientWidth := 1024;
  ClientHeight := 900;

  Caption := UpperCase(Application.Title);

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

  Main.ColorScheme := 4;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  MainVar.AppIsClosing := True;

  SudokuGraph.Free;

  Main.Free;
  Main := nil;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    Main.InitFirstSudoku;
  end;
end;

procedure TFormMain.SetIsUp(const Value: Boolean);
begin
  Main.IsUp := Value;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
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

function TFormMain.GetIsUp: Boolean;
begin
  if Assigned(Main) then
    result := Main.IsUp
  else
    result := False;
end;

procedure TFormMain.UpdateBackground;
begin
  Self.Fill.Color := MainVar.ColorScheme.claBackground;
end;

procedure TFormMain.InitSudokuGraph;
var
  t: Integer;
begin
  t := 1200;
  SudokuImage := TOriginalImage.Create(Self, t, t);
  SudokuImage.Name := 'SudokuImage';
  SudokuImage.Parent := Self;
  SudokuImage.Position.X := Raster + Margin;
  SudokuImage.Position.Y := Raster + Margin;
  SudokuImage.HitTest := False;
  SudokuImage.Visible := False;
  SudokuImage.OnMouseUp := SudokuImageMouseUp;

  SudokuGraph := TSudokuGraph.Create;
  SudokuGraph.BackgroundColor := MainVar.ColorScheme.claBackground;
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

procedure TFormMain.CreateComponents;
begin
  Raster := MainVar.Raster;
  Margin := 10.0;

  InitSudokuGraph;
  Main.SudokuGrid := SudokuGraph;
  Main.SudokuGraph := SudokuGraph;
  SudokuImage.Visible := True;
  UpdateSudokuGraph;

  ComponentsCreated := True;
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  Main.FederText.SB00.Caption := Application.Hint;
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