unit SH.SudokuGrid;

interface

uses
  System.Types,
  FMX.Controls;

type
  TSudokuGrid = class
  private
    procedure SetCol(const Value: Integer);
    procedure SetRow(const Value: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    DefaultRowHeight: Integer;
    DefaultColWidth: Integer;

    ColCount: Integer;
    RowCount: Integer;

    FCol: Integer;
    FRow: Integer;

    constructor Create;

    procedure NavCol(Delta: Integer);
    procedure NavRow(Delta: Integer);

    procedure Draw; virtual;

    procedure Invalidate; virtual;
    procedure InvalidateCell(ACol, ARow: Integer); virtual;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Col: Integer read FCol write SetCol;
    property Row: Integer read FRow write SetRow;
  end;

implementation

{ TSudokuGrid }

constructor TSudokuGrid.Create;
begin
  DefaultRowHeight := 64;
  DefaultColWidth := 64;

  ColCount := 9;
  RowCount := 9;

  FCol := 1;
  FRow := 1;
end;

procedure TSudokuGrid.Draw;
begin
  { virtual }
end;

function TSudokuGrid.GetHeight: Integer;
begin
  result := RowCount * DefaultRowHeight
end;

function TSudokuGrid.GetWidth: Integer;
begin
  result := ColCount * DefaultColWidth;
end;

procedure TSudokuGrid.Invalidate;
begin
  Draw;
end;

procedure TSudokuGrid.InvalidateCell(ACol, ARow: Integer);
begin
  Draw;
end;

procedure TSudokuGrid.NavCol(Delta: Integer);
begin
  Col := FCol + Delta;
  Draw;
end;

procedure TSudokuGrid.NavRow(Delta: Integer);
begin
  Row := FRow + Delta;
  Draw;
end;

procedure TSudokuGrid.SetCol(const Value: Integer);
begin
  if Value < 0 then
    FCol := 0
  else if Value > ColCount - 1 then
    FCol := ColCount - 1
  else
    FCol := Value;
end;

procedure TSudokuGrid.SetRow(const Value: Integer);
begin
  FRow := Value;
  if Value < 0 then
    FRow := 0
  else if Value > RowCount - 1 then
    FRow := RowCount - 1
  else
    FRow := Value;
end;

end.
