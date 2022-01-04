{!
<summary>
 This unit implements the base class for all Sudoku display handlers.
 </summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2021-10-04<p>
 Last modified by PB 2021-11-20<p>
 Last modified by GS 2022-01-xx<p>
</history>
<copyright>Copyright 2021 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>
<remarks>
 The display handler is responsible for drawing the Sudoku grid.
</remarks>
}
unit SH.DisplayHandlerBase;

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  SH.SudokuGrid,
  SH.Interfaces;

type
  {!
  <summary>
   All Sudoku display handlers have to descend from this class.
    </summary>
  <remarks>
   The display handler draws the cells of the grid used to display the
   Sudoku. The base class implements all functionality needed for a
   Sudoku using integer numbers, but descendants can override some of
   the virtual methods to modify the behaviour as needed. </remarks>
  }
  TBaseSudokuDisplayhandler = class abstract(TInterfacedObject, ISudokuDisplay)
  strict private
    FDataStorage: ISudokuData;
    FGrid: TSudokuGrid;
    FInitialized: Boolean;
  strict protected
    procedure DataChanged(Sender: TObject);
    function GetDefaultCellSize: Integer; virtual;
    function GetSymbol(aValue: TSudokuValue): string; virtual;
    procedure InitializeGrid(aGrid: TSudokuGrid);
    function IsInitialized: Boolean;
    procedure RedrawCell(aCol, aRow: TSudokuCellIndex);
    procedure Refresh;
    property DataStorage: ISudokuData read FDataStorage;
    property DefaultCellSize: Integer read GetDefaultCellSize;
    property Grid: TSudokuGrid read FGrid write FGrid;
  public
    constructor Create(const ADataStorage: ISudokuData);
  end;

implementation

uses
  SH.Exceptions,
  System.Math,
  FMX.Controls;

constructor TBaseSudokuDisplayhandler.Create(const ADataStorage: ISudokuData);
begin
  inherited Create;
  FDataStorage := ADataStorage;
  DataStorage.Events.OnDataChanged := DataChanged;
  DataStorage.Events.OnRedrawCell := RedrawCell;
end;

procedure TBaseSudokuDisplayhandler.DataChanged(Sender: TObject);
begin
  Refresh;
end;

function TBaseSudokuDisplayhandler.GetDefaultCellSize: Integer;
begin
  Result := 64;
end;

{! Get the string to draw in a cell for the cell value }
function TBaseSudokuDisplayhandler.GetSymbol(aValue: TSudokuValue): string;
begin
  Result := IntToStr(aValue);
end;

{!
<summary>
 Set up the draw grid as appropriate for the supported Sudoku type.</summary>
<param name="aGrid">is the grid to initialize, cannot be nil</param>
<exception cref="EParameterCannotBeNil">
 is raised if aGrid is nil.</exception>
<remarks>
 This method must be called once to connect the helper to the UI!</remarks>
}
procedure TBaseSudokuDisplayhandler.InitializeGrid(aGrid: TSudokuGrid);
var
  LSudokuSize: Integer;
begin
  if not Assigned(aGrid) then
    raise EParameterCannotBeNil.Create(Classname + '.InitializeGrid', 'aGrid');
  Grid := aGrid;
  LSudokuSize := Datastorage.Bounds.MaxValue;
  Grid.ColCount := LSudokuSize;
  Grid.RowCount := LSudokuSize;
  Grid.DefaultRowHeight := DefaultCellSize;
  Grid.DefaultColWidth := DefaultCellSize;
  FInitialized := True;
  Grid.Draw;
end;

function TBaseSudokuDisplayhandler.IsInitialized: boolean;
begin
  Result := FInitialized;
end;

procedure TBaseSudokuDisplayhandler.RedrawCell(aCol, aRow: TSudokuCellIndex);
begin
  Grid.InvalidateCell(aCol, aRow);
end;

procedure TBaseSudokuDisplayhandler.Refresh;
begin
  Grid.Invalidate;
end;

end.

