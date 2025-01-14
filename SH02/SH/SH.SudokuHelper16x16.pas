﻿{!
<summary>
 This unit implements the helper for the  16x16 Sudoku and its
 support classes.
 </summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2021-10-29<p>
 Last modified by PB 2021-10-29<p>
 Last modified by GS 2022-01-xx<p>
</history>
<copyright>Copyright 2021 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>
<remarks>
 This Sudoku uses a 16x16 grid and the numerals 1 to 16 as cell
 values. We use 0 to mark an empty cell. Blocks have a 4x4 size.
 We also support the Gosu variant and one using heptadecimal digits
 instead of decimal, with the value range 1 to G.
</remarks>
}
unit SH.SudokuHelper16x16;

interface

uses
  SH.HelperBase,
  SH.Interfaces;

type
  T16x16SudokuHelper = class(TBaseSudokuHelper)
  strict protected
    procedure CreateDataStorage; override;
    procedure CreateDisplayHandler; override;
    procedure CreateInputHandler; override;
  public
    class function GetDisplayname: string; override;
    class function GetSudokuID: string; override;
  end;

  T16x16SudokuGosuHelper = class(T16x16SudokuHelper)
  strict protected
    procedure CreateDataStorage; override;
  public
    class function GetDisplayname: string; override;
    class function GetSudokuID: string; override;
  end;

  T16x16HeptSudokuHelper = class(T16x16SudokuHelper)
  strict protected
    procedure CreateDisplayHandler; override;
    procedure CreateInputHandler; override;
  public
    class function GetDisplayname: string; override;
    class function GetSudokuID: string; override;
  end;

  T16x16HeptSudokuGosuHelper = class(T16x16HeptSudokuHelper)
  strict protected
    procedure CreateDataStorage; override;
  public
    class function GetDisplayname: string; override;
    class function GetSudokuID: string; override;
  end;

implementation

uses
  System.SysUtils,
  FMX.StdCtrls,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionLong,
  RiggVar.FB.ActionShort,
  SH.DataStorageBase,
  SH.DisplayhandlerBase,
  SH.InputhandlerBase;

const
  CMaxValue = 16;
  CBlockWidth = 4;
  CBlockHeight = 4;
  CSymbols: array [0..16] of Char =
    (' ','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G');

type
  T16x16SudokuDataStorage = class(TBaseSudokuDatastorage)
  end;

  T16x16SudokuDisplayhandler = class(TBaseSudokuDisplayhandler)
  strict protected
    function GetDefaultCellSize: Integer; override;
  end;

  T16x16SudokuInputhandler = class(TBaseSudokuInputHandler)
  strict protected
    function IsAllowedValue(aChar: Char): Boolean; override;
  end;

  T16x16HeptSudokuDisplayhandler = class(T16x16SudokuDisplayhandler)
  strict protected
    function GetSymbol(aValue: TSudokuValue): string; override;
  end;

  T16x16HeptSudokuInputhandler = class(T16x16SudokuInputhandler);

procedure T16x16SudokuHelper.CreateDataStorage;
begin
  Data := T16x16SudokuDataStorage.Create(CMaxValue, CBlockHeight, CBlockWidth, False);
end;

procedure T16x16SudokuHelper.CreateDisplayHandler;
begin
  Display := T16x16SudokuDisplayhandler.Create(Data);
end;

procedure T16x16SudokuHelper.CreateInputHandler;
begin
  InputHandler := T16x16SudokuInputhandler.Create(Data);
end;

class function T16x16SudokuHelper.GetDisplayname: string;
begin
  result := GetFederActionLong(faSudoku16A);
end;

class function T16x16SudokuHelper.GetSudokuID: string;
begin
  result := GetFederActionShort(faSudoku16A);
end;

procedure T16x16SudokuGosuHelper.CreateDataStorage;
begin
  Data := T16x16SudokuDataStorage.Create(CMaxValue, CBlockHeight, CBlockWidth, true);
end;

class function T16x16SudokuGosuHelper.GetDisplayname: string;
begin
  result := GetFederActionLong(faSudoku16B);
end;

class function T16x16SudokuGosuHelper.GetSudokuID: string;
begin
  result := GetFederActionShort(faSudoku16B);
end;

function T16x16SudokuDisplayhandler.GetDefaultCellSize: Integer;
begin
  Result := 48;
end;

function T16x16SudokuInputhandler.IsAllowedValue(aChar: Char): boolean;
begin
  Result := CharInSet(aChar, ['0'..'9', 'a'..'g']);
end;

{== T16x16HeptSudokuHelper =============================================}

procedure T16x16HeptSudokuHelper.CreateDisplayHandler;
begin
  Display := T16x16HeptSudokuDisplayhandler.Create(Data);
end;

procedure T16x16HeptSudokuHelper.CreateInputHandler;
begin
  InputHandler := T16x16HeptSudokuInputhandler.Create(Data);
end;

class function T16x16HeptSudokuHelper.GetDisplayname: string;
begin
  result := GetFederActionLong(faSudoku16C);
end;

class function T16x16HeptSudokuHelper.GetSudokuID: string;
begin
  result := GetFederActionShort(faSudoku16C);
end;

{== T16x16HeptSudokuGosuHelper =========================================}

procedure T16x16HeptSudokuGosuHelper.CreateDataStorage;
begin
  Data := T16x16SudokuDataStorage.Create(CMaxValue, CBlockHeight, CBlockWidth, true);
end;

class function T16x16HeptSudokuGosuHelper.GetDisplayname: string;
begin
  result := GetFederActionLong(faSudoku16D);
end;

class function T16x16HeptSudokuGosuHelper.GetSudokuID: string;
begin
  result := GetFederActionShort(faSudoku16D);
end;

{== T16x16HeptSudokuDisplayHandler =====================================}

{! Get the string to draw in a cell for cell value }
function T16x16HeptSudokuDisplayhandler.GetSymbol(aValue: TSudokuValue): string;
begin
  Result := CSymbols[aValue];
end;

initialization
  HelperRegistry.Register(T16x16SudokuHelper);
  HelperRegistry.Register(T16x16SudokuGosuHelper);
  HelperRegistry.Register(T16x16HeptSudokuHelper);
  HelperRegistry.Register(T16x16HeptSudokuGosuHelper);
end.
