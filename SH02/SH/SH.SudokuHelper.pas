﻿{!
<summary>
 This unit implements the helper for the classic 9x9 Sudoku and its
 support classes.
 </summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2021-10-02<p>
 Last modified by PB 2021-11-10<p>
 Last modified by GS 2022-01-xx<p>
</history>
<copyright>Copyright 2021 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>
<remarks>
 The classic Sudoku uses a 9x9 grid and the numerals 1 to 9 as cell
 values. We use 0 to mark an empty cell. Blocks have a 3x3 size.
</remarks>
}
unit SH.SudokuHelper;

interface

uses
  SH.HelperBase;

type
  {! Helper for the classic 9x9 Sudoku }
  TClassicSudokuHelper = class(TBaseSudokuHelper)
  strict protected
    procedure CreateDataStorage; override;
    procedure CreateDisplayHandler; override;
    procedure CreateInputHandler; override;
  public
    class function GetDisplayname: string; override;
    class function GetSudokuID: string; override;
  end;

  {! Helper for the classic 9x9 Sudoku Gosu }
  TClassicSudokuGosuHelper = class(TClassicSudokuHelper)
  strict protected
    procedure CreateDataStorage; override;
  public
    class function GetDisplayname: string; override;
    class function GetSudokuID: string; override;
  end;

implementation

uses
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionLong,
  RiggVar.FB.ActionShort,
  SH.DataStorageBase,
  SH.DisplayHandlerBase,
  SH.InputHandlerBase;

const
  CBlockSize = 3;
  CMaxValue = 9;

{
For the classic 9x9 Sudokus all the functionality needed is implemented
in the base classes for data storage, display and input handlers. Derived
classes are only declared here for clarity and to support future
refactorings.
}
type
  TClassicSudokuDataStorage = class(TBaseSudokuDatastorage)
  end;

  TClassicSudokuDisplayhandler = class(TBaseSudokuDisplayhandler)
  end;

  TClassicSudokuInputhandler = class(TBaseSudokuInputhandler)
  end;


{== TClassicSudokuHelper ==============================================}

procedure TClassicSudokuHelper.CreateDataStorage;
begin
  Data := TClassicSudokuDataStorage.Create(CMaxValue, CBlockSize, CBlockSize, False);
end;

procedure TClassicSudokuHelper.CreateDisplayHandler;
begin
  Display := TClassicSudokuDisplayhandler.Create(Data);
end;

procedure TClassicSudokuHelper.CreateInputHandler;
begin
  InputHandler := TClassicSudokuInputhandler.Create(Data);
end;

class function TClassicSudokuHelper.GetDisplayname: string;
begin
  result := GetFederActionLong(faSudoku09A);
end;

class function TClassicSudokuHelper.GetSudokuID: string;
begin
  result := GetFederActionShort(faSudoku09A);
end;

{== TClassicSudokuGosuHelper ==========================================}

procedure TClassicSudokuGosuHelper.CreateDataStorage;
begin
  Data := TClassicSudokuDataStorage.Create(CMaxValue, CBlockSize, CBlockSize, True);
end;

class function TClassicSudokuGosuHelper.GetDisplayname: string;
begin
  result := GetFederActionLong(faSudoku09B);
end;

class function TClassicSudokuGosuHelper.GetSudokuID: string;
begin
  result := GetFederActionShort(faSudoku09B);
end;

initialization
  HelperRegistry.Register(TClassicSudokuHelper);
  HelperRegistry.Register(TClassicSudokuGosuHelper);
end.

