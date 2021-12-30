{!
<summary>
 This unit implements a simple modal dialog to select one of the registered
 Sudoku helpers from.
</summary>
<author>Dr. Peter Below</author>
<history>
 Version 1.0 created 2021-11-07<p>
 Last modified       2021-11-07<p>
</history>
<copyright>Copyright 2021 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>
}
unit SH_SelectSudokuDlgU;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils,
  Classes,
  ExtCtrls,
  Buttons,
  StdCtrls,
  Forms,
  Controls,
  Graphics;

type

  { TSelectSudokuDlg }

  TSelectSudokuDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Cancelbutton: TBitBtn;
    OKButton: TBitBtn;
    Label1: TLabel;
    ValueList: TListBox;
    procedure CancelbuttonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  protected
    function GetSelectedValue: string;
    procedure PositionBelow(aControl: TControl);
    procedure UpdateActions; override;
  public
    procedure AfterConstruction; override;
    class function Execute(var aSudoku: string; aControl: TControl): Boolean;
    property SelectedSudoku: string read GetSelectedValue;
  end;

implementation

uses
  SH.HelperBase,
  SH.Strings;

{$R *.lfm}

procedure TSelectSudokuDlg.CancelbuttonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSelectSudokuDlg.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectSudokuDlg.AfterConstruction;
begin
  inherited;
  HelperRegistry.GetKnownHelpers(ValueList.Items);
  ValueList.Itemindex := 0;
end;

class function TSelectSudokuDlg.Execute(var aSudoku: string; aControl: TControl): Boolean;
var
  Dlg: TSelectSudokuDlg;
begin
  Dlg := TSelectSudokuDlg.Create(nil);
  try
    Dlg.Caption := SSelectSudokuCaption;
    Dlg.PositionBelow(aControl);
    Result := Dlg.ShowModal = mrOK;
    if Result then
      aSudoku := Dlg.SelectedSudoku;
  finally
    Dlg.Free;
  end;
end;

function TSelectSudokuDlg.GetSelectedValue: string;
begin
  Result := ValueList.Items[ValueList.ItemIndex];
end;

procedure TSelectSudokuDlg.PositionBelow(aControl: TControl);
var
  P: TPoint;
begin
  P:= aControl.BoundsRect.TopLeft;
  P.Offset(0, aControl.Height);
  P := aControl.Parent.ClientToScreen(P);
  Left := P.X - 2;
  Top := P.Y;
end;

procedure TSelectSudokuDlg.UpdateActions;
begin
  inherited;
  OKButton.Enabled := ValueList.ItemIndex >= 0;
end;

end.
