{!
<summary>
 This unit implements a simple modal dialog to select one of the defined
 stack marks from.
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
unit SH_SelectMarkDlgU;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  ExtCtrls,
  Buttons,
  StdCtrls,
  Forms,
  Controls,
  Graphics,
  Classes,
  SysUtils,
  SH.Interfaces;

type

  { TSelectMarkDlg }

  TSelectMarkDlg = class(TForm)
    Cancelbutton: TBitBtn;
    Label1: TLabel;
    OKButton: TBitBtn;
    ValueList: TListBox;
    procedure CancelbuttonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  protected
    function GetSelectedValue: string;
    procedure PositionBelow(aControl: TControl);
    procedure UpdateActions; override;
  public
    class function Execute(var aMark: string; const aSudoku: ISudokuHelper; aControl: TControl): Boolean;
    property SelectedMark: string read GetSelectedValue;
  end;

implementation

uses
  SH.Strings;

{$R *.lfm}

procedure TSelectMarkDlg.CancelbuttonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSelectMarkDlg.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

class function TSelectMarkDlg.Execute(var aMark: string; const aSudoku: ISudokuHelper; aControl: TControl): Boolean;
var
  Dlg: TSelectMarkDlg;
begin
  Dlg:= TSelectMarkDlg.Create(nil);
  try
    Dlg.Caption := SSelectMarkCaption;
    aSudoku.GetMarks(Dlg.ValueList.Items);
    Dlg.PositionBelow(aControl);
    Result := Dlg.ShowModal = mrOK;
    if Result then
      aMark := Dlg.SelectedMark;
  finally
    Dlg.Free;
  end;
end;

function TSelectMarkDlg.GetSelectedValue: string;
begin
  Result := ValueList.Items[ValueList.ItemIndex];
end;

procedure TSelectMarkDlg.PositionBelow(aControl: TControl);
var
  P: TPoint;
begin
  P:= aControl.BoundsRect.TopLeft;
  P.Offset(0, aControl.Height);
  P := aControl.Parent.ClientToScreen(P);
  Left := P.X - 2;
  Top := P.Y;
end;

procedure TSelectMarkDlg.UpdateActions;
begin
  inherited;
  OKButton.Enabled := ValueList.ItemIndex >= 0;
end;

end.
