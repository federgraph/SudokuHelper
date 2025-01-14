﻿unit FrmMemo;

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
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls;

type
  TFormMemo = class(TForm)
    SaveBtn: TButton;
    LoadBtn: TButton;
    TestBtn: TButton;
    HelpBtn: TButton;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure TestBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    Margin: Integer;
    ML: TStringList;
    procedure RunTest;
  end;

var
  FormMemo: TFormMemo;

implementation

{$R *.dfm}

uses
  SH_MainU,
  SH.HelpText,
  Generics.Collections;

procedure TFormMemo.FormCreate(Sender: TObject);
begin
  Width := 800;
  Height := 800;

  Margin := 10;
  ML := TStringList.Create;

  SaveBtn.Top := Margin;
  SaveBtn.Left := 3 * Margin;
  SaveBtn.Caption := 'Save Text';

  LoadBtn.Top := Margin;
  LoadBtn.Left := SaveBtn.Left + SaveBtn.Width + Margin;
  LoadBtn.Caption := 'Load Text';

  TestBtn.Top := Margin;
  TestBtn.Left := LoadBtn.Left + LoadBtn.Width + Margin;
  TestBtn.Caption := 'Run Test';

  HelpBtn.Top := Margin;
  HelpBtn.Left := TestBtn.Left + TestBtn.Width + Margin;
  HelpBtn.Caption := 'Show Help';

  Memo.Left := Margin;
  Memo.Top := SaveBtn.Top + SaveBtn.Height + Margin;
  Memo.ScrollBars := System.UITypes.TScrollStyle.ssBoth;
  Memo.Width := ClientWidth - Memo.Left - Margin;
  Memo.Height := ClientHeight - Memo.Top - Margin;
  Memo.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
  Memo.Font.Name := 'Consolas';
  Memo.Font.Size := 12;
  Memo.Font.Color := clBlue;
end;

procedure TFormMemo.FormDestroy(Sender: TObject);
begin
  ML.Free;
end;

procedure TFormMemo.SaveBtnClick(Sender: TObject);
begin
  ML.Clear;
  FormMain.Sudoku.Data.SaveToML(ML);
  Memo.Lines.Text := ML.Text;
end;

procedure TFormMemo.TestBtnClick(Sender: TObject);
begin
  RunTest;
end;

procedure TFormMemo.LoadBtnClick(Sender: TObject);
begin
  ML.Text := Memo.Lines.Text;
  FormMain.Sudoku.Data.LoadFromML(ML);
  FormMain.Sudoku.Display.Refresh;
end;

procedure TFormMemo.RunTest;
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
      Memo.Text := SB.ToString;
    finally
      LList.Free;
    end;
  finally
    SB.Free;
  end;
end;

procedure TFormMemo.HelpBtnClick(Sender: TObject);
begin
  ML.Clear;
  GetHelpText(ML);
  Memo.Lines.Text := ML.Text;
end;

end.
