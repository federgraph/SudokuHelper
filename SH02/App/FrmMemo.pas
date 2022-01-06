unit FrmMemo;

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
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView;

type
  TMemoAction = record
  public
    Tag: Integer;
    Caption: string;
    Handler: TNotifyEvent;
  end;

  TMemoActionList = class (TList<TMemoAction>)
  public
    procedure AddMemoAction(ACaption: string; AHandler: TNotifyEvent);
    function FindByTag(ATag: Integer): TMemoAction;
  end;

  TFormMemo = class(TForm)
    ListView: TListView;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
  private
    SL: TStringList;
    MemoActionList: TMemoActionList;
    procedure InitItems;
    procedure InitList;
    procedure MemoBeginUpdate;
    procedure MemoEndUpdate;
  protected
    procedure ActionTestBtnClick(Sender: TObject);
    procedure WriteHelpText(Sender: TObject);
    procedure WriteNewActionConstBtnClick(Sender: TObject);
    procedure WriteActionConstBtnClick(Sender: TObject);
    procedure WriteActionNamesBtnClick(Sender: TObject);
    procedure WriteActionShortBtnClick(Sender: TObject);
    procedure WriteActionLongBtnClick(Sender: TObject);
    procedure WriteActionEncoder(Sender: TObject);
    procedure WriteActionDecoder(Sender: TObject);
    procedure WriteItems(Sender: TObject);
    procedure WriteShortcuts(Sender: TObject);
    procedure WriteDebugText(Sender: TObject);
    procedure WriteCurrentState(Sender: TObject);
  end;

var
  FormMemo: TFormMemo;

implementation

uses
  RiggVar.App.Main,
  SH.HelpText,
  RiggVar.FB.ActionTest,
  FrmMain;

{$R *.fmx}

procedure TFormMemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TFormMemo.FormCreate(Sender: TObject);
begin
  Caption := 'Form Memo';
  Left := 10;
  Top := 120;
  Width := 1100;
  Height := 750;
  SL := TStringList.Create;

  ListView.Align := TAlignLayout.Left;
  ListView.ItemAppearanceName := 'ListItem';
  ListView.ItemAppearance.ItemHeight := 24;
  ListView.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
  ListView.ItemAppearanceObjects.ItemObjects.Text.Font.Family := 'Consolas';
  ListView.ItemAppearanceObjects.ItemObjects.Text.Font.Size := 16;
  ListView.ItemAppearanceObjects.ItemObjects.Text.TextColor := TAlphaColors.Dodgerblue;
  ListView.ItemAppearanceObjects.HeaderObjects.Text.Visible := False;
  ListView.ItemAppearanceObjects.FooterObjects.Text.Visible := False;

  Memo.Align := TAlignLayout.Client;
  Memo.ControlType := TControlType.Styled;
  Memo.StyledSettings := [];
  Memo.ShowScrollBars := True;
  Memo.TextSettings.Font.Family := 'Consolas';
  Memo.TextSettings.Font.Size := 16;
  Memo.TextSettings.FontColor := claBlue;

  MemoActionList := TMemoActionList.Create;
  InitList;
  InitItems;

  ListView.ItemIndex := 10;
end;

procedure TFormMemo.FormDestroy(Sender: TObject);
begin
  SL.Free;
  MemoActionList.Free;
end;

procedure TFormMemo.ActionTestBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.TestAll;
  Memo.Lines.Text := Main.ActionTest.SL.Text;
  MemoEndUpdate;
end;

procedure TFormMemo.WriteNewActionConstBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteNewActionConst(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionConstBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteActionConst(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionNamesBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteActionNames(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionShortBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteActionShort(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionLongBtnClick(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteActionLong(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionEncoder(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteAction(Memo.Lines, afEncoded);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteActionDecoder(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionTest.WriteAction(Memo.Lines, afDecoded);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteItems(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionItem.WriteAll(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteShortcuts(Sender: TObject);
begin
  MemoBeginUpdate;
  Main.ActionHandler.GetShortcutReport(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteHelpText(Sender: TObject);
begin
  MemoBeginUpdate;
  GetHelpText(Memo.Lines);
  MemoEndUpdate;
end;

procedure TFormMemo.InitList;
begin
  MemoActionList.AddMemoAction('Action Test', ActionTestBtnClick);
  MemoActionList.AddMemoAction('Write Action Const', WriteActionConstBtnClick);
  MemoActionList.AddMemoAction('Write New Action Const', WriteNewActionConstBtnClick);
  MemoActionList.AddMemoAction('Write Action Names', WriteActionNamesBtnClick);
  MemoActionList.AddMemoAction('Write Action Short', WriteActionShortBtnClick);
  MemoActionList.AddMemoAction('Write Action Long', WriteActionLongBtnClick);
  MemoActionList.AddMemoAction('Write Action Encoder', WriteActionEncoder);
  MemoActionList.AddMemoAction('Write Action Decoder', WriteActionDecoder);
  MemoActionList.AddMemoAction('Write Action Items', WriteItems);
  MemoActionList.AddMemoAction('Write Shortcuts', WriteShortcuts);
  MemoActionList.AddMemoAction('Write Help Text', WriteHelpText);
  MemoActionList.AddMemoAction('Write Debug Text', WriteDebugText);
  MemoActionList.AddMemoAction('Write Current State', WriteCurrentState);
end;

procedure TFormMemo.InitItems;
var
  cr: TMemoAction;
  lvi: TListViewItem;
begin
  ListView.Items.Clear;
  for cr in MemoActionList do
  begin
    lvi := ListView.Items.Add;
    lvi.Text := cr.Caption;
    lvi.Tag := cr.Tag;
    lvi.Height := 24;
  end;
end;

procedure TFormMemo.ListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  cr: TMemoAction;
begin
  cr := MemoActionList.FindByTag(AItem.Tag);
  if cr.Tag > -1 then
    cr.Handler(nil);
end;

{ TMemoActionList }

procedure TMemoActionList.AddMemoAction(ACaption: string; AHandler: TNotifyEvent);
var
  ma: TMemoAction;
begin
  ma.Caption := ACaption;
  ma.Handler := AHandler;
  ma.Tag := Self.Count;
  Self.Add(ma);
end;

function TMemoActionList.FindByTag(ATag: Integer): TMemoAction;
var
  cr: TMemoAction;
begin
  result.Caption := 'not found';
  for cr in Self do
    if cr.Tag = ATag then
    begin
      result := cr;
      break;
    end;
end;

procedure TFormMemo.MemoBeginUpdate;
begin
  Memo.Lines.BeginUpdate;
  Memo.Lines.Clear;
end;

procedure TFormMemo.MemoEndUpdate;
begin
  Memo.Lines.EndUpdate;
{$ifdef MSWINDOWS}
  Memo.ContentBounds := TRectF.Empty;
{$endif}
end;

procedure TFormMemo.WriteDebugText(Sender: TObject);
var
  ML: TStrings;
begin
  ML := Memo.Lines;
  MemoBeginUpdate;
  Main.RunTest01(ML);
  Main.SudokuGraph.AddToDebugText(ML);
  FormMain.AddToDebugText(ML);
  MemoEndUpdate;
end;

procedure TFormMemo.WriteCurrentState(Sender: TObject);
var
  ML: TStrings;
begin
  ML := Memo.Lines;
  MemoBeginUpdate;
  Main.Sudoku.Data.SaveToML(ML);
  MemoEndUpdate;
end;

end.
