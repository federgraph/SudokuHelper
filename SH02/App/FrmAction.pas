﻿unit FrmAction;

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
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Grid,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.Objects,
  FMX.ListView,
  FMX.Edit,
  FMX.Layouts,
  FMX.ListBox;

type
  TFormAction = class(TForm)
    Edit: TEdit;
    ListViewS: TListView;
    ListViewG: TListView;
    ListViewDetails: TListView;
    ListViewGroups: TListView;
    TextEdit: TText;
    TextDetail: TText;
    TextGroup: TText;
    TextGroups: TText;
    TextCaption: TText;
    TextSearchResult: TText;
    SortBtn: TButton;
    HideBtn: TButton;
    CaseBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ListViewSItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure ListViewGItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure ListViewActionsItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure ListViewGroupsItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure SortBtnClick(Sender: TObject);
    procedure HideBtnClick(Sender: TObject);
    procedure CaseBtnClick(Sender: TObject);
  private
    Margin: Integer;
  private
    ofa: TFederAction;
    ofg: Integer;
    osn, oln, oan, ogn: string;
    ML: TStrings;
    GroupsSorted: Boolean;
    InsensitiveSearch: Boolean;
    procedure ListGroups;
    procedure ListGroupsSorted;
    procedure ListGroup(g: Integer);
    procedure ListGroupForAction(fa: TFederAction);
    procedure DoSearch;
    procedure ListMappings(fa: TFederAction);
    procedure ListActionsLong(const t: string);
    procedure ListActionsShort(const t: string);
    procedure UpdateDetails;
    procedure UpdateLabel(fa: TFederAction);
    procedure ClearCaption;
    procedure ClearMappings;
    procedure UpdateVars(fa: TFederAction);
    procedure UpdateCaseBtnCaption;
    procedure SetupListView(LV: TListView; Color: TAlphaColor);
  end;

var
  FormAction: TFormAction;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionGroups,
  RiggVar.FB.ActionName,
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

procedure TFormAction.EditKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  DoSearch;
end;

procedure TFormAction.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caHide;
end;

procedure TFormAction.FormCreate(Sender: TObject);
begin
  Caption := 'Form Actions';
  ML := TStringList.Create;

  Left := 10;
  Top := 120;
  Width := 1000;
  Height := 650;
  Margin := 10;

  TextDetail.AutoSize := True;
  TextCaption.AutoSize := True;
  TextEdit.AutoSize := True;
  TextGroup.AutoSize := True;
  TextGroups.AutoSize := True;

  TextCaption.TextSettings.WordWrap := False;
  TextEdit.TextSettings.WordWrap := False;
  TextGroup.TextSettings.WordWrap := False;
  TextGroups.TextSettings.WordWrap := False;
  TextDetail.TextSettings.WordWrap := False;

  ListViewS.Height := ClientHeight - ListViewS.Position.Y - Margin;
  ListViewDetails.Height := ClientHeight - ListViewDetails.Position.Y - Margin;
  ListViewG.Height := ClientHeight - ListViewG.Position.Y - Margin;
  ListViewGroups.Height := ClientHeight - ListViewGroups.Position.Y - Margin;

  ListViewS.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];
  ListViewDetails.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];
  ListViewG.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];
  ListViewGroups.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];

  SetupListView(ListViewS, TAlphaColors.Tomato);
  SetupListView(ListViewDetails, TAlphaColors.Black);
  SetupListView(ListViewG, TAlphaColors.Teal);
  SetupListView(ListViewGroups, TAlphaColors.DodgerBlue);

  ListGroups;

  UpdateCaseBtnCaption;
end;

procedure TFormAction.FormDestroy(Sender: TObject);
begin
  ML.Free;
end;

procedure TFormAction.DoSearch;
var
  t: string;
begin
  ListViewS.Items.Clear;
  if InsensitiveSearch then
    t := Lowercase(Edit.Text)
  else
    t := Edit.Text;

  if t.StartsWith('fa') then
  begin
    t := t.Substring(2);
    if t.Length > 2 then
      ListActionsLong(t);
  end

  else if t.StartsWith('-') then
  begin
    t := t.Substring(1);
    if t.Length > 2 then
      ListActionsLong(t);
  end

  else if (t.Length >= 1) and (t.Length <= 3) then
    ListActionsShort(t)

  else if (t.Length > 3) and (t.Length <= 8) then
    ListActionsLong(t);
end;

procedure TFormAction.ListActionsShort(const t: string);
var
  a, s, u: string;
  fa: TFederAction;
  lvi: TListViewItem;
begin
  for fa := 0 to faMax-1 do
  begin
    a := GetFederActionName(fa);
    s := GetFederActionShort(fa);
      if InsensitiveSearch then
        u := LowerCase(s)
      else
        u := s;
    if u.StartsWith(t) then
    begin
      lvi := ListViewS.Items.Add;
      lvi.Text := s;
      lvi.Tag := fa;
      lvi.Height := 24;
    end;
  end;
end;

procedure TFormAction.ListActionsLong(const t: string);
var
  a, s: string;
  fa: TFederAction;
  lvi: TListViewItem;
begin
  for fa := 0 to faMax-1 do
  begin
    if InsensitiveSearch then
      a := LowerCase(GetFederActionName(fa))
    else
      a := GetFederActionName(fa);
    s := GetFederActionShort(fa);
    if a.Contains(t) then
    begin
      lvi := ListViewS.Items.Add;
      lvi.Text := s;
      lvi.Tag := fa;
      lvi.Height := 24;
    end;
  end;
end;

procedure TFormAction.ListGroups;
var
  g: Integer;
  gn: string;
  lvi: TListViewItem;
  agl: TActionGroupList;
begin
  agl := Main.ActionGroupList;
  ListViewGroups.Items.Clear;
  for g := 0 to agl.Count-1 do
  begin
    gn := agl.GetGroupName(g);
    lvi := ListViewGroups.Items.Add;
    lvi.Text := gn;
    lvi.Tag := g;
    lvi.Height := 24;
  end;
end;

procedure TFormAction.ListViewGroupsItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  g: Integer;
begin
  g := AItem.Tag;
  ClearCaption;
  ListGroup(g);
  ClearMappings;
end;

procedure TFormAction.ListGroupForAction(fa: TFederAction);
var
  g: Integer;
begin
  g := Main.ActionGroupList.GetGroup(fa);
  ListGroup(g);
end;

procedure TFormAction.ListGroup(g: Integer);
var
  fa: TFederAction;
  h: Integer;
  gn: string;
  lvi: TListViewItem;
begin
  gn := Main.ActionGroupList.GetGroupName(g);
  TextGroup.Text := Format('Group %d = %s', [g, gn]);

  ListViewG.Items.Clear;
  for fa := faNoop to Pred(faMax) do
  begin
    h := Main.ActionGroupList.GetGroup(fa);
    if g = h then
    begin
      lvi := ListViewG.Items.Add;
      lvi.Text := GetFederActionName(fa);
      lvi.Tag := fa;
      lvi.Height := 24;
    end;
  end;
end;

procedure TFormAction.ListViewGItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  fa: TFederAction;
begin
  fa := AItem.Tag;
  UpdateLabel(fa);
  ListMappings(fa);
end;

procedure TFormAction.ListViewActionsItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  fa: TFederAction;
begin
  fa := AItem.Index;
  UpdateLabel(fa);
  ListGroupForAction(fa);
  ListMappings(fa);
end;

procedure TFormAction.ListViewSItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  fa: TFederAction;
begin
  fa := AItem.Tag;
  UpdateLabel(fa);
  ListGroupForAction(fa);
  ListMappings(fa);
end;

procedure TFormAction.ClearCaption;
begin
  TextCaption.Text := '';
end;

procedure TFormAction.UpdateLabel(fa: TFederAction);
var
  fg: Integer;
  an, sn, ln, gn: string;
begin
  an := GetFederActionName(fa);
  sn := GetFederActionShort(fa);
  ln := GetFederActionLong(fa);
  fg := Main.ActionGroupList.GetGroup(fa);
  gn := Main.ActionGroupList.GetGroupName(fg);
  TextCaption.Text := Format('"%s" = "%s" in group "%s"', [sn, ln, gn]);
end;

procedure TFormAction.ClearMappings;
begin
  ListViewDetails.Items.Clear;
  TextDetail.Text := 'Action Details';
end;

procedure TFormAction.ListMappings(fa: TFederAction);
begin
  ML.Clear;
  UpdateVars(fa);

  ML.Add(Format('action id = %d', [ofa]));
  ML.Add(Format('action name = %s', [oan]));
  ML.Add(Format('short name = %s', [osn]));
  ML.Add(Format('long name = %s', [oln]));
  ML.Add(Format('group id = %d', [ofg]));
  ML.Add(Format('group name = %s', [ogn]));

  Main.CollectShortcuts(fa, ML);

  UpdateDetails;
end;

procedure TFormAction.UpdateDetails;
var
  i: Integer;
  s: string;
  lvi: TListViewItem;
begin
  TextDetail.Text := Format('Action %s = %s', [oan, osn]);

  ListViewDetails.Items.Clear;
  for i := 0 to ML.Count-1 do
  begin
    s := ML[i];
    if s <> '' then
    begin
      lvi := ListViewDetails.Items.Add;
      lvi.Text := s;
      lvi.Height := 24;
    end;
  end;
end;

procedure TFormAction.UpdateVars(fa: TFederAction);
begin
  ofa := fa;
  osn := GetFederActionShort(ofa);
  oln := GetFederActionLong(ofa);
  oan := GetFederActionName(ofa);
  ofg := Main.ActionGroupList.GetGroup(ofa);
  ogn := Main.ActionGroupList.GetGroupName(ofg);
end;

procedure TFormAction.SortBtnClick(Sender: TObject);
begin
  GroupsSorted := not GroupsSorted;
  if GroupsSorted then
    ListGroupsSorted
  else
    ListGroups;
end;

procedure TFormAction.ListGroupsSorted;
var
  g: Integer;
  gn: string;
  lvi: TListViewItem;
  agl: TActionGroupList;
  ML: TStringList;
  i: Integer;
begin
//  SortBtn.Enabled := False;

  agl := Main.ActionGroupList;

  ML := TStringList.Create;
  ML.Sorted := True;

  for i := 0 to agl.Count - 1 do
  begin
    ML.AddObject(agl.GroupNames[i], TObject(i));
  end;

  ListViewGroups.Items.Clear;

  for i := 0 to ML.Count-1 do
  begin
    g := Integer(ML.Objects[i]);
    gn := agl.GetGroupName(g);
    gn := ML[i];
    Assert(ML[i] = agl.GetGroupName(g));
    lvi := ListViewGroups.Items.Add;
    lvi.Text := gn;
    lvi.Tag := g;
    lvi.Height := 24;
  end;

  ML.Free;
end;

procedure TFormAction.HideBtnClick(Sender: TObject);
begin
  ClientWidth := Round(ListViewGroups.Position.X);
end;

procedure TFormAction.CaseBtnClick(Sender: TObject);
begin
  InsensitiveSearch  := not InsensitiveSearch;
  UpdateCaseBtnCaption;
  DoSearch;
end;

procedure TFormAction.UpdateCaseBtnCaption;
begin
  if InsensitiveSearch then
    CaseBtn.Text := 'aA'
  else
    CaseBtn.Text := 'aa';
end;

procedure TFormAction.SetupListView(LV: TListView; Color: TAlphaColor);
begin
  LV.ItemAppearanceName := 'ListItem';
  LV.ItemAppearance.ItemHeight := 24;
  LV.ItemAppearanceObjects.ItemObjects.Accessory.Visible := False;
  LV.ItemAppearanceObjects.ItemObjects.Text.Font.Family := 'Consolas';
  LV.ItemAppearanceObjects.ItemObjects.Text.Font.Size := 16;
  LV.ItemAppearanceObjects.ItemObjects.Text.TextColor := Color;
  LV.ItemAppearanceObjects.HeaderObjects.Text.Visible := False;
  LV.ItemAppearanceObjects.FooterObjects.Text.Visible := False;
end;

end.
