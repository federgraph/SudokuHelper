﻿unit RiggVar.FB.ActionGroups;

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
  System.Generics.Collections,
  RiggVar.FB.ActionConst,
  RiggVar.FB.ActionGroup;

type
  TActionGroupList = class(TList<TActionGroup>)
  public
    GroupNames: TStrings;
    constructor Create;
    destructor Destroy; override;
    procedure AddSpecial(const Value: TActionGroup; AName: string);
    function ActionCount: Integer;
    function GetUsage: string;
    function GetGroup(fa: TFederAction): Integer;
    function GetGroupName(i: Integer): string;
  end;

implementation

{ TActionGroupList }

destructor TActionGroupList.Destroy;
begin
  GroupNames.Free;
  inherited;
end;

function TActionGroupList.ActionCount: Integer;
var
  i: Integer;
  j: Integer;
  cr: TActionGroup;
begin
  j := 0;
  for i := 0 to Count-1 do
  begin
    cr := self[i];
    j := j + Length(cr);
  end;
  result := j;
end;

procedure TActionGroupList.AddSpecial(const Value: TActionGroup; AName: string);
var
  AG: TActionGroup;
begin
  AG := Value; { because of RSP-16471, a bug in 10.1 }
  GroupNames.Add(AName);
  Add(AG);
end;

constructor TActionGroupList.Create;
begin
  inherited;

  GroupNames := TStringList.Create;

  AddSpecial(ActionGroupEmptyAction, 'EmptyAction');
  AddSpecial(ActionGroupPages, 'Pages');
  AddSpecial(ActionGroupForms, 'Forms');
  AddSpecial(ActionGroupTouchLayout, 'TouchLayout');
  AddSpecial(ActionGroupWheel, 'Wheel');
  AddSpecial(ActionGroupColorScheme, 'ColorScheme');
  AddSpecial(ActionGroupTouchBarLegend, 'TouchBarLegend');
  AddSpecial(ActionGroupBtnLegendTablet, 'BtnLegendTablet');
  AddSpecial(ActionGroupBtnLegendPhone, 'BtnLegendPhone');

  AddSpecial(ActionGroupSudokuNavigation, 'SudokuNavigation');
  AddSpecial(ActionGroupSudokuSelection, 'SudokuSelection');
  AddSpecial(ActionGroupSudokuPlacing, 'SudokuPlacing');
  AddSpecial(ActionGroupSudokuScene, 'SudokuScene');
  AddSpecial(ActionGroupSudokuMode, 'SudokuMode');
  AddSpecial(ActionGroupSudokuCommands, 'SudokuCommands');
end;

function TActionGroupList.GetGroup(fa: TFederAction): Integer;
var
  i: Integer;
  j: Integer;
  l: Integer;
  cr: TActionGroup;
begin
  result := -1;
  for i := 0 to Count-1 do
  begin
    cr := Self.Items[i];
    l := Length(cr);
    for j := 0 to l-1 do
    begin
      if cr[j] = fa then
      begin
        result := i;
        Exit;
      end;
    end;
  end;
end;

function TActionGroupList.GetGroupName(i: Integer): string;
begin
  if (i >= 0) and (i < GroupNames.Count) and (i < Count) then
    result := GroupNames[i]
  else
    result := '';
end;

function TActionGroupList.GetUsage: string;
var
  fa: TFederAction;
  i: Integer;
  j: Integer;
  l: Integer;
  cr: TActionGroup;
  SL: TStringList;
  s1: string;
begin
  SL := TStringList.Create;
  for fa := 0 to faMax - 1 do
    SL.Add(Format('%d=0', [fa]));

  s1 := '1';
  for i := 0 to Count-1 do
  begin
    cr := Self.Items[i];
    l := Length(cr);
    for j := 0 to l - 1 do
    begin
      SL.Values[IntToStr(cr[j])] := s1;
    end;
  end;

  for i := SL.Count - 1 downto 0 do
    if (SL.Values[IntToStr(i)] = '1') then
      SL.Delete(i);

  result := SL.Text;
  SL.Free;
end;

end.
