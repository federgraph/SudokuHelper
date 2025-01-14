﻿unit RiggVar.FB.Action;

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
  RiggVar.FB.ActionConst;

type
  IFederActionHandler = interface
  ['{F4DE90A6-6D46-42F0-8119-F309A3802479}']
    procedure Execute(fa: TFederAction);
    function GetEnabled(fa: TFederAction): Boolean;
    function GetChecked(fa: TFederAction): Boolean;
    function GetCaption(fa: TFederAction): string;
    function GetShortCaption(fa: TFederAction): string;
  end;

  TFederActionHandlerBase = class(TInterfacedObject, IFederActionHandler)
  public
    procedure ExportTable(ML: TStrings); virtual;
    procedure CheckForDuplicates(ML: TStringList); virtual;
    procedure Execute(fa: TFederAction); virtual;
    function GetEnabled(fa: TFederAction): Boolean; virtual;
    function GetChecked(fa: TFederAction): Boolean; virtual;
    function GetCaption(fa: TFederAction): string; virtual;
    function GetShortCaption(fa: TFederAction): string; virtual;
  end;

implementation

uses
  RiggVar.FB.ActionShort,
  RiggVar.FB.ActionLong;

function TFederActionHandlerBase.GetShortCaption(fa: TFederAction): string;
begin
  result := GetFederActionShort(fa);
end;

function TFederActionHandlerBase.GetCaption(fa: TFederAction): string;
begin
  result := GetFederActionLong(fa);
end;

function TFederActionHandlerBase.GetChecked(fa: TFederAction): Boolean;
begin
  result := False;
end;

function TFederActionHandlerBase.GetEnabled(fa: TFederAction): Boolean;
begin
  result := True;
end;

procedure TFederActionHandlerBase.Execute(fa: TFederAction);
begin
  { virtual, do nothing here }
end;

procedure TFederActionHandlerBase.CheckForDuplicates(ML: TStringList);
begin
  { not implemented here }
end;

procedure TFederActionHandlerBase.ExportTable(ML: TStrings);
begin
  { virtual, do nothing here }
end;

end.
