{ Ovonote - My implementation of Todo.txt concept <http://todotxt.com/>

  Copyright (C) 2015 Marco Caselli <marcocas at gmail dot com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{+-----------------------------------------------------------------------------+
 | Class:       TTodoSyn
 | Created:     2014-11-14
 | Last change: 2014-11-14
 | Author:      Marco Caselli
 | Description: Syntax Parser/Highlighter
 | Version:     0.1
 |
 | Copyright (c) 2014 Marco Caselli. All rights reserved.
 |
 | Generated with SynGen.
 +----------------------------------------------------------------------------+}

unit todotxtsyn_highlighter;

interface

uses
  Classes, SysUtils,
  Graphics,
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (
    tkContext,
    tkText,
    tkPriority,
    tkProject,
    tkProperty,
    tkLinks,
    tkSpace,
    tkNull,
    tkDone);

  TProcTableProc = procedure of object;

type

  { TTodoSyn }

  TTodoSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: integer;
    fLinkAttri: TSynHighlighterAttributes;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: longint;
    fTokenPos: integer;
    fTokenID: TtkTokenKind;
    fContextAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fPriorityAttri: TSynHighlighterAttributes;
    fProjectAttri: TSynHighlighterAttributes;
    fPropertyAttri: TSynHighlighterAttributes;
    fDoneAttri: TSynHighlighterAttributes;
    procedure CRProc;
    procedure ContextProc;
    procedure TextProc;
    procedure DoneProc;
    procedure LFProc;
    procedure NullProc;
    procedure PriorityProc;
    procedure PropertyProc;
    procedure ProjectProc;
    procedure SpaceProc;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    class function GetLanguageName: string; override;
    function IsKeyword(const AKeyword: string): boolean;  override;
    function IsLink(const AKeyword: string): boolean;

  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEOL: boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: string; LineNumber: integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    procedure Next; override;
  published
    property ContextAttri: TSynHighlighterAttributes read fContextAttri write fContextAttri;
    property DoneAttri: TSynHighlighterAttributes read fDoneAttri write fDoneAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
    property PriorityAttri: TSynHighlighterAttributes read fPriorityAttri write fPriorityAttri;
    property ProjectAttri: TSynHighlighterAttributes read fProjectAttri write fProjectAttri;
    property PropertyAttri: TSynHighlighterAttributes read fPropertyAttri write fPropertyAttri;
    property LinkAttri: TSynHighlighterAttributes read fLinkAttri write fLinkAttri;

  end;

implementation
uses
  strutils;

const
  SPACES_SET = [#1..#9, #11, #12, #14..#32];
  TOKENTERM_SET = [#0..#32];

const
  KeysCount = 4;
  Keys: array[1..KeysCount] of string = (
    'DUE:','H:','PRI:','PERC:');

  LinksCount = 5;
  Links: array[1..LinksCount] of string = (
    'FTP:','HTTP:','HTTPS:','MAILTO:','OUTLOOK:');



procedure TTodoSyn.MakeMethodTables;
var
  I: char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[i] := @NullProc;
      #10: fProcTable[I] := @LFProc;
      #13: fProcTable[I] := @CRProc;
      '+': fProcTable[I] := @ProjectProc;
      '(': fProcTable[I] := @PriorityProc;
      'x': fProcTable[I] := @DoneProc;
      '@': fProcTable[I] := @ContextProc;
      'A'..'Z', 'a'..'w', 'y'..'z': fProcTable[I] := @PropertyProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      else
        fProcTable[I] := @TextProc;
      end;
end;

const

  SYNS_AttrContext = 'Context';
  SYNS_AttrIdentifiers = 'Identifier';
  SYNS_AttrPriority = 'Priority';
  SYNS_AttrProject = 'Project';
  SYNS_AttrProperty = 'Property';
  SYNS_DoneProperty = 'Done';
  SYNS_LinkProperty = 'Link';


constructor TTodoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // YOU SHOULD ADD AT LEAST ONE OR TWO DIFFERENT DEFAULT ATTRIBUTES
  fContextAttri := TSynHighLighterAttributes.Create(SYNS_AttrContext);
  AddAttribute(fContextAttri);
  fDoneAttri := TSynHighLighterAttributes.Create(SYNS_DoneProperty);
  AddAttribute(fDoneAttri);
  fTextAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifiers);
  AddAttribute(fTextAttri);
  fPriorityAttri := TSynHighLighterAttributes.Create(SYNS_AttrPriority);
  AddAttribute(fPriorityAttri);
  fProjectAttri := TSynHighLighterAttributes.Create(SYNS_AttrProject);
  AddAttribute(fProjectAttri);
  fPropertyAttri := TSynHighLighterAttributes.Create(SYNS_AttrProperty);
  AddAttribute(fPropertyAttri);
  fLinkAttri := TSynHighLighterAttributes.Create(SYNS_LinkProperty);
  AddAttribute(fLinkAttri);

  SetAttributesOnChange(@DefHighlightChange);

  fDefaultFilter := 'Todo.txt Files (*.txt)|*.txt';
  MakeMethodTables;

end;

procedure TTodoSyn.SetLine(const NewValue: string; LineNumber: integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TTodoSyn.PropertyProc;
begin
  while not(fLine[Run] in TOKENTERM_SET) do inc(run);
  if IsKeyWord(GetToken) then begin
    fTokenId := tkProperty;
    Exit;
  end
  else
  if IsLink(GetToken) then begin
    fTokenId := tkLinks;
    Exit;
  end

  else fTokenId := tkText;

end;

procedure TTodoSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: Inc(Run, 2);
    else
      Inc(Run);
    end;
end;

procedure TTodoSyn.ContextProc;
begin
  if (fTokenPos = 0) or ((fTokenPos > 0) and (fProcTable[fLine[Run - 1]] = @SpaceProc)) then
    begin
    fTokenID := tkContext;
    while (fProcTable[fLine[Run]] <> @SpaceProc) and (fProcTable[fLine[Run]] <> @NullProc) do
      Inc(Run);
    end
  else
    begin
    fTokenID := tkText;
    Inc(run);
    end;

end;


procedure TTodoSyn.ProjectProc;
begin
  if (fTokenPos = 0) or ((fTokenPos > 0) and (fProcTable[fLine[Run - 1]] = @SpaceProc)) then
    begin
    fTokenID := tkProject;
    while (fProcTable[fLine[Run]] <> @SpaceProc) and (fProcTable[fLine[Run]] <> @NullProc) do
      Inc(Run);
    end
  else
    begin
    fTokenID := tkText;
    Inc(run);
    end;

end;

procedure TTodoSyn.TextProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @TextProc)) do inc(Run);
  fTokenID := tkText;

end;

procedure TTodoSyn.DoneProc;
begin
  if (fTokenPos = 0) and (fLine[run] = 'x') and (FLine[Run + 1] in SPACES_SET) then
    begin
    fTokenID := tkDone;
    repeat
      Inc(Run);
    until (fLine[Run] in [#0, #10, #13]);
    end
  else
    TextProc;
end;

procedure TTodoSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TTodoSyn.PriorityProc;
begin
  Inc(run);
  if (fTokenPos = 0) and (fLine[Run] in ['A'..'Z']) and (fLine[Run + 1] = ')') then
    begin
    Inc(run, 2);
    fTokenID := tkPriority;
    end
  else
    fTokenID := tkText;
end;

procedure TTodoSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TTodoSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TTodoSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT   : Result := fTextAttri;
    SYN_ATTR_IDENTIFIER: Result := fTextAttri;
    SYN_ATTR_KEYWORD   : Result := fProjectAttri;
    SYN_ATTR_STRING    : Result := fTextAttri;
    SYN_ATTR_WHITESPACE: Result := fTextAttri;
    else Result := fTextAttri;
  end;
end;

procedure TTodoSyn.NullProc;
begin
  fTokenID := tkNull;
end;

function TTodoSyn.GetEOL: boolean;
begin
  Result := fTokenID = tkNull;
end;

function TTodoSyn.GetToken: string;
var
  Len: longint;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TTodoSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := FLine + fTokenPos;
end;

function TTodoSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TTodoSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkDone:     Result := fDoneAttri;
    tkContext:  Result := fContextAttri;
    tkText:     Result := fTextAttri;
    tkPriority: Result := fPriorityAttri;
    tkProject:  Result := fProjectAttri;
    tkProperty: Result := fPropertyAttri;
    tkLinks:    Result := fLinkAttri;
  else
    Result := nil;
  end;
end;

function TTodoSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TTodoSyn.GetTokenPos: integer;
begin
  Result := fTokenPos;
end;

function TTodoSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z', ':', '-','/','.','%'];

end;

class function TTodoSyn.GetLanguageName: string;
begin
  Result := 'TODO.TXT';
end;

function TTodoSyn.IsKeyword(const AKeyword: string): boolean;
var
  i: Integer;
begin
  i := Pos(':',AKeyword);
  if (i > 0) and (i < Length(AKeyword)) then
  begin
    Exit(True);
  end;
  Result := False;
end;

function TTodoSyn.IsLink(const AKeyword: string): boolean;
var
  i: Integer;
begin
  for i := 1 to LinksCount do if AnsiStartsText(Links[i],AKeyWord) then
  begin
    Exit(True);
  end;
  Result := False;
end;


initialization
  RegisterPlaceableHighlighter(TTodoSyn);
end.
