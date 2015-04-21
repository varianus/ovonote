{ Ovonote - My implementation of Todo.txt concept <http://todotxt.com/>

  Copyright (C) 2015 Marco Caselli <marco.caselli at gmail dot com>

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
unit USStringParser;

interface

uses
  classes,
  SysUtils;

type
  TUSStringParser = class
  private
    FSeparatore: string;
    FString: string;
    FStringItem: array of string;
    function GetItem(index: integer): string;
    function GetCount: integer;
  protected
    procedure SetString(const value: string); virtual;
    procedure FillStringItem;
  public
    constructor Create(str: string='');
    function GetStrings: TStringList;
    procedure Parse;
    procedure ParseExcel;

    function NormalizeString(MaxLength: Integer = -1): String;

    property Stringa: string read FString write SetString;
    property Separatore: string read FSeparatore write FSeparatore;
    property Count: integer read GetCount;
    property Item[Index: integer]: string read GetItem;  default;
  end;

implementation

constructor TUSStringParser.Create(str: string);
begin
  SetLength(FStringItem, 0);
  FSeparatore := ';';
  SetString(str);
end;

procedure TUSStringParser.SetString(const value: string);
begin
  FString := value;
  FillStringItem;
end;

procedure TUSStringParser.FillStringItem;
var
  i: integer;
  sl: TStringList;
begin
  sl := GetStrings;
  try
    SetLength(FStringItem, sl.Count);
    for i := 0 to sl.Count - 1 do
    begin
      FStringItem[i] := sl.Strings[i];
    end;
  finally
    sl.Free;
  end;
end;

function TUSStringParser.GetItem(index: integer): string;
begin
  if (index >= 0) and (index < Length(FStringItem)) then
    Result := FStringItem[index]
  else
    Result := '';
end;

function TUSStringParser.GetCount: integer;
begin
  Result := Length(FStringItem);
end;

procedure TUSStringParser.Parse;
begin
  FillStringItem;
end;

procedure TUSStringParser.ParseExcel;
var
  i: integer;
  dentroTesto: boolean;
begin
  dentroTesto := false;
  for i := 1 to Length(FString) do
  begin
    if (FString[i] = ';') and not dentroTesto then
      FString[i] := '@'
    else if FString[i] = '"' then
      dentroTesto := not dentroTesto;
  end;
  FSeparatore := '@';
  FillStringItem;
end;

function TUSStringParser.GetStrings: TStringList;
var
  tmp: string;
  iPosSeparatore: integer;
  rStrings: TStringList;
begin
  rStrings := TStringList.Create;
  tmp := FString;
  repeat
    iPosSeparatore := Pos(FSeparatore, tmp);
    if iPosSeparatore > 0 then
    begin
      rStrings.Add(Copy(tmp, 0, iPosSeparatore - 1));
      tmp := Copy(tmp, iPosSeparatore + 1, Length(tmp) - iPosSeparatore);
    end
  until (Length(tmp) <= 0) or (iPosSeparatore = 0);
  if Length(tmp) > 0 then
    rStrings.Add(tmp);
  Result := rStrings;
end;

function TUSStringParser.NormalizeString(MaxLength: Integer): String;
begin
  // parametri:
  // - Testo     : Testo iniziale da controllare/sistemare
  // - MaxLength : Massima dimensione della stringa, se <= 0 è disattivata la funzione
  // - Result    : Risultato della funzione (testo pulito)
  Result := FString;

  // Sostituisco i caratteri "non graditi" (causano errori al salvataggio"
  // Misti:
  Result := StringReplace(Result,  #96,   #39, [rfReplaceAll]);  // Accento particolare
  Result := StringReplace(Result, #146,   #39, [rfReplaceAll]);  // Accento particolare
  Result := StringReplace(Result, #147,   '"', [rfReplaceAll]);  // Virgolette aperte
  Result := StringReplace(Result, #148,   '"', [rfReplaceAll]);  // Virgolette chiuse
  Result := StringReplace(Result, #149,   '-', [rfReplaceAll]);  // Elenco puntato Word
  Result := StringReplace(Result, #150,   '-', [rfReplaceAll]);  // Trattino Word
  Result := StringReplace(Result, #180,   #39, [rfReplaceAll]);  // Accento particolare
  Result := StringReplace(Result, #183,   '-', [rfReplaceAll]);  // Elenco puntato Word

  Result := StringReplace(Result,  '€', 'EUR', [rfReplaceAll]);
  Result := StringReplace(Result,  '°',   '^', [rfReplaceAll]);

  // Accenti:
  Result := StringReplace(Result,  'É', ('E'+#39), [rfReplaceAll]);
  Result := StringReplace(Result,  'à', ('a'+#39), [rfReplaceAll]);
  Result := StringReplace(Result,  'á', ('a'+#39), [rfReplaceAll]);
  Result := StringReplace(Result,  'è', ('e'+#39), [rfReplaceAll]);
  Result := StringReplace(Result,  'é', ('e'+#39), [rfReplaceAll]);
  Result := StringReplace(Result,  'ì', ('i'+#39), [rfReplaceAll]);
  Result := StringReplace(Result,  'ò', ('o'+#39), [rfReplaceAll]);
  Result := StringReplace(Result,  'ù', ('u'+#39), [rfReplaceAll]);

  if MaxLength >= 1 then
    Result := Copy(Result, 1, MaxLength);
end;

end.
