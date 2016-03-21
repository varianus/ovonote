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
unit todo_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, USStringParser, contnrs;

type
  // forward
  TTaskList = class;

  { TTask }
  TTask = class
  private
    FCompletion: TDate;
    FContexts: TStringList;
    FCreation: TDate;
    FDone: boolean;
    FDue: TDate;
    FLink: string;
    FModified: Boolean;
    FOwner: TTaskList;
    FPriority: char;
    FProjects: TStringList;
    FRow: integer;
    FTask: string;
    procedure ExtractProjectsAndContexts(row: string);
    procedure SetCompletion(AValue: TDate);
    procedure SetContexts(AValue: TStringList);
    procedure SetCreation(AValue: TDate);
    procedure SetDone(AValue: boolean);
    procedure SetDue(AValue: TDate);
    procedure SetLink(AValue: string);
    procedure SetModified(AValue: Boolean);
    procedure SetPriority(AValue: char);
    procedure SetProjects(AValue: TStringList);
    procedure SetRow(AValue: integer);
    procedure SetTask(AValue: string);
    function SortKey: DWORD;
  public
    Property Owner: TTaskList read FOwner;
    property Modified: Boolean read FModified write SetModified;
    Property Row : integer read FRow write SetRow;
    property Task: string read FTask write SetTask;
    property Priority: char read FPriority write SetPriority;
    property Done: boolean read FDone write SetDone;
    property Projects: TStringList read FProjects write SetProjects;
    property Contexts: TStringList read FContexts write SetContexts;
    property Due: TDate read FDue write SetDue;
    property Completion: TDate read FCompletion write SetCompletion;
    property Creation: TDate read FCreation write SetCreation;
    property Link: string read FLink write SetLink;
    constructor Create(aOwner: TTaskList);
    destructor Destroy;  override;
  end;

  { TTaskList }

  TTaskList = class(TObjectList)
  private
    fMasterModified: boolean;
    FOnChange: TNotifyEvent;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
    procedure SetOnChange(AValue: TNotifyEvent);
  protected
    Procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    Procedure DoChange(Task:TTask);
  public
    Parser: TUSStringParser;
    Property Modified: boolean read GetModified write SetModified;
    procedure LoadFromStream(Data: TStream);
    procedure SaveToStream(Data: TStream);
    procedure AppendToStream(Data: TStream);
    function RowFromItem(Task: TTask): string;
    function ItemFromRow(row: string): TTask;
    Procedure GetContexts( TaskList: TStrings);
    Procedure GetProjects( TaskList: TStrings);
    Procedure Tasksort;
    Property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    constructor Create; reintroduce;
    destructor Destroy;  override;

  end;

function ExtractDate(strDate: string): TdateTime;

implementation

{ TTaskList }
uses strutils, Math, RegExpr;

function TaskCompare(Item1, Item2: Pointer): Integer;
var
  Task1: TTask absolute item1;
  Task2: TTask absolute item2;
begin
  result :=  CompareValue(ord(task1.done), ord(task2.done));
  if result = 0 then
    Result := CompareValue(task1.SortKey, Task2.SortKey);
  if result = 0 then
    Result := CompareValue(ifthen(Task1.due=0, MaxDateTime,task1.Due),
                           ifthen(Task2.due=0, MaxDateTime,task2.Due)) ;

  if result = 0 then
     result := CompareValue(task1.Row, task2.Row);

end;

procedure TTaskList.SetModified(AValue: boolean);
var
  i: integer;
begin
  fMasterModified:=AValue;

  for i :=0 to count -1 do
    TTask(Items[i]).Modified := false;

end;

procedure TTaskList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  fMasterModified:= true;
  DoChange(TTask(Ptr));
end;

procedure TTaskList.DoChange(Task: TTask);
begin
  if Assigned(FOnChange) then
     FOnChange(self);
end;

function TTaskList.GetModified: boolean;
var
  i: integer;
begin
  result := fMasterModified;
  if not fMasterModified then
    for i :=0 to count -1 do
      begin
        if TTask(Items[i]).Modified then
           begin
             result := true;
             break;
           end;
      end;

end;

procedure TTaskList.LoadFromStream(Data: TStream);
var
  Lista: TStringList;
  i, r: integer;
  aTask: TTask;
begin
  lista := TStringList.Create;
  r:= 1;
  try
    Lista.LoadFromStream(Data);
    for i := 0 to lista.Count - 1 do
      begin
       if trim(Lista[i]) = emptystr then
         Continue;

        atask := ItemFromRow(Lista[i]);
        if Assigned(aTask) then
          begin
            ATask.Row:= R;
            add(aTask);
            inc(r);
          end;
      end;

    finally
      lista.Free;
    end;
 Modified:=false;

end;

function TTaskList.RowFromItem(Task: TTask): string;
//var
//  i: integer;
//  CurrString: string;
begin
  Result := '';
  if Task.Done then
    begin
       Result := 'x ';
       if Task.Completion > 0 then
          Result := Result + FormatDateTime('YYYY-MM-DD ', Task.Completion);
    end
  else
    if Task.Priority <> #00 then
      Result := '(' + Task.Priority + ') ';

  if Task.Creation <> 0 then
      Result := Result + FormatDateTime('YYYY-MM-DD ', Task.Creation);

  Result := Result + Task.Task;

  if Task.Due <> 0 then
    Result := Result + ' due:' + FormatDateTime('YYYY-MM-DD', Task.Due);

  if Task.Done and (Task.Priority <> #00) then
    Result := Result + ' pri:' + Task.Priority;


  if task.Link <> '' then
    Result := Result + ' ' + task.link;

  RESULT:= trim(result);

end;

procedure TTaskList.SaveToStream(Data: TStream);
var
  row: string;
  i: integer;
begin
  Data.Size:=0;
  AppendToStream(Data);

end;

procedure TTaskList.AppendToStream(Data: TStream);
var
  row: string;
  i: integer;
begin
  for i := 0 to Count - 1 do
    begin
      row := RowFromItem(TTask(Items[i]));
      Data.Write(row[1], Length(row));
      Row:=sLineBreak;
      Data.Write(Row[1], SizeOf(sLineBreak));
    end;

  Modified:=false;

end;

function ExtractDate(strDate: string): TdateTime;
begin
  if not TryStrToDate(strDate, Result, 'YYYY-MM-DD', '-') then
    Result := 0;

end;


function TTaskList.ItemFromRow(row: string): TTask;
var
  i: integer;
  CurrString: string;
  wDate:TDate;
begin
  Result := TTask.Create(self);
  Parser.Stringa := row;
  for i := 0 to Parser.Count - 1 do
    begin
    CurrString := Parser[i];
   // handle positional
    case i of
    0: begin
        if (CurrString = 'x') then
          begin
            Result.FDone := True;
            Continue;
          end
        else if ExecRegExpr('^\([A-Z]\)$', CurrString) then
            begin
              Result.Priority := CurrString[2];
              Continue;
            end
          else
            Result.Priority := #00;

        wDate:= ExtractDate(CurrString);
        if wDate <> 0 then
          begin
            Result.Creation := wDate;
            Continue;
          end;

      end;
    1: begin
         wDate:= ExtractDate(CurrString);
         if wDate <> 0 then
           begin
              if Result.done then
                 Result.completion := wDate
              else
                 Result.Creation := wDate;
              Continue;
           end;
       end;
    2: begin
         if Result.Done and (result.Completion <> 0) then
            begin
               wDate:= ExtractDate(CurrString);
               if wDate <> 0 then
                  begin
                    Result.Creation := wDate;
                    Continue;
                  end;
            end;
        end;
    end;
    if Length(CurrString) >= 1 then
        begin
           if AnsiStartsStr('due:', CurrString) then
              Result.Due := ExtractDate(Copy(CurrString, 5, Length(CurrString)))
           else if AnsiStartsStr('pri:', CurrString) then
              Result.priority := Copy(CurrString, 5, 1)[1]
           else
                if (AnsiStartsStr('http:', CurrString) or
                    AnsiStartsStr('https:', CurrString)) then
              Result.Link := CurrString
              // //  //    //      //
           else
             begin
               Result.FTask := Result.FTask + CurrString + ' ';
               if CurrString[1] = '+' then
                 Result.Projects.Add(CurrString)
               else
                 if CurrString[1] = '@' then
                   Result.Contexts.Add(CurrString);

            end;
        end;
      end;
end;

procedure TTaskList.GetContexts( TaskList: TStrings);
var
  i:Integer;
  Task: TTask;
  IntList: TStringList;
begin
  TaskList.Clear;
  IntList := TStringList.Create;
  IntList.Sorted:= true;
  IntList.Duplicates:=dupIgnore;

  for i := 0 to Count -1 do
    begin
      Task := TTask(Items[i]);
      IntList.AddStrings(Task.Contexts);
    end;
  TaskList.Assign(IntList);
  IntList.free;

end;

procedure TTaskList.GetProjects( TaskList: TStrings);
var
  i:Integer;
  Task: TTask;
  IntList: TStringList;
begin
  TaskList.Clear;
  IntList := TStringList.Create;
  IntList.Sorted:= true;
  IntList.Duplicates:=dupIgnore;

  for i := 0 to Count -1 do
    begin
      Task := TTask(Items[i]);
      IntList.AddStrings(Task.Projects);
    end;
  TaskList.Assign(IntList);
  IntList.free;

end;

procedure TTaskList.Tasksort;
begin
    Sort(@TaskCompare);
end;

procedure TTaskList.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;


constructor TTaskList.Create;
begin
  inherited Create(true);
  Parser := TUSStringParser.Create;
  Parser.Separatore := ' ';
  fMasterModified := false;
end;

destructor TTaskList.Destroy;
begin
  inherited Destroy;
  Parser.free;
end;

{ TTask }

procedure TTask.SetCompletion(AValue: TDate);
begin
  if FCompletion = AValue then
    Exit;
  FCompletion := AValue;
  SetModified(true);
end;

procedure TTask.SetContexts(AValue: TStringList);
begin
  if FContexts = AValue then
    Exit;
  FContexts := AValue;
  SetModified(true);
end;

procedure TTask.SetCreation(AValue: TDate);
begin
  if FCreation=AValue then Exit;
  FCreation:=AValue;
  SetModified(true);
end;

procedure TTask.SetDone(AValue: boolean);
begin
  if FDone = AValue then
    Exit;
  FDone := AValue;
  if FDone then
    FCompletion:=trunc(Now)
  else
    FCompletion := 0;
  SetModified(true);

end;

procedure TTask.SetDue(AValue: TDate);
begin
  if FDue = AValue then
    Exit;
  FDue := AValue;
  SetModified(true);
end;

procedure TTask.SetLink(AValue: string);
begin
  if FLink = AValue then
    Exit;
  FLink := AValue;
  SetModified(true);
end;

procedure TTask.SetModified(AValue: Boolean);
begin
  FModified:=AValue;
  if (FModified) then
    begin
     FOwner.DoChange(self);
    end;

end;

procedure TTask.SetPriority(AValue: char);
begin
  if FPriority = AValue then
    Exit;
  FPriority := AValue;
  SetModified(true);
end;

procedure TTask.SetProjects(AValue: TStringList);
begin
  if FProjects = AValue then
    Exit;
  FProjects := AValue;
  SetModified(true);
end;

procedure TTask.SetRow(AValue: integer);
begin
  if FRow=AValue then Exit;
  FRow:=AValue;
  SetModified(true);
end;

procedure TTask.SetTask(AValue: string);
begin
  if FTask = AValue then
    Exit;
  FTask := AValue;
  ExtractProjectsAndContexts(AValue);
  SetModified(true);
end;

function TTask.SortKey: DWORD;
begin
  Result := $ffffFFFF;
  if Priority <> #00 then
     result := result and (byte(Priority))
  else
    result := result and (byte(ord('M')));

end;

constructor TTask.Create(aOwner: TTaskList);
begin
  FProjects := TStringList.Create;
  FContexts := TStringList.Create;
  FCompletion:= 0;
  FCreation := 0;
  FTask :='';
  FDone :=false;
  FDue :=0;
  FPriority:=#00;
  FOwner := aOwner;
end;


destructor TTask.Destroy;
begin
  FProjects.Free;
  FContexts.Free;

end;

procedure TTask.ExtractProjectsAndContexts(row: string);
var
  i: integer;
  CurrString: string;
  Parser: TUSStringParser;
begin
  fProjects.Clear;
  fContexts.Clear;
  Parser := FOwner.Parser;

  Parser.Stringa := row;
  for i := 0 to Parser.Count - 1 do
    begin
      CurrString := Parser[i];

      if Length(CurrString) < 2 then
         Continue;

      if CurrString[1] = '+' then
         fProjects.Add(CurrString)
      else
         if CurrString[1] = '@' then
            fContexts.Add(CurrString)

    end;
end;


end.
