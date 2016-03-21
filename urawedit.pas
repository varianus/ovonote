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
unit uRawEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LCLIntf, SynEdit,
  Forms, Controls, Graphics, Dialogs,
  ComCtrls, ActnList, todotxtsyn_highlighter,
  DefaultTranslator, ExtCtrls, udatamodule;

type

  { TfRawEdit }

  TfRawEdit = class(TForm)
    actRefresh: TAction;
    actFileSave: TAction;
    ActionList1: TActionList;
    Editor: TSynEdit;
    TimerAutoSave: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    procedure actFileSaveExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure actRefreshExecute(Sender: TObject);
    procedure EditorChangeUpdating(ASender: TObject; AnUpdating: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure TimerAutoSaveTimer(Sender: TObject);
  private
    Syne : TTodoSyn;
    Active_File: string;
    Modified :boolean;
    procedure CheckAutoSave;
  public
    Constructor Create(AOwner: TComponent; Archived: Boolean); reintroduce;
  end;

var
  fRawEdit: TfRawEdit;

implementation

{$R *.lfm}

{ TfRawEdit }

constructor TfRawEdit.Create(AOwner: TComponent; Archived: Boolean);
begin
  inherited Create(Aowner);
  Modified := false;
  if Archived then
    begin
      Active_File:=FILE_DONE;
      actFileSave.Enabled:=false;
    end
  else
  Active_File:=FILE_TODO;

  if FileExists(dm.FilePath+Active_File) then
     Editor.Lines.LoadFromFile(dm.FilePath+Active_File)
  else
     Editor.Clear;

  Syne := TTodoSyn.Create(Self);
  Editor.Highlighter := syne;

  syne.ProjectAttri.Style:=[fsBold];
  syne.ContextAttri.Style:=[fsBold];
  syne.LinkAttri.Style:= [fsUnderline];
  syne.PriorityAttri.Background:=clyellow;
  syne.PropertyAttri.Background:=clLtGray;
  syne.DoneAttri.Style:=[fsItalic];


end;

procedure TfRawEdit.actRefreshExecute(Sender: TObject);
begin
  if FileExists(dm.FilePath+Active_File) then
     Editor.Lines.LoadFromFile(dm.FilePath+Active_File)
  else
     Editor.Clear;

end;

procedure TfRawEdit.EditorChangeUpdating(ASender: TObject; AnUpdating: Boolean);
begin
  TimerAutoSave.Enabled:=false;
  TimerAutoSave.Enabled:=True;
end;

procedure TfRawEdit.CheckAutoSave;
begin
  if dm.AutoSave and Editor.Modified then
    actFileSave.Execute;
end;

procedure TfRawEdit.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 CanClose := true;
 CheckAutoSave;

 if Editor.Modified then
    Case MessageDlg(RS_SAVE,mtWarning, mbYesNoCancel,0) of
     mrYes : begin
              actFileSave.Execute;
             end;
     mrCancel : CanClose:= false;
   end;

  if Modified then
     ModalResult:= mrModified;

end;

procedure TfRawEdit.TimerAutoSaveTimer(Sender: TObject);
begin
  TimerAutoSave.Enabled:=false;
  CheckAutoSave;
end;

procedure TfRawEdit.actFileSaveExecute(Sender: TObject);
begin
   Editor.Lines.SavetoFile(dm.FilePath+Active_File);
   Modified:=True;
   Editor.Modified:=false;
end;

procedure TfRawEdit.ActionList1Update(AAction: TBasicAction;
  var Handled: Boolean);
begin
  actFileSave.Enabled:= Editor.Modified;
end;

end.

