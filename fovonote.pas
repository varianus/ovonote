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
unit fovonote;

{$mode objfpc}{$H+}

interface

uses
  types, Classes, SysUtils, FileUtil, DateTimePicker, lcltype, lclintf, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, Grids, ExtCtrls, ActnList, Menus,
  MaskEdit, Buttons, todo_parser, udatamodule, usettings,
  DefaultTranslator, ExtDlgs, uniqueInstance;

Const
 AppNameServerID = 'ovonote-MC';
  { TfrmOvoNote }

type
  TfrmOvoNote = class(TForm)
    actArchive: TAction;
    actAdd: TAction;
    actDelete: TAction;
    actEdit: TAction;
    actTerminate: TAction;
    actShowDone: TAction;
    actSettings: TAction;
    ActionList1: TActionList;
    edtNewTask: TEdit;
    edtTask: TEdit;
    edtSearch: TEdit;
    gridTask: TStringGrid;
    FileOpen1: TAction;
    FileSaveAs1: Taction;
    lbContexts: TListBox;
    lbProjects: TListBox;
    edtPriority: TMaskEdit;
    mList: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuFile: TMenuItem;
    pnlFilter: TPanel;
    pnlNew: TPanel;
    Panel4: TPanel;
    Panel2: TPanel;
    bSearch: TSpeedButton;
    SpeedButton1: TSpeedButton;
    TimerAutoSave: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    tbClose: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    dtpDueDate: TDateTimePicker;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TrayIcon: TTrayIcon;
    TrayMenu: TPopupMenu;
    procedure actAddExecute(Sender: TObject);
    procedure actArchiveExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure actSettingsExecute(Sender: TObject);
    procedure actShowDoneExecute(Sender: TObject);
    procedure actTerminateExecute(Sender: TObject);
    procedure bSearchClick(Sender: TObject);
    procedure edtNewTaskKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtSearchChange(Sender: TObject);
    procedure edtPriorityKeyPress(Sender: TObject; var Key: char);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtTaskKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridTaskButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure gridTaskCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
    procedure gridTaskDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure gridTaskExit(Sender: TObject);
    procedure gridTaskHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure gridTaskPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
    procedure gridTaskSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
    procedure gridTaskSetEditText(Sender: TObject; ACol, ARow: integer; const Value: string);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileSaveAs1Execute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gridTaskUserCheckboxBitmap(Sender: TObject; const aCol,
      aRow: Integer; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
    procedure lbProjectsSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure TimerAutoSaveTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    fLoading : boolean;
    TaskList: TTaskList;
    UniqueInstanceI: TUniqueInstance;

    procedure CheckAutoSave;
    procedure LoadFile;
    procedure LoadToGrid(FilterProject, FilterContext, FilterText: string); overload;
    procedure LoadToGrid(FilterProject, FilterContext: Integer); overload;
    procedure LoadToGrid(); overload;
    procedure ReloadFilters;
    procedure ResizeGrid;
    procedure TaskListChange(Sender: TObject);
    procedure TasktoGridRow(Task: TTask; Arow: Integer);
    procedure UniqueInstanceIOtherInstance(Sender: TObject;
      ParamCount: Integer; Parameters: array of String);

  end;

var
  frmOvoNote: TfrmOvoNote;

implementation

uses uRawEdit, strutils;

COnst
 COL_DONE = 1;
 COL_PRIORITY = 2;
 COL_TASK = 3;
 COL_DUE = 4;
 COL_CONTEXT = 5;
 COL_PROJECT = 6;
 COL_LINK = 7;
 COL_DELETE = 8;


{$R *.lfm}

{ TfrmOvoNote }
procedure TfrmOvoNote.TasktoGridRow(Task: TTask; Arow: Integer);
begin
    if Task.Done then
    gridTask.Cells[COL_DONE, aRow] := '1'
  else
    gridTask.Cells[COL_DONE, aRow] := '0';

  gridTask.Cells[COL_PRIORITY, aRow] := '   ' + Task.Priority + '   ';
  gridTask.Cells[COL_TASK, aRow] := Task.Task;

  if Task.Due <> 0 then
    gridTask.Cells[COL_DUE, aRow] := DateToStr(task.Due)
  else
    gridTask.Cells[COL_DUE, aRow] := '';

  gridTask.Cells[COL_CONTEXT, aRow] := Task.Contexts.Text;
  gridTask.Cells[COL_PROJECT, aRow] := Task.Projects.Text;
  gridTask.Cells[COL_LINK, aRow] := '  '+copy(Task.link, 1, pos(':', task.link)) + '  ';
  gridTask.Objects[0, aRow]:= Task;


end;

procedure TfrmOvoNote.LoadToGrid(FilterProject,FilterContext, FilterText:string);
var
  i: integer;
  Task: TTask;
  Idx: integer;
  r: TGridRect;
  FoundTasks:Integer;
begin
  TaskList.TaskSort;
  fLoading:= true;
  idx := -1;
  try
    Task := TTask(gridTask.objects[0,gridTask.Selection.Top]);
    if Assigned(Task) then
      idx := Task.Row;
  except
    idx := -1;
  end;

  FoundTasks := 0;

  try

    gridTask.RowCount:= 2;
    for i := 0 to TaskList.Count - 1 do
      begin
        Task := TTask(TaskList.Items[i]);
        if FilterProject <> EmptyStr then
           if Task.Projects.IndexOf(FilterProject) < 0 then
              Continue;

        if FilterContext <> EmptyStr then
           if Task.Contexts.IndexOf(FilterContext) < 0 then
              Continue;

        if FilterText <> EmptyStr then
           if not  AnsiContainsText(Task.Task, FilterText) then
              Continue;
        inc(FoundTasks);
        TasktoGridRow(Task, gridTask.RowCount -1);
        gridTask.RowCount:= gridTask.RowCount +1;
      end;

    gridTask.RowCount:= gridTask.RowCount -1;
    if (idx <> -1) and  (FoundTasks > 0) then
      for i := 1 To gridTask.RowCount -1 do
        begin
         if  TTask(gridTask.objects[0,i]).Row = idx then
           begin
             r.Top := i;
             r.Bottom := i;
             r.Left:=COL_DONE;
             r.Right:=COL_LINK;
             gridTask.Selection := r;
             Break;
           end;
        end;

  finally
    fLoading:=false;;
  end;
  ReloadFilters;
end;

procedure TfrmOvoNote.LoadToGrid(FilterProject, FilterContext: Integer);
VAR
  st1, st2: string;
begin
  if FilterProject > 0 then
     st1 := lbProjects.Items[lbProjects.ItemIndex]
  else
     st1 := '';

  if FilterContext > 0 then
     st2 := lbContexts.Items[lbContexts.ItemIndex]
  else
     st2 := '';

  LoadToGrid(st1, st2, edtSearch.Text);

end;

procedure TfrmOvoNote.LoadToGrid;
begin
  LoadToGrid(lbProjects.ItemIndex, lbContexts.ItemIndex);
end;

procedure TfrmOvoNote.ReloadFilters;
begin
  TaskList.GetContexts(lbContexts.Items);
  lbContexts.Items.Insert(0,ALL_ELEMENT);
  TaskList.GetProjects(lbProjects.Items);
  lbProjects.Items.Insert(0,ALL_ELEMENT);

end;

procedure TfrmOvoNote.LoadFile;
var
  TodoFile: TFileStream;
  Mode: Word;
begin
  if not Assigned(TaskList) then
    begin
       TaskList := TTaskList.Create;
       TaskList.OnChange := @TaskListChange;
    end;

  Mode := fmOpenRead;
  if not FileExists(dm.FilePath+FILE_TODO) then
    inc(mode, fmCreate);

  TodoFile := TFileStream.Create(dm.FilePath+FILE_TODO, Mode);
  TodoFile.Position := 0;
  TaskList.Clear;
  TaskList.LoadFromStream(TodoFile);

  gridTask.RowCount := TaskList.Count + 1;
  LoadToGrid('','','');

  gridTask.Invalidate;
  ReloadFilters;

  TodoFile.Free;
end;

procedure TfrmOvoNote.gridTaskCheckboxToggled(Sender: TObject; aCol, aRow: integer; aState: TCheckboxState);
var
   Task: TTask;
begin
  Task := TTask(gridTask.Objects[0,arow]);
  if aCol = COL_DELETE then
    begin
      TaskList.Delete(aRow-1);
      LoadToGrid();
    end
  else
     begin
      Task.Done:=aState = cbChecked;
      TasktoGridRow(Task, aRow);
     end;
end;

procedure TfrmOvoNote.gridTaskHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  r: TGridRect;
begin
  If not IsColumn then
    begin
     r.Top:=Index;
     r.Bottom:=index;
     r.Left:=COL_DONE;
     r.Right:=COL_LINK;
     gridTask.Selection := r;

    end;
end;

procedure TfrmOvoNote.gridTaskButtonClick(Sender: TObject; aCol, aRow: Integer);
var
 Task: TTask;
begin
  if aCol = COL_LINK then
    begin
       Task := TTask(gridTask.Objects[0, Arow]);
       OpenDocument(Task.link);
    end;
end;

procedure TfrmOvoNote.actArchiveExecute(Sender: TObject);
var
  Arch: TTaskList;
  Task: TTask;
  DoneStream: TFileStream;
  i:integer;
  Mode: word;
begin
  Arch := tTaskList.Create;

  for i:= TaskList.Count -1 downto 0 do
    begin
      Task := TTask(TaskList.Items[i]);
      if Task.Done then
        begin
         TaskList.Extract(Task);
         Arch.Add(Task);
        end;
    end;

  if FileExists(dm.FilePath+FILE_DONE) then
    begin
     CopyFile(dm.FilePath+FILE_DONE, ChangeFileExt(dm.FilePath+FILE_DONE,'.bak'));
     Mode := fmOpenReadWrite
    end
  else
     Mode:= fmOpenWrite+ fmCreate;

  DoneStream:= TFileStream.Create(dm.FilePath+FILE_DONE, mode);

  DoneStream.Seek(0,soEnd);
  Arch.AppendToStream(DoneStream);
  DoneStream.free;
  FileSaveAs1.Execute;
  LoadToGrid();

end;

procedure TfrmOvoNote.actDeleteExecute(Sender: TObject);
var
  x: integer;
begin
  x:=gridTask.Selection.top;
  TaskList.Remove(gridTask.Objects[0, x]);
  LoadToGrid();
  ResizeGrid;
end;

Procedure TfrmOvoNote.CheckAutoSave;
begin
  if dm.AutoSave and  TaskList.Modified then
    FileSaveAs1.Execute;

end;

procedure TfrmOvoNote.actEditExecute(Sender: TObject);
begin

  CheckAutoSave;

  if TaskList.Modified then
   Case MessageDlg(RS_SAVE,mtWarning, mbYesNoCancel,0) of
      mrYes : FileSaveAs1.Execute;
      mrCancel : exit;
   end;

  fRawEdit:=TfRawEdit.Create(Self, false);
  try
    if fRawEdit.showModal = mrModified then
      FileOpen1.Execute;
  finally
    fRawEdit.free;
  end;
  FileOpen1.Execute;
end;

procedure TfrmOvoNote.ActionList1Update(AAction: TBasicAction;
  var Handled: Boolean);
begin
  FileSaveAs1.Enabled:= TaskList.Modified;
end;

procedure TfrmOvoNote.actSettingsExecute(Sender: TObject);
begin
 if not Assigned(fSettings) then
   fSettings:= TfSettings.Create(Self);
 fSettings.Show;
end;

procedure TfrmOvoNote.actShowDoneExecute(Sender: TObject);
begin
  try
    fRawEdit:=TfRawEdit.Create(Self, True);
    fRawEdit.showModal;
  finally
    fRawEdit.free;
  end;
end;

procedure TfrmOvoNote.actTerminateExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmOvoNote.bSearchClick(Sender: TObject);
begin
  edtSearch.Text:='';
  LoadToGrid();
end;

procedure TfrmOvoNote.edtNewTaskKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_RETURN : if edtNewTask.Text <> EmptyStr then
                   actAdd.Execute;
    VK_ESCAPE : edtNewTask.Clear;
  end;
end;

procedure TfrmOvoNote.edtSearchChange(Sender: TObject);
begin
  if edtSearch.Text = '' then
    dm.imglSmall.GetBitmap(0, bSearch.Glyph)
  else
    begin
       dm.imglSmall.GetBitmap(1, bSearch.Glyph);
    end;

  LoadToGrid();

end;

procedure TfrmOvoNote.edtPriorityKeyPress(Sender: TObject; var Key: char);
begin
  if (upcase(key) in [#32, 'A'..'Z']) then
    edtPriority.Text:= upcase(KEY);
  if Key = #27 then
    edtTask.Undo;

end;

procedure TfrmOvoNote.edtSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    case key of
    VK_ESCAPE : edtSearch.Clear;
  end;
end;

procedure TfrmOvoNote.edtTaskKeyPress(Sender: TObject; var Key: char);
begin

  case key of
    #13 : begin
        gridTaskSetEditText(gridTask,gridTask.Col, gridTask.Row, edtTask.Text);
        gridTask.EditorMode :=false;
    end;
    #27 : edtTask.Undo;
  end;
end;

procedure TfrmOvoNote.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Application.ShowMainForm:=False;
  CheckAutoSave;
  if TaskList.Modified then
    Case MessageDlg(RS_SAVE,mtWarning, mbYesNoCancel,0) of
      mrYes : FileSaveAs1.Execute;
      mrCancel : exit;
    end;

  CloseAction:=caHide;
end;

procedure TfrmOvoNote.UniqueInstanceIOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
begin
  //
  Self.show;
end;

procedure TfrmOvoNote.FormCreate(Sender: TObject);
begin

  UniqueInstanceI:= TUniqueInstance.Create(Self);
  with UniqueInstanceI do
    begin
      Identifier := AppNameServerID;
      UpdateInterval := 500;
      OnOtherInstance := @UniqueInstanceIOtherInstance;
      Enabled := True;
      Loaded;
    end;

end;

procedure TfrmOvoNote.FormDestroy(Sender: TObject);
begin
  TaskList.Free ;
  UniqueInstanceI.free;
end;

procedure TfrmOvoNote.actAddExecute(Sender: TObject);
var
  Task : TTask;
begin
  if edtNewTask.Text = EmptyStr then
    exit;

  Task := TaskList.ItemFromRow(edtNewTask.Text);
  edtNewTask.Clear;
  TaskList.Add(Task);
  task.Modified:= true;
  LoadToGrid();

end;

procedure TfrmOvoNote.gridTaskPrepareCanvas(Sender: TObject; aCol, aRow: integer; aState: TGridDrawState);
var
  MyTextStyle: TTextStyle;
  Task: TTask;
begin
  if (aRow = 0) or not assigned(TaskList) or (TaskList.Count = 0) then
    exit;
  Task := TTask(gridTask.Objects[0,Arow]);
  if not Assigned(Task) then
    exit;
  gridTask.Canvas.Font := gridTask.Font;
  case aCol of
    COL_DONE:
      begin
      end;
    COL_PRIORITY:
      begin
        gridTask.Canvas.Font.Size := -15;
      end;
    COL_TASK:
      begin
      end;
    else
      begin
        MyTextStyle := gridTask.Canvas.TextStyle;
        MyTextStyle.SingleLine := False;
        gridTask.Canvas.TextStyle := MyTextStyle;
      end;
    end;

  if Task.Done then
    gridTask.Canvas.Font.Color := clGrayText;
end;

procedure TfrmOvoNote.gridTaskDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);

var
  X, i: integer;
  Parola:string;
  Function IsKey: boolean;  inline;
  var
    ps:integer;
  begin
    ps := Pos(':',Parola);
    result :=(ps > 0) and (ps < Length(Parola));
  end;

begin
  if (aRow > 0) and (aCol = COL_DONE) then
    begin
      aState:= [gdFixed];
    end;

  if (aRow > 0) and (aCol = COL_TASK) then
    begin
      //
      x:= aRect.Left +2;

      gridTask.Canvas.FillRect(Arect);
      TaskList.Parser.Stringa:=gridTask.Cells[aCol,aRow];
      TaskList.Parser.Parse;

      for i:= 0 to  TaskList.Parser.Count -1 do
        begin
           Parola:= TaskList.Parser.Item[i]+' ';
           if Parola[1] in ['+','@'] then
              begin
                gridTask.Canvas.Brush.Color := clDefault;
                gridTask.Canvas.Font.Style:=[fsBold];
                gridTask.Canvas.TextOut(X, arect.top+1, Parola);
              end
           else
           if IsKey then
              begin
                gridTask.Canvas.Font.Style:=[];
                gridTask.Canvas.Brush.Color := clInactiveCaption;
                gridTask.Canvas.TextOut(X, arect.top+1, Parola);
              end
           else
              begin
                gridTask.Canvas.Font.Style:=[];
                gridTask.Canvas.Brush.Color := clDefault;
                gridTask.Canvas.TextOut(X, arect.top, Parola);
              end;

           inc(x, gridTask.Canvas.GetTextWidth(Parola));

        end;
    end
  else
    gridTask.DefaultDrawCell(aCol, aRow, aRect, aState);
end;

procedure TfrmOvoNote.gridTaskExit(Sender: TObject);
begin
  gridTask.EditorMode := false;
end;

procedure TfrmOvoNote.gridTaskSelectEditor(Sender: TObject; aCol, aRow: integer; var Editor: TWinControl);
var
  aRect: Trect;
  Task: TTask;
begin
  aRect := gridTask.CellRect(aCol, aRow);
  Task := TTask(gridTask.Objects[0,aROW]);
  if not Assigned(Task) or fLoading then
     begin
       Editor:= nil;
       exit;
     end;

  case acol of
    COL_DONE:
      begin
      end;
    COL_task:
      begin
        edtTask.BoundsRect := aRect;
        Editor := edtTask;
        edtTask.Text := task.Task;
      end;

    COL_PRIORITY:
      begin
        edtPriority.BoundsRect := aRect;
        edtPriority.Font.Size := -15;

        Editor := edtPriority;
        edtPriority.Text := task.Priority;
      end;
    COL_CONTEXT:
      begin
        editor := nil;
        //mList.BoundsRect := aRect;
        //Editor := mList;
        //mList.Text := Task.Contexts.Text;
      end;
    COL_PROJECT:
      begin
        editor := nil;
        //mList.BoundsRect := aRect;
        //Editor := mList;
        //mList.Text := Task.Projects.Text;
      end;

    COL_DUE:
      begin
        dtpDueDate.BoundsRect := aRect;
        Editor := dtpDueDate;
        if Task.Due <> 0 then
          dtpDueDate.Date := Task.Due
        else
          dtpDueDate.Date := NullDate;
      end;
   end;
end;

procedure TfrmOvoNote.gridTaskSetEditText(Sender: TObject; ACol, ARow: integer; const Value: string);
var
  Task: TTask;
begin
  if (aRow = 0) or not assigned(TaskList) or (TaskList.Count = 0) or fLoading  then
    exit;
  Task := TTask(gridTask.Objects[0,Arow]);

  case ACol of
    COL_DONE:
      begin
      end;
    COL_PRIORITY:
      begin
        Task.Priority := (trim(edtPriority.Text)+#00)[1];
      end;
    COL_TASK:
      begin
        Task.Task := edtTask.Text;
      end;
    COL_DUE:
      begin
        if dtpDueDate.Date = NullDate then
           Task.Due := 0
        else
          Task.Due := dtpDueDate.Date;
      end;
    COL_CONTEXT:
      begin
        Task.Contexts.Text := mList.Lines.Text;
      end;
    COL_PROJECT:
      begin
        Task.Projects.Text := mList.Lines.Text;
      end;
    COL_LINK:
      begin
      end;
    end;

  TasktoGridRow(Task, ARow);
  ReloadFilters;
  gridTask.InvalidateRow(ARow);


end;

procedure TfrmOvoNote.TaskListChange(Sender: TObject);
begin
  if dm.AutoSave then
    begin
      TimerAutoSave.Enabled:=false;
      TimerAutoSave.Enabled:=True;
    end;
end;

procedure TfrmOvoNote.FileOpen1Execute(Sender: TObject);
begin
  LoadFile;
end;

procedure TfrmOvoNote.FileSaveAs1Execute(Sender: TObject);
var
  TodoFile: TFileStream;
  Mode: Word;
begin
  if FileExists(dm.FilePath+FILE_TODO) then
    begin
     CopyFile(dm.FilePath+FILE_TODO, ChangeFileExt(dm.FilePath+FILE_TODO,'.bak'));
     Mode := fmOpenReadWrite
    end
  else
     Mode:= fmOpenWrite+ fmCreate;

  CopyFile(dm.FilePath+FILE_TODO, ChangeFileExt(dm.FilePath+FILE_TODO,'.bak'));

  TodoFile := TFileStream.Create(dm.FilePath+FILE_TODO, Mode);
  TaskList.SaveToStream(TodoFile);
  TaskList.Modified:= false;
  TodoFile.Free;
end;

procedure TfrmOvoNote.FormResize(Sender: TObject);
begin
  ResizeGrid;
  lbProjects.Height := pnlFilter.ClientHeight div 2;
end;


procedure TfrmOvoNote.ResizeGrid;
var
  i, TotSize: integer;
begin
  gridTask.AutoSizeColumns;

  gridTask.ColWidths[0]:= 15;
  gridTask.ColWidths[COL_DUE]:= dtpDueDate.Width;
  totSize :=  gridTask.ColWidths[0];


  for i := 1 to gridTask.Columns.Count do
    Inc(TotSize, gridTask.ColWidths[i]);

  if TotSize < gridTask.ClientWidth then
    gridTask.ColWidths[COL_TASK] := gridTask.ColWidths[COL_TASK] + (gridTask.ClientWidth - (TotSize + 10));
end;

procedure TfrmOvoNote.FormShow(Sender: TObject);
begin
  LoadFile;
  ResizeGrid;
  tbClose.Align:=alRight;
  Panel2.Align:=alRight;

end;

procedure TfrmOvoNote.gridTaskUserCheckboxBitmap(Sender: TObject; const aCol,
  aRow: Integer; const CheckedState: TCheckboxState; var ABitmap: TBitmap);
begin
  if aCol = COL_DELETE then
     dm.imglSmall.GetBitmap(1, ABitmap)
  else
    if CheckedState = cbChecked then
       dm.imglSmall.GetBitmap(2, ABitmap)
    else
       dm.imglSmall.GetBitmap(3, ABitmap);
end;

procedure TfrmOvoNote.lbProjectsSelectionChange(Sender: TObject; User: boolean);
begin
  LoadToGrid();
end;

procedure TfrmOvoNote.MenuItem1Click(Sender: TObject);
begin
  Application.terminate;
end;

procedure TfrmOvoNote.TimerAutoSaveTimer(Sender: TObject);
begin
  TimerAutoSave.Enabled:=false;
  CheckAutoSave;

end;

procedure TfrmOvoNote.TrayIconDblClick(Sender: TObject);
begin
  if Visible then
    Hide
  else
    begin
      Show;
      BringToFront;
    end;
end;

end.
