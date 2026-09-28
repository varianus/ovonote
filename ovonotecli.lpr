program ovonotecli;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  SysUtils,
  todo_parser,
  lazutf8,
  XMLConf,
  Windows, Math,
  CustApp { you can add units after this }
  ;

type

  { TTodoCLI }

  TTodoCLI = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TCliProcessor }

  TCliProcessor = class
  private
    FMaxPriority: string;
    TaskList: TTaskList;
    procedure SetMaxPriority(AValue: string);
  public
    Constructor Create;
    procedure LoadFromFile;
    procedure ExtractActive;
    procedure ExtractCompleted;
    DEstructor Destroy; override;
    Property MaxPriority: string read FMaxPriority write SetMaxPriority;
  end;

var
  Application: TTodoCLI;

  Renderer: THighLightKind;


  procedure ProcessCli;
  var
    Processor: TCliProcessor;
    ExtractActive, ExtractCompleted : boolean;
  begin
    Processor := TCliProcessor.Create;
    Processor.LoadFromFile;

    ExtractActive := Application.HasOption('a','active');
    ExtractCompleted :=  Application.HasOption('c', 'completed');

    if Application.HasOption('p','priority') then
      Processor.MaxPriority := UpperCase(Application.GetOptionValue('p','priority'));

    if not (ExtractActive or ExtractCompleted) then
    begin
      ExtractActive := true;
      ExtractCompleted := true;
    end;

    if ExtractActive then
      Processor.ExtractActive;

    if ExtractCompleted then
       Processor.ExtractCompleted;

    Processor.Free;

  end;

  { TTodoCLI }


  procedure TTodoCLI.DoRun;
  var
    ErrorMsg: string;
  begin
    SetTextCodePage(Output, CP_UTF8);
    SetConsoleOutputCP(CP_UTF8);

    // quick check parameters
    ErrorMsg := CheckOptions('hacts:p:', 'help active completed tag sort: priority:');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { add your program here }
    if HasOption('t', 'tag') then
      Renderer := hlkHTML
    else
      Renderer := hlkNone;



    ProcessCli;

    // stop program loop
    Terminate;
  end;

const
  FILE_TODO = 'todo.txt';

  { TCliProcessor }

procedure TCliProcessor.SetMaxPriority(AValue: string);
begin
  if FMaxPriority = AValue then Exit;
  FMaxPriority := AValue;
end;

constructor TCliProcessor.Create;
begin
  MaxPriority := 'Z';
end;

  procedure TCliProcessor.LoadFromFile;
  var
    TodoFile: TFileStream;
    Mode: word;
    Config: TXMLConfig;
    TodoFileName: String;

  begin
    Config := TXMLConfig.Create(nil);
    Config.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'ovonote.cfg');

    if not Assigned(TaskList) then
      TaskList := TTaskList.Create;

    TodoFileName := UTF8Encode(IncludeTrailingPathDelimiter(ExpandFileName(Config.GetValue('Files/Path','.')))+FILE_TODO);

     Mode := fmOpenRead + fmShareDenyNone;
    if not FileExists(TodoFileName) then
      Inc(mode, fmCreate);

    TodoFile := TFileStream.Create(TodoFileName, Mode);
    if not assigned (TodoFile) then
      Writeln('ERROR ->', SysErrorMessage(GetLastOSError));
    TodoFile.Position := 0;
    TaskList.Clear;
    TaskList.LoadFromStream(TodoFile);

    TodoFile.Free;
  end;

  procedure TCliProcessor.ExtractActive;
  var
    i: integer;
    Task: TTask;
  begin
    TaskList.Tasksort;
    for i := 0 to TaskList.Count - 1 do
      begin
        Task:= TTask(TaskList[i]);
      if not Task.done and ((task.Priority = #00) or (task.priority <= MaxPriority))  then
        WriteLn(utf8toconsole(TaskList.RowFromItem(Task, Renderer)));

      end;

  end;

  procedure TCliProcessor.ExtractCompleted;
  var
    i: integer;
    Task: TTask;
  begin
    TaskList.Tasksort;
    for i := 0 to TaskList.Count - 1 do
      begin
        Task:= TTask(TaskList[i]);
      if Task.done and ((task.Priority = #00) or (task.priority <= MaxPriority))  then
        WriteLn(utf8toconsole(TaskList.RowFromItem(Task, Renderer)));

      end;

  end;

  destructor TCliProcessor.Destroy;
begin
  TaskList.free;
  inherited Destroy;
end;

  constructor TTodoCLI.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TTodoCLI.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TTodoCLI.WriteHelp;
  begin
    { add your help code here }
    writeln('ovonotecli  - Estract todo to console');
    writeln('Usage: ', ExeName, ' [OPTIONS]');
    WriteLn(' Estract todo items to console');
    WriteLn(' if no parameters are passed,  the default is " --active --completed"');
    writeln(' -h, --help ');
    writeln('     this messagge ');
    writeln(' -a, --active ');
    writeln('     extract active event  ');
    writeln(' -c, --completed ');
    writeln('     extract active event  ');
    writeln(' -p, --priority=MAXPRIORITY ');
    writeln('     extract only task with priority up to MAXPRIORITY ');
    writeln(' -t, --tag ');
    writeln('     use html tags for project, context and properties ');

  end;

begin
  Application := TTodoCLI.Create(nil);
  Application.Title := 'OvoNote';
  Application.Run;
  Application.Free;

end.
