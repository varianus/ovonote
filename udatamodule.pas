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
unit udatamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazConfigStorage, XMLPropStorage, Controls;

Const
  FILE_TODO = 'todo.txt';
  FILE_DONE = 'done.txt';

  mrModified = mrNone + 100;

Resourcestring
 ALL_ELEMENT = '<All>';
 RS_SAVE = 'Task list have been modified.'+sLineBreak+
           'Do you want to save changes?';



type
  { Tdm }

  Tdm = class(TDataModule)
    imglEnabled: TImageList;
    imglSmall: TImageList;
    imglDisabled: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    Config: TXMLConfigStorage;
    function getAutoSave: boolean;
    function getfilePath: string;
    procedure setAutoSave(AValue: boolean);
    procedure setfilePath(AValue: string);
  public
    property FilePath: string read getfilePath write setfilePath;
    property AutoSave: boolean read getAutoSave write setAutoSave;

    procedure SaveConfig;
  end;

var
  dm: Tdm;

implementation

{$R *.lfm}

{ Tdm }

procedure Tdm.DataModuleCreate(Sender: TObject);
begin
  Config := TXMLConfigStorage.Create('ovonote.cfg', FileExists('ovonote.cfg'));
end;

procedure Tdm.DataModuleDestroy(Sender: TObject);
begin
  Config.free;
end;

function Tdm.getfilePath: string;
begin
  result := ExpandFileName(Config.GetValue('Files/Path','.'));
end;

function Tdm.getAutoSave: boolean;
begin
  result := Config.GetValue('Files/AutoSave', false);
end;

procedure Tdm.setAutoSave(AValue: boolean);
begin
   Config.SetValue('Files/AutoSave',aValue);
end;

procedure Tdm.setfilePath(AValue: string);
begin
  Config.SetValue('Files/Path',aValue);
end;

procedure Tdm.SaveConfig;
begin
  Config.WriteToDisk;
end;

end.

