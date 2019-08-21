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

program ovonote;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fovonote, todo_parser, USStringParser,
  todotxtsyn_highlighter, uRawEdit,
  DefaultTranslator, datetimectrls, usettings, udatamodule;

{$R *.res}

begin

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(TfrmOvoNote, frmOvoNote);
  Application.Run;
end.

