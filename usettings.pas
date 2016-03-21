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
unit usettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IDEWindowIntf, Forms, Controls, Graphics,
  Dialogs, IniPropStorage, ExtCtrls, StdCtrls, EditBtn, ButtonPanel,
  udatamodule;

type

  { TfSettings }

  TfSettings = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbAutosave: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fSettings: TfSettings;

implementation

{$R *.lfm}

{ TfSettings }

procedure TfSettings.FormShow(Sender: TObject);
begin
  DirectoryEdit1.Directory := dm.FilePath;

end;

procedure TfSettings.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
  Close;
end;

procedure TfSettings.OKButtonClick(Sender: TObject);
begin
 dm.FilePath:= DirectoryEdit1.Directory;
 dm.SaveConfig;
 Close;
end;

end.

