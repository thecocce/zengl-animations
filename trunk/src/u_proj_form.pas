unit u_proj_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ValEdit, ExtCtrls, StdCtrls, Buttons, u_render;

type

  { TProjectForm }

  TProjectForm = class(TForm)
    AnimationFileName: TEdit;
    btnBrowse: TButton;
    btnSave: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    MainPanel: TPanel;
    od: TSaveDialog;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    Project: TEditorProject;
  public
    procedure Init(Proj: TEditorProject);
  end;

var
  ProjectForm: TProjectForm;

implementation

{$R *.lfm}

{ TProjectForm }

procedure TProjectForm.btnBrowseClick(Sender: TObject);
begin
  if od.Execute then
    AnimationFileName.Text :=  ExtractRelativepath(Project.ProjectFileName, od.FileName);
end;

procedure TProjectForm.btnSaveClick(Sender: TObject);
begin
  Project.AnimationFileName := AnimationFileName.Text;
end;

procedure TProjectForm.Init(Proj: TEditorProject);
begin
  Project := Proj;
  AnimationFileName.Text := Project.AnimationFileName;
end;

end.

