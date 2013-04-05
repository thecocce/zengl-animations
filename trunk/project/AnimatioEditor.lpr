program AnimatioEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, Interfaces, zglHeader, runtimetypeinfocontrols, pl_virtualtrees,
  u_main, u_animation, u_render, u_editorhistory, u_prop_form, u_db,
  u_proj_form, u_preview_form;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.CreateForm(TProjectForm, ProjectForm);
  Application.CreateForm(TPreviewBlenderForm, PreviewBlenderForm);
  Application.Run;
end.

