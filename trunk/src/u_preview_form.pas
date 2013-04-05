unit u_preview_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst, u_animation;

type

  { TPreviewBlenderForm }

  TPreviewBlenderForm = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    Blenders: TCheckListBox;
    Label1: TLabel;
    Panel1: TPanel;
  private
    { private declarations }
  public
    procedure FillBlender(Prototype: anAnimationPrototype);
  end;

var
  PreviewBlenderForm: TPreviewBlenderForm;

implementation

{$R *.lfm}

{ TPreviewBlenderForm }

procedure TPreviewBlenderForm.FillBlender(Prototype: anAnimationPrototype);
var
  i: Integer;
begin
  Blenders.Clear;
  for i := 0 to Prototype.Animation.Animations.Count - 1 do begin
    if Prototype.Animation.Animations.Data[i].Prototype = Prototype then begin
      Blenders.Items.AddObject(Prototype.Animation.Animations.Keys[i], Prototype.Animation.Animations.Data[i]);
    end;
  end;
  for i := 0 to Blenders.Count - 1 do
    Blenders.Checked[i] := true;
end;

end.

