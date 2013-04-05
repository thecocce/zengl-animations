unit u_prop_form;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, ComCtrls, StdCtrls, ExtCtrls, CheckLst, u_animation, zglHeader;

type

  { TPropertiesForm }

  TPropertiesForm = class(TForm)
    btnBrowse: TButton;
    btnExport: TButton;
    btnResetSize: TButton;
    btnUpdate: TButton;
    ButtonsPanel: TButtonPanel;
    objApply: TCheckListBox;
    ObjectProperties: TTIPropertyGrid;
    objInfo: TLabel;
    ttl: TLabel;
    PropContent: TPageControl;
    anTexture: TTabSheet;
    anTextureContent: TTabSheet;
    anAnimationBlender: TTabSheet;
    prSplitter: TSplitter;
    procedure anAnimationBlenderShow(Sender: TObject);
    procedure anTextureContentShow(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnResetSizeClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure ButtonsPanelClick(Sender: TObject);
    procedure objApplyClickCheck(Sender: TObject);
    procedure ObjectPropertiesClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    HandleObj: TObject;
  public
    procedure HandleObject(Obj: TObject);
    procedure RehandleObject;
    procedure FillTextureData(TextureData: u_animation.anTextureContent);
  end;

var
  PropertiesForm: TPropertiesForm;

implementation

uses u_main;

{$R *.lfm}

{ TPropertiesForm }

procedure TPropertiesForm.ObjectPropertiesClick(Sender: TObject);
begin

end;

procedure TPropertiesForm.OKButtonClick(Sender: TObject);
begin
  ObjectProperties.SaveChanges;
  ObjectProperties.Selection.Clear;
  ObjectProperties.TIObject := nil;
end;

procedure TPropertiesForm.HandleObject(Obj: TObject);
var
  i: Integer;
  tab: TTabSheet;
begin
  HandleObj := Obj;
  for i := 0 to PropContent.PageCount - 1 do begin
    tab := PropContent.Pages[i];
    if Obj.InheritsFrom(FindClass(tab.Caption)) then begin
       PropContent.ActivePage := tab;
       PropContent.Visible := true;
       if Assigned(tab.OnShow) then
          tab.OnShow(tab);
       exit;
    end;
  end;
  PropContent.Visible := false;
end;

procedure TPropertiesForm.RehandleObject;
var
  obj: TPersistent;
begin
  ObjectProperties.Selection.Clear;
  obj := ObjectProperties.TIObject;
  ObjectProperties.TIObject := nil;
  ObjectProperties.TIObject := obj;
end;

procedure TPropertiesForm.FillTextureData(
  TextureData: u_animation.anTextureContent);
begin
  objInfo.Caption := 'FileName: "' +
    file_GetName(TextureData.FileName) +
    '"; Extention: "' + TextureData.Extension +
    '"; Memory size: ' + IntToStr(TextureData.Content.Size);
  btnUpdate.Caption := 'Update from "' + file_GetName(TextureData.FileName) + '.' + file_GetExtension(TextureData.FileName) + '"';
  btnUpdate.Enabled := FileExists(TextureData.FileName);
end;

procedure TPropertiesForm.ButtonsPanelClick(Sender: TObject);
begin

end;

procedure TPropertiesForm.objApplyClickCheck(Sender: TObject);
var prxy: anBlenderObjectProxy;
begin
  if objApply.ItemIndex >= 0 then begin
    prxy := anBlenderObjectProxy(objApply.Items.Objects[objApply.ItemIndex]);
    prxy.Apply := objApply.Checked[objApply.ItemIndex];
  end;
end;

procedure TPropertiesForm.btnResetSizeClick(Sender: TObject);
var
  Tex: u_animation.anTexture;
begin
  Tex := u_animation.anTexture(ObjectProperties.TIObject);
  if Assigned(Tex.FileContent) then begin
    Tex.Width := Tex.FileContent.Width;
    Tex.Height := Tex.FileContent.Height;
    RehandleObject;
  end;
end;

procedure TPropertiesForm.btnUpdateClick(Sender: TObject);
var TextureData: u_animation.anTextureContent;
begin
  TextureData := u_animation.anTextureContent(HandleObj);
  if file_Exists(TextureData.FileName) then begin
    if TextureData.LoadFromFile(TextureData.FileName, MainForm.GetSavePromt(nil, TextureData.FileName)) then begin
      btnUpdate.Caption := 'Updated!';
    end;
  end;
end;

procedure TPropertiesForm.btnBrowseClick(Sender: TObject);
var
  od: TOpenDialog;
  TextureData: u_animation.anTextureContent;
begin
  TextureData := u_animation.anTextureContent(HandleObj);
  od := TOpenDialog.Create(nil);
  od.InitialDir := file_GetDirectory(TextureData.FileName);
  if od.Execute then begin
    TextureData.LoadFromFile(od.FileName, MainForm.GetSavePromt(od));
    FillTextureData(TextureData);
  end;
  od.Free;
end;

procedure TPropertiesForm.anTextureContentShow(Sender: TObject);
begin
  FillTextureData(u_animation.anTextureContent(HandleObj));
end;

procedure TPropertiesForm.anAnimationBlenderShow(Sender: TObject);
var
  blender: u_animation.anAnimationBlender;
  prxy: anBlenderObjectProxy;
  i: Integer;
begin
  blender := u_animation.anAnimationBlender(HandleObj);
  objApply.Clear;
  for i := 0 to blender.Proxy.Count - 1 do begin
    prxy := blender.Proxy[i];
    objApply.AddItem(prxy.ProxyTo.Name, prxy);
    objApply.Checked[objApply.Count - 1] := prxy.Apply;
  end;
end;

procedure TPropertiesForm.btnExportClick(Sender: TObject);
var
  od: TSaveDialog;
  TextureData: u_animation.anTextureContent;
begin
  TextureData := u_animation.anTextureContent(HandleObj);
  od := TSaveDialog.Create(nil);
  od.InitialDir := file_GetDirectory(TextureData.FileName);
  od.Options := [ofOverwritePrompt];
  if od.Execute then begin
    TextureData.SaveToFile(od.FileName);
  end;
  od.Free;
end;

end.

