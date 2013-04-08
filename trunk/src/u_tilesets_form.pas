unit u_tilesets_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, u_animation;

type

  { TTileSetsForm }

  TTileSetsForm = class(TForm)
    btnRenameTileSet: TToolButton;
    btnSave: TButton;
    edtTile: TEdit;
    edtFrame: TEdit;
    TilesList: TListView;
    TileSetPanel: TPanel;
    TileSetLabel: TLabel;
    TileSetsList: TListView;
    ToolBar1: TToolBar;
    btnAddSet: TToolButton;
    btnDeleteSet: TToolButton;
    ToolBar2: TToolBar;
    btnAddTile: TToolButton;
    btnDeleteTile: TToolButton;
    procedure Bevel1ChangeBounds(Sender: TObject);
    procedure btnAddSetClick(Sender: TObject);
    procedure btnAddTileClick(Sender: TObject);
    procedure btnDeleteSetClick(Sender: TObject);
    procedure btnDeleteTileClick(Sender: TObject);
    procedure btnRenameTileSetClick(Sender: TObject);
    procedure edtFrameChange(Sender: TObject);
    procedure edtTileChange(Sender: TObject);
    procedure TileSetsListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure TilesListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    Texture: anAnimatedSpriteTexture;
    SelectedTile: anAnimatedSpriteTextureTileSet;

    procedure RegisterTile(pTile: anAnimatedSpriteTextureTile);
    procedure RegisterTileSet(pTileSet: anAnimatedSpriteTextureTileSet);

    procedure UnRegisterTile(pTile: anAnimatedSpriteTextureTile);
    procedure UnRegisterTileSet(pTileSet: anAnimatedSpriteTextureTileSet);

    procedure HandleTileSet(pTileSet: anAnimatedSpriteTextureTileSet);
    procedure HandleTile(pTile: anAnimatedSpriteTextureTile);
  public
    procedure ResetTile;
    procedure Reset;
    procedure Edit(pTexture: anAnimatedSpriteTexture);
  end;

var
  TileSetsForm: TTileSetsForm;

implementation

uses u_main;

{$R *.lfm}

{ TTileSetsForm }

procedure TTileSetsForm.TileSetsListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  tile_set: anAnimatedSpriteTextureTileSet;
begin
  btnRenameTileSet.Enabled := Item <> nil;
  btnDeleteSet.Enabled := Item <> nil;
   if Assigned(Item) then begin
    tile_set := anAnimatedSpriteTextureTileSet(Item.Data);
    if Assigned(tile_set) then begin
      HandleTileSet(tile_set);
    end;
   end;
end;

procedure TTileSetsForm.TilesListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  tile_id: anAnimatedSpriteTextureTile;
begin
  btnDeleteTile.Enabled := Item <> nil;
  if Assigned(Item) then begin
    tile_id := anAnimatedSpriteTextureTile(Item.Data);
    if Assigned(tile_id) then begin
      HandleTile(tile_id);
    end else
      ResetTile;
  end else
    ResetTile;
end;

procedure TTileSetsForm.RegisterTile(pTile: anAnimatedSpriteTextureTile);
begin
  with TilesList.Items.Add do begin
    Caption := IntToStr(pTile.Tile);
    ImageIndex := 4;
    SubItems.Add(FloatToStr(pTile.Time));
    Data := pTile;
  end;
end;

procedure TTileSetsForm.btnAddSetClick(Sender: TObject);
var
  tile_set: anAnimatedSpriteTextureTileSet;
begin
  tile_set := anAnimatedSpriteTextureTileSet.Create(Texture);
  Texture.TileSets.Add(tile_set.GetUniqueName(InputBox('Enter tile set name', 'Please enter tile set name', 'NONAME')), tile_set);
  RegisterTileSet(tile_set);
  HandleTileSet(tile_set);
end;

procedure TTileSetsForm.btnAddTileClick(Sender: TObject);
var
  tile_id: anAnimatedSpriteTextureTile;
begin
  tile_id := anAnimatedSpriteTextureTile.Create;
  SelectedTile.TileList.Add(tile_id);
  RegisterTile(tile_id);
  HandleTile(tile_id);
end;

procedure TTileSetsForm.btnDeleteSetClick(Sender: TObject);
var
  sel: TListItem;
  tile_set: anAnimatedSpriteTextureTileSet;
begin
  sel := TileSetsList.Selected;
  if Assigned(sel) then begin
    tile_set := anAnimatedSpriteTextureTileSet(sel.Data);
    if MessageDlg('Are you shure to delete tile set ' + tile_set.Name + ' ?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      UnRegisterTileSet(tile_set);
      Texture.TileSets.Remove(tile_set.Name);
      Reset;
    end;
  end;
end;

procedure TTileSetsForm.btnDeleteTileClick(Sender: TObject);
var
  sel: TListItem;
  tile_id: anAnimatedSpriteTextureTile;
begin
  sel := TilesList.Selected;
  if Assigned(sel) then begin
    tile_id := anAnimatedSpriteTextureTile(sel.Data);
    if MessageDlg('Are you shure to delete this tile?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      UnRegisterTile(tile_id);
      SelectedTile.TileList.Remove(tile_id);
      ResetTile;
    end;
  end;
end;

procedure TTileSetsForm.Bevel1ChangeBounds(Sender: TObject);
begin

end;

procedure TTileSetsForm.btnRenameTileSetClick(Sender: TObject);
var
  sel: TListItem;
  tile_set: anAnimatedSpriteTextureTileSet;
begin
  sel := TileSetsList.Selected;
  if Assigned(sel) then begin
    tile_set := anAnimatedSpriteTextureTileSet(sel.Data);
    tile_set.Name := InputBox('Enter tile set name', 'Please enter tile set name', tile_set.Name);
    sel.Caption := tile_set.Name;
    HandleTileSet(tile_set);
  end;
end;

procedure TTileSetsForm.edtFrameChange(Sender: TObject);
var
  sel: TListItem;
  tile_id: anAnimatedSpriteTextureTile;
begin
  sel := TilesList.Selected;
  if Assigned(sel) then begin
    tile_id := anAnimatedSpriteTextureTile(sel.Data);
    tile_id.Time := StrToFloatDef(edtFrame.Text, tile_id.Time);
    sel.SubItems[0] := FloatToStr(tile_id.Time);
  end;
end;

procedure TTileSetsForm.edtTileChange(Sender: TObject);
var
  sel: TListItem;
  tile_id: anAnimatedSpriteTextureTile;
begin
  sel := TilesList.Selected;
  if Assigned(sel) then begin
    tile_id := anAnimatedSpriteTextureTile(sel.Data);
    tile_id.Tile := StrToIntDef(edtTile.Text, tile_id.Tile);
    sel.Caption := IntToStr(tile_id.Tile);
  end;
end;

procedure TTileSetsForm.RegisterTileSet(pTileSet: anAnimatedSpriteTextureTileSet
  );
begin
  with TileSetsList.Items.Add do begin
    Caption := pTileSet.Name;
    Data := pTileSet;
    ImageIndex := 5;
  end;
end;

procedure TTileSetsForm.UnRegisterTile(pTile: anAnimatedSpriteTextureTile);
var
  sel: TListItem;
begin
  sel := TilesList.Items.FindData(pTile);
  if Assigned(sel) then sel.Free;
end;

procedure TTileSetsForm.UnRegisterTileSet(
  pTileSet: anAnimatedSpriteTextureTileSet);
var
  sel: TListItem;
begin
  sel := TileSetsList.Items.FindData(pTileSet);
  if Assigned(sel) then sel.Free;
end;

procedure TTileSetsForm.HandleTileSet(pTileSet: anAnimatedSpriteTextureTileSet);
var
  i: Integer;
begin
  TileSetPanel.Enabled := true;
  TileSetLabel.Caption := pTileSet.Name;
  TileSetLabel.Font.Color := $0;
  SelectedTile := pTileSet;

  TilesList.Clear;

  for i := 0 to SelectedTile.TileList.Count - 1 do begin
    RegisterTile(SelectedTile.TileList[i]);
  end;

  ResetTile;
end;

procedure TTileSetsForm.HandleTile(pTile: anAnimatedSpriteTextureTile);
begin
  TilesList.Selected := TilesList.Items.FindData(pTile);

  edtFrame.Text := FloatToStr(pTile.Time);
  edtFrame.Enabled := true;
  edtFrame.Font.Color := 0;

  edtTile.Text := IntToStr(pTile.Tile);
  edtTile.Enabled := true;
  edtTile.Font.Color := 0;

  edtTile.SetFocus;
end;

procedure TTileSetsForm.ResetTile;
begin
  edtFrame.Text := 'Select tile';
  edtFrame.Enabled := false;
  edtFrame.Font.Color := $666666;

  edtTile.Text := 'Select tile';
  edtTile.Enabled := false;
  edtTile.Font.Color := $666666;
end;

procedure TTileSetsForm.Reset;
begin
  btnRenameTileSet.Enabled := false;
  btnDeleteSet.Enabled := false;
  TileSetLabel.Caption := '[select tile set to edit]';
  TileSetLabel.Font.Color := $999999;
  TileSetPanel.Enabled := false;
  SelectedTile := nil;
  ResetTile;
end;

procedure TTileSetsForm.Edit(pTexture: anAnimatedSpriteTexture);
var
  i: Integer;
begin
  TileSetsList.Clear;
  Reset;
  Texture := pTexture;
  for i := 0 to Texture.TileSets.Count - 1 do
    RegisterTileSet(Texture.TileSets.Data[i]);
  ShowModal;
end;

end.

