unit u_editorhistory;

{$mode delphi}

interface

uses
  Classes, ComCtrls, SysUtils, fgl, zglHeader, u_animation;

type

  TEditorHistory = class;
  TEditorHistoryItem = class;

  TEditorHistoryItemList = TFPGList<TEditorHistoryItem>;

  { TEditorHistoryItem }

  TEditorHistoryItem = class ( TObject )
    private
      fUnded: boolean;
      fSubItems: TEditorHistoryItemList;

    private
      procedure AddSubItem(Item: TEditorHistoryItem);
    public
      property SubItems: TEditorHistoryItemList read fSubItems write fSubItems;
      property Unded: boolean read fUnded write fUnded;

      procedure UndoAction; virtual;
      procedure DoAction; virtual;

      procedure Done; virtual;

      constructor Create; virtual;
      destructor Destroy; override;
  end;

  { TEditorDeleteHistoryItem }

  TEditorDeleteHistoryItem = class (TEditorHistoryItem)
    private
      fItemName: String;
    public
      property ItemName: String read fItemName write fItemName;

      constructor Create(pItemName: String);
  end;

  { TEditorDeleteSymbolItem }

  TEditorDeleteSymbolItem = class (TEditorDeleteHistoryItem)
    private
      fSymbol: anSymbol;
    public
      property Symbol: anSymbol read fSymbol write fSymbol;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pSymbol: anSymbol);
      destructor Destroy; override;
  end;

  { TEditorDeleteTextureItem }

  TEditorDeleteTextureItem = class (TEditorDeleteHistoryItem)
    private
      fTexture: anTexture;
    public
      property Texture: anTexture read fTexture write fTexture;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pTexture: anTexture);
      destructor Destroy; override;
  end;

  { TEditorDeleteAnimationItem }

  TEditorDeleteAnimationItem = class (TEditorDeleteHistoryItem)
    private
      fAnimation: anAnimation;
    public
      property Animation: anAnimation read fAnimation write fAnimation;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pAnimation: anAnimation);
      destructor Destroy; override;
  end;

  { TEditorDeleteKeyFrameItem }

  TEditorDeleteKeyFrameItem = class (TEditorHistoryItem)
    private
      fKey, fPrev, fNext: anAnimationKeyFrameInstance;
    public
      property Key: anAnimationKeyFrameInstance read fKey write fKey;
      property Prev: anAnimationKeyFrameInstance read fPrev write fPrev;
      property Next: anAnimationKeyFrameInstance read fNext write fNext;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pKey: anAnimationKeyFrameInstance);
      destructor Destroy; override;
  end;

  { TEditorAddKeyFrameItem }

  TEditorAddKeyFrameItem = class (TEditorHistoryItem)
    private
      fKey, fPrev, fNext: anAnimationKeyFrameInstance;
    public
      property Key: anAnimationKeyFrameInstance read fKey write fKey;
      property Prev: anAnimationKeyFrameInstance read fPrev write fPrev;
      property Next: anAnimationKeyFrameInstance read fNext write fNext;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pKey: anAnimationKeyFrameInstance);
      destructor Destroy; override;
  end;

  { TEditorUpdateLOTransformItem }

  TEditorUpdateLOTransformItem = class (TEditorHistoryItem)
    private
      fKey: anAnimationKeyFrameInstance;

      OldTransform, NewTransform: anTransform;
    public
      property Key: anAnimationKeyFrameInstance read fKey write fKey;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pKey: anAnimationKeyFrameInstance; pNewTransform: anTransform);
      destructor Destroy; override;
  end;

  { TEditorUpdateLOEditTransformItem }

  TEditorUpdateLOEditTransformItem = class (TEditorHistoryItem)
    private
      fKey: anAnimationKeyFrameInstance;

      OldTransform, NewTransform: anTransform;
    public
      property Key: anAnimationKeyFrameInstance read fKey write fKey;

      procedure UndoAction; override;
      procedure DoAction; override;
      procedure Done; override;

      constructor Create(pKey: anAnimationKeyFrameInstance);
      destructor Destroy; override;
  end;

  { TEditorDeleteLayerObjectItem }

  TEditorDeleteLayerObjectItem = class (TEditorDeleteHistoryItem)
    private
      fLO: anAnimationLayerObject;
      fLoIndex: integer;
    public
      property LO: anAnimationLayerObject read fLO write fLO;
      property LoIndex: Integer read fLoIndex write fLoIndex;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pLO: anAnimationLayerObject);
      destructor Destroy; override;
  end;

  { TEditorRemoveParentItem }

  TEditorRemoveParentItem = class (TEditorHistoryItem)
    private
      fHolder, fParentHolder: anAnimationLOInstanceHolder;
      fLO, fParent: anAnimationLayerObject;
      fAnimation: anAnimation;
    public
      property Holder: anAnimationLOInstanceHolder read fHolder write fHolder;
      property ParentHolder: anAnimationLOInstanceHolder read fParentHolder write fParentHolder;
      property LO: anAnimationLayerObject read fLO write fLO;
      property Parent: anAnimationLayerObject read fParent write fParent;
      property Animation: anAnimation read fAnimation write fAnimation;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pLO: anAnimationLayerObject;
        pHolder, pParentHolder: anAnimationLOInstanceHolder; pAnimation: anAnimation);
      destructor Destroy; override;
  end;


  { TEditorAssignParentItem }

  TEditorAssignParentItem = class (TEditorHistoryItem)
    private
      fHolder, fParentHolder: anAnimationLOInstanceHolder;
      fLO, fParent: anAnimationLayerObject;
      fAnimation: anAnimation;
    public
      property Holder: anAnimationLOInstanceHolder read fHolder write fHolder;
      property ParentHolder: anAnimationLOInstanceHolder read fParentHolder write fParentHolder;
      property LO: anAnimationLayerObject read fLO write fLO;
      property Parent: anAnimationLayerObject read fParent write fParent;
      property Animation: anAnimation read fAnimation write fAnimation;

      procedure UndoAction; override;
      procedure DoAction; override;

      constructor Create(pLO, pParent: anAnimationLayerObject;
        pHolder, pParentHolder: anAnimationLOInstanceHolder; pAnimation: anAnimation);
      destructor Destroy; override;
  end;

  { TEditorHistory }

  TEditorHistory = class
    private
      fItems: TEditorHistoryItemList;
      fPosition: integer;
      fActiveItem: TEditorHistoryItem;
    public
      property Items: TEditorHistoryItemList read fItems write fItems;
      property Position: integer read fPosition write fPosition;
      property ActiveItem: TEditorHistoryItem read fActiveItem write fActiveItem;

      procedure AddItem(Item: TEditorHistoryItem; ImmediatelyDoAction: Boolean = true);

      procedure UndoAction;
      procedure DoAction;
      procedure Done;

      procedure MadeChanges;

      constructor Create;
      destructor Destroy; override;
  end;

implementation

uses u_main, u_render;

{ TEditorDeleteAnimationItem }

procedure TEditorDeleteAnimationItem.UndoAction;
begin
  MainForm.RegisterAnimation(Editor.Project.Animation.AddAnimation(Animation, ItemName), Animation.Prototype);
  inherited UndoAction;
end;

procedure TEditorDeleteAnimationItem.DoAction;
var
  Proto: anAnimationPrototype;
  i: Integer;
begin

  if Animation is anAnimationPrototype then begin
    Proto := Animation as anAnimationPrototype;
    for i := 0 to Proto.Blenders.Count - 1 do
      AddSubItem(TEditorDeleteAnimationItem.Create(Proto.Blenders[i]));
  end;

  inherited DoAction;

  MainForm.UnRegisterAnimation(Animation);
  Editor.Project.Animation.DeleteAnimation(Animation, false);
end;

constructor TEditorDeleteAnimationItem.Create(pAnimation: anAnimation);
begin
  Animation := pAnimation;
  inherited Create(Animation.Name);
end;

destructor TEditorDeleteAnimationItem.Destroy;
begin
  if not Unded then
    Animation.Free;
  inherited Destroy;
end;

{ TEditorDeleteTextureItem }

procedure TEditorDeleteTextureItem.UndoAction;
begin
  with MainForm do begin
    Editor.Project.Animation.AddTexture(ItemName, Texture);
    RegisterTexture(Texture, GetTextureImgId(Texture));
  end;
  inherited UndoAction;
end;

procedure TEditorDeleteTextureItem.DoAction;
var
  i: Integer;
  sym: anSymbol;
begin
  with MainForm do begin
    for i := Editor.Project.Animation.AnimationLibrary.Symbols.Count - 1 downto 0 do begin
      sym := Editor.Project.Animation.AnimationLibrary.Symbols.Data[i];
      if sym.Texture = Texture then begin
        AddSubItem(TEditorDeleteSymbolItem.Create(sym));
      end;
    end;

    inherited;

    UnRegisterTexture(Texture);
    Editor.Project.Animation.DeleteTexture(Texture, false);
  end;
end;

constructor TEditorDeleteTextureItem.Create(pTexture: anTexture);
begin
  Texture := pTexture;
  inherited Create(Texture.Name);
end;

destructor TEditorDeleteTextureItem.Destroy;
begin
  inherited Destroy;
end;

{ TEditorAddKeyFrameItem }

procedure TEditorAddKeyFrameItem.UndoAction;
begin
  if Assigned(Prev) then begin
    Prev.Next := Next;
    Prev.Frames := Prev.GetNextFrames + Key.GetNextFrames;
  end else begin
    Key.Holder.FirstInstance := Next;
  end;
  if Assigned(Next) then begin
    Next.Prev := Prev;
  end else begin
    Key.Holder.LastInstance := Prev;
  end;
  inherited UndoAction;
end;

procedure TEditorAddKeyFrameItem.DoAction;
begin
  if Assigned(Prev) then begin
    Prev.Next := Key;
    Prev.Frames := Prev.GetNextFrames - Key.GetNextFrames;
  end else begin
    Key.Holder.FirstInstance := Key;
  end;
  if Assigned(Next) then begin
    Next.Prev := Key;
  end else begin
    Key.Holder.LastInstance := Key;
  end;
  inherited DoAction;
end;

constructor TEditorAddKeyFrameItem.Create(pKey: anAnimationKeyFrameInstance);
begin
  inherited Create;
  Key := pKey;
  Prev := Key.Prev;
  Next := Key.Next;
end;

destructor TEditorAddKeyFrameItem.Destroy;
begin
  inherited Destroy;
end;

{ TEditorUpdateLOEditTransformItem }

procedure TEditorUpdateLOEditTransformItem.UndoAction;
begin
  Key.Transformation.TransformData^ := OldTransform;
  inherited UndoAction;
end;

procedure TEditorUpdateLOEditTransformItem.DoAction;
begin
  Key.Transformation.TransformData^ := NewTransform;
  inherited DoAction;
end;

procedure TEditorUpdateLOEditTransformItem.Done;
begin
  NewTransform := Key.Transformation.TransformData^;
end;

constructor TEditorUpdateLOEditTransformItem.Create(
  pKey: anAnimationKeyFrameInstance);
begin
  inherited Create;
  Key := pKey;
  OldTransform := Key.Transformation.TransformData^;
end;

destructor TEditorUpdateLOEditTransformItem.Destroy;
begin
  inherited Destroy;
end;

{ TEditorDeleteKeyFrameItem }

procedure TEditorDeleteKeyFrameItem.UndoAction;
begin
  if Assigned(Prev) then begin
    Prev.Next := Key;
    Prev.Frames := Prev.GetNextFrames - Key.GetNextFrames;
  end else begin
    Key.Holder.FirstInstance := Key;
  end;
  if Assigned(Next) then begin
    Next.Prev := Key;
  end else begin
    Key.Holder.LastInstance := Key;
  end;
  inherited UndoAction;
end;

procedure TEditorDeleteKeyFrameItem.DoAction;
begin
  if Assigned(Prev) then begin
    Prev.Next := Next;
    Prev.Frames := Prev.GetNextFrames + Key.GetNextFrames;
  end else begin
    Key.Holder.FirstInstance := Next;
  end;
  if Assigned(Next) then begin
    Next.Prev := Prev;
  end else begin
    Key.Holder.LastInstance := Prev;
  end;

  inherited DoAction;
end;

constructor TEditorDeleteKeyFrameItem.Create(pKey: anAnimationKeyFrameInstance);
begin
  inherited Create;
  Key := pKey;
  Prev := Key.Prev;
  Next := Key.Next;
end;

destructor TEditorDeleteKeyFrameItem.Destroy;
begin
  if not Unded then
    Key.Free;
  inherited Destroy;
end;

{ TEditorUpdateLOTransformItem }

procedure TEditorUpdateLOTransformItem.UndoAction;
begin
  Key.Transformation.TransformData^ := OldTransform;
  inherited UndoAction;
end;

procedure TEditorUpdateLOTransformItem.DoAction;
begin
  Key.Transformation.TransformData^ := NewTransform;
  inherited DoAction;
end;

constructor TEditorUpdateLOTransformItem.Create(
  pKey: anAnimationKeyFrameInstance; pNewTransform: anTransform);
begin
  inherited Create;
  Key := pKey;
  OldTransform := Key.Transformation.TransformData^;
  NewTransform := pNewTransform;
end;

destructor TEditorUpdateLOTransformItem.Destroy;
begin
  inherited Destroy;
end;

{ TEditorAssignParentItem }

procedure TEditorAssignParentItem.UndoAction;
begin
  LO.RemoveParent(Holder, ParentHolder);
  inherited UndoAction;
end;

procedure TEditorAssignParentItem.DoAction;
begin
  if Assigned(LO.Parent) then begin
    AddSubItem(TEditorRemoveParentItem.Create(LO, Holder, Holder.ParentHolder, Animation));
  end;
  inherited DoAction;
  LO.AssignParent(Parent, Holder, ParentHolder);
end;

constructor TEditorAssignParentItem.Create(pLO,
  pParent: anAnimationLayerObject; pHolder,
  pParentHolder: anAnimationLOInstanceHolder; pAnimation: anAnimation);
begin
  inherited Create;
  LO := pLO;
  ParentHolder := pParentHolder;
  Holder := pHolder;
  Parent := pParent;
  Animation := pAnimation;
end;

destructor TEditorAssignParentItem.Destroy;
begin
  inherited Destroy;
end;

{ TEditorRemoveParentItem }

procedure TEditorRemoveParentItem.UndoAction;
begin
  LO.AssignParent(Parent, Holder, ParentHolder);
  inherited;
end;

procedure TEditorRemoveParentItem.DoAction;
begin
  Parent := LO.Parent;
  LO.RemoveParent(Holder, ParentHolder);
  inherited;
end;

constructor TEditorRemoveParentItem.Create(pLO: anAnimationLayerObject;
  pHolder, pParentHolder: anAnimationLOInstanceHolder; pAnimation: anAnimation);
begin
  inherited Create;
  LO := pLO;
  ParentHolder := pParentHolder;
  Holder := pHolder;
  Animation := pAnimation;
end;

destructor TEditorRemoveParentItem.Destroy;
begin
  inherited Destroy;
end;

{ TEditorDeleteLayerObjectItem }

procedure TEditorDeleteLayerObjectItem.UndoAction;
begin
  LO.Animation.AddObject(LO, ItemName);
  LO.Animation.MoveObject(LO, LoIndex);
  inherited;
end;

procedure TEditorDeleteLayerObjectItem.DoAction;
var
  i: Integer;
begin
  for i := LO.Animation.ObjectsCount - 1 downto 0 do begin
    if LO.Animation.ObjectByIndex[i].Parent = LO then begin
      AddSubItem(
        TEditorRemoveParentItem.Create(
          LO.Animation.ObjectByIndex[i],
          LO.Animation.HolderByIndex[i],
          LO.Animation.HolderByIndex[i].ParentHolder,
          LO.Animation
        )
      );
    end;
  end;

  inherited;

  LO.Animation.RemoveObject(LO, false);
end;

constructor TEditorDeleteLayerObjectItem.Create(pLO: anAnimationLayerObject);
begin
  inherited Create(pLO.Name);
  LO := pLo;
  LoIndex := LO.Index;
end;

destructor TEditorDeleteLayerObjectItem.Destroy;
begin
  if not Unded then
    LO.Free;
  inherited Destroy;
end;

{ TEditorDeleteHistoryItem }

constructor TEditorDeleteHistoryItem.Create(pItemName: String);
begin
  inherited Create;
  ItemName := pItemName;
end;

{ TEditorDeleteSymbolItem }

procedure TEditorDeleteSymbolItem.UndoAction;
begin
  with MainForm do begin
    Editor.Project.Animation.AnimationLibrary.AddSymbol(Symbol, ItemName);
    RegisterSymbol(Symbol);
  end;
  inherited;
end;

procedure TEditorDeleteSymbolItem.DoAction;
var
  i: Integer;
  anim: anAnimation;
  j: Integer;
  obj: anAnimationSymbolObject;
begin
  with MainForm do begin
    for i := 0 to Editor.Project.Animation.Animations.Count - 1 do begin
      anim := Editor.Project.Animation.Animations.Data[i];
      for j := anim.ObjectsCount - 1 downto 0 do begin
        if anim.ObjectByIndex[j] is anAnimationSymbolObject then begin
          obj := anim.ObjectByIndex[j] as anAnimationSymbolObject;
          if obj.Symbol = Symbol then begin
            AddSubItem(TEditorDeleteLayerObjectItem.Create(anim.ObjectByIndex[j]));
            RemoveDrawList(anim, SLOT_ANIMATION);
          end;
        end;
      end;
    end;

    inherited;

    UnregisterSymbol(Symbol);
    Editor.Project.Animation.AnimationLibrary.DeleteSymbol(Symbol, false);

  end;
end;

constructor TEditorDeleteSymbolItem.Create(pSymbol: anSymbol);
begin
  inherited Create(pSymbol.Name);
  Symbol := pSymbol;
end;

destructor TEditorDeleteSymbolItem.Destroy;
begin
  if not Unded then
    Symbol.Free;
  inherited Destroy;
end;

{ TEditorHistoryItem }

procedure TEditorHistoryItem.AddSubItem(Item: TEditorHistoryItem);
begin
  SubItems.Add(Item);
end;

procedure TEditorHistoryItem.UndoAction;
var
  i: Integer;
begin
  Unded := true;
  for i := SubItems.Count - 1 downto 0 do begin
    SubItems[i].UndoAction;
    SubItems[i].Free;
  end;

  SubItems.Clear;
end;

procedure TEditorHistoryItem.DoAction;
var
  i: Integer;
begin
  Unded := false;

  for i := 0 to SubItems.Count - 1 do begin
    SubItems[i].DoAction;
  end;
end;

procedure TEditorHistoryItem.Done;
begin
  // nothing to do by default
end;

constructor TEditorHistoryItem.Create;
begin
  Unded := false;

  SubItems := TEditorHistoryItemList.Create;
end;

destructor TEditorHistoryItem.Destroy;
var
  i: Integer;
begin
  for i := 0 to SubItems.Count - 1 do
    SubItems[i].Free;

  SubItems.Clear;
  SubItems.Free;
  inherited;
end;

{ TEditorHistory }

procedure TEditorHistory.AddItem(Item: TEditorHistoryItem;
  ImmediatelyDoAction: Boolean);
begin
  MadeChanges;

  if ImmediatelyDoAction then
    Item.DoAction;

  Items.Add(Item);
  Position := Position + 1;

  ActiveItem := item;
end;

procedure TEditorHistory.Done;
begin
  if Assigned(ActiveItem) then begin
    ActiveItem.Done;
    ActiveItem := nil;
  end;
end;

procedure TEditorHistory.UndoAction;
var
  i: Integer;
begin
  if Position >= 0 then begin
    Items[Position].UndoAction;
    Position := Position - 1;
  end;
  MainForm.UpdateAnimation;
end;

procedure TEditorHistory.DoAction;
begin
  if Position < Items.Count - 1 then begin
    Position := Position + 1;
    Items[Position].DoAction;
  end;
  MainForm.UpdateAnimation;
end;

procedure TEditorHistory.MadeChanges;
var
  i: Integer;
begin
  MainForm.MadeChanges;
  if Position <= Items.Count - 1 then begin
    for i := Items.Count - 1 downto Position + 1 do begin
      Items[i].Free;
      Items.Delete(i);
    end;
  end;
end;

constructor TEditorHistory.Create;
begin
  Items := TEditorHistoryItemList.Create;
  Position := -1;
  ActiveItem := nil;
end;

destructor TEditorHistory.Destroy;
var i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].Free;
  Items.Free;
  inherited Destroy;
end;

end.

