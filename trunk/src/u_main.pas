unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, VirtualTrees, Forms, Controls,
  zglHeader, Graphics, Dialogs, Arrow, Menus, ExtCtrls, StdCtrls, ComCtrls,
  Buttons, ButtonPanel, u_animation, PropEdits, u_render, types, LCLType,
  LResources, u_prop_form, u_db;

const
  ITEMS_PER_SLOT = 3;

  SLOT_COUNT = 4;

  SLOT_ANIMATION = 0;
  SLOT_TEXTURE = 1;
  SLOT_SYMBOL = 2;
  SLOT_CONTENT = 3;

  SLOT_ANIMATION_ID = 5;
  SLOT_ANIMATION_BLENDER_ID = 45;
  SLOT_SYMBOL_ID = 20;
  SLOT_CONTENT_ID = 43;
  SLOT_PREVIEW_ID = 46;

  SYMBOL_ID = 20;

  AppName = 'Animation editor';
  AppVersion = '1.0b';

  COLUMN_NAME = 0;
  COLUMN_TIMELINE = 1;

  IMG_VISIBLE_S         = 0;
  IMG_VISIBLE           = 1;
  IMG_KEYFRAME          = 2;
  IMG_DISABLED_S        = 3;
  IMG_DISABLED          = 4;
  IMG_KEYFRAME_FLAG     = 5;
  IMG_FILTER_OFF        = 6;
  IMG_FILTER_ON         = 7;
  IMG_MODE_NORMAL       = 8;
  IMG_MODE_TAGS         = 9;
  IMG_TREE_EXPANDED     = 10;
  IMG_TREE_NOT_EXPANDED = 11;
  IMG_CALLBACK_FLAG     = 12;
  IMG_ACTION_FLAG       = 13;


  IMG_SMALL_SIZE = 12;
  IMG_HEADER_OFFSET = 4;

type

  TPanelMode = (pmNone, pmControls, pmAtlas, pmPreview);
  TNodeAction = (actSetVisible, actSetEnabled, actCheckVisible, actCheckEnabled, actFindWithData, actShowFilter);

  { TMainForm }

  TMainForm = class(TForm)
    KFPEditFrame: TMenuItem;
    PreviewButtons: TToolBar;
    {%region vars}
    AnimationPopup: TPopupMenu;

    AnimationsSelector: TTreeView;
    CloneAnimationBtn: TToolButton;
    DataControl: TPageControl;
    DeleteAnimationBtn: TToolButton;
    DeleteTextureContent: TToolButton;
    EditAnimationBtn: TToolButton;
    BtnEditTextureContent: TToolButton;
    InterpolationPopup: TPopupMenu;
    lblTextures: TLabel;
    lblTextures1: TLabel;
    lblTextures2: TLabel;
    lblTextures3: TLabel;
    lblTextures4: TLabel;
    lblTextures5: TLabel;
    LibraryItems: TListView;
    DeleteInstance: TMenuItem;
    InterpolationImages: TImageList;
    ipNone: TMenuItem;
    ipLinear: TMenuItem;
    ipCos: TMenuItem;
    ipCosBegin: TMenuItem;
    ipCosEnd: TMenuItem;
    MenuItem1: TMenuItem;
    EditInstanceProperties: TMenuItem;
    KFPAddFrames: TMenuItem;
    KFPRemoveFrames: TMenuItem;
    CompileProject: TMenuItem;
    MenuItem10: TMenuItem;
    EditAnimationItem: TMenuItem;
    CreatePrototypeIcon: TMenuItem;
    EditObjectTags: TMenuItem;
    PreviewBlenders: TMenuItem;
    NewAnimationBtn: TToolButton;
    BtnAddTextureContent: TToolButton;
    NewContentItemButtonAndBrowse: TToolButton;
    OtherComponents: TListView;
    OtherImages: TImageList;
    OtherObjectDescription: TMemo;
    ProjectOptions: TMenuItem;
    MenuItem3: TMenuItem;
    KFPDeleteKeyFrames: TMenuItem;
    MenuItem5: TMenuItem;
    OpenProjectAnimation: TMenuItem;
    MoveUpInstance: TMenuItem;
    MoveDownInstance: TMenuItem;
    KeyFramesPopup: TPopupMenu;
    TabLibrary: TTabSheet;
    TabAnimations: TTabSheet;
    TabOthers: TTabSheet;
    TabContent: TTabSheet;
    TabTextures: TTabSheet;
    TabTools: TToolBar;
    TextureContent: TListView;
    RuleTool: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    DrawPanel: TScrollBox;
    AtlasButtons: TToolBar;
    AtlasAddRectZone: TToolButton;
    AtlasAddShapeZone: TToolButton;
    AtlasMoveZone: TToolButton;
    AtlasRotateZone: TToolButton;
    AtlasScaleZone: TToolButton;
    AtlasEditPointZone: TToolButton;
    AtlasDeleteZone: TToolButton;
    AtlasDrawZoneNames: TToolButton;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuAbout: TMenuItem;
    AboutEditor: TMenuItem;
    MenuItem8: TMenuItem;
    GridSizeTool: TMenuItem;
    MenuItem9: TMenuItem;
    SnapToGridTool: TMenuItem;
    RemoveFromKeyFrame: TMenuItem;
    RecentItems: TMenuItem;
    SymbolCreation: TPopupMenu;
    AtlasAskZoneNames: TToolButton;
    GridTool: TToolButton;
    NewInterpolationBtn: TToolButton;
    EditCurrentAnimation: TToolButton;
    PublishProject: TToolButton;
    ToolBar2: TToolBar;
    ToolButton3: TToolButton;
    ModeFlipX: TToolButton;
    ModeFlipY: TToolButton;
    EditCurrentAnimationBtn: TToolButton;
    ToolButton4: TToolButton;
    btnTabTextures: TToolButton;
    btnTabSymbols: TToolButton;
    btnTabAnimations: TToolButton;
    btnTabOther: TToolButton;
    btnTabContent: TToolButton;
    ToolButton5: TToolButton;
    DrawHightLightTool: TToolButton;
    TimeLineBox: TVirtualDrawTree;
    AnimationPreviewPlay: TToolButton;
    AnimationPreviewPause: TToolButton;
    AnimationPreviewStop: TToolButton;
    Zoom10: TMenuItem;
    Zoom400: TMenuItem;
    Zoom25: TMenuItem;
    Zoom50: TMenuItem;
    Zoom200: TMenuItem;
    Zoom150: TMenuItem;
    Zoom100: TMenuItem;
    ZoomMenu: TPopupMenu;
    ShowIntanceInLibrary: TMenuItem;
    mmParent: TMenuItem;
    mmScale: TMenuItem;
    mmRotate: TMenuItem;
    mmMove: TMenuItem;
    MoveToBackInstance: TMenuItem;
    MoveToFrontInstance: TMenuItem;
    ObjectMenu: TPopupMenu;
    PreDraw: TPanel;
    MiniImages: TImageList;
    CloneTextureItemButton: TToolButton;
    DeleteTextureItemButton: TToolButton;
    GuiImages: TImageList;
    AnimatedSpriteTexture: TMenuItem;
    AtlasTexture: TMenuItem;
    Edit: TMenuItem;
    AddAnimationPopup: TPopupMenu;
    DeleteAnimation: TMenuItem;
    EditAnimation: TMenuItem;
    CloneAnimation: TMenuItem;
    AnimationData: TPanel;
    Redo: TMenuItem;
    AnimationUpdate: TTimer;
    ToolBar1: TToolBar;
    AnimationActTools: TToolBar;
    AnimationPlay: TToolButton;
    AnimationStop: TToolButton;
    EditSymbol: TToolButton;
    EditTexture: TToolButton;
    AnimationSettings: TToolButton;
    HistoryButtons: TToolBar;
    EditHistoryObject: TToolButton;
    DrawBonesTool: TToolButton;
    NewTextureItemButtonAndBrowse: TToolButton;
    NewLibraryItemButtonAndAssignTexture: TToolButton;
    ZoomTools: TToolBar;
    ZoomResetTool: TToolButton;
    ZoomOutTool: TToolButton;
    ZoomInTool: TToolButton;
    RegisterParent: TToolButton;
    UnregisterParent: TToolButton;
    Undo: TMenuItem;
    StaticTexture: TMenuItem;
    TexturesItems: TListView;
    SpriteTexture: TMenuItem;
    LeftPanel: TPanel;
    ContentPanel: TPanel;
    TextureCreation: TPopupMenu;
    PropertiesPanel: TPanel;
    FunctionalPanel: TPanel;
    RightPanel: TPanel;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TimeLinePanel: TPanel;
    TexturesToolbar: TToolBar;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    ExitItem: TMenuItem;
    NewTextureItemButton: TToolButton;
    SaveAsItem: TMenuItem;
    SaveItem: TMenuItem;
    OpenItem: TMenuItem;
    LibraryToolBar: TToolBar;
    NewLibraryItemButton: TToolButton;
    TimeLineCaption: TLabel;
    AnimationButtons: TToolBar;
    ToolButton1: TToolButton;
    DeleteLibraryItemButton: TToolButton;
    ToolButton2: TToolButton;
    CloneLibraryItemButton: TToolButton;
    ModeMove: TToolButton;
    ModeRotate: TToolButton;
    ModeScale: TToolButton;
    {%endregion}
    {%region callbacks}
    procedure AboutEditorClick(Sender: TObject);
    procedure AnimationPlayClick(Sender: TObject);
    procedure AnimationPopupPopup(Sender: TObject);
    procedure AnimationPreviewPauseClick(Sender: TObject);
    procedure AnimationPreviewPlayClick(Sender: TObject);
    procedure AnimationPreviewStopClick(Sender: TObject);
    procedure AnimationSettingsClick(Sender: TObject);
    procedure AnimationsSelectorClick(Sender: TObject);
    procedure AnimationsSelectorDblClick(Sender: TObject);
    procedure AnimationStopClick(Sender: TObject);
    procedure AnimationUpdateTimer(Sender: TObject);
    procedure AtlasAddRectZoneClick(Sender: TObject);
    procedure AtlasAddShapeZoneClick(Sender: TObject);
    procedure AtlasAskZoneNamesClick(Sender: TObject);
    procedure AtlasDeleteZoneClick(Sender: TObject);
    procedure AtlasEditPointZoneClick(Sender: TObject);
    procedure AtlasMoveZoneClick(Sender: TObject);
    procedure AtlasRotateZoneClick(Sender: TObject);
    procedure AtlasScaleZoneClick(Sender: TObject);
    procedure BtnAddTextureContentClick(Sender: TObject);
    procedure BtnEditTextureContentClick(Sender: TObject);
    procedure btnTabAnimationsClick(Sender: TObject);
    procedure CloneAnimationClick(Sender: TObject);
    procedure CompileProjectClick(Sender: TObject);
    procedure CreatePrototypeIconClick(Sender: TObject);
    procedure DeleteInstanceClick(Sender: TObject);
    procedure DeleteTextureContentClick(Sender: TObject);
    procedure DrawHightLightToolClick(Sender: TObject);
    procedure DrawPanelClick(Sender: TObject);
    procedure DrawPanelMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure DrawPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure EditAnimationItemClick(Sender: TObject);
    procedure EditCurrentAnimationClick(Sender: TObject);
    procedure EditHistoryObjectClick(Sender: TObject);
    procedure EditInstancePropertiesClick(Sender: TObject);
    procedure EditObjectTagsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure GridSizeToolClick(Sender: TObject);
    procedure GridToolClick(Sender: TObject);
    procedure KeyFramesPopupPopup(Sender: TObject);
    procedure KFPAddFramesClick(Sender: TObject);
    procedure KFPDeleteKeyFramesClick(Sender: TObject);
    procedure KFPRemoveFramesClick(Sender: TObject);
    procedure KFPEditFrameClick(Sender: TObject);
    procedure ModeFlipXClick(Sender: TObject);
    procedure ModeFlipYClick(Sender: TObject);
    procedure MoveDownInstanceClick(Sender: TObject);
    procedure MoveUpInstanceClick(Sender: TObject);
    procedure NewContentItemButtonAndBrowseClick(Sender: TObject);
    procedure OpenProjectAnimationClick(Sender: TObject);
    procedure OtherComponentsClick(Sender: TObject);
    procedure PreviewBlendersClick(Sender: TObject);
    procedure ProjectOptionsClick(Sender: TObject);
    procedure PublishProjectClick(Sender: TObject);
    procedure RemoveFromKeyFrameClick(Sender: TObject);
    procedure SetIMClick(Sender: TObject);
    procedure mmMoveClick(Sender: TObject);
    procedure mmRotateClick(Sender: TObject);
    procedure mmScaleClick(Sender: TObject);
    procedure mmParentClick(Sender: TObject);
    procedure NewInterpolationBtnClick(Sender: TObject);
    procedure NewLibraryItemButtonAndAssignTextureClick(Sender: TObject);
    procedure NewTextureItemButtonAndBrowseClick(Sender: TObject);
    procedure ObjectMenuPopup(Sender: TObject);
    procedure OpenItemClick(Sender: TObject);
    procedure RecentItemsClick(Sender: TObject);
    procedure RegisterParentClick(Sender: TObject);
    procedure DeleteAnimationClick(Sender: TObject);
    procedure DrawPanelDblClick(Sender: TObject);
    procedure DrawPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawPanelDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure EditSymbolClick(Sender: TObject);
    procedure EditTextureClick(Sender: TObject);
    procedure HistoryButtonClick(Sender: TObject);
    procedure LibraryItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MoveToBackInstanceClick(Sender: TObject);
    procedure MoveToFrontInstanceClick(Sender: TObject);
    procedure RuleToolClick(Sender: TObject);
    procedure SaveAsItemClick(Sender: TObject);
    procedure SaveItemClick(Sender: TObject);
    procedure SelectAnimation(Anim: anAnimation);
    procedure AddAnimationButtonClick(Sender: TObject);
    procedure AnimatedSpriteTextureClick(Sender: TObject);
    procedure AtlasTextureClick(Sender: TObject);
    procedure CloneLibraryItemButtonClick(Sender: TObject);
    procedure CloneTextureItemButtonClick(Sender: TObject);
    procedure DeleteLibraryItemButtonClick(Sender: TObject);
    procedure DeleteTextureItemButtonClick(Sender: TObject);
    procedure DrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPanelResize(Sender: TObject);
    procedure AnimationAddClick(Sender: TObject);
    procedure EditAnimationClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LibraryItemsClick(Sender: TObject);
    procedure MainPanelClick(Sender: TObject);
    procedure MainPanelDragOver
      (Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean
      );
    procedure ModeScaleClick(Sender: TObject);
    procedure ModeMoveClick(Sender: TObject);
    procedure ModeRotateClick(Sender: TObject);
    procedure NewLibraryItemButtonClick(Sender: TObject);
    procedure NewTextureItemButtonClick(Sender: TObject);
    procedure ObjectPropertiesModified(Sender: TObject);
    procedure RedoClick(Sender: TObject);
    procedure ShowIntanceInLibraryClick(Sender: TObject);
    procedure SnapToGridToolClick(Sender: TObject);
    procedure SpriteTextureClick(Sender: TObject);
    procedure StaticTextureClick(Sender: TObject);
    procedure SymbolCreationPopup(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure TextureContentClick(Sender: TObject);
    procedure TextureContentDblClick(Sender: TObject);
    procedure TexturesItemsClick(Sender: TObject);
    procedure TimeLineBoxChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TimeLineBoxDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure TimeLineBoxGetNodeWidth(Sender: TBaseVirtualTree;
      HintCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      var NodeWidth: Integer);
    procedure TimeLineBoxHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure TimeLineBoxHeaderDraw(Sender: TVTHeader; HeaderCanvas: TCanvas;
      Column: TVirtualTreeColumn; const R: TRect; Hover, Pressed: Boolean;
      DropMark: TVTDropMarkMode);
    procedure TimeLineBoxHeaderMouseDown(Sender: TVTHeader;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimeLineBoxHeaderMouseMove(Sender: TVTHeader; Shift: TShiftState;
      X, Y: Integer);
    procedure TimeLineBoxHeaderMouseUp(Sender: TVTHeader; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimeLineBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimeLineBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimeLineBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimeLineBoxResize(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure UnregisterParentClick(Sender: TObject);
    procedure Zoom25Click(Sender: TObject);
    procedure ZoomInToolClick(Sender: TObject);
    procedure ZoomOutToolClick(Sender: TObject);
    procedure ZoomResetToolClick(Sender: TObject);
    {%endregion}
  private
    DrawSlots: array[0..SLOT_COUNT-1] of record
      ItemsCount: integer;
      Data: array[0..ITEMS_PER_SLOT-1] of record
        Button: TToolButton;
        TheObject: TObject;
        Name: AnsiString;
      end;
    end;

    function CheckHitY(X: Integer; Val, ValWidth: Integer): Boolean;
  public
    GridSize: Integer;
    AnimMouseDown: Boolean;
    MouseDownShift: TShiftState;

    procedure MakeOperation(Node: PVirtualNode; MakeAction: TNodeAction; Data: Pointer; Result: PPointer = nil);
    function GetActiveColumn(ClickX: Integer): Integer;

    function GetSavePromt(od: TOpenDialog; ForceFileName: AnsiString = ''): AnsiString;

    procedure SetInterpolationMode(Index: Integer);
    procedure MadeChanges;
    procedure SetPanel(Mode: TPanelMode);

    function SelectedTexture: anTexture;
    procedure RegisterTexture(tex: anTexture; ImgIndex: integer; Display: boolean);
    procedure UnRegisterTexture(tex: anTexture);

    function RegisterAnimation(anim, proto: anAnimation; Display: boolean): TTreeNode;
    procedure UnRegisterAnimation(anim: anAnimation);
    procedure UpdateAnimation;

    procedure RegisterContent(cnt: anTextureContent; Display: boolean);
    procedure UnRegisterContent(cnt: anTextureContent);

    procedure RegisterLayerObject(obj: anAnimationLayerObject);

    function SelectedSymbol: anSymbol;
    procedure RegisterSymbol(sym: anSymbol; Display: boolean);
    procedure UnRegisterSymbol(sym: anSymbol);

    procedure RegisterOtherObject(ObjName: AnsiString; ImageIdx: Integer;
      ObjClass: TOtherObjectRegistrationInfo);

    function GetTextureImgId(tex: anTexture): integer;
    function GetAnimationImgId(tex: anAnimation): integer;

    procedure SaveAnimationToFile(FileName: AnsiString);
    procedure LoadAnimationFromFile(FileName: AnsiString);
    procedure Clear;
    function CheckForSave(Sender: TObject; Force: Boolean = false): Boolean;

    procedure OnSymbolNameChange(Instance: anNamedObject);
    procedure OnTextureNameChange(Instance: anNamedObject);
    procedure OnAnimationNameChange(Instance: anNamedObject);
    procedure OnContentNameChange(Instance: anNamedObject);

    procedure InitTexture(pName: AnsiString; Texture: anTexture; ImgIdx: integer);
    function CreateTexture(ClassOf: anTextureClass; ImgIdx: integer): anTexture;

    procedure HandleObject(Obj: TObject);
    procedure RehandleObject;
    procedure AddRecent(FileName: AnsiString; Reg: Boolean);

    procedure FillDrawList(Obj: anNamedObject; SlotId, IconId: integer); overload;
    procedure FillDrawList(Obj: TObject; pName: AnsiString; SlotId, IconId: integer); overload;
    procedure RemoveDrawList(Obj: TObject; SlotId: integer);
    procedure UpdateDrawList;
    procedure ClearDrawList;

    procedure NewLibraryItemButtonAssignTextureClick(Sender: TObject);
  end;

  { TFPSPropertyEditor }

  TFPSPropertyEditor = class(TPropertyEditor)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TTextureSelectPropertyEditor }

  TTextureSelectPropertyEditor = class(TPersistentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  { TContentSelectPropertyEditor }

  TContentSelectPropertyEditor = class(TPersistentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  { TTileSetSelectPropertyEditor }

  TTileSetSelectPropertyEditor = class(TPersistentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  { TOtherTileSetSelectPropertyEditor }

  TOtherTileSetSelectPropertyEditor = class(TPersistentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  { TLookUpPropertyEditor }

  TLookUpPropertyEditor = class(TPersistentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  { TAtlasZoneSelectPropertyEditor }

  TAtlasZoneSelectPropertyEditor = class(TPersistentPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

var
  MainForm: TMainForm;

implementation

uses u_editorhistory, u_preview_form;

{ TOtherTileSetSelectPropertyEditor }

function TOtherTileSetSelectPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList];
end;

function TOtherTileSetSelectPropertyEditor.GetValue: AnsiString;
var obj: anAnimatedSpriteTextureTileSet;
begin
  obj := anAnimatedSpriteTextureTileSet(GetObjectValue);
  if Assigned(obj) then
    Result := obj.Name
  else
    Result := '';
end;

procedure TOtherTileSetSelectPropertyEditor.GetValues(Proc: TGetStrProc);
var i: integer;
  sym: anSymbol;
  tex: anAnimatedSpriteTexture;
  key: anAnimationKeyFrameInstance;
begin
  key := anAnimationKeyFrameInstance(Self.PropertyHook.LookupRoot);
  if Assigned(key.ActiveProperties) and (key.ActiveProperties is anOtherAnimationSpriteProperty) then begin
    sym := anAnimationSymbolObject(key.ObjectTo).Symbol;
    if Assigned(sym.Texture) and (sym.Texture is anAnimatedSpriteTexture) then begin
      tex := anAnimatedSpriteTexture(sym.Texture);
      for i := 0 to tex.TileSets.Count - 1 do
        Proc(tex.TileSets.Keys[i]);
    end;
  end;
end;

procedure TOtherTileSetSelectPropertyEditor.SetValue(const NewValue: ansistring);
var i: integer;
  idx: LongInt;
  key: anAnimationKeyFrameInstance;
  sym: anSymbol;
  tex: anAnimatedSpriteTexture;
begin
  key := anAnimationKeyFrameInstance(Self.PropertyHook.LookupRoot);
  if Assigned(key.ActiveProperties) and (key.ActiveProperties is anOtherAnimationSpriteProperty) then begin
    sym := anAnimationSymbolObject(key.ObjectTo).Symbol;
    if Assigned(sym.Texture) and (sym.Texture is anAnimatedSpriteTexture) then begin
      tex := anAnimatedSpriteTexture(sym.Texture);
      idx := tex.TileSets.IndexOf(NewValue);
      if idx >= 0 then begin
        SetPtrValue(tex.TileSets.Data[idx]);
      end else begin
        SetPtrValue(nil);
      end;
    end;
  end;
end;

{ TTileSetSelectPropertyEditor }

function TTileSetSelectPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList];
end;

function TTileSetSelectPropertyEditor.GetValue: AnsiString;
var obj: anAnimatedSpriteTextureTileSet;
begin
  obj := anAnimatedSpriteTextureTileSet(GetObjectValue);
  if Assigned(obj) then
    Result := obj.Name
  else
    Result := '';
end;

procedure TTileSetSelectPropertyEditor.GetValues(Proc: TGetStrProc);
var i: integer;
  sym: anSymbol;
  tex: anAnimatedSpriteTexture;
begin
  sym := anSymbol(Self.PropertyHook.LookupRoot);
  if Assigned(sym.Texture) and (sym.Texture is anAnimatedSpriteTexture) then begin
    tex := anAnimatedSpriteTexture(sym.Texture);
    for i := 0 to tex.TileSets.Count - 1 do
      Proc(tex.TileSets.Keys[i]);
  end;
end;

procedure TTileSetSelectPropertyEditor.SetValue(const NewValue: ansistring);
var i: integer;
  sym: anSymbol;
  tex: anAnimatedSpriteTexture;
  idx: LongInt;
begin
  sym := anSymbol(Self.PropertyHook.LookupRoot);
  if Assigned(sym.Texture) and (sym.Texture is anAnimatedSpriteTexture) then begin
    tex := anAnimatedSpriteTexture(sym.Texture);
    idx := tex.TileSets.IndexOf(NewValue);
    if idx >= 0 then begin
      SetPtrValue(tex.TileSets.Data[idx]);
    end else begin
      SetPtrValue(nil);
    end;
  end;
end;

{ TLookUpPropertyEditor }

function TLookUpPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList];
end;

function TLookUpPropertyEditor.GetValue: AnsiString;
var obj: anAnimationLayerObject;
begin
  obj := anAnimationLayerObject(GetObjectValue);
  if Assigned(obj) then
    Result := obj.Name
  else
    Result := '';
end;

procedure TLookUpPropertyEditor.GetValues(Proc: TGetStrProc);
var i: integer;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    for i := 0 to Editor.SelectedAnimation.ObjectsCount - 1 do
      Proc(Editor.SelectedAnimation.ObjectByIndex[i].Name);
  end;
end;

procedure TLookUpPropertyEditor.SetValue(const NewValue: ansistring);
var idx: integer;
    obj: TObject;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    idx := Editor.SelectedAnimation.IndexOfName[NewValue];
    if idx >= 0 then begin
      SetPtrValue(Editor.SelectedAnimation.ObjectByIndex[idx]);
    end else
      SetPtrValue(nil);
  end;
end;

{ TContentSelectPropertyEditor }

function TContentSelectPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList];
end;

function TContentSelectPropertyEditor.GetValue: AnsiString;
var obj: anTextureContent;
begin
  obj := anTextureContent(GetObjectValue);
  if Assigned(obj) then
    Result := obj.Name
  else
    Result := '';
end;

procedure TContentSelectPropertyEditor.GetValues(Proc: TGetStrProc);
var i: integer;
begin
  for i := 0 to Editor.Project.Animation.Contents.Count - 1 do
    Proc(Editor.Project.Animation.Contents.Keys[i]);
end;

procedure TContentSelectPropertyEditor.SetValue(const NewValue: ansistring);
var idx: integer;
    obj: TObject;
begin
  idx := Editor.Project.Animation.Contents.IndexOf(NewValue);
  if idx >= 0 then begin
    SetPtrValue(Editor.Project.Animation.Contents.Data[idx]);
  end else
    SetPtrValue(nil);
  MainForm.RehandleObject;
end;

{ TFPSPropertyEditor }

function TFPSPropertyEditor.GetValue: ansistring;
var obj: anAnimation;
begin
  Result := IntToStr(GetOrdValue);
  obj := anAnimation(PropertyHook.LookupRoot);
  if obj._FPS = 0 then
     Exit('Inherits from AnimationSet (' + IntToStr(obj.FPS) + ')');
end;

procedure TFPSPropertyEditor.SetValue(const NewValue: ansistring);
begin
  SetOrdValue(StrToIntDef(NewValue, 0));
end;

function TFPSPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [];
end;

{ TAtlasZoneSelectPropertyEditor }

function TAtlasZoneSelectPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paValueList];
end;

function TAtlasZoneSelectPropertyEditor.GetValue: AnsiString;
var obj: anAtlasTextureZone;
begin
  obj := anAtlasTextureZone(GetObjectValue);
  if Assigned(obj) then
    Result := obj.Name
  else
    Result := '';
end;

procedure TAtlasZoneSelectPropertyEditor.GetValues(Proc: TGetStrProc);
var
  obj: anSymbol;
  tex: anAtlasTexture;
  i: Integer;
begin
  obj := anSymbol(PropertiesForm.ObjectProperties.TIObject);
  if Assigned(obj.Texture) and (obj.Texture is anAtlasTexture) then begin
    tex := anAtlasTexture(obj.Texture);
    for i := 0 to tex.Zones.Count - 1 do
      Proc(tex.Zones.Data[i].Name);
  end;
end;

procedure TAtlasZoneSelectPropertyEditor.SetValue(const NewValue: ansistring);
var
  obj: anSymbol;
  i: Integer;
  prms: anAtlasTextureParams;
  tex: anAtlasTexture;
begin
  obj := anSymbol(PropertiesForm.ObjectProperties.TIObject);
  if Assigned(obj.Texture) and (obj.Texture is anAtlasTexture) and (obj.TextureParams is anAtlasTextureParams) then begin
    tex := anAtlasTexture(obj.Texture);
    prms := anAtlasTextureParams(obj.TextureParams);
    if tex.Zones.IndexOf(NewValue) >= 0 then
       prms.Zone := tex.Zones[NewValue];
  end;
end;

{ TTextureSelectPropertyEditor }

function TTextureSelectPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:= [paMultiSelect, paSubProperties, paValueList];
end;

function TTextureSelectPropertyEditor.GetValue: AnsiString;
var obj: anTexture;
begin
  obj := anTexture(GetObjectValue);
  if Assigned(obj) then
    Result := obj.Name
  else
    Result := '';
end;

procedure TTextureSelectPropertyEditor.GetValues(Proc: TGetStrProc);
var i: integer;
begin
  for i := 0 to Editor.Project.Animation.Textures.Count - 1 do
    Proc(Editor.Project.Animation.Textures.Keys[i]);
end;

procedure TTextureSelectPropertyEditor.SetValue(const NewValue: ansistring);
var idx: integer;
    obj: TObject;
begin
  idx := Editor.Project.Animation.Textures.IndexOf(NewValue);
  if idx >= 0 then begin
    SetPtrValue(Editor.Project.Animation.Textures.Data[idx]);
  end else
    SetPtrValue(nil);
  MainForm.RehandleObject;
end;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.HandleObject(Obj: TObject);
var
  i: Integer;
begin
  PropertiesForm.HandleObject(Obj);
  PropertiesForm.ObjectProperties.TIObject := TPersistent(Obj);
  PropertiesForm.Caption := 'Properties: ' + Obj.ClassName;
  PropertiesForm.ShowModal;
end;

procedure TMainForm.RehandleObject;
begin
  PropertiesForm.RehandleObject;
end;

procedure TMainForm.AddRecent(FileName: AnsiString; Reg: Boolean);
var
  mi: TMenuItem;
  i: Integer;
  ch: TDBRecord;
begin
  if Reg then begin
    for i := 0 to Editor.Recent.ChildCount - 1 do begin
      if (FileName = Editor.Recent.Child[i].AsString) then
        exit;
    end;
    if Editor.Recent.ChildCount > 10 then begin
      Editor.Recent.RemoveChildByIndex(0);
    end;
    ch := TDBRecord.Create(Editor.Settings, Editor.Recent, rtString);
    ch.AsString := FileName;
    Editor.Recent.AddChild(ch);
  end;

  if FileExists(FileName) then begin
    mi := TMenuItem.Create(RecentItems);
    mi.Caption := FileName;
    mi.OnClick := @RecentItemsClick;
    RecentItems.Add(mi);
    RecentItems.Enabled := true;
  end;
  Editor.Settings.Save;
end;

procedure TMainForm.NewTextureItemButtonClick(Sender: TObject);
begin
  TextureCreation.Tag := 0;
  TextureCreation.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;

procedure TMainForm.ObjectPropertiesModified(Sender: TObject);
begin
  Editor.History.MadeChanges;
  if Assigned(PropertiesForm.ObjectProperties.TIObject) then begin
    if PropertiesForm.ObjectProperties.TIObject.InheritsFrom(anAnimation) then begin
      Editor.SyncAnimation(anAnimation(PropertiesForm.ObjectProperties.TIObject));
    end;
    if PropertiesForm.ObjectProperties.TIObject.InheritsFrom(anAnimationKeyFrameInstance) then begin
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.OnSymbolNameChange(Instance: anNamedObject);
var idx: TListItem;
begin
  idx := LibraryItems.Items.FindData(Instance);
  if Assigned(idx) then begin
    idx.Caption := Instance.Name;
    UpdateDrawList;
  end;
end;

procedure TMainForm.OnTextureNameChange(Instance: anNamedObject);
var idx: TListItem;
begin
  idx := TexturesItems.Items.FindData(Instance);
  if Assigned(idx) then begin
    idx.Caption := Instance.Name;
    UpdateDrawList;
  end;
end;

procedure TMainForm.OnAnimationNameChange(Instance: anNamedObject);
var node: TTreeNode;
begin
  node := AnimationsSelector.Items.FindNodeWithData(Instance);
  if Assigned(node) then begin
    node.Text := Instance.Name;
    UpdateDrawList;
  end;
end;

procedure TMainForm.OnContentNameChange(Instance: anNamedObject);
var node: TListItem;
begin
  node := TextureContent.Items.FindData(Instance);
  if Assigned(node) then begin
    node.Caption := Instance.Name;
    UpdateDrawList;
  end;
end;

procedure TMainForm.InitTexture
  (pName: AnsiString; Texture: anTexture; ImgIdx: integer);
var nw: TListItem;
begin
  Editor.Project.Animation.AddTexture(Texture.GetUniqueName(pName), Texture);
  RegisterTexture(Texture, ImgIdx, true);
  MadeChanges;

  DeleteTextureItemButton.Enabled := true;
  CloneTextureItemButton.Enabled := true;
end;

function TMainForm.CreateTexture(ClassOf: anTextureClass; ImgIdx: integer
  ): anTexture;
var nw: TListItem;
    od: TOpenDialog;
    texName: AnsiString;
    cn: anTextureContent;
begin
  Result := Editor.Project.Animation.AddTexture('TEX_NEW', ClassOf);
  RegisterTexture(Result, ImgIdx, true);
  Editor.History.MadeChanges;

  if TextureCreation.Tag = 1 then begin
    od := TOpenDialog.Create(nil);
    if od.Execute then begin
      cn := Editor.Project.Animation.AddContent('CNT_NEW', anTextureContent);
      RegisterContent(cn, true);
      cn.LoadFromFile(od.FileName, MainForm.GetSavePromt(od));
      Result.FileContent := cn;
      texName := file_GetName(od.FileName);
      Result.Name := texName;
    end;
    od.Free;
  end;

  HandleObject(Result);

  DeleteTextureItemButton.Enabled := true;
  CloneTextureItemButton.Enabled := true;
end;

procedure TMainForm.RedoClick(Sender: TObject);
begin
  if mouse_state.state = msNone then
    Editor.History.DoAction;
end;

procedure TMainForm.ShowIntanceInLibraryClick(Sender: TObject);
var
  ins: anSymbol;
  anim_ins: anAnimation;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
      if Editor.ContextObject is anAnimationSymbolObject then begin;
        ins := anAnimationSymbolObject(Editor.ContextObject).Symbol;
        FillDrawList(ins, SLOT_SYMBOL, SLOT_SYMBOL_ID);
      end;
      if Editor.ContextObject is anAnimationAnimationObject then begin;
        anim_ins := anAnimationAnimationObject(Editor.ContextObject).DataAnimation;
        FillDrawList(anim_ins, SLOT_ANIMATION, SLOT_ANIMATION_ID);
      end;
    end;
  end;
end;

procedure TMainForm.SnapToGridToolClick(Sender: TObject);
begin
  SnapToGridTool.Checked := not SnapToGridTool.Checked;
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not zglInited Then
    begin
      zglInited := TRUE;
      {$IFNDEF USE_ZENGL_STATIC}
      zglLoad( 'ZenGL' + lowerCase({$I %FPCTARGETOS%})  + '.dll' );
      {$ENDIF}

      zgl_Reg( SYS_LOAD, @Init );
      zgl_Reg( SYS_DRAW, @Draw );
      zgl_Reg( SYS_UPDATE, @UpdateDT );

      zgl_Disable(APP_USE_LOG);

      wnd_ShowCursor( TRUE );

      zgl_InitToHandle( DrawPanel.Handle );

      AnimationUpdate.Enabled := false;

      Editor.Free;

      Application.Terminate();
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if zglInited Then
    begin
      zglInited := FALSE;
      zgl_Exit();
    end;
end;

procedure TMainForm.DeleteLibraryItemButtonClick(Sender: TObject);
var sel: anSymbol;
  idx: TListItem;
begin
  idx := LibraryItems.Selected;
  if Assigned(idx) then begin
    sel := anSymbol(idx.Data);
    if MessageDlg('Are you shure to delete symbol ' + sel.Name + ' ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

      Editor.History.AddItem(TEditorDeleteSymbolItem.Create(sel));

      DeleteLibraryItemButton.Enabled := false;
      CloneLibraryItemButton.Enabled := false;
    end;
  end;
end;

procedure TMainForm.DeleteTextureItemButtonClick(Sender: TObject);
var sel: anTexture;
    idx: TListItem;
    i: Integer;
begin
  idx := TexturesItems.Selected;
  if Assigned(idx) then begin
    sel := anTexture(idx.Data);
    if MessageDlg('Are you shure to delete texture ' + sel.Name + ' ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      Editor.History.AddItem(TEditorDeleteTextureItem.Create(sel));
      DeleteTextureItemButton.Enabled := false;
      CloneTextureItemButton.Enabled := false;
    end;
  end;
end;

procedure TMainForm.DrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  mouse_state.clickpos.X := (X - Editor.RenderRect.X - Editor.RenderRect.W / 2) / Editor.CAM.Zoom.X;
  mouse_state.clickpos.Y := (Y - Editor.RenderRect.Y - Editor.RenderRect.H / 2) / Editor.CAM.Zoom.Y;
  mouse_state.pos := mouse_state.clickpos;
  p.x := X; p.Y := Y;
  p := DrawPanel.ClientToScreen(p);
  mouse_state.rpos.X := p.X; mouse_state.rpos.Y := p.Y;
  mouse_state.button := Button;
  mouse_state.shift := shift;
  mouse_state.clicked := true;
  mouse_state.down := true;
end;

procedure TMainForm.DrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mouse_state.pos.X := (X - Editor.RenderRect.X - Editor.RenderRect.W / 2) / Editor.CAM.Zoom.X;
  mouse_state.pos.Y := (Y - Editor.RenderRect.Y - Editor.RenderRect.H / 2) / Editor.CAM.Zoom.Y;
end;

procedure TMainForm.DrawPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mouse_state.button := Button;
  mouse_state.clicked := false;
  mouse_state.down := false;
  mouse_state.dblclick := false;
end;

procedure TMainForm.DrawPanelResize(Sender: TObject);
begin
  if zglInited Then begin
    wnd_SetSize( DrawPanel.ClientWidth, DrawPanel.ClientHeight );
    with Editor.RenderRect do begin
      H := W * (DrawPanel.ClientHeight / DrawPanel.ClientWidth);
    end;
    Editor.UpdateRenderTarget;
  end;
end;

procedure TMainForm.AnimationAddClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.EditAnimationClick(Sender: TObject);
var
  node: TTreeNode;
begin
  node := AnimationsSelector.Selected;
  if Assigned(node) then begin
    HandleObject(anAnimation(node.Data));
  end;
  Editor.RepaintTimeline;
end;

procedure TMainForm.CloneLibraryItemButtonClick(Sender: TObject);
var symbol, sel: anSymbol;
  idx: TListItem;
begin
  idx := LibraryItems.Selected;
  if Assigned(idx) then begin
    sel := anSymbol(idx.Data);
    symbol := Editor.Project.Animation.AnimationLibrary.CloneSymbol(sel.Name);
    if symbol <> nil then begin
      RegisterSymbol(symbol, true);
      MadeChanges;
      HandleObject(symbol);
    end;
  end;
end;

procedure TMainForm.AnimatedSpriteTextureClick(Sender: TObject);
begin
  CreateTexture(anAnimatedSpriteTexture, 5);
end;

procedure TMainForm.SelectAnimation(Anim: anAnimation);
var i: integer;
  filter: String;
begin
  FillDrawList(Anim, SLOT_ANIMATION, GetAnimationImgId(Anim));
  with Editor do begin
    SelectedAnimation := Anim;
    Selected.Left := 0;
    Selected.Right := 0;
    Selected.ObjectTo := -1;
    SyncAnimation(Anim);

    filter := Editor.Project.GetPreset(Anim.Name).Filter;
    MakeOperation(TimeLineBox.RootNode, actShowFilter, @filter);

    RepaintTimeline;
  end;
end;

procedure TMainForm.DeleteAnimationClick(Sender: TObject);
var sel: anAnimation;
  node: TTreeNode;
begin
  node := AnimationsSelector.Selected;
  if Assigned(node) then begin
    sel := anAnimation(node.Data);
    if MessageDlg('Are you shure to delete animation ' + sel.Name + ' ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      Editor.History.AddItem(TEditorDeleteAnimationItem.Create(sel));
      AnimationsSelector.Selected := nil;
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.DrawPanelDblClick(Sender: TObject);
begin
  mouse_state.dblclick := true;
end;

procedure TMainForm.DrawPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
var obj: anAnimationLayerObject;
    sym: anSymbol;
    inst: anAnimationKeyFrameInstance;
    anim: anAnimation;
    classOf: anAnimationLayerObjectClass;
    prot: anAnimationPrototype;
begin
  if Editor.DragState.Drag then begin
    if Assigned(Editor.SelectedAnimation) then begin
      if Source = LibraryItems then begin
        sym := anSymbol(LibraryItems.Selected.Data);
        if Editor.SelectedAnimation is anAnimationPrototype then begin
          prot := anAnimationPrototype(Editor.SelectedAnimation);
          obj := anAnimationSymbolObject.Create(sym, prot);
          RegisterLayerObject(obj);
        end else begin
          ShowMessage('Cannot add objects to blender!');
        end;
        if Assigned(Editor.DragState.Content) then begin
          Editor.DragState.Symbol.TextureParams.ReleaseTexture(Editor.DragState.Content);
          Editor.DragState.Content := nil;
        end;
      end;
      if Source = AnimationsSelector then begin
        Editor.DragState.Animation.Free;
        Editor.DragState.Animation := nil;
        anim := anAnimation(AnimationsSelector.Selected.Data);
        if Editor.SelectedAnimation is anAnimationPrototype then begin
          prot := anAnimationPrototype(Editor.SelectedAnimation);
          obj := anAnimationAnimationObject.Create(anim, prot);
          RegisterLayerObject(obj);
        end else begin
          ShowMessage('Cannot add objects to blender!');
        end;
      end;
      if Source = OtherComponents then begin
        if Editor.SelectedAnimation is anAnimationPrototype then begin
          prot := anAnimationPrototype(Editor.SelectedAnimation);
          classOf := TOtherObjectRegistrationInfo(OtherComponents.Selected.Data).PropClass;
          obj := classOf.Create(prot);
          RegisterLayerObject(obj);
        end else begin
          ShowMessage('Cannot add objects to blender!');
        end;
      end;
    end;
    Editor.UpdateAnimationInstance;
  end;
  Editor.DragState.Drag := false;
end;

procedure TMainForm.DrawPanelDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var sym: anSymbol;
  anim: anAnimation;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Source = LibraryItems then begin
      sym := anSymbol(LibraryItems.Selected.Data);
      with Editor.DragState do begin
        Drag := true;
        DropClass := anDropSymbol;
        Symbol := sym;
        Pos.X := (X - Editor.RenderRect.X - Editor.RenderRect.W / 2) / Editor.CAM.Zoom.X;
        Pos.Y := (Y - Editor.RenderRect.Y - Editor.RenderRect.H / 2) / Editor.CAM.Zoom.Y;
      end;
      Accept := true;
      exit;
    end;
    if Source = AnimationsSelector then begin
      anim := anAnimation(AnimationsSelector.Selected.Data);
      with Editor.DragState do begin
        Drag := true;
        DropClass := anDropAnimation;
        if Animation = nil then
          Animation := anim.GetInstance;
        Pos.X := (X - Editor.RenderRect.X - Editor.RenderRect.W / 2) / Editor.CAM.Zoom.X;
        Pos.Y := (Y - Editor.RenderRect.Y - Editor.RenderRect.H / 2) / Editor.CAM.Zoom.Y;
      end;
      Accept := true;
      exit;
    end;
    if Source = OtherComponents then begin
      Accept := true;
      with Editor.DragState do begin
        Drag := true;
        DropClass := anDropObject;
        Pos.X := (X - Editor.RenderRect.X - Editor.RenderRect.W / 2) / Editor.CAM.Zoom.X;
        Pos.Y := (Y - Editor.RenderRect.Y - Editor.RenderRect.H / 2) / Editor.CAM.Zoom.Y;
      end;
      exit;
    end;
  end;
  Accept := false;
end;

procedure TMainForm.EditSymbolClick(Sender: TObject);
var
  idx: TListItem;
begin
  idx := LibraryItems.Selected;
  if Assigned(idx) then begin
    HandleObject(TObject(idx.Data));
  end;
end;

procedure TMainForm.EditTextureClick(Sender: TObject);
var
  idx: TListItem;
begin
  idx := TexturesItems.Selected;
  if Assigned(idx) then begin
    HandleObject(TObject(idx.Data));
  end;
end;

procedure TMainForm.HistoryButtonClick(Sender: TObject);
begin
  if Assigned(Editor.DrawData) then begin
    if Editor.DrawData is anAnimationInstance then begin
      RemoveDrawList(Editor.DrawData, SLOT_ANIMATION);
    end;
  end;
  Editor.DrawData := TObject(TToolButton(Sender).Tag);
  if Editor.DrawData is anAnimation then begin
    AnimationsSelector.Selected :=
      AnimationsSelector.Items.FindNodeWithData(Editor.DrawData);
    SelectAnimation(anAnimation(Editor.DrawData));
    SetPanel(pmControls);
  end else
  if Editor.DrawData is anAnimationInstance then begin
    SetPanel(pmPreview);
  end else
  if Editor.DrawData is anAtlasTexture then begin
    SetPanel(pmAtlas);
  end else
    SetPanel(pmNone);
end;

procedure TMainForm.LibraryItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (LibraryItems.ItemIndex >= 0) then
    LibraryItems.BeginDrag(True);
end;

procedure TMainForm.MoveToBackInstanceClick(Sender: TObject);
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
       anAnimationPrototype(Editor.SelectedAnimation).MoveObject(anAnimationLayerObject(Editor.ContextObject),
        Editor.SelectedAnimation.ObjectsCount - 1);
      UpdateAnimation;
    end;
  end;
end;

procedure TMainForm.MoveToFrontInstanceClick(Sender: TObject);
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
      anAnimationPrototype(Editor.SelectedAnimation).MoveObject(anAnimationLayerObject(Editor.ContextObject),
        0);

      UpdateAnimation;
    end;
  end;
end;

procedure TMainForm.RuleToolClick(Sender: TObject);
begin
  RuleTool.Checked := not RuleTool.Checked;
end;

procedure TMainForm.SaveAsItemClick(Sender: TObject);
var sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(nil);
  sd.Filter := 'Animation project (*.apr)|*.apr';
  sd.Options := [ofOverwritePrompt];
  if sd.Execute then begin
    if file_GetExtension(sd.FileName) = '' then
      SaveAnimationToFile(sd.FileName + '.apr')
    else
      SaveAnimationToFile(sd.FileName);
  end;
  sd.Free;

end;

procedure TMainForm.SaveItemClick(Sender: TObject);
begin
  if Editor.Project.Saved then
    SaveAnimationToFile(Editor.Project.ProjectFileName)
  else
    SaveAsItemClick(Sender);
end;

procedure TMainForm.CloneAnimationClick(Sender: TObject);
var
  an, sel: anAnimation;
  j: Integer;
  node: TTreeNode;
begin
  if Assigned(AnimationsSelector.Selected) then begin
    sel := anAnimation(AnimationsSelector.Selected.Data);
    an := Editor.Project.Animation.CloneAnimation(sel.Name, anAnimationClass(sel.ClassType));
    RegisterAnimation(an, sel.Prototype, true);
    SelectAnimation(an);
    HandleObject(an);
    Editor.RepaintTimeline;
  end;
end;

procedure TMainForm.CompileProjectClick(Sender: TObject);
begin
  Editor.Project.Generate;
end;

procedure TMainForm.CreatePrototypeIconClick(Sender: TObject);
var
  anim: anAnimationBlender;
  proto: anAnimationPrototype;
  i: Integer;
  inst: anAnimationKeyFrameInstance;
begin
  proto := anAnimationPrototype(AnimationsSelector.Selected.Data);
  anim := Editor.Project.Animation.CloneAnimation(proto.Name, anAnimationBlender) as anAnimationBlender;

  anim.Prototype := proto;
  anim.Update;

  for i := 0 to anim.ObjectsCount - 1 do begin
    inst := anim.ObjectByIndex[i].GetInstance(anim.HolderByIndex[i],
      anim.ObjectByIndex[i].GetTransform(proto.HolderByIndex[i], 0, false), 0);
    inst.InterpolationMode := DEFAULT_INTERPOLATION_MODE;
  end;
  anim.Name := proto.Name + '_blender';
  RegisterAnimation(anim, proto, true);
  HandleObject(anim);
end;

procedure TMainForm.DeleteInstanceClick(Sender: TObject);
var obj: anAnimationLayerObject;
  i: Integer;
  proto: anAnimationPrototype;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
      obj := anAnimationLayerObject(Editor.ContextObject);
      Editor.History.AddItem(TEditorDeleteLayerObjectItem.Create(obj));
      Editor.SelectedObject := nil;
      UpdateAnimation;
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.DeleteTextureContentClick(Sender: TObject);
var sel: anTextureContent;
  node: TListItem;
begin
  node := TextureContent.Selected;
  if Assigned(node) then begin
    sel := anTextureContent(node.Data);
    if MessageDlg('Are you shure to delete texture content ' + sel.Name + ' ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin

      //Editor.History.AddItem(TEditorDeleteContentItem.Create(sel)); { TODO : UNDO }

      UnRegisterContent(sel);
      Editor.Project.Animation.DeleteContent(sel, false);

      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.DrawHightLightToolClick(Sender: TObject);
begin
  Editor.UpdateRenderTarget;
end;

procedure TMainForm.DrawPanelClick(Sender: TObject);
begin

end;

procedure TMainForm.DrawPanelMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  mouse_state.pos.X := MousePos.X - Editor.RenderRect.X;
  mouse_state.pos.Y := MousePos.Y - Editor.RenderRect.Y;
  Editor.MultZoom(0.9);
end;

procedure TMainForm.DrawPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  mouse_state.pos.X := MousePos.X - Editor.RenderRect.X;
  mouse_state.pos.Y := MousePos.Y - Editor.RenderRect.Y;
  Editor.MultZoom(1.1);
end;

procedure TMainForm.EditAnimationItemClick(Sender: TObject);
var
  an: anAnimation;
  sel: TTreeNode;
begin
  sel := AnimationsSelector.Selected;
  if Assigned(sel) then begin
    an := anAnimation(sel.Data);
    SelectAnimation(an);
  end;
  EditAnimationBtn.Enabled := Assigned(Sel);
  CloneAnimationBtn.Enabled := Assigned(Sel);
  DeleteAnimationBtn.Enabled := Assigned(Sel);
end;

procedure TMainForm.EditCurrentAnimationClick(Sender: TObject);
begin
  HandleObject(Editor.SelectedAnimation);
  Editor.SyncAnimation(Editor.SelectedAnimation);
end;

procedure TMainForm.EditHistoryObjectClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to HistoryButtons.ButtonCount - 1 do begin
    if HistoryButtons.Buttons[i].Down then begin
      HandleObject(TObject(HistoryButtons.Buttons[i].Tag));
      break;
    end;
  end;
end;

procedure TMainForm.EditInstancePropertiesClick(Sender: TObject);
begin
  HandleObject(Editor.ContextObject);
end;

procedure TMainForm.EditObjectTagsClick(Sender: TObject);
var
  obj: anAnimationLayerObject;
  preset: TAnimationPreset;
  obj_set: TPresetObject;
begin
  with Editor do begin
    if Assigned(SelectedAnimation) then begin
      preset := Project.GetPreset(SelectedAnimation.Name);
      obj := Editor.ContextObject as anAnimationLayerObject;
      if Assigned(obj) then begin
        obj_set := preset.GetObject(obj.Name);
        obj_set.Tags := InputBox('Enter tags', 'Enter object tags separated by space', obj_set.Tags);
      end;
    end;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := not CheckForSave(Sender);
end;

procedure TMainForm.GridSizeToolClick(Sender: TObject);
begin
  GridSize := StrToIntDef(InputBox('Grid size', 'Enter grid size', IntToStr(GridSize)), GridSize);
  GridSizeTool.Caption := 'Grid size: ' + IntToStr(GridSize);
end;

procedure TMainForm.GridToolClick(Sender: TObject);
begin

end;

procedure TMainForm.KeyFramesPopupPopup(Sender: TObject);
var
  obj: anAnimationLOInstanceHolder;
  inst: anAnimationKeyFrameInstance;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Editor.SelectionMode = smDone then begin
      obj := Editor.SelectedAnimation.HolderByIndex[Editor.Selected.ObjectTo];
      inst := obj.InstanceAt[Editor.Selected.Left];
      KFPDeleteKeyFrames.Enabled := Assigned(inst.Prev);
      KFPAddFrames.Enabled := Assigned(inst.Next);
      KFPRemoveFrames.Enabled := KFPAddFrames.Enabled and (inst.Frames > 1);
      KFPEditFrame.Enabled := Editor.Selected.Left - Editor.Selected.Right = 0;
    end;
  end;
end;

procedure TMainForm.KFPAddFramesClick(Sender: TObject);
var
  obj: anAnimationLOInstanceHolder;
  inst: anAnimationKeyFrameInstance;
  i: anPosition;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Editor.SelectionMode = smDone then begin
      obj := Editor.SelectedAnimation.HolderByIndex[Editor.Selected.ObjectTo];
      inst := obj.InstanceAt[Editor.Selected.Left];
      inst.Frames := inst.Frames + (Editor.Selected.Right - Editor.Selected.Left + 1);
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.KFPDeleteKeyFramesClick(Sender: TObject);
var
  obj: anAnimationLOInstanceHolder;
  inst: anAnimationKeyFrameInstance;
  i: anPosition;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Editor.SelectionMode = smDone then begin
      obj := Editor.SelectedAnimation.HolderByIndex[Editor.Selected.ObjectTo];
      for i := Editor.Selected.Right downto Editor.Selected.Left do begin
        inst := obj.InstanceAt[i];
        if inst.Position = i then begin
          Editor.History.AddItem(TEditorDeleteKeyFrameItem.Create(inst));
        end;
      end;
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.KFPRemoveFramesClick(Sender: TObject);
var
  obj: anAnimationLOInstanceHolder;
  inst: anAnimationKeyFrameInstance;
  i: anPosition;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Editor.SelectionMode = smDone then begin
      obj := Editor.SelectedAnimation.HolderByIndex[Editor.Selected.ObjectTo];
      inst := obj.InstanceAt[Editor.Selected.Left];
      if inst.Frames > Editor.Selected.Right - Editor.Selected.Left + 1 then begin
        inst.Frames := inst.Frames - (Editor.Selected.Right - Editor.Selected.Left + 1);
      end else
        inst.Frames := 1;
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.KFPEditFrameClick(Sender: TObject);
var
  obj: anAnimationLOInstanceHolder;
  inst: anAnimationKeyFrameInstance;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Editor.SelectionMode = smDone then begin
      obj := Editor.SelectedAnimation.HolderByIndex[Editor.Selected.ObjectTo];
      inst := obj.InstanceAt[Editor.Selected.Left];
      HandleObject(inst);
    end;
  end;
end;

procedure TMainForm.ModeFlipXClick(Sender: TObject);
var
  trans: anTransform;
begin
  if Assigned(Editor.SelectedObject) then begin
    trans := Editor.SelectedObject.Transformation.TransformData^;
    trans.Flip := not trans.Flip;
    Editor.History.AddItem(TEditorUpdateLOTransformItem.Create(
      Editor.SelectedObject,
      trans
    ));
  end;
end;

procedure TMainForm.ModeFlipYClick(Sender: TObject);
var
  trans: anTransform;
begin
  if Assigned(Editor.SelectedObject) then begin

    trans := Editor.SelectedObject.Transformation.TransformData^;

    trans.Flip := not trans.Flip;
    trans.Rotation := trans.Rotation + 180;

    Editor.History.AddItem(TEditorUpdateLOTransformItem.Create(
      Editor.SelectedObject,
      trans
    ));
  end;
end;

procedure TMainForm.MoveDownInstanceClick(Sender: TObject);
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
      anAnimationPrototype(Editor.SelectedAnimation).MoveObject(anAnimationLayerObject(Editor.ContextObject),
        anAnimationLayerObject(Editor.ContextObject).Index + 1);
      UpdateAnimation;
    end;
  end;
end;

procedure TMainForm.MoveUpInstanceClick(Sender: TObject);
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
      anAnimationPrototype(Editor.SelectedAnimation).MoveObject(anAnimationLayerObject(Editor.ContextObject),
        anAnimationLayerObject(Editor.ContextObject).Index - 1);
      UpdateAnimation;
    end;
  end;
end;

procedure TMainForm.NewContentItemButtonAndBrowseClick(Sender: TObject);
var cn: anTextureContent;
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(nil);
  if od.Execute then begin
    cn := Editor.Project.Animation.AddContent('NONAME', anTextureContent);
    RegisterContent(cn, true);
    cn.LoadFromFile(od.FileName, MainForm.GetSavePromt(od));
    HandleObject(cn);
  end;
  od.Free;
end;

procedure TMainForm.OpenProjectAnimationClick(Sender: TObject);
var od: TOpenDialog;
begin
  if CheckForSave(Sender, true) then exit;
  od := TOpenDialog.Create(nil);
  od.Filter := 'Animation file (*.adb)|*.adb';
  od.Options := [ofFileMustExist];
  if od.Execute then begin
     Editor.Project.Import(od.FileName);
     MadeChanges;
  end;
  od.Free;
end;

procedure TMainForm.OtherComponentsClick(Sender: TObject);
var
  OthData: TOtherObjectRegistrationInfo;
begin
  OtherObjectDescription.Visible := Assigned(OtherComponents.Selected);
  if OtherObjectDescription.Visible then begin
     OthData := TOtherObjectRegistrationInfo(OtherComponents.Selected.Data);
     OtherObjectDescription.Text := OthData.Help;
  end;
end;

procedure TMainForm.PreviewBlendersClick(Sender: TObject);
var
  an: anAnimationPrototype;
  sel: TTreeNode;
  inst: anAnimationInstance;
  i: Integer;
begin
  sel := AnimationsSelector.Selected;
  if Assigned(sel) then begin
    an := anAnimationPrototype(sel.Data);
    PreviewBlenderForm.FillBlender(an);
    if PreviewBlenderForm.ShowModal = mrOK then begin
      inst := an.GetInstance;
      for i := 0 to PreviewBlenderForm.Blenders.Count - 1 do begin
        if PreviewBlenderForm.Blenders.Checked[i] then begin
          inst.AddBlender(anAnimationBlender(PreviewBlenderForm.Blenders.Items.Objects[i]));
        end;
      end;
      inst.Play;
      FillDrawList(inst, 'Preview', SLOT_ANIMATION, SLOT_PREVIEW_ID);
    end;
  end;
end;

procedure TMainForm.ProjectOptionsClick(Sender: TObject);
begin
  Editor.Project.Edit;
end;

procedure TMainForm.PublishProjectClick(Sender: TObject);
begin

end;


procedure TMainForm.RemoveFromKeyFrameClick(Sender: TObject);
var obj: anAnimationLayerObject;
    i: Integer;
    inst: anAnimationKeyFrameInstance;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
      obj := anAnimationLayerObject(Editor.ContextObject);
      inst := obj.GetHolder.InstanceAt[Editor.AnimInstance.CurrentFrame];
      inst.Free;
      Editor.RepaintTimeline;
      UpdateAnimation;
    end;
  end;
end;

procedure TMainForm.SetIMClick(Sender: TObject);
begin
  SetInterpolationMode(TMenuItem(Sender).ImageIndex);
end;

procedure TMainForm.FillDrawList(Obj: anNamedObject; SlotId, IconId: integer);
begin
  FillDrawList(Obj, Obj.Name, SlotId, IconId);
end;

procedure TMainForm.FillDrawList(Obj: TObject; pName: AnsiString; SlotId,
  IconId: integer);
var
  i, j: Integer;
begin
  if Obj is anAnimation then begin
    SetPanel(pmControls);
  end else
  if Obj is anAnimationInstance then begin
    SetPanel(pmPreview);
  end else
  if Obj is anAtlasTexture then begin
    SetPanel(pmAtlas);
  end else
    SetPanel(pmNone);
  with DrawSlots[SlotId] do begin
    RemoveDrawList(Obj, SlotId);
    if ItemsCount >= ITEMS_PER_SLOT then begin
      Data[0].Button.Free;
      for i := 0 to ITEMS_PER_SLOT - 2 do begin
        Data[i] := Data[i + 1];
      end;
      ItemsCount := ItemsCount - 1;
    end;
    Data[ItemsCount].Button := TToolButton.Create(HistoryButtons);
    Data[ItemsCount].Button.Parent := HistoryButtons;
    Data[ItemsCount].Button.Caption := pName;
    Data[ItemsCount].Button.ShowHint := true;
    Data[ItemsCount].Button.Hint := Obj.ClassName + ': ' + pName;
    Data[ItemsCount].Button.ImageIndex := IconId;
    Data[ItemsCount].Button.Tag := PtrInt(Obj);
    Data[ItemsCount].Button.Grouped := true;
    Data[ItemsCount].Button.Style := tbsCheck;
    Data[ItemsCount].Button.Down := true;
    Data[ItemsCount].Button.OnClick := @HistoryButtonClick;
    Data[ItemsCount].TheObject := Obj;
    Data[ItemsCount].Name := pName;

    ItemsCount := ItemsCount + 1;

    Editor.DrawData := Obj;
  end;
end;

procedure TMainForm.mmMoveClick(Sender: TObject);
begin
  Editor.ToolMode := tmMove;
  ModeMove.Down := true;
  mmMove.Checked := true;
end;

procedure TMainForm.mmRotateClick(Sender: TObject);
begin
  Editor.ToolMode := tmRotate;
  ModeRotate.Down := true;
  mmRotate.Checked := true;
end;

procedure TMainForm.mmScaleClick(Sender: TObject);
begin
  Editor.ToolMode := tmScale;
  ModeScale.Down := true;
  mmScale.Checked := true;
end;

procedure TMainForm.mmParentClick(Sender: TObject);
begin
  Editor.ToolMode := tmParent;
  RegisterParent.Down := true;
  mmParent.Checked := true;
end;

procedure TMainForm.NewInterpolationBtnClick(Sender: TObject);
var an: anAnimation;
    obj: anAnimationLOInstanceHolder;
    pos: integer;
    inst_from, inst_to, inst: anAnimationKeyFrameInstance;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Editor.SelectionMode = smDone then begin
      an := Editor.SelectedAnimation;
      obj := an.HolderByIndex[Editor.Selected.ObjectTo];
      inst_from := obj.InstanceAt[Editor.Selected.Left];
      inst_to := obj.InstanceAt[Editor.Selected.Right];
      inst := inst_from;
      if Assigned(inst) then
      repeat
        inst.InterpolationMode := Editor.NewInterpolation;
        inst := inst.Next;
      until (not Assigned(Inst)) or (inst.Position > inst_to.Position);
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.NewLibraryItemButtonAndAssignTextureClick(Sender: TObject);
begin
  SymbolCreation.Items.Clear;
  SymbolCreation.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;

procedure TMainForm.NewTextureItemButtonAndBrowseClick(Sender: TObject);
begin
  TextureCreation.Tag := 1;
  TextureCreation.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;

procedure TMainForm.ObjectMenuPopup(Sender: TObject);
var obj: anAnimationLayerObject;
    i: Integer;
    inst: anAnimationKeyFrameInstance;
    b: boolean;
begin
  if Assigned(Editor.SelectedAnimation) then begin
    if Assigned(Editor.ContextObject) then begin
      obj := anAnimationLayerObject(Editor.ContextObject);
      inst := obj.GetHolder.InstanceAt[Editor.AnimInstance.CurrentFrame];
      RemoveFromKeyFrame.Enabled := Assigned(inst) and (inst <> obj.GetHolder.FirstInstance);
      b := not Assigned(Editor.SelectedAnimation.Prototype);
      DeleteInstance.Enabled := b;
      MoveToFrontInstance.Enabled := b;
      MoveToBackInstance.Enabled := b;
      if Assigned(inst) and b then begin
        MoveDownInstance.Enabled := obj.Index < obj.Animation.ObjectsCount - 1;
        MoveUpInstance.Enabled := obj.Index > 0;
      end else begin
        MoveDownInstance.Enabled := false;
        MoveUpInstance.Enabled := false;
      end;
      exit;
    end;
  end;
  RemoveFromKeyFrame.Enabled := false;
end;

procedure TMainForm.OpenItemClick(Sender: TObject);
var od: TOpenDialog;
begin
  if CheckForSave(Sender) then exit;
  od := TOpenDialog.Create(nil);
  od.Filter := 'Animation project (*.apr)|*.apr';
  od.Options := [ofFileMustExist];
  if od.Execute then begin
     LoadAnimationFromFile(od.FileName);
  end;
  od.Free;
end;

procedure TMainForm.RecentItemsClick(Sender: TObject);
begin
  LoadAnimationFromFile(TMenuItem(Sender).Caption);
end;

procedure TMainForm.RegisterParentClick(Sender: TObject);
begin
  Editor.ToolMode := tmParent;
  mmParent.Checked := true;
end;

procedure TMainForm.RemoveDrawList(Obj: TObject; SlotId: integer);
var
  i: Integer;
  j: Integer;
begin
  if Editor.DrawData = Obj then
    Editor.DrawData := nil;
  with DrawSlots[SlotId] do begin
    for i := 0 to ItemsCount - 1 do begin
      if Data[i].TheObject = Obj then begin
        Data[i].Button.Free;
        for j := i to ItemsCount - 2 do begin
          Data[j] := Data[j + 1];
        end;
        ItemsCount := ItemsCount - 1;
        exit;
      end;
    end;
  end;
end;

procedure TMainForm.UpdateDrawList;
var
  j: Integer;
  i: Integer;
begin
  for j := 0 to SLOT_COUNT - 1 do begin
    with DrawSlots[j] do begin
      for i := 0 to ItemsCount - 1 do begin
        if Assigned(Data[i].TheObject) then begin
          if Data[i].TheObject.InheritsFrom(anNamedObject) then begin
            Data[i].Name := anNamedObject(Data[i].TheObject).Name;
          end;
          Data[i].Button.Caption := Data[i].Name;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.ClearDrawList;
var
  j: Integer;
  i: Integer;
begin
  for j := 0 to SLOT_COUNT - 1 do begin
    with DrawSlots[j] do begin
      for i := 0 to ItemsCount - 1 do begin
        if Assigned(Data[i].Button) then begin
          Data[i].Button.Free;
          Data[i].Button := nil;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.NewLibraryItemButtonAssignTextureClick(Sender: TObject);
var symbol: anSymbol;
    tex: anTexture;
    item: TMenuItem;
begin
  item := TMenuItem(Sender);
  tex := anTexture(item.Tag);

  symbol := Editor.Project.Animation.AnimationLibrary.AddSymbol(tex.Name + '_sym');

  symbol.Texture := tex;
  RegisterSymbol(symbol, true);
  MadeChanges;
  Editor.History.MadeChanges;

  DeleteLibraryItemButton.Enabled := true;
  CloneLibraryItemButton.Enabled := true;
end;

procedure TMainForm.AboutEditorClick(Sender: TObject);
begin
  ShowMessage('Animation editor for zengl animations.');
end;

procedure TMainForm.AnimationPlayClick(Sender: TObject);
begin
  if Assigned(Editor.AnimInstance) then begin
    if Editor.AnimInstance.TimeLine.State = asPlaying then begin
      Editor.AnimInstance.Stop(true);
      AnimationPlay.ImageIndex := 5;
    end else begin
      Editor.AnimInstance.Play;
      AnimationPlay.ImageIndex := 6;
    end;
    Editor.SelectedObject := nil;
  end;
end;

procedure TMainForm.AnimationPopupPopup(Sender: TObject);
var
  anim: anAnimation;
begin
  anim := anAnimation(AnimationsSelector.Selected.Data);
  CreatePrototypeIcon.Visible := anim is anAnimationPrototype;
  PreviewBlenders.Visible := anim is anAnimationPrototype;
end;

procedure TMainForm.AnimationPreviewPauseClick(Sender: TObject);
begin
  with Editor.DrawData as anAnimationInstance do
    Stop(true);
end;

procedure TMainForm.AnimationPreviewPlayClick(Sender: TObject);
begin
  with Editor.DrawData as anAnimationInstance do
    Play;
end;

procedure TMainForm.AnimationPreviewStopClick(Sender: TObject);
begin
  with Editor.DrawData as anAnimationInstance do
    Stop(false);
end;

procedure TMainForm.AnimationSettingsClick(Sender: TObject);
begin
  HandleObject(Editor.Project.Animation);
end;

procedure TMainForm.AnimationsSelectorClick(Sender: TObject);
var
  an: anAnimation;
  sel: TTreeNode;
begin
  sel := AnimationsSelector.Selected;
  DeleteAnimationBtn.Enabled := Assigned(sel);
  CloneAnimationBtn.Enabled := Assigned(sel);
  EditAnimationBtn.Enabled := Assigned(sel);
end;

procedure TMainForm.AnimationsSelectorDblClick(Sender: TObject);
var
  an: anAnimation;
  sel: TTreeNode;
begin
  sel := AnimationsSelector.Selected;
  if Assigned(sel) then begin
    an := anAnimation(sel.Data);
    HandleObject(an);
  end;
end;

procedure TMainForm.AnimationStopClick(Sender: TObject);
begin
  AnimationPlay.ImageIndex := 5;
  Editor.AnimInstance.GotoAndStop(0, false);
  Editor.RepaintTimeline;
  Editor.SelectedObject := nil;
end;

procedure TMainForm.AnimationUpdateTimer(Sender: TObject);
begin
  if Assigned(Editor.AnimInstance) then begin
    if Editor.AnimInstance.TimeLine.State = asPlaying then begin
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.AtlasAddRectZoneClick(Sender: TObject);
begin
  Editor.AtlasToolMode := atmAddRect;
end;

procedure TMainForm.AtlasAddShapeZoneClick(Sender: TObject);
begin
  Editor.AtlasToolMode := atmAddShape;
end;

procedure TMainForm.AtlasAskZoneNamesClick(Sender: TObject);
begin

end;

procedure TMainForm.AtlasDeleteZoneClick(Sender: TObject);
begin
  Editor.AtlasToolMode := atmDeleteZone;
end;

procedure TMainForm.AtlasEditPointZoneClick(Sender: TObject);
begin
  Editor.AtlasToolMode := atmEditPoints;
end;

procedure TMainForm.AtlasMoveZoneClick(Sender: TObject);
begin
  Editor.AtlasToolMode := atmMove;
end;

procedure TMainForm.AtlasRotateZoneClick(Sender: TObject);
begin
  Editor.AtlasToolMode := atmRotate;
end;

procedure TMainForm.AtlasScaleZoneClick(Sender: TObject);
begin
  Editor.AtlasToolMode := atmScale;
end;

procedure TMainForm.BtnAddTextureContentClick(Sender: TObject);
var an: anTextureContent;
begin
  an := Editor.Project.Animation.AddContent('NONAME', anTextureContent);
  an.OnNameChange := @OnContentNameChange;
  HandleObject(an);
  RegisterContent(an, true);
end;

procedure TMainForm.BtnEditTextureContentClick(Sender: TObject);
var
  node: TListItem;
begin
  node := TextureContent.Selected;
  if Assigned(node) then begin
    HandleObject(anTextureContent(node.Data));
  end;
  Editor.RepaintTimeline;
end;

procedure TMainForm.btnTabAnimationsClick(Sender: TObject);
var
  i: Integer;
begin
  DataControl.ActivePageIndex := TToolButton(Sender).Tag;
  for i := 0 to TabTools.ButtonCount - 1 do begin
    if TabTools.Buttons[i].Tag = DataControl.ActivePageIndex then begin
      TabTools.Buttons[i].Caption := TabTools.Buttons[i].Hint;
    end else
      TabTools.Buttons[i].Caption := '';
  end;
end;

procedure TMainForm.AddAnimationButtonClick(Sender: TObject);
var an: anAnimation;
begin
  an := Editor.Project.Animation.AddAnimation('NONAME', anAnimationPrototype);
  an.OnNameChange := @OnAnimationNameChange;
  SelectAnimation(an);
  HandleObject(an);
  RegisterAnimation(an, nil, true);
end;

procedure TMainForm.AtlasTextureClick(Sender: TObject);
begin
  CreateTexture(anAtlasTexture, 6);
end;

procedure TMainForm.CloneTextureItemButtonClick(Sender: TObject);
var texture, sel: anTexture;
    idx, nw: TListItem;
begin
  idx := TexturesItems.Selected;
  if Assigned(idx) then begin
    sel := anTexture(idx.Data);
    texture := Editor.Project.Animation.CloneTexture(sel.Name);
    if texture <> nil then begin
      RegisterTexture(texture, idx.ImageIndex, true);
      MadeChanges;
      HandleObject(texture);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, j: Integer;
begin
  Editor := TEditor.Create;

  for i := 0 to SLOT_COUNT - 1 do
    with DrawSlots[i] do begin
      for j := 0 to ITEMS_PER_SLOT - 1 do begin
        Data[j].Button := nil;
        Data[j].TheObject := nil;
        Data[j].Name := '';
      end;
    end;


  if Editor.Settings.Loaded then begin
    with Editor do begin
      SetInterpolationMode(Settings.Root.ChildByName['interpolation'].AsInteger);
      GridTool.Down := Settings.Root.ChildByName['draw_grid'].AsBoolean;
      GridSize := Settings.Root.ChildByName['grid'].AsInteger;
      SnapToGridTool.Checked := Settings.Root.ChildByName['snap_grid'].AsBoolean;
      RuleTool.Checked := Settings.Root.ChildByName['rulers'].AsBoolean;
      DrawBonesTool.Down := Settings.Root.ChildByName['draw_bones'].AsBoolean;
      DrawHightLightTool.Down := Settings.Root.ChildByName['highlight'].AsBoolean;
      AtlasDrawZoneNames.Down := Settings.Root.ChildByName['draw_zone_names'].AsBoolean;
      AtlasAskZoneNames.Down := Settings.Root.ChildByName['ask_zone_names'].AsBoolean;
    end;
  end;

  if GridSize = 0 then GridSize := 8;

  GridSizeTool.Caption := 'Grid size: ' + IntToStr(GridSize);

  for i := 0 to Editor.Recent.ChildCount - 1 do
    AddRecent(Editor.Recent.Child[i].AsString, false);

  Editor.Project.Animation.AnimationLibrary.OnNameChange := @OnSymbolNameChange;
  Editor.Project.Animation.OnTextureNameChange := @OnTextureNameChange;
  Editor.Project.Animation.OnAnimationNameChange := @OnAnimationNameChange;
  Editor.Project.Animation.OnContentNameChange := @OnContentNameChange;

  AnimMouseDown := false;

  RegisterPropertyEditor(TypeInfo(anTexture), nil, '', TTextureSelectPropertyEditor);
  RegisterPropertyEditor(TypeInfo(anTextureContent), nil, '', TContentSelectPropertyEditor);
  RegisterPropertyEditor(TypeInfo(anAnimationLayerObject), nil, 'LookUp', TLookUpPropertyEditor);
  RegisterPropertyEditor(TypeInfo(anAtlasTextureZone), nil, '', TAtlasZoneSelectPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), anAnimation, 'FPS', TFPSPropertyEditor);
  RegisterPropertyEditor(TypeInfo(anAnimatedSpriteTextureTileSet), anAnimatedSpriteTextureParams, '', TTileSetSelectPropertyEditor);
  RegisterPropertyEditor(TypeInfo(anAnimatedSpriteTextureTileSet), anOtherAnimationSpriteProperty, '', TOtherTileSetSelectPropertyEditor);

  TimeLineBox.NodeDataSize := SizeOf(Pointer);

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //Animation.Free;
end;

procedure TMainForm.LibraryItemsClick(Sender: TObject);
var
  sym: anSymbol;
  idx: TListItem;
begin
  idx := LibraryItems.Selected;
  if Assigned(idx) then begin
    sym := anSymbol(idx.Data);
    FillDrawList(sym, SLOT_SYMBOL, SLOT_SYMBOL_ID);
    DeleteLibraryItemButton.Enabled := true;
    CloneLibraryItemButton.Enabled := true;
  end else begin
    DeleteLibraryItemButton.Enabled := false;
    CloneLibraryItemButton.Enabled := false;
  end;
end;

procedure TMainForm.MainPanelClick(Sender: TObject);
begin

end;

procedure TMainForm.MainPanelDragOver
  (Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean
  );
begin
  Accept := true;
end;

procedure TMainForm.ModeScaleClick(Sender: TObject);
begin
  Editor.ToolMode := tmScale;
  mmScale.Checked := true;
end;

procedure TMainForm.ModeMoveClick(Sender: TObject);
begin
  Editor.ToolMode := tmMove;
  mmMove.Checked := true;
end;

procedure TMainForm.ModeRotateClick(Sender: TObject);
begin
  Editor.ToolMode := tmRotate;
  mmRotate.Checked := true;
end;

procedure TMainForm.NewLibraryItemButtonClick(Sender: TObject);
var symbol: anSymbol;
begin
  symbol := Editor.Project.Animation.AnimationLibrary.AddSymbol('newSymbol');

  RegisterSymbol(symbol, true);
  MadeChanges;
  Editor.History.MadeChanges;

  HandleObject(symbol);
  DeleteLibraryItemButton.Enabled := true;
  CloneLibraryItemButton.Enabled := true;
end;

procedure TMainForm.SpriteTextureClick(Sender: TObject);
begin
  CreateTexture(anFullTexture, 4);
end;

procedure TMainForm.StaticTextureClick(Sender: TObject);
begin
  CreateTexture(anStaticSpriteTexture, 7);
end;

procedure TMainForm.SymbolCreationPopup(Sender: TObject);
var
  i: Integer;
  item: TMenuItem;
begin
  for i := 0 to TexturesItems.Items.Count - 1 do begin
    item := TMenuItem.Create(SymbolCreation);
    item.Caption := TexturesItems.Items[i].Caption;
    item.ImageIndex := TexturesItems.Items[i].ImageIndex;
    item.Tag := PtrInt(TexturesItems.Items[i].Data);
    item.OnClick := @NewLibraryItemButtonAssignTextureClick;
    SymbolCreation.Items.Add(item);
  end;
end;

procedure TMainForm.TabControl1Change(Sender: TObject);
begin

end;

procedure TMainForm.TextureContentClick(Sender: TObject);
var
  sym: anTextureContent;
  idx: TListItem;
begin
  idx := TextureContent.Selected;
  if Assigned(idx) then begin
    sym := anTextureContent(idx.Data);
    FillDrawList(sym, SLOT_CONTENT, SLOT_CONTENT_ID);
    DeleteTextureContent.Enabled := true;
  end else begin
    DeleteTextureContent.Enabled := false;
  end;
end;

procedure TMainForm.TextureContentDblClick(Sender: TObject);
var
  idx: TListItem;
begin
  idx := TextureContent.Selected;
  if Assigned(idx) then begin
    HandleObject(TObject(idx.Data));
  end;
end;

procedure TMainForm.TexturesItemsClick(Sender: TObject);
var
  idx: TListItem;
  tex: anTexture;
begin
  idx := TexturesItems.Selected;
  if Assigned(idx) then begin
    tex := anTexture(idx.Data);
    FillDrawList(tex, SLOT_TEXTURE, idx.ImageIndex);
    DeleteTextureItemButton.Enabled := true;
    CloneTextureItemButton.Enabled := true;
  end else begin
    DeleteTextureItemButton.Enabled := false;
    CloneTextureItemButton.Enabled := false;
  end;
end;

procedure TMainForm.TimeLineBoxChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  obj_typ: TObject;
  obj: anAnimationLayerObject;
begin
  if Assigned(Node) then begin
    obj_typ := TObject(Sender.GetNodeData(Node)^);
    if obj_typ is anAnimationLayerObject then begin
      obj := obj_typ as anAnimationLayerObject;
      Editor.Selected.ObjectTo := obj.Index;
      Editor.SelectionMode := smNone;
      Editor.RepaintTimeline;
    end;
  end;
end;

procedure TMainForm.TimeLineBoxDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var obj: anAnimationLayerObject;
  idx: Integer;
  obj_typ: TObject;
  inst: anAnimationLOInstanceHolder;
  key: anAnimationKeyFrameInstance;
  frameid: Integer;
  img_idx: Integer;
  ps_x: Integer;
  ps_y: Integer;
  d_l: anPosition;
  d_r: anPosition;
  temp: anPosition;
  obj_tag: TPresetTag;
  checkRes: Boolean;
begin
  with PaintInfo.Canvas do begin
    obj_typ := TObject(Sender.GetNodeData(PaintInfo.Node)^);
    if obj_typ is TPresetTag then begin
      obj_tag := obj_typ as TPresetTag;
      case PaintInfo.Column of
        COLUMN_NAME: begin
          Brush.Color := $CCFFCC;
          FillRect(PaintInfo.ContentRect);
          if vsExpanded in PaintInfo.Node^.States then
            idx := IMG_TREE_EXPANDED else idx := IMG_TREE_NOT_EXPANDED;
          MiniImages.Draw(PaintInfo.Canvas,
            PaintInfo.ContentRect.Left + IMG_HEADER_OFFSET,
            (PaintInfo.ContentRect.Bottom - PaintInfo.ContentRect.Top) div 2 - IMG_SMALL_SIZE div 2,
            idx);


          checkRes := false;
          MakeOperation(PaintInfo.Node, actCheckVisible, @checkRes);
          if checkRes then idx := IMG_VISIBLE else idx := IMG_VISIBLE_S;

          MiniImages.Draw(PaintInfo.Canvas,
            PaintInfo.ContentRect.Left + IMG_HEADER_OFFSET * 2 + IMG_SMALL_SIZE + 2,
            (PaintInfo.ContentRect.Bottom - PaintInfo.ContentRect.Top) div 2 - IMG_SMALL_SIZE div 2,
            idx);

          checkRes := false;
          MakeOperation(PaintInfo.Node, actCheckEnabled, @checkRes);
          if checkRes then idx := IMG_DISABLED else idx := IMG_DISABLED_S;

          MiniImages.Draw(PaintInfo.Canvas,
            PaintInfo.ContentRect.Left + IMG_HEADER_OFFSET * 2 + IMG_SMALL_SIZE * 2 + 2,
            (PaintInfo.ContentRect.Bottom - PaintInfo.ContentRect.Top) div 2 - IMG_SMALL_SIZE div 2,
            idx);

          TextOut(PaintInfo.ContentRect.Left + IMG_HEADER_OFFSET * 2 + 4 +
            IMG_SMALL_SIZE * 3, PaintInfo.ContentRect.Top,
            obj_tag.Tag);
        end;
        COLUMN_TIMELINE: begin
          Brush.Color := $FFFFFF;
          FillRect(PaintInfo.ContentRect);
          Pen.Color := $3333BB;
          idx := PaintInfo.ContentRect.Left + Editor.AnimInstance.CurrentFrame * FRAME_SIZE + FRAME_SIZE div 2;
          Line(idx, PaintInfo.ContentRect.Top, idx, PaintInfo.ContentRect.Bottom - 1);
        end;
      end;
    end else
    if obj_typ is anAnimationLayerObject then begin
      obj := obj_typ as anAnimationLayerObject;
      if Assigned(Editor.SelectedObject) and (Editor.SelectedObject.ObjectTo = obj) then
        Brush.Color := $9999FF
      else
        Brush.Color := $FFFFFF;
      FillRect(PaintInfo.ContentRect);
      case PaintInfo.Column of
        COLUMN_NAME: begin
          if obj.Visible then idx := IMG_VISIBLE else idx := IMG_VISIBLE_S;
          MiniImages.Draw(PaintInfo.Canvas,
            PaintInfo.ContentRect.Left + IMG_HEADER_OFFSET,
            (PaintInfo.ContentRect.Bottom - PaintInfo.ContentRect.Top) div 2 - IMG_SMALL_SIZE div 2,
            idx);
          if obj.Enabled then idx := IMG_DISABLED else idx := IMG_DISABLED_S;
          MiniImages.Draw(PaintInfo.Canvas,
            PaintInfo.ContentRect.Left + IMG_HEADER_OFFSET + IMG_SMALL_SIZE,
            (PaintInfo.ContentRect.Bottom - PaintInfo.ContentRect.Top) div 2 - IMG_SMALL_SIZE div 2,
            idx);
          TextOut(PaintInfo.ContentRect.Left + IMG_HEADER_OFFSET * 2 +
            IMG_SMALL_SIZE * 2, PaintInfo.ContentRect.Top,
            obj.Name);
        end;
        COLUMN_TIMELINE: begin
          if PaintInfo.Node^.Index mod 2 = 0 then
            Brush.Color := $DDDDDD
          else
            Brush.Color := $EEEEEE;
          FillRect(PaintInfo.ContentRect);

          // selection
          if (Editor.Selected.ObjectTo = obj.Index) and (Editor.SelectionMode in [smStart, smDone]) then begin
            d_l := Editor.Selected.Left;
            d_r := Editor.Selected.Right;
            if d_l > d_r then begin
              temp := d_l;
              d_l := d_r;
              d_r := temp;
            end;
            Brush.Color := $FFD59B;
            FillRect(
              PaintInfo.ContentRect.Left + d_l * FRAME_SIZE,
              PaintInfo.ContentRect.Top,
              PaintInfo.ContentRect.Left + (d_r + 1) * FRAME_SIZE,
              PaintInfo.ContentRect.Bottom - 1
              );
          end;

          Pen.Color := $999999;
          Line(PaintInfo.ContentRect.Left, PaintInfo.ContentRect.Bottom - 1, PaintInfo.ContentRect.Right, PaintInfo.ContentRect.Bottom - 1);
          if PaintInfo.Node^.Index = 0 then
            Line(PaintInfo.ContentRect.Left, PaintInfo.ContentRect.Top, PaintInfo.ContentRect.Right, PaintInfo.ContentRect.Top);


          Pen.Color := $3333BB;
          idx := PaintInfo.ContentRect.Left + Editor.AnimInstance.CurrentFrame * FRAME_SIZE + FRAME_SIZE div 2;
          Line(idx, PaintInfo.ContentRect.Top, idx, PaintInfo.ContentRect.Bottom - 1);

          Pen.Color := $999999;
          for idx := 0 to Editor.SelectedAnimation.FramesCount do begin
            Line(
              PaintInfo.ContentRect.Left + idx * FRAME_SIZE,
              PaintInfo.ContentRect.Top,
              PaintInfo.ContentRect.Left + idx * FRAME_SIZE,
              PaintInfo.ContentRect.Bottom - 1);
          end;
          inst := Editor.SelectedAnimation.HolderByIndex[obj.Index];

          // keyframes
          key := inst.FirstInstance;
          frameid := 0;
          while assigned(key) do begin
            case key.InterpolationMode of
             imLinear: begin
               Pen.Color := $FFFFFF;
               Pen.Style := psSolid;
             end;
             imSin: begin
               Pen.Color := $FF0000;
               Pen.Style := psSolid;
             end;
             imSinFadeIn: begin
               Pen.Color := $FF8800;
               Pen.Style := psDash;
             end;
             imSinFadeOut: begin
               Pen.Color := $8800FF;
               Pen.Style := psDash;
             end;
            end;
            if key.InterpolationMode <> imNone then begin
              ps_x := PaintInfo.ContentRect.Left + (frameid + key.Frames) * FRAME_SIZE - 1;
              ps_y := (PaintInfo.ContentRect.Bottom - PaintInfo.ContentRect.Top) div 2;

              Line(
                PaintInfo.ContentRect.Left + frameid * FRAME_SIZE + FRAME_SIZE div 2,
                ps_y,
                PaintInfo.ContentRect.Left + (frameid + key.Frames) * FRAME_SIZE,
                ps_y);

              Line(ps_x - 2, ps_y - 2, ps_x, ps_y);
              Line(ps_x - 2, ps_y + 2, ps_x, ps_y);
            end;
            inc(frameid, key.Frames);
            key := key.Next;
          end;

          // keyframe points
          key := inst.FirstInstance;
          frameid := 0;
          while assigned(key) do begin
            if key.Name = '' then begin
              if (key.ActiveProperties.OnActivateCallback <> '') or
                 (key.ActiveProperties.OnFrameCallback <> '') then
                 img_idx := IMG_CALLBACK_FLAG
                else begin
                  if key.ActiveProperties.ThisAnimationAction.Action <> apaNone then
                    img_idx := IMG_ACTION_FLAG
                  else
                    img_idx := IMG_KEYFRAME;
                end;
            end else img_idx := IMG_KEYFRAME_FLAG;
            MiniImages.Draw(PaintInfo.Canvas,
              PaintInfo.ContentRect.Left + frameid * FRAME_SIZE + FRAME_SIZE div 2 - IMG_SMALL_SIZE div 2,
              (PaintInfo.ContentRect.Bottom - PaintInfo.ContentRect.Top) div 2 - IMG_SMALL_SIZE div 2, img_idx
              );
            inc(frameid, key.Frames);
            key := key.Next;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.TimeLineBoxGetNodeWidth(Sender: TBaseVirtualTree;
  HintCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  var NodeWidth: Integer);
begin
  case Column of
    COLUMN_TIMELINE: begin
      if Assigned(Editor.SelectedAnimation) then
        NodeWidth := Editor.SelectedAnimation.FramesCount * FRAME_SIZE;
    end;
  end;
end;

procedure TMainForm.TimeLineBoxHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
end;

procedure TMainForm.TimeLineBoxHeaderDraw(Sender: TVTHeader;
  HeaderCanvas: TCanvas; Column: TVirtualTreeColumn; const R: TRect; Hover,
  Pressed: Boolean; DropMark: TVTDropMarkMode);
var
  i, idx: Integer;
  txt: String;
  preset: TAnimationPreset;
  checkRes: Boolean;
begin
  case Column.Index of
    COLUMN_NAME: begin
      HeaderCanvas.Brush.Color := $FFFFFF;
      HeaderCanvas.Pen.Color := $000000;
      HeaderCanvas.Pen.Style := psDot;
      HeaderCanvas.Line(R.Right - 1, R.Top, R.Right - 1, R.Bottom);
      HeaderCanvas.Pen.Style := psSolid;

      checkRes := false;
      MakeOperation(TimeLineBox.RootNode, actCheckVisible, @checkRes);
      if checkRes then idx := IMG_VISIBLE else idx := IMG_VISIBLE_S;

      MiniImages.Draw(HeaderCanvas,
        R.Left + IMG_HEADER_OFFSET * 2,
        (R.Bottom - R.Top) div 2 - IMG_SMALL_SIZE div 2,
        idx);

      checkRes := false;
      MakeOperation(TimeLineBox.RootNode, actCheckEnabled, @checkRes);
      if checkRes then idx := IMG_DISABLED else idx := IMG_DISABLED_S;

      MiniImages.Draw(HeaderCanvas,
        R.Left + IMG_HEADER_OFFSET * 2 + IMG_SMALL_SIZE,
        (R.Bottom - R.Top) div 2 - IMG_SMALL_SIZE div 2,
        idx);
      if Assigned(Editor.SelectedAnimation) then
        preset := Editor.Project.GetPreset(Editor.SelectedAnimation.Name)
      else
        preset := nil;
      if Assigned(preset) and (preset.Mode = modeTags)
        then idx := IMG_MODE_TAGS else idx := IMG_MODE_NORMAL;
      MiniImages.Draw(HeaderCanvas,
        R.Left + IMG_HEADER_OFFSET * 3 + IMG_SMALL_SIZE * 2,
        (R.Bottom - R.Top) div 2 - IMG_SMALL_SIZE div 2,
        idx);
      if Assigned(preset) and (preset.Filter <> '')
        then idx := IMG_FILTER_ON else idx := IMG_FILTER_OFF;
      MiniImages.Draw(HeaderCanvas,
        R.Right - IMG_SMALL_SIZE - IMG_HEADER_OFFSET,
        (R.Bottom - R.Top) div 2 - IMG_SMALL_SIZE div 2,
        idx);
    end;
    COLUMN_TIMELINE: begin
      if Assigned( Editor.SelectedAnimation) then begin
        HeaderCanvas.Brush.Color := $EEEEEE;
        HeaderCanvas.Pen.Color := $BBBBBB;
        HeaderCanvas.FillRect(
          R.Left + FRAME_OFFSETX,
          R.Top + 1,
          R.Right, R.Bottom - 4);
        for i := 0 to Editor.SelectedAnimation.FramesCount do
          HeaderCanvas.Line(
            R.Left + i * FRAME_SIZE + FRAME_OFFSETX,
            R.Top + 1,
            R.Left + i * FRAME_SIZE + FRAME_OFFSETX,
            R.Bottom - 3);
        HeaderCanvas.Line(R.Left + FRAME_OFFSETX, R.Bottom - 4, R.Right, R.Bottom - 4);
        HeaderCanvas.Line(R.Left + FRAME_OFFSETX, R.Top + 1, R.Right, R.Top + 1);

        HeaderCanvas.Brush.Color := clNone;
        HeaderCanvas.Pen.Color := $3333BB;
        HeaderCanvas.Rectangle(
          R.Left + Editor.AnimInstance.TimeLine.CurrentFrame * FRAME_SIZE + FRAME_OFFSETX,
          R.Top + 1,
          R.Left + (Editor.AnimInstance.TimeLine.CurrentFrame + 1) * FRAME_SIZE + FRAME_OFFSETX,
          R.Bottom - 3);

        HeaderCanvas.Brush.Color := $3333BB;

        HeaderCanvas.FillRect(
          R.Left + Editor.AnimInstance.TimeLine.CurrentFrame * FRAME_SIZE + FRAME_OFFSETX,
          R.Bottom - 6,
          R.Left + (Editor.AnimInstance.TimeLine.CurrentFrame + 1) * FRAME_SIZE + FRAME_OFFSETX,
          R.Bottom);

        HeaderCanvas.Brush.Color := clNone;

        for i := 0 to Editor.SelectedAnimation.FramesCount - 1 do begin
          if (i = 0) or ((i + 1) mod 5 = 0) then begin
            txt := IntToStr(i + 1) ;
            HeaderCanvas.TextOut(
              R.Left + i * FRAME_SIZE + FRAME_OFFSETX + FRAME_SIZE div 2 -
                HeaderCanvas.TextWidth(txt) div 2,
              R.Top + 4, txt
              );
          end;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.TimeLineBoxHeaderMouseDown(Sender: TVTHeader;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  hideRes: Boolean;
  idx: Integer;
  str_text: String;
begin
  AnimMouseDown := true;
  case GetActiveColumn(X) of
    COLUMN_NAME: begin
      if CheckHitY(X, IMG_HEADER_OFFSET * 2, IMG_SMALL_SIZE) then begin
        hideRes := false;
        MakeOperation(TimeLineBox.RootNode, actCheckVisible, @hideRes);
        hideRes := not hideRes;
        MakeOperation(TimeLineBox.RootNode, actSetVisible, @hideRes);
        Editor.RepaintTimeline;
      end;
      if CheckHitY(X, IMG_HEADER_OFFSET * 2 + IMG_SMALL_SIZE, IMG_SMALL_SIZE) then begin
        hideRes := false;
        MakeOperation(TimeLineBox.RootNode, actCheckEnabled, @hideRes);
        hideRes := not hideRes;
        MakeOperation(TimeLineBox.RootNode, actSetEnabled, @hideRes);
        Editor.RepaintTimeline;
      end;
      if CheckHitY(X, IMG_HEADER_OFFSET * 3 + IMG_SMALL_SIZE * 2, IMG_SMALL_SIZE) then begin
        with Editor do begin
          with Project.GetPreset(SelectedAnimation.Name) do begin
            case Mode of
              modeNormal: Mode := modeTags;
              modeTags: Mode := modeNormal;
            end;
            SyncAnimation(SelectedAnimation);
          end;
        end;
      end;
      if CheckHitY(X, TimeLineBox.Header.Columns[COLUMN_NAME].Width - IMG_SMALL_SIZE - IMG_HEADER_OFFSET, IMG_SMALL_SIZE) then begin
        with Editor do begin
          with Project.GetPreset(SelectedAnimation.Name) do begin
            if Filter = '' then
              Filter := InputBox('Enter filter', 'leave empty for no filter', '')
            else
              Filter := '';
            MakeOperation(TimeLineBox.RootNode, actShowFilter, @Filter);
          end;
        end;
      end;
    end;
    COLUMN_TIMELINE: begin
      idx := TimeLineBox.Header.Columns[COLUMN_TIMELINE].Left;
      if X >= TimeLineBox.Header.Columns[COLUMN_TIMELINE].Left + FRAME_OFFSETX then begin
        idx := (X - (TimeLineBox.Header.Columns[COLUMN_TIMELINE].Left + FRAME_OFFSETX)) div FRAME_HEIGHT;
        Editor.AnimInstance.TimeLine.CurrentFrame := idx;
        if Editor.AnimInstance.TimeLine.State = asPlaying then begin
          AnimationPlay.Click;
        end;
        Editor.RepaintTimeline;
      end;
    end;
  end;
end;

procedure TMainForm.TimeLineBoxHeaderMouseMove(Sender: TVTHeader;
  Shift: TShiftState; X, Y: Integer);
var
  idx: Integer;
begin
  if AnimMouseDown then
    case GetActiveColumn(X) of
      COLUMN_TIMELINE: begin
        idx := TimeLineBox.Header.Columns[COLUMN_TIMELINE].Left;
        if X >= TimeLineBox.Header.Columns[COLUMN_TIMELINE].Left + FRAME_OFFSETX then begin
          idx := (X - (TimeLineBox.Header.Columns[COLUMN_TIMELINE].Left + FRAME_OFFSETX)) div FRAME_HEIGHT;
          Editor.AnimInstance.TimeLine.CurrentFrame := idx;
          Editor.RepaintTimeline;
        end;
      end;
    end;
end;

procedure TMainForm.TimeLineBoxHeaderMouseUp(Sender: TVTHeader;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AnimMouseDown := false;
end;

procedure TMainForm.TimeLineBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  obj: anAnimationLayerObject;
  f_id: Integer;
  node: PVirtualNode;
  obj_typ: TObject;
  obj_tag: TPresetTag;
  hideRes: Boolean;
begin
  MouseDownShift := Shift;
  if Y <= TimeLineBox.Header.Height then exit;
  node := TimeLineBox.GetNodeAt(X, Y - TimeLineBox.Header.Height);
  if not Assigned(node) then exit;
  X := X - TimeLineBox.GetNodeLevel(node) * 18;
  obj_typ := TObject(TimeLineBox.GetNodeData(node)^);
  if obj_typ is TPresetTag then begin
    obj_tag := obj_typ as TPresetTag;
    case GetActiveColumn(X) of
      COLUMN_NAME: begin
        if CheckHitY(X, IMG_HEADER_OFFSET * 2, IMG_SMALL_SIZE) then begin
          TimeLineBox.Expanded[node] := not TimeLineBox.Expanded[node];
          obj_tag.Visible := TimeLineBox.Expanded[node];
        end;
        if CheckHitY(X, IMG_HEADER_OFFSET * 2 + IMG_SMALL_SIZE + 2, IMG_SMALL_SIZE) then begin
          hideRes := false;
          MakeOperation(node, actCheckVisible, @hideRes);
          hideRes := not hideRes;
          MakeOperation(node, actSetVisible, @hideRes);
          Editor.RepaintTimeline;
        end;
        if CheckHitY(X, IMG_HEADER_OFFSET * 2 + IMG_SMALL_SIZE * 2 + 2, IMG_SMALL_SIZE) then begin
          hideRes := false;
          MakeOperation(node, actCheckEnabled, @hideRes);
          hideRes := not hideRes;
          MakeOperation(node, actSetEnabled, @hideRes);
          Editor.RepaintTimeline;
        end;
      end;
    end;
  end else
  if obj_typ is anAnimationLayerObject then begin
    obj := obj_typ as anAnimationLayerObject;
    case GetActiveColumn(X) of
      COLUMN_NAME: begin
        if CheckHitY(X, IMG_HEADER_OFFSET * 2, IMG_SMALL_SIZE) then begin
          obj.Visible := not obj.Visible;
        end else
        if CheckHitY(X, IMG_HEADER_OFFSET * 2 + IMG_SMALL_SIZE, IMG_SMALL_SIZE) then begin
          obj.Enabled := not obj.Enabled;
        end else begin
          Editor.SelectedObject := Editor.SelectedAnimation.HolderByIndex[obj.Index].InstanceAt[Editor.AnimInstance.CurrentFrame];
        end;
        Editor.RepaintTimeline;
      end;
      COLUMN_TIMELINE: begin
        AnimMouseDown := true;
        f_id := (X - TimeLineBox.Header.Columns[COLUMN_NAME].Width - 4) div FRAME_SIZE;
        if (f_id < Editor.Selected.Left) or (f_id > Editor.Selected.Right) then begin
           Editor.SelectFrame(f_id);
        end;
      end;
    end;
  end;
end;

procedure TMainForm.TimeLineBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  obj: anAnimationLayerObject;
begin
  if not AnimMouseDown then exit;
  if Editor.Selected.ObjectTo < 0 then exit;
  if Y <= TimeLineBox.Header.Height then exit;
  obj := Editor.SelectedAnimation.ObjectByIndex[Editor.Selected.ObjectTo];
  case GetActiveColumn(X) of
    COLUMN_TIMELINE: begin
      Editor.SelectFrame((X - TimeLineBox.Header.Columns[COLUMN_NAME].Width - 4) div FRAME_SIZE);
    end;
  end;
end;

procedure TMainForm.TimeLineBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  node: PVirtualNode;
  obj_typ: TObject;
  obj: anAnimationLayerObject;
  f_id: Integer;
begin
  AnimMouseDown := false;
  if Y <= TimeLineBox.Header.Height then exit;
  node := TimeLineBox.GetNodeAt(X, Y - TimeLineBox.Header.Height);
  if not Assigned(node) then exit;
  obj_typ := TObject(TimeLineBox.GetNodeData(node)^);
  if obj_typ is anAnimationLayerObject then begin
    obj := obj_typ as anAnimationLayerObject;
    case GetActiveColumn(X) of
      COLUMN_NAME: begin
          if ssRight in MouseDownShift then begin
            Editor.ContextObject := obj;
            ObjectMenu.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
          end;
        end;
      COLUMN_TIMELINE: begin
        if ssRight in MouseDownShift then begin
          KeyFramesPopup.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
        end;
        if Editor.Selected.ObjectTo < 0 then exit;
        f_id := (X - TimeLineBox.Header.Columns[COLUMN_NAME].Width - 4) div FRAME_SIZE;
        Editor.SelectDone(f_id);
      end;
    end;
  end;
end;

procedure TMainForm.TimeLineBoxResize(Sender: TObject);
begin
  Editor.RepaintTimeline;
end;

procedure TMainForm.UndoClick(Sender: TObject);
begin
  if mouse_state.state = msNone then
    Editor.History.UndoAction;
end;

procedure TMainForm.UnregisterParentClick(Sender: TObject);
var
  obj, obj_parent: anAnimationLOInstanceHolder;
begin
  if Assigned(Editor.SelectedObject) then begin
    obj := Editor.SelectedAnimation.HolderByIndex[Editor.SelectedObject.ObjectTo.Index];
    obj_parent := obj.ParentHolder;
    Editor.History.AddItem(TEditorRemoveParentItem.Create(
      Editor.SelectedObject.ObjectTo,
      obj,
      obj_parent,
      Editor.SelectedAnimation));
    UpdateAnimation;
  end;
end;

procedure TMainForm.Zoom25Click(Sender: TObject);
begin
  mouse_state.pos.X := MainForm.DrawPanel.ClientWidth / 2 - Editor.RenderRect.X;
  mouse_state.pos.Y := MainFOrm.DrawPanel.ClientHeight / 2 - Editor.RenderRect.Y;
  Editor.ZoomTo(TMenuItem(Sender).Tag / 100);
end;

procedure TMainForm.ZoomInToolClick(Sender: TObject);
begin
  mouse_state.pos.X := MainForm.DrawPanel.ClientWidth / 2 - Editor.RenderRect.X;
  mouse_state.pos.Y := MainFOrm.DrawPanel.ClientHeight / 2 - Editor.RenderRect.Y;
  Editor.MultZoom(1.1);
end;

procedure TMainForm.ZoomOutToolClick(Sender: TObject);
begin
  mouse_state.pos.X := MainForm.DrawPanel.ClientWidth / 2 - Editor.RenderRect.X;
  mouse_state.pos.Y := MainFOrm.DrawPanel.ClientHeight / 2 - Editor.RenderRect.Y;
  Editor.MultZoom(0.9);
end;

procedure TMainForm.ZoomResetToolClick(Sender: TObject);
begin
  Editor.ResetZoom;
end;

function TMainForm.CheckHitY(X: Integer; Val, ValWidth: Integer): Boolean;
begin
  Result := (X >= Val) and (X < Val + ValWidth);
end;

procedure TMainForm.MakeOperation(Node: PVirtualNode; MakeAction: TNodeAction;
  Data: Pointer; Result: PPointer);
var
  NextNode: PVirtualNode;
  obj: TObject;
  obj_layer: anAnimationLayerObject;
begin
  NextNode := Node^.FirstChild;
  while Assigned(NextNode) do begin
    obj := TObject(TimeLineBox.GetNodeData(NextNode)^);
    if obj is anAnimationLayerObject then begin
      obj_layer := obj as anAnimationLayerObject;
      case MakeAction of
        actCheckVisible: begin
          if obj_layer.Visible then PBoolean(Data)^ := true;
        end;
        actCheckEnabled: begin
          if obj_layer.Enabled then PBoolean(Data)^ := true;
        end;
        actSetVisible: begin
          obj_layer.Visible := PBoolean(Data)^;
        end;
        actSetEnabled: begin
          obj_layer.Enabled := PBoolean(Data)^;
        end;
        actShowFilter: begin
          TimeLineBox.IsVisible[NextNode] := (Pos(PString(Data)^, obj_layer.Name) > 0) or (PString(Data)^ = '');
        end;
        actFindWithData: begin
          if Assigned(Result^) then exit;
          if Pointer(obj_layer) = data then begin
            Result^ := NextNode;
            exit;
          end;
        end;
      end;
    end;
    if NextNode^.ChildCount > 0 then
      MakeOperation(NextNode, MakeAction, Data, Result);
    NextNode := NextNode^.NextSibling;
  end;
  case MakeAction of
    actFindWithData: begin
      Result^ := nil;
    end;
  end;
end;

function TMainForm.GetActiveColumn(ClickX: Integer): Integer;
begin
  if ClickX <= TimeLineBox.Header.Columns[0].Width then
    exit(COLUMN_NAME)
  else
    exit(COLUMN_TIMELINE);
end;

function TMainForm.GetSavePromt(od: TOpenDialog; ForceFileName: AnsiString
  ): AnsiString;
begin
  if ForceFileName <> '' then exit(ForceFileName);

  if CheckForSave(nil, true) then begin
    exit(od.FileName);
  end;
  Result := ExtractRelativepath(Editor.Project.ProjectFileName, od.FileName);

end;

procedure TMainForm.SetInterpolationMode(Index: Integer);
begin
  NewInterpolationBtn.ImageIndex := Index;
  NewInterpolationBtn.Hint := 'Interpolation for new keys: ' + InterpolationPopup.Items[Index].Caption;
  Editor.NewInterpolation := anInterpolationMode(Index);
end;

procedure TMainForm.MadeChanges;
begin
  Editor.Project.EditorChanged := true;
  Caption := AppName + ' [NOT SAVED]';
end;

procedure TMainForm.SetPanel(Mode: TPanelMode);
begin
  AnimationButtons.Visible := Mode = pmControls;
  FunctionalPanel.Visible := Mode = pmControls;
  AtlasButtons.Visible := Mode = pmAtlas;
  PreviewButtons.Visible := Mode = pmPreview;
end;

function TMainForm.SelectedTexture: anTexture;
var
  idx: TListItem;
begin
  idx := TexturesItems.Selected;
  if Assigned(idx) then
    exit(anTexture(idx.Data));
  exit(nil);
end;

procedure TMainForm.RegisterTexture(tex: anTexture; ImgIndex: integer;
  Display: boolean);
var
  nw: TListItem;
begin
  nw := TexturesItems.Items.Add;
  with nw do begin
    Caption := tex.Name;
    Data := tex;
    ImageIndex := ImgIndex;
  end;
  if Display then begin
    TexturesItems.Selected := nw;
    FillDrawList(tex, SLOT_TEXTURE, nw.ImageIndex);
  end;
end;

procedure TMainForm.UnRegisterTexture(tex: anTexture);
var
  idx: TListItem;
  i: Integer;
begin
  RemoveDrawList(tex, SLOT_TEXTURE);

  idx := TexturesItems.Items.FindData(tex);
  if Assigned(idx) then begin
    idx.Free;
    TexturesItems.Selected := nil;
  end;
end;

function TMainForm.RegisterAnimation(anim, proto: anAnimation; Display: boolean
  ): TTreeNode;
var
  node: TTreeNode;
  pnode: TTreeNode;
  img: Integer;
begin
  img := GetAnimationImgId(anim);
  anim.OnNameChange := @OnAnimationNameChange;
  if Assigned(proto) then
    pnode := AnimationsSelector.Items.FindNodeWithData(proto)
  else
    pnode := nil;
  node := AnimationsSelector.Items.AddChild(pnode,
    anim.Name);
  node.Data := anim;
  node.StateIndex := img;
  if Display then begin
    FillDrawList(anim, SLOT_ANIMATION, img);
    AnimationsSelector.Selected := node;
  end;
  Result := node;
end;

procedure TMainForm.UnRegisterAnimation(anim: anAnimation);
var
  node: TTreeNode;
begin
  RemoveDrawList(anim, SLOT_ANIMATION);
  Editor.SelectedAnimation := nil;
  AnimationsSelector.Selected := nil;
  node := AnimationsSelector.Items.FindNodeWithData(anim);
  if Assigned(node) then begin
    AnimationsSelector.Items.Delete(node);
  end;
end;

procedure TMainForm.UpdateAnimation;
var
  anim: anAnimation;
  blend: anAnimationBlender;
  i: Integer;
begin
  Editor.SyncAnimation(Editor.SelectedAnimation);
  for i := 0 to Editor.Project.Animation.Animations.Count - 1 do begin
    anim := Editor.Project.Animation.Animations.Data[i];
    if anim is anAnimationBlender then begin
      blend := anAnimationBlender(anim);
      if blend.Prototype = Editor.SelectedAnimation then
        blend.Update;
    end;
  end;
  Editor.UpdateAnimationInstance;
end;

procedure TMainForm.RegisterContent(cnt: anTextureContent; Display: boolean);
var
  node: TListItem;
begin
  cnt.OnNameChange := @OnContentNameChange;
  node := TextureContent.Items.Add;
  with node do begin
    Caption := cnt.Name;
    ImageIndex := SLOT_CONTENT_ID;
    Data := cnt;
  end;
  if Display then begin
    FillDrawList(cnt, SLOT_CONTENT, SLOT_CONTENT_ID);
    TextureContent.Selected := node;
  end;
end;

procedure TMainForm.UnRegisterContent(cnt: anTextureContent);
var
  node: TListItem;
begin
  RemoveDrawList(cnt, SLOT_CONTENT);
  TextureContent.Selected := nil;
  node := TextureContent.Items.FindData(cnt);
  if Assigned(node) then begin
    node.Free;
  end;
end;

procedure TMainForm.RegisterLayerObject(obj: anAnimationLayerObject);
var
  inst: anAnimationKeyFrameInstance;
  prot: anAnimationPrototype;
begin
  with Editor.DragState.Transform do begin
    Position.X := Editor.DragState.Pos.X;
    Position.Y := Editor.DragState.Pos.Y;
    Scale := 1;
    Rotation := 0;
    Transparency := 255;
  end;

  if Editor.SelectedAnimation is anAnimationPrototype then begin
    prot := Editor.SelectedAnimation as anAnimationPrototype;
    inst := obj.GetInstance(obj.GetHolder, Editor.DragState.Transform, 0);
    inst.InterpolationMode := Editor.NewInterpolation;

    prot.AddObject(obj, InputBox('Object name', 'Enter object name', 'OBJ'));
    Editor.SyncAnimation(prot);
    Editor.RepaintTimeline;
  end;
end;

function TMainForm.SelectedSymbol: anSymbol;
var
  idx: TListItem;
begin
  idx := LibraryItems.Selected;
  if Assigned(idx) then
    exit(anSymbol(idx.Data));
  exit(nil);
end;

procedure TMainForm.RegisterSymbol(sym: anSymbol; Display: boolean);
var
  nw: TListItem;
begin
  nw := LibraryItems.Items.Add;
  with nw do begin
    Caption := sym.Name;
    Data := sym;
    ImageIndex := SYMBOL_ID;
  end;
  if Display then begin
    LibraryItems.Selected := nw;
    FillDrawList(sym, SLOT_SYMBOL, SLOT_SYMBOL_ID);
    MainForm.DeleteLibraryItemButton.Enabled := true;
    MainForm.CloneLibraryItemButton.Enabled := true;
  end;
end;

procedure TMainForm.UnRegisterSymbol(sym: anSymbol);
var
  idx: TListItem;
  i: Integer;
begin
  idx := LibraryItems.Items.FindData(sym);
  RemoveDrawList(sym, SLOT_SYMBOL);
  if Assigned(idx) then begin
    LibraryItems.Items.Delete(idx.Index);
    LibraryItems.Selected := nil;

    MainForm.DeleteLibraryItemButton.Enabled := false;
    MainForm.CloneLibraryItemButton.Enabled := false;
  end;
end;

procedure TMainForm.RegisterOtherObject(ObjName: AnsiString; ImageIdx: Integer;
  ObjClass: TOtherObjectRegistrationInfo);
begin
  with OtherComponents.Items.Add do begin
    Caption := ObjName;
    ImageIndex := ImageIdx;
    Data := ObjClass;
  end;
end;

function TMainForm.GetTextureImgId(tex: anTexture): integer;
var
  texId: Integer;
begin
  if tex.ClassType = anAnimatedSpriteTexture then
    texId := 5
  else if tex.ClassType = anAtlasTexture then
    texId := 6
  else if tex.ClassType = anFullTexture then
    texId := 4
  else
    texId := 7;
  result := texId;
end;

function TMainForm.GetAnimationImgId(tex: anAnimation): integer;
var
  texId: Integer;
begin
  if tex.ClassType = anAnimationPrototype then
    texId := SLOT_ANIMATION_ID
  else
    texId := SLOT_ANIMATION_BLENDER_ID;
  result := texId;
end;

procedure TMainForm.SaveAnimationToFile(FileName: AnsiString);
begin
  Editor.Project.SaveToFile(FileName);
  AddRecent(FileName, true);
  ChDir(file_GetDirectory(FileName));
  Caption := AppName;
end;

procedure TMainForm.LoadAnimationFromFile(FileName: AnsiString);
begin
  Clear;
  AddRecent(FileName, true);
  Editor.Project.LoadFromFile(FileName);
end;

procedure TMainForm.Clear;
begin
  LibraryItems.Clear;
  TexturesItems.Clear;
  AnimationsSelector.Items.Clear;
  Editor.Clear;
  ClearDrawList;
end;

function TMainForm.CheckForSave(Sender: TObject; Force: Boolean): Boolean;
var
  dlg: TModalResult;
begin
  if Editor.Project.EditorChanged or (Force and (not Editor.Project.Saved)) then begin
    dlg := MessageDlg('Save changes to project?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case dlg of
      mrYes: begin
        SaveItemClick(Sender);
        exit((not Editor.Project.Saved));
      end;
      mrNo: if Force then exit(true);
      mrCancel: exit(true);
    end;
  end;
  Result := false;
end;

end.

