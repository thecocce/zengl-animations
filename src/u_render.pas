unit u_render;

{$mode delphi}

interface

uses
  Forms, Classes, Graphics, ExtCtrls, SysUtils, zglHeader, u_animation,
  u_editorhistory, Math, Controls, Dialogs, u_db, crc, VirtualTrees, fgl;

const
  FRAME_SIZE = 14;
  FRAME_HEIGHT = 14;
  HEADER_HEIGHT = 24;
  FRAME_OFFSETX = 4;
  FRAMES_OFFSET = 5;

  TEX_SELECTED_SIZE = 16;

  BONE_LINE = 1;
  BONE_ENDS = 2;
  BONE_ALL = BONE_LINE or BONE_ENDS;

  CIRCUIT_COUNT = 9;
  CIRCUIT_SIZE = 3;

type
  TToolMode = (tmMove, tmScale, tmRotate, tmParent);
  TAtlasToolMode = (atmMove, atmScale, atmRotate, atmAddRect,
    atmAddShape, atmEditPoints, atmDeleteZone);
  TMouseState = (msClicked, msNone);
  TScaleMode = (scmWidth, scmHeight);
  TTransformResult = (resNone, resOk, resDblClick, resContext);
  TSelectionMode = (smNone, smStart, smDone);

  { TSelection }

  TSelection = class
    private
      fLeft, fRight: anPosition;
      fObjectTo: integer;
      function getLeft: anPosition;
      function getRight: anPosition;
    public
      property Left: anPosition read getLeft write fLeft;
      property Right: anPosition read getRight write fRight;
      property ObjectTo: Integer read fObjectTo write fObjectTo;
  end;

  TTagMode = (modeNormal, modeTags);

  { TPresetObject }

  TPresetObject = class
    private
      FTags: AnsiString;
    public
      property Tags: AnsiString read fTags write fTags;

      procedure SaveToRectord(rec: TDBRecord);
      procedure LoadFromRectord(rec: TDBRecord);

      constructor Create;
  end;

  { TPresetTag }

  TPresetTag = class
    private
      FVisible: Boolean;
      FTag: AnsiString;
    public
      property Visible: Boolean read FVisible write FVisible;
      property Tag: AnsiString read FTag write FTag;

      procedure SaveToRectord(rec: TDBRecord);
      procedure LoadFromRectord(rec: TDBRecord);

      constructor Create(pTag: AnsiString);
  end;

  TPresetObjectList = TFPGMap<AnsiString, TPresetObject>;
  TPresetTagsList = TFPGMap<AnsiString, TPresetTag>;

  { TAnimationPreset }

  TAnimationPreset = class
    private
      FFilter: AnsiString;
      FMode: TTagMode;
      fPresetList: TPresetObjectList;
      fPresetTagsList: TPresetTagsList;

      property PresetList: TPresetObjectList read fPresetList write fPresetList;
      property PresetTagsList: TPresetTagsList read fPresetTagsList write fPresetTagsList;
    public
      property Filter: AnsiString read fFilter write fFilter;
      property Mode: TTagMode read fMode write fMode;

      function GetObject(pName: AnsiString): TPresetObject;
      function GetTag(pName: AnsiString): TPresetTag;

      procedure SaveToRectord(rec: TDBRecord);
      procedure LoadFromRectord(rec: TDBRecord);

      constructor Create;
      destructor Destroy; override;
  end;

  TAnimationPresetList = TFPGMap<AnsiString, TAnimationPreset>;

  { TEditorProject }

  TEditorProject = class
    private
      fProjectFileName, fAnimationFileName: AnsiString;
      fAnimation: anAnimationSet;
      fPresets: TAnimationPresetList;
      fSaved, fStored, fEditorChanged: Boolean;
      property Presets: TAnimationPresetList read fPresets write fPresets;
    public
      property AnimationFileName: AnsiString read fAnimationFileName write fAnimationFileName;
      property ProjectFileName: AnsiString read fProjectFileName write fProjectFileName;
      property Animation: anAnimationSet read fAnimation write fAnimation;
      property Saved: Boolean read fSaved write fSaved;
      property _Stored: Boolean read fStored write fStored;
      property EditorChanged: Boolean read fEditorChanged write fEditorChanged;

      function GetPreset(pName: AnsiString): TAnimationPreset;

      procedure Import(FileName: AnsiString);
      procedure LoadFromFile(FileName: AnsiString);
      procedure InitAnimation;
      procedure SaveToFile(FileName: AnsiString);

      procedure Edit;
      procedure Generate;
      procedure Clear;

      constructor Create;
      destructor Destroy; override;
  end;


  TInitNewInstanceCallback = procedure (Obj: anAnimationLayerObjectInstance) of object;

  { TOtherObjectRegistrationInfo }

  TOtherObjectRegistrationInfo = class
    private
      fPropClass: anAnimationLayerObjectClass;
      fCallback: TInitNewInstanceCallback;
      fHelp: AnsiString;
    public
      property PropClass: anAnimationLayerObjectClass read fPropClass write fPropClass;
      property CallBack: TInitNewInstanceCallback read fCallBack write fCallBack;
      property Help: AnsiString read fHelp write fHelp;

      procedure Test(obj: anAnimationLayerObjectInstance);

      constructor Create(pPropClass: anAnimationLayerObjectClass;
          pCallBack: TInitNewInstanceCallback; pHelp: AnsiString);
  end;
  { TEditor }

  TEditor = class
    private

      tex_selected, tex_bone, tex_bone_end, tex_bone_anign: zglPTexture;
      Counter: Single;

      fProject: TEditorProject;
      fNewInterpolation: anInterpolationMode;
      fDrawData: TObject;
      fContextObject: TObject;
      fToolMode: TToolMode;
      fAtlasToolMode: TAtlasToolMode;
      fHistory: TEditorHistory;
      fScaleMode: TScaleMode;
      fActivated: Boolean;
      fSelectedObject: anAnimationKeyFrameInstance;
      fSelectionMode: TSelectionMode;
      fSelectedAnimation: anAnimation;
      fAnimInstance: anAnimationInstance;
      fDefaultTransformation: anTransformClass;
      fSelectedZone: anAtlasTextureZone;
      fSettings: TDataBase;
      fRecent: TDBRecord;
      fAlternative: Boolean;

      MainFont: zglPFont;
      function getScX: Single;
      function getScY: Single;
      procedure setDrawData(AValue: TObject);
      procedure setSelectedAnimation(AValue: anAnimation);
    public
      CAM: zglTCamera2D;
      RenderRect: zglTRect;
      CurrentZoom: Single;
      FirstPos: zglTPoint2D;
      Points: array of zglTPoint2D;
      OtherObjects: array of TOtherObjectRegistrationInfo;
      OtherObjectsCount: integer;
      CircuitTarget: zglPRenderTarget;

      DragState: record
        Drag: boolean;
        DropClass: (anDropSymbol, anDropAnimation, anDropObject);
        Symbol: anSymbol;
        Content: zglPTexture;
        Animation: anAnimationInstance;
        Pos: zglTPoint2D;
        Transform: anTransform;
      end;
      ObjectDragState: record
        OBJ: anAnimationLayerObject;
        Pos: TPoint;
      end;
      Selected: TSelection;
      DrawContentPointer: Pointer;

      procedure Draw;
      procedure DrawAnimation;
      procedure DrawCircuit;
      procedure DrawGrid;
      procedure DrawHud;
      procedure DrawSymbol;
      procedure DrawTexture;
      procedure DrawContent;
      procedure UpdateRenderTarget;

      procedure OnPointObjectCreate(Obj: anAnimationLayerObjectInstance);
      procedure OnPointLookUpObjectCreate(Obj: anAnimationLayerObjectInstance);
      procedure OnPointObjectDraw(Sender: anAnimationPointObject;
        Instance: anAnimationLayerPointInstance; Data: anDrawResult);
      procedure OnPointLookUpObjectDraw(Sender: anAnimationPointObject;
        Instance: anAnimationLayerPointInstance; Data: anDrawResult);

      procedure InitOtherInstances(OthCount: integer);
      procedure RegisterOtherInstance(ObjName, Help: AnsiString; ImageIdx: Integer;
        ObjClass: anAnimationLayerObjectClass; InitNewInstance: TInitNewInstanceCallback);

      procedure UpdateAnimationInstance;
      procedure DrawAtlasZone(Zone: anAtlasTextureZone; ForceSelection: Boolean = false);
      procedure SetAtlasZoneCP(Zone: anAtlasTextureZone; cp: zglTPoint2D);
      function UpdateAtlasZone(Zone: anAtlasTextureZone): integer;
      function AtlasZoneMiddlePoint(Zone: anAtlasTextureZone): zglTPoint2D;
      procedure DrawAtlasZones(atlas: anAtlasTexture);
      function UpdateAtlasZones(atlas: anAtlasTexture) : anAtlasTextureZone;

      procedure Clear;

      procedure Update(dt: Double);
      procedure UpdateAnimation(dt: Double);
      procedure UpdateAtlasTexture(dt: Double);
      procedure UpdateSymbol(dt: Double);

      function ClosestObject: anAnimationKeyFrameInstance;
      procedure DrawBones;
      procedure DrawBone(x0, y0, x1, y1: Single; DrawEnd: byte = BONE_ALL);

      procedure SelectFrame(Id: integer);
      procedure SelectDone(Id: integer);

      function UpdateNames(X, Layer: integer; Context: Boolean): Boolean;
      function ForceCreation(Holder: anAnimationLOInstanceHolder; Obj: anAnimationKeyFrameInstance;
        Time: Integer): anAnimationKeyFrameInstance;

      procedure RepaintTimeline;
      procedure SyncAnimation(an: anAnimation);

      procedure MultZoom(f: Single);
      procedure ZoomTo(f: Single);
      procedure ResetZoom;

      procedure RotatePoint(var Point: zglTPoint2D; Angle: Single);

      function DistanceLineToPoint(x, y: single; x0, y0, x1, y1: Single): Single;
      function DistanceToMouse(x, y: single): single;
      function TestZone(Zone: anAtlasTextureZone): Boolean;

      property scX: Single read getScX;
      property scY: Single read getScY;

      property Alternative: Boolean read fAlternative write fAlternative;
      property Project: TEditorProject read fProject write fProject;
      property Settings: TDataBase read fSettings write fSettings;
      property Recent: TDBRecord read fRecent write fRecent;
      property SelectedZone: anAtlasTextureZone read fSelectedZone write fSelectedZone;
      property Activated: Boolean read fActivated write fActivated;
      property NewInterpolation: anInterpolationMode read fNewInterpolation write fNewInterpolation;
      property ContextObject: TObject read fContextObject write fContextObject;
      property DrawData: TObject read fDrawData write setDrawData;
      property DefaultTransformation: anTransformClass read fDefaultTransformation write fDefaultTransformation;
      property ToolMode: TToolMode read fToolMode write fToolMode;
      property AtlasToolMode: TAtlasToolMode read fAtlasToolMode write fAtlasToolMode;
      property ScaleMode: TScaleMode read fScaleMode write fScaleMode;
      property History: TEditorHistory read fHistory write fHistory;
      property SelectedAnimation: anAnimation read fSelectedAnimation write setSelectedAnimation;
      property AnimInstance: anAnimationInstance read fAnimInstance write fAnimInstance;
      property SelectionMode: TSelectionMode read fSelectionMode write fSelectionMode;
      property SelectedObject: anAnimationKeyFrameInstance read fSelectedObject write fSelectedObject;

      procedure Init;

      constructor Create;
      destructor Destroy; override;
  end;

const
  FIXDOT_SIZE = 4;
  SCALEDOT_SIZE = 3;
  ROTDOT_SIZE = 6;

procedure Init;
procedure Draw;
procedure UpdateDT( dt : Double );

function mouse_clicked: boolean;
procedure mouse_clear(resetdbl: boolean = true);

var
  Editor: TEditor;
  zglInited : Boolean;
  pointsIndexes: array[0..3] of record first, second: byte; end = (
    (first: 0; second: 1),
    (first: 1; second: 2),
    (first: 2; second: 3),
    (first: 3; second: 0)
  );
  CircuitPoints: array[0..CIRCUIT_COUNT - 1] of record x, y: Integer; end = (
    (x: 0; y: 0),
    (x: 1; y: 0),
    (x: 0; y: 1),
    (x: -1; y: 0),
    (x: 0; y: -1),

    (x: 1; y: 1),
    (x: -1; y: 1),
    (x: 1; y: -1),
    (x: -1; y: -1)
  );
  mouse_state: record
    pos, clickpos, obj_pos: zglTPoint2D;
    obj_single, obj_scale: Single;
    obj_int: integer;
    rpos: TPoint;
    clicked: boolean;
    down, dblclick: boolean;
    button: TMouseButton;
    shift: TShiftState;
    state: TMouseState;
  end;

implementation

uses u_main, u_proj_form;

procedure Init;
begin
  Editor.Init;
end;

procedure UpdateDT( dt : Double );
begin
  Editor.Update(dt);
end;

{ TPresetTag }

procedure TPresetTag.SaveToRectord(rec: TDBRecord);
begin
  rec.ChildByName['visible'].AsBoolean := Visible;
end;

procedure TPresetTag.LoadFromRectord(rec: TDBRecord);
begin
  Visible := rec.ChildByName['visible'].AsBoolean;
end;

constructor TPresetTag.Create(pTag: AnsiString);
begin
  Tag := pTag;
  Visible := true;
end;

{ TPresetObject }

procedure TPresetObject.SaveToRectord(rec: TDBRecord);
begin
  rec.ChildByName['tags'].AsString := Tags;
end;

procedure TPresetObject.LoadFromRectord(rec: TDBRecord);
begin
  Tags := rec.ChildByName['tags'].AsString;
end;

constructor TPresetObject.Create;
begin
  Tags := '';
end;

{ TAnimationPreset }

function TAnimationPreset.GetObject(pName: AnsiString): TPresetObject;
var
  idx: LongInt;
begin
  idx := PresetList.IndexOf(pName);
  if idx >= 0 then begin
    Result := PresetList.Data[idx];
  end else begin
    Result := TPresetObject.Create;
    PresetList.Add(pName, Result);
  end;
end;

function TAnimationPreset.GetTag(pName: AnsiString): TPresetTag;
var
  idx: LongInt;
begin
  idx := PresetTagsList.IndexOf(pName);
  if idx >= 0 then begin
    Result := PresetTagsList.Data[idx];
  end else begin
    Result := TPresetTag.Create(pName);
    PresetTagsList.Add(pName, Result);
  end;
end;

procedure TAnimationPreset.SaveToRectord(rec: TDBRecord);
var
  objects: TDBRecord;
  i: Integer;
begin
  rec.ChildByName['filter'].AsString := Filter;
  rec.ChildByName['mode'].AsByte := Byte(Mode);

  objects := rec.ChildByName['list'];
  objects.ChildCount := PresetList.Count;
  for i := 0 to PresetList.Count - 1 do begin
    objects.Child[i].Name := PresetList.Keys[i];
    PresetList.Data[i].SaveToRectord(objects.Child[i]);
  end;

  objects := rec.ChildByName['tags'];
  objects.ChildCount := PresetTagsList.Count;
  for i := 0 to PresetTagsList.Count - 1 do begin
    objects.Child[i].Name := PresetTagsList.Keys[i];
    PresetTagsList.Data[i].SaveToRectord(objects.Child[i]);
  end;
end;

procedure TAnimationPreset.LoadFromRectord(rec: TDBRecord);
var
  objects: TDBRecord;
  obj: TPresetObject;
  i: Integer;
  tag_obj: TPresetTag;
begin
  Filter := rec.ChildByName['filter'].AsString;
  Mode := TTagMode(rec.ChildByName['mode'].AsByte);

  objects := rec.ChildByName['list'];
  for i := 0 to objects.ChildCount - 1 do begin
    obj := GetObject(objects.Child[i].Name);
    obj.LoadFromRectord(objects.Child[i]);
  end;

  objects := rec.ChildByName['tags'];
  for i := 0 to objects.ChildCount - 1 do begin
    tag_obj := GetTag(objects.Child[i].Name);
    tag_obj.LoadFromRectord(objects.Child[i]);
  end;
end;

constructor TAnimationPreset.Create;
begin
  Filter := '';
  Mode := modeNormal;
  PresetList := TPresetObjectList.Create;
  PresetTagsList := TPresetTagsList.Create;
end;

destructor TAnimationPreset.Destroy;
var
  i: Integer;
begin
  for i := 0 to PresetList.Count - 1 do begin
    PresetList.Data[i].Free;
  end;
  PresetList.Free;

  for i := 0 to PresetTagsList.Count - 1 do begin
    PresetTagsList.Data[i].Free;
  end;
  PresetTagsList.Free;

  inherited Destroy;
end;

{ TOtherObjectRegistrationInfo }

procedure TOtherObjectRegistrationInfo.Test(obj: anAnimationLayerObjectInstance
  );
begin
  if (obj.ObjectTo.ClassType = PropClass) and Assigned(CallBack) then
    CallBack(obj);
end;

constructor TOtherObjectRegistrationInfo.Create(
  pPropClass: anAnimationLayerObjectClass; pCallBack: TInitNewInstanceCallback;
  pHelp: AnsiString);
begin
  PropClass := pPropClass;
  CallBack := pCallBack;
  Help := pHelp;
end;

{ TEditorProject }

function TEditorProject.GetPreset(pName: AnsiString): TAnimationPreset;
var
  idx: LongInt;
begin
  idx := Presets.IndexOf(pName);
  if idx >= 0 then begin
    Result := Presets.Data[idx];
  end else begin
    Result := TAnimationPreset.Create;
    Presets.Add(pName, Result);
  end;
end;

procedure TEditorProject.Import(FileName: AnsiString);
begin
  Animation.Clear;
  Animation.LoadFromFile(FileName, TFileHandler);
  InitAnimation;
  if AnimationFileName = '' then
    AnimationFileName := ExtractRelativepath(ProjectFileName, FileName);
end;

procedure TEditorProject.LoadFromFile(FileName: AnsiString);
var
  db: TDataBase;
  presets_db: TDBRecord;
  preset: TAnimationPreset;
  i: Integer;
begin
  Clear;
  db := TDataBase.Create(TFileHandler.Create(FileName), 'ROOT');
  if db.Loaded then begin
    with db.Root do begin
      AnimationFileName := ChildByName['animation_name'].AsString;
      Animation.Clear;
      if ChildExists['animation_data'] then begin
        Animation.LoadFromRecord(ChildByName['animation_data']);
        presets_db := ChildByName['presets'];
        for i := 0 to presets_db.ChildCount - 1 do begin
          preset := GetPreset(presets_db.Child[i].Name);
          preset.LoadFromRectord(presets_db.Child[i]);
        end;
      end;
    end;
  end;
  db.Free;
  ProjectFileName := FileName;
  InitAnimation;
  Saved := true;
  EditorChanged := false;
  _Stored := false;
end;

procedure TEditorProject.InitAnimation;
var i: Integer;
    tex: anTexture;
    sym: anSymbol;
    anim: anAnimation;
    cnt: anTextureContent;
begin
  ChDir(file_GetDirectory(file_GetDirectory(ProjectFileName) + '/' + AnimationFileName));
  for i := 0 to Animation.Contents.Count - 1 do begin
    cnt := Animation.Contents.Data[i];
    MainForm.RegisterContent(cnt);
  end;
  for i := 0 to Animation.Textures.Count - 1 do begin
    tex := Animation.Textures.Data[i];
    MainForm.RegisterTexture(tex, MainForm.GetTextureImgId(tex));
  end;
  for i := 0 to Animation.AnimationLibrary.Symbols.Count - 1 do begin
    sym := Animation.AnimationLibrary.Symbols.Data[i];
    MainForm.RegisterSymbol(sym);
  end;
  for i := 0 to Animation.Animations.Count - 1 do begin
    anim := Animation.Animations.Data[i];
    MainForm.RegisterAnimation(anim, anim.Prototype);
  end;
end;

procedure TEditorProject.SaveToFile(FileName: AnsiString);
var db: TDataBase;
  presets_db: TDBRecord;
  i: Integer;
begin
  if AnimationFileName = '' then begin
    AnimationFileName := file_GetName(FileName) + '.adb';
  end;
  db := TDataBase.Create(TFileHandler.Create(FileName), 'ROOT', false);
  with db.Root do begin
    ChildByName['animation_name'].AsString := AnimationFileName;
    Animation.SaveToRecord(ChildByName['animation_data']);
    presets_db := ChildByName['presets'];
    presets_db.ChildCount := Presets.Count;
    for i := 0 to Presets.Count - 1 do begin
      presets_db.Child[i].Name := Presets.Keys[i];
      Presets.Data[i].SaveToRectord(presets_db.Child[i]);
    end;
  end;
  db.Save;
  db.Free;
  Saved := true;
  _Stored := true;
  EditorChanged := false;
  ProjectFileName := FileName;
end;

procedure TEditorProject.Edit;
begin
  ProjectForm.Init(Self);
  ProjectForm.ShowModal;
end;

procedure TEditorProject.Generate;
begin
  if AnimationFileName <> '' then begin
    Animation.SaveToFile(AnimationFileName, TFileHandler);
    ShowMessage('Animation successfully published at "' + AnimationFileName + '"!');
  end else
    ShowMessage('Cannot publish animation: project not saved!');
end;

procedure TEditorProject.Clear;    var i: integer;
begin
  for i := 0 to Presets.Count - 1 do begin
    Presets.Data[i].Free;
  end;
  Presets.Clear;
  Animation.Clear;
end;

constructor TEditorProject.Create;
begin
  Saved := false;
  _Stored := false;
  EditorChanged := false;
  ProjectFileName := '';
  Animation := anAnimationSet.Create;
  Presets := TAnimationPresetList.Create;
end;

destructor TEditorProject.Destroy;
begin
  Clear;
  Presets.Free;
  inherited Destroy;
end;

{ TSelection }

function TSelection.getLeft: anPosition;
begin
  if Editor.SelectionMode in [smStart, smDone] then
    Result := fLeft
  else
    Result := Editor.AnimInstance.CurrentFrame;
end;

function TSelection.getRight: anPosition;
begin
  if Editor.SelectionMode in [smStart, smDone] then
    Result := fRight
  else
    Result := Editor.AnimInstance.CurrentFrame;
end;

procedure TEditor.RotatePoint(var Point: zglTPoint2D; Angle: Single);
var dist, angl: Double;
begin
  dist := m_Distance(Point.X, Point.Y, 0, 0);
  angl := m_Angle(Point.X, Point.Y, 0, 0) + Angle;
  Point.X := Cos(deg2rad * angl) * dist;
  Point.Y := Sin(deg2rad * angl) * dist;
end;

function TEditor.DistanceLineToPoint(x, y: single; x0, y0, x1, y1: Single
  ): Single;
begin
  Result := abs(
    ((y0 - y1) * x + (x1 - x0) * y + (x0 * y1 - x1 * y0))
      / m_Distance(x0, y0, x1, y1)
    );
end;

function TEditor.DistanceToMouse(x, y: single): single;
begin
  Result := m_Distance(mouse_state.pos.X, mouse_state.pos.Y, x, y);
end;

function TEditor.TestZone(Zone: anAtlasTextureZone): Boolean;
var i, j: Integer;
begin
  Result := False;
  j := Zone.Points.Count - 1;
  for i := 0 to Zone.Points.Count - 1 do begin
    if ((Zone.Points[i]^.Y > mouse_state.pos.Y - Zone.Y) <> (Zone.Points[j]^.Y > mouse_state.pos.Y - Zone.Y)) and
      (mouse_state.pos.X - Zone.X < (Zone.Points[j]^.X - Zone.Points[i]^.X) * (mouse_state.pos.Y - Zone.Y - Zone.Points[i]^.Y) /
      (Zone.Points[j]^.Y - Zone.Points[i]^.Y) + Zone.Points[i]^.X) then
      Result := not Result;
    j := i;
  end;
end;

function mouse_clicked: boolean;
begin
  result := mouse_state.clicked;
end;

procedure mouse_clear(resetdbl: boolean);
begin
  mouse_state.clicked := false;
  if (resetdbl) then
     mouse_state.dblclick := false;
end;

procedure Draw;
begin
  Editor.Draw;
end;

{ TEditor }

procedure TEditor.setSelectedAnimation(AValue: anAnimation);
begin
  if fSelectedAnimation = AValue then Exit;
  fSelectedAnimation := AValue;
  MainForm.TimeLinePanel.Visible := AValue <> nil;
  DrawData := AValue;
  UpdateAnimationInstance;
end;

procedure TEditor.UpdateRenderTarget;
begin
  if Assigned(CircuitTarget) then begin
    rtarget_Del(CircuitTarget);
    CircuitTarget := nil;
  end;
  if MainForm.DrawHightLightTool.Down then
    CircuitTarget := rtarget_Add(tex_CreateZero(MainForm.DrawPanel.ClientWidth, MainForm.DrawPanel.ClientHeight),
       RT_DEFAULT or RT_CLEAR_COLOR);
end;

function TEditor.getScX: Single;
begin
  Result := MAX(MainForm.DrawPanel.ClientWidth / CAM.Zoom.X, MainForm.DrawPanel.ClientWidth);
end;

function TEditor.getScY: Single;
begin
  Result := MAX(MainForm.DrawPanel.ClientHeight / CAM.Zoom.Y, MainForm.DrawPanel.ClientHeight);
end;

procedure TEditor.setDrawData(AValue: TObject);
var
  sym: anSymbol;
begin
  if fDrawData = AValue then Exit;
  if Assigned(fDrawData) then begin
    if fDrawData is anSymbol then begin
      sym := anSymbol(fDrawData);
      if Assigned(sym.TextureParams) then begin
        sym.TextureParams.ReleaseTexture(DrawContentPointer);
        DrawContentPointer := nil;
      end;
    end;
  end;
  fDrawData := AValue;
  if DrawData is anSymbol then begin
    sym := anSymbol(DrawData);
    if Assigned(sym.TextureParams) then begin
      DrawContentPointer := sym.TextureParams.GetTexture;
    end;
  end;
end;

procedure TEditor.Draw;
begin
  pr2d_Rect( 0, 0, MainForm.DrawPanel.ClientWidth, MainForm.DrawPanel.ClientHeight,
    $FFFFFF, 255, PR2D_FILL);

  CAM.Zoom.X := RenderRect.W / MainForm.DrawPanel.ClientWidth;
  CAM.Zoom.Y := RenderRect.H / MainForm.DrawPanel.ClientHeight;

  CAM.X := (- RenderRect.X - RenderRect.W / 2) / CAM.Zoom.X;
  CAM.Y := (- RenderRect.Y - RenderRect.H / 2) / CAM.Zoom.Y;

  DrawCircuit;

  cam2d_Set(@CAM);

  DrawGrid;

  if Assigned(DrawData) then begin
    if DrawData is anAnimationInstance then begin
      anAnimationInstance(DrawData).Draw;
    end else
    if DrawData is anAnimation then begin
      DrawAnimation;
    end else
    if DrawData is anSymbol then begin
      DrawSymbol;
    end else
    if DrawData.InheritsFrom(anTextureContent) then begin
      DrawContent;
    end else
    if DrawData.InheritsFrom(anTexture) then begin
      DrawTexture;
    end;
  end;

  cam2d_Set(nil);

  pr2d_Rect(
    RenderRect.X, RenderRect.Y,
    RenderRect.W, RenderRect.H, $FF0000);

  DrawHud;

  Application.ProcessMessages();
  Sleep(1);
end;

procedure TEditor.DrawAnimation;
var sym: anSymbol;
    trnsfrm, trnsfrm_to: anTransform;
    min_dist_obj, inst: anAnimationKeyFrameInstance;
    obj: anAnimationLOInstanceHolder;
    sz: Extended;
    angle: Single;
    angle_offcet: Single;
    rnd_text: UTF8String;
    anim: anAnimationInstance;
    obj_parent: anAnimationLOInstanceHolder;
begin
  if Assigned(SelectedAnimation) then begin
    if not Assigned(AnimInstance) then exit;

    AnimInstance.Draw;


    if Editor.SelectedAnimation.ObjectsCount > 0 then begin

      if Assigned(CircuitTarget) then begin
        cam2d_Set(nil);
        ssprite2d_Draw(CircuitTarget^.Surface,
          0, 0,
          MainForm.DrawPanel.ClientWidth,
          MainForm.DrawPanel.ClientHeight,
          0);
        cam2d_Set(@CAM);
      end;

      if Mainform.DrawBonesTool.Down then begin
        DrawBones;
      end;

      if Assigned(SelectedObject) then begin
        obj := Editor.SelectedAnimation.HolderByIndex[SelectedObject.ObjectTo.Index];
        inst := obj.InstanceAt[AnimInstance.CurrentFrame];
        if Assigned(inst) then begin
          trnsfrm := SelectedObject.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame + AnimInstance.OffsetTime);

          asprite2d_Draw(tex_selected,
            trnsfrm.Position.X - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
            trnsfrm.Position.Y - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
            TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
            TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
            0, Trunc(Counter) + Byte(ToolMode) * 2);
          if Activated then begin
            case ToolMode of
              tmRotate: begin
                if Alternative then begin
                  sz := 64 / Editor.CAM.Zoom.X;
                  pr2d_Circle(
                    trnsfrm.Position.X, trnsfrm.Position.Y, sz,
                    0, 200, 32);
                  angle := mouse_state.obj_single + 180 + SelectedObject.Rounds * 22.5;


                  if SelectedObject.Rounds > 0 then begin
                    angle_offcet := angle + 270;
                    rnd_text := u_IntToStr(SelectedObject.Rounds) + ' round(s)';
                  end else if SelectedObject.Rounds < 0 then begin
                    angle_offcet := angle + 90;
                    rnd_text := u_IntToStr(-SelectedObject.Rounds) + ' round(s)';
                  end else begin
                    angle_offcet := angle + 180;
                    rnd_text := 'do not rotate';
                  end;

                  ssprite2d_Draw(
                    tex_bone_anign,
                    trnsfrm.Position.X
                      + cos(deg2rad * angle) * sz
                      - 8 / Editor.CAM.Zoom.X,
                    trnsfrm.Position.Y
                      + sin(deg2rad * angle) * sz
                      - 8 / Editor.CAM.Zoom.X,
                    16 / Editor.CAM.Zoom.X,
                    16 / Editor.CAM.Zoom.X,
                    round(angle_offcet)
                  );
                  text_DrawEx(MainFont,
                    trnsfrm.Position.X, trnsfrm.Position.Y - 32 / Editor.CAM.Zoom.X,
                    1 / Editor.CAM.Zoom.X , 0,
                    rnd_text,
                    255, $3333FF, TEXT_VALIGN_TOP or TEXT_HALIGN_CENTER);
                end;
              end;
              tmParent: begin
                min_dist_obj := ClosestObject;
                trnsfrm := SelectedObject.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
                if min_dist_obj <> SelectedObject then begin
                  obj_parent := Editor.SelectedAnimation.HolderByIndex[min_dist_obj.ObjectTo.Index];
                  trnsfrm_to := min_dist_obj.ObjectTo.GetTransform(obj_parent, AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
                  DrawBone(
                    trnsfrm.Position.X,
                    trnsfrm.Position.Y,
                    trnsfrm_to.Position.X,
                    trnsfrm_to.Position.Y
                  );
                end else begin
                  DrawBone(
                    trnsfrm.Position.X,
                    trnsfrm.Position.Y,
                    mouse_state.pos.X,
                    mouse_state.pos.Y
                  );
                end;
              end;
            end;
          end;
        end;
      end else begin
        min_dist_obj := ClosestObject;

        if Assigned(min_dist_obj) then begin
          obj := Editor.SelectedAnimation.HolderByIndex[min_dist_obj.ObjectTo.Index];
          trnsfrm := min_dist_obj.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
          asprite2d_Draw(tex_selected,
            trnsfrm.Position.X - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
            trnsfrm.Position.Y - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
            TEX_SELECTED_SIZE / Editor.CAM.Zoom.X, TEX_SELECTED_SIZE / Editor.CAM.Zoom.X, 0, Trunc(Counter) + 8);
        end;
      end;
    end;
  end;
  if DragState.Drag then begin
    if DragState.DropClass = anDropSymbol then begin
      sym := DragState.Symbol;
      if Assigned(sym.TextureParams) then begin
        with trnsfrm do begin
          Position.X := DragState.Pos.X;
          Position.Y := DragState.Pos.Y;
          Scale := 1;
          Rotation := 0;
          Transparency := 255;
        end;
        if not Assigned(DragState.Content) then begin
          DragState.Content := sym.TextureParams.GetTexture;
        end;
        sym.Draw(trnsfrm, sym.TextureParams.FormatTexture(DragState.Content),  nil);
      end;
    end;
    if DragState.DropClass = anDropAnimation then begin
      anim := DragState.Animation;
      with anim.Transform do begin
        PositionX := DragState.Pos.X;
        PositionY := DragState.Pos.Y;
        Scale := 1;
        Rotation := 0;
      end;
      anim.Draw;
    end;
    if DragState.DropClass = anDropObject then begin
      pr2d_Circle(DragState.Pos.X, DragState.Pos.Y,
        32 / CAM.Zoom.X, 0);
    end;
  end;
end;

procedure TEditor.DrawCircuit;
var
  sot: anAnimationKeyFrameInstance;
  obj: anAnimationLOInstanceHolder;
  trnsfrm: anTransform;
  ainst: anAnimationLayerObjectInstance;
  i: Integer;
  offX: Extended;
  offY: Extended;
begin
  if not Assigned(Editor.SelectedAnimation) then exit;
  if Editor.SelectedAnimation.ObjectsCount > 0 then begin
     if Assigned(AnimInstance) and (MainForm.DrawHightLightTool.Down) then begin
      if Assigned(SelectedObject) then
        sot := SelectedObject
      else
        sot := ClosestObject;

       if Assigned(sot) then begin

         obj := Editor.SelectedAnimation.HolderByIndex[sot.ObjectTo.Index];
         trnsfrm := sot.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame + AnimInstance.OffsetTime);

        rtarget_Set(CircuitTarget);

        cam2d_Set(@CAM);

        // color by hash from name

        fx2d_SetColor(crc32(crc32(0, nil, 0), @sot.ObjectTo.Name[1], length(sot.ObjectTo.Name)));
        fx_SetBlendMode(FX_BLEND_NORMAL);
        fx_SetColorMode(FX_COLOR_SET);
        ainst := AnimInstance.GetInstanceObjectByLO(sot.ObjectTo);
        if Assigned(ainst) then begin
          for i := 0 to CIRCUIT_COUNT - 1 do begin
            offX := CircuitPoints[i].X * Min(CIRCUIT_SIZE / CAM.Zoom.X, 6);
            offY := CircuitPoints[i].Y * Min(CIRCUIT_SIZE / CAM.Zoom.X, 6);
            ainst.Transform := anTransformClass.ApplyTransform(
              anTransformClass.Get(offX, offY), trnsfrm);
            ainst.Update(0);
            ainst.Draw(ainst.Transform,
              AnimInstance.CurrentFrame + AnimInstance.OffsetTime,
              FX_BLEND or FX_COLOR);
          end;

          fx2d_SetColor(0);
          fx_SetBlendMode(FX_BLEND_NORMAL);
          fx_SetColorMode(FX_COLOR_MIX);

          ainst.Transform := trnsfrm;
          ainst.Update(0);
          ainst.Draw(trnsfrm,
            AnimInstance.CurrentFrame + AnimInstance.OffsetTime);

        end;

        AnimInstance.Update(0);

        cam2d_Set(nil);

        rtarget_Set(nil);
      end;
    end;
  end;
end;

procedure TEditor.DrawGrid;
var GridSize: Integer;
    i, color, cnt: Integer;
begin
  GridSize := MainForm.GridSize;
  if MainForm.GridTool.Down and (GridSize * CAM.Zoom.X > 2) then begin
    cnt := round(scX / GridSize);
    for i := -cnt to cnt do begin
      if i = 0 then
        color := $666666
      else if i mod 10 = 0 then
        color := $999999
      else
        color := $CCCCCC;
      pr2d_line(
         i * GridSize, -scY, i * GridSize, scY, color);
    end;
    cnt := round(scY / GridSize);
    for i := -cnt to cnt do begin
      if i = 0 then
        color := $666666
      else if i mod 10 = 0 then
        color := $999999
      else
        color := $CCCCCC;
      pr2d_line(
         -scX, i * GridSize, scX, i * GridSize, color);
    end;
  end;
end;

procedure TEditor.DrawHud;
var off, tex_w, tex_h: Single;
    GridSize, cnt, i, div_mod: Integer;
    text: String;
    obj: anAnimationKeyFrameInstance;
begin
  GridSize := MainForm.GridSize;

  if MainForm.RuleTool.Checked and (GridSize * CAM.Zoom.X > 2) then begin
    if GridSize * CAM.Zoom.X > 5 then
      div_mod := 10
    else
      div_mod := 20;

    CAM.X := (- RenderRect.X - RenderRect.W / 2) / CAM.Zoom.X;
    CAM.Y := 0;

    cam2d_Set(@CAM);
    cnt := Round(scX / GridSize);
    for i := -cnt to cnt do begin
      if i mod div_mod = 0 then begin
        text_DrawEx(MainFont,
          i * GridSize, 1,  1 / Editor.CAM.Zoom.X , 0,
          u_IntToStr(i * GridSize), 255, $FFFFFF, TEXT_VALIGN_TOP or TEXT_HALIGN_CENTER);
      end;
    end;
    cam2d_Set(nil);

    CAM.X := 0;
    CAM.Y := (- RenderRect.Y - RenderRect.H / 2) / CAM.Zoom.Y;

    cam2d_Set(@CAM);
    cnt := Round(scY / GridSize);
    for i := -cnt to cnt do begin
      if i mod div_mod = 0 then begin
        text_DrawEx(MainFont,
          1, i * GridSize,  1 / Editor.CAM.Zoom.Y , 0,
          u_IntToStr(i * GridSize), 255, $FFFFFF, TEXT_VALIGN_CENTER or TEXT_HALIGN_LEFT);
      end;
    end;
    cam2d_Set(nil);

  end;


  if Assigned(SelectedObject) then
    obj := SelectedObject
  else
    obj := ClosestObject;
  if Assigned(obj) then begin
    text := obj.ObjectTo.Name;
    tex_w := text_GetWidth(MainFont, text);
    tex_h := text_GetHeight(MainFont, tex_w, text);
    pr2d_Rect( 10, 10, tex_w + 4, tex_h + 4, $0, 128, PR2D_FILL);
    text_DrawEx(MainFont,
      12, 12,  1 , 0,
      text, 255, $33FF33, TEXT_VALIGN_TOP or TEXT_HALIGN_LEFT);
  end;

  if DrawData is anSymbol then begin
    text := ' - X flip axis';
    tex_w := text_GetWidth(MainFont, text);
    tex_h := text_GetHeight(MainFont, tex_w, text);
    text_DrawEx(MainFont,
      12, 28,  1 , 0,
      text, 255, $FF3333, TEXT_VALIGN_BOTTOM or TEXT_HALIGN_LEFT);

    text := ' - Y flip axis';
    tex_w := text_GetWidth(MainFont, text);
    tex_h := text_GetHeight(MainFont, tex_w, text);
    text_DrawEx(MainFont,
      12, 32,  1 , 0,
      text, 255, $33FF33, TEXT_VALIGN_TOP or TEXT_HALIGN_LEFT);
  end;


end;

procedure TEditor.DrawSymbol;
var sym: anSymbol;
    trnsfrm: anTransform;
begin
  sym := anSymbol(DrawData);
  if Assigned(sym) then begin
    if Assigned(sym.TextureParams) then begin
      trnsfrm := anTransformClass.NewTransform;
      with trnsfrm do begin
        Position.X := sym.PivotX;
        Position.Y := sym.PivotY;
        Rotation := sym.Rotation;
      end;
      if not Assigned(DrawContentPointer) and (sym.TextureParams.HaveTexture) then begin
        DrawContentPointer := sym.TextureParams.GetTexture;
      end;
      sym.Draw(trnsfrm, sym.TextureParams.FormatTexture(DrawContentPointer), nil);

      pr2d_line(
        sym.PivotX - ScX - 0.5,
        sym.PivotY - 0.5,
        sym.PivotX + ScX - 0.5,
        sym.PivotY - 0.5,
      0);

      pr2d_line(
        sym.PivotX - 0.5,
        sym.PivotY - ScY - 0.5,
        sym.PivotX - 0.5,
        sym.PivotY + ScY - 0.5,
      0);

      pr2d_line(
        sym.PivotX - 0.5,
        sym.PivotY - 0.5,
        sym.PivotX + cos(deg2rad * sym.Rotation) * ScX - 0.5,
        sym.PivotY + sin(deg2rad * sym.Rotation) * ScX - 0.5,
      $FF3333);

      pr2d_line(
        sym.PivotX - 0.5,
        sym.PivotY - 0.5,
        sym.PivotX + cos(deg2rad * (sym.Rotation + 90)) * ScX - 0.5,
        sym.PivotY + sin(deg2rad * (sym.Rotation + 90)) * ScX - 0.5,
      $33FF33);

       asprite2d_Draw(tex_selected,
                sym.PivotX - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
                sym.PivotY - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
                TEX_SELECTED_SIZE / Editor.CAM.Zoom.X, TEX_SELECTED_SIZE / Editor.CAM.Zoom.X, 0, Trunc(Counter) + 12);
    end
  end;
end;

procedure TEditor.DrawTexture;
var tex: anTexture;
    i: Integer;
    atex: anAtlasTexture;
begin
  tex := anTexture(DrawData);
  if Assigned(tex) then begin

    tex.Draw(0, 0, 255, nil);
      pr2d_line(
        - scX,
        - tex.Height / 2,
        scX,
        - tex.Height / 2,
      0);
      pr2d_line(
        - scX,
        tex.Height / 2,
        scX,
        tex.Height / 2,
      0);
      pr2d_line(
        tex.Width / 2,
        -scY,
        tex.Width / 2,
        scY,
      0);
      pr2d_line(
        -tex.Width / 2,
        -scY,
        -tex.Width / 2,
        scY,
      0);
  end;
  if DrawData is anAtlasTexture then begin
    atex := anAtlasTexture(DrawData);
    for i := 0 to atex.ZonesCount - 1 do begin
      DrawAtlasZone(atex.Zones.Data[i]);
    end;
    if Activated then begin
      case AtlasToolMode of
        atmAddRect: begin
          pr2d_Rect(
            mouse_state.clickpos.X,
            mouse_state.clickpos.Y,
            mouse_state.pos.X - mouse_state.clickpos.X,
            mouse_state.pos.Y - mouse_state.clickpos.Y,
          0);
        end;
      end;
    end else begin
      case AtlasToolMode of
        atmMove, atmScale, atmRotate: begin
          DrawAtlasZones(atex);
        end;
      end;
    end;
  end;
end;

procedure TEditor.DrawContent;
var tex: anTextureContent;
    ptex: zglPTexture;
    i: Integer;
begin
  tex := anTextureContent(DrawData);
  if Assigned(tex) then begin
    ptex := tex.TexData;
    if Assigned(ptex) then begin
      ssprite2d_Draw(ptex,
        - ptex^.Width / 2, - ptex^.Height / 2,
        ptex^.Width, ptex^.Height,
        0);

      pr2d_line(
        - scX,
        - ptex^.Height / 2,
        scX,
        - ptex^.Height / 2,
      0);
      pr2d_line(
        - scX,
        ptex^.Height / 2,
        scX,
        ptex^.Height / 2,
      0);
      pr2d_line(
        ptex^.Width / 2,
        -scY,
        ptex^.Width / 2,
        scY,
      0);
      pr2d_line(
        -ptex^.Width / 2,
        -scY,
        -ptex^.Width / 2,
        scY,
      0);

    end;
  end;
end;

procedure TEditor.OnPointObjectCreate(Obj: anAnimationLayerObjectInstance);
var
  ObjTo: anAnimationPointObject;
begin
  ObjTo := anAnimationPointObject(Obj.ObjectTo);
  ObjTo.OnDraw := OnPointObjectDraw;
end;

procedure TEditor.OnPointLookUpObjectCreate(Obj: anAnimationLayerObjectInstance);
var
  ObjTo: anAnimationLookUpPointObject;
begin
  ObjTo := anAnimationLookUpPointObject(Obj.ObjectTo);
  ObjTo.OnDraw := OnPointLookUpObjectDraw;
end;

procedure TEditor.OnPointObjectDraw(Sender: anAnimationPointObject;
  Instance: anAnimationLayerPointInstance; Data: anDrawResult);
begin
  asprite2d_Draw(tex_selected,
    Data.Transform.Position.X - (TEX_SELECTED_SIZE) / Editor.CAM.Zoom.X,
    Data.Transform.Position.Y - (TEX_SELECTED_SIZE) / Editor.CAM.Zoom.X,
    2 * TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
    2 * TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
    Data.Transform.Rotation, 19);
end;

procedure TEditor.OnPointLookUpObjectDraw(Sender: anAnimationPointObject;
  Instance: anAnimationLayerPointInstance; Data: anDrawResult);
begin
  asprite2d_Draw(tex_selected,
    Data.Transform.Position.X - (TEX_SELECTED_SIZE) / Editor.CAM.Zoom.X,
    Data.Transform.Position.Y - (TEX_SELECTED_SIZE) / Editor.CAM.Zoom.X,
    2 * TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
    2 * TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
    Data.Transform.Rotation, 19);
end;

procedure TEditor.InitOtherInstances(OthCount: integer);
begin
  SetLength(OtherObjects, OthCount);
  OtherObjectsCount := 0;
end;

procedure TEditor.RegisterOtherInstance(ObjName, Help: AnsiString;
  ImageIdx: Integer; ObjClass: anAnimationLayerObjectClass;
  InitNewInstance: TInitNewInstanceCallback);
var
  OthObj: TOtherObjectRegistrationInfo;
begin
  OthObj := TOtherObjectRegistrationInfo.Create(ObjClass, InitNewInstance, Help);
  OtherObjects[OtherObjectsCount] := OthObj;
  inc(OtherObjectsCount);
  MainForm.RegisterOtherObject(ObjName, ImageIdx, OthObj);
end;

procedure TEditor.UpdateAnimationInstance;
var
  Frame: anPosition;
  i, j: Integer;
begin
  ContextObject := nil;
  if Assigned(AnimInstance) then begin
    Frame := AnimInstance.CurrentFrame;
    AnimInstance.Free;
  end else
    Frame := 0;
  AnimInstance := nil;
  if Assigned(SelectedAnimation) then begin
    AnimInstance := SelectedAnimation.GetInstance;
    AnimInstance.TimeLine.CurrentFrame := Frame;
    for i := 0 to AnimInstance.InstancesCount - 1 do begin
      for j := 0 to OtherObjectsCount - 1 do begin
        OtherObjects[j].Test(AnimInstance.Instances[i]);
      end;
    end;
    SyncAnimation(SelectedAnimation);
    RepaintTimeline;
  end;
end;

procedure TEditor.DrawAtlasZone
  (Zone: anAtlasTextureZone; ForceSelection: Boolean);
var i: integer;
    mp: zglTPoint2D;

    function DrawAtlasPoint(pX, pY: Single; Offset: Byte = 15): Boolean;
    var b: Boolean;
    begin
      b := (DistanceToMouse(Zone.X + pX, Zone.Y + pY) < 6 / Editor.CAM.Zoom.X) or ForceSelection;
      asprite2d_Draw(tex_selected,
        Zone.X + pX - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
        Zone.Y + pY - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
        TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
        TEX_SELECTED_SIZE / Editor.CAM.Zoom.X,
        0, Byte(b) + Offset);
      Result := b;
    end;

begin

  asprite2d_Draw(tex_selected,
     Zone.X - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
     Zone.Y - (TEX_SELECTED_SIZE / 2) / Editor.CAM.Zoom.X,
     TEX_SELECTED_SIZE / Editor.CAM.Zoom.X, TEX_SELECTED_SIZE / Editor.CAM.Zoom.X, 0, Trunc(Counter) + 12);

  if Zone.Points.Count <= 1 then exit;

  if MainForm.AtlasDrawZoneNames.Down then begin;
    mp := AtlasZoneMiddlePoint(Zone);

    text_DrawEx(MainFont,
      trunc(mp.x), trunc(mp.Y),  1 / Editor.CAM.Zoom.X , 0,
      Zone.Name, 255, $FFFFFF, TEXT_VALIGN_CENTER or TEXT_HALIGN_CENTER);
  end;

  for i := 0 to Zone.Points.Count - 2 do
    pr2d_Line(
      Zone.X + Zone.Points[i]^.X - 0.5,
      Zone.Y + Zone.Points[i]^.Y - 0.5,
      Zone.X + Zone.Points[i + 1]^.X - 0.5,
      Zone.Y + Zone.Points[i + 1]^.Y - 0.5,
      $0A254A
    );
  pr2d_Line(
    Zone.X + Zone.Points[0]^.X - 0.5,
    Zone.Y + Zone.Points[0]^.Y - 0.5,
    Zone.X + Zone.Points[Zone.Points.Count - 1]^.X - 0.5,
    Zone.Y + Zone.Points[Zone.Points.Count - 1]^.Y - 0.5,
    $0A254A
  );

  if DrawAtlasPoint(0, 0, 13) then
    ForceSelection := true;

  for i := 0 to Zone.Points.Count - 1 do
    DrawAtlasPoint(Zone.Points[i]^.X, Zone.Points[i]^.Y);

  for i := 0 to Zone.Points.Count - 2 do
    DrawAtlasPoint(
      (Zone.Points[i]^.X + Zone.Points[i + 1]^.X) / 2,
      (Zone.Points[i]^.Y + Zone.Points[i + 1]^.Y) / 2, 17);
  DrawAtlasPoint(
    (Zone.Points[0]^.X + Zone.Points[Zone.Points.Count - 1]^.X) / 2,
    (Zone.Points[0]^.Y + Zone.Points[Zone.Points.Count - 1]^.Y) / 2, 17);
  //
end;

procedure TEditor.SetAtlasZoneCP(Zone: anAtlasTextureZone; cp: zglTPoint2D);
var
  i: Integer;
begin
  for i := 0 to Zone.Points.Count - 1 do
    with Zone.Points[i]^ do begin
      X += Zone.X - cp.X;
      Y += Zone.Y - cp.Y;
    end;
  Zone.X := cp.X;
  Zone.Y := cp.Y;
end;

function TEditor.UpdateAtlasZone(Zone: anAtlasTextureZone): integer;
var
  i: Integer;
  p: zglPPoint2D;
begin
  result := -1;
  if mouse_clicked then begin
    if DistanceToMouse(Zone.X, Zone.Y) < 6 / Editor.CAM.Zoom.X then begin
      mouse_clear;
      exit(0);
    end;
    for i := 0 to Zone.Points.Count - 1 do
      if DistanceToMouse(Zone.X + Zone.Points[i]^.X, Zone.Y + Zone.Points[i]^.Y) < 6 / Editor.CAM.Zoom.X then begin
        if (mouse_state.button = mbRight) and (Zone.Points.Count > 3) then begin
          Dispose(Zone.Points[i]);
          Zone.Points.Delete(i);
          exit(-1);
          mouse_clear;
        end;
        mouse_clear;
        exit(i + 1);
      end;
    for i := 0 to Zone.Points.Count - 2 do
      if DistanceToMouse(
        Zone.X + (Zone.Points[i]^.X + Zone.Points[i + 1]^.X) / 2,
        Zone.Y + (Zone.Points[i]^.Y + Zone.Points[i + 1]^.Y) / 2) < 6 / Editor.CAM.Zoom.X then begin

        new(p);
        with p^ do begin
          X := (Zone.Points[i]^.X + Zone.Points[i + 1]^.X) / 2;
          Y := (Zone.Points[i]^.Y + Zone.Points[i + 1]^.Y) / 2;
        end;
        Zone.Points.Insert(i + 1, p);

        mouse_clear;
        exit(i + 2);
      end;
    if DistanceToMouse(
      Zone.X + (Zone.Points[0]^.X + Zone.Points[Zone.Points.Count - 1]^.X) / 2,
      Zone.Y + (Zone.Points[0]^.Y + Zone.Points[Zone.Points.Count - 1]^.Y) / 2) < 6 / Editor.CAM.Zoom.X then begin

      new(p);
      with p^ do begin
        X := (Zone.Points[0]^.X + Zone.Points[Zone.Points.Count - 1]^.X) / 2;
        Y := (Zone.Points[0]^.Y + Zone.Points[Zone.Points.Count - 1]^.Y) / 2;
      end;
      Zone.Points.Add(p);

      mouse_clear;
      exit(Zone.Points.Count - 1);
    end;
  end;
end;

function TEditor.AtlasZoneMiddlePoint(Zone: anAtlasTextureZone): zglTPoint2D;
var
  i: Integer;
begin
  with Result do begin X := 0; Y := 0; end;

  for i := 0 to Zone.Points.Count - 1 do begin
    Result.X += Zone.Points[i]^.X;
    Result.Y += Zone.Points[i]^.Y;
  end;

  Result.X /= Zone.Points.Count;
  Result.Y /= Zone.Points.Count;

  with Result do begin X += Zone.X; Y += Zone.Y; end;
end;

procedure TEditor.DrawAtlasZones(atlas: anAtlasTexture);
var
  i: Integer;
begin
  for i := 0 to atlas.ZonesCount - 1 do begin
    if TestZone(atlas.Zones.Data[i]) then
      DrawAtlasZone(atlas.Zones.Data[i], true);
  end;
end;

function TEditor.UpdateAtlasZones(atlas: anAtlasTexture): anAtlasTextureZone;
var
  i: Integer;
begin
  Result := nil;
  if mouse_clicked then begin
    for i := 0 to atlas.ZonesCount - 1 do begin
      if TestZone(atlas.Zones.Data[i]) then begin
        mouse_clear(false);
        Result := atlas.Zones.Data[i];
        exit;
      end;
    end;
  end;
end;

procedure TEditor.Clear;
begin
  Project.Clear;
  SelectedAnimation := nil;
  AnimInstance := nil;
  DrawData := nil;
  SelectedObject := nil;
  Selected.ObjectTo := -1;
  Selected.Left := 0;
  Selected.Right := 0;
  ContextObject := nil;
end;

procedure TEditor.Update(dt: Double);
begin
  Counter := Counter + dt * 0.01;
  if Counter >= 3 then Counter := 1;
  if Assigned(DrawData) then begin
    if DrawData is anAnimationInstance then begin
      anAnimationInstance(DrawData).Update(dt);
    end else
    if DrawData is anAnimation then begin
      UpdateAnimation(dt);
    end else
    if DrawData is anAtlasTexture then begin
      UpdateAtlasTexture(dt);
    end else
    if DrawData is anSymbol then begin
      UpdateSymbol(dt);
    end;
  end;
  if Assigned(AnimInstance) then begin
    AnimInstance.Update(dt);
  end;

  if DragState.Drag and (DragState.DropClass = anDropAnimation) then begin
    DragState.Animation.Update(dt);
  end;
end;

procedure TEditor.UpdateAnimation(dt: Double);
var vec: zglTPoint2D;
    min_dist_obj: anAnimationKeyFrameInstance;
    obj: anAnimationLOInstanceHolder;
    inst: anAnimationKeyFrameInstance;
    trans: anTransform;
    trans_p: anTransform;
    tot_angle: Single;
    obj_parent: anAnimationLOInstanceHolder;
begin
  if Assigned(SelectedAnimation) then begin
    if Editor.SelectedAnimation.ObjectsCount > 0 then begin
      if Assigned(SelectedObject) then begin

        obj := Editor.SelectedAnimation.HolderByIndex[SelectedObject.ObjectTo.Index];
        inst := obj.InstanceAt[AnimInstance.CurrentFrame];
        if Assigned(inst) then begin
          if mouse_clicked then begin
            if mouse_state.button = mbRight then begin
              SelectedObject := nil;
              RepaintTimeline;
              mouse_clear;
              exit;
            end;
          end;
          if Activated then begin
            case ToolMode of
              tmMove: begin
                with MainForm do begin
                  if SnapToGridTool.Checked then begin
                    vec.X := Round((mouse_state.pos.X - mouse_state.clickpos.X) / GridSize) * GridSize;
                    vec.Y := Round((mouse_state.pos.Y - mouse_state.clickpos.Y) / GridSize) * GridSize;
                  end else begin
                    vec.X := mouse_state.pos.X - mouse_state.clickpos.X;
                    vec.Y := mouse_state.pos.Y - mouse_state.clickpos.Y;
                  end;
                end;

                if Assigned(SelectedObject.ObjectTo.Parent) then begin
                  trans_p := SelectedObject.ObjectTo.Parent.GetTransform(obj.ParentHolder, AnimInstance.CurrentFrame
                        + AnimInstance.OffsetTime);
                  if trans_p.Flip then
                    vec.X := -vec.X;
                  RotatePoint(vec, - trans_p.Rotation);
                end;



                SelectedObject.Transformation.PositionX
                 := mouse_state.obj_pos.X + vec.X;

                SelectedObject.Transformation.PositionY
                 := mouse_state.obj_pos.Y + vec.Y;
              end;
              tmRotate: begin
                trans := SelectedObject.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame
                            + AnimInstance.OffsetTime);
                tot_angle := m_Angle(
                  trans.Position.X,
                  trans.Position.Y,
                  mouse_state.pos.X,
                  mouse_state.pos.Y
                );

                if Alternative then begin
                  anTransformClass.FixAngles(mouse_state.obj_single, tot_angle);
                  SelectedObject.Rounds :=
                     Round((tot_angle - mouse_state.obj_single) / 22.5);
                end else begin
                  SelectedObject.Transformation.Rotation
                    := mouse_state.obj_single + tot_angle;
                end;
              end;
              tmScale: begin
                trans := SelectedObject.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame
                            + AnimInstance.OffsetTime);
                SelectedObject.Transformation.Scale :=
                  mouse_state.obj_scale * (
                    m_Distance(
                        trans.Position.X,
                        trans.Position.Y,
                        mouse_state.pos.X,
                        mouse_state.pos.Y
                    )
                    / mouse_state.obj_single
                  );
              end;
            end;
            if not mouse_state.down then begin
              case ToolMode of
                tmParent: begin
                  min_dist_obj := ClosestObject;
                  obj_parent := Editor.SelectedAnimation.HolderByIndex[min_dist_obj.ObjectTo.Index];

                  if Assigned(min_dist_obj) and (min_dist_obj <> SelectedObject) then begin
                    Editor.History.AddItem(TEditorAssignParentItem.Create(
                     SelectedObject.ObjectTo, min_dist_obj.ObjectTo, obj, obj_parent, Editor.SelectedAnimation ));
                    UpdateAnimationInstance;
                  end
                end;
              end;
              History.Done;
              Activated := false;
              mouse_clear;
            end;
          end else begin
            if mouse_clicked then begin
              if mouse_state.button = mbLeft then begin
                if mouse_state.dblclick then begin
                  MainForm.HandleObject(SelectedObject);
                end else begin
                  case ToolMode of
                    tmMove: begin
                      SelectedObject := ForceCreation(obj, SelectedObject,
                        AnimInstance.CurrentFrame);
                      RepaintTimeline;

                      History.AddItem(TEditorUpdateLOEditTransformItem.Create(SelectedObject), false);
                      with mouse_state.obj_pos do begin
                        X := SelectedObject.Transformation.PositionX;
                        Y := SelectedObject.Transformation.PositionY;
                      end;
                    end;
                    tmRotate: begin
                      trans := SelectedObject.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame
                        + AnimInstance.OffsetTime);
                      SelectedObject := ForceCreation(obj, SelectedObject,
                        AnimInstance.CurrentFrame);
                      RepaintTimeline;
                      History.AddItem(TEditorUpdateLOEditTransformItem.Create(SelectedObject), false);

                      tot_angle := m_Angle(
                        trans.Position.X,
                        trans.Position.Y,
                        mouse_state.clickpos.X,
                        mouse_state.clickpos.Y
                      );

                      Alternative := ssCtrl in mouse_state.shift;

                      if Alternative then begin
                        mouse_state.obj_single := tot_angle;
                      end else begin
                        mouse_state.obj_single := SelectedObject.Transformation.Rotation -
                          tot_angle;
                      end;
                    end;
                    tmScale: begin
                      SelectedObject := ForceCreation(obj, SelectedObject,
                        AnimInstance.CurrentFrame);
                      RepaintTimeline;
                      History.AddItem(TEditorUpdateLOEditTransformItem.Create(SelectedObject), false);

                      mouse_state.obj_scale :=
                        SelectedObject.Transformation.Scale;
                      mouse_state.obj_single :=
                        m_Distance(
                          SelectedObject.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame + AnimInstance.OffsetTime).Position.X,
                          SelectedObject.ObjectTo.GetTransform(obj, AnimInstance.CurrentFrame).Position.Y,
                          mouse_state.clickpos.X,
                          mouse_state.clickpos.Y
                        );
                    end;
                  end;
                  Activated := true;
                end;
              end;
              mouse_clear;
            end;
          end;
        end;
      end else begin
        if mouse_clicked then begin
          SelectedObject := ClosestObject;
          if not Assigned(SelectedObject) then
            mouse_clear
          else if (mouse_state.button = mbRight) then begin
            ContextObject := SelectedObject.ObjectTo;
            MainForm.ObjectMenu.PopUp(mouse.CursorPos.x, mouse.CursorPos.y);
            mouse_clear;
          end;
        end;
      end;
    end;
  end;
end;

procedure TEditor.UpdateAtlasTexture(dt: Double);
var i: Integer;
    pvec: zglPPoint2D;
    zn: anAtlasTextureZone;
    atex: anAtlasTexture;
    f: Single;
begin
  atex := anAtlasTexture(DrawData);
  if Activated then begin
    case AtlasToolMode of
      atmEditPoints: begin
        if mouse_state.obj_int = 0 then begin
          SetAtlasZoneCP(SelectedZone, mouse_state.pos);
        end else
          with SelectedZone.Points[mouse_state.obj_int - 1]^ do begin
            X := mouse_state.pos.X - SelectedZone.X;
            Y := mouse_state.pos.Y - SelectedZone.Y;
          end;
      end;
      atmAddShape: begin
        with SelectedZone.Points[SelectedZone.Points.Count - 1]^ do begin
          X := mouse_state.pos.X - SelectedZone.X;
          Y := mouse_state.pos.Y - SelectedZone.Y;
        end;
        SetAtlasZoneCP(SelectedZone,
          AtlasZoneMiddlePoint(SelectedZone));
        if mouse_clicked then begin
          if mouse_state.button = mbRight then begin
            if SelectedZone.Points.Count >= 3 then begin
              Activated := false;

              if MainForm.AtlasAskZoneNames.Down then
                 SelectedZone.Name:= InputBox('Aatlas zone name', 'Enter atlas zone name', SelectedZone.Name);

              MainForm.AtlasEditPointZone.Down := true;
              AtlasToolMode := atmEditPoints;
            end;
          end else begin
            new(pvec);
            pvec^ := mouse_state.clickpos;
            SelectedZone.Points.Add(pvec);
          end;
          mouse_clear;
        end;
      end;
      atmMove: begin
        SelectedZone.X :=
          mouse_state.obj_pos.X + mouse_state.pos.X - mouse_state.clickpos.X;
        SelectedZone.Y :=
          mouse_state.obj_pos.Y + mouse_state.pos.Y - mouse_state.clickpos.Y;
      end;
      atmRotate: begin
        f := m_Angle(SelectedZone.X,
              SelectedZone.Y,
              mouse_state.pos.X,
              mouse_state.pos.Y) - mouse_state.obj_single;
        for i := 0 to SelectedZone.Points.Count - 1 do begin
          SelectedZone.Points[i]^ := Points[i];
          RotatePoint(SelectedZone.Points[i]^, f);
        end;
      end;
      atmScale: begin
        f := m_Distance(SelectedZone.X,
              SelectedZone.Y,
              mouse_state.pos.X,
              mouse_state.pos.Y) / mouse_state.obj_scale;
        for i := 0 to SelectedZone.Points.Count - 1 do
          with SelectedZone.Points[i]^ do begin
            X := Points[i].X * f;
            Y := Points[i].Y * f;
          end;
      end;
    end;

    if not mouse_state.down then begin
      case AtlasToolMode of
        atmAddRect: begin
          SelectedZone.X := (mouse_state.clickpos.X + mouse_state.pos.X) / 2;
          SelectedZone.Y := (mouse_state.clickpos.Y + mouse_state.pos.Y) / 2;

          new(pvec);
          pvec^.X := mouse_state.clickpos.X - SelectedZone.X;
          pvec^.Y := mouse_state.clickpos.Y - SelectedZone.Y;
          SelectedZone.Points.Add(pvec);

          new(pvec);
          pvec^.X := mouse_state.pos.X - SelectedZone.X;
          pvec^.Y := mouse_state.clickpos.Y - SelectedZone.Y;
          SelectedZone.Points.Add(pvec);

          new(pvec);
          pvec^.X := mouse_state.pos.X - SelectedZone.X;
          pvec^.Y := mouse_state.pos.Y - SelectedZone.Y;
          SelectedZone.Points.Add(pvec);

          new(pvec);
          pvec^.X := mouse_state.clickpos.X - SelectedZone.X;
          pvec^.Y := mouse_state.pos.Y - SelectedZone.Y;
          SelectedZone.Points.Add(pvec);

          atex.AddZone(SelectedZone, 'RECT_ZONE');
          if MainForm.AtlasAskZoneNames.Down then
             SelectedZone.Name:= InputBox('Aatlas zone name', 'Enter atlas zone name', SelectedZone.Name);

          MainForm.AtlasEditPointZone.Down := true;
          AtlasToolMode := atmEditPoints;

          Activated := false;
        end;
        atmEditPoints: begin
          Editor.History.Done;
          Activated := false;
        end;
        atmMove, atmRotate, atmScale: begin
          Editor.History.Done;
          if AtlasToolMode in [atmRotate, atmScale] then begin
            Points := nil;
          end;
          Activated := false;
        end;

      end;
    end;
  end else begin
    if mouse_clicked then begin
      case AtlasToolMode of
        atmAddRect: begin
          Editor.History.MadeChanges;
          SelectedZone := anAtlasTextureZone.Create(atex);
          Activated := true;
        end;
        atmAddShape: begin
          Editor.History.MadeChanges;
          SelectedZone := anAtlasTextureZone.Create(atex);
          new(pvec);
          pvec^ := mouse_state.clickpos;
          SelectedZone.Points.Add(pvec);
          new(pvec);
          pvec^ := mouse_state.clickpos;
          SelectedZone.Points.Add(pvec);

          atex.AddZone(SelectedZone, 'SHAPE_ZONE');

          Activated := true;
        end;
        atmEditPoints: begin
          for i := 0 to atex.ZonesCount - 1 do begin
            mouse_state.obj_int := UpdateAtlasZone(atex.Zones.Data[i]);
            if mouse_state.obj_int >= 0 then begin
              SelectedZone := atex.Zones.Data[i];
              //Editor.History.AddItem(TEditorUpdateAtlasZone.Create(SelectedZone), false); { TODO : UNDO }
              Activated := true;
              break;
            end;
          end;
        end;
        atmDeleteZone: begin
          zn := UpdateAtlasZones(atex);
          if Assigned(zn) then begin
            if MessageDlg('Are you shure to delete atlas zone ' + zn.Name + ' ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
              //Editor.History.AddItem(TEditorDeleteAtlasZone.Create(zn)); { TODO : UNDO }
              atex.DeleteZone(zn, false);
            end;
          end;
        end;
        atmMove, atmRotate, atmScale: begin
          SelectedZone := UpdateAtlasZones(atex);

          if Assigned(SelectedZone) then begin
            if mouse_state.dblclick then begin
               MainForm.HandleObject(SelectedZone);
            end else begin
              if AtlasToolMode in [atmRotate, atmScale] then begin
                SetLength(Points, SelectedZone.Points.Count);
                for i := 0 to SelectedZone.Points.Count - 1 do
                  Points[i] := SelectedZone.Points[i]^;
              end;
              //Editor.History.AddItem(TEditorUpdateAtlasZone.Create(SelectedZone)); { TODO : UNDO }
              case AtlasToolMode of
                atmMove: begin
                  with mouse_state.obj_pos do begin
                    X := SelectedZone.X;
                    Y := SelectedZone.Y;
                  end;
                end;
                atmRotate: begin
                  mouse_state.obj_single :=
                    m_Angle(SelectedZone.X,
                      SelectedZone.Y,
                      mouse_state.pos.X,
                      mouse_state.pos.Y);
                end;
                atmScale: begin
                  mouse_state.obj_scale :=
                    m_Distance(SelectedZone.X,
                      SelectedZone.Y,
                      mouse_state.pos.X,
                      mouse_state.pos.Y);
                end;
              end;
              Activated := true;
            end;
          end;
        end;
      end;
      mouse_clear(false);
    end;
  end;
end;

procedure TEditor.UpdateSymbol(dt: Double);
var sym: anSymbol;
begin
  sym := anSymbol(DrawData);
  if Assigned(sym) then begin
    if Assigned(sym.Texture) then begin
      if mouse_state.down then begin
        if mouse_state.button = mbLeft then begin
          sym.PivotX := mouse_state.pos.X;
          sym.PivotY := mouse_state.pos.Y;
        end else begin
          sym.Rotation := m_angle(mouse_state.pos.X, mouse_state.pos.Y, sym.PivotX, sym.PivotY);
        end;
      end;
    end;
  end;
end;

function TEditor.ClosestObject: anAnimationKeyFrameInstance;
var i: Integer;
    trn: anTransform;
    dst, min_dist: single;
    inst: anAnimationKeyFrameInstance;
    an: anAnimation;
begin
  Result := nil;

  if Assigned(SelectedAnimation) then begin
    an := SelectedAnimation;

    min_dist := $FFFFFF;

    for i := 0 to an.ObjectsCount - 1 do begin
      if (not an.ObjectByIndex[i].Enabled) or (not an.ObjectByIndex[i].Visible) then Continue;
      inst := an.HolderByIndex[i].InstanceAt[AnimInstance.CurrentFrame];
      if Assigned(inst) then begin
        trn := an.ObjectByIndex[i].GetTransform(an.HolderByIndex[i], AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
        dst := m_Distance(
          trn.Position.X,
          trn.Position.Y,
          mouse_state.pos.X,
          mouse_state.pos.Y);
        if dst < min_dist then begin
          Result := inst;
          min_dist := dst;
        end;
      end;
    end;
  end;
end;

procedure TEditor.DrawBones;
var i: Integer;
    trn, trn_to: anTransform;
    inst: anAnimationKeyFrameInstance;
    an: anAnimation;
begin

  if Assigned(SelectedAnimation) then begin
    an := SelectedAnimation;

    for i := 0 to an.ObjectsCount - 1 do begin
      if Assigned(an.HolderByIndex[i].ParentHolder) then begin
        inst := an.HolderByIndex[i].InstanceAt[AnimInstance.CurrentFrame];
        if Assigned(inst) then begin
          trn := an.ObjectByIndex[i].GetTransform(an.HolderByIndex[i], AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
          trn_to := an.ObjectByIndex[i].GetTransform(an.HolderByIndex[i].ParentHolder, AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
          DrawBone(
            trn.Position.X,
            trn.Position.Y,
            trn_to.Position.X,
            trn_to.Position.Y, BONE_LINE);
        end;
      end;
    end;
    for i := 0 to an.ObjectsCount - 1 do begin
      if Assigned(an.HolderByIndex[i].ParentHolder) then begin
        inst := an.HolderByIndex[i].InstanceAt[AnimInstance.CurrentFrame];
        if Assigned(inst) then begin
          trn := an.ObjectByIndex[i].GetTransform(an.HolderByIndex[i], AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
          trn_to := an.ObjectByIndex[i].GetTransform(an.HolderByIndex[i].ParentHolder, AnimInstance.CurrentFrame + AnimInstance.OffsetTime);
          DrawBone(
            trn.Position.X,
            trn.Position.Y,
            trn_to.Position.X,
            trn_to.Position.Y, BONE_ENDS);
        end;
      end;
    end;
  end;
end;

procedure TEditor.DrawBone(x0, y0, x1, y1: Single; DrawEnd: byte);
var dist, angle: single;
begin
  if DrawEnd and BONE_LINE > 0 then begin;
    dist := m_Distance(x0, y0, x1, y1);
    angle := m_Angle(x1, y1, x0, y0);
    ssprite2d_Draw(tex_bone,
      x0 + cos(angle * deg2rad) * (dist / 2) - dist / 2,
      y0 + sin(angle * deg2rad) * (dist / 2) - 4 / Editor.CAM.Zoom.X,
      dist, 8 / Editor.CAM.Zoom.X, round(angle));
    ssprite2d_Draw(tex_bone_anign,
      x0 + cos(angle * deg2rad) * (dist / 2) - 4 / Editor.CAM.Zoom.X,
      y0 + sin(angle * deg2rad) * (dist / 2) - 4 / Editor.CAM.Zoom.X,
      8 / Editor.CAM.Zoom.X, 8 / Editor.CAM.Zoom.X, round(angle));
  end;
  if DrawEnd and BONE_ENDS > 0 then begin;
    ssprite2d_Draw(tex_bone_end,
      x0 - 4 / Editor.CAM.Zoom.X, y0 - 4 / Editor.CAM.Zoom.X, 8 / Editor.CAM.Zoom.X, 8 / Editor.CAM.Zoom.X, 0);
    ssprite2d_Draw(tex_bone_end,
      x1 - 4 / Editor.CAM.Zoom.X, y1 - 4 / Editor.CAM.Zoom.X, 8 / Editor.CAM.Zoom.X, 8 / Editor.CAM.Zoom.X, 0);
  end;
end;

procedure TEditor.SelectFrame(Id: integer);
begin
  if Assigned(AnimInstance) then begin
    Id := Min(AnimInstance.Animation.FramesCount - 1, Id);

    case SelectionMode of
      smNone, smDone: begin
        Selected.Left := Id;
        Selected.Right := Id;
        SelectionMode := smStart;
        RepaintTimeline;
      end;
      smStart: begin
        Selected.Right := Id;
        RepaintTimeline;
      end;
    end;
    AnimInstance.Stop(true);
    MainForm.AnimationPlay.ImageIndex := 5;
  end;
end;

procedure TEditor.SelectDone(Id: integer);
var
  temp: Integer;
begin
  case SelectionMode of
    smStart: begin
      Id := Min(AnimInstance.Animation.FramesCount - 1, Id);
      Selected.Right := Id;
      SelectionMode := smDone;

      if Selected.Left > Selected.Right then begin
        temp := Selected.Left;
        Selected.Left := Selected.Right;
        Selected.Right := temp;
      end;
    end;
  end;
  RepaintTimeline;
end;

function TEditor.UpdateNames(X, Layer: integer; Context: Boolean): Boolean;
var
  obj: anAnimationLayerObject;
  mode: (mdHandle, mdEnable, mdVisible);
  vis: boolean;
  i: Integer;
begin
  if Assigned(SelectedAnimation) then begin
    if (X > 24) then
      mode := mdHandle
    else if (X > 12) then
      mode := mdEnable
    else
      mode := mdVisible;
    if Layer >= 0 then begin
      obj := SelectedAnimation.ObjectByIndex[Layer];
      if (Context) then begin
        ContextObject := obj;
        MainForm.ObjectMenu.PopUp(mouse.CursorPos.x, mouse.CursorPos.y);
      end else begin
        case mode of
          mdHandle: begin
            Selected.Left := 0;
            Selected.Right := SelectedAnimation.FramesCount - 1;
            Selected.ObjectTo := Layer;
            SelectionMode := smDone;
          end;
          mdEnable: obj.Enabled := not obj.Enabled;
          mdVisible: obj.Visible := not obj.Visible;
        end;
      end;
    end else begin
      if mode in [mdEnable, mdVisible] then begin
        vis := false;
        for i := 0 to SelectedAnimation.ObjectsCount - 1 do begin
          obj := SelectedAnimation.ObjectByIndex[i];
          case mode of
            mdEnable: if not obj.Enabled then vis := true;
            mdVisible: if not obj.Visible then vis := true;
          end;
        end;
        for i := 0 to SelectedAnimation.ObjectsCount - 1 do begin
          obj := SelectedAnimation.ObjectByIndex[i];
          case mode of
            mdEnable: obj.Enabled := vis;
            mdVisible: obj.Visible := vis;
          end;
        end;
      end;
    end;
  end;
  Result := not ((mode in [mdEnable, mdVisible]) or Context);
end;

procedure TEditor.SyncAnimation(an: anAnimation);
var
  obj: anAnimationLayerObject;
  i: Integer;
  tags, tags_obj: TStringList;
  mode: TTagMode;
  preset: TPresetObject;
  j: Integer;
  parentNode: PVirtualNode;
  parent_preset: TAnimationPreset;
  p_tag: TPresetTag;
begin
  if not Assigned(an) then exit;

  with MainForm do begin
    TimeLineBox.Clear;

    TimeLineBox.Header.Columns[COLUMN_TIMELINE].Width
      := an.FramesCount * FRAME_HEIGHT + FRAMES_OFFSET;

    parent_preset := Project.GetPreset(an.Name);
    mode := parent_preset.Mode;

    case mode of
      modeNormal: begin
        for i := 0 to an.ObjectsCount - 1 do begin
          obj := an.ObjectByIndex[i];
          if an is anAnimationBlender then
            if not anAnimationBlender(an).Proxy[i].Apply then begin
              obj.Enabled := false;
              Continue;
            end;
          TimeLineBox.AddChild(TimeLineBox.RootNode, obj);
        end;
      end;
      modeTags: begin
        tags := TStringList.Create;
        tags.Sorted := true;
        tags.Duplicates := dupIgnore;
        tags_obj := TStringList.Create;

        for i := 0 to an.ObjectsCount - 1 do begin
          obj := an.ObjectByIndex[i];
          preset := parent_preset.GetObject(obj.Name);
          tags_obj.Text := StringReplace(preset.Tags, ' ' ,#13#10,[rfReplaceAll, rfIgnoreCase]);
          tags.AddStrings(tags_obj);
        end;

        for i := 0 to tags.Count - 1 do begin
          if tags[i] <> '' then begin
            p_tag := parent_preset.GetTag(tags[i]);
            parentNode := TimeLineBox.AddChild(TimeLineBox.RootNode, p_tag);
            for j := 0 to an.ObjectsCount - 1 do begin
              obj := an.ObjectByIndex[j];
              preset := parent_preset.GetObject(obj.Name);
              tags_obj.Text := StringReplace(preset.Tags, ' ' ,#13#10,[rfReplaceAll, rfIgnoreCase]); ;
              if tags_obj.IndexOf(tags[i]) >= 0 then begin
                if an is anAnimationBlender then
                  if not anAnimationBlender(an).Proxy[j].Apply then begin
                    obj.Enabled := false;
                    Continue;
                  end;
                TimeLineBox.AddChild(parentNode, obj);
              end;
            end;
            TimeLineBox.Expanded[parentNode] := p_tag.Visible;
          end;
        end;
      end;
    end;

    MakeOperation(TimeLineBox.RootNode, actShowFilter, @parent_preset.Filter);
  end;
end;

function TEditor.ForceCreation(Holder: anAnimationLOInstanceHolder;
  Obj: anAnimationKeyFrameInstance; Time: Integer): anAnimationKeyFrameInstance;
var transform: anTransform;
    i, fPos: Integer;
    objto: anAnimationLayerObject;
    F: Single;
    data: pointer;
    node: PVirtualNode;
begin
  fPos := Obj.Position;
  if fPos <> Time then begin
    transform := Obj.ObjectTo.GetTransform(Holder, Time, false);
    result := anAnimationKeyFrameInstance.Create(
      Obj, Obj.Next, Holder, Obj.ObjectTo, Holder.Animation, Time, transform);
    result.InterpolationMode := NewInterpolation;
    Editor.History.AddItem(TEditorAddKeyFrameItem.Create(Result), false);
    MainForm.TimeLineBox.Repaint;
  end else begin
    result := Obj;
  end;
  with MainForm do begin
    data := nil;

    MakeOperation(TimeLineBox.RootNode, actFindWithData, obj.ObjectTo, @data);

    if Assigned(data) then begin
      node := PVirtualNode(data);
      TimeLineBox.FocusedNode := node;
    end;
  end;
end;

procedure TEditor.RepaintTimeline;
begin
  MainForm.TimeLineBox.Repaint;
end;

procedure TEditor.MultZoom(f: Single);
var ZummedSize: zglTPoint2D;
begin
  with RenderRect do begin
    ZummedSize.X := W * f - W;
    ZummedSize.Y := H * f - H;
    X -= (mouse_state.pos.X / W) * ZummedSize.X;
    Y -= (mouse_state.pos.Y / H) * ZummedSize.Y;
    W *= f;
    H *= f;
  end;
  CurrentZoom *= f;
end;

procedure TEditor.ZoomTo(f: Single);
begin
   MultZoom(f / CurrentZoom);
end;

procedure TEditor.ResetZoom;
begin
  CurrentZoom := 1;
  with RenderRect do begin
    X := 0;
    Y := 0;
    W := MainForm.DrawPanel.ClientWidth;
    H := MainForm.DrawPanel.ClientHeight;
  end;
end;

procedure TEditor.Init;
begin
  mouse_state.state := msNone;

  MainFont := font_LoadFromFile('../data/Main.zfi');
  tex_Filter(MainFont.Pages[0], TEX_DEFAULT_NEAREST);

  tex_selected := tex_LoadFromFile('../data/selected.png',
    TEX_NO_COLORKEY, TEX_DEFAULT_NEAREST);
  tex_SetFrameSize(tex_selected,
    TEX_SELECTED_SIZE, TEX_SELECTED_SIZE);

  tex_bone := tex_LoadFromFile('../data/bone.png',
    TEX_NO_COLORKEY, TEX_DEFAULT_2D);
  tex_bone_end := tex_LoadFromFile('../data/bone_end.png',
    TEX_NO_COLORKEY, TEX_DEFAULT_2D);
  tex_bone_anign := tex_LoadFromFile('../data/bone_align.png',
    TEX_NO_COLORKEY, TEX_DEFAULT_2D);
  zgl_Disable(APP_USE_AUTOPAUSE);

  SelectedAnimation := nil;

  UpdateRenderTarget;

  scr_SetVSync( TRUE );
  cam2d_Init(CAM);
  with CAM do begin
    Zoom.X := 1;
    Zoom.Y := 1;
    Center.X := 0;
    Center.Y := 0;
  end;

  CurrentZoom := 1;
  with RenderRect do begin
    X := 0;
    Y := 0;
    W := MainForm.DrawPanel.ClientWidth;
    H := MainForm.DrawPanel.ClientHeight;
  end;

  wnd_SetSize( MainForm.DrawPanel.ClientWidth, MainForm.DrawPanel.ClientHeight );

  InitOtherInstances(2);

  RegisterOtherInstance('Point object',
    'Object with no visibility that allows you draw your own object at it''s position.',
    0, anAnimationPointObject, OnPointObjectCreate);

  RegisterOtherInstance('Lookup point object',
    'Object with no visibility that allows you draw your own object at it''s position, but you know the position of some other assigned object.',
    1, anAnimationLookUpPointObject, OnPointLookUpObjectCreate);

  MainForm.BringToFront();
end;

constructor TEditor.Create;
begin
  DrawData := nil;
  SelectedObject := nil;
  ObjectDragState.OBJ := nil;
  CircuitTarget := nil;
  Counter := 1;
  Alternative := false;
  Selected := TSelection.Create;
  SelectionMode := smNone;
  Activated := false;
  ToolMode := tmMove;
  AtlasToolMode := atmAddRect;
  NewInterpolation := imSin;
  History := TEditorHistory.Create;
  DefaultTransformation := anTransformClass.Create(nil);
  AnimInstance := nil;
  SelectedZone := nil;
  Settings := TDataBase.Create(TFileHandler.Create(ExtractFileDir(Application.ExeName) +  '/settings.db'));
  Recent := Settings.Root.ChildByName['recent'];

  DragState.Animation := nil;
  DragState.Content := nil;

  Project := TEditorProject.Create;
end;

destructor TEditor.Destroy;
begin
  Project.Free;
  with MainForm do begin
    Settings.Root.ChildByName['interpolation'].AsInteger := Integer(NewInterpolation);
    Settings.Root.ChildByName['grid'].AsInteger := GridSize;
    Settings.Root.ChildByName['draw_grid'].AsBoolean := GridTool.Down;
    Settings.Root.ChildByName['snap_grid'].AsBoolean := SnapToGridTool.Checked;
    Settings.Root.ChildByName['rulers'].AsBoolean := RuleTool.Checked;
    Settings.Root.ChildByName['draw_bones'].AsBoolean := DrawBonesTool.Down;
    Settings.Root.ChildByName['highlight'].AsBoolean := DrawHightLightTool.Down;
    Settings.Root.ChildByName['draw_zone_names'].AsBoolean := AtlasDrawZoneNames.Down;
    Settings.Root.ChildByName['ask_zone_names'].AsBoolean := AtlasAskZoneNames.Down;
  end;
  Settings.Save;
  Settings.Free;
  DefaultTransformation.Free;
  History.Destroy;
  Selected.Free;
  inherited Destroy;
end;

end.

