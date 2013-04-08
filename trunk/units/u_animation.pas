unit u_animation;

{
   *--------------------------------------*
   |        ZenGL ANIMATIONS UNIT         |
   |--------------------------------------|
   |    Version: | 0.1a                   |
   |   Requires: | u_db, zengl, generics  |
   |--------------------------------------|
   |     Author: | desertkun              |
   |     E-mail: | desertlun@gmail.com    |
   *--------------------------------------*
}

interface

{$mode DELPHI}

uses
  classes, fgl, u_db,
{$IFDEF STATIC}
  zgl_main, zgl_text, zgl_window, zgl_screen, zgl_utils, zgl_math_2d,
  zgl_font, zgl_log, zgl_textures, zgl_render, zgl_grid_2d, zgl_mouse,
  zgl_collision_2d, zgl_primitives_2d, zgl_keyboard, zgl_sprite_2d,
  zgl_file, zgl_ini, zgl_textures_tga, zgl_fx, zgl_render_target,
  zgl_render_2d, zgl_memory
{$ELSE}
  zglHeader
{$ENDIF}
  ;

type
  anTextureFilter = (tfNearest, tfLinear, tfBilinear, tfTrilinear, tfAnisotropy);
  anAnimationState = (asStopped, asPlaying);
  anInterpolationMode = (imNone, imLinear, imSin, imSinFadeIn, imSinFadeOut);
  anPosition = Integer;

const
  DEFAULT_INTERPOLATION_MODE:
    anInterpolationMode            = imSin;
  ANIM_VERSION                     = 0;

type
  anNamedObject                      = class;
  anObjectWithData                   = class;
  anAnimationSet                     = class;
  anLibrary                          = class;
  anSymbol                           = class;
  anAnimation                        = class;
  anAnimationPrototype               = class;
  anAnimationBlender                 = class;
  anTexture                          = class;
  anTextureParams                    = class;
  anAtlasTextureZone                 = class;
  anAnimationLayerObject             = class;
  anAnimationInstance                = class;
  anAnimationInstanceTree            = class;
  anAnimationLOInstanceHolder        = class;
  anAnimationLayerObjectInstance     = class;
  anAnimationKeyFrameInstance        = class;
  anOtherKFProperty                  = class;
  anAnimationLayerAnimationInstance  = class;
  anAnimatedSpriteTexture            = class;
  anAnimationLayerSymbolInstance     = class;

  anAnimationEvent                 = procedure (Sender: anObjectWithData) of object;
  anString                         = string;

  { anTransform }

  p_anTransform = ^anTransform;
  anTransform = packed record
    Position: zglTPoint2D;
    Scale: Single;
    Rotation: Single;
    Flip: Boolean;
    Transparency: Byte;
  end;

  anPInstanceParameters = ^anInstanceParameters;
  anInstanceParameters = packed record
    dt: double;
    Frame: integer;
    Data: Pointer;
  end;

  anDrawResult = record
    Transform: anTransform;
    Visible: Boolean;
  end;

  { anTransformClass }

  anTransformClass = class ( TPersistent )
    private
      fRecord: p_anTransform;
      function getPositionX: Single;
      function getPositionY: Single;
      function getFlip: Boolean;
      function getRotation: Single;
      function getScale: Single;
      function getTransparency: Byte;
      procedure setPositionX(AValue: Single);
      procedure setPositionY(AValue: Single);
      procedure setFlip(AValue: Boolean);
      procedure setRotation(AValue: Single);
      procedure setScale(AValue: Single);
      procedure setTransparency(AValue: Byte);
      class procedure FixAngle(var a: Single);
      class function BoolToSign(b: boolean): Integer;
      class function InterpolateData(f: Single; Kind: anInterpolationMode): Single;
      class function InterpolateFloat(a, b, f: Single; Kind: anInterpolationMode): Single;
      class function InterpolateByte(a, b: Byte; f: Single; Kind: anInterpolationMode): Byte;
      class procedure SwapAngle(var angle: Single; byAngle: Single);
    published
      property PositionX: Single read getPositionX write setPositionX;
      property PositionY: Single read getPositionY write setPositionY;
      property Scale: Single read getScale write setScale;
      property Rotation: Single read getRotation write setRotation;
      property Flip: Boolean read getFlip write setFlip;
      property Transparency: Byte read getTransparency write setTransparency;
    public
      procedure AssignTo(Dest: TPersistent); override;

      property TransformData: p_anTransform read fRecord write fRecord;

      class procedure GetAngle(var angle: Single; parentAngle: Single; Flip: Boolean);
      procedure Init(pX, pY: Single);

      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      class procedure FixAngles(var from_: Single; var to_: Single);
      class function Interpolate(FromW, ToW: anTransform;
        Value: Single; Rounds: Integer; Kind: anInterpolationMode): anTransform;
      class function ApplyTransform(Parent, Child: anTransform): anTransform;
      class function NewTransform: anTransform;
      class function Get(
        pX: Single = 0;
        pY: Single = 0;
        pRotation: Single = 0;
        pScale: Single = 1;
        pFlip: Boolean = false;
        pTransparency: Byte = 255): anTransform;

      constructor Create(pTransform: p_anTransform; pInit: boolean = true);
      destructor Destroy; override;
  end;

  { events }

  anOnNameChange = procedure (Instance: anNamedObject) of object;

  { anNamedObject }

  anNamedObjectList = TFPGMap<anString, anNamedObject>;

  anObjectWithData = class ( TPersistent )
    private
      fUserData: Pointer;
    public
      property UserData: Pointer read fUserData write fUserData;
  end;

  anNamedObject = class ( anObjectWithData )
    private
      fParentMap: anNamedObjectList;
      fOnNameChange: anOnNameChange;
      function getName: anString;
      procedure setName(AValue: anString);
      function GetMyIndex: integer;
    published
      property Name: anString read getName write setName;
    public
      procedure AssignTo(Dest: TPersistent); override;

      property OnNameChange: anOnNameChange read fOnNameChange write fOnNameChange;
      property ParentMap: anNamedObjectList read fParentMap write fParentMap;

      function GetUniqueName(From: anString): anString;

      constructor Create(aParentMap: TFPSMap);
  end;

  { anTextureContent }

  zglPPTexture = ^zglPTexture;

  anTextureContent = class ( anNamedObject )
    private
      fName, fExtension: anString;
      fAnimation: anAnimationSet;
      TextureData: zglPTexture;
      Data: array of Byte;

      fFilter: anTextureFilter;
      function getData: zglPTexture;
      function getHeight: LongWord;
      function getMemorySize: LongWord;
      function getWidth: LongWord;
      procedure setFilter(AValue: anTextureFilter);
      function getTexture: zglPPTexture;
      procedure Load;
    published
      property Extension: anString read fExtension write fExtension;
      property FileName: anString read fName write fName;
      property Filter: anTextureFilter read fFilter write setFilter;
      property MemorySize: LongWord read getMemorySize;
      property Width: LongWord read getWidth;
      property Height: LongWord read getHeight;
    public
      Content: zglTMemory;
      property TexData: zglPTexture read getData;
      property Animation: anAnimationSet read fAnimation write fAnimation;
      property _Texture: zglPPTexture read getTexture;

      procedure AssignTo(Dest: TPersistent); override;

      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      procedure LoadFromMemory(const Buffer; Size: LongWord);
      function LoadFromFile(const FileNameFrom: anString; const SetNameAs: anString): Boolean;
      procedure SaveToFile(const FileNameFrom: anString);

      class function getFilter(AValue: anTextureFilter): Integer;

      constructor Create(pAnimation: anAnimationSet);
      destructor Destroy; override;
  end;

  anTextureContentClass = class of anTextureContent;

  { anTexture }

  anTexture = class( anNamedObject )
    private
      fFileData: anTextureContent;
      fAnimation: anAnimationSet;
      fWidth, fHeight: integer;
      function getData: zglPTexture;

      function CreateParams(pSymbol: anSymbol): anTextureParams; virtual; abstract;
    published
      property FileContent: anTextureContent read fFileData write fFileData;
      property Animation: anAnimationSet read fAnimation write fAnimation;
      property Width: integer read fWidth write fWidth;
      property Height: integer read fHeight write fHeight;
    public
      property Data: zglPTexture read getData;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); virtual;
      procedure LoadFromRecord(rec: TDBRecord); virtual;

      procedure Draw(pX, pY: Single; pAplha: byte; Params: anPInstanceParameters); virtual;

      constructor Create(pAnimation: anAnimationSet); virtual;
      destructor Destroy; override;
  end;

  { anTextureParams }

  anTextureParams = class ( anObjectWithData )
    private
      fTexture: anTexture;
      fSymbol: anSymbol;
    public
      property Texture: anTexture read fTexture write fTexture;
      property Symbol: anSymbol read fSymbol write fSymbol;

      function Width(TexOf: zglPTexture): Single; virtual;
      function Height(TexOf: zglPTexture): Single; virtual;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); virtual;
      procedure LoadFromRecord(rec: TDBRecord); virtual;

      function HaveTexture: Boolean; virtual;
      function GetTexture: Pointer; virtual;

      function GetOtherProperties(AnimationTo: anAnimationPrototype; pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty; virtual;
      function GetObjectInstance(AnimInstance: anAnimationInstance;
        ObjectTo: anAnimationLayerObject;
        HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; virtual;

      function FormatTexture(TexturePointer: Pointer): zglPTexture; virtual;
      procedure ReleaseTexture(TexturePointer: Pointer); virtual;

      procedure Draw(Transform: anTransform; pContent: zglPTexture;
         pInstance: anAnimationLayerSymbolInstance;
         FX: LongWord = FX_BLEND); virtual;

      constructor Create(pTexture: anTexture; pSymbol: anSymbol); virtual;
  end;

  anTextureClass = class of anTexture;

  { anFullTexture }

  anFullTexture = class ( anTexture)
    private
      function CreateParams(pSymbol: anSymbol): anTextureParams; override;
  end;

  { anFullTextureParams }

  anFullTextureParams = class ( anTextureParams )
    public
      procedure AssignTo(Dest: TPersistent); override;
      procedure Draw(Transform: anTransform; pContent: zglPTexture;
         pInstance: anAnimationLayerSymbolInstance;
         FX: LongWord = FX_BLEND); override;
  end;

  { anStaticSpriteTexture }

  anStaticSpriteTexture = class ( anTexture )
    private
      fTileX, fTileY, fFramesCount: Integer;

      function getFramesCount: integer;
      procedure setTileX(AValue: Integer);
      procedure setTileY(AValue: Integer);
      procedure updateTile;
      function CreateParams(pSymbol: anSymbol): anTextureParams; override;
    published
      property TileX: Integer read fTileX write setTileX;
      property TileY: Integer read fTileY write setTileY;
      property FramesCount: integer read fFramesCount write fFramesCount;
    public

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      procedure Draw(pX, pY: single; pAplha: byte; Params: anPInstanceParameters); override;
      constructor Create(pAnimation: anAnimationSet); override;

  end;

  { anStaticSpriteTextureParams }

  anStaticSpriteTextureParams = class ( anTextureParams )
    private
      fTile: integer;

    published
       property Tile: integer read fTile write fTile;
    public
      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      function GetOtherProperties(AnimationTo: anAnimationPrototype; pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty; override;
      function GetObjectInstance(AnimInstance: anAnimationInstance;
        ObjectTo: anAnimationLayerObject;
        HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; override;

      procedure Draw(Transform: anTransform; pContent: zglPTexture;
         pInstance: anAnimationLayerSymbolInstance;
         FX: LongWord = FX_BLEND); override;

      constructor Create(pTexture: anTexture; pSymbol: anSymbol); override;
  end;

  { anAnimatedSpriteTextureTile }

  anAnimatedSpriteTextureTile = class ( anObjectWithData )
    private
      fTile: anPosition;
      fTime: Single;
    published
      property Tile: anPosition read fTile write fTile;
      property Time: Single read fTime write fTime;
    public

      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      constructor Create;
  end;

  anAnimatedSpriteTextureTileList = TFPGList<anAnimatedSpriteTextureTile>;

  { anAnimatedSpriteTextureTileSet }

  anAnimatedSpriteTextureTileSet = class ( anNamedObject )
    private
      fTexture: anAnimatedSpriteTexture;
      fTileList: anAnimatedSpriteTextureTileList;
    public
      property Texture: anAnimatedSpriteTexture read fTexture write fTexture;
      property TileList: anAnimatedSpriteTextureTileList read fTileList write fTileList;

      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      constructor Create(pTexture: anAnimatedSpriteTexture);
      destructor Destroy; override;
  end;

  anAnimatedSpriteTextureTileSetList = TFPGMap<anString, anAnimatedSpriteTextureTileSet>;

  { anAnimatedSpriteTexture }

  anAnimatedSpriteTexture = class ( anStaticSpriteTexture )
    private
      fTileSets: anAnimatedSpriteTextureTileSetList;
      function CreateParams(pSymbol: anSymbol): anTextureParams; override;
    public
      property TileSets: anAnimatedSpriteTextureTileSetList read fTileSets write fTileSets;
      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      constructor Create(pAnimation: anAnimationSet); override;
      destructor Destroy; override;
  end;

  { anAnimatedSpriteTextureParams }

  anAnimatedSpriteTextureParams = class ( anTextureParams )
    private
      fTileSet: anAnimatedSpriteTextureTileSet;
    published
       property TileSet: anAnimatedSpriteTextureTileSet read fTileSet write fTileSet;
    public
      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      function GetOtherProperties(AnimationTo: anAnimationPrototype; pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty; override;
      function GetObjectInstance(AnimInstance: anAnimationInstance;
         ObjectTo: anAnimationLayerObject;
         HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; override;

      procedure Draw(Transform: anTransform; pContent: zglPTexture;
         pInstance: anAnimationLayerSymbolInstance;
         FX: LongWord = FX_BLEND); override;

      constructor Create(pTexture: anTexture; pSymbol: anSymbol); override;
  end;

  { anAtlasTextureZone }

  anAtlasTextureZoneList = TFPGMap<anString, anAtlasTextureZone>;
  anAtlasTextureZonePoints = TFPGList<zglPPoint2D>;

  anAtlasTexture = class;

  anAtlasTextureZone = class ( anNamedObject )
    private
      fAtlas: anAtlasTexture;
      fX, fY: Single;
      fPoints: anAtlasTextureZonePoints;
    published
      property Points: anAtlasTextureZonePoints read fPoints write fPoints;
      property Atlas: anAtlasTexture read fAtlas;
      property X: Single read fX write fX;
      property Y: Single read fY write fY;
    public
      procedure Draw(pX, pY: Single);

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);
      procedure Clear;

      function GetTarget: zglPRenderTarget;

      constructor Create(pAtlas: anAtlasTexture);
      destructor Destroy; override;
  end;

  { anAtlasTexture }

  anAtlasTexture = class( anTexture )
    private
      fZones: anAtlasTextureZoneList;
      function CreateParams(pSymbol: anSymbol): anTextureParams; override;
      function getZonesCount: integer;
    published
      property ZonesCount: integer read getZonesCount;
    public
      property Zones: anAtlasTextureZoneList read fZones write fZones;

      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      procedure Draw(pX, pY: Single; pAplha: byte; Params: anPInstanceParameters); override;

      function AddZone(pName: anString): anAtlasTextureZone; overload;
      function AddZone(Zone: anAtlasTextureZone; pName: anString): anAtlasTextureZone; overload;

      procedure DeleteZone(pName: anString; FreeAfter: boolean = true); overload;
      procedure DeleteZone(Zone: anAtlasTextureZone; FreeAfter: boolean = true); overload;

      constructor Create(pAnimation: anAnimationSet); override;
      destructor Destroy; override;
  end;

  { anAtlasTextureParams }

  anAtlasTextureParams = class ( anFullTextureParams )
    private
      fZone: anAtlasTextureZone;
    published
      property Zone: anAtlasTextureZone read fZone write fZone;
    public

      function Width(TexOf: zglPTexture): Single; override;
      function Height(TexOf: zglPTexture): Single; override;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      function HaveTexture: Boolean; override;
      function GetTexture: Pointer; override;
      function FormatTexture(TexturePointer: Pointer): zglPTexture; override;
      procedure ReleaseTexture(TexturePointer: Pointer); override;

      constructor Create(pTexture: anTexture; pSymbol: anSymbol); override;
  end;

  { anAnimationObjectAssigner }

  anTimeLineApplyMode = (tlamNone, tlamOverwrite, tlamApply);

  anAnimationObjectAssigner = class (anObjectWithData)
    private
      Data: anTransform;
      fTransform: anTransformClass;
      fApply: Boolean;
    published
      property Transform: anTransformClass read fTransform write fTransform;
      property Apply: Boolean read fApply write fApply;
    public
      function ApplyTo(From: anTransform; Mode: anTimeLineApplyMode): anTransform;

      constructor Create;
      destructor Destroy; override;
  end;

  anTransformClassList = TFPGMap<anString, anAnimationObjectAssigner>;

  { anAnimationTimeLine }

  anAnimationTimeLine = class (anObjectWithData)
    private
      fAnimation: anAnimation;
      fApplyMode: anTimeLineApplyMode;
      fInstance: anAnimationInstance;
      fObjectTransforms: anTransformClassList;
      fState: anAnimationState;
      fCurrentFrame: anPosition;
      fDt: Double;
      fOnAnimationEnd: anAnimationEvent;

      procedure CheckFrame;
      function getOffsetTime: Double;
      function getTotalTime: Single;
      procedure setCurrentFrame(AValue: anPosition);
      function getKeyframePosition(Name: anString): anPosition;
    published
      property ApplyMode: anTimeLineApplyMode read fApplyMode write fApplyMode;
      property Animation: anAnimation read fAnimation write fAnimation;
      property Instance: anAnimationInstance read fInstance write fInstance;
      property CurrentFrame: anPosition read fCurrentFrame write setCurrentFrame;
      property OnAnimationEnd: anAnimationEvent read fOnAnimationEnd write fOnAnimationEnd;
    public
      property TotalTime: Single read getTotalTime;
      property ObjectTransforms: anTransformClassList read fObjectTransforms write fObjectTransforms;
      property State: anAnimationState read fState write fState;
      property DeltaTime: Double read fDt write fDt;
      property OffsetTime: Double read getOffsetTime;

      procedure Stop(Pause: Boolean);
      procedure Play;
      procedure GotoAndStop(Frame: anPosition; Pause: Boolean); overload;
      procedure GotoAndStop(Frame: anString; Pause: Boolean); overload;

      procedure GotoAndPlay(Frame: anPosition); overload;
      procedure GotoAndPlay(Frame: anString); overload;

      procedure Update(dt: double);

      constructor Create(pAnimation: anAnimation; pInstance: anAnimationInstance; pApplyMode: anTimeLineApplyMode);
      destructor Destroy; override;
    end;

  anOnActivateCallback = procedure (Obj: anAnimationLayerObjectInstance;
     Sender: anAnimationKeyFrameInstance;  Prop: anOtherKFProperty) of object;

  { anAnimationActioner }

  anAnimationAction = (apaNone, apaGotoAndStop, apaGotoAndPlay, apaStop, apaPlay);
  anAnimationActioner = class ( TPersistent )
    private
      fAction: anAnimationAction;
      fFrameTo: anString;
    published
      property FrameTo: anString read fFrameTo write fFrameTo;
      property Action: anAnimationAction read fAction write fAction;
    public
      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      procedure Activate(InstanceTo: anAnimationInstance);

      constructor Create;
  end;

  { anOtherKFProperty }

  anOtherKFProperty = class ( TPersistent )
    private
      fThisAnimationAction: anAnimationActioner;
      fAnimation: anAnimation;
      fOnActivateCallback, fOnFrameCallback: anString;
      fKeyFrame: anAnimationKeyFrameInstance;
    published
      property OnActivateCallback: anString read fOnActivateCallback write fOnActivateCallback;
      property OnFrameCallback: anString read fOnFrameCallback write fOnFrameCallback;
      property ThisAnimationAction: anAnimationActioner read fThisAnimationAction write fThisAnimationAction;
    public
      property KeyFrame: anAnimationKeyFrameInstance read fKeyFrame write fKeyFrame;
      property Animation: anAnimation read fAnimation write fAnimation;

      procedure OnActivate(Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance); virtual;
      procedure OnFrame(Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance; Frame: anPosition); virtual;

      procedure SaveToRecord(rec: TDBRecord); virtual;
      procedure LoadFromRecord(rec: TDBRecord); virtual;

      constructor Create(pAnimation: anAnimation; pKeyFrame: anAnimationKeyFrameInstance); virtual;
      destructor Destroy; override;
  end;

  anOtherStaticSpritePropertyAction = (spaNone, spaSetFrame);

  { anOtherAnimationProperty }

  anOtherAnimationProperty = class (anOtherKFProperty)
    private
      fChildAnimationAction: anAnimationActioner;
    published
      property ChildAnimationAction: anAnimationActioner read fChildAnimationAction write fChildAnimationAction;
    public
      procedure OnActivate(Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance); override;

      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      constructor Create(pAnimation: anAnimation; pKeyFrame: anAnimationKeyFrameInstance); override;
      destructor Destroy; override;
  end;

  { anOtherStaticSpriteProperty }

  anOtherStaticSpriteProperty = class (anOtherKFProperty)
    private
      fAction: anOtherStaticSpritePropertyAction;
      fFrameTo: anPosition;
    published
      property FrameTo: anPosition read fFrameTo write fFrameTo;
      property Action: anOtherStaticSpritePropertyAction read fAction write fAction;
    public
      procedure OnActivate(Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance); override;
      procedure OnFrame(Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance; Frame: anPosition); override;

      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      constructor Create(pAnimation: anAnimation; pKeyFrame: anAnimationKeyFrameInstance); override;
  end;

  { anOtherAnimationSpriteProperty }

  anAnimationTileSetAction = (tsaNone, tsaStop, tsaPlay, tsaChangeTileSetAndStop, tsaChangeTileSetAndPlay);

  anOtherAnimationSpriteProperty = class (anOtherKFProperty)
    private
      fTileSetAction: anAnimationTileSetAction;
      fTileSetTo: anAnimatedSpriteTextureTileSet;
    published
      property TileSetAction: anAnimationTileSetAction read fTileSetAction write fTileSetAction;
      property TileSetTo: anAnimatedSpriteTextureTileSet read fTileSetTo write fTileSetTo;
    public
      procedure OnActivate(Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance); override;
      procedure OnFrame(Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance; Frame: anPosition); override;

      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      constructor Create(pAnimation: anAnimation; pKeyFrame: anAnimationKeyFrameInstance); override;
  end;

  { anAnimationKeyFrameInstance }

  anAnimationKeyFrameInstance = class(anObjectWithData)
    private
      fVisible: boolean;
      fAnimation: anAnimation;
      fName: anString;
      fObjectTo: anAnimationLayerObject;
      fFrames: anPosition;
      fHolder: anAnimationLOInstanceHolder;
      fInterpolationMode: anInterpolationMode;
      fRounds: Integer;
      fTransformation: anTransformClass;
      fPrev, fNext: anAnimationKeyFrameInstance;
      fActiveProperties: anOtherKFProperty;

      Transform: anTransform;
      function getFrames: anPosition;
      function getPosition: Integer;
      procedure Init(pHolder: anAnimationLOInstanceHolder; pPrev, pNext: anAnimationKeyFrameInstance;
         pPosition: anPosition);
    published
      property Name: anString read fName write fName;
      property Animation: anAnimation read fAnimation write fAnimation;
      property Holder: anAnimationLOInstanceHolder read fHolder write fHolder;
      property ObjectTo: anAnimationLayerObject read fObjectTo write fObjectTo;
      property Visible: Boolean read fVisible write fVisible;
      property Transformation: anTransformClass read fTransformation write fTransformation;
      property InterpolationMode: anInterpolationMode read fInterpolationMode write fInterpolationMode;
      property Frames: anPosition read getFrames write fFrames;
      property Rounds: Integer read fRounds write fRounds;
      property ActiveProperties: anOtherKFProperty read fActiveProperties write fActiveProperties;

      function GetNextFrames: anPosition;
      function GetNextInstance: anAnimationKeyFrameInstance;
    public
      property Prev: anAnimationKeyFrameInstance read fPrev write fPrev;
      property Next: anAnimationKeyFrameInstance read fNext write fNext;
      property Position: Integer read getPosition;

      function Count: integer;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      function InterpolateTime(Time: Single): Single;

      constructor Create(pPrev, pNext: anAnimationKeyFrameInstance; pHolder: anAnimationLOInstanceHolder;
        pObjectTo: anAnimationLayerObject; pAnimation: anAnimation; pTimeAt: Integer; pTransform: anTransform); overload;
      constructor Create(pPrev, pNext: anAnimationKeyFrameInstance; pHolder: anAnimationLOInstanceHolder;
        pObjectTo: anAnimationLayerObject; pAnimation: anAnimation); overload;
      destructor Destroy; override;
  end;

  { anAnimationLayerObjectInstance }

  anAnimationLayerObjectInstance = class ( anObjectWithData )
    private
      fAnimationInstance: anAnimationInstance;
      fObjectTo: anAnimationLayerObject;
      fHolder: anAnimationLOInstanceHolder;
      fTimeLine: anAnimationTimeLine;
      fTransform: anTransform;
      function getTransform: anTransform; virtual;
      procedure setTransform(AValue: anTransform); virtual;
    public
      property AnimationInstance: anAnimationInstance read fAnimationInstance write fAnimationInstance;
      property TimeLine: anAnimationTimeLine read fTimeLine write fTimeLine;
      property Holder: anAnimationLOInstanceHolder read fHolder write fHolder;
      property ObjectTo: anAnimationLayerObject read fObjectTo write fObjectTo;
      property Transform: anTransform read getTransform write setTransform;

      procedure Play; virtual; abstract;
      procedure Stop(Pause: Boolean); virtual; abstract;

      procedure Update(dt: Double); virtual; abstract;
      procedure Draw(pTransform: anTransform; Time: Single; FX: LongWord = FX_BLEND); virtual; abstract;

      constructor Create(pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject;
        pHolder: anAnimationLOInstanceHolder);
  end;

  { anAnimationLOInstanceHolder }

  anAnimationLOInstanceHolder = class ( TPersistent )
    private
      fLayerObject: anAnimationLayerObject;
      fAnimation: anAnimation;
      fParentHolder: anAnimationLOInstanceHolder;
      fFirstInstance, fLastInstance: anAnimationKeyFrameInstance;
      procedure CheckFrame(Frame: anPosition; Instance: anAnimationLayerObjectInstance);
      function getObjectInstanceAt(Time: Single): anAnimationKeyFrameInstance;
      function getObjectInstanceAtName(Name: anString): anAnimationKeyFrameInstance;
    public
      property Animation: anAnimation read fAnimation write fAnimation;
      property LayerObject: anAnimationLayerObject read fLayerObject write fLayerObject;

      property FirstInstance: anAnimationKeyFrameInstance read fFirstInstance write fFirstInstance;
      property LastInstance: anAnimationKeyFrameInstance read fLastInstance write fLastInstance;
      property InstanceAt[Time: Single]: anAnimationKeyFrameInstance read getObjectInstanceAt;
      property InstanceAtName[Name: anString]: anAnimationKeyFrameInstance read getObjectInstanceAtName;
      property ParentHolder: anAnimationLOInstanceHolder read fParentHolder write fParentHolder;

      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      procedure Clear;

      constructor Create(pLayerObject: anAnimationLayerObject; pAnimation: anAnimation);
      destructor Destroy; override;
  end;

  { anAnimationLayerObject }

  anAnimationLayerObject = class (anNamedObject)
    private
      fAnimation: anAnimationPrototype;
      fParent: anAnimationLayerObject;
      fVisible, fEnabled: boolean;
      fHolder: anAnimationLOInstanceHolder;
      function getIndex: Integer;
      procedure Clear;

      property Holder: anAnimationLOInstanceHolder read fHolder write fHolder;
    published
      property Visible: Boolean read fVisible write fVisible;
      property Enabled: Boolean read fEnabled write fEnabled;
    public
      property Parent: anAnimationLayerObject read fParent write fParent;
      property Animation: anAnimationPrototype read fAnimation write fAnimation;
      property Index: Integer read getIndex;
      function GetHolder: anAnimationLOInstanceHolder;

      function Draw(HolderFor: anAnimationLOInstanceHolder; Transform: anTransform; Time: Single): anDrawResult;
      function GetTransform(HolderAt: anAnimationLOInstanceHolder; AtTime: Single; WithParent: Boolean = true): anTransform;

      procedure RemoveParent(HolderFor, ParentHolder: anAnimationLOInstanceHolder);
      procedure AssignParent(Obj: anAnimationLayerObject; HolderFor, ParentHolder: anAnimationLOInstanceHolder);

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); virtual;
      procedure LoadFromRecord(rec: TDBRecord); virtual;
      procedure InitParentFromRecord(rec: TDBRecord);

      procedure Init(HolderFor, ParentHolder: anAnimationLOInstanceHolder);

      function GetInstance(pHolder: anAnimationLOInstanceHolder; pTransform: anTransform; AtTime: integer): anAnimationKeyFrameInstance;
      function GetObjectInstance(AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; virtual; abstract;
      function GetOtherProperties(pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty; virtual;

      constructor Create(pAnimation: anAnimationPrototype;
        pParent: anAnimationLayerObject = nil); virtual;

      destructor Destroy; override;
  end;

  anAnimationLayerObjectClass = class of anAnimationLayerObject;

  { anAnimationLayerSymbolInstance }

  anAnimationLayerSymbolInstance = class (anAnimationLayerObjectInstance)
    private
      fSwapContent: anTextureContent;
      fTextureData: Pointer;
      fState: anAnimationState;
    public
      property TextureData: Pointer read fTextureData write fTextureData;
      property SwapContent: anTextureContent read fSwapContent write fSwapContent;
      property State: anAnimationState read fState write fState;

      procedure Play; override;
      procedure Stop(Pause: Boolean); override;

      procedure Update(dt: Double); override;
      procedure Draw(pTransform: anTransform; Time: Single; FX: LongWord = FX_BLEND); override;

      constructor Create(pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject; pHolder: anAnimationLOInstanceHolder); virtual;
      destructor Destroy; override;
  end;

  { anAnimationLayerStaticSymbolInstance }

  anAnimationLayerStaticSymbolInstance = class (anAnimationLayerSymbolInstance)
    private
      fFrame: anPosition;
    public
      property Frame: anPosition read fFrame write fFrame;

      constructor Create(pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject; pHolder: anAnimationLOInstanceHolder); override;
  end;

  { anAnimationLayerAnimatedSymbolInstance }

  anAnimationLayerAnimatedSymbolInstance = class (anAnimationLayerSymbolInstance)
    private
      fTileSet: anAnimatedSpriteTextureTileSet;
      fDt: Single;
      fFrame: anPosition;
    public
      property TileSet: anAnimatedSpriteTextureTileSet read fTileSet write fTileSet;
      property DeltaTime: Single read fDt write fDt;
      property Frame: anPosition read fFrame write fFrame;

      procedure Update(dt: Double); override;
      procedure Stop(Pause: Boolean); override;

      constructor Create(pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject; pHolder: anAnimationLOInstanceHolder); override;
  end;

  { anAnimationLayerAnimationInstance }

  anAnimationLayerAnimationInstance = class (anAnimationLayerObjectInstance)
    private
      fInstance: anAnimationInstance;
      function getTransform: anTransform; override;
      procedure setTransform(AValue: anTransform); override;
    public
      property Instance: anAnimationInstance read fInstance write fInstance;

      procedure Play; override;
      procedure Stop(Pause: Boolean); override;

      procedure Update(dt: Double); override;
      procedure Draw(pTransform: anTransform; Time: Single; FX: LongWord = FX_BLEND); override;

      constructor Create(pIntanceTo: anAnimationInstance; pInstanceFrom: anAnimation; pObjectTo: anAnimationLayerObject;
        pHolder: anAnimationLOInstanceHolder);
      destructor Destroy; override;
  end;

  { anAnimationLayerPointInstance }

  anAnimationLayerPointInstance = class (anAnimationLayerObjectInstance)
    public
      procedure Play; override;
      procedure Stop(Pause: Boolean); override;

      procedure Update(dt: Double); override;
      procedure Draw(pTransform: anTransform; Time: Single; FX: LongWord = FX_BLEND); override;
  end;

  { anAnimationLayerLookUpPointInstance }

  anAnimationLayerLookUpPointInstance = class (anAnimationLayerPointInstance)
    private
      fLookUp: anAnimationLayerObjectInstance;
    public
      procedure Play; override;
      procedure Stop(Pause: Boolean); override;

      procedure Update(dt: Double); override;
      procedure Draw(pTransform: anTransform; Time: Single; FX: LongWord = FX_BLEND); override;

      property LookUp: anAnimationLayerObjectInstance read fLookUp write fLookUp;
      constructor Create(pIntanceTo: anAnimationInstance; pLookUp: anAnimationLayerObjectInstance;
        pObjectTo: anAnimationLayerObject; pHolder: anAnimationLOInstanceHolder);
  end;

  { anAnimationSymbolObject }

  anAnimationSymbolObject = class (anAnimationLayerObject)
    private
      fSymbol: anSymbol;
    public
      property Symbol: anSymbol read fSymbol write fSymbol;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      function GetObjectInstance(AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; override;
      function GetOtherProperties(pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty; override;

      constructor Create(pSymbol: anSymbol;
        pAnimation: anAnimationPrototype;
        pParent: anAnimationLayerObject = nil);
  end;

  { anAnimationPointObject }

  anAnimationPointObject = class;

  anOnDrawCallback = procedure (Sender: anAnimationPointObject;
    Instance: anAnimationLayerPointInstance; Data: anDrawResult) of object;
  anOnUpdateCallback = procedure (dt: Double; Sender: anAnimationPointObject;
    Instance: anAnimationLayerPointInstance) of object;

  anAnimationPointObject = class (anAnimationLayerObject)
    private
      fOnDraw: anOnDrawCallback;
      fOnUpdate: anOnUpdateCallback;
    public
      property OnDraw: anOnDrawCallback read fOnDraw write fOnDraw;
      property OnUpdate: anOnUpdateCallback read fOnUpdate write fOnUpdate;

      function GetObjectInstance(AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; override;

      constructor Create(pAnimation: anAnimationPrototype;
        pParent: anAnimationLayerObject = nil); override;
  end;

  { anAnimationLookUpPointObject }

  anAnimationLookUpPointObject = class (anAnimationPointObject)
    private
      fLookUp: anAnimationLayerObject;
    published
      property LookUp: anAnimationLayerObject read fLookUp write fLookUp;
    public
      function GetObjectInstance(AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; override;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;

      constructor Create(pAnimation: anAnimationPrototype;
        pParent: anAnimationLayerObject = nil); override;
  end;

  { anAnimationAnimationObject }

  anAnimationAnimationObject = class (anAnimationLayerObject)
    private
      fDataAnimation: anAnimation;
    public
      property DataAnimation: anAnimation read fDataAnimation write fDataAnimation;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;
      function GetOtherProperties(pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty; override;

      function GetObjectInstance(AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance; override;

      constructor Create(pDataAnimation: anAnimation;
        pAnimation: anAnimationPrototype;
        pParent: anAnimationLayerObject = nil); virtual;
  end;

  anAnimationObjectMap = TFPGMap<anString, anAnimationLayerObject>;

  anAnimationTreeList = TFPGList<anAnimationInstanceTree>;

  { anAnimationInstanceTree }

  anAnimationInstanceTree = class ( anObjectWithData )
    private
      fObjectTo: anAnimationLayerObjectInstance;
      fInstance: anAnimationInstance;
      fIndex: Integer;
      fChilds: anAnimationTreeList;
    public
      property Instance: anAnimationInstance read fInstance write fInstance;
      property Index: Integer read fIndex write fIndex;
      property ObjectTo: anAnimationLayerObjectInstance read fObjectTo write fObjectTo;
      property Childs: anAnimationTreeList read fChilds write fChilds;

      procedure Update(ParentTransform: anTransform; dt: Double);

      constructor Create(pInstance: anAnimationInstance);
      destructor Destroy; override;
  end;

  { anAnimationInstance }

  anAnimationObjectInstancesList = array of anAnimationLayerObjectInstance;
  anBlendersList = TFPGList<anAnimationTimeLine>;

  anAnimationInstance = class ( anObjectWithData )
   private
      Data: anTransform;

      fAnimation: anAnimation;
      fTransform: anTransformClass;
      fInstanceTree: anAnimationInstanceTree;
      fInstances: anAnimationObjectInstancesList;
      fTimeLine: anAnimationTimeLine;
      fBlenders: anBlendersList;

      function getCurrentFrame: anPosition;
      function getInstancesCount: Integer;
      function getOffsetTime: Double;
      procedure PlayObjects;
      procedure StopObjects(Pause: Boolean);
      procedure ApplyBlender(Blender: anAnimationTimeLine);
      procedure CancelBlender(Blender: anAnimationTimeLine);
    public
      property Instances: anAnimationObjectInstancesList read fInstances write fInstances;
      property InstancesCount: Integer read getInstancesCount;
      property Transform: anTransformClass read fTransform write fTransform;
      property InstanceTree: anAnimationInstanceTree read fInstanceTree write fInstanceTree;
      property Animation: anAnimation read fAnimation write fAnimation;
      property TimeLine: anAnimationTimeLine read fTimeLine write fTimeLine;
      property CurrentFrame: anPosition read getCurrentFrame;
      property OffsetTime: Double read getOffsetTime;
      property Blenders: anBlendersList read fBlenders write fBlenders;

      function GetInstanceObject(IOName: anString): anAnimationLayerObjectInstance;
      function GetInstanceObjectByLO(LayerObject: anAnimationLayerObject): anAnimationLayerObjectInstance;

      function AddBlender(Blender: anAnimationBlender): anAnimationTimeLine; overload;
      function AddBlender(BlenderName: anString): anAnimationTimeLine; overload;
      procedure RemoveBlender(Blender: anAnimationTimeLine; FreeAfter: Boolean = true);

      procedure Stop(Pause: Boolean = true);
      procedure Play;
      procedure GotoAndStop(Frame: anPosition; Pause: Boolean = true); overload;
      procedure GotoAndStop(Frame: anString; Pause: Boolean = true); overload;
      procedure GotoAndPlay(Frame: anPosition); overload;
      procedure GotoAndPlay(Frame: anString); overload;

      procedure Draw(FX: LongWord = FX_BLEND);
      procedure Update(dt: double);

      constructor Create(pAnimation: anAnimation);
      destructor Destroy; override;
  end;

  anAnimationCallbacksList = TFPGMap<anString, anOnActivateCallback>;
  anAnimationBlendersList = TFPGList<anAnimationBlender>;

  { anAnimation }

  anAnimation = class ( anNamedObject )
    private
      fFPS: Integer;
      fAnimation: anAnimationSet;
      fFramesCount: anPosition;
      fRepeatAnimation: boolean;
      fCycled: boolean;
      fCallbacks: anAnimationCallbacksList;

      function getFPS: integer;

      function getObjectsCount: integer; virtual; abstract;
      function getObjectByIndex(Index: integer): anAnimationLayerObject; virtual; abstract;
      function getHolderByIndex(Index: integer): anAnimationLOInstanceHolder; virtual; abstract;
      function getObjectIndexOf(obj: anAnimationLayerObject): Integer; virtual; abstract;
      function getObjectIndexOfByName(objName: anString): Integer; virtual; abstract;
      function getPrototype: anAnimationPrototype; virtual;
      procedure setPrototype(AValue: anAnimationPrototype); virtual;

      procedure RemoveParent(HolderFor, ParentHolder: anAnimationLOInstanceHolder); virtual;
      procedure AssignParent(HolderFor, ParentHolder: anAnimationLOInstanceHolder); virtual;
    published
      property Prototype: anAnimationPrototype read getPrototype write setPrototype;
      property FPS: integer read getFPS write fFPS;
      property Animation: anAnimationSet read fAnimation write fAnimation;
      property FramesCount: anPosition read fFramesCount write fFramesCount;
      property RepeatAnimation: Boolean read fRepeatAnimation write fRepeatAnimation;
      property Cycled: Boolean read fCycled write fCycled;
    public
      property _FPS: integer read fFPS;

      property Callbacks: anAnimationCallbacksList read fCallbacks write fCallbacks;

      property ObjectsCount: integer read getObjectsCount;
      property ObjectByIndex[index: integer]: anAnimationLayerObject read getObjectByIndex;
      property HolderByIndex[index: integer]: anAnimationLOInstanceHolder read getHolderByIndex;
      property IndexOf[obj: anAnimationLayerObject]: Integer read getObjectIndexOf;
      property IndexOfName[objName: anString]: Integer read getObjectIndexOfByName;

      procedure RegisterCallback(CallbackName: anString; CallbackData: anOnActivateCallback);

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); virtual;
      procedure LoadFromRecord(rec: TDBRecord); virtual;
      procedure PostLoadFromRecord(rec: TDBRecord); virtual; abstract;

      function GetInstance: anAnimationInstance; virtual;
      procedure FillTree(Tree: anAnimationInstanceTree; ObjectTo: anAnimationLayerObject; Instance: anAnimationInstance);

      constructor Create(pAnimation: anAnimationSet); virtual;
      destructor Destroy; override;
  end;

  { anAnimationPrototype }

  anAnimationPrototype = class( anAnimation )
    private
      fObjects: anAnimationObjectMap;
      fBlenders: anAnimationBlendersList;

      function getObjectsCount: integer; override;
      function getObjectByIndex(Index: integer): anAnimationLayerObject; override;
      function getHolderByIndex(Index: integer): anAnimationLOInstanceHolder; override;
      function getObjectIndexOf(obj: anAnimationLayerObject): Integer; override;
      function getObjectIndexOfByName(objName: anString): Integer; override;

      procedure registerBlender(blender: anAnimationBlender);
      procedure unregisterBlender(blender: anAnimationBlender);

      procedure RemoveParent(HolderFor, ParentHolder: anAnimationLOInstanceHolder); override;
      procedure AssignParent(HolderFor, ParentHolder: anAnimationLOInstanceHolder); override;
    public
      property Objects: anAnimationObjectMap read fObjects write fObjects;
      property Blenders: anAnimationBlendersList read fBlenders write fBlenders;

      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;
      procedure PostLoadFromRecord(rec: TDBRecord); override;

      function AddObject(Obj: anAnimationLayerObject; pName: anString): anAnimationLayerObject;
      procedure RemoveObject(Obj: anAnimationLayerObject; FreeAfter: Boolean = true);
      procedure MoveObject(Obj: anAnimationLayerObject; index: integer);

      function GetInstance: anAnimationInstance; override;
      constructor Create(pAnimation: anAnimationSet); override;
      destructor Destroy; override;
  end;

  { anBlenderObjectProxy }

  anBlenderObjectProxy = class ( TPersistent )
    private
      fBlender: anAnimationBlender;
      fProxyTo: anAnimationLayerObject;
      fHolder: anAnimationLOInstanceHolder;
      fApply: Boolean;
    public
      property Apply: Boolean read fApply write fApply;
      property ProxyTo: anAnimationLayerObject read fProxyTo write fProxyTo;
      property Blender: anAnimationBlender read fBlender write fBlender;
      property Holder: anAnimationLOInstanceHolder read fHolder write fHolder;

      function Exists: boolean;

      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      procedure Init(pParentHolder: anAnimationLOInstanceHolder);
      constructor Create(pBlender: anAnimationBlender; pProxyTo: anAnimationLayerObject);
      destructor Destroy; override;
  end;

  anBlenderObjectProxyList = TFPGList<anBlenderObjectProxy>;

  { anAnimationBlender }

  anAnimationBlender = class (anAnimation)
    private
      fPrototype: anAnimationPrototype;
      fProxy: anBlenderObjectProxyList;
      fAutoPlay: Boolean;

      function getObjectsCount: integer; override;
      function getObjectByIndex(Index: integer): anAnimationLayerObject; override;
      function getHolderByIndex(Index: integer): anAnimationLOInstanceHolder; override;
      function getObjectIndexOf(obj: anAnimationLayerObject): Integer; override;
      function getObjectIndexOfByName(objName: anString): Integer; override;

      function getProxyFor(obj: anAnimationLayerObject; list: anBlenderObjectProxyList): anBlenderObjectProxy;

      function getPrototype: anAnimationPrototype; override;
      procedure setPrototype(AValue: anAnimationPrototype); override;
    published
      property AutoPlay: Boolean read fAutoPlay write fAutoPlay;
    public
      property Proxy: anBlenderObjectProxyList read fProxy write fProxy;

      procedure SaveToRecord(rec: TDBRecord); override;
      procedure LoadFromRecord(rec: TDBRecord); override;
      procedure PostLoadFromRecord(rec: TDBRecord); override;

      procedure Update;

      function GetInstance: anAnimationInstance; override;
      constructor Create(pAnimation: anAnimationSet); override;
      destructor Destroy; override;
  end;

  anAnimationClass = class of anAnimation;

  { anSymbol }

  anSymbol = class (anNamedObject)
    private
      fAnimationLibrary: anLibrary;
      fTexture: anTexture;
      fTextureParams: anTextureParams;
      fRotation: Single;
      fRotatingPivot: zglTPoint2D;

      function getPivotX: Single;
      function getPivotY: Single;
      procedure setPivotX(AValue: Single);
      procedure setPivotY(AValue: Single);
      procedure setTexture(AValue: anTexture);
    published
      property Texture: anTexture read fTexture write setTexture;
      property TextureParams: anTextureParams read fTextureParams write fTextureParams;
      property Rotation: Single read fRotation write fRotation;
      property PivotX: Single read getPivotX write setPivotX;
      property PivotY: Single read getPivotY write setPivotY;
    public
      property AnimationLibrary: anLibrary read fAnimationLibrary write fAnimationLibrary;

      procedure RemoveTexture;
      procedure AssignTo(Dest: TPersistent); override;
      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);

      procedure Draw(Transform: anTransform; pContent: zglPTexture);

      constructor Create(pLibrary: anLibrary);
      destructor Destroy; override;
  end;

  { anLibrary }

  asSymbolsList = TFPGMap<anString, anSymbol>;

  anLibrary = class (anObjectWithData)
    private
      fAnimation: anAnimationSet;
      fSymbols: asSymbolsList;

      fOnNameChange: anOnNameChange;
    public
      property Animation: anAnimationSet read fAnimation write fAnimation;
      property Symbols: asSymbolsList read fSymbols write fSymbols;
      property OnNameChange: anOnNameChange read fOnNameChange write fOnNameChange;

      procedure SaveToRecord(rec: TDBRecord);
      procedure LoadFromRecord(rec: TDBRecord);
      procedure Clear;

      function AddSymbol(Name: anString): anSymbol; overload;
      function AddSymbol(Symbol: anSymbol; Name: anString): anSymbol; overload;

      function CloneSymbol(CloneFrom: anString): anSymbol;

      procedure DeleteSymbol(Name: anString; FreeAfter: boolean = true); overload;
      procedure DeleteSymbol(Symbol: anSymbol; FreeAfter: boolean = true); overload;

      constructor Create(pAnimation: anAnimationSet);
      destructor Destroy; override;
  end;

  { anAnimationSet }

  asTexturesList = TFPGMap<anString, anTexture>;
  asAnimationsList = TFPGMap<anString, anAnimation>;
  asContentList = TFPGMap<anString, anTextureContent>;

  anAnimationSet = class ( anObjectWithData )
    private
      fFPS: Integer;
      fAnimationLibrary: anLibrary;
      fTextures: asTexturesList;
      fAnimations: asAnimationsList;
      fContents: asContentList;

      fOnTextureNameChange: anOnNameChange;
      fOnAnimationNameChange: anOnNameChange;
      fOnContentNameChange: anOnNameChange;
    published
      property FPS: integer read fFPS write fFPS;
      property AnimationLibrary: anLibrary read fAnimationLibrary write fAnimationLibrary;
    public

      function AddTexture(Name: anString; ClassOf: anTextureClass): anTexture; overload;
      function AddTexture(Name: anString; Texture: anTexture): anTexture; overload;
      function CloneTexture(CloneFrom: anString): anTexture;
      procedure DeleteTexture(Name: anString; FreeAfter: boolean = true); overload;
      procedure DeleteTexture(Texture: anTexture; FreeAfter: boolean = true); overload;

      function AddAnimation(Name: anString; ClassOf: anAnimationClass): anAnimation; overload;
      function AddAnimation(Animation: anAnimation; Name: anString): anAnimation; overload;
      function CloneAnimation(CloneFrom: anString; ClassOf: anAnimationClass): anAnimation;
      procedure DeleteAnimation(Name: anString; FreeAfter: boolean = true); overload;
      procedure DeleteAnimation(Animation: anAnimation; FreeAfter: boolean = true); overload;

      function AddContent(Name: anString; ClassOf: anTextureContentClass): anTextureContent; overload;
      function AddContent(Content: anTextureContent; Name: anString): anTextureContent; overload;
      procedure DeleteContent(Name: anString; FreeAfter: boolean = true); overload;
      procedure DeleteContent(Content: anTextureContent; FreeAfter: boolean = true); overload;

      procedure SaveToFile(Name: anString; FileClass: TFileHandlerClass);
      procedure SaveToRecord(Rec: TDBRecord);

      function LoadFromFile(Name: anString; FileClass: TFileHandlerClass): Boolean;
      {$IFDEF HANDLER_BUFFER}
      function LoadFromStream(Stream: TMemoryStream): Boolean;
      {$ENDIF}
      function LoadFromRecord(Rec: TDBRecord): Boolean;

      procedure Clear;

      property Contents: asContentList read fContents write fContents;
      property Textures: asTexturesList read fTextures write fTextures;
      property Animations: asAnimationsList read fAnimations write fAnimations;
      property OnTextureNameChange: anOnNameChange read fOnTextureNameChange write fOnTextureNameChange;
      property OnAnimationNameChange: anOnNameChange read fOnAnimationNameChange write fOnAnimationNameChange;
      property OnContentNameChange: anOnNameChange read fOnContentNameChange write fOnContentNameChange;

      function GetAnimationInstance(AnimationName: anString): anAnimationInstance;

      constructor Create;
      destructor Destroy; override;
  end;

implementation

{ anAnimationLayerAnimatedSymbolInstance }

procedure anAnimationLayerAnimatedSymbolInstance.Update(dt: Double);
begin
  inherited Update(dt);
  if not Assigned(TileSet) then exit;
  if TileSet.TileList.Count = 0 then exit;
  if State = asStopped then exit;
  DeltaTime := DeltaTime + dt;
  if DeltaTime >= TileSet.TileList[Frame].Time then begin
    Frame := Frame + 1;
    if Frame >= TileSet.TileList.Count then
      Frame := 0;
    DeltaTime := 0;
  end;
end;

procedure anAnimationLayerAnimatedSymbolInstance.Stop(Pause: Boolean);
begin
  inherited Stop(Pause);
  if not Pause then begin
    DeltaTime := 0;
    Frame := 0;
  end;
end;

constructor anAnimationLayerAnimatedSymbolInstance.Create(
  pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject;
  pHolder: anAnimationLOInstanceHolder);
begin
  inherited Create(pIntanceTo, pObjectTo, pHolder);
  Frame := 0;
  DeltaTime := 0;
  TileSet := nil;
end;

{ anAnimationLayerStaticSymbolInstance }

constructor anAnimationLayerStaticSymbolInstance.Create(
  pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject;
  pHolder: anAnimationLOInstanceHolder);
begin
  inherited Create(pIntanceTo, pObjectTo, pHolder);
  Frame := 0;
end;

{ anAnimatedSpriteTextureTile }

procedure anAnimatedSpriteTextureTile.SaveToRecord(rec: TDBRecord);
begin
  rec.ChildByName['f'].AsSingle := Time;
  rec.ChildByName['t'].AsInteger := Tile;
end;

procedure anAnimatedSpriteTextureTile.LoadFromRecord(rec: TDBRecord);
begin
  Time := rec.ChildByName['f'].AsSingle;
  Tile := rec.ChildByName['t'].AsInteger;
end;

constructor anAnimatedSpriteTextureTile.Create;
begin
  Tile := 0;
  Time := 1;
end;

{ anAnimatedSpriteTextureTileSet }

procedure anAnimatedSpriteTextureTileSet.SaveToRecord(rec: TDBRecord);
var
  i: Integer;
begin
  rec.Name := Name;
  rec.ChildCount := TileList.Count;
  for i := 0 to TileList.Count - 1 do
    TileList[i].SaveToRecord(rec.Child[i]);
end;

procedure anAnimatedSpriteTextureTileSet.LoadFromRecord(rec: TDBRecord);
var tile: anAnimatedSpriteTextureTile;
  i: Integer;
begin
  for i := 0 to rec.ChildCount - 1 do begin
    tile := anAnimatedSpriteTextureTile.Create;
    tile.LoadFromRecord(rec.Child[i]);
    TileList.Add(tile);
  end;
end;

constructor anAnimatedSpriteTextureTileSet.Create(
  pTexture: anAnimatedSpriteTexture);
begin
  Texture := pTexture;
  inherited Create(Texture.TileSets);
  TileList := anAnimatedSpriteTextureTileList.Create;
end;

destructor anAnimatedSpriteTextureTileSet.Destroy;
var
  i: Integer;
begin
  for i := 0 to TileList.Count - 1 do
    TileList[i].Free;
  TileList.Free;
  inherited Destroy;
end;

{ anAnimationActioner }

procedure anAnimationActioner.SaveToRecord(rec: TDBRecord);
begin
  if Action in [apaGotoAndPlay, apaGotoAndStop] then
    rec.ChildByName['f'].AsString := FrameTo;
  rec.ChildByName['a'].AsByte := Byte(Action);
end;

procedure anAnimationActioner.LoadFromRecord(rec: TDBRecord);
begin
  Action := anAnimationAction(rec.ChildByName['a'].AsByte);
  if Action in [apaGotoAndPlay, apaGotoAndStop] then
    FrameTo := rec.ChildByName['f'].AsString;
end;

procedure anAnimationActioner.Activate(InstanceTo: anAnimationInstance);
begin
  case Action of
    apaPlay: InstanceTo.Play;
    apaStop: InstanceTo.Stop(true);
    apaGotoAndStop: InstanceTo.GotoAndStop(FrameTo, true);
    apaGotoAndPlay: InstanceTo.GotoAndPlay(FrameTo);
  end;
end;

constructor anAnimationActioner.Create;
begin
  Action := apaNone;
  FrameTo := '';
end;

{ anOtherAnimationSpriteProperty }

procedure anOtherAnimationSpriteProperty.OnActivate(
  Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance);
var
  inst: anAnimationLayerAnimatedSymbolInstance;
begin
  inherited OnActivate(Obj, KeyFrom);
  inst := anAnimationLayerAnimatedSymbolInstance(Obj);
  case TileSetAction of
    tsaChangeTileSetAndStop: begin
      inst.TileSet := TileSetTo;
      inst.Stop(true);
    end;
    tsaChangeTileSetAndPlay: begin
      inst.TileSet := TileSetTo;
      inst.Play;
    end;
    tsaPlay: begin
      inst.Play;
    end;
    tsaStop: begin
      inst.Stop(true);
    end;
  end;
end;

procedure anOtherAnimationSpriteProperty.OnFrame(
  Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance;
  Frame: anPosition);
begin
  inherited OnFrame(Obj, KeyFrom, Frame);
end;

procedure anOtherAnimationSpriteProperty.SaveToRecord(rec: TDBRecord);
begin
  inherited SaveToRecord(rec);
  rec.ChildByName['a'].AsByte := Byte(TileSetAction);
  if Assigned(TileSetTo) then
    rec.ChildByName['t'].AsString := TileSetTo.Name
  else
    rec.ChildByName['t'].AsString := '';
end;

procedure anOtherAnimationSpriteProperty.LoadFromRecord(rec: TDBRecord);
var
  ts_name: String;
  inst: anAnimationLayerSymbolInstance;
  inst_sym: anAnimationSymbolObject;
  inst_tex: anAnimatedSpriteTexture;
  idx: LongInt;
begin
  inherited LoadFromRecord(rec);
  TileSetAction := anAnimationTileSetAction(rec.ChildByName['a'].AsByte);
  ts_name := rec.ChildByName['t'].AsString;
  if ts_name <> '' then begin
    inst_sym := anAnimationSymbolObject(KeyFrame.ObjectTo);
    inst_tex := anAnimatedSpriteTexture(inst_sym.Symbol.Texture);
    if Assigned(inst_tex) then begin
      idx := inst_tex.TileSets.IndexOf(ts_name);
      if idx >= 0 then
        TileSetTo := inst_tex.TileSets.Data[idx];
    end;
  end;
end;

constructor anOtherAnimationSpriteProperty.Create(pAnimation: anAnimation;
  pKeyFrame: anAnimationKeyFrameInstance);
begin
  inherited Create(pAnimation, pKeyFrame);
  TileSetAction := tsaNone;
end;

{ anOtherStaticSpriteProperty }

procedure anOtherStaticSpriteProperty.OnActivate(Obj: anAnimationLayerObjectInstance;
  KeyFrom: anAnimationKeyFrameInstance);
var
  inst: anAnimationLayerStaticSymbolInstance;
begin
  inherited OnActivate(Obj, KeyFrom);

  inst := anAnimationLayerStaticSymbolInstance(Obj);

  case Action of
    spaSetFrame: inst.Frame := FrameTo;
  end;
end;

procedure anOtherStaticSpriteProperty.OnFrame(Obj: anAnimationLayerObjectInstance;
  KeyFrom: anAnimationKeyFrameInstance; Frame: anPosition);
begin
  inherited OnFrame(Obj, KeyFrom, Frame);


end;

procedure anOtherStaticSpriteProperty.SaveToRecord(rec: TDBRecord);
begin
  inherited;
  if Action in [spaSetFrame] then
    rec.ChildByName['f'].AsInteger := FrameTo;
  rec.ChildByName['a'].AsByte := Byte(Action);
end;

procedure anOtherStaticSpriteProperty.LoadFromRecord(rec: TDBRecord);
begin
  inherited;
  Action := anOtherStaticSpritePropertyAction(rec.ChildByName['a'].AsByte);
  if Action in [spaSetFrame] then
    FrameTo := rec.ChildByName['f'].AsInteger;
end;

constructor anOtherStaticSpriteProperty.Create(pAnimation: anAnimation;
  pKeyFrame: anAnimationKeyFrameInstance);
begin
  inherited Create(pAnimation, pKeyFrame);
end;

{ anOtherKFProperty }

procedure anOtherKFProperty.OnActivate(Obj: anAnimationLayerObjectInstance;
  KeyFrom: anAnimationKeyFrameInstance);
var
  idx: LongInt;
begin
  ThisAnimationAction.Activate(Obj.AnimationInstance);
  if OnActivateCallback = '' then exit;
  idx := Animation.Callbacks.IndexOf(OnActivateCallback);
  if idx >= 0 then begin
    Animation.Callbacks.Data[idx](Obj, KeyFrom, Self);;
  end;
end;

procedure anOtherKFProperty.OnFrame(Obj: anAnimationLayerObjectInstance;
  KeyFrom: anAnimationKeyFrameInstance; Frame: anPosition);
var
  idx: LongInt;
begin
  if OnFrameCallback = '' then exit;
  idx := Animation.Callbacks.IndexOf(OnFrameCallback);
  if idx >= 0 then begin
    Animation.Callbacks.Data[idx](Obj, KeyFrom, Self);;
  end;
end;

procedure anOtherKFProperty.SaveToRecord(rec: TDBRecord);
begin
  rec.ChildByName['c'].AsString := OnActivateCallback;
  rec.ChildByName['f'].AsString := OnFrameCallback;
  ThisAnimationAction.SaveToRecord(rec.ChildByName['this_anim']);
end;

procedure anOtherKFProperty.LoadFromRecord(rec: TDBRecord);
begin
  OnActivateCallback := rec.ChildByName['c'].AsString;
  OnFrameCallback := rec.ChildByName['f'].AsString;
  ThisAnimationAction.LoadFromRecord(rec.ChildByName['this_anim']);
end;

constructor anOtherKFProperty.Create(pAnimation: anAnimation;
  pKeyFrame: anAnimationKeyFrameInstance);
begin
  Animation := pAnimation;
  KeyFrame := pKeyFrame;
  OnActivateCallback := '';
  OnFrameCallback := '';
  ThisAnimationAction := anAnimationActioner.Create;
end;

destructor anOtherKFProperty.Destroy;
begin
  ThisAnimationAction.Destroy;
  inherited Destroy;
end;

{ anOtherAnimationProperty }

procedure anOtherAnimationProperty.OnActivate(
  Obj: anAnimationLayerObjectInstance; KeyFrom: anAnimationKeyFrameInstance);
var
  Anim: anAnimationLayerAnimationInstance;
begin
  inherited OnActivate(Obj, KeyFrom);
  Anim := anAnimationLayerAnimationInstance(Obj);
  ChildAnimationAction.Activate(Anim.Instance);
end;

procedure anOtherAnimationProperty.SaveToRecord(rec: TDBRecord);
begin
  inherited;
  ChildAnimationAction.SaveToRecord(rec.ChildByName['child_anim']);
end;

procedure anOtherAnimationProperty.LoadFromRecord(rec: TDBRecord);
begin
  inherited;
  ChildAnimationAction.LoadFromRecord(rec.ChildByName['child_anim']);
end;

constructor anOtherAnimationProperty.Create(pAnimation: anAnimation;
  pKeyFrame: anAnimationKeyFrameInstance);
begin
  ChildAnimationAction := anAnimationActioner.Create;
  inherited Create(pAnimation, pKeyFrame);
end;

destructor anOtherAnimationProperty.Destroy;
begin
  ChildAnimationAction.Free;
  inherited Destroy;
end;

{ anBlenderObjectProxy }

function anBlenderObjectProxy.Exists: boolean;
begin
  Result := Blender.Prototype.Objects.IndexOfData(ProxyTo) >= 0;
end;

procedure anBlenderObjectProxy.SaveToRecord(rec: TDBRecord);
begin
  rec.Name := ProxyTo.Name;
  rec.ChildByName['a'].AsBoolean := Apply;
  Holder.SaveToRecord(rec.ChildByName['h']);
end;

procedure anBlenderObjectProxy.LoadFromRecord(rec: TDBRecord);
begin
  Apply := rec.ChildByName['a'].AsBoolean;
  Holder.LoadFromRecord(rec.ChildByName['h']);
end;

constructor anBlenderObjectProxy.Create(pBlender: anAnimationBlender;
  pProxyTo: anAnimationLayerObject);
begin
  Blender := pBlender;
  Apply := true;
  ProxyTo := pProxyTo;
  Holder := anAnimationLOInstanceHolder.Create(ProxyTo, Blender);
end;

procedure anBlenderObjectProxy.Init(pParentHolder: anAnimationLOInstanceHolder);
begin
  Holder.ParentHolder := pParentHolder;
end;

destructor anBlenderObjectProxy.Destroy;
begin
  Holder.Free;
  inherited Destroy;
end;

{ anAnimationLOInstanceHolder }

procedure anAnimationLOInstanceHolder.CheckFrame(Frame: anPosition;
  Instance: anAnimationLayerObjectInstance);
var
  inst: anAnimationKeyFrameInstance;
begin
  inst := InstanceAt[Frame];
  if inst.Position = Frame then begin
    if Assigned(inst.ActiveProperties) then
      inst.ActiveProperties.OnActivate(Instance, Inst);
  end;
  inst.ActiveProperties.OnFrame(Instance, Inst, Frame - Inst.Position);
end;

function anAnimationLOInstanceHolder.getObjectInstanceAt(Time: Single
  ): anAnimationKeyFrameInstance;
var
  inst: anAnimationKeyFrameInstance;
begin
  inst := LastInstance;
  while Assigned(Inst) do begin
    if (Inst.Position <= Time) then
       Exit(inst);
    inst := inst.Prev;
  end;
  Result := FirstInstance;
end;

function anAnimationLOInstanceHolder.getObjectInstanceAtName(Name: anString
  ): anAnimationKeyFrameInstance;
var
  inst: anAnimationKeyFrameInstance;
begin
  inst := LastInstance;
  while Assigned(Inst) do begin
    if (Inst.Name <= Name) then
       Exit(inst);
    inst := inst.Prev;
  end;
  Result := nil;
end;

procedure anAnimationLOInstanceHolder.SaveToRecord(rec: TDBRecord);
var
  item: anAnimationKeyFrameInstance;
  i: Integer;
begin
  rec.ChildCount := FirstInstance.Count;
  item := FirstInstance;
  i := 0;
  while Assigned(item) do begin
    item.SaveToRecord(rec.Child[i]);
    inc(i);
    item := item.Next;
  end;
end;

procedure anAnimationLOInstanceHolder.LoadFromRecord(rec: TDBRecord);
var
  i: Integer;
  inst: anAnimationKeyFrameInstance;
begin
  for i := 0 to rec.ChildCount - 1 do begin
    inst := anAnimationKeyFrameInstance.Create(LastInstance, nil, Self, LayerObject, Animation);
    inst.LoadFromRecord(rec.Child[i]);
  end;
end;

procedure anAnimationLOInstanceHolder.Clear;
begin
  while Assigned(FirstInstance) do begin
    FirstInstance.Free;
  end;
  FirstInstance := nil;
  LastInstance := nil;
end;

constructor anAnimationLOInstanceHolder.Create(
  pLayerObject: anAnimationLayerObject; pAnimation: anAnimation);
begin
  LayerObject := pLayerObject;
  Animation := pAnimation;
  ParentHolder := nil;
  FirstInstance := nil;
  LastInstance := nil;
end;

destructor anAnimationLOInstanceHolder.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ anAnimationBlender }

function anAnimationBlender.getObjectsCount: integer;
begin
  if not Assigned(Prototype) then exit(0);
  Result := Proxy.Count;
end;

function anAnimationBlender.getObjectByIndex(Index: integer
  ): anAnimationLayerObject;
begin
  if not Assigned(Prototype) then exit(nil);
  Result := Proxy[Index].ProxyTo;
end;

function anAnimationBlender.getHolderByIndex(Index: integer
  ): anAnimationLOInstanceHolder;
begin
  if not Assigned(Prototype) then exit(nil);
  Result := Proxy[Index].Holder;
end;

function anAnimationBlender.getObjectIndexOf(obj: anAnimationLayerObject
  ): Integer;
begin
  if not Assigned(Prototype) then exit(-1);
  Result := Prototype.IndexOf[obj];
end;

function anAnimationBlender.getObjectIndexOfByName(objName: anString
  ): Integer;
begin
  if not Assigned(Prototype) then exit(-1);
  Result := Prototype.IndexOfName[objName];
end;

function anAnimationBlender.getProxyFor(obj: anAnimationLayerObject;
  list: anBlenderObjectProxyList): anBlenderObjectProxy;
var
  i: Integer;
begin
  for i := 0 to list.Count - 1 do
    if list[i].ProxyTo = obj then exit(list[i]);
  Result := nil;
end;

function anAnimationBlender.getPrototype: anAnimationPrototype;
begin
  Result := fPrototype;
end;

procedure anAnimationBlender.setPrototype(AValue: anAnimationPrototype);
begin
  if fPrototype = AValue then exit;
  if Assigned(fPrototype) then
     fPrototype.unregisterBlender(self);
  fPrototype := AValue;
  if Assigned(fPrototype) then
     fPrototype.registerBlender(self);
end;

procedure anAnimationBlender.SaveToRecord(rec: TDBRecord);
var
  ch: TDBRecord;
  i: Integer;
begin
  inherited SaveToRecord(rec);
  rec.ChildByName['autoplay'].AsBoolean := AutoPlay;
  if Assigned(Prototype) then
     rec.ChildByName['prototype'].AsString := Prototype.Name;
  ch := rec.ChildByName['objects'];
  ch.ChildCount := Proxy.Count;
  for i := 0 to Proxy.Count - 1 do
    Proxy[i].SaveToRecord(ch.Child[i]);
end;

procedure anAnimationBlender.LoadFromRecord(rec: TDBRecord);
begin
  inherited LoadFromRecord(rec);
  AutoPlay := rec.ChildByName['autoplay'].AsBoolean;
end;

procedure anAnimationBlender.PostLoadFromRecord(rec: TDBRecord);
var
  ch: TDBRecord;
  i: Integer;
  prx: anBlenderObjectProxy;
begin
  if rec.ChildExists['prototype'] then begin
    Prototype := Animation.Animations[rec.ChildByName['prototype'].AsString] as anAnimationPrototype;
    if Assigned(Prototype) then begin
      ch := rec.ChildByName['objects'];
      for i := 0 to ch.ChildCount - 1 do begin
        prx := anBlenderObjectProxy.Create(Self, Prototype.Objects[ch.Child[i].Name]);
        prx.LoadFromRecord(ch.Child[i]);
        Proxy.Add(prx);
      end;

      for i := 0 to Proxy.Count - 1 do begin
        prx := Proxy[i];
        if Assigned(prx.ProxyTo.Parent) then begin
          prx.Init(HolderByIndex[prx.ProxyTo.Parent.Index]);
        end;
      end;
      Update;
    end;
  end;
end;

procedure anAnimationBlender.Update;
var
  i: Integer;
  proxys: anBlenderObjectProxyList;
  obj: anAnimationLayerObject;
  prx: anBlenderObjectProxy;
begin
  // remove unused proxys
  for i := Proxy.Count - 1 downto 0 do begin
    if not Proxy[i].Exists then begin
      Proxy[i].Free;
      Proxy.Delete(i);
    end;
  end;
  proxys := anBlenderObjectProxyList.Create;
  for i := 0 to Proxy.Count - 1 do
    proxys.Add(Proxy[i]);

  Proxy.Clear;

  if Assigned(Prototype) then begin
    for i := 0 to Prototype.ObjectsCount - 1 do begin
      obj := Prototype.ObjectByIndex[i];
      prx := getProxyFor(obj, proxys);
      if not Assigned(prx) then begin
        prx := anBlenderObjectProxy.Create(Self, obj);
      end;
      Proxy.Add(prx);
    end;
    proxys.Clear;
  end;

  for i := 0 to Proxy.Count - 1 do begin
    prx := Proxy[i];
    if Assigned(prx.ProxyTo.Parent) then begin
      prx.Init(HolderByIndex[prx.ProxyTo.Parent.Index]);
    end;
  end;

  for i := 0 to proxys.Count - 1 do
    proxys[i].Free;
  proxys.Free;
end;

function anAnimationBlender.GetInstance: anAnimationInstance;
begin
  Result := anAnimationInstance.Create(Self);
end;

constructor anAnimationBlender.Create(pAnimation: anAnimationSet);
begin
  inherited Create(pAnimation);
  Proxy := anBlenderObjectProxyList.Create;
  AutoPlay := true;
end;

destructor anAnimationBlender.Destroy;
var
  i: Integer;
begin
  for i := 0 to Proxy.Count - 1 do
    Proxy[i].Free;
  Proxy.Free;
  inherited Destroy;
end;

{ anAnimationLayerLookUpPointInstance }

procedure anAnimationLayerLookUpPointInstance.Play;
begin
  //
end;

procedure anAnimationLayerLookUpPointInstance.Stop(Pause: Boolean);
begin
  //
end;

procedure anAnimationLayerLookUpPointInstance.Update(dt: Double);
var
  pObj: anAnimationLookUpPointObject;
begin
  pObj := anAnimationLookUpPointObject(ObjectTo);

  if Assigned(pObj.OnUpdate) then begin
     pObj.OnUpdate(dt, pObj, Self);
  end;
end;

procedure anAnimationLayerLookUpPointInstance.Draw(pTransform: anTransform;
  Time: Single; FX: LongWord);
var
  pObj: anAnimationLookUpPointObject;
  pData: anDrawResult;
begin
  pObj := anAnimationLookUpPointObject(ObjectTo);
  if Assigned(pObj.OnDraw) then begin
     pData := ObjectTo.Draw(Holder, pTransform, Time);
     if pData.Visible then
       pObj.OnDraw(pObj, Self, pData);
  end;
end;

constructor anAnimationLayerLookUpPointInstance.Create(
  pIntanceTo: anAnimationInstance; pLookUp: anAnimationLayerObjectInstance;
  pObjectTo: anAnimationLayerObject; pHolder: anAnimationLOInstanceHolder);
begin
  LookUp := pLookUp;
  inherited Create(pIntanceTo, pObjectTo, pHolder);
end;

{ anAnimationLookUpPointObject }

function anAnimationLookUpPointObject.GetObjectInstance(
  AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder
  ): anAnimationLayerObjectInstance;
var OthInstance: anAnimationLayerObjectInstance;
begin
  if Assigned(LookUp) then begin
    OthInstance := AnimInstance.Instances[LookUp.Index];
  end else
    OthInstance := nil;
  Result := anAnimationLayerLookUpPointInstance.Create(AnimInstance, OthInstance, Self, HolderTo);
end;

procedure anAnimationLookUpPointObject.AssignTo(Dest: TPersistent);
var
  dst: anAnimationLookUpPointObject;
begin
  inherited AssignTo(Dest);
  dst := anAnimationLookUpPointObject(Dest);
  dst.LookUp := LookUp;
end;

procedure anAnimationLookUpPointObject.SaveToRecord(rec: TDBRecord);
begin
  inherited SaveToRecord(rec);
  if Assigned(LookUp) then
    rec.ChildByName['lookup'].AsString := LookUp.Name;
end;

procedure anAnimationLookUpPointObject.LoadFromRecord(rec: TDBRecord);
var
  idx: LongInt;
begin
  inherited LoadFromRecord(rec);
  idx := Animation.Objects.IndexOf(rec.ChildByName['lookup'].AsString);
  if idx >= 0 then begin
    LookUp := Animation.Objects.Data[idx];
  end;
end;

constructor anAnimationLookUpPointObject.Create(
  pAnimation: anAnimationPrototype; pParent: anAnimationLayerObject);
begin
  LookUp := nil;
  inherited Create(pAnimation, pParent);
end;

{ anAnimationLayerPointInstance }

procedure anAnimationLayerPointInstance.Play;
begin
  //
end;

procedure anAnimationLayerPointInstance.Stop(Pause: Boolean);
begin
  //
end;

procedure anAnimationLayerPointInstance.Update(dt: Double);
var
  pObj: anAnimationPointObject;
begin
  pObj := anAnimationPointObject(ObjectTo);
  if Assigned(pObj.OnUpdate) then begin
     pObj.OnUpdate(dt, pObj, Self);
  end;
end;

procedure anAnimationLayerPointInstance.Draw(pTransform: anTransform;
  Time: Single; FX: LongWord);
var
  pObj: anAnimationPointObject;
  pData: anDrawResult;
begin
  pObj := anAnimationPointObject(ObjectTo);
  if Assigned(pObj.OnDraw) then begin
     pData := ObjectTo.Draw(Holder, pTransform, Time);
     if pData.Visible then
       pObj.OnDraw(pObj, Self, pData);
  end;
end;

{ anAnimationPointObject }

function anAnimationPointObject.GetObjectInstance(
  AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder
  ): anAnimationLayerObjectInstance;
begin
  Result := anAnimationLayerPointInstance.Create(AnimInstance, Self, HolderTo);
end;

constructor anAnimationPointObject.Create(pAnimation: anAnimationPrototype;
  pParent: anAnimationLayerObject);
begin
  OnDraw := nil;
  OnUpdate := nil;
  inherited Create(pAnimation, pParent);
end;

{ anAnimationLayerAnimationInstance }

function anAnimationLayerAnimationInstance.getTransform: anTransform;
begin
  Result := Instance.Data;
end;

procedure anAnimationLayerAnimationInstance.setTransform(AValue: anTransform);
begin
  Instance.Data := AValue;
end;

procedure anAnimationLayerAnimationInstance.Play;
begin
  Instance.Play;
end;

procedure anAnimationLayerAnimationInstance.Stop(Pause: Boolean);
begin
  Instance.Stop(Pause);
end;

procedure anAnimationLayerAnimationInstance.Update(dt: Double);
begin
  Instance.Update(dt);
end;

procedure anAnimationLayerAnimationInstance.Draw(pTransform: anTransform;
  Time: Single; FX: LongWord);
begin
  {$IFDEF EDITOR}
  if ObjectTo.Visible then
  {$ENDIF}
  Instance.Draw(FX);
end;

constructor anAnimationLayerAnimationInstance.Create(
  pIntanceTo: anAnimationInstance; pInstanceFrom: anAnimation;
  pObjectTo: anAnimationLayerObject; pHolder: anAnimationLOInstanceHolder);
begin
  Instance := pInstanceFrom.GetInstance;
  inherited Create(pIntanceTo, pObjectTo, pHolder);
end;

destructor anAnimationLayerAnimationInstance.Destroy;
begin
  Instance.Free;
  inherited Destroy;
end;

{ anAnimationAnimationObject }

procedure anAnimationAnimationObject.AssignTo(Dest: TPersistent);
var
  dst: anAnimationAnimationObject;
begin
  dst := anAnimationAnimationObject(Dest);
  dst.DataAnimation := DataAnimation;
  inherited AssignTo(Dest);
end;

procedure anAnimationAnimationObject.SaveToRecord(rec: TDBRecord);
begin
  rec.AsString := DataAnimation.Name;
  inherited SaveToRecord(rec);
end;

procedure anAnimationAnimationObject.LoadFromRecord(rec: TDBRecord);
begin
  DataAnimation := anAnimationPrototype(Animation.Animation.Animations[rec.AsString]);
  inherited LoadFromRecord(rec);
end;

function anAnimationAnimationObject.GetOtherProperties(
  pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty;
begin
  Result := anOtherAnimationProperty.Create(Animation, pKeyFrame);
end;

function anAnimationAnimationObject.GetObjectInstance(
  AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder
  ): anAnimationLayerObjectInstance;
begin
  result := anAnimationLayerAnimationInstance.Create(AnimInstance, DataAnimation, Self, HolderTo);
end;

constructor anAnimationAnimationObject.Create(pDataAnimation: anAnimation;
  pAnimation: anAnimationPrototype; pParent: anAnimationLayerObject);
begin
  DataAnimation := pDataAnimation;
  inherited Create(pAnimation, pParent);
end;

{ anAnimationLayerObjectInstance }

procedure anAnimationLayerObjectInstance.setTransform(AValue: anTransform);
begin
  fTransform := AValue;
end;

function anAnimationLayerObjectInstance.getTransform: anTransform;
begin
  Result := fTransform;
end;

constructor anAnimationLayerObjectInstance.Create(
  pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject;
  pHolder: anAnimationLOInstanceHolder);
begin
  AnimationInstance := pIntanceTo;
  Transform := anTransformClass.NewTransform;
  ObjectTo := pObjectTo;
  Holder := pHolder;
  TimeLine := nil;
end;

{ anAnimationLayerSymbolInstance }

procedure anAnimationLayerSymbolInstance.Play;
begin
  State := asPlaying;
end;

procedure anAnimationLayerSymbolInstance.Stop(Pause: Boolean);
begin
  State := asStopped;
end;

procedure anAnimationLayerSymbolInstance.Update(dt: Double);
begin
  // nothing to do
end;

procedure anAnimationLayerSymbolInstance.Draw(pTransform: anTransform;
  Time: Single; FX: LongWord);
var
  Res: anDrawResult;
  symbol: anSymbol;
begin
  Res := ObjectTo.Draw(Holder, pTransform, Time);
  if Res.Visible then begin;
    symbol := anAnimationSymbolObject(ObjectTo).Symbol;
    if Assigned(Symbol.TextureParams) then begin
      Symbol.TextureParams.Draw(Res.Transform,
        Symbol.TextureParams.FormatTexture(TextureData), Self, FX);
    end;
  end;
end;

constructor anAnimationLayerSymbolInstance.Create(
  pIntanceTo: anAnimationInstance; pObjectTo: anAnimationLayerObject;
  pHolder: anAnimationLOInstanceHolder);
begin
  SwapContent := nil;
  inherited Create(pIntanceTo, pObjectTo, pHolder);
  TextureData := anAnimationSymbolObject(ObjectTo).Symbol.TextureParams.GetTexture;
  Stop(False);
end;

destructor anAnimationLayerSymbolInstance.Destroy;
begin
  anAnimationSymbolObject(ObjectTo).Symbol.TextureParams.ReleaseTexture(TextureData);
  inherited Destroy;
end;

{ anAnimationSymbolObject }

procedure anAnimationSymbolObject.AssignTo(Dest: TPersistent);
var dst: anAnimationSymbolObject;
begin
  inherited AssignTo(Dest);
  dst := anAnimationSymbolObject(Dest);
  dst.Symbol := Symbol;
end;

procedure anAnimationSymbolObject.SaveToRecord(rec: TDBRecord);
begin
  rec.AsString := Symbol.Name;
  inherited SaveToRecord(rec);
end;

procedure anAnimationSymbolObject.LoadFromRecord(rec: TDBRecord);
begin
  Symbol := Animation.Animation.AnimationLibrary.Symbols[rec.AsString];
  inherited LoadFromRecord(rec);
end;

function anAnimationSymbolObject.GetObjectInstance(
  AnimInstance: anAnimationInstance; HolderTo: anAnimationLOInstanceHolder
  ): anAnimationLayerObjectInstance;
begin
  if Assigned(Symbol.TextureParams) then
    Result := Symbol.TextureParams.GetObjectInstance(AnimInstance, Self, HolderTo)
  else
    Result := anAnimationLayerSymbolInstance.Create(AnimInstance, Self, HolderTo);
end;

function anAnimationSymbolObject.GetOtherProperties(
  pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty;
begin
  Result := Symbol.TextureParams.GetOtherProperties(Animation, pKeyFrame);
  if not Assigned(Result) then
    Result := inherited GetOtherProperties(pKeyFrame);
end;

constructor anAnimationSymbolObject.Create(pSymbol: anSymbol;
  pAnimation: anAnimationPrototype; pParent: anAnimationLayerObject);
begin
  Symbol := pSymbol;
  inherited Create(pAnimation, pParent);
end;

{ anAnimationInstanceTree }

procedure anAnimationInstanceTree.Update(ParentTransform: anTransform;
  dt: Double);
var
  i: Integer;
  Transform: anTransform;
  ch: anAnimationInstanceTree;
  inst: anAnimationLayerObjectInstance;
begin
  for i := 0 to Childs.Count - 1 do begin
    ch := Childs[i];
    inst := Instance.Instances[ch.Index];
    Transform := Instance.Animation.ObjectByIndex[ch.Index].GetTransform(
      inst.Holder, inst.TimeLine.TotalTime, false);

    Transform := anTransformClass.ApplyTransform(ParentTransform, Transform);

    Transform := Instance.TimeLine.ObjectTransforms.Data[ch.Index].ApplyTo(Transform, Instance.TimeLine.ApplyMode);
    inst.Transform := Transform;

    ch.Update(Transform, dt);
    inst.Update(dt);
  end;
end;

constructor anAnimationInstanceTree.Create(pInstance: anAnimationInstance);
begin
  Childs := anAnimationTreeList.Create;
  Instance := pInstance;
end;

destructor anAnimationInstanceTree.Destroy;
var i: integer;
begin
  for i := 0 to Childs.Count - 1 do begin
    Childs[i].Free;
  end;
  Childs.Free;
  inherited Destroy;
end;

{ anAnimationObjectAssigner }

function anAnimationObjectAssigner.ApplyTo(From: anTransform;
  Mode: anTimeLineApplyMode): anTransform;
begin
  if Apply then begin
    case Mode of
      tlamOverwrite: Exit(Data);
      tlamApply: Exit(anTransformClass.ApplyTransform(Data, From));
    end;
  end;
  Result := From;
end;

constructor anAnimationObjectAssigner.Create;
begin
  Transform := anTransformClass.Create(@Data);
  Apply := true;
end;

destructor anAnimationObjectAssigner.Destroy;
begin
  Transform.Free;
  inherited Destroy;
end;

{ anAnimationTimeLine }

procedure anAnimationTimeLine.CheckFrame;
var
  i: Integer;
begin
  for i := 0 to Animation.ObjectsCount - 1 do
    Animation.HolderByIndex[i].CheckFrame(CurrentFrame, Instance.Instances[i]);
end;

function anAnimationTimeLine.getOffsetTime: Double;
begin
  Result := DeltaTime / (1000 / Animation.FPS);
end;

function anAnimationTimeLine.getTotalTime: Single;
begin
  Result := CurrentFrame + OffsetTime;
end;

procedure anAnimationTimeLine.setCurrentFrame(AValue: anPosition);
begin
  if fCurrentFrame = AValue then Exit;
  fCurrentFrame := AValue;
  if CurrentFrame > Animation.FramesCount - 1 then
    CurrentFrame := Animation.FramesCount - 1;
  if (CurrentFrame < 0) then CurrentFrame := 0;
end;

function anAnimationTimeLine.getKeyframePosition(Name: anString): anPosition;
var
  i, iCode: Integer;
  key: anAnimationKeyFrameInstance;
begin
  val(Name, i, iCode);
  if iCode = 0 then
    exit(i);
  for i := 0 to Animation.ObjectsCount - 1 do begin
    key := Animation.HolderByIndex[i].InstanceAtName[Name];
    if Assigned(key) then
      exit(key.Position);
  end;
  Result := -1;
end;

procedure anAnimationTimeLine.Stop(Pause: Boolean);
begin
  State := asStopped;
  DeltaTime := 0;
  if not Pause then
    CurrentFrame := 0;
end;

procedure anAnimationTimeLine.Play;
begin
  State := asPlaying;
  CheckFrame;
end;

procedure anAnimationTimeLine.GotoAndStop(Frame: anPosition; Pause: Boolean);
begin
  CurrentFrame := Frame;
  Stop(Pause);
end;

procedure anAnimationTimeLine.GotoAndStop(Frame: anString; Pause: Boolean);
var
  i: Integer;
begin
  i := getKeyframePosition(Frame);
  if i >= 0 then begin
    GotoAndStop(i, Pause);
  end;
end;

procedure anAnimationTimeLine.GotoAndPlay(Frame: anPosition);
begin
  CurrentFrame := Frame;
  Play;
end;

procedure anAnimationTimeLine.GotoAndPlay(Frame: anString);
var
  i: Integer;
begin
  i := getKeyframePosition(Frame);
  if i >= 0 then begin
    GotoAndPlay(i);
  end;
end;

procedure anAnimationTimeLine.Update(dt: double);
begin
  case State of
    asPlaying: begin;
      DeltaTime := DeltaTime + dt;
      while DeltaTime > 1000 / Animation.FPS do begin
        DeltaTime := DeltaTime - 1000 / Animation.FPS;
        if CurrentFrame >= Animation.FramesCount - 1 then begin
          if Assigned(OnAnimationEnd) then begin
            OnAnimationEnd(Instance);
          end;
          if Animation.RepeatAnimation then begin;
            CurrentFrame := 0;
          end else begin
            Stop(true);
          end;
        end else begin
          CurrentFrame := CurrentFrame + 1;
        end;
        CheckFrame;
      end;
    end;
  end;
end;

constructor anAnimationTimeLine.Create(pAnimation: anAnimation;
  pInstance: anAnimationInstance; pApplyMode: anTimeLineApplyMode);
var
  i: Integer;
begin
  ApplyMode := pApplyMode;
  Animation := pAnimation;
  Instance := pInstance;
  State := asStopped;
  CurrentFrame := 0;
  DeltaTime := 0;

  OnAnimationEnd := nil;

  ObjectTransforms := anTransformClassList.Create;
  for i := 0 to Animation.ObjectsCount - 1 do begin
    ObjectTransforms.Add(Animation.ObjectByIndex[i].Name, anAnimationObjectAssigner.Create);
  end;
end;

destructor anAnimationTimeLine.Destroy;
var i: integer;
begin
  for i := 0 to ObjectTransforms.Count - 1 do
    ObjectTransforms.Data[i].Free;
  ObjectTransforms.Free;
  inherited Destroy;
end;

{ anNamedObject }

function anNamedObject.getName: anString;
var idx: integer;
begin
  idx := GetMyIndex;
  if idx >= 0 then
    Result := ParentMap.Keys[idx]
  else
    Result := '';
end;

procedure anNamedObject.setName(AValue: anString);
var idx: integer;
begin
  idx := GetMyIndex;
  if ParentMap.IndexOf(AValue) >= 0 then exit;
  if idx >= 0 then begin
    ParentMap.Keys[idx] := AValue;
    if Assigned(OnNameChange) then
      OnNameChange(self);
  end;
end;

function anNamedObject.GetMyIndex: integer;
var
  i: Integer;
begin
  if Assigned(ParentMap) then begin
    for i := 0 to ParentMap.Count - 1 do
      if ParentMap.Data[i] = Self then
        exit(i);
  end;
  exit(-1);
end;

procedure anNamedObject.AssignTo(Dest: TPersistent);
begin
  anNamedObject(Dest).OnNameChange := OnNameChange;
  anNamedObject(Dest).ParentMap := ParentMap;
end;

function anNamedObject.GetUniqueName(From: anString): anString;
var
  newId: Integer;
  idx: Integer;
begin
  if Assigned(ParentMap) then begin
    if ParentMap.Find(From, idx) then begin
      newId := 1;
      repeat
        inc (newId)
      until not ParentMap.Find(From + u_IntToStr(NewId), idx);
      Result := From + u_IntToStr(NewId);
    end else
      Result := From;
  end else
    Result := '';
end;

constructor anNamedObject.Create(aParentMap: TFPSMap);
begin
  ParentMap := anNamedObjectList(aParentMap);
  OnNameChange := nil;
end;

{ anAnimationKeyFrameInstance }

procedure anAnimationKeyFrameInstance.AssignTo(Dest: TPersistent);
var
  dst: anAnimationKeyFrameInstance;
begin
  dst := anAnimationKeyFrameInstance(Dest);
  dst.InterpolationMode := InterpolationMode;
  dst.Frames := Frames;
  dst.Visible := Visible;
  dst.Transform := Transform;
end;

function anAnimationKeyFrameInstance.getFrames: anPosition;
begin
  if Assigned(Next) then
    Result := fFrames
  else
    Result := Animation.FramesCount - Position;
end;

function anAnimationKeyFrameInstance.getPosition: Integer;
var key: anAnimationKeyFrameInstance;
begin
  key := Prev;
  Result := 0;
  while Assigned(key) do begin
    inc(Result, key.Frames);
    key := key.Prev;
  end;
end;

procedure anAnimationKeyFrameInstance.Init(
  pHolder: anAnimationLOInstanceHolder; pPrev,
  pNext: anAnimationKeyFrameInstance; pPosition: anPosition);
var Offset: anPosition;
begin
  Holder := pHolder;

  Prev := pPrev;
  Next := pNext;

  if Assigned(Prev) then begin
    Prev.Next := Self
  end else
    Holder.FirstInstance := self;

  if Assigned(Next) then begin
    Next.Prev := Self
  end else
    Holder.LastInstance := self;

  if pPosition <> 0 then begin
    if Assigned(Prev) then begin
      Offset := Position - pPosition;
      Frames := Offset;
      Prev.Frames := Prev.fFrames - Offset;
    end;
  end;
end;

function anAnimationKeyFrameInstance.GetNextFrames: anPosition;
begin
  Result := fFrames;
end;

function anAnimationKeyFrameInstance.GetNextInstance: anAnimationKeyFrameInstance;
begin
  if Assigned(Next) then exit(Next);
  if Animation.Cycled then exit(Holder.FirstInstance);
  Result := Self;
end;

function anAnimationKeyFrameInstance.Count: integer;
var item: anAnimationKeyFrameInstance;
begin
  Result := 1;
  item := Next;
  while Assigned(item) do begin
    inc(Result);
    item := item.next;
  end;
end;

procedure anAnimationKeyFrameInstance.SaveToRecord(rec: TDBRecord);
begin
  rec.ChildByName['v'].AsBoolean := Visible;
  rec.ChildByName['im'].AsByte := Byte(InterpolationMode);
  rec.ChildByName['f'].AsInteger := Frames;
  rec.ChildByName['r'].AsInteger := Rounds;
  rec.ChildByName['n'].AsString := Name;
  Transformation.SaveToRecord(rec.ChildByName['t']);
  if Assigned(ActiveProperties) then begin
    ActiveProperties.SaveToRecord(rec.ChildByName['o']);
  end;
end;

procedure anAnimationKeyFrameInstance.LoadFromRecord(rec: TDBRecord);
begin
  Visible := rec.ChildByName['v'].AsBoolean;
  InterpolationMode := anInterpolationMode(rec.ChildByName['im'].AsByte);
  Frames := rec.ChildByName['f'].AsInteger;
  Rounds := rec.ChildByName['r'].AsInteger;
  Name := rec.ChildByName['n'].AsString;
  Transformation.LoadFromRecord(rec.ChildByName['t']);
  if Assigned(ActiveProperties) then begin
    ActiveProperties.LoadFromRecord(rec.ChildByName['o']);
  end;
end;

function anAnimationKeyFrameInstance.InterpolateTime(Time: Single): Single;
begin
  Result := (Time - Position) / Frames;
end;

constructor anAnimationKeyFrameInstance.Create(pPrev,
  pNext: anAnimationKeyFrameInstance; pHolder: anAnimationLOInstanceHolder;
  pObjectTo: anAnimationLayerObject; pAnimation: anAnimation; pTimeAt: Integer;
  pTransform: anTransform);
begin
  ObjectTo := pObjectTo;
  Transform := pTransform;
  Transformation := anTransformClass.Create(@Transform, false);
  Visible := true;
  Frames := 0;
  Rounds := 0;
  Name := '';
  Animation := pAnimation;
  ActiveProperties := ObjectTo.GetOtherProperties(Self);
  Init(pHolder, pPrev, pNext, pTimeAt);
end;

constructor anAnimationKeyFrameInstance.Create(pPrev,
  pNext: anAnimationKeyFrameInstance; pHolder: anAnimationLOInstanceHolder;
  pObjectTo: anAnimationLayerObject; pAnimation: anAnimation);
begin
  ObjectTo := pObjectTo;
  Transformation := anTransformClass.Create(@Transform, false);
  Visible := true;
  Frames := 0;
  Rounds := 0;
  Name := '';
  Animation := pAnimation;
  ActiveProperties := ObjectTo.GetOtherProperties(Self);
  Init(pHolder, pPrev, pNext, 0);
end;

destructor anAnimationKeyFrameInstance.Destroy;
begin
  if Assigned(Prev) then begin
    Prev.Frames := Prev.fFrames + Frames;
    Prev.Next := Next;
  end else begin
    Holder.FirstInstance := Next;
  end;
  if Assigned(Next) then begin
    Next.Prev := Prev;
  end else begin
    Holder.LastInstance := Prev;
  end;
  Transformation.Free;
  if Assigned(ActiveProperties) then
    ActiveProperties.Free;
  inherited Destroy;
end;

{ anAnimationLayerObject }

function anAnimationLayerObject.getIndex: Integer;
begin
  Result := Animation.IndexOf[Self];
end;

procedure anAnimationLayerObject.AssignTo(Dest: TPersistent);
var dst: anAnimationLayerObject;
    inst, item: anAnimationKeyFrameInstance;
begin
  dst := anAnimationLayerObject(Dest);
  dst.Visible := Visible;
  dst.Enabled := Enabled;
  if Assigned(Parent) then begin
     dst.Parent := dst.Animation.ObjectByIndex[Parent.Index];
  end else
    dst.Parent := nil;
  dst.Clear;
  item := Holder.FirstInstance;
  while Assigned(item) do begin
    inst := anAnimationKeyFrameInstance.Create(dst.Holder.LastInstance, nil, dst.Holder,
      dst, dst.Animation, item.Position, item.Transform);
    inst.Assign(item);
    item := item.Next;
  end;
end;

procedure anAnimationLayerObject.Clear;
begin
  Holder.Clear;
end;

function anAnimationLayerObject.GetHolder: anAnimationLOInstanceHolder;
begin
  Result := Holder;
end;

function anAnimationLayerObject.Draw(HolderFor: anAnimationLOInstanceHolder;
  Transform: anTransform; Time: Single): anDrawResult;
begin
  {$IFDEF EDITOR}
  Result.Visible := Visible;
  if not Visible then exit;
  {$ELSE}
  Result.Visible := true;
  {$ENDIF}
  Result.Transform := Transform;
end;

function anAnimationLayerObject.GetTransform(
  HolderAt: anAnimationLOInstanceHolder; AtTime: Single; WithParent: Boolean
  ): anTransform;
var
  Inst, NextInst: anAnimationKeyFrameInstance;
  F: Single;
begin
  Inst := HolderAt.InstanceAt[AtTime];
  F := Inst.InterpolateTime(AtTime);

  Result := Inst.Transformation.TransformData^;
  NextInst := Inst.GetNextInstance;
  Result := anTransformClass.Interpolate(Result,
    NextInst.Transformation.TransformData^,
    F,
    Inst.Rounds,
    Inst.InterpolationMode);

  if Assigned(HolderAt.ParentHolder) and WithParent then begin
    Result := anTransformClass.ApplyTransform(
      Parent.GetTransform(HolderAt.ParentHolder, AtTime), Result);
  end;
end;


procedure anAnimationLayerObject.RemoveParent(HolderFor,
  ParentHolder: anAnimationLOInstanceHolder);
begin
  if Assigned(Parent) then begin
    Animation.RemoveParent(HolderFor, ParentHolder);
    Parent := nil;
  end;
end;

procedure anAnimationLayerObject.AssignParent(Obj: anAnimationLayerObject;
  HolderFor, ParentHolder: anAnimationLOInstanceHolder);
begin
  if Parent = Obj then exit;
  if Parent <> nil then
    RemoveParent(HolderFor, ParentHolder);
  Parent := Obj;
  Animation.AssignParent(HolderFor, ParentHolder);
end;

procedure anAnimationLayerObject.SaveToRecord(rec: TDBRecord);
begin
  rec.Name := Name;
  if Assigned(Parent) then
     rec.ChildByName['parent'].AsString := Parent.Name;
  rec.ChildByName['v'].AsBoolean := Visible;
  rec.ChildByName['e'].AsBoolean := Enabled;
  Holder.SaveToRecord(rec.ChildByName['instances']);
end;

procedure anAnimationLayerObject.LoadFromRecord(rec: TDBRecord);
begin
  Visible := rec.ChildByName['v'].AsBoolean;
  Enabled := rec.ChildByName['e'].AsBoolean;
  Holder.LoadFromRecord(rec.ChildByName['instances']);
end;

procedure anAnimationLayerObject.InitParentFromRecord(rec: TDBRecord);
var
  nm: anString;
  idx: LongInt;
begin
  nm := rec.ChildByName['parent'].AsString;
  if nm <> '' then begin
    idx := Animation.IndexOfName[nm];
    if idx >= 0 then begin
      Parent := Animation.ObjectByIndex[idx];
      Init(Animation.HolderByIndex[Index], Animation.HolderByIndex[idx]);
    end;
  end;
end;

procedure anAnimationLayerObject.Init(HolderFor,
  ParentHolder: anAnimationLOInstanceHolder);
begin
  HolderFor.ParentHolder := ParentHolder;
end;

function anAnimationLayerObject.GetInstance(
  pHolder: anAnimationLOInstanceHolder; pTransform: anTransform; AtTime: integer
  ): anAnimationKeyFrameInstance;
var Inst, NextInst: anAnimationKeyFrameInstance;
begin
  Inst := pHolder.InstanceAt[AtTime];
  if Assigned(Inst) then begin
    NextInst := Inst.Next;
  end else begin
    Inst := pHolder.LastInstance;
    NextInst := nil;
  end;
  Result := anAnimationKeyFrameInstance.Create(Inst, NextInst, pHolder,
    Self, pHolder.Animation, AtTime, pTransform);
end;

function anAnimationLayerObject.GetOtherProperties(
  pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty;
begin
  Result := anOtherKFProperty.Create(Animation, pKeyFrame);
end;

constructor anAnimationLayerObject.Create(pAnimation: anAnimationPrototype;
  pParent: anAnimationLayerObject);
begin
  Animation := pAnimation;
  inherited Create(Animation.Objects);
  Parent := pParent;
  Visible := true;
  Enabled := true;
  Holder := anAnimationLOInstanceHolder.Create(Self, Animation);
end;

destructor anAnimationLayerObject.Destroy;
begin
  Holder.Free;
  inherited Destroy;
end;

{ anAnimationInstance }

function anAnimationInstance.getCurrentFrame: anPosition;
begin
  Result := TimeLine.CurrentFrame;
end;

function anAnimationInstance.getInstancesCount: Integer;
begin
  Result := Animation.ObjectsCount;
end;

function anAnimationInstance.getOffsetTime: Double;
begin
  Result := TimeLine.OffsetTime;
end;

procedure anAnimationInstance.PlayObjects;
var
  i: Integer;
begin
  for i := Animation.ObjectsCount - 1 downto 0 do begin
    Instances[i].Play;
  end;
end;

procedure anAnimationInstance.StopObjects(Pause: Boolean);
var
  i: Integer;
begin
  for i := Animation.ObjectsCount - 1 downto 0 do begin
    Instances[i].Stop(Pause);
  end;
end;

procedure anAnimationInstance.ApplyBlender(Blender: anAnimationTimeLine);
var
  i: Integer;
  blend: anAnimationBlender;
begin
  blend := anAnimationBlender(Blender.Animation);
  for i := 0 to blend.ObjectsCount - 1 do begin
    if blend.Proxy[i].Apply then begin
      Instances[i].Holder := blend.HolderByIndex[i];
      Instances[i].TimeLine := Blender;
    end;
  end;
end;

procedure anAnimationInstance.CancelBlender(Blender: anAnimationTimeLine);
var
  i: Integer;
  blend: anAnimationBlender;
begin
  blend := anAnimationBlender(Blender.Animation);
  for i := 0 to blend.ObjectsCount - 1 do begin
    if blend.Proxy[i].Apply then
      Instances[i].Holder := blend.HolderByIndex[i];
      Instances[i].TimeLine := TimeLine;
  end;
end;

function anAnimationInstance.GetInstanceObject(IOName: anString
  ): anAnimationLayerObjectInstance;
var
  idx: LongInt;
begin
  idx :=  Animation.IndexOfName[IOName];
  if idx >= 0 then begin
    Exit(Instances[idx]);
  end;
  Result := nil;
end;

function anAnimationInstance.GetInstanceObjectByLO(
  LayerObject: anAnimationLayerObject): anAnimationLayerObjectInstance;
var
  i: Integer;
begin
  for i := 0 to Animation.ObjectsCount - 1 do begin
    if Animation.ObjectByIndex[i] = LayerObject then begin
      Exit(Instances[i]);
    end;
  end;
  Result := nil;
end;

function anAnimationInstance.AddBlender(Blender: anAnimationBlender
  ): anAnimationTimeLine;
begin
  Result := anAnimationTimeLine.Create(Blender, Blender.GetInstance, tlamOverwrite);
  ApplyBlender(Result);
  Blenders.Add(Result);
end;

function anAnimationInstance.AddBlender(BlenderName: anString
  ): anAnimationTimeLine;
var
  Blender: anAnimationBlender;
begin
  Blender := anAnimationBlender(Animation.Animation.Animations[BlenderName]);
  Result := anAnimationTimeLine.Create(Blender, Blender.GetInstance, tlamOverwrite);
  ApplyBlender(Result);
  Blenders.Add(Result);
end;

procedure anAnimationInstance.RemoveBlender(Blender: anAnimationTimeLine;
  FreeAfter: Boolean);
begin
  Blenders.Remove(Blender);
  CancelBlender(Blender);
  if FreeAfter then
    Blender.Free;
end;

procedure anAnimationInstance.Stop(Pause: Boolean);
var
  i: Integer;
begin
  TimeLine.Stop(Pause);
  for i := 0 to Blenders.Count - 1 do
    Blenders[i].Stop(Pause);
  StopObjects(Pause);
end;

procedure anAnimationInstance.Play;
var
  i: Integer;
begin
  TimeLine.Play;
  for i := 0 to Blenders.Count - 1 do
    if anAnimationBlender(Blenders[i].Animation).AutoPlay then
      Blenders[i].Play;
  PlayObjects;
end;

procedure anAnimationInstance.GotoAndStop(Frame: anPosition; Pause: Boolean);
begin
  TimeLine.GotoAndStop(Frame, Pause);
  StopObjects(Pause);
end;

procedure anAnimationInstance.GotoAndStop(Frame: anString; Pause: Boolean);
begin
  TimeLine.GotoAndStop(Frame, Pause);
  StopObjects(Pause);
end;

procedure anAnimationInstance.GotoAndPlay(Frame: anPosition);
begin
  TimeLine.GotoAndPlay(Frame);
  PlayObjects;
end;

procedure anAnimationInstance.GotoAndPlay(Frame: anString);
begin
  TimeLine.GotoAndPlay(Frame);
  PlayObjects;
end;

procedure anAnimationInstance.Draw(FX: LongWord);
var
  i: Integer;
begin
  for i := Animation.ObjectsCount - 1 downto 0 do begin
    Instances[i].Draw(Instances[i].Transform, TimeLine.CurrentFrame + TimeLine.OffsetTime, FX);
  end;
end;

procedure anAnimationInstance.Update(dt: double);
var
  i: Integer;
begin
  InstanceTree.Update(Data, dt);
  for i := 0 to Blenders.Count - 1 do
    Blenders[i].Update(dt);
  TimeLine.Update(dt);
end;

constructor anAnimationInstance.Create(pAnimation: anAnimation);
var
  i: Integer;
  ch: anAnimationLayerObject;
  holder: anAnimationLOInstanceHolder;
begin
  Data := anTransformClass.NewTransform;
  Transform := anTransformClass.Create(@Data);
  Animation := pAnimation;
  TimeLine := anAnimationTimeLine.Create(Animation, Self, tlamApply);
  Blenders := anBlendersList.Create;
  InstanceTree := anAnimationInstanceTree.Create(Self);
  SetLength(fInstances, Animation.ObjectsCount);
  for i := 0 to Animation.ObjectsCount - 1 do begin
    ch := Animation.ObjectByIndex[i];
    holder := Animation.HolderByIndex[i];
    fInstances[i] := ch.GetObjectInstance(Self, holder);
    fInstances[i].TimeLine := TimeLine;
  end;
  for i := 0 to Animation.ObjectsCount - 1 do begin
    ch := Animation.ObjectByIndex[i];
    if ch.Parent = nil then
      Animation.FillTree(InstanceTree, ch, Self);
  end;
  Update(0);
end;

destructor anAnimationInstance.Destroy;
var
  i: Integer;
begin
  for i := 0 to Blenders.Count - 1 do
    Blenders[i].Free;
  Blenders.Free;
  Transform.Free;
  InstanceTree.Free;
  TimeLine.Free;
  inherited Destroy;
end;

{ anAnimationPrototype }

procedure anAnimation.AssignTo(Dest: TPersistent);
var dst: anAnimation;
begin
  inherited;
  dst := anAnimation(Dest);
  dst.Animation := Animation;
  dst.FPS := fFPS;
  dst.FramesCount := FramesCount;
end;

function anAnimation.getFPS: integer;
begin
  if fFPS = 0 then
     result := Animation.FPS
  else
     result := fFPS;
end;

function anAnimation.getPrototype: anAnimationPrototype;
begin
  Result := nil;
end;

procedure anAnimation.setPrototype(AValue: anAnimationPrototype);
begin
  // nothing to do
end;

procedure anAnimation.RemoveParent(HolderFor,
  ParentHolder: anAnimationLOInstanceHolder);
var inst: anAnimationKeyFrameInstance;
    trnsfr: anTransform;
begin
  inst := HolderFor.FirstInstance;
  while Assigned(inst) do begin
    inst.Transform := anTransformClass.ApplyTransform(
      ParentHolder.LayerObject.GetTransform(ParentHolder, inst.Position),
      inst.Transform
    );
    inst := inst.Next;
  end;
  HolderFor.ParentHolder := nil;
end;

procedure anAnimation.AssignParent(HolderFor,
  ParentHolder: anAnimationLOInstanceHolder);
var
  angle, dist: single;
  m_tr, tr, my_tr: anTransform;
  item: anAnimationKeyFrameInstance;
begin
  HolderFor.ParentHolder := ParentHolder;

  item := HolderFor.FirstInstance;
  while Assigned(item) do begin
    tr := ParentHolder.LayerObject.GetTransform(ParentHolder, item.Position);
    my_tr := item.Transform;
    angle := m_Angle(
      my_tr.Position.X, my_tr.Position.Y,
      tr.Position.X, tr.Position.Y
    ) - tr.Rotation;
    dist := m_Distance(
      my_tr.Position.X, my_tr.Position.Y,
      tr.Position.X, tr.Position.Y
    );
    with item.Transform do begin
      Position.X := Cos(angle * deg2rad) * dist;
      Position.Y := Sin(angle * deg2rad) * dist;
      Rotation := Rotation - tr.Rotation;
    end;
    item := item.Next;
  end;
end;

procedure anAnimation.RegisterCallback(CallbackName: anString;
  CallbackData: anOnActivateCallback);
begin
  Callbacks.Add(CallbackName, CallbackData);
end;

procedure anAnimation.SaveToRecord(rec: TDBRecord);
begin
  rec.Name := Name;
  rec.AsString := ClassName;
  rec.ChildByName['frames_count'].AsInteger := FramesCount;
  rec.ChildByName['fps'].AsInteger := fFPS;
  rec.ChildByName['repeat'].AsBoolean := RepeatAnimation;
  rec.ChildByName['cycled'].AsBoolean := Cycled;
end;

procedure anAnimation.LoadFromRecord(rec: TDBRecord);
begin
  FramesCount := rec.ChildByName['frames_count'].AsInteger;
  FPS := rec.ChildByName['fps'].AsInteger;
  RepeatAnimation := rec.ChildByName['repeat'].AsBoolean;
  Cycled := rec.ChildByName['cycled'].AsBoolean;
end;

function anAnimationPrototype.getObjectsCount: integer;
begin
  Result := Objects.Count;
end;

function anAnimationPrototype.getObjectByIndex(Index: integer
  ): anAnimationLayerObject;
begin
  Result := Objects.Data[Index];
end;

function anAnimationPrototype.getHolderByIndex(Index: integer
  ): anAnimationLOInstanceHolder;
begin
  Result := Objects.Data[Index].Holder;
end;

function anAnimationPrototype.getObjectIndexOf(obj: anAnimationLayerObject
  ): Integer;
begin
  Result := Objects.IndexOfData(obj);
end;

function anAnimationPrototype.getObjectIndexOfByName(objName: anString
  ): Integer;
begin
  Result := Objects.IndexOf(objName);
end;

procedure anAnimationPrototype.registerBlender(blender: anAnimationBlender);
begin
  Blenders.Add(blender);
end;

procedure anAnimationPrototype.unregisterBlender(blender: anAnimationBlender);
begin
  Blenders.Remove(blender);
end;

procedure anAnimationPrototype.RemoveParent(HolderFor,
  ParentHolder: anAnimationLOInstanceHolder);
var
  i: Integer;
  blender: anAnimationBlender;
begin
  inherited RemoveParent(HolderFor, ParentHolder);
  for i := 0 to Blenders.Count - 1 do begin
    blender := Blenders[i];
    blender.RemoveParent(
      blender.HolderByIndex[HolderFor.LayerObject.Index],
      blender.HolderByIndex[ParentHolder.LayerObject.Index]);
  end;
end;

procedure anAnimationPrototype.AssignParent(HolderFor,
  ParentHolder: anAnimationLOInstanceHolder);
var
  i: Integer;
  blender: anAnimationBlender;
begin
  inherited AssignParent(HolderFor, ParentHolder);
  for i := 0 to Blenders.Count - 1 do begin
    blender := Blenders[i];
    blender.AssignParent(
      blender.HolderByIndex[HolderFor.LayerObject.Index],
      blender.HolderByIndex[ParentHolder.LayerObject.Index]);
  end;
end;

procedure anAnimationPrototype.AssignTo(Dest: TPersistent);
var dst: anAnimationPrototype;
  i: Integer;
  obj_class: anAnimationLayerObjectClass;
  obj: anAnimationLayerObject;
begin
  inherited AssignTo(Dest);
  if Dest is anAnimationPrototype then begin
    dst := anAnimationPrototype(Dest);
    for i := 0 to dst.Objects.Count - 1 do
      dst.Objects.Data[i].Free;
    dst.Objects.Clear;
    for i := 0 to Objects.Count - 1 do begin
      obj_class := anAnimationLayerObjectClass(Objects.Data[i].ClassType);
      obj := obj_class.Create(dst, nil);
      dst.AddObject(obj, Objects.Data[i].Name);
    end;
    for i := 0 to dst.Objects.Count - 1 do begin
      obj := dst.Objects.Data[i];
      obj.Assign(Objects.Data[i]);
    end;
    for i := 0 to dst.Objects.Count - 1 do begin
      obj := dst.Objects.Data[i];
      if Assigned(obj.Parent) then
        obj.Init(dst.HolderByIndex[i], dst.HolderByIndex[obj.Parent.Index]);
    end;
  end;
end;

procedure anAnimationPrototype.SaveToRecord(rec: TDBRecord);
var
  ch: TDBRecord;
  i: Integer;
begin
  inherited SaveToRecord(rec);
  ch := rec.ChildByName['objects'];

  ch.ChildCount := Objects.Count;
  for i := 0 to Objects.Count - 1 do begin
    ch.Child[i].ChildByName['class'].AsString := Objects.Data[i].ClassName;
    Objects.Data[i].SaveToRecord(ch.Child[i]);
  end;
end;

procedure anAnimationPrototype.LoadFromRecord(rec: TDBRecord);
var
  ch: TDBRecord;
  i: Integer;
  obj: anAnimationLayerObject;
begin
  inherited LoadFromRecord(rec);
  ch := rec.ChildByName['objects'];
  for i := 0 to ch.ChildCount - 1 do begin
    obj :=  anAnimationLayerObjectClass(FindClass(ch.Child[i].ChildByName['class'].AsString)).Create(Self);
    AddObject(obj, ch.Child[i].Name);
    obj.LoadFromRecord(ch.Child[i]);
  end;
end;

procedure anAnimationPrototype.PostLoadFromRecord(rec: TDBRecord);
var
  ch: TDBRecord;
  i: Integer;
  obj: anAnimationLayerObject;
begin
  ch := rec.ChildByName['objects'];
  for i := 0 to ch.ChildCount - 1 do begin
    obj := Objects.Data[i];
    obj.InitParentFromRecord(ch.Child[i]);
  end;
end;

function anAnimationPrototype.AddObject(Obj: anAnimationLayerObject;
  pName: anString): anAnimationLayerObject;
begin
  if Assigned(Prototype) then Exit(nil);
  Result := Obj;
  Objects.Add(Result.GetUniqueName(pName), Result);
end;

procedure anAnimationPrototype.RemoveObject(Obj: anAnimationLayerObject;
  FreeAfter: Boolean);
begin
  if Assigned(Prototype) then Exit;
  Objects.Remove(Obj.Name);
  if FreeAfter then
    Obj.Free;
end;

procedure anAnimationPrototype.MoveObject(Obj: anAnimationLayerObject; index: integer);
begin
  if Assigned(Prototype) then Exit;
  Objects.Move(Obj.Index, index);
end;

function anAnimationPrototype.GetInstance: anAnimationInstance;
begin
  Result := anAnimationInstance.Create(Self);
end;

function anAnimation.GetInstance: anAnimationInstance;
begin
  Result := nil;
end;

constructor anAnimation.Create(pAnimation: anAnimationSet);
begin
  Animation := pAnimation;
  inherited Create(Animation.Animations);
  FPS := 0;
  FramesCount := 100;
  RepeatAnimation := true;
  Cycled := true;

  Callbacks := anAnimationCallbacksList.Create;
end;

destructor anAnimation.Destroy;
begin
  Callbacks.Clear;
  Callbacks.Free;
  inherited Destroy;
end;

procedure anAnimation.FillTree(Tree: anAnimationInstanceTree;
  ObjectTo: anAnimationLayerObject; Instance: anAnimationInstance);
var
  obj: anAnimationLayerObject;
  new_tree: anAnimationInstanceTree;
  i: Integer;
begin
  new_tree := anAnimationInstanceTree.Create(Instance);
  new_tree.Index := ObjectTo.Index;
  new_tree.ObjectTo := Instance.Instances[ObjectTo.Index];
  Tree.Childs.Add(new_tree);
  for i := 0 to ObjectsCount - 1 do begin
    obj := ObjectByIndex[i];
    if obj.Parent = ObjectTo then begin
      FillTree(new_tree, obj, Instance);
    end;
  end;
end;

constructor anAnimationPrototype.Create(pAnimation: anAnimationSet);
begin
  Prototype := nil;
  inherited Create(pAnimation);
  Blenders := anAnimationBlendersList.Create;
  Objects := anAnimationObjectMap.Create;
end;

destructor anAnimationPrototype.Destroy;
var
  i: Integer;
begin
  for i := 0 to Objects.Count - 1 do begin
    Objects.Data[i].Free;
  end;
  Objects.Free;
  Blenders.Free;
  inherited Destroy;
end;

{ anTransformClass }

procedure anTransformClass.AssignTo(Dest: TPersistent);
var dst: anTransformClass;
begin
  dst := anTransformClass(Dest);
  dst.PositionX := PositionX;
  dst.PositionY := PositionY;
  dst.Scale := Scale;
  dst.Rotation := Rotation;
end;

function anTransformClass.getFlip: Boolean; begin Result := fRecord^.Flip; end;
function anTransformClass.getPositionX: Single; begin Result := fRecord^.Position.X; end;
function anTransformClass.getPositionY: Single; begin Result := fRecord^.Position.Y; end;
function anTransformClass.getRotation: Single; begin Result := fRecord^.Rotation; end;
function anTransformClass.getScale: Single; begin Result := fRecord^.Scale; end;
function anTransformClass.getTransparency: Byte; begin Result := fRecord^.Transparency; end;
procedure anTransformClass.setPositionX(AValue: Single); begin fRecord^.Position.X := AValue; end;
procedure anTransformClass.setPositionY(AValue: Single); begin fRecord^.Position.Y := AValue; end;
procedure anTransformClass.setRotation(AValue: Single); begin fRecord^.Rotation := AValue; end;
procedure anTransformClass.setScale(AValue: Single); begin fRecord^.Scale := AValue; end;
procedure anTransformClass.setTransparency(AValue: Byte); begin fRecord^.Transparency := AValue; end;

class procedure anTransformClass.GetAngle(var angle: Single;
  parentAngle: Single; Flip: Boolean);
begin
  if (Flip) then SwapAngle(angle, parentAngle);
end;

procedure anTransformClass.setFlip(AValue: Boolean); begin fRecord^.Flip := AValue; end;

class procedure anTransformClass.FixAngle(var a: Single);
begin
  while a > 360 do a -= 360;
  while a < 0 do a += 360;
end;

class procedure anTransformClass.FixAngles(var from_: Single; var to_: Single);
begin
  FixAngle(from_);
  FixAngle(to_);
  if abs(from_ - to_) > 180 then begin
    if from_ > to_ then
      from_ -= 360
    else
      to_ -= 360;
  end;
end;

class function anTransformClass.BoolToSign(b: boolean): Integer;
begin
  if b then exit(-1) else exit(1);
end;

class function anTransformClass.InterpolateData(f: Single;
  Kind: anInterpolationMode): Single;
begin
  case Kind of
    imNone: exit(0);
    imLinear: exit(f);
    imSin: exit(sin((f - 0.5) * PI) / 2 + 0.5);
    imSinFadeIn: exit(sin(PI * (f - 1) / 2) + 1);
    imSinFadeOut: exit(sin(PI * f / 2) );
  end;
end;

class function anTransformClass.InterpolateFloat(a, b, f: Single;
  Kind: anInterpolationMode): Single;
begin
  Result := a + (b - a) * InterpolateData(f, Kind);
end;

class function anTransformClass.InterpolateByte(a, b: Byte; f: Single;
  Kind: anInterpolationMode): Byte;
begin
  Result := a + trunc((b - a) * InterpolateData(f, Kind));
end;

class procedure anTransformClass.SwapAngle(var angle: Single; byAngle: Single);
begin
  angle := byAngle * 2 - angle;
end;

class function anTransformClass.Interpolate(FromW, ToW: anTransform;
  Value: Single; Rounds: Integer; Kind: anInterpolationMode): anTransform;
begin
  Result.Position.X := InterpolateFloat(FromW.Position.X, ToW.Position.X, Value, Kind);
  Result.Position.Y := InterpolateFloat(FromW.Position.Y, ToW.Position.Y, Value, Kind);
  Result.Scale := InterpolateFloat(FromW.Scale, ToW.Scale, Value, Kind);
  Result.Transparency := InterpolateByte(FromW.Transparency, ToW.Transparency, Value, Kind);

  FixAngles(FromW.Rotation, ToW.Rotation);

  Result.Rotation := InterpolateFloat(FromW.Rotation,
    ToW.Rotation + 360 * (Rounds), Value, Kind);

  Result.Flip := FromW.Flip;
end;

class function anTransformClass.ApplyTransform(Parent, Child: anTransform
  ): anTransform;
var dist, angle, angle_set: single;
begin
  dist := m_Distance(Child.Position.X, Child.Position.Y, 0, 0);
  angle_set := m_Angle(Child.Position.X, Child.Position.Y, 0, 0);
  angle := Parent.Rotation + angle_set;

  GetAngle(angle, Parent.Rotation, Parent.Flip);

  Result.Flip := Child.Flip xor Parent.Flip;

  Result.Position.X := Parent.Position.X +
    dist * Cos(deg2rad * angle) * Parent.Scale;
  Result.Position.Y := Parent.Position.Y +
    dist * Sin(deg2rad * angle) * Parent.Scale;
  Result.Scale := Parent.Scale * Child.Scale;

  Result.Rotation := Parent.Rotation + Child.Rotation;
  Result.Transparency := Round((Parent.Transparency / 255) * Child.Transparency );

  GetAngle(Result.Rotation, Parent.Rotation, Parent.Flip);
end;

class function anTransformClass.NewTransform: anTransform;
begin
  with Result do begin
    Position.X := 0;
    Position.Y := 0;
    Rotation := 0;
    Scale := 1;
    Flip := false;
    Transparency := 255;
  end;
end;

class function anTransformClass.Get(pX: Single; pY: Single; pRotation: Single;
  pScale: Single; pFlip: Boolean; pTransparency: Byte): anTransform;
begin
  with Result do begin
    Position.X := pX;
    Position.Y := pY;
    Rotation := pRotation;
    Scale := pScale;
    Flip := pFlip;
    Transparency := pTransparency;
  end;
end;

procedure anTransformClass.Init(pX, pY: Single);
begin
  PositionX := pX;
  PositionY := pY;
  Rotation := 0;
  Scale := 1;
  Flip := false;
  Transparency := 255;
end;

procedure anTransformClass.SaveToRecord(rec: TDBRecord);
begin
  rec.SetAsData(TransformData^, SizeOf(anTransform));
end;

procedure anTransformClass.LoadFromRecord(rec: TDBRecord);
begin
  rec.GetAsData(TransformData^);
end;

constructor anTransformClass.Create(pTransform: p_anTransform; pInit: boolean);
begin
  fRecord := pTransform;
  if Assigned(fRecord) and pInit then
    Init(0, 0);
end;

destructor anTransformClass.Destroy;
begin
  inherited Destroy;
end;

{ anTextureParams }

procedure anTextureParams.AssignTo(Dest: TPersistent);
begin
  //
end;

function anTextureParams.Width(TexOf: zglPTexture): Single;
begin
  Result := Texture.Width;
end;

function anTextureParams.Height(TexOf: zglPTexture): Single;
begin
  Result := Texture.Height;
end;

procedure anTextureParams.SaveToRecord(rec: TDBRecord);
begin
  //
end;

procedure anTextureParams.LoadFromRecord(rec: TDBRecord);
begin
  //
end;

function anTextureParams.HaveTexture: Boolean;
begin
  Result := Assigned(Texture);
end;

function anTextureParams.GetTexture: Pointer;
begin
  if Assigned(Texture) then
    Result := Texture.Data
  else
    Result := nil;
end;

function anTextureParams.GetOtherProperties(AnimationTo: anAnimationPrototype;
  pKeyFrame: anAnimationKeyFrameInstance): anOtherKFProperty;
begin
  Result := nil;
end;

function anTextureParams.GetObjectInstance(AnimInstance: anAnimationInstance;
  ObjectTo: anAnimationLayerObject; HolderTo: anAnimationLOInstanceHolder
  ): anAnimationLayerObjectInstance;
begin
  Result := anAnimationLayerSymbolInstance.Create(AnimInstance, ObjectTo, HolderTo);
end;

function anTextureParams.FormatTexture(TexturePointer: Pointer): zglPTexture;
begin
  Result := zglPTexture(TexturePointer);
end;

procedure anTextureParams.ReleaseTexture(TexturePointer: Pointer);
begin
  // nothing to do
end;

procedure anTextureParams.Draw(Transform: anTransform; pContent: zglPTexture;
  pInstance: anAnimationLayerSymbolInstance; FX: LongWord);
begin
  if not Assigned(pContent) then exit;
  fx2d_SetRotatingPivot(
    Symbol.PivotX * Transform.Scale * anTransformClass.BoolToSign(Transform.Flip) + (Width(pContent) / 2) * Transform.Scale,
    Symbol.PivotY * Transform.Scale + (Height(pContent) / 2) * Transform.Scale
  );
end;

constructor anTextureParams.Create(pTexture: anTexture; pSymbol: anSymbol);
begin
  Texture := pTexture;
  Symbol := pSymbol;
end;

{ anFullTextureParams }


procedure anFullTextureParams.AssignTo(Dest: TPersistent);
begin
  //
end;

procedure anFullTextureParams.Draw(Transform: anTransform;
  pContent: zglPTexture; pInstance: anAnimationLayerSymbolInstance; FX: LongWord
  );
var scX, scY: Single;
    LocalTranform: anTransform;
    flX, flY: Integer;
    angle_f: Single;
begin
  LocalTranform := Transform;
  if not Assigned(pContent) then exit;
  flX := anTransformClass.BoolToSign(Transform.Flip);
  flY := 1;
  scX := Width(pContent) * LocalTranform.Scale;
  scY := Height(pContent) * LocalTranform.Scale;
  inherited Draw(Transform, pContent, pInstance, FX);
  angle_f := Transform.Rotation - Symbol.Rotation;
  anTransformClass.GetAngle(angle_f, Transform.Rotation - 90, Transform.Flip);
  ssprite2d_Draw(pContent,
    LocalTranform.Position.X - scX / 2 - Symbol.PivotX * LocalTranform.Scale * flX,
    LocalTranform.Position.Y - scY / 2 - Symbol.PivotY * LocalTranform.Scale * flY,
    scX, scY,
    angle_f, LocalTranform.Transparency, FX or FX2D_RPIVOT or
    (FX2D_FLIPX * Byte(Transform.Flip)));
end;

{ anTextureContent }

procedure anTextureContent.Load;
begin
  if Assigned(TextureData) then begin
    tex_Del(TextureData);
    TextureData := nil;
  end;
  TextureData := tex_LoadFromMemory(Content, Extension, TEX_NO_COLORKEY,
    getFilter(Filter));
end;

procedure anTextureContent.setFilter(AValue: anTextureFilter);
begin
  if fFilter = AValue then Exit;
  fFilter := AValue;
  if Assigned(TextureData) then begin
    tex_Filter(TextureData, anTextureContent.GetFilter(Filter));
  end;
end;

function anTextureContent.getMemorySize: LongWord;
begin
  Result := Content.Size;
end;

function anTextureContent.getWidth: LongWord;
begin
  if Assigned(TextureData) then
    Result := TextureData^.Width
  else
    Result := 0;
end;

function anTextureContent.getData: zglPTexture;
begin
  Result := TextureData;
end;

function anTextureContent.getHeight: LongWord;
begin
  if Assigned(TextureData) then
    Result := TextureData^.Height
  else
    Result := 0;
end;

function anTextureContent.getTexture: zglPPTexture;
begin
  Result := @TextureData;
end;

procedure anTextureContent.AssignTo(Dest: TPersistent);
var dst: anTextureContent;
begin
  dst := anTextureContent(Dest);
  SetLength(dst.Data, Content.Size);
  dst.Content.Memory := @dst.Data[0];
  Move(Content.Memory^, dst.Content.Memory^, Content.Size);
  dst.Content.Size := Content.Size;
  dst.Content.Position := Content.Position;

  dst.FileName := FileName;
  dst.Extension := Extension;

  dst.Load;
end;

procedure anTextureContent.SaveToRecord(rec: TDBRecord);
begin
  rec.Name := Name;
  rec.AsString := ClassName;
  rec.ChildByName['file'].AsString := FileName;
  rec.ChildByName['ext'].AsString := Extension;
  rec.ChildByName['data'].SetAsData(Content.Memory^, Content.Size);
  rec.ChildByName['filter'].AsByte := Byte(Filter);
end;

procedure anTextureContent.LoadFromRecord(rec: TDBRecord);
begin
  FileName := rec.ChildByName['file'].AsString;
  Extension := rec.ChildByName['ext'].AsString;
  Filter := anTextureFilter(rec.ChildByName['filter'].AsByte);
  SetLength(Data, rec.ChildByName['data'].DataLength);
  Content.Memory := @Data[0];
  Content.Size := rec.ChildByName['data'].GetAsData(Content.Memory^);
  Content.Position := 0;
  Load;
end;

procedure anTextureContent.LoadFromMemory(const Buffer; Size: LongWord);
begin
  Content.Position := 0;
  Content.Memory := @Buffer;
  Content.Size := Size;
  Load;
end;

function anTextureContent.LoadFromFile(const FileNameFrom: anString;
  const SetNameAs: anString): Boolean;
var sz: LongWord;
    Handler: zglTFile;
begin
  Result := false;
  if file_Exists(FileNameFrom) then begin
    Extension := file_GetExtension(FileNameFrom);
    if file_Open(Handler, FileNameFrom, FOM_OPENR) then begin
      FileName := SetNameAs;
      sz := file_GetSize(Handler);
      if sz > 0 then begin
        SetLength(Data, sz);
        file_Read(Handler, Data[0], sz);
        LoadFromMemory(Data[0], sz);
      end;
      Result := true;
      file_Close(Handler);
    end;
  end;
end;

procedure anTextureContent.SaveToFile(const FileNameFrom: anString);
var Handler: zglTFile;
begin
  if file_Open(Handler, FileNameFrom, FOM_CREATE) then begin
    file_Write(Handler, Content.Memory^, Content.Size);
    file_Close(Handler);
  end;
end;

class function anTextureContent.getFilter(AValue: anTextureFilter): Integer;
begin
  Result := TEX_CLAMP or TEX_CONVERT_TO_POT or TEX_CALCULATE_ALPHA;
  case AValue of
    tfNearest: Result := Result or TEX_FILTER_NEAREST;
    tfLinear: Result := Result or TEX_FILTER_LINEAR;
    tfBilinear: Result := Result or TEX_FILTER_BILINEAR;
    tfTrilinear: Result := Result or TEX_FILTER_TRILINEAR;
    tfAnisotropy: Result := Result or TEX_FILTER_ANISOTROPY;
  end;
end;

constructor anTextureContent.Create(pAnimation: anAnimationSet);
begin
  Animation := pAnimation;
  inherited Create(Animation.Contents);
  FileName := '';
  fExtension := '';
  Data := nil;
  Content.Memory := nil;
  Content.Position := 0;
  Content.Size := 0;
end;

destructor anTextureContent.Destroy;
begin
  Data := nil;
  if Assigned(TextureData) then begin
    tex_Del(TextureData);
  end;
end;

{ anAtlasTextureParams }

procedure anAtlasTextureParams.AssignTo(Dest: TPersistent);
var dst: anAtlasTextureParams;
begin
  dst := anAtlasTextureParams(Dest);
  dst.Zone := Zone;
end;

function anAtlasTextureParams.Width(TexOf: zglPTexture): Single;
begin
  Result := TexOf^.Width;
end;

function anAtlasTextureParams.Height(TexOf: zglPTexture): Single;
begin
  Result := TexOf^.Height;
end;

procedure anAtlasTextureParams.SaveToRecord(rec: TDBRecord);
begin
  if Assigned(Zone) then
    rec.ChildByName['zone'].AsString := Zone.Name;
end;

procedure anAtlasTextureParams.LoadFromRecord(rec: TDBRecord);
var
  zn: String;
  res: Integer;
begin
  zn := rec.ChildByName['zone'].AsString;
  res := anAtlasTexture(Texture).Zones.IndexOf(zn);
  if res >= 0 then begin
     Zone := anAtlasTexture(Texture).Zones.Data[res];
  end;
end;

function anAtlasTextureParams.HaveTexture: Boolean;
begin
  Result := Assigned(Zone);
end;

function anAtlasTextureParams.GetTexture: Pointer;
begin
  if Assigned(Zone) then begin
    Result := Zone.GetTarget;
  end else
    Result := nil;
end;

function anAtlasTextureParams.FormatTexture(TexturePointer: Pointer
  ): zglPTexture;
begin
  if Assigned(TexturePointer) then
    Result := zglPRenderTarget(TexturePointer)^.Surface
  else
    Result := nil;
end;

procedure anAtlasTextureParams.ReleaseTexture(TexturePointer: Pointer);
begin
  if Assigned(TexturePointer) then
     rtarget_Del(TexturePointer);
end;

constructor anAtlasTextureParams.Create(pTexture: anTexture; pSymbol: anSymbol);
begin
  inherited;
  Zone := nil;
end;

{ anStaticSpriteTextureParams }

procedure anStaticSpriteTextureParams.AssignTo(Dest: TPersistent);
var dst: anStaticSpriteTextureParams;
begin
  dst := anStaticSpriteTextureParams(Dest);
  dst.Tile := Tile;
end;

procedure anStaticSpriteTextureParams.SaveToRecord(rec: TDBRecord);
begin
  rec.ChildByName['tile'].AsInteger := Tile;
end;

procedure anStaticSpriteTextureParams.LoadFromRecord(rec: TDBRecord);
begin
  Tile := rec.ChildByName['tile'].AsInteger;
end;

function anStaticSpriteTextureParams.GetOtherProperties(
  AnimationTo: anAnimationPrototype; pKeyFrame: anAnimationKeyFrameInstance
  ): anOtherKFProperty;
begin
  Result := anOtherStaticSpriteProperty.Create(AnimationTo, pKeyFrame);
end;

function anStaticSpriteTextureParams.GetObjectInstance(
  AnimInstance: anAnimationInstance; ObjectTo: anAnimationLayerObject;
  HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance;
var
  inst: anAnimationLayerStaticSymbolInstance;
begin
  inst := anAnimationLayerStaticSymbolInstance.Create(AnimInstance, ObjectTo, HolderTo);
  inst.Frame := Tile;
  Result := inst;
end;

procedure anStaticSpriteTextureParams.Draw(Transform: anTransform;
  pContent: zglPTexture; pInstance: anAnimationLayerSymbolInstance; FX: LongWord
  );
var scX, scY: Single;
    LocalTranform: anTransform;
    flX: Integer;
    flY: Integer;
    angle_f: Single;
begin
  LocalTranform := Transform;
  if not Assigned(pContent) then exit;

  flX := anTransformClass.BoolToSign(Transform.Flip);
  flY := 1;
  scX := Width(pContent) * LocalTranform.Scale;
  scY := Height(pContent) * LocalTranform.Scale;
  inherited Draw(Transform, pContent, pInstance, FX);
  angle_f := Transform.Rotation - Symbol.Rotation;
  anTransformClass.GetAngle(angle_f, Transform.Rotation - 90, Transform.Flip);

  asprite2d_Draw(pContent,
    LocalTranform.Position.X - scX / 2 - Symbol.PivotX * LocalTranform.Scale * flX,
    LocalTranform.Position.Y - scY / 2 - Symbol.PivotY * LocalTranform.Scale * flY,
    scX, scY,
    angle_f, Tile, LocalTranform.Transparency, FX or FX2D_RPIVOT or
    (FX2D_FLIPX * Byte(Transform.Flip)));
end;

constructor anStaticSpriteTextureParams.Create
  (pTexture: anTexture; pSymbol: anSymbol);
begin
  inherited;
  Tile := 0;
end;

{ anStaticSpriteTexture }

procedure anStaticSpriteTexture.updateTile;
begin
  if (TileX > 0) and (TileY > 0) then
    tex_SetFrameSize(FileContent._Texture^, TileX, TileY);
end;

procedure anStaticSpriteTexture.setTileX(AValue: Integer);
begin
  fTileX := AValue;
  updateTile;
end;

function anStaticSpriteTexture.getFramesCount: integer;
begin
  if Assigned(Data) then
    Result := Length(Data^.FramesCoord)
  else
    Result := 0;
end;

procedure anStaticSpriteTexture.setTileY(AValue: Integer);
begin
  fTileY := AValue;
  updateTile;
end;

function anStaticSpriteTexture.CreateParams(pSymbol: anSymbol): anTextureParams;
begin
  Result := anStaticSpriteTextureParams.Create(Self, pSymbol);
end;

procedure anStaticSpriteTexture.AssignTo(Dest: TPersistent);
var dst: anStaticSpriteTexture;
begin
  inherited AssignTo(Dest);
  dst := anStaticSpriteTexture(Dest);
  dst.TileX := TileX;
  dst.TileY := TileY;
  dst.FramesCount := FramesCount;
end;

procedure anStaticSpriteTexture.SaveToRecord(rec: TDBRecord);
begin
  inherited SaveToRecord(rec);
  rec.ChildByName['tile_x'].AsInteger := TileX;
  rec.ChildByName['tile_y'].AsInteger := TileY;
  rec.ChildByName['frames_count'].AsInteger := FramesCount;
end;

procedure anStaticSpriteTexture.LoadFromRecord(rec: TDBRecord);
begin
  inherited LoadFromRecord(rec);
  TileX := rec.ChildByName['tile_x'].AsInteger;
  TileY := rec.ChildByName['tile_y'].AsInteger;
  FramesCount := rec.ChildByName['frames_count'].AsInteger;
end;

procedure anStaticSpriteTexture.Draw(pX, pY: single; pAplha: byte;
  Params: anPInstanceParameters);
var
  i: Integer;
  offc: Integer;
begin
  if (TileX > 0) and (TileY > 0) then begin
    offc := - FramesCount * (Width + 3) div 2;
    if Assigned(Data) then begin
      for i := 0 to FramesCount - 1 do begin
        asprite2d_Draw(Data,
          pX + i * (Width + 3) + offc,
          pY,
          Width, Height, 0, i + 1, pAplha);
        pr2d_Rect(
          pX + i * (Width + 3) - 1 + offc,
          pY - 1,
          Width + 2, Height + 2, 0);
      end;
    end;
  end;
end;

constructor anStaticSpriteTexture.Create(pAnimation: anAnimationSet);
begin
  inherited;
end;

{ anAtlasTextureZone }

procedure anAtlasTextureZone.AssignTo(Dest: TPersistent);
var
  dst: anAtlasTextureZone;
  i: Integer;
  p: zglPPoint2D;
begin
  dst := anAtlasTextureZone(Dest);
  dst.fAtlas := fAtlas;
  dst.X := X;
  dst.Y := Y;
  dst.Clear;
  for i := 0 to Points.Count - 1 do begin
    new(p);
    p^ := Points[i]^;
    dst.Points.Add(p);
  end;
end;

procedure anAtlasTextureZone.Draw(pX, pY: Single);
begin
  //
end;

procedure anAtlasTextureZone.SaveToRecord(rec: TDBRecord);
var
  pnts: TDBRecord;
  i: Integer;
  ch: TDBRecord;
begin
  rec.Name := Name;
  rec.ChildByName['x'].AsSingle := X;
  rec.ChildByName['y'].AsSingle := Y;
  pnts := rec.ChildByName['points'];
  pnts.ChildCount := Points.Count;
  for i := 0 to Points.Count - 1 do begin
    ch := pnts.Child[i];
    ch.ChildByName['x'].AsSingle := Points[i]^.X;
    ch.ChildByName['y'].AsSingle := Points[i]^.Y;
  end;
end;

procedure anAtlasTextureZone.LoadFromRecord(rec: TDBRecord);
var
  pnts: TDBRecord;
  p: zglPPoint2D;
  i: Integer;
  ch: TDBRecord;
begin
  X := rec.ChildByName['x'].AsSingle;
  Y := rec.ChildByName['y'].AsSingle;
  pnts := rec.ChildByName['points'];
  for i := 0 to pnts.ChildCount - 1 do begin
    ch := pnts.Child[i];
    new(p);
    p^.X := ch.ChildByName['x'].AsSingle;
    p^.Y := ch.ChildByName['y'].AsSingle;
    Points.Add(p);
  end;
end;

procedure anAtlasTextureZone.Clear;
var
  i: Integer;
begin
  for i := 0 to Points.Count - 1 do
    Dispose(Points[i]);
  Points.Clear;
end;

function anAtlasTextureZone.GetTarget: zglPRenderTarget;
var pnts : array of zglTPoint2D;
  i: Integer;
  w: Extended;
  h: Extended;

  Vertices : zglPPoints2D;
  TexCoords : array of zglTPoint2D;
  min_sz, max_sz, ps: zglTPoint2D;
  VCount   : integer;
begin
  if Assigned(Atlas.Data) then begin
    SetLength(pnts, Points.Count);
    with max_sz do begin
      X := 0; Y := 0;
    end;
    with min_sz do begin
      X := Atlas.Width; Y := Atlas.Height;
    end;
    ps.X := X + Atlas.Width / 2;
    ps.Y := Y + Atlas.Height / 2;
    for i := 0 to Points.Count - 1 do begin
      with pnts[i] do begin
        X := Points[i]^.X + ps.X;
        Y := Points[i]^.Y + ps.Y;

        if X < min_sz.X then min_sz.X := X;
        if Y < min_sz.Y then min_sz.Y := Y;

        if X > max_sz.X then max_sz.X := X;
        if Y > max_sz.Y then max_sz.Y := Y;
      end;
    end;
    for i := 0 to Points.Count - 1 do begin
      with pnts[i] do begin
        X := X - min_sz.X;
        Y := Y - min_sz.Y;
      end;
    end;
    {$IFNDEF ANDROID}
    tess_Triangulate( @pnts[0], 0, Points.Count - 1 );
    pnts := nil;
    VCount := tess_GetData( Vertices );
    {$ELSE}
    VCount := 0;
    {$ENDIF}
    SetLength( TexCoords, VCount );
    w := 1 / ( Atlas.Width / Atlas.Data^.U );
    h := 1 / ( Atlas.Height / Atlas.Data^.V );
    for i := 0 to VCount - 1 do begin
      TexCoords[ i ].x := (Vertices^[ i ].x + min_sz.X) * w;
      TexCoords[ i ].y := Atlas.Data^.V - (Vertices^[ i ].y + min_sz.Y) * h;
    end;

    Result := rtarget_Add(
      tex_CreateZero(
        Round(max_sz.X - min_sz.X),
        Round(max_sz.Y - min_sz.Y),
        0, anTextureContent.getFilter(Atlas.FileContent.Filter)),
      RT_CLEAR_COLOR);

    rtarget_Set(Result);
      pr2d_TriList(Atlas.Data,
        Vertices,
        @TexCoords[0],
        0,
        VCount - 1,
        $FFFFFF, 255, FX_BLEND or PR2D_FILL );
    rtarget_Set(nil);

    TexCoords := nil;
    zgl_FreeMem( Pointer( Vertices ) );
  end;
end;

constructor anAtlasTextureZone.Create(pAtlas: anAtlasTexture);
begin
  Points := anAtlasTextureZonePoints.Create;
  fAtlas := pAtlas;
  inherited Create(Atlas.Zones);
  X := 0; Y := 0;
end;

destructor anAtlasTextureZone.Destroy;
begin
  Clear;
  Points.Free;
end;

{ anFullTexture }

function anFullTexture.CreateParams(pSymbol: anSymbol): anTextureParams;
begin
  Result := anFullTextureParams.Create(Self, pSymbol);
end;

{ anAnimatedSpriteTextureParams }

procedure anAnimatedSpriteTextureParams.AssignTo(Dest: TPersistent);
var dst: anAnimatedSpriteTextureParams;
begin
  dst := anAnimatedSpriteTextureParams(Dest);
  dst.TileSet := TileSet;
end;

procedure anAnimatedSpriteTextureParams.SaveToRecord(rec: TDBRecord);
begin
  if Assigned(TileSet) then
    rec.ChildByName['tile_set'].AsString := TileSet.Name
  else
    rec.ChildByName['tile_set'].AsString := '';
end;

procedure anAnimatedSpriteTextureParams.LoadFromRecord(rec: TDBRecord);
var
  tileset_name: String;
  tex_anim: anAnimatedSpriteTexture;
  idx: LongInt;
begin
  tileset_name := rec.ChildByName['tile_set'].AsString;
  if tileset_name <> '' then begin
    tex_anim := anAnimatedSpriteTexture(Texture);
    idx := tex_anim.TileSets.IndexOf(tileset_name);
    if idx >= 0 then begin
      TileSet := tex_anim.TileSets.Data[idx];
    end;
  end;
end;

function anAnimatedSpriteTextureParams.GetOtherProperties(
  AnimationTo: anAnimationPrototype; pKeyFrame: anAnimationKeyFrameInstance
  ): anOtherKFProperty;
begin
  Result := anOtherAnimationSpriteProperty.Create(AnimationTo, pKeyFrame);
end;

function anAnimatedSpriteTextureParams.GetObjectInstance(
  AnimInstance: anAnimationInstance; ObjectTo: anAnimationLayerObject;
  HolderTo: anAnimationLOInstanceHolder): anAnimationLayerObjectInstance;
var
  inst: anAnimationLayerAnimatedSymbolInstance;
begin
  inst := anAnimationLayerAnimatedSymbolInstance.Create(AnimInstance, ObjectTo, HolderTo);
  inst.TileSet := TileSet;
  Result := inst;
end;

procedure anAnimatedSpriteTextureParams.Draw(Transform: anTransform;
  pContent: zglPTexture; pInstance: anAnimationLayerSymbolInstance; FX: LongWord
  );
var scX, scY, angle_f: Single;
    LocalTranform: anTransform;
    frame: Integer;
    flX: Integer;
    flY: Integer;
    inst: anAnimationLayerAnimatedSymbolInstance;
begin
  LocalTranform := Transform;
  if not Assigned(pContent) then exit;
  if not Assigned(TileSet) then exit;

  flX := anTransformClass.BoolToSign(Transform.Flip);
  flY := 1;
  scX := Width(pContent) * LocalTranform.Scale;
  scY := Height(pContent) * LocalTranform.Scale;
  inherited Draw(Transform, pContent, pInstance, FX);
  angle_f := Transform.Rotation - Symbol.Rotation;
  anTransformClass.GetAngle(angle_f, Transform.Rotation - 90, Transform.Flip);

  inst := anAnimationLayerAnimatedSymbolInstance(pInstance);

  if Assigned(inst) then begin
    if Assigned(inst.TileSet) then
      frame := inst.TileSet.TileList[Inst.Frame].Tile
    else
      frame := 0;
  end else
    frame := 0;

  asprite2d_Draw(pContent,
    LocalTranform.Position.X - scX / 2 - Symbol.PivotX * LocalTranform.Scale * flX,
    LocalTranform.Position.Y - scY / 2 - Symbol.PivotY * LocalTranform.Scale * flY,
    scX, scY,
    angle_f, frame, LocalTranform.Transparency, FX or FX2D_RPIVOT or
    (FX2D_FLIPX * Byte(Transform.Flip)));
end;

constructor anAnimatedSpriteTextureParams.Create(pTexture: anTexture; pSymbol: anSymbol);
begin
  inherited;
  TileSet := nil;
end;

{ anAtlasTexture }

function anAtlasTexture.CreateParams(pSymbol: anSymbol): anTextureParams;
begin
  Result := anAtlasTextureParams.Create(Self, pSymbol);
end;

function anAtlasTexture.getZonesCount: integer;
begin
  if Assigned(Zones) then
    Result := Zones.Count
  else
    Result := 0;
end;

procedure anAtlasTexture.SaveToRecord(rec: TDBRecord);
var
  zn: TDBRecord;
  i: Integer;
begin
  inherited SaveToRecord(rec);
  zn := rec.ChildByName['zones'];
  zn.ChildCount := Zones.Count;
  for i := 0 to Zones.Count - 1 do
    Zones.Data[i].SaveToRecord(zn.Child[i]);
end;

procedure anAtlasTexture.LoadFromRecord(rec: TDBRecord);
var
  zn: TDBRecord;
  zone: anAtlasTextureZone;
  i: Integer;
begin
  inherited LoadFromRecord(rec);
  zn := rec.ChildByName['zones'];
  for i := 0 to zn.ChildCount - 1 do begin
    zone := AddZone(zn.Child[i].Name);
    zone.LoadFromRecord(zn.Child[i]);
  end;
end;

procedure anAtlasTexture.Draw
  (pX, pY: Single; pAplha: byte; Params: anPInstanceParameters);
var
  i: Integer;
begin
  inherited Draw(pX, pY, pAplha, Params);
  for i := 0 to ZonesCount - 1 do
    Zones.Data[i].Draw(pX, pY);
end;

function anAtlasTexture.AddZone(pName: anString): anAtlasTextureZone;
begin
  Result := anAtlasTextureZone.Create(Self);
  Zones.Add(Result.GetUniqueName(pName), Result);
end;

function anAtlasTexture.AddZone(Zone: anAtlasTextureZone; pName: anString
  ): anAtlasTextureZone;
begin
  Result := Zone;
  Zones.Add(Result.GetUniqueName(pName), Zone);
end;

procedure anAtlasTexture.DeleteZone(pName: anString; FreeAfter: boolean);
var idx: integer;
    Obj: anAtlasTextureZone;
begin
  if Zones.Find(pName, idx) then begin
    Obj := Zones.Data[idx];
    Zones.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

procedure anAtlasTexture.DeleteZone(Zone: anAtlasTextureZone; FreeAfter: boolean
  );
var idx: integer;
    Obj: anAtlasTextureZone;
begin
  idx := Zones.IndexOfData(Zone);
  if idx >= 0 then begin
    Obj := Zones.Data[idx];
    Zones.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

constructor anAtlasTexture.Create(pAnimation: anAnimationSet);
begin
  inherited;
  Zones := anAtlasTextureZoneList.Create;
end;

destructor anAtlasTexture.Destroy;
var i: Integer;
begin
  for i := 0 to Zones.Count - 1 do
    Zones.Data[i].Free;
  Zones.Free;
  inherited Destroy;
end;

{ anAnimatedSpriteTexture }

procedure anAnimatedSpriteTexture.AssignTo(Dest: TPersistent);
var dst: anAnimatedSpriteTexture;
begin
  inherited AssignTo(Dest);
  dst := anAnimatedSpriteTexture(Dest);
end;

procedure anAnimatedSpriteTexture.SaveToRecord(rec: TDBRecord);
var
  ch_data: TDBRecord;
  i: Integer;
begin
  inherited SaveToRecord(rec);
  ch_data := rec.ChildByName['tile_sets'];
  ch_data.ChildCount := TileSets.Count;
  for i := 0 to TileSets.Count - 1 do
    TileSets.Data[i].SaveToRecord(ch_data.Child[i]);
end;

procedure anAnimatedSpriteTexture.LoadFromRecord(rec: TDBRecord);
var
  ch_data: TDBRecord;
  tile_set: anAnimatedSpriteTextureTileSet;
  i: Integer;
begin
  inherited LoadFromRecord(rec);
  ch_data := rec.ChildByName['tile_sets'];
  for i := 0 to ch_data.ChildCount - 1 do begin
    tile_set := anAnimatedSpriteTextureTileSet.Create(Self);
    tile_set.LoadFromRecord(ch_data.Child[i]);
    TileSets.Add(ch_data.Child[i].Name, tile_set);
  end;
end;

function anAnimatedSpriteTexture.CreateParams(pSymbol: anSymbol
  ): anTextureParams;
begin
  Result := anAnimatedSpriteTextureParams.Create(Self, pSymbol);
end;

constructor anAnimatedSpriteTexture.Create(pAnimation: anAnimationSet);
begin
  inherited;
  TileX := 0;
  TileY := 0;
  TileSets := anAnimatedSpriteTextureTileSetList.Create;
end;

destructor anAnimatedSpriteTexture.Destroy;
var
  i: Integer;
begin
  for i := 0 to TileSets.Count - 1 do
    TileSets.Data[i].Free;
  TileSets.Free;
  inherited Destroy;
end;

{ anTexture }

function anTexture.getData: zglPTexture;
begin
  if Assigned(FileContent) then
    Result := FileContent._Texture^
  else
    Result := nil;
end;

procedure anTexture.AssignTo(Dest: TPersistent);
var dst: anTexture;
begin
  inherited;
  dst := anTexture(Dest);
  dst.FileContent := FileContent;
  dst.Width := Width;
  dst.Height := Height;
end;

procedure anTexture.SaveToRecord(rec: TDBRecord);
begin
  rec.Name := Name;
  rec.AsString := ClassName;
  rec.ChildByName['width'].AsInteger := Width;
  rec.ChildByName['height'].AsInteger := Height;
  if Assigned(FileContent) then
    rec.ChildByName['data'].AsString := FileContent.Name;
end;

procedure anTexture.LoadFromRecord(rec: TDBRecord);
var
  idx: LongInt;
begin
  Name := rec.Name;
  Width := rec.ChildByName['width'].AsInteger;
  Height := rec.ChildByName['height'].AsInteger;
  idx := Animation.Contents.IndexOf(rec.ChildByName['data'].AsString);
  if idx >= 0 then FileContent := Animation.Contents.Data[idx];
end;

procedure anTexture.Draw(pX, pY: Single; pAplha: byte;
  Params: anPInstanceParameters);
begin
  ssprite2d_Draw(Data,
    pX - Width div 2,
    pY - Height div 2,
    Width, Height, 0, pAplha);
end;

constructor anTexture.Create(pAnimation: anAnimationSet);
begin
  Animation := pAnimation;
  inherited Create(Animation.Textures);
  FileContent := nil;
  Width := 0;
  Height := 0;
end;

destructor anTexture.Destroy;
begin
  inherited Destroy;
end;

{ anSymbol }

function anSymbol.getPivotX: Single;
begin
  result := fRotatingPivot.X;
end;

function anSymbol.getPivotY: Single;
begin
  result := fRotatingPivot.Y;
end;

procedure anSymbol.AssignTo(Dest: TPersistent);
var dst: anSymbol;
begin
  inherited;
  dst := anSymbol(Dest);
  dst.fRotatingPivot := fRotatingPivot;
  dst.Texture := Texture;
  if Assigned(dst.TextureParams) then
    dst.TextureParams.Free;
  if Assigned(Texture) then begin
    dst.TextureParams := Texture.CreateParams(dst);
    dst.TextureParams.Assign(TextureParams);
  end;
end;

procedure anSymbol.setPivotX(AValue: Single);
begin
  fRotatingPivot.X := AValue;
end;

procedure anSymbol.setPivotY(AValue: Single);
begin
  fRotatingPivot.Y := AValue;
end;

procedure anSymbol.setTexture(AValue: anTexture);
begin
  if fTexture = AValue then Exit;
  if Assigned(TextureParams) then begin
    TextureParams.Free;
    TextureParams := nil;
  end;
  fTexture := AValue;
  if Assigned(AValue) then
    TextureParams := AValue.CreateParams(self);
end;

procedure anSymbol.RemoveTexture;
begin
  fTexture := nil;
  if Assigned(TextureParams) then begin
    TextureParams.Free;
    TextureParams := nil;
  end;
end;

procedure anSymbol.SaveToRecord(rec: TDBRecord);
begin
  rec.Name := Name;
  if Assigned(Texture) then begin
    rec.AsString := Texture.Name;
  end;
  if Assigned(TextureParams) then begin
    TextureParams.SaveToRecord(rec.ChildByName['params']);
  end;

  rec.ChildByName['rotation'].AsSingle := Rotation;

  rec.ChildByName['rpivot_x'].AsSingle := fRotatingPivot.X;
  rec.ChildByName['rpivot_y'].AsSingle := fRotatingPivot.Y;
end;

procedure anSymbol.LoadFromRecord(rec: TDBRecord);
begin
  if Rec.AsString <> '' then begin
    Texture := AnimationLibrary.Animation.Textures[Rec.AsString];
    if Assigned(TextureParams) then begin
      TextureParams.LoadFromRecord(rec.ChildByName['params']);
    end;
  end;

  Rotation := rec.ChildByName['rotation'].AsSingle;

  fRotatingPivot.X := rec.ChildByName['rpivot_x'].AsSingle;
  fRotatingPivot.Y := rec.ChildByName['rpivot_y'].AsSingle;
end;

procedure anSymbol.Draw(Transform: anTransform; pContent: zglPTexture);
begin
  if Assigned(TextureParams) then begin
    TextureParams.Draw(Transform, pContent, nil);
  end;
end;

constructor anSymbol.Create(pLibrary: anLibrary);
begin
  AnimationLibrary := pLibrary;
  inherited Create(AnimationLibrary.Symbols);
  PivotX := 0;
  PivotY := 0;
  TextureParams := nil;
end;

destructor anSymbol.Destroy;
begin
  inherited Destroy;
end;

{ anLibrary }

procedure anLibrary.SaveToRecord(rec: TDBRecord);
var
  i: Integer;
begin
  rec.ChildCount := Symbols.Count;
  for i := 0 to Symbols.Count - 1 do
    Symbols.Data[i].SaveToRecord(rec.Child[i]);
end;

procedure anLibrary.LoadFromRecord(rec: TDBRecord);
var
  sym: anSymbol;
  i: Integer;
begin
  for i := 0 to rec.ChildCount - 1 do begin
    sym := AddSymbol(rec.Child[i].Name);
    sym.LoadFromRecord(rec.Child[i]);
  end;
end;

procedure anLibrary.Clear;
var
  i: Integer;
begin
  for i := 0 to Symbols.Count - 1 do
    Symbols.Data[i].Free;
  Symbols.Clear;
end;

function anLibrary.AddSymbol(Name: anString): anSymbol;
begin
  Result := anSymbol.Create(Self);
  Result.OnNameChange := OnNameChange;
  Symbols.Add(Result.GetUniqueName(Name), Result);
end;

function anLibrary.AddSymbol(Symbol: anSymbol; Name: anString): anSymbol;
begin
  Result := Symbol;
  Symbols.Add(Result.GetUniqueName(Name), Symbol);
end;

function anLibrary.CloneSymbol(CloneFrom: anString): anSymbol;
var idx: integer;
    cloner: anSymbol;
begin
  Result := nil;
  idx := Symbols.IndexOf(CloneFrom);
  if idx >= 0 then begin
    cloner := Symbols.Data[idx];
    Result := anSymbol.Create(Self);
    Symbols.Add(Result.GetUniqueName(CloneFrom), Result);
    Result.Assign(cloner);
  end;
end;

procedure anLibrary.DeleteSymbol(Name: anString; FreeAfter: boolean);
var idx: integer;
    Obj: anSymbol;
begin
  idx := Symbols.IndexOf(Name);
  if idx >= 0 then begin
    Obj := Symbols.Data[idx];
    Symbols.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

procedure anLibrary.DeleteSymbol(Symbol: anSymbol; FreeAfter: boolean);
var idx: integer;
    Obj: anSymbol;
begin
  idx := Symbols.IndexOfData(Symbol);
  if idx >= 0 then begin
    Obj := Symbols.Data[idx];
    Symbols.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

constructor anLibrary.Create(pAnimation: anAnimationSet);
begin
  OnNameChange := nil;
  Animation := pAnimation;
  Symbols := asSymbolsList.Create;
end;

destructor anLibrary.Destroy;
begin
  Clear;
  Symbols.Free;
  inherited Destroy;
end;

{ anAnimationSet }

function anAnimationSet.AddTexture(Name: anString; ClassOf: anTextureClass
  ): anTexture;
begin
  Result := ClassOf.Create(Self);
  Result.OnNameChange := OnTextureNameChange;
  Textures.Add(Result.GetUniqueName(Name), Result);
end;

function anAnimationSet.AddTexture(Name: anString; Texture: anTexture
  ): anTexture;
begin
  Result := Texture;
  Textures.Add(Texture.GetUniqueName(Name), Texture);
end;

function anAnimationSet.CloneTexture(CloneFrom: anString): anTexture;
var idx: integer;
    cloner: anTexture;
begin
  Result := nil;
  idx := Textures.IndexOf(CloneFrom);
  if idx >= 0 then begin
    cloner := Textures.Data[idx];
    Result := anTextureClass(cloner.ClassType).Create(Self);
    Textures.Add(Result.GetUniqueName(CloneFrom), Result);
    Result.Assign(cloner);
  end;
end;

procedure anAnimationSet.DeleteTexture(Name: anString; FreeAfter: boolean);
var idx: integer;
    Obj: anTexture;
begin
  idx := Textures.IndexOf(Name);
  if idx >= 0 then begin
    Obj := Textures.Data[idx];
    Textures.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

procedure anAnimationSet.DeleteTexture(Texture: anTexture; FreeAfter: boolean);
var
  idx: Integer;
  Obj: anTexture;
begin
  idx := Textures.IndexOfData(Texture);
  if idx >= 0 then begin
    Obj := Textures.Data[idx];
    Textures.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

function anAnimationSet.AddAnimation(Name: anString; ClassOf: anAnimationClass
  ): anAnimation;
begin
  Result := ClassOf.Create(Self);
  Result.OnNameChange := OnAnimationNameChange;
  Animations.Add(Result.GetUniqueName(Name), Result);
end;

function anAnimationSet.AddAnimation(Animation: anAnimation; Name: anString
  ): anAnimation;
begin
  Result := Animation;
  Animations.Add(Result.GetUniqueName(Name), Animation);
end;

function anAnimationSet.CloneAnimation(CloneFrom: anString;
  ClassOf: anAnimationClass): anAnimation;
var idx: integer;
    cloner: anAnimation;
begin
  Result := nil;
  idx := Animations.IndexOf(CloneFrom);
  if idx >= 0 then begin
    cloner := Animations.Data[idx];
    Result := ClassOf.Create(Self);
    Animations.Add(Result.GetUniqueName(CloneFrom), Result);
    Result.Assign(cloner);
  end;
end;

procedure anAnimationSet.DeleteAnimation(Name: anString; FreeAfter: boolean);
var idx: integer;
    Obj: anAnimation;
begin
  if Animations.Find(Name, idx) then begin
    Obj := Animations.Data[idx];
    Animations.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

procedure anAnimationSet.DeleteAnimation(Animation: anAnimation;
  FreeAfter: boolean);
var
  idx: Integer;
  Obj: anAnimation;
begin
  idx := Animations.IndexOfData(Animation);
  if idx >= 0 then begin
    Obj := Animations.Data[idx];
    Animations.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

function anAnimationSet.AddContent(Name: anString;
  ClassOf: anTextureContentClass): anTextureContent;
begin
  Result := ClassOf.Create(Self);
  Result.OnNameChange := OnContentNameChange;
  Contents.Add(Result.GetUniqueName(Name), Result);
end;

function anAnimationSet.AddContent(Content: anTextureContent; Name: anString
  ): anTextureContent;
begin
  Result := Content;
  Contents.Add(Result.GetUniqueName(Name), Content);
end;

procedure anAnimationSet.DeleteContent(Name: anString; FreeAfter: boolean);
var idx: integer;
    Obj: anTextureContent;
begin
  if Contents.Find(Name, idx) then begin
    Obj := Contents.Data[idx];
    Contents.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

procedure anAnimationSet.DeleteContent(Content: anTextureContent;
  FreeAfter: boolean);
var
  idx: Integer;
  Obj: anTextureContent;
begin
  idx := Contents.IndexOfData(Content);
  if idx >= 0 then begin
    Obj := Contents.Data[idx];
    Contents.Delete(idx);
    if FreeAfter then
      Obj.Free;
  end;
end;

procedure anAnimationSet.SaveToFile(Name: anString; FileClass: TFileHandlerClass
  );
var db: TDataBase;
begin
  db := TDataBase.Create(FileClass.Create(Name), 'ANIM', false);

  SaveToRecord(db.Root);

  db.Save;
  db.Free;
end;

procedure anAnimationSet.SaveToRecord(Rec: TDBRecord);
var drec: TDBRecord;
    i: Integer;
begin
  Rec.SetAsInteger(ANIM_VERSION);

  drec := Rec.ChildByName['content'];
  drec.ChildCount := Contents.Count;
  for i := 0 to Contents.Count - 1 do
    Contents.Data[i].SaveToRecord(drec.Child[i]);

  drec := Rec.ChildByName['textures'];
  drec.ChildCount := Textures.Count;
  for i := 0 to Textures.Count - 1 do
    Textures.Data[i].SaveToRecord(drec.Child[i]);

  AnimationLibrary.SaveToRecord(Rec.ChildByName['library']);

  drec := Rec.ChildByName['animations'];
  drec.ChildCount := Animations.Count;

  for i := 0 to Animations.Count - 1 do
    Animations.Data[i].SaveToRecord(drec.Child[i]);
end;

function anAnimationSet.LoadFromFile(Name: anString;
  FileClass: TFileHandlerClass): Boolean;
var db: TDataBase;
begin
  db := TDataBase.Create(FileClass.Create(Name), 'ANIM');
  try
    if db.IsLoadedWith('ANIM') then begin
      Exit(LoadFromRecord(db.Root));
    end;
  except
    Clear;
    Exit(false)
  end;
  Exit(True);
end;

{$IFDEF HANDLER_BUFFER}
function anAnimationSet.LoadFromStream(Stream: TMemoryStream): Boolean;
var db: TDataBase;
begin
  db := TDataBase.Create(TBufferHandler.Create(Stream, nil, false), 'ANIM');
  try
    if db.IsLoadedWith('ANIM') then begin
      Exit(LoadFromRecord(db.Root));
    end;
  except
    Clear;
    Exit(false)
  end;
  Exit(True);
end;
{$ENDIF}

function anAnimationSet.LoadFromRecord(Rec: TDBRecord): Boolean;
var i: Integer;
    tex: anTexture;
    cnt: anTextureContent;
    anim: anAnimation;
    drec: TDBRecord;
begin
  try
    if Rec.AsInteger = ANIM_VERSION then begin
       drec := Rec.ChildByName['content'];
       for i := 0 to drec.ChildCount - 1 do begin
         cnt := AddContent(drec.Child[i].Name, anTextureContentClass(FindClass(drec.Child[i].AsString)));
         cnt.LoadFromRecord(drec.Child[i]);
       end;
       drec := Rec.ChildByName['textures'];
       for i := 0 to drec.ChildCount - 1 do begin
         tex := AddTexture(drec.Child[i].Name, anTextureClass(FindClass(drec.Child[i].AsString)));
         tex.LoadFromRecord(drec.Child[i]);
       end;
       AnimationLibrary.LoadFromRecord(Rec.ChildByName['library']);
       drec := Rec.ChildByName['animations'];
       for i := 0 to drec.ChildCount - 1 do begin
         anim := AddAnimation(drec.Child[i].Name, anAnimationClass(FindClass(drec.Child[i].AsString)));
         anim.LoadFromRecord(drec.Child[i]);
       end;
       for i := 0 to drec.ChildCount - 1 do begin
         Animations.Data[i].PostLoadFromRecord(drec.Child[i]);
       end;
    end else
      Exit(false);
  except
    Clear;
    Exit(false)
  end;
  Exit(True);
end;

procedure anAnimationSet.Clear;
var
  i: Integer;
begin
  for i := 0 to Textures.Count - 1 do
    Textures.Data[i].Free;
  Textures.Clear;
  for i := 0 to Animations.Count - 1 do
    Animations.Data[i].Free;
  Animations.Clear;
  for i := 0 to Contents.Count - 1 do
    Contents.Data[i].Free;
  Contents.Clear;
  AnimationLibrary.Clear;
end;

function anAnimationSet.GetAnimationInstance(AnimationName: anString
  ): anAnimationInstance;
begin
  Result := Animations[AnimationName].GetInstance;
end;

constructor anAnimationSet.Create;
begin
  OnTextureNameChange := nil;
  OnAnimationNameChange := nil;
  OnContentNameChange := nil;
  AnimationLibrary := anLibrary.Create(Self);
  Animations := asAnimationsList.Create;
  Textures := asTexturesList.Create;
  Contents := asContentList.Create;
  FPS := 30;
end;

destructor anAnimationSet.Destroy;
begin
  Clear;
  Contents.Free;
  Textures.Free;
  Animations.Free;
  AnimationLibrary.Free;
  inherited Destroy;
end;

initialization

  RegisterClasses([anFullTexture, anStaticSpriteTexture, anAnimatedSpriteTexture, anAtlasTexture]);
  RegisterClasses([
    anAnimationSymbolObject,
    anAnimationAnimationObject,
    anAnimationPointObject,
    anAnimationLookUpPointObject]);

  RegisterClasses([anTextureContent]);
  RegisterClasses([anAnimationPrototype, anAnimationBlender]);
end.

