unit u_db;

{$mode delphi}

interface

uses
  Classes, SysUtils
  {$IFDEF HANDLER_ZGLFILE}
  {$IFDEF STATIC}
  , zgl_file
  {$ELSE}
  , zglHeader
  {$ENDIF}
  {$ENDIF}
  ;


const
  DB_SIGN = 'DATAFILE';
  DB_VERS = 2;
  DB_DICTIONARY_SIZE = 64;
  DB_DICTIONARY_MIN_CNT = 16;

  ISNONAME_BIT = $80;
  ISNOCHILD_BIT = $40;
  DICTIONARY_BIT = $20;

  EXCLUSIVE_BIT = $FF - ISNONAME_BIT - ISNOCHILD_BIT - DICTIONARY_BIT;

type
  TRecordType = (rtNone, rtData, rtByte, rtInteger, rtSingle, rtDouble, rtBoolean, rtString);

{$IFDEF RECORDTYPES}
var
  RecordTypes: array[rtNone .. rtString] of AnsiString = (
    'None', 'Data', 'Byte', 'Integer', 'Single', 'Double', 'Boolean', 'String'
  );
{$ENDIF}

{$IF DEFINED(HANDLER_FILE) OR DEFINED(HANDLER_ZGLFILE)}
  {$DEFINE HANDLER_BASEFILE}
{$IFEND}

const
  SIGNATURE_SIZE = 8;

type
  TDataMode = (dmRead, dmWrite);
  TSignature = string[SIGNATURE_SIZE];

type
  TDataBase = class;

  { TStreamHandler }

  TStreamHandler = class
    private
      fOpened: Boolean;
    public
      property Opened:Boolean read fOpened write fOpened;

      // Write
      procedure WriteData(const Buf; Size: Cardinal); virtual; abstract;
      procedure WriteByte(i: Byte);
      procedure WriteInteger(i: integer);
      procedure WriteSingle(i: Single);
      procedure WriteDouble(i: Double);
      procedure WriteBoolean(i: boolean);
      procedure WriteString(s: AnsiString);
      // Read
      procedure ReadData(var Buf; Size: Cardinal); virtual; abstract;
      function ReadByte: Byte;
      function ReadInteger: integer;
      function ReadSingle: Single;
      function ReadDouble: Double;
      function ReadBoolean: boolean;
      function ReadString: AnsiString;

      procedure Open(Mode: TDataMode); virtual;
      procedure Close; virtual;
  end;

  {$IFDEF HANDLER_BUFFER}

  { TBufferHandler }

  TBufferHandler = class(TStreamHandler)
    private
      fOwnsObjects: Boolean;
      fReadBuffer: TStream;
      fWriteBuffer: TStream;
    public
      property OwnsObjects: Boolean read fOwnsObjects write fOwnsObjects;
      property ReadBuffer: TStream read fReadBuffer write fReadBuffer;
      property WriteBuffer: TStream read fWriteBuffer write fWriteBuffer;

      // Write
      procedure WriteData(const Buf; Size: Cardinal); override;
      // Read
      procedure ReadData(var Buf; Size: Cardinal); override;

      constructor Create(ReadBuf, WriteBuf: TStream; pOwnsObjects: Boolean = true);
      destructor Destroy; override;
  end;
  {$ENDIF}

  {$IFDEF HANDLER_BASEFILE}

  { TBaseFileHandler }

  TBaseFileHandler = class(TStreamHandler)
    private
      fFileName: AnsiString;
    public
      property FileName: AnsiString read fFileName write fFileName;

      constructor Create(pFileName: AnsiString); virtual;
      destructor Destroy; override;
  end;

  TFileHandlerClass = class of TBaseFileHandler;
  {$ENDIF}

  {$IFDEF HANDLER_FILE}

  { TFileHandler }

  TFileHandler = class(TBaseFileHandler)
    private
      Stream: File of Byte;
    public

      // Write
      procedure WriteData(const Buf; Size: Cardinal); override;
      // Read
      procedure ReadData(var Buf; Size: Cardinal); override;

      procedure Open(Mode: TDataMode); override;
      procedure Close; override;

      constructor Create(pFileName: AnsiString); override;
      destructor Destroy; override;
  end;
  {$ENDIF}
  {$IFDEF HANDLER_ZGLFILE}

  { TZGLFileHandler }

  TZGLFileHandler = class(TBaseFileHandler)
    private
      Stream: zglTFile;
    public

      // Write
      procedure WriteData(const Buf; Size: Cardinal); override;
      // Read
      procedure ReadData(var Buf; Size: Cardinal); override;

      procedure Open(Mode: TDataMode); override;
      procedure Close;  override;

      constructor Create(FileName: AnsiString); override;
      destructor Destroy; override;
  end;
  {$ENDIF}

  { TDBRecord }

  TDBRecord = class(TObject)
  private
    fRecordType: TRecordType;
    fName: AnsiString;
    fDataLength: Cardinal;
    fChild: TList;
    fParent: TDBRecord;
    fDataBase: TDataBase;

    Data: pointer;

    function getChild(index: integer): TDBRecord;
    function getChildByName(name: AnsiString): TDBRecord;
    function getChildCount: integer;
    function getChildCountWithChild: integer;
    function getChildExists(name: AnsiString): boolean;

    procedure setChild(index: integer; const AValue: TDBRecord);
    procedure setChildCount(const AValue: integer);

    function CheckRT(RT: TRecordType): Boolean;
    procedure ForceRT(RT: TRecordType);

    procedure InitChild;
    procedure CountStringValue(AValue: AnsiString; var cnt: Integer);
    procedure FillDictionary;
  public
    procedure Read(Handler: TStreamHandler);
    procedure Write(Handler: TStreamHandler);

    property DataBase: TDataBase read fDataBase write fDataBase;
    property Parent: TDBRecord read fParent write fParent;
    property RecordType: TRecordType read fRecordType write fRecordType;
    property DataLength: Cardinal read fDataLength write fDataLength;
    property DataPointer: pointer read Data write Data;
    property Name: AnsiString read fName write fName;

    // Child
    procedure AddChild(Child: TDBRecord);
    procedure AddChildBefore(Child, Before: TDBRecord);
    procedure AddChildAfter(Child, After: TDBRecord);
    procedure RemoveChild(Child: TDBRecord; FreeAfter: Boolean = true);
    procedure RemoveChildByName(Name: AnsiString; FreeAfter: Boolean = true);
    procedure RemoveChildByIndex(Index: Integer; FreeAfter: Boolean = true);

    property ChildCount: integer read getChildCount write setChildCount;
    property ChildCountWithChild: integer read getChildCountWithChild;
    property Child[index: integer]: TDBRecord read getChild write setChild;
    property ChildByName[name: AnsiString]: TDBRecord read getChildByName;
    property ChildExists[name: AnsiString]: boolean read getChildExists;

    // Set
    procedure SetAsNone;
    procedure SetAsData(const Buf; Size: Cardinal);
    procedure SetAsByte(i: Byte);
    procedure SetAsInteger(i: integer);
    procedure SetAsSingle(i: Single);
    procedure SetAsDouble(i: Double);
    procedure SetAsBoolean(i: boolean);
    procedure SetAsString(s: AnsiString);

    // Get
    function GetAsData(var Buf): Cardinal;
    function GetAsByte: Byte;
    function GetAsInteger: integer;
    function GetAsSingle: Single;
    function GetAsDouble: Double;
    function GetAsBoolean: boolean;
    function GetAsString: AnsiString;
    function GetAsXml: AnsiString;

    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsString: AnsiString read GetAsString write SetAsString;
    property AsXml: AnsiString read GetAsXml;

    constructor Create(pDataBase: TDataBase; pParent: TDBRecord = nil; RT: TRecordType = rtNone);
    destructor Destroy; override;
  end;

  { TDataBase }

  TDataBase = class(TObject)
  private
    fHandler: TStreamHandler;
    fRootNodeName: AnsiString;
    fRoot: TDBRecord;
    fLoaded: boolean;
    fDictionary: TStringList;
  public
    property RootNodeName: AnsiString read fRootNodeName write fRootNodeName;
    property Handler: TStreamHandler read fHandler write fHandler;
    property Dictionary: TStringList read fDictionary write fDictionary;
    property Root: TDBRecord read fRoot write fRoot;
    property Loaded:Boolean read fLoaded write fLoaded;

    function IsLoadedWith(RootName: AnsiString): boolean;

    procedure Load;
    procedure Save;
    procedure Clear;

    constructor Create(pHandler: TStreamHandler; pRootNode: AnsiString = 'ROOT'; pLoad: boolean = true);
    destructor Destroy; override;
  end;

implementation

{$IFDEF HANDLER_BUFFER}
procedure TBufferHandler.WriteData(const Buf; Size: Cardinal);
begin
  WriteBuffer.Write(Buf, Size);
end;

procedure TBufferHandler.ReadData(var Buf; Size: Cardinal);
begin
  ReadBuffer.Read(Buf, Size);
end;

constructor TBufferHandler.Create(ReadBuf, WriteBuf: TStream;
  pOwnsObjects: Boolean);
begin
  ReadBuffer := ReadBuf;
  WriteBuffer := WriteBuf;
  OwnsObjects := pOwnsObjects;
end;

destructor TBufferHandler.Destroy;
begin
  if OwnsObjects then begin
    if Assigned(ReadBuffer) then ReadBuffer.Free;
    if Assigned(WriteBuffer) and (ReadBuffer <> WriteBuffer) then WriteBuffer.Free;
  end;
  inherited Destroy;
end;
{$ENDIF}

{$IFDEF HANDLER_BASEFILE}
constructor TBaseFileHandler.Create(pFileName: AnsiString);
begin
  FileName := pFileName;
end;

destructor TBaseFileHandler.Destroy;
begin
  Close;
  inherited Destroy;
end;

{$ENDIF}

{$IFDEF HANDLER_ZGLFILE}
procedure TZGLFileHandler.ReadData(var Buf; Size: Cardinal);
begin
  file_Read(Stream, Buf, Size);
end;

procedure TZGLFileHandler.Open(Mode: TDataMode);
begin
  if Opened then begin
    Close;
  end;
  if file_Exists(FileName) then begin
    case Mode of
      dmRead: begin
        Opened := file_Open(Stream, FileName, FOM_OPENR);
      end;
      dmWrite: begin
        Opened := file_Open(Stream, FileName, FOM_CREATE);
      end;
    end;
    inherited Open(Mode);
  end else begin
    Close;
  end;
end;

procedure TZGLFileHandler.Close;
begin
  if Opened then begin
    file_Close(Stream);
    inherited Close;
  end;
end;

procedure TZGLFileHandler.WriteData(const Buf; Size: Cardinal);
begin
  file_Write(Stream, Buf, Size);
end;

constructor TZGLFileHandler.Create(FileName: AnsiString);
begin
  inherited Create(FileName);
end;

destructor TZGLFileHandler.Destroy;
begin
  inherited;
end;
{$ENDIF}

{$IFDEF HANDLER_FILE}
procedure TFileHandler.ReadData(var Buf; Size: Cardinal);
begin
  try
    BlockRead(Stream, Buf, Size);
  except
    Close;
  end;
end;

procedure TFileHandler.Open(Mode: TDataMode);
var CheckSign: TSignature;
begin
  if Opened then begin
    Close;
  end;
  AssignFile(Stream, FileName);
  try
    Opened := true;
    case Mode of
      dmRead: begin
        if FileExists(FileName) then begin
          Reset(Stream);
        end else begin
          Opened := false;
        end;
      end;
      dmWrite: begin
        //ForceDirectories(ExtractFileDir(FileName));
        Rewrite(Stream);
      end;
    end;
  except
    Close;
  end;
  inherited Open(Mode);
end;

procedure TFileHandler.Close;
begin
  if Opened then begin
    CloseFile(Stream);
    inherited Close;
  end;
end;

procedure TFileHandler.WriteData(const Buf; Size: Cardinal);
begin
  BlockWrite(Stream, Buf, Size);
end;

constructor TFileHandler.Create(pFileName: AnsiString);
begin
  inherited Create(pFileName);
end;

destructor TFileHandler.Destroy;
begin
  Close;
  inherited;
end;
{$ENDIF}

{ TStreamHandler }


procedure TStreamHandler.Open(Mode: TDataMode);
var CheckSign: TSignature;
begin
  if Opened then begin
    case Mode of
      dmRead: begin
        SetLength(CheckSign, SIGNATURE_SIZE);
        ReadData(CheckSign[1], SIGNATURE_SIZE);
        if (CheckSign <> DB_SIGN) then begin
          Close;
        end else begin
          if ReadInteger <> DB_VERS then begin
            Close;
          end else begin
          end;
        end;
      end;
      dmWrite: begin
        WriteData(DB_SIGN, SIGNATURE_SIZE);
        WriteInteger(DB_VERS);
      end;
    end;
  end;
end;

procedure TStreamHandler.Close;
begin
  Opened := false;
end;

function TStreamHandler.ReadByte: Byte;
begin
  ReadData(Result, 1);
end;

function TStreamHandler.ReadBoolean: Boolean;
begin
  ReadData(Result, 1);
end;

function TStreamHandler.ReadInteger: Integer;
begin
  ReadData(Result, 4);
end;

function TStreamHandler.ReadString: AnsiString;
var i: integer;
begin
  i := ReadInteger;
  SetLength(Result, i);
  if i > 0 then
     ReadData(Result[1], i);
end;

function TStreamHandler.ReadSingle: Single;
begin
  ReadData(Result, 4);
end;

function TStreamHandler.ReadDouble: Double;
begin
  ReadData(Result, 8);
end;

procedure TStreamHandler.WriteByte(i: Byte);
begin
  WriteData(i, 1);
end;

procedure TStreamHandler.WriteBoolean(i: Boolean);
begin
  WriteData(i, 1);
end;

procedure TStreamHandler.WriteInteger(i: Integer);
begin
  WriteData(i, 4);
end;

procedure TStreamHandler.WriteString(s: AnsiString);
begin
  WriteInteger(Length(s));
  WriteData(s[1], Length(s));
end;

procedure TStreamHandler.WriteSingle(i: Single);
begin
  WriteData(i, 4);
end;

procedure TStreamHandler.WriteDouble(i: Double);
begin
  WriteData(i, 8);
end;

{ TDBRecord }

procedure TDBRecord.Read(Handler: TStreamHandler);
var s: AnsiString;
    i: integer;
    RT: Byte;
    HasName, HasChild, Dictionary: boolean;
begin
  RT := Handler.ReadByte;
  HasName := (RT and ISNONAME_BIT) = 0;
  HasChild := (RT and ISNOCHILD_BIT) = 0;
  Dictionary := (RT and DICTIONARY_BIT) > 0;
  RecordType := TRecordType(RT and EXCLUSIVE_BIT);
  if HasChild then
    ChildCount := Handler.ReadInteger;
  if HasName then
    Name := Handler.ReadString;

  if DataLength > 0 then begin
    FreeMem(Data, DataLength);
    DataLength := 0;
  end;

  case RecordType of
    rtData: begin
      DataLength := Handler.ReadInteger;
      GetMem(Data, DataLength);
      Handler.ReadData(Data^, DataLength);
    end;
    rtByte: begin
      DataLength := 1;
      GetMem(Data, DataLength);
      PByte(Data)^ := Handler.ReadByte;
    end;
    rtInteger: begin
      DataLength := 4;
      GetMem(Data, DataLength);
      PInteger(Data)^ := Handler.ReadInteger;
    end;
    rtSingle: begin
      DataLength := 4;
      GetMem(Data, DataLength);
      PSingle(Data)^ := Handler.ReadSingle;
    end;
    rtDouble: begin
      DataLength := 8;
      GetMem(Data, DataLength);
      PDouble(Data)^ := Handler.ReadDouble;
    end;
    rtBoolean: begin
      DataLength := 1;
      GetMem(Data, DataLength);
      PBoolean(Data)^ := Handler.ReadBoolean;
    end;
    rtString: begin
      if Dictionary then begin
        s := DataBase.Dictionary.Strings[Handler.ReadByte];
      end else
        s := Handler.ReadString;
      DataLength := Length(s);
      GetMem(Data, DataLength);
      if DataLength > 0 then
         Move(S[1], Data^, DataLength);
    end;
  end;

  if HasChild then
    for i := 0 to ChildCount - 1 do begin
      Child[i].Read(Handler);
    end;
end;

procedure TDBRecord.setChild(index: integer; const AValue: TDBRecord);
begin
  fChild.Items[index] := AValue;
end;

procedure TDBRecord.setChildCount(const AValue: integer);
var i, cc: integer;
begin
  InitChild;
  cc := fChild.Count;
  if cc < AValue then begin
    fChild.Count := AValue;
    for i := cc to AValue - 1 do begin
      Child[i] := TDBRecord.Create(DataBase, Self);
    end;
  end;
  if cc > AValue then begin
    for i := cc - 1 downto AValue do begin
      Child[i].Free;
    end;
    fChild.Count := AValue;
  end;
end;

function TDBRecord.getChild(index: integer): TDBRecord;
begin
  Result := TDBRecord(fChild.Items[index]);
end;

function TDBRecord.GetAsXml: AnsiString;
var i: integer;
begin
  Result := '<item name="' + Name + '" value="' + AsString + '">';
  for i := 0 to ChildCount - 1 do
    Result += Child[i].AsXml;
  Result += '</item>';
end;

function TDBRecord.getChildByName(name: AnsiString): TDBRecord;
var i, p: integer;
begin
  InitChild;
  p := Pos('.', name);
  if p <> 0 then begin
    result := ChildByName[Copy(name, 1, p - 1)].ChildByName[Copy(name, p + 1, length(name))];
    exit;
  end;
  for i := 0 to ChildCount - 1 do begin
    if Child[i].Name = name then begin
      Result := Child[i];
      exit;
    end;
  end;
  Result := TDBRecord.Create(DataBase, Self);
  AddChild(Result);
  Result.Name := Name;
end;

function TDBRecord.getChildCount: integer;
begin
  if Assigned(fChild) then
   Result := fChild.Count
  else
    Result := 0;
end;

function TDBRecord.getChildCountWithChild: integer;
var i: integer;
begin
  Result := ChildCount;
  for i := 0 to ChildCount - 1 do begin
    inc(Result, Child[i].ChildCountWithChild);
  end;
end;

function TDBRecord.getChildExists(name: AnsiString): boolean;
var i, p: integer;
begin
  InitChild;
  p := Pos('.', name);
  Result := false;
  if p <> 0 then begin
    result := ChildByName[Copy(name, 1, p - 1)].ChildExists[Copy(name, p + 1, length(name))];
    exit;
  end;
  for i := 0 to ChildCount - 1 do begin
    if Child[i].Name = name then begin
      Result := true;
      exit;
    end;
  end;
end;

procedure TDBRecord.Write(Handler: TStreamHandler);
var i: integer;
    s: AnsiString;
    RT: Byte;
    HasName, HasChild, Dictionary: boolean;
    idx: Byte;
begin
  HasName := Name <> '';
  HasChild := ChildCount > 0;

  if RecordType = rtString then begin
    Dictionary := DataBase.Dictionary.Find(AsString, i);
    if Dictionary then
      idx := i;
  end else
    Dictionary := false;

  RT := Byte(RecordType) or
    ISNONAME_BIT * Byte(not HasName) or
    ISNOCHILD_BIT * Byte(not HasChild) or
    DICTIONARY_BIT * Byte(Dictionary);

  Handler.WriteByte(RT);
  if (HasChild) then
    Handler.WriteInteger(ChildCount);
  if (HasName) then
    Handler.WriteString(Name);

  case RecordType of
    rtData: begin
      Handler.WriteInteger(DataLength);
      Handler.WriteData(Data^, DataLength);
    end;
    rtByte: begin
      Handler.WriteByte(PByte(Data)^);
    end;
    rtInteger: begin
      Handler.WriteInteger(PInteger(Data)^);
    end;
    rtSingle: begin
      Handler.WriteSingle(PSingle(Data)^);
    end;
    rtDouble: begin
      Handler.WriteDouble(PDouble(Data)^);
    end;
    rtBoolean: begin
      Handler.WriteBoolean(PBoolean(Data)^);
    end;
    rtString: begin
      SetLength(s, DataLength);
      if (Dictionary) then
         Handler.WriteByte(idx)
      else begin
        if DataLength <> 0 then begin
          Move(Data^, s[1], DataLength);
          Handler.WriteString(s);
        end else
          Handler.WriteString('');
      end;
    end;
  end;

  if HasChild then
    for i := 0 to ChildCount - 1 do begin
      Child[i].Write(Handler);
    end;
end;

procedure TDBRecord.FillDictionary;
var
  i, idx: Integer;
  s: AnsiString;
  cnt, cnt_val: Integer;
  min_idx: Integer;
  min_value: Integer;
begin
  if RecordType = rtString then
    with DataBase.Dictionary do begin
      s := AsString;
      if not Find(s, i) then begin
        cnt := 0;
        DataBase.Root.CountStringValue(s, cnt);
        if cnt * Length(s) >= DB_DICTIONARY_MIN_CNT then begin
          if Count < DB_DICTIONARY_SIZE then begin
            AddObject(s, TObject(cnt));
          end else begin
            min_idx := -1;
            min_value := 255;
            for i := 0 to DB_DICTIONARY_SIZE - 1 do begin
              cnt_val := Integer(Objects[i]);
              if (cnt_val < min_value) and (cnt > cnt_val) then begin
                min_value := cnt_val;
                min_idx := i;
              end;
            end;
            if min_idx >= 0 then begin
              Delete(min_idx);
              AddObject(s, TObject(cnt));
            end;
          end;
        end;
      end;
    end;
  for i := 0 to ChildCount - 1 do begin
    Child[i].FillDictionary;
  end;
end;

function TDBRecord.CheckRT(RT: TRecordType): Boolean;
begin
  Result := RecordType = RT;
end;

procedure TDBRecord.ForceRT(RT: TRecordType);
begin
  if RecordType <> RT then begin
    RecordType := RT;
    if DataLength > 0 then begin
      FreeMem(Data, DataLength);
      DataLength := 0;
    end;
    case RecordType of
      rtNone: begin
        DataLength := 0;
      end;
      rtByte: begin
        DataLength := 1;
      end;
      rtInteger: begin
        DataLength := 4;
      end;
      rtSingle: begin
        DataLength := 4;
      end;
      rtDouble: begin
        DataLength := 8;
      end;
      rtBoolean: begin
        DataLength := 1;
      end;
    end;

    if DataLength > 0 then
      GetMem(Data, DataLength);
  end;
end;

procedure TDBRecord.InitChild;
begin
  if not Assigned(fChild) then begin
    fChild := TList.Create;
  end;
end;

procedure TDBRecord.CountStringValue(AValue: AnsiString; var cnt: Integer);
var
  i: Integer;
begin
  if RecordType = rtString then begin
    if AsString = AValue then inc(cnt);
  end;
  for i := 0 to ChildCount - 1 do
    Child[i].CountStringValue(AValue, cnt);
end;

procedure TDBRecord.AddChild(Child: TDBRecord);
begin
  InitChild;
  Child.Parent := self;
  fChild.Add(Child);
end;

procedure TDBRecord.AddChildBefore(Child, Before: TDBRecord);
var
  i: Integer;
begin
  InitChild;
  Child.Parent := self;
  i := fChild.IndexOf(Before) - 1;
  if (i < 0) then i := 0;
  fChild.Insert(i, Child);
end;

procedure TDBRecord.AddChildAfter(Child, After: TDBRecord);
begin
  InitChild;
  Child.Parent := self;
  fChild.Insert(fChild.IndexOf(After), Child);
end;

procedure TDBRecord.RemoveChild(Child: TDBRecord; FreeAfter: Boolean);
begin
  InitChild;
  fChild.Remove(Child);
  Child.Parent := nil;
  if FreeAfter then
     Child.Free;
end;

procedure TDBRecord.RemoveChildByName(Name: AnsiString; FreeAfter: Boolean);
begin
  if ChildExists[Name] then
    RemoveChild(ChildByName[Name], FreeAfter);
end;

procedure TDBRecord.RemoveChildByIndex(Index: Integer; FreeAfter: Boolean);
begin
  RemoveChild(Child[Index], FreeAfter);
end;

procedure TDBRecord.SetAsData(const Buf; Size: Cardinal);
begin
  ForceRT(rtData);
  if DataLength > 0 then
    FreeMem(Data, DataLength);
  DataLength := Size;
  GetMem(Data, DataLength);
  Move(Buf, Data^, DataLength);
end;

procedure TDBRecord.SetAsNone;
begin
  ForceRT(rtNone);
end;

procedure TDBRecord.SetAsByte(i: Byte);
begin
  ForceRT(rtByte);
  PByte(Data)^ := i;
end;

procedure TDBRecord.SetAsInteger(i: integer);
begin
  ForceRT(rtInteger);
  PInteger(Data)^ := i;
end;

procedure TDBRecord.SetAsSingle(i: Single);
begin
  ForceRT(rtSingle);
  PSingle(Data)^ := i;
end;

procedure TDBRecord.SetAsDouble(i: Double);
begin
  ForceRT(rtDouble);
  PDouble(Data)^ := i;
end;

procedure TDBRecord.SetAsBoolean(i: boolean);
begin
  ForceRT(rtBoolean);
  PBoolean(Data)^ := i;
end;

procedure TDBRecord.SetAsString(s: AnsiString);
begin
  ForceRT(rtString);
  if DataLength > 0 then
    FreeMem(Data, DataLength);
  DataLength := length(s);
  if DataLength <> 0 then begin
    GetMem(Data, DataLength);
    Move(s[1], Data^, DataLength);
  end;
end;

function TDBRecord.GetAsData(var Buf): Cardinal;
begin
  if CheckRT(rtData) then begin
    Result := DataLength;
    Move(Data^, Buf, DataLength);
  end;
end;

function TDBRecord.GetAsByte: Byte;
begin
  if CheckRT(rtByte) then begin
    Result := PByte(Data)^;
  end else begin
    case RecordType of
      rtInteger: Result := GetAsInteger();
      rtSingle: Result := Round(GetAsSingle());
      rtDouble: Result := Round(GetAsDouble());
      rtBoolean: Result := Byte(GetAsBoolean());
      rtString: Result := StrToIntDef(GetAsString(), 0);
      rtData: begin
        if DataLength > 0 then
          Result := PByte(DataPointer)^
        else
          Result := 0;
      end
        else Result := 0;
    end;
  end;
end;

function TDBRecord.GetAsInteger: integer;
begin
  if CheckRT(rtInteger) then begin
    Result := PInteger(Data)^;
  end else begin
    case RecordType of
      rtByte: Result := GetAsByte();
      rtSingle: Result := Round(GetAsSingle());
      rtDouble: Result := Round(GetAsDouble());
      rtBoolean: Result := Byte(GetAsBoolean());
      rtString: Result := StrToIntDef(GetAsString(), 0);
      rtData: begin
        if DataLength >= 4 then
          Result := PInteger(DataPointer)^
        else
          Result := 0;
      end
        else Result := 0;
    end;
  end;
end;

function TDBRecord.GetAsSingle: Single;
begin
  if CheckRT(rtSingle) then begin
    Result := PSingle(Data)^;
  end else begin
    case RecordType of
      rtByte: Result := GetAsByte();
      rtInteger: Result := GetAsInteger();
      rtDouble: Result := GetAsDouble();
      rtBoolean: Result := Byte(GetAsBoolean());
      rtString: Result := StrToFloatDef(GetAsString(), 0);
      rtData: begin
        if DataLength >= 4 then
          Result := PSingle(DataPointer)^
        else
          Result := 0;
      end
        else Result := 0;
    end;
  end;
end;

function TDBRecord.GetAsDouble: Double;
begin
  if CheckRT(rtDouble) then begin
    Result := PDouble(Data)^;
  end else begin
    case RecordType of
      rtByte: Result := GetAsByte();
      rtInteger: Result := GetAsInteger();
      rtSingle: Result := GetAsSingle();
      rtBoolean: Result := Byte(GetAsBoolean());
      rtString: Result := StrToFloatDef(GetAsString(), 0);
      rtData: begin
        if DataLength >= 8 then
          Result := PDouble(DataPointer)^
        else
          Result := 0;
      end
        else Result := 0;
    end;
  end;
end;

function TDBRecord.GetAsBoolean: boolean;
begin
  if CheckRT(rtBoolean) then begin
    Result := PBoolean(Data)^;
  end else begin
    case RecordType of
      rtByte: Result := GetAsByte() <> 0;
      rtInteger: Result := GetAsInteger() <> 0;
      rtSingle: Result := GetAsSingle() <> 0;
      rtDouble: Result := GetAsDouble() <> 0;
      rtString: Result := StrToBoolDef(GetAsString(), false);
      rtData: begin
        if DataLength >= 1 then
          Result := PBoolean(DataPointer)^
        else
          Result := false;
      end
        else Result := false;
    end;
  end;
end;

function TDBRecord.GetAsString: AnsiString;
begin
  if CheckRT(rtString) then begin
    SetLength(Result, DataLength);
    if DataLength > 0 then
      Move(Data^, Result[1], DataLength);
  end else begin
    case RecordType of
      rtByte: Result := IntToStr(GetAsByte());
      rtInteger: Result := IntToStr(GetAsInteger());
      rtSingle: Result := FloatToStr(GetAsSingle());
      rtDouble: Result := FloatToStr(GetAsDouble());
      rtBoolean: Result := BoolToStr(GetAsBoolean(), true);
      rtData: begin
        SetLength(Result, DataLength);
        if DataLength > 0 then
          Move(DataPointer^, Result[1], DataLength);
      end
        else Result := '';
    end;
  end;
end;

constructor TDBRecord.Create(pDataBase: TDataBase; pParent: TDBRecord;
  RT: TRecordType);
begin
  RecordType := rtNone;
  DataLength := 0;
  Data := nil;
  Parent := pParent;
  DataBase := pDataBase;

  Name := '';
  fChild := nil;

  ForceRT(RT);
end;

destructor TDBRecord.Destroy;
var i: integer;
begin
  FreeMem(Data, DataLength);

  if Assigned(fChild) then begin
    for i := 0 to ChildCount - 1 do begin
      Child[i].Free;
    end;
  end;
  inherited Destroy;
end;

{ TDataBase }

function TDataBase.IsLoadedWith(RootName: AnsiString): boolean;
begin
  Result := Loaded and (Root.Name = RootName);
end;

procedure TDataBase.Load;
var
  dictSize: Byte;
  i: Integer;
begin
  Clear;
  Handler.Open(dmRead);
  if Handler.Opened then begin
    dictSize := Handler.ReadByte;
    if dictSize > 0 then
      for i := 0 to dictSize - 1 do begin
        Dictionary.Add(Handler.ReadString);
      end;
    Root := TDBRecord.Create(Self, nil);
    Root.Read(Handler);
    Loaded := true;
  end;
end;

procedure TDataBase.Save;
var
  i: Integer;
begin
  Handler.Open(dmWrite);
  if Handler.Opened then begin
    Dictionary.Clear;
    Root.FillDictionary;
    Handler.WriteByte(Dictionary.Count);
    for i := 0 to Dictionary.Count - 1 do
      Handler.WriteString(Dictionary.Strings[i]);
    if Assigned(Root) then
      Root.Write(Handler);
  end;
end;

procedure TDataBase.Clear;
begin
  Dictionary.Clear;
  if Assigned(Root) then begin
    Root.Free;
    Root := TDBRecord.Create(Self, nil);
    Root.Name := RootNodeName;
  end;
end;

constructor TDataBase.Create(pHandler: TStreamHandler; pRootNode: AnsiString;
  pLoad: boolean);
begin
  Loaded := false;
  Handler := pHandler;
  RootNodeName := pRootNode;
  Dictionary := TStringList.Create;
  Dictionary.CaseSensitive := true;
  Dictionary.Sorted := true;
  Root := TDBRecord.Create(Self, nil);
  Root.Name := RootNodeName;
  if pLoad then
     Load;
end;

destructor TDataBase.Destroy;
begin
  Handler.Free;
  Clear;
  Dictionary.Free;
  inherited Destroy;
end;

end.

