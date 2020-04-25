// Baseado em: https://youtu.be/07fwLbnb3ns

{$IFDEF NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

unit FileSelectFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Generics.Defaults,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  System.ImageList, FMX.ImgList, FMX.Edit, FMX.Controls.Presentation,
  FMX.ListView;

const
{$IfDef MSWINDOWS}
  CAllFiles = '*.*';
{$Else}
  CAllFiles = '*';
{$EndIf}

type
  TFileInfo = class
  private
    FIsDirectory: Boolean;
    FName: String;
    FSize: Int64;
    FData: TDateTime;
  public
    constructor Create(const aFileName: String; const IsDir: Boolean;
      const ADate: TDateTime; const ASize: Int64);
    function FormatByteSize: String;
    function FormatDateTime: String;

    property Name: String read FName;
    property Size: Int64 read FSize;
    property Data: TDateTime read FData;
    property IsDirectory: Boolean read FIsDirectory;
  end;

  TFileList = class(TObjectList<TFileInfo>)
  public
    constructor Create;
    procedure AddFilesToList(APath: String;
      Attr: Integer = faAnyFile; ShowHidden: Boolean = False);
  end;

  TFrameFileSelect = class(TFrame)
    lvFileBrowse: TListView;
    tbTop: TToolBar;
    edDirectory: TEdit;
    sbReload: TSpeedButton;
    lExt: TLabel;
    ImageList1: TImageList;
    procedure lExtClick(Sender: TObject);
    procedure sbReloadClick(Sender: TObject);
    procedure lvFileBrowseDblClick(Sender: TObject);
    procedure lvFileBrowseItemClickEx(const Sender: TObject; ItemIndex: Integer;
      const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
  private
    { Private declarations }
    FInitialDir: String;
    FFileName: String;
    FShowHidden: Boolean;
    FFileMask: String;

    procedure SetInitialDir(const Value: String);

    procedure SetFileMask(const Value: String);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
    function ChangeDirectory(const ADirectory: String): Boolean;
    procedure SelectItem(const AItem: Integer);

    property InitialDir: String read FInitialDir write SetInitialDir;
    property FileName: String read FFileName write FFileName;
    property ShowHidden: Boolean read FShowHidden write FShowHidden;
    property FileMask: String read FFileMask write SetFileMask;
  end;

function CompareDirFiles(const Left, Right: TFileInfo): Integer;


implementation

uses
  System.IOUtils, System.Math, System.StrUtils;

{$R *.fmx}

function CompareDirFiles(const Left, Right: TFileInfo): Integer;
var
  Nome1, Nome2: String;
begin
  Nome1 := ifthen(Left.IsDirectory,'0','1')+Left.Name.ToUpper;
  Nome2 := ifthen(Right.IsDirectory,'0','1')+Right.Name.ToUpper;

  if Nome1 > Nome2 then
    Result := 1
  else if Nome1 < Nome2 then
    Result := -1
  else
    Result := 0;
end;

{ TFileInfo }

constructor TFileInfo.Create(const aFileName: String; const IsDir: Boolean;
  const ADate: TDateTime; const ASize: Int64);
begin
  FName := ExtractFileName(aFileName);
  FIsDirectory := IsDir;
  FData := ADate;
  FSize := ASize
end;

function TFileInfo.FormatByteSize: String;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if IsDirectory then
     Result := ''
  else if Size > GB then
    Result := FormatFloat('#.## GB',Size/GB)
  else if Size > MB then
    Result := FormatFloat('#.## MB',Size/MB)
  else if Size > KB then
    Result := FormatFloat('#.## KB',Size/KB)
  else if Size > 0 then
    Result := FormatFloat('#.## bytes',Size)
  else
    Result := 'n/d';
end;

function TFileInfo.FormatDateTime: String;
begin
  if Data <> 0 then
    Result := DateTimeToStr( Data )
  else
    Result := 'n/d';
end;

{ TFileList }

constructor TFileList.Create;
begin
  inherited Create(TComparer<TFileInfo>.Construct( CompareDirFiles ), True)
end;

procedure TFileList.AddFilesToList(APath: String; Attr: Integer; ShowHidden: Boolean);
var
  LastFile: string;
  RetFind: Integer;
  SearchRec: TSearchRec;
  FI: TFileInfo;
begin
  LastFile := '';
  RetFind := FindFirst(APath, Attr, SearchRec);
  try
    while (RetFind = 0) and (LastFile <> SearchRec.Name) do
    begin
      LastFile := SearchRec.Name;
      if ((SearchRec.Attr = 0) or ((SearchRec.Attr and Attr) <> 0)) and
         (ShowHidden or ((SearchRec.Attr and faHidden) = 0)) then
      begin
        FI := TFileInfo.Create( LastFile,
                                ((SearchRec.Attr and faDirectory) <> 0),
                                SearchRec.TimeStamp, SearchRec.Size);
        Add(FI);
      end;
      FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

{ TFrameFileSelect }

procedure TFrameFileSelect.SetInitialDir(const Value: String);
begin
  if Visible then
    ChangeDirectory(Value);
end;

function TFrameFileSelect.ChangeDirectory(const ADirectory: String): Boolean;
var
  FI: TFileInfo;
  FL: TFileList;
  LVI: TListViewItem;
begin
  Result := False;
  if (ADirectory = EmptyStr) or (not DirectoryExists(ADirectory)) then
    Exit;

  edDirectory.Text := ADirectory;
  FInitialDir := ADirectory;

  lvFileBrowse.BeginUpdate;
  FL := TFileList.Create;
  try
    lvFileBrowse.Items.Clear;

    //CapDir := ReverseString(ADirectory);
    //D Caption := ReverseString(copy(CapDir, 1, pos(PathDelim, CapDir)));

    if FFileMask = EmptyStr then
      FFileMask := CAllFiles;

    if FFileMask = CAllFiles then
      FL.AddFilesToList(TPath.Combine(FInitialDir, CAllFiles))
    else
    begin
      FL.AddFilesToList(TPath.Combine(FInitialDir, CAllFiles), faDirectory);
      FL.AddFilesToList(TPath.Combine(FInitialDir, FFileMask), (faAnyFile - faDirectory));
    end;

    FL.Sort();

    for FI in FL do
    begin
      LVI := lvFileBrowse.Items.Add;
      LVI.Data['FileName'] := FI.Name;
      LVI.Data['FileSize'] := FI.FormatByteSize;
      LVI.Data['DateTime'] := FI.FormatDateTime;
      LVI.Data['ImgFileDir'] := ifthen(FI.IsDirectory, 1, 0);
    end;

  finally
    lvFileBrowse.EndUpdate;
    FL.Free;
  end;

  Result := True;
end;

constructor TFrameFileSelect.Create(AOwner: TComponent);
begin
  inherited;

  FFileName := '';
  FInitialDir := TPath.GetHomePath;
  FShowHidden := False;
  FFileMask := CAllFiles;
end;

procedure TFrameFileSelect.Execute;
begin
  if not ChangeDirectory(FInitialDir) then
    ChangeDirectory(TPath.GetHomePath);
end;

procedure TFrameFileSelect.lExtClick(Sender: TObject);
var
  NewDir: String;
begin
  if SelectDirectory('Select Directory', FInitialDir, NewDir) then
    ChangeDirectory(NewDir);
end;

procedure TFrameFileSelect.lvFileBrowseDblClick(Sender: TObject);
var
  I: Integer;
begin
  if lvFileBrowse.Selected = nil then
    Exit;

  I := lvFileBrowse.Selected.Index;
  SelectItem(I);
end;

procedure TFrameFileSelect.lvFileBrowseItemClickEx(const Sender: TObject;
  ItemIndex: Integer; const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
begin
  SelectItem(ItemIndex);
end;

procedure TFrameFileSelect.sbReloadClick(Sender: TObject);
begin
  if ChangeDirectory(edDirectory.Text) then
    edDirectory.Text := FInitialDir;
end;

procedure TFrameFileSelect.SelectItem(const AItem: Integer);
var
  ItemText, NewDir: String;
begin
  FFileName := '';

  with lvFileBrowse.Items[AItem] do
  begin
    ItemText := Data['FileName'].ToString;

    if (Data['ImgFileDir'].AsInteger = 1) then
    begin
      if (ItemText = '.') then
        NewDir := FInitialDir
      else if (ItemText = '..') then
      begin
        NewDir := ReverseString(FInitialDir);
        NewDir := ReverseString(copy(NewDir, pos(PathDelim, NewDir)+1));
      end
      else
        NewDir := TPath.Combine(FInitialDir, ItemText);

      ChangeDirectory(NewDir);
    end
    else
      FFileName := TPath.Combine(FInitialDir, ItemText);
  end;
end;

procedure TFrameFileSelect.SetFileMask(const Value: String);
begin
  FFileMask := Value;
  lExt.Text := FFileMask;
end;

end.





