// Baseado em: https://youtu.be/07fwLbnb3ns

{$IFDEF NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

unit FileSelectFr;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Variants, System.SysUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView, FMX.Layouts,
  System.Math.Vectors, FMX.Controls3D, FMX.Edit, System.ImageList, FMX.ImgList,
  System.Generics.Collections, System.Generics.Defaults;

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
  end;

  TFileSelectForm = class(TForm)
    ToolBar2: TToolBar;
    btSelectDirectory: TCornerButton;
    edDirectory: TEdit;
    lvFileBrowse: TListView;
    ImageList1: TImageList;
    SpeedButton1: TSpeedButton;
    lExt: TLabel;
    ToolBar1: TToolBar;
    GridPanelLayout1: TGridPanelLayout;
    btPublic: TButton;
    btTemp: TButton;
    btDoctos: TButton;
    btDown: TButton;
    procedure btSelectDirectoryClick(Sender: TObject);
    procedure lvFileBrowseDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure lvFileBrowseItemClickEx(const Sender: TObject; ItemIndex: Integer;
      const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btPublicClick(Sender: TObject);
    procedure btTempClick(Sender: TObject);
    procedure btDoctosClick(Sender: TObject);
    procedure btDownClick(Sender: TObject);
  private
    FInitialDir: String;
    FFileName: String;
    FShowHidden: Boolean;
    FFileMask: String;
    FFileList: TFileList;

    { Private declarations }
    function ChangeDirectory(const ADirectory: String): Boolean;
    procedure AddFilesToList(APath: String = ''; Attr: Integer = faAnyFile);
    procedure SelectItem(const AItem: Integer);
    procedure SetFileMask(const Value: String);
    procedure SetInitialDir(const Value: String);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property InitialDir: String read FInitialDir write SetInitialDir;
    property FileName: String read FFileName write FFileName;
    property ShowHidden: Boolean read FShowHidden write FShowHidden;
    property FileMask: String read FFileMask write SetFileMask;
  end;

function CompareDirFiles(const Left, Right: TFileInfo): Integer;

var
  FileSelectForm: TFileSelectForm;

implementation

uses
  System.IOUtils, System.Math, System.StrUtils, FMX.Dialogs;

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


constructor TFileSelectForm.Create(AOwner: TComponent);
begin
  inherited;

  FFileList := TFileList.Create(TComparer<TFileInfo>.Construct( CompareDirFiles ));
  FFileName := '';
  FInitialDir := '';
  FShowHidden := False;
  FFileMask := '*.*';

  if AOwner is TForm then
  begin
    Top := TForm(AOwner).Top;
    Left := TForm(AOwner).Left;
    Width := TForm(AOwner).Width;
    Height := TForm(AOwner).Height;

    StyleBook := TForm(AOwner).StyleBook;
  end;

  {$IfDef ANDROID}
    btSelectDirectory.Visible := False;
  {$EndIf}
end;

destructor TFileSelectForm.Destroy;
begin
  FreeAndNil(FFileList);

  inherited;
end;

procedure TFileSelectForm.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
    ModalResult := mrCancel;
end;

procedure TFileSelectForm.FormShow(Sender: TObject);
begin
  if not ChangeDirectory(FInitialDir) then
    ChangeDirectory(TPath.GetHomePath);
end;

procedure TFileSelectForm.btDoctosClick(Sender: TObject);
begin
  ChangeDirectory(TPath.GetDocumentsPath)
end;

procedure TFileSelectForm.btDownClick(Sender: TObject);
begin
  ChangeDirectory(TPath.GetDownloadsPath)
end;

procedure TFileSelectForm.btPublicClick(Sender: TObject);
begin
  ChangeDirectory(TPath.GetPublicPath)
end;

procedure TFileSelectForm.btSelectDirectoryClick(Sender: TObject);
var
  NewDir: String;
begin
  if SelectDirectory('Select Directory', FInitialDir, NewDir) then
    ChangeDirectory(NewDir);
end;

procedure TFileSelectForm.btTempClick(Sender: TObject);
begin
  ChangeDirectory(TPath.GetTempPath)
end;

function TFileSelectForm.ChangeDirectory(const ADirectory: String): Boolean;
var
  CapDir: String;
  FI: TFileInfo;
  LVI: TListViewItem;
begin
  Result := False;
  if (ADirectory = EmptyStr) or (not DirectoryExists(ADirectory)) then
    Exit;

  edDirectory.Text := ADirectory;
  FInitialDir := ADirectory;

  lvFileBrowse.BeginUpdate;
  try
    FFileList.Clear;
    lvFileBrowse.Items.Clear;

    CapDir := ReverseString(ADirectory);
    Caption := ReverseString(copy(CapDir, 1, pos(PathDelim, CapDir)));

    if FFileMask = EmptyStr then
      FFileMask := CAllFiles;

    if FFileMask = CAllFiles then
      AddFilesToList()
    else
    begin
      AddFilesToList('',faDirectory);
      AddFilesToList(TPath.Combine(FInitialDir, FFileMask), (faAnyFile - faDirectory));
    end;

    FFileList.Sort();

    for FI in FFileList do
    begin
      LVI := lvFileBrowse.Items.Add;
      LVI.Data['FileName'] := FI.Name;
      LVI.Data['FileSize'] := FI.FormatByteSize;
      LVI.Data['DateTime'] := FI.FormatDateTime;
      LVI.Data['ImgFileDir'] := ifthen(FI.IsDirectory, 1, 0);
    end;

  finally
    lvFileBrowse.EndUpdate;
    FFileList.Clear;
  end;

  Result := True;
end;

procedure TFileSelectForm.AddFilesToList(APath: String; Attr: Integer);
var
  LastFile: string;
  RetFind: Integer;
  SearchRec: TSearchRec;
  FI: TFileInfo;
begin
  if APath = EmptyStr then
    APath := TPath.Combine(FInitialDir, CAllFiles);

  LastFile := '';
  RetFind := FindFirst(APath, Attr, SearchRec);
  try
    while (RetFind = 0) and (LastFile <> SearchRec.Name) do
    begin
      LastFile := SearchRec.Name;
      if ((SearchRec.Attr = 0) or ((SearchRec.Attr and Attr) <> 0)) and
         (FShowHidden or ((SearchRec.Attr and faHidden) = 0)) then
      begin
        FI := TFileInfo.Create( LastFile,
                                ((SearchRec.Attr and faDirectory) <> 0),
                                SearchRec.TimeStamp, SearchRec.Size);
        FFileList.Add(FI);
      end;
      FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TFileSelectForm.lvFileBrowseDblClick(Sender: TObject);
var
  I: Integer;
begin
  if lvFileBrowse.Selected = nil then
    Exit;

  I := lvFileBrowse.Selected.Index;
  SelectItem(I);
end;

procedure TFileSelectForm.lvFileBrowseItemClickEx(const Sender: TObject;
  ItemIndex: Integer; const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
begin
  SelectItem(ItemIndex);
end;

procedure TFileSelectForm.SelectItem(const AItem: Integer);
var
  ItemText, NewDir: String;
begin
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
    begin
      FileName := TPath.Combine(FInitialDir, ItemText);
      ModalResult := mrOk;
    end;
  end;
end;

procedure TFileSelectForm.SetFileMask(const Value: String);
begin
  FFileMask := Value;
  lExt.Text := FFileMask;
end;

procedure TFileSelectForm.SetInitialDir(const Value: String);
begin
  if Visible then
    ChangeDirectory(Value);
end;

procedure TFileSelectForm.SpeedButton1Click(Sender: TObject);
begin
  if ChangeDirectory(edDirectory.Text) then
    edDirectory.Text := FInitialDir;
end;

end.
