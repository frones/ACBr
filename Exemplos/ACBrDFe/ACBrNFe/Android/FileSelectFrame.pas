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
  FMX.ListView, FMX.Layouts, FMX.Gestures, FMX.ScrollBox;

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

  TFileListSort = (flsName, flsDate);

  TFrameFileSelect = class(TFrame)
    lvFileBrowse: TListView;
    tbPaths: TToolBar;
    lExt: TLabel;
    ImageList1: TImageList;
    hScrollboxPath: THorzScrollBox;
    sbPathScrollLeft: TSpeedButton;
    sbPathScrollRight: TSpeedButton;
    procedure lExtClick(Sender: TObject);
    procedure lvFileBrowseDblClick(Sender: TObject);
    procedure lvFileBrowseItemClickEx(const Sender: TObject; ItemIndex: Integer;
      const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure sbPathScrollLeftClick(Sender: TObject);
    procedure sbPathScrollRightClick(Sender: TObject);
    procedure hScrollboxPathHScrollChange(Sender: TObject);
    procedure hScrollboxPathResize(Sender: TObject);
  private
    { Private declarations }
    FExecuted: Boolean;
    FActualDir: String;
    FFileName: String;
    FShowHidden: Boolean;
    FFileMask: String;
    FCanChangeDir: Boolean;
    FSortType: TFileListSort;

    procedure SetActualDir(const Value: String);

    procedure SetFileMask(const Value: String);
    procedure SetShowHidden(const Value: Boolean);
    procedure SetCanChangeDir(const Value: Boolean);

  protected
    procedure CreateNavigatePathButtons(const APath: String);
    function CreateNavigatePathButtom(const ADir: String; Aparent: TFmxObject): TButton;
    procedure lButtomPathClick(Sender: TObject);
    procedure ShowNavigatePathButtons;
    procedure ScrollNavigatePathToEnd;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
    function ChangeDirectory(const ADirectory: String): Boolean;
    procedure SelectItem(const AItem: Integer);
    procedure Reload;

    property CanChangeDir: Boolean read FCanChangeDir write SetCanChangeDir;
    property ActualDir: String read FActualDir write SetActualDir;
    property FileName: String read FFileName write FFileName;
    property ShowHidden: Boolean read FShowHidden write SetShowHidden;
    property FileMask: String read FFileMask write SetFileMask;
    property SortType: TFileListSort read FSortType write FSortType;
  end;

function SortByName(const Left, Right: TFileInfo): Integer;
function SortByDate(const Left, Right: TFileInfo): Integer;


implementation

uses
  System.IOUtils, System.Math, System.StrUtils;

{$R *.fmx}

function SortByName(const Left, Right: TFileInfo): Integer;
var
  Str1, Str2: String;
begin
  Str1 := ifthen(Left.IsDirectory,'0','1')+Left.Name.ToUpper;
  Str2 := ifthen(Right.IsDirectory,'0','1')+Right.Name.ToUpper;

  if Str1 > Str2 then
    Result := 1
  else if Str1 < Str2 then
    Result := -1
  else
    Result := 0;
end;

function SortByDate(const Left, Right: TFileInfo): Integer;
var
  Str1, Str2: String;
begin
  Str1 := ifthen(Left.IsDirectory,'0','1')+FormatDateTime('yyyymmddhhnnss', Left.Data);
  Str2 := ifthen(Right.IsDirectory,'0','1')+FormatDateTime('yyyymmddhhnnss', Right.Data);

  if Str1 > Str2 then
    Result := 1
  else if Str1 < Str2 then
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
var
  AFormatSettings: TFormatSettings;
begin
  if Data <> 0 then
  begin
    AFormatSettings := TFormatSettings.Create;
    AFormatSettings.ShortDateFormat := 'dd/mm/yy';
    Result := DateTimeToStr(Data, AFormatSettings);
  end
  else
    Result := 'n/d';
end;

{ TFileList }

constructor TFileList.Create;
begin
  inherited Create(True)
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

constructor TFrameFileSelect.Create(AOwner: TComponent);
begin
  inherited;

  FExecuted := False;
  FFileName := '';
  FActualDir := TPath.GetHomePath;
  FShowHidden := False;
  FFileMask := CAllFiles;
  FCanChangeDir := True;
  FSortType := flsName;
end;

function TFrameFileSelect.ChangeDirectory(const ADirectory: String): Boolean;
var
  FI: TFileInfo;
  FL: TFileList;
  LVI: TListViewItem;
  OldDir: String;
  Attr: Integer;
begin
  Result := False;
  if (ADirectory = EmptyStr) or (not DirectoryExists(ADirectory)) then
    Exit;

  FL := TFileList.Create;
  try
    //CapDir := ReverseString(ADirectory);
    //D Caption := ReverseString(copy(CapDir, 1, pos(PathDelim, CapDir)));

    if FFileMask = EmptyStr then
      FFileMask := CAllFiles;

    if FFileMask = CAllFiles then
    begin
      if not FCanChangeDir then
        Attr := (faAnyFile - faDirectory)
      else
        Attr := faAnyFile;

      FL.AddFilesToList(TPath.Combine(ADirectory, CAllFiles), Attr)
    end
    else
    begin
      if FCanChangeDir then
        FL.AddFilesToList(TPath.Combine(ADirectory, CAllFiles), faDirectory);
      FL.AddFilesToList(TPath.Combine(ADirectory, FFileMask), (faAnyFile - faDirectory));
    end;

    if FCanChangeDir and (FL.Count < 1) then
      Exit;

    if FSortType = flsDate then
      FL.Sort(TComparer<TFileInfo>.Construct( SortByDate) )
    else
      FL.Sort(TComparer<TFileInfo>.Construct( SortByName ) );

    lvFileBrowse.BeginUpdate;
    try
      lvFileBrowse.Items.Clear;

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
    end;
  finally
    FL.Free;
  end;

  FActualDir := ADirectory;
  CreateNavigatePathButtons( ADirectory );
  Result := True;
end;

function TFrameFileSelect.CreateNavigatePathButtom(const ADir: String; Aparent: TFmxObject): TButton;
var
  Num: Integer;
begin
  Num := Aparent.ComponentCount;
  Result := TButton.Create(Aparent);
  //Result.Name := 'bDir'+Num.ToString;
  Result.StyleLookup := 'speedbuttonstyle';
  Result.CanFocus := False;
  Result.OnClick := lButtomPathClick;
  Result.Text := ADir + PathDelim;
  Result.Width := 60;
  Result.TextSettings.Font.Style := [TFontStyle.fsBold];
  Result.Margins.Top := 5;
  Result.Margins.Bottom := 5;
  Result.Margins.Left := 2;
  Result.Align := TAlignLayout.Left;
  Result.Parent := Aparent;
  Result.Width := Result.Canvas.TextWidth(ADir)+20;
end;

procedure TFrameFileSelect.CreateNavigatePathButtons(const APath: String);
var
  SL: TStringList;
  i: Integer;
begin
  hScrollboxPath.BeginUpdate;
  SL := TStringList.Create;
  try
    i := 0;
    while (i < hScrollboxPath.ComponentCount) do
    begin
      if (hScrollboxPath.Components[i] is TButton) then
        hScrollboxPath.Components[i].DisposeOf
      else
        Inc(i);
    end;

    SL.Delimiter := PathDelim;
    SL.StrictDelimiter := True;
    SL.DelimitedText := ExcludeTrailingPathDelimiter(APath);
    for i := 0 to SL.Count-1 do
      CreateNavigatePathButtom(SL[i], hScrollboxPath);

    hScrollboxPath.RealignContent;
  finally
    SL.Free;
    hScrollboxPath.EndUpdate;
    ScrollNavigatePathToEnd;
  end;

end;

procedure TFrameFileSelect.ShowNavigatePathButtons;
var
  x, w: Integer;
begin
  sbPathScrollLeft.Visible  := (hScrollboxPath.ViewportPosition.X > 0);
  x := Round(hScrollboxPath.ViewportPosition.X + hScrollboxPath.Width);
  w := Round(hScrollboxPath.ContentBounds.Width);
  sbPathScrollRight.Visible := (x < w);
end;

procedure TFrameFileSelect.Execute;
begin
  if not ChangeDirectory(FActualDir) then
    ChangeDirectory(TPath.GetHomePath);

  FExecuted := True;
end;

procedure TFrameFileSelect.hScrollboxPathHScrollChange(Sender: TObject);
begin
  ShowNavigatePathButtons;
end;

procedure TFrameFileSelect.hScrollboxPathResize(Sender: TObject);
begin
  ShowNavigatePathButtons;
end;

procedure TFrameFileSelect.lButtomPathClick(Sender: TObject);
var
  APath: string;
  p: Integer;
begin
  if not (Sender is TButton) then
    Exit;

  APath := TButton(Sender).Text;
  p := Pos(Trim(APath), FActualDir);
  if (p > 0) then
    ChangeDirectory( copy(FActualDir, 1, p+APath.Length-1) );
end;

procedure TFrameFileSelect.lExtClick(Sender: TObject);
var
  NewDir: String;
begin
  if SelectDirectory('Select Directory', FActualDir, NewDir) then
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

procedure TFrameFileSelect.Reload;
begin
  ChangeDirectory(FActualDir);
end;

procedure TFrameFileSelect.sbPathScrollLeftClick(Sender: TObject);
begin
  hScrollboxPath.ScrollBy(50,0);
end;

procedure TFrameFileSelect.sbPathScrollRightClick(Sender: TObject);
begin
  hScrollboxPath.ScrollBy(-50,0);
end;

procedure TFrameFileSelect.ScrollNavigatePathToEnd;
begin
  hScrollboxPath.ViewportPosition := TPointF.Create(hScrollboxPath.ContentBounds.Width,0);
  sbPathScrollLeft.Visible := (hScrollboxPath.ViewportPosition.X > 0);
  if sbPathScrollLeft.Visible then
    hScrollboxPath.ViewportPosition := TPointF.Create(hScrollboxPath.ContentBounds.Width,0);
  sbPathScrollRight.Visible := False;
end;

procedure TFrameFileSelect.SelectItem(const AItem: Integer);
var
  ItemText, NewDir: String;
begin
  FFileName := '';
  if (AItem < 0) or (AItem >= lvFileBrowse.ItemCount) then
    Exit;

  lvFileBrowse.ItemIndex := AItem;
  if lvFileBrowse.Selected = nil then
    Exit;

  with lvFileBrowse.Items[AItem] do
  begin
    ItemText := Data['FileName'].ToString;

    if (Data['ImgFileDir'].AsInteger = 1) then    // É diretório ?
    begin
      if (ItemText = '.') then
        NewDir := FActualDir
      else if (ItemText = '..') then
      begin
        NewDir := ReverseString(FActualDir);
        NewDir := ReverseString(copy(NewDir, pos(PathDelim, NewDir)+1));
      end
      else
        NewDir := TPath.Combine(FActualDir, ItemText);

      ChangeDirectory(NewDir);
    end
    else
      FFileName := TPath.Combine(FActualDir, ItemText);
  end;
end;

procedure TFrameFileSelect.SetActualDir(const Value: String);
begin
  FActualDir := Value;
  if FExecuted then
    ChangeDirectory(Value);
end;

procedure TFrameFileSelect.SetCanChangeDir(const Value: Boolean);
begin
  if Value = FCanChangeDir then
    Exit;

  FCanChangeDir := Value;
  hScrollboxPath.Enabled := FCanChangeDir;
  lExt.Enabled := FCanChangeDir;
  if FExecuted then
    ChangeDirectory(FActualDir);
end;

procedure TFrameFileSelect.SetFileMask(const Value: String);
begin
  if Value = FFileMask then
    Exit;

  FFileMask := Value;
  lExt.Text := FFileMask;
  lExt.Visible := not Value.IsEmpty;
  if FExecuted then
    ChangeDirectory(FActualDir);
end;

procedure TFrameFileSelect.SetShowHidden(const Value: Boolean);
begin
  if Value = FShowHidden then
    Exit;

  FShowHidden := Value;
  if FExecuted then
    ChangeDirectory(FActualDir);
end;

end.





