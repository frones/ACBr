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
  FileSelectFrame;

type

  TFileSelectForm = class(TForm)
    tbBottom: TToolBar;
    GridPanelLayout1: TGridPanelLayout;
    btPublic: TButton;
    btTemp: TButton;
    btDoctos: TButton;
    btDown: TButton;
    fraFileSelect: TFrameFileSelect;
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btPublicClick(Sender: TObject);
    procedure btTempClick(Sender: TObject);
    procedure btDoctosClick(Sender: TObject);
    procedure btDownClick(Sender: TObject);
    procedure fraFileSelectlvFileBrowseItemClickEx(const Sender: TObject;
      ItemIndex: Integer; const LocalClickPos: TPointF;
      const ItemObject: TListItemDrawable);
  private
    function GetFileMask: String;
    function GetFileName: String;
    function GetInitialDir: String;
    function GetShowHidden: Boolean;
    procedure SetFileMask(const Value: String);
    procedure SetFileName(const Value: String);
    procedure SetInitialDir(const Value: String);
    procedure SetShowHidden(const Value: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property InitialDir: String read GetInitialDir write SetInitialDir;
    property FileName: String read GetFileName write SetFileName;
    property ShowHidden: Boolean read GetShowHidden write SetShowHidden;
    property FileMask: String read GetFileMask write SetFileMask;
  end;

var
  FileSelectForm: TFileSelectForm;

implementation

uses
  System.IOUtils;

{$R *.fmx}

constructor TFileSelectForm.Create(AOwner: TComponent);
begin
  inherited;

  if AOwner is TForm then
  begin
    Top := TForm(AOwner).Top;
    Left := TForm(AOwner).Left;
    Width := TForm(AOwner).Width;
    Height := TForm(AOwner).Height;
    StyleBook := TForm(AOwner).StyleBook;
  end;
end;

destructor TFileSelectForm.Destroy;
begin
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
  fraFileSelect.Execute;
end;

procedure TFileSelectForm.fraFileSelectlvFileBrowseItemClickEx(
  const Sender: TObject; ItemIndex: Integer; const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
begin
  fraFileSelect.SelectItem( ItemIndex );
  if (fraFileSelect.FileName <> '') then
    ModalResult := mrOk;
end;

function TFileSelectForm.GetFileMask: String;
begin
  Result := fraFileSelect.FileMask;
end;

function TFileSelectForm.GetFileName: String;
begin
  Result := fraFileSelect.FileName;
end;

function TFileSelectForm.GetInitialDir: String;
begin
  Result := fraFileSelect.ActualDir;
end;

function TFileSelectForm.GetShowHidden: Boolean;
begin
  Result := fraFileSelect.ShowHidden;
end;

procedure TFileSelectForm.SetFileMask(const Value: String);
begin
  fraFileSelect.FileMask := Value;
end;

procedure TFileSelectForm.SetFileName(const Value: String);
begin
  fraFileSelect.FileName := Value;
end;

procedure TFileSelectForm.SetInitialDir(const Value: String);
begin
  fraFileSelect.ActualDir := Value;
end;

procedure TFileSelectForm.SetShowHidden(const Value: Boolean);
begin
  fraFileSelect.ShowHidden := Value;
end;

procedure TFileSelectForm.btDoctosClick(Sender: TObject);
begin
  fraFileSelect.ChangeDirectory(TPath.GetDocumentsPath)
end;

procedure TFileSelectForm.btDownClick(Sender: TObject);
begin
  fraFileSelect.ChangeDirectory(TPath.GetDownloadsPath)
end;

procedure TFileSelectForm.btPublicClick(Sender: TObject);
begin
  fraFileSelect.ChangeDirectory(TPath.GetPublicPath)
end;

procedure TFileSelectForm.btTempClick(Sender: TObject);
begin
  fraFileSelect.ChangeDirectory(TPath.GetTempPath)
end;

end.


