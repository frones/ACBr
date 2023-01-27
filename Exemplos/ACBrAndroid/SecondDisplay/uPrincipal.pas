unit uPrincipal;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation,
{$IF CompilerVersion > 32}
  System.Permissions,
{$IFDEF ANDROID}
  FMX.Platform.Android,
  FMX.Helpers.Android,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
{$ENDIF} {$ENDIF}
  ACBr.Android.VideoPlayer,
  uFrameVendaProduto;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Image2: TImage;
    Button3: TButton;
    Button4: TButton;
    ACBrVideoPlayer1: TACBrVideoPlayer;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    FrameVendaProduto: TFrameVendaProduto;
{$IFDEF ANDROID}
    FPermissions: TArray<String>;
    FPropListCount: Integer;
    procedure GetPermissions;
{$IF CompilerVersion > 32.0}
{$IF CompilerVersion >= 35.0}
    procedure PermissionsResult(Sender: TObject;
      const APermissions: TClassicStringDynArray;
      const AGrantResults: TClassicPermissionStatusDynArray);
{$ELSE}
    procedure PermissionsResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
{$ENDIF}
{$ENDIF}
{$ENDIF}
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  System.IOUtils;

{$R *.fmx}
{$IFDEF ANDROID}
{$IF CompilerVersion > 32.0}
{$IF Compilerversion >= 35.0}

procedure TForm2.PermissionsResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
{$ELSE}

procedure TForm2.PermissionsResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
{$ENDIF}
{$ENDIF}
begin
end;
{$ENDIF}

procedure TForm2.GetPermissions;
Begin
{$IFDEF ANDROID}
  SetLength(FPermissions, 2);
  FPermissions[0] := 'android.permission.READ_EXTERNAL_STORAGE';
  FPermissions[1] := 'android.permission.WRITE_EXTERNAL_STORAGE';
  PermissionsService.RequestPermissions(FPermissions, PermissionsResult);
{$ENDIF}
End;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FrameVendaProduto := TFrameVendaProduto.Create(Self);
  Image1.Visible := False;
  Image2.Visible := False;
  GetPermissions;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  FrameVendaProduto.Show;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  FrameVendaProduto.Imagem := Image1.Bitmap;
  FrameVendaProduto.Descricao := 'Alexa';
  FrameVendaProduto.Quantidade := 1;
  FrameVendaProduto.ValorUnitario := 356.99;
  FrameVendaProduto.TotalVenda := 356.99;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  FrameVendaProduto.Imagem := Image2.Bitmap;
  FrameVendaProduto.Descricao := 'Mintendo Switch';
  FrameVendaProduto.Quantidade := 1;
  FrameVendaProduto.ValorUnitario := 2600.00;
  FrameVendaProduto.TotalVenda := 2956.99;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  FrameVendaProduto.Hide;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  ACBrVideoPlayer1.SetBounds(0, 0, 0, 0);
  ACBrVideoPlayer1.SetPathAndFile(  TPath.Combine(TPath.GetDocumentsPath, 'video.mp4'));
  ACBrVideoPlayer1.Play;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  ACBrVideoPlayer1.SetBounds(100, 100, 300, 0);
  ACBrVideoPlayer1.SetPathAndFile( TPath.Combine(TPath.GetDocumentsPath, 'move-tool.mp4'));
  ACBrVideoPlayer1.Play;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  ACBrVideoPlayer1.Hide;
end;

end.
