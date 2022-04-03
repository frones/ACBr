unit FormPrincipal;

interface

uses
  System.SysUtils,
  System.Types,
  System.Messaging,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.ImageList,
  System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo,
  FMX.Memo.Types,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.ListBox,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Edit,
  FMX.Layouts,
  FMX.Ani,
  FMX.Effects,
  FMX.Objects,
  FMX.TabControl,
  FMX.ImgList,
  FMX.Controls.Presentation,
  ACBrBase,
  ACBrETQ, FMX.Media;

type
  TFrPrincipal = class(TForm)
    ImageList1: TImageList;
    tabsPrincipal: TTabControl;
    tabMenu: TTabItem;
    ToolBar4: TToolBar;
    Label26: TLabel;
    Image1: TImage;
    ShadowEffect2: TShadowEffect;
    GridPanelLayout1: TGridPanelLayout;
    laBtnEuAmo: TLayout;
    iEuAmo: TImage;
    lTestes: TLabel;
    laBtnSinalizacoes: TLayout;
    iSinalizacoes: TImage;
    ShadowEffect4: TShadowEffect;
    Label32: TLabel;
    FloatAnimation4: TFloatAnimation;
    laBtnWC: TLayout;
    iWC: TImage;
    ShadowEffect5: TShadowEffect;
    Label36: TLabel;
    FloatAnimation3: TFloatAnimation;
    laBtnFrases: TLayout;
    iFrases: TImage;
    Label37: TLabel;
    FloatAnimation2: TFloatAnimation;
    tabConfig: TTabItem;
    tobConfig: TToolBar;
    lblTituloConfig: TLabel;
    SpeedButton2: TSpeedButton;
    tabsConfig: TTabControl;
    gpBotoesConfig: TGridPanelLayout;
    btLerConfig: TCornerButton;
    btSalvarConfig: TCornerButton;
    ShadowEffect1: TShadowEffect;
    tbConfigImpressora: TTabItem;
    lbConfigImpressora: TListBox;
    lbhPorta: TListBoxGroupHeader;
    lbiPorta: TListBoxItem;
    lbgDiversos: TListBoxGroupHeader;
    lbiDiversos: TListBoxItem;
    gplDiversos: TGridPanelLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbxDPI: TComboBox;
    cbxDeteccao: TComboBox;
    cbxPagCodigo: TComboBox;
    lbiMemoriaGuilhotina: TListBoxItem;
    gplMemoriaGuilhotina: TGridPanelLayout;
    ckLimparMemoria: TCheckBox;
    ckGuilhotina: TCheckBox;
    edtPortaImpresora: TEdit;
    ACBrETQ1: TACBrETQ;
    StyleBook1: TStyleBook;
    laBtnSlap: TLayout;
    iSlap: TImage;
    ShadowEffect6: TShadowEffect;
    Label4: TLabel;
    FloatAnimation5: TFloatAnimation;
    laBtnLembranca: TLayout;
    iLembranca: TImage;
    ShadowEffect7: TShadowEffect;
    Label5: TLabel;
    FloatAnimation6: TFloatAnimation;
    ShadowEffect3: TShadowEffect;
    imgConfig: TImage;
    ShadowEffect8: TShadowEffect;
    CameraComponent1: TCameraComponent;
    tabLembranca: TTabItem;
    tobLembranca: TToolBar;
    Label6: TLabel;
    SpeedButton1: TSpeedButton;
    imgLembranca: TImage;
    GridPanelLayout2: TGridPanelLayout;
    edLembranca: TEdit;
    bFotoLembranca: TCornerButton;
    bImprimirLembranca: TCornerButton;
    OpenDialog1: TOpenDialog;
    tabConfDiversos: TTabItem;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    edPastaImagens: TEdit;
    tabEuAmo: TTabItem;
    ToolBar1: TToolBar;
    Label7: TLabel;
    SpeedButton3: TSpeedButton;
    iToolEuAmo: TImage;
    ShadowEffect9: TShadowEffect;
    iToolConfig: TImage;
    ShadowEffect10: TShadowEffect;
    iToolLembranca: TImage;
    ShadowEffect11: TShadowEffect;
    VertScrollBox1: TVertScrollBox;
    FlowLayout1: TFlowLayout;
    img_colorworks: TImage;
    img_epson: TImage;
    img_brasil: TImage;
    img_codaberto2: TImage;
    img_codaberto1: TImage;
    tabImprimir: TTabItem;
    imgImprimir: TImage;
    ToolBar2: TToolBar;
    Label8: TLabel;
    iToolImprimir: TImage;
    ShadowEffect12: TShadowEffect;
    SpeedButton4: TSpeedButton;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem2: TListBoxItem;
    sbLarguraEtiqueta: TSpinBox;
    Layout1: TLayout;
    edTextoImprimir: TEdit;
    btImprimir: TCornerButton;
    sbCopias: TSpinBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure btSalvarConfigClick(Sender: TObject);
    procedure btLerConfigClick(Sender: TObject);
    procedure imgConfigClick(Sender: TObject);
    procedure bFotoLembrancaClick(Sender: TObject);
    procedure laBtnLembrancaClick(Sender: TObject);
    procedure laBtnEuAmoClick(Sender: TObject);
    procedure img_colorworksClick(Sender: TObject);
    procedure btImprimirClick(Sender: TObject);
    procedure img_epsonClick(Sender: TObject);
    procedure img_brasilClick(Sender: TObject);
    procedure img_codaberto2Click(Sender: TObject);
    procedure img_codaberto1Click(Sender: TObject);
  private
    FTabList: TList<TTabItem>;
    FImgFile: String;
    FTextPos: TPoint;
    FFonte: Char;
    FMultiplicador: Integer;
    FTextColor: Cardinal;
    FTextOpacidade: Byte;
    FFundoColor: Cardinal;
    FFundoOpacidade: Byte;

    procedure DoDidFinish(Image: TBitmap);
    procedure DoMessageListener(const Sender: TObject; const M: TMessage);

    procedure TabForward(ANewTab: TTabItem);
    procedure TabBack;

    procedure LimparAbaLembranca;

    function CalcularPathImagem(Pasta, Imagem: String): String;
    function CalcularNomeArqINI: String;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

    procedure IrParaImpressao(AImg: TBitmap; ImgFile: String;
      MaxChars: Integer; TextPos: TPoint;
      Fonte: Char = 'V'; Multiplicador: Integer = 10;
      TextColor: Cardinal = $FFFFFF; TextoOpacidade: Byte = 255;
      FundoColor: Cardinal = $000000; FundoOpacidade: Byte = 0);
    procedure ConfigurarACBrETQ;
  public

  end;

var
  FrPrincipal: TFrPrincipal;

implementation

uses
  System.IniFiles,
  System.typinfo,
  FMX.DialogService.Async,
  FMX.MediaLibrary,
  FMX.Platform,
  {$IfDef ANDROID}
   Androidapi.JNI.Widget,
   Androidapi.Helpers,
  {$EndIf}
  {$IfDef MSWINDOWS}
   Winapi.ShellAPI,
  {$EndIf}
  ACBrImage,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrETQClass;

{$R *.fmx}

procedure Toast(const AMsg: string; ShortDuration: Boolean = True);
var
  ToastLength: Integer;
begin
  {$IfNDef ANDROID}
   TDialogServiceAsync.ShowMessage(AMsg);
  {$Else}
   if ShortDuration then
     ToastLength := TJToast.JavaClass.LENGTH_SHORT
   else
     ToastLength := TJToast.JavaClass.LENGTH_LONG;

   TJToast.JavaClass.makeText(SharedActivityContext, StrToJCharSequence(AMsg), ToastLength).show;
   Application.ProcessMessages;
  {$EndIf}
end;

{ TFrPrincipal }

procedure TFrPrincipal.FormCreate(Sender: TObject);
var
  d: TACBrETQDPI;
  e: TACBrETQDeteccaoEtiqueta;
  p: TACBrETQPaginaCodigo;
begin
  FTabList := TList<TTabItem>.Create;

  cbxDPI.Items.Clear ;
  For d := Low(TACBrETQDPI) to High(TACBrETQDPI) do
    cbxDPI.Items.Add( GetEnumName(TypeInfo(TACBrETQDPI), integer(d) ) ) ;

  cbxDeteccao.Items.Clear ;
  For e := Low(TACBrETQDeteccaoEtiqueta) to High(TACBrETQDeteccaoEtiqueta) do
    cbxDeteccao.Items.Add( GetEnumName(TypeInfo(TACBrETQDeteccaoEtiqueta), integer(e) ) ) ;

  cbxPagCodigo.Items.Clear ;
  For p := Low(TACBrETQPaginaCodigo) to High(TACBrETQPaginaCodigo) do
    cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrETQPaginaCodigo), integer(p) ) ) ;

  tabsConfig.First;
  tabsPrincipal.First;
  tabsPrincipal.TabPosition := TTabPosition.None;

  iToolEuAmo.Bitmap.Assign(iEuAmo.Bitmap);
  iToolConfig.Bitmap.Assign(imgConfig.Bitmap);
  iToolLembranca.Bitmap.Assign(iLembranca.Bitmap);
  iToolImprimir.Bitmap := ImageList1.Bitmap(TSizeF.Create(iToolImprimir.Width,iToolImprimir.Height), 10);

  LerConfiguracao;
end;

procedure TFrPrincipal.FormDestroy(Sender: TObject);
begin
  FTabList.Free;
end;

procedure TFrPrincipal.btLerConfigClick(Sender: TObject);
begin
  LerConfiguracao;
  Toast('Configuração carregada');
end;

procedure TFrPrincipal.btSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  Toast('Configuração Salva com sucesso');
  TabBack;
end;

function TFrPrincipal.CalcularNomeArqINI: String;
begin
  Result := ApplicationPath + 'ACBrEpsonColorWorks.ini';
end;

function TFrPrincipal.CalcularPathImagem(Pasta, Imagem: String): String;
begin
  Result := ApplicationPath + 'img' +PathDelim + Pasta + PathDelim + Imagem+'.png';
end;

procedure TFrPrincipal.bFotoLembrancaClick(Sender: TObject);
var
  Service: IFMXCameraService;
  Params: TParamsPhotoQuery;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCameraService, Service) then
  begin
    Params.Editable := True;
    // Specifies whether to save a picture to device Photo Library
    Params.NeedSaveToAlbum := True;
    Params.RequiredResolution := TSize.Create(1080, 1920);
    Params.OnDidFinishTaking := DoDidFinish;
    Service.TakePhoto(bFotoLembranca, Params);
  end
  else
  begin
   {$IfDef MSWINDOWS}
    ShellExecute(0, 'OPEN', PChar('microsoft.windows.camera:'), '', '', 1);
    OpenDialog1.InitialDir := edPastaImagens.Text;
    OpenDialog1.DefaultExt := 'JPEG (*.jpg)|*.jpg|PNG (*.png)|*.png|All files (*.*)|*.*';
    if OpenDialog1.Execute then
    begin
      imgLembranca.Bitmap.LoadFromFile(OpenDialog1.FileName);
    end;
   {$Else}
    ShowMessage('This device does not support the camera service');
   {$EndIf}
  end;
end;

procedure TFrPrincipal.DoDidFinish(Image: TBitmap);
begin
  imgLembranca.Bitmap.Assign(Image);
end;

procedure TFrPrincipal.DoMessageListener(const Sender: TObject; const M: TMessage);
begin
  if M is TMessageDidFinishTakingImageFromLibrary then
    imgLembranca.Bitmap.Assign(TMessageDidFinishTakingImageFromLibrary(M).Value);
end;

procedure TFrPrincipal.laBtnEuAmoClick(Sender: TObject);
begin
  TabForward(tabEuAmo);
end;

procedure TFrPrincipal.laBtnLembrancaClick(Sender: TObject);
begin
  LimparAbaLembranca;
  TabForward(tabLembranca);
end;

procedure TFrPrincipal.LimparAbaLembranca;
begin
  imgLembranca.Bitmap := nil;
  edLembranca.Text := 'Estive na Autocom 2022';
end;

procedure TFrPrincipal.LerConfiguracao;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := CalcularNomeArqINI;
  INI := TIniFile.Create(ArqINI);
  try
    edtPortaImpresora.Text := INI.ReadString('ETQ','Porta','');
    cbxDPI.ItemIndex := INI.ReadInteger('ETQ','DPI', Integer(ACBrETQ1.DPI));
    cbxDeteccao.ItemIndex := INI.ReadInteger('ETQ','DeteccaoEtiqueta', Integer(ACBrETQ1.DeteccaoEtiqueta));
    cbxPagCodigo.ItemIndex := INI.ReadInteger('ETQ','PaginaDeCodigo', Integer(ACBrETQ1.PaginaDeCodigo));
    ckLimparMemoria.IsChecked := INI.ReadBool('ETQ','LimparMemoria', ACBrETQ1.LimparMemoria);
    ckGuilhotina.IsChecked := INI.ReadBool('ETQ','Guilhotina', ACBrETQ1.Guilhotina);
    edPastaImagens.Text := INI.ReadString('Diversos','PastaImagens', '');
    sbLarguraEtiqueta.Value := INI.ReadInteger('Diversos','LarguraEtiqueta', 106);
  finally
    INI.Free ;
  end;
end;

procedure TFrPrincipal.GravarConfiguracao;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := CalcularNomeArqINI;
  INI := TIniFile.Create(ArqINI);
  try
    INI.WriteString('ETQ','Porta',edtPortaImpresora.Text);
    INI.WriteInteger('ETQ','DPI', cbxDPI.ItemIndex);
    INI.WriteInteger('ETQ','DeteccaoEtiqueta', cbxDeteccao.ItemIndex);
    INI.WriteInteger('ETQ','PaginaDeCodigo', cbxPagCodigo.ItemIndex);
    INI.WriteBool('ETQ','LimparMemoria', ckLimparMemoria.IsChecked);
    INI.WriteBool('ETQ','Guilhotina', ckGuilhotina.IsChecked);
    INI.WriteString('Diversos','PastaImagens', edPastaImagens.Text);
    INI.WriteInteger('Diversos','LarguraEtiqueta', Trunc(sbLarguraEtiqueta.Value));
  finally
    INI.Free ;
  end;
end;

procedure TFrPrincipal.SpeedButton2Click(Sender: TObject);
begin
  TabBack;
end;

procedure TFrPrincipal.TabBack;
var
  OldTab: TTabItem;
begin
  if (FTabList.Count > 0) then
  begin
    OldTab := FTabList.Last;
    FTabList.Delete(FTabList.Count-1);
  end
  else
    OldTab := tabMenu;

  tabsPrincipal.SetActiveTabWithTransition( OldTab,
                                            TTabTransition.Slide,
                                            TTabTransitionDirection.Reversed);
end;

procedure TFrPrincipal.TabForward(ANewTab: TTabItem);
begin
  FTabList.Add(tabsPrincipal.ActiveTab);
  tabsPrincipal.SetActiveTabWithTransition( ANewTab,
                                            TTabTransition.Slide) ;
end;

procedure TFrPrincipal.imgConfigClick(Sender: TObject);
begin
  TabForward(tabConfig);
end;

procedure TFrPrincipal.img_brasilClick(Sender: TObject);
begin
  IrParaImpressao( img_brasil.Bitmap,
                   CalcularPathImagem('euamo', 'brasil'),
                   36,
                   TPoint.Create(0, 120),
                   '0', 80 );
end;

procedure TFrPrincipal.img_codaberto1Click(Sender: TObject);
begin
  IrParaImpressao( img_codaberto1.Bitmap,
                   CalcularPathImagem('euamo', 'codaberto1'),
                   36,
                   TPoint.Create(0, 120),
                   '0', 80 );
end;

procedure TFrPrincipal.img_codaberto2Click(Sender: TObject);
begin
  IrParaImpressao( img_codaberto2.Bitmap,
                   CalcularPathImagem('euamo', 'codaberto2'),
                   36,
                   TPoint.Create(0, 10),
                   '0', 80 );
end;

procedure TFrPrincipal.img_colorworksClick(Sender: TObject);
begin
  IrParaImpressao( img_colorworks.Bitmap,
                   CalcularPathImagem('euamo', 'colorworks'),  //'C:\temp\teste2.png',
                   36,
                   TPoint.Create(0, 120),
                   '0', 80 );
end;

procedure TFrPrincipal.img_epsonClick(Sender: TObject);
begin
  IrParaImpressao( img_epson.Bitmap,
                   CalcularPathImagem('euamo', 'epson'),
                   36,
                   TPoint.Create(0, 10),
                   '0', 80 );
end;

procedure TFrPrincipal.IrParaImpressao(AImg: TBitmap; ImgFile: String;
  MaxChars: Integer; TextPos: TPoint; Fonte: Char; Multiplicador: Integer;
  TextColor: Cardinal; TextoOpacidade: Byte;
  FundoColor: Cardinal; FundoOpacidade: Byte);
begin
  FImgFile := ImgFile;
  FTextPos := TextPos;
  FFonte := Fonte;
  FMultiplicador := Multiplicador;
  FTextColor := TextColor;
  FTextOpacidade := TextoOpacidade;
  FFundoColor := FFundoColor;
  FFundoOpacidade := FundoOpacidade;

  imgImprimir.Bitmap.Assign(AImg);
  edTextoImprimir.Visible := (MaxChars > 0);
  edTextoImprimir.MaxLength := MaxChars;
  TabForward(tabImprimir);
end;

procedure TFrPrincipal.btImprimirClick(Sender: TObject);
var
  NomeImagem: string;
  MS: TMemoryStream;
  w, h: Cardinal;
  b, c, o, f, i: Byte;
  msg: string;
begin
  if FImgFile.Trim.IsEmpty then
    raise Exception.Create('Imagem não especificada');

  if not FileExists(FImgFile) then
    raise Exception.Create('Imagem "'+FImgFile+'" não encontrada');

  NomeImagem := 'ACBR.PNG';
  ConfigurarACBrETQ;

  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(FImgFile);
    PNGInfoIHDR(MS, w, h, b, c, o, f, i);
    ACBrETQ1.CarregarImagem(MS, NomeImagem);
  finally
    MS.Free;
  end;

  ACBrETQ1.DefinirDimensoes( Trunc(sbLarguraEtiqueta.Value),
                             trunc(ConverterUnidade(etqDots, h, etqMilimetros, ACBrETQ1.DPI)),
                             -1, -1);
  ACBrETQ1.ImprimirImagem(1, 0, 0, NomeImagem);

  if edTextoImprimir.Visible then
  begin
    msg := edTextoImprimir.Text;
    if (FTextPos.X = 0) then
      msg := PadCenter(msg, edTextoImprimir.MaxLength);

    ACBrETQ1.DefinirCor(FTextColor, FTextOpacidade, FFundoColor, FFundoOpacidade);
    ACBrETQ1.ImprimirTexto( orNormal, FFonte,
                            FMultiplicador, FMultiplicador,
                            FTextPos.Y, FTextPos.X,
                            msg );
  end;

  ACBrETQ1.Imprimir(Trunc(sbCopias.Value));

  ACBrETQ1.ApagarImagem(NomeImagem);
  ACBrETQ1.Desativar;

  TabBack;
end;

procedure TFrPrincipal.ConfigurarACBrETQ;
begin
  with ACBrETQ1 do
  begin
     Desativar;
     DPI := TACBrETQDPI(cbxDPI.ItemIndex);
     Modelo := etqEscLabel;
     Porta := edtPortaImpresora.Text;
     LimparMemoria := ckLimparMemoria.IsChecked;
     Guilhotina := ckGuilhotina.IsChecked;
     Unidade := etqMilimetros; //etqDecimoDeMilimetros;
     MargemEsquerda := 0;
     DeteccaoEtiqueta := TACBrETQDeteccaoEtiqueta(cbxDeteccao.ItemIndex);
     PaginaDeCodigo := TACBrETQPaginaCodigo(cbxPagCodigo.ItemIndex);
     ArqLOG := ApplicationPath+'ACBrETQ.log';

     Ativar;
     edtPortaImpresora.Text := Porta;
  end;
end;

end.
