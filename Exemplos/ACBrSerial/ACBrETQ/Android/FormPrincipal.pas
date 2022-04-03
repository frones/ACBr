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
    tabFrases: TTabItem;
    toolFrases: TToolBar;
    Label9: TLabel;
    iToolFrases: TImage;
    ShadowEffect13: TShadowEffect;
    SpeedButton5: TSpeedButton;
    VertScrollBox2: TVertScrollBox;
    FlowLayout2: TFlowLayout;
    img_erro: TImage;
    img_iniciar: TImage;
    img_falha: TImage;
    img_problema: TImage;
    img_sonhar: TImage;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem3: TListBoxItem;
    edtArqLog: TEdit;
    tabWC: TTabItem;
    ToolBar3: TToolBar;
    Label10: TLabel;
    iToolWC: TImage;
    ShadowEffect14: TShadowEffect;
    SpeedButton6: TSpeedButton;
    VertScrollBox3: TVertScrollBox;
    FlowLayout3: TFlowLayout;
    img_dart: TImage;
    img_leia: TImage;
    img_super: TImage;
    img_wonder: TImage;
    img_masc: TImage;
    img_femin: TImage;
    img_acessivel: TImage;
    tabSinalize: TTabItem;
    ToolBar5: TToolBar;
    Label11: TLabel;
    iToolSinalize: TImage;
    ShadowEffect15: TShadowEffect;
    SpeedButton7: TSpeedButton;
    VertScrollBox4: TVertScrollBox;
    FlowLayout4: TFlowLayout;
    img_wifi: TImage;
    img_fumar: TImage;
    img_devm: TImage;
    img_devf: TImage;
    img_cao: TImage;
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
    procedure img_erroClick(Sender: TObject);
    procedure img_iniciarClick(Sender: TObject);
    procedure img_falhaClick(Sender: TObject);
    procedure img_problemaClick(Sender: TObject);
    procedure img_sonharClick(Sender: TObject);
    procedure laBtnFrasesClick(Sender: TObject);
    procedure laBtnWCClick(Sender: TObject);
    procedure img_dartClick(Sender: TObject);
    procedure laBtnSinalizacoesClick(Sender: TObject);
    procedure img_wifiClick(Sender: TObject);
    procedure img_fumarClick(Sender: TObject);
    procedure img_devmClick(Sender: TObject);
    procedure img_devfClick(Sender: TObject);
    procedure img_caoClick(Sender: TObject);
  private
    FTabList: TList<TTabItem>;
    FImgFile: String;
    FProcImpressao: TProc;
    FTextoImpressao: String;

    procedure DoDidFinish(Image: TBitmap);
    procedure DoMessageListener(const Sender: TObject; const M: TMessage);

    procedure TabForward(ANewTab: TTabItem);
    procedure TabBack;

    procedure CarregarImagensNaInterface;
    procedure LimparAbaLembranca;

    function CalcularPathImagem(Pasta, Imagem: String): String;
    function CalcularNomeArqINI: String;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

    procedure ImpressaoTextoAdicionalBrancoTopo;
    procedure ImpressaoTextoAdicionalBrancoFundoMaisColorWorks;
    procedure ImpressaoTextoAdicionalBrancoFundo;
    procedure ImpressaoTextoAdicionalPretoFundo;
    procedure ImpressaoTextoWiFi;
    procedure ImpressaoTextoWC;
    procedure ImpressaoColorWorks;

    procedure IrParaImpressao(AImg: TBitmap; ImgFile: String;
      MaxChars: Integer; ProcImpressao: TProc);
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
  FTextoImpressao := '';

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
  iToolFrases.Bitmap.Assign(iFrases.Bitmap);
  iToolSinalize.Bitmap.Assign(iSinalizacoes.Bitmap);
  iToolWC.Bitmap.Assign(iWC.Bitmap);
  iToolConfig.Bitmap.Assign(imgConfig.Bitmap);
  iToolLembranca.Bitmap.Assign(iLembranca.Bitmap);
  iToolImprimir.Bitmap := ImageList1.Bitmap(TSizeF.Create(iToolImprimir.Width,iToolImprimir.Height), 10);

  CarregarImagensNaInterface;
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

procedure TFrPrincipal.CarregarImagensNaInterface;
begin
  img_colorworks.Bitmap.LoadFromFile(CalcularPathImagem('euamo','colorworks'));
  img_epson.Bitmap.LoadFromFile(CalcularPathImagem('euamo','epson'));
  img_brasil.Bitmap.LoadFromFile(CalcularPathImagem('euamo','brasil'));
  img_codaberto1.Bitmap.LoadFromFile(CalcularPathImagem('euamo','codaberto1'));
  img_codaberto2.Bitmap.LoadFromFile(CalcularPathImagem('euamo','codaberto2'));

  img_erro.Bitmap.LoadFromFile(CalcularPathImagem('frases','erro'));
  img_iniciar.Bitmap.LoadFromFile(CalcularPathImagem('frases','iniciar'));
  img_falha.Bitmap.LoadFromFile(CalcularPathImagem('frases','falha'));
  img_problema.Bitmap.LoadFromFile(CalcularPathImagem('frases','problema'));
  img_sonhar.Bitmap.LoadFromFile(CalcularPathImagem('frases','sonhar'));

  img_dart.Bitmap.LoadFromFile(CalcularPathImagem('wc','dart'));
  img_leia.Bitmap.LoadFromFile(CalcularPathImagem('wc','leia'));
  img_super.Bitmap.LoadFromFile(CalcularPathImagem('wc','super'));
  img_wonder.Bitmap.LoadFromFile(CalcularPathImagem('wc','wonder'));
  img_masc.Bitmap.LoadFromFile(CalcularPathImagem('wc','masc'));
  img_femin.Bitmap.LoadFromFile(CalcularPathImagem('wc','femin'));
  img_acessivel.Bitmap.LoadFromFile(CalcularPathImagem('wc','acessivel'));

  img_wifi.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','wifi'));
  img_fumar.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','fumar'));
  img_devm.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','devm'));
  img_devf.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','devf'));
  img_cao.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','cao'));
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
  FTextoImpressao := 'AUTOCOM 2022';
  TabForward(tabEuAmo);
end;

procedure TFrPrincipal.laBtnFrasesClick(Sender: TObject);
begin
  FTextoImpressao := 'Use a Força leia os Fontes';
  TabForward(tabFrases);
end;

procedure TFrPrincipal.laBtnLembrancaClick(Sender: TObject);
begin
  LimparAbaLembranca;
  TabForward(tabLembranca);
end;

procedure TFrPrincipal.laBtnSinalizacoesClick(Sender: TObject);
begin
  FTextoImpressao := '';
  TabForward(tabSinalize);
end;

procedure TFrPrincipal.laBtnWCClick(Sender: TObject);
begin
  TabForward(tabWC);
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
    cbxDPI.ItemIndex := INI.ReadInteger('ETQ','DPI', 1);
    cbxDeteccao.ItemIndex := INI.ReadInteger('ETQ','DeteccaoEtiqueta', 0);
    cbxPagCodigo.ItemIndex := INI.ReadInteger('ETQ','PaginaDeCodigo', 2);
    ckLimparMemoria.IsChecked := INI.ReadBool('ETQ','LimparMemoria', True);
    ckGuilhotina.IsChecked := INI.ReadBool('ETQ','Guilhotina', True);
    edPastaImagens.Text := INI.ReadString('Diversos','PastaImagens', '');
    sbLarguraEtiqueta.Value := INI.ReadInteger('Diversos','LarguraEtiqueta', 106);
    edtArqLog.Text := INI.ReadString('ETQ','ArqLog','');
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
    INI.WriteString('ETQ','ArqLog',edtArqLog.Text);
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
                   26, ImpressaoTextoAdicionalBrancoFundoMaisColorWorks );
end;

procedure TFrPrincipal.img_caoClick(Sender: TObject);
begin
  IrParaImpressao( img_cao.Bitmap,
                   CalcularPathImagem('sinalize', 'cao'),
                   0, nil);
end;

procedure TFrPrincipal.img_codaberto1Click(Sender: TObject);
begin
  IrParaImpressao( img_codaberto1.Bitmap,
                   CalcularPathImagem('euamo', 'codaberto1'),
                   26, ImpressaoTextoAdicionalBrancoTopo);
end;

procedure TFrPrincipal.img_codaberto2Click(Sender: TObject);
begin
  IrParaImpressao( img_codaberto2.Bitmap,
                   CalcularPathImagem('euamo', 'codaberto2'),
                   26, ImpressaoTextoAdicionalBrancoTopo);
end;

procedure TFrPrincipal.img_colorworksClick(Sender: TObject);
begin
  IrParaImpressao( img_colorworks.Bitmap,
                   CalcularPathImagem('euamo', 'colorworks'),  //'C:\temp\teste2.png',
                   0, ImpressaoColorWorks );
end;

procedure TFrPrincipal.img_dartClick(Sender: TObject);
begin
  IrParaImpressao( img_dart.Bitmap,
                   CalcularPathImagem('frases', 'falha'),
                   26, ImpressaoTextoAdicionalBrancoFundoMaisColorWorks);
end;

procedure TFrPrincipal.img_devfClick(Sender: TObject);
begin
  IrParaImpressao( img_devf.Bitmap,
                   CalcularPathImagem('sinalize', 'devf'),
                   22, ImpressaoTextoAdicionalPretoFundo);
end;

procedure TFrPrincipal.img_devmClick(Sender: TObject);
begin
  IrParaImpressao( img_devm.Bitmap,
                   CalcularPathImagem('sinalize', 'devm'),
                   22, ImpressaoTextoAdicionalPretoFundo);
end;

procedure TFrPrincipal.img_epsonClick(Sender: TObject);
begin
  IrParaImpressao( img_epson.Bitmap,
                   CalcularPathImagem('euamo', 'epson'),
                   26, ImpressaoTextoAdicionalBrancoFundoMaisColorWorks );
end;

procedure TFrPrincipal.img_erroClick(Sender: TObject);
begin
  IrParaImpressao( img_erro.Bitmap,
                   CalcularPathImagem('frases', 'erro'),
                   26, ImpressaoTextoAdicionalBrancoFundoMaisColorWorks);
end;

procedure TFrPrincipal.img_falhaClick(Sender: TObject);
begin
  IrParaImpressao( img_falha.Bitmap,
                   CalcularPathImagem('frases', 'falha'),
                   26, ImpressaoTextoAdicionalBrancoFundoMaisColorWorks);
end;

procedure TFrPrincipal.img_fumarClick(Sender: TObject);
begin
  IrParaImpressao( img_fumar.Bitmap,
                   CalcularPathImagem('sinalize', 'fumar'),
                   0, nil);
end;

procedure TFrPrincipal.img_iniciarClick(Sender: TObject);
begin
  IrParaImpressao( img_iniciar.Bitmap,
                   CalcularPathImagem('frases', 'iniciar'),
                   26, ImpressaoTextoAdicionalBrancoTopo);
end;

procedure TFrPrincipal.img_problemaClick(Sender: TObject);
begin
  IrParaImpressao( img_problema.Bitmap,
                   CalcularPathImagem('frases', 'problema'),
                   26, ImpressaoTextoAdicionalBrancoFundo);
end;

procedure TFrPrincipal.img_sonharClick(Sender: TObject);
begin
  IrParaImpressao( img_sonhar.Bitmap,
                   CalcularPathImagem('frases', 'sonhar'),
                   26,ImpressaoTextoAdicionalBrancoFundoMaisColorWorks);
end;

procedure TFrPrincipal.img_wifiClick(Sender: TObject);
begin
  IrParaImpressao( img_wifi.Bitmap,
                   CalcularPathImagem('sinalize', 'wifi'),
                   30, ImpressaoTextoWiFi);
end;

procedure TFrPrincipal.IrParaImpressao(AImg: TBitmap; ImgFile: String;
  MaxChars: Integer; ProcImpressao: TProc);
begin
  FImgFile := ImgFile;
  FProcImpressao := ProcImpressao;

  imgImprimir.Bitmap.Assign(AImg);
  edTextoImprimir.Visible := (MaxChars > 0);
  edTextoImprimir.MaxLength := MaxChars;
  edTextoImprimir.Text := FTextoImpressao;

  TabForward(tabImprimir);
end;

procedure TFrPrincipal.ImpressaoColorWorks;
begin
  ACBrETQ1.DefinirCor($ffffff, 255, $ffffff, 255);  // Branco
  ACBrETQ1.ImprimirCaixa(3,3,19,19,19,0,2);
  ACBrETQ1.DefinirCorPadrao;
  ACBrETQ1.ImprimirQRCode(4, 4, 'https://epson.com.br/impressoras-etiquetas-coloridas-colorworks', 6);
  ACBrETQ1.DefinirCor($ffffff, 255, $000000, 0);  // Branco e Transparente
  ACBrETQ1.ImprimirTexto(or270, 'T', 1, 1, 5, 94, 'www.projetoacbr.com.br');
  ACBrETQ1.DefinirCorPadrao;
end;

procedure TFrPrincipal.ImpressaoTextoAdicionalBrancoTopo;
var
  msg: string;
begin
  if (not edTextoImprimir.Visible) or edTextoImprimir.Text.IsEmpty then
    Exit;

  msg := PadCenter(edTextoImprimir.Text, edTextoImprimir.MaxLength);
  ACBrETQ1.DefinirCor($ffffff, 255, $000000, 0); // Branco e Transparente
  ACBrETQ1.ImprimirTexto( orNormal, '0', 80, 80, 10, 6, msg );
  ACBrETQ1.DefinirCorPadrao;
end;

procedure TFrPrincipal.ImpressaoTextoAdicionalPretoFundo;
var
  msg: string;
begin
  if (not edTextoImprimir.Visible) or edTextoImprimir.Text.IsEmpty then
    Exit;

  msg := PadCenter(edTextoImprimir.Text, edTextoImprimir.MaxLength);
  ACBrETQ1.DefinirCorPadrao;
  ACBrETQ1.ImprimirTexto( orNormal, '0', 100, 100, 120, 4, msg );
  ACBrETQ1.DefinirCor($ffffff, 255, $000000, 0); // Branco e Transparente
  ACBrETQ1.ImprimirTexto(or270, 'T', 1, 1, 5, 94, 'www.projetoacbr.com.br');
end;

procedure TFrPrincipal.ImpressaoTextoWC;
begin

end;

procedure TFrPrincipal.ImpressaoTextoWiFi;
var
  s, rede, senha: string;
  p: Integer;
begin
  if (not edTextoImprimir.Visible) or edTextoImprimir.Text.IsEmpty then
    Exit;

  s := edTextoImprimir.Text;
  p := pos(' ',s);
  if (p = 0) then
    p := s.Length;

  rede := Copy(s, 1, p-1);
  senha := Copy(s, p+1, s.Length);

  ACBrETQ1.DefinirCorPadrao;
  ACBrETQ1.ImprimirTexto( orNormal, '0', 100, 100, 99, 35, rede );
  if not senha.IsEmpty then
    ACBrETQ1.ImprimirTexto( orNormal, '0', 100, 100, 108, 35, senha );
end;

procedure TFrPrincipal.ImpressaoTextoAdicionalBrancoFundoMaisColorWorks;
begin
 ImpressaoTextoAdicionalBrancoFundo;
 ImpressaoColorWorks;
end;

procedure TFrPrincipal.ImpressaoTextoAdicionalBrancoFundo;
var
  msg: string;
begin
  if (not edTextoImprimir.Visible) or edTextoImprimir.Text.IsEmpty then
    Exit;

  msg := PadCenter(edTextoImprimir.Text, edTextoImprimir.MaxLength);
  ACBrETQ1.DefinirCor($ffffff, 255, $000000, 0); // Branco e Transparente
  ACBrETQ1.ImprimirTexto( orNormal, '0', 80, 80, 120, 6, msg );
  ACBrETQ1.ImprimirTexto(or270, 'T', 1, 1, 5, 94, 'www.projetoacbr.com.br');
  ACBrETQ1.DefinirCorPadrao;
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

  if Assigned(FProcImpressao) then
    FProcImpressao;

  ACBrETQ1.Imprimir(Trunc(sbCopias.Value));

  ACBrETQ1.ApagarImagem(NomeImagem);
  ACBrETQ1.Desativar;

  TabBack;
end;

procedure TFrPrincipal.ConfigurarACBrETQ;
begin
  if edtPortaImpresora.Text.IsEmpty then
    raise Exception.Create('Porta da Impressora não Definida');

  with ACBrETQ1 do
  begin
     Desativar;
     DPI := TACBrETQDPI(cbxDPI.ItemIndex);
     Modelo := etqEscLabel;
     Porta := edtPortaImpresora.Text;
     LimparMemoria := ckLimparMemoria.IsChecked;
     Guilhotina := ckGuilhotina.IsChecked;
     Unidade := etqMilimetros;
     MargemEsquerda := 0;
     DeteccaoEtiqueta := TACBrETQDeteccaoEtiqueta(cbxDeteccao.ItemIndex);
     PaginaDeCodigo := TACBrETQPaginaCodigo(cbxPagCodigo.ItemIndex);
     ArqLOG := edtArqLog.Text;

     Ativar;
     edtPortaImpresora.Text := Porta;
  end;
end;

end.
