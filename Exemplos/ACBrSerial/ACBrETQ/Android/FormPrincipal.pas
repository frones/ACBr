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
    laBtnWC: TLayout;
    iWC: TImage;
    ShadowEffect5: TShadowEffect;
    Label36: TLabel;
    FloatAnimation3: TFloatAnimation;
    laBtnFrases: TLayout;
    iFrases: TImage;
    Label37: TLabel;
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
    laBtnSlap: TLayout;
    iSlap: TImage;
    ShadowEffect6: TShadowEffect;
    Label4: TLabel;
    laBtnLembranca: TLayout;
    iLembranca: TImage;
    ShadowEffect7: TShadowEffect;
    Label5: TLabel;
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
    img_misto: TImage;
    tabSlap: TTabItem;
    ToolBar6: TToolBar;
    Label12: TLabel;
    iToolSlap: TImage;
    ShadowEffect16: TShadowEffect;
    SpeedButton8: TSpeedButton;
    VertScrollBox5: TVertScrollBox;
    FlowLayout5: TFlowLayout;
    img_avise: TImage;
    img_horas: TImage;
    img_banheiro: TImage;
    img_ingles: TImage;
    img_talento: TImage;
    lAvise: TLabel;
    lHoras: TLabel;
    lBanheiro: TLabel;
    lIngles: TLabel;
    lTalento: TLabel;
    GridPanelLayout3: TGridPanelLayout;
    bFotoLembranca: TCornerButton;
    bLerImagem: TCornerButton;
    Layout2: TLayout;
    bImprimirLembranca: TCornerButton;
    sbCopiasLembranca: TSpinBox;
    edtLembranca: TEdit;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem4: TListBoxItem;
    edTextoLembranca: TEdit;
    Label13: TLabel;
    ListBoxItem5: TListBoxItem;
    edTextoEuAmo: TEdit;
    Label14: TLabel;
    ListBoxItem6: TListBoxItem;
    edTextoFrases: TEdit;
    Label15: TLabel;
    ListBoxItem7: TListBoxItem;
    edTextoSinalizacoes: TEdit;
    Label16: TLabel;
    ListBoxItem8: TListBoxItem;
    edTextoWC: TEdit;
    Label17: TLabel;
    ListBoxItem9: TListBoxItem;
    edTextoSlap: TEdit;
    Label18: TLabel;
    StyleBook1: TStyleBook;
    GridPanelLayout4: TGridPanelLayout;
    sbLarguraEtiqueta: TSpinBox;
    sbMargem: TSpinBox;
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
    procedure img_leiaClick(Sender: TObject);
    procedure img_superClick(Sender: TObject);
    procedure img_wonderClick(Sender: TObject);
    procedure img_mascClick(Sender: TObject);
    procedure img_feminClick(Sender: TObject);
    procedure img_acessivelClick(Sender: TObject);
    procedure img_mistoClick(Sender: TObject);
    procedure laBtnSlapClick(Sender: TObject);
    procedure img_aviseClick(Sender: TObject);
    procedure img_horasClick(Sender: TObject);
    procedure img_banheiroClick(Sender: TObject);
    procedure img_inglesClick(Sender: TObject);
    procedure img_talentoClick(Sender: TObject);
    procedure bLerImagemClick(Sender: TObject);
    procedure bImprimirLembrancaClick(Sender: TObject);
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

    function CalcularPathImagem(Pasta, Imagem: String): String;
    function CalcularNomeArqINI: String;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

    procedure ImpressaoTextoAdicionalBrancoTopo;
    procedure ImpressaoTextoAdicionalBrancoFundoMaisColorWorks;
    procedure ImpressaoTextoAdicionalBrancoFundo;
    procedure ImpressaoTextoAdicionalPretoFundo;
    procedure ImpressaoTextoAdicionalPretoFundoMaisACBrLateralBranca;
    procedure ImpressaoTextoWiFi;
    procedure ImpressaoTextoSlapEsquerda;
    procedure ImpressaoTextoSlapFundo;
    procedure ImpressaoTextoWCPretoFundo;
    procedure ImpressaoTextoWCBrancoTopo;
    procedure ImpressaoColorWorks;
    procedure ImpressaoLembranca;
    procedure ImpressaoACBrLateralBranca;

    function PedirPermissoes: Boolean;

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
  System.Math,
  FMX.DialogService.Async,
  FMX.MediaLibrary,
  FMX.Platform,
  {$IfDef ANDROID}
   Androidapi.JNI.Widget,
   Androidapi.JNI.Os,
   Androidapi.Helpers,
   System.Permissions,
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
  iToolSlap.Bitmap.Assign(iSlap.Bitmap);
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
  img_misto.Bitmap.LoadFromFile(CalcularPathImagem('wc','misto'));

  img_wifi.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','wifi'));
  img_fumar.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','fumar'));
  img_devm.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','devm'));
  img_devf.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','devf'));
  img_cao.Bitmap.LoadFromFile(CalcularPathImagem('sinalize','cao'));

  img_avise.Bitmap.LoadFromFile(CalcularPathImagem('slap','avise'));
  img_horas.Bitmap.LoadFromFile(CalcularPathImagem('slap','horas'));
  img_banheiro.Bitmap.LoadFromFile(CalcularPathImagem('slap','banheiro'));
  img_ingles.Bitmap.LoadFromFile(CalcularPathImagem('slap','ingles'));
  img_talento.Bitmap.LoadFromFile(CalcularPathImagem('slap','talento'));
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
    bLerImagemClick(nil);
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
  FTextoImpressao := edTextoEuAmo.Text;
  TabForward(tabEuAmo);
end;

procedure TFrPrincipal.laBtnFrasesClick(Sender: TObject);
begin
  FTextoImpressao := edTextoFrases.Text;
  TabForward(tabFrases);
end;

procedure TFrPrincipal.laBtnLembrancaClick(Sender: TObject);
begin
  imgLembranca.Bitmap := nil;
  edtLembranca.Text := edTextoLembranca.Text;
  TabForward(tabLembranca);
end;

procedure TFrPrincipal.laBtnSinalizacoesClick(Sender: TObject);
begin
  FTextoImpressao := edTextoSinalizacoes.Text;
  TabForward(tabSinalize);
end;

procedure TFrPrincipal.laBtnSlapClick(Sender: TObject);
begin
  lAvise.Text := edTextoSlap.Text;
  TabForward(tabSlap);
end;

procedure TFrPrincipal.laBtnWCClick(Sender: TObject);
begin
  FTextoImpressao := edTextoWC.Text;
  TabForward(tabWC);
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
    sbMargem.Value := INI.ReadInteger('Diversos','Margem', 0);
    edtArqLog.Text := INI.ReadString('Diversos','ArqLog','');

    edTextoEuAmo.Text := INI.ReadString('Texto','EuAmo',edTextoEuAmo.Text);
    edTextoFrases.Text := INI.ReadString('Texto','Frases',edTextoFrases.Text);
    edTextoSinalizacoes.Text := INI.ReadString('Texto','Sinalizacoes',edTextoSinalizacoes.Text);
    edTextoWC.Text := INI.ReadString('Texto','WC',edTextoWC.Text);
    edTextoSlap.Text := INI.ReadString('Texto','Slap',edTextoSlap.Text);
    edTextoLembranca.Text := INI.ReadString('Texto','Lembranca',edTextoLembranca.Text);
  finally
    INI.Free ;
  end;
end;

function TFrPrincipal.PedirPermissoes: Boolean;
Var
  Ok: Boolean;
begin
  Ok := True;
  {$IfDef ANDROID}
  PermissionsService.RequestPermissions( [JStringToString(TJManifest_permission.JavaClass.BLUETOOTH),
                                          JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_ADMIN),
                                          JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_PRIVILEGED),
                                          JStringToString(TJManifest_permission.JavaClass.CAMERA)],
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      var
        GR: TPermissionStatus;
      begin
        Ok := (Length(AGrantResults) = 4);

        if Ok then
        begin
          for GR in AGrantResults do
            if (GR <> TPermissionStatus.Granted) then
            begin
              Ok := False;
              Break;
            end;
        end;
      end );

  if not OK then
  begin
    TDialogServiceAsync.MessageDialog( 'Sem permissões para acessar despositivo BlueTooth',
                                       TMsgDlgType.mtError, [TMsgDlgBtn.mbOK],
                                       TMsgDlgBtn.mbOk, 0, nil, nil);
  end;
  {$EndIf}

  Result := Ok;
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
    INI.WriteInteger('Diversos','Margem', Trunc(sbMargem.Value));

    INI.WriteString('Diversos','ArqLog',edtArqLog.Text);

    INI.WriteString('Texto','EuAmo',edTextoEuAmo.Text);
    INI.WriteString('Texto','Frases',edTextoFrases.Text);
    INI.WriteString('Texto','Sinalizacoes',edTextoSinalizacoes.Text);
    INI.WriteString('Texto','WC',edTextoWC.Text);
    INI.WriteString('Texto','Slap',edTextoSlap.Text);
    INI.WriteString('Texto','Lembranca',edTextoLembranca.Text);
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

procedure TFrPrincipal.img_acessivelClick(Sender: TObject);
begin
  IrParaImpressao( img_acessivel.Bitmap,
                   CalcularPathImagem('wc', 'acessivel'),
                   30, ImpressaoTextoWCBrancoTopo);
end;

procedure TFrPrincipal.img_aviseClick(Sender: TObject);
begin
  FTextoImpressao := lAvise.Text;
  IrParaImpressao( img_avise.Bitmap,
                   CalcularPathImagem('slap', 'avise'),
                   48, ImpressaoTextoSlapEsquerda);
end;

procedure TFrPrincipal.img_banheiroClick(Sender: TObject);
begin
  FTextoImpressao := lBanheiro.Text;
  IrParaImpressao( img_banheiro.Bitmap,
                   CalcularPathImagem('slap', 'banheiro'),
                   48, ImpressaoTextoSlapEsquerda);
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
                   CalcularPathImagem('euamo', 'colorworks'),
                   0, ImpressaoColorWorks );
end;

procedure TFrPrincipal.img_dartClick(Sender: TObject);
begin
  IrParaImpressao( img_dart.Bitmap,
                   CalcularPathImagem('wc', 'dart'),
                   30, ImpressaoTextoWCPretoFundo);
end;

procedure TFrPrincipal.img_devfClick(Sender: TObject);
begin
  IrParaImpressao( img_devf.Bitmap,
                   CalcularPathImagem('sinalize', 'devf'),
                   22, ImpressaoTextoAdicionalPretoFundoMaisACBrLateralBranca);
end;

procedure TFrPrincipal.img_devmClick(Sender: TObject);
begin
  IrParaImpressao( img_devm.Bitmap,
                   CalcularPathImagem('sinalize', 'devm'),
                   22, ImpressaoTextoAdicionalPretoFundoMaisACBrLateralBranca);
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

procedure TFrPrincipal.img_feminClick(Sender: TObject);
begin
  IrParaImpressao( img_femin.Bitmap,
                   CalcularPathImagem('wc', 'femin'),
                   30, ImpressaoTextoWCBrancoTopo);
end;

procedure TFrPrincipal.img_fumarClick(Sender: TObject);
begin
  IrParaImpressao( img_fumar.Bitmap,
                   CalcularPathImagem('sinalize', 'fumar'),
                   0, nil);
end;

procedure TFrPrincipal.img_horasClick(Sender: TObject);
begin
  FTextoImpressao := lHoras.Text;
  IrParaImpressao( img_horas.Bitmap,
                   CalcularPathImagem('slap', 'horas'),
                   48, ImpressaoTextoSlapEsquerda);
end;

procedure TFrPrincipal.img_inglesClick(Sender: TObject);
begin
  FTextoImpressao := lIngles.Text;
  IrParaImpressao( img_ingles.Bitmap,
                   CalcularPathImagem('slap', 'ingles'),
                   48, ImpressaoTextoSlapEsquerda);
end;

procedure TFrPrincipal.img_iniciarClick(Sender: TObject);
begin
  IrParaImpressao( img_iniciar.Bitmap,
                   CalcularPathImagem('frases', 'iniciar'),
                   26, ImpressaoTextoAdicionalBrancoTopo);
end;

procedure TFrPrincipal.img_leiaClick(Sender: TObject);
begin
  IrParaImpressao( img_leia.Bitmap,
                   CalcularPathImagem('wc', 'leia'),
                   30, ImpressaoTextoWCPretoFundo);
end;

procedure TFrPrincipal.img_mascClick(Sender: TObject);
begin
  IrParaImpressao( img_masc.Bitmap,
                   CalcularPathImagem('wc', 'masc'),
                   30, ImpressaoTextoWCBrancoTopo);
end;

procedure TFrPrincipal.img_mistoClick(Sender: TObject);
begin
  IrParaImpressao( img_misto.Bitmap,
                   CalcularPathImagem('wc', 'misto'),
                   30, ImpressaoTextoWCBrancoTopo);
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

procedure TFrPrincipal.img_superClick(Sender: TObject);
begin
  IrParaImpressao( img_super.Bitmap,
                   CalcularPathImagem('wc', 'super'),
                   30, ImpressaoTextoWCPretoFundo);
end;

procedure TFrPrincipal.img_talentoClick(Sender: TObject);
begin
  FTextoImpressao := lTalento.Text;
  IrParaImpressao( img_talento.Bitmap,
                   CalcularPathImagem('slap', 'talento'),
                   80, ImpressaoTextoSlapFundo);
end;

procedure TFrPrincipal.img_wifiClick(Sender: TObject);
begin
  IrParaImpressao( img_wifi.Bitmap,
                   CalcularPathImagem('sinalize', 'wifi'),
                   30, ImpressaoTextoWiFi);
end;

procedure TFrPrincipal.img_wonderClick(Sender: TObject);
begin
  IrParaImpressao( img_wonder.Bitmap,
                   CalcularPathImagem('wc', 'wonder'),
                   30, ImpressaoTextoWCPretoFundo);
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

procedure TFrPrincipal.ImpressaoACBrLateralBranca;
begin
  ACBrETQ1.DefinirCor($ffffff, 255, $000000, 0); // Branco e Transparente
  ACBrETQ1.ImprimirTexto(or270, 'T', 1, 1, 5, 94+Trunc(sbMargem.Value), 'www.projetoacbr.com.br');
  ACBrETQ1.DefinirCorPadrao;
end;

procedure TFrPrincipal.ImpressaoColorWorks;
begin
  ACBrETQ1.DefinirCor($ffffff, 255, $ffffff, 255);  // Branco
  ACBrETQ1.ImprimirCaixa(3,3+Trunc(sbMargem.Value),19,19,19,0,2);
  ACBrETQ1.DefinirCorPadrao;
  ACBrETQ1.ImprimirQRCode(4, 4+Trunc(sbMargem.Value), 'https://epson.com.br/impressoras-etiquetas-coloridas-colorworks', 6);
  ImpressaoACBrLateralBranca;
  ACBrETQ1.DefinirCorPadrao;
end;

procedure TFrPrincipal.ImpressaoLembranca;
begin
  ImpressaoTextoAdicionalBrancoTopo;
  ImpressaoACBrLateralBranca;
end;

procedure TFrPrincipal.ImpressaoTextoAdicionalBrancoTopo;
var
  msg: string;
begin
  if (not edTextoImprimir.Visible) or edTextoImprimir.Text.IsEmpty then
    Exit;

  msg := PadCenter(edTextoImprimir.Text, edTextoImprimir.MaxLength);
  ACBrETQ1.DefinirCor($ffffff, 255, $000000, 0); // Branco e Transparente
  ACBrETQ1.ImprimirTexto( orNormal, '0', 80, 80, 10, 6+Trunc(sbMargem.Value), msg );
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
  ACBrETQ1.ImprimirTexto( orNormal, '0', 100, 100, 120, 14+Trunc(sbMargem.Value), msg );
end;

procedure TFrPrincipal.ImpressaoTextoAdicionalPretoFundoMaisACBrLateralBranca;
begin
  ImpressaoTextoAdicionalPretoFundo;
  ImpressaoACBrLateralBranca;
end;

procedure TFrPrincipal.ImpressaoTextoSlapFundo;
var
  msg: string;
  SL: TStringList;
  l, i, v: Integer;
begin
  msg := edTextoImprimir.Text;
  if msg.Trim.IsEmpty then
    Exit;

  SL := TStringList.Create;
  try
    SL.Text := QuebraLinhas(msg, 20);
    l := min(SL.Count, 4);
    v := 28;
    if (l < 3) then
      Inc(v, 7);

    ACBrETQ1.DefinirCor($FF0000, 255, $FF0000, 255);  // Azul, Azul
    ACBrETQ1.ImprimirCaixa(27, 1+Trunc(sbMargem.Value), 98, 30, 0, 0, 2);
    ACBrETQ1.DefinirCor($FFFFFF, 255, $000000, 0);
    for i := 0 to l-1 do
    begin
      ACBrETQ1.ImprimirTexto(orNormal, 'E', 40, 100, v, 2+Trunc(sbMargem.Value), PadCenter(SL[i], 20));
      Inc(v, 7);
    end;
    ACBrETQ1.DefinirCorPadrao;
  finally
    SL.Free;
  end;

end;

procedure TFrPrincipal.ImpressaoTextoSlapEsquerda;
var
  msg: string;
  SL: TStringList;
  l, i, v: Integer;
begin
  msg := edTextoImprimir.Text;
  if msg.Trim.IsEmpty then
    Exit;

  SL := TStringList.Create;
  try
    SL.Text := QuebraLinhas(msg, 12);
    l := min(SL.Count, 4);
    v := 7;
    if (l < 3) then
      Inc(v, 11);

    ACBrETQ1.DefinirCor($0000FF, 255, $0000FF, 255);  // Vermelho $0000FF
    ACBrETQ1.ImprimirCaixa(6, 1, 60, 48, 0, 0, 2);
    ACBrETQ1.DefinirCor($FFFFFF, 255, $000000, 0);  // Branco e Transparente
    for i := 0 to l-1 do
    begin
      msg := PadCenter(SL[i], 12);
      ACBrETQ1.ImprimirTexto(orNormal, 'E', 40, 150, v, 2+Trunc(sbMargem.Value), msg);
      Inc(v, 11);
    end;
    ACBrETQ1.DefinirCorPadrao;
  finally
    SL.Free;
  end;
end;

procedure TFrPrincipal.ImpressaoTextoWCBrancoTopo;
var
  msg: string;
begin
  if (not edTextoImprimir.Visible) or edTextoImprimir.Text.IsEmpty then
    Exit;

  msg := PadCenter(edTextoImprimir.Text, edTextoImprimir.MaxLength);
  ACBrETQ1.DefinirCor($ffffff, 255, $000000, 0); // Branco e Transparente
  ACBrETQ1.ImprimirTexto( orNormal, '0', 50, 50, 12, 14+Trunc(sbMargem.Value), msg );
  ACBrETQ1.DefinirCorPadrao;
end;

procedure TFrPrincipal.ImpressaoTextoWCPretoFundo;
var
  msg: string;
begin
  if (not edTextoImprimir.Visible) or edTextoImprimir.Text.IsEmpty then
    Exit;

  msg := PadCenter(edTextoImprimir.Text, edTextoImprimir.MaxLength);
  ACBrETQ1.DefinirCorPadrao;
  ACBrETQ1.ImprimirTexto( orNormal, '0', 50, 50, 122, 18+Trunc(sbMargem.Value), msg );
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
  ACBrETQ1.ImprimirTexto( orNormal, '0', 100, 100, 99, 35+Trunc(sbMargem.Value), rede );
  if not senha.IsEmpty then
    ACBrETQ1.ImprimirTexto( orNormal, '0', 100, 100, 108, 35+Trunc(sbMargem.Value), senha );
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
  ACBrETQ1.ImprimirTexto( orNormal, '0', 80, 80, 120, 6+Trunc(sbMargem.Value), msg );
  ImpressaoACBrLateralBranca;
  ACBrETQ1.DefinirCorPadrao;
end;

procedure TFrPrincipal.bImprimirLembrancaClick(Sender: TObject);
var
  NomeImagem: string;
  MS: TMemoryStream;
  w, h: Cardinal;
  b, c, o, f, i: Byte;
  msg: string;
begin
  NomeImagem := 'ACBR.PNG';
  ConfigurarACBrETQ;

  MS := TMemoryStream.Create;
  try
    imgLembranca.Bitmap.SaveToStream(MS);
    ACBrETQ1.CarregarImagem(MS, NomeImagem);
  finally
    MS.Free;
  end;

  ACBrETQ1.DefinirDimensoes( Trunc(sbLarguraEtiqueta.Value),
                             trunc(ConverterUnidade(etqDots, imgLembranca.Bitmap.Height, etqMilimetros, ACBrETQ1.DPI)),
                             -1, -1);
  ACBrETQ1.ImprimirImagem(1, 0, Trunc(sbMargem.Value), NomeImagem);
  if not edtLembranca.Text.IsEmpty then
  begin
    msg := ' '+edtLembranca.Text+' - '+FormatDateTime('dd/mm/yy hh:nn', Now)+' ';
    ACBrETQ1.DefinirCor($FF0000, 255, $ffffff, 100); // Azul sobre Branco com opacidade
    ACBrETQ1.ImprimirTexto(or90, 'S', 1, 1, 5, 1+Trunc(sbMargem.Value), msg);
    ACBrETQ1.DefinirCorPadrao;
  end;

  ACBrETQ1.Imprimir(Trunc(sbCopiasLembranca.Value));

  ACBrETQ1.ApagarImagem(NomeImagem);
  ACBrETQ1.Desativar;

  TabBack;
end;

procedure TFrPrincipal.bLerImagemClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := edPastaImagens.Text;
  OpenDialog1.Filter := 'Arquivos de Imagem (png,jpg,bmp)|*.png;*.jpg;*.bmp|Todos Arquivos (*.*)|*.*';
  imgLembranca.Bitmap := Nil;
  if OpenDialog1.Execute then
  begin
    imgLembranca.Bitmap.LoadFromFile(OpenDialog1.FileName);
    if imgLembranca.Bitmap.Width > imgLembranca.Bitmap.Height then
      imgLembranca.Bitmap.Rotate(90);
  end
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
  ACBrETQ1.ImprimirImagem(1, 0, Trunc(sbMargem.Value), NomeImagem);

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
     Device.SendBytesCount := 1024000;
     Device.SendBytesInterval := 1;
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
