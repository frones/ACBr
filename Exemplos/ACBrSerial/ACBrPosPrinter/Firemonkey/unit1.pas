unit Unit1;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR



uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Soap.EncdDecd,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, System.ImageList, FMX.ImgList, FMX.EditBox, FMX.SpinBox,
  FMX.ScrollBox, FMX.Controls.Presentation,
  ACBrPosPrinter, ACBrBase, ACBrDevice, FMX.ComboEdit;

type

  { TFrPosPrinterTeste }

  TFrPosPrinterTeste = class(TForm)
StyleBook1: TStyleBook;
    bAtivar: TButton;
    bApagarLogo: TButton;
    bImprimir: TButton;
    bImprimirLogo: TButton;
    bImpTagsValidas: TButton;
    bLerInfo: TButton;
    bLimpar: TButton;
    bTagFormtacaoCaracter: TButton;
    bTagGaveta: TButton;
    bTagLogo: TButton;
    bTagBMP: TButton;
    bTagQRCode: TButton;
    bLerStatus: TButton;
    bTagsAlinhamento: TButton;
    bTagsCodBarras: TButton;
    bTagsTesteInvalidas: TButton;
    bTagsTestePagCodigo: TButton;
    bImpLinhaALinha: TButton;
    bTagsTestePageMode: TButton;
    Button1: TButton;
    bCaregarImagem: TButton;
    bImprimirImagem: TButton;
    bGravarLogo: TButton;
    bConverter: TButton;
    cbCortarPapel: TCheckBox;
    cbHRI: TCheckBox;
    cbGavetaSinalInvertido: TCheckBox;
    cbxLimparTexto: TCheckBox;
    cbxModelo: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboEdit;
    cbIgnorarTags: TCheckBox;
    cbTraduzirTags: TCheckBox;
    cbControlePorta: TCheckBox;
    edImagem: TEdit;
    edLog: TEdit;
    gbCodBarrasConfig1: TGroupBox;
    gbCodBarrasConfig2: TGroupBox;
    gbGavetaConfig: TGroupBox;
    gbConfiguracao: TGroupBox;
    gbCodBarrasConfig: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    mImp: TMemo;
    mLog: TMemo;
    OpenPictureDialog1: TOpenDialog;
    PageControl1: TTabControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    rbArquivo: TRadioButton;
    rbStream: TRadioButton;
    SbArqLog: TSpeedButton;
    btSerial: TSpeedButton;
    seGavetaTempoON: TSpinBox;
    seGavetaTempoOFF: TSpinBox;
    seLogoFatorX: TSpinBox;
    seLogoFatorY: TSpinBox;
    seLogoKC1: TSpinBox;
    seLogoKC2: TSpinBox;
    seQRCodeLarguraModulo: TSpinBox;
    seQRCodeErrorLevel: TSpinBox;
    seQRCodeTipo: TSpinBox;
    seColunas: TSpinBox;
    seBarrasLargura: TSpinBox;
    seEspLinhas: TSpinBox;
    seBarrasAltura: TSpinBox;
    seLinhasBuffer: TSpinBox;
    seLinhasPular: TSpinBox;
    seGavetaNum: TSpinBox;
    tsImagens: TTabItem;
    tsImprimir: TTabItem;
    tsLog: TTabItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    btSerial_SubImage: TImage;
    ACBrPosPrinter1: TACBrPosPrinter;
    btSearchPorts: TButton;
    btInfoUSB: TButton;
    procedure ACBrPosPrinter1GravarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure bApagarLogoClick(Sender: TObject);
    procedure bAtivarClick(Sender: TObject);
    procedure bCaregarImagemClick(Sender: TObject);
    procedure bConverterClick(Sender: TObject);
    procedure bGravarLogoClick(Sender: TObject);
    procedure bImprimirClick(Sender: TObject);
    procedure bImprimirImagemClick(Sender: TObject);
    procedure bImprimirLogoClick(Sender: TObject);
    procedure bImpTagsValidasClick(Sender: TObject);
    procedure bLerInfoClick(Sender: TObject);
    procedure bLerStatusClick(Sender: TObject);
    procedure bLimparClick(Sender: TObject);
    procedure bTagFormtacaoCaracterClick(Sender: TObject);
    procedure bTagGavetaClick(Sender: TObject);
    procedure bTagLogoClick(Sender: TObject);
    procedure bTagQRCodeClick(Sender: TObject);
    procedure bTagsAlinhamentoClick(Sender: TObject);
    procedure bTagsTesteInvalidasClick(Sender: TObject);
    procedure bTagsCodBarrasClick(Sender: TObject);
    procedure bTagsTestePagCodigoClick(Sender: TObject);
    procedure bImpLinhaALinhaClick(Sender: TObject);
    procedure bTagsTestePageModeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbControlePortaChange(Sender: TObject);
    procedure cbCortarPapelChange(Sender: TObject);
    procedure cbGavetaSinalInvertidoChange(Sender: TObject);
    procedure cbHRIChange(Sender: TObject);
    procedure cbIgnorarTagsChange(Sender: TObject);
    procedure cbTraduzirTagsChange(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure cbxPagCodigoChange(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SbArqLogClick(Sender: TObject);
    procedure seBarrasAlturaChange(Sender: TObject);
    procedure seGavetaTempoOFFChange(Sender: TObject);
    procedure seGavetaTempoONChange(Sender: TObject);
    procedure seBarrasLarguraChange(Sender: TObject);
    procedure seEspLinhasChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
    procedure seColunasChange(Sender: TObject);
    procedure seLinhasBufferChange(Sender: TObject);
    procedure seLinhasPularChange(Sender: TObject);
    procedure seLogoFatorXChange(Sender: TObject);
    procedure seLogoFatorYChange(Sender: TObject);
    procedure seLogoKC1Change(Sender: TObject);
    procedure seLogoKC2Change(Sender: TObject);
    procedure seQRCodeErrorLevelChange(Sender: TObject);
    procedure seQRCodeLarguraModuloChange(Sender: TObject);
    procedure seQRCodeTipoChange(Sender: TObject);
    procedure LimparTexto;
    procedure bTagBMPClick(Sender: TObject);
    procedure btSearchPortsClick(Sender: TObject);
    procedure btInfoUSBClick(Sender: TObject);
  private
    { private declarations }
    Procedure GravarINI ;
    Procedure LerINI ;
  public
    { public declarations }
  end;

var
  FrPosPrinterTeste: TFrPosPrinterTeste;

implementation

Uses
  FMX.Printer,
  typinfo, math, System.StrUtils,
  synacode,
  ConfiguraSerial,
  ACBrUtil, ACBrImage, ACBrConsts
  {$IfDef MSWINDOWS}
   ,ACBrWinUSBDevice
  {$EndIf};


{$R *.FMX}

{ TFrPosPrinterTeste }

procedure TFrPosPrinterTeste.FormCreate(Sender: TObject);
var
  I: TACBrPosPrinterModelo;
  J: TACBrPosPaginaCodigo;
  K: Integer;
begin
  cbxModelo.Items.Clear ;
  For I := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(I) ) ) ;

  cbxPagCodigo.Items.Clear ;
  For J := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(J) ) ) ;

  btSearchPortsClick(Sender);
  PageControl1.First;

  LerINI;
end;

procedure TFrPosPrinterTeste.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  GravarINI;
end;

procedure TFrPosPrinterTeste.bLimparClick(Sender: TObject);
begin
  mImp.Lines.Clear;
end;

procedure TFrPosPrinterTeste.bTagFormtacaoCaracterClick(Sender: TObject);
begin
  LimparTexto;
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('FONTE NORMAL: '+IntToStr(ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
  mImp.Lines.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteNormal));
  mImp.Lines.Add('<e>EXPANDIDO: '+IntToStr(ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
  mImp.Lines.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteExpandida));
  mImp.Lines.Add('</e><c>CONDENSADO: '+IntToStr(ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
  mImp.Lines.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteCondensada));
  mImp.Lines.Add('</c><n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');
  mImp.Lines.Add('FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('<n>LIGA NEGRITO');
  mImp.Lines.Add('<i>LIGA ITALICO');
  mImp.Lines.Add('<S>LIGA SUBLINHADA');
  mImp.Lines.Add('<c>LIGA CONDENSADA');
  mImp.Lines.Add('<e>LIGA EXPANDIDA');
  mImp.Lines.Add('<a>LIGA ALTURA DUPLA');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('<e><n>NEGRITO E EXPANDIDA</n></e>');
  mImp.Lines.Add('<c><n>NEGRITO E CONDENSADA</n></c>');
  mImp.Lines.Add('<e><a>EXPANDIDA E ALT.DUPLA</a></e>');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('<in><e>INVERTIDA E EXPANDIDA</e></in>');
  mImp.Lines.Add('<in><c>INVERTIDA E CONDENSADA</c></in>');
  mImp.Lines.Add('<in><a>INVERTIDA E ALT.DUPLA</a></in>');
  mImp.Lines.Add('</fn>FONTE NORMAL');
  mImp.Lines.Add('</linha_simples>');
  mImp.Lines.Add('</FB>FONTE TIPO B');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');
  mImp.Lines.Add('</FA>FONTE TIPO A');
  mImp.Lines.Add('</FN>FONTE NORMAL');
  mImp.Lines.Add('</corte_total>');     
end;

procedure TFrPosPrinterTeste.bTagGavetaClick(Sender: TObject);
begin
  LimparTexto;
  mImp.Lines.Add('Abertura da Gaveta padr„o');
  mImp.Lines.Add('</abre_gaveta>');
  mImp.Lines.Add('');
  mImp.Lines.Add('');
  mImp.Lines.Add('Abertura da Gaveta especÌfica');
  mImp.Lines.Add('<abre_gaveta>'+IntToStr(Trunc(seGavetaNum.Value))+'</abre_gaveta>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagLogoClick(Sender: TObject);
begin
  LimparTexto;
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('<logo_imprimir>'+ifthen(ACBrPosPrinter1.ConfigLogo.IgnorarLogo,'0','1')+'</logo_imprimir>');
  mImp.Lines.Add('<logo_kc1>'+IntToStr(ACBrPosPrinter1.ConfigLogo.KeyCode1)+'</logo_kc1>');
  mImp.Lines.Add('<logo_kc2>'+IntToStr(ACBrPosPrinter1.ConfigLogo.KeyCode2)+'</logo_kc2>');
  mImp.Lines.Add('<logo_fatorx>'+IntToStr(ACBrPosPrinter1.ConfigLogo.FatorX)+'</logo_fatorx>');
  mImp.Lines.Add('<logo_fatory>'+IntToStr(ACBrPosPrinter1.ConfigLogo.FatorY)+'</logo_fatory>');
  mImp.Lines.Add('</logo>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagQRCodeClick(Sender: TObject);
begin
  LimparTexto;
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('<qrcode_tipo>'+IntToStr(ACBrPosPrinter1.ConfigQRCode.Tipo)+'</qrcode_tipo>');
  mImp.Lines.Add('<qrcode_largura>'+IntToStr(ACBrPosPrinter1.ConfigQRCode.LarguraModulo)+'</qrcode_largura>');
  mImp.Lines.Add('<qrcode_error>'+IntToStr(ACBrPosPrinter1.ConfigQRCode.ErrorLevel)+'</qrcode_error>');
  mImp.Lines.Add('<qrcode>http://projetoacbr.com.br</qrcode>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('</ad>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/questoes_importantes.html</qrcode>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('Exemplo de QRCode para NFCe');
  mImp.Lines.Add('<qrcode_error>0</qrcode_error><qrcode>https://www.homologacao.nfce.fazenda.sp.gov.br/NFCeConsultaPublica/Paginas/ConsultaQRCode.aspx?'+
                 'chNFe=35150805481336000137650220000000711000001960&nVersao=100&tpAmb=2&dhEmi=323031352D30382D31395432323A33333A32352D30333A3030&vNF=3.00&'+
                 'vICMS=0.12&digVal=776967396F2B665861706673396878776E64594C396F61654C35493D&cIdToken=000001&cHashQRCode=9BD312D558823E1EC68CEDB338A39B6150B0480E</qrcode>');
  mImp.Lines.Add('Exemplo de QRCode para SAT');
  mImp.Lines.Add('<qrcode_error>0</qrcode_error><qrcode>35150811111111111111591234567890001672668828|20150820201736|118.72|05481336000137|'+
                 'TCbeD81ePUpMvso4VjFqRTvs4ovqmR1ZG3bwSCumzHtW8bbMedVJjVnww103v3LxKfgckAyuizcR/9pXaKay6M4Gu8kyDef+6VH5qONIZV1cB+mFfXiaCgeZ'+
                 'ALuRDCH1PRyb6hoBeRUkUk6lOdXSczRW9Y83GJMXdOFroEbzFmpf4+WOhe2BZ3mEdXKKGMfl1EB0JWnAThkGT+1Er9Jh/3En5YI4hgQP3NC2BiJVJ6oCEbKb'+
                 '85s5915DSZAw4qB/MlESWViDsDVYEnS/FQgA2kP2A9pR4+agdHmgWiz30MJYqX5Ng9XEYvvOMzl1Y6+7/frzsocOxfuQyFsnfJzogw==</qrcode>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsAlinhamentoClick(Sender: TObject);
var
  BmpMono: String;
begin
  BmpMono := ApplicationPath+'acbrmono.bmp';
  if not FileExists(BmpMono) then
    Image1.Bitmap.SaveToFile(BmpMono);

  LimparTexto;
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('TEXTO NORMAL');
  mImp.Lines.Add('</ae>ALINHADO A ESQUERDA');
  mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ce>ALINHADO NO CENTRO');
  mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</fn></ad>ALINHADO A DIREITA');
  mImp.Lines.Add('<bmp>'+BmpMono+'</bmp>');
  mImp.Lines.Add('1 2 3 TESTANDO');
  mImp.Lines.Add('<n>FONTE NEGRITO</N>');
  mImp.Lines.Add('<e>FONTE EXPANDIDA</e>');
  mImp.Lines.Add('<a>FONTE ALT.DUPLA</a>');
  mImp.Lines.Add('<c>FONTE CONDENSADA</c>');
  mImp.Lines.Add('<in>FONTE INVERTIDA</in>');
  mImp.Lines.Add('<S>FONTE SUBLINHADA</s>');
  mImp.Lines.Add('<i>FONTE ITALICO</i>');

  mImp.Lines.Add('</ae></fn>TEXTO NORMAL');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsTesteInvalidasClick(Sender: TObject);
begin
  LimparTexto;
  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('<CE>*** TESTE DE TAGS INV¡LIDAS ***</CE>');
  mImp.Lines.Add('<ce> <>tags inv·lidas no texto">">><<</CE>');
  mImp.Lines.Add('<AD><da><ec></</A Direita</ad>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsCodBarrasClick(Sender: TObject);
begin
  LimparTexto;
  if not ACBrPosPrinter1.Ativo then
    ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModelo.ItemIndex );

  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('<barra_mostrar>'+ifthen(cbHRI.IsChecked,'1','0')+'</barra_mostrar>');
  mImp.Lines.Add('<barra_largura>'+IntToStr(Trunc(seBarrasLargura.Value))+'</barra_largura>');
  mImp.Lines.Add('<barra_altura>'+IntToStr(Trunc(seBarrasAltura.Value))+'</barra_altura>');
  mImp.Lines.Add('</ce>');
  mImp.Lines.Add('</linha_dupla>');
  mImp.Lines.Add('EAN 8: 1234567');
  mImp.Lines.Add('<ean8>1234567</ean8>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('EAN13: 123456789012');
  mImp.Lines.Add('<ean13>123456789012</ean13>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('std25: 1234567890');
  mImp.Lines.Add('<std>1234567890</std>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('INT25: 1234567890');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE11: 1234567890');
  mImp.Lines.Add('<code11>1234567890</code11>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE39: ABCDE12345');
  mImp.Lines.Add('<code39>ABCDE12345</code39>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE93: ABC123abc');
  mImp.Lines.Add('<code93>ABC123abc</code93>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODE128: $-=+ABC123abc');
  mImp.Lines.Add('<code128>$-=+ABC123abc</code128>');
  mImp.Lines.Add('CODE128C: 3515071111111111111159');
  mImp.Lines.Add('<code128c>3515071111111111111159</code128c>');
  if ACBrPosPrinter1.TagsNaoSuportadas.IndexOf(cTagBarraCode128c) >= 0 then
  begin
    mImp.Lines.Add('<c>CODE128C: 3515071111111111111159</c>');
    mImp.Lines.Add('<code128c>3515071111111111111159</code128c>');
    mImp.Lines.Add('<c>CODE128C: 1234567890001135408700</c>');
    mImp.Lines.Add('<code128c>1234567890001135408700</code128c>');
  end
  else
  begin
    mImp.Lines.Add('<c>CODE128C: 35150711111111111111591234567890001135408700</c>');
    mImp.Lines.Add('<code128c>35150711111111111111591234567890001135408700</code128c>');
  end;

  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('UPCA: 12345678901');
  mImp.Lines.Add('<upca>12345678901</upca>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('CODABAR: A123456789012345A');
  mImp.Lines.Add('<codabar>A123456789012345A</codabar>');
  mImp.Lines.Add('</Linha_Simples>');
  mImp.Lines.Add('MSI: 1234567890');
  mImp.Lines.Add('<msi>1234567890</msi>');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsTestePagCodigoClick(Sender: TObject);
begin
  LimparTexto;
  if cbxPagCodigo.ItemIndex < 0 then Exit;

  mImp.Lines.Add('</zera>');
  mImp.Lines.Add('Fonte tipo A com P·gina de CÛdigo ' + cbxPagCodigo.Selected.Text);
  mImp.Lines.Add('¿ noite, vovÙ Kowalsky vÍ o Ìm„ cair no pÈ do ping¸im queixoso e vovÛ pıe aÁ˙car no ch· de t‚maras do jabuti feliz.');
  mImp.Lines.Add('¡…Õ”⁄·ÈÌÛ˙Á«„ı√’ Í¬‚‘Ù¿‡');
  mImp.Lines.Add('');
  mImp.Lines.Add('</FB>Fonte tipo B com P·gina de CÛdigo ' + cbxPagCodigo.Selected.Text);
  mImp.Lines.Add('¿ noite, vovÙ Kowalsky vÍ o Ìm„ cair no pÈ do ping¸im queixoso e vovÛ pıe aÁ˙car no ch· de t‚maras do jabuti feliz.');
  mImp.Lines.Add('¡…Õ”⁄·ÈÌÛ˙Á«„ı√’ Í¬‚‘Ù¿‡');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.bImpLinhaALinhaClick(Sender: TObject);
begin
  ACBrPosPrinter1.ImprimirLinha('</zera>');
  ACBrPosPrinter1.ImprimirLinha('</linha_dupla>');
  ACBrPosPrinter1.ImprimirLinha('FONTE NORMAL: '+IntToStr(ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
  ACBrPosPrinter1.ImprimirLinha(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteNormal));
  ACBrPosPrinter1.ImprimirLinha('<e>EXPANDIDO: '+IntToStr(ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
  ACBrPosPrinter1.ImprimirLinha(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteExpandida));
  ACBrPosPrinter1.ImprimirLinha('</e><c>CONDENSADO: '+IntToStr(ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
  ACBrPosPrinter1.ImprimirLinha(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteCondensada));
  ACBrPosPrinter1.ImprimirLinha('</c><n>FONTE NEGRITO</N>');
  ACBrPosPrinter1.ImprimirLinha('<in>FONTE INVERTIDA</in>');
  ACBrPosPrinter1.ImprimirLinha('<S>FONTE SUBLINHADA</s>');
  ACBrPosPrinter1.ImprimirLinha('<i>FONTE ITALICO</i>');
  ACBrPosPrinter1.ImprimirLinha('FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</linha_simples>');
  ACBrPosPrinter1.ImprimirLinha('<n>LIGA NEGRITO');
  ACBrPosPrinter1.ImprimirLinha('<i>LIGA ITALICO');
  ACBrPosPrinter1.ImprimirLinha('<S>LIGA SUBLINHADA');
  ACBrPosPrinter1.ImprimirLinha('<c>LIGA CONDENSADA');
  ACBrPosPrinter1.ImprimirLinha('<e>LIGA EXPANDIDA');
  ACBrPosPrinter1.ImprimirLinha('</fn>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</linha_simples>');
  ACBrPosPrinter1.ImprimirLinha('<e><n>NEGRITO E EXPANDIDA</n></e>');
  ACBrPosPrinter1.ImprimirLinha('</fn>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('<in><c>INVERTIDA E CONDENSADA</c></in>');
  ACBrPosPrinter1.ImprimirLinha('</fn>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</linha_simples>');
  ACBrPosPrinter1.ImprimirLinha('</FB>FONTE TIPO B');
  ACBrPosPrinter1.ImprimirLinha('<n>FONTE NEGRITO</N>');
  ACBrPosPrinter1.ImprimirLinha('<e>FONTE EXPANDIDA</e>');
  ACBrPosPrinter1.ImprimirLinha('<in>FONTE INVERTIDA</in>');
  ACBrPosPrinter1.ImprimirLinha('<S>FONTE SUBLINHADA</s>');
  ACBrPosPrinter1.ImprimirLinha('<i>FONTE ITALICO</i>');
  ACBrPosPrinter1.ImprimirLinha('</FA>FONTE TIPO A');
  ACBrPosPrinter1.ImprimirLinha('</FN>FONTE NORMAL');
  ACBrPosPrinter1.ImprimirLinha('</corte_total>');
end;

procedure TFrPosPrinterTeste.bTagsTestePageModeClick(Sender: TObject);
begin
  LimparTexto;
  mImp.Lines.Add('</zera><barra_mostrar>0</barra_mostrar><barra_largura>2</barra_largura><barra_altura>40</barra_altura>');
  mImp.Lines.Add('<mp><mp_direcao>0</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>0</mp_esquerda><mp_largura>257</mp_largura><mp_altura>740</mp_altura><mp_espaco>50</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('<mp_direcao>1</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>210</mp_esquerda><mp_largura>400</mp_largura><mp_altura>500</mp_altura><mp_espaco>25</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('</mp>');
  mImp.Lines.Add('MODO PAGINA DESLIGADO');
  mImp.Lines.Add('<mp><mp_direcao>3</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>0</mp_esquerda><mp_largura>400</mp_largura><mp_altura>500</mp_altura><mp_espaco>25</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('<mp_direcao>2</mp_direcao><mp_topo>0</mp_topo><mp_esquerda>350</mp_esquerda><mp_largura>257</mp_largura><mp_altura>740</mp_altura><mp_espaco>50</mp_espaco></mp_configurar>');
  mImp.Lines.Add('<c><n>CONDENSADA/NEGRITO</n></c>');
  mImp.Lines.Add('<e>EXPANDIDO</e>');
  mImp.Lines.Add('<in>INVERTIDA</in>');
  mImp.Lines.Add('');
  mImp.Lines.Add('<inter>1234567890</inter>');
  mImp.Lines.Add('<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>');
  mImp.Lines.Add('</mp>');
  mImp.Lines.Add('');
  mImp.Lines.Add('</corte_total>');
end;

procedure TFrPosPrinterTeste.Button1Click(Sender: TObject);
begin
  LimparTexto;
  ACBrPosPrinter1.Imprimir('<code93>1234'+#9+'5678</code93></corte_total>');
end;

procedure TFrPosPrinterTeste.cbControlePortaChange(Sender: TObject);
begin
  ACBrPosPrinter1.ControlePorta := cbControlePorta.IsChecked;
end;

procedure TFrPosPrinterTeste.cbCortarPapelChange(Sender: TObject);
begin
  ACBrPosPrinter1.CortaPapel := cbCortarPapel.IsChecked;
end;

procedure TFrPosPrinterTeste.cbGavetaSinalInvertidoChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigGaveta.SinalInvertido := cbGavetaSinalInvertido.IsChecked;
end;

procedure TFrPosPrinterTeste.cbHRIChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.MostrarCodigo := cbHRI.IsChecked;
end;

procedure TFrPosPrinterTeste.cbIgnorarTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.IgnorarTags := cbIgnorarTags.IsChecked;
end;

procedure TFrPosPrinterTeste.cbTraduzirTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.TraduzirTags := cbTraduzirTags.IsChecked;
end;

procedure TFrPosPrinterTeste.cbxModeloChange(Sender: TObject);
begin
  try
     ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModelo.ItemIndex ) ;
  except
     cbxModelo.ItemIndex := Integer( ACBrPosPrinter1.Modelo ) ;
     raise ;
  end ;
end;

procedure TFrPosPrinterTeste.cbxPagCodigoChange(Sender: TObject);
begin
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo(cbxPagCodigo.ItemIndex);
end;

procedure TFrPosPrinterTeste.cbxPortaChange(Sender: TObject);
begin
  try
    ACBrPosPrinter1.Porta := cbxPorta.Text ;
  finally
    cbxPorta.Text := ACBrPosPrinter1.Porta ;
  end ;

  btSerial.Visible := ACBrPosPrinter1.Device.IsSerialPort;
end;

procedure TFrPosPrinterTeste.SbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim,edLog.Text) = 0 then
    AFileLog := ApplicationPath + edLog.Text
  else
    AFileLog := edLog.Text;

  OpenURL( AFileLog );
end;

procedure TFrPosPrinterTeste.seBarrasAlturaChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.Altura := Trunc(seBarrasAltura.Value);
end;

procedure TFrPosPrinterTeste.seGavetaTempoOFFChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigGaveta.TempoOFF := Trunc(seGavetaTempoOFF.Value);
end;

procedure TFrPosPrinterTeste.seGavetaTempoONChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigGaveta.TempoON := Trunc(seGavetaTempoON.Value);
end;

procedure TFrPosPrinterTeste.seBarrasLarguraChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.LarguraLinha := Trunc(seBarrasLargura.Value);
end;

procedure TFrPosPrinterTeste.seEspLinhasChange(Sender: TObject);
begin
  ACBrPosPrinter1.EspacoEntreLinhas := Trunc(seEspLinhas.Value);
end;

procedure TFrPosPrinterTeste.btInfoUSBClick(Sender: TObject);
var
  i: Integer;
begin
  {$IfNDef MSWINDOWS}
  MessageDlg('DisponÌvel apenas em MS-Windows', mtError, [mbOK], 0);
  {$Else}
  PageControl1.ActiveTab := tsLog;
  mLog.Lines.Clear;
  mLog.Lines.Add('Procurando por Impressoras USB...');
  ACBrPosPrinter1.Device.WinUSB.FindUSBPrinters();
  mLog.Lines.Add('  Encontrado '+IntToStr(ACBrPosPrinter1.Device.WinUSB.DeviceList.Count)+' impressora(s)');

  for i := 0 to ACBrPosPrinter1.Device.WinUSB.DeviceList.Count-1 do
  begin
    with ACBrPosPrinter1.Device.WinUSB do
    begin
      mLog.Lines.Add('----------------------------------------------');
      mLog.Lines.Add('PrinterName: '+DeviceList.Items[i].DeviceName);
      mLog.Lines.Add('GUID: '+DeviceList.Items[i].GUID);
      mLog.Lines.Add('FrendlyName: '+DeviceList.Items[i].FrendlyName);
      mLog.Lines.Add('HardwareID: '+DeviceList.Items[i].HardwareID);
      mLog.Lines.Add('PrinterInterface: '+DeviceList.Items[i].DeviceInterface);
      mLog.Lines.Add('USBPort: '+DeviceList.Items[i].USBPort);
      mLog.Lines.Add('Class GUID: '+DeviceList.Items[i].ClassGUID);
    end;
  end;
  {$EndIf}
end;

procedure TFrPosPrinterTeste.btSearchPortsClick(Sender: TObject);
var
  K: Integer;
begin
  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );

  {$IfDef MSWINDOWS}
   ACBrPosPrinter1.Device.WinUSB.FindUSBPrinters();
   for K := 0 to ACBrPosPrinter1.Device.WinUSB.DeviceList.Count-1 do
     cbxPorta.Items.Add('USB:'+ACBrPosPrinter1.Device.WinUSB.DeviceList.Items[K].DeviceName);
  {$EndIf}

  For K := 0 to Printer.Count-1 do
    cbxPorta.Items.Add('RAW:'+Printer.Printers[K].Title);

  cbxPorta.Items.Add('LPT1') ;
  cbxPorta.Items.Add('\\localhost\Epson') ;
  cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;

  {$IfNDef MSWINDOWS}
   cbxPorta.Items.Add('/dev/ttyS0') ;
   cbxPorta.Items.Add('/dev/ttyS1') ;
   cbxPorta.Items.Add('/dev/ttyUSB0') ;
   cbxPorta.Items.Add('/dev/ttyUSB1') ;
   cbxPorta.Items.Add('/tmp/ecf.txt') ;
  {$EndIf}
end;

procedure TFrPosPrinterTeste.btSerialClick(Sender: TObject);
Var
  frConfiguraSerial : TfrConfiguraSerial ;
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    frConfiguraSerial.Device.Porta := ACBrPosPrinter1.Device.Porta ;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text ;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString ;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
       cbxPorta.Text                       := frConfiguraSerial.Device.Porta ;
       ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString ;
    end ;
  finally
     FreeAndNil( frConfiguraSerial ) ;
  end ;
end;

procedure TFrPosPrinterTeste.seColunasChange(Sender: TObject);
begin
  ACBrPosPrinter1.ColunasFonteNormal := Trunc(seColunas.Value);
end;

procedure TFrPosPrinterTeste.seLinhasBufferChange(Sender: TObject);
begin
  ACBrPosPrinter1.LinhasBuffer := Trunc(seLinhasBuffer.Value);
end;

procedure TFrPosPrinterTeste.seLinhasPularChange(Sender: TObject);
begin
  ACBrPosPrinter1.LinhasEntreCupons := Trunc(seLinhasPular.Value);
end;

procedure TFrPosPrinterTeste.seLogoFatorXChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.FatorX := Trunc(seLogoFatorX.Value);
end;

procedure TFrPosPrinterTeste.seLogoFatorYChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.FatorY := Trunc(seLogoFatorY.Value);
end;

procedure TFrPosPrinterTeste.seLogoKC1Change(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.KeyCode1 := Trunc(seLogoKC1.Value);
end;

procedure TFrPosPrinterTeste.seLogoKC2Change(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigLogo.KeyCode2 := Trunc(seLogoKC2.Value);
end;

procedure TFrPosPrinterTeste.seQRCodeErrorLevelChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigQRCode.ErrorLevel := Trunc(seQRCodeErrorLevel.Value);
end;

procedure TFrPosPrinterTeste.seQRCodeLarguraModuloChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigQRCode.LarguraModulo := Trunc(seQRCodeLarguraModulo.Value);
end;

procedure TFrPosPrinterTeste.seQRCodeTipoChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigQRCode.Tipo := Trunc(seQRCodeTipo.Value);
end;

procedure TFrPosPrinterTeste.LimparTexto; {Limpa o texto do mImp somente se o cbxLimparTexto.IsChecked  for true}
begin
  if cbxLimparTexto.IsChecked  then 
     mImp.Lines.Clear;
end;

procedure TFrPosPrinterTeste.GravarINI;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := ChangeFileExt( ParamStr(0),'.ini' ) ;

  INI := TIniFile.Create(ArqINI);
  try
     INI.WriteInteger('PosPrinter','Modelo',cbxModelo.ItemIndex);
     INI.WriteString('PosPrinter','Porta',cbxPorta.Text);
     INI.WriteString('PosPrinter','DeviceParams',ACBrPosPrinter1.Device.ParamsString);
     INI.WriteInteger('PosPrinter','Colunas', Trunc(seColunas.Value));
     INI.WriteInteger('PosPrinter','EspacoEntreLinhas',Trunc(seEspLinhas.Value));
     INI.WriteInteger('PosPrinter','LinhasBuffer',Trunc(seLinhasBuffer.Value));
     INI.WriteInteger('PosPrinter','LinhasPular',Trunc(seLinhasPular.Value));
     INI.WriteBool('PosPrinter','CortarPapel',cbCortarPapel.IsChecked);
     INI.WriteBool('PosPrinter','ControlePorta',cbControlePorta.IsChecked);
     INI.WriteBool('PosPrinter','TraduzirTags',cbTraduzirTags.IsChecked);
     INI.WriteBool('PosPrinter','IgnorarTags',cbIgnorarTags.IsChecked);
     INI.WriteString('PosPrinter','ArqLog',edLog.Text);
     INI.WriteInteger('PosPrinter','PaginaDeCodigo',cbxPagCodigo.ItemIndex);
     INI.WriteInteger('Barras','Largura',Trunc(seBarrasLargura.Value));
     INI.WriteInteger('Barras','Altura',Trunc(seBarrasAltura.Value));
     INI.WriteBool('Barras','HRI',cbHRI.IsChecked);
     INI.WriteInteger('QRCode','Tipo',Trunc(seQRCodeTipo.Value));
     INI.WriteInteger('QRCode','LarguraModulo',Trunc(seQRCodeLarguraModulo.Value));
     INI.WriteInteger('QRCode','ErrorLevel',Trunc(seQRCodeErrorLevel.Value));
     INI.WriteInteger('Logo','KC1',Trunc(seLogoKC1.Value));
     INI.WriteInteger('Logo','KC2',Trunc(seLogoKC2.Value));
     INI.WriteInteger('Logo','FatorX',Trunc(seLogoFatorX.Value));
     INI.WriteInteger('Logo','FatorY',Trunc(seLogoFatorY.Value));
     INI.WriteInteger('Gaveta','Numero',Trunc(seGavetaNum.Value));
     INI.WriteInteger('Gaveta','TempoOn',Trunc(seGavetaTempoON.Value));
     INI.WriteInteger('Gaveta','TempoOff',Trunc(seGavetaTempoOFF.Value));
     INI.WriteBool('Gaveta','SinalInvertido',cbGavetaSinalInvertido.IsChecked);
  finally
     INI.Free ;
  end ;
end;

procedure TFrPosPrinterTeste.LerINI;
Var
  ArqINI : String ;
  INI : TIniFile ;
begin
  ArqINI := ChangeFileExt( ParamStr(0),'.ini' ) ;

  INI := TIniFile.Create(ArqINI);
  try
     cbxModelo.ItemIndex := INI.ReadInteger('PosPrinter','Modelo', Integer(ACBrPosPrinter1.Modelo));
     cbxPorta.Text := INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta);
     cbxPortaChange(nil);
     ACBrPosPrinter1.Device.ParamsString := INI.ReadString('PosPrinter','DeviceParams',ACBrPosPrinter1.Device.ParamsString);
     seColunas.Value := INI.ReadInteger('PosPrinter','Colunas',ACBrPosPrinter1.ColunasFonteNormal);
     seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoEntreLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
     seLinhasBuffer.Value := INI.ReadInteger('PosPrinter','LinhasBuffer',ACBrPosPrinter1.LinhasBuffer);
     seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasPular',ACBrPosPrinter1.LinhasEntreCupons);
     cbCortarPapel.IsChecked  := INI.ReadBool('PosPrinter','CortarPapel',ACBrPosPrinter1.CortaPapel);
     cbControlePorta.IsChecked  := INI.ReadBool('PosPrinter','ControlePorta',ACBrPosPrinter1.ControlePorta);
     cbTraduzirTags.IsChecked  := INI.ReadBool('PosPrinter','TraduzirTags',ACBrPosPrinter1.TraduzirTags);
     cbIgnorarTags.IsChecked  := INI.ReadBool('PosPrinter','IgnorarTags',ACBrPosPrinter1.IgnorarTags);
     edLog.Text := INI.ReadString('PosPrinter','ArqLog',ACBrPosPrinter1.ArqLOG);
     cbxPagCodigo.ItemIndex := INI.ReadInteger('PosPrinter','PaginaDeCodigo',Integer(ACBrPosPrinter1.PaginaDeCodigo));
     seBarrasLargura.Value := INI.ReadInteger('Barras','Largura',ACBrPosPrinter1.ConfigBarras.LarguraLinha);
     seBarrasAltura.Value := INI.ReadInteger('Barras','Altura',ACBrPosPrinter1.ConfigBarras.Altura);
     cbHRI.IsChecked  := INI.ReadBool('Barras','HRI',ACBrPosPrinter1.ConfigBarras.MostrarCodigo);
     seQRCodeTipo.Value := INI.ReadInteger('QRCode','Tipo',ACBrPosPrinter1.ConfigQRCode.Tipo);
     seQRCodeLarguraModulo.Value := INI.ReadInteger('QRCode','LarguraModulo',ACBrPosPrinter1.ConfigQRCode.LarguraModulo);
     seQRCodeErrorLevel.Value := INI.ReadInteger('QRCode','ErrorLevel',ACBrPosPrinter1.ConfigQRCode.ErrorLevel);
     seLogoKC1.Value := INI.ReadInteger('Logo','KC1',ACBrPosPrinter1.ConfigLogo.KeyCode1);
     seLogoKC2.Value := INI.ReadInteger('Logo','KC2',ACBrPosPrinter1.ConfigLogo.KeyCode2);
     seLogoFatorX.Value := INI.ReadInteger('Logo','FatorX',ACBrPosPrinter1.ConfigLogo.FatorX);
     seLogoFatorY.Value := INI.ReadInteger('Logo','FatorY',ACBrPosPrinter1.ConfigLogo.FatorY);
     seGavetaNum.Value := INI.ReadInteger('Gaveta','Numero',1);
     seGavetaTempoON.Value := INI.ReadInteger('Gaveta','TempoOn',ACBrPosPrinter1.ConfigGaveta.TempoON);
     seGavetaTempoOFF.Value := INI.ReadInteger('Gaveta','TempoOff',ACBrPosPrinter1.ConfigGaveta.TempoOFF);
     cbGavetaSinalInvertido.IsChecked  := INI.ReadBool('Gaveta','SinalInvertido',ACBrPosPrinter1.ConfigGaveta.SinalInvertido);
  finally
     INI.Free ;
  end ;
end;

procedure TFrPosPrinterTeste.bImprimirClick(Sender: TObject);
begin
  ACBrPosPrinter1.Buffer.Text := mImp.Lines.Text;
  ACBrPosPrinter1.Imprimir;
end;

procedure TFrPosPrinterTeste.bImprimirImagemClick(Sender: TObject);
var
  MS: TMemoryStream;
begin
  if rbStream.IsChecked  then
  begin
    MS := TMemoryStream.Create;
    try
      Image1.Bitmap.SaveToStream(MS);
      MS.Position := 0;
      ACBrPosPrinter1.ImprimirImagemStream(MS);
    finally
      MS.Free ;
    end ;
  end
  else
    ACBrPosPrinter1.ImprimirImagemArquivo(edImagem.Text);
end;

procedure TFrPosPrinterTeste.bImprimirLogoClick(Sender: TObject);
begin
  ACBrPosPrinter1.ImprimirLogo( Trunc(seLogoKC1.Value),
                                Trunc(seLogoKC2.Value),
                                Trunc(seLogoFatorX.Value),
                                Trunc(seLogoFatorY.Value));
end;

procedure TFrPosPrinterTeste.bImpTagsValidasClick(Sender: TObject);
begin
  ACBrPosPrinter1.RetornarTags(mImp.Lines);
  ACBrPosPrinter1.ImprimirTags;
end;

procedure TFrPosPrinterTeste.bLerInfoClick(Sender: TObject);
begin
  mImp.Lines.Add( ACBrPosPrinter1.LerInfoImpressora );
end;

procedure TFrPosPrinterTeste.bLerStatusClick(Sender: TObject);
var
  Status: TACBrPosPrinterStatus;
  i: TACBrPosTipoStatus;
  AStr: String;
begin
  Status := ACBrPosPrinter1.LerStatusImpressora;

  if Status = [] then
    mImp.Lines.Add('Nennhum Erro encontrado')
  else
  begin
    AStr := '';
    For i := Low(TACBrPosTipoStatus) to High(TACBrPosTipoStatus) do
    begin
      if i in Status then
        AStr := AStr + GetEnumName(TypeInfo(TACBrPosTipoStatus), integer(i) )+ ', ';
    end;

    mImp.Lines.Add( AStr );
  end;
end;

procedure TFrPosPrinterTeste.bAtivarClick(Sender: TObject);
begin
  if not btSerial.Enabled then
  begin
     ACBrPosPrinter1.Desativar ;
     bAtivar.Text  := 'Ativar' ;
     btSerial.Enabled := True ;
  end
  else
  begin
    Self.BeginUpdate;
    try
      ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModelo.ItemIndex );
      ACBrPosPrinter1.Porta  := cbxPorta.Text;
      ACBrPosPrinter1.ArqLOG := edLog.Text;
      ACBrPosPrinter1.LinhasBuffer := Trunc(seLinhasBuffer.Value);
      ACBrPosPrinter1.LinhasEntreCupons := Trunc(seLinhasPular.Value);
      ACBrPosPrinter1.EspacoEntreLinhas := Trunc(seEspLinhas.Value);
      ACBrPosPrinter1.ColunasFonteNormal := Trunc(seColunas.Value);
      ACBrPosPrinter1.ControlePorta := cbControlePorta.IsChecked;
      ACBrPosPrinter1.CortaPapel := cbCortarPapel.IsChecked;
      ACBrPosPrinter1.TraduzirTags := cbTraduzirTags.IsChecked;
      ACBrPosPrinter1.IgnorarTags := cbIgnorarTags.IsChecked;
      ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo( cbxPagCodigo.ItemIndex );
      ACBrPosPrinter1.ConfigBarras.MostrarCodigo := cbHRI.IsChecked;
      ACBrPosPrinter1.ConfigBarras.LarguraLinha := Trunc(seBarrasLargura.Value);
      ACBrPosPrinter1.ConfigBarras.Altura := Trunc(seBarrasAltura.Value);
      ACBrPosPrinter1.ConfigQRCode.Tipo := Trunc(seQRCodeTipo.Value);
      ACBrPosPrinter1.ConfigQRCode.LarguraModulo := Trunc(seQRCodeLarguraModulo.Value);
      ACBrPosPrinter1.ConfigQRCode.ErrorLevel := Trunc(seQRCodeErrorLevel.Value);
      ACBrPosPrinter1.ConfigLogo.KeyCode1 := Trunc(seLogoKC1.Value);
      ACBrPosPrinter1.ConfigLogo.KeyCode2 := Trunc(seLogoKC2.Value);
      ACBrPosPrinter1.ConfigLogo.FatorX := Trunc(seLogoFatorX.Value);
      ACBrPosPrinter1.ConfigLogo.FatorY := Trunc(seLogoFatorY.Value);

      ACBrPosPrinter1.Ativar ;

      btSerial.Enabled := False ;
      bAtivar.Text  := 'Desativar' ;

      GravarINI ;
    finally
      EndUpdate;
      cbxModelo.ItemIndex := Integer(ACBrPosPrinter1.Modelo) ;
      cbxPorta.Text       := ACBrPosPrinter1.Porta ;
    end ;
  end;
end;

procedure TFrPosPrinterTeste.bCaregarImagemClick(Sender: TObject);
begin
  OpenPictureDialog1.Filter := 'BMP MonoCrom·tico|*.bmp';

  if OpenPictureDialog1.Execute then
  begin
    try
      Image1.Bitmap.LoadFromFile(OpenPictureDialog1.FileName);
      edImagem.Text := OpenPictureDialog1.FileName;
    except
      Image1.Bitmap := nil;
    end;
  end;
end;

procedure TFrPosPrinterTeste.bConverterClick(Sender: TObject);
var
  ARasterStr: AnsiString;
  AWidth, AHeight: Integer;
  MS: TMemoryStream;
begin
  BitmapToRasterStr(Image1.Bitmap, True, AWidth, AHeight, ARasterStr);
  MS := TMemoryStream.Create;
  try
    RasterStrToBMPMono(ARasterStr, AWidth, True, MS);
    MS.Position := 0;
    Image1.Bitmap.LoadFromStream(MS);
    ACBrPosPrinter1.ImprimirImagemStream(MS);
  finally
    MS.Free;
  end; 
end;

procedure TFrPosPrinterTeste.bGravarLogoClick(Sender: TObject);
var
  MS: TMemoryStream;
begin
  if rbStream.IsChecked  then
  begin
    MS := TMemoryStream.Create;
    try
      Image1.Bitmap.SaveToStream(MS);
      MS.Position := 0;
      ACBrPosPrinter1.GravarLogoStream(MS, Trunc(seLogoKC1.Value), Trunc(seLogoKC2.Value));
    finally
      MS.Free ;
    end ;
  end
  else
    ACBrPosPrinter1.GravarLogoArquivo(edImagem.Text, Trunc(seLogoKC1.Value), Trunc(seLogoKC2.Value));
end;

procedure TFrPosPrinterTeste.ACBrPosPrinter1GravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  mLog.Lines.Add(ALogLine);
  Tratado := False;
end;

procedure TFrPosPrinterTeste.bApagarLogoClick(Sender: TObject);
begin
  ACBrPosPrinter1.ApagarLogo(Trunc(seLogoKC1.Value), Trunc(seLogoKC2.Value));
end;

procedure TFrPosPrinterTeste.bTagBMPClick(Sender: TObject);
var
  SL: TStringList;
  MS: TMemoryStream;
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
  mResp: TModalResult;
  SS: TStringStream;
begin
  mImp.Lines.Add('</zera></ce>');

  if rbStream.IsChecked  then
  begin
    mResp := MessageDlg('SIM - Em ASCII Art'+sLineBreak+
                        'N√O - Em Base64', TMsgDlgType.mtConfirmation, mbYesNoCancel, 0);

    if (mResp = mrYes) then
    begin
      SL := TStringList.Create;
      MS := TMemoryStream.Create;
      try
        Image1.Bitmap.SaveToStream(MS);
        MS.Position := 0;
        BMPMonoToRasterStr(MS, True, AWidth, AHeight, ARasterStr);
        RasterStrToAscII(ARasterStr, AWidth, False, SL);
        mImp.Lines.Add('<bmp>');
        mImp.Lines.AddStrings(SL);
        mImp.Lines.Add('</bmp>');
      finally
        MS.Free;
        SL.Free;
      end;
    end;

    if (mResp = mrNo) then
    begin
      SS := TStringStream.Create('');
      try
        Image1.Bitmap.SaveToStream(SS);
        mImp.Lines.Add('<bmp>');
        mImp.Lines.Add(EncodeBase64(SS.DataString));
        mImp.Lines.Add('</bmp>');
      finally
        SS.Free;
      end;
    end;

  end
  else
  begin
    if (edImagem.Text = '') then
      raise Exception.Create('Nome de Arquivo de Imagem n„o especificado');

    mImp.Lines.Add('<bmp>'+edImagem.Text+'</bmp>');
  end;

  PageControl1.First;
end;

end.

