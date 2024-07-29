{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}
unit ACBrMonitorConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, ACBrMonitorConsts, Graphics, ACBrUtil.FilesIO;

type

  TACBrMonitor = record
    Modo_TCP          : Boolean;
    Modo_TXT          : Boolean;
    MonitoraPasta     : Boolean;
    TCP_Porta         : Integer;
    TCP_TimeOut       : Integer;
    Converte_TCP_Ansi : Boolean;
    TXT_Entrada       : String;
    TXT_Saida         : String;
    Converte_TXT_Entrada_Ansi: Boolean;
    Converte_TXT_Saida_Ansi  : Boolean;
    Intervalo         : Integer;
    Gravar_Log        : Boolean;
    Arquivo_Log       : String;
    Linhas_Log        : Integer;
    Comandos_Remotos  : Boolean;
    Uma_Instancia     : Boolean;
    MostraAbas        : Boolean;
    MostrarNaBarraDeTarefas : Boolean;
    RetirarAcentosNaResposta : Boolean;
    MostraLogEmRespostasEnviadas : Boolean;
    HashSenha         : String;
    Senha             : String;
    VersaoSSL         : Integer;
    TipoResposta      : Integer;
  end;

  TECF = record
    Modelo            : Integer;
    Porta             : String;
    SerialParams      : String;
    Timeout           : Integer;
    IntervaloAposComando : Integer;
    MaxLinhasBuffer   : Integer;
    PaginaCodigo      : Integer;
    LinhasEntreCupons : Integer;
    ArredondamentoPorQtd : Boolean;
    ArredondamentoItemMFD: Boolean;
    DescricaoGrande   : Boolean;
    GavetaSinalInvertido : Boolean;
    IgnorarTagsFormatacao: Boolean;
    ControlePorta     : Boolean;
    ArqLog            : String;
  end;

  TCHQ = record
    Modelo            : Integer;
    Porta             : String;
    SerialParams      : String;
    VerificaFormulario: Boolean;
    Favorecido        : String;
    Cidade            : String;
    PathBemafiINI     : String;
  end;

  TGAV = record
    Modelo            : Integer;
    Porta             : String;
    StringAbertura    : String;
    AberturaIntervalo : Integer;
    AcaoAberturaAntecipada: Integer;
  end;

  TDIS = record
    Modelo            : Integer;
    Porta             : String;
    Intervalo         : Integer;
    Passos            : Integer;
    IntervaloEnvioBytes : Integer;
  end;

  TLCB = record
    Porta             : String;
    Intervalo         : Integer;
    SufixoLeitor      : String;
    ExcluirSufixo     : Boolean;
    PrefixoAExcluir   : String;
    SufixoIncluir     : String;
    Dispositivo       : String;
    Teclado           : Boolean;
    Device            : String;
  end;

  TRFD = record
    GerarRFD          : Boolean;
    DirRFD            : String;
    IgnoraECF_MFD     : Boolean;
  end;

  TBAL = record
    Modelo            : Integer;
    Porta             : String;
    Intervalo         : Integer;
    ArqLog            : String;
    Device            : String;
  end;

  TETQ = record
    Modelo            : Integer;
    Porta             : String;
    DPI               : Integer;
    LimparMemoria     : Boolean;
    Temperatura       : Integer;
    Velocidade        : Integer;
    BackFeed          : Integer;
    MargemEsquerda    : Integer;
    Origem            : Integer;
    Unidade           : Integer;
    Copias            : Integer;
    Avanco            : Integer;
  end;

  TCEP = record
    WebService        : Integer;
    Chave_BuscarCEP   : String;
    Proxy_Host        : String;
    Proxy_Port        : String;
    Proxy_User        : String;
    Proxy_Pass        : String;
    IBGEAcentos       : Boolean;
  end;

  TConsultaCNPJ = record
    Provedor          : integer;
    Usuario           : string;
    Senha             : string;
    Proxy_Host        : String;
    Proxy_Port        : String;
    Proxy_User        : String;
    Proxy_Pass        : String;
  end;

  TTC = record
    Modelo            : Integer;
    TCP_Porta         : Integer;
    Arq_Precos        : String;
    Nao_Econtrado     : String;

  end;

  TSEDEX = record
    Contrato          : String;
    SenhaSedex        : String;
  end;

  TNCM = record
    DirNCMSalvar      : String;
    DiasValidadeCache : Integer;
  end;

  TEmail = record
    NomeExibicao      : String;
    Endereco          : String;
    Email             : String;
    Usuario           : String;
    Senha             : String;
    Porta             : Integer;
    ExigeSSL          : Boolean;
    ExigeTLS          : Boolean;
    Confirmacao       : Boolean;
    SegundoPlano      : Boolean;
    Codificacao       : String;
    HTML              : Boolean;
    AttemptsMail      : Integer;
    TimeOutMail       : Integer;
    SSLType           : Integer;

  end;

  TCertificado = record
    SSLLib            : Integer;
    CryptLib          : Integer;
    HttpLib           : Integer;
    XmlSignLib        : Integer;
    SSLType           : Integer;
    ArquivoPFX        : String;
    URLPFX            : String;
    NumeroSerie       : String;
    Senha             : String;
    ExibeRazaoSocialCertificado: Boolean;
    VerificarValidade : Boolean;
  end;

  TProxy = record
    Host              : String;
    Porta             : String;
    User              : String;
    Pass              : String;
  end;

  TNFCeWebService = record
    IdToken           : String;
    Token             : String;
    TagQrCode         : Boolean;
    UsarIntegrador    : Boolean;
  end;

  TDANFCe = record
    MargemInf         : Double;
    MargemSup         : Double;
    MargemDir         : Double;
    MargemEsq         : Double;
    LarguraBobina     : Double;
    ImprimeNNFFormatadoNFCe : Boolean;
  end;

  TDANFCeTipoPagto = record
    tipo              : Boolean;
    Bandeira          : Boolean;
    Autorizacao       : Boolean;
  end;

  TNFCeImpressao = record
    Modelo            : Integer;
    ModoImpressaoEvento: Integer;
    ImprimirItem1Linha: Boolean;
    ImprimirDescAcresItem: Boolean;
    ImpressoraPadrao  : String;
    UsaCodigoEanImpressao: Boolean;
    ImprimeNomeFantasia: Boolean;
    QRCodeLateral     : Boolean;
    DANFCe            : TDANFCe;
    DANFCeTipoPagto   : TDANFCeTipoPagto;
    ImprimeTributos   : Integer;
    ExibeTotalTributosItem : Boolean;
    LogoLateral       : Boolean;
    ImprimeItens      : Boolean;
  end;

  TNFCe = record
    WebService        : TNFCeWebService;
    Emissao           : TNFCeImpressao;
  end;

  TNFe = record
    CNPJContador      : String;
  end;

  TWebService = record
    Versao            : String;
    VersaoMDFe        : String;
    VersaoCTe         : String;
    VersaoBPe         : String;
    VersaoeSocial     : String;
    VersaoReinf       : String;
    VersaoGNRe        : String;
    VersaoQRCode      : String;
    CamposFatObrig    : Boolean;
    FormaEmissaoNFe   : Integer;
    FormaEmissaoCTe   : Integer;
    FormaEmissaoMDFe  : Integer;
    FormaEmissaoBPe   : Integer;
    FormaEmissaoGNRe  : Integer;
    Ambiente          : Integer;
    UF                : String;
    AjustarAut        : Boolean;
    Aguardar          : String;
    Tentativas        : String;
    Intervalo         : String;
    TimeZoneMode      : Integer;
    TimeZoneStr       : String;
    TagRejeicao938    : Integer;
    Proxy             : TProxy;
    NFCe              : TNFCeWebService;
    NFe               : TNFe;
  end;

  TDFeGeral = record
    DANFE              : Integer;
    FormaEmissao       : Integer;
    Logomarca          : String;
    LogoMarcaNFCeSAT   : String;
    LogoMarcaPrefeitura: String;
    Salvar             : Boolean;
    PathSalvar         : String;
    Impressora         : String;
  end;

  TDANFE = record
    Modelo                           : Integer;
    TamanhoPapel                     : Integer;
    Site                             : String;
    Email                            : String;
    Fax                              : String;
    ImpDescPorc                      : Boolean;
    MostrarPreview                   : Boolean;
    Copias                           : Integer;
    CopiasNFCe                       : Integer;
    LarguraCodigoProduto             : Integer;
    EspacoEntreProdutos              : Integer;
    FonteRazao                       : Integer;
    FonteEndereco                    : Integer;
    FonteCampos                      : Integer;
    FonteAdicionais                  : Integer;
    AlturaCampos                     : Integer;
    Margem                           : Double;
    MargemSup                        : Double;
    MargemDir                        : Double;
    MargemEsq                        : Double;
    PathPDF                          : String;
    DecimaisQTD                      : Integer;
    DecimaisValor                    : Integer;
    ExibeResumo                      : Boolean;
    TextoResumoCanhoto               : String;
    ImprimirTributosItem             : Boolean;
    ImprimirValLiq                   : Boolean;
    UNComercialETributavel           : Integer;
    PreImpresso                      : Boolean;
    MostrarStatus                    : Boolean;
    ExibirEAN                        : Boolean;
    ExibirCampoFatura                : Boolean;
    ExpandirLogo                     : Boolean;
    Fonte                            : Integer;
    LocalCanhoto                     : Integer;
    LayoutCanhoto                    : Integer;
    QuebrarLinhasDetalheItens        : Boolean;
    ImprimirDetalhamentoEspecifico   : Boolean;
    ImprimirDadosDocReferenciados    : Boolean;
    ExibirBandInforAdicProduto       : Integer;
    ImprimeDescAcrescItemNFe         : Integer;
    LogoEmCima                       : Boolean;
    ImprimeInscSuframa               : Boolean;
    ExpandirDadosAdicionaisAuto      : Boolean;
    ImprimeContinuacaoDadosAdicionaisPrimeiraPagina: Boolean;
    ImprimirCampoFormaPagamento      : Integer;
    ImprimeXPedNitemPed              : boolean;
    ImprimeNNFFormatadoNFe           : boolean;
  end;

  TDACTE = record
    TamanhoPapel                     : Integer;
  end;

  TDAMFE = record
    ExibirMunicipioDescarregamento   : Boolean;
  end;

  TDFeDiretorios = record
    Salvar                       : Boolean;
    PastaMensal                  : Boolean;
    AddLiteral                   : Boolean;
    EmissaoPathNFe               : Boolean;
    SalvarCCeCanPathEvento       : Boolean;
    SepararPorCNPJ               : Boolean;
    SepararPorModelo             : Boolean;
    SalvarApenasNFesAutorizadas  : Boolean;
    AtualizarXMLCancelado        : Boolean;
    NormatizarMunicipios         : Boolean;
    UsarSeparadorPathPDF         : Boolean;
    SepararPorNome               : Boolean;
    PathNFe                      : String;
    PathInu                      : String;
    PathDPEC                     : String;
    PathEvento                   : String;
    PathArqTXT                   : String;
    PathDownload                 : String;
    PathSchemasDFe               : String;

  end;

  TDFeEmail = record
    MensagemCTe       : String;
    MensagemNFe       : String;
    MensagemMDFe      : String;
    MensagemBPe       : String;
    MensagemNFSe      : String;
    AssuntoCTe        : String;
    AssuntoNFe        : String;
    AssuntoMDFe       : String;
    AssuntoBPe        : String;
    AssuntoNFSe       : String;
  end;

  TDFeRespTecnico = record
    CSRT        : String;
    idCSRT      : String;
  end;

  TDFeImpressao = record
    Geral       : TDFeGeral;
    DANFE       : TDANFE;
    NFCe        : TNFCe;
    DACTE       : TDACTE;
    DAMFE       : TDAMFe;
  end;

  TeSocial = record
    IdEmpregador   : String;
    IdTransmissor  : String;
    TipoEmpregador : String;
  end;

  TReinf = record
    IdContribuinte   : String;
    IdTransmissor    : String;
    TipoContribuinte : String;
  end;

  TDFe = record
    IgnorarComandoModoEmissao: Boolean;
    ModoXML           : Boolean;
    RetirarAcentos    : Boolean;
    RetirarEspacos    : Boolean;
    Gravar_Log_Comp   : Boolean;
    Arquivo_Log_Comp  : String;
    Linhas_Log_Comp   : Integer;
    ArquivoWebServices: String;
    ArquivoWebServicesCTe: String;
    ArquivoWebServicesMDFe: String;
    ArquivoWebServicesGNRe: String;
    ArquivoWebServiceseSocial: String;
    ArquivoWebServicesReinf: String;
    ArquivoWebServicesBPe: String;
    ArquivoWebServicesNFSe: String;
    ValidarDigest      : Boolean;
    TimeoutWebService  : Integer;
    Certificado        : TCertificado;
    WebService         : TWebService;
    Email              : TDFeEmail;
    Impressao          : TDFeImpressao;
    Diretorios         : TDFeDiretorios;
    ESocial            : TeSocial;
    Reinf              : TReinf;
    RespTecnico        : TDFeRespTecnico;
  end;

  TSATExtrato = record
    MostrarStatus               : Boolean;
    ParamsString                : String;
    ImprimeDescAcrescItem       : Boolean;
    ImprimeEmUmaLinha           : Boolean;
    ImprimeChaveEmUmaLinha      : Integer;
    UsaCodigoEanImpressao       : Boolean;
    ImprimeQRCodeLateral        : Boolean;
    ImprimeLogoLateral          : Boolean;
    ExtratoDecimaisQTD          : Integer;
    ExtratoDecimaisValor        : Integer;
    ExtratoMaskQTD              : String;
    ExtratoMaskValor            : String;
    FormatoDecimal              : Integer;
  end;

  TSATEmit = record
    CNPJ                        : String;
    IE                          : String;
    IM                          : String;
    RegTributario               : Integer;
    RegTribISSQN                : Integer;
    IndRatISSQN                 : Integer;
  end;

  TSATFortes = record
    UsarFortes                  : Boolean;
    Largura                     : Integer;
    MargemTopo                  : Integer;
    MargemFundo                 : Integer;
    MargemEsquerda              : Integer;
    MargemDireita               : Integer;
    Preview                     : Boolean;
  end;

  TSATRede = record
    tipoInter                  : Integer;
    tipoLan                    : Integer;
    SSID                       : String ;
    seg                        : Integer;
    codigo                     : String ;
    lanIP                      : String ;
    lanMask                    : String ;
    lanGW                      : String ;
    lanDNS1                    : String ;
    lanDNS2                    : String ;
    usuario                    : String ;
    senha                      : String ;
    proxy                      : Integer;
    proxy_ip                   : String ;
    proxy_porta                : Integer;
    proxy_user                 : String ;
    proxy_senha                : String ;
  end;

  TSATPrinter = record
    Name                       : String;
  end;

  TSATSwH = record
    CNPJ                       : String;
    Assinatura                 : String;
  end;

  TSATImpressao = record
    SATEmit                    : TSATEmit;
    SATExtrato                 : TSATExtrato;
    SATFortes                  : TSATFortes;
    SATPrinter                 : TSATPrinter;
  end;

  TSATEmail = record
    MensagemSAT                : String;
    AssuntoSAT                 : String;
  end;

  TSAT = record
    Modelo                       : Integer;
    Marca                        : String;
    ArqLog                       : String;
    NomeDLL                      : String;
    CodigoAtivacao               : String;
    CodigoUF                     : String;
    NumeroCaixa                  : Integer;
    Ambiente                     : Integer;
    PaginaDeCodigo               : Integer;
    versaoDadosEnt               : Double;
    FormatarXML                  : Boolean;
    PathCFe                      : String;
    SalvarCFe                    : Boolean;
    SalvarCFeCanc                : Boolean;
    SalvarEnvio                  : Boolean;
    SepararPorCNPJ               : Boolean;
    SepararPorMES                : Boolean;
    SepararPorANO                : Boolean;
    SepararPorDIA                : Boolean;
    SepararPorModelo             : Boolean;
    ValidarNumeroSessaoResposta  : Boolean;
    SATImpressao                 : TSATImpressao;
    SATRede                      : TSATRede;
    SATSWH                       : TSATSwH;
    SATEmail                     : TSATEmail;
    PathCFeCanc                  : String;
    PathCFeEnvio                 : String;
    PrefixoArqCFe                : String;
    PrefixoArqCFeCanc            : String;

  end;

  TIntegradorFiscal = record
    Input                      : String;
    Output                     : String;
    Timeout                    : Integer;
  end;

  TBarras = record
    Largura                    : Integer;
    Altura                     : Integer;
    HRI                        : Boolean;
  end;

  TQRCode = record
    Tipo                       : Integer;
    LarguraModulo              : Integer;
    ErrorLevel                 : Integer;
  end;

  TLogo = record
    Imprimir                   : Boolean;
    KC1                        : Integer;
    KC2                        : Integer;
    FatorX                     : Integer;
    FatorY                     : Integer;
  end;

  TGaveta = record
    TempoON                    : Integer;
    TempoOFF                   : Integer;
    SinalInvertido             : Boolean;
  end;

  TPosPrinter = record
    Modelo                     : Integer;
    Porta                      : String;
    Colunas                    : Integer;
    EspacoEntreLinhas          : Integer;
    LinhasBuffer               : Integer;
    LinhasPular                : Integer;
    PaginaDeCodigo             : Integer;
    ControlePorta              : Boolean;
    CortarPapel                : Boolean;
    TraduzirTags               : Boolean;
    IgnorarTags                : Boolean;
    ArqLog                     : String;
    SerialParams               : String;
    CodigoBarras               : TBarras;
    QRCode                     : TQRCode;
    Logo                       : TLogo;
    Gaveta                     : TGaveta;
  end;

  TBoletoConta = record
    RespEmis                   : Integer;
    Pessoa                     : Integer;
    Modalidade                 : String ;
    Convenio                   : String ;
    Banco                      : Integer;
    Conta                      : String ;
    DigitoConta                : String ;
    Agencia                    : String ;
    DigitoAgencia              : String ;
    DigitoAgenciaConta         : String ;
    CodCedente                 : String ;
    LocalPagamento             : String ;
    CodigoOperacao             : String ;
  end;

  TBoletoLayout = record
    DirLogos                   : String ;
    Copias                     : Integer;
    Preview                    : Boolean;
    Progresso                  : Boolean;
    Setup                      : Boolean;
    AlteraEscala               : Boolean;
    Escala                     : Integer;
    Layout                     : Integer;
    Filtro                     : Integer;
    DirArquivoBoleto           : String ;
    Impressora                 : String ;
    NomeArquivoBoleto          : String;
    TipoMotorRelatorio         : integer;
    MargemInferior             : double;
    MargemSuperior             : double;
    MargemEsquerda             : double;
    MargemDireita              : double;
  end;

  TBoletoRemessaRetorno = record
    DirArquivoRemessa          : String ;
    DirArquivoRetorno          : String ;
    CNAB                       : Integer;
    LerCedenteRetorno          : Boolean;
    CodTransmissao             : String ;
    RemoveAcentos              : Boolean;
    PrefixArqRemessa           : String;
    VersaoArquivo              : String;
    VersaoLote                 : String;
  end;

  TBoletoRelatorio = record
    MostraPreviewRelRetorno    : Boolean;
    LogoEmpresa                : String ;
  end;

  TBoletoEmail = record
    EmailAssuntoBoleto         : String ;
    EmailMensagemBoleto        : String ;
    EmailFormatoHTML           : Boolean;
  end;

  TBoletoPIX = record
    TipoChavePix               : integer;
    ChavePix                   : String ;
  end;


  TBoletoCedenteWS = record
    ClientID                   : String;
    ClientSecret               : String;
    KeyUser                    : String;
    Scope                      : String;
    IndicadorPix               : Boolean;
  end;

  TBoletoSSL = record
    Ambiente                   : Integer;
    Operacao                   : Integer;
    VersaoDF                   : String;
    Proxy_Host                 : String;
    Proxy_Port                 : String;
    Proxy_User                 : String;
    Proxy_Pass                 : String;
    CryptLib                   : Integer;
    HttpLib                    : Integer;
    XmlSignLib                 : Integer;
    SSLType                    : Integer;
    TimeOut                    : Integer;
    CertificadoHTTP            : Boolean;
    ArquivoCRT                 : String;
    ArquivoKEY                 : String;
  end;

  TBoletoConfig = record
    LogNivel                   : TNivelLog;
    PathGravarRegistro         : String;
    NomeArquivoLog             : String;
    SSL                        : TBoletoSSL;
  end;

  TBoletoWS = record
    CedenteWS                  : TBoletoCedenteWS;
    Config                     : TBoletoConfig;
  end;

  TBOLETO = record
    Nome                       : String ;
    CNPJCPF                    : String ;
    Logradouro                 : String ;
    Numero                     : String ;
    Bairro                     : String ;
    CodCidade                  : Integer;
    Cidade                     : String ;
    CEP                        : String ;
    Complemento                : String ;
    UF                         : String ;
    Conta                      : TBoletoConta;
    Layout                     : TBoletoLayout;
    RemessaRetorno             : TBoletoRemessaRetorno;
    Relatorio                  : TBoletoRelatorio;
    Email                      : TBoletoEmail;
    WS                         : TBoletoWS;
    PIX                        : TBoletoPIX;
  end;

  TNFSe = record
    LayoutProvedor: Integer;
    CodigoMunicipio: Integer;
    NomeMunicipio: string;
    UFMunicipio: string;
    Usuario: string;
    Senha: string;
    ChaveAcesso: string;
    ChaveAutenticacao: string;
    FraseSecreta: string;
    CNPJEmitente: string;
    IMEmitente: string;
    NomeEmitente: string;
    MontarAutoPathSchema: Boolean;
    ConsultarLoteAposEnvio: Boolean;
    ConsultarAposCancelar: Boolean;
    NomePrefeitura: string;
    CNPJPrefeitura: string;
    NomeLongoNFSe : boolean;
  end;




  EDFeException = class(Exception);
  EDFeConfigException = class(EDFeException);

  TMonitorConfig = class;
  TACBrOnGravarConfig = procedure(AMonitorConfig: TMonitorConfig) of object;

  { TMonitorConfig }

  TMonitorConfig = class
  private
    FNomeArquivo: String;
    FACBrMonitor: TACBrMonitor;
    FECF: TECF;
    FCHQ : TCHQ;
    FGAV : TGAV;
    FDIS: TDIS;
    FLCB: TLCB;
    FRFD: TRFD;
    FBAL: TBAL;
    FETQ: TETQ;
    FCEP: TCEP;
    FTC : TTC;
    FSEDEX: TSEDEX;
    FNCM: TNCM;
    FEmail: TEmail;
    FDFe : TDFe;
    FSAT : TSAT;
    FIntegradorFiscal : TIntegradorFiscal;
    FPosPrinter : TPosPrinter;
    FBoleto : TBOLETO;
    FFonteLinha: TFont;
    FNFSE: TNFSe;
    FConsultaCNPJ :TConsultaCNPJ;

    FOnGravarConfig: TACBrOnGravarConfig;
    procedure DefinirValoresPadrao;
    procedure ValidarNomeCaminho(Salvar: Boolean);

    procedure DoOnGravarConfig;
  public
    constructor Create(ANomeArquivo: String);
    destructor Destroy; override;

    procedure SalvarArquivoMemoria(AStream: TStream);
    procedure SalvarArquivo;

    procedure CarregarArquivoMemoria(AStream: TStream);
    procedure CarregarArquivo;

    procedure CriarArquivo;

    property NomeArquivo: String             read FNomeArquivo;
    property ACBrMonitor : TACBrMonitor      read FACBrMonitor;
    property ECF : TECF                      read FECF;
    property CHQ : TCHQ                      read FCHQ;
    property GAV : TGAV                      read FGAV;
    property DIS : TDIS                      read FDIS;
    property LCB : TLCB                      read FLCB;
    property RFD : TRFD                      read FRFD;
    property BAL : TBAL                      read FBAL;
    property ETQ : TETQ                      read FETQ;
    property CEP : TCEP                      read FCEP;
    property TC  : TTC                       read FTC ;
    property SEDEX : TSEDEX                  read FSEDEX;
    property NCM : TNCM                      read FNCM;
    property Email : TEmail                  read FEmail;
    property DFE: TDFe                       read FDFE;
    property SAT : TSAT                      read FSAT;
    property IntegradorFiscal : TIntegradorFiscal  read FIntegradorFiscal;
    property PosPrinter : TPosPrinter        read FPosPrinter;
    property BOLETO : TBOLETO                read FBoleto;
    property FonteLinha: TFont               read FFonteLinha;
    property NFSE: TNFSe                     read FNFSE;
    property ConsultaCNPJ :TConsultaCNPJ     read FConsultaCNPJ;

    property OnGravarConfig: TACBrOnGravarConfig read FOnGravarConfig write FOnGravarConfig;
  end;


implementation

uses
  UtilUnit;

{ TMonitorConfig }

constructor TMonitorConfig.Create(ANomeArquivo: String);
begin
  FNomeArquivo:= ANomeArquivo;
  FOnGravarConfig := Nil;
  FFonteLinha := TFont.Create;
end;

destructor TMonitorConfig.Destroy;
begin
  FFonteLinha.Free;
  inherited;
end;


procedure TMonitorConfig.SalvarArquivoMemoria(AStream: TStream);
var
  Ini: TMemIniFile;
  SL: TStringList;
begin
  Ini := TMemIniFile.Create('');
  try
    with ACBrMonitor do
    begin
      Ini.WriteBool( CSecACBrMonitor, CKeyModo_TCP, Modo_TCP );
      Ini.WriteBool( CSecACBrMonitor, CKeyModo_TXT, Modo_TXT) ;
      Ini.WriteBool( CSecACBrMonitor, CKeyMonitorarPasta, MonitoraPasta );
      Ini.WriteInteger( CSecACBrMonitor, CKeyTCP_Porta, TCP_Porta );
      Ini.WriteInteger( CSecACBrMonitor, CKeyTCP_TimeOut, TCP_TimeOut );
      Ini.WriteBool( CSecACBrMonitor, CKeyConverte_TCP_Ansi, Converte_TCP_Ansi );
      Ini.WriteString( CSecACBrMonitor, CKeyTXT_Entrada, TXT_Entrada );
      Ini.WriteString( CSecACBrMonitor, CKeyTXT_Saida, TXT_Saida );
      Ini.WriteBool( CSecACBrMonitor, CKeyConverte_TXT_Entrada_Ansi, Converte_TXT_Entrada_Ansi );
      Ini.WriteBool( CSecACBrMonitor, CKeyConverte_TXT_Saida_Ansi, Converte_TXT_Saida_Ansi );
      Ini.WriteInteger( CSecACBrMonitor, CKeyIntervalo, Intervalo );
      Ini.WriteBool( CSecACBrMonitor, CKeyGravar_Log, Gravar_Log );
      Ini.WriteString( CSecACBrMonitor, CKeyArquivo_Log, Arquivo_Log );
      Ini.WriteInteger( CSecACBrMonitor, CKeyLinhas_Log, Linhas_Log );
      Ini.WriteBool( CSecACBrMonitor, CKeyComandos_Remotos, Comandos_Remotos );
      Ini.WriteBool( CSecACBrMonitor, CKeyUma_Instancia, Uma_Instancia );
      Ini.WriteBool( CSecACBrMonitor, CKeyMostraAbas, MostraAbas );
      Ini.WriteBool( CSecACBrMonitor, CKeyMostrarNaBarraDeTarefas, MostrarNaBarraDeTarefas );
      Ini.WriteBool( CSecACBrMonitor, CKeyRetirarAcentosNaResposta, RetirarAcentosNaResposta );
      Ini.WriteBool( CSecACBrMonitor, CKeyMostraLogEmRespostasEnviadas, MostraLogEmRespostasEnviadas );
      GravaINICrypt(Ini, CSecACBrMonitor, CKeyHashSenha, HashSenha, _C);
      Ini.WriteInteger(CSecACBrMonitor, CKeyTipoResposta, TipoResposta);
    end;

    with ECF do
    begin
      Ini.WriteInteger( CSecECF, CKeymodelo, Modelo );
      Ini.WriteString( CSecECF, CKeyPorta, Porta );
      Ini.WriteString( CSecECF, CKeySerialParams, SerialParams );
      Ini.WriteInteger( CSecECF, CKeyTimeout, Timeout );
      Ini.WriteInteger( CSecECF, CKeyIntervaloAposComando, IntervaloAposComando );
      Ini.WriteInteger( CSecECF, CKeyMaxLinhasBuffer, MaxLinhasBuffer );
      Ini.WriteInteger( CSecECF, CKeyPaginaCodigo, PaginaCodigo );
      Ini.WriteInteger( CSecECF, CKeyLinhasEntreCupons, LinhasEntreCupons );
      Ini.WriteBool( CSecECF, CKeyArredondamentoPorQtd, ArredondamentoPorQtd );
      Ini.WriteBool( CSecECF, CKeyArredondamentoItemMFD, ArredondamentoItemMFD );
      Ini.WriteBool( CSecECF, CKeyDescricaoGrande, DescricaoGrande );
      Ini.WriteBool( CSecECF, CKeyGavetaSinalInvertido, GavetaSinalInvertido );
      Ini.WriteBool( CSecECF, CKeyIgnorarTagsFormatacao, IgnorarTagsFormatacao );
      Ini.WriteBool( CSecECF, CKeyControlePorta, ControlePorta );
      Ini.WriteString( CSecECF, CKeyArqLog, ArqLog );
    end;

    with CHQ do
    begin
      Ini.WriteInteger( CSecCHQ, CKeyCHQModelo, modelo );
      Ini.WriteString( CSecCHQ, CKeyCHQPorta, Porta );
      Ini.WriteString( CSecCHQ, CKeyCHQSerialParams, SerialParams );
      Ini.WriteBool( CSecCHQ, CKeyCHQVerificaFormulario, VerificaFormulario );
      Ini.WriteString( CSecCHQ, CKeyCHQFavorecido, Favorecido );
      Ini.WriteString( CSecCHQ, CKeyCHQCidade, Cidade );
      Ini.WriteString( CSecCHQ, CKeyCHQPathBemafiINI, PathBemafiINI );
    end;

    with GAV do
    begin
      Ini.WriteInteger( CSecGAV, CKeyGAVModelo, Modelo );
      Ini.WriteString( CSecGAV, CKeyGAVPorta, Porta );
      Ini.WriteString( CSecGAV, CKeyGAVStringAbertura, StringAbertura );
      Ini.WriteInteger( CSecGAV, CKeyGAVAberturaIntervalo, AberturaIntervalo );
      Ini.WriteInteger( CSecGAV, CKeyGAVAcaoAberturaAntecipada, AcaoAberturaAntecipada );
    end;

    with DIS do
    begin
      Ini.WriteInteger( CSecDIS, CKeyDISModelo, Modelo );
      Ini.WriteString( CSecDIS, CKeyDISPorta, Porta );
      Ini.WriteInteger( CSecDIS, CKeyDISIntervalo, Intervalo );
      Ini.WriteInteger( CSecDIS, CKeyDISPassos, Passos );
      Ini.WriteInteger( CSecDIS, CKeyDISIntervaloEnvioBytes, IntervaloEnvioBytes );
    end;

    with LCB do
    begin
      Ini.WriteString( CSecLCB, CKeyLCBPorta, Porta );
      Ini.WriteInteger( CSecLCB, CKeyLCBIntervalo, Intervalo );
      Ini.WriteString( CSecLCB, CKeyLCBSufixoLeitor, SufixoLeitor );
      Ini.WriteBool( CSecLCB, CKeyLCBExcluirSufixo, ExcluirSufixo );
      Ini.WriteString( CSecLCB, CKeyLCBPrefixoAExcluir, PrefixoAExcluir );
      Ini.WriteString( CSecLCB, CKeyLCBSufixoIncluir, SufixoIncluir );
      Ini.WriteString( CSecLCB, CKeyLCBDispositivo, Dispositivo );
      Ini.WriteBool( CSecLCB, CKeyLCBTeclado, Teclado );
      Ini.WriteString( CSecLCB, CKeyLCBDevice, Device );
    end;

    with RFD do
    begin
      Ini.WriteBool( CSecRFD, CKeyRFDGerarRFD, GerarRFD );
      Ini.WriteString( CSecRFD, CKeyRFDDirRFD, DirRFD );
      Ini.WriteBool( CSecRFD, CKeyRFDIgnoraECF_MFD, IgnoraECF_MFD );
    end;

    with BAL do
    begin
      Ini.WriteInteger( CSecBAL, CKeyBALModelo, Modelo );
      Ini.WriteString( CSecBAL, CKeyBALPorta, Porta );
      Ini.WriteInteger( CSecBAL, CKeyBALIntervalo, Intervalo );
      Ini.WriteString( CSecBAL, CKeyBALArqLog, ArqLog );
      Ini.WriteString( CSecBAL, CKeyBALDevice, Device );
    end;

    with ETQ do
    begin
      Ini.WriteInteger( CSecETQ, CKeyETQModelo, Modelo );
      Ini.WriteString( CSecETQ, CKeyETQPorta, Porta );
      Ini.WriteInteger( CSecETQ, CKeyETQDPI, DPI );
      Ini.WriteBool( CSecETQ, CKeyETQLimparMemoria, LimparMemoria );
      Ini.WriteInteger( CSecETQ, CKeyETQTemperatura, Temperatura );
      Ini.WriteInteger( CSecETQ, CKeyETQVelocidade, Velocidade );
      Ini.WriteInteger( CSecETQ, CKeyETQBackFeed, BackFeed );
      Ini.WriteInteger( CSecETQ, CKeyETQMargemEsquerda, MargemEsquerda );
      Ini.WriteInteger( CSecETQ, CKeyETQOrigem, Origem );
      Ini.WriteInteger( CSecETQ, CKeyETQUnidade, Unidade );
      Ini.WriteInteger( CSecETQ, CKeyETQCopias, Copias );
      Ini.WriteInteger( CSecETQ, CKeyETQAvanco, Avanco );
    end;

    with CEP do
    begin
      Ini.WriteInteger( CSecCEP, CKeyCEPWebService, WebService );
      Ini.WriteString( CSecCEP, CKeyCEPChave_BuscarCEP, Chave_BuscarCEP );
      Ini.WriteString( CSecCEP, CKeyCEPProxy_Host, Proxy_Host);
      Ini.WriteString( CSecCEP, CKeyCEPProxy_Port, Proxy_Port );
      Ini.WriteString( CSecCEP, CKeyCEPProxy_User, Proxy_User );
      Ini.WriteBool( CSecCEP, CKeyCEPIBGEAcentos, IBGEAcentos );
      GravaINICrypt(Ini, CSecCEP, CKeyCEPProxy_Pass, Proxy_Pass, _C);
    end;

    with ConsultaCNPJ do
    begin
      ini.WriteInteger( CSecConsultaCNPJ,CKeyConsultaCNPJProvedor, Provedor);
      GravaINICrypt(Ini,CSecConsultaCNPJ,CKeyConsultaCNPJUsuario , Usuario, _C);
      GravaINICrypt(Ini,CSecConsultaCNPJ,CKeyConsultaCNPJSenha   , Senha  , _C);
      Ini.WriteString( CSecConsultaCNPJ, CKeyCEPProxy_Host, Proxy_Host);
      Ini.WriteString( CSecConsultaCNPJ, CKeyCEPProxy_Port, Proxy_Port );
      Ini.WriteString( CSecConsultaCNPJ, CKeyCEPProxy_User, Proxy_User );
      GravaINICrypt(Ini, CSecConsultaCNPJ, CKeyCEPProxy_Pass, Proxy_Pass, _C);

    end;

    with TC do
    begin
      Ini.WriteInteger( CSecTC, CKeyTCModelo, Modelo );
      Ini.WriteInteger( CSecTC, CKeyTCP_Porta, TCP_Porta);
      Ini.WriteString( CSecTC, CKeyTCArq_Precos, Arq_Precos);
      Ini.WriteString( CSecTC, CKeyTCNao_Econtrado, Nao_Econtrado);
    end;

    with SEDEX do
    begin
      Ini.WriteString( CSecSEDEX, CKeyContratoSEDEX, Contrato );
      GravaINICrypt(Ini, CSecSEDEX, CKeySenhaSEDEX, SenhaSedex, _C);
    end;

    with NCM do
    begin
      Ini.WriteString( CSecNCM, CKeyDirNCMSalvar, DirNCMSalvar );
      Ini.WriteInteger( CSecNCM, CKeyDiasValidadeCache, DiasValidadeCache );
    end;

     with Email do
    begin
      Ini.WriteString( CSecEmail, CKeyEmailNomeExibicao, NomeExibicao );
      Ini.WriteString( CSecEmail, CKeyEmailEndereco, Endereco);
      Ini.WriteString( CSecEmail, CKeyEmail, Email );
      GravaINICrypt(Ini, CSecEmail, CKeyEmailUsuario, Usuario, _C);
      GravaINICrypt(Ini, CSecEmail, CKeyEmailSenha, Senha, _C);
      Ini.WriteInteger( CSecEmail, CKeyEmailPorta, Porta );
      Ini.WriteBool( CSecEmail, CKeyEmailExigeSSL, ExigeSSL );
      Ini.WriteBool( CSecEmail, CKeyEmailExigeTLS, ExigeTLS );
      Ini.WriteBool( CSecEmail, CKeyEmailConfirmacao, Confirmacao );
      Ini.WriteBool( CSecEmail, CKeyEmailSegundoPlano, SegundoPlano );
      Ini.WriteString( CSecEmail, CKeyEmailCodificacao, Codificacao );
      Ini.WriteBool( CSecEmail, CKeyEmailHTML, HTML );
      Ini.WriteInteger( CSecEmail, CKeyAttemptsMail, AttemptsMail );
      Ini.WriteInteger( CSecEmail, CKeyTimeoutMail, TimeOutMail );
      ini.WriteInteger( CSecEmail, CKeyEmailSSLType, SSLType);
    end;

    with DFe do
    begin
      Ini.WriteBool( CSecACBrNFeMonitor, CKeyIgnorarComandoModoEmissao, IgnorarComandoModoEmissao );
      Ini.WriteBool( CSecACBrNFeMonitor, CKeyModoXML, ModoXML );
      Ini.WriteBool( CSecACBrNFeMonitor, CKeyRetirarAcentos, RetirarAcentos );
      Ini.WriteBool( CSecACBrNFeMonitor, CKeyRetirarEspacos, RetirarEspacos );
      Ini.WriteBool( CSecACBrNFeMonitor, CKeyGravar_Log_Comp, Gravar_Log_Comp );
      Ini.WriteString( CSecACBrNFeMonitor, CKeyArquivo_Log_Comp, Arquivo_Log_Comp );
      Ini.WriteInteger( CSecACBrNFeMonitor, CKeyLinhas_Log_Comp, Linhas_Log_Comp );
      Ini.WriteString( CSecACBrNFeMonitor, CKeyArquivoWebServices, ArquivoWebServices );
      Ini.WriteString( CSecACBrNFeMonitor, CKeyArquivoWebServicesCTe, ArquivoWebServicesCTe );
      Ini.Writestring( CSecACBrNFeMonitor, CKeyArquivoWebServicesMDFe, ArquivoWebServicesMDFe );
      Ini.WriteString( CSecACBrNFeMonitor, CKeyArquivoWebServicesGNRe, ArquivoWebServicesGNRe );
      Ini.Writestring( CSecACBrNFeMonitor, CKeyArquivoWebServiceseSocial, ArquivoWebServiceseSocial );
      Ini.Writestring( CSecACBrNFeMonitor, CKeyArquivoWebServicesReinf, ArquivoWebServicesReinf );
      Ini.Writestring( CSecACBrNFeMonitor, CKeyArquivoWebServicesBPe, ArquivoWebServicesBPe );
      Ini.Writestring( CSecACBrNFeMonitor, CKeyArquivoWebServicesNFSe, ArquivoWebServicesNFSe );
      Ini.WriteBool( CSecACBrNFeMonitor, CKeyValidarDigest, ValidarDigest );
      Ini.WriteInteger( CSecACBrNFeMonitor, CKeyTimeoutWebService, TimeoutWebService );
    end;

    with DFE.Certificado do
    begin
      Ini.WriteInteger( CSecCertificado, CKeySSLLib, SSLLib );
      Ini.WriteInteger( CSecCertificado, CKeyCryptLib, CryptLib );
      Ini.WriteInteger( CSecCertificado, CKeyHttpLib, HttpLib );
      Ini.WriteInteger( CSecCertificado, CKeyXmlSignLib, XmlSignLib );
      Ini.WriteInteger( CSecCertificado, CKeySSLType, SSLType );
      Ini.WriteString( CSecCertificado, CKeyArquivoPFX, ArquivoPFX );
      Ini.WriteString( CSecCertificado, CKeyURLPFX, URLPFX );
      Ini.WriteString( CSecCertificado, CKeyNumeroSerie, NumeroSerie );
      GravaINICrypt(Ini, CSecCertificado, CKeySenha, Senha, _C);
      Ini.WriteBool( CSecCertificado, CKeyExibeRazaoSocialCertificado, ExibeRazaoSocialCertificado );
      Ini.WriteBool( CSecCertificado, CKeyVerificarValidade, VerificarValidade );
    end;

    with DFe.Impressao.Geral do
    begin
      Ini.WriteInteger( CSecGeral, CKeyDANFE, DANFE );
      Ini.WriteInteger( CSecGeral, CKeyFormaEmissao, FormaEmissao );
      Ini.WriteString( CSecGeral, CKeyLogomarca, Logomarca );
      Ini.WriteString( CSecGeral, CKeyLogoMarcaNFCeSAT, LogoMarcaNFCeSAT );
      Ini.WriteString( CSecGeral, CKeyLogoMarcaPrefeitura, LogoMarcaPrefeitura );
      Ini.WriteBool( CSecGeral, CKeySalvar, Salvar );
      Ini.WriteString( CSecGeral, CKeyPathSalvar, PathSalvar );
      Ini.WriteString( CSecGeral, CKeyImpressora, Impressora );
    end;

    with DFe.WebService do
    begin
      Ini.WriteString( CSecWebService, CKeyVersao, Versao );
      Ini.WriteString( CSecWebService, CKeyVersaoCTe, VersaoCTe );
      Ini.WriteString( CSecWebService, CKeyVersaoMDFe, VersaoMDFe );
      Ini.WriteString( CSecWebService, CKeyVersaoReinf, VersaoReinf );
      Ini.WriteString( CSecWebService, CKeyVersaoeSocial, VersaoeSocial );
      Ini.WriteString( CSecWebService, CKeyVersaoQRCode, VersaoQRCode );
      Ini.WriteString( CSecWebService, CKeyVersaoBPe, VersaoBPe );
      Ini.WriteString( CSecWebService, CKeyVersaoGNRe, VersaoGNRe );
      Ini.WriteInteger( CSecWebService, CKeyFormaEmissaoNFe, FormaEmissaoNFe );
      Ini.WriteInteger( CSecWebService, CKeyFormaEmissaoCTe, FormaEmissaoCTe );
      Ini.WriteInteger( CSecWebService, CKeyFormaEmissaoMDFe, FormaEmissaoMDFe );
      Ini.WriteInteger( CSecWebService, CKeyFormaEmissaoGNRe, FormaEmissaoGNRe );
      Ini.WriteInteger( CSecWebService, CKeyFormaEmissaoBPe, FormaEmissaoBPe );
      Ini.WriteInteger( CSecWebService, CKeyAmbiente, Ambiente );
      Ini.WriteString( CSecWebService, CKeyUF, UF );
      Ini.WriteBool( CSecWebService, CKeyAjustarAut, AjustarAut );
      Ini.WriteString( CSecWebService, CKeyAguardar, Aguardar );
      Ini.WriteString( CSecWebService, CKeyTentativas, Tentativas);
      Ini.WriteString( CSecWebService, CKeyWebServiceIntervalo, Intervalo );
      Ini.WriteInteger( CSecWebService, CKeyTimeZoneMode , TimeZoneMode );
      Ini.WriteString( CSecWebService, CKeyTimeZoneStr, TimeZoneStr );
      Ini.WriteBool( CSecWebService, CKeyCamposFatObrig, CamposFatObrig );
      Ini.WriteInteger( CSecWebService, CKeyTagRejeicao938, TagRejeicao938 );
    end;

    with DFe.RespTecnico do
    begin
      Ini.WriteString( CSecRespTecnico, CKeyCSRT, CSRT );
      Ini.WriteString( CSecRespTecnico, CKeyidCSRT, idCSRT );
    end;

    with DFe.ESocial do
    begin
      Ini.WriteString( CSecESocial, CKeyIdEmpregador, IdEmpregador );
      Ini.WriteString( CSecESocial, CKeyIdTransmissor, IdTransmissor );
      Ini.WriteString( CSecESocial, CKeyTipoEmpregador, TipoEmpregador );
    end;

    with DFe.Reinf do
    begin
      Ini.WriteString( CSecReinf, CKeyIdContribuinte, IdContribuinte );
      Ini.WriteString( CSecReinf, CKeyIdTransmissor, IdTransmissor );
      Ini.WriteString( CSecReinf, CKeyTipoContribuinte, TipoContribuinte );
    end;

    with DFe.WebService.Proxy do
    begin
      Ini.WriteString( CSecProxy, CKeyProxyHost, Host );
      Ini.WriteString( CSecProxy, CKeyProxyPorta, Porta );
      Ini.WriteString( CSecProxy, CKeyProxyUser, User );
      GravaINICrypt(Ini, CSecProxy, CKeyProxyPass, Pass, _C);
    end;

    with DFe.WebService.NFCe do
    begin
      Ini.WriteString( CSecNFCe, CKeyNFCeIdToken, IdToken);
      Ini.WriteString( CSecNFCe, CKeyNFCeToken, Token);
      Ini.WriteBool( CSecNFCe, CKeyNFCeTagQrCode, TagQrCode);
      Ini.WriteBool( CSecNFCe, CKeyNFCeUsarIntegrador, UsarIntegrador );
    end;

    with DFe.Email do
    begin
      Ini.WriteString( CSecEmail, CKeyMensagemNFe, MensagemNFe );
      Ini.WriteString( CSecEmail, CKeyAssuntoNFe, AssuntoNFe );
      Ini.WriteString( CSecEmail, CKeyMensagemCTe, MensagemCTe );
      Ini.WriteString( CSecEmail, CKeyAssuntoCTe, AssuntoCTe );
      Ini.WriteString( CSecEmail, CKeyMensagemMDFe, MensagemMDFe );
      Ini.WriteString( CSecEmail, CKeyAssuntoMDFe, AssuntoMDFe );
      Ini.WriteString( CSecEmail, CKeyMensagemBPe, MensagemBPe );
      Ini.WriteString( CSecEmail, CKeyAssuntoBPe, AssuntoBPe );
      Ini.WriteString( CSecEmail, CKeyMensagemNFSe, MensagemNFSe );
      Ini.WriteString( CSecEmail, CKeyAssuntoNFSe, AssuntoNFSe );
    end;

    with DFe.WebService.NFe do
    begin
      Ini.WriteString( CSecNFe, CKeyNFeCNPJContador, CNPJContador);
    end;

    with DFe.Impressao.NFCe.Emissao do
    begin
      Ini.WriteInteger( CSecNFCe, CKeyNFCeModelo, Modelo);
      Ini.WriteInteger( CSecNFCe, CKeyNFCeModoImpressaoEvento, ModoImpressaoEvento);
      Ini.WriteBool( CSecNFCe, CKeyNFCeImprimirItem1Linha, ImprimirItem1Linha);
      Ini.WriteBool( CSecNFCe, CKeyNFCeImprimirDescAcresItem, ImprimirDescAcresItem);
      Ini.WriteString( CSecNFCe, CKeyNFCeImpressoraPadrao, ImpressoraPadrao);
      Ini.WriteBool( CSecNFCe, CKeyNFCeQRCodeLateral, QRCodeLateral);
      Ini.WriteBool( CSecNFCe, CKeyNFCeUsaCodigoEanImpressao, UsaCodigoEanImpressao);
      Ini.WriteBool( CSecNFCe, CKeyNFCeImprimeNomeFantasia, ImprimeNomeFantasia);
      Ini.WriteInteger( CSecNFCe, CKeyNFCeImprimeTributos, ImprimeTributos);
      Ini.WriteBool( CSecNFCe, CKeyNFCeExibeTotalTributosItem, ExibeTotalTributosItem);
      Ini.WriteBool( CSecNFCe, CKeyNFCeLogoLateral, LogoLateral);
      Ini.WriteBool( CSecNFCe, CKeyNFCeImprimeItens, imprimeitens);
    end;

    with DFE.Impressao.NFCe.Emissao.DANFCe do
    begin
      Ini.WriteFloat( CSecDANFCe,   CKeyDANFCeMargemInf      , MargemInf );
      Ini.WriteFloat( CSecDANFCe,   CKeyDANFCeMargemSup      , MargemSup );
      Ini.WriteFloat( CSecDANFCe,   CKeyDANFCeMargemDir      , MargemDir );
      Ini.WriteFloat( CSecDANFCe,   CKeyDANFCeMargemEsq      , MargemEsq );
      Ini.WriteFloat( CSecDANFCe,   CKeyDANFCeLarguraBobina  , LarguraBobina );
      Ini.WriteBool(  CSecDANFCe,   CKeyDANFCEImprimeNNFFormatadoNFCe   , ImprimeNNFFormatadoNFCe );
    end;

    with DFE.Impressao.NFCe.Emissao.DANFCeTipoPagto do
    begin
      Ini.WriteBool( CSecDANFCeTipoPagto,   CKeyDANFCeTipoPagtoTipo      , Tipo );
      Ini.WriteBool( CSecDANFCeTipoPagto,   CKeyDANFCeTipoPagtoBandeira  , Bandeira );
      Ini.WriteBool( CSecDANFCeTipoPagto,   CKeyDANFCeTipoPagtoAutorizacao, Autorizacao );
    end;

    with FonteLinha do
    begin
      Ini.WriteString( CSecFonte,   CKeyFonteName   , name );
      Ini.WriteInteger( CSecFonte,  CKeyFonteColor , Color );
      Ini.WriteInteger( CSecFonte,  CKeyFonteSize  , Size );
      Ini.WriteBool( CSecFonte,   CKeyFonteStyleBold , fsBold in Style );
      Ini.WriteBool( CSecFonte,   CKeyFonteStyleItalic , fsItalic in Style );
      Ini.WriteBool( CSecFonte,   CKeyFonteStyleUnderline , fsUnderline in Style );
      Ini.WriteBool( CSecFonte,   CKeyFonteStyleStrckout , fsStrikeOut in Style );
    end;

    with DFE.Impressao.DANFE do
    begin
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEModelo                 , Modelo );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFETamanhoPapel           , TamanhoPapel );
      Ini.WriteString( CSecDANFE,   CKeyDANFESite                   , Site );
      Ini.WriteString( CSecDANFE,   CKeyDANFEEmail                  , Email );
      Ini.WriteString( CSecDANFE,   CKeyDANFEFax                    , Fax );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEImpDescPorc               , ImpDescPorc );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEMostrarPreview            , MostrarPreview );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFECopias                 , Copias );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFECopiasNFCe             , CopiasNFCe );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFELarguraCodigoProduto   , LarguraCodigoProduto );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEEspacoEntreProdutos    , EspacoEntreProdutos );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEFonteRazao             , FonteRazao );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEFonteEndereco          , FonteEndereco );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEFonteCampos            , FonteCampos );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEFonteAdicionais        , FonteAdicionais );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEAlturaCampos           , AlturaCampos );
      Ini.WriteFloat( CSecDANFE,   CKeyDANFEMargem                  , Margem );
      Ini.WriteFloat( CSecDANFE,   CKeyDANFEMargemSup               , MargemSup );
      Ini.WriteFloat( CSecDANFE,   CKeyDANFEMargemDir               , MargemDir );
      Ini.WriteFloat( CSecDANFE,   CKeyDANFEMargemEsq               , MargemEsq );
      Ini.WriteString( CSecDANFE,   CKeyDANFEPathPDF                , PathPDF );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEDecimaisQTD            , DecimaisQTD );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEDecimaisValor          , DecimaisValor );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEExibeResumo               , ExibeResumo );
      Ini.WriteString( CSecDANFE, CKeyDANFETextoResumoCanhoto       , TextoResumoCanhoto );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEImprimirTributosItem      , ImprimirTributosItem );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEImprimirValLiq            , ImprimirValLiq );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEUNComercialETributavel , UNComercialETributavel );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEPreImpresso               , PreImpresso );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEMostrarStatus             , MostrarStatus );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEExibirEAN                 , ExibirEAN );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEExibirCampoFatura         , ExibirCampoFatura );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEExpandirLogo              , ExpandirLogo );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEFonte                  , Fonte );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFELocalCanhoto           , LocalCanhoto );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFELayoutCanhoto          , LayoutCanhoto );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEQuebrarLinhasDetalheItens      , QuebrarLinhasDetalheItens );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEImprimirDetalhamentoEspecifico , ImprimirDetalhamentoEspecifico );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEImprimirDadosDocReferenciados  , ImprimirDadosDocReferenciados );
      Ini.WriteInteger( CSecDANFE,  CKeyDANFEExibirBandInforAdicProduto  , ExibirBandInforAdicProduto );
      Ini.WriteInteger (CSecDANFE, CKeyDANFEImprimeDescAcrescItemNFe     , ImprimeDescAcrescItemNFe);
      Ini.WriteBool( CSecDANFE,  CKeyDANFELogoEmCima                     , LogoEmCima );
      Ini.WriteBool( CSecDANFE, CKeyDANFEImprimeInscSuframa              , ImprimeInscSuframa);
      Ini.WriteBool( CSecDANFE, CKeyDANFEImprimeNNFFormatadoNFe          , ImprimeNNFFormatadoNFe);
      Ini.WriteBool( CSecDANFE,  CKeyDANFEExpandirDadosAdicionaisAuto , ExpandirDadosAdicionaisAuto );
      Ini.WriteBool( CSecDANFE,  CKeyDANFEImprimeContinuacaoDadosAdicionaisPrimeiraPagina, ImprimeContinuacaoDadosAdicionaisPrimeiraPagina );
      Ini.WriteInteger (CSecDANFE, CKeyDANFEImprimirCampoFormaPagamento, ImprimirCampoFormaPagamento);
      Ini.WriteBool( CSecDANFE,  CKeyDANFEImprimeXPedNitemPed  , ImprimeXPedNitemPed );

    end;

    with DFe.Impressao.DACTE do
    begin
      Ini.WriteInteger( CSecDACTE,  CKeyDACTETamanhoPapel           , TamanhoPapel );
    end;

    with DFe.Impressao.DAMFE do
    begin
      Ini.WriteBool( CSecDAMFE,  CKeyDAMFEExibirMunicipioDescar, ExibirMunicipioDescarregamento );
    end;

    with DFe.Diretorios do
    begin
      Ini.WriteBool(CSecArquivos,    CKeyArquivosSalvar,                      Salvar                      );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosPastaMensal,                 PastaMensal                 );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosAddLiteral,                  AddLiteral                  );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosEmissaoPathNFe,              EmissaoPathNFe              );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosSalvarCCeCanPathEvento,      SalvarCCeCanPathEvento      );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosSepararPorCNPJ,              SepararPorCNPJ              );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosSepararPorModelo,            SepararPorModelo            );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosSalvarApenasNFesAutorizadas, SalvarApenasNFesAutorizadas );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosAtualizarXMLCancelado,       AtualizarXMLCancelado       );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosNormatizarMunicipios,        NormatizarMunicipios        );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosUsarSeparadorPathPDF,        UsarSeparadorPathPDF        );
      Ini.WriteBool(CSecArquivos,    CKeyArquivosSepararPorNome,              SepararPorNome              );
      Ini.WriteString(CSecArquivos,  CKeyArquivosPathNFe,                     PathNFe                     );
      Ini.WriteString(CSecArquivos,  CKeyArquivosPathInu,                     PathInu                     );
      Ini.WriteString(CSecArquivos,  CKeyArquivosPathDPEC,                    PathDPEC                    );
      Ini.WriteString(CSecArquivos,  CKeyArquivosPathEvento,                  PathEvento                  );
      Ini.WriteString(CSecArquivos,  CKeyArquivosPathArqTXT,                  PathArqTXT                  );
      Ini.WriteString(CSecArquivos,  CKeyArquivosPathDownload,                PathDownload                );
      Ini.WriteString(CSecArquivos,  CKeyArquivosPathSchemasDFe,              PathSchemasDFe              );
    end;

    with SAT do
    begin
      ini.WriteInteger( CSecSAT, CKeySATModelo         , Modelo         );
      ini.WriteString(  CSecSAT, CKeySATMarca          , Marca          );
      ini.WriteString(  CSecSAT, CKeySATArqLog         , ArqLog         );
      ini.WriteString(  CSecSAT, CKeySATNomeDLL        , NomeDLL        );
      ini.WriteString(  CSecSAT, CKeySATCodigoAtivacao , CodigoAtivacao );
      ini.WriteString(  CSecSAT, CKeySATCodigoUF       , CodigoUF       );
      ini.WriteInteger( CSecSAT, CKeySATNumeroCaixa    , NumeroCaixa    );
      ini.WriteInteger( CSecSAT, CKeySATAmbiente       , Ambiente       );
      ini.WriteInteger( CSecSAT, CKeySATPaginaDeCodigo , PaginaDeCodigo );
      ini.WriteFloat(   CSecSAT, CKeySATversaoDadosEnt , versaoDadosEnt );
      ini.WriteBool(    CSecSAT, CKeySATFormatarXML    , FormatarXML    );
      ini.WriteString(  CSecSAT, CKeySATPathCFe        , PathCFe        );
      ini.WriteBool(    CSecSAT, CKeySATSalvarCFe      , SalvarCFe      );
      ini.WriteBool(    CSecSAT, CKeySATSalvarCFeCanc  , SalvarCFeCanc  );
      ini.WriteBool(    CSecSAT, CKeySATSalvarEnvio    , SalvarEnvio    );
      ini.WriteBool(    CSecSAT, CKeySATSepararPorCNPJ , SepararPorCNPJ );
      ini.WriteBool(    CSecSAT, CKeySATSepararPorMES  , SepararPorMES  );
      ini.WriteBool(    CSecSAT, CKeySATSepararPorANO  , SepararPorANO  );
      ini.WriteBool(    CSecSAT, CKeySATSepararPorDIA  , SepararPorDIA  );
      ini.WriteBool(    CSecSAT, CKeySATSepararPorModelo, SepararPorModelo  );
      ini.WriteBool(    CSecSAT, CKeySATValidarNumeroSessaoResposta, ValidarNumeroSessaoResposta );
      ini.WriteString(  CSecSAT, CKeySATPathCFeCanc    , PathCFeCanc    );
      ini.WriteString(  CSecSAT, CKeySATPathCFeEnvio   , PathCFeEnvio   );
      ini.WriteString(  CSecSAT, CKeySATPrefixoArqCFe  , PrefixoArqCFe  );
      ini.WriteString(  CSecSAT, CKeySATPrefixoArqCFeCanc  , PrefixoArqCFeCanc  );
    end;

    with SAT.SATImpressao.SATExtrato do
    begin
      ini.WriteBool(    CSecSATExtrato, CKeySATExtMostrarStatus           , MostrarStatus        );
      ini.WriteString(  CSecSATExtrato, CKeySATExtParamsString           , ParamsString          );
      ini.WriteBool(    CSecSATExtrato, CKeySATExtImprimeDescAcrescItem  , ImprimeDescAcrescItem );
      ini.WriteBool(    CSecSATExtrato, CKeySATExtImprimeEmUmaLinha      , ImprimeEmUmaLinha     );
      ini.WriteInteger( CSecSATExtrato, CKeySATExtImprimeChaveEmUmaLinha , ImprimeChaveEmUmaLinha);
      ini.WriteBool(    CSecSATExtrato, CKeySATExtUsaCodigoEanImpressao  , UsaCodigoEanImpressao );
      Ini.WriteBool(    CSecSATExtrato, CKeySATExtQRCodeLateral          , ImprimeQRCodeLateral);
      Ini.WriteBool(    CSecSATExtrato, CKeySATExtLogoLateral            , ImprimeLogoLateral);
      Ini.WriteInteger( CSecSATExtrato, CKeySATExtDecimaisQTD            , ExtratoDecimaisQTD );
      Ini.WriteInteger( CSecSATExtrato, CKeySATExtDecimaisValor          , ExtratoDecimaisValor );
      Ini.WriteString( CSecSATExtrato,   CKeySATExtMaskQTD                , ExtratoMaskQTD );
      Ini.WriteString( CSecSATExtrato,   CKeySATExtMaskValor              , ExtratoMaskValor );
      Ini.WriteInteger( CSecSATExtrato, CKeySATExtFormatoDecimal         , FormatoDecimal );
    end;

    with SAT.SATImpressao.SATEmit do
    begin
      ini.WriteString( CSecSATEmit, CKeySATEmitCNPJ           , CNPJ          );
      ini.WriteString( CSecSATEmit, CKeySATEmitIE             , IE            );
      ini.WriteString( CSecSATEmit, CKeySATEmitIM             , IM            );
      ini.WriteInteger(CSecSATEmit, CKeySATEmitRegTributario  , RegTributario );
      ini.WriteInteger(CSecSATEmit, CKeySATEmitRegTribISSQN   , RegTribISSQN  );
      ini.WriteInteger(CSecSATEmit, CKeySATEmitIndRatISSQN    , IndRatISSQN   );
    end;

    with SAT.SATImpressao.SATFortes do
    begin
      ini.WriteBool   (CSecSATFortes, CKeySATFortesUsarFortes        , UsarFortes    );
      ini.WriteInteger(CSecSATFortes, CKeySATFortesLargura           , Largura       );
      ini.WriteInteger(CSecSATFortes, CKeySATFortesMargemTopo        , MargemTopo    );
      ini.WriteInteger(CSecSATFortes, CKeySATFortesMargemFundo       , MargemFundo   );
      ini.WriteInteger(CSecSATFortes, CKeySATFortesMargemEsquerda    , MargemEsquerda);
      ini.WriteInteger(CSecSATFortes, CKeySATFortesMargemDireita     , MargemDireita );
      ini.WriteBool   (CSecSATFortes, CKeySATFortesPreview           , Preview       );
    end;

    with SAT.SATImpressao.SATPrinter do
    begin
      ini.WriteString(CSecSATPrinter, CKeySATPrinterName, Name);
    end;

    with SAT.SATRede do
    begin
      ini.WriteInteger(CSecSATRede, CKeySATRedetipoInter   , tipoInter   );
      ini.WriteInteger(CSecSATRede, CKeySATRedetipoLan     , tipoLan     );
      ini.WriteString (CSecSATRede, CKeySATRedeSSID        , SSID        );
      ini.WriteInteger(CSecSATRede, CKeySATRedeseg         , seg         );
      ini.WriteString (CSecSATRede, CKeySATRedecodigo      , codigo      );
      ini.WriteString (CSecSATRede, CKeySATRedelanIP       , lanIP       );
      ini.WriteString (CSecSATRede, CKeySATRedelanMask     , lanMask     );
      ini.WriteString (CSecSATRede, CKeySATRedelanGW       , lanGW       );
      ini.WriteString (CSecSATRede, CKeySATRedelanDNS1     , lanDNS1     );
      ini.WriteString (CSecSATRede, CKeySATRedelanDNS2     , lanDNS2     );
      ini.WriteString (CSecSATRede, CKeySATRedeusuario     , usuario     );
      ini.WriteString (CSecSATRede, CKeySATRedesenha       , senha       );
      ini.WriteInteger(CSecSATRede, CKeySATRedeproxy       , proxy       );
      ini.WriteString (CSecSATRede, CKeySATRedeproxy_ip    , proxy_ip    );
      ini.WriteInteger(CSecSATRede, CKeySATRedeproxy_porta , proxy_porta );
      ini.WriteString (CSecSATRede, CKeySATRedeproxy_user  , proxy_user  );
      ini.WriteString (CSecSATRede, CKeySATRedeproxy_senha , proxy_senha );
    end;

    with SAT.SATSwH do
    begin
      ini.WriteString(CSecSATSwH, CKeySATSwHCNPJ,       CNPJ);
      ini.WriteString(CSecSATSwH, CKeySATSwHAssinatura, Assinatura);
    end;

    with SAT.SATEmail do
    begin
      ini.WriteString(CSecSATEmail, CKeySATEmailAssunto, AssuntoSAT);
      ini.WriteString(CSecSATEmail, CKeySATEmailMensagem, MensagemSAT);
    end;

    with IntegradorFiscal do
    begin
      ini.WriteString(CSecSATIntegrador, CKeySATIntegradorInput,       Input);
      ini.WriteString(CSecSATIntegrador, CKeySATIntegradorOutput,      Output);
      ini.WriteInteger(CSecSATIntegrador, CKeySATIntegradorTimeout,    Timeout);
    end;

    with PosPrinter do
    begin
      ini.WriteInteger(CSecPosPrinter, CKeyPosPrinterModelo                , Modelo             );
      ini.WriteString( CSecPosPrinter, CKeyPosPrinterPorta                 , Porta              );
      ini.WriteInteger(CSecPosPrinter, CKeyPosPrinterColunas               , Colunas            );
      ini.WriteInteger(CSecPosPrinter, CKeyPosPrinterEspacoEntreLinhas     , EspacoEntreLinhas  );
      ini.WriteInteger(CSecPosPrinter, CKeyPosPrinterLinhasBuffer          , LinhasBuffer       );
      ini.WriteInteger(CSecPosPrinter, CKeyPosPrinterLinhasPular           , LinhasPular        );
      ini.WriteInteger(CSecPosPrinter, CKeyPosPrinterPaginaDeCodigo        , PaginaDeCodigo     );
      ini.WriteBool(   CSecPosPrinter, CKeyPosPrinterControlePorta         , ControlePorta      );
      ini.WriteBool(   CSecPosPrinter, CKeyPosPrinterCortarPapel           , CortarPapel        );
      ini.WriteBool(   CSecPosPrinter, CKeyPosPrinterTraduzirTags          , TraduzirTags       );
      ini.WriteBool(   CSecPosPrinter, CKeyPosPrinterIgnorarTags           , IgnorarTags        );
      ini.WriteString( CSecPosPrinter, CKeyPosPrinterArqLog                , ArqLog             );
      ini.WriteString( CSecPosPrinter, CKeyPosPrinterSerialParams          , SerialParams       );
    end;

    with PosPrinter.CodigoBarras do
    begin
      ini.WriteInteger(CSecBarras, CKeyBarrasLargura,Largura);
      ini.WriteInteger(CSecBarras, CKeyBarrasAltura ,Altura );
      ini.WriteBool(   CSecBarras, CKeyBarrasHRI    ,HRI    );
    end;

    with PosPrinter.QRCode do
    begin
      ini.WriteInteger(CSecQRCode, CKeyQRCodeTipo            , Tipo           );
      ini.WriteInteger(CSecQRCode, CKeyQRCodeLarguraModulo   , LarguraModulo  );
      ini.WriteInteger(CSecQRCode, CKeyQRCodeErrorLevel      , ErrorLevel     );
    end;

    with PosPrinter.Logo do
    begin
      ini.WriteBool(CSecLogo, CKeyLogoImprimir     , Imprimir );
      ini.WriteInteger(CSecLogo, CKeyLogoKC1       , KC1      );
      ini.WriteInteger(CSecLogo, CKeyLogoKC2       , KC2      );
      ini.WriteInteger(CSecLogo, CKeyLogoFatorX    , FatorX   );
      ini.WriteInteger(CSecLogo, CKeyLogoFatorY    , FatorY   );
    end;

    with PosPrinter.Gaveta do
    begin
      ini.WriteInteger(CSecGaveta, CKeyGavetaTempoON    , TempoON   );
      ini.WriteInteger(CSecGaveta, CKeyGavetaTempoOFF   , TempoOFF  );
      ini.WriteBool(CSecGaveta, CKeyGavSinalInvertido   , SinalInvertido  );
    end;

    with BOLETO do
    begin
      ini.WriteString( CSecBOLETO, CKeyBOLETONome,          Nome          );
      ini.WriteString( CSecBOLETO, CKeyBOLETOCNPJCPF,       CNPJCPF       );
      ini.WriteString( CSecBOLETO, CKeyBOLETOLogradouro,    Logradouro    );
      ini.WriteString( CSecBOLETO, CKeyBOLETONumero,        Numero        );
      ini.WriteString( CSecBOLETO, CKeyBOLETOBairro,        Bairro        );
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOCodCidade,    CodCidade     );
      ini.WriteString( CSecBOLETO, CKeyBOLETOCidade,        Cidade        );
      ini.WriteString( CSecBOLETO, CKeyBOLETOCEP,           CEP           );
      ini.WriteString( CSecBOLETO, CKeyBOLETOComplemento,   Complemento   );
      ini.WriteString( CSecBOLETO, CKeyBOLETOUF,            UF            );
    end;

    with Boleto.Conta do
    begin
      ini.WriteInteger(CSecBOLETO, CKeyBOLETORespEmis,      RespEmis      );
      ini.WriteInteger(CSecBOLETO, CKeyBOLETOPessoa,        Pessoa        );
      ini.WriteString( CSecBOLETO, CKeyBOLETOModalidade,    Modalidade    );
      ini.WriteString( CSecBOLETO, CKeyBOLETOConvenio,      Convenio      );
      Ini.WriteInteger(CSecBOLETO, CKeyBOLETOBanco,         Banco         );
      ini.WriteString( CSecBOLETO, CKeyBOLETOConta,         Conta         );
      ini.WriteString( CSecBOLETO, CKeyBOLETODigitoConta,   DigitoConta   );
      ini.WriteString( CSecBOLETO, CKeyBOLETOAgencia,       Agencia       );
      ini.WriteString( CSecBOLETO, CKeyBOLETODigitoAgencia, DigitoAgencia );
      ini.WriteString( CSecBOLETO, CKeyBOLETODigitoAgenciaConta, DigitoAgenciaConta );
      ini.WriteString( CSecBOLETO, CKeyBOLETOCodCedente,    CodCedente    );
      ini.WriteString( CSecBOLETO, CKeyBOLETOLocalPagamento,LocalPagamento );
      ini.WriteString( CSecBOLETO, CKeyBOLETOCodigoOperacao,CodigoOperacao );
    end;

    with BOLETO.PIX do
    begin
      ini.WriteString(CSecBOLETO,CKeyBOLETOChavePIX,ChavePix);
      ini.WriteInteger(CSecBOLETO,CKeyBOLETOTipoChavePix,TipoChavePix);
    end;


    with BOLETO.Layout do
    begin
      ini.WriteString( CSecBOLETO, CKeyBOLETODirLogos,      DirLogos      );
      ini.WriteInteger(CSecBOLETO, CKeyBOLETOCopias,        Copias        );
      Ini.WriteBool(   CSecBOLETO, CKeyBOLETOPreview,       Preview       );
      ini.WriteBool(   CSecBOLETO, CKeyBOLETOProgresso,     Progresso     );
      ini.WriteBool(   CSecBOLETO, CKeyBOLETOSetup,         Setup         );
      ini.WriteBool(   CSecBOLETO, CKeyBOLETOAlteraEscala,  AlteraEscala  );
      ini.WriteInteger(CSecBOLETO, CKeyBOLETOEscala,        Escala        );
      ini.WriteInteger(CSecBOLETO, CKeyBOLETOLayout,        Layout        );
      ini.WriteInteger(CSecBOLETO, CKeyBOLETOFiltro,        Filtro        );
      ini.WriteString( CSecBOLETO, CKeyBOLETODirArquivoBoleto,  DirArquivoBoleto       );
      Ini.WriteString( CSecBOLETO, CKeyBOLETOImpressora,                Impressora             );
      Ini.WriteString( CSecBOLETO, CKeyBOLETONomeArquivoBoleto, NomeArquivoBoleto);
      ini.WriteInteger(CSecBOLETO, CKeyBOLETOTipoMotorRelatorio,TipoMotorRelatorio);
      ini.WriteFloat(CSecBOLETO,   CKeyBOLETOMargemInferior,MargemInferior);
      ini.WriteFloat(CSecBOLETO,   CKeyBOLETOMargemSuperior,MargemSuperior);
      ini.WriteFloat(CSecBOLETO,   CKeyBOLETOMargemEsquerda,MargemEsquerda);
      ini.WriteFloat(CSecBOLETO,   CKeyBOLETOMargemDireita ,MargemDireita);

    end;

    with BOLETO.RemessaRetorno do
    begin
      ini.WriteString( CSecBOLETO, CKeyBOLETODirArquivoRemessa, DirArquivoRemessa      );
      ini.WriteString( CSecBOLETO, CKeyBOLETODirArquivoRetorno, DirArquivoRetorno      );
      ini.WriteInteger(CSecBOLETO, CKeyBOLETOCNAB,              CNAB                   );
      Ini.WriteBool(   CSecBOLETO, CKeyBOLETOLerCedenteRetorno, LerCedenteRetorno      );
      ini.WriteString( CSecBOLETO, CKeyBOLETOCodTransmissao,CodTransmissao);
      Ini.WriteBool(   CSecBOLETO, CKeyBOLETORemoveAcentos, RemoveAcentos      );
      ini.WriteString( CSecBOLETO, CKeyBoletoPrefixArqRemessa, PrefixArqRemessa );
      ini.WriteString( CSecBOLETO, CKeyBOLETOVersaoArquivo, VersaoArquivo);
      ini.WriteString( CSecBOLETO, CKeyBOLETOVersaoLote, VersaoLote);
    end;


    with BOLETO.Relatorio do
    begin
      Ini.WriteBool(   CSecBOLETO, CKeyBOLETOMostraPreviewRelRetorno,   MostraPreviewRelRetorno);
      Ini.WriteString( CSecBOLETO, CKeyBOLETOLogoEmpresa,               LogoEmpresa            );
    end;

    With BOLETO.Email do
    begin
      ini.WriteString( CSecBOLETO, CKeyBOLETOEmailAssuntoBoleto,        EmailAssuntoBoleto     );
      ini.WriteString( CSecBOLETO, CKeyBOLETOEmailMensagemBoleto,       EmailMensagemBoleto    );
      ini.WriteBool(   CSecBOLETO, CKeyBOLETOEmailFormatoHTML,          EmailFormatoHTML       );
    end;

    with BOLETO.WS.CedenteWS do
    begin
      ini.WriteString( CSecBOLETO, CKeyBOLETOClientID, ClientID );
      ini.WriteString( CSecBOLETO, CKeyBOLETOClientSecret, ClientSecret);
      ini.WriteString( CSecBOLETO, CKeyBOLETOKeyUser, KeyUser);
      ini.WriteString( CSecBOLETO, CKeyBOLETOScope, Scope);
      ini.WriteBool( CSecBOLETO, CKeyBOLETOIndicadorPix, IndicadorPix);
    end;

    with BOLETO.WS.Config do
    begin
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOLogNivel, Integer(LogNivel));
      ini.WriteString( CSecBOLETO, CKeyBOLETOPathGravarRegistro, PathGravarRegistro);
      ini.WriteString( CSecBOLETO, CKeyBOLETONomeArquivoLog, NomeArquivoLog);
    end;

    with BOLETO.WS.Config.SSL do
    begin
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOAmbiente, Ambiente);
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOOperacao, Operacao);
      ini.WriteString( CSecBOLETO, CKeyBOLETOProxyHost, Proxy_Host);
      ini.WriteString( CSecBOLETO, CKeyBOLETOProxyPass, Proxy_Pass);
      ini.WriteString( CSecBOLETO, CKeyBOLETOProxyPort, Proxy_Port);
      ini.WriteString( CSecBOLETO, CKeyBOLETOProxyUser, Proxy_User);
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOCryptLib, CryptLib);
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOHttpLib, HttpLib);
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOSSLType, SSLType);
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOXmlSignLib, XmlSignLib);
      ini.WriteInteger( CSecBOLETO, CKeyBOLETOTimeOut, TimeOut);
      ini.WriteBool( CSecBOLETO, CKeyBOLETOCertificadoHTTP, CertificadoHTTP);
      ini.WriteString( CSecBOLETO, CKeyBOLETOVersaoDF, VersaoDF);
      ini.WriteString( CSecBOLETO, CKeyBOLETOArquivoCRT, ArquivoCRT);
      ini.WriteString( CSecBOLETO, CKeyBOLETOArquivoKEY, ArquivoKEY);
    end;

    with NFSE do
    begin
      Ini.WriteInteger( CSecNFSE, CKeyNFSELayoutProvedor, LayoutProvedor );
      Ini.WriteInteger( CSecNFSE, CKeyNFSECodigoMunicipio, CodigoMunicipio );
      Ini.WriteString( CSecNFSE, CKeyNFSENomeMunicipio, NomeMunicipio );
      Ini.WriteString( CSecNFSE, CKeyNFSEUFMunicipio, UFMunicipio );
      Ini.WriteString( CSecNFSE, CKeyNFSeUsuario, Usuario );
      Ini.WriteString( CSecNFSE, CKeyNFSeSenha, Senha );
      Ini.WriteString( CSecNFSE, CKeyNFSeChaveAcesso, ChaveAcesso );
      Ini.WriteString( CSecNFSE, CKeyNFSeChaveAutenticacao, ChaveAutenticacao );
      Ini.WriteString( CSecNFSE, CKeyNFSeFraseSecreta, FraseSecreta );
      Ini.WriteString( CSecNFSE, CKeyNFSeCNPJEmitente, CNPJEmitente );
      Ini.WriteString( CSecNFSE, CKeyNFSeIMEmitente, IMEmitente );
      Ini.WriteString( CSecNFSE, CKeyNFSeNomeEmitente, NomeEmitente );
      Ini.WriteBool( CSecNFSE, CKeyNFSeMontarAutoPathSchema, MontarAutoPathSchema );
      Ini.WriteBool( CSecNFSE, CKeyNFSeConsultarLoteAposEnvio, ConsultarLoteAposEnvio );
      Ini.WriteBool( CSecNFSE, CKeyNFSeConsultarAposCancelar, ConsultarAposCancelar );
      Ini.WriteString( CSecNFSE, CKeyNFSeNomePrefeitura, NomePrefeitura );
      Ini.WriteString( CSecNFSE, CKeyNFSeCNPJPrefeitura, CNPJPrefeitura );
      Ini.WriteBool( CSecNFSE, CKeyNFSeNomeLongoNFSe, NomeLongoNFSe );

    end;

    SL := TStringList.Create;
    try
      Ini.GetStrings(SL);
      AStream.Position := 0;
      SL.SaveToStream(AStream);
    finally
      SL.Free;
    end;

  finally
    Ini.free;

  end;
end;

procedure TMonitorConfig.SalvarArquivo;
var
  FS: TFileStream;
begin
  ValidarNomeCaminho(True);

  FS := TFileStream.Create(FNomeArquivo, fmCreate or fmOpenReadWrite);
  try
    SalvarArquivoMemoria(FS);
  finally
    FS.Free;
    DoOnGravarConfig;
  end;

end;

procedure TMonitorConfig.CarregarArquivoMemoria(AStream: TStream);
var
  Ini: TMemIniFile;
   SL: TStringList;
begin
  Ini := TMemIniFile.Create('');
  try
    SL := TStringList.Create;
    try
      AStream.Position := 0;
      SL.LoadFromStream(AStream);
      Ini.SetStrings(SL);
    finally
      SL.Free;
    end;

    with ACBrMonitor do
    Begin
      Modo_TCP                  := Ini.ReadBool( CSecACBrMonitor, CKeyModo_TCP, Modo_TCP );
      Modo_TXT                  := Ini.ReadBool( CSecACBrMonitor, CKeyModo_TXT, Modo_TXT );
      MonitoraPasta             := Ini.ReadBool( CSecACBrMonitor, CKeyMonitorarPasta, MonitoraPasta );
      TCP_Porta                 := Ini.ReadInteger( CSecACBrMonitor, CKeyTCP_Porta, TCP_Porta);
      TCP_TimeOut               := Ini.ReadInteger( CSecACBrMonitor, CKeyTCP_TimeOut, TCP_TimeOut );
      Converte_TCP_Ansi         := Ini.ReadBool( CSecACBrMonitor, CKeyConverte_TCP_Ansi, Converte_TCP_Ansi );
      TXT_Entrada               := Ini.ReadString( CSecACBrMonitor, CKeyTXT_Entrada, TXT_Entrada );
      TXT_Saida                 := Ini.ReadString( CSecACBrMonitor, CKeyTXT_Saida, TXT_Saida );
      Converte_TXT_Entrada_Ansi := Ini.ReadBool( CSecACBrMonitor, CKeyConverte_TXT_Entrada_Ansi, Converte_TXT_Entrada_Ansi );
      Converte_TXT_Saida_Ansi   := Ini.ReadBool( CSecACBrMonitor, CKeyConverte_TXT_Saida_Ansi, Converte_TXT_Saida_Ansi );
      Intervalo                 := Ini.ReadInteger( CSecACBrMonitor, CKeyIntervalo, Intervalo);
      Gravar_Log                := Ini.ReadBool( CSecACBrMonitor, CKeyGravar_Log, Gravar_Log);
      Arquivo_Log               := Ini.ReadString( CSecACBrMonitor, CKeyArquivo_Log, Arquivo_Log);
      Linhas_Log                := Ini.ReadInteger( CSecACBrMonitor, CKeyLinhas_Log, Linhas_Log);
      Comandos_Remotos          := Ini.ReadBool( CSecACBrMonitor, CKeyComandos_Remotos, Comandos_Remotos);
      Uma_Instancia             := Ini.ReadBool( CSecACBrMonitor, CKeyUma_Instancia, Uma_Instancia);
      MostraAbas                := Ini.ReadBool( CSecACBrMonitor, CKeyMostraAbas, MostraAbas);
      {$IFDEF LINUX}
      MostrarNaBarraDeTarefas   := Ini.ReadBool( CSecACBrMonitor, CKeyMostrarNaBarraDeTarefas, True);
      {$ELSE}
      MostrarNaBarraDeTarefas   := Ini.ReadBool( CSecACBrMonitor, CKeyMostrarNaBarraDeTarefas, False);
      {$ENDIF}
      RetirarAcentosNaResposta  := Ini.ReadBool( CSecACBrMonitor, CKeyRetirarAcentosNaResposta, RetirarAcentosNaResposta);
      MostraLogEmRespostasEnviadas:= Ini.ReadBool( CSecACBrMonitor, CKeyMostraLogEmRespostasEnviadas, MostraLogEmRespostasEnviadas);
      HashSenha                 := LeINICrypt(Ini, CSecACBrMonitor, CKeyHashSenha, _C);
      Senha                     := Ini.ReadString( CSecACBrMonitor, CKeyMonitorSenha, Senha);
      VersaoSSL                 := Ini.ReadInteger( CSecACBrMonitor, CKeyMonitorSenha, VersaoSSL);
      TipoResposta              := Ini.ReadInteger( CSecACBrMonitor, CKeyTipoResposta, TipoResposta );
    end;

    with ECF do
    begin
      Modelo                    := Ini.ReadInteger( CSecECF, CKeymodelo, Modelo);
      Porta                     := Ini.ReadString( CSecECF, CKeyPorta, Porta );
      SerialParams              := Ini.ReadString( CSecECF, CKeySerialParams, SerialParams );
      Timeout                   := Ini.ReadInteger( CSecECF, CKeyTimeout, Timeout );
      IntervaloAposComando      := Ini.ReadInteger( CSecECF, CKeyIntervaloAposComando, IntervaloAposComando );
      MaxLinhasBuffer           := Ini.ReadInteger( CSecECF, CKeyMaxLinhasBuffer, MaxLinhasBuffer );
      PaginaCodigo              := Ini.ReadInteger( CSecECF, CKeyPaginaCodigo, PaginaCodigo );
      LinhasEntreCupons         := Ini.ReadInteger( CSecECF, CKeyLinhasEntreCupons, LinhasEntreCupons );
      ArredondamentoPorQtd      := Ini.ReadBool( CSecECF, CKeyArredondamentoPorQtd, ArredondamentoPorQtd );
      ArredondamentoItemMFD     := Ini.ReadBool( CSecECF, CKeyArredondamentoItemMFD, ArredondamentoItemMFD );
      DescricaoGrande           := Ini.ReadBool( CSecECF, CKeyDescricaoGrande, DescricaoGrande );
      GavetaSinalInvertido      := Ini.ReadBool( CSecECF, CKeyGavetaSinalInvertido, GavetaSinalInvertido );
      IgnorarTagsFormatacao     := Ini.ReadBool( CSecECF, CKeyIgnorarTagsFormatacao, IgnorarTagsFormatacao );
      ControlePorta             := Ini.ReadBool( CSecECF, CKeyControlePorta, ControlePorta );
      ArqLog                    := Ini.ReadString( CSecECF, CKeyArqLog, ArqLog );
    end;

    with CHQ do
    begin
      Modelo                    := Ini.ReadInteger( CSecCHQ, CKeyCHQModelo, Modelo);
      Porta                     := Ini.ReadString( CSecCHQ, CKeyCHQPorta, Porta);
      SerialParams              := Ini.ReadString( CSecCHQ, CKeyCHQSerialParams, SerialParams);
      VerificaFormulario        := Ini.ReadBool( CSecCHQ, CKeyCHQVerificaFormulario, VerificaFormulario);
      Favorecido                := Ini.ReadString( CSecCHQ, CKeyCHQFavorecido, Favorecido);
      Cidade                    := Ini.ReadString( CSecCHQ, CKeyCHQCidade, Cidade);
      PathBemafiINI             := Ini.ReadString( CSecCHQ, CKeyCHQPathBemafiINI, PathBemafiINI);
    end;

    with GAV do
    begin
      Modelo                    := Ini.ReadInteger( CSecGAV, CKeyGAVModelo, Modelo);
      Porta                     := Ini.ReadString( CSecGAV, CKeyGAVPorta, Porta);
      StringAbertura            := Ini.ReadString( CSecGAV, CKeyGAVStringAbertura, StringAbertura);
      AberturaIntervalo         := Ini.ReadInteger( CSecGAV, CKeyGAVAberturaIntervalo, AberturaIntervalo);
      AcaoAberturaAntecipada    := Ini.ReadInteger( CSecGAV, CKeyGAVAcaoAberturaAntecipada, AcaoAberturaAntecipada);
    end;

    with DIS do
    begin
      Modelo                    := Ini.ReadInteger( CSecDIS, CKeyDISModelo, Modelo );
      Porta                     := Ini.ReadString( CSecDIS, CKeyDISPorta, Porta );
      Intervalo                 := Ini.ReadInteger( CSecDIS, CKeyDISIntervalo, Intervalo );
      Passos                    := Ini.ReadInteger( CSecDIS, CKeyDISPassos, Passos );
      IntervaloEnvioBytes       := Ini.ReadInteger( CSecDIS, CKeyDISIntervaloEnvioBytes, IntervaloEnvioBytes );
    end;

    with LCB do
    begin
      Porta                     := Ini.ReadString( CSecLCB, CKeyLCBPorta, Porta );
      Intervalo                 := Ini.ReadInteger( CSecLCB, CKeyLCBIntervalo, Intervalo );
      SufixoLeitor              := Ini.ReadString( CSecLCB, CKeyLCBSufixoLeitor, SufixoLeitor );
      ExcluirSufixo             := Ini.ReadBool( CSecLCB, CKeyLCBExcluirSufixo, ExcluirSufixo );
      PrefixoAExcluir           := Ini.ReadString( CSecLCB, CKeyLCBPrefixoAExcluir, PrefixoAExcluir );
      SufixoIncluir             := Ini.ReadString( CSecLCB, CKeyLCBSufixoIncluir, SufixoIncluir );
      Dispositivo               := Ini.ReadString( CSecLCB, CKeyLCBDispositivo, Dispositivo );
      Teclado                   := Ini.ReadBool( CSecLCB, CKeyLCBTeclado, Teclado );
      Device                    := Ini.ReadString( CSecLCB, CKeyLCBDevice, Device );
    end;

    with RFD do
    begin
      GerarRFD                  := Ini.ReadBool( CSecRFD, CKeyRFDGerarRFD, GerarRFD );
      DirRFD                    := Ini.ReadString( CSecRFD, CKeyRFDDirRFD, DirRFD );
      IgnoraECF_MFD             := Ini.ReadBool( CSecRFD, CKeyRFDIgnoraECF_MFD, IgnoraECF_MFD );
    end;

    with BAL do
    begin
      Modelo                    := Ini.ReadInteger( CSecBAL, CKeyBALModelo, Modelo );
      Porta                     := Ini.ReadString( CSecBAL, CKeyBALPorta, Porta );
      Intervalo                 := Ini.ReadInteger( CSecBAL, CKeyBALIntervalo, Intervalo );
      ArqLog                    := Ini.ReadString( CSecBAL, CKeyBALArqLog, ArqLog );
      Device                    := Ini.ReadString( CSecBAL, CKeyBALDevice, Device );
    end;

    with ETQ do
    begin
      Modelo                    := Ini.ReadInteger( CSecETQ, CKeyETQModelo, Modelo );
      Porta                     := Ini.ReadString( CSecETQ, CKeyETQPorta, Porta );
      DPI                       := Ini.ReadInteger( CSecETQ, CKeyETQDPI, DPI );
      LimparMemoria             := Ini.ReadBool( CSecETQ, CKeyETQLimparMemoria, LimparMemoria );
      Temperatura               := Ini.ReadInteger( CSecETQ, CKeyETQTemperatura, Temperatura );
      Velocidade                := Ini.ReadInteger( CSecETQ, CKeyETQVelocidade, Velocidade );
      BackFeed                  := Ini.ReadInteger( CSecETQ, CKeyETQBackFeed, BackFeed );
      MargemEsquerda            := Ini.ReadInteger( CSecETQ, CKeyETQMargemEsquerda, MargemEsquerda );
      Origem                    := Ini.ReadInteger( CSecETQ, CKeyETQOrigem, Origem );
      Unidade                   := Ini.ReadInteger( CSecETQ, CKeyETQUnidade, Unidade );
      Copias                    := Ini.ReadInteger( CSecETQ, CKeyETQCopias, Copias );
      Avanco                    := Ini.ReadInteger( CSecETQ, CKeyETQAvanco, Avanco );
    end;

    with CEP do
    begin
      WebService                := Ini.ReadInteger( CSecCEP, CKeyCEPWebService, WebService );
      Chave_BuscarCEP           := Ini.ReadString( CSecCEP, CKeyCEPChave_BuscarCEP, Chave_BuscarCEP );
      Proxy_Host                := Ini.ReadString( CSecCEP, CKeyCEPProxy_Host, Proxy_Host );
      Proxy_Port                := Ini.ReadString( CSecCEP, CKeyCEPProxy_Port, Proxy_Port );
      Proxy_User                := Ini.ReadString( CSecCEP, CKeyCEPProxy_User, Proxy_User );
      IBGEAcentos               := Ini.ReadBool( CSecCEP, CKeyCEPIBGEAcentos, IBGEAcentos);
      Proxy_Pass                := LeINICrypt(Ini, CSecCEP, CKeyCEPProxy_Pass, _C);
    end;

    with ConsultaCNPJ do
    begin
      Provedor                  := Ini.ReadInteger( CSecConsultaCNPJ, CKeyConsultaCNPJProvedor, Provedor );
      Usuario                   := LeINICrypt(Ini, CSecConsultaCNPJ, CKeyConsultaCNPJUsuario, _C );
      Senha                     := LeINICrypt(Ini, CSecConsultaCNPJ, CKeyConsultaCNPJSenha, _C );
      Proxy_Host                := Ini.ReadString( CSecCEP, CKeyCEPProxy_Host, Proxy_Host );
      Proxy_Port                := Ini.ReadString( CSecCEP, CKeyCEPProxy_Port, Proxy_Port );
      Proxy_User                := Ini.ReadString( CSecCEP, CKeyCEPProxy_User, Proxy_User );
      Proxy_Pass                := LeINICrypt(Ini, CSecCEP, CKeyCEPProxy_Pass, _C);
    end;


    with TC do
    begin
      Modelo                    := Ini.ReadInteger( CSecTC, CKeyTCModelo, Modelo );
      TCP_Porta                 := Ini.ReadInteger( CSecTC, CKeyTCTCP_Porta, TCP_Porta );
      Arq_Precos                := Ini.ReadString( CSecTC, CKeyTCArq_Precos, Arq_Precos );
      Nao_Econtrado             := Ini.ReadString( CSecTC, CKeyTCNao_Econtrado, Nao_Econtrado );
    end;

    with SEDEX do
    begin
      Contrato                  := Ini.ReadString( CSecSEDEX, CKeyContratoSEDEX, Contrato );
      SenhaSedex                := LeINICrypt(Ini, CSecSEDEX, CKeySenhaSEDEX, _C);
    end;

    with NCM do
    begin
      DirNCMSalvar              := Ini.ReadString( CSecNCM, CKeyDirNCMSalvar, DirNCMSalvar );
      DiasValidadeCache         := Ini.ReadInteger( CSecNCM, CKeyDiasValidadeCache, DiasValidadeCache );
    end;

    with Email do
    begin
      NomeExibicao              := Ini.ReadString( CSecEmail, CKeyEmailNomeExibicao, NomeExibicao );
      Endereco                  := Ini.ReadString( CSecEmail, CKeyEmailEndereco, Endereco );
      Email                     := Ini.ReadString( CSecEmail, CKeyEmail, Email );
      Usuario                   := LeINICrypt(Ini, CSecEmail, CKeyEmailUsuario, _C);
      Senha                     := LeINICrypt(Ini, CSecEmail, CKeyEmailSenha, _C);
      Porta                     := Ini.ReadInteger( CSecEmail, CKeyEmailPorta, Porta );
      ExigeSSL                  := Ini.ReadBool( CSecEmail, CKeyEmailExigeSSL, ExigeSSL );
      ExigeTLS                  := Ini.ReadBool( CSecEmail, CKeyEmailExigeTLS, ExigeTLS );
      Confirmacao               := Ini.ReadBool( CSecEmail, CKeyEmailConfirmacao, Confirmacao );
      SegundoPlano              := Ini.ReadBool( CSecEmail, CKeyEmailSegundoPlano, SegundoPlano );
      Codificacao               := Ini.ReadString( CSecEmail, CKeyEmailCodificacao, Codificacao );
      HTML                      := Ini.ReadBool( CSecEmail, CKeyEmailHTML, HTML );
      AttemptsMail              := Ini.ReadInteger( CSecEmail, CKeyAttemptsMail, AttemptsMail );
      TimeOutMail               := Ini.ReadInteger( CSecEmail, CKeyTimeoutMail, TimeOutMail );
      SSLType                   := Ini.ReadInteger( CSecEmail, CKeyEmailSSLType, SSLType );
    end;

    with DFe do
    begin
      IgnorarComandoModoEmissao := Ini.ReadBool( CSecACBrNFeMonitor, CKeyIgnorarComandoModoEmissao, IgnorarComandoModoEmissao );
      ModoXML                   := Ini.ReadBool( CSecACBrNFeMonitor, CKeyModoXML, ModoXML );
      RetirarAcentos            := Ini.ReadBool( CSecACBrNFeMonitor, CKeyRetirarAcentos, RetirarAcentos );
      RetirarEspacos            := Ini.ReadBool( CSecACBrNFeMonitor, CKeyRetirarEspacos, RetirarEspacos );
      Gravar_Log_Comp           := Ini.ReadBool( CSecACBrNFeMonitor, CKeyGravar_Log_Comp, Gravar_Log_Comp );
      Arquivo_Log_Comp          := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivo_Log_Comp, Arquivo_Log_Comp );
      Linhas_Log_Comp           := Ini.ReadInteger( CSecACBrNFeMonitor, CKeyLinhas_Log_Comp, Linhas_Log_Comp );
      ArquivoWebServices        := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServices, AcertaPath( CACBrNFeServicosIni ) );
      ArquivoWebServicesCTe     := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServicesCTe, AcertaPath( CACBrCTeServicosIni ) );
      ArquivoWebServicesMDFe    := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServicesMDFe, AcertaPath( CACBrMDFeServicosIni ) );
      ArquivoWebServicesGNRe    := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServicesGNRe, AcertaPath( CACBrGNREServicosIni ) );
      ArquivoWebServiceseSocial := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServiceseSocial, AcertaPath( CACBreSocialServicosIni ) );
      ArquivoWebServicesReinf   := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServicesReinf, AcertaPath( CACBrReinfServicosIni ) );
      ArquivoWebServicesBPe     := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServicesBPe, AcertaPath( CACBrBPeServicosIni ) );
      ArquivoWebServicesNFSe    := Ini.ReadString( CSecACBrNFeMonitor, CKeyArquivoWebServicesNFSe, AcertaPath( CACBrNFSeServicosIni ) );
      ValidarDigest             := Ini.ReadBool( CSecACBrNFeMonitor, CKeyValidarDigest, ValidarDigest );
      TimeoutWebService         := Ini.ReadInteger( CSecACBrNFeMonitor, CKeyTimeoutWebService, TimeoutWebService );
    end;

    with DFe.Certificado do
    begin
      SSLLib                    := Ini.ReadInteger( CSecCertificado, CKeySSLLib, SSLLib );
      CryptLib                  := Ini.ReadInteger( CSecCertificado, CKeyCryptLib, CryptLib );
      HttpLib                   := Ini.ReadInteger( CSecCertificado, CKeyHttpLib, HttpLib );
      XmlSignLib                := Ini.ReadInteger( CSecCertificado, CKeyXmlSignLib, XmlSignLib );
      SSLType                   := Ini.ReadInteger( CSecCertificado, CKeySSLType, SSLType );
      ArquivoPFX                := Ini.ReadString( CSecCertificado, CKeyArquivoPFX, ArquivoPFX );
      URLPFX                    := Ini.ReadString( CSecCertificado, CKeyURLPFX, URLPFX);
      NumeroSerie               := Ini.Readstring( CSecCertificado, CKeyNumeroSerie, NumeroSerie );
      Senha                     := LeINICrypt(Ini, CSecCertificado, CKeySenha, _C);
      ExibeRazaoSocialCertificado:= Ini.ReadBool( CSecCertificado, CKeyExibeRazaoSocialCertificado, ExibeRazaoSocialCertificado );
      VerificarValidade         := Ini.ReadBool( CSecCertificado, CKeyVerificarValidade, VerificarValidade );
    end;

    with DFe.Impressao.Geral do
    begin
      DANFE                     := Ini.ReadInteger( CSecGeral, CKeyDANFE, DANFE );
      FormaEmissao              := Ini.ReadInteger( CSecGeral, CKeyFormaEmissao, 0 );
      Logomarca                 := Ini.ReadString( CSecGeral, CKeyLogomarca, Logomarca );
      LogoMarcaNFCeSAT          := Ini.ReadString( CSecGeral, CKeyLogoMarcaNFCeSAT, LogoMarcaNFCeSAT );
      LogoMarcaPrefeitura       := Ini.ReadString( CSecGeral, CKeyLogoMarcaPrefeitura, LogoMarcaPrefeitura );
      Salvar                    := Ini.ReadBool( CSecGeral, ckeysalvar, Salvar );
      PathSalvar                := Ini.ReadString( CSecGeral, CKeyPathSalvar, PathSalvar );
      Impressora                := Ini.ReadString( CSecGeral, CKeyImpressora, Impressora );
    end;

    with DFe.WebService do
    begin
      Versao                    := Ini.ReadString( CSecWebService, CKeyVersao, Versao );
      VersaoCTe                 := Ini.ReadString( CSecWebService, CKeyVersaoCTe, VersaoCTe );
      VersaoMDFe                := Ini.ReadString( CSecWebService, CKeyVersaoMDFe, VersaoMDFe );
      VersaoeSocial             := Ini.ReadString( CSecWebService, CKeyVersaoeSocial, CvalueVersaoeSocial );
      VersaoReinf               := Ini.ReadString( CSecWebService, CKeyVersaoReinf, CvalueVersaoReinf );
      VersaoQRCode              := Ini.ReadString( CSecWebService, CKeyVersaoQRCode, CvalueVersaoQRCode );
      VersaoBPe                 := Ini.ReadString( CSecWebService, CKeyVersaoBPe, VersaoBPe );
      VersaoGNRe                := Ini.ReadString( CSecWebService, CKeyVersaoGNRe, VersaoGNRe );
      FormaEmissaoNFe           := Ini.ReadInteger( CSecWebService, CKeyFormaEmissaoNFe, DFe.Impressao.Geral.FormaEmissao );
      FormaEmissaoCTe           := Ini.ReadInteger( CSecWebService, CKeyFormaEmissaoCTe, DFe.Impressao.Geral.FormaEmissao );
      FormaEmissaoGNRe          := Ini.ReadInteger( CSecWebService, CKeyFormaEmissaoGNRe, DFe.Impressao.Geral.FormaEmissao );
      FormaEmissaoMDFe          := Ini.ReadInteger( CSecWebService, CKeyFormaEmissaoMDFe, DFe.Impressao.Geral.FormaEmissao );
      FormaEmissaoBPe           := Ini.ReadInteger( CSecWebService, CKeyFormaEmissaoBPe, DFe.Impressao.Geral.FormaEmissao );
      Ambiente                  := Ini.ReadInteger( CSecWebService, CKeyAmbiente, Ambiente);
      UF                        := Ini.ReadString( CSecWebService, CKeyUF, UF);
      AjustarAut                := Ini.ReadBool( CSecWebService, CKeyAjustarAut, AjustarAut);
      Aguardar                  := Ini.ReadString( CSecWebService, CKeyAguardar, Aguardar);
      Tentativas                := Ini.ReadString( CSecWebService, CKeyTentativas, Tentativas);
      Intervalo                 := Ini.ReadString( CSecWebService, CKeyWebServiceIntervalo, Intervalo);
      TimeZoneMode              := Ini.ReadInteger( CSecWebService, CKeyTimeZoneMode, TimeZoneMode);
      TimeZoneStr               := Ini.ReadString( CSecWebService, CKeyTimeZoneStr, TimeZoneStr);
      CamposFatObrig            := Ini.ReadBool( CSecWebService, CKeyCamposFatObrig, True);
      TagRejeicao938            := Ini.ReadInteger( CSecWebService, CKeyTagRejeicao938, TagRejeicao938 );
    end;

    with DFe.WebService.Proxy do
    begin
      Host                      := Ini.ReadString( CSecProxy, CKeyProxyHost, Host );
      Porta                     := Ini.ReadString( CSecProxy, CKeyProxyPorta, Porta );
      User                      := Ini.ReadString( CSecProxy, CKeyProxyUser, User );
      Pass                      := LeINICrypt(Ini, CSecProxy, CKeyProxyPass, _C);
    end;

    with DFe.WebService.NFCe do
    begin
      IdToken                   := Ini.ReadString( CSecNFCe, CKeyNFCeIdToken, IdToken );
      Token                     := Ini.ReadString( CSecNFCe, CKeyNFCeToken, Token );
      TagQrCode                 := Ini.ReadBool( CSecNFCe, CKeyNFCeTagQrCode, TagQrCode );
      UsarIntegrador            := Ini.ReadBool( CSecNFCe, CKeyNFCeUsarIntegrador, UsarIntegrador );
    end;

    with DFe.Email do
    begin
      MensagemNFe               := Ini.ReadString( CSecEmail, CKeyMensagemNFe, MensagemNFe );
      AssuntoNFe                := Ini.ReadString( CSecEmail, CKeyAssuntoNFe, AssuntoNFe);
      MensagemCTe               := Ini.ReadString( CSecEmail, CKeyMensagemCTe, MensagemCTe );
      AssuntoCTe                := Ini.ReadString( CSecEmail, CKeyAssuntoCTe, AssuntoCTe);
      MensagemMDFe              := Ini.ReadString( CSecEmail, CKeyMensagemMDFe, MensagemMDFe );
      AssuntoMDFe               := Ini.ReadString( CSecEmail, CKeyAssuntoMDFe, AssuntoMDFe);
      MensagemBPe               := Ini.ReadString( CSecEmail, CKeyMensagemBPe, MensagemBPe );
      AssuntoBPe                := Ini.ReadString( CSecEmail, CKeyAssuntoBPe, AssuntoBPe);
      MensagemNFSe              := Ini.ReadString( CSecEmail, CKeyMensagemNFSe, MensagemNFSe );
      AssuntoNFSe               := Ini.ReadString( CSecEmail, CKeyAssuntoNFSe, AssuntoNFSe);
    end;

    with DFe.WebService.NFe do
    begin
      CNPJContador              := Ini.ReadString( CSecNFe, CKeyNFeCNPJContador, CNPJContador );
    end;

    with DFe.Impressao.NFCe.Emissao do
    begin
      Modelo                    := Ini.ReadInteger( CSecNFCe, CKeyNFCeModelo, Modelo );
      ModoImpressaoEvento       := Ini.ReadInteger( CSecNFCe, CKeyNFCeModoImpressaoEvento, ModoImpressaoEvento );
      ImprimirItem1Linha        := Ini.ReadBool( CSecNFCe, CKeyNFCeImprimirItem1Linha, ImprimirItem1Linha );
      ImprimirDescAcresItem     := Ini.ReadBool( CSecNFCe, CKeyNFCeImprimirDescAcresItem, ImprimirDescAcresItem );
      ImpressoraPadrao          := Ini.ReadString( CSecNFCe, CKeyNFCeImpressoraPadrao, ImpressoraPadrao );
      QRCodeLateral             := Ini.ReadBool( CSecNFCe, CKeyNFCeQRCodeLateral, QRCodeLateral );
      UsaCodigoEanImpressao     := Ini.ReadBool( CSecNFCe, CKeyNFCeUsaCodigoEanImpressao, UsaCodigoEanImpressao );
      ImprimeNomeFantasia       := Ini.ReadBool( CSecNFCe, CKeyNFCeImprimeNomeFantasia, ImprimeNomeFantasia );
      ImprimeTributos           := Ini.ReadInteger( CSecNFCe, CKeyNFCeImprimeTributos, ImprimeTributos );
      ExibeTotalTributosItem    := Ini.ReadBool( CSecNFCe, CKeyNFCeExibeTotalTributosItem, ExibeTotalTributosItem);
      LogoLateral               := Ini.ReadBool( CSecNFCe, CKeyNFCeLogoLateral, LogoLateral );
      ImprimeItens              := Ini.ReadBool( CSecNFCe, CKeyNFCeImprimeItens, ImprimeItens );
    end;

    with DFE.Impressao.NFCe.Emissao.DANFCe do
    begin
      MargemInf                 :=  Ini.ReadFloat( CSecDANFCe,   CKeyDANFCeMargemInf,     MargemInf     );
      MargemSup                 :=  Ini.ReadFloat( CSecDANFCe,   CKeyDANFCEMargemSup,     MargemSup     );
      MargemDir                 :=  Ini.ReadFloat( CSecDANFCe,   CKeyDANFCEMargemDir,     MargemDir     );
      MargemEsq                 :=  Ini.ReadFloat( CSecDANFCe,   CKeyDANFCEMargemEsq,     MargemEsq     );
      LarguraBobina             :=  Ini.ReadFloat( CSecDANFCe,   CKeyDANFCeLarguraBobina, LarguraBobina );
      ImprimeNNFFormatadoNFCe   :=  Ini.ReadBool( CSecDANFCE, CKeyDANFCEImprimeNNFFormatadoNFCe          , ImprimeNNFFormatadoNFCe);
    end;

    with DFE.Impressao.NFCe.Emissao.DANFCeTipoPagto do
    begin
      tipo                 :=  Ini.ReadBool( CSecDANFCeTipoPagto,   CKeyDANFCeTipoPagtoTipo,     Tipo     );
      Bandeira             :=  Ini.ReadBool( CSecDANFCeTipoPagto,   CKeyDANFCeTipoPagtoBandeira, Bandeira );
      Autorizacao          :=  Ini.ReadBool( CSecDANFCeTipoPagto,   CKeyDANFCeTipoPagtoAutorizacao, Autorizacao );
    end;

    with FonteLinha do
    begin
      Name    :=  Ini.ReadString( CSecFonte, CKeyFonteName, FFonteLinha.Name );
      Color   :=  TColor(Ini.ReadInteger( CSecFonte, CKeyFonteColor, FFonteLinha.Color ));
      Size    :=  Ini.ReadInteger( CSecFonte, CKeyFonteSize, FFonteLinha.Size );
      Style := [];
      if Ini.ReadBool( CSecFonte,   CKeyFonteStyleBold , False ) then
        Style := Style + [fsBold];
      if Ini.ReadBool( CSecFonte,   CKeyFonteStyleItalic , False ) then
        Style := Style + [fsItalic];
      if Ini.ReadBool( CSecFonte,   CKeyFonteStyleUnderline , False ) then
        Style := Style + [fsUnderline];
      if Ini.ReadBool( CSecFonte,   CKeyFonteStyleStrckout , False ) then
        Style := Style + [fsStrikeOut];
    end;

    with DFE.Impressao.DANFE do
    begin
      Modelo                    :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEModelo                         , Modelo );
      TamanhoPapel              :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFETamanhoPapel                   , TamanhoPapel );
      Site                      :=  Ini.ReadString( CSecDANFE,   CKeyDANFESite                           , Site );
      Email                     :=  Ini.ReadString( CSecDANFE,   CKeyDANFEEmail                          , Email );
      Fax                       :=  Ini.ReadString( CSecDANFE,   CKeyDANFEFax                            , Fax );
      ImpDescPorc               :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEImpDescPorc                       , ImpDescPorc );
      MostrarPreview            :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEMostrarPreview                    , MostrarPreview );
      Copias                    :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFECopias                         , Copias );
      CopiasNFCe                :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFECopiasNFCe                     , CopiasNFCe );
      LarguraCodigoProduto      :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFELarguraCodigoProduto           , LarguraCodigoProduto );
      EspacoEntreProdutos       :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEEspacoEntreProdutos            , EspacoEntreProdutos );
      FonteRazao                :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEFonteRazao                     , FonteRazao );
      FonteEndereco             :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEFonteEndereco                  , FonteEndereco );
      FonteCampos               :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEFonteCampos                    , FonteCampos );
      FonteAdicionais           :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEFonteAdicionais                , FonteAdicionais );
      AlturaCampos              :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEAlturaCampos                   , AlturaCampos );
      Margem                    :=  Ini.ReadFloat( CSecDANFE,   CKeyDANFEMargem                          , Margem );
      MargemSup                 :=  Ini.ReadFloat( CSecDANFE,   CKeyDANFEMargemSup                       , MargemSup );
      MargemDir                 :=  Ini.ReadFloat( CSecDANFE,   CKeyDANFEMargemDir                       , MargemDir );
      MargemEsq                 :=  Ini.ReadFloat( CSecDANFE,   CKeyDANFEMargemEsq                       , MargemEsq );
      PathPDF                   :=  Ini.ReadString( CSecDANFE,   CKeyDANFEPathPDF                        , PathPDF);
      DecimaisQTD               :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEDecimaisQTD                    , DecimaisQTD );
      DecimaisValor             :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEDecimaisValor                  , DecimaisValor );
      ExibeResumo               :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEExibeResumo                       , ExibeResumo );
      TextoResumoCanhoto        :=  Ini.ReadString( CSecDANFE, CKeyDANFETextoResumoCanhoto               , TextoResumoCanhoto);
      ImprimirTributosItem      :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEImprimirTributosItem              , ImprimirTributosItem );
      ImprimirValLiq            :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEImprimirValLiq                    , ImprimirValLiq );
      UNComercialETributavel    :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEUNComercialETributavel         , UNComercialETributavel );
      PreImpresso               :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEPreImpresso                       , PreImpresso );
      MostrarStatus             :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEMostrarStatus                     , MostrarStatus );
      ExibirEAN                 :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEExibirEAN                         , ExibirEAN );
      ExibirCampoFatura         :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEExibirCampoFatura                 , ExibirCampoFatura );
      ExpandirLogo              :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEExpandirLogo                      , ExpandirLogo );
      Fonte                     :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFEFonte                          , Fonte );
      LocalCanhoto              :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFELocalCanhoto                   , LocalCanhoto );
      LayoutCanhoto             :=  Ini.ReadInteger( CSecDANFE,  CKeyDANFELayoutCanhoto                  , LayoutCanhoto );
      QuebrarLinhasDetalheItens :=  Ini.ReadBool( CSecDANFE,  CKeyDANFEQuebrarLinhasDetalheItens         , QuebrarLinhasDetalheItens );
      ImprimirDetalhamentoEspecifico := Ini.ReadBool( CSecDANFE,  CKeyDANFEImprimirDetalhamentoEspecifico , ImprimirDetalhamentoEspecifico );
      ImprimirDadosDocReferenciados := Ini.ReadBool( CSecDANFE,  CKeyDANFEImprimirDadosDocReferenciados  , ImprimirDadosDocReferenciados );
      ExibirBandInforAdicProduto := Ini.ReadInteger( CSecDANFE,  CKeyDANFEExibirBandInforAdicProduto     , ExibirBandInforAdicProduto );
      ImprimeDescAcrescItemNFe   := Ini.ReadInteger( CSecDANFE, CKeyDANFEImprimeDescAcrescItemNFe        , ImprimeDescAcrescItemNFe );
      LogoEmCima                 := Ini.ReadBool( CSecDANFE,  CKeyDANFELogoEmCima                        , LogoEmCima );
      ImprimeInscSuframa         := Ini.ReadBool( CSecDANFE, CKeyDANFEImprimeInscSuframa                 , ImprimeInscSuframa);
      ImprimeNNFFormatadoNFe     := Ini.ReadBool( CSecDANFE, CKeyDANFEImprimeNNFFormatadoNFe             , ImprimeNNFFormatadoNFe);
      ExpandirDadosAdicionaisAuto:= Ini.ReadBool( CSecDANFE,  CKeyDANFEExpandirDadosAdicionaisAuto      , ExpandirDadosAdicionaisAuto );
      ImprimeContinuacaoDadosAdicionaisPrimeiraPagina:= Ini.ReadBool( CSecDANFE,  CKeyDANFEImprimeContinuacaoDadosAdicionaisPrimeiraPagina,
                                                        ImprimeContinuacaoDadosAdicionaisPrimeiraPagina );
      ImprimirCampoFormaPagamento   := Ini.ReadInteger( CSecDANFE, CKeyDANFEImprimirCampoFormaPagamento  , ImprimirCampoFormaPagamento );
      ImprimeXPedNitemPed           := Ini.ReadBool( CSecDANFE, CKeyDANFEImprimeXPedNitemPed  , ImprimeXPedNitemPed );

    end;

    with DFe.Impressao.DACTE do
    begin
      TamanhoPapel              :=  Ini.ReadInteger( CSecDACTE,  CKeyDACTETamanhoPapel , TamanhoPapel );
    end;

    with DFe.Impressao.DAMFE do
    begin
      ExibirMunicipioDescarregamento := Ini.ReadBool( CSecDAMFE,  CKeyDAMFEExibirMunicipioDescar, ExibirMunicipioDescarregamento );
    end;

    with DFe.Diretorios do
    begin
      Salvar                     := Ini.ReadBool( CSecArquivos,    CKeyArquivosSalvar,                      Salvar                      );
      PastaMensal                := Ini.ReadBool( CSecArquivos,    CKeyArquivosPastaMensal,                 PastaMensal                 );
      AddLiteral                 := Ini.ReadBool( CSecArquivos,    CKeyArquivosAddLiteral,                  AddLiteral                  );
      EmissaoPathNFe             := Ini.ReadBool( CSecArquivos,    CKeyArquivosEmissaoPathNFe,              EmissaoPathNFe              );
      SalvarCCeCanPathEvento     := Ini.ReadBool( CSecArquivos,    CKeyArquivosSalvarCCeCanPathEvento,      SalvarCCeCanPathEvento      );
      SepararPorCNPJ             := Ini.ReadBool( CSecArquivos,    CKeyArquivosSepararPorCNPJ,              SepararPorCNPJ              );
      SepararPorModelo           := Ini.ReadBool( CSecArquivos,    CKeyArquivosSepararPorModelo,            SepararPorModelo            );
      SalvarApenasNFesAutorizadas:= Ini.ReadBool( CSecArquivos,    CKeyArquivosSalvarApenasNFesAutorizadas, SalvarApenasNFesAutorizadas );
      AtualizarXMLCancelado      := Ini.ReadBool( CSecArquivos,    CKeyArquivosAtualizarXMLCancelado,       AtualizarXMLCancelado       );
      NormatizarMunicipios       := Ini.ReadBool( CSecArquivos,    CKeyArquivosNormatizarMunicipios,        NormatizarMunicipios        );
      UsarSeparadorPathPDF       := Ini.ReadBool( CSecArquivos,    CKeyArquivosUsarSeparadorPathPDF,        UsarSeparadorPathPDF        );
      SepararPorNome             := Ini.ReadBool( CSecArquivos,    CKeyArquivosSepararPorNome,              SepararPorNome              );
      PathNFe                    := Ini.ReadString(CSecArquivos,   CKeyArquivosPathNFe,                     PathNFe                     );
      PathInu                    := Ini.ReadString(CSecArquivos,   CKeyArquivosPathInu,                     PathInu                     );
      PathDPEC                   := Ini.ReadString(CSecArquivos,   CKeyArquivosPathDPEC,                    PathDPEC                    );
      PathEvento                 := Ini.ReadString(CSecArquivos,   CKeyArquivosPathEvento,                  PathEvento                  );
      PathArqTXT                 := Ini.ReadString(CSecArquivos,   CKeyArquivosPathArqTXT,                  PathArqTXT                  );
      PathDownload               := Ini.ReadString(CSecArquivos,   CKeyArquivosPathDownload,                PathDownload                );
      PathSchemasDFe             := Ini.ReadString(CSecArquivos,   CKeyArquivosPathSchemasDFe,              PathSchemasDFe              );

    end;

    with DFe.RespTecnico do
    begin
      CSRT                       := Ini.ReadString( CSecRespTecnico,    CKeyCSRT,  CSRT  );
      idCSRT                     := Ini.ReadString( CSecRespTecnico,    CKeyidCSRT,  idCSRT  );
    end;

    with DFe.ESocial do
    begin
      IdEmpregador               := Ini.ReadString( CSecESocial,    CKeyIdEmpregador,  IdEmpregador  );
      IdTransmissor              := Ini.ReadString( CSecESocial,    CKeyIdTransmissor,  IdTransmissor  );
      TipoEmpregador             := Ini.ReadString( CSecESocial,    CKeyTipoEmpregador,  CValueTipoEmpregador  );
    end;

    with DFe.Reinf do
    begin
      IdContribuinte             := Ini.ReadString( CSecReinf,    CKeyIdContribuinte,  IdContribuinte  );
      IdTransmissor              := Ini.ReadString( CSecReinf,    CKeyIdTransmissor,  IdTransmissor  );
      TipoContribuinte           := Ini.ReadString( CSecReinf,    CKeyTipoContribuinte,  CValueTipoContribuinte  );
    end;

    with SAT do
    begin
      Modelo                    := ini.ReadInteger( CSecSAT, CKeySATModelo         , Modelo         );
      Marca                     := ini.ReadString(  CSecSAT, CKeySATMarca          , Marca          );
      ArqLog                    := ini.ReadString(  CSecSAT, CKeySATArqLog         , ArqLog         );
      NomeDLL                   := ini.ReadString(  CSecSAT, CKeySATNomeDLL        , NomeDLL        );
      CodigoAtivacao            := ini.ReadString(  CSecSAT, CKeySATCodigoAtivacao , CodigoAtivacao );
      CodigoUF                  := ini.ReadString(  CSecSAT, CKeySATCodigoUF       , CodigoUF       );
      NumeroCaixa               := ini.ReadInteger( CSecSAT, CKeySATNumeroCaixa    , NumeroCaixa    );
      Ambiente                  := ini.ReadInteger( CSecSAT, CKeySATAmbiente       , Ambiente       );
      PaginaDeCodigo            := ini.ReadInteger( CSecSAT, CKeySATPaginaDeCodigo , PaginaDeCodigo );
      versaoDadosEnt            := ini.ReadFloat(   CSecSAT, CKeySATversaoDadosEnt , versaoDadosEnt );
      FormatarXML               := ini.ReadBool(    CSecSAT, CKeySATFormatarXML    , FormatarXML    );
      PathCFe                   := ini.ReadString(  CSecSAT, CKeySATPathCFe        , PathCFe        );
      SalvarCFe                 := ini.ReadBool(    CSecSAT, CKeySATSalvarCFe      , SalvarCFe      );
      SalvarCFeCanc             := ini.ReadBool(    CSecSAT, CKeySATSalvarCFeCanc  , SalvarCFeCanc  );
      SalvarEnvio               := ini.ReadBool(    CSecSAT, CKeySATSalvarEnvio    , SalvarEnvio    );
      SepararPorCNPJ            := ini.ReadBool(    CSecSAT, CKeySATSepararPorCNPJ , SepararPorCNPJ );
      SepararPorMES             := ini.ReadBool(    CSecSAT, CKeySATSepararPorMES  , SepararPorMES  );
      SepararPorANO             := ini.ReadBool(    CSecSAT, CKeySATSepararPorANO  , SepararPorANO  );
      SepararPorDIA             := ini.ReadBool(    CSecSAT, CKeySATSepararPorDIA  , SepararPorDIA  );
      SepararPorModelo          := ini.ReadBool(    CSecSAT, CKeySATSepararPorModelo  , SepararPorModelo  );
      ValidarNumeroSessaoResposta := ini.ReadBool(CSecSAT, CKeySATValidarNumeroSessaoResposta, ValidarNumeroSessaoResposta );
      PathCFeCanc               := ini.ReadString(  CSecSAT, CKeySATPathCFeCanc    , PathCFeCanc    );
      PathCFeEnvio              := ini.ReadString(  CSecSAT, CKeySATPathCFeEnvio   , PathCFeEnvio   );
      PrefixoArqCFe             := ini.ReadString(  CSecSAT, CKeySATPrefixoArqCFe  , PrefixoArqCFe  );
      PrefixoArqCFeCanc         := ini.ReadString(  CSecSAT, CKeySATPrefixoArqCFeCanc  , PrefixoArqCFeCanc   );
    end;

    with SAT.SATImpressao.SATExtrato do
    begin
      MostrarStatus          := ini.ReadBool(  CSecSATExtrato, CKeySATExtMostrarStatus          , MostrarStatus         );
      ParamsString           := ini.ReadString(  CSecSATExtrato, CKeySATExtParamsString           , ParamsString          );
      ImprimeDescAcrescItem  := ini.ReadBool(    CSecSATExtrato, CKeySATExtImprimeDescAcrescItem  , ImprimeDescAcrescItem );
      ImprimeEmUmaLinha      := ini.ReadBool(    CSecSATExtrato, CKeySATExtImprimeEmUmaLinha      , ImprimeEmUmaLinha     );
      ImprimeChaveEmUmaLinha := ini.ReadInteger( CSecSATExtrato, CKeySATExtImprimeChaveEmUmaLinha , ImprimeChaveEmUmaLinha);
      UsaCodigoEanImpressao  := ini.ReadBool(    CSecSATExtrato, CKeySATExtUsaCodigoEanImpressao  , UsaCodigoEanImpressao );
      ImprimeQRCodeLateral   := Ini.ReadBool(    CSecSATExtrato, CKeySATExtQRCodeLateral          , ImprimeQRCodeLateral);
      ImprimeLogoLateral     := Ini.ReadBool(    CSecSATExtrato, CKeySATExtLogoLateral            , ImprimeLogoLateral);
      ExtratoDecimaisQTD     := Ini.ReadInteger( CSecSATExtrato, CKeySATExtDecimaisQTD            , ExtratoDecimaisQTD);
      ExtratoDecimaisValor   := Ini.ReadInteger( CSecSATExtrato, CKeySATExtDecimaisValor          , ExtratoDecimaisValor);
      ExtratoMaskQTD         := Ini.ReadString( CSecSATExtrato,  CKeySATExtMaskQTD                , ExtratoMaskQTD);
      ExtratoMaskValor       := Ini.ReadString( CSecSATExtrato,  CKeySATExtMaskValor              , ExtratoMaskValor);
      FormatoDecimal         := Ini.ReadInteger( CSecSATExtrato, CKeySATExtFormatoDecimal         , FormatoDecimal);

    end;

    with SAT.SATImpressao.SATEmit do
    begin
      CNPJ                   := ini.ReadString( CSecSATEmit,  CKeySATEmitCNPJ           , CNPJ          );
      IE                     := ini.ReadString( CSecSATEmit,  CKeySATEmitIE             , IE            );
      IM                     := ini.ReadString( CSecSATEmit,  CKeySATEmitIM             , IM            );
      RegTributario          := ini.ReadInteger(CSecSATEmit,  CKeySATEmitRegTributario  , RegTributario );
      RegTribISSQN           := ini.ReadInteger(CSecSATEmit,  CKeySATEmitRegTribISSQN   , RegTribISSQN  );
      IndRatISSQN            := ini.ReadInteger(CSecSATEmit,  CKeySATEmitIndRatISSQN    , IndRatISSQN   );
    end;

    with SAT.SATImpressao.SATFortes do
    begin
      UsarFortes            := ini.ReadBool   (CSecSATFortes,  CKeySATFortesUsarFortes        , UsarFortes    );
      Largura               := ini.ReadInteger(CSecSATFortes,  CKeySATFortesLargura           , Largura       );
      MargemTopo            := ini.ReadInteger(CSecSATFortes,  CKeySATFortesMargemTopo        , MargemTopo    );
      MargemFundo           := ini.ReadInteger(CSecSATFortes,  CKeySATFortesMargemFundo       , MargemFundo   );
      MargemEsquerda        := ini.ReadInteger(CSecSATFortes,  CKeySATFortesMargemEsquerda    , MargemEsquerda);
      MargemDireita         := ini.ReadInteger(CSecSATFortes,  CKeySATFortesMargemDireita     , MargemDireita );
      Preview               := ini.ReadBool   (CSecSATFortes,  CKeySATFortesPreview           , Preview       );
    end;

    with SAT.SATImpressao.SATPrinter do
    begin
      Name                  := ini.ReadString(CSecSATPrinter, CKeySATPrinterName, Name);
    end;

    with SAT.SATRede do
    begin
      tipoInter             := ini.ReadInteger(CSecSATRede,  CKeySATRedetipoInter   , tipoInter   );
      tipoLan               := ini.ReadInteger(CSecSATRede,  CKeySATRedetipoLan     , tipoLan     );
      SSID                  := ini.ReadString (CSecSATRede,  CKeySATRedeSSID        , SSID        );
      seg                   := ini.ReadInteger(CSecSATRede,  CKeySATRedeseg         , seg         );
      codigo                := ini.ReadString (CSecSATRede,  CKeySATRedecodigo      , codigo      );
      lanIP                 := ini.ReadString (CSecSATRede,  CKeySATRedelanIP       , lanIP       );
      lanMask               := ini.ReadString (CSecSATRede,  CKeySATRedelanMask     , lanMask     );
      lanGW                 := ini.ReadString (CSecSATRede,  CKeySATRedelanGW       , lanGW       );
      lanDNS1               := ini.ReadString (CSecSATRede,  CKeySATRedelanDNS1     , lanDNS1     );
      lanDNS2               := ini.ReadString (CSecSATRede,  CKeySATRedelanDNS2     , lanDNS2     );
      usuario               := ini.ReadString (CSecSATRede,  CKeySATRedeusuario     , usuario     );
      senha                 := ini.ReadString (CSecSATRede,  CKeySATRedesenha       , senha       );
      proxy                 := ini.ReadInteger(CSecSATRede,  CKeySATRedeproxy       , proxy       );
      proxy_ip              := ini.ReadString (CSecSATRede,  CKeySATRedeproxy_ip    , proxy_ip    );
      proxy_porta           := ini.ReadInteger(CSecSATRede,  CKeySATRedeproxy_porta , proxy_porta );
      proxy_user            := ini.ReadString (CSecSATRede,  CKeySATRedeproxy_user  , proxy_user  );
      proxy_senha           := ini.ReadString (CSecSATRede,  CKeySATRedeproxy_senha , proxy_senha );
    end;

    with SAT.SATSwH do
    begin
      CNPJ                  := ini.ReadString(CSecSATSwH, CKeySATSwHCNPJ,       CNPJ);
      Assinatura            := ini.ReadString(CSecSATSwH, CKeySATSwHAssinatura, Assinatura);
    end;

    with SAT.SATEmail do
    begin
      AssuntoSAT            := ini.ReadString(CSecSATEmail, CKeySATEmailAssunto, AssuntoSAT);
      MensagemSAT           := ini.ReadString(CSecSATEmail, CKeySATEmailMensagem, MensagemSAT);
    end;

    with IntegradorFiscal do
    begin
      Input                 :=  ini.ReadString(CSecSATIntegrador, CKeySATIntegradorInput,       Input);
      Output                :=  ini.ReadString(CSecSATIntegrador, CKeySATIntegradorOutput,      Output);
      Timeout               :=  ini.ReadInteger(CSecSATIntegrador, CKeySATIntegradorTimeout,    Timeout);
    end;

    with PosPrinter do
    begin
      Modelo            := ini.ReadInteger(CSecPosPrinter,  CKeyPosPrinterModelo             , Modelo             );
      Porta             := ini.ReadString( CSecPosPrinter,  CKeyPosPrinterPorta              , Porta              );
      Colunas           := ini.ReadInteger(CSecPosPrinter,  CKeyPosPrinterColunas            , Colunas            );
      EspacoEntreLinhas := ini.ReadInteger(CSecPosPrinter,  CKeyPosPrinterEspacoEntreLinhas  , EspacoEntreLinhas  );
      LinhasBuffer      := ini.ReadInteger(CSecPosPrinter,  CKeyPosPrinterLinhasBuffer       , LinhasBuffer       );
      LinhasPular       := ini.ReadInteger(CSecPosPrinter,  CKeyPosPrinterLinhasPular        , LinhasPular        );
      PaginaDeCodigo    := ini.ReadInteger(CSecPosPrinter,  CKeyPosPrinterPaginaDeCodigo     , PaginaDeCodigo     );
      ControlePorta     := ini.ReadBool(   CSecPosPrinter,  CKeyPosPrinterControlePorta      , ControlePorta      );
      CortarPapel       := ini.ReadBool(   CSecPosPrinter,  CKeyPosPrinterCortarPapel        , CortarPapel        );
      TraduzirTags      := ini.ReadBool(   CSecPosPrinter,  CKeyPosPrinterTraduzirTags       , TraduzirTags       );
      IgnorarTags       := ini.ReadBool(   CSecPosPrinter,  CKeyPosPrinterIgnorarTags        , IgnorarTags        );
      ArqLog            := ini.ReadString( CSecPosPrinter,  CKeyPosPrinterArqLog             , ArqLog             );
      SerialParams      := ini.ReadString( CSecPosPrinter,  CKeyPosPrinterSerialParams       , SerialParams       );
    end;

    with PosPrinter.CodigoBarras do
    begin
      Largura:=  ini.ReadInteger(CSecBarras, CKeyBarrasLargura,Largura);
      Altura :=  ini.ReadInteger(CSecBarras, CKeyBarrasAltura ,Altura );
      HRI    :=  ini.ReadBool(   CSecBarras, CKeyBarrasHRI    ,HRI    );
    end;

    with PosPrinter.QRCode do
    begin
      Tipo          :=  ini.ReadInteger(CSecQRCode, CKeyQRCodeTipo            , Tipo           );
      LarguraModulo :=  ini.ReadInteger(CSecQRCode, CKeyQRCodeLarguraModulo   , LarguraModulo  );
      ErrorLevel    :=  ini.ReadInteger(CSecQRCode, CKeyQRCodeErrorLevel      , ErrorLevel     );
    end;

    with PosPrinter.Logo do
    begin
      Imprimir :=  ini.ReadBool   (CSecLogo, CKeyLogoImprimir  , Imprimir );
      KC1      :=  ini.ReadInteger(CSecLogo, CKeyLogoKC1       , KC1      );
      KC2      :=  ini.ReadInteger(CSecLogo, CKeyLogoKC2       , KC2      );
      FatorX   :=  ini.ReadInteger(CSecLogo, CKeyLogoFatorX    , FatorX   );
      FatorY   :=  ini.ReadInteger(CSecLogo, CKeyLogoFatorY    , FatorY   );
    end;

    with PosPrinter.Gaveta do
    begin
      TempoON   :=      ini.ReadInteger(CSecGaveta, CKeyGavetaTempoON    , TempoON   );
      TempoOFF  :=      ini.ReadInteger(CSecGaveta, CKeyGavetaTempoOFF   , TempoOFF  );
      SinalInvertido := ini.ReadBool(CSecGaveta, CKeyGavSinalInvertido, SinalInvertido  );
    end;

    with BOLETO do
    begin
      Nome                   :=  ini.ReadString( CSecBOLETO, CKeyBOLETONome,        ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteNome, '') );
      CNPJCPF                :=  ini.ReadString( CSecBOLETO, CKeyBOLETOCNPJCPF,     ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteCNPJCPF, '') );
      Logradouro             :=  ini.ReadString( CSecBOLETO, CKeyBOLETOLogradouro,  ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteLogradouro, '') );
      Numero                 :=  ini.ReadString( CSecBOLETO, CKeyBOLETONumero,      ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteNumero, '') );
      Bairro                 :=  ini.ReadString( CSecBOLETO, CKeyBOLETOBairro,      ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteBairro, '') );
      CodCidade              :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOCodCidade,   ini.ReadInteger( CSecBOLETO, CKeyBOLETOCodCidade, 0)  );
      Cidade                 :=  ini.ReadString( CSecBOLETO, CKeyBOLETOCidade,      ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteCidade, '') );
      CEP                    :=  ini.ReadString( CSecBOLETO, CKeyBOLETOCEP,         ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteCEP, '') );
      Complemento            :=  ini.ReadString( CSecBOLETO, CKeyBOLETOComplemento, ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteComplemento, '') );
      UF                     :=  ini.ReadString( CSecBOLETO, CKeyBOLETOUF,          ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteUF, '') );
    end;

    with Boleto.Conta do
    begin
      RespEmis               :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETORespEmis,        ini.ReadInteger(CSecBOLETO, CKeyBOLETOCedenteRespEmis, 0) );
      Pessoa                 :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOPessoa,          ini.ReadInteger(CSecBOLETO, CKeyBOLETOCedentePessoa, 1 ) );
      Modalidade             :=  ini.ReadString( CSecBOLETO, CKeyBOLETOModalidade,      ini.ReadString( CSecBOLETO, CKeyBOLETOCedenteModalidade, ''  ) );
      Convenio               :=  ini.ReadString( CSecBOLETO, CKeyBOLETOConvenio,        ini.ReadString( CSecBOLETO, CKeyBOLETOCedenteConvenio, '' ) );
      Banco                  :=  Ini.ReadInteger(CSecBOLETO, CKeyBOLETOBanco,            Banco                  );
      Conta                  :=  ini.ReadString( CSecBOLETO, CKeyBOLETOConta,            Conta                  );
      DigitoConta            :=  ini.ReadString( CSecBOLETO, CKeyBOLETODigitoConta,      DigitoConta            );
      Agencia                :=  ini.ReadString( CSecBOLETO, CKeyBOLETOAgencia,          Agencia                );
      DigitoAgencia          :=  ini.ReadString( CSecBOLETO, CKeyBOLETODigitoAgencia,    DigitoAgencia          );
      DigitoAgenciaConta     :=  ini.ReadString( CSecBOLETO, CKeyBOLETODigitoAgenciaConta, DigitoAgenciaConta   );
      CodCedente             :=  ini.ReadString( CSecBOLETO, CKeyBOLETOCodCedente,       CodCedente             );
      LocalPagamento         :=  ini.ReadString( CSecBOLETO, CKeyBOLETOLocalPagamento,   LocalPagamento         );
      CodigoOperacao         :=  ini.ReadString( CSecBOLETO, CKeyBOLETOCodigoOperacao,   CodigoOperacao         );
    end;

    with BOLETO.PIX do
      begin
        ChavePix                := ini.ReadString(CSecBOLETO,CKeyBOLETOChavePix, '');
        TipoChavePix         := ini.ReadInteger(CSecBOLETO,CKeyBOLETOTipoChavePix, 0);
      end;

    with BOLETO.Layout do
    begin
      DirLogos               :=  ini.ReadString( CSecBOLETO, CKeyBOLETODirLogos,         DirLogos               );
      Copias                 :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOCopias,           Copias                 );
      Preview                :=  Ini.ReadBool(   CSecBOLETO, CKeyBOLETOPreview,          Preview                );
      Progresso              :=  ini.ReadBool(   CSecBOLETO, CKeyBOLETOProgresso,        Progresso              );
      Setup                  :=  ini.ReadBool(   CSecBOLETO, CKeyBOLETOSetup,            Setup                  );
      AlteraEscala           :=  ini.ReadBool(   CSecBOLETO, CKeyBOLETOAlteraEscala,     AlteraEscala           );
      Escala                 :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOEscala,           Escala                 );
      Layout                 :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOLayout,           Layout                 );
      Filtro                 :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOFiltro,           Filtro                 );
      DirArquivoBoleto       :=  ini.ReadString( CSecBOLETO, CKeyBOLETODirArquivoBoleto, DirArquivoBoleto       );
      Impressora             :=  Ini.ReadString( CSecBOLETO, CKeyBOLETOImpressora,       Impressora             );
      NomeArquivoBoleto      :=  Ini.ReadString( CSecBOLETO, CKeyBOLETONomeArquivoBoleto, NomeArquivoBoleto     );
      TipoMotorRelatorio     :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOTipoMotorRelatorio,TipoMotorRelatorio    );
      MargemInferior         :=  ini.ReadFloat(CSecBOLETO,   CKeyBOLETOMargemInferior,    MargemInferior        );
      MargemSuperior         :=  ini.ReadFloat(CSecBOLETO,   CKeyBOLETOMargemSuperior,    MargemSuperior        );
      MargemEsquerda         :=  ini.ReadFloat(CSecBOLETO,   CKeyBOLETOMargemEsquerda,    MargemEsquerda        );
      MargemDireita          :=  ini.ReadFloat(CSecBOLETO,   CKeyBOLETOMargemDireita,     MargemDireita        );
    end;

    with BOLETO.RemessaRetorno do
    begin
      DirArquivoRemessa      :=  ini.ReadString( CSecBOLETO, CKeyBOLETODirArquivoRemessa,  DirArquivoRemessa      );
      DirArquivoRetorno      :=  ini.ReadString( CSecBOLETO, CKeyBOLETODirArquivoRetorno,  DirArquivoRetorno      );
      CNAB                   :=  ini.ReadInteger(CSecBOLETO, CKeyBOLETOCNAB,               CNAB                   );
      LerCedenteRetorno      :=  Ini.ReadBool(   CSecBOLETO, CKeyBOLETOLerCedenteRetorno,  LerCedenteRetorno      );
      CodTransmissao         :=  ini.ReadString( CSecBOLETO, CKeyBOLETOCodTransmissao,     ini.ReadString( CSecBOLETO,CKeyBOLETOCedenteCodTransmissao,'') );
      RemoveAcentos          :=  Ini.ReadBool(   CSecBOLETO, CKeyBOLETORemoveAcentos,      RemoveAcentos      );
      PrefixArqRemessa       :=  Ini.ReadString( CSecBOLETO, CKeyBoletoPrefixArqRemessa,   PrefixArqRemessa );
      VersaoArquivo          :=  ini.ReadString( CSecBOLETO, CKeyBOLETOVersaoArquivo,       VersaoArquivo);
      VersaoLote             :=  Ini.ReadString( CSecBOLETO, CKeyBOLETOVersaoLote,          VersaoLote);
    end;

    with BOLETO.Relatorio do
    begin
      MostraPreviewRelRetorno:=  Ini.ReadBool(   CSecBOLETO, CKeyBOLETOMostraPreviewRelRetorno,   MostraPreviewRelRetorno);
      LogoEmpresa            :=  Ini.ReadString( CSecBOLETO, CKeyBOLETOLogoEmpresa,               LogoEmpresa            );
    end;

    With BOLETO.Email do
    begin
      EmailAssuntoBoleto     :=  ini.ReadString( CSecBOLETO, CKeyBOLETOEmailAssuntoBoleto,        EmailAssuntoBoleto     );
      EmailMensagemBoleto    :=  ini.ReadString( CSecBOLETO, CKeyBOLETOEmailMensagemBoleto,       EmailMensagemBoleto    );
      EmailFormatoHTML       :=  ini.ReadBool(   CSecBOLETO, CKeyBOLETOEmailFormatoHTML,          EmailFormatoHTML    );
    end;

    with BOLETO.WS.CedenteWS do
    begin
      ClientID := ini.ReadString( CSecBOLETO, CKeyBOLETOClientID, ClientID );
      ClientSecret := ini.ReadString( CSecBOLETO, CKeyBOLETOClientSecret, ClientSecret);
      KeyUser := ini.ReadString( CSecBOLETO, CKeyBOLETOKeyUser, KeyUser);
      Scope := ini.ReadString( CSecBOLETO, CKeyBOLETOScope, Scope);
      IndicadorPix := ini.ReadBool( CSecBOLETO, CKeyBOLETOIndicadorPix, IndicadorPix);
    end;

    with BOLETO.WS.Config do
    begin
      LogNivel := TNivelLog(ini.ReadInteger( CSecBOLETO, CKeyBOLETOLogNivel, Integer(LogNivel)));
      PathGravarRegistro := ini.ReadString( CSecBOLETO, CKeyBOLETOPathGravarRegistro, PathGravarRegistro);
      NomeArquivoLog := ini.ReadString( CSecBOLETO, CKeyBOLETONomeArquivoLog, NomeArquivoLog);
    end;

    with BOLETO.WS.Config.SSL do
    begin
      Ambiente := ini.ReadInteger( CSecBOLETO, CKeyBOLETOAmbiente, Ambiente);
      Operacao :=  ini.ReadInteger( CSecBOLETO, CKeyBOLETOOperacao, Operacao);
      Proxy_Host := ini.ReadString( CSecBOLETO, CKeyBOLETOProxyHost, Proxy_Host);
      Proxy_Pass := ini.ReadString( CSecBOLETO, CKeyBOLETOProxyPass, Proxy_Pass);
      Proxy_Port := ini.ReadString( CSecBOLETO, CKeyBOLETOProxyPort, Proxy_Port);
      Proxy_User := ini.ReadString( CSecBOLETO, CKeyBOLETOProxyUser, Proxy_User);
      CryptLib := ini.ReadInteger( CSecBOLETO, CKeyBOLETOCryptLib, CryptLib);
      HttpLib := ini.ReadInteger( CSecBOLETO, CKeyBOLETOHttpLib, HttpLib);
      SSLType := ini.ReadInteger( CSecBOLETO, CKeyBOLETOSSLType, SSLType);
      XmlSignLib := ini.ReadInteger( CSecBOLETO, CKeyBOLETOXmlSignLib, XmlSignLib);
      TimeOut := ini.ReadInteger( CSecBOLETO, CKeyBOLETOTimeOut, TimeOut);
      CertificadoHTTP := ini.ReadBool( CSecBOLETO, CKeyBOLETOCertificadoHTTP, CertificadoHTTP);
      VersaoDF := ini.ReadString( CSecBOLETO, CKeyBOLETOVersaoDF, VersaoDF);
      ArquivoKEY := ini.ReadString( CSecBOLETO, CKeyBOLETOArquivoKEY, ArquivoKEY);
      ArquivoCRT := ini.ReadString( CSecBOLETO, CKeyBOLETOArquivoCRT, ArquivoCRT);
    end;

    with NFSE do
    begin
      LayoutProvedor := ini.ReadInteger( CSecNFSE, CKeyNFSELayoutProvedor, LayoutProvedor);
      CodigoMunicipio := ini.ReadInteger( CSecNFSE, CKeyNFSECodigoMunicipio, CodigoMunicipio);
      NomeMunicipio := ini.ReadString( CSecNFSE, CKeyNFSENomeMunicipio, NomeMunicipio);
      UFMunicipio := ini.ReadString( CSecNFSE, CKeyNFSEUFMunicipio, UFMunicipio);
      Usuario := ini.ReadString( CSecNFSE, CKeyNFSeUsuario, Usuario);
      Senha := ini.ReadString( CSecNFSE, CKeyNFSeSenha, Senha);
      ChaveAcesso := ini.ReadString( CSecNFSE, CKeyNFSeChaveAcesso, ChaveAcesso);
      ChaveAutenticacao := ini.ReadString( CSecNFSE, CKeyNFSeChaveAutenticacao, ChaveAutenticacao);
      FraseSecreta := ini.ReadString( CSecNFSE, CKeyNFSeFraseSecreta, FraseSecreta);
      CNPJEmitente := ini.ReadString( CSecNFSE, CKeyNFSeCNPJEmitente, CNPJEmitente);
      IMEmitente := ini.ReadString( CSecNFSE, CKeyNFSeIMEmitente, IMEmitente);
      NomeEmitente := ini.ReadString( CSecNFSE, CKeyNFSeNomeEmitente, NomeEmitente);
      MontarAutoPathSchema := ini.ReadBool( CSecBOLETO, CKeyNFSeMontarAutoPathSchema, MontarAutoPathSchema);
      ConsultarLoteAposEnvio := ini.ReadBool( CSecBOLETO, CKeyNFSeConsultarLoteAposEnvio, ConsultarLoteAposEnvio);
      ConsultarAposCancelar := ini.ReadBool( CSecBOLETO, CKeyNFSeConsultarAposCancelar, ConsultarAposCancelar);
      NomePrefeitura := ini.ReadString( CSecNFSE, CKeyNFSeNomePrefeitura, NomePrefeitura);
      CNPJPrefeitura := ini.ReadString( CSecNFSE, CKeyNFSeCNPJPrefeitura, CNPJPrefeitura);
      NomeLongoNFSe  := ini.ReadBool( CSecNFSE, CKeyNFSeNomeLongoNFSe, NomeLongoNFSe);
    end;

  finally
    Ini.Free;

  end;

end;

procedure TMonitorConfig.CarregarArquivo;
var
  FS: TFileStream;
begin
  ValidarNomeCaminho(False);
  DefinirValoresPadrao;

  FS := TFileStream.Create(FNomeArquivo, fmOpenRead);
  try
    CarregarArquivoMemoria(FS);
  finally
    FS.Free;
  end;

end;

procedure TMonitorConfig.DefinirValoresPadrao;
begin
  with ACBrMonitor do
  Begin
    Modo_TCP                  := False;
    Modo_TXT                  := False;
    MonitoraPasta             := False;
    TCP_Porta                 := 3434;
    TCP_TimeOut               := 10000;
    Converte_TCP_Ansi         := False;
    TXT_Entrada               := 'ENT.TXT';
    TXT_Saida                 := 'SAI.TXT';
    Converte_TXT_Entrada_Ansi := False;
    Converte_TXT_Saida_Ansi   := False;
    Intervalo                 := 50;
    Gravar_Log                := True;
    Arquivo_Log               := 'LOG.TXT';
    Linhas_Log                := 0;
    Comandos_Remotos          := False;
    Uma_Instancia             := True;
    MostraAbas                := False;
    {$IFDEF LINUX}
    MostrarNaBarraDeTarefas   := True;
    {$ELSE}
    MostrarNaBarraDeTarefas   := False;
    {$ENDIF}
    RetirarAcentosNaResposta  := False;
    MostraLogEmRespostasEnviadas:= False;
    HashSenha                 := '';
    Senha                     := '';
    VersaoSSL                 := 0;
    TipoResposta              := 0;
  end;

  with ECF do
  begin
    Modelo                    := 0;
    Porta                     := 'Procurar';
    SerialParams              := '';
    Timeout                   := 3;
    IntervaloAposComando      := 100;
    MaxLinhasBuffer           := 0;
    PaginaCodigo              := 0;
    LinhasEntreCupons         := 0;
    ArredondamentoPorQtd      := False;
    ArredondamentoItemMFD     := False;
    DescricaoGrande           := True;
    GavetaSinalInvertido      := False;
    IgnorarTagsFormatacao     := False;
    ControlePorta             := False;
    ArqLog                    := '';
  end;

  with CHQ do
  begin
    Modelo                    := 0;
    Porta                     := '';
    SerialParams              := '';
    VerificaFormulario        := False;
    Favorecido                := '';
    Cidade                    := '';
    PathBemafiINI             := '';
  end;

  with GAV do
  begin
    Modelo                    := 0;
    Porta                     := '';
    StringAbertura            := '';
    AberturaIntervalo         := 5000;
    AcaoAberturaAntecipada    := 1;
  end;

  with DIS do
  begin
    Modelo                    := 0;
    Porta                     := '';
    Intervalo                 := 300;
    Passos                    := 1;
    IntervaloEnvioBytes       := 3;
  end;

  with LCB do
  begin
    Porta                     := 'Sem Leitor';
    Intervalo                 := 100;
    SufixoLeitor              := '#13';
    ExcluirSufixo             := False;
    PrefixoAExcluir           := '';
    SufixoIncluir             := '';
    Dispositivo               := '';
    Teclado                   := True;
    Device                    := '';
  end;

  with RFD do
  begin
    GerarRFD                  := False;
    DirRFD                    := '';
    IgnoraECF_MFD             := True;
  end;

  with BAL do
  begin
    Modelo                    := 0;
    Porta                     := '';
    Intervalo                 := 200;
    ArqLog                    := '';
    Device                    := '';
  end;

  with ETQ do
  begin
    Modelo                    := 0;
    Porta                     := '';
    DPI                       := 0;
    LimparMemoria             := True;
    Temperatura               := 10;
    Velocidade                := -1;
    BackFeed                  := -1;
    MargemEsquerda            := 10;
    Origem                    := -1;
    Unidade                   := -1;
    Copias                    := 1;
    Avanco                    := 0;
  end;

  with CEP do
  begin
    WebService                := 0;
    Chave_BuscarCEP           := '';
    Proxy_Host                := '';
    Proxy_Port                := '';
    Proxy_User                := '';
    Proxy_Pass                := '';
    IBGEAcentos               := False;
  end;

  with ConsultaCNPJ do
  begin
    Provedor := 0;
    Usuario:='';
    Senha:='';
    Proxy_Host                := '';
    Proxy_Port                := '';
    Proxy_User                := '';
    Proxy_Pass                := '';
  end;

  with TC do
  begin
    Modelo                    := 0;
    TCP_Porta                 := 6500;
    Arq_Precos                := 'PRICETAB.TXT';
    Nao_Econtrado             := 'PRODUTO|NAO ENCONTRADO';
  end;

  with SEDEX do
  begin
    Contrato                  := '';
    SenhaSedex                := '';
  end;

  with NCM do
  begin
    DirNCMSalvar              := '';
  end;

  with Email do
  begin
    NomeExibicao              := '';
    Endereco                  := '';
    Email                     := '';
    Usuario                   := '';
    Senha                     := '';
    Porta                     := 0;
    ExigeSSL                  := False;
    ExigeTLS                  := False;
    Confirmacao               := False;
    SegundoPlano              := False;
    Codificacao               := '';
    HTML                      := False;
    AttemptsMail              := 3;
    TimeOutMail               := 0;
    SSLType                   := 0;
  end;

  with DFe do
  begin
    IgnorarComandoModoEmissao := False;
    ModoXML                   := False;
    RetirarAcentos            := True;
    RetirarEspacos            := False;
    Gravar_Log_Comp           := False;
    Arquivo_Log_Comp          := 'LOG_COMP.TXT';
    Linhas_Log_Comp           := 0;
    ArquivoWebServices        := AcertaPath( 'ACBrNFeServicos.ini' );
    ArquivoWebServicesCTe     := AcertaPath( 'ACBrCTeServicos.ini' );
    ArquivoWebServicesMDFe    := AcertaPath( 'ACBrMDFeServicos.ini' );
    ArquivoWebServicesGNRe    := AcertaPath( 'ACBrGNREServicos.ini' );
    ArquivoWebServiceseSocial := AcertaPath( 'ACBreSocialServicos.ini' );
    ArquivoWebServicesReinf   := AcertaPath( 'ACBrReinfServicos.ini' );
    ArquivoWebServicesBPe     := AcertaPath( 'ACBrBPeServicos.ini' );
    ArquivoWebServicesNFSe    := AcertaPath( 'ACBrNFSeXServicos.ini' );
    ValidarDigest             := True;
    TimeoutWebService         := 15;
  end;

  with DFe.Certificado do
  begin
    SSLLib                    := 4;
    CryptLib                  := 0;
    HttpLib                   := 0;
    XmlSignLib                := 0;
    SSLType                   := 0;
    ArquivoPFX                := '';
    URLPFX                    := '';
    NumeroSerie               := '';
    Senha                     := '';
    ExibeRazaoSocialCertificado:= False;
    VerificarValidade         := False;
  end;

  with DFe.Impressao.Geral do
  begin
    DANFE                     := 0;
    FormaEmissao              := 0;
    Logomarca                 := '';
    LogoMarcaNFCeSAT          := '';
    Salvar                    := True;
    PathSalvar                := AcertaPath('Logs' );
    Impressora                := '0';
  end;

  with DFe.WebService do
  begin
    Versao                    := '4.00';
    VersaoCTe                 := '4.00';
    VersaoMDFe                := '3.00';
    VersaoeSocial             := 'S01_00_00';
    VersaoReinf               := '1_03_02';
    VersaoQRCode              := '0';
    VersaoBPe                 := '1.00';
    VersaoGNRe                := '1.00';
    FormaEmissaoNFe           := 0;
    FormaEmissaoCTe           := 0;
    FormaEmissaoGNRe          := 0;
    FormaEmissaoMDFe          := 0;
    FormaEmissaoBPe           := 0;
    Ambiente                  := 1;
    UF                        := 'SP';
    AjustarAut                := False;
    Aguardar                  := '0';
    Tentativas                := '5';
    Intervalo                 := '0';
    TimeZoneMode              := 0;
    TimeZoneStr               := '';
    CamposFatObrig            := True;
    TagRejeicao938            := 0;
  end;

  with DFe.WebService.Proxy do
  begin
    Host                      := '';
    Porta                     := '';
    User                      := '';
    Pass                      := '';
  end;

  with DFe.WebService.NFCe do
  begin
    IdToken                   := '';
    Token                     := '';
    TagQrCode                 := True;
    UsarIntegrador            := False;
  end;

  with DFe.Email do
  begin
    MensagemNFe               := '';
    AssuntoNFe                := '';
    MensagemCTe               := '';
    AssuntoCTe                := '';
    MensagemMDFe              := '';
    AssuntoMDFe               := '';
    MensagemBPe               := '';
    AssuntoBPe                := '';
    MensagemNFSe              := '';
    AssuntoNFSe               := '';
  end;

  with DFe.WebService.NFe do
  begin
    CNPJContador              := '';
  end;

  with DFe.Impressao.NFCe.Emissao do
  begin
    Modelo                    := CEmissaoFortes;
    ModoImpressaoEvento       := 0;
    ImprimirItem1Linha        := True;
    ImprimirDescAcresItem     := True;
    ImpressoraPadrao          := '0';
    QRCodeLateral             := True;
    UsaCodigoEanImpressao     := False;
    ImprimeNomeFantasia       := False;
    ImprimeTributos           := 1;
    ExibeTotalTributosItem    := False;
    LogoLateral               := False;
    ImprimeItens              := True;
  end;

  with DFE.Impressao.NFCe.Emissao.DANFCe do
  begin
    MargemInf                 :=  0.8;
    MargemSup                 :=  0.8;
    MargemDir                 :=  0.51;
    MargemEsq                 :=  0.6;
    LarguraBobina             :=  302;
    ImprimeNNFFormatadoNFCe    := True;
  end;

  with DFE.Impressao.NFCe.Emissao.DANFCeTipoPagto do
  begin
    tipo                      := True;
    Bandeira                  := True;
    Autorizacao               := False;
  end;

  with FonteLinha do
  begin
    Name                 := 'Lucida Console';
    Size                 := 7;
    Color                := clBlack;
    Style                := [];
  end;

  with DFE.Impressao.DANFE do
  begin
    Modelo                    :=  0;
    TamanhoPapel              :=  0;
    Site                      :=  '';
    Email                     :=  '';
    Fax                       :=  '';
    ImpDescPorc               :=  True;
    MostrarPreview            :=  False;
    Copias                    :=  1;
    CopiasNFCe                :=  1;
    LarguraCodigoProduto      :=  40;
    EspacoEntreProdutos       :=  11;
    FonteRazao                :=  8;
    FonteEndereco             :=  7;
    FonteCampos               :=  8;
    FonteAdicionais           :=  8;
    AlturaCampos              :=  30;
    Margem                    :=  7;
    MargemSup                 :=  7;
    MargemDir                 :=  5;
    MargemEsq                 :=  9;
    PathPDF                   :=  AcertaPath('PDF');
    DecimaisQTD               :=  2;
    DecimaisValor             :=  2;
    ExibeResumo               :=  False;
    TextoResumoCanhoto        := '';
    ImprimirTributosItem      :=  False;
    ImprimirValLiq            :=  False;
    UNComercialETributavel    :=  0;
    PreImpresso               :=  False;
    MostrarStatus             :=  False;
    ExibirEAN                 :=  False;
    ExibirCampoFatura         :=  True;
    ExpandirLogo              :=  False;
    Fonte                     :=  0;
    LocalCanhoto              :=  0;
    LayoutCanhoto             :=  0;
    QuebrarLinhasDetalheItens :=  False;
    ImprimirDetalhamentoEspecifico := True;
    ImprimirDadosDocReferenciados := True;
    ExibirBandInforAdicProduto := 0;
    ImprimeDescAcrescItemNFe   := 0;
    ImprimirCampoFormaPagamento:= 0;
    LogoEmCima                 := False;
    ImprimeInscSuframa         := True;
    ImprimeNNFFormatadoNFe     := True;
    ExpandirDadosAdicionaisAuto := False;
    ImprimeContinuacaoDadosAdicionaisPrimeiraPagina:= False;
    ImprimeXPedNitemPed        := False;
  end;

  with DFe.Impressao.DACTE do
  begin
    TamanhoPapel              :=  0;
  end;

  with DFe.Impressao.DAMFE do
  begin
    ExibirMunicipioDescarregamento :=  False;
  end;

  with DFe.Diretorios do
  begin
    Salvar                     :=  True;
    PastaMensal                :=  True;
    AddLiteral                 :=  True;
    EmissaoPathNFe             :=  True;
    SalvarCCeCanPathEvento     :=  True;
    SepararPorCNPJ             :=  True;
    SepararPorModelo           :=  True;
    SalvarApenasNFesAutorizadas:=  False;
    AtualizarXMLCancelado      :=  True;
    NormatizarMunicipios       :=  True;
    UsarSeparadorPathPDF       :=  True;
    SepararPorNome             :=  False;
    PathNFe                    :=  AcertaPath('Arqs');
    PathInu                    :=  AcertaPath('Arqs');
    PathDPEC                   :=  AcertaPath('Arqs');
    PathEvento                 :=  AcertaPath('Arqs');
    PathArqTXT                 :=  AcertaPath('TXT');
    PathDownload               :=  AcertaPath('Arqs');
    PathSchemasDFe             :=  AcertaPath('Schemas');
  end;

  with DFe.RespTecnico do
  begin
    CSRT                       := '';
    idCSRT                     := '';
  end;

  with DFe.ESocial do
  begin
    IdEmpregador               := '';
    IdTransmissor              := '';
    TipoEmpregador             := 'tePessoaJuridica';
  end;

  with DFe.Reinf do
  begin
    IdContribuinte             := '';
    IdTransmissor              := '';
    TipoContribuinte           := 'tcPessoaJuridica';
  end;

  with SAT do
  begin
    Modelo                    := 0;
    Marca                     := '';
    ArqLog                    := 'ACBrSAT.log';
    NomeDLL                   := AcertaPath('SAT\Emulador\SAT.DLL');
    CodigoAtivacao            := '123456';
    CodigoUF                  := '35';
    NumeroCaixa               := 1;
    Ambiente                  := 1;
    PaginaDeCodigo            := 0;
    versaoDadosEnt            := 0.07;
    FormatarXML               := True;
    PathCFe                   := AcertaPath('Arqs'+PathDelim+'SAT');
    SalvarCFe                 := True;
    SalvarCFeCanc             := True;
    SalvarEnvio               := True;
    SepararPorCNPJ            := True;
    SepararPorMES             := True;
    SepararPorDIA             := False;
    SepararPorANO             := False;
    SepararPorModelo          := False;
    ValidarNumeroSessaoResposta:= True;
    PathCFeCanc               := AcertaPath('Arqs'+PathDelim+'SAT');
    PathCFeEnvio              := AcertaPath('Arqs'+PathDelim+'SAT');
    PrefixoArqCFe             := '';
    PrefixoArqCFeCanc         := '';
  end;

  with SAT.SATImpressao.SATExtrato do
  begin
    MostrarStatus          := False;
    ParamsString           := '';
    ImprimeDescAcrescItem  := True;
    ImprimeEmUmaLinha      := False;
    ImprimeChaveEmUmaLinha := 0;
    UsaCodigoEanImpressao  := False;
    ImprimeQRCodeLateral   := True;
    ImprimeLogoLateral     := True;
    ExtratoDecimaisQTD     := 2;
    ExtratoDecimaisValor   := 2;
    ExtratoMaskQTD         := '0.0000';
    ExtratoMaskValor       := '0.000';
    FormatoDecimal         := 0;
  end;

  with SAT.SATImpressao.SATEmit do
  begin
    CNPJ                   := '';
    IE                     := '';
    IM                     := '';
    RegTributario          := 0;
    RegTribISSQN           := 0;
    IndRatISSQN            := 0;
  end;

  with SAT.SATImpressao.SATFortes do
  begin
    UsarFortes            := True;
    Largura               := 302;
    MargemTopo            := 2;
    MargemFundo           := 4;
    MargemEsquerda        := 2;
    MargemDireita         := 2;
    Preview               := True;
  end;

  with SAT.SATImpressao.SATPrinter do
  begin
    Name                  := '';
  end;

  with SAT.SATRede do
  begin
    tipoInter             := 0;
    tipoLan               := 0;
    SSID                  := '';
    seg                   := 0;
    codigo                := '';
    lanIP                 := '';
    lanMask               := '';
    lanGW                 := '';
    lanDNS1               := '';
    lanDNS2               := '';
    usuario               := '';
    senha                 := '';
    proxy                 := 0;
    proxy_ip              := '';
    proxy_porta           := 0;
    proxy_user            := '';
    proxy_senha           := '';
  end;

  with SAT.SATSwH do
  begin
    CNPJ                  := '';
    Assinatura            := '';
  end;

  with SAT.SATEmail do
  begin
    AssuntoSAT             := '';
    MensagemSAT            := '';
  end;

  with IntegradorFiscal do
  begin
    Input                 := 'c:\Integrador\Input\';
    Output                := 'c:\Integrador\Output\';
    Timeout               := 30;
  end;

  with PosPrinter do
  begin
    Modelo            := 0;
    Porta             := '';
    Colunas           := 48;
    EspacoEntreLinhas := 0;
    LinhasBuffer      := 0;
    LinhasPular       := 4;
    PaginaDeCodigo    := 0;
    ControlePorta     := False;
    CortarPapel       := True;
    TraduzirTags      := True;
    IgnorarTags       := False;
    ArqLog            := '';
    SerialParams      := '';
  end;

  with PosPrinter.CodigoBarras do
  begin
    Largura:=  0;
    Altura :=  0;
    HRI    :=  False;
  end;

  with PosPrinter.QRCode do
  begin
    Tipo          :=  2;
    LarguraModulo :=  4;
    ErrorLevel    :=  0;
  end;

  with PosPrinter.Logo do
  begin
    Imprimir :=  False;
    KC1      :=  48;
    KC2      :=  48;
    FatorX   :=  2;
    FatorY   :=  2;
  end;

  with PosPrinter.Gaveta do
  begin
    TempoON   :=      50;
    TempoOFF  :=      200;
    SinalInvertido := False;
  end;

  with BOLETO do
  begin
    Nome                   :=  '';
    CNPJCPF                :=  '';
    Logradouro             :=  '';
    Numero                 :=  '';
    Bairro                 :=  '';
    CodCidade              :=  0;
    Cidade                 :=  '';
    CEP                    :=  '';
    Complemento            :=  '';
    UF                     :=  '';
  end;

  with Boleto.Conta do
  begin
    RespEmis               :=  -1;
    Pessoa                 :=  -1;
    Modalidade             :=  '';
    Convenio               :=  '';
    Banco                  :=  1;
    Conta                  :=  '';
    DigitoConta            :=  '';
    Agencia                :=  '';
    DigitoAgencia          :=  '';
    DigitoAgenciaConta     :=  '';
    CodCedente             :=  '';
    LocalPagamento         :=  '';
    CodigoOperacao         :=  '';
  end;

  with BOLETO.PIX do
  begin
    ChavePix               :=  '';
    TipoChavePix           :=  0;
  end;


  with BOLETO.Layout do
  begin
    DirLogos               :=  AcertaPath('Logos');
    Copias                 :=  1;
    Preview                :=  True;
    Progresso              :=  True;
    Setup                  :=  True;
    AlteraEscala           :=  True;
    Escala                 := 96;
    Layout                 :=  0;
    Filtro                 :=  0;
    DirArquivoBoleto       :=  '';
    NomeArquivoBoleto      :=  '';
    Impressora             :=  '';
    TipoMotorRelatorio     :=  0;
    MargemInferior         :=  5;
    MargemSuperior         :=  5;
    MargemEsquerda         :=  4;
    MargemDireita          :=  3;
  end;

  with BOLETO.RemessaRetorno do
  begin
    DirArquivoRemessa      :=  '';
    DirArquivoRetorno      :=  '';
    CNAB                   :=  0;
    LerCedenteRetorno      :=  False;
    CodTransmissao         :=  '';
    RemoveAcentos          :=  False;
    PrefixArqRemessa       := '';
  end;

  with BOLETO.Relatorio do
  begin
    MostraPreviewRelRetorno:=  True;
    LogoEmpresa            :=  '';
  end;

  With BOLETO.Email do
  begin
    EmailAssuntoBoleto     :=  '';
    EmailMensagemBoleto    :=  '';
    EmailFormatoHTML       := False;
  end;

  with BOLETO.WS.CedenteWS do
  begin
    ClientID := '';
    ClientSecret := '';
    KeyUser := '';
    Scope := '';
    IndicadorPix := False;
  end;

  with BOLETO.WS.Config do
  begin
    LogNivel:= logNenhum;
    PathGravarRegistro := '';
    NomeArquivoLog := '';
  end;

  with BOLETO.WS.Config.SSL do
  begin
    Ambiente := 1;
    Operacao := 0;
    Proxy_Host := '';
    Proxy_Pass := '';
    Proxy_Port := '';
    Proxy_User := '';
    CryptLib := 0;
    HttpLib := 0;
    SSLType := 0;
    XmlSignLib := 0;
    TimeOut := 30;
    CertificadoHTTP := False;
    VersaoDF := '1.2';
    ArquivoKEY := '';
    ArquivoCRT := '';
  end;

  with NFSE do
  begin
    LayoutProvedor := 0;
    CodigoMunicipio := 0;
    NomeMunicipio := '';
    UFMunicipio := '';
    Usuario := '';
    Senha := '';
    ChaveAcesso := '';
    ChaveAutenticacao := '';
    FraseSecreta := '';
    CNPJEmitente := '';
    IMEmitente := '';
    NomeEmitente := '';
    MontarAutoPathSchema := True;
    ConsultarLoteAposEnvio := True;
    ConsultarAposCancelar := True;
    NomePrefeitura := '';
    CNPJPrefeitura := '';
    NomeLongoNFSe := True;
  end;
end;

procedure TMonitorConfig.ValidarNomeCaminho(Salvar: Boolean);
var
  APath: String;
begin
  if FNomeArquivo = '' then
    raise EDFeConfigException.Create(SErrArqConfigNaoDefinido);

  APath := ExtractFilePath(FNomeArquivo);
  if (APath <> '') and (not DirectoryExists(APath)) then
    raise EDFeConfigException.CreateFmt(SErrDiretorioInvalido, [APath]);

  if (not Salvar) and (not FileExists(FNomeArquivo)) then
    raise EDFeConfigException.Create(SErrArqConfNaoEncontrado);

end;

procedure TMonitorConfig.CriarArquivo;
begin
  DefinirValoresPadrao;
  SalvarArquivo;
end;

procedure TMonitorConfig.DoOnGravarConfig;
begin
  if Assigned(FOnGravarConfig) then
    FOnGravarConfig( Self );
end;


end.

