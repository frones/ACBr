{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz usocdo conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:     2005 Fábio Rogério Baía                     }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

{$mode objfpc}{$H+}

unit ACBrMonitor1;

interface

uses
  {$IFDEF MSWINDOWS} windows, {$ENDIF}
  SysUtils, Classes, Forms, CmdUnit, ACBrECF, ACBrDIS, ACBrGAV, ACBrDevice,
  ACBrCHQ, ACBrLCB, ACBrRFD, Dialogs, ExtCtrls, Menus, Buttons, StdCtrls,
  ComCtrls, Controls, Graphics, Spin, MaskEdit, EditBtn, ACBrBAL, ACBrETQ,
  ACBrPosPrinter, ACBrSocket, ACBrCEP, ACBrIBGE, blcksock, ACBrValidador,
  ACBrGIF, ACBrEAD, ACBrMail, ACBrSedex, ACBrNCMs, ACBrNFe, ACBrNFeDANFeESCPOS,
  ACBrDANFCeFortesFr, ACBrNFeDANFeRLClass, ACBrBoleto, ACBrBoletoFCFortesFr,
  Printers, DbCtrls, DBGrids, SynHighlighterXML, SynMemo, PrintersDlgs, IpHtml,
  pcnConversao, pcnConversaoNFe, pcteConversaoCTe, ACBrSAT,
  ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr, ACBrSATClass, pcnRede,
  ACBrDFeSSL, ACBrGNRE2, ACBrGNReGuiaRLClass, ACBrBlocoX, ACBrMDFe,
  ACBrMDFeDAMDFeRLClass, ACBrCTe, ACBrCTeDACTeRLClass, types, fileinfo,
  ACBrDFeConfiguracoes, ACBrIntegrador, LazHelpCHM, pmdfeConversaoMDFe;

const
  //{$I versao.txt}
  CEstados: array[TACBrECFEstado] of string =
    ('Não Inicializada', 'Desconhecido', 'Livre', 'Venda',
    'Pagamento', 'Relatório', 'Bloqueada', 'Requer Z', 'Requer X',
    'Não Fiscal');
  CBufferMemoResposta = 10000;              { Maximo de Linhas no MemoResposta }
  _C = 'tYk*5W@';
  UTF8BOM : AnsiString = #$EF#$BB#$BF;
  HelpShortcut = 'F1';

type
  TCores = class
    class var
    Buttons: TColor;
    SubButtons: TColor;
    ButtonSelected: TColor;
    SubButtonSelected: TColor;
    ButtonMouseEnter: TColor;
    SubButtonMouseEnter: TColor;
  public
    class constructor Create;
  end;

  TDFeCarregar = (tDFeNFe, tDFeEventoNFe, tDFeInutNFe,
                  tDFeCTe, tDFeEventoCTe, tDFeInutCTe,
                  tDFeMDFe, tDFeEventoMDFe,
                  tDFeGNRe);

  { TFrmACBrMonitor }

  TFrmACBrMonitor = class(TForm)
    ACBrBlocoX1: TACBrBlocoX;
    ACBrBoleto1: TACBrBoleto;
    ACBrBoletoFCFortes1: TACBrBoletoFCFortes;
    ACBrCEP1: TACBrCEP;
    ACBrCTe1: TACBrCTe;
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;
    ACBrEAD1: TACBrEAD;
    ACBrECF1: TACBrECF;
    ACBrGIF1: TACBrGIF;
    ACBrGNRE1: TACBrGNRE;
    ACBrGNREGuiaRL1: TACBrGNREGuiaRL;
    ACBrIBGE1: TACBrIBGE;
    ACBrIntegrador1: TACBrIntegrador;
    ACBrMail1: TACBrMail;
    ACBrMDFe1: TACBrMDFe;
    ACBrMDFeDAMDFeRL1: TACBrMDFeDAMDFeRL;
    ACBrNCMs1: TACBrNCMs;
    ACBrNFe1: TACBrNFe;
    ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrNFeDANFeRL1: TACBrNFeDANFeRL;
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrSAT1: TACBrSAT;
    ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS;
    ACBrSATExtratoFortes1: TACBrSATExtratoFortes;
    ACBrSedex1: TACBrSedex;
    ACBrValidador1: TACBrValidador;
    ApplicationProperties1: TApplicationProperties;
    bBALAtivar: TBitBtn;
    bBALTestar: TBitBtn;
    bBoletoRelatorioRetorno: TBitBtn;
    bBOLLerArqRelatorio: TBitBtn;
    bCEPTestar: TButton;
    bCHQTestar: TBitBtn;
    bDISAnimar: TBitBtn;
    bDISLimpar: TBitBtn;
    bDISTestar: TBitBtn;
    bDownloadLista: TButton;
    bECFAtivar: TBitBtn;
    bECFLeituraX: TBitBtn;
    bECFTestar: TBitBtn;
    bEmailTestarConf: TBitBtn;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btnBoletoRelatorioRetorno: TPanel;
    btnIntegrador: TPanel;
    bvCadastro: TBevel;
    bExecECFTeste: TBitBtn;
    bGAVAbrir: TBitBtn;
    bGAVAtivar: TBitBtn;
    bGAVEstado: TBitBtn;
    bIBGETestar: TButton;
    bImpressora: TButton;
    bInicializar: TButton;
    bbAtivar: TBitBtn;
    bLCBAtivar: TBitBtn;
    bLCBSerial: TBitBtn;
    bNcmConsultar: TButton;
    bRFDID: TButton;
    bRFDINI: TButton;
    bRFDINISalvar: TButton;
    bRFDMF: TBitBtn;
    bRSAeECFc: TButton;
    bRSALoadKey: TButton;
    bRSAPrivKey: TButton;
    bRSAPubKey: TButton;
    bSedexRastrear: TButton;
    bSedexTestar: TButton;
    btAtivarsat: TButton;
    bTCAtivar: TBitBtn;
    btConsultarStatusOPSAT: TButton;
    btnBalanca: TPanel;
    btnBoleto: TPanel;
    btnBoletoCedente: TPanel;
    btnBoletoCont: TPanel;
    btnBoletoConta: TPanel;
    btnBoletoEmail: TPanel;
    btnBoletoLeiaute: TPanel;
    btnBoletoRR: TPanel;
    btnCadastro: TPanel;
    btnCadastroCont: TPanel;
    btnCancNF: TButton;
    btnCancelarCTe: TButton;
    btnCancMDFe: TButton;
    btnConsultar: TButton;
    btnConsultarCTe: TButton;
    btnConsultarMDFe: TButton;
    btnConsultas: TPanel;
    btnDFe: TPanel;
    btnDFeCertificados: TPanel;
    btnDFeCont: TPanel;
    btnDFeDir: TPanel;
    btnDFeTeste: TPanel;
    btnDFeEmail: TPanel;
    btnDFeGeral: TPanel;
    btnDFePrint: TPanel;
    btnDFeWebServices: TPanel;
    btnDisplay: TPanel;
    btnECF: TPanel;
    btnEmail: TPanel;
    btnEnviar: TButton;
    btnEnviarCTe: TButton;
    btnEnviarMDFe: TButton;
    btnEnviarEmail: TButton;
    btnEnviarEmailCTe: TButton;
    btnEnviarEmailMDFe: TButton;
    btnEtiqueta: TPanel;
    btnGaveta: TPanel;
    btnImpCheque: TPanel;
    btnImprimir: TButton;
    btnImprimirCTe: TButton;
    btnImprimirMDFe: TButton;
    btnInutilizar: TButton;
    btnInutilizarCTe: TButton;
    btnInutilizarMDFe: TButton;
    btnLeitorSerial: TPanel;
    btnMonitor: TPanel;
    btnNCM: TPanel;
    btnPosPrinter: TPanel;
    btnRFD: TPanel;
    btnRFDCont: TPanel;
    btnRFDFile: TPanel;
    btnRFDGeral: TPanel;
    btnSAT: TPanel;
    btnSATCont: TPanel;
    btnSATDados: TPanel;
    btnSATEmitente: TPanel;
    btnSATPrint: TPanel;
    btnSATRede: TPanel;
    btnSATSH: TPanel;
    btnSedex: TPanel;
    btnSH: TPanel;
    btnStatusServ: TButton;
    btnStatusServCTe: TButton;
    btnStatusServMDFe: TButton;
    btnTC: TPanel;
    btnUser: TPanel;
    btnValidarXML: TButton;
    btnValidarXMLCTe: TButton;
    btnValidarXMLMDFe: TButton;
    btSATAssocia: TButton;
    btSATConfigRede: TButton;
    bvCadastro1: TBevel;
    bvCadastro2: TBevel;
    bvCadastro3: TBevel;
    bvCadastro4: TBevel;
    bvCadastro5: TBevel;
    cbBackFeed: TComboBox;
    cbDPI: TComboBox;
    cbETQModelo: TComboBox;
    cbETQPorta: TComboBox;
    cbFormaEmissaoCTe: TComboBox;
    cbFormaEmissaoMDFe: TComboBox;
    cbFormaEmissaoGNRe: TComboBox;
    cbMostrarNaBarraDeTarefas: TCheckBox;
    cbBALModelo: TComboBox;
    cbBALPorta: TComboBox;
    cbCEPWebService: TComboBox;
    cbCHQModelo: TComboBox;
    cbCHQPorta: TComboBox;
    cbComandos: TCheckBox;
    cbControlePorta: TCheckBox;
    cbCortarPapel: TCheckBox;
    cbDISModelo: TComboBox;
    cbDISPorta: TComboBox;
    cbECFModelo: TComboBox;
    cbECFPorta: TComboBox;
    cbEmailCodificacao: TComboBox;
    cbEmailThread: TCheckBox;
    cbEmailSsl: TCheckBox;
    cbEmailTls: TCheckBox;
    cbGAVAcaoAberturaAntecipada: TComboBox;
    cbGAVModelo: TComboBox;
    cbGAVPorta: TComboBox;
    cbGAVStrAbre: TComboBox;
    cbIgnorarTags: TCheckBox;
    cbLCBDispositivo: TComboBox;
    cbLCBPorta: TComboBox;
    cbLCBSufixo: TComboBox;
    cbLCBSufixoLeitor: TComboBox;
    cbLog: TCheckBox;
    cbLogComp: TCheckBox;
    cbModoEmissao: TCheckBox;
    cbModoXML: TCheckBox;
    cbMonitorarPasta: TCheckBox;
    cbOrigem: TComboBox;
    cbRetirarAcentosNaResposta: TCheckBox;
    cbPreview: TCheckBox;
    cbRetirarAcentos: TCheckBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbVersaoWSMDFe: TComboBox;
    cbxUsarSeparadorPathPDF: TCheckBox;
    cbxExibirInfAdicProduto: TCheckBox;
    cbxExibirLogoEmCima: TCheckBox;
    cbxImpDocsReferenciados: TCheckBox;
    cbxImprimirNomeFantasiaNFCe: TCheckBox;
    cbxImprimirDescAcresItemSAT: TCheckBox;
    cbxImprimirCodEANitemSAT: TCheckBox;
    cbxImprimirItem1LinhaSAT: TCheckBox;
    cbxImprimirQRCodeLateralNFCe: TCheckBox;
    cbxImpDetEspNFe: TCheckBox;
    cbxImprimirCodigoEANNFCe: TCheckBox;
    cbXMLSignLib: TComboBox;
    cbSSLType: TComboBox;
    cbxNormatizarMunicipios: TCheckBox;
    chkVerificarValidadeCertificado: TCheckBox;
    chkMostraLogNaTela: TCheckBox;
    cbRFDModelo: TComboBox;
    cbSenha: TCheckBox;
    cbTraduzirTags: TCheckBox;
    cbUF: TComboBox;
    cbAbas: TCheckBox;
    cbUsarEscPos: TRadioButton;
    cbUsarFortes: TRadioButton;
    cbValidarDigest: TCheckBox;
    cbVersaoWS: TComboBox;
    cbVersaoWSCTe: TComboBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxAjustarAut: TCheckBox;
    cbxAmbiente: TComboBox;
    cbxBOLBanco: TComboBox;
    cbxBOLEmissao: TComboBox;
    cbxBOLFiltro: TComboBox;
    cbxBOLF_J: TComboBox;
    cbxBOLImpressora: TComboBox;
    cbxBOLLayout: TComboBox;
    cbxBOLUF: TComboBox;
    cbxCNAB: TComboBox;
    cbxEmissaoPathNFe: TCheckBox;
    cbxExibeResumo: TCheckBox;
    cbxExibirCampoFatura: TCheckBox;
    cbxExibirEAN: TCheckBox;
    cbxExpandirLogo: TCheckBox;
    cbxFormCont: TCheckBox;
    cbxImpDescPorc: TCheckBox;
    cbxImpValLiq: TCheckBox;
    cbxMostrarPreview: TCheckBox;
    cbxMostraStatus: TCheckBox;
    cbxQuebrarLinhasDetalhesItens: TCheckBox;
    cbxTimeZoneMode: TComboBox;
    cbxFormatXML: TCheckBox;
    cbxImpressora: TComboBox;
    cbxImpressoraNFCe: TComboBox;
    cbxImprimirDescAcresItemNFCe: TCheckBox;
    cbxImprimirItem1LinhaNFCe: TCheckBox;
    cbxImprimirTributos: TCheckBox;
    cbxIndRatISSQN: TComboBox;
    cbxModelo: TComboBox;
    cbxModeloSAT: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPastaMensal: TCheckBox;
    cbxPorta: TComboBox;
    cbxRedeProxy: TComboBox;
    cbxRedeSeg: TComboBox;
    cbxRegTribISSQN: TComboBox;
    cbxRegTributario: TComboBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSalvarArqs: TCheckBox;
    cbxSATSalvarCFe: TCheckBox;
    cbxSATSalvarCFeCanc: TCheckBox;
    cbxSATSalvarEnvio: TCheckBox;
    cbxSalvarNFesProcessadas: TCheckBox;
    cbxSedexAvisoReceb: TComboBox;
    cbxSedexFormato: TComboBox;
    cbxSedexMaoPropria: TComboBox;
    cbxSedexServico: TComboBox;
    cbxSepararPorCNPJ: TCheckBox;
    cbxSATSepararPorCNPJ: TCheckBox;
    cbxSATSepararPorMES: TCheckBox;
    cbxSepararporModelo: TCheckBox;
    cbxTCModelo: TComboBox;
    cbxUTF8: TCheckBox;
    chbTCPANSI: TCheckBox;
    chbTagQrCode: TCheckBox;
    cbEscPosImprimirLogo: TCheckBox;
    cbEmailConfirmation: TCheckBox;
    cbxAtualizarXMLCancelado: TCheckBox;
    cbxUnComTributavel: TComboBox;
    cbFormaEmissaoNFe: TComboBox;
    chkBOLRelMostraPreview: TCheckBox;
    chkExibeRazaoSocial: TCheckBox;
    cbSSLLib: TComboBox;
    CHMHelpDatabase1: TCHMHelpDatabase;
    ckMemoria: TCheckBox;
    ckNFCeUsarIntegrador: TCheckBox;
    deBolDirRetornoRel: TDirectoryEdit;
    deUSUDataCadastro: TDateEdit;
    eAvanco: TEdit;
    eCopias: TEdit;
    edMFEInput: TEdit;
    edMFEOutput: TEdit;
    edtArquivoPFX: TEdit;
    edtArquivoWebServicesGNRe: TEdit;
    edtBOLEmailAssunto: TEdit;
    edtBOLEmailMensagem: TMemo;
    edtBOLLocalPagamento: TEdit;
    edtBOLLogoEmpresa: TEdit;
    edtEmailAssuntoCTe: TEdit;
    edtEmailAssuntoMDFe: TEdit;
    edtEmailAssuntoNFe: TEdit;
    edTimeZoneStr: TEdit;
    edtLogoMarcaNFCeSAT: TEdit;
    eMargemEsquerda: TEdit;
    eTemperatura: TEdit;
    eVelocidade: TEdit;
    fspeLarguraNFCe: TSpinEdit;
    edtNumeroSerie: TEdit;
    edtSenha: TEdit;
    edtTimeoutWebServices: TSpinEdit;
    gbExtratoSAT: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    gbImpressao: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox5: TGroupBox;
    gbxConfigSSL: TGroupBox;
    GroupBox6: TGroupBox;
    gbConfigImp: TGroupBox;
    Image2: TImage;
    imgLogoBanco: TImage;
    Label109: TLabel;
    Label110: TLabel;
    Label121: TLabel;
    Label133: TLabel;
    Label138: TLabel;
    Label155: TLabel;
    Label156: TLabel;
    Label163: TLabel;
    Label165: TLabel;
    Label179: TLabel;
    Label180: TLabel;
    Label181: TLabel;
    Label189: TLabel;
    Label190: TLabel;
    Label191: TLabel;
    Label192: TLabel;
    Label193: TLabel;
    Label194: TLabel;
    Label195: TLabel;
    Label196: TLabel;
    Label197: TLabel;
    Label198: TLabel;
    Label199: TLabel;
    Label200: TLabel;
    Label201: TLabel;
    Label202: TLabel;
    Label203: TLabel;
    Label204: TLabel;
    Label207: TLabel;
    Label208: TLabel;
    Label209: TLabel;
    Label210: TLabel;
    Label211: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    lbAvanco: TLabel;
    lbBackFeed: TLabel;
    lbBackFeed1: TLabel;
    lbCopias: TLabel;
    lbDPI: TLabel;
    lblBOLLogoEmpresa: TLabel;
    Label205: TLabel;
    Label206: TLabel;
    Label40: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label68: TLabel;
    Label81: TLabel;
    lblArquivoPFX: TLabel;
    lblNumeroSerie: TLabel;
    lblSenha: TLabel;
    lbMargem: TLabel;
    lbTemperatura: TLabel;
    lbTemperatura2: TLabel;
    LCaption: TLabel;
    Label141: TLabel;
    Label142: TLabel;
    Label143: TLabel;
    Label144: TLabel;
    Label145: TLabel;
    lblAlturaCampos: TLabel;
    lblFonteEndereco: TLabel;
    LHelpConnector1: TLHelpConnector;
    lsvArqsRetorno: TListView;
    mBOLRelatorio: TMemo;
    meUSUHoraCadastro: TMaskEdit;
    mmEmailMsgCTe: TMemo;
    mmEmailMsgMDFe: TMemo;
    mmEmailMsgNFe: TMemo;
    pnLogoBanco: TPanel;
    PanelMenu: TPanel;
    PanelScroll: TPanel;
    PanelTitle: TPanel;
    rdgImprimeChave1LinhaSAT: TRadioGroup;
    rgTipoFonte: TRadioGroup;
    sbArquivoCert: TSpeedButton;
    sbArquivoWebServicesGNRe: TSpeedButton;
    sbLogoMarca1: TSpeedButton;
    sbLogoMarcaNFCeSAT: TSpeedButton;
    sbNumeroSerieCert: TSpeedButton;
    ScrollBox: TScrollBox;
    seMFETimeout: TSpinEdit;
    seUSUCROCadastro: TSpinEdit;
    seUSUGTCadastro: TFloatSpinEdit;
    seUSUNumeroCadastro: TSpinEdit;
    speAlturaCampos: TSpinEdit;
    spedtDecimaisVUnit: TSpinEdit;
    spedtCasasDecimaisQtd: TSpinEdit;
    speEspBorda: TSpinEdit;
    speFonteCampos: TSpinEdit;
    speFonteEndereco: TSpinEdit;
    speFonteRazao: TSpinEdit;
    speLargCodProd: TSpinEdit;
    fspeNFCeMargemDir: TFloatSpinEdit;
    fspeNFCeMargemEsq: TFloatSpinEdit;
    fspeNFCeMargemInf: TFloatSpinEdit;
    fspeNFCeMargemSup: TFloatSpinEdit;
    fspeMargemDir: TFloatSpinEdit;
    fspeMargemEsq: TFloatSpinEdit;
    fspeMargemInf: TFloatSpinEdit;
    fspeMargemSup: TFloatSpinEdit;
    gbDFeConfDiversas: TGroupBox;
    gbDFeTimeZone: TGroupBox;
    Label187: TLabel;
    edtArquivoWebServicesMDFe: TEdit;
    edtArquivoWebServicesNFe: TEdit;
    edtArquivoWebServicesCTe: TEdit;
    edtNumCopia: TSpinEdit;
    edtCNPJContador: TEdit;
    gbxMargem1: TGroupBox;
    GroupBox4: TGroupBox;
    Label157: TLabel;
    Label159: TLabel;
    Label182: TLabel;
    Label183: TLabel;
    Label184: TLabel;
    Label185: TLabel;
    Label186: TLabel;
    Label188: TLabel;
    pgEmailDFe: TPageControl;
    rgTamanhoPapelDacte: TRadioGroup;
    rgTipoAmb: TRadioGroup;
    sbArquivoWebServicesMDFe: TSpeedButton;
    sbArquivoWebServicesNFe: TSpeedButton;
    sbArquivoWebServicesCTe: TSpeedButton;
    TabSheet1: TTabSheet;
    tsIntegrador: TTabSheet;
    tsRelatorio: TTabSheet;
    tsImpCTe: TTabSheet;
    tsTesteMDFe: TTabSheet;
    tsEmailMDFe: TTabSheet;
    tsTesteCTe: TTabSheet;
    tsEmailNFe: TTabSheet;
    tsEmailCTe: TTabSheet;
    tsCertificadoDFe: TTabSheet;
    chbArqEntANSI: TCheckBox;
    chbArqSaiANSI: TCheckBox;
    chCHQVerForm: TCheckBox;
    chECFArredondaMFD: TCheckBox;
    chECFArredondaPorQtd: TCheckBox;
    chECFControlePorta: TCheckBox;
    chECFDescrGrande: TCheckBox;
    chECFIgnorarTagsFormatacao: TCheckBox;
    chECFSinalGavetaInvertido: TCheckBox;
    cbUmaInstancia: TCheckBox;
    cbHRI: TCheckBox;
    chkLerCedenteRetorno: TCheckBox;
    chLCBExcluirSufixo: TCheckBox;
    chRFD: TCheckBox;
    chRFDIgnoraMFD: TCheckBox;
    ckgBOLMostrar: TCheckGroup;
    ckSalvar: TCheckBox;
    tsImpressaoDFe: TTabSheet;
    deBOLDirArquivo: TDirectoryEdit;
    deBOLDirLogo: TDirectoryEdit;
    deBolDirRemessa: TDirectoryEdit;
    deBolDirRetorno: TDirectoryEdit;
    deNcmSalvar: TDirectoryEdit;
    deRFDDataSwBasico: TDateEdit;
    tsDiretoriosDFe: TTabSheet;
    edBALLog: TEdit;
    edCEPChaveBuscarCEP: TEdit;
    edCEPTestar: TEdit;
    edCHQBemafiINI: TEdit;
    edCHQCidade: TEdit;
    edCHQFavorecido: TEdit;
    edCONProxyHost: TEdit;
    edCONProxyPass: TEdit;
    edCONProxyPort: TEdit;
    edCONProxyUser: TEdit;
    edECFLog: TEdit;
    edEmailEndereco: TEdit;
    edEmailHost: TEdit;
    edEmailNome: TEdit;
    edEmailPorta: TSpinEdit;
    edEmailSenha: TEdit;
    edEmailUsuario: TEdit;
    edEntTXT: TEdit;
    edIBGECodNome: TEdit;
    edNomeDLL: TEdit;
    edPosPrinterLog: TEdit;
    edRedeCodigo: TEdit;
    edRedeDNS1: TEdit;
    edRedeDNS2: TEdit;
    edRedeGW: TEdit;
    edRedeIP: TEdit;
    edRedeMask: TEdit;
    edRedeProxyIP: TEdit;
    edRedeProxyPorta: TSpinEdit;
    edRedeProxySenha: TEdit;
    edRedeProxyUser: TEdit;
    edRedeSenha: TEdit;
    edRedeSSID: TEdit;
    edRedeUsuario: TEdit;
    edSATLog: TEdit;
    edSATPathArqs: TEdit;
    edLogComp: TEdit;
    edtCodigoAtivacao: TEdit;
    edtCodUF: TEdit;
    edLCBPreExcluir: TEdit;
    edLogArq: TEdit;
    edPortaTCP: TEdit;
    edRFDDir: TEdit;
    edSaiTXT: TEdit;
    edSenha: TEdit;
    edSH_Aplicativo: TEdit;
    edSH_CNPJ: TEdit;
    edSH_COO: TEdit;
    edSH_IE: TEdit;
    edSH_IM: TEdit;
    edSH_Linha1: TEdit;
    edSH_Linha2: TEdit;
    edSH_Site: TEdit;
    edSH_NumeroAP: TEdit;
    edSH_RazaoSocial: TEdit;
    edSH_VersaoAP: TEdit;
    edtAguardar: TEdit;
    edtBOLAgencia: TEdit;
    edtBOLBairro: TEdit;
    edtBOLCEP: TMaskEdit;
    edtBOLCidade: TEdit;
    edtBOLCNPJ: TMaskEdit;
    edtBOLComplemento: TEdit;
    edtBOLConta: TEdit;
    edtBOLDigitoAgencia: TEdit;
    edtBOLDigitoConta: TEdit;
    edtBOLLogradouro: TEdit;
    edtBOLNumero: TEdit;
    edtBOLRazaoSocial: TEdit;
    edtEmitCNPJ: TEdit;
    edtEmitIE: TEdit;
    edtEmitIM: TEdit;
    edTCArqPrecos: TEdit;
    edTCNaoEncontrado: TEdit;
    edtCodCliente: TEdit;
    edtCodTransmissao: TEdit;
    edtConvenio: TEdit;
    edTCPort: TEdit;
    edtEmailEmpresa: TEdit;
    edtFaxEmpresa: TEdit;
    edtIdToken: TEdit;
    edTimeOutTCP: TEdit;
    edtIntervalo: TEdit;
    edtLogoMarca: TEdit;
    edtModalidade: TEdit;
    edtNcmNumero: TEdit;
    edtPathDPEC: TEdit;
    edtPathEvento: TEdit;
    edtPathInu: TEdit;
    edtPathLogs: TEdit;
    edtPathNFe: TEdit;
    edtPathPDF: TEdit;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    edtSedexAltura: TEdit;
    edtSedexCEPDestino: TEdit;
    edtSedexCEPOrigem: TEdit;
    edtSedexComprimento: TEdit;
    edtSedexContrato: TEdit;
    edtSedexDiametro: TEdit;
    edtSedexLargura: TEdit;
    edtSedexPeso: TEdit;
    edtSedexSenha: TEdit;
    edtSedexValorDeclarado: TEdit;
    edtSiteEmpresa: TEdit;
    edtSwHAssinatura: TEdit;
    edtSwHCNPJ: TEdit;
    edtTentativas: TEdit;
    edtToken: TEdit;
    edUSUCNPJ: TEdit;
    edUSUEndereco: TEdit;
    edUSUIE: TEdit;
    edUSURazaoSocial: TEdit;
    tsEmailDFe: TTabSheet;
    gbCEP: TGroupBox;
    gbCEPProxy: TGroupBox;
    gbCEPTestar: TGroupBox;
    gbCHQDados: TGroupBox;
    gbDANFeESCPOS: TGroupBox;
    gbEmailDados: TGroupBox;
    gbIPFix: TGroupBox;
    gbLog: TGroupBox;
    gbLogComp: TGroupBox;
    gbPPPoE: TGroupBox;
    gbProxy: TGroupBox;
    gbRFDECF: TGroupBox;
    gbSenha: TGroupBox;
    gbTCP: TGroupBox;
    gbTXT: TGroupBox;
    gbWiFi: TGroupBox;
    gbxMargem: TGroupBox;
    gbxProxy: TGroupBox;
    gbxRetornoEnvio: TGroupBox;
    GroupBox1: TGroupBox;
    gbxWSNFe: TGroupBox;
    gbConfiguracao: TGroupBox;
    gbCodBarras: TGroupBox;
    gbQRCode: TGroupBox;
    gbLogotipo: TGroupBox;
    gbGeral: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    Image1: TImage;
    ImageList1: TImageList;
    Impressao: TTabSheet;
    Label1: TLabel;
    Label10: TLabel;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label108: TLabel;
    Label11: TLabel;
    Label111: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    Label12: TLabel;
    Label120: TLabel;
    Label122: TLabel;
    Label123: TLabel;
    Label124: TLabel;
    Label125: TLabel;
    Label126: TLabel;
    Label127: TLabel;
    Label128: TLabel;
    Label129: TLabel;
    Label13: TLabel;
    Label130: TLabel;
    Label131: TLabel;
    Label132: TLabel;
    Label134: TLabel;
    Label135: TLabel;
    Label136: TLabel;
    Label137: TLabel;
    Label154: TLabel;
    lbBuffer: TLabel;
    lbColunas: TLabel;
    lbEspacosLinhas: TLabel;
    lbLinhasPular: TLabel;
    lbModelo: TLabel;
    lbPorPrinterLog: TLabel;
    lbPorta: TLabel;
    lbQRCodeTipo: TLabel;
    lbQRCodeLargMod: TLabel;
    lbQRCodeErrorLevel: TLabel;
    lbLogoKC1: TLabel;
    lbLogoKC2: TLabel;
    lbLogoFatorX: TLabel;
    lbLogoFatorY: TLabel;
    lbLargura: TLabel;
    lbAltura: TLabel;
    Label139: TLabel;
    Label14: TLabel;
    Label140: TLabel;
    Label146: TLabel;
    Label147: TLabel;
    Label148: TLabel;
    Label149: TLabel;
    Label15: TLabel;
    Label150: TLabel;
    Label151: TLabel;
    Label152: TLabel;
    Label153: TLabel;
    Label158: TLabel;
    Label16: TLabel;
    Label160: TLabel;
    Label161: TLabel;
    Label162: TLabel;
    Label164: TLabel;
    Label166: TLabel;
    Label167: TLabel;
    Label168: TLabel;
    Label169: TLabel;
    Label17: TLabel;
    Label170: TLabel;
    Label171: TLabel;
    Label172: TLabel;
    Label173: TLabel;
    Label174: TLabel;
    Label175: TLabel;
    Label176: TLabel;
    Label177: TLabel;
    Label178: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label8: TLabel;
    Label80: TLabel;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    Label87: TLabel;
    Label88: TLabel;
    Label89: TLabel;
    Label9: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    lAdSufixo: TLabel;
    lblBOLAgencia: TLabel;
    lblBOLBairro: TLabel;
    lblBOLBanco: TLabel;
    lblBOLCep: TLabel;
    lblBOLCidade: TLabel;
    lblBOLComplemento: TLabel;
    lblBOLConta: TLabel;
    lblBOLCPFCNPJ: TLabel;
    lblBOLDigAgencia: TLabel;
    lblBOLDigConta: TLabel;
    lblBOLDirLogo: TLabel;
    lblBOLEmissao: TLabel;
    lblBOLLogradouro: TLabel;
    lblBOLNomeRazao: TLabel;
    lblBOLNumero: TLabel;
    lblBOLPessoa: TLabel;
    lCEPCEP: TLabel;
    lCEPChave: TLabel;
    lCEPProxyPorta: TLabel;
    lCEPProxySenha: TLabel;
    lCEPProxyServidor: TLabel;
    lCEPProxyUsuario: TLabel;
    lCEPWebService: TLabel;
    lGAVEstado: TLabel;
    lIBGECodNome: TLabel;
    lImpressora: TLabel;
    lLCBCodigoLido: TPanel;
    lNumPortaTCP: TLabel;
    lRFDID: TLabel;
    lRFDMarca: TLabel;
    lSSID: TLabel;
    lSSID1: TLabel;
    lSSID10: TLabel;
    lSSID11: TLabel;
    lSSID12: TLabel;
    lSSID2: TLabel;
    lSSID3: TLabel;
    lSSID4: TLabel;
    lSSID5: TLabel;
    lSSID6: TLabel;
    lSSID7: TLabel;
    lSSID8: TLabel;
    lSSID9: TLabel;
    lTimeOutTCP: TLabel;
    mCmd: TMemo;
    meRFDHoraSwBasico: TMaskEdit;
    mResp: TMemo;
    mRFDINI: TMemo;
    mRSAKey: TMemo;
    mTCConexoes: TMemo;
    mResposta: TSynMemo;
    pgDFe: TPageControl;
    pgSAT: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pgConfig: TPageControl;
    pCentral: TPanel;
    Panel4: TPanel;
    pbEmailTeste: TProgressBar;
    pCmd: TPanel;
    pConfig: TPanel;
    pgBoleto: TPageControl;
    pgCadastro: TPageControl;
    pgConRFD: TPageControl;
    pgImpressaoDFe: TPageControl;
    pgECFParams: TPageControl;
    pgSwHouse: TPageControl;
    pComandos: TPanel;
    pgTestes: TPageControl;
    pgTipoWebService: TPageControl;
    pRespostas: TPanel;
    PrintDialog1: TPrintDialog;
    pTopCmd: TPanel;
    pTopRespostas: TPanel;
    rgRedeTipoInter: TRadioGroup;
    rgRedeTipoLan: TRadioGroup;
    rbLCBFila: TRadioButton;
    rbLCBTeclado: TRadioButton;
    rbTCP: TRadioButton;
    rbTXT: TRadioButton;
    rgLocalCanhoto: TRadioGroup;
    rgModeloDanfe: TRadioGroup;
    rgModeloDANFeNFCE: TRadioGroup;
    rgModoImpressaoEvento: TRadioGroup;
    rgTipoDanfe: TRadioGroup;
    SbArqLog: TSpeedButton;
    SbArqLog2: TSpeedButton;
    sbBALSerial: TSpeedButton;
    sbBALLog: TSpeedButton;
    sbCHQBemafiINI: TSpeedButton;
    sbCHQSerial: TSpeedButton;
    sbDirRFD: TSpeedButton;
    sbECFLog: TSpeedButton;
    sbECFSerial: TSpeedButton;
    sbLog: TSpeedButton;
    sbLogoMarca: TSpeedButton;
    sbPathDPEC: TSpeedButton;
    sbPathEvento: TSpeedButton;
    sbPathInu: TSpeedButton;
    sbPathNFe: TSpeedButton;
    sbPathPDF: TSpeedButton;
    sbPathSalvar: TSpeedButton;
    sbPosPrinterLog: TSpeedButton;
    sbSerial: TSpeedButton;
    sbTCArqPrecosEdit: TSpeedButton;
    sbTCArqPrecosFind: TSpeedButton;
    seBuffer: TSpinEdit;
    seColunas: TSpinEdit;
    sedBALIntervalo: TSpinEdit;
    sedECFIntervalo: TSpinEdit;
    sedECFLinhasEntreCupons: TSpinEdit;
    sedECFMaxLinhasBuffer: TSpinEdit;
    sedECFPaginaCodigo: TSpinEdit;
    sedECFTimeout: TSpinEdit;
    sedGAVIntervaloAbertura: TSpinEdit;
    sedIntervalo: TSpinEdit;
    seDISIntByte: TSpinEdit;
    seDISIntervalo: TSpinEdit;
    seDISPassos: TSpinEdit;
    sedLCBIntervalo: TSpinEdit;
    sedLogLinhas: TSpinEdit;
    sedLogLinhasComp: TSpinEdit;
    seEspacosLinhas: TSpinEdit;
    seLargura: TSpinEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    lblCep: TLabel;
    sbSobre: TSpeedButton;
    seLinhasPular: TSpinEdit;
    seMargemDireita: TSpinEdit;
    seMargemEsquerda: TSpinEdit;
    seMargemFundo: TSpinEdit;
    seMargemTopo: TSpinEdit;
    seNumeroCaixa: TSpinEdit;
    sePagCod: TSpinEdit;
    sfeVersaoEnt: TFloatSpinEdit;
    shpLCB: TShape;
    shpTC: TShape;
    spBOLCopias: TSpinEdit;
    SpeedButton1: TSpeedButton;
    seFundoImp: TSpinEdit;
    seCodBarrasLargura: TSpinEdit;
    seCodBarrasAltura: TSpinEdit;
    seQRCodeTipo: TSpinEdit;
    seQRCodeLargMod: TSpinEdit;
    seQRCodeErrorLevel: TSpinEdit;
    seLogoKC1: TSpinEdit;
    seLogoKC2: TSpinEdit;
    seLogoFatorY: TSpinEdit;
    seLogoFatorX: TSpinEdit;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ACBrCHQ1: TACBrCHQ;
    ACBrGAV1: TACBrGAV;
    ACBrDIS1: TACBrDIS;
    pmTray: TPopupMenu;
    Restaurar1: TMenuItem;
    Encerrar1: TMenuItem;
    Ocultar1: TMenuItem;
    N1: TMenuItem;
    pBotoes: TPanel;
    btMinimizar: TBitBtn;
    bConfig: TBitBtn;
    ACBrLCB1: TACBrLCB;
    SynXMLSyn1: TSynXMLSyn;
    TabControl1: TTabControl;
    tsImpNFCe: TTabSheet;
    tsConfiguracaoDFe: TTabSheet;
    tsEscPos: TTabSheet;
    tsDadosEmit: TTabSheet;
    tsDadosSAT: TTabSheet;
    tsDadosSwHouse: TTabSheet;
    tsRede: TTabSheet;
    tsSat: TTabSheet;
    tsTestesDFe: TTabSheet;
    tsDadosEmpresa: TTabSheet;
    tsImpGeralDFe: TTabSheet;
    tsDFe: TTabSheet;
    tsTesteNFe: TTabSheet;
    tsWSNFCe: TTabSheet;
    tsWSNFe: TTabSheet;
    TrayIcon1: TTrayIcon;
    bCancelar: TBitBtn;
    Timer1: TTimer;
    TcpServer: TACBrTCPServer;
    OpenDialog1: TOpenDialog;
    ACBrRFD1: TACBrRFD;
    ACBrBAL1: TACBrBAL;
    ACBrETQ1: TACBrETQ;
    TCPServerTC: TACBrTCPServer;
    TimerTC: TTimer;
    tsACBrBoleto: TTabSheet;
    tsBAL: TTabSheet;
    tsLayoutBoleto: TTabSheet;
    tsBoletoEmail: TTabSheet;
    tsCadastro: TTabSheet;
    tsCadSwChaveRSA: TTabSheet;
    tsCadSwDados: TTabSheet;
    tsCadSwH: TTabSheet;
    tsCadUsuario: TTabSheet;
    tsCedente: TTabSheet;
    tsCHQ: TTabSheet;
    tsConsultas: TTabSheet;
    tsContaBancaria: TTabSheet;
    tsDIS: TTabSheet;
    tsECF: TTabSheet;
    tsECFParamI: TTabSheet;
    tsECFParamII: TTabSheet;
    tsEmail: TTabSheet;
    tsETQ: TTabSheet;
    tsGAV: TTabSheet;
    tsLCB: TTabSheet;
    tsMonitor: TTabSheet;
    tsNcm: TTabSheet;
    tsRemessaRetorno: TTabSheet;
    tsRFD: TTabSheet;
    tsRFDConfig: TTabSheet;
    tsRFDINI: TTabSheet;
    tsSEDEX: TTabSheet;
    tsTC: TTabSheet;
    tsWebServiceDFe: TTabSheet;
    procedure ACBrEAD1GetChavePrivada(var Chave: ansistring);
    procedure ACBrEAD1GetChavePublica(var Chave: ansistring);
    procedure ACBrGIF1Click(Sender: TObject);
    procedure ACBrMail1MailProcess(const AMail: TACBrMail;
      const aStatus: TMailStatus);
    procedure ACBrNFe1GerarLog(const ALogLine: string; var Tratado: boolean);
    procedure ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
    procedure ACBrSAT1GetNumeroSessao(var NumeroSessao: Integer);
    procedure ACBrSAT1GetsignAC(var Chave: AnsiString);
    procedure ACBrSAT1GravarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure ApplicationProperties1Minimize(Sender: TObject);
    procedure ApplicationProperties1Restore(Sender: TObject);
    procedure bAtivarClick(Sender: TObject);
    procedure bbAtivarClick(Sender: TObject);
    procedure bBoletoRelatorioRetornoClick(Sender: TObject);
    procedure bBOLLerArqRelatorioClick(Sender: TObject);
    procedure bCEPTestarClick(Sender: TObject);
    procedure bDownloadListaClick(Sender: TObject);
    procedure bEmailTestarConfClick(Sender: TObject);
    procedure bIBGETestarClick(Sender: TObject);
    procedure bImpressoraClick(Sender: TObject);
    procedure bInicializarClick(Sender: TObject);
    procedure bNcmConsultarClick(Sender: TObject);
    procedure bRSAeECFcClick(Sender: TObject);
    procedure bSedexRastrearClick(Sender: TObject);
    procedure bSedexTestarClick(Sender: TObject);
    procedure btAtivarsatClick(Sender: TObject);
    procedure btConsultarStatusOPSATClick(Sender: TObject);
    procedure btnBalancaClick(Sender: TObject);
    procedure btnBoletoCedenteClick(Sender: TObject);
    procedure btnBoletoClick(Sender: TObject);
    procedure btnBoletoContaClick(Sender: TObject);
    procedure btnBoletoEmailClick(Sender: TObject);
    procedure btnBoletoLeiauteClick(Sender: TObject);
    procedure btnBoletoRelatorioRetornoClick(Sender: TObject);
    procedure btnBoletoRRClick(Sender: TObject);
    procedure btnCadastroClick(Sender: TObject);
    procedure btnCancelarCTeClick(Sender: TObject);
    procedure btnCancMDFeClick(Sender: TObject);
    procedure btnCancNFClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarCTeClick(Sender: TObject);
    procedure btnConsultarMDFeClick(Sender: TObject);
    procedure btnConsultasClick(Sender: TObject);
    procedure btnDFeCertificadosClick(Sender: TObject);
    procedure btnDFeClick(Sender: TObject);
    procedure btnDFeDirClick(Sender: TObject);
    procedure btnDFeEmailClick(Sender: TObject);
    procedure btnDFeGeralClick(Sender: TObject);
    procedure btnDFePrintClick(Sender: TObject);
    procedure btnDFeTesteClick(Sender: TObject);
    procedure btnDFeWebServicesClick(Sender: TObject);
    procedure btnDisplayClick(Sender: TObject);
    procedure btnECFClick(Sender: TObject);
    procedure btnEmailClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnEnviarCTeClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure btnEnviarEmailCTeClick(Sender: TObject);
    procedure btnEnviarEmailMDFeClick(Sender: TObject);
    procedure btnEnviarMDFeClick(Sender: TObject);
    procedure btnEtiquetaClick(Sender: TObject);
    procedure btnGavetaClick(Sender: TObject);
    procedure btnImpChequeClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnImprimirCTeClick(Sender: TObject);
    procedure btnImprimirMDFeClick(Sender: TObject);
    procedure btnIntegradorClick(Sender: TObject);
    procedure btnInutilizarClick(Sender: TObject);
    procedure btnInutilizarCTeClick(Sender: TObject);
    procedure btnLeitorSerialClick(Sender: TObject);
    procedure btnMonitorClick(Sender: TObject);
    procedure btnNCMClick(Sender: TObject);
    procedure btnPosPrinterClick(Sender: TObject);
    procedure btnRFDClick(Sender: TObject);
    procedure btnRFDFileClick(Sender: TObject);
    procedure btnRFDGeralClick(Sender: TObject);
    procedure btnSATClick(Sender: TObject);
    procedure btnSATDadosClick(Sender: TObject);
    procedure btnSATEmitenteClick(Sender: TObject);
    procedure btnSATPrintClick(Sender: TObject);
    procedure btnSATRedeClick(Sender: TObject);
    procedure btnSATSHClick(Sender: TObject);
    procedure btnSedexClick(Sender: TObject);
    procedure btnSHClick(Sender: TObject);
    procedure btnStatusServClick(Sender: TObject);
    procedure btnStatusServCTeClick(Sender: TObject);
    procedure btnStatusServMDFeClick(Sender: TObject);
    procedure btnTCClick(Sender: TObject);
    procedure btnUserClick(Sender: TObject);
    procedure btnValidarXMLClick(Sender: TObject);
    procedure btnValidarXMLCTeClick(Sender: TObject);
    procedure btnValidarXMLMDFeClick(Sender: TObject);
    procedure btSATAssociaClick(Sender: TObject);
    procedure btSATConfigRedeClick(Sender: TObject);
    procedure cbControlePortaChange(Sender: TObject);
    procedure cbCortarPapelChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbIgnorarTagsChange(Sender: TObject);
    procedure cbLogCompClick(Sender: TObject);
    procedure cbHRIChange(Sender: TObject);
    procedure cbMonitorarPastaChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbTraduzirTagsChange(Sender: TObject);
    procedure cbUsarEscPosClick(Sender: TObject);
    procedure cbUsarFortesClick(Sender: TObject);
    procedure cbxBOLBancoChange(Sender: TObject);
    procedure cbxBOLF_JChange(Sender: TObject);
    procedure cbCEPWebServiceChange(Sender: TObject);
    procedure cbxImpDescPorcChange(Sender: TObject);
    procedure cbXMLSignLibChange(Sender: TObject);
    procedure cbxModeloSATChange(Sender: TObject);
    procedure cbxPastaMensalClick(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure cbxRedeProxyChange(Sender: TObject);
    procedure cbxSalvarArqsChange(Sender: TObject);
    procedure cbxSATSalvarCFeCancChange(Sender: TObject);
    procedure cbxSATSalvarCFeChange(Sender: TObject);
    procedure cbxSATSalvarEnvioChange(Sender: TObject);
    procedure cbxSedexAvisoRecebChange(Sender: TObject);
    procedure cbxSedexFormatoChange(Sender: TObject);
    procedure cbxSedexMaoPropriaChange(Sender: TObject);
    procedure cbxSedexServicoChange(Sender: TObject);
    procedure cbxSATSepararPorCNPJChange(Sender: TObject);
    procedure cbxSATSepararPorMESChange(Sender: TObject);
    procedure cbxSepararPorCNPJChange(Sender: TObject);
    procedure cbxTimeZoneModeChange(Sender: TObject);
    procedure cbxUTF8Change(Sender: TObject);
    procedure chbTagQrCodeChange(Sender: TObject);
    procedure chECFArredondaMFDClick(Sender: TObject);
    procedure chECFControlePortaClick(Sender: TObject);
    procedure chECFIgnorarTagsFormatacaoClick(Sender: TObject);
    procedure chRFDChange(Sender: TObject);
    procedure ckSalvarClick(Sender: TObject);
    procedure deBOLDirArquivoExit(Sender: TObject);
    procedure deBOLDirLogoExit(Sender: TObject);
    procedure deBolDirRemessaExit(Sender: TObject);
    procedure BOLDirRetornoExit(Sender: TObject);
    procedure deBolDirRetornoRelChange(Sender: TObject);
    procedure deUSUDataCadastroExit(Sender: TObject);
    procedure deRFDDataSwBasicoExit(Sender: TObject);
    procedure edBALLogChange(Sender: TObject);
    procedure edEmailEnderecoExit(Sender: TObject);
    procedure edSATLogChange(Sender: TObject);
    procedure edTimeZoneStrEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);{%h-}
    procedure FormCreate(Sender: TObject);
    procedure ACBrECF1MsgAguarde(Mensagem: string);
    procedure ACBrECF1MsgPoucoPapel(Sender: TObject);
    procedure bConfigClick(Sender: TObject);
    procedure cbLogClick(Sender: TObject);
    procedure edOnlyNumbers(Sender: TObject; var Key: char);
    procedure bECFTestarClick(Sender: TObject);
    procedure bECFLeituraXClick(Sender: TObject);
    procedure bECFAtivarClick(Sender: TObject);
    procedure meUSUHoraCadastroExit(Sender: TObject);
    procedure meRFDHoraSwBasicoExit(Sender: TObject);
    procedure pgBoletoChange(Sender: TObject);
    procedure pgConfigChange(Sender: TObject);
    procedure rgRedeTipoInterClick(Sender: TObject);
    procedure rgRedeTipoLanClick(Sender: TObject);
    procedure SbArqLog2Click(Sender: TObject);
    procedure SbArqLogClick(Sender: TObject);
    procedure sbArquivoCertClick(Sender: TObject);
    procedure sbArquivoWebServicesCTeClick(Sender: TObject);
    procedure sbArquivoWebServicesGNReClick(Sender: TObject);
    procedure sbArquivoWebServicesMDFeClick(Sender: TObject);
    procedure sbArquivoWebServicesNFeClick(Sender: TObject);
    procedure sbBALSerialClick(Sender: TObject);
    procedure sbLogoMarca1Click(Sender: TObject);
    procedure sbLogoMarcaNFCeSATClick(Sender: TObject);
    procedure sbNumeroSerieCertClick(Sender: TObject);
    procedure sbBALLogClick(Sender: TObject);
    procedure sbLogoMarcaClick(Sender: TObject);
    procedure sbPathDPECClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
    procedure sbPathInuClick(Sender: TObject);
    procedure sbPathNFeClick(Sender: TObject);
    procedure sbPathPDFClick(Sender: TObject);
    procedure sbPathSalvarClick(Sender: TObject);
    procedure sbPosPrinterLogClick(Sender: TObject);
    procedure sbSerialClick(Sender: TObject);
    procedure sbSobreClick(Sender: TObject);
    procedure ScrollBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure sedECFLinhasEntreCuponsChange(Sender: TObject);
    procedure sedECFMaxLinhasBufferChange(Sender: TObject);
    procedure sedECFPaginaCodigoChange(Sender: TObject);
    procedure sePagCodChange(Sender: TObject);
    procedure sfeVersaoEntChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TcpServerConecta(const TCPBlockSocket: TTCPBlockSocket;
      var Enviar: ansistring);{%h-}
    procedure TcpServerDesConecta(const TCPBlockSocket: TTCPBlockSocket;
      Erro: integer; ErroDesc: string);{%h-}
    procedure TcpServerRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
      const Recebido: ansistring; var Enviar: ansistring);{%h-}
    procedure TCPServerTCConecta(const TCPBlockSocket: TTCPBlockSocket;
      var Enviar: ansistring);{%H-}
    procedure TCPServerTCDesConecta(const TCPBlockSocket: TTCPBlockSocket;
      Erro: integer; ErroDesc: string);{%H-}
    procedure TCPServerTCRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
      const Recebido: ansistring; var Enviar: ansistring);
    procedure TrayIcon1Click(Sender: TObject);
    procedure tsACBrBoletoShow(Sender: TObject);
    procedure tsECFShow(Sender: TObject);
    procedure Ocultar1Click(Sender: TObject);
    procedure Restaurar1Click(Sender: TObject);
    procedure Encerrar1Click(Sender: TObject);
    procedure cbGAVModeloChange(Sender: TObject);
    procedure cbGAVPortaChange(Sender: TObject);
    procedure bGAVEstadoClick(Sender: TObject);
    procedure bGAVAbrirClick(Sender: TObject);
    procedure cbDISModeloChange(Sender: TObject);
    procedure cbDISPortaChange(Sender: TObject);
    procedure bDISLimparClick(Sender: TObject);
    procedure bDISTestarClick(Sender: TObject);
    procedure btMinimizarClick(Sender: TObject);
    procedure rbTCPTXTClick(Sender: TObject);
    procedure cbCHQModeloChange(Sender: TObject);
    procedure cbCHQPortaChange(Sender: TObject);
    procedure cbECFModeloChange(Sender: TObject);
    procedure cbECFPortaChange(Sender: TObject);
    procedure chECFArredondaPorQtdClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bCancelarClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure DoACBrTimer(Sender: TObject);
    procedure chECFDescrGrandeClick(Sender: TObject);
    procedure bCHQTestarClick(Sender: TObject);
    procedure cbLCBPortaChange(Sender: TObject);
    procedure bLCBAtivarClick(Sender: TObject);
    procedure edLCBSufLeituraKeyPress(Sender: TObject; var Key: char);
    procedure chLCBExcluirSufixoClick(Sender: TObject);
    procedure edLCBPreExcluirChange(Sender: TObject);
    procedure ACBrLCB1LeCodigo(Sender: TObject);
    procedure AumentaTempoHint(Sender: TObject);
    procedure DiminuiTempoHint(Sender: TObject);
    procedure cbLCBSufixoLeitorChange(Sender: TObject);
    procedure FormShortCut(Key: integer; Shift: TShiftState;{%h-}
      var Handled: boolean);{%h-}
    procedure cbGAVStrAbreChange(Sender: TObject);
    procedure bExecECFTesteClick(Sender: TObject);
    procedure chECFSinalGavetaInvertidoClick(Sender: TObject);
    procedure sedLCBIntervaloChanged(Sender: TObject);
    procedure sedECFTimeoutChanged(Sender: TObject);
    procedure sedGAVIntervaloAberturaChanged(Sender: TObject);
    procedure bGAVAtivarClick(Sender: TObject);
    procedure tsGAVShow(Sender: TObject);
    procedure cbGAVAcaoAberturaAntecipadaChange(Sender: TObject);
    procedure edCHQFavorecidoChange(Sender: TObject);
    procedure edCHQCidadeChange(Sender: TObject);
    procedure sbCHQBemafiINIClick(Sender: TObject);
    procedure edCHQBemafiINIExit(Sender: TObject);
    procedure ACBrECF1AguardandoRespostaChange(Sender: TObject);
    procedure bLCBSerialClick(Sender: TObject);
    procedure rbLCBTecladoClick(Sender: TObject);
    procedure tsLCBShow(Sender: TObject);
    procedure sedECFIntervaloChanged(Sender: TObject);
    procedure seDISPassosChanged(Sender: TObject);
    procedure seDISIntervaloChanged(Sender: TObject);
    procedure seDISIntByteChanged(Sender: TObject);
    procedure bDISAnimarClick(Sender: TObject);
    procedure bRFDINILerClick(Sender: TObject);
    procedure bRFDINISalvarClick(Sender: TObject);
    procedure bRFDMFClick(Sender: TObject);
    procedure sbDirRFDClick(Sender: TObject);
    procedure edSH_AplicativoChange(Sender: TObject);
    procedure edSH_NumeroAPChange(Sender: TObject);
    procedure tsRFDShow(Sender: TObject);
    procedure bRSALoadKeyClick(Sender: TObject);
    procedure ACBrRFD1GetKeyRSA(var PrivateKey_RSA: string);
    procedure cbRFDModeloChange(Sender: TObject);
    procedure bRFDIDClick(Sender: TObject);
    procedure tsRFDINIShow(Sender: TObject);
    procedure seUSUGTCadastroKeyPress(Sender: TObject; var Key: char);
    procedure seUSUGTCadastroExit(Sender: TObject);
    procedure tsRFDRSAShow(Sender: TObject);
    procedure cbSenhaClick(Sender: TObject);
    procedure bRSAPrivKeyClick(Sender: TObject);
    procedure bRSAPubKeyClick(Sender: TObject);
    procedure edECFLogChange(Sender: TObject);
    procedure sbLogClick(Sender: TObject);
    procedure sbECFLogClick(Sender: TObject);
    procedure cbBALModeloChange(Sender: TObject);
    procedure cbBALPortaChange(Sender: TObject);
    procedure sedBALIntervaloChanged(Sender: TObject);
    procedure bBALAtivarClick(Sender: TObject);
    procedure bBALTestarClick(Sender: TObject);
    procedure sbECFSerialClick(Sender: TObject);
    procedure cbETQModeloChange(Sender: TObject);
    procedure cbETQPortaChange(Sender: TObject);
    procedure bTCAtivarClick(Sender: TObject);
    procedure tsTCShow(Sender: TObject);
    procedure cbxTCModeloChange(Sender: TObject);
    procedure sbTCArqPrecosEditClick(Sender: TObject);
    procedure sbTCArqPrecosFindClick(Sender: TObject);
    procedure TimerTCTimer(Sender: TObject);
    procedure sbCHQSerialClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure ACT_ButtonMouseEnter(Sender: TObject);
    procedure ACT_ButtonMouseLeave(Sender: TObject);
  private
    ACBrMonitorINI: string;
    Inicio, MonitorarPasta: boolean;
    ArqSaiTXT, ArqSaiTMP, ArqEntTXT, ArqLogTXT, ArqLogCompTXT,
    ArqEntOrig, ArqSaiOrig: string;
    fsCmd: TACBrCmd;
    fsProcessar: TStringList;
    fsInicioLoteTXT: boolean;
    NewLines: ansistring;
    fsDisWorking: boolean;
    fsRFDIni: string;
    fsRFDLeuParams: boolean;
    fsHashSenha: integer;
    fsCNPJSWOK: boolean;
    TipoCMD: string;
    pCanClose: boolean;
    fsSLPrecos: TStringList;
    fsDTPrecos: integer;

    FWasHidden: Boolean;
    FLastHandle: Integer;
    function IsVisible : Boolean; virtual;

    procedure DesInicializar;
    procedure Inicializar;
    procedure EscondeConfig;
    procedure ExibeConfig;

    procedure LerSW;
    function LerChaveSWH: ansistring;
    procedure SalvarSW;

    procedure Processar;
    procedure Resposta(Comando, Resposta: ansistring);

    procedure AddLinesLog(aLineLog : String);
    procedure AddLinesLog(aLinesLog : TStrings);
    procedure AddLinesLogFile(const ArqFileLog: String; aLineLogFile: AnsiString;
      const AppendIfExists : Boolean = True; const AddLineBreak : Boolean = True;
      const PrintDateTime: Boolean = False);

    procedure SetDisWorking(const Value: boolean);

    procedure LeDadosRedeSAT;
    procedure ConfiguraRedeSAT;

    procedure SetColorButtons(Sender: TObject);
    procedure SetColorSubButtons(Sender: TObject);
    procedure SetPanel(Sender: TPanel);
    procedure SetSize25(Sender: TObject);
    procedure SetScroll(Sender: TObject);

  protected
    procedure MostraLogoBanco;
  public
    Conexao: TTCPBlockSocket;

    procedure CarregaArquivosRetorno;
    procedure DefineTextoTrayTitulo;
    procedure SetDFeSSLType;
    property DISWorking: boolean read fsDisWorking write SetDisWorking;

    procedure SalvarConfBoletos;

    procedure AvaliaEstadoTsECF;
    procedure AvaliaEstadoTsGAV;
    procedure AvaliaEstadoTsLCB;
    procedure AvaliaEstadoTsRFD;
    procedure AvaliaEstadoTsBAL;
    procedure AvaliaEstadoTsTC;

    procedure AjustaLinhasLog;

    procedure LerIni;
    procedure SalvarIni;
    procedure ConfiguraDANFe(GerarPDF, MostrarPreview : Boolean);
    procedure VerificaDiretorios;
    procedure LimparResp;
    procedure ExibeResp(Documento: ansistring);

    procedure AjustaACBrSAT ;
    procedure TrataErrosSAT(Sender : TObject ; E : Exception) ;
    procedure PrepararImpressaoSAT(NomeImpressora : string = ''; GerarPDF : boolean = false);

    procedure ConfiguraPosPrinter(SerialParams : String = '');
    procedure SetComumConfig(Configuracoes : TConfiguracoes) ;
    procedure AtualizaSSLLibsCombo ;

    procedure CarregarDFe( XMLorFile : String; var PathDfe: String; tipoDFe : TDFeCarregar = tDFeNFe); overload;
    procedure CarregarDFe( XMLsorFiles : TStrings; var PathDfe: String; tipoDFe : TDFeCarregar = tDFeNFe); overload;

    procedure AntesDeImprimir(ShowPreview: Boolean = true);
    procedure DepoisDeImprimir;
    procedure HelptabSheet;
    procedure ValidarIntegradorNFCe(ChaveNFe: String = '');
  end;

var
  FrmACBrMonitor: TFrmACBrMonitor;
  sVersaoACBr : string;

implementation

uses IniFiles, TypInfo, LCLType, strutils,
  UtilUnit, pcnAuxiliar,
  DoECFUnit, DoGAVUnit, DoCHQUnit, DoDISUnit, DoLCBUnit, DoACBrUnit, DoBALUnit,
  DoBoletoUnit, DoCEPUnit, DoIBGEUnit,
  {$IFDEF MSWINDOWS} sndkey32, {$ENDIF}
  {$IFDEF LINUX} unix, baseunix, termio, {$ENDIF}
  ACBrECFNaoFiscal, ACBrUtil, ACBrConsts, Math, Sobre, DateUtils,
  ConfiguraSerial, DoECFBemafi32, DoECFObserver, DoETQUnit, DoEmailUnit,
  DoSedexUnit, DoNcmUnit, DoACBrNFeUnit, DoACBrMDFeUnit, DoACBrCTeUnit,
  DoSATUnit, DoPosPrinterUnit, DoACBrGNReUnit, ACBrSATExtratoClass,
  SelecionarCertificado, ACBrNFeConfiguracoes,
  ACBrCTeConfiguracoes, ACBrMDFeConfiguracoes, ACBrGNREConfiguracoes;

{$R *.lfm}

{ TCores }

class constructor TCores.Create;
begin
  Buttons := clBtnFace;
  ButtonSelected := $00CACA00;
  ButtonMouseEnter := clGray;

  SubButtons := $00FFFFB3;
  SubButtonSelected := clWhite;
  SubButtonMouseEnter := clGray;
end;

{-------------------------------- TFrmACBrMonitor -----------------------------}
procedure TFrmACBrMonitor.FormCreate(Sender: TObject);
var
  iECF: TACBrECFModelo;
  iCHQ: TACBrCHQModelo;
  iDIS: TACBrDISModelo;
  iBAL: TACBrBALModelo;
  iCEP: TACBrCEPWebService;
  IBanco: TACBrTipoCobranca;
  iSAT: TACBrSATModelo;
  iTipo: TpcnTipoAmbiente;
  iRegISSQN: TpcnRegTribISSQN;
  iRatISSQN: TpcnindRatISSQN;
  iRegTrib: TpcnRegTrib;
  AppDir: string;
  ILayout: TACBrBolLayOut;
  iImpressoraESCPOS: TACBrPosPrinterModelo;
  iPagCodigoESCPOS: TACBrPosPaginaCodigo;
  iTZMode: TTimeZoneModoDeteccao;
  FileVerInfo: TFileVersionInfo;
  T: TSSLLib;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  IFormaEmissaoNFe, IFormaEmissaoCTe, IFormaEmissaoGNRe,
    IFormaEmissaoMDFe: TpcnTipoEmissao;
  iETQModelo : TACBrETQModelo ;
  iETQDPI: TACBrETQDPI;
  iETQUnidade: TACBrETQUnidade;
  iETQBackFeed: TACBrETQBackFeed;
  iETQOrigem: TACBrETQOrigem;
  M: Integer;
begin
  {$IFDEF MSWINDOWS}
  WindowState := wsMinimized;
  {$ENDIF}
  {$IFDEF LINUX}
  FpUmask(0);
  {$ENDIF}

  LHelpConnector1.LHelpPath := 'lhelp'+PathDelim+'lhelp';

  // Seta as definições iniciais para navegação
  SetColorButtons(btnMonitor);

  mResp.Clear;
  mCmd.Clear;

  fsCmd := TACBrCmd.Create;
  fsProcessar := TStringList.Create;
  fsInicioLoteTXT := False;

  FWasHidden := False;
  FLastHandle:= 0;

  Inicio := True;
  ArqSaiTXT := '';
  ArqSaiTMP := '';
  ArqEntTXT := '';
  ArqLogTXT := '';
  ArqLogCompTXT := '';
  Conexao := nil;
  NewLines := '';
  DISWorking := False;

  Top := max(Screen.Height - Height - 100,1);
  Left := max(Screen.Width - Width - 50,1);

  pCanClose := False;
  fsRFDIni := '';
  fsRFDLeuParams := False;
  fsCNPJSWOK := False;

  TipoCMD := 'A'; {Tipo de Comando A - ACBr, B - Bematech, D - Daruma}

  { Definindo constantes de Verdadeiro para TrueBoolsStrs }
  SetLength(TrueBoolStrs, 5);
  TrueBoolStrs[0] := 'True';
  TrueBoolStrs[1] := 'T';
  TrueBoolStrs[2] := 'Verdadeiro';
  TrueBoolStrs[3] := 'V';
  TrueBoolStrs[4] := 'Ok';

  { Definindo constantes de Falso para FalseBoolStrs }
  SetLength(FalseBoolStrs, 3);
  FalseBoolStrs[0] := 'False';
  FalseBoolStrs[1] := 'F';
  FalseBoolStrs[2] := 'Falso';

  { Criando lista de Bancos disponiveis }
  cbxBOLBanco.Items.Clear;
  IBanco := Low(TACBrTipoCobranca);
  while IBanco <= High(TACBrTipoCobranca) do
  begin
    cbxBOLBanco.Items.Add(GetEnumName(TypeInfo(TACBrTipoCobranca), integer(IBanco)));
    Inc(IBanco);
  end;

  { Criando lista de Layouts de Boleto disponiveis }
  cbxBOLLayout.Items.Clear;
  ILayout := Low(TACBrBolLayOut);
  while ILayout <= High(TACBrBolLayOut) do
  begin
    cbxBOLLayout.Items.Add(GetEnumName(TypeInfo(TACBrBolLayOut), integer(ILayout)));
    Inc(ILayout);
  end;

  { Criando lista modelos de ECFs disponiveis }
  cbECFModelo.Items.Clear;
  cbECFModelo.Items.Add('Procurar');
  iECF := Low(TACBrECFModelo);
  while iECF <= High(TACBrECFModelo) do
  begin
    cbECFModelo.Items.Add(GetEnumName(TypeInfo(TACBrECFModelo), integer(iECF)));
    Inc(iECF);
  end;
  AvaliaEstadoTsECF;

  AvaliaEstadoTsGAV;

  AvaliaEstadoTsLCB;

  AvaliaEstadoTsTC;
  fsSLPrecos := TStringList.Create;
  fsSLPrecos.NameValueSeparator := '|';
  fsDTPrecos := 0;

  { Criando lista modelos de Impres.Cheque disponiveis }
  cbCHQModelo.Items.Clear;
  iCHQ := Low(TACBrCHQModelo);
  while iCHQ <= High(TACBrCHQModelo) do
  begin
    cbCHQModelo.Items.Add(GetEnumName(TypeInfo(TACBrCHQModelo), integer(iCHQ)));
    Inc(iCHQ);
  end;

  { Criando lista Displays disponiveis }
  cbDISModelo.Items.Clear;
  iDIS := Low(TACBrDISModelo);
  while iDIS <= High(TACBrDISModelo) do
  begin
    cbDISModelo.Items.Add(GetEnumName(TypeInfo(TACBrDISModelo), integer(iDIS)));
    Inc(iDIS);
  end;

  { Criando lista Balanças disponiveis }
  cbBALModelo.Items.Clear;
  iBAL := Low(TACBrBALModelo);
  while iBAL <= High(TACBrBALModelo) do
  begin
    cbBALModelo.Items.Add(GetEnumName(TypeInfo(TACBrBALModelo), integer(iBAL)));
    Inc(iBAL);
  end;

  { Criando lista modelos de ECFs disponiveis }
  cbCEPWebService.Items.Clear;
  iCEP := Low(TACBrCEPWebService);
  while iCEP <= High(TACBrCEPWebService) do
  begin
    cbCEPWebService.Items.Add(GetEnumName(TypeInfo(TACBrCEPWebService), integer(iCEP)));
    Inc(iCEP);
  end;

  { Carregando lista de impressoras}
  cbxBOLImpressora.Items.Clear;
  cbxBOLImpressora.Items.Assign(Printer.Printers);
  cbxImpressora.Items.Clear;
  cbxImpressora.Items.Assign(Printer.Printers);
  cbxImpressoraNFCe.Items.Clear;
  cbxImpressoraNFCe.Items.Assign(Printer.Printers);

  cbxTimeZoneMode.Items.Clear;
  For iTZMode := Low(TTimeZoneModoDeteccao) to High(TTimeZoneModoDeteccao) do
     cbxTimeZoneMode.Items.Add( GetEnumName(TypeInfo(TTimeZoneModoDeteccao), integer(iTZMode) ) ) ;

  cbSSLLib.Items.Clear ;
  For T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add( GetEnumName(TypeInfo(TSSLLib), integer(T) ) ) ;
  cbSSLLib.ItemIndex := 0 ;

  cbCryptLib.Items.Clear ;
  For U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add( GetEnumName(TypeInfo(TSSLCryptLib), integer(U) ) ) ;
  cbCryptLib.ItemIndex := 0 ;

  cbHttpLib.Items.Clear ;
  For V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add( GetEnumName(TypeInfo(TSSLHttpLib), integer(V) ) ) ;
  cbHttpLib.ItemIndex := 0 ;

  cbXmlSignLib.Items.Clear ;
  For X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add( GetEnumName(TypeInfo(TSSLXmlSignLib), integer(X) ) ) ;
  cbXmlSignLib.ItemIndex := 0 ;

  cbSSLType.Items.Clear ;
  For Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add( GetEnumName(TypeInfo(TSSLType), integer(Y) ) ) ;
  cbSSLType.ItemIndex := 0 ;

  {SAT}
  cbxModeloSAT.Items.Clear;
  For iSAT := Low(TACBrSATModelo) to High(TACBrSATModelo) do
     cbxModeloSAT.Items.Add( GetEnumName(TypeInfo(TACBrSATModelo), integer(iSAT) ) ) ;

  cbxAmbiente.Items.Clear ;
  For iTipo := Low(TpcnTipoAmbiente) to High(TpcnTipoAmbiente) do
     cbxAmbiente.Items.Add( GetEnumName(TypeInfo(TpcnTipoAmbiente), integer(iTipo) ) ) ;

  cbxRegTribISSQN.Items.Clear ;
  For iRegISSQN := Low(TpcnRegTribISSQN) to High(TpcnRegTribISSQN) do
     cbxRegTribISSQN.Items.Add( GetEnumName(TypeInfo(TpcnRegTribISSQN), integer(iRegISSQN) ) ) ;

  cbxIndRatISSQN.Items.Clear ;
  For iRatISSQN := Low(TpcnindRatISSQN) to High(TpcnindRatISSQN) do
     cbxIndRatISSQN.Items.Add( GetEnumName(TypeInfo(TpcnindRatISSQN), integer(iRatISSQN) ) ) ;

  cbxRegTributario.Items.Clear ;
  For iRegTrib := Low(TpcnRegTrib) to High(TpcnRegTrib) do
     cbxRegTributario.Items.Add( GetEnumName(TypeInfo(TpcnRegTrib), integer(iRegTrib) ) ) ;

  Application.OnException := @TrataErrosSAT ;

  {ETQ}
  cbETQModelo.Items.Clear ;
  For iETQModelo := Low(TACBrETQModelo) to High(TACBrETQModelo) do
     cbETQModelo.Items.Add( GetEnumName(TypeInfo(TACBrETQModelo), integer(iETQModelo) ) ) ;

  cbDPI.Items.Clear ;
  For iETQDPI := Low(TACBrETQDPI) to High(TACBrETQDPI) do
     cbDPI.Items.Add( GetEnumName(TypeInfo(TACBrETQDPI), integer(iETQDPI) ) ) ;

  cbBackFeed.Items.Clear ;
  For iETQBackFeed := Low(TACBrETQBackFeed) to High(TACBrETQBackFeed) do
     cbBackFeed.Items.Add( GetEnumName(TypeInfo(TACBrETQBackFeed), integer(iETQBackFeed) ) ) ;

  cbOrigem.Items.Clear ;
  For iETQOrigem := Low(TACBrETQOrigem) to High(TACBrETQOrigem) do
     cbOrigem.Items.Add( GetEnumName(TypeInfo(TACBrETQOrigem), integer(iETQOrigem) ) ) ;

  cbETQPorta.Items.Clear;
  ACBrETQ1.Device.AcharPortasSeriais( cbETQPorta.Items );
  cbETQPorta.Items.Add('LPT1') ;
  cbETQPorta.Items.Add('LPT2') ;
  cbETQPorta.Items.Add('\\localhost\L42') ;
  cbETQPorta.Items.Add('c:\temp\ecf.txt') ;
  cbETQPorta.Items.Add('TCP:192.168.0.31:9100') ;

  For M := 0 to Printer.Printers.Count-1 do
    cbETQPorta.Items.Add('RAW:'+Printer.Printers[M]);

  cbETQPorta.Items.Add('/dev/ttyS0') ;
  cbETQPorta.Items.Add('/dev/ttyS1') ;
  cbETQPorta.Items.Add('/dev/ttyUSB0') ;
  cbETQPorta.Items.Add('/dev/ttyUSB1') ;
  cbETQPorta.Items.Add('/tmp/ecf.txt') ;

  cbDPI.ItemIndex := 0;
  cbETQModelo.ItemIndex := 3;
  cbETQPorta.ItemIndex := 0;

  {PosPrinter}
  cbxModelo.Items.Clear;
  for iImpressoraESCPOS := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
    cbxModelo.Items.Add(GetEnumName(TypeInfo(TACBrPosPrinterModelo), Integer(iImpressoraESCPOS)));

  cbxPagCodigo.Items.Clear;
  for iPagCodigoESCPOS := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
    cbxPagCodigo.Items.Add(GetEnumName(TypeInfo(TACBrPosPaginaCodigo), Integer(iPagCodigoESCPOS)));

  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais(cbxPorta.Items);
  cbxPorta.Items.Add('LPT1');
  cbxPorta.Items.Add('LPT2');
  cbxPorta.Items.Add('/dev/ttyS0');
  cbxPorta.Items.Add('/dev/ttyS1');
  cbxPorta.Items.Add('/dev/ttyUSB0');
  cbxPorta.Items.Add('/dev/ttyUSB1');
  cbxPorta.Items.Add('\\localhost\Epson');
  cbxPorta.Items.Add('c:\temp\ecf.txt');
  cbxPorta.Items.Add('/temp/ecf.txt');

  cbFormaEmissaoNFe.Items.Clear;
  for IFormaEmissaoNFe := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
    cbFormaEmissaoNFe.Items.Add(GetEnumName(TypeInfo(TpcnTipoEmissao), integer(IFormaEmissaoNFe)));
  cbFormaEmissaoNFe.ItemIndex := 0;

  cbFormaEmissaoCTe.Items.Clear;
  for IFormaEmissaoCTe := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
    cbFormaEmissaoCTe.Items.Add(GetEnumName(TypeInfo(TpcnTipoEmissao), integer(IFormaEmissaoCTe)));
  cbFormaEmissaoCTe.ItemIndex := 0;

  cbFormaEmissaoGNRe.Items.Clear;
  for IFormaEmissaoGNRe := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
    cbFormaEmissaoGNRe.Items.Add(GetEnumName(TypeInfo(TpcnTipoEmissao), integer(IFormaEmissaoGNRe)));
  cbFormaEmissaoGNRe.ItemIndex := 0;

  cbFormaEmissaoMDFe.Items.Clear;
  for IFormaEmissaoMDFe := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
    cbFormaEmissaoMDFe.Items.Add(GetEnumName(TypeInfo(TpcnTipoEmissao), integer(IFormaEmissaoMDFe)));
  cbFormaEmissaoMDFe.ItemIndex := 0;

  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(0);
    FileVerInfo.ReadFileInfo;
    sVersaoACBr := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;

  DefineTextoTrayTitulo;

  {$IFDEF LINUX}
  rbLCBTeclado.Caption := 'Dispositivo';
  cbLCBSufixo.Hint := 'Use a Sinaxe:  #NNN' + sLineBreak +
    'Onde: NNN = Numero do caracter ASC em Decimal' + sLineBreak +
    '      a adicionar no final do código lido.' + sLineBreak +
    sLineBreak + 'Para vários caracteres use a , (virgula) como separador';
  cbLCBSufixo.Items.Clear;
  cbLCBSufixo.Items.Add('#13 | Enter');
  cbLCBSufixo.Items.Add('#10 | LF');
  cbLCBSufixo.Items.Add('#13,#13 | 2 x Enter');
  cbLCBSufixo.Items.Add('#18 | PgUp');
  cbLCBSufixo.Items.Add('#09 | Tab');
  cbLCBSufixo.Items.Add('#24 | Down');
  {$ELSE}
  lAdSufixo.Caption := 'Adicionar Sufixo "SndKey32"';
  {$ENDIF}
  lAdSufixo.Hint := cbLCBSufixo.Hint;

  chRFD.Font.Style := chRFD.Font.Style + [fsBold];
  chRFD.Font.Color := clRed;

  deBOLDirLogo.Text := ExtractFilePath(Application.ExeName) + 'Logos' + PathDelim;
  cbxBOLF_JChange(Self);

  pgBoleto.ActivePageIndex := 0;
  pgCadastro.ActivePageIndex := 0;
  pgConfig.ActivePageIndex := 0;
  pgConRFD.ActivePageIndex := 0;
  pgDFe.ActivePageIndex := 0;
  pgImpressaoDFe.ActivePageIndex := 0;
  pgTestes.ActivePageIndex := 0;
  pgSAT.ActivePageIndex := 0;
  pgECFParams.ActivePageIndex := 0;
  pgSwHouse.ActivePageIndex := 0;
  pgTipoWebService.ActivePageIndex := 0;
  pgEmailDFe.ActivePageIndex := 0;

  Application.Title := Caption;

  AppDir := ExtractFilePath(Application.ExeName);

  if FileExists(AppDir + 'banner_acbrmonitor.gif') then
  begin
    ACBrGIF1.LoadFromFile(AppDir + 'banner_acbrmonitor.gif');
    ACBrGIF1.Transparent := True;
    ACBrGIF1.Start;
  end
  else
    ACBrGIF1.Visible := False;

  pgConfig.ShowTabs := False;
  Timer1.Enabled := True;
end;

procedure TFrmACBrMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DesInicializar;

  Timer1.Enabled := False;
  TimerTC.Enabled := False;

  TcpServer.OnDesConecta := nil;
  TCPServerTC.OnDesConecta := nil;
end;

procedure TFrmACBrMonitor.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  AddLinesLog(E.Message);

  StatusBar1.Panels[0].Text := 'Exception';
end;

procedure TFrmACBrMonitor.ACBrEAD1GetChavePrivada(var Chave: ansistring);
begin
  Chave := LerChaveSWH;

  if Chave = '' then
  begin
    Chave := mRSAKey.Text;
    if copy(Chave, 1, 5) <> '-----' then
      Chave := '';
  end;

  if Chave = '' then
    raise Exception.Create('Chave RSA Privada não especificada.' +
      sLineBreak + '- Selecione a aba "Chave RSA"' + sLineBreak +
      '- Calcule sua Chave Privada' + sLineBreak + '- Salve as configurações' +
      sLineBreak + '- Distribua a sua Chave Privada com o arquivo ' +
      sLineBreak + '  criptografado "swh.ini"');
end;

procedure TFrmACBrMonitor.ACBrEAD1GetChavePublica(var Chave: ansistring);
begin
  Chave := ACBrEAD1.CalcularChavePublica;
  Chave := StringReplace(Chave, #10, sLineBreak, [rfReplaceAll]);
end;

procedure TFrmACBrMonitor.ACBrGIF1Click(Sender: TObject);
begin
  OpenURL('http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html');
end;

procedure TFrmACBrMonitor.ACBrMail1MailProcess(const AMail: TACBrMail;
  const aStatus: TMailStatus);
begin
  pbEmailTeste.Position := integer(aStatus);
  case aStatus of
    pmsStartProcess:
      AddLinesLog('Email: Iniciando processo de envio.');
    pmsConfigHeaders:
      AddLinesLog('Email: Configurando o cabeçalho do e-mail.');
    pmsLoginSMTP:
      AddLinesLog('Email: Logando no servidor de e-mail.');
    pmsStartSends:
      AddLinesLog('Email: Iniciando os envios.');
    pmsSendTo:
      AddLinesLog('Email: Processando lista de destinatários.');
    pmsSendData:
      AddLinesLog('Email: Enviando dados.');
    pmsLogoutSMTP:
      AddLinesLog('Email: Fazendo Logout no servidor de e-mail.');
    pmsDone, pmsError:
    begin
      bEmailTestarConf.Enabled := True;
      bCancelar.Enabled := True;
      bConfig.Enabled := True;
      pbEmailTeste.Visible := False;
      Screen.Cursor := crDefault;

      if aStatus = pmsError then
        AddLinesLog(ACBrMail1.GetLastSmtpError)
      else
        AddLinesLog('Email: Enviado com sucesso');
    end;
  end;
end;

procedure TFrmACBrMonitor.ACBrNFe1GerarLog(const ALogLine: string;
  var Tratado: boolean);
begin
  if cbLogComp.Checked then
    AddLinesLogFile(ArqLogCompTXT, ALogLine);
end;

procedure TFrmACBrMonitor.ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := AnsiString( edtCodigoAtivacao.Text );
end;

procedure TFrmACBrMonitor.ACBrSAT1GetNumeroSessao(var NumeroSessao: Integer);
begin
  if ACBrSAT1.Tag <> 0 then
    NumeroSessao := ACBrSAT1.Tag;

  ACBrSAT1.Tag := 0;
end;

procedure TFrmACBrMonitor.ACBrSAT1GetsignAC(var Chave: AnsiString);
begin
  Chave := AnsiString( edtSwHAssinatura.Text );
end;

procedure TFrmACBrMonitor.ACBrSAT1GravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  AddLinesLog(ALogLine);
  Tratado := False;
end;

procedure TFrmACBrMonitor.ApplicationProperties1Minimize(Sender: TObject);
begin
  if WindowState <> wsMinimized then
    Application.Minimize;

  if not ( cbMostrarNaBarraDeTarefas.Checked ) then
  begin
    Visible := False;
    Application.ShowMainForm := False;
  end;
end;

procedure TFrmACBrMonitor.ApplicationProperties1Restore(Sender: TObject);
begin
  Application.BringToFront;
end;

procedure TFrmACBrMonitor.bAtivarClick(Sender: TObject);
begin
//
end;

procedure TFrmACBrMonitor.bCEPTestarClick(Sender: TObject);
var
  AMsg: string;
  I: integer;
begin
  with ACBrCEP1 do
  begin
    WebService := TACBrCEPWebService(cbCEPWebService.ItemIndex);
    ChaveAcesso := edCEPChaveBuscarCEP.Text;
    ProxyHost := edCONProxyHost.Text;
    ProxyPort := edCONProxyPort.Text;
    ProxyUser := edCONProxyUser.Text;
    ProxyPass := edCONProxyPass.Text;

    if BuscarPorCEP(edCEPTestar.Text) > 0 then
    begin
      AMsg := IntToStr(Enderecos.Count) + ' Endereço(s) encontrado(s)' +
        sLineBreak + sLineBreak;

      for I := 0 to Enderecos.Count - 1 do
      begin
        with Enderecos[I] do
        begin
          AMsg := AMsg + 'CEP: ' + CEP + sLineBreak + 'Logradouro: ' +
            Tipo_Logradouro + ' ' + Logradouro + sLineBreak +
            'Complemento: ' + Complemento + sLineBreak + 'Bairro: ' +
            Bairro + sLineBreak + 'Municipio: ' + Municipio + ' - IBGE: ' +
            IBGE_Municipio + sLineBreak + 'UF: ' + UF + ' - IBGE: ' +
            IBGE_UF + sLineBreak + sLineBreak;
        end;
      end;
    end
    else
      AMsg := 'Nenhum Endereço encontrado';

    MessageDlg(AMsg, mtInformation, [mbOK], 0);
  end;
end;

procedure TFrmACBrMonitor.bDownloadListaClick(Sender: TObject);
var
  DirNcmSalvar: string;
begin
  if (deNcmSalvar.Text = '') then
    DirNcmSalvar := ExtractFilePath(Application.ExeName)
  else
    DirNcmSalvar := PathWithoutDelim(deNcmSalvar.Text);

  DirNcmSalvar := DirNcmSalvar + PathDelim + 'ListaNCM.csv';

  with ACBrNCMs1 do
  begin
    ListarNcms();
    NCMS.SaveToFile(DirNcmSalvar);
  end;

  AddLinesLog('Arquivo salvo em: ' + DirNcmSalvar);
end;

procedure TFrmACBrMonitor.bEmailTestarConfClick(Sender: TObject);
var
  Teste: string;

begin
  if (Trim(edEmailEndereco.Text) = '') or not ValidarEmail(edEmailEndereco.Text) then
  begin
    MessageDlg('Atenção',
      'O endereço de E-mail informado não é Válido ou não foi Preenchido',
      mtWarning, [mbOK], '');
    edEmailEndereco.SetFocus;
    Exit;
  end;

  if Trim(edEmailHost.Text) = '' then
  begin
    MessageDlg('Atenção', 'Host SMTP não informado', mtWarning, [mbOK], '');
    edEmailHost.SetFocus;
    Exit;
  end;

  if (edEmailPorta.Value = 0) then
  begin
    MessageDlg('Atenção', 'A Porta SMTP informada não é Válida', mtWarning, [mbOK], '');
    edEmailPorta.SetFocus;
    Exit;
  end;

  Application.ProcessMessages;

  with ACBrMail1 do
  begin
    Attempts := 1;
    FromName := edEmailNome.Text;
    From := edEmailEndereco.Text;
    Username := edEmailUsuario.Text;
    Password := edEmailSenha.Text;
    Host := edEmailHost.Text;
    Teste := IntToStr(edEmailPorta.Value);
    Port := Teste;
    SetSSL := cbEmailSsl.Checked;
    SetTLS := cbEmailTls.Checked;
    ReadingConfirmation := cbEmailConfirmation.Checked;
    UseThread := cbEmailThread.Checked;
    DefaultCharset := TMailCharset(GetEnumValue(TypeInfo(TMailCharset),
      cbEmailCodificacao.Text));

    AddAddress(edEmailEndereco.Text);
    Subject := 'ACBrMonitor : Teste de Configuração de Email';

    Body.Add('Se você consegue ler esta mensagem, significa que suas configurações');
    Body.Add('de SMTP estão corretas.');
    Body.Add('');
    Body.Add('ACBrMonitor');
    Body.Add('http://www.projetoacbr.com.br/');

    bEmailTestarConf.Enabled := False;
    bCancelar.Enabled := False;
    bConfig.Enabled := False;
    pbEmailTeste.Visible := True;
    pbEmailTeste.Position := 1;
    Screen.Cursor := crHourGlass;
    IsHTML := True;
    Application.ProcessMessages;
    try
      Send(False);
      MessageDlg('EMAIL','Email enviado com sucesso',mtInformation,[mbOK],0);
    except
      bEmailTestarConf.Enabled := True;
      bCancelar.Enabled := True;
      bConfig.Enabled := True;
      pbEmailTeste.Visible := False;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TFrmACBrMonitor.bIBGETestarClick(Sender: TObject);
var
  AMsg: string;
  I, Cod: integer;
begin
  with ACBrIBGE1 do
  begin
    ProxyHost := edCONProxyHost.Text;
    ProxyPort := edCONProxyPort.Text;
    ProxyUser := edCONProxyUser.Text;
    ProxyPass := edCONProxyPass.Text;

    Cod := StrToIntDef(edIBGECodNome.Text, 0);

    if Cod > 0 then
      I := BuscarPorCodigo(Cod)
    else
      I := BuscarPorNome(edIBGECodNome.Text);

    if I > 0 then
    begin
      AMsg := IntToStr(Cidades.Count) + ' Cidade(s) encontrada(s)' +
        sLineBreak + sLineBreak;

      for I := 0 to Cidades.Count - 1 do
      begin
        with Cidades[I] do
        begin
          AMsg := AMsg + 'Cod UF: ' + IntToStr(CodUF) + sLineBreak +
            'UF: ' + UF + sLineBreak + 'Cod.Município: ' + IntToStr(CodMunicipio) +
            sLineBreak + 'Município: ' + Municipio + sLineBreak +
            'Área: ' + FormatFloat('0.00', Area) + sLineBreak + sLineBreak;
        end;
      end;
    end
    else
      AMsg := 'Nenhuma Cidade encontrada';

    MessageDlg(AMsg, mtInformation, [mbOK], 0);
  end;
end;

procedure TFrmACBrMonitor.bImpressoraClick(Sender: TObject);
begin
  if PrintDialog1.Execute then
    lImpressora.Caption := Printer.PrinterName ;
end;

procedure TFrmACBrMonitor.bInicializarClick(Sender: TObject);
begin
  AjustaACBrSAT;

  ACBrSAT1.Inicializado := not ACBrSAT1.Inicializado ;

  if ACBrSAT1.Inicializado then
    bInicializar.Caption := 'DesInicializar'
  else
    bInicializar.Caption := 'Inicializar' ;

end;

procedure TFrmACBrMonitor.bNcmConsultarClick(Sender: TObject);
var
  AMsg: string;
begin
  AMsg := '';
  with ACBrNCMs1 do
  begin
    if (Length(OnlyNumber(edtNcmNumero.Text)) <> 8) then
      raise Exception.Create('O codigo do NCM deve conter 8 Caracteres');

    if validar(OnlyNumber(edtNcmNumero.Text)) then
      AMsg := 'OK: NCM Valido'
    else
      AMsg := 'Erro: NCM Invalido';
  end;

  AddLinesLog(AMsg);
end;

procedure TFrmACBrMonitor.bRSAeECFcClick(Sender: TObject);
var
  NomeSH, ArqXML: string;
begin
  NomeSH := edSH_RazaoSocial.Text;
  if NomeSH = '' then
    NomeSH := 'Sua SoftwareHouse';

  if not InputQuery('Sw.House', 'Entre com o nome da Sw.House', NomeSH) then
    exit;

  if SelectDirectoryDialog1.Execute then
  begin
    ArqXML := PathWithDelim(SelectDirectoryDialog1.FileName) + NomeSH + '.xml';
    if FileExists(ArqXML) then
      if MessageDlg('Arquivo já existe, sobrescrever ?', mtConfirmation,
        mbYesNoCancel, 0) <> mrYes then
        exit;

    if ACBrEAD1.GerarXMLeECFc(NomeSH, SelectDirectoryDialog1.FileName) then
      MessageDlg('Arquivo: ' + ArqXML + ' criado', mtInformation, [mbOK], 0);
  end;
end;

procedure TFrmACBrMonitor.bSedexRastrearClick(Sender: TObject);
var
  CodRastreio: string;
begin
  CodRastreio := '';

  if not InputQuery('Código de Rastreio', 'Entre com o Código de Rastreio',
    CodRastreio) then
    exit;

  ACBrSedex1.Rastrear(CodRastreio);
  AddLinesLog(ProcessarRespostaRastreio);
end;

procedure TFrmACBrMonitor.bSedexTestarClick(Sender: TObject);
var
  AMsg: string;
begin
  AMsg := '';

  with ACBrSedex1 do
  begin
    Formato := TACBrTpFormato(cbxSedexFormato.ItemIndex);
    MaoPropria := (cbxSedexMaoPropria.ItemIndex = 0);
    AvisoRecebimento := (cbxSedexAvisoReceb.ItemIndex = 0);
    Servico := TACBrTpServico(cbxSedexAvisoReceb.ItemIndex);
    CodContrato := edtSedexContrato.Text;
    Senha := edtSedexSenha.Text;
    CepOrigem := edtSedexCEPOrigem.Text;
    CepDestino := edtSedexCEPDestino.Text;
    Peso := StrToFloatDef(edtSedexPeso.Text, 0);
    Comprimento := StrToFloatDef(edtSedexComprimento.Text, 0);
    Largura := StrToFloatDef(edtSedexLargura.Text, 0);
    Altura := StrToFloatDef(edtSedexAltura.Text, 0);
    Diametro := StrToFloatDef(edtSedexDiametro.Text, 0);
    ValorDeclarado := StrToFloatDef(edtSedexValorDeclarado.Text, 0);

    if ACBrSedex1.Consultar then
      AMsg := ProcessarRespostaSedex
    else
      AMsg := 'Erro na consulta';
  end;

  AddLinesLog(AMsg);
end;

procedure TFrmACBrMonitor.btAtivarsatClick(Sender: TObject);
var
  ACNPJ: String;
begin
  ACNPJ := OnlyNumber(edtEmitCNPJ.Text);
  if ACNPJ = '' then
    raise Exception.Create('CNPJ inválido. Configure a aba "Dados Emitente"');

  ACBrSAT1.AtivarSAT(1, ACNPJ, StrToInt(edtCodUF.Text) );
end;

procedure TFrmACBrMonitor.btConsultarStatusOPSATClick(Sender: TObject);
begin
  ACBrSAT1.ConsultarStatusOperacional;

  with ACBrSAT1.Status do
  begin
    AddLinesLog('NSERIE.........: '+NSERIE);
    AddLinesLog('LAN_MAC........: '+LAN_MAC);
    AddLinesLog('STATUS_LAN.....: '+StatusLanToStr(STATUS_LAN));
    AddLinesLog('NIVEL_BATERIA..: '+NivelBateriaToStr(NIVEL_BATERIA));
    AddLinesLog('MT_TOTAL.......: '+MT_TOTAL);
    AddLinesLog('MT_USADA.......: '+MT_USADA);
    AddLinesLog('DH_ATUAL.......: '+DateTimeToStr(DH_ATUAL));
    AddLinesLog('VER_SB.........: '+VER_SB);
    AddLinesLog('VER_LAYOUT.....: '+VER_LAYOUT);
    AddLinesLog('ULTIMO_CFe.....: '+ULTIMO_CFe);
    AddLinesLog('LISTA_INICIAL..: '+LISTA_INICIAL);
    AddLinesLog('LISTA_FINAL....: '+LISTA_FINAL);
    AddLinesLog('DH_CFe.........: '+DateTimeToStr(DH_CFe));
    AddLinesLog('DH_ULTIMA......: '+DateTimeToStr(DH_ULTIMA));
    AddLinesLog('CERT_EMISSAO...: '+DateToStr(CERT_EMISSAO));
    AddLinesLog('CERT_VENCIMENTO: '+DateToStr(CERT_VENCIMENTO));
    AddLinesLog('ESTADO_OPERACAO: '+EstadoOperacaoToStr(ESTADO_OPERACAO));
  end;

  LeDadosRedeSAT;
end;

procedure TFrmACBrMonitor.btnBalancaClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsBAL;
end;

procedure TFrmACBrMonitor.btnBoletoCedenteClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgBoleto.ActivePage := tsCedente;
end;

procedure TFrmACBrMonitor.btnBoletoClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  SetSize25(TPanel(Sender).Parent);
  pgConfig.ActivePage := tsACBrBoleto;
    SetScroll(TPanel(Sender).Parent);
  // Ativa a 1a página do pegecontrol
  btnBoletoCedenteClick(btnBoletoCedente);
end;

procedure TFrmACBrMonitor.btnBoletoContaClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgBoleto.ActivePage := tsContaBancaria;
end;

procedure TFrmACBrMonitor.btnBoletoEmailClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgBoleto.ActivePage := tsBoletoEmail;
end;

procedure TFrmACBrMonitor.btnBoletoLeiauteClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgBoleto.ActivePage := tsLayoutBoleto;
end;

procedure TFrmACBrMonitor.btnBoletoRelatorioRetornoClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgBoleto.ActivePage := tsRelatorio;
end;

procedure TFrmACBrMonitor.btnBoletoRRClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgBoleto.ActivePage := tsRemessaRetorno;
end;

procedure TFrmACBrMonitor.btnCadastroClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  SetSize25(TPanel(Sender).Parent);
  pgConfig.ActivePage := tsCadastro;
  // Ativa a 1a página do pegecontrol
  btnUserClick(btnUser);
end;

procedure TFrmACBrMonitor.btnCancelarCTeClick(Sender: TObject);
var
  idLote, vAux: string;
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a CTE';
  OpenDialog1.DefaultExt := '*-cte.XML';
  OpenDialog1.Filter := 'Arquivos CTE (*-cte.XML)|*-cte.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    idLote := '1';
    vAux := '';
    if not (InputQuery('WebServices Eventos: Cancelamento',
      'Identificador de controle do Lote de envio do Evento', idLote)) then
      exit;

    if not (InputQuery('WebServices Cancelamento', 'Justificativa', vAux)) then
      exit;

    ACBrCTe1.EventoCTe.Evento.Clear;
    ACBrCTe1.EventoCTe.idLote := StrToInt(idLote);
    with ACBrCTe1.EventoCTe.Evento.Add do
    begin
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.detEvento.xJust := vAux;
    end;
    ACBrCTe1.EnviarEvento(StrToInt(idLote));
    ExibeResp(ACBrCTe1.WebServices.EnvEvento.RetWS);
  end;

end;

procedure TFrmACBrMonitor.btnCancMDFeClick(Sender: TObject);
var
 vAux : String;
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  vAux := '';
  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    if not(InputQuery('WebServices Cancelamento', 'Justificativa', vAux))
      then exit;

    with ACBrMDFe1.EventoMDFe.Evento.Add do
    begin
      infEvento.chMDFe   := Copy(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID, 5, 44);
      infEvento.CNPJ     := ACBrMDFe1.Manifestos.Items[0].MDFe.emit.CNPJ;
      infEvento.dhEvento := now;
      infEvento.tpEvento   := teCancelamento;
      infEvento.nSeqEvento := 1;
      infEvento.detEvento.nProt := ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.nProt;
      infEvento.detEvento.xJust := trim(vAux);
    end;

    ACBrMDFe1.EnviarEvento( 1 ); // 1 = Numero do Lote
    ExibeResp(ACBrMDFe1.WebServices.EnvEvento.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnCancNFClick(Sender: TObject);
var
  idLote, vAux: string;
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a NFE';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter :=
    'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    idLote := '1';
    vAux := '';
    if not (InputQuery('WebServices Eventos: Cancelamento',
      'Identificador de controle do Lote de envio do Evento', idLote)) then
      exit;

    if not (InputQuery('WebServices Cancelamento', 'Justificativa', vAux)) then
      exit;

    ACBrNFe1.EventoNFe.Evento.Clear;
    ACBrNFe1.EventoNFe.idLote := StrToInt(idLote);
    with ACBrNFe1.EventoNFe.Evento.Add do
    begin
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.detEvento.xJust := vAux;
    end;
    ACBrNFe1.EnviarEvento(StrToInt(idLote));
    ExibeResp(ACBrNFe1.WebServices.EnvEvento.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnConsultarClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a NFE';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter :=
    'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ACBrNFe1.Consultar;
    ExibeResp(ACBrNFe1.WebServices.Consulta.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnConsultarCTeClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a CTE';
  OpenDialog1.DefaultExt := '*-cte.XML';
  OpenDialog1.Filter :=
    'Arquivos CTE (*-cte.XML)|*-cte.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Consultar;
    ExibeResp(ACBrCTe1.WebServices.Consulta.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnConsultarMDFeClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    ACBrMDFe1.Consultar;
    ExibeResp(ACBrMDFe1.WebServices.Consulta.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnConsultasClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsConsultas;
end;

procedure TFrmACBrMonitor.btnDFeCertificadosClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsCertificadoDFe;
end;

procedure TFrmACBrMonitor.btnDFeClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  SetSize25(TPanel(Sender).Parent);
  pgConfig.ActivePage := tsDFe;
    SetScroll(TPanel(Sender).Parent);
  // Ativa a 1a página do pegecontrol
  btnDFeGeralClick(btnDFeGeral);
end;

procedure TFrmACBrMonitor.btnDFeDirClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsDiretoriosDFe;
end;

procedure TFrmACBrMonitor.btnDFeEmailClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsEmailDFe;
end;

procedure TFrmACBrMonitor.btnDFeGeralClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsConfiguracaoDFe;
end;

procedure TFrmACBrMonitor.btnDFePrintClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsImpressaoDFe;
end;

procedure TFrmACBrMonitor.btnDFeTesteClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsTestesDFe;
end;

procedure TFrmACBrMonitor.btnDFeWebServicesClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsWebServiceDFe;
end;

procedure TFrmACBrMonitor.btnDisplayClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsDIS;
end;

procedure TFrmACBrMonitor.btnECFClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsECF;
end;

procedure TFrmACBrMonitor.btnEmailClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsEmail;
end;

procedure TFrmACBrMonitor.btnEnviarClick(Sender: TObject);
var
  vAux: string;
begin
  LimparResp;
  vAux := '';
  if not (InputQuery('WebServices Enviar', 'Numero do Lote', vAux)) then
    exit;
  OpenDialog1.Title := 'Selecione a NFE';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter :=
    'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ACBrNFe1.Enviar(StrToInt(vAux));
    ExibeResp(ACBrNFe1.WebServices.Retorno.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnEnviarCTeClick(Sender: TObject);
var
  vAux: string;
begin
  LimparResp;
  vAux := '';
  if not (InputQuery('WebServices Enviar', 'Numero do Lote', vAux)) then
    exit;
  OpenDialog1.Title := 'Selecione a CTE';
  OpenDialog1.DefaultExt := '*-cte.XML';
  OpenDialog1.Filter :=
    'Arquivos CTE (*-cte.XML)|*-cte.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Enviar(StrToInt(vAux));
    ExibeResp(ACBrCTe1.WebServices.Retorno.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnEnviarEmailClick(Sender: TObject);
var
  vPara: string;
  sAssunto: String;
  sMensagem: TStringList;
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a NFE';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter :=
    'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ConfiguraDANFe(True, False);

    vPara := '';
    if not (InputQuery('Enviar Email', 'Email de Destino', vPara)) then
      exit;

    try
      sMensagem := TStringList.Create;
      try
        sAssunto       := SubstituirVariaveis(edtEmailAssuntoNFe.Text);
        sMensagem.Text := SubstituirVariaveis(mmEmailMsgNFe.Text);

        ACBrNFe1.NotasFiscais.Items[0].EnviarEmail(
          vPara,
          sAssunto,
          sMensagem,
          True,  // Enviar PDF junto
          nil,    // Lista com emails que serão enviado cópias - TStrings
          nil
        );
      finally
        sMensagem.Free;
      end;
    except
      on E: Exception do
      begin
        raise Exception.Create('Erro ao enviar email' + sLineBreak + E.Message);
        exit;
      end;
    end;
    ShowMessage('Email enviado com sucesso!');
  end;
end;

procedure TFrmACBrMonitor.btnEnviarEmailCTeClick(Sender: TObject);
var
  vPara: string;
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a CTE';
  OpenDialog1.DefaultExt := '*-cte.XML';
  OpenDialog1.Filter := 'Arquivos CTE (*-cte.XML)|*-cte.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    vPara := '';
    if not (InputQuery('Enviar Email', 'Email de Destino', vPara)) then
      exit;

    try
      ACBrCTe1.Conhecimentos.Items[0].EnviarEmail(vPara, edtEmailAssuntoCTe.Text,
        mmEmailMsgCTe.Lines
        , True  // Enviar PDF junto
        ,
        nil    // Lista com emails que serão enviado cópias - TStrings
        , nil);
      // Lista de anexos - TStrings
    except
      on E: Exception do
      begin
        raise Exception.Create('Erro ao enviar email' + sLineBreak + E.Message);
        exit;
      end;
    end;
    ShowMessage('Email enviado com sucesso!');
  end
end;

procedure TFrmACBrMonitor.btnEnviarEmailMDFeClick(Sender: TObject);
var
  vPara: string;
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

    vPara := '';
    if not (InputQuery('Enviar Email', 'Email de Destino', vPara)) then
      exit;

    try
      ACBrMDFe1.Manifestos.Items[0].EnviarEmail(vPara, edtEmailAssuntoMDFe.Text,
        mmEmailMsgMDFe.Lines
        , True  // Enviar PDF junto
        ,
        nil    // Lista com emails que serão enviado cópias - TStrings
        , nil);
      // Lista de anexos - TStrings
    except
      on E: Exception do
      begin
        raise Exception.Create('Erro ao enviar email' + sLineBreak + E.Message);
        exit;
      end;
    end;
    ShowMessage('Email enviado com sucesso!');
  end;
end;

procedure TFrmACBrMonitor.btnEnviarMDFeClick(Sender: TObject);
  var
  vAux: string;
begin
  LimparResp;
  vAux := '';
  if not (InputQuery('WebServices Enviar', 'Numero do Lote', vAux)) then
    exit;
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    ACBrMDFe1.Enviar(StrToInt(vAux));
    ExibeResp(ACBrMDFe1.WebServices.Retorno.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnEtiquetaClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsETQ;
end;

procedure TFrmACBrMonitor.btnGavetaClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsGAV;
end;

procedure TFrmACBrMonitor.btnImpChequeClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsCHQ;
end;

procedure TFrmACBrMonitor.btnImprimirClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a NFE';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter :=
    'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ConfiguraDANFe(False, False);

    try
      AntesDeImprimir(False);
      ACBrNFe1.NotasFiscais.Imprimir;
    finally
      DepoisDeImprimir;
    end;
  end;
end;

procedure TFrmACBrMonitor.btnImprimirCTeClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a CTE';
  OpenDialog1.DefaultExt := '*-cte.XML';
  OpenDialog1.Filter := 'Arquivos CTE (*-cte.XML)|*-cte.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Conhecimentos.Imprimir;
  end;
end;

procedure TFrmACBrMonitor.btnImprimirMDFeClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    ACBrMDFe1.Manifestos.Imprimir;
  end;
end;

procedure TFrmACBrMonitor.btnIntegradorClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsIntegrador;
end;

procedure TFrmACBrMonitor.btnInutilizarClick(Sender: TObject);
var
  CNPJ, Modelo, Serie, Ano, NumeroInicial, NumeroFinal, Justificativa: string;
begin
  LimparResp;
  CNPJ := '';
  Ano := IntToStr(YearOf(Now));
  Modelo := '55';
  Serie := '1';
  NumeroInicial := '0';
  NumeroFinal := '0';
  Justificativa := '';

  if not (InputQuery('WebServices Inutilização ', 'CNPJ', CNPJ)) then
    exit;
  if not (InputQuery('WebServices Inutilização ', 'Ano', Ano)) then
    exit;
  if not (InputQuery('WebServices Inutilização ', 'Modelo', Modelo)) then
    exit;
  if not (InputQuery('WebServices Inutilização ', 'Serie', Serie)) then
    exit;
  if not (InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroInicial)) then
    exit;
  if not (InputQuery('WebServices Inutilização ', 'Número Final', NumeroFinal)) then
    exit;
  if not (InputQuery('WebServices Inutilização ', 'Justificativa', Justificativa)) then
    exit;

  ACBrNFe1.WebServices.Inutiliza(CNPJ, Justificativa, StrToInt(Ano),
    StrToInt(Modelo), StrToInt(Serie), StrToInt(NumeroInicial), StrToInt(NumeroFinal));
  ExibeResp(ACBrNFe1.WebServices.Inutilizacao.RetWS);
end;

procedure TFrmACBrMonitor.btnInutilizarCTeClick(Sender: TObject);
var
 CNPJ, Modelo, Serie, Ano, NumeroInicial, NumeroFinal, Justificativa : String;
begin
  CNPJ := '';
  Ano := IntToStr(YearOf(Now));
  Modelo := '55';
  Serie := '1';
  NumeroInicial := '0';
  NumeroFinal := '0';
  Justificativa := '';

  if not(InputQuery('WebServices Inutilização ', 'CNPJ',   CNPJ)) then
    exit;
  if not(InputQuery('WebServices Inutilização ', 'Ano',    Ano)) then
    exit;
  if not(InputQuery('WebServices Inutilização ', 'Modelo', Modelo)) then
    exit;
  if not(InputQuery('WebServices Inutilização ', 'Serie',  Serie)) then
    exit;
  if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroInicial)) then
    exit;
  if not(InputQuery('WebServices Inutilização ', 'Número Final', NumeroFinal)) then
    exit;
  if not(InputQuery('WebServices Inutilização ', 'Justificativa', Justificativa)) then
    exit;

  ACBrCTe1.WebServices.Inutiliza(CNPJ, Justificativa, StrToInt(Ano), StrToInt(Modelo), StrToInt(Serie), StrToInt(NumeroInicial), StrToInt(NumeroFinal));
  ExibeResp(ACBrCTe1.WebServices.Inutilizacao.RetWS);
end;

procedure TFrmACBrMonitor.btnLeitorSerialClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsLCB;
end;

procedure TFrmACBrMonitor.btnMonitorClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsMonitor;
end;

procedure TFrmACBrMonitor.btnNCMClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsNcm;
end;

procedure TFrmACBrMonitor.btnPosPrinterClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsEscPos;
end;

procedure TFrmACBrMonitor.btnRFDClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  SetSize25(TPanel(Sender).Parent);
  pgConfig.ActivePage := tsRFD;
    SetScroll(TPanel(Sender).Parent);
  // Ativa a 1a página do pegecontrol
  btnRFDGeralClick(btnRFDGeral);
end;

procedure TFrmACBrMonitor.btnRFDFileClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgConRFD.ActivePage := tsRFDINI;
end;

procedure TFrmACBrMonitor.btnRFDGeralClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgConRFD.ActivePage := tsRFDConfig;
end;

procedure TFrmACBrMonitor.btnSATClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  SetSize25(TPanel(Sender).Parent);
  pgConfig.ActivePage := tsSat;
  // Ativa a 1a página do pegecontrol
  btnSATDadosClick(btnSATDados);
end;

procedure TFrmACBrMonitor.btnSATDadosClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  SetScroll(TPanel(Sender).Parent);
  pgSAT.ActivePage := tsDadosSAT;
end;

procedure TFrmACBrMonitor.btnSATEmitenteClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgSAT.ActivePage := tsDadosEmit;
end;

procedure TFrmACBrMonitor.btnSATPrintClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgSAT.ActivePage := Impressao;
end;

procedure TFrmACBrMonitor.btnSATRedeClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgSAT.ActivePage := tsRede;
end;

procedure TFrmACBrMonitor.btnSATSHClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgSAT.ActivePage := tsDadosSwHouse;
end;

procedure TFrmACBrMonitor.btnSedexClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsSEDEX;
end;

procedure TFrmACBrMonitor.btnSHClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgCadastro.ActivePage := tsCadSwH;
end;

procedure TFrmACBrMonitor.btnStatusServClick(Sender: TObject);
begin
  LimparResp;
  ACBrNFe1.WebServices.StatusServico.Executar;
  ExibeResp(ACBrNFe1.WebServices.StatusServico.RetWS);
end;

procedure TFrmACBrMonitor.btnStatusServCTeClick(Sender: TObject);
begin
  LimparResp;
  ACBrCTe1.WebServices.StatusServico.Executar;
  ExibeResp(ACBrCTe1.WebServices.StatusServico.RetWS);
end;

procedure TFrmACBrMonitor.btnStatusServMDFeClick(Sender: TObject);
begin
  LimparResp;
  ACBrMDFe1.WebServices.StatusServico.Executar;
  ExibeResp(ACBrMDFe1.WebServices.StatusServico.RetWS);
end;

procedure TFrmACBrMonitor.btnTCClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsTC;
end;

procedure TFrmACBrMonitor.btnUserClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgCadastro.ActivePage := tsCadUsuario;
end;

procedure TFrmACBrMonitor.btnValidarXMLClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a NFE';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter :=
    'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ACBrNFe1.NotasFiscais.Validar;
    ShowMessage('Nota Fiscal Eletrônica Valida');
  end;
end;

procedure TFrmACBrMonitor.btnValidarXMLCTeClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a CTE';
  OpenDialog1.DefaultExt := '*-cte.XML';
  OpenDialog1.Filter :=
    'Arquivos CTE (*-cte.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Conhecimentos.Validar;
    ShowMessage('Conhecimento de Transporte Eletrônico Valido');
  end;
end;

procedure TFrmACBrMonitor.btnValidarXMLMDFeClick(Sender: TObject);
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;
  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    ACBrMDFe1.Manifestos.Validar;
    showmessage('Manifesto Eletrônico de Documentos Fiscais Valido');
  end;
end;

procedure TFrmACBrMonitor.btSATAssociaClick(Sender: TObject);
begin
  ACBrSAT1.AssociarAssinatura( edtSwHCNPJ.Text + edtEmitCNPJ.Text, edtSwHAssinatura.Text );
end;

procedure TFrmACBrMonitor.btSATConfigRedeClick(Sender: TObject);
begin
  ConfiguraRedeSAT;

  AddLinesLog(ACBrSAT1.ConfigurarInterfaceDeRede(ACBrSAT1.Rede.AsXMLString));
end;

procedure TFrmACBrMonitor.cbLogCompClick(Sender: TObject);
begin
  gbLogComp.Enabled := cbLogComp.Checked;

  if cbLogComp.Checked and (edLogComp.Text = '') then
    edLogComp.Text := 'LOG_NFE.TXT';
end;

procedure TFrmACBrMonitor.cbUsarEscPosClick(Sender: TObject);
begin
 cbUsarFortes.Checked := False;
 ACBrSAT1.Extrato := ACBrSATExtratoESCPOS1;
end;

procedure TFrmACBrMonitor.cbUsarFortesClick(Sender: TObject);
begin
  cbUsarEscPos.Checked := False;
  ACBrSAT1.Extrato := ACBrSATExtratoFortes1
end;

procedure TFrmACBrMonitor.cbxBOLBancoChange(Sender: TObject);
begin
  MostraLogoBanco;
end;

procedure TFrmACBrMonitor.cbxBOLF_JChange(Sender: TObject);
begin
  if cbxBOLF_J.ItemIndex = 0 then
  begin
    lblBOLCPFCNPJ.Caption := 'C.P.F';
    lblBOLNomeRazao.Caption := 'Nome';
    //edtBOLCNPJ.EditMask := '999.999.999-99;1';
  end
  else
  begin
    lblBOLCPFCNPJ.Caption := 'C.N.P.J';
    lblBOLNomeRazao.Caption := 'Razão Social';
    // edtBOLCNPJ.EditMask := '99.999.999/9999-99;1';
  end;
end;

procedure TFrmACBrMonitor.cbCEPWebServiceChange(Sender: TObject);
begin
  ACBrCEP1.WebService := TACBrCEPWebService(cbCEPWebService.ItemIndex);
  edCEPChaveBuscarCEP.Enabled := (ACBrCEP1.WebService in [wsBuscarCep, wsCepLivre]);
end;

procedure TFrmACBrMonitor.cbxImpDescPorcChange(Sender: TObject);
begin
  cbxImpValLiq.Enabled := not cbxImpDescPorc.Checked;
  if not cbxImpValLiq.Enabled then
    cbxImpValLiq.Checked := False;
end;

procedure TFrmACBrMonitor.cbXMLSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
    begin
      ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrCTe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrMDFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrGNRE1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrBlocoX1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
    end;
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFrmACBrMonitor.cbxModeloSATChange(Sender: TObject);
begin
  try
    if (cbxModeloSAT.ItemIndex = 3) then
      ACBrSAT1.Integrador:= ACBrIntegrador1
    else
      ACBrSAT1.Integrador:= nil;
    ACBrSAT1.Modelo := TACBrSATModelo( cbxModeloSAT.ItemIndex ) ;
  except
    cbxModeloSAT.ItemIndex := Integer( ACBrSAT1.Modelo ) ;
    raise ;
  end ;
end;

procedure TFrmACBrMonitor.cbxPastaMensalClick(Sender: TObject);
begin
  cbxEmissaoPathNFe.Enabled := cbxPastaMensal.Checked;
end;

procedure TFrmACBrMonitor.cbxPortaChange(Sender: TObject);
begin
  try
    ACBrPosPrinter1.Porta := cbxPorta.Text;
  finally
     cbxPorta.Text := ACBrPosPrinter1.Porta;
  end;
end;

procedure TFrmACBrMonitor.cbxRedeProxyChange(Sender: TObject);
begin
  edRedeProxyIP.Enabled := (cbxRedeProxy.ItemIndex > 0);
  edRedeProxyPorta.Enabled := edRedeProxyIP.Enabled;
  edRedeProxyUser.Enabled  := edRedeProxyIP.Enabled;
  edRedeProxySenha.Enabled := edRedeProxyIP.Enabled;
end;

procedure TFrmACBrMonitor.cbxSalvarArqsChange(Sender: TObject);
begin
  VerificaDiretorios;
end;

procedure TFrmACBrMonitor.cbxSATSalvarCFeCancChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SalvarCFeCanc := cbxSATSalvarCFeCanc.Checked;
end;

procedure TFrmACBrMonitor.cbxSATSalvarCFeChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SalvarCFe := cbxSATSalvarCFe.Checked;
end;

procedure TFrmACBrMonitor.cbxSATSalvarEnvioChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SalvarEnvio := cbxSATSalvarEnvio.Checked;
end;

procedure TFrmACBrMonitor.cbxSedexAvisoRecebChange(Sender: TObject);
begin
  ACBrSedex1.AvisoRecebimento := (cbxSedexAvisoReceb.ItemIndex = 0);
end;

procedure TFrmACBrMonitor.cbxSedexFormatoChange(Sender: TObject);
begin
  ACBrSedex1.Formato := TACBrTpFormato(cbxSedexFormato.ItemIndex);
end;

procedure TFrmACBrMonitor.cbxSedexMaoPropriaChange(Sender: TObject);
begin
  ACBrSedex1.MaoPropria := (cbxSedexMaoPropria.ItemIndex = 0);
end;

procedure TFrmACBrMonitor.cbxSedexServicoChange(Sender: TObject);
begin
  ACBrSedex1.Servico := TACBrTpServico(cbxSedexServico.ItemIndex);
end;

procedure TFrmACBrMonitor.cbxSATSepararPorCNPJChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorCNPJ := cbxSATSepararPorCNPJ.Checked;
end;

procedure TFrmACBrMonitor.cbxSATSepararPorMESChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorMes := cbxSATSepararPorMES.Checked;
end;

procedure TFrmACBrMonitor.cbxSepararPorCNPJChange(Sender: TObject);
begin
  ACBrNFe1.Configuracoes.Arquivos.SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
end;

procedure TFrmACBrMonitor.cbxTimeZoneModeChange(Sender: TObject);
begin
  ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.ModoDeteccao :=
    TTimeZoneModoDeteccao( cbxTimeZoneMode.ItemIndex );
  edTimeZoneStr.Caption := ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr;
  edTimeZoneStr.Enabled := (ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.ModoDeteccao = tzManual);

  ACBrCTe1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
  ACBrMDFe1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
  ACBrBlocoX1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
  ACBrGNRE1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
end;

procedure TFrmACBrMonitor.cbxUTF8Change(Sender: TObject);
begin
  ACBrSAT1.Config.EhUTF8 := cbxUTF8.Checked;
  sePagCod.Value := ACBrSAT1.Config.PaginaDeCodigo;
end;

procedure TFrmACBrMonitor.chbTagQrCodeChange(Sender: TObject);
begin
  if chbTagQrCode.Checked and (EstaVazio(Trim(edtToken.Text)) or EstaVazio(Trim(edtIdToken.Text))) then
  begin
    MessageDlg('Erro', 'Preencha o campo CSC e IDCSC corretamente', mtError, [mbOK], '');
    chbTagQrCode.Checked := False;
    edtToken.SetFocus;
  end;
end;

procedure TFrmACBrMonitor.chECFArredondaMFDClick(Sender: TObject);
begin
  ACBrECF1.ArredondaItemMFD :=
    ((chECFArredondaMFD.Enabled) and (chECFArredondaMFD.Checked));
end;

procedure TFrmACBrMonitor.chECFControlePortaClick(Sender: TObject);
begin
  ACBrECF1.ControlePorta := chECFControlePorta.Checked;
end;

procedure TFrmACBrMonitor.chECFIgnorarTagsFormatacaoClick(Sender: TObject);
begin
  ACBrECF1.IgnorarTagsFormatacao := chECFIgnorarTagsFormatacao.Checked;
end;

procedure TFrmACBrMonitor.chRFDChange(Sender: TObject);
begin
  ACBrECF1.Desativar;

  if chRFD.Checked then
    ACBrECF1.RFD := ACBrRFD1
  else
    ACBrECF1.RFD := nil;

  AvaliaEstadoTsRFD;
  AvaliaEstadoTsECF;
end;

procedure TFrmACBrMonitor.ckSalvarClick(Sender: TObject);
begin
  edtPathLogs.Enabled := ckSalvar.Checked;
  sbPathSalvar.Enabled := ckSalvar.Checked;
end;

procedure TFrmACBrMonitor.deBOLDirArquivoExit(Sender: TObject);
begin
  if trim(deBOLDirArquivo.Text) <> '' then
  begin
    if not DirectoryExists(deBOLDirArquivo.Text) then
    begin
      deBOLDirArquivo.SetFocus;
      raise Exception.Create('Diretorio destino do Arquivo não encontrado.');
    end;
  end;
end;

procedure TFrmACBrMonitor.deBOLDirLogoExit(Sender: TObject);
begin
  if trim(deBOLDirLogo.Text) <> '' then
  begin
    if not DirectoryExists(deBOLDirLogo.Text) then
    begin
      deBOLDirLogo.SetFocus;
      raise Exception.Create('Diretorio de Logos não encontrado.');
    end;
  end;
end;

procedure TFrmACBrMonitor.deBolDirRemessaExit(Sender: TObject);
begin
  if trim(deBolDirRemessa.Text) <> '' then
  begin
    if not DirectoryExists(deBolDirRemessa.Text) then
    begin
      deBolDirRemessa.SetFocus;
      raise Exception.Create('Diretorio de Arquivos Remessa não encontrado.');
    end;
  end;
end;

procedure TFrmACBrMonitor.BOLDirRetornoExit(Sender: TObject);
begin
  if trim( TDirectoryEdit(Sender).Text) <> '' then
  begin
    if not DirectoryExists(TDirectoryEdit(Sender).Text) then
    begin
      TDirectoryEdit(Sender).Clear;
      raise Exception.Create('Diretorio de Arquivos Retorno não encontrado.');
    end;

    CarregaArquivosRetorno;
  end;
end;

procedure TFrmACBrMonitor.deBolDirRetornoRelChange(Sender: TObject);
begin
  CarregaArquivosRetorno;
end;

procedure TFrmACBrMonitor.deUSUDataCadastroExit(Sender: TObject);
begin
  if (deUSUDataCadastro.Date = 0) then
  begin
    AddLinesLog('Data Inválida');
    deUSUDataCadastro.SetFocus;
  end;
end;

procedure TFrmACBrMonitor.deRFDDataSwBasicoExit(Sender: TObject);
begin
  if (deRFDDataSwBasico.Date = 0) then
  begin
    AddLinesLog('Data Inválida');
    deRFDDataSwBasico.SetFocus;
  end;
end;

procedure TFrmACBrMonitor.edBALLogChange(Sender: TObject);
begin
  ACBrBAL1.ArqLOG := edBALLog.Text;
end;

procedure TFrmACBrMonitor.edEmailEnderecoExit(Sender: TObject);
begin
  if (Trim(edEmailEndereco.Text) <> '') and not ValidarEmail(
    edEmailEndereco.Text) then
  begin
    AddLinesLog('O endereço de E-mail informado não é Válido');
    edEmailEndereco.SetFocus;
  end;
end;

procedure TFrmACBrMonitor.edSATLogChange(Sender: TObject);
begin
  ACBrSAT1.ArqLOG:= edSATLog.Text;
end;

procedure TFrmACBrMonitor.edTimeZoneStrEditingDone(Sender: TObject);
begin
  try
    ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr := edTimeZoneStr.Caption;
  finally
    edTimeZoneStr.Caption := ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.FormDestroy(Sender: TObject);
begin
  fsCmd.Free;
  fsProcessar.Free;

  fsSLPrecos.Free;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;

  if pConfig.Visible then
  begin
    MessageDlg('Por favor "Salve" ou "Cancele" as configurações ' +
      'efetuadas antes de fechar o programa',
      mtWarning, [mbOK], 0);
    CanClose := False;
    exit;
  end;

  CanClose := pCanClose or (WindowState = wsMinimized);

  if not CanClose then
    Application.Minimize;
end;


{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.Restaurar1Click(Sender: TObject);
begin
  pmTray.Close;
  if WindowState <> wsMaximized then
    WindowState := wsNormal;
  Visible := True;
  Application.ShowMainForm := True;
  Application.Restore;
  Application.BringToFront;
  Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.Ocultar1Click(Sender: TObject);
begin
  Application.Minimize;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.Encerrar1Click(Sender: TObject);
begin
  pCanClose := True;
  Close;
end;

{------------------------- Procedures de Uso Interno --------------------------}
procedure TFrmACBrMonitor.Inicializar;
var
  Ini: TIniFile;
  ArqIni: string;
  Txt: string;
  Erro: string;
  IpList: TStringList;
  I: integer;
begin
  cbxBOLImpressora.Items.Assign(Printer.Printers);
  cbxImpressora.Items.Assign(Printer.Printers);
  cbxImpressoraNFCe.Items.Assign(Printer.Printers);
  Timer1.Enabled := False;
  Inicio := False;
  MonitorarPasta := False;
  Erro := '';
  ACBrMonitorINI := ExtractFilePath(Application.ExeName) + 'ACBrMonitor.ini';

  if not FileExists(ACBrMonitorINI) then //verifica se o arq. de config existe
  begin                                   //se nao existir, lê configurações default e vai para as configs
    WindowState := wsNormal;
    MessageDlg('Bem vindo ao ACBrMonitor',
      'Bem vindo ao ACBrMonitor,' + sLineBreak + sLineBreak +
      'Por favor configure o ACBrMonitor, ' + sLineBreak +
      'informando o Método de Monitoramento, e a configuração ' +
      sLineBreak + 'dos Equipamentos de Automação ligados a essa máquina.' +
      sLineBreak + sLineBreak +
      'IMPORTANTE: Após configurar o Método de Monitoramento' +
      sLineBreak + ' o ACBrMonitor deve ser reiniciado', mtInformation, [mbOK], 0);
    LerIni;
    bConfig.Click;
    exit;
  end;

  try
    LerIni;
    cbxEmissaoPathNFe.Enabled := cbxPastaMensal.Checked;
  except
    on E: Exception do
      Erro := Erro + sLineBreak + E.Message;
  end;

  EscondeConfig;

  try
    AjustaLinhasLog;  { Diminui / Ajusta o numero de linhas do Log }
  except
    on E: Exception do
      Erro := Erro + sLineBreak + E.Message;
  end;

  try
    bConfig.Enabled := True;
    Timer1.Interval := sedIntervalo.Value;
    Timer1.Enabled := rbTXT.Checked;
    TcpServer.Terminador := '#13,#10,#46,#13,#10';
    TcpServer.Ativo := rbTCP.Checked;

    AddLinesLog('ACBr MonitorPLUS Ver.' + sVersaoACBr);
    AddLinesLog('Aguardando comandos ACBr');
  except
    on E: Exception do
      Erro := Erro + sLineBreak + E.Message;
  end;

  try
    if rbTCP.Checked then
    begin
      if TcpServer.Ativo then
      begin
        try
          Txt := 'Endereço Local: [' + TcpServer.TCPBlockSocket.LocalName + '],   IP: ';
          with TcpServer.TCPBlockSocket do
          begin
            IpList := TStringList.Create;
            try
              ResolveNameToIP(LocalName, IpList);
              for I := 0 to IpList.Count - 1 do
                if pos(':', IpList[I]) = 0 then
                  Txt := Txt + '   ' + IpList[I];
            finally
              IpList.Free;
            end;
          end;
        except
        end;
        AddLinesLog(Txt);
        AddLinesLog('Porta: [' + TcpServer.Port + ']');
      end;
    end
    else
    begin
      if MonitorarPasta then
      begin
        AddLinesLog('Monitorando Arquivos em: ' + ExtractFilePath(ArqEntTXT));
        AddLinesLog('Respostas gravadas em: ' + ExtractFilePath(ArqSaiTXT));
        if ExtractFilePath(ArqEntTXT) = ExtractFilePath(ArqSaiTXT) then
        begin
          AddLinesLog('ATENÇÃO: Use diretórios diferentes para entrada e saída.');
          raise Exception.Create('Use diretórios diferentes para entrada e saída.');
        end;
      end
      else
      begin
        AddLinesLog('Monitorando Comandos TXT em: ' + ArqEntTXT);
        AddLinesLog('Respostas gravadas em: ' + ArqSaiTXT);
      end;
    end;

    if cbLog.Checked then
      AddLinesLog('Log de comandos será gravado em: ' + ArqLogTXT);

    if cbLogComp.Checked then
      AddLinesLog('Log de mensagens da NFe/NFCe será gravado em: ' + ArqLogCompTXT);

    { Se for NAO fiscal, desliga o AVISO antes de ativar }
    if ACBrECF1.Modelo = ecfNaoFiscal then
    begin
      ArqIni := (ACBrECF1.ECF as TACBrECFNaoFiscal).NomeArqINI;
      if FileExists(ArqIni) then
      begin
        Ini := TIniFile.Create(ArqIni);
        try
          Ini.WriteString('Variaveis', 'Aviso_Legal', 'NAO');
        finally
          Ini.Free;
        end;
      end;
    end;
  except
    on E: Exception do
      Erro := Erro + sLineBreak + E.Message;
  end;

  if Erro <> '' then
    raise Exception.Create(Erro);

end;

procedure TFrmACBrMonitor.DesInicializar;
begin
  ACBrECF1.Desativar;
  ACBrCHQ1.Desativar;
  ACBrGAV1.Desativar;
  ACBrDIS1.Desativar;
  ACBrLCB1.Desativar;
  ACBrBAL1.Desativar;
  ACBrETQ1.Desativar;
  ACBrPosPrinter1.Desativar;
  TCPServer.Desativar;
  TCPServerTC.Desativar;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.AjustaLinhasLog;

  procedure AjustaLogFile(AFile: string; LinhasMax: integer);
  var
    LogNew, LogOld: TStringList;
    I: integer;
  begin
    if not FileExists(AFile) then
      exit;

    LogOld := TStringList.Create;
    try
      LogOld.LoadFromFile(AFile);
      if LogOld.Count > LinhasMax then
      begin
        AddLinesLog('Ajustando o tamanho do arquivo: ' + AFile);
        AddLinesLog('Numero de Linhas atual: ' + IntToStr(LogOld.Count));
        AddLinesLog('Reduzindo para: ' + IntToStr(LinhasMax) + ' linhas');

        { Se for muito grande é mais rápido copiar para outra lista do que Deletar }
        if (LogOld.Count - LinhasMax) > LinhasMax then
        begin
          LogNew := TStringList.Create;
          try
            LogNew.Clear;

            for I := LinhasMax downto 1 do
              LogNew.Add(LogOld[LogOld.Count - I]);

            LogNew.SaveToFile(AFile);
          finally
            LogNew.Free;
          end;
        end
        else
        begin
          { Existe alguma maneira mais rápida de fazer isso ??? }
          LogOld.BeginUpdate;
          while LogOld.Count > LinhasMax do
            LogOld.Delete(0);
          LogOld.EndUpdate;
          LogOld.SaveToFile(AFile);
        end;
      end;
    finally
      LogOld.Free;
    end;
  end;

begin
  if (sedLogLinhas.Value <= 0) then
    exit;

  // Ajustado LOG do ACBrMonitor //
  if (cbLog.Checked) then
    AjustaLogFile(ArqLogTXT, sedLogLinhas.Value);

  // Ajustado LOG do ECF //
  if (edECFLog.Text <> '') then
    AjustaLogFile(edECFLog.Text, sedLogLinhas.Value);

  // Ajustado LOG do Balança //
  if (edBALLog.Text <> '') then
    AjustaLogFile(edBALLog.Text, sedLogLinhas.Value);

  // Ajustado LOG da NFe/NFCe //
  if (sedLogLinhasComp.Value > 0) and (cbLogComp.Checked) then
    AjustaLogFile(ArqLogCompTXT, sedLogLinhasComp.Value);

  // Ajustado LOG do SAT //
  if (edSATLog.Text <> '') then
    AjustaLogFile(edSATLog.Text, sedLogLinhas.Value);

  // Ajustado LOG do PosPrinter //
  if (edPosPrinterLog.Text <> '') then
    AjustaLogFile(edPosPrinterLog.Text, sedLogLinhas.Value);

end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.LerIni;
var
  Ini: TIniFile;
  ECFAtivado, CHQAtivado, GAVAtivado, DISAtivado, BALAtivado,
  ETQAtivado, ESCPOSAtivado: boolean;
  Senha, POSPrDeviceParams, ECFDeviceParams, CHQDeviceParams, SATDeviceParams, PathApplication: string;
  wNomeArquivo: string;
  OK: boolean;
begin
  PathApplication := PathWithDelim(ExtractFilePath(Application.ExeName));

  ECFAtivado := ACBrECF1.Ativo;
  CHQAtivado := ACBrCHQ1.Ativo;
  GAVAtivado := ACBrGAV1.Ativo;
  DISAtivado := ACBrDIS1.Ativo;
  BALAtivado := ACBrBAL1.Ativo;
  ETQAtivado := ACBrETQ1.Ativo;

  if ( Assigned(ACBrNFeDANFeESCPOS1.PosPrinter) ) then
     ESCPOSAtivado := ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativo
  else
     ESCPOSAtivado := False;

  Ini := TIniFile.Create(ACBrMonitorINI);
  try

    { Lendo Senha }
    //     Ini.ReadString('ACBrMonitor','TXT_Saida','SAI.TXT');
    fsHashSenha := StrToIntDef(LeINICrypt(INI, 'ACBrMonitor', 'HashSenha', _C), -1);

    if fsHashSenha < 1 then  { INI antigo não tinha essa chave }
    begin
      Senha := Ini.ReadString('ACBrMonitor', 'Senha', '');
      if Senha <> '' then
        fsHashSenha := StringCrc16(Senha);
    end;

    if fsHashSenha > 0 then
    begin
      cbSenha.Checked := True;
      edSenha.Text := 'NADAAQUI';
    end;

    { Parametros do Monitor }
    rbTCP.Checked := Ini.ReadBool('ACBrMonitor', 'Modo_TCP', False);
    rbTXT.Checked := Ini.ReadBool('ACBrMonitor', 'Modo_TXT', True);
    edPortaTCP.Text := IntToStr(Ini.ReadInteger('ACBrMonitor', 'TCP_Porta', 3434));
    edTimeOutTCP.Text := IntToStr(Ini.ReadInteger('ACBrMonitor', 'TCP_TimeOut', 10000));
    chbTCPANSI.Checked := Ini.ReadBool('ACBrMonitor','Converte_TCP_Ansi', False);
    edEntTXT.Text := Ini.ReadString('ACBrMonitor', 'TXT_Entrada', 'ENT.TXT');
    edSaiTXT.Text := Ini.ReadString('ACBrMonitor', 'TXT_Saida', 'SAI.TXT');
    chbArqEntANSI.Checked := Ini.ReadBool('ACBrMonitor','Converte_TXT_Entrada_Ansi', False);
    chbArqSaiANSI.Checked := Ini.ReadBool('ACBrMonitor','Converte_TXT_Saida_Ansi', False);
    sedIntervalo.Value := Ini.ReadInteger('ACBrMonitor', 'Intervalo', 50);
    edLogArq.Text := Ini.ReadString('ACBrMonitor', 'Arquivo_Log', 'LOG.TXT');
    cbLog.Checked := Ini.ReadBool('ACBrMonitor', 'Gravar_Log', False) and
      (edLogArq.Text <> '');
    sedLogLinhas.Value := Ini.ReadInteger('ACBrMonitor', 'Linhas_Log', 0);
    cbComandos.Checked := Ini.ReadBool('ACBrMonitor', 'Comandos_Remotos', False);
    cbUmaInstancia.Checked := Ini.ReadBool('ACBrMonitor', 'Uma_Instancia', True);
    cbAbas.Checked := Ini.ReadBool('ACBrMonitor', 'MostraAbas', False);
    {$IFDEF LINUX}
    cbMostrarNaBarraDeTarefas.Checked := Ini.ReadBool('ACBrMonitor', 'MostrarNaBarraDeTarefas', True);
    {$ELSE}
    cbMostrarNaBarraDeTarefas.Checked := Ini.ReadBool('ACBrMonitor', 'MostrarNaBarraDeTarefas', False);
    {$ENDIF}
    cbRetirarAcentosNaResposta.Checked := Ini.ReadBool('ACBrMonitor', 'RetirarAcentosNaResposta', False);
    chkMostraLogNaTela.Checked := Ini.ReadBool('ACBrMonitor', 'MostraLogEmRespostasEnviadas', True)
                                  and cbLog.Checked;
    cbMonitorarPasta.OnChange := Nil;
    cbMonitorarPasta.Checked := Ini.ReadBool('ACBrMonitor', 'MonitorarPasta', False);
    cbMonitorarPasta.OnChange := @cbMonitorarPastaChange;

    MonitorarPasta := cbMonitorarPasta.Checked;

    ArqEntTXT := AcertaPath(edEntTXT.Text);
    ArqEntOrig := ArqEntTXT;
    ArqSaiTXT := AcertaPath(edSaiTXT.Text);
    ArqSaiOrig := ArqSaiTXT;
    ArqSaiTMP := ChangeFileExt(ArqSaiTXT, '.tmp');
    ArqLogTXT := AcertaPath(edLogArq.Text);

    TcpServer.Port := edPortaTCP.Text;
    TcpServer.TimeOut := StrToIntDef(edTimeOutTCP.Text, 10000);

    { Parametros do ECF }
    ECFDeviceParams := Ini.ReadString('ECF', 'SerialParams', '');
    cbECFModelo.ItemIndex := Ini.ReadInteger('ECF', 'Modelo', 0) + 1;
    cbECFModeloChange(Self);
    cbECFPorta.Text := Ini.ReadString('ECF', 'Porta', 'Procurar');
    sedECFTimeout.Value := Ini.ReadInteger('ECF', 'Timeout', 3);
    sedECFIntervalo.Value := Ini.ReadInteger('ECF', 'IntervaloAposComando', 100);
    sedECFMaxLinhasBuffer.Value := Ini.ReadInteger('ECF', 'MaxLinhasBuffer', 0);
    sedECFPaginaCodigo.Value := Ini.ReadInteger('ECF', 'PaginaCodigo', 0);
    sedECFLinhasEntreCupons.Value := Ini.ReadInteger('ECF', 'LinhasEntreCupons', 0);
    chECFArredondaPorQtd.Checked := Ini.ReadBool('ECF', 'ArredondamentoPorQtd', False);
    chECFArredondaMFD.Checked := Ini.ReadBool('ECF', 'ArredondamentoItemMFD', False);
    chECFDescrGrande.Checked := Ini.ReadBool('ECF', 'DescricaoGrande', True);
    chECFSinalGavetaInvertido.Checked :=
      Ini.ReadBool('ECF', 'GavetaSinalInvertido', False);
    chECFIgnorarTagsFormatacao.Checked :=
      Ini.ReadBool('ECF', 'IgnorarTagsFormatacao', False);
    chECFControlePorta.Checked := Ini.ReadBool('ECF', 'ControlePorta', False);
    edECFLog.Text := Ini.ReadString('ECF', 'ArqLog', '');

    { Parametros do CHQ }
    cbCHQModelo.ItemIndex := Ini.ReadInteger('CHQ', 'Modelo', 0);
    cbCHQModeloChange(Self);
    cbCHQPorta.Text := Ini.ReadString('CHQ', 'Porta', '');
    chCHQVerForm.Checked := Ini.ReadBool('CHQ', 'VerificaFormulario', False);
    edCHQFavorecido.Text := Ini.ReadString('CHQ', 'Favorecido', '');
    edCHQCidade.Text := Ini.ReadString('CHQ', 'Cidade', '');
    edCHQBemafiINI.Text := Ini.ReadString('CHQ', 'PathBemafiINI', '');
    CHQDeviceParams := Ini.ReadString('CHQ', 'SerialParams', '');

    { Parametros do GAV }
    cbGAVModelo.ItemIndex := Ini.ReadInteger('GAV', 'Modelo', 0);
    cbGAVModeloChange(Self);
    cbGAVPorta.Text := Ini.ReadString('GAV', 'Porta', '');
    cbGAVStrAbre.Text := Ini.ReadString('GAV', 'StringAbertura', '');
    sedGAVIntervaloAbertura.Value :=
      Ini.ReadInteger('GAV', 'AberturaIntervalo', ACBrGAV1.AberturaIntervalo);
    cbGAVAcaoAberturaAntecipada.ItemIndex :=
      Ini.ReadInteger('GAV', 'AcaoAberturaAntecipada', 1);

    { Parametros do DIS }
    cbDISModelo.ItemIndex := Ini.ReadInteger('DIS', 'Modelo', 0);
    cbDISModeloChange(Self);
    cbDISPorta.Text := Ini.ReadString('DIS', 'Porta', '');
    seDISIntervalo.Value := Ini.ReadInteger('DIS', 'Intervalo', 300);
    seDISPassos.Value := Ini.ReadInteger('DIS', 'Passos', 1);
    seDISIntByte.Value := Ini.ReadInteger('DIS', 'IntervaloEnvioBytes', 3);

    { Parametros do LCB }
    cbLCBPorta.Text := Ini.ReadString('LCB', 'Porta', 'Sem Leitor');
    cbLCBPortaChange(Self);
    sedLCBIntervalo.Value := Ini.ReadInteger('LCB', 'Intervalo', 100);
    cbLCBSufixoLeitor.Text := Ini.ReadString('LCB', 'SufixoLeitor', '#13');
    chLCBExcluirSufixo.Checked := Ini.ReadBool('LCB', 'ExcluirSufixo', False);
    edLCBPreExcluir.Text := Ini.ReadString('LCB', 'PrefixoAExcluir', '');
    cbLCBSufixo.Text := Ini.ReadString('LCB', 'SufixoIncluir', '');
    cbLCBDispositivo.Text := Ini.ReadString('LCB', 'Dispositivo', '');
    rbLCBTeclado.Checked := Ini.ReadBool('LCB', 'Teclado', True);
    rbLCBFila.Checked := not rbLCBTeclado.Checked;
    ACBrLCB1.Device.ParamsString := Ini.ReadString('LCB', 'Device', '');

    { Parametros do RFD }
    chRFD.Checked := INI.ReadBool('RFD', 'GerarRFD', False);
    chRFDIgnoraMFD.Checked := INI.ReadBool('RFD', 'IgnoraECF_MFD', True);
    edRFDDir.Text := INI.ReadString('RFD', 'DirRFD', edRFDDir.Text);

    { Parametros do BAL }
    cbBALModelo.ItemIndex := Ini.ReadInteger('BAL', 'Modelo', 0);
    cbBALModeloChange(Self);
    cbBALPorta.Text := Ini.ReadString('BAL', 'Porta', '');
    sedBALIntervalo.Value := Ini.ReadInteger('BAL', 'Intervalo', 200);
    edBALLog.Text := Ini.ReadString('BAL', 'ArqLog', '');
    ACBrBAL1.Device.ParamsString := Ini.ReadString('BAL', 'Device', '');

    { Parametros do ETQ }
    cbETQModelo.ItemIndex := Ini.ReadInteger('ETQ', 'Modelo', 0);
    cbETQModeloChange(Self);
    cbETQPorta.Text := Ini.ReadString('ETQ', 'Porta', '');
    cbDPI.ItemIndex := Ini.ReadInteger('ETQ', 'DPI', 0);
    ckMemoria.Checked:= Ini.ReadBool('ETQ', 'LimparMemoria', True);
    eTemperatura.Text:= IntToStr(Ini.ReadInteger('ETQ', 'Temperatura', 10));
    eVelocidade.Text := IntToStr(Ini.ReadInteger('ETQ', 'Velocidade', -1));
    cbBackFeed.ItemIndex:= Ini.ReadInteger('ETQ', 'BackFeed', -1);
    eMargemEsquerda.Text:= IntToStr(Ini.ReadInteger('ETQ', 'MargemEsquerda', 10));
    cbOrigem.ItemIndex  := Ini.ReadInteger('ETQ', 'Origem', -1);
    eCopias.Text  := IntToStr(Ini.ReadInteger('ETQ', 'Copias', 1));

    { Parametros do TC }
    cbxTCModelo.ItemIndex := Ini.ReadInteger('TC', 'Modelo', 0);
    cbxTCModeloChange(Self);
    edTCArqPrecos.Text := IntToStr(Ini.ReadInteger('TC', 'TCP_Porta', 6500));
    edTCArqPrecos.Text := Ini.ReadString('TC', 'Arq_Precos', 'PRICETAB.TXT');
    edTCNaoEncontrado.Text :=
      Ini.ReadString('TC', 'Nao_Econtrado', 'PRODUTO|NAO ENCONTRADO');

    { Parametros do CEP }
    cbCEPWebService.ItemIndex := Ini.ReadInteger('CEP', 'WebService', 0);
    cbCEPWebServiceChange(Self);
    edCEPChaveBuscarCEP.Text := Ini.ReadString('CEP', 'Chave_BuscarCEP', '');
    edCONProxyHost.Text := Ini.ReadString('CEP', 'Proxy_Host', '');
    edCONProxyPort.Text := Ini.ReadString('CEP', 'Proxy_Port', '');
    edCONProxyUser.Text := Ini.ReadString('CEP', 'Proxy_User', '');
    edCONProxyPass.Text := LeINICrypt(INI, 'CEP', 'Proxy_Pass', _C);

    {Parametros do Boleto - Cliente}
    edtBOLRazaoSocial.Text := Ini.ReadString('BOLETO', 'Cedente.Nome', '');
    edtBOLCNPJ.Text := Ini.ReadString('BOLETO', 'Cedente.CNPJCPF', '');
    edtBOLLogradouro.Text := Ini.ReadString('BOLETO', 'Cedente.Logradouro', '');
    edtBOLNumero.Text := Ini.ReadString('BOLETO', 'Cedente.Numero', '');
    edtBOLBairro.Text := Ini.ReadString('BOLETO', 'Cedente.Bairro', '');
    edtBOLCidade.Text := Ini.ReadString('BOLETO', 'Cedente.Cidade', '');
    edtBOLCEP.Text := Ini.ReadString('BOLETO', 'Cedente.CEP', '');
    edtBOLComplemento.Text := Ini.ReadString('BOLETO', 'Cedente.Complemento', '');
    cbxBOLUF.Text := Ini.ReadString('BOLETO', 'Cedente.UF', '');
    cbxBOLEmissao.ItemIndex := Ini.ReadInteger('BOLETO', 'Cedente.RespEmis', -1);
    cbxBOLF_J.ItemIndex := Ini.ReadInteger('BOLETO', 'Cedente.Pessoa', -1);
    edtCodTransmissao.Text := Ini.ReadString('BOLETO', 'Cedente.CodTransmissao', '');
    edtModalidade.Text := Ini.ReadString('BOLETO', 'Cedente.Modalidade', '');
    edtConvenio.Text := Ini.ReadString('BOLETO', 'Cedente.Convenio', '');

    {Parametros do Boleto - Banco}
    cbxBOLBanco.ItemIndex := Ini.ReadInteger('BOLETO', 'Banco', 0);
    edtBOLConta.Text := Ini.ReadString('BOLETO', 'Conta', '');
    edtBOLDigitoConta.Text := Ini.ReadString('BOLETO', 'DigitoConta', '');
    edtBOLAgencia.Text := Ini.ReadString('BOLETO', 'Agencia', '');
    edtBOLDigitoAgencia.Text := Ini.ReadString('BOLETO', 'DigitoAgencia', '');
    edtCodCliente.Text := Ini.ReadString('BOLETO', 'CodCedente', '');
    edtBOLLocalPagamento.Text := Ini.ReadString('BOLETO', 'LocalPagamento', ACBrBoleto1.Banco.LocalPagamento);

    {Parametros do Boleto - Boleto}
    deBOLDirLogo.Text :=
      Ini.ReadString('BOLETO', 'DirLogos', ExtractFilePath(Application.ExeName) +
      'Logos' + PathDelim);

    spBOLCopias.Value := Ini.ReadInteger('BOLETO', 'Copias', 1);
    ckgBOLMostrar.Checked[0] := Ini.ReadBool('BOLETO', 'Preview', True);
    ckgBOLMostrar.Checked[1] := Ini.ReadBool('BOLETO', 'Progresso', True);
    ckgBOLMostrar.Checked[2] := Ini.ReadBool('BOLETO', 'Setup', True);
    cbxBOLLayout.ItemIndex := Ini.ReadInteger('BOLETO', 'Layout', 0);
    cbxBOLFiltro.ItemIndex := Ini.ReadInteger('BOLETO', 'Filtro', 0);
    deBOLDirArquivo.Text := Ini.ReadString('BOLETO', 'DirArquivoBoleto', '');
    deBolDirRemessa.Text := Ini.ReadString('BOLETO', 'DirArquivoRemessa', '');
    deBolDirRetorno.Text := Ini.ReadString('BOLETO', 'DirArquivoRetorno', '');
    deBolDirRetornoRel.Text := deBolDirRetorno.Text;

    cbxCNAB.ItemIndex := Ini.ReadInteger('BOLETO', 'CNAB', 0);
    chkLerCedenteRetorno.Checked := Ini.ReadBool('BOLETO','LerCedenteRetorno',False);
    chkBOLRelMostraPreview.Checked := Ini.ReadBool('BOLETO','MostraPreviewRelRetorno',True);
    edtBOLLogoEmpresa.Text := Ini.ReadString('BOLETO', 'LogoEmpresa', '');
    {Parametro do Boleto Impressora}
    cbxBOLImpressora.ItemIndex :=
      cbxBOLImpressora.Items.IndexOf(Ini.ReadString('BOLETO', 'Impressora', ''));
    {Parametros do Boleto - E-mail}
    edtBOLEmailAssunto.Text := Ini.ReadString('BOLETO', 'EmailAssuntoBoleto', '');
    edtBOLEmailMensagem.Text := StringToBinaryString(Ini.ReadString('BOLETO', 'EmailMensagemBoleto', ''));

    {Parametro da Conta de tsEmailDFe}
    edEmailNome.Text := Ini.ReadString('EMAIL', 'NomeExibicao', '');
    edEmailEndereco.Text := Ini.ReadString('EMAIL', 'Endereco', '');
    edEmailHost.Text := Ini.ReadString('EMAIL', 'Email', '');
    edEmailPorta.Value := Ini.ReadInteger('EMAIL', 'Porta', 0);
    edEmailUsuario.Text := LeINICrypt(Ini, 'EMAIL', 'Usuario', _C);
    edEmailSenha.Text := LeINICrypt(Ini, 'EMAIL', 'Senha', _C);
    cbEmailSsl.Checked := Ini.ReadBool('EMAIL', 'ExigeSSL', False);
    cbEmailTls.Checked := Ini.ReadBool('EMAIL', 'ExigeTLS', False);
    cbEmailConfirmation.Checked := Ini.ReadBool('EMAIL', 'Confirmacao', False);
    cbEmailThread.Checked := Ini.ReadBool('EMAIL', 'SegundoPlano', False);
    cbEmailCodificacao.Text := Ini.ReadString('EMAIL', 'Codificacao', '');

    {Parametro Sedex}
    edtSedexContrato.Text := Ini.ReadString('SEDEX', 'Contrato', '');
    edtSedexSenha.Text := LeINICrypt(Ini, 'SEDEX', 'SenhaSedex', _C);
    {Parametro NCM}
    deNcmSalvar.Text := Ini.ReadString('NCM', 'DirNCMSalvar', '');

    edtIdToken.Text := Ini.ReadString('NFCe', 'IdToken', '');
    edtToken.Text := Ini.ReadString('NFCe', 'Token', '');
    chbTagQrCode.Checked := Ini.ReadBool('NFCe', 'TagQrCode', True);
    ckNFCeUsarIntegrador.Checked := Ini.ReadBool('NFCe', 'UsarIntegrador', False);

        {Parametros NFe}
    ACBrNFeDANFeESCPOS1.PosPrinter.Device.Desativar;

    cbModoXML.Checked := Ini.ReadBool('ACBrNFeMonitor', 'ModoXML', False);
    cbRetirarAcentos.Checked := Ini.ReadBool('ACBrNFeMonitor', 'RetirarAcentos', True);
    edLogComp.Text := Ini.ReadString('ACBrNFeMonitor', 'Arquivo_Log_Comp', 'LOG_COMP.TXT');
    cbLogComp.Checked := Ini.ReadBool('ACBrNFeMonitor', 'Gravar_Log_Comp', False) and (edLogComp.Text <> '');
    sedLogLinhasComp.Value := Ini.ReadInteger('ACBrNFeMonitor', 'Linhas_Log_Comp', 0);
    ArqLogCompTXT := AcertaPath(edLogComp.Text);
    //rgVersaoSSL.ItemIndex := Ini.ReadInteger('ACBrNFeMonitor', 'VersaoSSL', 0);
    edtArquivoWebServicesNFe.Text := Ini.ReadString('ACBrNFeMonitor', 'ArquivoWebServices',
      PathApplication + 'ACBrNFeServicos.ini');
    edtArquivoWebServicesCTe.Text := Ini.ReadString('ACBrNFeMonitor', 'ArquivoWebServicesCTe',
      PathApplication + 'ACBrCTeServicos.ini');
    edtArquivoWebServicesMDFe.Text := Ini.ReadString('ACBrNFeMonitor', 'ArquivoWebServicesMDFe',
      PathApplication + 'ACBrMDFeServicos.ini');
    edtArquivoWebServicesGNRe.Text := Ini.ReadString('ACBrNFeMonitor', 'ArquivoWebServicesGNRe',PathApplication + 'ACBrGNREServicos.ini');
    cbValidarDigest.Checked := Ini.ReadBool('ACBrNFeMonitor', 'ValidarDigest', True);
    edtTimeoutWebServices.Value := Ini.ReadInteger('ACBrNFeMonitor', 'TimeoutWebService', 15);

    cbxTimeZoneMode.ItemIndex := Ini.ReadInteger('WebService','TimeZoneMode',0);
    edTimeZoneStr.Caption := Ini.ReadString('WebService','TimeZoneStr','');

    cbModoEmissao.Checked := Ini.ReadBool('ACBrNFeMonitor', 'IgnorarComandoModoEmissao', False);
    //rgFormaEmissao.ItemIndex := Ini.ReadInteger('Geral', 'FormaEmissao', 0);
    cbFormaEmissaoNFe.ItemIndex := Ini.ReadInteger('WebService', 'FormaEmissaoNFe', Ini.ReadInteger('Geral', 'FormaEmissao', 0));
    cbFormaEmissaoCTe.ItemIndex := Ini.ReadInteger('WebService', 'FormaEmissaoCTe', Ini.ReadInteger('Geral', 'FormaEmissao', 0));
    cbFormaEmissaoMDFe.ItemIndex := Ini.ReadInteger('WebService', 'FormaEmissaoMDFe', Ini.ReadInteger('Geral', 'FormaEmissao', 0));
    cbFormaEmissaoGNRe.ItemIndex := Ini.ReadInteger('WebService', 'FormaEmissaoGNRe', Ini.ReadInteger('Geral', 'FormaEmissao', 0));
    ckSalvar.Checked := Ini.ReadBool('Geral', 'Salvar', True);
    edtPathLogs.Text := Ini.ReadString('Geral', 'PathSalvar',PathApplication + 'Logs');
    cbxImpressora.ItemIndex := cbxImpressora.Items.IndexOf(Ini.ReadString('Geral', 'Impressora', '0'));

    cbxAjustarAut.Checked := Ini.ReadBool('WebService', 'AjustarAut', False);
    edtAguardar.Text := Ini.ReadString('WebService', 'Aguardar', '0');
    edtTentativas.Text := Ini.ReadString('WebService', 'Tentativas', '5');
    edtIntervalo.Text := Ini.ReadString('WebService', 'Intervalo', '0');

    cbUF.ItemIndex := cbUF.Items.IndexOf(Ini.ReadString('WebService', 'UF', 'SP'));
    rgTipoAmb.ItemIndex := Ini.ReadInteger('WebService', 'Ambiente', 0);
    cbVersaoWS.ItemIndex := cbVersaoWS.Items.IndexOf(
      Ini.ReadString('WebService', 'Versao', '3.10'));
    cbVersaoWSCTe.ItemIndex := cbVersaoWSCTe.Items.IndexOf(
      Ini.ReadString('WebService','VersaoCTe','2.00'));
    cbVersaoWSMDFe.ItemIndex := cbVersaoWSMDFe.Items.IndexOf(
      Ini.ReadString('WebService','VersaoMDFe','1.00'));

    edtCNPJContador.Text := Ini.ReadString('NFe', 'CNPJContador', '');

    cbSSLLib.ItemIndex     := Ini.ReadInteger( 'Certificado','SSLLib' ,
      IfThen(Ini.ReadInteger('ACBrNFeMonitor', 'VersaoSSL', 0)=0, 1, 4));
    cbCryptLib.ItemIndex   := Ini.ReadInteger( 'Certificado','CryptLib', 0);
    cbHttpLib.ItemIndex    := Ini.ReadInteger( 'Certificado','HttpLib', 0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger( 'Certificado','XmlSignLib', 0);
    cbSSLType.ItemIndex    := Ini.ReadInteger( 'Certificado','SSLType', 0);

    edtArquivoPFX.Text := Ini.ReadString('Certificado', 'ArquivoPFX', '');
    edtNumeroSerie.Text := Ini.ReadString('Certificado', 'NumeroSerie', '');
    edtSenha.Text := LeINICrypt(INI, 'Certificado', 'Senha', _C);
    chkExibeRazaoSocial.Checked := Ini.ReadBool('Certificado', 'ExibeRazaoSocialCertificado', False);
    chkVerificarValidadeCertificado.Checked := Ini.ReadBool('Certificado', 'VerificarValidade', False);

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := LeINICrypt(INI, 'Proxy', 'Pass', _C);

    cbxSalvarArqs.Checked := Ini.ReadBool('Arquivos', 'Salvar', True);
    cbxPastaMensal.Checked := Ini.ReadBool('Arquivos', 'PastaMensal', True);
    cbxAdicionaLiteral.Checked := Ini.ReadBool('Arquivos', 'AddLiteral', True);
    cbxEmissaoPathNFe.Checked := Ini.ReadBool('Arquivos', 'EmissaoPathNFe', True);
    cbxSalvaPathEvento.Checked :=
      Ini.ReadBool('Arquivos', 'SalvarCCeCanPathEvento', True);
    cbxSepararPorCNPJ.Checked := Ini.ReadBool('Arquivos', 'SepararPorCNPJ', True);
    cbxSepararporModelo.Checked := Ini.ReadBool('Arquivos', 'SepararPorModelo', True);
    cbxSalvarNFesProcessadas.Checked :=
      Ini.ReadBool('Arquivos', 'SalvarApenasNFesAutorizadas', False);
    cbxAtualizarXMLCancelado.Checked := Ini.ReadBool('Arquivos', 'AtualizarXMLCancelado', True);
    cbxNormatizarMunicipios.Checked := Ini.ReadBool('Arquivos', 'NormatizarMunicipios', True);
    cbxUsarSeparadorPathPDF.Checked := Ini.ReadBool('Arquivos', 'UsarSeparadorPathPDF', True);

    edtPathNFe.Text := Ini.ReadString('Arquivos', 'PathNFe', PathApplication+'Arqs');
    edtPathInu.Text := Ini.ReadString('Arquivos', 'PathInu', PathApplication+'Arqs');
    edtPathDPEC.Text := Ini.ReadString('Arquivos', 'PathDPEC', PathApplication+'Arqs');
    edtPathEvento.Text := Ini.ReadString('Arquivos', 'PathEvento', PathApplication+'Arqs');

    SetDFeSSLType;

    SetComumConfig(ACBrNFe1.Configuracoes);
    SetComumConfig(ACBrCTe1.Configuracoes);
    SetComumConfig(ACBrMDFe1.Configuracoes);
    SetComumConfig(ACBrGNRE1.Configuracoes);
    SetComumConfig(ACBrBlocoX1.Configuracoes);

    AtualizaSSLLibsCombo;

    ACBrNFe1.Configuracoes.Geral.AtualizarXMLCancelado := Ini.ReadBool('Arquivos', 'AtualizarXMLCancelado', True);

    if EstaVazio(edtAguardar.Text) then
      edtAguardar.Text := IntToStr(ACBrNFe1.Configuracoes.WebServices.AguardarConsultaRet);

    if EstaVazio(edtTentativas.Text) then
      edtTentativas.Text := IntToStr(ACBrNFe1.Configuracoes.WebServices.Tentativas);

    if EstaVazio(edtIntervalo.Text) then
      edtIntervalo.Text := IntToStr(ACBrNFe1.Configuracoes.WebServices.IntervaloTentativas);

    edTimeZoneStr.Caption := ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr;
    edTimeZoneStr.Enabled := (ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.ModoDeteccao = tzManual);

    rgTipoDanfe.ItemIndex := Ini.ReadInteger('Geral', 'DANFE', 0);
    edtLogoMarca.Text := Ini.ReadString('Geral', 'LogoMarca', '');
    edtLogoMarcaNFCeSAT.Text := Ini.ReadString('Geral', 'LogoMarcaNFCeSAT', '');
    rgModeloDanfe.ItemIndex := Ini.ReadInteger('DANFE', 'Modelo', 0);
    rgTamanhoPapelDacte.ItemIndex := Ini.ReadInteger('DACTE', 'TamanhoPapel', 0);
    edtSiteEmpresa.Text := Ini.ReadString('DANFE', 'Site', '');
    edtEmailEmpresa.Text := Ini.ReadString('DANFE', 'Email', '');
    edtFaxEmpresa.Text := Ini.ReadString('DANFE', 'Fax', '');
    cbxImpDescPorc.Checked := Ini.ReadBool('DANFE', 'ImpDescPorc', True);
    cbxMostrarPreview.Checked := Ini.ReadBool('DANFE', 'MostrarPreview', False);
    edtNumCopia.Value := Ini.ReadInteger('DANFE', 'Copias', 1);
    speLargCodProd.Value := Ini.ReadInteger('DANFE', 'LarguraCodigoProduto', 40);
    speEspBorda.Value := Ini.ReadInteger('DANFE', 'EspessuraBorda', 1);
    speFonteRazao.Value := Ini.ReadInteger('DANFE', 'FonteRazao', 12);
    speFonteEndereco.Value := Ini.ReadInteger('DANFE', 'FonteEndereco', 10);
    speFonteCampos.Value := Ini.ReadInteger('DANFE', 'FonteCampos', 10);
    speAlturaCampos.Value := Ini.ReadInteger('DANFE', 'AlturaCampos', 30);
    fspeMargemInf.Value := Ini.ReadFloat('DANFE', 'Margem', 0.8);
    fspeMargemSup.Value := Ini.ReadFloat('DANFE', 'MargemSup', 0.8);
    fspeMargemDir.Value := Ini.ReadFloat('DANFE', 'MargemDir', 0.51);
    fspeMargemEsq.Value := Ini.ReadFloat('DANFE', 'MargemEsq', 0.6);
    fspeNFCeMargemInf.Value := Ini.ReadFloat('DANFCe', 'MargemInf', 0.8);
    fspeNFCeMargemSup.Value := Ini.ReadFloat('DANFCe', 'MargemSup', 0.8);
    fspeNFCeMargemDir.Value := Ini.ReadFloat('DANFCe', 'MargemDir', 0.51);
    fspeNFCeMargemEsq.Value := Ini.ReadFloat('DANFCe', 'MargemEsq', 0.6);
    fspeLarguraNFCe.Value   := Ini.ReadInteger('DANFCe', 'LarguraBobina', 302);
    edtPathPDF.Text :=
      Ini.ReadString('DANFE', 'PathPDF', PathApplication+'PDF');
    spedtCasasDecimaisQtd.Value := Ini.ReadInteger('DANFE', 'DecimaisQTD', 2);
    spedtDecimaisVUnit.Value := Ini.ReadInteger('DANFE', 'DecimaisValor', 2);
    cbxExibeResumo.Checked := Ini.ReadBool('DANFE', 'ExibeResumo', False);
    cbxImprimirTributos.Checked :=
      Ini.ReadBool('DANFE', 'ImprimirTributosItem', False);
    cbxImpValLiq.Checked := Ini.ReadBool('DANFE', 'ImprimirValLiq', False);
    cbxUnComTributavel.ItemIndex := Ini.ReadInteger('DANFE', 'UNComercialETributavel', 0);
    cbxFormCont.Checked := Ini.ReadBool('DANFE', 'PreImpresso', False);
    cbxMostraStatus.Checked := Ini.ReadBool('DANFE', 'MostrarStatus', False);
    cbxExibirEAN.Checked := Ini.ReadBool('DANFE', 'ExibirEAN', False);
    cbxExibirCampoFatura.Checked := Ini.ReadBool('DANFE', 'ExibirCampoFatura', True);
    cbxExpandirLogo.Checked := Ini.ReadBool('DANFE', 'ExpandirLogo', False);
    rgTipoFonte.ItemIndex := Ini.ReadInteger('DANFE', 'Fonte', 0);
    rgLocalCanhoto.ItemIndex := Ini.ReadInteger('DANFE', 'LocalCanhoto', 0);
    cbxQuebrarLinhasDetalhesItens.Checked := ini.ReadBool('DANFE','QuebrarLinhasDetalheItens', False) ;
    cbxImpDetEspNFe.Checked := ini.ReadBool('DANFE','ImprimirDetalhamentoEspecifico', True) ;
    cbxImpDocsReferenciados.Checked := ini.ReadBool('DANFE','ImprimirDadosDocReferenciados', True) ;
    cbxExibirInfAdicProduto.Checked := Ini.ReadBool('DANFE', 'ExibirBandInforAdicProduto', False);
    cbxExibirLogoEmCima.Checked := Ini.ReadBool('DANFE', 'LogoEmCima', False);

    cbxImpDescPorcChange(nil);

    if rgModeloDanfe.ItemIndex = 0 then
    begin
      ACBrNFe1.DANFE           := ACBrNFeDANFeRL1;
      ACBrCTe1.DACTE           := ACBrCTeDACTeRL1;
      ACBrMDFe1.DAMDFE         := ACBrMDFeDAMDFeRL1;
      ACBrGNRE1.GNREGuia       :=ACBrGNREGuiaRL1;
    end;

    if ckNFCeUsarIntegrador.Checked then
      ACBrNFe1.Integrador := ACBrIntegrador1
    else
      ACBrNFe1.Integrador := nil;

    ACBrCTeDACTeRL1.TamanhoPapel := TpcnTamanhoPapel(rgTamanhoPapelDacte.ItemIndex);

    rgModeloDANFeNFCE.ItemIndex := Ini.ReadInteger('NFCe', 'Modelo', 0);
    rgModoImpressaoEvento.ItemIndex :=
      Ini.ReadInteger('NFCe', 'ModoImpressaoEvento', 0);
    cbxImprimirItem1LinhaNFCe.Checked :=
      Ini.ReadBool('NFCe', 'ImprimirItem1Linha', True);
    cbxImprimirDescAcresItemNFCe.Checked :=
      Ini.ReadBool('NFCe', 'ImprimirDescAcresItem', True);
    cbxImpressoraNFCe.ItemIndex :=
      cbxImpressoraNFCe.Items.IndexOf(Ini.ReadString('NFCe', 'ImpressoraPadrao', '0'));
    cbxImprimirQRCodeLateralNFCe.Checked :=
      Ini.ReadBool('NFCe', 'QRCodeLateral', True);
    cbxImprimirCodigoEANNFCe.Checked :=
      Ini.ReadBool('NFCe', 'UsaCodigoEanImpressao', False);
    cbxImprimirNomeFantasiaNFCe.Checked :=
      Ini.ReadBool('NFCe', 'ImprimeNomeFantasia', False);


    ACBrCTe1.DACTe.TipoDACTE  := StrToTpImp(OK,IntToStr(rgTipoDanfe.ItemIndex+1));
    ACBrCTe1.DACTe.Logo       := edtLogoMarca.Text;
    ACBrCTe1.DACTe.Sistema := edSH_RazaoSocial.Text;
    ACBrCTe1.DACTe.Site    := edtSiteEmpresa.Text;
    ACBrCTe1.DACTe.Email   := edtEmailEmpresa.Text;
    ACBrCTe1.DACTe.Fax     := edtFaxEmpresa.Text;
    ACBrCTe1.DACTe.ImprimirDescPorc  := cbxImpDescPorc.Checked;
    ACBrCTe1.DACTe.MostrarPreview    := cbxMostrarPreview.Checked;
    ACBrCTe1.DACTe.Impressora := cbxImpressora.Text;
    ACBrCTe1.DACTe.NumCopias  := edtNumCopia.Value;
    ACBrCTe1.DACTe.MargemInferior  := fspeMargemInf.Value;
    ACBrCTe1.DACTe.MargemSuperior  := fspeMargemSup.Value;
    ACBrCTe1.DACTe.MargemDireita   := fspeMargemDir.Value;
    ACBrCTe1.DACTe.MargemEsquerda  := fspeMargemEsq.Value;
    ACBrCTe1.DACTe.PathPDF     := edtPathPDF.Text;
    ACBrCTe1.DACTe.MostrarStatus        := cbxMostraStatus.Checked;
    ACBrCTe1.DACTe.ExpandirLogoMarca    := cbxExpandirLogo.Checked;
    ACBrCTe1.DACTE.UsarSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;
    ACBrCTeDACTeRL1.PosCanhoto := TPosRecibo( rgLocalCanhoto.ItemIndex );

    ACBrMDFe1.DAMDFe.TipoDAMDFe        := StrToTpImp(OK,IntToStr(rgTipoDanfe.ItemIndex+1));
    ACBrMDFe1.DAMDFe.Logo              := edtLogoMarca.Text;
    ACBrMDFe1.DAMDFe.Sistema           := edSH_RazaoSocial.Text;
    ACBrMDFe1.DAMDFe.Site              := edtSiteEmpresa.Text;
    ACBrMDFe1.DAMDFe.Email             := edtEmailEmpresa.Text;
    ACBrMDFe1.DAMDFe.Fax               := edtFaxEmpresa.Text;
    ACBrMDFe1.DAMDFe.MostrarPreview    := cbxMostrarPreview.Checked;
    ACBrMDFe1.DAMDFe.Impressora        := cbxImpressora.Text;
    ACBrMDFe1.DAMDFe.NumCopias         := edtNumCopia.Value;
    ACBrMDFe1.DAMDFe.MargemInferior    := fspeMargemInf.Value;
    ACBrMDFe1.DAMDFe.MargemSuperior    := fspeMargemSup.Value;
    ACBrMDFe1.DAMDFe.MargemDireita     := fspeMargemDir.Value;
    ACBrMDFe1.DAMDFe.MargemEsquerda    := fspeMargemEsq.Value;
    ACBrMDFe1.DAMDFe.PathPDF           := edtPathPDF.Text;
    ACBrMDFe1.DAMDFe.MostrarStatus     := cbxMostraStatus.Checked;
    ACBrMDFe1.DAMDFe.ExpandirLogoMarca := cbxExpandirLogo.Checked;
    ACBrMDFe1.DAMDFe.UsarSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;

    ACBrGNRE1.GNREGuia.Sistema         := edSH_RazaoSocial.Text;
    ACBrGNRE1.GNREGuia.Site            := edtSiteEmpresa.Text;
    ACBrGNRE1.GNREGuia.Email           := edtEmailEmpresa.Text;
    ACBrGNRE1.GNREGuia.Fax             := edtFaxEmpresa.Text;
    ACBrGNRE1.GNREGuia.MostrarPreview  := cbxMostrarPreview.Checked;
    ACBrGNRE1.GNREGuia.Impressora      := cbxImpressora.Text;
    ACBrGNRE1.GNREGuia.NumCopias       := edtNumCopia.Value;
    ACBrGNRE1.GNREGuia.MargemInferior  := fspeMargemInf.Value;
    ACBrGNRE1.GNREGuia.MargemSuperior  := fspeMargemSup.Value;
    ACBrGNRE1.GNREGuia.MargemDireita   := fspeMargemDir.Value;
    ACBrGNRE1.GNREGuia.MargemEsquerda  := fspeMargemEsq.Value;
    ACBrGNRE1.GNREGuia.PathPDF         := edtPathPDF.Text;
    ACBrGNRE1.GNREGuia.MostrarStatus   := cbxMostraStatus.Checked;

    edtEmailAssuntoNFe.Text := Ini.ReadString('Email', 'AssuntoNFe', '');
    mmEmailMsgNFe.Lines.Text := StringToBinaryString( Ini.ReadString('Email', 'MensagemNFe', '') );
    edtEmailAssuntoCTe.Text := Ini.ReadString('Email', 'AssuntoCTe', '');
    mmEmailMsgCTe.Lines.Text := StringToBinaryString( Ini.ReadString('Email', 'MensagemCTe', '') );
    edtEmailAssuntoMDFe.Text := Ini.ReadString('Email', 'AssuntoMDFe', '');
    mmEmailMsgMDFe.Lines.Text := StringToBinaryString( Ini.ReadString('Email', 'MensagemMDFe', '') );

    VerificaDiretorios;

    // token da nfce
    ACBrNFe1.Configuracoes.Geral.IdCSC := Ini.ReadString('NFCe', 'IdToken', '');
    ACBrNFe1.Configuracoes.Geral.CSC := Ini.ReadString('NFCe', 'Token', '');
    ACBrNFe1.Configuracoes.Geral.IncluirQRCodeXMLNFCe := Ini.ReadBool('NFCe', 'TagQrCode', True);

    ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativo := ESCPOSAtivado;

    {Parametro SAT}

    cbxModeloSAT.ItemIndex    := INI.ReadInteger('SAT','Modelo',0);
    edSATLog.Text             := INI.ReadString('SAT','ArqLog','ACBrSAT.log');
    edNomeDLL.Text         := INI.ReadString('SAT','NomeDLL',PathApplication+'SAT\Emulador\SAT.DLL');
    edtCodigoAtivacao.Text := INI.ReadString('SAT','CodigoAtivacao','123456');
    edtCodUF.Text          := INI.ReadString('SAT','CodigoUF','35');
    seNumeroCaixa.Value    := INI.ReadInteger('SAT','NumeroCaixa',1);
    cbxAmbiente.ItemIndex  := INI.ReadInteger('SAT','Ambiente',1);
    sePagCod.Value         := INI.ReadInteger('SAT','PaginaDeCodigo',0);
    sePagCodChange(self);
    sfeVersaoEnt.Value     := INI.ReadFloat('SAT','versaoDadosEnt', cversaoDadosEnt);
    cbxFormatXML.Checked   := INI.ReadBool('SAT','FormatarXML', True);
    edSATPathArqs.Text     := INI.ReadString('SAT','PathCFe',PathApplication+'Arqs'+PathDelim+'SAT');
    cbxSATSalvarCFe.Checked     := INI.ReadBool('SAT','SalvarCFe', True);
    cbxSATSalvarCFeCanc.Checked := INI.ReadBool('SAT','SalvarCFeCanc', True);
    cbxSATSalvarEnvio.Checked   := INI.ReadBool('SAT','SalvarEnvio', True);
    cbxSATSepararPorCNPJ.Checked:= INI.ReadBool('SAT','SepararPorCNPJ', True);
    cbxSATSepararPorMES.Checked := INI.ReadBool('SAT','SepararPorMES', True);

    ACBrSATExtratoESCPOS1.PosPrinter.Device.ParamsString := INI.ReadString('SATExtrato','ParamsString','');
    ACBrSATExtratoESCPOS1.ImprimeDescAcrescItem := INI.ReadBool('SATExtrato', 'ImprimeDescAcrescItem', True);
    ACBrSATExtratoESCPOS1.ImprimeEmUmaLinha := INI.ReadBool('SATExtrato', 'ImprimeEmUmaLinha', False);
    ACBrSATExtratoESCPOS1.UsaCodigoEanImpressao := INI.ReadBool('SATExtrato', 'UsaCodigoEanImpressao', False);

    ACBrSATExtratoFortes1.ImprimeDescAcrescItem := INI.ReadBool('SATExtrato', 'ImprimeDescAcrescItem', True);
    ACBrSATExtratoFortes1.ImprimeEmUmaLinha := INI.ReadBool('SATExtrato', 'ImprimeEmUmaLinha', False);
    ACBrSATExtratoFortes1.UsaCodigoEanImpressao := INI.ReadBool('SATExtrato', 'UsaCodigoEanImpressao', False);

    cbxImprimirDescAcresItemSAT.Checked := ACBrSATExtratoESCPOS1.ImprimeDescAcrescItem;
    cbxImprimirItem1LinhaSAT.Checked := ACBrSATExtratoESCPOS1.ImprimeEmUmaLinha;
    cbxImprimirCodEANitemSAT.Checked := ACBrSATExtratoESCPOS1.UsaCodigoEanImpressao;

    rdgImprimeChave1LinhaSAT.ItemIndex := INI.ReadInteger('SATExtrato', 'ImprimeChaveEmUmaLinha', 0);
    ACBrSATExtratoESCPOS1.ImprimeChaveEmUmaLinha := TAutoSimNao(rdgImprimeChave1LinhaSAT.ItemIndex);

    edtEmitCNPJ.Text := INI.ReadString('SATEmit','CNPJ','');
    edtEmitIE.Text   := INI.ReadString('SATEmit','IE','');
    edtEmitIM.Text   := INI.ReadString('SATEmit','IM','');
    cbxRegTributario.ItemIndex := INI.ReadInteger('SATEmit','RegTributario',0);
    cbxRegTribISSQN.ItemIndex  := INI.ReadInteger('SATEmit','RegTribISSQN',0);
    cbxIndRatISSQN.ItemIndex   := INI.ReadInteger('SATEmit','IndRatISSQN',0);

    edtSwHCNPJ.Text       := INI.ReadString('SATSwH','CNPJ','');
    edtSwHAssinatura.Text := INI.ReadString('SATSwH','Assinatura','');

    cbUsarFortes.Checked   := INI.ReadBool('SATFortes','UsarFortes', True) ;
    cbUsarEscPos.Checked   := not cbUsarFortes.Checked;
    seLargura.Value        := INI.ReadInteger('SATFortes','Largura',ACBrSATExtratoFortes1.LarguraBobina);
    seMargemTopo.Value     := INI.ReadInteger('SATFortes','MargemTopo',ACBrSATExtratoFortes1.Margens.Topo);
    seMargemFundo.Value    := INI.ReadInteger('SATFortes','MargemFundo',ACBrSATExtratoFortes1.Margens.Fundo);
    seMargemEsquerda.Value := INI.ReadInteger('SATFortes','MargemEsquerda',ACBrSATExtratoFortes1.Margens.Esquerda);
	seMargemDireita.Value  := INI.ReadInteger('SATFortes','MargemDireita',ACBrSATExtratoFortes1.Margens.Direita);
    cbPreview.Checked      := INI.ReadBool('SATFortes','Preview',True);

    lImpressora.Caption := INI.ReadString('SATPrinter','Name',Printer.PrinterName);

    rgRedeTipoInter.ItemIndex := INI.ReadInteger('SATRede','tipoInter',0);
    rgRedeTipoLan.ItemIndex   := INI.ReadInteger('SATRede','tipoLan',0);
    edRedeSSID.Text           := INI.ReadString('SATRede','SSID','');
    cbxRedeSeg.ItemIndex      := INI.ReadInteger('SATRede','seg',0);
    edRedeCodigo.Text         := INI.ReadString('SATRede','codigo','');
    edRedeIP.Text             := INI.ReadString('SATRede','lanIP','');
    edRedeMask.Text           := INI.ReadString('SATRede','lanMask','');
    edRedeGW.Text             := INI.ReadString('SATRede','lanGW','');
    edRedeDNS1.Text           := INI.ReadString('SATRede','lanDNS1','');
    edRedeDNS2.Text           := INI.ReadString('SATRede','lanDNS2','');
    edRedeUsuario.Text        := INI.ReadString('SATRede','usuario','');
    edRedeSenha.Text          := INI.ReadString('SATRede','senha','');
    cbxRedeProxy.ItemIndex    := INI.ReadInteger('SATRede','proxy',0);
    edRedeProxyIP.Text        := INI.ReadString('SATRede','proxy_ip','');
    edRedeProxyPorta.Value    := INI.ReadInteger('SATRede','proxy_porta',0);
    edRedeProxyUser.Text      := INI.ReadString('SATRede','proxy_user','');
    edRedeProxySenha.Text     := INI.ReadString('SATRede','proxy_senha','');

    edMFEInput.Text           := INI.ReadString('SATIntegrador','Input','c:\Integrador\Input\');
    edMFEOutput.Text          := INI.ReadString('SATIntegrador','Output','c:\Integrador\Output\');
    seMFETimeout.Value        := INI.ReadInteger('SATIntegrador','Timeout',30);

    AjustaACBrSAT;

    {Parâmetro PosPrinter}
    POSPrDeviceParams       := INI.ReadString('PosPrinter', 'SerialParams', ACBrPosPrinter1.Device.ParamsString);
    cbxModelo.ItemIndex     := INI.ReadInteger('PosPrinter', 'Modelo', Integer(ACBrPosPrinter1.Modelo));
    cbxPorta.Text           := INI.ReadString('PosPrinter', 'Porta', ACBrPosPrinter1.Porta);
    seColunas.Value         := INI.ReadInteger('PosPrinter', 'Colunas', ACBrPosPrinter1.ColunasFonteNormal);
    seEspacosLinhas.Value   := INI.ReadInteger('PosPrinter', 'EspacoEntreLinhas', ACBrPosPrinter1.EspacoEntreLinhas);
    seBuffer.Value          := INI.ReadInteger('PosPrinter', 'LinhasBuffer', ACBrPosPrinter1.LinhasBuffer);
    seLinhasPular.Value     := INI.ReadInteger('PosPrinter', 'LinhasPular', ACBrPosPrinter1.LinhasEntreCupons);
    cbxPagCodigo.ItemIndex  := INI.ReadInteger('PosPrinter', 'PaginaDeCodigo', Integer(ACBrPosPrinter1.PaginaDeCodigo));
    cbControlePorta.Checked := INI.ReadBool('PosPrinter', 'ControlePorta', ACBrPosPrinter1.ControlePorta);
    cbCortarPapel.Checked   := INI.ReadBool('PosPrinter', 'CortarPapel', ACBrPosPrinter1.CortaPapel);

    cbTraduzirTags.Checked  := INI.ReadBool('PosPrinter', 'TraduzirTags', ACBrPosPrinter1.TraduzirTags);
    cbIgnorarTags.Checked   := INI.ReadBool('PosPrinter', 'IgnorarTags', ACBrPosPrinter1.IgnorarTags);
    edPosPrinterLog.Text    := INI.ReadString('PosPrinter', 'ArqLog', ACBrPosPrinter1.ArqLOG);

    seCodBarrasLargura.Value := INI.ReadInteger('Barras', 'Largura', ACBrPosPrinter1.ConfigBarras.LarguraLinha);
    seCodBarrasAltura.Value  := INI.ReadInteger('Barras', 'Altura', ACBrPosPrinter1.ConfigBarras.Altura);
    cbHRI.Checked            := INI.ReadBool('Barras', 'HRI', ACBrPosPrinter1.ConfigBarras.MostrarCodigo);

    seQRCodeTipo.Value       := INI.ReadInteger('QRCode', 'Tipo', ACBrPosPrinter1.ConfigQRCode.Tipo);
    seQRCodeLargMod.Value    := INI.ReadInteger('QRCode', 'LarguraModulo', ACBrPosPrinter1.ConfigQRCode.LarguraModulo);
    seQRCodeErrorLevel.Value := INI.ReadInteger('QRCode', 'ErrorLevel', ACBrPosPrinter1.ConfigQRCode.ErrorLevel);

    cbEscPosImprimirLogo.Checked := INI.ReadBool('Logo', 'Imprimir', not ACBrPosPrinter1.ConfigLogo.IgnorarLogo);
    seLogoKC1.Value    := INI.ReadInteger('Logo', 'KC1', ACBrPosPrinter1.ConfigLogo.KeyCode1);
    seLogoKC2.Value    := INI.ReadInteger('Logo', 'KC2', ACBrPosPrinter1.ConfigLogo.KeyCode2);
    seLogoFatorX.Value := INI.ReadInteger('Logo', 'FatorX', ACBrPosPrinter1.ConfigLogo.FatorX);
    seLogoFatorY.Value := INI.ReadInteger('Logo', 'FatorY', ACBrPosPrinter1.ConfigLogo.FatorY);

    DefineTextoTrayTitulo;

    ConfiguraPosPrinter(POSPrDeviceParams);
  finally
    Ini.Free;
  end;

  if FileExists(PathWithDelim(ExtractFilePath(Application.ExeName)) + 'swh.ini') then;
     LerSW;

  with ACBrECF1 do
  begin
    Desativar;
    Modelo := TACBrECFModelo(Max(cbECFModelo.ItemIndex - 1, 0));
    Porta := cbECFPorta.Text;
    if ECFDeviceParams <> '' then
      Device.ParamsString := ECFDeviceParams;
    TimeOut := sedECFTimeout.Value;
    IntervaloAposComando := sedECFIntervalo.Value;
    MaxLinhasBuffer := sedECFMaxLinhasBuffer.Value;
    PaginaDeCodigo := sedECFPaginaCodigo.Value;
    LinhasEntreCupons := sedECFLinhasEntreCupons.Value;
    ArredondaPorQtd := chECFArredondaPorQtd.Checked;
    ArredondaItemMFD := ((chECFArredondaMFD.Enabled) and
      (chECFArredondaMFD.Checked));
    DescricaoGrande := chECFDescrGrande.Checked;
    GavetaSinalInvertido := chECFSinalGavetaInvertido.Checked;
    BloqueiaMouseTeclado := False;
    ExibeMensagem := False;
    IgnorarTagsFormatacao := chECFIgnorarTagsFormatacao.Checked;
    ControlePorta := chECFControlePorta.Checked;
    ArqLOG := edECFLog.Text;
    Ativo := ECFAtivado;
  end;

  with ACBrCHQ1 do
  begin
    Desativar;
    Modelo := TACBrCHQModelo(cbCHQModelo.ItemIndex);
    Porta := cbCHQPorta.Text;
    if CHQDeviceParams <> '' then
      Device.ParamsString := CHQDeviceParams;
    Favorecido := edCHQFavorecido.Text;
    Cidade := edCHQCidade.Text;

    if edCHQBemafiINI.Text <> '' then
    begin
      try
        ArquivoBemaFiINI := edCHQBemafiINI.Text;
        AddLinesLog('Arquivo de Cheques: ' + ArquivoBemaFiINI +
          sLineBreak + ' lido com sucesso.');
      except
        on E: Exception do
          AddLinesLog(E.Message);
      end;
    end;
    Ativo := CHQAtivado;
  end;

  with ACBrGAV1 do
  begin
    Desativar;
    StrComando := cbGAVStrAbre.Text;
    AberturaIntervalo := sedGAVIntervaloAbertura.Value;
    AberturaAntecipada := TACBrGAVAberturaAntecipada(
      cbGAVAcaoAberturaAntecipada.ItemIndex);
    Modelo := TACBrGAVModelo(cbGAVModelo.ItemIndex);
    Porta := cbGAVPorta.Text;
    Ativo := (GAVAtivado or (pos('serial', LowerCase(ModeloStr)) > 0));
  end;

  with ACBrDIS1 do
  begin
    Desativar;
    Intervalo := seDISIntervalo.Value;
    Passos := seDISPassos.Value;
    IntervaloEnvioBytes := seDISIntByte.Value;
    Modelo := TACBrDISModelo(cbDISModelo.ItemIndex);
    Porta := cbDISPorta.Text;
    Ativo := DISAtivado;
  end;

  with ACBrLCB1 do
  begin
    Desativar;
    Porta := cbLCBPorta.Text;
    Intervalo := sedLCBIntervalo.Value;
    Sufixo := cbLCBSufixoLeitor.Text;
    ExcluirSufixo := chLCBExcluirSufixo.Checked;
    PrefixoAExcluir := edLCBPreExcluir.Text;
    UsarFila := rbLCBFila.Checked;

    { SndKey32.pas só funciona no Windows pois usa a API  "keybd_event" }
    if (ACBrLCB1.Porta <> 'Sem Leitor') and (ACBrLCB1.Porta <> '') then
      ACBrLCB1.Ativar;
  end;

  with ACBrRFD1 do
  begin
    DirRFD := edRFDDir.Text;
    IgnoraEcfMfd := chRFDIgnoraMFD.Checked;

    if chRFD.Checked then
    begin
      if FileExists(ArqINI) then
        ACBrRFD1.LerINI;
    end;
  end;

  with ACBrBAL1 do
  begin
    Desativar;
    Intervalo := sedBALIntervalo.Value;
    Modelo := TACBrBALModelo(cbBALModelo.ItemIndex);
    Porta := cbBALPorta.Text;
    Ativo := BALAtivado;
    ArqLOG := edBALLog.Text;
  end;

  with ACBrETQ1 do
  begin
    Desativar;

    DPI           := TACBrETQDPI(cbDPI.ItemIndex);
    Modelo        := TACBrETQModelo(cbETQModelo.ItemIndex);
    Porta         :=  cbETQPorta.Text;
    LimparMemoria := ckMemoria.Checked;
    Temperatura   := StrToIntDef(eTemperatura.Text,10);
    Velocidade    := StrToIntDef(eVelocidade.Text,-1);
    BackFeed      := TACBrETQBackFeed(cbBackFeed.ItemIndex);
    Unidade       := etqMilimetros;
    MargemEsquerda:= StrToIntDef(eMargemEsquerda.Text, 0);
    Origem        := TACBrETQOrigem(cbOrigem.ItemIndex);


    Ativo := ETQAtivado;
  end;

  with ACBrCEP1 do
  begin
    WebService := TACBrCEPWebService(cbCEPWebService.ItemIndex);
    ChaveAcesso := edCEPChaveBuscarCEP.Text;
    ProxyHost := edCONProxyHost.Text;
    ProxyPort := edCONProxyPort.Text;
    ProxyUser := edCONProxyUser.Text;
    ProxyPass := edCONProxyPass.Text;
  end;

  with ACBrMail1 do
  begin
    FromName := edEmailNome.Text;
    From := edEmailEndereco.Text;
    Host := edEmailHost.Text;
    Port := IntToStr(edEmailPorta.Value);
    Username := edEmailUsuario.Text;
    Password := edEmailSenha.Text;
    SetSSL := cbEmailSsl.Checked;
    SetTLS := cbEmailTls.Checked;
    ReadingConfirmation := cbEmailConfirmation.Checked;
    UseThread := cbEmailThread.Checked;
    DefaultCharset := TMailCharset(GetEnumValue(TypeInfo(TMailCharset),
      cbEmailCodificacao.Text));
  end;

  with ACBrIBGE1 do
  begin
    ProxyHost := edCONProxyHost.Text;
    ProxyPort := edCONProxyPort.Text;
    ProxyUser := edCONProxyUser.Text;
    ProxyPass := edCONProxyPass.Text;
  end;

  with ACBrSedex1 do
  begin
    CodContrato := edtSedexContrato.Text;
    Senha := edtSedexSenha.Text;
    ProxyHost := edCONProxyHost.Text;
    ProxyPort := edCONProxyPort.Text;
    ProxyUser := edCONProxyUser.Text;
    ProxyPass := edCONProxyPass.Text;
  end;

  with ACBrNCMs1 do
  begin
    ProxyHost := edCONProxyHost.Text;
    ProxyPort := edCONProxyPort.Text;
    ProxyUser := edCONProxyUser.Text;
    ProxyPass := edCONProxyPass.Text;
  end;

  with ACBrBoleto1 do
  begin
    Cedente.Nome := edtBOLRazaoSocial.Text;

    if cbxBOLF_J.ItemIndex = 0 then
      Cedente.TipoInscricao := pFisica
    else
      Cedente.TipoInscricao := pJuridica;

    Cedente.CNPJCPF := edtBOLCNPJ.Text;

    Cedente.Logradouro := edtBOLLogradouro.Text;
    Cedente.NumeroRes := edtBOLNumero.Text;
    Cedente.Bairro := edtBOLBairro.Text;
    Cedente.Cidade := edtBOLCidade.Text;
    Cedente.CEP := edtBOLCEP.Text;
    Cedente.Complemento := edtBOLComplemento.Text;
    Cedente.UF := cbxBOLUF.Text;
    Cedente.CodigoCedente := edtCodCliente.Text;
    Cedente.Convenio := edtConvenio.Text;
    Cedente.CodigoTransmissao := edtCodTransmissao.Text;

    Banco.TipoCobranca   := TACBrTipoCobranca(cbxBOLBanco.ItemIndex);
    Banco.LocalPagamento := edtBOLLocalPagamento.Text;

    Cedente.Agencia := edtBOLAgencia.Text;
    Cedente.AgenciaDigito := edtBOLDigitoAgencia.Text;
    Cedente.Conta := edtBOLConta.Text;
    Cedente.ContaDigito := edtBOLDigitoConta.Text;
    Cedente.Modalidade := edtModalidade.Text;

    case cbxBOLEmissao.ItemIndex of
      0: Cedente.ResponEmissao := tbCliEmite;
      1: Cedente.ResponEmissao := tbBancoEmite;
      2: Cedente.ResponEmissao := tbBancoReemite;
      3: Cedente.ResponEmissao := tbBancoNaoReemite;
    end;

    if cbxCNAB.ItemIndex = 0 then
      LayoutRemessa := c240
    else
      LayoutRemessa := c400;

    DirArqRemessa   := PathWithDelim(deBolDirRemessa.Text);
    DirArqRetorno   := PathWithDelim(deBolDirRetorno.Text);
    LeCedenteRetorno:= chkLerCedenteRetorno.Checked;

    MAIL := ACBrMail1;
  end;

  with ACBrBoleto1.ACBrBoletoFC do
  begin
    Filtro := TACBrBoletoFCFiltro(cbxBOLFiltro.ItemIndex);
    LayOut := TACBrBolLayOut(cbxBOLLayout.ItemIndex);

    NumCopias := spBOLCopias.Value;
    SoftwareHouse := edSH_RazaoSocial.Text; //edtBOLSH.Text;
    DirLogo := deBOLDirLogo.Text;
    MostrarPreview := ckgBOLMostrar.Checked[0];
    MostrarProgresso := ckgBOLMostrar.Checked[1];
    MostrarSetup := ckgBOLMostrar.Checked[2];
    PrinterName := cbxBOLImpressora.Text;

    wNomeArquivo := Trim(deBOLDirArquivo.Text);
    if wNomeArquivo = '' then
      wNomeArquivo := ExtractFilePath(Application.ExeName)
    else
      wNomeArquivo := PathWithDelim(wNomeArquivo);

    if Filtro = TACBrBoletoFCFiltro(fiHTML) then
      NomeArquivo := wNomeArquivo + 'boleto.html'
    else
      NomeArquivo := wNomeArquivo + 'boleto.pdf';
  end;

  if cbxTCModelo.ItemIndex > 0 then
    bTCAtivar.Click;

  // Mostrar Abas
  pgConfig.ShowTabs:=cbAbas.Checked;
  pgSwHouse.ShowTabs:=cbAbas.Checked;
  pgConRFD.ShowTabs:=cbAbas.Checked;
  pgBoleto.ShowTabs:=cbAbas.Checked;
  pgDFe.ShowTabs:=cbAbas.Checked;
  pgSAT.ShowTabs:=cbAbas.Checked;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.LerSW;
var
  INI: TIniFile;
  ArqSWH, Pass, Chave: ansistring;
begin
  ArqSWH := ExtractFilePath(Application.ExeName) + 'swh.ini';
  if not FileExists(ArqSWH) then
  begin
    AddLinesLog('ATENÇÃO: Arquivo "swh.ini" não encontrado.' +
      sLineBreak + '     Nenhuma Chave RSA Privada pode ser lida.' + sLineBreak);
    exit;
  end;

  Ini := TIniFile.Create(ArqSWH);
  try
    edSH_CNPJ.Text := LeINICrypt(INI, 'SWH', 'CNPJ', IntToStrZero(fsHashSenha, 8));
    Pass := IntToStrZero(StringCrc16(edSH_CNPJ.Text + IntToStrZero(fsHashSenha, 8)), 8);

    if LeINICrypt(INI, 'SWH', 'Verifica', Pass) <> 'ARQUIVO SWH.INI ESTA OK' then
      raise Exception.Create('Arquivo "swh.ini" inválido.');

    edSH_RazaoSocial.Text := LeINICrypt(INI, 'SWH', 'RazaoSocial', Pass);
    edSH_COO.Text         := LeINICrypt(INI, 'SWH', 'COO', Pass);
    edSH_IE.Text          := LeINICrypt(INI, 'SWH', 'IE', Pass);
    edSH_IM.Text          := LeINICrypt(INI, 'SWH', 'IM', Pass);
    edSH_Aplicativo.Text  := LeINICrypt(INI, 'SWH', 'Aplicativo', Pass);
    edSH_NumeroAP.Text    := LeINICrypt(INI, 'SWH', 'NumeroAplicativo', Pass);
    edSH_VersaoAP.Text    := LeINICrypt(INI, 'SWH', 'VersaoAplicativo', Pass);
    edSH_Linha1.Text      := LeINICrypt(INI, 'SWH', 'Linha1', Pass);
    edSH_Linha2.Text      := LeINICrypt(INI, 'SWH', 'Linha2', Pass);
    edSH_Site.Text        := LeINICrypt(INI, 'SWH', 'Site', Pass);

    ACBrRFD1.SH_RazaoSocial      := edSH_RazaoSocial.Text;
    ACBrRFD1.SH_COO              := edSH_COO.Text;
    ACBrRFD1.SH_CNPJ             := edSH_CNPJ.Text;
    ACBrRFD1.SH_IE               := edSH_IE.Text;
    ACBrRFD1.SH_IM               := edSH_IM.Text;
    ACBrRFD1.SH_NomeAplicativo   := edSH_Aplicativo.Text;
    ACBrRFD1.SH_NumeroAplicativo := edSH_NumeroAP.Text;
    ACBrRFD1.SH_VersaoAplicativo := edSH_VersaoAP.Text;
    ACBrRFD1.SH_Linha1           := edSH_Linha1.Text;
    ACBrRFD1.SH_Linha2           := edSH_Linha2.Text;
  finally
    Ini.Free;
  end;

  try
    Chave := '';
    ACBrEAD1GetChavePrivada(Chave);
    mRSAKey.Text := '- Chave válida encontrada no arquivo "swh.ini"' +
      sLineBreak + '- Conteudo omitido por segurança. ' + sLineBreak +
      '- Chave será utilizada para assinatura digital';
  except
    mRSAKey.Text := 'ATENÇÃO: Chave RSA Privada NÃO pode ser lida no arquivo "swh.ini".';
    AddLinesLog(mRSAKey.Text + sLineBreak);
  end;
end;

{------------------------------------------------------------------------------}
function TFrmACBrMonitor.LerChaveSWH: ansistring;
var
  INI: TIniFile;
  Pass: string;
begin
  Result := '';
  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'swh.ini');
  try
    Pass := LeINICrypt(INI, 'SWH', 'CNPJ', IntToStrZero(fsHashSenha, 8));
    Pass := IntToStrZero(StringCrc16(Pass + IntToStrZero(fsHashSenha, 8)), 8);

    if LeINICrypt(INI, 'SWH', 'Verifica', Pass) = 'ARQUIVO SWH.INI ESTA OK' then
      Result := Trim(LeINICrypt(INI, 'SWH', 'RSA', Pass));
  finally
    Ini.Free;
  end;
end;


{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.SalvarIni;
var
  Ini: TIniFile;
  OldMonitoraTXT, OldMonitoraTCP, OldMonitoraPasta: boolean;
  OldVersaoSSL : Integer;
begin
  if cbSenha.Checked and (edSenha.Text <> 'NADAAQUI') and (edSenha.Text <> '') then
    fsHashSenha := StringCrc16(edSenha.Text);

  if pConfig.Visible and chRFD.Checked and (fsHashSenha < 1) then
  begin
    btnMonitorClick(btnMonitor); //    pgConfig.ActivePageIndex := 0;
    cbSenha.Checked := True;
    edSenha.SetFocus;
    raise Exception.Create('Para trabalhar com RFD é necessário definir uma Senha ' +
      'para proteger sua Chave Privada');
  end;

  if Trim(deNcmSalvar.Text) <> '' then
  begin
    if not DirectoryExists(deNcmSalvar.Text) then
    begin
      btnNCMClick(btnNCM); //pgConfig.ActivePageIndex := 15;
      deNcmSalvar.SetFocus;
      raise Exception.Create('Diretorio para salvar arquivo de NCM nao encontrado.');
    end;
  end;

  Ini := TIniFile.Create(ACBrMonitorINI);
  try
    // Verificando se modificou o Modo de Monitoramento //
    OldMonitoraTCP := Ini.ReadBool('ACBrMonitor', 'Modo_TCP', False);
    OldMonitoraTXT := Ini.ReadBool('ACBrMonitor', 'Modo_TXT', False);
    OldMonitoraPasta := Ini.ReadBool('ACBrMonitor', 'MonitorarPasta', False);
    OldVersaoSSL :=  Ini.ReadInteger('ACBrNFeMonitor', 'VersaoSSL', 0);

    // Parametros do Monitor //
    Ini.WriteBool('ACBrMonitor', 'Modo_TCP', rbTCP.Checked);
    Ini.WriteBool('ACBrMonitor', 'Modo_TXT', rbTXT.Checked);
    Ini.WriteBool('ACBrMonitor', 'MonitorarPasta', cbMonitorarPasta.Checked);
    Ini.WriteInteger('ACBrMonitor', 'TCP_Porta', StrToIntDef(edPortaTCP.Text, 3434));
    Ini.WriteInteger('ACBrMonitor', 'TCP_TimeOut', StrToIntDef(edTimeOutTCP.Text, 10000));
    Ini.WriteBool('ACBrMonitor','Converte_TCP_Ansi', chbTCPANSI.Checked);

    if cbMonitorarPasta.Checked then
    begin
      Ini.WriteString('ACBrMonitor', 'TXT_Entrada', PathWithDelim(edEntTXT.Text));
      Ini.WriteString('ACBrMonitor', 'TXT_Saida', PathWithDelim(edSaiTXT.Text));
    end
    else
    begin
      Ini.WriteString('ACBrMonitor', 'TXT_Entrada', edEntTXT.Text);
      Ini.WriteString('ACBrMonitor', 'TXT_Saida', edSaiTXT.Text);
    end;
    Ini.WriteBool('ACBrMonitor', 'Converte_TXT_Entrada_Ansi', chbArqEntANSI.Checked);
    Ini.WriteBool('ACBrMonitor', 'Converte_TXT_Saida_Ansi', chbArqSaiANSI.Checked);
    Ini.WriteInteger('ACBrMonitor', 'Intervalo', sedIntervalo.Value);
    GravaINICrypt(INI, 'ACBrMonitor', 'HashSenha', IntToStrZero(fsHashSenha, 8), _C);

    Ini.WriteBool('ACBrMonitor', 'Gravar_Log', cbLog.Checked);
    Ini.WriteString('ACBrMonitor', 'Arquivo_Log', edLogArq.Text);
    Ini.WriteInteger('ACBrMonitor', 'Linhas_Log', sedLogLinhas.Value);
    Ini.WriteBool('ACBrMonitor', 'Comandos_Remotos', cbComandos.Checked);
    Ini.WriteBool('ACBrMonitor', 'Uma_Instancia', cbUmaInstancia.Checked);
    Ini.WriteBool('ACBrMonitor', 'MostraAbas', cbAbas.Checked);
    Ini.WriteBool('ACBrMonitor', 'MostrarNaBarraDeTarefas', cbMostrarNaBarraDeTarefas.Checked);
    Ini.WriteBool('ACBrMonitor', 'RetirarAcentosNaResposta', cbRetirarAcentosNaResposta.Checked);
    Ini.WriteBool('ACBrMonitor', 'MostraLogEmRespostasEnviadas', chkMostraLogNaTela.Checked);

    { Parametros do ECF }
    Ini.WriteInteger('ECF', 'Modelo', max(cbECFModelo.ItemIndex - 1, 0));
    Ini.WriteString('ECF', 'Porta', cbECFPorta.Text);
    Ini.WriteString('ECF', 'SerialParams', ACBrECF1.Device.ParamsString);
    Ini.WriteInteger('ECF', 'Timeout', sedECFTimeout.Value);
    Ini.WriteInteger('ECF', 'IntervaloAposComando', sedECFIntervalo.Value);
    Ini.WriteInteger('ECF', 'MaxLinhasBuffer', sedECFMaxLinhasBuffer.Value);
    Ini.WriteInteger('ECF', 'PaginaCodigo', sedECFPaginaCodigo.Value);
    Ini.WriteInteger('ECF', 'LinhasEntreCupons', sedECFLinhasEntreCupons.Value);
    Ini.WriteBool('ECF', 'ArredondamentoPorQtd', chECFArredondaPorQtd.Checked);
    Ini.WriteBool('ECF', 'ArredondamentoItemMFD', chECFArredondaMFD.Checked);
    Ini.WriteBool('ECF', 'DescricaoGrande', chECFDescrGrande.Checked);
    Ini.WriteBool('ECF', 'GavetaSinalInvertido', chECFSinalGavetaInvertido.Checked);
    Ini.WriteBool('ECF', 'IgnorarTagsFormatacao', chECFIgnorarTagsFormatacao.Checked);
    Ini.WriteBool('ECF', 'ControlePorta', chECFControlePorta.Checked);
    Ini.WriteString('ECF', 'ArqLog', edECFLog.Text);

    { Parametros do CHQ }
    Ini.WriteInteger('CHQ', 'Modelo', cbCHQModelo.ItemIndex);
    Ini.WriteString('CHQ', 'Porta', cbCHQPorta.Text);
    Ini.WriteString('CHQ', 'SerialParams', ACBrCHQ1.Device.ParamsString);
    Ini.WriteBool('CHQ', 'VerificaFormulario', chCHQVerForm.Checked);
    Ini.WriteString('CHQ', 'Favorecido', edCHQFavorecido.Text);
    Ini.WriteString('CHQ', 'Cidade', edCHQCidade.Text);
    Ini.WriteString('CHQ', 'PathBemafiINI', edCHQBemafiINI.Text);

    { Parametros do GAV }
    Ini.WriteInteger('GAV', 'Modelo', cbGAVModelo.ItemIndex);
    Ini.WriteString('GAV', 'Porta', cbGAVPorta.Text);
    Ini.WriteString('GAV', 'StringAbertura', cbGAVStrAbre.Text);
    Ini.WriteInteger('GAV', 'AberturaIntervalo', sedGAVIntervaloAbertura.Value);
    Ini.WriteInteger('GAV', 'AcaoAberturaAntecipada',
      cbGAVAcaoAberturaAntecipada.ItemIndex);

    { Parametros do DIS }
    Ini.WriteInteger('DIS', 'Modelo', cbDISModelo.ItemIndex);
    Ini.WriteString('DIS', 'Porta', cbDISPorta.Text);
    Ini.WriteInteger('DIS', 'Intervalo', seDISIntervalo.Value);
    Ini.WriteInteger('DIS', 'Passos', seDISPassos.Value);
    Ini.WriteInteger('DIS', 'IntervaloEnvioBytes', seDISIntByte.Value);

    { Parametros do LCB }
    Ini.WriteString('LCB', 'Porta', cbLCBPorta.Text);
    Ini.WriteInteger('LCB', 'Intervalo', sedLCBIntervalo.Value);
    Ini.WriteString('LCB', 'SufixoLeitor', cbLCBSufixoLeitor.Text);
    Ini.WriteBool('LCB', 'ExcluirSufixo', chLCBExcluirSufixo.Checked);
    Ini.WriteString('LCB', 'PrefixoAExcluir', edLCBPreExcluir.Text);
    Ini.WriteString('LCB', 'SufixoIncluir', cbLCBSufixo.Text);
    Ini.WriteString('LCB', 'Dispositivo', cbLCBDispositivo.Text);
    Ini.WriteBool('LCB', 'Teclado', rbLCBTeclado.Checked);
    Ini.WriteString('LCB', 'Device', ACBrLCB1.Device.ParamsString);

    { Parametros do RFD }
    INI.WriteBool('RFD', 'GerarRFD', chRFD.Checked);
    INI.WriteString('RFD', 'DirRFD', edRFDDir.Text);
    INI.WriteBool('RFD', 'IgnoraECF_MFD', chRFDIgnoraMFD.Checked);

    { Parametros do BAL }
    Ini.WriteInteger('BAL', 'Modelo', cbBALModelo.ItemIndex);
    Ini.WriteString('BAL', 'Porta', cbBALPorta.Text);
    Ini.WriteInteger('BAL', 'Intervalo', sedBALIntervalo.Value);
    Ini.WriteString('BAL', 'ArqLog', edBALLog.Text);
    Ini.WriteString('BAL', 'Device', ACBrBAL1.Device.ParamsString);

    { Parametros do ETQ }
    Ini.WriteInteger('ETQ', 'Modelo', cbETQModelo.ItemIndex);
    Ini.WriteString('ETQ', 'Porta', cbETQPorta.Text);
    Ini.WriteInteger('ETQ', 'DPI', cbDPI.ItemIndex);
    Ini.WriteBool('ETQ', 'LimparMemoria', ckMemoria.Checked);
    Ini.WriteInteger('ETQ', 'Temperatura', StrToIntDef(eTemperatura.Text, 10));
    Ini.WriteInteger('ETQ', 'Velocidade', StrToIntDef(eVelocidade.Text, -1));
    Ini.WriteInteger('ETQ', 'BackFeed', cbBackFeed.ItemIndex);
    Ini.WriteInteger('ETQ', 'MargemEsquerda', StrToIntDef(eMargemEsquerda.Text, 10));
    Ini.WriteInteger('ETQ', 'Origem', cbOrigem.ItemIndex);
    Ini.WriteInteger('ETQ', 'Copias', StrToIntDef(eCopias.Text, 1));

    { Parametros do CEP }
    Ini.WriteInteger('CEP', 'WebService', cbCEPWebService.ItemIndex);
    Ini.WriteString('CEP', 'Chave_BuscarCEP', edCEPChaveBuscarCEP.Text);
    Ini.WriteString('CEP', 'Proxy_Host', edCONProxyHost.Text);
    Ini.WriteString('CEP', 'Proxy_Port', edCONProxyPort.Text);
    Ini.WriteString('CEP', 'Proxy_User', edCONProxyUser.Text);
    GravaINICrypt(Ini, 'CEP', 'Proxy_Pass', edCONProxyPass.Text, _C);

    { Parametros do TC }
    Ini.WriteInteger('TC', 'Modelo', cbxTCModelo.ItemIndex);
    Ini.WriteInteger('TC', 'TCP_Porta', StrToIntDef(edTCArqPrecos.Text, 6500));
    Ini.WriteString('TC', 'Arq_Precos', edTCArqPrecos.Text);
    Ini.WriteString('TC', 'Nao_Econtrado', edTCNaoEncontrado.Text);

    { Parametros da Conta de tsEmailDFe }
    Ini.WriteString('EMAIL', 'NomeExibicao', edEmailNome.Text);
    Ini.WriteString('EMAIL', 'Endereco', edEmailEndereco.Text);
    Ini.WriteString('EMAIL', 'Email', edEmailHost.Text);
    GravaINICrypt(Ini, 'EMAIL', 'Usuario', edEmailUsuario.Text, _C);
    GravaINICrypt(Ini, 'EMAIL', 'Senha', edEmailSenha.Text, _C);
    Ini.WriteInteger('EMAIL', 'Porta', edEmailPorta.Value);
    Ini.WriteBool('EMAIL', 'ExigeSSL', cbEmailSsl.Checked);
    Ini.WriteBool('EMAIL', 'ExigeTLS', cbEmailTls.Checked);
    Ini.WriteBool('EMAIL', 'Confirmacao',  cbEmailConfirmation.Checked);
    Ini.WriteBool('EMAIL', 'SegundoPlano', cbEmailThread.Checked);
    Ini.WriteString('EMAIL', 'Codificacao', cbEmailCodificacao.Text);

    { Parametros Sedex }
    Ini.WriteString('SEDEX', 'Contrato', edtSedexContrato.Text);
    GravaINICrypt(Ini, 'SEDEX', 'SenhaSedex', edtSedexSenha.Text, _C);

    { Parametros NCM }
    ini.WriteString('NCM', 'DirNCMSalvar', PathWithoutDelim(deNcmSalvar.Text));

    { Parametros NFe }
    Ini.WriteInteger( 'Certificado','SSLLib' , cbSSLLib.ItemIndex);
    Ini.WriteInteger( 'Certificado','CryptLib', cbCryptLib.ItemIndex);
    Ini.WriteInteger( 'Certificado','HttpLib', cbHttpLib.ItemIndex);
    Ini.WriteInteger( 'Certificado','XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteInteger( 'Certificado','SSLType', cbSSLType.ItemIndex);

    Ini.WriteString('Certificado', 'ArquivoPFX', edtArquivoPFX.Text);
    Ini.WriteString('Certificado', 'NumeroSerie', edtNumeroSerie.Text);
    GravaINICrypt(INI, 'Certificado', 'Senha', edtSenha.Text, _C);
    Ini.WriteBool('Certificado', 'ExibeRazaoSocialCertificado', chkExibeRazaoSocial.Checked);
    Ini.WriteBool('Certificado', 'VerificarValidade', chkVerificarValidadeCertificado.Checked);

    Ini.WriteBool('ACBrNFeMonitor', 'IgnorarComandoModoEmissao', cbModoEmissao.Checked);
    Ini.WriteBool('ACBrNFeMonitor', 'ModoXML', cbModoXML.Checked);
    Ini.WriteBool('ACBrNFeMonitor', 'RetirarAcentos', cbRetirarAcentos.Checked);
    Ini.WriteBool('ACBrNFeMonitor', 'Gravar_Log_Comp', cbLogComp.Checked);
    Ini.WriteString('ACBrNFeMonitor', 'Arquivo_Log_Comp', edLogComp.Text);
    Ini.WriteInteger('ACBrNFeMonitor', 'Linhas_Log_Comp', sedLogLinhasComp.Value);
    Ini.WriteString('ACBrNFeMonitor', 'ArquivoWebServices', edtArquivoWebServicesNFe.Text );
    Ini.WriteString('ACBrNFeMonitor', 'ArquivoWebServicesCTe', edtArquivoWebServicesCTe.Text );
    Ini.WriteString('ACBrNFeMonitor', 'ArquivoWebServicesMDFe', edtArquivoWebServicesMDFe.Text );
    Ini.WriteString('ACBrNFeMonitor', 'ArquivoWebServicesGNRe', edtArquivoWebServicesGNRe.Text );
    Ini.WriteBool('ACBrNFeMonitor', 'ValidarDigest', cbValidarDigest.Checked);
    Ini.WriteInteger('ACBrNFeMonitor', 'TimeoutWebService', edtTimeoutWebServices.Value);

    Ini.WriteInteger('Geral', 'DANFE', rgTipoDanfe.ItemIndex);
    //Ini.WriteInteger('Geral', 'FormaEmissao', rgFormaEmissao.ItemIndex);
    Ini.WriteInteger('WebService', 'FormaEmissaoNFe', cbFormaEmissaoNFe.ItemIndex);
    Ini.WriteInteger('WebService', 'FormaEmissaoCTe', cbFormaEmissaoCTe.ItemIndex);
    Ini.WriteInteger('WebService', 'FormaEmissaoMDFe', cbFormaEmissaoMDFe.ItemIndex);
    Ini.WriteInteger('WebService', 'FormaEmissaoGNRe', cbFormaEmissaoGNRe.ItemIndex);
    Ini.WriteString('Geral', 'LogoMarca', edtLogoMarca.Text);
    Ini.WriteString('Geral', 'LogoMarcaNFCeSAT', edtLogoMarcaNFCeSAT.Text);
    Ini.WriteBool('Geral', 'Salvar', ckSalvar.Checked);
    Ini.WriteString('Geral', 'PathSalvar', edtPathLogs.Text);
    Ini.WriteString('Geral', 'Impressora', cbxImpressora.Text);

    Ini.WriteString('WebService', 'UF', cbUF.Text);
    Ini.WriteInteger('WebService', 'Ambiente', rgTipoAmb.ItemIndex);
    Ini.WriteString('WebService', 'Versao', cbVersaoWS.Text);
    Ini.WriteString('WebService', 'VersaoCTe', cbVersaoWSCTe.Text);
    Ini.WriteString('WebService', 'VersaoMDFe', cbVersaoWSMDFe.Text);
    Ini.WriteBool('WebService', 'AjustarAut', cbxAjustarAut.Checked);
    Ini.WriteString('WebService', 'Aguardar', edtAguardar.Text);
    Ini.WriteString('WebService', 'Tentativas', edtTentativas.Text);
    Ini.WriteString('WebService', 'Intervalo', edtIntervalo.Text);
    Ini.WriteInteger('WebService','TimeZoneMode', cbxTimeZoneMode.ItemIndex);
    Ini.WriteString('WebService','TimeZoneStr', edTimeZoneStr.Caption);

    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    GravaINICrypt(INI, 'Proxy', 'Pass', edtProxySenha.Text, _C);

    Ini.WriteString('NFCe', 'IdToken', edtIdToken.Text);
    Ini.WriteString('NFCe', 'Token', edtToken.Text);
    Ini.WriteBool('NFCe', 'TagQrCode', chbTagQrCode.Checked);
    Ini.WriteBool('NFCe', 'UsarIntegrador', ckNFCeUsarIntegrador.Checked );

    Ini.WriteString('NFe', 'CNPJContador', edtCNPJContador.Text);

    Ini.WriteString('Email', 'AssuntoNFe', edtEmailAssuntoNFe.Text);
    Ini.WriteString('Email', 'MensagemNFe', BinaryStringToString(mmEmailMsgNFe.Lines.Text));
    Ini.WriteString('Email', 'AssuntoCTe', edtEmailAssuntoCTe.Text);
    Ini.WriteString('Email', 'MensagemCTe', BinaryStringToString(mmEmailMsgCTe.Lines.Text) );
    Ini.WriteString('Email', 'AssuntoMDFe', edtEmailAssuntoMDFe.Text);
    Ini.WriteString('Email', 'MensagemMDFe', BinaryStringToString(mmEmailMsgMDFe.Lines.Text) );

    Ini.WriteInteger('DANFE', 'Modelo', rgModeloDanfe.ItemIndex);
    Ini.WriteInteger('DACTE', 'TamanhoPapel', rgTamanhoPapelDacte.ItemIndex);
    Ini.WriteString('DANFE', 'Site', edtSiteEmpresa.Text);
    Ini.WriteString('DANFE', 'Email', edtEmailEmpresa.Text);
    Ini.WriteString('DANFE', 'Fax', edtFaxEmpresa.Text);
    Ini.WriteBool('DANFE', 'ImpDescPorc', cbxImpDescPorc.Checked);
    Ini.WriteBool('DANFE', 'MostrarPreview', cbxMostrarPreview.Checked);
    Ini.WriteInteger('DANFE', 'Copias', edtNumCopia.Value);
    Ini.WriteInteger('DANFE', 'LarguraCodigoProduto', speLargCodProd.Value);
    Ini.WriteInteger('DANFE', 'EspessuraBorda', speEspBorda.Value);
    Ini.WriteInteger('DANFE', 'FonteRazao', speFonteRazao.Value);
    Ini.WriteInteger('DANFE', 'FonteEndereco', speFonteEndereco.Value);
    Ini.WriteInteger('DANFE', 'FonteCampos', speFonteCampos.Value);
    Ini.WriteInteger('DANFE', 'AlturaCampos', speAlturaCampos.Value);
    Ini.WriteFloat('DANFE', 'Margem', fspeMargemInf.Value);
    Ini.WriteFloat('DANFE', 'MargemSup', fspeMargemSup.Value);
    Ini.WriteFloat('DANFE', 'MargemDir', fspeMargemDir.Value);
    Ini.WriteFloat('DANFE', 'MargemEsq', fspeMargemEsq.Value);
    Ini.WriteFloat('DANFCe', 'MargemInf', fspeNFCeMargemInf.Value);
    Ini.WriteFloat('DANFCe', 'MargemSup', fspeNFCeMargemSup.Value);
    Ini.WriteFloat('DANFCe', 'MargemDir', fspeNFCeMargemDir.Value);
    Ini.WriteFloat('DANFCe', 'MargemEsq', fspeNFCeMargemEsq.Value);
    Ini.WriteFloat('DANFCe', 'LarguraBobina', fspeLarguraNFCe.Value);
    Ini.WriteString('DANFE', 'PathPDF', edtPathPDF.Text);
    Ini.WriteInteger('DANFE', 'DecimaisQTD', spedtCasasDecimaisQtd.Value);
    Ini.WriteInteger('DANFE', 'DecimaisValor', spedtDecimaisVUnit.Value);
    Ini.WriteBool('DANFE', 'ExibeResumo', cbxExibeResumo.Checked);
    Ini.WriteBool('DANFE', 'ImprimirTributosItem', cbxImprimirTributos.Checked);
    Ini.WriteBool('DANFE', 'ImprimirValLiq', cbxImpValLiq.Checked);
    Ini.WriteInteger('DANFE', 'UNComercialETributavel', cbxUnComTributavel.ItemIndex);
    Ini.WriteBool('DANFE', 'PreImpresso', cbxFormCont.Checked);
    Ini.WriteBool('DANFE', 'MostrarStatus', cbxMostraStatus.Checked);
    Ini.WriteBool('DANFE', 'ExibirEAN', cbxExibirEAN.Checked);
    Ini.WriteBool('DANFE', 'ExibirCampoFatura', cbxExibirCampoFatura.Checked);
    Ini.WriteBool('DANFE', 'ExpandirLogo', cbxExpandirLogo.Checked);
    Ini.WriteInteger('DANFE', 'Fonte', rgTipoFonte.ItemIndex);
    Ini.WriteInteger('DANFE', 'LocalCanhoto', rgLocalCanhoto.ItemIndex);
    ini.WriteBool('DANFE','QuebrarLinhasDetalheItens', cbxQuebrarLinhasDetalhesItens.Checked) ;
    ini.WriteBool('DANFE','ImprimirDetalhamentoEspecifico',cbxImpDetEspNFe.Checked);
    ini.WriteBool('DANFE','ImprimirDadosDocReferenciados',cbxImpDocsReferenciados.Checked);
    ini.WriteBool('DANFE','ExibirBandInforAdicProduto',cbxExibirInfAdicProduto.Checked);
    ini.WriteBool('DANFE','LogoEmCima',cbxExibirLogoEmCima.Checked);

    Ini.WriteInteger('NFCe', 'Modelo', rgModeloDANFeNFCE.ItemIndex);
    Ini.WriteInteger('NFCe', 'ModoImpressaoEvento', rgModoImpressaoEvento.ItemIndex);
    Ini.WriteBool('NFCe', 'ImprimirItem1Linha', cbxImprimirItem1LinhaNFCe.Checked);
    Ini.WriteBool('NFCe', 'ImprimirDescAcresItem', cbxImprimirDescAcresItemNFCe.Checked);
    Ini.WriteString('NFCe', 'ImpressoraPadrao', cbxImpressoraNFCe.Text);
    Ini.WriteBool('NFCe', 'QRCodeLateral', cbxImprimirQRCodeLateralNFCe.Checked);
    Ini.WriteBool('NFCe', 'UsaCodigoEanImpressao', cbxImprimirCodigoEANNFCe.Checked);
    Ini.WriteBool('NFCe', 'ImprimeNomeFantasia', cbxImprimirNomeFantasiaNFCe.Checked);

    Ini.WriteBool('Arquivos', 'Salvar', cbxSalvarArqs.Checked);
    Ini.WriteBool('Arquivos', 'PastaMensal', cbxPastaMensal.Checked);
    Ini.WriteBool('Arquivos', 'AddLiteral', cbxAdicionaLiteral.Checked);
    Ini.WriteBool('Arquivos', 'EmissaoPathNFe', cbxEmissaoPathNFe.Checked);
    Ini.WriteBool('Arquivos', 'SalvarCCeCanPathEvento',
      cbxSalvaPathEvento.Checked);
    Ini.WriteBool('Arquivos', 'SepararPorCNPJ', cbxSepararPorCNPJ.Checked);
    Ini.WriteBool('Arquivos', 'SepararPorModelo', cbxSepararporModelo.Checked);
    Ini.WriteBool('Arquivos', 'SalvarApenasNFesAutorizadas',
      cbxSalvarNFesProcessadas.Checked);
    Ini.WriteBool('Arquivos', 'AtualizarXMLCancelado',
      cbxAtualizarXMLCancelado.Checked);
    Ini.WriteBool('Arquivos', 'NormatizarMunicipios', cbxNormatizarMunicipios.Checked);
    Ini.WriteBool('Arquivos', 'UsarSeparadorPathPDF', cbxUsarSeparadorPathPDF.Checked);
    Ini.WriteString('Arquivos', 'PathNFe', edtPathNFe.Text);
    Ini.WriteString('Arquivos', 'PathInu', edtPathInu.Text);
    Ini.WriteString('Arquivos', 'PathDPEC', edtPathDPEC.Text);
    Ini.WriteString('Arquivos', 'PathEvento', edtPathEvento.Text);

    {Parametros SAT}
    INI.WriteInteger('SAT','Modelo',cbxModeloSAT.ItemIndex);
    INI.WriteString('SAT','ArqLog',edSATLog.Text);
    INI.WriteString('SAT','NomeDLL',edNomeDLL.Text);
    INI.WriteString('SAT','CodigoAtivacao',edtCodigoAtivacao.Text);
    INI.WriteString('SAT','CodigoUF',edtCodUF.Text);
    INI.WriteInteger('SAT','NumeroCaixa',seNumeroCaixa.Value);
    INI.WriteInteger('SAT','Ambiente',cbxAmbiente.ItemIndex);
    INI.WriteInteger('SAT','PaginaDeCodigo',sePagCod.Value);
    INI.WriteFloat('SAT','versaoDadosEnt',sfeVersaoEnt.Value);
    INI.WriteBool('SAT','FormatarXML', cbxFormatXML.Checked);
    INI.WriteString('SAT','PathCFe',edSATPathArqs.Text);
    INI.WriteBool('SAT','SalvarCFe', cbxSATSalvarCFe.Checked);
    INI.WriteBool('SAT','SalvarCFeCanc', cbxSATSalvarCFeCanc.Checked);
    INI.WriteBool('SAT','SalvarEnvio', cbxSATSalvarEnvio.Checked);
    INI.WriteBool('SAT','SepararPorCNPJ', cbxSATSepararPorCNPJ.Checked);
    INI.WriteBool('SAT','SepararPorMES', cbxSATSepararPorMES.Checked);

    INI.WriteString('SATExtrato','ParamsString',ACBrSATExtratoESCPOS1.PosPrinter.Device.ParamsString);
    INI.WriteBool('SATExtrato', 'ImprimeDescAcrescItem', cbxImprimirDescAcresItemSAT.Checked);
    INI.WriteBool('SATExtrato', 'ImprimeEmUmaLinha', cbxImprimirItem1LinhaSAT.Checked);
    INI.WriteInteger('SATExtrato', 'ImprimeChaveEmUmaLinha', rdgImprimeChave1LinhaSAT.ItemIndex);
    INI.WriteBool('SATExtrato', 'UsaCodigoEanImpressao', cbxImprimirCodEANitemSAT.Checked);

    INI.WriteString('SATEmit','CNPJ',edtEmitCNPJ.Text);
    INI.WriteString('SATEmit','IE',edtEmitIE.Text);
    INI.WriteString('SATEmit','IM',edtEmitIM.Text);
    INI.WriteInteger('SATEmit','RegTributario',cbxRegTributario.ItemIndex);
    INI.WriteInteger('SATEmit','RegTribISSQN',cbxRegTribISSQN.ItemIndex);
    INI.WriteInteger('SATEmit','IndRatISSQN',cbxIndRatISSQN.ItemIndex);

    INI.WriteString('SATSwH','CNPJ',edtSwHCNPJ.Text);
    INI.WriteString('SATSwH','Assinatura',edtSwHAssinatura.Text);

    INI.WriteBool('SATFortes','UsarFortes',cbUsarFortes.Checked) ;
    INI.WriteInteger('SATFortes','Largura',seLargura.Value);
    INI.WriteInteger('SATFortes','MargemTopo',seMargemTopo.Value);
    INI.WriteInteger('SATFortes','MargemFundo',seMargemFundo.Value);
    INI.WriteInteger('SATFortes','MargemEsquerda',seMargemEsquerda.Value);
    INI.WriteInteger('SATFortes','MargemDireita',seMargemDireita.Value);
    INI.WriteBool('SATFortes','Preview',cbPreview.Checked);

    INI.WriteString('SATPrinter','Name',lImpressora.Caption);

    INI.WriteInteger('SATRede','tipoInter',rgRedeTipoInter.ItemIndex);
    INI.WriteInteger('SATRede','tipoLan',rgRedeTipoLan.ItemIndex);
    INI.WriteString('SATRede','SSID',edRedeSSID.Text);
    INI.WriteInteger('SATRede','seg',cbxRedeSeg.ItemIndex);
    INI.WriteString('SATRede','codigo',edRedeCodigo.Text);
    INI.WriteString('SATRede','lanIP',edRedeIP.Text);
    INI.WriteString('SATRede','lanMask',edRedeMask.Text);
    INI.WriteString('SATRede','lanGW',edRedeGW.Text);
    INI.WriteString('SATRede','lanDNS1',edRedeDNS1.Text);
    INI.WriteString('SATRede','lanDNS2',edRedeDNS2.Text);
    INI.WriteString('SATRede','usuario',edRedeUsuario.Text);
    INI.WriteString('SATRede','senha',edRedeSenha.Text);
    INI.WriteInteger('SATRede','proxy',cbxRedeProxy.ItemIndex);
    INI.WriteString('SATRede','proxy_ip',edRedeProxyIP.Text);
    INI.WriteInteger('SATRede','proxy_porta',edRedeProxyPorta.Value);
    INI.WriteString('SATRede','proxy_user',edRedeProxyUser.Text);
    INI.WriteString('SATRede','proxy_senha',edRedeProxySenha.Text);

    INI.WriteString('SATIntegrador','Input',edMFEInput.Text);
    INI.WriteString('SATIntegrador','Output',edMFEOutput.Text);
    INI.WriteInteger('SATIntegrador','Timeout',seMFETimeout.Value);

    {Parâmetros PosPrinter}
    INI.WriteInteger('PosPrinter', 'Modelo', cbxModelo.ItemIndex);
    INI.WriteString('PosPrinter', 'Porta', cbxPorta.Text);
    INI.WriteInteger('PosPrinter', 'Colunas', seColunas.Value);
    INI.WriteInteger('PosPrinter', 'EspacoEntreLinhas', seEspacosLinhas.Value);
    INI.WriteInteger('PosPrinter', 'LinhasBuffer', seBuffer.Value);
    INI.WriteInteger('PosPrinter', 'LinhasPular', seLinhasPular.Value);
    INI.WriteInteger('PosPrinter', 'PaginaDeCodigo', cbxPagCodigo.ItemIndex);
    INI.WriteBool('PosPrinter', 'ControlePorta', cbControlePorta.Checked);
    INI.WriteBool('PosPrinter', 'CortarPapel', cbCortarPapel.Checked);
    INI.WriteBool('PosPrinter', 'TraduzirTags', cbTraduzirTags.Checked);
    INI.WriteBool('PosPrinter', 'IgnorarTags', cbIgnorarTags.Checked);
    INI.WriteString('PosPrinter', 'ArqLog', edPosPrinterLog.Text);
    Ini.WriteString('PosPrinter', 'SerialParams', ACBrPosPrinter1.Device.ParamsString);

    INI.WriteInteger('Barras', 'Largura', seCodBarrasLargura.Value);
    INI.WriteInteger('Barras', 'Altura', seCodBarrasAltura.Value);
    INI.WriteBool('Barras', 'HRI',cbHRI.Checked);

    INI.WriteInteger('QRCode', 'Tipo', seQRCodeTipo.Value);
    INI.WriteInteger('QRCode', 'LarguraModulo', seQRCodeLargMod.Value);
    INI.WriteInteger('QRCode', 'ErrorLevel', seQRCodeErrorLevel.Value);

    INI.WriteBool('Logo', 'Imprimir', cbEscPosImprimirLogo.Checked);
    INI.WriteInteger('Logo', 'KC1', seLogoKC1.Value);
    INI.WriteInteger('Logo', 'KC2', seLogoKC2.Value);
    INI.WriteInteger('Logo', 'FatorX', seLogoFatorX.Value);
    INI.WriteInteger('Logo', 'FatorY', seLogoFatorY.Value);

  finally
    Ini.Free;
  end;

  AddLinesLog('Configuração geral gravada com sucesso');

  SalvarConfBoletos;
  SalvarSW;

  if chRFD.Checked then
  begin
    with ACBrRFD1 do
    begin
      if Ativo then
      begin
        CONT_RazaoSocial := edUSURazaoSocial.Text;
        CONT_CNPJ := edUSUCNPJ.Text;
        CONT_Endereco := edUSUEndereco.Text;
        CONT_IE := edUSUIE.Text;
        CONT_NumUsuario := seUSUNumeroCadastro.Value;
        CONT_DataHoraCadastro := deUSUDataCadastro.Date;
        try
          CONT_DataHoraCadastro :=
            CONT_DataHoraCadastro + StrToTime(meUSUHoraCadastro.Text, ':');
        except
        end;
        CONT_CROCadastro := seUSUCROCadastro.Value;
        CONT_GTCadastro := seUSUGTCadastro.Value;
        ECF_RFDID := lRFDID.Caption;
        ECF_DataHoraSwBasico := deRFDDataSwBasico.Date;
        try
          ECF_DataHoraSwBasico :=
            ECF_DataHoraSwBasico + StrToTime(meRFDHoraSwBasico.Text, ':');
        except
        end;

        GravarINI;

        AddLinesLog('Dados do RFD gravados com sucesso');
      end;
    end;
  end;

  if (OldMonitoraTXT <> rbTXT.Checked) or (OldMonitoraTCP <> rbTCP.Checked) or
    (OldMonitoraPasta <> cbMonitorarPasta.Checked) then
  begin
    MessageDlg('ACBrMonitor PLUS',
      'Configurações de inicialização do ACBrMonitorPLUS foram modificadas' +
      sLineBreak + sLineBreak + 'Será necessário reinicar o ACBrMonitorPLUS',
      mtInformation, [mbOK], 0);
    Application.Terminate;
  end;
end;

procedure TFrmACBrMonitor.SalvarConfBoletos;
var
  Ini: TIniFile;
  TrimedCNPJ, TrimedCEP: string;
begin
  TrimedCNPJ := OnlyNumber(edtBOLCNPJ.Text);
  TrimedCEP := OnlyNumber(edtBOLCEP.Text);

  if pConfig.Visible and (TrimedCNPJ <> '') then
  begin
    with ACBrValidador1 do
    begin
      if cbxBOLF_J.ItemIndex = 0 then
        TipoDocto := docCPF
      else
        TipoDocto := docCNPJ;

      Documento := edtBOLCNPJ.Text;
      try
        Validar;    // Dispara Exception se Documento estiver errado
      except
        btnBoletoClick(btnBoleto);  //pgConfig.ActivePage := tsACBrBoleto;
        btnBoletoCedenteClick(btnBoletoCedente); //pgBoleto.ActivePage := tsCedente;
        edtBOLCNPJ.SetFocus;
        raise;
      end;

      edtBOLCNPJ.Text := Formatar;
    end;
  end;

  Ini := TIniFile.Create(ACBrMonitorINI);
  try
    {Parametros do Boleto - Cliente}
    ini.WriteString('BOLETO', 'Cedente.Nome', edtBOLRazaoSocial.Text);
    ini.WriteString('BOLETO', 'Cedente.CNPJCPF',
      ifthen(TrimedCNPJ = '', '', edtBOLCNPJ.Text));
    ini.WriteString('BOLETO', 'Cedente.Logradouro', edtBOLLogradouro.Text);
    ini.WriteString('BOLETO', 'Cedente.Numero', edtBOLNumero.Text);
    ini.WriteString('BOLETO', 'Cedente.Bairro', edtBOLBairro.Text);
    ini.WriteString('BOLETO', 'Cedente.Cidade', edtBOLCidade.Text);
    ini.WriteString('BOLETO', 'Cedente.CEP', ifthen(TrimedCEP = '', '', edtBOLCEP.Text));
    ini.WriteString('BOLETO', 'Cedente.Complemento', edtBOLComplemento.Text);
    ini.WriteString('BOLETO', 'Cedente.UF', cbxBOLUF.Text);
    ini.WriteInteger('BOLETO', 'Cedente.RespEmis', cbxBOLEmissao.ItemIndex);
    ini.WriteInteger('BOLETO', 'Cedente.Pessoa', cbxBOLF_J.ItemIndex);
    ini.WriteString('BOLETO', 'Cedente.CodTransmissao', edtCodTransmissao.Text);
    ini.WriteString('BOLETO', 'Cedente.Modalidade', edtModalidade.Text);
    ini.WriteString('BOLETO', 'Cedente.Convenio', edtConvenio.Text);

    {Parametros do Boleto - Banco}
    Ini.WriteInteger('BOLETO', 'Banco', max(cbxBOLBanco.ItemIndex, 0));
    ini.WriteString('BOLETO', 'Conta', edtBOLConta.Text);
    ini.WriteString('BOLETO', 'DigitoConta', edtBOLDigitoConta.Text);
    ini.WriteString('BOLETO', 'Agencia', edtBOLAgencia.Text);
    ini.WriteString('BOLETO', 'DigitoAgencia', edtBOLDigitoAgencia.Text);
    ini.WriteString('BOLETO', 'CodCedente', edtCodCliente.Text);
    ini.WriteString('BOLETO', 'LocalPagamento', edtBOLLocalPagamento.Text);

    {Parametros do Boleto - Boleto}
    ini.WriteString('BOLETO', 'DirLogos', PathWithoutDelim(deBOLDirLogo.Text));
//    ini.WriteString('BOLETO', 'SoftwareHouse', edtBOLSH.Text);
    ini.WriteInteger('BOLETO', 'Copias', spBOLCopias.Value);
    Ini.WriteBool('BOLETO', 'Preview', ckgBOLMostrar.Checked[0]);
    ini.WriteBool('BOLETO', 'Progresso', ckgBOLMostrar.Checked[1]);
    ini.WriteBool('BOLETO', 'Setup', ckgBOLMostrar.Checked[2]);
    ini.WriteInteger('BOLETO', 'Layout', cbxBOLLayout.ItemIndex);
    ini.WriteInteger('BOLETO', 'Filtro', cbxBOLFiltro.ItemIndex);
    ini.WriteString('BOLETO', 'DirArquivoBoleto', PathWithoutDelim(
      deBOLDirArquivo.Text));
    ini.WriteString('BOLETO', 'DirArquivoRemessa', PathWithoutDelim(
      deBolDirRemessa.Text));
    ini.WriteString('BOLETO', 'DirArquivoRetorno', PathWithoutDelim(
      deBolDirRetorno.Text));
    ini.WriteInteger('BOLETO', 'CNAB', cbxCNAB.ItemIndex);
    Ini.WriteBool('BOLETO','LerCedenteRetorno', chkLerCedenteRetorno.Checked);
    Ini.WriteBool('BOLETO','MostraPreviewRelRetorno',chkBOLRelMostraPreview.Checked);
    Ini.WriteString('BOLETO', 'LogoEmpresa', edtBOLLogoEmpresa.Text);

    {Parametros do Boleto - E-mail}
    ini.WriteString('BOLETO', 'EmailAssuntoBoleto', edtBOLEmailAssunto.Text);
    ini.WriteString('BOLETO', 'EmailMensagemBoleto', BinaryStringToString(edtBOLEmailMensagem.Lines.Text));
    {Parametros do Boleto - Impressora}
    Ini.WriteString('BOLETO', 'Impressora', cbxBOLImpressora.Text);
  finally
    ini.Free;
  end;

  AddLinesLog('Configuração de Boletos gravada com sucesso');
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.SalvarSW;
var
  INI: TIniFile;
  Pass, Chave: ansistring;
begin
  with ACBrRFD1 do
  begin
    SH_CNPJ := edSH_CNPJ.Text;
    SH_RazaoSocial := edSH_RazaoSocial.Text;
    SH_COO := edSH_COO.Text;
    SH_IE := edSH_IE.Text;
    SH_IM := edSH_IM.Text;
    SH_NomeAplicativo := edSH_Aplicativo.Text;
    SH_NumeroAplicativo := edSH_NumeroAP.Text;
    SH_VersaoAplicativo := edSH_VersaoAP.Text;
    SH_Linha1 := edSH_Linha1.Text;
    SH_Linha2 := edSH_Linha2.Text;
  end;

  try
    Chave := '';
    ACBrEAD1GetChavePrivada(Chave);
  except
  end;

  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'swh.ini');
  try
    GravaINICrypt(INI, 'SWH', 'CNPJ', ACBrRFD1.SH_CNPJ, IntToStrZero(fsHashSenha, 8));
    Pass := IntToStrZero(StringCrc16(ACBrRFD1.SH_CNPJ +
      IntToStrZero(fsHashSenha, 8)), 8);

    GravaINICrypt(INI, 'SWH', 'Verifica', 'ARQUIVO SWH.INI ESTA OK', Pass);
    GravaINICrypt(INI, 'SWH', 'RazaoSocial', ACBrRFD1.SH_RazaoSocial, Pass);
    GravaINICrypt(INI, 'SWH', 'COO', ACBrRFD1.SH_COO, Pass);
    GravaINICrypt(INI, 'SWH', 'IE', ACBrRFD1.SH_IE, Pass);
    GravaINICrypt(INI, 'SWH', 'IM', ACBrRFD1.SH_IM, Pass);
    GravaINICrypt(INI, 'SWH', 'Aplicativo', ACBrRFD1.SH_NomeAplicativo, Pass);
    GravaINICrypt(INI, 'SWH', 'NumeroAplicativo', ACBrRFD1.SH_NumeroAplicativo, Pass);
    GravaINICrypt(INI, 'SWH', 'VersaoAplicativo', ACBrRFD1.SH_VersaoAplicativo, Pass);
    GravaINICrypt(INI, 'SWH', 'Linha1', ACBrRFD1.SH_Linha1, Pass);
    GravaINICrypt(INI, 'SWH', 'Linha2', ACBrRFD1.SH_Linha2, Pass);
    GravaINICrypt(INI, 'SWH', 'Site', edSH_Site.Text, Pass);

    if copy(mRSAKey.Text, 1, 5) = '-----' then
      GravaINICrypt(INI, 'SWH', 'RSA', Trim(mRSAKey.Text), Pass)

    else
    begin
      if (Chave = '') and chRFD.Checked then
      begin
        btnCadastroClick(btnCadastro); //pgConfig.ActivePage := tsCadastro;
        btnSHClick(btnSH); // pgCadastro.ActivePage := tsCadSwH;
        pgSwHouse.ActivePage := tsCadSwChaveRSA;

        raise Exception.Create('Para trabalhar com RFD é necessário ' +
          'definir uma Chave Privada');
      end;
    end;

  finally
    Ini.Free;
  end;

  AddLinesLog('Dados da Sw.House gravados com sucesso');
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.EscondeConfig;
begin
  PanelScroll.Visible := False;
  pConfig.Visible := False;
  bConfig.Caption := '&Configurar';
  bConfig.Glyph := nil;
  ImageList1.GetBitmap(11, bConfig.Glyph);
  bCancelar.Visible := False;
  btMinimizar.Visible := True;
  Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.ExibeConfig;
var
  Senha: ansistring;
  SenhaOk: boolean;
  HashSenha: integer;
begin
  SenhaOk := (fsHashSenha < 1);
  if not SenhaOk then
  begin
    Senha := '';
    if InputQuery('Configuração', 'Digite a Senha de Configuração', True, Senha) then
    begin
      Senha := Trim(Senha);
      HashSenha := StringCrc16(Senha);
      SenhaOk := (HashSenha = fsHashSenha);
    end;
  end;

  if not SenhaOk then
    raise Exception.Create('Senha [' + Senha + '] inválida');

  fsCNPJSWOK := False;
  PanelScroll.Visible := True;
  pConfig.Visible := True;
  bConfig.Caption := '&Salvar';
  bConfig.Glyph := nil;
  ImageList1.GetBitmap(12, bConfig.Glyph);
  bCancelar.Visible := True;
  btMinimizar.Visible := False;
//  pgConfig.ActivePageIndex := 0;
  LimparResp;

  Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.Processar;
var
  Linha, Objeto: String;
begin
  if NewLines <> '' then
    fsProcessar.Add(NewLines);

  NewLines := '';
  fsInicioLoteTXT := True;

  // Apagando linhas em branco no final do arquivo, pois isso atrapalha a detectção do final de arquivo //
  if fsProcessar.Count > 0 then
  begin
    Linha := Trim(fsProcessar[fsProcessar.Count - 1]);
    while Linha = '' do
    begin
      fsProcessar.Delete(fsProcessar.Count - 1);
      Linha := Trim(fsProcessar[fsProcessar.Count - 1]);
    end;
  end;

  while fsProcessar.Count > 0 do
  begin
    // Atualiza Memo de Entrada //
    mCmd.Lines.Assign(fsProcessar);
    Application.ProcessMessages;

    { Objeto BOLETO/NFE pode receber comandos com várias Linhas,
      portanto deve processar todas linhas de uma só vez... }
    Objeto := TrimLeft(fsProcessar[0]);
    if Copy(Objeto, 1, 3) = UTF8BOM then
      Objeto := copy(Objeto, 4, Length(Objeto) );

    if (UpperCase(Copy(Objeto, 1, 6)) = 'BOLETO') or
       (UpperCase(Copy(Objeto, 1, 5)) = 'EMAIL')  or
       (UpperCase(Copy(Objeto, 1, 3)) = 'NFE')  or
       (UpperCase(Copy(Objeto, 1, 3)) = 'SAT') or
       (UpperCase(Copy(Objeto, 1, 4)) = 'MDFE') or
       (UpperCase(Copy(Objeto, 1, 4)) = 'GNRE') or
       (UpperCase(Copy(Objeto, 1, 3)) = 'CTE')then
    begin
      Linha := Trim(fsProcessar.Text);
      if Copy(Linha, 1, 3) = UTF8BOM then
        Linha := copy(Linha, 4, Length(Linha) );

      fsProcessar.Clear;
    end
    else
    begin
      Linha := Objeto;
      fsProcessar.Delete(0);
    end;

    if Linha <> '' then
    begin
      StatusBar1.Panels[2].Text := Linha;

      try
        if pos('.', Linha) = 0 then              { Comandos do ACBrMonitor }
          Linha := 'ACBR.' + Linha;

        { Interpretanto o Comando }
        fsCmd.Comando := Linha;

        //Log Comando
        AddLinesLog(Linha);

        if fsCmd.Objeto = 'ACBR' then
          DoACBr(fsCmd)
        else if fsCmd.Objeto = 'ECF' then
          DoECF(fsCmd)
        else if fsCmd.Objeto = 'GAV' then
          DoGAV(fsCmd)
        else if fsCmd.Objeto = 'CHQ' then
          DoCHQ(fsCmd)
        else if fsCmd.Objeto = 'DIS' then
          DoDIS(fsCmd)
        else if fsCmd.Objeto = 'LCB' then
          DoLCB(fsCmd)
        else if fsCmd.Objeto = 'BAL' then
          DoBAL(fsCmd)
        else if fsCmd.Objeto = 'ETQ' then
          DoETQ(fsCmd)
        else if fsCmd.Objeto = 'BOLETO' then
          DoBoleto(fsCmd)
        else if fsCmd.Objeto = 'CEP' then
          DoCEP(fsCmd)
        else if fsCmd.Objeto = 'IBGE' then
          DoIBGE(fsCmd)
        else if fsCmd.Objeto = 'EMAIL' then
          DoEmail(fsCmd)
        else if fsCmd.Objeto = 'SEDEX' then
          DoSedex(fsCmd)
        else if fsCmd.Objeto = 'NCM' then
          DoNcm(fsCmd)
        else if fsCmd.Objeto = 'NFE' then
          DoACBrNFe(fsCmd)
        else if fsCmd.Objeto = 'CTE' then
          DoACBrCTe(fsCmd)
        else if fsCmd.Objeto = 'MDFE' then
          DoACBrMDFe(fsCmd)
        else if fsCmd.Objeto = 'GNRE' then
          DoACBrGNRe(fsCmd)
        else if fsCmd.Objeto = 'SAT' then
          DoSAT(fsCmd)
        else if fsCmd.Objeto = 'ESCPOS' then
          DoPosPrinter(fsCmd);

        // Atualiza Memo de Entrada //
        mCmd.Lines.Assign(fsProcessar);

        Resposta(Linha, 'OK: ' + fsCmd.Resposta);
        Application.ProcessMessages;

      except
        on E: Exception do
          Resposta(Linha, 'ERRO: ' + E.Message);
      end;

      StatusBar1.Panels[2].Text := '';
    end;

    fsInicioLoteTXT := False;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.Resposta(Comando, Resposta: ansistring);
begin
  if cbRetirarAcentosNaResposta.Checked then
    Resposta:= TiraAcentos(Resposta);

  if rbTCP.Checked then
  begin
    if Assigned(Conexao) then
    begin
      if chbTCPANSI.Checked then
        Resposta := ACBrUTF8ToAnsi(Resposta);

      Resposta := StringReplace(Resposta, chr(3), '', [rfReplaceAll]);
      Conexao.SendString(Resposta);
      Conexao.SendByte(3);
    end;
  end;

  if rbTXT.Checked then
  begin
    { Primeiro salva em Temporário para que a gravação de todos os Bytes ocorra
      antes que a aplicação controladora do ACBrMonitor tente ler o arquivo de
      Resposta incompleto }
    if fsInicioLoteTXT or (TipoCMD <> 'A') then
      TryDeleteFile(ArqSaiTMP, 1000); // Tenta apagar por até 1 segundo

    if FileExists(ArqSaiTXT) then
      RenameFile(ArqSaiTXT, ArqSaiTMP); { GravaArqResp faz append se arq. existir }

    if TipoCMD = 'A' then     // ACBr
    begin
      if chbArqSaiANSI.Checked then
        Resposta := Utf8ToAnsi(Resposta);

      AddLinesLogFile(ArqSaiTMP, Resposta);

      if (fsProcessar.Count < 1) then    // É final do Lote TXT ?
        RenameFile(ArqSaiTMP, ArqSaiTXT);
    end

    else if TipoCMD = 'B' then          // Bematech Monitor
    begin
      if copy(Resposta, 1, 3) <> 'OK:' then
      begin
        AddLinesLogFile(ExtractFilePath(ArqSaiTMP)+'STATUS.TXT', '0,0,0');
      end
      else
      begin
        AddLinesLogFile(ExtractFilePath(ArqSaiTMP) + 'STATUS.TXT', '6,0,0');
        Resposta := StringReplace(Resposta, 'OK: ', '', [rfReplaceAll]);
        Resposta := StringReplace(Resposta, '/', '', [rfReplaceAll]);
        Resposta := StringReplace(Resposta, ':', '', [rfReplaceAll]);
        AddLinesLogFile(ArqSaiTMP, Resposta);
        RenameFile(ArqSaiTMP, ArqSaiTXT);
      end;
    end

    else if TipoCMD = 'D' then      // Daruma Monitor
    begin
      if copy(Resposta, 1, 3) <> 'OK:' then
      begin
        AddLinesLogFile(ExtractFilePath(ArqSaiTMP) + 'DARUMA.RET', '-27;006;000;000');
      end
      else
      begin
        Resposta := StringReplace(Resposta, 'OK: ', '', [rfReplaceAll]);
        Resposta := StringReplace(Resposta, '/', '', [rfReplaceAll]);
        Resposta := StringReplace(Resposta, ':', '', [rfReplaceAll]);
        Resposta := '001;006;000;000;' + Resposta;
        AddLinesLogFile(ArqSaiTMP, Resposta);
        RenameFile(ArqSaiTMP, ExtractFilePath(ArqSaiTMP) + 'DARUMA.RET');
      end;
    end;

  end;

  pTopRespostas.Caption := 'Respostas Enviadas (' + IntToStr(mResp.Lines.Count) +
    ' linhas)';

  AddLinesLog(Resposta);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.btMinimizarClick(Sender: TObject);
begin
  Ocultar1Click(Sender);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bCancelarClick(Sender: TObject);
begin
  EscondeConfig;
  DesInicializar;
  Inicializar;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bConfigClick(Sender: TObject);
begin
  if pConfig.Visible then
  begin
    SalvarIni;
    EscondeConfig;

    DesInicializar;  { Re-Inicializa, para as alteraçoes fazerem efeito }
    Inicializar;
  end
  else
    ExibeConfig;

  fsRFDLeuParams := False;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.rbTCPTXTClick(Sender: TObject);
begin
  gbTCP.Enabled := rbTCP.Checked;
  gbTXT.Enabled := rbTXT.Checked;
  cbMonitorarPasta.Enabled := rbTXT.Checked;

  if rbTXT.Checked then
  begin
    if edENTTXT.Text = '' then
      edENTTXT.Text := 'ENT.TXT';

    if edSAITXT.Text = '' then
      edSAITXT.Text := 'SAI.TXT';
  end
  else
  begin
    if edPortaTCP.Text = '' then
      edPortaTCP.Text := '3434';

    if edTimeOutTCP.Text = '' then
      edTimeOutTCP.Text := '10000';
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbSenhaClick(Sender: TObject);
begin
  gbSenha.Enabled := cbSenha.Checked;
  if not cbSenha.Checked then
  begin
    fsHashSenha := -1;
    edSenha.Text := '';
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbLogClick(Sender: TObject);
begin
  gbLog.Enabled := cbLog.Checked;

  if cbLog.Checked and (edLogArq.Text = '') then
    edLogArq.Text := 'LOG.TXT';
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sbLogClick(Sender: TObject);
begin
  PathClick(edLogArq);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.edOnlyNumbers(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #13, #8]) then
    Key := #0;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.ACBrECF1MsgAguarde(Mensagem: string);
begin
  StatusBar1.Panels[1].Text :=
    StringReplace(Mensagem, sLineBreak, ' ', [rfReplaceAll]);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.ACBrECF1MsgPoucoPapel(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := 'ATENÇAO. Pouco papel';
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.DoACBrTimer(Sender: TObject);
var
  MS: TMemoryStream;
  Linhas: TStringList;
  S: ansistring;
  RetFind: integer;
  SearchRec: TSearchRec;
  NomeArqEnt, NomeArqSai: string;
begin
  Timer1.Enabled := False;

  if Inicio then
  begin
    Inicializar;
    if FileExists(ACBrMonitorINI) then
       Application.Minimize;

    exit;
  end;

  try
    try
      if MonitorarPasta then
      begin
        NomeArqEnt := PathWithDelim(ExtractFileDir(ArqEntOrig)) + '*.*';
        RetFind := SysUtils.FindFirst(NomeArqEnt, faAnyFile, SearchRec);
        if (RetFind = 0) then
        begin
          if SearchRec.Name = '.' then
            FindNext(SearchRec);
          if SearchRec.Name = '..' then
            FindNext(SearchRec);

          ArqEntTXT := PathWithDelim(ExtractFileDir(ArqEntOrig)) + SearchRec.Name;
          { Arquivo de Requisicao }
          NomeArqEnt := StringReplace(ExtractFileName(ArqEntTXT),
            ExtractFileExt(ArqEntTXT), '', [rfReplaceAll]);
          NomeArqEnt := NomeArqEnt + '-resp' + ExtractFileExt(ArqEntTXT);
          ArqSaiTXT := PathWithDelim(ExtractFilePath(ArqSaiOrig)) + NomeArqEnt;
          ArqSaiTMP := ChangeFileExt(ArqSaiTXT, '.tmp');
        end;
      end
      else
      begin
        NomeArqEnt := PathWithDelim(ExtractFileDir(ArqEntOrig)) +
          StringReplace(ExtractFileName(ArqEntOrig), ExtractFileExt(ArqEntOrig),
          '', [rfReplaceAll]) + '*' + ExtractFileExt(ArqEntOrig);
        RetFind := SysUtils.FindFirst(NomeArqEnt, faAnyFile, SearchRec);
        if (RetFind = 0) then
        begin
          NomeArqEnt := StringReplace(ExtractFileName(ArqEntOrig),
            ExtractFileExt(ArqEntOrig), '', [rfReplaceAll]);
          NomeArqSai := StringReplace(ExtractFileName(ArqSaiOrig),
            ExtractFileExt(ArqSaiOrig), '', [rfReplaceAll]);
          ArqEntTXT := PathWithDelim(ExtractFileDir(ArqEntOrig)) + SearchRec.Name;
          { Arquivo de Requisicao }
          ArqSaiTXT := PathWithDelim(ExtractFilePath(ArqSaiOrig)) + StringReplace(
            ExtractFileName(LowerCase(ArqEntTXT)), LowerCase(NomeArqEnt), LowerCase(
            NomeArqSai), [rfReplaceAll]);
          ArqSaiTMP := ChangeFileExt(ArqSaiTXT, '.tmp');
        end;
      end;
    finally
      SysUtils.FindClose(SearchRec);
    end;

    if FileExists(ArqEntTXT) then  { Existe arquivo para ler ? }
      try
        Linhas := TStringList.Create;

        TipoCMD := 'A';
        if (UpperCase(ExtractFileName(ArqEntTXT)) = 'BEMAFI32.CMD') then
          TipoCMD := 'B'
        else if (UpperCase(ExtractFileName(ArqEntTXT)) = 'DARUMA.CMD') then
          TipoCMD := 'D';

        { Lendo em MemoryStream temporário para nao apagar comandos nao processados }
        MS := TMemoryStream.Create;
        try
          MS.LoadFromFile(ArqEntTXT);
          MS.Position := 0;
          SetLength(S, MS.Size);
          MS.ReadBuffer(PChar(S)^, MS.Size);
          if chbArqEntANSI.Checked then
            S := ACBrAnsiToUTF8(S);
          Linhas.Text := S;
        finally
          MS.Free;
        end;

        TryDeleteFile(ArqEntTXT, 1000); // Tenta apagar por até 1 segundo

        if TipoCMD = 'B' then
          Linhas.Text := TraduzBemafi(Linhas.Text)
        else if TipoCMD = 'D' then
          Linhas.Text := TraduzObserver(Linhas.Text);

        fsProcessar.AddStrings(Linhas);
      finally
        Linhas.Free;
      end;

    Processar;
  finally
    Timer1.Enabled := True;
  end;
end;

{---------------------------------- ACBrECF -----------------------------------}
{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.tsECFShow(Sender: TObject);
begin
  AvaliaEstadoTsECF;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbECFModeloChange(Sender: TObject);
begin
  try
    if ACBrECF1.Ativo then
      bECFAtivar.Click;

    ACBrECF1.Modelo := TACBrECFModelo(Max(cbECFModelo.ItemIndex - 1, 0))
  finally
    if cbECFModelo.Text <> 'Procurar' then
      cbECFModelo.ItemIndex := integer(ACBrECF1.Modelo) + 1;
    cbECFPorta.Text := ACBrECF1.Porta;
  end;

  AvaliaEstadoTsECF;
end;

procedure TFrmACBrMonitor.AvaliaEstadoTsECF;
begin
  bECFAtivar.Enabled :=
    ((ACBrECF1.Modelo <> ecfNenhum) or (cbECFModelo.Text = 'Procurar'));

  cbECFPorta.Enabled := bECFAtivar.Enabled;
  sedECFTimeout.Enabled := bECFAtivar.Enabled;
  sedECFIntervalo.Enabled := bECFAtivar.Enabled;
  tsECFParamI.Enabled := bECFAtivar.Enabled;
  tsECFParamII.Enabled := bECFAtivar.Enabled;

  bECFTestar.Enabled := ACBrECF1.Ativo;
  bECFLeituraX.Enabled := ACBrECF1.Ativo;

  bECFAtivar.Glyph := nil;
  if ACBrECF1.Ativo then
  begin
    bECFAtivar.Caption := '&Desativar';
    ImageList1.GetBitmap(6, bECFAtivar.Glyph);
  end
  else
  begin
    bECFAtivar.Caption := '&Ativar';
    ImageList1.GetBitmap(5, bECFAtivar.Glyph);
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbECFPortaChange(Sender: TObject);
begin
  try
    if ACBrECF1.Ativo then
      bECFAtivar.Click;

    ACBrECF1.Porta := cbECFPorta.Text;
  finally
    cbECFPorta.Text := ACBrECF1.Porta;
  end;
end;


{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sbECFSerialClick(Sender: TObject);
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    if ACBrECF1.Ativo then
      bECFAtivar.Click;

    frConfiguraSerial.Device.Porta := ACBrECF1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := cbECFPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrECF1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbECFPorta.Text := frConfiguraSerial.Device.Porta;
      ACBrECF1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
    AvaliaEstadoTsECF;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sedECFTimeoutChanged(Sender: TObject);
begin
  ACBrECF1.TimeOut := sedECFTimeout.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sedECFIntervaloChanged(Sender: TObject);
begin
  ACBrECF1.IntervaloAposComando := sedECFIntervalo.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.chECFArredondaPorQtdClick(Sender: TObject);
begin
  ACBrECF1.ArredondaPorQtd := chECFArredondaPorQtd.Checked;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.chECFDescrGrandeClick(Sender: TObject);
begin
  ACBrECF1.DescricaoGrande := chECFDescrGrande.Checked;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.chECFSinalGavetaInvertidoClick(Sender: TObject);
begin
  ACBrECF1.GavetaSinalInvertido := chECFSinalGavetaInvertido.Checked;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.edECFLogChange(Sender: TObject);
begin
  ACBrECF1.ArqLOG := edECFLog.Text;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sbECFLogClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(Application.ExeName) + edECFLog.Text);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bECFAtivarClick(Sender: TObject);
begin
  if bECFAtivar.Caption = '&Ativar' then
  begin
    Self.Enabled := False;

    try
      if cbECFModelo.ItemIndex = 0 then
        if not ACBrECF1.AcharECF(True, False) then
        begin
          MessageDlg('Nenhum ECF encontrado.', mtInformation, [mbOK], 0);
          exit;
        end;

      if chRFD.Checked then
      begin
        with ACBrRFD1 do
        begin
          DirRFD := edRFDDir.Text;
          IgnoraEcfMfd := chRFDIgnoraMFD.Checked;
        end;
      end;

      ACBrECF1.Ativar;
    finally
      Self.Enabled := True;

      cbECFModelo.ItemIndex := integer(ACBrECF1.Modelo) + 1;
      cbECFPorta.Text := ACBrECF1.Porta;
    end;
  end
  else
    ACBrECF1.Desativar;

  AvaliaEstadoTsECF;
  AvaliaEstadoTsRFD;
end;

procedure TFrmACBrMonitor.meUSUHoraCadastroExit(Sender: TObject);
begin
  try
    StrToTime(meUSUHoraCadastro.Text, ':');
  except
    AddLinesLog('Hora Inválida');
    meUSUHoraCadastro.SetFocus;
  end;
end;

procedure TFrmACBrMonitor.meRFDHoraSwBasicoExit(Sender: TObject);
begin
  try
    StrToTime(meRFDHoraSwBasico.Text, ':');
  except
    AddLinesLog('Hora Inválida');
    meRFDHoraSwBasico.SetFocus;
  end;
end;

procedure TFrmACBrMonitor.pgBoletoChange(Sender: TObject);
begin
  if pgBoleto.ActivePage = tsContaBancaria then
     MostraLogoBanco;

  if pgBoleto.ActivePage = tsRelatorio then
     if lsvArqsRetorno.Items.Count = 0 then
        CarregaArquivosRetorno;
end;

procedure TFrmACBrMonitor.pgConfigChange(Sender: TObject);
begin
  HelptabSheet;
end;

procedure TFrmACBrMonitor.rgRedeTipoInterClick(Sender: TObject);
begin
  gbWiFi.Visible := (rgRedeTipoInter.ItemIndex = 1);
end;

procedure TFrmACBrMonitor.rgRedeTipoLanClick(Sender: TObject);
begin
  gbPPPoE.Visible := (rgRedeTipoLan.ItemIndex = 1);
  gbIPFix.Visible := (rgRedeTipoLan.ItemIndex = 2);
end;

procedure TFrmACBrMonitor.SbArqLog2Click(Sender: TObject);
begin
  PathClick(edSATPathArqs);
end;

procedure TFrmACBrMonitor.SbArqLogClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(Application.ExeName) + edSATLog.Text);
end;

procedure TFrmACBrMonitor.sbArquivoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter :=
    'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoPFX.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbArquivoWebServicesCTeClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServicesCTe.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbArquivoWebServicesMDFeClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServicesMDFe.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbArquivoWebServicesGNReClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServicesGNRe.Text := OpenDialog1.FileName;
  end;
end;
procedure TFrmACBrMonitor.sbArquivoWebServicesNFeClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServicesNFe.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbBALSerialClick(Sender: TObject);
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    if ACBrBAL1.Ativo then
      bBALAtivar.Click;

    frConfiguraSerial.Device.Porta := ACBrBAL1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := cbBALPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrBAL1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbBALPorta.Text := frConfiguraSerial.Device.Porta;
      ACBrBAL1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
    AvaliaEstadoTsBAL;
  end;

end;

procedure TFrmACBrMonitor.sbLogoMarca1Click(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.png';
  OpenDialog1.Filter :=
    'Arquivos PNG (*.png)|*.png|Arquivos JPG (*.jpg)|*.jpg|Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtBOLLogoEmpresa.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbLogoMarcaNFCeSATClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.png';
  OpenDialog1.Filter :=
    'Arquivos PNG (*.png)|*.png|Arquivos JPG (*.jpg)|*.jpg|Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtLogoMarcaNFCeSAT.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbNumeroSerieCertClick(Sender: TObject);
var
  I: Integer;
begin
  //OldSSL := ACBrNFe1.Configuracoes.Geral.SSLLib;
  //
  //ACBrNFe1.Configuracoes.Geral.SSLLib  := libCapicom;
  //edtNumeroSerie.Text := ACBrNFe1.SSL.SelecionarCertificado;
  //
  //ACBrNFe1.Configuracoes.Geral.SSLLib := OldSSL;

  frSelecionarCertificado := TfrSelecionarCertificado.Create(Self);
  try
    ACBrNFe1.SSL.LerCertificadosStore;

    For I := 0 to ACBrNFe1.SSL.ListaCertificados.Count-1 do
    begin
      with ACBrNFe1.SSL.ListaCertificados[I] do
      begin
        if (CNPJ <> '')  then
        begin
          with frSelecionarCertificado.StringGrid1 do
          begin
            RowCount := RowCount + 1;
            Cells[ 0, RowCount-1] := NumeroSerie;
            Cells[ 1, RowCount-1] := RazaoSocial;
            Cells[ 2, RowCount-1] := CNPJ;
            Cells[ 3, RowCount-1] := FormatDateBr(DataVenc);
            Cells[ 4, RowCount-1] := Certificadora;
          end;
        end;
      end;
    end;

    frSelecionarCertificado.ShowModal;

    if frSelecionarCertificado.ModalResult = mrOK then
      edtNumeroSerie.Text := frSelecionarCertificado.StringGrid1.Cells[ 0,
        frSelecionarCertificado.StringGrid1.Row];

  finally
     frSelecionarCertificado.Free;
  end;

end;

procedure TFrmACBrMonitor.sbBALLogClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(Application.ExeName) + edBALLog.Text);
end;

procedure TFrmACBrMonitor.sbLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.png';
  OpenDialog1.Filter :=
    'Arquivos PNG (*.png)|*.png|Arquivos JPG (*.jpg)|*.jpg|Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtLogoMarca.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbPathDPECClick(Sender: TObject);
begin
  PathClick(edtPathDPEC);
end;

procedure TFrmACBrMonitor.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TFrmACBrMonitor.sbPathInuClick(Sender: TObject);
begin
  PathClick(edtPathInu);
end;

procedure TFrmACBrMonitor.sbPathNFeClick(Sender: TObject);
begin
  PathClick(edtPathNFe);
end;

procedure TFrmACBrMonitor.sbPathPDFClick(Sender: TObject);
begin
  PathClick(edtPathPDF);
end;

procedure TFrmACBrMonitor.sbPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TFrmACBrMonitor.sbPosPrinterLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim, edPosPrinterLog.Text) = 0 then
     AFileLog := ExtractFilePath(Application.ExeName) + edPosPrinterLog.Text
  else
     AFileLog := edPosPrinterLog.Text;

  OpenURL(AFileLog);
end;

procedure TFrmACBrMonitor.sbSobreClick(Sender: TObject);
begin
  frmSobre := TfrmSobre.Create(self);
  try
    frmSobre.lVersao.Caption := 'Ver: ' + sVersaoACBr;
    frmSobre.ShowModal;
  finally
    FreeAndNil(frmSobre);
  end;
end;

procedure TFrmACBrMonitor.ScrollBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position+30;
  Application.ProcessMessages;
end;

procedure TFrmACBrMonitor.ScrollBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position-30;
  Application.ProcessMessages;
end;

procedure TFrmACBrMonitor.sedECFLinhasEntreCuponsChange(Sender: TObject);
begin
  ACBrECF1.LinhasEntreCupons := sedECFLinhasEntreCupons.Value;
end;

procedure TFrmACBrMonitor.sedECFMaxLinhasBufferChange(Sender: TObject);
begin
  ACBrECF1.MaxLinhasBuffer := sedECFMaxLinhasBuffer.Value;
end;

procedure TFrmACBrMonitor.sedECFPaginaCodigoChange(Sender: TObject);
begin
  ACBrECF1.PaginaDeCodigo := sedECFPaginaCodigo.Value;
end;

procedure TFrmACBrMonitor.sePagCodChange(Sender: TObject);
begin
  ACBrSAT1.Config.PaginaDeCodigo := sePagCod.Value;
  cbxUTF8.Checked := ACBrSAT1.Config.EhUTF8;
end;

procedure TFrmACBrMonitor.sfeVersaoEntChange(Sender: TObject);
begin
  ACBrSAT1.Config.infCFe_versaoDadosEnt := sfeVersaoEnt.Value;
end;

procedure TFrmACBrMonitor.SpeedButton1Click(Sender: TObject);
begin
  PathClick(edLogComp);
end;

procedure TFrmACBrMonitor.TcpServerConecta(const TCPBlockSocket: TTCPBlockSocket;
  var Enviar: ansistring);
var
  Resp: string;
begin

  Conexao := TCPBlockSocket;
  mCmd.Lines.Clear;
  fsProcessar.Clear;
  Resp := 'ACBrMonitor/ACBrNFeMonitor PLUS Ver. ' + sVersaoACBr + sLineBreak + 'Conectado em: ' +
    FormatDateTime('dd/mm/yy hh:nn:ss', now) + sLineBreak + 'Maquina: ' +
    Conexao.GetRemoteSinIP + sLineBreak + 'Esperando por comandos.';

  Resposta('', Resp);
end;

procedure TFrmACBrMonitor.TcpServerDesConecta(const TCPBlockSocket: TTCPBlockSocket;
  Erro: integer; ErroDesc: string);
var
  Resp: string;
begin
  if not Assigned( TCPBlockSocket ) then
    Exit;

  Conexao := TCPBlockSocket;
  Resp := 'ALERTA: Fim da Conexão com: ' + Conexao.GetRemoteSinIP +
    ' em: ' + FormatDateTime('dd/mm/yy hh:nn:ss', now);

  AddLinesLog(Resp);
end;

procedure TFrmACBrMonitor.TcpServerRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
  const Recebido: ansistring; var Enviar: ansistring);
var
  S: AnsiString;
begin
  Conexao := TCPBlockSocket;
  { Le o que foi enviado atravez da conexao TCP }
  if chbTCPANSI.Checked then
    S := ACBrAnsiToUTF8(Recebido)
  else
    S := Recebido;

  fsProcessar.Add(S);
  Processar;
end;

procedure TFrmACBrMonitor.TCPServerTCConecta(const TCPBlockSocket: TTCPBlockSocket;
  var Enviar: ansistring);
var
  IP, Id: ansistring;
  Indice: integer;
begin
  TCPBlockSocket.SendString('#ok');

  Id := Trim(TCPBlockSocket.RecvPacket(1000));
  IP := TCPBlockSocket.GetRemoteSinIP;
  Indice := mTCConexoes.Lines.IndexOf(IP);
  if Indice < 0 then
  begin
    mTCConexoes.Lines.Add(IP);
    AddLinesLog('T.C. Inicio Conexão IP: [' + IP + '] ID: [' + Id +
      ']' + ' em: ' + FormatDateTime('dd/mm/yy hh:nn:ss', now));
  end;
end;

procedure TFrmACBrMonitor.TCPServerTCDesConecta(const TCPBlockSocket: TTCPBlockSocket;
  Erro: integer; ErroDesc: string);
var
  IP: string;
  Indice: integer;
begin
  if not Assigned( TCPBlockSocket ) then
     Exit;

  IP := TCPBlockSocket.GetRemoteSinIP;
  AddLinesLog('T.C. Fim Conexão IP: [' + IP + '] em: ' +
    FormatDateTime('dd/mm/yy hh:nn:ss', now));

  Indice := mTCConexoes.Lines.IndexOf(IP);
  if Indice >= 0 then
    mTCConexoes.Lines.Delete(Indice);
end;

procedure TFrmACBrMonitor.TCPServerTCRecebeDados(const TCPBlockSocket: TTCPBlockSocket;
  const Recebido: ansistring; var Enviar: ansistring);
var
  Comando, Linha: ansistring;
  Indice, P1, P2: integer;
begin
  { Le o que foi enviado atravez da conexao TCP }
  Comando := StringReplace(Trim(Recebido), #0, '', [rfReplaceAll]);  // Remove nulos

  if pos('#live', Comando) > 0 then
  begin
    Comando := StringReplace(Comando, '#live', '', [rfReplaceAll]); // Remove #live
    TCPBlockSocket.Tag := 0;                      // Zera falhas de #live?
  end;

  if Comando = '' then
    exit;

  AddLinesLog('TC: [' + TCPBlockSocket.GetRemoteSinIP + '] RX: <- [' + Comando + ']');

  if copy(Comando, 1, 1) = '#' then
  begin
    Comando := copy(Comando, 2, Length(Comando));
    P1 := 0;
    P2 := 0;
    Indice := fsSLPrecos.IndexOfName(Comando);
    if Indice >= 0 then
    begin
      Linha := fsSLPrecos[Indice];
      P1 := Pos('|', Linha);
      P2 := PosAt('|', Linha, 3);
    end
    else
      Linha := edTCNaoEncontrado.Text;

    if P2 = 0 then
      P2 := Length(Linha) + 1;

    Enviar := '#' + copy(Linha, P1 + 1, P2 - P1 - 1);
    Enviar := LeftStr(Enviar, 45);

    TCPBlockSocket.Tag := 0;  // Zera falhas de #live?
    AddLinesLog('     TX: -> [' + Enviar + ']');
  end;
end;

procedure TFrmACBrMonitor.TrayIcon1Click(Sender: TObject);
begin
  TrayIcon1.PopUpMenu.PopUp;
end;

procedure TFrmACBrMonitor.tsACBrBoletoShow(Sender: TObject);
begin
  pgBoleto.ActivePageIndex := 0;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bECFTestarClick(Sender: TObject);
begin
  ACBrECF1.TestarDialog;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bECFLeituraXClick(Sender: TObject);
var
  wAtivo: boolean;
begin
  wAtivo := ACBrECF1.Ativo;

  try
    ACBrECF1.Ativar;
    ACBrECF1.LeituraX;
  finally
    ACBrECF1.Ativo := wAtivo;
  end;
end;

{------------------------------------ ACBrCHQ ---------------------------------}
procedure TFrmACBrMonitor.cbCHQPortaChange(Sender: TObject);
begin
  if ACBrCHQ1.Modelo <> chqImpressoraECF then
  begin
    try
      ACBrCHQ1.Desativar;
      ACBrCHQ1.Porta := cbCHQPorta.Text;
    finally
      cbCHQPorta.Text := ACBrCHQ1.Porta;
    end;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.edCHQFavorecidoChange(Sender: TObject);
begin
  ACBrCHQ1.Favorecido := edCHQFavorecido.Text;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.edCHQCidadeChange(Sender: TObject);
begin
  ACBrCHQ1.Cidade := edCHQCidade.Text;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bCHQTestarClick(Sender: TObject);
var
  wAtivo: boolean;
begin
  wAtivo := ACBrCHQ1.Ativo;

  try
   {  ACBrCHQ1.Ativar ;
     ACBrCHQ1.Banco     := '001' ;
     ACBrCHQ1.Cidade    := IfThen(edCHQCidade.Text='',
                                    'Nome da sua Cidade',edCHQCidade.Text) ;
     ACBrCHQ1.Favorecido:= IfThen(edCHQFavorecido.Text='',
                                     'Nome do Favorecido', edCHQFavorecido.Text) ;
     ACBrCHQ1.Observacao:= 'Texto de Observacao' ;
     ACBrCHQ1.Valor     := 123456.12 ;
     ACBrCHQ1.ImprimirCheque ;}
  finally
    ACBrCHQ1.Ativo := wAtivo;
  end;
end;

{------------------------------------ ACBrGAV ---------------------------------}
{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.tsGAVShow(Sender: TObject);
begin
  AvaliaEstadoTsGAV;
end;

procedure TFrmACBrMonitor.bGAVAtivarClick(Sender: TObject);
begin
  if bGAVAtivar.Caption = '&Ativar' then
    ACBrGAV1.Ativar
  else
    ACBrGAV1.Desativar;

  AvaliaEstadoTsGAV;
end;

procedure TFrmACBrMonitor.cbGAVPortaChange(Sender: TObject);
begin
  if ACBrGAV1.Modelo <> gavImpressoraECF then
  begin
    try
      ACBrGAV1.Desativar;
      ACBrGAV1.Porta := cbGAVPorta.Text;
    finally
      cbGAVPorta.Text := ACBrGAV1.Porta;
    end;
  end;

  AvaliaEstadoTsGAV;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbGAVStrAbreChange(Sender: TObject);
begin
  ACBrGAV1.StrComando := cbGAVStrAbre.Text;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sedGAVIntervaloAberturaChanged(Sender: TObject);
begin
  ACBrGAV1.AberturaIntervalo := sedGAVIntervaloAbertura.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbGAVAcaoAberturaAntecipadaChange(Sender: TObject);
begin
  ACBrGAV1.AberturaAntecipada :=
    TACBrGAVAberturaAntecipada(cbGAVAcaoAberturaAntecipada.ItemIndex);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bGAVEstadoClick(Sender: TObject);
begin
  if not ACBrGAV1.Ativo then
    ACBrGAV1.Ativar;

  if ACBrGAV1.GavetaAberta then
    lGAVEstado.Caption := 'Aberta'
  else
    lGAVEstado.Caption := 'Fechada';
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bGAVAbrirClick(Sender: TObject);
begin
  try
    tsGAV.Enabled := False;
    lGAVEstado.Caption := 'AGUARDE';

    ACBrGAV1.AbreGaveta;
  finally
    tsGAV.Enabled := True;
    bGAVEstado.Click;
  end;
end;

{------------------------------------ ACBrDIS ---------------------------------}
procedure TFrmACBrMonitor.cbDISPortaChange(Sender: TObject);
begin
  try
    ACBrDIS1.Desativar;
    ACBrDIS1.Porta := cbDISPorta.Text;
  finally
    cbDISPorta.Text := ACBrDIS1.Porta;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.seDISIntervaloChanged(Sender: TObject);
begin
  ACBrDIS1.Intervalo := seDISIntervalo.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.seDISPassosChanged(Sender: TObject);
begin
  ACBrDIS1.Passos := seDISPassos.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.seDISIntByteChanged(Sender: TObject);
begin
  ACBrDIS1.IntervaloEnvioBytes := seDISIntByte.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bDISLimparClick(Sender: TObject);
begin
  ACBrDIS1.LimparDisplay;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bDISTestarClick(Sender: TObject);
begin
  ACBrDIS1.Ativar;
  ACBrDIS1.ExibirLinha(1, 'Projeto ACBr');
  ACBrDIS1.ExibirLinha(2, 'http://acbr.sf.net');
end;

procedure TFrmACBrMonitor.bDISAnimarClick(Sender: TObject);
begin
  ACBrDIS1.Ativar;
  ACBrDIS1.LimparDisplay;
  ACBrDIS1.ExibirLinha(1, PadCenter('Projeto ACBr', ACBrDIS1.Colunas)
    , efeDireita_Esquerda);
  ACBrDIS1.ExibirLinha(2, PadCenter('http://acbr.sf.net', ACBrDIS1.Colunas)
    , efeEsquerda_Direita);
end;

{------------------------------------ ACBrLCB ---------------------------------}
{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.tsLCBShow(Sender: TObject);
begin
  AvaliaEstadoTsLCB;
end;

procedure TFrmACBrMonitor.cbLCBPortaChange(Sender: TObject);
begin
  try
    ACBrLCB1.Desativar;
    ACBrLCB1.Porta := cbLCBPorta.Text;
  finally
    cbLCBPorta.Text := ACBrLCB1.Porta;
  end;

  AvaliaEstadoTsLCB;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bLCBSerialClick(Sender: TObject);
begin
  ACBrLCB1.Desativar;
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    frConfiguraSerial.Device.Porta := ACBrLCB1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := cbLCBPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrLCB1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbLCBPorta.Text := frConfiguraSerial.Device.Porta;
      ACBrLCB1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
    AvaliaEstadoTsLCB;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sedLCBIntervaloChanged(Sender: TObject);
begin
  ACBrLCB1.Intervalo := sedLCBIntervalo.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.rbLCBTecladoClick(Sender: TObject);
begin
  cbLCBSufixo.Enabled := rbLCBTeclado.Checked;
  cbLCBDispositivo.Enabled := rbLCBTeclado.Checked;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bLCBAtivarClick(Sender: TObject);
begin
  sedLCBIntervalo.Value := ACBrLCB1.Intervalo;
  if bLCBAtivar.Caption = '&Ativar' then
    ACBrLCB1.Ativar
  else
    ACBrLCB1.Desativar;

  AvaliaEstadoTsLCB;
end;

procedure TFrmACBrMonitor.AvaliaEstadoTsLCB;
begin
  bLCBAtivar.Enabled := ((cbLCBPorta.Text <> 'Sem Leitor') and
    (cbLCBPorta.ItemIndex > 0));
  cbLCBSufixo.Enabled := bLCBAtivar.Enabled;
  cbLCBSufixoLeitor.Enabled := bLCBAtivar.Enabled;
  cbLCBDispositivo.Enabled := bLCBAtivar.Enabled;
  edLCBPreExcluir.Enabled := bLCBAtivar.Enabled;
  chLCBExcluirSufixo.Enabled := bLCBAtivar.Enabled;
  sedLCBIntervalo.Enabled := bLCBAtivar.Enabled;
  bLCBSerial.Enabled := bLCBAtivar.Enabled;
  rbLCBTeclado.Enabled := bLCBAtivar.Enabled;
  rbLCBFila.Enabled := bLCBAtivar.Enabled;

  rbLCBTecladoClick(Self);

  bLCBAtivar.Glyph := nil;
  if ACBrLCB1.Ativo then
  begin
    bLCBAtivar.Caption := '&Desativar';
    shpLCB.Brush.Color := clLime;
    ImageList1.GetBitmap(6, bLCBAtivar.Glyph);
  end
  else
  begin
    bLCBAtivar.Caption := '&Ativar';
    shpLCB.Brush.Color := clRed;
    ImageList1.GetBitmap(5, bLCBAtivar.Glyph);
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbLCBSufixoLeitorChange(Sender: TObject);
begin
  ACBrLCB1.Sufixo := cbLCBSufixoLeitor.Text;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.edLCBSufLeituraKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', '#', ',', #13, #8]) then
    Key := #0;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.chLCBExcluirSufixoClick(Sender: TObject);
begin
  ACBrLCB1.ExcluirSufixo := chLCBExcluirSufixo.Checked;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.edLCBPreExcluirChange(Sender: TObject);
begin
  ACBrLCB1.PrefixoAExcluir := edLCBPreExcluir.Text;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.AumentaTempoHint(Sender: TObject);
begin
  Application.HintHidePause := 15000;
end;

procedure TFrmACBrMonitor.DiminuiTempoHint(Sender: TObject);
begin
  Application.HintHidePause := 5000;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.ACBrLCB1LeCodigo(Sender: TObject);
var
  Codigo: ansistring;
    {$IFDEF LINUX}
  fd, I: integer;
  C: char;
    {$ENDIF}
begin
  lLCBCodigoLido.Caption := Converte(ACBrLCB1.UltimaLeitura);

  AddLinesLog('LCB -> ' + ACBrLCB1.UltimoCodigo);

  if rbLCBTeclado.Checked then
  begin
    Codigo := ACBrLCB1.UltimoCodigo;
    if Codigo = '' then
      exit;

     {$IFDEF MSWINDOWS}
    Codigo := Codigo + Trim(cbLCBSufixo.Text);
    SendKeys(PChar(Codigo), True);
     {$ENDIF}

    { Alguem sabe como enviar as teclas para o Buffer do KDE ??? }
     {$IFDEF LINUX}
    Codigo := Codigo + TraduzComando(cbLCBSufixo.Text);
    fd := FileOpen(Trim(cbLCBDispositivo.Text), O_WRONLY + O_NONBLOCK);
    if fd < 0 then
      Writeln('Erro ao abrir o dispositivo: ' + Trim(cbLCBDispositivo.Text))
    else
      try
        for I := 1 to length(Codigo) do
        begin
          C := Codigo[I];
          FpIOCtl(fd, TIOCSTI, @C);
        end;
      finally
        FileClose(fd);
      end;

     {$ENDIF}
  end;
end;


{---------------------------------- ACBrRFD -----------------------------------}
{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.tsRFDShow(Sender: TObject);
begin
  pgConRFD.ActivePageIndex := 0;

  AvaliaEstadoTsRFD;

  mRFDINI.Lines.Clear;
  fsRFDIni := '';
end;

procedure TFrmACBrMonitor.AvaliaEstadoTsRFD;
var
  MM: string;
  I: integer;
  SL: TStringList;
  Ini: TIniFile;
begin
  bRFDMF.Enabled := ACBrECF1.Ativo;
  edRFDDir.Enabled := not bRFDMF.Enabled;
  cbRFDModelo.Enabled := bRFDMF.Enabled;

  tsRFDINI.Enabled := ACBrECF1.Ativo and ACBrRFD1.Ativo;

  lRFDID.Caption := ACBrRFD1.ECF_RFDID;
  deRFDDataSwBasico.Date := ACBrRFD1.ECF_DataHoraSwBasico;
  deRFDDataSwBasico.Enabled := tsRFDINI.Enabled;
  meRFDHoraSwBasico.Text := FormatDateTime('hh:nn', ACBrRFD1.ECF_DataHoraSwBasico);
  meRFDHoraSwBasico.Enabled := tsRFDINI.Enabled;

  if ACBrECF1.Ativo then
    gbRFDECF.Hint := 'Selecione o Modelo do ECF'
  else
    pgConRFD.Hint := 'Para Configurar o RFD é necessário ativar o ECF e' +
      sLineBreak + 'Selecionar a opção para Geração de RFD';

  if ACBrECF1.Ativo and ACBrRFD1.Ativo then
  begin
    if (copy(lRFDID.Caption, 1, Length(ACBrECF1.RFDID)) <> ACBrECF1.RFDID) or
      (cbRFDModelo.Items.Count = 0) then
    begin
      if copy(lRFDID.Caption, 1, Length(ACBrECF1.RFDID)) <> ACBrECF1.RFDID then
        lRFDID.Caption := ACBrECF1.RFDID;

      MM := ACBrRFD1.AchaRFDID(lRFDID.Caption);
      lRFDMarca.Caption := Trim(copy(MM, 1, pos('|', MM + '|') - 1));

      SL := TStringList.Create;
      Ini := TIniFile.Create(ACBrRFD1.ArqRFDID);
      try
        Ini.ReadSectionValues('Modelos', SL);

        cbRFDModelo.Items.Clear;
        for I := 0 to SL.Count - 1 do
        begin
          if copy(SL[I], 1, 2) = copy(lRFDID.Caption, 1, 2) then
            cbRFDModelo.Items.Add(SL[I]);

          if copy(SL[I], 1, 3) = lRFDID.Caption then
            cbRFDModelo.Text := SL[I];
        end;
      finally
        SL.Free;
        Ini.Free;
      end;

      ACBrRFD1.ECF_RFDID := lRFDID.Caption;

      if not fsRFDLeuParams then
      begin
        edUSURazaoSocial.Text := ACBrRFD1.CONT_RazaoSocial;
        edUSUEndereco.Text := ACBrRFD1.CONT_Endereco;
        edUSUCNPJ.Text := ACBrRFD1.CONT_CNPJ;
        edUSUIE.Text := ACBrRFD1.CONT_IE;
        seUSUNumeroCadastro.Value := ACBrRFD1.CONT_NumUsuario;
        deUSUDataCadastro.Date := ACBrRFD1.CONT_DataHoraCadastro;
        seUSUCROCadastro.Value := ACBrRFD1.CONT_CROCadastro;
        meUSUHoraCadastro.Text :=
          FormatDateTime('hh:nn', ACBrRFD1.CONT_DataHoraCadastro);
        seUSUGTCadastro.Value := ACBrRFD1.CONT_GTCadastro;

        fsRFDLeuParams := True;
      end;
    end;
  end;
end;

procedure TFrmACBrMonitor.sbDirRFDClick(Sender: TObject);
begin
  OpenURL(ACBrRFD1.DirRFD);
end;

procedure TFrmACBrMonitor.bRFDMFClick(Sender: TObject);
var
  SL: TStringList;
begin
  if not ACBrECF1.Ativo then
    raise Exception.Create('ECF não está ativo');

  SL := TStringList.Create;
  try
    SL.Clear;
    ACBrECF1.LeituraMemoriaFiscalSerial(now, now, SL);

    AddLinesLog(SL);
  finally
    SL.Free;
  end;
end;

procedure TFrmACBrMonitor.cbRFDModeloChange(Sender: TObject);
begin
  lRFDID.Caption := copy(cbRFDModelo.Text, 1, 3);
end;


procedure TFrmACBrMonitor.seUSUGTCadastroKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [',', '.'] then
    Key := DefaultFormatSettings.DecimalSeparator;

  if not (Key in ['0'..'9', #13, #8, DefaultFormatSettings.DecimalSeparator]) then
    Key := #0;
end;

procedure TFrmACBrMonitor.seUSUGTCadastroExit(Sender: TObject);
begin
  ACBrRFD1.CONT_GTCadastro :=
    StrToFloatDef(seUSUGTCadastro.Text, ACBrRFD1.CONT_GTCadastro);
  seUSUGTCadastro.Text := FormatFloat('0.00', ACBrRFD1.CONT_GTCadastro);
end;

{------------------------------ Aba Chave RSA --------------------------------}
procedure TFrmACBrMonitor.tsRFDRSAShow(Sender: TObject);
begin
  if mRSAKey.Text = '' then
    mRSAKey.Text := LerChaveSWH;
end;

procedure TFrmACBrMonitor.bRSALoadKeyClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivos KEY|*.key|Arquivos PEM|*.pem|Todos Arquivos|*.*';

  if OpenDialog1.Execute then
    mRSAKey.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TFrmACBrMonitor.ACBrRFD1GetKeyRSA(var PrivateKey_RSA: string);
begin
  PrivateKey_RSA := LerChaveSWH;
end;

procedure TFrmACBrMonitor.bRSAPrivKeyClick(Sender: TObject);
var
  ChavePublica, ChavePrivada: ansistring;
begin
  if ((mRSAKey.Text <> '') and (mRSAKey.Text <>
    'ATENÇÃO: Chave RSA Privada NÃO pode ser lida no arquivo "swh.ini".')) then
    raise Exception.Create('Você já possui uma chave RSA.');

  ChavePrivada := '';
  ChavePublica := '';

  ACBrEAD1.GerarChaves(ChavePublica, ChavePrivada);

  mRSAKey.Lines.Text := StringReplace(ChavePrivada, #10, sLineBreak, [rfReplaceAll]);

(*
  try
     { Executando o "openssl.exe"
       Sintaxe de comandos extraidas de:  http://www.madboa.com/geek/openssl/ }

    RunCommand('openssl', 'genrsa -out id.rsa 1024', True, 0);

    { Lendo a resposta }
    try
      mRSAKey.Clear;
      mRSAKey.Lines.LoadFromFile('id.rsa');
    except
      raise Exception.Create('Erro ao gerar Chave Privada, usando o "openssl"');
    end;
  finally
    DeleteFile('id.rsa');  // Removendo a chave privada do disco ;
  end;
*)
end;

procedure TFrmACBrMonitor.bRSAPubKeyClick(Sender: TObject);
var
  //SL: TStringList;
  Chave, NomeArq: ansistring;
begin
  AddLinesLog('Calculando Chave Pública através da Chave Privada');
  Chave := ACBrEAD1.CalcularChavePublica;
  Chave := StringReplace(Chave, #10, sLineBreak, [rfReplaceAll]);
  NomeArq := ExtractFilePath(Application.ExeName) + 'pub_key.pem';

  AddLinesLogFile(NomeArq, Chave, False, False);
  AddLinesLog(Chave);
  AddLinesLog('Chave pública gravada no arquivo: ' + sLineBreak + NomeArq);

(*
  ChDir(ExtractFilePath(Application.ExeName));
  SL := TStringList.Create;
  try
    { Gravando a chave RSA temporariamente no DirLog }
    mRSAKey.Lines.SaveToFile('id.rsa');

     { Executando o "openssl.exe"
       Sintaxe de comandos extraidas de:  http://www.madboa.com/geek/openssl/ }

    RunCommand('openssl', 'rsa -in id.rsa -pubout -out rsakey.pub', True, 0);

    { Lendo a resposta }
    try
      SL.Clear;
      SL.LoadFromFile('rsakey.pub');

      AddLinesLogStrings(SL);
      AddLinesLog('');
      AddLinesLog('Chave pública gravada no arquivo: "rsakey.pub"');
    except
      raise Exception.Create('Erro ao gerar Chave Publica, usando o "openssl"');
    end;
  finally
    DeleteFile('id.rsa');  // Removendo a chave privada do disco ;
  end;
*)
end;

procedure TFrmACBrMonitor.edSH_AplicativoChange(Sender: TObject);
begin
  ACBrRFD1.SH_NomeAplicativo := edSH_Aplicativo.Text;
end;

procedure TFrmACBrMonitor.edSH_NumeroAPChange(Sender: TObject);
begin
  ACBrRFD1.SH_NumeroAplicativo := edSH_NumeroAP.Text;
end;

{------------------------------ Aba Arquivos  --------------------------------}
procedure TFrmACBrMonitor.tsRFDINIShow(Sender: TObject);
begin
  if fsRFDIni = '' then
    mRFDINI.Clear;
end;

procedure TFrmACBrMonitor.bRFDINILerClick(Sender: TObject);
begin
  if fsRFDLeuParams then   { Pode ter modificações pendentes da Aba Usuário }
    ACBrRFD1.GravarINI;

  mRFDINI.Lines.LoadFromFile(ACBrRFD1.ArqINI);
  fsRFDIni := 'acbrrfd';
end;

procedure TFrmACBrMonitor.bRFDIDClick(Sender: TObject);
begin
  mRFDINI.Lines.LoadFromFile(ACBrRFD1.ArqRFDID);
  fsRFDIni := 'rfdid';
end;

procedure TFrmACBrMonitor.bRFDINISalvarClick(Sender: TObject);
begin
  if fsRFDIni = '' then
    exit;

  if fsRFDIni = 'acbrrfd' then
  begin
    try
      mRFDINI.Lines.SaveToFile(ACBrRFD1.ArqINI);
      ACBrRFD1.Desativar;        { Desativa e Ativa para re-ler ACBrRFD.ini }
    finally
      ACBrRFD1.Ativar;
    end;
  end
  else
    mRFDINI.Lines.SaveToFile(ACBrRFD1.ArqRFDID);
end;



{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbCHQModeloChange(Sender: TObject);
begin
  try
    ACBrCHQ1.Desativar;
    ACBrCHQ1.Modelo := TACBrCHQModelo(cbCHQModelo.ItemIndex);

    if ACBrCHQ1.Modelo = chqImpressoraECF then
      ACBrCHQ1.ECF := ACBrECF1;
  finally
    cbCHQModelo.ItemIndex := integer(ACBrCHQ1.Modelo);
    cbCHQPorta.Text := ACBrCHQ1.Porta;
  end;

  bCHQTestar.Enabled := (ACBrCHQ1.Modelo <> chqNenhuma);
  cbCHQPorta.Enabled := bCHQTestar.Enabled and (ACBrCHQ1.Modelo <> chqImpressoraECF);
  chCHQVerForm.Enabled := bCHQTestar.Enabled;
  gbCHQDados.Enabled := bCHQTestar.Enabled;
  edCHQBemafiINI.Enabled := bCHQTestar.Enabled;
  sbCHQBemafiINI.Enabled := bCHQTestar.Enabled;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbGAVModeloChange(Sender: TObject);
begin
  try
    ACBrGAV1.Desativar;
    ACBrGAV1.Modelo := TACBrGAVModelo(cbGAVModelo.ItemIndex);

    if ACBrGAV1.Modelo = gavImpressoraECF then
      ACBrGAV1.ECF := ACBrECF1;
  finally
    cbGAVModelo.ItemIndex := integer(ACBrGAV1.Modelo);
    cbGAVPorta.Text := ACBrGAV1.Porta;
    sedGAVIntervaloAbertura.Value := ACBrGAV1.AberturaIntervalo;
  end;

  AvaliaEstadoTsGAV;
end;

procedure TFrmACBrMonitor.AvaliaEstadoTsGAV;
begin
  bGAVAtivar.Enabled := (ACBrGAV1.Modelo <> gavNenhuma);
  bGAVEstado.Enabled := ACBrGAV1.Ativo;
  bGAVAbrir.Enabled := bGAVEstado.Enabled;
  cbGAVPorta.Enabled := not (ACBrGAV1.Modelo in [gavImpressoraECF, gavNenhuma]);
  cbGAVStrAbre.Enabled := (ACBrGAV1.Modelo = gavImpressoraComum);
  sedGAVIntervaloAbertura.Enabled := bGAVAtivar.Enabled;
  cbGAVAcaoAberturaAntecipada.Enabled := bGAVAtivar.Enabled;

  bGAVAtivar.Glyph := nil;
  if ACBrGAV1.Ativo then
  begin
    bGAVAtivar.Caption := '&Desativar';
    ImageList1.GetBitmap(6, bGAVAtivar.Glyph);
  end
  else
  begin
    bGAVAtivar.Caption := '&Ativar';
    ImageList1.GetBitmap(5, bGAVAtivar.Glyph);
  end;
end;


{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbDISModeloChange(Sender: TObject);
begin
  try
    ACBrDIS1.Desativar;
    ACBrDIS1.Modelo := TACBrDISModelo(cbDISModelo.ItemIndex);
  finally
    cbDISModelo.ItemIndex := integer(ACBrDIS1.Modelo);
    cbDISPorta.Text := ACBrDIS1.Porta;
  end;

  bDISTestar.Enabled := (ACBrDIS1.Modelo <> disNenhum);
  bDISLimpar.Enabled := bDISTestar.Enabled;
  bDISAnimar.Enabled := bDISTestar.Enabled;
  seDISPassos.Enabled := bDISTestar.Enabled;
  seDISIntervalo.Enabled := bDISTestar.Enabled;
  cbDISPorta.Enabled := bDISTestar.Enabled and
    (not (ACBrDIS1.Modelo in [disGertecTeclado, disKeytecTeclado]));
  seDISIntByte.Enabled := bDISTestar.Enabled and (not cbDISPorta.Enabled);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.FormShortCut(Key: integer; Shift: TShiftState;
  var Handled: boolean);
begin
  if (Key = VK_HELP) or (Key = VK_F1) then
    sbSobre.Click;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bExecECFTesteClick(Sender: TObject);
var
  Arquivo: string;
  OldAtivo: boolean;
begin
  OldAtivo := ACBrECF1.Ativo;
  try
    ACBrECF1.Desativar;
    {$IFDEF LINUX}
    Arquivo := 'ECFTeste';
    {$ELSE}
    Arquivo := 'ECFTeste.exe';
    {$ENDIF}

    Arquivo := ExtractFilePath(Application.ExeName) + Arquivo;

    if not FileExists(Arquivo) then
      MessageDlg('Programa: "' + Arquivo + '" não encontrado.', mtError, [mbOK], 0)
    else
      RunCommand(Arquivo, '', True);
  finally
    ACBrECF1.Ativo := OldAtivo;
  end;

  AvaliaEstadoTsECF;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sbCHQBemafiINIClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivos ini|*.ini|Arquivos INI|*.INI';
  OpenDialog1.FileName := edCHQBemafiINI.Text;

  if OpenDialog1.Execute then
  begin
    edCHQBemafiINI.Text := OpenDialog1.FileName;
    ACBrCHQ1.ArquivoBemaFiINI := edCHQBemafiINI.Text;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.edCHQBemafiINIExit(Sender: TObject);
begin
  ACBrCHQ1.ArquivoBemaFiINI := edCHQBemafiINI.Text;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.ACBrECF1AguardandoRespostaChange(Sender: TObject);
begin
  { ECF sendo usado junto LCB, deve desabilitar o LCB enquando o ECF estiver
    ocupado imprimindo, para evitar de enviar códigos na hora indevida, como
    por exemplo, quando o EDIT / GET do Campos código não está com o FOCO }
  if ACBrLCB1.Ativo then
    if ACBrECF1.AguardandoResposta then
      ACBrLCB1.Intervalo := 0
    else
      ACBrLCB1.Intervalo := sedLCBIntervalo.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.SetDisWorking(const Value: boolean);
begin
  if ACBrLCB1.Ativo then
    if Value then
      ACBrLCB1.Intervalo := 0
    else
      ACBrLCB1.Intervalo := sedLCBIntervalo.Value;

  fsDisWorking := Value;
end;

procedure TFrmACBrMonitor.LeDadosRedeSAT;
begin
  with ACBrSAT1.Rede do
  begin
    rgRedeTipoInter.ItemIndex := Integer(tipoInter);
    edRedeSSID.Text           := SSID ;
    cbxRedeSeg.ItemIndex      := Integer(seg) ;
    edRedeCodigo.Text         := codigo ;
    rgRedeTipoLan.ItemIndex   := Integer(tipoLan);
    edRedeIP.Text             := lanIP;
    edRedeMask.Text           := lanMask;
    edRedeGW.Text             := lanGW;
    edRedeDNS1.Text           := lanDNS1;
    edRedeDNS2.Text           := lanDNS2;
    edRedeUsuario.Text        := usuario;
    edRedeSenha.Text          := senha;
    cbxRedeProxy.ItemIndex    := proxy;
    edRedeProxyIP.Text        := proxy_ip;
    edRedeProxyPorta.Value    := proxy_porta;
    edRedeProxyUser.Text      := proxy_user;
    edRedeProxySenha.Text     := proxy_senha;
  end;
end;

procedure TFrmACBrMonitor.ConfiguraRedeSAT;
begin
  with ACBrSAT1.Rede do
  begin
    tipoInter   := TTipoInterface( rgRedeTipoInter.ItemIndex );
    SSID        := edRedeSSID.Text ;
    seg         := TSegSemFio( cbxRedeSeg.ItemIndex ) ;
    codigo      := edRedeCodigo.Text ;
    tipoLan     := TTipoLan( rgRedeTipoLan.ItemIndex ) ;
    lanIP       := edRedeIP.Text ;
    lanMask     := edRedeMask.Text ;
    lanGW       := edRedeGW.Text ;
    lanDNS1     := edRedeDNS1.Text ;
    lanDNS2     := edRedeDNS2.Text ;
    usuario     := edRedeUsuario.Text ;
    senha       := edRedeSenha.Text ;
    proxy       := cbxRedeProxy.ItemIndex ;
    proxy_ip    := edRedeProxyIP.Text ;
    proxy_porta := edRedeProxyPorta.Value ;
    proxy_user  := edRedeProxyUser.Text ;
    proxy_senha := edRedeProxySenha.Text ;
  end;
end;

procedure TFrmACBrMonitor.AjustaACBrSAT;
begin
  with ACBrSAT1 do
  begin
    if (cbxModeloSAT.ItemIndex = 3) then
    begin
      ACBrIntegrador1.PastaInput  := edMFEInput.Text;
      ACBrIntegrador1.PastaOutput := edMFEOutput.Text;
      ACBrIntegrador1.Timeout     := seMFETimeout.Value;

      Integrador := ACBrIntegrador1;
    end
    else
      Integrador := Nil;

    Modelo  := TACBrSATModelo( cbxModeloSAT.ItemIndex ) ;
    ArqLOG  := edSATLog.Text;
    NomeDLL := edNomeDLL.Text;
    Config.ide_numeroCaixa := seNumeroCaixa.Value;
    Config.ide_tpAmb       := TpcnTipoAmbiente( cbxAmbiente.ItemIndex );
    Config.ide_CNPJ        := edtSwHCNPJ.Text;
    Config.emit_CNPJ       := edtEmitCNPJ.Text;
    Config.emit_IE         := edtEmitIE.Text;
    Config.emit_IM         := edtEmitIM.Text;
    Config.emit_cRegTrib      := TpcnRegTrib( cbxRegTributario.ItemIndex ) ;
    Config.emit_cRegTribISSQN := TpcnRegTribISSQN( cbxRegTribISSQN.ItemIndex ) ;
    Config.emit_indRatISSQN   := TpcnindRatISSQN( cbxIndRatISSQN.ItemIndex ) ;
    Config.PaginaDeCodigo     := sePagCod.Value;
    Config.EhUTF8             := cbxUTF8.Checked;
    Config.infCFe_versaoDadosEnt := sfeVersaoEnt.Value;

    ConfigArquivos.PastaCFeVenda := PathWithDelim(edSATPathArqs.Text)+'Vendas';
    ConfigArquivos.PastaCFeCancelamento := PathWithDelim(edSATPathArqs.Text)+'Cancelamentos';
    ConfigArquivos.PastaEnvio:= PathWithDelim(edSATPathArqs.Text)+'Enviados';
    ConfigArquivos.SalvarCFe := cbxSATSalvarCFe.Checked;
    ConfigArquivos.SalvarCFeCanc := cbxSATSalvarCFeCanc.Checked;
    ConfigArquivos.SalvarEnvio := cbxSATSalvarEnvio.Checked;
    ConfigArquivos.SepararPorCNPJ := cbxSATSepararPorCNPJ.Checked;
    ConfigArquivos.SepararPorMes := cbxSATSepararPorMES.Checked;

  end;

  ConfiguraRedeSAT;
end;

procedure TFrmACBrMonitor.TrataErrosSAT(Sender: TObject; E: Exception);
var
   Erro : String ;
begin
   Erro := Trim(E.Message) ;
   ACBrSAT1.DoLog( E.ClassName+' - '+Erro);
end ;

procedure TFrmACBrMonitor.PrepararImpressaoSAT(NomeImpressora : string; GerarPDF : boolean);
begin
  if cbUsarFortes.Checked then
  begin
    ACBrSAT1.Extrato := ACBrSATExtratoFortes1;

    ACBrSATExtratoFortes1.LarguraBobina    := seLargura.Value;
    ACBrSATExtratoFortes1.Margens.Topo     := seMargemTopo.Value ;
    ACBrSATExtratoFortes1.Margens.Fundo    := seMargemFundo.Value ;
    ACBrSATExtratoFortes1.Margens.Esquerda := seMargemEsquerda.Value ;
    ACBrSATExtratoFortes1.Margens.Direita  := seMargemDireita.Value ;
    ACBrSATExtratoFortes1.MostrarPreview   := cbPreview.Checked;

    ACBrSATExtratoFortes1.ImprimeDescAcrescItem := cbxImprimirDescAcresItemSAT.Checked;
    ACBrSATExtratoFortes1.ImprimeEmUmaLinha := cbxImprimirItem1LinhaSAT.Checked;
    ACBrSATExtratoFortes1.UsaCodigoEanImpressao := cbxImprimirCodEANitemSAT.Checked;

    if ( GerarPDF ) then
      ACBrSATExtratoFortes1.Filtro := TACBrSATExtratoFiltro(fiPDF)
    else
      ACBrSATExtratoFortes1.Filtro := TACBrSATExtratoFiltro(fiNenhum);

    if ( Trim(edtLogoMarcaNFCeSAT.Text) <> '') and FileExists(edtLogoMarcaNFCeSAT.Text) then
    begin
       ACBrSATExtratoFortes1.LogoVisible := True;
       ACBrSATExtratoFortes1.PictureLogo.LoadFromFile(edtLogoMarcaNFCeSAT.Text);
    end
    else
      ACBrSATExtratoFortes1.LogoVisible := False;

    try
      if NomeImpressora <> '' then
         ACBrSATExtratoFortes1.PrinterName := NomeImpressora
      else
      if lImpressora.Caption <> '' then
        ACBrSATExtratoFortes1.PrinterName := lImpressora.Caption;
    except
    end;
  end
  else
  begin
    if GerarPDF then
      raise Exception.Create('Função gerar PDF não implementada para modo de impressão ESCPOS.');

    ACBrSAT1.Extrato := ACBrSATExtratoESCPOS1;

    ConfiguraPosPrinter;

    ACBrSATExtratoESCPOS1.ImprimeDescAcrescItem   := cbxImprimirDescAcresItemSAT.Checked;
    ACBrSATExtratoESCPOS1.ImprimeEmUmaLinha       := cbxImprimirItem1LinhaSAT.Checked;
    ACBrSATExtratoESCPOS1.PosPrinter.Device.Porta := cbxPorta.Text;
    ACBrSATExtratoESCPOS1.ImprimeChaveEmUmaLinha  := TAutoSimNao(rdgImprimeChave1LinhaSAT.ItemIndex);
    ACBrSATExtratoESCPOS1.UsaCodigoEanImpressao   := cbxImprimirDescAcresItemSAT.Checked;

    ACBrSATExtratoESCPOS1.PosPrinter.Device.Ativar;
    ACBrSATExtratoESCPOS1.ImprimeQRCode := True;
  end;

  if (edSH_RazaoSocial.Text <> '') then
  begin
    ACBrSAT1.Extrato.SoftwareHouse := edSH_RazaoSocial.Text;
    ACBrSAT1.Extrato.Site          := edSH_Site.Text;
  end;
end;

procedure TFrmACBrMonitor.PathClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(TEdit(Sender).Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
  begin
    Dir := TEdit(Sender).Text;
    if Dir = '' then
      Dir := ExtractFileDir(application.ExeName)
    else if not DirectoryExists(Dir) then
      Dir := ExtractFileDir(Dir);
  end;

  Dialogs.SelectDirectory('Selecione o diretório',Dir,Dir);
  TEdit(Sender).Text := Dir;
end;

{---------------------------------- ACBrBAL -----------------------------------}
procedure TFrmACBrMonitor.cbBALModeloChange(Sender: TObject);
begin
  try
    ACBrBAL1.Desativar;
    if cbBALModelo.ItemIndex >= 0 then
      ACBrBAL1.Modelo := TACBrBALModelo(cbBALModelo.ItemIndex)
    else
      ACBrBAL1.Modelo := balNenhum;
  finally
    cbBALModelo.ItemIndex := integer(ACBrBAL1.Modelo);
    cbBALPorta.Text := ACBrBAL1.Porta;
  end;

  AvaliaEstadoTsBAL;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.AvaliaEstadoTsBAL;
begin
  bBALAtivar.Enabled := (ACBrBAL1.Modelo <> balNenhum);
  bBALTestar.Enabled := ACBrBAL1.Ativo;
  cbBALPorta.Enabled := bBALAtivar.Enabled;
  sedBALIntervalo.Enabled := bBALAtivar.Enabled;

  bBALAtivar.Glyph := nil;
  if ACBrBAL1.Ativo then
  begin
    bBALAtivar.Caption := '&Desativar';
    ImageList1.GetBitmap(6, bBALAtivar.Glyph);
  end
  else
  begin
    bBALAtivar.Caption := '&Ativar';
    ImageList1.GetBitmap(5, bBALAtivar.Glyph);
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbBALPortaChange(Sender: TObject);
begin
  try
    if ACBrBAL1.Ativo then
      bBALAtivar.Click;

    ACBrBAL1.Porta := cbBALPorta.Text;
  finally
    cbBALPorta.Text := ACBrBAL1.Porta;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sedBALIntervaloChanged(Sender: TObject);
begin
  ACBrBal1.Intervalo := sedIntervalo.Value;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bBALAtivarClick(Sender: TObject);
begin
  if bBALAtivar.Caption = '&Ativar' then
  begin
    ACBrBAL1.Ativar;

    ACBrBAL1.LePeso;
    if ACBrBAL1.UltimaResposta = '' then
    begin
      ACBrBAL1.Desativar;
      AddLinesLog('BAL -> Balança não responde!');
    end;
  end
  else
    ACBrBAL1.Desativar;

  AvaliaEstadoTsBAL;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bBALTestarClick(Sender: TObject);
begin
  ACBrBAL1.LePeso;
  if ACBrBAL1.UltimaResposta <> '' then
    AddLinesLog(Format('BAL -> Peso Lido: %f', [ACBrBAL1.UltimoPesoLido]))
  else
    AddLinesLog('BAL -> Timeout');
end;

procedure TFrmACBrMonitor.cbETQModeloChange(Sender: TObject);
begin
  try
    ACBrETQ1.Desativar;
    if cbETQModelo.ItemIndex >= 0 then
      ACBrETQ1.Modelo := TACBrETQModelo(cbETQModelo.ItemIndex)
    else
      ACBrETQ1.Modelo := etqNenhum;
  finally
    cbETQModelo.ItemIndex := integer(ACBrETQ1.Modelo);
    cbETQPorta.Text := ACBrETQ1.Porta;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbETQPortaChange(Sender: TObject);
begin
  try
    ACBrETQ1.Porta := cbETQPorta.Text;
  finally
    cbETQPorta.Text := ACBrETQ1.Porta;
  end;
end;


{-------------------------- Terminal de Consulta ------------------------------}
procedure TFrmACBrMonitor.tsTCShow(Sender: TObject);
begin
  AvaliaEstadoTsTC;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.cbxTCModeloChange(Sender: TObject);
begin
  TCPServerTC.Ativo := False;
  TimerTC.Enabled := False;
  AvaliaEstadoTsTC;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bTCAtivarClick(Sender: TObject);
begin
  if not TCPServerTC.Ativo then
    TCPServerTC.Port := edTCPort.Text;

  if not FileExists(edTCArqPrecos.Text) then
    raise Exception.Create('Arquivo de Preços não encontrado em: [' +
      edTCArqPrecos.Text + ']');

  TCPServerTC.Ativo := (bTCAtivar.Caption = '&Ativar');
  TimerTC.Enabled := TCPServerTC.Ativo;

  AvaliaEstadoTsTC;

  AddLinesLog('Servidor de Terminal de Consulta: ' + IfThen(
    TCPServerTC.Ativo, 'ATIVADO', 'DESATIVADO'));
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sbTCArqPrecosEditClick(Sender: TObject);
begin
  OpenURL(edTCArqPrecos.Text);
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.AvaliaEstadoTsTC;
begin
  edTCPort.Enabled := (cbxTCModelo.ItemIndex > 0);
  edTCArqPrecos.Enabled := edTCPort.Enabled;
  sbTCArqPrecosEdit.Enabled := edTCPort.Enabled;
  sbTCArqPrecosFind.Enabled := edTCPort.Enabled;

  bTCAtivar.Enabled := edTCPort.Enabled and (StrToIntDef(edTCPort.Text, 0) > 0);

  bTCAtivar.Glyph := nil;
  if TCPServerTC.Ativo then
  begin
    bTCAtivar.Caption := '&Desativar';
    shpTC.Brush.Color := clLime;
    ImageList1.GetBitmap(6, bTCAtivar.Glyph);
  end
  else
  begin
    bTCAtivar.Caption := '&Ativar';
    shpTC.Brush.Color := clRed;
    ImageList1.GetBitmap(5, bTCAtivar.Glyph);
    mTCConexoes.Lines.Clear;
  end;
end;

procedure TFrmACBrMonitor.ConfiguraDANFe(GerarPDF, MostrarPreview: Boolean);
var
  OK: boolean;
begin
  if ACBrNFe1.NotasFiscais.Count > 0 then
  begin
    if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
    begin
      if (rgModeloDANFeNFCE.ItemIndex = 0) or GerarPDF then
        ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1
      else
        ACBrNFe1.DANFE := ACBrNFeDANFeESCPOS1;

      ACBrNFe1.DANFE.Impressora := cbxImpressoraNFCe.Text;
    end
    else
    begin
      ACBrNFe1.DANFE := ACBrNFeDANFeRL1;
      ACBrNFe1.DANFE.Impressora := cbxImpressora.Text;
    end;

    if (ACBrNFe1.NotasFiscais.Items[0].NFe.procNFe.cStat in [101, 151, 155]) then
       ACBrNFe1.DANFE.NFeCancelada := True
    else
       ACBrNFe1.DANFE.NFeCancelada := False;
  end;

  if GerarPDF and not DirectoryExists(PathWithDelim(edtPathPDF.Text))then
    ForceDirectories(PathWithDelim(edtPathPDF.Text));

  if ACBrNFe1.DANFE <> nil then
  begin
    ACBrNFe1.DANFE.TipoDANFE := StrToTpImp(OK, IntToStr(rgTipoDanfe.ItemIndex + 1));
    ACBrNFe1.DANFE.Logo := edtLogoMarca.Text;
    ACBrNFe1.DANFE.Sistema := edSH_RazaoSocial.Text;
    ACBrNFe1.DANFE.Site := edtSiteEmpresa.Text;
    ACBrNFe1.DANFE.Email := edtEmailEmpresa.Text;
    ACBrNFe1.DANFE.Fax := edtFaxEmpresa.Text;
    ACBrNFe1.DANFE.ImprimirDescPorc := cbxImpDescPorc.Checked;
    ACBrNFe1.DANFE.NumCopias := edtNumCopia.Value;
    ACBrNFe1.DANFE.MargemInferior := fspeMargemInf.Value;
    ACBrNFe1.DANFE.MargemSuperior := fspeMargemSup.Value;
    ACBrNFe1.DANFE.MargemDireita := fspeMargemDir.Value;
    ACBrNFe1.DANFE.MargemEsquerda := fspeMargemEsq.Value;
    ACBrNFe1.DANFE.PathPDF := PathWithDelim(edtPathPDF.Text);
    ACBrNFe1.DANFE.CasasDecimais._qCom := spedtCasasDecimaisQtd.Value;
    ACBrNFe1.DANFE.CasasDecimais._vUnCom := spedtDecimaisVUnit.Value;
    ACBrNFe1.DANFE.ExibirResumoCanhoto := cbxExibeResumo.Checked;
    ACBrNFe1.DANFE.ImprimirTotalLiquido := cbxImpValLiq.Checked;
    ACBrNFe1.DANFE.FormularioContinuo := cbxFormCont.Checked;
    ACBrNFe1.DANFE.MostrarStatus := cbxMostraStatus.Checked;
    ACBrNFe1.DANFE.ExpandirLogoMarca := cbxExpandirLogo.Checked;
    ACBrNFe1.DANFE.TamanhoFonte_DemaisCampos := speFonteCampos.Value;
    ACBrNFe1.DANFE.TamanhoFonteEndereco:= speFonteEndereco.Value;
    ACBrNFe1.DANFE.PosCanhoto := TPosRecibo( rgLocalCanhoto.ItemIndex );
    ACBrNFe1.DANFE.UsarSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;

    if ACBrNFe1.DANFE = ACBrNFeDANFeRL1 then
    begin
      ACBrNFeDANFeRL1.Fonte.Nome := TNomeFonte(rgTipoFonte.ItemIndex);
      ACBrNFeDANFeRL1.LarguraCodProd := speLargCodProd.Value;
      ACBrNFeDANFeRL1.ExibirEAN := cbxExibirEAN.Checked;
      ACBrNFeDANFeRL1.ExibeCampoFatura := cbxExibirCampoFatura.Checked;
      ACBrNFeDANFeRL1.QuebraLinhaEmDetalhamentoEspecifico := cbxQuebrarLinhasDetalhesItens.Checked;
      ACBrNFeDANFeRL1.Fonte.TamanhoFonte_RazaoSocial := speFonteRazao.Value;
      ACBrNFeDANFeRL1.AltLinhaComun := speAlturaCampos.Value;
      ACBrNFeDANFeRL1.PosCanhoto := TPosRecibo( rgLocalCanhoto.ItemIndex );
      ACBrNFeDANFeRL1.ImprimirUnQtVlComercial := TImprimirUnidQtdeValor(cbxUnComTributavel.ItemIndex);
      ACBrNFeDANFeRL1.ImprimirDetalhamentoEspecifico := cbxImpDetEspNFe.Checked;
      ACBrNFeDANFeRL1.ImprimirDadosDocReferenciados := cbxImpDocsReferenciados.Checked;
      ACBrNFeDANFeRL1.ExibirBandInforAdicProduto := cbxExibirInfAdicProduto.Checked;
      ACBrNFeDANFeRL1.LogoemCima := cbxExibirLogoEmCima.Checked;
    end
    else if ACBrNFe1.DANFE = ACBrNFeDANFCeFortes1 then
    begin
      ACBrNFeDANFCeFortes1.ImprimeDescAcrescItem := cbxImprimirDescAcresItemNFCe.Checked;
      ACBrNFeDANFCeFortes1.ImprimirTotalLiquido  := cbxImprimirDescAcresItemNFCe.Checked;
      ACBrNFeDANFCeFortes1.MargemInferior        := fspeNFCeMargemInf.Value;
      ACBrNFeDANFCeFortes1.MargemSuperior        := fspeNFCeMargemSup.Value;
      ACBrNFeDANFCeFortes1.MargemDireita         := fspeNFCeMargemDir.Value;
      ACBrNFeDANFCeFortes1.MargemEsquerda        := fspeNFCeMargemEsq.Value;
      ACBrNFeDANFCeFortes1.LarguraBobina         := fspeLarguraNFCe.Value;
      ACBrNFeDANFCeFortes1.ImprimeEmUmaLinha     := cbxImprimirItem1LinhaNFCe.Checked;
      ACBrNFEDANFCeFortes1.QRCodeLateral         := cbxImprimirQRCodeLateralNFCe.Checked;
      ACBrNFeDANFCeFortes1.UsaCodigoEanImpressao := cbxImprimirCodigoEANNFCe.Checked;
      ACBrNFeDANFCeFortes1.ImprimeNomeFantasia   := cbxImprimirNomeFantasiaNFCe.Checked;

      if ( Trim(edtLogoMarcaNFCeSAT.Text) <> '') and FileExists(edtLogoMarcaNFCeSAT.Text) then
        ACBrNFeDANFCeFortes1.Logo                := edtLogoMarcaNFCeSAT.Text
      else
        ACBrNFeDANFCeFortes1.Logo                := '';
    end
    else if ACBrNFe1.DANFE = ACBrNFeDANFeESCPOS1 then
    begin
      ACBrNFeDANFeESCPOS1.PosPrinter.Modelo := TACBrPosPrinterModelo(cbxModelo.ItemIndex);
      ACBrNFeDANFeESCPOS1.PosPrinter.Device.Porta := cbxPorta.Text;
      ACBrNFeDANFeESCPOS1.ImprimeEmUmaLinha := cbxImprimirItem1LinhaNFCe.Checked;
      ACBrNFeDANFeESCPOS1.ImprimeDescAcrescItem := cbxImprimirDescAcresItemNFCe.Checked;
      ACBrNFeDANFeESCPOS1.QRCodeLateral         := cbxImprimirQRCodeLateralNFCe.Checked;
      ACBrNFeDANFeESCPOS1.ImprimeNomeFantasia   := cbxImprimirNomeFantasiaNFCe.Checked;

      if ( Trim(edtLogoMarcaNFCeSAT.Text) <> '') and FileExists(edtLogoMarcaNFCeSAT.Text) then
        ACBrNFeDANFeESCPOS1.Logo                := edtLogoMarcaNFCeSAT.Text
      else
        ACBrNFeDANFeESCPOS1.Logo                := '';

      if not ACBrPosPrinter1.ControlePorta then
      begin
        ACBrNFeDANFeESCPOS1.PosPrinter.Ativar;
        if not ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativo then
          ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativar;
      end;
    end;
  end;

  ACBrNFe1.DANFE.MostrarPreview := (not GerarPDF) and
                                   (cbxMostrarPreview.Checked or MostrarPreview) and
                                   (ACBrNFe1.DANFE <> ACBrNFeDANFeESCPOS1);

  //if ACBrNFe1.DANFE.MostrarPreview or MostrarPreview then
  //  ForceForeground(Self.Handle);
end;

procedure TFrmACBrMonitor.VerificaDiretorios;
var
  CanEnabled: Boolean;
begin
  CanEnabled := cbxSalvarArqs.Checked;

  cbxPastaMensal.Enabled := CanEnabled;
  cbxAdicionaLiteral.Enabled := CanEnabled;
  cbxEmissaoPathNFe.Enabled := CanEnabled;
  cbxSalvaPathEvento.Enabled := CanEnabled;
  cbxSepararPorCNPJ.Enabled := CanEnabled;
  cbxSepararporModelo.Enabled := CanEnabled;
  cbxSalvarNFesProcessadas.Enabled := CanEnabled;
  edtPathNFe.Enabled := CanEnabled;
  edtPathInu.Enabled := CanEnabled;
  edtPathDPEC.Enabled := CanEnabled;
  edtPathEvento.Enabled := CanEnabled;
  sbPathNFe.Enabled := CanEnabled;
  sbPathInu.Enabled := CanEnabled;
  sbPathDPEC.Enabled := CanEnabled;
  sbPathEvento.Enabled := CanEnabled;

  edtPathLogs.Enabled := ckSalvar.Checked;
  sbPathSalvar.Enabled := ckSalvar.Checked;
end;

procedure TFrmACBrMonitor.LimparResp;
begin
  mResposta.Clear;
end;

procedure TFrmACBrMonitor.ExibeResp(Documento: ansistring);
begin
  Documento := StringReplace(Documento, '><', '>' + LineBreak + '<', [rfReplaceAll]);
  Documento := StringReplace(Documento, '> <', '>' + LineBreak + '<', [rfReplaceAll]);
  mResposta.Text := Documento;
end;


{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.AddLinesLog(aLineLog: String);
  procedure RemoveLinesLog;
  begin
    mResp.Lines.BeginUpdate;
    while mResp.Lines.Count > 500 do
      mResp.Lines.Delete(0);

    mResp.Lines.EndUpdate;
  end;

begin
  if aLineLog <> '' then
  begin
    if cbLog.Checked then
    begin
      if chkMostraLogNaTela.Checked then
      begin
        if ( mResp.Lines.Count > 500 ) then
           RemoveLinesLog;

        mResp.Lines.Add(aLineLog);
      end;

      AddLinesLogFile(ArqLogTXT, aLineLog, True, True, True);

      Application.ProcessMessages;
    end;
  end;
end;

procedure TFrmACBrMonitor.AddLinesLog(aLinesLog: TStrings);
begin
  if chkMostraLogNaTela.Checked and ( aLinesLog.Count > 0 ) then
     mResp.Lines.AddStrings(aLinesLog);
end;

procedure TFrmACBrMonitor.AddLinesLogFile(const ArqFileLog: String; aLineLogFile: AnsiString;
  const AppendIfExists : Boolean; const AddLineBreak : Boolean; const PrintDateTime: Boolean);
var
  sDateTime: String;
begin
  sDateTime := '';
  if PrintDateTime then
     sDateTime := FormatDateTime('dd/mm/yyyy hh:nn:ss',Now)+' - ';

  try
    WriteToTXT(ArqFileLog, sDateTime+aLineLogFile, AppendIfExists, AddLineBreak);
  except
    on E: Exception do
    begin
      mResp.Lines.Add(sDateTime+'Erro ao escrever no arquivo: '+ArqFileLog+' ['+E.Message+']');
    end;
  end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.TimerTCTimer(Sender: TObject);
var
  I: integer;
  AConnection: TTCPBlockSocket;
begin
  // Verificando se o arquivo de Preços foi atualizado //
  if FileAge(edTCArqPrecos.Text) > fsDTPrecos then
  begin
    fsSLPrecos.Clear;
    fsSLPrecos.LoadFromFile(edTCArqPrecos.Text);
    fsDTPrecos := FileAge(edTCArqPrecos.Text);
  end;

  with TCPServerTC.ThreadList.LockList do
    try
      for I := 0 to Count - 1 do
      begin
        AConnection := TACBrTCPServerThread(Items[I]).TCPBlockSocket;
        try
          AConnection.Tag := AConnection.Tag + 1;
          AConnection.SendString('#live?');
          if AConnection.Tag > 10 then   // 10 Falhas no #live?... desconecte
            AConnection.CloseSocket;
        except
          AConnection.CloseSocket;
        end;
      end;
    finally
      TCPServerTC.ThreadList.UnlockList;
    end;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.sbTCArqPrecosFindClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivos txt|*.txt|Arquivos TXT|*.TXT';

  if OpenDialog1.Execute then
  begin
    edTCArqPrecos.Text := OpenDialog1.FileName;
    fsDTPrecos := 0; // Força re-leitura
  end;
end;

(*
procedure TFrmACBrMonitor.TCPServerTCConecta(
  const TCPBlockSocket: TTCPBlockSocket; var Enviar: String);
 Var IP, Resp, Id : String ;
     Indice : Integer ;
begin
  TCPServerTC.OnRecebeDados := nil ;
  try
     TCPBlockSocket.SendString('#ok') ;
     Id := Trim(TCPBlockSocket.RecvPacket(2000)) ;
     TCPBlockSocket.SendString('#alwayslive');
     Resp := Trim(TCPBlockSocket.RecvPacket(2000)) ;
     if Resp <> '#alwayslive_ok' then
     begin
        fsLinesLog := 'Resposta Inválida do T.C.' ;
        AddLinesLog ;
        TCPBlockSocket.CloseSocket ;
     end ;

     IP := TCPBlockSocket.GetRemoteSinIP ;

     Indice := mTCConexoes.Lines.IndexOf( IP ) ;
     if Indice < 0 then
     begin
        mTCConexoes.Lines.Add( IP ) ;
        fsLinesLog := 'Inicio Conexão TC: ['+Id+'] IP: ['+ IP +
                      '] em: ['+FormatDateTime('dd/mm/yy hh:nn:ss', now )+']' ;
        AddLinesLog ;
     end ;
  finally
     TCPServerTC.OnRecebeDados := TCPServerTCRecebeDados ;
  end ;
end;

procedure TFrmACBrMonitor.TCPServerTCDesConecta(
  const TCPBlockSocket: TTCPBlockSocket; Erro: Integer; ErroDesc: String);
 Var IP : String ;
     Indice : Integer ;
begin
  IP  := TCPBlockSocket.GetRemoteSinIP ;
  fsLinesLog := 'Fim Conexão TC IP: ['+ IP + '] em: '+
                FormatDateTime('dd/mm/yy hh:nn:ss', now ) ;
  AddLinesLog ;

  Indice := mTCConexoes.Lines.IndexOf( IP ) ;
  if Indice >= 0 then
     mTCConexoes.Lines.Delete( Indice );
end;

procedure TFrmACBrMonitor.TCPServerTCRecebeDados(
  const TCPBlockSocket: TTCPBlockSocket; const Recebido: String;
  var Enviar: String);
begin
  { Le o que foi enviado atravez da conexao TCP }
  fsTCComando := Trim(Recebido) ;
  if fsTCComando = '' then
     exit ;

  if fsTCComando = '#live' then
     exit ;

  fsLinesLog := 'TC: ['+TCPBlockSocket.GetRemoteSinIP+
                '] RX: <- ['+fsTCComando+']' ;
  AddLinesLog ;

  if copy(fsTCComando,1,1) = '#' then
  begin
     fsTCResposta := '' ;
     BuscaPreco ;

     if fsTCResposta <> '' then
     begin
        TCPBlockSocket.SendString( fsTCResposta ) ;
        fsLinesLog := '     TX: -> ['+fsTCResposta+']' ;
        AddLinesLog ;
     end ;
  end ;
end;
*)
procedure TFrmACBrMonitor.sbCHQSerialClick(Sender: TObject);
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(self);

  try
    if ACBrCHQ1.Ativo then
      ACBrCHQ1.Desativar;

    frConfiguraSerial.Device.Porta := ACBrCHQ1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := cbCHQPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrCHQ1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbCHQPorta.Text := frConfiguraSerial.Device.Porta;
      ACBrCHQ1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
  end;

end;

{----------------------------PosPrinter----------------------------------------}
procedure TFrmACBrMonitor.bbAtivarClick(Sender: TObject);
begin
  if ACBrPosPrinter1.Ativo then
  begin
    ACBrPosPrinter1.Desativar;

    bbAtivar.Caption := 'Ativar';
    sbSerial.Enabled := True;
  end
  else
  begin
    ConfiguraPosPrinter;
    ACBrPosPrinter1.Ativar;

    sbSerial.Enabled := False;
    bbAtivar.Caption := 'Desativar';
  end;
end;

procedure TFrmACBrMonitor.bBoletoRelatorioRetornoClick(Sender: TObject);
begin
  if ACBrBoleto1.ListadeBoletos.Count > 0 then
    ImprimeRelatorioRetorno(lsvArqsRetorno.Selected.Caption);

end;

procedure TFrmACBrMonitor.bBOLLerArqRelatorioClick(Sender: TObject);
begin
  if lsvArqsRetorno.ItemIndex >= 0 then
  begin
    ACBrBoleto1.NomeArqRetorno := deBolDirRetornoRel.Text + PathDelim + lsvArqsRetorno.Selected.Caption;
    ACBrBoleto1.LerRetorno();

     with mBOLRelatorio do
     begin
       Lines.BeginUpdate;

       Lines.Add('Arquivo de Retorno processado com sucesso:');
       Lines.Add('Arquivo: '+lsvArqsRetorno.Selected.Caption);
       Lines.Add('');
       Lines.Add('Beneficiario: '+ACBrBoleto1.Cedente.Nome);
       Lines.Add('');
       Lines.Add('Total de Titulos: '+ IntToStrZero(ACBrBoleto1.ListadeBoletos.Count,3));
       Lines.Add('');
       Lines.Add(' - Clique em Gerar Relatório para imprimir.');

       Lines.EndUpdate;
     end;

     if ACBrBoleto1.ListadeBoletos.Count > 0 then
       bBoletoRelatorioRetorno.Enabled := True;
  end
  else
    MessageDlg('Atenção', 'Selecione o arquivo a carregar',
      mtError, [mbOK], '');
end;

procedure TFrmACBrMonitor.ConfiguraPosPrinter(SerialParams : String);
var
  OldAtivo: Boolean;
begin
  OldAtivo := ACBrPosPrinter1.Ativo;
  try
    ACBrPosPrinter1.Ativo              := False;  //Deliga para poder configurar
    ACBrPosPrinter1.Modelo             := TACBrPosPrinterModelo(cbxModelo.ItemIndex);
    ACBrPosPrinter1.Porta              := cbxPorta.Text;
    ACBrPosPrinter1.LinhasBuffer       := seBuffer.Value;
    ACBrPosPrinter1.LinhasEntreCupons  := seLinhasPular.Value;
    ACBrPosPrinter1.EspacoEntreLinhas  := seEspacosLinhas.Value;
    ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
    ACBrPosPrinter1.ControlePorta      := cbControlePorta.Checked;
    ACBrPosPrinter1.CortaPapel         := cbCortarPapel.Checked;
    ACBrPosPrinter1.PaginaDeCodigo     := TACBrPosPaginaCodigo(cbxPagCodigo.ItemIndex);
    ACBrPosPrinter1.IgnorarTags        := cbIgnorarTags.Checked;
    ACBrPosPrinter1.TraduzirTags       := cbTraduzirTags.Checked;
    ACBrPosPrinter1.ArqLOG             := edPosPrinterLog.Text;

    ACBrPosPrinter1.ConfigBarras.LarguraLinha  := seCodBarrasLargura.Value;
    ACBrPosPrinter1.ConfigBarras.Altura        := seCodBarrasAltura.Value;
    ACBrPosPrinter1.ConfigBarras.MostrarCodigo := cbHRI.Checked;

    ACBrPosPrinter1.ConfigQRCode.ErrorLevel    := seQRCodeErrorLevel.Value;
    ACBrPosPrinter1.ConfigQRCode.LarguraModulo := seQRCodeLargMod.Value;
    ACBrPosPrinter1.ConfigQRCode.Tipo          := seQRCodeTipo.Value;

    ACBrPosPrinter1.ConfigLogo.IgnorarLogo := not cbEscPosImprimirLogo.Checked;
    ACBrPosPrinter1.ConfigLogo.FatorX   := seLogoFatorX.Value;
    ACBrPosPrinter1.ConfigLogo.FatorY   := seLogoFatorY.Value;
    ACBrPosPrinter1.ConfigLogo.KeyCode1 := seLogoKC1.Value;
    ACBrPosPrinter1.ConfigLogo.KeyCode2 := seLogoKC2.Value;

    if NaoEstaVazio(SerialParams) then
      ACBrPosPrinter1.Device.ParamsString := SerialParams;
  finally
    ACBrPosPrinter1.Ativo := OldAtivo;

    cbxModelo.ItemIndex := Integer(ACBrPosPrinter1.Modelo);
    cbxPorta.Text       := ACBrPosPrinter1.Porta;
  end;
end;

procedure TFrmACBrMonitor.SetComumConfig(Configuracoes: TConfiguracoes);
var
  OK: boolean;
  PathMunIBGE: String;
begin
  PathMunIBGE := PathWithDelim(ExtractFilePath(Application.ExeName)) + 'MunIBGE' + PathDelim ;
  with Configuracoes do
  begin
    with Geral do
    begin
      Salvar         := ckSalvar.Checked;
      SSLLib         := TSSLLib(cbSSLLib.ItemIndex);
      SSLCryptLib    := TSSLCryptLib(cbCryptLib.ItemIndex);
      SSLHttpLib     := TSSLHttpLib(cbHttpLib.ItemIndex);
      SSLXmlSignLib  := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ValidarDigest  := cbValidarDigest.Checked;
      RetirarAcentos := cbRetirarAcentos.Checked;
    end;

    with Certificados do
    begin
      ArquivoPFX  := edtArquivoPFX.Text;
      NumeroSerie := edtNumeroSerie.Text;
      Senha       := edtSenha.Text;
      VerificarValidade := chkVerificarValidadeCertificado.Checked;
    end;

    with WebServices do
    begin
      UF       := cbUF.Text;
      Ambiente := StrToTpAmb(Ok, IntToStr(rgTipoAmb.ItemIndex + 1));
      Salvar   := ckSalvar.Checked;
      TimeOut  := edtTimeoutWebServices.Value * 1000;
      AjustaAguardaConsultaRet  := cbxAjustarAut.Checked;
      TimeZoneConf.ModoDeteccao := TTimeZoneModoDeteccao( cbxTimeZoneMode.ItemIndex );

      try
        TimeZoneConf.TimeZoneStr := edTimeZoneStr.Caption;
      except
        TimeZoneConf.TimeZoneStr := GetUTCSistema;
      end;

      if NaoEstaVazio(edtAguardar.Text) then
        AguardarConsultaRet :=
        IfThen(StrToInt(edtAguardar.Text) < 1000, StrToInt(edtAguardar.Text) *
          1000, StrToInt(edtAguardar.Text));

      if NaoEstaVazio(edtTentativas.Text) then
        Tentativas := StrToInt(edtTentativas.Text);

      if NaoEstaVazio(edtIntervalo.Text) then
        IntervaloTentativas :=
          IfThen(StrToInt(edtIntervalo.Text) < 1000, StrToInt(edtIntervalo.Text) *
          1000, StrToInt(edtIntervalo.Text));

      ProxyHost := edtProxyHost.Text;
      ProxyPort := edtProxyPorta.Text;
      ProxyUser := edtProxyUser.Text;
      ProxyPass := edtProxySenha.Text;
    end;

    with Arquivos do
    begin
      Salvar           := True;
      PathSalvar       := edtPathLogs.Text;
      SepararPorMes    := cbxPastaMensal.Checked;
      AdicionarLiteral := cbxAdicionaLiteral.Checked;
      SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
      SepararPorModelo := cbxSepararporModelo.Checked;
    end;
  end;

  // Configurações específicas
  if Configuracoes is TConfiguracoesNFe then
  begin
    TConfiguracoesNFe(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoNFe.ItemIndex+1));
    TConfiguracoesNFe(Configuracoes).Geral.VersaoDF     := StrToVersaoDF(ok, cbVersaoWS.Text);

    TConfiguracoesNFe(Configuracoes).Arquivos.IniServicos    := edtArquivoWebServicesNFe.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.EmissaoPathNFe := cbxEmissaoPathNFe.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.SalvarEvento   := cbxSalvaPathEvento.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.PathNFe        := edtPathNFe.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.PathInu        := edtPathInu.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.PathEvento     := edtPathEvento.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.SalvarApenasNFeProcessadas := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.NormatizarMunicipios  := cbxNormatizarMunicipios.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.PathArquivoMunicipios := PathMunIBGE;

  end
  else if Configuracoes is TConfiguracoesCTe then
  begin
    TConfiguracoesCTe(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoCTe.ItemIndex + 1));
    TConfiguracoesCTe(Configuracoes).Geral.VersaoDF     := StrToVersaoCTe(ok, cbVersaoWSCTe.Text);

    TConfiguracoesCTe(Configuracoes).Arquivos.IniServicos    := edtArquivoWebServicesCTe.Text;
    TConfiguracoesCTe(Configuracoes).Arquivos.EmissaoPathCTe := cbxEmissaoPathNFe.Checked;
    TConfiguracoesCTe(Configuracoes).Arquivos.PathCTe        := edtPathNFe.Text;
    TConfiguracoesCTe(Configuracoes).Arquivos.PathInu        := edtPathInu.Text;
    TConfiguracoesCTe(Configuracoes).Arquivos.PathEvento     := edtPathEvento.Text;
    TConfiguracoesCTe(Configuracoes).Arquivos.SalvarApenasCTeProcessados := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesCTe(Configuracoes).Arquivos.NormatizarMunicipios  := cbxNormatizarMunicipios.Checked;
    TConfiguracoesCTe(Configuracoes).Arquivos.PathArquivoMunicipios := PathMunIBGE;
  end
  else if Configuracoes is TConfiguracoesMDFe then
  begin
    TConfiguracoesMDFe(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoMDFe.ItemIndex + 1));
    TConfiguracoesMDFe(Configuracoes).Geral.VersaoDF     := StrToVersaoMDFe(ok, cbVersaoWSMDFe.Text);

    TConfiguracoesMDFe(Configuracoes).Arquivos.IniServicos     := edtArquivoWebServicesMDFe.Text;
    TConfiguracoesMDFe(Configuracoes).Arquivos.EmissaoPathMDFe := cbxEmissaoPathNFe.Checked;
    TConfiguracoesMDFe(Configuracoes).Arquivos.PathMDFe        := edtPathNFe.Text;
    TConfiguracoesMDFe(Configuracoes).Arquivos.PathEvento      := edtPathEvento.Text;
    TConfiguracoesMDFe(Configuracoes).Arquivos.SalvarApenasMDFeProcessados := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesMDFe(Configuracoes).Arquivos.NormatizarMunicipios := cbxNormatizarMunicipios.Checked;
    TConfiguracoesMDFe(Configuracoes).Arquivos.PathArquivoMunicipios := PathMunIBGE;
  end
  else if Configuracoes is TConfiguracoesGNRE then
  begin
    TConfiguracoesGNRE(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoGNRe.ItemIndex + 1));

    TConfiguracoesGNRE(Configuracoes).Arquivos.IniServicos     := edtArquivoWebServicesGNRe.Text;
    TConfiguracoesGNRE(Configuracoes).Arquivos.EmissaoPathGNRE := cbxEmissaoPathNFe.Checked;
    TConfiguracoesGNRE(Configuracoes).Arquivos.PathGNRE        := edtPathNFe.Text;
    TConfiguracoesGNRE(Configuracoes).Arquivos.SalvarApenasGNREProcessadas := cbxSalvarNFesProcessadas.Checked;
  end;
end;

procedure TFrmACBrMonitor.AtualizaSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer( ACBrNFe1.Configuracoes.Geral.SSLLib );
  cbCryptLib.ItemIndex   := Integer( ACBrNFe1.Configuracoes.Geral.SSLCryptLib );
  cbHttpLib.ItemIndex    := Integer( ACBrNFe1.Configuracoes.Geral.SSLHttpLib );
  cbXmlSignLib.ItemIndex := Integer( ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib );

  cbSSLType.Enabled := (ACBrNFe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TFrmACBrMonitor.CarregarDFe( XMLorFile : String; var PathDfe: String;
  tipoDFe : TDFeCarregar = tDFeNFe);
var
  IsXML : Boolean;
begin
  IsXML := StringIsXML(XMLorFile);
  if IsXML then
  begin
    case tipoDFe of
      tDFeNFe :
      begin
        if not ACBrNFe1.NotasFiscais.LoadFromString(XMLorFile) then
           raise Exception.Create('Erro ao carregar a nfe.');
      end;

      tDFeEventoNFe :
      begin
        if not ACBrNFe1.EventoNFe.LerXMLFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar o evento da nota.');
      end;

      tDFeInutNFe :
      begin
        if not ACBrNFe1.InutNFe.LerXMLFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar a inutilização da nota.');
      end;

      tDFeCTe :
      begin
        if not ACBrCTe1.Conhecimentos.LoadFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar CTe');
      end;

      tDFeEventoCTe:
      begin
        if not ACBrCTe1.EventoCTe.LerXMLFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar o evento do CTe');
      end;

      tDFeInutCTe:
      begin
        if not ACBrCTe1.InutCTe.LerXMLFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar inutilização do CTe');
      end;

      tDFeMDFe:
      begin
        if not ACBrMDFe1.Manifestos.LoadFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar MDFe');
      end;

      tDFeEventoMDFe:
      begin
        if not ACBrMDFe1.EventoMDFe.LerXMLFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar o evento do MDFe');
      end;

      tDFeGNRe:
      begin
        if not ACBrGNRE1.GuiasRetorno.LoadFromString(XMLorFile) then
          raise Exception.Create('Erro ao carregar a GNRe');
      end;
    end;
  end
  else
  begin
    case tipoDFe of
      tDFeNFe :
      begin
        if FilesExists(XMLorFile) then
        begin
           if not ACBrNFe1.NotasFiscais.LoadFromFile(XMLorFile) then
             raise Exception.Create('Erro ao abrir o arquivo '+ XMLorFile)
           else
             PathDfe := XMLorFile;
        end
        else
          raise Exception.Create('Arquivo '+ XMLorFile +' não encontrado.');
      end;

      tDFeEventoNFe :
      begin
        if FilesExists(XMLorFile) then
        begin
          if not ACBrNFe1.EventoNFe.LerXML(XMLorFile) then
            raise Exception.Create('Erro ao abrir o arquivo '+ XMLorFile)
          else
            PathDfe := XMLorFile;
        end
        else
          raise Exception.Create('Arquivo '+ XMLorFile +' não encontrado.');
      end;

      tDFeInutNFe :
      begin
        if FilesExists(XMLorFile) then
        begin
          if not ACBrNFe1.InutNFe.LerXML(XMLorFile) then
            raise Exception.Create('Erro ao abrir o arquivo '+ XMLorFile)
          else
            PathDfe := XMLorFile;
        end
        else
          raise Exception.Create('Arquivo '+ XMLorFile +' não encontrado.');
      end;

      tDFeCTe:
      begin
        if FilesExists(XMLorFile) then
        begin
          if not ACBrCTe1.Conhecimentos.LoadFromFile(XMLorFile) then
            raise Exception.Create('Erro ao abrir do arquivo do CTe '+ XMLorFile)
          else
            PathDfe := XMLorFile;
        end
        else
          raise Exception.Create('Arquivo do CTe '+ XMLorFile +' não encontrado.');
      end;

      tDFeEventoCTe:
      begin
        if FileExists(XMLorFile) then
        begin
          if not ACBrCTe1.EventoCTe.LerXML(XMLorFile) then
            raise Exception.Create('Erro ao abrir o arquivo de Evento do CTe '+XMLorFile)
          else
            PathDfe := XMLorFile;
        end
        else
          raise Exception.Create('Arquivo '+ XMLorFile +' não encontrado.');
      end;

      tDFeInutCTe:
      begin
        if FileExists(XMLorFile) then
        begin
          if not ACBrCTe1.InutCTe.LerXML(XMLorFile) then
            raise Exception.Create('Erro ao abrir o arquivo de Inutilização do CTe '+XMLorFile)
          else
            PathDfe := XMLorFile;
        end;
      end;

      tDFeMDFe:
      begin
        if FileExists(XMLorFile) then
        begin
          if not ACBrMDFe1.Manifestos.LoadFromFile(XMLorFile) then
            raise Exception.Create('Erro ao abrir o arquivo do Manifesto: '+XMLorFile)
          else
            PathDfe := XMLorFile;
        end;
      end;

      tDFeEventoMDFe:
      begin
        if FileExists(XMLorFile) then
        begin
          if not ACBrMDFe1.EventoMDFe.LerXML(XMLorFile) then
            raise Exception.Create('Erro ao abrir o evento do MDFe '+XMLorFile)
          else
            PathDfe := XMLorFile;
        end;
      end;

      tDFeGNRe:
      begin
        if FileExists(XMLorFile) then
        begin
          if not ACBrGNRE1.GuiasRetorno.LoadFromFile(XMLorFile) then
            raise Exception.Create('Erro ao abrir o arquivo da Guia: '+XMLorFile)
          else
            PathDfe := XMLorFile;
        end;
      end;
    end;
  end;
end;

procedure TFrmACBrMonitor.CarregarDFe(XMLsorFiles : TStrings;
  var PathDfe: String; tipoDFe : TDFeCarregar = tDFeNFe);
var
  i : Integer;
begin
  for i := 0 to XMLsorFiles.Count -1 do
  begin
    try
      CarregarDFe(XMLsorFiles[i], PathDfe, tipoDFe);
      case tipoDFe of
        tDFeNFe :
        begin
          if ACBrNFe1.NotasFiscais.Count > 0 then
             Exit;
        end;

        tDFeEventoNFe:
        begin
          if ACBrNFe1.EventoNFe.Evento.Count > 0 then
             Exit;
        end;

        tDFeInutNFe:
        begin
          if Assigned(ACBrNFe1.InutNFe) then
             Exit;
        end;

        tDFeCTe:
        begin
          if ACBrCTe1.Conhecimentos.Count > 0 then
             Exit;
        end;

        tDFeEventoCTe:
        begin
          if ACBrCTe1.EventoCTe.Evento.Count > 0 then
             Exit;
        end;

        tDFeInutCTe:
        begin
          if Assigned(ACBrCTe1.InutCTe) then
             Exit;
        end;

        tDFeMDFe:
        begin
          if ACBrMDFe1.Manifestos.Count > 0 then
             Exit;
        end;

        tDFeEventoMDFe:
        begin
          if ACBrMDFe1.EventoMDFe.Evento.Count > 0 then
             Exit;
        end;

        tDFeGNRe:
        begin
          if ACBrGNRE1.GuiasRetorno.Count > 0 then
             Exit;
        end;
      end;
    except
      on E: Exception do
      begin
        if i = XMLsorFiles.Count -1 then
          raise Exception.Create(E.Message);
      end;
    end;
  end;
end;

procedure TFrmACBrMonitor.AntesDeImprimir(ShowPreview: Boolean);
begin
  FWasHidden := not IsVisible;

  if ShowPreview then
  begin
    if FWasHidden then
      Restaurar1.Click;

    {$IfDef MSWINDOWS}
    FLastHandle := GetForegroundWindow;
    {$EndIf}

    ForceForeground(Self.Handle);
  end;
end;

procedure TFrmACBrMonitor.DepoisDeImprimir;
begin
  if FWasHidden and IsVisible then
  begin
    FWasHidden := False;
    Application.Minimize;
    Application.ProcessMessages;
  end;
end;

procedure TFrmACBrMonitor.HelptabSheet;
begin
  case pgConfig.TabIndex of
    0:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Monitor.htm';
    1:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Usuario.htm';
    2:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/ECF.htm';
    3:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/ImpressaodeCheque.htm';
    4:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Gaveta1.htm';
    5:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Display.htm';
    6:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/LeitorSerial.htm';
    7:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Geral.htm';
    8:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Balanca.htm';
    9:   FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Etiqueta.htm';
    10:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/TerminaldeConsulta.htm';
    11:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Cedente.htm';
    12:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/ConsultasCEPIBGE.htm';
    13:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/EMail.htm';
    14:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Sedex.htm';
    15:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/NCM.htm';
    16:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Geral1.htm';
    17:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/DadosdoSATCFe.htm';
    18:  FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/PosPrinter.htm';
  else
    FrmACBrMonitor.HelpKeyword := 'ACBrMonitor/Monitor.htm';
  end;

end;

procedure TFrmACBrMonitor.ValidarIntegradorNFCe(ChaveNFe: String = '');
var
  Modelo: Integer;
begin
  if (FrmACBrMonitor.ckNFCeUsarIntegrador.Checked) then
  begin
    if NaoEstaVazio(ChaveNFe) then
      Modelo:= StrToIntDef(copy(OnlyNumber(ChaveNFe),21,2),55);
    if (ACBrNFe1.Configuracoes.Geral.ModeloDF = moNFe) or (Modelo = 55) then
      ACBrNFe1.Integrador := nil
    else
      ACBrNFe1.Integrador := ACBrIntegrador1;
  end;
end;

procedure TFrmACBrMonitor.sbSerialClick(Sender: TObject);
var
  frConfiguraSerial: TfrConfiguraSerial;
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(Self);

  try
    frConfiguraSerial.Device.Porta        := ACBrPosPrinter1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOK then
    begin
      cbxPorta.Text                       := frConfiguraSerial.Device.Porta;
      ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
  end;
end;

procedure TFrmACBrMonitor.cbControlePortaChange(Sender: TObject);
begin
  ACBrPosPrinter1.ControlePorta := cbControlePorta.Checked;
end;

procedure TFrmACBrMonitor.cbCortarPapelChange(Sender: TObject);
begin
  ACBrPosPrinter1.CortaPapel := cbCortarPapel.Checked;
end;

procedure TFrmACBrMonitor.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
    begin
      ACBrNFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrCTe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrMDFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrGNRE1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrBlocoX1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
    end;
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFrmACBrMonitor.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
    begin
      ACBrNFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrCTe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrMDFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrGNRE1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrBlocox1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
    end;
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFrmACBrMonitor.cbTraduzirTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.TraduzirTags := cbTraduzirTags.Checked;
end;

procedure TFrmACBrMonitor.cbIgnorarTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.IgnorarTags := cbIgnorarTags.Checked;
end;

procedure TFrmACBrMonitor.cbHRIChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.MostrarCodigo := cbHRI.Checked;
end;

procedure TFrmACBrMonitor.cbMonitorarPastaChange(Sender: TObject);
begin
  if cbMonitorarPasta.Checked then
  begin
    if MessageDlg(
      'Ao ativar esta opção, TODOS os arquivos do diretório serão lidos e apagados.' + sLineBreak +
      'Deseja realmente continuar?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      cbMonitorarPasta.Checked := False;
  end;
end;

procedure TFrmACBrMonitor.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
    begin
      ACBrNFe1.Configuracoes.Geral.SSLLib    := TSSLLib(cbSSLLib.ItemIndex);
      ACBrCTe1.Configuracoes.Geral.SSLLib    := TSSLLib(cbSSLLib.ItemIndex);
      ACBrMDFe1.Configuracoes.Geral.SSLLib   := TSSLLib(cbSSLLib.ItemIndex);
      ACBrGNRE1.Configuracoes.Geral.SSLLib   := TSSLLib(cbSSLLib.ItemIndex);
      ACBrBlocoX1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
    end;
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFrmACBrMonitor.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
    SetDFeSSLType;
end;

procedure TFrmACBrMonitor.ACT_ButtonMouseEnter(Sender: TObject);
begin
  if (TPanel(Sender).Color = TCores.Buttons) or
     (TPanel(Sender).Color = TCores.SubButtons) then
  begin
     TPanel(Sender).Color := clGray;
  end;
end;

procedure TFrmACBrMonitor.ACT_ButtonMouseLeave(Sender: TObject);
begin
  if (TPanel(Sender).Color = TCores.ButtonMouseEnter) or
     (TPanel(Sender).Color = TCores.SubButtonMouseEnter) then
  begin
    if TPanel(Sender).Tag = 0 then
       TPanel(Sender).Color := TCores.Buttons
    else
       TPanel(Sender).Color := TCores.SubButtons;
  end;
end;

function TFrmACBrMonitor.IsVisible: Boolean;
begin
  Result := IsControlVisible and ((Parent = nil) or (Parent.IsVisible));
end;

procedure TFrmACBrMonitor.SetColorButtons(Sender: TObject);
var
  iFor: Integer;
begin
   for iFor := 0 to ScrollBox.ControlCount -1 do
   begin
     if TPanel(ScrollBox.Controls[iFor]).Tag = 0 then
        TPanel(ScrollBox.Controls[iFor]).Color := TCores.Buttons
     else
     if TPanel(ScrollBox.Controls[iFor]).Tag = 9 then
     begin
       if ScrollBox.Controls[iFor] is TPanel then
          SetPanel(TPanel(ScrollBox.Controls[iFor]));
     end;
   end;
   TPanel(Sender).Color := TCores.ButtonSelected;
end;

procedure TFrmACBrMonitor.SetPanel(Sender: TPanel);
var
  iFor: Integer;
begin
  for iFor := 0 to Sender.ControlCount -1 do
  begin
    if Sender.Controls[iFor].Tag = 1 then
    begin
       Sender.Controls[iFor].Color := TCores.SubButtons;
       Sender.Controls[iFor].Height := 0;
       Sender.Controls[iFor].Font.Bold := False;
    end
    else
    if Sender.Controls[iFor].Tag = 0 then
       Sender.Controls[iFor].Color := TCores.Buttons;
  end;
end;

procedure TFrmACBrMonitor.SetSize25(Sender: TObject);
var
  iFor: Integer;
begin
  for iFor := 0 to TPanel(Sender).ControlCount -1 do
  begin
    if TPanel(Sender).Controls[iFor].Tag = 1 then
    begin
       TPanel(Sender).Controls[iFor].Height := 25;
    end;
  end;
end;

procedure TFrmACBrMonitor.SetScroll(Sender: TObject);
begin
  ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Position+TPanel(Sender).Height;
end;

procedure TFrmACBrMonitor.MostraLogoBanco;
var
  Banco: TACBrBanco;
begin
  try
    try
      Banco := TACBrBanco.Create(ACBrBoleto1);
      Banco.TipoCobranca := TACBrTipoCobranca(cbxBOLBanco.ItemIndex);

      pnLogoBanco.Caption := '';
      imgLogoBanco.Picture.LoadFromFile(deBOLDirLogo.Text + PathDelim +
        IntToStrZero(Banco.Numero, 3)+'.bmp');
    except
      pnLogoBanco.Caption := 'Sem logo';
      imgLogoBanco.Picture.Clear;
    end;
  finally
    Banco.Free;
  end;
end;

procedure TFrmACBrMonitor.CarregaArquivosRetorno;
var
  Item: TListItem;
  Rec: TRawByteSearchRec;
begin
  if FindFirst(deBolDirRetornoRel.Text + '\*.ret', faAnyFile, Rec) = 0 then
  begin
    try
      lsvArqsRetorno.Items.Clear;

      repeat
        if (Rec.Name = '.') or (Rec.Name = '..') then
          continue;

        Item := lsvArqsRetorno.Items.Add;
        Item.Caption := Rec.Name;

      until FindNext(Rec) <> 0;
    finally
      SysUtils.FindClose(Rec);
    end;
  end;
end;

procedure TFrmACBrMonitor.DefineTextoTrayTitulo;
begin
  TrayIcon1.Hint := 'ACBrMonitor PLUS' + sVersaoACBr;
  TrayIcon1.BalloonTitle := TrayIcon1.Hint;
  TrayIcon1.BalloonHint := 'Projeto ACBr' + sLineBreak + 'http://acbr.sf.net';

  FrmACBrMonitor.Caption := ' ACBrMonitorPLUS ' + sVersaoACBr + ' ';

  try
    if (chkExibeRazaoSocial.Checked and Assigned(ACBrNFe1.SSL)) then
      if NaoEstaVazio( ACBrNFe1.SSL.CertRazaoSocial) then
      begin
        TrayIcon1.Hint := TrayIcon1.Hint + sLineBreak +
          ACBrNFe1.SSL.CertRazaoSocial;
        FrmACBrMonitor.Caption := FrmACBrMonitor.Caption + ' [' +
          ACBrNFe1.SSL.CertRazaoSocial+']';
      end;
  except
    AddLinesLog('Erro ao obter dados do certificado digital.');
  end;
end;

procedure TFrmACBrMonitor.SetDFeSSLType;
begin
  ACBrNFe1.SSL.SSLType    := TSSLType( cbSSLType.ItemIndex );
  ACBrCTe1.SSL.SSLType    := TSSLType( cbSSLType.ItemIndex );
  ACBrMDFe1.SSL.SSLType   := TSSLType( cbSSLType.ItemIndex );
  ACBrGNRE1.SSL.SSLType   := TSSLType( cbSSLType.ItemIndex );
  ACBrBlocoX1.SSL.SSLType := TSSLType( cbSSLType.ItemIndex );
end;

procedure TFrmACBrMonitor.SetColorSubButtons(Sender: TObject);
var
  iFor: Integer;
begin
   for iFor := 0 to TPanel(Sender).Parent.ControlCount -1 do
   begin
     if TPanel(Sender).Parent.Controls[iFor].Tag = 1 then
     begin
        TPanel(Sender).Parent.Controls[iFor].Color := TCores.SubButtons;
        TPanel(Sender).Parent.Controls[iFor].Font.Bold := False;
     end;
   end;
   TPanel(Sender).Font.Bold := True;

end;

end.
