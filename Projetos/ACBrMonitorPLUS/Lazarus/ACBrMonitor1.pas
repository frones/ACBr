{*******************************************************************************}
{ Projeto: ACBrMonitor                                                          }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: 2005 Fábio Rogério Baía                          }
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

{$mode objfpc}{$H+}

unit ACBrMonitor1;

interface

uses
  {$IFDEF MSWINDOWS} windows, {$ENDIF}
  SysUtils, Classes, Forms, CmdUnit, ACBrECF, ACBrDIS, ACBrGAV, ACBrDevice,
  ACBrCHQ, ACBrLCB, ACBrRFD, Dialogs, ExtCtrls, Menus, Buttons, StdCtrls,
  FileUtil, ComCtrls, Controls, Graphics, Spin, MaskEdit, EditBtn, ACBrBAL,
  ACBrETQ, ACBrPosPrinter, ACBrSocket, ACBrCEP, ACBrIBGE, blcksock,
  ACBrValidador, ACBrGIF, ACBrEAD, ACBrMail, ACBrSedex, ACBrNCMs,
  ACBrConsultaCNPJ, ACBrConsultaCPF, ACBrNFe, ACBrNFeDANFeESCPOS,
  ACBrDANFCeFortesFr, ACBrDANFCeFortesFrA4, ACBrNFeDANFeRLClass, ACBrBoleto,
  ACBrBoletoFCFortesFr, Printers, DbCtrls, DBGrids, LazHelpHTML,
  SynHighlighterXML, SynMemo, PrintersDlgs, IpHtml, TreeFilterEdit,
  ACBrNFSeXConversao,
  pcnConversao, pcnConversaoNFe, pcteConversaoCTe, pcnConversaoBPe, ACBrSAT,
  ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr, ACBrSATClass, pcnRede,
  pgnreConversao, ACBrDFeSSL, ACBrGNRE2, ACBrGNReGuiaRLClass, ACBrBlocoX,
  ACBrMDFe, ACBrMDFeDAMDFeRLClass, ACBrCTe, ACBrCTeDACTeRLClass, types,
  fileinfo, ACBrDFeConfiguracoes, ACBrBPe, ACBrBPeDABPeESCPOS, ACBrReinf,
  ACBreSocial, ACBrIntegrador, pmdfeConversaoMDFe, pcesConversaoeSocial,
  pcnConversaoReinf, ACBrMonitorConfig, ACBrMonitorConsts, DOACBrNFeUnit,
  DoACBrCTeUnit, DoACBrMDFeUnit, DoBoletoUnit, DoACBrReinfUnit, DoBALUnit,
  DoEmailUnit, DoCEPUnit, DoCHQUnit, DoGAVUnit, DoIBGEUnit, DoNcmUnit,
  DoLCBUnit, DoDISUnit, DoSedexUnit, DoETQUnit, DoACBrGNReUnit,
  DoPosPrinterUnit, DoECFUnit, DoECFObserver, DoECFBemafi32, DoSATUnit,
  DoACBreSocialUnit, DoACBrBPeUnit, ACBrLibResposta, DoACBrUnit, DoCNPJUnit,
  DoCPFUnit, ACBrBoletoConversao, FormConsultaCNPJ, ACBrMonitorMenu,
  ACBrDFeReport, ACBrNFSeX, ACBrNFSeXDANFSeRLClass,
  DoACBrNFSeUnit, ACBrGTIN, DoACBrGTINUnit,
  ACBrPIXBase;

const
  CEstados: array[TACBrECFEstado] of string =
    ('Não Inicializada', 'Desconhecido', 'Livre', 'Venda',
    'Pagamento', 'Relatório', 'Bloqueada', 'Requer Z', 'Requer X',
    'Não Fiscal');

  CDFeNaoSobreescrever: array[0..7] of String =
    ( 'libltdl-7.dll', 'libwinpthread-1.dll',
      'zlib1.dll', 'iconv.dll',
      'bemasat.xml', 'gersat.conf', 'sat.ini', 'satdimep.ini' );

  CBufferMemoResposta = 10000;              { Maximo de Linhas no MemoResposta }
  //_C = 'tYk*5W@';
  UTF8BOM : AnsiString = #$EF#$BB#$BF;

  HelpShortcut = 'F1';
  ClHelp = 'lhelp';
  CMODELO_NFCE_FORTES = 0;
  CMODELO_NFCE_ESCPOS = 1;
  CIMPRESSAO_NFCE_A4 = 0;
  CIMPRESSAO_NFCE_BOBINA = 1;
  CSATManual = 'Manual';

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
                  tDFeGNRe, tDFeBPe, tDFeEventoBPe);

  { TFrmACBrMonitor }

  TFrmACBrMonitor = class(TForm)
    ACBrBlocoX1: TACBrBlocoX;
    ACBrBoleto1: TACBrBoleto;
    ACBrBoletoFCFortes1: TACBrBoletoFCFortes;
    ACBrBPe1: TACBrBPe;
    ACBrBPeDABPeESCPOS1: TACBrBPeDABPeESCPOS;
    ACBrCEP1: TACBrCEP;
    ACBrConsultaCNPJ1: TACBrConsultaCNPJ;
    ACBrConsultaCPF1: TACBrConsultaCPF;
    ACBrCTe1: TACBrCTe;
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;
    ACBrEAD1: TACBrEAD;
    ACBrECF1: TACBrECF;
    ACBreSocial1: TACBreSocial;
    ACBrGIF1: TACBrGIF;
    ACBrGNRE1: TACBrGNRE;
    ACBrGNREGuiaRL1: TACBrGNREGuiaRL;
    ACBrGTIN1: TACBrGTIN;
    ACBrIBGE1: TACBrIBGE;
    ACBrIntegrador1: TACBrIntegrador;
    ACBrMail1: TACBrMail;
    ACBrMDFe1: TACBrMDFe;
    ACBrMDFeDAMDFeRL1: TACBrMDFeDAMDFeRL;
    ACBrNCMs1: TACBrNCMs;
    ACBrNFe1: TACBrNFe;
    ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes;
    ACBrNFeDANFCeFortesA4_1: TACBrNFeDANFCeFortesA4;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrNFeDANFeRL1: TACBrNFeDANFeRL;
    ACBrNFSeX1: TACBrNFSeX;
    ACBrNFSeXDANFSeRL1: TACBrNFSeXDANFSeRL;
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrReinf1: TACBrReinf;
    ACBrSAT1: TACBrSAT;
    ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS;
    ACBrSATExtratoFortes1: TACBrSATExtratoFortes;
    ACBrSedex1: TACBrSedex;
    ACBrValidador1: TACBrValidador;
    ApplicationProperties1: TApplicationProperties;
    bBALAtivar: TBitBtn;
    bBALTestar: TBitBtn;
    bbAtivar: TBitBtn;
    bBoletoRelatorioRetorno: TBitBtn;
    bBOLLerArqRelatorio: TBitBtn;
    bCEPTestar: TButton;
    bCHQTestar: TBitBtn;
    bDISAnimar: TBitBtn;
    bDISLimpar: TBitBtn;
    bDISTestar: TBitBtn;
    bECFAtivar: TBitBtn;
    bECFLeituraX: TBitBtn;
    bECFTestar: TBitBtn;
    bEmailTestarConf: TBitBtn;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    bExecECFTeste: TBitBtn;
    bGAVAbrir: TBitBtn;
    bGAVAtivar: TBitBtn;
    bGAVEstado: TBitBtn;
    bIBGETestar: TButton;
    bImpressora: TButton;
    bInicializar: TButton;
    bLCBAtivar: TBitBtn;
    bLCBSerial: TBitBtn;
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
    btCertInfo: TBitBtn;
    btConsultarStatusOPSAT: TButton;
    btnBoletoRelatorioRetorno: TPanel;
    btnCancelarCTe: TButton;
    btnCancMDFe: TButton;
    btnCancNF: TButton;
    btnCancNFeSubs: TButton;
    btNCMConsultar: TBitBtn;
    btNCMSalvarArquivo: TBitBtn;
    btNCMValidadeHelp: TBitBtn;
    btnConsultar: TButton;
    btnConsultarCTe: TButton;
    btnConsultarMDFe: TButton;
    btnDFeRespTecnico: TPanel;
    btnEnviar: TButton;
    btnEnviarCTe: TButton;
    btnEnviarEmail: TButton;
    btnEnviarEmailCTe: TButton;
    btnEnviarEmailMDFe: TButton;
    btnEnviarMDFe: TButton;
    btnFonteItens: TButton;
    btnGerarAssinaturaSAT: TButton;
    btnImprimir: TButton;
    btnImprimirCTe: TButton;
    btnImprimirMDFe: TButton;
    btnIntegrador: TPanel;
    btnInutilizar: TButton;
    btnInutilizarCTe: TButton;
    btnSATEMAIL: TPanel;
    btnBalanca: TPanel;
    btnBoleto: TPanel;
    btnBoletoBeneficiario: TPanel;
    btnBoletoCont: TPanel;
    btnBoletoConta: TPanel;
    btnBoletoEmail: TPanel;
    btnBoletoLeiaute: TPanel;
    btnBoletoRR: TPanel;
    btnCadastro: TPanel;
    btnCadastroCont: TPanel;
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
    btnEtiqueta: TPanel;
    btnGaveta: TPanel;
    btnImpCheque: TPanel;
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
    btnConsultarGTIN: TButton;
    btnTC: TPanel;
    btnUser: TPanel;
    btnValidarXML: TButton;
    btnValidarXMLCTe: TButton;
    btnValidarXMLMDFe: TButton;
    btnVersaoSSL: TBitBtn;
    btSATAssocia: TButton;
    btSATConfigRede: TButton;
    btStatusServico: TBitBtn;
    bvCadastro: TBevel;
    bvCadastro2: TBevel;
    bvCadastro3: TBevel;
    bvCadastro4: TBevel;
    bvCadastro5: TBevel;
    bvCadastro6: TBevel;
    bvCadastro7: TBevel;
    cbAbas: TCheckBox;
    cbBackFeed: TComboBox;
    cbBALModelo: TComboBox;
    cbBALPorta: TComboBox;
    cbCEPWebService: TComboBox;
    cbCHQModelo: TComboBox;
    cbCHQPorta: TComboBox;
    cbComandos: TCheckBox;
    cbControlePorta: TCheckBox;
    cbCortarPapel: TCheckBox;
    cbCryptLib: TComboBox;
    cbDISModelo: TComboBox;
    cbDISPorta: TComboBox;
    cbDPI: TComboBox;
    cbECFModelo: TComboBox;
    cbECFPorta: TComboBox;
    cbEmailCodificacao: TComboBox;
    cbEmailConfirmation: TCheckBox;
    cbEmailHTML: TCheckBox;
    cbEmailSsl: TCheckBox;
    cbEmailThread: TCheckBox;
    cbEmailTls: TCheckBox;
    cbEscPosImprimirLogo: TCheckBox;
    cbETQModelo: TComboBox;
    cbETQPorta: TComboBox;
    cbFormaEmissaoBPe: TComboBox;
    cbFormaEmissaoCTe: TComboBox;
    cbFormaEmissaoGNRe: TComboBox;
    cbFormaEmissaoMDFe: TComboBox;
    cbFormaEmissaoNFe: TComboBox;
    cbFormatoDecimais: TComboBox;
    cbGAVAcaoAberturaAntecipada: TComboBox;
    cbGavetaSinalInvertido: TCheckBox;
    cbGAVModelo: TComboBox;
    cbGAVPorta: TComboBox;
    cbGAVStrAbre: TComboBox;
    cbHRI: TCheckBox;
    cbHttpLib: TComboBox;
    cbHttpLibBoleto: TComboBox;
    cbIgnorarTags: TCheckBox;
    cbLayoutNFSe: TComboBox;
    cbMunicipio: TComboBox;
    cbLCBDispositivo: TComboBox;
    cbLCBPorta: TComboBox;
    cbLCBSufixo: TComboBox;
    cbLCBSufixoLeitor: TComboBox;
    cbLog: TCheckBox;
    cbLogComp: TCheckBox;
    cbModoEmissao: TCheckBox;
    cbMonitorarPasta: TCheckBox;
    cbMostrarNaBarraDeTarefas: TCheckBox;
    cbOrigem: TComboBox;
    cbPreview: TCheckBox;
    cbRetirarAcentos: TCheckBox;
    cbRetirarAcentosNaResposta: TCheckBox;
    cbRetirarEspacos: TCheckBox;
    cbRFDModelo: TComboBox;
    cbSATMarca: TComboBox;
    cbSenha: TCheckBox;
    cbSSLLib: TComboBox;
    cbSSLType: TComboBox;
    cbSSLTypeBoleto: TComboBox;
    cbOperacaoBoleto: TComboBox;
    cbTagRejeicao938: TComboBox;
    cbTipoContribuinte: TComboBox;
    cbTipoEmpregador: TComboBox;
    cbTipoResposta: TComboBox;
    cbTraduzirTags: TCheckBox;
    cbUF: TComboBox;
    cbUmaInstancia: TCheckBox;
    cbUnidade: TComboBox;
    cbUsarEscPos: TRadioButton;
    cbUsarFortes: TRadioButton;
    cbValidarDigest: TCheckBox;
    cbVersaoWS: TComboBox;
    cbVersaoWSBPe: TComboBox;
    cbVersaoWSCTe: TComboBox;
    cbVersaoWSeSocial: TComboBox;
    cbVersaoWSGNRE: TComboBox;
    cbVersaoWSMDFe: TComboBox;
    cbVersaoWSQRCode: TComboBox;
    cbVersaoWSReinf: TComboBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxAjustarAut: TCheckBox;
    cbxMontarPathSchemas: TCheckBox;
    cbxAmbiente: TComboBox;
    cbxAtualizarXMLCancelado: TCheckBox;
    cbxBOLBanco: TComboBox;
    cbxBOLEmissao: TComboBox;
    cbxBOLFiltro: TComboBox;
    cbxBOLF_J: TComboBox;
    cbxBOLImpressora: TComboBox;
    cbxBOLLayout: TComboBox;
    cbxBOLTipoChavePix: TComboBox;
    cbxBOLUF: TComboBox;
    cbxCNAB: TComboBox;
    cbxEmissaoPathNFe: TCheckBox;
    cbxEmitCidade: TComboBox;
    cbxExibeResumo: TCheckBox;
    cbxExibeTotalTributosItem: TCheckBox;
    cbxExibirCampoFatura: TCheckBox;
    cbxExibirEAN: TCheckBox;
    cbxExibirLogoEmCima: TCheckBox;
    cbxExpandirDadosAdicionaisAuto: TCheckBox;
    cbxExpandirLogo: TCheckBox;
    cbxFormatXML: TCheckBox;
    cbxFormCont: TCheckBox;
    cbxImpDescPorc: TCheckBox;
    cbxImpDetEspNFe: TCheckBox;
    cbxImpDocsReferenciados: TCheckBox;
    cbxImprimeXPedNitemPed: TCheckBox;
    cbxImpressora: TComboBox;
    cbxImpressoraNFCe: TComboBox;
    cbxImprimeContinuacaoDadosAdicionaisPrimeiraPagina: TCheckBox;
    cbxImprimirCodEANitemSAT: TCheckBox;
    cbxImprimirCodigoEANNFCe: TCheckBox;
    cbxImprimirDescAcresItemNFCe: TCheckBox;
    cbxImprimirDescAcresItemSAT: TCheckBox;
    cbxImprimirItem1LinhaNFCe: TCheckBox;
    cbxImprimirItem1LinhaSAT: TCheckBox;
    cbxConsultarLoteAposEnvio: TCheckBox;
    cbxConsultarAposCancelar: TCheckBox;
    cbxMostrarStatusSAT: TCheckBox;
    cbxImprimirLogoLateralNFCe: TCheckBox;
    cbxImprimeItens: TCheckBox;
    cbxImprimirNomeFantasiaNFCe: TCheckBox;
    cbxImprimirQRCodeLateralNFCe: TCheckBox;
    cbxImprimirTributos: TCheckBox;
    cbxImpValLiq: TCheckBox;
    cbxIndRatISSQN: TComboBox;
    cbxLogoLateral: TCheckBox;
    cbXMLSignLib: TComboBox;
    cbxModelo: TComboBox;
    cbxModeloSAT: TComboBox;
    cbxMostrarPreview: TCheckBox;
    cbxMostraStatus: TCheckBox;
    cbxNormatizarMunicipios: TCheckBox;
    cbxPagCodigo: TComboBox;
    cbxPastaMensal: TCheckBox;
    cbxPorta: TComboBox;
    cbxQRCodeLateral: TCheckBox;
    cbxQuebrarLinhasDetalhesItens: TCheckBox;
    cbxRedeProxy: TComboBox;
    cbxRedeSeg: TComboBox;
    cbxRegTribISSQN: TComboBox;
    cbxRegTributario: TComboBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSalvarArqs: TCheckBox;
    cbxSalvarNFesProcessadas: TCheckBox;
    cbxSATSalvarCFe: TCheckBox;
    cbxSATSalvarCFeCanc: TCheckBox;
    cbxSATSalvarEnvio: TCheckBox;
    cbxSATSepararPorANO: TCheckBox;
    cbxSATSepararPorCNPJ: TCheckBox;
    cbxSATSepararPorDIA: TCheckBox;
    cbxSATSepararPorMES: TCheckBox;
    cbxSATSepararPorModelo: TCheckBox;
    cbxSedexAvisoReceb: TComboBox;
    cbxSedexFormato: TComboBox;
    cbxSedexMaoPropria: TComboBox;
    cbxSedexServico: TComboBox;
    cbxSepararPorCNPJ: TCheckBox;
    cbxSepararporModelo: TCheckBox;
    cbxSepararPorNome: TCheckBox;
    cbxTCModelo: TComboBox;
    cbxTimeZoneMode: TComboBox;
    cbxUnComTributavel: TComboBox;
    cbxUsarSeparadorPathPDF: TCheckBox;
    cbxUTF8: TCheckBox;
    cbxValidarNumeroSessaoResposta: TCheckBox;
    chbArqEntANSI: TCheckBox;
    chbArqSaiANSI: TCheckBox;
    chbTCPANSI: TCheckBox;
    chCHQVerForm: TCheckBox;
    chECFArredondaMFD: TCheckBox;
    chECFArredondaPorQtd: TCheckBox;
    chECFControlePorta: TCheckBox;
    chECFDescrGrande: TCheckBox;
    chECFIgnorarTagsFormatacao: TCheckBox;
    chECFSinalGavetaInvertido: TCheckBox;
    cbNCMForcarDownload: TCheckBox;
    cbxImprimeInscSuframa: TCheckBox;
    ckbExibirMunicipioDescarregamento: TCheckBox;
    ChkPix: TCheckBox;
    chgDescricaoPagamento: TCheckGroup;
    chkBOLRelMostraPreview: TCheckBox;
    chkExibeRazaoSocial: TCheckBox;
    chkLerBeneficiarioRetorno: TCheckBox;
    cbxBOLEmailMensagemHTML: TCheckBox;
    chkMostraLogNaTela: TCheckBox;
    ChkLogBoletoWeb: TCheckBox;
    chkRemoveAcentos: TCheckBox;
    chkVerificarValidadeCertificado: TCheckBox;
    chLCBExcluirSufixo: TCheckBox;
    chRFD: TCheckBox;
    chRFDIgnoraMFD: TCheckBox;
    ckCamposFatObrigatorio: TCheckBox;
    ckgBOLMostrar: TCheckGroup;
    ckIBGEAcentos: TCheckBox;
    ckIBGEUTF8: TCheckBox;
    ckMemoria: TCheckBox;
    ckNFCeUsarIntegrador: TCheckBox;
    ckSalvar: TCheckBox;
    deBOLDirArquivo: TDirectoryEdit;
    deBOLDirLogo: TDirectoryEdit;
    deBolDirRemessa: TDirectoryEdit;
    deBolDirRetorno: TDirectoryEdit;
    deBolDirRetornoRel: TDirectoryEdit;
    deRFDDataSwBasico: TDateEdit;
    deUSUDataCadastro: TDateEdit;
    eAvanco: TEdit;
    eCopias: TEdit;
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
    edConsultarGTIN: TEdit;
    edtArquivoWebServicesNFSe: TEdit;
    edtBOLChavePix: TEdit;
    edtBolArquivoKey: TEdit;
    edtBolArquivoCRT: TEdit;
    edtCodigoCidade: TEdit;
    edtCNPJPrefeitura: TEdit;
    edtNomeCidade: TEdit;
    edtUFCidade: TEdit;
    edtEmailAssuntoNFSe: TEdit;
    edtLogoMarcaPrefeitura: TEdit;
    edtNomeEmitenteNFSe: TEdit;
    edtFraseSecretaNFSe: TEdit;
    edtChaveAutenticacaoNFSe: TEdit;
    edtIMEmitenteNFSe: TEdit;
    edtNomePrefeitura: TEdit;
    edtUsuarioNFSe: TEdit;
    edtSenhaNFSe: TEdit;
    edtChaveAcessoNFSe: TEdit;
    edtCNPJEmitenteNFSe: TEdit;
    edtVersaoArquivo: TEdit;
    edtVersaoLote: TEdit;
    edNCMCodigo: TEdit;
    edNCMDiasValidade: TSpinEdit;
    edNCMDiretorio: TDirectoryEdit;
    edtOperacaoBeneficiario: TEdit;
    edLCBPreExcluir: TEdit;
    edLogArq: TEdit;
    edLogComp: TEdit;
    edMFEInput: TEdit;
    edMFEOutput: TEdit;
    edNomeArquivo: TEdit;
    edNomeDLL: TEdit;
    edPortaTCP: TEdit;
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
    edRFDDir: TEdit;
    edSaiTXT: TEdit;
    edSATLog: TEdit;
    edSATPathArqs: TEdit;
    edSATPathArqsCanc: TEdit;
    edSATPathArqsEnvio: TEdit;
    edSATPrefixoCFe: TEdit;
    edSATPrefixoCFeCanc: TEdit;
    edSenha: TEdit;
    edSH_Aplicativo: TEdit;
    edSH_CNPJ: TEdit;
    edSH_COO: TEdit;
    edSH_IE: TEdit;
    edSH_IM: TEdit;
    edSH_Linha1: TEdit;
    edSH_Linha2: TEdit;
    edSH_NumeroAP: TEdit;
    edSH_RazaoSocial: TEdit;
    edSH_Site: TEdit;
    edSH_VersaoAP: TEdit;
    edtAguardar: TEdit;
    edtArquivoPFX: TEdit;
    edtArquivoWebServicesBPe: TEdit;
    edtArquivoWebServicesCTe: TEdit;
    edtArquivoWebServiceseSocial: TEdit;
    edtArquivoWebServicesGNRe: TEdit;
    edtArquivoWebServicesMDFe: TEdit;
    edtArquivoWebServicesNFe: TEdit;
    edtArquivoWebServicesReinf: TEdit;
    edtBOLAgencia: TEdit;
    edtBOLBairro: TEdit;
    edtBOLCEP: TMaskEdit;
    edtBOLCNPJ: TMaskEdit;
    edtBOLComplemento: TEdit;
    edtBOLConta: TEdit;
    edtBOLDigitoAgConta: TEdit;
    edtBOLDigitoAgencia: TEdit;
    edtBOLDigitoConta: TEdit;
    edtBOLEmailAssunto: TEdit;
    edtBOLEmailMensagem: TMemo;
    edtBOLLocalPagamento: TEdit;
    edtBOLLogoEmpresa: TEdit;
    edtBOLLogradouro: TEdit;
    edtBOLNumero: TEdit;
    edtBOLRazaoSocial: TEdit;
    edTCArqPrecos: TEdit;
    edtClientID: TEdit;
    edtClientSecret: TEdit;
    edtPrefixRemessa: TEdit;
    edtKeyUser: TEdit;
    edTCNaoEncontrado: TEdit;
    edtCNPJContador: TEdit;
    edtCodCliente: TEdit;
    edtCodigoAtivacao: TEdit;
    edtCodTransmissao: TEdit;
    edtCodUF: TEdit;
    edtConvenio: TEdit;
    edTCPort: TEdit;
    edtCSRT: TEdit;
    edtEmailAssuntoCTe: TEdit;
    edtEmailAssuntoMDFe: TEdit;
    edtEmailAssuntoNFe: TEdit;
    edtEmailAssuntoSAT: TEdit;
    edtEmailEmpresa: TEdit;
    edtEmitCNPJ: TEdit;
    edtEmitIE: TEdit;
    edtEmitIM: TEdit;
    edtIDContribuinte: TEdit;
    edtIdCSRT: TEdit;
    edtIDEmpregador: TEdit;
    edtIdToken: TEdit;
    edtIDTransmissor: TEdit;
    edtIDTransmissorReinf: TEdit;
    edTimeOutTCP: TEdit;
    edTimeZoneStr: TEdit;
    edtIntervalo: TEdit;
    edtScope: TEdit;
    edtLogoMarca: TEdit;
    edtLogoMarcaNFCeSAT: TEdit;
    edtModalidade: TEdit;
    edtMsgResumoCanhoto: TEdit;
    edtNumCopia: TSpinEdit;
    edtNumCopiaNFCe: TSpinEdit;
    edtNumeroSerie: TEdit;
    edtPathArqTXT: TEdit;
    edtPathDownload: TEdit;
    edtPathDPEC: TEdit;
    edtPathEvento: TEdit;
    edtPathInu: TEdit;
    edtPathLogs: TEdit;
    edtPathNFe: TEdit;
    edtPathPDF: TEdit;
    edtPathSchemasDFe: TEdit;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    edtSATCasasMaskQtd: TEdit;
    edtSATMaskVUnit: TEdit;
    edtVersaoBoleto: TEdit;
    edtPathLogBoleto: TEdit;
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
    edtSenha: TEdit;
    edtSiteEmpresa: TEdit;
    edtSwHAssinatura: TEdit;
    edtSwHCNPJ: TEdit;
    edtTentativas: TEdit;
    edtTimeoutWebServices: TSpinEdit;
    edtTimeoutWebServicesBoleto: TSpinEdit;
    edtToken: TEdit;
    edtURLPFX: TEdit;
    edUSUCNPJ: TEdit;
    edUSUEndereco: TEdit;
    edUSUIE: TEdit;
    edUSURazaoSocial: TEdit;
    eMargemEsquerda: TEdit;
    eTemperatura: TEdit;
    eVelocidade: TEdit;
    fspeLarguraNFCe: TSpinEdit;
    fspeMargemDir: TFloatSpinEdit;
    fspeMargemEsq: TFloatSpinEdit;
    fspeMargemInf: TFloatSpinEdit;
    fspeMargemSup: TFloatSpinEdit;
    fspeNFCeMargemDir: TFloatSpinEdit;
    fspeNFCeMargemEsq: TFloatSpinEdit;
    fspeNFCeMargemInf: TFloatSpinEdit;
    fspeNFCeMargemSup: TFloatSpinEdit;
    gbCEP: TGroupBox;
    gbCEPProxy: TGroupBox;
    gbCEPTestar: TGroupBox;
    gbCHQDados: TGroupBox;
    gbCodBarras: TGroupBox;
    gbConfigImp: TGroupBox;
    gbConfiguracao: TGroupBox;
    gbDANFeESCPOS: TGroupBox;
    gbDFeConfDiversas: TGroupBox;
    gbDFeTimeZone: TGroupBox;
    gbEmailDados: TGroupBox;
    gbExtratoSAT: TGroupBox;
    gbGavetaConfig: TGroupBox;
    gbGeral: TGroupBox;
    gbImpressao: TGroupBox;
    gbIPFix: TGroupBox;
    gbLog: TGroupBox;
    gbLogComp: TGroupBox;
    gbLogotipo: TGroupBox;
    gbNCMConsultar: TGroupBox;
    gbNCMSalvarArquivo: TGroupBox;
    gbPPPoE: TGroupBox;
    gbProxy: TGroupBox;
    gbQRCode: TGroupBox;
    gbRFDECF: TGroupBox;
    gbSenha: TGroupBox;
    gbTCP: TGroupBox;
    gbTXT: TGroupBox;
    gbWiFi: TGroupBox;
    gbxEmitenteNFSe: TGroupBox;
    gbxLauoutMunicipio: TGroupBox;
    gbxConfeSocial: TGroupBox;
    gbxConfigSSL: TGroupBox;
    gbxConfReinf: TGroupBox;
    gbxAutenticacaoNFSe: TGroupBox;
    gbxMargem: TGroupBox;
    gbxMargem1: TGroupBox;
    gbxProxy: TGroupBox;
    gbxRegrasGeral: TTabSheet;
    gbxRetornoEnvio: TGroupBox;
    gbxWSeSocial: TTabSheet;
    gbxWSNFe: TGroupBox;
    gbxWSReinf: TTabSheet;
    grbConfigArqs: TGroupBox;
    grbPathSchemas: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    GroupBox13: TGroupBox;
    GrbVersaoDFe: TGroupBox;
    GrbDadosBeneficiarioBoletoWeb: TGroupBox;
    grbWsConfig: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    HTMLBrowserHelpViewer1: THTMLBrowserHelpViewer;
    HTMLHelpDatabase1: THTMLHelpDatabase;
    Image2: TImage;
    imgErrEmail_Mail: TImage;
    imgErrEmail_User: TImage;
    imgErrEmail_Smtp: TImage;
    imgErrEmail_Porta: TImage;
    imgErrEmail_Senha: TImage;
    imgErrEmail_Name: TImage;
    imgErrSATLibModelo: TImage;
    imgNetChat: TImage;
    imgNetDoc: TImage;
    imgExp: TImage;
    ImgCanal: TImage;
    ImgChat: TImage;
    ImgDocumentacao: TImage;
    Image7: TImage;
    ImageList2: TImageList;
    FontDialog1: TFontDialog;
    ImageACBr: TImage;
    imgErrCEP: TImage;
    imgErrCertificado: TImage;
    imgErrCidade: TImage;
    imgErrCNPJ: TImage;
    imgErrComunicacao: TImage;
    imgErrCNPJBoleto: TImage;
    imgErrCryptLib: TImage;
    imgErrEmail: TImage;
    imgErrHttpLib: TImage;
    imgErrPathSchemas: TImage;
    imgErrRazaoSocial: TImage;
    imgErrSAT: TImage;
    imgErrSATAssign: TImage;
    imgErrSATCodAtivacao: TImage;
    imgErrSATEmitente: TImage;
    imgErrSATCNPJSH: TImage;
    imgErrSATLib: TImage;
    imgErrSATPathVendas: TImage;
    imgErrSATInicializar: TImage;
    imgErrSATAtivar: TImage;
    imgErrSATPathCancelamento: TImage;
    imgErrSATPathEnvio: TImage;
    imgErrTokenCSC: TImage;
    imgErrTokenID: TImage;
    imgErrUF: TImage;
    imgErrWebServer: TImage;
    imgErrSSLLib: TImage;
    imgErrWebService: TImage;
    imgErrXmlSignLib: TImage;
    imgLogoBanco: TImage;
    imgNetCanal: TImage;
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
    Label109: TLabel;
    Label114: TLabel;
    Label118: TLabel;
    Label152: TLabel;
    Label260: TLabel;
    Label261: TLabel;
    Label262: TLabel;
    Label263: TLabel;
    Label264: TLabel;
    Label265: TLabel;
    Label266: TLabel;
    Label267: TLabel;
    Label268: TLabel;
    Label269: TLabel;
    Label270: TLabel;
    Label271: TLabel;
    Label272: TLabel;
    Label273: TLabel;
    Label274: TLabel;
    Label275: TLabel;
    Label276: TLabel;
    Label277: TLabel;
    Label278: TLabel;
    Label279: TLabel;
    Label280: TLabel;
    Label281: TLabel;
    Label282: TLabel;
    lbConsultarGTIN: TLabel;
    Label254: TLabel;
    lblBOLChavePix: TLabel;
    lblBOLTipoChavePix: TLabel;
    lblPrefixRemessa: TLabel;
    Label255: TLabel;
    Label256: TLabel;
    Label257: TLabel;
    Label258: TLabel;
    Label259: TLabel;
    lblPathLogBoleto: TLabel;
    labelbolcep: TLabel;
    lbNCMCarregando: TLabel;
    lbNCMDiasValidade: TLabel;
    lbNCMDiretorio: TLabel;
    lBolUF: TLabel;
    lBOLLogradouro: TLabel;
    Label11: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label119: TLabel;
    Label12: TLabel;
    Label120: TLabel;
    Label121: TLabel;
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
    Label133: TLabel;
    Label134: TLabel;
    Label135: TLabel;
    Label136: TLabel;
    Label137: TLabel;
    Label138: TLabel;
    Label139: TLabel;
    Label14: TLabel;
    Label140: TLabel;
    Label141: TLabel;
    Label142: TLabel;
    Label143: TLabel;
    Label144: TLabel;
    Label145: TLabel;
    Label146: TLabel;
    Label147: TLabel;
    Label148: TLabel;
    Label149: TLabel;
    Label15: TLabel;
    Label150: TLabel;
    Label151: TLabel;
    Label153: TLabel;
    Label154: TLabel;
    Label155: TLabel;
    Label156: TLabel;
    Label157: TLabel;
    Label158: TLabel;
    Label159: TLabel;
    Label16: TLabel;
    Label160: TLabel;
    Label161: TLabel;
    Label162: TLabel;
    Label163: TLabel;
    Label164: TLabel;
    Label165: TLabel;
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
    Label179: TLabel;
    Label18: TLabel;
    Label180: TLabel;
    Label181: TLabel;
    Label182: TLabel;
    Label183: TLabel;
    Label184: TLabel;
    Label185: TLabel;
    Label186: TLabel;
    Label187: TLabel;
    Label188: TLabel;
    Label189: TLabel;
    Label19: TLabel;
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
    Label2: TLabel;
    Label20: TLabel;
    Label200: TLabel;
    Label201: TLabel;
    Label202: TLabel;
    Label203: TLabel;
    Label204: TLabel;
    Label205: TLabel;
    Label206: TLabel;
    Label207: TLabel;
    Label208: TLabel;
    Label209: TLabel;
    Label21: TLabel;
    Label210: TLabel;
    Label211: TLabel;
    Label212: TLabel;
    Label213: TLabel;
    Label214: TLabel;
    Label215: TLabel;
    Label216: TLabel;
    Label217: TLabel;
    Label218: TLabel;
    Label219: TLabel;
    Label22: TLabel;
    Label220: TLabel;
    Label221: TLabel;
    Label222: TLabel;
    Label223: TLabel;
    Label224: TLabel;
    Label225: TLabel;
    Label226: TLabel;
    Label227: TLabel;
    Label228: TLabel;
    Label229: TLabel;
    Label23: TLabel;
    Label230: TLabel;
    Label231: TLabel;
    Label232: TLabel;
    Label233: TLabel;
    Label234: TLabel;
    Label235: TLabel;
    Label236: TLabel;
    Label237: TLabel;
    Label238: TLabel;
    Label239: TLabel;
    Label24: TLabel;
    Label240: TLabel;
    Label241: TLabel;
    Label242: TLabel;
    Label243: TLabel;
    Label244: TLabel;
    Label245: TLabel;
    Label246: TLabel;
    Label247: TLabel;
    Label248: TLabel;
    Label249: TLabel;
    Label25: TLabel;
    Label250: TLabel;
    Label251: TLabel;
    Label252: TLabel;
    Label253: TLabel;
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
    Label40: TLabel;
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
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label6: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
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
    Label81: TLabel;
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
    labelModeloDll: TLabel;
    LabelNomedll: TLabel;
    LabelpagCod: TLabel;
    lAdSufixo: TLabel;
    lbAltura: TLabel;
    lbAvanco: TLabel;
    lbBackFeed: TLabel;
    lbBackFeed1: TLabel;
    lbBuffer: TLabel;
    lbColunas: TLabel;
    lbCopias: TLabel;
    lbDPI: TLabel;
    lbEspacosLinhas: TLabel;
    lblAlturaCampos: TLabel;
    lbLargura: TLabel;
    lblArquivoPFX: TLabel;
    lblBOLAgencia: TLabel;
    lblBOLBairro: TLabel;
    lblBOLBanco: TLabel;
    lblBOLCidade: TLabel;
    lblBOLComplemento: TLabel;
    lblBOLConta: TLabel;
    lblBOLCPFCNPJ: TLabel;
    lblBOLDigAgencia: TLabel;
    lblBOLDigConta: TLabel;
    lblBOLDirLogo: TLabel;
    lblBOLEmissao: TLabel;
    lblBOLLogoEmpresa: TLabel;
    lblBOLLogradouro: TLabel;
    lblBOLNomeRazao: TLabel;
    lblBOLNumero: TLabel;
    lblBOLPessoa: TLabel;
    lblCSRT: TLabel;
    lblFonteEndereco: TLabel;
    lblIDCSRT: TLabel;
    lbLinhasPular: TLabel;
    lblMsgCanhoto: TLabel;
    lblNumeroSerie1: TLabel;
    lblogArquivo: TLabel;
    lbLogMaxLinhas: TLabel;
    lbLogoFatorX: TLabel;
    lbLogoFatorY: TLabel;
    lbLogoKC1: TLabel;
    lbLogoKC2: TLabel;
    lblSenha: TLabel;
    lbl_URL_Certificado: TLabel;
    lbMargem: TLabel;
    lbModelo: TLabel;
    lbPorPrinterLog: TLabel;
    lbPorta: TLabel;
    lbQRCodeErrorLevel: TLabel;
    lbQRCodeLargMod: TLabel;
    lbQRCodeTipo: TLabel;
    lbTemperatura: TLabel;
    lbTemperatura2: TLabel;
    lbTipoResp: TLabel;
    LCaption: TLabel;
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
    lsvArqsRetorno: TListView;
    lTimeOutTCP: TLabel;
    mBOLRelatorio: TMemo;
    mCmd: TMemo;
    meRFDHoraSwBasico: TMaskEdit;
    meUSUHoraCadastro: TMaskEdit;
    mmEmailMsgCTe: TMemo;
    mmEmailMsgMDFe: TMemo;
    mmEmailMsgNFe: TMemo;
    mmEmailMsgNFSe: TMemo;
    mmEmailMsgSAT: TMemo;
    mResp: TMemo;
    mResposta: TSynMemo;
    mRFDINI: TMemo;
    mRSAKey: TMemo;
    mTCConexoes: TMemo;
    pnTestesDFe: TPanel;
    pnTestesResposta: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlPesquisa: TPanel;
    PanelStart: TPanel;
    PanelMenu: TPanel;
    PanelScroll: TPanel;
    PanelTitle: TPanel;
    pbEmailTeste: TProgressBar;
    pCmd: TPanel;
    pConfig: TPanel;
    edtBOLCodCidade: TPanel;
    pgBoleto: TPageControl;
    pgCadastro: TPageControl;
    pgConfig: TPageControl;
    pgConRFD: TPageControl;
    pgDFe: TPageControl;
    pgECFParams: TPageControl;
    pgEmailDFe: TPageControl;
    pgImpressaoDFe: TPageControl;
    pgSAT: TPageControl;
    pgSwHouse: TPageControl;
    pgTestes: TPageControl;
    pgTipoWebService: TPageControl;
    pnLogoBanco: TPanel;
    pnNCMCarregando: TPanel;
    pRespostas: TPanel;
    pTopCmd: TPanel;
    pTopRespostas: TPanel;
    rbLCBFila: TRadioButton;
    rbLCBTeclado: TRadioButton;
    rbTCP: TRadioButton;
    rbTXT: TRadioButton;
    rdgImprimeChave1LinhaSAT: TRadioGroup;
    rgImprimeDescAcrescItemNFe: TRadioGroup;
    rgImprimeTributos: TRadioGroup;
    rgInfAdicProduto: TRadioGroup;
    rgInfFormaPagNFe: TRadioGroup;
    rgLayoutCanhoto: TRadioGroup;
    rgLocalCanhoto: TRadioGroup;
    rgModeloDanfe: TRadioGroup;
    rgModeloDANFeNFCE: TRadioGroup;
    rgModoImpressaoEvento: TRadioGroup;
    rgRedeTipoInter: TRadioGroup;
    rgRedeTipoLan: TRadioGroup;
    rgrMsgCanhoto: TRadioGroup;
    rgTamanhoPapelDacte: TRadioGroup;
    rgTipoAmb: TRadioGroup;
    rgTipoAmbBoleto: TRadioGroup;
    rgTipoDanfe: TRadioGroup;
    rgTipoFonte: TRadioGroup;
    SbArqLog: TSpeedButton;
    SbArqLog2: TSpeedButton;
    SbArqLog3: TSpeedButton;
    SbArqLog4: TSpeedButton;
    sbArquivoCert: TSpeedButton;
    sbArquivoKEY: TSpeedButton;
    sbArquivoCRT: TSpeedButton;
    sbArquivoWebServicesBPe: TSpeedButton;
    sbArquivoWebServicesNFSe: TSpeedButton;
    sbArquivoWebServicesCTe: TSpeedButton;
    sbArquivoWebServiceseSocial: TSpeedButton;
    sbArquivoWebServicesGNRe: TSpeedButton;
    sbArquivoWebServicesMDFe: TSpeedButton;
    sbArquivoWebServicesNFe: TSpeedButton;
    sbArquivoWebServicesReinf: TSpeedButton;
    sbBALLog: TSpeedButton;
    sbBALSerial: TSpeedButton;
    sbCHQBemafiINI: TSpeedButton;
    sbCHQSerial: TSpeedButton;
    sbConsultaCEP: TSpeedButton;
    sbConsultaCNPJBoleto: TSpeedButton;
    sbDirRFD: TSpeedButton;
    sbECFLog: TSpeedButton;
    sbECFSerial: TSpeedButton;
    sbLog: TSpeedButton;
    sbLogoMarca: TSpeedButton;
    sbLogoMarca1: TSpeedButton;
    sbLogoMarcaNFCeSAT: TSpeedButton;
    sbLogoMarcaPrefeitura: TSpeedButton;
    sbNomeDLL: TSpeedButton;
    sbNumeroSerieCert: TSpeedButton;
    sbPathArqTXT: TSpeedButton;
    sbPathDownload: TSpeedButton;
    sbPathDPEC: TSpeedButton;
    sbPathEvento: TSpeedButton;
    sbPathInu: TSpeedButton;
    sbPathNFe: TSpeedButton;
    sbPathPDF: TSpeedButton;
    sbPathSalvar: TSpeedButton;
    sbPosPrinterLog: TSpeedButton;
    sbSchemaDFe: TSpeedButton;
    sbSerial: TSpeedButton;
    sbTCArqPrecosEdit: TSpeedButton;
    sbTCArqPrecosFind: TSpeedButton;
    sbtnNumSerie: TSpeedButton;
    sbVerSenhaCertificado: TSpeedButton;
    sbVerSenhaEmail: TSpeedButton;
    sbVerSenhaProxySAT: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    ScrollBox: TScrollBox;
    Image1: TImage;
    ImageList1: TImageList;
    pCentral: TPanel;
    pComandos: TPanel;
    PrintDialog1: TPrintDialog;
    seBuffer: TSpinEdit;
    seCodBarrasAltura: TSpinEdit;
    seCodBarrasLargura: TSpinEdit;
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
    seGavetaTempoOFF: TSpinEdit;
    seGavetaTempoON: TSpinEdit;
    seLargura: TSpinEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    lblCep: TLabel;
    sbSobre: TSpeedButton;
    seFundoImp: TSpinEdit;
    seLinhasPular: TSpinEdit;
    seLogoFatorX: TSpinEdit;
    seLogoFatorY: TSpinEdit;
    seLogoKC1: TSpinEdit;
    seLogoKC2: TSpinEdit;
    seMargemDireita: TSpinEdit;
    seMargemEsquerda: TSpinEdit;
    seMargemFundo: TSpinEdit;
    seMargemTopo: TSpinEdit;
    seMFETimeout: TSpinEdit;
    seNumeroCaixa: TSpinEdit;
    sePagCod: TSpinEdit;
    seQRCodeErrorLevel: TSpinEdit;
    seQRCodeLargMod: TSpinEdit;
    seQRCodeTipo: TSpinEdit;
    seUSUCROCadastro: TSpinEdit;
    seUSUGTCadastro: TFloatSpinEdit;
    seUSUNumeroCadastro: TSpinEdit;
    sfeVersaoEnt: TFloatSpinEdit;
    shpLCB: TShape;
    shpTC: TShape;
    spBOLCopias: TSpinEdit;
    speAlturaCampos: TSpinEdit;
    spedtCasasDecimaisQtd: TSpinEdit;
    spedtDecimaisVUnit: TSpinEdit;
    spedtSATCasasDecimaisQtd: TSpinEdit;
    spedtSATDecimaisVUnit: TSpinEdit;
    SpeedButton1: TSpeedButton;
    spbExpand: TSpeedButton;
    spbCollapse: TSpeedButton;
    speEspEntreProd: TSpinEdit;
    speFonteAdic: TSpinEdit;
    speFonteCampos: TSpinEdit;
    speFonteEndereco: TSpinEdit;
    speFonteRazao: TSpinEdit;
    speLargCodProd: TSpinEdit;
    SpinEdit1: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    spnAttemptsMail: TSpinEdit;
    spnTimeOutMail: TSpinEdit;
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
    TabSheet1: TTabSheet;
    tsEmailNFSe: TTabSheet;
    tsImpNFSe: TTabSheet;
    tsWSNFSe: TTabSheet;
    tsTesteGTIN: TTabSheet;
    tsImpMDFe: TTabSheet;
    tsNCM: TTabSheet;
    tsWebBoleto: TTabSheet;
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
    TreeFilterEdit1: TTreeFilterEdit;
    TreeViewMenu: TTreeView;
    tsACBrBoleto: TTabSheet;
    tsBAL: TTabSheet;
    tsBoletoEmail: TTabSheet;
    tsCadastro: TTabSheet;
    tsCadSwChaveRSA: TTabSheet;
    tsCadSwDados: TTabSheet;
    tsCadSwH: TTabSheet;
    tsCadUsuario: TTabSheet;
    tsBeneficiario: TTabSheet;
    tsCertificadoDFe: TTabSheet;
    tsCHQ: TTabSheet;
    tsConfiguracaoDFe: TTabSheet;
    tsConsultas: TTabSheet;
    tsContaBancaria: TTabSheet;
    tsDadosEmit: TTabSheet;
    tsDadosEmpresa: TTabSheet;
    tsDadosSAT: TTabSheet;
    tsDadosSwHouse: TTabSheet;
    tsDFe: TTabSheet;
    tsDiretoriosDFe: TTabSheet;
    tsDIS: TTabSheet;
    tsECF: TTabSheet;
    tsECFParamI: TTabSheet;
    tsECFParamII: TTabSheet;
    tsEmail: TTabSheet;
    tsEmailCTe: TTabSheet;
    tsEmailDFe: TTabSheet;
    tsEmailMDFe: TTabSheet;
    tsEmailNFe: TTabSheet;
    tsEscPos: TTabSheet;
    tsETQ: TTabSheet;
    tsGAV: TTabSheet;
    tsImpCTe: TTabSheet;
    tsImpGeralDFe: TTabSheet;
    tsImpNFCe: TTabSheet;
    tsImpressaoDFe: TTabSheet;
    tsIntegrador: TTabSheet;
    tsLayoutBoleto: TTabSheet;
    tsLCB: TTabSheet;
    tsMonitor: TTabSheet;
    tsRede: TTabSheet;
    tsRelatorio: TTabSheet;
    tsRemessaRetorno: TTabSheet;
    tsRespTecnico: TTabSheet;
    tsRFD: TTabSheet;
    tsRFDConfig: TTabSheet;
    tsRFDINI: TTabSheet;
    tsSat: TTabSheet;
    tsSATemail: TTabSheet;
    tsSEDEX: TTabSheet;
    tsTC: TTabSheet;
    tsTesteCTe: TTabSheet;
    tsTesteMDFe: TTabSheet;
    tsTesteNFe: TTabSheet;
    tsTestesDFe: TTabSheet;
    tsWebServiceDFe: TTabSheet;
    tsWSNFCe: TTabSheet;
    tsWSNFe: TTabSheet;
    procedure ACBrEAD1GetChavePrivada(var Chave: ansistring);
    procedure ACBrEAD1GetChavePublica(var Chave: ansistring);
    procedure ACBrGIF1Click(Sender: TObject);
    procedure ACBrMail1MailException(const AMail: TACBrMail;
      const E: Exception; var ThrowIt: Boolean);
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
    procedure bEmailTestarConfClick(Sender: TObject);
    procedure bIBGETestarClick(Sender: TObject);
    procedure bImpressoraClick(Sender: TObject);
    procedure bInicializarClick(Sender: TObject);
    procedure bRSAeECFcClick(Sender: TObject);
    procedure bSedexRastrearClick(Sender: TObject);
    procedure bSedexTestarClick(Sender: TObject);
    procedure btAtivarsatClick(Sender: TObject);
    procedure btCertInfoClick(Sender: TObject);
    procedure btConsultarStatusOPSATClick(Sender: TObject);
    procedure btnBalancaClick(Sender: TObject);
    procedure btnBoletoBeneficiarioClick(Sender: TObject);
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
    procedure btnCancNFeSubsClick(Sender: TObject);
    procedure btNCMConsultarClick(Sender: TObject);
    procedure btNCMSalvarArquivoClick(Sender: TObject);
    procedure btNCMValidadeHelpClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarCTeClick(Sender: TObject);
    procedure btnConsultarGTINClick(Sender: TObject);
    procedure btnConsultarMDFeClick(Sender: TObject);
    procedure btnConsultasClick(Sender: TObject);
    procedure btnDFeCertificadosClick(Sender: TObject);
    procedure btnDFeClick(Sender: TObject);
    procedure btnDFeDirClick(Sender: TObject);
    procedure btnDFeEmailClick(Sender: TObject);
    procedure btnDFeGeralClick(Sender: TObject);
    procedure btnDFePrintClick(Sender: TObject);
    procedure btnDFeRespTecnicoClick(Sender: TObject);
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
    procedure btnFonteItensClick(Sender: TObject);
    procedure btnGavetaClick(Sender: TObject);
    procedure btnGerarAssinaturaSATClick(Sender: TObject);
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
    procedure btnSATEMAILClick(Sender: TObject);
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
    procedure btnVersaoSSLClick(Sender: TObject);
    procedure btSATAssociaClick(Sender: TObject);
    procedure btSATConfigRedeClick(Sender: TObject);
    procedure btStatusServicoClick(Sender: TObject);
    procedure cbControlePortaChange(Sender: TObject);
    procedure cbCortarPapelChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbIgnorarTagsChange(Sender: TObject);
    procedure cbLogChange(Sender: TObject);
    procedure cbLogCompClick(Sender: TObject);
    procedure cbHRIChange(Sender: TObject);
    procedure cbMonitorarPastaChange(Sender: TObject);
    procedure cbMunicipioChange(Sender: TObject);
    procedure cbSATMarcaChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbTraduzirTagsChange(Sender: TObject);
    procedure cbUFChange(Sender: TObject);
    procedure cbUsarEscPosClick(Sender: TObject);
    procedure cbUsarFortesClick(Sender: TObject);
    procedure cbxBOLBancoChange(Sender: TObject);
    procedure cbxBOLF_JChange(Sender: TObject);
    procedure cbCEPWebServiceChange(Sender: TObject);
    procedure cbxBOLUFChange(Sender: TObject);
    procedure cbxEmitCidadeChange(Sender: TObject);
    procedure cbxExibeResumoChange(Sender: TObject);
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
    procedure cbxSATSepararPorANOChange(Sender: TObject);
    procedure cbxSATSepararPorDIAChange(Sender: TObject);
    procedure cbxSATSepararPorModeloChange(Sender: TObject);
    procedure cbxSedexAvisoRecebChange(Sender: TObject);
    procedure cbxSedexFormatoChange(Sender: TObject);
    procedure cbxSedexMaoPropriaChange(Sender: TObject);
    procedure cbxSedexServicoChange(Sender: TObject);
    procedure cbxSATSepararPorCNPJChange(Sender: TObject);
    procedure cbxSATSepararPorMESChange(Sender: TObject);
    procedure cbxSepararPorCNPJChange(Sender: TObject);
    procedure cbxTimeZoneModeChange(Sender: TObject);
    procedure cbxUTF8Change(Sender: TObject);
    procedure chECFArredondaMFDClick(Sender: TObject);
    procedure chECFControlePortaClick(Sender: TObject);
    procedure chECFIgnorarTagsFormatacaoClick(Sender: TObject);
    procedure chRFDChange(Sender: TObject);
    procedure ckSalvarClick(Sender: TObject);
    procedure cbFormatoDecimaisChange(Sender: TObject);
    procedure deBOLDirArquivoExit(Sender: TObject);
    procedure deBOLDirLogoExit(Sender: TObject);
    procedure deBolDirRemessaExit(Sender: TObject);
    procedure BOLDirRetornoExit(Sender: TObject);
    procedure deBolDirRetornoRelChange(Sender: TObject);
    procedure deUSUDataCadastroExit(Sender: TObject);
    procedure deRFDDataSwBasicoExit(Sender: TObject);
    procedure edBALLogChange(Sender: TObject);
    procedure edEmailEnderecoChange(Sender: TObject);
    procedure edEmailEnderecoExit(Sender: TObject);
    procedure edEmailHostChange(Sender: TObject);
    procedure edEmailNomeChange(Sender: TObject);
    procedure edEmailPortaChange(Sender: TObject);
    procedure edEmailSenhaChange(Sender: TObject);
    procedure edEmailUsuarioChange(Sender: TObject);
    procedure edNomeDLLChange(Sender: TObject);
    procedure edSATLogChange(Sender: TObject);
    procedure edSATPathArqsCancChange(Sender: TObject);
    procedure edSATPathArqsChange(Sender: TObject);
    procedure edSATPathArqsEnvioChange(Sender: TObject);
    procedure edtArquivoPFXChange(Sender: TObject);
    procedure edtBOLCEPChange(Sender: TObject);
    procedure edtBOLCEPExit(Sender: TObject);
    procedure edtBOLCEPKeyPress(Sender: TObject; var Key: char);
    procedure edtBOLCNPJChange(Sender: TObject);
    procedure edtBOLCNPJKeyPress(Sender: TObject; var Key: char);
    procedure edtBOLRazaoSocialChange(Sender: TObject);
    procedure edtCNPJContadorChange(Sender: TObject);
    procedure edtCNPJContadorKeyPress(Sender: TObject; var Key: char);
    procedure edtCodigoAtivacaoChange(Sender: TObject);
    procedure edtEmitCNPJChange(Sender: TObject);
    procedure edtEmitCNPJKeyPress(Sender: TObject; var Key: char);
    procedure edtIdTokenChange(Sender: TObject);
    procedure edTimeZoneStrEditingDone(Sender: TObject);
    procedure edtNumeroSerieChange(Sender: TObject);
    procedure edtPathSchemasDFeChange(Sender: TObject);
    procedure edtSwHAssinaturaChange(Sender: TObject);
    procedure edtSwHCNPJChange(Sender: TObject);
    procedure edtSwHCNPJKeyPress(Sender: TObject; var Key: char);
    procedure edtTokenChange(Sender: TObject);
    procedure edtURLPFXChange(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image7MouseEnter(Sender: TObject);
    procedure Image7MouseLeave(Sender: TObject);
    procedure ImgCanalClick(Sender: TObject);
    procedure ImgCanalMouseEnter(Sender: TObject);
    procedure ImgCanalMouseLeave(Sender: TObject);
    procedure ImgChatClick(Sender: TObject);
    procedure ImgChatMouseEnter(Sender: TObject);
    procedure ImgChatMouseLeave(Sender: TObject);
    procedure ImgDocumentacaoClick(Sender: TObject);
    procedure Image7Click(Sender: TObject);
    procedure ImgDocumentacaoMouseEnter(Sender: TObject);
    procedure ImgDocumentacaoMouseLeave(Sender: TObject);
    procedure OnMascaraFormatKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);{%h-}
    procedure FormCreate(Sender: TObject);
    procedure ACBrECF1MsgAguarde(Mensagem: string);
    procedure ACBrECF1MsgPoucoPapel(Sender: TObject);
    procedure bConfigClick(Sender: TObject);
    procedure edOnlyNumbers(Sender: TObject; var Key: char);
    procedure bECFTestarClick(Sender: TObject);
    procedure bECFLeituraXClick(Sender: TObject);
    procedure bECFAtivarClick(Sender: TObject);
    procedure meUSUHoraCadastroExit(Sender: TObject);
    procedure meRFDHoraSwBasicoExit(Sender: TObject);
    procedure pgBoletoChange(Sender: TObject);
    procedure pgCadastroChange(Sender: TObject);
    procedure pgConfigChange(Sender: TObject);
    procedure pgConRFDChange(Sender: TObject);
    procedure pgDFeChange(Sender: TObject);
    procedure pgECFParamsChange(Sender: TObject);
    procedure pgSATChange(Sender: TObject);
    procedure rgRedeTipoInterClick(Sender: TObject);
    procedure rgRedeTipoLanClick(Sender: TObject);
    procedure SbArqLog2Click(Sender: TObject);
    procedure SbArqLog3Click(Sender: TObject);
    procedure SbArqLog4Click(Sender: TObject);
    procedure SbArqLogClick(Sender: TObject);
    procedure sbArquivoCertClick(Sender: TObject);
    procedure sbArquivoCRTClick(Sender: TObject);
    procedure sbArquivoKEYClick(Sender: TObject);
    procedure sbArquivoWebServicesCTeClick(Sender: TObject);
    procedure sbArquivoWebServiceseSocialClick(Sender: TObject);
    procedure sbArquivoWebServicesGNReClick(Sender: TObject);
    procedure sbArquivoWebServicesMDFeClick(Sender: TObject);
    procedure sbArquivoWebServicesNFeClick(Sender: TObject);
    procedure sbArquivoWebServicesBPeClick(Sender: TObject);
    procedure sbArquivoWebServicesNFSeClick(Sender: TObject);
    procedure sbArquivoWebServicesReinfClick(Sender: TObject);
    procedure sbBALSerialClick(Sender: TObject);
    procedure sbConsultaCEPClick(Sender: TObject);
    procedure sbConsultaCNPJBoletoClick(Sender: TObject);
    procedure sbLogoMarca1Click(Sender: TObject);
    procedure sbLogoMarcaNFCeSATClick(Sender: TObject);
    procedure sbBALLogClick(Sender: TObject);
    procedure sbLogoMarcaClick(Sender: TObject);
    procedure sbLogoMarcaPrefeituraClick(Sender: TObject);
    procedure sbNomeDLLClick(Sender: TObject);
    procedure sbNumeroSerieCertClick(Sender: TObject);
    procedure sbPathArqTXTClick(Sender: TObject);
    procedure sbPathDownloadClick(Sender: TObject);
    procedure sbPathDPECClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
    procedure sbPathInuClick(Sender: TObject);
    procedure sbPathNFeClick(Sender: TObject);
    procedure sbPathPDFClick(Sender: TObject);
    procedure sbPathSalvarClick(Sender: TObject);
    procedure sbPosPrinterLogClick(Sender: TObject);
    procedure sbSchemaDFeClick(Sender: TObject);
    procedure sbSerialClick(Sender: TObject);
    procedure sbSobreClick(Sender: TObject);
    procedure sbtnNumSerieClick(Sender: TObject);
    procedure sbVerSenhaCertificadoClick(Sender: TObject);
    procedure sbVerSenhaEmailClick(Sender: TObject);
    procedure sbVerSenhaProxyClick(Sender: TObject);
    procedure sbVerSenhaProxySATClick(Sender: TObject);
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
    procedure spbExpandClick(Sender: TObject);
    procedure spbCollapseClick(Sender: TObject);
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
    procedure TreeViewMenuClick(Sender: TObject);
    procedure tsACBrBoletoShow(Sender: TObject);
    procedure tsCadastroShow(Sender: TObject);
    procedure tsDFeShow(Sender: TObject);
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
    procedure tsSatShow(Sender: TObject);
    procedure tsTCShow(Sender: TObject);
    procedure cbxTCModeloChange(Sender: TObject);
    procedure sbTCArqPrecosEditClick(Sender: TObject);
    procedure sbTCArqPrecosFindClick(Sender: TObject);
    procedure TimerTCTimer(Sender: TObject);
    procedure sbCHQSerialClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure ACT_ButtonMouseEnter(Sender: TObject);
    procedure ACT_ButtonMouseLeave(Sender: TObject);
    function ValidaArquivo(APath:String; AArquivoDefault: String = ''): String;
    // Italo
    procedure AtualizarCidades;
  private
    ACBrMonitorINI: string;
    Inicio, fsMonitorarPasta: boolean;
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
    fSATLibsMarcas: String;
    fcUF: Integer;
    fcMunList: TStringList;

    FWasHidden: Boolean;
    FLastHandle: Integer;

    FMonitorConfig: TMonitorConfig;
    FDoACBr: TACBrObjetoACBr;
    FDoNFe: TACBrObjetoNFe;
    FDoCTe: TACBrObjetoCTe;
    FDoMDFe: TACBrObjetoMDFe;
    FDoBoleto: TACBrObjetoBoleto;
    FDoeSocial: TACBrObjetoeSocial;
    FDoReinf: TACBrObjetoReinf;
    FDoBAL: TACBrObjetoBAL;
    FDoEmail: TACBrObjetoEmail;
    FDoCEP: TACBrObjetoCEP;
    FDoCHQ: TACBrObjetoCHQ;
    FDoGAV: TACBrObjetoGAV;
    FDoIBGE: TACBrObjetoIBGE;
    FDoNCM: TACBrObjetoNCM;
    FDoLCB: TACBrObjetoLCB;
    FDoDIS: TACBrObjetoDIS;
    FDoSedex: TACBrObjetoSedex;
    FDoETQ: TACBrObjetoETQ;
    FDoGNRe: TACBrObjetoGNRe;
    FDoPosPrinter: TACBrObjetoPosPrinter;
    FDoSAT: TACBrObjetoSAT;
    FDoECF: TACBrObjetoECF;
    FDoBPe: TACBrObjetoBPe;
    FDoCNPJ: TACBrObjetoConsultaCNPJ;
    FDoCPF: TACBrObjetoConsultaCPF;
    FDoGTIN: TACBrObjetoGTIN;
    FDoNFSe: TACBrObjetoNFSe;

    FMenuTreeView: TMenu;

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

    procedure AtualizarHomologacaoDFe(Config: TConfiguracoes);
    procedure AtualizarImpressaoHomologacaoDFe(Report: TACBrDFeReport);
    procedure AtualizaAplicacaoDemo;
    procedure ValidarComunicacao;
    procedure ValidarConfigCertificado;
    procedure ValidarConfigWebService;
    procedure ValidarConfigSAT;
    procedure ValidarConfigMail;
    procedure LigarAlertasdeErrosDeConfiguracao;

    procedure VerificarErrosConfiguracaoComponentes(AfsCmd: TACBrCmd);
    procedure VerificarErrosComunicacao;
    function VerificarErrosConfiguracaoDFe: String;
    function VerificarErrosConfiguracaoSAT: String;
    function VerificarErrosConfiguracaoEMAIL: String;
    function VerificarErrosConfiguracaoBoleto: String;

    procedure CarregarListaDeCidades(cUF: Integer);
    procedure VerificarInterfaceNCM(Carregando: Boolean = True);

    procedure LeDadosRedeSAT;
    procedure ConfiguraRedeSAT;
    procedure ConsultarModeloSAT;
    procedure LeConfigMarcaSAT;

    procedure SetColorButtons(Sender: TObject);
    procedure SetColorSubButtons(Sender: TObject);
    procedure SetPanel(Sender: TPanel);
    procedure SetSize25(Sender: TObject);
    procedure SetScroll(Sender: TObject);

    procedure SetFontLabels(Sender: TObject);

    procedure ConfigPainelMenu(Edicao: Boolean);

  protected
    procedure MostraLogoBanco;
    procedure AtualizarTela(AMonitorConfig: TMonitorConfig);

    procedure EfeitoBotaoEnter(ASender, AIco: TImage);
    procedure EfeitoBotaoLeave(ASender, AIco: TImage);

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

    procedure LerIni(AtualizaMonitoramento: Boolean = True);
    procedure SalvarIni;
    procedure ConfiguraDANFe(GerarPDF: Boolean; MostrarPreview : String);    //MostrarPreview está sendo Tratado como String, pois pode receber três parâmetros: True, False, Vazio ''
    procedure ConfiguraDACTe(GerarPDF: Boolean; MostrarPreview : String);
    procedure ConfiguraDABPe(GerarPDF: Boolean; MostrarPreview : String);
    procedure ConfiguraDANFSe(GerarPDF: Boolean; MostrarPreview : String);
    procedure VerificaDiretorios;
    procedure LimparResp;
    procedure ExibeResp(Documento: ansistring);

    procedure AjustaACBrSAT ;
    procedure TrataErrosSAT(Sender : TObject ; E : Exception) ;
    procedure PrepararImpressaoSAT(NomeImpressora : string = ''; GerarPDF : boolean = false);

    procedure ConfiguraPosPrinter(SerialParams : String = '');
    procedure SetComumConfig(Configuracoes : TConfiguracoes) ;
    procedure AtualizaSSLLibsCombo ;

    procedure AntesDeImprimir(ShowPreview: Boolean = true);
    procedure DepoisDeImprimir;
    procedure HelptabSheet;
    procedure ValidarIntegradorNFCe(ChaveNFe: String = '');
    function RespostaIntegrador():String;
    function SubstituirVariaveis(const ATexto: String): String;
    procedure OnFormataDecimalSAT;
    procedure OnMensagemCanhotoNFe;
    procedure OnSATManual;

    property MonitorConfig: TMonitorConfig read FMonitorConfig;
  end;

var
  FrmACBrMonitor: TFrmACBrMonitor;
  sVersaoACBr : string;

implementation

uses
  IniFiles, TypInfo, LCLType, strutils,
  UtilUnit, pcnAuxiliar,
  {$IFDEF MSWINDOWS} sndkey32, {$ENDIF}
  {$IFDEF LINUX} unix, baseunix, termio, {$ENDIF}
  ACBrECFNaoFiscal, ACBrConsts, Math, Sobre, DateUtils,
  ConfiguraSerial, SelecionarCertificado, ACBrSATExtratoClass,
  ACBrNFeConfiguracoes, ACBrNFeDANFEClass, ACBrCTeConfiguracoes,
  ACBrMDFeConfiguracoes, ACBrGNREConfiguracoes, ACBreSocialConfiguracoes,
  ACBrReinfConfiguracoes, ACBrGTINConfiguracoes, ACBrBPeConfiguracoes,
  ACBrNFSeXConfiguracoes,
  ACBrDFeDANFeReport, ACBrETQClass, ACBrUtil.Base, ACBrUtil.FilesIO,
  ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math;

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
  iESO: TVersaoeSocial;
  iREI: TVersaoReinf;
  iGNR: TVersaoGNRE;
  iBPE: TVersaoBPe;
  iMDF: TVersaoMDFe;
  iCTE: TVersaoCTe;
  iNFe: TpcnVersaoDF;
  iNFSe: TLayoutNFSe;
  IPosReciboLayout: TPosReciboLayout;
  SPosReciboLayout: String;

  IBanco: TACBrTipoCobranca;
  ITipoChavePix: TACBrPIXTipoChave;

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
  IFormaEmissaoMDFe, IFormaEmissaoBPe: TpcnTipoEmissao;
  IForcarTagICMSSubs: TForcarGeracaoTag;
  IpcnImprimeDescAcrescItem: TpcnImprimeDescAcrescItem;
  IACBrLibRespostaTipo: TACBrLibRespostaTipo;
  IInformacoesDePagamento : TpcnInformacoesDePagamento;
  iETQModelo : TACBrETQModelo ;
  iETQDPI: TACBrETQDPI;
  iETQUnidade: TACBrETQUnidade;
  iETQBackFeed: TACBrETQBackFeed;
  iETQOrigem: TACBrETQOrigem;
  iFormatoDecimal: TDetFormato;
  iOperacao: TOperacao;
  M: Integer;
  K: Integer;
  vFormatSettings: TFormatSettings;
begin
  {$IFDEF MSWINDOWS}
  WindowState := wsMinimized;
  {$ENDIF}
  {$IFDEF LINUX}
  FpUmask(0);
  {$ENDIF}

  //LHelpConnector1.LHelpPath := ExtractFilePath(Application.ExeName)+
  //                          ClHelp + PathDelim + ClHelp;

  FMonitorConfig := TMonitorConfig.Create(
                 PathWithDelim(ExtractFilePath(Application.ExeName)) + CMonitorIni );
  FMonitorConfig.OnGravarConfig := @AtualizarTela;

  FDoACBr := TACBrObjetoACBr.Create(MonitorConfig);

  FDoNFe := TACBrObjetoNFe.Create(MonitorConfig, ACBrNFe1);
  FDoNFe.OnAntesDeImprimir  := @AntesDeImprimir;
  FDoNFe.OnDepoisDeImprimir := @DepoisDeImprimir;
  FDoNFe.OnConfiguraDANFe   := @ConfiguraDANFe;
  FDoNFe.OnValidarIntegradorNFCe:= @ValidarIntegradorNFCe;
  FDoNFe.OnSubstituirVariaveis  := @SubstituirVariaveis;
  FDoNFe.OnRespostaIntegrador   := @RespostaIntegrador;

  FDoCTe := TACBrObjetoCTe.Create(MonitorConfig, ACBrCTe1);
  FDoCTe.OnAntesDeImprimir := @AntesDeImprimir;
  FDoCTe.OnDepoisDeImprimir := @DepoisDeImprimir;
  FDoCTe.OnConfiguraDACTe   := @ConfiguraDACTe;

  FDoMDFe := TACBrObjetoMDFe.Create(MonitorConfig, ACBrMDFe1);
  FDoMDFe.OnAntesDeImprimir := @AntesDeImprimir;
  FDoMDFe.OnDepoisDeImprimir := @DepoisDeImprimir;
  
  FDoBoleto := TACBrObjetoBoleto.Create(MonitorConfig, ACBrBoleto1);
  FDoBoleto.OnAntesDeImprimir := @AntesDeImprimir;
  FDoBoleto.OnDepoisDeImprimir := @DepoisDeImprimir;

  FDoeSocial := TACBrObjetoeSocial.Create(MonitorConfig, ACBreSocial1);
  FDoReinf   := TACBrObjetoReinf.Create(MonitorConfig, ACBrReinf1);
  FDoBAL     := TACBrObjetoBAL.Create(MonitorConfig, ACBrBAL1);
  FDoEmail   := TACBrObjetoEmail.Create(MonitorConfig, ACBrMail1);
  FDoCEP     := TACBrObjetoCEP.Create(MonitorConfig, ACBrCEP1);

  FDoCHQ := TACBrObjetoCHQ.Create(MonitorConfig, ACBrCHQ1);
  FDoCHQ.OnAntesDeImprimir := @AntesDeImprimir;
  FDoCHQ.OnDepoisDeImprimir := @DepoisDeImprimir;

  FDoGAV := TACBrObjetoGAV.Create(MonitorConfig, ACBrGAV1);

  FDoIBGE := TACBrObjetoIBGE.Create(MonitorConfig, ACBrIBGE1);

  FDoNCM := TACBrObjetoNCM.Create(MonitorConfig, ACBrNCMs1);

  FDoLCB := TACBrObjetoLCB.Create(MonitorConfig, ACBrLCB1);

  FDoDIS := TACBrObjetoDIS.Create(MonitorConfig, ACBrDIS1);

  FDoSedex := TACBrObjetoSedex.Create(MonitorConfig, ACBrSedex1);

  FDoETQ := TACBrObjetoETQ.Create(MonitorConfig, ACBrETQ1);

  FDoCNPJ := TACBrObjetoConsultaCNPJ.Create(MonitorConfig, ACBrConsultaCNPJ1);
  FDoCPF := TACBrObjetoConsultaCPF.Create(MonitorConfig, ACBrConsultaCPF1);

  FDoGNRe := TACBrObjetoGNRe.Create(MonitorConfig, ACBrGNRe1);
  FDoGNRe.OnAntesDeImprimir := @AntesDeImprimir;
  FDoGNRe.OnDepoisDeImprimir := @DepoisDeImprimir;
//  FDoGNRe.OnConfiguraGuia   := @ConfiguraGuia;

  FDoPosPrinter := TACBrObjetoPosPrinter.Create(MonitorConfig, ACBrPosPrinter1);

  FDoSAT := TACBrObjetoSAT.Create(MonitorConfig, ACBrSAT1);
  FDoSAT.OnPrepararImpressaoSAT := @PrepararImpressaoSAT;
  FDoSAT.OnRespostaIntegrador := @RespostaIntegrador;

  FDoECF := TACBrObjetoECF.Create(MonitorConfig, ACBrECF1, ACBrBlocoX1);

  FDoBPe := TACBrObjetoBPe.Create(MonitorConfig, ACBrBPe1);
  FDoBPe.OnAntesDeImprimir := @AntesDeImprimir;
  FDoBPe.OnDepoisDeImprimir := @DepoisDeImprimir;

  FDoGTIN := TACBrObjetoGTIN.Create(MonitorConfig, ACBrGTIN1);

  FDoNFSe := TACBrObjetoNFSe.Create(MonitorConfig, ACBrNFSeX1);
  FDoNFSe.OnAntesDeImprimir := @AntesDeImprimir;
  FDoNFSe.OnDepoisDeImprimir := @DepoisDeImprimir;
  FDoNFSe.OnConfiguraDANFSe   := @ConfiguraDANFSe;
  FDoNFSe.OnSubstituirVariaveis  := @SubstituirVariaveis;

// Seta as definições iniciais para navegação
  SetColorButtons(btnMonitor);
  SetFontLabels(pgConfig);

  mResp.Clear;
  mCmd.Clear;

  fsCmd := TACBrCmd.Create;
  fsProcessar := TStringList.Create;
  fsInicioLoteTXT := False;

  FWasHidden := False;
  FLastHandle:= 0;
  fcUF := 0;
  fcMunList := TStringList.Create;

  Inicio := True;
  ArqSaiTXT := '';
  ArqSaiTMP := '';
  ArqEntTXT := '';
  ArqLogTXT := '';
  ArqLogCompTXT := '';
  Conexao := nil;
  NewLines := '';
  DISWorking := False;
  fSATLibsMarcas:= '';

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

  { Criando Lista Layout Canhoto }
  rgLayoutCanhoto.Items.Clear;
  IPosReciboLayout := Low(TPosReciboLayout);
  while IPosReciboLayout <= High(TPosReciboLayout) do
  begin
    SPosReciboLayout:= GetEnumName(TypeInfo(TPosReciboLayout), integer(IPosReciboLayout));
    rgLayoutCanhoto.Items.Add(copy(SPosReciboLayout,4, Length(SPosReciboLayout))); // Removendo "prl" do modelo.
    Inc(IPosReciboLayout);
  end;

  { Criando lista de Bancos disponiveis }
  cbxBOLBanco.Items.Clear;
  IBanco := Low(TACBrTipoCobranca);
  while IBanco <= High(TACBrTipoCobranca) do
  begin
    cbxBOLBanco.Items.Add(GetEnumName(TypeInfo(TACBrTipoCobranca), integer(IBanco)));
    Inc(IBanco);
  end;

  { Criando lista de chave pix disponiveis }
  cbxBOLTipoChavePix.Items.Clear;
  ITipoChavePix := Low(TACBrPIXTipoChave);
  while ITipoChavePix <= High(TACBrPIXTipoChave) do
  begin
    cbxBOLTipoChavePix.Items.Add(GetEnumName(TypeInfo(TACBrPIXTipoChave), integer(ITipoChavePix)));
    Inc(ITipoChavePix);
  end;
  cbxBOLTipoChavePix.ItemIndex:=0;



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

  vFormatSettings.DecimalSeparator  := '.';
  { Criando lista versões GNRe disponiveis }
  cbVersaoWSGNRE.Items.Clear;
  iGNR := Low(TVersaoGNRE);
  while iGNR <= High(TVersaoGNRE) do
  begin
    cbVersaoWSGNRE.Items.Add( FormatFloat('0.00', StrToFloat(copy( GetEnumName(TypeInfo(TVersaoGNRE), integer(iGNR)), 3, 1))
                              , vFormatSettings ) );
    Inc(iGNR);
  end;

  { Criando lista versões BPe disponiveis }
  cbVersaoWSBPe.Items.Clear;
  iBPE := Low(TVersaoBPe);
  while iBPE <= High(TVersaoBPe) do
  begin
    cbVersaoWSBPe.Items.Add( FormatFloat('0.00', StrToFloat(copy( GetEnumName(TypeInfo(TVersaoBPe), integer(iBPE)), 3, 1))
                              , vFormatSettings ) );
    Inc(iBPE);
  end;

  { Criando lista versões MDFe disponiveis }
  cbVersaoWSMDFe.Items.Clear;
  iMDF := Low(TVersaoMDFe);
  while iMDF <= High(TVersaoMDFe) do
  begin
    cbVersaoWSMDFe.Items.Add( FormatFloat('0.00', StrToFloat(copy( GetEnumName(TypeInfo(TVersaoMDFe), integer(iMDF)), 3, 1))
                              , vFormatSettings ) );
    Inc(iMDF);
  end;

  { Criando lista versões CTe disponiveis }
  cbVersaoWSCTe.Items.Clear;
  iCTE := Low(TVersaoCTe);
  while iCTE <= High(TVersaoCTe) do
  begin
    cbVersaoWSCTe.Items.Add( FormatFloat('0.00', StrToFloat(copy( GetEnumName(TypeInfo(TVersaoCTe), integer(iCTE)), 3, 1))
                              , vFormatSettings ) );
    Inc(iCTE);
  end;

  { Criando lista versões NFe disponiveis }
  cbVersaoWS.Items.Clear;
  iNFe := Low(TpcnVersaoDF);
  while iNFe <= High(TpcnVersaoDF) do
  begin
    cbVersaoWS.Items.Add( FormatFloat('0.00', (StrToFloat(copy( GetEnumName(TypeInfo(TpcnVersaoDF ), integer(iNFe)), 3, 3))) /100
                              , vFormatSettings ) );
    Inc(iNFe);
  end;

  { Criando lista Layouts NFSe disponiveis }
  cbLayoutNFSe.Items.Clear;
  iNFSe := Low(TLayoutNFSe);
  while iNFSe <= High(TLayoutNFSe) do
  begin
    cbLayoutNFSe.Items.Add( GetEnumName(TypeInfo(TLayoutNFSe ), integer(iNFSe)));
    Inc(iNFSe);
  end;

  { Criando lista versões e-social disponiveis }
  cbVersaoWSeSocial.Items.Clear;
  iESO := Low(TVersaoeSocial);
  while iESO <= High(TVersaoeSocial) do
  begin
    cbVersaoWSeSocial.Items.Add( Trim(copy( GetEnumName(TypeInfo(TVersaoeSocial), integer(iESO)), 3, 9) ) );
    Inc(iESO);
  end;

  { Criando lista versões ReInf disponiveis }
  cbVersaoWSReinf.Items.Clear;
  iREI := Low(TVersaoReinf);
  while iREI <= High(TVersaoReinf) do
  begin
    cbVersaoWSReinf.Items.Add( copy( GetEnumName(TypeInfo(TVersaoReinf), integer(iREI)), 2, 8) );
    Inc(iREI);
  end;

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

  {Boleto}
  cbHttpLibBoleto.Items.Clear ;
  For V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLibBoleto.Items.Add( GetEnumName(TypeInfo(TSSLHttpLib), integer(V) ) ) ;
  cbHttpLibBoleto.ItemIndex := 0 ;

  cbSSLTypeBoleto.Items.Clear ;
  For Y := Low(TSSLType) to High(TSSLType) do
    cbSSLTypeBoleto.Items.Add( GetEnumName(TypeInfo(TSSLType), integer(Y) ) ) ;
  cbSSLTypeBoleto.ItemIndex := 0 ;

  cbOperacaoBoleto.Items.Clear ;
  For iOperacao := Low(TOperacao) to High(TOperacao) do
    cbOperacaoBoleto.Items.Add( GetEnumName(TypeInfo(TOperacao), integer(iOperacao) ) ) ;
  cbOperacaoBoleto.ItemIndex := 0 ;

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

  cbUnidade.Items.Clear ;
  For iETQUnidade := Low(TACBrETQUnidade) to High(TACBrETQUnidade) do
     cbUnidade.Items.Add( GetEnumName(TypeInfo(TACBrETQUnidade), integer(iETQUnidade) ) ) ;

  cbFormatoDecimais.Items.Clear ;
  For iFormatoDecimal := Low(TDetFormato) to High(TDetFormato) do
     cbFormatoDecimais.Items.Add( GetEnumName(TypeInfo(TDetFormato), integer(iFormatoDecimal) ) ) ;

  cbETQPorta.Items.Clear;
  ACBrETQ1.Device.AcharPortasSeriais( cbETQPorta.Items );

  {$IFDEF LINUX}
  cbETQPorta.Items.Add('/dev/ttyS0') ;
  cbETQPorta.Items.Add('/dev/ttyS1') ;
  cbETQPorta.Items.Add('/dev/ttyUSB0') ;
  cbETQPorta.Items.Add('/dev/ttyUSB1') ;
  cbETQPorta.Items.Add('/tmp/ecf.txt') ;
  {$ELSE}
  cbETQPorta.Items.Add('LPT1') ;
  cbETQPorta.Items.Add('LPT2') ;
  cbETQPorta.Items.Add('\\localhost\L42') ;
  cbETQPorta.Items.Add('c:\temp\ecf.txt') ;
  {$ENDIF}

  cbETQPorta.Items.Add('TCP:192.168.0.31:9100') ;
  For M := 0 to Printer.Printers.Count-1 do
    cbETQPorta.Items.Add('RAW:'+Printer.Printers[M]);

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

  {$IFDEF LINUX}
  cbxPorta.Items.Add('/dev/ttyS0') ;
  cbxPorta.Items.Add('/dev/ttyS1') ;
  cbxPorta.Items.Add('/dev/ttyUSB0') ;
  cbxPorta.Items.Add('/dev/ttyUSB1') ;
  cbxPorta.Items.Add('/tmp/ecf.txt') ;
  {$ELSE}
  cbxPorta.Items.Add('LPT1') ;
  cbxPorta.Items.Add('LPT2') ;
  cbxPorta.Items.Add('\\localhost\Epson') ;
  cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  {$ENDIF}

  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;
  For K := 0 to Printer.Printers.Count-1 do
    cbxPorta.Items.Add('RAW:'+Printer.Printers[K]);

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

  cbFormaEmissaoBPe.Items.Clear;
  for IFormaEmissaoBPe := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
    cbFormaEmissaoBPe.Items.Add(GetEnumName(TypeInfo(TpcnTipoEmissao), integer(IFormaEmissaoBPe)));
  cbFormaEmissaoBPe.ItemIndex := 0;

  cbTagRejeicao938.Items.Clear;
  for IForcarTagICMSSubs := Low(TForcarGeracaoTag) to High(TForcarGeracaoTag) do
    cbTagRejeicao938.Items.Add(GetEnumName(TypeInfo(TForcarGeracaoTag), integer(IForcarTagICMSSubs)));
  cbTagRejeicao938.ItemIndex := 0;

  rgImprimeDescAcrescItemNFe.Items.Clear;
  for IpcnImprimeDescAcrescItem := Low(TpcnImprimeDescAcrescItem) to High(TpcnImprimeDescAcrescItem) do
    rgImprimeDescAcrescItemNFe.Items.Add(copy( GetEnumName(TypeInfo(TpcnImprimeDescAcrescItem), integer(IpcnImprimeDescAcrescItem)), 5, 8) );
  rgImprimeDescAcrescItemNFe.ItemIndex := 0;

  cbTipoResposta.Items.Clear;
  for IACBrLibRespostaTipo := Low(TACBrLibRespostaTipo) to High(TACBrLibRespostaTipo) do
    cbTipoResposta.Items.Add(copy( GetEnumName(TypeInfo(TACBrLibRespostaTipo), integer(IACBrLibRespostaTipo)), 4, 4) );
  cbTipoResposta.ItemIndex := 0;

  rgInfFormaPagNFe.Items.Clear;
  for IInformacoesDePagamento := Low(TpcnInformacoesDePagamento) to High(TpcnInformacoesDePagamento) do
    rgInfFormaPagNFe.Items.Add(copy( GetEnumName(TypeInfo(TpcnInformacoesDePagamento), integer(IInformacoesDePagamento)), 4, 10) );
  rgInfFormaPagNFe.ItemIndex := 0;

  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(0);
    FileVerInfo.ReadFileInfo;
    sVersaoACBr := FileVerInfo.VersionStrings.Values['FileVersion'] + ' - ' +
                   {$IfDef CPU64}'x64'{$Else}'x86'{$EndIf};
  finally
    FileVerInfo.Free;
  end;

  DefineTextoTrayTitulo;

  {$IFDEF Demo}
  rgTipoAmb.Enabled:= False;
  cbxAmbiente.Enabled:= False;
  {$ENDIF}

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

  ImageList2.GetBitmap(16, imgErrComunicacao.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrCertificado.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSSLLib.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrCryptLib.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrHttpLib.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrXmlSignLib.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrPathSchemas.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrWebService.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrTokenID.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrTokenCSC.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrCNPJ.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrCNPJBoleto.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrCEP.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrRazaoSocial.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrUF.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrWebServer.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrCidade.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSAT.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATLib.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATLibModelo.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATAtivar.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATInicializar.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATPathEnvio.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATPathVendas.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATPathCancelamento.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATCodAtivacao.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATEmitente.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATCNPJSH.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrSATAssign.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrEmail.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrEmail_Name.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrEmail_Mail.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrEmail_User.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrEmail_Smtp.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrEmail_Senha.Picture.Bitmap);
  ImageList2.GetBitmap(16, imgErrEmail_Porta.Picture.Bitmap);




  FMenuTreeView := TMenu.Create(pgConfig, TreeViewMenu);

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
  OpenURL('https://www.projetoacbr.com.br/forum/sac/sobre/');
end;

procedure TFrmACBrMonitor.ACBrMail1MailException(const AMail: TACBrMail;
  const E: Exception; var ThrowIt: Boolean);
begin
  if ACBrMail1.UseThread then
  begin
    ThrowIt := False;
    Resposta('', 'ERRO: ' + 'Erro ao Enviar email: ('+ E.Message + ')');
  end;

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

procedure TFrmACBrMonitor.bEmailTestarConfClick(Sender: TObject);
var
  Teste: string;

begin
  if (Trim(edEmailEndereco.Text) = '') or not FDoEmail.ValidarEmail(edEmailEndereco.Text) then
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
  CodRastreio, AMsg: string;
  I: Integer;
begin
  CodRastreio := '';
  AMsg := '';

  if not InputQuery('Código de Rastreio', 'Entre com o Código de Rastreio',CodRastreio) then
    exit;
  try
    ACBrSedex1.Rastrear(CodRastreio);
  except
    on E: Exception do
    begin
      raise Exception.Create('Falha na conexão para Rastreio' + sLineBreak + E.Message);
      exit;
    end;
  end;

  for I := 0 to ACBrSedex1.retRastreio.Count - 1 do
    AMsg := AMsg + 'Data Hora: '+ DateTimeToStr(ACBrSedex1.retRastreio[I].DataHora) + sLineBreak
               + 'Local: '+ ACBrSedex1.retRastreio[I].Local + sLineBreak
               + 'Situação: '+ ACBrSedex1.retRastreio[I].Situacao + sLineBreak
               + 'Obs: '+ ACBrSedex1.retRastreio[I].Observacao + sLineBreak;


  AddLinesLog(AMsg);
  if NaoEstaVazio(AMsg) then
    MessageDlg('Rastrear', AMsg, mtInformation, [mbOK], 0);

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

    try
      if not( ACBrSedex1.Consultar) then
      begin
        AMsg := 'Não Foi Possivel Fazer a Consulta: '+sLineBreak
          +IntToStr(ACBrSedex1.retErro)+' - '+ACBrSedex1.retMsgErro;
      end
      else
      begin
        AMsg :=  'CodigoServico: '+ retCodigoServico + sLineBreak +
              'Valor: '+ FloatToString(retValor) + sLineBreak +
              'PrazoEntrega: '+ IntToStr(retPrazoEntrega) + sLineBreak +
              'ValorSemAdicionais: '+ FloatToString(retValorSemAdicionais) + sLineBreak +
              'ValorMaoPropria: '+ FloatToString(retValorMaoPropria) + sLineBreak +
              'ValorAvisoRecebimento: '+ FloatToString(retValorAvisoRecebimento) + sLineBreak +
              'ValorValorDeclarado: '+ FloatToString(retValorValorDeclarado) + sLineBreak +
              'EntregaDomiciliar: '+retEntregaDomiciliar + sLineBreak +
              'EntregaSabado: '+retEntregaSabado + sLineBreak +
              'Erro: '+ IntToStr(retErro) + sLineBreak +
              'MsgErro: '+retMsgErro;

      end;

    finally
      AddLinesLog(AMsg);
      if NaoEstaVazio(AMsg) then
        MessageDlg('Consultar', AMsg, mtInformation, [mbOK], 0);
    end;

  end;

end;

procedure TFrmACBrMonitor.btAtivarsatClick(Sender: TObject);
var
  ACNPJ: String;
  AResult: String;
begin
  ACNPJ := OnlyNumber(edtEmitCNPJ.Text);
  if ACNPJ = '' then
    raise Exception.Create('CNPJ inválido. Configure a aba "Dados Emitente"');

  AResult:= ACBrSAT1.AtivarSAT(1, ACNPJ, StrToInt(edtCodUF.Text) );
  if NaoEstaVazio(AResult) then
    MessageDlg('Ativar SAT', AResult, mtInformation, [mbOK], 0);

end;

procedure TFrmACBrMonitor.btCertInfoClick(Sender: TObject);
var
  SL: TStringList;
begin
  SetComumConfig(ACBrNFe1.Configuracoes);
  ACBrNFe1.SSL.CarregarCertificado;
  SL := TStringList.Create;
  try
    SL.Add('Número de Série: '+ACBrNFe1.SSL.CertNumeroSerie);
    SL.Add('Válido até: '+FormatDateBr(ACBrNFe1.SSL.CertDataVenc));
    SL.Add('Subject Name: '+ACBrNFe1.SSL.CertSubjectName);
    SL.Add('Razão Social: ' + ACBrNFe1.SSL.CertRazaoSocial);
    SL.Add('CNPJ/CPF: ' + ACBrNFe1.SSL.CertCNPJ);
    SL.Add('Emissor: ' + ACBrNFe1.SSL.CertIssuerName);
    SL.Add('Certificadora: ' + ACBrNFe1.SSL.CertCertificadora);

    MessageDlg('Informações do Certificado', SL.Text, mtInformation, [mbOK], 0);
  finally
    SL.Free;
  end;

end;

procedure TFrmACBrMonitor.btConsultarStatusOPSATClick(Sender: TObject);
var
  SL: TStringList;
begin
  ACBrSAT1.ConsultarStatusOperacional;
  SL := TStringList.Create;
  try
    with ACBrSAT1.Status do
    begin
      SL.Add('NSERIE.........: '+NSERIE);
      SL.Add('LAN_MAC........: '+LAN_MAC);
      SL.Add('STATUS_LAN.....: '+StatusLanToStr(STATUS_LAN));
      SL.Add('NIVEL_BATERIA..: '+NivelBateriaToStr(NIVEL_BATERIA));
      SL.Add('MT_TOTAL.......: '+MT_TOTAL);
      SL.Add('MT_USADA.......: '+MT_USADA);
      SL.Add('DH_ATUAL.......: '+DateTimeToStr(DH_ATUAL));
      SL.Add('VER_SB.........: '+VER_SB);
      SL.Add('VER_LAYOUT.....: '+VER_LAYOUT);
      SL.Add('ULTIMO_CFe.....: '+ULTIMO_CFe);
      SL.Add('LISTA_INICIAL..: '+LISTA_INICIAL);
      SL.Add('LISTA_FINAL....: '+LISTA_FINAL);
      SL.Add('DH_CFe.........: '+DateTimeToStr(DH_CFe));
      SL.Add('DH_ULTIMA......: '+DateTimeToStr(DH_ULTIMA));
      SL.Add('CERT_EMISSAO...: '+DateToStr(CERT_EMISSAO));
      SL.Add('CERT_VENCIMENTO: '+DateToStr(CERT_VENCIMENTO));
      SL.Add('ESTADO_OPERACAO: '+EstadoOperacaoToStr(ESTADO_OPERACAO));

      AddLinesLog(SL.Text);

      MessageDlg('Consultar Status Operacional', SL.Text, mtInformation, [mbOK], 0);
    end;

    LeDadosRedeSAT;

  finally
    SL.Free;
  end;

end;

procedure TFrmACBrMonitor.btnBalancaClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsBAL;
end;

procedure TFrmACBrMonitor.btnBoletoBeneficiarioClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgBoleto.ActivePage := tsBeneficiario;
end;

procedure TFrmACBrMonitor.btnBoletoClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  SetSize25(TPanel(Sender).Parent);
  pgConfig.ActivePage := tsACBrBoleto;
    SetScroll(TPanel(Sender).Parent);
  // Ativa a 1a página do pegecontrol
  btnBoletoBeneficiarioClick(btnBoletoBeneficiario);
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
    with ACBrCTe1.EventoCTe.Evento.New do
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

    with ACBrMDFe1.EventoMDFe.Evento.New do
    begin
      infEvento.chMDFe   := Copy(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID, 5, 44);
      infEvento.CNPJCPF  := ACBrMDFe1.Manifestos.Items[0].MDFe.emit.CNPJCPF;
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
    with ACBrNFe1.EventoNFe.Evento.New do
    begin
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.detEvento.xJust := vAux;
    end;
    ACBrNFe1.EnviarEvento(StrToInt(idLote));
    ExibeResp(ACBrNFe1.WebServices.EnvEvento.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btnCancNFeSubsClick(Sender: TObject);
var
  idLote, vAux, chRef: string;
begin
  LimparResp;
  OpenDialog1.Title := 'Selecione a NFE para Cancelamento';
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
    if not (InputQuery('WebServices Eventos: Cancelamento Subst',
      'Identificador de controle do Lote de envio do Evento', idLote)) then
      exit;

    if not (InputQuery('WebServices Cancelamento Subst', 'Justificativa', vAux)) then
      exit;

    if not (InputQuery('WebServices Cancelamento Subst', 'Chave NFe Referencia', chRef)) then
      exit;

    ACBrNFe1.EventoNFe.Evento.Clear;
    ACBrNFe1.EventoNFe.idLote := StrToInt(idLote);
    with ACBrNFe1.EventoNFe.Evento.New do
    begin
      infEvento.dhEvento := now;

      infEvento.chNFe    := Copy(ACBrNFe1.NotasFiscais.Items[0].NFe.infNFe.ID, 4, 44);
      infEvento.CNPJ     := ACBrNFe1.NotasFiscais.Items[0].NFe.emit.CNPJCPF;
      infEvento.tpEvento := teCancSubst;
      infEvento.nSeqEvento := 1;
      infEvento.detEvento.xJust := vAux;
      infEvento.detEvento.nProt := ACBrNFe1.NotasFiscais.Items[0].NFe.procNFe.nProt;
      InfEvento.detEvento.chNFeRef := chRef;
      InfEvento.detEvento.verAplic := '1.0';
      InfEvento.detEvento.cOrgaoAutor:= ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.cUF;
    end;
    ACBrNFe1.EnviarEvento(StrToInt(idLote));
    ExibeResp(ACBrNFe1.WebServices.EnvEvento.RetWS);
  end;
end;

procedure TFrmACBrMonitor.btNCMConsultarClick(Sender: TObject);
var
  aMsg: String;
begin
  aMsg := EmptyStr;

  if (Length(edNCMCodigo.Text) <> 8) then
  begin
    MessageDlg('O Codigo NCM deve conter 8 caracteres', mtInformation, [mbOK], 0);
    Exit;
  end;

  VerificarInterfaceNCM;
  try
    if ACBrNCMs1.Validar(edNCMCodigo.Text) then
      aMsg := 'OK: NCM Valido'
    else
      aMsg := 'Erro: NCM Invalido';
  finally
    VerificarInterfaceNCM(False);
  end;

  AddLinesLog(aMsg);
  MessageDlg(aMsg, mtInformation, [mbOK], 0);
end;

procedure TFrmACBrMonitor.btNCMSalvarArquivoClick(Sender: TObject);
var
  aMsg, aDiretorio: String;
begin
  if (edNCMDiretorio.Text = EmptyStr) then
    aDiretorio := PathWithoutDelim(ExtractFilePath(Application.ExeName))
  else
    aDiretorio := PathWithoutDelim(edNCMDiretorio.Text);

  aDiretorio := aDiretorio + PathDelim + 'ListaNCM.csv';

  VerificarInterfaceNCM;
  try
    with ACBrNCMs1 do
    begin
      CacheDiasValidade := edNCMDiasValidade.Value;
      ObterNCMs(cbNCMForcarDownload.Checked);
      NCMS.SaveToFile(aDiretorio);
    end;
  finally
    VerificarInterfaceNCM(False);
  end;

  aMsg := 'Arquivo com a lista de NCMs salvo em: ' + aDiretorio;
  AddLinesLog(aMsg);
  MessageDlg(aMsg, mtInformation, [mbOK], 0);
end;

procedure TFrmACBrMonitor.btNCMValidadeHelpClick(Sender: TObject);
begin
  MessageDlg('Número de dias que o arquivo de NCMs baixado será válido' +
    sLineBreak + 'Após a validade o Download será feito novamente ' +
    sLineBreak + '(Zero para sempre ler NCMs do Cache local)',
    mtInformation, [mbOK], 0);
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

procedure TFrmACBrMonitor.btnConsultarGTINClick(Sender: TObject);
var
  wGTIN: String;
begin
  wGTIN := Trim(edConsultarGTIN.Text);
  ACBrGTIN1.Consultar(wGTIN);

  mResposta.Lines.Clear;
  mResposta.Lines.Add('GTIN Consultado: ' + wGTIN);
  mResposta.Lines.Add('--- Retorno ---');
  mResposta.Lines.Add('Retorno..: ' + DateTimeToStr(ACBrGTIN1.WebServices.Consulta.dhResp));
  mResposta.Lines.Add('Status...: ' + IntToStr(ACBrGTIN1.WebServices.Consulta.cStat));
  mResposta.Lines.Add('Motivo...: ' + ACBrGTIN1.WebServices.Consulta.xMotivo);
  mResposta.Lines.Add('Tipo GTIN: ' + IntToStr(ACBrGTIN1.WebServices.Consulta.tpGTIN));
  mResposta.Lines.Add('Produto..: ' + ACBrGTIN1.WebServices.Consulta.xProd);
  mResposta.Lines.Add('NCM......: ' + ACBrGTIN1.WebServices.Consulta.NCM);
  mResposta.Lines.Add('CEST.....: ' + ACBrGTIN1.WebServices.Consulta.CEST);
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

procedure TFrmACBrMonitor.btnDFeRespTecnicoClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgDFe.ActivePage := tsRespTecnico;
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
    ConfiguraDANFe(True, '');

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
          nil,   // Lista com emails que serão enviado cópias - TStrings
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

procedure TFrmACBrMonitor.btnFonteItensClick(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    ACBrNFeDANFCeFortes1.FonteLinhaItem.Name:= FontDialog1.Font.Name;
    ACBrNFeDANFCeFortes1.FonteLinhaItem.Size:= FontDialog1.Font.Size;
    ACBrNFeDANFCeFortes1.FonteLinhaItem.Style:= FontDialog1.Font.Style;
  end;
end;

procedure TFrmACBrMonitor.btnGavetaClick(Sender: TObject);
begin
  SetColorButtons(Sender);
  pgConfig.ActivePage := tsGAV;
end;

procedure TFrmACBrMonitor.btnGerarAssinaturaSATClick(Sender: TObject);
var
 S: Ansistring;
 cnpjSwh, cnpjEmit: String;
begin
  if EstaVazio(edtSwHCNPJ.text) then
  begin
    MessageDlg('Informe CNPJ da Software House!', mtError, [mbOK], 0);
    exit;
  end;

  if EstaVazio(edtEmitCNPJ.text) then
  begin
    MessageDlg('Informe CNPJ do Emitente!', mtError, [mbOK], 0);
    exit;
  end;

  if edtSwHAssinatura.Text <> '' then
    if MessageDlg('Assinatura já existente!'+sLineBreak+
       'Confirma geração de uma nova Assinatura?', mtConfirmation,
        mbYesNoCancel, 0) <> mrYes then
        exit;

  cnpjSwh:= OnlyNumber(edtSwHCNPJ.text);
  cnpjEmit:= OnlyNumber(edtEmitCNPJ.Text);

  S:= 'SAT.'+CMetodoGerarAssinaturaSAT+'("'+cnpjSwh+'","'+cnpjEmit+'")' ;
  fsProcessar.Add(S);

  Processar;
  if fsCmd.Resposta <> '' then
    edtSwHAssinatura.Text:= fsCmd.Resposta;

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
    ConfiguraDANFe(False, '');

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
  pgConfig.ActivePage := tsNCM;
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

procedure TFrmACBrMonitor.btnSATEMAILClick(Sender: TObject);
begin
  SetColorSubButtons(Sender);
  pgSAT.ActivePage := tsSATemail;
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

procedure TFrmACBrMonitor.btnVersaoSSLClick(Sender: TObject);
begin
  MessageDlg( ACBrNFe1.SSL.SSLCryptClass.Versao , mtInformation, [mbOK], 0);
end;

procedure TFrmACBrMonitor.btSATAssociaClick(Sender: TObject);
var
  Result: String;
begin
  Result := ACBrSAT1.AssociarAssinatura( edtSwHCNPJ.Text + edtEmitCNPJ.Text, edtSwHAssinatura.Text );
  if NaoEstaVazio(Result) then
    MessageDlg('Associar Assinatura', Result, mtInformation, [mbOK], 0);

end;

procedure TFrmACBrMonitor.btSATConfigRedeClick(Sender: TObject);
var
  Result : String;
begin
  ConfiguraRedeSAT;

  Result := ACBrSAT1.ConfigurarInterfaceDeRede(ACBrSAT1.Rede.AsXMLString);
  if NaoEstaVazio(Result) then
  begin
    AddLinesLog(Result);
    MessageDlg('Configurar Interface de Rede', Result, mtInformation, [mbOK], 0);
  end;

end;

procedure TFrmACBrMonitor.btStatusServicoClick(Sender: TObject);
var
  SL: TStringList;
begin
  SetComumConfig(ACBrNFe1.Configuracoes);
  ACBrNFe1.WebServices.StatusServico.Executar;
  SL := TStringList.Create;
  try
    SL.Add('versao: ' + ACBrNFe1.WebServices.StatusServico.versao);
    SL.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.StatusServico.tpAmb));
    SL.Add('verAplic: ' + ACBrNFe1.WebServices.StatusServico.verAplic);
    SL.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.cStat));
    SL.Add('xMotivo: ' + ACBrNFe1.WebServices.StatusServico.xMotivo);
    SL.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.cUF));
    SL.Add('dhRecbto: ' + DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRecbto));
    //SL.Add('tMed: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.TMed));
    //SL.Add('dhRetorno: ' + DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRetorno));
    SL.Add('xObs: ' + ACBrNFe1.WebServices.StatusServico.xObs);

    MessageDlg('Status Serviço', SL.Text, mtInformation, [mbOK], 0);
  finally
    SL.Free;
  end;
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

procedure TFrmACBrMonitor.cbxBOLUFChange(Sender: TObject);
var
  cUF: Integer;
  Ok: Boolean;
begin
  Ok := (cbxBOLUF.ItemIndex >= 0);
  imgErrUF.Visible := not Ok;

  if Ok then
  begin
    cUF := UFtoCUF(cbxBOLUF.Text);
    if (cUF <> FcUF) then
    begin
      //pEmitCodUF.Caption := IntToStrZero(cUF, 2);
      CarregarListaDeCidades(cUF);
    end;
  end;
end;

procedure TFrmACBrMonitor.cbxEmitCidadeChange(Sender: TObject);
var
  Ok: Boolean;
begin
  Ok := (cbxEmitCidade.ItemIndex >= 0);
  imgErrCidade.Visible := not Ok;
  if Ok then
    edtBOLCodCidade.Caption := FcMunList[cbxEmitCidade.ItemIndex];
end;

procedure TFrmACBrMonitor.cbxExibeResumoChange(Sender: TObject);
begin
  OnMensagemCanhotoNFe;
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
      ACBreSocial1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrReinf1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrBPe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrGTIN1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      ACBrNFSeX1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
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
  ValidarConfigSAT;
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

procedure TFrmACBrMonitor.cbxSATSepararPorANOChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorAno := cbxSATSepararPorANO.Checked;
end;

procedure TFrmACBrMonitor.cbxSATSepararPorDIAChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorDia := cbxSATSepararPorDIA.Checked;
end;

procedure TFrmACBrMonitor.cbxSATSepararPorModeloChange(Sender: TObject);
begin
  ACBrSAT1.ConfigArquivos.SepararPorModelo := cbxSATSepararPorModelo.Checked;
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
  ACBrCTe1.Configuracoes.Arquivos.SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
  ACBrMDFe1.Configuracoes.Arquivos.SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
  ACBrBPe1.Configuracoes.Arquivos.SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
  ACBrNFSeX1.Configuracoes.Arquivos.SepararPorCNPJ := cbxSepararPorCNPJ.Checked;
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
  ACBreSocial1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
  ACBrReinf1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
  ACBrBPe1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
  ACBrGTIN1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
  ACBrNFSeX1.Configuracoes.WebServices.TimeZoneConf.Assign( ACBrNFe1.Configuracoes.WebServices.TimeZoneConf );
end;

procedure TFrmACBrMonitor.cbxUTF8Change(Sender: TObject);
begin
  ACBrSAT1.Config.EhUTF8 := cbxUTF8.Checked;
  sePagCod.Value := ACBrSAT1.Config.PaginaDeCodigo;
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

procedure TFrmACBrMonitor.cbFormatoDecimaisChange(Sender: TObject);
begin
  OnFormataDecimalSAT;
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

procedure TFrmACBrMonitor.edEmailEnderecoChange(Sender: TObject);
begin
  //imgErrEmail_Mail.Visible := not(edEmailEndereco.Text <> '');
  ValidarConfigMail;
end;

procedure TFrmACBrMonitor.edEmailEnderecoExit(Sender: TObject);
begin
  if (Trim(edEmailEndereco.Text) <> '') and not FDoEmail.ValidarEmail(
    edEmailEndereco.Text) then
  begin
    AddLinesLog('O endereço de E-mail informado não é Válido');
    edEmailEndereco.SetFocus;
  end;
end;

procedure TFrmACBrMonitor.edEmailHostChange(Sender: TObject);
begin
  //imgErrEmail_Smtp.Visible := not(edEmailHost.Text <> '');
  ValidarConfigMail;
end;

procedure TFrmACBrMonitor.edEmailNomeChange(Sender: TObject);
begin
  //imgErrEmail_Name.Visible := not(edEmailNome.Text <> '');
  ValidarConfigMail;
end;

procedure TFrmACBrMonitor.edEmailPortaChange(Sender: TObject);
begin
  //imgErrEmail_Porta.Visible := not(edEmailPorta.Text <> '');
  ValidarConfigMail;
end;

procedure TFrmACBrMonitor.edEmailSenhaChange(Sender: TObject);
begin
  //imgErrEmail_Senha.Visible := not(edEmailSenha.Text <> '');
  ValidarConfigMail;
end;

procedure TFrmACBrMonitor.edEmailUsuarioChange(Sender: TObject);
begin
  //imgErrEmail_User.Visible := not(edEmailUsuario.Text <> '');
  ValidarConfigMail;
end;

procedure TFrmACBrMonitor.edNomeDLLChange(Sender: TObject);
begin
  ValidarConfigSAT;
end;

procedure TFrmACBrMonitor.edSATLogChange(Sender: TObject);
begin
  ACBrSAT1.ArqLOG:= edSATLog.Text;
end;

procedure TFrmACBrMonitor.edSATPathArqsCancChange(Sender: TObject);
begin
  ValidarConfigSAT;
  //imgErrSATPathCancelamento.Visible := not( (edSATPathArqsCanc.Text <> '') and
  //      DirectoryExists(edSATPathArqsCanc.Text));
end;

procedure TFrmACBrMonitor.edSATPathArqsChange(Sender: TObject);
begin
  ValidarConfigSAT;
  //imgErrSATPathVendas.Visible := not( (edSATPathArqs.Text <> '') and
    //    DirectoryExists(edSATPathArqs.Text));
end;

procedure TFrmACBrMonitor.edSATPathArqsEnvioChange(Sender: TObject);
begin
  ValidarConfigSAT;
  //imgErrSATPathEnvio.Visible := not( (edSATPathArqsEnvio.Text <> '') and
  //      DirectoryExists(edSATPathArqsEnvio.Text));
end;

procedure TFrmACBrMonitor.edtArquivoPFXChange(Sender: TObject);
begin
  if (edtArquivoPFX.Text <> '') then
  begin
    if (edtNumeroSerie.Text <> '') then
      edtNumeroSerie.Text := '';
  end;

  ValidarConfigCertificado;
end;

procedure TFrmACBrMonitor.edtBOLCEPChange(Sender: TObject);
begin
  if (Length(edtBOLCEP.Text) > 5) then
  begin
    edtBOLCEP.Text := FormatarMascaraDinamica(OnlyNumber(edtBOLCEP.Text), '*****-***');
    edtBOLCEP.SelStart := Length(edtBOLCEP.Text);
  end;

  imgErrCEP.Visible := (Length(edtBOLCEP.Text) < 9);
  sbConsultaCEP.Visible := not imgErrCEP.Visible;

end;

procedure TFrmACBrMonitor.edtBOLCEPExit(Sender: TObject);
begin
  if (not imgErrCEP.Visible) and (edtBOLLogradouro.Text = '') then
    sbConsultaCEP.Click;
end;

procedure TFrmACBrMonitor.edtBOLCEPKeyPress(Sender: TObject; var Key: char);
begin
    if not CharInSet( Key, [#8,'0'..'9'] ) then
    Key := #0;
end;

procedure TFrmACBrMonitor.edtBOLCNPJChange(Sender: TObject);
begin
   if (Length(edtBOLCNPJ.Text) > 2) and (cbxBOLF_J.ItemIndex <> 2) then
  begin
    if cbxBOLF_J.ItemIndex = 0 then
      edtBOLCNPJ.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtBOLCNPJ.Text), '***.***.***-**')
    else
      edtBOLCNPJ.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtBOLCNPJ.Text), '**.***.***/****-**');
    edtBOLCNPJ.SelStart := Length(edtBOLCNPJ.Text);

    imgErrCNPJBoleto.Visible := (Length(edtBOLCNPJ.Text) < 14) or
                        (ACBrValidador.ValidarCNPJouCPF(edtBOLCNPJ.Text) <> '');
    sbConsultaCNPJBoleto.Visible := (not imgErrCNPJBoleto.Visible) and (cbxBOLF_J.ItemIndex = 1);
  end;

end;

procedure TFrmACBrMonitor.edtBOLCNPJKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet( Key, [#8,'0'..'9'] ) then
    Key := #0;
end;

procedure TFrmACBrMonitor.edtBOLRazaoSocialChange(Sender: TObject);
begin
  imgErrRazaoSocial.Visible := (Length(edtBOLRazaoSocial.Text) < 4);
end;

procedure TFrmACBrMonitor.edtCNPJContadorChange(Sender: TObject);
begin
  if (Length(edtCNPJContador.Text) > 2) then
  begin
    edtCNPJContador.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtCNPJContador.Text), '**.***.***/****-**');
    edtCNPJContador.SelStart := Length(edtCNPJContador.Text);
  end;

  imgErrCNPJ.Visible :=  (Length(edtCNPJContador.Text) > 1) and
                        (ACBrValidador.ValidarCNPJ(edtCNPJContador.Text) <> '');

end;

procedure TFrmACBrMonitor.edtCNPJContadorKeyPress(Sender: TObject; var Key: char
  );
begin
  if not CharInSet( Key, [#8,'0'..'9'] ) then
    Key := #0;
end;

procedure TFrmACBrMonitor.edtCodigoAtivacaoChange(Sender: TObject);
begin
  ValidarConfigSAT
  //imgErrSATCodAtivacao.Visible := not( Length( edtCodigoAtivacao.Text) > 5);
  //edNomeDLLChange(Sender);
end;

procedure TFrmACBrMonitor.edtEmitCNPJChange(Sender: TObject);
begin
  if (Length(edtEmitCNPJ.Text) > 2) then
  begin
    edtEmitCNPJ.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtEmitCNPJ.Text), '**.***.***/****-**');
    edtEmitCNPJ.SelStart := Length(edtEmitCNPJ.Text);
  end;
  ValidarConfigSAT;

  //imgErrSATEmitente.Visible := not( Length( edtEmitCNPJ.Text) > 5);
  //edNomeDLLChange(Sender);

end;

procedure TFrmACBrMonitor.edtEmitCNPJKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet( Key, [#8,'0'..'9'] ) then
    Key := #0;
end;

procedure TFrmACBrMonitor.edtIdTokenChange(Sender: TObject);
begin
  imgErrTokenID.Visible := (edtIdToken.Text = '');
end;

procedure TFrmACBrMonitor.edTimeZoneStrEditingDone(Sender: TObject);
begin
  try
    ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr := edTimeZoneStr.Caption;
  finally
    edTimeZoneStr.Caption := ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr;
  end;
end;

procedure TFrmACBrMonitor.edtNumeroSerieChange(Sender: TObject);
begin
  if (edtNumeroSerie.Text <> '') then
  begin
    if (edtURLPFX.Text <> '') then
      edtURLPFX.Text := '';

    if (edtArquivoPFX.Text <> '') then
      edtArquivoPFX.Text := '';
  end;

  ValidarConfigCertificado;
end;

procedure TFrmACBrMonitor.edtPathSchemasDFeChange(Sender: TObject);
begin
  ValidarConfigWebService;
end;

procedure TFrmACBrMonitor.edtSwHAssinaturaChange(Sender: TObject);
begin
  ValidarConfigSAT;
  //imgErrSATAssign.Visible := not( Length( edtSwHAssinatura.Text) > 20);
  //edNomeDLLChange(Sender);
end;

procedure TFrmACBrMonitor.edtSwHCNPJChange(Sender: TObject);
begin
  if (Length(edtSwHCNPJ.Text) > 2) then
  begin
    edtSwHCNPJ.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtSwHCNPJ.Text), '**.***.***/****-**');
    edtSwHCNPJ.SelStart := Length(edtSwHCNPJ.Text);
  end;
  ValidarConfigSAT;

  //imgErrSATCNPJSH.Visible := not( Length( edtSwHCNPJ.Text) > 5);
  //edNomeDLLChange(Sender);
end;

procedure TFrmACBrMonitor.edtSwHCNPJKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet( Key, [#8,'0'..'9'] ) then
    Key := #0;
end;

procedure TFrmACBrMonitor.edtTokenChange(Sender: TObject);
begin
  imgErrTokenCSC.Visible := (edtToken.Text = '');
end;

procedure TFrmACBrMonitor.edtURLPFXChange(Sender: TObject);
begin
if (edtURLPFX.Text <> '') then
  begin
    if (edtNumeroSerie.Text <> '') then
      edtNumeroSerie.Text := '';

    if (edtArquivoPFX.Text = '') then
      edtArquivoPFX.Text := 'CertA1.pfx';
  end;

  ValidarConfigCertificado;
end;

procedure TFrmACBrMonitor.Image2Click(Sender: TObject);
begin
  bConfigClick(Self);
end;

procedure TFrmACBrMonitor.Image7MouseEnter(Sender: TObject);
begin
  EfeitoBotaoEnter( (Sender as TImage), imgExp);

end;

procedure TFrmACBrMonitor.Image7MouseLeave(Sender: TObject);
begin
  EfeitoBotaoLeave( (Sender as TImage), imgExp);

end;

procedure TFrmACBrMonitor.ImgCanalClick(Sender: TObject);
begin
  OpenURL('https://www.youtube.com/playlist?list=PLhDFxIHG3stqHUbfs_eOtMoWRKvO1NCpT');
end;

procedure TFrmACBrMonitor.ImgCanalMouseEnter(Sender: TObject);
begin
  EfeitoBotaoEnter( (Sender as TImage), imgNetCanal);
end;

procedure TFrmACBrMonitor.ImgCanalMouseLeave(Sender: TObject);
begin
  EfeitoBotaoLeave( (Sender as TImage), imgNetCanal);
end;

procedure TFrmACBrMonitor.ImgChatClick(Sender: TObject);
begin
  OpenURL('https://projetoacbr.com.br/discord');
end;

procedure TFrmACBrMonitor.ImgChatMouseEnter(Sender: TObject);
begin
  EfeitoBotaoEnter( (Sender as TImage), imgNetChat);
end;

procedure TFrmACBrMonitor.ImgChatMouseLeave(Sender: TObject);
begin
  EfeitoBotaoLeave( (Sender as TImage), imgNetChat);
end;

procedure TFrmACBrMonitor.ImgDocumentacaoClick(Sender: TObject);
begin
  OpenURL('https://acbr.sourceforge.io/ACBrMonitor/Apresentacao.html');
end;

procedure TFrmACBrMonitor.Image7Click(Sender: TObject);
begin
  bConfigClick(Self);
end;

procedure TFrmACBrMonitor.ImgDocumentacaoMouseEnter(Sender: TObject);
begin
  EfeitoBotaoEnter( (Sender as TImage), imgNetDoc);

end;

procedure TFrmACBrMonitor.ImgDocumentacaoMouseLeave(Sender: TObject);
begin
  EfeitoBotaoLeave( (Sender as TImage), imgNetDoc);

end;

procedure TFrmACBrMonitor.OnMascaraFormatKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = '.')
        or ((Key = ',') and (Pos(',', TEdit(Sender).Text) < 1))
        or (Key in ['0'..'9'])
        or (Key = Char(VK_BACK))
        or (Key = Char(VK_END)) then
    Exit;

    Key := #0;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.FormDestroy(Sender: TObject);
begin
  fcMunList.Free;
  fsCmd.Free;
  fsProcessar.Free;

  fsSLPrecos.Free;

  FDoACBr.Free;
  FDoSAT.Free;
  FDoECF.Free;
  FDoMDFe.Free;
  FDoNFe.Free;
  FDoBoleto.Free;
  FDoCTe.Free;
  FDoeSocial.Free;
  FDoReinf.Free;
  FDoBAL.Free;
  FDoEmail.Free;
  FDoCEP.Free;
  FDoCHQ.Free;
  FDoGAV.Free;
  FDoIBGE.Free;
  FDoNCM.Free;
  FDoLCB.Free;
  FDoDIS.Free;
  FDoSedex.Free;
  FDoETQ.Free;
  FDoCNPJ.Free;
  FDoCPF.Free;
  FDoGNRe.Free;
  FDoPosPrinter.Free;
  FDoBPe.Free;
  FDoGTIN.Free;
  FDoNFSe.Free;

  FMenuTreeView.Free;

  FMonitorConfig.Free;
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
  fsMonitorarPasta := False;
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

  // Tenta definir o modelo SAT e DLL usando a "Marca"
  try
  if (FMonitorConfig.SAT.Marca <> '') then
    LeConfigMarcaSAT;
  except
    on E: Exception do
      Erro := Erro + sLineBreak + E.Message;
  end;

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
      if fsMonitorarPasta then
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

    cbLogChange(Self);

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
procedure TFrmACBrMonitor.LerIni(AtualizaMonitoramento: Boolean);
var
  ECFAtivado, CHQAtivado, GAVAtivado, DISAtivado, BALAtivado,
  ETQAtivado, ESCPOSAtivado, ESCPOSAtivadoBPe: boolean;
  wSenha, POSPrDeviceParams, ECFDeviceParams, CHQDeviceParams: string;
  //PathApplication: string;
  wDirArquivo, wNomeArquivo, wPathArquivo: string;
  OK: boolean;
begin
  //PathApplication := PathWithDelim(ExtractFilePath(Application.ExeName));

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

  if ( Assigned(ACBrBPeDABPeESCPOS1.PosPrinter) ) then
     ESCPOSAtivadoBPe := ACBrBPeDABPeESCPOS1.PosPrinter.Device.Ativo
  else
     ESCPOSAtivadoBPe := False;

  //Atualiza Campos Classe ACbrMonitorConfig
  try
    FMonitorConfig.CarregarArquivo;
  except
    on E: Exception do
    begin
      if pos(SErrArqConfNaoEncontrado,E.Message) > 0 then
         FMonitorConfig.CriarArquivo;
    end;
  end;

  { Parametros do Monitor }
  with FMonitorConfig.ACBrMonitor do
  begin

    { Lendo Senha }
    fsHashSenha := StrToIntDef(HashSenha, -1);

    if (fsHashSenha < 1) then  { INI antigo não tinha essa chave }
    begin
       wSenha := Senha;
       if (wSenha <> '') then
         fsHashSenha := StringCrc16(wSenha);
    end;

    if (fsHashSenha > 0) then
    begin
       cbSenha.Checked := True;
       edSenha.Text := 'NADAAQUI';
    end;

    rbTCP.Checked                     := Modo_TCP;
    rbTXT.Checked                     := Modo_TXT;
    edPortaTCP.Text                   := IntToStr(TCP_Porta);
    edTimeOutTCP.Text                 := IntToStr(TCP_TimeOut);
    chbTCPANSI.Checked                := Converte_TCP_Ansi;
    edEntTXT.Text                     := TXT_Entrada;
    edSaiTXT.Text                     := TXT_Saida;
    chbArqEntANSI.Checked             := Converte_TXT_Entrada_Ansi;
    chbArqSaiANSI.Checked             := Converte_TXT_Saida_Ansi;
    sedIntervalo.Value                := Intervalo;
    edLogArq.Text                     := Arquivo_Log;
    cbLog.Checked                     := Gravar_Log and (edLogArq.Text <> '');
    sedLogLinhas.Value                := Linhas_Log;
    cbComandos.Checked                := Comandos_Remotos;
    cbUmaInstancia.Checked            := Uma_Instancia;
    cbAbas.Checked                    := MostraAbas;
    cbMostrarNaBarraDeTarefas.Checked := MostrarNaBarraDeTarefas;
    cbRetirarAcentosNaResposta.Checked:= RetirarAcentosNaResposta;
    chkMostraLogNaTela.Checked        := MostraLogEmRespostasEnviadas and cbLog.Checked;
    cbTipoResposta.ItemIndex          := TipoResposta;

    if AtualizaMonitoramento then
    begin
      cbMonitorarPasta.OnChange         := Nil;
      cbMonitorarPasta.Checked          := MonitoraPasta;
      cbMonitorarPasta.OnChange         := @cbMonitorarPastaChange;

      fsMonitorarPasta                  := cbMonitorarPasta.Checked;

      ArqEntTXT                         := AcertaPath(edEntTXT.Text);
      ArqEntOrig                        := ArqEntTXT;
      ArqSaiTXT                         := AcertaPath(edSaiTXT.Text);
      ArqSaiOrig                        := ArqSaiTXT;
      ArqSaiTMP                         := ChangeFileExt(ArqSaiTXT, '.tmp');
      ArqLogTXT                         := AcertaPath(edLogArq.Text);

      TcpServer.Port                    := edPortaTCP.Text;
      TcpServer.TimeOut                 := StrToIntDef(edTimeOutTCP.Text, 10000);
    end;

  end;

  { Parametros do ECF }
  with FMonitorConfig.ECF do
  begin
    ECFDeviceParams                   := SerialParams;
    cbECFModelo.ItemIndex             := Modelo + 1;
    cbECFModeloChange(Self);
    cbECFPorta.Text                   := Porta;
    sedECFTimeout.Value               := Timeout;
    sedECFIntervalo.Value             := IntervaloAposComando;
    sedECFMaxLinhasBuffer.Value       := MaxLinhasBuffer;
    sedECFPaginaCodigo.Value          := PaginaCodigo;
    sedECFLinhasEntreCupons.Value     := LinhasEntreCupons;
    chECFArredondaPorQtd.Checked      := ArredondamentoPorQtd;
    chECFArredondaMFD.Checked         := ArredondamentoItemMFD;
    chECFDescrGrande.Checked          := DescricaoGrande;
    chECFSinalGavetaInvertido.Checked := GavetaSinalInvertido;
    chECFIgnorarTagsFormatacao.Checked:= IgnorarTagsFormatacao;
    chECFControlePorta.Checked        := ControlePorta;
    edECFLog.Text                     := ArqLog;
  end;

  { Parametros do CHQ }
  with FMonitorConfig.CHQ do
  begin
    cbCHQModelo.ItemIndex             := Modelo;
    cbCHQModeloChange(Self);
    cbCHQPorta.Text                   := Porta;
    chCHQVerForm.Checked              := VerificaFormulario;
    edCHQFavorecido.Text              := Favorecido;
    edCHQCidade.Text                  := Cidade;
    edCHQBemafiINI.Text               := PathBemafiINI;
    CHQDeviceParams                   := SerialParams;
  end;

  { Parametros do GAV }
  with FMonitorConfig.GAV do
  begin
    cbGAVModelo.ItemIndex             := Modelo;
    cbGAVModeloChange(Self);
    cbGAVPorta.Text                   := Porta;
    cbGAVStrAbre.Text                 := StringAbertura;
    sedGAVIntervaloAbertura.Value     := AberturaIntervalo;
    cbGAVAcaoAberturaAntecipada.ItemIndex := AcaoAberturaAntecipada;
  end;

   { Parametros do DIS }
  with FMonitorConfig.DIS do
  begin
    cbDISModelo.ItemIndex             := Modelo;
    cbDISModeloChange(Self);
    cbDISPorta.Text                   := Porta;
    seDISIntervalo.Value              := Intervalo;
    seDISPassos.Value                 := Passos;
    seDISIntByte.Value                := IntervaloEnvioBytes;
  end;

  { Parametros do LCB }
  with FMonitorConfig.LCB do
  begin
    cbLCBPorta.Text                   := Porta;
    cbLCBPortaChange(Self);
    sedLCBIntervalo.Value             := Intervalo;
    cbLCBSufixoLeitor.Text            := SufixoLeitor;
    chLCBExcluirSufixo.Checked        := ExcluirSufixo;
    edLCBPreExcluir.Text              := PrefixoAExcluir;
    cbLCBSufixo.Text                  := SufixoIncluir;
    cbLCBDispositivo.Text             := Dispositivo;
    rbLCBTeclado.Checked              := Teclado;
    rbLCBFila.Checked                 := not rbLCBTeclado.Checked;
    ACBrLCB1.Device.ParamsString      := Device;
  end;

  { Parametros do RFD }
  with FMonitorConfig.RFD do
  begin
    chRFD.Checked                     := GerarRFD;
    chRFDIgnoraMFD.Checked            := IgnoraECF_MFD;
    edRFDDir.Text                     := DirRFD;
  end;

  { Parametros do BAL }
  with FMonitorConfig.BAL do
  begin
    cbBALModelo.ItemIndex             := Modelo;
    cbBALModeloChange(Self);
    cbBALPorta.Text                   := Porta;
    sedBALIntervalo.Value             := Intervalo;
    edBALLog.Text                     := ArqLog;
    ACBrBAL1.Device.ParamsString      := Device;
  end;

  { Parametros do ETQ }
  with FMonitorConfig.ETQ do
  begin
    cbETQModelo.ItemIndex             := Modelo;
    cbETQModeloChange(Self);
    cbETQPorta.Text                   := Porta;
    cbDPI.ItemIndex                   := DPI;
    ckMemoria.Checked                 := LimparMemoria;
    eTemperatura.Text                 := IntToStr(Temperatura);
    eVelocidade.Text                  := IntToStr(Velocidade);
    cbBackFeed.ItemIndex              := BackFeed;
    eMargemEsquerda.Text              := IntToStr(MargemEsquerda);
    cbOrigem.ItemIndex                := Origem;
    cbUnidade.ItemIndex               := Unidade;
    eCopias.Text                      := IntToStr(Copias);
    eAvanco.Text                      := IntToStr(Avanco);
  end;

  { Parametros do TC }
  with FMonitorConfig.TC do
  begin
    cbxTCModelo.ItemIndex             := Modelo;
    cbxTCModeloChange(Self);
    edTCArqPrecos.Text                := IntToStr(TCP_Porta);
    edTCArqPrecos.Text                := Arq_Precos;
    edTCNaoEncontrado.Text            := Nao_Econtrado;
  end;

  { Parametros do CEP }
  with FMonitorConfig.CEP do
  begin
    cbCEPWebService.ItemIndex         := WebService;
    cbCEPWebServiceChange(Self);
    edCEPChaveBuscarCEP.Text          := Chave_BuscarCEP;
    edCONProxyHost.Text               := Proxy_Host;
    edCONProxyPort.Text               := Proxy_Port;
    edCONProxyUser.Text               := Proxy_User;
    edCONProxyPass.Text               := Proxy_Pass;
    ckIBGEAcentos.Checked             := IBGEAcentos;
    ckIBGEUTF8.Checked                := IBGEUTF8;
  end;

    with ACBrIBGE1 do
    begin
      ProxyHost := edCONProxyHost.Text;
      ProxyPort := edCONProxyPort.Text;
      ProxyUser := edCONProxyUser.Text;
      ProxyPass := edCONProxyPass.Text;
      IgnorarCaixaEAcentos:= ckIBGEAcentos.Checked;
      IsUTF8    :=  ckIBGEUTF8.Checked;
    end;

  {Parametros do Boleto}
  with FMonitorConfig.BOLETO do
  begin
    edtBOLRazaoSocial.Text            := Nome;
    cbxBOLF_J.ItemIndex               := Conta.Pessoa;
    edtBOLCNPJ.Text                   := CNPJCPF;
    edtBOLLogradouro.Text             := Logradouro;
    edtBOLNumero.Text                 := Numero;
    edtBOLBairro.Text                 := Bairro;
    edtBOLCEP.Text                    := CEP;

    CarregarListaDeCidades(UFtoCUF(UF));
    cbxBOLUF.ItemIndex                := cbxBOLUF.Items.IndexOf(UF);
    edtBOLCodCidade.Caption           := IntToStr(CodCidade);
    if ( CodCidade = 0 ) then
    begin
      if ( cbxEmitCidade.Items.IndexOf(Cidade) > 0 ) and (fcMunList.IndexOf(IntToStr(CodCidade)) > 0 ) then
        cbxEmitCidade.ItemIndex           := FcMunList.IndexOf(IntToStr(CodCidade))
      else
      begin
        cbxEmitCidade.Items.Add(Cidade);
        cbxEmitCidade.ItemIndex:= cbxEmitCidade.Items.IndexOf(Cidade);
        fcMunList.Add(IntToStrZero(CodCidade,1));

      end;
    end
    else
      cbxEmitCidade.ItemIndex           := FcMunList.IndexOf(IntToStr(CodCidade));

    edtBOLComplemento.Text            := Complemento;


    with Conta do
    begin
      cbxBOLEmissao.ItemIndex          := RespEmis;

      edtModalidade.Text               := Modalidade;
      edtConvenio.Text                 := Convenio;
      cbxBOLBanco.ItemIndex            := Banco;
      edtBOLConta.Text                 := Conta;
      edtBOLDigitoConta.Text           := DigitoConta;
      edtBOLAgencia.Text               := Agencia;
      edtBOLDigitoAgencia.Text         := DigitoAgencia;
      edtBOLDigitoAgConta.Text         := DigitoAgenciaConta;
      edtCodCliente.Text               := CodCedente;
      edtBOLLocalPagamento.Text        := LocalPagamento;
      edtOperacaoBeneficiario.Text     := CodigoOperacao;
    end;

    with RemessaRetorno do
    begin
      deBolDirRemessa.Text             := DirArquivoRemessa;
      deBolDirRetorno.Text             := DirArquivoRetorno;
      edtCodTransmissao.Text           := CodTransmissao;
      cbxCNAB.ItemIndex                := StrToInt(IfThen(CNAB = 0, '1', '0'));
      chkLerBeneficiarioRetorno.Checked     := LerCedenteRetorno;
      chkRemoveAcentos.Checked         := RemoveAcentos;
      edtPrefixRemessa.Text            := PrefixArqRemessa;
      edtVersaoArquivo.Text            := VersaoArquivo;
      edtVersaoLote.Text               := VersaoLote;
    end;

    with Layout do
    begin
      deBOLDirLogo.Text                := DirLogos;
      spBOLCopias.Value                := Copias;
      ckgBOLMostrar.Checked[0]         := Preview;
      ckgBOLMostrar.Checked[1]         := Progresso;
      ckgBOLMostrar.Checked[2]         := Setup;
      cbxBOLLayout.ItemIndex           := Layout;
      cbxBOLFiltro.ItemIndex           := Filtro;
      deBOLDirArquivo.Text             := DirArquivoBoleto;
      edNomeArquivo.Text               := NomeArquivoBoleto;
      cbxBOLImpressora.ItemIndex       := cbxBOLImpressora.Items.IndexOf(Impressora);
    end;

    with PIX do
    begin
      cbxBOLTipoChavePix.ItemIndex     := pix.TipoChavePix;
      edtBOLChavePix.text              := pix.ChavePix;
    end;


    with Relatorio do
    begin
      deBolDirRetornoRel.Text          := deBolDirRetorno.Text;
      chkBOLRelMostraPreview.Checked   := MostraPreviewRelRetorno;
      edtBOLLogoEmpresa.Text           := LogoEmpresa;
    end;

    with Email do
    begin
      edtBOLEmailAssunto.Text          := EmailAssuntoBoleto;
      edtBOLEmailMensagem.Text         := StringToBinaryString(EmailMensagemBoleto);
      cbxBOLEmailMensagemHTML.Checked  := EmailFormatoHTML;
    end;

    with WS.CedenteWS do
     begin
       edtClientID.Text := ClientID;
       edtClientSecret.Text := ClientSecret;
       edtKeyUser.Text := KeyUser;
       edtScope.Text := Scope;
       chkPix.Checked := IndicadorPix;
     end;

     with WS.Config do
     begin
       ChkLogBoletoWeb.Checked := LogRegistro;
       edtPathLogBoleto.Text := PathGravarRegistro;
     end;

     with WS.Config.SSL do
     begin
       rgTipoAmbBoleto.ItemIndex := Ambiente;
       cbOperacaoBoleto.ItemIndex := Operacao;
       edtVersaoBoleto.Text := VersaoDF;
       cbHttpLibBoleto.ItemIndex := HttpLib;
       edtTimeoutWebServicesBoleto.Value := TimeOut ;
       cbSSLTypeBoleto.ItemIndex := SSLType;
       edtBolArquivoKey.Text := ArquivoKEY;
       edtBolArquivoCRT.Text := ArquivoCRT;
     end;

  end;

  {Parametro e-mail}
  with FMonitorConfig.Email do
  begin
    edEmailNome.Text                   := NomeExibicao;
    edEmailEndereco.Text               := Endereco;
    edEmailHost.Text                   := Email;
    edEmailPorta.Value                 := Porta;
    edEmailUsuario.Text                := Usuario;
    edEmailSenha.Text                  := Senha;
    cbEmailSsl.Checked                 := ExigeSSL;
    cbEmailTls.Checked                 := ExigeTLS;
    cbEmailConfirmation.Checked        := Confirmacao;
    cbEmailThread.Checked              := SegundoPlano;
    cbEmailCodificacao.Text            := Codificacao;
    cbEmailHTML.Checked                := HTML;
    spnAttemptsMail.Value              := AttemptsMail;
    spnTimeOutMail.Value               := TimeoutMail;
  end;

  {Parametro Sedex}
  with FMonitorConfig.SEDEX do
  begin
    edtSedexContrato.Text              := Contrato;
    edtSedexSenha.Text                 := SenhaSedex;
  end;

  {Parametro NCM}
  with FMonitorConfig.NCM do
  begin
    edNCMDiretorio.Text := DirNCMSalvar;
    edNCMDiasValidade.Value := DiasValidadeCache;
  end;

  {Parâmetro NFSe}
  with FMonitorConfig.NFSE do
  begin
    cbLayoutNFSe.ItemIndex             := LayoutProvedor;
    edtCodigoCidade.Text               := IntToStr(CodigoMunicipio);
    edtNomeCidade.Text                 := NomeMunicipio;
    edtUFCidade.Text                   := UFMunicipio;
    edtUsuarioNFSe.Text                := Usuario;
    edtSenhaNFSe.Text                  := Senha;
    edtChaveAcessoNFSe.Text            := ChaveAcesso;
    edtChaveAutenticacaoNFSe.Text      := ChaveAutenticacao;
    edtFraseSecretaNFSe.Text           := FraseSecreta;
    edtCNPJEmitenteNFSe.Text           := CNPJEmitente;
    edtIMEmitenteNFSe.Text             := IMEmitente;
    edtNomeEmitenteNFSe.Text           := NomeEmitente;
    cbxMontarPathSchemas.Checked       := MontarAutoPathSchema;
    cbxConsultarLoteAposEnvio.Checked  := ConsultarLoteAposEnvio;
    cbxConsultarAposCancelar.Checked   := ConsultarAposCancelar;
    edtNomePrefeitura.Text             := NomePrefeitura;
    edtCNPJPrefeitura.Text             := CNPJPrefeitura;
  end;

  {Parametro DFe}
  with FMonitorConfig.DFE do
  begin
    ACBrNFeDANFeESCPOS1.PosPrinter.Device.Desativar;
    ACBrBPeDABPeESCPOS1.PosPrinter.Device.Desativar;

    cbRetirarAcentos.Checked           := RetirarAcentos;
    cbRetirarEspacos.Checked           := RetirarEspacos;
    edLogComp.Text                     := Arquivo_Log_Comp;
    cbLogComp.Checked                  := Gravar_Log_Comp;
    sedLogLinhasComp.Value             := Linhas_Log_Comp;
    ArqLogCompTXT                      := AcertaPath(edLogComp.Text);
    edtArquivoWebServicesNFe.Text      := ValidaArquivo(ArquivoWebServices, CACBrNFeServicosIni);
    edtArquivoWebServicesCTe.Text      := ValidaArquivo(ArquivoWebServicesCTe, CACBrCTeServicosIni);
    edtArquivoWebServicesMDFe.Text     := ValidaArquivo(ArquivoWebServicesMDFe, CACBrMDFeServicosIni);
    edtArquivoWebServicesBPe.Text      := ValidaArquivo(ArquivoWebServicesBPe, CACBrBPeServicosIni);
    edtArquivoWebServicesGNRe.Text     := ValidaArquivo(ArquivoWebServicesGNRe, CACBrGNREServicosIni);
    edtArquivoWebServiceseSocial.Text  := ValidaArquivo(ArquivoWebServiceseSocial, CACBreSocialServicosIni);
    edtArquivoWebServicesReinf.Text    := ValidaArquivo(ArquivoWebServicesReinf, CACBrReinfServicosIni);
    edtArquivoWebServicesNFSe.Text     := ValidaArquivo(ArquivoWebServicesNFSe, CACBrNFSeServicosIni);
    cbValidarDigest.Checked            := ValidarDigest;
    edtTimeoutWebServices.Value        := TimeoutWebService;
    cbModoEmissao.Checked              := IgnorarComandoModoEmissao;

    with Certificado do
    begin
      cbSSLLib.ItemIndex               := SSLLib;
      cbCryptLib.ItemIndex             := CryptLib;
      cbHttpLib.ItemIndex              := HttpLib;
      cbXmlSignLib.ItemIndex           := XmlSignLib;
      cbSSLType.ItemIndex              := SSLType;
      edtArquivoPFX.Text               := ArquivoPFX;
      edtURLPFX.Text                   := URLPFX;
      edtNumeroSerie.Text              := NumeroSerie;
      edtSenha.Text                    := Senha;
      chkExibeRazaoSocial.Checked      := ExibeRazaoSocialCertificado;
      chkVerificarValidadeCertificado.Checked := VerificarValidade;
    end;

    with Impressao.Geral do
    begin
      ckSalvar.Checked                 := Salvar;
      edtPathLogs.Text                 := PathSalvar;
      cbxImpressora.ItemIndex          := cbxImpressora.Items.IndexOf(Impressora);
      rgTipoDanfe.ItemIndex            := DANFE;
      edtLogoMarca.Text                := LogoMarca;
      edtLogoMarcaNFCeSAT.Text         := LogoMarcaNFCeSAT;
      edtLogoMarcaPrefeitura.Text      := LogoMarcaPrefeitura;
    end;

    with WebService do
    begin
      cbxTimeZoneMode.ItemIndex        := TimeZoneMode;
      edTimeZoneStr.Caption            := TimeZoneStr;
      cbFormaEmissaoNFe.ItemIndex      := FormaEmissaoNFe;
      cbFormaEmissaoCTe.ItemIndex      := FormaEmissaoCTe;
      cbFormaEmissaoMDFe.ItemIndex     := FormaEmissaoMDFe;
      cbFormaEmissaoBPe.ItemIndex      := FormaEmissaoBPe;
      cbFormaEmissaoGNRe.ItemIndex     := FormaEmissaoGNRe;
      cbxAjustarAut.Checked            := AjustarAut;
      edtAguardar.Text                 := Aguardar;
      edtTentativas.Text               := Tentativas;
      edtIntervalo.Text                := Intervalo;
      cbUF.ItemIndex                   := cbUF.Items.IndexOf(UF);
      {$IFDEF Demo}
      rgTipoAmb.ItemIndex              := CODIGO_HOMOLOGACAO;
      {$ELSE}
      rgTipoAmb.ItemIndex              := Ambiente;
      {$ENDIF}
      cbVersaoWS.ItemIndex             := cbVersaoWS.Items.IndexOf(Versao);
      cbVersaoWSCTe.ItemIndex          := cbVersaoWSCTe.Items.IndexOf(VersaoCTe);
      cbVersaoWSMDFe.ItemIndex         := cbVersaoWSMDFe.Items.IndexOf(VersaoMDFe);
      cbVersaoWSBPe.ItemIndex          := cbVersaoWSBPe.Items.IndexOf(VersaoBPe);
      cbVersaoWSGNRE.ItemIndex         := cbVersaoWSGNRE.Items.IndexOf(VersaoGNRe);
      cbVersaoWSeSocial.ItemIndex      := cbVersaoWSeSocial.Items.IndexOf(VersaoeSocial);
      cbVersaoWsReinf.ItemIndex        := cbVersaoWSReinf.Items.IndexOf(VersaoReinf);
      cbVersaoWSQRCode.ItemIndex       := cbVersaoWSQRCode.Items.IndexOf(VersaoQRCode);
      ckCamposFatObrigatorio.Checked   := CamposFatObrig;
      cbTagRejeicao938.ItemIndex       := TagRejeicao938;

    end;

    with ESocial do
    begin
      edtIDEmpregador.Text             := IdEmpregador;
      edtIDTransmissor.Text            := IdTransmissor;
      cbTipoEmpregador.ItemIndex       := cbTipoEmpregador.Items.IndexOf(TipoEmpregador);
    end;

    with Reinf do
    begin
      edtIDContribuinte.Text             := IdContribuinte;
      edtIDTransmissorReinf.Text         := IdTransmissor;
      cbTipoContribuinte.ItemIndex       := cbTipoContribuinte.Items.IndexOf(TipoContribuinte);
    end;

    with WebService.Proxy do
    begin
      edtProxyHost.Text                := Host;
      edtProxyPorta.Text               := Porta;
      edtProxyUser.Text                := User;
      edtProxySenha.Text               := Pass;
    end;

    with WebService.NFCe do
    begin
      edtIdToken.Text                    := IdToken;
      edtToken.Text                      := Token;
      ckNFCeUsarIntegrador.Checked       := UsarIntegrador;

       ACBrNFe1.Configuracoes.Geral.IdCSC := IdToken;
       ACBrNFe1.Configuracoes.Geral.CSC   := Token;
    end;

    with RespTecnico do
    begin
      edtCSRT.Text                       := CSRT;
      edtIdCSRT.Text                     := idCSRT;
    end;

    with Email do
    begin
      edtEmailAssuntoNFe.Text            := AssuntoNFe;
      mmEmailMsgNFe.Lines.Text           := StringToBinaryString(MensagemNFe);
      edtEmailAssuntoCTe.Text            := AssuntoCTe;
      mmEmailMsgCTe.Lines.Text           := StringToBinaryString(MensagemCTe);
      edtEmailAssuntoMDFe.Text           := AssuntoMDFe;
      mmEmailMsgMDFe.Lines.Text          := StringToBinaryString(MensagemMDFe);
      edtEmailAssuntoNFSe.Text           := AssuntoNFSe;
      mmEmailMsgNFSe.Lines.Text          := StringToBinaryString(MensagemNFSe);
    end;

    with WebService.NFe do
    begin
      edtCNPJContador.Text               := CNPJContador;
    end;

    with Impressao.NFCe.Emissao do
    begin
      rgModeloDANFeNFCE.ItemIndex         := Modelo;
      rgModoImpressaoEvento.ItemIndex     := ModoImpressaoEvento;
      cbxImprimirItem1LinhaNFCe.Checked   := ImprimirItem1Linha;
      cbxImprimirDescAcresItemNFCe.Checked:= ImprimirDescAcresItem;
      cbxImpressoraNFCe.ItemIndex         := cbxImpressoraNFCe.Items.IndexOf(ImpressoraPadrao);
      cbxImprimirQRCodeLateralNFCe.Checked:= QRCodeLateral;
      cbxImprimirCodigoEANNFCe.Checked    := UsaCodigoEanImpressao;
      cbxImprimirNomeFantasiaNFCe.Checked := ImprimeNomeFantasia;
      cbxExibeTotalTributosItem.Checked   := ExibeTotalTributosItem;
      rgImprimeTributos.ItemIndex         := ImprimeTributos;
      cbxImprimirLogoLateralNFCe.Checked  := LogoLateral;
      cbxImprimeItens.Checked             := ImprimeItens;
    end;

    with Impressao.NFCe.Emissao.DANFCe do
    begin
      fspeNFCeMargemInf.Value             := MargemInf ;
      fspeNFCeMargemSup.Value             := MargemSup ;
      fspeNFCeMargemDir.Value             := MargemDir;
      fspeNFCeMargemEsq.Value             := MargemEsq ;
      fspeLarguraNFCe.Value               := LarguraBobina;
    end;

    with Impressao.NFCe.Emissao.DANFCeTipoPagto do
    begin
      chgDescricaoPagamento.Checked[0]   := tipo;
      chgDescricaoPagamento.Checked[1]   := Bandeira;
      chgDescricaoPagamento.Checked[2]   := Autorizacao;
    end;

    with FMonitorConfig.FonteLinha do
    begin
      FontDialog1.Font.Name  := Name;
      FontDialog1.Font.Size  := Size;
      FontDialog1.Font.Color := Color;
      FontDialog1.Font.Style := Style;
    end;

    with Impressao.DANFE do
    begin
      rgModeloDanfe.ItemIndex             := Modelo;
      edtSiteEmpresa.Text                 := Site;
      edtEmailEmpresa.Text                := Email;
      cbxImpDescPorc.Checked              := ImpDescPorc;
      cbxMostrarPreview.Checked           := MostrarPreview;
      edtNumCopia.Value                   := Copias;
      edtNumCopiaNFCe.Value               := CopiasNFCe;
      speLargCodProd.Value                := LarguraCodigoProduto;
      speEspEntreProd.Value               := EspacoEntreProdutos;
      speFonteRazao.Value                 := FonteRazao;
      speFonteEndereco.Value              := FonteEndereco;
      speFonteCampos.Value                := FonteCampos;
      speFonteAdic.Value                  := FonteAdicionais;
      speAlturaCampos.Value               := AlturaCampos;
      fspeMargemInf.Value                 := Margem;
      fspeMargemSup.Value                 := MargemSup;
      fspeMargemDir.Value                 := MargemDir;
      fspeMargemEsq.Value                 := MargemEsq;
      edtPathPDF.Text                     := IfThen( NaoEstaVazio(PathPDF), PathPDF, AcertaPath('PDF'));
      spedtCasasDecimaisQtd.Value         := DecimaisQTD;
      spedtDecimaisVUnit.Value            := DecimaisValor;
      cbxExibeResumo.Checked              := ExibeResumo;
      edtMsgResumoCanhoto.Text            := TextoResumoCanhoto;
      OnMensagemCanhotoNFe;
      cbxImprimirTributos.Checked         := ImprimirTributosItem;
      cbxImpValLiq.Checked                := ImprimirValLiq;
      cbxUnComTributavel.ItemIndex        := UNComercialETributavel;
      cbxFormCont.Checked                 := PreImpresso;
      cbxMostraStatus.Checked             := MostrarStatus;
      cbxExibirEAN.Checked                := ExibirEAN;
      cbxExibirCampoFatura.Checked        := ExibirCampoFatura;
      cbxExpandirLogo.Checked             := ExpandirLogo;
      rgTipoFonte.ItemIndex               := Fonte;
      rgLocalCanhoto.ItemIndex            := LocalCanhoto;
      rgLayoutCanhoto.ItemIndex           := LayoutCanhoto;
      cbxQuebrarLinhasDetalhesItens.Checked:=QuebrarLinhasDetalheItens;
      cbxImpDetEspNFe.Checked             := ImprimirDetalhamentoEspecifico;
      cbxImpDocsReferenciados.Checked     := ImprimirDadosDocReferenciados;
      rgInfAdicProduto.ItemIndex          := ExibirBandInforAdicProduto;
      cbxExibirLogoEmCima.Checked         := LogoEmCima;
      cbxImprimeInscSuframa.Checked       := ImprimeInscSuframa;
      cbxExpandirDadosAdicionaisAuto.Checked:= ExpandirDadosAdicionaisAuto;
      cbxImprimeContinuacaoDadosAdicionaisPrimeiraPagina.Checked:= ImprimeContinuacaoDadosAdicionaisPrimeiraPagina;
      rgImprimeDescAcrescItemNFe.ItemIndex:= ImprimeDescAcrescItemNFe;
      rgInfFormaPagNFe.ItemIndex          := ImprimirCampoFormaPagamento;
      cbxImprimeXPedNitemPed.Checked      := ImprimeXPedNitemPed;
    end;

    with Impressao.DACTE do
    begin
      rgTamanhoPapelDacte.ItemIndex := TamanhoPapel;
    end;

    with Impressao.DAMFE do
    begin
      ckbExibirMunicipioDescarregamento.Checked := ExibirMunicipioDescarregamento;
    end;

    with Diretorios do
    begin
      cbxSalvarArqs.Checked         := Salvar;
      cbxPastaMensal.Checked        := PastaMensal;
      cbxAdicionaLiteral.Checked    := AddLiteral;
      cbxEmissaoPathNFe.Checked     := EmissaoPathNFe;
      cbxSalvaPathEvento.Checked    := SalvarCCeCanPathEvento;
      cbxSepararPorCNPJ.Checked     := SepararPorCNPJ;
      cbxSepararporModelo.Checked   := SepararPorModelo;
      cbxSalvarNFesProcessadas.Checked := SalvarApenasNFesAutorizadas;
      cbxAtualizarXMLCancelado.Checked := AtualizarXMLCancelado;
      cbxNormatizarMunicipios.Checked  := NormatizarMunicipios;
      cbxUsarSeparadorPathPDF.Checked  := UsarSeparadorPathPDF;
      cbxSepararPorNome.Checked        := SepararPorNome;
      edtPathNFe.Text                  := IfThen( NaoEstaVazio(PathNFe), PathNFe, AcertaPath('Arqs'));
      edtPathInu.Text                  := IfThen( NaoEstaVazio(PathInu), PathInu, AcertaPath('Arqs'));
      edtPathDPEC.Text                 := IfThen( NaoEstaVazio(PathDPEC), PathDPEC, AcertaPath('Arqs'));
      edtPathEvento.Text               := IfThen( NaoEstaVazio(PathEvento), PathEvento, AcertaPath('Arqs'));
      edtPathArqTXT.Text               := IfThen( NaoEstaVazio(PathArqTXT), PathArqTXT, AcertaPath('TXT'));
      edtPathDownload.Text             := IfThen( NaoEstaVazio(PathDownload), PathDownload, AcertaPath('Arqs'));
      edtPathSchemasDFe.Text           := IfThen( NaoEstaVazio(PathSchemasDFe), PathSchemasDFe, AcertaPath('Schemas'));

    end;

    SetDFeSSLType;

    SetComumConfig(ACBrNFe1.Configuracoes);
    SetComumConfig(ACBrCTe1.Configuracoes);
    SetComumConfig(ACBrMDFe1.Configuracoes);
    SetComumConfig(ACBrGNRE1.Configuracoes);
    SetComumConfig(ACBrBlocoX1.Configuracoes);
    SetComumConfig(ACBreSocial1.Configuracoes);
    SetComumConfig(ACBrReinf1.Configuracoes);
    SetComumConfig(ACBrBPe1.Configuracoes);
    SetComumConfig(ACBrGTIN1.Configuracoes);
    SetComumConfig(ACBrNFSeX1.Configuracoes);

    AtualizaSSLLibsCombo;
    // Italo;
    AtualizarCidades;
    cbMunicipio.ItemIndex := cbMunicipio.Items.IndexOf(edtNomeCidade.Text + '/' +
                               edtCodigoCidade.Text + '/' + edtUFCidade.Text);

    if EstaVazio(edtAguardar.Text) then
      edtAguardar.Text := IntToStr(ACBrNFe1.Configuracoes.WebServices.AguardarConsultaRet);

    if EstaVazio(edtTentativas.Text) then
      edtTentativas.Text := IntToStr(ACBrNFe1.Configuracoes.WebServices.Tentativas);

    if EstaVazio(edtIntervalo.Text) then
      edtIntervalo.Text := IntToStr(ACBrNFe1.Configuracoes.WebServices.IntervaloTentativas);

    edTimeZoneStr.Caption := ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.TimeZoneStr;
    edTimeZoneStr.Enabled := (ACBrNFe1.Configuracoes.WebServices.TimeZoneConf.ModoDeteccao = tzManual);

    cbxImpDescPorcChange(nil);

    if rgModeloDanfe.ItemIndex = 0 then
    begin
       ACBrNFe1.DANFE           := ACBrNFeDANFeRL1;
       ACBrCTe1.DACTE           := ACBrCTeDACTeRL1;
       ACBrMDFe1.DAMDFE         := ACBrMDFeDAMDFeRL1;
       ACBrGNRE1.GNREGuia       := ACBrGNREGuiaRL1;
       ACBrNFSeX1.DANFSE        := ACBrNFSeXDANFSeRL1;
    end;

    if ckNFCeUsarIntegrador.Checked then
       ACBrNFe1.Integrador          := ACBrIntegrador1
    else
       ACBrNFe1.Integrador          := nil;

    ACBrCTeDACTeRL1.TamanhoPapel    := TpcnTamanhoPapel(rgTamanhoPapelDacte.ItemIndex);

    ACBrCTe1.DACTe.TipoDACTE        := StrToTpImp(OK,IntToStr(rgTipoDanfe.ItemIndex+1));
    ACBrCTe1.DACTe.Logo             := edtLogoMarca.Text;
    ACBrCTe1.DACTe.Sistema          := edSH_RazaoSocial.Text;
    ACBrCTe1.DACTe.Site             := edtSiteEmpresa.Text;
    ACBrCTe1.DACTe.Email            := edtEmailEmpresa.Text;
    ACBrCTe1.DACTe.ImprimeDescPorc  := cbxImpDescPorc.Checked;
    ACBrCTe1.DACTe.MostraPreview    := cbxMostrarPreview.Checked;
    ACBrCTe1.DACTe.Impressora       := cbxImpressora.Text;
    ACBrCTe1.DACTe.NumCopias        := edtNumCopia.Value;
    ACBrCTe1.DACTe.MargemInferior   := fspeMargemInf.Value;
    ACBrCTe1.DACTe.MargemSuperior   := fspeMargemSup.Value;
    ACBrCTe1.DACTe.MargemDireita    := fspeMargemDir.Value;
    ACBrCTe1.DACTe.MargemEsquerda   := fspeMargemEsq.Value;
    ACBrCTe1.DACTe.PathPDF          := edtPathPDF.Text;
    ACBrCTe1.DACTe.MostraStatus     := cbxMostraStatus.Checked;
    ACBrCTe1.DACTe.ExpandeLogoMarca := cbxExpandirLogo.Checked;
    ACBrCTe1.DACTE.UsaSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;
    ACBrCTeDACTeRL1.PosCanhoto         := TPosRecibo( rgLocalCanhoto.ItemIndex );

    ACBrMDFe1.DAMDFe.TipoDAMDFe        := StrToTpImp(OK,IntToStr(rgTipoDanfe.ItemIndex+1));
    ACBrMDFe1.DAMDFe.Logo              := edtLogoMarca.Text;
    ACBrMDFe1.DAMDFe.Sistema           := edSH_RazaoSocial.Text;
    ACBrMDFe1.DAMDFe.Site              := edtSiteEmpresa.Text;
    ACBrMDFe1.DAMDFe.Email             := edtEmailEmpresa.Text;
    ACBrMDFe1.DAMDFe.MostraPreview     := cbxMostrarPreview.Checked;
    ACBrMDFe1.DAMDFe.Impressora        := cbxImpressora.Text;
    ACBrMDFe1.DAMDFe.NumCopias         := edtNumCopia.Value;
    ACBrMDFe1.DAMDFe.MargemInferior    := fspeMargemInf.Value;
    ACBrMDFe1.DAMDFe.MargemSuperior    := fspeMargemSup.Value;
    ACBrMDFe1.DAMDFe.MargemDireita     := fspeMargemDir.Value;
    ACBrMDFe1.DAMDFe.MargemEsquerda    := fspeMargemEsq.Value;
    ACBrMDFe1.DAMDFe.PathPDF           := edtPathPDF.Text;
    ACBrMDFe1.DAMDFe.MostraStatus      := cbxMostraStatus.Checked;
    ACBrMDFe1.DAMDFe.ExpandeLogoMarca  := cbxExpandirLogo.Checked;
    ACBrMDFe1.DAMDFe.UsaSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;
    ACBrMDFe1.DAMDFE.ExibirMunicipioDescarregamento:= ckbExibirMunicipioDescarregamento.Checked;

    ACBrGNRE1.GNREGuia.Sistema         := edSH_RazaoSocial.Text;
    ACBrGNRE1.GNREGuia.Site            := edtSiteEmpresa.Text;
    ACBrGNRE1.GNREGuia.Email           := edtEmailEmpresa.Text;
    ACBrGNRE1.GNREGuia.MostrarPreview  := cbxMostrarPreview.Checked;
    ACBrGNRE1.GNREGuia.Impressora      := cbxImpressora.Text;
    ACBrGNRE1.GNREGuia.NumCopias       := edtNumCopia.Value;
    ACBrGNRE1.GNREGuia.MargemInferior  := fspeMargemInf.Value;
    ACBrGNRE1.GNREGuia.MargemSuperior  := fspeMargemSup.Value;
    ACBrGNRE1.GNREGuia.MargemDireita   := fspeMargemDir.Value;
    ACBrGNRE1.GNREGuia.MargemEsquerda  := fspeMargemEsq.Value;
    ACBrGNRE1.GNREGuia.PathPDF         := edtPathPDF.Text;
    ACBrGNRE1.GNREGuia.MostrarStatus   := cbxMostraStatus.Checked;

    ACBrBPe1.DABPe.TipoDABPe         := StrToTpImp(OK,IntToStr(rgTipoDanfe.ItemIndex+1));
    ACBrBPe1.DABPe.Logo              := edtLogoMarca.Text;
    ACBrBPe1.DABPe.Sistema           := edSH_RazaoSocial.Text;
    ACBrBPe1.DABPe.Site              := edtSiteEmpresa.Text;
    ACBrBPe1.DABPe.Email             := edtEmailEmpresa.Text;
    ACBrBPe1.DABPe.MostraPreview     := cbxMostrarPreview.Checked;
    ACBrBPe1.DABPe.Impressora        := cbxImpressora.Text;
    ACBrBPe1.DABPe.NumCopias         := edtNumCopia.Value;
    ACBrBPe1.DABPe.MargemInferior    := fspeMargemInf.Value;
    ACBrBPe1.DABPe.MargemSuperior    := fspeMargemSup.Value;
    ACBrBPe1.DABPe.MargemDireita     := fspeMargemDir.Value;
    ACBrBPe1.DABPe.MargemEsquerda    := fspeMargemEsq.Value;
    ACBrBPe1.DABPe.PathPDF           := edtPathPDF.Text;
    ACBrBPe1.DABPe.MostraStatus      := cbxMostraStatus.Checked;
    ACBrBPe1.DABPe.ExpandeLogoMarca  := cbxExpandirLogo.Checked;
    ACBrBPe1.DABPe.UsaSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;

    ACBrNFSeX1.DANFSe.TipoDANFSE        := tpPadrao; //StrToTpImp(OK,IntToStr(rgTipoDanfe.ItemIndex+1));
    ACBrNFSeX1.DANFSe.Logo              := edtLogoMarcaPrefeitura.Text;
    ACBrNFSeX1.DANFSe.Sistema           := edSH_RazaoSocial.Text;
    ACBrNFSeX1.DANFSe.Site              := edtSiteEmpresa.Text;
    ACBrNFSeX1.DANFSe.Email             := edtEmailEmpresa.Text;
    ACBrNFSeX1.DANFSe.MostraPreview     := cbxMostrarPreview.Checked;
    ACBrNFSeX1.DANFSe.Impressora        := cbxImpressora.Text;
    ACBrNFSeX1.DANFSe.NumCopias         := edtNumCopia.Value;
    ACBrNFSeX1.DANFSe.MargemInferior    := fspeMargemInf.Value;
    ACBrNFSeX1.DANFSe.MargemSuperior    := fspeMargemSup.Value;
    ACBrNFSeX1.DANFSe.MargemDireita     := fspeMargemDir.Value;
    ACBrNFSeX1.DANFSe.MargemEsquerda    := fspeMargemEsq.Value;
    ACBrNFSeX1.DANFSe.PathPDF           := edtPathPDF.Text;
    ACBrNFSeX1.DANFSe.MostraStatus      := cbxMostraStatus.Checked;
    ACBrNFSeX1.DANFSe.ExpandeLogoMarca  := cbxExpandirLogo.Checked;
    ACBrNFSeX1.DANFSe.Prefeitura        := edtNomePrefeitura.Text;
    ACBrNFSeX1.DANFSe.Prestador.Logo    := edtLogoMarca.Text;

    VerificaDiretorios;

    ACBrNFeDANFeESCPOS1.PosPrinter.Device.Ativo := ESCPOSAtivado;
    ACBrBPeDABPeESCPOS1.PosPrinter.Device.Ativo := ESCPOSAtivadoBPe;

  end;

  {Parametro SAT}

  with FMonitorConfig.SAT do
  begin
    ConsultarModeloSAT;
    cbxModeloSAT.ItemIndex             := Modelo;
    edSATLog.Text                      := ArqLog;
    edNomeDLL.Text                     := NomeDLL;
    edtCodigoAtivacao.Text             := CodigoAtivacao;
    edtCodUF.Text                      := CodigoUF;
    seNumeroCaixa.Value                := NumeroCaixa;
    {$IFDEF Demo}
    cbxAmbiente.ItemIndex              := CODIGO_HOMOLOGACAO;
    {$ELSE}
    cbxAmbiente.ItemIndex              := Ambiente;
    {$ENDIF}
    sePagCod.Value                     := PaginaDeCodigo;
    sePagCodChange(self);
    sfeVersaoEnt.Value                 := versaoDadosEnt;
    cbxFormatXML.Checked               := FormatarXML;
    edSATPathArqs.Text                 := PathCFe;
    cbxSATSalvarCFe.Checked            := SalvarCFe;
    cbxSATSalvarCFeCanc.Checked        := SalvarCFeCanc;
    cbxSATSalvarEnvio.Checked          := SalvarEnvio;
    cbxSATSepararPorCNPJ.Checked       := SepararPorCNPJ;
    cbxSATSepararPorMES.Checked        := SepararPorMES;
    cbxSATSepararPorANO.Checked        := SepararPorANO;
    cbxSATSepararPorDIA.Checked        := SepararPorDIA;
    cbxSATSepararporModelo.Checked     := SepararPorModelo;
    cbxValidarNumeroSessaoResposta.Checked:= ValidarNumeroSessaoResposta;
    edSATPathArqsCanc.Text             := ifthen((PathCFeCanc = '') and (PathCFe <> ''), PathCFe, PathCFeCanc);
    edSATPathArqsEnvio.Text            := ifthen((PathCFeEnvio = '') and (PathCFe <> ''), PathCFe, PathCFeEnvio);
    edSATPrefixoCFe.Text               := PrefixoArqCFe;
    edSATPrefixoCFeCanc.Text           := PrefixoArqCFeCanc;

    with SATImpressao.SATExtrato do
    begin
      ACBrSATExtratoESCPOS1.PosPrinter.Device.ParamsString := ParamsString;
      ACBrSATExtratoESCPOS1.ImprimeDescAcrescItem          := ImprimeDescAcrescItem;
      ACBrSATExtratoESCPOS1.ImprimeEmUmaLinha              := ImprimeEmUmaLinha;
      ACBrSATExtratoESCPOS1.ImprimeCodigoEan               := UsaCodigoEanImpressao;
      ACBrSATExtratoESCPOS1.ImprimeQRCodeLateral           := ImprimeQRCodeLateral;
      ACBrSATExtratoESCPOS1.ImprimeLogoLateral             := ImprimeLogoLateral;
      ACBrSATExtratoESCPOS1.CasasDecimais.qCom             := ExtratoDecimaisQTD;
      ACBrSATExtratoESCPOS1.CasasDecimais.vUnCom           := ExtratoDecimaisValor;
      ACBrSATExtratoESCPOS1.CasasDecimais.MaskqCom         := ExtratoMaskQTD;
      ACBrSATExtratoESCPOS1.CasasDecimais.MaskvUnCom       := ExtratoMaskValor;
      ACBrSATExtratoESCPOS1.CasasDecimais.Formato          := TDetFormato( cbFormatoDecimais.ItemIndex );

      ACBrSATExtratoFortes1.ImprimeDescAcrescItem          := ImprimeDescAcrescItem;
      ACBrSATExtratoFortes1.ImprimeEmUmaLinha              := ImprimeEmUmaLinha;
      ACBrSATExtratoFortes1.ImprimeCodigoEan               := UsaCodigoEanImpressao;
      ACBrSATExtratoFortes1.ImprimeQRCodeLateral           := ImprimeQRCodeLateral;
      ACBrSATExtratoFortes1.ImprimeLogoLateral             := ImprimeLogoLateral;
      ACBrSATExtratoFortes1.CasasDecimais.qCom             := ExtratoDecimaisQTD;
      ACBrSATExtratoFortes1.CasasDecimais.vUnCom           := ExtratoDecimaisValor;
      ACBrSATExtratoFortes1.CasasDecimais.MaskqCom         := ExtratoMaskQTD;
      ACBrSATExtratoFortes1.CasasDecimais.MaskvUnCom       := ExtratoMaskValor;
      ACBrSATExtratoFortes1.CasasDecimais.Formato          := TDetFormato( cbFormatoDecimais.ItemIndex );

      cbxImprimirDescAcresItemSAT.Checked   := ACBrSATExtratoESCPOS1.ImprimeDescAcrescItem;
      cbxImprimirItem1LinhaSAT.Checked      := ACBrSATExtratoESCPOS1.ImprimeEmUmaLinha;
      cbxImprimirCodEANitemSAT.Checked      := ACBrSATExtratoESCPOS1.ImprimeCodigoEan;
      cbxQRCodeLateral.Checked              := ACBrSATExtratoESCPOS1.ImprimeQRCodeLateral;
      cbxLogoLateral.Checked                := ACBrSATExtratoESCPOS1.ImprimeLogoLateral;
      spedtSATCasasDecimaisQtd.Value        := ExtratoDecimaisQTD;
      spedtSATDecimaisVUnit.Value           := ExtratoDecimaisValor;
      edtSATCasasMaskQtd.Text               := ExtratoMaskQTD;
      edtSATMaskVUnit.Text                  := ExtratoMaskValor;
      cbFormatoDecimais.ItemIndex           := FormatoDecimal;
      OnFormataDecimalSAT;

      rdgImprimeChave1LinhaSAT.ItemIndex    := ImprimeChaveEmUmaLinha;
      ACBrSATExtratoESCPOS1.ImprimeChaveEmUmaLinha := TAutoSimNao(rdgImprimeChave1LinhaSAT.ItemIndex);
    end;

    with SATImpressao.SATEmit do
    begin
      edtEmitCNPJ.Text                      := CNPJ;
      edtEmitIE.Text                        := IE;
      edtEmitIM.Text                        := IM;
      cbxRegTributario.ItemIndex            := RegTributario;
      cbxRegTribISSQN.ItemIndex             := RegTribISSQN;
      cbxIndRatISSQN.ItemIndex              := IndRatISSQN;
    end;

    with SATImpressao.SATFortes do
    begin
      cbUsarFortes.Checked                  := UsarFortes;
      cbUsarEscPos.Checked                  := not cbUsarFortes.Checked;
      seLargura.Value                       := Largura;
      seMargemTopo.Value                    := MargemTopo;
      seMargemFundo.Value                   := MargemFundo;
      seMargemEsquerda.Value                := MargemEsquerda;
      seMargemDireita.Value                 := MargemDireita;
      cbPreview.Checked                     := Preview;
    end;

    with SATImpressao.SATPrinter do
    begin
      lImpressora.Caption                   := Name;
    end;

    with SATRede do
    begin
      rgRedeTipoInter.ItemIndex             := tipoInter;
      rgRedeTipoLan.ItemIndex               := tipoLan;
      edRedeSSID.Text                       := SSID;
      cbxRedeSeg.ItemIndex                  := seg;
      edRedeCodigo.Text                     := codigo;
      edRedeIP.Text                         := lanIP;
      edRedeMask.Text                       := lanMask;
      edRedeGW.Text                         := lanGW;
      edRedeDNS1.Text                       := lanDNS1;
      edRedeDNS2.Text                       := lanDNS2;;
      edRedeUsuario.Text                    := usuario;
      edRedeSenha.Text                      := senha;
      cbxRedeProxy.ItemIndex                := proxy;
      edRedeProxyIP.Text                    := proxy_ip;
      edRedeProxyPorta.Value                := proxy_porta;
      edRedeProxyUser.Text                  := proxy_user;
      edRedeProxySenha.Text                 := proxy_senha;
    end;

    with SATSWH do
    begin
      edtSwHCNPJ.Text       := CNPJ;
      edtSwHAssinatura.Text := Assinatura;
    end;

    with SATEmail do
    begin
      edtEmailAssuntoSAT.Text            := AssuntoSAT;
      mmEmailMsgSAT.Lines.Text           := StringToBinaryString(MensagemSAT);
    end;

    AjustaACBrSAT;

  end;

  {Parâmetro Integrador}
  with FMonitorConfig.IntegradorFiscal do
  begin
    edMFEInput.Text           := Input;
    edMFEOutput.Text          := Output;
    seMFETimeout.Value        := Timeout;
  end;

   {Parâmetro PosPrinter}
  with FMonitorConfig.PosPrinter do
  begin
    POSPrDeviceParams       :=  SerialParams;
    cbxModelo.ItemIndex     :=  Modelo;
    cbxPorta.Text           :=  Porta;
    seColunas.Value         :=  Colunas;
    seEspacosLinhas.Value   :=  EspacoEntreLinhas;
    seBuffer.Value          :=  LinhasBuffer;
    seLinhasPular.Value     :=  LinhasPular;
    cbxPagCodigo.ItemIndex  :=  PaginaDeCodigo;
    cbControlePorta.Checked :=  ControlePorta;
    cbCortarPapel.Checked   :=  CortarPapel;

    cbTraduzirTags.Checked  :=  TraduzirTags;
    cbIgnorarTags.Checked   :=  IgnorarTags;
    edPosPrinterLog.Text    :=  ArqLog;

    with CodigoBarras do
    begin
      seCodBarrasLargura.Value :=  Largura;
      seCodBarrasAltura.Value  :=  Altura;
      cbHRI.Checked            :=  HRI;
    end;

    with QRCode do
    begin
      seQRCodeTipo.Value       :=  Tipo;
      seQRCodeLargMod.Value    :=  LarguraModulo;
      seQRCodeErrorLevel.Value :=  ErrorLevel;
    end;

    with Logo do
    begin
      cbEscPosImprimirLogo.Checked := Imprimir;
      seLogoKC1.Value              := KC1;
      seLogoKC2.Value              := KC2;
      seLogoFatorX.Value           := FatorX;
      seLogoFatorY.Value           := FatorY;
    end;

    with Gaveta do
    begin
      seGavetaTempoON.Value   := TempoON;
      seGavetaTempoOFF.Value  := TempoOFF;
      cbGavetaSinalInvertido.Checked  := SinalInvertido;
    end;

    DefineTextoTrayTitulo;

    ConfiguraPosPrinter(POSPrDeviceParams);
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
    Modelo := TACBrETQModelo(cbETQModelo.ItemIndex);
    Porta := cbETQPorta.Text;
    DPI           := TACBrETQDPI(cbDPI.ItemIndex);
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
    IsHTML := cbEmailHTML.Checked;
    Attempts := spnAttemptsMail.Value;
    TimeOut := spnTimeOutMail.Value;
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
    Cedente.Cidade := cbxEmitCidade.Text;
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
    Cedente.DigitoVerificadorAgenciaConta := edtBOLDigitoAgConta.Text;
    Cedente.Modalidade := edtModalidade.Text;
    Cedente.Operacao := edtOperacaoBeneficiario.Text;
    Cedente.PIX.Chave :=edtBOLChavePix.Text;
    Cedente.PIX.TipoChavePIX :=  TACBrPIXTipoChave(cbxBOLTipoChavePix.ItemIndex);

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
    LeCedenteRetorno:= chkLerBeneficiarioRetorno.Checked;
    RemoveAcentosArqRemessa:= chkRemoveAcentos.Checked;
    PrefixArqRemessa:= edtPrefixRemessa.Text;

    MAIL := ACBrMail1;

    //Configurações Boleto Web

    Cedente.CedenteWS.ClientID := edtClientID.Text;
    Cedente.CedenteWS.ClientSecret := edtClientSecret.Text;
    Cedente.CedenteWS.KeyUser := edtKeyUser.Text;
    Cedente.CedenteWS.Scope := edtScope.Text;
    Cedente.CedenteWS.IndicadorPix := ChkPix.Checked;

    Configuracoes.Arquivos.LogRegistro := ChkLogBoletoWeb.Checked;
    Configuracoes.Arquivos.PathGravarRegistro := PathWithoutDelim(edtPathLogBoleto.Text);

    Configuracoes.WebService.Ambiente := TpcnTipoAmbiente( rgTipoAmbBoleto.ItemIndex );
    Configuracoes.WebService.Operacao := TOperacao( cbOperacaoBoleto.ItemIndex );
    Configuracoes.WebService.VersaoDF := edtVersaoBoleto.Text;
    Configuracoes.WebService.SSLHttpLib := TSSLHttpLib( cbHttpLibBoleto.ItemIndex );
    Configuracoes.WebService.TimeOut := edtTimeoutWebServicesBoleto.Value * 1000;
    Configuracoes.WebService.SSLType := TSSLType( cbSSLTypeBoleto.ItemIndex );
    Configuracoes.WebService.ArquivoKEY:=edtBolArquivoKey.Text;
    Configuracoes.WebService.ArquivoCRT:=edtBolArquivoCRT.Text;

  end;

  with ACBrBoleto1.ACBrBoletoFC do
  begin
    Filtro := TACBrBoletoFCFiltro(cbxBOLFiltro.ItemIndex);
    LayOut := TACBrBolLayOut(cbxBOLLayout.ItemIndex);

    NumCopias := spBOLCopias.Value;
    SoftwareHouse := edSH_RazaoSocial.Text;
    DirLogo := deBOLDirLogo.Text;
    MostrarPreview := ckgBOLMostrar.Checked[0];
    MostrarProgresso := ckgBOLMostrar.Checked[1];
    MostrarSetup := ckgBOLMostrar.Checked[2];
    PrinterName := cbxBOLImpressora.Text;

    AlterarEscalaPadrao:= FMonitorConfig.BOLETO.Layout.AlteraEscala;
    NovaEscala := FMonitorConfig.BOLETO.Layout.Escala;

    wDirArquivo := Trim(deBOLDirArquivo.Text);
    if wDirArquivo = '' then
      wDirArquivo := ExtractFilePath(Application.ExeName)
    else
      wDirArquivo := PathWithDelim(wDirArquivo);

    wNomeArquivo:= edNomeArquivo.Text;

    if wNomeArquivo <> '' then
      wPathArquivo:= wDirArquivo + wNomeArquivo
    else
      wPathArquivo:= wDirArquivo + 'boleto';

    if Filtro = TACBrBoletoFCFiltro(fiHTML) then
      NomeArquivo := wPathArquivo + '.html'
    else
      NomeArquivo := wPathArquivo + '.pdf';
    
  end;

  LigarAlertasdeErrosDeConfiguracao;

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
    //AddLinesLog('ATENÇÃO: Arquivo "swh.ini" não encontrado.' +
      //sLineBreak + '     Nenhuma Chave RSA Privada pode ser lida.' + sLineBreak);
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
    //AddLinesLog(mRSAKey.Text + sLineBreak);
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
  OldMonitoraTXT, OldMonitoraTCP, OldMonitoraPasta: boolean;
  //OldVersaoSSL : Integer;
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

  if (Trim(edNCMDiretorio.Text) <> EmptyStr) then
  begin
    if (not DirectoryExists(edNCMDiretorio.Text)) then
    begin
      btnNCMClick(btnNCM); //pgConfig.ActivePageIndex := 15;
      edNCMDiretorio.SetFocus;
      raise Exception.Create('Diretorio para salvar arquivo de NCM nao encontrado');
    end;
  end;

  SalvarSW;

  if chRFD.Checked then
  begin
    with ACBrRFD1 do
    begin
      if Ativo then
      begin
        CONT_RazaoSocial                := edUSURazaoSocial.Text;
        CONT_CNPJ                       := edUSUCNPJ.Text;
        CONT_Endereco                   := edUSUEndereco.Text;
        CONT_IE                         := edUSUIE.Text;
        CONT_NumUsuario                 := seUSUNumeroCadastro.Value;
        CONT_DataHoraCadastro           := deUSUDataCadastro.Date;
        try
          CONT_DataHoraCadastro         := CONT_DataHoraCadastro
                                        + StrToTime(meUSUHoraCadastro.Text, ':');
        except
        end;
        CONT_CROCadastro                := seUSUCROCadastro.Value;
        CONT_GTCadastro                 := seUSUGTCadastro.Value;
        ECF_RFDID                       := lRFDID.Caption;
        ECF_DataHoraSwBasico            := deRFDDataSwBasico.Date;
        try
          ECF_DataHoraSwBasico          := ECF_DataHoraSwBasico
                                        + StrToTime(meRFDHoraSwBasico.Text, ':');
        except
        end;

        GravarINI;

        AddLinesLog('Dados do RFD gravados com sucesso');
      end;
    end;
  end;

  try
    {Parametros do Monitor }
    With FMonitorConfig.ACBrMonitor do
    begin
      // Verificando se modificou o Modo de Monitoramento //
      OldMonitoraTCP   := Modo_TCP;
      OldMonitoraTXT   := Modo_TXT;
      OldMonitoraPasta := MonitoraPasta;
      //OldVersaoSSL     := VersaoSSL;

      Modo_TCP         := rbTCP.Checked;
      Modo_TXT         := rbTXT.Checked;
      MonitoraPasta    := cbMonitorarPasta.Checked;
      TCP_Porta        := StrToIntDef(edPortaTCP.Text, 3434);
      TCP_TimeOut      := StrToIntDef(edTimeOutTCP.Text, 10000);
      Converte_TCP_Ansi:= chbTCPANSI.Checked;

      if cbMonitorarPasta.Checked then
      begin
        if (not DirectoryExists(edEntTXT.Text)) or (not DirectoryExists(edSaiTXT.Text))  then
        begin
          cbMonitorarPasta.Checked := False;
          MonitoraPasta := False;
          raise Exception.Create('Diretorio para monitorar pasta não encontrado! Verifique os Campos: Entrada, Saida.');
        end;

        TXT_Entrada    := PathWithDelim(edEntTXT.Text);
        TXT_Saida      := PathWithDelim(edSaiTXT.Text);
      end
      else
      begin
        TXT_Entrada    := edEntTXT.Text;
        TXT_Saida      := edSaiTXT.Text;
      end;

      Converte_TXT_Entrada_Ansi   := chbArqEntANSI.Checked;
      Converte_TXT_Saida_Ansi     := chbArqSaiANSI.Checked;
      Intervalo                   := sedIntervalo.Value;
      HashSenha                   := IntToStrZero(fsHashSenha, 8);

      Gravar_Log                  := cbLog.Checked;
      Arquivo_Log                 := edLogArq.Text;
      Linhas_Log                  := sedLogLinhas.Value;
      Comandos_Remotos            := cbComandos.Checked;
      Uma_Instancia               := cbUmaInstancia.Checked;
      MostraAbas                  := cbAbas.Checked;
      MostrarNaBarraDeTarefas     := cbMostrarNaBarraDeTarefas.Checked;
      RetirarAcentosNaResposta    := cbRetirarAcentosNaResposta.Checked;
      MostraLogEmRespostasEnviadas:= chkMostraLogNaTela.Checked;
      TipoResposta                := cbTipoResposta.ItemIndex;
    end;

    { Parametros do ECF }
    with FMonitorConfig.ECF do
    begin
      Modelo                      := max(cbECFModelo.ItemIndex - 1, 0);
      Porta                       := cbECFPorta.Text;
      SerialParams                := ACBrECF1.Device.ParamsString;
      Timeout                     := sedECFTimeout.Value;
      IntervaloAposComando        := sedECFIntervalo.Value;
      MaxLinhasBuffer             := sedECFMaxLinhasBuffer.Value;
      PaginaCodigo                := sedECFPaginaCodigo.Value;
      LinhasEntreCupons           := sedECFLinhasEntreCupons.Value;
      ArredondamentoPorQtd        := chECFArredondaPorQtd.Checked;
      ArredondamentoItemMFD       := chECFArredondaMFD.Checked;
      DescricaoGrande             := chECFDescrGrande.Checked;
      GavetaSinalInvertido        := chECFSinalGavetaInvertido.Checked;
      IgnorarTagsFormatacao       := chECFIgnorarTagsFormatacao.Checked;
      ControlePorta               := chECFControlePorta.Checked;
      ArqLog                      := edECFLog.Text;
    end;

    { Parametros do CHQ }
    with FMonitorConfig.CHQ do
    begin
      Modelo                      := cbCHQModelo.ItemIndex;
      Porta                       := cbCHQPorta.Text;
      SerialParams                := ACBrCHQ1.Device.ParamsString;
      VerificaFormulario          := chCHQVerForm.Checked;
      Favorecido                  := edCHQFavorecido.Text;
      Cidade                      := edCHQCidade.Text;
      PathBemafiINI               := edCHQBemafiINI.Text;
    end;

    { Parametros do GAV }
    with FMonitorConfig.GAV do
    begin
      Modelo                      := cbGAVModelo.ItemIndex;
      Porta                       := cbGAVPorta.Text;
      StringAbertura              := cbGAVStrAbre.Text;
      AberturaIntervalo           := sedGAVIntervaloAbertura.Value;
      AcaoAberturaAntecipada      := cbGAVAcaoAberturaAntecipada.ItemIndex;
    end;

    { Parametros do DIS }
    with FMonitorConfig.DIS do
    begin
      Modelo                      := cbDISModelo.ItemIndex;
      Porta                       := cbDISPorta.Text;
      Intervalo                   := seDISIntervalo.Value;
      Passos                      := seDISPassos.Value;
      IntervaloEnvioBytes         := seDISIntByte.Value;
    end;

    { Parametros do LCB }
    with FMonitorConfig.LCB do
    begin
      Porta                       := cbLCBPorta.Text;
      Intervalo                   := sedLCBIntervalo.Value;
      SufixoLeitor                := cbLCBSufixoLeitor.Text;
      ExcluirSufixo               := chLCBExcluirSufixo.Checked;
      PrefixoAExcluir             := edLCBPreExcluir.Text;
      SufixoIncluir               := cbLCBSufixo.Text;
      Dispositivo                 := cbLCBDispositivo.Text;
      Teclado                     := rbLCBTeclado.Checked;
      Device                      := ACBrLCB1.Device.ParamsString;
    end;

    { Parametros do RFD }
    with FMonitorConfig.RFD do
    begin
      GerarRFD                    := chRFD.Checked;
      DirRFD                      := edRFDDir.Text;
      IgnoraECF_MFD               := chRFDIgnoraMFD.Checked;
    end;

    { Parametros do BAL }
    with FMonitorConfig.BAL do
    begin
      Modelo                      := cbBALModelo.ItemIndex;
      Porta                       := cbBALPorta.Text;
      Intervalo                   := sedBALIntervalo.Value;
      ArqLog                      := edBALLog.Text;
      Device                      := ACBrBAL1.Device.ParamsString;
    end;

    { Parametros do ETQ }
    with FMonitorConfig.ETQ do
    begin
      Modelo                      := cbETQModelo.ItemIndex;
      Porta                       := cbETQPorta.Text;
      DPI                         := cbDPI.ItemIndex;
      LimparMemoria               := ckMemoria.Checked;
      Temperatura                 := StrToIntDef(eTemperatura.Text, 10);
      Velocidade                  := StrToIntDef(eVelocidade.Text, -1);
      BackFeed                    := cbBackFeed.ItemIndex;
      MargemEsquerda              := StrToIntDef(eMargemEsquerda.Text, 10);
      Origem                      := cbOrigem.ItemIndex;
      Unidade                     := cbUnidade.ItemIndex;
      Copias                      := StrToIntDef(eCopias.Text, 1);
      Avanco                      := StrToIntDef(eAvanco.Text, 0);
    end;

    { Parametros do CEP }
    with FMonitorConfig.CEP do
    begin
      WebService                  := cbCEPWebService.ItemIndex;
      Chave_BuscarCEP             := edCEPChaveBuscarCEP.Text;
      Proxy_Host                  := edCONProxyHost.Text;
      Proxy_Port                  := edCONProxyPort.Text;
      Proxy_User                  := edCONProxyUser.Text;
      Proxy_Pass                  := edCONProxyPass.Text;
      IBGEAcentos                 := ckIBGEAcentos.Checked;
      IBGEUTF8                    := ckIBGEUTF8.Checked;
    end;

    { Parametros do TC }
    with FMonitorConfig.TC do
    begin
      Modelo                      := cbxTCModelo.ItemIndex;
      TCP_Porta                   := StrToIntDef(edTCArqPrecos.Text, 6500);
      Arq_Precos                  := edTCArqPrecos.Text;
      Nao_Econtrado               := edTCNaoEncontrado.Text;
    end;

    { Parametros de EMAIL }
    with FMonitorConfig.Email do
    begin
      NomeExibicao            := edEmailNome.Text;
      Endereco                := edEmailEndereco.Text;
      Email                   := edEmailHost.Text;
      Usuario                 := edEmailUsuario.Text;
      Senha                   := edEmailSenha.Text ;
      Porta                   := edEmailPorta.Value;
      ExigeSSL                := cbEmailSsl.Checked;
      ExigeTLS                := cbEmailTls.Checked;
      Confirmacao             := cbEmailConfirmation.Checked;
      SegundoPlano            := cbEmailThread.Checked;
      Codificacao             := cbEmailCodificacao.Text;
      HTML                    := cbEmailHTML.Checked;
      AttemptsMail            := spnAttemptsMail.Value;
      TimeoutMail             := spnTimeOutMail.Value;
    end;

    { Parametros Sedex }
    with FMonitorConfig.SEDEX do
    begin
      Contrato                := edtSedexContrato.Text;
      SenhaSedex              := edtSedexSenha.Text;
    end;

    { Parametros NCM }
    with FMonitorConfig.NCM do
    begin
      DirNCMSalvar := edNCMDiretorio.Text;
      DiasValidadeCache := edNCMDiasValidade.Value;
    end;

    { Parametros NFSe }
    with FMonitorConfig.NFSe do
    begin
      LayoutProvedor         := cbLayoutNFSe.ItemIndex;
      CodigoMunicipio        := StrToIntDef(edtCodigoCidade.Text, 0);
      NomeMunicipio          := edtNomeCidade.Text;
      UFMunicipio            := edtUFCidade.Text;
      Usuario                := edtUsuarioNFSe.Text;
      Senha                  := edtSenhaNFSe.Text;
      ChaveAcesso            := edtChaveAcessoNFSe.Text;
      ChaveAutenticacao      := edtChaveAutenticacaoNFSe.Text;
      FraseSecreta           := edtFraseSecretaNFSe.Text;
      CNPJEmitente           := edtCNPJEmitenteNFSe.Text;
      IMEmitente             := edtIMEmitenteNFSe.Text;
      NomeEmitente           := edtNomeEmitenteNFSe.Text;
      MontarAutoPathSchema   := cbxMontarPathSchemas.Checked;
      ConsultarLoteAposEnvio := cbxConsultarLoteAposEnvio.Checked;
      ConsultarAposCancelar  := cbxConsultarAposCancelar.Checked;
      NomePrefeitura         := edtNomePrefeitura.Text;
      CNPJPrefeitura         := edtCNPJPrefeitura.Text;
    end;

    { Parametros DFe }
    with FMonitorConfig.DFE do
    begin
      IgnorarComandoModoEmissao := cbModoEmissao.Checked;
      RetirarAcentos            := cbRetirarAcentos.Checked;
      RetirarEspacos            := cbRetirarEspacos.Checked;
      Gravar_Log_Comp           := cbLogComp.Checked;
      Arquivo_Log_Comp          := edLogComp.Text;
      Linhas_Log_Comp           := sedLogLinhasComp.Value;
      ArquivoWebServices        := edtArquivoWebServicesNFe.Text;
      ArquivoWebServicesCTe     := edtArquivoWebServicesCTe.Text;
      ArquivoWebServicesMDFe    := edtArquivoWebServicesMDFe.Text;
      ArquivoWebServicesBPe     := edtArquivoWebServicesBPe.Text;
      ArquivoWebServicesGNRe    := edtArquivoWebServicesGNRe.Text;
      ArquivoWebServiceseSocial := edtArquivoWebServiceseSocial.Text;
      ArquivoWebServicesReinf   := edtArquivoWebServicesReinf.Text;
      ValidarDigest             := cbValidarDigest.Checked;
      TimeoutWebService         := edtTimeoutWebServices.Value;

      with Certificado do
      begin
        SSLLib                  := cbSSLLib.ItemIndex;
        CryptLib                := cbCryptLib.ItemIndex;
        HttpLib                 := cbHttpLib.ItemIndex;
        XmlSignLib              := cbXmlSignLib.ItemIndex;
        SSLType                 := cbSSLType.ItemIndex;
        ArquivoPFX              := edtArquivoPFX.Text;
        URLPFX                  := edtURLPFX.Text;
        NumeroSerie             := edtNumeroSerie.Text;
        Senha                   := edtSenha.Text;
        ExibeRazaoSocialCertificado := chkExibeRazaoSocial.Checked;
        VerificarValidade           := chkVerificarValidadeCertificado.Checked;
      end;

      with Impressao.Geral do
      begin
        DANFE                    := rgTipoDanfe.ItemIndex;
        LogoMarca                := edtLogoMarca.Text;
        LogoMarcaNFCeSAT         := edtLogoMarcaNFCeSAT.Text;
        Salvar                   := ckSalvar.Checked;
        PathSalvar               := edtPathLogs.Text;
        Impressora               := cbxImpressora.Text;
        LogoMarcaPrefeitura      := edtLogoMarcaPrefeitura.Text;
      end;

      with WebService do
      begin
        UF                       := cbUF.Text;
        Ambiente                 := rgTipoAmb.ItemIndex;
        Versao                   := cbVersaoWS.Text;
        VersaoCTe                := cbVersaoWSCTe.Text;
        VersaoMDFe               := cbVersaoWSMDFe.Text;
        VersaoBPe                := cbVersaoWSBPe.Text;
        VersaoGNRe               := cbVersaoWSGNRE.Text;
        VersaoeSocial            := cbVersaoWSeSocial.Text;
        VersaoReinf              := cbVersaoWSReinf.Text;
        VersaoQRCode             := cbVersaoWSQRCode.Text;
        AjustarAut               := cbxAjustarAut.Checked;
        Aguardar                 := edtAguardar.Text;
        Tentativas               := edtTentativas.Text;
        Intervalo                := edtIntervalo.Text;
        TimeZoneMode             := cbxTimeZoneMode.ItemIndex;
        TimeZoneStr              := edTimeZoneStr.Caption;
        FormaEmissaoNFe          := cbFormaEmissaoNFe.ItemIndex;
        FormaEmissaoCTe          := cbFormaEmissaoCTe.ItemIndex;
        FormaEmissaoMDFe         := cbFormaEmissaoMDFe.ItemIndex;
        FormaEmissaoBPe          := cbFormaEmissaoBPe.ItemIndex;
        FormaEmissaoGNRe         := cbFormaEmissaoGNRe.ItemIndex;
        CamposFatObrig           := ckCamposFatObrigatorio.Checked;
        TagRejeicao938           := cbTagRejeicao938.ItemIndex;
      end;

      with ESocial do
      begin
        IdEmpregador             := edtIDEmpregador.Text;
        IdTransmissor            := edtIDTransmissor.Text;
        TipoEmpregador           := cbTipoEmpregador.Text;
      end;

      with Reinf do
      begin
        IdContribuinte           := edtIDContribuinte.Text;
        IdTransmissor            := edtIDTransmissorReinf.Text;
        TipoContribuinte         := cbTipoContribuinte.Text;
      end;

      with WebService.Proxy do
      begin
        Host                     := edtProxyHost.Text;
        Porta                    := edtProxyPorta.Text;
        User                     := edtProxyUser.Text;
        Pass                     := edtProxySenha.Text;
      end;

      with WebService.NFCe do
      begin
        IdToken                  := edtIdToken.Text;
        Token                    := edtToken.Text;
        UsarIntegrador           := ckNFCeUsarIntegrador.Checked;
      end;

      with WebService.NFe do
      begin
        CNPJContador             := edtCNPJContador.Text;
      end;

      with RespTecnico do
      begin
        CSRT                     := edtCSRT.Text ;
        idCSRT                   := edtIdCSRT.Text;
      end;

      with Email do
      begin
        AssuntoNFe               := edtEmailAssuntoNFe.Text;
        MensagemNFe              := BinaryStringToString(mmEmailMsgNFe.Lines.Text);
        AssuntoCTe               := edtEmailAssuntoCTe.Text;
        MensagemCTe              := BinaryStringToString(mmEmailMsgCTe.Lines.Text);
        AssuntoMDFe              := edtEmailAssuntoMDFe.Text;
        MensagemMDFe             := BinaryStringToString(mmEmailMsgMDFe.Lines.Text);
        AssuntoNFSe              := edtEmailAssuntoNFSe.Text;
        MensagemNFSe             := BinaryStringToString(mmEmailMsgNFSe.Lines.Text);
      end;

      with Impressao.NFCe.Emissao do
      begin
        Modelo                    := rgModeloDANFeNFCE.ItemIndex;
        ModoImpressaoEvento       := rgModoImpressaoEvento.ItemIndex;
        ImprimirItem1Linha        := cbxImprimirItem1LinhaNFCe.Checked;
        ImprimirDescAcresItem     := cbxImprimirDescAcresItemNFCe.Checked;
        ImpressoraPadrao          := cbxImpressoraNFCe.Text;
        QRCodeLateral             := cbxImprimirQRCodeLateralNFCe.Checked;
        UsaCodigoEanImpressao     := cbxImprimirCodigoEANNFCe.Checked;
        ImprimeNomeFantasia       := cbxImprimirNomeFantasiaNFCe.Checked;
        ExibeTotalTributosItem    := cbxExibeTotalTributosItem.Checked;
        ImprimeTributos           := rgImprimeTributos.ItemIndex;
        LogoLateral               := cbxImprimirLogoLateralNFCe.Checked;
        ImprimeItens              := cbxImprimeItens.Checked;
      end;

      with Impressao.NFCe.Emissao.DANFCe do
      begin
        MargemInf                  := fspeNFCeMargemInf.Value;
        MargemSup                  := fspeNFCeMargemSup.Value;
        MargemDir                  := fspeNFCeMargemDir.Value;
        MargemEsq                  := fspeNFCeMargemEsq.Value;
        LarguraBobina              := fspeLarguraNFCe.Value;
      end;

      with Impressao.NFCe.Emissao.DANFCeTipoPagto do
      begin
        tipo                       := chgDescricaoPagamento.Checked[0];
        Bandeira                   := chgDescricaoPagamento.Checked[1];
        Autorizacao                := chgDescricaoPagamento.Checked[2];
      end;

      with FMonitorConfig.FonteLinha do
      begin
        Name                       := FontDialog1.Font.Name;
        Size                       := FontDialog1.Font.Size;
        Color                      := FontDialog1.Font.Color;
        Style                      := FontDialog1.Font.Style;
      end;

      with Impressao.DANFE do
      begin
        Modelo                     := rgModeloDanfe.ItemIndex;
        Site                       := edtSiteEmpresa.Text;
        Email                      := edtEmailEmpresa.Text;
        ImpDescPorc                := cbxImpDescPorc.Checked;
        MostrarPreview             := cbxMostrarPreview.Checked;
        Copias                     := edtNumCopia.Value;
        CopiasNFCe                 := edtNumCopiaNFCe.Value;
        LarguraCodigoProduto       := speLargCodProd.Value;
        EspacoEntreProdutos        := speEspEntreProd.Value;
        FonteRazao                 := speFonteRazao.Value;
        FonteEndereco              := speFonteEndereco.Value;
        FonteCampos                := speFonteCampos.Value;
        FonteAdicionais            := speFonteAdic.Value;
        AlturaCampos               := speAlturaCampos.Value;
        Margem                     := fspeMargemInf.Value;
        MargemSup                  := fspeMargemSup.Value;
        MargemDir                  := fspeMargemDir.Value;
        MargemEsq                  := fspeMargemEsq.Value;
        PathPDF                    := edtPathPDF.Text;
        DecimaisQTD                := spedtCasasDecimaisQtd.Value;
        DecimaisValor              := spedtDecimaisVUnit.Value;
        ExibeResumo                := cbxExibeResumo.Checked;
        TextoResumoCanhoto         := trim(edtMsgResumoCanhoto.Text);
        ImprimirTributosItem       := cbxImprimirTributos.Checked;
        ImprimirValLiq             := cbxImpValLiq.Checked;
        UNComercialETributavel     := cbxUnComTributavel.ItemIndex;
        PreImpresso                := cbxFormCont.Checked;
        MostrarStatus              := cbxMostraStatus.Checked;
        ExibirEAN                  := cbxExibirEAN.Checked;
        ExibirCampoFatura          := cbxExibirCampoFatura.Checked;
        ExpandirLogo               := cbxExpandirLogo.Checked;
        Fonte                      := rgTipoFonte.ItemIndex;
        LocalCanhoto               := rgLocalCanhoto.ItemIndex;
        LayoutCanhoto              := rgLayoutCanhoto.ItemIndex;
        QuebrarLinhasDetalheItens  := cbxQuebrarLinhasDetalhesItens.Checked ;
        ImprimirDetalhamentoEspecifico := cbxImpDetEspNFe.Checked;
        ImprimirDadosDocReferenciados  := cbxImpDocsReferenciados.Checked;
        ExibirBandInforAdicProduto     := rgInfAdicProduto.ItemIndex;
        LogoEmCima                     := cbxExibirLogoEmCima.Checked;
        ImprimeInscSuframa             := cbxImprimeInscSuframa.Checked;
        ExpandirDadosAdicionaisAuto    := cbxExpandirDadosAdicionaisAuto.Checked;
        ImprimeContinuacaoDadosAdicionaisPrimeiraPagina := cbxImprimeContinuacaoDadosAdicionaisPrimeiraPagina.Checked;
        ImprimeDescAcrescItemNFe   := rgImprimeDescAcrescItemNFe.ItemIndex;
        ImprimirCampoFormaPagamento:= rgInfFormaPagNFe.ItemIndex;
        ImprimeXPedNitemPed        := cbxImprimeXPedNitemPed.Checked;
      end;

      with Impressao.DACTE do
      begin
        TamanhoPapel                   := rgTamanhoPapelDacte.ItemIndex;
      end;

      with Impressao.DAMFE do
      begin
        ExibirMunicipioDescarregamento := ckbExibirMunicipioDescarregamento.Checked;
      end;

      with Diretorios do
      begin
        Salvar                         := cbxSalvarArqs.Checked;
        PastaMensal                    := cbxPastaMensal.Checked;
        AddLiteral                     := cbxAdicionaLiteral.Checked;
        EmissaoPathNFe                 := cbxEmissaoPathNFe.Checked;
        SalvarCCeCanPathEvento         := cbxSalvaPathEvento.Checked;
        SepararPorCNPJ                 := cbxSepararPorCNPJ.Checked;
        SepararPorModelo               := cbxSepararporModelo.Checked;
        SalvarApenasNFesAutorizadas    := cbxSalvarNFesProcessadas.Checked;
        AtualizarXMLCancelado          := cbxAtualizarXMLCancelado.Checked;
        NormatizarMunicipios           := cbxNormatizarMunicipios.Checked;
        UsarSeparadorPathPDF           := cbxUsarSeparadorPathPDF.Checked;
        SepararPorNome                 := cbxSepararPorNome.Checked;
        PathNFe                        := edtPathNFe.Text;
        PathInu                        := edtPathInu.Text;
        PathDPEC                       := edtPathDPEC.Text;
        PathEvento                     := edtPathEvento.Text;
        PathArqTXT                     := edtPathArqTXT.Text;
        PathDownload                   := edtPathDownload.Text;
        PathSchemasDFe                 := edtPathSchemasDFe.Text;

      end;
    end;

    {Parametro SAT}
    with FMonitorConfig.SAT do
    begin
      if cbSATMarca.ItemIndex <= 0 then
        Marca := ''
      else
        Marca := cbSATMarca.Text;

      Modelo := cbxModeloSAT.ItemIndex;
      NomeDLL := edNomeDLL.Text;
      PaginaDeCodigo := sePagCod.Value;

      ArqLog                           := edSATLog.Text;
      CodigoAtivacao                   := edtCodigoAtivacao.Text;
      CodigoUF                         := edtCodUF.Text;
      NumeroCaixa                      := seNumeroCaixa.Value;
      Ambiente                         := cbxAmbiente.ItemIndex;
      versaoDadosEnt                   := sfeVersaoEnt.Value;
      FormatarXML                      := cbxFormatXML.Checked;
      PathCFe                          := edSATPathArqs.Text;
      SalvarCFe                        := cbxSATSalvarCFe.Checked;
      SalvarCFeCanc                    := cbxSATSalvarCFeCanc.Checked;
      SalvarEnvio                      := cbxSATSalvarEnvio.Checked;
      SepararPorCNPJ                   := cbxSATSepararPorCNPJ.Checked;
      SepararPorMES                    := cbxSATSepararPorMES.Checked;
      SepararPorANO                    := cbxSATSepararPorANO.Checked;
      SepararPorDIA                    := cbxSATSepararPorDIA.Checked;
      SepararPorModelo                 := cbxSATSepararPorModelo.Checked;
      ValidarNumeroSessaoResposta      := cbxValidarNumeroSessaoResposta.Checked;
      PathCFeCanc                      := edSATPathArqsCanc.Text;
      PathCFeEnvio                     := edSATPathArqsEnvio.Text;
      PrefixoArqCFe                    := edSATPrefixoCFe.Text;
      PrefixoArqCFeCanc                := edSATPrefixoCFeCanc.Text;

      with SATImpressao.SATExtrato do
      begin
        MostrarStatus                  := cbxMostrarStatusSAT.Checked;
        ParamsString                   := ACBrSATExtratoESCPOS1.PosPrinter.Device.ParamsString;
        ImprimeDescAcrescItem          := cbxImprimirDescAcresItemSAT.Checked;
        ImprimeEmUmaLinha              := cbxImprimirItem1LinhaSAT.Checked;
        ImprimeChaveEmUmaLinha         := rdgImprimeChave1LinhaSAT.ItemIndex;
        UsaCodigoEanImpressao          := cbxImprimirCodEANitemSAT.Checked;
        ImprimeQRCodeLateral           := cbxQRCodeLateral.Checked;
        ImprimeLogoLateral             := cbxLogoLateral.Checked;
        ExtratoDecimaisQTD             := spedtSATCasasDecimaisQtd.Value;
        ExtratoDecimaisValor           := spedtSATDecimaisVUnit.Value;
        ExtratoMaskQTD                 := edtSATCasasMaskQtd.Text;
        ExtratoMaskValor               := edtSATMaskVUnit.Text;
        FormatoDecimal                 := cbFormatoDecimais.ItemIndex;

      end;

      with SATImpressao.SATEmit do
      begin
        CNPJ                           := edtEmitCNPJ.Text;
        IE                             := edtEmitIE.Text;
        IM                             := edtEmitIM.Text;
        RegTributario                  := cbxRegTributario.ItemIndex;
        RegTribISSQN                   := cbxRegTribISSQN.ItemIndex;
        IndRatISSQN                    := cbxIndRatISSQN.ItemIndex;
      end;

      with SATImpressao.SATFortes do
      begin
        UsarFortes                     := cbUsarFortes.Checked;
        Largura                        := seLargura.Value;
        MargemTopo                     := seMargemTopo.Value;
        MargemFundo                    := seMargemFundo.Value;
        MargemEsquerda                 := seMargemEsquerda.Value;
        MargemDireita                  := seMargemDireita.Value;
        Preview                        := cbPreview.Checked;
      end;

      with SATImpressao.SATPrinter do
      begin
        Name                           := lImpressora.Caption;
      end;

      with SATRede do
      begin
        tipoInter                      := rgRedeTipoInter.ItemIndex;
        tipoLan                        := rgRedeTipoLan.ItemIndex;
        SSID                           := edRedeSSID.Text;
        seg                            := cbxRedeSeg.ItemIndex;
        codigo                         := edRedeCodigo.Text;
        lanIP                          := edRedeIP.Text;
        lanMask                        := edRedeMask.Text;
        lanGW                          := edRedeGW.Text;
        lanDNS1                        := edRedeDNS1.Text;
        lanDNS2                        := edRedeDNS2.Text;
        usuario                        := edRedeUsuario.Text;
        senha                          := edRedeSenha.Text;
        proxy                          := cbxRedeProxy.ItemIndex;
        proxy_ip                       := edRedeProxyIP.Text;
        proxy_porta                    := edRedeProxyPorta.Value;
        proxy_user                     := edRedeProxyUser.Text;
        proxy_senha                    := edRedeProxySenha.Text;
      end;

      with SATSWH do
      begin
        CNPJ                           := edtSwHCNPJ.Text;
        Assinatura                     := edtSwHAssinatura.Text;
      end;

      with SATEmail do
      begin
         AssuntoSAT                    := edtEmailAssuntoSAT.Text;
         MensagemSAT                   := BinaryStringToString( mmEmailMsgSAT.Lines.Text );
      end;

    end;

    {Parametro Integrador}
    with FMonitorConfig.IntegradorFiscal do
    begin
      Input                            := edMFEInput.Text;
      Output                           := edMFEOutput.Text;
      Timeout                          := seMFETimeout.Value;
    end;

    {Parâmetros PosPrinter}
    with FMonitorConfig.PosPrinter do
    begin
      Modelo                           := cbxModelo.ItemIndex;
      Porta                            := cbxPorta.Text;
      Colunas                          := seColunas.Value;
      EspacoEntreLinhas                := seEspacosLinhas.Value;
      LinhasBuffer                     := seBuffer.Value;
      LinhasPular                      := seLinhasPular.Value;
      PaginaDeCodigo                   := cbxPagCodigo.ItemIndex;
      ControlePorta                    := cbControlePorta.Checked;
      CortarPapel                      := cbCortarPapel.Checked;
      TraduzirTags                     := cbTraduzirTags.Checked;
      IgnorarTags                      := cbIgnorarTags.Checked;
      ArqLog                           := edPosPrinterLog.Text;
      SerialParams                     := ACBrPosPrinter1.Device.ParamsString;

      with CodigoBarras do
      begin
        Largura                       := seCodBarrasLargura.Value;
        Altura                        := seCodBarrasAltura.Value;
        HRI                           := cbHRI.Checked;
      end;

      with QRCode do
      begin
        Tipo                          := seQRCodeTipo.Value;
        LarguraModulo                 := seQRCodeLargMod.Value;
        ErrorLevel                    := seQRCodeErrorLevel.Value;
      end;

      with Logo do
      begin
        Imprimir                      := cbEscPosImprimirLogo.Checked;
        KC1                           := seLogoKC1.Value;
        KC2                           := seLogoKC2.Value;
        FatorX                        := seLogoFatorX.Value;
        FatorY                        := seLogoFatorY.Value;
      end;

      with Gaveta do
      begin
        TempoON                       := seGavetaTempoON.Value;
        TempoOFF                      := seGavetaTempoOFF.Value;
        SinalInvertido                := cbGavetaSinalInvertido.Checked;
      end;

    end;

    {Parâmetros Boleto}
    SalvarConfBoletos;

  finally
    FMonitorConfig.SalvarArquivo;
  end;

  AddLinesLog('Configuração geral gravada com sucesso');

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
         Validar;
       except
         btnBoletoClick(btnBoleto);
         btnBoletoBeneficiarioClick(btnBoletoBeneficiario);
         edtBOLCNPJ.SetFocus;
         raise;
       end;
          edtBOLCNPJ.Text := Formatar;
     end;
   end;

   {Parametros do Boleto}
   with FMonitorConfig.BOLETO do
   begin
     Nome               := edtBOLRazaoSocial.Text;
     CNPJCPF            := ifthen(TrimedCNPJ = '', '', edtBOLCNPJ.Text);
     Logradouro         := edtBOLLogradouro.Text;
     Numero             := edtBOLNumero.Text;
     Bairro             := edtBOLBairro.Text;
     CodCidade          := IfThen( StrToIntDef(edtBOLCodCidade.Caption,0) > 0, StrToIntDef(edtBOLCodCidade.Caption, 0) );
     Cidade             := cbxEmitCidade.Text ;
     CEP                := ifthen(TrimedCEP = '', '', edtBOLCEP.Text);
     Complemento        := edtBOLComplemento.Text;
     UF                 := cbxBOLUF.Text;

     with Conta do
     begin
       Banco                    := max(cbxBOLBanco.ItemIndex, 0);
       Conta                    := edtBOLConta.Text;
       DigitoConta              := edtBOLDigitoConta.Text;
       Agencia                  := edtBOLAgencia.Text;
       DigitoAgencia            := edtBOLDigitoAgencia.Text;
       DigitoAgenciaConta       := edtBOLDigitoAgConta.Text;
       CodCedente               := edtCodCliente.Text;
       LocalPagamento           := edtBOLLocalPagamento.Text;
       RespEmis                 := cbxBOLEmissao.ItemIndex;
       Pessoa                   := cbxBOLF_J.ItemIndex;
       Modalidade               := edtModalidade.Text;
       Convenio                 := edtConvenio.Text;
       CodigoOperacao           := edtOperacaoBeneficiario.Text;
     end;

     with PIX do
     begin
       TipoChavePix             := cbxBOLTipoChavePix.ItemIndex;
       ChavePix                 := edtBOLChavePix.Text;
     end;

     with Layout do
     begin
       DirLogos                 := PathWithoutDelim(deBOLDirLogo.Text);
       Copias                   := spBOLCopias.Value;
       Preview                  := ckgBOLMostrar.Checked[0];
       Progresso                := ckgBOLMostrar.Checked[1];
       Setup                    := ckgBOLMostrar.Checked[2];
       Layout                   := cbxBOLLayout.ItemIndex;
       Filtro                   := cbxBOLFiltro.ItemIndex;
       DirArquivoBoleto         := PathWithoutDelim(deBOLDirArquivo.Text);
       NomeArquivoBoleto        := trim(edNomeArquivo.Text);
       Impressora               := cbxBOLImpressora.Text;
     end;

     with RemessaRetorno do
     begin
       DirArquivoRemessa        := PathWithoutDelim(deBolDirRemessa.Text);
       DirArquivoRetorno        := PathWithoutDelim(deBolDirRetorno.Text);
       CNAB                     := StrToInt(IfThen(cbxCNAB.ItemIndex = 0, '1', '0'));
       LerCedenteRetorno        := chkLerBeneficiarioRetorno.Checked;
       CodTransmissao           := edtCodTransmissao.Text;
       RemoveAcentos            := chkRemoveAcentos.Checked;
       PrefixArqRemessa         := edtPrefixRemessa.Text;
       VersaoArquivo            := edtVersaoArquivo.Text;
       VersaoLote               := edtVersaoLote.Text;
     end;

     with Email do
     begin
       EmailAssuntoBoleto       := edtBOLEmailAssunto.Text;
       EmailMensagemBoleto      := BinaryStringToString(edtBOLEmailMensagem.Lines.Text);
       EmailFormatoHTML         := cbxBOLEmailMensagemHTML.Checked;
     end;

     with Relatorio do
     begin
       MostraPreviewRelRetorno := chkBOLRelMostraPreview.Checked;
       LogoEmpresa             := edtBOLLogoEmpresa.Text;
     end;

     with WS.CedenteWS do
     begin
       ClientID := edtClientID.Text;
       ClientSecret := edtClientSecret.Text;
       KeyUser := edtKeyUser.Text;
       Scope := edtScope.Text;
       IndicadorPix := ChkPix.Checked;
     end;

     with WS.Config do
     begin
       LogRegistro := ChkLogBoletoWeb.Checked;
       PathGravarRegistro := PathWithoutDelim(edtPathLogBoleto.Text);
     end;

     with WS.Config.SSL do
     begin
       Ambiente := rgTipoAmbBoleto.ItemIndex;
       Operacao := cbOperacaoBoleto.ItemIndex;
       VersaoDF := edtVersaoBoleto.Text;
       HttpLib := cbHttpLibBoleto.ItemIndex;
       TimeOut := edtTimeoutWebServicesBoleto.Value;
       SSLType := cbSSLTypeBoleto.ItemIndex;
       ArquivoKEY:=edtBolArquivoKey.Text;
       ArquivoCRT:=edtBolArquivoCRT.Text;
     end;

   end;

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

  ConfigPainelMenu(False);
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
  begin
    MessageDlg('Senha inválida!!!', mtWarning, [mbOK], 0);
    raise Exception.Create('Senha inválida');
  end;

  fsCNPJSWOK := False;

  ConfigPainelMenu(True);
  PanelScroll.Visible := false;
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
       (UpperCase(Copy(Objeto, 1, 7)) = 'ESOCIAL') or
       (UpperCase(Copy(Objeto, 1, 5)) = 'REINF') or
       (UpperCase(Copy(Objeto, 1, 3)) = 'BPE') or
       (UpperCase(Copy(Objeto, 1, 3)) = 'CTE') or
       (UpperCase(Copy(Objeto, 1, 4)) = 'CNPJ') or
       (UpperCase(Copy(Objeto, 1, 3)) = 'CPF') then
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

        //Verifica ajustes p/ versao demo
        {$IFDEF Demo}
        AtualizaAplicacaoDemo;
        {$ENDIF}

        //Validar Erros de configuração dos Componentes
        VerificarErrosConfiguracaoComponentes(fsCmd);

        //Log Comando
        AddLinesLog(Linha);

        if fsCmd.Objeto = 'ACBR' then
          FDoACBr.Executar(fsCmd)
        else if fsCmd.Objeto = 'ECF' then
          FDoECF.Executar(fsCmd)
        else if fsCmd.Objeto = 'GAV' then
          FDoGAV.Executar(fsCmd)
        else if fsCmd.Objeto = 'CHQ' then
          FDoCHQ.Executar(fsCmd)
        else if fsCmd.Objeto = 'DIS' then
          FDoDIS.Executar(fsCmd)
        else if fsCmd.Objeto = 'LCB' then
          FDoLCB.Executar(fsCmd)
        else if fsCmd.Objeto = 'BAL' then
          FDoBAL.Executar(fsCmd)
        else if fsCmd.Objeto = 'ETQ' then
          FDoETQ.Executar(fsCmd)
        else if fsCmd.Objeto = 'BOLETO' then
          FDoBoleto.Executar(fsCmd)
        else if fsCmd.Objeto = 'CEP' then
          FDoCEP.Executar(fsCmd)
        else if fsCmd.Objeto = 'IBGE' then
          FDoIBGE.Executar(fsCmd)
        else if fsCmd.Objeto = 'EMAIL' then
          FDoEmail.Executar(fsCmd)
        else if fsCmd.Objeto = 'SEDEX' then
          FDoSedex.Executar(fsCmd)
        else if fsCmd.Objeto = 'CNPJ' then
          FDoCNPJ.Executar(fsCmd)
        else if fsCmd.Objeto = 'CPF' then
          FDoCPF.Executar(fsCmd)
        else if fsCmd.Objeto = 'NCM' then
          FDoNcm.Executar(fsCmd)
        else if fsCmd.Objeto = 'NFE' then
          FDoNFe.Executar(fsCmd)
        else if fsCmd.Objeto = 'CTE' then
          FDoCTe.Executar(fsCmd)
        else if fsCmd.Objeto = 'MDFE' then
          FDoMDFe.Executar(fsCmd)
        else if fsCmd.Objeto = 'ESOCIAL' then
          FDoeSocial.Executar(fsCmd)
        else if fsCmd.Objeto = 'REINF' then
          FDoReinf.Executar(fsCmd)
        else if fsCmd.Objeto = 'GNRE' then
          FDoGNRe.Executar(fsCmd)
        else if fsCmd.Objeto = 'SAT' then
          FDoSAT.Executar(fsCmd)
        else if fsCmd.Objeto = 'BPE' then
          FDoBPe.Executar(fsCmd)
        else if fsCmd.Objeto = 'ESCPOS' then
          FDoPosPrinter.Executar(fsCmd)
        else if fsCmd.Objeto = 'GTIN' then
          FDoGTIN.Executar(fsCmd)
        else if fsCmd.Objeto = 'NFSE' then
          FDoNFSe.Executar(fsCmd);

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
procedure TFrmACBrMonitor.sbLogClick(Sender: TObject);
begin

  OpenDialog1.Title := 'Selecione o arquivo de log';
  OpenDialog1.DefaultExt := '*.txt';
  OpenDialog1.Filter :=
    'Arquivos LOG (*.txt)|*.txt';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edLogArq.Text := OpenDialog1.FileName;
  end;

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
  NomeArqEnt, NomeArqSai: string;
  SL: TStringList;
begin
  Timer1.Enabled := False;
  ArqEntTXT := '';

  if Inicio then
  begin
    Inicializar;
    if FileExists(ACBrMonitorINI) then
       Application.Minimize;

    exit;
  end;

  try
    if fsMonitorarPasta then
    begin
      NomeArqEnt := PathWithDelim(ExtractFileDir(ArqEntOrig)) + '*.*';
      SL:= TStringList.Create;
      try
        FindFiles(NomeArqEnt, SL, True, fstDateTime, fsdAscending);
        if (SL.Count > 0) then
        begin
          ArqEntTXT := SL[0];
          { Arquivo de Requisicao }
          NomeArqEnt := StringReplace(ExtractFileName(ArqEntTXT),
                     ExtractFileExt(ArqEntTXT), '', [rfReplaceAll]);
          NomeArqEnt:= NomeArqEnt + '-resp' + ExtractFileExt(ArqEntTXT);
          ArqSaiTXT := PathWithDelim(ExtractFilePath(ArqSaiOrig)) + NomeArqEnt;
          ArqSaiTMP := ChangeFileExt(ArqSaiTXT, '.tmp');
        end;
      finally
        SL.Free;
      end;
    end
    else
    begin
      NomeArqEnt := PathWithDelim(ExtractFileDir(ArqEntOrig)) +
                 StringReplace(ExtractFileName(ArqEntOrig), ExtractFileExt(ArqEntOrig),
                 '', [rfReplaceAll]) + '*' + ExtractFileExt(ArqEntOrig);
      SL:= TStringList.Create;
      try
        FindFiles(NomeArqEnt, SL, True, fstDateTime, fsdAscending);
        if (SL.Count > 0) then
        begin
          NomeArqEnt := StringReplace(ExtractFileName(ArqEntOrig),
                     ExtractFileExt(ArqEntOrig), '', [rfReplaceAll]);
          NomeArqSai := StringReplace(ExtractFileName(ArqSaiOrig),
                     ExtractFileExt(ArqSaiOrig), '', [rfReplaceAll]);
          ArqEntTXT := SL[0];
          { Arquivo de Requisicao }
          ArqSaiTXT := PathWithDelim(ExtractFilePath(ArqSaiOrig)) + StringReplace(
                    ExtractFileName(LowerCase(ArqEntTXT)), LowerCase(NomeArqEnt), LowerCase(
                    NomeArqSai), [rfReplaceAll]);
          ArqSaiTMP := ChangeFileExt(ArqSaiTXT, '.tmp');
        end;
      finally
        SL.Free;
      end;
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
  pgECFParams.ActivePageIndex := 0;
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

  FMenuTreeView.SincronizaTreeView( (Sender as TPageControl).ActivePage.Tag );
end;

procedure TFrmACBrMonitor.pgCadastroChange(Sender: TObject);
begin
  FMenuTreeView.SincronizaTreeView( (Sender as TPageControl).ActivePage.Tag );
end;

procedure TFrmACBrMonitor.pgConfigChange(Sender: TObject);
begin
  HelptabSheet;
  FMenuTreeView.SincronizaTreeView( (Sender as TPageControl).ActivePage.Tag );
end;

procedure TFrmACBrMonitor.pgConRFDChange(Sender: TObject);
begin
  FMenuTreeView.SincronizaTreeView( (Sender as TPageControl).ActivePage.Tag );
end;

procedure TFrmACBrMonitor.pgDFeChange(Sender: TObject);
begin
  FMenuTreeView.SincronizaTreeView( (Sender as TPageControl).ActivePage.Tag );
end;

procedure TFrmACBrMonitor.pgECFParamsChange(Sender: TObject);
begin
  FMenuTreeView.SincronizaTreeView( (Sender as TPageControl).ActivePage.Tag );
end;

procedure TFrmACBrMonitor.pgSATChange(Sender: TObject);
begin
  FMenuTreeView.SincronizaTreeView( (Sender as TPageControl).ActivePage.Tag );
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

procedure TFrmACBrMonitor.SbArqLog3Click(Sender: TObject);
begin
  PathClick(edSATPathArqsCanc);
end;

procedure TFrmACBrMonitor.SbArqLog4Click(Sender: TObject);
begin
  PathClick(edSATPathArqsEnvio);
end;

procedure TFrmACBrMonitor.SbArqLogClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(Application.ExeName) + edSATLog.Text);
end;

procedure TFrmACBrMonitor.sbArquivoCertClick(Sender: TObject);
var
  AFile: String;
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ApplicationPath;

  if OpenDialog1.Execute then
  begin
    AFile := OpenDialog1.FileName;
    if (pos(ApplicationPath, AFile) > 0) then
      AFile := ExtractFileName(AFile);

    edtArquivoPFX.Text := AFile;
  end;
end;

procedure TFrmACBrMonitor.sbArquivoCRTClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo crt';
  OpenDialog1.DefaultExt := '*.crt';
  OpenDialog1.Filter :=
    'Arquivos CRT (*.crt)|*.crt|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtBolArquivoCRT.Text := OpenDialog1.FileName;
  end;

end;

procedure TFrmACBrMonitor.sbArquivoKEYClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo Key';
  OpenDialog1.DefaultExt := '*.key';
  OpenDialog1.Filter :=
    'Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtBolArquivoKey.Text := OpenDialog1.FileName;
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

procedure TFrmACBrMonitor.sbArquivoWebServiceseSocialClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServiceseSocial.Text := OpenDialog1.FileName;
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

procedure TFrmACBrMonitor.sbArquivoWebServicesBPeClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServicesBPe.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbArquivoWebServicesNFSeClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServicesNFSe.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbArquivoWebServicesReinfClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtArquivoWebServicesReinf.Text := OpenDialog1.FileName;
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

procedure TFrmACBrMonitor.sbConsultaCEPClick(Sender: TObject);
var
  EndAchado: TACBrCEPEndereco;
  cUF: Integer;
begin
  try
    ACBrCEP1.BuscarPorCEP(OnlyNumber(edtBOLCEP.Text));
    if (ACBrCEP1.Enderecos.Count > 0) then
    begin
      EndAchado := ACBrCEP1.Enderecos[0];
      edtBOLLogradouro.Text := Trim(EndAchado.Tipo_Logradouro + ' ' + EndAchado.Logradouro);
      edtBOLBairro.Text := EndAchado.Bairro;
      edtBOLCEP.Text := ACBrValidador.FormatarCEP(EndAchado.CEP);
      edtBOLComplemento.Text := EndAchado.Complemento;
      cUF := UFtoCUF(EndAchado.UF);
      CarregarListaDeCidades(cUF);
      cbxBOLUF.ItemIndex := cbxBOLUF.Items.IndexOf(EndAchado.UF);
      edtBOLCodCidade.Caption:= trim(EndAchado.IBGE_Municipio);
      cbxEmitCidade.ItemIndex := cbxEmitCidade.Items.IndexOf(EndAchado.Municipio);
      cbxEmitCidadeChange(nil);
      edtBOLNumero.SetFocus;
    end;
  except
    MessageDlg('Erro ao executar Consulta do CEP', mtError, [mbOK], 0);
  end;
end;

procedure TFrmACBrMonitor.sbConsultaCNPJBoletoClick(Sender: TObject);
var
  frConsultaCNPJ: TfrConsultaCNPJ;
  MR: TModalResult;
  cUF: Integer;
begin
  frConsultaCNPJ := TfrConsultaCNPJ.Create(Self);
  try
    MR := frConsultaCNPJ.ShowModal;

    if (MR = mrOK) then
    begin
      try
        if ACBrConsultaCNPJ1.Consulta(edtBOLCNPJ.Text, frConsultaCNPJ.edtCaptcha.Text) then
        begin
          //EditTipo.Text := ACBrConsultaCNPJ1.EmpresaTipo;
          edtBOLRazaoSocial.Text := ACBrConsultaCNPJ1.RazaoSocial;
          //edtEmitFantasia.Text := ACBrConsultaCNPJ1.Fantasia;
          edtBOLLogradouro.Text := ACBrConsultaCNPJ1.Endereco;
          edtBOLNumero.Text := ACBrConsultaCNPJ1.Numero;
          edtBOLComplemento.Text := ACBrConsultaCNPJ1.Complemento;
          edtBOLCEP.Text := ACBrConsultaCNPJ1.CEP;
          edtBOLBairro.Text := ACBrConsultaCNPJ1.Bairro;
          cbxBOLUF.Text := ACBrConsultaCNPJ1.UF;

          cUF := UFtoCUF(ACBrConsultaCNPJ1.UF);
          CarregarListaDeCidades(cUF);
          cbxBOLUF.ItemIndex := cbxBOLUF.Items.IndexOf(ACBrConsultaCNPJ1.UF);
          edtBOLCodCidade.Caption := Trim(ACBrConsultaCNPJ1.IBGE_Municipio);
          cbxEmitCidade.ItemIndex := fcMunList.IndexOf(edtBOLCodCidade.Caption);
          cbxEmitCidadeChange(nil);
        end;
      except
        MessageDlg('Erro ao Consultar CNPJ'+sLineBreak+'Verifique o Captcha', mtError, [mbOK], 0);
      end;
    end;
  finally
     frConsultaCNPJ.Free;
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

procedure TFrmACBrMonitor.sbLogoMarcaPrefeituraClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.png';
  OpenDialog1.Filter :=
    'Arquivos PNG (*.png)|*.png|Arquivos JPG (*.jpg)|*.jpg|Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtLogoMarcaPrefeitura.Text := OpenDialog1.FileName;
  end;
end;

procedure TFrmACBrMonitor.sbNomeDLLClick(Sender: TObject);
begin
  {$IFDEF LINUX}
    OpenDialog1.Filter := 'Arquivo LIB|*.so';
  {$ELSE}
    OpenDialog1.Filter := 'Arquivo DLL|*.dll';
  {$ENDIF}
  OpenDialog1.InitialDir := ExtractFilePath(edNomeDLL.Text);
  OpenDialog1.FileName := edNomeDLL.Text;
  if OpenDialog1.Execute then
    edNomeDLL.Text := OpenDialog1.FileName ;
end;

procedure TFrmACBrMonitor.sbNumeroSerieCertClick(Sender: TObject);
begin
  edtNumeroSerie.Text := ACBrNFe1.SSL.SelecionarCertificado;
end;

procedure TFrmACBrMonitor.sbPathArqTXTClick(Sender: TObject);
begin
  PathClick(edtPathArqTXT);
end;

procedure TFrmACBrMonitor.sbPathDownloadClick(Sender: TObject);
begin
  PathClick(edtPathDownload);
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

procedure TFrmACBrMonitor.sbSchemaDFeClick(Sender: TObject);
begin
  PathClick(edtPathSchemasDFe);
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

procedure TFrmACBrMonitor.sbtnNumSerieClick(Sender: TObject);
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

procedure TFrmACBrMonitor.sbVerSenhaCertificadoClick(Sender: TObject);
begin
  if sbVerSenhaCertificado.Down then
    edtSenha.EchoMode := emNormal
  else
    edtSenha.EchoMode := emPassword;
end;

procedure TFrmACBrMonitor.sbVerSenhaEmailClick(Sender: TObject);
begin
  if sbVerSenhaEmail.Down then
    edEmailSenha.EchoMode := emNormal
  else
    edEmailSenha.EchoMode := emPassword;
end;

procedure TFrmACBrMonitor.sbVerSenhaProxyClick(Sender: TObject);
begin
  if sbVerSenhaProxy.Down then
    edtProxySenha.EchoMode := emNormal
  else
    edtProxySenha.EchoMode := emPassword;
end;

procedure TFrmACBrMonitor.sbVerSenhaProxySATClick(Sender: TObject);
begin
  if sbVerSenhaProxySAT.Down then
    edRedeProxySenha.EchoMode := emNormal
  else
    edRedeProxySenha.EchoMode := emPassword;
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

procedure TFrmACBrMonitor.spbExpandClick(Sender: TObject);
begin
  TreeViewMenu.FullExpand;
end;

procedure TFrmACBrMonitor.spbCollapseClick(Sender: TObject);
begin
  TreeViewMenu.FullCollapse;
end;

procedure TFrmACBrMonitor.TcpServerConecta(const TCPBlockSocket: TTCPBlockSocket;
  var Enviar: ansistring);
var
  Resp: string;
begin

  Conexao := TCPBlockSocket;
  mCmd.Lines.Clear;
  fsProcessar.Clear;
  Resp := 'ACBrMonitorPLUS Ver. ' + sVersaoACBr + sLineBreak + 'Conectado em: ' +
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

procedure TFrmACBrMonitor.TreeViewMenuClick(Sender: TObject);
begin
  if Assigned(TreeViewMenu.Selected) then
    FMenuTreeView.AbrirTelaTreeView( TreeViewMenu.Selected.Level                          //Indice do Nível TreeView
                                    , TreeViewMenu.Selected.Index                         //Indice do Menu Selecionado
                                    , TreeViewMenu.Selected.Parent.Index);                //Indice do Nó Pai

end;

procedure TFrmACBrMonitor.tsACBrBoletoShow(Sender: TObject);
begin
  pgBoleto.ActivePageIndex := 0;
end;

procedure TFrmACBrMonitor.tsCadastroShow(Sender: TObject);
begin
  pgCadastro.ActivePageIndex := 0;
end;

procedure TFrmACBrMonitor.tsDFeShow(Sender: TObject);
begin
  pgDFe.ActivePageIndex := 0;
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
    if (cbLCBPorta.Text <> 'Sem Leitor') then
      ACBrLCB1.Porta := cbLCBPorta.Text
    else
      ACBrLCB1.Porta := '';
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

procedure TFrmACBrMonitor.AtualizarHomologacaoDFe(Config: TConfiguracoes);
begin
  Config.WebServices.Ambiente:= taHomologacao;

end;

procedure TFrmACBrMonitor.AtualizarImpressaoHomologacaoDFe(Report: TACBrDFeReport);
begin
  Report.Sistema:= C_PROJETO_ACBR;
  Report.Site:= C_PROJETOACBR_COM_BR;

end;

procedure TFrmACBrMonitor.AtualizaAplicacaoDemo;
begin
  AtualizarHomologacaoDFe(ACBrNFe1.Configuracoes);
  AtualizarHomologacaoDFe(ACBrCTe1.Configuracoes);
  AtualizarHomologacaoDFe(ACBrMDFe1.Configuracoes);
  AtualizarHomologacaoDFe(ACBrGNRE1.Configuracoes);
  AtualizarHomologacaoDFe(ACBrBPe1.Configuracoes);
  AtualizarHomologacaoDFe(ACBrBlocoX1.Configuracoes);
  AtualizarHomologacaoDFe(ACBreSocial1.Configuracoes);
  AtualizarHomologacaoDFe(ACBrReinf1.Configuracoes);
  ACBrSAT1.Config.ide_tpAmb:= taHomologacao;

  AtualizarImpressaoHomologacaoDFe(ACBrNFe1.DANFE);
  AtualizarImpressaoHomologacaoDFe(ACBrCTe1.DACTE);
  AtualizarImpressaoHomologacaoDFe(ACBrMDFe1.DAMDFE);
  AtualizarImpressaoHomologacaoDFe(ACBrBPe1.DABPE);
  AtualizarImpressaoHomologacaoDFe(ACBrSAT1.Extrato);

end;

procedure TFrmACBrMonitor.ValidarComunicacao;
begin
  imgErrComunicacao.Visible :=  ( not(rbTCP.Checked)) and ( not(rbTXT.Checked)) ;
  VerificarErrosComunicacao;
end;

procedure TFrmACBrMonitor.ValidarConfigCertificado;
var
 PathPfx, UrlPfx, ArqPfx, NumSerie, Senha: String;
 Ok, Res: Boolean;
begin
  Res:= imgErrCertificado.Visible;

  UrlPfx := edtURLPFX.Text;
  ArqPfx := edtArquivoPFX.Text;
  NumSerie := edtNumeroSerie.Text;
  Senha := edtSenha.Text;
  Ok := (cbSSLLib.ItemIndex > 0);

  if (NumSerie = '') then
  begin
    Ok := Ok and (Senha <> '');
    if Ok then
    begin
      if (UrlPfx <> '') then
        Ok := (ArqPfx <> '')   // Precisa do PFX, para Cache Local
      else
      begin
        if (ExtractFilePath(ArqPfx) = '') then
          PathPfx := ApplicationPath + ArqPfx
        else
          PathPfx := ArqPfx;

        Ok := (ArqPfx <> '') and FileExists(PathPfx);
      end;
    end;
  end;

  imgErrCertificado.Visible := not Ok;
  btCertInfo.Enabled := not imgErrCertificado.Visible;

  if (Res <> imgErrCertificado.Visible) or (imgErrCertificado.Visible) then
    FMenuTreeView.AtualizaItemTela(tsCertificadoDFe.Tag,
                                    IfThen(imgErrCertificado.Visible
                                          , C_INDEX_IMG_ERRO
                                          , C_INDEX_IMG_FOLDER));


  ValidarConfigWebService;

end;


procedure TFrmACBrMonitor.ValidarConfigWebService;
var
  Ok, Res: Boolean;
  PathSchemas: String;
begin
  Res:= imgErrWebServer.Visible;
  imgErrWebService.Visible := (cbUF.ItemIndex < 0);

  PathSchemas := edtPathSchemasDFe.Text;
  Ok := (PathSchemas <> '');
  if Ok then
    Ok := FileExists(PathWithDelim(PathSchemas) + PathWithDelim( 'NFe' ) + 'nfe_v4.00.xsd');

  imgErrPathSchemas.Visible := not Ok;
  btStatusServico.Enabled := not ( imgErrCertificado.Visible or
                                   imgErrPathSchemas.Visible or
                                   imgErrWebService.Visible);
  imgErrWebServer.Visible := not ( btStatusServico.Enabled );

  if (Res <> imgErrWebServer.Visible) or (imgErrWebServer.Visible) then
    FMenuTreeView.AtualizaItemTela(tsWebServiceDFe.Tag,
                                    IfThen(imgErrWebServer.Visible
                                          , C_INDEX_IMG_ERRO
                                          , C_INDEX_IMG_FOLDER));


end;

procedure TFrmACBrMonitor.LigarAlertasdeErrosDeConfiguracao;
begin

  AtualizaSSLLibsCombo;
  edtArquivoPFXChange(Nil);
  edtNumeroSerieChange(Nil);
  edtIdTokenChange(Nil);
  edtTokenChange(Nil);
  edtCNPJContadorChange(Nil);
  edtBOLCNPJChange(Nil);
  edtBOLCEPChange(Nil);
  edtBOLRazaoSocialChange(Nil);
  cbxEmitCidadeChange(Nil);

  ValidarComunicacao;
  ValidarConfigWebService;
  ValidarConfigCertificado;
  ValidarConfigSAT;
  ValidarConfigMail;

  FMenuTreeView.CarregarMenuTreeView;

end;

procedure TFrmACBrMonitor.ValidarConfigSAT;
  procedure LigarAlertaErros;
    begin
      imgErrSATCodAtivacao.Visible := not( Length( edtCodigoAtivacao.Text) > 5);
      imgErrSATEmitente.Visible := not( Length( edtEmitCNPJ.Text) > 5);
      imgErrSATCNPJSH.Visible := not( Length( edtSwHCNPJ.Text) > 5);
      imgErrSATAssign.Visible := not( Length( edtSwHAssinatura.Text) > 20);
      imgErrSATPathVendas.Visible := not( (edSATPathArqs.Text <> '') and DirectoryExists(edSATPathArqs.Text));
      imgErrSATPathCancelamento.Visible := not( (edSATPathArqsCanc.Text <> '') and DirectoryExists(edSATPathArqsCanc.Text));
      imgErrSATPathEnvio.Visible := not( (edSATPathArqsEnvio.Text <> '') and DirectoryExists(edSATPathArqsEnvio.Text));
      imgErrSATLibModelo.Visible:= not(cbxModeloSAT.ItemIndex > 0);
    end;
var
  Ok, Res: Boolean;
begin

  Res:= imgErrSATAtivar.Visible;

  //Botão Inicializar
  //Botão Consultar Status
  Ok := (cbxModeloSAT.ItemIndex > 0);
  imgErrSATLibModelo.Visible:= Ok;
  bInicializar.Enabled := Ok;

  Ok := Ok and
        (edNomeDLL.Text <> '') and
        FileExists(edNomeDLL.Text);

  imgErrSATInicializar.Visible := not(Ok);
  bInicializar.Enabled := Ok;

  imgErrSAT.Visible := not(Ok);
  imgErrSATLib.Visible := not(Ok);
  btConsultarStatusOPSAT.Enabled := Ok;

  //Botão Ativar Sat
  LigarAlertaErros;

  Ok := Ok and
        not(imgErrSATCodAtivacao.Visible) and
        not(imgErrSATEmitente.Visible) and
        not(imgErrSATCNPJSH.Visible) and
        not(imgErrSATAssign.Visible);

  imgErrSATAtivar.Visible := not(Ok);
  btAtivarsat.Enabled := Ok;

  if (Res <> imgErrSATAtivar.Visible) or (imgErrSATAtivar.Visible) then
  begin
    FMenuTreeView.AtualizaItemTela(tsDadosSAT.Tag,
                                    IfThen(imgErrSATAtivar.Visible
                                          , C_INDEX_IMG_ERRO
                                          , C_INDEX_IMG_FOLDER));
    FMenuTreeView.AtualizaItemTela(tsDadosEmit.Tag,
                                    IfThen(imgErrSATEmitente.Visible
                                          , C_INDEX_IMG_ERRO
                                          , C_INDEX_IMG_FOLDER));
    FMenuTreeView.AtualizaItemTela(tsDadosSwHouse.Tag,
                                    IfThen(imgErrSATAssign.Visible
                                          , C_INDEX_IMG_ERRO
                                          , C_INDEX_IMG_FOLDER));

  end;

end;

procedure TFrmACBrMonitor.ValidarConfigMail;
  procedure LigarAlertaErros;
      begin
        imgErrEmail_Name.Visible := not(edEmailNome.Text <> '');
        imgErrEmail_Mail.Visible := not(edEmailEndereco.Text <> '');
        imgErrEmail_User.Visible := not(edEmailUsuario.Text <> '');
        imgErrEmail_Smtp.Visible := not(edEmailHost.Text <> '');
        imgErrEmail_Senha.Visible := not(edEmailSenha.Text <> '');
        imgErrEmail_Porta.Visible := not(edEmailPorta.Text <> '');
      end;
var
  Ok, Res: Boolean;
begin
  Res:= imgErrEmail.Visible;

  LigarAlertaErros;
  Ok :=  not(imgErrEmail_Name.Visible) and
         not(imgErrEmail_Mail.Visible) and
         not(imgErrEmail_User.Visible) and
         not(imgErrEmail_Smtp.Visible) and
         not(imgErrEmail_Senha.Visible) and
         not(imgErrEmail_Porta.Visible );

  imgErrEmail.Visible := not Ok;
  bEmailTestarConf.Enabled := Ok;

  if ((Res <> imgErrEmail.Visible) or (imgErrEmail.Visible)) then
    FMenuTreeView.AtualizaItemTela(tsEmail.Tag,
                                    IfThen(imgErrEmail.Visible
                                          , C_INDEX_IMG_ERRO
                                          , tsEmail.ImageIndex));


end;

procedure TFrmACBrMonitor.VerificarErrosConfiguracaoComponentes(AfsCmd: TACBrCmd);
var
  MsgErro: String;
begin
  MsgErro:= '';
  if (AfsCmd.Objeto = 'NFE')
     or (AfsCmd.Objeto = 'CTE')
     or (AfsCmd.Objeto = 'MDFE')
     or (AfsCmd.Objeto = 'ESOCIAL')
     or (AfsCmd.Objeto = 'REINF')
     or (AfsCmd.Objeto = 'GNRE')
     or (AfsCmd.Objeto = 'BPE')
     or (AfsCmd.Objeto = 'NFSE')
     or (AfsCmd.Objeto = 'GTIN') then
       MsgErro :=  VerificarErrosConfiguracaoDFe;

  if (AfsCmd.Objeto = 'SAT') then
    MsgErro :=  VerificarErrosConfiguracaoSAT;

  if (AfsCmd.Objeto = 'EMAIL') then
    MsgErro :=  VerificarErrosConfiguracaoEMAIL;

  if (AfsCmd.Objeto = 'BOLETO') then
    MsgErro :=  VerificarErrosConfiguracaoBoleto;

  if MsgErro <> '' then
    AddLinesLog( 'ALERTA: ' + MsgErro );

end;

procedure TFrmACBrMonitor.VerificarErrosComunicacao;
begin
  if imgErrComunicacao.Visible then
    AddLinesLog( 'ALERTA- Configure a forma de Integração TCP/IP ou TXT (Menu: MONITOR)' );

end;

function TFrmACBrMonitor.VerificarErrosConfiguracaoDFe: String;
var
  MsgErro: String;
begin
  Result := '';
  MsgErro := '';

  if imgErrCertificado.Visible then
    MsgErro := MsgErro + sLineBreak + '- Certificado não configurado (Menu: DFE / CERTIFICADOS)';

  if imgErrWebServer.Visible then
    MsgErro := MsgErro + sLineBreak + '- WebService não configurado (Menu: DFE / WEBSERVICES) ';

  MsgErro := Trim(MsgErro);

  if (MsgErro <> '') then
    Result:=  MsgErro;

end;

function TFrmACBrMonitor.VerificarErrosConfiguracaoSAT: String;
var
  MsgErro: String;
begin
  Result := '';
  MsgErro := '';
  if imgErrSAT.Visible then
    MsgErro := '- Falha nas configurações SAT (Menu: SAT)';

  if (MsgErro <> '') then
    Result := MsgErro;

end;

function TFrmACBrMonitor.VerificarErrosConfiguracaoEMAIL: String;
var
  MsgErro: String;
begin
  Result := '';
  MsgErro := '';
  if imgErrEmail.Visible then
    MsgErro := '- Falha nas configurações de e-mail (Menu: E-MAIL)';

  if (MsgErro <> '') then
    Result := MsgErro;

end;

function TFrmACBrMonitor.VerificarErrosConfiguracaoBoleto: String;
var
  MsgErro: String;
begin
  Result := '';
  MsgErro := '';
  if imgErrCNPJBoleto.Visible then
    MsgErro := '- Falha nas configurações de Boleto (Menu: BOLETO / CEDENTE)';

  if (MsgErro <> '') then
    Result := MsgErro;

end;

procedure TFrmACBrMonitor.CarregarListaDeCidades(cUF: Integer);
var
  i: Integer;
  Cidade: TACBrIBGECidade;
begin
  if (cUF <= 0) or (fcUF = cUF) then
    Exit;

  fcUF := cUF;
  try
    ACBrIBGE1.BuscarPorcUF(FcUF);
    cbxEmitCidade.Items.Clear;
    cbxEmitCidade.ItemIndex := -1;
    fcMunList.Clear;
    for i := 0 to ACBrIBGE1.Cidades.Count-1 do
    begin
      Cidade := ACBrIBGE1.Cidades[i];
      cbxEmitCidade.Items.Add(Cidade.Municipio);
      fcMunList.Add(IntToStr(Cidade.CodMunicipio));
    end;

    if (cbxEmitCidade.Items.Count > 0) then
    begin
      cbxEmitCidade.ItemIndex := 0;
      cbxEmitCidadeChange(Nil);
    end;
  except
    AddLinesLog( 'ERRO: Erro ao carregar cidades! Verifique a configuração de CONSULTA IBGE.'  );
  end;
end;

procedure TFrmACBrMonitor.VerificarInterfaceNCM(Carregando: Boolean);
begin
  if Carregando and (ACBrNCMs1.NCMs.Count > 0) then  // Já carregou os itens?
    Exit;

  pnNCMCarregando.Visible := Carregando;
  gbNCMConsultar.Enabled := (not Carregando);
  gbNCMSalvarArquivo.Enabled := (not Carregando);
  Application.ProcessMessages;
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

procedure TFrmACBrMonitor.ConsultarModeloSAT;
var
  comando: String;
begin
  comando:= CObjSAT + '.' + CMetodoConsultarModeloSAT;
  if cbSATMarca.Items.Count <= 1 then
  begin
    try
      cbSATMarca.Items.Clear;

      fsCmd.Comando := comando;
      FDoSAT.Executar(fsCmd);

      cbSATMarca.Items.Text := StringReplace(fsCmd.Resposta, '|', sLineBreak, [rfReplaceAll]);

    finally
       cbSATMarca.Items.Insert(0, CSATManual);
       cbSATMarca.Text := IfEmptyThen( FMonitorConfig.SAT.Marca , CSATManual );
       cbSATMarcaChange(Self);
   end;
  end;

end;

procedure TFrmACBrMonitor.LeConfigMarcaSAT;
  function SectionSAT(N: Integer): String;
  begin
    Result := 'SAT'+IntToStr(N);
  end;

  function AjustarCaminhoPasta( AIniCaminhoPasta: String): String;
  begin
    Result := PathWithDelim( StringReplace(AIniCaminhoPasta,'{app}',
                             PathWithoutDelim(ExtractFilePath(Application.ExeName)), [rfReplaceAll]));
  end;

var
  I: Integer;
  Ini: TIniFile;
  dfesat_ini, Sec, Marca, LibName, LibFolder, AFile: String;
  Encontrada: Boolean;
  {$IfDef MSWINDOWS}
   J : Integer;
   FolderSource, FolderDest: String;
   SL: TStringList;
   CopyFlags: TCopyFileFlags;
  {$EndIf}
begin
  if FMonitorConfig.SAT.Marca = '' then
    exit;

  dfesat_ini := PathWithDelim(ExtractFilePath(Application.ExeName)) + CDirSAT + PathDelim + CDFeSATIniFile;
  if not FileExists(dfesat_ini) then
    raise Exception.CreateFmt(SErrArqNaoEncontrado,[dfesat_ini]);

  Encontrada := False;
  Ini := TIniFile.Create(dfesat_ini);
  try
    I := 1;
    while Ini.SectionExists(SectionSAT(I)) do
    begin
      Sec := SectionSAT(I);

      LibFolder := '';
      Marca := Ini.ReadString(Sec, CKeySATMarca, '');
      if LowerCase(Marca) = LowerCase(FMonitorConfig.SAT.Marca) then
      begin
        {$IfDef MSWINDOWS}
          {$IfDef CPU64}
          FolderSource := AjustarCaminhoPasta( Ini.ReadString(Sec, CKeySATPastaOrigemLib64, '' ) );
          LibName := Ini.ReadString( Sec, CKeySATLibWin64, '');
          {$Else}
          FolderSource := AjustarCaminhoPasta( Ini.ReadString(Sec, CKeySATPastaOrigemLib, '' ) );
          LibName := Ini.ReadString( Sec, CKeySATLibWin32, '');
          {$EndIf}

        FolderDest := AjustarCaminhoPasta( Ini.ReadString(Sec, CKeySATPastaDestLib, '' ) );
        if (FolderSource <> '') and (FolderDest <> '') then
        begin
          if (fSATLibsMarcas <> Marca) then
          begin
            fSATLibsMarcas := Marca;

            // Copiar arquivos de FolderSource to FolderDest
            SL := TStringList.Create;
            try
              FindFiles(FolderSource+'*.*', SL );

              For J := 0 to SL.Count-1 do
              begin
                AFile := ExtractFileName(SL[J]);

                // Verifica se Arquivo não pode ser substituido
                CopyFlags := [cffPreserveTime];
                if not AnsiMatchStr(LowerCase(AFile) , CDFeNaoSobreescrever ) then
                  CopyFlags := CopyFlags + [cffOverwriteFile];

                try
                  CopyFile( SL[J], FolderDest + AFile, CopyFlags );
                except
                  { ignora exceção de falha }
                end;
              end;
            finally
              SL.Free;
            end;
          end;

          LibFolder := FolderDest;
        end
        else
          LibFolder := FolderSource;

        if (LibName <> '') then
        begin
          LibName := LibFolder + LibName;
          if not FileExists(LibName) then
            raise Exception.CreateFmt( SErrArqNaoEncontrado, [LibName] );

        end;
        {$Else}
        LibName := Ini.ReadString( Sec, CKeySATLibLinux, '');
        {$EndIf}

        edNomeDLL.Text := LibName;
        {$IfDef MSWINDOWS}
        cbxModeloSAT.ItemIndex := integer(TACBrSATModelo(Ini.ReadInteger(Sec, CKeySATModelo, 2)));  // Default = 2-STDCALL
        {$Else}
        cbxModeloSAT.ItemIndex := 1;
        {$EndIf}

        sePagCod.Value := Ini.ReadInteger(Sec, CKeySATPaginaDeCodigo, CUTF8CodPage);  // Default = UTF8

        Encontrada := True;
        Break;
      end;

      Inc( I );
    end;
  finally
    Ini.Free;
    AjustaACBrSAT;
  end;

  if not Encontrada then
    raise Exception.CreateFmt( SErrSATMarcaNaoEncontrada, [FMonitorConfig.SAT.Marca, dfesat_ini] );

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
    ValidarNumeroSessaoResposta := cbxValidarNumeroSessaoResposta.Checked;

    ConfigArquivos.PastaCFeVenda := PathWithDelim(edSATPathArqs.Text)+'Vendas';
    ConfigArquivos.PastaCFeCancelamento := PathWithDelim(edSATPathArqsCanc.Text)+'Cancelamentos';
    ConfigArquivos.PastaEnvio:= PathWithDelim(edSATPathArqsEnvio.Text)+'Enviados';
    ConfigArquivos.SalvarCFe := cbxSATSalvarCFe.Checked;
    ConfigArquivos.SalvarCFeCanc := cbxSATSalvarCFeCanc.Checked;
    ConfigArquivos.SalvarEnvio := cbxSATSalvarEnvio.Checked;
    ConfigArquivos.SepararPorCNPJ := cbxSATSepararPorCNPJ.Checked;
    ConfigArquivos.SepararPorMes := cbxSATSepararPorMES.Checked;
    ConfigArquivos.SepararPorAno := cbxSATSepararPorANO.Checked;
    ConfigArquivos.SepararPorDia := cbxSATSepararPorDIA.Checked;
    ConfigArquivos.SepararPorModelo := cbxSATSepararPorModelo.Checked;
    ConfigArquivos.PrefixoArqCFe := edSATPrefixoCFe.Text;
    ConfigArquivos.PrefixoArqCFeCanc := edSATPrefixoCFeCanc.Text;

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

  if ( not(cbUsarEscPos.Checked) or (GerarPDF)) then
  begin
    ACBrSAT1.Extrato := ACBrSATExtratoFortes1;

    ACBrSATExtratoFortes1.LarguraBobina    := seLargura.Value;
    ACBrSATExtratoFortes1.MargemSuperior   := seMargemTopo.Value ;
    ACBrSATExtratoFortes1.MargemInferior   := seMargemFundo.Value ;
    ACBrSATExtratoFortes1.MargemEsquerda   := seMargemEsquerda.Value ;
    ACBrSATExtratoFortes1.MargemDireita    := seMargemDireita.Value ;
    ACBrSATExtratoFortes1.MostraPreview    := cbPreview.Checked;

    ACBrSATExtratoFortes1.ImprimeDescAcrescItem := cbxImprimirDescAcresItemSAT.Checked;
    ACBrSATExtratoFortes1.ImprimeEmUmaLinha     := cbxImprimirItem1LinhaSAT.Checked;
    ACBrSATExtratoFortes1.ImprimeCodigoEan      := cbxImprimirCodEANitemSAT.Checked;
    ACBrSATExtratoFortes1.ImprimeQRCodeLateral  := cbxQRCodeLateral.Checked;
    ACBrSATExtratoFortes1.ImprimeLogoLateral    := cbxLogoLateral.Checked;

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
         ACBrSATExtratoFortes1.Impressora := NomeImpressora
      else
      if lImpressora.Caption <> '' then
        ACBrSATExtratoFortes1.Impressora := lImpressora.Caption;
    except
    end;
  end
  else
  begin

    ACBrSAT1.Extrato := ACBrSATExtratoESCPOS1;

    ConfiguraPosPrinter;

    ACBrSATExtratoESCPOS1.ImprimeDescAcrescItem   := cbxImprimirDescAcresItemSAT.Checked;
    ACBrSATExtratoESCPOS1.ImprimeEmUmaLinha       := cbxImprimirItem1LinhaSAT.Checked;
    ACBrSATExtratoESCPOS1.PosPrinter.Device.Porta := cbxPorta.Text;
    ACBrSATExtratoESCPOS1.ImprimeChaveEmUmaLinha  := TAutoSimNao(rdgImprimeChave1LinhaSAT.ItemIndex);
    ACBrSATExtratoESCPOS1.ImprimeCodigoEan        := cbxImprimirCodEANitemSAT.Checked;
    ACBrSATExtratoESCPOS1.ImprimeQRCodeLateral    := cbxQRCodeLateral.Checked;
    ACBrSATExtratoESCPOS1.ImprimeLogoLateral      := cbxLogoLateral.Checked;

    ACBrSATExtratoESCPOS1.PosPrinter.Device.Ativar;
    ACBrSATExtratoESCPOS1.ImprimeQRCode := True;
  end;

  {$IFDEF Demo}
  AtualizarImpressaoHomologacaoDFe(ACBrSAT1.Extrato);
  {$ELSE}
  if (edSH_RazaoSocial.Text <> '') then
  begin
    ACBrSAT1.Extrato.Sistema := edSH_RazaoSocial.Text;
    ACBrSAT1.Extrato.Site    := edSH_Site.Text;
  end;
  {$ENDIF}

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
  if NaoEstaVazio(Dir) then
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
      if pgConfig.ActivePage = tsBAL then
        MessageDlg('Balança', 'BAL -> Balança não responde!', mtInformation, [mbOK], 0);
    end;
  end
  else
    ACBrBAL1.Desativar;

  AvaliaEstadoTsBAL;
end;

{------------------------------------------------------------------------------}
procedure TFrmACBrMonitor.bBALTestarClick(Sender: TObject);
var
  AResult: String;
begin
  ACBrBAL1.LePeso;
  if ACBrBAL1.UltimaResposta <> '' then
  begin
    AResult:= Format('BAL -> Peso Lido: %f', [ACBrBAL1.UltimoPesoLido]);
    AddLinesLog(AResult);
    MessageDlg('Balança', AResult, mtInformation, [mbOK], 0);
  end
  else
  begin
    AResult:= 'BAL -> Timeout';
    AddLinesLog(AResult);
    MessageDlg('Balança', AResult, mtInformation, [mbOK], 0);
  end;
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
var
  AMsg: String;
begin
  if not TCPServerTC.Ativo then
    TCPServerTC.Port := edTCPort.Text;

  if not FileExists(edTCArqPrecos.Text) then
    raise Exception.Create('Arquivo de Preços não encontrado em: [' +
      edTCArqPrecos.Text + ']');

  TCPServerTC.Ativo := (bTCAtivar.Caption = '&Ativar');
  TimerTC.Enabled := TCPServerTC.Ativo;

  AvaliaEstadoTsTC;

  AMsg:= 'Servidor de Terminal de Consulta: ' + IfThen(TCPServerTC.Ativo, 'ATIVADO', 'DESATIVADO');
  AddLinesLog(AMsg);

end;

procedure TFrmACBrMonitor.tsSatShow(Sender: TObject);
begin
  pgSAT.ActivePageIndex := 0;
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

procedure TFrmACBrMonitor.ConfiguraDANFe(GerarPDF: Boolean; MostrarPreview: String);
//MostrarPreview está sendo Tratado como String, pois pode receber três parâmetros: True, False, Vazio
//(a definição vazio '' permite utilizar a configuração preview definida em tela)
var
  OK: boolean;
  tDescPagto: TDescricaoPagamento;
begin
  if ACBrNFe1.NotasFiscais.Count > 0 then
  begin
    if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 65 then
    begin
      if (rgModeloDANFeNFCE.ItemIndex = CMODELO_NFCE_FORTES) or GerarPDF then
      begin
        if (rgModoImpressaoEvento.ItemIndex = CIMPRESSAO_NFCE_A4) then
          ACBrNFe1.DANFE := ACBrNFeDANFCeFortesA4_1
        else
          ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1;
      end
      else
        ACBrNFe1.DANFE := ACBrNFeDANFeESCPOS1;

      ACBrNFe1.DANFE.Impressora := cbxImpressoraNFCe.Text;
    end
    else
    begin
      ACBrNFe1.DANFE := ACBrNFeDANFeRL1;
      if NaoEstaVazio(cbxImpressora.Text) then
        ACBrNFe1.DANFE.Impressora := cbxImpressora.Text
    end;

    if (ACBrNFe1.NotasFiscais.Items[0].NFe.procNFe.cStat in [101, 151, 155]) then
       ACBrNFe1.DANFE.Cancelada := True
    else
       ACBrNFe1.DANFE.Cancelada := False;
  end
  else if NaoEstaVazio(cbxImpressora.Text) then
    ACBrNFe1.DANFE.Impressora := cbxImpressora.Text;

  if GerarPDF and not DirectoryExists(PathWithDelim(edtPathPDF.Text))then
    ForceDirectories(PathWithDelim(edtPathPDF.Text));

  if ACBrNFe1.DANFE <> nil then
  begin
    ACBrNFe1.DANFE.TipoDANFE            := StrToTpImp(OK, IntToStr(rgTipoDanfe.ItemIndex + 1));
    ACBrNFe1.DANFE.Logo                 := edtLogoMarca.Text;
    ACBrNFe1.DANFE.Email                := edtEmailEmpresa.Text;
    ACBrNFe1.DANFE.NumCopias            := edtNumCopia.Value;
    ACBrNFe1.DANFE.MargemInferior       := fspeMargemInf.Value;
    ACBrNFe1.DANFE.MargemSuperior       := fspeMargemSup.Value;
    ACBrNFe1.DANFE.MargemDireita        := fspeMargemDir.Value;
    ACBrNFe1.DANFE.MargemEsquerda       := fspeMargemEsq.Value;
    ACBrNFe1.DANFE.PathPDF              := PathWithDelim(edtPathPDF.Text);
    ACBrNFe1.DANFE.CasasDecimais.qCom   := spedtCasasDecimaisQtd.Value;
    ACBrNFe1.DANFE.CasasDecimais.vUnCom := spedtDecimaisVUnit.Value;
    ACBrNFe1.DANFE.ImprimeTotalLiquido  := cbxImpValLiq.Checked;
    ACBrNFe1.DANFE.MostraStatus := cbxMostraStatus.Checked;
    ACBrNFe1.DANFE.ExpandeLogoMarca := cbxExpandirLogo.Checked;
    ACBrNFe1.DANFE.UsaSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;

    {$IFDEF Demo}
    AtualizarImpressaoHomologacaoDFe(ACBrNFe1.DANFE);
    {$ELSE}
    ACBrNFe1.DANFE.Sistema              := edSH_RazaoSocial.Text;
    ACBrNFe1.DANFE.Site                 := edtSiteEmpresa.Text;
    {$ENDIF}

    if (ACBrNFe1.DANFE is TACBrNFeDANFEClass) then
    begin
      (ACBrNFe1.DANFE as TACBrNFeDANFEClass).ImprimeDescPorPercentual := cbxImpDescPorc.Checked;
      (ACBrNFe1.DANFE as TACBrNFeDANFEClass).ExibeResumoCanhoto       := cbxExibeResumo.Checked;
      if (cbxExibeResumo.Checked) and (trim(edtMsgResumoCanhoto.Text) <> '') then
        (ACBrNFe1.DANFE as TACBrNFeDANFEClass).TextoResumoCanhoto     := SubstituirVariaveis(edtMsgResumoCanhoto.Text)
      else
        (ACBrNFe1.DANFE as TACBrNFeDANFEClass).TextoResumoCanhoto     := edtMsgResumoCanhoto.Text;

      (ACBrNFe1.DANFE as TACBrNFeDANFEClass).FormularioContinuo       := cbxFormCont.Checked;
      (ACBrNFe1.DANFE as TACBrNFeDANFEClass).PosCanhoto               := TPosRecibo( rgLocalCanhoto.ItemIndex );
    end;

    if ACBrNFe1.DANFE = ACBrNFeDANFeRL1 then
    begin
      ACBrNFeDANFeRL1.Fonte.Nome := TNomeFonte(rgTipoFonte.ItemIndex);
      ACBrNFeDANFeRL1.Fonte.TamanhoFonteDemaisCampos := speFonteCampos.Value;
      ACBrNFeDANFeRL1.Fonte.TamanhoFonteEndereco     := speFonteEndereco.Value;
      ACBrNFeDANFeRL1.Fonte.TamanhoFonteInformacoesComplementares := speFonteAdic.Value;
      ACBrNFeDANFeRL1.LarguraCodProd := speLargCodProd.Value;
      ACBrNFeDANFeRL1.ExibeEAN := cbxExibirEAN.Checked;
      ACBrNFeDANFeRL1.ExibeCampoFatura := cbxExibirCampoFatura.Checked;
      ACBrNFeDANFeRL1.QuebraLinhaEmDetalhamentos := cbxQuebrarLinhasDetalhesItens.Checked;
      ACBrNFeDANFeRL1.Fonte.TamanhoFonteRazaoSocial := speFonteRazao.Value;
      ACBrNFeDANFeRL1.EspacoEntreProdutos := speEspEntreProd.Value;
      ACBrNFeDANFeRL1.AltLinhaComun := speAlturaCampos.Value;
      ACBrNFeDANFeRL1.PosCanhoto := TPosRecibo( rgLocalCanhoto.ItemIndex );
      ACBrNFeDANFeRL1.PosCanhotoLayout := TPosReciboLayout( rgLayoutCanhoto.ItemIndex );
      ACBrNFeDANFeRL1.ImprimeValor := TImprimirUnidQtdeValor(cbxUnComTributavel.ItemIndex);
      ACBrNFeDANFeRL1.ImprimeDetalhamentoEspecifico := cbxImpDetEspNFe.Checked;
      ACBrNFeDANFeRL1.ExibeDadosDocReferenciados := cbxImpDocsReferenciados.Checked;
      ACBrNFeDANFeRL1.ExibeInforAdicProduto := TinfAdcProd(rgInfAdicProduto.ItemIndex);
      ACBrNFeDANFeRL1.LogoemCima := cbxExibirLogoEmCima.Checked;
      ACBrNFeDANFeRL1.ExibeDadosInscricaoSuframa := cbxImprimeInscSuframa.Checked;
      ACBrNFeDANFeRL1.ExpandirDadosAdicionaisAuto:= cbxExpandirDadosAdicionaisAuto.Checked;
      ACBrNFeDANFeRL1.ImprimeContinuacaoDadosAdicionaisPrimeiraPagina:= cbxImprimeContinuacaoDadosAdicionaisPrimeiraPagina.Checked;
      ACBrNFeDANFeRL1.ImprimeDescAcrescItem:= TpcnImprimeDescAcrescItem(rgImprimeDescAcrescItemNFe.ItemIndex);
      ACBrNFeDANFeRL1.ExibeCampoDePagamento:= TpcnInformacoesDePagamento(rgInfFormaPagNFe.ItemIndex);
      ACBrNFeDANFeRL1.ExibeTotalTributosItem:= cbxExibeTotalTributosItem.Checked;
      ACBrNFeDANFeRL1.ImprimeXPedNItemPed := cbxImprimeXPedNitemPed.Checked;
    end
    else if ACBrNFe1.DANFE = ACBrNFeDANFCeFortesA4_1 then
    begin
      ACBrNFeDANFCeFortesA4_1.ExibeInforAdicProduto := TinfAdcProd(rgInfAdicProduto.ItemIndex);
      ACBrNFeDANFCeFortesA4_1.ImprimeDescAcrescItem := cbxImprimirDescAcresItemNFCe.Checked;
      ACBrNFeDANFCeFortesA4_1.ImprimeTotalLiquido   := cbxImprimirDescAcresItemNFCe.Checked;
      ACBrNFeDANFCeFortesA4_1.MargemInferior        := fspeMargemInf.Value;
      ACBrNFeDANFCeFortesA4_1.MargemSuperior        := fspeMargemSup.Value;
      ACBrNFeDANFCeFortesA4_1.MargemDireita         := fspeMargemDir.Value;
      ACBrNFeDANFCeFortesA4_1.MargemEsquerda        := fspeMargemEsq.Value;
      ACBrNFeDANFCeFortesA4_1.ImprimeCodigoEan      := cbxImprimirCodigoEANNFCe.Checked;
      ACBrNFeDANFCeFortesA4_1.ImprimeNomeFantasia   := cbxImprimirNomeFantasiaNFCe.Checked;
      ACBrNFeDANFCeFortesA4_1.ExibeTotalTributosItem:= cbxExibeTotalTributosItem.Checked;
      ACBrNFeDANFCeFortesA4_1.ImprimeTributos       := TpcnTributos(rgImprimeTributos.ItemIndex);
      ACBrNFeDANFCeFortesA4_1.ImprimeItens          := cbxImprimeItens.Checked;
      if ( Trim(edtLogoMarcaNFCeSAT.Text) <> '') and FileExists(edtLogoMarcaNFCeSAT.Text) then
        ACBrNFeDANFCeFortesA4_1.Logo                := edtLogoMarcaNFCeSAT.Text
      else
        ACBrNFeDANFCeFortesA4_1.Logo                := '';
      ACBrNFeDANFCeFortesA4_1.NumCopias             := edtNumCopiaNFCe.Value;

    end
    else if ACBrNFe1.DANFE = ACBrNFeDANFCeFortes1 then
    begin
      ACBrNFeDANFCeFortes1.ImprimeDescAcrescItem := cbxImprimirDescAcresItemNFCe.Checked;
      ACBrNFeDANFCeFortes1.ImprimeTotalLiquido   := cbxImprimirDescAcresItemNFCe.Checked;
      ACBrNFeDANFCeFortes1.MargemInferior        := fspeNFCeMargemInf.Value;
      ACBrNFeDANFCeFortes1.MargemSuperior        := fspeNFCeMargemSup.Value;
      ACBrNFeDANFCeFortes1.MargemDireita         := fspeNFCeMargemDir.Value;
      ACBrNFeDANFCeFortes1.MargemEsquerda        := fspeNFCeMargemEsq.Value;
      ACBrNFeDANFCeFortes1.LarguraBobina         := fspeLarguraNFCe.Value;
      ACBrNFeDANFCeFortes1.ImprimeEmUmaLinha     := cbxImprimirItem1LinhaNFCe.Checked;
      ACBrNFEDANFCeFortes1.ImprimeQRCodeLateral  := cbxImprimirQRCodeLateralNFCe.Checked;
      ACBrNFeDANFCeFortes1.ImprimeCodigoEan      := cbxImprimirCodigoEANNFCe.Checked;
      ACBrNFeDANFCeFortes1.ImprimeNomeFantasia   := cbxImprimirNomeFantasiaNFCe.Checked;
      ACBrNFeDANFCeFortes1.ExibeTotalTributosItem:= cbxExibeTotalTributosItem.Checked;
      ACBrNFeDANFCeFortes1.ImprimeTributos       := TpcnTributos(rgImprimeTributos.ItemIndex);
      ACBrNFeDANFCeFortes1.NumCopias             := edtNumCopiaNFCe.Value;
      ACBrNFEDANFCeFortes1.ImprimeLogoLateral    := cbxImprimirLogoLateralNFCe.Checked;
      ACBrNFEDANFCeFortes1.ImprimeItens          := cbxImprimeItens.Checked;

      ACBrNFeDANFCeFortes1.DescricaoPagamentos   := [];
      for tDescPagto:= Low(tDescPagto) to High(tDescPagto) do
        if chgDescricaoPagamento.Checked[ Integer(tDescPagto) ] then
          ACBrNFeDANFCeFortes1.DescricaoPagamentos := ACBrNFeDANFCeFortes1.DescricaoPagamentos
                                                   + [tDescPagto];

      ACBrNFeDANFCeFortes1.FonteLinhaItem.Name     := FontDialog1.Font.Name ;
      ACBrNFeDANFCeFortes1.FonteLinhaItem.Size     := FontDialog1.Font.Size;
      ACBrNFeDANFCeFortes1.FonteLinhaItem.Style    := FontDialog1.Font.Style;

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
      ACBrNFeDANFeESCPOS1.ImprimeQRCodeLateral  := cbxImprimirQRCodeLateralNFCe.Checked;
      ACBrNFeDANFeESCPOS1.ImprimeNomeFantasia   := cbxImprimirNomeFantasiaNFCe.Checked;
      ACBrNFeDANFeESCPOS1.ExibeTotalTributosItem:= cbxExibeTotalTributosItem.Checked;
      ACBrNFeDANFeESCPOS1.ImprimeTributos       := TpcnTributos(rgImprimeTributos.ItemIndex);
      ACBrNFeDANFeESCPOS1.NumCopias             := edtNumCopiaNFCe.Value;
      ACBrNFeDANFeESCPOS1.ImprimeLogoLateral    := cbxImprimirLogoLateralNFCe.Checked;
      ACBrNFeDANFeESCPOS1.ImprimeItens          := cbxImprimeItens.Checked;

      ACBrNFeDANFeESCPOS1.DescricaoPagamentos   := [];
      for tDescPagto:= Low(tDescPagto) to High(tDescPagto) do
        if chgDescricaoPagamento.Checked[ Integer(tDescPagto) ] then
          ACBrNFeDANFeESCPOS1.DescricaoPagamentos := ACBrNFeDANFeESCPOS1.DescricaoPagamentos
                                                   + [tDescPagto];

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

  ACBrNFe1.DANFE.MostraPreview := False;
  if ((not GerarPDF) and (ACBrNFe1.DANFE <> ACBrNFeDANFeESCPOS1)) then
    if EstaVazio(MostrarPreview) then
      ACBrNFe1.DANFE.MostraPreview := cbxMostrarPreview.Checked
    else
      ACBrNFe1.DANFE.MostraPreview := StrToBoolDef(MostrarPreview, False);

  //if ACBrNFe1.DANFE.MostrarPreview or MostrarPreview then
  //  ForceForeground(Self.Handle);
end;

procedure TFrmACBrMonitor.ConfiguraDACTe(GerarPDF: Boolean; MostrarPreview: String);
//MostrarPreview está sendo Tratado como String, pois pode receber três parâmetros: True, False, Vazio
//(a definição vazio '' permite utilizar a configuração preview definida em tela)
var
  OK: boolean;
begin
  if ACBrCTe1.Conhecimentos.Count > 0 then
  begin
    ACBrCTe1.DACTE := ACBrCTeDACTeRL1;
    if NaoEstaVazio(cbxImpressora.Text) then
      ACBrCTe1.DACTE.Impressora := cbxImpressora.Text;

    if (ACBrCTe1.Conhecimentos.Items[0].CTe.procCTe.cStat in [101, 151, 155]) then
       ACBrCTe1.DACTE.Cancelada := True
    else
       ACBrCTe1.DACTE.Cancelada := False;
  end;

  if GerarPDF and not DirectoryExists(PathWithDelim(edtPathPDF.Text))then
    ForceDirectories(PathWithDelim(edtPathPDF.Text));

  if ACBrCTe1.DACTE <> nil then
  begin
    ACBrCTe1.DACTE.TipoDACTE := StrToTpImp(OK, IntToStr(rgTipoDanfe.ItemIndex + 1));
    ACBrCTe1.DACTE.Logo := edtLogoMarca.Text;
    ACBrCTe1.DACTE.Email := edtEmailEmpresa.Text;
    ACBrCTe1.DACTE.ImprimeDescPorc := cbxImpDescPorc.Checked;
    ACBrCTe1.DACTE.NumCopias := edtNumCopia.Value;
    ACBrCTe1.DACTE.MargemInferior := fspeMargemInf.Value;
    ACBrCTe1.DACTE.MargemSuperior := fspeMargemSup.Value;
    ACBrCTe1.DACTE.MargemDireita := fspeMargemDir.Value;
    ACBrCTe1.DACTE.MargemEsquerda := fspeMargemEsq.Value;
    ACBrCTe1.DACTE.PathPDF := PathWithDelim(edtPathPDF.Text);
    ACBrCTe1.DACTE.ExibeResumoCanhoto := cbxExibeResumo.Checked;
    ACBrCTe1.DACTE.MostraStatus := cbxMostraStatus.Checked;
    ACBrCTe1.DACTE.ExpandeLogoMarca := cbxExpandirLogo.Checked;
    ACBrCTe1.DACTE.PosCanhoto := TPosRecibo( rgLocalCanhoto.ItemIndex );
    ACBrCTe1.DACTE.UsaSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;

    {$IFDEF Demo}
    AtualizarImpressaoHomologacaoDFe(ACBrCTe1.DACTE);
    {$ELSE}
    ACBrCTe1.DACTE.Sistema := edSH_RazaoSocial.Text;
    ACBrCTe1.DACTE.Site := edtSiteEmpresa.Text;
    {$ENDIF}

    if ACBrCTe1.DACTE = ACBrCTeDACTeRL1 then
    begin
//      ACBrCTeDACTeRL1.Fonte.Nome := TNomeFonte(rgTipoFonte.ItemIndex);
//      ACBrCTeDACTeRL1.Fonte.TamanhoFonte_RazaoSocial := speFonteRazao.Value;
//      ACBrCTeDACTeRL1.AltLinhaComun := speAlturaCampos.Value;
      ACBrCTeDACTeRL1.PosCanhoto := TPosRecibo( rgLocalCanhoto.ItemIndex );
    end;
  end;

  ACBrCTe1.DACTE.MostraPreview := False;
  if (not GerarPDF) then
    if EstaVazio(MostrarPreview) then
      ACBrCTe1.DACTE.MostraPreview := cbxMostrarPreview.Checked
    else
      ACBrCTe1.DACTE.MostraPreview := StrToBoolDef(MostrarPreview, False);
end;

procedure TFrmACBrMonitor.ConfiguraDABPe(GerarPDF: Boolean;
  MostrarPreview: String);
begin
  GerarPDF:= False;
  MostrarPreview:= '';
  ACBrBPe1.DABPE := ACBrBPeDABPeESCPOS1;
  ACBrBPeDABPeESCPOS1.PosPrinter.Modelo := TACBrPosPrinterModelo(cbxModelo.ItemIndex);
  ACBrBPeDABPeESCPOS1.PosPrinter.Device.Porta := cbxPorta.Text;
  ACBrBPeDABPeESCPOS1.ImprimeNomeFantasia := cbxImprimirNomeFantasiaNFCe.Checked;

  if not ACBrPosPrinter1.ControlePorta then
  begin
     ACBrBPeDABPeESCPOS1.PosPrinter.Ativar;
     if not ACBrBPeDABPeESCPOS1.PosPrinter.Device.Ativo then
       ACBrBPeDABPeESCPOS1.PosPrinter.Device.Ativar;
  end
end;

procedure TFrmACBrMonitor.ConfiguraDANFSe(GerarPDF: Boolean;
  MostrarPreview: String);
//MostrarPreview está sendo Tratado como String, pois pode receber três parâmetros: True, False, Vazio
//(a definição vazio '' permite utilizar a configuração preview definida em tela)
var
  OK: boolean;
begin
  if ACBrNFSeX1.NotasFiscais.Count > 0 then
  begin
    ACBrNFSeX1.DANFSE := ACBrNFSeXDANFSeRL1;
    if NaoEstaVazio(cbxImpressora.Text) then
      ACBrNFSeX1.DANFSE.Impressora := cbxImpressora.Text;
  end;

  if GerarPDF and not DirectoryExists(PathWithDelim(edtPathPDF.Text))then
    ForceDirectories(PathWithDelim(edtPathPDF.Text));

  if ACBrNFSeX1.DANFSE <> nil then
  begin
    ACBrNFSeX1.DANFSE.Logo := edtLogoMarcaPrefeitura.Text;
    ACBrNFSeX1.DANFSE.Prefeitura := edtNomePrefeitura.Text;
    ACBrNFSeX1.DANFSE.Email := edtEmailEmpresa.Text;
    ACBrNFSeX1.DANFSE.NumCopias := edtNumCopia.Value;
    ACBrNFSeX1.DANFSE.MargemInferior := fspeMargemInf.Value;
    ACBrNFSeX1.DANFSE.MargemSuperior := fspeMargemSup.Value;
    ACBrNFSeX1.DANFSE.MargemDireita := fspeMargemDir.Value;
    ACBrNFSeX1.DANFSE.MargemEsquerda := fspeMargemEsq.Value;
    ACBrNFSeX1.DANFSE.PathPDF := PathWithDelim(edtPathPDF.Text);
    ACBrNFSeX1.DANFSE.MostraStatus := cbxMostraStatus.Checked;
    ACBrNFSeX1.DANFSE.ExpandeLogoMarca := cbxExpandirLogo.Checked;
    ACBrNFSeX1.DANFSE.UsaSeparadorPathPDF := cbxUsarSeparadorPathPDF.Checked;

    ACBrNFSeX1.DANFSE.Prestador.Logo := edtLogoMarca.Text;

    {$IFDEF Demo}
    AtualizarImpressaoHomologacaoDFe(ACBrCTe1.DACTE);
    {$ELSE}
    ACBrNFSeX1.DANFSE.Sistema := edSH_RazaoSocial.Text;
    ACBrNFSeX1.DANFSE.Site := edtSiteEmpresa.Text;
    {$ENDIF}
  end;

  ACBrNFSeX1.DANFSE.MostraPreview := False;
  if (not GerarPDF) then
    if EstaVazio(MostrarPreview) then
      ACBrNFSeX1.DANFSE.MostraPreview := cbxMostrarPreview.Checked
    else
      ACBrNFSeX1.DANFSE.MostraPreview := StrToBoolDef(MostrarPreview, False);
end;


procedure TFrmACBrMonitor.VerificaDiretorios;
var
  CanEnabled: Boolean;
begin
  CanEnabled := cbxSalvarArqs.Checked;

  if not(CanEnabled) then
  begin
    cbxPastaMensal.Checked := False;
    cbxAdicionaLiteral.Checked  := False;
    cbxEmissaoPathNFe.Checked  := False;
    cbxSalvaPathEvento.Checked  := False;
    cbxSepararPorCNPJ.Checked  := False;
    cbxSepararporModelo.Checked  := False;
    cbxSalvarNFesProcessadas.Checked  := False;
  end;

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
  sbPathDownload.Enabled := CanEnabled;

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
      if (chkMostraLogNaTela.Checked) and (Self.WindowState <> wsMinimized) then
      begin
        if ( mResp.Lines.Count > 500 ) then
           RemoveLinesLog;

        mResp.Lines.Add(aLineLog);
      end;

      if (chkMostraLogNaTela.Checked) and (mResp.GetTextLen = 0) and (Self.WindowState = wsMinimized) then
        mResp.Lines.Add('Gerando Log em: '+ AcertaPath(edLogArq.Text) + sLineBreak + 'O Log em tela é apresentado apenas com o ACBrMonitor aberto!');

      AddLinesLogFile(ArqLogTXT, aLineLog, True, True, True);

      Application.ProcessMessages;
    end;
  end;
end;

procedure TFrmACBrMonitor.AddLinesLog(aLinesLog: TStrings);
begin
  if (chkMostraLogNaTela.Checked) and (Self.WindowState <> wsMinimized) and ( aLinesLog.Count > 0 ) then
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
    FDoBoleto.ImprimeRelatorioRetorno(lsvArqsRetorno.Selected.Caption);

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

    ACBrPosPrinter1.ConfigGaveta.TempoON := seGavetaTempoON.Value;
    ACBrPosPrinter1.ConfigGaveta.TempoOFF:= seGavetaTempoOFF.Value;
    ACBrPosPrinter1.ConfigGaveta.SinalInvertido:= cbGavetaSinalInvertido.Checked;

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
  //PathMunIBGE: String;
  PathSchemaDFe: String;
begin
  PathSchemaDFe := '';
  //PathMunIBGE := PathWithDelim(ExtractFilePath(Application.ExeName)) + 'MunIBGE' + PathDelim ;
  with Configuracoes do
  begin
    with Geral do
    begin
      Salvar := ckSalvar.Checked;

      try
        SSLLib := TSSLLib(cbSSLLib.ItemIndex);
      Except
        on E: Exception do
        begin
          {$IFDEF LINUX}
            SSLLib  := libOpenSSL;
          {$ELSE}
            SSLLib  := libWinCrypt;
          {$ENDIF}
          cbSSLLib.ItemIndex := Integer( SSLLib );
          AddLinesLog(E.Message
                      + sLineBreak + Format(SErroSSLDesabilitado, [GetEnumName(TypeInfo(TSSLLib), Integer( SSLLib ) )]) );
        end;
      end;

      try
        SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      Except
        on E: Exception do
        begin
          {$IFDEF LINUX}
            SSLCryptLib  := cryOpenSSL;
          {$ELSE}
            SSLCryptLib  := cryWinCrypt;
          {$ENDIF}
          cbCryptLib.ItemIndex := Integer( SSLCryptLib );
          AddLinesLog(E.Message
                      + sLineBreak + Format(SErroSSLDesabilitado, [GetEnumName(TypeInfo(TSSLCryptLib), Integer( SSLCryptLib ) )]) );
        end;
      end;

      try
        SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      Except
        on E: Exception do
        begin
          {$IFDEF LINUX}
            SSLHttpLib  := httpOpenSSL;
          {$ELSE}
            SSLHttpLib  := httpWinHttp;
          {$ENDIF}
          cbHttpLib.ItemIndex := Integer( SSLHttpLib );
          AddLinesLog(E.Message
                      + sLineBreak + Format(SErroSSLDesabilitado, [GetEnumName(TypeInfo(TSSLHttpLib), Integer( SSLHttpLib ) )]) );
        end;
      end;

      try
        SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
      Except
        on E: Exception do
        begin
          SSLXmlSignLib  := xsLibXml2;
          cbXmlSignLib.ItemIndex := Integer( SSLXmlSignLib );
          AddLinesLog(E.Message
                      + sLineBreak + Format(SErroSSLDesabilitado, [GetEnumName(TypeInfo(TSSLXmlSignLib), Integer( SSLXmlSignLib ) )]) );
        end;
      end;

      ValidarDigest  := cbValidarDigest.Checked;
      RetirarAcentos := cbRetirarAcentos.Checked;
      RetirarEspacos := cbRetirarEspacos.Checked;
    end;

    with Certificados do
    begin
      ArquivoPFX  := edtArquivoPFX.Text;
      URLPFX := edtURLPFX.Text;
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

    with RespTec do
    begin
      CSRT := edtCSRT.Text;
      if NaoEstaVazio(edtIdCSRT.Text) then
        IdCSRT := StrToIntDef(edtIdCSRT.Text,0);
    end;
  end;

  // Configurações específicas
  if Configuracoes is TConfiguracoesNFe then
  begin
    TConfiguracoesNFe(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoNFe.ItemIndex+1));
    TConfiguracoesNFe(Configuracoes).Geral.VersaoDF     := StrToVersaoDF(ok, cbVersaoWS.Text);
    TConfiguracoesNFe(Configuracoes).Geral.VersaoQRCode := StrToVersaoQrCode(ok, cbVersaoWSQRCode.Text);
    TConfiguracoesNFe(Configuracoes).Geral.AtualizarXMLCancelado:= FMonitorConfig.DFE.Diretorios.AtualizarXMLCancelado;
    TConfiguracoesNFe(Configuracoes).Arquivos.IniServicos    := edtArquivoWebServicesNFe.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.EmissaoPathNFe := cbxEmissaoPathNFe.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.SalvarEvento   := cbxSalvaPathEvento.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.PathNFe        := edtPathNFe.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.PathInu        := edtPathInu.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.PathEvento     := edtPathEvento.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.DownloadDFe.PathDownload:= edtPathDownload.Text;
    TConfiguracoesNFe(Configuracoes).Arquivos.DownloadDFe.SepararPorNome:= cbxSepararPorNome.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.SalvarApenasNFeProcessadas := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesNFe(Configuracoes).Arquivos.NormatizarMunicipios  := cbxNormatizarMunicipios.Checked;
    //TConfiguracoesNFe(Configuracoes).Arquivos.PathArquivoMunicipios := PathMunIBGE;
    TConfiguracoesNFe(Configuracoes).Geral.CamposFatObrigatorios    := ckCamposFatObrigatorio.Checked;
    TConfiguracoesNFe(Configuracoes).Geral.ForcarGerarTagRejeicao938 := TForcarGeracaoTag(cbTagRejeicao938.ItemIndex);

    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'NFe';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoesNFe(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe ;

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
    TConfiguracoesCTe(Configuracoes).Arquivos.DownloadDFe.PathDownload:= edtPathDownload.Text;
    TConfiguracoesCTe(Configuracoes).Arquivos.DownloadDFe.SepararPorNome:= cbxSepararPorNome.Checked;
    TConfiguracoesCTe(Configuracoes).Arquivos.SalvarApenasCTeProcessados := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesCTe(Configuracoes).Arquivos.NormatizarMunicipios  := cbxNormatizarMunicipios.Checked;
    //TConfiguracoesCTe(Configuracoes).Arquivos.PathArquivoMunicipios := PathMunIBGE;

    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'CTe';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoesCTe(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe ;

  end
  else if Configuracoes is TConfiguracoesMDFe then
  begin
    TConfiguracoesMDFe(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoMDFe.ItemIndex + 1));
    TConfiguracoesMDFe(Configuracoes).Geral.VersaoDF     := StrToVersaoMDFe(ok, cbVersaoWSMDFe.Text);

    TConfiguracoesMDFe(Configuracoes).Arquivos.IniServicos     := edtArquivoWebServicesMDFe.Text;
    TConfiguracoesMDFe(Configuracoes).Arquivos.EmissaoPathMDFe := cbxEmissaoPathNFe.Checked;
    TConfiguracoesMDFe(Configuracoes).Arquivos.PathMDFe        := edtPathNFe.Text;
    TConfiguracoesMDFe(Configuracoes).Arquivos.PathEvento      := edtPathEvento.Text;
    TConfiguracoesMDFe(Configuracoes).Arquivos.DownloadDFe.PathDownload:= edtPathDownload.Text;
    TConfiguracoesMDFe(Configuracoes).Arquivos.DownloadDFe.SepararPorNome:= cbxSepararPorNome.Checked;
    TConfiguracoesMDFe(Configuracoes).Arquivos.SalvarApenasMDFeProcessados := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesMDFe(Configuracoes).Arquivos.NormatizarMunicipios := cbxNormatizarMunicipios.Checked;
    //TConfiguracoesMDFe(Configuracoes).Arquivos.PathArquivoMunicipios := PathMunIBGE;

    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'MDFe';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoesMDFe(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe ;

  end
  else if Configuracoes is TConfiguracoesBPe then
  begin
    TConfiguracoesBPe(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoBPe.ItemIndex + 1));
    TConfiguracoesBPe(Configuracoes).Geral.VersaoDF     := StrToVersaoBPe(ok, cbVersaoWSBPe.Text);

    TConfiguracoesBPe(Configuracoes).Arquivos.IniServicos    := edtArquivoWebServicesBPe.Text;
    TConfiguracoesBPe(Configuracoes).Arquivos.EmissaoPathBPe := cbxEmissaoPathNFe.Checked;
    TConfiguracoesBPe(Configuracoes).Arquivos.PathBPe        := edtPathNFe.Text;
    TConfiguracoesBPe(Configuracoes).Arquivos.PathEvento     := edtPathEvento.Text;
    TConfiguracoesBPe(Configuracoes).Arquivos.DownloadDFe.PathDownload:= edtPathDownload.Text;
    TConfiguracoesBPe(Configuracoes).Arquivos.DownloadDFe.SepararPorNome:= cbxSepararPorNome.Checked;
    TConfiguracoesBPe(Configuracoes).Arquivos.SalvarApenasBPeProcessadas := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesBPe(Configuracoes).Arquivos.NormatizarMunicipios := cbxNormatizarMunicipios.Checked;
    //TConfiguracoesBPe(Configuracoes).Arquivos.PathArquivoMunicipios := PathMunIBGE;

    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'BPe';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoesBPe(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe ;

  end
  else if Configuracoes is TConfiguracoesNFSe then
  begin
    // Italo
    TConfiguracoesNFSe(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoNFe.ItemIndex + 1));
    TConfiguracoesNFSe(Configuracoes).Geral.ConsultaLoteAposEnvio := cbxConsultarLoteAposEnvio.Checked;
    TConfiguracoesNFSe(Configuracoes).Geral.ConsultaAposCancelar := cbxConsultarAposCancelar.Checked;
    TConfiguracoesNFSe(Configuracoes).Geral.MontarPathSchema := cbxMontarPathSchemas.Checked;
    TConfiguracoesNFSe(Configuracoes).Geral.CNPJPrefeitura := edtCNPJPrefeitura.Text;

    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.CNPJ := edtCNPJEmitenteNFSe.Text;
    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.InscMun := edtIMEmitenteNFSe.Text;
    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.RazSocial := edtNomeEmitenteNFSe.Text;
    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.WSUser := edtUsuarioNFSe.Text;
    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.WSSenha := edtSenhaNFSe.Text;
    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.WSChaveAcesso := edtChaveAcessoNFSe.Text;
    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.WSChaveAutoriz := edtChaveAutenticacaoNFSe.Text;
    TConfiguracoesNFSe(Configuracoes).Geral.Emitente.WSFraseSecr := edtFraseSecretaNFSe.Text;

    with TConfiguracoesNFSe(Configuracoes).Arquivos do
    begin
      NomeLongoNFSe := True;
      EmissaoPathNFSe := cbxEmissaoPathNFe.Checked;
      PathNFSe := edtPathNFe.Text;
      PathGer := edtPathNFe.Text;
      PathSalvar := edtPathLogs.Text;

      PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'NFSe';
      if DirectoryExists(PathSchemaDFe) then
        PathSchemas := PathSchemaDFe;
    end;

    // As duas configurações abaixo devem sempre ser as ultimas.
    TConfiguracoesNFSe(Configuracoes).Geral.LayoutNFSe := TLayoutNFSe(cbLayoutNFSe.ItemIndex);
    TConfiguracoesNFSe(Configuracoes).Geral.CodigoMunicipio := StrToIntDef(edtCodigoCidade.Text, -1);
  end
  else if Configuracoes is TConfiguracoesGNRE then
  begin
    TConfiguracoesGNRE(Configuracoes).Geral.FormaEmissao := StrToTpEmis(OK, IntToStr(cbFormaEmissaoGNRe.ItemIndex + 1));
    TConfiguracoesGNRE(Configuracoes).Geral.VersaoDF     := StrToVersaoGNRe(ok, cbVersaoWSGNRE.Text);

    TConfiguracoesGNRE(Configuracoes).Arquivos.IniServicos     := edtArquivoWebServicesGNRe.Text;
    TConfiguracoesGNRE(Configuracoes).Arquivos.EmissaoPathGNRE := cbxEmissaoPathNFe.Checked;
    TConfiguracoesGNRE(Configuracoes).Arquivos.PathGNRE        := edtPathNFe.Text;
    TConfiguracoesGNRE(Configuracoes).Arquivos.SalvarApenasGNREProcessadas := cbxSalvarNFesProcessadas.Checked;
    TConfiguracoesGNRE(Configuracoes).Arquivos.PathArqTXT      := edtPathArqTXT.Text;

    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'GNRe';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoesGNRE(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe ;
  end
  else if Configuracoes is TConfiguracoeseSocial then
  begin
    TConfiguracoeseSocial(Configuracoes).Arquivos.IniServicos       := edtArquivoWebServiceseSocial.Text;
    TConfiguracoeseSocial(Configuracoes).Arquivos.PatheSocial       := edtPathNFe.Text;
    TConfiguracoeseSocial(Configuracoes).Arquivos.EmissaoPatheSocial:= cbxEmissaoPathNFe.Checked;
    TConfiguracoeseSocial(Configuracoes).Geral.VersaoDF             := StrToVersaoeSocialEX(cbVersaoWSeSocial.Text);
    TConfiguracoeseSocial(Configuracoes).Geral.TipoEmpregador       := TEmpregador(cbTipoEmpregador.ItemIndex);
    TConfiguracoeseSocial(Configuracoes).Geral.IdEmpregador         := edtIDEmpregador.Text;
    TConfiguracoeseSocial(Configuracoes).Geral.IdTransmissor        := edtIDTransmissor.Text;

    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'eSocial';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoeseSocial(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe ;

  end
  else if Configuracoes is TConfiguracoesReinf then
  begin
    TConfiguracoesReinf(Configuracoes).Arquivos.IniServicos       := edtArquivoWebServicesReinf.Text;
    TConfiguracoesReinf(Configuracoes).Arquivos.PathReinf         := edtPathNFe.Text;
    TConfiguracoesReinf(Configuracoes).Arquivos.EmissaoPathReinf  := cbxEmissaoPathNFe.Checked;
    TConfiguracoesReinf(Configuracoes).Geral.VersaoDF             := StrToVersaoReinf(ok, cbVersaoWSReinf.Text);
    TConfiguracoesReinf(Configuracoes).Geral.TipoContribuinte     := TContribuinte(cbTipoContribuinte.ItemIndex);
    TConfiguracoesReinf(Configuracoes).Geral.IdContribuinte       := edtIDContribuinte.Text;
    TConfiguracoesReinf(Configuracoes).Geral.IdTransmissor        := edtIDTransmissorReinf.Text;

    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'Reinf';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoesReinf(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe;
  end
  else if Configuracoes is TConfiguracoesGTIN then
  begin
    PathSchemaDFe := edtPathSchemasDFe.Text + PathDelim + 'GTIN';
    if DirectoryExists(PathSchemaDFe) then
      TConfiguracoesReinf(Configuracoes).Arquivos.PathSchemas:= PathSchemaDFe;
  end;

end;

procedure TFrmACBrMonitor.AtualizaSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer( ACBrNFe1.Configuracoes.Geral.SSLLib );
  imgErrSSLLib.Visible := (cbSSLLib.ItemIndex < 1);

  cbCryptLib.ItemIndex   := Integer( ACBrNFe1.Configuracoes.Geral.SSLCryptLib );
  imgErrCryptLib.Visible := (cbCryptLib.ItemIndex < 1);

  cbHttpLib.ItemIndex    := Integer( ACBrNFe1.Configuracoes.Geral.SSLHttpLib );
  imgErrHttpLib.Visible := (cbHttpLib.ItemIndex < 1);

  cbXmlSignLib.ItemIndex := Integer( ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib );
  imgErrXmlSignLib.Visible := (cbXmlSignLib.ItemIndex < 1);

  cbSSLType.Enabled := (ACBrNFe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);

  ValidarConfigCertificado;
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
    if (ACBrNFe1.Configuracoes.Geral.ModeloDF = moNFe) and (Modelo <> 65) then
      ACBrNFe1.Integrador := nil
    else
      ACBrNFe1.Integrador := ACBrIntegrador1;
  end;

end;

function TFrmACBrMonitor.RespostaIntegrador(): String;
begin
   Result := '';
   if (ACBrSAT1.Integrador= ACBrIntegrador1) or
     (ACBrNFe1.Integrador= ACBrIntegrador1) then
   begin
     if (ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo <> '') then
     begin
       Result := sLineBreak+'[Integrador]'+sLineBreak;
       Result := Result + 'Codigo='+ ACBrIntegrador1.ComandoIntegrador.
                                     IntegradorResposta.Codigo + sLineBreak;
       Result := Result + 'Valor='+ ACBrIntegrador1.ComandoIntegrador.
                                   IntegradorResposta.Valor ;

       ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo:= '';
       ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Valor:= '';

     end;

   end;
end;

function TFrmACBrMonitor.SubstituirVariaveis(const ATexto: String): String;
var
  TextoStr: String;
begin
  if Trim(ATexto) = '' then
    Result := ''
  else
  begin
    TextoStr := ATexto;

    if ACBrNFe1.NotasFiscais.Count > 0 then
    begin
      with ACBrNFe1.NotasFiscais.Items[0].NFe do
      begin
        TextoStr := StringReplace(TextoStr,'[EmitNome]',     Emit.xNome,   [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[EmitFantasia]', Emit.xFant,   [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[EmitCNPJCPF]',  Emit.CNPJCPF, [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[EmitIE]',       Emit.IE,      [rfReplaceAll, rfIgnoreCase]);

        TextoStr := StringReplace(TextoStr,'[DestNome]',     Dest.xNome,   [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[DestCNPJCPF]',  Dest.CNPJCPF, [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[DestIE]',       Dest.IE,      [rfReplaceAll, rfIgnoreCase]);

        TextoStr := StringReplace(TextoStr,'[ChaveNFe]',     procNFe.chNFe, [rfReplaceAll, rfIgnoreCase]);

        TextoStr := StringReplace(TextoStr,'[SerieNF]',      FormatFloat('000',           Ide.serie),         [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[NumeroNF]',     FormatFloat('000000000',     Ide.nNF),           [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[ValorNF]',      FormatFloat('0.00',          Total.ICMSTot.vNF), [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[dtEmissao]',    FormatDateTime('dd/mm/yyyy', Ide.dEmi),          [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[dtSaida]',      FormatDateTime('dd/mm/yyyy', Ide.dSaiEnt),       [rfReplaceAll, rfIgnoreCase]);
        TextoStr := StringReplace(TextoStr,'[hrSaida]',      FormatDateTime('hh:mm:ss',   Ide.hSaiEnt),       [rfReplaceAll, rfIgnoreCase]);
      end;
    end;
    Result := TextoStr;
  end;
end;

procedure TFrmACBrMonitor.OnFormataDecimalSAT;
begin
  spedtSATCasasDecimaisQtd.Enabled:= cbFormatoDecimais.ItemIndex = 0;
  spedtSATDecimaisVUnit.Enabled:= cbFormatoDecimais.ItemIndex = 0;
  edtSATCasasMaskQtd.Enabled:= cbFormatoDecimais.ItemIndex = 1;
  edtSATMaskVUnit.Enabled:= cbFormatoDecimais.ItemIndex = 1;
end;

procedure TFrmACBrMonitor.OnMensagemCanhotoNFe;
begin
  lblMsgCanhoto.Enabled:= cbxExibeResumo.Checked;
  edtMsgResumoCanhoto.Enabled:= cbxExibeResumo.Checked;

end;

procedure TFrmACBrMonitor.OnSATManual;
var
  AVisible : Boolean;
begin
  AVisible := (cbSATMarca.ItemIndex <= 0);
  edNomeDLL.Visible := AVisible;
  LabelNomedll.Visible := AVisible;
  sbNomeDLL.Visible := AVisible;
  sePagCod.Visible := AVisible;
  LabelpagCod.Visible := AVisible;
  cbxModeloSAT.Visible := AVisible;
  labelModeloDll.Visible := AVisible;

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
      ACBreSocial1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrReinf1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrBPe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrGTIN1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
      ACBrNFSeX1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
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
      ACBreSocial1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrReinf1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrBPe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrGTIN1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
      ACBrNFSeX1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
    end;
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TFrmACBrMonitor.cbTraduzirTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.TraduzirTags := cbTraduzirTags.Checked;
end;

procedure TFrmACBrMonitor.cbUFChange(Sender: TObject);
begin
  ValidarConfigWebService;
end;

procedure TFrmACBrMonitor.cbIgnorarTagsChange(Sender: TObject);
begin
  ACBrPosPrinter1.IgnorarTags := cbIgnorarTags.Checked;
end;

procedure TFrmACBrMonitor.cbLogChange(Sender: TObject);
begin
  lblogArquivo.Enabled := cbLog.Checked;
  lbLogMaxLinhas.Enabled := cbLog.Checked;
  edLogArq.Enabled := cbLog.Checked;
  sbLog.Enabled := cbLog.Checked;
  sedLogLinhas.Enabled := cbLog.Checked;
  chkMostraLogNaTela.Enabled := cbLog.Checked;

  if cbLog.Checked and (edLogArq.Text = '') then
    edLogArq.Text := 'LOG.TXT';
end;

procedure TFrmACBrMonitor.cbHRIChange(Sender: TObject);
begin
  ACBrPosPrinter1.ConfigBarras.MostrarCodigo := cbHRI.Checked;
end;

procedure TFrmACBrMonitor.cbMonitorarPastaChange(Sender: TObject);
begin
  if cbMonitorarPasta.Checked then
  begin
    if (not DirectoryExists(edEntTXT.Text)) or (not DirectoryExists(edSaiTXT.Text))  then
    begin
      cbMonitorarPasta.Checked := False;
      MessageDlg('Atenção',
                 'Diretorio para monitorar pasta não encontrado!'
                 + sLineBreak + 'Verifique os campos: Entrada, Saida.',
                 mtWarning, [mbOK], '');
      edEntTXT.SetFocus;
      exit;
    end;

    if MessageDlg(
      'Ao ativar esta opção, TODOS os arquivos do diretório serão lidos e apagados.' + sLineBreak +
      'Deseja realmente continuar?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      cbMonitorarPasta.Checked := False;
  end;
end;

procedure TFrmACBrMonitor.cbMunicipioChange(Sender: TObject);
var
  Tamanho: Integer;
begin
  Tamanho := Length(Trim(cbMunicipio.Text));

  edtNomeCidade.Text := Copy(cbMunicipio.Text, 1, Tamanho - 11);
  edtUFCidade.Text := Copy(cbMunicipio.Text, Tamanho - 1, 2);
  edtCodigoCidade.Text := Copy(cbMunicipio.Text, Tamanho - 9, 7);
end;

procedure TFrmACBrMonitor.cbSATMarcaChange(Sender: TObject);
begin
  OnSATManual;
  ValidarConfigSAT;
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
      ACBreSocial1.Configuracoes.Geral.SSLLib:= TSSLLib(cbSSLLib.ItemIndex);
      ACBrReinf1.Configuracoes.Geral.SSLLib  := TSSLLib(cbSSLLib.ItemIndex);
      ACBrBPe1.Configuracoes.Geral.SSLLib    := TSSLLib(cbSSLLib.ItemIndex);
      ACBrGTIN1.Configuracoes.Geral.SSLLib   := TSSLLib(cbSSLLib.ItemIndex);
      ACBrNFSeX1.Configuracoes.Geral.SSLLib  := TSSLLib(cbSSLLib.ItemIndex);
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

function TFrmACBrMonitor.ValidaArquivo(APath: String; AArquivoDefault: String): String;
var
  ErroStr: String;
begin
  ErroStr := '';
  Result := Trim(APath);

  if not FileExists(Result) then
  begin
    Result := ApplicationPath + AArquivoDefault;
    if not FileExists(Result) then
      ErroStr := 'ATENÇÃO: Arquivo ' + APath + ' não encontrado!!!';
  end
  else if VerificaArquivoDesatualizado(Result) then
  begin
    ErroStr := 'ATENÇÃO: Arquivo ' + ExtractFileName(Result)
              + ' disponível em: ' + ExtractFileDir(Result) + ' está desatualizado!!!';
    Result := ApplicationPath + ExtractFileName(Result);
  end;

  if (ErroStr <> '') then
    AddLinesLog( ErroStr );

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

procedure TFrmACBrMonitor.SetFontLabels(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
  begin
    {$IFDEF MSWINDOWS}
    if Components[I] is TLabel  then
    begin
      TLabel( Components[I] ).Font.Name:= 'Segoe UI Light';
      TLabel( Components[I] ).Font.Size:= 9;
      TLabel( Components[I] ).Font.Quality:= fqDraft;
    end;

    if Components[I] is TCheckBox  then
    begin
      TCheckBox( Components[I] ).Font.Name:= 'Segoe UI Light';
      TCheckBox( Components[I] ).Font.Size:= 9;
      TCheckBox( Components[I] ).Font.Quality:= fqDraft;
    end;

    if Components[I] is TGroupBox  then
    begin
      TGroupBox( Components[I] ).Font.Name:= 'Segoe UI Light';
      TGroupBox( Components[I] ).Font.Size:= 10;
      TGroupBox( Components[I] ).Font.Style:=  [] + [fsItalic];
    end;

    if Components[I] is TBitBtn  then
    begin
      TGroupBox( Components[I] ).Font.Name:= 'Segoe UI Light';
      TGroupBox( Components[I] ).Font.Size:= 9;
      TGroupBox( Components[I] ).Font.Style:=  [] + [fsItalic];
    end;

    {$ELSE}
    if Components[I] is TLabel  then
    begin
      TLabel( Components[I] ).Font.Name:= 'Open Sans Light';
      TLabel( Components[I] ).Font.Size:= 9;
      TLabel( Components[I] ).Font.Style:=  [] + [fsItalic];
    end;

    if Components[I] is TCheckBox  then
    begin
      TCheckBox( Components[I] ).Font.Name:= 'Open Sans Light';
      TCheckBox( Components[I] ).Font.Size:= 9;
      TCheckBox( Components[I] ).Font.Style:=  [] + [fsItalic];
    end;

    if Components[I] is TGroupBox  then
    begin
      TGroupBox( Components[I] ).Font.Name:= 'Open Sans Light';
      TGroupBox( Components[I] ).Font.Size:= 10;
      TGroupBox( Components[I] ).Font.Style:=  [] + [fsItalic];
    end;

    if Components[I] is TBitBtn  then
    begin
      TGroupBox( Components[I] ).Font.Name:= 'Open Sans Light';
      TGroupBox( Components[I] ).Font.Size:= 9;
      TGroupBox( Components[I] ).Font.Style:=  [] + [fsItalic];
    end;

    {$IFEND}

  end;

  bConfig.Font.Style:= [] + [fsBold];
  btMinimizar.Font.Style:= [] + [fsBold];
  LCaption.Font.Size:= 16;

end;

procedure TFrmACBrMonitor.ConfigPainelMenu(Edicao: Boolean);
var
  i, posMin, posMax: integer;
begin
  i:= 0;
  posMin:= 65;
  posMax:= 212;

  if Edicao then
  begin
    PanelStart.Visible:= False;
    ImageACBr.Visible:= True;
    pnlPesquisa.Visible:= True;
    TreeViewMenu.Visible:= True;
    TreeFilterEdit1.SetFocus;

    //Gerar Efeito Deslizar para o Painel
    repeat
      i:= i + Trunc(posMin / 3);
      if i >= posMax then
        PanelMenu.Width:= posMax
      else
        PanelMenu.Width:= i;
      PanelMenu.Repaint;
      Sleep(5);

    until (i > posMax);

  end
  else
  begin

    //Gerar Efeito Recolher para o Painel
    i:= posMax;
    repeat
      i:= i - Trunc(posMin);
      if i <= posMin then
        PanelMenu.Width:= posMin
      else
        PanelMenu.Width:= i;
      PanelMenu.Repaint;
      pgConfig.Repaint;
      Sleep(5);

    until (i < posMin);

    ImageACBr.Visible:= False;
    pnlPesquisa.Visible:= False;
    TreeViewMenu.Visible:= False;
    PanelStart.Visible:= True;
    pgConfig.ActivePageIndex := 0;
  end;

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

procedure TFrmACBrMonitor.AtualizarTela(AMonitorConfig: TMonitorConfig);
begin
  if AMonitorConfig = FMonitorConfig then
    LerIni(False);
end;

procedure TFrmACBrMonitor.EfeitoBotaoEnter(ASender, AIco: TImage);
begin
  ASender.Left := ASender.Left -3 ;
  ASender.Top:= ASender.Top -3 ;
  AIco.Visible:= True;
end;

procedure TFrmACBrMonitor.EfeitoBotaoLeave(ASender, AIco: TImage);
begin
  AIco.Visible:= False;
  ASender.Left := ASender.Left +3 ;
  ASender.Top:= ASender.Top +3 ;
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
  function LocalMonitoramento:String;
  begin
    if rbTCP.Checked then
      Result := 'Monitorando Porta: ' + edPortaTCP.Text
    else
    begin
      if cbMonitorarPasta.Checked then
        Result := 'Monitorando Dir.: ' + ExtractFilePath(ArqEntTXT)
      else
        Result := 'Monitorando Arq.: ' + ExtractFileName(ArqEntTXT);
    end;

  end;

begin
  TrayIcon1.Hint := {$IFDEF Demo} 'DEMO - ' + {$ENDIF} 'ACBrMonitorPLUS ' + sVersaoACBr +
                    sLineBreak + LocalMonitoramento + ' ';
  TrayIcon1.BalloonTitle := TrayIcon1.Hint;
  TrayIcon1.BalloonHint := 'Projeto ACBr' + sLineBreak + 'http://acbr.sf.net';

  FrmACBrMonitor.Caption := {$IFDEF Demo} 'DEMO - ' + {$ENDIF} ' ACBrMonitorPLUS ' + sVersaoACBr + ' ';

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
  ACBreSocial1.SSL.SSLType:= TSSLType( cbSSLType.ItemIndex );
  ACBrReinf1.SSL.SSLType  := TSSLType( cbSSLType.ItemIndex );
  ACBrBPe1.SSL.SSLType    := TSSLType( cbSSLType.ItemIndex );
  ACBrGTIN1.SSL.SSLType   := TSSLType( cbSSLType.ItemIndex );
  ACBrNFSeX1.SSL.SSLType  := TSSLType( cbSSLType.ItemIndex );
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

// Italo
procedure TFrmACBrMonitor.AtualizarCidades;
var
  IniCidades: TMemIniFile;
  Cidades: TStringList;
  I: Integer;
  sNome, sCod, sUF: String;
begin
  IniCidades := TMemIniFile.Create('');
  Cidades    := TStringList.Create;

  ACBrNFSeX1.LerCidades;
  IniCidades.SetStrings(ACBrNFSeX1.Configuracoes.WebServices.Params);

  try
    IniCidades.ReadSections(Cidades);
    cbMunicipio.Items.Clear;

    for I := 0 to Pred(Cidades.Count) do
    begin
      if (StrToIntdef(Cidades[I], 0) > 0) then
      begin
        //Exemplo: Alfenas/3101607/MG
        sCod  := Cidades[I];
        sNome := ACBrStr(IniCidades.ReadString(sCod, 'Nome', ''));
        sUF   := IniCidades.ReadString(sCod, 'UF', '');

        cbMunicipio.Items.Add(Format('%s/%s/%s', [sNome, sCod, sUF]));
      end;
    end;

    //Sort
    cbMunicipio.Sorted := false;
    cbMunicipio.Sorted := true;
//    edtTotalCidades.Text := IntToStr(cbCidades.Items.Count);
  finally
    FreeAndNil(Cidades);
    IniCidades.Free;
  end;
end;

end.
