{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

//{$I ACBr.inc}

unit FrPrincipal;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, Grids, ACBrCEP, ACBrPIXCD, ACBrPIXPSPItau,
  ACBrPIXPSPBancoDoBrasil, ACBrPIXPSPSantander, ACBrPIXBase, ACBrPIXSchemasPix,
  ACBrPIXSchemasDevolucao, ACBrPIXSchemasCob, ACBrPIXPSPShipay, ACBrOpenSSLUtils,
  ACBrSocket, ACBrBase
  {$IfDef FPC}
  , DateTimePicker, ACBrSocket, ACBrBase
  {$EndIf};

const
  CMaxConsultas = 36;
  CURL_ACBR = 'https://projetoacbr.com.br/tef/';
  CURL_MCC = 'https://classification.codes/classifications/industry/mcc/';

type

  TFluxoPagtoDados = record
    TxID: String;
    E2E: String;
    QRCode: String;
    Total: Double;
    StatusCobranca: TACBrPIXStatusCobranca;
    StatusDevolucao: TACBrPIXStatusDevolucao;
    EmErro: Boolean;
    QtdConsultas: Integer;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ACBrCEP1: TACBrCEP;
    ACBrOpenSSLUtils1: TACBrOpenSSLUtils;
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPBancoDoBrasil1: TACBrPSPBancoDoBrasil;
    ACBrPSPItau1: TACBrPSPItau;
    ACBrPSPSantander1: TACBrPSPSantander;
    ACBrPSPShipay1: TACBrPSPShipay;
    btFluxoCopiaECola: TSpeedButton;
    btConsultarCobrancaImediata: TBitBtn;
    btCancelarCobranca: TBitBtn;
    btConsultarCobrancas: TBitBtn;
    btBBSimulaPagamento_Executar: TBitBtn;
    btFluxoCancelarCobranca: TBitBtn;
    btFluxoCancelarConsulta: TBitBtn;
    btFluxoEstornarPagto: TBitBtn;
    btFluxoFecharVenda: TBitBtn;
    btFluxoItemExcluir: TBitBtn;
    btFluxoItemIncluir: TBitBtn;
    btFluxoNovaVenda: TBitBtn;
    btFluxoPagar: TBitBtn;
    btFluxoTentarNovamente: TBitBtn;
    btItauGerarChavePrivada: TBitBtn;
    btItauSolicitarCertificado: TBitBtn;
    btItauValidarChaveCertificado: TBitBtn;
    btLimparConsultarCobrancaImediata: TBitBtn;
    btCancelarCobrancaLimparMemo: TBitBtn;
    btLimparConsultarCobrancas: TBitBtn;
    btBBSimulaPagamento_Limpar: TBitBtn;
    btLimparCriarCobrancaImediata: TBitBtn;
    btLimparConsultarPix: TBitBtn;
    btLimparConsultarPixRecebidos: TBitBtn;
    btLimparConsultarDevolucaoPix: TBitBtn;
    btLimparSolicitarDevolucaoPix: TBitBtn;
    btQRDGerar: TBitBtn;
    btQREAnalisar: TBitBtn;
    btQREAnalisar2: TBitBtn;
    btQREColar: TBitBtn;
    btQREColar1: TBitBtn;
    btQREGerar: TBitBtn;
    btSolicitarDevolucaoPix: TBitBtn;
    btConsultarPix: TBitBtn;
    btConsultarPixRecebidos: TBitBtn;
    btConsultarDevolucaoPix: TBitBtn;
    btQREAnalisar1: TBitBtn;
    btLerParametros: TBitBtn;
    btSalvarParametros: TBitBtn;
    btCriarCobrancaImediata: TBitBtn;
    cbxAmbiente: TComboBox;
    cbxItauTipoChave: TComboBox;
    cbxNivelLog: TComboBox;
    cbxPSPAtual: TComboBox;
    cbxRecebedorUF: TComboBox;
    cbxSolicitarDevolucaoPix_Natureza: TComboBox;
    cbxSantanderTipoChave: TComboBox;
    cbxBBTipoChave: TComboBox;
    cbxConsultarCobrancas_Status: TComboBox;
    chCriarCobrancaImediata_PermiterAlterarValor: TCheckBox;
    chConsultarCobrancas_ComLocation: TCheckBox;
    dtConsultarCobrancas_Fim: TDateTimePicker;
    dtConsultarPixRecebidosInicio: TDateTimePicker;
    dtConsultarPixRecebidosFim: TDateTimePicker;
    dtConsultarCobrancas_Inicio: TDateTimePicker;
    edFluxoClienteDoc: TEdit;
    edFluxoClienteNome: TEdit;
    edFluxoCopiaECola: TEdit;
    edFluxoItemDescricao: TEdit;
    edFluxoItemEAN: TEdit;
    edFluxoItemValor: TEdit;
    edtArqLog: TEdit;
    edtProxyHost: TEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    edtRecebedorCEP: TEdit;
    edtRecebedorCidade: TEdit;
    edtRecebedorNome: TEdit;
    fleQREValor: TEdit;
    feSolicitarDevolucaoPix_Valor: TEdit;
    edtBBSimulaPagamento_pixCopiaECola: TEdit;
    edCancelarCobrancaTxID: TEdit;
    edtShipayClientID: TEdit;
    edtShipaySecretKey: TEdit;
    edtShipayAccessKey: TEdit;
    edtConsultarDevolucaoPix_e2eid: TEdit;
    edtConsultarCobrancaImediata_TxId: TEdit;
    edtConsultarCobrancas_CPFCNPJ: TEdit;
    edtCriarCobrancaImediata_TxId: TEdit;
    edtCriarCobrancaImediata_CPF_CNPJ: TEdit;
    edtItauArqCertificado: TEdit;
    edtItauArqCertificado2: TEdit;
    edtItauArqChavePrivada: TEdit;
    edtItauArqChavePrivada2: TEdit;
    edtItauChavePIX: TEdit;
    edtItauClientID: TEdit;
    edtItauClientSecret: TEdit;
    edtItauXCorrelationId: TEdit;
    edtQREInfoAdicional: TEdit;
    edtQRDLocation: TEdit;
    edtQRETxId: TEdit;
    edtCriarCobrancaImediata_NomeDevedor: TEdit;
    edtSolicitarDevolucaoPix_e2eid: TEdit;
    edtSolicitarDevolucaoPix_Descricao: TEdit;
    edtConsultarDevolucaoPix_id: TEdit;
    edtSolicitarDevolucaoPix_id: TEdit;
    edtConsultarPixRecebidosTxId: TEdit;
    edtConsultarPixRecebidosCPFCNPJ: TEdit;
    edtCriarCobrancaImediata_SolicitacaoAoPagador: TEdit;
    feCriarCobrancaImediatax_Valor: TEdit;
    gbCobranca: TGroupBox;
    gbFluxoCliente: TGroupBox;
    gbFluxoItens: TGroupBox;
    gbFluxoStatus: TGroupBox;
    gbFluxoTotal: TGroupBox;
    gdFluxoItens: TStringGrid;
    imFluxoQRCode: TImage;
    imgErrCEP: TImage;
    imgErrNome: TImage;
    imgErrPSP: TImage;
    imgInfoMCC: TImage;
    imgItauErroCertificado: TImage;
    imgItauErroChavePIX: TImage;
    imgItauErroChavePrivada: TImage;
    imgItauErroClientID: TImage;
    imgItauErroClientSecret: TImage;
    imgQRCriarCobrancaImediata: TImage;
    imgQRE: TImage;
    imgQRD: TImage;
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
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label42: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbFluxoClienteDoc: TLabel;
    lbFluxoClienteNome: TLabel;
    lbFluxoCopiaECola: TLabel;
    lbFluxoItemDescricao: TLabel;
    lbFluxoItemEAN: TLabel;
    lbFluxoItemValor: TLabel;
    lConsultarPixE2eid1: TLabel;
    lConsultarPixE2eid2: TLabel;
    lbCancelarCobrancaTxID: TLabel;
    lCPFCPNJ2: TLabel;
    lFim1: TLabel;
    lInicio1: TLabel;
    lItauAvisoChaveCertificadoDesabilitado: TLabel;
    Label43: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label44: TLabel;
    Label6: TLabel;
    lConsultarDevolucaoPixE2eid2: TLabel;
    lConsultarDevolucaoPixE2eid3: TLabel;
    lConsultarDevolucaoPixE2eid5: TLabel;
    lConsultarDevolucaoPixIdentificadorDevolucao1: TLabel;
    lConsultarDevolucaoPixIdentificadorDevolucao2: TLabel;
    lConsultarPixE2eid: TLabel;
    edtBBClientID: TEdit;
    edtConsultarPixE2eid: TEdit;
    edtSantanderChavePIX: TEdit;
    edtBBClientSecret: TEdit;
    edtSantanderConsumerKey: TEdit;
    edtBBDevAppKey: TEdit;
    edtSantanderConsumerSecret: TEdit;
    edtBBChavePIX: TEdit;
    imgBBErroChavePIX: TImage;
    imgSantanderErroChavePIX: TImage;
    lCPFCPNJ1: TLabel;
    lE2eid: TLabel;
    lConsultarDevolucaoPixE2eid1: TLabel;
    lConsultarDevolucaoPixIdentificadorDevolucao: TLabel;
    lE2eid1: TLabel;
    lInicio: TLabel;
    lFim: TLabel;
    lCPFCPNJ: TLabel;
    lItauErroCertificado: TLabel;
    lItauErroChavePrivada: TLabel;
    lPagina: TLabel;
    lPagina1: TLabel;
    lPagina2: TLabel;
    lPagina3: TLabel;
    lPagina4: TLabel;
    lTokenTemporario: TLabel;
    mConsultarCobrancaImediata: TMemo;
    mmCancelarCobranca: TMemo;
    mConsultarCobrancas: TMemo;
    mBBSimulaPagamento: TMemo;
    mItauCertificadoPEM: TMemo;
    mItauChavePrivadaPEM: TMemo;
    mItauTokenTemporario: TMemo;
    mQRE: TMemo;
    mQRD: TMemo;
    mSolicitarDevolucaoPix: TMemo;
    mConsultarPix: TMemo;
    mConsultarPixRecebidos: TMemo;
    mConsultarDevolucaoPix: TMemo;
    mCriarCobrancaImediata: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    pnLog: TPanel;
    pnProxy: TPanel;
    pnCobranca: TPanel;
    pnPSP: TPanel;
    pnRecebedor: TPanel;
    pnFluxoCliente: TPanel;
    pnCancelarCobrancaRodape: TPanel;
    Panel9: TPanel;
    pConfPSPBB3: TPanel;
    pConsultarCobrancaImediata: TPanel;
    pnCancelarCobranca: TPanel;
    pConsultarCobrancas: TPanel;
    pBBSimulaPagamento: TPanel;
    pgTestesEndPointCob: TPageControl;
    pgQRCode: TPageControl;
    Panel1: TPanel;
    Panel8: TPanel;
    pItauCertificadoRecebido: TPanel;
    pItauTokentemporario: TPanel;
    pItauEditCertificado: TPanel;
    pgPSPItauGerarChaveCertificado: TPageControl;
    Panel7: TPanel;
    pgPSPItauChaveCertificado: TPageControl;
    pConfPSPBB1: TPanel;
    pgPSPItau: TPageControl;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pConsultarDevolucaoPix: TPanel;
    pnFluxoBackground: TPanel;
    pnFluxoBotoes: TPanel;
    pnFluxoBotoesErroConsultar: TPanel;
    pnFluxoBotoesPrincipais: TPanel;
    pnFluxoBotoesRight: TPanel;
    pnFluxoDadosItem: TPanel;
    pnFluxoDiv1: TPanel;
    pnFluxoDiv10: TPanel;
    pnFluxoDiv2: TPanel;
    pnFluxoDiv3: TPanel;
    pnFluxoDiv7: TPanel;
    pnFluxoDiv8: TPanel;
    pnFluxoPagto: TPanel;
    pnFluxoCopiaECola: TPanel;
    pnFluxoQRCode: TPanel;
    pnFluxoRodape: TPanel;
    pnFluxoStatus: TPanel;
    pnFluxoTotal: TPanel;
    pnFluxoTotalStr: TPanel;
    pQREDados: TPanel;
    pQRDDados: TPanel;
    pQREGerado: TPanel;
    pQREGerado1: TPanel;
    pQREMemo: TPanel;
    pQRDMemo: TPanel;
    pSolicitarDevolucaoPix: TPanel;
    pgTestesPix: TPageControl;
    Panel2: TPanel;
    pConsultarPix: TPanel;
    pConsultarPixRecebidos: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    pConfPSPBB: TPanel;
    pBotoesConfiguracao: TPanel;
    pConfPSPBB2: TPanel;
    pCriarCobrancaImediata: TPanel;
    sbArqLog: TSpeedButton;
    sbConsultaCEP: TSpeedButton;
    sbCriarCobrancaImediata_GerarTxId: TSpeedButton;
    sbItauAcharArqCertificado: TSpeedButton;
    sbItauAcharArqChavePrivada: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    seCobrancaExpiracao: TSpinEdit;
    seConsultarCobrancaImediata_Revisao: TSpinEdit;
    seConsultarCobrancas_ItensPagina: TSpinEdit;
    seConsultarCobrancas_Pagina: TSpinEdit;
    gbLog: TGroupBox;
    gbProxy: TGroupBox;
    gbPSP: TGroupBox;
    gbRecebedor: TGroupBox;
    Label2: TLabel;
    lURLTEF: TLabel;
    mLog: TMemo;
    pConfPIX: TPanel;
    pgConfPixPSP: TPageControl;
    pgPSPs: TPageControl;
    pgTestes: TPageControl;
    pgTesteEndPoints: TPageControl;
    pgPrincipal: TPageControl;
    pLogs: TPanel;
    seProxyPorta: TSpinEdit;
    seRecebedorMCC: TSpinEdit;
    seConsultarPixRecebidosPagina: TSpinEdit;
    seConsultarPixRecebidosItensPagina: TSpinEdit;
    seTimeout: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tmConsultarDevolucao: TTimer;
    tsCancelarCobranca: TTabSheet;
    tmConsultarPagto: TTimer;
    tsFluxoPagto: TTabSheet;
    tsBBSimularPagamento: TTabSheet;
    tsBBTestes: TTabSheet;
    tsConsultarCobrancas: TTabSheet;
    tsQRCodeEstatico: TTabSheet;
    tsQRCodeDinamico: TTabSheet;
    tsItauChaveCertificadoArquivos: TTabSheet;
    tsItauGerarChaveCertificado: TTabSheet;
    tsItauCertPasso1: TTabSheet;
    tsItauCertPasso3: TTabSheet;
    tsItauChave: TTabSheet;
    tsItauCertificado: TTabSheet;
    tsCriarCobrancaImediata: TTabSheet;
    tsConsultarCobrancaImediata: TTabSheet;
    tsSolicitarDevolucaoPix: TTabSheet;
    tsConsultarDevolucaoPix: TTabSheet;
    tsConsultarPix: TTabSheet;
    tsConsultarPixRecebidos: TTabSheet;
    tsEndPointPix: TTabSheet;
    tsEndPointCob: TTabSheet;
    tsEndPointCobV: TTabSheet;
    tsEndPoints: TTabSheet;
    tsQRCode: TTabSheet;
    tsShipay: TTabSheet;
    tsBB: TTabSheet;
    tsItau: TTabSheet;
    tsSantander: TTabSheet;
    tsPSP: TTabSheet;
    tsPIX: TTabSheet;
    tsTestes: TTabSheet;
    tsConfiguracao: TTabSheet;
    Valor: TLabel;
    procedure ACBrPixCD1QuandoGravarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure btBBSimulaPagamento_ExecutarClick(Sender: TObject);
    procedure btBBSimulaPagamento_LimparClick(Sender: TObject);
    procedure btCancelarCobrancaClick(Sender: TObject);
    procedure btCancelarCobrancaLimparMemoClick(Sender: TObject);
    procedure btFluxoCopiaEColaClick(Sender: TObject);
    procedure btConsultarCobrancaImediataClick(Sender: TObject);
    procedure btConsultarCobrancasClick(Sender: TObject);
    procedure btConsultarPixRecebidosClick(Sender: TObject);
    procedure btConsultarPixClick(Sender: TObject);
    procedure btConsultarDevolucaoPixClick(Sender: TObject);
    procedure btCriarCobrancaImediataClick(Sender: TObject);
    procedure btFluxoCancelarCobrancaClick(Sender: TObject);
    procedure btFluxoEstornarPagtoClick(Sender: TObject);
    procedure btFluxoItemExcluirClick(Sender: TObject);
    procedure btFluxoItemIncluirClick(Sender: TObject);
    procedure btFluxoNovaVendaClick(Sender: TObject);
    procedure btFluxoPagarClick(Sender: TObject);
    procedure btItauGerarChavePrivadaClick(Sender: TObject);
    procedure btItauSolicitarCertificadoClick(Sender: TObject);
    procedure btItauValidarChaveCertificadoClick(Sender: TObject);
    procedure btLimparConsultarCobrancaImediataClick(Sender: TObject);
    procedure btLimparConsultarCobrancasClick(Sender: TObject);
    procedure btLimparConsultarDevolucaoPixClick(Sender: TObject);
    procedure btLimparConsultarPixClick(Sender: TObject);
    procedure btLimparConsultarPixRecebidosClick(Sender: TObject);
    procedure btLimparCriarCobrancaImediataClick(Sender: TObject);
    procedure btLimparSolicitarDevolucaoPixClick(Sender: TObject);
    procedure btQRDGerarClick(Sender: TObject);
    procedure btQREAnalisar1Click(Sender: TObject);
    procedure btQREAnalisarClick(Sender: TObject);
    procedure btQREGerarClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btQREColarClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btSolicitarDevolucaoPixClick(Sender: TObject);
    procedure cbxAmbienteChange(Sender: TObject);
    procedure cbxPSPAtualChange(Sender: TObject);
    procedure edtBBChavePIXChange(Sender: TObject);
    procedure edtCriarCobrancaImediata_CPF_CNPJChange(Sender: TObject);
    procedure edtCriarCobrancaImediata_NomeDevedorChange(Sender: TObject);
    procedure edtRecebedorCEPChange(Sender: TObject);
    procedure edtRecebedorCEPExit(Sender: TObject);
    procedure edOnlyNumbersKeyPress(Sender: TObject; var Key: char);
    procedure edtConsultarPixRecebidosCPFCNPJChange(Sender: TObject);
    procedure edtItauArqChavePrivadaChange(Sender: TObject);
    procedure edtItauChavePIXChange(Sender: TObject);
    procedure edtItauClientIDChange(Sender: TObject);
    procedure edtItauClientSecretChange(Sender: TObject);
    procedure edtRecebedorNomeChange(Sender: TObject);
    procedure edtSantanderChavePIXChange(Sender: TObject);
    procedure mQREChange(Sender: TObject);
    procedure pgPrincipalChange(Sender: TObject);
    procedure pgPSPItauChaveCertificadoChange(Sender: TObject);
    procedure QuandoMudarDadosQRCode(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgInfoMCCClick(Sender: TObject);
    procedure lURLTEFClick(Sender: TObject);
    procedure sbArqLogClick(Sender: TObject);
    procedure sbConsultaCEPClick(Sender: TObject);
    procedure sbCriarCobrancaImediata_GerarTxIdClick(Sender: TObject);
    procedure sbItauAcharArqCertificadoClick(Sender: TObject);
    procedure sbItauAcharArqChavePrivadaClick(Sender: TObject);
    procedure sbVerSenhaProxyClick(Sender: TObject);
    procedure tmConsultarDevolucaoTimer(Sender: TObject);
    procedure tmConsultarPagtoTimer(Sender: TObject);
  private
    fFluxoDados: TFluxoPagtoDados;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;

    procedure InicializarBitmaps;
    procedure InicializarActivePages;
    procedure InicializarComponentesDefault;

    function GetNomeArquivoConfiguracao: String;
    procedure AdicionarLinhaLog(AMensagem: String);
    procedure TratarException(Sender: TObject; E: Exception);

    procedure LigarAlertasdeErrosDeConfiguracao;
    procedure LigarAlertasdeErrosDeConfiguracaoPIXCD;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPItau;

    procedure VerificarConfiguracao;
    procedure VerificarConfiguracaoPIXCD;
    procedure VerificarConfiguracaoPSPItau;
    procedure ValidarChaveCertificadoPSPItau;
    procedure ValidarChavePSPItau;
    procedure ValidarCertificadoPSPItau;

    procedure ConfigurarACBrPIXCD;
    procedure ConfigurarACBrPSPs;

    procedure LimparQRCodeEstatico;
    procedure PintarQRCodeEstatico;
    procedure PintarQRCodeDinamico;

    procedure MostrarPixEmLinhas(const NomePix: String; APix: TACBrPIX; SL: TStrings);
    procedure MostrarDevolucaoEmLinhas(const NomeDev: String;
      ADev: TACBrPIXDevolucao; SL: TStrings);
    procedure MostrarCobrancaEmLinhas(const NomeCobranca: String;
      ACob: TACBrPIXCobGerada; SL: TStrings);

    function FormatarJSON(const AJSON: String): String;
    function RemoverPathAplicacao(const AFileName: String): String;
    function AdicionarPathAplicacao(const AFileName: String): String;

    procedure ReiniciarFluxo;
    procedure ConsultarCobranca;
    procedure ConsultarDevolucao;
    procedure EstornarPagamento;

    procedure AvaliarInterfaceFluxo;
    procedure AvaliarInterfaceFluxoItem;
    procedure LimparInterfaceFluxoItem;
    procedure HabilitarInterface(aLiberada: Boolean);

    procedure AtualizarTotal;
    procedure AtualizarStatus(aStatus: TACBrPIXStatusCobranca = stcNENHUM;
      aStatusDevolucao: TACBrPIXStatusDevolucao = stdNENHUM);

    procedure InicializarGridFluxo;
    procedure ExcluirItemGrid(aGrid: TStringGrid; aIndex: Integer);
    procedure AdicionarItemGridFluxo(aEan, aDescricao: String; aValor: Double);

  public
    property FluxoDados: TFluxoPagtoDados read fFluxoDados;
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;

  end;

var
  Form1: TForm1;

implementation

uses
  {$IfDef FPC}
   fpjson, jsonparser, jsonscanner, Jsons,
  {$EndIf}
  TypInfo, Clipbrd, IniFiles, DateUtils, synacode, synautil, pcnConversao,
  ACBrDelphiZXingQRCode, ACBrImage, ACBrValidador, ACBrPIXUtil, ACBrPIXBRCode,
  ACBrUtil.FilesIO, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InicializarBitmaps;
  InicializarActivePages;
  InicializarComponentesDefault;
  {$IfDef FPC}
  Application.OnException := @TratarException;
  {$Else}
  Application.OnException := TratarException;
  {$EndIf};

  LerConfiguracao;
  ReiniciarFluxo;
end;

procedure TForm1.imgInfoMCCClick(Sender: TObject);
begin
  OpenURL(CURL_MCC);
end;

procedure TForm1.lURLTEFClick(Sender: TObject);
begin
  OpenURL(CURL_ACBR);
end;

procedure TForm1.sbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if (Trim(edtArqLog.Text) = '') then
  begin
    MessageDlg('Arquivo de Log não informado', mtError, [mbOK], 0);
    Exit;
  end;

  if pos(PathDelim,edtArqLog.Text) = 0 then
    AFileLog := ApplicationPath + edtArqLog.Text
  else
    AFileLog := edtArqLog.Text;

  if not FileExists(AFileLog) then
    MessageDlg('Arquivo '+AFileLog+' não encontrado', mtError, [mbOK], 0)
  else
    OpenURL(AFileLog);
end;

procedure TForm1.sbConsultaCEPClick(Sender: TObject);
var
  EndAchado: TACBrCEPEndereco;
begin
  try
    ACBrCEP1.BuscarPorCEP(OnlyNumber(edtRecebedorCEP.Text));
    if (ACBrCEP1.Enderecos.Count > 0) then
    begin
      EndAchado := ACBrCEP1.Enderecos[0];
      edtRecebedorCidade.Text := EndAchado.Municipio;
      cbxRecebedorUF.ItemIndex := cbxRecebedorUF.Items.IndexOf(EndAchado.UF);
    end;
  except
    MessageDlg('Erro ao executar Consulta do CEP', mtError, [mbOK], 0);
  end;
end;

procedure TForm1.sbCriarCobrancaImediata_GerarTxIdClick(Sender: TObject);
begin
  edtCriarCobrancaImediata_TxId.Text := CriarTxId;
end;

procedure TForm1.sbItauAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edtItauArqCertificado.Text;
  if OpenDialog1.Execute then
    edtItauArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPItau;
end;

procedure TForm1.sbItauAcharArqChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edtItauArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edtItauArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPItau;
end;

procedure TForm1.sbVerSenhaProxyClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbVerSenhaProxy.Down then
    edtProxySenha.EchoMode := emNormal
  else
    edtProxySenha.EchoMode := emPassword;
  {$EndIf}
end;

procedure TForm1.tmConsultarDevolucaoTimer(Sender: TObject);
begin
  tmConsultarDevolucao.Enabled := False;
  try
    if EstaVazio(FluxoDados.E2E) then
    begin
      ShowMessage('Nenhum pagamento a ser consultado (E2E)');
      Exit;
    end;

    ConsultarDevolucao;
  finally
    if (FluxoDados.StatusDevolucao = stdEM_PROCESSAMENTO) then
      tmConsultarDevolucao.Enabled := True;
  end;
end;

procedure TForm1.tmConsultarPagtoTimer(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;
  try
    if EstaVazio(FluxoDados.TxID) then
    begin
      ShowMessage('Nenhuma cobrança a ser consultada');
      Exit;
    end;

    ConsultarCobranca;
    fFluxoDados.QtdConsultas := fFluxoDados.QtdConsultas + 1;
  finally
    if (FluxoDados.StatusCobranca = stcATIVA) and
       (not fFluxoDados.EmErro) and
       (fFluxoDados.QtdConsultas <= CMaxConsultas) then
      tmConsultarPagto.Enabled := True;
  end;
end;

function TForm1.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName,'.ini');
end;

procedure TForm1.edtRecebedorCEPChange(Sender: TObject);
begin
  if (Length(edtRecebedorCEP.Text) > 5) then
  begin
    edtRecebedorCEP.Text := FormatarMascaraDinamica(OnlyNumber(edtRecebedorCEP.Text), '*****-***');
    edtRecebedorCEP.SelStart := Length(edtRecebedorCEP.Text);
  end;

  imgErrCEP.Visible := (Length(edtRecebedorCEP.Text) < 9);
  sbConsultaCEP.Visible := not imgErrCEP.Visible;
end;

procedure TForm1.ACBrPixCD1QuandoGravarLog(const ALogLine: String; var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TForm1.btBBSimulaPagamento_ExecutarClick(Sender: TObject);
var
  code: Integer;
  texto: String;
begin
  VerificarConfiguracao;
  mBBSimulaPagamento.Lines.Clear;
  if not (ACBrPixCD1.PSP is TACBrPSPBancoDoBrasil) then
    raise Exception.Create('PSP Configurado, não é Banco do Brasil');

  if (ACBrPixCD1.Ambiente <> ambTeste) then
    raise Exception.Create('Função só disponível em ambiente de Testes');

  try
    code := 0;
    texto := '';
    ACBrPSPBancoDoBrasil1.SimularPagamentoPIX(edtBBSimulaPagamento_pixCopiaECola.Text, code, texto);
    mBBSimulaPagamento.Lines.Add('Result Code: '+IntToStr(ACBrPSPBancoDoBrasil1.Http.ResultCode));
    mBBSimulaPagamento.Lines.Add('');
    mBBSimulaPagamento.Lines.Add(texto);
  except
    On E: Exception do
      mBBSimulaPagamento.Lines.Add(E.Message);
  end;
end;

procedure TForm1.btBBSimulaPagamento_LimparClick(Sender: TObject);
begin
  mBBSimulaPagamento.Lines.Clear;
end;

procedure TForm1.btCancelarCobrancaClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmCancelarCobranca.Lines.Clear;

  with ACBrPixCD1.PSP.epCob do
  begin
    CobRevisada.status := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;

    if RevisarCobrancaImediata(edCancelarCobrancaTxID.Text) then
    begin
      mmCancelarCobranca.Lines.Text := FormatarJSON(CobGerada.AsJSON);
      MostrarCobrancaEmLinhas('  Cobrança', CobGerada, mmCancelarCobranca.Lines);
    end
    else
      mmCancelarCobranca.Lines.Text := FormatarJSON(Problema.AsJSON);
  end;
end;

procedure TForm1.btCancelarCobrancaLimparMemoClick(Sender: TObject);
begin
  mmCancelarCobranca.Lines.Clear;
end;

procedure TForm1.btFluxoCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edFluxoCopiaECola.Text);
end;

procedure TForm1.btConsultarCobrancaImediataClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarCobrancaImediata.Lines.Clear;
  if ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata( edtConsultarCobrancaImediata_TxId.Text,
                                                     seConsultarCobrancaImediata_Revisao.Value) then
  begin
    mConsultarCobrancaImediata.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.CobCompleta.AsJSON);
    MostrarCobrancaEmLinhas( '  Cobranca',
                             ACBrPixCD1.PSP.epCob.CobCompleta,
                             mConsultarCobrancaImediata.Lines );
  end
  else
    mConsultarCobrancaImediata.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON);
end;

procedure TForm1.btConsultarCobrancasClick(Sender: TObject);
var
  Ok: Boolean;
  i: Integer;
begin
  VerificarConfiguracao;
  mConsultarCobrancas.Lines.Clear;
  Ok := ACBrPixCD1.PSP.epCob.ConsultarCobrancas( dtConsultarCobrancas_Inicio.DateTime,
                                                 dtConsultarCobrancas_Fim.DateTime,
                                                 OnlyNumber(edtConsultarCobrancas_CPFCNPJ.Text),
                                                 chConsultarCobrancas_ComLocation.Checked,
                                                 TACBrPIXStatusCobranca(cbxConsultarCobrancas_Status.ItemIndex),
                                                 seConsultarCobrancas_Pagina.Value,
                                                 seConsultarCobrancas_ItensPagina.Value);
  if Ok then
  begin
    mConsultarCobrancas.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.CobsConsultadas.AsJSON);
    mConsultarCobrancas.Lines.Add('');
    mConsultarCobrancas.Lines.Add('Encontrado: '+IntToStr(ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs.Count)+', Cobranças');
    for i := 0 to ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs.Count-1 do
    begin
      mConsultarCobrancas.Lines.Add('');
      MostrarCobrancaEmLinhas( '  Cob['+IntToStr(i)+']',
                               ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs[i],
                               mConsultarCobrancas.Lines );
    end;
  end
  else
    mConsultarCobrancas.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TForm1.btConsultarPixRecebidosClick(Sender: TObject);
var
  Ok: Boolean;
  i: Integer;
begin
  VerificarConfiguracao;
  mConsultarPixRecebidos.Lines.Clear;
  Ok := ACBrPixCD1.PSP.epPix.ConsultarPixRecebidos( dtConsultarPixRecebidosInicio.DateTime,
                                                    dtConsultarPixRecebidosFim.DateTime,
                                                    edtConsultarPixRecebidosTxId.Text,
                                                    OnlyNumber(edtConsultarPixRecebidosCPFCNPJ.Text),
                                                    seConsultarPixRecebidosPagina.Value,
                                                    seConsultarPixRecebidosItensPagina.Value);
  if Ok then
  begin
    mConsultarPixRecebidos.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.PixConsultados.AsJSON);
    mConsultarPixRecebidos.Lines.Add('');
    mConsultarPixRecebidos.Lines.Add('Encontrado: '+IntToStr(ACBrPixCD1.PSP.epPix.PixConsultados.pix.Count)+', documentos PIX');
    for i := 0 to ACBrPixCD1.PSP.epPix.PixConsultados.pix.Count-1 do
    begin
      mConsultarPixRecebidos.Lines.Add('');
      MostrarPixEmLinhas( '  Pix['+IntToStr(i)+']',
                          ACBrPixCD1.PSP.epPix.PixConsultados.pix[i],
                          mConsultarPixRecebidos.Lines );
    end;
  end
  else
    mConsultarPixRecebidos.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TForm1.btConsultarPixClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarPix.Lines.Clear;
  if ACBrPixCD1.PSP.epPix.ConsultarPix(edtConsultarPixE2eid.Text) then
  begin
    mConsultarPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Pix.AsJSON);
    MostrarPixEmLinhas( '  Pix',
                        ACBrPixCD1.PSP.epPix.Pix,
                        mConsultarPix.Lines );
  end
  else
    mConsultarPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TForm1.btConsultarDevolucaoPixClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarDevolucaoPix.Lines.Clear;
  if ACBrPixCD1.PSP.epPix.ConsultarDevolucaoPix( edtConsultarDevolucaoPix_e2eid.Text,
                                                 edtConsultarDevolucaoPix_id.Text) then
  begin
    mConsultarDevolucaoPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Devolucao.AsJSON);
    MostrarDevolucaoEmLinhas( '  Devolucao',
                              ACBrPixCD1.PSP.epPix.Devolucao,
                              mConsultarDevolucaoPix.Lines );
  end
  else
    mConsultarDevolucaoPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TForm1.btCriarCobrancaImediataClick(Sender: TObject);
var
  s, qrcode: String;
  Ok: Boolean;
begin
  VerificarConfiguracao;
  mCriarCobrancaImediata.Lines.Clear;

  with ACBrPixCD1.PSP.epCob.CobSolicitada do
  begin
    Clear;
    chave := ACBrPixCD1.PSP.ChavePIX;
    calendario.expiracao := seCobrancaExpiracao.Value;

    solicitacaoPagador := edtCriarCobrancaImediata_SolicitacaoAoPagador.Text;

    s := Trim(edtCriarCobrancaImediata_NomeDevedor.Text);
    if (s <> '') then
    begin
      devedor.nome := s;
      s := OnlyNumber(edtCriarCobrancaImediata_CPF_CNPJ.Text);
      if (s = '') then
        raise Exception.Create('Caso o Nome do Devedor seja Informado, e necessário informar CPF ou CNPJ')
      else if (Length(s) > 11) then
        devedor.cnpj := s
      else
        devedor.cpf := s;
    end;

    valor.original := StrToFloatDef(feCriarCobrancaImediatax_Valor.Text, 0);
    valor.modalidadeAlteracao := chCriarCobrancaImediata_PermiterAlterarValor.Checked;

    if (ACBrPixCD1.PSP is TACBrPSPShipay) then
    begin
      with infoAdicionais.New do
      begin
        nome := 'order_ref';
        valor := IfEmptyThen(edtCriarCobrancaImediata_TxId.Text, FormatDateTime('yymmddhhnnss', Now));
      end;
      s := FormatarValorPIX(valor.original);
      with infoAdicionais.New do
      begin
        nome := 'item_1';
        valor := '{"ean": "0123456789012", "item_title": "produtos diversos",'+
                 '"quantity": 1, "sku": "0001", "unit_price": '+s+' }';
      end;
    end;
  end;

  if (Trim(edtCriarCobrancaImediata_TxId.Text) <> '') then
    Ok := ACBrPixCD1.PSP.epCob.CriarCobrancaImediata(edtCriarCobrancaImediata_TxId.Text)
  else
    Ok := ACBrPixCD1.PSP.epCob.CriarCobrancaImediata;

  if Ok then
  begin
    mCriarCobrancaImediata.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.CobGerada.AsJSON);
    MostrarCobrancaEmLinhas( '  CobGerada',
                              ACBrPixCD1.PSP.epCob.CobGerada,
                              mCriarCobrancaImediata.Lines );
    qrcode := Trim(ACBrPixCD1.PSP.epCob.CobGerada.pixCopiaECola);
    if (qrcode = '') then
      qrcode := ACBrPixCD1.GerarQRCodeDinamico( ACBrPixCD1.PSP.epCob.CobGerada.location );
    PintarQRCode(qrcode, imgQRCriarCobrancaImediata.Picture.Bitmap, qrUTF8BOM);
    mCriarCobrancaImediata.Lines.Add('');
    mCriarCobrancaImediata.Lines.Add('- pixCopiaECola -');
    mCriarCobrancaImediata.Lines.Add(qrcode);
  end
  else
    mCriarCobrancaImediata.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON);
end;

procedure TForm1.btFluxoCancelarCobrancaClick(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;
  HabilitarInterface(False);
  try
    ConsultarCobranca;
    if (fFluxoDados.StatusCobranca = stcCONCLUIDA) then
    begin
      ShowMessage('Cobrança já foi PAGA. Impossível cancelar');
      Exit;
    end;

    if (MessageDlg('Deseja realmente Cancelar a Cobrança?', mtConfirmation, mbOKCancel, 0) = mrNo) then
    begin
      tmConsultarPagto.Enabled := True;
      Exit;
    end;

    ACBrPixCD1.PSP.epCob.CobRevisada.status := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
    if ACBrPixCD1.PSP.epCob.RevisarCobrancaImediata(FluxoDados.TxID) then
    begin
      ConsultarCobranca;
      ShowMessage('Cobrança cancelada com sucesso');
    end
    else
    begin
      ShowMessage('Falha ao Cancelar. Reiniciando Fluxo de Pagamento');
      ReiniciarFluxo;
    end;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TForm1.btFluxoEstornarPagtoClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente estornar o pagamento?', mtConfirmation, mbOKCancel, 0) = mrNo) then
    Exit;

  EstornarPagamento;
end;

procedure TForm1.btFluxoItemExcluirClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente excluir o Item?', mtConfirmation, mbOKCancel, 0) = mrNo) then
    Exit;

  ExcluirItemGrid(gdFluxoItens, gdFluxoItens.Row);

  AtualizarTotal;
  AvaliarInterfaceFluxoItem;
end;

procedure TForm1.btFluxoItemIncluirClick(Sender: TObject);
var
  wValor: Double;
begin
  wValor := StrToFloatDef(edFluxoItemValor.Text, 1);

  if EstaVazio(edFluxoItemDescricao.Text) then
  begin
    ShowMessage('Informe a Descrição do Item');
    edFluxoItemDescricao.SetFocus;
  end
  else if EstaVazio(edFluxoItemEAN.Text) then
  begin
    ShowMessage('Informe o Código EAN do Item');
    edFluxoItemEAN.SetFocus;
  end
  else
  begin
    AdicionarItemGridFluxo(
      Trim(edFluxoItemEAN.Text),
      Trim(edFluxoItemDescricao.Text),
      wValor);

    AtualizarTotal;
  end;

  AvaliarInterfaceFluxoItem;
end;

procedure TForm1.btFluxoNovaVendaClick(Sender: TObject);
begin
  ReiniciarFluxo;
end;

procedure TForm1.btFluxoPagarClick(Sender: TObject);
var
  wNome, wDoc: String;
  I: Integer;
begin
  VerificarConfiguracao;

  HabilitarInterface(False);
  try
    with ACBrPixCD1.PSP.epCob.CobSolicitada do
    begin
      Clear;
      chave := ACBrPixCD1.PSP.ChavePIX;
      calendario.expiracao := seCobrancaExpiracao.Value;

      wNome := Trim(edFluxoClienteNome.Text);
      if (wNome <> EmptyStr) then
      begin
        devedor.nome := wNome;
        wDoc := OnlyNumber(edFluxoClienteDoc.Text);
        if (wDoc = EmptyStr) then
        begin
          ShowMessage('Informe o Documento');
          edFluxoClienteDoc.SetFocus;
          Exit;
        end
        else if (Length(wDoc) > 11) then
          devedor.cnpj := wDoc
        else
          devedor.cpf := wDoc;
      end;

      // PSP Shipay necessita enviar os itens
      if (ACBrPixCD1.PSP is TACBrPSPShipay) then
      begin
        with infoAdicionais.New do
        begin
          nome := 'order_ref';
          valor := FormatDateTime('yymmddhhnnss', Now);
        end;

        for I := 1 to Pred(gdFluxoItens.RowCount) do
          with infoAdicionais.New do
          begin
            nome := 'item_' + IntToStr(I);
            valor := '{' +
              '"ean": "' + gdFluxoItens.Cells[0, I] + '", ' +
              '"item_title": "' + gdFluxoItens.Cells[1, I] + '", ' +
              '"quantity": 1, ' +
              '"sku": "' + gdFluxoItens.Cells[0, I] + '", ' +
              '"unit_price": ' + StringReplace(gdFluxoItens.Cells[2, I], '.', '', []) + '}';
          end;
      end;

      valor.original := fFluxoDados.Total;
    end;

    if ACBrPixCD1.PSP.epCob.CriarCobrancaImediata then
    begin
      fFluxoDados.TxID := ACBrPixCD1.PSP.epCob.CobGerada.txId;
      fFluxoDados.QRCode := Trim(ACBrPixCD1.PSP.epCob.CobGerada.pixCopiaECola);

      if (fFluxoDados.QRCode = EmptyStr) then
        fFluxoDados.QRCode := ACBrPixCD1.GerarQRCodeDinamico(ACBrPixCD1.PSP.epCob.CobGerada.location);

      edFluxoCopiaECola.Text := fFluxoDados.QRCode;
      PintarQRCode(fFluxoDados.QRCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
      ConsultarCobranca;
    end
    else
    begin
      fFluxoDados.EmErro := True;
      ShowMessage('Erro ao criar cobrança: ' + sLineBreak +
        FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON));
    end;

    tmConsultarPagto.Enabled := True;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TForm1.btItauGerarChavePrivadaClick(Sender: TObject);
var
  aPrivateKey, aPublicKey: String;
begin
  if FileExists(edtItauArqChavePrivada2.Text) then
    if MessageDlg( 'A chave já existe, deseja realmente sobreescrecer ?',
                   mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  ACBrOpenSSLUtils.GenerateKeyPair(aPrivateKey, aPublicKey);
  mItauChavePrivadaPEM.Lines.Text := ChangeLineBreak(aPrivateKey, sLineBreak);
  mItauChavePrivadaPEM.Lines.SaveToFile(edtItauArqChavePrivada2.Text);
end;

procedure TForm1.btItauSolicitarCertificadoClick(Sender: TObject);
var
  t, c: String;
begin
  ValidarChavePSPItau;
  if imgItauErroChavePrivada.Visible  then
  begin
    pgPSPItauChaveCertificado.ActivePageIndex := 0;
    pgPSPItauGerarChaveCertificado.ActivePageIndex := 0;
    MessageDlg('Favor configurar a Chave Privada', mtWarning, [mbOK], 0);
    Abort;
  end;

  t := Trim(mItauTokenTemporario.Lines.Text);
  if (t = '') then
  begin
    MessageDlg('Favor informar o Token temporário', mtWarning, [mbOK], 0);
    Abort;
  end;

  c := ACBrPSPItau1.SolicitarCertificado(t);
  mItauCertificadoPEM.Lines.Text := ChangeLineBreak(c, sLineBreak);
  mItauCertificadoPEM.Lines.SaveToFile(edtItauArqCertificado2.Text);
end;

procedure TForm1.btItauValidarChaveCertificadoClick(Sender: TObject);
begin
  ValidarChaveCertificadoPSPItau;
end;

procedure TForm1.btLimparConsultarCobrancaImediataClick(Sender: TObject);
begin
  mConsultarCobrancaImediata.Lines.Clear;
end;

procedure TForm1.btLimparConsultarCobrancasClick(Sender: TObject);
begin
  mConsultarCobrancas.Lines.Clear;
end;

procedure TForm1.btLimparConsultarDevolucaoPixClick(Sender: TObject);
begin
  mConsultarDevolucaoPix.Lines.Clear;
end;

procedure TForm1.btLimparConsultarPixClick(Sender: TObject);
begin
  mConsultarPix.Lines.Clear;
end;

procedure TForm1.btLimparConsultarPixRecebidosClick(Sender: TObject);
begin
  mConsultarPixRecebidos.Lines.Clear;
end;

procedure TForm1.btLimparCriarCobrancaImediataClick(Sender: TObject);
begin
  mCriarCobrancaImediata.Lines.Clear;
  imgQRCriarCobrancaImediata.Picture.Bitmap.FreeImage;
end;

procedure TForm1.btLimparSolicitarDevolucaoPixClick(Sender: TObject);
begin
  mSolicitarDevolucaoPix.Lines.Clear;
end;

procedure TForm1.btQRDGerarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  PintarQRCodeDinamico;
end;

procedure TForm1.btQREAnalisar1Click(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TForm1.btQREAnalisarClick(Sender: TObject);
var
  qre: TACBrPIXQRCodeEstatico;
begin
  qre := TACBrPIXQRCodeEstatico.Create;
  try
    AdicionarLinhaLog('----- Analise do QRCode Estático -----');
    qre.IgnoreErrors := True;
    qre.AsString := mQRE.Lines.Text;
    AdicionarLinhaLog('');
    AdicionarLinhaLog('NomeRecebedor: '+qre.MerchantName);
    AdicionarLinhaLog('CidadeRecebedor: '+qre.MerchantCity);
    AdicionarLinhaLog('CEPRecebedor: '+qre.PostalCode);
    AdicionarLinhaLog('ChavePix: '+qre.PixKey);
    AdicionarLinhaLog('TipoChavePix: '+GetEnumName(TypeInfo(TACBrPIXTipoChave), integer(qre.PixKeyType)));
    AdicionarLinhaLog('Valor: '+FormatFloat('0.00', qre.TransactionAmount));
    AdicionarLinhaLog('infoAdicional: '+qre.AdditionalInfo);
    AdicionarLinhaLog('TxId: '+qre.TxId);
    AdicionarLinhaLog('pss: '+IntToStr(qre.pss));
    AdicionarLinhaLog('mcc: '+IntToStr(qre.MerchantCategoryCode));
  finally
    qre.Free;
  end;
end;

procedure TForm1.btQREGerarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  PintarQRCodeEstatico;
end;

procedure TForm1.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TForm1.btQREColarClick(Sender: TObject);
begin
  mQRE.CopyToClipboard;
end;

procedure TForm1.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
  AplicarConfiguracao;
end;

procedure TForm1.btSolicitarDevolucaoPixClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mSolicitarDevolucaoPix.Lines.Clear;

  with ACBrPixCD1.PSP.epPix.DevolucaoSolicitada do
  begin
    Clear;
    valor := StrToFloatDef(feSolicitarDevolucaoPix_Valor.Text, 0);
    natureza := TACBrPIXNaturezaDevolucao(cbxSolicitarDevolucaoPix_Natureza.ItemIndex);
    descricao := edtSolicitarDevolucaoPix_Descricao.Text;
  end;

  if ACBrPixCD1.PSP.epPix.SolicitarDevolucaoPix( edtSolicitarDevolucaoPix_e2eid.Text,
                                                 edtSolicitarDevolucaoPix_id.Text ) then
  begin
    mSolicitarDevolucaoPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Devolucao.AsJSON);
    MostrarDevolucaoEmLinhas( '  Devolucao',
                              ACBrPixCD1.PSP.epPix.Devolucao,
                              mSolicitarDevolucaoPix.Lines );
  end
  else
    mSolicitarDevolucaoPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TForm1.cbxAmbienteChange(Sender: TObject);
begin
  tsItauCertificado.Enabled := (cbxAmbiente.ItemIndex > 0);
  lItauAvisoChaveCertificadoDesabilitado.Visible := not tsItauCertificado.Enabled;
end;

procedure TForm1.cbxPSPAtualChange(Sender: TObject);
begin
  imgErrPSP.Visible := (cbxPSPAtual.ItemIndex < 0);
end;

procedure TForm1.edtRecebedorCEPExit(Sender: TObject);
begin
  if (not imgErrCEP.Visible) and (edtRecebedorCidade.Text = '') then
    sbConsultaCEP.Click;
end;

procedure TForm1.edOnlyNumbersKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet( Key, [#8,#13,'0'..'9'] ) then
    Key := #0;
end;

procedure TForm1.edtConsultarPixRecebidosCPFCNPJChange(Sender: TObject);
var
  AStr, Mascara: String;
  AEdit: TEdit;
begin
  if not (Sender is TEdit) then
    Exit;

  AEdit := TEdit(Sender);
  AStr := OnlyNumber(AEdit.Text);
  if (Length(AStr) > 11) then
    Mascara := '**.***.***/****-**'
  else
    Mascara := '***.***.***-**';

  AEdit.Text := ACBrValidador.FormatarMascaraDinamica(AStr, Mascara);
  AEdit.SelStart := Length(AEdit.Text);
end;

procedure TForm1.edtItauArqChavePrivadaChange(Sender: TObject);
begin
  lItauErroChavePrivada.Caption := '';
  lItauErroCertificado.Caption := '';
  btItauValidarChaveCertificado.Visible :=
     imgItauErroChavePrivada.Visible or
     imgItauErroCertificado.Visible or
     (edtItauArqChavePrivada.Text <> ACBrPSPItau1.ArquivoChavePrivada) or
     (edtItauArqCertificado.Text <> ACBrPSPItau1.ArquivoCertificado);
end;

procedure TForm1.edtBBChavePIXChange(Sender: TObject);
begin
  cbxBBTipoChave.ItemIndex := Integer(DetectarTipoChave(edtBBChavePIX.Text));
  imgBBErroChavePIX.Visible := (edtBBChavePIX.Text <> '') and (cbxBBTipoChave.ItemIndex = 0);
end;

procedure TForm1.edtCriarCobrancaImediata_CPF_CNPJChange(Sender: TObject);
var
  AStr, Mascara: String;
begin
  AStr := OnlyNumber(edtCriarCobrancaImediata_CPF_CNPJ.Text);
  if (Length(AStr) > 11) then
    Mascara := '**.***.***/****-**'
  else
    Mascara := '***.***.***-**';

  edtCriarCobrancaImediata_CPF_CNPJ.Text := ACBrValidador.FormatarMascaraDinamica(AStr, Mascara);
  edtCriarCobrancaImediata_CPF_CNPJ.SelStart := Length(edtCriarCobrancaImediata_CPF_CNPJ.Text);
end;

procedure TForm1.edtCriarCobrancaImediata_NomeDevedorChange(Sender: TObject);
begin
  edtCriarCobrancaImediata_CPF_CNPJ.Enabled := (Trim(edtCriarCobrancaImediata_NomeDevedor.Text) <> '');
end;

procedure TForm1.edtItauChavePIXChange(Sender: TObject);
begin
  cbxItauTipoChave.ItemIndex := Integer(DetectarTipoChave(edtItauChavePIX.Text));
  imgItauErroChavePIX.Visible := (edtItauChavePIX.Text <> '') and (cbxItauTipoChave.ItemIndex = 0);
end;

procedure TForm1.edtItauClientIDChange(Sender: TObject);
begin
  imgItauErroClientID.Visible := not ValidarChaveAleatoria(edtItauClientID.Text);
end;

procedure TForm1.edtItauClientSecretChange(Sender: TObject);
begin
  imgItauErroClientSecret.Visible := not ValidarChaveAleatoria(edtItauClientSecret.Text);
end;

procedure TForm1.edtRecebedorNomeChange(Sender: TObject);
begin
  imgErrNome.Visible := (Length(Trim(edtRecebedorNome.Text)) < 5);
end;

procedure TForm1.edtSantanderChavePIXChange(Sender: TObject);
begin
  cbxSantanderTipoChave.ItemIndex := Integer(DetectarTipoChave(edtSantanderChavePIX.Text));
  imgSantanderErroChavePIX.Visible := (edtSantanderChavePIX.Text <> '') and (cbxSantanderTipoChave.ItemIndex = 0);
end;

procedure TForm1.mQREChange(Sender: TObject);
begin
  btQREAnalisar.Enabled := (Trim(mQRE.Lines.Text) <> '');
end;

procedure TForm1.pgPrincipalChange(Sender: TObject);
begin
  if (pgPrincipal.ActivePageIndex in [0, 1]) and btSalvarParametros.Enabled then
  begin
    GravarConfiguracao;
    AplicarConfiguracao;
  end;

  btSalvarParametros.Enabled := (pgPrincipal.ActivePageIndex = 2);
end;

procedure TForm1.pgPSPItauChaveCertificadoChange(Sender: TObject);
var
  a: String;
begin
  if (pgPSPItauChaveCertificado.ActivePageIndex = 1) then
  begin
    ValidarChavePSPItau;
    a := AdicionarPathAplicacao(edtItauArqChavePrivada.Text);
    if (a = '') then
      a := AdicionarPathAplicacao('ItauChavePrivada.pem');
    edtItauArqChavePrivada2.Text := a;
    if FileExists(a) then
    begin
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
      mItauChavePrivadaPEM.Lines.Text := ChangeLineBreak(ACBrOpenSSLUtils1.PrivateKeyAsString, sLineBreak);
    end
    else
      mItauChavePrivadaPEM.Lines.Text := 'Arquivo: '+a+'  não encontrado';

    a := AdicionarPathAplicacao(edtItauArqCertificado.Text);
    if (a = '') then
      a := AdicionarPathAplicacao('ItauCertificado.pem');
    edtItauArqCertificado2.Text := a;
  end;
end;

procedure TForm1.QuandoMudarDadosQRCode(Sender: TObject);
begin
  LimparQRCodeEstatico;
end;

procedure TForm1.AdicionarLinhaLog(AMensagem: String);
begin
  mLog.Lines.Add(AMensagem);
end;

procedure TForm1.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');

  if pgPrincipal.ActivePage = tsConfiguracao then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracao;
begin
  LigarAlertasdeErrosDeConfiguracaoPIXCD;
  LigarAlertasdeErrosDeConfiguracaoPSPItau;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPIXCD;
begin
  edtRecebedorNomeChange(Nil);
  edtRecebedorCEPChange(Nil);
  cbxPSPAtualChange(Nil);
  mQREChange(Nil);
  cbxAmbienteChange(Nil)
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPItau;
begin
  edtItauChavePIXChange(Nil);
  edtItauClientIDChange(Nil);
  edtItauClientSecretChange(Nil);
  tsItauCertificado.Enabled := (ACBrPixCD1.Ambiente > ambTeste);
  ValidarChaveCertificadoPSPItau;
end;

procedure TForm1.VerificarConfiguracao;
begin
  VerificarConfiguracaoPIXCD;
  if (ACBrPixCD1.PSP = ACBrPSPItau1) then
    VerificarConfiguracaoPSPItau;
end;

procedure TForm1.VerificarConfiguracaoPIXCD;
begin
  if imgErrNome.Visible or imgErrCEP.Visible or imgErrPSP.Visible then
  begin
    pgPrincipal.ActivePageIndex := 2;
    pgConfPixPSP.ActivePageIndex := 0;
    MessageDlg('Favor configurar os campos sinalizados', mtWarning, [mbOK], 0);
    Abort;
  end;
end;

procedure TForm1.VerificarConfiguracaoPSPItau;
begin
  if imgItauErroChavePIX.Visible or imgItauErroClientID.Visible or imgItauErroClientSecret.Visible then
  begin
    pgPrincipal.ActivePageIndex := 2;
    pgConfPixPSP.ActivePageIndex := 1;
    pgPSPs.ActivePageIndex := 2;
    pgPSPItau.ActivePageIndex := 0;
    pgPSPItauChaveCertificado.ActivePageIndex := 0;
    pgPSPItauGerarChaveCertificado.ActivePageIndex := 0;
    MessageDlg('Favor configurar as credenciais de acesso ao Itaú', mtWarning, [mbOK], 0);
    Abort;
  end;

  if (ACBrPixCD1.Ambiente > ambTeste) then
  begin
    if imgItauErroChavePrivada.Visible or imgItauErroCertificado.Visible then
    begin
      pgPrincipal.ActivePageIndex := 2;
      pgConfPixPSP.ActivePageIndex := 1;
      pgPSPs.ActivePageIndex := 2;
      pgPSPItau.ActivePageIndex := 1;
      pgPSPItauChaveCertificado.ActivePageIndex := 0;
      pgPSPItauGerarChaveCertificado.ActivePageIndex := 0;
      MessageDlg('Favor configurar a Chave Privada e Certificado', mtWarning, [mbOK], 0);
      Abort;
    end;
  end;
end;

procedure TForm1.ValidarChaveCertificadoPSPItau;
begin
  ValidarChavePSPItau;
  ValidarCertificadoPSPItau;
end;

procedure TForm1.ValidarChavePSPItau;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edtItauArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := 'Arquivo não especificado'
  else if (not FileExists(a)) then
    e := 'Arquivo não encontrado'
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lItauErroChavePrivada.Caption := e;
  imgItauErroChavePrivada.Visible := (e <> 'OK');
  btItauValidarChaveCertificado.Visible := imgItauErroChavePrivada.Visible;
end;

procedure TForm1.ValidarCertificadoPSPItau;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edtItauArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := 'Arquivo não especificado'
  else if (not FileExists(a)) then
    e := 'Arquivo não encontrado'
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadCertificateFromFile(a);  // Verifica se o arquivo de Chave é válido
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lItauErroCertificado.Caption := e;
  imgItauErroCertificado.Visible := (e <> 'OK');
  btItauValidarChaveCertificado.Visible := imgItauErroCertificado.Visible;
end;

procedure TForm1.LerConfiguracao;
var
  Ini: TIniFile;
begin
  AdicionarLinhaLog('- LerConfiguracao: '+NomeArquivoConfiguracao);
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    edtRecebedorNome.Text := Ini.ReadString('Recebedor', 'Nome', '');
    edtRecebedorCEP.Text := Ini.ReadString('Recebedor', 'CEP', '');
    edtRecebedorCidade.Text := Ini.ReadString('Recebedor', 'Cidade', '');
    cbxRecebedorUF.ItemIndex := cbxRecebedorUF.Items.IndexOf(Ini.ReadString('Recebedor', 'UF', ''));

    seRecebedorMCC.Value := Ini.ReadInteger('Recebedor', 'MCC', 0);

    cbxPSPAtual.ItemIndex := Ini.ReadInteger('PIX','PSP', 0);
    cbxAmbiente.ItemIndex := Ini.ReadInteger('PIX','Ambiente', 0);
    seTimeout.Value := Ini.ReadInteger('PIX', 'TimeOut', ChttpTimeOutDef);

    seCobrancaExpiracao.Value := Ini.ReadInteger('Cobranca', 'Expiracao', seCobrancaExpiracao.Value);

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    seProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := StrCrypt(DecodeBase64(Ini.ReadString('Proxy', 'Pass', '')), CURL_ACBR);

    edtArqLog.Text := Ini.ReadString('Log', 'Arquivo', '');
    cbxNivelLog.ItemIndex := Ini.ReadInteger('Log', 'Nivel', 1);

    edtShipayClientID.Text := Ini.ReadString('Shipay', 'ClientID', '');
    edtShipaySecretKey.Text := Ini.ReadString('Shipay', 'SecretKey', '');
    edtShipayAccessKey.Text := Ini.ReadString('Shipay', 'AccessKey', '');

    edtBBChavePIX.Text := Ini.ReadString('BancoBrasil', 'ChavePIX', '');
    edtBBClientID.Text := Ini.ReadString('BancoBrasil', 'ClientID', '');
    edtBBClientSecret.Text := Ini.ReadString('BancoBrasil', 'ClientSecret', '');
    edtBBDevAppKey.Text := Ini.ReadString('BancoBrasil', 'DeveloperApplicationKey', '');

    edtItauChavePIX.Text := Ini.ReadString('Itau', 'ChavePIX', '');
    edtItauClientID.Text := Ini.ReadString('Itau', 'ClientID', '');
    edtItauClientSecret.Text := Ini.ReadString('Itau', 'ClientSecret', '');
    edtItauXCorrelationId.Text := Ini.ReadString('Itau', 'XCorrelationId', '');
    edtItauArqChavePrivada.Text := Ini.ReadString('Itau', 'ArqChavePrivada', edtItauArqChavePrivada.Text);
    edtItauArqCertificado.Text := Ini.ReadString('Itau', 'ArqCertificado', edtItauArqCertificado.Text);

    edtSantanderChavePIX.Text := Ini.ReadString('Santander', 'ChavePIX', '');
    edtSantanderConsumerKey.Text := Ini.ReadString('Santander', 'ConsumerKey', '');
    edtSantanderConsumerSecret.Text := Ini.ReadString('Santander', 'ConsumerSecret', '');

  finally
    Ini.Free;
  end;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TForm1.GravarConfiguracao;
Var
  Ini : TIniFile ;
begin
  AdicionarLinhaLog('- LerConfiguracao: '+NomeArquivoConfiguracao);
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    Ini.WriteString('Recebedor', 'Nome', edtRecebedorNome.Text);
    Ini.WriteString('Recebedor', 'CEP', edtRecebedorCEP.Text);
    Ini.WriteString('Recebedor', 'Cidade', edtRecebedorCidade.Text);
    Ini.WriteString('Recebedor', 'UF', cbxRecebedorUF.Text);
    Ini.WriteInteger('Recebedor', 'MCC', seRecebedorMCC.Value);

    Ini.WriteInteger('PIX','PSP', cbxPSPAtual.ItemIndex);
    Ini.WriteInteger('PIX','Ambiente', cbxAmbiente.ItemIndex);
    Ini.WriteInteger('PIX', 'TimeOut', seTimeout.Value);

    Ini.WriteInteger('Cobranca', 'Expiracao', seCobrancaExpiracao.Value);

    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', seProxyPorta.Text);
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', EncodeBase64(StrCrypt(edtProxySenha.Text, CURL_ACBR)) );

    Ini.WriteString('Log', 'Arquivo', edtArqLog.Text);
    Ini.WriteInteger('Log', 'Nivel', cbxNivelLog.ItemIndex);

    Ini.WriteString('Shipay', 'ClientID', edtShipayClientID.Text);
    Ini.WriteString('Shipay', 'SecretKey', edtShipaySecretKey.Text);
    Ini.WriteString('Shipay', 'AccessKey', edtShipayAccessKey.Text);

    Ini.WriteString('BancoBrasil', 'ChavePIX', edtBBChavePIX.Text);
    Ini.WriteString('BancoBrasil', 'ClientID', edtBBClientID.Text);
    Ini.WriteString('BancoBrasil', 'ClientSecret', edtBBClientSecret.Text);
    Ini.WriteString('BancoBrasil', 'DeveloperApplicationKey', edtBBDevAppKey.Text);

    Ini.WriteString('Itau', 'ChavePIX', edtItauChavePIX.Text);
    Ini.WriteString('Itau', 'ClientID', edtItauClientID.Text);
    Ini.WriteString('Itau', 'ClientSecret', edtItauClientSecret.Text);
    Ini.WriteString('Itau', 'XCorrelationId', edtItauXCorrelationId.Text);
    Ini.WriteString('Itau', 'ArqChavePrivada', edtItauArqChavePrivada.Text);
    Ini.WriteString('Itau', 'ArqCertificado', edtItauArqCertificado.Text);

    Ini.WriteString('Santander', 'ChavePIX', edtSantanderChavePIX.Text);
    Ini.WriteString('Santander', 'ConsumerKey', edtSantanderConsumerKey.Text);
    Ini.WriteString('Santander', 'ConsumerSecret', edtSantanderConsumerSecret.Text);
  finally
     Ini.Free ;
  end ;

  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TForm1.AplicarConfiguracao;
begin
  AdicionarLinhaLog('- AplicarConfiguracao');
  ConfigurarACBrPIXCD;
  ConfigurarACBrPSPs;
end;

procedure TForm1.InicializarBitmaps;
begin       
  {$IfDef FPC}
  ImageList1.GetBitmap(5, imgInfoMCC.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrNome.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrCEP.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrPSP.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgBBErroChavePIX.Picture.Bitmap);

  ImageList1.GetBitmap(6, imgItauErroChavePIX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroClientID.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroClientSecret.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroChavePrivada.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroCertificado.Picture.Bitmap);

  ImageList1.GetBitmap(6, imgSantanderErroChavePIX.Picture.Bitmap);
  {$EndIf}
end;

procedure TForm1.InicializarActivePages;
begin
  pgPrincipal.ActivePageIndex := 0;
  pgConfPixPSP.ActivePageIndex := 0;
  pgPSPs.ActivePageIndex := 0;
  pgTestes.ActivePageIndex := 0;
  pgTestesPix.ActivePageIndex := 0;
  pgQRCode.ActivePageIndex := 0;
  pgTesteEndPoints.ActivePageIndex := 1;
  pgTestesEndPointCob.ActivePageIndex := 0;

  pgPSPItau.ActivePageIndex := 0;
  pgPSPItauChaveCertificado.ActivePageIndex := 0;
  pgPSPItauGerarChaveCertificado.ActivePageIndex := 0;
end;

procedure TForm1.InicializarComponentesDefault;
var
  i, l: Integer;
  j: TACBrPixCDAmbiente;
  k: TACBrPIXTipoChave;
  m: TACBrPIXStatusCobranca;
begin
  cbxPSPAtual.Items.Clear;
  For i := 0 to pgPSPs.PageCount-1 do
     cbxPSPAtual.Items.Add( pgPSPs.Pages[i].Caption );

  cbxRecebedorUF.Items.Clear;
  For i := Low(DFeUF) to High(DFeUF) do
     cbxRecebedorUF.Items.Add( DFeUF[i] );

  cbxAmbiente.Items.Clear;
  For j := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
     cbxAmbiente.Items.Add( GetEnumName(TypeInfo(TACBrPixCDAmbiente), integer(j) ));

  cbxBBTipoChave.Items.Clear;
  For k := Low(TACBrPIXTipoChave) to High(TACBrPIXTipoChave) do
     cbxBBTipoChave.Items.Add( GetEnumName(TypeInfo(TACBrPIXTipoChave), integer(k) ));
  cbxItauTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbxSantanderTipoChave.Items.Assign(cbxBBTipoChave.Items);

  cbxSolicitarDevolucaoPix_Natureza.Items.Clear;
  For l := 1 to Integer(High(TACBrPIXNaturezaDevolucao)) do
     cbxSolicitarDevolucaoPix_Natureza.Items.Add( GetEnumName(TypeInfo(TACBrPIXNaturezaDevolucao), l ));
  cbxSolicitarDevolucaoPix_Natureza.ItemIndex := 0;

  cbxConsultarCobrancas_Status.Items.Clear;
  For m := Low(TACBrPIXStatusCobranca) to High(TACBrPIXStatusCobranca) do
     cbxConsultarCobrancas_Status.Items.Add( GetEnumName(TypeInfo(TACBrPIXStatusCobranca), Integer(m) ));
  cbxConsultarCobrancas_Status.ItemIndex := 0;

  dtConsultarPixRecebidosInicio.DateTime := StartOfTheDay(IncDay(Today, -1));
  dtConsultarPixRecebidosFim.DateTime := EndOfTheDay(IncDay(Today, -1));

  dtConsultarCobrancas_Inicio.DateTime := StartOfTheDay(Today);
  dtConsultarCobrancas_Fim.DateTime := EndOfTheDay(Today);
end;

procedure TForm1.ConfigurarACBrPIXCD;
begin
  AdicionarLinhaLog('  - ConfigurarACBrPIXCD');
  ACBrPixCD1.Recebedor.Nome := edtRecebedorNome.Text;
  ACBrPixCD1.Recebedor.CEP := edtRecebedorCEP.Text;
  ACBrPixCD1.Recebedor.Cidade := edtRecebedorCidade.Text;
  ACBrPixCD1.Recebedor.UF := cbxRecebedorUF.Text;
  ACBrPixCD1.Recebedor.CodCategoriaComerciante := seRecebedorMCC.Value;

  ACBrPixCD1.Ambiente := TACBrPixCDAmbiente(cbxAmbiente.ItemIndex);
  ACBrPixCD1.TimeOut := seTimeout.Value;

  ACBrPixCD1.Proxy.Host := edtProxyHost.Text;
  ACBrPixCD1.Proxy.Port := seProxyPorta.Text;
  ACBrPixCD1.Proxy.User := edtProxyUser.Text;
  ACBrPixCD1.Proxy.Pass := edtProxySenha.Text;

  ACBrPixCD1.ArqLOG := edtArqLog.Text;
  ACBrPixCD1.NivelLog := cbxNivelLog.ItemIndex;

  case cbxPSPAtual.ItemIndex of
    0: ACBrPixCD1.PSP := ACBrPSPShipay1;
    1: ACBrPixCD1.PSP := ACBrPSPBancoDoBrasil1;
    2: ACBrPixCD1.PSP := ACBrPSPItau1;
    3: ACBrPixCD1.PSP := ACBrPSPSantander1;
  else
    raise Exception.Create('PSP configurado é inválido');
  end;
end;

procedure TForm1.ConfigurarACBrPSPs;
begin
  AdicionarLinhaLog('  - ConfigurarACBrPSPs');

  ACBrPSPShipay1.ClientID := edtShipayClientID.Text;
  ACBrPSPShipay1.SecretKey := edtShipaySecretKey.Text;
  ACBrPSPShipay1.AccessKey := edtShipayAccessKey.Text;

  ACBrPSPBancoDoBrasil1.ChavePIX := edtBBChavePIX.Text;
  ACBrPSPBancoDoBrasil1.ClientID := edtBBClientID.Text;
  ACBrPSPBancoDoBrasil1.ClientSecret := edtBBClientSecret.Text;
  ACBrPSPBancoDoBrasil1.DeveloperApplicationKey := edtBBDevAppKey.Text;

  ACBrPSPItau1.ChavePIX := edtItauChavePIX.Text;
  ACBrPSPItau1.ClientID := edtItauClientID.Text;
  ACBrPSPItau1.ClientSecret := edtItauClientSecret.Text;
  ACBrPSPItau1.xCorrelationID := edtItauXCorrelationId.Text;
  ACBrPSPItau1.ArquivoChavePrivada := edtItauArqChavePrivada.Text;
  ACBrPSPItau1.ArquivoCertificado := edtItauArqCertificado.Text;

  ACBrPSPSantander1.ChavePIX := edtItauChavePIX.Text;
  ACBrPSPSantander1.ConsumerKey := edtSantanderConsumerKey.Text;
  ACBrPSPSantander1.ConsumerSecret := edtSantanderConsumerSecret.Text;
end;

procedure TForm1.LimparQRCodeEstatico;
begin
  mQRE.Lines.Clear;
  imgQRE.Picture.Bitmap.FreeImage;
end;

procedure TForm1.PintarQRCodeEstatico;
begin
  mQRE.Lines.Text := ACBrPixCD1.GerarQRCodeEstatico(
                       StrToFloatDef(fleQREValor.Text, 0),
                       edtQREInfoAdicional.Text,
                       edtQRETxId.Text);
  PintarQRCode(mQRE.Lines.Text, imgQRE.Picture.Bitmap, qrUTF8BOM);
end;

procedure TForm1.PintarQRCodeDinamico;
begin
  mQRD.Lines.Text := ACBrPixCD1.GerarQRCodeDinamico( edtQRDLocation.Text );
  PintarQRCode(mQRD.Lines.Text, imgQRD.Picture.Bitmap, qrUTF8BOM);
end;

procedure TForm1.MostrarPixEmLinhas(const NomePix: String;
  APix: TACBrPIX; SL: TStrings);
var
  i: Integer;
begin
  SL.Add(NomePix+'.endToEndId: '+APix.endToEndId);
  SL.Add(NomePix+'.TxId: '+APix.txid);
  SL.Add(NomePix+'.valor: '+FormatFloatBr(APix.valor));
  if not APix.componentesValor.IsEmpty then
  begin
    SL.Add(NomePix+'.componentesValor.original.valor: '+FormatFloatBr(APix.componentesValor.original.valor));
    if (APix.componentesValor.saque.valor > 0) then
      SL.Add(NomePix+'.componentesValor.saque.valor: '+FormatFloatBr(APix.componentesValor.saque.valor));
    if (APix.componentesValor.troco.valor > 0) then
      SL.Add(NomePix+'.componentesValor.troco.valor: '+FormatFloatBr(APix.componentesValor.troco.valor));
    if (APix.componentesValor.juros.valor > 0) then
      SL.Add(NomePix+'.componentesValor.juros.valor: '+FormatFloatBr(APix.componentesValor.juros.valor));
    if (APix.componentesValor.multa.valor > 0) then
      SL.Add(NomePix+'.componentesValor.multa.valor: '+FormatFloatBr(APix.componentesValor.multa.valor));
    if (APix.componentesValor.abatimento.valor > 0) then
      SL.Add(NomePix+'.componentesValor.abatimento.valor: '+FormatFloatBr(APix.componentesValor.abatimento.valor));
    if (APix.componentesValor.desconto.valor > 0) then
      SL.Add(NomePix+'.componentesValor.desconto.valor: '+FormatFloatBr(APix.componentesValor.desconto.valor));
  end;
  SL.Add(NomePix+'.chave: '+APix.chave);
  SL.Add(NomePix+'.horario: '+FormatDateTimeBr(APix.horario));
  SL.Add(NomePix+'.infoPagador: '+APix.infoPagador);
  SL.Add(NomePix+'.devolucoes: '+IntToStr(APix.devolucoes.Count) );

  for i := 0 to APix.devolucoes.Count-1 do
    MostrarDevolucaoEmLinhas( NomePix+'.devolucoes['+IntToStr(i)+']',
                              APix.devolucoes[i],
                              SL );
end;

procedure TForm1.MostrarDevolucaoEmLinhas(const NomeDev: String;
  ADev: TACBrPIXDevolucao; SL: TStrings);
begin
  SL.Add(NomeDev+'.valor: '+FormatFloatBr(ADev.valor));
  SL.Add(NomeDev+'.natureza: '+PIXNaturezaDevolucaoToString(ADev.natureza));
  SL.Add(NomeDev+'.descricao: '+ADev.descricao);
  SL.Add(NomeDev+'.id: '+ADev.id);
  SL.Add(NomeDev+'.rtrId: '+ADev.rtrId);
  SL.Add(NomeDev+'.horario.solicitacao: '+FormatDateTimeBr(ADev.horario.solicitacao));
  SL.Add(NomeDev+'.horario.liquidacao: '+FormatDateTimeBr(ADev.horario.liquidacao));
  SL.Add(NomeDev+'.status: '+ PIXStatusDevolucaoToString(ADev.status));
  SL.Add(NomeDev+'.motivo: '+ADev.motivo);
end;

procedure TForm1.MostrarCobrancaEmLinhas(const NomeCobranca: String;
  ACob: TACBrPIXCobGerada; SL: TStrings);
var
  i: Integer;
begin
  SL.Add(NomeCobranca+'.calendario.criacao: '+FormatDateTimeBr(ACob.calendario.criacao));
  SL.Add(NomeCobranca+'.calendario.expiracao: '+IntToStr(ACob.calendario.expiracao));
  SL.Add(NomeCobranca+'.txId: '+ACob.txId);
  SL.Add(NomeCobranca+'.revisao: '+IntToStr(ACob.revisao));
  if (ACob.devedor.nome <> '') then
  begin
    SL.Add(NomeCobranca+'.devedor.nome: '+ACob.devedor.nome);
    if (ACob.devedor.cpf <> '') then
      SL.Add(NomeCobranca+'.devedor.cpf: '+ACob.devedor.cpf)
    else
      SL.Add(NomeCobranca+'.devedor.cnpj: '+ACob.devedor.cnpj)
  end;
  SL.Add(NomeCobranca+'.loc.id: '+IntToStr(ACob.loc.id));
  SL.Add(NomeCobranca+'.loc.txId: '+ACob.loc.txId);
  SL.Add(NomeCobranca+'.loc.location: '+ACob.loc.location);
  SL.Add(NomeCobranca+'.loc.criacao: '+FormatDateTimeBr(ACob.loc.criacao));
  SL.Add(NomeCobranca+'.location: '+ACob.location);
  SL.Add(NomeCobranca+'.status: '+ PIXStatusCobrancaToString(ACob.status));
  SL.Add(NomeCobranca+'.valor.original: '+FormatFloatBr(ACob.valor.original));
  SL.Add(NomeCobranca+'.valor.modalidadeAlteracao: '+BoolToStr(ACob.valor.modalidadeAlteracao, True));
  if (ACob.valor.retirada.saque.valor <> 0) then
  begin
    SL.Add(NomeCobranca+'.valor.retirada.saque.valor: '+FormatFloatBr(ACob.valor.retirada.saque.valor));
    SL.Add(NomeCobranca+'.valor.retirada.saque.modalidadeAlteracao: '+BoolToStr(ACob.valor.retirada.saque.modalidadeAlteracao, True));
    SL.Add(NomeCobranca+'.valor.retirada.saque.modalidadeAgente: '+PIXModalidadeAgenteToString(ACob.valor.retirada.saque.modalidadeAgente));
    SL.Add(NomeCobranca+'.valor.retirada.saque.prestadorDoServicoDeSaque: '+IntToStr(ACob.valor.retirada.saque.prestadorDoServicoDeSaque));
  end;
  if (ACob.valor.retirada.troco.valor <> 0) then
  begin
    SL.Add(NomeCobranca+'.valor.retirada.troco.valor: '+FormatFloatBr(ACob.valor.retirada.troco.valor));
    SL.Add(NomeCobranca+'.valor.retirada.troco.modalidadeAlteracao: '+BoolToStr(ACob.valor.retirada.troco.modalidadeAlteracao, True));
    SL.Add(NomeCobranca+'.valor.retirada.troco.modalidadeAgente: '+PIXModalidadeAgenteToString(ACob.valor.retirada.troco.modalidadeAgente));
    SL.Add(NomeCobranca+'.valor.retirada.troco.prestadorDoServicoDeSaque: '+IntToStr(ACob.valor.retirada.troco.prestadorDoServicoDeSaque));
  end;
  if (ACob.pixCopiaECola <> '') then
    SL.Add(NomeCobranca+'.pixCopiaECola: '+ACob.pixCopiaECola);

  if ACob is TACBrPIXCobCompleta then
  begin
    for i := 0 to TACBrPIXCobCompleta(ACob).pix.Count-1 do
      MostrarPixEmLinhas( '  '+NomeCobranca+'.Pix['+IntToStr(i)+']',
                          TACBrPIXCobCompleta(ACob).pix[i], SL );
  end;
end;

function TForm1.FormatarJSON(const AJSON: String): String;
{$IfDef FPC}
var
  jpar: TJSONParser;
  j: TJsonObject;
{$EndIf}
begin
  Result := AJSON;
  {$IfDef FPC}
  try
    j := TJSONObject.Create();
    try
      Result := j.Decode(Result);
    finally
      j.Free;
    end;
    jpar :=TJSONParser.Create(Result, [joUTF8]);
    try
      Result := jpar.Parse.FormatJSON([], 2);
    finally
      jpar.Free;
    end;
  except
    Result := AJSON;
  end;
  {$EndIf}
end;

function TForm1.RemoverPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (pos(ApplicationPath, s) = 1) then
    Result := ExtractFileName(s)
  else
    Result := s;
end;

function TForm1.AdicionarPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (s = '') then
    Result := s
  else if (ExtractFilePath(AFileName) <> '') then
    Result := s
  else
    Result := ApplicationPath + s;
end;

procedure TForm1.ReiniciarFluxo;
begin
  ACBrPixCD1.PSP.Clear;
  LimparInterfaceFluxoItem;

  with fFluxoDados do
  begin
    Total := 0;
    EmErro := False;
    TxID := EmptyStr;
    QRCode := EmptyStr;
    StatusCobranca := stcNENHUM;
    StatusDevolucao := stdNENHUM;
  end;

  AtualizarTotal;
  AtualizarStatus(stcNENHUM);
end;

procedure TForm1.ConsultarCobranca;
begin
  if EstaVazio(fFluxoDados.TxID) then
  begin
    ShowMessage('Nenhum TxID para ser consultado');
    Exit;
  end;

  HabilitarInterface(False);
  try
    if (not ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata(fFluxoDados.TxID)) then
    begin
      fFluxoDados.EmErro := True;
      ShowMessage('Erro ao consultar cobrança' + sLineBreak +
        ACBrPixCD1.PSP.epCob.Problema.title + sLineBreak +
        ACBrPixCD1.PSP.epCob.Problema.detail);
    end;

    if (ACBrPixCD1.PSP.epCob.CobCompleta.pix.Count > 0) then
      fFluxoDados.E2E := ACBrPixCD1.PSP.epCob.CobCompleta.pix[0].endToEndId;
    AtualizarStatus(ACBrPixCD1.PSP.epCob.CobCompleta.status);
  finally
    HabilitarInterface(True);
  end;
end;

procedure TForm1.ConsultarDevolucao;
begin
  if EstaVazio(fFluxoDados.E2E) then
  begin
    ShowMessage('Nenhum E2E para ser consultar');
    Exit;
  end;

  HabilitarInterface(False);
  try
    if (not ACBrPixCD1.PSP.epPix.ConsultarPix(fFluxoDados.E2E)) then
    begin
      fFluxoDados.EmErro := True;
      ShowMessage('Erro ao consultar devolucao' + sLineBreak +
        ACBrPixCD1.PSP.epPix.Problema.title + sLineBreak +
        ACBrPixCD1.PSP.epPix.Problema.detail);
    end;

    if (ACBrPixCD1.PSP.epPix.Pix.devolucoes.Count > 0) then
      AtualizarStatus(stcNENHUM, ACBrPixCD1.PSP.epPix.Pix.devolucoes[0].status);
  finally
    HabilitarInterface(True);
  end;
end;

procedure TForm1.EstornarPagamento;
begin
  if EstaVazio(fFluxoDados.E2E) then
  begin
    ShowMessage('Nenhum End2End para ser estornado');
    Exit;
  end;
  
  HabilitarInterface(False);
  try
    with ACBrPixCD1.PSP.epPix do
    begin
      DevolucaoSolicitada.Clear;
      DevolucaoSolicitada.valor := fFluxoDados.Total;
      DevolucaoSolicitada.natureza := ndORIGINAL;
      DevolucaoSolicitada.descricao := 'Devolucao da Venda';

      if SolicitarDevolucaoPix(fFluxoDados.E2E, '1234') then
      begin
        ConsultarDevolucao;

        if (fFluxoDados.StatusDevolucao = stdDEVOLVIDO) then
          ShowMessage('Pagamento Estornado com Sucesso')
        else if (fFluxoDados.StatusDevolucao = stdEM_PROCESSAMENTO) then
          tmConsultarDevolucao.Enabled := True;  // Estorno pendente? ...Consultar até alterar Status
      end
      else
      begin
        ShowMessage('Falha ao Estornar. Reiniciando o Fluxo de Pagamento');
        ReiniciarFluxo;
      end;
    end;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TForm1.AvaliarInterfaceFluxo;
var
  wVendendo, wSemEstorno, wAguardandoPagto: Boolean;
begin
  with fFluxoDados do
  begin
    wSemEstorno := (StatusDevolucao = stdNENHUM);
    wAguardandoPagto := (StatusCobranca = stcATIVA);
    wVendendo := (StatusCobranca = stcNENHUM) and (StatusDevolucao = stdNENHUM);

    gbFluxoCliente.Enabled := wVendendo;
    gbFluxoItens.Enabled := wVendendo;

    btFluxoPagar.Visible := wVendendo;
    btFluxoPagar.Enabled := wVendendo and (Total > 0);

    pnFluxoQRCode.Visible := wAguardandoPagto;
    pnFluxoCopiaECola.Visible := wAguardandoPagto;
    btFluxoCancelarCobranca.Visible := wAguardandoPagto;
    btFluxoEstornarPagto.Visible := (StatusCobranca = stcCONCLUIDA) and wSemEstorno;
    btFluxoNovaVenda.Visible := (StatusCobranca <> stcNENHUM);
  end;

  if gbFluxoItens.Enabled then
    AvaliarInterfaceFluxoItem;
end;

procedure TForm1.AvaliarInterfaceFluxoItem;
var
  wRemovido: Boolean;
begin
  with FluxoDados do
  begin
    wRemovido := (StatusCobranca in [stcREMOVIDA_PELO_PSP, stcREMOVIDA_PELO_USUARIO_RECEBEDOR]);
    btFluxoItemIncluir.Enabled := (StatusCobranca = stcNENHUM) or wRemovido;
    btFluxoItemExcluir.Enabled := ((StatusCobranca = stcNENHUM) or wRemovido) and
      (gdFluxoItens.RowCount > 1) and (gdFluxoItens.Row > 0);
  end;
end;

procedure TForm1.LimparInterfaceFluxoItem;
begin
  edFluxoItemEAN.Clear;
  edFluxoItemValor.Clear;
  edFluxoItemDescricao.Clear;
  InicializarGridFluxo;
end;

procedure TForm1.HabilitarInterface(aLiberada: Boolean);
begin
  pnFluxoBackground.Enabled := aLiberada;
end;

procedure TForm1.AtualizarTotal;
var
  I: Integer;
begin
  fFluxoDados.Total := 0;
  for I := 1 to Pred(gdFluxoItens.RowCount) do
    fFluxoDados.Total := FluxoDados.Total +
      StrToCurrDef(StringReplace(gdFluxoItens.Cells[2, I], '.', '', []), 0);
  pnFluxoTotalStr.Caption := FormatFloatBr(FluxoDados.Total, 'R$ ,0.00');
end;

procedure TForm1.AtualizarStatus(aStatus: TACBrPIXStatusCobranca;
  aStatusDevolucao: TACBrPIXStatusDevolucao);

  procedure AtualizarPanelPrincipal(aTexto: String; aCor: TColor);
  begin
    pnFluxoStatus.Color := aCor;
    pnFluxoStatus.Caption := aTexto;
  end;

begin
  if FluxoDados.EmErro then
  begin
    AtualizarPanelPrincipal('ERRO AO CONSULTAR', clRed);
    AvaliarInterfaceFluxo;
    Exit;
  end;

  fFluxoDados.StatusCobranca := aStatus;
  fFluxoDados.StatusDevolucao := aStatusDevolucao;
  AvaliarInterfaceFluxo;

  case FluxoDados.StatusDevolucao of
    stdDEVOLVIDO: AtualizarPanelPrincipal('PAGAMENTO DEVOLVIDO', $009A9A9A);
    stdEM_PROCESSAMENTO: AtualizarPanelPrincipal('DEVOLUÇAO PENDENTE', $00523C30);
    stdNAO_REALIZADO: AtualizarPanelPrincipal('DEVOLUÇÃO NÃO REALIZADA', $00523C30);
  else
    case FluxoDados.StatusCobranca of
      stcATIVA: AtualizarPanelPrincipal('AGUARDANDO PAGAMENTO', $001ADAE3);
      stcCONCLUIDA: AtualizarPanelPrincipal('PAGAMENTO FINALIZADO', $0009E31F);
      stcREMOVIDA_PELO_USUARIO_RECEBEDOR: AtualizarPanelPrincipal('PAGAMENTO CANCELADO', $000600EA);
      stcREMOVIDA_PELO_PSP: AtualizarPanelPrincipal('CANCELADO PELO PSP', $000600EA);
    else
      AtualizarPanelPrincipal('VENDENDO', clMenuHighlight);
    end;
  end;
end;

procedure TForm1.InicializarGridFluxo;
begin
  with gdFluxoItens do
  begin
    RowCount := 1;
    ColWidths[0] := 175;
    ColWidths[1] := 300;
    ColWidths[2] := 120;

    Cells[0,0] := 'EAN';
    Cells[1,0] := 'Descrição';
    Cells[2,0] := 'Valor';

    AdicionarItemGridFluxo('0123456789012', 'Batata Doce', 3.69);
  end;
end;

procedure TForm1.ExcluirItemGrid(aGrid: TStringGrid; aIndex: Integer);
var
  I, J: Integer;
begin
  with aGrid do
  begin
    for I := aIndex to RowCount - 2 do
      for J := 0 to ColCount - 1 do
        Cells[J, I] := Cells[J, I+1];

    RowCount := RowCount - 1
  end;
end;

procedure TForm1.AdicionarItemGridFluxo(aEan, aDescricao: String; aValor: Double);
begin
  with gdFluxoItens do
  begin
    RowCount := RowCount + 1;
    Cells[0, RowCount-1] := aEAN;
    Cells[1, RowCount-1] := aDescricao;
    Cells[2, RowCount-1] := FormatFloatBr(aValor);
  end;
end;

end.


e := VerificarChavePrivadaItau(OpenDialog1.FileName);
if (e <> '') then
begin
  mItauChavePrivadaPEM.Lines.Text := e;
  edtItauArqChavePrivada2.Text := '';
end
else
begin
  AFile := OpenDialog1.FileName;
  if (pos(ApplicationPath, AFile) = 1) then
    AFile := ExtractFileName(AFile);
  edtItauArqChavePrivada2.Text := AFile;
  mItauChavePrivadaPEM.Lines.Text := ChangeLineBreak(ACBrOpenSSLUtils1.PrivateKeyAsString, sLineBreak);
end;

