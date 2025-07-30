{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para intera��o com equipa- }
{ mentos de Automa��o Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Voc� pode obter a �ltima vers�o desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{  Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto}
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Sim�es de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatu� - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit FrPrincipal;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, Grids, ACBrCEP, ACBrPIXCD, ACBrPIXPSPItau,
  ACBrPIXPSPBancoDoBrasil, ACBrPIXPSPSantander, ACBrPIXBase, ACBrPIXSchemasPix,
  ACBrPIXSchemasDevolucao, ACBrPIXSchemasCob, ACBrPIXPSPShipay, ShellAPI,
  ACBrOpenSSLUtils, ACBrPIXPSPSicredi, ACBrPIXBRCode, ACBrSocket, ACBrBase,
  ImgList, ACBrPIXPSPSicoob, ACBrPIXPSPPagSeguro, ACBrPIXPSPGerenciaNet,
  ACBrPIXPSPBradesco, ACBrPIXPSPPixPDV, ACBrPIXPSPInter, ACBrPIXPSPAilos,
  ACBrPIXPSPMatera, ACBrPIXPSPCielo, ACBrPIXPSPMercadoPago, ACBrPIXPSPGate2All,
  ACBrPIXPSPBanrisul, ACBrPIXPSPC6Bank, ACBrPIXPSPAppLess
  {$IfDef FPC}
  , DateTimePicker
  {$EndIf};

const
  CMaxConsultas = 36;
  CURL_ACBR = 'https://projetoacbr.com.br/tef/';
  CURL_MateraPagto = 'https://flagship-payment-app.vercel.app/';
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
    ACBrPSPAilos1: TACBrPSPAilos;
    ACBrPSPAppLess1: TACBrPSPAppLess;
    ACBrPSPBancoDoBrasil1: TACBrPSPBancoDoBrasil;
    ACBrPSPBanrisul1: TACBrPSPBanrisul;
    ACBrPSPBradesco1: TACBrPSPBradesco;
    ACBrPSPC6Bank1: TACBrPSPC6Bank;
    ACBrPSPCielo1: TACBrPSPCielo;
    ACBrPSPGate2All1: TACBrPSPGate2All;
    ACBrPSPInter1: TACBrPSPInter;
    ACBrPSPItau1: TACBrPSPItau;
    ACBrPSPMatera1: TACBrPSPMatera;
    ACBrPSPMercadoPago1: TACBrPSPMercadoPago;
    ACBrPSPPagSeguro1: TACBrPSPPagSeguro;
    ACBrPSPPixPDV1: TACBrPSPPixPDV;
    ACBrPSPSantander1: TACBrPSPSantander;
    ACBrPSPShipay1: TACBrPSPShipay;
    ACBrPSPSicoob1: TACBrPSPSicoob;
    ACBrPSPSicredi1: TACBrPSPSicredi;
    btC6BankAcharCertificado: TSpeedButton;
    btC6BankAcharChavePrivada: TSpeedButton;
    btCancelarCobR: TBitBtn;
    btDesvincularLocRec: TBitBtn;
    btDesvincularLocRecLimpar: TBitBtn;
    btConsultarLocationsRec: TBitBtn;
    btConsultarLocationsRecLimpar: TBitBtn;
    btConsultarLocationRec: TBitBtn;
    btConsultarLocationRecLimpar: TBitBtn;
    btCriarLocationRec: TBitBtn;
    btLocationRecLimpar: TBitBtn;
    btSolicitarRetentativaCobR: TBitBtn;
    btCancelarCobRLimpar: TBitBtn;
    btSolicitarRetentativaLimpar: TBitBtn;
    btCancelarSolicitacaoRec: TBitBtn;
    btConsultarCobR: TBitBtn;
    btConsultarCobRLimpar: TBitBtn;
    btConsultarCobsR: TBitBtn;
    btConsultarCobsRLimpar: TBitBtn;
    btConsultarSolicitacaoRec: TBitBtn;
    btConsultarSolicitacaoRecLimpar: TBitBtn;
    btCancelarSolicitacaoRecLimpar: TBitBtn;
    btCriarCobR: TBitBtn;
    btCriarCobRLimpar: TBitBtn;
    btCriarCobRPreencher: TBitBtn;
    btCriarSolicitacaoRec: TBitBtn;
    btCriarSolicitacaoRecLimpar: TBitBtn;
    btRevisarRecorrenciaLimpar: TBitBtn;
    btConsultarRecorrencia: TBitBtn;
    btConsultarRecorrencias: TBitBtn;
    btCriarRecorrencia: TBitBtn;
    btCancelarRecorrencia: TBitBtn;
    btRevisarRecorrencia: TBitBtn;
    btCriarRecorrenciaPreencher: TBitBtn;
    btConsultarRecorrenciaLimpar: TBitBtn;
    btConsultarRecorrenciasLimpar: TBitBtn;
    btCriarRecorrenciaLimpar: TBitBtn;
    btMateraAcharArqCertificado: TSpeedButton;
    btMateraAcharChavePrivada: TSpeedButton;
    btAilosAcharCertificado: TSpeedButton;
    btAilosAcharCertificadoRoot: TSpeedButton;
    btAilosAcharchavePrivada: TSpeedButton;
    btPagSeguroPagar: TBitBtn;
    btPagSeguroLimpar: TBitBtn;
    btPixPDVSimularPagto: TBitBtn;
    btPixPDVSimularPagtoLimpar: TBitBtn;
    btSantanderExtrairPEM: TBitBtn;
    btCobVCancelar: TBitBtn;
    btCobVCancelarLimpar: TBitBtn;
    btCobVConsultar: TBitBtn;
    btCobVConsultarLimpar: TBitBtn;
    btCobVConsultarLista: TBitBtn;
    btCobVConsultarListaLimpar: TBitBtn;
    btCobVCopiaECola: TSpeedButton;
    btCriarCobV: TBitBtn;
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
    btItauRenovarCertificado: TBitBtn;
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
    btQRDAnalisar: TBitBtn;
    btQREColar: TBitBtn;
    btQRDColar: TBitBtn;
    btQREGerar: TBitBtn;
    btSicoobExtrairChaveCertificado: TBitBtn;
    btSicoobExtrairChaveCertificadoArqPFX: TSpeedButton;
    btSicoobExtrairChaveCertificadoVerSenhaPFX: TSpeedButton;
    btSicrediGerarCSR: TBitBtn;
    btSicrediGerarCSRInfo: TSpeedButton;
    btSicrediGerarChaveInfo: TSpeedButton;
    btSicrediGerarChavePrivada: TBitBtn;
    btSolicitarDevolucaoPix: TBitBtn;
    btConsultarPix: TBitBtn;
    btConsultarPixRecebidos: TBitBtn;
    btConsultarDevolucaoPix: TBitBtn;
    btLogLimpar: TBitBtn;
    btLerParametros: TBitBtn;
    btSalvarParametros: TBitBtn;
    btCriarCobrancaImediata: TBitBtn;
    cbAilosTipoChave: TComboBox;
    cbBanrisulTipoChave: TComboBox;
    cbPIXPDVVersaoAPI: TComboBox;
    cbC6BankTipoChave: TComboBox;
    cbConsultarCobsRStatus: TComboBox;
    cbConsultarLocationsRecComIdLocRed: TCheckBox;
    cbCriarRecorrenciaPeriodicidade: TComboBox;
    cbCriarRecorrenciaPoliticaRetentativa: TComboBox;
    cbCriarCobRTipoConta: TComboBox;
    cbMercadoPagoTipoChave: TComboBox;
    cbCobVConsultarLocation: TCheckBox;
    cbCobVConsultarStatus: TComboBox;
    cbCobVDescModalidade: TComboBox;
    cbCobVJurosModalidade: TComboBox;
    cbCobVMultaModalidade: TComboBox;
    cbGerenciaNetTipoChave: TComboBox;
    cbBradescoTipoChave: TComboBox;
    cbInterTipoChave: TComboBox;
    cbCieloTipoChave: TComboBox;
    cbPagSeguroTipoChave: TComboBox;
    cbSicoobTipoChave: TComboBox;
    cbSicrediTipoChave: TComboBox;
    cbxAmbiente: TComboBox;
    cbBBVersaoAPI: TComboBox;
    cbConsultarRecorrenciasStatus: TComboBox;
    cbxItauTipoChave: TComboBox;
    cbxNivelLog: TComboBox;
    cbxPSPAtual: TComboBox;
    cbxRecebedorUF: TComboBox;
    cbSantanderTipoChave: TComboBox;
    cbxSolicitarDevolucaoPix_Natureza: TComboBox;
    cbxBBTipoChave: TComboBox;
    cbxConsultarCobrancas_Status: TComboBox;
    cbConsultarRecorrenciasComLocation: TCheckBox;
    chCriarCobrancaImediata_PermiterAlterarValor: TCheckBox;
    chConsultarCobrancas_ComLocation: TCheckBox;
    cbAutenticacaoManual: TCheckBox;
    CobVConsultarRodapeLista: TPanel;
    dtConsultarCobrancas_Fim: TDateTimePicker;
    edCancelarCobRTxID: TEdit;
    edConsultarRecorrenciaTxId: TEdit;
    edDesvincularLocRecId: TEdit;
    edConsultarLocationsRecConvenio: TEdit;
    edConsultarLocationsRecFim: TDateTimePicker;
    edConsultarLocationsRecInicio: TDateTimePicker;
    edConsultarLocationsRecItensPorPagina: TSpinEdit;
    edConsultarLocationsRecPagina: TSpinEdit;
    edConsultarLocationRecId: TEdit;
    edSolicitarRetentativaTxID: TEdit;
    edConsultarCobRTxID: TEdit;
    edConsultarCobsRIdRec: TEdit;
    edCriarCobRIdRec: TEdit;
    edConsultarCobsRConvenio: TEdit;
    edConsultarCobsRDoc: TEdit;
    edConsultarCobsRFim: TDateTimePicker;
    edConsultarCobsRInicio: TDateTimePicker;
    edConsultarCobsRItensPorPagina: TSpinEdit;
    edConsultarCobsRPagina: TSpinEdit;
    edConsultarSolicitacaoRecIdSolicRec: TEdit;
    edConsultarRecorrenciasConvenio: TEdit;
    edConsultarRecorrenciasFim: TDateTimePicker;
    edConsultarRecorrenciasInicio: TDateTimePicker;
    dtConsultarPixRecebidosInicio: TDateTimePicker;
    dtConsultarPixRecebidosFim: TDateTimePicker;
    dtConsultarCobrancas_Inicio: TDateTimePicker;
    edAppLessHMAC: TEdit;
    edBanrisulChavePIX: TEdit;
    edBanrisulClientID: TEdit;
    edBanrisulClientSecret: TEdit;
    edBanrisulArqCertificadoPFX: TEdit;
    edBanrisulSenhaCertificadoPFX: TEdit;
    edBradescoArqCertificado: TEdit;
    edBradescoArqChavePrivada: TEdit;
    edBradescoArqPFX: TEdit;
    edBradescoSenhaPFX: TEdit;
    edCieloChavePIX: TEdit;
    edC6BankCertificado: TEdit;
    edC6BankChavePIX: TEdit;
    edC6BankChavePrivada: TEdit;
    edC6BankClientID: TEdit;
    edC6BankClientSecret: TEdit;
    edAppLessClientId: TEdit;
    edAppLessClientSecret: TEdit;
    edCriarCobRConta: TEdit;
    edCriarCobRAgencia: TEdit;
    edCriarCobREmail: TEdit;
    edCriarCobRCEP: TEdit;
    edCriarCobRCidade: TEdit;
    edCriarCobRUF: TEdit;
    edCriarCobRVencimento: TDateTimePicker;
    edCriarCobRLogradouro: TEdit;
    edCriarCobRInfoAdicional: TEdit;
    edCriarCobRValor: TEdit;
    edSolicitarRetentativaLiquidacao: TDateTimePicker;
    edCriarSolicitacaoRecAgencia: TEdit;
    edCriarSolicitacaoRecDoc: TEdit;
    edCriarSolicitacaoRecExpiracao: TDateTimePicker;
    edCriarSolicitacaoRecConta: TEdit;
    edCriarSolicitacaoRecISPBParticipante: TEdit;
    edRevisarRecorrenciaDocDevedor: TEdit;
    edRevisarRecorrenciaDataInicial: TDateTimePicker;
    edCriarRecorrenciaLoc: TSpinEdit;
    edCriarRecorrenciaDataInicial: TDateTimePicker;
    edCriarRecorrenciaDataFinal: TDateTimePicker;
    edCriarSolicitacaoRecIdRec: TEdit;
    edCancelarSolicitacaoRecIdSolicRec: TEdit;
    edRevisarRecorrenciaLoc: TSpinEdit;
    edRevisarRecorrenciaNomeDevedor: TEdit;
    edRevisarRecorrenciaIdRec: TEdit;
    edRevisarRecorrenciatxId: TEdit;
    edMercadoPagoChavePIX: TEdit;
    edCieloClientID: TEdit;
    edGate2AllAuthenticationApi: TEdit;
    edMercadoPagoAccessToken: TEdit;
    edCieloClientSecret: TEdit;
    edMateraAccountId: TEdit;
    edAilosCertificado: TEdit;
    edAilosCertificadoRoot: TEdit;
    edAilosChavePIX: TEdit;
    edAilosChavePrivada: TEdit;
    edAilosClientID: TEdit;
    edAilosClientSecret: TEdit;
    edMateraArqCertificado: TEdit;
    edMateraArqChavePrivada: TEdit;
    edBBArqCertificado: TEdit;
    edBBArqChavePrivada: TEdit;
    edBBArqPFX: TEdit;
    edBBSenhaPFX: TEdit;
    edMateraChavePIX: TEdit;
    edCobVCancelarTxID: TEdit;
    edCobVCompradorDoc: TEdit;
    edCobVCompradorNome: TEdit;
    edCobVConsultarCPFCNPJ: TEdit;
    edCobVConsultarFim: TDateTimePicker;
    edCobVConsultarInicio: TDateTimePicker;
    edCobVConsultarItensPag: TSpinEdit;
    edCobVConsultarPagina: TSpinEdit;
    edCobVConsultarRevisao: TSpinEdit;
    edCobVConsultarTxID: TEdit;
    edCobVCopiaECola: TEdit;
    edCobVDescValor: TEdit;
    edCobVDiasPagar: TSpinEdit;
    edCobVJurosValor: TEdit;
    edCobVMultaValor: TEdit;
    edCobVValor: TEdit;
    edCobVVencimento: TDateTimePicker;
    edFluxoClienteDoc: TEdit;
    edFluxoClienteNome: TEdit;
    edFluxoCopiaECola: TEdit;
    edFluxoItemDescricao: TEdit;
    edFluxoItemEAN: TEdit;
    edFluxoItemValor: TEdit;
    edGerenciaNetArqPFX: TEdit;
    edGerenciaNetChavePIX: TEdit;
    edBradescoChavePIX: TEdit;
    edInterChavePIX: TEdit;
    edGerenciaNetClientID: TEdit;
    edBradescoClientID: TEdit;
    edInterClientID: TEdit;
    edGerenciaNetClientSecret: TEdit;
    edBradescoClientSecret: TEdit;
    edInterClientSecret: TEdit;
    edItauRenovarCertificadoArq: TEdit;
    edInterCertificado: TEdit;
    edInterChavePrivada: TEdit;
    edMateraClientSecret: TEdit;
    edGate2AllAuthenticationKey: TEdit;
    edPagSeguroTokenPay: TEdit;
    edPixPDVCNPJ: TEdit;
    edPixPDVSecretKey: TEdit;
    edPixPDVSimularPagtoQRCodeID: TEdit;
    edPixPDVToken: TEdit;
    edMateraClientID: TEdit;
    edMateraSecretKey: TEdit;
    edSantanderArqCertificadoPFX: TEdit;
    edSantanderExtrairCertificadoPFX: TEdit;
    edSantanderExtrairCertificadoPEM: TEdit;
    edSantanderSenhaCertificadoPFX: TEdit;
    edSantanderExtrairCertificadoSenhaPFX: TEdit;
    edSicoobArqCertificado: TEdit;
    edSicoobArqChavePrivada: TEdit;
    edSicoobChavePIX: TEdit;
    edSicoobClientID: TEdit;
    edSicoobTokenSandbox: TEdit;
    edSicoobExtrairCertificado: TEdit;
    edSicoobExtrairChaveCertificadoArqPFX: TEdit;
    edSicoobExtrairChaveCertificadoSenhaPFX: TEdit;
    edSicoobExtrairChavePrivada: TEdit;
    edSicrediArqCertificado: TEdit;
    edPagSeguroArqCertificado: TEdit;
    edCieloArqCertificado: TEdit;
    edSicrediArqChavePrivada: TEdit;
    edPagSeguroArqChavePrivada: TEdit;
    edCieloArqChavePrivada: TEdit;
    edSicrediChavePIX: TEdit;
    edPagSeguroChavePIX: TEdit;
    edSicrediClientID: TEdit;
    edPagSeguroClientID: TEdit;
    edSicrediClientSecret: TEdit;
    edPagSeguroClientSecret: TEdit;
    edSicrediGerarCSR: TEdit;
    edSicrediGerarChavePrivada: TEdit;
    edSicrediGerarCSREmail: TEdit;
    edtArqLog: TEdit;
    edPagSeguroTxID: TEdit;
    edConsultarRecorrenciaIdRec: TEdit;
    edConsultarRecorrenciasDoc: TEdit;
    edCriarRecorrenciaCPFCNPJDevedor: TEdit;
    edCriarRecorrenciaContrato: TEdit;
    edCriarRecorrenciaNomeDevedor: TEdit;
    edCriarRecorrenciaObjeto: TEdit;
    edCriarRecorrenciaTxID: TEdit;
    edCriarCobRTxID: TEdit;
    edtProxyHost: TEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    edQRDTxID: TEdit;
    edtRecebedorCEP: TEdit;
    edtRecebedorCidade: TEdit;
    edtRecebedorNome: TEdit;
    edSantanderChavePIX: TEdit;
    edSantanderConsumerKey: TEdit;
    edSantanderConsumerSecret: TEdit;
    edMateraMediatorFee: TEdit;
    edCriarRecorrenciaValor: TEdit;
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
    edtQREInfoAdicional: TEdit;
    edQRDLocation: TEdit;
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
    gbCobVComprador: TGroupBox;
    gbCobVDesconto: TGroupBox;
    gbCobVJuros: TGroupBox;
    gbCobVMulta: TGroupBox;
    gbFluxoCliente: TGroupBox;
    gbFluxoItens: TGroupBox;
    gbFluxoStatus: TGroupBox;
    gbFluxoTotal: TGroupBox;
    gdFluxoItens: TStringGrid;
    gbAutenticacaoManual: TGroupBox;
    imAilosErroCertificado: TImage;
    imAilosErroCertificadoRoot: TImage;
    imAilosErroChavePIX: TImage;
    imAilosErroChavePrivada: TImage;
    imBBErroCertificado: TImage;
    imBradescoErroCertificado: TImage;
    imBBErroChavePrivada: TImage;
    imBradescoErroChavePrivada: TImage;
    imBBErroPFX: TImage;
    imBanrisulErroChavePix: TImage;
    imBanrisulErroCertificadoPFX: TImage;
    imBradescoErroPFX: TImage;
    imC6BankErroCertificado: TImage;
    imC6BankErroChavePix: TImage;
    imC6BankErroChavePrivada: TImage;
    imMercadoPagoErroChavePix: TImage;
    imCobVQRCode: TImage;
    imCieloErroChavePix: TImage;
    imMateraErroCertificado: TImage;
    imMateraErroChavePrivada: TImage;
    imFluxoQRCode: TImage;
    imGerenciaNetErroChavePix: TImage;
    imBradescoErroChavePix: TImage;
    imInterErroChavePix: TImage;
    imGerenciaNetErroPFX: TImage;
    imgErrCEP: TImage;
    imgErrNome: TImage;
    imgErrPSP: TImage;
    imgItauErroCertificado: TImage;
    imgItauErroChavePIX: TImage;
    imgItauErroChavePrivada: TImage;
    imgItauErroClientID: TImage;
    imgItauErroClientSecret: TImage;
    imgQRCriarCobrancaImediata: TImage;
    imgQRE: TImage;
    imgQRD: TImage;
    imInterErroCertificado: TImage;
    imInterErroChavePrivada: TImage;
    imSantanderErroChavePIX: TImage;
    imSantanderErroCertificadoPFX: TImage;
    imSicoobErroCertificado: TImage;
    imSicoobErroChavePIX: TImage;
    imSicoobErroChavePrivada: TImage;
    imSicrediErroCertificado: TImage;
    imPagSeguroErroCertificado: TImage;
    imCieloErroCertificado: TImage;
    imSicrediErroChavePix: TImage;
    imPagSeguroErroChavePix: TImage;
    imSicrediErroChavePrivada: TImage;
    imPagSeguroErroChavePrivada: TImage;
    imCieloErroChavePrivada: TImage;
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
    lbConsultarRecorrenciatxId: TLabel;
    lbPIXPDVVersaoAPI: TLabel;
    lbCancelarCobRTxID: TLabel;
    lbDesvincularLocRecId: TLabel;
    lbConsultarLocationsRecConvenio: TLabel;
    lbConsultarLocationsRecFim: TLabel;
    lbConsultarLocationsRecInicio: TLabel;
    lbConsultarLocationsRecItensPorPagina: TLabel;
    lbConsultarLocationsRecPagina: TLabel;
    lbConsultarLocationRecId: TLabel;
    lbSolicitarRetentativaTxID: TLabel;
    lbConsultarCobRTxID: TLabel;
    lbConsultarCobsRIdRec: TLabel;
    lbCriarCobRIdRec: TLabel;
    lbConsultarCobsRConvenio: TLabel;
    lbConsultarCobsRDoc: TLabel;
    lbConsultarCobsRFim: TLabel;
    lbConsultarCobsRInicio: TLabel;
    lbConsultarCobsRItensPorPagina: TLabel;
    lbConsultarCobsRPagina: TLabel;
    lbConsultarCobsRStatus: TLabel;
    lbConsultarSolicitacaoRecIdSolicRec: TLabel;
    lbConsultarRecorrenciasConvenio: TLabel;
    lbCriarCobRConta: TLabel;
    lbCriarCobRAgencia: TLabel;
    lbCriarCobREmail: TLabel;
    lbCriarCobRCEP: TLabel;
    lbCriarCobRCidade: TLabel;
    lbCriarCobRUF: TLabel;
    lbCriarCobRVencimento: TLabel;
    lbCriarCobRLogradouro: TLabel;
    lbCriarCobRInfoAdicional: TLabel;
    lbCriarCobRTipoConta: TLabel;
    lbCriarCobRValor: TLabel;
    lbSolicitarRetentativaLiquidacao: TLabel;
    lbCriarSolicitacaoRecAgencia: TLabel;
    lbCriarSolicitacaoRecDoc: TLabel;
    lbCriarSolicitacaoRecExpiracao: TLabel;
    lbCriarSolicitacaoRecConta: TLabel;
    lbCriarSolicitacaoRecISPBParticipante: TLabel;
    lbRevisarRecorrenciaDocDevedor: TLabel;
    lbRevisarRecorrenciaDataInicial: TLabel;
    lbCriarRecorrenciaLoc: TLabel;
    lbCriarSolicitacaoRecIdRec: TLabel;
    lbCancelarSolicitacaoRecIdSolicRec: TLabel;
    lbRevisarRecorrenciaLoc: TLabel;
    lbRevisarRecorrenciaNomeDevedor: TLabel;
    lbRevisarRecorrenciaIdRec: TLabel;
    lbRevisarRecorrenciaTxID: TLabel;
    lbCriarRecorrenciaValor: TLabel;
    lbConsultarRecorrenciasStatus: TLabel;
    lbAppLessHMAC: TLabel;
    lbBanrisulChave: TLabel;
    lbBanrisulClientID: TLabel;
    lbBanrisulClientSecret: TLabel;
    lbBanrisulTipoChave: TLabel;
    lbBanrisulArqCertificadoPFX: TLabel;
    lbBanrisulErroCertificadoPFX: TLabel;
    lbBanrisulSenhaCertificado: TLabel;
    lbBradescoArqCertificado: TLabel;
    lbBradescoArqChavePrivada: TLabel;
    lbBradescoErroCertificado: TLabel;
    lbBradescoErroChavePrivada: TLabel;
    lbBradescoErroPFX: TLabel;
    lbBradescoPFX: TLabel;
    lbBradescoSenhaPFX: TLabel;
    lbCieloChave: TLabel;
    lbAppLessClientId: TLabel;
    lbCriarRecorrenciaPeriodicidade: TLabel;
    lbCriarRecorrenciaPoliticaRetentativa: TLabel;
    lbCriarRecorrenciaDataInicial: TLabel;
    lbCriarRecorrenciaDataFinal: TLabel;
    lbGate2AllAuthenticationKey: TLabel;
    lbC6BankCertificado: TLabel;
    lbC6BankChave: TLabel;
    lbC6BankChavePrivada: TLabel;
    lbC6BankClientID: TLabel;
    lbC6BankClientSecret: TLabel;
    lbC6BankErroCertificado: TLabel;
    lbC6BankErroChavePrivada: TLabel;
    lbC6BankTipoChave: TLabel;
    lbAppLessClientSecret: TLabel;
    lbMercadoPagoChavePIX: TLabel;
    lbCieloClientID: TLabel;
    lbMercadoPagoAccessToken: TLabel;
    lbCieloClientSecret: TLabel;
    lbCieloTipoChave: TLabel;
    lbGate2AllAuthenticationApi: TLabel;
    lbMercadoPagoTipoChave: TLabel;
    lbMateraMediatorFee: TLabel;
    lbMateraSimularPagamento: TLabel;
    lbMateraArqCertificado: TLabel;
    lbMateraArqChavePrivada: TLabel;
    lbBBArqCertificado: TLabel;
    lbBBArqChavePrivada: TLabel;
    lbBBArqPFX: TLabel;
    lbBBErroCertificado: TLabel;
    lbBBErroChavePrivada: TLabel;
    lbBBErroPFX: TLabel;
    lbBBVersaoAPI: TLabel;
    lbAilosCertificado: TLabel;
    lbAilosCertificadoRoot: TLabel;
    lbAilosChave: TLabel;
    lbAilosChavePrivada: TLabel;
    lbAilosClienteID: TLabel;
    lbAilosClientSecret: TLabel;
    lbAilosErroCertificado: TLabel;
    lbAilosErroCertificadoRoot: TLabel;
    lbAilosErroChavePrivada: TLabel;
    lbAilosTipoChave: TLabel;
    lbBradescoChave: TLabel;
    lbBradescoClientKey: TLabel;
    lbBBSenhaPFX: TLabel;
    lbMateraClientID: TLabel;
    lbMateraAccountId: TLabel;
    lbMateraChavePIX: TLabel;
    lbMateraClientSecret: TLabel;
    lbMateraErroCertificado: TLabel;
    lbMateraErroChavePrivada: TLabel;
    lbInterChave: TLabel;
    lbInterClientID: TLabel;
    lbGerenciaNetClientSecret: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    lbGerenciaNetClientID: TLabel;
    Label3: TLabel;
    lbGerenciaNetChave: TLabel;
    lbBradescoClientSecret: TLabel;
    lbInterClientSecret: TLabel;
    lbGerenciaNetTipoChave: TLabel;
    lbGerenciaNetErroPFX: TLabel;
    lbGerenciaNetPFX: TLabel;
    lbBradescoTipoChave: TLabel;
    lbInterTipoChave: TLabel;
    lbInterChavePrivada: TLabel;
    lbInterErroCertificado: TLabel;
    lbInterErroChavePrivada: TLabel;
    lbInterCertificado: TLabel;
    lbPagSeguroTokenPay: TLabel;
    lbPixPDVCNPJ: TLabel;
    lbPixPDVQRCodeID: TLabel;
    lbPixPDVSecretKey: TLabel;
    lbPixPDVToken: TLabel;
    lbSantanderChavePIX: TLabel;
    lbSantanderTipoChave: TLabel;
    lbSantanderConsumerKey: TLabel;
    lbSantanderConsumerSecret: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label42: TLabel;
    lbSantanderArqCertificadoPFX: TLabel;
    lbSantanderExtrairCertificadoPFX: TLabel;
    lbSantanderExtrairCertificadoPEM: TLabel;
    lbSantanderErroCertificadoPFX: TLabel;
    lbSantanderSenhaCertificado: TLabel;
    lbSantanderExtrairCertificadoSenhaPFX: TLabel;
    lbQRDTxID: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbCobVCancelarTxID: TLabel;
    lbCobVCompradorDoc: TLabel;
    lbCobVCompradorNome: TLabel;
    lbCobVConsultarCPFCNPJ: TLabel;
    lbCobVConsultarFim: TLabel;
    lbCobVConsultarInicio: TLabel;
    lbCobVConsultarItensPag: TLabel;
    lbCobVConsultarPagina: TLabel;
    lbCobVConsultarRevisao: TLabel;
    lbCobVConsultarStatus: TLabel;
    lbCobVConsultarTxID: TLabel;
    lbCobVCopiaECola: TLabel;
    lbCobVDescModalidade: TLabel;
    lbCobVDescValor: TLabel;
    lbCobVDiasPagar: TLabel;
    lbCobVJurosModalidade: TLabel;
    lbCobVJurosValor: TLabel;
    lbCobVMultaModalidade: TLabel;
    lbCobVMultaValor: TLabel;
    lbCobVValor: TLabel;
    lbCobVVencimento: TLabel;
    lbFluxoClienteDoc: TLabel;
    lbFluxoClienteNome: TLabel;
    lbFluxoCopiaECola: TLabel;
    lbFluxoItemDescricao: TLabel;
    lbFluxoItemEAN: TLabel;
    lbFluxoItemValor: TLabel;
    lbItauRenovarCertificadoArq: TLabel;
    lbItauRenovarCertificadoPEM: TLabel;
    lbMateraSecretKey: TLabel;
    lbSicoobArquivoCertificado: TLabel;
    lbSicoobArquivoChavePrivada: TLabel;
    lbSicoobChavePix: TLabel;
    lbSicoobClientID: TLabel;
    lbSicoobTokenSandbox: TLabel;
    lbSicoobErroCertificado: TLabel;
    lbSicoobErroChavePrivada: TLabel;
    lbSicoobExtrairCertificado: TLabel;
    lbSicoobExtrairChaveCertificadoArqPFX: TLabel;
    lbSicoobExtrairChaveCertificadoSenhaPFX: TLabel;
    lbSicoobTipoChave: TLabel;
    lbSicrediArqCertificado: TLabel;
    lbPagSeguroArqCertificado: TLabel;
    lbCieloArqCertificado: TLabel;
    lbSicrediArqChavePrivada: TLabel;
    lbPagSeguroArqChavePrivada: TLabel;
    lbCieloArqChavePrivada: TLabel;
    lbSicrediChavePIX: TLabel;
    lbPagSeguroChavePIX: TLabel;
    lbSicrediClientID: TLabel;
    lbPagSeguroClientID: TLabel;
    lbSicrediClientSecret: TLabel;
    lbPagSeguroClientSecret: TLabel;
    lbSicrediErroCertificado: TLabel;
    lbPagSeguroErroCertificado: TLabel;
    lbCieloErroCertificado: TLabel;
    lbSicrediErroChavePrivada: TLabel;
    lbPagSeguroErroChavePrivada: TLabel;
    lbCieloErroChavePrivada: TLabel;
    lbSicrediGerarCSR: TLabel;
    lbSicrediGerarChavePrivada: TLabel;
    lbSicrediGerarCSREmail: TLabel;
    lbSicrediTipoChave: TLabel;
    lbPagSeguroTipoChave: TLabel;
    lbCriarRecorrenciaNomeDevedor: TLabel;
    lbCriarRecorrenciaObjeto: TLabel;
    lbCriarRecorrenciaTxID: TLabel;
    lConsultarPixE2eid1: TLabel;
    lConsultarPixE2eid2: TLabel;
    lbCancelarCobrancaTxID: TLabel;
    lbPagSeguroTxID: TLabel;
    lbConsultarRecorrenciaIdRec: TLabel;
    lCPFCPNJ2: TLabel;
    lbCriarRecorrenciaCPFCNPJ: TLabel;
    lbConsultarRecorrenciasDoc: TLabel;
    lbCriarRecorrenciaContrato: TLabel;
    lbCriarCobRTxID: TLabel;
    lFim1: TLabel;
    lbConsultarRecorrenciasFim: TLabel;
    lInicio1: TLabel;
    lbConsultarRecorrenciasInicio: TLabel;
    lItauAvisoChaveCertificadoDesabilitado: TLabel;
    Label43: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    lbQRDLocation: TLabel;
    Label6: TLabel;
    lConsultarDevolucaoPixE2eid2: TLabel;
    lConsultarDevolucaoPixE2eid3: TLabel;
    lConsultarDevolucaoPixE2eid5: TLabel;
    lConsultarDevolucaoPixIdentificadorDevolucao1: TLabel;
    lbCriarCobrancaImediata_SolicitacaoAoPagador: TLabel;
    lConsultarPixE2eid: TLabel;
    edtBBClientID: TEdit;
    edtConsultarPixE2eid: TEdit;
    edtBBClientSecret: TEdit;
    edtBBDevAppKey: TEdit;
    edtBBChavePIX: TEdit;
    imgBBErroChavePIX: TImage;
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
    lbConsultarRecorrenciasPagina: TLabel;
    lbConsultarRecorrenciasItensPorPagina: TLabel;
    lTokenTemporario: TLabel;
    mmCancelarCobR: TMemo;
    mmDesvincularLocRec: TMemo;
    mmConsultarLocationsRec: TMemo;
    mmConsultarLocationRec: TMemo;
    mmCriarLocationRec: TMemo;
    mmSolicitarRetentativa: TMemo;
    mmConsultarRecorrencia: TMemo;
    mmConsultarCobR: TMemo;
    mmConsultarCobsR: TMemo;
    mmConsultarSolicitacaoRec: TMemo;
    mmConsultarRecorrencias: TMemo;
    mmCancelarSolicitacaoRec: TMemo;
    mmCriarRecorrencia: TMemo;
    mmCriarCobR: TMemo;
    mmCriarSolicitacaoRec: TMemo;
    mmRevisarRecorrencia: TMemo;
    mmPagSeguroResp: TMemo;
    mConsultarCobrancaImediata: TMemo;
    mmCancelarCobranca: TMemo;
    mConsultarCobrancas: TMemo;
    mBBSimulaPagamento: TMemo;
    mItauCertificadoPEM: TMemo;
    mItauChavePrivadaPEM: TMemo;
    mItauTokenTemporario: TMemo;
    mmCobVCancelar: TMemo;
    mmCobVConsultar: TMemo;
    mmCobVConsultarLista: TMemo;
    mmItauRenovarCertificadoPEM: TMemo;
    mmPixPDVSimularPagto: TMemo;
    mmSicrediGerarCSR: TMemo;
    mmSicrediGerarChavePrivada: TMemo;
    mQRE: TMemo;
    mQRD: TMemo;
    mSolicitarDevolucaoPix: TMemo;
    mConsultarPix: TMemo;
    mConsultarPixRecebidos: TMemo;
    mConsultarDevolucaoPix: TMemo;
    mCriarCobrancaImediata: TMemo;
    OpenDialog1: TOpenDialog;
    pgTestesEndPointCobR: TPageControl;
    pgTestesEndPointSolicRec: TPageControl;
    pgTestesEndPointLocRec: TPageControl;
    pnCancelarCobR: TPanel;
    pnDesvincularLocRec: TPanel;
    pnDesvincularLocRecRodape: TPanel;
    pnConsultarLocationsRec: TPanel;
    pnConsultarLocationsRecRodape: TPanel;
    pnConsultarLocationRec: TPanel;
    pnConsultarLocationRecRodape: TPanel;
    pnCriarLocationRec: TPanel;
    pnLocationRecRodape: TPanel;
    pnSolicitarRetentativa: TPanel;
    pnCancelarCobRRodape: TPanel;
    pnSolicitarRetentativaRodape: TPanel;
    pnConsultarCobR: TPanel;
    pnConsultarCobRRodape: TPanel;
    pnConsultarCobsR: TPanel;
    pnConsultarCobsRRodape: TPanel;
    pnConsultarSolicitacaoRec: TPanel;
    pnConsultarSolicitacaoRecRodape: TPanel;
    pnCancelarSolicitacaoRec: TPanel;
    pnCancelarSolicitacaoRecRodape: TPanel;
    pnCriarRecorrenciaRodape: TPanel;
    pnConsultarRecorrenciaRodape: TPanel;
    pnConsultarRecorrenciasRodape: TPanel;
    pnConsultarRecorrencia: TPanel;
    pnConsultarRecorrencias: TPanel;
    pnCriarCobRRodape: TPanel;
    pnCriarSolicitacaoRecRodape: TPanel;
    pnRecCriarRecorrencia: TPanel;
    pgTestesEndPointRec: TPageControl;
    pnCriarSolicitacaoRec: TPanel;
    pnCobRCriar: TPanel;
    pnRevisarRecorrencia: TPanel;
    pnRevisarRecorrenciaRodape: TPanel;
    pcBradescoCertificados: TPageControl;
    pnBradescoChaveECert: TPanel;
    pnBradescoPFX: TPanel;
    pnBradescoCertificados: TPanel;
    pnBanrisul: TPanel;
    pnCielo: TPanel;
    pnC6Bank: TPanel;
    pnAppLess: TPanel;
    pnMercadoPago: TPanel;
    pnMateraSimularPagamento: TPanel;
    pcBBCertificados: TPageControl;
    pnBBCertificados: TPanel;
    pnAilos: TPanel;
    pnBBChaveECert: TPanel;
    pnBBPFX: TPanel;
    pnInter: TPanel;
    pnGate2All: TPanel;
    pnPixPDV: TPanel;
    pnGerenciaNet: TPanel;
    pnAutenticacaoManual: TPanel;
    pnBradesco: TPanel;
    pnPagSeguroLimpar: TPanel;
    pnPagSeguroCabecalho: TPanel;
    pcSimularPagamento: TPageControl;
    pnPixPDVSimularPagto: TPanel;
    pnPixPDVSimularPagtoLimpar: TPanel;
    pnMatera: TPanel;
    pnSicoobCredenciais: TPanel;
    pgSantander: TPageControl;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    pgSicoob: TPageControl;
    pgSicredi: TPageControl;
    pnConfPSPSicoob: TPanel;
    pnSantanderExtrairCertificado: TPanel;
    pConfPSPSantander: TPanel;
    pgTestesEndPointCobV: TPageControl;
    pItauEditCertificado1: TPanel;
    pnCobVCancelarParams: TPanel;
    pnCobVCancelarRodape: TPanel;
    pnCobVComprador: TPanel;
    pnCobVConsultarParams: TPanel;
    pnCobVConsultarRodape: TPanel;
    pnCobVDesconto: TPanel;
    pnCobVJuros: TPanel;
    pnCobVMulta: TPanel;
    pnConsultarCobrancaVencto: TPanel;
    pnItauRenovarCertificadoPEM: TPanel;
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
    pnFluxoDiv2: TPanel;
    pnFluxoDiv3: TPanel;
    pnFluxoDiv7: TPanel;
    pnFluxoPagto: TPanel;
    pnFluxoCopiaECola: TPanel;
    pnFluxoQRCode: TPanel;
    pnFluxoRodape: TPanel;
    pnFluxoStatus: TPanel;
    pnFluxoTotal: TPanel;
    pnFluxoTotalStr: TPanel;
    pnSicoobExtrairChaveCertificado: TPanel;
    pnSicrediCredenciais: TPanel;
    pnPagSeguroCredenciais: TPanel;
    pnSicrediGerarChaveCSR: TPanel;
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
    pConfPSPBB: TPanel;
    pBotoesConfiguracao: TPanel;
    pCriarCobrancaImediata: TPanel;
    rgBBTipoCertificado: TRadioGroup;
    rgBradescoTipoCertificado: TRadioGroup;
    sbArqLog: TSpeedButton;
    sbBanrisulAcharCertificadoPFX: TSpeedButton;
    sbBanrisulVerSenhaPFX: TSpeedButton;
    sbBBAcharArqCertificado: TSpeedButton;
    sbBradescoAcharArqCertificado: TSpeedButton;
    sbBBAcharChavePrivada: TSpeedButton;
    sbBradescoAcharChavePrivada: TSpeedButton;
    sbBBAcharPFX: TSpeedButton;
    btBBVerSenhaPFX: TSpeedButton;
    sbBradescoAcharPFX: TSpeedButton;
    sbBradescoVerSenhaPFX: TSpeedButton;
    sbConsultaCEP: TSpeedButton;
    sbCriarCobrancaImediata_GerarTxId: TSpeedButton;
    btCriarCobRGerarTxID: TSpeedButton;
    sbGerenciaNetAcharPFX: TSpeedButton;
    sbItauAcharArqCertificado: TSpeedButton;
    sbItauAcharArqChavePrivada: TSpeedButton;
    btInterAcharCertificado: TSpeedButton;
    btInterAcharChavePrivada: TSpeedButton;
    sbSantanderAcharCertificadoPFX: TSpeedButton;
    sbSantanderExtrairCertificadoPFX: TSpeedButton;
    sbSantanderExtrairCertificadoInfo: TSpeedButton;
    sbSantanderVerSenhaPFX: TSpeedButton;
    sbSantanderExtrairCertificadoVerSenhaPFX: TSpeedButton;
    sbSicoobAcharArqCertificado: TSpeedButton;
    sbSicoobAcharChavePrivada: TSpeedButton;
    sbSicrediAcharArqCertificado: TSpeedButton;
    sbPagSeguroAcharArqCertificado: TSpeedButton;
    sbCieloAcharArqCertificado: TSpeedButton;
    sbSicrediAcharChavePrivada: TSpeedButton;
    sbPagSeguroAcharChavePrivada: TSpeedButton;
    sbCieloAcharChavePrivada: TSpeedButton;
    sbSicrediAcharChavePrivada2: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    seCobrancaExpiracao: TSpinEdit;
    seConsultarCobrancaImediata_Revisao: TSpinEdit;
    seConsultarCobrancas_ItensPagina: TSpinEdit;
    edConsultarRecorrenciasItensPorPagina: TSpinEdit;
    seConsultarCobrancas_Pagina: TSpinEdit;
    gbLog: TGroupBox;
    gbProxy: TGroupBox;
    gbPSP: TGroupBox;
    gbRecebedor: TGroupBox;
    ImageList1: TImageList;
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
    edConsultarRecorrenciasPagina: TSpinEdit;
    seProxyPorta: TSpinEdit;
    seConsultarPixRecebidosPagina: TSpinEdit;
    seConsultarPixRecebidosItensPagina: TSpinEdit;
    seTimeout: TSpinEdit;
    lbSicoobExtrairChavePrivada: TLabel;
    btSicoobExtrairChaveCertificadoInfo: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    spCriarCobR: TSplitter;
    spLocationRec: TSplitter;
    tsEndPointLocRec: TTabSheet;
    tsConsultarLocationsRec: TTabSheet;
    tsDesvincularLocRec: TTabSheet;
    tsConsultarLocationRec: TTabSheet;
    tsCriarLocationRec: TTabSheet;
    tsSolicitarRetentativa: TTabSheet;
    tsEndPointCobR: TTabSheet;
    tsConsultarCobR: TTabSheet;
    tsConsultarCobsR: TTabSheet;
    tsConsultarSolicitacaoRec: TTabSheet;
    tsCancelarSolicitacaoRec: TTabSheet;
    tsCobRCriar: TTabSheet;
    tsCriarSolicitacaoRec: TTabSheet;
    tsEndPointSolicRec: TTabSheet;
    tsEndPointRec: TTabSheet;
    tsRevisarRecorrencia: TTabSheet;
    tsConsultarRecorrencia: TTabSheet;
    tsConsultarRecorrencias: TTabSheet;
    tsCriarRecorrencia: TTabSheet;
    tsAppLess: TTabSheet;
    tsBradescoChaveECertificado: TTabSheet;
    tsBradescoPFX: TTabSheet;
    tsC6Bank: TTabSheet;
    tsBanrisul: TTabSheet;
    tsGate2All: TTabSheet;
    tsMercadoPago: TTabSheet;
    tsCielo: TTabSheet;
    tsMateraSimularPagamento: TTabSheet;
    tsMatera: TTabSheet;
    tsBBChaveECertificado: TTabSheet;
    tsBBPFX: TTabSheet;
    tsAilos: TTabSheet;
    tsPIXPDVSimularPagamento: TTabSheet;
    tsInter: TTabSheet;
    tsPIXPDV: TTabSheet;
    tsBradesco: TTabSheet;
    tsPagSeguro: TTabSheet;
    tsPagSeguroSimularPagamento: TTabSheet;
    tsCancelarCobR: TTabSheet;
    tsSicoobCredenciais: TTabSheet;
    tsSantanderCredenciais: TTabSheet;
    tsSantanderExtrairCertificado: TTabSheet;
    tsSicoob: TTabSheet;
    tsSicoobExtrairChaveCertificado: TTabSheet;
    tsSicredi: TTabSheet;
    tsItauRenovarChaveCertificado: TTabSheet;
    tmConsultarDevolucao: TTimer;
    tsCancelarCobranca: TTabSheet;
    tmConsultarPagto: TTimer;
    tsCobVCancelarCobranca: TTabSheet;
    tsCobVConsultarCobranca: TTabSheet;
    tsCobVConsultarCobrancas: TTabSheet;
    tsCobVCriarCobranca: TTabSheet;
    tsFluxoPagto: TTabSheet;
    tsBBSimularPagamento: TTabSheet;
    tsSimularPagamento: TTabSheet;
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
    tsSicrediCredenciais: TTabSheet;
    tsSicrediGerarChaveCSR: TTabSheet;
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
    ACBrPSPGerenciaNet1: TACBrPSPGerenciaNet;
    tsGerenciaNet: TTabSheet;
    procedure ACBrPixCD1QuandoGravarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ACBrPSPBancoDoBrasil1QuandoReceberRespostaHttp(const AURL: String;
      const AMethod: String; RespHeaders: TStrings; var AResultCode: Integer;
      var RespostaHttp: String);
    procedure btAilosAcharCertificadoClick(Sender: TObject);
    procedure btAilosAcharCertificadoRootClick(Sender: TObject);
    procedure btAilosAcharchavePrivadaClick(Sender: TObject);
    procedure btBBVerSenhaPFXClick(Sender: TObject);
    procedure btC6BankAcharCertificadoClick(Sender: TObject);
    procedure btC6BankAcharChavePrivadaClick(Sender: TObject);
    procedure btCancelarCobRClick(Sender: TObject);
    procedure btCancelarCobRLimparClick(Sender: TObject);
    procedure btCancelarRecorrenciaClick(Sender: TObject);
    procedure btCancelarSolicitacaoRecClick(Sender: TObject);
    procedure btConsultarCobRClick(Sender: TObject);
    procedure btConsultarCobRLimparClick(Sender: TObject);
    procedure btConsultarCobsRClick(Sender: TObject);
    procedure btConsultarCobsRLimparClick(Sender: TObject);
    procedure btConsultarLocationsRecClick(Sender: TObject);
    procedure btConsultarRecorrenciaClick(Sender: TObject);
    procedure btConsultarRecorrenciaLimparClick(Sender: TObject);
    procedure btConsultarRecorrenciasClick(Sender: TObject);
    procedure btConsultarRecorrenciasLimparClick(Sender: TObject);
    procedure btConsultarSolicitacaoRecClick(Sender: TObject);
    procedure btCriarCobRClick(Sender: TObject);
    procedure btCriarCobRGerarTxIDClick(Sender: TObject);
    procedure btCriarCobRLimparClick(Sender: TObject);
    procedure btCriarCobRPreencherClick(Sender: TObject);
    procedure btCriarLocationRecClick(Sender: TObject);
    procedure btCriarRecorrenciaClick(Sender: TObject);
    procedure btCriarRecorrenciaPreencherClick(Sender: TObject);
    procedure btCriarSolicitacaoRecClick(Sender: TObject);
    procedure btCriarSolicitacaoRecLimparClick(Sender: TObject);
    procedure btDesvincularLocRecClick(Sender: TObject);
    procedure btInterAcharCertificadoClick(Sender: TObject);
    procedure btInterAcharChavePrivadaClick(Sender: TObject);
    procedure btConsultarLocationRecClick(Sender: TObject);
    procedure btMateraAcharArqCertificadoClick(Sender: TObject);
    procedure btMateraAcharChavePrivadaClick(Sender: TObject);
    procedure btPagSeguroLimparClick(Sender: TObject);
    procedure btPagSeguroPagarClick(Sender: TObject);
    procedure btPixPDVSimularPagtoClick(Sender: TObject);
    procedure btPixPDVSimularPagtoLimparClick(Sender: TObject);
    procedure btRevisarRecorrenciaClick(Sender: TObject);
    procedure btRevisarRecorrenciaLimparClick(Sender: TObject);
    procedure btSantanderExtrairPEMClick(Sender: TObject);
    procedure btBBSimulaPagamento_ExecutarClick(Sender: TObject);
    procedure btBBSimulaPagamento_LimparClick(Sender: TObject);
    procedure btCancelarCobrancaClick(Sender: TObject);
    procedure btCancelarCobrancaLimparMemoClick(Sender: TObject);
    procedure btCobVCancelarClick(Sender: TObject);
    procedure btCobVCancelarLimparClick(Sender: TObject);
    procedure btCobVConsultarClick(Sender: TObject);
    procedure btCobVConsultarLimparClick(Sender: TObject);
    procedure btCobVConsultarListaClick(Sender: TObject);
    procedure btCobVConsultarListaLimparClick(Sender: TObject);
    procedure btCobVCopiaEColaClick(Sender: TObject);
    procedure btCriarCobVClick(Sender: TObject);
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
    procedure btGerenciaNetBaixarConversorClick(Sender: TObject);
    procedure btItauGerarChavePrivadaClick(Sender: TObject);
    procedure btItauRenovarCertificadoClick(Sender: TObject);
    procedure btItauSolicitarCertificadoClick(Sender: TObject);
    procedure btItauValidarChaveCertificadoClick(Sender: TObject);
    procedure btLimparConsultarCobrancaImediataClick(Sender: TObject);
    procedure btLimparConsultarCobrancasClick(Sender: TObject);
    procedure btLimparConsultarDevolucaoPixClick(Sender: TObject);
    procedure btLimparConsultarPixClick(Sender: TObject);
    procedure btLimparConsultarPixRecebidosClick(Sender: TObject);
    procedure btLimparCriarCobrancaImediataClick(Sender: TObject);
    procedure btLimparSolicitarDevolucaoPixClick(Sender: TObject);
    procedure btQRDAnalisarClick(Sender: TObject);
    procedure btQRDColarClick(Sender: TObject);
    procedure btQRDGerarClick(Sender: TObject);
    procedure btLogLimparClick(Sender: TObject);
    procedure btQREAnalisarClick(Sender: TObject);
    procedure btQREGerarClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btQREColarClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btSicoobExtrairChaveCertificadoArqPFXClick(Sender: TObject);
    procedure btSicoobExtrairChaveCertificadoClick(Sender: TObject);
    procedure btSicoobExtrairChaveCertificadoInfoClick(Sender: TObject);
    procedure btSicoobExtrairChaveCertificadoVerSenhaPFXClick(Sender: TObject);
    procedure btSicrediGerarChaveCertificadoInfoClick(Sender: TObject);
    procedure btSicrediGerarChavePrivadaClick(Sender: TObject);
    procedure btSicrediGerarCSRClick(Sender: TObject);
    procedure btSolicitarDevolucaoPixClick(Sender: TObject);
    procedure btSolicitarRetentativaCobRClick(Sender: TObject);
    procedure btSolicitarRetentativaLimparClick(Sender: TObject);
    procedure cbBBVersaoAPIChange(Sender: TObject);
    procedure cbxAmbienteChange(Sender: TObject);
    procedure cbxPSPAtualChange(Sender: TObject);
    procedure edAilosArqsChange(Sender: TObject);
    procedure edAilosCertificadoExit(Sender: TObject);
    procedure edAilosCertificadoRootExit(Sender: TObject);
    procedure edAilosChavePIXChange(Sender: TObject);
    procedure edAilosChavePrivadaExit(Sender: TObject);
    procedure edBanrisulArqCertificadoPFXChange(Sender: TObject);
    procedure edBanrisulArqCertificadoPFXExit(Sender: TObject);
    procedure edBanrisulChavePIXChange(Sender: TObject);
    procedure edBanrisulSenhaCertificadoPFXExit(Sender: TObject);
    procedure edBBArqPFXChange(Sender: TObject);
    procedure edBBArqPFXExit(Sender: TObject);
    procedure edBBArqsChange(Sender: TObject);
    procedure edBBArqCertificadoExit(Sender: TObject);
    procedure edBBArqChavePrivadaExit(Sender: TObject);
    procedure edBBSenhaPFXExit(Sender: TObject);
    procedure edBradescoArqCertificadoExit(Sender: TObject);
    procedure edBradescoArqsChange(Sender: TObject);
    procedure edBradescoArqChavePrivadaExit(Sender: TObject);
    procedure edBradescoArqPFXChange(Sender: TObject);
    procedure edBradescoValidarPFXExit(Sender: TObject);
    procedure edBradescoChavePIXChange(Sender: TObject);
    procedure edC6BankCertificadoExit(Sender: TObject);
    procedure edC6BankChavePIXChange(Sender: TObject);
    procedure edC6BankArqsChange(Sender: TObject);
    procedure edC6BankChavePrivadaExit(Sender: TObject);
    procedure edCieloArqCertificadoChange(Sender: TObject);
    procedure edCieloArqCertificadoExit(Sender: TObject);
    procedure edCieloArqChavePrivadaChange(Sender: TObject);
    procedure edCieloArqChavePrivadaExit(Sender: TObject);
    procedure edCieloChavePIXChange(Sender: TObject);
    procedure edCriarRecorrenciaNomeDevedorChange(Sender: TObject);
    procedure edGerenciaNetChavePIXChange(Sender: TObject);
    procedure edGerenciaNetArqPFXExit(Sender: TObject);
    procedure edGerenciaNetArqPFXChange(Sender: TObject);
    procedure edGerenciaNetArqCertificadoExit(Sender: TObject);
    procedure edGerenciaNetArqCertificadoChange(Sender: TObject);
    procedure edInterCertificadoExit(Sender: TObject);
    procedure edInterChavePIXChange(Sender: TObject);
    procedure edInterArqsChange(Sender: TObject);
    procedure edInterChavePrivadaExit(Sender: TObject);
    procedure edMateraArqCertificadoExit(Sender: TObject);
    procedure edMateraArqChavePrivadaExit(Sender: TObject);
    procedure edMercadoPagoChavePIXChange(Sender: TObject);
    procedure edPagSeguroArqCertificadoExit(Sender: TObject);
    procedure edPagSeguroArqsChange(Sender: TObject);
    procedure edPagSeguroArqChavePrivadaExit(Sender: TObject);
    procedure edPagSeguroChavePIXChange(Sender: TObject);
    procedure edRevisarRecorrenciaNomeDevedorChange(Sender: TObject);
    procedure edSantanderArqCertificadoPFXChange(Sender: TObject);
    procedure edSicoobArqCertificadoExit(Sender: TObject);
    procedure edSicoobArqChavePrivadaExit(Sender: TObject);
    procedure edSicoobArqsChange(Sender: TObject);
    procedure edSicoobChavePIXChange(Sender: TObject);
    procedure edSicrediArqCertificadoExit(Sender: TObject);
    procedure edSicrediArqChavePrivadaExit(Sender: TObject);
    procedure edSicrediArqsChange(Sender: TObject);
    procedure edSicrediChavePIXChange(Sender: TObject);
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
    procedure edSantanderChavePIXChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbMateraSimularPagamentoClick(Sender: TObject);
    procedure lbMateraSimularPagamentoMouseEnter(Sender: TObject);
    procedure lbMateraSimularPagamentoMouseLeave(Sender: TObject);
    procedure mQREChange(Sender: TObject);
    procedure pgPrincipalChange(Sender: TObject);
    procedure pgPSPItauChaveCertificadoChange(Sender: TObject);
    procedure QuandoMudarDadosQRCode(Sender: TObject);
    procedure imgInfoMCCClick(Sender: TObject);
    procedure lURLTEFClick(Sender: TObject);
    procedure rgBBTipoCertificadoClick(Sender: TObject);
    procedure rgBradescoTipoCertificadoClick(Sender: TObject);
    procedure sbArqLogClick(Sender: TObject);
    procedure sbBanrisulAcharCertificadoPFXClick(Sender: TObject);
    procedure sbBanrisulVerSenhaPFXClick(Sender: TObject);
    procedure sbBBAcharArqCertificadoClick(Sender: TObject);
    procedure sbBBAcharChavePrivadaClick(Sender: TObject);
    procedure sbBBAcharPFXClick(Sender: TObject);
    procedure sbBradescoAcharArqCertificadoClick(Sender: TObject);
    procedure sbBradescoAcharChavePrivadaClick(Sender: TObject);
    procedure sbBradescoAcharPFXClick(Sender: TObject);
    procedure sbCieloAcharArqCertificadoClick(Sender: TObject);
    procedure sbCieloAcharChavePrivadaClick(Sender: TObject);
    procedure sbConsultaCEPClick(Sender: TObject);
    procedure sbCriarCobrancaImediata_GerarTxIdClick(Sender: TObject);
    procedure sbGerenciaNetAcharPFXClick(Sender: TObject);
    procedure sbItauAcharArqCertificadoClick(Sender: TObject);
    procedure sbItauAcharArqChavePrivadaClick(Sender: TObject);
    procedure sbPagSeguroAcharArqCertificadoClick(Sender: TObject);
    procedure sbPagSeguroAcharChavePrivadaClick(Sender: TObject);
    procedure sbSantanderExtrairCertificadoPFXClick(Sender: TObject);
    procedure sbSantanderAcharCertificadoPFXClick(Sender: TObject);
    procedure sbSantanderExtrairCertificadoInfoClick(Sender: TObject);
    procedure sbSantanderExtrairCertificadoVerSenhaPFXClick(Sender: TObject);
    procedure sbBradescoVerSenhaPFXClick(Sender: TObject);
    procedure sbSantanderVerSenhaPFXClick(Sender: TObject);
    procedure sbSicoobAcharArqCertificadoClick(Sender: TObject);
    procedure sbSicoobAcharChavePrivadaClick(Sender: TObject);
    procedure sbSicrediAcharChavePrivadaClick(Sender: TObject);
    procedure sbSicrediAcharArqCertificadoClick(Sender: TObject);
    procedure sbVerSenhaProxyClick(Sender: TObject);
    procedure tmConsultarDevolucaoTimer(Sender: TObject);
    procedure tmConsultarPagtoTimer(Sender: TObject);
    procedure tsSicrediGerarChaveCSRShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fFluxoDados: TFluxoPagtoDados;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;

    procedure InicializarBitmaps;
    procedure InicializarActivePages;
    procedure InicializarComponentesDefault;

    function GetInfoOpenSSL: String;
    function GetNomeArquivoConfiguracao: String;
    procedure AdicionarLinhaLog(AMensagem: String);
    procedure TratarException(Sender: TObject; E: Exception);

    procedure LigarAlertasdeErrosDeConfiguracao;
    procedure LigarAlertasdeErrosDeConfiguracaoPIXCD;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPItau;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPSicoob;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPSicredi;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPSantander;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPGerenciaNet;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPPagSeguro;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPInter;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPAilos;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPBB;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPBanrisul;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPC6Bank;

    procedure VerificarConfiguracao;
    procedure VerificarConfiguracaoPIXCD;
    procedure VerificarConfiguracaoPSPItau;

    procedure ValidarChaveCertificadoPSPItau;
    procedure ValidarChavePSPItau;
    procedure ValidarChavePSPSicredi;
    procedure ValidarChavePSPCielo;
    procedure ValidarChavePSPSicoob;
    procedure ValidarChavePSPPagSeguro;
    procedure ValidarChavePSPInter;
    procedure ValidarChavePSPAilos;
    procedure ValidarChavePSPBB;
    procedure ValidarChavePSPMatera;
    procedure ValidarChavePSPC6Bank;
    procedure ValidarChavePSPBradesco;

    procedure ValidarCertificadoPSPItau;
    procedure ValidarCertificadoPSPSicoob;
    procedure ValidarCertificadoPSPSicredi;
    procedure ValidarCertificadoPSPCielo;
    procedure ValidarCertificadoPSPSantander;
    procedure ValidarCertificadoPSPPagSeguro;
    procedure ValidarCertificadoPSPGerenciaNet;
    procedure ValidarCertificadoPSPBradesco;
    procedure ValidarCertificadoPSPInter;
    procedure ValidarCertificadoPSPAilos;
    procedure ValidarCertificadoRootPSPAilos;
    procedure ValidarCertificadoPSPMatera;
    procedure ValidarCertificadoPSPBB;
    procedure ValidarCertificadoPSPC6Bank;
    procedure ValidarPFXPSPBB;
    procedure ValidarPFXBanrisul;
    procedure ValidarPFXBradesco;

    procedure ConfigurarACBrPIXCD;
    procedure ConfigurarACBrPSPs;

    procedure LimparQRCodeEstatico;
    procedure PintarQRCodeEstatico;
    procedure PintarQRCodeDinamico;
    procedure AnalisarBRCode(aBRCode: TACBrBRCode);

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

    procedure DoAntesAutenticar(var aToken: String; var aValidadeToken: TDateTime);
    procedure DoDepoisAutenticar(const aToken: String; const aValidadeToken: TDateTime);

  public
    property FluxoDados: TFluxoPagtoDados read fFluxoDados;
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;
  end;

var
  Form1: TForm1;

implementation

uses
  {$IfDef FPC}
   fpjson, jsonparser, jsonscanner,
  {$Else}
    {$IFDEF DELPHIXE6_UP}JSON,{$ENDIF}
  {$EndIf}
  TypInfo, Clipbrd, IniFiles, DateUtils, synacode, synautil, pcnConversao,
  ACBrDelphiZXingQRCode, ACBrImage, ACBrValidador, ACBrPIXUtil, ACBrConsts,
  ACBrPIXSchemasCobV, ACBrPIXSchemasCobR, OpenSSLExt,
  ACBrJSON,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Compatibilidade;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InicializarBitmaps;
  InicializarActivePages;
  InicializarComponentesDefault;
  Application.OnException := TratarException;

  LerConfiguracao;
  VerificarConfiguracao;
  ReiniciarFluxo;
  AdicionarLinhaLog(GetInfoOpenSSL);
end;

procedure TForm1.imgInfoMCCClick(Sender: TObject);
begin
  OpenURL(CURL_MCC);
end;

procedure TForm1.lURLTEFClick(Sender: TObject);
begin
  OpenURL(CURL_ACBR);
end;

procedure TForm1.rgBBTipoCertificadoClick(Sender: TObject);
begin
  pnBBPFX.Visible := (rgBBTipoCertificado.ItemIndex = 0);
  pnBBChaveECert.Visible := (rgBBTipoCertificado.ItemIndex = 1);
end;

procedure TForm1.rgBradescoTipoCertificadoClick(Sender: TObject);
begin 
  pnBradescoPFX.Visible := (rgBradescoTipoCertificado.ItemIndex = 0);
  pnBradescoChaveECert.Visible := (rgBradescoTipoCertificado.ItemIndex = 1);
end;

procedure TForm1.sbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if (Trim(edtArqLog.Text) = '') then
  begin
    MessageDlg(ACBrStr('Arquivo de Log n�o informado'), mtError, [mbOK], 0);
    Exit;
  end;

  if pos(PathDelim,edtArqLog.Text) = 0 then
    AFileLog := ApplicationPath + edtArqLog.Text
  else
    AFileLog := edtArqLog.Text;

  if not FileExists(AFileLog) then
    MessageDlg(ACBrStr('Arquivo '+AFileLog+' n�o encontrado'), mtError, [mbOK], 0)
  else
    OpenURL(AFileLog);
end;

procedure TForm1.sbBanrisulAcharCertificadoPFXClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edBanrisulArqCertificadoPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarPFXBanrisul;
end;

procedure TForm1.sbBanrisulVerSenhaPFXClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbBanrisulVerSenhaPFX.Down then
    edBanrisulSenhaCertificadoPFX.EchoMode := emNormal
  else
    edBanrisulSenhaCertificadoPFX.EchoMode := emPassword;
  {$Else}
  if sbBanrisulVerSenhaPFX.Down then
    edBanrisulSenhaCertificadoPFX.PasswordChar := #0
  else
    edBanrisulSenhaCertificadoPFX.PasswordChar := '*';
  {$EndIf}
end;

procedure TForm1.sbBBAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBBArqCertificado.Text;
  if OpenDialog1.Execute then
    edBBArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPBB;
end;

procedure TForm1.sbBBAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBBArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edBBArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPBB;
end;

procedure TForm1.sbBBAcharPFXClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBBArqPFX.Text;
  if OpenDialog1.Execute then
    edBBArqPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarPFXPSPBB;
end;

procedure TForm1.sbBradescoAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBradescoArqCertificado.Text;
  if OpenDialog1.Execute then
    edBradescoArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPBradesco;
end;

procedure TForm1.sbBradescoAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edBradescoArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edBradescoArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPBradesco;
end;

procedure TForm1.sbBradescoAcharPFXClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edBradescoArqPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarPFXBradesco;
end;

procedure TForm1.sbCieloAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edCieloArqCertificado.Text;
  if OpenDialog1.Execute then
    edCieloArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPCielo;
end;

procedure TForm1.sbCieloAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edCieloArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edCieloArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPCielo;
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

procedure TForm1.sbGerenciaNetAcharPFXClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edGerenciaNetArqPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPGerenciaNet;
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

procedure TForm1.sbPagSeguroAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edPagSeguroArqCertificado.Text;
  if OpenDialog1.Execute then
    edPagSeguroArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPPagSeguro;
end;

procedure TForm1.sbPagSeguroAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edPagSeguroArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edPagSeguroArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPPagSeguro;
end;

procedure TForm1.sbSantanderExtrairCertificadoPFXClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edSantanderExtrairCertificadoPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
end;

procedure TForm1.sbSantanderAcharCertificadoPFXClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edSantanderArqCertificadoPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPSantander;
end;

procedure TForm1.sbSantanderExtrairCertificadoInfoClick(Sender: TObject);
begin
  MessageDlg(ACBrStr('PSP Santander solicita um arquivo em formato PEM para ' +
    'disponibilizar as credenciais Consumer Key e Consumer Secret.' + sLineBreak +
    'Ap�s a gera��o do certificado PEM a libera��o das credenciais deve ser ' +
    'feita no Internet Banking Santander!'), mtInformation, [mbOK], 0);
end;

procedure TForm1.sbSantanderExtrairCertificadoVerSenhaPFXClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbSantanderExtrairCertificadoVerSenhaPFX.Down then
    edSantanderExtrairCertificadoSenhaPFX.EchoMode := emNormal
  else
    edSantanderExtrairCertificadoSenhaPFX.EchoMode := emPassword;
  {$Else}
  if sbSantanderExtrairCertificadoVerSenhaPFX.Down then
    edSantanderExtrairCertificadoSenhaPFX.PasswordChar := #0
  else
    edSantanderExtrairCertificadoSenhaPFX.PasswordChar := '*';
  {$EndIf}
end;

procedure TForm1.sbBradescoVerSenhaPFXClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbBradescoVerSenhaPFX.Down then
    edBradescoSenhaPFX.EchoMode := emNormal
  else
    edBradescoSenhaPFX.EchoMode := emPassword;
  {$Else}
  if sbBradescoVerSenhaPFX.Down then
    edBradescoSenhaPFX.PasswordChar := #0
  else
    edBradescoSenhaPFX.PasswordChar := '*';
  {$EndIf}
end;

procedure TForm1.sbSantanderVerSenhaPFXClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbSantanderVerSenhaPFX.Down then
    edSantanderSenhaCertificadoPFX.EchoMode := emNormal
  else
    edSantanderSenhaCertificadoPFX.EchoMode := emPassword;
  {$Else}
  if sbSantanderVerSenhaPFX.Down then
    edSantanderSenhaCertificadoPFX.PasswordChar := #0
  else
    edSantanderSenhaCertificadoPFX.PasswordChar := '*';
  {$EndIf}
end;

procedure TForm1.sbSicoobAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edSicoobArqCertificado.Text;
  if OpenDialog1.Execute then
    edSicoobArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPSicoob;
end;

procedure TForm1.sbSicoobAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edSicoobArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edSicoobArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPSicoob;
end;

procedure TForm1.sbSicrediAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edSicrediArqCertificado.Text;
  if OpenDialog1.Execute then
    edSicrediArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPSicredi;
end;

procedure TForm1.sbSicrediAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edSicrediArqChavePrivada.Text;
  if OpenDialog1.Execute then
  begin
    edSicrediArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
    edSicrediGerarChavePrivada.Text := edSicrediArqChavePrivada.Text;
    mmSicrediGerarChavePrivada.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
  ValidarChavePSPSicredi;
end;

procedure TForm1.sbVerSenhaProxyClick(Sender: TObject);
begin
  {$IfDef FPC}
  if sbVerSenhaProxy.Down then
    edtProxySenha.EchoMode := emNormal
  else
    edtProxySenha.EchoMode := emPassword;
  {$Else}
  if sbVerSenhaProxy.Down then
    edtProxySenha.PasswordChar := #0
  else
    edtProxySenha.PasswordChar := '*';
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
      ShowMessage('Nenhuma cobran�a a ser consultada');
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

procedure TForm1.tsSicrediGerarChaveCSRShow(Sender: TObject);
var
  wSL: TStringList;
begin
  wSL := TStringList.Create;
  try
    if FileExists(edSicrediGerarChavePrivada.Text) then
    begin
      wSL.LoadFromFile(edSicrediGerarChavePrivada.Text);
      mmSicrediGerarChavePrivada.Text := wSL.Text;
    end;

    if FileExists(edSicrediGerarCSR.Text) then
    begin
      wSL.LoadFromFile(edSicrediGerarCSR.Text);
      mmSicrediGerarCSR.Text := wSL.Text;
    end;
  finally
    wSL.Free;
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

procedure TForm1.ACBrPSPBancoDoBrasil1QuandoReceberRespostaHttp(
  const AURL: String; const AMethod: String; RespHeaders: TStrings;
  var AResultCode: Integer; var RespostaHttp: String);
var
  jsRet, js: TACBrJSONObject;
  ja, jsArr: TACBrJSONArray;
  I: Integer;

  function GetDetalhesPagador(aJson: TACBrJSONObject): String;
  var
    jPag: TACBrJSONObject;
  begin
    jPag := aJson.AsJSONObject['pagador'];
    if Assigned(jPag) then
      Result := aJson.AsString['infoPagador'] + ' ' + jPag.AsString['cpf'] +
        jPag.AsString['cnpj'] + ' - ' + jPag.AsString['nome'];
  end;

begin
  if (AMethod = ChttpMethodGET) and (AResultCode = HTTP_OK) and (Pos(cEndPointPix, AURL) > 0) then
  begin
    jsRet := TACBrJSONObject.Parse(String(RespostaHttp));
    jsArr :=  jsRet.AsJSONArray['pix'];
    try
      if Assigned(jsArr) and (jsArr.Count > 0) then
      begin
        ja := TACBrJSONArray.Create;

        for i := 0 to jsArr.Count - 1 do
        begin
          js := jsArr.ItemAsJSONObject[i];
          js.AddPair('infoPagador', GetDetalhesPagador(js));
          ja.AddElementJSONString(js.ToJSON);
        end;
        jsRet.AddPair('pix', ja);
      end
      else
        jsRet.AddPair('infoPagador', GetDetalhesPagador(jsRet));

      RespostaHttp := jsRet.ToJSON;
    finally
      jsRet.Free;
    end;
  end;
end;

procedure TForm1.btAilosAcharCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edAilosCertificado.Text;
  if OpenDialog1.Execute then
    edAilosCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPAilos;
end;

procedure TForm1.btAilosAcharCertificadoRootClick(Sender: TObject);
begin
  OpenDialog1.FileName := edAilosCertificadoRoot.Text;
  if OpenDialog1.Execute then
    edAilosCertificadoRoot.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPAilos;
end;

procedure TForm1.btAilosAcharchavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edAilosChavePrivada.Text;
  if OpenDialog1.Execute then
    edAilosChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPAilos;
end;

procedure TForm1.btBBVerSenhaPFXClick(Sender: TObject);
begin
  {$IfDef FPC}
  if btBBVerSenhaPFX.Down then
    edBBSenhaPFX.EchoMode := emNormal
  else
    edBBSenhaPFX.EchoMode := emPassword;
  {$Else}
  if btBBVerSenhaPFX.Down then
    edBBSenhaPFX.PasswordChar := #0
  else
    edBBSenhaPFX.PasswordChar := '*';
  {$EndIf}
end;

procedure TForm1.btC6BankAcharCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edC6BankCertificado.Text;
  if OpenDialog1.Execute then
    edC6BankCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPC6Bank;
end;

procedure TForm1.btC6BankAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edC6BankChavePrivada.Text;
  if OpenDialog1.Execute then
    edC6BankChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPC6Bank;
end;

procedure TForm1.btCancelarCobRClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmCancelarCobR.Lines.Clear;

  with ACBrPixCD1.PSP.epCobR do
  begin
    CobRRevisada.Clear;
    CobRRevisada.status := srcCANCELADA;

    if RevisarCobranca(edCancelarCobRTxID.Text) then
      mmCancelarCobR.Lines.Text := FormatarJSON(CobRGerada.AsJSON)
    else
      mmCancelarCobR.Lines.Text := FormatarJSON(Problema.AsJSON);
  end;
end;

procedure TForm1.btCancelarCobRLimparClick(Sender: TObject);
begin
  mmCancelarCobR.Lines.Clear;
end;

procedure TForm1.btCancelarRecorrenciaClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmRevisarRecorrencia.Lines.Clear;

  ACBrPixCD1.PSP.epRec.RecorrenciaRevisada.Clear;
  ACBrPixCD1.PSP.epRec.RecorrenciaRevisada.status := strCANCELADA;

  if ACBrPixCD1.PSP.epRec.RevisarRecorrencia(Trim(edRevisarRecorrenciaIdRec.Text)) then
    mmRevisarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.RecorrenciaGerada.AsJSON)
  else
    mmRevisarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.Problema.AsJSON);
end;

procedure TForm1.btCancelarSolicitacaoRecClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmCancelarSolicitacaoRec.Lines.Clear;

  ACBrPixCD1.PSP.epSolicRec.SolicitacaoRevisada.Clear;
  ACBrPixCD1.PSP.epSolicRec.SolicitacaoRevisada.status := ssrCANCELADA;

  if ACBrPixCD1.PSP.epSolicRec.RevisarSolicitacao(Trim(edCancelarSolicitacaoRecIdSolicRec.Text)) then
    mmCancelarSolicitacaoRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epSolicRec.SolicitacaoGerada.AsJSON)
  else
    mmCancelarSolicitacaoRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epSolicRec.Problema.AsJSON);
end;

procedure TForm1.btConsultarCobRClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmConsultarCobR.Lines.Clear;
  if ACBrPixCD1.PSP.epCobR.ConsultarCobranca(edConsultarCobRTxID.Text) then
    mmConsultarCobR.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobR.CobRCompleta.AsJSON)
  else
    mmConsultarCobR.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobR.Problema.AsJSON);
end;

procedure TForm1.btConsultarCobRLimparClick(Sender: TObject);
begin
  mmConsultarCobR.Lines.Clear;
end;

procedure TForm1.btConsultarCobsRClick(Sender: TObject);
var
  Ok: Boolean;
begin
  VerificarConfiguracao;
  mmConsultarCobsR.Lines.Clear;

  Ok := ACBrPixCD1.PSP.epCobR.ConsultarCobrancas(
          StartOfTheDay(edConsultarCobsRInicio.DateTime),
          EndOfTheDay(edConsultarCobsRFim.DateTime),
          Trim(edConsultarCobsRIdRec.Text),
          OnlyNumber(edConsultarCobsRDoc.Text),
          TACBrPIXStatusRegistroCobranca(cbConsultarCobsRStatus.ItemIndex),
          edConsultarCobsRConvenio.Text,
          edConsultarCobsRPagina.Value,
          edConsultarCobsRItensPorPagina.Value);

  if Ok then
  begin
    mmConsultarCobsR.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCobR.CobsRConsultadas.AsJSON);
    mmConsultarCobsR.Lines.Add('');
    mmConsultarCobsR.Lines.Add('Encontradas: ' + IntToStr(
      ACBrPixCD1.PSP.epCobR.CobsRConsultadas.cobsr.Count)+' Cobran�as');
  end
  else
    mmConsultarCobsR.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobR.Problema.AsJSON);
end;

procedure TForm1.btConsultarCobsRLimparClick(Sender: TObject);
begin
  mmConsultarCobsR.Lines.Clear;
end;

procedure TForm1.btConsultarLocationsRecClick(Sender: TObject);
var
  Ok: Boolean;
begin
  VerificarConfiguracao;
  mmConsultarLocationsRec.Lines.Clear;

  Ok := ACBrPixCD1.PSP.epLocRec.ConsultarLocations(
          StartOfTheDay(edConsultarRecorrenciasInicio.DateTime),
          EndOfTheDay(edConsultarRecorrenciasFim.DateTime),
          cbConsultarLocationsRecComIdLocRed.Checked,
          edConsultarRecorrenciasConvenio.Text,
          edConsultarRecorrenciasPagina.Value,
          edConsultarRecorrenciasItensPorPagina.Value);

  if Ok then
  begin
    mmConsultarLocationsRec.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epLocRec.LocationsConsultadas.AsJSON);
    mmConsultarLocationsRec.Lines.Add('');
    mmConsultarLocationsRec.Lines.Add('Encontradas: ' + IntToStr(
      ACBrPixCD1.PSP.epLocRec.LocationsConsultadas.loc.Count)+' Locations');
  end
  else
    mmConsultarLocationsRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epLocRec.Problema.AsJSON);
end;

procedure TForm1.btConsultarRecorrenciaClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmConsultarRecorrencia.Lines.Clear;
  if ACBrPixCD1.PSP.epRec.ConsultarRecorrencia(edConsultarRecorrenciaIdRec.Text, edConsultarRecorrenciaTxId.Text) then
    mmConsultarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.RecorrenciaCompleta.AsJSON)
  else
    mmConsultarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.Problema.AsJSON);
end;

procedure TForm1.btConsultarRecorrenciaLimparClick(Sender: TObject);
begin
  mmConsultarRecorrencia.Lines.Clear;
end;

procedure TForm1.btConsultarRecorrenciasClick(Sender: TObject);
var
  Ok: Boolean;
begin
  VerificarConfiguracao;
  mmConsultarRecorrencias.Lines.Clear;

  Ok := ACBrPixCD1.PSP.epRec.ConsultarRecorrencias(
          StartOfTheDay(edConsultarRecorrenciasInicio.DateTime),
          EndOfTheDay(edConsultarRecorrenciasFim.DateTime),
          OnlyNumber(edConsultarRecorrenciasDoc.Text),
          cbConsultarRecorrenciasComLocation.Checked,
          TACBrPIXStatusRecorrencia(cbConsultarRecorrenciasStatus.ItemIndex),
          edConsultarRecorrenciasConvenio.Text,
          edConsultarRecorrenciasPagina.Value,
          edConsultarRecorrenciasItensPorPagina.Value);

  if Ok then
  begin
    mmConsultarRecorrencias.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epRec.RecorrenciasConsultadas.AsJSON);
    mmConsultarRecorrencias.Lines.Add('');
    mmConsultarRecorrencias.Lines.Add('Encontradas: ' + IntToStr(
      ACBrPixCD1.PSP.epRec.RecorrenciasConsultadas.recs.Count)+' Recorrencias');
  end
  else
    mmConsultarRecorrencias.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.Problema.AsJSON);
end;

procedure TForm1.btConsultarRecorrenciasLimparClick(Sender: TObject);
begin
  mmConsultarRecorrencias.Lines.Clear;
end;

procedure TForm1.btConsultarSolicitacaoRecClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmConsultarSolicitacaoRec.Lines.Clear;
  if ACBrPixCD1.PSP.epSolicRec.ConsultarSolicitacao(edConsultarSolicitacaoRecIdSolicRec.Text) then
    mmConsultarSolicitacaoRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epSolicRec.SolicitacaoCompleta.AsJSON)
  else
    mmConsultarSolicitacaoRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epSolicRec.Problema.AsJSON);
end;

procedure TForm1.btCriarCobRClick(Sender: TObject);
var
  wValor: Double;
  wErros: TStringList;
  wConta, wIdRec, wTxId: String;
  wTipoConta: TACBrPIXTipoConta;
begin
  VerificarConfiguracao;
  mmCriarCobR.Lines.Clear;

  wErros := TStringList.Create;
  try
    wTxId := Trim(edCriarCobRTxID.Text);
    wIdRec := Trim(edCriarCobRIdRec.Text);
    wConta := Trim(edCriarCobRConta.Text);
    wValor := StrToFloatDef(edCriarCobRValor.Text, 0);
    wTipoConta := TACBrPIXTipoConta(cbCriarCobRTipoConta.ItemIndex);
    if EstaVazio(wIdRec) then
      wErros.Add('idRec deve ser informado');
    if EstaZerado(wValor) then
      wErros.Add('Valor deve ser informado');
    if (wTipoConta = ptcNENHUM) then
      wErros.Add('Tipo Conta deve ser informado');
    if EstaVazio(wConta) then
      wErros.Add('Conta deve ser informada');
    if (edCriarCobRVencimento.DateTime < Now) then
      wErros.Add('Vencimento inv�lido');

    if NaoEstaZerado(wErros.Count) then
      raise Exception.Create('Erro ao criar Cobran�a:' + sLineBreak + wErros.Text);
  finally
    wErros.Free;
  end;

  with ACBrPixCD1.PSP.epCobR.CobRSolicitada do
  begin
    Clear;
    idRec := wIdRec;
    infoAdicional := Trim(edCriarCobRInfoAdicional.Text);
    calendario.dataDeVencimento := edCriarCobRVencimento.DateTime;
    valor.original := wValor;
    recebedor.conta := wConta;
    recebedor.tipoConta := wTipoConta;
    recebedor.agencia := Trim(edCriarCobRAgencia.Text);
    devedor.email := Trim(edCriarCobREmail.Text);
    devedor.logradouro := Trim(edCriarCobRLogradouro.Text);
    devedor.cidade := Trim(edCriarCobRCidade.Text);
    devedor.uf := Trim(edCriarCobRUF.Text);
    devedor.cep := Trim(edCriarCobRCEP.Text);
  end;

  if ACBrPixCD1.PSP.epCobR.CriarCobranca(wTxId) then
    mmCriarCobR.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobR.CobRSolicitada.AsJSON)
  else
    mmCriarCobR.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobR.Problema.AsJSON);
end;

procedure TForm1.btCriarCobRGerarTxIDClick(Sender: TObject);
begin
  edCriarCobRTxID.Text := CriarTxId;
end;

procedure TForm1.btCriarCobRLimparClick(Sender: TObject);
begin
  mmCriarCobR.Lines.Clear;
end;

procedure TForm1.btCriarCobRPreencherClick(Sender: TObject);
begin
  edCriarCobRInfoAdicional.Text := 'Streaming de Musica';
  edCriarCobRVencimento.DateTime := IncDay(Now, 1);
  edCriarCobRValor.Text := '9,99';
  cbCriarCobRTipoConta.ItemIndex := 1;
  edCriarCobRAgencia.Text := '9708';
  edCriarCobRConta.Text := '012682';
  edCriarCobRCEP.Text := '89256-140';
  edCriarCobRCidade.Text := 'Uberlandia';
  edCriarCobRUF.Text := 'MG';
  edCriarCobRLogradouro.Text := 'Alameda Franco 1056';
  edCriarCobREmail.Text := 'sebastiao.tavares@mail.com';
end;

procedure TForm1.btCriarLocationRecClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmCriarLocationRec.Lines.Clear;

  if ACBrPixCD1.PSP.epLocRec.CriarLocation then
    mmCriarLocationRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epLocRec.LocationGerada.AsJSON)
  else
    mmCriarLocationRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epLocRec.Problema.AsJSON);
end;

procedure TForm1.btCriarRecorrenciaClick(Sender: TObject);
var
  wDoc: String;
begin
  VerificarConfiguracao;
  mmCriarRecorrencia.Lines.Clear;

  with ACBrPixCD1.PSP.epRec.RecorrenciaSolicitada do
  begin
    Clear;
    vinculo.objeto := edCriarRecorrenciaObjeto.Text;
    vinculo.contrato := edCriarRecorrenciaContrato.Text;
    vinculo.devedor.nome := edCriarRecorrenciaNomeDevedor.Text;
    if NaoEstaVazio(vinculo.devedor.nome) then
    begin
      wDoc := OnlyNumber(edCriarRecorrenciaCPFCNPJDevedor.Text);
      if EstaVazio(wDoc) then
        raise Exception.Create('Caso o Nome do Devedor seja Informado, e necess�rio informar CPF/CNPJ')
      else if (Length(wDoc) > 11) then
        vinculo.devedor.cnpj := wDoc
      else
        vinculo.devedor.cpf := wDoc;
    end;
    
    calendario.dataFinal := edCriarRecorrenciaDataFinal.DateTime;
    calendario.dataInicial := edCriarRecorrenciaDataInicial.DateTime;
    calendario.periodicidade := TACBrPIXPeriodicidade(cbCriarRecorrenciaPeriodicidade.ItemIndex);

    valor.valorRec := StrToFloatDef(edCriarRecorrenciaValor.Text, 0);
    politicaRetentativa := TACBrPIXRetentativa(cbCriarRecorrenciaPoliticaRetentativa.ItemIndex);
    if NaoEstaZerado(edCriarRecorrenciaLoc.Value) then
      loc := edCriarRecorrenciaLoc.Value;
    ativacao.dadosJornada.txid := edCriarRecorrenciaTxID.Text;
  end;

  if ACBrPixCD1.PSP.epRec.CriarRecorrencia then
    mmCriarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.RecorrenciaGerada.AsJSON)
  else
    mmCriarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.Problema.AsJSON);
end;

procedure TForm1.btCriarRecorrenciaPreencherClick(Sender: TObject);
begin
  edCriarRecorrenciaNomeDevedor.Text := 'Fulano de Tal';
  edCriarRecorrenciaCPFCNPJDevedor.Text := '91259268888';
  edCriarRecorrenciaContrato.Text := '63100862';
  edCriarRecorrenciaObjeto.Text := 'Servico de Streaming de Musica';
  edCriarRecorrenciaDataInicial.DateTime := IncMonth(Now, 1);
  edCriarRecorrenciaDataFinal.DateTime := IncMonth(Now, 4);
  cbCriarRecorrenciaPeriodicidade.ItemIndex := 2;
  edCriarRecorrenciaValor.Text := '1,00';
  cbCriarRecorrenciaPoliticaRetentativa.ItemIndex := 1;
  edCriarRecorrenciaLoc.Text := EmptyStr;
  edCriarRecorrenciaTxID.Text := EmptyStr;
end;

procedure TForm1.btCriarSolicitacaoRecClick(Sender: TObject);
var
  wDoc: String;
  wErro: TStringList;
  wSolicRec: TACBrPixEndPointSolicRec;
begin
  VerificarConfiguracao;
  mmCriarSolicitacaoRec.Lines.Clear;

  wErro := TStringList.Create;
  try
    if EstaVazio(edCriarSolicitacaoRecIdRec.Text) then
      wErro.Add('- idRec');
    if EstaVazio(edCriarSolicitacaoRecDoc.Text) then
      wErro.Add('- CPF / CNPJ');
    if EstaVazio(edCriarSolicitacaoRecConta.Text) then
      wErro.Add('- Conta');
    if EstaVazio(edCriarSolicitacaoRecISPBParticipante.Text) then
      wErro.Add('- ISPB');

    if NaoEstaVazio(wErro.Text) then
    begin
      ShowMessage('Campos obrigatorios:' + sLineBreak + wErro.Text);
      Exit;
    end;
  finally
    wErro.Free;
  end;

  wSolicRec := ACBrPixCD1.PSP.epSolicRec;
  wSolicRec.SolicitacaoSolicitada.Clear;
  wSolicRec.SolicitacaoSolicitada.idRec := Trim(edCriarSolicitacaoRecIdRec.Text);
  wSolicRec.SolicitacaoSolicitada.destinatario.conta := edCriarSolicitacaoRecConta.Text;
  wSolicRec.SolicitacaoSolicitada.destinatario.agencia := edCriarSolicitacaoRecAgencia.Text;
  wSolicRec.SolicitacaoSolicitada.destinatario.ispbParticipante := edCriarSolicitacaoRecISPBParticipante.Text;
  wSolicRec.SolicitacaoSolicitada.calendario.dataExpiracaoSolicitacao := edCriarSolicitacaoRecExpiracao.DateTime;
  wDoc := OnlyNumber(edCriarSolicitacaoRecDoc.Text);
  if (Length(wDoc) > 11) then
    wSolicRec.SolicitacaoSolicitada.destinatario.cnpj := wDoc
  else
    wSolicRec.SolicitacaoSolicitada.destinatario.cpf := wDoc;

  if ACBrPixCD1.PSP.epSolicRec.CriarSolicitacaoConfirmacao then
    mmCriarSolicitacaoRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epSolicRec.SolicitacaoGerada.AsJSON)
  else
    mmCriarSolicitacaoRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epSolicRec.Problema.AsJSON);
end;

procedure TForm1.btCriarSolicitacaoRecLimparClick(Sender: TObject);
begin
  mmCriarSolicitacaoRec.Lines.Clear;
end;

procedure TForm1.btDesvincularLocRecClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmDesvincularLocRec.Lines.Clear;

  if ACBrPixCD1.PSP.epLocRec.DesvincularLocation(StrToIntDef(edDesvincularLocRecId.Text, 0)) then
    mmDesvincularLocRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epLocRec.LocationCompleta.AsJSON)
  else
    mmDesvincularLocRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epLocRec.Problema.AsJSON);
end;

procedure TForm1.btInterAcharCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edInterCertificado.Text;
  if OpenDialog1.Execute then
    edInterCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPInter;
end;

procedure TForm1.btInterAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edInterChavePrivada.Text;
  if OpenDialog1.Execute then
    edInterChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPInter;
end;

procedure TForm1.btConsultarLocationRecClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmConsultarLocationRec.Lines.Clear;
  if ACBrPixCD1.PSP.epLocRec.ConsultarLocation(StrToIntDef(edConsultarLocationRecId.Text, 0)) then
    mmConsultarLocationRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epLocRec.LocationCompleta.AsJSON)
  else
    mmConsultarLocationRec.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epLocRec.Problema.AsJSON);
end;

procedure TForm1.btMateraAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edMateraArqCertificado.Text;
  if OpenDialog1.Execute then
    edMateraArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificadoPSPMatera;
end;

procedure TForm1.btMateraAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edMateraArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edMateraArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePSPMatera;
end;

procedure TForm1.btPagSeguroLimparClick(Sender: TObject);
begin
  mmPagSeguroResp.Lines.Clear;
end;

procedure TForm1.btPagSeguroPagarClick(Sender: TObject);
var
  wTimerPagtoEnabled: Boolean;
begin
  VerificarConfiguracao;
  mmPagSeguroResp.Lines.Clear;
  if not (ACBrPixCD1.PSP is TACBrPSPPagSeguro) then
    raise Exception.Create(ACBrStr('PSP Configurado n�o � PagSeguro'));

  if (ACBrPixCD1.Ambiente <> ambTeste) then
    raise Exception.Create(ACBrStr('Fun��o s� dispon�vel em ambiente de Testes'));

  if EstaVazio(edPagSeguroTokenPay.Text) then
    raise Exception.Create(ACBrStr('Token Pay para simula��o de pagamento n�o foi informado'));

  wTimerPagtoEnabled := tmConsultarPagto.Enabled;
  tmConsultarPagto.Enabled := False;
  try
    try
      ACBrPSPPagSeguro1.TokenPay := edPagSeguroTokenPay.Text;
      ACBrPSPPagSeguro1.SimularPagamentoPIX(edPagSeguroTxID.Text);
      mmPagSeguroResp.Lines.Add('Result Code: '+IntToStr(ACBrPSPPagSeguro1.Http.ResultCode));
    except
      On E: Exception do
        mmPagSeguroResp.Lines.Add(E.Message);
    end;
  finally
    tmConsultarPagto.Enabled := wTimerPagtoEnabled;
  end;
end;

procedure TForm1.btPixPDVSimularPagtoClick(Sender: TObject);
var
  wTimerPagtoEnabled: Boolean;
begin
  VerificarConfiguracao;
  mmPixPDVSimularPagto.Lines.Clear;
                      
  if not (ACBrPixCD1.PSP is TACBrPSPPixPDV) then
    raise Exception.Create(ACBrStr('PSP Configurado n�o � PIXPDV'));

  if EstaVazio(edPixPDVSimularPagtoQRCodeID.Text) then
  begin
    MessageDlg('Preencha a QRCode_ID para Simular o Pagamento', mtError, [mbOK], 0);
    edPixPDVSimularPagtoQRCodeID.SetFocus;
    Exit;
  end;

  if (ACBrPixCD1.Ambiente <> ambTeste) then
    raise Exception.Create(ACBrStr('Fun��o dispon�vel apenas em ambiente de Testes'));

  wTimerPagtoEnabled := tmConsultarPagto.Enabled;
  tmConsultarPagto.Enabled := False;
  try
    try
      ACBrPSPPixPDV1.PostQrSimulaPagar(edPixPDVSimularPagtoQRCodeID.Text);
      mmPixPDVSimularPagto.Lines.Text := 'Result Code: ' +
        IntToStr(ACBrPSPPixPDV1.Http.ResultCode) + sLineBreak +
        ACBrPSPPixPDV1.Http.ResultString;
    except
      On E: Exception do
        mmPixPDVSimularPagto.Lines.Add(E.Message);
    end;
  finally
    tmConsultarPagto.Enabled := wTimerPagtoEnabled;
  end;
end;

procedure TForm1.btPixPDVSimularPagtoLimparClick(Sender: TObject);
begin
  mmPixPDVSimularPagto.Lines.Clear;
end;

procedure TForm1.btRevisarRecorrenciaClick(Sender: TObject);
var
  wDoc: String;
begin
  VerificarConfiguracao;
  mmRevisarRecorrencia.Lines.Clear;

  with ACBrPixCD1.PSP.epRec.RecorrenciaRevisada do
  begin
    Clear;
    calendario.dataInicial := edRevisarRecorrenciaDataInicial.DateTime;
    vinculo.devedor.nome := edRevisarRecorrenciaNomeDevedor.Text;
    if NaoEstaVazio(vinculo.devedor.nome) then
    begin
      wDoc := OnlyNumber(edRevisarRecorrenciaDocDevedor.Text);
      if EstaVazio(wDoc) then
        raise Exception.Create('Caso o Nome do Devedor seja Informado, e necess�rio informar CPF/CNPJ')
      else if (Length(wDoc) > 11) then
        vinculo.devedor.cnpj := wDoc
      else
        vinculo.devedor.cpf := wDoc;
    end;

    if NaoEstaZerado(edRevisarRecorrenciaLoc.Value) then
      loc := edRevisarRecorrenciaLoc.Value;
    ativacao.dadosJornada.txid := edRevisarRecorrenciaTxID.Text;
  end;

  if ACBrPixCD1.PSP.epRec.RevisarRecorrencia(Trim(edRevisarRecorrenciaIdRec.Text)) then
    mmRevisarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.RecorrenciaGerada.AsJSON)
  else
    mmRevisarRecorrencia.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epRec.Problema.AsJSON);
end;

procedure TForm1.btRevisarRecorrenciaLimparClick(Sender: TObject);
begin
  mmRevisarRecorrencia.Lines.Clear;
end;

procedure TForm1.btSantanderExtrairPEMClick(Sender: TObject);
var
  wArqPEM: String;
  wSL: TStringList;
begin
  if EstaVazio(edSantanderExtrairCertificadoPFX.Text) or
     (not FileExists(edSantanderExtrairCertificadoPFX.Text)) then
  begin
    MessageDlg(ACBrStr('Arquivo PFX n�o informado/existe'), mtError, [mbOK], 0);
    Exit;
  end;

  if EstaVazio(edSantanderExtrairCertificadoPEM.Text) then
  begin
    MessageDlg(ACBrStr('Arquivo de destino PEM n�o informado'), mtError, [mbOK], 0);
    Exit;
  end;

  wSL := TStringList.Create;
  try
    wArqPEM := edSantanderExtrairCertificadoPEM.Text;
    ACBrOpenSSLUtils1.LoadPFXFromFile(
      edSantanderExtrairCertificadoPFX.Text,
      edSantanderExtrairCertificadoSenhaPFX.Text);
    wSL.Text := ACBrOpenSSLUtils1.CertificateAsString;
    wSL.SaveToFile(wArqPEM);

    MessageDlg('Arquivo PEM gerado em: ' + wArqPEM, mtInformation, [mbOK], 0);
  finally
    wSL.Free;
  end;
end;

procedure TForm1.btBBSimulaPagamento_ExecutarClick(Sender: TObject);
var
  code: Integer;
  texto: String;
  wTimerPagtoEnabled: Boolean;
begin
  VerificarConfiguracao;
  mBBSimulaPagamento.Lines.Clear;
  if not (ACBrPixCD1.PSP is TACBrPSPBancoDoBrasil) then
    raise Exception.Create(ACBrStr('PSP Configurado, n�o � Banco do Brasil'));

  if (ACBrPixCD1.Ambiente <> ambTeste) then
    raise Exception.Create(ACBrStr('Fun��o s� dispon�vel em ambiente de Testes'));

  wTimerPagtoEnabled := tmConsultarPagto.Enabled;
  tmConsultarPagto.Enabled := False;
  try
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
  finally
    tmConsultarPagto.Enabled := wTimerPagtoEnabled;
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
    CobRevisada.Clear;
    CobRevisada.status := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;

    if RevisarCobrancaImediata(edCancelarCobrancaTxID.Text) then
    begin
      mmCancelarCobranca.Lines.Text := FormatarJSON(CobGerada.AsJSON);
      MostrarCobrancaEmLinhas('  Cobran�a', CobGerada, mmCancelarCobranca.Lines);
    end
    else
      mmCancelarCobranca.Lines.Text := FormatarJSON(Problema.AsJSON);
  end;
end;

procedure TForm1.btCancelarCobrancaLimparMemoClick(Sender: TObject);
begin
  mmCancelarCobranca.Lines.Clear;
end;

procedure TForm1.btCobVCancelarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmCobVCancelar.Lines.Clear;

  with ACBrPixCD1.PSP.epCobV do
  begin
    CobVRevisada.Clear;
    CobVRevisada.status := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;

    if RevisarCobranca(edCobVCancelarTxID.Text) then
      mmCobVCancelar.Lines.Text := FormatarJSON(CobVGerada.AsJSON)
    else
      mmCobVCancelar.Lines.Text := FormatarJSON(Problema.AsJSON);
  end;
end;

procedure TForm1.btCobVCancelarLimparClick(Sender: TObject);
begin
  mmCobVCancelar.Lines.Clear;
end;

procedure TForm1.btCobVConsultarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmCobVConsultar.Lines.Clear;
  if ACBrPixCD1.PSP.epCobV.ConsultarCobranca(edCobVConsultarTxID.Text, edCobVConsultarRevisao.Value) then
    mmCobVConsultar.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobV.CobVCompleta.AsJSON)
  else
    mmCobVConsultar.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobV.Problema.AsJSON);
end;

procedure TForm1.btCobVConsultarLimparClick(Sender: TObject);
begin
  mmCobVConsultar.Lines.Clear;
end;

procedure TForm1.btCobVConsultarListaClick(Sender: TObject);
var
  Ok: Boolean;
  i: Integer;
begin
  VerificarConfiguracao;
  mmCobVConsultarLista.Lines.Clear;

  Ok := ACBrPixCD1.PSP.epCobV.ConsultarCobrancas(
          StartOfTheDay(edCobVConsultarInicio.DateTime),
          EndOfTheDay(edCobVConsultarFim.DateTime),
          OnlyNumber(edCobVConsultarCPFCNPJ.Text),
          cbCobVConsultarLocation.Checked,
          TACBrPIXStatusCobranca(cbCobVConsultarStatus.ItemIndex),
          edCobVConsultarPagina.Value,
          edCobVConsultarItensPag.Value);

  if Ok then
  begin
    mmCobVConsultarLista.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobV.CobsVConsultadas.AsJSON);
    mmCobVConsultarLista.Lines.Add('');
    mmCobVConsultarLista.Lines.Add('Encontrado: '+IntToStr(ACBrPixCD1.PSP.epCobV.CobsVConsultadas.cobs.Count)+', Cobran�as');
    for i := 0 to ACBrPixCD1.PSP.epCobV.CobsVConsultadas.cobs.Count-1 do
      mmCobVConsultarLista.Lines.Add('');
  end
  else
    mmCobVConsultarLista.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobV.Problema.AsJSON);
end;

procedure TForm1.btCobVConsultarListaLimparClick(Sender: TObject);
begin
  mmCobVConsultarLista.Lines.Clear;
end;

procedure TForm1.btCobVCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edCobVCopiaECola.Text);
end;

procedure TForm1.btCriarCobVClick(Sender: TObject);
var
  s, wQrCode: String;
  Ok: Boolean;
begin
  VerificarConfiguracao;
  mCriarCobrancaImediata.Lines.Clear;

  with ACBrPixCD1.PSP.epCobV.CobVSolicitada do
  begin
    Clear;
    chave := ACBrPixCD1.PSP.ChavePIX;

    with calendario do
    begin
      dataDeVencimento := edCobVVencimento.DateTime;
      validadeAposVencimento := edCobVDiasPagar.Value;
    end;

    with devedor do
    begin
      s := Trim(edCobVCompradorNome.Text);
      if NaoEstaVazio(s) then
      begin
        nome := s;
        s := OnlyNumber(edCobVCompradorDoc.Text);
        if EstaVazio(s) then
          raise Exception.Create('Caso o Nome do Devedor seja Informado, � necess�rio informar CPF ou CNPJ')
        else if (Length(s) > 11) then
          cnpj := s
        else
          cpf := s;
      end;

      if (ACBrPixCD1.PSP = ACBrPSPMatera1) or (ACBrPixCD1.PSP = ACBrPSPPixPDV1) then
      begin
        uf := 'SP';
        cep := '99999999';
        cidade := 'Cidade';
        logradouro := 'Nome da Rua';
      end;
    end;

    with valor do
    begin
      original := StrToFloatDef(edCobVValor.Text, 0);

      multa.modalidade := TACBrPIXValoresModalidade(cbCobVMultaModalidade.ItemIndex);
      multa.valorPerc := StrToFloatDef(edCobVMultaValor.Text, 0);

      juros.modalidade := TACBrPIXJurosModalidade(cbCobVJurosModalidade.ItemIndex);
      juros.valorPerc := StrToFloatDef(edCobVJurosValor.Text, 0);

      desconto.modalidade := TACBrPIXDescontoModalidade(cbCobVDescModalidade.ItemIndex);

      if desconto.modalidade in [pdmValorFixo, pdmPercentual] then
      begin
        with desconto.descontosDataFixa.New do
        begin
          data := edCobVVencimento.DateTime;
          valorPerc := StrToFloatDef(edCobVDescValor.Text, 0);
        end;
      end
      else
        desconto.valorPerc := StrToFloatDef(edCobVDescValor.Text, 0);
    end;
  end;

  Ok := ACBrPixCD1.PSP.epCobV.CriarCobranca(CriarTxId);
  imCobVQRCode.Visible := Ok;
  edCobVCopiaECola.Visible := Ok;
  btCobVCopiaECola.Visible := Ok;
  lbCobVCopiaECola.Visible := Ok;

  if Ok then
  begin
    wQrCode := Trim(ACBrPixCD1.PSP.epCobV.CobVGerada.pixCopiaECola);
    if EstaVazio(wQrCode) then
      wQrCode := ACBrPixCD1.GerarQRCodeDinamico(ACBrPixCD1.PSP.epCobV.CobVGerada.loc.location);

    PintarQRCode(wQrCode, imCobVQRCode.Picture.Bitmap, qrUTF8BOM);
    edCobVCopiaECola.Text := wQrCode;
  end
  else
    mCriarCobrancaImediata.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCobV.Problema.AsJSON);
end;

procedure TForm1.btFluxoCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edFluxoCopiaECola.Text);
end;

procedure TForm1.btConsultarCobrancaImediataClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarCobrancaImediata.Lines.Clear;
  if ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata(
    edtConsultarCobrancaImediata_TxId.Text, seConsultarCobrancaImediata_Revisao.Value) then
  begin
    mConsultarCobrancaImediata.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.CobCompleta.AsJSON);
    MostrarCobrancaEmLinhas('  Cobranca',
      ACBrPixCD1.PSP.epCob.CobCompleta, mConsultarCobrancaImediata.Lines);
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

  Ok := ACBrPixCD1.PSP.epCob.ConsultarCobrancas(
          StartOfTheDay(dtConsultarCobrancas_Inicio.DateTime),
          EndOfTheDay(dtConsultarCobrancas_Fim.DateTime),
          OnlyNumber(edtConsultarCobrancas_CPFCNPJ.Text),
          chConsultarCobrancas_ComLocation.Checked,
          TACBrPIXStatusCobranca(cbxConsultarCobrancas_Status.ItemIndex),
          seConsultarCobrancas_Pagina.Value,
          seConsultarCobrancas_ItensPagina.Value);

  if Ok then
  begin
    mConsultarCobrancas.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.CobsConsultadas.AsJSON);
    mConsultarCobrancas.Lines.Add('');
    mConsultarCobrancas.Lines.Add('Encontrado: '+IntToStr(ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs.Count)+', Cobran�as');
    for i := 0 to ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs.Count-1 do
    begin
      mConsultarCobrancas.Lines.Add('');
      MostrarCobrancaEmLinhas('  Cob['+IntToStr(i)+']',
        ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs[i], mConsultarCobrancas.Lines);
    end;
  end
  else
    mConsultarCobrancas.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON);
end;

procedure TForm1.btConsultarPixRecebidosClick(Sender: TObject);
var
  Ok: Boolean;
  i: Integer;
begin
  VerificarConfiguracao;
  mConsultarPixRecebidos.Lines.Clear;

  Ok := ACBrPixCD1.PSP.epPix.ConsultarPixRecebidos(
          StartOfTheDay(dtConsultarPixRecebidosInicio.DateTime),
          EndOfTheDay(dtConsultarPixRecebidosFim.DateTime),
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
      MostrarPixEmLinhas('  Pix['+IntToStr(i)+']',
        ACBrPixCD1.PSP.epPix.PixConsultados.pix[i], mConsultarPixRecebidos.Lines);
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
    MostrarPixEmLinhas('  Pix', ACBrPixCD1.PSP.epPix.Pix, mConsultarPix.Lines);
  end
  else
    mConsultarPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TForm1.btConsultarDevolucaoPixClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarDevolucaoPix.Lines.Clear;
  if ACBrPixCD1.PSP.epPix.ConsultarDevolucaoPix(
    edtConsultarDevolucaoPix_e2eid.Text, edtConsultarDevolucaoPix_id.Text) then
  begin
    mConsultarDevolucaoPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.Devolucao.AsJSON);
    MostrarDevolucaoEmLinhas('  Devolucao', ACBrPixCD1.PSP.epPix.Devolucao, mConsultarDevolucaoPix.Lines);
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
    s := ACBrPixCD1.PSP.ChavePIX;
    chave := ACBrPixCD1.PSP.ChavePIX;
    calendario.expiracao := seCobrancaExpiracao.Value;

    solicitacaoPagador := edtCriarCobrancaImediata_SolicitacaoAoPagador.Text;

    s := Trim(edtCriarCobrancaImediata_NomeDevedor.Text);
    if (s <> '') then
    begin
      devedor.nome := s;
      s := OnlyNumber(edtCriarCobrancaImediata_CPF_CNPJ.Text);
      if (s = '') then
        raise Exception.Create('Caso o Nome do Devedor seja Informado, e necess�rio informar CPF ou CNPJ')
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
      qrcode := ACBrPixCD1.GerarQRCodeDinamico( ACBrPixCD1.PSP.epCob.CobGerada.location);
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
      ShowMessage('Cobran�a j� foi PAGA. Imposs�vel cancelar');
      Exit;
    end;

    if (MessageDlg('Deseja realmente Cancelar a Cobran�a?', mtConfirmation, mbOKCancel, 0) = mrNo) then
    begin
      tmConsultarPagto.Enabled := True;
      Exit;
    end;

    ACBrPixCD1.PSP.epCob.CobRevisada.status := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
    if ACBrPixCD1.PSP.epCob.RevisarCobrancaImediata(FluxoDados.TxID) then
    begin
      Sleep(1000);
      ConsultarCobranca;
      ShowMessage('Cobran�a cancelada com sucesso');
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
    ShowMessage('Informe a Descri��o do Item');
    edFluxoItemDescricao.SetFocus;
  end
  else if EstaVazio(edFluxoItemEAN.Text) then
  begin
    ShowMessage('Informe o C�digo EAN do Item');
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
  sl: TStringList;
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

        sl := TStringList.Create;
        try
          for I := 1 to Pred(gdFluxoItens.RowCount) do
          begin
            with infoAdicionais.New do
            begin
              nome := 'item_' + IntToStr(I);
              sl.Add('ean=' + gdFluxoItens.Cells[0, I]);
              sl.Add('item_title=' + gdFluxoItens.Cells[1, I]);
              sl.Add('quantity=1');
              sl.Add('sku=' + gdFluxoItens.Cells[0, I]);
              sl.Add('unit_price=' + gdFluxoItens.Cells[2, I]);
              valor := sl.Text;
            end;
          end;
        finally
          sl.Free;
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
      ShowMessage('Erro ao criar cobran�a: ' + sLineBreak +
        FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON));
    end;

    tmConsultarPagto.Enabled := True;
  finally
    HabilitarInterface(True);
  end;
end;

procedure TForm1.btGerenciaNetBaixarConversorClick(Sender: TObject);
begin
  shellexecute(0, 'open', 'https://pix.gerencianet.com.br/ferramentas/conversorGerencianet.exe', '', '', 1);
end;

procedure TForm1.btItauGerarChavePrivadaClick(Sender: TObject);
var
  aPrivateKey, aPublicKey: String;
begin
  if FileExists(edtItauArqChavePrivada2.Text) then
    if MessageDlg( 'A chave j� existe, deseja realmente sobreescrecer ?',
                   mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  ACBrOpenSSLUtils.GenerateKeyPair(aPrivateKey, aPublicKey, EmptyStr, bit2048);
  mItauChavePrivadaPEM.Lines.Text := ChangeLineBreak(aPrivateKey, sLineBreak);
  mItauChavePrivadaPEM.Lines.SaveToFile(edtItauArqChavePrivada2.Text);
end;

procedure TForm1.btItauRenovarCertificadoClick(Sender: TObject);
var
  c: String;
begin
  ValidarChavePSPItau;
  if imgItauErroChavePrivada.Visible  then
  begin
    pgPSPItauChaveCertificado.ActivePageIndex := 0;
    pgPSPItauGerarChaveCertificado.ActivePageIndex := 0;
    MessageDlg('Favor configurar a Chave Privada', mtWarning, [mbOK], 0);
    Abort;
  end;

  c := ACBrPSPItau1.RenovarCertificado;
  mmItauRenovarCertificadoPEM.Lines.Text := ChangeLineBreak(c, sLineBreak);
  mmItauRenovarCertificadoPEM.Lines.SaveToFile(edItauRenovarCertificadoArq.Text);
end;

procedure TForm1.btItauSolicitarCertificadoClick(Sender: TObject);
var
  wSL: TStringList;
  t, c, s: String;
  I: Integer;
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
    MessageDlg('Favor informar o Token tempor�rio', mtWarning, [mbOK], 0);
    Abort;
  end;

  wSL := TStringList.Create;
  try
    c := ACBrPSPItau1.SolicitarCertificado(t);
    wSL.Text := ChangeLineBreak(c, sLineBreak);
    wSL.SaveToFile(edtItauArqCertificado2.Text);

    // Pega Client Secret que est� na resposta
    for I := 0 to wSL.Count - 1 do
      if (Pos('Secret', wSL[I]) > 0) then
      begin
        s := wSL[I];
        wSL.Delete(I);
        Break;
      end;

    mItauCertificadoPEM.Lines.Text := wSL.Text;
    edtItauClientSecret.Text := Trim(Copy(s, Pos(':', s) + 1, Length(s)));
  finally
    wSL.Free;
  end;
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

procedure TForm1.btQRDAnalisarClick(Sender: TObject);
var
  qrd: TACBrPIXQRCodeDinamico;
begin
  qrd := TACBrPIXQRCodeDinamico.Create;
  try
    qrd.IgnoreErrors := True;
    qrd.AsString := Trim(mQRD.Lines.Text);
    AnalisarBRCode(qrd);
  finally
    qrd.Free;
  end;
end;

procedure TForm1.btQRDColarClick(Sender: TObject);
begin
  mQRD.CopyToClipboard;
end;

procedure TForm1.btQRDGerarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  PintarQRCodeDinamico;
end;

procedure TForm1.btLogLimparClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TForm1.btQREAnalisarClick(Sender: TObject);
var
  qre: TACBrPIXQRCodeEstatico;
begin
  qre := TACBrPIXQRCodeEstatico.Create;
  try
    qre.IgnoreErrors := True;
    qre.AsString := mQRE.Lines.Text;
    AnalisarBRCode(qre);
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

procedure TForm1.btSicoobExtrairChaveCertificadoArqPFXClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edSicoobExtrairChaveCertificadoArqPFX.Text := RemoverPathAplicacao(OpenDialog1.FileName);
end;

procedure TForm1.btSicoobExtrairChaveCertificadoClick(Sender: TObject);
var
  wArqPEM, wChave: String;
  wSL: TStringList;
begin
  if EstaVazio(edSicoobExtrairChaveCertificadoArqPFX.Text) or
     (not FileExists(edSicoobExtrairChaveCertificadoArqPFX.Text)) then
  begin
    MessageDlg(ACBrStr('Arquivo PFX n�o informado/existe'), mtError, [mbOK], 0);
    Exit;
  end;

  if EstaVazio(edSicoobExtrairChavePrivada.Text) then
  begin
    MessageDlg(ACBrStr('Arquivo de destino da Chave Privada n�o informado'), mtError, [mbOK], 0);
    Exit;
  end;

  if EstaVazio(edSicoobExtrairCertificado.Text) then
  begin
    MessageDlg(ACBrStr('Arquivo de destino do Certificado PEM n�o informado'), mtError, [mbOK], 0);
    Exit;
  end;

  wSL := TStringList.Create;
  try
    wChave := edSicoobExtrairChavePrivada.Text;
    wArqPEM := edSicoobExtrairCertificado.Text;

    ACBrOpenSSLUtils1.LoadPFXFromFile(
      edSicoobExtrairChaveCertificadoArqPFX.Text,
      edSicoobExtrairChaveCertificadoSenhaPFX.Text);

    // Salvando arquivo Chave Privada
    wSL.Text := ACBrOpenSSLUtils1.PrivateKeyAsString;
    wSL.SaveToFile(wChave);
    edSicoobArqChavePrivada.Text := wChave;

    // Salvando arquivo Certificado
    wSL.Text := ACBrOpenSSLUtils1.CertificateAsString;
    wSL.SaveToFile(wArqPEM);
    edSicoobArqCertificado.Text := wArqPEM;

    MessageDlg(
      '- Chave Privada gerada em: ' + wChave + sLineBreak +
      '- Certificado PEM gerado em: ' + wArqPEM, mtInformation, [mbOK], 0);
  finally
    wSL.Free;
  end;
end;

procedure TForm1.btSicoobExtrairChaveCertificadoInfoClick(Sender: TObject);
begin 
  MessageDlg(ACBrStr('Para utilizar o PSP Sicoob em ambiente de Produ��o � ' +
    'necess�rio extrair a Chave Privada e o Certificado PEM do Certificado PFX ' +
    sLineBreak + 'Ap�s esse procedimento envie o arquivo PEM para o Sicoob pelo ' +
    'Internet Banking e receba seu Client ID para utilizar nas requisi��es'),
    mtInformation, [mbOk], 0);
end;

procedure TForm1.btSicoobExtrairChaveCertificadoVerSenhaPFXClick(Sender: TObject);
begin
  {$IfDef FPC}
  if btSicoobExtrairChaveCertificadoVerSenhaPFX.Down then
    edSicoobExtrairChaveCertificadoSenhaPFX.EchoMode := emNormal
  else
    edSicoobExtrairChaveCertificadoSenhaPFX.EchoMode := emPassword;
  {$Else}
  if btSicoobExtrairChaveCertificadoVerSenhaPFX.Down then
    edSicoobExtrairChaveCertificadoSenhaPFX.PasswordChar := #0
  else
    edSicoobExtrairChaveCertificadoSenhaPFX.PasswordChar := '*';
  {$EndIf}
end;

procedure TForm1.btSicrediGerarChaveCertificadoInfoClick(Sender: TObject);
begin
  MessageDlg(ACBrStr('Para utilizar o PSP Sicredi em ambiente de Produ��o � ' +
    'necess�rio gerar uma Chave Privada e um CSR. ' + sLineBreak +
    'Ap�s esse procedimento envie esses arquivos para o Sicredi pelo Internet ' +
    'Banking e receba um novo arquivo certificado .CER para utilizar nas requisi��es'),
    mtInformation, [mbOk], 0);
end;

procedure TForm1.btSicrediGerarChavePrivadaClick(Sender: TObject);
var
  wPrivateKey, wPublicKey: String;
begin
  if FileExists(edSicrediGerarChavePrivada.Text) and
     (MessageDlg(ACBrStr('Chave Privada j� existe, deseja realmente sobreescrecer ?'),
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    Exit;

  ACBrOpenSSLUtils.GenerateKeyPair(wPrivateKey, wPublicKey, EmptyStr, bit2048);
  mmSicrediGerarChavePrivada.Lines.Text := ChangeLineBreak(wPrivateKey, sLineBreak);
  mmSicrediGerarChavePrivada.Lines.SaveToFile(edSicrediGerarChavePrivada.Text);
  edSicrediArqChavePrivada.Text := edSicrediGerarChavePrivada.Text;
end;

procedure TForm1.btSicrediGerarCSRClick(Sender: TObject);
var
  wErros, wCertificado: String;
begin
  if FileExists(edSicrediGerarCSR.Text) and
     (MessageDlg(ACBrStr('Certificado CSR j� existe, deseja realmente sobreescrecer ?'),
       mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    Exit;

  if EstaVazio(mmSicrediGerarChavePrivada.Text) then
  begin
    MessageDlg(ACBrStr('Antes de gerar o CSR � necess�rio gerar a Chave Privada'), mtInformation, [mbOK], 0);
    Exit;
  end;

  wErros := EmptyStr;
  if (Trim(edtRecebedorNome.Text) = EmptyStr) then
    wErros := sLineBreak + ACBrStr('- Campo Nome do Recebedor n�o informado');

  if EstaVazio(Trim(edSicrediGerarCSREmail.Text)) then
    wErros := wErros + sLineBreak + ACBrStr('- Campo E-mail n�o informado');

  if NaoEstaVazio(wErros) then
  begin
    MessageDlg('Erro ao gerar CSR:' + wErros, mtError, [mbOK], 0);
    Exit;
  end;

  ACBrOpenSSLUtils1.Clear;
  ACBrOpenSSLUtils1.LoadPrivateKeyFromString(mmSicrediGerarChavePrivada.Text);
  wCertificado := ACBrOpenSSLUtils1.CreateCertificateSignRequest(
                    'api-pix-' + OnlyAlphaNum(TiraAcentos(edtRecebedorNome.Text)),
                    'Confederacao Interestadual das Cooperativas Ligadas ao Sicredi',
                    'API PIX Sicredi',
                    'Porto Alegre',
                    'Rio Grande do Sul', 'BR',
                    edSicrediGerarCSREmail.Text, algSHA256);
  mmSicrediGerarCSR.Text := ChangeLineBreak(wCertificado, CRLF);
  mmSicrediGerarCSR.Lines.SaveToFile(edSicrediGerarCSR.Text);
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

procedure TForm1.btSolicitarRetentativaCobRClick(Sender: TObject);
var
  wTxId: String;
begin
  VerificarConfiguracao;
  mmSolicitarRetentativa.Lines.Clear;

  wTxId := Trim(edSolicitarRetentativaTxID.Text);
  if EstaVazio(wTxId) then
  begin
    MessageDlg('Informe o txId', mtError, [mbOK], 0);
    Exit;
  end;

  if (edSolicitarRetentativaLiquidacao.DateTime < Now) then
  begin
    MessageDlg('Data de Liquida��o precisa ser maior que data atual', mtError, [mbOK], 0);
    Exit;
  end;

  with ACBrPixCD1.PSP.epCobR do
  begin
    if SolicitarRetentativa(wTxId, edSolicitarRetentativaLiquidacao.DateTime) then
      mmSolicitarRetentativa.Lines.Text := FormatarJSON(CobRCompleta.AsJSON)
    else
      mmSolicitarRetentativa.Lines.Text := FormatarJSON(Problema.AsJSON);
  end;
end;

procedure TForm1.btSolicitarRetentativaLimparClick(Sender: TObject);
begin
  mmSolicitarRetentativa.Lines.Clear;
end;

procedure TForm1.cbBBVersaoAPIChange(Sender: TObject);
begin
  pnBBCertificados.Visible := (cbBBVersaoAPI.ItemIndex = 1);
  rgBBTipoCertificado.Visible := (cbBBVersaoAPI.ItemIndex = 1);
end;

procedure TForm1.cbxAmbienteChange(Sender: TObject);
var
  wProducao: Boolean;
begin
  wProducao := (cbxAmbiente.ItemIndex = 1);

  tsItauCertificado.Enabled := wProducao;
  lItauAvisoChaveCertificadoDesabilitado.Visible := not tsItauCertificado.Enabled;

  edSicoobTokenSandbox.Enabled := (not wProducao);
end;

procedure TForm1.cbxPSPAtualChange(Sender: TObject);
begin
  imgErrPSP.Visible := (cbxPSPAtual.ItemIndex < 0);
end;

procedure TForm1.edAilosArqsChange(Sender: TObject);
begin
  lbAilosErroChavePrivada.Caption := '';
  lbAilosErroCertificado.Caption := '';
end;

procedure TForm1.edAilosCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPAilos;
end;

procedure TForm1.edAilosCertificadoRootExit(Sender: TObject);
begin
  ValidarCertificadoRootPSPAilos;
end;

procedure TForm1.edAilosChavePIXChange(Sender: TObject);
begin
  cbAilosTipoChave.ItemIndex := Integer(DetectarTipoChave(edAilosChavePIX.Text));
  imAilosErroChavePIX.Visible := NaoEstaVazio(edAilosChavePIX.Text) and (cbAilosTipoChave.ItemIndex = 0);
end;

procedure TForm1.edAilosChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPAilos;
end;

procedure TForm1.edBanrisulArqCertificadoPFXChange(Sender: TObject);
begin
  lbBanrisulErroCertificadoPFX.Caption := EmptyStr;
end;

procedure TForm1.edBanrisulArqCertificadoPFXExit(Sender: TObject);
begin
  if NaoEstaVazio(edBanrisulSenhaCertificadoPFX.Text) then
    ValidarPFXBanrisul;
end;

procedure TForm1.edBanrisulChavePIXChange(Sender: TObject);
begin
  cbBanrisulTipoChave.ItemIndex := Integer(DetectarTipoChave(edBanrisulChavePIX.Text));
  imBanrisulErroChavePix.Visible := NaoEstaVazio(edBanrisulChavePIX.Text) and (cbBanrisulTipoChave.ItemIndex = 0);
end;

procedure TForm1.edBanrisulSenhaCertificadoPFXExit(Sender: TObject);
begin
  if NaoEstaVazio(edBanrisulArqCertificadoPFX.Text) then
    ValidarPFXBanrisul;
end;

procedure TForm1.edBBArqPFXChange(Sender: TObject);
begin
  lbBBErroPFX.Caption := EmptyStr;
end;

procedure TForm1.edBBArqPFXExit(Sender: TObject);
begin
  ValidarPFXPSPBB;
end;

procedure TForm1.edBBArqsChange(Sender: TObject);
begin
  lbBBErroChavePrivada.Caption := EmptyStr;
  lbBBErroCertificado.Caption := EmptyStr;
end;

procedure TForm1.edBBArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPBB;
end;

procedure TForm1.edBBArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPBB;
end;

procedure TForm1.edBBSenhaPFXExit(Sender: TObject);
begin
  if NaoEstaVazio(edBBSenhaPFX.Text) and (NaoEstaVazio(edBBArqPFX.Text)) then
    ValidarPFXPSPBB;
end;

procedure TForm1.edBradescoArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPBradesco;
end;

procedure TForm1.edBradescoArqsChange(Sender: TObject);
begin
  lbBradescoErroChavePrivada.Caption := EmptyStr;
  lbBradescoErroCertificado.Caption := EmptyStr;
end;

procedure TForm1.edBradescoArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPBradesco;
end;

procedure TForm1.edBradescoArqPFXChange(Sender: TObject);
begin
  lbBradescoErroPFX.Caption := EmptyStr;
end;

procedure TForm1.edBradescoValidarPFXExit(Sender: TObject);
begin
  if NaoEstaVazio(edBradescoSenhaPFX.Text) then
    ValidarPFXBradesco;
end;

procedure TForm1.edBradescoChavePIXChange(Sender: TObject);
begin
  cbBradescoTipoChave.ItemIndex := Integer(DetectarTipoChave(edBradescoChavePIX.Text));
  imBradescoErroChavePix.Visible := NaoEstaVazio(edBradescoChavePIX.Text) and (cbBradescoTipoChave.ItemIndex = 0);
end;

procedure TForm1.edC6BankCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPC6Bank;
end;

procedure TForm1.edC6BankChavePIXChange(Sender: TObject);
begin
  cbC6BankTipoChave.ItemIndex := Integer(DetectarTipoChave(edC6BankChavePIX.Text));
  imC6BankErroChavePix.Visible := NaoEstaVazio(edC6BankChavePIX.Text) and (cbC6BankTipoChave.ItemIndex = 0);
end;

procedure TForm1.edC6BankArqsChange(Sender: TObject);
begin
  lbC6BankErroChavePrivada.Caption := EmptyStr;
  lbC6BankErroCertificado.Caption := EmptyStr;
end;

procedure TForm1.edC6BankChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPC6Bank;
end;

procedure TForm1.edCieloArqCertificadoChange(Sender: TObject);
begin
  lbCieloErroChavePrivada.Caption := '';
  lbCieloErroCertificado.Caption := '';
end;

procedure TForm1.edCieloArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPCielo;
end;

procedure TForm1.edCieloArqChavePrivadaChange(Sender: TObject);
begin
  lbCieloErroChavePrivada.Caption := '';
  lbCieloErroCertificado.Caption := '';
end;

procedure TForm1.edCieloArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPCielo;
end;

procedure TForm1.edCieloChavePIXChange(Sender: TObject);
begin
  cbCieloTipoChave.ItemIndex := Integer(DetectarTipoChave(edCieloChavePIX.Text));
  imCieloErroChavePix.Visible := NaoEstaVazio(edCieloChavePIX.Text) and (cbCieloTipoChave.ItemIndex = 0);
end;

procedure TForm1.edCriarRecorrenciaNomeDevedorChange(Sender: TObject);
begin
  edCriarRecorrenciaCPFCNPJDevedor.Enabled := NaoEstaVazio(Trim(edCriarRecorrenciaNomeDevedor.Text));
end;

procedure TForm1.edGerenciaNetChavePIXChange(Sender: TObject);
begin
  cbGerenciaNetTipoChave.ItemIndex := Integer(DetectarTipoChave(edGerenciaNetChavePIX.Text));
  imGerenciaNetErroChavePix.Visible := NaoEstaVazio(edGerenciaNetChavePIX.Text) and (cbGerenciaNetTipoChave.ItemIndex = 0);
end;

procedure TForm1.edGerenciaNetArqPFXExit(Sender: TObject);
begin
  ValidarCertificadoPSPGerenciaNet;
end;

procedure TForm1.edGerenciaNetArqPFXChange(Sender: TObject);
begin
  lbGerenciaNetErroPFX.Caption := EmptyStr;
end;

procedure TForm1.edGerenciaNetArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPGerenciaNet;
end;

procedure TForm1.edPagSeguroArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPPagSeguro;
end;

procedure TForm1.edPagSeguroArqsChange(Sender: TObject);
begin
  lbPagSeguroErroChavePrivada.Caption := '';
  lbPagSeguroErroCertificado.Caption := '';
end;

procedure TForm1.edPagSeguroArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPPagSeguro;
end;

procedure TForm1.edPagSeguroChavePIXChange(Sender: TObject);
begin
  cbPagSeguroTipoChave.ItemIndex := Integer(DetectarTipoChave(edPagSeguroChavePIX.Text));
  imPagSeguroErroChavePIX.Visible := NaoEstaVazio(edPagSeguroChavePIX.Text) and (cbPagSeguroTipoChave.ItemIndex = 0);
end;

procedure TForm1.edRevisarRecorrenciaNomeDevedorChange(Sender: TObject);
begin
  edRevisarRecorrenciaDocDevedor.Enabled := NaoEstaVazio(Trim(edRevisarRecorrenciaNomeDevedor.Text));
end;

procedure TForm1.edSantanderArqCertificadoPFXChange(Sender: TObject);
begin
  lbSantanderErroCertificadoPFX.Caption := EmptyStr;
end;

procedure TForm1.edSicoobArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPSicoob;
end;

procedure TForm1.edSicoobArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPSicoob;
end;

procedure TForm1.edSicoobArqsChange(Sender: TObject);
begin
  lbSicoobErroChavePrivada.Caption := '';
  lbSicoobErroCertificado.Caption := '';
end;

procedure TForm1.edSicoobChavePIXChange(Sender: TObject);
begin
  cbSicoobTipoChave.ItemIndex := Integer(DetectarTipoChave(edSicoobChavePIX.Text));
  imSicoobErroChavePIX.Visible := (edSicoobChavePIX.Text <> '') and (cbSicoobTipoChave.ItemIndex = 0);
end;

procedure TForm1.edSicrediArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPSicredi;
end;

procedure TForm1.edSicrediArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPSicredi;
end;

procedure TForm1.edtRecebedorCEPExit(Sender: TObject);
begin
  if (not imgErrCEP.Visible) and (edtRecebedorCidade.Text = '') then
    sbConsultaCEP.Click;
end;

procedure TForm1.edGerenciaNetArqCertificadoChange(Sender: TObject);
begin
  lbGerenciaNetErroPFX.Caption := EmptyStr;
end;

procedure TForm1.edInterCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPInter;
end;

procedure TForm1.edInterChavePIXChange(Sender: TObject);
begin
  cbInterTipoChave.ItemIndex := Integer(DetectarTipoChave(edInterChavePIX.Text));
  imInterErroChavePix.Visible := NaoEstaVazio(edInterChavePIX.Text) and (cbInterTipoChave.ItemIndex = 0);
end;

procedure TForm1.edInterArqsChange(Sender: TObject);
begin
  lbInterErroChavePrivada.Caption := '';
  lbInterErroCertificado.Caption := '';
end;

procedure TForm1.edInterChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPInter;
end;

procedure TForm1.edMateraArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificadoPSPMatera;
end;

procedure TForm1.edMateraArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePSPMatera;
end;

procedure TForm1.edMercadoPagoChavePIXChange(Sender: TObject);
begin
  cbMercadoPagoTipoChave.ItemIndex := Integer(DetectarTipoChave(edMercadoPagoChavePIX.Text));
  imMercadoPagoErroChavePix.Visible := NaoEstaVazio(edMercadoPagoChavePIX.Text) and (cbMercadoPagoTipoChave.ItemIndex = 0);
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

procedure TForm1.edSantanderChavePIXChange(Sender: TObject);
begin
  cbSantanderTipoChave.ItemIndex := Integer(DetectarTipoChave(edSantanderChavePIX.Text));
  imSantanderErroChavePIX.Visible := (edSantanderChavePIX.Text <> '') and (cbSantanderTipoChave.ItemIndex = 0);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  AvaliarInterfaceFluxo;
end;

procedure TForm1.lbMateraSimularPagamentoClick(Sender: TObject);
begin
  OpenURL(CURL_MateraPagto);
end;

procedure TForm1.lbMateraSimularPagamentoMouseEnter(Sender: TObject);
begin
  lbMateraSimularPagamento.Font.Color := clBlue;
end;

procedure TForm1.lbMateraSimularPagamentoMouseLeave(Sender: TObject);
begin
  lbMateraSimularPagamento.Font.Color := clHighlight;
end;

procedure TForm1.edSicrediArqsChange(Sender: TObject);
begin
  lbSicrediErroChavePrivada.Caption := '';
  lbSicrediErroCertificado.Caption := '';
end;

procedure TForm1.edSicrediChavePIXChange(Sender: TObject);
begin
  cbSicrediTipoChave.ItemIndex := Integer(DetectarTipoChave(edSicrediChavePIX.Text));
  imSicrediErroChavePIX.Visible := (edSicrediChavePIX.Text <> '') and (cbSicrediTipoChave.ItemIndex = 0);
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
      mItauChavePrivadaPEM.Lines.Text := ACBrStr('Arquivo: '+a+'  n�o encontrado');

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
  if Assigned(mLog) then
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
  LigarAlertasdeErrosDeConfiguracaoPSPSicoob;
  LigarAlertasdeErrosDeConfiguracaoPSPSicredi;
  LigarAlertasdeErrosDeConfiguracaoPSPSantander;
  LigarAlertasdeErrosDeConfiguracaoPSPGerenciaNet;
  LigarAlertasdeErrosDeConfiguracaoPSPPagSeguro;
  LigarAlertasdeErrosDeConfiguracaoPSPInter;
  LigarAlertasdeErrosDeConfiguracaoPSPAilos;
  LigarAlertasdeErrosDeConfiguracaoPSPBB;
  LigarAlertasdeErrosDeConfiguracaoPSPBanrisul;
  LigarAlertasdeErrosDeConfiguracaoPSPC6Bank
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

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPSicoob;
begin
  edSicoobChavePIXChange(Nil);
  edSicoobArqsChange(Nil);
  ValidarChavePSPSicoob;
  ValidarCertificadoPSPSicoob;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPSicredi;
begin
  edSicrediChavePIXChange(Nil);
  edSicrediArqsChange(Nil);
  ValidarChavePSPSicredi;
  ValidarCertificadoPSPSicredi;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPSantander;
begin
  edSantanderChavePIXChange(Nil);
  ValidarCertificadoPSPSantander;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPGerenciaNet;
begin
  edGerenciaNetChavePIXChange(Nil);
  edGerenciaNetArqCertificadoChange(Nil);
  ValidarCertificadoPSPGerenciaNet;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPPagSeguro;
begin
  edPagSeguroChavePIXChange(Nil);
  edPagSeguroArqsChange(Nil);
  ValidarCertificadoPSPPagSeguro;
  ValidarChavePSPPagSeguro;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPInter;
begin
  edInterChavePIXChange(Nil);
  edInterArqsChange(Nil);
  ValidarCertificadoPSPInter;
  ValidarChavePSPInter;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPAilos;
begin
  edAilosChavePIXChange(Nil);
  edAilosArqsChange(Nil);
  ValidarCertificadoPSPAilos;
  ValidarCertificadoRootPSPAilos;
  ValidarChavePSPAilos;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPBB;
begin
  edtBBChavePIXChange(Nil);
  edBBArqsChange(Nil);
  ValidarCertificadoPSPBB;
  ValidarChavePSPBB;
  ValidarPFXPSPBB;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPBanrisul;
begin
  edBanrisulChavePIXChange(Nil);
  edBanrisulArqCertificadoPFXChange(Nil);
  ValidarPFXBanrisul;
end;

procedure TForm1.LigarAlertasdeErrosDeConfiguracaoPSPC6Bank;
begin
  edC6BankChavePIXChange(Nil);
  edC6BankArqsChange(Nil);
  ValidarCertificadoPSPC6Bank;
  ValidarChavePSPC6Bank;
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
    MessageDlg('Favor configurar as credenciais de acesso ao Ita�', mtWarning, [mbOK], 0);
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

procedure TForm1.ValidarChavePSPSicredi;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edSicrediArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbSicrediErroChavePrivada.Caption := e;
  imSicrediErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPCielo;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edCieloArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;
  lbCieloErroChavePrivada.Caption := e;
  imCieloErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPSicoob;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edSicoobArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbSicoobErroChavePrivada.Caption := e;
  lbSicoobErroChavePrivada.Visible := True;
  imSicoobErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPPagSeguro;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edPagSeguroArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbPagSeguroErroChavePrivada.Caption := e;
  imPagSeguroErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPInter;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edInterChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbInterErroChavePrivada.Caption := e;
  imInterErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPAilos;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edAilosChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbAilosErroChavePrivada.Caption := e;
  imAilosErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPBB;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edBBArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbBBErroChavePrivada.Caption := e;
  imBBErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPMatera;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edMateraArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbMateraErroChavePrivada.Caption := e;
  imMateraErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPC6Bank;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edC6BankChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbC6BankErroChavePrivada.Caption := e;
  imC6BankErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPBradesco;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edBradescoArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbBradescoErroChavePrivada.Caption := e;
  lbBradescoErroChavePrivada.Visible := True;
  imBradescoErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPSicoob;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edSicoobArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadCertificateFromFile(a);  // Verifica se o arquivo de Chave � v�lido
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbSicoobErroCertificado.Caption := e;
  lbSicoobErroCertificado.Visible := True;
  imSicoobErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPSicredi;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edSicrediArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadCertificateFromFile(a);  // Verifica se o arquivo de Chave � v�lido
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbSicrediErroCertificado.Caption := e;
  imSicrediErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPCielo;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edCieloArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbCieloErroCertificado.Caption := e;
  imCieloErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPSantander;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edSantanderArqCertificadoPFX.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o informado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else if EstaVazio(edSantanderSenhaCertificadoPFX.Text) then
    e := ACBrStr('Senha do Certificado PFX n�o informada')
  else
  begin
    try
      // Verifica se o arquivo PFX � v�lido
      ACBrOpenSSLUtils1.LoadPFXFromFile(a, edSantanderSenhaCertificadoPFX.Text);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbSantanderErroCertificadoPFX.Caption := e;
  imSantanderErroCertificadoPFX.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPPagSeguro;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edPagSeguroArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadCertificateFromFile(a);  // Verifica se o arquivo de Chave � v�lido
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbPagSeguroErroCertificado.Caption := e;
  imPagSeguroErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPGerenciaNet;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edGerenciaNetArqPFX.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPFXFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbGerenciaNetErroPFX.Caption := e;
  imGerenciaNetErroPFX.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPBradesco;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edBradescoArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbBradescoErroCertificado.Caption := e;
  lbBradescoErroCertificado.Visible := True;
  imBradescoErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPInter;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edInterCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbInterErroCertificado.Caption := e;
  imInterErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPAilos;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edAilosCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbAilosErroCertificado.Caption := e;
  imAilosErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoRootPSPAilos;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edAilosCertificadoRoot.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado');

  lbAilosErroCertificadoRoot.Caption := e;
  imAilosErroCertificadoRoot.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPMatera;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edMateraArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := 'Arquivo n�o especificado'
  else if (not FileExists(a)) then
    e := 'Arquivo n�o encontrado'
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbMateraErroCertificado.Caption := e;
  imMateraErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPBB;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edBBArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbBBErroCertificado.Caption := e;
  imBBErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarCertificadoPSPC6Bank;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edC6BankCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbC6BankErroCertificado.Caption := e;
  imC6BankErroCertificado.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarPFXPSPBB;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edBBArqPFX.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPFXFromFile(a, edBBSenhaPFX.Text);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbBBErroPFX.Caption := e;
  imBBErroPFX.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarPFXBanrisul;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edBanrisulArqCertificadoPFX.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o informado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else if EstaVazio(edBanrisulSenhaCertificadoPFX.Text) then
    e := ACBrStr('Senha do Certificado PFX n�o informada')
  else
  begin
    try
      // Verifica se o arquivo PFX � v�lido
      ACBrOpenSSLUtils1.LoadPFXFromFile(a, edBanrisulSenhaCertificadoPFX.Text);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbBanrisulErroCertificadoPFX.Caption := e;
  imBanrisulErroCertificadoPFX.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarPFXBradesco;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edBradescoArqPFX.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPFXFromFile(a, edBradescoSenhaPFX.Text);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbBradescoErroPFX.Caption := e;
  lbBradescoErroPFX.Visible := True;
  imBradescoErroPFX.Visible := (e <> 'OK');
end;

procedure TForm1.ValidarChavePSPItau;
var
  a, e: String;
begin
  a := AdicionarPathAplicacao(edtItauArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
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
    e := ACBrStr('Arquivo n�o especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo n�o encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadCertificateFromFile(a);  // Verifica se o arquivo de Chave � v�lido
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

    cbAutenticacaoManual.Checked := Ini.ReadBool('Autenticar', 'Manual', False);

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
    edBBArqChavePrivada.Text := Ini.ReadString('BancoBrasil', 'ArqChavePrivada', edBBArqChavePrivada.Text);
    edBBArqCertificado.Text := Ini.ReadString('BancoBrasil', 'ArqCertificado', edBBArqCertificado.Text);
    edBBArqPFX.Text := Ini.ReadString('BancoBrasil', 'ArqPFX', edBBArqPFX.Text);
    edBBSenhaPFX.Text := Ini.ReadString('BancoBrasil', 'SenhaPFX', edBBSenhaPFX.Text);
    cbBBVersaoAPI.ItemIndex := Ini.ReadInteger('BancoBrasil', 'VersaoAPI', cbBBVersaoAPI.ItemIndex);
    rgBBTipoCertificado.ItemIndex := Ini.ReadInteger('BancoBrasil', 'TipoCertificado', rgBBTipoCertificado.ItemIndex);
    cbBBVersaoAPIChange(Nil);
    rgBBTipoCertificadoClick(Nil);

    edtItauChavePIX.Text := Ini.ReadString('Itau', 'ChavePIX', '');
    edtItauClientID.Text := Ini.ReadString('Itau', 'ClientID', '');
    edtItauClientSecret.Text := Ini.ReadString('Itau', 'ClientSecret', '');
    edtItauArqChavePrivada.Text := Ini.ReadString('Itau', 'ArqChavePrivada', edtItauArqChavePrivada.Text);
    edtItauArqCertificado.Text := Ini.ReadString('Itau', 'ArqCertificado', edtItauArqCertificado.Text);

    edSantanderChavePIX.Text := Ini.ReadString('Santander', 'ChavePIX', '');
    edSantanderConsumerKey.Text := Ini.ReadString('Santander', 'ConsumerKey', '');
    edSantanderConsumerSecret.Text := Ini.ReadString('Santander', 'ConsumerSecret', '');
    edSantanderArqCertificadoPFX.Text := Ini.ReadString('Santander', 'ArqCertificadoPFX', '');
    edSantanderSenhaCertificadoPFX.Text := Ini.ReadString('Santander', 'SenhaCertificadoPFX', '');

    edSicrediChavePIX.Text := Ini.ReadString('Sicredi', 'ChavePIX', '');
    edSicrediClientID.Text := Ini.ReadString('Sicredi', 'ClientID', '');
    edSicrediClientSecret.Text := Ini.ReadString('Sicredi', 'ClientSecret', '');
    edSicrediArqChavePrivada.Text := Ini.ReadString('Sicredi', 'ArqChavePrivada', edSicrediArqChavePrivada.Text);
    edSicrediArqCertificado.Text := Ini.ReadString('Sicredi', 'ArqCertificado', edSicrediArqCertificado.Text);
    edSicrediGerarCSR.Text := Ini.ReadString('Sicredi', 'CertificadoCSR', edSicrediGerarCSR.Text);
    edSicrediGerarChavePrivada.Text := edSicrediArqChavePrivada.Text;

    edSicoobChavePIX.Text := Ini.ReadString('Sicoob', 'ChavePIX', '');
    edSicoobClientID.Text := Ini.ReadString('Sicoob', 'ClientID', '');
    edSicoobTokenSandbox.Text := Ini.ReadString('Sicoob', 'TokenSandbox', '');
    edSicoobArqChavePrivada.Text := Ini.ReadString('Sicoob', 'ArqChavePrivada', edSicoobArqChavePrivada.Text);
    edSicoobArqCertificado.Text := Ini.ReadString('Sicoob', 'ArqCertificado', edSicoobArqCertificado.Text);

    edPagSeguroChavePIX.Text := Ini.ReadString('PagSeguro', 'ChavePIX', '');
    edPagSeguroClientID.Text := Ini.ReadString('PagSeguro', 'ClientID', '');
    edPagSeguroClientSecret.Text := Ini.ReadString('PagSeguro', 'ClientSecret', '');
    edPagSeguroArqChavePrivada.Text := Ini.ReadString('PagSeguro', 'ArqChavePrivada', edPagSeguroArqChavePrivada.Text);
    edPagSeguroArqCertificado.Text := Ini.ReadString('PagSeguro', 'ArqCertificado', edPagSeguroArqCertificado.Text);

    edGerenciaNetChavePIX.Text := Ini.ReadString('GerenciaNet', 'ChavePIX', '');
    edGerenciaNetClientID.Text := Ini.ReadString('GerenciaNet', 'ClientID', '');
    edGerenciaNetClientSecret.Text := Ini.ReadString('GerenciaNet', 'ClientSecret', '');
    edGerenciaNetArqPFX.Text := Ini.ReadString('GerenciaNet', 'ArqPFX', edGerenciaNetArqPFX.Text);

    edBradescoChavePIX.Text := Ini.ReadString('Bradesco', 'ChavePIX', '');
    edBradescoClientID.Text := Ini.ReadString('Bradesco', 'ClientID', '');
    edBradescoClientSecret.Text := Ini.ReadString('Bradesco', 'ClientSecret', '');
    rgBradescoTipoCertificado.ItemIndex := Ini.ReadInteger('Bradesco', 'TipoCertificado', 1);
    if EstaZerado(rgBradescoTipoCertificado.ItemIndex) then
    begin
      edBradescoArqPFX.Text := Ini.ReadString('Bradesco', 'ArqPFX', edBradescoArqPFX.Text);
      edBradescoSenhaPFX.Text := Ini.ReadString('Bradesco', 'SenhaPFX', '');
    end
    else
    begin
      edBradescoArqChavePrivada.Text := Ini.ReadString('Bradesco', 'ArqChavePrivada', edBradescoArqChavePrivada.Text);
      edBradescoArqCertificado.Text := Ini.ReadString('Bradesco', 'ArqCertificado', edBradescoArqCertificado.Text);
    end;

    edInterChavePIX.Text := Ini.ReadString('Inter', 'ChavePIX', '');
    edInterClientID.Text := Ini.ReadString('Inter', 'ClientID', '');
    edInterClientSecret.Text := Ini.ReadString('Inter', 'ClientSecret', '');
    edInterChavePrivada.Text := Ini.ReadString('Inter', 'ArqChavePrivada', edInterChavePrivada.Text);
    edInterCertificado.Text := Ini.ReadString('Inter', 'ArqCertificado', edInterCertificado.Text);

    edPixPDVCNPJ.Text := Ini.ReadString('PixPDV', 'CNPJ', '');
    edPixPDVToken.Text := Ini.ReadString('PixPDV', 'Token', '');
    edPixPDVSecretKey.Text := Ini.ReadString('PixPDV', 'SecretKey', '');
    cbPIXPDVVersaoAPI.ItemIndex := Ini.ReadInteger('PixPDV', 'VersaoAPI', cbPIXPDVVersaoAPI.ItemIndex);

    edAilosChavePIX.Text := Ini.ReadString('Ailos', 'ChavePIX', '');
    edAilosClientID.Text := Ini.ReadString('Ailos', 'ClientID', '');
    edAilosClientSecret.Text := Ini.ReadString('Ailos', 'ClientSecret', '');
    edAilosChavePrivada.Text := Ini.ReadString('Ailos', 'ArqChavePrivada', edAilosChavePrivada.Text);
    edAilosCertificado.Text := Ini.ReadString('Ailos', 'ArqCertificado', edAilosCertificadoRoot.Text);
    edAilosCertificadoRoot.Text := Ini.ReadString('Ailos', 'ArqCertificadoRoot', edAilosCertificadoRoot.Text);

    edMateraClientID.Text := Ini.ReadString('Matera', 'ClientID', '');
    edMateraSecretKey.Text := Ini.ReadString('Matera', 'SecretKey', '');
    edMateraClientSecret.Text := Ini.ReadString('Matera', 'ClientSecret', '');
    edMateraArqCertificado.Text := Ini.ReadString('Matera', 'ArqCertificado', '');
    edMateraArqChavePrivada.Text := Ini.ReadString('Matera', 'ArqChavePrivada', '');
    edMateraAccountId.Text := Ini.ReadString('Matera', 'AccountID', '');
    edMateraChavePIX.Text := Ini.ReadString('Matera', 'ChavePIX', '');
    edMateraMediatorFee.Text := FloatToString(Ini.ReadFloat('Matera', 'MediatorFee', 0));

    edCieloChavePIX.Text := Ini.ReadString('Cielo', 'ChavePIX', '');
    edCieloClientID.Text := Ini.ReadString('Cielo', 'ClientID', '');
    edCieloClientSecret.Text := Ini.ReadString('Cielo', 'ClientSecret', '');

    edMercadoPagoChavePIX.Text := Ini.ReadString('MercadoPago', 'ChavePix', '');
    edMercadoPagoAccessToken.Text := Ini.ReadString('MercadoPago', 'AccessToken', '');
    cbMercadoPagoTipoChave.ItemIndex := Ini.ReadInteger('MercadoPago', 'TipoChave', 0);

    edGate2AllAuthenticationApi.Text := Ini.ReadString('Gate2All', 'AuthenticationApi', '');
    edGate2AllAuthenticationKey.Text := Ini.ReadString('Gate2All', 'AuthenticationKey', '');

    edBanrisulChavePIX.Text := Ini.ReadString('Banrisul', 'ChavePIX', '');
    edBanrisulClientID.Text := Ini.ReadString('Banrisul', 'ClientID', '');
    edBanrisulClientSecret.Text := Ini.ReadString('Banrisul', 'ClientSecret', '');
    edBanrisulArqCertificadoPFX.Text := Ini.ReadString('Banrisul', 'ArqCertificadoPFX', '');
    edBanrisulSenhaCertificadoPFX.Text := Ini.ReadString('Banrisul', 'SenhaCertificadoPFX', '');

    edC6BankChavePIX.Text := Ini.ReadString('C6Bank', 'ChavePIX', '');
    edC6BankClientID.Text := Ini.ReadString('C6Bank', 'ClientID', '');
    edC6BankClientSecret.Text := Ini.ReadString('C6Bank', 'ClientSecret', '');
    edC6BankChavePrivada.Text := Ini.ReadString('C6Bank', 'ArqChavePrivada', edC6BankChavePrivada.Text);
    edC6BankCertificado.Text := Ini.ReadString('C6Bank', 'ArqCertificado', edC6BankCertificado.Text);

    edAppLessClientId.Text := Ini.ReadString('AppLess', 'ClientId', EmptyStr);
    edAppLessClientSecret.Text := Ini.ReadString('AppLess', 'ClientSecret', EmptyStr);
    edAppLessHMAC.Text := Ini.ReadString('AppLess', 'HMAC', EmptyStr);
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
  AdicionarLinhaLog('- GravarConfiguracao: '+NomeArquivoConfiguracao);
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    Ini.WriteString('Recebedor', 'Nome', edtRecebedorNome.Text);
    Ini.WriteString('Recebedor', 'CEP', edtRecebedorCEP.Text);
    Ini.WriteString('Recebedor', 'Cidade', edtRecebedorCidade.Text);
    Ini.WriteString('Recebedor', 'UF', cbxRecebedorUF.Text);

    Ini.WriteBool('Autenticar', 'Manual', cbAutenticacaoManual.Checked);
    if (not cbAutenticacaoManual.Checked) then
    begin
      Ini.DeleteKey('Autenticar', 'Token');
      Ini.DeleteKey('Autenticar', 'Validade');
    end;

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
    Ini.WriteString('BancoBrasil', 'ArqChavePrivada', edBBArqChavePrivada.Text);
    Ini.WriteString('BancoBrasil', 'ArqCertificado', edBBArqCertificado.Text);  
    Ini.WriteString('BancoBrasil', 'ArqPFX', edBBArqPFX.Text);
    Ini.WriteString('BancoBrasil', 'SenhaPFX', edBBSenhaPFX.Text);
    Ini.WriteInteger('BancoBrasil', 'VersaoAPI', cbBBVersaoAPI.ItemIndex);
    Ini.WriteInteger('BancoBrasil', 'TipoCertificado', rgBBTipoCertificado.ItemIndex);

    Ini.WriteString('Itau', 'ChavePIX', edtItauChavePIX.Text);
    Ini.WriteString('Itau', 'ClientID', edtItauClientID.Text);
    Ini.WriteString('Itau', 'ClientSecret', edtItauClientSecret.Text);
    Ini.WriteString('Itau', 'ArqChavePrivada', edtItauArqChavePrivada.Text);
    Ini.WriteString('Itau', 'ArqCertificado', edtItauArqCertificado.Text);

    Ini.WriteString('Santander', 'ChavePIX', edSantanderChavePIX.Text);
    Ini.WriteString('Santander', 'ConsumerKey', edSantanderConsumerKey.Text);
    Ini.WriteString('Santander', 'ConsumerSecret', edSantanderConsumerSecret.Text);
    Ini.WriteString('Santander', 'ArqCertificadoPFX', edSantanderArqCertificadoPFX.Text);
    Ini.WriteString('Santander', 'SenhaCertificadoPFX', edSantanderSenhaCertificadoPFX.Text);

    Ini.WriteString('Sicredi', 'ChavePIX', edSicrediChavePIX.Text);
    Ini.WriteString('Sicredi', 'ClientID', edSicrediClientID.Text);
    Ini.WriteString('Sicredi', 'ClientSecret', edSicrediClientSecret.Text);
    Ini.WriteString('Sicredi', 'ArqChavePrivada', edSicrediArqChavePrivada.Text);
    Ini.WriteString('Sicredi', 'ArqCertificado', edSicrediArqCertificado.Text);
    if FileExists(edSicrediGerarCSR.Text) then
      Ini.WriteString('Sicredi', 'CertificadoCSR', edSicrediGerarCSR.Text);

    Ini.WriteString('Sicoob', 'ChavePIX', edSicoobChavePIX.Text);
    Ini.WriteString('Sicoob', 'ClientID', edSicoobClientID.Text);
    Ini.WriteString('Sicoob', 'TokenSandbox', edSicoobTokenSandbox.Text);
    Ini.WriteString('Sicoob', 'ArqChavePrivada', edSicoobArqChavePrivada.Text);
    Ini.WriteString('Sicoob', 'ArqCertificado', edSicoobArqCertificado.Text);

    Ini.WriteString('PagSeguro', 'ChavePIX', edPagSeguroChavePIX.Text);
    Ini.WriteString('PagSeguro', 'ClientID', edPagSeguroClientID.Text);
    Ini.WriteString('PagSeguro', 'ClientSecret', edPagSeguroClientSecret.Text);
    Ini.WriteString('PagSeguro', 'ArqChavePrivada', edPagSeguroArqChavePrivada.Text);
    Ini.WriteString('PagSeguro', 'ArqCertificado', edPagSeguroArqCertificado.Text);

    Ini.WriteString('GerenciaNet', 'ChavePIX', edGerenciaNetChavePIX.Text);
    Ini.WriteString('GerenciaNet', 'ClientID', edGerenciaNetClientID.Text);
    Ini.WriteString('GerenciaNet', 'ClientSecret', edGerenciaNetClientSecret.Text);
    Ini.WriteString('GerenciaNet', 'ArqPFX', edGerenciaNetArqPFX.Text);

    Ini.WriteString('Bradesco', 'ChavePIX', edBradescoChavePIX.Text);
    Ini.WriteString('Bradesco', 'ClientID', edBradescoClientID.Text);
    Ini.WriteString('Bradesco', 'ClientSecret', edBradescoClientSecret.Text);
    Ini.WriteInteger('Bradesco', 'TipoCertificado', rgBradescoTipoCertificado.ItemIndex);
    Ini.WriteString('Bradesco', 'ArqPFX', edBradescoArqPFX.Text);
    Ini.WriteString('Bradesco', 'SenhaPFX', edBradescoSenhaPFX.Text);
    Ini.WriteString('Bradesco', 'ArqChavePrivada', edBradescoArqChavePrivada.Text);
    Ini.WriteString('Bradesco', 'ArqCertificado', edBradescoArqCertificado.Text);

    Ini.WriteString('Inter', 'ChavePIX', edInterChavePIX.Text);
    Ini.WriteString('Inter', 'ClientID', edInterClientID.Text);
    Ini.WriteString('Inter', 'ClientSecret', edInterClientSecret.Text);
    Ini.WriteString('Inter', 'ArqChavePrivada', edInterChavePrivada.Text);
    Ini.WriteString('Inter', 'ArqCertificado', edInterCertificado.Text);

    Ini.WriteString('PixPDV', 'CNPJ', edPixPDVCNPJ.Text);
    Ini.WriteString('PixPDV', 'Token', edPixPDVToken.Text);
    Ini.WriteString('PixPDV', 'SecretKey', edPixPDVSecretKey.Text);
    Ini.WriteInteger('PixPDV', 'VersaoAPI', cbPIXPDVVersaoAPI.ItemIndex);

    Ini.WriteString('Ailos', 'ChavePIX', edAilosChavePIX.Text);
    Ini.WriteString('Ailos', 'ClientID', edAilosClientID.Text);
    Ini.WriteString('Ailos', 'ClientSecret', edAilosClientSecret.Text);
    Ini.WriteString('Ailos', 'ArqChavePrivada', edAilosChavePrivada.Text);
    Ini.WriteString('Ailos', 'ArqCertificado', edAilosCertificado.Text);
    Ini.WriteString('Ailos', 'ArqCertificadoRoot', edAilosCertificadoRoot.Text);

    Ini.WriteString('Matera', 'ClientID', edMateraClientID.Text);
    Ini.WriteString('Matera', 'SecretKey', edMateraSecretKey.Text);
    Ini.WriteString('Matera', 'ClientSecret', edMateraClientSecret.Text);
    Ini.WriteString('Matera', 'ArqCertificado', edMateraArqCertificado.Text);
    Ini.WriteString('Matera', 'ArqChavePrivada', edMateraArqChavePrivada.Text);
    Ini.WriteString('Matera', 'AccountID', edMateraAccountId.Text);
    Ini.WriteString('Matera', 'ChavePIX', edMateraChavePIX.Text);
    Ini.WriteFloat('Matera', 'MediatorFee', StringToFloatDef(edMateraMediatorFee.Text, 0));

    Ini.WriteString('Cielo', 'ChavePIX', edCieloChavePIX.Text);
    Ini.WriteString('Cielo', 'ClientID', edCieloClientID.Text);
    Ini.WriteString('Cielo', 'ClientSecret', edCieloClientSecret.Text);

    Ini.WriteString('MercadoPago', 'ChavePix', edMercadoPagoChavePIX.Text);
    Ini.WriteString('MercadoPago', 'AccessToken', edMercadoPagoAccessToken.Text);
    Ini.WriteInteger('MercadoPago', 'TipoChave', cbMercadoPagoTipoChave.ItemIndex);

    Ini.WriteString('Gate2All', 'AuthenticationApi', edGate2AllAuthenticationApi.Text);
    Ini.WriteString('Gate2All', 'AuthenticationKey', edGate2AllAuthenticationKey.Text);

    Ini.WriteString('Banrisul', 'ChavePIX', edBanrisulChavePIX.Text);
    Ini.WriteString('Banrisul', 'ClientID', edBanrisulClientID.Text);
    Ini.WriteString('Banrisul', 'ClientSecret', edBanrisulClientSecret.Text);
    Ini.WriteString('Banrisul', 'ArqCertificadoPFX', edBanrisulArqCertificadoPFX.Text);
    Ini.WriteString('Banrisul', 'SenhaCertificadoPFX', edBanrisulSenhaCertificadoPFX.Text);

    Ini.WriteString('C6Bank', 'ChavePIX', edC6BankChavePIX.Text);
    Ini.WriteString('C6Bank', 'ClientID', edC6BankClientID.Text);
    Ini.WriteString('C6Bank', 'ClientSecret', edC6BankClientSecret.Text);
    Ini.WriteString('C6Bank', 'ArqChavePrivada', edC6BankChavePrivada.Text);
    Ini.WriteString('C6Bank', 'ArqCertificado', edC6BankCertificado.Text);

    Ini.WriteString('AppLess', 'ClientId', edAppLessClientId.Text);
    Ini.WriteString('AppLess', 'ClientSecret', edAppLessClientSecret.Text);
    Ini.WriteString('AppLess', 'HMAC', edAppLessHMAC.Text);
  finally
     Ini.Free;
  end;

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
  ImageList1.GetBitmap(6, imgErrNome.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrCEP.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrPSP.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgBBErroChavePIX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imBBErroPFX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imBBErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imBBErroChavePrivada.Picture.Bitmap);

  ImageList1.GetBitmap(6, imgItauErroChavePIX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroClientID.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroClientSecret.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroChavePrivada.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgItauErroCertificado.Picture.Bitmap);

  ImageList1.GetBitmap(6, imSicrediErroChavePix.Picture.Bitmap);
  ImageList1.GetBitmap(6, imSicrediErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imSicrediErroChavePrivada.Picture.Bitmap);

  ImageList1.GetBitmap(6, imSicoobErroChavePix.Picture.Bitmap);
  ImageList1.GetBitmap(6, imSicoobErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imSicoobErroChavePrivada.Picture.Bitmap);

  ImageList1.GetBitmap(6, imInterErroChavePix.Picture.Bitmap);
  ImageList1.GetBitmap(6, imInterErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imInterErroChavePrivada.Picture.Bitmap);
  ImageList1.GetBitmap(9, btInterAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, btInterAcharCertificado.Glyph);

  ImageList1.GetBitmap(6, imAilosErroChavePix.Picture.Bitmap);
  ImageList1.GetBitmap(6, imAilosErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imAilosErroCertificadoRoot.Picture.Bitmap);
  ImageList1.GetBitmap(6, imAilosErroChavePrivada.Picture.Bitmap);
  ImageList1.GetBitmap(9, btAilosAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, btAilosAcharCertificado.Glyph);
  ImageList1.GetBitmap(9, btAilosAcharCertificadoRoot.Glyph);

  ImageList1.GetBitmap(6, imSantanderErroChavePIX.Picture.Bitmap);
  ImageList1.GetBitmap(6, imSantanderErroCertificadoPFX.Picture.Bitmap);

  ImageList1.GetBitmap(31, btFluxoItemIncluir.Glyph);
  ImageList1.GetBitmap(32, btFluxoItemExcluir.Glyph);
  ImageList1.GetBitmap(33, btFluxoPagar.Glyph);
  ImageList1.GetBitmap(17, btFluxoCancelarCobranca.Glyph);
  ImageList1.GetBitmap(12, btFluxoEstornarPagto.Glyph);
  ImageList1.GetBitmap(23, btFluxoNovaVenda.Glyph);
  ImageList1.GetBitmap(11, btFluxoTentarNovamente.Glyph);
  ImageList1.GetBitmap(17, btFluxoCancelarConsulta.Glyph);
  ImageList1.GetBitmap(16, btFluxoFecharVenda.Glyph);
  ImageList1.GetBitmap(13, btFluxoCopiaECola.Glyph);
  ImageList1.GetBitmap(13, btCobVCopiaECola.Glyph);

  ImageList1.GetBitmap(8, btConsultarPix.Glyph);
  ImageList1.GetBitmap(8, btConsultarPixRecebidos.Glyph);
  ImageList1.GetBitmap(4, btSolicitarDevolucaoPix.Glyph);
  ImageList1.GetBitmap(8, btConsultarDevolucaoPix.Glyph);
  ImageList1.GetBitmap(33, btCriarCobrancaImediata.Glyph);
  ImageList1.GetBitmap(8, btConsultarCobrancaImediata.Glyph);
  ImageList1.GetBitmap(8, btConsultarCobrancas.Glyph);
  ImageList1.GetBitmap(33, btCriarCobV.Glyph);
  ImageList1.GetBitmap(8, btCobVConsultar.Glyph);
  ImageList1.GetBitmap(8, btCobVConsultarLista.Glyph);
  ImageList1.GetBitmap(17, btCobVCancelar.Glyph);
  ImageList1.GetBitmap(33, btBBSimulaPagamento_Executar.Glyph);

  ImageList1.GetBitmap(33, btCriarCobR.Glyph);
  ImageList1.GetBitmap(18, btCriarCobRLimpar.Glyph);
  ImageList1.GetBitmap(8, btConsultarCobR.Glyph);
  ImageList1.GetBitmap(8, btConsultarCobsR.Glyph);
  ImageList1.GetBitmap(17, btCancelarCobR.Glyph);
  ImageList1.GetBitmap(29, btCriarCobRPreencher.Glyph);
  ImageList1.GetBitmap(30, btCriarCobRGerarTxID.Glyph);
  ImageList1.GetBitmap(11, btSolicitarRetentativaCobR.Glyph);

  ImageList1.GetBitmap(33, btCriarRecorrencia.Glyph);
  ImageList1.GetBitmap(18, btCriarRecorrenciaLimpar.Glyph);
  ImageList1.GetBitmap(8, btConsultarRecorrencia.Glyph);
  ImageList1.GetBitmap(8, btConsultarRecorrencias.Glyph);
  ImageList1.GetBitmap(27, btRevisarRecorrencia.Glyph);
  ImageList1.GetBitmap(17, btCancelarRecorrencia.Glyph);
  ImageList1.GetBitmap(29, btCriarRecorrenciaPreencher.Glyph);

  ImageList1.GetBitmap(16, btCriarSolicitacaoRec.Glyph);
  ImageList1.GetBitmap(8, btConsultarSolicitacaoRec.Glyph);
  ImageList1.GetBitmap(18, btConsultarSolicitacaoRecLimpar.Glyph);
  ImageList1.GetBitmap(18, btConsultarSolicitacaoRecLimpar.Glyph);
  ImageList1.GetBitmap(18, btCancelarSolicitacaoRecLimpar.Glyph);
  ImageList1.GetBitmap(17, btCancelarSolicitacaoRec.Glyph);

  ImageList1.GetBitmap(16, btCriarLocationRec.Glyph);
  ImageList1.GetBitmap(8, btConsultarLocationRec.Glyph);
  ImageList1.GetBitmap(8, btConsultarLocationsRec.Glyph);
  ImageList1.GetBitmap(18, btConsultarLocationRecLimpar.Glyph);
  ImageList1.GetBitmap(18, btConsultarLocationsRecLimpar.Glyph);
  ImageList1.GetBitmap(18, btDesvincularLocRecLimpar.Glyph);
  ImageList1.GetBitmap(17, btDesvincularLocRec.Glyph);

  ImageList1.GetBitmap(18, btLimparConsultarPix.Glyph);
  ImageList1.GetBitmap(18, btLimparConsultarPixRecebidos.Glyph);
  ImageList1.GetBitmap(18, btLimparSolicitarDevolucaoPix.Glyph);
  ImageList1.GetBitmap(18, btLimparConsultarDevolucaoPix.Glyph);
  ImageList1.GetBitmap(18, btLimparCriarCobrancaImediata.Glyph);
  ImageList1.GetBitmap(18, btLimparConsultarCobrancaImediata.Glyph);
  ImageList1.GetBitmap(18, btLimparConsultarCobrancas.Glyph);
  ImageList1.GetBitmap(18, btCancelarCobrancaLimparMemo.Glyph);
  ImageList1.GetBitmap(18, btCobVConsultarLimpar.Glyph);
  ImageList1.GetBitmap(18, btCobVConsultarListaLimpar.Glyph);
  ImageList1.GetBitmap(18, btCobVCancelarLimpar.Glyph);
  ImageList1.GetBitmap(18, btBBSimulaPagamento_Limpar.Glyph);
  ImageList1.GetBitmap(17, btCancelarCobranca.Glyph);
  ImageList1.GetBitmap(30, sbCriarCobrancaImediata_GerarTxId.Glyph);

  ImageList1.GetBitmap(1, btQREGerar.Glyph);
  ImageList1.GetBitmap(1, btQRDGerar.Glyph);
  ImageList1.GetBitmap(5, btQREAnalisar.Glyph);
  ImageList1.GetBitmap(5, btQRDAnalisar.Glyph);
  ImageList1.GetBitmap(5, btSicrediGerarChaveInfo.Glyph);
  ImageList1.GetBitmap(5, btSicrediGerarCSRInfo.Glyph);
  ImageList1.GetBitmap(5, btSicoobExtrairChaveCertificadoInfo.Glyph);
  ImageList1.GetBitmap(13, btQREColar.Glyph);
  ImageList1.GetBitmap(13, btQRDColar.Glyph);

  ImageList1.GetBitmap(8, sbConsultaCEP.Glyph);
  ImageList1.GetBitmap(9, sbArqLog.Glyph);
  ImageList1.GetBitmap(7, sbVerSenhaProxy.Glyph);
  ImageList1.GetBitmap(7, sbSantanderVerSenhaPFX.Glyph);
  ImageList1.GetBitmap(7, sbSantanderExtrairCertificadoVerSenhaPFX.Glyph);
  ImageList1.GetBitmap(7, btSicoobExtrairChaveCertificadoVerSenhaPFX.Glyph);
  ImageList1.GetBitmap(9, sbGerenciaNetAcharPFX.Glyph);
  ImageList1.GetBitmap(9, sbBradescoAcharPFX.Glyph);
  ImageList1.GetBitmap(7, sbBradescoVerSenhaPFX.Glyph);
  ImageList1.GetBitmap(7, btBBVerSenhaPFX.Glyph);

  ImageList1.GetBitmap(16, btItauValidarChaveCertificado.Glyph);
  ImageList1.GetBitmap(9, sbItauAcharArqChavePrivada.Glyph);
  ImageList1.GetBitmap(9, sbItauAcharArqCertificado.Glyph);
  ImageList1.GetBitmap(28, btItauGerarChavePrivada.Glyph);
  ImageList1.GetBitmap(4, btItauSolicitarCertificado.Glyph);
  ImageList1.GetBitmap(4, btItauRenovarCertificado.Glyph);

  ImageList1.GetBitmap(18, btLogLimpar.Glyph);
  ImageList1.GetBitmap(10, btSalvarParametros.Glyph);
  ImageList1.GetBitmap(11, btLerParametros.Glyph);

  ImageList1.GetBitmap(9, sbBBAcharPFX.Glyph);
  ImageList1.GetBitmap(9, sbBBAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, sbBBAcharArqCertificado.Glyph);
  ImageList1.GetBitmap(9, sbSicrediAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, sbSicrediAcharArqCertificado.Glyph);
  ImageList1.GetBitmap(9, sbSicoobAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, sbSicoobAcharArqCertificado.Glyph);
  ImageList1.GetBitmap(9, sbSantanderAcharCertificadoPFX.Glyph);
  ImageList1.GetBitmap(9, sbSantanderExtrairCertificadoPFX.Glyph);
  ImageList1.GetBitmap(9, btSicoobExtrairChaveCertificadoArqPFX.Glyph);
  ImageList1.GetBitmap(27, sbSantanderExtrairCertificadoInfo.Glyph);
                                                                 
  ImageList1.GetBitmap(7, sbBanrisulVerSenhaPFX.Glyph);
  ImageList1.GetBitmap(6, imBanrisulErroChavePix.Picture.Bitmap);
  ImageList1.GetBitmap(6, imBanrisulErroCertificadoPFX.Picture.Bitmap);
  ImageList1.GetBitmap(9, sbBanrisulAcharCertificadoPFX.Glyph);

  ImageList1.GetBitmap(6, imC6BankErroChavePix.Picture.Bitmap);
  ImageList1.GetBitmap(6, imC6BankErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imC6BankErroChavePrivada.Picture.Bitmap);
  ImageList1.GetBitmap(9, btC6BankAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, btC6BankAcharCertificado.Glyph);
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
  pgTestesEndPointCobV.ActivePageIndex := 0;

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
  n: TACBrPIXDescontoModalidade;
  o: TACBrPIXValoresModalidade;
  p: TACBrPIXJurosModalidade;
  q: TACBrBBAPIVersao;
  r: TACBrPIXRetentativa;
  s: TACBrPIXPeriodicidade;
  t: TACBrPIXStatusRecorrencia;
  u: TACBrPIXTipoConta;
  v: TACBrPIXStatusRegistroCobranca;
  w: TACBrPIXPDVAPIVersao;
begin
  cbxPSPAtual.Items.Clear;
  for i := 0 to pgPSPs.PageCount-1 do
     cbxPSPAtual.Items.Add( pgPSPs.Pages[i].Caption );

  cbxRecebedorUF.Items.Clear;
  for i := Low(DFeUF) to High(DFeUF) do
     cbxRecebedorUF.Items.Add( DFeUF[i] );

  cbxAmbiente.Items.Clear;
  for j := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
     cbxAmbiente.Items.Add( GetEnumName(TypeInfo(TACBrPixCDAmbiente), integer(j) ));

  cbxBBTipoChave.Items.Clear;
  for k := Low(TACBrPIXTipoChave) to High(TACBrPIXTipoChave) do
     cbxBBTipoChave.Items.Add( GetEnumName(TypeInfo(TACBrPIXTipoChave), integer(k) ));
  cbxItauTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbSantanderTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbSicrediTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbSicoobTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbPagSeguroTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbGerenciaNetTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbBradescoTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbInterTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbAilosTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbCieloTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbBanrisulTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbC6BankTipoChave.Items.Assign(cbxBBTipoChave.Items);
  cbMercadoPagoTipoChave.Items.Assign(cbxBBTipoChave.Items);

  cbxSolicitarDevolucaoPix_Natureza.Items.Clear;
  for l := 0 to Integer(High(TACBrPIXNaturezaDevolucao)) do
     cbxSolicitarDevolucaoPix_Natureza.Items.Add( GetEnumName(TypeInfo(TACBrPIXNaturezaDevolucao), l ));
  cbxSolicitarDevolucaoPix_Natureza.ItemIndex := 1;

  cbxConsultarCobrancas_Status.Items.Clear;
  for m := Low(TACBrPIXStatusCobranca) to High(TACBrPIXStatusCobranca) do
     cbxConsultarCobrancas_Status.Items.Add( GetEnumName(TypeInfo(TACBrPIXStatusCobranca), Integer(m) ));
  cbxConsultarCobrancas_Status.ItemIndex := 0;

  cbCobVConsultarStatus.Items.Clear;
  for m := Low(TACBrPIXStatusCobranca) to High(TACBrPIXStatusCobranca) do
     cbCobVConsultarStatus.Items.Add( GetEnumName(TypeInfo(TACBrPIXStatusCobranca), Integer(m) ));
  cbCobVConsultarStatus.ItemIndex := 0;

  cbCobVDescModalidade.Items.Clear;
  for n := Low(TACBrPIXDescontoModalidade) to High(TACBrPIXDescontoModalidade) do
    cbCobVDescModalidade.Items.Add(IntToStr(Ord(n)) + ' - ' + DescontoModalidadeToString(n));
  cbCobVDescModalidade.ItemIndex := 0;

  cbCobVMultaModalidade.Items.Clear;
  for o := Low(TACBrPIXValoresModalidade) to High(TACBrPIXValoresModalidade) do
    cbCobVMultaModalidade.Items.Add(IntToStr(Ord(o)) + ' - ' + ValoresModalidadeToString(o));
  cbCobVMultaModalidade.ItemIndex := 0;

  cbCobVJurosModalidade.Items.Clear;
  for p := Low(TACBrPIXJurosModalidade) to High(TACBrPIXJurosModalidade) do
    cbCobVJurosModalidade.Items.Add(IntToStr(Ord(p)) + ' - ' + JurosModalidadeToString(p));
  cbCobVJurosModalidade.ItemIndex := 0;

  cbBBVersaoAPI.Items.Clear;
  for q := Low(TACBrBBAPIVersao) to High(TACBrBBAPIVersao) do
    cbBBVersaoAPI.Items.Add(GetEnumName(TypeInfo(TACBrBBAPIVersao), Integer(q)));
  cbBBVersaoAPI.ItemIndex := 0;
  cbBBVersaoAPIChange(Nil);

  dtConsultarPixRecebidosInicio.DateTime := StartOfTheDay(Today);
  dtConsultarPixRecebidosFim.DateTime := EndOfTheDay(Today);

  dtConsultarCobrancas_Inicio.DateTime := StartOfTheDay(Today);
  dtConsultarCobrancas_Fim.DateTime := EndOfTheDay(Today);

  cbCriarRecorrenciaPoliticaRetentativa.Items.Clear;
  for r := Low(TACBrPIXRetentativa) to High(TACBrPIXRetentativa) do
    cbCriarRecorrenciaPoliticaRetentativa.Items.Add(PIXRetentativaToString(r));
  cbCriarRecorrenciaPoliticaRetentativa.ItemIndex := 0;

  cbCriarRecorrenciaPeriodicidade.Items.Clear;
  for s := Low(TACBrPIXPeriodicidade) to High(TACBrPIXPeriodicidade) do
    cbCriarRecorrenciaPeriodicidade.Items.Add(PIXPeriodicidadeToString(s));
  cbCriarRecorrenciaPeriodicidade.ItemIndex := 0;

  cbConsultarRecorrenciasStatus.Items.Clear;
  for t := Low(TACBrPIXStatusRecorrencia) to High(TACBrPIXStatusRecorrencia) do
    cbConsultarRecorrenciasStatus.Items.Add(PIXStatusRecorrenciaToString(t));
  cbConsultarRecorrenciasStatus.ItemIndex := 0;

  cbCriarCobRTipoConta.Items.Clear;
  for u := Low(TACBrPIXTipoConta) to High(TACBrPIXTipoConta) do
    cbCriarCobRTipoConta.Items.Add(PIXTipoContaToString(u));
  cbCriarCobRTipoConta.ItemIndex := 0;

  cbConsultarCobsRStatus.Items.Clear;
  for v := Low(TACBrPIXStatusRegistroCobranca) to High(TACBrPIXStatusRegistroCobranca) do
    cbConsultarCobsRStatus.Items.Add(PIXStatusRegistroCobrancaToString(v));
  cbConsultarCobsRStatus.ItemIndex := 0;

  cbPIXPDVVersaoAPI.Items.Clear;
  for w := Low(TACBrPIXPDVAPIVersao) to High(TACBrPIXPDVAPIVersao) do
    cbPIXPDVVersaoAPI.Items.Add(GetEnumName(TypeInfo(TACBrPIXPDVAPIVersao), Integer(w)));
  cbPIXPDVVersaoAPI.ItemIndex := 0;

  edCobVVencimento.DateTime := IncDay(Now, 7);
  edCriarRecorrenciaDataInicial.DateTime := IncMonth(Now, 1);
  edCriarRecorrenciaDataFinal.DateTime := IncMonth(Now, 4);
  edRevisarRecorrenciaDataInicial.DateTime := IncMonth(Now, 1);
  edCriarSolicitacaoRecExpiracao.DateTime := IncDay(Now, 1);
  edCriarCobRVencimento.DateTime := IncDay(Now, 1);
  edConsultarRecorrenciasInicio.DateTime := StartOfTheMonth(IncMonth(Now, -1));
  edConsultarRecorrenciasFim.DateTime := EndOfTheMonth(IncMonth(Now, -1));
  edConsultarCobsRInicio.DateTime := StartOfTheMonth(IncMonth(Now, -1));
  edConsultarCobsRFim.DateTime := EndOfTheMonth(IncMonth(Now, -1));
  edConsultarLocationsRecInicio.DateTime := StartOfTheMonth(Now);
  edConsultarLocationsRecFim.DateTime := Now;
  edSolicitarRetentativaLiquidacao.DateTime := IncDay(Now, 2);
  pnBBPFX.Parent := pnBBCertificados;
  pnBBChaveECert.Parent := pnBBCertificados;
  pnBradescoPFX.Parent := pnBradescoCertificados;
  pnBradescoChaveECert.Parent := pnBradescoCertificados;
end;

function TForm1.GetInfoOpenSSL: String;
begin
  with ACBrOpenSSLUtils1 do
    Result := 'Info OpenSSL: ' + sLineBreak +
      OpenSSLExt.OpenSSLFullVersion + sLineBreak +
      OpenSSLExt.SSLUtilFile + sLineBreak +
      OpenSSLExt.SSLLibFile + sLineBreak +
      OpenSSLExt.OpenSSLVersion(0) + sLineBreak +
      IntToStr(OpenSSLExt.OpenSSLVersionNum) + sLineBreak;
end;

procedure TForm1.ConfigurarACBrPIXCD;
begin
  AdicionarLinhaLog('  - ConfigurarACBrPIXCD');
  ACBrPixCD1.Recebedor.Nome := edtRecebedorNome.Text;
  ACBrPixCD1.Recebedor.CEP := edtRecebedorCEP.Text;
  ACBrPixCD1.Recebedor.Cidade := edtRecebedorCidade.Text;
  ACBrPixCD1.Recebedor.UF := cbxRecebedorUF.Text;

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
    4: ACBrPixCD1.PSP := ACBrPSPSicredi1;
    5: ACBrPixCD1.PSP := ACBrPSPSicoob1;
    6: ACBrPixCD1.PSP := ACBrPSPPagSeguro1;
    7: ACBrPixCD1.PSP := ACBrPSPGerenciaNet1;
    8: ACBrPixCD1.PSP := ACBrPSPBradesco1;
    9: ACBrPixCD1.PSP := ACBrPSPPixPDV1;
    10: ACBrPixCD1.PSP := ACBrPSPInter1;
    11: ACBrPixCD1.PSP := ACBrPSPAilos1;
    12: ACBrPixCD1.PSP := ACBrPSPMatera1;
    13: ACBrPixCD1.PSP := ACBrPSPCielo1;
    14: ACBrPixCD1.PSP := ACBrPSPMercadoPago1;
    15: ACBrPixCD1.PSP := ACBrPSPGate2All1;
    16: ACBrPixCD1.PSP := ACBrPSPBanrisul1;
    17: ACBrPixCD1.PSP := ACBrPSPC6Bank1;
    18: ACBrPixCD1.PSP := ACBrPSPAppLess1;
  else
    raise Exception.Create('PSP configurado � inv�lido');
  end;

  if cbAutenticacaoManual.Checked then
  begin
    ACBrPixCD1.PSP.OnAntesAutenticar := DoAntesAutenticar;
    ACBrPixCD1.PSP.OnDepoisAutenticar := DoDepoisAutenticar;
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
  ACBrPSPBancoDoBrasil1.BBAPIVersao := TACBrBBAPIVersao(cbBBVersaoAPI.ItemIndex);

  if (ACBrPSPBancoDoBrasil1.BBAPIVersao = apiVersao2) then
  begin
    if (rgBBTipoCertificado.ItemIndex = 0) then  // Se usa PFX
    begin
      ACBrPSPBancoDoBrasil1.ArquivoPFX := edBBArqPFX.Text;
      ACBrPSPBancoDoBrasil1.SenhaPFX := edBBSenhaPFX.Text;
    end
    else  // Se usa Certificado PEM + Chave Privada
    begin
      ACBrPSPBancoDoBrasil1.ArquivoChavePrivada := edBBArqChavePrivada.Text;
      ACBrPSPBancoDoBrasil1.ArquivoCertificado := edBBArqCertificado.Text;
    end;
  end;

  ACBrPSPItau1.ChavePIX := edtItauChavePIX.Text;
  ACBrPSPItau1.ClientID := edtItauClientID.Text;
  ACBrPSPItau1.ClientSecret := edtItauClientSecret.Text;
  ACBrPSPItau1.ArquivoChavePrivada := edtItauArqChavePrivada.Text;
  ACBrPSPItau1.ArquivoCertificado := edtItauArqCertificado.Text;

  ACBrPSPSantander1.ChavePIX := edSantanderChavePIX.Text;
  ACBrPSPSantander1.ConsumerKey := edSantanderConsumerKey.Text;
  ACBrPSPSantander1.ConsumerSecret := edSantanderConsumerSecret.Text;
  ACBrPSPSantander1.SenhaPFX := edSantanderSenhaCertificadoPFX.Text;
  ACBrPSPSantander1.ArquivoPFX := edSantanderArqCertificadoPFX.Text;

  ACBrPSPSicredi1.ChavePIX := edSicrediChavePIX.Text;
  ACBrPSPSicredi1.ClientID := edSicrediClientID.Text;
  ACBrPSPSicredi1.ClientSecret := edSicrediClientSecret.Text;
  ACBrPSPSicredi1.ArquivoChavePrivada := edSicrediArqChavePrivada.Text;
  ACBrPSPSicredi1.ArquivoCertificado := edSicrediArqCertificado.Text;

  ACBrPSPSicoob1.ChavePIX := edSicoobChavePIX.Text;
  ACBrPSPSicoob1.ClientID := edSicoobClientID.Text;
  ACBrPSPSicoob1.TokenSandbox := edSicoobTokenSandbox.Text;
  ACBrPSPSicoob1.ArquivoChavePrivada := edSicoobArqChavePrivada.Text;
  ACBrPSPSicoob1.ArquivoCertificado := edSicoobArqCertificado.Text;

  ACBrPSPPagSeguro1.ChavePIX := edPagSeguroChavePIX.Text;
  ACBrPSPPagSeguro1.ClientID := edPagSeguroClientID.Text;
  ACBrPSPPagSeguro1.ClientSecret := edPagSeguroClientSecret.Text;
  ACBrPSPPagSeguro1.ArquivoChavePrivada := edPagSeguroArqChavePrivada.Text;
  ACBrPSPPagSeguro1.ArquivoCertificado := edPagSeguroArqCertificado.Text;

  ACBrPSPGerenciaNet1.ChavePIX := edGerenciaNetChavePIX.Text;
  ACBrPSPGerenciaNet1.ClientID := edGerenciaNetClientID.Text;
  ACBrPSPGerenciaNet1.ClientSecret := edGerenciaNetClientSecret.Text;
  ACBrPSPGerenciaNet1.ArquivoPFX := edGerenciaNetArqPFX.Text;

  ACBrPSPBradesco1.ChavePIX := edBradescoChavePIX.Text;
  ACBrPSPBradesco1.ClientID := edBradescoClientID.Text;
  ACBrPSPBradesco1.ClientSecret := edBradescoClientSecret.Text;
  if (rgBradescoTipoCertificado.ItemIndex = 0) then  // Se usa PFX
  begin
    ACBrPSPBradesco1.ArquivoPFX := edBradescoArqPFX.Text;
    ACBrPSPBradesco1.SenhaPFX := edBradescoSenhaPFX.Text;
  end
  else  // Se usa Certificado PEM + Chave Privada
  begin
    ACBrPSPBradesco1.ArquivoChavePrivada := edBradescoArqChavePrivada.Text;
    ACBrPSPBradesco1.ArquivoCertificado := edBradescoArqCertificado.Text;
  end;

  ACBrPSPPixPDV1.CNPJ := edPixPDVCNPJ.Text;
  ACBrPSPPixPDV1.Token := edPixPDVToken.Text;
  ACBrPSPPixPDV1.ClientSecret := edPixPDVSecretKey.Text;
  ACBrPSPPixPDV1.APIVersao := TACBrPIXPDVAPIVersao(cbPIXPDVVersaoAPI.ItemIndex);

  ACBrPSPInter1.ChavePIX := edInterChavePIX.Text;
  ACBrPSPInter1.ClientID := edInterClientID.Text;
  ACBrPSPInter1.ClientSecret := edInterClientSecret.Text;
  ACBrPSPInter1.ArquivoChavePrivada := edInterChavePrivada.Text;
  ACBrPSPInter1.ArquivoCertificado := edInterCertificado.Text;

  ACBrPSPAilos1.ChavePIX := edAilosChavePIX.Text;
  ACBrPSPAilos1.ClientID := edAilosClientID.Text;
  ACBrPSPAilos1.ClientSecret := edAilosClientSecret.Text;
  ACBrPSPAilos1.ArquivoChavePrivada := edAilosChavePrivada.Text;
  ACBrPSPAilos1.ArquivoCertificado := edAilosCertificado.Text;
  ACBrPSPAilos1.RootCrt := edAilosCertificadoRoot.Text;
  
  ACBrPSPMatera1.ChavePIX := edMateraChavePIX.Text;
  ACBrPSPMatera1.ClientID := edMateraClientID.Text;
  ACBrPSPMatera1.AccountId := edMateraAccountId.Text;
  ACBrPSPMatera1.SecretKey := edMateraSecretKey.Text;
  ACBrPSPMatera1.MediatorFee := StringToFloatDef(edMateraMediatorFee.Text, 0);
  ACBrPSPMatera1.ClientSecret := edMateraClientSecret.Text;
  ACBrPSPMatera1.ArquivoCertificado := edMateraArqCertificado.Text;
  ACBrPSPMatera1.ArquivoChavePrivada := edMateraArqChavePrivada.Text;

  ACBrPSPCielo1.ChavePIX := edCieloChavePIX.Text;
  ACBrPSPCielo1.ClientID := edCieloClientID.Text;
  ACBrPSPCielo1.ClientSecret := edCieloClientSecret.Text;

  ACBrPSPMercadoPago1.ChavePIX := edMercadoPagoChavePIX.Text;
  ACBrPSPMercadoPago1.AccessToken := edMercadoPagoAccessToken.Text;
  ACBrPSPMercadoPago1.TipoChave := TACBrPIXTipoChave(cbMercadoPagoTipoChave.ItemIndex);

  ACBrPSPGate2All1.AuthenticationApi := edGate2AllAuthenticationApi.Text;
  ACBrPSPGate2All1.AuthenticationKey := edGate2AllAuthenticationKey.Text;

  ACBrPSPBanrisul1.ChavePIX := edBanrisulChavePIX.Text;
  ACBrPSPBanrisul1.ClientID := edBanrisulClientID.Text;
  ACBrPSPBanrisul1.ClientSecret := edBanrisulClientSecret.Text;
  ACBrPSPBanrisul1.ArquivoPFX := edBanrisulArqCertificadoPFX.Text;
  ACBrPSPBanrisul1.SenhaPFX := edBanrisulSenhaCertificadoPFX.Text;

  ACBrPSPC6Bank1.ChavePIX := edC6BankChavePIX.Text;
  ACBrPSPC6Bank1.ClientID := edC6BankClientID.Text;
  ACBrPSPC6Bank1.ClientSecret := edC6BankClientSecret.Text;
  ACBrPSPC6Bank1.ArquivoChavePrivada := edC6BankChavePrivada.Text;
  ACBrPSPC6Bank1.ArquivoCertificado := edC6BankCertificado.Text;

  ACBrPSPAppLess1.ClientID := edAppLessClientId.Text;
  ACBrPSPAppLess1.ClientSecret := edAppLessClientSecret.Text;
  ACBrPSPAppLess1.SecretKeyHMAC := edAppLessHMAC.Text;
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
  mQRD.Lines.Text := ACBrPixCD1.GerarQRCodeDinamico(
                       edQRDLocation.Text,
                       edQRDTxID.Text);
  PintarQRCode(mQRD.Lines.Text, imgQRD.Picture.Bitmap, qrUTF8BOM);
end;

procedure TForm1.AnalisarBRCode(aBRCode: TACBrBRCode);
begin
  AdicionarLinhaLog('');
  if (aBRCode is TACBrPIXQRCodeEstatico) then
  with TACBrPIXQRCodeEstatico(aBRCode) do
  begin
    AdicionarLinhaLog('----- Analise do QRCode Est�tico -----');
    AdicionarLinhaLog('ChavePix: ' + PixKey);
    AdicionarLinhaLog('TipoChavePix: ' + GetEnumName(TypeInfo(TACBrPIXTipoChave), Integer(PixKeyType)));
    AdicionarLinhaLog('infoAdicional: ' + AdditionalInfo);
    AdicionarLinhaLog('pss: ' + IntToStr(pss));
  end
  else if (aBRCode is TACBrPIXQRCodeDinamico) then
  begin
    AdicionarLinhaLog('----- Analise do QRCode Din�mico -----');
    AdicionarLinhaLog('URL: ' + TACBrPIXQRCodeDinamico(aBRCode).URL);
  end;

  AdicionarLinhaLog('NomeRecebedor: ' + aBRCode.MerchantName);
  AdicionarLinhaLog('CidadeRecebedor: ' + aBRCode.MerchantCity);
  AdicionarLinhaLog('CEPRecebedor: ' + aBRCode.PostalCode);
  AdicionarLinhaLog('Valor: ' + FormatFloat('0.00', aBRCode.TransactionAmount));
  AdicionarLinhaLog('TxId: ' + aBRCode.TxId);
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
  jdata: TJSONData;
  ms: TMemoryStream;
{$ELSE}
  {$IFDEF DELPHIXE6_UP}
  var
    wJsonValue: TJSONValue;
  {$ENDIF}
{$ENDIF}
begin
  Result := AJSON;
  try
    {$IFDEF FPC}
    ms := TMemoryStream.Create;
    try  
      ms.Write(Pointer(AJSON)^, Length(AJSON));
      ms.Position := 0;
      jpar := TJSONParser.Create(ms, [joUTF8]);
      jdata := jpar.Parse;
      if Assigned(jdata) then
        Result := jdata.FormatJSON;
    finally
      ms.Free;
      if Assigned(jpar) then
        jpar.Free;
      if Assigned(jdata) then
        jdata.Free;
    end;
    {$ELSE}
      {$IFDEF DELPHIXE6_UP}
      wJsonValue := TJSONObject.ParseJSONValue(AJSON);
      try
        if Assigned(wJsonValue) then
        begin
          Result := wJsonValue.Format(2);
        end;
      finally
        wJsonValue.Free;
      end;
      {$ENDIF}
    {$ENDIF}
  except
    Result := AJSON;
  end;
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
      ShowMessage('Erro ao consultar cobran�a' + sLineBreak +
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

      if SolicitarDevolucaoPix(fFluxoDados.E2E, StringReplace(fFluxoDados.E2E, 'E', 'D', [rfReplaceAll])) then
      begin
        Sleep(1000);
        ConsultarDevolucao;

        if (fFluxoDados.StatusDevolucao = stdDEVOLVIDO) then
          ShowMessage('Pagamento Estornado com Sucesso')
        else if (fFluxoDados.StatusDevolucao = stdEM_PROCESSAMENTO) then
          tmConsultarDevolucao.Enabled := True;  // Estorno pendente? ...Consultar at� alterar Status
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
    btFluxoNovaVenda.Visible := (StatusCobranca <> stcNENHUM) or (StatusDevolucao = stdDEVOLVIDO);
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
    stdEM_PROCESSAMENTO: AtualizarPanelPrincipal('DEVOLU�AO PENDENTE', $00523C30);
    stdNAO_REALIZADO: AtualizarPanelPrincipal('DEVOLU��O N�O REALIZADA', $00523C30);
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
    Cells[1,0] := 'Descri��o';
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

    RowCount := RowCount - 1;
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

procedure TForm1.DoAntesAutenticar(var aToken: String;
  var aValidadeToken: TDateTime);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    aToken := Ini.ReadString('Autenticar', 'Token', EmptyStr);
    aValidadeToken := Ini.ReadDateTime('Autenticar', 'Validade', 0);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.DoDepoisAutenticar(const aToken: String;
  const aValidadeToken: TDateTime);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    Ini.WriteString('Autenticar', 'Token', aToken);
    Ini.WriteDateTime('Autenticar', 'Validade', aValidadeToken);
  finally
    Ini.Free;
  end;
end;

end.

