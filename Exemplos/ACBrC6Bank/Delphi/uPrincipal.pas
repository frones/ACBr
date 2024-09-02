unit uPrincipal;

interface

// descomentar o motor de relatório que desejar utilizar! removendo o ponto
{$DEFINE GERADOR_FORTES_REPORT}
{ .$DEFINE GERADOR_FAST_REPORT }

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  Buttons,
  FileCtrl,
  ACBrBoletoConversao,
  ACBrBase,
  ACBrBoleto,
  TypInfo
{$IFDEF GERADOR_FORTES_REPORT},
  ACBrBoletoFCFortesFr {$ENDIF}
{$IFDEF GERADOR_FAST_REPORT},
  ACBrBoletoFCFR{$ENDIF}
    ,
  ACBrUtil.FilesIO,
  ACBrUtil,
  ACBrDFeSSL,
  IniFiles,
  Mask,
  ACBrBoletoFPDF,
  StrUtils,
  ACBrBoletoRetorno,
  ACBrPIXPSPBancoDoBrasil,
  ACBrPIXCD,
  ACBrPIXPSPC6Bank,
  ACBrOpenSSLUtils,
  ACBrSocket,
  ACBrCEP,
  synacode,
  ACBrPIXBase,
  ACBrPIXUtil,
  Spin,
  jpeg,
  //pngimage,
  ACBrPIXSchemasCobV,
  DateUtils,
//  ImageList,
  ImgList
{$IFDEF FPC}
    ,
  DateTimePicker
{$ENDIF},
  Classes,
  ACBrJSON,
  ACBrPIXSchemasPix,
  ACBrPIXSchemasDevolucao,
  ACBrPIXSchemasCob,
  ACBrImage,
  ACBrDelphiZXingQRCode,
  ACBrPIXBRCode,
  Grids,
  ImageList,
  OpenSSLExt;



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

  TIndicadorTela = (itBoleto, itPix, itNenhuma, itCfgPix, itCfgBoleto,
    itTestePix);

  TfrmPrincipal = class(TForm)
    pnpMenu: TPanel;
    pnpPrincipal: TPanel;
    pagPrincipal: TPageControl;
    tabnenhum: TTabSheet;
    Boleto: TTabSheet;
    tabPix: TTabSheet;
    btnBoletoMenu: TButton;
    btnPixMenu: TButton;
    btnSairMenu: TButton;
    Shape2: TShape;
    Label2: TLabel;
    btnConfigPix: TSpeedButton;
    TabConfigPix: TTabSheet;
    ConfigBoleto: TTabSheet;
    Label3: TLabel;
    Shape5: TShape;
    Label5: TLabel;
    btnCfgSalvaBoleto: TButton;
    btnCfgSalvaPix: TButton;
    imgLogo: TImage;
    pnpCobranca: TPanel;
    Label6: TLabel;
    edtEspecieDoc: TEdit;
    Label7: TLabel;
    edtEspecieMod: TEdit;
    Label8: TLabel;
    cbxAceite: TComboBox;
    Label9: TLabel;
    edtCarteira: TEdit;
    Label64: TLabel;
    cbxTipoDocumento: TComboBox;
    Label50: TLabel;
    cbxCaracteristicaTitulo: TComboBox;
    Label51: TLabel;
    cbxResponsavelEmissao: TComboBox;
    StaticText1: TStaticText;
    pnpBeneficiario: TPanel;
    StaticText2: TStaticText;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    edtContaDV: TEdit;
    edtConta: TEdit;
    edtAgenciaDV: TEdit;
    edtAgencia: TEdit;
    Label43: TLabel;
    Label44: TLabel;
    Label47: TLabel;
    edtOperacao: TEdit;
    edtModalidade: TEdit;
    edtConvenio: TEdit;
    edtCodigoCedente: TEdit;
    Label48: TLabel;
    Label53: TLabel;
    edtBenifRazao: TEdit;
    Label54: TLabel;
    edtBenifCNPJ: TEdit;
    Label62: TLabel;
    edtBenifFantasia: TEdit;
    edtBenifEndereco: TEdit;
    Label55: TLabel;
    edtBenifNum: TEdit;
    Label56: TLabel;
    edtBenifComplemento: TEdit;
    Label57: TLabel;
    edtBenifBairro: TEdit;
    Label58: TLabel;
    Label59: TLabel;
    edtBenifCidade: TEdit;
    Label60: TLabel;
    edtBenifCEP: TEdit;
    Label61: TLabel;
    edtBenifUF: TEdit;
    Label63: TLabel;
    edtBenifTelefone: TEdit;
    cbxCNAB: TComboBox;
    Label33: TLabel;
    Label65: TLabel;
    edtPathRemessa: TEdit;
    Label66: TLabel;
    edtPathRetorno: TEdit;
    pnpWebService: TPanel;
    StaticText3: TStaticText;
    Label67: TLabel;
    edtClientID: TEdit;
    Label68: TLabel;
    edtClientSecret: TEdit;
    edtPathArqCRT: TEdit;
    Label10: TLabel;
    edtPathArqKEY: TEdit;
    Label11: TLabel;
    btnPesqCRT: TSpeedButton;
    btnPesqKEY: TSpeedButton;
    btnPesqRemessa: TSpeedButton;
    btnPesqRetorno: TSpeedButton;
    OpenDialog1: TOpenDialog;
    pnpLog: TPanel;
    Label87: TLabel;
    cbxLogNivel: TComboBox;
    Label88: TLabel;
    edtArquivoLog: TEdit;
    Label89: TLabel;
    edtPathLog: TEdit;
    btnPesArqLogWS: TButton;
    btnPesPathLogWs: TButton;
    pnpDiversos: TPanel;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    ckbImprimirMensagemPadrao: TCheckBox;
    ckbRemoverAcentuacaoRemessa: TCheckBox;
    chkEMVFicticio: TCheckBox;
    chkIndicadorPix: TCheckBox;
    ckbEmHomologacao: TCheckBox;
    FACBrBoleto: TACBrBoleto;
    ckbLerCedenteArquivoRetorno: TCheckBox;
    pnpBolAcrescDesconto: TPanel;
    Label12: TLabel;
    edtMoraJuros: TEdit;
    Label13: TLabel;
    edtDataMora: TMaskEdit;
    Label14: TLabel;
    edtValorDesconto: TEdit;
    Label15: TLabel;
    edtDataDesconto: TMaskEdit;
    Label16: TLabel;
    edtValorAbatimento: TEdit;
    Label17: TLabel;
    edtDataAbatimento: TMaskEdit;
    Label18: TLabel;
    edtMulta: TEdit;
    Label19: TLabel;
    edtDataMulta: TMaskEdit;
    Label20: TLabel;
    pnpMensagem: TPanel;
    Label21: TLabel;
    memMensagem: TMemo;
    pnpDuplicata: TPanel;
    Label22: TLabel;
    edtNossoNro: TEdit;
    Label23: TLabel;
    edtNumeroDoc: TEdit;
    Label24: TLabel;
    edtValorDoc: TEdit;
    Label25: TLabel;
    edtDataDoc: TMaskEdit;
    Label26: TLabel;
    edtVencimento: TMaskEdit;
    Label27: TLabel;
    edtSeuNumero: TEdit;
    Label28: TLabel;
    Label29: TLabel;
    edtDataProtesto: TMaskEdit;
    Label30: TLabel;
    cbxCodMoraJuros: TComboBox;
    Label31: TLabel;
    cbxTipoMulta: TComboBox;
    Label32: TLabel;
    edtDataBaixa: TMaskEdit;
    Label34: TLabel;
    pnpCedente: TPanel;
    Label35: TLabel;
    edtNome: TEdit;
    Label36: TLabel;
    edtCPFCNPJ: TEdit;
    Label37: TLabel;
    edtEmail: TEdit;
    Label42: TLabel;
    edtEndereco: TEdit;
    Label45: TLabel;
    edtNumero: TEdit;
    Label46: TLabel;
    edtComplemento: TEdit;
    Label69: TLabel;
    edtCidade: TEdit;
    Label70: TLabel;
    edtCEP: TEdit;
    Label71: TLabel;
    edtUF: TEdit;
    edtBairro: TEdit;
    Label72: TLabel;
    Label73: TLabel;
    pnpMotorRelatorio: TPanel;
    Label80: TLabel;
    cbxMotorRelatorio: TComboBox;
    Label83: TLabel;
    Label74: TLabel;
    cbxLayOut: TComboBox;
    Label86: TLabel;
    edtSenhaPDF: TEdit;
    Label84: TLabel;
    Label81: TLabel;
    edtPathFR3: TEdit;
    edtPathLogoMarca: TEdit;
    Label82: TLabel;
    btnPesqFR3: TSpeedButton;
    btnPesqLogoBanco: TSpeedButton;
    dlgFile: TOpenDialog;
    btnBoletoTeste: TButton;
    btnIncluiBoleto: TButton;
    Label75: TLabel;
    cbxTipoDesconto: TComboBox;
    btnTotalListaBoleto: TButton;
    btnImprimeBoleto: TButton;
    pnpCNAB: TPanel;
    Label76: TLabel;
    btnLerRetorno: TButton;
    btnGerarRemessa: TButton;
    pnpAPIWS: TPanel;
    Label77: TLabel;
    btnRegistrarBoletoAPI: TButton;
    btnConsultarBoletoAPI: TButton;
    btnLimparListaTitulo: TButton;
    Image1: TImage;
    TestePIX: TTabSheet;
    pgConfPixPSP: TPageControl;
    tsPIX: TTabSheet;
    pConfPIX: TPanel;
    gbRecebedor: TGroupBox;
    pnRecebedor: TPanel;
    imgErrCEP: TImage;
    Label52: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    sbConsultaCEP: TSpeedButton;
    imgErrNome: TImage;
    Label85: TLabel;
    edtRecebedorNome: TEdit;
    edtRecebedorCidade: TEdit;
    edtRecebedorCEP: TEdit;
    cbxRecebedorUF: TComboBox;
    gbProxy: TGroupBox;
    pnProxy: TPanel;
    Label90: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    sbVerSenhaProxy: TSpeedButton;
    edtProxyHost: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    seProxyPorta: TSpinEdit;
    gbLog: TGroupBox;
    pnLog: TPanel;
    Label94: TLabel;
    Label95: TLabel;
    sbArqLog: TSpeedButton;
    edtArqLog: TEdit;
    cbxNivelLog: TComboBox;
    gbPSP: TGroupBox;
    pnPSP: TPanel;
    Label97: TLabel;
    Label98: TLabel;
    cbxAmbiente: TComboBox;
    seTimeout: TSpinEdit;
    gbCobranca: TGroupBox;
    pnCobranca: TPanel;
    Label99: TLabel;
    seCobrancaExpiracao: TSpinEdit;
    gbAutenticacaoManual: TGroupBox;
    pnAutenticacaoManual: TPanel;
    cbAutenticacaoManual: TCheckBox;
    tsPSP: TTabSheet;
    pnC6Bank: TPanel;
    lbC6BankTipoChave: TLabel;
    lbC6BankClientSecret: TLabel;
    lbC6BankClientID: TLabel;
    lbC6BankChave: TLabel;
    imC6BankErroChavePix: TImage;
    lbC6BankChavePrivada: TLabel;
    lbC6BankErroChavePrivada: TLabel;
    imC6BankErroChavePrivada: TImage;
    btC6BankAcharChavePrivada: TSpeedButton;
    btC6BankAcharCertificado: TSpeedButton;
    imC6BankErroCertificado: TImage;
    lbC6BankErroCertificado: TLabel;
    lbC6BankCertificado: TLabel;
    edC6BankClientSecret: TEdit;
    edC6BankClientID: TEdit;
    edC6BankChavePIX: TEdit;
    cbC6BankTipoChave: TComboBox;
    edC6BankChavePrivada: TEdit;
    edC6BankCertificado: TEdit;
    tmConsultarDevolucao: TTimer;
    tmConsultarPagto: TTimer;
    ACBrPixCD1: TACBrPixCD;
    ACBrCEP1: TACBrCEP;
    ACBrOpenSSLUtils1: TACBrOpenSSLUtils;
    ACBrPSPC6Bank1: TACBrPSPC6Bank;
    Splitter1: TSplitter;
    pgTestes: TPageControl;
    tsEndPoints: TTabSheet;
    pgTesteEndPoints: TPageControl;
    tsEndPointPix: TTabSheet;
    pgTestesPix: TPageControl;
    tsConsultarPix: TTabSheet;
    pConsultarPix: TPanel;
    lConsultarPixE2eid: TLabel;
    edtConsultarPixE2eid: TEdit;
    btConsultarPix: TBitBtn;
    mConsultarPix: TMemo;
    Panel3: TPanel;
    btLimparConsultarPix: TBitBtn;
    tsConsultarPixRecebidos: TTabSheet;
    pConsultarPixRecebidos: TPanel;
    lE2eid: TLabel;
    lInicio: TLabel;
    lFim: TLabel;
    lCPFCPNJ: TLabel;
    lPagina: TLabel;
    lPagina1: TLabel;
    edtConsultarPixRecebidosTxId: TEdit;
    btConsultarPixRecebidos: TBitBtn;
    dtConsultarPixRecebidosInicio: TDateTimePicker;
    dtConsultarPixRecebidosFim: TDateTimePicker;
    edtConsultarPixRecebidosCPFCNPJ: TEdit;
    seConsultarPixRecebidosPagina: TSpinEdit;
    seConsultarPixRecebidosItensPagina: TSpinEdit;
    mConsultarPixRecebidos: TMemo;
    Panel4: TPanel;
    btLimparConsultarPixRecebidos: TBitBtn;
    tsSolicitarDevolucaoPix: TTabSheet;
    pSolicitarDevolucaoPix: TPanel;
    lConsultarDevolucaoPixE2eid2: TLabel;
    lConsultarDevolucaoPixIdentificadorDevolucao1: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    lConsultarDevolucaoPixE2eid3: TLabel;
    edtSolicitarDevolucaoPix_e2eid: TEdit;
    btSolicitarDevolucaoPix: TBitBtn;
    edtSolicitarDevolucaoPix_id: TEdit;
    cbxSolicitarDevolucaoPix_Natureza: TComboBox;
    edtSolicitarDevolucaoPix_Descricao: TEdit;
    feSolicitarDevolucaoPix_Valor: TEdit;
    mSolicitarDevolucaoPix: TMemo;
    Panel6: TPanel;
    btLimparSolicitarDevolucaoPix: TBitBtn;
    tsConsultarDevolucaoPix: TTabSheet;
    pConsultarDevolucaoPix: TPanel;
    lConsultarDevolucaoPixE2eid1: TLabel;
    lConsultarDevolucaoPixIdentificadorDevolucao: TLabel;
    edtConsultarDevolucaoPix_e2eid: TEdit;
    btConsultarDevolucaoPix: TBitBtn;
    edtConsultarDevolucaoPix_id: TEdit;
    mConsultarDevolucaoPix: TMemo;
    Panel5: TPanel;
    btLimparConsultarDevolucaoPix: TBitBtn;
    tsEndPointCob: TTabSheet;
    pgTestesEndPointCob: TPageControl;
    tsCriarCobrancaImediata: TTabSheet;
    imgQRCriarCobrancaImediata: TImage;
    Splitter2: TSplitter;
    pCriarCobrancaImediata: TPanel;
    Label107: TLabel;
    lConsultarDevolucaoPixE2eid5: TLabel;
    lCPFCPNJ1: TLabel;
    lConsultarDevolucaoPixIdentificadorDevolucao2: TLabel;
    lE2eid1: TLabel;
    sbCriarCobrancaImediata_GerarTxId: TSpeedButton;
    btCriarCobrancaImediata: TBitBtn;
    edtCriarCobrancaImediata_NomeDevedor: TEdit;
    edtCriarCobrancaImediata_CPF_CNPJ: TEdit;
    chCriarCobrancaImediata_PermiterAlterarValor: TCheckBox;
    edtCriarCobrancaImediata_SolicitacaoAoPagador: TEdit;
    edtCriarCobrancaImediata_TxId: TEdit;
    feCriarCobrancaImediatax_Valor: TEdit;
    mCriarCobrancaImediata: TMemo;
    Panel9: TPanel;
    btLimparCriarCobrancaImediata: TBitBtn;
    tsConsultarCobrancaImediata: TTabSheet;
    pConsultarCobrancaImediata: TPanel;
    lConsultarPixE2eid1: TLabel;
    lPagina2: TLabel;
    edtConsultarCobrancaImediata_TxId: TEdit;
    btConsultarCobrancaImediata: TBitBtn;
    seConsultarCobrancaImediata_Revisao: TSpinEdit;
    mConsultarCobrancaImediata: TMemo;
    Panel10: TPanel;
    btLimparConsultarCobrancaImediata: TBitBtn;
    tsConsultarCobrancas: TTabSheet;
    pConsultarCobrancas: TPanel;
    lInicio1: TLabel;
    lFim1: TLabel;
    lCPFCPNJ2: TLabel;
    lPagina3: TLabel;
    lPagina4: TLabel;
    Label108: TLabel;
    btConsultarCobrancas: TBitBtn;
    dtConsultarCobrancas_Inicio: TDateTimePicker;
    dtConsultarCobrancas_Fim: TDateTimePicker;
    edtConsultarCobrancas_CPFCNPJ: TEdit;
    seConsultarCobrancas_Pagina: TSpinEdit;
    seConsultarCobrancas_ItensPagina: TSpinEdit;
    chConsultarCobrancas_ComLocation: TCheckBox;
    cbxConsultarCobrancas_Status: TComboBox;
    mConsultarCobrancas: TMemo;
    Panel11: TPanel;
    btLimparConsultarCobrancas: TBitBtn;
    tsCancelarCobranca: TTabSheet;
    pnCancelarCobranca: TPanel;
    lbCancelarCobrancaTxID: TLabel;
    edCancelarCobrancaTxID: TEdit;
    btCancelarCobranca: TBitBtn;
    mmCancelarCobranca: TMemo;
    pnCancelarCobrancaRodape: TPanel;
    btCancelarCobrancaLimparMemo: TBitBtn;
    tsEndPointCobV: TTabSheet;
    pgTestesEndPointCobV: TPageControl;
    tsCobVCriarCobranca: TTabSheet;
    btCobVCopiaECola: TSpeedButton;
    imCobVQRCode: TImage;
    lbCobVCopiaECola: TLabel;
    lbCobVValor: TLabel;
    lbCobVDiasPagar: TLabel;
    lbCobVVencimento: TLabel;
    edCobVCopiaECola: TEdit;
    btCriarCobV: TBitBtn;
    edCobVValor: TEdit;
    edCobVDiasPagar: TSpinEdit;
    edCobVVencimento: TDateTimePicker;
    gbCobVMulta: TGroupBox;
    pnCobVMulta: TPanel;
    lbCobVMultaModalidade: TLabel;
    lbCobVMultaValor: TLabel;
    edCobVMultaValor: TEdit;
    cbCobVMultaModalidade: TComboBox;
    gbCobVJuros: TGroupBox;
    pnCobVJuros: TPanel;
    lbCobVJurosModalidade: TLabel;
    lbCobVJurosValor: TLabel;
    edCobVJurosValor: TEdit;
    cbCobVJurosModalidade: TComboBox;
    gbCobVDesconto: TGroupBox;
    pnCobVDesconto: TPanel;
    lbCobVDescModalidade: TLabel;
    lbCobVDescValor: TLabel;
    edCobVDescValor: TEdit;
    cbCobVDescModalidade: TComboBox;
    gbCobVComprador: TGroupBox;
    pnCobVComprador: TPanel;
    lbCobVCompradorNome: TLabel;
    lbCobVCompradorDoc: TLabel;
    edCobVCompradorNome: TEdit;
    edCobVCompradorDoc: TEdit;
    tsCobVConsultarCobranca: TTabSheet;
    pnConsultarCobrancaVencto: TPanel;
    lbCobVConsultarTxID: TLabel;
    lbCobVConsultarRevisao: TLabel;
    edCobVConsultarTxID: TEdit;
    btCobVConsultar: TBitBtn;
    edCobVConsultarRevisao: TSpinEdit;
    mmCobVConsultar: TMemo;
    pnCobVConsultarRodape: TPanel;
    btCobVConsultarLimpar: TBitBtn;
    tsCobVConsultarCobrancas: TTabSheet;
    CobVConsultarRodapeLista: TPanel;
    btCobVConsultarListaLimpar: TBitBtn;
    mmCobVConsultarLista: TMemo;
    pnCobVConsultarParams: TPanel;
    lbCobVConsultarInicio: TLabel;
    lbCobVConsultarFim: TLabel;
    lbCobVConsultarCPFCNPJ: TLabel;
    lbCobVConsultarPagina: TLabel;
    lbCobVConsultarItensPag: TLabel;
    lbCobVConsultarStatus: TLabel;
    btCobVConsultarLista: TBitBtn;
    edCobVConsultarInicio: TDateTimePicker;
    edCobVConsultarFim: TDateTimePicker;
    edCobVConsultarCPFCNPJ: TEdit;
    edCobVConsultarPagina: TSpinEdit;
    edCobVConsultarItensPag: TSpinEdit;
    cbCobVConsultarLocation: TCheckBox;
    cbCobVConsultarStatus: TComboBox;
    tsCobVCancelarCobranca: TTabSheet;
    pnCobVCancelarParams: TPanel;
    lbCobVCancelarTxID: TLabel;
    edCobVCancelarTxID: TEdit;
    btCobVCancelar: TBitBtn;
    mmCobVCancelar: TMemo;
    pnCobVCancelarRodape: TPanel;
    btCobVCancelarLimpar: TBitBtn;
    tsSimularPagamento: TTabSheet;
    pcSimularPagamento: TPageControl;
    tsBBSimularPagamento: TTabSheet;
    Panel12: TPanel;
    btBBSimulaPagamento_Limpar: TBitBtn;
    mBBSimulaPagamento: TMemo;
    pBBSimulaPagamento: TPanel;
    lConsultarPixE2eid2: TLabel;
    edtBBSimulaPagamento_pixCopiaECola: TEdit;
    btBBSimulaPagamento_Executar: TBitBtn;
    tsPagSeguroSimularPagamento: TTabSheet;
    pnPagSeguroLimpar: TPanel;
    btPagSeguroLimpar: TBitBtn;
    mmPagSeguroResp: TMemo;
    pnPagSeguroCabecalho: TPanel;
    lbPagSeguroTxID: TLabel;
    lbPagSeguroTokenPay: TLabel;
    edPagSeguroTxID: TEdit;
    btPagSeguroPagar: TBitBtn;
    edPagSeguroTokenPay: TEdit;
    tsPIXPDVSimularPagamento: TTabSheet;
    pnPixPDVSimularPagto: TPanel;
    lbPixPDVQRCodeID: TLabel;
    edPixPDVSimularPagtoQRCodeID: TEdit;
    btPixPDVSimularPagto: TBitBtn;
    mmPixPDVSimularPagto: TMemo;
    pnPixPDVSimularPagtoLimpar: TPanel;
    btPixPDVSimularPagtoLimpar: TBitBtn;
    tsMateraSimularPagamento: TTabSheet;
    pnMateraSimularPagamento: TPanel;
    lbMateraSimularPagamento: TLabel;
    tsQRCode: TTabSheet;
    pgQRCode: TPageControl;
    tsQRCodeEstatico: TTabSheet;
    pQREDados: TPanel;
    Valor: TLabel;
    Label109: TLabel;
    Label110: TLabel;
    edtQREInfoAdicional: TEdit;
    edtQRETxId: TEdit;
    fleQREValor: TEdit;
    btQREGerar: TBitBtn;
    pQREGerado: TPanel;
    imgQRE: TImage;
    pQREMemo: TPanel;
    mQRE: TMemo;
    Panel1: TPanel;
    btQREColar: TBitBtn;
    btQREAnalisar: TBitBtn;
    tsQRCodeDinamico: TTabSheet;
    pQRDDados: TPanel;
    lbQRDLocation: TLabel;
    lbQRDTxID: TLabel;
    edQRDLocation: TEdit;
    btQRDGerar: TBitBtn;
    edQRDTxID: TEdit;
    pQREGerado1: TPanel;
    imgQRD: TImage;
    pQRDMemo: TPanel;
    mQRD: TMemo;
    Panel8: TPanel;
    btQRDColar: TBitBtn;
    btQRDAnalisar: TBitBtn;
    pLogs: TPanel;
    Label111: TLabel;
    mLog: TMemo;
    Panel2: TPanel;
    btLogLimpar: TBitBtn;
    ImageList1: TImageList;
    btnTestePIX: TButton;
    btnTestePixVoltar: TButton;
    imgErrPSP: TImage;
    cbxPSPAtual: TComboBox;
    Label49: TLabel;
    pnFluxoBackground: TPanel;
    pnFluxoPagto: TPanel;
    gbFluxoCliente: TGroupBox;
    pnFluxoCliente: TPanel;
    lbFluxoClienteDoc: TLabel;
    edFluxoClienteNome: TEdit;
    edFluxoClienteDoc: TEdit;
    gbFluxoItens: TGroupBox;
    pnFluxoDadosItem: TPanel;
    lbFluxoItemEAN: TLabel;
    lbFluxoItemValor: TLabel;
    lbFluxoItemDescricao: TLabel;
    edFluxoItemEAN: TEdit;
    edFluxoItemDescricao: TEdit;
    btFluxoItemIncluir: TBitBtn;
    btFluxoItemExcluir: TBitBtn;
    edFluxoItemValor: TEdit;
    gdFluxoItens: TStringGrid;
    gbFluxoStatus: TGroupBox;
    pnFluxoStatus: TPanel;
    pnFluxoDiv1: TPanel;
    pnFluxoDiv2: TPanel;
    pnFluxoDiv3: TPanel;
    btnLerParametrosPIX: TBitBtn;
    pnlRecebimento: TPanel;
    imFluxoQRCode: TImage;
    btFluxoPagar: TBitBtn;
    btFluxoCancelarCobranca: TBitBtn;
    btFluxoEstornarPagto: TBitBtn;
    lbFluxoCopiaECola: TLabel;
    edFluxoCopiaECola: TEdit;
    Panel7: TPanel;
    pnpSombra: TPanel;
    btFluxoCopiaECola: TSpeedButton;
    lblTotalVendaPix: TLabel;
    Label100: TLabel;
    gbFluxoTotal: TGroupBox;
    pnFluxoTotalStr: TPanel;
    btnEncerraVenda: TButton;
    btFluxoNovaVenda: TBitBtn;
    edtNossoNumeroCorrespondente: TEdit;
    Label96: TLabel;
    btnBoletoBaixa: TButton;
    btnAlterarBoleto: TButton;
    cmbTipoOcorrencia: TComboBox;
    Label101: TLabel;
    btnFecharTelaPagamento: TSpeedButton;
    pnpCfgBoleto: TPanel;
    Label4: TLabel;
    Shape4: TShape;
    pnpBoletoPrincipal: TPanel;
    Shape1: TShape;
    Label1: TLabel;
    btnConfigBoleto: TSpeedButton;
    procedure ACBrPSPBancoDoBrasil1QuandoReceberRespostaHttp(const AURL,
      AMethod: string; RespHeaders: TStrings; var AResultCode: Integer;
      var RespostaHttp: AnsiString);
    procedure btC6BankAcharCertificadoClick(Sender: TObject);
    procedure btC6BankAcharChavePrivadaClick(Sender: TObject);
    procedure btCancelarCobrancaClick(Sender: TObject);
    procedure btCobVCancelarClick(Sender: TObject);
    procedure btCobVConsultarClick(Sender: TObject);
    procedure btCobVConsultarListaClick(Sender: TObject);
    procedure btConsultarCobrancaImediataClick(Sender: TObject);
    procedure btConsultarCobrancasClick(Sender: TObject);
    procedure btConsultarDevolucaoPixClick(Sender: TObject);
    procedure btConsultarPixClick(Sender: TObject);
    procedure btConsultarPixRecebidosClick(Sender: TObject);
    procedure btCriarCobrancaImediataClick(Sender: TObject);
    procedure btCriarCobVClick(Sender: TObject);
    procedure btFluxoCancelarCobrancaClick(Sender: TObject);
    procedure btFluxoEstornarPagtoClick(Sender: TObject);
    procedure btFluxoNovaVendaClick(Sender: TObject);
    procedure btFluxoPagarClick(Sender: TObject);
    procedure btnAlterarBoletoClick(Sender: TObject);
    procedure btnBoletoBaixaClick(Sender: TObject);
    procedure btnBoletoMenuClick(Sender: TObject);
    procedure btnBoletoTesteClick(Sender: TObject);
    procedure btnCfgSalvaBoletoClick(Sender: TObject);
    procedure btnCfgSalvaPixClick(Sender: TObject);
    procedure btnConfigBoletoClick(Sender: TObject);
    procedure btnConfigPixClick(Sender: TObject);
    procedure btnConsultarBoletoAPIClick(Sender: TObject);
    procedure btnEncerraVendaClick(Sender: TObject);
    procedure btnFecharTelaPagamentoClick(Sender: TObject);
    procedure btnGerarRemessaClick(Sender: TObject);
    procedure btnImprimeBoletoClick(Sender: TObject);
    procedure btnIncluiBoletoClick(Sender: TObject);
    procedure btnLerRetornoClick(Sender: TObject);
    procedure btnLimparListaTituloClick(Sender: TObject);
    procedure btnPesArqLogWSClick(Sender: TObject);
    procedure btnPesPathLogWsClick(Sender: TObject);
    procedure btnPesqCRTClick(Sender: TObject);
    procedure btnPesqFR3Click(Sender: TObject);
    procedure btnPesqKEYClick(Sender: TObject);
    procedure btnPesqLogoBancoClick(Sender: TObject);
    procedure btnPesqRemessaClick(Sender: TObject);
    procedure btnPesqRetornoClick(Sender: TObject);
    procedure btnPixMenuClick(Sender: TObject);
    procedure btnRegistrarBoletoAPIClick(Sender: TObject);
    procedure btnSairMenuClick(Sender: TObject);
    procedure btnTestePIXClick(Sender: TObject);
    procedure btnTestePixVoltarClick(Sender: TObject);
    procedure btnTotalListaBoletoClick(Sender: TObject);
    procedure btQRDGerarClick(Sender: TObject);
    procedure btQREAnalisarClick(Sender: TObject);
    procedure btQREColarClick(Sender: TObject);
    procedure btQREGerarClick(Sender: TObject);
    procedure btSolicitarDevolucaoPixClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbC6BankTipoChaveChange(Sender: TObject);
    procedure cbxAmbienteChange(Sender: TObject);
    procedure cbxLayOutChange(Sender: TObject);
    procedure cbxMotorRelatorioChange(Sender: TObject);
    // procedure cbxRecebedorUFChange(Sender: TObject);
    procedure edC6BankChavePIXChange(Sender: TObject);
    procedure edtRecebedorCEPChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmConsultarDevolucaoTimer(Sender: TObject);
    procedure tmConsultarPagtoTimer(Sender: TObject);
    procedure EnviaBoleto(ATipoEnvio: TOperacao);
  private
    { Private declarations }
{$IFDEF GERADOR_FORTES_REPORT}
    FACBrBoletoFCRL: TACBrBoletoFCFortes;
{$ENDIF}
{$IFDEF GERADOR_FAST_REPORT}
    FACBrBoletoFCFR: TACBrBoletoFCFR;
{$ENDIF}
    fFluxoDados: TFluxoPagtoDados;
    FACBrBoletoFPDF: TACBrBoletoFPDF;

    procedure MostraTela(ATela: TIndicadorTela);

    procedure SelecionaPath(AComponent: TEdit);
    procedure CarregarTipoDocumento;
    procedure CarregarCaracTitulo;
    procedure CarregarResponsavelEmissao;
    procedure CarregarVersaoCnab;
    procedure CarregarNivelLogWS;
    procedure CarregarCodMoraJuros;
    procedure CarregarTipoMulta;
    procedure CarregarLayoutMotor;
    procedure CarregarLayOutImpressao;
    procedure CarregarTipoDesconto;
    procedure CarregarTipoOcorrenciaBoleto;

    procedure GravarConfiguracaoPix;
    procedure InicializarComponentesPixDefault;
    function GetNomeArquivoConfiguracao: String;
    procedure AdicionarLinhaLog(AMensagem: String);
    Function FormatarMascaraDinamica(const AValue: String;
      const Mascara: String): String;
    procedure TratarException(Sender: TObject; E: Exception);

//  function SelectFolder: string;
    procedure AplicarConfiguracoesAoComponente;
    procedure GravarIniComponente;
    procedure LerIniComponente(const ADialog: Boolean = False);
    procedure AplicarConfiguracoesComponenteATela;

    procedure GerarDadosTeste;
    procedure LerConfiguracaoPix;
    procedure AplicarConfiguracao;
    procedure ConfigurarACBrPIXCD;
    procedure ConfigurarACBrPSPs;
    procedure LigarAlertasdeErrosDeConfiguracaoPSPC6Bank;
    procedure ValidarCertificadoPSPC6Bank;
    procedure ValidarChavePSPC6Bank;
    function AdicionarPathAplicacao(const AFileName: String): String;

    procedure DoDepoisAutenticar(const aToken: String;
      const aValidadeToken: TDateTime);
    procedure DoAntesAutenticar(var aToken: String;
      var aValidadeToken: TDateTime);
    procedure VerificarConfiguracao;
    procedure VerificarConfiguracaoPIXCD;
    procedure MostrarPixEmLinhas(const NomePix: String; APix: TACBrPIX;
      SL: TStrings);
    procedure MostrarDevolucaoEmLinhas(const NomeDev: String;
      ADev: TACBrPIXDevolucao; SL: TStrings);
    procedure MostrarCobrancaEmLinhas(const NomeCobranca: String;
      ACob: TACBrPIXCobGerada; SL: TStrings);
    procedure VerificarConfiguracaoPix;

    function FormatarJSON(const AJSON: String): String;
    procedure AnalisarBRCode(aBRCode: TACBrBRCode);
    procedure PintarQRCodeEstatico;
    procedure PintarQRCodeDinamico;
    procedure InicializarBitmaps;
    procedure ReiniciarFluxo;
    procedure LimparInterfaceFluxoItem;
    procedure InicializarGridFluxo;
    function GetInfoOpenSSL: String;
    procedure AdicionarItemGridFluxo(aEan, aDescricao: String; aValor: Double);

    procedure AtualizarTotal;
    procedure AtualizarStatus(aStatus: TACBrPIXStatusCobranca = stcNENHUM;
      aStatusDevolucao: TACBrPIXStatusDevolucao = stdNENHUM);
    procedure AvaliarInterfaceFluxo;
    procedure AvaliarInterfaceFluxoItem;
    procedure HabilitarInterface(aLiberada: Boolean);
    procedure ConsultarCobranca;
    procedure EstornarPagamento;
    procedure ConsultarDevolucao;
    procedure EncerraVenda(LFlag: Boolean);
    procedure zoomin(LFlag: Boolean);
    procedure VisualizaReposta(aArquivo: string);

  public
    { Public declarations }
    property FluxoDados: TFluxoPagtoDados read fFluxoDados;
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;
  end;

var
  frmPrincipal: TfrmPrincipal;
  Tela: TIndicadorTela;

CONST
  CMaxConsultas = 36;
  CURL_ACBR = 'https://projetoacbr.com.br/tef/';
  CURL_MateraPagto = 'https://flagship-payment-app.vercel.app/';
  CURL_MCC = 'https://classification.codes/classifications/industry/mcc/';

  MOTOR_NAO_SELECIONADO =
    'MOTOR DE RELATÓRIO NÃO FOI SELECIONADO, VERIFIQUE!!!';

  FILTER_RETORNO = '*.txt|*.txt|*.ret|*.ret|*.*|*.*';
  FILTER_INI = '*.ini|*.ini|*.*|*.*';

implementation

uses pcnConversao,
  uresposta;

{$R *.dfm}

procedure TfrmPrincipal.ACBrPSPBancoDoBrasil1QuandoReceberRespostaHttp
  (const AURL, AMethod: string; RespHeaders: TStrings; var AResultCode: Integer;
  var RespostaHttp: AnsiString);
Var
  jsRet, js: TACBrJSONObject;
  ja, jsArr: TACBrJSONArray;
  I: Integer;

  function GetDetalhesPagador(AJSON: TACBrJSONObject): String;
  var
    jPag: TACBrJSONObject;
  begin
    jPag := AJSON.AsJSONObject['pagador'];
    if Assigned(jPag) then
      Result := AJSON.AsString['infoPagador'] + ' ' + jPag.AsString['cpf'] +
        jPag.AsString['cnpj'] + ' - ' + jPag.AsString['nome'];
  end;

begin
  if (AMethod = ChttpMethodGET) and (AResultCode = HTTP_OK) and
    (Pos(cEndPointPix, AURL) > 0) then
  begin
    jsRet := TACBrJSONObject.Parse(String(RespostaHttp));
    jsArr := jsRet.AsJSONArray['pix'];
    try
      if Assigned(jsArr) and (jsArr.Count > 0) then
      begin
        ja := TACBrJSONArray.Create;

        for I := 0 to jsArr.Count - 1 do
        begin
          js := jsArr.ItemAsJSONObject[I];
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

procedure TfrmPrincipal.btnBoletoMenuClick(Sender: TObject);
begin
  MostraTela(itBoleto)
end;

procedure TfrmPrincipal.btnCfgSalvaBoletoClick(Sender: TObject);
begin
  AplicarConfiguracoesAoComponente;
  GravarIniComponente;
  MostraTela(itBoleto)
end;

procedure TfrmPrincipal.btnCfgSalvaPixClick(Sender: TObject);
begin
  GravarConfiguracaoPix;
  MostraTela(itPix)
end;

procedure TfrmPrincipal.btnConfigBoletoClick(Sender: TObject);
begin
  MostraTela(itCfgBoleto)
end;

procedure TfrmPrincipal.btnConfigPixClick(Sender: TObject);
begin
  MostraTela(itCfgPix)
end;

procedure TfrmPrincipal.btnPesArqLogWSClick(Sender: TObject);
begin
  SelecionaPath(edtArquivoLog)
end;

procedure TfrmPrincipal.btnPesPathLogWsClick(Sender: TObject);
begin
  SelecionaPath(edtPathLog);
end;

procedure TfrmPrincipal.btnPesqCRTClick(Sender: TObject);
begin
  SelecionaPath(edtPathArqCRT);
end;

procedure TfrmPrincipal.btnPesqKEYClick(Sender: TObject);
begin
  SelecionaPath(edtPathArqKEY);
end;

procedure TfrmPrincipal.btnPesqRemessaClick(Sender: TObject);
begin
  SelecionaPath(edtPathRemessa);
end;

procedure TfrmPrincipal.btnPesqRetornoClick(Sender: TObject);
begin
  SelecionaPath(edtPathRetorno);
end;

procedure TfrmPrincipal.btnPixMenuClick(Sender: TObject);
begin
  MostraTela(itPix)
end;

procedure TfrmPrincipal.CarregarCaracTitulo;
var
  LCaracTitulo: TACBrCaracTitulo;
begin
  cbxCaracteristicaTitulo.Items.Clear;
  for LCaracTitulo := Low(TACBrCaracTitulo) to High(TACBrCaracTitulo) do
    cbxCaracteristicaTitulo.Items.Add(GetEnumName(TypeInfo(TACBrCaracTitulo),
      Integer(LCaracTitulo)));
  cbxCaracteristicaTitulo.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarCodMoraJuros;
var
  LMoraJuros: TACBrCodigoJuros;
begin
  cbxCodMoraJuros.Items.Clear;
  for LMoraJuros := Low(TACBrCodigoJuros) to High(TACBrCodigoJuros) do
    cbxCodMoraJuros.Items.Add(GetEnumName(TypeInfo(TACBrCodigoJuros),
      Integer(LMoraJuros)));
  cbxCodMoraJuros.ItemIndex := 4;
end;

procedure TfrmPrincipal.CarregarLayOutImpressao;
var
  LLayoutImpressao: TACBrBolLayOut;
begin
  cbxLayOut.Items.Clear;
  for LLayoutImpressao := Low(TACBrBolLayOut) to High(TACBrBolLayOut) do
    cbxLayOut.Items.Add(GetEnumName(TypeInfo(TACBrBolLayOut),
      Integer(LLayoutImpressao)));
  cbxLayOut.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarLayoutMotor;
begin
{$IFDEF GERADOR_FORTES_REPORT}
  FACBrBoletoFCRL := TACBrBoletoFCFortes.Create(FACBrBoleto);
  cbxMotorRelatorio.AddItem('Fortes Reports', FACBrBoletoFCRL);
{$ENDIF}
{$IFDEF GERADOR_FAST_REPORT}
  FACBrBoletoFCFR := TACBrBoletoFCFR.Create(FACBrBoleto);
  cbxMotorRelatorio.AddItem('Fast Reports', FACBrBoletoFCFR);
{$ENDIF}
  FACBrBoletoFPDF := TACBrBoletoFPDF.Create(FACBrBoleto);
  cbxMotorRelatorio.AddItem('FDPF', FACBrBoletoFPDF);
  cbxMotorRelatorio.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarNivelLogWS;
var
  LNivel: TNivelLog;
begin
  cbxLogNivel.Items.Clear;
  for LNivel := Low(TNivelLog) to High(TNivelLog) do
    cbxLogNivel.Items.Add(GetEnumName(TypeInfo(TNivelLog), Integer(LNivel)));
  cbxLogNivel.ItemIndex := 4;
end;

procedure TfrmPrincipal.CarregarResponsavelEmissao;
var
  LResponsavelEmissao: TACBrResponEmissao;
begin
  cbxResponsavelEmissao.Items.Clear;
  for LResponsavelEmissao := Low(TACBrResponEmissao)
    to High(TACBrResponEmissao) do
    cbxResponsavelEmissao.Items.Add(GetEnumName(TypeInfo(TACBrResponEmissao),
      Integer(LResponsavelEmissao)));
  cbxResponsavelEmissao.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarTipoDesconto;
var
  LTipoDesconto: TACBrTipoDesconto;
begin
  cbxTipoDesconto.Items.Clear;
  for LTipoDesconto := Low(TACBrTipoDesconto) to High(TACBrTipoDesconto) do
    cbxTipoDesconto.Items.Add(GetEnumName(TypeInfo(TACBrTipoDesconto),
      Integer(LTipoDesconto)));
  cbxTipoDesconto.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarTipoDocumento;
begin
  cbxTipoDocumento.Items.Clear;
  cbxTipoDocumento.Items.Add('Tradicional');
  cbxTipoDocumento.Items.Add('Escritural');
  cbxTipoDocumento.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarTipoMulta;
begin
  cbxTipoMulta.Items.Clear;
  cbxTipoMulta.Items.Add('% Percentual');
  cbxTipoMulta.Items.Add('R$ Monetario');
  cbxTipoMulta.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarTipoOcorrenciaBoleto;
begin
  cmbTipoOcorrencia.Items.Clear;
  cmbTipoOcorrencia.Items.Add(GetEnumName(TypeInfo(TACBrTipoOcorrencia),
    Integer(toRemessaRegistrar)));
  cmbTipoOcorrencia.Items.Add(GetEnumName(TypeInfo(TACBrTipoOcorrencia),
    Integer(toRemessaAlterarVencimento)));
  cmbTipoOcorrencia.Items.Add(GetEnumName(TypeInfo(TACBrTipoOcorrencia),
    Integer(toRemessaAlterarValorTitulo)));
  cmbTipoOcorrencia.Items.Add(GetEnumName(TypeInfo(TACBrTipoOcorrencia),
    Integer(toRemessaAlterarDesconto)));
  cmbTipoOcorrencia.Items.Add(GetEnumName(TypeInfo(TACBrTipoOcorrencia),
    Integer(toRemessaAlterarJurosMora)));
  cmbTipoOcorrencia.Items.Add(GetEnumName(TypeInfo(TACBrTipoOcorrencia),
    Integer(toRemessaAlterarMulta)));
  cmbTipoOcorrencia.ItemIndex := 0;
end;

procedure TfrmPrincipal.CarregarVersaoCnab;
var
  LVersaoCNAB: TACBrLayoutRemessa;
begin
  cbxCNAB.Items.Clear;
  for LVersaoCNAB := Low(TACBrLayoutRemessa) to High(TACBrLayoutRemessa) do
    cbxCNAB.Items.Add(GetEnumName(TypeInfo(TACBrLayoutRemessa),
      Integer(LVersaoCNAB)));
  cbxCNAB.ItemIndex := 0;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  { escondendo tabs }
  for I := 0 to pagPrincipal.PageCount - 1 do
    pagPrincipal.Pages[I].TabVisible := False;

  MostraTela(itNenhuma);
  { carregamento Combos }
  // CarregarTipoDistribuicao;
  CarregarTipoDocumento;
  CarregarCaracTitulo;
  CarregarResponsavelEmissao;
  // CarregarTipoCarteira;
  CarregarVersaoCnab;
  CarregarNivelLogWS;
  CarregarCodMoraJuros;
  CarregarTipoMulta;
  CarregarLayoutMotor;
  CarregarLayOutImpressao;
  CarregarTipoDesconto;
  CarregarTipoOcorrenciaBoleto;
  { carregando informações Salvas }
  LerIniComponente;
  AplicarConfiguracoesComponenteATela;
  InicializarComponentesPixDefault;
  Application.OnException := TratarException;

  InicializarBitmaps;
  Application.OnException := TratarException;
  LerConfiguracaoPix; // LerConfiguracao;
  VerificarConfiguracaoPIXCD;
  VerificarConfiguracao;
  ReiniciarFluxo;
  AdicionarLinhaLog(GetInfoOpenSSL);
  EncerraVenda(False);

end;

procedure TfrmPrincipal.MostrarCobrancaEmLinhas(const NomeCobranca: String;
  ACob: TACBrPIXCobGerada; SL: TStrings);
var
  I: Integer;
begin
  SL.Add(NomeCobranca + '.calendario.criacao: ' +
    FormatDateTimeBr(ACob.calendario.criacao));
  SL.Add(NomeCobranca + '.calendario.expiracao: ' +
    IntToStr(ACob.calendario.expiracao));
  SL.Add(NomeCobranca + '.txId: ' + ACob.TxID);
  SL.Add(NomeCobranca + '.revisao: ' + IntToStr(ACob.revisao));
  if (ACob.devedor.nome <> '') then
  begin
    SL.Add(NomeCobranca + '.devedor.nome: ' + ACob.devedor.nome);
    if (ACob.devedor.cpf <> '') then
      SL.Add(NomeCobranca + '.devedor.cpf: ' + ACob.devedor.cpf)
    else
      SL.Add(NomeCobranca + '.devedor.cnpj: ' + ACob.devedor.cnpj)
  end;
  SL.Add(NomeCobranca + '.loc.id: ' + IntToStr(ACob.loc.id));
  SL.Add(NomeCobranca + '.loc.txId: ' + ACob.loc.TxID);
  SL.Add(NomeCobranca + '.loc.location: ' + ACob.loc.location);
  SL.Add(NomeCobranca + '.loc.criacao: ' + FormatDateTimeBr(ACob.loc.criacao));
  SL.Add(NomeCobranca + '.location: ' + ACob.location);
  SL.Add(NomeCobranca + '.status: ' + PIXStatusCobrancaToString(ACob.status));
  SL.Add(NomeCobranca + '.valor.original: ' +
    FormatFloatBr(ACob.Valor.original));
  SL.Add(NomeCobranca + '.valor.modalidadeAlteracao: ' +
    BoolToStr(ACob.Valor.modalidadeAlteracao, True));
  if (ACob.Valor.retirada.saque.Valor <> 0) then
  begin
    SL.Add(NomeCobranca + '.valor.retirada.saque.valor: ' +
      FormatFloatBr(ACob.Valor.retirada.saque.Valor));
    SL.Add(NomeCobranca + '.valor.retirada.saque.modalidadeAlteracao: ' +
      BoolToStr(ACob.Valor.retirada.saque.modalidadeAlteracao, True));
    SL.Add(NomeCobranca + '.valor.retirada.saque.modalidadeAgente: ' +
      PIXModalidadeAgenteToString(ACob.Valor.retirada.saque.modalidadeAgente));
    SL.Add(NomeCobranca + '.valor.retirada.saque.prestadorDoServicoDeSaque: ' +
      IntToStr(ACob.Valor.retirada.saque.prestadorDoServicoDeSaque));
  end;
  if (ACob.Valor.retirada.troco.Valor <> 0) then
  begin
    SL.Add(NomeCobranca + '.valor.retirada.troco.valor: ' +
      FormatFloatBr(ACob.Valor.retirada.troco.Valor));
    SL.Add(NomeCobranca + '.valor.retirada.troco.modalidadeAlteracao: ' +
      BoolToStr(ACob.Valor.retirada.troco.modalidadeAlteracao, True));
    SL.Add(NomeCobranca + '.valor.retirada.troco.modalidadeAgente: ' +
      PIXModalidadeAgenteToString(ACob.Valor.retirada.troco.modalidadeAgente));
    SL.Add(NomeCobranca + '.valor.retirada.troco.prestadorDoServicoDeSaque: ' +
      IntToStr(ACob.Valor.retirada.troco.prestadorDoServicoDeSaque));
  end;
  if (ACob.pixCopiaECola <> '') then
    SL.Add(NomeCobranca + '.pixCopiaECola: ' + ACob.pixCopiaECola);

  if ACob is TACBrPIXCobCompleta then
  begin
    for I := 0 to TACBrPIXCobCompleta(ACob).pix.Count - 1 do
      MostrarPixEmLinhas('  ' + NomeCobranca + '.Pix[' + IntToStr(I) + ']',
        TACBrPIXCobCompleta(ACob).pix[I], SL);
  end;
end;

procedure TfrmPrincipal.MostrarDevolucaoEmLinhas(const NomeDev: String;
  ADev: TACBrPIXDevolucao; SL: TStrings);
begin
  SL.Add(NomeDev + '.valor: ' + FormatFloatBr(ADev.Valor));
  SL.Add(NomeDev + '.natureza: ' + PIXNaturezaDevolucaoToString(ADev.natureza));
  SL.Add(NomeDev + '.descricao: ' + ADev.descricao);
  SL.Add(NomeDev + '.id: ' + ADev.id);
  SL.Add(NomeDev + '.rtrId: ' + ADev.rtrId);
  SL.Add(NomeDev + '.horario.solicitacao: ' +
    FormatDateTimeBr(ADev.horario.solicitacao));
  SL.Add(NomeDev + '.horario.liquidacao: ' +
    FormatDateTimeBr(ADev.horario.liquidacao));
  SL.Add(NomeDev + '.status: ' + PIXStatusDevolucaoToString(ADev.status));
  SL.Add(NomeDev + '.motivo: ' + ADev.motivo);

end;

procedure TfrmPrincipal.MostrarPixEmLinhas(const NomePix: String;
  APix: TACBrPIX; SL: TStrings);
var
  I: Integer;
begin
  SL.Add(NomePix + '.endToEndId: ' + APix.endToEndId);
  SL.Add(NomePix + '.TxId: ' + APix.TxID);
  SL.Add(NomePix + '.valor: ' + FormatFloatBr(APix.Valor));
  if not APix.componentesValor.IsEmpty then
  begin
    SL.Add(NomePix + '.componentesValor.original.valor: ' +
      FormatFloatBr(APix.componentesValor.original.Valor));
    if (APix.componentesValor.saque.Valor > 0) then
      SL.Add(NomePix + '.componentesValor.saque.valor: ' +
        FormatFloatBr(APix.componentesValor.saque.Valor));
    if (APix.componentesValor.troco.Valor > 0) then
      SL.Add(NomePix + '.componentesValor.troco.valor: ' +
        FormatFloatBr(APix.componentesValor.troco.Valor));
    if (APix.componentesValor.juros.Valor > 0) then
      SL.Add(NomePix + '.componentesValor.juros.valor: ' +
        FormatFloatBr(APix.componentesValor.juros.Valor));
    if (APix.componentesValor.multa.Valor > 0) then
      SL.Add(NomePix + '.componentesValor.multa.valor: ' +
        FormatFloatBr(APix.componentesValor.multa.Valor));
    if (APix.componentesValor.abatimento.Valor > 0) then
      SL.Add(NomePix + '.componentesValor.abatimento.valor: ' +
        FormatFloatBr(APix.componentesValor.abatimento.Valor));
    if (APix.componentesValor.desconto.Valor > 0) then
      SL.Add(NomePix + '.componentesValor.desconto.valor: ' +
        FormatFloatBr(APix.componentesValor.desconto.Valor));
  end;
  SL.Add(NomePix + '.chave: ' + APix.chave);
  SL.Add(NomePix + '.horario: ' + FormatDateTimeBr(APix.horario));
  SL.Add(NomePix + '.infoPagador: ' + APix.infoPagador);
  SL.Add(NomePix + '.devolucoes: ' + IntToStr(APix.devolucoes.Count));

  for I := 0 to APix.devolucoes.Count - 1 do
    MostrarDevolucaoEmLinhas(NomePix + '.devolucoes[' + IntToStr(I) + ']',
      APix.devolucoes[I], SL);
end;

procedure TfrmPrincipal.MostraTela(ATela: TIndicadorTela);
begin
  case ATela of
    itBoleto:
      pagPrincipal.ActivePageIndex := 1;
    itPix:
      pagPrincipal.ActivePageIndex := 2;
    itNenhuma:
      pagPrincipal.ActivePageIndex := 0;
    itCfgPix:
      pagPrincipal.ActivePageIndex := 3;
    itCfgBoleto:
      pagPrincipal.ActivePageIndex := 4;
    itTestePix:
      pagPrincipal.ActivePageIndex := 5;
  end;
end;

procedure TfrmPrincipal.SelecionaPath(AComponent: TEdit);
begin
  OpenDialog1.Execute;
  if AComponent = edtPathRemessa then
    edtPathRemessa.Text := ExtractFilePath(OpenDialog1.FileName);
  if AComponent = edtPathRetorno then
    edtPathRetorno.Text := OpenDialog1.FileName;
  if AComponent = edtPathArqCRT then
    edtPathArqCRT.Text := OpenDialog1.FileName;
  if AComponent = edtPathArqKEY then
    edtPathArqKEY.Text := OpenDialog1.FileName;
  if AComponent = edtArquivoLog then
    edtArquivoLog.Text := ExtractFileName(OpenDialog1.FileName);
  if AComponent = edC6BankChavePrivada then
    edC6BankChavePrivada.Text := ExtractFileName(OpenDialog1.FileName);
  if AComponent = edC6BankCertificado then
    edC6BankCertificado.Text := ExtractFileName(OpenDialog1.FileName);
  if AComponent = edtPathLog then
    edtPathLog.Text := ExtractFilePath(OpenDialog1.FileName);
  if AComponent = edtPathRemessa then
    edtPathRemessa.Text := ExtractFilePath(OpenDialog1.FileName);
  if AComponent = edtPathFR3 then
    edtPathFR3.Text := ExtractFilePath(OpenDialog1.FileName);
  if AComponent = edtPathLogoMarca then
    edtPathLogoMarca.Text := ExtractFilePath(OpenDialog1.FileName);
end;

(*
function TfrmPrincipal.SelectFolder: string;
var
  LDialog: TFileOpenDialog;
begin
  LDialog := TFileOpenDialog.Create(nil);
  Result := '';
  try
    LDialog.Options := LDialog.Options + [fdoPickFolders];
    if LDialog.Execute then
      Result := LDialog.FileName;
  finally
    LDialog.Free;
  end;

end;
*)

procedure TfrmPrincipal.AplicarConfiguracoesAoComponente;
var
  Beneficiario: TACBrCedente;
  Banco: TACBrBanco;
  Boleto: TACBrBoleto;
  WebService: TACBrWebService;
  BeneficiarioWS: TACBrCedenteWS;
  CobAnterior: TACBrTipoCobranca;
begin
  Boleto := FACBrBoleto;
  WebService := Boleto.Configuracoes.WebService;

  Boleto.Banco.TipoCobranca := cobBancoC6;

  Boleto.LayoutRemessa := TACBrLayoutRemessa(0);
  Boleto.Homologacao := ckbEmHomologacao.Checked;

  Boleto.ImprimirMensagemPadrao := ckbImprimirMensagemPadrao.Checked;
  Boleto.LeCedenteRetorno := ckbLerCedenteArquivoRetorno.Checked;
  Boleto.RemoveAcentosArqRemessa := ckbRemoverAcentuacaoRemessa.Checked;

  Beneficiario := Boleto.Cedente;
  BeneficiarioWS := Beneficiario.CedenteWS;

  Beneficiario.Agencia := edtAgencia.Text;
  Beneficiario.AgenciaDigito := edtAgenciaDV.Text;
  Beneficiario.Conta := edtConta.Text;
  Beneficiario.ContaDigito := edtContaDV.Text;
  Beneficiario.Convenio := edtConvenio.Text;
  Beneficiario.Modalidade := edtModalidade.Text;
  Beneficiario.Operacao := edtOperacao.Text;
  Beneficiario.CodigoCedente := edtCodigoCedente.Text;

  if Length(OnlyNumber(edtBenifCNPJ.Text)) = 14 then
    Beneficiario.TipoInscricao := pJuridica
  else
    Beneficiario.TipoInscricao := pFisica;

  // Beneficiario.TipoDocumento                 := TACBrTipoDocumento(cbxTipoDocumento.ItemIndex);

  // Beneficiario.IdentDistribuicao             := TACBrIdentDistribuicao(cbxTipoDistribuicao.itemIndex);
  Beneficiario.ResponEmissao := TACBrResponEmissao
    (cbxResponsavelEmissao.ItemIndex);
  Beneficiario.CaracTitulo :=
    TACBrCaracTitulo(cbxCaracteristicaTitulo.ItemIndex);
  // Beneficiario.TipoCarteira                  := TACBrTipoCarteira(cbxTipoCarteira.itemIndex);

  Beneficiario.CNPJCPF := edtBenifCNPJ.Text;
  Beneficiario.nome := edtBenifRazao.Text;
  Beneficiario.FantasiaCedente := edtBenifFantasia.Text;
  Beneficiario.Logradouro := edtBenifEndereco.Text;
  Beneficiario.NumeroRes := edtBenifNum.Text;
  Beneficiario.Complemento := edtBenifComplemento.Text;
  Beneficiario.Bairro := edtBenifBairro.Text;
  Beneficiario.Cidade := edtBenifCidade.Text;
  Beneficiario.UF := edtBenifUF.Text;
  Beneficiario.CEP := edtBenifCEP.Text;
  Beneficiario.Telefone := edtBenifTelefone.Text;

  Banco := Boleto.Banco;
  Banco.TipoCobranca := cobBancoC6;

  BeneficiarioWS.ClientID := edtClientID.Text;
  BeneficiarioWS.ClientSecret := edtClientSecret.Text;

  Boleto.Configuracoes.WebService.ArquivoCRT := edtPathArqCRT.Text;
  Boleto.Configuracoes.WebService.ArquivoKEY := edtPathArqKEY.Text;

  BeneficiarioWS.IndicadorPix := chkIndicadorPix.Checked;

  // Beneficiario.PIX.TipoChavePIX := tchAleatoria;
  // Beneficiario.PIX.Chave := '112';

  WebService.Ambiente := TpcnTipoAmbiente(Ord(ckbEmHomologacao.Checked));
  WebService.SSLHttpLib := httpOpenSSL;

  Boleto.Configuracoes.Arquivos.LogNivel := TNivelLog(cbxLogNivel.ItemIndex);
  Boleto.Configuracoes.Arquivos.PathGravarRegistro := edtPathLog.Text;
  Boleto.Configuracoes.Arquivos.NomeArquivoLog := edtArquivoLog.Text;

  // AplicarConfiguracoesComponenteEmail;

  // if Assigned(FACBrBoleto.ACBrBoletoFC) then
  // FACBrBoleto.ACBrBoletoFC.DirLogo := edtPathLogoMarca.Text;

{$IFDEF GERADOR_FAST_REPORT}
  FACBrBoletoFCFR.FastReportFile := edtPathFR3.Text;
  FACBrBoletoFCFR.MostrarPreview := True;
  FACBrBoletoFCFR.MostrarSetup := True;
{$ENDIF}
end;

procedure TfrmPrincipal.btnPesqFR3Click(Sender: TObject);
begin
  SelecionaPath(edtPathFR3);
end;

procedure TfrmPrincipal.btnPesqLogoBancoClick(Sender: TObject);
begin
  SelecionaPath(edtPathLogoMarca);
end;

procedure TfrmPrincipal.GerarDadosTeste;
var
  LVencimento: TDate;
begin
  LVencimento := IncMonth(now, 1);
  { desconto acrescimos }
  edtDataMora.Text := datetostr(LVencimento + 5);
  edtMoraJuros.Text := '3';
  cbxCodMoraJuros.ItemIndex := 1;

  edtDataDesconto.Text := datetostr(LVencimento - 10);
  edtValorDesconto.Text := '10,00';
  cbxTipoDesconto.ItemIndex := 1;

  edtDataAbatimento.Text := datetostr(LVencimento + 2);
  edtValorAbatimento.Text := '1,00';
  edtDataMulta.Text := datetostr(LVencimento + 5);
  edtMulta.Text := '5';
  edtDataProtesto.Text := datetostr(LVencimento + 10);
  edtDataBaixa.Text := datetostr(LVencimento + 50);
  memMensagem.Lines.Text := 'Teste MSG!';

  { informacoes duplicata }
  edtNumeroDoc.Text := '001';
  edtValorDoc.Text := '100,00';
  edtDataDoc.Text := datetostr(now);
  edtVencimento.Text := datetostr(IncMonth(now, 1));
  edtSeuNumero.Text := 'MN00001';
  edtNossoNro.Text := '0001';
  { informacoes sacado }
  edtNome.Text := 'Teste de Nome Sacado';
  edtCPFCNPJ.Text := '50146335015';
  edtEndereco.Text := 'Teste de Endereco';
  edtNumero.Text := '01';
  edtBairro.Text := 'Teste Bairro';
  edtCEP.Text := '01014-000';
  edtComplemento.Text := 'Teste Compl';
  edtCidade.Text := 'Teste Cidade';
  edtUF.Text := 'SP'
end;

function TfrmPrincipal.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName, '_Pix.ini');
end;

procedure TfrmPrincipal.GravarIniComponente;
var
  xPath, xArquivo: String;
  IniFile: TMemIniFile;
begin
  xPath := ExtractFilePath(ParamStr(0));
  xArquivo := ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
  FACBrBoleto.GravarConfiguracao(xPath, xArquivo);

  IniFile := TMemIniFile.Create(xPath + xArquivo);
  try
    (*
      IniFile.WriteString('EMAIL', 'FromEmail', edtFrom.Text);
      IniFile.WriteString('EMAIL', 'FromName' , edtFromName.Text);
      IniFile.WriteString('EMAIL', 'Host', edtHost.Text);
      IniFile.WriteString('EMAIL', 'Port', edtPort.Text);
      IniFile.WriteBool('EMAIL', 'SSL', chkSSL.Checked);
      IniFile.WriteBool('EMAIL', 'TLS', chkTLS.Checked);
      IniFile.WriteString('EMAIL', 'UserName', edtUserName.Text);
      IniFile.WriteString('EMAIL', 'PassWord', edtPassword.Text);
      IniFile.WriteBool('EMAIL', 'MostrarSenha', chkMostrarSenha.Checked);
    *)

    IniFile.WriteBool('Outros', 'EMVFicticio', chkEMVFicticio.Checked);
    IniFile.WriteInteger('Outros', 'MotorRelatorio',
      cbxMotorRelatorio.ItemIndex);
    IniFile.WriteString('PATH', 'PathRemessa', edtPathRemessa.Text);
    IniFile.WriteString('PATH', 'PathRetorno', edtPathRetorno.Text);
    IniFile.WriteString('PATH', 'BOLETOFR3', edtPathFR3.Text);
    IniFile.WriteString('PATH', 'LOGOMARCA', edtPathLogoMarca.Text);
    // IniFile.WriteBool('PATH', 'MostrarSenha', chkMostrarSenha.Checked);

    IniFile.WriteString('BANCO', 'CARTEIRA', edtCarteira.Text);

    IniFile.UpdateFile;

  finally
    IniFile.Free;
  end;
end;

procedure TfrmPrincipal.LerIniComponente(const ADialog: Boolean);
var
  xArquivo: String;
  IniFile: TMemIniFile;
begin
  if ADialog then
  begin
    dlgFile.Filter := FILTER_INI;
    if dlgFile.Execute then
      xArquivo := dlgFile.FileName
    else
      raise Exception.Create
        ('É NECESSÁRIO SELECIONAR O ARQUIVO DE CONFIGURAÇÕES');
  end
  else
    xArquivo := ExtractFilePath(ParamStr(0)) +
      ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');

  if (FileExists(xArquivo)) then
    FACBrBoleto.LerConfiguracao(xArquivo);

  IniFile := TMemIniFile.Create(xArquivo);
  try
    cbxMotorRelatorio.ItemIndex := IniFile.ReadInteger('Outros',
      'MotorRelatorio', 0);
    chkEMVFicticio.Checked := IniFile.ReadBool('Outros', 'EMVFicticio', False);
    edtPathRemessa.Text := IniFile.ReadString('PATH', 'PathRemessa', '');
    edtPathRetorno.Text := IniFile.ReadString('PATH', 'PathRetorno', '');
    edtPathFR3.Text := IniFile.ReadString('PATH', 'BOLETOFR3',
      ExtractFilePath(ParamStr(0)) + 'Report\Boleto.fr3');
    edtPathLogoMarca.Text := IniFile.ReadString('PATH', 'LOGOMARCA',
      '..\..\..\Fontes\ACBrBoleto\Logos\Colorido\png\');
    edtCarteira.Text := IniFile.ReadString('BANCO', 'CARTEIRA', '00');
    // AplicarConfiguracoesEmailNaTela(IniFile);
  finally
    IniFile.Free;
  end;
  cbxMotorRelatorio.OnChange(self);
end;

procedure TfrmPrincipal.AplicarConfiguracoesComponenteATela;
var
  Beneficiario: TACBrCedente;
  BeneficiarioWS: TACBrCedenteWS;
  Banco: TACBrBanco;
  Boleto: TACBrBoleto;
  I: Integer;
begin
  Boleto := FACBrBoleto;
  Boleto.ListadeBoletos.Clear;

  cbxCNAB.ItemIndex := 0;
  ckbEmHomologacao.Checked := Boleto.Homologacao;
  ckbImprimirMensagemPadrao.Checked := Boleto.ImprimirMensagemPadrao;
  ckbLerCedenteArquivoRetorno.Checked := Boleto.LeCedenteRetorno;
  ckbRemoverAcentuacaoRemessa.Checked := Boleto.RemoveAcentosArqRemessa;

  Beneficiario := Boleto.Cedente;

  edtAgencia.Text := Beneficiario.Agencia;
  edtAgenciaDV.Text := Beneficiario.AgenciaDigito;
  edtConta.Text := Beneficiario.Conta;
  edtContaDV.Text := Beneficiario.ContaDigito;
  edtConvenio.Text := Beneficiario.Convenio;
  edtModalidade.Text := Beneficiario.Modalidade;
  edtOperacao.Text := Beneficiario.Operacao;
  edtCodigoCedente.Text := Beneficiario.CodigoCedente;

  edtBenifCNPJ.Text := Beneficiario.CNPJCPF;
  edtBenifRazao.Text := Beneficiario.nome;
  edtBenifFantasia.Text := Beneficiario.FantasiaCedente;
  edtBenifEndereco.Text := Beneficiario.Logradouro;
  edtBenifNum.Text := Beneficiario.NumeroRes;
  edtBenifComplemento.Text := Beneficiario.Complemento;
  edtBenifBairro.Text := Beneficiario.Bairro;
  edtBenifCidade.Text := Beneficiario.Cidade;
  edtBenifUF.Text := Beneficiario.UF;
  edtBenifCEP.Text := Beneficiario.CEP;

  cbxCaracteristicaTitulo.ItemIndex := Ord(Beneficiario.CaracTitulo);
  cbxResponsavelEmissao.ItemIndex := Ord(Beneficiario.ResponEmissao);
  cbxTipoDocumento.ItemIndex :=
    Integer(TACBrTipoDocumento(Beneficiario.TipoDocumento)) - 1;

  Banco := Boleto.Banco;

  BeneficiarioWS := Beneficiario.CedenteWS;
  edtClientID.Text := BeneficiarioWS.ClientID;
  edtClientSecret.Text := BeneficiarioWS.ClientSecret;
  edtPathArqCRT.Text := Boleto.Configuracoes.WebService.ArquivoCRT;
  edtPathArqKEY.Text := Boleto.Configuracoes.WebService.ArquivoKEY;
  chkIndicadorPix.Checked := BeneficiarioWS.IndicadorPix;

  edtPathLog.Text := Boleto.Configuracoes.Arquivos.PathGravarRegistro;
  edtArquivoLog.Text := Boleto.Configuracoes.Arquivos.NomeArquivoLog;
  for I := 0 to Pred(cbxLogNivel.Items.Count) do
    if Integer(cbxLogNivel.Items.Objects[I])
      = Ord(Boleto.Configuracoes.Arquivos.LogNivel) then
    begin
      cbxLogNivel.ItemIndex := I;
      break
    end;
end;

procedure TfrmPrincipal.btnBoletoTesteClick(Sender: TObject);
begin
  GerarDadosTeste;
end;

procedure TfrmPrincipal.btnConsultarBoletoAPIClick(Sender: TObject);
var
  FiltrosAPI: TACBrBoletoWSFiltroConsulta;
  Boleto: TACBrBoleto;
  SLRetorno: TStringList;
  Retorno: TListaACBrBoletoRetornoWS;
  RetornoDetalhe: TACBrBoletoRetornoWS;
  LArquivo: String;
  I: Integer;
Begin
  FiltrosAPI := FACBrBoleto.Configuracoes.WebService.Filtro;
  FiltrosAPI.Clear;
  FACBrBoleto.Configuracoes.WebService.Operacao := tpConsultaDetalhe;
  FACBrBoleto.Enviar;
  if FACBrBoleto.TotalListaRetornoWeb > 0 then
  begin
    I := 0;
    RetornoDetalhe := FACBrBoleto.ListaRetornoWeb[I];
    SLRetorno := TStringList.Create;
    try
      SLRetorno.Add('Cod_Retorno=' + RetornoDetalhe.CodRetorno + sLineBreak +
        'Msg_Retorno=' + RetornoDetalhe.MsgRetorno + sLineBreak + 'Ori_Retorno='
        + RetornoDetalhe.OriRetorno + sLineBreak + 'HTTP_Result=' +
        IntToStr(RetornoDetalhe.HTTPResultCode) + sLineBreak + 'JSON=' +
        RetornoDetalhe.JSON);
      SLRetorno.Add('indicadorContinuidade=' +
        BoolToStr(RetornoDetalhe.indicadorContinuidade));
      SLRetorno.Add('proximoIndice=' + IntToStr(RetornoDetalhe.proximoIndice));
      SLRetorno.Add(' ');
      SLRetorno.Add(' ');
      for I := 0 to Pred(FACBrBoleto.ListadeBoletos.Count) do
      begin
        RetornoDetalhe := FACBrBoleto.ListaRetornoWeb[I];
        SLRetorno.Add('[Boletos Index = ' + FormatFloat('000', I) + ']');
        SLRetorno.Add('nossoNumero = ' + RetornoDetalhe.DadosRet.TituloRet.
          NossoNumero);
        SLRetorno.Add('dataRegistro = ' +
          datetostr(RetornoDetalhe.DadosRet.TituloRet.DataRegistro));
        SLRetorno.Add('dataVencimento = ' +
          datetostr(RetornoDetalhe.DadosRet.TituloRet.Vencimento));
        SLRetorno.Add('valorDocumento = ' +
          CurrToStr(RetornoDetalhe.DadosRet.TituloRet.ValorDocumento));
        SLRetorno.Add('carteiraConvenio = ' +
          RetornoDetalhe.DadosRet.TituloRet.Carteira);
        SLRetorno.Add('variacaoCarteiraConvenio = ' +
          IntToStr(RetornoDetalhe.DadosRet.TituloRet.Modalidade));
        SLRetorno.Add('codigoEstadoTituloCobranca = ' +
          RetornoDetalhe.DadosRet.TituloRet.codigoEstadoTituloCobranca);
        SLRetorno.Add('estadoTituloCobranca = ' +
          RetornoDetalhe.DadosRet.TituloRet.estadoTituloCobranca);
        SLRetorno.Add('contrato = ' +
          RetornoDetalhe.DadosRet.TituloRet.Contrato);
        SLRetorno.Add('dataMovimento = ' +
          datetostr(RetornoDetalhe.DadosRet.TituloRet.dataMovimento));
        SLRetorno.Add('dataCredito = ' +
          datetostr(RetornoDetalhe.DadosRet.TituloRet.dataCredito));
        SLRetorno.Add('valorAtual = ' +
          CurrToStr(RetornoDetalhe.DadosRet.TituloRet.valorAtual));
        SLRetorno.Add('valorPago = ' +
          CurrToStr(RetornoDetalhe.DadosRet.TituloRet.ValorPago));
        SLRetorno.Add('NossoNumeroCorrespondente = ' +
          RetornoDetalhe.DadosRet.TituloRet.NossoNumeroCorrespondente);
        SLRetorno.Add('SeuNumero = ' + RetornoDetalhe.DadosRet.TituloRet.
          SeuNumero);
        SLRetorno.Add('EMV (QrCode Pix) = ' +
          RetornoDetalhe.DadosRet.TituloRet.EMV);

        SLRetorno.Add('  ---  ');
      end;
      LArquivo := PathWithDelim(ExtractFilePath(Application.ExeName)) +
        formatDateTime('yyyy.mm.dd.hh.nn.ss.zzz', now) + '-ConsultaDetalhe.txt';
      SLRetorno.SaveToFile(LArquivo);
    finally
      SLRetorno.Free;
    end;
  end;
  VisualizaReposta(LArquivo);

end;

procedure TfrmPrincipal.btnGerarRemessaClick(Sender: TObject);
var
  NumRemessa: string;
  LRemessa: string;
begin
  FACBrBoleto.DirArqRemessa := edtPathRemessa.Text;
  NumRemessa := '1';
  InputQuery('Num. Remessa', 'Informe o Numero da Remessa :', NumRemessa);
  LRemessa := FACBrBoleto.GerarRemessa(StrToInt64Def(NumRemessa, 0));
  if FileExists(LRemessa) then
    ShowMessage('Remessa gerada em:' + sLineBreak + LRemessa);
end;

procedure TfrmPrincipal.btnImprimeBoletoClick(Sender: TObject);
begin
  if not Assigned(FACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create(MOTOR_NAO_SELECIONADO);

  FACBrBoleto.ACBrBoletoFC.PdfSenha := edtSenhaPDF.Text;
  FACBrBoleto.Imprimir;

end;

procedure TfrmPrincipal.btnIncluiBoletoClick(Sender: TObject);
var
  Titulo: TACBrTitulo;
  VQtdeCarcA, VQtdeCarcB, VQtdeCarcC: Integer;
  VLinha, logo: string;
  I: Integer;
  DadosNota: TACBrDadosNFe;
  ok: Boolean;
  // var_CodigoBarras, var_CodigoBarraBoleto;
begin
  Titulo := FACBrBoleto.CriarTituloNaLista;

  case cmbTipoOcorrencia.ItemIndex of
    0:
      Titulo.OcorrenciaOriginal.Tipo := toRemessaRegistrar;
    1:
      Titulo.OcorrenciaOriginal.Tipo := toRemessaAlterarVencimento;
    2:
      Titulo.OcorrenciaOriginal.Tipo := toRemessaAlterarValorTitulo;
    3:
      Titulo.OcorrenciaOriginal.Tipo := toRemessaAlterarDesconto;
    4:
      Titulo.OcorrenciaOriginal.Tipo := toRemessaAlterarJurosMora;
    5:
      Titulo.OcorrenciaOriginal.Tipo := toRemessaAlterarMulta;
  end;
  ShowMessage('Ocorrência Original: ' + sLineBreak +
    GetEnumName(TypeInfo(TACBrTipoOcorrencia),
    Integer(Titulo.OcorrenciaOriginal.Tipo)));

  { Informacoes Duplicata }
  Titulo.NumeroDocumento := edtNumeroDoc.Text;
  Titulo.ValorDocumento := StrToCurr(edtValorDoc.Text);
  Titulo.DataDocumento := StrToDate(edtDataDoc.Text);
  Titulo.Vencimento := StrToDate(edtVencimento.Text);
  Titulo.SeuNumero := trim(edtSeuNumero.Text);
  Titulo.NossoNumero := trim(edtNossoNro.Text);
  { Acrescimos e descontos }
  Titulo.DataMoraJuros := StrToDateDef(edtDataMora.Text, 0);
  Titulo.ValorMoraJuros := StrToCurrDef(edtMoraJuros.Text, 0);
  Titulo.CodigoMoraJuros := TACBrCodigoJuros(cbxCodMoraJuros.ItemIndex);
  Titulo.NossoNumeroCorrespondente := edtNossoNumeroCorrespondente.Text;

  Titulo.DataDesconto := StrToDateDef(edtDataDesconto.Text, 0);
  Titulo.ValorDesconto := StrToCurrDef(edtValorDesconto.Text, 0);
  Titulo.TipoDesconto := TACBrTipoDesconto(cbxTipoDesconto.ItemIndex);

  {

  Titulo.DataDesconto2 := StrToDateDef(edtDataDesconto.Text, 0) + 2;
  Titulo.ValorDesconto2 := StrToCurrDef(edtValorDesconto.Text, 0) - 2;
  Titulo.TipoDesconto2 := TACBrTipoDesconto(cbxTipoDesconto.ItemIndex);

  Titulo.DataDesconto3 := StrToDateDef(edtDataDesconto.Text, 0) + 4;
  Titulo.ValorDesconto3 := StrToCurrDef(edtValorDesconto.Text, 0) - 3;
  Titulo.TipoDesconto3 := TACBrTipoDesconto(cbxTipoDesconto.ItemIndex);

  }

  Titulo.DataAbatimento := StrToDateDef(edtDataAbatimento.Text, 0);
  Titulo.ValorAbatimento := StrToCurrDef(edtValorAbatimento.Text, 0);

  Titulo.DataMulta := StrToDateDef(edtDataMulta.Text, 0);
  Titulo.PercentualMulta := StrToCurrDef(edtMulta.Text, 0);

  case cbxTipoMulta.ItemIndex of
    0:
      Titulo.MultaValorFixo := False;
    1:
      Titulo.MultaValorFixo := True;
  end;
  Titulo.DataProtesto := StrToDateDef(edtDataProtesto.Text, 0);
  Titulo.DataBaixa := StrToDateDef(edtDataBaixa.Text, 0);
  Titulo.CodigoNegativacao := cnProtestarUteis;
  Titulo.Mensagem.Text := memMensagem.Text;

  { Informacao pagador }
  Titulo.Sacado.NomeSacado := edtNome.Text;
  Titulo.Sacado.CNPJCPF := OnlyNumber(edtCPFCNPJ.Text);
  Titulo.Sacado.Logradouro := edtEndereco.Text;
  Titulo.Sacado.Numero := edtNumero.Text;
  Titulo.Sacado.Bairro := edtBairro.Text;
  Titulo.Sacado.Cidade := edtCidade.Text;
  Titulo.Sacado.UF := edtUF.Text;
  Titulo.Sacado.CEP := OnlyNumber(edtCEP.Text);
  Titulo.Sacado.Email := edtEmail.Text;

  Titulo.EspecieDoc := edtEspecieDoc.Text;
  Titulo.EspecieMod := edtEspecieMod.Text;
  if cbxAceite.ItemIndex = 0 then
    Titulo.Aceite := atSim
  else
    Titulo.Aceite := atNao;
  Titulo.Carteira := edtCarteira.Text;

  Titulo.QtdePagamentoParcial := 1;
  Titulo.TipoPagamento := tpNao_Aceita_Valor_Divergente;
  Titulo.PercentualMinPagamento := 0;
  Titulo.PercentualMaxPagamento := 0;
  Titulo.ValorMinPagamento := 0;
  Titulo.ValorMaxPagamento := 0;

  if chkEMVFicticio.Checked then
    Titulo.QRCode.EMV :=
      '00020101021226870014br.gov.bcb.pix2565qrcodepix-h.bb.com.br/pix/v2/22657e83-ecac-4631-a767-65e16fc56bff5204000053039865802BR5925EMPRORT AMBIENTAL        6008BRASILIA62070503***6304BD3D';

  // FACBrBoleto.AdicionarMensagensPadroes(Titulo,Mensagem);

  if cbxLayOut.ItemIndex = 6 then
  begin
    for I := 0 to 3 do
    begin
      VLinha := '.';

      VQtdeCarcA := Length('Descrição Produto/Serviço ' + IntToStr(I));
      VQtdeCarcB := Length('Valor:');
      VQtdeCarcC := 85 - (VQtdeCarcA + VQtdeCarcB);

      VLinha := PadLeft(VLinha, VQtdeCarcC, '.');

      Titulo.Detalhamento.Add('Descrição Produto/Serviço ' + IntToStr(I) + ' ' +
        VLinha + ' Valor:   ' + PadRight(FormatCurr('R$ ###,##0.00',
        StrToCurr(edtValorDoc.Text) * 0.25), 18, ' '));
    end;
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add
      ('Desconto ........................................................................... Valor: R$ 0,00');

  end;

  // Titulo.ArquivoLogoEmp := logo;  // logo da empresa

  // var_CodigoBarras         := FACBrBoleto.Banco.MontarCodigoBarras(FACBrBoleto.ListadeBoletos[i]);
  // Var_CodigoBarraBoleto    := FACBrBoleto.Banco.MontarLinhaDigitavel(var_CodigoBarras,FACBrBoleto.ListadeBoletos[i]);

end;

procedure TfrmPrincipal.btnLerRetornoClick(Sender: TObject);
var
  Boleto: TACBrBoleto;
  Retorno: TListadeBoletos;
  I: Integer;
  RetText: TStringList;
begin
  Boleto := FACBrBoleto;
  btnPesqRetorno.Click;

  // FACBrBoleto.LerNossoNumeroCompleto := true

  Boleto.DirArqRetorno := ExtractFilePath(edtPathRetorno.Text);
  Boleto.NomeArqRetorno := ExtractFileName(edtPathRetorno.Text);
  // boleto.LerNossoNumeroCompleto

  Boleto.ListadeBoletos.Clear;
  Boleto.LerRetorno();

  Retorno := Boleto.ListadeBoletos;
  RetText := TStringList.Create;
  try
    for I := 0 to Pred(Retorno.Count) do
    begin
      RetText.Add('---------------------------');
      RetText.Add('Nosso Número         : ' + Retorno[I].NossoNumero);
      RetText.Add('Seu Número           : ' + Retorno[I].SeuNumero);
      RetText.Add('Data Vencimento      : ' + datetostr(Retorno[I].Vencimento));
      RetText.Add('ValorDocumento       : ' +
        CurrToStr(Retorno[I].ValorDocumento));
      RetText.Add('DataCredito          : ' +
        datetostr(Retorno[I].dataCredito));
      RetText.Add('DataBaixa            : ' + datetostr(Retorno[I].DataBaixa));
      RetText.Add('DataOcorrencia       : ' +
        datetostr(Retorno[I].DataOcorrencia));
      RetText.Add('ValorPago            : ' + CurrToStr(Retorno[I].ValorPago));
      RetText.Add('ValorRecebido        : ' +
        CurrToStr(Retorno[I].ValorRecebido));
      RetText.Add('ValorMoraJuros       : ' +
        CurrToStr(Retorno[I].ValorMoraJuros));
      RetText.Add('ValorOutrosCreditos  : ' +
        CurrToStr(Retorno[I].ValorOutrosCreditos));
      RetText.Add('ValorOUtrasDespesas  : ' +
        CurrToStr(Retorno[I].ValorOutrasDespesas));
      RetText.Add('EMV (QrCode Pix)     : ' + Retorno[I].QRCode.EMV);
      RetText.Add('CodTipoOcorrencia    : ' +
        GetEnumName(TypeInfo(TACBrTipoOcorrencia),
        Integer(Retorno[I].OcorrenciaOriginal.Tipo)));
      RetText.Add('Desc Tipo Ocorrencia : ' + Retorno[I]
        .OcorrenciaOriginal.descricao);
      RetText.Add('Descriçãoo Comando   : ' + Retorno[I]
        .DescricaoMotivoRejeicaoComando.Text);
      // [...] demais propriedades do titulo a gosto
    end;
    RetText.SaveToFile(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'RetornoProcessado.txt');
    ShowMessage('Retorno processado em: ' +
      PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'RetornoProcessado.txt');
  finally
    RetText.Free;
  end;

end;

procedure TfrmPrincipal.btnLimparListaTituloClick(Sender: TObject);
begin
  FACBrBoleto.ListadeBoletos.Clear;
end;

procedure TfrmPrincipal.btnRegistrarBoletoAPIClick(Sender: TObject);
begin
  EnviaBoleto(tpInclui);
end;

procedure TfrmPrincipal.btnSairMenuClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmPrincipal.btnTotalListaBoletoClick(Sender: TObject);
begin
  ShowMessage('Total de Boletos na lista :' +
    IntToStrZero(FACBrBoleto.ListadeBoletos.Count, 5));
end;

procedure TfrmPrincipal.cbxLayOutChange(Sender: TObject);
begin
  FACBrBoleto.ACBrBoletoFC.LayOut := TACBrBolLayOut(cbxLayOut.ItemIndex);
  (*
    cbxImprimirVersoFatura.Enabled := (cbxLayOut.ItemIndex = 6); // lFaturaDetal
    if cbxLayOut.ItemIndex <> 6 then
    cbxImprimirVersoFatura.Checked := false;
  *)
end;

procedure TfrmPrincipal.cbxMotorRelatorioChange(Sender: TObject);
var
  LSelectedItemIndex: Integer;
  LSelectedObject: TObject;
begin
  LSelectedItemIndex := cbxMotorRelatorio.ItemIndex;

  if LSelectedItemIndex <> -1 then
    LSelectedObject := TObject(cbxMotorRelatorio.Items.Objects
      [LSelectedItemIndex]);

  if Assigned(LSelectedObject) then
  begin
    FACBrBoleto.ACBrBoletoFC := TACBrBoletoFCClass(LSelectedObject);
    FACBrBoleto.ACBrBoletoFC.DirLogo := edtPathLogoMarca.Text;
{$IFDEF GERADOR_FAST_REPORT}
    if FACBrBoleto.ACBrBoletoFC is TACBrBoletoFCFR then
      TACBrBoletoFCFR(FACBrBoleto.ACBrBoletoFC).FastReportFile :=
        edtPathFR3.Text;
{$ENDIF}
  end;

  GravarIniComponente;

end;

procedure TfrmPrincipal.GravarConfiguracaoPix;
Var
  Ini: TIniFile;
begin
  AdicionarLinhaLog('- LerConfiguracao: ' + NomeArquivoConfiguracao);
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

    Ini.WriteInteger('PIX', 'Ambiente', cbxAmbiente.ItemIndex);
    Ini.WriteInteger('PIX', 'TimeOut', seTimeout.Value);

    Ini.WriteInteger('Cobranca', 'Expiracao', seCobrancaExpiracao.Value);

    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', seProxyPorta.Text);
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', EncodeBase64(StrCrypt(edtProxySenha.Text,
      CURL_ACBR)));

    Ini.WriteString('Log', 'Arquivo', edtArqLog.Text);
    Ini.WriteInteger('Log', 'Nivel', cbxNivelLog.ItemIndex);

    Ini.WriteString('C6Bank', 'ChavePIX', edC6BankChavePIX.Text);
    Ini.WriteString('C6Bank', 'ClientID', edC6BankClientID.Text);
    Ini.WriteString('C6Bank', 'ClientSecret', edC6BankClientSecret.Text);
    Ini.WriteString('C6Bank', 'ArqChavePrivada', edC6BankChavePrivada.Text);
    Ini.WriteString('C6Bank', 'ArqCertificado', edC6BankCertificado.Text);
  finally
    Ini.Free;
  end;

  // LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrmPrincipal.AdicionarItemGridFluxo(aEan, aDescricao: String;
  aValor: Double);
begin
  with gdFluxoItens do
  begin
    RowCount := RowCount + 1;
    Cells[0, RowCount - 1] := aEan;
    Cells[1, RowCount - 1] := aDescricao;
    Cells[2, RowCount - 1] := FormatFloatBr(aValor);
  end;

end;

procedure TfrmPrincipal.AdicionarLinhaLog(AMensagem: String);
begin
  if Assigned(mLog) then
    mLog.Lines.Add(AMensagem);
end;

procedure TfrmPrincipal.cbC6BankTipoChaveChange(Sender: TObject);
begin
  cbC6BankTipoChave.ItemIndex :=
    Integer(DetectarTipoChave(edC6BankChavePIX.Text));
  imC6BankErroChavePix.Visible := NaoEstaVazio(edC6BankChavePIX.Text) and
    (cbC6BankTipoChave.ItemIndex = 0);
end;

procedure TfrmPrincipal.cbxAmbienteChange(Sender: TObject);
var
  wProducao: Boolean;
begin
  wProducao := (cbxAmbiente.ItemIndex = 1);
end;

procedure TfrmPrincipal.edtRecebedorCEPChange(Sender: TObject);
begin
    if (Length(edtRecebedorCEP.Text) > 5) then
    begin
      edtRecebedorCEP.Text := FormatarMascaraDinamica
        (OnlyNumber(edtRecebedorCEP.Text), '*****-***');
      edtRecebedorCEP.SelStart := Length(edtRecebedorCEP.Text);
    end;

    imgErrCEP.Visible := (Length(edtRecebedorCEP.Text) < 9);
    sbConsultaCEP.Visible := not imgErrCEP.Visible;
end;

procedure TfrmPrincipal.EncerraVenda(LFlag: Boolean);
begin
  lblTotalVendaPix.Caption := pnFluxoTotalStr.Caption;
  pnpSombra.Visible := LFlag;
  pnlRecebimento.Visible := LFlag;
  zoomin(LFlag)
end;

procedure TfrmPrincipal.EnviaBoleto(ATipoEnvio: TOperacao);
var
  SLRemessa: TStringList;
  I, j: Integer;
  Boleto: TACBrBoleto;
begin
  Boleto := FACBrBoleto;
  Boleto.Configuracoes.WebService.Operacao := ATipoEnvio;
  Boleto.Enviar;
  // <<< retorna como false se o httpresult code for diferente de 200,201,202
  // Verifica Lista com os retornos
  if Boleto.TotalListaRetornoWeb > 0 then
  begin
    SLRemessa := TStringList.Create;
    try
      for I := 0 to Pred(Boleto.TotalListaRetornoWeb) do
      begin
        // Ler todos os campos da classe Retorno
        SLRemessa.Add('Cod_Retorno=' + Boleto.ListaRetornoWeb[I].CodRetorno +
          sLineBreak + 'Msg_Retorno=' + Boleto.ListaRetornoWeb[I].MsgRetorno +
          sLineBreak + 'Ori_Retorno=' + Boleto.ListaRetornoWeb[I].OriRetorno +
          sLineBreak + 'HTTP_Result=' + IntToStr(Boleto.ListaRetornoWeb[I]
          .HTTPResultCode) + sLineBreak + 'JSON=' +
          Boleto.ListaRetornoWeb[I].JSON);
        for j := 0 to Pred(Boleto.ListaRetornoWeb[I].ListaRejeicao.Count) do
        begin
          SLRemessa.Add('[Rejeicao' + IntToStr(j) + ']' + sLineBreak + 'Campo='
            + Boleto.ListaRetornoWeb[I].ListaRejeicao[j].Campo + sLineBreak +
            'Codigo=' + Boleto.ListaRetornoWeb[I].ListaRejeicao[j].Codigo +
            sLineBreak + 'Versao=' + Boleto.ListaRetornoWeb[I].ListaRejeicao[j]
            .Versao + sLineBreak + 'Mensagem=' + Boleto.ListaRetornoWeb[I]
            .ListaRejeicao[j].Mensagem + sLineBreak + 'Ocorrencia=' +
            Boleto.ListaRetornoWeb[I].ListaRejeicao[j].Ocorrencia + sLineBreak +
            'Valor=' + Boleto.ListaRetornoWeb[I].ListaRejeicao[j].Valor +
            sLineBreak);
        end;

        SLRemessa.Add('HEADER' + sLineBreak + 'Versao=' + Boleto.ListaRetornoWeb
          [I].Header.Versao + sLineBreak + 'Autenticacao=' +
          Boleto.ListaRetornoWeb[I].Header.Autenticacao + sLineBreak +
          'Usuario_Servico=' + Boleto.ListaRetornoWeb[I].Header.Usuario_Servico
          + sLineBreak + 'Usuario=' + Boleto.ListaRetornoWeb[I].Header.Usuario +
          sLineBreak + 'Operacao=' + TipoOperacaoToStr(Boleto.ListaRetornoWeb[I]
          .Header.Operacao) + sLineBreak + 'Indice=' +
          IntToStr(Boleto.ListaRetornoWeb[I].Header.Indice) + sLineBreak +
          'Sistema_Origem=' + Boleto.ListaRetornoWeb[I].Header.Sistema_Origem +
          sLineBreak + 'Agencia=' + IntToStr(Boleto.ListaRetornoWeb[I]
          .Header.Agencia) + sLineBreak + 'ID_Origem=' + Boleto.ListaRetornoWeb
          [I].Header.Id_Origem + sLineBreak + 'Data_Hora=' +
          formatDateTime('dd/mm/yyyy hh:nn:ss',
          Boleto.ListaRetornoWeb[I].Header.Data_Hora) + sLineBreak +
          'ID_Processo=' + Boleto.ListaRetornoWeb[I].Header.Id_Processo +
          sLineBreak + 'DADOS' + sLineBreak + 'Excessao=' +
          Boleto.ListaRetornoWeb[I].DadosRet.Excecao + sLineBreak +
          'CONTROLE_NEGOCIAL' + sLineBreak + 'Origem_Retorno=' +
          Boleto.ListaRetornoWeb[I].DadosRet.ControleNegocial.OriRetorno +
          sLineBreak + 'NSU=' + Boleto.ListaRetornoWeb[I]
          .DadosRet.ControleNegocial.NSU + sLineBreak + 'Cod_Retorno=' +
          Boleto.ListaRetornoWeb[I].DadosRet.ControleNegocial.CodRetorno +
          sLineBreak + 'Msg_Retorno=' + Boleto.ListaRetornoWeb[I]
          .DadosRet.ControleNegocial.Retorno + sLineBreak + 'COMPROVANTE' +
          sLineBreak + 'Data=' + formatDateTime('dd/mm/yyyy',
          Boleto.ListaRetornoWeb[I].DadosRet.Comprovante.Data) + sLineBreak +
          'Hora=' + Boleto.ListaRetornoWeb[I].DadosRet.Comprovante.Hora +
          sLineBreak + 'ID_BOLETO' + sLineBreak + 'Codigo_Barras=' +
          Boleto.ListaRetornoWeb[I].DadosRet.IDBoleto.CodBarras + sLineBreak +
          'Linha_Digitavel=' + Boleto.ListaRetornoWeb[I]
          .DadosRet.IDBoleto.LinhaDig + sLineBreak + 'Nosso_Numero=' +
          Boleto.ListaRetornoWeb[I].DadosRet.IDBoleto.NossoNum + sLineBreak +
          'URL=' + Boleto.ListaRetornoWeb[I].DadosRet.IDBoleto.URL + sLineBreak
          + 'CONSULTA_BOLETO' + sLineBreak + 'Numero_Documento=' +
          Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.NumeroDocumento +
          sLineBreak + 'Data_Vencimento=' + formatDateTime('dd/mm/yyyy',
          Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.Vencimento) + sLineBreak
          + 'Valor=' + CurrToStr(Boleto.ListaRetornoWeb[I]
          .DadosRet.TituloRet.ValorDocumento) + sLineBreak);
        if NaoEstaVazio(Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.CodBarras)
        then
        begin
          SLRemessa.Add('TITULO_RETORNO' + sLineBreak + 'vencimento_titulo=' +
            formatDateTime('dd/mm/yyyy',
            Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.Vencimento) +
            sLineBreak + 'data_processamento=' + formatDateTime('dd/mm/yyyy',
            Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.DataProcessamento) +
            sLineBreak + 'data_emissao=' + formatDateTime('dd/mm/yyyy',
            Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.DataDocumento) +
            sLineBreak + 'tipo_carteira_titulo=' + Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.Carteira + sLineBreak + 'nosso_numero=' +
            Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.NossoNumero +
            sLineBreak + 'NossoNumeroCorrespondente=' + Boleto.ListaRetornoWeb
            [I].DadosRet.TituloRet.NossoNumeroCorrespondente + sLineBreak +
            'seu_numero=' + Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.SeuNumero + sLineBreak + 'especie=' +
            Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.EspecieDoc + sLineBreak
            + 'codigo_barras=' + Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.CodBarras + sLineBreak +
            'numero_linha_digitavel=' + Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.LinhaDig + sLineBreak + 'local_pagamento=' +
            Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.Mensagem.Text +
            sLineBreak + 'uso_banco=' + Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.UsoBanco + sLineBreak + 'valor_titulo=' +
            CurrToStr(Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.ValorDocumento) + sLineBreak + 'valor_desconto='
            + CurrToStr(Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.ValorDesconto) + sLineBreak +
            'valor_outra_deducao=' + CurrToStr(Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.ValorDespesaCobranca) + sLineBreak +
            'valor_juro_multa=' + CurrToStr(Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.ValorMoraJuros) + sLineBreak +
            'valor_outro_acrescimo=' + CurrToStr(Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.ValorOutrosCreditos) + sLineBreak +
            'valor_total_cobrado=' + CurrToStr(Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.ValorPago) + sLineBreak + 'EMV (QrCode) =' +
            Boleto.ListaRetornoWeb[I].DadosRet.TituloRet.EMV + sLineBreak +
            'texto_informacao_cliente_beneficiario=' + Boleto.ListaRetornoWeb[I]
            .DadosRet.TituloRet.Informativo.Text);
        end;
      end;
      SLRemessa.SaveToFile(PathWithDelim(ExtractFilePath(Application.ExeName)) +
        'RetornoRegistro.txt');

    finally
      SLRemessa.Free;
    end;

    VisualizaReposta(PathWithDelim(ExtractFilePath(Application.ExeName)) +
      'RetornoRegistro.txt')

    // ShowMessage('Retorno Envio gerado em: '+ PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoRegistro.txt' );

  end;
end;

procedure TfrmPrincipal.zoomin(LFlag: Boolean);
var
  LTempo, I: Integer;
begin
  LTempo := 100;
  if LFlag then
  begin
    if pnpSombra.Top = 600 then
    begin
      for I := 600 DownTo 131 do
        pnpSombra.Top := I;
      Sleep(100);
      pnlRecebimento.Top := 114;
    end;
  end
  else
  begin
    if pnpSombra.Top = 131 then
    begin
      for I := 131 To 600 do
        pnpSombra.Top := I;
      Sleep(100);
      pnlRecebimento.Top := 600;
    end;
  end;
end;

procedure TfrmPrincipal.EstornarPagamento;
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
      DevolucaoSolicitada.Valor := fFluxoDados.Total;
      DevolucaoSolicitada.natureza := ndORIGINAL;
      DevolucaoSolicitada.descricao := 'Devolucao da Venda';

      if SolicitarDevolucaoPix(fFluxoDados.E2E, StringReplace(fFluxoDados.E2E,
        'E', 'D', [rfReplaceAll])) then
      begin
        Sleep(1000);
        ConsultarDevolucao;

        if (fFluxoDados.StatusDevolucao = stdDEVOLVIDO) then
          ShowMessage('Pagamento Estornado com Sucesso')
        else if (fFluxoDados.StatusDevolucao = stdEM_PROCESSAMENTO) then
          tmConsultarDevolucao.Enabled := True;
        // Estorno pendente? ...Consultar até alterar Status
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

procedure TfrmPrincipal.InicializarComponentesPixDefault;
var
  I, l: Integer;
  j: TACBrPixCDAmbiente;
  k: TACBrPIXTipoChave;
  m: TACBrPIXStatusCobranca;
  n: TACBrPIXDescontoModalidade;
  o: TACBrPIXValoresModalidade;
  p: TACBrPIXJurosModalidade;
  q: TACBrBBAPIVersao;
begin
  cbxPSPAtual.Items.Clear;
  for I := 0 to pgConfPixPSP.PageCount - 1 do
    cbxPSPAtual.Items.Add(pgConfPixPSP.Pages[I].Caption);

  cbxRecebedorUF.Items.Clear;
  for I := Low(DFeUF) to High(DFeUF) do
    cbxRecebedorUF.Items.Add(DFeUF[I]);

  cbxAmbiente.Items.Clear;
  for j := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
    cbxAmbiente.Items.Add(GetEnumName(TypeInfo(TACBrPixCDAmbiente),
      Integer(j)));

  cbxSolicitarDevolucaoPix_Natureza.Items.Clear;
  for l := 0 to Integer(High(TACBrPIXNaturezaDevolucao)) do
    cbxSolicitarDevolucaoPix_Natureza.Items.Add
      (GetEnumName(TypeInfo(TACBrPIXNaturezaDevolucao), l));
  cbxSolicitarDevolucaoPix_Natureza.ItemIndex := 1;

  cbxConsultarCobrancas_Status.Items.Clear;
  for m := Low(TACBrPIXStatusCobranca) to High(TACBrPIXStatusCobranca) do
    cbxConsultarCobrancas_Status.Items.Add
      (GetEnumName(TypeInfo(TACBrPIXStatusCobranca), Integer(m)));
  cbxConsultarCobrancas_Status.ItemIndex := 0;

  cbCobVConsultarStatus.Items.Clear;
  for m := Low(TACBrPIXStatusCobranca) to High(TACBrPIXStatusCobranca) do
    cbCobVConsultarStatus.Items.Add
      (GetEnumName(TypeInfo(TACBrPIXStatusCobranca), Integer(m)));
  cbCobVConsultarStatus.ItemIndex := 0;

  cbCobVDescModalidade.Items.Clear;
  for n := Low(TACBrPIXDescontoModalidade)
    to High(TACBrPIXDescontoModalidade) do
    cbCobVDescModalidade.Items.Add(IntToStr(Ord(n)) + ' - ' +
      DescontoModalidadeToString(n));
  cbCobVDescModalidade.ItemIndex := 0;

  cbCobVMultaModalidade.Items.Clear;
  for o := Low(TACBrPIXValoresModalidade) to High(TACBrPIXValoresModalidade) do
    cbCobVMultaModalidade.Items.Add(IntToStr(Ord(o)) + ' - ' +
      ValoresModalidadeToString(o));
  cbCobVMultaModalidade.ItemIndex := 0;

  cbCobVJurosModalidade.Items.Clear;
  for p := Low(TACBrPIXJurosModalidade) to High(TACBrPIXJurosModalidade) do
    cbCobVJurosModalidade.Items.Add(IntToStr(Ord(p)) + ' - ' +
      JurosModalidadeToString(p));
  cbCobVJurosModalidade.ItemIndex := 0;

  dtConsultarPixRecebidosInicio.DateTime := StartOfTheDay(Today);
  dtConsultarPixRecebidosFim.DateTime := EndOfTheDay(Today);

  dtConsultarCobrancas_Inicio.DateTime := StartOfTheDay(Today);
  dtConsultarCobrancas_Fim.DateTime := EndOfTheDay(Today);

  edCobVVencimento.DateTime := IncDay(now, 7);
end;

Function TfrmPrincipal.FormatarMascaraDinamica(const AValue: String;
  const Mascara: String): String;
var
  LenMas, LenDoc: Integer;
  I, j: Integer;
  c: Char;
  wValue: String;
begin
  Result := '';
  wValue := trim(AValue);
  LenMas := Length(Mascara);
  LenDoc := Length(wValue);

  I := 1;
  j := 1;
  while (I <= LenMas) and (j <= LenDoc) do
  begin
    c := Mascara[I];
    if c = '*' then
    begin
      c := wValue[j];
      Inc(j);
    end;

    Result := Result + c;
    Inc(I);
  end;
end;

procedure TfrmPrincipal.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');

  MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TfrmPrincipal.LerConfiguracaoPix;
var
  Ini: TIniFile;
begin
  AdicionarLinhaLog('- LerConfiguracao: ' + NomeArquivoConfiguracao);
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    edtRecebedorNome.Text := Ini.ReadString('Recebedor', 'Nome', '');
    edtRecebedorCEP.Text := Ini.ReadString('Recebedor', 'CEP', '');
    edtRecebedorCidade.Text := Ini.ReadString('Recebedor', 'Cidade', '');
    cbxRecebedorUF.ItemIndex := cbxRecebedorUF.Items.IndexOf
      (Ini.ReadString('Recebedor', 'UF', ''));

    cbAutenticacaoManual.Checked := Ini.ReadBool('Autenticar', 'Manual', False);

    cbxAmbiente.ItemIndex := Ini.ReadInteger('PIX', 'Ambiente', 0);
    seTimeout.Value := Ini.ReadInteger('PIX', 'TimeOut', ChttpTimeOutDef);

    seCobrancaExpiracao.Value := Ini.ReadInteger('Cobranca', 'Expiracao',
      seCobrancaExpiracao.Value);

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    seProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text :=
      StrCrypt(DecodeBase64(Ini.ReadString('Proxy', 'Pass', '')), CURL_ACBR);

    edtArqLog.Text := Ini.ReadString('Log', 'Arquivo', '');
    cbxNivelLog.ItemIndex := Ini.ReadInteger('Log', 'Nivel', 1);

    edC6BankChavePIX.Text := Ini.ReadString('C6Bank', 'ChavePIX', '');
    edC6BankClientID.Text := Ini.ReadString('C6Bank', 'ClientID', '');
    edC6BankClientSecret.Text := Ini.ReadString('C6Bank', 'ClientSecret', '');
    edC6BankChavePrivada.Text := Ini.ReadString('C6Bank', 'ArqChavePrivada',
      edC6BankChavePrivada.Text);
    edC6BankCertificado.Text := Ini.ReadString('C6Bank', 'ArqCertificado',
      edC6BankCertificado.Text);
  finally
    Ini.Free;
  end;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracaoPSPC6Bank;
end;

procedure TfrmPrincipal.LigarAlertasdeErrosDeConfiguracaoPSPC6Bank;
begin
  edC6BankChavePIXChange(Nil);
  ValidarCertificadoPSPC6Bank;
  ValidarChavePSPC6Bank;
end;

procedure TfrmPrincipal.AplicarConfiguracao;
begin
  AdicionarLinhaLog('- AplicarConfiguracao');
  ConfigurarACBrPIXCD;
  ConfigurarACBrPSPs;
end;

procedure TfrmPrincipal.ConfigurarACBrPSPs;
begin
  AdicionarLinhaLog('  - ConfigurarACBrPSPs');

  ACBrPSPC6Bank1.ChavePIX := edC6BankChavePIX.Text;
  ACBrPSPC6Bank1.ClientID := edC6BankClientID.Text;
  ACBrPSPC6Bank1.ClientSecret := edC6BankClientSecret.Text;
  ACBrPSPC6Bank1.ArquivoChavePrivada := edC6BankChavePrivada.Text;
  ACBrPSPC6Bank1.ArquivoCertificado := edC6BankCertificado.Text;
end;

procedure TfrmPrincipal.ConfigurarACBrPIXCD;
begin
  AdicionarLinhaLog('  - ConfigurarACBrPIXCD');
  ACBrPixCD1.Recebedor.nome := edtRecebedorNome.Text;
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

  ACBrPixCD1.PSP := ACBrPSPC6Bank1;

  if cbAutenticacaoManual.Checked then
  begin
    ACBrPixCD1.PSP.OnAntesAutenticar := DoAntesAutenticar;
    ACBrPixCD1.PSP.OnDepoisAutenticar := DoDepoisAutenticar;
  end;
end;

procedure TfrmPrincipal.DoAntesAutenticar(var aToken: String;
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

procedure TfrmPrincipal.DoDepoisAutenticar(const aToken: String;
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

procedure TfrmPrincipal.edC6BankChavePIXChange(Sender: TObject);
begin
  cbC6BankTipoChave.ItemIndex :=
    Integer(DetectarTipoChave(edC6BankChavePIX.Text));
  imC6BankErroChavePix.Visible := NaoEstaVazio(edC6BankChavePIX.Text) and
    (cbC6BankTipoChave.ItemIndex = 0);
end;

procedure TfrmPrincipal.ValidarCertificadoPSPC6Bank;
var
  a, E: String;
begin
  a := AdicionarPathAplicacao(edC6BankCertificado.Text);
  E := 'OK';
  if (a = '') then
    E := ACBrStr('Arquivo não especificado')
  else if (not FileExists(a)) then
    E := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        E := Ex.Message;
    end;
  end;

  lbC6BankErroCertificado.Caption := E;
  imC6BankErroCertificado.Visible := (E <> 'OK');
end;

procedure TfrmPrincipal.ValidarChavePSPC6Bank;
var
  a, E: String;
begin
  a := AdicionarPathAplicacao(edC6BankChavePrivada.Text);
  E := 'OK';
  if (a = '') then
    E := ACBrStr('Arquivo não especificado')
  else if (not FileExists(a)) then
    E := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        E := Ex.Message;
    end;
  end;

  lbC6BankErroChavePrivada.Caption := E;
  imC6BankErroChavePrivada.Visible := (E <> 'OK');
end;

function TfrmPrincipal.AdicionarPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := trim(AFileName);
  if (s = '') then
    Result := s
  else if (ExtractFilePath(AFileName) <> '') then
    Result := s
  else
    Result := ApplicationPath + s;
end;

procedure TfrmPrincipal.btCancelarCobrancaClick(Sender: TObject);
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
      MostrarCobrancaEmLinhas('  Cobrança', CobGerada,
        mmCancelarCobranca.Lines);
    end
    else
      mmCancelarCobranca.Lines.Text := FormatarJSON(Problema.AsJSON);
  end;
end;

procedure TfrmPrincipal.btCobVCancelarClick(Sender: TObject);
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

procedure TfrmPrincipal.btCobVConsultarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mmCobVConsultar.Lines.Clear;
  if ACBrPixCD1.PSP.epCobV.ConsultarCobranca(edCobVConsultarTxID.Text,
    edCobVConsultarRevisao.Value) then
    mmCobVConsultar.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCobV.CobVCompleta.AsJSON)
  else
    mmCobVConsultar.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCobV.Problema.AsJSON);
end;

procedure TfrmPrincipal.btCobVConsultarListaClick(Sender: TObject);
var
  ok: Boolean;
  I: Integer;
begin
  VerificarConfiguracao;
  mmCobVConsultarLista.Lines.Clear;

  ok := ACBrPixCD1.PSP.epCobV.ConsultarCobrancas
    (StartOfTheDay(edCobVConsultarInicio.DateTime),
    EndOfTheDay(edCobVConsultarFim.DateTime),
    OnlyNumber(edCobVConsultarCPFCNPJ.Text), cbCobVConsultarLocation.Checked,
    TACBrPIXStatusCobranca(cbCobVConsultarStatus.ItemIndex),
    edCobVConsultarPagina.Value, edCobVConsultarItensPag.Value);

  if ok then
  begin
    mmCobVConsultarLista.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCobV.CobsVConsultadas.AsJSON);
    mmCobVConsultarLista.Lines.Add('');
    mmCobVConsultarLista.Lines.Add('Encontrado: ' +
      IntToStr(ACBrPixCD1.PSP.epCobV.CobsVConsultadas.cobs.Count) +
      ', Cobranças');
    for I := 0 to ACBrPixCD1.PSP.epCobV.CobsVConsultadas.cobs.Count - 1 do
      mmCobVConsultarLista.Lines.Add('');
  end
  else
    mmCobVConsultarLista.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCobV.Problema.AsJSON);

end;

procedure TfrmPrincipal.btConsultarCobrancaImediataClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarCobrancaImediata.Lines.Clear;
  if ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata
    (edtConsultarCobrancaImediata_TxId.Text,
    seConsultarCobrancaImediata_Revisao.Value) then
  begin
    mConsultarCobrancaImediata.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCob.CobCompleta.AsJSON);
    MostrarCobrancaEmLinhas('  Cobranca', ACBrPixCD1.PSP.epCob.CobCompleta,
      mConsultarCobrancaImediata.Lines);
  end
  else
    mConsultarCobrancaImediata.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON);
end;

procedure TfrmPrincipal.btConsultarCobrancasClick(Sender: TObject);
var
  ok: Boolean;
  I: Integer;
begin
  VerificarConfiguracao;
  mConsultarCobrancas.Lines.Clear;

  ok := ACBrPixCD1.PSP.epCob.ConsultarCobrancas
    (StartOfTheDay(dtConsultarCobrancas_Inicio.DateTime),
    EndOfTheDay(dtConsultarCobrancas_Fim.DateTime),
    OnlyNumber(edtConsultarCobrancas_CPFCNPJ.Text),
    chConsultarCobrancas_ComLocation.Checked,
    TACBrPIXStatusCobranca(cbxConsultarCobrancas_Status.ItemIndex),
    seConsultarCobrancas_Pagina.Value, seConsultarCobrancas_ItensPagina.Value);

  if ok then
  begin
    mConsultarCobrancas.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCob.CobsConsultadas.AsJSON);
    mConsultarCobrancas.Lines.Add('');
    mConsultarCobrancas.Lines.Add('Encontrado: ' +
      IntToStr(ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs.Count) +
      ', Cobranças');
    for I := 0 to ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs.Count - 1 do
    begin
      mConsultarCobrancas.Lines.Add('');
      MostrarCobrancaEmLinhas('  Cob[' + IntToStr(I) + ']',
        ACBrPixCD1.PSP.epCob.CobsConsultadas.cobs[I],
        mConsultarCobrancas.Lines);
    end;
  end
  else
    mConsultarCobrancas.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON);

end;

procedure TfrmPrincipal.btConsultarDevolucaoPixClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarDevolucaoPix.Lines.Clear;
  if ACBrPixCD1.PSP.epPix.ConsultarDevolucaoPix
    (edtConsultarDevolucaoPix_e2eid.Text, edtConsultarDevolucaoPix_id.Text) then
  begin
    mConsultarDevolucaoPix.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epPix.Devolucao.AsJSON);
    MostrarDevolucaoEmLinhas('  Devolucao', ACBrPixCD1.PSP.epPix.Devolucao,
      mConsultarDevolucaoPix.Lines);
  end
  else
    mConsultarDevolucaoPix.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TfrmPrincipal.btConsultarPixClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mConsultarPix.Lines.Clear;
  if ACBrPixCD1.PSP.epPix.ConsultarPix(edtConsultarPixE2eid.Text) then
  begin
    mConsultarPix.Lines.Text := FormatarJSON(ACBrPixCD1.PSP.epPix.pix.AsJSON);
    MostrarPixEmLinhas('  Pix', ACBrPixCD1.PSP.epPix.pix, mConsultarPix.Lines);
  end
  else
    mConsultarPix.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);

end;

procedure TfrmPrincipal.btConsultarPixRecebidosClick(Sender: TObject);
var
  ok: Boolean;
  I: Integer;
begin
  VerificarConfiguracao;
  mConsultarPixRecebidos.Lines.Clear;

  ok := ACBrPixCD1.PSP.epPix.ConsultarPixRecebidos
    (StartOfTheDay(dtConsultarPixRecebidosInicio.DateTime),
    EndOfTheDay(dtConsultarPixRecebidosFim.DateTime),
    edtConsultarPixRecebidosTxId.Text,
    OnlyNumber(edtConsultarPixRecebidosCPFCNPJ.Text),
    seConsultarPixRecebidosPagina.Value,
    seConsultarPixRecebidosItensPagina.Value);

  if ok then
  begin
    mConsultarPixRecebidos.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epPix.PixConsultados.AsJSON);
    mConsultarPixRecebidos.Lines.Add('');
    mConsultarPixRecebidos.Lines.Add('Encontrado: ' +
      IntToStr(ACBrPixCD1.PSP.epPix.PixConsultados.pix.Count) +
      ', documentos PIX');
    for I := 0 to ACBrPixCD1.PSP.epPix.PixConsultados.pix.Count - 1 do
    begin
      mConsultarPixRecebidos.Lines.Add('');
      MostrarPixEmLinhas('  Pix[' + IntToStr(I) + ']',
        ACBrPixCD1.PSP.epPix.PixConsultados.pix[I],
        mConsultarPixRecebidos.Lines);
    end;
  end
  else
    mConsultarPixRecebidos.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);

end;

procedure TfrmPrincipal.btCriarCobrancaImediataClick(Sender: TObject);
var
  s, QRCode: String;
  ok: Boolean;
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

    s := trim(edtCriarCobrancaImediata_NomeDevedor.Text);
    if (s <> '') then
    begin
      devedor.nome := s;
      s := OnlyNumber(edtCriarCobrancaImediata_CPF_CNPJ.Text);
      if (s = '') then
        raise Exception.Create
          ('Caso o Nome do Devedor seja Informado, e necessário informar CPF ou CNPJ')
      else if (Length(s) > 11) then
        devedor.cnpj := s
      else
        devedor.cpf := s;
    end;

    Valor.original := StrToFloatDef(feCriarCobrancaImediatax_Valor.Text, 0);
    Valor.modalidadeAlteracao :=
      chCriarCobrancaImediata_PermiterAlterarValor.Checked;
  end;

  if (trim(edtCriarCobrancaImediata_TxId.Text) <> '') then
    ok := ACBrPixCD1.PSP.epCob.CriarCobrancaImediata
      (edtCriarCobrancaImediata_TxId.Text)
  else
    ok := ACBrPixCD1.PSP.epCob.CriarCobrancaImediata;

  if ok then
  begin
    mCriarCobrancaImediata.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCob.CobGerada.AsJSON);
    MostrarCobrancaEmLinhas('  CobGerada', ACBrPixCD1.PSP.epCob.CobGerada,
      mCriarCobrancaImediata.Lines);
    QRCode := trim(ACBrPixCD1.PSP.epCob.CobGerada.pixCopiaECola);
    if (QRCode = '') then
      QRCode := ACBrPixCD1.GerarQRCodeDinamico
        (ACBrPixCD1.PSP.epCob.CobGerada.location);
    PintarQRCode(QRCode, imgQRCriarCobrancaImediata.Picture.Bitmap, qrUTF8BOM);
    mCriarCobrancaImediata.Lines.Add('');
    mCriarCobrancaImediata.Lines.Add('- pixCopiaECola -');
    mCriarCobrancaImediata.Lines.Add(QRCode);
  end
  else
    mCriarCobrancaImediata.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCob.Problema.AsJSON);

end;

procedure TfrmPrincipal.btCriarCobVClick(Sender: TObject);
var
  s, wQrCode: String;
  ok: Boolean;
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
      s := trim(edCobVCompradorNome.Text);
      if NaoEstaVazio(s) then
      begin
        nome := s;
        s := OnlyNumber(edCobVCompradorDoc.Text);
        if EstaVazio(s) then
          raise Exception.Create
            ('Caso o Nome do Devedor seja Informado, é necessário informar CPF ou CNPJ')
        else if (Length(s) > 11) then
          cnpj := s
        else
          cpf := s;
      end;

    end;

    with Valor do
    begin
      original := StrToFloatDef(edCobVValor.Text, 0);

      multa.Modalidade := TACBrPIXValoresModalidade
        (cbCobVMultaModalidade.ItemIndex);
      multa.valorPerc := StrToFloatDef(edCobVMultaValor.Text, 0);

      juros.Modalidade := TACBrPIXJurosModalidade
        (cbCobVJurosModalidade.ItemIndex);
      juros.valorPerc := StrToFloatDef(edCobVJurosValor.Text, 0);

      desconto.Modalidade := TACBrPIXDescontoModalidade
        (cbCobVDescModalidade.ItemIndex);

      if desconto.Modalidade in [pdmValorFixo, pdmPercentual] then
      begin
        with desconto.descontosDataFixa.New do
        begin
          Data := edCobVVencimento.DateTime;
          valorPerc := StrToFloatDef(edCobVDescValor.Text, 0);
        end;
      end
      else
        desconto.valorPerc := StrToFloatDef(edCobVDescValor.Text, 0);
    end;
  end;

  ok := ACBrPixCD1.PSP.epCobV.CriarCobranca(CriarTxId);
  imCobVQRCode.Visible := ok;
  edCobVCopiaECola.Visible := ok;
  btCobVCopiaECola.Visible := ok;
  lbCobVCopiaECola.Visible := ok;

  if ok then
  begin
    wQrCode := trim(ACBrPixCD1.PSP.epCobV.CobVGerada.pixCopiaECola);
    if EstaVazio(wQrCode) then
      wQrCode := ACBrPixCD1.GerarQRCodeDinamico
        (ACBrPixCD1.PSP.epCobV.CobVGerada.loc.location);

    PintarQRCode(wQrCode, imCobVQRCode.Picture.Bitmap, qrUTF8BOM);
    edCobVCopiaECola.Text := wQrCode;
  end
  else
    mCriarCobrancaImediata.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epCobV.Problema.AsJSON);
end;

procedure TfrmPrincipal.btQREAnalisarClick(Sender: TObject);
var
  qrd: TACBrPIXQRCodeDinamico;
begin
  qrd := TACBrPIXQRCodeDinamico.Create;
  try
    qrd.IgnoreErrors := True;
    qrd.AsString := trim(mQRD.Lines.Text);
    AnalisarBRCode(qrd);
  finally
    qrd.Free;
  end;
end;

procedure TfrmPrincipal.btQREColarClick(Sender: TObject);
begin
  mQRD.CopyToClipboard;
end;

procedure TfrmPrincipal.btQREGerarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  PintarQRCodeEstatico;
end;

procedure TfrmPrincipal.btSolicitarDevolucaoPixClick(Sender: TObject);
begin
  VerificarConfiguracao;
  mSolicitarDevolucaoPix.Lines.Clear;

  with ACBrPixCD1.PSP.epPix.DevolucaoSolicitada do
  begin
    Clear;
    Valor := StrToFloatDef(feSolicitarDevolucaoPix_Valor.Text, 0);
    natureza := TACBrPIXNaturezaDevolucao
      (cbxSolicitarDevolucaoPix_Natureza.ItemIndex);
    descricao := edtSolicitarDevolucaoPix_Descricao.Text;
  end;

  if ACBrPixCD1.PSP.epPix.SolicitarDevolucaoPix
    (edtSolicitarDevolucaoPix_e2eid.Text, edtSolicitarDevolucaoPix_id.Text) then
  begin
    mSolicitarDevolucaoPix.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epPix.Devolucao.AsJSON);
    MostrarDevolucaoEmLinhas('  Devolucao', ACBrPixCD1.PSP.epPix.Devolucao,
      mSolicitarDevolucaoPix.Lines);
  end
  else
    mSolicitarDevolucaoPix.Lines.Text :=
      FormatarJSON(ACBrPixCD1.PSP.epPix.Problema.AsJSON);
end;

procedure TfrmPrincipal.VerificarConfiguracaoPix;
begin
  VerificarConfiguracaoPIXCD;
end;

procedure TfrmPrincipal.VerificarConfiguracaoPIXCD;
begin
  if imgErrNome.Visible or imgErrCEP.Visible then
  begin
    MessageDlg('Favor revise configuração das propriedades Nome/CEP', mtWarning,
      [mbOK], 0);
    Abort;
  end;
end;

procedure TfrmPrincipal.VisualizaReposta(aArquivo: string);
begin
  if FileExists(aArquivo) then
  begin
    try
      frmResposta := TfrmResposta.Create(self);
      frmResposta.Memo1.Lines.Clear;
      frmResposta.Memo1.Lines.LoadFromFile(aArquivo);
      frmResposta.ShowModal;
    finally
      FreeAndNil(frmResposta);
    end;

  end
  else
    ShowMessage('Arquivo nao foi encontrado:' + sLineBreak + aArquivo);

end;

procedure TfrmPrincipal.VerificarConfiguracao;
begin
  VerificarConfiguracaoPIXCD;
end;

function TfrmPrincipal.FormatarJSON(const AJSON: String): String;
{$IFDEF FPC}
var
  jpar: TJSONParser;
  j: TJsonObject;
{$ENDIF}
begin
  Result := AJSON;
{$IFDEF FPC}
  try
    j := TJsonObject.Create();
    try
      Result := j.Decode(Result);
    finally
      j.Free;
    end;
    jpar := TJSONParser.Create(Result, [joUTF8]);
    try
      Result := jpar.Parse.FormatJSON([], 2);
    finally
      jpar.Free;
    end;
  except
    Result := AJSON;
  end;
{$ENDIF}
end;

procedure TfrmPrincipal.AnalisarBRCode(aBRCode: TACBrBRCode);
begin
  AdicionarLinhaLog('');
  if (aBRCode is TACBrPIXQRCodeEstatico) then
    with TACBrPIXQRCodeEstatico(aBRCode) do
    begin
      AdicionarLinhaLog('----- Analise do QRCode Estático -----');
      AdicionarLinhaLog('ChavePix: ' + PixKey);
      AdicionarLinhaLog('TipoChavePix: ' +
        GetEnumName(TypeInfo(TACBrPIXTipoChave), Integer(PixKeyType)));
      AdicionarLinhaLog('infoAdicional: ' + AdditionalInfo);
      AdicionarLinhaLog('pss: ' + IntToStr(pss));
    end
  else if (aBRCode is TACBrPIXQRCodeDinamico) then
  begin
    AdicionarLinhaLog('----- Analise do QRCode Dinâmico -----');
    AdicionarLinhaLog('URL: ' + TACBrPIXQRCodeDinamico(aBRCode).URL);
  end;

  AdicionarLinhaLog('NomeRecebedor: ' + aBRCode.MerchantName);
  AdicionarLinhaLog('CidadeRecebedor: ' + aBRCode.MerchantCity);
  AdicionarLinhaLog('CEPRecebedor: ' + aBRCode.PostalCode);
  AdicionarLinhaLog('Valor: ' + FormatFloat('0.00', aBRCode.TransactionAmount));
  AdicionarLinhaLog('TxId: ' + aBRCode.TxID);
end;

procedure TfrmPrincipal.btnTestePIXClick(Sender: TObject);
begin
  MostraTela(itTestePix);
end;

procedure TfrmPrincipal.btnTestePixVoltarClick(Sender: TObject);
begin
  MostraTela(itCfgPix);
end;

procedure TfrmPrincipal.btQRDGerarClick(Sender: TObject);
begin
  VerificarConfiguracao;
  PintarQRCodeDinamico;
end;

procedure TfrmPrincipal.PintarQRCodeEstatico;
begin
  mQRE.Lines.Text := ACBrPixCD1.GerarQRCodeEstatico
    (StrToFloatDef(fleQREValor.Text, 0), edtQREInfoAdicional.Text,
    edtQRETxId.Text);
  PintarQRCode(mQRE.Lines.Text, imgQRE.Picture.Bitmap, qrUTF8BOM);
end;

procedure TfrmPrincipal.PintarQRCodeDinamico;
begin
  mQRD.Lines.Text := ACBrPixCD1.GerarQRCodeDinamico(edQRDLocation.Text,
    edQRDTxID.Text);
  PintarQRCode(mQRD.Lines.Text, imgQRD.Picture.Bitmap, qrUTF8BOM);
end;

procedure TfrmPrincipal.InicializarBitmaps;
begin
  ImageList1.GetBitmap(6, imgErrNome.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrCEP.Picture.Bitmap);
  ImageList1.GetBitmap(6, imgErrPSP.Picture.Bitmap);

  ImageList1.GetBitmap(31, btFluxoItemIncluir.Glyph);
  ImageList1.GetBitmap(32, btFluxoItemExcluir.Glyph);
  ImageList1.GetBitmap(33, btFluxoPagar.Glyph);
  ImageList1.GetBitmap(17, btFluxoCancelarCobranca.Glyph);
  ImageList1.GetBitmap(12, btFluxoEstornarPagto.Glyph);
  ImageList1.GetBitmap(23, btFluxoNovaVenda.Glyph);
  // ImageList1.GetBitmap(11, btFluxoTentarNovamente.Glyph);
  // ImageList1.GetBitmap(17, btFluxoCancelarConsulta.Glyph);
  // ImageList1.GetBitmap(16, btFluxoFecharVenda.Glyph);
  ImageList1.GetBitmap(13, btFluxoCopiaECola.Glyph);
  ImageList1.GetBitmap(13, btCobVCopiaECola.Glyph);

  ImageList1.GetBitmap(4, btConsultarPix.Glyph);
  ImageList1.GetBitmap(4, btConsultarPixRecebidos.Glyph);
  ImageList1.GetBitmap(4, btSolicitarDevolucaoPix.Glyph);
  ImageList1.GetBitmap(4, btConsultarDevolucaoPix.Glyph);
  ImageList1.GetBitmap(4, btCriarCobrancaImediata.Glyph);
  ImageList1.GetBitmap(4, btConsultarCobrancaImediata.Glyph);
  ImageList1.GetBitmap(4, btConsultarCobrancas.Glyph);
  ImageList1.GetBitmap(4, btCriarCobV.Glyph);
  ImageList1.GetBitmap(4, btCobVConsultar.Glyph);
  ImageList1.GetBitmap(4, btCobVConsultarLista.Glyph);
  ImageList1.GetBitmap(4, btCobVCancelar.Glyph);
  ImageList1.GetBitmap(4, btBBSimulaPagamento_Executar.Glyph);
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
  ImageList1.GetBitmap(13, btQREColar.Glyph);
  ImageList1.GetBitmap(13, btQRDColar.Glyph);

  ImageList1.GetBitmap(8, sbConsultaCEP.Glyph);
  ImageList1.GetBitmap(9, sbArqLog.Glyph);
  ImageList1.GetBitmap(7, sbVerSenhaProxy.Glyph);

  ImageList1.GetBitmap(18, btLogLimpar.Glyph);
  // ImageList1.GetBitmap(10, btnCfgSalvaPix.Glyph);
  ImageList1.GetBitmap(11, btnLerParametrosPIX.Glyph);

  ImageList1.GetBitmap(6, imC6BankErroChavePix.Picture.Bitmap);
  ImageList1.GetBitmap(6, imC6BankErroCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(6, imC6BankErroChavePrivada.Picture.Bitmap);
  ImageList1.GetBitmap(9, btC6BankAcharChavePrivada.Glyph);
  ImageList1.GetBitmap(9, btC6BankAcharCertificado.Glyph);
end;

procedure TfrmPrincipal.ReiniciarFluxo;
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

procedure TfrmPrincipal.LimparInterfaceFluxoItem;
begin
  edFluxoItemEAN.Clear;
  edFluxoItemValor.Clear;
  edFluxoItemDescricao.Clear;
  InicializarGridFluxo;
end;

procedure TfrmPrincipal.AtualizarTotal;
var
  I: Integer;
begin
  fFluxoDados.Total := 0;
  for I := 1 to Pred(gdFluxoItens.RowCount) do
    fFluxoDados.Total := FluxoDados.Total +
      StrToCurrDef(StringReplace(gdFluxoItens.Cells[2, I], '.', '', []), 0);
  pnFluxoTotalStr.Caption := FormatFloatBr(FluxoDados.Total, 'R$ ,0.00');
end;

procedure TfrmPrincipal.AtualizarStatus(aStatus: TACBrPIXStatusCobranca;
  aStatusDevolucao: TACBrPIXStatusDevolucao);

  procedure AtualizarPanelPrincipal(aTexto: String; aCor: TColor;
    LpnpReceber: Boolean);
  begin
    pnFluxoStatus.Color := aCor;
    pnFluxoStatus.Caption := aTexto;
    EncerraVenda(LpnpReceber);
  end;

begin
  if FluxoDados.EmErro then
  begin
    AtualizarPanelPrincipal('ERRO AO CONSULTAR', clRed, False);
    AvaliarInterfaceFluxo;
    Exit;
  end;

  fFluxoDados.StatusCobranca := aStatus;
  fFluxoDados.StatusDevolucao := aStatusDevolucao;
  AvaliarInterfaceFluxo;

  case FluxoDados.StatusDevolucao of
    stdDEVOLVIDO:
      AtualizarPanelPrincipal('PAGAMENTO DEVOLVIDO', $009A9A9A, False);
    stdEM_PROCESSAMENTO:
      AtualizarPanelPrincipal('DEVOLUÇAO PENDENTE', $00523C30, True);
    stdNAO_REALIZADO:
      AtualizarPanelPrincipal('DEVOLUÇÃO NÃO REALIZADA', $00523C30, False);
  else
    case FluxoDados.StatusCobranca of
      stcATIVA:
        AtualizarPanelPrincipal('AGUARDANDO PAGAMENTO', $001ADAE3, True);
      stcCONCLUIDA:
        AtualizarPanelPrincipal('PAGAMENTO FINALIZADO', $0009E31F, False);
      stcREMOVIDA_PELO_USUARIO_RECEBEDOR:
        AtualizarPanelPrincipal('PAGAMENTO CANCELADO', $000600EA, False);
      stcREMOVIDA_PELO_PSP:
        AtualizarPanelPrincipal('CANCELADO PELO PSP', $000600EA, False);
    else
      AtualizarPanelPrincipal('VENDENDO', clMenuHighlight, False);
    end;

  end;
end;

procedure TfrmPrincipal.InicializarGridFluxo;
begin
  with gdFluxoItens do
  begin
    RowCount := 1;
    ColWidths[0] := 175;
    ColWidths[1] := 300;
    ColWidths[2] := 120;

    Cells[0, 0] := 'EAN';
    Cells[1, 0] := 'Descrição';
    Cells[2, 0] := 'Valor';
    AdicionarItemGridFluxo('0123456789012', 'Batata Doce', 3.69);
  end;
end;

function TfrmPrincipal.GetInfoOpenSSL: String;
begin
  with ACBrOpenSSLUtils1 do
    Result := 'Info OpenSSL: ' + sLineBreak + OpenSSLExt.OpenSSLFullVersion +
      sLineBreak + OpenSSLExt.SSLUtilFile + sLineBreak + OpenSSLExt.SSLLibFile +
      sLineBreak + OpenSSLExt.OpenSSLVersion(0) + sLineBreak +
      IntToStr(OpenSSLExt.OpenSSLVersionNum) + sLineBreak;
end;

procedure TfrmPrincipal.AvaliarInterfaceFluxo;
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

    // pnFluxoQRCode.Visible := wAguardandoPagto;
    // pnFluxoCopiaECola.Visible := wAguardandoPagto;

    btFluxoCancelarCobranca.Visible := wAguardandoPagto;
    btFluxoEstornarPagto.Visible := (StatusCobranca = stcCONCLUIDA) and
      wSemEstorno;
    btFluxoNovaVenda.Visible := (StatusCobranca <> stcNENHUM) or
      (StatusDevolucao = stdDEVOLVIDO);
  end;

  if gbFluxoItens.Enabled then
    AvaliarInterfaceFluxoItem;
end;

procedure TfrmPrincipal.AvaliarInterfaceFluxoItem;
var
  wRemovido: Boolean;
begin
  with FluxoDados do
  begin
    wRemovido := (StatusCobranca in [stcREMOVIDA_PELO_PSP,
      stcREMOVIDA_PELO_USUARIO_RECEBEDOR]);
    btFluxoItemIncluir.Enabled := (StatusCobranca = stcNENHUM) or wRemovido;
    btFluxoItemExcluir.Enabled := ((StatusCobranca = stcNENHUM) or wRemovido)
      and (gdFluxoItens.RowCount > 1) and (gdFluxoItens.Row > 0);
  end;
end;

procedure TfrmPrincipal.btC6BankAcharCertificadoClick(Sender: TObject);
begin
  SelecionaPath(edC6BankCertificado);
end;

procedure TfrmPrincipal.btC6BankAcharChavePrivadaClick(Sender: TObject);
begin
  SelecionaPath(edC6BankChavePrivada);
end;

procedure TfrmPrincipal.btFluxoCancelarCobrancaClick(Sender: TObject);
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

    if (MessageDlg('Deseja realmente Cancelar a Cobrança?', mtConfirmation,
      mbOKCancel, 0) = mrNo) then
    begin
      tmConsultarPagto.Enabled := True;
      Exit;
    end;

    ACBrPixCD1.PSP.epCob.CobRevisada.status :=
      stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
    if ACBrPixCD1.PSP.epCob.RevisarCobrancaImediata(FluxoDados.TxID) then
    begin
      Sleep(1000);
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

procedure TfrmPrincipal.btFluxoEstornarPagtoClick(Sender: TObject);
begin
  if (MessageDlg('Deseja realmente estornar o pagamento?', mtConfirmation,
    mbOKCancel, 0) = mrNo) then
    Exit;

  EstornarPagamento;
end;

procedure TfrmPrincipal.btFluxoNovaVendaClick(Sender: TObject);
begin
  ReiniciarFluxo;
end;

procedure TfrmPrincipal.btFluxoPagarClick(Sender: TObject);
var
  wNome, wDoc: String;
  SL: TStringList;
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

      wNome := trim(edFluxoClienteNome.Text);
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

      Valor.original := fFluxoDados.Total;
    end;

    if ACBrPixCD1.PSP.epCob.CriarCobrancaImediata then
    begin
      fFluxoDados.TxID := ACBrPixCD1.PSP.epCob.CobGerada.TxID;
      fFluxoDados.QRCode := trim(ACBrPixCD1.PSP.epCob.CobGerada.pixCopiaECola);

      if (fFluxoDados.QRCode = EmptyStr) then
        fFluxoDados.QRCode := ACBrPixCD1.GerarQRCodeDinamico
          (ACBrPixCD1.PSP.epCob.CobGerada.location);

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

procedure TfrmPrincipal.btnAlterarBoletoClick(Sender: TObject);
begin
  EnviaBoleto(tpAltera);
end;

procedure TfrmPrincipal.btnBoletoBaixaClick(Sender: TObject);
begin
  EnviaBoleto(tpBaixa);
end;

procedure TfrmPrincipal.btnEncerraVendaClick(Sender: TObject);
begin
  if not pnlRecebimento.Visible then
     EncerraVenda(True);
end;

procedure TfrmPrincipal.btnFecharTelaPagamentoClick(Sender: TObject);
begin
  EncerraVenda(false);
end;

procedure TfrmPrincipal.Button1Click(Sender: TObject);
var
  lInicio, lFim: TDateTime;
  I: Integer;
begin
  lInicio := now;
  for I := 0 to 99 do
  begin
    edtSeuNumero.Text := (StrToIntDef(edtSeuNumero.Text, 0) + 1).ToString;
    btnIncluiBoleto.Click;
  end;

  btnRegistrarBoletoAPI.Click;
  lFim := now;

  ShowMessage(lInicio.ToString + '    ' + lFim.ToString);
end;

procedure TfrmPrincipal.HabilitarInterface(aLiberada: Boolean);
begin
  pnFluxoBackground.Enabled := aLiberada;
end;

procedure TfrmPrincipal.ConsultarCobranca;
begin
  if EstaVazio(fFluxoDados.TxID) then
  begin
    ShowMessage('Nenhum TxID para ser consultado');
    Exit;
  end;

  HabilitarInterface(False);
  try
    if (not ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata(fFluxoDados.TxID))
    then
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

procedure TfrmPrincipal.ConsultarDevolucao;
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

    if (ACBrPixCD1.PSP.epPix.pix.devolucoes.Count > 0) then
      AtualizarStatus(stcNENHUM, ACBrPixCD1.PSP.epPix.pix.devolucoes[0].status);
  finally
    HabilitarInterface(True);
  end;
end;

procedure TfrmPrincipal.tmConsultarDevolucaoTimer(Sender: TObject);
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

procedure TfrmPrincipal.tmConsultarPagtoTimer(Sender: TObject);
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
    if (FluxoDados.StatusCobranca = stcATIVA) and (not fFluxoDados.EmErro) and
      (fFluxoDados.QtdConsultas <= CMaxConsultas) then
      tmConsultarPagto.Enabled := True;
  end;
end;

end.
