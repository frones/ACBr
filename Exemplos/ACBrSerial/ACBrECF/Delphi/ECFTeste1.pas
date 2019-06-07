{$I ACBr.inc}
unit ECFTeste1;

interface

uses ACBrECF, ACBrRFD, ACBrBase, ACBrDevice, ACBrECFClass, ACBrConsts, FileCtrl,
{$IFDEF Delphi6_UP} StrUtils, DateUtils, Types, {$ELSE} ACBrD5, {$ENDIF}
  SysUtils, Classes, Graphics, printers,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  Menus, Spin, jpeg, OleCtrls, SHDocVw
{$IFDEF Delphi7}, XPMan{$ENDIF}, ACBrAAC, ACBrECFVirtual,
  ACBrECFVirtualPrinter, ACBrECFVirtualNaoFiscal, ACBrECFVirtualBuffer,
  DB, ACBrPosPrinter, ACBrECFVirtualSAT, ACBrSATExtratoReportClass,
  ACBrSATExtratoFortesFr, ACBrSATExtratoClass, ACBrSATExtratoESCPOS,
  ACBrSAT, ACBrSATclass, ACBrDFe, ACBrNFe, ACBrECFVirtualNFCe, ACBrNFeDANFeESCPOS,
  ACBrNFeDANFEClass, ACBrDANFCeFortesFr, Mask, DBCtrls, Grids,
  DBGrids, pcnNFe, pcnCFe, pcnConversao, ACBrDFeReport, ACBrDFeDANFeReport;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    sbtnCaminhoCert: TSpeedButton;
    Principal1: TMenuItem;
    Sair1: TMenuItem;
    cbxModeloSAT: TComboBox;
    cbxAmbiente: TComboBox;
    Sobre1: TMenuItem;
    N1: TMenuItem;
    sbtnGetCert: TSpeedButton;
    Label71: TLabel;
    Label72: TLabel;
    Ativcar1: TMenuItem;
    Desativar1: TMenuItem;
    N2: TMenuItem;
    Testar1: TMenuItem;
    Variaveis1: TMenuItem;
    DataHora1: TMenuItem;
    NumECF1: TMenuItem;
    NSrie1: TMenuItem;
    NVerso1: TMenuItem;
    N3: TMenuItem;
    PoucoPapel1: TMenuItem;
    Relatrios1: TMenuItem;
    LeituraX1: TMenuItem;
    ReduoZ1: TMenuItem;
    N4: TMenuItem;
    AliquotasICMS1: TMenuItem;
    FormasdePagamento1: TMenuItem;
    AbrirCupom1: TMenuItem;
    N5: TMenuItem;
    VenderItem1: TMenuItem;
    CancelarItemVendido1: TMenuItem;
    DBGrid1: TDBGrid;
    N6: TMenuItem;
    Sub1: TMenuItem;
    EfetuarPagamento1: TMenuItem;
    FecharCupom1: TMenuItem;
    CancelaCupom1: TMenuItem;
    N8: TMenuItem;
    Variveis1: TMenuItem;
    ckVisualizar: TCheckBox;
    NUltimoCupom1: TMenuItem;
    SubTotal1: TMenuItem;
    TotalPago1: TMenuItem;
    N9: TMenuItem;
    RelatorioGerencial1: TMenuItem;
    N10: TMenuItem;
    FechaRelatrio1: TMenuItem;
    cbUsarEscPos: TRadioButton;
    cbUsarFortes: TRadioButton;
    rgFormaEmissao: TRadioGroup;
    rgTipoAmb: TRadioGroup;
    ckSalvar: TCheckBox;
    rgTipoDanfe: TRadioGroup;
    Dispositivos1: TMenuItem;
    Gaveta1: TMenuItem;
    GavetaAberta1: TMenuItem;
    AbreGaveta1: TMenuItem;
    Cheque1: TMenuItem;
    edLogSAT: TEdit;
    edNomeDLL: TEdit;
    edtCaminho: TEdit;
    edtCodigoAtivacao: TEdit;
    edtCodUF: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCEP: TEdit;
    edtEmitCidade: TEdit;
    edtEmitCNPJ: TEdit;
    edtEmitCNPJNFe: TEdit;
    edtEmitCodCidade: TEdit;
    edtEmitComp: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitIE: TEdit;
    edtEmitIENFe: TEdit;
    edtEmitIM: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitRazao: TEdit;
    edtEmitUF: TEdit;
    edtLogoMarca: TEdit;
    cbUF: TComboBox;
    edtNumSerie: TEdit;
    edtPathLogs: TEdit;
    edtPorta: TEdit;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    edtSenha: TEdit;
    edtSwHAssinatura: TEdit;
    edtSwHCNPJ: TEdit;
    CancelaImpressoCheque1: TMenuItem;
    ImprimeCheque1: TMenuItem;
    cbxFormatXML: TCheckBox;
    cbxIndRatISSQN: TComboBox;
    cbxModelo: TComboBox;
    cbxPorta: TComboBox;
    cbxRegTribISSQN: TComboBox;
    cbxRegTributario: TComboBox;
    cbxSalvarCFe: TCheckBox;
    cbxUTF8: TCheckBox;
    chIgnorarTagsFormatacao: TCheckBox;
    chArredondaPorQtd: TCheckBox;
    ChequePronto1: TMenuItem;
    Utilitrios1: TMenuItem;
    HorarioVerao1: TMenuItem;
    ImpactoAgulhas1: TMenuItem;
    N7: TMenuItem;
    EnviaComando1: TMenuItem;
    TestaPodeAbrirCupom1: TMenuItem;
    ACBrECF1: TACBrECF;
    CarregaComprovantesNAOFiscais1: TMenuItem;
    HorarioVerao2: TMenuItem;
    Arredonda1: TMenuItem;
    MudaArredondamento1: TMenuItem;
    NumLoja1: TMenuItem;
    NumCRO1: TMenuItem;
    N11: TMenuItem;
    TestedeVelocidade1: TMenuItem;
    N12: TMenuItem;
    LeituradeMemoriaFiscal1: TMenuItem;
    CapturaporNReduaoZ1: TMenuItem;
    CapturaporPeriodo1: TMenuItem;
    ImprimeporNReduaoZ1: TMenuItem;
    ImprimeporPeriodo1: TMenuItem;
    ProgramaAliquota1: TMenuItem;
    N13: TMenuItem;
    ProgramaComprovanteNAOFiscal1: TMenuItem;
    ProgramaFormadePagamento1: TMenuItem;
    CorrigeEstadodeErro1: TMenuItem;
    N14: TMenuItem;
    CarregaUnidadesdeMedida1: TMenuItem;
    ProgramaUnidadeMedida1: TMenuItem;
    N15: TMenuItem;
    AbreRelatorioGerencial1: TMenuItem;
    ImprimeLinhaRelatorio1: TMenuItem;
    ListaRelatorioGerencial1: TMenuItem;
    edAACNomeArq: TEdit;
    lImpressora: TLabel;
    edAACLog: TEdit;
    edAAC_ECF_CRO: TDBEdit;
    edAAC_ECF_NumSerie: TDBEdit;
    edAAC_PAF_Aplicativo: TEdit;
    edAAC_PAF_MD5: TEdit;
    edAAC_PAF_Versao: TEdit;
    edAAC_SH_CNPJ: TEdit;
    edAAC_SH_IE: TEdit;
    edAAC_SH_IM: TEdit;
    edAAC_ECF_GT: TDBEdit;
    edAAC_SH_RazaoSocial: TEdit;
    N17: TMenuItem;
    PularLinhas1: TMenuItem;
    N18: TMenuItem;
    LerTodasasVariveis1: TMenuItem;
    MFD1: TMenuItem;
    Termica1: TMenuItem;
    pgPrincipal: TPageControl;
    tsCMD: TTabSheet;
    tsCupom: TTabSheet;
    mResp: TMemo;
    Panel1: TPanel;
    mBobina: TMemo;
    Panel2: TPanel;
    cbMemoHTML: TCheckBox;
    bBobinaParams: TButton;
    bBobinaLimpar: TButton;
    Equipamento1: TMenuItem;
    N19: TMenuItem;
    Flags1: TMenuItem;
    MapaResumo1: TMenuItem;
    DadosReducaoZ1: TMenuItem;
    N20: TMenuItem;
    CNPJIE1: TMenuItem;
    NumCRZ1: TMenuItem;
    NumCOOInicial1: TMenuItem;
    VendaBruta1: TMenuItem;
    GrandeTotal1: TMenuItem;
    TotalCancelamentos1: TMenuItem;
    TotalDescontos1: TMenuItem;
    TotalAcrescimos1: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    Aliquotas1: TMenuItem;
    LerTotaisAliquotas1: TMenuItem;
    FormasdePagamento2: TMenuItem;
    ComprovantesNaoFiscais1: TMenuItem;
    LerTotaisFormadePagamento1: TMenuItem;
    LerTotaisComprovanetNaoFiscal1: TMenuItem;
    UltimoItemVendido1: TMenuItem;
    N23: TMenuItem;
    LeituraMFD1: TMenuItem;
    PorCOO1: TMenuItem;
    PorPeriodo1: TMenuItem;
    Estado1: TMenuItem;
    Cupom1: TMenuItem;
    CupomVinculado1: TMenuItem;
    CupomVinculadoCompleto2: TMenuItem;
    N27: TMenuItem;
    AbreCupomVinculado1: TMenuItem;
    ImprimeLinhaCupomVinculado2: TMenuItem;
    NoFiscal1: TMenuItem;
    NoFiscalCompleto1: TMenuItem;
    N16: TMenuItem;
    AbreNoFiscal1: TMenuItem;
    RegistraItemNaoFiscal1: TMenuItem;
    SubTotalizaNaoFiscal1: TMenuItem;
    EfetuaPagamentoNaoFiscal1: TMenuItem;
    FechaNoFiscal1: TMenuItem;
    N24: TMenuItem;
    CancelaNoFiscal1: TMenuItem;
    NumCCF1: TMenuItem;
    NumCOO1: TMenuItem;
    N25: TMenuItem;
    IdentificaConsumidor1: TMenuItem;
    ACBrRFD1: TACBrRFD;
    tsRFD: TTabSheet;
    tsECF: TTabSheet;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    chTentar: TCheckBox;
    chBloqueia: TCheckBox;
    chExibeMsg: TCheckBox;
    chGavetaSinalInvertido: TCheckBox;
    Label6: TLabel;
    mMsg: TMemo;
    Label9: TLabel;
    edLog: TEdit;
    SbArqLog: TSpeedButton;
    Label2: TLabel;
    mEnviado: TMemo;
    Label17: TLabel;
    pBotoes: TPanel;
    bAtivar: TBitBtn;
    pgRFD: TPageControl;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Label8: TLabel;
    Label13: TLabel;
    edSH_RazaoSocial: TEdit;
    edSH_COO: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edSH_CNPJ: TEdit;
    edSH_IE: TEdit;
    edSH_IM: TEdit;
    Label14: TLabel;
    edSH_Aplicativo: TEdit;
    Label15: TLabel;
    edSH_NumeroAP: TEdit;
    Label16: TLabel;
    edSH_VersaoAP: TEdit;
    Label18: TLabel;
    edSH_Linha1: TEdit;
    Label19: TLabel;
    edSH_Linha2: TEdit;
    Panel3: TPanel;
    chRFD: TCheckBox;
    Label3: TLabel;
    edDirRFD: TEdit;
    sbDirRFD: TSpeedButton;
    Panel4: TPanel;
    bRFDLer: TButton;
    mRFDParam: TMemo;
    bRFDSalvar: TButton;
    seTimeOut: TSpinEdit;
    seIntervaloAposComando: TSpinEdit;
    IE1: TMenuItem;
    Image1: TImage;
    wbBobina: TWebBrowser;
    N26: TMenuItem;
    DataMovimento1: TMenuItem;
    DadosUltimaReduoZ1: TMenuItem;
    btSerial: TBitBtn;
    chDescricaoGrande: TCheckBox;
    CortaPapel1: TMenuItem;
    N28: TMenuItem;
    estedeVinculado1: TMenuItem;
    Label20: TLabel;
    edOperador: TEdit;
    Sangria1: TMenuItem;
    Suprimento1: TMenuItem;
    N29: TMenuItem;
    edMsgTrabalhando: TEdit;
    mAACParams: TMemo;
    Label21: TLabel;
    ProgramaRelatrioGerencial1: TMenuItem;
    RelatriosGerenciais1: TMenuItem;
    CarregaRelatriosGerenciais1: TMenuItem;
    LegendaInmetroproximoItem1: TMenuItem;
    CancelaItemVendidoParcial1: TMenuItem;
    N30: TMenuItem;
    CancelaDescontoAcrescimoItem1: TMenuItem;
    CancelaDescontoAcrescimoSubTotal1: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    ConsultaRegistradorECF1: TMenuItem;
    N33: TMenuItem;
    EstornaMeiodePagamento1: TMenuItem;
    DeCodificaTexto1: TMenuItem;
    N34: TMenuItem;
    AchaAliquotaporIndice1: TMenuItem;
    AchaAliquotaporValor1: TMenuItem;
    N35: TMenuItem;
    AcharMeioPagamentoporIndice1: TMenuItem;
    AcharMeiodePagametoporDescrio1: TMenuItem;
    N36: TMenuItem;
    AchaCNFporIndice1: TMenuItem;
    AchaCNFporDescrio1: TMenuItem;
    N37: TMenuItem;
    AchaRGporIndice1: TMenuItem;
    AchaRGporDescrio1: TMenuItem;
    N38: TMenuItem;
    PorCOO2: TMenuItem;
    PorDatadeMovimento1: TMenuItem;
    LeituraSerialMFD1: TMenuItem;
    PorCOO3: TMenuItem;
    PorPeriodo2: TMenuItem;
    UsuarioAual1: TMenuItem;
    N39: TMenuItem;
    ArquivoMFDDLL1: TMenuItem;
    PorCOO4: TMenuItem;
    PorPeriodo3: TMenuItem;
    N40: TMenuItem;
    NumGNF1: TMenuItem;
    tsDadosRedZ: TTabSheet;
    btnDadosRZ: TButton;
    mRZ: TMemo;
    Label37: TLabel;
    N41: TMenuItem;
    LerTroco1: TMenuItem;
    Label22: TLabel;
    speLinBuf: TSpinEdit;
    NumSerieMFD: TMenuItem;
    ParametroDescontoISSQN1: TMenuItem;
    N42: TMenuItem;
    mModeloStr: TMenuItem;
    tbsMenuFiscal: TTabSheet;
    grpMenuFiscalOpcoes: TGroupBox;
    btnMenuFiscalLX: TButton;
    btnMenuFiscalLMFC: TButton;
    btnMenuFiscalLMFS: TButton;
    btnMenuFiscalMFDEspelho: TButton;
    btnMenuFiscalMFDArq: TButton;
    btnMenuFiscalRelMeiosPagto: TButton;
    btnMenuFiscalRelDAVEmitidos: TButton;
    btnMenuFiscalRelIdentPAFECF: TButton;
    dlgDialogoSalvar: TSaveDialog;
    pgcMenuFiscalTipo: TPageControl;
    tbsMenuFiscalTipoData: TTabSheet;
    Label24: TLabel;
    Label25: TLabel;
    edtDtInicial: TDateTimePicker;
    edtDtFinal: TDateTimePicker;
    tbsMenuFiscalTipoCOO: TTabSheet;
    Label26: TLabel;
    Label30: TLabel;
    edtCOOInicial: TSpinEdit;
    edtCOOFinal: TSpinEdit;
    chkMenuFiscalCotepe1704: TCheckBox;
    chkMenuFiscalGerarArquivo: TCheckBox;
    Label31: TLabel;
    seLargura: TSpinEdit;
    seMargemDireita: TSpinEdit;
    seMargemEsquerda: TSpinEdit;
    seMargemFundo: TSpinEdit;
    seMargemTopo: TSpinEdit;
    seNumeroCaixa: TSpinEdit;
    cbPreview: TCheckBox;
    sePagCod: TSpinEdit;
    sePaginaCodigo: TSpinEdit;
    RelatorioGerencialcomformatacao1: TMenuItem;
    btnMenuFiscalConfigPAFECF: TButton;
    N43: TMenuItem;
    DAV1: TMenuItem;
    DAVOS1: TMenuItem;
    btnMenuFiscalNotaPaulista: TButton;
    chArredondamentoItemMFD: TCheckBox;
    tsTagsImpressao: TTabSheet;
    speBarrasLargura: TSpinEdit;
    BitBtn6: TBitBtn;
    MemoTesteTags: TMemo;
    chBarrasImprimeTexto: TCheckBox;
    speBarrasAltura: TSpinEdit;
    Label28: TLabel;
    Label27: TLabel;
    Label23: TLabel;
    chbCupomMania: TCheckBox;
    DataHoraltimaReduoZ1: TMenuItem;
    btnArqMFNovo: TButton;
    btnArqMFDNovo: TButton;
    btnDadosUltimaRZ: TButton;
    ProgramaIdentificaoPafECF1: TMenuItem;
    ACBrECFVirtualNaoFiscal1: TACBrECFVirtualNaoFiscal;
    VendaFrentica1: TMenuItem;
    Label46: TLabel;

    tsArqAuxCript: TTabSheet;
    bAACGravarArquivo: TButton;
    Label33: TLabel;
    SbAACNomeArq: TSpeedButton;
    Label41: TLabel;
    SbAACArqLog: TSpeedButton;
    bAACAbrirArquivo: TButton;
    ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrECFVirtualNFCe1: TACBrECFVirtualNFCe;
    ACBrNFe1: TACBrNFe;
    ACBrSAT1: TACBrSAT;
    ACBrSATExtratoESCPOS1: TACBrSATExtratoESCPOS;
    ACBrSATExtratoFortes1: TACBrSATExtratoFortes;
    ACBrECFVirtualSAT1: TACBrECFVirtualSAT;
    ACBrAAC1: TACBrAAC;
    OpenDialog1: TOpenDialog;
    ACBrPosPrinter1: TACBrPosPrinter;
    tsSAT: TTabSheet;
    tsNFCe: TTabSheet;
    sfeVersaoEnt: TEdit;
    Button2: TButton;
    N44: TMenuItem;
    SalvarParmetros1: TMenuItem;
    lerParmetros1: TMenuItem;
    chControlePorta: TCheckBox;
    cbxECFVirtual: TComboBox;
    Label69: TLabel;
    seBandWidth: TSpinEdit;
    Label32: TLabel;
    Button3: TButton;
    edInfo: TEdit;
    chAACUsar: TCheckBox;
    chAACFlush: TCheckBox;
    chProcessMessages: TCheckBox;
    SAT1: TMenuItem;
    mConsultarStatusSAT: TMenuItem;
    mConsultarSAT: TMenuItem;
    mConsultarSessaoSAT: TMenuItem;
    N45: TMenuItem;
    CarregarCFe1: TMenuItem;
    N46: TMenuItem;
    ImprimirExtratoVenda1: TMenuItem;
    ImprimirExtratoResumido1: TMenuItem;
    ImprimirExtratoCancelamento1: TMenuItem;
    estedeArredondamento1: TMenuItem;
    estedeRateio1: TMenuItem;
    LeituraXSerial1: TMenuItem;
    otalizadoresnofiscais1: TMenuItem;
    CarregaTotalizadoresNaoTributados1: TMenuItem;
    LerTotaisTotalizadoresNaoTributados1: TMenuItem;
    N47: TMenuItem;
    AchaTotalizadorNaoTributadoIndice1: TMenuItem;
    estarLeituradeTotais1: TMenuItem;
    NmeroReduesZrestantes1: TMenuItem;
    NumGNFC1: TMenuItem;
    NumGRG1: TMenuItem;
    NumCDC1: TMenuItem;
    NumCCDC1: TMenuItem;
    NumCFD1: TMenuItem;
    NumNCN1: TMenuItem;
    otalTroco1: TMenuItem;
    otalICMS1: TMenuItem;
    otalCancelamentos1: TMenuItem;
    otalDescontos1: TMenuItem;
    otalAcrescimos1: TMenuItem;
    N48: TMenuItem;
    otalSubstituicaoTributaria1: TMenuItem;
    otalNaoTributado1: TMenuItem;
    otalIseno1: TMenuItem;
    otaisISSQN1: TMenuItem;
    otalCancelamentos2: TMenuItem;
    otalDescontos2: TMenuItem;
    otalAcrescimos2: TMenuItem;
    N49: TMenuItem;
    otalSubstituicaoTributaria2: TMenuItem;
    otalNaoTributado2: TMenuItem;
    otalIseno2: TMenuItem;
    ValorTotal1: TMenuItem;
    otalCancelamentos3: TMenuItem;
    otalDescontos3: TMenuItem;
    otalAcrscimos1: TMenuItem;
    MFAdicional1: TMenuItem;
    ipoltimoDocumento1: TMenuItem;
    N50: TMenuItem;
    IdentificaConsumidorRodap1: TMenuItem;
    SubModeloECF1: TMenuItem;
    N51: TMenuItem;
    IM1: TMenuItem;
    PAF1: TMenuItem;
    Cliche1: TMenuItem;
    DataHoraSwBasico1: TMenuItem;
    N52: TMenuItem;
    Decimais1: TMenuItem;
    Colunas1: TMenuItem;
    N53: TMenuItem;
    EstornaCCD1: TMenuItem;
    LeituraCMC71: TMenuItem;
    N54: TMenuItem;
    IdentificaOperador1: TMenuItem;
    AbreBilhetePassagem1: TMenuItem;
    procedure cbxModeloChange(Sender: TObject);
    procedure Sair1Click(Sender: TObject);
    procedure bAtivarClick(Sender: TObject);
    procedure cbxPortaChange(Sender: TObject);
    procedure Ativar1Click(Sender: TObject);
    procedure Desativar1Click(Sender: TObject);
    procedure chTentarClick(Sender: TObject);
    procedure chBloqueiaClick(Sender: TObject);
    procedure chExibeMsgClick(Sender: TObject);
    procedure mMsgChange(Sender: TObject);
    procedure Testar1Click(Sender: TObject);
    procedure ACBrECF1MsgAguarde(Mensagem: String);
    procedure DataHora1Click(Sender: TObject);
    procedure NumECF1Click(Sender: TObject);
    procedure NSrie1Click(Sender: TObject);
    procedure NVerso1Click(Sender: TObject);
    procedure NumUltimoCupom1Click(Sender: TObject);
    procedure PoucoPapel1Click(Sender: TObject);
    procedure LeituraX1Click(Sender: TObject);
    procedure ReduoZ1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Sobre1Click(Sender: TObject);
    procedure AliquotasICMS1Click(Sender: TObject);
    procedure FormasdePagamento1Click(Sender: TObject);
    procedure AbreGaveta1Click(Sender: TObject);
    procedure GavetaAberta1Click(Sender: TObject);
    procedure ChequePronto1Click(Sender: TObject);
    procedure CancelaImpressoCheque1Click(Sender: TObject);
    procedure HorarioVerao1Click(Sender: TObject);
    procedure ImpactoAgulhas1Click(Sender: TObject);
    procedure TestaPodeAbrirCupom1Click(Sender: TObject);
    procedure NUltimoCupom1Click(Sender: TObject);
    procedure SubTotal1Click(Sender: TObject);
    procedure TotalPago1Click(Sender: TObject);
    procedure AbrirCupom1Click(Sender: TObject);
    procedure CancelaCupom1Click(Sender: TObject);
    procedure VenderItem1Click(Sender: TObject);
    procedure CancelarItemVendido1Click(Sender: TObject);
    procedure Sub1Click(Sender: TObject);
    procedure EfetuarPagamento1Click(Sender: TObject);
    procedure FecharCupom1Click(Sender: TObject);
    procedure EnviaComando1Click(Sender: TObject);
    procedure ACBrECF1AguardandoRespostaChange(Sender: TObject);
    procedure CarregaComprovantesNAOFiscais1Click(Sender: TObject);
    procedure FechaRelatrio1Click(Sender: TObject);
    procedure HorarioVerao2Click(Sender: TObject);
    procedure Arredonda1Click(Sender: TObject);
    procedure MudaArredondamento1Click(Sender: TObject);
    procedure NumLoja1Click(Sender: TObject);
    procedure NumCRO1Click(Sender: TObject);
    procedure TestedeVelocidade1Click(Sender: TObject);
    procedure chArredondaPorQtdClick(Sender: TObject);
    procedure CapturaporNReduaoZ1Click(Sender: TObject);
    procedure ImprimeporNReduaoZ1Click(Sender: TObject);
    procedure CapturaporPeriodo1Click(Sender: TObject);
    procedure ImprimeporPeriodo1Click(Sender: TObject);
    procedure ProgramaAliquota1Click(Sender: TObject);
    procedure ProgramaComprovanteNAOFiscal1Click(Sender: TObject);
    procedure ACBrECF1MsgPoucoPapel(Sender: TObject);
    procedure ProgramaFormadePagamento1Click(Sender: TObject);
    procedure CorrigeEstadodeErro1Click(Sender: TObject);
    procedure ImprimeCheque1Click(Sender: TObject);
    procedure CarregaUnidadesdeMedida1Click(Sender: TObject);
    procedure ProgramaUnidadeMedida1Click(Sender: TObject);
    procedure AbreRelatorioGerencial1Click(Sender: TObject);
    procedure AbreCupomVinculado1Click(Sender: TObject);
    procedure ImprimeLinhaRelatorio1Click(Sender: TObject);
    procedure ImprimeLinhaVinculado1Click(Sender: TObject);
    procedure ListaRelatorioGerencial1Click(Sender: TObject);
    procedure ListaCupomVinculado1Click(Sender: TObject);
    procedure PularLinhas1Click(Sender: TObject);
    procedure chGavetaSinalInvertidoClick(Sender: TObject);
    procedure LerTodasasVariveis1Click(Sender: TObject);
    procedure MFD1Click(Sender: TObject);
    procedure Termica1Click(Sender: TObject);
    procedure edLogChange(Sender: TObject);
    procedure SbArqLogClick(Sender: TObject);
    procedure cbMemoHTMLClick(Sender: TObject);
    procedure bBobinaLimparClick(Sender: TObject);
    procedure bBobinaParamsClick(Sender: TObject);
    procedure ACBrECF1BobinaAdicionaLinhas(const Linhas, Operacao: String);
    procedure DadosReducaoZ1Click(Sender: TObject);
    procedure CNPJIE1Click(Sender: TObject);
    procedure NumCRZ1Click(Sender: TObject);
    procedure NumCOOInicial1Click(Sender: TObject);
    procedure VendaBruta1Click(Sender: TObject);
    procedure GrandeTotal1Click(Sender: TObject);
    procedure TotalCancelamentos1Click(Sender: TObject);
    procedure TotalDescontos1Click(Sender: TObject);
    procedure TotalAcrescimos1Click(Sender: TObject);
    procedure LerTotaisAliquotas1Click(Sender: TObject);
    procedure LerTotaisFormadePagamento1Click(Sender: TObject);
    procedure LerTotaisComprovanetNaoFiscal1Click(Sender: TObject);
    procedure UltimoItemVendido1Click(Sender: TObject);
    procedure PorCOO1Click(Sender: TObject);
    procedure PorPeriodo1Click(Sender: TObject);
    procedure Estado1Click(Sender: TObject);
    procedure NoFiscalCompleto1Click(Sender: TObject);
    procedure AbreNoFiscal1Click(Sender: TObject);
    procedure RegistraItemNaoFiscal1Click(Sender: TObject);
    procedure SubTotalizaNaoFiscal1Click(Sender: TObject);
    procedure EfetuaPagamentoNaoFiscal1Click(Sender: TObject);
    procedure FechaNoFiscal1Click(Sender: TObject);
    procedure CancelaNoFiscal1Click(Sender: TObject);
    procedure NumCCF1Click(Sender: TObject);
    procedure NumCOO1Click(Sender: TObject);
    procedure IdentificaConsumidor1Click(Sender: TObject);
    procedure edDirRFDChange(Sender: TObject);
    procedure sbDirRFDClick(Sender: TObject);
    procedure bRFDLerClick(Sender: TObject);
    procedure bRFDSalvarClick(Sender: TObject);
    procedure chRFDClick(Sender: TObject);
    procedure seTimeOutChange(Sender: TObject);
    procedure seIntervaloAposComandoChange(Sender: TObject);
    procedure edSH_RazaoSocialChange(Sender: TObject);
    procedure edSH_COOChange(Sender: TObject);
    procedure edSH_CNPJChange(Sender: TObject);
    procedure edSH_IEChange(Sender: TObject);
    procedure edSH_IMChange(Sender: TObject);
    procedure edSH_AplicativoChange(Sender: TObject);
    procedure edSH_NumeroAPChange(Sender: TObject);
    procedure edSH_VersaoAPChange(Sender: TObject);
    procedure edSH_Linha1Change(Sender: TObject);
    procedure edSH_Linha2Change(Sender: TObject);
    procedure IE1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure otalNoFiscal1Click(Sender: TObject);
    procedure DataMovimento1Click(Sender: TObject);
    procedure DadosUltimaReduoZ1Click(Sender: TObject);
    procedure btSerialClick(Sender: TObject);

    procedure chDescricaoGrandeClick(Sender: TObject);
    procedure CortaPapel1Click(Sender: TObject);
    procedure edMsgTrabalhandoChange(Sender: TObject);
    procedure edOperadorChange(Sender: TObject);
    procedure Sangria1Click(Sender: TObject);
    procedure Suprimento1Click(Sender: TObject);
    procedure TestedeVinculado1Click(Sender: TObject);
    procedure ProgramaRelatrioGerencial1Click(Sender: TObject);
    procedure CarregaRelatriosGerenciais1Click(Sender: TObject);
    procedure LegendaInmetroproximoItem1Click(Sender: TObject);
    procedure CancelaItemVendidoParcial1Click(Sender: TObject);
    procedure CancelaDescontoAcrescimoItem1Click(Sender: TObject);
    procedure CancelaDescontoAcrescimoSubTotal1Click(Sender: TObject);
    procedure ConsultaRegistradorECF1Click(Sender: TObject);
    procedure EstornaMeiodePagamento1Click(Sender: TObject);
    procedure DeCodificaTexto1Click(Sender: TObject);
    procedure AchaAliquotaporIndice1Click(Sender: TObject);
    procedure AchaAliquotaporValor1Click(Sender: TObject);
    procedure AcharMeioPagamentoporIndice1Click(Sender: TObject);
    procedure AcharMeiodePagametoporDescrio1Click(Sender: TObject);
    procedure AchaCNFporIndice1Click(Sender: TObject);
    procedure AchaCNFporDescrio1Click(Sender: TObject);
    procedure AchaRGporIndice1Click(Sender: TObject);
    procedure AchaRGporDescrio1Click(Sender: TObject);
    procedure PorCOO2Click(Sender: TObject);
    procedure PorDatadeMovimento1Click(Sender: TObject);
    procedure PorCOO3Click(Sender: TObject);
    procedure PorPeriodo2Click(Sender: TObject);
    procedure UsuarioAual1Click(Sender: TObject);
    procedure PorCOO4Click(Sender: TObject);
    procedure PorPeriodo3Click(Sender: TObject);
    procedure NumGNF1Click(Sender: TObject);
    procedure btnDadosRZClick(Sender: TObject);
    procedure LerTroco1Click(Sender: TObject);
    procedure speLinBufChange(Sender: TObject);
    procedure NumSerieMFDClick(Sender: TObject);
    procedure ParametroDescontoISSQN1Click(Sender: TObject);
    procedure mModeloStrClick(Sender: TObject);
    procedure btnMenuFiscalLXClick(Sender: TObject);
    procedure btnMenuFiscalLMFCClick(Sender: TObject);
    procedure btnMenuFiscalLMFSClick(Sender: TObject);
    procedure btnMenuFiscalMFDEspelhoClick(Sender: TObject);
    procedure btnMenuFiscalMFDArqClick(Sender: TObject);
    procedure btnMenuFiscalRelMeiosPagtoClick(Sender: TObject);
    procedure btnMenuFiscalRelDAVEmitidosClick(Sender: TObject);
    procedure btnMenuFiscalRelIdentPAFECFClick(Sender: TObject);
    procedure RelatorioGerencialcomformatacao1Click(Sender: TObject);
    procedure btnMenuFiscalConfigPAFECFClick(Sender: TObject);
    procedure ACBrECF1ChangeEstado(const EstadoAnterior, EstadoAtual: TACBrECFEstado);
    procedure DAV1Click(Sender: TObject);
    procedure DAVOS1Click(Sender: TObject);
    procedure btnMenuFiscalNotaPaulistaClick(Sender: TObject);
    procedure chArredondamentoItemMFDClick(Sender: TObject);
    procedure chBarrasImprimeTextoClick(Sender: TObject);
    procedure chIgnorarTagsFormatacaoClick(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure speBarrasLarguraChange(Sender: TObject);
    procedure speBarrasAlturaChange(Sender: TObject);
    procedure chbCupomManiaClick(Sender: TObject);
    procedure DataHoraltimaReduoZ1Click(Sender: TObject);
    procedure btnArqMFNovoClick(Sender: TObject);
    procedure btnArqMFDNovoClick(Sender: TObject);
    procedure btnDadosUltimaRZClick(Sender: TObject);
    procedure ProgramaIdentificaoPafECF1Click(Sender: TObject);
    procedure ACBrECFVirtualNaoFiscal1GravaArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
    procedure ACBrECFVirtualNaoFiscal1LeArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
    procedure VendaFrentica1Click(Sender: TObject);
    procedure sePaginaCodigoChange(Sender: TObject);
    procedure ACBrECFVirtualNFCe1QuandoEfetuarPagamento(Det: TpagCollectionItem);
    procedure ACBrECFVirtualNFCe1QuandoFecharDocumento(NFe: TNFe);
    procedure ACBrECFVirtualNFCe1QuandoGravarArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
    procedure ACBrECFVirtualNFCe1QuandoLerArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
    procedure ACBrECFVirtualNFCe1QuandoVenderItem(Det: TDetCollectionItem);
    procedure ACBrSAT1GetsignAC(var Chave: AnsiString);
    procedure ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
    procedure ACBrECFVirtualSAT1QuandoAbrirDocumento(CFe: TCFe);
    procedure ACBrECFVirtualSAT1QuandoCancelarCupom(const NumCOOCancelar: Integer; CupomVirtual: TACBrECFVirtualClassCupom;
      var PermiteCancelamento: Boolean);
    procedure ACBrECFVirtualSAT1QuandoEfetuarPagamento(Det: TMPCollectionItem);
    procedure ACBrECFVirtualSAT1QuandoGravarArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
    procedure ACBrECFVirtualSAT1QuandoLerArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
    procedure ACBrECFVirtualSAT1QuandoVenderItem(Det: TDetCollectionItem);
    procedure ACBrAAC1GetChave(var Chave: AnsiString);
    procedure ACBrAAC1DepoisAbrirArquivo(Sender: TObject);
    procedure ACBrAAC1AntesGravarArquivo(var Continua: Boolean);
    procedure ACBrAAC1AntesAbrirArquivo(var Continua: Boolean);
    procedure btSerial1Click(Sender: TObject);
    procedure bACCVerificarGTClick(Sender: TObject);
    procedure bAACAtualizarGTClick(Sender: TObject);
    procedure bAACAbrirArquivoClick(Sender: TObject);
    procedure bAACGravarArquivoClick(Sender: TObject);
    procedure SbAACMD5AtualizarClick(Sender: TObject);
    procedure SbAACNomeArqClick(Sender: TObject);
    procedure SbAACArqLogClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnLogoMarcaClick(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SalvarParmetros1Click(Sender: TObject);
    procedure lerParmetros1Click(Sender: TObject);
    procedure chControlePortaClick(Sender: TObject);
    procedure cbxECFVirtualChange(Sender: TObject);
    procedure seBandWidthChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure chAACUsarClick(Sender: TObject);
    procedure chAACFlushClick(Sender: TObject);
    procedure chProcessMessagesClick(Sender: TObject);
    procedure mConsultarStatusSATClick(Sender: TObject);
    procedure mConsultarSATClick(Sender: TObject);
    procedure mConsultarSessaoSATClick(Sender: TObject);
    procedure CarregarCFe1Click(Sender: TObject);
    procedure ImprimirExtratoVenda1Click(Sender: TObject);
    procedure ImprimirExtratoResumido1Click(Sender: TObject);
    procedure ImprimirExtratoCancelamento1Click(Sender: TObject);
    procedure estedeArredondamento1Click(Sender: TObject);
    procedure estedeRateio1Click(Sender: TObject);
    procedure LeituraXSerial1Click(Sender: TObject);
    procedure CarregaTotalizadoresNaoTributados1Click(Sender: TObject);
    procedure LerTotaisTotalizadoresNaoTributados1Click(Sender: TObject);
    procedure AchaTotalizadorNaoTributadoIndice1Click(Sender: TObject);
    procedure estarLeituradeTotais1Click(Sender: TObject);
    procedure NmeroReduesZrestantes1Click(Sender: TObject);
    procedure NumGNFC1Click(Sender: TObject);
    procedure NumGRG1Click(Sender: TObject);
    procedure NumCDC1Click(Sender: TObject);
    procedure NumCCDC1Click(Sender: TObject);
    procedure NumCFD1Click(Sender: TObject);
    procedure NumNCN1Click(Sender: TObject);
    procedure otalTroco1Click(Sender: TObject);
    procedure otalCancelamentos1Click(Sender: TObject);
    procedure otalDescontos1Click(Sender: TObject);
    procedure otalAcrescimos1Click(Sender: TObject);
    procedure otalSubstituicaoTributaria1Click(Sender: TObject);
    procedure otalNaoTributado1Click(Sender: TObject);
    procedure otalIseno1Click(Sender: TObject);
    procedure otalCancelamentos2Click(Sender: TObject);
    procedure otalDescontos2Click(Sender: TObject);
    procedure otalAcrescimos2Click(Sender: TObject);
    procedure otalSubstituicaoTributaria2Click(Sender: TObject);
    procedure otalNaoTributado2Click(Sender: TObject);
    procedure otalIseno2Click(Sender: TObject);
    procedure ValorTotal1Click(Sender: TObject);
    procedure otalCancelamentos3Click(Sender: TObject);
    procedure otalDescontos3Click(Sender: TObject);
    procedure otalAcrscimos1Click(Sender: TObject);
    procedure MFAdicional1Click(Sender: TObject);
    procedure ipoltimoDocumento1Click(Sender: TObject);
    procedure IdentificaConsumidorRodap1Click(Sender: TObject);
    procedure SubModeloECF1Click(Sender: TObject);
    procedure IM1Click(Sender: TObject);
    procedure PAF1Click(Sender: TObject);
    procedure Cliche1Click(Sender: TObject);
    procedure DataHoraSwBasico1Click(Sender: TObject);
    procedure Decimais1Click(Sender: TObject);
    procedure Colunas1Click(Sender: TObject);
    procedure EstornaCCD1Click(Sender: TObject);
    procedure LeituraCMC71Click(Sender: TObject);
    procedure IdentificaOperador1Click(Sender: TObject);
    procedure AbreBilhetePassagem1Click(Sender: TObject);
  private
    mdsAACECF: TDataSet;
    { Private declarations }
    Function Converte(cmd: String): String;
    procedure TrataErros(Sender: TObject; E: Exception);
    function EstadoECF: String;
    Procedure GravarINI;
    Procedure LerINI;
    procedure AjustaACBrNFe;
    procedure PrepararImpressaoNFCe;
    procedure AjustaACBrSAT;
    procedure PrepararImpressaoSAT;

    procedure WB_LoadHTML(WebBrowser: TWebBrowser; HTMLCode: string);
    procedure WB_ScrollToBottom(WebBrowser1: TWebBrowser);
    procedure WB_ScrollToTop(WebBrowser1: TWebBrowser);

  public
    { Public declarations }
    Procedure AtualizaMemos(VerificaEstado: Boolean = true);
  end;

const
  ECFTeste_VERSAO = '2.01';
  Estados: array [TACBrECFEstado] of string = ('Não Inicializada', 'Desconhecido', 'Livre', 'Venda', 'Pagamento', 'Relatório', 'Bloqueada',
    'Requer Z', 'Requer X', 'Nao Fiscal');
  _C = 'tYk*5W@';

var
  Form1: TForm1;

implementation

uses ACBrUtil, ACBrECFBematech, VendeItem, EfetuaPagamento,
  Relatorio, Sobre, TypInfo, Math, ActiveX, MSHTML, IniFiles,
  ConfiguraSerial, ACBrPAFClass, RelatorioGerencialFormatado, uDAV, uDAVOS, uVendaFrenetica;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
Var
  I: TACBrECFModelo;
begin
  cbxModelo.Items.Clear;
  For I := Low(TACBrECFModelo) to High(TACBrECFModelo) do
    cbxModelo.Items.Add(GetEnumName(TypeInfo(TACBrECFModelo), Integer(I)));
  cbxModelo.Items[0] := 'Procurar';
  cbxModelo.ItemIndex := 0;

  cbxPorta.Items.Clear;
  ACBrECF1.Device.AcharPortasSeriais(cbxPorta.Items);
  cbxPorta.Items.Insert(0, 'Procurar');
  cbxPorta.Items.Add('USB');
  cbxPorta.Items.Add('LPT1');
  cbxPorta.Items.Add('LPT2');
  cbxPorta.Items.Add('LPT3');
  cbxPorta.Items.Add('/dev/ttyS0');
  cbxPorta.Items.Add('/dev/ttyS1');
  cbxPorta.Items.Add('/dev/ttyUSB0');
  cbxPorta.Items.Add('/dev/ttyUSB1');
  cbxPorta.Items.Add('c:\temp\ecf.txt');
  cbxPorta.Items.Add('/tmp/ecf.txt');

  mMsgChange(Sender);
  Application.OnException := TrataErros;
  pgPrincipal.ActivePageIndex := 0;

  LerINI;

  if FileExists('ACBrECFMemoParams.ini') then
    ACBrECF1.MemoParams.LoadFromFile('ACBrECFMemoParams.ini');

  cbMemoHTML.Checked := (ACBrECF1.MemoParams.Values['HTML'] = '1');

  if (not chRFD.Checked) and DirectoryExists(ACBrRFD1.DirRFD) then
    chRFD.Checked := true;
end;

{-----------------------------------------------------------------------------}
Procedure TForm1.TrataErros(Sender: TObject; E: Exception);
begin
  mResp.Lines.Add(E.Message);
  StatusBar1.Panels[0].Text := 'Exception';
  AtualizaMemos(False);
  StatusBar1.Panels[2].Text := E.Message;
  //  PageControl1.ActivePageIndex := 1 ;
  //  MessageDlg( E.Message,mtError,[mbOk],0) ;
end;

procedure TForm1.AtualizaMemos(VerificaEstado: Boolean = true);
begin
  mEnviado.Text := Converte(ACBrECF1.ComandoEnviado);
  mResp.Lines.Add(Converte(ACBrECF1.RespostaComando));
  mResp.Lines.Add('- + - + - + - + - + - + - + - + - + - + - + -');
  if VerificaEstado then
    StatusBar1.Panels[0].Text := EstadoECF;
end;

Function TForm1.EstadoECF: String;
begin
  try
    Result := Estados[ACBrECF1.Estado];
    { GetEnumName(TypeInfo(TACBrECFEstado), integer( ACBrECF1.Estado ) ) ;}
  except
    Result := 'Falha ao ler';
    mResp.Lines.Add('**** Falha ao ler ESTADO do ECF ****');
  end;
end;

procedure TForm1.estarLeituradeTotais1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.CorrigeEstadoErro();
  ACBrECF1.AbreCupom();

  For A := 0 to ACBrECF1.TotalizadoresNaoTributados.Count - 1 do
  begin
    ACBrECF1.VendeItem('1' + IntToStrZero(A, 5), 'TOTALIZADOR NAO TRIBUTADO ' + ACBrECF1.TotalizadoresNaoTributados[A].Indice,
      ACBrECF1.TotalizadoresNaoTributados[A].Indice, 1, 1 + A);
  end;

  ACBrECF1.SubtotalizaCupom();
  ACBrECF1.EfetuaPagamento(ACBrECF1.FormasPagamento[0].Indice, ACBrECF1.Subtotal);
  ACBrECF1.FechaCupom('FIM DO TESTES|CONFIRA VALORES COM A LEITURAX');

  ACBrECF1.LeituraX;

  LerTotaisTotalizadoresNaoTributados1Click(Sender);
end;

procedure TForm1.estedeArredondamento1Click(Sender: TObject);
var
  SubTot, TotalEsperado: Double;
  OldArr: Boolean;
begin
  Form1.Enabled := False;
  OldArr := ACBrECF1.ArredondaItemMFD;
  try
    ACBrECF1.ArredondaItemMFD := true;
    mResp.Lines.Add('Abrindo Cupom...');
    ACBrECF1.AbreCupom();
    mResp.Lines.Add('Cupom Aberto.');

    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 0.015);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 0.025);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 0.035);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 0.045);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 5.555);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 1.875);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 47.76226);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 36.21672);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 47.2150);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 58.6851);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 72.3650);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 58.93497);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 93.58746);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 667.4756);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 667.4856);
    ACBrECF1.VendeItem('123456789', 'TESTE ARREDONDAMENTO', 'NN', 1, 667.4850);

    mResp.Lines.Add('SubTotalizando o Cupom.');
    ACBrECF1.SubtotalizaCupom();

    mResp.Lines.Add('Verificando o SubTotal Calculado com o do ECF');
    SubTot := ACBrECF1.Subtotal;
    TotalEsperado := 2424.78;
    if SubTot <> TotalEsperado then
      mResp.Lines.Add('SubTotal do ECF: (' + FormatFloat('0.00', SubTot) + ') diferente do esperado (' + FormatFloat('0.00', TotalEsperado) + ')!')
    else
    begin
      mResp.Lines.Add('O SubTotal está correto.  Verifique a Bobina da Tela...');
      pgPrincipal.ActivePage := tsCupom;
    end;

    ACBrECF1.CancelaCupom;
  finally
    ACBrECF1.ArredondaItemMFD := OldArr;
    Form1.Enabled := true;
  end;
end;

procedure TForm1.estedeRateio1Click(Sender: TObject);
var
  Subtotal: Double;
  Aliq: TACBrECFAliquota;
begin
  //http://partners.bematech.com.br/bemacast/Paginas/post.aspx?idPost=5790

  ACBrECF1.CarregaFormasPagamento;
  if ACBrECF1.FormasPagamento.Count < 1 then
    raise Exception.Create('Nenhuma Forma de Pagamento programada no ECF');

  ACBrECF1.CarregaAliquotas;

  Aliq := ACBrECF1.AchaICMSAliquota(18);
  if not Assigned(Aliq) then
    raise Exception.Create('Aliquota 18% não encontrada');

  Aliq := ACBrECF1.AchaICMSAliquota(7);
  if not Assigned(Aliq) then
    raise Exception.Create('Aliquota 7% não encontrada');

  ACBrECF1.CorrigeEstadoErro();
  ACBrECF1.AbreCupom();
  ACBrECF1.VendeItem('0001', 'PRODUTO 01 aliquota A', '18', 1, 369.37);
  ACBrECF1.VendeItem('0002', 'PRODUTO 02 aliquota B', '07', 1, 456.99);
  ACBrECF1.VendeItem('0003', 'PRODUTO 03 aliquota B', '07', 1, 277.33);
  ACBrECF1.VendeItem('0004', 'PRODUTO 04 aliquota A', '18', 1, 998.13);
  ACBrECF1.VendeItem('0005', 'PRODUTO 05 aliquota B', '07', 1, 554.11);
  ACBrECF1.VendeItem('0006', 'PRODUTO 06 aliquota A', '18', 1, 682.77);

  ACBrECF1.SubtotalizaCupom(-298.82);

  Subtotal := ACBrECF1.Subtotal;
  ACBrECF1.EfetuaPagamento(ACBrECF1.FormasPagamento[0].Indice, Subtotal);
  ACBrECF1.FechaCupom('TESTE DE RATEIO');
end;

function TForm1.Converte(cmd: String): String;
var
  A: Integer;
begin
  Result := '';
  For A := 1 to length(cmd) do
  begin
    if (Ord(cmd[A]) < 32) or (Ord(cmd[A]) > 127) then
      Result := Result + '#' + IntToStr(Ord(cmd[A]))
    else
      Result := Result + cmd[A];
  end;
end;

procedure TForm1.cbxECFVirtualChange(Sender: TObject);
begin
  if ACBrECF1.Ativo then
    bAtivar.Click;

  if cbxECFVirtual.ItemIndex = 1 then
  begin
    ACBrECF1.ECFVirtual := ACBrECFVirtualSAT1
  end
  else if cbxECFVirtual.ItemIndex = 2 then
  begin
    ACBrECF1.ECFVirtual := ACBrECFVirtualNFCe1;
  end
  else
    ACBrECF1.ECFVirtual := ACBrECFVirtualNaoFiscal1;
end;

procedure TForm1.cbxModeloChange(Sender: TObject);
begin
  try
    ACBrECF1.Modelo := TACBrECFModelo(cbxModelo.ItemIndex);
    cbxECFVirtual.Enabled := (ACBrECF1.Modelo = ecfECFVirtual);
  except
    cbxModelo.ItemIndex := Integer(ACBrECF1.Modelo);
    raise;
  end;
end;

procedure TForm1.Sair1Click(Sender: TObject);
begin
  close;
end;

procedure TForm1.SalvarParmetros1Click(Sender: TObject);
begin
  GravarINI;
end;

procedure TForm1.bAACAbrirArquivoClick(Sender: TObject);
begin
  ACBrAAC1.AbrirArquivo;
end;

procedure TForm1.bAACAtualizarGTClick(Sender: TObject);
begin
  ACBrAAC1.AtualizarValorGT(mdsAACECF.FieldByName('NumSerie').AsString, mdsAACECF.FieldByName('ValorGT').AsFloat);
end;

procedure TForm1.bAACGravarArquivoClick(Sender: TObject);
begin
  ACBrAAC1.IdentPAF.Empresa.RazaoSocial := edAAC_SH_RazaoSocial.Text;
  ACBrAAC1.IdentPAF.Empresa.CNPJ := edAAC_SH_CNPJ.Text;
  ACBrAAC1.IdentPAF.Empresa.IM := edAAC_SH_IM.Text;
  ACBrAAC1.IdentPAF.Empresa.IE := edAAC_SH_IE.Text;
  ACBrAAC1.IdentPAF.Paf.Nome := edAAC_PAF_Aplicativo.Text;
  ACBrAAC1.IdentPAF.Paf.Versao := edAAC_PAF_Versao.Text;
  ACBrAAC1.IdentPAF.ArquivoListaAutenticados.MD5 := edAAC_PAF_MD5.Text;

  ACBrAAC1.IdentPAF.ECFsAutorizados.Clear;
  with mdsAACECF do
  begin
    // Zera Tabela em memoria //
    First;
    while not EOF do
    begin
      with ACBrAAC1.IdentPAF.ECFsAutorizados.New do
      begin
        NumeroSerie := FieldByName('NumSerie').AsString;
        CRO := FieldByName('CRO').AsInteger;
        ValorGT := FieldByName('ValorGT').AsFloat;
        DtHrAtualizado := FieldByName('DtHrAtualizado').AsDateTime;
      end;
      Next;
    end;
  end;

  ACBrAAC1.Params.Assign(mAACParams.Lines);

  ACBrAAC1.SalvarArquivo;
end;

procedure TForm1.bACCVerificarGTClick(Sender: TObject);
var
  Erro: Integer;
  Msg: String;
  ValorGT: Double;
begin
  ValorGT := ACBrECF1.GrandeTotal; //mdsAACECF.FieldByName('ValorGT').AsFloat ;
  Erro := ACBrAAC1.VerificarGTECF(mdsAACECF.FieldByName('NumSerie').AsString, ValorGT);

  case Erro of
    0:
      Msg := 'G.T. OK';
    -1:
      Msg := 'Num.Serie não encontrado';
    -2:
      Msg := 'GT diferente';
  else
    Msg := 'Erro não definido';
  end;

  ShowMessage(Msg);
end;

procedure TForm1.bAtivarClick(Sender: TObject);
begin
  if bAtivar.Caption = 'Ativar' then
    Ativar1Click(Sender)
  else
    Desativar1Click(Sender);
end;

procedure TForm1.cbxPortaChange(Sender: TObject);
begin
  try
    ACBrECF1.Porta := cbxPorta.Text;
  finally
    cbxPorta.Text := ACBrECF1.Porta;
  end;
end;

procedure TForm1.Ativar1Click(Sender: TObject);
begin
  try
    Self.Enabled := False;
    ACBrECF1.Porta := cbxPorta.Text;

    if cbxModelo.ItemIndex = 0 then
      if not ACBrECF1.AcharECF(true, False) then
      begin
        MessageDlg('Nenhum ECF encontrado.', mtInformation, [mbOk], 0);
        exit;
      end;

    ACBrECF1.Ativar;

    btSerial.Enabled := False;
    bAtivar.Caption := 'Desativar';
    mResp.Lines.Add('Ativar');
    AtualizaMemos;

    GravarINI;

    if pgPrincipal.ActivePageIndex = 0 then
      pgPrincipal.ActivePageIndex := 1;
  finally
    Self.Enabled := true;
    cbxModelo.ItemIndex := Integer(ACBrECF1.Modelo);
    cbxPorta.Text := ACBrECF1.Porta;
    sePaginaCodigo.Value := ACBrECF1.PaginaDeCodigo;
  end;
end;

procedure TForm1.Desativar1Click(Sender: TObject);
begin
  ACBrECF1.Desativar;
  bAtivar.Caption := 'Ativar';
  mResp.Lines.Add('Desativar');
  AtualizaMemos;
  btSerial.Enabled := true;
end;

procedure TForm1.chTentarClick(Sender: TObject);
begin
  ACBrECF1.ReTentar := chTentar.Checked;
end;

procedure TForm1.chBloqueiaClick(Sender: TObject);
begin
  ACBrECF1.BloqueiaMouseTeclado := chBloqueia.Checked;
end;

procedure TForm1.chControlePortaClick(Sender: TObject);
begin
  ACBrECF1.ControlePorta := chControlePorta.Checked;
end;

procedure TForm1.chExibeMsgClick(Sender: TObject);
begin
  ACBrECF1.ExibeMensagem := chExibeMsg.Checked;
end;

procedure TForm1.chArredondaPorQtdClick(Sender: TObject);
begin
  ACBrECF1.ArredondaPorQtd := chArredondaPorQtd.Checked;
end;

procedure TForm1.chDescricaoGrandeClick(Sender: TObject);
begin
  ACBrECF1.DescricaoGrande := chDescricaoGrande.Checked;
end;

procedure TForm1.mMsgChange(Sender: TObject);
Var
  Msg: String;
  L: Integer;
begin
  Msg := '';
  For L := 0 to mMsg.Lines.Count - 1 do
  begin
    Msg := Msg + mMsg.Lines[L] + #10
  end;

  ACBrECF1.MsgAguarde := copy(Msg, 1, length(Msg) - 1);
end;

procedure TForm1.Testar1Click(Sender: TObject);
begin
  ACBrECF1.TestarDialog;
  AtualizaMemos;
end;

procedure TForm1.ACBrECF1MsgAguarde(Mensagem: String);
begin
  StatusBar1.Panels[2].Text := StringReplace(Mensagem, #10, ' ', [rfReplaceAll]);
end;

procedure TForm1.DataHora1Click(Sender: TObject);
begin
  mResp.Lines.Add('Data/Hora: ' + DateTimeToStr(ACBrECF1.DataHora));
  AtualizaMemos;
end;

procedure TForm1.NumECF1Click(Sender: TObject);
begin
  mResp.Lines.Add('N.ECF: (' + ACBrECF1.NumECF + ')');
  AtualizaMemos;
end;

procedure TForm1.NSrie1Click(Sender: TObject);
begin
  mResp.Lines.Add('N.Série: (' + ACBrECF1.NumSerie + ')');
  AtualizaMemos;
end;

procedure TForm1.NVerso1Click(Sender: TObject);
begin
  mResp.Lines.Add('N.Versão: ' + ACBrECF1.NumVersao);
  AtualizaMemos;
end;

procedure TForm1.NumUltimoCupom1Click(Sender: TObject);
begin
  mResp.Lines.Add('N.Último Cupom: (' + ACBrECF1.NumCupom + ')');
  AtualizaMemos;
end;

procedure TForm1.PoucoPapel1Click(Sender: TObject);
begin
  mResp.Lines.Add('Pouco Papel: ' + IfThen(ACBrECF1.PoucoPapel, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.LeituraCMC71Click(Sender: TObject);
begin
mResp.Lines.Add( 'Leitura CMC7: ['+ACBrECF1.LeituraCMC7+']' );
  AtualizaMemos ;
end;

procedure TForm1.LeituraX1Click(Sender: TObject);
begin
  ACBrECF1.LeituraX;
  mResp.Lines.Add('Leitura X');
  AtualizaMemos;
end;

procedure TForm1.LeituraXSerial1Click(Sender: TObject);
Var
  Linhas: TStringList;
  I: Integer;
  Arquivo: String;
begin
  Arquivo := 'C:\TEMP\LeituraX.txt';
  if not InputQuery('LeituraX Serial', 'Nome Arquivo (vazio lista no Memo):', Arquivo) then
    exit;

  Arquivo := Trim(Arquivo);
  if Arquivo <> '' then
    ACBrECF1.LeituraXSerial(Arquivo)
  else
  begin
    Linhas := TStringList.Create;
    try
      ACBrECF1.LeituraXSerial(Linhas);

      For I := 0 to Linhas.Count - 1 do
        mResp.Lines.Add(Linhas[I]);
    finally
      Linhas.Free;
    end;
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.ReduoZ1Click(Sender: TObject);
Var
  Resp: TModalResult;
begin
  if ACBrECF1.Estado <> estRequerZ then
  begin
    if MessageDlg('A Redução Z pode Bloquear o seu ECF até a 12:00pm' + #10 + #10 + 'Continua assim mesmo ?', mtWarning, mbYesNoCancel, 0) <> mrYes
    then
      exit;

    if MessageDlg('Você tem certeza ?', mtWarning, mbYesNoCancel, 0) <> mrYes then
      exit;
  end;

  Resp := MessageDlg('Envia hora atual ?', mtConfirmation, mbYesNoCancel, 0);

  if Resp = mrYes then
    ACBrECF1.ReducaoZ(now)
  else if Resp = mrNo then
    ACBrECF1.ReducaoZ()
  else
    exit;

  mResp.Lines.Add('Reduçao Z');
  AtualizaMemos;
end;

procedure TForm1.Sobre1Click(Sender: TObject);
begin
  ACBrAboutDialog;
end;

procedure TForm1.AliquotasICMS1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.CarregaAliquotas;

  for A := 0 to ACBrECF1.Aliquotas.Count - 1 do
  begin
    mResp.Lines.Add('Aliquota: ' + IntToStrZero(ACBrECF1.Aliquotas[A].Sequencia, 2) + ' Indice: ' + ACBrECF1.Aliquotas[A].Indice + ' -> ' +
      FloatToStr(ACBrECF1.Aliquotas[A].Aliquota) + ' Tipo: ' + ACBrECF1.Aliquotas[A].Tipo);
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.LerTotaisAliquotas1Click(Sender: TObject);
Var
  A: Integer;
begin
  ACBrECF1.LerTotaisAliquota;

  for A := 0 to ACBrECF1.Aliquotas.Count - 1 do
  begin
    mResp.Lines.Add('Aliquota: ' + ACBrECF1.Aliquotas[A].Indice + ' - ' + FloatToStr(ACBrECF1.Aliquotas[A].Aliquota) + ' Tipo: ' +
      ACBrECF1.Aliquotas[A].Tipo + ' -> ' + FormatFloat('###,##0.00', ACBrECF1.Aliquotas[A].Total));
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.FormasdePagamento1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.CarregaFormasPagamento;

  for A := 0 to ACBrECF1.FormasPagamento.Count - 1 do
  begin
    if ACBrECF1.FormasPagamento[A].Descricao <> '' then
      mResp.Lines.Add('Forma Pagto: ' + ACBrECF1.FormasPagamento[A].Indice + ' -> ' + ACBrECF1.FormasPagamento[A].Descricao + '  Permite Vinculado: '
        + IfThen(ACBrECF1.FormasPagamento[A].PermiteVinculado, 'S', 'N'));
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.LerTotaisFormadePagamento1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.LerTotaisFormaPagamento;

  for A := 0 to ACBrECF1.FormasPagamento.Count - 1 do
  begin
    if ACBrECF1.FormasPagamento[A].Descricao <> '' then
      mResp.Lines.Add('Forma Pagto: ' + ACBrECF1.FormasPagamento[A].Indice + ' - ' + ACBrECF1.FormasPagamento[A].Descricao + '  -> ' +
        FormatFloat('###,##0.00', ACBrECF1.FormasPagamento[A].Total));
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.LerTotaisTotalizadoresNaoTributados1Click(Sender: TObject);
Var
  A: Integer;
begin
  ACBrECF1.LerTotaisTotalizadoresNaoTributados;

  for A := 0 to ACBrECF1.TotalizadoresNaoTributados.Count - 1 do
  begin
    mResp.Lines.Add('TotalizadorNaoTributado: ' + ACBrECF1.TotalizadoresNaoTributados[A].Indice + ' - ' + ACBrECF1.TotalizadoresNaoTributados[A].Tipo
      + ' -> ' + FloatToStr(ACBrECF1.TotalizadoresNaoTributados[A].Total));
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.LerTroco1Click(Sender: TObject);
begin
  mResp.Lines.Add('Total do Troco: ' + FormatFloat('#,###,##0.00', ACBrECF1.TotalTroco));
  AtualizaMemos;
end;

procedure TForm1.CarregaComprovantesNAOFiscais1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.CarregaComprovantesNaoFiscais;

  for A := 0 to ACBrECF1.ComprovantesNaoFiscais.Count - 1 do
  begin
    if ACBrECF1.ComprovantesNaoFiscais[A].Descricao <> '' then
      mResp.Lines.Add('CNF: ' + ACBrECF1.ComprovantesNaoFiscais[A].Indice + ' -> ' + ACBrECF1.ComprovantesNaoFiscais[A].Descricao +
        '  Permite Vinculado: ' + IfThen(ACBrECF1.ComprovantesNaoFiscais[A].PermiteVinculado, 'S', 'N') + ' - FPG associada: ' +
        ACBrECF1.ComprovantesNaoFiscais[A].FormaPagamento);
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.CarregarCFe1Click(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo CFe|CFe*.xml';
  if OpenDialog1.Execute then
    ACBrSAT1.CFe.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.CarregaRelatriosGerenciais1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.CarregaRelatoriosGerenciais;

  for A := 0 to ACBrECF1.RelatoriosGerenciais.Count - 1 do
  begin
    if ACBrECF1.RelatoriosGerenciais[A].Descricao <> '' then
      mResp.Lines.Add('RG: ' + ACBrECF1.RelatoriosGerenciais[A].Indice + ' -> ' + PadRight(ACBrECF1.RelatoriosGerenciais[A].Descricao, 15) + ' CER:' +
        IntToStr(ACBrECF1.RelatoriosGerenciais[A].Contador));
  end;
  mResp.Lines.Add('---------------------------------');

end;

procedure TForm1.CarregaTotalizadoresNaoTributados1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.CarregaTotalizadoresNaoTributados;

  for A := 0 to ACBrECF1.TotalizadoresNaoTributados.Count - 1 do
  begin
    mResp.Lines.Add('TotalizadorNaoTributado: - Indice: ' + ACBrECF1.TotalizadoresNaoTributados[A].Indice + ' Tipo: ' +
      ACBrECF1.TotalizadoresNaoTributados[A].Tipo);
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.LerTotaisComprovanetNaoFiscal1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.LerTotaisComprovanteNaoFiscal;

  for A := 0 to ACBrECF1.ComprovantesNaoFiscais.Count - 1 do
  begin
    if ACBrECF1.ComprovantesNaoFiscais[A].Descricao <> '' then
      mResp.Lines.Add('CNF: ' + ACBrECF1.ComprovantesNaoFiscais[A].Indice + ' - ' + ACBrECF1.ComprovantesNaoFiscais[A].Descricao + ' CON (' +
        IntToStrZero(ACBrECF1.ComprovantesNaoFiscais[A].Contador, 4) + ') -> ' + FormatFloat('###,##0.00', ACBrECF1.ComprovantesNaoFiscais[A].Total));
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.AbreGaveta1Click(Sender: TObject);
begin
  ACBrECF1.AbreGaveta;
  mResp.Lines.Add('AbreGaveta');
  AtualizaMemos;
end;

procedure TForm1.GavetaAberta1Click(Sender: TObject);
begin
  mResp.Lines.Add('Gaveta Aberta: ' + IfThen(ACBrECF1.GavetaAberta, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.HorarioVerao1Click(Sender: TObject);
begin
  ACBrECF1.MudaHorarioVerao;
  mResp.Lines.Add('MudaHorarioVerao');
  AtualizaMemos;
end;

procedure TForm1.ImpactoAgulhas1Click(Sender: TObject);
begin
  ACBrECF1.ImpactoAgulhas;
  mResp.Lines.Add('ImpactoAgulhas');
  AtualizaMemos;
end;

procedure TForm1.TestaPodeAbrirCupom1Click(Sender: TObject);
var
  Est: String;
begin

  Est := Estados[ACBrECF1.Estado];

  try
    ACBrECF1.TestaPodeAbrirCupom;

    mResp.Lines.Add('Pode Abrir Cupom.. OK');
  except
    mResp.Lines.Add('NAO Pode Abrir Cupom..');
    mResp.Lines.Add('pois o estado Atual é ' + Est);
    AtualizaMemos;

    raise;
  end;

  AtualizaMemos;

end;

procedure TForm1.NUltimoCupom1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num Ultimo Cupom: ' + ACBrECF1.NumCupom);
  AtualizaMemos;
end;

procedure TForm1.SubTotal1Click(Sender: TObject);
begin
  mResp.Lines.Add('SubTotal: ' + FloatToStr(ACBrECF1.Subtotal));
  AtualizaMemos;
end;

procedure TForm1.TotalPago1Click(Sender: TObject);
begin
  mResp.Lines.Add('Total Pago: ' + FloatToStr(ACBrECF1.TotalPago));
  AtualizaMemos;
end;

procedure TForm1.AbrirCupom1Click(Sender: TObject);
Var
  Consumidor: String;
  CPF, Nome, ENDERECO: String;
  P: Integer;
begin
  Consumidor := ' ';
  if ACBrECF1.Consumidor.Documento <> '' then
    Consumidor := ACBrECF1.Consumidor.Documento;
  if ACBrECF1.Consumidor.Nome <> '' then
    Consumidor := Consumidor + '|' + ACBrECF1.Consumidor.Nome;
  if ACBrECF1.Consumidor.ENDERECO <> '' then
    Consumidor := Consumidor + '|' + ACBrECF1.Consumidor.ENDERECO;

  if InputQuery('Abre Cupom', 'Se necessário, Informe o Documento | Nome | Endereco' + sLineBreak +
    'Nota: Use o sinal pipe (|) para separar os campos', Consumidor) then
  begin
    wbBobina.Navigate('about:blank');
    mBobina.Clear;

    Consumidor := Trim(Consumidor);
    P := pos('|', Consumidor + '|');
    CPF := copy(Consumidor, 1, P - 1);
    Consumidor := copy(Consumidor, P + 1, length(Consumidor));
    P := pos('|', Consumidor + '|');
    Nome := copy(Consumidor, 1, P - 1);
    Consumidor := copy(Consumidor, P + 1, length(Consumidor));
    P := pos('|', Consumidor + '|');
    ENDERECO := copy(Consumidor, 1, P - 1);

    ACBrECF1.AbreCupom(CPF, Nome, ENDERECO);
    mResp.Lines.Add('AbreCupom');
    AtualizaMemos;
  end;
end;

procedure TForm1.CancelaCupom1Click(Sender: TObject);
begin
  ACBrECF1.CancelaCupom;
  mResp.Lines.Add('CancelaCupom');
  AtualizaMemos;
end;

procedure TForm1.VenderItem1Click(Sender: TObject);
begin
  frVendeItem.Show;
end;

procedure TForm1.LegendaInmetroproximoItem1Click(Sender: TObject);
begin
  ACBrECF1.LegendaInmetroProximoItem;
end;

procedure TForm1.CancelarItemVendido1Click(Sender: TObject);
Var
  Item: String;
begin
  Item := '1';
  if InputQuery('Cancelar Item Vendido', 'Informe o número da Sequencia de Venda', Item) then
  begin
    ACBrECF1.CancelaItemVendido(StrToIntDef(Item, 0));
    mResp.Lines.Add('Cancela Item Vendido: ' + Item);
    AtualizaMemos;
  end;
end;

procedure TForm1.CancelaItemVendidoParcial1Click(Sender: TObject);
Var
  Item: String;
  Qdte: String;
begin
  Item := '1';
  if InputQuery('Cancelar Item Vendido', 'Informe o número da Sequencia de Venda', Item) then

    if InputQuery('Quantidade do Item a cancelar', 'Informe quantidade do Item a cancelar', Qdte) then
    begin
      ACBrECF1.CancelaItemVendidoParcial(StrToIntDef(Item, 0), StrToFloatDef(Qdte, 0));
      mResp.Lines.Add('Cancela Item Vendido Parcial: ' + Item + ', Qdte: ' + Qdte);
      AtualizaMemos;
    end;
end;

procedure TForm1.CancelaDescontoAcrescimoItem1Click(Sender: TObject);
Var
  Item: String;
begin
  Item := '1';
  if InputQuery('Cancelar DescontoAcrescimo do Item Vendido', 'Informe o número da Sequencia de Venda', Item) then
  begin
    ACBrECF1.CancelaDescontoAcrescimoItem(StrToIntDef(Item, 0));
    mResp.Lines.Add('Cancelar AcrescimoDesconto: ' + Item);
    AtualizaMemos;
  end;

end;

procedure TForm1.Sub1Click(Sender: TObject);
Var
  Desc, Obs: String;
begin
  Desc := '0';
  Obs := '';

  if ACBrECF1.ModeloStr = 'DataRegis' then
    InputQuery('Subtotaliza Cupom', 'Se Necessário digite alguma Observaçao (até 8 linhas)' + #10 +
      'O sinal | (pipe) será convertido para #10 (quebra de linha)' + #10 + 'A Observação também pode ser enviada no metodo FechaCupom', Obs);

  if InputQuery('Subtotaliza Cupom', 'Digite Valor negativo para Desconto' + #10 + 'ou Valor Positivo para Acrescimo', Desc) then
  begin
    ACBrECF1.SubtotalizaCupom(StrToFloat(Desc), Obs);
    mResp.Lines.Add('Subtotaliza Cupom: ' + Desc);
    AtualizaMemos;
  end;
end;

procedure TForm1.SubModeloECF1Click(Sender: TObject);
begin
  mResp.Lines.Add('SubModeloECF: (' + ACBrECF1.SubModeloECF + ')');
  AtualizaMemos;
end;

procedure TForm1.CancelaDescontoAcrescimoSubTotal1Click(Sender: TObject);
Var
  Info: String;
  Tipo: Char;
begin
  Info := 'D';

  if InputQuery('Cancela DescontoAcrescimo SubTotal do Cupom', 'Digite "A" para cancelar Acrescimo ou "D" para Desconto', Info) then
  begin
    Tipo := Info[1];
    ACBrECF1.CancelaDescontoAcrescimoSubTotal(Tipo);
    mResp.Lines.Add('Cancela DescontoAcrescimo SubTotal do Cupom: ' + Tipo);
    AtualizaMemos;
  end;

end;

procedure TForm1.EfetuarPagamento1Click(Sender: TObject);
begin
  if not(ACBrECF1.Modelo in [ecfDataRegis, ecfFiscNET]) then
    if ACBrECF1.Estado <> estPagamento then
      MessageDlg('Impressora nao está em Estado de Pagamento' + #10 + 'Primeiro use SubTotaliza Cupom', mtWarning, [mbOk], 0);

  frPagamento.Show;
  frPagamento.TipoCupom := 'F';
end;

procedure TForm1.FecharCupom1Click(Sender: TObject);
Var
  Obs: String;
  IndiceBMP: String;
begin
  Obs := 'Componentes ACBr|http://acbr.sourceforge.net';
  IndiceBMP := '0';
  if InputQuery('Fechar Cupom', 'Se Necessário digite alguma Observaçao (até 8 linhas)' + #10 +
    'O sinal | (pipe) será convertido para #10 (quebra de linha)', Obs) then
  begin
    if (ACBrECF1.Modelo = ecfDaruma) and (ACBrECF1.MFD) then
      if Not InputQuery('Impressao de imagem BMP ', 'Digite o Indice do BMP que deseja utilizar', IndiceBMP) then
        exit;

    // informações que devem ir no rodapé do cupom obrigatoriamente
    // conforme a legislação do paf-ecf
    // preencha somente as informações que for utilizar, o que não foi informado
    // não será impresso
    ACBrECF1.InfoRodapeCupom.MD5 := '12345678901234567890123456789012';
    ACBrECF1.InfoRodapeCupom.Dav := '0000000001';
    ACBrECF1.InfoRodapeCupom.DavOs := '0000000002';
    ACBrECF1.InfoRodapeCupom.PreVenda := '0000000003';
    //     ACBrECF1.InfoRodapeCupom.CupomMania := cbxUF.Text = 'RJ';
    //     ACBrECF1.InfoRodapeCupom.MinasLegal := cbxUF.Text = 'MG';
    //     ACBrECF1.InfoRodapeCupom.ParaibaLegal := cbxUF.Text = 'PB';
    //     ACBrECF1.InfoRodapeCupom.NotaLegalDF.Imprimir := cbxUF.Text = 'DF';

    if ACBrECF1.InfoRodapeCupom.NotaLegalDF.Imprimir then
    begin
      ACBrECF1.InfoRodapeCupom.NotaLegalDF.ProgramaDeCredito := true;
      ACBrECF1.InfoRodapeCupom.NotaLegalDF.ValorICMS := 123456.99;
      ACBrECF1.InfoRodapeCupom.NotaLegalDF.ValorISS := 123456.88;
    end;

    // lei 12.741/2013 transparencia dos impostos
    // ACBrECF1.InfoRodapeCupom.Imposto.Texto := '...'; utilize essa propriedade se quiser personalizar o texto exemplo: Valor impostos %s (%s %) meu texto
    // ACBrECF1.InfoRodapeCupom.Imposto.ValorAproximado := 1.23;  // informar o valor aproximado calculado a partir dos itens
    // ACBrECF1.InfoRodapeCupom.Imposto.Fonte           := 'IBPT'; // informar a fonte de onde veio a informação para calculo

    // Novo formato da lei de transparência que divide a informação.
    ACBrECF1.InfoRodapeCupom.Imposto.ValorAproximadoFederal := 0.60; // informar o valor aproximado calculado a partir dos itens
    ACBrECF1.InfoRodapeCupom.Imposto.ValorAproximadoEstadual := 0.40; // informar o valor aproximado calculado a partir dos itens
    ACBrECF1.InfoRodapeCupom.Imposto.ValorAproximadoMunicipal := 0.23; // informar o valor aproximado calculado a partir dos itens
    ACBrECF1.InfoRodapeCupom.Imposto.Fonte := 'IBPT/FECOMERCIO (aWd7S8)';
    // informar a fonte de onde veio a informação para calculo e a Chave se vier da tabela do IBPT
    ACBrECF1.InfoRodapeCupom.Imposto.ModoCompacto := true; //Faz com que a impressão seja em apenas duas linhas;

    // ER 02.01 - Requisito XXVIII item 8
    ACBrECF1.InfoRodapeCupom.NF := '123456';

    Obs := StringReplace(Obs, '|', #10, [rfReplaceAll, rfIgnoreCase]);
    ACBrECF1.FechaCupom(Obs, StrToIntDef(IndiceBMP, 0));
    mResp.Lines.Add('Fecha Cupom: ' + #10 + Obs);
    AtualizaMemos;
  end;
end;

procedure TForm1.EnviaComando1Click(Sender: TObject);
Var
  CMD1, CMD2, C: String;
  A: Integer;
  T: String;
begin
  CMD1 := '';
  T := '0';
  if InputQuery('Enviar Comando', 'Digite o comando de acordo com a Sintaxe da Impressora' + #10 + #10 +
    'Para Caracteres ASC use #nnn  Ex: #006 = chr(006)' + #10 + #10 + 'Exemplo: #006 irá imprimir uma Leitura X na Bematech', CMD1) then
    if (not(ACBrECF1.Modelo = ecfBematech)) or InputQuery('Enviar Comando BEMATECH', 'Digite o tamanho em Bytes do Retorno esperado' + #10 + #10 +
      'NAO adcione os 3 bytes de ACK+ST1+ST2', T) then
    begin
      CMD2 := '';
      A := 1;

      if ACBrECF1.ECF is TACBrECFBematech then
        (ACBrECF1.ECF as TACBrECFBematech).BytesResp := StrToIntDef(T, 0);

      while A <= length(CMD1) do
      begin
        C := copy(CMD1, A, 1);

        if C = '#' then
        begin
          CMD2 := CMD2 + chr(StrToIntDef(copy(CMD1, A + 1, 3), 0));
          A := A + 3;
        end
        else
          CMD2 := CMD2 + C;

        A := A + 1;
      end;

      ACBrECF1.EnviaComando(CMD2);
      mResp.Lines.Add('Envia Comando: ' + CMD1);
      AtualizaMemos;
    end;
end;

procedure TForm1.FechaRelatrio1Click(Sender: TObject);
begin
  ACBrECF1.FechaRelatorio;
end;

procedure TForm1.ACBrAAC1AntesAbrirArquivo(var Continua: Boolean);
begin
  ACBrAAC1.NomeArquivoAux := ExtractFilePath(Application.ExeName) + edAACNomeArq.Text;
  ACBrAAC1.ArqLOG := ExtractFilePath(Application.ExeName) + edAACLog.Text;
  Continua := true;
end;

procedure TForm1.ACBrAAC1AntesGravarArquivo(var Continua: Boolean);
begin
  ACBrAAC1.NomeArquivoAux := ExtractFilePath(Application.ExeName) + edAACNomeArq.Text;
  ACBrAAC1.ArqLOG := ExtractFilePath(Application.ExeName) + edAACLog.Text;
  Continua := true;
end;

procedure TForm1.ACBrAAC1DepoisAbrirArquivo(Sender: TObject);
var
  I: Integer;
begin
  edAAC_SH_RazaoSocial.Text := ACBrAAC1.IdentPAF.Empresa.RazaoSocial;
  edAAC_SH_CNPJ.Text := ACBrAAC1.IdentPAF.Empresa.CNPJ;
  edAAC_SH_IM.Text := ACBrAAC1.IdentPAF.Empresa.IM;
  edAAC_SH_IE.Text := ACBrAAC1.IdentPAF.Empresa.IE;
  edAAC_PAF_Aplicativo.Text := ACBrAAC1.IdentPAF.Paf.Nome;
  edAAC_PAF_Versao.Text := ACBrAAC1.IdentPAF.Paf.Versao;
  edAAC_PAF_MD5.Text := ACBrAAC1.IdentPAF.ArquivoListaAutenticados.MD5;

  with mdsAACECF do
  begin
    Open;
    // Zera Tabela em memoria //
    First;
    while not EOF do
      Delete;

    // Insere Itens da Lista de ECFS //
    For I := 0 to ACBrAAC1.IdentPAF.ECFsAutorizados.Count - 1 do
    begin
      Insert;
      FieldByName('Indice').AsInteger := I;
      FieldByName('NumSerie').AsString := ACBrAAC1.IdentPAF.ECFsAutorizados[I].NumeroSerie;
      FieldByName('CRO').AsInteger := ACBrAAC1.IdentPAF.ECFsAutorizados[I].CRO;
      FieldByName('ValorGT').AsFloat := ACBrAAC1.IdentPAF.ECFsAutorizados[I].ValorGT;
      FieldByName('DtHrAtualizado').AsDateTime := ACBrAAC1.IdentPAF.ECFsAutorizados[I].DtHrAtualizado;
      Post;
    end;
  end;

  mAACParams.Lines.Clear;
  mAACParams.Lines.Assign(ACBrAAC1.Params);
end;

procedure TForm1.ACBrAAC1GetChave(var Chave: AnsiString);
begin
  Chave := 'Informe aqui a SUA CHAVE';
  // Dicas: Evite Strings fixas.. prefira uma Constante
  // Use Chr(nn) ou outra função para compor a chave
end;

procedure TForm1.ACBrECF1AguardandoRespostaChange(Sender: TObject);
begin
  if ACBrECF1.AguardandoResposta then
    StatusBar1.Panels[0].Text := 'Processando...'
  else
    StatusBar1.Panels[0].Text := '';
end;

procedure TForm1.HorarioVerao2Click(Sender: TObject);
begin
  mResp.Lines.Add('Horário de Verão: ' + IfThen(ACBrECF1.HorarioVerao, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.Arredonda1Click(Sender: TObject);
begin
  mResp.Lines.Add('Arredondamento: ' + IfThen(ACBrECF1.Arredonda, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.MudaArredondamento1Click(Sender: TObject);
Var
  Resp: TModalResult;
begin
  Resp := MessageDlg('Arredondar ?', mtConfirmation, mbYesNoCancel, 0);
  if Resp <> mrCancel then
  begin
    ACBrECF1.MudaArredondamento((Resp = mrYes));
    mResp.Lines.Add('MudaArredondamento');
    AtualizaMemos;
  end;
end;

procedure TForm1.NumLoja1Click(Sender: TObject);
begin
  mResp.Lines.Add('NUM Loja: (' + ACBrECF1.NumLoja + ')');
  AtualizaMemos;
end;

procedure TForm1.NumNCN1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.NCN: (' + ACBrECF1.NumNCN + ')');
  AtualizaMemos;
end;

procedure TForm1.NumCRO1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.CRO: (' + ACBrECF1.NumCRO + ')');
  AtualizaMemos;
end;

procedure TForm1.TestedeVelocidade1Click(Sender: TObject);
Var
  cItens, cCupons: String;
  nItens, nCupons, I, J: Integer;
  tIni, tFim: TDateTime;
  Resp: TModalResult;
  SubTot, Desc: Double;
begin
  ACBrECF1.CarregaFormasPagamento;
  if ACBrECF1.FormasPagamento.Count < 1 then
    raise Exception.Create('Nenhuma Forma de Pagamento programada no ECF');

  cItens := '10';
  if not InputQuery('Teste de Velocidade', 'Numero de Itens a imprimir:', cItens) then
    exit;

  cCupons := '1';
  if not InputQuery('Teste de Velocidade', 'Numero de Cupons a imprimir:', cCupons) then
    exit;

  Resp := MessageDlg('Monitorar estado do ECF ?', mtConfirmation, mbYesNoCancel, 0);
  if Resp = mrCancel then
    exit;

  nItens := StrToIntDef(cItens, 0);
  if nItens < 1 then
    exit;
  nCupons := StrToIntDef(cCupons, 0);
  if nCupons < 1 then
    exit;

  wbBobina.Navigate('about:blank');
  Form1.Enabled := False;
  try
    For J := 1 to nCupons do
    begin
      tIni := now;
      mResp.Lines.Add('Imprimindo ' + cItens + ' itens.');
      mResp.Lines.Add('Iniciando Cupom: ' + DateTimeToStr(tIni));
      ACBrECF1.AbreCupom();
      mResp.Lines.Add('Cupom Aberto: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');
      if Resp = mrYes then
        mResp.Lines.Add('Estado ECF: ' + EstadoECF);

      For I := 1 to nItens do
      begin
        if I = 1 then
          if Resp = mrYes then
            mResp.Lines.Add('Estado ECF: ' + EstadoECF);

        ACBrECF1.VendeItem(IntToStrZero(I, 3), 'DESCRICAO PRODUTO: ' + IntToStrZero(I, 3), 'NN', 1, I / 100, 0, 'UN');
        { Aguarda 1 segundo ou até o ECF ficar Em linha novamente }
        ACBrECF1.EmLinha(1);
        mResp.Lines.Add('Item ' + IntToStr(I) + ': ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');
        { Semelhante ao "AguardaImpressao := True", porém é mais rápido, pois no
          método "VerificaFimImpressao" alem de verificado o "EmLinha" também é
         solicitado o Status do ECF }
      end;

      SubTot := ACBrECF1.Subtotal;
      Desc := 0;
      if SubTot >= 1 then
        Desc := Frac(SubTot) * -1;
      ACBrECF1.SubtotalizaCupom(Desc);
      mResp.Lines.Add('SubTotalizado: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');

      if Resp = mrYes then
        mResp.Lines.Add('Estado ECF: ' + EstadoECF);

      //   ACBrECF1.AbreGaveta ;

      { Executando todos os Pagamentos disponiveis }
      (*   Parcela := max(  RoundTo(ACBrECF1.Subtotal/ACBrECF1.FormasPagamento.Count,-2),
        0.01) ;
       For i := 1 to ACBrECF1.FormasPagamento.Count - 1 do
       Try
       ACBrECF1.EfetuaPagamento(ACBrECF1.FormasPagamento[i].Indice,  Parcela,
       'OBSERVACAO PAGAMENTO: '+ IntToStrZero(i+1,2) );
       except
       Break ;
       end ;
      *)
      { Efetuando ultimo pagamento no Item 0, deve zerar o Saldo a pagar }
      ACBrECF1.EfetuaPagamento(ACBrECF1.FormasPagamento[0].Indice, (ACBrECF1.Subtotal - ACBrECF1.TotalPago), 'ZERANDO SALDO A PAGAR RESTANTE');
      mResp.Lines.Add('Pagamento Efetuado: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');
      if Resp = mrYes then
        mResp.Lines.Add('Estado ECF: ' + EstadoECF);

      ACBrECF1.FechaCupom('TESTE DE CUPOM');
      tFim := now;
      mResp.Lines.Add('Finalizado em: ' + DateTimeToStr(tFim));
      mResp.Lines.Add('Diferença: ' + FormatFloat('###.##', SecondSpan(tIni, tFim)) + ' segundos');
      mResp.Lines.Add('---------------------------------');
      AtualizaMemos;
    end;
  finally
    Form1.Enabled := true;
  end;

end;

procedure TForm1.CapturaporNReduaoZ1Click(Sender: TObject);
Var
  Linhas: TStringList;
  cRedIni, cRedFim: String;
  I, nRedIni, nRedFim: Integer;
begin
  cRedIni := '0';
  cRedFim := '0';

  if not InputQuery('Captura da Memoria Fiscal', 'Entre com o a Reduçao Z Inicial:', cRedIni) then
    exit;
  nRedIni := StrToIntDef(cRedIni, -1);
  if nRedIni < 0 then
    exit;

  if not InputQuery('Captura da Memoria Fiscal', 'Entre com o a Reduçao Z Final:', cRedFim) then
    exit;
  nRedFim := StrToIntDef(cRedFim, -1);
  if nRedFim < 0 then
    exit;

  Linhas := TStringList.Create;
  try
    ACBrECF1.LeituraMemoriaFiscalSerial(nRedIni, nRedFim, Linhas);

    For I := 0 to Linhas.Count - 1 do
      mResp.Lines.Add(Linhas[I]);
  finally
    Linhas.Free;
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.ImprimeporNReduaoZ1Click(Sender: TObject);
Var
  cRedIni, cRedFim: String;
  nRedIni, nRedFim: Integer;
begin
  cRedIni := '0';
  cRedFim := '0';

  if not InputQuery('Impressão da Memoria Fiscal', 'Entre com o a Reduçao Z Inicial:', cRedIni) then
    exit;
  nRedIni := StrToIntDef(cRedIni, -1);
  if nRedIni < 0 then
    exit;

  if not InputQuery('Impressão da Memoria Fiscal', 'Entre com o a Reduçao Z Final:', cRedFim) then
    exit;
  nRedFim := StrToIntDef(cRedFim, -1);
  if nRedFim < 0 then
    exit;

  ACBrECF1.LeituraMemoriaFiscal(nRedIni, nRedFim);
  mResp.Lines.Add('Leitura da Memoria Fiscal por Reduçao');
end;

procedure TForm1.CapturaporPeriodo1Click(Sender: TObject);
Var
  Linhas: TStringList;
  cDatIni, cDatFim: String;
  dDatIni, dDatFim: TDateTime;
  I: Integer;
begin
  cDatIni := '01/' + FormatDateTime('mm/yy', now);
  cDatFim := FormatDateTime('dd/mm/yy', now);

  if not InputQuery('Captura da Memoria Fiscal', 'Entre com o a Data Inicial (DD/MM/AA):', cDatIni) then
    exit;
  try
    dDatIni := StrToDateTime(StringReplace(cDatIni, '/', DateSeparator, [rfReplaceAll]));
  except
    exit;
  end;

  if not InputQuery('Captura da Memoria Fiscal', 'Entre com o a Data Final (DD/MM/AA):', cDatFim) then
    exit;
  try
    dDatFim := StrToDateTime(StringReplace(cDatFim, '/', DateSeparator, [rfReplaceAll]));
  except
    exit
  end;

  Linhas := TStringList.Create;
  try
    ACBrECF1.LeituraMemoriaFiscalSerial(dDatIni, dDatFim, Linhas);

    For I := 0 to Linhas.Count - 1 do
      mResp.Lines.Add(Linhas[I]);
  finally
    Linhas.Free;
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.ImprimeporPeriodo1Click(Sender: TObject);
Var
  cDatIni, cDatFim: String;
  dDatIni, dDatFim: TDateTime;
begin
  cDatIni := '01/' + FormatDateTime('mm/yy', now);
  cDatFim := FormatDateTime('dd/mm/yy', now);

  if not InputQuery('Captura da Memoria Fiscal', 'Entre com o a Data Inicial (DD/MM/AA):', cDatIni) then
    exit;
  try
    dDatIni := StrToDateTime(StringReplace(cDatIni, '/', DateSeparator, [rfReplaceAll]));
  except
    exit;
  end;

  if not InputQuery('Captura da Memoria Fiscal', 'Entre com o a Data Final (DD/MM/AA):', cDatFim) then
    exit;
  try
    dDatFim := StrToDateTime(StringReplace(cDatFim, '/', DateSeparator, [rfReplaceAll]));
  except
    exit
  end;

  ACBrECF1.LeituraMemoriaFiscal(dDatIni, dDatFim);
  mResp.Lines.Add('Leitura da Memoria Fiscal por Datas');
end;

procedure TForm1.ImprimirExtratoCancelamento1Click(Sender: TObject);
begin
  ACBrSAT1.ImprimirExtratoCancelamento;
end;

procedure TForm1.ImprimirExtratoResumido1Click(Sender: TObject);
begin
  ACBrSAT1.ImprimirExtratoResumido;
end;

procedure TForm1.ImprimirExtratoVenda1Click(Sender: TObject);
begin
  ACBrSAT1.ImprimirExtrato;
end;

procedure TForm1.ipoltimoDocumento1Click(Sender: TObject);
begin
  mResp.Lines.Add('TipoUltimoDocumento: (' + GetEnumName(TypeInfo(TACBrECFTipoDocumento), Integer(ACBrECF1.TipoUltimoDocumento)) + ')');
  AtualizaMemos;
end;

procedure TForm1.PrepararImpressaoNFCe;
begin
  if ACBrNFe1.DANFE = ACBrNFeDANFeESCPOS1 then
    ACBrPosPrinter1.Porta := edtPorta.Text
  else
    ACBrNFeDANFCeFortes1.MostraPreview := cbPreview.Checked;
end;

procedure TForm1.PrepararImpressaoSAT;
begin
  ACBrPosPrinter1.Device.Porta := edtPorta.Text;
  ACBrSATExtratoESCPOS1.ImprimeQRCode := true;

  ACBrSATExtratoFortes1.LarguraBobina := seLargura.Value;
  ACBrSATExtratoFortes1.MargemSuperior := seMargemTopo.Value;
  ACBrSATExtratoFortes1.MargemInferior := seMargemFundo.Value;
  ACBrSATExtratoFortes1.MargemEsquerda := seMargemEsquerda.Value;
  ACBrSATExtratoFortes1.MargemDireita := seMargemDireita.Value;
  ACBrSATExtratoFortes1.MostraPreview := cbPreview.Checked;

  try
    if lImpressora.Caption <> '' then
      ACBrSATExtratoFortes1.Impressora := lImpressora.Caption;
  except
  end;
end;

procedure TForm1.ProgramaAliquota1Click(Sender: TObject);
Var
  cAliq: String;
  nAliq: Double;
  Tipo: Char;
  Resp: TModalResult;
begin
  cAliq := '18,00';

  if not InputQuery('Programaçao de Aliquotas', 'Entre com o valor da Aliquota:', cAliq) then
    exit;

  cAliq := StringReplace(StringReplace(cAliq, '.', DecimalSeparator, []), ',', DecimalSeparator, []);
  nAliq := StrToFloatDef(cAliq, 0);
  if nAliq = 0 then
    exit;

  Resp := MessageDlg('Aliquota do ICMS ?' + sLineBreak + 'SIM = ICMS, NAO = ISS', mtConfirmation, mbYesNoCancel, 0);
  case Resp of
    mrCancel:
      exit;
    mrYes:
      Tipo := 'T';
  else
    ;
    Tipo := 'S';
  end;

  if MessageDlg('A aliquota: [' + FloatToStr(nAliq) + '] do Tipo: [' + Tipo + '] será programada.' + sLineBreak + sLineBreak +
    'Cuidado !! A programação de Aliquotas é irreversivel' + sLineBreak + 'Confirma a operação ?', mtConfirmation, mbYesNoCancel, 0) <> mrYes then
    exit;

  ACBrECF1.ProgramaAliquota(nAliq, Tipo);
  AliquotasICMS1Click(Sender);
end;

procedure TForm1.ProgramaFormadePagamento1Click(Sender: TObject);
Var
  cDescricao: String;
  Vinculado: Boolean;
  Resp: TModalResult;
begin
  cDescricao := 'CARTAO';
  Vinculado := true;

  if not InputQuery('Programaçao de Formas de Pagamento (FPG)', 'Entre com a Descriçao:', cDescricao) then
    exit;

  if not(ACBrECF1.Modelo in [ecfBematech, ecfNaoFiscal, ecfMecaf]) then
  begin
    Resp := MessageDlg('Permite Vinculado nessa Forma de Pagamento ?', mtConfirmation, mbYesNoCancel, 0);
    if Resp = mrCancel then
      exit
    else
      Vinculado := (Resp = mrYes);
  end;

  if MessageDlg('A Forma de Pagamento: [' + cDescricao + '] ' + 'será programada.' + sLineBreak + sLineBreak +
    'Cuidado !! A programação de Formas de Pagamento é irreversivel' + sLineBreak + 'Confirma a operação ?', mtConfirmation, mbYesNoCancel, 0) <> mrYes
  then
    exit;

  ACBrECF1.ProgramaFormaPagamento(cDescricao, Vinculado);
  FormasdePagamento1Click(Sender);
end;

procedure TForm1.ProgramaIdentificaoPafECF1Click(Sender: TObject);
Var
  PAFStr, MD5, ProgramaVersao: String;
  P: Integer;
begin
  PAFStr := ACBrECF1.Paf;
  P := pos('|', PAFStr);
  if P > 0 then
  begin
    MD5 := copy(PAFStr, 1, P - 1);
    ProgramaVersao := copy(PAFStr, P + 1, length(PAFStr));
  end
  else
  begin
    MD5 := copy(PAFStr, 1, 42);
    ProgramaVersao := copy(PAFStr, 43, 42);
  end;

  if not InputQuery('Identifica PAF (Programa Aplicativo Fiscal)', 'Programa e Versao:', ProgramaVersao) then
    exit;

  if not InputQuery('Identifica PAF (Programa Aplicativo Fiscal)', 'MD5:', MD5) then
    exit;

  if MD5 + ProgramaVersao <> '' then
  begin
    ACBrECF1.IdentificaPAF(ProgramaVersao, MD5);
  end;
end;

procedure TForm1.ProgramaComprovanteNAOFiscal1Click(Sender: TObject);
Var
  cDescricao, cTipo, cPosicao: String;
begin
  cDescricao := 'CARTAO';
  cTipo := '';
  cPosicao := '';

  if not InputQuery('Programaçao de Comprovantes NAO Fiscais (CNF)', 'Entre com a posição:', cPosicao) then
    exit;

  if not InputQuery('Programaçao de Comprovantes NAO Fiscais (CNF)', 'Entre com a Descriçao:', cDescricao) then
    exit;

  case ACBrECF1.Modelo of
    ecfSchalter:
      if not InputQuery('Comprovantes NAO Fiscal ' + ACBrECF1.ModeloStr, 'Entre com a String do parametro "Tipo".' + sLineBreak +
        'D - Permite Desconto e Item ' + sLineBreak + 'A - Permite Acrescimo no Subtotal' + sLineBreak + 'C - Permite Cancelamento de Item' +
        sLineBreak + 'P - Obriga forma de Pagamento' + sLineBreak + sLineBreak + 'Vnn - Obriga emissao de vinculado na Forma de ' + 'Pagamento nn' +
        sLineBreak + sLineBreak + 'Se vazio assume Default = "DAC"' + sLineBreak + 'Exemplos:  V04 -> Torna a Emissao do Cupom Fiscal ' +
        'Vinculado obrigatória para a Forma de Pagamento 04', cTipo) then
        exit;

    ecfDaruma:
      if not InputQuery('Comprovantes NAO Fiscal ' + ACBrECF1.ModeloStr, 'Entre com a String do parametro "Tipo".' + sLineBreak +
        'V  Comprovante Vinculado' + sLineBreak + '+  Entrada de Recursos' + sLineBreak + '-  Saida de Recursos' + sLineBreak + sLineBreak +
        'Se vazio assume Default = "V"' + sLineBreak + 'Informe Apenas uma das Opçoes', cTipo) then
        exit;

    ecfSweda, ecfSwedaSTX, ecfQuattro:
      if not InputQuery('Comprovantes NAO Fiscal ' + ACBrECF1.ModeloStr, 'Entre com a String do parametro "Tipo".' + sLineBreak +
        '&  Criaçao de um novo Grupo (Titulo)' + sLineBreak + '+  Entrada de Recursos' + sLineBreak + '-  Saida de Recursos' + sLineBreak + sLineBreak
        + 'Se vazio assume Default = "+"' + sLineBreak + 'Informe Apenas uma das Opçoes', cTipo) then
        exit;

    ecfFiscNET, ecfICash:
      if not InputQuery('Comprovantes NAO Fiscal ' + ACBrECF1.ModeloStr, 'Entre com a String do parametro "Tipo".' + sLineBreak +
        '+  Entrada de Recursos' + sLineBreak + '-  Saida de Recursos' + sLineBreak + sLineBreak + 'Se vazio assume Default = "+"' + sLineBreak +
        'Informe Apenas uma das Opçoes', cTipo) then
        exit;

  end;

  if MessageDlg('O Comprovante Nao Fiscal: [' + cDescricao + '] ' + IfThen(ACBrECF1.Modelo in [ecfDaruma, ecfSchalter, ecfSweda, ecfQuattro,
    ecfFiscNET], ' do Tipo: [' + cTipo + '] ', '') + 'será programado.' + sLineBreak + sLineBreak + 'Cuidado !! A programação de CNFs é irreversivel'
    + sLineBreak + 'Confirma a operação ?', mtConfirmation, mbYesNoCancel, 0) <> mrYes then
    exit;

  ACBrECF1.ProgramaComprovanteNaoFiscal(cDescricao, cTipo, cPosicao);
  CarregaComprovantesNAOFiscais1Click(Sender);
end;

procedure TForm1.ProgramaRelatrioGerencial1Click(Sender: TObject);
var
  Descricao: String;
begin
  if not InputQuery('Programaçao de Relatórios Gerenciais', 'Entre com a Descrição do Relatório Gerencial:', Descricao) then
    exit;
  if MessageDlg('O Relatório: [' + Descricao + '] será programado.' + sLineBreak + sLineBreak +
    'Cuidado a programação de Relatórios Gerenciais é irreversivel' + sLineBreak + 'Confirma a operação ?', mtConfirmation, mbYesNoCancel, 0) <> mrYes
  then
    exit;
  ACBrECF1.ProgramaRelatoriosGerenciais(Descricao);
  CarregaRelatriosGerenciais1Click(Sender)
end;

procedure TForm1.ACBrECF1MsgPoucoPapel(Sender: TObject);
begin
  mResp.Lines.Add('ATENÇÃO... POUCO PAPEL');
end;

procedure TForm1.CorrigeEstadodeErro1Click(Sender: TObject);
begin
  ACBrECF1.CorrigeEstadoErro;
end;

procedure TForm1.ChequePronto1Click(Sender: TObject);
begin
  mResp.Lines.Add('Cheque Pronto: ' + IfThen(ACBrECF1.ChequePronto, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.ImprimeCheque1Click(Sender: TObject);
Var
  sValor: String;
  dValor: Double;
  sBanco, sFavorecido, sCidade: String;
begin
  sValor := '10,00';
  sBanco := '001';
  sFavorecido := 'Projeto ACBr';
  sCidade := 'Sao Paulo';

  if not InputQuery('Impressão de Cheque', 'Entre com o valor do Cheque:', sValor) then
    exit;

  sValor := StringReplace(StringReplace(sValor, '.', DecimalSeparator, []), ',', DecimalSeparator, []);
  dValor := StrToFloatDef(sValor, 0);
  if dValor = 0 then
    exit;

  if not InputQuery('Impressão de Cheque', 'Entre com o Numero do Banco', sBanco) then
    exit;

  if not InputQuery('Impressão de Cheque', 'Entre com o Favorecido', sFavorecido) then
    exit;

  if not InputQuery('Impressão de Cheque', 'Entre com a Cidade', sCidade) then
    exit;

  while not ACBrECF1.ChequePronto do
    if (MessageDlg('Favor inserir o cheque e pressionar OK', mtConfirmation, [mbOk, mbCancel], 0) = mrCancel) then
      exit;

  ACBrECF1.ImprimeCheque(sBanco, dValor, sFavorecido, sCidade, now, 'TESTE DE IMPRESSAO DE CHEQUE');

  mResp.Lines.Add('ImprimeCheque Banco:' + sBanco + ' Valor:' + sValor + ' Favorecido:' + sFavorecido + ' Cidade:' + sCidade + ' Data:' +
    FormatDateTime('dd/mm/yy', now));
  AtualizaMemos;
end;

procedure TForm1.CancelaImpressoCheque1Click(Sender: TObject);
begin
  ACBrECF1.CancelaImpressaoCheque;
  mResp.Lines.Add('CancelaImpressaoCheque');
  AtualizaMemos;
end;

procedure TForm1.CarregaUnidadesdeMedida1Click(Sender: TObject);
var
  A: Integer;
begin
  ACBrECF1.CarregaUnidadesMedida;

  for A := 0 to ACBrECF1.UnidadesMedida.Count - 1 do
  begin
    if ACBrECF1.UnidadesMedida[A].Descricao <> '' then
      mResp.Lines.Add('Unid Medida: ' + ACBrECF1.UnidadesMedida[A].Indice + ' -> ' + ACBrECF1.UnidadesMedida[A].Descricao);
  end;
  mResp.Lines.Add('---------------------------------');

end;

procedure TForm1.ProgramaUnidadeMedida1Click(Sender: TObject);
var
  um: String;
begin
  if not InputQuery('Programaçao de Unidades de Medida', 'Entre com a Descrição da Unidade de Medida:', um) then
    exit;
  if MessageDlg('A Unidade de Medida: [' + um + '] será programada.' + sLineBreak + sLineBreak +
    'Cuidado a programação de Unidades de Medida é irreversivel' + sLineBreak + 'Confirma a operação ?', mtConfirmation, mbYesNoCancel, 0) <> mrYes
  then
    exit;
  ACBrECF1.ProgramaUnidadeMedida(um);
end;

procedure TForm1.AbreRelatorioGerencial1Click(Sender: TObject);
Var
  IndiceStr: String;
begin
  IndiceStr := '1';
  if not InputQuery('Abertura de Relatório Gerencial', 'Digite o Indice do Relatório Gerencial a ser utilizado', IndiceStr) then
    exit;
  ACBrECF1.AbreRelatorioGerencial(StrToIntDef(IndiceStr, 0));
end;

procedure TForm1.AbreBilhetePassagem1Click(Sender: TObject);
begin
  wbBobina.Navigate('about:blank');
  mBobina.Clear;

  ACBrECF1.AbreBilhetePassagem(
    'Origem',
    'Destino',
    'Linha',
    'Agencia',
    NOW,
    '32',
    'A1',
    tbRodInterest,
    'MG',
    '',
    '',
    ''
  );
  mResp.Lines.Add('AbreCupom');
  AtualizaMemos;
end;

procedure TForm1.AbreCupomVinculado1Click(Sender: TObject);
Var
  COO, CodFormaPagamento, CodComprovanteNaoFiscal: String;
  sValor: String;
  dValor: Double;
begin
  COO := ACBrECF1.NumCupom;
  CodFormaPagamento := '01';
  CodComprovanteNaoFiscal := ' ';
  sValor := '0';

  if not InputQuery('Abertura de Cupom Vinculado', 'Digite o Cod.Forma Pagamento utilizada no cupom anterior', CodFormaPagamento) then
    exit;

  if not InputQuery('Abertura de Cupom Vinculado', 'Digite o Cod.Comprovante Não Fiscal' + sLineBreak + '(Não é necessário na maioria dos modelos)',
    CodComprovanteNaoFiscal) then
    exit;

  if not InputQuery('Abertura de Cupom Vinculado', 'Digite o Valor a vincular no cupom anterior' + sLineBreak +
    '(Não é necessário em alguns modelos)', sValor) then
    exit;

  sValor := StringReplace(StringReplace(sValor, '.', DecimalSeparator, []), ',', DecimalSeparator, []);
  dValor := StrToFloatDef(sValor, 0);
  if dValor = 0 then
    exit;

  if Trim(CodComprovanteNaoFiscal) <> '' then
    ACBrECF1.AbreCupomVinculado(COO, CodFormaPagamento, CodComprovanteNaoFiscal, dValor)
  else
    ACBrECF1.AbreCupomVinculado(COO, CodFormaPagamento, dValor);
end;

procedure TForm1.ImprimeLinhaRelatorio1Click(Sender: TObject);
var
  Linha: String;
begin
  if not InputQuery('Inpressão de Linha NÃO Fiscal', 'Digite a linha a imprimir', Linha) then
    exit;
  ACBrECF1.LinhaRelatorioGerencial(Linha, 1);
end;

procedure TForm1.ImprimeLinhaVinculado1Click(Sender: TObject);
var
  Linha: String;
begin
  if not InputQuery('Digite a linha a imprimir', '', Linha) then
    exit;
  ACBrECF1.LinhaCupomVinculado(Linha);
end;

procedure TForm1.ListaRelatorioGerencial1Click(Sender: TObject);
begin
  frRelatorio.TipoRelatorio := 'G';
  frRelatorio.ShowModal;
end;

procedure TForm1.mConsultarSATClick(Sender: TObject);
begin
  ACBrSAT1.ConsultarSAT;
end;

procedure TForm1.mConsultarSessaoSATClick(Sender: TObject);
Var
  strSessao: String;
  nSessao: Integer;
begin
  strSessao := '';
  if not InputQuery('Consultar Número de Sessão', 'Entre com o Número de Sessão a ser consultada:', strSessao) then
    exit;

  nSessao := StrToIntDef(strSessao, 0);
  if nSessao <= 0 then
    raise Exception.Create('Numero de sessão informado é inválido');

  ACBrSAT1.ConsultarNumeroSessao(nSessao);
end;

procedure TForm1.mConsultarStatusSATClick(Sender: TObject);
begin
  ACBrSAT1.ConsultarStatusOperacional;
end;

procedure TForm1.RelatorioGerencialcomformatacao1Click(Sender: TObject);
begin
  frmGerencialFormatado := TfrmGerencialFormatado.Create(Self);
  try
    frmGerencialFormatado.ShowModal;
  finally
    FreeAndNil(frmGerencialFormatado);
  end;
end;

procedure TForm1.ListaCupomVinculado1Click(Sender: TObject);
begin
  MessageDlg('Para imprimir um Cupom Vinculado você deve ter ' + 'informaçoes dos Pagamentos Efetuados no último Cupom Fiscal', mtInformation,
    [mbOk], 0);
  frRelatorio.TipoRelatorio := 'V';
  frRelatorio.ShowModal;
end;

procedure TForm1.PularLinhas1Click(Sender: TObject);
Var
  Linhas: String;
begin
  Linhas := IntToStr(ACBrECF1.LinhasEntreCupons);
  if not InputQuery('Pular Linhas', 'Digite o Numero de Linhas a Pular', Linhas) then
    exit;

  ACBrECF1.PulaLinhas(StrToIntDef(Linhas, 0));
end;

procedure TForm1.chGavetaSinalInvertidoClick(Sender: TObject);
begin
  ACBrECF1.GavetaSinalInvertido := chGavetaSinalInvertido.Checked;
end;

procedure TForm1.LerTodasasVariveis1Click(Sender: TObject);
begin
  DataHora1.Click;
  NumECF1.Click;
  NumLoja1.Click;
  NSrie1.Click;
  NVerso1.Click;
  NumCRO1.Click;
  NUltimoCupom1.Click;
  SubTotal1.Click;
  TotalPago1.Click;

  PoucoPapel1.Click;
  HorarioVerao2.Click;
  Arredonda1.Click;

  AliquotasICMS1.Click;
  FormasdePagamento1.Click;
  CarregaComprovantesNAOFiscais1.Click;
  CarregaUnidadesdeMedida1.Click;
end;

procedure TForm1.MFAdicional1Click(Sender: TObject);
begin
  mResp.Lines.Add('MF Adicional: ' + ACBrECF1.MFAdicional);
  AtualizaMemos;
end;

procedure TForm1.MFD1Click(Sender: TObject);
begin
  mResp.Lines.Add('É MFD: ' + IfThen(ACBrECF1.MFD, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.Termica1Click(Sender: TObject);
begin
  mResp.Lines.Add('É Termica: ' + IfThen(ACBrECF1.Termica, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.edLogChange(Sender: TObject);
begin
  ACBrECF1.ArqLOG := edLog.Text;
end;

procedure TForm1.SbAACArqLogClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(Application.ExeName) + edAACLog.Text);
end;

procedure TForm1.SbAACMD5AtualizarClick(Sender: TObject);
begin
  ACBrAAC1.AtualizarMD5(edAAC_PAF_MD5.Text);
end;

procedure TForm1.SbAACNomeArqClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(Application.ExeName) + edAACNomeArq.Text);
end;

procedure TForm1.SbArqLogClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(Application.ExeName) + edLog.Text);
end;

procedure TForm1.cbMemoHTMLClick(Sender: TObject);
begin
  if cbMemoHTML.Checked then
  begin
    ACBrECF1.MemoParams.Values['HTML'] := '1';
    wbBobina.BringToFront;
  end
  else
  begin
    ACBrECF1.MemoParams.Values['HTML'] := '0';
    wbBobina.SendToBack;
  end;

  mBobina.Visible := not cbMemoHTML.Checked;
  ACBrECF1.MemoLeParams;
end;

procedure TForm1.bBobinaLimparClick(Sender: TObject);
begin
  wbBobina.Navigate('about:blank');
  mBobina.Clear;
  if bBobinaParams.Caption = 'Salvar' then
  begin
    cbMemoHTMLClick(Sender);
    bBobinaParams.Caption := 'Parametros';
  end;
end;

procedure TForm1.bBobinaParamsClick(Sender: TObject);
begin
  if bBobinaParams.Caption = 'Parametros' then
  begin
    mBobina.Text := ACBrECF1.MemoParams.Text;
    mBobina.Visible := true;
    wbBobina.SendToBack;
    bBobinaParams.Caption := 'Salvar';
  end
  else
  begin
    ACBrECF1.MemoParams.Text := mBobina.Text;
    ACBrECF1.MemoParams.SaveToFile('ACBrECFMemoParams.ini');
    cbMemoHTMLClick(Sender);
    bBobinaParams.Caption := 'Parametros';
    bBobinaLimpar.Click;
  end;
end;

procedure TForm1.ACBrECF1BobinaAdicionaLinhas(const Linhas, Operacao: String);
begin
  if bBobinaParams.Caption = 'Salvar' then
  begin
    mBobina.Clear;
    cbMemoHTMLClick(nil);
    bBobinaParams.Caption := 'Parametros';
  end;

  WB_LoadHTML(wbBobina, mBobina.Text);
  Application.ProcessMessages;

  WB_ScrollToBottom(wbBobina);
end;

procedure TForm1.ACBrECF1ChangeEstado(const EstadoAnterior, EstadoAtual: TACBrECFEstado);
var
  sEstAnterior, sEstAtual: String;
begin
  case EstadoAnterior of
    estNaoInicializada:
      sEstAnterior := 'estNaoInicializada';
    estDesconhecido:
      sEstAnterior := 'estDesconhecido';
    estLivre:
      sEstAnterior := 'estLivre';
    estVenda:
      sEstAnterior := 'estVenda';
    estPagamento:
      sEstAnterior := 'estPagamento';
    estRelatorio:
      sEstAnterior := 'estRelatorio';
    estBloqueada:
      sEstAnterior := 'estBloqueada';
    estRequerZ:
      sEstAnterior := 'estRequerZ';
    estRequerX:
      sEstAnterior := 'estRequerX';
    estNaoFiscal:
      sEstAnterior := 'estNaoFiscal';
  end;

  case EstadoAtual of
    estNaoInicializada:
      sEstAtual := 'estNaoInicializada';
    estDesconhecido:
      sEstAtual := 'estDesconhecido';
    estLivre:
      sEstAtual := 'estLivre';
    estVenda:
      sEstAtual := 'estVenda';
    estPagamento:
      sEstAtual := 'estPagamento';
    estRelatorio:
      sEstAtual := 'estRelatorio';
    estBloqueada:
      sEstAtual := 'estBloqueada';
    estRequerZ:
      sEstAtual := 'estRequerZ';
    estRequerX:
      sEstAtual := 'estRequerX';
    estNaoFiscal:
      sEstAtual := 'estNaoFiscal';
  end;

  StatusBar1.Panels[1].Text := Format('Anterior: %s - Atual: %s', [sEstAnterior, sEstAtual]);
end;

procedure TForm1.WB_LoadHTML(WebBrowser: TWebBrowser; HTMLCode: string);
var
  sl: TStringList;
  ms: TMemoryStream;
begin
  WebBrowser.Navigate('about:blank');
  while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do
    Application.ProcessMessages;

  if Assigned(WebBrowser.Document) then
  begin
    sl := TStringList.Create;
    try
      ms := TMemoryStream.Create;
      try
        sl.Text := HTMLCode;
        sl.SaveToStream(ms);
        ms.Seek(0, 0);
        (WebBrowser.Document as IPersistStreamInit).Load(TStreamAdapter.Create(ms));
      finally
        ms.Free;
      end;
    finally
      sl.Free;
    end;
  end;
end;

procedure TForm1.WB_ScrollToTop(WebBrowser1: TWebBrowser);
var
  scrollpos: Integer;
  pw: IHTMLWindow2;
  Doc: IHTMLDocument2;
begin
  Doc := WebBrowser1.Document as IHTMLDocument2;
  pw := IHTMLWindow2(Doc.parentWindow);
  scrollpos := pw.screen.height;
  pw.scrollBy(0, -scrollpos);
end;

procedure TForm1.WB_ScrollToBottom(WebBrowser1: TWebBrowser);
var
  scrollpos: Integer;
  pw: IHTMLWindow2;
  Doc: IHTMLDocument2;
begin
  Doc := WebBrowser1.Document as IHTMLDocument2;
  pw := IHTMLWindow2(Doc.parentWindow);
  scrollpos := pw.screen.height;
  pw.scrollBy(0, scrollpos);
end;

procedure TForm1.DadosReducaoZ1Click(Sender: TObject);
begin
  mResp.Lines.Add('Dados da Redução Z' + sLineBreak + ACBrECF1.DadosReducaoZ);
  AtualizaMemos;
end;

procedure TForm1.Cliche1Click(Sender: TObject);
begin
  mResp.Lines.Add('Cliche: (' + ACBrECF1.Cliche + ')');
  AtualizaMemos;
end;

procedure TForm1.CNPJIE1Click(Sender: TObject);
begin
  mResp.Lines.Add('CNPJ: (' + ACBrECF1.CNPJ + ')');
  AtualizaMemos;
end;

procedure TForm1.IE1Click(Sender: TObject);
begin
  mResp.Lines.Add('IE: (' + ACBrECF1.IE + ')');
  AtualizaMemos;
end;

procedure TForm1.IM1Click(Sender: TObject);
begin
  mResp.Lines.Add('IM: (' + ACBrECF1.IM + ')');
  AtualizaMemos;
end;

procedure TForm1.NumCRZ1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num CRZ: (' + ACBrECF1.NumCRZ + ')');
  AtualizaMemos;
end;

procedure TForm1.NumCOOInicial1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num NumCOOInicial: (' + ACBrECF1.NumCOOInicial + ')');
  AtualizaMemos;
end;

procedure TForm1.ValorTotal1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalNaoFiscal: (' + FloatToStr(ACBrECF1.TotalNaoFiscal) + ')');
  AtualizaMemos;
end;

procedure TForm1.VendaBruta1Click(Sender: TObject);
begin
  mResp.Lines.Add('VendaBruta: (' + FloatToStr(ACBrECF1.VendaBruta) + ')');
  AtualizaMemos;
end;

procedure TForm1.GrandeTotal1Click(Sender: TObject);
begin
  mResp.Lines.Add('GrandeTotal: (' + FloatToStr(ACBrECF1.GrandeTotal) + ')');
  AtualizaMemos;
end;

procedure TForm1.TotalCancelamentos1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalCancelamentos: (' + FloatToStr(ACBrECF1.TotalCancelamentos) + ')');
  AtualizaMemos;
end;

procedure TForm1.TotalDescontos1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalDescontos: (' + FloatToStr(ACBrECF1.TotalDescontos) + ')');
  AtualizaMemos;
end;

procedure TForm1.TotalAcrescimos1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalAcrescimos: (' + FloatToStr(ACBrECF1.TotalAcrescimos) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalAcrescimos1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalAcrescimos: (' + FloatToStr(ACBrECF1.TotalAcrescimos) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalAcrescimos2Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalAcrescimosISSQN: (' + FloatToStr(ACBrECF1.TotalAcrescimosISSQN) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalAcrscimos1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalAcrescimosOPNF: (' + FloatToStr(ACBrECF1.TotalAcrescimosOPNF) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalCancelamentos1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalCancelamentos: (' + FloatToStr(ACBrECF1.TotalCancelamentos) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalCancelamentos2Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalCancelamentosISSQN: (' + FloatToStr(ACBrECF1.TotalCancelamentosISSQN) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalCancelamentos3Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalCancelamentosOPNF: (' + FloatToStr(ACBrECF1.TotalCancelamentosOPNF) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalDescontos1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalDescontos: (' + FloatToStr(ACBrECF1.TotalDescontos) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalDescontos2Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalDescontosISSQN: (' + FloatToStr(ACBrECF1.TotalDescontosISSQN) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalDescontos3Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalDescontosOPNF: (' + FloatToStr(ACBrECF1.TotalDescontosOPNF) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalIseno1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalIsencao: (' + FloatToStr(ACBrECF1.TotalIsencao) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalIseno2Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalIsencaoISSQN: (' + FloatToStr(ACBrECF1.TotalIsencaoISSQN) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalNaoTributado1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalNaoTributado: (' + FloatToStr(ACBrECF1.TotalNaoTributado) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalNaoTributado2Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalNaoTributadoISSQN: (' + FloatToStr(ACBrECF1.TotalNaoTributadoISSQN) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalNoFiscal1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalNaoFiscal: (' + FloatToStr(ACBrECF1.TotalNaoFiscal) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalSubstituicaoTributaria1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalSubstituicaoTributaria: (' + FloatToStr(ACBrECF1.TotalSubstituicaoTributaria) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalSubstituicaoTributaria2Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalSubstituicaoTributariaISSQN: (' + FloatToStr(ACBrECF1.TotalSubstituicaoTributariaISSQN) + ')');
  AtualizaMemos;
end;

procedure TForm1.otalTroco1Click(Sender: TObject);
begin
  mResp.Lines.Add('TotalTroco: (' + FloatToStr(ACBrECF1.TotalTroco) + ')');
  AtualizaMemos;
end;

procedure TForm1.UltimoItemVendido1Click(Sender: TObject);
begin
  mResp.Lines.Add('NumUltItem: (' + IntToStr(ACBrECF1.NumUltItem) + ')');
  AtualizaMemos;
end;

procedure TForm1.PAF1Click(Sender: TObject);
begin
  mResp.Lines.Add('PAF: (' + ACBrECF1.Paf + ')');
  AtualizaMemos;
end;

procedure TForm1.ParametroDescontoISSQN1Click(Sender: TObject);
begin
  mResp.Lines.Add('Parametro Desconto ISSQN: ' + IfThen(ACBrECF1.ParamDescontoISSQN, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.PorCOO1Click(Sender: TObject);
Var
  Linhas: TStringList;
  cCOOIni, cCOOFim: String;
  I, nCOOIni, nCOOFim: Integer;
begin
  cCOOIni := '0';
  cCOOFim := '0';

  if not InputQuery('Captura da MFD', 'Entre com o COO Inicial:', cCOOIni) then
    exit;
  nCOOIni := StrToIntDef(cCOOIni, -1);
  if nCOOIni < 0 then
    exit;

  if not InputQuery('Captura da MFD', 'Entre com o COO Final:', cCOOFim) then
    exit;
  nCOOFim := StrToIntDef(cCOOFim, -1);
  if nCOOFim < 0 then
    exit;

  Linhas := TStringList.Create;
  try
    ACBrECF1.LeituraMFDSerial(nCOOIni, nCOOFim, Linhas);

    For I := 0 to Linhas.Count - 1 do
      mResp.Lines.Add(Linhas[I]);
  finally
    Linhas.Free;
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.PorPeriodo1Click(Sender: TObject);
Var
  Linhas: TStringList;
  cDatIni, cDatFim: String;
  dDatIni, dDatFim: TDateTime;
  I: Integer;
begin
  cDatIni := '01/' + FormatDateTime('mm/yy', now);
  cDatFim := FormatDateTime('dd/mm/yy', now);

  if not InputQuery('Captura da MFD', 'Entre com o a Data Inicial (DD/MM/AA):', cDatIni) then
    exit;
  try
    dDatIni := StrToDateTime(StringReplace(cDatIni, '/', DateSeparator, [rfReplaceAll]));
  except
    exit;
  end;

  if not InputQuery('Captura da MFD', 'Entre com o a Data Final (DD/MM/AA):', cDatFim) then
    exit;
  try
    dDatFim := StrToDateTime(StringReplace(cDatFim, '/', DateSeparator, [rfReplaceAll]));
  except
    exit
  end;

  Linhas := TStringList.Create;
  try
    ACBrECF1.LeituraMFDSerial(dDatIni, dDatFim, Linhas);

    For I := 0 to Linhas.Count - 1 do
      mResp.Lines.Add(Linhas[I]);
  finally
    Linhas.Free;
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.Estado1Click(Sender: TObject);
begin
  mResp.Lines.Add('Estado: ' + Estados[ACBrECF1.Estado]);
  AtualizaMemos;
end;

procedure TForm1.NmeroReduesZrestantes1Click(Sender: TObject);
begin
  mResp.Lines.Add('NumReducoesZRestantes: ' + ACBrECF1.NumReducoesZRestantes);

  AtualizaMemos;
end;

procedure TForm1.NoFiscalCompleto1Click(Sender: TObject);
Var
  Valor, CodCNF, CodFPG: String;
begin
  CodCNF := '01';
  CodFPG := '01';
  Valor := '0';

  if not InputQuery('Comprovante Não Fiscal Completo', 'Entre com o indice do Comprovante Não Fiscal', CodCNF) then
    exit;

  if not InputQuery('Comprovante Não Fiscal Completo', 'Entre com o Valor do Comprovante Não Fiscal', Valor) then
    exit;

  if not InputQuery('Comprovante Não Fiscal Completo', 'Entre com o indice da Forma de Pagamento', CodFPG) then
    exit;

  ACBrECF1.NaoFiscalCompleto(CodCNF, StrToFloatDef(Valor, 0), CodFPG, 'TESTE DE COMPROVANTE NAO FISCAL');
  mResp.Lines.Add('Nao Fiscal Completo: ' + CodCNF + ' ' + Valor + ' ' + CodFPG);
  AtualizaMemos;
end;

procedure TForm1.AbreNoFiscal1Click(Sender: TObject);
Var
  CPF_CNPJ: String;
begin
  if not InputQuery('Abre Comprovante Não Fiscal', 'Se necessário, informe o CPF ou CNPJ do cliente', CPF_CNPJ) then
    exit;

  ACBrECF1.AbreNaoFiscal(CPF_CNPJ);
  mResp.Lines.Add('Abre Não Fiscal: ' + CPF_CNPJ);
  AtualizaMemos;
end;

procedure TForm1.RegistraItemNaoFiscal1Click(Sender: TObject);
Var
  Valor, CodCNF: String;
begin
  CodCNF := '01';
  Valor := '0';

  if not InputQuery('Registra Item Não Fiscal', 'Entre com o indice do Comprovante Não Fiscal', CodCNF) then
    exit;

  if not InputQuery('Registra Item Não Fiscal', 'Entre com o Valor do Comprovante Não Fiscal', Valor) then
    exit;

  ACBrECF1.RegistraItemNaoFiscal(CodCNF, StrToFloatDef(Valor, 0), 'TESTE DE COMPROVANTE NAO FISCAL');
  mResp.Lines.Add('Registra Item Nao Fiscal: ' + CodCNF + ' ' + Valor);
  AtualizaMemos;
end;

procedure TForm1.SubTotalizaNaoFiscal1Click(Sender: TObject);
Var
  Desc: String;
begin
  Desc := '0';

  if InputQuery('Subtotaliza Não Fiscal', 'Digite Valor negativo para Desconto' + #10 + 'ou Valor Positivo para Acrescimo', Desc) then
  begin
    ACBrECF1.SubtotalizaNaoFiscal(StrToFloat(Desc));
    mResp.Lines.Add('Subtotaliza Não Fiscal ' + Desc);
    AtualizaMemos;
  end;
end;

procedure TForm1.EfetuaPagamentoNaoFiscal1Click(Sender: TObject);
begin
  frPagamento.Show;
  frPagamento.TipoCupom := 'N';
end;

procedure TForm1.FechaNoFiscal1Click(Sender: TObject);
Var
  Obs: String;
  IndiceBMP: String;
begin
  Obs := 'Componentes ACBr|http://acbr.sourceforge.net';
  IndiceBMP := '0';
  if InputQuery('Fecha Não Fiscal', 'Se Necessário digite alguma Observaçao (até 8 linhas)' + #10 +
    'O sinal | (pipe) será convertido para #10 (quebra de linha)', Obs) then
  begin
    if (ACBrECF1.Modelo = ecfDaruma) and (ACBrECF1.MFD) then
      InputQuery('Impressao de imagem BMP ', 'Digite o Indice do BMP que deseja utilizar', IndiceBMP);

    Obs := StringReplace(Obs, '|', #10, [rfReplaceAll, rfIgnoreCase]);
    ACBrECF1.FechaNaoFiscal(Obs, StrToIntDef(IndiceBMP, 0));
    mResp.Lines.Add('Fecha Não Fiscal: ' + #10 + Obs);
    AtualizaMemos;
  end;
end;

procedure TForm1.CancelaNoFiscal1Click(Sender: TObject);
begin
  ACBrECF1.CancelaNaoFiscal;
  mResp.Lines.Add('Cancela Não Fiscal');
  AtualizaMemos;
end;

procedure TForm1.NumCCDC1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.CCDC: (' + ACBrECF1.NumCCDC + ')');
  AtualizaMemos;
end;

procedure TForm1.NumCCF1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.CCF: (' + ACBrECF1.NumCCF + ')');
  AtualizaMemos;
end;

procedure TForm1.NumCDC1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.CDC: (' + ACBrECF1.NumCDC + ')');
  AtualizaMemos;
end;

procedure TForm1.NumCFD1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.CFD: (' + ACBrECF1.NumCFD + ')');
  AtualizaMemos;
end;

procedure TForm1.NumCOO1Click(Sender: TObject);
begin
  NUltimoCupom1Click(Sender);
end;

procedure TForm1.IdentificaConsumidor1Click(Sender: TObject);
Var
  CPF, Nome, ENDERECO: String;
begin
  CPF := ACBrECF1.Consumidor.Documento;
  Nome := ACBrECF1.Consumidor.Nome;
  ENDERECO := ACBrECF1.Consumidor.ENDERECO;

  InputQuery('Identifica Consumidor', 'Informe o Documento', CPF);
  InputQuery('Identifica Consumidor', 'Informe o Nome do Consumidor', Nome);
  InputQuery('Identifica Consumidor', 'Se necessários, informe o Endereço do Consumidor', ENDERECO);

  ACBrECF1.IdentificaConsumidor(CPF, Nome, ENDERECO);
end;

procedure TForm1.IdentificaConsumidorRodap1Click(Sender: TObject);
begin
  mResp.Lines.Add('Identifica Consumidor no Rodapé: ' + IfThen(ACBrECF1.IdentificaConsumidorRodape, 'SIM', 'NAO'));
  AtualizaMemos;
end;

procedure TForm1.IdentificaOperador1Click(Sender: TObject);
Var
  Operador : String ;
begin
  Operador := edOperador.Text ;
  if not InputQuery('Identifica Operador',
                    'Entre com o Nome do Operador', Operador ) then
     exit ;

  if Operador <> '' then
  begin
     ACBrECF1.IdentificaOperador( Operador );
     edOperador.Text := ACBrECF1.Operador ;
  end ;
end;

procedure TForm1.edDirRFDChange(Sender: TObject);
begin
  ACBrRFD1.DirRFD := edDirRFD.Text;
end;

procedure TForm1.sbDirRFDClick(Sender: TObject);
begin
  OpenURL(ACBrRFD1.DirRFD);
end;

procedure TForm1.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(Application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtCaminho.Text := OpenDialog1.FileName;
  end;
end;

procedure TForm1.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrNFe1.SSL.SelecionarCertificado;
end;

procedure TForm1.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(Application.ExeName);
  if OpenDialog1.Execute then
  begin
    edtLogoMarca.Text := OpenDialog1.FileName;
  end;
end;

procedure TForm1.sbtnPathSalvarClick(Sender: TObject);
var
  Dir: string;
begin
  if length(edtPathLogs.Text) <= 0 then
    Dir := ExtractFileDir(Application.ExeName)
  else
    Dir := edtPathLogs.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    edtPathLogs.Text := Dir;
end;

procedure TForm1.bRFDLerClick(Sender: TObject);
begin
  if not ACBrRFD1.Ativo then
    raise Exception.Create('ACBrRFD não está ativo');

  mRFDParam.Lines.LoadFromFile(ACBrRFD1.ArqINI);
end;

procedure TForm1.bRFDSalvarClick(Sender: TObject);
Var
  OldAtivo: Boolean;
begin
  OldAtivo := ACBrRFD1.Ativo;
  try
    mRFDParam.Lines.SaveToFile(ACBrRFD1.ArqINI);
    ACBrRFD1.Desativar;
  finally
    ACBrRFD1.Ativo := OldAtivo;
  end;
end;

procedure TForm1.chRFDClick(Sender: TObject);
Var
  OldAtivo: Boolean;
begin
  OldAtivo := ACBrECF1.Ativo;
  try
    try
      ACBrECF1.Desativar;

      if chRFD.Checked then
        ACBrECF1.RFD := ACBrRFD1
      else
        ACBrECF1.RFD := nil;
    except
      chRFD.OnClick := nil;
      chRFD.Checked := Assigned(ACBrECF1.RFD);
      chRFD.OnClick := chRFDClick;

      raise;
    end;
  finally
    ACBrECF1.Ativo := OldAtivo;
  end;
end;

procedure TForm1.GravarINI;
Var
  ArqINI: String;
  INI: TIniFile;
begin
  ArqINI := ChangeFileExt(Application.ExeName, '.ini');

  INI := TIniFile.Create(ArqINI);
  try
    INI.WriteInteger('ECF', 'Modelo', cbxModelo.ItemIndex);
    INI.WriteInteger('ECF', 'Virtual', cbxECFVirtual.ItemIndex);
    INI.WriteString('ECF', 'Porta', cbxPorta.Text);
    INI.WriteInteger('ECF', 'TimeOut', seTimeOut.Value);
    INI.WriteInteger('ECF', 'IntervaloAposComando', seIntervaloAposComando.Value);
    INI.WriteBool('ECF', 'TentarNovamente', chTentar.Checked);
    INI.WriteBool('ECF', 'ControlePorta', chControlePorta.Checked);
    INI.WriteBool('ECF', 'BloqueiaMouseTeclado', chBloqueia.Checked);
    INI.WriteBool('ECF', 'ExibeMsgAguarde', chExibeMsg.Checked);
    INI.WriteBool('ECF', 'ArredondaPorQtd', chArredondaPorQtd.Checked);
    INI.WriteBool('ECF', 'GavetaSinalInvertido', chGavetaSinalInvertido.Checked);
    INI.WriteBool('ECF', 'DescricaoGrande', chDescricaoGrande.Checked);
    INI.WriteBool('ECF', 'ArredondamentoItemMFD', chArredondamentoItemMFD.Checked);
    INI.WriteString('ECF', 'MensagemAguarde', StringReplace(mMsg.Text, sLineBreak, '|', [rfReplaceAll]));
    INI.WriteString('ECF', 'ArqLog', edLog.Text);
    INI.WriteString('ECF', 'SerialParams', ACBrECF1.Device.ParamsString);
    INI.WriteString('ECF', 'Operador', ACBrECF1.Operador);
    INI.WriteInteger('ECF', 'BandWidth', seBandWidth.Value);

    INI.WriteBool('RFD', 'GerarRFD', chRFD.Checked);
    INI.WriteString('RFD', 'DirRFD', edDirRFD.Text);
    INI.WriteString('RFD', 'SH_RazaoSocial', edSH_RazaoSocial.Text);
    INI.WriteString('RFD', 'SH_COO', edSH_COO.Text);
    INI.WriteString('RFD', 'SH_CNPJ', edSH_CNPJ.Text);
    INI.WriteString('RFD', 'SH_IE', edSH_IE.Text);
    INI.WriteString('RFD', 'SH_IM', edSH_IM.Text);
    INI.WriteString('RFD', 'SH_Aplicativo', edSH_Aplicativo.Text);
    INI.WriteString('RFD', 'SH_NumeroAplicativo', edSH_NumeroAP.Text);
    INI.WriteString('RFD', 'SH_VersaoAplicativo', edSH_VersaoAP.Text);
    INI.WriteString('RFD', 'SH_Linha1', edSH_Linha1.Text);
    INI.WriteString('RFD', 'SH_Linha2', edSH_Linha2.Text);

    INI.WriteBool('AAC', 'GerarAAC', chAACUsar.Checked);
    INI.WriteString('AAC', 'NomeArquivo', edAACNomeArq.Text);
    INI.WriteString('AAC', 'ArqLog', edAACLog.Text);

    INI.WriteInteger('SAT', 'Modelo', cbxModeloSAT.ItemIndex);
    INI.WriteString('SAT', 'ArqLog', edLogSAT.Text);
    INI.WriteString('SAT', 'NomeDLL', edNomeDLL.Text);
    INI.WriteString('SAT', 'CodigoAtivacao', edtCodigoAtivacao.Text);
    INI.WriteString('SAT', 'CodigoUF', edtCodUF.Text);
    INI.WriteInteger('SAT', 'NumeroCaixa', seNumeroCaixa.Value);
    INI.WriteInteger('SAT', 'Ambiente', cbxAmbiente.ItemIndex);
    INI.WriteInteger('SAT', 'PaginaDeCodigo', sePagCod.Value);
    INI.WriteFloat('SAT', 'versaoDadosEnt', StrToFloatDef(sfeVersaoEnt.Text, 0.00));
    INI.WriteBool('SAT', 'FormatarXML', cbxFormatXML.Checked);
    INI.ReadBool('SAT', 'SalvarCFe', cbxSalvarCFe.Checked);

    INI.WriteString('Extrato', 'Porta', edtPorta.Text);
    INI.WriteString('Extrato', 'ParamsString', ACBrPosPrinter1.Device.ParamsString);

    INI.WriteString('Emit', 'CNPJ', edtEmitCNPJ.Text);
    INI.WriteString('Emit', 'IE', edtEmitIE.Text);
    INI.WriteString('Emit', 'IM', edtEmitIM.Text);
    INI.WriteInteger('Emit', 'RegTributario', cbxRegTributario.ItemIndex);
    INI.WriteInteger('Emit', 'RegTribISSQN', cbxRegTribISSQN.ItemIndex);
    INI.WriteInteger('Emit', 'IndRatISSQN', cbxIndRatISSQN.ItemIndex);

    INI.WriteString('SwH', 'CNPJ', edtSwHCNPJ.Text);
    INI.WriteString('SwH', 'Assinatura', edtSwHAssinatura.Text);

    INI.WriteBool('Fortes', 'UsarFortes', cbUsarFortes.Checked);
    INI.WriteInteger('Fortes', 'Largura', seLargura.Value);
    INI.WriteInteger('Fortes', 'MargemTopo', seMargemTopo.Value);
    INI.WriteInteger('Fortes', 'MargemFundo', seMargemFundo.Value);
    INI.WriteInteger('Fortes', 'MargemEsquerda', seMargemEsquerda.Value);
    INI.WriteInteger('Fortes', 'MargemDireita', seMargemDireita.Value);
    INI.WriteBool('Fortes', 'Preview', cbPreview.Checked);

    INI.WriteString('Printer', 'Name', Printer.printers[Printer.PrinterIndex]);

    INI.WriteString('Certificado', 'Caminho', edtCaminho.Text);
    INI.WriteString('Certificado', 'Senha', edtSenha.Text);
    INI.WriteString('Certificado', 'NumSerie', edtNumSerie.Text);

    INI.WriteInteger('Geral', 'DANFE', rgTipoDanfe.ItemIndex);
    INI.WriteInteger('Geral', 'FormaEmissao', rgFormaEmissao.ItemIndex);
    INI.WriteString('Geral', 'LogoMarca', edtLogoMarca.Text);
    INI.WriteBool('Geral', 'Salvar', ckSalvar.Checked);
    INI.WriteString('Geral', 'PathSalvar', edtPathLogs.Text);

    INI.WriteString('WebService', 'UF', cbUF.Text);
    INI.WriteInteger('WebService', 'Ambiente', rgTipoAmb.ItemIndex);
    INI.WriteBool('WebService', 'Visualizar', ckVisualizar.Checked);

    INI.WriteString('Proxy', 'Host', edtProxyHost.Text);
    INI.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    INI.WriteString('Proxy', 'User', edtProxyUser.Text);
    INI.WriteString('Proxy', 'Pass', edtProxySenha.Text);

    INI.WriteString('Emitente', 'CNPJ', edtEmitCNPJNFe.Text);
    INI.WriteString('Emitente', 'IE', edtEmitIENFe.Text);
    INI.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    INI.WriteString('Emitente', 'Fantasia', edtEmitFantasia.Text);
    INI.WriteString('Emitente', 'Fone', edtEmitFone.Text);
    INI.WriteString('Emitente', 'CEP', edtEmitCEP.Text);
    INI.WriteString('Emitente', 'Logradouro', edtEmitLogradouro.Text);
    INI.WriteString('Emitente', 'Numero', edtEmitNumero.Text);
    INI.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    INI.WriteString('Emitente', 'Bairro', edtEmitBairro.Text);
    INI.WriteString('Emitente', 'CodCidade', edtEmitCodCidade.Text);
    INI.WriteString('Emitente', 'Cidade', edtEmitCidade.Text);
    INI.WriteString('Emitente', 'UF', edtEmitUF.Text);
  finally
    INI.Free;
  end;
end;

procedure TForm1.LerINI;
Var
  ArqINI: String;
  INI: TIniFile;
  Ok: Boolean;
begin
  ArqINI := ChangeFileExt(Application.ExeName, '.ini');

  INI := TIniFile.Create(ArqINI);
  try
    cbxModelo.ItemIndex := INI.ReadInteger('ECF', 'Modelo', cbxModelo.ItemIndex);
    cbxModeloChange(nil);
    cbxECFVirtual.ItemIndex := INI.ReadInteger('ECF', 'Virtual', 0);
    cbxECFVirtualChange(nil);
    cbxPorta.Text := INI.ReadString('ECF', 'Porta', cbxPorta.Text);
    seTimeOut.Value := INI.ReadInteger('ECF', 'TimeOut', seTimeOut.Value);
    seIntervaloAposComando.Value := INI.ReadInteger('ECF', 'IntervaloAposComando', seIntervaloAposComando.Value);
    chTentar.Checked := INI.ReadBool('ECF', 'TentarNovamente', chTentar.Checked);
    chControlePorta.Checked := INI.ReadBool('ECF', 'ControlePorta', true);
    chBloqueia.Checked := INI.ReadBool('ECF', 'BloqueiaMouseTeclado', chBloqueia.Checked);
    chExibeMsg.Checked := INI.ReadBool('ECF', 'ExibeMsgAguarde', chExibeMsg.Checked);
    chArredondaPorQtd.Checked := INI.ReadBool('ECF', 'ArredondaPorQtd', chArredondaPorQtd.Checked);
    chDescricaoGrande.Checked := INI.ReadBool('ECF', 'DescricaoGrande', chDescricaoGrande.Checked);
    chArredondamentoItemMFD.Checked := INI.ReadBool('ECF', 'ArredondamentoItemMFD', chArredondamentoItemMFD.Checked);
    chGavetaSinalInvertido.Checked := INI.ReadBool('ECF', 'GavetaSinalInvertido', chGavetaSinalInvertido.Checked);
    mMsg.Text := StringReplace(INI.ReadString('ECF', 'MensagemAguarde', mMsg.Text), '|', sLineBreak, [rfReplaceAll]);
    edLog.Text := INI.ReadString('ECF', 'ArqLog', edLog.Text);
    ACBrECF1.Device.ParamsString := INI.ReadString('ECF', 'SerialParams', '');
    edOperador.Text := INI.ReadString('ECF', 'Operador', '');
    seBandWidth.Value := INI.ReadInteger('ECF', 'BandWidth', seBandWidth.Value);

    chRFD.Checked := INI.ReadBool('RFD', 'GerarRFD', chRFD.Checked);
    edDirRFD.Text := INI.ReadString('RFD', 'DirRFD', edDirRFD.Text);
    edSH_RazaoSocial.Text := INI.ReadString('RFD', 'SH_RazaoSocial', edSH_RazaoSocial.Text);
    edSH_COO.Text := INI.ReadString('RFD', 'SH_COO', edSH_COO.Text);
    edSH_CNPJ.Text := INI.ReadString('RFD', 'SH_CNPJ', edSH_CNPJ.Text);
    edSH_IE.Text := INI.ReadString('RFD', 'SH_IE', edSH_IE.Text);
    edSH_IM.Text := INI.ReadString('RFD', 'SH_IM', edSH_IM.Text);
    edSH_Aplicativo.Text := INI.ReadString('RFD', 'SH_Aplicativo', edSH_Aplicativo.Text);
    edSH_NumeroAP.Text := INI.ReadString('RFD', 'SH_NumeroAplicativo', edSH_NumeroAP.Text);
    edSH_VersaoAP.Text := INI.ReadString('RFD', 'SH_VersaoAplicativo', edSH_VersaoAP.Text);
    edSH_Linha1.Text := INI.ReadString('RFD', 'SH_Linha1', edSH_Linha1.Text);
    edSH_Linha2.Text := INI.ReadString('RFD', 'SH_Linha2', edSH_Linha2.Text);

    chAACUsar.Checked := INI.ReadBool('AAC', 'GerarAAC', False);
    edAACNomeArq.Text := INI.ReadString('AAC', 'NomeArquivo', edAACNomeArq.Text);
    edAACLog.Text := INI.ReadString('AAC', 'ArqLog', edAACLog.Text);

    cbxModeloSAT.ItemIndex := INI.ReadInteger('SAT', 'Modelo', 0);
    edLogSAT.Text := INI.ReadString('SAT', 'ArqLog', 'ACBrSAT.log');
    edNomeDLL.Text := INI.ReadString('SAT', 'NomeDLL', 'C:\SAT\SAT.DLL');
    edtCodigoAtivacao.Text := INI.ReadString('SAT', 'CodigoAtivacao', '123456');
    edtCodUF.Text := INI.ReadString('SAT', 'CodigoUF', '35');
    seNumeroCaixa.Value := INI.ReadInteger('SAT', 'NumeroCaixa', 1);
    cbxAmbiente.ItemIndex := INI.ReadInteger('SAT', 'Ambiente', 1);
    sePagCod.Value := INI.ReadInteger('SAT', 'PaginaDeCodigo', 0);
    sfeVersaoEnt.Text := FloatToStr(INI.ReadFloat('SAT', 'versaoDadosEnt', cversaoDadosEnt));
    cbxFormatXML.Checked := INI.ReadBool('SAT', 'FormatarXML', true);
    cbxSalvarCFe.Checked := INI.ReadBool('SAT', 'SalvarCFe', true);
    sePaginaCodigoChange(Self);

    edtPorta.Text := INI.ReadString('Extrato', 'Porta', 'COM1');
    ACBrPosPrinter1.Device.ParamsString := INI.ReadString('Extrato', 'ParamsString', '');

    edtEmitCNPJ.Text := INI.ReadString('Emit', 'CNPJ', '');
    edtEmitIE.Text := INI.ReadString('Emit', 'IE', '');
    edtEmitIM.Text := INI.ReadString('Emit', 'IM', '');
    cbxRegTributario.ItemIndex := INI.ReadInteger('Emit', 'RegTributario', 0);
    cbxRegTribISSQN.ItemIndex := INI.ReadInteger('Emit', 'RegTribISSQN', 0);
    cbxIndRatISSQN.ItemIndex := INI.ReadInteger('Emit', 'IndRatISSQN', 0);

    edtSwHCNPJ.Text := INI.ReadString('SwH', 'CNPJ', '11111111111111');
    edtSwHAssinatura.Text := INI.ReadString('SwH', 'Assinatura', '');

    cbUsarFortes.Checked := INI.ReadBool('Fortes', 'UsarFortes', true);
    cbUsarEscPos.Checked := not cbUsarFortes.Checked;
    seLargura.Value := INI.ReadInteger('Fortes', 'Largura', Trunc(ACBrSATExtratoFortes1.LarguraBobina));
    seMargemTopo.Value := INI.ReadInteger('Fortes', 'MargemTopo', Trunc(ACBrSATExtratoFortes1.MargemSuperior));
    seMargemFundo.Value := INI.ReadInteger('Fortes', 'MargemFundo', Trunc(ACBrSATExtratoFortes1.MargemInferior));
    seMargemEsquerda.Value := INI.ReadInteger('Fortes', 'MargemEsquerda', Trunc(ACBrSATExtratoFortes1.MargemEsquerda));
    seMargemDireita.Value := INI.ReadInteger('Fortes', 'MargemDireita', Trunc(ACBrSATExtratoFortes1.MargemDireita));
    cbPreview.Checked := INI.ReadBool('Fortes', 'Preview', true);

    lImpressora.Caption := INI.ReadString('Printer', 'Name', Printer.printers[Printer.PrinterIndex]);

{$IFDEF ACBrNFeOpenSSL}
    edtCaminho.Text := INI.ReadString('Certificado', 'Caminho', '');
    edtSenha.Text := INI.ReadString('Certificado', 'Senha', '');
    ACBrNFe1.Configuracoes.Certificados.Certificado := edtCaminho.Text;
    ACBrNFe1.Configuracoes.Certificados.Senha := edtSenha.Text;
    edtNumSerie.Visible := False;
    Label73.Visible := False;
    sbtnGetCert.Visible := False;
{$ELSE}
    edtNumSerie.Text := INI.ReadString('Certificado', 'NumSerie', '');
    ACBrNFe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;
    edtNumSerie.Text := ACBrNFe1.Configuracoes.Certificados.NumeroSerie;
    Label71.Caption := 'Informe o número de série do certificado'#13 + 'Disponível no Internet Explorer no menu'#13 +
      'Ferramentas - Opções da Internet - Conteúdo '#13 + 'Certificados - Exibir - Detalhes - '#13 + 'Número do certificado';
    Label72.Visible := False;
    edtCaminho.Visible := False;
    edtSenha.Visible := False;
    sbtnCaminhoCert.Visible := False;
{$ENDIF}
    rgFormaEmissao.ItemIndex := INI.ReadInteger('Geral', 'FormaEmissao', 0);
    ckSalvar.Checked := INI.ReadBool('Geral', 'Salvar', true);
    edtPathLogs.Text := INI.ReadString('Geral', 'PathSalvar', '');
    ACBrNFe1.Configuracoes.Geral.FormaEmissao := StrToTpEmis(Ok, IntToStr(rgFormaEmissao.ItemIndex + 1));
    ACBrNFe1.Configuracoes.Geral.Salvar := ckSalvar.Checked;
    ACBrNFe1.Configuracoes.Arquivos.PathSalvar := edtPathLogs.Text;

    cbUF.ItemIndex := cbUF.Items.IndexOf(INI.ReadString('WebService', 'UF', 'SP'));
    rgTipoAmb.ItemIndex := INI.ReadInteger('WebService', 'Ambiente', 0);
    ckVisualizar.Checked := INI.ReadBool('WebService', 'Visualizar', False);
    ACBrNFe1.Configuracoes.WebServices.UF := cbUF.Text;
    ACBrNFe1.Configuracoes.WebServices.Ambiente := StrToTpAmb(Ok, IntToStr(rgTipoAmb.ItemIndex + 1));
    ACBrNFe1.Configuracoes.WebServices.Visualizar := ckVisualizar.Checked;

    edtProxyHost.Text := INI.ReadString('Proxy', 'Host', '');
    edtProxyPorta.Text := INI.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := INI.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := INI.ReadString('Proxy', 'Pass', '');
    ACBrNFe1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
    ACBrNFe1.Configuracoes.WebServices.ProxyPort := edtProxyPorta.Text;
    ACBrNFe1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
    ACBrNFe1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;

    rgTipoDanfe.ItemIndex := INI.ReadInteger('Geral', 'DANFE', 0);
    edtLogoMarca.Text := INI.ReadString('Geral', 'LogoMarca', '');
    if ACBrNFe1.DANFE <> nil then
    begin
      ACBrNFe1.DANFE.TipoDANFE := StrToTpImp(Ok, IntToStr(rgTipoDanfe.ItemIndex + 1));
      ACBrNFe1.DANFE.Logo := edtLogoMarca.Text;
    end;

    edtEmitCNPJNFe.Text := INI.ReadString('Emitente', 'CNPJ', '');
    edtEmitIENFe.Text := INI.ReadString('Emitente', 'IE', '');
    edtEmitRazao.Text := INI.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text := INI.ReadString('Emitente', 'Fantasia', '');
    edtEmitFone.Text := INI.ReadString('Emitente', 'Fone', '');
    edtEmitCEP.Text := INI.ReadString('Emitente', 'CEP', '');
    edtEmitLogradouro.Text := INI.ReadString('Emitente', 'Logradouro', '');
    edtEmitNumero.Text := INI.ReadString('Emitente', 'Numero', '');
    edtEmitComp.Text := INI.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text := INI.ReadString('Emitente', 'Bairro', '');
    edtEmitCodCidade.Text := INI.ReadString('Emitente', 'CodCidade', '');
    edtEmitCidade.Text := INI.ReadString('Emitente', 'Cidade', '');
    edtEmitUF.Text := INI.ReadString('Emitente', 'UF', '');
  finally
    INI.Free;
  end;
end;

procedure TForm1.lerParmetros1Click(Sender: TObject);
begin
  LerINI;
end;

procedure TForm1.seTimeOutChange(Sender: TObject);
begin
  ACBrECF1.TimeOut := seTimeOut.Value;
end;

procedure TForm1.seBandWidthChange(Sender: TObject);
begin
  ACBrECF1.Device.MaxBandwidth := seBandWidth.Value;
end;

procedure TForm1.seIntervaloAposComandoChange(Sender: TObject);
begin
  ACBrECF1.IntervaloAposComando := seIntervaloAposComando.Value;
end;

procedure TForm1.sePaginaCodigoChange(Sender: TObject);
begin
  ACBrECF1.PaginaDeCodigo := sePaginaCodigo.Value;
end;

procedure TForm1.edSH_RazaoSocialChange(Sender: TObject);
begin
  ACBrRFD1.SH_RazaoSocial := edSH_RazaoSocial.Text;
end;

procedure TForm1.edSH_COOChange(Sender: TObject);
begin
  ACBrRFD1.SH_COO := edSH_COO.Text;
end;

procedure TForm1.edSH_CNPJChange(Sender: TObject);
begin
  ACBrRFD1.SH_CNPJ := edSH_CNPJ.Text;
end;

procedure TForm1.edSH_IEChange(Sender: TObject);
begin
  ACBrRFD1.SH_IE := edSH_IE.Text;
end;

procedure TForm1.edSH_IMChange(Sender: TObject);
begin
  ACBrRFD1.SH_IM := edSH_IM.Text;
end;

procedure TForm1.edSH_AplicativoChange(Sender: TObject);
begin
  ACBrRFD1.SH_NomeAplicativo := edSH_Aplicativo.Text;
end;

procedure TForm1.edSH_NumeroAPChange(Sender: TObject);
begin
  ACBrRFD1.SH_NumeroAplicativo := edSH_NumeroAP.Text;
end;

procedure TForm1.edSH_VersaoAPChange(Sender: TObject);
begin
  ACBrRFD1.SH_VersaoAplicativo := edSH_VersaoAP.Text;
end;

procedure TForm1.edSH_Linha1Change(Sender: TObject);
begin
  ACBrRFD1.SH_Linha1 := edSH_Linha1.Text;
end;

procedure TForm1.edSH_Linha2Change(Sender: TObject);
begin
  ACBrRFD1.SH_Linha2 := edSH_Linha2.Text;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  frmSobre := TfrmSobre.Create(Self);
  try
    frmSobre.lVersao.Caption := 'Ver: ' + ECFTeste_VERSAO + ' ACBr ' + ACBR_VERSAO;
    frmSobre.ShowModal;
  finally
    FreeAndNil(frmSobre);
  end;
end;

procedure TForm1.DataMovimento1Click(Sender: TObject);
begin
  mResp.Lines.Add('Data Movimento: (' + FormatDateTime('dd/mm/yy', ACBrECF1.DataMovimento) + ')');
  AtualizaMemos;
end;

procedure TForm1.DAV1Click(Sender: TObject);
begin
  frmDAV := TfrmDAV.Create(Self);
  try
    frmDAV.ShowModal;
  finally
    FreeAndNil(frmDAV);
  end;
end;

procedure TForm1.DAVOS1Click(Sender: TObject);
begin
  frmDAVOS := TfrmDAVOS.Create(Self);
  try
    frmDAVOS.ShowModal;
  finally
    FreeAndNil(frmDAVOS);
  end;
end;

procedure TForm1.DadosUltimaReduoZ1Click(Sender: TObject);
Var
  AIni: TMemIniFile;
  AStringList: TStringList;
  Resp: String;
  AVal: Double;
  ADate: TDateTime;
  AStr: String;
begin
  Resp := ACBrECF1.DadosUltimaReducaoZ;
  mResp.Lines.Add('Dados da Ultima Redução Z' + sLineBreak + Resp);

  AStringList := TStringList.Create;
  AIni := TMemIniFile.Create('DadosUltimaReducaoZ.ini');
  try
    AStringList.Text := Resp;
    AIni.SetStrings(AStringList);

    // Lendo a Data do Movimento
    ADate := AIni.ReadDateTime('ECF', 'DataMovimento', 0);
    ShowMessage('Data do Movimento' + sLineBreak + DateToStr(ADate));

    // Lendo o NumCOOInicial
    AStr := AIni.ReadString('ECF', 'NumCOOInicial', '');
    ShowMessage('COO Inicial' + AStr);

    // Lendo a Venda Bruta:
    AVal := AIni.ReadFloat('Totalizadores', 'VendaBruta', 0);
    ShowMessage('Venda Bruta' + sLineBreak + FormatFloat('0.00', AVal));

  finally
    AIni.Free;
    AStringList.Free;
  end;

  AtualizaMemos;
end;

procedure TForm1.btSerial1Click(Sender: TObject);
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(Self);

  try
    frConfiguraSerial.Device.Porta := ACBrPosPrinter1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := edtPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      edtPorta.Text := frConfiguraSerial.Device.Porta;
      ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
  end;
end;

procedure TForm1.btSerialClick(Sender: TObject);
Var
  frConfiguraSerial: TfrConfiguraSerial;
begin
  frConfiguraSerial := TfrConfiguraSerial.Create(Self);

  try
    frConfiguraSerial.Device.Porta := ACBrECF1.Device.Porta;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text;
    frConfiguraSerial.Device.ParamsString := ACBrECF1.Device.ParamsString;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbxPorta.Text := frConfiguraSerial.Device.Porta;
      ACBrECF1.Device.ParamsString := frConfiguraSerial.Device.ParamsString;
    end;
  finally
    FreeAndNil(frConfiguraSerial);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Str: String;
  A: Integer;
begin
  Str := '';
  For A := 1 to 255 do
    Str := Str + chr(A);

  ACBrECF1.LinhaRelatorioGerencial(Str);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  mResp.Lines.Add(ACBrECF1.RetornaInfoECF(edInfo.Text));
end;

procedure TForm1.btnDadosRZClick(Sender: TObject);
var
  I: Integer;
  SRZ: AnsiString;
begin
  SRZ := ACBrECF1.DadosReducaoZ;

  mRZ.Clear;
  with ACBrECF1.DadosReducaoZClass do
  begin
    mRZ.Lines.Add('Data Impressora    : ' + DateToStr(DataDaImpressora));
    mRZ.Lines.Add('Numero Série       : ' + NumeroDeSerie);
    mRZ.Lines.Add('Numero Série MFD   : ' + NumeroDeSerieMFD);
    mRZ.Lines.Add('Numero ECF         : ' + NumeroDoECF);
    mRZ.Lines.Add('Numero Loja        : ' + NumeroDaLoja);
    mRZ.Lines.Add('Numero COO Inicial : ' + NumeroCOOInicial);

    mRZ.Lines.Add('{ REDUÇÃO Z }');
    mRZ.Lines.Add('Data Movimento  : ' + DateToStr(DataDoMovimento));
    mRZ.Lines.Add('');
    mRZ.Lines.Add('{ CONTADORES }');
    mRZ.Lines.Add('COO  : ' + IntToStr(COO));
    mRZ.Lines.Add('GNF  : ' + IntToStr(GNF));
    mRZ.Lines.Add('CRO  : ' + IntToStr(CRO));
    mRZ.Lines.Add('CRZ  : ' + IntToStr(CRZ));
    mRZ.Lines.Add('CCF  : ' + IntToStr(CCF));
    mRZ.Lines.Add('CFD  : ' + IntToStr(CFD));
    mRZ.Lines.Add('CDC  : ' + IntToStr(CDC));
    mRZ.Lines.Add('GRG  : ' + IntToStr(GRG));
    mRZ.Lines.Add('GNFC : ' + IntToStr(GNFC));
    mRZ.Lines.Add('CFC  : ' + IntToStr(CFC));
    mRZ.Lines.Add('NCN  : ' + IntToStr(NCN));
    mRZ.Lines.Add('CCDC : ' + IntToStr(CCDC));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ TOTALIZADORES }');
    mRZ.Lines.Add('Grande Total      : ' + FormatFloat('###,##0.00', ValorGrandeTotal));
    mRZ.Lines.Add('VendaBruta        : ' + FormatFloat('###,##0.00', ValorVendaBruta));
    mRZ.Lines.Add('CancelamentoICMS  : ' + FormatFloat('###,##0.00', CancelamentoICMS));
    mRZ.Lines.Add('DescontoICMS      : ' + FormatFloat('###,##0.00', DescontoICMS));
    mRZ.Lines.Add('CancelamentoISSQN : ' + FormatFloat('###,##0.00', CancelamentoISSQN));
    mRZ.Lines.Add('DescontoISSQN     : ' + FormatFloat('###,##0.00', DescontoISSQN));
    mRZ.Lines.Add('CancelamentoOPNF  : ' + FormatFloat('###,##0.00', CancelamentoOPNF));
    mRZ.Lines.Add('DescontoOPNF      : ' + FormatFloat('###,##0.00', DescontoOPNF));
    mRZ.Lines.Add('VendaLiquida      : ' + FormatFloat('###,##0.00', VendaLiquida));
    mRZ.Lines.Add('AcrescimoICMS     : ' + FormatFloat('###,##0.00', AcrescimoICMS));
    mRZ.Lines.Add('AcrescimoISSQN    : ' + FormatFloat('###,##0.00', AcrescimoISSQN));
    mRZ.Lines.Add('AcrescimoOPNF     : ' + FormatFloat('###,##0.00', AcrescimoOPNF));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ ICMS }');
    for I := 0 to ICMS.Count - 1 do
    begin
      mRZ.Lines.Add('Indice    : ' + ICMS[I].Indice);
      mRZ.Lines.Add('Tipo      : ' + ICMS[I].Tipo);
      mRZ.Lines.Add('Aliquota  : ' + FormatFloat('0.00', ICMS[I].Aliquota));
      mRZ.Lines.Add('Total     : ' + FormatFloat('###,##0.00', ICMS[I].Total));
    end;
    mRZ.Lines.Add('TotalICMS         : ' + FormatFloat('###,##0.00', TotalICMS));
    mRZ.Lines.Add('SubstituicaoTributariaICMS: ' + FormatFloat('###,##0.00', SubstituicaoTributariaICMS));
    mRZ.Lines.Add('IsentoICMS                : ' + FormatFloat('###,##0.00', IsentoICMS));
    mRZ.Lines.Add('NaoTributadoICMS          : ' + FormatFloat('###,##0.00', NaoTributadoICMS));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ ISSQN }');
    for I := 0 to ISSQN.Count - 1 do
    begin
      mRZ.Lines.Add('Indice    : ' + ISSQN[I].Indice);
      mRZ.Lines.Add('Tipo      : ' + ISSQN[I].Tipo);
      mRZ.Lines.Add('Aliquota  : ' + FormatFloat('0.00', ISSQN[I].Aliquota));
      mRZ.Lines.Add('Total     : ' + FormatFloat('###,##0.00', ISSQN[I].Total));
    end;
    mRZ.Lines.Add('TotalISSQN        : ' + FormatFloat('###,##0.00', TotalISSQN));
    mRZ.Lines.Add('SubstituicaoTributariaISSQN: ' + FormatFloat('###,##0.00', SubstituicaoTributariaISSQN));
    mRZ.Lines.Add('IsentoISSQN                : ' + FormatFloat('###,##0.00', IsentoISSQN));
    mRZ.Lines.Add('NaoTributadoISSQN          : ' + FormatFloat('###,##0.00', NaoTributadoISSQN));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ TOTALIZADORES NÃO FISCAIS }');
    for I := 0 to TotalizadoresNaoFiscais.Count - 1 do
    begin
      mRZ.Lines.Add('Indice     : ' + TotalizadoresNaoFiscais[I].Indice);
      mRZ.Lines.Add('Descrição  : ' + TotalizadoresNaoFiscais[I].Descricao);
      mRZ.Lines.Add('Forma Pagto: ' + TotalizadoresNaoFiscais[I].FormaPagamento);
      mRZ.Lines.Add('Total      : ' + FormatFloat('###,##0.00', TotalizadoresNaoFiscais[I].Total));
    end;
    mRZ.Lines.Add('TotalOperacaoNaoFiscal : ' + FormatFloat('###,##0.00', TotalOperacaoNaoFiscal));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ RELATÓRIO GERENCIAL }');
    for I := 0 to RelatorioGerencial.Count - 1 do
    begin
      mRZ.Lines.Add('Indice     : ' + RelatorioGerencial[I].Indice);
      mRZ.Lines.Add('Descrição  : ' + RelatorioGerencial[I].Descricao);
    end;
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ MEIOS DE PAGAMENTO }');
    for I := 0 to MeiosDePagamento.Count - 1 do
    begin
      mRZ.Lines.Add('Indice     : ' + MeiosDePagamento[I].Indice);
      mRZ.Lines.Add('Descrição  : ' + MeiosDePagamento[I].Descricao);
      mRZ.Lines.Add('Total      : ' + FormatFloat('###,##0.00', MeiosDePagamento[I].Total));
    end;
    mRZ.Lines.Add('Total Troco : ' + FormatFloat('###,##0.00', TotalTroco));
  end;

  mRZ.Lines.Add('********  Retorno sem tratamento INICIO  ********');
  mRZ.Lines.Add(SRZ);
  mRZ.Lines.Add('********  Retorno sem tratamento FIM  ********');
end;

procedure TForm1.btnDadosUltimaRZClick(Sender: TObject);
var
  I: Integer;
  SRZ: AnsiString;
begin
  SRZ := ACBrECF1.DadosUltimaReducaoZ;

  mRZ.Clear;
  with ACBrECF1.DadosReducaoZClass do
  begin
    mRZ.Lines.Add('Data Impressora    : ' + DateToStr(DataDaImpressora));
    mRZ.Lines.Add('Numero Série       : ' + NumeroDeSerie);
    mRZ.Lines.Add('Numero Série MFD   : ' + NumeroDeSerieMFD);
    mRZ.Lines.Add('Numero ECF         : ' + NumeroDoECF);
    mRZ.Lines.Add('Numero Loja        : ' + NumeroDaLoja);
    mRZ.Lines.Add('Numero COO Inicial : ' + NumeroCOOInicial);

    mRZ.Lines.Add('{ REDUÇÃO Z }');
    mRZ.Lines.Add('Data Movimento  : ' + DateToStr(DataDoMovimento));
    mRZ.Lines.Add('');
    mRZ.Lines.Add('{ CONTADORES }');
    mRZ.Lines.Add('COO  : ' + IntToStr(COO));
    mRZ.Lines.Add('GNF  : ' + IntToStr(GNF));
    mRZ.Lines.Add('CRO  : ' + IntToStr(CRO));
    mRZ.Lines.Add('CRZ  : ' + IntToStr(CRZ));
    mRZ.Lines.Add('CCF  : ' + IntToStr(CCF));
    mRZ.Lines.Add('CFD  : ' + IntToStr(CFD));
    mRZ.Lines.Add('CDC  : ' + IntToStr(CDC));
    mRZ.Lines.Add('GRG  : ' + IntToStr(GRG));
    mRZ.Lines.Add('GNFC : ' + IntToStr(GNFC));
    mRZ.Lines.Add('CFC  : ' + IntToStr(CFC));
    mRZ.Lines.Add('NCN  : ' + IntToStr(NCN));
    mRZ.Lines.Add('CCDC : ' + IntToStr(CCDC));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ TOTALIZADORES }');
    mRZ.Lines.Add('Grande Total      : ' + FormatFloat('###,##0.00', ValorGrandeTotal));
    mRZ.Lines.Add('VendaBruta        : ' + FormatFloat('###,##0.00', ValorVendaBruta));
    mRZ.Lines.Add('CancelamentoICMS  : ' + FormatFloat('###,##0.00', CancelamentoICMS));
    mRZ.Lines.Add('DescontoICMS      : ' + FormatFloat('###,##0.00', DescontoICMS));
    mRZ.Lines.Add('CancelamentoISSQN : ' + FormatFloat('###,##0.00', CancelamentoISSQN));
    mRZ.Lines.Add('DescontoISSQN     : ' + FormatFloat('###,##0.00', DescontoISSQN));
    mRZ.Lines.Add('CancelamentoOPNF  : ' + FormatFloat('###,##0.00', CancelamentoOPNF));
    mRZ.Lines.Add('DescontoOPNF      : ' + FormatFloat('###,##0.00', DescontoOPNF));
    mRZ.Lines.Add('VendaLiquida      : ' + FormatFloat('###,##0.00', VendaLiquida));
    mRZ.Lines.Add('AcrescimoICMS     : ' + FormatFloat('###,##0.00', AcrescimoICMS));
    mRZ.Lines.Add('AcrescimoISSQN    : ' + FormatFloat('###,##0.00', AcrescimoISSQN));
    mRZ.Lines.Add('AcrescimoOPNF     : ' + FormatFloat('###,##0.00', AcrescimoOPNF));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ ICMS }');
    for I := 0 to ICMS.Count - 1 do
    begin
      mRZ.Lines.Add('Indice    : ' + ICMS[I].Indice);
      mRZ.Lines.Add('Tipo      : ' + ICMS[I].Tipo);
      mRZ.Lines.Add('Aliquota  : ' + FormatFloat('0.00', ICMS[I].Aliquota));
      mRZ.Lines.Add('Total     : ' + FormatFloat('###,##0.00', ICMS[I].Total));
    end;
    mRZ.Lines.Add('TotalICMS         : ' + FormatFloat('###,##0.00', TotalICMS));
    mRZ.Lines.Add('SubstituicaoTributariaICMS: ' + FormatFloat('###,##0.00', SubstituicaoTributariaICMS));
    mRZ.Lines.Add('IsentoICMS                : ' + FormatFloat('###,##0.00', IsentoICMS));
    mRZ.Lines.Add('NaoTributadoICMS          : ' + FormatFloat('###,##0.00', NaoTributadoICMS));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ ISSQN }');
    for I := 0 to ISSQN.Count - 1 do
    begin
      mRZ.Lines.Add('Indice    : ' + ISSQN[I].Indice);
      mRZ.Lines.Add('Tipo      : ' + ISSQN[I].Tipo);
      mRZ.Lines.Add('Aliquota  : ' + FormatFloat('0.00', ISSQN[I].Aliquota));
      mRZ.Lines.Add('Total     : ' + FormatFloat('###,##0.00', ISSQN[I].Total));
    end;
    mRZ.Lines.Add('TotalISSQN        : ' + FormatFloat('###,##0.00', TotalISSQN));
    mRZ.Lines.Add('SubstituicaoTributariaISSQN: ' + FormatFloat('###,##0.00', SubstituicaoTributariaISSQN));
    mRZ.Lines.Add('IsentoISSQN                : ' + FormatFloat('###,##0.00', IsentoISSQN));
    mRZ.Lines.Add('NaoTributadoISSQN          : ' + FormatFloat('###,##0.00', NaoTributadoISSQN));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ TOTALIZADORES NÃO FISCAIS }');
    for I := 0 to TotalizadoresNaoFiscais.Count - 1 do
    begin
      mRZ.Lines.Add('Indice     : ' + TotalizadoresNaoFiscais[I].Indice);
      mRZ.Lines.Add('Descrição  : ' + TotalizadoresNaoFiscais[I].Descricao);
      mRZ.Lines.Add('Forma Pagto: ' + TotalizadoresNaoFiscais[I].FormaPagamento);
      mRZ.Lines.Add('Total      : ' + FormatFloat('###,##0.00', TotalizadoresNaoFiscais[I].Total));
    end;
    mRZ.Lines.Add('TotalOperacaoNaoFiscal : ' + FormatFloat('###,##0.00', TotalOperacaoNaoFiscal));
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ RELATÓRIO GERENCIAL }');
    for I := 0 to RelatorioGerencial.Count - 1 do
    begin
      mRZ.Lines.Add('Indice     : ' + RelatorioGerencial[I].Indice);
      mRZ.Lines.Add('Descrição  : ' + RelatorioGerencial[I].Descricao);
    end;
    mRZ.Lines.Add('');

    mRZ.Lines.Add('{ MEIOS DE PAGAMENTO }');
    for I := 0 to MeiosDePagamento.Count - 1 do
    begin
      mRZ.Lines.Add('Indice     : ' + MeiosDePagamento[I].Indice);
      mRZ.Lines.Add('Descrição  : ' + MeiosDePagamento[I].Descricao);
      mRZ.Lines.Add('Total      : ' + FormatFloat('###,##0.00', MeiosDePagamento[I].Total));
    end;
    mRZ.Lines.Add('Total Troco : ' + FormatFloat('###,##0.00', TotalTroco));
  end;
  mRZ.Lines.Add('********  Retorno sem tratamento INICIO  ********');
  mRZ.Lines.Add(SRZ);
  mRZ.Lines.Add('********  Retorno sem tratamento FIM  ********');
end;

procedure TForm1.TestedeVinculado1Click(Sender: TObject);
Var
  cCupons, cFPG, cCOO: String;
  nCupons, J: Integer;
  tIni: TDateTime;
begin
  ACBrECF1.CarregaFormasPagamento;
  if ACBrECF1.FormasPagamento.Count < 1 then
    raise Exception.Create('Nenhuma Forma de Pagamento programada no ECF');

  cCupons := '1';
  if not InputQuery('Teste de Vinculado', 'Numero de Cupons a imprimir:', cCupons) then
    exit;

  cFPG := '02';
  if not InputQuery('Teste de Vinculado', 'Forma de Pagamento a utilizar:', cFPG) then
    exit;

  if ACBrECF1.AchaFPGIndice(cFPG) = nil then
    raise Exception.Create('Forma de pagamento ' + cFPG + ' não encontrada');

  nCupons := StrToIntDef(cCupons, 0);
  if nCupons < 1 then
    exit;

  For J := 1 to nCupons do
  begin
    tIni := now;
    mResp.Lines.Add('Iniciando Cupom: ' + IntToStr(nCupons) + ' - ' + DateTimeToStr(tIni));
    ACBrECF1.AbreCupom();
    mResp.Lines.Add('Cupom Aberto: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');

    ACBrECF1.VendeItem('7654321', 'TESTE DE PRODUTO, CUPOM: ' + IntToStrZero(nCupons, 3), 'NN', 1, 1, 0, 'UN');
    mResp.Lines.Add('Item Vendido: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');

    ACBrECF1.SubtotalizaCupom();
    mResp.Lines.Add('SubTotalizado: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');

    { Efetuando pagamento na FPG informada }
    ACBrECF1.EfetuaPagamento(cFPG, 1, 'TESTE DE VINCULADO', true);
    mResp.Lines.Add('Pagamento Efetuado: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');

    ACBrECF1.FechaCupom('TESTE DE CUPOM VINCULADO');
    mResp.Lines.Add('Finalizado Cupom: ' + FormatFloat('###.##', SecondSpan(tIni, now)) + ' segundos');

    cCOO := ACBrECF1.NumCupom;
    ACBrECF1.CupomVinculado(cCOO, cFPG, 1, frRelatorio.mRelat.Lines, 2);
    {
     ACBrECF1.AbreCupomVinculado(cCOO, cFPG, 1);
     mResp.Lines.Add('Abrindo Vinculado: '+FormatFloat('###.##',SecondSpan(tIni,Now))+' segundos') ;
     ACBrECF1.LinhaCupomVinculado( frRelatorio.mRelat.Lines.Text ) ;
     ACBrECF1.PulaLinhas  ;
     ACBrECF1.AcionaGuilhotina ;
     ACBrECF1.LinhaCupomVinculado( frRelatorio.mRelat.Lines.Text ) ;
     mResp.Lines.Add('Imprimindo Linhas Vinculado: '+FormatFloat('###.##',SecondSpan(tIni,Now))+' segundos') ;
     ACBrECF1.FechaRelatorio ;
     mResp.Lines.Add('Finalizado Vinculado: '+FormatFloat('###.##',SecondSpan(tIni,Now))+' segundos') ;
    }
    mResp.Lines.Add('---------------------------------');
    AtualizaMemos;
  end;
end;

procedure TForm1.CortaPapel1Click(Sender: TObject);
Var
  Resp: TModalResult;
begin
  Resp := MessageDlg('Corte Parcial ?', mtConfirmation, mbYesNoCancel, 0);

  if Resp = mrCancel then
    exit;

  ACBrECF1.CortaPapel((Resp = mrYes));
end;

procedure TForm1.Sangria1Click(Sender: TObject);
Var
  CNF, FPG, cValor: String;
  Valor: Double;
begin
  CNF := 'SANGRIA';
  FPG := 'DINHEIRO';
  cValor := '0';

  if not InputQuery('Sangria', 'Entre com o Valor da Sangria', cValor) then
    exit;
  Valor := StrToFloatDef(cValor, -1);
  if Valor <= 0 then
    exit;

  if not InputQuery('Sangria', 'Entre com a Descrição do Comprovante Não Fiscal', CNF) then
    exit;

  if not InputQuery('Sangria', 'Entre com a Descrição da Forrma de Pagamento', FPG) then
    exit;

  ACBrECF1.Sangria(Valor, 'TESTE DE SANGRIA', CNF, FPG);

  mResp.Lines.Add('Sangria: ' + FloatToStr(Valor) + ' ' + CNF + ' ' + FPG);
  AtualizaMemos;
end;

procedure TForm1.Suprimento1Click(Sender: TObject);
Var
  CNF, FPG, cValor: String;
  Valor: Double;
begin
  CNF := 'SUPRIMENTO';
  FPG := 'DINHEIRO';
  cValor := '0';

  if not InputQuery('Suprimento', 'Entre com o Valor do Suprimento', cValor) then
    exit;
  Valor := StrToFloatDef(cValor, -1);
  if Valor <= 0 then
    exit;

  if not InputQuery('Suprimento', 'Entre com a Descrição do Comprovante Não Fiscal', CNF) then
    exit;

  if not InputQuery('Suprimento', 'Entre com a Descrição da Forrma de Pagamento', FPG) then
    exit;

  ACBrECF1.Suprimento(Valor, 'TESTE DE SUPRIMENTO', CNF, FPG);

  mResp.Lines.Add('Suprimento: ' + FloatToStr(Valor) + ' ' + CNF + ' ' + FPG);
  AtualizaMemos;
end;

procedure TForm1.edOperadorChange(Sender: TObject);
begin
  ACBrECF1.Operador := edOperador.Text;
end;

procedure TForm1.edMsgTrabalhandoChange(Sender: TObject);
begin
  ACBrECF1.MsgTrabalhando := edMsgTrabalhando.Text;
end;

procedure TForm1.Colunas1Click(Sender: TObject);
begin
  mResp.Lines.Add('Colunas: ( ' + IntToStr(ACBrECF1.Colunas) + ' )');
  AtualizaMemos;
end;

procedure TForm1.ConsultaRegistradorECF1Click(Sender: TObject);
var
  Indice: String;
  Linhas: String;
begin
  if not InputQuery('Consulta Registrador ECF', 'Entre com o Indice da Informação:', Indice) then
    exit;

  Linhas := ACBrECF1.RetornaInfoECF(Indice);

  mResp.Lines.Add('Informação: ' + Linhas);
  AtualizaMemos();
end;

procedure TForm1.EstornaCCD1Click(Sender: TObject);
var
   Estor : Integer ;
begin
   Estor := ACBrECF1.EstornaCCD ;
   mResp.Lines.Add( 'EstornaCCD  - Estornados: '+IntToStr(Estor) );

   AtualizaMemos;
end;

procedure TForm1.EstornaMeiodePagamento1Click(Sender: TObject);
begin
  frPagamento.Estado := 'Estorno';
  frPagamento.Show;
end;

procedure TForm1.Decimais1Click(Sender: TObject);
begin
  mResp.Lines.Add('Decimais QTD: ( ' + IntToStr(ACBrECF1.DecimaisQtd) + ' )');
  mResp.Lines.Add('Decimais Preço Unit: ( ' + IntToStr(ACBrECF1.DecimaisPreco) + ' )');
  AtualizaMemos;

end;

procedure TForm1.DeCodificaTexto1Click(Sender: TObject);
Var
  Operacao: String;
  Texto: String;
  Resposta: String;
begin
  Operacao := 'C';
  if not InputQuery('De/Codificacao', 'Informe a operação a ser realizada "C", "D" ou "V"', Operacao) then
    exit;

  Texto := 'ACBr';
  if not InputQuery('Texto a ser de/codificado ou verificado', 'Informe o texto a ser decodificado', Texto) then
    exit;

  ACBrECF1.DeCodificaTexto(Operacao[1], Texto, Resposta);

  mResp.Lines.Add('Resposta: ' + Resposta);
  AtualizaMemos;
end;

procedure TForm1.AchaAliquotaporIndice1Click(Sender: TObject);
var
  Aliquota: TACBrECFAliquota;
  Indice: String;
begin
  ACBrECF1.LerTotaisAliquota;

  if not InputQuery('Acha Aliquota por Indice', 'Entre com o Indice:', Indice) then
    exit;

  Aliquota := ACBrECF1.AchaICMSIndice(Indice);

  if Aliquota <> Nil then
  begin
    mResp.Lines.Add('Indice  : ' + Aliquota.Indice);
    mResp.Lines.Add('Aliquota: ' + FormatFloat('###,##0.00', Aliquota.Aliquota));
    mResp.Lines.Add('Valor atual do totalizador R$ ' + FormatFloat('###,##0.00', Aliquota.Total));
  end
  else
    mResp.Lines.Add('Indice (' + Indice + ') não encontrado!');

  AtualizaMemos();
end;

procedure TForm1.AchaAliquotaporValor1Click(Sender: TObject);
var
  Aliquota: TACBrECFAliquota;
  ValorStr: String;
  Valor: Double;
begin
  ACBrECF1.LerTotaisAliquota;

  if not InputQuery('Acha Aliquota por Valor', 'Entre com o Valor:', ValorStr) then
    exit;

  Valor := StrToFloatDef(ValorStr, 0);
  Aliquota := ACBrECF1.AchaICMSAliquota(Valor);

  if Aliquota <> Nil then
  begin
    mResp.Lines.Add('Indice  : ' + Aliquota.Indice);
    mResp.Lines.Add('Aliquota: ' + FormatFloat('###,##0.00', Aliquota.Aliquota));
    mResp.Lines.Add('Valor atual do totalizador R$ ' + FormatFloat('###,##0.00', Aliquota.Total));
  end
  else
    mResp.Lines.Add('Aliquota (' + FloatToStr(Valor) + ') não encontrada!');

  AtualizaMemos();

end;

procedure TForm1.AcharMeioPagamentoporIndice1Click(Sender: TObject);
var
  FormaPagto: TACBrECFFormaPagamento;
  Indice: String;
begin
  ACBrECF1.LerTotaisFormaPagamento;

  if not InputQuery('Acha Forma Pagamento por Indice', 'Entre com o Indice:', Indice) then
    exit;

  FormaPagto := ACBrECF1.AchaFPGIndice(Indice);

  if FormaPagto <> Nil then
  begin
    mResp.Lines.Add('Indice   : ' + FormaPagto.Indice);
    mResp.Lines.Add('Descrição: ' + FormaPagto.Descricao);
    mResp.Lines.Add('Valor atual do totalizador R$ ' + FormatFloat('###,##0.00', FormaPagto.Total));
  end
  else
    mResp.Lines.Add('Indice (' + Indice + ') não encontrado!');

  AtualizaMemos();
end;

procedure TForm1.AchaTotalizadorNaoTributadoIndice1Click(Sender: TObject);
var
  TotalizadorNaoTributado: TACBrECFTotalizadorNaoTributado;
  Indice: String;
begin
  ACBrECF1.CarregaTotalizadoresNaoTributados;
  try
    ACBrECF1.LerTotaisTotalizadoresNaoTributados;
  except
  end;

  Indice := '';
  if not InputQuery('Acha Totalizador Nao Tributado  por Indice', 'Entre com o Indice:', Indice) then
    exit;

  TotalizadorNaoTributado := ACBrECF1.AchaTotalizadorNaoTributadoIndice(Indice);

  if TotalizadorNaoTributado <> Nil then
  begin
    mResp.Lines.Add('Indice: ' + TotalizadorNaoTributado.Indice);
    mResp.Lines.Add('Tipo..: ' + TotalizadorNaoTributado.Tipo);
    mResp.Lines.Add('Valor atual do totalizador R$ ' + FormatFloat('###,##0.00', TotalizadorNaoTributado.Total));
  end
  else
    mResp.Lines.Add('Indice (' + Indice + ') não encontrado!');

  AtualizaMemos();
end;

procedure TForm1.AjustaACBrNFe;
begin
  PrepararImpressaoNFCe;
end;

procedure TForm1.AjustaACBrSAT;
begin
  with ACBrSAT1 do
  begin
    Modelo := TACBrSATModelo(cbxModeloSAT.ItemIndex);
    ArqLOG := edLogSAT.Text;
    NomeDLL := edNomeDLL.Text;
    Config.ide_numeroCaixa := seNumeroCaixa.Value;
    Config.ide_tpAmb := TpcnTipoAmbiente(cbxAmbiente.ItemIndex);
    Config.ide_CNPJ := edtSwHCNPJ.Text;
    Config.emit_CNPJ := edtEmitCNPJ.Text;
    Config.emit_IE := edtEmitIE.Text;
    Config.emit_IM := edtEmitIM.Text;
    Config.emit_cRegTrib := TpcnRegTrib(cbxRegTributario.ItemIndex);
    Config.emit_cRegTribISSQN := TpcnRegTribISSQN(cbxRegTribISSQN.ItemIndex);
    Config.emit_indRatISSQN := TpcnindRatISSQN(cbxIndRatISSQN.ItemIndex);
    Config.PaginaDeCodigo := sePagCod.Value;
    Config.EhUTF8 := cbxUTF8.Checked;
    Config.infCFe_versaoDadosEnt := StrToFloat(sfeVersaoEnt.Text);
    ConfigArquivos.SalvarCFe := cbxSalvarCFe.Checked;
    ConfigArquivos.SalvarCFeCanc := cbxSalvarCFe.Checked;
  end;

  PrepararImpressaoSAT;
end;

procedure TForm1.AcharMeiodePagametoporDescrio1Click(Sender: TObject);
var
  FormaPagto: TACBrECFFormaPagamento;
  Descricao: String;
begin
  ACBrECF1.LerTotaisFormaPagamento;

  if not InputQuery('Acha Forma Pagamento por Descrição', 'Entre com a descrição:', Descricao) then
    exit;

  FormaPagto := ACBrECF1.AchaFPGDescricao(Descricao);

  if FormaPagto <> Nil then
  begin
    mResp.Lines.Add('Indice   : ' + FormaPagto.Indice);
    mResp.Lines.Add('Descrição: ' + FormaPagto.Descricao);
    mResp.Lines.Add('Valor atual do totalizador R$ ' + FormatFloat('###,##0.00', FormaPagto.Total));
  end
  else
    mResp.Lines.Add('Forma de Pagamento (' + Descricao + ') não encontrada!');

  AtualizaMemos();

end;

procedure TForm1.AchaCNFporIndice1Click(Sender: TObject);
var
  CNF: TACBrECFComprovanteNaoFiscal;
  Indice: String;
begin
  ACBrECF1.LerTotaisComprovanteNaoFiscal;

  if not InputQuery('Acha CNF por Indice', 'Entre com o Indice:', Indice) then
    exit;

  CNF := ACBrECF1.AchaCNFIndice(Indice);

  if CNF <> Nil then
  begin
    mResp.Lines.Add('Indice   : ' + CNF.Indice);
    mResp.Lines.Add('CON      : ' + IntToStrZero(CNF.Contador, 4));
    mResp.Lines.Add('Descrição: ' + CNF.Descricao);
    mResp.Lines.Add('Valor atual do totalizador R$ ' + FormatFloat('###,##0.00', CNF.Total));
  end
  else
    mResp.Lines.Add('Indice (' + Indice + ') não encontrado!');

  AtualizaMemos();
end;

procedure TForm1.AchaCNFporDescrio1Click(Sender: TObject);
var
  CNF: TACBrECFComprovanteNaoFiscal;
  Descricao: String;
begin
  ACBrECF1.LerTotaisComprovanteNaoFiscal;

  if not InputQuery('Acha CNF por Descrição', 'Entre com o descricao:', Descricao) then
    exit;

  CNF := ACBrECF1.AchaCNFDescricao(Descricao);

  if CNF <> Nil then
  begin
    mResp.Lines.Add('Indice   : ' + CNF.Indice);
    mResp.Lines.Add('CON      : ' + IntToStrZero(CNF.Contador, 4));
    mResp.Lines.Add('Descrição: ' + CNF.Descricao);
    mResp.Lines.Add('Valor atual do totalizador R$ ' + FormatFloat('###,##0.00', CNF.Total));
  end
  else
    mResp.Lines.Add('CNF (' + Descricao + ') não encontrado!');

  AtualizaMemos();
end;

procedure TForm1.AchaRGporIndice1Click(Sender: TObject);
var
  RG: TACBrECFRelatorioGerencial;
  Indice: String;
begin
  ACBrECF1.CarregaRelatoriosGerenciais;

  if not InputQuery('Acha Relatório Gerencial por Indice', 'Entre com o Indice:', Indice) then
    exit;

  RG := ACBrECF1.AchaRGIndice(Indice);

  if RG <> Nil then
  begin
    mResp.Lines.Add('Indice   : ' + RG.Indice);
    mResp.Lines.Add('Descrição: ' + RG.Descricao);
    mResp.Lines.Add('CER:     : ' + FormatFloat('0000', RG.Contador));
  end
  else
    mResp.Lines.Add('Indice (' + Indice + ') não encontrado!');

  AtualizaMemos();
end;

procedure TForm1.AchaRGporDescrio1Click(Sender: TObject);
var
  RG: TACBrECFRelatorioGerencial;
  Descricao: String;
begin
  ACBrECF1.CarregaRelatoriosGerenciais;

  if not InputQuery('Acha Relatório Gerencial por Indice', 'Entre com o Indice:', Descricao) then
    exit;

  RG := ACBrECF1.AchaRGDescricao(Descricao);

  if RG <> Nil then
  begin
    mResp.Lines.Add('Indice   : ' + RG.Indice);
    mResp.Lines.Add('Descrição: ' + RG.Descricao);
    mResp.Lines.Add('CER:     : ' + FormatFloat('0000', RG.Contador));
  end
  else
    mResp.Lines.Add('Relatório Gerencial (' + Descricao + ') não encontrado!');

  AtualizaMemos();
end;

procedure TForm1.PorCOO2Click(Sender: TObject);
Var
  Linhas: TStringList;
  cCOO: String;
  I, nCOO: Integer;
begin
  cCOO := '0';

  if not InputQuery('Captura da MFD', 'Entre com o COO que deseja capturar:', cCOO) then
    exit;

  nCOO := StrToIntDef(cCOO, -1);

  if nCOO < 0 then
    exit;

  Linhas := TStringList.Create;
  try
    ACBrECF1.LeituraMFDSerial(nCOO, nCOO, Linhas);

    For I := 0 to Linhas.Count - 1 do
      mResp.Lines.Add(Linhas[I]);
  finally
    Linhas.Free;
  end;
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.PorDatadeMovimento1Click(Sender: TObject);
Var
  Linhas: TStringList;
  cData: String;
  dData: TDateTime;
  I: Integer;
begin
  cData := FormatDateTime('dd/mm/yy', now);

  if not InputQuery('Captura da MFD', 'Entre com o a Data do Movimento (DD/MM/AA):', cData) then
    exit;
  try
    dData := StrToDateTime(StringReplace(cData, '/', DateSeparator, [rfReplaceAll]));
  except
    exit;
  end;

  Linhas := TStringList.Create;
  try
    ACBrECF1.LeituraMFDSerial(dData, dData, Linhas);

    For I := 0 to Linhas.Count - 1 do
      mResp.Lines.Add(Linhas[I]);
  finally
    Linhas.Free;
  end;
  mResp.Lines.Add('---------------------------------');

end;

procedure TForm1.PorCOO3Click(Sender: TObject);
Var
  Arquivo: String;
  cCOOIni, cCOOFim: String;
  nCOOIni, nCOOFim: Integer;
begin
  Arquivo := 'c:\temp\teste.txt';
  if not InputQuery('Espelho da MFD DLL', 'Nome Arquivo:', Arquivo) then
    exit;

  cCOOIni := '0';
  cCOOFim := '0';

  if not InputQuery('Espelho da MFD DLL', 'Entre com o COO Inicial:', cCOOIni) then
    exit;
  nCOOIni := StrToIntDef(cCOOIni, -1);
  if nCOOIni < 0 then
    exit;

  if not InputQuery('Espelho da MFD DLL', 'Entre com o COO Final:', cCOOFim) then
    exit;
  nCOOFim := StrToIntDef(cCOOFim, -1);
  if nCOOFim < 0 then
    exit;

  ACBrECF1.EspelhoMFD_DLL(nCOOIni, nCOOFim, Arquivo);
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.PorPeriodo2Click(Sender: TObject);
Var
  Arquivo: String;
  cDatIni, cDatFim: String;
  dDatIni, dDatFim: TDateTime;
begin
  Arquivo := 'c:\temp\teste.txt';
  if not InputQuery('Espelho da MFD DLL', 'Nome Arquivo:', Arquivo) then
    exit;

  cDatIni := '01/' + FormatDateTime('mm/yy', now);
  cDatFim := FormatDateTime('dd/mm/yy', now);

  if not InputQuery('Espelho da MFD DLL', 'Entre com o a Data Inicial (DD/MM/AA):', cDatIni) then
    exit;
  try
    dDatIni := StrToDateTime(StringReplace(cDatIni, '/', DateSeparator, [rfReplaceAll]));
  except
    exit;
  end;

  if not InputQuery('Captura da MFD', 'Entre com o a Data Final (DD/MM/AA):', cDatFim) then
    exit;
  try
    dDatFim := StrToDateTime(StringReplace(cDatFim, '/', DateSeparator, [rfReplaceAll]));
  except
    exit
  end;

  ACBrECF1.EspelhoMFD_DLL(dDatIni, dDatFim, Arquivo);
  mResp.Lines.Add('---------------------------------');

end;

procedure TForm1.UsuarioAual1Click(Sender: TObject);
begin
  mResp.Lines.Add('UsuarioAtual: (' + ACBrECF1.UsuarioAtual + ')');
  AtualizaMemos;
end;

procedure TForm1.PorCOO4Click(Sender: TObject);
Var
  Arquivo: String;
  cCOOIni, cCOOFim: String;
  nCOOIni, nCOOFim: Integer;
begin
  Arquivo := 'c:\temp\teste.txt';
  if not InputQuery('Arquivo da MFD DLL', 'Nome Arquivo:', Arquivo) then
    exit;

  cCOOIni := '0';
  cCOOFim := '0';

  if not InputQuery('Arquivo da MFD DLL', 'Entre com o COO Inicial:', cCOOIni) then
    exit;
  nCOOIni := StrToIntDef(cCOOIni, -1);
  if nCOOIni < 0 then
    exit;

  if not InputQuery('Arquivo da MFD DLL', 'Entre com o COO Final:', cCOOFim) then
    exit;
  nCOOFim := StrToIntDef(cCOOFim, -1);
  if nCOOFim < 0 then
    exit;

  ACBrECF1.ArquivoMFD_DLL(nCOOIni, nCOOFim, Arquivo);
  mResp.Lines.Add('---------------------------------');
end;

procedure TForm1.PorPeriodo3Click(Sender: TObject);
Var
  Arquivo: String;
  cDatIni, cDatFim: String;
  dDatIni, dDatFim: TDateTime;
begin
  Arquivo := 'c:\temp\teste.txt';
  if not InputQuery('Arquivo da MFD DLL', 'Nome Arquivo:', Arquivo) then
    exit;

  cDatIni := '01/' + FormatDateTime('mm/yy', now);
  cDatFim := FormatDateTime('dd/mm/yy', now);

  if not InputQuery('Arquivo da MFD DLL', 'Entre com o a Data Inicial (DD/MM/AA):', cDatIni) then
    exit;
  try
    dDatIni := StrToDateTime(StringReplace(cDatIni, '/', DateSeparator, [rfReplaceAll]));
  except
    exit;
  end;

  if not InputQuery('Arquivo da MFD', 'Entre com o a Data Final (DD/MM/AA):', cDatFim) then
    exit;
  try
    dDatFim := StrToDateTime(StringReplace(cDatFim, '/', DateSeparator, [rfReplaceAll]));
  except
    exit
  end;

  ACBrECF1.ArquivoMFD_DLL(dDatIni, dDatFim, Arquivo);
  mResp.Lines.Add('---------------------------------');

end;

procedure TForm1.NumGNF1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.GNF: (' + ACBrECF1.NumGNF + ')');
  AtualizaMemos;
end;

procedure TForm1.NumGNFC1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.GNFC: (' + ACBrECF1.NumGNFC + ')');
  AtualizaMemos;
end;

procedure TForm1.NumGRG1Click(Sender: TObject);
begin
  mResp.Lines.Add('Num.GRG: (' + ACBrECF1.NumGRG + ')');
  AtualizaMemos;
end;

procedure TForm1.speLinBufChange(Sender: TObject);
begin
  ACBrECF1.MaxLinhasBuffer := speLinBuf.Value;
end;

procedure TForm1.NumSerieMFDClick(Sender: TObject);
begin
  mResp.Lines.Add('N.Série MFD: (' + ACBrECF1.NumSerieMFD + ')');
  AtualizaMemos;
end;

procedure TForm1.mModeloStrClick(Sender: TObject);
begin
  mResp.Lines.Add('ModeloStr: (' + ACBrECF1.ModeloStr + ')');
  AtualizaMemos;
end;

procedure TForm1.btnArqMFDNovoClick(Sender: TObject);
var
  PathArquivo: String;
begin
  dlgDialogoSalvar.DefaultExt := '.mfd';
  dlgDialogoSalvar.Filter := 'Arquivos binários MF|*.mfd';

  if dlgDialogoSalvar.Execute then
  begin
    PathArquivo := dlgDialogoSalvar.FileName;
    ACBrECF1.PafMF_ArqMFD_Binario(PathArquivo);

    // será gerado o arquivo bináio e o arquivo .txt com a assinatura EAD

    ShowMessage(Format('Arquivo MFD gerado com sucesso em:'#13#10' "%s"', [PathArquivo]));
  end;
end;

procedure TForm1.btnArqMFNovoClick(Sender: TObject);
var
  PathArquivo: String;
begin
  dlgDialogoSalvar.DefaultExt := '.mf';
  dlgDialogoSalvar.Filter := 'Arquivos binários MF|*.mf';

  if dlgDialogoSalvar.Execute then
  begin
    PathArquivo := dlgDialogoSalvar.FileName;
    ACBrECF1.PafMF_ArqMFD_Binario(PathArquivo);

    // será gerado o arquivo bináio e o arquivo .txt com a assinatura EAD

    ShowMessage(Format('Arquivo MF gerado com sucesso em:'#13#10' "%s"', [PathArquivo]));
  end;
end;

procedure TForm1.btnMenuFiscalConfigPAFECFClick(Sender: TObject);
//var
//  Parametros: TACBrECFInfoPaf;
begin
  // para ERs mais novas usar, onde 'X' é o perfil de requisitos aplicado
  ACBrECF1.PafMF_RelParametrosConfiguracao('X');

  {
   if Assigned(ACBrECF1.AAC) then
   ACBrECF1.PafMF_RelParametrosConfiguracao(ACBrECF1.AAC.IdentPAF.Paf)
   else
   begin
   Parametros := TACBrECFInfoPaf.Create;
   try
   Parametros.TipoFuncionamento   := tpfEmRede;
   Parametros.TipoDesenvolvimento := tpdExclusivoTerceirizado;
   Parametros.IntegracaoPAFECF    := tpiRetaguarda;

   Parametros.RealizaPreVenda              := True;
   Parametros.RealizaDAVECF                := True;
   Parametros.RealizaDAVNaoFiscal          := True;
   Parametros.RealizaDAVOS                 := True;
   Parametros.DAVConfAnexoII               := True;
   Parametros.RealizaLancamentoMesa        := True;
   Parametros.IndiceTecnicoProd            := True;
   Parametros.BarSimilarECFRestaurante     := True;
   Parametros.BarSimilarECFComum           := True;
   Parametros.BarSimilarBalanca            := True;
   Parametros.UsaImpressoraNaoFiscal       := True;
   Parametros.DAVDiscrFormula              := True;
   Parametros.ImpedeVendaVlrZero           := True;
   Parametros.AcumulaVolumeDiario          := True;
   Parametros.ArmazenaEncerranteIniFinal   := True;
   Parametros.EmiteContrEncerrAposREDZLEIX := True;
   Parametros.IntegradoComBombas           := True;
   Parametros.CriaAbastDivergEncerrante    := True;
   Parametros.CadastroPlacaBomba           := True;
   Parametros.TransportePassageiro         := True;
   Parametros.TotalizaValoresLista         := True;
   Parametros.TransfPreVenda               := True;
   Parametros.TransfDAV                    := True;
   Parametros.RecompoeGT                   := True;
   Parametros.EmitePED                     := True;
   Parametros.CupomMania                   := True;
   Parametros.MinasLegal                   := True;
   Parametros.NotaLegalDF                  := True;
   Parametros.ParaibaLegal                 := True;
   Parametros.TrocoEmCartao                := True;

   ACBrECF1.PafMF_RelParametrosConfiguracao(Parametros);
   finally
   Parametros.Free;
   end;
   end;
  }
end;

procedure TForm1.btnMenuFiscalLMFCClick(Sender: TObject);
var
  PathArquivo: string;
begin
  if chkMenuFiscalGerarArquivo.Checked then
  begin
    dlgDialogoSalvar.DefaultExt := '.txt';
    dlgDialogoSalvar.Filter := 'Arquivos texto|*.txt';

    if dlgDialogoSalvar.Execute then
    begin
      PathArquivo := dlgDialogoSalvar.FileName;

      if chkMenuFiscalCotepe1704.Checked then
      begin
        if pgcMenuFiscalTipo.ActivePageIndex = 0 then
          ACBrECF1.PafMF_LMFC_Cotepe1704(edtDtInicial.Date, edtDtFinal.Date, PathArquivo)
        else
          ACBrECF1.PafMF_LMFC_Cotepe1704(edtCOOInicial.Value, edtCOOFinal.Value, PathArquivo);
      end
      else
      begin
        if pgcMenuFiscalTipo.ActivePageIndex = 0 then
          ACBrECF1.PafMF_LMFC_Espelho(edtDtInicial.Date, edtDtFinal.Date, PathArquivo)
        else
          ACBrECF1.PafMF_LMFC_Espelho(edtCOOInicial.Value, edtCOOFinal.Value, PathArquivo);
      end;

      ShowMessage(Format('Arquivo gerado com sucesso em:'#13#10' "%s"', [PathArquivo]));
    end;
  end
  else
  begin
    if pgcMenuFiscalTipo.ActivePageIndex = 0 then
      ACBrECF1.PafMF_LMFC_Impressao(edtDtInicial.Date, edtDtFinal.Date)
    else
      ACBrECF1.PafMF_LMFC_Impressao(edtCOOInicial.Value, edtCOOFinal.Value);
  end;
end;

procedure TForm1.btnMenuFiscalLMFSClick(Sender: TObject);
var
  PathArquivo: string;
begin
  if chkMenuFiscalGerarArquivo.Checked then
  begin
    dlgDialogoSalvar.DefaultExt := '.txt';
    dlgDialogoSalvar.Filter := 'Arquivos texto|*.txt';

    if dlgDialogoSalvar.Execute then
    begin
      PathArquivo := dlgDialogoSalvar.FileName;
      if pgcMenuFiscalTipo.ActivePageIndex = 0 then
        ACBrECF1.PafMF_LMFS_Espelho(edtDtInicial.Date, edtDtFinal.Date, PathArquivo)
      else
        ACBrECF1.PafMF_LMFS_Espelho(edtCOOInicial.Value, edtCOOFinal.Value, PathArquivo);

      ShowMessage(Format('Arquivo gerado com sucesso em:'#13#10' "%s"', [PathArquivo]));
    end;
  end
  else
  begin
    if pgcMenuFiscalTipo.ActivePageIndex = 0 then
      ACBrECF1.PafMF_LMFS_Impressao(edtDtInicial.Date, edtDtFinal.Date)
    else
      ACBrECF1.PafMF_LMFS_Impressao(edtCOOInicial.Value, edtCOOFinal.Value);
  end;
end;

procedure TForm1.btnMenuFiscalLXClick(Sender: TObject);
begin
  ACBrECF1.PafMF_LX_Impressao;
end;

procedure TForm1.btnMenuFiscalMFDArqClick(Sender: TObject);
var
  PathArquivo: string;
begin
  dlgDialogoSalvar.DefaultExt := '.txt';
  dlgDialogoSalvar.Filter := 'Arquivos text|*.txt';

  if dlgDialogoSalvar.Execute then
  begin
    PathArquivo := dlgDialogoSalvar.FileName;

    if pgcMenuFiscalTipo.ActivePageIndex = 0 then
      ACBrECF1.PafMF_MFD_Cotepe1704(edtDtInicial.Date, edtDtFinal.Date, PathArquivo)
    else
      ACBrECF1.PafMF_MFD_Cotepe1704(edtCOOInicial.Value, edtCOOFinal.Value, PathArquivo);

    ShowMessage(Format('Arquivo gerado com sucesso em:'#13#10' "%s"', [PathArquivo]));
  end;
end;

procedure TForm1.btnMenuFiscalMFDEspelhoClick(Sender: TObject);
var
  PathArquivo: string;
begin
  dlgDialogoSalvar.DefaultExt := '.txt';
  dlgDialogoSalvar.Filter := 'Arquivos text|*.txt';

  if dlgDialogoSalvar.Execute then
  begin
    PathArquivo := dlgDialogoSalvar.FileName;

    if pgcMenuFiscalTipo.ActivePageIndex = 0 then
      ACBrECF1.PafMF_MFD_Espelho(edtDtInicial.Date, edtDtFinal.Date, PathArquivo)
    else
      ACBrECF1.PafMF_MFD_Espelho(edtCOOInicial.Value, edtCOOFinal.Value, PathArquivo);

    ShowMessage(Format('Arquivo gerado com sucesso em:'#13#10' "%s"', [PathArquivo]));
  end;
end;

procedure TForm1.btnMenuFiscalRelDAVEmitidosClick(Sender: TObject);
var
  DAVs: TACBrECFDAVs;
  I: Integer;
const
  TipoDAV: array [0 .. 1] of string = ('PEDIDO', 'ORCAMENTO');
  Valores: array [0 .. 3] of Double = (1.00, 2.00, 3.50, 10.45);
  Datas: array [0 .. 4] of string = ('30/12/2000', '01/01/2011', '25/02/2010', '04/02/2011', '13/04/2011');
begin
  DAVs := TACBrECFDAVs.Create;
  try
    for I := 1 to 25 do
    begin
      with DAVs.New do
      begin
        Numero := Format('%10.10d', [I]);
        COO_Dav := RandomRange(0, 999999);
        COO_Cupom := RandomRange(0, 999999);
        Titulo := RandomFrom(TipoDAV);
        DtEmissao := StrToDate(RandomFrom(Datas));
        Valor := RandomFrom(Valores)
      end;
    end;

    ACBrECF1.PafMF_RelDAVEmitidos(DAVs, 'REFERENCIA: 00/00/0000 A 00/00/0000', 0);
  finally
    DAVs.Free;
  end;
end;

procedure TForm1.btnMenuFiscalRelIdentPAFECFClick(Sender: TObject);
var
  IdentPAF: TACBrECFIdentificacaoPAF;
  I: Integer;
begin
  // Se está usando o AAC, basta informar o Objeto IdentPAF //
  // Se NAO está usando o AAC, o Objeto IdentPAF deve ser instânciado e populado //
  if Assigned(ACBrECF1.AAC) then
    ACBrECF1.PafMF_RelIdentificacaoPafECF(ACBrECF1.AAC.IdentPAF, 0)
  else
  begin
    IdentPAF := TACBrECFIdentificacaoPAF.Create;
    try
      IdentPAF.NumeroLaudo := 'ABC1234567890'; // retirar do laudo
      IdentPAF.VersaoER := '01.06'; // retirar do laudo

      // preencher dados da empresa conforme o que foi informado no laudo de análise
      IdentPAF.Empresa.RazaoSocial := 'Razao social Empresa';
      IdentPAF.Empresa.CNPJ := '01.222.333/00001-99';
      IdentPAF.Empresa.ENDERECO := 'Rua da Felicidade, 1';
      IdentPAF.Empresa.Cidade := 'SAO PAULO';
      IdentPAF.Empresa.UF := 'SP';
      IdentPAF.Empresa.Cep := '99.999-999';
      IdentPAF.Empresa.Telefone := '(99)1111.2222';
      IdentPAF.Empresa.Contato := 'Nome do Contato';

      IdentPAF.Paf.Nome := 'DemoECF'; // preencher conforme o laudo
      IdentPAF.Paf.Versao := 'v01.01.01'; // versão atual do aplicativo
      IdentPAF.Paf.PrincipalExe.Nome := UpperCase(ExtractFileName(ParamStr(0)));
      IdentPAF.Paf.PrincipalExe.MD5 := StringOfChar('X', 32); // md5 atual do aplicativo

      IdentPAF.ArquivoListaAutenticados.Nome := 'lista_arquivos.txt'; // nome do arquivo contendo a lista de autenticados
      IdentPAF.ArquivoListaAutenticados.MD5 := 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'; // md5 do arquivo, mesmo que vai impresso nos cupons

      // adicionar os arquivos adicionados ao arquivo da lista de autenticados
      for I := 1 to 5 do
      begin
        with IdentPAF.OutrosArquivos.New do
        begin
          Nome := Format('Arquivo %3.3d', [I]);
          MD5 := StringOfChar('X', 32);
        end;
      end;

      // ecfs autorizados para funcionamento na máquina
      IdentPAF.ECFsAutorizados.Clear;
      for I := 1 to 3 do
      begin
        with IdentPAF.ECFsAutorizados.New do
          NumeroSerie := StringOfChar('A', 15);
      end;

      ACBrECF1.PafMF_RelIdentificacaoPafECF(IdentPAF, 0);
    finally
      IdentPAF.Free;
    end;
  end;
end;

procedure TForm1.btnMenuFiscalRelMeiosPagtoClick(Sender: TObject);
var
  FormasPagamento: TACBrECFFormasPagamento;
  I: Integer;
const
  arrayTipoDoc: array [0 .. 2] of String = ('Cupom Fiscal', 'Compr. Não Fiscal', 'Nota Fiscal');
  arrayDescrFormaPagto: array [0 .. 3] of string = ('Dinheiro', 'Cheque', 'Cartão Crédito', 'Cartão Débito');
  arrayDataLancamento: array [0 .. 4] of String = ('01/01/2010', '31/12/2010', '04/05/2011', '02/01/2010', '03/05/2011');
  arrayValores: array [0 .. 4] of Double = (10.56, 14.23, 0.00, 12.00, 1.20);
begin
  FormasPagamento := TACBrECFFormasPagamento.Create;
  try
    for I := 1 to 25 do
    begin
      with FormasPagamento.New do
      begin
        Descricao := RandomFrom(arrayDescrFormaPagto);
        Data := StringToDateTime(RandomFrom(arrayDataLancamento), 'dd/mm/yyyy');
        Total := RandomFrom(arrayValores);
        TipoDoc := RandomFrom(arrayTipoDoc);
      end;
    end;

    ACBrECF1.PafMF_RelMeiosPagamento(FormasPagamento, 'PERIODO DE 01/01/2000 A 31/12/2000', 0);
  finally
    FormasPagamento.Free;
  end;
end;

procedure TForm1.btnMenuFiscalNotaPaulistaClick(Sender: TObject);
var
  DirArquivos: string;
begin
  DirArquivos := ExtractFilePath(ParamStr(0)) + 'CAT52';
  if not DirectoryExists(DirArquivos) then
    ForceDirectories(DirArquivos);

  ACBrECF1.PafMF_GerarCAT52(edtDtInicial.Date, edtDtFinal.Date, DirArquivos);

  ShowMessage(Format('Arquivos gerados com sucesso em:'#13#10' "%s"', [DirArquivos]));
end;

procedure TForm1.chAACFlushClick(Sender: TObject);
begin
  ACBrAAC1.EfetuarFlush := chAACFlush.Checked;
end;

procedure TForm1.chAACUsarClick(Sender: TObject);
Var
  OldAtivo: Boolean;
begin
  OldAtivo := ACBrECF1.Ativo;
  try
    try
      ACBrECF1.Desativar;

      if chAACUsar.Checked then
        ACBrECF1.AAC := ACBrAAC1
      else
        ACBrECF1.AAC := nil;
    except
      chAACUsar.OnClick := nil;
      chAACUsar.Checked := Assigned(ACBrECF1.AAC);
      chAACUsar.OnClick := chAACUsarClick;

      raise;
    end;
  finally
    ACBrECF1.Ativo := OldAtivo;
  end;
end;

procedure TForm1.chArredondamentoItemMFDClick(Sender: TObject);
begin
  ACBrECF1.ArredondaItemMFD := chArredondamentoItemMFD.Checked;
end;

procedure TForm1.chBarrasImprimeTextoClick(Sender: TObject);
begin
  ACBrECF1.ConfigBarras.MostrarCodigo := chBarrasImprimeTexto.Checked;
end;

procedure TForm1.chIgnorarTagsFormatacaoClick(Sender: TObject);
begin
  ACBrECF1.IgnorarTagsFormatacao := chIgnorarTagsFormatacao.Checked;
end;

procedure TForm1.chProcessMessagesClick(Sender: TObject);
begin
  ACBrECF1.Device.ProcessMessages := chProcessMessages.Checked;
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
begin
  if ACBrECF1.Estado <> estRelatorio then
  begin
    ACBrECF1.CorrigeEstadoErro;
    ACBrECF1.AbreRelatorioGerencial;
  end;

  ACBrECF1.ConfigBarras.LarguraLinha := speBarrasLargura.Value;
  ACBrECF1.ConfigBarras.Altura := speBarrasAltura.Value;
  ACBrECF1.ConfigBarras.MostrarCodigo := chBarrasImprimeTexto.Checked;
  ACBrECF1.IgnorarTagsFormatacao := chIgnorarTagsFormatacao.Checked;

  ACBrECF1.LinhaRelatorioGerencial(MemoTesteTags.Text);
end;

procedure TForm1.speBarrasLarguraChange(Sender: TObject);
begin
  ACBrECF1.ConfigBarras.LarguraLinha := speBarrasLargura.Value;
end;

procedure TForm1.speBarrasAlturaChange(Sender: TObject);
begin
  ACBrECF1.ConfigBarras.Altura := speBarrasAltura.Value;
end;

procedure TForm1.chbCupomManiaClick(Sender: TObject);
begin
  ACBrECF1.InfoRodapeCupom.CupomMania := chbCupomMania.Checked;
end;

procedure TForm1.DataHoraltimaReduoZ1Click(Sender: TObject);
begin
  mResp.Lines.Add('Data/Hora Última Redução Z: (' + FormatDateTime('dd/mm/yy hh:nn:ss', ACBrECF1.DataHoraUltimaReducaoZ) + ')');
  AtualizaMemos;
end;

procedure TForm1.DataHoraSwBasico1Click(Sender: TObject);
begin
  mResp.Lines.Add('Data Hora Sw.Básico: (' + FormatDateTime('dd/mm/yy hh:nn:ss', ACBrECF1.DataHoraSB) + ')');
  AtualizaMemos;
end;

procedure TForm1.ACBrECFVirtualNaoFiscal1GravaArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
begin
  mResp.Lines.Add('ECFVirtualNaoFiscal: INI será gravado');
end;

procedure TForm1.ACBrECFVirtualNaoFiscal1LeArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
begin
  mResp.Lines.Add('ECFVirtualNaoFiscal: INI será gravado');
end;

procedure TForm1.ACBrECFVirtualNFCe1QuandoEfetuarPagamento(Det: TpagCollectionItem);
begin
  mResp.Lines.Add('ECFVirtualNFCe: Pagamento efetuado');
end;

procedure TForm1.ACBrECFVirtualNFCe1QuandoFecharDocumento(NFe: TNFe);
begin
  mResp.Lines.Add('ECFVirtualNFCe: Cupom Fechado');
  //Recalcular Totais se necessário
end;

procedure TForm1.ACBrECFVirtualNFCe1QuandoGravarArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
begin
  mResp.Lines.Add('ECFVirtualNFCe: INI será gravado');
end;

procedure TForm1.ACBrECFVirtualNFCe1QuandoLerArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
begin
  mResp.Lines.Add('ECFVirtualNFCe: INI será lido');
  AjustaACBrNFe;
end;

procedure TForm1.ACBrECFVirtualNFCe1QuandoVenderItem(Det: TDetCollectionItem);
begin
  mResp.Lines.Add('ECFVirtualNFCe: Item Vendido');

  Det.Prod.NCM := '94051010'; // Tabela NCM disponível em  http://www.receita.fazenda.gov.br/Aliquotas/DownloadArqTIPI.htm
  with Det.Imposto do
  begin
    // lei da transparencia nos impostos
    //     vTotTrib := 0.10;

    { //Preencher com dados dos impostos - o componente virtual por padrão adiciona todos como CST00 caso o ICMS seja diferente de 0
      with ICMS do
     begin
     CST          := cst00;
     ICMS.orig    := oeNacional;
     ICMS.modBC   := dbiValorOperacao;
     ICMS.vBC     := 100;
     ICMS.pICMS   := 18;
     ICMS.vICMS   := 18;
     ICMS.modBCST := dbisMargemValorAgregado;
     ICMS.pMVAST  := 0;
     ICMS.pRedBCST:= 0;
     ICMS.vBCST   := 0;
     ICMS.pICMSST := 0;
     ICMS.vICMSST := 0;
     ICMS.pRedBC  := 0;
     end;
     with PIS do
     begin
     CST      := pis99;
     PIS.vBC  := 0;
     PIS.pPIS := 0;
     PIS.vPIS := 0;

     PIS.qBCProd   := 0;
     PIS.vAliqProd := 0;
     PIS.vPIS      := 0;
     end;

     with PISST do
     begin
     vBc       := 0;
     pPis      := 0;
     qBCProd   := 0;
     vAliqProd := 0;
     vPIS      := 0;
     end;

     with COFINS do
     begin
     CST            := cof99;
     COFINS.vBC     := 0;
     COFINS.pCOFINS := 0;
     COFINS.vCOFINS := 0;

     COFINS.qBCProd   := 0;
     COFINS.vAliqProd := 0;
     end;

     with COFINSST do
     begin
     vBC       := 0;
     pCOFINS   := 0;
     qBCProd   := 0;
     vAliqProd := 0;
     vCOFINS   := 0;
     end;
    }
  end;
end;

procedure TForm1.ACBrECFVirtualSAT1QuandoAbrirDocumento(CFe: TCFe);
begin
  mResp.Lines.Add('ECFVirtualSAT: Documento Aberto');
end;

procedure TForm1.ACBrECFVirtualSAT1QuandoCancelarCupom(const NumCOOCancelar: Integer; CupomVirtual: TACBrECFVirtualClassCupom;
  var PermiteCancelamento: Boolean);
begin
  mResp.Lines.Add('ECFVirtualSAT: Inicio de Cancelamento do Cupom: ' + IntToStrZero(NumCOOCancelar, 6));
  PermiteCancelamento := true;
end;

procedure TForm1.ACBrECFVirtualSAT1QuandoEfetuarPagamento(Det: TMPCollectionItem);
begin
  mResp.Lines.Add('ECFVirtualSAT: Pagamento efetuado');
end;

procedure TForm1.ACBrECFVirtualSAT1QuandoGravarArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
begin
  mResp.Lines.Add('ECFVirtualSAT: INI será gravado');
end;

procedure TForm1.ACBrECFVirtualSAT1QuandoLerArqINI(ConteudoINI: TStrings; var Tratado: Boolean);
begin
  mResp.Lines.Add('ECFVirtualSAT: INI será lido');
  if not ACBrSAT1.Inicializado then
    AjustaACBrSAT;
end;

procedure TForm1.ACBrECFVirtualSAT1QuandoVenderItem(Det: TDetCollectionItem);
begin
  mResp.Lines.Add('ECFVirtualSAT: Item Vendido');
end;

procedure TForm1.ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := AnsiString(edtCodigoAtivacao.Text);
end;

procedure TForm1.ACBrSAT1GetsignAC(var Chave: AnsiString);
begin
  Chave := AnsiString(edtSwHAssinatura.Text);
end;

procedure TForm1.VendaFrentica1Click(Sender: TObject);
begin
  FrVendaFrenetica.Show;
end;

end.
