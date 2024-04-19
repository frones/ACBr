unit Frm_ACBrNFSeX;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrBase,
  ACBrUtil.Base,
  ACBrUtil.DateTime, ACBrUtil.FilesIO,
  ACBrDFe, ACBrDFeReport, ACBrMail, ACBrNFSeX,
  ACBrNFSeXConversao, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXDANFSeClass, ACBrNFSeXDANFSeRLClass;

type
  TfrmACBrNFSe = class(TForm)
    pnlMenus: TPanel;
    pnlCentral: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PageControl4: TPageControl;
    TabSheet3: TTabSheet;
    lSSLLib: TLabel;
    lCryptLib: TLabel;
    lHttpLib: TLabel;
    lXmlSign: TLabel;
    gbCertificado: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label25: TLabel;
    sbtnGetCert: TSpeedButton;
    sbtnNumSerie: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    cbSSLLib: TComboBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbXmlSignLib: TComboBox;
    TabSheet4: TTabSheet;
    GroupBox3: TGroupBox;
    sbtnPathSalvar: TSpeedButton;
    Label29: TLabel;
    Label31: TLabel;
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    chkSalvarGer: TCheckBox;
    cbFormaEmissao: TComboBox;
    cbxAtualizarXML: TCheckBox;
    cbxExibirErroSchema: TCheckBox;
    edtFormatoAlerta: TEdit;
    cbxRetirarAcentos: TCheckBox;
    edtPathSchemas: TEdit;
    TabSheet7: TTabSheet;
    GroupBox4: TGroupBox;
    lTimeOut: TLabel;
    lSSLLib1: TLabel;
    cbxVisualizar: TCheckBox;
    rgTipoAmb: TRadioGroup;
    chkSalvarSOAP: TCheckBox;
    seTimeOut: TSpinEdit;
    cbSSLType: TComboBox;
    gbProxy: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    gbxRetornoEnvio: TGroupBox;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    cbxAjustarAut: TCheckBox;
    edtTentativas: TEdit;
    edtIntervalo: TEdit;
    edtAguardar: TEdit;
    TabSheet12: TTabSheet;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edtEmitCNPJ: TEdit;
    edtEmitIM: TEdit;
    edtEmitRazao: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitCEP: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitComp: TEdit;
    edtEmitBairro: TEdit;
    TabSheet13: TTabSheet;
    sbPathNFSe: TSpeedButton;
    Label35: TLabel;
    chkSalvarArq: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathNFSe: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathNFSe: TEdit;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDANFSE: TRadioGroup;
    TabSheet14: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    edtSmtpHost: TEdit;
    edtSmtpPort: TEdit;
    edtSmtpUser: TEdit;
    edtSmtpPass: TEdit;
    edtEmailAssunto: TEdit;
    cbEmailSSL: TCheckBox;
    mmEmailMsg: TMemo;
    btnSalvarConfig: TBitBtn;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    pgRespostas: TPageControl;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    WBXmlRetorno: TWebBrowser;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    OpenDialog1: TOpenDialog;
    Label6: TLabel;
    Label39: TLabel;
    edtPrestLogo: TEdit;
    sbtnPrestLogo: TSpeedButton;
    Label40: TLabel;
    edtPrefeitura: TEdit;
    Label21: TLabel;
    cbCidades: TComboBox;
    Label22: TLabel;
    edtEmitUF: TEdit;
    Label20: TLabel;
    edtCodCidade: TEdit;
    Label32: TLabel;
    edtTotalCidades: TEdit;
    edtEmitCidade: TEdit;
    Label42: TLabel;
    edtEmailRemetente: TEdit;
    cbEmailTLS: TCheckBox;
    ACBrMail1: TACBrMail;
    ACBrNFSeX1: TACBrNFSeX;
    Label43: TLabel;
    edtCNPJPrefeitura: TEdit;
    chkConsultaLoteAposEnvio: TCheckBox;
    chkConsultaAposCancelar: TCheckBox;
    ACBrNFSeXDANFSeRL1: TACBrNFSeXDANFSeRL;
    Label30: TLabel;
    edtSenhaWeb: TEdit;
    Label33: TLabel;
    edtUserWeb: TEdit;
    Label34: TLabel;
    edtFraseSecWeb: TEdit;
    Label41: TLabel;
    Label44: TLabel;
    edtChaveAcessoWeb: TEdit;
    edtChaveAutorizWeb: TEdit;
    WBXmlEnvio: TWebBrowser;
    WBXmlNotas: TWebBrowser;
    chkMontarPathSchemas: TCheckBox;
    Label46: TLabel;
    edtPathPDF: TEdit;
    sbtnPathPDF: TSpeedButton;
    Label47: TLabel;
    lblSchemas: TLabel;
    Label45: TLabel;
    lblVersaoSchemas: TLabel;
    pgcProvedores: TPageControl;
    tsDemais: TTabSheet;
    tsPadraoNacional: TTabSheet;
    pgcBotoes: TPageControl;
    tsEnvios: TTabSheet;
    btnGerarEnviarLote: TButton;
    btnGerarEnviarNFSe: TButton;
    btnGerarEnviarSincrono: TButton;
    btnImprimir: TButton;
    btnEnviaremail: TButton;
    btnLinkNFSe: TButton;
    btnGerarLoteRPS: TButton;
    btnSubsNFSe: TButton;
    btnEmitir: TButton;
    tsConsultas: TTabSheet;
    btnConsultarSitLote: TButton;
    btnConsultarLote: TButton;
    btnConsultarNFSeRPS: TButton;
    btnConsultarNFSePeriodo: TButton;
    btnConsultarNFSePeloNumero: TButton;
    btnConsultarNFSeFaixa: TButton;
    btnConsultarNFSeGenerico: TButton;
    tsConsServPrest: TTabSheet;
    btnConsultarNFSeServicoPrestadoPorNumero: TButton;
    btnConsultarNFSeServicoPrestadoPorPeriodo: TButton;
    btnConsultarNFSeServicoPrestadoPorTomador: TButton;
    btnConsultarNFSeServicoPrestadoPorIntermediario: TButton;
    tsConsServTom: TTabSheet;
    btnConsultarNFSeServicoTomadoPorNumero: TButton;
    btnConsultarNFSeServicoTomadoPorPeriodo: TButton;
    btnConsultarNFSeServicoTomadoPorPrestador: TButton;
    btnConsultarNFSeServicoTomadoPorIntermediario: TButton;
    btnConsultarNFSeServicoTomadoPorTomador: TButton;
    tsCancelamento: TTabSheet;
    btnCancNFSe: TButton;
    tsTeste: TTabSheet;
    btnGerarEnviarTeste_SP: TButton;
    mmoObs: TMemo;
    tsOutros: TTabSheet;
    btnGerarToken: TButton;
    pgcBotoes1: TPageControl;
    tsEnvios1: TTabSheet;
    btnImprimirPN: TButton;
    btnEnviaremailPN: TButton;
    btnEmitirPN: TButton;
    tsEventos1: TTabSheet;
    btnEventoCancNFSePN: TButton;
    btnEventoCancPorSubPN: TButton;
    btnEventoAnaliseCancPN: TButton;
    btnEventoConfPrestPN: TButton;
    btnEventoConfTomPN: TButton;
    btnEventoConfInterPN: TButton;
    btnEventoRejPrestPN: TButton;
    btnEventoRejTomPN: TButton;
    btnEventoRejInterPN: TButton;
    tsConsultas1: TTabSheet;
    btnConsultarDPSPorChavePN: TButton;
    btnConsultarNFSePelaChavePN: TButton;
    btnObterPDFdoDANFSEPN: TButton;
    btnConsultarEventoChavPN: TButton;
    btnConsultarDFe: TButton;
    tsConsultarParametros: TTabSheet;
    btnConsultarParamMunicConvenio: TButton;
    btnConsultarParamMunicAliquota: TButton;
    btnConsultarParamMunicHistAliquota: TButton;
    btnConsultarParamMunicRegimesEspeciais: TButton;
    btnConsultarParamMunicRetencoes: TButton;
    btnConsultarParamMunicBeneficio: TButton;
    Label48: TLabel;
    lblLayout: TLabel;
    cbLayoutNFSe: TComboBox;
    Label49: TLabel;
    btnLerINI: TButton;
    btnConsultarLinkNFSe: TButton;
    btnConsultarNFSePelaChave: TButton;
    btnGerarArqINI: TButton;
    tsOutrosPN: TTabSheet;
    btnLerINIPN: TButton;
    btnGerarArqINIPN: TButton;
    btnDataValidade: TButton;
    btnNumSerie: TButton;
    btnSubName: TButton;
    btnIssuerName: TButton;
    btnCNPJ: TButton;
    btnLeituraX509: TButton;
    btnInformacoes: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathNFSeClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnNumSerieClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure btnDataValidadeClick(Sender: TObject);
    procedure btnNumSerieClick(Sender: TObject);
    procedure btnSubNameClick(Sender: TObject);
    procedure btnCNPJClick(Sender: TObject);
    procedure btnIssuerNameClick(Sender: TObject);
    procedure btnLeituraX509Click(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure sbtnLogoMarcaClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure ACBrNFSeX1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure ACBrNFSeX1StatusChange(Sender: TObject);
    procedure sbtnPrestLogoClick(Sender: TObject);
    procedure btnGerarEnviarLoteClick(Sender: TObject);
    procedure btnGerarEnviarNFSeClick(Sender: TObject);
    procedure btnGerarEnviarSincronoClick(Sender: TObject);
    procedure btnGerarLoteRPSClick(Sender: TObject);
    procedure btnSubsNFSeClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnEnviaremailClick(Sender: TObject);
    procedure btnLinkNFSeClick(Sender: TObject);
    procedure btnConsultarSitLoteClick(Sender: TObject);
    procedure btnConsultarLoteClick(Sender: TObject);
    procedure btnConsultarNFSeRPSClick(Sender: TObject);
    procedure btnConsultarNFSePeriodoClick(Sender: TObject);
    procedure btnCancNFSeClick(Sender: TObject);
    procedure cbCidadesChange(Sender: TObject);
    procedure btnConsultarNFSePeloNumeroClick(Sender: TObject);
    procedure btnConsultarNFSeFaixaClick(Sender: TObject);
    procedure btnGerarEnviarTeste_SPClick(Sender: TObject);
    procedure btnConsultarNFSeServicoPrestadoPorNumeroClick(Sender: TObject);
    procedure btnConsultarNFSeServicoTomadoPorNumeroClick(Sender: TObject);
    procedure btnEmitirClick(Sender: TObject);
    procedure btnConsultarNFSeServicoPrestadoPorPeriodoClick(Sender: TObject);
    procedure btnConsultarNFSeServicoPrestadoPorTomadorClick(Sender: TObject);
    procedure btnConsultarNFSeServicoPrestadoPorIntermediarioClick(
      Sender: TObject);
    procedure btnConsultarNFSeServicoTomadoPorPeriodoClick(Sender: TObject);
    procedure btnConsultarNFSeServicoTomadoPorPrestadorClick(Sender: TObject);
    procedure btnConsultarNFSeServicoTomadoPorIntermediarioClick(
      Sender: TObject);
    procedure btnConsultarNFSeServicoTomadoPorTomadorClick(Sender: TObject);
    procedure btnConsultarNFSeGenericoClick(Sender: TObject);
    procedure sbtnPathPDFClick(Sender: TObject);
    procedure btnGerarTokenClick(Sender: TObject);
    procedure btnObterPDFdoDANFSEPNClick(Sender: TObject);
    procedure btnEventoCancNFSePNClick(Sender: TObject);
    procedure btnEventoCancPorSubPNClick(Sender: TObject);
    procedure btnEventoAnaliseCancPNClick(Sender: TObject);
    procedure btnEventoConfPrestPNClick(Sender: TObject);
    procedure btnEventoConfTomPNClick(Sender: TObject);
    procedure btnEventoConfInterPNClick(Sender: TObject);
    procedure btnEventoRejPrestPNClick(Sender: TObject);
    procedure btnEventoRejTomPNClick(Sender: TObject);
    procedure btnEventoRejInterPNClick(Sender: TObject);
    procedure btnConsultarDPSPorChavePNClick(Sender: TObject);
    procedure btnConsultarEventoChavPNClick(Sender: TObject);
    procedure btnConsultarDFeClick(Sender: TObject);
    procedure btnConsultarParamMunicConvenioClick(Sender: TObject);
    procedure btnConsultarParamMunicAliquotaClick(Sender: TObject);
    procedure btnConsultarParamMunicHistAliquotaClick(Sender: TObject);
    procedure btnConsultarParamMunicRegimesEspeciaisClick(Sender: TObject);
    procedure btnConsultarParamMunicRetencoesClick(Sender: TObject);
    procedure btnConsultarParamMunicBeneficioClick(Sender: TObject);
    procedure btnEmitirPNClick(Sender: TObject);
    procedure btnImprimirPNClick(Sender: TObject);
    procedure btnEnviaremailPNClick(Sender: TObject);
    procedure btnConsultarNFSePelaChavePNClick(Sender: TObject);
    procedure btnLerINIClick(Sender: TObject);
    procedure btnConsultarLinkNFSeClick(Sender: TObject);
    procedure btnConsultarNFSePelaChaveClick(Sender: TObject);
    procedure btnGerarArqINIClick(Sender: TObject);
    procedure btnLerINIPNClick(Sender: TObject);
    procedure btnGerarArqINIPNClick(Sender: TObject);
    procedure btnInformacoesClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;

    procedure Alimentar_Componente(NumDFe, NumLote: String);

    procedure Alimentar_Componente_layout_ABRASF(NumDFe, NumLote: String);
    procedure Alimentar_Componente_layout_Proprio(NumDFe, NumLote: String);
    procedure Alimentar_Componente_layout_PadraoNacional(NumDFe, NumLote: String);

    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser;
      NomeArq: string = 'temp.xml'; aTempo: Integer = 0);
    procedure AtualizarSSLLibsCombo;
    procedure AtualizarCidades;
    function RoundTo5(Valor: Double; Casas: Integer): Double;

    procedure ChecarResposta(aMetodo: TMetodo);
  public
    { Public declarations }
  end;

var
  frmACBrNFSe: TfrmACBrNFSe;

implementation

uses
  strutils, math, TypInfo, DateUtils, blcksock, FileCtrl, Grids, IniFiles, Printers,
  pcnConversao,
  ACBrOpenSSLUtils, OpenSSLExt,
  ACBrDFeConfiguracoes, ACBrDFeSSL,
  ACBrDFeUtil,
  ACBrNFSeXWebserviceBase,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrNFSe }

procedure TfrmACBrNFSe.ACBrNFSeX1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
  Tratado := False;
end;

procedure TfrmACBrNFSe.ACBrNFSeX1StatusChange(Sender: TObject);
begin
  case ACBrNFSeX1.Status of
    stNFSeIdle:
      begin
        if (frmStatus <> nil) then
          frmStatus.Hide;
      end;

    stNFSeRecepcao:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Enviando RPS...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFSeConsultaSituacao:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Consultando a Situação...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFSeConsulta:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Consultando...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFSeCancelamento:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Cancelando NFSe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFSeSubstituicao:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Substituindo NFSe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFSeEmail:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Enviando Email...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFSeAguardaProcesso:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Aguardando o Processo...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFSeEnvioWebService:
      begin
        if (frmStatus = nil) then
          frmStatus := TfrmStatus.Create(Application);
        frmStatus.lblStatus.Caption := 'Enviando para o WebService...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;
  end;

  Application.ProcessMessages;
end;

procedure TfrmACBrNFSe.Alimentar_Componente(NumDFe, NumLote: String);
begin
  if ACBrNFSeX1.Configuracoes.Geral.Layout = loABRASF then
    Alimentar_Componente_layout_ABRASF(NumDFe, NumLote)
  else
  begin
    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proPadraoNacional then
      Alimentar_Componente_layout_PadraoNacional(NumDFe, NumLote)
    else
      Alimentar_Componente_layout_Proprio(NumDFe, NumLote);
  end;
end;

procedure TfrmACBrNFSe.Alimentar_Componente_layout_PadraoNacional(NumDFe,
  NumLote: String);
begin
  with ACBrNFSeX1 do
  begin
    NotasFiscais.NumeroLote := NumLote;

    with NotasFiscais.New.NFSe do
    begin
      // Numero do DPS a ser gerado e enviado para o WebService
      Numero := NumDFe;

      verAplic := 'ACBrNFSeX-1.00';

      IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumDFe));

      IdentificacaoRps.Serie := '900';

      DataEmissao := Now;
      Competencia := Now;

      {
        TnfseRegimeEspecialTributacao = (retNenhum, retMicroempresaMunicipal, retEstimativa,
                                         retSociedadeProfissionais, retCooperativa,
                                         retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                                         retLucroReal, retLucroPresumido, retSimplesNacional,
                                         retImune, retEmpresaIndividualRELI, retEmpresaPP,
                                         retMicroEmpresario, retOutros, retMovimentoMensal,
                                         retISSQNAutonomos, retISSQNSociedade,
                                         retNotarioRegistrador,
                                         retTribFaturamentoVariavel, retFixo,
                                         retIsencao,retExigibSuspensaJudicial,
                                         retExigibSuspensaAdm);
      }
      RegimeEspecialTributacao := retNenhum;

      // TOptanteSN = (osnNaoOptante, osnOptanteMEI, osnOptanteMEEPP)
      OptanteSN := osnOptanteMEI;

      {=========================================================================
        Dados do Serviço
      =========================================================================}

      Servico.ItemListaServico := '010601';

      Servico.CodigoNBS := '';
      Servico.Discriminacao := 'discriminacao I; discriminacao II';

      Servico.CodigoTributacaoMunicipio := '';

      Servico.CodigoMunicipio := edtCodCidade.Text;
      Servico.CodigoPais := 1058; // Brasil

     {=========================================================================
        Dados do Serviço (valores)
      =========================================================================}

      Servico.Valores.ValorServicos := 100.35;
      Servico.Valores.ValorDeducoes := 0.00;

      Servico.Valores.DescontoIncondicionado := 0.00;
      Servico.Valores.DescontoCondicionado := 0.00;

      Servico.Valores.tribMun.cPaisResult := 0;
      // TtribISSQN = (tiOperacaoTributavel, tiImunidade, tiExportacao, tiNaoIncidencia);
      Servico.Valores.tribMun.tribISSQN := tiNaoIncidencia;
      Servico.Valores.tribMun.tpImunidade := timNenhum;
      Servico.Valores.tribMun.tpRetISSQN := trNaoRetido;
      Servico.Valores.totTrib.indTotTrib := indNao;

      if OptanteSN = osnOptanteMEEPP then
      begin
        Servico.Valores.totTrib.indTotTrib := indSim;
        Servico.Valores.totTrib.pTotTribSN := 2.01;
      end;

      {
         Só devem ser informados se o Prestador não for Simples Nacional

      Servico.Valores.tribFed.CST := cst01;
      Servico.Valores.tribFed.vBCPisCofins := Servico.Valores.ValorServicos -
                                         Servico.Valores.ValorDeducoes -
                                         Servico.Valores.DescontoIncondicionado;

      Servico.Valores.tribFed.pAliqPis := 1.65;
      Servico.Valores.tribFed.pAliqCofins := 7.60;
      Servico.Valores.tribFed.vPis := Servico.Valores.tribFed.vBCPisCofins *
                                      Servico.Valores.tribFed.pAliqPis / 100;
      Servico.Valores.tribFed.vCofins := Servico.Valores.tribFed.vBCPisCofins *
                                    Servico.Valores.tribFed.pAliqCofins / 100;
      Servico.Valores.tribFed.tpRetPisCofins := trpcNaoRetido;

      Servico.Valores.totTrib.vTotTribFed := Servico.Valores.tribFed.vPis +
                                             Servico.Valores.tribFed.vCofins;
      Servico.Valores.totTrib.vTotTribEst := 0;
      Servico.Valores.totTrib.vTotTribMun := 0;
      }

      {=========================================================================
        Dados do Prestador de Serviço
      =========================================================================}
      Prestador.IdentificacaoPrestador.CpfCnpj := edtEmitCNPJ.Text;
      Prestador.IdentificacaoPrestador.InscricaoMunicipal := edtEmitIM.Text;

      Prestador.cUF := UFtoCUF(edtEmitUF.Text);

      Prestador.Endereco.CodigoMunicipio := edtCodCidade.Text;

      Prestador.Contato.Telefone := '1633224455';
      Prestador.Contato.Email := 'nome@provedor.com.br';

      {=========================================================================
        Dados do Tomador de Serviço
      =========================================================================}

      Tomador.AtualizaTomador := snNao;
      Tomador.TomadorExterior := snNao;

      Tomador.IdentificacaoTomador.CpfCnpj := '12345678901';
      Tomador.IdentificacaoTomador.InscricaoMunicipal := '';
      Tomador.IdentificacaoTomador.InscricaoEstadual := '';

      Tomador.RazaoSocial := 'INSCRICAO DE TESTE E TESTE';

      Tomador.Endereco.TipoLogradouro := 'RUA';
      Tomador.Endereco.Endereco := 'RUA PRINCIPAL';
      Tomador.Endereco.Numero := '100';
      Tomador.Endereco.Complemento := 'APTO 11';
      Tomador.Endereco.TipoBairro := 'BAIRRO';
      Tomador.Endereco.Bairro := 'CENTRO';
      Tomador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Tomador.Endereco.xMunicipio := 'Cidade do Tomador';
      Tomador.Endereco.UF := edtEmitUF.Text;
      Tomador.Endereco.CodigoPais := 1058; // Brasil
      Tomador.Endereco.CEP := '14800000';
      Tomador.Endereco.xPais := 'BRASIL';

      Tomador.Contato.Telefone := '1622223333';
      Tomador.Contato.Email := 'nome@provedor.com.br';

      {=========================================================================
        Dados do Intermediario na prestação do serviço
      =========================================================================}

//       Intermediario.RazaoSocial := 'razao';
//       Intermediario.Identificacao.CpfCnpj := '00000000000';
//       Intermediario.Identificacao.InscricaoMunicipal := '12547478';

      {=========================================================================
        Dados da Obra (quando o serviço for uma obra)
      =========================================================================}

//      ConstrucaoCivil.CodigoObra := '88888';
//      ConstrucaoCivil.Art        := '433';
    end;
  end;
end;

procedure TfrmACBrNFSe.Alimentar_Componente_layout_Proprio(NumDFe,
  NumLote: String);
var
  vValorISS: Double;
  i: Integer;
begin
  with ACBrNFSeX1 do
  begin
    NotasFiscais.NumeroLote := NumLote;

    // Provedor ISSDSF e ISSSaoPaulo
    NotasFiscais.Transacao := True;

    with NotasFiscais.New.NFSe do
    begin
      // Numero do RPS a ser gerado e enviado para o WebService
      Numero := NumDFe;

      // Provedor CTAConsult, Governa
      TipoRecolhimento := '1';

      // Provedor SigISS, ISSCambe
      {
        Situação pode ser:
        tp – Tributada no prestador = tsTributadaNoPrestador;
        tt – Tributada no tomador = tsTibutadaNoTomador;
        is – Isenta = tsIsenta;
        im – Imune = tsImune;
        nt – Não tributada = tsNaoTributada.
      }
      SituacaoTrib := tsTributadaNoPrestador;

      // Usado pelo provedor AssessorPublico
      {
        A tag SITUACAO refere-se ao código da situação da NFS-e e aceita números
        inteiros de até 4 caracteres, sendo que devem estar previamente
        cadastradas no sistema.
      }
      Situacao := 1;
      // Usado pelo provedor AssessorPublico
      NumeroLote := NumLote;

      // Provedor Infisc
      cNFSe := GerarCodigoDFe(StrToIntDef(Numero, 0));
      ModeloNFSe := '90';

      // Para os provedores ISSDSF e Siat no campo SeriePrestacao devemos informar:
      {
        Número do equipamento emissor do RPS ou série de prestação.
        Caso não utilize a série, preencha o campo com o valor ‘99’ que indica
        modelo único. Caso queira utilizar o campo série para indicar o número do
        equipamento emissor do RPS deve-se solicitar liberação da prefeitura.

      }
      // Para o provedor ISSBarueri usar esse campo para informar a série da nota
      // a ser cancelada ou substituida.
      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat] then
        SeriePrestacao := '99'
      else
        SeriePrestacao := '1';

      IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumDFe));

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proEquiplano:
          IdentificacaoRps.Serie := '1';

        proISSDSF, proSiat:
          IdentificacaoRps.Serie := 'NF';
      else
        IdentificacaoRps.Serie := '85';
      end;

      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      IdentificacaoRps.Tipo := trRPS;

      DataEmissao := Now;
      Competencia := Now;
      DataEmissaoRPS := Now;

      // Provedor RLZ
      Vencimento := Now + 1;

      (*
        TnfseNaturezaOperacao = ( no1, no2, no3, no4, no5, no6, no7,
        no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
        no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
        no70, no71, no72, no78, no79,
        no101, no111, no121, no201, no301,
        no501, no511, no541, no551, no601, no701 );
      *)

      NaturezaOperacao := no1;

      {
        TnfseRegimeEspecialTributacao = (retNenhum, retMicroempresaMunicipal, retEstimativa,
                                         retSociedadeProfissionais, retCooperativa,
                                         retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                                         retLucroReal, retLucroPresumido, retSimplesNacional,
                                         retImune, retEmpresaIndividualRELI, retEmpresaPP,
                                         retMicroEmpresario, retOutros, retMovimentoMensal,
                                         retISSQNAutonomos, retISSQNSociedade,
                                         retNotarioRegistrador,
                                         retTribFaturamentoVariavel, retFixo,
                                         retIsencao,retExigibSuspensaJudicial,
                                         retExigibSuspensaAdm);
      }
      RegimeEspecialTributacao := retMicroempresaMunicipal;

      // TnfseSimNao = ( snSim, snNao );
      OptanteSimplesNacional := snSim;

      // TnfseSimNao = ( snSim, snNao );
      IncentivadorCultural := snNao;

      // Provedor ISSSaoPaulo
      PercentualCargaTributaria := 0;
      ValorCargaTributaria := 0;

      // TnfseSimNao = ( snSim, snNao );
      // snSim = Ambiente de Produção
      // snNao = Ambiente de Homologação
      {=== Usado pelo provedor Infisc, eGoverneISS ===}
      if ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taProducao then
        Producao := snSim
      else
        Producao := snNao;

      // TnfseStatusRPS = ( srNormal, srCancelado );
      StatusRps := srNormal;

      {=========================================================================
        Numero, Série e Tipo do Rps que esta sendo substituido por este
      =========================================================================}

      {
       RpsSubstituido.Numero := FormatFloat('#########0', i);
       RpsSubstituido.Serie  := 'UNICA';
       // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
       RpsSubstituido.Tipo   := trRPS;
      }

      {=========================================================================
        Dados do Serviço (valores)
      =========================================================================}

      // Provedores que permitem informar mais de 1 serviço:
      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proAgili, proAssessorPublico,
           proCTA, proCTAConsult, proEquiplano, proFacundo, proFGMaiss, proEL,
           proGoverna, proInfisc, proIPM, proISSDSF, proPriMax, proRLZ, proSam,
           proSimple, proSmarAPD, proWebFisco, proBauhaus, proeISS, proSoftPlan] then
      begin
        Servico.Valores.ValorServicos := 0;

        with Servico.ItemServico.New do
        begin
          Descricao := 'Desc. do Serv. 1';

          ItemListaServico := '09.01';

          // infisc, EL
          CodServ := '12345';
          // Infisc, EL
          codLCServ := '123';

          ValorDeducoes := 0;
          xJustDeducao := '';

          AliqReducao := 0;
          ValorReducao := 0;

          DescontoIncondicionado := 0;
          DescontoCondicionado := 0;

          // TUnidade = (tuHora, tuQtde);
          TipoUnidade := tuQtde;
          Unidade := 'UN';
          Quantidade := 10;
          ValorUnitario := 5;

          QtdeDiaria := 0;
          ValorTaxaTurismo := 0;

          ValorTotal := Quantidade * ValorUnitario;

          Servico.Valores.ValorServicos := Servico.Valores.ValorServicos +
                                           ValorTotal;

          BaseCalculo := ValorTotal - ValorDeducoes - DescontoIncondicionado;

          Aliquota := 2;

          ValorISS := BaseCalculo * Aliquota / 100;

          ValorISSRetido := 0;

          AliqISSST := 0;
          ValorISSST := 0;

          ValorBCCSLL := 0;
          AliqRetCSLL := 0;
          ValorCSLL := 0;

          ValorBCPIS := 0;
          AliqRetPIS := 0;
          ValorPIS := 0;

          ValorBCCOFINS := 0;
          AliqRetCOFINS := 0;
          ValorCOFINS := 0;

          ValorBCINSS := 0;
          AliqRetINSS := 0;
          ValorINSS := 0;

          ValorBCRetIRRF := 0;
          AliqRetIRRF := 0;
          ValorIRRF := 0;

          case ACBrNFSeX1.Configuracoes.Geral.Provedor of
            proAgili:
              // código com 9 digitos
              CodigoCnae := '452000200';
          else
            // código com 7 digitos
            CodigoCnae := '6203100';
          end;

          // Provedor IPM
          { define se o tributo é no municipio do prestador ou não }
          TribMunPrestador := snNao;
          { codigo do municipio que ocorreu a prestação de serviço }
          CodMunPrestacao :=  edtCodCidade.Text;
          { codigo da situação tributária: 0 até 15 }
          SituacaoTributaria := 0;

          // Provedor Agili
          DadosProfissionalParceiro.IdentificacaoParceiro.CpfCnpj := '12345678000123';
          DadosProfissionalParceiro.IdentificacaoParceiro.InscricaoMunicipal := '123';
          DadosProfissionalParceiro.RazaoSocial := 'Nome do Parceiro';
          DadosProfissionalParceiro.PercentualProfissionalParceiro := 5;
        end;
      end
      else
      begin
        // Provedor Giap
        Servico.Endereco.Bairro := 'Bairro onde o serviço foi prestado';
        Servico.Endereco.CEP := 'cep do local da prestação do serviço';
        Servico.Endereco.xMunicipio := 'município do local da prestação do serviço';
        Servico.Endereco.Complemento := 'complemento do local da prestação do serviço';
        Servico.Endereco.Endereco := 'endereço do local da prestação do serviço';
        Servico.Endereco.Numero := 'numero do local da prestação do serviço';
        Servico.Endereco.xPais := 'pais do local da prestação do serviço';
        Servico.Endereco.UF := 'UF do local da prestação do serviço';

        Servico.Valores.ValorServicos := 100.35;
        Servico.Valores.ValorDeducoes := 0.00;
        Servico.Valores.AliquotaPis := 0.00;
        Servico.Valores.ValorPis := 0.00;
        Servico.Valores.AliquotaCofins := 2.00;
        Servico.Valores.ValorCofins := 2.00;
        Servico.Valores.ValorInss := 0.00;
        Servico.Valores.ValorIr := 0.00;
        Servico.Valores.ValorCsll := 0.00;

        // TnfseSituacaoTributaria = ( stRetencao, stNormal, stSubstituicao );
        // stRetencao = snSim
        // stNormal   = snNao

        // Neste exemplo não temos ISS Retido ( stNormal = Não )
        // Logo o valor do ISS Retido é igual a zero.
        Servico.Valores.IssRetido := stNormal;
        Servico.Valores.ValorIssRetido := 0.00;

        Servico.Valores.OutrasRetencoes := 0.00;
        Servico.Valores.OutrosDescontos := 0.00;
        Servico.Valores.DescontoIncondicionado := 0.00;
        Servico.Valores.DescontoCondicionado := 0.00;

        Servico.Valores.BaseCalculo := Servico.Valores.ValorServicos -
        Servico.Valores.ValorDeducoes - Servico.Valores.DescontoIncondicionado;

        Servico.Valores.Aliquota := 2;

        Servico.Valores.tribMun.cPaisResult := 0;
        // TtribISSQN = (tiOperacaoTributavel, tiImunidade, tiExportacao, tiNaoIncidencia);
        Servico.Valores.tribMun.tribISSQN := tiNaoIncidencia;
        Servico.Valores.tribMun.tpImunidade := timNenhum;
        Servico.Valores.tribMun.tpRetISSQN := trNaoRetido;
        Servico.Valores.totTrib.indTotTrib := indNao;

        vValorISS := Servico.Valores.BaseCalculo * Servico.Valores.Aliquota / 100;

        // A função RoundTo5 é usada para arredondar valores, sendo que o segundo
        // parametro se refere ao numero de casas decimais.
        // exemplos: RoundTo5(50.532, -2) ==> 50.53
        // exemplos: RoundTo5(50.535, -2) ==> 50.54
        // exemplos: RoundTo5(50.536, -2) ==> 50.54

        Servico.Valores.ValorISS := RoundTo5(vValorISS, -2);

        Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
          Servico.Valores.ValorPis - Servico.Valores.ValorCofins -
          Servico.Valores.ValorInss - Servico.Valores.ValorIr -
          Servico.Valores.ValorCsll - Servico.Valores.OutrasRetencoes -
          Servico.Valores.ValorIssRetido - Servico.Valores.DescontoIncondicionado
          - Servico.Valores.DescontoCondicionado;
      end;

      {=========================================================================
        Dados do Serviço
      =========================================================================}

      // Provedor Prescon
      DeducaoMateriais := snSim;
      with Servico.Deducao.New do
      begin
        DeducaoPor := dpValor;
        TipoDeducao := tdMateriais;
        ValorDeduzir := 10.00;
      end;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proCTAConsult:
          Servico.ItemListaServico := '01050';
      else
        // código padrão da ABRASF
        Servico.ItemListaServico := '09.01';
      end;

//      Servico.CodigoNBS := '123456789';
      Servico.Discriminacao := 'discriminacao I; discriminacao II';

      // TnfseResponsavelRetencao = ( rtTomador, rtPrestador, rtIntermediario, rtNenhum )
      //                              '1',       '',          '2',             ''
      Servico.ResponsavelRetencao := rtTomador;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proIPM:
          Servico.CodigoTributacaoMunicipio := '';
      else
        Servico.CodigoTributacaoMunicipio := '63194';
      end;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proSiat, proISSDSF, proCTAConsult:
          // código com 9 digitos
          Servico.CodigoCnae := '452000200';
      else
        // código com 7 digitos
        Servico.CodigoCnae := '6203100';
      end;

      Servico.CodigoMunicipio := edtCodCidade.Text;
      Servico.UFPrestacao := 'SP';

      Servico.CodigoPais := 1058; // Brasil
      Servico.MunicipioIncidencia := StrToIntDef(edtCodCidade.Text, 0);

      // Provedor GeisWeb
      // tlDevidoNoMunicPrestador, tlDevidoNoMunicTomador, tlSimplesNacional, tlIsentoImune
      Servico.TipoLancamento := tlSimplesNacional;

      // Provedor SoftPlan
      Servico.CFPS := '9201';

      // Provedor ISSDSF
      // (ttIsentaISS, ttNaoIncidencianoMunic, ttImune, ttExigibilidadeSusp,
      //  ttNaoTributavel, ttTributavel, ttTributavelFixo, ttTributavelSN,
      //  ttMEI);
      Servico.Tributacao := ttImune;

      {=========================================================================
        Dados do Prestador de Serviço
      =========================================================================}
      Prestador.IdentificacaoPrestador.CpfCnpj := edtEmitCNPJ.Text;
      Prestador.IdentificacaoPrestador.InscricaoMunicipal := edtEmitIM.Text;

      Prestador.RazaoSocial  := edtEmitRazao.Text;
      Prestador.NomeFantasia := edtEmitRazao.Text;

      Prestador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Prestador.Endereco.Endereco := edtEmitLogradouro.Text;
      Prestador.Endereco.Numero   := edtEmitNumero.Text;
      Prestador.Endereco.Bairro   := edtEmitBairro.Text;
      Prestador.Endereco.xMunicipio := 'Nome do Municipio';
      Prestador.Endereco.UF := edtEmitUF.Text;
      Prestador.Endereco.CodigoPais := 1058;
      Prestador.Endereco.xPais := 'BRASIL';
      Prestador.Endereco.CEP := edtEmitCEP.Text;

      Prestador.Contato.DDD := '16';

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proCTAConsult:
          Prestador.Contato.Telefone := '33224455';
      else
        Prestador.Contato.Telefone := '1633224455';
      end;

      Prestador.Contato.Email := 'nome@provedor.com.br';

      {=========================================================================
        Dados do Tomador de Serviço
      =========================================================================}

      Tomador.AtualizaTomador := snNao;
      Tomador.TomadorExterior := snNao;

      // Para o provedor IPM usar os valores:
      // tpPFNaoIdentificada ou tpPF para pessoa Fisica
      // tpPJdoMunicipio ou tpPJforaMunicipio ou tpPJforaPais para pessoa Juridica

      // Para o provedor SigISS usar os valores acima de forma adquada
      Tomador.IdentificacaoTomador.Tipo := tpPF;
      Tomador.IdentificacaoTomador.CpfCnpj := '12345678901';
      Tomador.IdentificacaoTomador.InscricaoMunicipal := '';
      Tomador.IdentificacaoTomador.InscricaoEstadual := '';
      Tomador.IdentificacaoTomador.Nif := '';
      // (tnnNaoInformado, tnnDispensado, tnnNaoExigencia);
      Tomador.IdentificacaoTomador.cNaoNIF := tnnDispensado;

      Tomador.RazaoSocial := 'INSCRICAO DE TESTE E TESTE';

      // O campo EnderecoInformado é utilizado pelo provedor IPM
      // A tag <endereco_informado> é opcional, caso não deseje que ela seja
      // gerada devemos informar uma string vazia, ou S = Sim ou N = Não
      Tomador.Endereco.EnderecoInformado := 'S';
      Tomador.Endereco.TipoLogradouro := 'RUA';
      Tomador.Endereco.Endereco := 'RUA PRINCIPAL';
      Tomador.Endereco.Numero := '100';
      Tomador.Endereco.Complemento := 'APTO 11';
      Tomador.Endereco.TipoBairro := 'BAIRRO';
      Tomador.Endereco.Bairro := 'CENTRO - tamanho maior que o permitido para o nome do bairro da cidade';
      Tomador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Tomador.Endereco.xMunicipio := 'Cidade do Tomador';
      Tomador.Endereco.UF := edtEmitUF.Text;
      Tomador.Endereco.CodigoPais := 1058; // Brasil
      Tomador.Endereco.CEP := '14800000';
      Tomador.Endereco.xPais := 'BRASIL';

      Tomador.Contato.DDD := '16';

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proCTAConsult:
          Tomador.Contato.Telefone := '22223333';
      else
        Tomador.Contato.Telefone := '1622223333';
      end;

      Tomador.Contato.Email := 'nome@provedor.com.br';

      {=========================================================================
        Dados do Intermediario na prestação do serviço
      =========================================================================}

//       Intermediario.RazaoSocial := 'razao';
//       Intermediario.Identificacao.CpfCnpj := '00000000000';
//       Intermediario.Identificacao.InscricaoMunicipal := '12547478';

      {=========================================================================
        Dados da Obra (quando o serviço for uma obra)
      =========================================================================}

//      ConstrucaoCivil.CodigoObra := '88888';
//      ConstrucaoCivil.Art        := '433';

      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proInfisc, proIPM, proSmarAPD] then
      begin
        // Condicao = (cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoCredito, cpCartaoDebito)
        CondicaoPagamento.Condicao   := cpAVista;
        CondicaoPagamento.QtdParcela := 1;

        for i := 1 to CondicaoPagamento.QtdParcela do
        begin
          with CondicaoPagamento.Parcelas.New do
          begin
            Parcela := IntToStr(i);
            DataVencimento := Date + (30 * i);
            Valor := (Servico.Valores.ValorLiquidoNfse / CondicaoPagamento.QtdParcela);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBrNFSe.Alimentar_Componente_layout_ABRASF(NumDFe,
  NumLote: String);
var
  vValorISS: Double;
  i: Integer;
begin
  with ACBrNFSeX1 do
  begin
    NotasFiscais.NumeroLote := NumLote;

    with NotasFiscais.New.NFSe do
    begin
      // Numero do RPS a ser gerado e enviado para o WebService
      Numero := NumDFe;

      IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumDFe));

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proNFSeBrasil:
          IdentificacaoRps.Serie := '1';

        proSudoeste:
          IdentificacaoRps.Serie := 'E';

        proBetha:
          IdentificacaoRps.Serie := 'NF';

        proISSNet:
          if ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taProducao then
            IdentificacaoRps.Serie := '1'
          else
            IdentificacaoRps.Serie := '8';

        proSystemPro:
          IdentificacaoRps.Serie := 'RPP';
      else
        IdentificacaoRps.Serie := '85';
      end;

      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      IdentificacaoRps.Tipo := trRPS;

      DataEmissao := Now;
      Competencia := Now;
      DataEmissaoRPS := Now;

      // Provedor fintelISS
      DataPagamento := Now;

      (*
        TnfseNaturezaOperacao = ( no1, no2, no3, no4, no5, no6, no7,
        no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
        no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
        no70, no71, no72, no78, no79,
        no101, no111, no121, no201, no301,
        no501, no511, no541, no551, no601, no701 );
      *)

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        // Provedor Thema:
        // 50|51|52|53|54|55|56|57|58|59|60|61|62|63|64|65|66|67|68|69|70|71|72|78|79
        proThema:
          NaturezaOperacao := no51;
      else
        NaturezaOperacao := no1;
      end;

      {
        TnfseRegimeEspecialTributacao = (retNenhum, retMicroempresaMunicipal, retEstimativa,
                                         retSociedadeProfissionais, retCooperativa,
                                         retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
                                         retLucroReal, retLucroPresumido, retSimplesNacional,
                                         retImune, retEmpresaIndividualRELI, retEmpresaPP,
                                         retMicroEmpresario, retOutros, retMovimentoMensal,
                                         retISSQNAutonomos, retISSQNSociedade,
                                         retNotarioRegistrador,
                                         retTribFaturamentoVariavel, retFixo,
                                         retIsencao,retExigibSuspensaJudicial,
                                         retExigibSuspensaAdm);
      }
      RegimeEspecialTributacao := retMicroempresaMunicipal;

      // TnfseSimNao = ( snSim, snNao );
      OptanteSimplesNacional := snSim;

      // TnfseSimNao = ( snSim, snNao );
      IncentivadorCultural := snNao;

      // Provedor Tecnos
      PercentualCargaTributaria := 0;
      ValorCargaTributaria := 0;
      PercentualCargaTributariaMunicipal := 0;
      ValorCargaTributariaMunicipal := 0;
      PercentualCargaTributariaEstadual := 0;
      ValorCargaTributariaEstadual := 0;

      // TnfseSimNao = ( snSim, snNao );
      // snSim = Ambiente de Produção
      // snNao = Ambiente de Homologação
      {=== Usado pelo provedor ISSDigital ===}
      if ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taProducao then
        Producao := snSim
      else
        Producao := snNao;

      // TnfseStatusRPS = ( srNormal, srCancelado );
      StatusRps := srNormal;

      // Somente Os provedores Betha, DataSmart, FISSLex, SimplISS e TcheInfo
      // permitem incluir no RPS a TAG: OutrasInformacoes os demais essa TAG
      // é gerada e preenchida pelo WebService do provedor.
      OutrasInformacoes := 'Pagamento a Vista';

      {=========================================================================
        Numero, Série e Tipo do Rps que esta sendo substituido por este
      =========================================================================}

      {
       RpsSubstituido.Numero := FormatFloat('#########0', i);
       RpsSubstituido.Serie  := 'UNICA';
       // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
       RpsSubstituido.Tipo   := trRPS;
      }

      {=========================================================================
        Dados do Serviço (valores)
      =========================================================================}

      // Provedores que permitem informar mais de 1 serviço:
      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proEloTech, profintelISS,
          proSimplISS] then
      begin
        // Provedor Elotech
        Servico.Valores.RetidoPis := snNao;
        Servico.Valores.RetidoCofins := snNao;
        Servico.Valores.AliquotaInss := 0;
        Servico.Valores.RetidoInss := snNao;
        Servico.Valores.AliquotaIr := 0;
        Servico.Valores.RetidoIr := snNao;
        Servico.Valores.AliquotaCsll := 0;
        Servico.Valores.RetidoCsll := snNao;

        with Servico.ItemServico.New do
        begin
          Descricao := 'Desc. do Serv. 1';
          ItemListaServico := '09.01';

          ValorDeducoes := 0;
          xJustDeducao := '';

          AliqReducao := 0;
          ValorReducao := 0;

          DescontoIncondicionado := 0;
          DescontoCondicionado := 0;

          // TUnidade = (tuHora, tuQtde);
          TipoUnidade := tuQtde;
          Unidade := 'UN';
          Quantidade := 10;
          ValorUnitario := 5;

          QtdeDiaria := 0;
          ValorTaxaTurismo := 0;

          ValorTotal := Quantidade * ValorUnitario;

          BaseCalculo := ValorTotal - ValorDeducoes - DescontoIncondicionado;

          Aliquota := 2;

          ValorISS := BaseCalculo * Aliquota / 100;

          ValorISSRetido := 0;

          AliqISSST := 0;
          ValorISSST := 0;

          ValorBCCSLL := 0;
          AliqRetCSLL := 0;
          ValorCSLL := 0;

          ValorBCPIS := 0;
          AliqRetPIS := 0;
          ValorPIS := 0;

          ValorBCCOFINS := 0;
          AliqRetCOFINS := 0;
          ValorCOFINS := 0;

          ValorBCINSS := 0;
          AliqRetINSS := 0;
          ValorINSS := 0;

          ValorBCRetIRRF := 0;
          AliqRetIRRF := 0;
          ValorIRRF := 0;

          // Provedor EloTech
          Tributavel := snNao;

          CodigoCnae := '6203100';
        end;
      end
      else
      begin
        Servico.Valores.ValorServicos := 100.35;
        Servico.Valores.ValorDeducoes := 0.00;
        Servico.Valores.AliquotaPis := 0.00;
        Servico.Valores.ValorPis := 0.00;
        Servico.Valores.AliquotaCofins := 2.00;
        Servico.Valores.ValorCofins := 2.00;
        Servico.Valores.ValorInss := 0.00;
        Servico.Valores.ValorIr := 0.00;
        Servico.Valores.ValorCsll := 0.00;
        // Usado pelo provedor SystemPro
        Servico.Valores.ValorTaxaTurismo := 0.00;
        Servico.Valores.QtdeDiaria := 0.00;

        // TnfseSituacaoTributaria = ( stRetencao, stNormal, stSubstituicao );
        // stRetencao = snSim
        // stNormal   = snNao

        // Neste exemplo não temos ISS Retido ( stNormal = Não )
        // Logo o valor do ISS Retido é igual a zero.
        Servico.Valores.IssRetido := stNormal;
        Servico.Valores.ValorIssRetido := 0.00;

        Servico.Valores.OutrasRetencoes := 0.00;
        Servico.Valores.DescontoIncondicionado := 0.00;
        Servico.Valores.DescontoCondicionado := 0.00;

        Servico.Valores.BaseCalculo := Servico.Valores.ValorServicos -
        Servico.Valores.ValorDeducoes - Servico.Valores.DescontoIncondicionado;

        Servico.Valores.Aliquota := 2;

        Servico.Valores.tribMun.cPaisResult := 0;
        // TtribISSQN = (tiOperacaoTributavel, tiImunidade, tiExportacao, tiNaoIncidencia);
        Servico.Valores.tribMun.tribISSQN := tiNaoIncidencia;
        Servico.Valores.tribMun.tpImunidade := timNenhum;
        Servico.Valores.tribMun.tpRetISSQN := trNaoRetido;
        Servico.Valores.totTrib.indTotTrib := indNao;

        if OptanteSN = osnOptanteMEEPP then
        begin
          Servico.Valores.totTrib.indTotTrib := indSim;
          Servico.Valores.totTrib.pTotTribSN := 2.01;
        end;

        vValorISS := Servico.Valores.BaseCalculo * Servico.Valores.Aliquota / 100;

        // A função RoundTo5 é usada para arredondar valores, sendo que o segundo
        // parametro se refere ao numero de casas decimais.
        // exemplos: RoundTo5(50.532, -2) ==> 50.53
        // exemplos: RoundTo5(50.535, -2) ==> 50.54
        // exemplos: RoundTo5(50.536, -2) ==> 50.54

        Servico.Valores.ValorISS := RoundTo5(vValorISS, -2);

        Servico.Valores.ValorLiquidoNfse := Servico.Valores.ValorServicos -
          Servico.Valores.ValorPis - Servico.Valores.ValorCofins -
          Servico.Valores.ValorInss - Servico.Valores.ValorIr -
          Servico.Valores.ValorCsll - Servico.Valores.OutrasRetencoes -
          Servico.Valores.ValorIssRetido - Servico.Valores.DescontoIncondicionado
          - Servico.Valores.DescontoCondicionado;
      end;

      {=========================================================================
        Dados do Serviço
      =========================================================================}

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proSiapSistemas:
          // código padrão ABRASF acrescido de um sub-item
          Servico.ItemListaServico := '01.05.00';
      else
        // código padrão da ABRASF
        Servico.ItemListaServico := '09.01';
      end;

//      servico.CodigoNBS := '123456789';
      Servico.Discriminacao := 'discriminacao I; discriminacao II;' +
                               'discriminacao III; discriminacao IV';

      // TnfseResponsavelRetencao = ( rtTomador, rtPrestador, rtIntermediario, rtNenhum )
      //                              '1',       '',          '2',             ''
      Servico.ResponsavelRetencao := rtTomador;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proISSSJP:
          Servico.CodigoTributacaoMunicipio := '631940000';

        proCenti:
          Servico.CodigoTributacaoMunicipio := '0901';

        proISSSalvador:
          Servico.CodigoTributacaoMunicipio := '0901001';

        proIPM, proSystemPro:
          Servico.CodigoTributacaoMunicipio := '';
      else
        Servico.CodigoTributacaoMunicipio := '63194';
      end;

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proSystemPro:
          Servico.CodigoCnae := '';
      else
        // código com 7 digitos
        Servico.CodigoCnae := '6203100';
      end;

      // Para o provedor ISSNet em ambiente de Homologação
      // o Codigo do Municipio tem que ser '999'
      Servico.CodigoMunicipio := edtCodCidade.Text;
      Servico.UFPrestacao := 'SP';

      // Informar A Exigibilidade ISS para fintelISS [1/2/3/4/5/6/7]
      Servico.ExigibilidadeISS := exiExigivel;

      Servico.CodigoPais := 1058; // Brasil
      Servico.MunicipioIncidencia := StrToIntDef(edtCodCidade.Text, 0);

      {=========================================================================
        Dados do Prestador de Serviço
      =========================================================================}
      Prestador.IdentificacaoPrestador.CpfCnpj := edtEmitCNPJ.Text;
      Prestador.IdentificacaoPrestador.InscricaoMunicipal := edtEmitIM.Text;
      Prestador.RazaoSocial  := edtEmitRazao.Text;
      Prestador.NomeFantasia := edtEmitRazao.Text;
      // Para o provedor ISSDigital deve-se informar também:
      Prestador.cUF := UFtoCUF(edtEmitUF.Text);

      Prestador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Prestador.Endereco.Endereco := edtEmitLogradouro.Text;
      Prestador.Endereco.Numero   := edtEmitNumero.Text;
      Prestador.Endereco.Bairro   := edtEmitBairro.Text;
      Prestador.Endereco.xMunicipio := edtEmitCidade.Text;
      Prestador.Endereco.UF := edtEmitUF.Text;
      Prestador.Endereco.CodigoPais := 0;
      Prestador.Endereco.xPais := 'BRASIL';
      Prestador.Endereco.CEP := edtEmitCEP.Text;

      Prestador.Contato.Telefone := '33224455';
      Prestador.Contato.Email := 'nome@provedor.com.br';

      {=========================================================================
        Dados do Tomador de Serviço
      =========================================================================}

      Tomador.AtualizaTomador := snNao;
      Tomador.TomadorExterior := snNao;

      Tomador.IdentificacaoTomador.CpfCnpj := '12345678901';
      Tomador.IdentificacaoTomador.InscricaoMunicipal := '';
      Tomador.IdentificacaoTomador.InscricaoEstadual := '';

      Tomador.RazaoSocial := 'INSCRICAO DE TESTE E TESTE';

      Tomador.Endereco.TipoLogradouro := 'RUA';
      Tomador.Endereco.Endereco := 'RUA PRINCIPAL';
      Tomador.Endereco.Numero := '100';
      Tomador.Endereco.Complemento := 'APTO 11';
      Tomador.Endereco.TipoBairro := 'BAIRRO';
      Tomador.Endereco.Bairro := 'CENTRO';
      Tomador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Tomador.Endereco.xMunicipio := 'Cidade do Tomador';
      Tomador.Endereco.UF := edtEmitUF.Text;
      Tomador.Endereco.CodigoPais := 0;
      Tomador.Endereco.CEP := '14800000';
      Tomador.Endereco.xPais := 'BRASIL';
      Tomador.Contato.Telefone := '22223333';
      Tomador.Contato.Email := 'nome@provedor.com.br';

      {=========================================================================
        Dados do Intermediario na prestação do serviço
      =========================================================================}

//       Intermediario.RazaoSocial := 'razao';
//       Intermediario.Identificacao.CpfCnpj := '00000000000';
//       Intermediario.Identificacao.InscricaoMunicipal := '12547478';

      {=========================================================================
        Dados da Obra (quando o serviço for uma obra)
      =========================================================================}

//      ConstrucaoCivil.CodigoObra := '88888';
//      ConstrucaoCivil.Art        := '433';

      {=========================================================================
        Dados da Condição de Pagamento (provedor Betha e Publica
        versão 1 do Layout da ABRASF)
      =========================================================================}

      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proBetha, proPublica] then
      begin
        // Condicao = (cpAVista, cpNaApresentacao, cpAPrazo, cpCartaoCredito, cpCartaoDebito)
        CondicaoPagamento.Condicao   := cpAVista;
        CondicaoPagamento.QtdParcela := 1;

        for i := 1 to CondicaoPagamento.QtdParcela do
        begin
          with CondicaoPagamento.Parcelas.New do
          begin
            Parcela := IntToStr(i);
            DataVencimento := Date + (30 * i);
            Valor := (Servico.Valores.ValorLiquidoNfse / CondicaoPagamento.QtdParcela);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmACBrNFSe.AtualizarCidades;
var
  IniCidades: TMemIniFile;
  Cidades: TStringList;
  I: Integer;
  sNome, sCod, sUF: String;
begin
  IniCidades := TMemIniFile.Create('');
  try
    Cidades := TStringList.Create;
    try
      IniCidades.SetStrings(ACBrNFSeX1.Configuracoes.WebServices.Params);
      IniCidades.ReadSections(Cidades);
      cbCidades.Items.Clear;

      for I := 0 to Pred(Cidades.Count) do
      begin
        if (StrToIntdef(Cidades[I], 0) > 0) then
        begin
          //Exemplo: Alfenas/3101607/MG
          sCod  := Cidades[I];
          sNome := IniCidades.ReadString(sCod, 'Nome', '');
          sUF   := IniCidades.ReadString(sCod, 'UF', '');

          cbCidades.Items.Add(Format('%s/%s/%s', [sNome, sCod, sUF]));
        end;
      end;

      //Sort
      cbCidades.Sorted := false;
      cbCidades.Sorted := true;
      edtTotalCidades.Text := IntToStr(cbCidades.Items.Count);
    finally
      FreeAndNil(Cidades);
    end;
  finally
    IniCidades.Free;
  end;
end;

procedure TfrmACBrNFSe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrNFSeX1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrNFSeX1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrNFSeX1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrNFSeX1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrNFSeX1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrNFSe.btnCancNFSeClick(Sender: TObject);
var
  Titulo, NumNFSe, Codigo, Motivo, NumLote, CodVerif, SerNFSe, NumRps,
  SerRps, ValNFSe, ChNFSe, eMailTomador, vNumRPS, xCodServ,
  xDataEmissao: String;
  DataEmissao: TDateTime;
  CodCanc: Integer;
  InfCancelamento: TInfCancelamento;
begin
  Titulo := 'Cancelar NFSe';
  DataEmissao := 0;

  // Os Provedores da lista requerem que seja informado a chave e o código
  // de cancelamento
  if (ACBrNFSeX1.Configuracoes.Geral.Provedor = proInfisc) and
     (ACBrNFSeX1.Configuracoes.Geral.Versao <> ve201) then
  begin
    {
      A Chave é composta por:
       2 | N |Código IBGE para UF do prestador
      14 | N |CNPJ do prestador
       2 | N |Modelo da nota (valor 98 por padrão)
       3 | C |Série da nota (em maiúsculas, com zeros à direita)
       9 | N |Número da nota (com zeros à esquerda)
       9 | N |Código numérico aleatório
    }
    ChNFSe := '434945460000011998000000976482769641000';
    if not (InputQuery(Titulo, 'Chave da NFSe', ChNFSe)) then
      exit;

    Codigo := '1';
    if not (InputQuery(Titulo, 'Código de Cancelamento', Codigo)) then
      exit;
  end
  else
  begin
    NumNFSe := '';
    if not (InputQuery(Titulo, 'Numero da NFSe', NumNFSe)) then
      exit;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proiiBrasil, proWebFisco,
      proSimple, proFGMaiss, proIPM, proPriMax, proSigISSWeb] then
    begin
      SerNFSe := '1';
      if not (InputQuery(Titulo, 'Série da NFSe', SerNFSe)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proConam then
    begin
      SerNFSe := '1';
      if not (InputQuery(Titulo, 'Série da NFSe', SerNFSe)) then
        exit;

      NumRps := '';
      if not (InputQuery(Titulo, 'Numero do RPS', NumRps)) then
        exit;

      SerRps := '';
      if not (InputQuery(Titulo, 'Série do Rps', SerRps)) then
        exit;

      ValNFSe := '';
      if not (InputQuery(Titulo, 'Valor da NFSe', ValNFSe)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proAssessorPublico then
    begin
      NumRps := '';
      if not (InputQuery(Titulo, 'Numero do RPS', NumRps)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proCTAConsult then
    begin
      ChNFSe := '434945460000011998000000976482769641000';
      if not (InputQuery(Titulo, 'Chave da NFSe', ChNFSe)) then
        exit;
    end;

    // Codigo de Cancelamento
    // 1 - Erro de emissão
    // 2 - Serviço não concluido
    // 3 - RPS Cancelado na Emissão

    Codigo := '1';
    if not (InputQuery(Titulo, 'Código de Cancelamento', Codigo)) then
      exit;

    // Provedor SigEp - O código de cancelamento é diferente
    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proSigep then
    begin
      CodCanc := StrToIntDef(Codigo, 1);

      case CodCanc of
        1: Codigo := 'EE';
        2: Codigo := 'ED';
        3: Codigo := 'OU';
        4: Codigo := 'SB';
      end;
    end;

    // Os Provedores da lista requerem que seja informado o motivo do cancelamento
    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proAgili, proAssessorPublico,
      proConam, proEquiplano, proFGMaiss, proGoverna, proIPM, proISSBarueri,
      proISSDSF, proISSLencois, proModernizacaoPublica, proPrescon, proPriMax, proPublica,
      proSiat, proSigISS, proSigep, proSimple, proSmarAPD, proSudoeste, proTecnos,
      proWebFisco, proCenti, proCTA, proBauhaus, proSigISSWeb] then
    begin
      Motivo := 'Motivo do Cancelamento';
      if not (InputQuery(Titulo, 'Motivo do Cancelamento', Motivo)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proAssessorPublico then
    begin
      NumLote := '1';
      if not (InputQuery(Titulo, 'Numero do Lote', NumLote)) then
        exit;
    end;

    // Os Provedores da lista requerem que seja informado o código de verificação
    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proInfisc, proISSDSF, proSiappa,
         proISSLencois, proGoverna, proSiat, proSigep, proElotech, proCenti] then
    begin
      CodVerif := '12345678';
      if not (InputQuery(Titulo, 'Código de Verificação ou Chave de Autenticação', CodVerif)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proCenti then
    begin
      ChNFSe := '';
      if not (InputQuery(Titulo, 'Identificador da NFSe', ChNFSe)) then
        exit;
    end;

    // Os Provedores da lista requerem que seja informado a Data de Emissão e o Código do Serviço
    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proSiappa] then
    begin
      xDataEmissao := FormatDateTime('MM/YYYY',Date);
      if not (InputQuery(Titulo, 'Período (MM/AAAA):', xDataEmissao)) then
        exit;
      DataEmissao := StrToDateDef('01/' + xDataEmissao, 0);

      xCodServ := '702';
      if not(InputQuery(Titulo, 'Codigo do Serviço', xCodServ)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proCTAConsult then
    begin
      xDataEmissao := FormatDateTime('DD/MM/YYYY', Date);
      if not (InputQuery(Titulo, 'Data de Emissão (DD/MM/AAAA):', xDataEmissao)) then
        exit;
      DataEmissao := StrToDateDef(xDataEmissao, 0);
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor = proSigISS then
    begin
      eMailTomador := '';
      if not (InputQuery(Titulo, 'eMail do Tomador', eMailTomador)) then
        exit;
    end;
  end;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proISSBarueri then
  begin
    vNumRPS := '';
    if not(InputQuery('Substituir NFS-e', 'Numero do novo RPS', vNumRPS)) then
      exit;

    ACBrNFSeX1.NotasFiscais.Clear;
    Alimentar_Componente(vNumRPS, '1');
  end;

  InfCancelamento := TInfCancelamento.Create;

  try
    with InfCancelamento do
    begin
      NumeroNFSe      := NumNFSe;
      SerieNFSe       := SerNFSe;
      ChaveNFSe       := ChNFSe;
      CodCancelamento := Codigo;
      MotCancelamento := Motivo;
      NumeroLote      := NumLote;
      NumeroRps       := StrToIntDef(NumRps, 0);
      SerieRps        := SerRps;
      ValorNFSe       := StrToFloatDef(ValNFSe, 0);
      CodVerificacao  := CodVerif;
      email           := eMailTomador;
      DataEmissaoNFSe := DataEmissao;
      CodServ         := xCodServ;
    end;

    ACBrNFSeX1.CancelarNFSe(InfCancelamento);
  finally
    InfCancelamento.Free;
  end;

  ChecarResposta(tmCancelarNFSe);
end;

procedure TfrmACBrNFSe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSeX1.SSL.CertCNPJ);
end;

procedure TfrmACBrNFSe.btnConsultarEventoChavPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xTipoEvento, xNumSeqEvento: string;
  Ok: Boolean;
begin
  xTitulo := 'Consultar Evento pela chave da NFSe';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  {
   'e101101', 'e105102', 'e101103', 'e105104', 'e105105', 'e202201', 'e203202',
   'e204203', 'e205204', 'e202205', 'e203206', 'e204207', 'e205208', 'e305101',
   'e305102', 'e305103'
  }
  xTipoEvento := 'e101101';
  if not(InputQuery(xTitulo, 'Tipo de Evento:', xTipoEvento)) then
    exit;

  xNumSeqEvento := '';
  if not(InputQuery(xTitulo, 'Numero Sequencial do Evento:', xNumSeqEvento)) then
    exit;

  if (xChaveNFSe <> '') and (xTipoEvento = '') and (xNumSeqEvento = '') then
    ACBrNFSeX1.ConsultarEvento(xChaveNFSe);

  if (xChaveNFSe <> '') and (xTipoEvento <> '') and (xNumSeqEvento = '') then
    ACBrNFSeX1.ConsultarEvento(xChaveNFSe, StrTotpEvento(Ok, xTipoEvento));

  if (xChaveNFSe <> '') and (xTipoEvento <> '') and (xNumSeqEvento <> '') then
    ACBrNFSeX1.ConsultarEvento(xChaveNFSe, StrTotpEvento(Ok, xTipoEvento),
      StrToIntDef(xNumSeqEvento, 1));

  ChecarResposta(tmConsultarEvento);
end;

procedure TfrmACBrNFSe.btnConsultarDFeClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xNSU: string;
begin
  xTitulo := 'Consultar DFe pela chave da NFSe ou NSU';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  xNSU := '';
  if not(InputQuery(xTitulo, 'NSU - Numero Sequencial Unico:', xNSU)) then
    exit;

  if (xChaveNFSe <> '') and (xNSU = '') then
    ACBrNFSeX1.ConsultarDFe(xChaveNFSe);

  if (xChaveNFSe = '') and (xNSU <> '') then
    ACBrNFSeX1.ConsultarDFe(StrToIntDef(xNSU, 0));

  ChecarResposta(tmConsultarDFe);
end;

procedure TfrmACBrNFSe.btnConsultarLinkNFSeClick(Sender: TObject);
var
  InfConsultaLinkNFSe: TInfConsultaLinkNFSe;
  xTitulo, xCompetencia, xNumeroNFSe, xSerieNFSe,
  xNumeroRps, xSerieRps, xTomador: string;
begin
  xTitulo := 'Consultar Link da NFSe';

  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proConam then
  begin
    xCompetencia := '07/07/2023';
    if not(InputQuery(xTitulo, 'Data de Competencia:', xCompetencia)) then
      exit;

    xNumeroRps := '';
    if not(InputQuery(xTitulo, 'Numero do RPS:', xNumeroRps)) then
      exit;

    xSerieRps := '';
    if not(InputQuery(xTitulo, 'Serie do RPS:', xSerieRps)) then
      exit;
  end;

  xNumeroNFSe := '';
  if not(InputQuery(xTitulo, 'Numero da NFS-e:', xNumeroNFSe)) then
    exit;

  xSerieNFSe := '';
  if not(InputQuery(xTitulo, 'Serie da NFS-e:', xSerieNFSe)) then
    exit;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proGeisWeb then
  begin
    xTomador := '';
    if not(InputQuery(xTitulo, 'CNPJ/CPF do Tomador:', xTomador)) then
      exit;
  end;

  InfConsultaLinkNFSe := TInfConsultaLinkNFSe.Create;
  try
    InfConsultaLinkNFSe.Competencia := StrToDateDef(xCompetencia, 0);
    InfConsultaLinkNFSe.NumeroNFSe := xNumeroNFSe;
    InfConsultaLinkNFSe.SerieNFSe := xSerieNFSe;
    InfConsultaLinkNFSe.NumeroRps := StrToIntDef(xNumeroRps, 1);
    InfConsultaLinkNFSe.SerieRps := xSerieRps;
    InfConsultaLinkNFSe.CnpjCpfToma := xTomador;

    ACBrNFSeX1.ConsultarLinkNFSe(InfConsultaLinkNFSe);

   ChecarResposta(tmConsultarLinkNFSe);
  finally
    InfConsultaLinkNFSe.Free;
  end;
end;

procedure TfrmACBrNFSe.btnConsultarLoteClick(Sender: TObject);
var
  Protocolo, Lote: String;
begin
  Protocolo := '';
  if not (InputQuery('Consultar Lote', 'Número do Protocolo (Obrigatório):', Protocolo)) then
    exit;

  Lote := '';
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proAssessorPublico, proElotech,
       proInfisc, proIPM, proISSDSF, proEquiplano, proeGoverneISS, proGeisWeb,
       proSiat, proISSSaoPaulo] then
  begin
    if not (InputQuery('Consultar Lote', 'Número do Lote:', Lote)) then
      exit;
  end;

  ACBrNFSeX1.ConsultarLoteRps(Protocolo, Lote);

  ChecarResposta(tmConsultarLote);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeFaixaClick(Sender: TObject);
var
  xTitulo, NumNFSeIni, NumNFSeFin, NumPagina: String;
begin
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proPrescon] then
  begin
    NumPagina := '1';
  end
  else
  begin
    xTitulo := 'Consultar NFSe Por Faixa';

    NumNFSeIni := '';
    if not(InputQuery(xTitulo, 'Numero da NFSe Inicial:', NumNFSeIni)) then
      exit;

    NumNFSeFin := NumNFSeIni;
    if not(InputQuery(xTitulo, 'Numero da NFSe Final:', NumNFSeFin)) then
      exit;

    NumPagina := '1';
    if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
      exit;
  end;

  ACBrNFSeX1.ConsultarNFSePorFaixa(NumNFSeIni, NumNFSeFin, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSePorFaixa);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeGenericoClick(Sender: TObject);
var
  xTitulo, NumIniNFSe, NumFinNFSe, SerNFSe, DataIni, DataFin,
  CPFCNPJ_Prestador, IM_Prestador, CPFCNPJ_Tomador, IM_Tomador,
  CPFCNPJ_Inter, IM_Inter, NumLote, CadEcon, NumPagina: String;
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  xTitulo := 'Consultar NFSe Genérico';

  NumIniNFSe := '';
  if not(InputQuery(xTitulo, 'Numero Inicial da NFSe:', NumIniNFSe)) then
    exit;

  NumFinNFSe := '';
  if not(InputQuery(xTitulo, 'Numero Final da NFSe:', NumFinNFSe)) then
    exit;

  SerNFSe := '1';
  if not (InputQuery(xTitulo, 'Série da NFSe', SerNFSe)) then
    exit;

  DataIni := DateToStr(Date);
  if not (InputQuery(xTitulo, 'Data Inicial (DD/MM/AAAA):', DataIni)) then
    exit;

  DataFin := DateToStr(Date);
  if not (InputQuery(xTitulo, 'Data Final (DD/MM/AAAA):', DataFin)) then
    exit;

  CPFCNPJ_Prestador := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Prestador:', CPFCNPJ_Prestador)) then
    exit;

  IM_Prestador := '';
  if not(InputQuery(xTitulo, 'I.M. Prestador:', IM_Prestador)) then
    exit;

  CPFCNPJ_Tomador := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Tomador:', CPFCNPJ_Tomador)) then
    exit;

  IM_Tomador := '';
  if not(InputQuery(xTitulo, 'I.M. Tomador:', IM_Tomador)) then
    exit;

  CPFCNPJ_Inter := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Intermediário:', CPFCNPJ_Inter)) then
    exit;

  IM_Inter := '';
  if not(InputQuery(xTitulo, 'I.M. Intermediário:', IM_Inter)) then
    exit;

  NumLote := '1';
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat] then
  begin
    if not(InputQuery(xTitulo, 'Numero do Lote:', NumLote)) then
      exit;
  end;

  CadEcon := '';
  if not(InputQuery(xTitulo, 'Cadastro Economico (Insc. Munic.):', CadEcon)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      // Valores aceito para o Tipo de Consulta:
      // tcPorNumero, tcPorFaixa, tcPorPeriodo, tcServicoPrestado, tcServicoTomado
      tpConsulta := tcPorNumero;

      // Necessário para a consulta por numero e por faixa
      NumeroIniNFSe := NumIniNFSe;
      NumeroFinNFSe := NumFinNFSe;
      SerieNFSe := SerNFSe;

      // Valores aceito para o Tipo de Periodo:
      // tpEmissao, tpCompetencia
      tpPeriodo := tpEmissao;

      // Necessário para consulta por periodo
      DataInicial := StrToDateDef(DataIni, 0);
      DataFinal   := StrToDateDef(DataFin, 0);

      // Necessário para consulta a serviço tomado por prestador
      CNPJPrestador := CPFCNPJ_Prestador;
      IMPrestador   := IM_Prestador;

      // Necessário para consulta a serviço prestado ou tomado por tomador
      CNPJTomador := CPFCNPJ_Tomador;
      IMTomador   := IM_Tomador;

      // Necessário para consulta a serviço prestado ou tomado por Intermediário
      CNPJInter := CPFCNPJ_Inter;
      IMInter   := IM_Inter;

      // Necessário para os provedores: proISSDSF, proSiat
      NumeroLote := NumLote;

      // Necessário para os provedores que seguem a versão 2 do layout da ABRASF
      Pagina := StrToIntDef(NumPagina, 1);

      CadEconomico := CadEcon;
    end;

    ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
  finally
    InfConsultaNFSe.Free;
  end;

  ChecarResposta(tmConsultarNFSe);
end;

procedure TfrmACBrNFSe.btnConsultarNFSePeloNumeroClick(Sender: TObject);
var
  xTitulo, NumeroNFSe, SerNFSe, NumPagina, NumLote, xDataIni, xDataFin,
  xCodServ, xCodVerif: String;
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  xTitulo := 'Consultar NFSe Por Numero';

  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proPadraoNacional] then
  begin
    NumeroNFSe := '';
    if not(InputQuery(xTitulo, 'Chave da NFS-e:', NumeroNFSe)) then
      exit;
  end
  else
  begin
    NumeroNFSe := '';
    if not(InputQuery(xTitulo, 'Numero da NFSe:', NumeroNFSe)) then
      exit;

    SerNFSe := '';
    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proIPM] then
    begin
      if not(InputQuery(xTitulo, 'Série da NFSe:', SerNFSe)) then
        exit;
    end;

    NumLote := '1';
    xDataIni := '';
    xDataFin := '';

    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proAssessorPublico] then
    begin
      if not(InputQuery(xTitulo, 'Numero do Lote:', NumLote)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat] then
    begin
      if not(InputQuery(xTitulo, 'Numero do Lote:', NumLote)) then
        exit;

      xDataIni := DateToStr(Date);
      if not (InputQuery(xTitulo, 'Data Inicial (DD/MM/AAAA):', xDataIni)) then
        exit;

      xDataFin := DateToStr(Date);
      if not (InputQuery(xTitulo, 'Data Final (DD/MM/AAAA):', xDataFin)) then
        exit;
    end;

    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proSiappa] then
    begin
      xDataIni := FormatDateTime('MM/YYYY',Date);
      if not (InputQuery(xTitulo, 'Período (MM/AAAA):', xDataIni)) then
        exit;

      if not(InputQuery(xTitulo, 'Codigo do Serviço', xCodServ)) then
        exit;

      if not(InputQuery(xTitulo, 'Codigo de Verificacao', xCodVerif)) then
        exit;
    end;

    NumPagina := '1';
    if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
      exit;
  end;

  case ACBrNFSeX1.Configuracoes.Geral.Provedor of
    proISSDSF,
    proSiat:
      begin
        InfConsultaNFSe := TInfConsultaNFSe.Create;

        try
          with InfConsultaNFSe do
          begin
            tpConsulta := tcPorNumero;

            NumeroIniNFSe := NumeroNFSe;
            NumeroLote := NumLote;
            DataInicial :=  StrToDateDef(xDataIni, 0);
            DataFinal := StrToDateDef(xDataFin, 0);
            Pagina := StrToIntDef(NumPagina, 1);
          end;

          ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
        finally
          InfConsultaNFSe.Free;
        end;
      end;
{
    proFGMaiss,
    proPriMax,
    proWebFisco:
      begin
        InfConsultaNFSe := TInfConsultaNFSe.Create;

        try
          with InfConsultaNFSe do
          begin
            tpConsulta := tcPorNumero;

            NumeroIniNFSe := NumeroNFSe;
          end;

          ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
        finally
          InfConsultaNFSe.Free;
        end;
      end;
}
    proAssessorPublico:
      begin
        InfConsultaNFSe := TInfConsultaNFSe.Create;

        try
          with InfConsultaNFSe do
          begin
            tpConsulta := tcPorNumero;

            NumeroIniNFSe := NumeroNFSe;
            NumeroLote := NumLote;
            Pagina := StrToIntDef(NumPagina, 1);
          end;

          ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
        finally
          InfConsultaNFSe.Free;
        end;
      end;

    proIPM:
      begin
        InfConsultaNFSe := TInfConsultaNFSe.Create;

        try
          with InfConsultaNFSe do
          begin
            tpConsulta := tcPorNumero;

            NumeroIniNFSe := NumeroNFSe;
            SerieNFSe := SerNFSe;
            CadEconomico := edtEmitIM.Text;
          end;

          ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
        finally
          InfConsultaNFSe.Free;
        end;
      end;

    proSiappa:
      begin
        InfConsultaNFSe := TInfConsultaNFSe.Create;

        try
          with InfConsultaNFSe do
          begin
            tpConsulta := tcPorNumero;

            NumeroIniNFSe := NumeroNFSe;
            DataInicial := StrToDateDef('01/' + xDataIni, 0);
            CodServ := xCodServ;
            CodVerificacao := xCodVerif;
          end;

          ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
        finally
          InfConsultaNFSe.Free;
        end;
      end;
  else
    ACBrNFSeX1.ConsultarNFSeporNumero(NumeroNFSe);
  end;

  ChecarResposta(tmConsultarNFSe);
end;

procedure TfrmACBrNFSe.btnConsultarNFSePelaChaveClick(Sender: TObject);
var
  xTitulo, xChaveNFSe: String;
begin
  xTitulo := 'Consultar NFSe pela Chave de Acesso da NFSe';

  xChaveNFSe := '';
  if not (InputQuery(xTitulo, 'Chave de Acesso:', xChaveNFSe)) then
    exit;

  ACBrNFSeX1.ConsultarNFSePorChave(xChaveNFSe);

  ChecarResposta(tmConsultarNFSePorChave);
end;

procedure TfrmACBrNFSe.btnConsultarNFSePelaChavePNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe: String;
begin
  xTitulo := 'Consultar NFSe pela Chave da NFSe';

  xChaveNFSe := '';
  if not (InputQuery(xTitulo, 'Chave da NFSe:', xChaveNFSe)) then
    exit;

  ACBrNFSeX1.ConsultarNFSePorChave(xChaveNFSe);

  ChecarResposta(tmConsultarNFSePorChave);
end;

procedure TfrmACBrNFSe.btnConsultarNFSePeriodoClick(Sender: TObject);
var
  xTitulo, DataIni, DataFin, NumPagina, NumLote: String;
begin
  xTitulo := 'Consultar NFSe por Período';

  DataIni := DateToStr(Date);
  if not (InputQuery(xTitulo, 'Data Inicial (DD/MM/AAAA):', DataIni)) then
    exit;

  DataFin := DateToStr(Date);
  if not (InputQuery(xTitulo, 'Data Final (DD/MM/AAAA):', DataFin)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  NumLote := '1';
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat] then
  begin
    if not(InputQuery(xTitulo, 'Numero do Lote:', NumLote)) then
      exit;
  end;

  ACBrNFSeX1.ConsultarNFSeporPeriodo(StrToDateDef(DataIni, 0),
    StrToDateDef(DataFin, 0), StrToIntDef(NumPagina, 1), NumLote);

  ChecarResposta(tmConsultarNFSePorFaixa);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeRPSClick(Sender: TObject);
var
  NumeroRps, SerieRps, TipoRps, CodVerificacao: String;
  iTipoRps: Integer;
begin
  NumeroRps := '';
  if not (InputQuery('Consultar NFSe por RPS', 'Numero do RPS:', NumeroRps)) then
    exit;

  SerieRps := '1';
  if not (InputQuery('Consultar NFSe por RPS', 'Serie do RPS:', SerieRps)) then
    exit;

  TipoRps := '1';
  if not (InputQuery('Consultar NFSe por RPS', 'Tipo do RPS:', TipoRps)) then
    exit;

  // Provedor ISSDSF e Siat
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat] then
  begin
    // Utilizado como serie da prestação
    SerieRps := '99';
  end;

  // Provedor SigEp o Tipo do RPS é diferente
  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proSigep then
  begin
    iTipoRps := StrToIntDef(TipoRps, 1);

    case iTipoRps of
      1: TipoRps := 'R1';
      2: TipoRps := 'R2';
      3: TipoRps := 'R3';
    end;
  end;

  // Provedor Agili o Tipo do RPS é diferente
  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proAgili then
  begin
    iTipoRps := StrToIntDef(TipoRps, 1);

    case iTipoRps of
      1: TipoRps := '-2';
      2: TipoRps := '-4';
      3: TipoRps := '-5';
    end;
  end;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proGiap, proGoverna,
     proPrescon, proIntertec] then
  begin
    CodVerificacao := '123';
    if not (InputQuery('Consultar NFSe por RPS', 'Codigo Verificação:', CodVerificacao)) then
      exit;
  end;

  ACBrNFSeX1.ConsultarNFSePorRps(NumeroRps, SerieRps, TipoRps, CodVerificacao);

  ChecarResposta(tmConsultarNFSePorRps);
end;

procedure TfrmACBrNFSe.btnConsultarDPSPorChavePNClick(Sender: TObject);
var
  xTitulo, xChaveDPS: String;
begin
  xTitulo := 'Consultar DPS por Chave';

  xChaveDPS := '';
  if not (InputQuery(xTitulo, 'Chave do DPS:', xChaveDPS)) then
    exit;

  ACBrNFSeX1.ConsultarDPSPorChave(xChaveDPS);

  ChecarResposta(tmConsultarNFSePorRps);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoPrestadoPorIntermediarioClick(
  Sender: TObject);
var
  xTitulo, NumPagina, CPFCNPJInter, IMInter: String;
begin
  xTitulo := 'Consultar NFSe Serviço Prestado Por Intermediário';

  CPFCNPJInter := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Intermediário:', CPFCNPJInter)) then
    exit;

  IMInter := '';
  if not(InputQuery(xTitulo, 'I.M. Intermediário:', IMInter)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoPrestadoPorIntermediario(CPFCNPJInter,
    IMInter, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoPrestado);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoPrestadoPorNumeroClick(Sender: TObject);
var
  xTitulo, NumeroNFSe, NumPagina: String;
begin
  xTitulo := 'Consultar NFSe Serviço Prestado Por Número';

  NumeroNFSe := '0';
  if not(InputQuery(xTitulo, 'Numero da NFSe:', NumeroNFSe)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoPrestadoPorNumero(NumeroNFSe, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoPrestado);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoPrestadoPorPeriodoClick(
  Sender: TObject);
var
  xTitulo, NumPagina, DataIni, DataFin: String;
begin
  xTitulo := 'Consultar NFSe Serviço Prestado Por Periodo';

  DataIni := DateToStr(Date);
  if not (InputQuery(xTitulo, 'Data Inicial (DD/MM/AAAA):', DataIni)) then
    exit;

  DataFin := DataIni;
  if not (InputQuery(xTitulo, 'Data Final (DD/MM/AAAA):', DataFin)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoPrestadoPorPeriodo(StrToDateDef(DataIni, 0),
    StrToDateDef(DataFin, 0), StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoPrestado);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoPrestadoPorTomadorClick(
  Sender: TObject);
var
  xTitulo, NumPagina, CPFCNPJTomador, IMTomador: String;
begin
  xTitulo := 'Consultar NFSe Serviço Prestado Por Tomador';

  CPFCNPJTomador := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Tomador:', CPFCNPJTomador)) then
    exit;

  IMTomador := '';
  if not(InputQuery(xTitulo, 'I.M. Tomador:', IMTomador)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoPrestadoPorTomador(CPFCNPJTomador,
    IMTomador, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoPrestado);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoTomadoPorIntermediarioClick(
  Sender: TObject);
var
  xTitulo, NumPagina, CPFCNPJInter, IMInter: String;
begin
  xTitulo := 'Consultar NFSe Serviço Tomado Por Intermediário';

  CPFCNPJInter := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Intermediário:', CPFCNPJInter)) then
    exit;

  IMInter := '';
  if not(InputQuery(xTitulo, 'I.M. Intermediário:', IMInter)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoTomadoPorIntermediario(CPFCNPJInter,
    IMInter, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoTomado);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoTomadoPorNumeroClick(Sender: TObject);
var
  xTitulo, NumeroNFSe, NumPagina: String;
begin
  xTitulo := 'Consultar NFSe Serviço Tomado Por Número';

  NumeroNFSe := '';
  if not(InputQuery(xTitulo, 'Numero da NFSe:', NumeroNFSe)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoTomadoPorNumero(NumeroNFSe, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoTomado);
end;

procedure TfrmACBrNFSe.btnConsultarSitLoteClick(Sender: TObject);
var
  Protocolo, Lote: String;
begin
  Protocolo := '';
  if not (InputQuery('Consultar Lote', 'Número do Protocolo (Obrigatório):', Protocolo)) then
    exit;

  Lote := '';
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proAssessorPublico,
      proEquiplano, proISSSaoPaulo] then
  begin
    if not (InputQuery('Consultar Lote', 'Número do Lote:', Lote)) then
      exit;
  end;

  ACBrNFSeX1.ConsultarSituacao(Protocolo, Lote);

  ChecarResposta(tmConsultarSituacao);
end;

procedure TfrmACBrNFSe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateTimeBr(ACBrNFSeX1.SSL.CertDataVenc));
end;

procedure TfrmACBrNFSe.btnEmitirClick(Sender: TObject);
var
  sQtde, vNumRPS, vNumLote: String;
  iQtde, iAux, I: Integer;
begin
  sQtde := '1';
  if not(InputQuery('Emitir', 'Quantidade de RPS', sQtde)) then
    exit;

  vNumRPS := '';
  if not(InputQuery('Emitir', 'Numero do RPS', vNumRPS)) then
    exit;

  vNumLote := vNumRPS;
  if not(InputQuery('Emitir', 'Numero do Lote', vNumLote)) then
    exit;

  iQtde := StrToIntDef(sQtde, 1);
  iAux := StrToIntDef(vNumRPS, 1);

  ACBrNFSeX1.NotasFiscais.Clear;

  for I := 1 to iQtde do
  begin
    vNumRPS := IntToStr(iAux);
    Alimentar_Componente(vNumRPS, vNumLote);
    inc(iAux);
  end;

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario, meTeste]
     aImprimir (Boolean) Valor Padrão = True, portanto imprime o DANFSE
  }
  // como não foi informado o segundo parâmetro o método assume o valor
  // meAutomatico, isso faz com que ele se ajusta ao provedor selecionado
  ACBrNFSeX1.Emitir(vNumLote);

  ChecarResposta(tmRecepcionar);
end;

procedure TfrmACBrNFSe.btnEmitirPNClick(Sender: TObject);
var
  vNumRPS, vNumLote: String;
begin
  vNumRPS := '';
  if not(InputQuery('Gerar e Enviar um RPS', 'Numero do RPS', vNumRPS)) then
    exit;

  vNumLote := vNumRPS;
  if not(InputQuery('Gerar e Enviar um RPS', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrNFSeX1.NotasFiscais.Clear;
  Alimentar_Componente(vNumRPS, vNumLote);

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario]
     aImprimir (Boolean) Valor Padrão = True, portanto imprime o DANFSE
  }
  // meUnitario: Ajusta o Emitir para enviar somente um Rps

  // No caso do Padrão Nacional o envio é sempre unitário
  ACBrNFSeX1.Emitir(vNumLote, meUnitario);

  ChecarResposta(tmGerar);
end;

procedure TfrmACBrNFSe.btnEnviaremailClick(Sender: TObject);
var
  vAux: String;
  sCC: TStrings;
begin
  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSeX1.NotasFiscais.Clear;
    ACBrNFSeX1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    if not(InputQuery('Enviar e-mail', 'Destinatário', vAux)) then
      exit;

    sCC := TStringList.Create;
    sCC.Clear; // Usando para add outros e-mail como Com-Cópia

    ACBrNFSeX1.NotasFiscais.Items[0].EnviarEmail(vAux, edtEmailAssunto.Text,
      mmEmailMsg.Lines, True // Enviar PDF junto
      , nil // Lista com emails que serão enviado cópias - TStrings
      , nil // Lista de anexos - TStrings
      );

    sCC.Free;

    memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArq);

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrNFSe.btnEnviaremailPNClick(Sender: TObject);
var
  vAux: String;
  sCC: TStrings;
begin
  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSeX1.NotasFiscais.Clear;
    ACBrNFSeX1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    if not(InputQuery('Enviar e-mail', 'Destinatário', vAux)) then
      exit;

    sCC := TStringList.Create;
    sCC.Clear; // Usando para add outros e-mail como Com-Cópia

    ACBrNFSeX1.NotasFiscais.Items[0].EnviarEmail(vAux, edtEmailAssunto.Text,
      mmEmailMsg.Lines, True // Enviar PDF junto
      , nil // Lista com emails que serão enviado cópias - TStrings
      , nil // Lista de anexos - TStrings
      );

    sCC.Free;

    memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArq);

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrNFSe.btnEventoAnaliseCancPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xCodigo, xMotivoCanc: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Analise de Cancelamento';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  xCodigo := '1';
  if not(InputQuery(xTitulo, 'Código de Cancelamento:', xCodigo)) then
    exit;

  xMotivoCanc := 'Motido do Cancelamento da nota';
  if not(InputQuery(xTitulo, 'Motivo do Cancelamento:', xMotivoCanc)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teAnaliseParaCancelamento;
      cMotivo := StrToIntDef(xCodigo, 1);
      xMotivo := xMotivoCanc;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoCancNFSePNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xCodigo, xMotivoCanc: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Cancelamento';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  xCodigo := '1';
  if not(InputQuery(xTitulo, 'Código de Cancelamento:', xCodigo)) then
    exit;

  xMotivoCanc := 'Motido do Cancelamento da nota';
  if not(InputQuery(xTitulo, 'Motivo do Cancelamento:', xMotivoCanc)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teCancelamento;
      cMotivo := StrToIntDef(xCodigo, 1);
      xMotivo := xMotivoCanc;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoCancPorSubPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xCodigo, xMotivoCanc, xChaveSub: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Cancelamento Por Substituição';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  xCodigo := '1';
  if not(InputQuery(xTitulo, 'Código de Cancelamento:', xCodigo)) then
    exit;

  xMotivoCanc := 'Motido do Cancelamento da nota';
  if not(InputQuery(xTitulo, 'Motivo do Cancelamento:', xMotivoCanc)) then
    exit;

  xChaveSub := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e Substitutiva:', xChaveSub)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teCancelamentoSubstituicao;
      cMotivo := StrToIntDef(xCodigo, 1);
      xMotivo := xMotivoCanc;
      chSubstituta := xChaveSub;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoConfInterPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Confirmação do Intermediário';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teConfirmacaoIntermediario;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoConfPrestPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Confirmação do Prestador';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teConfirmacaoPrestador;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoConfTomPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Confirmação do Tomador';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teConfirmacaoTomador;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoRejInterPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xCodigo, xMotivoRej: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Rejeição do Intermediário';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  xCodigo := '1';
  if not(InputQuery(xTitulo, 'Código de Rejeição:', xCodigo)) then
    exit;

  xMotivoRej := 'Motido da Rejeição da nota';
  if not(InputQuery(xTitulo, 'Motivo da Rejeição:', xMotivoRej)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teRejeicaoIntermediario;
      cMotivo := StrToIntDef(xCodigo, 1);
      xMotivo := xMotivoRej;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoRejPrestPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xCodigo, xMotivoRej: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Rejeição do Prestador';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  xCodigo := '1';
  if not(InputQuery(xTitulo, 'Código de Rejeição:', xCodigo)) then
    exit;

  xMotivoRej := 'Motido da Rejeição da nota';
  if not(InputQuery(xTitulo, 'Motivo da Rejeição:', xMotivoRej)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teRejeicaoPrestador;
      cMotivo := StrToIntDef(xCodigo, 1);
      xMotivo := xMotivoRej;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnEventoRejTomPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe, xCodigo, xMotivoRej: String;
  InfEvento: TInfEvento;
begin
  xTitulo := 'Evento de Rejeição do Tomador';

  xChaveNFSe := '';
  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  xCodigo := '1';
  if not(InputQuery(xTitulo, 'Código de Rejeição:', xCodigo)) then
    exit;

  xMotivoRej := 'Motido da Rejeição da nota';
  if not(InputQuery(xTitulo, 'Motivo da Rejeição:', xMotivoRej)) then
    exit;

  InfEvento := TInfEvento.Create;

  try
    with InfEvento.pedRegEvento do
    begin
      tpAmb := ACBrNFSeX1.Configuracoes.WebServices.AmbienteCodigo;
      verAplic := 'ACBrNFSeX-1.0';
      dhEvento := Now;
      chNFSe := xChaveNFSe;
      nPedRegEvento := 1;
      tpEvento := ACBrNFSeXConversao.teRejeicaoTomador;
      cMotivo := StrToIntDef(xCodigo, 1);
      xMotivo := xMotivoRej;
    end;

    ACBrNFSeX1.EnviarEvento(InfEvento);
  finally
    InfEvento.Free;
  end;

  ChecarResposta(tmEnviarEvento);
end;

procedure TfrmACBrNFSe.btnGerarArqINIClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  ArqINI: TStringList;
begin
  ConfigurarComponente;
  ACBrNFSeX1.NotasFiscais.Clear;
  Alimentar_Componente('1', '1');

  ArqINI := TStringList.Create;
  SaveDlg := TSaveDialog.Create(nil);
  try
    ArqINI.Text := ACBrNFSeX1.NotasFiscais.GerarIni;

    SaveDlg.Title := 'Escolha o local onde gerar o INI';
    SaveDlg.DefaultExt := '*.INI';
    SaveDlg.Filter := 'Arquivo INI(*.INI)|*.INI|Arquivo ini(*.ini)|*.ini|Todos os arquivos(*.*)|*.*';

    if SaveDlg.Execute then
      ArqINI.SaveToFile(SaveDlg.FileName);

    memoLog.Lines.Add('Arquivo Salvo: ' + SaveDlg.FileName);

  finally
    SaveDlg.Free;
    ArqINI.Free;
  end;
end;

procedure TfrmACBrNFSe.btnGerarArqINIPNClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  ArqINI: TStringList;
begin
  ConfigurarComponente;
  ACBrNFSeX1.NotasFiscais.Clear;
  Alimentar_Componente('1', '1');

  ArqINI := TStringList.Create;
  SaveDlg := TSaveDialog.Create(nil);
  try
    ArqINI.Text := ACBrNFSeX1.NotasFiscais.GerarIni;

    SaveDlg.Title := 'Escolha o local onde gerar o INI';
    SaveDlg.DefaultExt := '*.INI';
    SaveDlg.Filter := 'Arquivo INI(*.INI)|*.INI|Arquivo ini(*.ini)|*.ini|Todos os arquivos(*.*)|*.*';

    if SaveDlg.Execute then
      ArqINI.SaveToFile(SaveDlg.FileName);

    memoLog.Lines.Add('Arquivo Salvo: ' + SaveDlg.FileName);

  finally
    SaveDlg.Free;
    ArqINI.Free;
  end;
end;

procedure TfrmACBrNFSe.btnGerarEnviarLoteClick(Sender: TObject);
var
  sQtde, vNumRPS, vNumLote: String;
  iQtde, iAux, I: Integer;
begin
  sQtde := '1';
  if not(InputQuery('Gerar e Enviar um Lote de RPS (Assíncrono)', 'Quantidade de RPS', sQtde)) then
    exit;

  vNumRPS := '';
  if not(InputQuery('Gerar e Enviar um Lote de RPS (Assíncrono)', 'Numero do RPS', vNumRPS)) then
    exit;

  vNumLote := vNumRPS;
  if not(InputQuery('Gerar e Enviar um Lote de RPS (Assíncrono)', 'Numero do Lote', vNumLote)) then
    exit;

  iQtde := StrToIntDef(sQtde, 1);
  iAux := StrToIntDef(vNumRPS, 1);

  ACBrNFSeX1.NotasFiscais.Clear;

  for I := 1 to iQtde do
  begin
    vNumRPS := IntToStr(iAux);
    Alimentar_Componente(vNumRPS, vNumLote);
    inc(iAux);
  end;

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario, meTeste]
     aImprimir (Boolean) Valor Padrão = True, portanto imprime o DANFSE
  }
  // meLoteAssincrono: Ajusta o Emitir para enviar um lote de Rps no modo Assincrono
  ACBrNFSeX1.Emitir(vNumLote, meLoteAssincrono);

  ChecarResposta(tmRecepcionar);
end;

procedure TfrmACBrNFSe.btnGerarEnviarNFSeClick(Sender: TObject);
var
  vNumRPS, vNumLote: String;
begin
  vNumRPS := '';
  if not(InputQuery('Gerar e Enviar um RPS', 'Numero do RPS', vNumRPS)) then
    exit;

  vNumLote := vNumRPS;
  if not(InputQuery('Gerar e Enviar um RPS', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrNFSeX1.NotasFiscais.Clear;
  Alimentar_Componente(vNumRPS, vNumLote);

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario]
     aImprimir (Boolean) Valor Padrão = True, portanto imprime o DANFSE
  }
  // meUnitario: Ajusta o Emitir para enviar somente um Rps
  ACBrNFSeX1.Emitir(vNumLote, meUnitario);

  ChecarResposta(tmGerar);
end;

procedure TfrmACBrNFSe.btnGerarEnviarSincronoClick(Sender: TObject);
var
  sQtde, vNumRPS, vNumLote: String;
  iQtde, iAux, i: Integer;
begin
  sQtde := '1';
  if not(InputQuery('Gerar e Enviar um Lote de RPS (Síncrono)', 'Quantidade de RPS', sQtde)) then
    exit;

  vNumRPS := '';
  if not(InputQuery('Gerar e Enviar um Lote de RPS (Síncrono)', 'Numero do RPS', vNumRPS)) then
    exit;

  vNumLote := vNumRPS;
  if not(InputQuery('Gerar e Enviar um Lote de RPS (Síncrono)', 'Numero do Lote', vNumLote)) then
    exit;

  iQtde := StrToIntDef(sQtde, 1);
  iAux := StrToIntDef(vNumRPS, 1);

  ACBrNFSeX1.NotasFiscais.Clear;

  for I := 1 to iQtde do
  begin
    vNumRPS := IntToStr(iAux);
    Alimentar_Componente(vNumRPS, vNumLote);
    inc(iAux);
  end;

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario, meTeste]
     aImprimir (Boolean) Valor Padrão = True, portanto imprime o DANFSE
  }
  // meLoteSincrono: Ajusta o Emitir para enviar um lote de Rps no modo Sincrono
  ACBrNFSeX1.Emitir(vNumLote, meLoteSincrono);

  ChecarResposta(tmRecepcionarSincrono);
end;

procedure TfrmACBrNFSe.btnGerarLoteRPSClick(Sender: TObject);
var
  vNumRPS, vNumLote: String;
begin
  // **************************************************************************
  //
  // O método GerarLote apenas gera o XML do lote, assina se necessário
  // e valida, salvando o arquivo com o nome: <lote>-lot-rps.xml na pasta Ger
  // Não ocorre o envio para nenhum webservice.
  //
  // **************************************************************************
  vNumLote := '';
  if not(InputQuery('Gerar e Enviar Lote', 'Numero do Lote', vNumLote)) then
    exit;

  vNumRPS := vNumLote;
  if not(InputQuery('Gerar e Enviar Lote', 'Numero do RPS', vNumRPS)) then
    exit;

  ACBrNFSeX1.NotasFiscais.Clear;
  Alimentar_Componente(vNumRPS, vNumLote);

  ACBrNFSeX1.GerarLote(vNumLote);

  ChecarResposta(tmGerarLote);
end;

procedure TfrmACBrNFSe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSeX1.NotasFiscais.Clear;

    // LoadFromFile - Usado para carregar o Xml de apenas uma nota
    ACBrNFSeX1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    // LoadFromLoteNfse - Usado para carregar um lote de notas
//    ACBrNFSeX1.NotasFiscais.LoadFromLoteNfse(OpenDialog1.FileName);

    {
      Se o webservice não retorna o conteudo de OutrasInformacoes, mas a
      prefeitura tem um texto padrão a ser impresso no DANFSE, pode-se alimentar
      o campo OutrasInformacoes antes de executar o método Imprimir
    }
//    ACBrNFSeX1.NotasFiscais.Items[0].NFSe.OutrasInformacoes := 'Outras Informações 1;Outras Informações 2';
    ACBrNFSeX1.NotasFiscais.Imprimir;

    {
      Se desejar gerar o PDF do DANFSE basta descomentar a linha abaixo
    }
//    ACBrNFSeX1.NotasFiscais.ImprimirPDF;

    if ACBrNFSeX1.NotasFiscais.Items[0].NomeArqRps <> '' then
      memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArqRps)
    else
      memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArq);

    memoLog.Lines.Add('Nota Numero..........: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.Numero);
    memoLog.Lines.Add('Código de Verificação: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.CodigoVerificacao);
    memoLog.Lines.Add('Data de Emissão......: ' + DateToStr(ACBrNFSeX1.NotasFiscais.Items[0].NFSe.DataEmissao));
    memoLog.Lines.Add('Prestador............: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.Prestador.RazaoSocial);
    memoLog.Lines.Add('Tomador..............: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.Tomador.RazaoSocial);

    if ACBrNFSeX1.NotasFiscais.Items[0].NFSe.SituacaoNfse = ACBrNFSeXConversao.snCancelado then
      memoLog.Lines.Add('A Nota encontra-se Cancelada.');

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrNFSe.btnImprimirPNClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSeX1.NotasFiscais.Clear;

    // LoadFromFile - Usado para carregar o Xml de apenas uma nota
    ACBrNFSeX1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    // LoadFromLoteNfse - Usado para carregar um lote de notas
//    ACBrNFSeX1.NotasFiscais.LoadFromLoteNfse(OpenDialog1.FileName);

    {
      Se o webservice não retorna o conteudo de OutrasInformacoes, mas a
      prefeitura tem um texto padrão a ser impresso no DANFSE, pode-se alimentar
      o campo OutrasInformacoes antes de executar o método Imprimir
    }
//    ACBrNFSeX1.NotasFiscais.Items[0].NFSe.OutrasInformacoes := 'Outras Informações 1;Outras Informações 2';
    ACBrNFSeX1.NotasFiscais.Imprimir;

    {
      Se desejar gerar o PDF do DANFSE basta descomentar a linha abaixo
    }
    ACBrNFSeX1.NotasFiscais.ImprimirPDF;

    if ACBrNFSeX1.NotasFiscais.Items[0].NomeArqRps <> '' then
      memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArqRps)
    else
      memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArq);

    memoLog.Lines.Add('Chave do DPS.........: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.InfID.ID);
    memoLog.Lines.Add('Nota Numero..........: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.Numero);
    memoLog.Lines.Add('Código de Verificação: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.CodigoVerificacao);
    memoLog.Lines.Add('Data de Emissão......: ' + DateToStr(ACBrNFSeX1.NotasFiscais.Items[0].NFSe.DataEmissao));
    memoLog.Lines.Add('Prestador............: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.Prestador.RazaoSocial);
    memoLog.Lines.Add('Tomador..............: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.Tomador.RazaoSocial);

    if ACBrNFSeX1.NotasFiscais.Items[0].NFSe.SituacaoNfse = ACBrNFSeXConversao.snCancelado then
      memoLog.Lines.Add('A Nota encontra-se Cancelada.');

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrNFSe.btnInformacoesClick(Sender: TObject);
begin
  with ACBrNFSeX1.Configuracoes.Geral do
  begin
    memoLog.Lines.Add('------------------------------------');
    memoLog.Lines.Add('Informações sobre o provedor: ' + xProvedor +
            ' - Versão: ' + VersaoNFSeToStr(ACBrNFSeX1.Configuracoes.Geral.Versao) +
            ' - Layout: ' + IfThen(ACBrNFSeX1.Configuracoes.Geral.Layout = loABRASF,
                                   'ABRASF', 'Próprio'));
    memoLog.Lines.Add('');
    memoLog.Lines.Add('Autenticação');
    memoLog.Lines.Add('');

    if Autenticacao.RequerCertificado then
      memoLog.Lines.Add(' Requer Certificado Digital')
    else
      memoLog.Lines.Add(' Não requer Certificado Digital');

    if Autenticacao.RequerLogin then
      memoLog.Lines.Add(' Requer Login/Senha')
    else
      memoLog.Lines.Add(' Não requer Login/Senha');

    if Autenticacao.RequerChaveAcesso then
      memoLog.Lines.Add(' Requer Chave de Acesso')
    else
      memoLog.Lines.Add(' Não requer Chave de Acesso');

    if Autenticacao.RequerChaveAutorizacao then
      memoLog.Lines.Add(' Requer Chave de Autorizacao')
    else
      memoLog.Lines.Add(' Não requer Chave de Autorizacao');

    if Autenticacao.RequerFraseSecreta then
      memoLog.Lines.Add(' Requer Frase Secreta')
    else
      memoLog.Lines.Add(' Não requer Frase Secreta');

    memoLog.Lines.Add('');
    memoLog.Lines.Add('Serviços Disponibilizados');
    memoLog.Lines.Add('');

    if ServicosDisponibilizados.EnviarLoteAssincrono then
      memoLog.Lines.Add(' Permite o envio de Lote em Modo Assíncrono')
    else
      memoLog.Lines.Add(' Não permite o envio de Lote em Modo Assíncrono');

    if ServicosDisponibilizados.EnviarLoteSincrono then
      memoLog.Lines.Add(' Permite o envio de Lote em Modo Síncrono')
    else
      memoLog.Lines.Add(' Não permite o envio de Lote em Modo Síncrono');

    if ServicosDisponibilizados.EnviarUnitario then
      memoLog.Lines.Add(' Permite o envio Unitário em Modo Síncrono')
    else
      memoLog.Lines.Add(' Não permite o envio Unitário em Modo Síncrono');

    if ServicosDisponibilizados.ConsultarSituacao then
      memoLog.Lines.Add(' Permite Consultar a Situação do Lote')
    else
      memoLog.Lines.Add(' Não permite Consultar a Situação do Lote');

    if ServicosDisponibilizados.ConsultarLote then
      memoLog.Lines.Add(' Permite Consultar o Lote')
    else
      memoLog.Lines.Add(' Não permite Consultar o Lote');

    if ServicosDisponibilizados.ConsultarRps then
      memoLog.Lines.Add(' Permite Consultar o Rps')
    else
      memoLog.Lines.Add(' Não permite Consultar o Rps');

    if ServicosDisponibilizados.ConsultarNfse then
      memoLog.Lines.Add(' Permite Consultar a NFS-e')
    else
      memoLog.Lines.Add(' Não permite Consultar a NFS-e');

    if ServicosDisponibilizados.ConsultarFaixaNfse then
      memoLog.Lines.Add(' Permite Consultar uma Faixa de NFS-e')
    else
      memoLog.Lines.Add(' Não permite Consultar uma Faixa de NFS-e');

    if ServicosDisponibilizados.ConsultarServicoPrestado then
      memoLog.Lines.Add(' Permite Consultar Serviço Prestado')
    else
      memoLog.Lines.Add(' Não permite Consultar Serviço Prestado');

    if ServicosDisponibilizados.ConsultarServicoTomado then
      memoLog.Lines.Add(' Permite Consultar Serviço Tomado')
    else
      memoLog.Lines.Add(' Não permite Consultar Serviço Tomado');

    if ServicosDisponibilizados.CancelarNfse then
      memoLog.Lines.Add(' Permite Cancelar NFS-e')
    else
      memoLog.Lines.Add(' Não permite Cancelar NFS-e');

    if ServicosDisponibilizados.SubstituirNfse then
      memoLog.Lines.Add(' Permite Substituir NFS-e')
    else
      memoLog.Lines.Add(' Não permite Substituir NFS-e');

    if ServicosDisponibilizados.GerarToken then
      memoLog.Lines.Add(' Permite Gerar Token')
    else
      memoLog.Lines.Add(' Não permite Gerar Token');

    if ServicosDisponibilizados.EnviarEvento then
      memoLog.Lines.Add(' Permite Enviar Evento')
    else
      memoLog.Lines.Add(' Não permite Enviar Evento');

    if ServicosDisponibilizados.ConsultarEvento then
      memoLog.Lines.Add(' Permite Consultar Evento')
    else
      memoLog.Lines.Add(' Não permite Consultar Evento');

    if ServicosDisponibilizados.ConsultarDFe then
      memoLog.Lines.Add(' Permite Consultar DF-e')
    else
      memoLog.Lines.Add(' Não permite Consultar DF-e');

    if ServicosDisponibilizados.ConsultarParam then
      memoLog.Lines.Add(' Permite Consultar Parâmetros')
    else
      memoLog.Lines.Add(' Não permite Consultar Parâmetros');

    if ServicosDisponibilizados.ConsultarSeqRps then
      memoLog.Lines.Add(' Permite Consultar Sequencia de Rps')
    else
      memoLog.Lines.Add(' Não permite Consultar Sequencia de Rps');

    if ServicosDisponibilizados.ConsultarLinkNfse then
      memoLog.Lines.Add(' Permite Consultar Link da NFS-e')
    else
      memoLog.Lines.Add(' Não permite Consultar Link da NFS-e');

    if ServicosDisponibilizados.ConsultarNfseChave then
      memoLog.Lines.Add(' Permite Consultar NFS-e por Chave')
    else
      memoLog.Lines.Add(' Não permite Consultar NFS-e por Chave');

    memoLog.Lines.Add('');
    memoLog.Lines.Add('Particularidades');
    memoLog.Lines.Add('');

    if Particularidades.PermiteMaisDeUmServico then
      memoLog.Lines.Add(' Permite mais de um serviço')
    else
      memoLog.Lines.Add(' Não permite mais de um serviço');

    if Particularidades.PermiteTagOutrasInformacoes then
      memoLog.Lines.Add(' Permite o envio da tag OutrasInformacoes no Rps')
    else
      memoLog.Lines.Add(' Não permite o envio da tag OutrasInformacoes no Rps');

    memoLog.Lines.Add('');
    memoLog.Lines.Add('------------------------------------');
  end;
end;

procedure TfrmACBrNFSe.btnIssuerNameClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSeX1.SSL.CertIssuerName + sLineBreak + sLineBreak +
              'Certificadora: ' + ACBrNFSeX1.SSL.CertCertificadora);
end;

procedure TfrmACBrNFSe.btnLeituraX509Click(Sender: TObject);
begin
  with ACBrNFSeX1.SSL do
  begin
     CarregarCertificadoPublico(AnsiString(memoLog.Lines.Text));
     memoLog.Lines.Add(CertIssuerName);
     memoLog.Lines.Add(CertRazaoSocial);
     memoLog.Lines.Add(CertCNPJ);
     memoLog.Lines.Add(CertSubjectName);
     memoLog.Lines.Add(CertNumeroSerie);

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrNFSe.btnLerINIClick(Sender: TObject);
var
  vNumLote: string;
begin
  OpenDialog1.Title := 'Selecione o Arquivo INI';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSeX1.NotasFiscais.Clear;

    // LoadFromIni - Usado para carregar o arquivo INI para gerar posteriormente
    // o XML do Rps
    ACBrNFSeX1.NotasFiscais.LoadFromIni(OpenDialog1.FileName);

    vNumLote := '1';
    if not(InputQuery('Gerar Lote de Envio', 'Numero do Lote', vNumLote)) then
      exit;

    ACBrNFSeX1.GerarLote(vNumLote);

    ChecarResposta(tmGerarLote);
  end;
end;

procedure TfrmACBrNFSe.btnLerINIPNClick(Sender: TObject);
var
  vNumLote: string;
begin
  OpenDialog1.Title := 'Selecione o Arquivo INI';
  OpenDialog1.DefaultExt := '*.ini';
  OpenDialog1.Filter :=
    'Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSeX1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSeX1.NotasFiscais.Clear;

    // LoadFromIni - Usado para carregar o arquivo INI para gerar posteriormente
    // o XML do Rps
    ACBrNFSeX1.NotasFiscais.LoadFromIni(OpenDialog1.FileName);

    vNumLote := '1';
    if not(InputQuery('Gerar Lote de Envio', 'Numero do Lote', vNumLote)) then
      exit;

    ACBrNFSeX1.GerarLote(vNumLote);

    ChecarResposta(tmGerarLote);
  end;
end;

procedure TfrmACBrNFSe.btnLinkNFSeClick(Sender: TObject);
var
  xTitulo, xNumNFSe, xCodVerif, xID, sLink: String;
begin
  xTitulo := 'Gerar o Link da NFSe';

  xNumNFSe := '';
  if not(InputQuery(xTitulo, 'Numero da NFSe', xNumNFSe)) then
    exit;

  xCodVerif := '';
  if not(InputQuery(xTitulo, 'Codigo de Verificacao', xCodVerif)) then
    exit;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proInfisc then
  begin
    xID := '';
    if not(InputQuery(xTitulo, 'ID:', xID)) then
      exit;
  end;

  sLink := ACBrNFSeX1.LinkNFSe(xNumNFSe, xCodVerif, '', '', xID);

  memoLog.Lines.Add('Link Gerado: ' + sLink);

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSeX1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrNFSe.btnObterPDFdoDANFSEPNClick(Sender: TObject);
var
  xTitulo, xChaveNFSe: String;
begin
  xTitulo := 'Obter PDF do DANFSE';

  xChaveNFSe := '';

  if not(InputQuery(xTitulo, 'Chave da NFS-e:', xChaveNFSe)) then
    exit;

  ACBrNFSeX1.ObterDANFSE(xChaveNFSe);

  ChecarResposta(tmConsultarNFSePorChave);
end;

procedure TfrmACBrNFSe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrNFSe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSeX1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrNFSeX1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrNFSe.btnSubsNFSeClick(Sender: TObject);
var
  vNumRPS, Codigo, Motivo, sNumNFSe, sSerieNFSe, NumLote, CodVerif: String;
  CodCanc: Integer;
begin
  vNumRPS := '';
  if not(InputQuery('Substituir NFS-e', 'Numero do novo RPS', vNumRPS)) then
    exit;

  ACBrNFSeX1.NotasFiscais.Clear;
  Alimentar_Componente(vNumRPS, '1');

  // Codigo de Cancelamento
  // 1 - Erro de emissão
  // 2 - Serviço não concluido
  // 3 - RPS Cancelado na Emissão

  Codigo := '1';
  if not(InputQuery('Substituir NFSe', 'Código de Cancelamento', Codigo)) then
    exit;

  // Provedor SigEp o código de cancelamento é diferente
  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proSigep then
  begin
    CodCanc := StrToIntDef(Codigo, 1);

    case CodCanc of
      1: Codigo := 'EE';
      2: Codigo := 'ED';
      3: Codigo := 'OU';
      4: Codigo := 'SB';
    end;
  end;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proAgili, proConam, proEquiplano,
    proGoverna, proIPM, proISSDSF, proISSLencois, proModernizacaoPublica,
    proPublica, proSiat, proSigISS, proSmarAPD, proWebFisco, proSudoeste,
    proBauhaus] then
  begin
    Motivo := 'Teste de Cancelamento';
    if not (InputQuery('Cancelar NFSe', 'Motivo de Cancelamento', Motivo)) then
      exit;
  end;

  sNumNFSe := vNumRPS;
  if not(InputQuery('Substituir NFS-e', 'Numero da NFS-e', sNumNFSe)) then
    exit;

  sSerieNFSe := '';
  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proiiBrasil then
  begin
    if not(InputQuery('Substituir NFS-e', 'Série da NFS-e', sSerieNFSe)) then
      exit;
  end;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proAssessorPublico then
  begin
    NumLote := '1';
    if not (InputQuery('Cancelar NFSe', 'Numero do Lote', NumLote)) then
      exit;
  end;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSLencois, proGoverna,
       proSiat, proSigep, proElotech] then
  begin
    CodVerif := '12345678';
    if not (InputQuery('Cancelar NFSe', 'Código de Verificação', CodVerif)) then
      exit;
  end;

  ACBrNFSeX1.SubstituirNFSe(sNumNFSe, sSerieNFSe, Codigo,
                                        Motivo, NumLote, CodVerif);

  ChecarResposta(tmSubstituirNFSe);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoTomadoPorPeriodoClick(Sender: TObject);
var
  xTitulo, NumPagina, DataIni, DataFin: String;
begin
  xTitulo := 'Consultar NFSe Serviço Tomado Por Periodo';

  DataIni := DateToStr(Date);
  if not (InputQuery(xTitulo, 'Data Inicial (DD/MM/AAAA):', DataIni)) then
    exit;

  DataFin := DataIni;
  if not (InputQuery(xTitulo, 'Data Final (DD/MM/AAAA):', DataFin)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoTomadoPorPeriodo(StrToDateDef(DataIni, 0),
    StrToDateDef(DataFin, 0), StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoTomado);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoTomadoPorPrestadorClick(Sender: TObject);
var
  xTitulo, NumPagina, CPFCNPJPrestador, IMPrestador: String;
begin
  xTitulo := 'Consultar NFSe Serviço Tomado Por Prestador';

  CPFCNPJPrestador := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Prestador:', CPFCNPJPrestador)) then
    exit;

  IMPrestador := '';
  if not(InputQuery(xTitulo, 'I.M. Prestador:', IMPrestador)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  ACBrNFSeX1.ConsultarNFSeServicoTomadoPorPrestador(CPFCNPJPrestador,
    IMPrestador, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoTomado);
end;

procedure TfrmACBrNFSe.btnConsultarNFSeServicoTomadoPorTomadorClick(
  Sender: TObject);
var
  xTitulo, NumPagina, CPFCNPJTomador, IMTomador, DataIni, DataFin: String;
begin
  xTitulo := 'Consultar NFSe Serviço Tomado Por Tomador';

  CPFCNPJTomador := '';
  if not(InputQuery(xTitulo, 'CPF/CNPJ Tomador:', CPFCNPJTomador)) then
    exit;

  IMTomador := '';
  if not(InputQuery(xTitulo, 'I.M. Tomador:', IMTomador)) then
    exit;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proISSSaoPaulo then
  begin
    DataIni := DateToStr(Date);
    if not (InputQuery(xTitulo, 'Data Inicial (DD/MM/AAAA):', DataIni)) then
      exit;

    DataFin := DataIni;
    if not (InputQuery(xTitulo, 'Data Final (DD/MM/AAAA):', DataFin)) then
      exit;
  end;

  ACBrNFSeX1.ConsultarNFSeServicoTomadoPorTomador(CPFCNPJTomador,
    IMTomador, StrToIntDef(NumPagina, 1), StrToDateDef(DataIni, 0),
    StrToDateDef(DataFin, 0));

  ChecarResposta(tmConsultarNFSeServicoTomado);
end;

procedure TfrmACBrNFSe.btnConsultarParamMunicAliquotaClick(Sender: TObject);
var
  xTitulo, CodServ, Compet: String;
begin
  xTitulo := 'Consultar Parâmetros Municipais';

  CodServ := '';
  if not(InputQuery(xTitulo, 'Código do Serviço:', CodServ)) then
    exit;

  Compet := DateToStr(Date);
  if not(InputQuery(xTitulo, 'Data de Competencia:', Compet)) then
    exit;

  ACBrNFSeX1.ConsultarParametros(pmAliquota, CodServ,
    StrToDateDef(Compet, 0));

  ChecarResposta(tmConsultarParam);
end;

procedure TfrmACBrNFSe.btnConsultarParamMunicBeneficioClick(Sender: TObject);
var
  xTitulo, NumBenef, Compet: String;
begin
  xTitulo := 'Consultar Parâmetros Municipais';

  NumBenef := '';
  if not(InputQuery(xTitulo, 'Numero do Beneficio:', NumBenef)) then
    exit;

  Compet := DateToStr(Date);
  if not(InputQuery(xTitulo, 'Data de Competencia:', Compet)) then
    exit;

  ACBrNFSeX1.ConsultarParametros(pmBeneficios, '',
    StrToDateDef(Compet, 0), NumBenef);

  ChecarResposta(tmConsultarParam);
end;

procedure TfrmACBrNFSe.btnConsultarParamMunicConvenioClick(Sender: TObject);
begin
  ACBrNFSeX1.ConsultarParametros(pmConvenio);

  ChecarResposta(tmConsultarParam);
end;

procedure TfrmACBrNFSe.btnConsultarParamMunicHistAliquotaClick(Sender: TObject);
var
  xTitulo, CodServ: String;
begin
  xTitulo := 'Consultar Parâmetros Municipais';

  CodServ := '';
  if not(InputQuery(xTitulo, 'Código do Serviço:', CodServ)) then
    exit;

  ACBrNFSeX1.ConsultarParametros(pmHistoricoAliquota, CodServ);

  ChecarResposta(tmConsultarParam);
end;

procedure TfrmACBrNFSe.btnConsultarParamMunicRegimesEspeciaisClick(
  Sender: TObject);
var
  xTitulo, CodServ, Compet: String;
begin
  xTitulo := 'Consultar Parâmetros Municipais';

  CodServ := '';
  if not(InputQuery(xTitulo, 'Código do Serviço:', CodServ)) then
    exit;

  Compet := DateToStr(Date);
  if not(InputQuery(xTitulo, 'Data de Competencia:', Compet)) then
    exit;

  ACBrNFSeX1.ConsultarParametros(pmRegimesEspeciais, CodServ,
    StrToDateDef(Compet, 0));

  ChecarResposta(tmConsultarParam);
end;

procedure TfrmACBrNFSe.btnConsultarParamMunicRetencoesClick(Sender: TObject);
var
  xTitulo, Compet: String;
begin
  xTitulo := 'Consultar Parâmetros Municipais';

  Compet := DateToStr(Date);
  if not(InputQuery(xTitulo, 'Data de Competencia:', Compet)) then
    exit;

  ACBrNFSeX1.ConsultarParametros(pmRetencoes, '', StrToDateDef(Compet, 0));

  ChecarResposta(tmConsultarParam);
end;

procedure TfrmACBrNFSe.btnGerarEnviarTeste_SPClick(Sender: TObject);
var
  sQtde, vNumRPS, vNumLote: String;
  iQtde, iAux, I: Integer;
begin
  sQtde := '1';
  if not(InputQuery('Teste de Envio', 'Quantidade de RPS', sQtde)) then
    exit;

  vNumRPS := '';
  if not(InputQuery('Teste de Envio', 'Numero do RPS', vNumRPS)) then
    exit;

  vNumLote := vNumRPS;
  if not(InputQuery('Teste de Envio', 'Numero do Lote', vNumLote)) then
    exit;

  iQtde := StrToIntDef(sQtde, 1);
  iAux := StrToIntDef(vNumRPS, 1);

  ACBrNFSeX1.NotasFiscais.Clear;

  for I := 1 to iQtde do
  begin
    vNumRPS := IntToStr(iAux);
    Alimentar_Componente(vNumRPS, vNumLote);
    inc(iAux);
  end;

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario, meTeste]
     aImprimir (Boolean) Valor Padrão = True, portanto imprime o DANFSE
  }
  ACBrNFSeX1.Emitir(vNumLote, meTeste);

  ChecarResposta(tmTeste);
end;

procedure TfrmACBrNFSe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrNFSeX1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrNFSeX1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrNFSeX1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
    ACBrNFSeX1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrNFSe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrNFSeX1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFSe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  L: TLayoutNFSe;
begin
  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add(GetEnumName(TypeInfo(TSSLLib), integer(T)));
  cbSSLLib.ItemIndex := 4;

  cbCryptLib.Items.Clear;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add(GetEnumName(TypeInfo(TSSLCryptLib), integer(U)));
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Items.Clear;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add(GetEnumName(TypeInfo(TSSLHttpLib), integer(V)));
  cbHttpLib.ItemIndex := 0;

  cbXmlSignLib.Items.Clear;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add(GetEnumName(TypeInfo(TSSLXmlSignLib), integer(X)));
  cbXmlSignLib.ItemIndex := 0;

  cbSSLType.Items.Clear;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add(GetEnumName(TypeInfo(TSSLType), integer(Y)));
  cbSSLType.ItemIndex := 5;

  cbFormaEmissao.Items.Clear;
  for I := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
    cbFormaEmissao.Items.Add(GetEnumName(TypeInfo(TpcnTipoEmissao), integer(I)));
  cbFormaEmissao.ItemIndex := 0;

  cbLayoutNFSe.Items.Clear;
  for L := Low(TLayoutNFSe) to High(TLayoutNFSe) do
    cbLayoutNFSe.Items.Add(GetEnumName(TypeInfo(TLayoutNFSe), integer(L)));
  cbLayoutNFSe.ItemIndex := 0;

  LerConfiguracao;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteInteger('Certificado', 'SSLLib',     cbSSLLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'CryptLib',   cbCryptLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'HttpLib',    cbHttpLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString( 'Certificado', 'Caminho',    edtCaminho.Text);
    Ini.WriteString( 'Certificado', 'Senha',      edtSenha.Text);
    Ini.WriteString( 'Certificado', 'NumSerie',   edtNumSerie.Text);

    Ini.WriteBool(   'Geral', 'AtualizarXML',     cbxAtualizarXML.Checked);
    Ini.WriteBool(   'Geral', 'ExibirErroSchema', cbxExibirErroSchema.Checked);
    Ini.WriteString( 'Geral', 'FormatoAlerta',    edtFormatoAlerta.Text);
    Ini.WriteInteger('Geral', 'FormaEmissao',     cbFormaEmissao.ItemIndex);
    Ini.WriteBool(   'Geral', 'RetirarAcentos',   cbxRetirarAcentos.Checked);
    Ini.WriteBool(   'Geral', 'Salvar',           chkSalvarGer.Checked);
    Ini.WriteString( 'Geral', 'PathSalvar',       edtPathLogs.Text);
    Ini.WriteString( 'Geral', 'PathSchemas',      edtPathSchemas.Text);
    Ini.WriteString( 'Geral', 'LogoMarca',        edtLogoMarca.Text);
    Ini.WriteString( 'Geral', 'PrestLogo',        edtPrestLogo.Text);
    Ini.WriteString( 'Geral', 'Prefeitura',       edtPrefeitura.Text);

    Ini.WriteBool(   'Geral', 'ConsultaAposEnvio',    chkConsultaLoteAposEnvio.Checked);
    Ini.WriteBool(   'Geral', 'ConsultaAposCancelar', chkConsultaAposCancelar.Checked);
    Ini.WriteBool(   'Geral', 'MontarPathSchemas',    chkMontarPathSchemas.Checked);

    Ini.WriteInteger('Geral', 'LayoutNFSe',     cbLayoutNFSe.ItemIndex);

    Ini.WriteInteger('WebService', 'Ambiente',     rgTipoAmb.ItemIndex);
    Ini.WriteBool(   'WebService', 'Visualizar',   cbxVisualizar.Checked);
    Ini.WriteBool(   'WebService', 'SalvarSOAP',   chkSalvarSOAP.Checked);
    Ini.WriteBool(   'WebService', 'AjustarAut',   cbxAjustarAut.Checked);
    Ini.WriteString( 'WebService', 'Aguardar',     edtAguardar.Text);
    Ini.WriteString( 'WebService', 'Tentativas',   edtTentativas.Text);
    Ini.WriteString( 'WebService', 'Intervalo',    edtIntervalo.Text);
    Ini.WriteInteger('WebService', 'TimeOut',      seTimeOut.Value);
    Ini.WriteInteger('WebService', 'SSLType',      cbSSLType.ItemIndex);
    Ini.WriteString( 'WebService', 'SenhaWeb',     edtSenhaWeb.Text);
    Ini.WriteString( 'WebService', 'UserWeb',      edtUserWeb.Text);
    Ini.WriteString( 'WebService', 'FraseSecWeb',  edtFraseSecWeb.Text);
    Ini.WriteString( 'WebService', 'ChAcessoWeb',  edtChaveAcessoWeb.Text);
    Ini.WriteString( 'WebService', 'ChAutorizWeb', edtChaveAutorizWeb.Text);

    Ini.WriteString('Proxy', 'Host',  edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString('Proxy', 'User',  edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass',  edtProxySenha.Text);

    Ini.WriteBool(  'Arquivos', 'Salvar',          chkSalvarArq.Checked);
    Ini.WriteBool(  'Arquivos', 'PastaMensal',     cbxPastaMensal.Checked);
    Ini.WriteBool(  'Arquivos', 'AddLiteral',      cbxAdicionaLiteral.Checked);
    Ini.WriteBool(  'Arquivos', 'EmissaoPathNFSe', cbxEmissaoPathNFSe.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',  cbxSepararPorCNPJ.Checked);
    Ini.WriteString('Arquivos', 'PathNFSe',        edtPathNFSe.Text);

    Ini.WriteString('Emitente', 'CNPJ',        edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IM',          edtEmitIM.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia',    edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone',        edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP',         edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro',  edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero',      edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro',      edtEmitBairro.Text);
    Ini.WriteString('Emitente', 'CodCidade',   edtCodCidade.Text);
    Ini.WriteString('Emitente', 'Cidade',      edtEmitCidade.Text);
    Ini.WriteString('Emitente', 'UF',          edtEmitUF.Text);
    Ini.WriteString('Emitente', 'CNPJPref',    edtCNPJPrefeitura.Text);

    Ini.WriteString('Email', 'Host',    edtSmtpHost.Text);
    Ini.WriteString('Email', 'Port',    edtSmtpPort.Text);
    Ini.WriteString('Email', 'User',    edtSmtpUser.Text);
    Ini.WriteString('Email', 'Pass',    edtSmtpPass.Text);
    Ini.WriteString('Email', 'Assunto', edtEmailAssunto.Text);
    Ini.WriteBool(  'Email', 'SSL',     cbEmailSSL.Checked );

    StreamMemo := TMemoryStream.Create;
    mmEmailMsg.Lines.SaveToStream(StreamMemo);
    StreamMemo.Seek(0,soFromBeginning);

    Ini.WriteBinaryStream('Email', 'Mensagem', StreamMemo);

    StreamMemo.Free;

    if edtPathPDF.Text = '' then
      edtPathPDF.Text := edtPathNFSe.Text;

    Ini.WriteInteger('DANFSE', 'Tipo',      rgTipoDANFSE.ItemIndex);
    Ini.WriteString( 'DANFSE', 'LogoMarca', edtLogoMarca.Text);
    Ini.WriteString( 'DANFSE', 'PathPDF',   edtPathPDF.Text);

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFSe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrNFSe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrNFSe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrNFSe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrNFSe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrNFSe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrNFSe.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    cbSSLLib.ItemIndex     := Ini.ReadInteger('Certificado', 'SSLLib',     4);
    cbCryptLib.ItemIndex   := Ini.ReadInteger('Certificado', 'CryptLib',   0);
    cbHttpLib.ItemIndex    := Ini.ReadInteger('Certificado', 'HttpLib',    0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger('Certificado', 'XmlSignLib', 0);
    if cbSSLLib.ItemIndex <> 5 then
      cbSSLLibChange(cbSSLLib);
    edtCaminho.Text        := Ini.ReadString( 'Certificado', 'Caminho',    '');
    edtSenha.Text          := Ini.ReadString( 'Certificado', 'Senha',      '');
    edtNumSerie.Text       := Ini.ReadString( 'Certificado', 'NumSerie',   '');

    cbxAtualizarXML.Checked     := Ini.ReadBool(   'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(   'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString( 'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger('Geral', 'FormaEmissao',     0);

    chkSalvarGer.Checked      := Ini.ReadBool(  'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(  'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString('Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString('Geral', 'PathSchemas',    '');
    edtLogoMarca.Text         := Ini.ReadString('Geral', 'LogoMarca',      '');
    edtPrestLogo.Text         := Ini.ReadString('Geral', 'PrestLogo',      '');
    edtPrefeitura.Text        := Ini.ReadString('Geral', 'Prefeitura',     '');

    chkConsultaLoteAposEnvio.Checked := Ini.ReadBool('Geral', 'ConsultaAposEnvio',    False);
    chkConsultaAposCancelar.Checked  := Ini.ReadBool('Geral', 'ConsultaAposCancelar', False);
    chkMontarPathSchemas.Checked     := Ini.ReadBool('Geral', 'MontarPathSchemas',    True);

    cbLayoutNFSe.ItemIndex     := Ini.ReadInteger('Geral', 'LayoutNFSe',     0);

    rgTipoAmb.ItemIndex     := Ini.ReadInteger('WebService', 'Ambiente',     0);
    cbxVisualizar.Checked   := Ini.ReadBool(   'WebService', 'Visualizar',   False);
    chkSalvarSOAP.Checked   := Ini.ReadBool(   'WebService', 'SalvarSOAP',   False);
    cbxAjustarAut.Checked   := Ini.ReadBool(   'WebService', 'AjustarAut',   False);
    edtAguardar.Text        := Ini.ReadString( 'WebService', 'Aguardar',     '0');
    edtTentativas.Text      := Ini.ReadString( 'WebService', 'Tentativas',   '5');
    edtIntervalo.Text       := Ini.ReadString( 'WebService', 'Intervalo',    '0');
    seTimeOut.Value         := Ini.ReadInteger('WebService', 'TimeOut',      5000);
    cbSSLType.ItemIndex     := Ini.ReadInteger('WebService', 'SSLType',      5);
    edtSenhaWeb.Text        := Ini.ReadString( 'WebService', 'SenhaWeb',     '');
    edtUserWeb.Text         := Ini.ReadString( 'WebService', 'UserWeb',      '');
    edtFraseSecWeb.Text     := Ini.ReadString( 'WebService', 'FraseSecWeb',  '');
    edtChaveAcessoWeb.Text  := Ini.ReadString( 'WebService', 'ChAcessoWeb',  '');
    edtChaveAutorizWeb.Text := Ini.ReadString( 'WebService', 'ChAutorizWeb', '');

    edtProxyHost.Text  := Ini.ReadString('Proxy', 'Host',  '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text  := Ini.ReadString('Proxy', 'User',  '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass',  '');

    chkSalvarArq.Checked       := Ini.ReadBool(  'Arquivos', 'Salvar',          False);
    cbxPastaMensal.Checked     := Ini.ReadBool(  'Arquivos', 'PastaMensal',     False);
    cbxAdicionaLiteral.Checked := Ini.ReadBool(  'Arquivos', 'AddLiteral',      False);
    cbxEmissaoPathNFSe.Checked := Ini.ReadBool(  'Arquivos', 'EmissaoPathNFSe', False);
    cbxSepararPorCNPJ.Checked  := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',  False);
    edtPathNFSe.Text           := Ini.ReadString('Arquivos', 'PathNFSe',        '');

    edtEmitCNPJ.Text       := Ini.ReadString('Emitente', 'CNPJ',        '');
    edtEmitIM.Text         := Ini.ReadString('Emitente', 'IM',          '');
    edtEmitRazao.Text      := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text   := Ini.ReadString('Emitente', 'Fantasia',    '');
    edtEmitFone.Text       := Ini.ReadString('Emitente', 'Fone',        '');
    edtEmitCEP.Text        := Ini.ReadString('Emitente', 'CEP',         '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro',  '');
    edtEmitNumero.Text     := Ini.ReadString('Emitente', 'Numero',      '');
    edtEmitComp.Text       := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text     := Ini.ReadString('Emitente', 'Bairro',      '');
    edtCodCidade.Text      := Ini.ReadString('Emitente', 'CodCidade',   '');
    edtEmitCidade.Text     := Ini.ReadString('Emitente', 'Cidade',      '');
    edtEmitUF.Text         := Ini.ReadString('Emitente', 'UF',          '');
    edtCNPJPrefeitura.Text := Ini.ReadString('Emitente', 'CNPJPref',    '');

    edtSmtpHost.Text     := Ini.ReadString('Email', 'Host',    '');
    edtSmtpPort.Text     := Ini.ReadString('Email', 'Port',    '');
    edtSmtpUser.Text     := Ini.ReadString('Email', 'User',    '');
    edtSmtpPass.Text     := Ini.ReadString('Email', 'Pass',    '');
    edtEmailAssunto.Text := Ini.ReadString('Email', 'Assunto', '');
    cbEmailSSL.Checked   := Ini.ReadBool(  'Email', 'SSL',     False);

    StreamMemo := TMemoryStream.Create;
    Ini.ReadBinaryStream('Email', 'Mensagem', StreamMemo);
    mmEmailMsg.Lines.LoadFromStream(StreamMemo);
    StreamMemo.Free;

    rgTipoDANFSe.ItemIndex := Ini.ReadInteger('DANFSE', 'Tipo',       0);
    edtLogoMarca.Text      := Ini.ReadString( 'DANFSE', 'LogoMarca', '');
    edtPathPDF.Text        := Ini.ReadString( 'DANFSE', 'PathPDF',   '');

    ConfigurarComponente;

    AtualizarCidades;

    cbCidades.ItemIndex := cbCidades.Items.IndexOf(edtEmitCidade.Text + '/' +
      edtCodCidade.Text + '/' + edtEmitUF.Text);
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFSe.ChecarResposta(aMetodo: TMetodo);
var
  i: Integer;

  procedure ListaDeErros(aErros: TNFSeEventoCollection);
  var
    i: Integer;
  begin
    if aErros.Count > 0 then
    begin
      memoLog.Lines.Add(' ');
      memoLog.Lines.Add('Erro(s):');
      for i := 0 to aErros.Count -1 do
      begin
        memoLog.Lines.Add('Código  : ' + aErros[i].Codigo);
        memoLog.Lines.Add('Mensagem: ' + aErros[i].Descricao);
        memoLog.Lines.Add('Correção: ' + aErros[i].Correcao);
        memoLog.Lines.Add('---------');
      end;
    end;
  end;

  procedure ListaDeAlertas(aAlertas: TNFSeEventoCollection);
  var
    i: Integer;
  begin
    if aAlertas.Count > 0 then
    begin
      memoLog.Lines.Add(' ');
      memoLog.Lines.Add('Alerta(s):');
      for i := 0 to aAlertas.Count -1 do
      begin
        memoLog.Lines.Add('Código  : ' + aAlertas[i].Codigo);
        memoLog.Lines.Add('Mensagem: ' + aAlertas[i].Descricao);
        memoLog.Lines.Add('Correção: ' + aAlertas[i].Correcao);
        memoLog.Lines.Add('---------');
      end;
    end;
  end;

  procedure ListaDeResumos(aResumos: TNFSeResumoCollection; aMetodo: TMetodo);
  var
    i: Integer;
  begin
    if aResumos.Count > 0 then
    begin
      memoLog.Lines.Add(' ');
      memoLog.Lines.Add('Resumo(s):');
      for i := 0 to aResumos.Count -1 do
      begin
        case aMetodo of
          tmConsultarEvento:
            begin
              memoLog.Lines.Add('Chave DFe: ' + aResumos[i].ChaveDFe);
              memoLog.Lines.Add('Tipo Doc.: ' + aResumos[i].TipoDoc);
            end;

          tmConsultarDFe:
            begin
              memoLog.Lines.Add('NSU      : ' + IntToStr(aResumos[i].NSU));
              memoLog.Lines.Add('Chave DFe: ' + aResumos[i].ChaveDFe);
              memoLog.Lines.Add('Tipo Doc.: ' + aResumos[i].TipoDoc);
            end;
        else
          begin
            memoLog.Lines.Add('Numero da Nota    : ' + aResumos[i].NumeroNota);
            memoLog.Lines.Add('Série da Nota     : ' + aResumos[i].SerieNota);
            memoLog.Lines.Add('Código Verificação: ' + aResumos[i].CodigoVerificacao);
            memoLog.Lines.Add('Numero do Rps     : ' + aResumos[i].NumeroRps);
            memoLog.Lines.Add('Série do Rps      : ' + aResumos[i].SerieRps);
            memoLog.Lines.Add('Id da Nota        : ' + aResumos[i].idNota);
            memoLog.Lines.Add('Id da Rps         : ' + aResumos[i].idRps);
            memoLog.Lines.Add('Data              : ' + DateTimeToStr(aResumos[i].Data));
            memoLog.Lines.Add('Link              : ' + aResumos[i].Link);
            memoLog.Lines.Add('Protocolo         : ' + aResumos[i].Protocolo);
            memoLog.Lines.Add('Situação          : ' + aResumos[i].Situacao);
            memoLog.Lines.Add('Desc. da Situação : ' + aResumos[i].DescSituacao);
          end;
        end;

        memoLog.Lines.Add('---------');
      end;
    end;
  end;
begin
  memoLog.Clear;
  memoLog.Lines.Clear;
  memoLog.Update;

  memoLog.Lines.Add('------------------------------');
  memoLog.Lines.Add('Versão OpenSSL');
  memoLog.Lines.Add( OpenSSLExt.OpenSSLVersion(0) );
  memoLog.Lines.Add( OpenSSLExt.OpenSSLFullVersion );
  memoLog.Lines.Add( OpenSSLExt.SSLUtilFile );
  memoLog.Lines.Add( OpenSSLExt.SSLLibFile );
  memoLog.Lines.Add('------------------------------');

  memoLog.Lines.Add('Requisição');
  memoLog.Lines.Add('Ambiente : ' + TpAmbToStr(ACBrNFSeX1.Configuracoes.WebServices.Ambiente));
  memoLog.Lines.Add('Cidade   : ' + ACBrNFSeX1.Configuracoes.Geral.xMunicipio + '/' +
                                   ACBrNFSeX1.Configuracoes.Geral.xUF);
  memoLog.Lines.Add('Provedor : ' + ACBrNFSeX1.Configuracoes.Geral.xProvedor +
          ' Versão: ' + VersaoNFSeToStr(ACBrNFSeX1.Configuracoes.Geral.Versao));
  memoLog.Lines.Add('Data/Hora: ' + DateTimeToStr(Now));
  memoLog.Lines.Add(' ');

  with ACBrNFSeX1.WebService do
  begin
    case aMetodo of
      tmRecepcionar,
      tmTeste:
        begin
          with Emite do
          begin
            memoLog.Lines.Add('Método Executado: ' + ModoEnvioToStr(ModoEnvio));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Data de Envio : ' + DateToStr(Data));
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Numero da Nota: ' + NumeroNota);
            memoLog.Lines.Add('Link          : ' + Link);
            memoLog.Lines.Add('Código Verif. : ' + CodigoVerificacao);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;

          if ACBrNFSeX1.Configuracoes.Geral.ConsultaLoteAposEnvio and
             ((Emite.Protocolo <> '') or (Emite.NumeroLote <> '')) then
          begin
            if ACBrNFSeX1.Provider.ConfigGeral.ConsultaSitLote then
            begin
              with ConsultaSituacao do
              begin
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarSituacao));
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Parâmetros de Envio');
                memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
                memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Parâmetros de Retorno');
                memoLog.Lines.Add('Situação Lote : ' + Situacao);
                memoLog.Lines.Add('Descrição Sit : ' + DescSituacao);
                memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

                LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml', 1000);
                LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml', 1000);

                ListaDeErros(Erros);
                ListaDeAlertas(Alertas);
              end;
            end;

            if ACBrNFSeX1.Provider.ConfigGeral.ConsultaLote then
            begin
              with ConsultaLoteRps do
              begin
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarLote));
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Parâmetros de Envio');
                memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
                memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Parâmetros de Retorno');
                memoLog.Lines.Add('Situação Lote : ' + Situacao);
                memoLog.Lines.Add('ID Nota       : ' + idNota);
                memoLog.Lines.Add('ID Rps        : ' + idRps);
                memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

                ListaDeResumos(Resumos, tmConsultarLote);

                LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml', 1000);
                LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml', 1000);

                ListaDeErros(Erros);
                ListaDeAlertas(Alertas);
              end;
            end;
          end;
        end;

      tmRecepcionarSincrono,
      tmGerar:
        begin
          with Emite do
          begin
            memoLog.Lines.Add('Método Executado: ' + ModoEnvioToStr(ModoEnvio));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Data de Envio : ' + DateToStr(Data));
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Numero da Nota: ' + NumeroNota);
            memoLog.Lines.Add('Link          : ' + Link);
            memoLog.Lines.Add('Código Verif. : ' + CodigoVerificacao);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarSituacao:
        begin
          with ConsultaSituacao do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarSituacao));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Situação Lote : ' + Situacao);
            memoLog.Lines.Add('Descrição Sit : ' + DescSituacao);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarLote:
        begin
          with ConsultaLoteRps do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarLote));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Situação Lote : ' + Situacao);
            memoLog.Lines.Add('ID Nota       : ' + idNota);
            memoLog.Lines.Add('ID Rps        : ' + idRps);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            ListaDeResumos(Resumos, tmConsultarLote);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarNFSePorRps:
        begin
          with ConsultaNFSeporRps do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarNFSePorRps));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Rps : ' + NumeroRps);
            memoLog.Lines.Add('Série do Rps  : ' + SerieRps);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Situação      : ' + Situacao);
            memoLog.Lines.Add('Data          : ' + DateToStr(Data));
            memoLog.Lines.Add('Desc. Situação: ' + DescSituacao);
            memoLog.Lines.Add('ID Nota       : ' + idNota);
            memoLog.Lines.Add('Link          : ' + Link);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarNFSe,
      tmConsultarNFSePorFaixa,
      tmConsultarNFSeServicoPrestado,
      tmConsultarNFSeServicoTomado:
        begin
          with ConsultaNFSe do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(Metodo));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Num. Ini. NFSe: ' + InfConsultaNFSe.NumeroIniNFSe);
            memoLog.Lines.Add('Num. Fin. NFSe: ' + InfConsultaNFSe.NumeroFinNFSe);
            memoLog.Lines.Add('Data Inicial  : ' + DateToStr(InfConsultaNFSe.DataInicial));
            memoLog.Lines.Add('Data Final    : ' + DateToStr(InfConsultaNFSe.DataFinal));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proPrescon] then
              memoLog.Lines.Add('Número NFSe   : ' + NumeroNota);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarNFSePorChave:
        begin
          with ConsultaNFSe do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(Metodo));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Chave da NFSe: ' + InfConsultaNFSe.ChaveNFSe);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proPrescon] then
              memoLog.Lines.Add('Número NFSe   : ' + NumeroNota);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarLinkNFSe:
        begin
          with ConsultaLinkNFSe do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarLinkNFSe));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Competencia   : ' + DateToStr(InfConsultaLinkNFSe.Competencia));
            memoLog.Lines.Add('Numero da NFSe: ' + InfConsultaLinkNFSe.NumeroNFSe);
            memoLog.Lines.Add('Série da NFSe : ' + InfConsultaLinkNFSe.SerieNFSe);
            memoLog.Lines.Add('Numero da NFSe: ' + IntToStr(InfConsultaLinkNFSe.NumeroRps));
            memoLog.Lines.Add('Série da NFSe : ' + InfConsultaLinkNFSe.SerieRps);

            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Situação: ' + Situacao);
            memoLog.Lines.Add('Link    : ' + Link);
            memoLog.Lines.Add('Sucesso : ' + BoolToStr(Sucesso, True));
            memoLog.Lines.Add(' ');

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmCancelarNFSe:
        begin
          with CancelaNFSe do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmCancelarNFSe));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero da NFSe: ' + InfCancelamento.NumeroNFSe);
            memoLog.Lines.Add('Série da NFSe : ' + InfCancelamento.SerieNFSe);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Situação: ' + Situacao);
            memoLog.Lines.Add('Link    : ' + Link);
            memoLog.Lines.Add('Sucesso : ' + BoolToStr(Sucesso, True));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Retorno do Pedido de Cancelamento:');
            memoLog.Lines.Add('Situação : ' + RetCancelamento.Situacao);
            memoLog.Lines.Add('Data/Hora: ' + DateToStr(RetCancelamento.DataHora));
            memoLog.Lines.Add('Mensagem : ' + RetCancelamento.MsgCanc);
            memoLog.Lines.Add('Sucesso  : ' + RetCancelamento.Sucesso);
            memoLog.Lines.Add('Link     : ' + RetCancelamento.Link);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;

          if ACBrNFSeX1.Configuracoes.Geral.ConsultaAposCancelar and
             ACBrNFSeX1.Provider.ConfigGeral.ConsultaNFSe then
          begin
            with ConsultaNFSe do
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Método Executado: ' + MetodoToStr(Metodo));
              memoLog.Lines.Add('Parâmetros de Envio');
              memoLog.Lines.Add('Num. Ini. NFSe: ' + InfConsultaNFSe.NumeroIniNFSe);
              memoLog.Lines.Add('Num. Fin. NFSe: ' + InfConsultaNFSe.NumeroFinNFSe);
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Parâmetros de Retorno');
              memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

              LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml', 1000);
              LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml', 1000);

              ListaDeErros(Erros);
              ListaDeAlertas(Alertas);
            end;
          end;
        end;

      tmSubstituirNFSe:
        begin
          with SubstituiNFSe do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmSubstituirNFSe));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero da NFSe: ' + InfCancelamento.NumeroNFSe);
            memoLog.Lines.Add('Série da NFSe : ' + InfCancelamento.SerieNFSe);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Situação: ' + Situacao);
            memoLog.Lines.Add('Link    : ' + Link);
            memoLog.Lines.Add('Sucesso : ' + BoolToStr(Sucesso, True));
            memoLog.Lines.Add('Numero da NFSe Substituida  : ' + NumNotaSubstituida);
            memoLog.Lines.Add('Numero da NFSe Substituidora: ' + NumNotaSubstituidora);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Retorno do Pedido de Cancelamento:');
            memoLog.Lines.Add('Situação : ' + RetCancelamento.Situacao);
            memoLog.Lines.Add('Data/Hora: ' + DateToStr(RetCancelamento.DataHora));
            memoLog.Lines.Add('Mensagem : ' + RetCancelamento.MsgCanc);
            memoLog.Lines.Add('Sucesso  : ' + RetCancelamento.Sucesso);
            memoLog.Lines.Add('Link     : ' + RetCancelamento.Link);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmGerarLote:
        begin
          with Gerar do
          begin
            memoLog.Lines.Add('Método Executado: ' + ModoEnvioToStr(ModoEnvio));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Lote: ' + NumeroLote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Nome Arquivo : ' + NomeArq);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmGerarToken:
        begin
          with GerarToken do
          begin
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmGerarToken));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Token         : ' + Token);
            memoLog.Lines.Add('Data Expiracao: ' + DateTimeToStr(DataExpiracao));

            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmAbrirSessao:
        begin
        end;

      tmFecharSessao:
        begin
        end;

      tmEnviarEvento:
        begin
          with EnviarEvento do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmEnviarEvento));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            with InfEvento.pedRegEvento do
            begin
              memoLog.Lines.Add('Chave NFSe : ' + chNFSe);
              memoLog.Lines.Add('Evento     : ' + tpEventoToDesc(tpEvento));
            end;
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Chave NFSe      : ' + idNota);
            memoLog.Lines.Add('Data            : ' + DateToStr(Data));
            memoLog.Lines.Add('Tipo Evento     : ' + tpEventoToDesc(tpEvento));
            memoLog.Lines.Add('Num. Seq. Evento: ' + IntToStr(nSeqEvento));
            memoLog.Lines.Add('ID do Evento    : ' + idEvento);
            memoLog.Lines.Add('Sucesso         : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarEvento:
        begin
          with ConsultarEvento do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarEvento));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Chave NFSe      : ' + ChaveNFSe);
            memoLog.Lines.Add('Evento          : ' + tpEventoToDesc(tpEvento));
            memoLog.Lines.Add('Num. Seq. Evento: ' + IntToStr(nSeqEvento));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Chave NFSe      : ' + idNota);
            memoLog.Lines.Add('Data            : ' + DateToStr(Data));
            memoLog.Lines.Add('Tipo Evento     : ' + tpEventoToDesc(tpEvento));
            memoLog.Lines.Add('Num. Seq. Evento: ' + IntToStr(nSeqEvento));
            memoLog.Lines.Add('ID do Evento    : ' + idEvento);
            memoLog.Lines.Add('Sucesso         : ' + BoolToStr(Sucesso, True));

            ListaDeResumos(Resumos, tmConsultarEvento);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarDFe:
        begin
          with ConsultarDFe do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarDFe));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Chave NFSe     : ' + ChaveNFSe);
            memoLog.Lines.Add('Num. Seq. Unico: ' + IntToStr(NSU));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Data   : ' + DateToStr(Data));
            memoLog.Lines.Add('Sucesso: ' + BoolToStr(Sucesso, True));

            ListaDeResumos(Resumos, tmConsultarDFe);

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;

      tmConsultarParam:
        begin
          with ConsultarParam do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarParam));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Tipo Parâmetro  : ' + ParamMunicToStr(tpParamMunic));
            memoLog.Lines.Add('Código Municipío: ' + IntToStr(CodigoMunicipio));
            memoLog.Lines.Add('Código Serviço  : ' + CodigoServico);
            memoLog.Lines.Add('Competencia     : ' + DateToStr(Competencia));
            memoLog.Lines.Add('Numero Beneficio: ' + NumeroBeneficio);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Data      : ' + DateTimeToStr(Data));
            memoLog.Lines.Add('Sucesso   : ' + BoolToStr(Sucesso, True));

            if Parametros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Parâmetros(s):');
              for i := 0 to Parametros.Count -1 do
              begin
                memoLog.Lines.Add(Parametros[i]);
              end;
            end;

            LoadXML(XmlEnvio, WBXmlEnvio, 'temp1.xml');
            LoadXML(XmlRetorno, WBXmlRetorno, 'temp2.xml');

            ListaDeErros(Erros);
            ListaDeAlertas(Alertas);
          end;
        end;
    end;
  end;

  for i := 0 to ACBrNFSeX1.NotasFiscais.Count -1 do
  begin
    memoLog.Lines.Add(' ');
    memoLog.Lines.Add('NFS-e Numero....: ' + ACBrNFSeX1.NotasFiscais.Items[i].NFSe.Numero);
    memoLog.Lines.Add('Cod. Verificacao: ' + ACBrNFSeX1.NotasFiscais.Items[i].NFSe.CodigoVerificacao);
    memoLog.Lines.Add('Prestador.......: ' + ACBrNFSeX1.NotasFiscais.Items[i].NFSe.Prestador.RazaoSocial);
    memoLog.Lines.Add('Tomador.........: ' + ACBrNFSeX1.NotasFiscais.Items[i].NFSe.Tomador.RazaoSocial);

    if ACBrNFSeX1.NotasFiscais.Items[i].NFSe.SituacaoNfse = ACBrNFSeXConversao.snCancelado then
      memoLog.Lines.Add('A Nota encontra-se Cancelada.');

    if ACBrNFSeX1.NotasFiscais.Items[i].NomeArq <> '' then
    begin
      memoLog.Lines.Add('Nome do arquivo.: ' + ACBrNFSeX1.Configuracoes.Arquivos.GetPathNFSe() + '\' +
                                               ACBrNFSeX1.NotasFiscais.Items[i].NomeArq);
      if ACBrNFSeX1.Configuracoes.Arquivos.Salvar then
        memoLog.Lines.Add('==> Xml da nota salvo na pasta e com o nome informado acima.')
      else
        memoLog.Lines.Add('==> Xml da nota não salvo em disco.');

      // Na propriedade XmlNfse temos o XML da NFS-e
      LoadXML(ACBrNFSeX1.NotasFiscais.Items[i].XmlNfse, WBXmlNotas);
    end;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: String;
begin
  ACBrNFSeX1.Configuracoes.Certificados.ArquivoPFX  := '';
  ACBrNFSeX1.Configuracoes.Certificados.Senha       := '';
  ACBrNFSeX1.Configuracoes.Certificados.NumeroSerie := '';

  ACBrNFSeX1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrNFSeX1.Configuracoes.Certificados.Senha       := AnsiString(edtSenha.Text);
  ACBrNFSeX1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrNFSeX1.SSL.DescarregarCertificado;

  with ACBrNFSeX1.Configuracoes.Geral do
  begin
    SSLLib        := TSSLLib(cbSSLLib.ItemIndex);
    SSLCryptLib   := TSSLCryptLib(cbCryptLib.ItemIndex);
    SSLHttpLib    := TSSLHttpLib(cbHttpLib.ItemIndex);
    SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

    AtualizarSSLLibsCombo;

    Salvar           := chkSalvarGer.Checked;
    ExibirErroSchema := cbxExibirErroSchema.Checked;
    RetirarAcentos   := cbxRetirarAcentos.Checked;
    FormatoAlerta    := edtFormatoAlerta.Text;
    FormaEmissao     := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);

    ConsultaLoteAposEnvio := chkConsultaLoteAposEnvio.Checked;
    ConsultaAposCancelar  := chkConsultaAposCancelar.Checked;
    MontarPathSchema := chkMontarPathSchemas.Checked;

    CNPJPrefeitura := edtCNPJPrefeitura.Text;

    Emitente.CNPJ           := edtEmitCNPJ.Text;
    Emitente.InscMun        := edtEmitIM.Text;
    Emitente.RazSocial      := edtEmitRazao.Text;
    Emitente.WSUser         := edtUserWeb.Text;
    Emitente.WSSenha        := edtSenhaWeb.Text;
    Emitente.WSFraseSecr    := edtFraseSecWeb.Text;
    Emitente.WSChaveAcesso  := edtChaveAcessoWeb.Text;
    Emitente.WSChaveAutoriz := edtChaveAutorizWeb.Text;

    Emitente.DadosEmitente.NomeFantasia := edtEmitFantasia.Text;
    Emitente.DadosEmitente.InscricaoEstadual := '';
    Emitente.DadosEmitente.Endereco := edtEmitLogradouro.Text;
    Emitente.DadosEmitente.Numero := edtEmitNumero.Text;
    Emitente.DadosEmitente.CEP := edtEmitCEP.Text;
    Emitente.DadosEmitente.Bairro := edtEmitBairro.Text;
    Emitente.DadosEmitente.Complemento := edtEmitComp.Text;
    Emitente.DadosEmitente.Municipio := edtEmitCidade.Text;
    Emitente.DadosEmitente.UF := edtEmitUF.Text;
    Emitente.DadosEmitente.CodigoMunicipio := edtCodCidade.Text;
    Emitente.DadosEmitente.Telefone := edtEmitFone.Text;
    Emitente.DadosEmitente.Email := edtEmailRemetente.Text;

    {
      Para o provedor ADM, utilizar as seguintes propriedades de configurações:
      WSChaveAcesso  para o Key
      WSChaveAutoriz para o Auth
      WSUser         para o RequestId

      O Key, Auth e RequestId são gerados pelo provedor quando o emitente se cadastra.
    }
  end;

  with ACBrNFSeX1.Configuracoes.WebServices do
  begin
    Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    Visualizar := cbxVisualizar.Checked;
    Salvar     := chkSalvarSOAP.Checked;
    UF         := edtEmitUF.Text;

    AjustaAguardaConsultaRet := cbxAjustarAut.Checked;

    if NaoEstaVazio(edtAguardar.Text)then
      AguardarConsultaRet := ifThen(StrToInt(edtAguardar.Text) < 1000, StrToInt(edtAguardar.Text) * 1000, StrToInt(edtAguardar.Text))
    else
      edtAguardar.Text := IntToStr(AguardarConsultaRet);

    if NaoEstaVazio(edtTentativas.Text) then
      Tentativas := StrToInt(edtTentativas.Text)
    else
      edtTentativas.Text := IntToStr(Tentativas);

    if NaoEstaVazio(edtIntervalo.Text) then
      IntervaloTentativas := ifThen(StrToInt(edtIntervalo.Text) < 1000, StrToInt(edtIntervalo.Text) * 1000, StrToInt(edtIntervalo.Text))
    else
      edtIntervalo.Text := IntToStr(ACBrNFSeX1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrNFSeX1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrNFSeX1.Configuracoes.Arquivos do
  begin
    NomeLongoNFSe    := True;
    Salvar           := chkSalvarArq.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathNFSe  := cbxEmissaoPathNFSe.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathGer          := edtPathLogs.Text;
    PathMensal       := GetPathGer(0);
    PathSalvar       := PathMensal;
  end;

  if ACBrNFSeX1.DANFSE <> nil then
  begin
    // TTipoDANFSE = ( tpPadrao, tpIssDSF, tpFiorilli );
    ACBrNFSeX1.DANFSE.TipoDANFSE := tpPadrao;
    ACBrNFSeX1.DANFSE.Logo       := edtLogoMarca.Text;
    ACBrNFSeX1.DANFSE.Prefeitura := edtPrefeitura.Text;
    ACBrNFSeX1.DANFSE.PathPDF    := edtPathPDF.Text;

    ACBrNFSeX1.DANFSE.Prestador.Logo := edtPrestLogo.Text;

    ACBrNFSeX1.DANFSE.MargemDireita  := 5;
    ACBrNFSeX1.DANFSE.MargemEsquerda := 5;
    ACBrNFSeX1.DANFSE.MargemSuperior := 5;
    ACBrNFSeX1.DANFSE.MargemInferior := 5;

    ACBrNFSeX1.DANFSE.ImprimeCanhoto := True;

    // Defini a quantidade de casas decimais para o campo aliquota
    ACBrNFSeX1.DANFSE.CasasDecimais.Aliquota := 2;
  end;

  with ACBrNFSeX1.MAIL do
  begin
    Host      := edtSmtpHost.Text;
    Port      := edtSmtpPort.Text;
    Username  := edtSmtpUser.Text;
    Password  := edtSmtpPass.Text;
    From      := edtEmailRemetente.Text;
    FromName  := edtEmitRazao.Text;
    SetTLS    := cbEmailTLS.Checked;
    SetSSL    := cbEmailSSL.Checked;
    UseThread := False;

    ReadingConfirmation := False;
  end;

  // A propriedade CodigoMunicipio tem que ser a ultima a receber o seu valor
  // Pois ela se utiliza das demais configurações
  with ACBrNFSeX1.Configuracoes.Geral do
  begin
    LayoutNFSe := TLayoutNFSe(cbLayoutNFSe.ItemIndex);
    CodigoMunicipio := StrToIntDef(edtCodCidade.Text, -1);
  end;

  lblSchemas.Caption := ACBrNFSeX1.Configuracoes.Geral.xProvedor;

  if ACBrNFSeX1.Configuracoes.Geral.Layout = loABRASF then
    lblLayout.Caption := 'ABRASF'
  else
    lblLayout.Caption := 'Próprio';

  lblVersaoSchemas.Caption := VersaoNFSeToStr(ACBrNFSeX1.Configuracoes.Geral.Versao);

  if ACBrNFSeX1.Configuracoes.Geral.Provedor = proPadraoNacional then
  begin
    pgcProvedores.Pages[0].TabVisible := False;
    pgcProvedores.Pages[1].TabVisible := True;
  end
  else
  begin
    pgcProvedores.Pages[0].TabVisible := True;
    pgcProvedores.Pages[1].TabVisible := False;
  end
end;

procedure TfrmACBrNFSe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser;
  NomeArq: string; aTempo: Integer);
begin
  if RetWS <> '' then
  begin
    WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + NomeArq,
                        RetWS, False, False);

    MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + NomeArq);

    sleep(aTempo);
  end;
end;

procedure TfrmACBrNFSe.PathClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(TEdit(Sender).Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := TEdit(Sender).Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
    TEdit(Sender).Text := Dir;
end;

function TfrmACBrNFSe.RoundTo5(Valor: Double; Casas: Integer): Double;
var
  xValor, xDecimais: String;
  p, nCasas: Integer;
  nValor: Double;
  OldRM: TFPURoundingMode;
begin
  nValor := Valor;
  xValor := Trim(FloatToStr(Valor));
  p := pos(',', xValor);

  if Casas < 0 then
    nCasas := -Casas
  else
    nCasas := Casas;

  if p > 0 then
  begin
    xDecimais := Copy(xValor, p + 1, Length(xValor));

    OldRM := GetRoundMode;
    try
      if Length(xDecimais) > nCasas then
      begin
        if xDecimais[nCasas + 1] >= '5' then
          SetRoundMode(rmUP)
        else
          SetRoundMode(rmNearest);
      end;

      nValor := RoundTo(Valor, Casas);

    finally
      SetRoundMode(OldRM);
    end;
  end;

  Result := nValor;
end;

procedure TfrmACBrNFSe.sbPathNFSeClick(Sender: TObject);
begin
  PathClick(edtPathNFSe);
end;

procedure TfrmACBrNFSe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFSe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrNFSeX1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrNFSe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFSe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  AddRow: Boolean;
begin
  ACBrNFSeX1.SSL.LerCertificadosStore;
  AddRow := False;

  with frmSelecionarCertificado.StringGrid1 do
  begin
    ColWidths[0] := 220;
    ColWidths[1] := 250;
    ColWidths[2] := 120;
    ColWidths[3] := 80;
    ColWidths[4] := 150;

    Cells[0, 0] := 'Num.Série';
    Cells[1, 0] := 'Razão Social';
    Cells[2, 0] := 'CNPJ';
    Cells[3, 0] := 'Validade';
    Cells[4, 0] := 'Certificadora';
  end;

  for I := 0 to ACBrNFSeX1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrNFSeX1.SSL.ListaCertificados[I] do
    begin
      if (CNPJ <> '') then
      begin
        with frmSelecionarCertificado.StringGrid1 do
        begin
          if Addrow then
            RowCount := RowCount + 1;

          Cells[0, RowCount-1] := NumeroSerie;
          Cells[1, RowCount-1] := RazaoSocial;
          Cells[2, RowCount-1] := CNPJ;
          Cells[3, RowCount-1] := FormatDateBr(DataVenc);
          Cells[4, RowCount-1] := Certificadora;

          AddRow := True;
        end;
      end;
    end;
  end;

  frmSelecionarCertificado.ShowModal;

  if frmSelecionarCertificado.ModalResult = mrOK then
    edtNumSerie.Text := frmSelecionarCertificado.StringGrid1.Cells[0, frmSelecionarCertificado.StringGrid1.Row];
end;

procedure TfrmACBrNFSe.sbtnPathPDFClick(Sender: TObject);
begin
  PathClick(edtPathPDF);
end;

procedure TfrmACBrNFSe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrNFSe.sbtnPrestLogoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter :=
    'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(Application.ExeName);

  if OpenDialog1.Execute then
  begin
    edtPrestLogo.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmACBrNFSe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TfrmACBrNFSe.cbCidadesChange(Sender: TObject);
var
  Tamanho: Integer;
begin
  Tamanho := Length(Trim(cbCidades.Text));

  edtEmitCidade.Text := Copy(cbCidades.Text, 1, Tamanho - 11);
  edtEmitUF.Text := Copy(cbCidades.Text, Tamanho - 1, 2);
  edtCodCidade.Text := Copy(cbCidades.Text, Tamanho - 9, 7);
end;

procedure TfrmACBrNFSe.btnGerarTokenClick(Sender: TObject);
begin
  ACBrNFSeX1.GerarToken;

  ChecarResposta(tmGerarToken);
end;

end.
