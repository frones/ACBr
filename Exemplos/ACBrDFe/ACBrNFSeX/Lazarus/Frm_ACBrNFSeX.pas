unit Frm_ACBrNFSeX;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls,
  SynEdit, ACBrUtil, ACBrDFe, ACBrDFeReport, ACBrNFSeX,
  ACBrNFSeXConversao,
  ACBrNFSeXDANFSeRLClass, ACBrMail, ACBrNFSeXWebservicesResponse;

type

  { TfrmACBrNFSe }

  TfrmACBrNFSe = class(TForm)
    chkMontarPathSchemas: TCheckBox;
    edtPathPDF: TEdit;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    lblSchemas: TLabel;
    lblVersaoSchemas: TLabel;
    pnlMenus: TPanel;
    pnlCentral: TPanel;
    PageControl1: TPageControl;
    sbtnPathPDF: TSpeedButton;
    WBXmlRetorno: TSynEdit;
    WBXmlEnvio: TSynEdit;
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
    btnDataValidade: TButton;
    btnNumSerie: TButton;
    btnSubName: TButton;
    btnCNPJ: TButton;
    btnIssuerName: TButton;
    grpCalculo: TGroupBox;
    edtTexto: TEdit;
    btnSha256: TButton;
    cbAssinar: TCheckBox;
    btnHTTPS: TButton;
    btnLeituraX509: TButton;
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
    ckSalvar: TCheckBox;
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
    cbxSalvarSOAP: TCheckBox;
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
    cbxSalvarArqs: TCheckBox;
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
    pgcBotoes: TPageControl;
    tsEnvios: TTabSheet;
    tsConsultas: TTabSheet;
    tsCancelamento: TTabSheet;
    pgRespostas: TPageControl;
    tsResposta: TTabSheet;
    //WBResposta: TWebBrowser;
    tsLog: TTabSheet;
    memoLog: TMemo;
    tsEnvio: TTabSheet;
    tsNotas: TTabSheet;
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
    btnGerarEnviarLote: TButton;
    btnGerarEnviarNFSe: TButton;
    btnGerarEnviarSincrono: TButton;
    btnImprimir: TButton;
    btnEnviaremail: TButton;
    btnLinkNFSe: TButton;
    btnGerarLoteRPS: TButton;
    btnSubsNFSe: TButton;
    btnConsultarSitLote: TButton;
    btnConsultarLote: TButton;
    btnConsultarNFSeRPS: TButton;
    btnConsultarNFSePeriodo: TButton;
    btnCancNFSe: TButton;
    ACBrMail1: TACBrMail;
    btnConsultarNFSePeloNumero: TButton;
    btnConsultarNFSeFaixa: TButton;
    ACBrNFSeX1: TACBrNFSeX;
    btnEmitir: TButton;
    Label43: TLabel;
    edtCNPJPrefeitura: TEdit;
    tsTeste: TTabSheet;
    btnGerarEnviarTeste_SP: TButton;
    mmoObs: TMemo;
    chkConsultaLoteAposEnvio: TCheckBox;
    chkConsultaAposCancelar: TCheckBox;
    ACBrNFSeXDANFSeRL1: TACBrNFSeXDANFSeRL;
    tsConsServPrest: TTabSheet;
    btnConsultarNFSeServicoPrestadoPorNumero: TButton;
    tsConsServTom: TTabSheet;
    btnConsultarNFSeServicoTomadoPorNumero: TButton;
    btnConsultarNFSeServicoPrestadoPorPeriodo: TButton;
    btnConsultarNFSeServicoPrestadoPorTomador: TButton;
    btnConsultarNFSeServicoPrestadoPorIntermediario: TButton;
    btnConsultarNFSeServicoTomadoPorPeriodo: TButton;
    btnConsultarNFSeServicoTomadoPorPrestador: TButton;
    btnConsultarNFSeServicoTomadoPorIntermediario: TButton;
    btnConsultarNFSeServicoTomadoPorTomador: TButton;
    btnConsultarNFSeGenerico: TButton;
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
    WBXmlNotas: TSynEdit;

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
    procedure btnSha256Click(Sender: TObject);
    procedure btnHTTPSClick(Sender: TObject);
    procedure btnLeituraX509Click(Sender: TObject);
    procedure sbtnPathPDFClick(Sender: TObject);
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
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure AlimentarNFSe(NumDFe, NumLote: String);
    procedure LoadXML(RetWS: String; SynEdit: TSynEdit);
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
  strutils, math, TypInfo, DateUtils, blcksock, Grids, IniFiles, Printers,
  pcnAuxiliar, pcnConversao,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeUtil, ACBrNFSeXWebserviceBase,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.lfm}

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

procedure TfrmACBrNFSe.AlimentarNFSe(NumDFe, NumLote: String);
var
  vValorISS: Double;
  i: Integer;
begin
  with ACBrNFSeX1 do
  begin
    NotasFiscais.NumeroLote := NumLote;
    NotasFiscais.Transacao := True;

    with NotasFiscais.New.NFSe do
    begin
      // Provedor SigISS
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

//      refNF := '123456789012345678901234567890123456789';
      Numero := NumDFe;
      // Provedor Infisc - Layout Proprio
      cNFSe := GerarCodigoDFe(StrToIntDef(Numero, 0));

      // no Caso dos provedores abaixo o campo SeriePrestacao devemos informar:
      {
        Número do equipamento emissor do RPS ou série de prestação.
        Caso não utilize a série, preencha o campo com o valor ‘99’ que indica
        modelo único. Caso queira utilizar o campo série para indicar o número do
        equipamento emissor do RPS deve-se solicitar liberação da prefeitura.
      }
      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat] then
        SeriePrestacao := '99'
      else
        SeriePrestacao := '1';

      NumeroLote := NumLote;

      IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumDFe));

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proNFSeBrasil,
        proEquiplano:
          IdentificacaoRps.Serie := '1';

        proSudoeste:
          IdentificacaoRps.Serie := 'E';

        proBetha,
        proISSDSF,
        proSiat:
          IdentificacaoRps.Serie := 'NF';

        proISSNet:
          if ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taProducao then
            IdentificacaoRps.Serie := '1'
          else
            IdentificacaoRps.Serie := '8';

      else
        IdentificacaoRps.Serie := '85'; // NF
      end;

      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      IdentificacaoRps.Tipo := trRPS;

      DataEmissao := Now;
      Competencia := Now;
      DataEmissaoRPS := Now;

      (*
        TnfseNaturezaOperacao = ( no1, no2, no3, no4, no5, no6, no7,
        no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
        no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
        no70, no71, no72, no78, no79,
        no101, no111, no121, no201, no301,
        no501, no511, no541, no551, no601, no701 );
      *)

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        // Provedor Thema: 50|51|52|53|54|55|56|57|58|59|60|61|62|63|64|65|66|67|68|69|70|71|72|78|79
        proThema:
          NaturezaOperacao := no51;
      else
        NaturezaOperacao := no1;
      end;

      {
        TnfseRegimeEspecialTributacao =
        ( retNenhum, retMicroempresaMunicipal, retEstimativa,
          retSociedadeProfissionais, retCooperativa,
          retMicroempresarioIndividual, retMicroempresarioEmpresaPP,
          retLucroReal, retLucroPresumido, retSimplesNacional,
          retImune, retEmpresaIndividualRELI, retEmpresaPP,
          retMicroEmpresario, retOutros);
      }
      RegimeEspecialTributacao := retMicroempresaMunicipal;

      // TnfseSimNao = ( snSim, snNao );
      OptanteSimplesNacional := snNao;

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
      if ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taProducao then
        Producao := snSim
      else
        Producao := snNao;

      // TnfseStatusRPS = ( srNormal, srCancelado );
      StatusRps := srNormal;

      // Somente Os provedores Betha, FISSLex e SimplISS permitem incluir no RPS
      // a TAG: OutrasInformacoes os demais essa TAG é gerada e preenchida pelo
      // WebService do provedor.
      OutrasInformacoes := 'Pagamento a Vista';

      // Usado quando o RPS for substituir outro
      {
       RpsSubstituido.Numero := FormatFloat('#########0', i);
       RpsSubstituido.Serie  := 'UNICA';
       // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
       RpsSubstituido.Tipo   := trRPS;
      }

      Servico.Valores.ValorServicos := 100.35;
      Servico.Valores.ValorDeducoes := 0.00;
      Servico.Valores.AliquotaPis := 1.00;
      Servico.Valores.ValorPis := 1.00;
      Servico.Valores.AliquotaCofins := 2.00;
      Servico.Valores.ValorCofins := 2.00;
      Servico.Valores.ValorInss := 0.00;
      Servico.Valores.ValorIr := 0.00;
      Servico.Valores.ValorCsll := 0.00;

      // Provedor Elotech
      Servico.Valores.RetidoPis := snNao;
      Servico.Valores.RetidoCofins := snNao;
      Servico.Valores.AliquotaInss := 0;
      Servico.Valores.RetidoInss := snNao;
      Servico.Valores.AliquotaIr := 0;
      Servico.Valores.RetidoIr := snNao;
      Servico.Valores.AliquotaCsll := 0;
      Servico.Valores.RetidoCsll := snNao;

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

      // TnfseResponsavelRetencao = ( rtTomador, rtPrestador, rtIntermediario, rtNenhum )
      //                              '1',       '',          '2',             ''
      Servico.ResponsavelRetencao := rtTomador;

      Servico.ItemListaServico := '09.01';

      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proISSDSF, proSiat,
          proAgili] then
        Servico.CodigoCnae := '452000200'
      else
        Servico.CodigoCnae := '852010';

      if (ACBrNFSeX1.Configuracoes.Geral.Provedor = proISSNet) and
         (ACBrNFSeX1.Configuracoes.WebServices.Ambiente = taHomologacao)  then
        Servico.CodigoCnae := '6511102';

      case ACBrNFSeX1.Configuracoes.Geral.Provedor of
        proISSSJP:
          Servico.CodigoTributacaoMunicipio := '631940000';

        proCenti:
          Servico.CodigoTributacaoMunicipio := '0901';

        proISSSalvador:
          Servico.CodigoTributacaoMunicipio := '0901001';

        proIPM:
          Servico.CodigoTributacaoMunicipio := '';
      else
        Servico.CodigoTributacaoMunicipio := '63194';
      end;

      Servico.Discriminacao := 'discriminacao I; discriminacao II';

      // Para o provedor ISS.NET em ambiente de Homologação
      // o Codigo do Municipio tem que ser '999'
      Servico.CodigoMunicipio := edtCodCidade.Text;
      Servico.UFPrestacao := 'SP';

      // Informar A Exigibilidade ISS para fintelISS [1/2/3/4/5/6/7]
      Servico.ExigibilidadeISS := exiExigivel;

      // Informar para Saatri
//      Servico.CodigoPais := 1058; // Brasil
      Servico.MunicipioIncidencia := StrToIntDef(edtCodCidade.Text, 0);

      // Provedores que permitem informar mais de 1 serviço:
      // Agili, AssessorPublico, EL, EloTech, Equiplano, fintelISS, Governa,
      // Infisc, IPM, ISSDSF, Simple, SmarAPD, WebFisco
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

        BaseCalculo := ValorTotal - ValorDeducoes - DescontoIncondicionado;

        Aliquota := 4;

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

        // Provedor IPM
        { define se o tributo é no municipio do prestador ou não }
        TribMunPrestador := snNao;
        { codigo do municipio que ocorreu a prestação de serviço }
        CodMunPrestacao :=  edtCodCidade.Text;
        { codigo da situação tributária: 0 até 15 }
        SituacaoTributaria := 0;
      end;

      Prestador.IdentificacaoPrestador.CpfCnpj := edtEmitCNPJ.Text; //'88888888888888';
      Prestador.IdentificacaoPrestador.InscricaoMunicipal := edtEmitIM.Text;

      Prestador.RazaoSocial  := edtEmitRazao.Text;
      Prestador.NomeFantasia := edtEmitRazao.Text;
      // Para o provedor ISSDigital deve-se informar também:
      Prestador.cUF := UFtoCUF(edtEmitUF.Text);

      Prestador.Endereco.Endereco := edtEmitLogradouro.Text;
      Prestador.Endereco.Numero   := edtEmitNumero.Text;
      Prestador.Endereco.Bairro   := edtEmitBairro.Text;
      Prestador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Prestador.Endereco.xMunicipio := CodIBGEToCidade(StrToIntDef(edtCodCidade.Text, 0));
      Prestador.Endereco.UF := edtEmitUF.Text;
      Prestador.Endereco.CodigoPais := 1058;
      Prestador.Endereco.xPais := 'BRASIL';
      Prestador.Endereco.CEP := '14800123';

      Prestador.Contato.Telefone := '1633224455';
      Prestador.Contato.Email    := 'nome@provedor.com.br';

      // Para o provedor IPM usar os valores:
      // tpPFNaoIdentificada ou tpPF para pessoa Fisica
      // tpPJdoMunicipio ou tpPJforaMunicipio ou tpPJforaPais para pessoa Juridica

      // Para o provedor SigISS usar os valores acima de forma adquada
      Tomador.IdentificacaoTomador.Tipo := tpPJdoMunicipio;
      Tomador.IdentificacaoTomador.CpfCnpj := edtEmitCNPJ.Text; //'55555555555555';
      Tomador.IdentificacaoTomador.InscricaoMunicipal := '17331600';

      Tomador.RazaoSocial := 'INSCRICAO DE TESTE';

      // O campo EnderecoInformado é utilizado pelo provedor IMP
      // A tag <endereco_informado> é opcional, caso não deseje que ela seja
      // gerada devemos informar uma string vazia, ou S = Sim ou N = Não
      Tomador.Endereco.EnderecoInformado := 'S';
      Tomador.Endereco.TipoLogradouro := 'RUA';
      Tomador.Endereco.Endereco := 'RUA PRINCIPAL';
      Tomador.Endereco.Numero := '100';
      Tomador.Endereco.Complemento := 'APTO 11';
      Tomador.Endereco.Bairro := 'CENTRO';
      Tomador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Tomador.Endereco.xMunicipio := CodIBGEToCidade(StrToIntDef(edtCodCidade.Text, 0));
      Tomador.Endereco.UF := edtEmitUF.Text;
//      Tomador.Endereco.CodigoPais := 1058; // Brasil
      Tomador.Endereco.CEP := edtEmitCEP.Text;

      // Provedor Equiplano é obrigatório o pais e IE
      Tomador.Endereco.xPais := 'BRASIL';
      Tomador.IdentificacaoTomador.InscricaoEstadual := '123456';

      if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proSigep] then
        Tomador.Contato.Telefone := '22223333'
      else
        Tomador.Contato.Telefone := '1622223333';

      Tomador.Contato.Email := 'nome@provedor.com.br';

      Tomador.AtualizaTomador := snNao;
      Tomador.TomadorExterior := snNao;

      // Usado quando houver um intermediario na prestação do serviço
      // IntermediarioServico.RazaoSocial        := 'razao';
      // IntermediarioServico.CpfCnpj            := '00000000000';
      // IntermediarioServico.InscricaoMunicipal := '12547478';

      // Usado quando o serviço for uma obra
      // ConstrucaoCivil.CodigoObra := '88888';
      // ConstrucaoCivil.Art        := '433';

      // Condição de Pagamento usado pelo provedor Betha versão 1 do Layout da ABRASF
      CondicaoPagamento.QtdParcela := 2;
//      CondicaoPagamento.Condicao   := cpAPrazo;
      CondicaoPagamento.Condicao   := cpAVista;

      for i := 1 to CondicaoPagamento.QtdParcela do
      begin
        with CondicaoPagamento.Parcelas.New do
        begin
          Parcela := i;
          DataVencimento := Date + (30 * i);
          Valor := (Servico.Valores.ValorLiquidoNfse / CondicaoPagamento.QtdParcela);
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
  Cidades    := TStringList.Create;

  ACBrNFSeX1.LerCidades;
  IniCidades.SetStrings(ACBrNFSeX1.Configuracoes.WebServices.Params);

  try
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
  NumNFSe, Codigo, Motivo, NumLote, CodVerif, SerNFSe, NumRps,
  SerRps, ValNFSe, ChNFSe: String;
  CodCanc: Integer;
  InfCancelamento: TInfCancelamento;
  Titulo: string;
begin
  Titulo := 'Cancelar NFSe';

  // Os Provedores da lista requerem que seja informado a chave e o código
  // de cancelamento
  if (ACBrNFSeX1.Configuracoes.Geral.Provedor = proInfisc) and
     (ACBrNFSeX1.Configuracoes.Geral.Versao <> ve201) then
  begin
    ChNFSe := '12345678';
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

    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proiiBrasil, proWebFisco] then
    begin
      SerNFSe := '1';
      if not (InputQuery(Titulo, 'Série da NFSe', SerNFSe)) then
        exit;
    end;

    // Provedor Conam
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
      proConam, proEquiplano, proGoverna, proIPM, proISSDSF, proISSLencois,
      proModernizacaoPublica, proPublica, proSiat, proSigISS, proSigep,
      proSmarAPD, proWebFisco, proTecnos, proSudoeste, proSimple, proFGMaiss] then
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
    if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proInfisc, proISSDSF,
         proISSLencois, proGoverna, proSiat, proSigep, proElotech] then
    begin
      CodVerif := '12345678';
      if not (InputQuery(Titulo, 'Código de Verificação ou Chave de Autenticação', CodVerif)) then
        exit;
    end;
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
  if not(InputQuery(xTitulo, 'Série da NFSe:', SerNFSe)) then
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

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

  CadEcon := '';
  if not(InputQuery(xTitulo, 'Cadastro Economico (Insc. Munic.):', CadEcon)) then
    exit;

  InfConsultaNFSe := TInfConsultaNFSe.Create;

  try
    with InfConsultaNFSe do
    begin
      // Valores aceito para o Tipo de Consulta:
      // tcPorNumero, tcPorFaixa, tcPorPeriodo, tcServicoPrestado,
      // tcServicoTomado, tcPorNumeroURLRetornado
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
  xTitulo, NumeroNFSe, SerNFSe, NumPagina, NumLote, xDataIni, xDataFin, xTipo: String;
  InfConsultaNFSe: TInfConsultaNFSe;
begin
  xTitulo := 'Consultar NFSe Por Numero';

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

  xTipo := '';
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proFGMaiss, proWebFisco] then
  begin
    if not(InputQuery(xTitulo, 'Tipo da NFSe:', xTipo)) then
      exit;
  end;

  NumPagina := '1';
  if not(InputQuery(xTitulo, 'Pagina:', NumPagina)) then
    exit;

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

    proFGMaiss,
    proWebFisco:
      begin
        InfConsultaNFSe := TInfConsultaNFSe.Create;

        try
          with InfConsultaNFSe do
          begin
            tpConsulta := tcPorNumero;

            NumeroIniNFSe := NumeroNFSe;
            Tipo := xTipo;
          end;

          ACBrNFSeX1.ConsultarNFSeGenerico(InfConsultaNFSe);
        finally
          InfConsultaNFSe.Free;
        end;
      end;

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
  else
    ACBrNFSeX1.ConsultarNFSeporNumero(NumeroNFSe);
  end;

  ChecarResposta(tmConsultarNFSe);
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
  {
  NumeroLote := '';
  if not (InputQuery('Consultar NFSe por RPS', 'Numero do Lote:', NumeroLote)) then
    exit;
  }
  if ACBrNFSeX1.Configuracoes.Geral.Provedor in [proGiap, proGoverna] then
  begin
    CodVerificacao := '123';
    if not (InputQuery('Consultar NFSe por RPS', 'Codigo Verificação:', CodVerificacao)) then
      exit;
  end;

  ACBrNFSeX1.ConsultarNFSeporRps(NumeroRps, SerieRps, TipoRps, CodVerificacao);

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
  ShowMessage(FormatDateBr(ACBrNFSeX1.SSL.CertDataVenc));
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
    AlimentarNFSe(vNumRPS, vNumLote);
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
    ACBrNFSeX1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

    vAux := '';
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

    memolog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArq);

    pgRespostas.ActivePageIndex := 0;
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
    AlimentarNFSe(vNumRPS, vNumLote);
    inc(iAux);
  end;

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (Integer ou String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario, meTeste]
     aImprimir (Boolean)
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
  AlimentarNFSe(vNumRPS, vNumLote);

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (Integer ou String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario]
     aImprimir (Boolean)
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
    AlimentarNFSe(vNumRPS, vNumLote);
    inc(iAux);
  end;

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (Integer ou String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario, meTeste]
     aImprimir (Boolean)
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
  // A function GerarLote apenas gera o XML do lote, assina se necessário
  // e valida, salvando o arquivo com o nome: <lote>-lot-rps.xml na pasta Ger
  // Não ocorre o envio para nenhum webservice.
  //
  // **************************************************************************
  vNumRPS := '';
  if not(InputQuery('Gerar e Enviar Lote', 'Numero do RPS', vNumRPS)) then
    exit;

  vNumLote := vNumRPS;
  if not(InputQuery('Gerar e Enviar Lote', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrNFSeX1.NotasFiscais.Clear;
  AlimentarNFSe(vNumRPS, vNumLote);

  ACBrNFSeX1.GerarLote(vNumLote);

  ChecarResposta(tmGerarLote);
end;

procedure TfrmACBrNFSe.btnHTTPSClick(Sender: TObject);
var
  Acao: String;
  OldUseCert: Boolean;
begin
  Acao := '<?xml version="1.0" encoding="UTF-8" standalone="no"?>' +
     '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
     'xmlns:cli="http://cliente.bean.master.sigep.bsb.correios.com.br/"> ' +
     ' <soapenv:Header/>' +
     ' <soapenv:Body>' +
     ' <cli:consultaCEP>' +
     ' <cep>18270-170</cep>' +
     ' </cli:consultaCEP>' +
     ' </soapenv:Body>' +
     ' </soapenv:Envelope>';

  OldUseCert := ACBrNFSeX1.SSL.UseCertificateHTTP;
  ACBrNFSeX1.SSL.UseCertificateHTTP := False;

  try
    memolog.Lines.Text := ACBrNFSeX1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrNFSeX1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
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
    ACBrNFSeX1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

        // LoadFromLoteNfse - Usado para carregar um lote de notas
    //    ACBrNFSeX1.NotasFiscais.LoadFromLoteNfse(OpenDialog1.FileName);

    ACBrNFSeX1.NotasFiscais.Imprimir;
    ACBrNFSeX1.NotasFiscais.ImprimirPDF;

    if ACBrNFSeX1.NotasFiscais.Items[0].NomeArqRps <> '' then
      memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArqRps)
    else
      memoLog.Lines.Add('Arquivo Carregado de: ' + ACBrNFSeX1.NotasFiscais.Items[0].NomeArq);

    memoLog.Lines.Add('Nota Numero: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.Numero);
    memoLog.Lines.Add('Código de Verificação: ' + ACBrNFSeX1.NotasFiscais.Items[0].NFSe.CodigoVerificacao);
    memoLog.Lines.Add('Data de Emissão: ' + DateToStr(ACBrNFSeX1.NotasFiscais.Items[0].NFSe.DataEmissao));

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrNFSe.btnIssuerNameClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSeX1.SSL.CertIssuerName + sLineBreak + sLineBreak +
              'Certificadora: ' + ACBrNFSeX1.SSL.CertCertificadora);
end;

procedure TfrmACBrNFSe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrNFSeX1.SSL do
  begin
     CarregarCertificadoPublico(AnsiString(memoLog.Lines.Text));
     memoLog.Lines.Add(CertIssuerName);
     memoLog.Lines.Add(CertRazaoSocial);
     memoLog.Lines.Add(CertCNPJ);
     memoLog.Lines.Add(CertSubjectName);
     memoLog.Lines.Add(CertNumeroSerie);

    //MemoDados.Lines.LoadFromFile('c:\temp\teste2.xml');
    //MemoResp.Lines.Text := Assinar(MemoDados.Lines.Text, 'Entrada', 'Parametros');
    //Erro := '';
    //if VerificarAssinatura(MemoResp.Lines.Text, Erro, 'Parametros' ) then
    //  ShowMessage('OK')
    //else
    //  ShowMessage('ERRO: '+Erro)

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrNFSe.sbtnPathPDFClick(Sender: TObject);
begin
  PathClick(edtPathPDF);
end;

procedure TfrmACBrNFSe.btnLinkNFSeClick(Sender: TObject);
var
  vNumNFSe, sCodVerif, sLink: String;
begin
  vNumNFSe := '';
  if not(InputQuery('Gerar o Link da NFSe', 'Numero da NFSe', vNumNFSe)) then
    exit;

  sCodVerif := '';
  if not(InputQuery('Gerar o Link da NFSe', 'Codigo de Verificacao', sCodVerif)) then
    exit;

  sLink := ACBrNFSeX1.LinkNFSe(vNumNFSe, sCodVerif);

  memoLog.Lines.Add('Link Gerado: ' + sLink);

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrNFSeX1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrNFSe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrNFSe.btnSha256Click(Sender: TObject);
var
  Ahash: String;
  xAssinatura: TStringList;
begin
  xAssinatura := TStringList.Create;
  try
    xAssinatura.Add(edtTexto.Text);

    Ahash := string(ACBrNFSeX1.SSL.CalcHash(xAssinatura, dgstSHA256, outBase64, cbAssinar.Checked));
    memoLog.Lines.Add( Ahash );
    pgRespostas.ActivePageIndex := 0;
  finally
    xAssinatura.Free;
  end;
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
  AlimentarNFSe(vNumRPS, '1');

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
    proPublica, proSiat, proSigISS, proSmarAPD, proWebFisco, proSudoeste] then
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
  xTitulo, NumPagina, CPFCNPJTomador, IMTomador: String;
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

  ACBrNFSeX1.ConsultarNFSeServicoTomadoPorTomador(CPFCNPJTomador,
    IMTomador, StrToIntDef(NumPagina, 1));

  ChecarResposta(tmConsultarNFSeServicoTomado);
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
    AlimentarNFSe(vNumRPS, vNumLote);
    inc(iAux);
  end;

  {
     O método Emitir possui os seguintes parâmetros:
     aNumLote (Integer ou String)
     aModEnvio [meAutomatico, meLoteAssincrono, meLoteSincrono, meUnitario, meTeste]
     aImprimir (Boolean)
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
begin
  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add( GetEnumName(TypeInfo(TSSLLib), integer(T) ) );
  cbSSLLib.ItemIndex := 0;

  cbCryptLib.Items.Clear;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add( GetEnumName(TypeInfo(TSSLCryptLib), integer(U) ) );
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Items.Clear;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add( GetEnumName(TypeInfo(TSSLHttpLib), integer(V) ) );
  cbHttpLib.ItemIndex := 0;

  cbXmlSignLib.Items.Clear;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add( GetEnumName(TypeInfo(TSSLXmlSignLib), integer(X) ) );
  cbXmlSignLib.ItemIndex := 0;

  cbSSLType.Items.Clear;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add( GetEnumName(TypeInfo(TSSLType), integer(Y) ) );
  cbSSLType.ItemIndex := 0;

  cbFormaEmissao.Items.Clear;
  for I := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
    cbFormaEmissao.Items.Add( GetEnumName(TypeInfo(TpcnTipoEmissao), integer(I) ) );
  cbFormaEmissao.ItemIndex := 0;

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
    Ini.WriteBool(   'Geral', 'Salvar',           ckSalvar.Checked);
    Ini.WriteString( 'Geral', 'PathSalvar',       edtPathLogs.Text);
    Ini.WriteString( 'Geral', 'PathSchemas',      edtPathSchemas.Text);
    Ini.WriteString( 'Geral', 'LogoMarca',        edtLogoMarca.Text);
    Ini.WriteString( 'Geral', 'PrestLogo',        edtPrestLogo.Text);
    Ini.WriteString( 'Geral', 'Prefeitura',       edtPrefeitura.Text);

    Ini.WriteBool(   'Geral', 'ConsultaAposEnvio',    chkConsultaLoteAposEnvio.Checked);
    Ini.WriteBool(   'Geral', 'ConsultaAposCancelar', chkConsultaAposCancelar.Checked);
    Ini.WriteBool(   'Geral', 'MontarPathSchemas',    chkMontarPathSchemas.Checked);

    Ini.WriteInteger('WebService', 'Ambiente',     rgTipoAmb.ItemIndex);
    Ini.WriteBool(   'WebService', 'Visualizar',   cbxVisualizar.Checked);
    Ini.WriteBool(   'WebService', 'SalvarSOAP',   cbxSalvarSOAP.Checked);
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

    Ini.WriteBool(  'Arquivos', 'Salvar',          cbxSalvarArqs.Checked);
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
    cbSSLLib.ItemIndex     := Ini.ReadInteger('Certificado', 'SSLLib',     0);
    cbCryptLib.ItemIndex   := Ini.ReadInteger('Certificado', 'CryptLib',   0);
    cbHttpLib.ItemIndex    := Ini.ReadInteger('Certificado', 'HttpLib',    0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger('Certificado', 'XmlSignLib', 0);
    edtCaminho.Text        := Ini.ReadString( 'Certificado', 'Caminho',    '');
    edtSenha.Text          := Ini.ReadString( 'Certificado', 'Senha',      '');
    edtNumSerie.Text       := Ini.ReadString( 'Certificado', 'NumSerie',   '');

    AtualizarCidades;

    cbxAtualizarXML.Checked     := Ini.ReadBool(   'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(   'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString( 'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger('Geral', 'FormaEmissao',     0);

    ckSalvar.Checked          := Ini.ReadBool(  'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(  'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString('Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString('Geral', 'PathSchemas',    '');
    edtLogoMarca.Text         := Ini.ReadString('Geral', 'LogoMarca',      '');
    edtPrestLogo.Text         := Ini.ReadString('Geral', 'PrestLogo',      '');
    edtPrefeitura.Text        := Ini.ReadString('Geral', 'Prefeitura',     '');

    chkConsultaLoteAposEnvio.Checked := Ini.ReadBool('Geral', 'ConsultaAposEnvio', False);
    chkConsultaAposCancelar.Checked  := Ini.ReadBool('Geral', 'ConsultaAposCancelar', False);
    chkMontarPathSchemas.Checked     := Ini.ReadBool('Geral', 'MontarPathSchemas',    True);

    rgTipoAmb.ItemIndex     := Ini.ReadInteger('WebService', 'Ambiente',    0);
    cbxVisualizar.Checked   := Ini.ReadBool(   'WebService', 'Visualizar',  False);
    cbxSalvarSOAP.Checked   := Ini.ReadBool(   'WebService', 'SalvarSOAP',  False);
    cbxAjustarAut.Checked   := Ini.ReadBool(   'WebService', 'AjustarAut',  False);
    edtAguardar.Text        := Ini.ReadString( 'WebService', 'Aguardar',    '0');
    edtTentativas.Text      := Ini.ReadString( 'WebService', 'Tentativas',  '5');
    edtIntervalo.Text       := Ini.ReadString( 'WebService', 'Intervalo',   '0');
    seTimeOut.Value         := Ini.ReadInteger('WebService', 'TimeOut',     5000);
    cbSSLType.ItemIndex     := Ini.ReadInteger('WebService', 'SSLType',     0);
    edtSenhaWeb.Text        := Ini.ReadString( 'WebService', 'SenhaWeb',    '');
    edtUserWeb.Text         := Ini.ReadString( 'WebService', 'UserWeb',     '');
    edtFraseSecWeb.Text     := Ini.ReadString( 'WebService', 'FraseSecWeb', '');
    edtChaveAcessoWeb.Text  := Ini.ReadString( 'WebService', 'ChAcessoWeb', '');
    edtChaveAutorizWeb.Text := Ini.ReadString( 'WebService', 'ChAutorizWeb', '');

    edtProxyHost.Text  := Ini.ReadString('Proxy', 'Host',  '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text  := Ini.ReadString('Proxy', 'User',  '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass',  '');

    cbxSalvarArqs.Checked       := Ini.ReadBool(  'Arquivos', 'Salvar',           false);
    cbxPastaMensal.Checked      := Ini.ReadBool(  'Arquivos', 'PastaMensal',      false);
    cbxAdicionaLiteral.Checked  := Ini.ReadBool(  'Arquivos', 'AddLiteral',       false);
    cbxEmissaoPathNFSe.Checked  := Ini.ReadBool(  'Arquivos', 'EmissaoPathNFSe',   false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    edtPathNFSe.Text            := Ini.ReadString('Arquivos', 'PathNFSe',          '');

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
    cbCidades.ItemIndex    := cbCidades.Items.IndexOf(edtEmitCidade.Text + '/' +
      edtCodCidade.Text + '/' + edtEmitUF.Text);
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
    edtLogoMarca.Text      := Ini.ReadString( 'DANFSE', 'LogoMarca',  '');
    edtPathPDF.Text        := Ini.ReadString( 'DANFSE', 'PathPDF',   '');

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFSe.ChecarResposta(aMetodo: TMetodo);
var
  i: Integer;
begin
  memoLog.Clear;
  memoLog.Lines.Clear;
  memoLog.Update;

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
            memoLog.Lines.Add('Numero do Lote: ' + Lote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Data de Envio : ' + DateToStr(Data));
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Numero da Nota: ' + NumeroNota);
            memoLog.Lines.Add('Link          : ' + Link);
            memoLog.Lines.Add('Código Verif. : ' + CodVerificacao);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
          end;

          if ACBrNFSeX1.Configuracoes.Geral.ConsultaLoteAposEnvio and
             (Emite.Protocolo <> '') then
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
                memoLog.Lines.Add('Numero do Lote: ' + Lote);
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Parâmetros de Retorno');
                memoLog.Lines.Add('Situação Lote : ' + Situacao);
                memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

                LoadXML(XmlEnvio, WBXmlEnvio);
                LoadXML(XmlRetorno, WBXmlRetorno);

                if Erros.Count > 0 then
                begin
                  memoLog.Lines.Add(' ');
                  memoLog.Lines.Add('Erro(s):');
                  for i := 0 to Erros.Count -1 do
                  begin
                    memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                    memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                    memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                    memoLog.Lines.Add('---------');
                  end;
                end;

                if Alertas.Count > 0 then
                begin
                  memoLog.Lines.Add(' ');
                  memoLog.Lines.Add('Alerta(s):');
                  for i := 0 to Alertas.Count -1 do
                  begin
                    memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                    memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                    memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                    memoLog.Lines.Add('---------');
                  end;
                end;
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
                memoLog.Lines.Add('Numero do Lote: ' + Lote);
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Parâmetros de Retorno');
                memoLog.Lines.Add('Situação Lote : ' + Situacao);
                memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

                LoadXML(XmlEnvio, WBXmlEnvio);
                LoadXML(XmlRetorno, WBXmlRetorno);

                if Erros.Count > 0 then
                begin
                  memoLog.Lines.Add(' ');
                  memoLog.Lines.Add('Erro(s):');
                  for i := 0 to Erros.Count -1 do
                  begin
                    memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                    memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                    memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                    memoLog.Lines.Add('---------');
                  end;
                end;

                if Alertas.Count > 0 then
                begin
                  memoLog.Lines.Add(' ');
                  memoLog.Lines.Add('Alerta(s):');
                  for i := 0 to Alertas.Count -1 do
                  begin
                    memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                    memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                    memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                    memoLog.Lines.Add('---------');
                  end;
                end;
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
            memoLog.Lines.Add('Numero do Lote: ' + Lote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Data de Envio : ' + DateToStr(Data));
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Numero da Nota: ' + NumeroNota);
            memoLog.Lines.Add('Link          : ' + Link);
            memoLog.Lines.Add('Código Verif. : ' + CodVerificacao);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
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
            memoLog.Lines.Add('Numero do Lote: ' + Lote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Situação Lote : ' + Situacao);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
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
            memoLog.Lines.Add('Numero do Lote: ' + Lote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Situação Lote : ' + Situacao);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
          end;
        end;

      tmConsultarNFSePorRps:
        begin
          with ConsultaNFSeporRps do
          begin
            memoLog.Lines.Add('Método Executado: ' + MetodoToStr(tmConsultarNFSePorRps));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Rps : ' + NumRPS);
            memoLog.Lines.Add('Série do Rps  : ' + Serie);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Numero do Lote: ' + Lote);
            memoLog.Lines.Add('Numero do Prot: ' + Protocolo);
            memoLog.Lines.Add('Situação Lote : ' + Situacao);
            memoLog.Lines.Add('Data          : ' + DateToStr(Data));
            memoLog.Lines.Add('Desc. Situação: ' + DescSituacao);
            memoLog.Lines.Add('Link          : ' + Link);
            memoLog.Lines.Add('Sucesso       : ' + BoolToStr(Sucesso, True));

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
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

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
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

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
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

              LoadXML(XmlEnvio, WBXmlEnvio);
              LoadXML(XmlRetorno, WBXmlRetorno);

              if Erros.Count > 0 then
              begin
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Erro(s):');
                for i := 0 to Erros.Count -1 do
                begin
                  memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                  memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                  memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                  memoLog.Lines.Add('---------');
                end;
              end;

              if Alertas.Count > 0 then
              begin
                memoLog.Lines.Add(' ');
                memoLog.Lines.Add('Alerta(s):');
                for i := 0 to Alertas.Count -1 do
                begin
                  memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                  memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                  memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                  memoLog.Lines.Add('---------');
                end;
              end;
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

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
          end;
        end;

      tmGerarLote:
        begin
          with Gerar do
          begin
            memoLog.Lines.Add('Método Executado: ' + ModoEnvioToStr(ModoEnvio));
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Envio');
            memoLog.Lines.Add('Numero do Lote: ' + Lote);
            memoLog.Lines.Add(' ');
            memoLog.Lines.Add('Parâmetros de Retorno');
            memoLog.Lines.Add('Nome Arquivo : ' + NomeArq);

            LoadXML(XmlEnvio, WBXmlEnvio);
            LoadXML(XmlRetorno, WBXmlRetorno);

            if Erros.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Erro(s):');
              for i := 0 to Erros.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Erros[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Erros[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Erros[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;

            if Alertas.Count > 0 then
            begin
              memoLog.Lines.Add(' ');
              memoLog.Lines.Add('Alerta(s):');
              for i := 0 to Alertas.Count -1 do
              begin
                memoLog.Lines.Add('Código  : ' + Alertas[i].Codigo);
                memoLog.Lines.Add('Mensagem: ' + Alertas[i].Descricao);
                memoLog.Lines.Add('Correção: ' + Alertas[i].Correcao);
                memoLog.Lines.Add('---------');
              end;
            end;
          end;
        end;

      tmAbrirSessao:
        begin
        end;

      tmFecharSessao:
        begin
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
    memoLog.Lines.Add('Nome do arquivo.: ' + ACBrNFSeX1.Configuracoes.Arquivos.GetPathNFSe() + '\' +
                                             ACBrNFSeX1.NotasFiscais.Items[i].NomeArq);
    if ACBrNFSeX1.Configuracoes.Arquivos.Salvar then
      memoLog.Lines.Add('==> Xml da nota salvo na pasta e com o nome informado acima.')
    else
      memoLog.Lines.Add('==> Xml da nota não salvo em disco.');

    // Na propriedade XML temos o XML da NFS-e
    LoadXML(ACBrNFSeX1.NotasFiscais.Items[i].XmlNfse, WBXmlNotas);
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFSe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: String;
begin
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

    Salvar           := ckSalvar.Checked;
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

    // Para o provedor Giap a Chave de Autorização deve ser composta:
    // Inscrição Municipal - Chave
    Emitente.WSChaveAutoriz := edtChaveAutorizWeb.Text;

    {
      Para o provedor ADM, utilizar as seguintes propriedades de configurações:
      WSChaveAcesso  para o Key
      WSChaveAutoriz para o Auth
      WSUser         para o RequestId

      Essas 3 propriedades são geradas pelo provedor quando o emitente se cadastra
    }
  end;

  with ACBrNFSeX1.Configuracoes.WebServices do
  begin
    Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    Visualizar := cbxVisualizar.Checked;
    Salvar     := cbxSalvarSOAP.Checked;
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
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathNFSe  := cbxEmissaoPathNFSe.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathGer          := edtPathLogs.Text;
    PathMensal       := GetPathGer(0);
    PathSalvar       := PathMensal;
    PathCan          := PathMensal;
  end;

  if ACBrNFSeX1.DANFSe <> nil then
  begin
    // TTipoDANFSE = ( tpPadrao, tpIssDSF, tpFiorilli );
    ACBrNFSeX1.DANFSe.TipoDANFSE := tpPadrao;
    ACBrNFSeX1.DANFSe.Logo       := edtLogoMarca.Text;
    ACBrNFSeX1.DANFSe.Prefeitura := edtPrefeitura.Text;
    ACBrNFSeX1.DANFSe.PathPDF    := edtPathPDF.Text;

    ACBrNFSeX1.DANFSe.Prestador.Logo := edtPrestLogo.Text;

    ACBrNFSeX1.DANFSe.MargemDireita  := 5;
    ACBrNFSeX1.DANFSe.MargemEsquerda := 5;
    ACBrNFSeX1.DANFSe.MargemSuperior := 5;
    ACBrNFSeX1.DANFSe.MargemInferior := 5;
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
    CodigoMunicipio := StrToIntDef(edtCodCidade.Text, 0);

    // Exemplos de valores para WSChaveAcesso para alguns provedores.

    if Provedor in [proAgili, proElotech] then
      Emitente.WSChaveAcesso := '0aA1bB2cC3dD4eE5fF6aA7bB8cC9dDEF';

    if Provedor = proISSNet then
      Emitente.WSChaveAcesso := 'A001.B0001.C0001-1';

    if Provedor = proSigep then
      Emitente.WSChaveAcesso := 'A001.B0001.C0001';

    if Provedor = proiiBrasil then
      Emitente.WSChaveAcesso := 'TLXX4JN38KXTRNSEAJYYEA==';
  end;

  lblSchemas.Caption := ACBrNFSeX1.Configuracoes.Geral.xProvedor;
  lblVersaoSchemas.Caption := VersaoNFSeToStr(ACBrNFSeX1.Configuracoes.Geral.Versao);
end;

procedure TfrmACBrNFSe.LoadXML(RetWS: String; SynEdit: TSynEdit);
var
  vText: String;
begin
  vText := RetWS;

  // formata resposta
  vText := StringReplace(vText, '>', '>' + LineEnding + '    ', [rfReplaceAll]);
  vText := StringReplace(vText, '<', LineEnding + '  <', [rfReplaceAll]);
  vText := StringReplace(vText, '>' + LineEnding + '    ' + LineEnding +
           '  <', '>' + LineEnding + '  <', [rfReplaceAll]);
  vText := StringReplace(vText, '  </ret', '</ret', []);

  // exibe resposta
  SynEdit.Text := Trim(vText);
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

    if Length(xDecimais) > nCasas then
    begin
      if xDecimais[nCasas + 1] >= '5' then
        SetRoundMode(rmUP)
      else
        SetRoundMode(rmNearest);
    end;

    nValor := RoundTo(Valor, Casas);
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
    edtLogoMarca.Text := OpenDialog1.FileName;
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

end.
