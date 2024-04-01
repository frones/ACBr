{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			                                         }
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

unit Frm_ACBrNFe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, ComCtrls, OleCtrls, SHDocVw, ACBrMail,
  ACBrPosPrinter, ACBrNFeDANFeESCPOS, ACBrNFeDANFEClass, ACBrDANFCeFortesFr,
  ACBrDFeReport, ACBrDFeDANFeReport, ACBrNFeDANFeRLClass, ACBrBase, ACBrDFe,
  ACBrNFe, ShellAPI, XMLIntf, XMLDoc, zlib, ACBrIntegrador,
  ACBrDANFCeFortesFrA4;

type
  TfrmACBrNFe = class(TForm)
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
    btnDataValidade: TButton;
    btnNumSerie: TButton;
    btnSubName: TButton;
    btnCNPJ: TButton;
    btnIssuerName: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
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
    Label30: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label42: TLabel;
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    cbFormaEmissao: TComboBox;
    cbxAtualizarXML: TCheckBox;
    cbxExibirErroSchema: TCheckBox;
    edtFormatoAlerta: TEdit;
    cbModeloDF: TComboBox;
    cbxRetirarAcentos: TCheckBox;
    cbVersaoDF: TComboBox;
    edtIdToken: TEdit;
    edtToken: TEdit;
    edtPathSchemas: TEdit;
    TabSheet7: TTabSheet;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    lTimeOut: TLabel;
    lSSLLib1: TLabel;
    cbxVisualizar: TCheckBox;
    cbUF: TComboBox;
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
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edtEmitCNPJ: TEdit;
    edtEmitIE: TEdit;
    edtEmitRazao: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitCEP: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitComp: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCodCidade: TEdit;
    edtEmitCidade: TEdit;
    edtEmitUF: TEdit;
    TabSheet13: TTabSheet;
    sbPathNFe: TSpeedButton;
    Label35: TLabel;
    Label40: TLabel;
    sbPathInu: TSpeedButton;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathNFe: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathNFe: TEdit;
    edtPathInu: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDanfe: TRadioGroup;
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
    pgcBotoes: TPageControl;
    tsEnvios: TTabSheet;
    tsConsultas: TTabSheet;
    tsEventos: TTabSheet;
    tsInutilizacao: TTabSheet;
    btnCriarEnviar: TButton;
    btnConsultar: TButton;
    btnConsultarChave: TButton;
    btnConsCad: TButton;
    btnConsultarRecibo: TButton;
    btnInutilizar: TButton;
    btnInutilizarImprimir: TButton;
    btnValidarRegrasNegocio: TButton;
    btnGerarTXT: TButton;
    btnGerarXML: TButton;
    btnImportarXML: TButton;
    btnGerarPDF: TButton;
    btnValidarXML: TButton;
    btnImprimir: TButton;
    btnEnviarEmail: TButton;
    btnAdicionarProtocolo: TButton;
    btnCarregarXMLEnviar: TButton;
    btnValidarAssinatura: TButton;
    btnCancelarXML: TButton;
    btnCancelarChave: TButton;
    btnCartadeCorrecao: TButton;
    btnImprimirEvento: TButton;
    btnEnviarEventoEmail: TButton;
    tsDistribuicao: TTabSheet;
    btnManifDestConfirmacao: TButton;
    btnDistrDFePorUltNSU: TButton;
    pgRespostas: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
    WBResposta: TWebBrowser;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    trvwDocumento: TTreeView;
    TabSheet10: TTabSheet;
    memoRespWS: TMemo;
    Dados: TTabSheet;
    MemoDados: TMemo;
    ACBrNFe1: TACBrNFe;
    ACBrNFeDANFeRL1: TACBrNFeDANFeRL;
    ACBrNFeDANFCeFortes1: TACBrNFeDANFCeFortes;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrMail1: TACBrMail;
    OpenDialog1: TOpenDialog;
    gbEscPos: TGroupBox;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    btSerial: TBitBtn;
    cbxModeloPosPrinter: TComboBox;
    cbxPorta: TComboBox;
    cbxPagCodigo: TComboBox;
    seColunas: TSpinEdit;
    seEspLinhas: TSpinEdit;
    seLinhasPular: TSpinEdit;
    cbCortarPapel: TCheckBox;
    btnImprimirDANFCE: TButton;
    btnImprimirDANFCEOffline: TButton;
    rgDANFCE: TRadioGroup;
    btnStatusServ: TButton;
    ACBrIntegrador1: TACBrIntegrador;
    btVersao: TButton;
    ACBrNFeDANFCeFortesA41: TACBrNFeDANFCeFortesA4;
    Label51: TLabel;
    edtURLPFX: TEdit;
    Label52: TLabel;
    cbTipoEmpresa: TComboBox;
    btnAtorInterNFeTransp: TButton;
    btnDistrDFePorNSU: TButton;
    btnDistrDFePorChave: TButton;
    btnManifDestDesconnhecimento: TButton;
    Label39: TLabel;
    sbPathPDF: TSpeedButton;
    edtPathPDF: TEdit;
    btnManifDestCiencia: TButton;
    btnManifDestOperNaoRealizada: TButton;
    btnEventoEPEC: TButton;
    btnAdministrarCSC: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathNFeClick(Sender: TObject);
    procedure sbPathInuClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
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
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure sbtnLogoMarcaClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure ACBrNFe1StatusChange(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure btnStatusServClick(Sender: TObject);
    procedure btnGerarXMLClick(Sender: TObject);
    procedure btnGerarTXTClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnCarregarXMLEnviarClick(Sender: TObject);
    procedure btnImportarXMLClick(Sender: TObject);
    procedure btnValidarRegrasNegocioClick(Sender: TObject);
    procedure btnValidarXMLClick(Sender: TObject);
    procedure btnValidarAssinaturaClick(Sender: TObject);
    procedure btnAdicionarProtocoloClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnGerarPDFClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure btnConsultarReciboClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarChaveClick(Sender: TObject);
    procedure btnConsCadClick(Sender: TObject);
    procedure btnCancelarXMLClick(Sender: TObject);
    procedure btnCancelarChaveClick(Sender: TObject);
    procedure btnCartadeCorrecaoClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnEnviarEventoEmailClick(Sender: TObject);
    procedure btnInutilizarClick(Sender: TObject);
    procedure btnInutilizarImprimirClick(Sender: TObject);
    procedure btnDistrDFePorUltNSUClick(Sender: TObject);
    procedure btnManifDestConfirmacaoClick(Sender: TObject);
    procedure ACBrNFe1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure btSerialClick(Sender: TObject);
    procedure btnImprimirDANFCEClick(Sender: TObject);
    procedure btnImprimirDANFCEOfflineClick(Sender: TObject);
    procedure btVersaoClick(Sender: TObject);
    procedure btnAtorInterNFeTranspClick(Sender: TObject);
    procedure btnDistrDFePorNSUClick(Sender: TObject);
    procedure btnDistrDFePorChaveClick(Sender: TObject);
    procedure sbPathPDFClick(Sender: TObject);
    procedure btnEventoEPECClick(Sender: TObject);
    procedure btnAdministrarCSCClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    procedure AlimentarNFe(NumDFe: String);
    procedure AlimentarNFCe(NumDFe: String);
    Procedure AlimentarComponente(NumDFe: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
    procedure PrepararImpressao;
  public
    { Public declarations }
  end;

var
  frmACBrNFe: TfrmACBrNFe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.DateTime, ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  pcnAuxiliar, pcnNFe, pcnConversao, pcnConversaoNFe, pcnNFeRTXT, pcnRetConsReciDFe,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  ACBrNFeNotasFiscais, ACBrNFeConfiguracoes,
  Frm_Status, Frm_SelecionarCertificado, Frm_ConfiguraSerial;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrNFe }

procedure TfrmACBrNFe.ACBrNFe1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmACBrNFe.ACBrNFe1StatusChange(Sender: TObject);
begin
  case ACBrNFe1.Status of
    stIdle:
      begin
        if ( frmStatus <> nil ) then
          frmStatus.Hide;
      end;

    stNFeStatusServico:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFeRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados da NFe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNfeRetRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Recebendo dados da NFe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNfeConsulta:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando NFe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNfeCancelamento:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando cancelamento de NFe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNfeInutilizacao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando pedido de Inutilização...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFeRecibo:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando Recibo de Lote...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFeCadastro:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando Cadastro...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFeEmail:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Email...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFeCCe:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Carta de Correção...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stNFeEvento:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Evento...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;
  end;

  Application.ProcessMessages;
end;

procedure TfrmACBrNFe.AlimentarComponente(NumDFe: String);
begin
  ACBrNFe1.NotasFiscais.Clear;

  if ACBrNFe1.Configuracoes.Geral.ModeloDF = moNFe then
    AlimentarNFe(NumDFe)
  else
    AlimentarNFCe(NumDFe);
end;

procedure TfrmACBrNFe.AlimentarNFCe(NumDFe: String);
var
  Ok: Boolean;
  BaseCalculo,
  ValorICMS: Double;
begin
  with ACBrNFe1.NotasFiscais.Add.NFe do
  begin
    Ide.natOp     := 'VENDA';
    Ide.indPag    := ipVista;
    Ide.modelo    := 65;
    Ide.serie     := 1;
    Ide.nNF       := StrToInt(NumDFe);
    Ide.cNF       := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi      := now;
    Ide.dSaiEnt   := now;
    Ide.hSaiEnt   := now;
    Ide.tpNF      := tnSaida;
    Ide.tpEmis    := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);;
    Ide.tpAmb     := taHomologacao;  //Lembre-se de trocar esta variavel quando for para ambiente de producao
    Ide.cUF       := UFtoCUF(edtEmitUF.Text);
    Ide.cMunFG    := StrToInt(edtEmitCodCidade.Text);
    Ide.finNFe    := fnNormal;
    Ide.tpImp     := tiNFCe;
    Ide.indFinal  := cfConsumidorFinal;
    {
      valores aceitos pelo campo:
      pcNao, pcPresencial, pcInternet, pcTeleatendimento, pcEntregaDomicilio,
      pcPresencialForaEstabelecimento, pcOutros
    }
    Ide.indPres   := pcPresencial;

    {
      abaixo o campo incluido no layout a partir da NT 2020/006
    }
    {
      valores aceitos pelo campo:
      iiSemOperacao, iiOperacaoSemIntermediador, iiOperacaoComIntermediador
    }
    // Indicador de intermediador/marketplace
    Ide.indIntermed := iiSemOperacao;

//     Ide.dhCont := date;
//     Ide.xJust  := 'Justificativa Contingencia';

    Emit.CNPJCPF           := edtEmitCNPJ.Text;
    Emit.IE                := edtEmitIE.Text;
    Emit.xNome             := edtEmitRazao.Text;
    Emit.xFant             := edtEmitFantasia.Text;

    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.EnderEmit.CEP     := StrToInt(edtEmitCEP.Text);
    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.EnderEmit.xMun    := edtEmitCidade.Text;
    Emit.EnderEmit.UF      := edtEmitUF.Text;
    Emit.enderEmit.cPais   := 1058;
    Emit.enderEmit.xPais   := 'BRASIL';

    Emit.IEST := '';
    // esta sendo somando 1 uma vez que o ItemIndex inicia do zero e devemos
    // passar os valores 1, 2 ou 3
    // (1-crtSimplesNacional, 2-crtSimplesExcessoReceita, 3-crtRegimeNormal)
    Emit.CRT  := StrToCRT(Ok, IntToStr(cbTipoEmpresa.ItemIndex + 1));

    // Na NFC-e o Destinatário é opcional
    {
    Dest.CNPJCPF           := 'informar o CPF do destinatário';
    Dest.ISUF              := '';
    Dest.xNome             := 'nome do destinatário';

    Dest.indIEDest         := inNaoContribuinte;

    Dest.EnderDest.Fone    := '1533243333';
    Dest.EnderDest.CEP     := 18270170;
    Dest.EnderDest.xLgr    := 'Rua Coronel Aureliano de Camargo';
    Dest.EnderDest.nro     := '973';
    Dest.EnderDest.xCpl    := '';
    Dest.EnderDest.xBairro := 'Centro';
    Dest.EnderDest.cMun    := 3554003;
    Dest.EnderDest.xMun    := 'Tatuí';
    Dest.EnderDest.UF      := 'SP';
    Dest.EnderDest.cPais   := 1058;
    Dest.EnderDest.xPais   := 'BRASIL';
    }

//Use os campos abaixo para informar o endereço de retirada quando for diferente do Remetente/Destinatário
    Retirada.CNPJCPF := '';
    Retirada.xLgr    := '';
    Retirada.nro     := '';
    Retirada.xCpl    := '';
    Retirada.xBairro := '';
    Retirada.cMun    := 0;
    Retirada.xMun    := '';
    Retirada.UF      := '';

//Use os campos abaixo para informar o endereço de entrega quando for diferente do Remetente/Destinatário
    Entrega.CNPJCPF := '';
    Entrega.xLgr    := '';
    Entrega.nro     := '';
    Entrega.xCpl    := '';
    Entrega.xBairro := '';
    Entrega.cMun    := 0;
    Entrega.xMun    := '';
    Entrega.UF      := '';

//Adicionando Produtos
    with Det.New do
    begin
      Prod.nItem    := 1; // Número sequencial, para cada item deve ser incrementado
      Prod.cProd    := '123456';
      Prod.cEAN     := '7896523206646';
      Prod.xProd    := 'Descrição do Produto';
      Prod.NCM      := '94051010'; // Tabela NCM disponível em  http://www.receita.fazenda.gov.br/Aliquotas/DownloadArqTIPI.htm
      Prod.EXTIPI   := '';
      Prod.CFOP     := '5101';
      Prod.uCom     := 'UN';
      Prod.qCom     := 1;
      Prod.vUnCom   := 100;
      Prod.vProd    := 100;

      Prod.cEANTrib  := '7896523206646';
      Prod.uTrib     := 'UN';
      Prod.qTrib     := 1;
      Prod.vUnTrib   := 100;

      Prod.vOutro    := 0;
      Prod.vFrete    := 0;
      Prod.vSeg      := 0;
      Prod.vDesc     := 0;

      Prod.CEST := '1111111';

//         infAdProd      := 'Informação Adicional do Produto';

      {
        abaixo os campos incluidos no layout a partir da NT 2020/005
      }
      // Opcional - Preencher com o Código de Barras próprio ou de terceiros que seja diferente do padrão GTIN
      // por exemplo: código de barras de catálogo, partnumber, etc
      Prod.cBarra := 'ABC123456';
      // Opcional - Preencher com o Código de Barras próprio ou de terceiros que seja diferente do padrão GTIN
      //  correspondente àquele da menor unidade comercializável identificado por Código de Barras
      // por exemplo: código de barras de catálogo, partnumber, etc
      Prod.cBarraTrib := 'ABC123456';

      // Declaração de Importação. Pode ser adicionada várias através do comando Prod.DI.New
      (*
      with Prod.DI.New do
      begin
        nDi         := '';
        dDi         := now;
        xLocDesemb  := '';
        UFDesemb    := '';
        dDesemb     := now;
        {
          tvMaritima, tvFluvial, tvLacustre, tvAerea, tvPostal, tvFerroviaria, tvRodoviaria,

          abaixo os novos valores incluidos a partir da NT 2020/005

          tvConduto, tvMeiosProprios, tvEntradaSaidaFicta, tvCourier, tvEmMaos, tvPorReboque
        }
        tpViaTransp := tvRodoviaria;
        vAFRMM := 0;
        {
          tiContaPropria, tiContaOrdem, tiEncomenda
        }
        tpIntermedio := tiContaPropria;
        CNPJ := '';
        UFTerceiro := '';
        cExportador := '';

        with adi.New do
        begin
          nAdicao     := 1;
          nSeqAdi     := 1;
          cFabricante := '';
          vDescDI     := 0;
          nDraw       := '';
        end;
      end;
      *)

      with Imposto do
      begin
        // lei da transparencia nos impostos
        vTotTrib := 0;

        with ICMS do
        begin
          // caso o CRT seja:
          // 1=Simples Nacional
          // Os valores aceitos para CSOSN são:
          // csosn101, csosn102, csosn103, csosn201, csosn202, csosn203,
          // csosn300, csosn400, csosn500,csosn900

          // 2=Simples Nacional, excesso sublimite de receita bruta;
          // ou 3=Regime Normal.
          // Os valores aceitos para CST são:
          // cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51,
          // cst60, cst70, cst80, cst81, cst90, cstPart10, cstPart90,
          // cstRep41, cstVazio, cstICMSOutraUF, cstICMSSN, cstRep60

          // (consulte o contador do seu cliente para saber qual deve ser utilizado)
          // Pode variar de um produto para outro.

          if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
            CST := cst00
          else
            CSOSN := csosn102;

          orig    := oeNacional;
          modBC   := dbiValorOperacao;

          if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
            BaseCalculo := 100
          else
            BaseCalculo := 0;

          vBC     := BaseCalculo;
          pICMS   := 18;

          ValorICMS := vBC * pICMS;

          vICMS   := ValorICMS;
          modBCST := dbisMargemValorAgregado;
          pMVAST  := 0;
          pRedBCST:= 0;
          vBCST   := 0;
          pICMSST := 0;
          vICMSST := 0;
          pRedBC  := 0;

          pCredSN := 5;
          vCredICMSSN := 50;
          vBCFCPST := 100;
          pFCPST := 2;
          vFCPST := 2;
          vBCSTRet := 0;
          pST := 0;
          vICMSSubstituto := 0;
          vICMSSTRet := 0;

          vBCFCPSTRet := 0;
          pFCPSTRet := 0;
          vFCPSTRet := 0;
          pRedBCEfet := 0;
          vBCEfet := 0;
          pICMSEfet := 0;
          vICMSEfet := 0;

          {
            abaixo os campos incluidos no layout a partir da NT 2020/005
          }
          // Informar apenas nos motivos de desoneração documentados abaixo
          vICMSSTDeson := 0;
          {
            o campo abaixo só aceita os valores:
            mdiProdutorAgropecuario, mdiOutros, mdiOrgaoFomento
            Campo será preenchido quando o campo anterior estiver preenchido.
          }
          motDesICMSST := mdiOutros;

          // Percentual do diferimento do ICMS relativo ao Fundo de Combate à Pobreza (FCP).
          // No caso de diferimento total, informar o percentual de diferimento "100"
          pFCPDif := 0;
          // Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) diferido
          vFCPDif := 0;
          // Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) realmente devido.
          vFCPEfet := 0;

          // partilha do ICMS e fundo de probreza
          with ICMSUFDest do
          begin
            vBCUFDest      := 0.00;
            pFCPUFDest     := 0.00;
            pICMSUFDest    := 0.00;
            pICMSInter     := 0.00;
            pICMSInterPart := 0.00;
            vFCPUFDest     := 0.00;
            vICMSUFDest    := 0.00;
            vICMSUFRemet   := 0.00;
          end;
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
          {
            abaixo o campo incluido no layout a partir da NT 2020/005
          }
          {
            valores aceitos pelo campo:
            ispNenhum, ispPISSTNaoCompoe, ispPISSTCompoe
          }
          // Indica se o valor do PISST compõe o valor total da NF-e
          IndSomaPISST :=  ispNenhum;
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
          {
            abaixo o campo incluido no layout a partir da NT 2020/005
          }
          {
            valores aceitos pelo campo:
            iscNenhum, iscCOFINSSTNaoCompoe, iscCOFINSSTCompoe
          }
          // Indica se o valor da COFINS ST compõe o valor total da NF-e
          indSomaCOFINSST :=  iscNenhum;
        end;
      end;
    end;

    Total.ICMSTot.vBC     := BaseCalculo;
    Total.ICMSTot.vICMS   := ValorICMS;
    Total.ICMSTot.vBCST   := 0;
    Total.ICMSTot.vST     := 0;
    Total.ICMSTot.vProd   := 100;
    Total.ICMSTot.vFrete  := 0;
    Total.ICMSTot.vSeg    := 0;
    Total.ICMSTot.vDesc   := 0;
    Total.ICMSTot.vII     := 0;
    Total.ICMSTot.vIPI    := 0;
    Total.ICMSTot.vPIS    := 0;
    Total.ICMSTot.vCOFINS := 0;
    Total.ICMSTot.vOutro  := 0;
    Total.ICMSTot.vNF     := 100;

    // partilha do icms e fundo de probreza
    Total.ICMSTot.vFCPUFDest   := 0.00;
    Total.ICMSTot.vICMSUFDest  := 0.00;
    Total.ICMSTot.vICMSUFRemet := 0.00;

    Total.ISSQNtot.vServ   := 0;
    Total.ISSQNTot.vBC     := 0;
    Total.ISSQNTot.vISS    := 0;
    Total.ISSQNTot.vPIS    := 0;
    Total.ISSQNTot.vCOFINS := 0;

    Total.retTrib.vRetPIS    := 0;
    Total.retTrib.vRetCOFINS := 0;
    Total.retTrib.vRetCSLL   := 0;
    Total.retTrib.vBCIRRF    := 0;
    Total.retTrib.vIRRF      := 0;
    Total.retTrib.vBCRetPrev := 0;
    Total.retTrib.vRetPrev   := 0;

    Transp.modFrete := mfSemFrete; // NFC-e não pode ter FRETE

    with pag.New do
    begin
      tPag := fpDinheiro;
      {
        abaixo o campo incluido no layout a partir da NT 2020/006
      }
      {
        se tPag for fpOutro devemos incluir o campo xPag
      xPag := 'Caderneta';
      }
      vPag := 100;
    end;

    // O grupo infIntermed só deve ser gerado nos casos de operação não presencial
    // pela internet em site de terceiros (Intermediadores).
//    infIntermed.CNPJ := '';
//    infIntermed.idCadIntTran := '';

    InfAdic.infCpl     :=  '';
    InfAdic.infAdFisco :=  '';

    with InfAdic.obsCont.New do
    begin
      xCampo := 'ObsCont';
      xTexto := 'Texto';
    end;

    with InfAdic.obsFisco.New do
    begin
      xCampo := 'ObsFisco';
      xTexto := 'Texto';
    end;

    {
      abaixo o campo incluido no layout a partir da NT 2020/006
    }
    // CNPJ do Intermediador da Transação (agenciador, plataforma de delivery,
    // marketplace e similar) de serviços e de negócios.
    infIntermed.CNPJ := '';
    // Nome do usuário ou identificação do perfil do vendedor no site do intermediador
    // (agenciador, plataforma de delivery, marketplace e similar) de serviços e de
    // negócios.
    infIntermed.idCadIntTran := '';
  end;

  ACBrNFe1.NotasFiscais.GerarNFe;
end;

procedure TfrmACBrNFe.AlimentarNFe(NumDFe: String);
var
  Ok: Boolean;
  NotaF: NotaFiscal;
  Produto: TDetCollectionItem;
//    Servico: TDetCollectionItem;
  Volume: TVolCollectionItem;
  Duplicata: TDupCollectionItem;
  ObsComplementar: TobsContCollectionItem;
  ObsFisco: TobsFiscoCollectionItem;
//    Referenciada: TNFrefCollectionItem;
//    DI: TDICollectionItem;
//    Adicao: TAdiCollectionItem;
//    Rastro: TrastroCollectionItem;
//    Medicamento: TMedCollectionItem;
//    Arma: TArmaCollectionItem;
//    Reboque: TreboqueCollectionItem;
//    Lacre: TLacresCollectionItem;
//    ProcReferenciado: TprocRefCollectionItem;
  InfoPgto: TpagCollectionItem;
begin
  NotaF := ACBrNFe1.NotasFiscais.Add;
  NotaF.NFe.Ide.natOp     := 'VENDA PRODUCAO DO ESTAB.';
  NotaF.NFe.Ide.indPag    := ipVista;
  NotaF.NFe.Ide.modelo    := 55;
  NotaF.NFe.Ide.serie     := 1;
  NotaF.NFe.Ide.nNF       := StrToInt(NumDFe);
  NotaF.NFe.Ide.cNF       := GerarCodigoDFe(NotaF.NFe.Ide.nNF);
  NotaF.NFe.Ide.dEmi      := Date;
  NotaF.NFe.Ide.dSaiEnt   := Date;
  NotaF.NFe.Ide.hSaiEnt   := Now;
  NotaF.NFe.Ide.tpNF      := tnSaida;
  NotaF.NFe.Ide.tpEmis    := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);;
  NotaF.NFe.Ide.tpAmb     := taHomologacao;  //Lembre-se de trocar esta variável quando for para ambiente de produção
  NotaF.NFe.Ide.verProc   := '1.0.0.0'; //Versão do seu sistema
  NotaF.NFe.Ide.cUF       := UFtoCUF(edtEmitUF.Text);
  NotaF.NFe.Ide.cMunFG    := StrToInt(edtEmitCodCidade.Text);
  NotaF.NFe.Ide.finNFe    := fnNormal;
  if  Assigned( ACBrNFe1.DANFE ) then
    NotaF.NFe.Ide.tpImp     := ACBrNFe1.DANFE.TipoDANFE;

  // Valores aceitos:
  // iiSemOperacao, iiOperacaoSemIntermediador, iiOperacaoComIntermediador
//  NotaF.NFe.Ide.indIntermed := iiSemOperacao;

//  NotaF.NFe.Ide.dhCont := date;
//  NotaF.NFe.Ide.xJust  := 'Justificativa Contingencia';

  {
    abaixo o campo incluido no layout a partir da NT 2020/006
  }
  {
    valores aceitos pelo campo:
    iiSemOperacao, iiOperacaoSemIntermediador, iiOperacaoComIntermediador
  }
  // Indicador de intermediador/marketplace
  NotaF.NFe.Ide.indIntermed := iiSemOperacao;


  //Para NFe referenciada use os campos abaixo
  (*
  Referenciada := NotaF.NFe.Ide.NFref.Add;
  Referenciada.refNFe       := ''; //NFe Eletronica

  Referenciada.RefNF.cUF    := 0;  // |
  Referenciada.RefNF.AAMM   := ''; // |
  Referenciada.RefNF.CNPJ   := ''; // |
  Referenciada.RefNF.modelo := 1;  // |- NFe Modelo 1/1A
  Referenciada.RefNF.serie  := 1;  // |
  Referenciada.RefNF.nNF    := 0;  // |

  Referenciada.RefNFP.cUF     := 0;  // |
  Referenciada.RefNFP.AAMM    := ''; // |
  Referenciada.RefNFP.CNPJCPF := ''; // |
  Referenciada.RefNFP.IE      := ''; // |- NF produtor Rural
  Referenciada.RefNFP.modelo  := ''; // |
  Referenciada.RefNFP.serie   := 1;  // |
  Referenciada.RefNFP.nNF     := 0;  // |

  Referenciada.RefECF.modelo  := ECFModRef2B; // |
  Referenciada.RefECF.nECF    := '';          // |- Cupom Fiscal
  Referenciada.RefECF.nCOO    := '';          // |
  *)
  NotaF.NFe.Emit.CNPJCPF           := edtEmitCNPJ.Text;
  NotaF.NFe.Emit.IE                := edtEmitIE.Text;
  NotaF.NFe.Emit.xNome             := edtEmitRazao.Text;
  NotaF.NFe.Emit.xFant             := edtEmitFantasia.Text;

  NotaF.NFe.Emit.EnderEmit.fone    := edtEmitFone.Text;
  NotaF.NFe.Emit.EnderEmit.CEP     := StrToInt(edtEmitCEP.Text);
  NotaF.NFe.Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
  NotaF.NFe.Emit.EnderEmit.nro     := edtEmitNumero.Text;
  NotaF.NFe.Emit.EnderEmit.xCpl    := edtEmitComp.Text;
  NotaF.NFe.Emit.EnderEmit.xBairro := edtEmitBairro.Text;
  NotaF.NFe.Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
  NotaF.NFe.Emit.EnderEmit.xMun    := edtEmitCidade.Text;
  NotaF.NFe.Emit.EnderEmit.UF      := edtEmitUF.Text;
  NotaF.NFe.Emit.enderEmit.cPais   := 1058;
  NotaF.NFe.Emit.enderEmit.xPais   := 'BRASIL';

  NotaF.NFe.Emit.IEST              := '';
  NotaF.NFe.Emit.IM                := '2648800'; // Preencher no caso de existir serviços na nota
  NotaF.NFe.Emit.CNAE              := '6201500'; // Verifique na cidade do emissor da NFe se é permitido
                                                 // a inclusão de serviços na NFe

    // esta sendo somando 1 uma vez que o ItemIndex inicia do zero e devemos
    // passar os valores 1, 2 ou 3
    // (1-crtSimplesNacional, 2-crtSimplesExcessoReceita, 3-crtRegimeNormal)
  NotaF.NFe.Emit.CRT  := StrToCRT(Ok, IntToStr(cbTipoEmpresa.ItemIndex + 1));

//Para NFe Avulsa preencha os campos abaixo

  NotaF.NFe.Avulsa.CNPJ    := '';
  NotaF.NFe.Avulsa.xOrgao  := '';
  NotaF.NFe.Avulsa.matr    := '';
  NotaF.NFe.Avulsa.xAgente := '';
  NotaF.NFe.Avulsa.fone    := '';
  NotaF.NFe.Avulsa.UF      := '';
  NotaF.NFe.Avulsa.nDAR    := '';
  NotaF.NFe.Avulsa.dEmi    := now;
  NotaF.NFe.Avulsa.vDAR    := 0;
  NotaF.NFe.Avulsa.repEmi  := '';
  NotaF.NFe.Avulsa.dPag    := now;


  NotaF.NFe.Dest.CNPJCPF           := '05481336000137';
  NotaF.NFe.Dest.IE                := '687138770110';
  NotaF.NFe.Dest.ISUF              := '';
  NotaF.NFe.Dest.xNome             := 'D.J. COM. E LOCAÇÃO DE SOFTWARES LTDA - ME';

  NotaF.NFe.Dest.EnderDest.Fone    := '1532599600';
  NotaF.NFe.Dest.EnderDest.CEP     := 18270170;
  NotaF.NFe.Dest.EnderDest.xLgr    := 'Rua Coronel Aureliano de Camargo';
  NotaF.NFe.Dest.EnderDest.nro     := '973';
  NotaF.NFe.Dest.EnderDest.xCpl    := '';
  NotaF.NFe.Dest.EnderDest.xBairro := 'Centro';
  NotaF.NFe.Dest.EnderDest.cMun    := 3554003;
  NotaF.NFe.Dest.EnderDest.xMun    := 'Tatui';
  NotaF.NFe.Dest.EnderDest.UF      := 'SP';
  NotaF.NFe.Dest.EnderDest.cPais   := 1058;
  NotaF.NFe.Dest.EnderDest.xPais   := 'BRASIL';

//Use os campos abaixo para informar o endereço de retirada quando for diferente do Remetente/Destinatário

  NotaF.NFe.Retirada.CNPJCPF := '';
  NotaF.NFe.Retirada.xLgr    := '';
  NotaF.NFe.Retirada.nro     := '';
  NotaF.NFe.Retirada.xCpl    := '';
  NotaF.NFe.Retirada.xBairro := '';
  NotaF.NFe.Retirada.cMun    := 0;
  NotaF.NFe.Retirada.xMun    := '';
  NotaF.NFe.Retirada.UF      := '';

//Use os campos abaixo para informar o endereço de entrega quando for diferente do Remetente/Destinatário

  NotaF.NFe.Entrega.CNPJCPF := '';
  NotaF.NFe.Entrega.xLgr    := '';
  NotaF.NFe.Entrega.nro     := '';
  NotaF.NFe.Entrega.xCpl    := '';
  NotaF.NFe.Entrega.xBairro := '';
  NotaF.NFe.Entrega.cMun    := 0;
  NotaF.NFe.Entrega.xMun    := '';
  NotaF.NFe.Entrega.UF      := '';

//Adicionando Produtos
  Produto := NotaF.NFe.Det.New;
  Produto.Prod.nItem    := 1; // Número sequencial, para cada item deve ser incrementado
  Produto.Prod.cProd    := '123456';
  Produto.Prod.cEAN     := '7896523206646';
  Produto.Prod.xProd    := 'Camisa Polo ACBr';
  Produto.Prod.NCM      := '61051000';
  Produto.Prod.EXTIPI   := '';
  Produto.Prod.CFOP     := '5101';
  Produto.Prod.uCom     := 'UN';
  Produto.Prod.qCom     := 1;
  Produto.Prod.vUnCom   := 100;
  Produto.Prod.vProd    := 100;

  Produto.Prod.cEANTrib  := '7896523206646';
  Produto.Prod.uTrib     := 'UN';
  Produto.Prod.qTrib     := 1;
  Produto.Prod.vUnTrib   := 100;

  Produto.Prod.vOutro    := 0;
  Produto.Prod.vFrete    := 0;
  Produto.Prod.vSeg      := 0;
  Produto.Prod.vDesc     := 0;

  //Produto.Prod.CEST := '1111111';

  Produto.infAdProd := 'Informacao Adicional do Produto';

  {
    abaixo os campos incluidos no layout a partir da NT 2020/005
  }
  // Opcional - Preencher com o Código de Barras próprio ou de terceiros que seja diferente do padrão GTIN
  // por exemplo: código de barras de catálogo, partnumber, etc
  Produto.Prod.cBarra := 'ABC123456';
  // Opcional - Preencher com o Código de Barras próprio ou de terceiros que seja diferente do padrão GTIN
  //  correspondente àquele da menor unidade comercializável identificado por Código de Barras
  // por exemplo: código de barras de catálogo, partnumber, etc
  Produto.Prod.cBarraTrib := 'ABC123456';

  // Declaração de Importação. Pode ser adicionada várias através do comando Prod.DI.Add
  (*
  DI := Produto.Prod.DI.Add;
  DI.nDi         := '';
  DI.dDi         := now;
  DI.xLocDesemb  := '';
  DI.UFDesemb    := '';
  DI.dDesemb     := now;
  {
    tvMaritima, tvFluvial, tvLacustre, tvAerea, tvPostal, tvFerroviaria, tvRodoviaria,

    abaixo os novos valores incluidos a partir da NT 2020/005

    tvConduto, tvMeiosProprios, tvEntradaSaidaFicta, tvCourier, tvEmMaos, tvPorReboque
  }
  DI.tpViaTransp := tvRodoviaria;
  DI.vAFRMM := 0;
  {
    tiContaPropria, tiContaOrdem, tiEncomenda
  }
  DI.tpIntermedio := tiContaPropria;
  DI.CNPJ := '';
  DI.UFTerceiro := '';
  DI.cExportador := '';

  Adicao := DI.adi.Add;
  Adicao.nAdicao     := 1;
  Adicao.nSeqAdi     := 1;
  Adicao.cFabricante := '';
  Adicao.vDescDI     := 0;
  Adicao.nDraw       := '';
  *)

//Campos para venda de veículos novos

  Produto.Prod.veicProd.tpOP    := toVendaConcessionaria;
  Produto.Prod.veicProd.chassi  := '';
  Produto.Prod.veicProd.cCor    := '';
  Produto.Prod.veicProd.xCor    := '';
  Produto.Prod.veicProd.pot     := '';
  Produto.Prod.veicProd.Cilin   := '';
  Produto.Prod.veicProd.pesoL   := '';
  Produto.Prod.veicProd.pesoB   := '';
  Produto.Prod.veicProd.nSerie  := '';
  Produto.Prod.veicProd.tpComb  := '';
  Produto.Prod.veicProd.nMotor  := '';
  Produto.Prod.veicProd.CMT     := '';
  Produto.Prod.veicProd.dist    := '';
  Produto.Prod.veicProd.anoMod  := 0;
  Produto.Prod.veicProd.anoFab  := 0;
  Produto.Prod.veicProd.tpPint  := '';
  Produto.Prod.veicProd.tpVeic  := 0;
  Produto.Prod.veicProd.espVeic := 0;
  Produto.Prod.veicProd.VIN     := '';
  Produto.Prod.veicProd.condVeic := cvAcabado;
  Produto.Prod.veicProd.cMod    := '';

// Campos de Rastreabilidade do produto
  {
  O grupo <rastro> permiti a rastreabilidade de qualquer produto sujeito a
  regulações sanitárias, casos de recolhimento/recall, além de defensivos agrícolas,
  produtos veterinários, odontológicos, medicamentos, bebidas, águas envasadas,
  embalagens, etc., a partir da indicação de informações de número de lote,
  data de fabricação/produção, data de validade, etc.
  Obrigatório o preenchimento deste grupo no caso de medicamentos e
  produtos farmacêuticos.
  }

  // Ocorrências: 0 - 500
  (*
  Rastro := Produto.Prod.rastro.Add;

  Rastro.nLote  := '17H8F5';
  Rastro.qLote  := 1;
  Rastro.dFab   := StrToDate('01/08/2017');
  Rastro.dVal   := StrToDate('01/08/2019');
  Rastro.cAgreg := ''; // Código de Agregação (opcional) de 1 até 20 dígitos
  *)

//Campos específicos para venda de medicamentos

  // Ocorrências: 1 - 500 ==> 1 - 1 (4.00)
  (*
  Medicamento := Produto.Prod.med.Add;

  Medicamento.cProdANVISA := '1256802470029';
  Medicamento.vPMC        := 100.00; // Preço máximo consumidor
  *)

//Campos específicos para venda de armamento
  (*
  Arma := Produto.Prod.arma.Add;
  Arma.nSerie := 0;
  Arma.tpArma := taUsoPermitido;
  Arma.nCano  := 0;
  Arma.descr  := '';
  *)

//Campos específicos para venda de combustível(distribuidoras)

  Produto.Prod.comb.cProdANP := 0;
  Produto.Prod.comb.CODIF    := '';
  Produto.Prod.comb.qTemp    := 0;
  Produto.Prod.comb.UFcons   := '';

  Produto.Prod.comb.CIDE.qBCprod   := 0;
  Produto.Prod.comb.CIDE.vAliqProd := 0;
  Produto.Prod.comb.CIDE.vCIDE     := 0;

  Produto.Prod.comb.ICMS.vBCICMS   := 0;
  Produto.Prod.comb.ICMS.vICMS     := 0;
  Produto.Prod.comb.ICMS.vBCICMSST := 0;
  Produto.Prod.comb.ICMS.vICMSST   := 0;

  Produto.Prod.comb.ICMSInter.vBCICMSSTDest := 0;
  Produto.Prod.comb.ICMSInter.vICMSSTDest   := 0;

  Produto.Prod.comb.ICMSCons.vBCICMSSTCons := 0;
  Produto.Prod.comb.ICMSCons.vICMSSTCons   := 0;
  Produto.Prod.comb.ICMSCons.UFcons        := '';


  with Produto.Imposto do
  begin
    // lei da transparencia nos impostos
    vTotTrib := 0;

    with ICMS do
    begin
      // caso o CRT seja:
      // 1=Simples Nacional
      // Os valores aceitos para CSOSN são:
      // csosn101, csosn102, csosn103, csosn201, csosn202, csosn203,
      // csosn300, csosn400, csosn500,csosn900

      // 2=Simples Nacional, excesso sublimite de receita bruta;
      // ou 3=Regime Normal.
      // Os valores aceitos para CST são:
      // cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51,
      // cst60, cst70, cst80, cst81, cst90, cstPart10, cstPart90,
      // cstRep41, cstVazio, cstICMSOutraUF, cstICMSSN, cstRep60

      // (consulte o contador do seu cliente para saber qual deve ser utilizado)
      // Pode variar de um produto para outro.

      orig := oeNacional;

      //Grupo de Tributação do ICMS Monofásico sobre combustíveis.
      (*
      CST       := cst02;
      qBCMono   := 100;
      adRemICMS := 10;
      vICMSMono := 10;
      *)
      //Grupo de Tributação do ICMS Monofásico sobre combustíveis.
      (*
      CST          := cst15;
      qBCMono      := 100;
      adRemICMS    := 10;
      vICMSMono    := 10;
      qBCMonoReten := 100;
      adRemICMSReten := 10;
      vICMSMonoReten := 10;
      pRedAdRem      := 10;
      motRedAdRem    := TmotRedAdRem.motTranspColetivo;
      *)
      //Grupo de Tributação do ICMS Monofásico sobre combustíveis.
      (*
      CST           := cst53;
      qBCMono       := 100;
      adRemICMS     := 10;
      vICMSMonoOp   := 10;
      pDif          := 10;
      vICMSMonoDif  := 1;
      vICMSMono     := 10;
      *)
      //Grupo de Tributação do ICMS Monofásico sobre combustíveis.
      (*
      CST           := cst61;
      qBCMonoRet    := 100;
      adRemICMSRet  := 10;
      vICMSMonoRet  := 10;
      *)

      if NotaF.NFe.Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
      begin
        CST     := cst00;
        modBC   := dbiPrecoTabelado;
        vBC     := 100;
        pICMS   := 18;
        vICMS   := 18;
        modBCST := dbisMargemValorAgregado;
        pMVAST  := 0;
        pRedBCST:= 0;
        vBCST   := 0;
        pICMSST := 0;
        vICMSST := 0;
        pRedBC  := 0;
      end
      else
      begin
        CSOSN   := csosn101;
        modBC   := dbiValorOperacao;
        pCredSN := 5;
        vCredICMSSN := 100 * pCredSN / 100;;
        vBC     := 0;
        pICMS   := 0;
        vICMS   := 0;
        modBCST := dbisListaNeutra;
        pMVAST  := 0;
        pRedBCST:= 0;
        vBCST   := 0;
        pICMSST := 0;
        vICMSST := 0;
      end;

      vBCFCPST := 100;
      pFCPST := 2;
      vFCPST := 2;
      vBCSTRet := 0;
      pST := 0;
      vICMSSubstituto := 0;
      vICMSSTRet := 0;
      vBCFCPSTRet := 0;
      pFCPSTRet := 0;
      vFCPSTRet := 0;
      pRedBCEfet := 0;
      vBCEfet := 0;
      pICMSEfet := 0;
      vICMSEfet := 0;

      {
        abaixo os campos incluidos no layout a partir da NT 2020/005
      }
      // Informar apenas nos motivos de desoneração documentados abaixo
      vICMSSTDeson := 0;
      {
        o campo abaixo só aceita os valores:
        mdiProdutorAgropecuario, mdiOutros, mdiOrgaoFomento
        Campo será preenchido quando o campo anterior estiver preenchido.
      }
      motDesICMSST := mdiOutros;

      // Percentual do diferimento do ICMS relativo ao Fundo de Combate à Pobreza (FCP).
      // No caso de diferimento total, informar o percentual de diferimento "100"
      pFCPDif := 0;
      // Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) diferido
      vFCPDif := 0;
      // Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) realmente devido.
      vFCPEfet := 0;
    end;

    with ICMSUFDest do
    begin
      // partilha do ICMS e fundo de probreza
      vBCUFDest      := 0.00;
      pFCPUFDest     := 0.00;
      pICMSUFDest    := 0.00;
      pICMSInter     := 0.00;
      pICMSInterPart := 0.00;
      vFCPUFDest     := 0.00;
      vICMSUFDest    := 0.00;
      vICMSUFRemet   := 0.00;
    end;

    (*
    // IPI, se hpouver...
    with IPI do
    begin
      CST      := ipi99;
      clEnq    := '999';
      CNPJProd := '';
      cSelo    := '';
      qSelo    := 0;
      cEnq     := '';

      vBC    := 100;
      qUnid  := 0;
      vUnid  := 0;
      pIPI   := 5;
      vIPI   := 5;
    end;
    *)

    with II do
    begin
      II.vBc      := 0;
      II.vDespAdu := 0;
      II.vII      := 0;
      II.vIOF     := 0;
    end;

    with PIS do
    begin
      CST  := pis99;
      vBC  := 0;
      pPIS := 0;
      vPIS := 0;

      qBCProd   := 0;
      vAliqProd := 0;
      vPIS      := 0;
    end;

    with PISST do
    begin
      vBc       := 0;
      pPis      := 0;
      qBCProd   := 0;
      vAliqProd := 0;
      vPIS      := 0;
      {
        abaixo o campo incluido no layout a partir da NT 2020/005
      }
      {
        valores aceitos pelo campo:
        ispNenhum, ispPISSTNaoCompoe, ispPISSTCompoe
      }
      // Indica se o valor do PISST compõe o valor total da NF-e
      IndSomaPISST :=  ispNenhum;
    end;

    with COFINS do
    begin
      CST     := cof99;
      vBC     := 0;
      pCOFINS := 0;
      vCOFINS := 0;
      qBCProd   := 0;
      vAliqProd := 0;
    end;

    with COFINSST do
    begin
      vBC       := 0;
      pCOFINS   := 0;
      qBCProd   := 0;
      vAliqProd := 0;
      vCOFINS   := 0;
      {
        abaixo o campo incluido no layout a partir da NT 2020/005
      }
      {
        valores aceitos pelo campo:
        iscNenhum, iscCOFINSSTNaoCompoe, iscCOFINSSTCompoe
      }
      // Indica se o valor da COFINS ST compõe o valor total da NF-e
      indSomaCOFINSST :=  iscNenhum;
    end;
  end;



  //Adicionando Serviços
  (*
  Servico := NotaF.Nfe.Det.Add;
  Servico.Prod.nItem    := 1; // Número sequencial, para cada item deve ser incrementado
  Servico.Prod.cProd    := '123457';
  Servico.Prod.cEAN     := '';
  Servico.Prod.xProd    := 'Descrição do Serviço';
  Servico.Prod.NCM      := '99';
  Servico.Prod.EXTIPI   := '';
  Servico.Prod.CFOP     := '5933';
  Servico.Prod.uCom     := 'UN';
  Servico.Prod.qCom     := 1;
  Servico.Prod.vUnCom   := 100;
  Servico.Prod.vProd    := 100;

  Servico.Prod.cEANTrib  := '';
  Servico.Prod.uTrib     := 'UN';
  Servico.Prod.qTrib     := 1;
  Servico.Prod.vUnTrib   := 100;

  Servico.Prod.vFrete    := 0;
  Servico.Prod.vSeg      := 0;
  Servico.Prod.vDesc     := 0;

  Servico.infAdProd      := 'Informação Adicional do Serviço';

  //Grupo para serviços
  Servico.Imposto.ISSQN
  Servico.Imposto.cSitTrib  := ISSQNcSitTribNORMAL;
  Servico.Imposto.vBC       := 100;
  Servico.Imposto.vAliq     := 2;
  Servico.Imposto.vISSQN    := 2;
  Servico.Imposto.cMunFG    := 3554003;
  // Preencha este campo usando a tabela disponível
  // em http://www.planalto.gov.br/Ccivil_03/LEIS/LCP/Lcp116.htm
  Servico.Imposto.cListServ := '1402';

  NotaF.NFe.Total.ISSQNtot.vServ   := 100;
  NotaF.NFe.Total.ISSQNTot.vBC     := 100;
  NotaF.NFe.Total.ISSQNTot.vISS    := 2;
  NotaF.NFe.Total.ISSQNTot.vPIS    := 0;
  NotaF.NFe.Total.ISSQNTot.vCOFINS := 0;

*)

  if NotaF.NFe.Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
  begin
    NotaF.NFe.Total.ICMSTot.vBC := 100;
    NotaF.NFe.Total.ICMSTot.vICMS := 18;
  end
  else
  begin
    NotaF.NFe.Total.ICMSTot.vBC := 0;
    NotaF.NFe.Total.ICMSTot.vICMS := 0;
  end;

  NotaF.NFe.Total.ICMSTot.vBCST   := 0;
  NotaF.NFe.Total.ICMSTot.vST     := 0;
  NotaF.NFe.Total.ICMSTot.vProd   := 100;
  NotaF.NFe.Total.ICMSTot.vFrete  := 0;
  NotaF.NFe.Total.ICMSTot.vSeg    := 0;
  NotaF.NFe.Total.ICMSTot.vDesc   := 0;
  NotaF.NFe.Total.ICMSTot.vII     := 0;
  NotaF.NFe.Total.ICMSTot.vIPI    := 0;
  NotaF.NFe.Total.ICMSTot.vPIS    := 0;
  NotaF.NFe.Total.ICMSTot.vCOFINS := 0;
  NotaF.NFe.Total.ICMSTot.vOutro  := 0;
  NotaF.NFe.Total.ICMSTot.vNF     := 100;

  // lei da transparencia de impostos
  NotaF.NFe.Total.ICMSTot.vTotTrib := 0;

  // partilha do icms e fundo de probreza
  NotaF.NFe.Total.ICMSTot.vFCPUFDest   := 0.00;
  NotaF.NFe.Total.ICMSTot.vICMSUFDest  := 0.00;
  NotaF.NFe.Total.ICMSTot.vICMSUFRemet := 0.00;

  NotaF.NFe.Total.ICMSTot.vFCPST     := 0;
  NotaF.NFe.Total.ICMSTot.vFCPSTRet  := 0;

  NotaF.NFe.Total.retTrib.vRetPIS    := 0;
  NotaF.NFe.Total.retTrib.vRetCOFINS := 0;
  NotaF.NFe.Total.retTrib.vRetCSLL   := 0;
  NotaF.NFe.Total.retTrib.vBCIRRF    := 0;
  NotaF.NFe.Total.retTrib.vIRRF      := 0;
  NotaF.NFe.Total.retTrib.vBCRetPrev := 0;
  NotaF.NFe.Total.retTrib.vRetPrev   := 0;

  NotaF.NFe.Transp.modFrete := mfContaEmitente;
  NotaF.NFe.Transp.Transporta.CNPJCPF  := '';
  NotaF.NFe.Transp.Transporta.xNome    := '';
  NotaF.NFe.Transp.Transporta.IE       := '';
  NotaF.NFe.Transp.Transporta.xEnder   := '';
  NotaF.NFe.Transp.Transporta.xMun     := '';
  NotaF.NFe.Transp.Transporta.UF       := '';

  NotaF.NFe.Transp.retTransp.vServ    := 0;
  NotaF.NFe.Transp.retTransp.vBCRet   := 0;
  NotaF.NFe.Transp.retTransp.pICMSRet := 0;
  NotaF.NFe.Transp.retTransp.vICMSRet := 0;
  NotaF.NFe.Transp.retTransp.CFOP     := '';
  NotaF.NFe.Transp.retTransp.cMunFG   := 0;

  Volume := NotaF.NFe.Transp.Vol.New;
  Volume.qVol  := 1;
  Volume.esp   := 'Especie';
  Volume.marca := 'Marca';
  Volume.nVol  := 'Numero';
  Volume.pesoL := 100;
  Volume.pesoB := 110;

  //Lacres do volume. Pode ser adicionado vários
  (*
  Lacre := Volume.Lacres.Add;
  Lacre.nLacre := '';
  *)

  NotaF.NFe.Cobr.Fat.nFat  := '1001'; // 'Numero da Fatura'
  NotaF.NFe.Cobr.Fat.vOrig := 100;
  NotaF.NFe.Cobr.Fat.vDesc := 0;
  NotaF.NFe.Cobr.Fat.vLiq  := 100;

  Duplicata := NotaF.NFe.Cobr.Dup.New;
  Duplicata.nDup  := '001';
  Duplicata.dVenc := now+10;
  Duplicata.vDup  := 50;

  Duplicata := NotaF.NFe.Cobr.Dup.New;
  Duplicata.nDup  := '002';
  Duplicata.dVenc := now+20;
  Duplicata.vDup  := 50;

    // O grupo infIntermed só deve ser gerado nos casos de operação não presencial
    // pela internet em site de terceiros (Intermediadores).
//  NotaF.NFe.infIntermed.CNPJ := '';
//  NotaF.NFe.infIntermed.idCadIntTran := '';

  NotaF.NFe.InfAdic.infCpl     :=  '';
  NotaF.NFe.InfAdic.infAdFisco :=  '';

  ObsComplementar := NotaF.NFe.InfAdic.obsCont.New;
  ObsComplementar.xCampo := 'ObsCont';
  ObsComplementar.xTexto := 'Texto';

  ObsFisco := NotaF.NFe.InfAdic.obsFisco.New;
  ObsFisco.xCampo := 'ObsFisco';
  ObsFisco.xTexto := 'Texto';

//Processo referenciado
  (*
  ProcReferenciado := NotaF.Nfe.InfAdic.procRef.Add;
  ProcReferenciado.nProc := '';
  ProcReferenciado.indProc := ipSEFAZ;
  *)

  NotaF.NFe.exporta.UFembarq   := '';;
  NotaF.NFe.exporta.xLocEmbarq := '';

  NotaF.NFe.compra.xNEmp := '';
  NotaF.NFe.compra.xPed  := '';
  NotaF.NFe.compra.xCont := '';

// YA. Informações de pagamento

  InfoPgto := NotaF.NFe.pag.New;
  InfoPgto.indPag := ipVista;
  InfoPgto.tPag   := fpDinheiro;
  InfoPgto.vPag   := 100;

// Exemplo de pagamento integrado.

  InfoPgto := NotaF.NFe.pag.New;
  InfoPgto.indPag := ipVista;
  InfoPgto.tPag   := fpCartaoCredito;

  {
    abaixo o campo incluido no layout a partir da NT 2020/006
  }
  {
    se tPag for fpOutro devemos incluir o campo xPag
  InfoPgto.xPag := 'Caderneta';
  }
  InfoPgto.vPag   := 75;
  InfoPgto.tpIntegra := tiPagIntegrado;
  InfoPgto.CNPJ      := '05481336000137';
  InfoPgto.tBand     := bcVisa;
  InfoPgto.cAut      := '1234567890123456';

// YA09 Troco
// Regra opcional: Informar se valor dos pagamentos maior que valor da nota.
// Regra obrigatória: Se informado, Não pode diferir de "(+) vPag (id:YA03) (-) vNF (id:W16)"
//  NotaF.NFe.pag.vTroco := 75;

  {
    abaixo o campo incluido no layout a partir da NT 2020/006
  }
  // CNPJ do Intermediador da Transação (agenciador, plataforma de delivery,
  // marketplace e similar) de serviços e de negócios.
  NotaF.NFe.infIntermed.CNPJ := '';
  // Nome do usuário ou identificação do perfil do vendedor no site do intermediador
  // (agenciador, plataforma de delivery, marketplace e similar) de serviços e de
  // negócios.
  NotaF.NFe.infIntermed.idCadIntTran := '';

  ACBrNFe1.NotasFiscais.GerarNFe;
end;

procedure TfrmACBrNFe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrNFe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrNFe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrNFe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrNFe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrNFe.btnAdicionarProtocoloClick(Sender: TObject);
var
  NomeArq: String;
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFE (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ACBrNFe1.Consultar;

    ShowMessage(ACBrNFe1.WebServices.Consulta.Protocolo);

    MemoResp.Lines.Text := ACBrNFe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrNFe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrNFe1.WebServices.Consulta.RetornoWS, WBResposta);
    NomeArq := OpenDialog1.FileName;

    if pos(UpperCase('-nfe.xml'), UpperCase(NomeArq)) > 0 then
       NomeArq := StringReplace(NomeArq, '-nfe.xml', '-procNfe.xml', [rfIgnoreCase]);

    ACBrNFe1.NotasFiscais.Items[0].GravarXML(NomeArq);
    ShowMessage('Arquivo gravado em: ' + NomeArq);
    memoLog.Lines.Add('Arquivo gravado em: ' + NomeArq);
  end;
end;

procedure TfrmACBrNFe.btnAdministrarCSCClick(Sender: TObject);
var
  xTitulo, xCNPJ, xIndOp, xIdCSC, xCodigoCSC: string;
  AIndOP: TpcnIndOperacao;
  Ok: Boolean;
  i: Integer;
begin
  xTitulo := 'Administrar CSC - Código de Segurança do Contribuinte';

  xIndOp := '1';
  if not(InputQuery(xTitulo, '1 = Consultar / 2 = Novo / 3 = Revogar', xIndOp)) then
     exit;

  xIdCSC := '';
  if not(InputQuery(xTitulo, 'Id do CSC', xIdCSC)) then
     exit;

  xCodigoCSC := '';
  if not(InputQuery(xTitulo, 'Código do CSC', xCodigoCSC)) then
     exit;

  xCNPJ := Copy(OnlyNumber(edtEmitCNPJ.Text), 1, 8);
  AIndOP := StrToIndOperacao(Ok, xIndOp);

  ACBrNFe1.AdministrarCSC(xCNPJ, AIndOP, StrToIntDef(xIdCSC, 0), xCodigoCSC);

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add(xTitulo);
  MemoDados.Lines.Add('tpAmb  : '+ TpAmbToStr(ACBrNFe1.WebServices.AdministrarCSCNFCe.retAdmCSCNFCe.tpAmb));
  MemoDados.Lines.Add('cStat  : '+ IntToStr(ACBrNFe1.WebServices.AdministrarCSCNFCe.retAdmCSCNFCe.cStat));
  MemoDados.Lines.Add('xMotivo: '+ ACBrNFe1.WebServices.AdministrarCSCNFCe.retAdmCSCNFCe.xMotivo);

  for i := 0 to ACBrNFe1.WebServices.AdministrarCSCNFCe.retAdmCSCNFCe.dadosCsc.Count -1 do
  begin
    MemoDados.Lines.Add('idCsc    : '+ IntToStr(ACBrNFe1.WebServices.AdministrarCSCNFCe.retAdmCSCNFCe.dadosCsc[i].idCsc));
    MemoDados.Lines.Add('codigoCsc: '+ ACBrNFe1.WebServices.AdministrarCSCNFCe.retAdmCSCNFCe.dadosCsc[i].codigoCsc);
  end;
end;

procedure TfrmACBrNFe.btnAtorInterNFeTranspClick(Sender: TObject);
var
  xTitulo, Chave, idLote, CNPJ, nSeqEvento, xUF, TipoAtor, VerAplic,
  AutXML, TipoAutoriz: string;
  Ok: Boolean;
begin
  {
    Esse evento consta na NT 2020/007
    Só será possível realizar testes em ambiente de homologação a partir de 01/11/2021
    E enviar em produção a partir de 30/11/2021
  }
  xTitulo := 'Evento Ator Interessado na NF-e - Transportador';

  idLote := '1';
  if not(InputQuery(xTitulo, 'Numero do Lote de envio do Evento', idLote)) then
     exit;

  Chave := '';
  if not(InputQuery(xTitulo, 'Chave da NF-e', Chave)) then
     exit;

  Chave := Trim(OnlyNumber(Chave));

  CNPJ := copy(Chave,7,14);
  if not(InputQuery(xTitulo, 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;

  nSeqEvento := '1';
  if not(InputQuery(xTitulo, 'Sequencial do evento para o mesmo tipo de evento', nSeqEvento)) then
     exit;

  xUF := 'SP';
  if not(InputQuery(xTitulo, 'UF do emitente do Evento', xUF)) then
     exit;

  {
    1 = Empresa Emitente,
    2 = Empresa Destinatária,
    3 = Empresa Transportadora.
  }
  TipoAtor := '1';
  if not(InputQuery(xTitulo, 'Tipo de Ator do Evento (1, 2 ou 3)', TipoAtor)) then
     exit;

  VerAplic := '1.00';
  if not(InputQuery(xTitulo, 'Versão do Aplicativo do emitente', VerAplic)) then
     exit;

  AutXML := '';
  if not(InputQuery(xTitulo, 'CNPJ/CPF do Autorizado', AutXML)) then
     exit;

  {
    0 = Não permite;
    1 = Permite o transportador autorizado pelo emitente ou destinatário
        autorizar outros transportadores para ter acesso ao download da NF-e
  }
  TipoAutoriz := '0';
  if not(InputQuery(xTitulo, 'Tipo de Autorização (0 ou 1)', TipoAutoriz)) then
     exit;

  ACBrNFe1.EventoNFe.Evento.Clear;

  with ACBrNFe1.EventoNFe.Evento.New do
  begin
    infEvento.chNFe      := Chave;
    infEvento.CNPJ       := CNPJ;
    infEvento.dhEvento   := now;
    infEvento.tpEvento   := teAtorInteressadoNFe;
    infEvento.nSeqEvento := StrToIntDef(nSeqEvento, 1);

    infEvento.detEvento.cOrgaoAutor := UFtoCUF(xUF);
    infEvento.detEvento.tpAutor     := StrToTipoAutor(Ok, TipoAtor);
    infEvento.detEvento.verAplic    := VerAplic;

    // No momento a SEFAZ só aceita apenas 1 CNPJ/CPF
    with InfEvento.detEvento.autXML.New do
      CNPJCPF := AutXML;

    infEvento.detEvento.tpAutorizacao := StrToAutorizacao(Ok, TipoAutoriz);
  end;

  ACBrNFe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetWS;
  LoadXML(ACBrNFe1.WebServices.EnvEvento.RetWS, WBResposta);
end;

procedure TfrmACBrNFe.btnCancelarChaveClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, Justificativa: string;
begin
  Chave := '';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Chave da NF-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := copy(Chave,7,14);
  if not(InputQuery('WebServices Eventos: Cancelamento', 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;
  Protocolo:='';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Protocolo de Autorização', Protocolo)) then
     exit;
  Justificativa := 'Justificativa do Cancelamento';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa do Cancelamento', Justificativa)) then
     exit;

  ACBrNFe1.EventoNFe.Evento.Clear;

  with ACBrNFe1.EventoNFe.Evento.New do
  begin
    infEvento.chNFe := Chave;
    infEvento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teCancelamento;
    infEvento.detEvento.xJust := Justificativa;
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrNFe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrNFe1.WebServices.EnvEvento.RetornoWS, WBResposta);
  (*
  ACBrNFe1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrNFe1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrNFe1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrNFe1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chNFe
  ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrNFe.btnCancelarXMLClick(Sender: TObject);
var
  idLote, vAux: String;
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

    idLote := '1';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
       exit;

    vAux := '';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa', vAux)) then
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

    MemoResp.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetornoWS;
    LoadXML(ACBrNFe1.WebServices.EnvEvento.RetornoWS, WBResposta);
    ShowMessage(IntToStr(ACBrNFe1.WebServices.EnvEvento.cStat));
    ShowMessage(ACBrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrNFe.btnCarregarXMLEnviarClick(Sender: TObject);
var
  Ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    with ACBrNFe1.NotasFiscais.Items[0].NFe do
    begin
      Emit.CNPJCPF           := edtEmitCNPJ.Text;
      Emit.IE                := edtEmitIE.Text;
      Emit.xNome             := edtEmitRazao.Text;
      Emit.xFant             := edtEmitFantasia.Text;

      Emit.EnderEmit.fone    := edtEmitFone.Text;
      Emit.EnderEmit.CEP     := StrToInt(edtEmitCEP.Text);
      Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
      Emit.EnderEmit.nro     := edtEmitNumero.Text;
      Emit.EnderEmit.xCpl    := edtEmitComp.Text;
      Emit.EnderEmit.xBairro := edtEmitBairro.Text;
      Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
      Emit.EnderEmit.xMun    := edtEmitCidade.Text;
      Emit.EnderEmit.UF      := edtEmitUF.Text;
      Emit.enderEmit.cPais   := 1058;
      Emit.enderEmit.xPais   := 'BRASIL';

      Emit.IEST              := '';
      Emit.IM                := ''; // Preencher no caso de existir serviços na nota
      Emit.CNAE              := ''; // Verifique na cidade do emissor da NFe se é permitido
                                    // a inclusão de serviços na NFe
      Emit.CRT               := StrToCRT(Ok, IntToStr(cbTipoEmpresa.ItemIndex + 1));
    end;

    if ACBrNFe1.NotasFiscais.Items[0].NFe.Ide.modelo = 55 then
      ACBrNFe1.Enviar(1)
    else
      ACBrNFe1.Enviar(1, True, True);

    MemoResp.Lines.Text := ACBrNFe1.WebServices.Retorno.RetWS;
    memoRespWS.Lines.Text := ACBrNFe1.WebServices.Retorno.RetornoWS;
    LoadXML(ACBrNFe1.WebServices.Retorno.RetornoWS, WBResposta);

    MemoDados.Lines.Add('');
    MemoDados.Lines.Add('Envio NFe');
    MemoDados.Lines.Add('tpAmb: '+ TpAmbToStr(ACBrNFe1.WebServices.Retorno.TpAmb));
    MemoDados.Lines.Add('verAplic: '+ ACBrNFe1.WebServices.Retorno.verAplic);
    MemoDados.Lines.Add('cStat: '+ IntToStr(ACBrNFe1.WebServices.Retorno.cStat));
    MemoDados.Lines.Add('cUF: '+ IntToStr(ACBrNFe1.WebServices.Retorno.cUF));
    MemoDados.Lines.Add('xMotivo: '+ ACBrNFe1.WebServices.Retorno.xMotivo);
    MemoDados.Lines.Add('cMsg: '+ IntToStr(ACBrNFe1.WebServices.Retorno.cMsg));
    MemoDados.Lines.Add('xMsg: '+ ACBrNFe1.WebServices.Retorno.xMsg);
    MemoDados.Lines.Add('Recibo: '+ ACBrNFe1.WebServices.Retorno.Recibo);
    MemoDados.Lines.Add('Protocolo: '+ ACBrNFe1.WebServices.Retorno.Protocolo);
  end;
end;

procedure TfrmACBrNFe.btnCartadeCorrecaoClick(Sender: TObject);
var
  Chave, idLote, CNPJ, nSeqEvento, Correcao: string;
begin
  Chave := '';
  if not(InputQuery('WebServices Eventos: Carta de Correção', 'Chave da NF-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery('WebServices Eventos: Carta de Correção', 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := copy(Chave,7,14);
  if not(InputQuery('WebServices Eventos: Carta de Correção', 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;
  nSeqEvento := '1';
  if not(InputQuery('WebServices Eventos: Carta de Correção', 'Sequencial do evento para o mesmo tipo de evento', nSeqEvento)) then
     exit;
  Correcao := 'Correção a ser considerada, texto livre. A correção mais recente substitui as anteriores.';
  if not(InputQuery('WebServices Eventos: Carta de Correção', 'Correção a ser considerada', Correcao)) then
     exit;

  ACBrNFe1.EventoNFe.Evento.Clear;

  with ACBrNFe1.EventoNFe.Evento.New do
  begin
    infEvento.chNFe := Chave;
    infEvento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teCCe;
    infEvento.nSeqEvento := StrToInt(nSeqEvento);
    infEvento.detEvento.xCorrecao := Correcao;
  end;

  ACBrNFe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetWS;
  LoadXML(ACBrNFe1.WebServices.EnvEvento.RetWS, WBResposta);
end;

procedure TfrmACBrNFe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrNFe1.SSL.CertCNPJ);
end;

procedure TfrmACBrNFe.btnConsCadClick(Sender: TObject);
var
  UF, Documento: String;
  i: Integer;
begin
  UF := '';
  if not(InputQuery('WebServices Consulta Cadastro ', 'UF do Documento a ser Consultado:', UF)) then
    exit;

  Documento := '';
  if not(InputQuery('WebServices Consulta Cadastro ', 'Documento(CPF/CNPJ)', Documento)) then
    exit;

  Documento :=  Trim(OnlyNumber(Documento));

  ACBrNFe1.WebServices.ConsultaCadastro.UF := UF;

  if Length(Documento) > 11 then
     ACBrNFe1.WebServices.ConsultaCadastro.CNPJ := Documento
  else
     ACBrNFe1.WebServices.ConsultaCadastro.CPF := Documento;

  ACBrNFe1.WebServices.ConsultaCadastro.Executar;

  MemoResp.Lines.Text := ACBrNFe1.WebServices.ConsultaCadastro.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.ConsultaCadastro.RetornoWS;
  LoadXML(ACBrNFe1.WebServices.ConsultaCadastro.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Consulta Cadastro');
  MemoDados.Lines.Add('versao: ' + ACBrNFe1.WebServices.ConsultaCadastro.versao);
  MemoDados.Lines.Add('verAplic: ' + ACBrNFe1.WebServices.ConsultaCadastro.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.cStat));
  MemoDados.Lines.Add('xMotivo: ' + ACBrNFe1.WebServices.ConsultaCadastro.xMotivo);
  MemoDados.Lines.Add('DhCons: ' + DateTimeToStr(ACBrNFe1.WebServices.ConsultaCadastro.DhCons));
  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Dados da Empresa:');

  for i := 0 to ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Count -1 do
  begin
    MemoDados.Lines.Add('IE: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].IE);
    MemoDados.Lines.Add('CNPJ: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CNPJ);
    MemoDados.Lines.Add('CPF: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CPF);
    MemoDados.Lines.Add('cSit: ' + IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].cSit));
    MemoDados.Lines.Add('indCredNFe: ' + IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].indCredNFe));
    MemoDados.Lines.Add('indCredCTe: ' + IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].indCredCTe));
    MemoDados.Lines.Add('Nome: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xNome);
    MemoDados.Lines.Add('Fantasia: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xFant);
    MemoDados.Lines.Add('Reg. Apuração: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xRegApur);
    MemoDados.Lines.Add('CNAE: ' + IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CNAE));
    MemoDados.Lines.Add('dIniAtiv: ' + DateToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].dIniAtiv));
    MemoDados.Lines.Add('dUltSit: ' + DateToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].dUltSit));
    MemoDados.Lines.Add('dBaixa: ' + DateToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].dBaixa));
    MemoDados.Lines.Add('IEUnica: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].IEUnica);
    MemoDados.Lines.Add('IEAtual: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].IEAtual);

    MemoDados.Lines.Add('xLgr: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xLgr);
    MemoDados.Lines.Add('nro: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].nro);
    MemoDados.Lines.Add('xCpl: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xCpl);
    MemoDados.Lines.Add('xBairro: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xBairro);
    MemoDados.Lines.Add('cMun: ' + IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].cMun));
    MemoDados.Lines.Add('xMun: ' + ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xMun);
    MemoDados.Lines.Add('CEP: ' + IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CEP));
  end;
end;

procedure TfrmACBrNFe.btnConsultarChaveClick(Sender: TObject);
var
  vChave: String;
begin
  vChave := '';
  if not(InputQuery('WebServices Consultar', 'Chave da NF-e:', vChave)) then
    exit;

  ACBrNFe1.NotasFiscais.Clear;
  ACBrNFe1.WebServices.Consulta.NFeChave := vChave;
  ACBrNFe1.WebServices.Consulta.Executar;

  MemoResp.Lines.Text := ACBrNFe1.WebServices.Consulta.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.Consulta.RetornoWS;
  LoadXML(ACBrNFe1.WebServices.Consulta.RetornoWS, WBResposta);
end;

procedure TfrmACBrNFe.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    ACBrNFe1.Consultar;

    ShowMessage(ACBrNFe1.WebServices.Consulta.Protocolo);
    MemoResp.Lines.Text := ACBrNFe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrNFe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrNFe1.WebServices.Consulta.RetornoWS, WBResposta);
  end;
end;

procedure TfrmACBrNFe.btnConsultarReciboClick(Sender: TObject);
var
  aux: String;
begin
  aux := '';
  if not(InputQuery('Consultar Recibo Lote', 'Número do Recibo', aux)) then
    exit;

  ACBrNFe1.WebServices.Recibo.Recibo := aux;
  ACBrNFe1.WebServices.Recibo.Executar;

  MemoResp.Lines.Text := ACBrNFe1.WebServices.Recibo.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.Recibo.RetornoWS;
  LoadXML(ACBrNFe1.WebServices.Recibo.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Consultar Recibo');
  MemoDados.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.Recibo.tpAmb));
  MemoDados.Lines.Add('versao: ' + ACBrNFe1.WebServices.Recibo.versao);
  MemoDados.Lines.Add('verAplic: ' + ACBrNFe1.WebServices.Recibo.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.Recibo.cStat));
  MemoDados.Lines.Add('xMotivo: ' + ACBrNFe1.WebServices.Recibo.xMotivo);
  MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.Recibo.cUF));
  MemoDados.Lines.Add('xMsg: ' + ACBrNFe1.WebServices.Recibo.xMsg);
  MemoDados.Lines.Add('cMsg: ' + IntToStr(ACBrNFe1.WebServices.Recibo.cMsg));
  MemoDados.Lines.Add('Recibo: ' + ACBrNFe1.WebServices.Recibo.Recibo);
end;

procedure TfrmACBrNFe.btnCriarEnviarClick(Sender: TObject);
var
  vAux, vNumLote: String;
  Sincrono: Boolean;
begin
  vAux := '';
  if not(InputQuery('WebServices Enviar', 'Numero da Nota', vAux)) then
    exit;

  vNumLote := '1';
  if not(InputQuery('WebServices Enviar', 'Numero do Lote', vNumLote)) then
    exit;

  vNumLote := OnlyNumber(vNumLote);
  Sincrono := False;

  if Trim(vNumLote) = '' then
  begin
    MessageDlg('Número do Lote inválido.', mtError,[mbok], 0);
    exit;
  end;

  AlimentarComponente(vAux);

  if ACBrNFe1.Configuracoes.Geral.ModeloDF = moNFe then
    {
      Envio de NF-e modelo 55
    }
    ACBrNFe1.Enviar(vNumLote)
  else
  begin
    {
      Envio de NFC-e modelo 65
    }
    if (rgDANFCE.ItemIndex = 1) then
      PrepararImpressao;

    if ACBrNFe1.NotasFiscais.Count > 1 then
      {
        Se o lote tem 2 ou mais notas (até 50) o envio tem que ser no modo
        assíncrono (terceiro parâmetro tem que valer False)
      }
      ACBrNFe1.Enviar(vNumLote, True, False)
    else
      {
        Se o lote tem apenas 1 nota o envio tem que ser no modo
        síncrono (terceiro parâmetro tem que valer True)
      }
      ACBrNFe1.Enviar(vNumLote, True, True);
  end;

  pgRespostas.ActivePageIndex := 1;

  if not Sincrono then
  begin
    MemoResp.Lines.Text := ACBrNFe1.WebServices.Retorno.RetWS;
    memoRespWS.Lines.Text := ACBrNFe1.WebServices.Retorno.RetornoWS;
    LoadXML(ACBrNFe1.WebServices.Retorno.RetWS, WBResposta);

    MemoDados.Lines.Add('');
    MemoDados.Lines.Add('Envio NFe');
    MemoDados.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.Retorno.TpAmb));
    MemoDados.Lines.Add('verAplic: ' + ACBrNFe1.WebServices.Retorno.verAplic);
    MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.Retorno.cStat));
    MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.Retorno.cUF));
    MemoDados.Lines.Add('xMotivo: ' + ACBrNFe1.WebServices.Retorno.xMotivo);
    MemoDados.Lines.Add('cMsg: ' + IntToStr(ACBrNFe1.WebServices.Retorno.cMsg));
    MemoDados.Lines.Add('xMsg: ' + ACBrNFe1.WebServices.Retorno.xMsg);
    MemoDados.Lines.Add('Recibo: ' + ACBrNFe1.WebServices.Retorno.Recibo);
    MemoDados.Lines.Add('Protocolo: ' + ACBrNFe1.WebServices.Retorno.Protocolo);
  end
  else
  begin
    MemoResp.Lines.Text := ACBrNFe1.WebServices.Enviar.RetWS;
    memoRespWS.Lines.Text := ACBrNFe1.WebServices.Enviar.RetornoWS;
    LoadXML(ACBrNFe1.WebServices.Enviar.RetWS, WBResposta);

    MemoDados.Lines.Add('');
    MemoDados.Lines.Add('Envio NFCe');
    MemoDados.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.Enviar.TpAmb));
    MemoDados.Lines.Add('verAplic: ' + ACBrNFe1.WebServices.Enviar.verAplic);
    MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.Enviar.cStat));
    MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.Enviar.cUF));
    MemoDados.Lines.Add('xMotivo: ' + ACBrNFe1.WebServices.Enviar.xMotivo);
    MemoDados.Lines.Add('Recibo: '+ ACBrNFe1.WebServices.Enviar.Recibo);

    if (ACBrNFe1.Integrador= ACBrIntegrador1) then
    begin
      if (ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo <> '') then
      begin
        MemoResp.Lines.Add('[Integrador]');
        MemoResp.Lines.Add('Codigo=' + ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo);
        MemoResp.Lines.Add('Valor=' + ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Valor);

        ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo := '';
        ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Valor := '';
      end;
    end;
  end;
  (*
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].tpAmb
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].verAplic
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].chNFe
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].dhRecbto
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].nProt
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].digVal
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].cStat
  ACBrNFe1.WebServices.Retorno.NFeRetorno.ProtNFe.Items[0].xMotivo
  *)
end;

procedure TfrmACBrNFe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateTimeBr(ACBrNFe1.SSL.CertDataVenc));
end;

procedure TfrmACBrNFe.btnDistrDFePorChaveClick(Sender: TObject);
var
  xTitulo, cUFAutor, CNPJ, Chave: string;
begin
  xTitulo := 'Distribuição DFe Por Chave';
  cUFAutor := '';
  if not(InputQuery(xTitulo, 'Código da UF do Autor', cUFAutor)) then
     exit;

  CNPJ := '';
  if not(InputQuery(xTitulo, 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
     exit;

  Chave := '';
  if not(InputQuery(xTitulo, 'Chave da NF-e', Chave)) then
     exit;

  ACBrNFe1.DistribuicaoDFePorChaveNFe(StrToInt(cUFAutor), CNPJ, Chave);

  MemoResp.Lines.Text := ACBrNFe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrNFe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);
end;

procedure TfrmACBrNFe.btnDistrDFePorNSUClick(Sender: TObject);
var
  xTitulo, cUFAutor, CNPJ, ANSU: string;
begin
  xTitulo := 'Distribuição DF-e por NSU';

  cUFAutor := '';
  if not(InputQuery(xTitulo, 'Código da UF do Autor', cUFAutor)) then
     exit;

  CNPJ := '';
  if not(InputQuery(xTitulo, 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
     exit;

  ANSU := '';
  if not(InputQuery(xTitulo, 'NSU específico', ANSU)) then
     exit;

  ACBrNFe1.DistribuicaoDFePorNSU(StrToInt(cUFAutor), CNPJ, ANSU);

  MemoResp.Lines.Text := ACBrNFe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrNFe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);
end;

procedure TfrmACBrNFe.btnDistrDFePorUltNSUClick(Sender: TObject);
var
  xTitulo, cUFAutor, CNPJ, AultNSU: string;
  i: Integer;
begin
  // Opção para simular uma consulta ao WebService lendo o arquivo de retorno e populando o componente
  OpenDialog1.Title := 'Selecione um Arquivo de Distribuição para simular uma consulta ou feche para consultar o WebService';
  OpenDialog1.DefaultExt := '*-dist-dfe.XML';
  OpenDialog1.Filter := 'Arquivos Distribuição DFe (*-dist-dfe.XML)|*-dist-dfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  // Lê o arquivo selecionado
  if OpenDialog1.Execute then
  begin
    ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.CarregarArquivo(OpenDialog1.FileName);
    ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.LerXml;

    AultNSU := ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU;
  end
  // Consulta o WebService
  else
  begin
    xTitulo := 'Distribuição DF-e por último NSU';

    cUFAutor := IntToStr(ACBrNFe1.Configuracoes.WebServices.UFCodigo);
    if not(InputQuery(xTitulo, 'Código da UF do Autor', cUFAutor)) then
       exit;

    CNPJ := edtEmitCNPJ.Text;
    if not(InputQuery(xTitulo, 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
       exit;

    AultNSU := '';
    if not(InputQuery(xTitulo, 'Último NSU recebido pelo ator', AultNSU)) then
       exit;

    ACBrNFe1.DistribuicaoDFePorUltNSU(StrToInt(cUFAutor), CNPJ, AultNSU);
  end;

  with ACBrNFe1.WebServices.DistribuicaoDFe.retDistDFeInt do
  begin
    // Caso não retorne registros, ocorra consumo indevido ou seja o último lote, gera alerta
    if ( ( cStat = 137 ) or
         ( cStat = 656 ) or
         ( ultNSU = maxNSU ) ) then
    begin
      // 656-Consumo indevido
      if cStat = 656 then
      begin
        MemoDados.Lines.Add('Atenção...: Consumo indevido.');

        if AultNSU <> ultNSU then
          MemoDados.Lines.Add('            ultNSU utilizado nesta consulta [' + AultNSU + '] é diferente ' +
                              'do ultNSU consultado na Sefaz [' + ultNSU + '].');
      end
      // 137-Nenhum documento localizado
      else if cStat = 137 then
        MemoDados.Lines.Add('Atenção...: Não existem mais registros disponíveis.')
      // ultNSU = maxNSU - Documentos Localizados, mas é o último lote
      else
        MemoDados.Lines.Add('Atenção...: Este é o último lote de registros disponíveis para distribuição.');

      MemoDados.Lines.Add('Atenção...: Aguarde 1 hora para a próxima consulta.');
      MemoDados.Lines.Add(' ');
    end;

    MemoDados.Lines.Add('Qtde Documentos Retornados: ' + IntToStr(docZip.Count));
    MemoDados.Lines.Add('Status....: ' + IntToStr(cStat));
    MemoDados.Lines.Add('Motivo....: ' + xMotivo);
    MemoDados.Lines.Add('Último NSU: ' + ultNSU);
    MemoDados.Lines.Add('Máximo NSU: ' + maxNSU);
    MemoDados.Lines.Add(' ');
    MemoDados.Lines.Add('Documentos Retornados:');

    for i := 0 to docZip.Count -1 do
    begin
      case docZip[i].schema of
        schresNFe:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (Resumo Nota) Chave: ' + docZip[i].resDFe.chDFe);

        schprocNFe:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (Nota Completa) Chave: ' + docZip[i].resDFe.chDFe);

        schresEvento:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (Resumo Evento) Chave: ' + docZip[i].resEvento.chDFe);

        schprocEventoNFe:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (Evento Completo) ID: ' + docZip[i].procEvento.Id);
      end;
    end;
  end;

  MemoResp.Lines.Text := ACBrNFe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrNFe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);

  pgRespostas.ActivePage := Dados;
end;

procedure TfrmACBrNFe.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  Para := '';
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrNFe1.NotasFiscais.Clear;
  ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido

    ConfigurarEmail;
    ACBrNFe1.NotasFiscais.Items[0].EnviarEmail(Para
      , edtEmailAssunto.Text
      , mmEmailMsg.Lines
      , True  // Enviar PDF junto
      , CC    // Lista com emails que serao enviado copias - TStrings
      , nil // Lista de anexos - TStrings
      );

  finally
    CC.Free;
  end;

end;

procedure TfrmACBrNFe.btnEnviarEventoEmailClick(Sender: TObject);
var
  Para: String;
  CC, Evento: Tstrings;
begin
  Para := '';
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  Evento := TStringList.Create;
  CC := TStringList.Create;
  try
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);
    ACBrNFe1.EventoNFe.Evento.Clear;
    ACBrNFe1.EventoNFe.LerXML(OpenDialog1.FileName);

    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrNFe1.EnviarEmailEvento(Para
      , edtEmailAssunto.Text
      , mmEmailMsg.Lines
      , CC // Lista com emails que serao enviado copias - TStrings
      , nil // Lista de anexos - TStrings
      , nil  // ReplyTo
      );
  finally
    CC.Free;
    Evento.Free;
  end;

end;

procedure TfrmACBrNFe.btnEventoEPECClick(Sender: TObject);
var
  Chave, idLote, CNPJ, IE, ValorNF, CNPJDest, UFDest, Titulo: string;
begin
  Titulo := 'Evento EPEC';
  Chave := '';
  if not(InputQuery(Titulo, 'Chave da NF-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery(Titulo, 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := copy(Chave,7,14);
  if not(InputQuery(Titulo, 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;
  IE:='';
  if not(InputQuery(Titulo, 'Insc. Estadual', IE)) then
     exit;
  ValorNF:='';
  if not(InputQuery(Titulo, 'Valor da Nota', ValorNF)) then
     exit;
  CNPJDest := '';
  if not(InputQuery(Titulo, 'CNPJ ou o CPF do Destinatario', CNPJDest)) then
     exit;
  UFDest:='';
  if not(InputQuery(Titulo, 'UF do Destinatario', UFDest)) then
     exit;

  ACBrNFe1.EventoNFe.Evento.Clear;

  with ACBrNFe1.EventoNFe.Evento.New do
  begin
    infEvento.chNFe := Chave;
    infEvento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teEPECNFe;

    infEvento.detEvento.cOrgaoAutor := ACBrNFe1.Configuracoes.WebServices.UFCodigo;
    infEvento.detEvento.tpAutor := taEmpresaEmitente;
    infEvento.detEvento.verAplic := '1.00';
    infEvento.detEvento.dhEmi := now;
    infEvento.detEvento.tpNF := tnSaida;
    infEvento.detEvento.IE := IE;
    infEvento.detEvento.vNF := StrToFloatDef(ValorNF, 0);

    InfEvento.detEvento.dest.UF := UFDest;
    InfEvento.detEvento.dest.CNPJCPF := CNPJDest;
  end;

  ACBrNFe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrNFe1.WebServices.EnvEvento.RetornoWS, WBResposta);
end;

procedure TfrmACBrNFe.btnGerarPDFClick(Sender: TObject);
var
  CarregarMaisXML: Boolean;
begin
  CarregarMaisXML := true;
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
  ACBrNFe1.NotasFiscais.Clear;

  while CarregarMaisXML do
  begin
    if OpenDialog1.Execute then
      ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

    CarregarMaisXML := MessageDlg('Carregar mais Notas?', mtConfirmation, mbYesNoCancel, 0) = mrYes;
  end;

  ACBrNFe1.NotasFiscais.ImprimirPDF;
end;

procedure TfrmACBrNFe.btnGerarTXTClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  vAux := '';
  if not(InputQuery('WebServices Enviar', 'Numero da Nota', vAux)) then
    exit;

  vNumLote := '1';
  if not(InputQuery('WebServices Enviar', 'Numero do Lote', vNumLote)) then
    exit;

  vNumLote := OnlyNumber(vNumLote);

  if Trim(vNumLote) = '' then
  begin
    MessageDlg('Número do Lote inválido.',mtError,[mbok],0);
    exit;
  end;

  ACBrNFe1.NotasFiscais.Clear;

  AlimentarComponente(vAux);

//  ACBrNFe1.NotasFiscais.GravarTXT(caminho e nome do arquivo TXT);
end;

procedure TfrmACBrNFe.btnGerarXMLClick(Sender: TObject);
var
  vAux: String;
begin
  vAux := '';
  if not(InputQuery('WebServices Enviar', 'Numero da Nota', vAux)) then
    exit;

  ACBrNFe1.NotasFiscais.Clear;

  AlimentarComponente(vAux);

  ACBrNFe1.NotasFiscais.Assinar;

  ACBrNFe1.NotasFiscais.Items[0].GravarXML();

  ShowMessage('Arquivo gerado em: ' + ACBrNFe1.NotasFiscais.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: ' + ACBrNFe1.NotasFiscais.Items[0].NomeArq);

  MemoResp.Lines.LoadFromFile(ACBrNFe1.NotasFiscais.Items[0].NomeArq);

  LoadXML(MemoResp.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrNFe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrNFe1.SSL.UseCertificateHTTP;
  ACBrNFe1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrNFe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrNFe1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFe.btnImportarXMLClick(Sender: TObject);
var
//  i, j, k, n: integer;
//  Nota, Node, NodePai, NodeItem: TTreeNode;
  NFeRTXT: TNFeRTXT;
begin
  OpenDialog1.FileName  :=  '';
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Arquivos TXT (*.TXT)|*.TXT|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    //tenta TXT
    ACBrNFe1.NotasFiscais.Add;
    NFeRTXT := TNFeRTXT.Create(ACBrNFe1.NotasFiscais.Items[0].NFe);
    NFeRTXT.CarregarArquivo(OpenDialog1.FileName);
    if NFeRTXT.LerTxt then
       NFeRTXT.Free
    else
    begin
       NFeRTXT.Free;
       //tenta XML
       ACBrNFe1.NotasFiscais.Clear;
       try
          ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
       except
          ShowMessage('Arquivo NFe Inválido');
          exit;
       end;
    end;
    (*
    trvwNFe.Items.Clear;

    for n:=0 to ACBrNFe1.NotasFiscais.Count-1 do
    begin
    with ACBrNFe1.NotasFiscais.Items[n].NFe do
     begin

       Nota := trvwNFe.Items.Add(nil,infNFe.ID);
       trvwNFe.Items.AddChild(Nota,'ID= ' +infNFe.ID);
       Node := trvwNFe.Items.AddChild(Nota,'procNFe');
       trvwNFe.Items.AddChild(Node,'tpAmb= '     +TpAmbToStr(procNFe.tpAmb));
       trvwNFe.Items.AddChild(Node,'verAplic= '  +procNFe.verAplic);
       trvwNFe.Items.AddChild(Node,'chNFe= '     +procNFe.chNFe);
       trvwNFe.Items.AddChild(Node,'dhRecbto= '  +DateTimeToStr(procNFe.dhRecbto));
       trvwNFe.Items.AddChild(Node,'nProt= '     +procNFe.nProt);
       trvwNFe.Items.AddChild(Node,'digVal= '    +procNFe.digVal);
       trvwNFe.Items.AddChild(Node,'cStat= '     +IntToStr(procNFe.cStat));
       trvwNFe.Items.AddChild(Node,'xMotivo= '   +procNFe.xMotivo);

       Node := trvwNFe.Items.AddChild(Nota,'Ide');
       trvwNFe.Items.AddChild(Node,'cNF= '     +IntToStr(Ide.cNF));
       trvwNFe.Items.AddChild(Node,'natOp= '   +Ide.natOp );
       trvwNFe.Items.AddChild(Node,'indPag= '  +IndpagToStr(Ide.indPag));
       trvwNFe.Items.AddChild(Node,'modelo= '  +IntToStr(Ide.modelo));
       trvwNFe.Items.AddChild(Node,'serie= '   +IntToStr(Ide.serie));
       trvwNFe.Items.AddChild(Node,'nNF= '     +IntToStr(Ide.nNF));
       trvwNFe.Items.AddChild(Node,'dEmi= '    +DateToStr(Ide.dEmi));
       trvwNFe.Items.AddChild(Node,'dSaiEnt= ' +DateToStr(Ide.dSaiEnt));
       trvwNFe.Items.AddChild(Node,'hSaiEnt= ' +DateToStr(Ide.hSaiEnt));
       trvwNFe.Items.AddChild(Node,'tpNF= '    +tpNFToStr(Ide.tpNF));
       trvwNFe.Items.AddChild(Node,'finNFe= '  +FinNFeToStr(Ide.finNFe));
       trvwNFe.Items.AddChild(Node,'verProc= ' +Ide.verProc);
       trvwNFe.Items.AddChild(Node,'cUF= '     +IntToStr(Ide.cUF));
       trvwNFe.Items.AddChild(Node,'cMunFG= '  +IntToStr(Ide.cMunFG));
       trvwNFe.Items.AddChild(Node,'tpImp= '   +TpImpToStr(Ide.tpImp));
       trvwNFe.Items.AddChild(Node,'tpEmis= '  +TpEmisToStr(Ide.tpEmis));
       trvwNFe.Items.AddChild(Node,'cDV= '     +IntToStr(Ide.cDV));
       trvwNFe.Items.AddChild(Node,'tpAmb= '   +TpAmbToStr(Ide.tpAmb));
       trvwNFe.Items.AddChild(Node,'finNFe= '  +FinNFeToStr(Ide.finNFe));
       trvwNFe.Items.AddChild(Node,'procEmi= ' +procEmiToStr(Ide.procEmi));
       trvwNFe.Items.AddChild(Node,'verProc= ' +Ide.verProc);
       trvwNFe.Items.AddChild(Node,'dhCont= '  +DateTimeToStr(Ide.dhCont));
       trvwNFe.Items.AddChild(Node,'xJust= '   +Ide.xJust);

       for i:=0 to Ide.NFref.Count-1 do
        begin
          if Ide.NFref.Items[i].refNFe <> '' then
          begin
            Node := trvwNFe.Items.AddChild(Node,'NFRef'+IntToStrZero(i+1,3));
            trvwNFe.Items.AddChild(Node,'refNFe= ' +Ide.NFref.Items[i].refNFe);
            trvwNFe.Items.AddChild(Node,'cUF= '    +IntToStr(Ide.NFref.Items[i].RefNF.cUF));
            trvwNFe.Items.AddChild(Node,'AAMM= '   +Ide.NFref.Items[i].RefNF.AAMM);
            trvwNFe.Items.AddChild(Node,'CNPJ= '   +Ide.NFref.Items[i].RefNF.CNPJ);
            trvwNFe.Items.AddChild(Node,'modelo= ' +IntToStr(Ide.NFref.Items[i].RefNF.modelo));
            trvwNFe.Items.AddChild(Node,'serie= '  +IntToStr(Ide.NFref.Items[i].RefNF.serie));
            trvwNFe.Items.AddChild(Node,'nNF= '    +IntToStr(Ide.NFref.Items[i].RefNF.nNF));
          end;

          if Ide.NFref.Items[i].RefECF.nCOO <> '' then
          begin
            Node := trvwNFe.Items.AddChild(Node,'refECF'+IntToStrZero(i+1,3));
            trvwNFe.Items.AddChild(Node,'mod= '  +ECFModRefToStr(Ide.NFref.Items[i].RefECF.modelo));
            trvwNFe.Items.AddChild(Node,'nECF= ' +Ide.NFref.Items[i].RefECF.nECF);
            trvwNFe.Items.AddChild(Node,'nCOO= ' +Ide.NFref.Items[i].RefECF.nCOO);
          end;
        end;

       Node := trvwNFe.Items.AddChild(Nota,'Emit');
       trvwNFe.Items.AddChild(Node,'CNPJCPF= ' +Emit.CNPJCPF);
       trvwNFe.Items.AddChild(Node,'IE='       +Emit.IE);
       trvwNFe.Items.AddChild(Node,'xNome='    +Emit.xNome);
       trvwNFe.Items.AddChild(Node,'xFant='    +Emit.xFant );
       trvwNFe.Items.AddChild(Node,'IEST='     +Emit.IEST);
       trvwNFe.Items.AddChild(Node,'IM='       +Emit.IM);
       trvwNFe.Items.AddChild(Node,'CNAE='     +Emit.CNAE);
       trvwNFe.Items.AddChild(Node,'CRT='      +CRTToStr(Emit.CRT));

       Node := trvwNFe.Items.AddChild(Node,'EnderEmit');
       trvwNFe.Items.AddChild(Node,'Fone='    +Emit.EnderEmit.fone);
       trvwNFe.Items.AddChild(Node,'CEP='     +IntToStr(Emit.EnderEmit.CEP));
       trvwNFe.Items.AddChild(Node,'xLgr='    +Emit.EnderEmit.xLgr);
       trvwNFe.Items.AddChild(Node,'nro='     +Emit.EnderEmit.nro);
       trvwNFe.Items.AddChild(Node,'xCpl='    +Emit.EnderEmit.xCpl);
       trvwNFe.Items.AddChild(Node,'xBairro=' +Emit.EnderEmit.xBairro);
       trvwNFe.Items.AddChild(Node,'cMun='    +IntToStr(Emit.EnderEmit.cMun));
       trvwNFe.Items.AddChild(Node,'xMun='    +Emit.EnderEmit.xMun);
       trvwNFe.Items.AddChild(Node,'UF'       +Emit.EnderEmit.UF);
       trvwNFe.Items.AddChild(Node,'cPais='   +IntToStr(Emit.EnderEmit.cPais));
       trvwNFe.Items.AddChild(Node,'xPais='   +Emit.EnderEmit.xPais);

       if Avulsa.CNPJ  <> '' then
        begin
          Node := trvwNFe.Items.AddChild(Nota,'Avulsa');
          trvwNFe.Items.AddChild(Node,'CNPJ='    +Avulsa.CNPJ);
          trvwNFe.Items.AddChild(Node,'xOrgao='  +Avulsa.xOrgao);
          trvwNFe.Items.AddChild(Node,'matr='    +Avulsa.matr );
          trvwNFe.Items.AddChild(Node,'xAgente=' +Avulsa.xAgente);
          trvwNFe.Items.AddChild(Node,'fone='    +Avulsa.fone);
          trvwNFe.Items.AddChild(Node,'UF='      +Avulsa.UF);
          trvwNFe.Items.AddChild(Node,'nDAR='    +Avulsa.nDAR);
          trvwNFe.Items.AddChild(Node,'dEmi='    +DateToStr(Avulsa.dEmi));
          trvwNFe.Items.AddChild(Node,'vDAR='    +FloatToStr(Avulsa.vDAR));
          trvwNFe.Items.AddChild(Node,'repEmi='  +Avulsa.repEmi);
          trvwNFe.Items.AddChild(Node,'dPag='    +DateToStr(Avulsa.dPag));
        end;
       Node := trvwNFe.Items.AddChild(Nota,'Dest');
       trvwNFe.Items.AddChild(Node,'CNPJCPF= ' +Dest.CNPJCPF);
       trvwNFe.Items.AddChild(Node,'IE='       +Dest.IE);
       trvwNFe.Items.AddChild(Node,'ISUF='     +Dest.ISUF);
       trvwNFe.Items.AddChild(Node,'xNome='    +Dest.xNome);
       trvwNFe.Items.AddChild(Node,'email='    +Dest.Email);

       Node := trvwNFe.Items.AddChild(Node,'EnderDest');
       trvwNFe.Items.AddChild(Node,'Fone='    +Dest.EnderDest.Fone);
       trvwNFe.Items.AddChild(Node,'CEP='     +IntToStr(Dest.EnderDest.CEP));
       trvwNFe.Items.AddChild(Node,'xLgr='    +Dest.EnderDest.xLgr);
       trvwNFe.Items.AddChild(Node,'nro='     +Dest.EnderDest.nro);
       trvwNFe.Items.AddChild(Node,'xCpl='    +Dest.EnderDest.xCpl);
       trvwNFe.Items.AddChild(Node,'xBairro=' +Dest.EnderDest.xBairro);
       trvwNFe.Items.AddChild(Node,'cMun='    +IntToStr(Dest.EnderDest.cMun));
       trvwNFe.Items.AddChild(Node,'xMun='    +Dest.EnderDest.xMun);
       trvwNFe.Items.AddChild(Node,'UF='      +Dest.EnderDest.UF );
       trvwNFe.Items.AddChild(Node,'cPais='   +IntToStr(Dest.EnderDest.cPais));
       trvwNFe.Items.AddChild(Node,'xPais='   +Dest.EnderDest.xPais);

       if Retirada.CNPJ <> '' then
        begin
          Node := trvwNFe.Items.AddChild(Nota,'Retirada');
          trvwNFe.Items.AddChild(Node,'CNPJ='    +Retirada.CNPJ);
          trvwNFe.Items.AddChild(Node,'xLgr='    +Retirada.xLgr);
          trvwNFe.Items.AddChild(Node,'nro='     +Retirada.nro);
          trvwNFe.Items.AddChild(Node,'xCpl='    +Retirada.xCpl);
          trvwNFe.Items.AddChild(Node,'xBairro=' +Retirada.xBairro);
          trvwNFe.Items.AddChild(Node,'cMun='    +IntToStr(Retirada.cMun));
          trvwNFe.Items.AddChild(Node,'xMun='    +Retirada.xMun);
          trvwNFe.Items.AddChild(Node,'UF='      +Retirada.UF);
        end;

       if Entrega.CNPJ <> '' then
        begin
          Node := trvwNFe.Items.AddChild(Nota,'Entrega');
          trvwNFe.Items.AddChild(Node,'CNPJ='    +Entrega.CNPJ);
          trvwNFe.Items.AddChild(Node,'xLgr='    +Entrega.xLgr);
          trvwNFe.Items.AddChild(Node,'nro='     +Entrega.nro);
          trvwNFe.Items.AddChild(Node,'xCpl='    +Entrega.xCpl);
          trvwNFe.Items.AddChild(Node,'xBairro=' +Entrega.xBairro);
          trvwNFe.Items.AddChild(Node,'cMun='    +IntToStr(Entrega.cMun));
          trvwNFe.Items.AddChild(Node,'xMun='    +Entrega.xMun);
          trvwNFe.Items.AddChild(Node,'UF='      +Entrega.UF);
        end;

       for I := 0 to Det.Count-1 do
        begin
          with Det.Items[I] do
           begin
               NodeItem := trvwNFe.Items.AddChild(Nota,'Produto'+IntToStrZero(I+1,3));
               trvwNFe.Items.AddChild(NodeItem,'nItem='  +IntToStr(Prod.nItem) );
               trvwNFe.Items.AddChild(NodeItem,'cProd='  +Prod.cProd );
               trvwNFe.Items.AddChild(NodeItem,'cEAN='   +Prod.cEAN);
               trvwNFe.Items.AddChild(NodeItem,'xProd='  +Prod.xProd);
               trvwNFe.Items.AddChild(NodeItem,'NCM='    +Prod.NCM);
               trvwNFe.Items.AddChild(NodeItem,'EXTIPI=' +Prod.EXTIPI);
               //trvwNFe.Items.AddChild(NodeItem,'genero=' +IntToStr(Prod.genero));
               trvwNFe.Items.AddChild(NodeItem,'CFOP='   +Prod.CFOP);
               trvwNFe.Items.AddChild(NodeItem,'uCom='   +Prod.uCom);
               trvwNFe.Items.AddChild(NodeItem,'qCom='   +FloatToStr(Prod.qCom));
               trvwNFe.Items.AddChild(NodeItem,'vUnCom=' +FloatToStr(Prod.vUnCom));
               trvwNFe.Items.AddChild(NodeItem,'vProd='  +FloatToStr(Prod.vProd));

               trvwNFe.Items.AddChild(NodeItem,'cEANTrib=' +Prod.cEANTrib);
               trvwNFe.Items.AddChild(NodeItem,'uTrib='    +Prod.uTrib);
               trvwNFe.Items.AddChild(NodeItem,'qTrib='    +FloatToStr(Prod.qTrib));
               trvwNFe.Items.AddChild(NodeItem,'vUnTrib='  +FloatToStr(Prod.vUnTrib));

               trvwNFe.Items.AddChild(NodeItem,'vFrete='      +FloatToStr(Prod.vFrete));
               trvwNFe.Items.AddChild(NodeItem,'vSeg='        +FloatToStr(Prod.vSeg));
               trvwNFe.Items.AddChild(NodeItem,'vDesc='       +FloatToStr(Prod.vDesc));
               trvwNFe.Items.AddChild(NodeItem,'vOutro='      +FloatToStr(Prod.vOutro));
               trvwNFe.Items.AddChild(NodeItem,'indTot='      +indTotToStr(Prod.IndTot));
               trvwNFe.Items.AddChild(NodeItem,'xPed='        +Prod.xPed);
               trvwNFe.Items.AddChild(NodeItem,'nItemPedido=' +Prod.nItemPed);

               trvwNFe.Items.AddChild(NodeItem,'infAdProd=' +infAdProd);

               for J:=0 to Prod.DI.Count-1 do
                begin
                  if Prod.DI.Items[j].nDi <> '' then
                   begin
                     with Prod.DI.Items[j] do
                      begin
                        NodePai := trvwNFe.Items.AddChild(NodeItem,'DI'+IntToStrZero(J+1,3));
                        trvwNFe.Items.AddChild(NodePai,'nDi='         +nDi);
                        trvwNFe.Items.AddChild(NodePai,'dDi='         +DateToStr(dDi));
                        trvwNFe.Items.AddChild(NodePai,'xLocDesemb='  +xLocDesemb);
                        trvwNFe.Items.AddChild(NodePai,'UFDesemb='    +UFDesemb);
                        trvwNFe.Items.AddChild(NodePai,'dDesemb='     +DateToStr(dDesemb));
                        trvwNFe.Items.AddChild(NodePai,'cExportador=' +cExportador);;

                        for K:=0 to adi.Count-1 do
                         begin
                           with adi.Items[K] do
                            begin
                              Node := trvwNFe.Items.AddChild(NodePai,'LADI'+IntToStrZero(K+1,3));
                              trvwNFe.Items.AddChild(Node,'nAdicao='     +IntToStr(nAdicao));
                              trvwNFe.Items.AddChild(Node,'nSeqAdi='     +IntToStr(nSeqAdi));
                              trvwNFe.Items.AddChild(Node,'cFabricante=' +cFabricante);
                              trvwNFe.Items.AddChild(Node,'vDescDI='     +FloatToStr(vDescDI));
                            end;
                         end;
                      end;
                   end
                  else
                    Break;
                end;

              if Prod.veicProd.chassi <> '' then
               begin
                 Node := trvwNFe.Items.AddChild(NodeItem,'Veiculo');
                 with Prod.veicProd do
                  begin
                    trvwNFe.Items.AddChild(Node,'tpOP='     +tpOPToStr(tpOP));
                    trvwNFe.Items.AddChild(Node,'chassi='   +chassi);
                    trvwNFe.Items.AddChild(Node,'cCor='     +cCor);
                    trvwNFe.Items.AddChild(Node,'xCor='     +xCor);
                    trvwNFe.Items.AddChild(Node,'pot='      +pot);
                    trvwNFe.Items.AddChild(Node,'Cilin='      +Cilin);
                    trvwNFe.Items.AddChild(Node,'pesoL='    +pesoL);
                    trvwNFe.Items.AddChild(Node,'pesoB='    +pesoB);
                    trvwNFe.Items.AddChild(Node,'nSerie='   +nSerie);
                    trvwNFe.Items.AddChild(Node,'tpComb='   +tpComb);
                    trvwNFe.Items.AddChild(Node,'nMotor='   +nMotor);
                    trvwNFe.Items.AddChild(Node,'CMT='     +CMT);
                    trvwNFe.Items.AddChild(Node,'dist='     +dist);
                    //trvwNFe.Items.AddChild(Node,'RENAVAM='  +RENAVAM);
                    trvwNFe.Items.AddChild(Node,'anoMod='   +IntToStr(anoMod));
                    trvwNFe.Items.AddChild(Node,'anoFab='   +IntToStr(anoFab));
                    trvwNFe.Items.AddChild(Node,'tpPint='   +tpPint);
                    trvwNFe.Items.AddChild(Node,'tpVeic='   +IntToStr(tpVeic));
                    trvwNFe.Items.AddChild(Node,'espVeic='  +IntToStr(espVeic));
                    trvwNFe.Items.AddChild(Node,'VIN='      +VIN);
                    trvwNFe.Items.AddChild(Node,'condVeic=' +condVeicToStr(condVeic));
                    trvwNFe.Items.AddChild(Node,'cMod='     +cMod);
                  end;
               end;

               for J:=0 to Prod.med.Count-1 do
                begin
                  Node := trvwNFe.Items.AddChild(NodeItem,'Medicamento'+IntToStrZero(J+1,3) );
                  with Prod.med.Items[J] do
                   begin
                     trvwNFe.Items.AddChild(Node,'nLote=' +nLote);
                     trvwNFe.Items.AddChild(Node,'qLote=' +FloatToStr(qLote));
                     trvwNFe.Items.AddChild(Node,'dFab='  +DateToStr(dFab));
                     trvwNFe.Items.AddChild(Node,'dVal='  +DateToStr(dVal));
                     trvwNFe.Items.AddChild(Node,'vPMC='  +FloatToStr(vPMC));
                    end;
                end;

               for J:=0 to Prod.arma.Count-1 do
                begin
                  Node := trvwNFe.Items.AddChild(NodeItem,'Arma'+IntToStrZero(J+1,3));
                  with Prod.arma.Items[J] do
                   begin
                     trvwNFe.Items.AddChild(Node,'nSerie=' +nSerie);
                     trvwNFe.Items.AddChild(Node,'tpArma=' +tpArmaToStr(tpArma));
                     trvwNFe.Items.AddChild(Node,'nCano='  +nCano);
                     trvwNFe.Items.AddChild(Node,'descr='  +descr);
                    end;
                end;

               if (Prod.comb.cProdANP > 0) then
                begin
                 NodePai := trvwNFe.Items.AddChild(NodeItem,'Combustivel');
                 with Prod.comb do
                  begin
                    trvwNFe.Items.AddChild(NodePai,'cProdANP=' +IntToStr(cProdANP));
                    trvwNFe.Items.AddChild(NodePai,'CODIF='    +CODIF);
                    trvwNFe.Items.AddChild(NodePai,'qTemp='    +FloatToStr(qTemp));
                    trvwNFe.Items.AddChild(NodePai,'UFcons='    +UFcons);

                    Node := trvwNFe.Items.AddChild(NodePai,'CIDE'+IntToStrZero(I+1,3));
                    trvwNFe.Items.AddChild(Node,'qBCprod='   +FloatToStr(CIDE.qBCprod));
                    trvwNFe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(CIDE.vAliqProd));
                    trvwNFe.Items.AddChild(Node,'vCIDE='     +FloatToStr(CIDE.vCIDE));

                    Node := trvwNFe.Items.AddChild(NodePai,'ICMSComb'+IntToStrZero(I+1,3));
                    trvwNFe.Items.AddChild(Node,'vBCICMS='   +FloatToStr(ICMS.vBCICMS));
                    trvwNFe.Items.AddChild(Node,'vICMS='     +FloatToStr(ICMS.vICMS));
                    trvwNFe.Items.AddChild(Node,'vBCICMSST=' +FloatToStr(ICMS.vBCICMSST));
                    trvwNFe.Items.AddChild(Node,'vICMSST='   +FloatToStr(ICMS.vICMSST));

                    if (ICMSInter.vBCICMSSTDest>0) then
                     begin
                       Node := trvwNFe.Items.AddChild(NodePai,'ICMSInter'+IntToStrZero(I+1,3));
                       trvwNFe.Items.AddChild(Node,'vBCICMSSTDest=' +FloatToStr(ICMSInter.vBCICMSSTDest));
                       trvwNFe.Items.AddChild(Node,'vICMSSTDest='   +FloatToStr(ICMSInter.vICMSSTDest));
                     end;

                    if (ICMSCons.vBCICMSSTCons>0) then
                     begin
                       Node := trvwNFe.Items.AddChild(NodePai,'ICMSCons'+IntToStrZero(I+1,3));
                       trvwNFe.Items.AddChild(Node,'vBCICMSSTCons=' +FloatToStr(ICMSCons.vBCICMSSTCons));
                       trvwNFe.Items.AddChild(Node,'vICMSSTCons='   +FloatToStr(ICMSCons.vICMSSTCons));
                       trvwNFe.Items.AddChild(Node,'UFCons='        +ICMSCons.UFcons);
                     end;
                  end;
               end;

               with Imposto do
                begin
                   NodePai := trvwNFe.Items.AddChild(NodeItem,'Imposto');

                   if ISSQN.cSitTrib = ISSQNcSitTribVazio then
                   begin
                     Node := trvwNFe.Items.AddChild(NodePai,'ICMS');
                     with ICMS do
                      begin
                        trvwNFe.Items.AddChild(Node,'CST=' +CSTICMSToStr(CST));
                        trvwNFe.Items.AddChild(Node,'CSOSN=' +CSOSNIcmsToStr(CSOSN));
                        trvwNFe.Items.AddChild(Node,'orig='  +OrigToStr(ICMS.orig));
                        trvwNFe.Items.AddChild(Node,'modBC=' +modBCToStr(ICMS.modBC));
                        trvwNFe.Items.AddChild(Node,'pRedBC=' +FloatToStr(ICMS.pRedBC));
                        trvwNFe.Items.AddChild(Node,'vBC='   +FloatToStr(ICMS.vBC));
                        trvwNFe.Items.AddChild(Node,'pICMS=' +FloatToStr(ICMS.pICMS));
                        trvwNFe.Items.AddChild(Node,'vICMS=' +FloatToStr(ICMS.vICMS));
                        trvwNFe.Items.AddChild(Node,'modBCST='  +modBCSTToStr(ICMS.modBCST));
                        trvwNFe.Items.AddChild(Node,'pMVAST='   +FloatToStr(ICMS.pMVAST));
                        trvwNFe.Items.AddChild(Node,'pRedBCST=' +FloatToStr(ICMS.pRedBCST));
                        trvwNFe.Items.AddChild(Node,'vBCST='    +FloatToStr(ICMS.vBCST));
                        trvwNFe.Items.AddChild(Node,'pICMSST='  +FloatToStr(ICMS.pICMSST));
                        trvwNFe.Items.AddChild(Node,'vICMSST='  +FloatToStr(ICMS.vICMSST));
                        trvwNFe.Items.AddChild(Node,'vBCSTRet='   +FloatToStr(ICMS.vBCSTRet));
                        trvwNFe.Items.AddChild(Node,'vICMSSTRet=' +FloatToStr(ICMS.vICMSSTRet));
                        trvwNFe.Items.AddChild(Node,'pCredSN='   +FloatToStr(ICMS.pCredSN));
                        trvwNFe.Items.AddChild(Node,'vCredICMSSN='   +FloatToStr(ICMS.vCredICMSSN));
                      end;

                      Node := trvwNFe.Items.AddChild(NodePai,'ICMSUFDest');
                      with ICMSUFDest do
                      begin
                        trvwNFe.Items.AddChild(Node,'vBCUFDest='   +FloatToStr(vBCUFDest));
                        trvwNFe.Items.AddChild(Node,'pFCPUFDest='   +FloatToStr(pFCPUFDest));
                        trvwNFe.Items.AddChild(Node,'pICMSUFDest='   +FloatToStr(pICMSUFDest));
                        trvwNFe.Items.AddChild(Node,'pICMSInter='   +FloatToStr(pICMSInter));
                        trvwNFe.Items.AddChild(Node,'pICMSInterPart='   +FloatToStr(pICMSInterPart));
                        trvwNFe.Items.AddChild(Node,'vFCPUFDest='   +FloatToStr(vFCPUFDest));
                        trvwNFe.Items.AddChild(Node,'vICMSUFDest='   +FloatToStr(vICMSUFDest));
                        trvwNFe.Items.AddChild(Node,'vICMSUFRemet='   +FloatToStr(vICMSUFRemet));
                      end;
                   end
                   else
                   begin
                     Node := trvwNFe.Items.AddChild(NodePai,'ISSQN');
                     with ISSQN do
                      begin
                        trvwNFe.Items.AddChild(Node,'vBC='       +FloatToStr(vBC));
                        trvwNFe.Items.AddChild(Node,'vAliq='     +FloatToStr(vAliq));
                        trvwNFe.Items.AddChild(Node,'vISSQN='    +FloatToStr(vISSQN));
                        trvwNFe.Items.AddChild(Node,'cMunFG='    +IntToStr(cMunFG));
                        trvwNFe.Items.AddChild(Node,'cListServ=' +cListServ);
                      end;
                   end;

                   if (IPI.vBC > 0) then
                    begin
                      Node := trvwNFe.Items.AddChild(NodePai,'IPI');
                      with IPI do
                       begin
                         trvwNFe.Items.AddChild(Node,'CST='       +CSTIPIToStr(CST));
                         trvwNFe.Items.AddChild(Node,'clEnq='    +clEnq);
                         trvwNFe.Items.AddChild(Node,'CNPJProd=' +CNPJProd);
                         trvwNFe.Items.AddChild(Node,'cSelo='    +cSelo);
                         trvwNFe.Items.AddChild(Node,'qSelo='    +IntToStr(qSelo));
                         trvwNFe.Items.AddChild(Node,'cEnq='     +cEnq);

                         trvwNFe.Items.AddChild(Node,'vBC='    +FloatToStr(vBC));
                         trvwNFe.Items.AddChild(Node,'qUnid='  +FloatToStr(qUnid));
                         trvwNFe.Items.AddChild(Node,'vUnid='  +FloatToStr(vUnid));
                         trvwNFe.Items.AddChild(Node,'pIPI='   +FloatToStr(pIPI));
                         trvwNFe.Items.AddChild(Node,'vIPI='   +FloatToStr(vIPI));
                       end;
                    end;

                   if (II.vBc > 0) then
                    begin
                      Node := trvwNFe.Items.AddChild(NodePai,'II');
                      with II do
                       begin
                         trvwNFe.Items.AddChild(Node,'vBc='      +FloatToStr(vBc));
                         trvwNFe.Items.AddChild(Node,'vDespAdu=' +FloatToStr(vDespAdu));
                         trvwNFe.Items.AddChild(Node,'vII='      +FloatToStr(vII));
                         trvwNFe.Items.AddChild(Node,'vIOF='     +FloatToStr(vIOF));
                       end;
                    end;

                   Node := trvwNFe.Items.AddChild(NodePai,'PIS');
                   with PIS do
                    begin
                      trvwNFe.Items.AddChild(Node,'CST=' +CSTPISToStr(CST));

                      if (CST = pis01) or (CST = pis02) then
                       begin
                         trvwNFe.Items.AddChild(Node,'vBC='  +FloatToStr(PIS.vBC));
                         trvwNFe.Items.AddChild(Node,'pPIS=' +FloatToStr(PIS.pPIS));
                         trvwNFe.Items.AddChild(Node,'vPIS=' +FloatToStr(PIS.vPIS));
                       end
                      else if CST = pis03 then
                       begin
                         trvwNFe.Items.AddChild(Node,'qBCProd='   +FloatToStr(PIS.qBCProd));
                         trvwNFe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(PIS.vAliqProd));
                         trvwNFe.Items.AddChild(Node,'vPIS='      +FloatToStr(PIS.vPIS));
                       end
                      else if CST = pis99 then
                       begin
                         trvwNFe.Items.AddChild(Node,'vBC='       +FloatToStr(PIS.vBC));
                         trvwNFe.Items.AddChild(Node,'pPIS='      +FloatToStr(PIS.pPIS));
                         trvwNFe.Items.AddChild(Node,'qBCProd='   +FloatToStr(PIS.qBCProd));
                         trvwNFe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(PIS.vAliqProd));
                         trvwNFe.Items.AddChild(Node,'vPIS='      +FloatToStr(PIS.vPIS));
                       end;
                    end;

                   if (PISST.vBc>0) then
                    begin
                      Node := trvwNFe.Items.AddChild(NodePai,'PISST');
                      with PISST do
                       begin
                         trvwNFe.Items.AddChild(Node,'vBc='       +FloatToStr(vBc));
                         trvwNFe.Items.AddChild(Node,'pPis='      +FloatToStr(pPis));
                         trvwNFe.Items.AddChild(Node,'qBCProd='   +FloatToStr(qBCProd));
                         trvwNFe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(vAliqProd));
                         trvwNFe.Items.AddChild(Node,'vPIS='      +FloatToStr(vPIS));
                       end;
                      end;

                   Node := trvwNFe.Items.AddChild(NodePai,'COFINS');
                   with COFINS do
                    begin
                      trvwNFe.Items.AddChild(Node,'CST=' +CSTCOFINSToStr(CST));

                      if (CST = cof01) or (CST = cof02)   then
                       begin
                         trvwNFe.Items.AddChild(Node,'vBC='     +FloatToStr(COFINS.vBC));
                         trvwNFe.Items.AddChild(Node,'pCOFINS=' +FloatToStr(COFINS.pCOFINS));
                         trvwNFe.Items.AddChild(Node,'vCOFINS=' +FloatToStr(COFINS.vCOFINS));
                       end
                      else if CST = cof03 then
                       begin
                         trvwNFe.Items.AddChild(Node,'qBCProd='   +FloatToStr(COFINS.qBCProd));
                         trvwNFe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(COFINS.vAliqProd));
                         trvwNFe.Items.AddChild(Node,'vCOFINS='   +FloatToStr(COFINS.vCOFINS));
                       end
                      else if CST = cof99 then
                       begin
                         trvwNFe.Items.AddChild(Node,'vBC='       +FloatToStr(COFINS.vBC));
                         trvwNFe.Items.AddChild(Node,'pCOFINS='   +FloatToStr(COFINS.pCOFINS));
                         trvwNFe.Items.AddChild(Node,'qBCProd='   +FloatToStr(COFINS.qBCProd));
                         trvwNFe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(COFINS.vAliqProd));
                         trvwNFe.Items.AddChild(Node,'vCOFINS='   +FloatToStr(COFINS.vCOFINS));
                       end;
                    end;

                   if (COFINSST.vBC > 0) then
                    begin
                      Node := trvwNFe.Items.AddChild(NodePai,'COFINSST');
                      with COFINSST do
                       begin
                         trvwNFe.Items.AddChild(Node,'vBC='       +FloatToStr(vBC));
                         trvwNFe.Items.AddChild(Node,'pCOFINS='   +FloatToStr(pCOFINS));
                         trvwNFe.Items.AddChild(Node,'qBCProd='   +FloatToStr(qBCProd));
                         trvwNFe.Items.AddChild(Node,'vAliqProd=' +FloatToStr(vAliqProd));
                         trvwNFe.Items.AddChild(Node,'vCOFINS='   +FloatToStr(vCOFINS));
                       end;
                    end;
                end;
             end;
          end;

       NodePai := trvwNFe.Items.AddChild(Nota,'Total');
       Node := trvwNFe.Items.AddChild(NodePai,'ICMSTot');
       trvwNFe.Items.AddChild(Node,'vBC='     +FloatToStr(Total.ICMSTot.vBC));
       trvwNFe.Items.AddChild(Node,'vICMS='   +FloatToStr(Total.ICMSTot.vICMS));
       trvwNFe.Items.AddChild(Node,'vBCST='   +FloatToStr(Total.ICMSTot.vBCST));
       trvwNFe.Items.AddChild(Node,'vST='     +FloatToStr(Total.ICMSTot.vST));
       trvwNFe.Items.AddChild(Node,'vProd='   +FloatToStr(Total.ICMSTot.vProd));
       trvwNFe.Items.AddChild(Node,'vFrete='  +FloatToStr(Total.ICMSTot.vFrete));
       trvwNFe.Items.AddChild(Node,'vSeg='    +FloatToStr(Total.ICMSTot.vSeg));
       trvwNFe.Items.AddChild(Node,'vDesc='   +FloatToStr(Total.ICMSTot.vDesc));
       trvwNFe.Items.AddChild(Node,'vII='     +FloatToStr(Total.ICMSTot.vII));
       trvwNFe.Items.AddChild(Node,'vIPI='    +FloatToStr(Total.ICMSTot.vIPI));
       trvwNFe.Items.AddChild(Node,'vPIS='    +FloatToStr(Total.ICMSTot.vPIS));
       trvwNFe.Items.AddChild(Node,'vCOFINS=' +FloatToStr(Total.ICMSTot.vCOFINS));
       trvwNFe.Items.AddChild(Node,'vOutro='  +FloatToStr(Total.ICMSTot.vOutro));
       trvwNFe.Items.AddChild(Node,'vNF='     +FloatToStr(Total.ICMSTot.vNF));
       trvwNFe.Items.AddChild(Node,'vFCPUFDest='   +FloatToStr(Total.ICMSTot.vFCPUFDest));
       trvwNFe.Items.AddChild(Node,'vICMSUFDest='  +FloatToStr(Total.ICMSTot.vICMSUFDest));
       trvwNFe.Items.AddChild(Node,'vICMSUFRemet=' +FloatToStr(Total.ICMSTot.vICMSUFRemet));

       if Total.ISSQNtot.vServ > 0 then
        begin
          Node := trvwNFe.Items.AddChild(NodePai,'ISSQNtot');
          trvwNFe.Items.AddChild(Node,'vServ='   +FloatToStr(Total.ISSQNtot.vServ));
          trvwNFe.Items.AddChild(Node,'vBC='     +FloatToStr(Total.ISSQNTot.vBC));
          trvwNFe.Items.AddChild(Node,'vISS='    +FloatToStr(Total.ISSQNTot.vISS));
          trvwNFe.Items.AddChild(Node,'vPIS='    +FloatToStr(Total.ISSQNTot.vPIS));
          trvwNFe.Items.AddChild(Node,'vCOFINS=' +FloatToStr(Total.ISSQNTot.vCOFINS));
        end;

       Node := trvwNFe.Items.AddChild(NodePai,'retTrib');
       trvwNFe.Items.AddChild(Node,'vRetPIS='   +FloatToStr(Total.retTrib.vRetPIS));
       trvwNFe.Items.AddChild(Node,'vRetCOFINS='+FloatToStr(Total.retTrib.vRetCOFINS));
       trvwNFe.Items.AddChild(Node,'vRetCSLL='  +FloatToStr(Total.retTrib.vRetCSLL));
       trvwNFe.Items.AddChild(Node,'vBCIRRF='   +FloatToStr(Total.retTrib.vBCIRRF));
       trvwNFe.Items.AddChild(Node,'vIRRF='     +FloatToStr(Total.retTrib.vIRRF));
       trvwNFe.Items.AddChild(Node,'vBCRetPrev='+FloatToStr(Total.retTrib.vBCRetPrev));
       trvwNFe.Items.AddChild(Node,'vRetPrev='  +FloatToStr(Total.retTrib.vRetPrev));

       NodePai := trvwNFe.Items.AddChild(Nota,'Transp');
       Node := trvwNFe.Items.AddChild(NodePai,'Transporta');
       trvwNFe.Items.AddChild(Node,'modFrete=' +modFreteToStr(Transp.modFrete));
       trvwNFe.Items.AddChild(Node,'CNPJCPF='  +Transp.Transporta.CNPJCPF);
       trvwNFe.Items.AddChild(Node,'xNome='    +Transp.Transporta.xNome);
       trvwNFe.Items.AddChild(Node,'IE='       +Transp.Transporta.IE);
       trvwNFe.Items.AddChild(Node,'xEnder='   +Transp.Transporta.xEnder);
       trvwNFe.Items.AddChild(Node,'xMun='     +Transp.Transporta.xMun);
       trvwNFe.Items.AddChild(Node,'UF='       +Transp.Transporta.UF);

       Node := trvwNFe.Items.AddChild(NodePai,'retTransp');
       trvwNFe.Items.AddChild(Node,'vServ='    +FloatToStr(Transp.retTransp.vServ));
       trvwNFe.Items.AddChild(Node,'vBCRet='   +FloatToStr(Transp.retTransp.vBCRet));
       trvwNFe.Items.AddChild(Node,'pICMSRet=' +FloatToStr(Transp.retTransp.pICMSRet));
       trvwNFe.Items.AddChild(Node,'vICMSRet=' +FloatToStr(Transp.retTransp.vICMSRet));
       trvwNFe.Items.AddChild(Node,'CFOP='     +Transp.retTransp.CFOP);
       trvwNFe.Items.AddChild(Node,'cMunFG='   +FloatToStr(Transp.retTransp.cMunFG));

       Node := trvwNFe.Items.AddChild(NodePai,'veicTransp');
       trvwNFe.Items.AddChild(Node,'placa='  +Transp.veicTransp.placa);
       trvwNFe.Items.AddChild(Node,'UF='     +Transp.veicTransp.UF);
       trvwNFe.Items.AddChild(Node,'RNTC='   +Transp.veicTransp.RNTC);

       for I:=0 to Transp.Reboque.Count-1 do
        begin
          Node := trvwNFe.Items.AddChild(NodePai,'Reboque'+IntToStrZero(I+1,3));
          with Transp.Reboque.Items[I] do
           begin
             trvwNFe.Items.AddChild(Node,'placa=' +placa);
             trvwNFe.Items.AddChild(Node,'UF='    +UF);
             trvwNFe.Items.AddChild(Node,'RNTC='  +RNTC);
           end;
        end;

       for I:=0 to Transp.Vol.Count-1 do
        begin
          Node := trvwNFe.Items.AddChild(NodePai,'Volume'+IntToStrZero(I+1,3));
          with Transp.Vol.Items[I] do
           begin
             trvwNFe.Items.AddChild(Node,'qVol='  +IntToStr(qVol));
             trvwNFe.Items.AddChild(Node,'esp='   +esp);
             trvwNFe.Items.AddChild(Node,'marca=' +marca);
             trvwNFe.Items.AddChild(Node,'nVol='  +nVol);
             trvwNFe.Items.AddChild(Node,'pesoL=' +FloatToStr(pesoL));
             trvwNFe.Items.AddChild(Node,'pesoB'  +FloatToStr(pesoB));

             for J:=0 to Lacres.Count-1 do
              begin
                Node := trvwNFe.Items.AddChild(Node,'Lacre'+IntToStrZero(I+1,3)+IntToStrZero(J+1,3) );
                trvwNFe.Items.AddChild(Node,'nLacre='+Lacres.Items[J].nLacre);
              end;
           end;
        end;

       NodePai := trvwNFe.Items.AddChild(Nota,'Cobr');
       Node    := trvwNFe.Items.AddChild(NodePai,'Fat');
       trvwNFe.Items.AddChild(Node,'nFat='  +Cobr.Fat.nFat);
       trvwNFe.Items.AddChild(Node,'vOrig=' +FloatToStr(Cobr.Fat.vOrig));
       trvwNFe.Items.AddChild(Node,'vDesc=' +FloatToStr(Cobr.Fat.vDesc));
       trvwNFe.Items.AddChild(Node,'vLiq='  +FloatToStr(Cobr.Fat.vLiq));

       for I:=0 to Cobr.Dup.Count-1 do
        begin
          Node    := trvwNFe.Items.AddChild(NodePai,'Duplicata'+IntToStrZero(I+1,3));
          with Cobr.Dup.Items[I] do
           begin
             trvwNFe.Items.AddChild(Node,'nDup='  +nDup);
             trvwNFe.Items.AddChild(Node,'dVenc=' +DateToStr(dVenc));
             trvwNFe.Items.AddChild(Node,'vDup='  +FloatToStr(vDup));
           end;
        end;

       NodePai := trvwNFe.Items.AddChild(Nota,'InfAdic');
       trvwNFe.Items.AddChild(NodePai,'infCpl='     +InfAdic.infCpl);
       trvwNFe.Items.AddChild(NodePai,'infAdFisco=' +InfAdic.infAdFisco);

       for I:=0 to InfAdic.obsCont.Count-1 do
        begin
          Node := trvwNFe.Items.AddChild(NodePai,'obsCont'+IntToStrZero(I+1,3));
          with InfAdic.obsCont.Items[I] do
           begin
             trvwNFe.Items.AddChild(Node,'xCampo=' +xCampo);
             trvwNFe.Items.AddChild(Node,'xTexto=' +xTexto);
           end;
        end;

         for I:=0 to InfAdic.obsFisco.Count-1 do
          begin
            Node := trvwNFe.Items.AddChild(NodePai,'obsFisco'+IntToStrZero(I+1,3));
            with InfAdic.obsFisco.Items[I] do
             begin
                trvwNFe.Items.AddChild(Node,'xCampo=' +xCampo);
                trvwNFe.Items.AddChild(Node,'xTexto=' +xTexto);
             end;
          end;

         for I:=0 to InfAdic.procRef.Count-1 do
          begin
            Node := trvwNFe.Items.AddChild(NodePai,'procRef'+IntToStrZero(I+1,3));
            with InfAdic.procRef.Items[I] do
             begin
               trvwNFe.Items.AddChild(Node,'nProc='   +nProc);
               trvwNFe.Items.AddChild(Node,'indProc=' +indProcToStr(indProc));
             end;
          end;

         if (exporta.UFembarq <> '') then
          begin
            Node := trvwNFe.Items.AddChild(Nota,'exporta');
            trvwNFe.Items.AddChild(Node,'UFembarq='   +exporta.UFembarq);
            trvwNFe.Items.AddChild(Node,'xLocEmbarq=' +exporta.xLocEmbarq);
          end;

         if (compra.xNEmp <> '') then
          begin
            Node := trvwNFe.Items.AddChild(Nota,'compra');
            trvwNFe.Items.AddChild(Node,'xNEmp=' +compra.xNEmp);
            trvwNFe.Items.AddChild(Node,'xPed='  +compra.xPed);
            trvwNFe.Items.AddChild(Node,'xCont=' +compra.xCont);
          end;
     end;
       pgRespostas.ActivePageIndex := 3;
    end;
    *)
  end;
end;

procedure TfrmACBrNFe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName,False);
    ACBrNFe1.NotasFiscais.Imprimir;
  end;
end;

procedure TfrmACBrNFe.btnImprimirDANFCEClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    if ACBrNFe1.DANFE = ACBrNFeDANFeESCPOS1 then
      PrepararImpressao;

    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName,False);
    ACBrNFe1.NotasFiscais.Imprimir;
  end;
end;

procedure TfrmACBrNFe.btnImprimirDANFCEOfflineClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    if ACBrNFe1.DANFE = ACBrNFeDANFeESCPOS1 then
      PrepararImpressao;

    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName,False);
    ACBrNFe1.NotasFiscais.Imprimir;
  end;
end;

procedure TfrmACBrNFe.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.EventoNFe.Evento.Clear;
    ACBrNFe1.EventoNFe.LerXML(OpenDialog1.FileName);
    ACBrNFe1.ImprimirEvento;
  end;
end;

procedure TfrmACBrNFe.btnInutilizarClick(Sender: TObject);
var
  Modelo, Serie, Ano, NumeroInicial, NumeroFinal, Justificativa: String;
begin
 Ano := '';
 if not(InputQuery('WebServices Inutilização ', 'Ano',    Ano)) then
    exit;
 Modelo := '';
 if not(InputQuery('WebServices Inutilização ', 'Modelo', Modelo)) then
    exit;
 Serie := '';
 if not(InputQuery('WebServices Inutilização ', 'Serie',  Serie)) then
    exit;
 NumeroInicial := '';
 if not(InputQuery('WebServices Inutilização ', 'Número Inicial', NumeroInicial)) then
    exit;
 NumeroFinal := '';
 if not(InputQuery('WebServices Inutilização ', 'Número Final', NumeroFinal)) then
    exit;
 Justificativa := '';
 if not(InputQuery('WebServices Inutilização ', 'Justificativa', Justificativa)) then
    exit;

  ACBrNFe1.WebServices.Inutiliza(edtEmitCNPJ.Text, Justificativa, StrToInt(Ano), StrToInt(Modelo), StrToInt(Serie), StrToInt(NumeroInicial), StrToInt(NumeroFinal));

  MemoResp.Lines.Text :=  ACBrNFe1.WebServices.Inutilizacao.RetWS;
  memoRespWS.Lines.Text :=  ACBrNFe1.WebServices.Inutilizacao.RetornoWS;
  LoadXML(ACBrNFe1.WebServices.Inutilizacao.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Inutilização');
  MemoDados.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.Inutilizacao.tpAmb));
  MemoDados.Lines.Add('verAplic: ' + ACBrNFe1.WebServices.Inutilizacao.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.Inutilizacao.cStat));
  MemoDados.Lines.Add('xMotivo: ' + ACBrNFe1.WebServices.Inutilizacao.xMotivo);
  MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.Inutilizacao.cUF));
  MemoDados.Lines.Add('Ano: ' + IntToStr(ACBrNFe1.WebServices.Inutilizacao.Ano));
  MemoDados.Lines.Add('CNPJ: ' + ACBrNFe1.WebServices.Inutilizacao.CNPJ);
  MemoDados.Lines.Add('Modelo: ' + IntToStr(ACBrNFe1.WebServices.Inutilizacao.Modelo));
  MemoDados.Lines.Add('Serie: ' + IntToStr(ACBrNFe1.WebServices.Inutilizacao.Serie));
  MemoDados.Lines.Add('NumeroInicial: ' + IntToStr(ACBrNFe1.WebServices.Inutilizacao.NumeroInicial));
  MemoDados.Lines.Add('NumeroInicial: ' + IntToStr(ACBrNFe1.WebServices.Inutilizacao.NumeroFinal));
  MemoDados.Lines.Add('dhRecbto: ' + DateTimeToStr(ACBrNFe1.WebServices.Inutilizacao.dhRecbto));
  MemoDados.Lines.Add('Protocolo: ' + ACBrNFe1.WebServices.Inutilizacao.Protocolo);
end;

procedure TfrmACBrNFe.btnInutilizarImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Inutilização';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.InutNFe.LerXML(OpenDialog1.FileName);
    ACBrNFe1.ImprimirInutilizacao;
  end;
end;

procedure TfrmACBrNFe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrNFe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrNFe1.SSL.CertCertificadora);
end;

procedure TfrmACBrNFe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrNFe1.SSL do
  begin
     CarregarCertificadoPublico(MemoDados.Lines.Text);
     MemoResp.Lines.Add(CertIssuerName);
     MemoResp.Lines.Add(CertRazaoSocial);
     MemoResp.Lines.Add(CertCNPJ);
     MemoResp.Lines.Add(CertSubjectName);
     MemoResp.Lines.Add(CertNumeroSerie);

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

procedure TfrmACBrNFe.btnManifDestConfirmacaoClick(Sender: TObject);
var
  Chave, idLote, CNPJ, lMsg, Titulo: string;
  AtpEvento: TpcnTpEvento;
begin
  if Sender = btnManifDestConfirmacao then
    AtpEvento := teManifDestConfirmacao
  else if Sender = btnManifDestDesconnhecimento then
    AtpEvento := teManifDestDesconhecimento
  else if Sender = btnManifDestCiencia then
    AtpEvento := teManifDestCiencia
  else if Sender = btnManifDestOperNaoRealizada then
    AtpEvento := teManifDestOperNaoRealizada;

  Titulo := 'Manifestação Destinatario - ' + StringReplace(TpEventoToDescStr(AtpEvento),'ManifDest','',[rfReplaceAll]);

  Chave:='';
  if not(InputQuery(Titulo, 'Chave da NF-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery(Titulo, 'Numero do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := '';
  if not(InputQuery(Titulo, 'CNPJ ou CPF do autor do Evento', CNPJ)) then
     exit;

  ACBrNFe1.EventoNFe.Evento.Clear;

  with ACBrNFe1.EventoNFe.Evento.New do
  begin
    {
     Como os Eventos de Manifestação do Destinatário são enviados para o
     Ambiente Nacional, o código informado em cOrgao deve ser sempre 91
    }
    InfEvento.cOrgao   := 91;
    infEvento.chNFe    := Chave;
    infEvento.CNPJ     := CNPJ; // CNPJ/CPF do Destinatário da Mercadoria
    infEvento.dhEvento := now;
    {
     Tipos de Eventos de Manifestação do Destinatário:
       teManifDestConfirmacao      = Confirmação da Operação
       teManifDestCiencia          = Ciência da Operação
       teManifDestDesconhecimento  = Desconhecimento da Operação
       teManifDestOperNaoRealizada = Operação Não Realizada
    }
    infEvento.tpEvento := AtpEvento;

    {
     Se o tipo for: teManifDestOperNaoRealizada, informar a justificativa
    }
    if AtpEvento = teManifDestOperNaoRealizada then
      InfEvento.detEvento.xJust := 'justificativa';
  end;

  ACBrNFe1.EnviarEvento(StrToInt(idLote));

  with AcbrNFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento do
  begin
    lMsg:=
      'Id: ' + Id + #13 +
      'tpAmb: ' + TpAmbToStr(tpAmb) + #13 +
      'verAplic: ' + verAplic + #13 +
      'cOrgao: ' + IntToStr(cOrgao) + #13 +
      'cStat: ' + IntToStr(cStat) + #13 +
      'xMotivo: ' + xMotivo + #13 +
      'chNFe: ' + chNFe + #13 +
      'tpEvento: ' + TpEventoToStr(tpEvento) + #13 +
      'xEvento: ' + xEvento + #13 +
      'nSeqEvento: ' + IntToStr(nSeqEvento) + #13 +
      'CNPJDest: ' + CNPJDest + #13 +
      'emailDest: ' + emailDest + #13 +
      'dhRegEvento: ' + DateTimeToStr(dhRegEvento) + #13 +
      'nProt: ' + nProt;
  end;

  ShowMessage(lMsg);

  MemoResp.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.EnvEvento.RetornoWS;

  LoadXML(ACBrNFe1.WebServices.EnvEvento.RetornoWS, WBResposta);
end;

procedure TfrmACBrNFe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrNFe1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrNFe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrNFe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrNFe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrNFe.btnStatusServClick(Sender: TObject);
begin
  ACBrNFe1.WebServices.StatusServico.Executar;

  MemoResp.Lines.Text := ACBrNFe1.WebServices.StatusServico.RetWS;
  memoRespWS.Lines.Text := ACBrNFe1.WebServices.StatusServico.RetornoWS;
  LoadXML(ACBrNFe1.WebServices.StatusServico.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Status Serviço');
  MemoDados.Lines.Add('tpAmb: '    +TpAmbToStr(ACBrNFe1.WebServices.StatusServico.tpAmb));
  MemoDados.Lines.Add('verAplic: ' +ACBrNFe1.WebServices.StatusServico.verAplic);
  MemoDados.Lines.Add('cStat: '    +IntToStr(ACBrNFe1.WebServices.StatusServico.cStat));
  MemoDados.Lines.Add('xMotivo: '  +ACBrNFe1.WebServices.StatusServico.xMotivo);
  MemoDados.Lines.Add('cUF: '      +IntToStr(ACBrNFe1.WebServices.StatusServico.cUF));
  MemoDados.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRecbto));
  MemoDados.Lines.Add('tMed: '     +IntToStr(ACBrNFe1.WebServices.StatusServico.TMed));
  MemoDados.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRetorno));
  MemoDados.Lines.Add('xObs: '     +ACBrNFe1.WebServices.StatusServico.xObs);

  if (ACBrNFe1.Integrador= ACBrIntegrador1) then
  begin
    if (ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo <> '') then
    begin
      MemoDados.Lines.Add('[Integrador]');
      MemoDados.Lines.Add('Codigo=' + ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo);
      MemoDados.Lines.Add('Valor=' + ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Valor);

      ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Codigo := '';
      ACBrIntegrador1.ComandoIntegrador.IntegradorResposta.Valor := '';
    end;
  end;
end;

procedure TfrmACBrNFe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrNFe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrNFe1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrNFe.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg: String;
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    pgRespostas.ActivePageIndex := 0;
    MemoResp.Lines.Add('');
    MemoResp.Lines.Add('');

    if not ACBrNFe1.NotasFiscais.VerificarAssinatura(Msg) then
      MemoResp.Lines.Add('Erro: '+Msg)
    else
    begin
      MemoResp.Lines.Add('OK: Assinatura Válida');
      ACBrNFe1.SSL.CarregarCertificadoPublico( ACBrNFe1.NotasFiscais[0].NFe.signature.X509Certificate );
      MemoResp.Lines.Add('Assinado por: '+ ACBrNFe1.SSL.CertRazaoSocial);
      MemoResp.Lines.Add('CNPJ: '+ ACBrNFe1.SSL.CertCNPJ);
      MemoResp.Lines.Add('Num.Série: '+ ACBrNFe1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure TfrmACBrNFe.btnValidarRegrasNegocioClick(Sender: TObject);
var
  Msg, Tempo: String;
  Inicio: TDateTime;
  Ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);
    Inicio := Now;
    Ok := ACBrNFe1.NotasFiscais.ValidarRegrasdeNegocios(Msg);
    Tempo := FormatDateTime('hh:nn:ss:zzz', Now - Inicio);

    if not Ok then
    begin
      MemoDados.Lines.Add('Erro: ' + Msg);
      ShowMessage('Erros encontrados' + sLineBreak + 'Tempo: ' + Tempo);
    end
    else
      ShowMessage('Tudo OK' + sLineBreak + 'Tempo: ' + Tempo);
  end;
end;

procedure TfrmACBrNFe.btnValidarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFe';
  OpenDialog1.DefaultExt := '*-nfe.XML';
  OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;

  // Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrNFe1.Configuracoes.Geral.ExibirErroSchema := False;
  ACBrNFe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    ACBrNFe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, True);

    try
      ACBrNFe1.NotasFiscais.Validar;

      if ACBrNFe1.NotasFiscais.Items[0].Alertas <> '' then
        MemoDados.Lines.Add('Alertas: '+ACBrNFe1.NotasFiscais.Items[0].Alertas);

      ShowMessage('Nota Fiscal Eletrônica Valida');
    except
      on E: Exception do
      begin
        pgRespostas.ActivePage := Dados;
        MemoDados.Lines.Add('Exception: ' + E.Message);
        MemoDados.Lines.Add('Erro: ' + ACBrNFe1.NotasFiscais.Items[0].ErroValidacao);
        MemoDados.Lines.Add('Erro Completo: ' + ACBrNFe1.NotasFiscais.Items[0].ErroValidacaoCompleto);
      end;
    end;
  end;
end;

procedure TfrmACBrNFe.btSerialClick(Sender: TObject);
begin
  frmConfiguraSerial.Device.Porta        := ACBrPosPrinter1.Device.Porta;
  frmConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text;
  frmConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString;

  if frmConfiguraSerial.ShowModal = mrOk then
  begin
    cbxPorta.Text := frmConfiguraSerial.Device.Porta;
    ACBrPosPrinter1.Device.ParamsString := frmConfiguraSerial.Device.ParamsString;
  end;
end;

procedure TfrmACBrNFe.btVersaoClick(Sender: TObject);
begin
  pgRespostas.ActivePageIndex := 0;
  MemoResp.Lines.Add(ACBrNFe1.SSL.SSLCryptClass.Versao);
end;

procedure TfrmACBrNFe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrNFe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrNFe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrNFe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  J: TpcnModeloDF;
  K: TpcnVersaoDF;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  N: TACBrPosPrinterModelo;
  O: TACBrPosPaginaCodigo;
begin
  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add( GetEnumName(TypeInfo(TSSLLib), integer(T) ) );
  cbSSLLib.ItemIndex := 4;

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
  cbSSLType.ItemIndex := 5;

  cbFormaEmissao.Items.Clear;
  for I := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
     cbFormaEmissao.Items.Add( GetEnumName(TypeInfo(TpcnTipoEmissao), integer(I) ) );
  cbFormaEmissao.ItemIndex := 0;

  cbModeloDF.Items.Clear;
  for J := Low(TpcnModeloDF) to High(TpcnModeloDF) do
     cbModeloDF.Items.Add( GetEnumName(TypeInfo(TpcnModeloDF), integer(J) ) );
  cbModeloDF.ItemIndex := 0;

  cbVersaoDF.Items.Clear;
  for K := Low(TpcnVersaoDF) to High(TpcnVersaoDF) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TpcnVersaoDF), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  cbxModeloPosPrinter.Items.Clear ;
  for N := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
    cbxModeloPosPrinter.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(N) ) ) ;

  cbxPagCodigo.Items.Clear ;
  for O := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(O) ) ) ;

  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );
  ACBrPosPrinter1.Device.AcharPortasRAW( cbxPorta.Items );

  {$IfDef MSWINDOWS}
  cbxPorta.Items.Add('LPT1') ;
  cbxPorta.Items.Add('\\localhost\Epson') ;
  cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  {$EndIf}

  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;
  {$IfDef LINUX}
  cbxPorta.Items.Add('/dev/ttyS0') ;
  cbxPorta.Items.Add('/dev/ttyUSB0') ;
  cbxPorta.Items.Add('/tmp/ecf.txt') ;
  {$EndIf}

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 0;
  PageControl1.ActivePageIndex := 3;
  PageControl4.ActivePageIndex := 0;
end;

procedure TfrmACBrNFe.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(ParamStr(0), '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteInteger('Certificado', 'SSLLib',     cbSSLLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'CryptLib',   cbCryptLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'HttpLib',    cbHttpLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString( 'Certificado', 'URL',        edtURLPFX.Text);
    Ini.WriteString( 'Certificado', 'Caminho',    edtCaminho.Text);
    Ini.WriteString( 'Certificado', 'Senha',      edtSenha.Text);
    Ini.WriteString( 'Certificado', 'NumSerie',   edtNumSerie.Text);

    Ini.WriteBool(   'Geral', 'AtualizarXML',     cbxAtualizarXML.Checked);
    Ini.WriteBool(   'Geral', 'ExibirErroSchema', cbxExibirErroSchema.Checked);
    Ini.WriteString( 'Geral', 'FormatoAlerta',    edtFormatoAlerta.Text);
    Ini.WriteInteger('Geral', 'FormaEmissao',     cbFormaEmissao.ItemIndex);
    Ini.WriteInteger('Geral', 'ModeloDF',         cbModeloDF.ItemIndex);
    Ini.WriteInteger('Geral', 'VersaoDF',         cbVersaoDF.ItemIndex);
    Ini.WriteString( 'Geral', 'IdToken',          edtIdToken.Text);
    Ini.WriteString( 'Geral', 'Token',            edtToken.Text);
    Ini.WriteBool(   'Geral', 'RetirarAcentos',   cbxRetirarAcentos.Checked);
    Ini.WriteBool(   'Geral', 'Salvar',           ckSalvar.Checked);
    Ini.WriteString( 'Geral', 'PathSalvar',       edtPathLogs.Text);
    Ini.WriteString( 'Geral', 'PathSchemas',      edtPathSchemas.Text);

    Ini.WriteString( 'WebService', 'UF',         cbUF.Text);
    Ini.WriteInteger('WebService', 'Ambiente',   rgTipoAmb.ItemIndex);
    Ini.WriteBool(   'WebService', 'Visualizar', cbxVisualizar.Checked);
    Ini.WriteBool(   'WebService', 'SalvarSOAP', cbxSalvarSOAP.Checked);
    Ini.WriteBool(   'WebService', 'AjustarAut', cbxAjustarAut.Checked);
    Ini.WriteString( 'WebService', 'Aguardar',   edtAguardar.Text);
    Ini.WriteString( 'WebService', 'Tentativas', edtTentativas.Text);
    Ini.WriteString( 'WebService', 'Intervalo',  edtIntervalo.Text);
    Ini.WriteInteger('WebService', 'TimeOut',    seTimeOut.Value);
    Ini.WriteInteger('WebService', 'SSLType',    cbSSLType.ItemIndex);

    Ini.WriteString('Proxy', 'Host',  edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString('Proxy', 'User',  edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass',  edtProxySenha.Text);

    Ini.WriteBool(  'Arquivos', 'Salvar',           cbxSalvarArqs.Checked);
    Ini.WriteBool(  'Arquivos', 'PastaMensal',      cbxPastaMensal.Checked);
    Ini.WriteBool(  'Arquivos', 'AddLiteral',       cbxAdicionaLiteral.Checked);
    Ini.WriteBool(  'Arquivos', 'EmissaoPathNFe',   cbxEmissaoPathNFe.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathNFe',          edtPathNFe.Text);
    Ini.WriteString('Arquivos', 'PathInu',          edtPathInu.Text);
    Ini.WriteString('Arquivos', 'PathEvento',       edtPathEvento.Text);
    Ini.WriteString('Arquivos', 'PathPDF',          edtPathPDF.Text);

    Ini.WriteString('Emitente', 'CNPJ',        edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IE',          edtEmitIE.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia',    edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone',        edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP',         edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro',  edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero',      edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro',      edtEmitBairro.Text);
    Ini.WriteString('Emitente', 'CodCidade',   edtEmitCodCidade.Text);
    Ini.WriteString('Emitente', 'Cidade',      edtEmitCidade.Text);
    Ini.WriteString('Emitente', 'UF',          edtEmitUF.Text);
    Ini.WriteInteger('Emitente', 'CRT',        cbTipoEmpresa.ItemIndex);

    Ini.WriteString('Email', 'Host',    edtSmtpHost.Text);
    Ini.WriteString('Email', 'Port',    edtSmtpPort.Text);
    Ini.WriteString('Email', 'User',    edtSmtpUser.Text);
    Ini.WriteString('Email', 'Pass',    edtSmtpPass.Text);
    Ini.WriteString('Email', 'Assunto', edtEmailAssunto.Text);
    Ini.WriteBool(  'Email', 'SSL',     cbEmailSSL.Checked );

    StreamMemo := TMemoryStream.Create;
    mmEmailMsg.Lines.SaveToStream(StreamMemo);
    StreamMemo.Seek(0,soBeginning);

    Ini.WriteBinaryStream('Email', 'Mensagem', StreamMemo);

    StreamMemo.Free;

    Ini.WriteInteger('DANFE', 'Tipo',       rgTipoDanfe.ItemIndex);
    Ini.WriteString( 'DANFE', 'LogoMarca',  edtLogoMarca.Text);
    Ini.WriteInteger('DANFE', 'TipoDANFCE', rgDANFCE.ItemIndex);

    INI.WriteInteger('PosPrinter', 'Modelo',            cbxModeloPosPrinter.ItemIndex);
    INI.WriteString( 'PosPrinter', 'Porta',             cbxPorta.Text);
    INI.WriteInteger('PosPrinter', 'PaginaDeCodigo',    cbxPagCodigo.ItemIndex);
    INI.WriteString( 'PosPrinter', 'ParamsString',      ACBrPosPrinter1.Device.ParamsString);
    INI.WriteInteger('PosPrinter', 'Colunas',           seColunas.Value);
    INI.WriteInteger('PosPrinter', 'EspacoLinhas',      seEspLinhas.Value);
    INI.WriteInteger('PosPrinter', 'LinhasEntreCupons', seLinhasPular.Value);
    Ini.WriteBool(   'PosPrinter', 'CortarPapel',       cbCortarPapel.Checked );

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrNFe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrNFe.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(ParamStr(0), '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    cbSSLLib.ItemIndex     := Ini.ReadInteger('Certificado', 'SSLLib',     4);
    cbCryptLib.ItemIndex   := Ini.ReadInteger('Certificado', 'CryptLib',   0);
    cbHttpLib.ItemIndex    := Ini.ReadInteger('Certificado', 'HttpLib',    0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger('Certificado', 'XmlSignLib', 0);
    cbSSLLibChange(cbSSLLib);
    edtURLPFX.Text         := Ini.ReadString( 'Certificado', 'URL',        '');
    edtCaminho.Text        := Ini.ReadString( 'Certificado', 'Caminho',    '');
    edtSenha.Text          := Ini.ReadString( 'Certificado', 'Senha',      '');
    edtNumSerie.Text       := Ini.ReadString( 'Certificado', 'NumSerie',   '');

    cbxAtualizarXML.Checked     := Ini.ReadBool(   'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(   'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString( 'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger('Geral', 'FormaEmissao',     0);
    cbModeloDF.ItemIndex        := Ini.ReadInteger('Geral', 'ModeloDF',         0);

    cbVersaoDF.ItemIndex      := Ini.ReadInteger('Geral', 'VersaoDF',       0);
    edtIdToken.Text           := Ini.ReadString( 'Geral', 'IdToken',        '');
    edtToken.Text             := Ini.ReadString( 'Geral', 'Token',          '');
    ckSalvar.Checked          := Ini.ReadBool(   'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(   'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString( 'Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TpcnVersaoDF), integer(cbVersaoDF.ItemIndex) ));

    cbUF.ItemIndex := cbUF.Items.IndexOf(Ini.ReadString('WebService', 'UF', 'SP'));

    rgTipoAmb.ItemIndex   := Ini.ReadInteger('WebService', 'Ambiente',   0);
    cbxVisualizar.Checked := Ini.ReadBool(   'WebService', 'Visualizar', False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(   'WebService', 'SalvarSOAP', False);
    cbxAjustarAut.Checked := Ini.ReadBool(   'WebService', 'AjustarAut', False);
    edtAguardar.Text      := Ini.ReadString( 'WebService', 'Aguardar',   '0');
    edtTentativas.Text    := Ini.ReadString( 'WebService', 'Tentativas', '5');
    edtIntervalo.Text     := Ini.ReadString( 'WebService', 'Intervalo',  '0');
    seTimeOut.Value       := Ini.ReadInteger('WebService', 'TimeOut',    5000);
    cbSSLType.ItemIndex   := Ini.ReadInteger('WebService', 'SSLType',    5);

    edtProxyHost.Text  := Ini.ReadString('Proxy', 'Host',  '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text  := Ini.ReadString('Proxy', 'User',  '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass',  '');

    cbxSalvarArqs.Checked       := Ini.ReadBool(  'Arquivos', 'Salvar',           false);
    cbxPastaMensal.Checked      := Ini.ReadBool(  'Arquivos', 'PastaMensal',      false);
    cbxAdicionaLiteral.Checked  := Ini.ReadBool(  'Arquivos', 'AddLiteral',       false);
    cbxEmissaoPathNFe.Checked   := Ini.ReadBool(  'Arquivos', 'EmissaoPathNFe',   false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathNFe.Text             := Ini.ReadString('Arquivos', 'PathNFe',          '');
    edtPathInu.Text             := Ini.ReadString('Arquivos', 'PathInu',          '');
    edtPathEvento.Text          := Ini.ReadString('Arquivos', 'PathEvento',       '');
    edtPathPDF.Text             := Ini.ReadString('Arquivos', 'PathPDF',          '');

    edtEmitCNPJ.Text       := Ini.ReadString('Emitente', 'CNPJ',        '');
    edtEmitIE.Text         := Ini.ReadString('Emitente', 'IE',          '');
    edtEmitRazao.Text      := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text   := Ini.ReadString('Emitente', 'Fantasia',    '');
    edtEmitFone.Text       := Ini.ReadString('Emitente', 'Fone',        '');
    edtEmitCEP.Text        := Ini.ReadString('Emitente', 'CEP',         '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro',  '');
    edtEmitNumero.Text     := Ini.ReadString('Emitente', 'Numero',      '');
    edtEmitComp.Text       := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text     := Ini.ReadString('Emitente', 'Bairro',      '');
    edtEmitCodCidade.Text  := Ini.ReadString('Emitente', 'CodCidade',   '');
    edtEmitCidade.Text     := Ini.ReadString('Emitente', 'Cidade',      '');
    edtEmitUF.Text         := Ini.ReadString('Emitente', 'UF',          '');

    cbTipoEmpresa.ItemIndex := Ini.ReadInteger('Emitente', 'CRT', 2);

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

    rgTipoDanfe.ItemIndex := Ini.ReadInteger('DANFE', 'Tipo',       0);
    edtLogoMarca.Text     := Ini.ReadString( 'DANFE', 'LogoMarca',  '');
    rgDANFCE.ItemIndex    := Ini.ReadInteger('DANFE', 'TipoDANFCE', 0);

    cbxModeloPosPrinter.ItemIndex := INI.ReadInteger('PosPrinter', 'Modelo',            Integer(ACBrPosPrinter1.Modelo));
    cbxPorta.Text                 := INI.ReadString( 'PosPrinter', 'Porta',             ACBrPosPrinter1.Porta);
    cbxPagCodigo.ItemIndex        := INI.ReadInteger('PosPrinter', 'PaginaDeCodigo',    Integer(ACBrPosPrinter1.PaginaDeCodigo));
    seColunas.Value               := INI.ReadInteger('PosPrinter', 'Colunas',           ACBrPosPrinter1.ColunasFonteNormal);
    seEspLinhas.Value             := INI.ReadInteger('PosPrinter', 'EspacoLinhas',      ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value           := INI.ReadInteger('PosPrinter', 'LinhasEntreCupons', ACBrPosPrinter1.LinhasEntreCupons);
    cbCortarPapel.Checked         := Ini.ReadBool(   'PosPrinter', 'CortarPapel',       True);

    ACBrPosPrinter1.Device.ParamsString := INI.ReadString('PosPrinter', 'ParamsString', '');

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrNFe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrNFe1.Configuracoes.Certificados.URLPFX      := edtURLPFX.Text;
  ACBrNFe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrNFe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrNFe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  if cbModeloDF.ItemIndex = 0 then
    ACBrNFe1.DANFE := ACBrNFeDANFeRL1
  else
  begin
    case rgDANFCE.ItemIndex of
      0: ACBrNFe1.DANFE := ACBrNFeDANFCeFortes1;
      1: ACBrNFe1.DANFE := ACBrNFeDANFeESCPOS1;
      2: ACBrNFe1.DANFE := ACBrNFeDANFCeFortesA41;
    end;
  end;

  ACBrNFe1.SSL.DescarregarCertificado;

  with ACBrNFe1.Configuracoes.Geral do
  begin
    SSLLib        := TSSLLib(cbSSLLib.ItemIndex);
    SSLCryptLib   := TSSLCryptLib(cbCryptLib.ItemIndex);
    SSLHttpLib    := TSSLHttpLib(cbHttpLib.ItemIndex);
    SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

    AtualizarSSLLibsCombo;

    AtualizarXMLCancelado := cbxAtualizarXML.Checked;

    Salvar           := ckSalvar.Checked;
    ExibirErroSchema := cbxExibirErroSchema.Checked;
    RetirarAcentos   := cbxRetirarAcentos.Checked;
    FormatoAlerta    := edtFormatoAlerta.Text;
    FormaEmissao     := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);
    ModeloDF         := TpcnModeloDF(cbModeloDF.ItemIndex);
    VersaoDF         := TpcnVersaoDF(cbVersaoDF.ItemIndex);

    IdCSC            := edtIdToken.Text;
    CSC              := edtToken.Text;
    VersaoQRCode     := veqr200;
  end;

  with ACBrNFe1.Configuracoes.WebServices do
  begin
    UF         := cbUF.Text;
    Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    Visualizar := cbxVisualizar.Checked;
    Salvar     := cbxSalvarSOAP.Checked;

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
      edtIntervalo.Text := IntToStr(ACBrNFe1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrNFe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrNFe1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathNFe   := cbxEmissaoPathNFe.Checked;
    SalvarEvento     := cbxSalvaPathEvento.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathNFe          := edtPathNFe.Text;
    PathInu          := edtPathInu.Text;
    PathEvento       := edtPathEvento.Text;
    PathMensal       := GetPathNFe(0);
    PathSalvar       := PathMensal;
  end;

  if ACBrNFe1.DANFE <> nil then
  begin
    ACBrNFe1.DANFE.TipoDANFE := StrToTpImp(OK, IntToStr(rgTipoDanfe.ItemIndex + 1));

    {
      A Configuração abaixo utilizanda em conjunto com o TipoDANFE = tiSimplificado
      para impressão do DANFE Simplificado - Etiqueta (Fortes Report)

    ACBrNFeDANFeRL1.Etiqueta := True;
    }

    ACBrNFe1.DANFE.Logo    := edtLogoMarca.Text;
    ACBrNFe1.DANFE.PathPDF := edtPathPDF.Text;

    ACBrNFe1.DANFE.MargemDireita  := 7;
    ACBrNFe1.DANFE.MargemEsquerda := 7;
    ACBrNFe1.DANFE.MargemSuperior := 5;
    ACBrNFe1.DANFE.MargemInferior := 5;
  end;
end;

procedure TfrmACBrNFe.ConfigurarEmail;
begin
  ACBrMail1.Host := edtSmtpHost.Text;
  ACBrMail1.Port := edtSmtpPort.Text;
  ACBrMail1.Username := edtSmtpUser.Text;
  ACBrMail1.Password := edtSmtpPass.Text;
  ACBrMail1.From := edtSmtpUser.Text;
  ACBrMail1.SetSSL := cbEmailSSL.Checked; // SSL - Conexao Segura
  ACBrMail1.SetTLS := cbEmailSSL.Checked; // Auto TLS
  ACBrMail1.ReadingConfirmation := False; // Pede confirmacao de leitura do email
  ACBrMail1.UseThread := False;           // Aguarda Envio do Email(nao usa thread)
  ACBrMail1.FromName := 'Projeto ACBr - ACBrNFe';
end;

procedure TfrmACBrNFe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  ACBrUtil.FilesIO.WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      RetWS, False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');

  if ACBrNFe1.NotasFiscais.Count > 0then
    MemoResp.Lines.Add('Empresa: ' + ACBrNFe1.NotasFiscais.Items[0].NFe.Emit.xNome);
end;

procedure TfrmACBrNFe.PathClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(TEdit(Sender).Text) <= 0 then
     Dir := ApplicationPath
  else
     Dir := TEdit(Sender).Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
    TEdit(Sender).Text := Dir;
end;

procedure TfrmACBrNFe.PrepararImpressao;
begin
  ACBrPosPrinter1.Desativar;

  ACBrPosPrinter1.Modelo         := TACBrPosPrinterModelo(cbxModeloPosPrinter.ItemIndex);
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo(cbxPagCodigo.ItemIndex);
  ACBrPosPrinter1.Porta          := cbxPorta.Text;

  ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
  ACBrPosPrinter1.LinhasEntreCupons  := seLinhasPular.Value;
  ACBrPosPrinter1.EspacoEntreLinhas  := seEspLinhas.Value;
  ACBrPosPrinter1.CortaPapel         := cbCortarPapel.Checked;

  ACBrPosPrinter1.Ativar;
end;

procedure TfrmACBrNFe.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TfrmACBrNFe.sbPathInuClick(Sender: TObject);
begin
  PathClick(edtPathInu);
end;

procedure TfrmACBrNFe.sbPathNFeClick(Sender: TObject);
begin
  PathClick(edtPathNFe);
end;

procedure TfrmACBrNFe.sbPathPDFClick(Sender: TObject);
begin
  PathClick(edtPathPDF);
end;

procedure TfrmACBrNFe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ApplicationPath;

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrNFe1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrNFe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ApplicationPath;

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrNFe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
//  ASerie: String;
  AddRow: Boolean;
begin
  ACBrNFe1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrNFe1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrNFe1.SSL.ListaCertificados[I] do
    begin
//      ASerie := NumeroSerie;

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

procedure TfrmACBrNFe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrNFe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

end.
