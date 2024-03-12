{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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

unit Frm_ACBrCTe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrBase, ACBrMail, ACBrDFe, ACBrDFeReport, ACBrCTe,
  ACBrCTeDACTEClass, ACBrCTeDACTeRLClass;

type
  TfrmACBrCTe = class(TForm)
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
    sbPathCTe: TSpeedButton;
    Label35: TLabel;
    Label40: TLabel;
    sbPathInu: TSpeedButton;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathCTe: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathCTe: TEdit;
    edtPathInu: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDACTE: TRadioGroup;
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
    tsEventos: TTabSheet;
    tsInutilizacao: TTabSheet;
    btnConsultar: TButton;
    btnConsultarChave: TButton;
    btnConsCad: TButton;
    btnConsultarRecibo: TButton;
    btnInutilizar: TButton;
    btnInutilizarImprimir: TButton;
    btnValidarRegrasNegocio: TButton;
    btnGerarXML: TButton;
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
    ACBrMail1: TACBrMail;
    OpenDialog1: TOpenDialog;
    btnStatusServ: TButton;
    ACBrCTe1: TACBrCTe;
    ACBrCTeDACTeRL1: TACBrCTeDACTeRL;
    btnCriarEnviarSincrono: TButton;
    btnCompEntr: TButton;
    btnCancEntr: TButton;
    btnEnvEPEC: TButton;
    btnPrestacaoDesacordo: TButton;
    btnGerarPDFEvento: TButton;
    btnGerarPDFInut: TButton;
    btnDistrDFePorNSU: TButton;
    btnDistrDFePorChave: TButton;
    btnInsucessoEntrega: TButton;
    btnCancInsuc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathCTeClick(Sender: TObject);
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
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure btnStatusServClick(Sender: TObject);
    procedure btnGerarXMLClick(Sender: TObject);
    procedure btnCarregarXMLEnviarClick(Sender: TObject);
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
    procedure ACBrCTe1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure ACBrCTe1StatusChange(Sender: TObject);
    procedure btnCriarEnviarSincronoClick(Sender: TObject);
    procedure btnCompEntrClick(Sender: TObject);
    procedure btnCancEntrClick(Sender: TObject);
    procedure btnEnvEPECClick(Sender: TObject);
    procedure btnPrestacaoDesacordoClick(Sender: TObject);
    procedure btnGerarPDFEventoClick(Sender: TObject);
    procedure btnGerarPDFInutClick(Sender: TObject);
    procedure btnDistrDFePorNSUClick(Sender: TObject);
    procedure btnDistrDFePorChaveClick(Sender: TObject);
    procedure btnInsucessoEntregaClick(Sender: TObject);
    procedure btnCancInsucClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    procedure AlimentarCTe(NumDFe: String);
    procedure AlimentarCTeOS(NumDFe: String);
    procedure AlimentarGTVe(NumDFe: String);
    Procedure AlimentarComponente(NumDFe: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmACBrCTe: TfrmACBrCTe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrUtil.XMLHTML,
  pcnAuxiliar, pcteCTe, pcnConversao, pcteConversaoCTe, pcnRetConsReciDFe,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  ACBrCTeConhecimentos, ACBrCTeConfiguracoes,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrCTe }

procedure TfrmACBrCTe.ACBrCTe1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmACBrCTe.ACBrCTe1StatusChange(Sender: TObject);
begin
  case ACBrCTe1.Status of
    stCTeIdle:
      begin
        if ( frmStatus <> nil ) then
          frmStatus.Hide;
      end;

    stCTeStatusServico:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados da CTe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeRetRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Recebendo dados da CTe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeConsulta:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando CTe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeCancelamento:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando cancelamento de CTe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeInutilizacao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando pedido de Inutilização...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeRecibo:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando Recibo de Lote...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeCadastro:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando Cadastro...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeEmail:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Email...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeCCe:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Carta de Correção...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCTeEvento:
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

procedure TfrmACBrCTe.AlimentarComponente(NumDFe: String);
begin
  ACBrCTe1.Conhecimentos.Clear;

  case ACBrCTe1.Configuracoes.Geral.ModeloDF of
    moCTeOS: AlimentarCTeOS(NumDFe);
    moGTVe: AlimentarGTVe(NumDFe);
  else
    AlimentarCTe(NumDFe);
  end;
end;

procedure TfrmACBrCTe.AlimentarCTeOS(NumDFe: String);
begin
  //CTeOS
  with ACBrCTe1.Conhecimentos.Add.CTe do
  begin
    case cbVersaoDF.ItemIndex of
      0: infCTe.versao := 2.0;
      1: infCTe.versao := 3.0;
      2: infCTe.versao := 4.0;
    end;

    Ide.cUF    := UFtoCUF(edtEmitUF.Text);
    Ide.CFOP   := 6932;
    Ide.natOp  := 'PRESTACAO SERVICO TRANSPORTE INICIO OUTRA UF FORA DO ESTADO';
    Ide.modelo := 67;
    Ide.serie  := 1;
    Ide.nCT    := StrToInt(NumDFe);
    Ide.cCT    := GerarCodigoDFe(Ide.nCT);
    Ide.dhEmi  := Now;
    Ide.tpImp  := tiRetrato;
    Ide.tpEmis := teNormal;

    if rgTipoAmb.ItemIndex = 0 then
      Ide.tpAmb := taProducao
    else
      Ide.tpAmb := taHomologacao;

    Ide.tpCTe     := tcNormal;
    Ide.procEmi   := peAplicativoContribuinte;
    Ide.verProc   := '3.0';
    Ide.cMunEnv   := StrToInt(edtEmitCodCidade.Text);
    Ide.xMunEnv   := Trim(edtEmitCidade.Text);
    Ide.UFEnv     := Trim(edtEmitUF.Text);
    Ide.modal     := mdRodoviario;
    Ide.tpServ    := tsTranspValores;
    ide.indIEToma := inContribuinte;
    Ide.cMunIni   := 3119401;
    Ide.xMunIni   := 'CORONEL FABRICIANO';
    Ide.UFIni     := 'MG';
    Ide.cMunFim   := 2900207;
    Ide.xMunFim   := 'ABARE';
    Ide.UFFim     := 'BA';

    {Dados do Percurso}
    //ide.infPercurso.Add.UFPer := 'PR';

    {Dados do Emitente}
    Emit.CRT               := crtRegimeNormal; {Obrigatório na versão 4.00}
    Emit.CNPJ              := Trim(edtEmitCNPJ.Text);
    Emit.IE                := Trim(edtEmitIE.Text);
    Emit.xNome             := Trim(edtEmitRazao.Text);
    Emit.xFant             := Trim(edtEmitFantasia.Text);
    Emit.enderEmit.xLgr    := Trim(edtEmitLogradouro.Text);
    Emit.enderEmit.nro     := Trim(edtEmitNumero.Text);
    Emit.enderEmit.xCpl    := Trim(edtEmitComp.Text);
    Emit.enderEmit.xBairro := Trim(edtEmitBairro.Text);
    Emit.enderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.enderEmit.xMun    := Trim(edtEmitCidade.Text);
    Emit.enderEmit.CEP     := StrToInt(edtEmitCEP.Text);
    Emit.enderEmit.UF      := Trim(edtEmitUF.Text);
    Emit.enderEmit.fone    := Trim(edtEmitFone.Text);

    //Adiciona dados do tomador do serviço
    toma.CNPJCPF           := '10242141000174';
    toma.IE                := '0010834420031';
    toma.xNome             := 'ACOUGUE E SUPERMERCADO SOUZA LTDA';
    toma.xFant             := '';
    toma.fone              := '';
    toma.enderToma.xLgr    := 'RUA BELO HORIZONTE';
    toma.enderToma.nro     := '614';
    toma.enderToma.xCpl    := 'N D';
    toma.enderToma.xBairro := 'CALADINA';
    toma.enderToma.cMun    := 3119401;
    toma.enderToma.xMun    := 'CORONEL FABRICIANO';
    toma.enderToma.CEP     := 35171167;
    toma.enderToma.UF      := 'MG';
    toma.enderToma.cPais   := 1058;
    toma.enderToma.xPais   := 'BRASIL';
    toma.email             := '';

    {Carrega valores da prestacao de servico}
    vPrest.vTPrest := 100.00;
    vPrest.vRec    := 100.00;

    {Carrega componentes do valor da prestacao}
    with vPrest.comp.New do
    begin
      xNome := 'Componente 1';
      vComp := 30.00;
    end;

    with vPrest.comp.New do
    begin
      xNome := 'Componente 2';
      vComp := 70.00;
    end;

    {Carrega Impostos}
    //00 - Tributação Normal ICMS
    {Imp.ICMS.SituTrib    := cst00;
    Imp.ICMS.ICMS00.CST  := cst00;
    Imp.ICMS.ICMS00.vBC  := Impostos.Vbc;
    Imp.ICMS.ICMS00.pICMS:= Impostos.Picms;
    Imp.ICMS.ICMS00.vICMS:= Impostos.Vicms;

    //40 - ICMS Isento
    Imp.ICMS.SituTrib  := cst40;
    Imp.ICMS.ICMS45.CST:= cst40;

    //41 - ICMS não Tributada
    Imp.ICMS.SituTrib  := cst41;
    Imp.ICMS.ICMS45.CST:= cst41;

    //51 - ICMS diferido
    Imp.ICMS.SituTrib  := cst51;
    Imp.ICMS.ICMS45.CST:= cst51; }

    //90 - ICMS Outros
    if Emit.enderEmit.UF = Rem.enderReme.UF then
    begin
      Imp.ICMS.SituTrib     := cst90;
      Imp.ICMS.ICMS90.CST   := cst90;
      Imp.ICMS.ICMS90.pRedBC:= 10.00;
      Imp.ICMS.ICMS90.vBC   := 100.00;
      Imp.ICMS.ICMS90.pICMS := 7.00;
      Imp.ICMS.ICMS90.vICMS := 6.30;
      Imp.ICMS.ICMS90.vCred := 0.00;
    end
    else
    begin
      Imp.ICMS.SituTrib                  := cstICMSOutraUF;
      Imp.ICMS.ICMSOutraUF.CST           := cstICMSOutraUF; // ICMS Outros
      Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := 0;
      Imp.ICMS.ICMSOutraUF.vBCOutraUF    := 100.00;
      Imp.ICMS.ICMSOutraUF.pICMSOutraUF  := 7.00;
      Imp.ICMS.ICMSOutraUF.vICMSOutraUF  := 7.00;
    end;

    //SN - Simples Nacional
    {Imp.ICMS.SituTrib     := cstICMSSN;
    Imp.ICMS.ICMSSN.indSN := 1;}

    Imp.infAdFisco := 'Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preço deste servico é de R$ 17,00 (17,00%) Fonte: IBPT';
    imp.vTotTrib   := 17.00;

    //Impostos federais
    imp.infTribFed.vPIS    := 1;
    imp.infTribFed.vCOFINS := 2;
    imp.infTribFed.vIR     := 3;
    imp.infTribFed.vINSS   := 4.00;
    imp.infTribFed.vCSLL   := 5;

    {Carrega as informacoes CTe Normal}
    infCTeNorm.infServico.xDescServ := 'TEJEJRBEFR ERFERF TESTET JFREJ';
    infCTeNorm.infServico.qCarga    := 5000.0000;

    {Informações dos documentos referenciados}
    {with infCTeNorm.infDocRef.Add do
    begin
      nDoc     := '';
      serie    := '';
      subserie := '';
      dEmi     := Date;
      vDoc     := 0.00;
    end;}

    {Carrega informacoes do seguro}
    with infCTeNorm.Seg.New do
    begin
      respSeg := rsTomadorServico;
      xSeg    := 'TESTE';
      nApol   := '3743784738473847';
    end;

    {Carrega Informacoes do Modal}
    {Rodoviario}
    infCTeNorm.rodoOS.TAF            := '454545445454';
    infCTeNorm.rodoOS.NroRegEstadual := '';

    {Carega inf veiculos do modal rodo 0-1}
    with infCTeNorm.rodoOS.veic do
    begin
      placa    := 'MBC2448';
      RENAVAM  := '00709229895';
      UF       := 'SC';
      //Se for de Terceiro
      {Prop.CNPJCPF        := '';
      Prop.xNome          := '';
      Prop.UF             := '';
      prop.TAF            := '';
      prop.NroRegEstadual := '';
      Prop.IE             := '';
      Prop.tpProp         := tpTACAgregado;}
    end;

    {Carrega dados da CTe substituta 0-1}
    {with infCTeNorm.infCTeSub do
    begin
      chCte := '';
      //Se tomador não é Contribuinte
        tomaNaoICMS.refCteAnu := '';

      //Se tomador for Contribuinte
        case TipoDoc of //Tipo do Documento que o Tomador Emitiu para anulação de valor do Cte Anterior
          0: tomaICMS.refNFe := '';//CTe
          1: tomaICMS.refCte := '';//CTe
          2://NF
          begin
            tomaICMS.refNF.CNPJCPF  := '';
            tomaICMS.refNF.modelo   := '';
            tomaICMS.refNF.serie    := 0;
            tomaICMS.refNF.subserie := 0;
            tomaICMS.refNF.nro      := 0;
            tomaICMS.refNF.valor    := 0;
            tomaICMS.refNF.dEmi     := Date;
          end;
        end;
    end;}


    {CTe de Complemento de valor}
      {infCTe.versao = 3.0}
      //InfCTeComp.chave := '';

      {infCTe.versao = 4.0}
      //with infCteComp10.New do
      //begin
      //  chCTe  := 'chave';
      //end;

    {CTe de Anulacao de valores}
      //infCteAnu.chCTe := '';
      //infCteAnu.dEmi  := Date;

    {Lista de até 10 CNPJ/CPF de pessoas Autorizadas a baixar o xml}
    //autXML.Add.CNPJCPF := '';

    {Informações do Responsável Técnico pela emissão do DF-e}
    infRespTec.xContato := '';
    infRespTec.email    := '';
    infRespTec.fone     := '';
  end;
end;

procedure TfrmACBrCTe.AlimentarCTe(NumDFe: String);
begin
  //CTe
  with ACBrCTe1.Conhecimentos.Add.CTe do
  begin
    case cbVersaoDF.ItemIndex of
      0: infCTe.versao := 2.0;
      1: infCTe.versao := 3.0;
      2: infCTe.versao := 4.0;
    end;

    Ide.cUF    := UFtoCUF(edtEmitUF.Text);
    Ide.CFOP   := 5353;
    Ide.natOp  := 'PRESTACAO SERVICO';
    ide.forPag := fpAPagar; // fpAPagar ou fpPago
    Ide.modelo := 57;
    Ide.serie  := 1;
    Ide.nCT    := StrToInt(NumDFe);
    // Atenção o valor de cCT tem que ser um numero aleatório conforme recomendação
    // da SEFAZ, mas neste exemplo vamos atribuir o mesmo numero do CT-e.
    Ide.cCT    := GerarCodigoDFe(Ide.nCT);
    Ide.dhEmi  := Now;
    Ide.tpImp  := tiRetrato;
    Ide.tpEmis := teNormal;

    if rgTipoAmb.ItemIndex = 0 then
      Ide.tpAmb := taProducao
    else
      Ide.tpAmb := taHomologacao;

    Ide.tpCTe      := tcNormal; // tcNormal, tcComplemento, tcAnulacao, tcSubstituto
    Ide.procEmi    := peAplicativoContribuinte;
    Ide.verProc    := '3.0';
    Ide.cMunEnv    := StrToInt(edtEmitCodCidade.Text);
    Ide.xMunEnv    := Trim(edtEmitCidade.Text);
    Ide.UFEnv      := Trim(edtEmitUF.Text);
    Ide.modal      := mdRodoviario;
    Ide.tpServ     := tsNormal; // tsNormal, tsSubcontratacao, tsRedespacho, tsIntermediario
    ide.indIEToma  := inContribuinte;
    Ide.cMunIni    := 3119401;
    Ide.xMunIni    := 'CORONEL FABRICIANO';
    Ide.UFIni      := 'MG';
    Ide.cMunFim    := 2900207;
    Ide.xMunFim    := 'ABARE';
    Ide.UFFim      := 'BA';
    Ide.retira     := rtSim; // rtSim, rtNao
    Ide.xdetretira := '';

    ide.indGlobalizado := tiNao;

    {Dados do Percurso}
    (*
    with ide.infPercurso.New do
      UFPer := 'PR';
    *)

    Ide.Toma03.Toma := tmRemetente; // tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmRemetente

    {Dados do Tomador: Outros}
    Ide.Toma4.Toma    := tmOutros;
    Ide.Toma4.CNPJCPF := '10242141000174';
    Ide.Toma4.IE      := '0010834420031';
    Ide.Toma4.xNome   := 'ACOUGUE E SUPERMERCADO SOUZA LTDA';
    Ide.Toma4.xFant   := '';
    Ide.Toma4.fone    := '';

    Ide.Toma4.enderToma.xLgr    := 'RUA BELO HORIZONTE';
    Ide.Toma4.enderToma.nro     := '614';
    Ide.Toma4.enderToma.xCpl    := 'N D';
    Ide.Toma4.enderToma.xBairro := 'CALADINA';
    Ide.Toma4.enderToma.cMun    := 3119401;
    Ide.Toma4.enderToma.xMun    := 'CORONEL FABRICIANO';
    Ide.Toma4.enderToma.CEP     := 35171167;
    Ide.Toma4.enderToma.UF      := 'MG';
    Ide.Toma4.enderToma.cPais   := 1058;
    Ide.Toma4.enderToma.xPais   := 'BRASIL';
    Ide.Toma4.email             := '';

    {Informações Complementares do CTe}
    compl.xCaracAd  := 'Carac Adic';
    compl.xCaracSer := 'Carac Adicionais do Serviço';
    compl.xEmi      := 'Nome do Emitente';

    // Descricao da Origiem do Fluxo
    compl.fluxo.xOrig := '';

    (*
    with compl.fluxo.pass.New do
    begin
      xPass := 'Sigla ou código interno da Filial/Porto/Estação/Aeroporto de Passagem ';
    end;

    compl.fluxo.xDest := 'Destino';
    compl.fluxo.xRota := 'Rota';
    *)

    compl.Entrega.TipoData      := tdSemData;
    compl.Entrega.semData.tpPer := tdSemData;
    // se tdNaData ou tdAteData ou tdApartirData
//    compl.Entrega.comData.tpPer := tdNaData
//    compl.Entrega.comData.dProg := Date;
    // se tdNoPeriodo
//    compl.Entrega.noPeriodo.tpPer := tdNoPeriodo;
//    compl.Entrega.noPeriodo.dIni  := Date;
//    compl.Entrega.noPeriodo.dFim  := Date + 5;

    compl.Entrega.TipoHora      := thSemHorario;
    compl.Entrega.semHora.tpHor := thSemHorario;
    // se thNoHorario ou thAteHorario ou thApartirHorario
//    compl.Entrega.comHora.tpHor := thNoHorario;
//    compl.Entrega.comHora.hProg := Time;
    // se thNoIntervalo
    compl.Entrega.noInter.tpHor := thNoIntervalo;
    compl.Entrega.noInter.hIni  := Time;
    compl.Entrega.noInter.hFim  := Time + 60;

    // Município de origem para efeito de cálculo do frete
    compl.origCalc := 'Sao Paulo';
    // Município de destino para efeito de cálculo do frete
    compl.destCalc := 'Campinas';
    compl.xObs     := 'Observação livre';

    // Obs Estruturada do Contribuinte - Incluir se necessário
    with compl.ObsCont.New do
    begin
      xCampo := 'Nome do Campo';
      xTexto := 'Valor do Campo';
    end;

    // Obs Estruturada para o Fisco - Incluir se necessário
    with compl.ObsFisco.New do
    begin
      xCampo := 'Nome do Campo';
      xTexto := 'Valor do Campo';
    end;

    {Dados do Emitente}
    // crtNenhum, crtSimplesNacional, crtSimplesExcessoReceita, crtRegimeNormal,
    // crtSimplesNacionalMEI
    Emit.CRT               := crtRegimeNormal; {Obrigatório na versão 4.00}
    Emit.CNPJ              := Trim(edtEmitCNPJ.Text);
    Emit.IE                := Trim(edtEmitIE.Text);
    Emit.xNome             := Trim(edtEmitRazao.Text);
    Emit.xFant             := Trim(edtEmitFantasia.Text);
    Emit.enderEmit.xLgr    := Trim(edtEmitLogradouro.Text);
    Emit.enderEmit.nro     := Trim(edtEmitNumero.Text);
    Emit.enderEmit.xCpl    := Trim(edtEmitComp.Text);
    Emit.enderEmit.xBairro := Trim(edtEmitBairro.Text);
    Emit.enderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.enderEmit.xMun    := Trim(edtEmitCidade.Text);
    Emit.enderEmit.CEP     := StrToInt(edtEmitCEP.Text);
    Emit.enderEmit.UF      := Trim(edtEmitUF.Text);
    Emit.enderEmit.fone    := Trim(edtEmitFone.Text);

    {Dados do Remetente}
    Rem.CNPJCPF := '05481336000137';
    Rem.IE      := '12345678';
    Rem.xNome   := 'Nome do Remetente';
    Rem.xFant   := 'Nome Fantasia';
    Rem.fone    := '33445566';

    Rem.EnderReme.xLgr    := 'Rua 1';
    Rem.EnderReme.nro     := '200';
    Rem.EnderReme.xCpl    := '';
    Rem.EnderReme.xBairro := 'Centro';
    Rem.EnderReme.cMun    := 3554003;
    Rem.EnderReme.xMun    := 'Nome do Municipio';
    Rem.EnderReme.CEP     := 14123456;
    Rem.EnderReme.UF      := 'SP';
    Rem.EnderReme.cPais   := 1058;
    Rem.EnderReme.xPais   := 'BRASIL';

    {Dados do Expedidor - Utilizado no Redespacho Intermediário}
    (*
    Exped.CNPJCPF := '12345678000123';
    Exped.IE      := '12345678';
    Exped.xNome   := 'Nome do Expedidor';
    Exped.fone    := '33445566';

    Exped.EnderExped.xLgr    := 'Rua 1';
    Exped.EnderExped.nro     := '200';
    Exped.EnderExped.xCpl    := '';
    Exped.EnderExped.xBairro := 'Centro';
    Exped.EnderExped.cMun    := 3512345;
    Exped.EnderExped.xMun    := 'Nome do Municipio';
    Exped.EnderExped.CEP     := 14123456;
    Exped.EnderExped.UF      := 'SP';
    Exped.EnderExped.cPais   := 1058;
    Exped.EnderExped.xPais   := 'BRASIL';
    *)

    {Dados do Recebedor - Utilizado no Redespacho e Redespacho Intermediário}
    (*
    Receb.CNPJCPF := '12345678000123';
    Receb.IE      := '12345678';
    Receb.xNome   := 'Nome do Recebedor';
    Receb.fone    := '33445566';

    Receb.EnderReceb.xLgr    := 'Rua 1';
    Receb.EnderReceb.nro     := '200';
    Receb.EnderReceb.xCpl    := '';
    Receb.EnderReceb.xBairro := 'Centro';
    Receb.EnderReceb.cMun    := 3512345;
    Receb.EnderReceb.xMun    := 'Nome do Municipio';
    Receb.EnderReceb.CEP     := 14123456;
    Receb.EnderReceb.UF      := 'SP';
    Receb.EnderReceb.cPais   := 1058;
    Receb.EnderReceb.xPais   := 'BRASIL';
    *)

    {Dados do Destinatário}
    Dest.CNPJCPF := '05481336000137';
    Dest.IE      := '12345678';
    Dest.xNome   := 'Nome do Destinatário';
    Dest.fone    := '33445566';

    Dest.EnderDest.xLgr    := 'Rua 1';
    Dest.EnderDest.nro     := '200';
    Dest.EnderDest.xCpl    := '';
    Dest.EnderDest.xBairro := 'Centro';
    Dest.EnderDest.cMun    := 3554003;
    Dest.EnderDest.xMun    := 'Nome do Municipio';
    Dest.EnderDest.CEP     := 14123456;
    Dest.EnderDest.UF      := 'SP';
    Dest.EnderDest.cPais   := 1058;
    Dest.EnderDest.xPais   := 'BRASIL';

    {Carrega valores da prestacao de servico}
    vPrest.vTPrest := 100.00;
    vPrest.vRec    := 100.00;

    {Carrega componentes do valor da prestacao}
    with vPrest.comp.New do
    begin
      xNome := 'DFRNER KRTJ';
      vComp := 100.00;
    end;

    {Carrega Impostos}
    //00 - Tributação Normal ICMS
    {Imp.ICMS.SituTrib    := cst00;
    Imp.ICMS.ICMS00.CST  := cst00;
    Imp.ICMS.ICMS00.vBC  := Impostos.Vbc;
    Imp.ICMS.ICMS00.pICMS:= Impostos.Picms;
    Imp.ICMS.ICMS00.vICMS:= Impostos.Vicms;

    //40 - ICMS Isento
    Imp.ICMS.SituTrib  := cst40;
    Imp.ICMS.ICMS45.CST:= cst40;

    //41 - ICMS não Tributada
    Imp.ICMS.SituTrib  := cst41;
    Imp.ICMS.ICMS45.CST:= cst41;

    //51 - ICMS diferido
    Imp.ICMS.SituTrib  := cst51;
    Imp.ICMS.ICMS45.CST:= cst51; }

    //90 - ICMS Outros
    if Emit.enderEmit.UF = Rem.enderReme.UF then
    begin
      Imp.ICMS.SituTrib     := cst90;
      Imp.ICMS.ICMS90.CST   := cst90;
      Imp.ICMS.ICMS90.pRedBC:= 10.00;
      Imp.ICMS.ICMS90.vBC   := 100.00;
      Imp.ICMS.ICMS90.pICMS := 7.00;
      Imp.ICMS.ICMS90.vICMS := 6.30;
      Imp.ICMS.ICMS90.vCred := 0.00;
    end
    else
    begin
      Imp.ICMS.SituTrib                  := cstICMSOutraUF;
      Imp.ICMS.ICMSOutraUF.CST           := cstICMSOutraUF; // ICMS Outros
      Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := 0;
      Imp.ICMS.ICMSOutraUF.vBCOutraUF    := 100.00;
      Imp.ICMS.ICMSOutraUF.pICMSOutraUF  := 7.00;
      Imp.ICMS.ICMSOutraUF.vICMSOutraUF  := 7.00;
    end;

    //SN - Simples Nacional
    {Imp.ICMS.SituTrib     := cstICMSSN;
    Imp.ICMS.ICMSSN.indSN := 1;}

    Imp.infAdFisco := 'Lei da Transparencia: O valor aproximado de tributos incidentes sobre o preço deste servico é de R$ 17,00 (17,00%) Fonte: IBPT';
    imp.vTotTrib   := 17.00;

    {Carrega as informacoes CTe Normal}
    with infCTeNorm do
    begin
      {Informações da Carga}
      infCarga.vCarga      := 5000;
      infCarga.proPred     := 'Produto Predominante';
      // Outras Caracteristicas da Carga
      infCarga.xOutCat     := 'Pacotes';
      infCarga.vCargaAverb := 5000;

      // UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS);
      with infCarga.InfQ.New do
      begin
        cUnid  := uKg;
        tpMed  := 'Kg';
        qCarga := 10;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uUnidade;
        tpMed  := 'Caixa';
        qCarga := 5;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uM3;
        tpMed  := 'Volume';
        qCarga := 10;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uTON;
        tpMed  := 'Toneladas';
        qCarga := 1;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uLITROS;
        tpMed  := 'Litros';
        qCarga := 10;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uLITROS;
        tpMed  := 'Litros2';
        qCarga := 10;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uLITROS;
        tpMed  := 'Litros3';
        qCarga := 10;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uLITROS;
        tpMed  := 'Litros4';
        qCarga := 10;
      end;

      with infCarga.InfQ.New do
      begin
        cUnid  := uLITROS;
        tpMed  := 'Litros5';
        qCarga := 10;
      end;

      {Informações dos Documentos}
      with infDoc.infNFe.New do
        // chave da NFe emitida pelo remente da carga
        chave := '33190100127817000125650080000000581000384589';

      // o bloco de código abaixo devemos utilizar para informar documentos
      // anteriores emitidos por outras transportadoras que chamamos de
      // Expedidores
      // Devemos informar o Expedidor quando se tratar de Redespacho ou
      // Redespacho Intermediário.

      (*
      // o grupo <emiDocAnt> é uma lista que pode ter de 1-n ocorrências
      with docAnt.emiDocAnt.New do
      begin
        CNPJCPF := 'informar o CNPJ/CPF do Expedidor (transportadora anterior)';
        IE := 'informar a IE do Expedidor';
        UF := 'SP'; // UF do Expedidor
        xNome := 'Nome do Expedidor';

        // o grupo <idDocAnt> é uma lista que pode ter de 1-2 ocorrências
        with idDocAnt.New do
        begin
          {
          ATENÇÃO: no XML se existir o grupo <idDocAntPap> não pode existir o
                   grupo <idDocAntEle> e vice-versa
          }

          // Caso o documento anterior for de Papel ou seja não é um documento eletronico
          // o grupo <idDocAntPap> é uma lista que pode ter de 1-n ocorrências
          with idDocAntPap.New do
          begin
            // daATRE, daDTA, daCAI, daCCPI, daCA, daTIF, daBL
            tpDoc := daATRE;
            serie := '1';
            subser := '';
            nDoc := '123';
            dEmi := StrToDate('10/12/2020');
          end;

          // Caso o documento anterior for eletronico
          // o grupo <idDocAntEle> é uma lista que pode ter de 1-n ocorrências
          with idDocAntEle.New do
          begin
            chCTe := 'chave do CT-e emitido pelo Expedidor';
          end;
        end;
      end;
      *)

      {Carrega Informacoes do Modal}
      {Rodoviario}
      rodo.RNTRC := '12345678';

      {Ordens de Coleta associados}
      (*
      with rodo.occ.New do
      begin
        serie := '001';
        nOcc  := 1;
        dEmi  := Date;

        emiOcc.CNPJ := '12345678000123';
        emiOcc.cInt := '501';
        emiOcc.IE   := '1234567';
        emiOcc.UF   := 'SP';
        emiOcc.fone := '22334455';
      end;
      *)

      {Carrega dados da CTe substituta 0-1}
      {
      with infCTeSub do
      begin
        // Chave do CT-e Original
        chCte := '';

        // Se tomador não é Contribuinte informar a Chave do CT-e de Anulação
        refCteAnu := '';

        // Se tomador for Contribuinte, verificar o tipo de documento emitido
        // pelo tomador (NF-e, CT-e ou NF (comum de papel)

        // Tipo do Documento que o Tomador Emitiu para anulação de valor do
        // CT-e Anterior
        case TipoDoc of
          0: tomaICMS.refNFe := ''; // NF-e de Anulação de Valores
          1: tomaICMS.refCte := ''; // CT-e de Anulação emitido por outra Transportadora
          2: // NF (comum de papel)
          begin
            tomaICMS.refNF.CNPJCPF  := '';
            tomaICMS.refNF.modelo   := '';
            tomaICMS.refNF.serie    := 0;
            tomaICMS.refNF.subserie := 0;
            tomaICMS.refNF.nro      := 0;
            tomaICMS.refNF.valor    := 0;
            tomaICMS.refNF.dEmi     := Date;
          end;
        end;
      end;
      }

      with cobr do
      begin
        fat.nFat  := '123';
        fat.vOrig := 100;
        fat.vDesc := 0;
        fat.vLiq  := 100;

        with dup.New do
        begin
          nDup  := '123';
          dVenc := Date + 30;
          vDup  := 100;
        end;
      end;
    end;

    {CTe de Complemento de valor}
      {infCTe.versao = 3.0}
      //InfCTeComp.chave := '';

      {infCTe.versao = 4.0}
      //with infCteComp10.New do
      //begin
      //  chCTe  := 'chave';
      //end;


    {CTe de Anulacao de valores}
      //infCteAnu.chCTe := '';
      //infCteAnu.dEmi  := Date;

    {Lista de até 10 CNPJ/CPF de pessoas Autorizadas a baixar o xml}
    //autXML.New.CNPJCPF := '';

    {Informações do Responsável Técnico pela emissão do DF-e}
    infRespTec.xContato := '';
    infRespTec.email    := '';
    infRespTec.fone     := '';
  end;
end;

procedure TfrmACBrCTe.AlimentarGTVe(NumDFe: String);
begin
  //GTVe
  with ACBrCTe1.Conhecimentos.Add.CTe do
  begin
    case cbVersaoDF.ItemIndex of
      0: infCTe.versao := 2.0;
      1: infCTe.versao := 3.0;
      2: infCTe.versao := 4.0;
    end;

    Ide.cUF    := UFtoCUF(edtEmitUF.Text);
    Ide.CFOP   := 5353;
    Ide.natOp  := 'PRESTACAO SERVICO';
    ide.forPag := fpAPagar; // fpAPagar ou fpPago
    Ide.modelo := 64;
    Ide.serie  := 1;
    Ide.nCT    := StrToInt(NumDFe);
    // Atenção o valor de cCT tem que ser um numero aleatório conforme recomendação
    // da SEFAZ, mas neste exemplo vamos atribuir o mesmo numero do CT-e.
    Ide.cCT    := GerarCodigoDFe(Ide.nCT);
    Ide.dhEmi  := Now;
    Ide.tpImp  := tiRetrato;
    Ide.tpEmis := teNormal;

    if rgTipoAmb.ItemIndex = 0 then
      Ide.tpAmb := taProducao
    else
      Ide.tpAmb := taHomologacao;

    Ide.tpCTe      := tcGTVe;
    Ide.verProc    := '3.0';
    Ide.cMunEnv    := StrToInt(edtEmitCodCidade.Text);
    Ide.xMunEnv    := Trim(edtEmitCidade.Text);
    Ide.UFEnv      := Trim(edtEmitUF.Text);
    Ide.modal      := mdRodoviario;
    Ide.tpServ     := tsGTV;
    ide.indIEToma  := inContribuinte;

    Ide.dhSaidaOrig   := Now;
    Ide.dhChegadaDest := Now + 1;

//    Ide.cMunIni    := 3119401;
//    Ide.xMunIni    := 'CORONEL FABRICIANO';
//    Ide.UFIni      := 'MG';
//    Ide.cMunFim    := 2900207;
//    Ide.xMunFim    := 'ABARE';
//    Ide.UFFim      := 'BA';
//    Ide.retira     := rtSim; // rtSim, rtNao
//    Ide.xdetretira := '';

    {Informações Complementares do CTe}
    compl.xCaracAd  := 'Carac. Adic. Tr'; // no máximo 15 caracteres
    compl.xCaracSer := 'Carac. Adic. do Serviço';  // no máximo 30 caracteres
    compl.xEmi      := 'Nome do Emitente';
    compl.xObs     := 'Observação livre';

    // Obs Estruturada do Contribuinte - Incluir se necessário
    with compl.ObsCont.New do
    begin
      xCampo := 'Nome do Campo';
      xTexto := 'Valor do Campo';
    end;

    // Obs Estruturada para o Fisco - Incluir se necessário
    with compl.ObsFisco.New do
    begin
      xCampo := 'Nome do Campo';
      xTexto := 'Valor do Campo';
    end;

    {Dados do Emitente}
    Emit.CRT               := crtRegimeNormal; {Obrigatório na versão 4.00}
    Emit.CNPJ              := Trim(edtEmitCNPJ.Text);
    Emit.IE                := Trim(edtEmitIE.Text);
    Emit.xNome             := Trim(edtEmitRazao.Text);
    Emit.xFant             := Trim(edtEmitFantasia.Text);
    Emit.enderEmit.xLgr    := Trim(edtEmitLogradouro.Text);
    Emit.enderEmit.nro     := Trim(edtEmitNumero.Text);
    Emit.enderEmit.xCpl    := Trim(edtEmitComp.Text);
    Emit.enderEmit.xBairro := Trim(edtEmitBairro.Text);
    Emit.enderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.enderEmit.xMun    := Trim(edtEmitCidade.Text);
    Emit.enderEmit.CEP     := StrToInt(edtEmitCEP.Text);
    Emit.enderEmit.UF      := Trim(edtEmitUF.Text);
    Emit.enderEmit.fone    := Trim(edtEmitFone.Text);

    {Dados do Remetente}
    Rem.CNPJCPF := '05481336000137';
    Rem.IE      := '12345678';
    Rem.xNome   := 'Nome do Remetente';
    Rem.xFant   := 'Nome Fantasia';
    Rem.fone    := '33445566';

    Rem.EnderReme.xLgr    := 'Rua 1';
    Rem.EnderReme.nro     := '200';
    Rem.EnderReme.xCpl    := '';
    Rem.EnderReme.xBairro := 'Centro';
    Rem.EnderReme.cMun    := 3554003;
    Rem.EnderReme.xMun    := 'Nome do Municipio';
    Rem.EnderReme.CEP     := 14123456;
    Rem.EnderReme.UF      := 'SP';
    Rem.EnderReme.cPais   := 1058;
    Rem.EnderReme.xPais   := 'BRASIL';

    {Dados do Destinatário}
    Dest.CNPJCPF := '05481336000137';
    Dest.IE      := '12345678';
    Dest.xNome   := 'Nome do Destinatário';
    Dest.fone    := '33445566';

    Dest.EnderDest.xLgr    := 'Rua 1';
    Dest.EnderDest.nro     := '200';
    Dest.EnderDest.xCpl    := '';
    Dest.EnderDest.xBairro := 'Centro';
    Dest.EnderDest.cMun    := 3554003;
    Dest.EnderDest.xMun    := 'Nome do Municipio';
    Dest.EnderDest.CEP     := 14123456;
    Dest.EnderDest.UF      := 'SP';
    Dest.EnderDest.cPais   := 1058;
    Dest.EnderDest.xPais   := 'BRASIL';

    {Informações do endereço da origem do serviço}
    origem.xLgr    := '';
    origem.nro     := '';
    origem.xCpl    := '';
    origem.xBairro := '';
    origem.cMun    := 0;
    origem.xMun    := '';
    origem.CEP     := 0;
    origem.UF      := '';
    origem.fone    := '';

    {Informações do endereço do destino do serviço}
    destino.xLgr    := '';
    destino.nro     := '';
    destino.xCpl    := '';
    destino.xBairro := '';
    destino.cMun    := 0;
    destino.xMun    := '';
    destino.CEP     := 0;
    destino.UF      := '';
    destino.fone    := '';

    {Detalhamento do GTV}
    with detGTV.infEspecie.New do
    begin
      tpEspecie   := teNumerario; // Numerario = Cedulas
      vEspecie    := 5000;
      tpNumerario := tnNacional;
      xMoedaEstr  := 'Nacional';
    end;

    // Quantidade de volumes/malotes
    detGTV.qCarga := 1;

    with detGTV.infVeiculo.New do
    begin
      placa := 'XYZ1234';
      UF    := 'SP';
      RNTRC := '';
    end;

    {Lista de até 10 CNPJ/CPF de pessoas Autorizadas a baixar o xml}
    //autXML.Add.CNPJCPF := '';

    {Informações do Responsável Técnico pela emissão do DF-e}
    infRespTec.xContato := '';
    infRespTec.email    := '';
    infRespTec.fone     := '';
  end;
end;

procedure TfrmACBrCTe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrCTe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrCTe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrCTe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrCTe1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrCTe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrCTe.btnAdicionarProtocoloClick(Sender: TObject);
var
  NomeArq: String;
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Consultar;

    ShowMessage(ACBrCTe1.WebServices.Consulta.Protocolo);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.Consulta.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.Consulta.RetWS, WBResposta);
    NomeArq := OpenDialog1.FileName;

    if pos(UpperCase('-CTe.xml'), UpperCase(NomeArq)) > 0 then
       NomeArq := StringReplace(NomeArq, '-CTe.xml', '-procCTe.xml', [rfIgnoreCase]);

    ACBrCTe1.Conhecimentos.Items[0].GravarXML(NomeArq);
    ShowMessage('Arquivo gravado em: ' + NomeArq);
    memoLog.Lines.Add('Arquivo gravado em: ' + NomeArq);
  end;
end;

procedure TfrmACBrCTe.btnCancelarChaveClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, Justificativa: string;
begin
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Chave da CT-e', Chave)) then
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

  ACBrCTe1.EventoCTe.Evento.Clear;

  with ACBrCTe1.EventoCTe.Evento.New do
  begin
    infEvento.chCTe := Chave;
    infEvento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teCancelamento;
    infEvento.detEvento.xJust := Justificativa;
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrCTe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);
  (*
  ACBrCTe1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrCTe1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrCTe1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrCTe1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chCTe
  ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrCTe.btnCancelarXMLClick(Sender: TObject);
var
  idLote, vAux: String;
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    idLote := '1';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
       exit;

    if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa', vAux)) then
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

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnCancEntrClick(Sender: TObject);
var
  vProt: String;
  iLote: Integer;
begin
  OpenDialog1.Title := 'Selecione o CTe para Cancelar o Comprovante de Entrega';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    if not(InputQuery('Comprovante de Entrega:', 'Numero do Protocolo', vProt)) then
      exit;

    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      // Para o Evento de Cancelamento de Comprovante de Entrega: nSeqEvento sempre = 1
      infEvento.nSeqEvento      := 1;
      infEvento.chCTe           := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ            := edtEmitCNPJ.Text;
      infEvento.dhEvento        := now;
      infEvento.tpEvento        := teCancComprEntrega;

      infEvento.detEvento.nProt   := ACBrCTe1.Conhecimentos.Items[0].CTe.procCTe.nProt;
      infEvento.detEvento.nProtCE := vProt;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnCancInsucClick(Sender: TObject);
var
  vProt: String;
  iLote: Integer;
begin
  OpenDialog1.Title := 'Selecione o CTe para Cancelar o Insucesso de Entrega';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    if not(InputQuery('Insucesso de Entrega:', 'Numero do Protocolo', vProt)) then
      exit;

    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      // Para o Evento de Cancelamento de Insucesso de Entrega:
      // nSeqEvento sempre = 1
      infEvento.nSeqEvento := 1;
      infEvento.chCTe      := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ       := edtEmitCNPJ.Text;
      infEvento.dhEvento   := now;
      infEvento.tpEvento   := teCancInsucessoEntregaCTe;

      infEvento.detEvento.nProt   := ACBrCTe1.Conhecimentos.Items[0].CTe.procCTe.nProt;
      infEvento.detEvento.nProtIE := vProt;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnCarregarXMLEnviarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    with ACBrCTe1.Conhecimentos.Items[0].CTe do
    begin
      Emit.CNPJ              := edtEmitCNPJ.Text;
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

      Emit.IEST              := '';
    end;

    ACBrCTe1.Enviar(1, True, True);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.Enviar.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.Enviar.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.Enviar.RetWS, WBResposta);

    pgRespostas.ActivePageIndex := 1;

    with MemoDados do
    begin
      Lines.Add('');
      Lines.Add('Envio CTe');
      Lines.Add('tpAmb: '     + TpAmbToStr(ACBrCTe1.WebServices.Enviar.tpAmb));
      Lines.Add('verAplic: '  + ACBrCTe1.WebServices.Enviar.verAplic);
      Lines.Add('cStat: '     + IntToStr(ACBrCTe1.WebServices.Enviar.cStat));
      Lines.Add('xMotivo: '   + ACBrCTe1.WebServices.Enviar.xMotivo);
      Lines.Add('cUF: '       + IntToStr(ACBrCTe1.WebServices.Enviar.cUF));
      Lines.Add('xMsg: '      + ACBrCTe1.WebServices.Enviar.Msg);
      Lines.Add('Recibo: '    + ACBrCTe1.WebServices.Enviar.Recibo);
      Lines.Add('Protocolo: ' + ACBrCTe1.WebServices.Enviar.Protocolo);
    end;
  end;
end;

procedure TfrmACBrCTe.btnCartadeCorrecaoClick(Sender: TObject);
var
  vGrupo, vCampo, vConteudo, vIndice: String;
  iLote: Integer;
begin
  OpenDialog1.Title := 'Selecione o CTe a ser Corrigido por CC-e';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    if not(InputQuery('Carta de Correção do CTe:', 'Grupo', vGrupo)) then
      exit;

    if not(InputQuery('Carta de Correção do CTe:', 'Campo', vCampo)) then
      exit;

    if not(InputQuery('Carta de Correção do CTe:', 'Conteudo', vConteudo)) then
      exit;

    if not(InputQuery('Carta de Correção do CTe:', 'Indice (por padrão é 1)', vIndice)) then
      exit;

    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      // Para o Evento de CCe: nSeqEvento varia de 1 até 20 por CT-e
      infEvento.nSeqEvento      := 1;
      infEvento.chCTe           := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ            := edtEmitCNPJ.Text;
      infEvento.dhEvento        := now;
      infEvento.tpEvento        := teCCe;

      infEvento.detEvento.xCondUso := '';

      ACBrCTe1.EventoCTe.Evento.Items[0].InfEvento.detEvento.infCorrecao.Clear;

      with ACBrCTe1.EventoCTe.Evento.Items[0].InfEvento.detEvento.infCorrecao.New do
      begin
        grupoAlterado   := vGrupo;
        campoAlterado   := vCampo;
        valorAlterado   := vConteudo;
        nroItemAlterado := StrToIntDef(vIndice, 1);
      end;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrCTe1.SSL.CertCNPJ);
end;

procedure TfrmACBrCTe.btnCompEntrClick(Sender: TObject);
var
  vData, vHora, vDoc, vNome, vPathImg, vChaveNFe: String;
  iLote: Integer;
begin
  OpenDialog1.Title := 'Selecione o CTe para enviar o Comprovante de Entrega';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    if not(InputQuery('Comprovante de Entrega:', 'Data da Entrega (DD/MM/AAAA)', vData)) then
      exit;

    if not(InputQuery('Comprovante de Entrega:', 'Hora da Entrega (HH:MM:SS)', vHora)) then
      exit;

    if not(InputQuery('Comprovante de Entrega:', 'Num. Doc. de quem recebeu', vDoc)) then
      exit;

    if not(InputQuery('Comprovante de Entrega:', 'Nome de quem recebeu', vNome)) then
      exit;

    if not(InputQuery('Comprovante de Entrega:', 'Chave da NFe Entregue', vChaveNFe)) then
      exit;

    OpenDialog1.Title := 'Selecione a Imagem da Entrega';
    OpenDialog1.DefaultExt := '*.jpg';
    OpenDialog1.Filter := 'Arquivos de Imagem (*.jpg)|*.jpg|Todos os Arquivos (*.*)|*.*';
    OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

    if OpenDialog1.Execute then
      vPathImg := OpenDialog1.FileName
    else
      exit;


    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      // Para o Evento de Cancelamento: nSeqEvento sempre = 1
      infEvento.nSeqEvento      := 1;
      infEvento.chCTe           := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ            := edtEmitCNPJ.Text;
      infEvento.dhEvento        := now;
      infEvento.tpEvento        := teComprEntrega;

      infEvento.detEvento.nProt         := ACBrCTe1.Conhecimentos.Items[0].CTe.procCTe.nProt;
      infEvento.detEvento.dhEntrega     := StrToDateTime(vData + ' ' + vHora);
      infEvento.detEvento.nDoc          := vDoc;
      infEvento.detEvento.xNome         := vNome;
      infEvento.detEvento.hashEntrega   := CalcularHashArquivo(vPathImg, infEvento.chCTe);
      infEvento.detEvento.dhHashEntrega := Now;

      InfEvento.detEvento.infEntrega.Clear;

      with InfEvento.detEvento.infEntrega.New do
      begin
        chNFe := vChaveNFe;
      end;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnConsCadClick(Sender: TObject);
var
  UF, Documento: String;
  i: Integer;
begin
 if not(InputQuery('WebServices Consulta Cadastro ', 'UF do Documento a ser Consultado:',    UF)) then
    exit;

 if not(InputQuery('WebServices Consulta Cadastro ', 'Documento(CPF/CNPJ)',    Documento)) then
    exit;

  Documento :=  Trim(OnlyNumber(Documento));

  ACBrCTe1.WebServices.ConsultaCadastro.UF := UF;

  if Length(Documento) > 11 then
     ACBrCTe1.WebServices.ConsultaCadastro.CNPJ := Documento
  else
     ACBrCTe1.WebServices.ConsultaCadastro.CPF := Documento;

  ACBrCTe1.WebServices.ConsultaCadastro.Executar;

  MemoResp.Lines.Text   := ACBrCTe1.WebServices.ConsultaCadastro.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.ConsultaCadastro.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.ConsultaCadastro.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Consulta Cadastro');
  MemoDados.Lines.Add('versao: ' + ACBrCTe1.WebServices.ConsultaCadastro.versao);
  MemoDados.Lines.Add('verAplic: ' + ACBrCTe1.WebServices.ConsultaCadastro.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.cStat));
  MemoDados.Lines.Add('xMotivo: ' + ACBrCTe1.WebServices.ConsultaCadastro.xMotivo);
  MemoDados.Lines.Add('DhCons: ' + DateTimeToStr(ACBrCTe1.WebServices.ConsultaCadastro.DhCons));
  MemoDados.Lines.Add('Dados da Empresa:');

  for i := 0 to ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad.Count -1 do
  begin
    MemoDados.Lines.Add('IE: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].IE);
    MemoDados.Lines.Add('CNPJ: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CNPJ);
    MemoDados.Lines.Add('CPF: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CPF);
    MemoDados.Lines.Add('cSit: ' + IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].cSit));
    MemoDados.Lines.Add('indCredNFe: ' + IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].indCredNFe));
    MemoDados.Lines.Add('indCredCTe: ' + IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].indCredCTe));
    MemoDados.Lines.Add('Nome: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xNome);
    MemoDados.Lines.Add('Fantasia: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xFant);
    MemoDados.Lines.Add('Reg. Apuração: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xRegApur);
    MemoDados.Lines.Add('CNAE: ' + IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CNAE));
    MemoDados.Lines.Add('dIniAtiv: ' + DateToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].dIniAtiv));
    MemoDados.Lines.Add('dUltSit: ' + DateToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].dUltSit));
    MemoDados.Lines.Add('dBaixa: ' + DateToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].dBaixa));
    MemoDados.Lines.Add('IEUnica: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].IEUnica);
    MemoDados.Lines.Add('IEAtual: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].IEAtual);

    MemoDados.Lines.Add('xLgr: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xLgr);
    MemoDados.Lines.Add('nro: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].nro);
    MemoDados.Lines.Add('xCpl: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xCpl);
    MemoDados.Lines.Add('xBairro: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xBairro);
    MemoDados.Lines.Add('cMun: ' + IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].cMun));
    MemoDados.Lines.Add('xMun: ' + ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].xMun);
    MemoDados.Lines.Add('CEP: ' + IntToStr(ACBrCTe1.WebServices.ConsultaCadastro.RetConsCad.InfCad[i].CEP));
  end;
end;

procedure TfrmACBrCTe.btnConsultarChaveClick(Sender: TObject);
var
  vChave: String;
begin
  if not(InputQuery('WebServices Consultar', 'Chave do CT-e:', vChave)) then
    exit;

  ACBrCTe1.Conhecimentos.Clear;
  ACBrCTe1.WebServices.Consulta.CTeChave := vChave;
  ACBrCTe1.WebServices.Consulta.Executar;

  MemoResp.Lines.Text   := ACBrCTe1.WebServices.Consulta.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.Consulta.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.Consulta.RetWS, WBResposta);
end;

procedure TfrmACBrCTe.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Consultar;

    ShowMessage(ACBrCTe1.WebServices.Consulta.Protocolo);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.Consulta.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.Consulta.RetWS, WBResposta);
  end;
end;

procedure TfrmACBrCTe.btnConsultarReciboClick(Sender: TObject);
var
  aux: String;
begin
  if not(InputQuery('Consultar Recibo Lote', 'Número do Recibo', aux)) then
    exit;

  ACBrCTe1.WebServices.Recibo.Recibo := aux;
  ACBrCTe1.WebServices.Recibo.Executar;

  MemoResp.Lines.Text   := ACBrCTe1.WebServices.Recibo.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.Recibo.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.Recibo.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Consultar Recibo');
  MemoDados.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrCTe1.WebServices.Recibo.tpAmb));
  MemoDados.Lines.Add('versao: ' + ACBrCTe1.WebServices.Recibo.versao);
  MemoDados.Lines.Add('verAplic: ' + ACBrCTe1.WebServices.Recibo.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrCTe1.WebServices.Recibo.cStat));
  MemoDados.Lines.Add('xMotivo: ' + ACBrCTe1.WebServices.Recibo.xMotivo);
  MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrCTe1.WebServices.Recibo.cUF));
  MemoDados.Lines.Add('xMsg: ' + ACBrCTe1.WebServices.Recibo.xMsg);
  MemoDados.Lines.Add('cMsg: ' + IntToStr(ACBrCTe1.WebServices.Recibo.cMsg));
  MemoDados.Lines.Add('Recibo: ' + ACBrCTe1.WebServices.Recibo.Recibo);
end;

procedure TfrmACBrCTe.btnCriarEnviarSincronoClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  if not(InputQuery('WebServices Enviar Síncrono', 'Numero do Conhecimento', vAux)) then
    exit;

  if not(InputQuery('WebServices Enviar Síncrono', 'Numero do Lote', vNumLote)) then
    exit;

  vNumLote := OnlyNumber(vNumLote);

  if Trim(vNumLote) = '' then
  begin
    MessageDlg('Número do Lote inválido.', mtError,[mbok], 0);
    exit;
  end;

  AlimentarComponente(vAux);

  // Parâmetros do método Enviar:
  // 1o = Número do Lote
  // 2o = Se True imprime automaticamente o DACTE
  // 3o = Se True o envio é no modo Síncrono, caso contrario Assíncrono.
  // Obs: no modo Síncrono só podemos enviar UM CT-e por vez.
  ACBrCTe1.Enviar(StrToInt(vNumLote), True, True);

  MemoResp.Lines.Text   := ACBrCTe1.WebServices.Enviar.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.Enviar.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.Enviar.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  with MemoDados do
  begin
    Lines.Add('');
    Lines.Add('Envio CTe');
    Lines.Add('tpAmb: '     + TpAmbToStr(ACBrCTe1.WebServices.Enviar.tpAmb));
    Lines.Add('verAplic: '  + ACBrCTe1.WebServices.Enviar.verAplic);
    Lines.Add('cStat: '     + IntToStr(ACBrCTe1.WebServices.Enviar.cStat));
    Lines.Add('xMotivo: '   + ACBrCTe1.WebServices.Enviar.xMotivo);
    Lines.Add('cUF: '       + IntToStr(ACBrCTe1.WebServices.Enviar.cUF));
    Lines.Add('xMsg: '      + ACBrCTe1.WebServices.Enviar.Msg);
    Lines.Add('Recibo: '    + ACBrCTe1.WebServices.Enviar.Recibo);
    Lines.Add('Protocolo: ' + ACBrCTe1.WebServices.Enviar.Protocolo);
  end;
end;

procedure TfrmACBrCTe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrCTe1.SSL.CertDataVenc));
end;

procedure TfrmACBrCTe.btnDistrDFePorChaveClick(Sender: TObject);
var
  xTitulo, cUFAutor, CNPJ, Chave: string;
begin
  raise Exception.Create('Aguardando a SEFAZ implementar esse recurso já existente para a NF-e.');

  xTitulo := 'Distribuição DFe Por Chave';
  cUFAutor := '';
  if not(InputQuery(xTitulo, 'Código da UF do Autor', cUFAutor)) then
     exit;

  CNPJ := '';
  if not(InputQuery(xTitulo, 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
     exit;

  Chave := '';
  if not(InputQuery(xTitulo, 'Chave do DF-e', Chave)) then
     exit;

  ACBrCTe1.DistribuicaoDFePorChaveCTe(StrToInt(cUFAutor), CNPJ, Chave);

  MemoResp.Lines.Text := ACBrCTe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);
end;

procedure TfrmACBrCTe.btnDistrDFePorNSUClick(Sender: TObject);
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

  ACBrCTe1.DistribuicaoDFePorNSU(StrToInt(cUFAutor), CNPJ, ANSU);

  MemoResp.Lines.Text := ACBrCTe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);
end;

procedure TfrmACBrCTe.btnDistrDFePorUltNSUClick(Sender: TObject);
var
  xTitulo, cUFAutor, CNPJ, AultNSU: string;
  i: Integer;
begin
  // Opção para simular uma consulta ao WebService lendo o arquivo de retorno e populando o componente
  OpenDialog1.Title := 'Selecione um Arquivo de Distribuição para simular uma consulta ou feche para consultar o WebService';
  OpenDialog1.DefaultExt := '*-dist-dfe.XML';
  OpenDialog1.Filter := 'Arquivos Distribuição DFe (*-dist-dfe.XML)|*-dist-dfe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  // Lê o arquivo selecionado
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.Leitor.CarregarArquivo(OpenDialog1.FileName);
    ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.LerXml;

    AultNSU := ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt.ultNSU;
  end
  // Consulta o WebService
  else
  begin
    xTitulo := 'Distribuição DF-e por último NSU';

    cUFAutor := IntToStr(ACBrCTe1.Configuracoes.WebServices.UFCodigo);
    if not(InputQuery(xTitulo, 'Código da UF do Autor', cUFAutor)) then
       exit;

    CNPJ := edtEmitCNPJ.Text;
    if not(InputQuery(xTitulo, 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
       exit;

    AultNSU := '';
    if not(InputQuery(xTitulo, 'Último NSU recebido pelo ator', AultNSU)) then
       exit;

    ACBrCTe1.DistribuicaoDFePorUltNSU(StrToInt(cUFAutor), CNPJ, AultNSU);
  end;

  with ACBrCTe1.WebServices.DistribuicaoDFe.retDistDFeInt do
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
        schresCTe:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (Resumo DFe) Chave: ' + docZip[i].resDFe.chDFe);

        schprocCTe:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (CTe Completa) Chave: ' + docZip[i].resDFe.chDFe);

        schprocCTeOS:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (CTeOS Completa) Chave: ' + docZip[i].resDFe.chDFe);

        schprocGTVe:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (GTVe Completa) Chave: ' + docZip[i].resDFe.chDFe);

        schresEvento:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (Resumo Evento) Chave: ' + docZip[i].resEvento.chDFe);

        schprocEventoCTe:
          MemoDados.Lines.Add(IntToStr(i+1) + ' NSU: ' + docZip[i].NSU +
            ' (Evento Completo) ID: ' + docZip[i].procEvento.Id);
      end;
    end;
  end;

  MemoResp.Lines.Text := ACBrCTe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);

  pgRespostas.ActivePage := Dados;
end;

procedure TfrmACBrCTe.btnEnvEPECClick(Sender: TObject);
var
  vAux: String;
  iLote: Integer;
begin
  OpenDialog1.Title := 'Selecione o CTe a ser Enviado por EPEC';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    if not(InputQuery('EPEC do CTe:', 'Justificativa', vAux)) then
      exit;

    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      // Para o Evento de EPEC: nSeqEvento sempre = 1
      infEvento.nSeqEvento      := 1;
      infEvento.chCTe           := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ            := edtEmitCNPJ.Text;
      infEvento.dhEvento        := now;
      infEvento.tpEvento        := teEPEC;

      infEvento.detEvento.xJust   := ACBrCTe1.Conhecimentos.Items[0].CTe.ide.xJust;

      // Exemplo com CST = 00 (vICMS, vICMSRet ou vICMSOutraUF)
      infEvento.detEvento.vICMS   := ACBrCTe1.Conhecimentos.Items[0].CTe.imp.ICMS.ICMS00.vICMS;

      // Exemplo com CST = 60 o campo abaixo é opcional
      infEvento.detEvento.vICMSST := ACBrCTe1.Conhecimentos.Items[0].CTe.imp.ICMS.ICMS60.vICMSSTRet;

      infEvento.detEvento.vTPrest := ACBrCTe1.Conhecimentos.Items[0].CTe.vPrest.vTPrest;
      infEvento.detEvento.vCarga  := ACBrCTe1.Conhecimentos.Items[0].CTe.infCTeNorm.infCarga.vCarga;

      InfEvento.detEvento.toma    := ACBrCTe1.Conhecimentos.Items[0].CTe.ide.toma03.Toma;

      // Exemplo quando o tomador é o remetente da carga
      InfEvento.detEvento.UF      := ACBrCTe1.Conhecimentos.Items[0].CTe.rem.enderReme.UF;
      InfEvento.detEvento.CNPJCPF := ACBrCTe1.Conhecimentos.Items[0].CTe.rem.CNPJCPF;
      InfEvento.detEvento.IE      := ACBrCTe1.Conhecimentos.Items[0].CTe.rem.IE;

      InfEvento.detEvento.modal   := ACBrCTe1.Conhecimentos.Items[0].CTe.ide.modal;
      InfEvento.detEvento.UFIni   := ACBrCTe1.Conhecimentos.Items[0].CTe.ide.UFIni;
      InfEvento.detEvento.UFFim   := ACBrCTe1.Conhecimentos.Items[0].CTe.ide.UFFim;
      InfEvento.detEvento.tpCTe   := tcNormal;
      InfEvento.detEvento.dhEmi   := ACBrCTe1.Conhecimentos.Items[0].CTe.ide.dhEmi;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrCTe1.Conhecimentos.Clear;
  ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrCTe1.Conhecimentos.Items[0].EnviarEmail(Para
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

procedure TfrmACBrCTe.btnEnviarEventoEmailClick(Sender: TObject);
var
  Para: String;
  CC, Evento: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione ao Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  Evento := TStringList.Create;
  CC := TStringList.Create;
  try
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);

    ACBrCTe1.EventoCTe.Evento.Clear;
    ACBrCTe1.EventoCTe.LerXML(OpenDialog1.FileName);

    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrCTe1.EnviarEmailEvento(Para
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

procedure TfrmACBrCTe.btnGerarPDFClick(Sender: TObject);
var
  CarregarMaisXML: Boolean;
begin
	CarregarMaisXML := true;
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;
  ACBrCTe1.Conhecimentos.Clear;

  while CarregarMaisXML do
  begin
    if OpenDialog1.Execute then
      ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    CarregarMaisXML := MessageDlg('Carregar mais Conhecimentos?', mtConfirmation, mbYesNoCancel, 0) = mrYes;
  end;

  ACBrCTe1.Conhecimentos.ImprimirPDF;
end;

procedure TfrmACBrCTe.btnGerarPDFEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o CTe';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  ACBrCTe1.Conhecimentos.Clear;
  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*-procEventoCTe.xml';
  OpenDialog1.Filter := 'Arquivos Evento (*-procEventoCTe.xml)|*-procEventoCTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.EventoCTe.Evento.Clear;
    ACBrCTe1.EventoCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirEventoPDF;
  end;
end;

procedure TfrmACBrCTe.btnGerarPDFInutClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o ProcInutCTe';
  OpenDialog1.DefaultExt := '*-ProcInutCTe.xml';
  OpenDialog1.Filter := 'Arquivos ProcInutCTe (*-ProcInutCTe.xml)|*-ProcInutCTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.InutCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirInutilizacaoPDF;
  end;
end;

procedure TfrmACBrCTe.btnGerarXMLClick(Sender: TObject);
var
  vAux: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Conhecimento', vAux)) then
    exit;

  ACBrCTe1.Conhecimentos.Clear;

  AlimentarComponente(vAux);

  ACBrCTe1.Conhecimentos.Assinar;

  ACBrCTe1.Conhecimentos.Items[0].GravarXML();

  ShowMessage('Arquivo gerado em: ' + ACBrCTe1.Conhecimentos.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: ' + ACBrCTe1.Conhecimentos.Items[0].NomeArq);

  MemoResp.Lines.LoadFromFile(ACBrCTe1.Conhecimentos.Items[0].NomeArq);

  LoadXML(MemoResp.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrCTe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrCTe1.SSL.UseCertificateHTTP;
  ACBrCTe1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrCTe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrCTe1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrCTe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    ACBrCTe1.Conhecimentos.Imprimir;
  end;
end;

procedure TfrmACBrCTe.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.EventoCTe.Evento.Clear;
    ACBrCTe1.EventoCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirEvento;
  end;
end;

procedure TfrmACBrCTe.btnInsucessoEntregaClick(Sender: TObject);
var
  vData, vHora, vNumTentativa, vJustificativa, vPathImg, vChaveNFe: String;
  iLote: Integer;
begin
  OpenDialog1.Title := 'Selecione o CTe para enviar o Evento de Insucesso de Entrega';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    if not(InputQuery('Insucesso de Entrega:', 'Data da Tentativa de Entrega (DD/MM/AAAA)', vData)) then
      exit;

    if not(InputQuery('Insucesso de Entrega:', 'Hora da Tentativa de Entrega (HH:MM:SS)', vHora)) then
      exit;

    if not(InputQuery('Insucesso de Entrega:', 'Numero da Tentativa', vNumTentativa)) then
      exit;

    if not(InputQuery('Insucesso de Entrega:', 'Justificativa/Motivo (15-255)', vJustificativa)) then
      exit;

    if not(InputQuery('Insucesso de Entrega:', 'Chave da NFe Entregue', vChaveNFe)) then
      exit;

    OpenDialog1.Title := 'Selecione a Imagem da Entrega';
    OpenDialog1.DefaultExt := '*.jpg';
    OpenDialog1.Filter := 'Arquivos de Imagem (*.jpg)|*.jpg|Todos os Arquivos (*.*)|*.*';
    OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

    if OpenDialog1.Execute then
      vPathImg := OpenDialog1.FileName
    else
      exit;

    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      // Para o Evento de Cancelamento: nSeqEvento sempre = 1
      infEvento.nSeqEvento := 1;
      infEvento.chCTe := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ := edtEmitCNPJ.Text;
      infEvento.dhEvento := now;
      infEvento.tpEvento := teInsucessoEntregaCTe;

      infEvento.detEvento.nProt := ACBrCTe1.Conhecimentos.Items[0].CTe.procCTe.nProt;
      infEvento.detEvento.dhTentativaEntrega := StrToDateTime(vData + ' ' + vHora);
      infEvento.detEvento.nTentativa := StrToIntDef(vNumTentativa, 1);

      // (tmNaoEncontrado, tmRecusa, tmInexistente, tmOutro);
      InfEvento.detEvento.tpMotivo := tmNaoEncontrado;
      infEvento.detEvento.xJustMotivo := vJustificativa;
      infEvento.detEvento.hashTentativaEntrega := CalcularHashArquivo(vPathImg, infEvento.chCTe);
      infEvento.detEvento.dhHashTentativaEntrega := Now;

      InfEvento.detEvento.infEntrega.Clear;

      with InfEvento.detEvento.infEntrega.New do
      begin
        chNFe := vChaveNFe;
      end;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnInutilizarClick(Sender: TObject);
var
  Modelo, Serie, Ano, NumeroInicial, NumeroFinal, Justificativa: String;
begin
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

  ACBrCTe1.WebServices.Inutiliza(edtEmitCNPJ.Text, Justificativa, StrToInt(Ano), StrToInt(Modelo), StrToInt(Serie), StrToInt(NumeroInicial), StrToInt(NumeroFinal));

  MemoResp.Lines.Text   :=  ACBrCTe1.WebServices.Inutilizacao.RetWS;
  memoRespWS.Lines.Text :=  ACBrCTe1.WebServices.Inutilizacao.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.Inutilizacao.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Inutilização');
  MemoDados.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrCTe1.WebServices.Inutilizacao.tpAmb));
  MemoDados.Lines.Add('verAplic: ' + ACBrCTe1.WebServices.Inutilizacao.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrCTe1.WebServices.Inutilizacao.cStat));
  MemoDados.Lines.Add('xMotivo: ' + ACBrCTe1.WebServices.Inutilizacao.xMotivo);
  MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrCTe1.WebServices.Inutilizacao.cUF));
  MemoDados.Lines.Add('Ano: ' + IntToStr(ACBrCTe1.WebServices.Inutilizacao.Ano));
  MemoDados.Lines.Add('CNPJ: ' + ACBrCTe1.WebServices.Inutilizacao.CNPJ);
  MemoDados.Lines.Add('Modelo: ' + IntToStr(ACBrCTe1.WebServices.Inutilizacao.Modelo));
  MemoDados.Lines.Add('Serie: ' + IntToStr(ACBrCTe1.WebServices.Inutilizacao.Serie));
  MemoDados.Lines.Add('NumeroInicial: ' + IntToStr(ACBrCTe1.WebServices.Inutilizacao.NumeroInicial));
  MemoDados.Lines.Add('NumeroInicial: ' + IntToStr(ACBrCTe1.WebServices.Inutilizacao.NumeroFinal));
  MemoDados.Lines.Add('dhRecbto: ' + DateTimeToStr(ACBrCTe1.WebServices.Inutilizacao.dhRecbto));
  MemoDados.Lines.Add('Protocolo: ' + ACBrCTe1.WebServices.Inutilizacao.Protocolo);
end;

procedure TfrmACBrCTe.btnInutilizarImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Inutilização';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.InutCTe.LerXML(OpenDialog1.FileName);
    ACBrCTe1.ImprimirInutilizacao;
  end;
end;

procedure TfrmACBrCTe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrCTe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrCTe1.SSL.CertCertificadora);
end;

procedure TfrmACBrCTe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrCTe1.SSL do
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

procedure TfrmACBrCTe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrCTe1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrCTe.btnPrestacaoDesacordoClick(Sender: TObject);
var
  xObs, xUF, xUFOld: String;
  iLote: Integer;
begin
  {
   O Evento de Prestação de Serviço em Desacordo deve ser enviado pelo Tomador
   do serviço que pode ser tanto o Remetente quanto o Destinatário da carga.
   Sendo assim na linha abaixo que faz referencia a edtEmitCNPJ entende-se que
   o CNPJ é do tomador (emitente do evento) e não o CNPJ do emitente do CT-e.
  }
  OpenDialog1.Title := 'Selecione o CTe para enviar o Evento de Prestação de Serviço em Desacordo';
  OpenDialog1.DefaultExt := '*-cte.xml';
  OpenDialog1.Filter := 'Arquivos CTe (*-cte.xml)|*-cte.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    xObs := 'Observacao do Tomador (com no minimo 15 caracteres)';
    if not(InputQuery('Prestação de Serviço em Desacordo:', 'Observação do Tomador', xObs)) then
      exit;

    xUF := '';
    if not(InputQuery('Prestação de Serviço em Desacordo:', 'UF do Emitente do CT-e', xUF)) then
      exit;

    // Salva a UF configurada no componente
    xUFOld := ACBrCTe1.Configuracoes.WebServices.UF;
    // O evento tem que ser enviado para a UF do Emitente do CT-e
    ACBrCTe1.Configuracoes.WebServices.UF := xUF;

    ACBrCTe1.EventoCTe.Evento.Clear;

    with ACBrCTe1.EventoCTe.Evento.New do
    begin
      // Para o Evento: nSeqEvento sempre = 1
      infEvento.nSeqEvento := 1;
      // Devemos informar a UF do Emitente do CT-e
      InfEvento.cOrgao     := UFtoCUF(xUF);
      infEvento.chCTe      := Copy(ACBrCTe1.Conhecimentos.Items[0].CTe.infCTe.Id, 4, 44);
      infEvento.CNPJ       := edtEmitCNPJ.Text;
      infEvento.dhEvento   := now;
      infEvento.tpEvento   := tePrestDesacordo;

      infEvento.detEvento.xOBS  := xObs;
    end;

    iLote := 1; // Numero do Lote do Evento
    ACBrCTe1.EnviarEvento(iLote);

    // Retorna a configuração
    ACBrCTe1.Configuracoes.WebServices.UF := xUFOld;

    MemoResp.Lines.Text   := ACBrCTe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrCTe1.WebServices.EnvEvento.RetornoWS;

    LoadXML(ACBrCTe1.WebServices.EnvEvento.RetWS, WBResposta);

    ShowMessage(IntToStr(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat));
    ShowMessage(ACBrCTe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrCTe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrCTe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrCTe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrCTe.btnStatusServClick(Sender: TObject);
begin
  ACBrCTe1.WebServices.StatusServico.Executar;

  MemoResp.Lines.Text   := ACBrCTe1.WebServices.StatusServico.RetWS;
  memoRespWS.Lines.Text := ACBrCTe1.WebServices.StatusServico.RetornoWS;

  LoadXML(ACBrCTe1.WebServices.StatusServico.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Status Serviço');
  MemoDados.Lines.Add('tpAmb: '    +TpAmbToStr(ACBrCTe1.WebServices.StatusServico.tpAmb));
  MemoDados.Lines.Add('verAplic: ' +ACBrCTe1.WebServices.StatusServico.verAplic);
  MemoDados.Lines.Add('cStat: '    +IntToStr(ACBrCTe1.WebServices.StatusServico.cStat));
  MemoDados.Lines.Add('xMotivo: '  +ACBrCTe1.WebServices.StatusServico.xMotivo);
  MemoDados.Lines.Add('cUF: '      +IntToStr(ACBrCTe1.WebServices.StatusServico.cUF));
  MemoDados.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrCTe1.WebServices.StatusServico.dhRecbto));
  MemoDados.Lines.Add('tMed: '     +IntToStr(ACBrCTe1.WebServices.StatusServico.TMed));
  MemoDados.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrCTe1.WebServices.StatusServico.dhRetorno));
  MemoDados.Lines.Add('xObs: '     +ACBrCTe1.WebServices.StatusServico.xObs);
end;

procedure TfrmACBrCTe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrCTe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrCTe1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrCTe.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg: String;
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    pgRespostas.ActivePageIndex := 0;
    MemoResp.Lines.Add('');
    MemoResp.Lines.Add('');

    if not ACBrCTe1.Conhecimentos.VerificarAssinatura(Msg) then
      MemoResp.Lines.Add('Erro: '+Msg)
    else
    begin
      MemoResp.Lines.Add('OK: Assinatura Válida');
      ACBrCTe1.SSL.CarregarCertificadoPublico( ACBrCTe1.Conhecimentos[0].CTe.signature.X509Certificate );
      MemoResp.Lines.Add('Assinado por: '+ ACBrCTe1.SSL.CertRazaoSocial);
      MemoResp.Lines.Add('CNPJ: '+ ACBrCTe1.SSL.CertCNPJ);
      MemoResp.Lines.Add('Num.Série: '+ ACBrCTe1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure TfrmACBrCTe.btnValidarRegrasNegocioClick(Sender: TObject);
var
  Msg, Tempo: String;
  Inicio: TDateTime;
  Ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);
    Inicio := Now;
    Ok := ACBrCTe1.Conhecimentos.ValidarRegrasdeNegocios(Msg);
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

procedure TfrmACBrCTe.btnValidarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a CTe';
  OpenDialog1.DefaultExt := '*-CTe.XML';
  OpenDialog1.Filter := 'Arquivos CTe (*-CTe.XML)|*-CTe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrCTe1.Configuracoes.Arquivos.PathSalvar;

  // Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrCTe1.Configuracoes.Geral.ExibirErroSchema := False;
  ACBrCTe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  if OpenDialog1.Execute then
  begin
    ACBrCTe1.Conhecimentos.Clear;
    ACBrCTe1.Conhecimentos.LoadFromFile(OpenDialog1.FileName);

    try
      ACBrCTe1.Conhecimentos.Validar;

      if ACBrCTe1.Conhecimentos.Items[0].Alertas <> '' then
        MemoDados.Lines.Add('Alertas: '+ACBrCTe1.Conhecimentos.Items[0].Alertas);

      ShowMessage('Conhecimento de Transporte Eletrônico Valido');
    except
      on E: Exception do
      begin
        pgRespostas.ActivePage := Dados;
        MemoDados.Lines.Add('Exception: ' + E.Message);
        MemoDados.Lines.Add('Erro: ' + ACBrCTe1.Conhecimentos.Items[0].ErroValidacao);
        MemoDados.Lines.Add('Erro Completo: ' + ACBrCTe1.Conhecimentos.Items[0].ErroValidacaoCompleto);
      end;
    end;
  end;
end;

procedure TfrmACBrCTe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCTe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCTe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCTe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrCTe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrCTe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrCTe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCTe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  J: TModeloCTe;
  K: TVersaoCTe;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
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
  for J := Low(TModeloCTe) to High(TModeloCTe) do
     cbModeloDF.Items.Add( GetEnumName(TypeInfo(TModeloCTe), integer(J) ) );
  cbModeloDF.ItemIndex := 0;

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoCTe) to High(TVersaoCTe) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoCTe), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBrCTe.GravarConfiguracao;
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
    Ini.WriteInteger('Geral', 'ModeloDF',         cbModeloDF.ItemIndex);
    Ini.WriteInteger('Geral', 'VersaoDF',         cbVersaoDF.ItemIndex);
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
    Ini.WriteBool(  'Arquivos', 'EmissaoPathCTe',   cbxEmissaoPathCTe.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathCTe',          edtPathCTe.Text);
    Ini.WriteString('Arquivos', 'PathInu',          edtPathInu.Text);
    Ini.WriteString('Arquivos', 'PathEvento',       edtPathEvento.Text);

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

    Ini.WriteInteger('DACTE', 'Tipo',      rgTipoDaCTe.ItemIndex);
    Ini.WriteString( 'DACTE', 'LogoMarca', edtLogoMarca.Text);

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrCTe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrCTe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrCTe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrCTe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrCTe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrCTe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrCTe.LerConfiguracao;
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
    cbSSLLibChange(cbSSLLib);
    edtCaminho.Text        := Ini.ReadString( 'Certificado', 'Caminho',    '');
    edtSenha.Text          := Ini.ReadString( 'Certificado', 'Senha',      '');
    edtNumSerie.Text       := Ini.ReadString( 'Certificado', 'NumSerie',   '');

    cbxAtualizarXML.Checked     := Ini.ReadBool(   'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(   'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString( 'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger('Geral', 'FormaEmissao',     0);
    cbModeloDF.ItemIndex        := Ini.ReadInteger('Geral', 'ModeloDF',         0);

    cbVersaoDF.ItemIndex      := Ini.ReadInteger('Geral', 'VersaoDF',       0);
    ckSalvar.Checked          := Ini.ReadBool(   'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(   'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString( 'Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoCTe), integer(cbVersaoDF.ItemIndex) ));

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
    cbxEmissaoPathCTe.Checked   := Ini.ReadBool(  'Arquivos', 'EmissaoPathCTe',   false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathCTe.Text             := Ini.ReadString('Arquivos', 'PathCTe',          '');
    edtPathInu.Text             := Ini.ReadString('Arquivos', 'PathInu',          '');
    edtPathEvento.Text          := Ini.ReadString('Arquivos', 'PathEvento',       '');

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

    rgTipoDaCTe.ItemIndex := Ini.ReadInteger('DACTE', 'Tipo',      0);
    edtLogoMarca.Text     := Ini.ReadString( 'DACTE', 'LogoMarca', '');

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrCTe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrCTe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrCTe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrCTe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrCTe1.SSL.DescarregarCertificado;

  with ACBrCTe1.Configuracoes.Geral do
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
    ModeloDF         := TModeloCTe(cbModeloDF.ItemIndex);
    VersaoDF         := TVersaoCTe(cbVersaoDF.ItemIndex);
  end;

  with ACBrCTe1.Configuracoes.WebServices do
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
      edtIntervalo.Text := IntToStr(ACBrCTe1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrCTe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrCTe1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathCTe   := cbxEmissaoPathCTe.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathCTe          := edtPathCTe.Text;
    PathInu          := edtPathInu.Text;
    PathEvento       := edtPathEvento.Text;
    PathMensal       := GetPathCTe(0);
    PathSalvar       := PathMensal;
  end;

  if ACBrCTe1.DACTe <> nil then
  begin
    ACBrCTe1.DACTe.TipoDACTe := StrToTpImp(OK, IntToStr(rgTipoDaCTe.ItemIndex + 1));
    ACBrCTe1.DACTe.Logo      := edtLogoMarca.Text;

    ACBrCTe1.DACTe.PathPDF      := PathMensal;
    ACBrCTe1.DACTe.TamanhoPapel := tpA4;

    ACBrCTe1.DACTe.MargemDireita  := 7;
    ACBrCTe1.DACTe.MargemEsquerda := 7;
    ACBrCTe1.DACTe.MargemSuperior := 5;
    ACBrCTe1.DACTe.MargemInferior := 5;

    {
     Canhoto na posição Cabeçalho, layout Padrão e Barra funcionando perfeitamente
     Canhoto na posição Rodape, Layout Padrão e Barra não esta funcionando
    }
    // (prCabecalho, prRodape);
    ACBrCTe1.DACTE.PosCanhoto := prCabecalho;
    // (prlPadrao, prlBarra);
    ACBrCTe1.DACTE.PosCanhotoLayout := prlPadrao;
  end;
end;

procedure TfrmACBrCTe.ConfigurarEmail;
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
  ACBrMail1.FromName := 'Projeto ACBr - ACBrCTe';
end;

procedure TfrmACBrCTe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      RetWS, False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');

  if ACBrCTe1.Conhecimentos.Count > 0then
    MemoResp.Lines.Add('Empresa: ' + ACBrCTe1.Conhecimentos.Items[0].CTe.Emit.xNome);
end;

procedure TfrmACBrCTe.PathClick(Sender: TObject);
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

procedure TfrmACBrCTe.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TfrmACBrCTe.sbPathInuClick(Sender: TObject);
begin
  PathClick(edtPathInu);
end;

procedure TfrmACBrCTe.sbPathCTeClick(Sender: TObject);
begin
  PathClick(edtPathCTe);
end;

procedure TfrmACBrCTe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrCTe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrCTe1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrCTe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrCTe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  ACBrCTe1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrCTe1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrCTe1.SSL.ListaCertificados[I] do
    begin
      ASerie := NumeroSerie;

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

procedure TfrmACBrCTe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrCTe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

end.
