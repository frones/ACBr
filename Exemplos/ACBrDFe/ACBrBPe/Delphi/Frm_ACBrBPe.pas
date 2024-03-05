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

unit Frm_ACBrBPe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, ComCtrls, OleCtrls, SHDocVw, ACBrMail,
  ACBrPosPrinter, ACBrBPeDABPeESCPOS, ACBrBPeDABPEClass,
  ACBrDFeReport, ACBrBase, ACBrDFe,
  ACBrBPe, ShellAPI, XMLIntf, XMLDoc, zlib;

type
  TfrmACBrBPe = class(TForm)
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
    Label42: TLabel;
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
    sbPathBPe: TSpeedButton;
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathBPe: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathBPe: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDABPe: TRadioGroup;
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
    btnCriarEnviar: TButton;
    btnConsultar: TButton;
    btnConsultarChave: TButton;
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
    btnNaoEmbarque: TButton;
    btnImprimirEvento: TButton;
    btnEnviarEventoEmail: TButton;
    tsDistribuicao: TTabSheet;
    btnDistribuicaoDFe: TButton;
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
    ACBrBPe1: TACBrBPe;
    ACBrBPeDABPeESCPOS1: TACBrBPeDABPeESCPOS;
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
    btnImprimirDANFCEOffline: TButton;
    rgComponenteDABPE: TRadioGroup;
    Label32: TLabel;
    cbVersaoDF: TComboBox;
    btnStatusServ: TButton;
    btnAlteracaoPoltrona: TButton;
    chkLogoLateral: TCheckBox;
    Label30: TLabel;
    cbModeloDF: TComboBox;
    btnExcessoBagagem: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathBPeClick(Sender: TObject);
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
    procedure ACBrBPe1StatusChange(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure btnStatusServClick(Sender: TObject);
    procedure btnGerarXMLClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnCarregarXMLEnviarClick(Sender: TObject);
    procedure btnValidarRegrasNegocioClick(Sender: TObject);
    procedure btnValidarXMLClick(Sender: TObject);
    procedure btnValidarAssinaturaClick(Sender: TObject);
    procedure btnAdicionarProtocoloClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnGerarPDFClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarChaveClick(Sender: TObject);
    procedure btnCancelarXMLClick(Sender: TObject);
    procedure btnCancelarChaveClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnEnviarEventoEmailClick(Sender: TObject);
    procedure btnDistribuicaoDFeClick(Sender: TObject);
    procedure ACBrBPe1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure btSerialClick(Sender: TObject);
    procedure btnImprimirDANFCEOfflineClick(Sender: TObject);
    procedure btnNaoEmbarqueClick(Sender: TObject);
    procedure btnAlteracaoPoltronaClick(Sender: TObject);
    procedure btnExcessoBagagemClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    procedure AlimentarBPe(NumDFe: String);
    procedure AlimentarBPeTM(NumDFe: String);
    Procedure AlimentarComponente(NumDFe: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
    procedure PrepararImpressao;
  public
    { Public declarations }
  end;

var
  frmACBrBPe: TfrmACBrBPe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrUtil.XMLHTML,
  pcnConversao,
  ACBrXmlBase,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  ACBrBPeClass,
  ACBrBPeConversao, ACBrDFeComum.ConsReciDFe,
  ACBrBPeBilhetes, ACBrBPeConfiguracoes,
  Frm_Status, Frm_SelecionarCertificado, Frm_ConfiguraSerial;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrBPe }

procedure TfrmACBrBPe.ACBrBPe1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmACBrBPe.ACBrBPe1StatusChange(Sender: TObject);
begin
  case ACBrBPe1.Status of
    stBPeIdle:
      begin
        if ( frmStatus <> nil ) then
          frmStatus.Hide;
      end;

    stBPeStatusServico:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stBPeRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados da BPe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stBPeRetRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Recebendo dados da BPe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stBPeConsulta:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando BPe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stBPeEmail:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Email...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stBPeEvento:
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

procedure TfrmACBrBPe.AlimentarComponente(NumDFe: String);
begin
  ACBrBPe1.Bilhetes.Clear;

  case ACBrBPe1.Configuracoes.Geral.ModeloDF of
    moBPeTM: AlimentarBPeTM(NumDFe);
  else
    AlimentarBPe(NumDFe);
  end;
end;

procedure TfrmACBrBPe.AlimentarBPe(NumDFe: String);
begin
  with ACBrBPe1.Bilhetes.Add.BPe do
  begin
    //
    // Dados de Identificação do BP-e
    //
    Ide.cUF := UFtoCUF(edtEmitUF.Text);

    // TpcnTipoAmbiente = (taProducao, taHomologacao);
    case rgTipoAmb.ItemIndex of
      0: Ide.tpAmb := TACBrTipoAmbiente(taProducao);
      1: Ide.tpAmb := TACBrTipoAmbiente(taHomologacao);
    end;

    Ide.modelo  := 63;
    Ide.serie   := 1;
    Ide.nBP    := StrToIntDef(NumDFe, 0);
    Ide.cBP    := GerarCodigoDFe(Ide.nBP);
    // ( moRodoviario, moAquaviario, moFerroviario );
    Ide.modal   := moRodoviario;
    Ide.dhEmi   := Now;
    // TpcnTipoEmissao = (teNormal, teOffLine);
    Ide.tpEmis  := TACBrTipoEmissao(teNormal);
    Ide.verProc := '1.0.0.0'; //Versão do seu sistema
    Ide.indPres := pcPresencial;
    Ide.UFIni   := 'SP';
    Ide.cMunIni := 3503208;
    Ide.UFFim   := 'SP';
    Ide.cMunFim := 3550308;
    Ide.tpBPe   := tbNormal;

    //   Ide.dhCont  := Now;
//   Ide.xJust   := 'Motivo da Contingência';

    //
    // Dados do Emitente
    //
    Emit.CNPJ  := edtEmitCNPJ.Text;
    Emit.IE    := edtEmitIE.Text;
    Emit.IEST  := '';
    Emit.xNome := edtEmitRazao.Text;
    Emit.xFant := edtEmitFantasia.Text;
    Emit.IM    := '123';
    Emit.CNAE  := '1234567';
    Emit.CRT   := crtRegimeNormal;
    Emit.TAR   := '';

    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.Nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.EnderEmit.xMun    := edtEmitCidade.Text;
    Emit.EnderEmit.CEP     := StrToIntDef(edtEmitCEP.Text, 0);
    Emit.EnderEmit.UF      := edtEmitUF.Text;
    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.enderEmit.email   := 'endereco@provedor.com.br';

    //
    // Dados do Comprador
    //
    Comp.xNome   := 'Nome do Comprador';
    Comp.CNPJCPF := '06760213874';
    Comp.IE      := '';

    Comp.EnderComp.xLgr    := 'Nome do Logradouro';
    Comp.EnderComp.Nro     := 'Numero';
    Comp.EnderComp.xCpl    := 'Complemento';
    Comp.EnderComp.xBairro := 'Bairro';
    Comp.EnderComp.cMun    := 3503208; //StrToInt('Codigo IBGE da cidade do comprador');
    Comp.EnderComp.xMun    := 'Nome da Cidade';
    Comp.EnderComp.CEP     := StrToIntDef('00000000', 0);
    Comp.EnderComp.UF      := 'SP';
    Comp.EnderComp.cPais   := 1058;
    Comp.EnderComp.xPais   := 'BRASIL';
    Comp.EnderComp.fone    := 'Telefone do comprador';
    Comp.enderComp.email   := 'endereco@provedor.com.br';

    //
    // Dados da Agencia
    //
    Agencia.xNome := 'Nome da Agencia';
    Agencia.CNPJ  := edtEmitCNPJ.Text;

    Agencia.EnderAgencia.xLgr    := 'Nome do Logradouro';
    Agencia.EnderAgencia.Nro     := 'Numero';
    Agencia.EnderAgencia.xCpl    := 'Complemento';
    Agencia.EnderAgencia.xBairro := 'Bairro';
    Agencia.EnderAgencia.cMun    := 3503208; //StrToInt('Codigo IBGE da cidade da Agencia');
    Agencia.EnderAgencia.xMun    := 'Nome da Cidade';
    Agencia.EnderAgencia.CEP     := StrToIntDef('00000000', 0);
    Agencia.EnderAgencia.UF      := 'SP';
    Agencia.EnderAgencia.fone    := 'Telefone da Agencia';
    Agencia.enderAgencia.email   := 'endereco@provedor.com.br';
    Agencia.EnderAgencia.cPais   := 1058;
    Agencia.EnderAgencia.xPais   := 'BRASIL';

    //
    // Informações do BP-e Substituido (informar se ocorrer)
    //
//   infBPeSub.chBPe := 'Chave do BPe substituido';
//   infBPeSub.tpSub := tsRemarcacao;

    //
    // Informações sobre a Passagem
    //
    infPassagem.cLocOrig := '1234567'; // Codigo da Localidade de Origem
    infPassagem.xLocOrig := 'Descrição da Localidade de Origem';
    infPassagem.cLocDest := '1234567'; // Codigo da Localidade de Destino
    infPassagem.xLocDest := 'Descrição da Localidade de Destino';
    infPassagem.dhEmb    := Now;
    //
    // Informações sobre o Passageiro
    //
    infPassagem.infPassageiro.xNome := 'Nome do Passageiro';
    infPassagem.infPassageiro.CPF   := '06760213874';
    infPassagem.infPassageiro.tpDoc := tdRG;
    infPassagem.infPassageiro.nDoc  := '12345678'; // Numero do documento
//   infPassagem.infPassageiro.dNasc := StrToDate('10/10/1970');
    infPassagem.infPassageiro.Fone  := '33445566'; // telefone do passageiro
    infPassagem.infPassageiro.Email := 'passageiro@provedor.com.br';
    infPassagem.dhValidade := Now + 366.0;

    //
    // Informações sobre a Viagem
    //

    with infViagem.New do
    begin
      cPercurso    := 'Código do Percurso';
      xPercurso    := 'Descrição do Percurso';
      tpViagem     := tvRegular;
      tpServ       := tsConvencionalComSanitario;
      tpAcomodacao := taAssento;
      tpTrecho     := ttNormal;
//     dhConexao    := ** Informar se o tpTrecho for ttConexao
      prefixo      := 'Prefixo da linha';
      Poltrona     := 21;
      dhViagem     := now;
      //
      // Informações sobre a Travessia (se ocorrer)
      //
//     infTravessia.tpVeiculo  := tvAutomovel;
//     infTravessia.sitVeiculo := svCarregado;
    end;

    //
    // Informações sobre o valor do BPe
    //

    infValorBPe.vBP        :=  98.00;
    infValorBPe.vDesconto  :=   0.00;
    infValorBPe.vPgto      := 100.00;
    infValorBPe.vTroco     :=   2.00;
    infValorBPe.tpDesconto := tdNenhum;
    infValorBPe.xDesconto  := '';
    //
    // Composição do valor do BPe
    //
    with infValorBPe.Comp.New do
    begin
      tpComp := tcTarifa;
      vComp  := 25.00;
    end;
    with infValorBPe.Comp.New do
    begin
      tpComp := tcPedagio;
      vComp  := 35.00;
    end;
    with infValorBPe.Comp.New do
    begin
      tpComp := tcOutros;
      vComp  := 38.00;
    end;

    //
    // Informações sobre o valor do BPe
    //

    Imp.ICMS.CST   := cst00;
    Imp.ICMS.vBC   := 98.00;
    Imp.ICMS.pICMS := 18.00;
    Imp.ICMS.vICMS := 17.64;

    Imp.vTotTrib   := 0.00;
    Imp.infAdFisco := '';

    //
    // Informações sobre o Pagamento
    //

    with Pag.New do
    begin
      tPag := fpDinheiro;
      vPag := 98.00;

      tpIntegra := tiNaoInformado;
      CNPJ      := '';
      tBand     := bcOutros;
      cAut      := '';
    end;

    //
    // Autorizados para o Download do XML do BPe
    //
    (*
    with autXML.New do
    begin
      CNPJCPF := '00000000000000';
    end;

    with autXML.New do
    begin
      CNPJCPF := '11111111111111';
    end;
    *)
    //
    // Informações Adicionais
    //

    infAdic.infAdFisco := '';
    infAdic.infCpl     := 'Informações Complementares';
  end;
end;

procedure TfrmACBrBPe.AlimentarBPeTM(NumDFe: String);
begin
  with ACBrBPe1.Bilhetes.Add.BPe do
  begin
    //
    // Dados de Identificação do BP-e TM
    //
    Ide.cUF := UFtoCUF(edtEmitUF.Text);

    // TpcnTipoAmbiente = (taProducao, taHomologacao);
    case rgTipoAmb.ItemIndex of
      0: Ide.tpAmb := taProducao;
      1: Ide.tpAmb := taHomologacao;
    end;

    Ide.modelo  := 63;
    Ide.serie   := 1;
    Ide.nBP    := StrToIntDef(NumDFe, 0);
    Ide.cBP    := GerarCodigoDFe(Ide.nBP);
    // ( moRodoviario, moAquaviario, moFerroviario );
    Ide.modal   := moRodoviario;
    Ide.dhEmi   := Now;
    Ide.dCompet := Date;
    // TpcnTipoEmissao = (teNormal, teOffLine);
    Ide.tpEmis  := TACBrTipoEmissao(teNormal);
    Ide.verProc := '1.0.0.0'; //Versão do seu sistema
    Ide.indPres := pcPresencial;
    Ide.UFIni   := 'SP';
    Ide.cMunIni := 3503208;
    Ide.UFFim   := 'SP';
    Ide.cMunFim := 3550308;
    Ide.tpBPe   := tbBPeTM;
    Ide.CFOP    := 5104;

//   Ide.dhCont  := Now;
//   Ide.xJust   := 'Motivo da Contingência';

    //
    // Dados do Emitente
    //
    Emit.CNPJ  := edtEmitCNPJ.Text;
    Emit.IE    := edtEmitIE.Text;
    Emit.IEST  := '';
    Emit.xNome := edtEmitRazao.Text;
    Emit.xFant := edtEmitFantasia.Text;
    Emit.IM    := '123';
    Emit.CNAE  := '1234567';
    Emit.CRT   := crtRegimeNormal;
    Emit.TAR   := '';

    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.Nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.EnderEmit.xMun    := edtEmitCidade.Text;
    Emit.EnderEmit.CEP     := StrToIntDef(edtEmitCEP.Text, 0);
    Emit.EnderEmit.UF      := edtEmitUF.Text;
    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.enderEmit.email   := 'endereco@provedor.com.br';

    //
    // Informações sobre o Detalhamento do BPe TM
    //
    with detBPeTM.New do
    begin
      idEqpCont := 1;
      UFIniViagem := 'SP';
      UFFimViagem := 'SP';
      Placa := 'XYZ1234';
      Prefixo := '123';

      //
      // Detalhamento da viagem por trechos do BPe TM
      //
      with detBPeTM[0].det.New do
      begin
        nViagem := 1;
        cMunIni := 3554003;
        cMunFim := 3554003;
        nContInicio := '1';
        nContFim := '2';
        qPass := '10';
        vBP := 100;

        //
        // Informações sobre o Imposto
        //
        Imp.ICMS.CST   := cst00;
        Imp.ICMS.vBC   := 98.00;
        Imp.ICMS.pICMS := 18.00;
        Imp.ICMS.vICMS := 17.64;

        Imp.infAdFisco := '';

        //
        // Informações sobre os Componentes da Viagem
        //
        with detBPeTM[0].det[0].Comp.New do
        begin
          xNome := 'IDOSOS';
          qComp := 1;
        end;

        with detBPeTM[0].det[0].Comp.New do
        begin
          xNome := 'VT';
          qComp := 9;
        end;
      end;
    end;

    //
    // Informações sobre o Total de Valores
    //
    total.qPass := 10;
    total.vBP   := 100;
    total.vBC   := 100;
    total.vICMS := 18;

    //
    // Autorizados para o Download do XML do BPe
    //
    (*
    with autXML.New do
    begin
      CNPJCPF := '00000000000000';
    end;

    with autXML.New do
    begin
      CNPJCPF := '11111111111111';
    end;
    *)

    //
    // Informações Adicionais
    //
    infAdic.infAdFisco := '';
    infAdic.infCpl     := 'Informações Complementares';

    //
    // Informações do Responsável Técnico pela emissão do DF-e
    //
    infRespTec.xContato := '';
    infRespTec.email    := '';
    infRespTec.fone     := '';
  end;
end;

procedure TfrmACBrBPe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrBPe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrBPe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrBPe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrBPe1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrBPe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrBPe.btnAdicionarProtocoloClick(Sender: TObject);
var
  NomeArq: String;
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);
    ACBrBPe1.Consultar;

    ShowMessage(ACBrBPe1.WebServices.Consulta.Protocolo);

    MemoResp.Lines.Text := ACBrBPe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrBPe1.WebServices.Consulta.RetornoWS, WBResposta);
    NomeArq := OpenDialog1.FileName;

    if pos(UpperCase('-BPe.xml'), UpperCase(NomeArq)) > 0 then
       NomeArq := StringReplace(NomeArq, '-BPe.xml', '-procBPe.xml', [rfIgnoreCase]);

    ACBrBPe1.Bilhetes.Items[0].GravarXML(NomeArq);
    ShowMessage('Arquivo gravado em: ' + NomeArq);
    memoLog.Lines.Add('Arquivo gravado em: ' + NomeArq);
  end;
end;

procedure TfrmACBrBPe.btnAlteracaoPoltronaClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, NumPoltrona: string;
begin
  if not(InputQuery('WebServices Eventos: Alteração de Poltrona', 'Chave da BP-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery('WebServices Eventos: Alteração de Poltrona', 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := copy(Chave,7,14);
  if not(InputQuery('WebServices Eventos: Alteração de Poltrona', 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;
  Protocolo:='';
  if not(InputQuery('WebServices Eventos: Alteração de Poltrona', 'Protocolo de Autorização', Protocolo)) then
     exit;
  NumPoltrona := '1';
  if not(InputQuery('WebServices Eventos: Alteração de Poltrona', 'Número da Poltrona', NumPoltrona)) then
     exit;

  NumPoltrona := OnlyNumber(NumPoltrona);

  if Trim(NumPoltrona) = '' then
  begin
    MessageDlg('Número da Poltrona inválido.', mtError,[mbok], 0);
    exit;
  end;

  ACBrBPe1.EventoBPe.Evento.Clear;

  with ACBrBPe1.EventoBPe.Evento.New do
  begin
    infevento.chBPe := Chave;
    infevento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teAlteracaoPoltrona;
    infEvento.detEvento.poltrona := StrToInt(NumPoltrona);
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrBPe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrBPe1.WebServices.EnvEvento.RetornoWS, WBResposta);
  (*
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfevento.chBPe
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrBPe.btnCancelarChaveClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, Justificativa: string;
begin
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Chave da BP-e', Chave)) then
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

  ACBrBPe1.EventoBPe.Evento.Clear;

  with ACBrBPe1.EventoBPe.Evento.New do
  begin
    infevento.chBPe := Chave;
    infevento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teCancelamento;
    infEvento.detEvento.xJust := Justificativa;
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrBPe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrBPe1.WebServices.EnvEvento.RetornoWS, WBResposta);
  (*
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfevento.chBPe
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrBPe.btnCancelarXMLClick(Sender: TObject);
var
  idLote, vAux: String;
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);

    idLote := '1';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
       exit;

    if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa', vAux)) then
       exit;

    ACBrBPe1.EventoBPe.Evento.Clear;
    ACBrBPe1.EventoBPe.idLote := StrToInt(idLote);

    with ACBrBPe1.EventoBPe.Evento.New do
    begin
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.detEvento.xJust := vAux;
    end;

    ACBrBPe1.EnviarEvento(StrToInt(idLote));

    MemoResp.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetornoWS;
    LoadXML(ACBrBPe1.WebServices.EnvEvento.RetornoWS, WBResposta);
    ShowMessage(IntToStr(ACBrBPe1.WebServices.EnvEvento.cStat));
    ShowMessage(ACBrBPe1.WebServices.EnvEvento.EventoRetorno.RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrBPe.btnCarregarXMLEnviarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName, False);

    with ACBrBPe1.Bilhetes.Items[0].BPe do
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
      Emit.IM                := ''; // Preencher no caso de existir serviços
      Emit.CNAE              := ''; // Verifique na cidade do emissor da BPe se é permitido
                                    // a inclusão de serviços na BPe
      Emit.CRT               := crtRegimeNormal;// (1-crtSimplesNacional, 2-crtSimplesExcessoReceita, 3-crtRegimeNormal)
    end;

    ACBrBPe1.Enviar(1);
    {
    MemoResp.Lines.Text := ACBrBPe1.WebServices.Retorno.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.Retorno.RetornoWS;
    LoadXML(ACBrBPe1.WebServices.Retorno.RetornoWS, WBResposta);

    MemoDados.Lines.Add('');
    MemoDados.Lines.Add('Envio BPe');
    MemoDados.Lines.Add('tpAmb: '+ TpAmbToStr(ACBrBPe1.WebServices.Retorno.TpAmb));
    MemoDados.Lines.Add('verAplic: '+ ACBrBPe1.WebServices.Retorno.verAplic);
    MemoDados.Lines.Add('cStat: '+ IntToStr(ACBrBPe1.WebServices.Retorno.cStat));
    MemoDados.Lines.Add('cUF: '+ IntToStr(ACBrBPe1.WebServices.Retorno.cUF));
    MemoDados.Lines.Add('xMotivo: '+ ACBrBPe1.WebServices.Retorno.xMotivo);
    MemoDados.Lines.Add('cMsg: '+ IntToStr(ACBrBPe1.WebServices.Retorno.cMsg));
    MemoDados.Lines.Add('xMsg: '+ ACBrBPe1.WebServices.Retorno.xMsg);
    MemoDados.Lines.Add('Recibo: '+ ACBrBPe1.WebServices.Retorno.Recibo);
    MemoDados.Lines.Add('Protocolo: '+ ACBrBPe1.WebServices.Retorno.Protocolo);
    }
  end;
end;

procedure TfrmACBrBPe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrBPe1.SSL.CertCNPJ);
end;

procedure TfrmACBrBPe.btnConsultarChaveClick(Sender: TObject);
var
  vChave: String;
begin
  if not(InputQuery('WebServices Consultar', 'Chave da BP-e:', vChave)) then
    exit;

  ACBrBPe1.Bilhetes.Clear;
  ACBrBPe1.WebServices.Consulta.BPeChave := vChave;
  ACBrBPe1.WebServices.Consulta.Executar;

  MemoResp.Lines.Text := ACBrBPe1.WebServices.Consulta.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.Consulta.RetornoWS;
  LoadXML(ACBrBPe1.WebServices.Consulta.RetornoWS, WBResposta);
end;

procedure TfrmACBrBPe.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);
    ACBrBPe1.Consultar;

    ShowMessage(ACBrBPe1.WebServices.Consulta.Protocolo);
    MemoResp.Lines.Text := ACBrBPe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrBPe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrBPe1.WebServices.Consulta.RetornoWS, WBResposta);
  end;
end;

procedure TfrmACBrBPe.btnCriarEnviarClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Bilhete', vAux)) then
    exit;

  if not(InputQuery('WebServices Enviar', 'Numero do Lote', vNumLote)) then
    exit;

  vNumLote := OnlyNumber(vNumLote);

  if Trim(vNumLote) = '' then
  begin
    MessageDlg('Número do Lote inválido.', mtError,[mbok], 0);
    exit;
  end;

  AlimentarComponente(vAux);

  ACBrBPe1.Enviar(vNumLote);

  pgRespostas.ActivePageIndex := 1;

  MemoResp.Lines.Text := ACBrBPe1.WebServices.Enviar.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.Enviar.RetornoWS;
  LoadXML(ACBrBPe1.WebServices.Enviar.RetWS, WBResposta);

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Envio BPe');
  MemoDados.Lines.Add('tpAmb: ' + TipoAmbienteToStr(ACBrBPe1.WebServices.Enviar.TpAmb));
  MemoDados.Lines.Add('verAplic: ' + ACBrBPe1.WebServices.Enviar.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrBPe1.WebServices.Enviar.cStat));
  MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrBPe1.WebServices.Enviar.cUF));
  MemoDados.Lines.Add('xMotivo: ' + ACBrBPe1.WebServices.Enviar.xMotivo);
  MemoDados.Lines.Add('Protocolo: ' + ACBrBPe1.WebServices.Enviar.Protocolo);
  (*
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].tpAmb
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].verAplic
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].chBPe
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].dhRecbto
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].nProt
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].digVal
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].cStat
  ACBrBPe1.WebServices.Retorno.BPeRetorno.ProtBPe.Items[0].xMotivo
  *)
end;

procedure TfrmACBrBPe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrBPe1.SSL.CertDataVenc));
end;

procedure TfrmACBrBPe.btnDistribuicaoDFeClick(Sender: TObject);
var
  cUFAutor, CNPJ, ultNSU, ANSU: string;
begin
  cUFAutor := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'Código da UF do Autor', cUFAutor)) then
     exit;

  CNPJ := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
     exit;

  ultNSU := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'Último NSU recebido pelo ator', ultNSU)) then
     exit;

  ANSU := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'NSU específico', ANSU)) then
     exit;

  ACBrBPe1.DistribuicaoDFe(StrToInt(cUFAutor), CNPJ, ultNSU, ANSU);
  {
  MemoResp.Lines.Text := ACBrBPe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrBPe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);
  }
end;

procedure TfrmACBrBPe.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrBPe1.Bilhetes.Clear;
  ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);

  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrBPe1.Bilhetes.Items[0].EnviarEmail(Para
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

procedure TfrmACBrBPe.btnEnviarEventoEmailClick(Sender: TObject);
var
  Para: String;
  CC, Evento: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione ao Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  Evento := TStringList.Create;
  CC := TStringList.Create;
  try
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);
    ACBrBPe1.EventoBPe.Evento.Clear;
    ACBrBPe1.EventoBPe.LerXML(OpenDialog1.FileName);

    //CC.Add('email_1@provedor.com'); //especifique um email valido
    //CC.Add('email_2@provedor.com.br');    //especifique um email valido
    ConfigurarEmail;
    ACBrBPe1.EnviarEmailEvento(Para
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

procedure TfrmACBrBPe.btnExcessoBagagemClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, qBagagem, vTotBag: string;
begin
  if not(InputQuery('WebServices Eventos: Excesso de Bagagem', 'Chave da BP-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery('WebServices Eventos: Excesso de Bagagem', 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := copy(Chave,7,14);
  if not(InputQuery('WebServices Eventos: Excesso de Bagagem', 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;
  Protocolo:='';
  if not(InputQuery('WebServices Eventos: Excesso de Bagagem', 'Protocolo de Autorização', Protocolo)) then
     exit;
  qBagagem := '1';
  if not(InputQuery('WebServices Eventos: Excesso de Bagagem', 'Quantidade de volumes de bagagem carregados', qBagagem)) then
     exit;
  vTotBag := '1';
  if not(InputQuery('WebServices Eventos: Excesso de Bagagem', 'Valor total do serviço', vTotBag)) then
     exit;

  qBagagem := OnlyNumber(qBagagem);

  if Trim(qBagagem) = '' then
  begin
    MessageDlg('Quantidade de volumes de bagagem carregados inválida.', mtError,[mbok], 0);
    exit;
  end;

  if Trim(vTotBag) = '' then
  begin
    MessageDlg('Valor total do serviço inválido.', mtError,[mbok], 0);
    exit;
  end;

  ACBrBPe1.EventoBPe.Evento.Clear;

  with ACBrBPe1.EventoBPe.Evento.New do
  begin
    infevento.chBPe := Chave;
    infevento.CNPJ := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teExcessoBagagem;
    infEvento.detEvento.nProt := Protocolo;
    infEvento.detEvento.qBagagem := StrToIntDef(qBagagem, 0);
    infEvento.detEvento.vTotBag := StrToFloatDef(vTotBag, 0);
  end;

  ACBrBPe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrBPe1.WebServices.EnvEvento.RetornoWS, WBResposta);
end;

procedure TfrmACBrBPe.btnGerarPDFClick(Sender: TObject);
var
  CarregarMaisXML: Boolean;
begin
	CarregarMaisXML := true;
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;
  ACBrBPe1.Bilhetes.Clear;

  while CarregarMaisXML do
  begin
    if OpenDialog1.Execute then
      ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);

    CarregarMaisXML := MessageDlg('Carregar mais Bilhetes?', mtConfirmation, mbYesNoCancel, 0) = mrYes;
  end;

  ACBrBPe1.Bilhetes.ImprimirPDF;
end;

procedure TfrmACBrBPe.btnGerarXMLClick(Sender: TObject);
var
  vAux: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Bilhete', vAux)) then
    exit;

  ACBrBPe1.Bilhetes.Clear;

  AlimentarComponente(vAux);

  ACBrBPe1.Bilhetes.Assinar;

  ACBrBPe1.Bilhetes.Items[0].GravarXML();

  ShowMessage('Arquivo gerado em: ' + ACBrBPe1.Bilhetes.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: ' + ACBrBPe1.Bilhetes.Items[0].NomeArq);

  MemoResp.Lines.LoadFromFile(ACBrBPe1.Bilhetes.Items[0].NomeArq);

  LoadXML(MemoResp.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrBPe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrBPe1.SSL.UseCertificateHTTP;
  ACBrBPe1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrBPe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrBPe1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrBPe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    PrepararImpressao;

    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName,False);
    ACBrBPe1.Bilhetes.Imprimir;
  end;
end;

procedure TfrmACBrBPe.btnImprimirDANFCEOfflineClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    PrepararImpressao;

    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName,False);
    ACBrBPe1.Bilhetes.ImprimirOffline;
  end;
end;

procedure TfrmACBrBPe.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.EventoBPe.Evento.Clear;
    ACBrBPe1.EventoBPe.LerXML(OpenDialog1.FileName);
    ACBrBPe1.ImprimirEvento;
  end;
end;

procedure TfrmACBrBPe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrBPe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrBPe1.SSL.CertCertificadora);
end;

procedure TfrmACBrBPe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrBPe1.SSL do
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

procedure TfrmACBrBPe.btnNaoEmbarqueClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, Justificativa: string;
begin
  if not(InputQuery('WebServices Eventos: Não Embarque', 'Chave da BP-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));
  idLote := '1';
  if not(InputQuery('WebServices Eventos: Não Embarque', 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;
  CNPJ := copy(Chave,7,14);
  if not(InputQuery('WebServices Eventos: Não Embarque', 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;
  Protocolo:='';
  if not(InputQuery('WebServices Eventos: Não Embarque', 'Protocolo de Autorização', Protocolo)) then
     exit;
  Justificativa := 'Justificativa do Não Embarque';
  if not(InputQuery('WebServices Eventos: Não Embarque', 'Justificativa do Não Embarque', Justificativa)) then
     exit;

  ACBrBPe1.EventoBPe.Evento.Clear;

  with ACBrBPe1.EventoBPe.Evento.New do
  begin
    infevento.chBPe := Chave;
    infevento.CNPJ   := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teNaoEmbarque;
    infEvento.detEvento.xJust := Justificativa;
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrBPe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrBPe1.WebServices.EnvEvento.RetornoWS, WBResposta);
  (*
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfevento.chBPe
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrBPe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrBPe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrBPe1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrBPe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrBPe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrBPe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrBPe.btnStatusServClick(Sender: TObject);
begin
  ACBrBPe1.WebServices.StatusServico.Executar;

  MemoResp.Lines.Text := ACBrBPe1.WebServices.StatusServico.RetWS;
  memoRespWS.Lines.Text := ACBrBPe1.WebServices.StatusServico.RetornoWS;
  LoadXML(ACBrBPe1.WebServices.StatusServico.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Status Serviço');
  MemoDados.Lines.Add('tpAmb: '    +TipoAmbienteToStr(ACBrBPe1.WebServices.StatusServico.tpAmb));
  MemoDados.Lines.Add('verAplic: ' +ACBrBPe1.WebServices.StatusServico.verAplic);
  MemoDados.Lines.Add('cStat: '    +IntToStr(ACBrBPe1.WebServices.StatusServico.cStat));
  MemoDados.Lines.Add('xMotivo: '  +ACBrBPe1.WebServices.StatusServico.xMotivo);
  MemoDados.Lines.Add('cUF: '      +IntToStr(ACBrBPe1.WebServices.StatusServico.cUF));
  MemoDados.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrBPe1.WebServices.StatusServico.dhRecbto));
  MemoDados.Lines.Add('tMed: '     +IntToStr(ACBrBPe1.WebServices.StatusServico.TMed));
  MemoDados.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrBPe1.WebServices.StatusServico.dhRetorno));
  MemoDados.Lines.Add('xObs: '     +ACBrBPe1.WebServices.StatusServico.xObs);
end;

procedure TfrmACBrBPe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrBPe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrBPe1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrBPe.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg: String;
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);
    pgRespostas.ActivePageIndex := 0;
    MemoResp.Lines.Add('');
    MemoResp.Lines.Add('');

    if not ACBrBPe1.Bilhetes.VerificarAssinatura(Msg) then
      MemoResp.Lines.Add('Erro: '+Msg)
    else
    begin
      MemoResp.Lines.Add('OK: Assinatura Válida');
      ACBrBPe1.SSL.CarregarCertificadoPublico( ACBrBPe1.Bilhetes[0].BPe.signature.X509Certificate );
      MemoResp.Lines.Add('Assinado por: '+ ACBrBPe1.SSL.CertRazaoSocial);
      MemoResp.Lines.Add('CNPJ: '+ ACBrBPe1.SSL.CertCNPJ);
      MemoResp.Lines.Add('Num.Série: '+ ACBrBPe1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure TfrmACBrBPe.btnValidarRegrasNegocioClick(Sender: TObject);
var
  Msg, Tempo: String;
  Inicio: TDateTime;
  Ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);
    Inicio := Now;
    Ok := ACBrBPe1.Bilhetes.ValidarRegrasdeNegocios(Msg);
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

procedure TfrmACBrBPe.btnValidarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a BPe';
  OpenDialog1.DefaultExt := '*-BPe.XML';
  OpenDialog1.Filter := 'Arquivos BPe (*-BPe.XML)|*-BPe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrBPe1.Configuracoes.Arquivos.PathSalvar;

  // Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrBPe1.Configuracoes.Geral.ExibirErroSchema := False;
  ACBrBPe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  if OpenDialog1.Execute then
  begin
    ACBrBPe1.Bilhetes.Clear;
    ACBrBPe1.Bilhetes.LoadFromFile(OpenDialog1.FileName);

    try
      ACBrBPe1.Bilhetes.Validar;

      if ACBrBPe1.Bilhetes.Items[0].Alertas <> '' then
        MemoDados.Lines.Add('Alertas: ' + ACBrBPe1.Bilhetes.Items[0].Alertas);

      ShowMessage('Bilhete de Passagem Eletrônico Valido');
    except
      on E: Exception do
      begin
        pgRespostas.ActivePage := Dados;
        MemoDados.Lines.Add('Exception: ' + E.Message);
        MemoDados.Lines.Add('Erro: ' + ACBrBPe1.Bilhetes.Items[0].ErroValidacao);
        MemoDados.Lines.Add('Erro Completo: ' + ACBrBPe1.Bilhetes.Items[0].ErroValidacaoCompleto);
      end;
    end;
  end;
end;

procedure TfrmACBrBPe.btSerialClick(Sender: TObject);
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

procedure TfrmACBrBPe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrBPe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrBPe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrBPe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrBPe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrBPe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrBPe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrBPe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoBPe;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  N: TACBrPosPrinterModelo;
  O: TACBrPosPaginaCodigo;
  l: Integer;
  J: TModeloBPe;
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

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoBPe) to High(TVersaoBPe) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoBPe), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  cbxModeloPosPrinter.Items.Clear ;
  for N := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
    cbxModeloPosPrinter.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(N) ) ) ;

  cbxPagCodigo.Items.Clear ;
  for O := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(O) ) ) ;

  cbModeloDF.Items.Clear;
  for J := Low(TModeloBPe) to High(TModeloBPe) do
     cbModeloDF.Items.Add( GetEnumName(TypeInfo(TModeloBPe), integer(J) ) );
  cbModeloDF.ItemIndex := 0;

  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );
  cbxPorta.Items.Add('LPT1') ;
  cbxPorta.Items.Add('LPT2') ;
  cbxPorta.Items.Add('\\localhost\Epson') ;
  cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;

  for l := 0 to Printer.Printers.Count-1 do
    cbxPorta.Items.Add('RAW:'+Printer.Printers[l]);

  cbxPorta.Items.Add('/dev/ttyS0') ;
  cbxPorta.Items.Add('/dev/ttyS1') ;
  cbxPorta.Items.Add('/dev/ttyUSB0') ;
  cbxPorta.Items.Add('/dev/ttyUSB1') ;
  cbxPorta.Items.Add('/tmp/ecf.txt') ;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBrBPe.GravarConfiguracao;
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
    Ini.WriteInteger('Geral', 'VersaoDF',         cbVersaoDF.ItemIndex);
    Ini.WriteInteger('Geral', 'ModeloDF',         cbModeloDF.ItemIndex);

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
    Ini.WriteBool(  'Arquivos', 'EmissaoPathBPe',   cbxEmissaoPathBPe.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathBPe',          edtPathBPe.Text);
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

    Ini.WriteInteger('DABPE', 'Tipo',      rgTipoDaBPe.ItemIndex);
    Ini.WriteString( 'DABPE', 'LogoMarca', edtLogoMarca.Text);
    Ini.WriteInteger('DABPE', 'TipoDABPE', rgComponenteDABPE.ItemIndex);

    INI.WriteInteger('PosPrinter', 'Modelo',            cbxModeloPosPrinter.ItemIndex);
    INI.WriteString( 'PosPrinter', 'Porta',             cbxPorta.Text);
    INI.WriteInteger('PosPrinter', 'PaginaDeCodigo',    cbxPagCodigo.ItemIndex);
    INI.WriteString( 'PosPrinter', 'ParamsString',      ACBrPosPrinter1.Device.ParamsString);
    INI.WriteInteger('PosPrinter', 'Colunas',           seColunas.Value);
    INI.WriteInteger('PosPrinter', 'EspacoLinhas',      seEspLinhas.Value);
    INI.WriteInteger('PosPrinter', 'LinhasEntreCupons', seLinhasPular.Value);
    Ini.WriteBool(   'PosPrinter', 'CortarPapel',       cbCortarPapel.Checked );
    Ini.WriteBool(   'PosPrinter', 'LogoLateral',       chkLogoLateral.Checked );

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrBPe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrBPe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrBPe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrBPe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrBPe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrBPe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrBPe.LerConfiguracao;
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

    cbxAtualizarXML.Checked     := Ini.ReadBool(   'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(   'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString( 'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger('Geral', 'FormaEmissao',     0);
    cbVersaoDF.ItemIndex        := Ini.ReadInteger('Geral', 'VersaoDF',         0);
    cbModeloDF.ItemIndex        := Ini.ReadInteger('Geral', 'ModeloDF',         0);

    ckSalvar.Checked          := Ini.ReadBool(  'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(  'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString('Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString('Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoBPe), integer(cbVersaoDF.ItemIndex) ));

    cbUF.ItemIndex := cbUF.Items.IndexOf(Ini.ReadString('WebService', 'UF', 'SP'));

    rgTipoAmb.ItemIndex   := Ini.ReadInteger('WebService', 'Ambiente',   0);
    cbxVisualizar.Checked := Ini.ReadBool(   'WebService', 'Visualizar', False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(   'WebService', 'SalvarSOAP', False);
    cbxAjustarAut.Checked := Ini.ReadBool(   'WebService', 'AjustarAut', False);
    edtAguardar.Text      := Ini.ReadString( 'WebService', 'Aguardar',   '0');
    edtTentativas.Text    := Ini.ReadString( 'WebService', 'Tentativas', '5');
    edtIntervalo.Text     := Ini.ReadString( 'WebService', 'Intervalo',  '0');
    seTimeOut.Value       := Ini.ReadInteger('WebService', 'TimeOut',    5000);
    cbSSLType.ItemIndex   := Ini.ReadInteger('WebService', 'SSLType',    0);

    edtProxyHost.Text  := Ini.ReadString('Proxy', 'Host',  '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text  := Ini.ReadString('Proxy', 'User',  '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass',  '');

    cbxSalvarArqs.Checked       := Ini.ReadBool(  'Arquivos', 'Salvar',           false);
    cbxPastaMensal.Checked      := Ini.ReadBool(  'Arquivos', 'PastaMensal',      false);
    cbxAdicionaLiteral.Checked  := Ini.ReadBool(  'Arquivos', 'AddLiteral',       false);
    cbxEmissaoPathBPe.Checked   := Ini.ReadBool(  'Arquivos', 'EmissaoPathBPe',   false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathBPe.Text             := Ini.ReadString('Arquivos', 'PathBPe',          '');
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

    rgTipoDaBPe.ItemIndex       := Ini.ReadInteger('DABPE', 'Tipo',       0);
    edtLogoMarca.Text           := Ini.ReadString( 'DABPE', 'LogoMarca',  '');
    rgComponenteDABPE.ItemIndex := Ini.ReadInteger('DABPE', 'TipoDABPE', 0);

    cbxModeloPosPrinter.ItemIndex := INI.ReadInteger('PosPrinter', 'Modelo',            Integer(ACBrPosPrinter1.Modelo));
    cbxPorta.Text                 := INI.ReadString( 'PosPrinter', 'Porta',             ACBrPosPrinter1.Porta);
    cbxPagCodigo.ItemIndex        := INI.ReadInteger('PosPrinter', 'PaginaDeCodigo',    Integer(ACBrPosPrinter1.PaginaDeCodigo));
    seColunas.Value               := INI.ReadInteger('PosPrinter', 'Colunas',           ACBrPosPrinter1.ColunasFonteNormal);
    seEspLinhas.Value             := INI.ReadInteger('PosPrinter', 'EspacoLinhas',      ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value           := INI.ReadInteger('PosPrinter', 'LinhasEntreCupons', ACBrPosPrinter1.LinhasEntreCupons);
    cbCortarPapel.Checked         := Ini.ReadBool(   'PosPrinter', 'CortarPapel',       True);
    chkLogoLateral.Checked        := Ini.ReadBool(   'PosPrinter', 'LogoLateral',       False);

    ACBrPosPrinter1.Device.ParamsString := INI.ReadString('PosPrinter', 'ParamsString', '');

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrBPe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrBPe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrBPe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrBPe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrBPe1.DABPe := ACBrBPeDABPeESCPOS1;

  ACBrBPe1.SSL.DescarregarCertificado;

  with ACBrBPe1.Configuracoes.Geral do
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
    VersaoDF         := TVersaoBPe(cbVersaoDF.ItemIndex);
    ModeloDF         := TModeloBPe(cbModeloDF.ItemIndex);
  end;

  with ACBrBPe1.Configuracoes.WebServices do
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
      edtIntervalo.Text := IntToStr(ACBrBPe1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrBPe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrBPe1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathBPe   := cbxEmissaoPathBPe.Checked;
    SalvarEvento     := cbxSalvaPathEvento.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathBPe          := edtPathBPe.Text;
    PathEvento       := edtPathEvento.Text;
    PathMensal       := GetPathBPe(0, '', '', ACBrBPe1.Configuracoes.Geral.ModeloDF);
    PathSalvar       := PathMensal;
  end;

  if ACBrBPe1.DABPE <> nil then
  begin
    ACBrBPe1.DABPE.TipoDABPe := StrToTpImp(OK, IntToStr(rgTipoDaBPe.ItemIndex + 1));
    ACBrBPe1.DABPE.Logo      := edtLogoMarca.Text;
    ACBrBPe1.DABPE.ImprimeLogoLateral := chkLogoLateral.Checked;
  end;
end;

procedure TfrmACBrBPe.ConfigurarEmail;
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
  ACBrMail1.FromName := 'Projeto ACBr - ACBrBPe';
end;

procedure TfrmACBrBPe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');

  if ACBrBPe1.Bilhetes.Count > 0then
    MemoResp.Lines.Add('Empresa: ' + ACBrBPe1.Bilhetes.Items[0].BPe.Emit.xNome);
end;

procedure TfrmACBrBPe.PathClick(Sender: TObject);
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

procedure TfrmACBrBPe.PrepararImpressao;
begin
  ACBrPosPrinter1.Desativar;

  ACBrPosPrinter1.Modelo         := TACBrPosPrinterModelo(cbxModeloPosPrinter.ItemIndex);
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo(cbxPagCodigo.ItemIndex);
  ACBrPosPrinter1.Porta          := cbxPorta.Text;

  ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
  ACBrPosPrinter1.LinhasEntreCupons  := seLinhasPular.Value;
  ACBrPosPrinter1.EspacoEntreLinhas  := seEspLinhas.Value;
  ACBrPosPrinter1.CortaPapel         := cbCortarPapel.Checked;

  ACBrPosPrinter1.ApagarLogo(32, 32);
  ACBrPosPrinter1.GravarLogoArquivo(edtLogoMarca.Text, 32, 32);

  ACBrPosPrinter1.Ativar;
end;

procedure TfrmACBrBPe.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TfrmACBrBPe.sbPathBPeClick(Sender: TObject);
begin
  PathClick(edtPathBPe);
end;

procedure TfrmACBrBPe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrBPe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrBPe1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrBPe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrBPe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  ACBrBPe1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrBPe1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrBPe1.SSL.ListaCertificados[I] do
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

procedure TfrmACBrBPe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrBPe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

end.
