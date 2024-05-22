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

unit Frm_ACBrMDFe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrBase, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.XMLHTML, ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrDFe, ACBrDFeReport,
  ACBrMDFe, ACBrMail, ACBrMDFeDAMDFeClass, ACBrMDFeDAMDFeRLClass;

type
  TfrmACBrMDFe = class(TForm)
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
    Label32: TLabel;
    Label42: TLabel;
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    cbFormaEmissao: TComboBox;
    cbxAtualizarXML: TCheckBox;
    cbxExibirErroSchema: TCheckBox;
    edtFormatoAlerta: TEdit;
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
    sbPathMDFe: TSpeedButton;
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathMDFe: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathMDFe: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDAMDFE: TRadioGroup;
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
    btnConsultar: TButton;
    btnConsultarChave: TButton;
    btnConsultarRecibo: TButton;
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
    btnEncerramento: TButton;
    btnImprimirEvento: TButton;
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
    ACBrMail1: TACBrMail;
    OpenDialog1: TOpenDialog;
    btnStatusServ: TButton;
    ACBrMDFe1: TACBrMDFe;
    btnCriarEnviar: TButton;
    btnCriarEnviarSincrono: TButton;
    btnGerarPDFEvento: TButton;
    btnInclusaoCondutor: TButton;
    btnInclusaoDFe: TButton;
    btnConsultarNaoEncerrados: TButton;
    btnPagOperacaoTransp: TButton;
    ACBrMDFeDAMDFeRL1: TACBrMDFeDAMDFeRL;
    btnEnviarEventoEmail: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathMDFeClick(Sender: TObject);
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
    procedure ACBrMDFe1StatusChange(Sender: TObject);
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
    procedure btnCancelarXMLClick(Sender: TObject);
    procedure btnCancelarChaveClick(Sender: TObject);
    procedure btnEncerramentoClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnDistribuicaoDFeClick(Sender: TObject);
    procedure ACBrMDFe1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnCriarEnviarSincronoClick(Sender: TObject);
    procedure btnGerarPDFEventoClick(Sender: TObject);
    procedure btnConsultarNaoEncerradosClick(Sender: TObject);
    procedure btnInclusaoCondutorClick(Sender: TObject);
    procedure btnInclusaoDFeClick(Sender: TObject);
    procedure btnPagOperacaoTranspClick(Sender: TObject);
    procedure btnEnviarEventoEmailClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    procedure AlimentarMDFe(NumDFe: String);
    Procedure AlimentarComponente(NumDFe: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmACBrMDFe: TfrmACBrMDFe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  pcnAuxiliar, pmdfeMDFe, pcnConversao, pmdfeConversaoMDFe, pcnRetConsReciDFe,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  ACBrMDFeManifestos, ACBrMDFeConfiguracoes,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrMDFe }

procedure TfrmACBrMDFe.ACBrMDFe1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmACBrMDFe.ACBrMDFe1StatusChange(Sender: TObject);
begin
  case ACBrMDFe1.Status of
    stMDFeIdle:
      begin
        if ( frmStatus <> nil ) then
          frmStatus.Hide;
      end;

    stMDFeStatusServico:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stMDFeRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados da MDFe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stMDFeRetRecepcao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Recebendo dados da MDFe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stMDFeConsulta:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando MDFe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stMDFeRecibo:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando Recibo de Lote...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stMDFeEmail:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Email...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stMDFeEvento:
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

procedure TfrmACBrMDFe.AlimentarComponente(NumDFe: String);
begin
  ACBrMDFe1.Manifestos.Clear;

  AlimentarMDFe(NumDFe);
end;

procedure TfrmACBrMDFe.AlimentarMDFe(NumDFe: String);
begin
  with ACBrMDFe1.Manifestos.Add.MDFe do
  begin
    //
    // Dados de Identificação do MDF-e
    //
    Ide.cUF := 35;

    // TpcnTipoAmbiente = (taProducao, taHomologacao);
    case rgTipoAmb.ItemIndex of
      0: Ide.tpAmb := taProducao;
      1: Ide.tpAmb := taHomologacao;
    end;

    // TMDFeTpEmitente = ( teTransportadora, teTranspCargaPropria );
    Ide.tpEmit  := teTransportadora;
    Ide.modelo  := '58';
    Ide.serie   := 1;
    Ide.nMDF    := StrToIntDef(NumDFe, 0);
    Ide.cMDF    := GerarCodigoDFe(Ide.nMDF);
    // TMDFeModal = ( moRodoviario, moAereo, moAquaviario, moFerroviario );
    Ide.modal   := moRodoviario;
    Ide.dhEmi   := Now;
    // TpcnTipoEmissao = (teNormal, teContingencia, teSCAN, teDPEC, teFSDA);
    Ide.tpEmis  := teNormal;
    // TpcnProcessoEmissao = (peAplicativoContribuinte, peAvulsaFisco, peAvulsaContribuinte, peContribuinteAplicativoFisco);
    Ide.procEmi := peAplicativoContribuinte;
    Ide.verProc := '1.0';
    Ide.UFIni   := 'SP';
    Ide.UFFim   := 'SP';

    // incluir a o percurso caso a URIni seja diferente de UFFim e que exista UF
    // entre elas. (exemplo: UFIni = SP, UFFim = BA, UF de percuso = MG
    {
    with Ide.infPercurso.New do
    begin
      UFPer := 'XX';
    end;
    }

    with Ide.infMunCarrega.New do
    begin
      cMunCarrega := 3503208;
      xMunCarrega := 'ARARAQUARA';
    end;

    //
    // Dados do Emitente
    //
    Emit.CNPJCPF := edtEmitCNPJ.Text;
    Emit.IE      := edtEmitIE.Text;
    Emit.xNome   := edtEmitRazao.Text;
    Emit.xFant   := edtEmitFantasia.Text;

    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(edtEmitCodCidade.Text);
    Emit.EnderEmit.xMun    := edtEmitCidade.Text;
    Emit.EnderEmit.CEP     := StrToIntDef(edtEmitCEP.Text, 0);
    Emit.EnderEmit.UF      := edtEmitUF.Text;
    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.enderEmit.email   := 'endereco@provedor.com.br';

    rodo.infANTT.RNTRC := '12345678';

    with rodo.infANTT.infCIOT.New do
    begin
      CIOT    := '123456789012';
      CNPJCPF := edtEmitCNPJ.Text;
    end;

    with rodo.infANTT.valePed.disp.New do
    begin
      CNPJForn := edtEmitCNPJ.Text;
      CNPJPg   := edtEmitCNPJ.Text;
      nCompra  := '789';
    end;

    with rodo.infANTT.infContratante.New do
    begin
      CNPJCPF       := edtEmitCNPJ.Text;
      idEstrangeiro := '';
      xNome         := 'Nome do Contatratante';
    end;

    {
    Informações OPCIONAIS sobre o Pagamento do Frete
    }
    with rodo.infANTT.infPag.New do
    begin
      xNome         := 'Nome do Responsavel pelo Pagamento';
      idEstrangeiro := '';
      CNPJCPF       := edtEmitCNPJ.Text;

      vContrato := 1000.00;
      indPag    := ipPrazo;

      with rodo.infANTT.infPag[0].Comp.New do
      begin
        tpComp := tcValePedagio;
        vComp  := 250.00;
        xComp  := '';
      end;

      with rodo.infANTT.infPag[0].Comp.New do
      begin
        tpComp := tcImpostos;
        vComp  := 250.00;
        xComp  := '';
      end;

      with rodo.infANTT.infPag[0].Comp.New do
      begin
        tpComp := tcDespesas;
        vComp  := 250.00;
        xComp  := '';
      end;

      with rodo.infANTT.infPag[0].Comp.New do
      begin
        tpComp := tcOutros;
        vComp  := 250.00;
        xComp  := 'Outros custos do Frete';
      end;

      if rodo.infANTT.infPag[0].indPag = ipPrazo then
      begin
        with rodo.infANTT.infPag[0].infPrazo.New do
        begin
          nParcela := 1;
          dVenc    := StringToDateTime('10/03/2020');
          vParcela := 500.00;
        end;

        with rodo.infANTT.infPag[0].infPrazo.New do
        begin
          nParcela := 2;
          dVenc    := StringToDateTime('10/04/2020');
          vParcela := 500.00;
        end;
      end;

      // CNPJ da Instituição de pagamento Eletrônico do Frete
      rodo.infANTT.infPag[0].infBanc.CNPJIPEF := '12345678000199';

      if rodo.infANTT.infPag[0].infBanc.CNPJIPEF = '' then
      begin
        rodo.infANTT.infPag[0].infBanc.codBanco   := '001';
        rodo.infANTT.infPag[0].infBanc.codAgencia := '00001';
      end;
    end;

    // Informações sobre o veiculo de tração
    rodo.veicTracao.cInt    := '001';
    rodo.veicTracao.placa   := 'ABC1234';
    rodo.veicTracao.RENAVAM := '123456789';
    rodo.veicTracao.tara    := 5000;
    rodo.veicTracao.capKG   := 4500;
    rodo.veicTracao.capM3   := 400;

    // TpcteTipoRodado = (trNaoAplicavel, trTruck, trToco, trCavaloMecanico, trVAN, trUtilitario, trOutros);
    // Para o MDF-e não utilizar o trNaoAplicavel.
    rodo.veicTracao.tpRod := trTruck;

    // TpcteTipoCarroceria = (tcNaoAplicavel, tcAberta, tcFechada, tcGraneleira, tcPortaContainer, tcSider);
    rodo.veicTracao.tpCar := tcFechada;

    rodo.veicTracao.UF := edtEmitUF.Text;

    with rodo.veicTracao.condutor.New do
    begin
      xNome := 'JOAO';
      CPF   := '12345678912';
    end;

    with rodo.veicReboque.New do
    begin
      cInt    := '002';
      placa   := 'XYZ4567';
      RENAVAM := '123456789';
      tara    := 4000;
      capKG   := 3000;
      capM3   := 300;

      // TpcteTipoCarroceria = (tcNaoAplicavel, tcAberta, tcFechada, tcGraneleira, tcPortaContainer, tcSider);
      tpCar := tcFechada;

      UF := edtEmitUF.Text;
    end;

    with infDoc.infMunDescarga.New do
    begin
      cMunDescarga := 3550308;
      xMunDescarga := 'SAO PAULO';

      with infCTe.New do
      begin
        chCTe := '35110803911545000148570010000001011000001018';

        // Informações das Unidades de Transporte (Carreta/Reboque/Vagão)

        with infUnidTransp.New do
        begin
          //TpcnUnidTransp = ( utRodoTracao, utRodoReboque, utNavio, utBalsa, utAeronave, utVagao, utOutros );
          tpUnidTransp := utRodoTracao;
          idUnidTransp := 'ABC1234'; // informar a placa se rodoviário

          with lacUnidTransp.New do
          begin
            nLacre := '123';
          end;

          // Informações das Unidades de carga (Containeres/ULD/Outros)
          with infUnidCarga.New do
          begin
            // TpcnUnidCarga  = ( ucContainer, ucULD, ucPallet, ucOutros );
            tpUnidCarga := ucOutros;
            idUnidCarga := 'AB45'; // informar o numero da unidade da carga

            with lacUnidCarga.New do
            begin
              nLacre := '123';
            end;

            qtdRat := 1.0;
          end;

          qtdRat := 1.0;
        end;
      end; // fim do with

      with infCTe.New do
      begin
        chCTe := '35110803911545000148570010000001021000001023';

        // Informações das Unidades de Transporte (Carreta/Reboque/Vagão)

        with infUnidTransp.New do
        begin
          //TpcnUnidTransp = ( utRodoTracao, utRodoReboque, utNavio, utBalsa, utAeronave, utVagao, utOutros );
          tpUnidTransp := utRodoReboque;
          idUnidTransp := 'XYZ5678';

          with lacUnidTransp.New do
          begin
            nLacre := '321';
          end;

          // Informações das Unidades de carga (Containeres/ULD/Outros)
          with infUnidCarga.New do
          begin
            // TpcnUnidCarga  = ( ucContainer, ucULD, ucPallet, ucOutros );
            tpUnidCarga := ucOutros;
            idUnidCarga := 'DD98';

            with lacUnidCarga.New do
            begin
              nLacre := '321';
            end;

            qtdRat := 1.0;
          end;

          qtdRat := 1.0;
        end;
      end; // fim do with
    end;

    {
    Informações sobre o Seguro da Carga
    }
    if Ide.tpEmit = teTransportadora then
    begin
      with seg.New do
      begin
        respSeg := rsEmitente;

        if respSeg = rsTomadorServico then
          CNPJCPF := edtEmitCNPJ.Text; // CNPJ do Responsável pelo seguro

        xSeg := 'Nome da Seguradora';

        CNPJ := edtEmitCNPJ.Text; // CNPJ da Seguradora

        nApol := '1234';

        with aver.New do
        begin
          nAver := '67890';
        end;
      end;
    end;

    {
    Informações OPCIONAIS sobre o produto predominante
    }
    prodPred.tpCarga := tcGranelSolido;
    prodPred.xProd   := 'Descricao do Produto';
    prodPred.cEAN    := '78967142344650';
    prodPred.NCM     := '01012100';

    // Informações do Local de Carregamento
    // Informar somente quando MDF-e for de carga lotação
    prodPred.infLocalCarrega.CEP       := 14800000;
    prodPred.infLocalCarrega.latitude  := 0;
    prodPred.infLocalCarrega.longitude := 0;

    // Informações do Local de Descarregamento
    // Informar somente quando MDF-e for de carga lotação
    prodPred.infLocalDescarrega.CEP       := 14800000;
    prodPred.infLocalDescarrega.latitude  := 0;
    prodPred.infLocalDescarrega.longitude := 0;

    tot.qCTe := 2;
    tot.vCarga := 3500.00;
    // UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS);
    tot.cUnid  :=  uTon;
    tot.qCarga := 2.8000;

    with lacres.New do
    begin
      nLacre := '123';
    end;

    infAdic.infCpl     := 'Empresa optante pelo Simples Nacional.; Caminhao VW.';
    infAdic.infAdFisco := '';
  end;

  ACBrMDFe1.Manifestos.GerarMDFe;
end;

procedure TfrmACBrMDFe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrMDFe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrMDFe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrMDFe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrMDFe1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrMDFe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrMDFe.btnAdicionarProtocoloClick(Sender: TObject);
var
  NomeArq: String;
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    ACBrMDFe1.Consultar;

    ShowMessage(ACBrMDFe1.WebServices.Consulta.Protocolo);

    MemoResp.Lines.Text := ACBrMDFe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrMDFe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrMDFe1.WebServices.Consulta.RetornoWS, WBResposta);
    NomeArq := OpenDialog1.FileName;

    if pos(UpperCase('-MDFe.xml'), UpperCase(NomeArq)) > 0 then
       NomeArq := StringReplace(NomeArq, '-MDFe.xml', '-procMDFe.xml', [rfIgnoreCase]);

    ACBrMDFe1.Manifestos.Items[0].GravarXML(NomeArq);
    ShowMessage('Arquivo gravado em: ' + NomeArq);
    memoLog.Lines.Add('Arquivo gravado em: ' + NomeArq);
  end;
end;

procedure TfrmACBrMDFe.btnCancelarChaveClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, Justificativa: string;
begin
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Chave da MDF-e', Chave)) then
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

  ACBrMDFe1.EventoMDFe.Evento.Clear;

  with ACBrMDFe1.EventoMDFe.Evento.New do
  begin
    infEvento.chMDFe := Chave;
    infEvento.CNPJCPF  := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teCancelamento;
    infEvento.detEvento.xJust := Justificativa;
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrMDFe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrMDFe1.WebServices.EnvEvento.RetornoWS, WBResposta);
  (*
  ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chMDFe
  ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrMDFe.btnCancelarXMLClick(Sender: TObject);
var
  idLote, vAux: String;
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

    idLote := '1';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
       exit;

    if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa', vAux)) then
       exit;

    ACBrMDFe1.EventoMDFe.Evento.Clear;
    ACBrMDFe1.EventoMDFe.idLote := StrToInt(idLote);

    with ACBrMDFe1.EventoMDFe.Evento.New do
    begin
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.detEvento.xJust := vAux;
    end;

    ACBrMDFe1.EnviarEvento(StrToInt(idLote));

    MemoResp.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetornoWS;
    LoadXML(ACBrMDFe1.WebServices.EnvEvento.RetornoWS, WBResposta);
    ShowMessage(IntToStr(ACBrMDFe1.WebServices.EnvEvento.cStat));
    ShowMessage(ACBrMDFe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrMDFe.btnCarregarXMLEnviarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName, False);

    with ACBrMDFe1.Manifestos.Items[0].MDFe do
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
    end;

    ACBrMDFe1.Enviar(1);

    MemoResp.Lines.Text := ACBrMDFe1.WebServices.Retorno.RetWS;
    memoRespWS.Lines.Text := ACBrMDFe1.WebServices.Retorno.RetornoWS;
    LoadXML(ACBrMDFe1.WebServices.Retorno.RetornoWS, WBResposta);

    MemoDados.Lines.Add('');
    MemoDados.Lines.Add('Retorno do Envio do MDFe no modo Assíncrono');
    MemoDados.Lines.Add('tpAmb: '+ TpAmbToStr(ACBrMDFe1.WebServices.Retorno.TpAmb));
    MemoDados.Lines.Add('verAplic: '+ ACBrMDFe1.WebServices.Retorno.verAplic);
    MemoDados.Lines.Add('cStat: '+ IntToStr(ACBrMDFe1.WebServices.Retorno.cStat));
    MemoDados.Lines.Add('cUF: '+ IntToStr(ACBrMDFe1.WebServices.Retorno.cUF));
    MemoDados.Lines.Add('xMotivo: '+ ACBrMDFe1.WebServices.Retorno.xMotivo);
    MemoDados.Lines.Add('cMsg: '+ IntToStr(ACBrMDFe1.WebServices.Retorno.cMsg));
    MemoDados.Lines.Add('xMsg: '+ ACBrMDFe1.WebServices.Retorno.xMsg);
    MemoDados.Lines.Add('Recibo: '+ ACBrMDFe1.WebServices.Retorno.Recibo);
    MemoDados.Lines.Add('Protocolo: '+ ACBrMDFe1.WebServices.Retorno.Protocolo);
  end;
end;

procedure TfrmACBrMDFe.btnEncerramentoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

    ACBrMDFe1.EventoMDFe.Evento.Clear;

    with ACBrMDFe1.EventoMDFe.Evento.New do
    begin
      infEvento.chMDFe     := Copy(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID, 5, 44);
      infEvento.CNPJCPF    := edtEmitCNPJ.Text;
      infEvento.dhEvento   := now;
      infEvento.tpEvento   := teEncerramento;
      infEvento.nSeqEvento := 1;

      infEvento.detEvento.nProt := ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.nProt;
      infEvento.detEvento.dtEnc := Date;
      infEvento.detEvento.cUF   := StrToInt(Copy(IntToStr(ACBrMDFe1.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga),1,2));
      infEvento.detEvento.cMun  := ACBrMDFe1.Manifestos.Items[0].MDFe.infDoc.infMunDescarga.Items[0].cMunDescarga;
    end;

    ACBrMDFe1.EnviarEvento( 1 ); // 1 = Numero do Lote

    MemoResp.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetWS;
    LoadXML(ACBrMDFe1.WebServices.EnvEvento.RetWS, WBResposta);
  end;
end;

procedure TfrmACBrMDFe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrMDFe1.SSL.CertCNPJ);
end;

procedure TfrmACBrMDFe.btnConsultarChaveClick(Sender: TObject);
var
  vChave: String;
begin
  if not(InputQuery('WebServices Consultar', 'Chave da MDF-e:', vChave)) then
    exit;

  ACBrMDFe1.Manifestos.Clear;
  ACBrMDFe1.WebServices.Consulta.MDFeChave := vChave;
  ACBrMDFe1.WebServices.Consulta.Executar;

  MemoResp.Lines.Text := ACBrMDFe1.WebServices.Consulta.RetWS;
  memoRespWS.Lines.Text := ACBrMDFe1.WebServices.Consulta.RetornoWS;
  LoadXML(ACBrMDFe1.WebServices.Consulta.RetornoWS, WBResposta);
end;

procedure TfrmACBrMDFe.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    ACBrMDFe1.Consultar;

    ShowMessage(ACBrMDFe1.WebServices.Consulta.Protocolo);
    MemoResp.Lines.Text := ACBrMDFe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrMDFe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrMDFe1.WebServices.Consulta.RetornoWS, WBResposta);
  end;
end;

procedure TfrmACBrMDFe.btnConsultarNaoEncerradosClick(Sender: TObject);
var
  vCNPJ: String;
begin
  vCNPJ := '';

  if not(InputQuery('WebServices Consultar não encerrados', 'Informe o CNPJ:', vCNPJ)) then
    exit;

  try
    ACBrMDFe1.WebServices.ConsultaMDFeNaoEnc( vCNPJ );
  finally
    MemoResp.Lines.Text := ACBrMDFe1.WebServices.ConsMDFeNaoEnc.RetWS;
    memoRespWS.Lines.Text := ACBrMDFe1.WebServices.ConsMDFeNaoEnc.RetornoWS;
  end;

  LoadXML(ACBrMDFe1.WebServices.ConsMDFeNaoEnc.RetWS, WBResposta);
end;

procedure TfrmACBrMDFe.btnConsultarReciboClick(Sender: TObject);
var
  aux: String;
begin
  if not(InputQuery('Consultar Recibo Lote', 'Número do Recibo', aux)) then
    exit;

  ACBrMDFe1.WebServices.Recibo.Recibo := aux;
  ACBrMDFe1.WebServices.Recibo.Executar;

  MemoResp.Lines.Text := ACBrMDFe1.WebServices.Recibo.RetWS;
  memoRespWS.Lines.Text := ACBrMDFe1.WebServices.Recibo.RetornoWS;
  LoadXML(ACBrMDFe1.WebServices.Recibo.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Retorno do Consultar Recibo');
  MemoDados.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrMDFe1.WebServices.Recibo.tpAmb));
  MemoDados.Lines.Add('versao: ' + ACBrMDFe1.WebServices.Recibo.versao);
  MemoDados.Lines.Add('verAplic: ' + ACBrMDFe1.WebServices.Recibo.verAplic);
  MemoDados.Lines.Add('cStat: ' + IntToStr(ACBrMDFe1.WebServices.Recibo.cStat));
  MemoDados.Lines.Add('xMotivo: ' + ACBrMDFe1.WebServices.Recibo.xMotivo);
  MemoDados.Lines.Add('cUF: ' + IntToStr(ACBrMDFe1.WebServices.Recibo.cUF));
  MemoDados.Lines.Add('xMsg: ' + ACBrMDFe1.WebServices.Recibo.xMsg);
  MemoDados.Lines.Add('cMsg: ' + IntToStr(ACBrMDFe1.WebServices.Recibo.cMsg));
  MemoDados.Lines.Add('Recibo: ' + ACBrMDFe1.WebServices.Recibo.Recibo);
end;

procedure TfrmACBrMDFe.btnCriarEnviarClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Manifesto', vAux)) then
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

  ACBrMDFe1.Enviar(StrToInt(vNumLote));

  MemoResp.Lines.Text   := ACBrMDFe1.WebServices.Retorno.RetWS;
  memoRespWS.Lines.Text := ACBrMDFe1.WebServices.Retorno.RetornoWS;

  LoadXML(ACBrMDFe1.WebServices.Retorno.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  with MemoDados do
  begin
    Lines.Add('');
    Lines.Add('Retorno do Envio do MDFe em modo Assíncrono');
    Lines.Add('');
    Lines.Add('Chave: '    + ACBrMDFe1.Manifestos[0].MDFe.procMDFe.chMDFe);
    Lines.Add('Protocolo: '    + ACBrMDFe1.Manifestos[0].MDFe.procMDFe.nProt);
    Lines.Add('');
    Lines.Add('tpAmb: '     + TpAmbToStr(ACBrMDFe1.WebServices.Retorno.tpAmb));
    Lines.Add('verAplic: '  + ACBrMDFe1.WebServices.Retorno.verAplic);
    Lines.Add('cStat: '     + IntToStr(ACBrMDFe1.WebServices.Retorno.cStat));
    Lines.Add('xMotivo: '   + ACBrMDFe1.WebServices.Retorno.xMotivo);
    Lines.Add('cUF: '       + IntToStr(ACBrMDFe1.WebServices.Retorno.cUF));
    Lines.Add('xMsg: '      + ACBrMDFe1.WebServices.Retorno.Msg);
    Lines.Add('Recibo: '    + ACBrMDFe1.WebServices.Retorno.Recibo);
    Lines.Add('Protocolo: ' + ACBrMDFe1.WebServices.Retorno.Protocolo);
  end;
end;

procedure TfrmACBrMDFe.btnCriarEnviarSincronoClick(Sender: TObject);
var
  vAux, vNumLote: String;
begin
  if not(InputQuery('WebServices Enviar Síncrono', 'Numero do Manifesto', vAux)) then
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
  // 2o = Se True imprime automaticamente o DAMDFE
  // 3o = Se True o envio é no modo Síncrono, caso contrario Assíncrono.
  // Obs: no modo Síncrono só podemos enviar UM MDF-e por vez.
  ACBrMDFe1.Enviar(StrToInt(vNumLote), True, True);

  MemoResp.Lines.Text   := ACBrMDFe1.WebServices.Enviar.RetWS;
  memoRespWS.Lines.Text := ACBrMDFe1.WebServices.Enviar.RetornoWS;

  LoadXML(ACBrMDFe1.WebServices.Enviar.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  with MemoDados do
  begin
    Lines.Add('');
    Lines.Add('Retorno do Envio do MDFe em modo Síncrono');
    Lines.Add('');
    Lines.Add('Chave: '    + ACBrMDFe1.Manifestos[0].MDFe.procMDFe.chMDFe);
    Lines.Add('Protocolo: '    + ACBrMDFe1.Manifestos[0].MDFe.procMDFe.nProt);
    Lines.Add('');
    Lines.Add('tpAmb: '     + TpAmbToStr(ACBrMDFe1.WebServices.Enviar.tpAmb));
    Lines.Add('verAplic: '  + ACBrMDFe1.WebServices.Enviar.verAplic);
    Lines.Add('cStat: '     + IntToStr(ACBrMDFe1.WebServices.Enviar.cStat));
    Lines.Add('xMotivo: '   + ACBrMDFe1.WebServices.Enviar.xMotivo);
    Lines.Add('cUF: '       + IntToStr(ACBrMDFe1.WebServices.Enviar.cUF));
    Lines.Add('xMsg: '      + ACBrMDFe1.WebServices.Enviar.Msg);
  end;
end;

procedure TfrmACBrMDFe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrMDFe1.SSL.CertDataVenc));
end;

procedure TfrmACBrMDFe.btnDistribuicaoDFeClick(Sender: TObject);
var
  CNPJ, ultNSU, ANSU: string;
begin
  CNPJ := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'CNPJ/CPF do interessado no DF-e', CNPJ)) then
     exit;

  ultNSU := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'Último NSU recebido pelo ator', ultNSU)) then
     exit;

  ANSU := '';
  if not(InputQuery('WebServices Distribuição Documentos Fiscais', 'NSU específico', ANSU)) then
     exit;

  if ANSU = '' then
    ACBrMDFe1.DistribuicaoDFePorUltNSU(CNPJ, ultNSU)
  else
    ACBrMDFe1.DistribuicaoDFePorNSU(CNPJ, ANSU);

  MemoResp.Lines.Text := ACBrMDFe1.WebServices.DistribuicaoDFe.RetWS;
  memoRespWS.Lines.Text := ACBrMDFe1.WebServices.DistribuicaoDFe.RetornoWS;

  LoadXML(ACBrMDFe1.WebServices.DistribuicaoDFe.RetWS, WBResposta);
end;

procedure TfrmACBrMDFe.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrMDFe1.Manifestos.Clear;
  ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); //especifique um email valido
    //CC.Add('email_2@provedor.com.br');    //especifique um email valido
    ConfigurarEmail;
    ACBrMDFe1.Manifestos.Items[0].EnviarEmail(Para
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

procedure TfrmACBrMDFe.btnEnviarEventoEmailClick(Sender: TObject);
var
  Para: String;
  CC, Evento: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.xml)|*.xml|Todos os arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  Evento := TStringList.Create;
  CC := TStringList.Create;
  try
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);

    ACBrMDFe1.EventoMDFe.Evento.Clear;
    ACBrMDFe1.EventoMDFe.LerXML(OpenDialog1.FileName);

    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrMDFe1.EnviarEmailEvento(Para
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

procedure TfrmACBrMDFe.btnGerarPDFClick(Sender: TObject);
var
  CarregarMaisXML: Boolean;
begin
	CarregarMaisXML := true;
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;
  ACBrMDFe1.Manifestos.Clear;

  while CarregarMaisXML do
  begin
    if OpenDialog1.Execute then
      ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

    CarregarMaisXML := MessageDlg('Carregar mais Manifestos?', mtConfirmation, mbYesNoCancel, 0) = mrYes;
  end;

  ACBrMDFe1.Manifestos.ImprimirPDF;
end;

procedure TfrmACBrMDFe.btnGerarPDFEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  ACBrMDFe1.Manifestos.Clear;
  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*-procEventoMDFe.xml';
  OpenDialog1.Filter := 'Arquivos Evento (*-procEventoMDFe.xml)|*-procEventoMDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.EventoMDFe.Evento.Clear;
    ACBrMDFe1.EventoMDFe.LerXML(OpenDialog1.FileName);
    ACBrMDFe1.ImprimirEventoPDF;
  end;
end;

procedure TfrmACBrMDFe.btnGerarXMLClick(Sender: TObject);
var
  vAux: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero do Manifesto', vAux)) then
    exit;

  ACBrMDFe1.Manifestos.Clear;

  AlimentarComponente(vAux);

  ACBrMDFe1.Manifestos.Assinar;

  ACBrMDFe1.Manifestos.Items[0].GravarXML();

  ShowMessage('Arquivo gerado em: ' + ACBrMDFe1.Manifestos.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: ' + ACBrMDFe1.Manifestos.Items[0].NomeArq);

  MemoResp.Lines.LoadFromFile(ACBrMDFe1.Manifestos.Items[0].NomeArq);

  LoadXML(MemoResp.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrMDFe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrMDFe1.SSL.UseCertificateHTTP;
  ACBrMDFe1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrMDFe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrMDFe1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrMDFe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName,False);
    ACBrMDFe1.Manifestos.Imprimir;
  end;
end;

procedure TfrmACBrMDFe.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.EventoMDFe.Evento.Clear;
    ACBrMDFe1.EventoMDFe.LerXML(OpenDialog1.FileName);
    ACBrMDFe1.ImprimirEvento;
  end;
end;

procedure TfrmACBrMDFe.btnInclusaoCondutorClick(Sender: TObject);
var
  xNome, CPF: string;
begin
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  xNome := '';
  if not(InputQuery('WebServices Eventos: Inclusão de Condutor', 'Nome', xNome)) then
    exit;

  CPF := '';
  if not(InputQuery('WebServices Eventos: Inclusão de Condutor', 'CPF', CPF)) then
    exit;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

    ACBrMDFe1.EventoMDFe.Evento.Clear;

    with ACBrMDFe1.EventoMDFe.Evento.New do
    begin
      infEvento.chMDFe     := Copy(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID, 5, 44);
      infEvento.CNPJCPF    := edtEmitCNPJ.Text;
      infEvento.dhEvento   := now;
      infEvento.tpEvento   := teInclusaoCondutor;
      infEvento.nSeqEvento := 1;

      infEvento.detEvento.nProt := ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.nProt;
      infEvento.detEvento.xNome := xNome;
      infEvento.detEvento.CPF   := CPF;
    end;

    ACBrMDFe1.EnviarEvento( 1 ); // 1 = Numero do Lote

    MemoResp.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetWS;
    LoadXML(ACBrMDFe1.WebServices.EnvEvento.RetWS, WBResposta);
  end;
end;

procedure TfrmACBrMDFe.btnInclusaoDFeClick(Sender: TObject);
var
  cMunCarrega, xMunCarrega, cMunDescarrega, xMunDescarrega, chaveNFe: string;
begin
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  cMunCarrega := '';
  if not(InputQuery('WebServices Eventos: Inclusão de DF-e', 'Código do Municipio de Carregamento', cMunCarrega)) then
    exit;

  xMunCarrega := '';
  if not(InputQuery('WebServices Eventos: Inclusão de DF-e', 'Nome do Municipio de Carregamento', xMunCarrega)) then
    exit;

  cMunDescarrega := '';
  if not(InputQuery('WebServices Eventos: Inclusão de DF-e', 'Código do Municipio de Descarregamento', cMunDescarrega)) then
    exit;

  xMunDescarrega := '';
  if not(InputQuery('WebServices Eventos: Inclusão de DF-e', 'Nome do Municipio de Descarregamento', xMunDescarrega)) then
    exit;

  chaveNFe := '';
  if not(InputQuery('WebServices Eventos: Inclusão de DF-e', 'Chave da NF-e', chaveNFe)) then
    exit;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

    ACBrMDFe1.EventoMDFe.Evento.Clear;

    with ACBrMDFe1.EventoMDFe.Evento.New do
    begin
      infEvento.chMDFe     := Copy(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID, 5, 44);
      infEvento.CNPJCPF    := edtEmitCNPJ.Text;
      infEvento.dhEvento   := now;
      infEvento.tpEvento   := teInclusaoDFe;
      infEvento.nSeqEvento := 1;

      infEvento.detEvento.nProt := ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.nProt;

      infEvento.detEvento.cMunCarrega := StrToIntDef(cMunCarrega, 0);
      infEvento.detEvento.xMunCarrega := xMunCarrega;

      with InfEvento.detEvento.infDoc.New do
      begin
        infEvento.detEvento.infDoc[0].cMunDescarga := StrToIntDef(cMunDescarrega, 0);
        infEvento.detEvento.infDoc[0].xMunDescarga := xMunDescarrega;
        infEvento.detEvento.infDoc[0].chNFe        := chaveNFe;
      end;
    end;

    ACBrMDFe1.EnviarEvento( 1 ); // 1 = Numero do Lote

    MemoResp.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetWS;
    LoadXML(ACBrMDFe1.WebServices.EnvEvento.RetWS, WBResposta);
  end;
end;

procedure TfrmACBrMDFe.btnPagOperacaoTranspClick(Sender: TObject);
var
  qtdViagens, nroViagem, xNomeContratante, idEstrang, sCNPJCPF,
  vlrContrato, xindPag, xCNPJIPEF, xcodBanco, xcodAgencia: string;
  ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione o MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.xml';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.xml)|*-MDFe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  qtdViagens := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                      'Quantidade de Viagens', qtdViagens)) then
    exit;

  nroViagem := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                           'Numero de Viagens', nroViagem)) then
    exit;

  xNomeContratante := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                  'Nome do Contratante', xNomeContratante)) then
    exit;

  idEstrang := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                'Identificação do Estrangeiro', idEstrang)) then
    exit;

  sCNPJCPF := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                      'CNPJ/CPF do Contratante', sCNPJCPF)) then
    exit;

  vlrContrato := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                         'Valor do Contrato', vlrContrato)) then
    exit;

  xindPag := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                  'Indicador de Pagamento (0=A Vista, 1=A Prazo)', xindPag)) then
    exit;

  xCNPJIPEF := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
        'CNPJ da Instituição de pagamento Eletrônico do Frete', xCNPJIPEF)) then
    exit;

  xcodBanco := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                             'Código do Banco', xcodBanco)) then
    exit;

  xcodAgencia := '';
  if not(InputQuery('WebServices Eventos: Pagamento Operação Transporte',
                                         'Código da Agência', xcodAgencia)) then
    exit;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);

    ACBrMDFe1.EventoMDFe.Evento.Clear;

    with ACBrMDFe1.EventoMDFe.Evento.New do
    begin
      infEvento.chMDFe     := Copy(ACBrMDFe1.Manifestos.Items[0].MDFe.infMDFe.ID, 5, 44);
      infEvento.CNPJCPF    := edtEmitCNPJ.Text;
      infEvento.dhEvento   := now;
      infEvento.tpEvento   := tePagamentoOperacao;
      infEvento.nSeqEvento := 1;

      infEvento.detEvento.nProt := ACBrMDFe1.Manifestos.Items[0].MDFe.procMDFe.nProt;

      infEvento.detEvento.infViagens.qtdViagens := StrToIntDef(qtdViagens, 0);
      infEvento.detEvento.infViagens.nroViagem  := StrToIntDef(nroViagem, 0);

      ACBrMDFe1.EventoMDFe.Evento[0].InfEvento.detEvento.infPag.Clear;

      with ACBrMDFe1.EventoMDFe.Evento[0].InfEvento.detEvento.infPag.New do
      begin
        xNome         := xNomeContratante;
        idEstrangeiro := idEstrang;

        if idEstrangeiro = '' then
          CNPJCPF := sCNPJCPF;

        ACBrMDFe1.EventoMDFe.Evento[0].InfEvento.detEvento.infPag[0].Comp.Clear;
        with ACBrMDFe1.EventoMDFe.Evento[0].InfEvento.detEvento.infPag[0].Comp.New do
        begin
          tpComp := tcValePedagio;
          vComp  := StringToFloatDef(vlrContrato, 0);
          xComp  := '';
        end;

        vContrato := StringToFloatDef(vlrContrato, 0);
        indPag    := StrToTIndPag(ok, xindPag);

        if indPag = ipPrazo then
        begin
          ACBrMDFe1.EventoMDFe.Evento[0].InfEvento.detEvento.infPag[0].infPrazo.Clear;
          with ACBrMDFe1.EventoMDFe.Evento[0].InfEvento.detEvento.infPag[0].infPrazo.New do
          begin
            nParcela := 1;
            dVenc    := Date + 30;
            vParcela := StringToFloatDef(vlrContrato, 0);
          end;
        end;

        infBanc.CNPJIPEF := xCNPJIPEF;

        if infBanc.CNPJIPEF = '' then
        begin
          infBanc.codBanco   := xcodBanco;
          infBanc.codAgencia := xcodAgencia;
        end;
      end;
    end;

    ACBrMDFe1.EnviarEvento( 1 ); // 1 = Numero do Lote

    MemoResp.Lines.Text := ACBrMDFe1.WebServices.EnvEvento.RetWS;
    LoadXML(ACBrMDFe1.WebServices.EnvEvento.RetWS, WBResposta);
  end;
end;

procedure TfrmACBrMDFe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrMDFe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrMDFe1.SSL.CertCertificadora);
end;

procedure TfrmACBrMDFe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrMDFe1.SSL do
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

procedure TfrmACBrMDFe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrMDFe1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrMDFe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrMDFe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrMDFe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrMDFe.btnStatusServClick(Sender: TObject);
begin
  ACBrMDFe1.WebServices.StatusServico.Executar;

  MemoResp.Lines.Text := ACBrMDFe1.WebServices.StatusServico.RetWS;
  memoRespWS.Lines.Text := ACBrMDFe1.WebServices.StatusServico.RetornoWS;
  LoadXML(ACBrMDFe1.WebServices.StatusServico.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Retorno do Status de Serviço');
  MemoDados.Lines.Add('tpAmb: '    +TpAmbToStr(ACBrMDFe1.WebServices.StatusServico.tpAmb));
  MemoDados.Lines.Add('verAplic: ' +ACBrMDFe1.WebServices.StatusServico.verAplic);
  MemoDados.Lines.Add('cStat: '    +IntToStr(ACBrMDFe1.WebServices.StatusServico.cStat));
  MemoDados.Lines.Add('xMotivo: '  +ACBrMDFe1.WebServices.StatusServico.xMotivo);
  MemoDados.Lines.Add('cUF: '      +IntToStr(ACBrMDFe1.WebServices.StatusServico.cUF));
  MemoDados.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrMDFe1.WebServices.StatusServico.dhRecbto));
  MemoDados.Lines.Add('tMed: '     +IntToStr(ACBrMDFe1.WebServices.StatusServico.TMed));
  MemoDados.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrMDFe1.WebServices.StatusServico.dhRetorno));
  MemoDados.Lines.Add('xObs: '     +ACBrMDFe1.WebServices.StatusServico.xObs);
end;

procedure TfrmACBrMDFe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrMDFe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrMDFe1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrMDFe.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg: String;
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    pgRespostas.ActivePageIndex := 0;
    MemoResp.Lines.Add('');
    MemoResp.Lines.Add('');

    if not ACBrMDFe1.Manifestos.VerificarAssinatura(Msg) then
      MemoResp.Lines.Add('Erro: '+Msg)
    else
    begin
      MemoResp.Lines.Add('OK: Assinatura Válida');
      ACBrMDFe1.SSL.CarregarCertificadoPublico( ACBrMDFe1.Manifestos[0].MDFe.signature.X509Certificate );
      MemoResp.Lines.Add('Assinado por: '+ ACBrMDFe1.SSL.CertRazaoSocial);
      MemoResp.Lines.Add('CNPJ: '+ ACBrMDFe1.SSL.CertCNPJ);
      MemoResp.Lines.Add('Num.Série: '+ ACBrMDFe1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure TfrmACBrMDFe.btnValidarRegrasNegocioClick(Sender: TObject);
var
  Msg, Tempo: String;
  Inicio: TDateTime;
  Ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName);
    Inicio := Now;
    Ok := ACBrMDFe1.Manifestos.ValidarRegrasdeNegocios(Msg);
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

procedure TfrmACBrMDFe.btnValidarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a MDFe';
  OpenDialog1.DefaultExt := '*-MDFe.XML';
  OpenDialog1.Filter := 'Arquivos MDFe (*-MDFe.XML)|*-MDFe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrMDFe1.Configuracoes.Arquivos.PathSalvar;

  // Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrMDFe1.Configuracoes.Geral.ExibirErroSchema := False;
  ACBrMDFe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  if OpenDialog1.Execute then
  begin
    ACBrMDFe1.Manifestos.Clear;
    ACBrMDFe1.Manifestos.LoadFromFile(OpenDialog1.FileName, True);

    try
      ACBrMDFe1.Manifestos.Assinar;
      ACBrMDFe1.Manifestos.Validar;

      if ACBrMDFe1.Manifestos.Items[0].Alertas <> '' then
        MemoDados.Lines.Add('Alertas: '+ACBrMDFe1.Manifestos.Items[0].Alertas);

      ShowMessage('Manifesto de Documentos Fiscais Eletrônicos Valido');
    except
      on E: Exception do
      begin
        pgRespostas.ActivePage := Dados;
        MemoDados.Lines.Add('Exception: ' + E.Message);
        MemoDados.Lines.Add('Erro: ' + ACBrMDFe1.Manifestos.Items[0].ErroValidacao);
        MemoDados.Lines.Add('Erro Completo: ' + ACBrMDFe1.Manifestos.Items[0].ErroValidacaoCompleto);
      end;
    end;
  end;
end;

procedure TfrmACBrMDFe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrMDFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrMDFe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrMDFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrMDFe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrMDFe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrMDFe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrMDFe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrMDFe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrMDFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrMDFe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoMDFe;
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

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoMDFe) to High(TVersaoMDFe) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoMDFe), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBrMDFe.GravarConfiguracao;
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
    Ini.WriteBool(  'Arquivos', 'EmissaoPathMDFe',  cbxEmissaoPathMDFe.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathMDFe',         edtPathMDFe.Text);
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

    Ini.WriteInteger('DAMDFE', 'Tipo',      rgTipoDaMDFe.ItemIndex);
    Ini.WriteString( 'DAMDFE', 'LogoMarca', edtLogoMarca.Text);

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrMDFe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrMDFe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrMDFe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrMDFe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrMDFe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrMDFe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrMDFe.LerConfiguracao;
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

    cbVersaoDF.ItemIndex      := Ini.ReadInteger('Geral', 'VersaoDF',       0);
    ckSalvar.Checked          := Ini.ReadBool(   'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(   'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString( 'Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoMDFe), integer(cbVersaoDF.ItemIndex) ));

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
    cbxEmissaoPathMDFe.Checked  := Ini.ReadBool(  'Arquivos', 'EmissaoPathMDFe',   false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathMDFe.Text            := Ini.ReadString('Arquivos', 'PathMDFe',          '');
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
    edtEmitCidade.Text     := Ini.ReadString( 'Emitente', 'Cidade',      '');
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

    rgTipoDaMDFe.ItemIndex := Ini.ReadInteger('DAMDFe', 'Tipo',       0);
    edtLogoMarca.Text      := Ini.ReadString( 'DAMDFe', 'LogoMarca',  '');

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrMDFe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrMDFe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrMDFe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrMDFe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrMDFe1.SSL.DescarregarCertificado;

  with ACBrMDFe1.Configuracoes.Geral do
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
    VersaoDF         := TVersaoMDFe(cbVersaoDF.ItemIndex);
  end;

  with ACBrMDFe1.Configuracoes.WebServices do
  begin
    UF         := cbUF.Text;
    Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    Visualizar := cbxVisualizar.Checked;
    Salvar     := cbxSalvarSOAP.Checked;

    AjustaAguardaConsultaRet := cbxAjustarAut.Checked;

    if NaoEstaVazio(edtAguardar.Text) then
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
      edtIntervalo.Text := IntToStr(ACBrMDFe1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrMDFe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrMDFe1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathMDFe  := cbxEmissaoPathMDFe.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathMDFe         := edtPathMDFe.Text;
    PathEvento       := edtPathEvento.Text;
    PathMensal       := GetPathMDFe(0);
    PathSalvar       := PathMensal;
  end;

  if ACBrMDFe1.DAMDFe <> nil then
  begin
    ACBrMDFe1.DAMDFe.TipoDAMDFe := StrToTpImp(OK, IntToStr(rgTipoDaMDFe.ItemIndex + 1));
    ACBrMDFe1.DAMDFe.Logo       := edtLogoMarca.Text;

    ACBrMDFe1.DAMDFe.PathPDF      := PathMensal;
    ACBrMDFe1.DAMDFe.TamanhoPapel := tpA4;

    ACBrMDFe1.DAMDFe.MargemDireita  := 4;
    ACBrMDFe1.DAMDFe.MargemEsquerda := 4;
    ACBrMDFe1.DAMDFe.MargemSuperior := 7;
    ACBrMDFe1.DAMDFe.MargemInferior := 7;

//    ACBrMDFe1.DAMDFE.ImprimeDadosExtras := [];
//    ACBrMDFe1.DAMDFE.ImprimeDadosExtras := [deRelacaoDFe];
    ACBrMDFe1.DAMDFE.ImprimeDadosExtras := [deRelacaoDFe, deValorTotal];
    ACBrMDFe1.DAMDFE.ExibirMunicipioDescarregamento := True;
  end;
end;

procedure TfrmACBrMDFe.ConfigurarEmail;
begin
  ACBrMail1.Host := edtSmtpHost.Text;
  ACBrMail1.Port := edtSmtpPort.Text;
  ACBrMail1.Username := edtSmtpUser.Text;
  ACBrMail1.Password := edtSmtpPass.Text;
  ACBrMail1.From := edtSmtpUser.Text;
  ACBrMail1.SetSSL := cbEmailSSL.Checked; // SSL - Conexao Segura
  ACBrMail1.SetTLS := cbEmailSSL.Checked; // Auto TLS
  ACBrMail1.ReadingConfirmation := False; //Pede confirmacao de leitura do email
  ACBrMail1.UseThread := False;           //Aguarda Envio do Email(nao usa thread)
  ACBrMail1.FromName := 'Projeto ACBr - ACBrMDFe';
end;

procedure TfrmACBrMDFe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');

  if ACBrMDFe1.Manifestos.Count > 0then
    MemoResp.Lines.Add('Empresa: ' + ACBrMDFe1.Manifestos.Items[0].MDFe.Emit.xNome);
end;

procedure TfrmACBrMDFe.PathClick(Sender: TObject);
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

procedure TfrmACBrMDFe.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TfrmACBrMDFe.sbPathMDFeClick(Sender: TObject);
begin
  PathClick(edtPathMDFe);
end;

procedure TfrmACBrMDFe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrMDFe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrMDFe1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrMDFe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrMDFe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  ACBrMDFe1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrMDFe1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrMDFe1.SSL.ListaCertificados[I] do
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

procedure TfrmACBrMDFe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrMDFe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

end.
