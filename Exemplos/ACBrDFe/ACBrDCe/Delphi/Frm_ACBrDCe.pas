{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit Frm_ACBrDCe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrBase,
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrDFe, ACBrDFeReport,
  ACBrMail, ACBrDCe, ACBrDCe.DACEClass, ACBrDCe.DACERLClass;

type
  TfrmACBrDCe = class(TForm)
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
    sbPathDCe: TSpeedButton;
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathDCe: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathDCe: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDACE: TRadioGroup;
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
    btnImprimirEvento: TButton;
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
    btnCriarEnviarSincrono: TButton;
    btnGerarPDFEvento: TButton;
    btnEnviarEventoEmail: TButton;
    ACBrDCe1: TACBrDCe;
    ACBrDCeDACERL1: TACBrDCeDACERL;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathDCeClick(Sender: TObject);
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
    procedure btnConsultarClick(Sender: TObject);
    procedure btnConsultarChaveClick(Sender: TObject);
    procedure btnCancelarXMLClick(Sender: TObject);
    procedure btnCancelarChaveClick(Sender: TObject);
    procedure btnImprimirEventoClick(Sender: TObject);
    procedure btnCriarEnviarSincronoClick(Sender: TObject);
    procedure btnGerarPDFEventoClick(Sender: TObject);
    procedure btnEnviarEventoEmailClick(Sender: TObject);
    procedure ACBrDCe1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure ACBrDCe1StatusChange(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    procedure AlimentarDCe(NumDFe: String);
    Procedure AlimentarComponente(NumDFe: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmACBrDCe: TfrmACBrDCe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  pcnAuxiliar,
  ACBrDCe.Classes, pcnConversao, ACBrDCe.Conversao,
  pcnRetConsReciDFe,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  ACBrDCeDeclaracoes, ACBrDCeConfiguracoes,
  Frm_Status, Frm_SelecionarCertificado, ACBrXmlBase;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrDCe }

procedure TfrmACBrDCe.ACBrDCe1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmACBrDCe.ACBrDCe1StatusChange(Sender: TObject);
begin
  case ACBrDCe1.Status of
    stDCeIdle:
      begin
        if ( frmStatus <> nil ) then
          frmStatus.Hide;
      end;

    stDCeStatusServico:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Verificando Status do servico...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stDCeAutorizacao:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados da DCe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stDCeConsulta:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Consultando DCe...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stDCeEmail:
      begin
        if ( frmStatus = nil ) then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando Email...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stDCeEvento:
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

procedure TfrmACBrDCe.AlimentarComponente(NumDFe: String);
begin
  ACBrDCe1.Declaracoes.Clear;

  AlimentarDCe(NumDFe);
end;

procedure TfrmACBrDCe.AlimentarDCe(NumDFe: String);
var
  i: Integer;
  vTotalItem: Double;
begin
  with ACBrDCe1.Declaracoes.Add.DCe do
  begin
    //
    // Dados de Identificação do DC-e
    //
    Ide.cUF := UFparaCodigoUF(edtEmitUF.Text);
    Ide.cDC := StrToInt(RightStr(IntToStr(GerarCodigoDFe(Ide.nDC)), 6));
    Ide.serie := 1;
    Ide.nDC := StrToIntDef(NumDFe, 0);
    Ide.dhEmi := Now;
    // TACBrTipoEmissao = (teNormal, teOffLine);
    Ide.tpEmis := teNormal;
    // TEmitenteDCe = (teFisco, teMarketplace, teEmissorProprio, teTransportadora);
    Ide.tpEmit := teTransportadora;
    Ide.nSiteAutoriz := 0;
    Ide.verProc := 'ACBrDCe-v1.00';

    //
    // Dados do Emitente
    //
    Emit.CNPJCPF := edtEmitCNPJ.Text;
    Emit.idOutros := '';
    Emit.xNome := edtEmitRazao.Text;

    Emit.EnderEmit.xLgr := edtEmitLogradouro.Text;
    Emit.EnderEmit.nro := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun := StrToInt(edtEmitCodCidade.Text);
    Emit.EnderEmit.xMun := edtEmitCidade.Text;
    Emit.EnderEmit.UF := edtEmitUF.Text;
    Emit.EnderEmit.CEP := StrToIntDef(edtEmitCEP.Text, 0);
    Emit.EnderEmit.fone := edtEmitFone.Text;

    //
    // Dados do Fisco ou Marketplace ou Emissor Próprio ou Transportadora
    //
    case Ide.tpEmit of
      teFisco:
        begin
          Fisco.CNPJ := '';
          Fisco.xOrgao := '';
          Fisco.UF := '';
        end;
      teMarketplace:
        begin
          Marketplace.CNPJ := '';
          Marketplace.xNome := '';
          Marketplace.Site := '';
        end;
      teEmissorProprio:
        begin
          EmpEmisProp.CNPJ := edtEmitCNPJ.Text;
          EmpEmisProp.xNome := 'Transportadora Leva e Traz';
        end;
      teTransportadora:
        begin
          Transportadora.CNPJ := edtEmitCNPJ.Text;
          Transportadora.xNome := 'Transportadora Leva e Traz';
        end;
    end;

    //
    // Dados do Destinatário
    //
    Dest.CNPJCPF := '06760213874';
    Dest.idOutros := '';
    Dest.xNome := 'Joao';

    Dest.enderDest.xLgr := 'Rua Central';
    Dest.enderDest.nro := '50';
    Dest.enderDest.xCpl := '';
    Dest.enderDest.xBairro := 'Centro';
    Dest.enderDest.cMun := 3550308;
    Dest.enderDest.xMun := 'Sao Paulo';
    Dest.enderDest.UF := 'SP';
    Dest.enderDest.CEP := 14800;
    Dest.enderDest.fone := '33445566';
    Dest.enderDest.email := '';

    //
    // Dados do Autorizado a obter o XML (máximo 10)
    //
    with autXML.New do
    begin
      CNPJCPF := '06760213874';
    end;

    //
    // Dados do Detalhamento de itens (máximo 999)
    //
    with det.New do
    begin
      Prod.nItem := 1;
      Prod.xProd := 'Produto 1';
      Prod.NCM := '12345678';
      Prod.qCom := 1;
      Prod.vUnCom := 10;
      Prod.vProd := 10;

      infAdProd := '';
    end;

    //
    // Dados do Total
    //
    vTotalItem := 0;

    for i := 0 to det.Count -1 do
      vTotalItem := vTotalItem + det[i].Prod.vProd;

    total.vDC := vTotalItem;

    //
    // Dados do Transporte
    //
    // TModTrans = (mtCorreios, mtPropria, mtTransportadora);
    transp.modTrans := mtTransportadora;
    transp.CNPJTrans := edtEmitCNPJ.Text;

    //
    // Dados de informações adicionais
    //
    infAdic.infAdFisco := '';
    infAdic.infCpl := '';
    infAdic.infAdMarketplace := '';
    {
    //
    // Dados de Observação do Contribuinte (máximo 10)
    //
    with obscont.New do
    begin
      xCampo := '';
      xTexto := '';
    end;

    //
    // Dados de Observação do Marketplace (máximo 10)
    //
    with obsMarketplace.New do
    begin
      xCampo := '';
      xTexto := '';
    end;
    }
  end;

  ACBrDCe1.Declaracoes.GerarDCe;
end;

procedure TfrmACBrDCe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrDCe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrDCe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrDCe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrDCe1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrDCe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrDCe.btnAdicionarProtocoloClick(Sender: TObject);
var
  NomeArq: String;
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);
    ACBrDCe1.Consultar;

    ShowMessage(ACBrDCe1.WebServices.Consulta.Protocolo);

    MemoResp.Lines.Text := ACBrDCe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrDCe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrDCe1.WebServices.Consulta.RetornoWS, WBResposta);
    NomeArq := OpenDialog1.FileName;

    if pos(UpperCase('-DCe.xml'), UpperCase(NomeArq)) > 0 then
       NomeArq := StringReplace(NomeArq, '-DCe.xml', '-procDCe.xml', [rfIgnoreCase]);

    ACBrDCe1.Declaracoes.Items[0].GravarXML(NomeArq);
    ShowMessage('Arquivo gravado em: ' + NomeArq);
    memoLog.Lines.Add('Arquivo gravado em: ' + NomeArq);
  end;
end;

procedure TfrmACBrDCe.btnCancelarChaveClick(Sender: TObject);
var
  Chave, idLote, CNPJ, Protocolo, Justificativa: string;
begin
  Chave := '41240629406475000164990010000000011302911292';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Chave da DC-e', Chave)) then
     exit;
  Chave := Trim(OnlyNumber(Chave));

  idLote := '1';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
     exit;

  CNPJ := copy(Chave,7,14);
  if not(InputQuery('WebServices Eventos: Cancelamento', 'CNPJ ou o CPF do autor do Evento', CNPJ)) then
     exit;

  Protocolo:='1234567890123456';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Protocolo de Autorização', Protocolo)) then
     exit;

  Justificativa := 'Justificativa do Cancelamento';
  if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa do Cancelamento', Justificativa)) then
     exit;

  ACBrDCe1.EventoDCe.Evento.Clear;

  with ACBrDCe1.EventoDCe.Evento.New do
  begin
    infEvento.chDCe := Chave;
    infEvento.tpEmit := teTransportadora;
    infEvento.CNPJCPF := CNPJ;
    infEvento.CNPJCPFEmit := CNPJ;
    infEvento.dhEvento := now;
    infEvento.tpEvento := teCancelamento;
    infEvento.detEvento.xJust := Justificativa;
    infEvento.detEvento.nProt := Protocolo;
  end;

  ACBrDCe1.EnviarEvento(StrToInt(idLote));

  MemoResp.Lines.Text := ACBrDCe1.WebServices.EnvEvento.RetWS;
  memoRespWS.Lines.Text := ACBrDCe1.WebServices.EnvEvento.RetornoWS;
  LoadXML(ACBrDCe1.WebServices.EnvEvento.RetornoWS, WBResposta);
  (*
  ACBrDCe1.WebServices.EnvEvento.EventoRetorno.TpAmb
  ACBrDCe1.WebServices.EnvEvento.EventoRetorno.verAplic
  ACBrDCe1.WebServices.EnvEvento.EventoRetorno.cStat
  ACBrDCe1.WebServices.EnvEvento.EventoRetorno.xMotivo
  ACBrDCe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.chDCe
  ACBrDCe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento
  ACBrDCe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt
  *)
end;

procedure TfrmACBrDCe.btnCancelarXMLClick(Sender: TObject);
var
  idLote, vAux: String;
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);

    idLote := '1';
    if not(InputQuery('WebServices Eventos: Cancelamento', 'Identificador de controle do Lote de envio do Evento', idLote)) then
       exit;

    if not(InputQuery('WebServices Eventos: Cancelamento', 'Justificativa', vAux)) then
       exit;

    ACBrDCe1.EventoDCe.Evento.Clear;
    ACBrDCe1.EventoDCe.idLote := StrToInt(idLote);

    with ACBrDCe1.EventoDCe.Evento.New do
    begin
      infEvento.dhEvento := now;
      infEvento.tpEvento := teCancelamento;
      infEvento.detEvento.xJust := vAux;
    end;

    ACBrDCe1.EnviarEvento(StrToInt(idLote));

    MemoResp.Lines.Text := ACBrDCe1.WebServices.EnvEvento.RetWS;
    memoRespWS.Lines.Text := ACBrDCe1.WebServices.EnvEvento.RetornoWS;
    LoadXML(ACBrDCe1.WebServices.EnvEvento.RetornoWS, WBResposta);
    ShowMessage(IntToStr(ACBrDCe1.WebServices.EnvEvento.cStat));
//    ShowMessage(ACBrDCe1.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt);
  end;
end;

procedure TfrmACBrDCe.btnCarregarXMLEnviarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName, False);

    with ACBrDCe1.Declaracoes.Items[0].DCe do
    begin
      Emit.CNPJCPF           := edtEmitCNPJ.Text;
      Emit.xNome             := edtEmitRazao.Text;

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

    ACBrDCe1.Enviar('1');

    MemoResp.Lines.Text := ACBrDCe1.WebServices.Enviar.RetWS;
    memoRespWS.Lines.Text := ACBrDCe1.WebServices.Enviar.RetornoWS;
    LoadXML(ACBrDCe1.WebServices.Enviar.RetornoWS, WBResposta);

    MemoDados.Lines.Add('');
    MemoDados.Lines.Add('Envio DCe');
    MemoDados.Lines.Add('tpAmb: '+ TpAmbToStr(ACBrDCe1.WebServices.Enviar.TpAmb));
    MemoDados.Lines.Add('verAplic: '+ ACBrDCe1.WebServices.Enviar.verAplic);
    MemoDados.Lines.Add('cStat: '+ IntToStr(ACBrDCe1.WebServices.Enviar.cStat));
    MemoDados.Lines.Add('cUF: '+ IntToStr(ACBrDCe1.WebServices.Enviar.cUF));
    MemoDados.Lines.Add('xMotivo: '+ ACBrDCe1.WebServices.Enviar.xMotivo);
    MemoDados.Lines.Add('Protocolo: '+ ACBrDCe1.WebServices.Enviar.Protocolo);
  end;
end;

procedure TfrmACBrDCe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrDCe1.SSL.CertCNPJ);
end;

procedure TfrmACBrDCe.btnConsultarChaveClick(Sender: TObject);
var
  vChave: String;
begin
  vChave := '35240729406475000164990010000000021305905084';
  if not(InputQuery('WebServices Consultar', 'Chave da DC-e:', vChave)) then
    exit;

  ACBrDCe1.Declaracoes.Clear;
  ACBrDCe1.Consultar(vChave);

  MemoResp.Lines.Text := ACBrDCe1.WebServices.Consulta.RetWS;
  memoRespWS.Lines.Text := ACBrDCe1.WebServices.Consulta.RetornoWS;
  LoadXML(ACBrDCe1.WebServices.Consulta.RetornoWS, WBResposta);
end;

procedure TfrmACBrDCe.btnConsultarClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);
    ACBrDCe1.Consultar;

    ShowMessage(ACBrDCe1.WebServices.Consulta.Protocolo);
    MemoResp.Lines.Text := ACBrDCe1.WebServices.Consulta.RetWS;
    memoRespWS.Lines.Text := ACBrDCe1.WebServices.Consulta.RetornoWS;
    LoadXML(ACBrDCe1.WebServices.Consulta.RetornoWS, WBResposta);
  end;
end;

procedure TfrmACBrDCe.btnCriarEnviarSincronoClick(Sender: TObject);
var
  vNumDCe, vNumLote, vZipado: String;
  bZipado: Boolean;
begin
  vNumDCe := '';
  if not(InputQuery('WebServices Enviar Síncrono', 'Numero da Declaração', vNumDCe)) then
    exit;

  vNumLote := '1';
  if not(InputQuery('WebServices Enviar Síncrono', 'Numero do Lote', vNumLote)) then
    exit;

  vZipado := 'S';
  if not(InputQuery('WebServices Enviar Síncrono', 'Zipado (S/N)', vZipado)) then
    exit;

  bZipado := (vZipado = 'S') or (vZipado = 's');

  vNumLote := OnlyNumber(vNumLote);

  if Trim(vNumLote) = '' then
  begin
    MessageDlg('Número do Lote inválido.', mtError,[mbok], 0);
    exit;
  end;

  AlimentarComponente(vNumDCe);

  // Parâmetros do método Enviar:
  // 1o = Número do Lote
  // 2o = Se True (padrão) imprime automaticamente o DAMDFE
  // 3o = Se True (padrão) envia o XML zipado e codificado em base 64

  ACBrDCe1.Enviar(vNumLote, True, bZipado);

  MemoResp.Lines.Text   := ACBrDCe1.WebServices.Enviar.RetWS;
  memoRespWS.Lines.Text := ACBrDCe1.WebServices.Enviar.RetornoWS;

  LoadXML(ACBrDCe1.WebServices.Enviar.RetWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  with MemoDados do
  begin
    Lines.Add('');
    Lines.Add('Envio DCe');
    Lines.Add('tpAmb: '     + TpAmbToStr(ACBrDCe1.WebServices.Enviar.tpAmb));
    Lines.Add('verAplic: '  + ACBrDCe1.WebServices.Enviar.verAplic);
    Lines.Add('cStat: '     + IntToStr(ACBrDCe1.WebServices.Enviar.cStat));
    Lines.Add('xMotivo: '   + ACBrDCe1.WebServices.Enviar.xMotivo);
    Lines.Add('cUF: '       + IntToStr(ACBrDCe1.WebServices.Enviar.cUF));
    Lines.Add('xMsg: '      + ACBrDCe1.WebServices.Enviar.Msg);
    Lines.Add('Protocolo: ' + ACBrDCe1.WebServices.Enviar.Protocolo);
  end;
end;

procedure TfrmACBrDCe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrDCe1.SSL.CertDataVenc));
end;

procedure TfrmACBrDCe.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrDCe1.Declaracoes.Clear;
  ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);

  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); //especifique um email valido
    //CC.Add('email_2@provedor.com.br');    //especifique um email valido
    ConfigurarEmail;
    ACBrDCe1.Declaracoes.Items[0].EnviarEmail(Para
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

procedure TfrmACBrDCe.btnEnviarEventoEmailClick(Sender: TObject);
var
  Para: String;
  CC, Evento: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o DCe';
  OpenDialog1.DefaultExt := '*-DCe.xml';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.xml)|*-DCe.xml|Arquivos XML (*.xml)|*.xml|Todos os arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.xml)|*.xml|Todos os arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  Evento := TStringList.Create;
  CC := TStringList.Create;
  try
    Evento.Clear;
    Evento.Add(OpenDialog1.FileName);

//    ACBrDCe1.EventoDCe.Evento.Clear;
//    ACBrDCe1.EventoDCe.LerXML(OpenDialog1.FileName);

    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrDCe1.EnviarEmailEvento(Para
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

procedure TfrmACBrDCe.btnGerarPDFClick(Sender: TObject);
var
  CarregarMaisXML: Boolean;
begin
	CarregarMaisXML := true;
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;
  ACBrDCe1.Declaracoes.Clear;

  while CarregarMaisXML do
  begin
    if OpenDialog1.Execute then
      ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);

    CarregarMaisXML := MessageDlg('Carregar mais Declaracoes?', mtConfirmation, mbYesNoCancel, 0) = mrYes;
  end;

  ACBrDCe1.Declaracoes.ImprimirPDF;
end;

procedure TfrmACBrDCe.btnGerarPDFEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o DCe';
  OpenDialog1.DefaultExt := '*-DCe.xml';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.xml)|*-DCe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  ACBrDCe1.Declaracoes.Clear;
  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*-procEventoDCe.xml';
  OpenDialog1.Filter := 'Arquivos Evento (*-procEventoDCe.xml)|*-procEventoDCe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.EventoDCe.Evento.Clear;
    ACBrDCe1.EventoDCe.LerXML(OpenDialog1.FileName);
    ACBrDCe1.ImprimirEventoPDF;
  end;
end;

procedure TfrmACBrDCe.btnGerarXMLClick(Sender: TObject);
var
  vAux: String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero da Declaração', vAux)) then
    exit;

  ACBrDCe1.Declaracoes.Clear;

  AlimentarComponente(vAux);

  ACBrDCe1.Declaracoes.Assinar;

  ACBrDCe1.Declaracoes.Items[0].GravarXML();

  ShowMessage('Arquivo gerado em: ' + ACBrDCe1.Declaracoes.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: ' + ACBrDCe1.Declaracoes.Items[0].NomeArq);

  MemoResp.Lines.LoadFromFile(ACBrDCe1.Declaracoes.Items[0].NomeArq);

  LoadXML(MemoResp.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrDCe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrDCe1.SSL.UseCertificateHTTP;
  ACBrDCe1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrDCe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrDCe1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrDCe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName,False);
    ACBrDCe1.Declaracoes.Imprimir;
  end;
end;

procedure TfrmACBrDCe.btnImprimirEventoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);
  end;

  OpenDialog1.Title := 'Selecione o Evento';
  OpenDialog1.DefaultExt := '*.XML';
  OpenDialog1.Filter := 'Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.EventoDCe.Evento.Clear;
    ACBrDCe1.EventoDCe.LerXML(OpenDialog1.FileName);
    ACBrDCe1.ImprimirEvento;
  end;
end;

procedure TfrmACBrDCe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrDCe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrDCe1.SSL.CertCertificadora);
end;

procedure TfrmACBrDCe.btnLeituraX509Click(Sender: TObject);
begin
  with ACBrDCe1.SSL do
  begin
    CarregarCertificadoPublico(MemoDados.Lines.Text);
    MemoResp.Lines.Add(CertIssuerName);
    MemoResp.Lines.Add(CertRazaoSocial);
    MemoResp.Lines.Add(CertCNPJ);
    MemoResp.Lines.Add(CertSubjectName);
    MemoResp.Lines.Add(CertNumeroSerie);

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrDCe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrDCe1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrDCe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrDCe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrDCe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrDCe.btnStatusServClick(Sender: TObject);
begin
  ACBrDCe1.WebServices.StatusServico.Executar;

  MemoResp.Lines.Text := ACBrDCe1.WebServices.StatusServico.RetWS;
  memoRespWS.Lines.Text := ACBrDCe1.WebServices.StatusServico.RetornoWS;
  LoadXML(ACBrDCe1.WebServices.StatusServico.RetornoWS, WBResposta);

  pgRespostas.ActivePageIndex := 1;

  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Status Serviço');
  MemoDados.Lines.Add('tpAmb    : ' + TipoAmbienteToStr(ACBrDCe1.WebServices.StatusServico.tpAmb));
  MemoDados.Lines.Add('verAplic : ' + ACBrDCe1.WebServices.StatusServico.verAplic);
  MemoDados.Lines.Add('cStat    : ' + IntToStr(ACBrDCe1.WebServices.StatusServico.cStat));
  MemoDados.Lines.Add('xMotivo  : ' + ACBrDCe1.WebServices.StatusServico.xMotivo);
  MemoDados.Lines.Add('cUF      : ' + IntToStr(ACBrDCe1.WebServices.StatusServico.cUF));
  MemoDados.Lines.Add('dhRecbto : ' + DateTimeToStr(ACBrDCe1.WebServices.StatusServico.dhRecbto));
  MemoDados.Lines.Add('tMed     : ' + IntToStr(ACBrDCe1.WebServices.StatusServico.TMed));
  MemoDados.Lines.Add('dhRetorno: ' + DateTimeToStr(ACBrDCe1.WebServices.StatusServico.dhRetorno));
  MemoDados.Lines.Add('xObs     : ' + ACBrDCe1.WebServices.StatusServico.xObs);
end;

procedure TfrmACBrDCe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrDCe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrDCe1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrDCe.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg: String;
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);
    pgRespostas.ActivePageIndex := 0;
    MemoResp.Lines.Add('');
    MemoResp.Lines.Add('');

    if not ACBrDCe1.Declaracoes.VerificarAssinatura(Msg) then
      MemoResp.Lines.Add('Erro: '+Msg)
    else
    begin
      MemoResp.Lines.Add('OK: Assinatura Válida');
      ACBrDCe1.SSL.CarregarCertificadoPublico( ACBrDCe1.Declaracoes[0].DCe.signature.X509Certificate );
      MemoResp.Lines.Add('Assinado por: '+ ACBrDCe1.SSL.CertRazaoSocial);
      MemoResp.Lines.Add('CNPJ: '+ ACBrDCe1.SSL.CertCNPJ);
      MemoResp.Lines.Add('Num.Série: '+ ACBrDCe1.SSL.CertNumeroSerie);

      ShowMessage('ASSINATURA VÁLIDA');
    end;
  end;
end;

procedure TfrmACBrDCe.btnValidarRegrasNegocioClick(Sender: TObject);
var
  Msg, Tempo: String;
  Inicio: TDateTime;
  Ok: Boolean;
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);
    Inicio := Now;
    Ok := ACBrDCe1.Declaracoes.ValidarRegrasdeNegocios(Msg);
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

procedure TfrmACBrDCe.btnValidarXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a DCe';
  OpenDialog1.DefaultExt := '*-DCe.XML';
  OpenDialog1.Filter := 'Arquivos DCe (*-DCe.XML)|*-DCe.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrDCe1.Configuracoes.Arquivos.PathSalvar;

  // Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrDCe1.Configuracoes.Geral.ExibirErroSchema := False;
  ACBrDCe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  if OpenDialog1.Execute then
  begin
    ACBrDCe1.Declaracoes.Clear;
    ACBrDCe1.Declaracoes.LoadFromFile(OpenDialog1.FileName);

    try
      ACBrDCe1.Declaracoes.Validar;

      if ACBrDCe1.Declaracoes.Items[0].Alertas <> '' then
        MemoDados.Lines.Add('Alertas: '+ACBrDCe1.Declaracoes.Items[0].Alertas);

      ShowMessage('Declaração de Conteúdo Eletrônica Valida');
    except
      on E: Exception do
      begin
        pgRespostas.ActivePage := Dados;
        MemoDados.Lines.Add('Exception: ' + E.Message);
        MemoDados.Lines.Add('Erro: ' + ACBrDCe1.Declaracoes.Items[0].ErroValidacao);
        MemoDados.Lines.Add('Erro Completo: ' + ACBrDCe1.Declaracoes.Items[0].ErroValidacaoCompleto);
      end;
    end;
  end;
end;

procedure TfrmACBrDCe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrDCe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrDCe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrDCe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrDCe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrDCe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrDCe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
    ACBrDCe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrDCe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrDCe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrDCe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoDCe;
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

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoDCe) to High(TVersaoDCe) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoDCe), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBrDCe.GravarConfiguracao;
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
    Ini.WriteBool(  'Arquivos', 'EmissaoPathDCe',   cbxEmissaoPathDCe.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathDCe',          edtPathDCe.Text);
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

    Ini.WriteInteger('DACE', 'Tipo',      rgTipoDACE.ItemIndex);
    Ini.WriteString( 'DACE', 'LogoMarca', edtLogoMarca.Text);

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrDCe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrDCe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrDCe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrDCe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrDCe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrDCe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrDCe.LerConfiguracao;
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

    cbVersaoDF.ItemIndex      := Ini.ReadInteger('Geral', 'VersaoDF',       0);
    ckSalvar.Checked          := Ini.ReadBool(   'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(   'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString( 'Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoDCe), integer(cbVersaoDF.ItemIndex) ));

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
    cbxEmissaoPathDCe.Checked   := Ini.ReadBool(  'Arquivos', 'EmissaoPathDCe',   false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathDCe.Text             := Ini.ReadString('Arquivos', 'PathDCe',          '');
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

    rgTipoDACE.ItemIndex := Ini.ReadInteger('DACE', 'Tipo',       0);
    edtLogoMarca.Text    := Ini.ReadString( 'DACE', 'LogoMarca',  '');

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrDCe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrDCe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrDCe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrDCe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrDCe1.SSL.DescarregarCertificado;

  with ACBrDCe1.Configuracoes.Geral do
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
    VersaoDF         := TVersaoDCe(cbVersaoDF.ItemIndex);
  end;

  with ACBrDCe1.Configuracoes.WebServices do
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
      edtIntervalo.Text := IntToStr(ACBrDCe1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrDCe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrDCe1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathDCe  := cbxEmissaoPathDCe.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathDCe         := edtPathDCe.Text;
    PathEvento       := edtPathEvento.Text;
    PathMensal       := GetPathDCe(0);
    PathSalvar       := PathMensal;
  end;

  if ACBrDCe1.DACE <> nil then
  begin
    ACBrDCe1.DACE.TipoDACE := StrToTpImp(OK, IntToStr(rgTipoDACE.ItemIndex + 1));
    ACBrDCe1.DACE.Logo := edtLogoMarca.Text;

    ACBrDCe1.DACE.PathPDF := PathMensal;
    ACBrDCe1.DACE.TamanhoPapel := tpA4;
    ACBrDCe1.DACE.Usuario := 'ACBr';

    ACBrDCe1.DACE.MargemDireita  := 4;
    ACBrDCe1.DACE.MargemEsquerda := 4;
    ACBrDCe1.DACE.MargemSuperior := 7;
    ACBrDCe1.DACE.MargemInferior := 7;

    ACBrDCe1.DACE.ImprimeDadosExtras := [deRelacaoDFe, deValorTotal];
  end;
end;

procedure TfrmACBrDCe.ConfigurarEmail;
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
  ACBrMail1.FromName := 'Projeto ACBr - ACBrDCe';
end;

procedure TfrmACBrDCe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');

  if ACBrDCe1.Declaracoes.Count > 0then
    MemoResp.Lines.Add('Empresa: ' + ACBrDCe1.Declaracoes.Items[0].DCe.Emit.xNome);
end;

procedure TfrmACBrDCe.PathClick(Sender: TObject);
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

procedure TfrmACBrDCe.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TfrmACBrDCe.sbPathDCeClick(Sender: TObject);
begin
  PathClick(edtPathDCe);
end;

procedure TfrmACBrDCe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrDCe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrDCe1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrDCe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtLogoMarca.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrDCe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  ACBrDCe1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrDCe1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrDCe1.SSL.ListaCertificados[I] do
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

procedure TfrmACBrDCe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrDCe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

end.
