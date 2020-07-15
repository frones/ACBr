{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
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

unit FormTelaPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, Buttons, Grids, DBGrids, Menus, ACBrTEFPayGoComum, ACBrPOS,
  ACBrPOSPGWebAPI, ACBrPosPrinter, ACBrMail, ACBrConsultaCNPJ, ACBrCEP,
  ACBrIBGE, ACBrNFe, ACBrNFeDANFeESCPOS;

type
  TItemPedido = record
    CodItem: Integer;
    Descricao: String;
    UN: String;
    Qtd: Double;
    PrecoUnit: Double;
  end;

  TPedido = record
    NumPedido: Integer;
    Nome: String;
    DataHora: TDateTime;
    ValorTotal: Double;
    Items: array of TItemPedido;
  end;

  TPedidos = array of TPedido;

  { TfrPOSTEFServer }

  TfrPOSTEFServer = class(TForm)
    ACBrCEP1: TACBrCEP;
    ACBrConsultaCNPJ1: TACBrConsultaCNPJ;
    ACBrIBGE1: TACBrIBGE;
    ACBrMail1: TACBrMail;
    ACBrNFe1: TACBrNFe;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    ACBrPOS1: TACBrPOS;
    ACBrPosPrinter1: TACBrPosPrinter;
    btConfiguracao: TBitBtn;
    btOperacao: TBitBtn;
    btStatusServico: TBitBtn;
    btVoltar2: TBitBtn;
    btIniciarParaServidor: TBitBtn;
    btLerParametros: TBitBtn;
    btCertInfo: TBitBtn;
    btLimparLog: TBitBtn;
    btVoltar: TBitBtn;
    btSalvarParametros: TBitBtn;
    cbCryptLib: TComboBox;
    cbEmailSSL: TCheckBox;
    cbHttpLib: TComboBox;
    cbIMprimirViaReduzida: TCheckBox;
    cbSSLLib: TComboBox;
    cbSSLType: TComboBox;
    cbSuportaDesconto: TCheckBox;
    cbSuportaSaque: TCheckBox;
    cbTipoAplicacao: TComboBox;
    cbWebServiceUF: TComboBox;
    cbxAmbiente: TComboBox;
    cbXmlSignLib: TComboBox;
    cbxTipoEmpresa: TComboBox;
    cbUtilizarSaldoTotalVoucher: TCheckBox;
    cbxEmitCidade: TComboBox;
    cbxEmitUF: TComboBox;
    cbxIndRatISSQN: TComboBox;
    cbxRegTribISSQN: TComboBox;
    cbxUTF8: TCheckBox;
    edAplicacaoNome: TEdit;
    edAplicacaoVersao: TEdit;
    edLog: TEdit;
    edMensagemBoasVindas: TEdit;
    edNomeDLL: TEdit;
    edRazaoSocial: TEdit;
    edRegistro: TEdit;
    edtCertArqPFX: TEdit;
    edtCodigoAtivacao: TEdit;
    edtCodUF: TEdit;
    edtEmailAssunto: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCEP: TEdit;
    edtEmitCNPJ: TEdit;
    edtCertNumSerie: TEdit;
    edtCertSenha: TEdit;
    edtCertURLPFX: TEdit;
    edtPathSchemas: TEdit;
    edtSmtpHost: TEdit;
    edtSmtpPass: TEdit;
    edtSmtpPort: TEdit;
    edtSmtpUser: TEdit;
    edtSwHAssinatura: TEdit;
    edtSwHCNPJ: TEdit;
    edtTokenID: TEdit;
    edtProxyHost: TEdit;
    Image1: TImage;
    Image2: TImage;
    imgErrWebService: TImage;
    imgErrPathSchemas: TImage;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label6: TLabel;
    mmEmailMsg: TMemo;
    rgTipoAmb: TComboBox;
    seProxyPorta: TSpinEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    edtTokenCSC: TEdit;
    gbCertificado: TGroupBox;
    gbBibliotecas: TGroupBox;
    gbProxy: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox6: TGroupBox;
    imgErrCertificado: TImage;
    imgErrTokenID: TImage;
    imgErrHttpLib: TImage;
    imgErrCryptLib: TImage;
    imgErrSSLLib: TImage;
    imgErrTokenCSC: TImage;
    imgErrXmlSignLib: TImage;
    Label10: TLabel;
    Label3: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label5: TLabel;
    Label51: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lCryptLib: TLabel;
    lHttpLib: TLabel;
    lSSLLib: TLabel;
    lSSLLib1: TLabel;
    lTimeOut: TLabel;
    lXmlSign: TLabel;
    OpenDialog1: TOpenDialog;
    pEmitCodCidade: TPanel;
    pEmitCodUF: TPanel;
    edtEmitComp: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitIE: TEdit;
    edtEmitIM: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitRazao: TEdit;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    imgErrCEP: TImage;
    imgErrCidade: TImage;
    imgErrCNPJ: TImage;
    imgErrFone: TImage;
    imgErrRazaoSocial: TImage;
    imgErrUF: TImage;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label52: TLabel;
    Label7: TLabel;
    lURLTEF: TLabel;
    mLog: TMemo;
    pConfEmissor: TPanel;
    pgConfig: TPageControl;
    pOperacao: TPanel;
    pBotoesOperacao: TPanel;
    pBotoesConfiguracao: TPanel;
    pConfiguracao: TPanel;
    pgPrincipal: TPageControl;
    pLogs: TPanel;
    pTerminais: TPanel;
    SbArqLog: TSpeedButton;
    sbConsultaCEP: TSpeedButton;
    sbConsultaCNPJ: TSpeedButton;
    sbNomeDLL: TSpeedButton;
    sbtnCaminhoCert: TSpeedButton;
    sbtnGetCert: TSpeedButton;
    sbtnNumSerie: TSpeedButton;
    seMaxConexoes: TSpinEdit;
    seNumeroCaixa: TSpinEdit;
    sePagCod: TSpinEdit;
    sePortaTCP: TSpinEdit;
    seTimeOut: TSpinEdit;
    sfeVersaoEnt: TFloatSpinEdit;
    sgTerminais: TStringGrid;
    Splitter1: TSplitter;
    spPathSchemas: TSpeedButton;
    tsMenuPrincipal: TTabSheet;
    tsConfSAT: TTabSheet;
    tsConfEmissor: TTabSheet;
    tsConfEMail: TTabSheet;
    tsConfNFCe: TTabSheet;
    tsConfPosTef: TTabSheet;
    tsConfiguracao: TTabSheet;
    tsOperacao: TTabSheet;
    procedure ACBrPOS1GravarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ACBrPOS1MudaEstadoTerminal(const TerminalId: String; EstadoAtual,
      EstadoAnterior: TACBrPOSPGWebEstadoTerminal);
    procedure ACBrPOS1NovaConexao(const TerminalId: String; const Model: String;
      const MAC: String; const SerNo: String);
    procedure btConfiguracaoClick(Sender: TObject);
    procedure btOperacaoClick(Sender: TObject);
    procedure btVoltar2Click(Sender: TObject);
    procedure btIniciarParaServidorClick(Sender: TObject);
    procedure btCertInfoClick(Sender: TObject);
    procedure btVoltarClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btLimparLogClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btStatusServicoClick(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbWebServiceUFChange(Sender: TObject);
    procedure cbxEmitCidadeChange(Sender: TObject);
    procedure cbxEmitUFChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure edtCertArqPFXChange(Sender: TObject);
    procedure edtCertNumSerieChange(Sender: TObject);
    procedure edtEmitCEPChange(Sender: TObject);
    procedure edtEmitCEPExit(Sender: TObject);
    procedure edtEmitCNPJChange(Sender: TObject);
    procedure edtEmitFoneChange(Sender: TObject);
    procedure edtEmitRazaoChange(Sender: TObject);
    procedure edtPathSchemasChange(Sender: TObject);
    procedure edtTokenCSCChange(Sender: TObject);
    procedure edtTokenIDChange(Sender: TObject);
    procedure edtOnlyNumberKeyPress(Sender: TObject; var Key: char);
    procedure edtCertURLPFXChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lURLTEFClick(Sender: TObject);
    procedure SbArqLogClick(Sender: TObject);
    procedure sbConsultaCEPClick(Sender: TObject);
    procedure sbConsultaCNPJClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnNumSerieClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
  private
    fPedidos: TPedidos;
    fcUF: Integer;
    fcMunList: TStringList;

    procedure AtualizarSSLLibsCombo;
    function GetNomeArquivoConfiguracao: String;
    procedure TratarException(Sender : TObject; E : Exception);
    procedure AdicionarLinhaLog(AMensagem: String);
    function EstadoTerminal(AEstado: TACBrPOSPGWebEstadoTerminal): String;
    procedure ValidarConfigCertificado;
    procedure ValidarConfigWebService;

    procedure LigarAlertasdeErrosDeConfiguracao;
    procedure ConfigurarACBrNFe;
    procedure ConfigurarACBrPOS;

  protected
    procedure IrParaMenuPrincipal;
    procedure IrParaOperacaoPOS;
    procedure IrParaConfiguracao;
    procedure IniciarServidorPOS;
    procedure PararServidorPOS;

    procedure CarregarListaDeCidades(cUF: Integer);

    procedure ExecutarFluxoEntrega(const TerminalId: String);
    procedure ExibirMenuPedidosEntrega(const TerminalId: String);
    procedure ImprimirPedido(const TerminalId: String; IndicePedido: Integer);
    procedure ImprimirNFCe(const TerminalId: String; IndicePedido: Integer);

    procedure ExecutarFluxoFechamentoMesa(const TerminalId: String);

    procedure ExecutarFluxoFechamentoBomba(const TerminalId: String);
    procedure ExecutarReimpressao(const TerminalId: String);

    procedure EfetuarPagamento(const TerminalId: String; const Descricao: String;
      ValorTotal: Double);
    procedure CriarNFCe(IndicePedido: Integer);
    procedure IncluirPedidosSimulados;
  public
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;

    procedure LerConfiguracao;
    procedure AplicarConfiguracao;
    procedure GravarConfiguracao;
    procedure GravarNumeracaoNFCe(const TerminalId: String; Lote, Numero: Integer);
  end;

var
  frPOSTEFServer: TfrPOSTEFServer;

implementation

uses
  IniFiles, math, dateutils, TypInfo,
  FormConsultaCNPJ, FormSelecionarCertificado,
  ACBrUtil, ACBrValidador, ACBrDFeSSL, pcnConversao, blcksock;

{$R *.lfm}

{ TfrPOSTEFServer }

procedure TfrPOSTEFServer.FormCreate(Sender: TObject);
var
  K: TpcnRegTribISSQN;
  L: TpcnindRatISSQN;
  T: TSSLLib;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
begin
  fcUF := 0;
  fcMunList := TStringList.Create;

  pgPrincipal.ShowTabs := False;
  pgPrincipal.ActivePageIndex := 0;
  pgConfig.ActivePageIndex := 0;

  cbxRegTribISSQN.Items.Clear ;
  For K := Low(TpcnRegTribISSQN) to High(TpcnRegTribISSQN) do
     cbxRegTribISSQN.Items.Add( GetEnumName(TypeInfo(TpcnRegTribISSQN), integer(K) ));

  cbxIndRatISSQN.Items.Clear ;
  For L := Low(TpcnindRatISSQN) to High(TpcnindRatISSQN) do
     cbxIndRatISSQN.Items.Add( GetEnumName(TypeInfo(TpcnindRatISSQN), integer(L) ));

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

  LerConfiguracao;
  Application.OnException := @TratarException;

  IncluirPedidosSimulados;

  ImageList1.GetBitmap(16, imgErrCNPJ.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrRazaoSocial.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCEP.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrFone.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrUF.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCidade.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrSSLLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCryptLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrHttpLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrXmlSignLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrWebService.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrTokenID.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrTokenCSC.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrPathSchemas.Picture.Bitmap);
end;

procedure TfrPOSTEFServer.FormDestroy(Sender: TObject);
begin
  fcMunList.Free;
end;

procedure TfrPOSTEFServer.btLimparLogClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TfrPOSTEFServer.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrPOSTEFServer.btStatusServicoClick(Sender: TObject);
var
  SL: TStringList;
begin
  ConfigurarACBrNFe;
  ACBrNFe1.WebServices.StatusServico.Executar;
  SL := TStringList.Create;
  try
    SL.Add('versao: ' + ACBrNFe1.WebServices.StatusServico.versao);
    SL.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.StatusServico.tpAmb));
    SL.Add('verAplic: ' + ACBrNFe1.WebServices.StatusServico.verAplic);
    SL.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.cStat));
    SL.Add('xMotivo: ' + ACBrNFe1.WebServices.StatusServico.xMotivo);
    SL.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.cUF));
    SL.Add('dhRecbto: ' + DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRecbto));
    SL.Add('tMed: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.TMed));
    SL.Add('dhRetorno: ' + DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRetorno));
    SL.Add('xObs: ' + ACBrNFe1.WebServices.StatusServico.xObs);

    MessageDlg('Status Serviço', SL.Text, mtInformation, [mbOK], 0);
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrNFe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrPOSTEFServer.cbWebServiceUFChange(Sender: TObject);
begin
  ValidarConfigWebService;
end;

procedure TfrPOSTEFServer.cbxEmitCidadeChange(Sender: TObject);
var
  Ok: Boolean;
begin
  Ok := (cbxEmitCidade.ItemIndex >= 0);
  imgErrCidade.Visible := not Ok;
  if Ok then
    pEmitCodCidade.Caption := FcMunList[cbxEmitCidade.ItemIndex];
end;

procedure TfrPOSTEFServer.cbxEmitUFChange(Sender: TObject);
var
  cUF: Integer;
  Ok: Boolean;
begin
  Ok := (cbxEmitUF.ItemIndex >= 0);
  imgErrUF.Visible := not Ok;

  if Ok then
  begin
    cUF := UFtoCUF(cbxEmitUF.Text);
    if (cUF <> FcUF) then
    begin
      pEmitCodUF.Caption := IntToStrZero(cUF, 2);
      CarregarListaDeCidades(cUF);
    end;
  end;
end;

procedure TfrPOSTEFServer.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.edtCertURLPFXChange(Sender: TObject);
begin
if (edtCertURLPFX.Text <> '') then
  begin
    if (edtCertNumSerie.Text <> '') then
      edtCertNumSerie.Text := '';

    if (edtCertArqPFX.Text = '') then
      edtCertArqPFX.Text := 'CertA1.pfx';
  end;

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.edtCertArqPFXChange(Sender: TObject);
begin
  if (edtCertArqPFX.Text <> '') then
  begin
    if (edtCertNumSerie.Text <> '') then
      edtCertNumSerie.Text := '';
  end;

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.edtCertNumSerieChange(Sender: TObject);
begin
  if (edtCertNumSerie.Text <> '') then
  begin
    if (edtCertURLPFX.Text <> '') then
      edtCertURLPFX.Text := '';

    if (edtCertArqPFX.Text <> '') then
      edtCertArqPFX.Text := '';
  end;

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.edtEmitCEPChange(Sender: TObject);
begin
  if (Length(edtEmitCEP.Text) > 5) then
  begin
    edtEmitCEP.Text := FormatarMascaraDinamica(OnlyNumber(edtEmitCEP.Text), '*****-***');
    edtEmitCEP.SelStart := Length(edtEmitCEP.Text);
  end;

  imgErrCEP.Visible := (Length(edtEmitCEP.Text) < 9);
  sbConsultaCEP.Visible := not imgErrCEP.Visible;
end;

procedure TfrPOSTEFServer.edtEmitCEPExit(Sender: TObject);
begin
  if (not imgErrCEP.Visible) and (edtEmitLogradouro.Text = '') then
    sbConsultaCEP.Click;
end;

procedure TfrPOSTEFServer.edtEmitCNPJChange(Sender: TObject);
begin
  if (Length(edtEmitCNPJ.Text) > 2) then
  begin
    edtEmitCNPJ.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtEmitCNPJ.Text), '**.***.***/****-**');
    edtEmitCNPJ.SelStart := Length(edtEmitCNPJ.Text);
  end;

  imgErrCNPJ.Visible := (Length(edtEmitCNPJ.Text) < 18) or
                        (ACBrValidador.ValidarCNPJ(edtEmitCNPJ.Text) <> '');
  sbConsultaCNPJ.Visible := not imgErrCNPJ.Visible;
end;

procedure TfrPOSTEFServer.edtEmitFoneChange(Sender: TObject);
var
  AStr, Mascara: String;
begin
  if (Length(edtEmitFone.Text) > 2) then
  begin
    AStr := OnlyNumber(edtEmitFone.Text);
    Mascara := '(**)****-****';
    case Length(AStr) of
      10:
      begin
        if (copy(AStr,1,1) = '0') and (copy(AStr,3,2) = '00') then  // 0300,0500,0800,0900
          Mascara := '****-***-****';
      end;
      11: Mascara := '(**)*****-****';
      12: Mascara := '+**(**)****-****';
    end;

    edtEmitFone.Text := ACBrValidador.FormatarMascaraDinamica(AStr, Mascara);
    edtEmitFone.SelStart := Length(edtEmitFone.Text);
  end;

  imgErrFone.Visible := (Length(OnlyNumber(edtEmitFone.Text)) < 10);
end;

procedure TfrPOSTEFServer.edtEmitRazaoChange(Sender: TObject);
begin
  imgErrRazaoSocial.Visible := (Length(edtEmitRazao.Text) < 4);
end;

procedure TfrPOSTEFServer.edtPathSchemasChange(Sender: TObject);
begin
  ValidarConfigWebService;
end;

procedure TfrPOSTEFServer.edtTokenCSCChange(Sender: TObject);
begin
  imgErrTokenCSC.Visible := (edtTokenCSC.Text = '');
end;

procedure TfrPOSTEFServer.edtTokenIDChange(Sender: TObject);
begin
  imgErrTokenID.Visible := (edtTokenID.Text = '');
end;

procedure TfrPOSTEFServer.edtOnlyNumberKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet( Key, [#8,'0'..'9'] ) then
    Key := #0;
end;

procedure TfrPOSTEFServer.btVoltarClick(Sender: TObject);
begin
  GravarConfiguracao;
  IrParaMenuPrincipal;
end;

procedure TfrPOSTEFServer.btVoltar2Click(Sender: TObject);
begin
  PararServidorPOS;
  IrParaMenuPrincipal;
end;

procedure TfrPOSTEFServer.ACBrPOS1GravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TfrPOSTEFServer.ACBrPOS1MudaEstadoTerminal(const TerminalId: String;
  EstadoAtual, EstadoAnterior: TACBrPOSPGWebEstadoTerminal);
var
  Linha, i: Integer;
  EstadoStr: String;
begin
  EstadoStr := EstadoTerminal(EstadoAtual);
  AdicionarLinhaLog('- MudaEstadoTerminal: '+TerminalId+', '+EstadoStr);
  Linha := -1;
  For i := 0 to sgTerminais.RowCount-1 do
  begin
    if sgTerminais.Cells[0,i] = TerminalId then
    begin
      Linha := i;
      Break;
    end;
  end;

  if (Linha < 0) then
  begin
    if sgTerminais.RowCount = 0 then
      sgTerminais.RowCount := 1;

    Linha := sgTerminais.RowCount;
    sgTerminais.RowCount := sgTerminais.RowCount + 1;
  end;

  sgTerminais.Rows[Linha][0] := TerminalId;
  sgTerminais.Rows[Linha][1] := EstadoStr;
end;

procedure TfrPOSTEFServer.ACBrPOS1NovaConexao(const TerminalId: String;
  const Model: String; const MAC: String; const SerNo: String);
begin
  case cbTipoAplicacao.ItemIndex of
    1: ExecutarFluxoFechamentoMesa(TerminalId);
    2: ExecutarFluxoFechamentoBomba(TerminalId)
  else
    ExecutarFluxoEntrega(TerminalId)
  end;
end;

procedure TfrPOSTEFServer.btConfiguracaoClick(Sender: TObject);
begin
  IrParaConfiguracao;
end;

procedure TfrPOSTEFServer.btOperacaoClick(Sender: TObject);
begin
  IrParaOperacaoPOS;
end;

procedure TfrPOSTEFServer.btIniciarParaServidorClick(Sender: TObject);
begin
  if ACBrPOS1.Inicializada then
    PararServidorPOS
  else
    IniciarServidorPOS;
end;

procedure TfrPOSTEFServer.btCertInfoClick(Sender: TObject);
var
  SL: TStringList;
begin
  ConfigurarACBrNFe;
  ACBrNFe1.SSL.CarregarCertificado;
  SL := TStringList.Create;
  try
    SL.Add('Número de Série: '+ACBrNFe1.SSL.CertNumeroSerie);
    SL.Add('Válido até: '+FormatDateBr(ACBrNFe1.SSL.CertDataVenc));
    SL.Add('Subject Name: '+ACBrNFe1.SSL.CertSubjectName);
    SL.Add('Razão Social: ' + ACBrNFe1.SSL.CertRazaoSocial);
    SL.Add('CNPJ/CPF: ' + ACBrNFe1.SSL.CertCNPJ);
    SL.Add('Emissor: ' + ACBrNFe1.SSL.CertIssuerName);
    SL.Add('Certificadora: ' + ACBrNFe1.SSL.CertCertificadora);

    MessageDlg('Informações do Certificado', SL.Text, mtInformation, [mbOK], 0);
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrPOSTEFServer.lURLTEFClick(Sender: TObject);
begin
  OpenURL('https://projetoacbr.com.br/tef/');
end;

procedure TfrPOSTEFServer.SbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim,edLog.Text) = 0 then
    AFileLog := ExtractFilePath( Application.ExeName ) + edLog.Text
  else
    AFileLog := edLog.Text;

  OpenURL( AFileLog );
end;

procedure TfrPOSTEFServer.sbConsultaCEPClick(Sender: TObject);
var
  Erro: string;
  EndAchado: TACBrCEPEndereco;
  cUF: Integer;
begin
  Erro := '';
  try
    ACBrCEP1.BuscarPorCEP(OnlyNumber(edtEmitCEP.Text));
    if (ACBrCEP1.Enderecos.Count > 0) then
    begin
      EndAchado := ACBrCEP1.Enderecos[0];
      edtEmitLogradouro.Text := Trim(EndAchado.Tipo_Logradouro + ' ' + EndAchado.Logradouro);
      edtEmitBairro.Text := EndAchado.Bairro;
      edtEmitCEP.Text := ACBrValidador.FormatarCEP(EndAchado.CEP);
      edtEmitComp.Text := EndAchado.Complemento;
      cUF := UFtoCUF(EndAchado.UF);
      pEmitCodUF.Caption := IntToStr(cUF);
      CarregarListaDeCidades(cUF);
      cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(EndAchado.UF);
      cbxEmitCidade.ItemIndex := cbxEmitCidade.Items.IndexOf(EndAchado.Municipio);
      cbxEmitCidadeChange(nil);
      edtEmitNumero.SetFocus;
    end;
  except
    MessageDlg('Erro ao executar Consulta do CEP', mtError, [mbOK], 0);
  end;
end;

procedure TfrPOSTEFServer.sbConsultaCNPJClick(Sender: TObject);
var
  frConsultaCNPJ: TfrConsultaCNPJ;
  MR: TModalResult;
  cUF: Integer;
begin
  frConsultaCNPJ := TfrConsultaCNPJ.Create(Self);
  try
    MR := frConsultaCNPJ.ShowModal;

    if (MR = mrOK) then
    begin
      try
        if ACBrConsultaCNPJ1.Consulta(edtEmitCNPJ.Text, frConsultaCNPJ.edtCaptcha.Text) then
        begin
          //EditTipo.Text := ACBrConsultaCNPJ1.EmpresaTipo;
          edtEmitRazao.Text := ACBrConsultaCNPJ1.RazaoSocial;
          edtEmitFantasia.Text := ACBrConsultaCNPJ1.Fantasia;
          edtEmitLogradouro.Text := ACBrConsultaCNPJ1.Endereco;
          edtEmitNumero.Text := ACBrConsultaCNPJ1.Numero;
          edtEmitComp.Text := ACBrConsultaCNPJ1.Complemento;
          edtEmitCEP.Text := ACBrConsultaCNPJ1.CEP;
          edtEmitBairro.Text := ACBrConsultaCNPJ1.Bairro;
          cUF := UFtoCUF(ACBrConsultaCNPJ1.UF);
          pEmitCodUF.Caption := IntToStr(cUF);
          CarregarListaDeCidades(cUF);
          cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(ACBrConsultaCNPJ1.UF);
          pEmitCodCidade.Caption := Trim(ACBrConsultaCNPJ1.IBGE_Municipio);
          cbxEmitCidade.ItemIndex := fcMunList.IndexOf(pEmitCodCidade.Caption);
          cbxEmitCidadeChange(nil);
        end;
      except
        MessageDlg('Errro ao Consultar CNPJ'+sLineBreak+'Verifique o Captcha', mtError, [mbOK], 0);
      end;
    end;
  finally
     frConsultaCNPJ.Free;
  end;
end;

procedure TfrPOSTEFServer.sbtnCaminhoCertClick(Sender: TObject);
var
  AFile: String;
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ApplicationPath;

  if OpenDialog1.Execute then
  begin
    AFile := OpenDialog1.FileName;
    if (pos(ApplicationPath, AFile) > 0) then
      AFile := ExtractFileName(AFile);

    edtCertArqPFX.Text := AFile;
  end;
end;

procedure TfrPOSTEFServer.sbtnGetCertClick(Sender: TObject);
begin
  edtCertNumSerie.Text := ACBrNFe1.SSL.SelecionarCertificado;
end;

procedure TfrPOSTEFServer.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  AddRow: Boolean;
  frmSelecionarCertificado: TfrmSelecionarCertificado;
begin
  frmSelecionarCertificado := TfrmSelecionarCertificado.Create(Self);
  try
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
      edtCertNumSerie.Text := frmSelecionarCertificado.StringGrid1.Cells[0, frmSelecionarCertificado.StringGrid1.Row];
  finally
    frmSelecionarCertificado.Free;
  end;
end;

procedure TfrPOSTEFServer.spPathSchemasClick(Sender: TObject);
var
  Dir: string;
begin
  if (Trim(edtPathSchemas.Text) = '') then
     Dir := ApplicationPath
  else
     Dir := edtPathSchemas.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    edtPathSchemas.Text := Dir;
end;

function TfrPOSTEFServer.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName,'.ini') ;
end;

procedure TfrPOSTEFServer.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');

  if pgPrincipal.ActivePage = tsConfiguracao then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TfrPOSTEFServer.AdicionarLinhaLog(AMensagem: String);
begin
  mLog.Lines.Add(AMensagem);
end;

function TfrPOSTEFServer.EstadoTerminal(AEstado: TACBrPOSPGWebEstadoTerminal
  ): String;
begin
  case AEstado of
    statConectado: Result := 'Conectado';
    statOcupado: Result := 'Ocupado';
    statDesconectado: Result := 'Desconectado';
    statEsperaConexao: Result := 'Reconectando';
  else
    Result := 'Desconhecido';
  end;
end;

procedure TfrPOSTEFServer.IrParaOperacaoPOS;
begin
  AdicionarLinhaLog('- IrParaOperacaoPOS');
  GravarConfiguracao;
  sgTerminais.Clear;
  pgPrincipal.ActivePage := tsOperacao;
end;

procedure TfrPOSTEFServer.IrParaConfiguracao;
begin
  AdicionarLinhaLog('- IrParaConfiguracao');
  pgPrincipal.ActivePage := tsConfiguracao;
end;

procedure TfrPOSTEFServer.IniciarServidorPOS;
begin
  AdicionarLinhaLog('- IniciarServidorPOS');

  sgTerminais.Clear;
  ConfigurarACBrPOS;
  ACBrPOS1.Inicializar;
  btIniciarParaServidor.Caption := 'Parar Servidor';
  btIniciarParaServidor.ImageIndex := 9;
end;

procedure TfrPOSTEFServer.PararServidorPOS;
begin
  AdicionarLinhaLog('- PararServidorPOS');
  ACBrPOS1.DesInicializar;
  btIniciarParaServidor.Caption := 'Iniciar Servidor';
  btIniciarParaServidor.ImageIndex := 8;
end;

procedure TfrPOSTEFServer.CarregarListaDeCidades(cUF: Integer);
var
  i: Integer;
  Cidade: TACBrIBGECidade;
begin
  if (cUF = 0) or (fcUF = cUF) then
    Exit;

  fcUF := cUF;
  try
    ACBrIBGE1.BuscarPorcUF(FcUF);
    cbxEmitCidade.Items.Clear;
    cbxEmitCidade.ItemIndex := -1;
    fcMunList.Clear;
    for i := 0 to ACBrIBGE1.Cidades.Count-1 do
    begin
      Cidade := ACBrIBGE1.Cidades[i];
      cbxEmitCidade.Items.Add(Cidade.Municipio);
      fcMunList.Add(IntToStr(Cidade.CodMunicipio));
    end;

    if (cbxEmitCidade.Items.Count > 0) then
    begin
      cbxEmitCidade.ItemIndex := 0;
      cbxEmitCidadeChange(Nil);
    end;
  except
    MessageDlg('Erro ao carregar cidades', mtError, [mbOK], 0);
  end;
end;

procedure TfrPOSTEFServer.ExecutarFluxoEntrega(const TerminalId: String);
var
  OP: SmallInt;
  OK: Boolean;
begin
  OK := False;
  repeat
    OP := ACBrPOS1.ExecutarMenu( TerminalId,
                                 'VER PEDIDOS|REIMPRESSAO|ADMINISTRATIVO|SAIR',
                                 'ACBR - DEMO ENTREGA' );
    case OP of
      0: ExibirMenuPedidosEntrega(TerminalId);
      1: ExecutarReimpressao(TerminalId);
      2: ACBrPOS1.ExecutarTransacaoTEF(TerminalId, operAdmin);
    else
      Exit;
    end;
  until OK;
end;

procedure TfrPOSTEFServer.ExibirMenuPedidosEntrega(const TerminalId: String);
var
  SL: TStringList;
  OP: SmallInt;
  i, IndicePedido, ColDis: Integer;
begin
  SL := TStringList.Create;
  try
    // *** NOTA *** Aqui você deve ler os Pedidos Pendentes no Seu Banco de Dados, com algum Filtro
    for i := 0 to Length(fPedidos)-1 do
      SL.Add(LeftStr(fPedidos[i].Nome,8)+' R$ '+FormatFloatBr(fPedidos[i].ValorTotal,'###0.00') );

    SL.Add('V O L T A R');
    OP := ACBrPOS1.ExecutarMenu(TerminalId, SL, 'ESCOLHA O PEDIDO');

    if (OP >= 0) and (OP < SL.Count-1) then
    begin
      ColDis := CACBrPOSPGWebColunasDisplay;
      IndicePedido := OP;
      ImprimirPedido(TerminalId, IndicePedido);
      ACBrPOS1.ExibirMensagem(TerminalId,
        PadCenter('PEDIDO '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4)+' IMPRESSO', ColDis)+
        PadCenter('CONFIRA O PEDIDO', ColDis), 5);

      EfetuarPagamento( TerminalId,
                        'PEDIDO: '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4),
                        fPedidos[IndicePedido].ValorTotal );
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.ImprimirPedido(const TerminalId: String;
  IndicePedido: Integer);
var
  SL: TStringList;
  ColImp, ColExp, i: Integer;
  TotalItem: Currency;
begin
  // *** NOTA *** Aqui você deve ler os Itens do Pedido Selecionado, no Seu Banco de Dados

  ColImp := CACBrPOSPGWebColunasImpressora;
  ColExp := Trunc(ColImp/2)-1;

  // Montando relatório com o Pedido
  SL := TStringList.Create;
  try
    SL.Add( '<e>'+PadCenter('PROJETO ACBR', ColExp) );
    SL.Add( PadCenter(' https://projetoacbr.com.br/tef ', ColImp, '-')  );
    SL.Add( '' );
    SL.Add( PadSpace( 'Pedido: '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4)+'|'+
                      'Data: '+FormatDateTimeBr(fPedidos[IndicePedido].DataHora),
              ColImp, '|') );
    SL.Add( '<e>Nome: '+fPedidos[IndicePedido].Nome );
    SL.Add( StringOfChar('-', ColImp) );
    for i := 0 to Length(fPedidos[IndicePedido].Items)-1 do
    begin
      TotalItem := fPedidos[IndicePedido].Items[i].Qtd * fPedidos[IndicePedido].Items[i].PrecoUnit;
      SL.Add( IntToStrZero(fPedidos[IndicePedido].Items[i].CodItem, 4)   + ' '+
              PadRight(fPedidos[IndicePedido].Items[i].Descricao,35) );
      SL.Add( PadSpace( FormatFloatBr(fPedidos[IndicePedido].Items[i].Qtd, '##0.00')+ ' '+
              PadRight(fPedidos[IndicePedido].Items[i].UN,2)+ ' x ' +
                        FormatFloatBr(fPedidos[IndicePedido].Items[i].PrecoUnit, '#,##0.00') + '|' +
                        FormatFloatBr( TotalItem, '#,##0.00'),
                ColImp, '|') );
    end;
    SL.Add( StringOfChar('-', ColImp) );
    SL.Add( '<e>'+PadSpace( 'TOTAL:|'+
                            FormatFloatBr( fPedidos[IndicePedido].ValorTotal, '#,##0.00'),
                    ColExp, '|') );

    ACBrPOS1.ImprimirTexto(TerminalId, SL.Text);
    ACBrPOS1.AvancarPapel(TerminalId);
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.ImprimirNFCe(const TerminalId: String;
  IndicePedido: Integer);
begin
  { *** NOTA ***
    Aqui você deve Ler o Pedido e gerar uma NFCe, baseado nos itens dele }

end;

procedure TfrPOSTEFServer.ExecutarFluxoFechamentoMesa(const TerminalId: String);
begin
  raise Exception.Create('Exemplo ainda não implementado');
end;

procedure TfrPOSTEFServer.ExecutarFluxoFechamentoBomba(const TerminalId: String);
begin
  raise Exception.Create('Exemplo ainda não implementado');
end;

procedure TfrPOSTEFServer.ExecutarReimpressao(const TerminalId: String);
begin
  try
    ACBrPOS1.ImprimirComprovantesTEF(TerminalId, prnAmbas, False);
  except
    On E: Exception do
    begin
      ACBrPOS1.Beep(TerminalId, beepAlerta);
      ACBrPOS1.ExibirMensagem(TerminalId, E.Message, 5);
    end;
  end;
end;

procedure TfrPOSTEFServer.EfetuarPagamento(const TerminalId: String;
  const Descricao: String; ValorTotal: Double);
var
  OP: SmallInt;
  ColDis: Integer;
begin
  ColDis := CACBrPOSPGWebColunasDisplay;
  OP := ACBrPOS1.ExecutarMenu(TerminalId, 'CARTAO|DINHEIRO|CANCELAR', 'EFETUAR O PAGAMENTO');

  case OP of
    0:
      ACBrPOS1.ExecutarTransacaoPagamento(TerminalId, ValorTotal);
    1:
    begin
      ACBrPOS1.ExibirMensagem(TerminalId,
        LeftStr(Descricao, ColDis) + sLineBreak +
        'TOTAL.: '+FormatFloatBr(ValorTotal) + sLineBreak +
        '* D I N H E I R O *'+ sLineBreak +
        '* CONFIRA O TROCO *', 5);
    end
  else
    raise Exception.Create('PAGAMENTO CANCELADO');
  end;
end;

procedure TfrPOSTEFServer.CriarNFCe(IndicePedido: Integer);
var
  Ok: Boolean;
  BaseCalculo,
  ValorICMS: Double;
begin
  {
    *** NOTA ***
    1 - Aqui você deve consultar seu Banco de Dados, Ler o Pedido e calcular os Impostos
        corretamente
    2 - Nesse Exemplo, a NFCe está sendo gerada com Dados de Emissor inválios, e
        portanto, ela não será transmitida, pois o nosso objetivo é apenas mostrar a
        Impressão de NFCe no POS TEF
  }

(*  ACBrNFe1.NotasFiscais.Clear;
  with ACBrNFe1.NotasFiscais.Add.NFe do
  begin
    Ide.natOp     := 'VENDA';
    Ide.indPag    := ipVista;
    Ide.modelo    := 65;
    Ide.serie     := 1;
    Ide.nNF       := Trunc(sbProximaNFCe.Value);
    Ide.cNF       := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi      := now;
    Ide.dSaiEnt   := now;
    Ide.hSaiEnt   := now;
    Ide.tpNF      := tnSaida;
    Ide.tpEmis    := TpcnTipoEmissao( IfThen(swOnLine.IsChecked, 0, 8) );
    Ide.tpAmb     := TpcnTipoAmbiente( IfThen(swWebServiceAmbiente.IsChecked, 0, 1) );
    Ide.cUF       := StrToInt(lEmitcUF.Text);
    Ide.cMunFG    := StrToInt(lEmitcMun.Text);
    Ide.finNFe    := fnNormal;
    Ide.tpImp     := tiNFCe;
    Ide.indFinal  := cfConsumidorFinal;
    Ide.indPres   := pcPresencial;

    if not swOnLine.IsChecked then
    begin
      Ide.dhCont := date;
      Ide.xJust  := 'Justificativa Contingencia';
    end;

    Emit.CNPJCPF           := OnlyNumber(edtEmitCNPJ.Text);
    Emit.IE                := OnlyNumber(edtEmitIE.Text);
    Emit.xNome             := edtEmitRazao.Text;
    Emit.xFant             := edtEmitFantasia.Text;

    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.EnderEmit.CEP     := StrToInt(OnlyNumber(edtEmitCEP.Text));
    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(lEmitcMun.Text);
    Emit.EnderEmit.xMun    := cbxEmitCidade.Selected.Text;
    Emit.EnderEmit.UF      := cbxEmitUF.Selected.Text;
    Emit.enderEmit.cPais   := 1058;
    Emit.enderEmit.xPais   := 'BRASIL';

    Emit.IEST := '';
    // esta sendo somando 1 uma vez que o ItemIndex inicia do zero e devemos
    // passar os valores 1, 2 ou 3
    // (1-crtSimplesNacional, 2-crtSimplesExcessoReceita, 3-crtRegimeNormal)
    Emit.CRT  := StrToCRT(Ok, IntToStr(cbxTipoEmpresa.ItemIndex + 1));

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
      Prod.xProd    := 'Descrição do Produto. Teste acentos ÁÉÍÓÚ';
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
      vPag := 100;
    end;

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
  end;

  ACBrNFe1.NotasFiscais.GerarNFe;
  *)
end;

procedure TfrPOSTEFServer.IncluirPedidosSimulados;
begin
  SetLength(fPedidos, 4);
  fPedidos[0].NumPedido := 98;
  fPedidos[0].DataHora := IncMinute(Now, -40);
  fPedidos[0].Nome := 'DANIEL SIMOES';
  fPedidos[0].ValorTotal := 25;
  SetLength(fPedidos[0].Items, 1);
  fPedidos[0].Items[0].CodItem := 100;
  fPedidos[0].Items[0].Descricao := 'PIZZA BROTO PEPPERONI';
  fPedidos[0].Items[0].PrecoUnit := 25;
  fPedidos[0].Items[0].Qtd := 1;
  fPedidos[0].Items[0].UN := 'UN';

  fPedidos[1].NumPedido := 102;
  fPedidos[1].DataHora := IncMinute(Now, -30);
  fPedidos[1].Nome := 'INDIANA JONES';
  fPedidos[1].ValorTotal := 75;
  SetLength(fPedidos[1].Items, 2);
  fPedidos[1].Items[0].CodItem := 100;
  fPedidos[1].Items[0].Descricao := 'PIZZA BROTO PEPPERONI';
  fPedidos[1].Items[0].PrecoUnit := 25;
  fPedidos[1].Items[0].Qtd := 1;
  fPedidos[1].Items[0].UN := 'UN';
  fPedidos[1].Items[1].CodItem := 201;
  fPedidos[1].Items[1].Descricao := 'PIZZA GRANDE MARGUERITA';
  fPedidos[1].Items[1].PrecoUnit := 50;
  fPedidos[1].Items[1].Qtd := 1;
  fPedidos[1].Items[1].UN := 'PC';

  fPedidos[2].NumPedido := 113;
  fPedidos[2].DataHora := IncMinute(Now, -20);
  fPedidos[2].Nome := 'ANA MARIA';
  fPedidos[2].ValorTotal := 115;
  SetLength(fPedidos[2].Items, 3);
  fPedidos[2].Items[0].CodItem := 200;
  fPedidos[2].Items[0].Descricao := 'PIZZA GRANDE PEPPERONI';
  fPedidos[2].Items[0].PrecoUnit := 50;
  fPedidos[2].Items[0].Qtd := 1;
  fPedidos[2].Items[0].UN := 'UN';
  fPedidos[2].Items[1].CodItem := 201;
  fPedidos[2].Items[1].Descricao := 'PIZZA GRANDE MARGUERITA';
  fPedidos[2].Items[1].PrecoUnit := 50;
  fPedidos[2].Items[1].Qtd := 1;
  fPedidos[2].Items[1].UN := 'UN';
  fPedidos[2].Items[2].CodItem := 300;
  fPedidos[2].Items[2].Descricao := 'COCA PET 1,5';
  fPedidos[2].Items[2].PrecoUnit := 15;
  fPedidos[2].Items[2].Qtd := 1;
  fPedidos[2].Items[2].UN := 'LT';

  fPedidos[3].NumPedido := 212;
  fPedidos[3].DataHora := IncMinute(Now, -10);
  fPedidos[3].Nome := 'JUCA PATO';
  fPedidos[3].ValorTotal := 25.50;
  SetLength(fPedidos[3].Items, 2);
  fPedidos[3].Items[0].CodItem := 101;
  fPedidos[3].Items[0].Descricao := 'PIZZA BROTO MARGUERITA';
  fPedidos[3].Items[0].PrecoUnit := 25;
  fPedidos[3].Items[0].Qtd := 1;
  fPedidos[3].Items[0].UN := 'UN';
  fPedidos[3].Items[1].CodItem := 500;
  fPedidos[3].Items[1].Descricao := 'CHICLETE ADAMS';
  fPedidos[3].Items[1].PrecoUnit := 0.50;
  fPedidos[3].Items[1].Qtd := 1;
  fPedidos[3].Items[1].UN := 'UN';
 end;

procedure TfrPOSTEFServer.LerConfiguracao;
Var
  Ini : TIniFile ;
  cUF: LongInt;
begin
  AdicionarLinhaLog('- LerConfiguracao');
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    sePortaTCP.Value := Ini.ReadInteger('POS', 'PortaTCP', sePortaTCP.Value);
    seMaxConexoes.Value := Ini.ReadInteger('POS', 'MaxConexoes.', seMaxConexoes.Value);
    edLog.Text := Ini.ReadString('POS', 'Log', '');
    edMensagemBoasVindas.Text := Ini.ReadString('POS', 'MensagemBoasVindas', edMensagemBoasVindas.Text);
    cbImprimirViaReduzida.Checked := Ini.ReadBool('POS', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    cbSuportaDesconto.Checked := Ini.ReadBool('POS', 'SuportaDesconto', cbSuportaDesconto.Checked);
    cbSuportaSaque.Checked := Ini.ReadBool('POS', 'SuportaSaque', cbSuportaSaque.Checked);
    cbUtilizarSaldoTotalVoucher.Checked := Ini.ReadBool('POS', 'UtilizarSaldoTotalVoucher', cbUtilizarSaldoTotalVoucher.Checked);

    cbTipoAplicacao.ItemIndex := Ini.ReadInteger('Aplicacao', 'TipoDemo', cbTipoAplicacao.ItemIndex);
    edRazaoSocial.Text := Ini.ReadString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    edRegistro.Text := Ini.ReadString('Aplicacao', 'Registro', edRegistro.Text);
    edAplicacaoNome.Text := Ini.ReadString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    edAplicacaoVersao.Text := Ini.ReadString('Aplicacao', 'Versao', edAplicacaoVersao.Text);

    edtEmitCNPJ.Text := Ini.ReadString('Emitente', 'CNPJ', '');
    edtEmitIE.Text := Ini.ReadString('Emitente', 'IE', '');
    edtEmitIM.Text := Ini.ReadString('Emitente', 'IM', '');
    edtEmitRazao.Text := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text := Ini.ReadString('Emitente', 'Fantasia', '');
    edtEmitFone.Text := Ini.ReadString('Emitente', 'Fone', '');
    edtEmitCEP.Text := Ini.ReadString('Emitente', 'CEP', '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro', '');
    edtEmitNumero.Text := Ini.ReadString('Emitente', 'Numero', '');
    edtEmitComp.Text := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text := Ini.ReadString('Emitente', 'Bairro', '');
    cUF := Ini.ReadInteger('Emitente', 'cUF', 0);
    pEmitCodUF.Caption := IntToStr(cUF);
    CarregarListaDeCidades(cUF);
    cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(CUFtoUF(cUF));
    cbxEmitCidade.ItemIndex := FcMunList.IndexOf(IntToStr(Ini.ReadInteger('Emitente', 'cMun', 0)));
    cbxTipoEmpresa.ItemIndex := Ini.ReadInteger('Emitente','CRT', 0);
    cbxRegTribISSQN.ItemIndex  := Ini.ReadInteger('Emitente','RegTribISSQN',0);
    cbxIndRatISSQN.ItemIndex := Ini.ReadInteger('Emitente','IndRatISSQN',0);

    cbSSLLib.ItemIndex := Ini.ReadInteger('Certificado', 'SSLLib', 4);
    cbSSLLibChange(nil);
    cbCryptLib.ItemIndex := Ini.ReadInteger('Certificado', 'CryptLib', cbCryptLib.ItemIndex);
    cbHttpLib.ItemIndex := Ini.ReadInteger('Certificado', 'HttpLib', cbHttpLib.ItemIndex);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    edtCertURLPFX.Text := Ini.ReadString('Certificado', 'URL', '');
    edtCertArqPFX.Text := Ini.ReadString('Certificado', 'Caminho', '');
    edtCertSenha.Text := Ini.ReadString('Certificado', 'Senha', '');
    edtCertNumSerie.Text := Ini.ReadString('Certificado', 'NumSerie', '');

    cbSSLType.ItemIndex := Ini.ReadInteger('WebService', 'SSLType', 5);
    cbWebServiceUF.ItemIndex := cbWebServiceUF.Items.IndexOf(Ini.ReadString('WebService', 'UF', ''));
    rgTipoAmb.ItemIndex := Ini.ReadInteger('WebService', 'Ambiente', 1);
    seTimeOut.Value := Ini.ReadInteger('WebService', 'TimeOut', seTimeOut.Value);
    edtPathSchemas.Text := Ini.ReadString('Geral', 'PathSchemas', ApplicationPath+'Schemas\NFe');

    edtTokenID.Text := Ini.ReadString('NFCe', 'TokenID', '');
    edtTokenCSC.Text := Ini.ReadString('NFCe', 'TokenCSC', '');

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    seProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass', '');
  finally
     Ini.Free ;
  end ;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPOSTEFServer.AplicarConfiguracao;
begin
  AdicionarLinhaLog('- AplicarConfiguracao');
  ConfigurarACBrNFe;
  ConfigurarACBrPOS;
end;

procedure TfrPOSTEFServer.GravarConfiguracao;
Var
  INI : TIniFile ;
begin
  AplicarConfiguracao;

  AdicionarLinhaLog('- GravarConfiguracao');
  INI := TIniFile.Create(NomeArquivoConfiguracao);
  try
    INI.WriteInteger('POS', 'PortaTCP', sePortaTCP.Value);
    INI.WriteInteger('POS', 'MaxConexoes.', seMaxConexoes.Value);
    INI.WriteString('POS', 'Log', edLog.Text);
    INI.WriteString('POS', 'MensagemBoasVindas', edMensagemBoasVindas.Text);
    INI.WriteBool('POS', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    INI.WriteBool('POS', 'SuportaDesconto', cbSuportaDesconto.Checked);
    INI.WriteBool('POS', 'SuportaSaque', cbSuportaSaque.Checked);
    INI.WriteBool('POS', 'UtilizarSaldoTotalVoucher', cbUtilizarSaldoTotalVoucher.Checked);

    INI.WriteInteger('Aplicacao', 'TipoDemo', cbTipoAplicacao.ItemIndex);
    INI.WriteString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    INI.WriteString('Aplicacao', 'Registro', edRegistro.Text);
    INI.WriteString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    INI.WriteString('Aplicacao', 'Versao', edAplicacaoVersao.Text);

    Ini.WriteString('Emitente', 'CNPJ', edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IE', edtEmitIE.Text);
    Ini.WriteString('Emitente', 'IM', edtEmitIM.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia', edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone', edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP', edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro', edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero', edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro', edtEmitBairro.Text);
    Ini.WriteInteger('Emitente', 'cUF', StrToIntDef(pEmitCodUF.Caption,0));
    Ini.WriteInteger('Emitente', 'cMun', StrToIntDef(pEmitCodCidade.Caption,0));
    Ini.WriteInteger('Emitente', 'CRT', cbxTipoEmpresa.ItemIndex);
    INI.WriteInteger('Emitente', 'RegTribISSQN', cbxRegTribISSQN.ItemIndex);
    INI.WriteInteger('Emitente', 'IndRatISSQN', cbxIndRatISSQN.ItemIndex);

    Ini.WriteInteger('Certificado', 'SSLLib', cbSSLLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'CryptLib', cbCryptLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'HttpLib', cbHttpLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString('Certificado', 'URL', edtCertURLPFX.Text);
    Ini.WriteString('Certificado', 'Caminho', edtCertArqPFX.Text);
    Ini.WriteString('Certificado', 'Senha', edtCertSenha.Text);
    Ini.WriteString('Certificado', 'NumSerie', edtCertNumSerie.Text);

    Ini.WriteInteger('WebService', 'SSLType', cbSSLType.ItemIndex);
    Ini.WriteString('WebService', 'UF', cbWebServiceUF.Text);
    Ini.WriteInteger('WebService', 'Ambiente', rgTipoAmb.ItemIndex);
    Ini.WriteInteger('WebService', 'TimeOut', seTimeOut.Value);
    Ini.WriteString('Geral', 'PathSchemas', edtPathSchemas.Text);

    Ini.WriteString('NFCe', 'TokenID', edtTokenID.Text);
    Ini.WriteString('NFCe', 'TokenCSC', edtTokenCSC.Text);

    Ini.WriteString('Proxy', 'Host',  edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', seProxyPorta.Text);
    Ini.WriteString('Proxy', 'User',  edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass',  edtProxySenha.Text);
  finally
     INI.Free ;
  end ;
end;

procedure TfrPOSTEFServer.GravarNumeracaoNFCe(const TerminalId: String; Lote,
  Numero: Integer);
begin

end;

procedure TfrPOSTEFServer.ConfigurarACBrNFe;
var
  CertPFX: string;
begin
  AdicionarLinhaLog('- ConfigurarACBrNFe');
  CertPFX := edtCertArqPFX.Text;
  if (ExtractFilePath(CertPFX) = '') then
    CertPFX := ApplicationPath + CertPFX;

  ACBrNFe1.Configuracoes.Certificados.URLPFX := edtCertURLPFX.Text;
  ACBrNFe1.Configuracoes.Certificados.ArquivoPFX := CertPFX;
  ACBrNFe1.Configuracoes.Certificados.NumeroSerie := edtCertNumSerie.Text;
  ACBrNFe1.Configuracoes.Certificados.Senha := edtCertSenha.Text;
  ACBrNFe1.SSL.URLPFX := ACBrNFe1.Configuracoes.Certificados.URLPFX;
  ACBrNFe1.SSL.ArquivoPFX := ACBrNFe1.Configuracoes.Certificados.ArquivoPFX;
  ACBrNFe1.SSL.NumeroSerie := ACBrNFe1.Configuracoes.Certificados.NumeroSerie;
  ACBrNFe1.SSL.Senha := ACBrNFe1.Configuracoes.Certificados.Senha;

  ACBrNFe1.Configuracoes.WebServices.UF := cbWebServiceUF.Text;

  ACBrNFe1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(rgTipoAmb.ItemIndex);
  ACBrNFe1.Configuracoes.WebServices.TimeOut := seTimeOut.Value;

  ACBrNFe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  ACBrNFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  ACBrNFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  ACBrNFe1.Configuracoes.WebServices.SSLType := TSSLType(cbSSLType.ItemIndex);
  AtualizarSSLLibsCombo;

  ACBrNFe1.Configuracoes.Geral.IdCSC := edtTokenID.Text;
  ACBrNFe1.Configuracoes.Geral.CSC := edtTokenCSC.Text;
  ACBrNFe1.Configuracoes.Geral.ExibirErroSchema := True;
  ACBrNFe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  ACBrNFe1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyPort := seProxyPorta.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;
  ACBrNFe1.Configuracoes.WebServices.Salvar := False;

  ACBrNFe1.Configuracoes.Arquivos.PathSchemas := edtPathSchemas.Text;
  ACBrNFe1.Configuracoes.Arquivos.PathNFe := ApplicationPath + 'xml';
  ACBrNFe1.Configuracoes.Arquivos.PathEvento := ACBrNFe1.Configuracoes.Arquivos.PathNFe;
  ACBrNFe1.Configuracoes.Arquivos.PathInu := ACBrNFe1.Configuracoes.Arquivos.PathNFe;
  ACBrNFe1.Configuracoes.Arquivos.PathSalvar := ACBrNFe1.Configuracoes.Arquivos.PathNFe + PathDelim + 'soap';
end;

procedure TfrPOSTEFServer.ConfigurarACBrPOS;
begin
  AdicionarLinhaLog('- ConfigurarACBrPOS');
  ACBrPOS1.DesInicializar;
  ACBrPOS1.PortaTCP := sePortaTCP.Value;
  ACBrPOS1.MaximoTerminaisConectados := seMaxConexoes.Value;
  ACBrPOS1.ArqLOG := edLog.Text;
  ACBrPOS1.NomeAplicacao := edAplicacaoNome.Text;
  ACBrPOS1.VersaoAplicacao := edAplicacaoVersao.Text;
  ACBrPOS1.SoftwareHouse := edRazaoSocial.Text;
  ACBrPOS1.MensagemBoasVindas := edMensagemBoasVindas.Text;
  ACBrPOS1.ImprimirViaClienteReduzida := cbImprimirViaReduzida.Checked;
  ACBrPOS1.SuportaDesconto := cbSuportaDesconto.Checked;
  ACBrPOS1.SuportaSaque := cbSuportaSaque.Checked;
  ACBrPOS1.UtilizaSaldoTotalVoucher := cbUtilizarSaldoTotalVoucher.Checked;
end;

procedure TfrPOSTEFServer.IrParaMenuPrincipal;
begin
  AdicionarLinhaLog('- IrParaMenuPrincipal');
  pgPrincipal.ActivePage := tsMenuPrincipal;
end;

procedure TfrPOSTEFServer.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLLib);
  imgErrSSLLib.Visible := (cbSSLLib.ItemIndex < 1);

  cbCryptLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLCryptLib);
  imgErrCryptLib.Visible := (cbCryptLib.ItemIndex < 1);

  cbHttpLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLHttpLib);
  imgErrHttpLib.Visible := (cbHttpLib.ItemIndex < 1);

  cbXmlSignLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib);
  imgErrXmlSignLib.Visible := (cbXmlSignLib.ItemIndex < 1);

  cbSSLType.Enabled := (ACBrNFe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.ValidarConfigCertificado;
var
 PathPfx, UrlPfx, ArqPfx, NumSerie, Senha: String;
 Ok: Boolean;
begin
  UrlPfx := edtCertURLPFX.Text;
  ArqPfx := edtCertArqPFX.Text;
  NumSerie := edtCertNumSerie.Text;
  Senha := edtCertSenha.Text;
  Ok := (cbCryptLib.ItemIndex > 0);

  if (NumSerie = '') then
  begin
    Ok := Ok and (Senha <> '');
    if Ok then
    begin
      if (UrlPfx <> '') then
        Ok := (ArqPfx <> '')   // Precisa do PFX, para Cache Local
      else
      begin
        if (ExtractFilePath(ArqPfx) = '') then
          PathPfx := ApplicationPath + ArqPfx
        else
          PathPfx := ArqPfx;

        Ok := (ArqPfx <> '') and FileExists(PathPfx);
      end;
    end;
  end;

  imgErrCertificado.Visible := not Ok;
  btCertInfo.Enabled := not imgErrCertificado.Visible;
  ValidarConfigWebService;
end;

procedure TfrPOSTEFServer.ValidarConfigWebService;
var
  Ok: Boolean;
  PathSchemas: String;
begin
  imgErrWebService.Visible := (cbWebServiceUF.ItemIndex < 0);

  PathSchemas := edtPathSchemas.Text;
  Ok := (PathSchemas <> '');
  if Ok then
    Ok := FileExists(PathWithDelim(PathSchemas) + 'nfe_v4.00.xsd');

  imgErrPathSchemas.Visible := not Ok;
  btStatusServico.Enabled := not ( imgErrCertificado.Visible or
                                   imgErrPathSchemas.Visible or
                                   imgErrWebService.Visible);
end;

procedure TfrPOSTEFServer.LigarAlertasdeErrosDeConfiguracao;
begin
  edtEmitCNPJChange(Nil);
  edtEmitRazaoChange(Nil);
  edtEmitFoneChange(Nil);
  edtEmitCEPChange(Nil);
  cbxEmitCidadeChange(Nil);

  AtualizarSSLLibsCombo;
  edtCertArqPFXChange(Nil);
  edtCertNumSerieChange(Nil);

  ValidarConfigWebService;
  edtTokenCSCChange(Nil);
  edtTokenIDChange(Nil);
end;

end.

