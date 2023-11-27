{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{																		                                      	   }
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

unit uDemoBoleto;

{$MODE Delphi}

interface

//descomentar o motor de relatório que desejar utilizar! removendo o ponto
{$DEFINE GERADOR_FORTES_REPORT}
{.$DEFINE GERADOR_FAST_REPORT}
{.$DEFINE GERADOR_FPDF}

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Masks, IniFiles,
  ACBrBase, ACBrBoleto, ACBrUtil, ACBrMail, ACBrUtil.FilesIO,
  ACBrBoletoConversao, ACBrBoletoRetorno, ComCtrls, MaskEdit
  {$IFDEF GERADOR_FORTES_REPORT},ACBrBoletoFCFortesFr{$ENDIF}
  {$IFDEF GERADOR_FAST_REPORT},ACBrBoletoFCFR{$ENDIF}
  {$IFDEF GERADOR_FPDF},ACBrBoletoFPDF{$ENDIF}
  ;
type
  TfrmDemoBoleto = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    grpFichaBancaria: TGroupBox;
    Button3: TButton;
    btnBoletoIndividual: TButton;
    Button5: TButton;
    grpCNAB: TGroupBox;
    grpWebServicesApi: TGroupBox;
    btnGerarRemessa: TButton;
    btnLerRetorno: TButton;
    btnWSConsulta: TButton;
    btnWSRegistrar: TButton;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edtMoraJuros: TEdit;
    edtValorDesconto: TEdit;
    edtValorAbatimento: TEdit;
    edtMulta: TEdit;
    edtDataMora: TMaskEdit;
    edtDataDesconto: TMaskEdit;
    edtDataAbatimento: TMaskEdit;
    edtDataProtesto: TMaskEdit;
    GroupBox4: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    edtNumeroDoc: TEdit;
    edtValorDoc: TEdit;
    edtDataDoc: TMaskEdit;
    edtVencimento: TMaskEdit;
    GroupBox3: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    memMensagem: TMemo;
    edtInstrucoes1: TEdit;
    edtInstrucoes2: TEdit;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtLocalPag: TEdit;
    edtEspecieDoc: TEdit;
    edtEspecieMod: TEdit;
    cbxAceite: TComboBox;
    edtCarteira: TEdit;
    Label6: TLabel;
    edtNossoNro: TEdit;
    cbxBanco: TComboBox;
    Label32: TLabel;
    GroupBox10: TGroupBox;
    cbxCNAB: TComboBox;
    edtCNABLVLote: TEdit;
    edtCNABLVArquivo: TEdit;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    GroupBox11: TGroupBox;
    edtCIP: TEdit;
    Label36: TLabel;
    GroupBox12: TGroupBox;
    Label38: TLabel;
    edtAgencia: TEdit;
    edtAgenciaDV: TEdit;
    Label39: TLabel;
    edtConta: TEdit;
    Label40: TLabel;
    edtContaDV: TEdit;
    Label41: TLabel;
    edtAgenciaContaDV: TEdit;
    Label42: TLabel;
    edtConvenio: TEdit;
    Label43: TLabel;
    edtModalidade: TEdit;
    Label44: TLabel;
    edtOperacao: TEdit;
    Label47: TLabel;
    Label45: TLabel;
    edtCodigoTransmissao: TEdit;
    Label46: TLabel;
    edtDensidadeGravacao: TEdit;
    Label37: TLabel;
    edtPrefixRemessa: TEdit;
    GroupBox13: TGroupBox;
    btnConfigLer: TButton;
    btnConfigGravar: TButton;
    edtCodigoCedente: TEdit;
    Label48: TLabel;
    Label49: TLabel;
    cbxTipoDistribuicao: TComboBox;
    Label51: TLabel;
    cbxResponsavelEmissao: TComboBox;
    Label52: TLabel;
    cbxTipoCarteira: TComboBox;
    GroupBox14: TGroupBox;
    PageControl2: TPageControl;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    edtCidade: TEdit;
    Label28: TLabel;
    edtEndereco: TEdit;
    Label24: TLabel;
    edtNome: TEdit;
    Label21: TLabel;
    edtCPFCNPJ: TEdit;
    Label22: TLabel;
    edtNumero: TEdit;
    Label25: TLabel;
    edtCEP: TEdit;
    Label29: TLabel;
    edtUF: TEdit;
    Label30: TLabel;
    edtComplemento: TEdit;
    Label26: TLabel;
    edtEmail: TEdit;
    Label23: TLabel;
    edtBairro: TEdit;
    Label27: TLabel;
    Label53: TLabel;
    edtBenifRazao: TEdit;
    Label54: TLabel;
    edtBenifCNPJ: TEdit;
    Label55: TLabel;
    edtBenifEndereco: TEdit;
    Label56: TLabel;
    edtBenifNum: TEdit;
    Label57: TLabel;
    edtBenifComplemento: TEdit;
    Label58: TLabel;
    edtBenifBairro: TEdit;
    Label59: TLabel;
    edtBenifCidade: TEdit;
    Label60: TLabel;
    edtBenifCEP: TEdit;
    Label61: TLabel;
    edtBenifUF: TEdit;
    Label62: TLabel;
    edtBenifFantasia: TEdit;
    edtBenifTelefone: TEdit;
    Label63: TLabel;
    cbxTipoDocumento: TComboBox;
    Label64: TLabel;
    GroupBox6: TGroupBox;
    btnImpressaoHTML: TButton;
    btnImpressaoPDF: TButton;
    btnImpressaoSpooler: TButton;
    btnImpressaoStream: TButton;
    edtPathRemessa: TEdit;
    Label65: TLabel;
    edtPathRetorno: TEdit;
    Label66: TLabel;
    dlgFile: TOpenDialog;
    btnRetorno: TButton;
    edtClientID: TEdit;
    Label67: TLabel;
    edtClientSecret: TEdit;
    Label68: TLabel;
    edtKeyUser: TEdit;
    Label69: TLabel;
    cbxCaracteristicaTitulo: TComboBox;
    Label50: TLabel;
    cbxSSLLib: TComboBox;
    Label70: TLabel;
    btnImpressaoPDFIndividual: TButton;
    Label71: TLabel;
    edtScope: TEdit;
    TabSheet3: TTabSheet;
    Label72: TLabel;
    edtFrom: TEdit;
    Label73: TLabel;
    edtFromName: TEdit;
    Label74: TLabel;
    edtHost: TEdit;
    Label75: TLabel;
    edtPort: TEdit;
    Label76: TLabel;
    chkTLS: TCheckBox;
    chkSSL: TCheckBox;
    Label77: TLabel;
    edtUserName: TEdit;
    Label78: TLabel;
    chkMostrarSenha: TCheckBox;
    edtPassword: TEdit;
    btnEnviarEmail: TButton;
    Label79: TLabel;
    PageControl3: TPageControl;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    edtPathFR3: TEdit;
    Label80: TLabel;
    cbxMotorRelatorio: TComboBox;
    Label31: TLabel;
    cbxLayOut: TComboBox;
    cbxImprimirVersoFatura: TCheckBox;
    Label81: TLabel;
    edtPathLogoMarca: TEdit;
    Label82: TLabel;
    Label83: TLabel;
    Label84: TLabel;
    edtSenhaPDF: TEdit;
    btnImprimirTeste: TButton;
    Label86: TLabel;
    PageControlConfg: TPageControl;
    TabSheet8: TTabSheet;
    ckbImprimirMensagemPadrao: TCheckBox;
    ckbLerCedenteArquivoRetorno: TCheckBox;
    ckbLerNossoNumeroCompleto: TCheckBox;
    ckbRemoverAcentuacaoRemessa: TCheckBox;
    TabSheet9: TTabSheet;
    cxbEMV: TCheckBox;
    chkIndicadorPix: TCheckBox;
    ckbEmHomologacao: TCheckBox;
    TabSheet10: TTabSheet;
    Label85: TLabel;
    cbbWSConsulta: TComboBox;
    chkLogComponente: TCheckBox;
    edtPathLog: TEdit;
    Button1: TButton;
    dlgSave: TSaveDialog;
    procedure btnImpressaoHTMLClick(Sender: TObject);
    procedure btnImpressaoPDFClick(Sender: TObject);
    procedure btnBoletoIndividualClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btnGerarRemessaClick(Sender: TObject);
    procedure btnImpressaoSpoolerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cbxLayOutChange(Sender: TObject);
    procedure btnLerRetornoClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure btnWSRegistrarClick(Sender: TObject);
    procedure btnConfigLerClick(Sender: TObject);
    procedure btnConfigGravarClick(Sender: TObject);
    procedure btnImpressaoPDFIndividualClick(Sender: TObject);
    procedure btnImpressaoStreamClick(Sender: TObject);
    procedure btnImprimirTesteClick(Sender: TObject);
    procedure btnRetornoClick(Sender: TObject);
    procedure btnWSConsultaClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbxMotorRelatorioChange(Sender: TObject);
    procedure chkMostrarSenhaClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label79Click(Sender: TObject);
  private
    FACBrBoleto : TACBrBoleto;
    FACBrMail   : TACBrMail;
    {$IFDEF GERADOR_FORTES_REPORT}
      FACBrBoletoFCRL   : TACBrBoletoFCFortes;
    {$ENDIF}

    {$IFDEF GERADOR_FAST_REPORT}
      FACBrBoletoFCFR   : TACBrBoletoFCFR;
    {$ENDIF}

    {$IFDEF GERADOR_FPDF}
      FACBrBoletoFPDF   : TACBrBoletoFPDF;
    {$ENDIF}
    { Private declarations }
    procedure CarregarBancos;
    procedure CarregarTipoDistribuicao;
    procedure CarregarCaracteristicaTitulo;
    procedure CarregarResponsavelEmissao;
    procedure CarregarTipoCarteira;
    procedure CarregarTipoDocumento;
    procedure CarregarSSLLib;
    procedure GravarIniComponente;
    procedure LerIniComponente(const ADialog : Boolean = False);
    procedure AplicarConfiguracoesAoComponente;
    procedure AplicarConfiguracoesComponenteATela;
    procedure AplicarConfiguracoesEmailNaTela(IniConfig: TMemIniFile);
    procedure AplicarConfiguracoesComponenteEmail;
  public
    { Public declarations }
  end;

var
  frmDemoBoleto: TfrmDemoBoleto;

  CONST MOTOR_NAO_SELECIONADO = 'MOTOR DE RELATÓRIO NÃO FOI SELECIONADO, VERIFIQUE!!!';
  CONST FILTER_RETORNO        = '*.txt|*.txt|*.ret|*.ret|*.*|*.*';
  CONST FILTER_INI            = '*.ini|*.ini|*.*|*.*';

implementation

Uses TypInfo, DateUtils, pcnConversao, ACBrDFeSSL;

{$R *.lfm}

procedure TfrmDemoBoleto.GravarIniComponente;
var
  xPath, xArquivo : String;
  IniFile: TMemIniFile;
begin
  xPath    := ExtractFilePath(ParamStr(0));
  xArquivo := ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
  FACBrBoleto.GravarConfiguracao(xPath,xArquivo);

  IniFile := TMemIniFile.Create(xPath+xArquivo);
  try
    IniFile.WriteString('EMAIL', 'FromEmail', edtFrom.Text);
    IniFile.WriteString('EMAIL', 'FromName' , edtFromName.Text);
    IniFile.WriteString('EMAIL', 'Host', edtHost.Text);
    IniFile.WriteString('EMAIL', 'Port', edtPort.Text);
    IniFile.WriteBool('EMAIL', 'SSL', chkSSL.Checked);
    IniFile.WriteBool('EMAIL', 'TLS', chkTLS.Checked);
    IniFile.WriteString('EMAIL', 'UserName', edtUserName.Text);
    IniFile.WriteString('EMAIL', 'PassWord', edtPassword.Text);
    IniFile.WriteBool('EMAIL', 'MostrarSenha', chkMostrarSenha.Checked);

    IniFile.WriteString('PATH', 'BOLETOFR3', edtPathFR3.Text);
    IniFile.WriteString('PATH', 'LOGOMARCA', edtPathLogoMarca.Text);
    IniFile.WriteBool('PATH', 'MostrarSenha', chkMostrarSenha.Checked);
    IniFile.WriteString('BANCO', 'CARTEIRA', edtCarteira.Text);

    IniFile.UpdateFile;

  finally
    IniFile.FRee;
  end;
end;

procedure TfrmDemoBoleto.Label79Click(Sender: TObject);
begin
  OpenURL('https://www.projetoacbr.com.br/forum/topic/56101-configura%C3%A7%C3%B5es-do-acbrmail-para-os-principais-servi%C3%A7os-de-emails-do-mercado/');
end;

procedure TfrmDemoBoleto.LerIniComponente(const ADialog : Boolean);
var xArquivo : String;
  IniFile: TMemIniFile;
begin
  if ADialog then
  begin
    dlgFile.Filter := FILTER_INI;
    if dlgFile.Execute then
      xArquivo := dlgFile.FileName
    else
      raise Exception.Create('É NECESSÁRIO SELECIONAR O ARQUIVO DE CONFIGURAÇÕES');
  end else
    xArquivo := ExtractFilePath(ParamStr(0)) + ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');

  if (FileExists(xArquivo)) then
    FACBrBoleto.LerConfiguracao(xArquivo);

  IniFile := TMemIniFile.Create(xArquivo);
  try
    edtPathFR3.Text := IniFile.ReadString('PATH', 'BOLETOFR3', ExtractFilePath(ParamStr(0))+'Report\Boleto.fr3');
    edtPathLogoMarca.Text := IniFile.ReadString('PATH', 'LOGOMARCA', '..\..\..\Fontes\ACBrBoleto\Logos\Colorido\png\');
    edtCarteira.Text := IniFile.ReadString('BANCO', 'CARTEIRA', '00');
    AplicarConfiguracoesEmailNaTela(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TfrmDemoBoleto.AplicarConfiguracoesAoComponente;
var Beneficiario   : TACBrCedente;
    Banco          : TACBrBanco;
    Boleto         : TACBrBoleto;
    WebService     : TACBrWebService;
    BeneficiarioWS : TACBrCedenteWS;
    CobAnterior    : TACBrTipoCobranca;
begin
  Boleto := FACBrBoleto;
  WebService := Boleto.Configuracoes.WebService;

  CobAnterior := Boleto.Banco.TipoCobranca;
  if CobAnterior <> TACBrTipoCobranca(cbxBanco.Items.Objects[cbxBanco.ItemIndex]) then
    edtLocalPag.Text := '';

  //Boleto.ListadeBoletos.Clear;

  Boleto.PrefixArqRemessa                  := edtPrefixRemessa.Text;
  Boleto.LayoutRemessa                     := TACBrLayoutRemessa(cbxCNAB.itemindex);
  Boleto.Homologacao                       := ckbEmHomologacao.Checked;

  Boleto.ImprimirMensagemPadrao            := ckbImprimirMensagemPadrao.Checked;
  Boleto.LeCedenteRetorno                  := ckbLerCedenteArquivoRetorno.Checked;
  Boleto.LerNossoNumeroCompleto            := ckbLerNossoNumeroCompleto.Checked;
  Boleto.RemoveAcentosArqRemessa           := ckbRemoverAcentuacaoRemessa.Checked;


  Beneficiario   := Boleto.Cedente;
  BeneficiarioWS := Beneficiario.CedenteWS;

  Beneficiario.Agencia                       := edtAgencia.Text;
  Beneficiario.AgenciaDigito                 := edtAgenciaDV.Text;
  Beneficiario.Conta                         := edtConta.Text;
  Beneficiario.ContaDigito                   := edtContaDV.Text;
  Beneficiario.DigitoVerificadorAgenciaConta := edtAgenciaContaDV.Text;
  Beneficiario.Convenio                      := edtConvenio.Text;
  Beneficiario.Modalidade                    := edtModalidade.Text;
  Beneficiario.Operacao                      := edtOperacao.Text;
  Beneficiario.CodigoTransmissao             := edtCodigoTransmissao.Text;
  Beneficiario.CodigoCedente                 := edtCodigoCedente.Text;

  if Length(OnlyNumber(edtBenifCNPJ.Text)) = 14 then
    Beneficiario.TipoInscricao               := pJuridica
  else
    Beneficiario.TipoInscricao               := pFisica;

  Beneficiario.TipoDocumento                 := TACBrTipoDocumento(cbxTipoDocumento.ItemIndex);

  Beneficiario.IdentDistribuicao             := TACBrIdentDistribuicao(cbxTipoDistribuicao.itemIndex);
  Beneficiario.ResponEmissao                 := TACBrResponEmissao(cbxResponsavelEmissao.ItemIndex);
  Beneficiario.CaracTitulo                   := TACBrCaracTitulo(cbxCaracteristicaTitulo.itemIndex);
  Beneficiario.TipoCarteira                  := TACBrTipoCarteira(cbxTipoCarteira.itemIndex);

  Beneficiario.CNPJCPF                       := edtBenifCNPJ.Text;
  Beneficiario.Nome                          := edtBenifRazao.Text;
  Beneficiario.FantasiaCedente               := edtBenifFantasia.Text;
  Beneficiario.Logradouro                    := edtBenifEndereco.Text;
  Beneficiario.NumeroRes                     := edtBenifNum.Text;
  Beneficiario.Complemento                   := edtBenifComplemento.Text;
  Beneficiario.Bairro                        := edtBenifBairro.Text;
  Beneficiario.Cidade                        := edtBenifCidade.Text;
  Beneficiario.UF                            := edtBenifUF.Text;
  Beneficiario.CEP                           := edtBenifCEP.Text;
  Beneficiario.Telefone                      := edtBenifTelefone.Text;


  Banco := Boleto.Banco;
  Banco.TipoCobranca        := TACBrTipoCobranca(cbxBanco.Items.Objects[cbxBanco.ItemIndex]);
  Banco.LayoutVersaoArquivo := StrToIntDef(edtCNABLVArquivo.Text,0);
  Banco.LayoutVersaoLote    := StrToIntDef(edtCNABLVLote.Text,0);
  Banco.CIP                 := edtCIP.Text;
  Banco.DensidadeGravacao   := edtDensidadeGravacao.Text;

  if (Banco.LocalPagamento <> edtLocalPag.Text) and (edtLocalPag.Text <> '') then
    Banco.LocalPagamento      := edtLocalPag.Text;

  if edtLocalPag.Text = '' then
    edtLocalPag.Text := Banco.LocalPagamento;

  BeneficiarioWS.ClientID     := edtClientID.Text;
  BeneficiarioWS.ClientSecret := edtClientSecret.Text;
  BeneficiarioWS.KeyUser      := edtKeyUser.Text;
  BeneficiarioWS.Scope        := edtScope.Text;
  BeneficiarioWS.IndicadorPix := chkIndicadorPix.Checked;
  WebService.Ambiente         := TpcnTipoAmbiente(Ord(ckbEmHomologacao.Checked));
  WebService.SSLHttpLib       := TSSLHttpLib(cbxSSLLib.ItemIndex);

  Boleto.Configuracoes.Arquivos.LogRegistro        := chkLogComponente.Checked;
  Boleto.Configuracoes.Arquivos.PathGravarRegistro := edtPathLog.Text;

  AplicarConfiguracoesComponenteEmail;

  if Assigned(FACBrBoleto.ACBrBoletoFC) then
    FACBrBoleto.ACBrBoletoFC.DirLogo := edtPathLogoMarca.Text;

  {$IFDEF GERADOR_FAST_REPORT}
    FACBrBoletoFCFR.FastReportFile := edtPathFR3.Text;
    FACBrBoletoFCFR.MostrarPreview := True;
    FACBrBoletoFCFR.MostrarSetup   := True;
  {$ENDIF}
end;

procedure TfrmDemoBoleto.AplicarConfiguracoesComponenteATela;
var Beneficiario : TACBrCedente;
    BeneficiarioWS : TACBrCedenteWS;
    Banco : TACBrBanco;
    Boleto : TACBrBoleto;
    I : Integer;
begin
  Boleto := FACBrBoleto;
  Boleto.ListadeBoletos.Clear;

  edtPrefixRemessa.Text               := Boleto.PrefixArqRemessa;

  cbxCNAB.itemindex                   := Ord(Boleto.LayoutRemessa);
  ckbEmHomologacao.Checked            := Boleto.Homologacao;
  ckbImprimirMensagemPadrao.Checked   := Boleto.ImprimirMensagemPadrao;
  ckbLerCedenteArquivoRetorno.Checked := Boleto.LeCedenteRetorno;
  ckbLerNossoNumeroCompleto.Checked   := Boleto.LerNossoNumeroCompleto;
  ckbRemoverAcentuacaoRemessa.Checked := Boleto.RemoveAcentosArqRemessa;

  Beneficiario := Boleto.Cedente;

  edtAgencia.Text           := Beneficiario.Agencia;
  edtAgenciaDV.Text         := Beneficiario.AgenciaDigito;
  edtConta.Text             := Beneficiario.Conta;
  edtContaDV.Text           := Beneficiario.ContaDigito;
  edtAgenciaContaDV.Text    := Beneficiario.DigitoVerificadorAgenciaConta;
  edtConvenio.Text          := Beneficiario.Convenio;
  edtModalidade.Text        := Beneficiario.Modalidade;
  edtOperacao.Text          := Beneficiario.Operacao;
  edtCodigoTransmissao.Text := Beneficiario.CodigoTransmissao;
  edtCodigoCedente.Text     := Beneficiario.CodigoCedente;

  cbxTipoDistribuicao.ItemIndex     := Ord(Beneficiario.IdentDistribuicao);
  cbxCaracteristicaTitulo.ItemIndex := Ord(Beneficiario.CaracTitulo);
  cbxResponsavelEmissao.ItemIndex   := Ord(Beneficiario.ResponEmissao);
  cbxTipoCarteira.ItemIndex         := Ord(Beneficiario.TipoCarteira);
  cbxTipoDocumento.ItemIndex        := Integer(TACBrTipoDocumento(Beneficiario.TipoDocumento)) -1;

  cbxSSLLib.ItemIndex               := Ord(Boleto.Configuracoes.WebService.SSLHttpLib);

  Banco := Boleto.Banco;

  for I := 0 to cbxBanco.Items.Count - 1 do
    if Integer(cbxBanco.Items.Objects[i]) = Ord(Banco.TipoCobranca) then
      cbxBanco.ItemIndex        := I;

  edtCNABLVArquivo.Text     := IntToStr(Banco.LayoutVersaoArquivo);
  edtCNABLVLote.Text        := IntToStr(Banco.LayoutVersaoLote);
  edtCIP.Text               := Banco.CIP;
  edtDensidadeGravacao.Text := Banco.DensidadeGravacao;
  edtLocalPag.Text          := Banco.LocalPagamento;

  BeneficiarioWS := Beneficiario.CedenteWS;
  edtClientID.Text          := BeneficiarioWS.ClientID;
  edtClientSecret.Text      := BeneficiarioWS.ClientSecret;
  edtKeyUser.Text           := BeneficiarioWS.KeyUser;
  edtScope.Text             := BeneficiarioWS.Scope;

  chkLogComponente.Checked :=  Boleto.Configuracoes.Arquivos.LogRegistro;
  edtPathLog.Text           := Boleto.Configuracoes.Arquivos.PathGravarRegistro;
end;

procedure TfrmDemoBoleto.AplicarConfiguracoesComponenteEmail;
var
  Mail: TACBrMail;
begin
  Mail := FACBrMail;

  Mail.From := edtFrom.Text;
  Mail.FromName := edtFromName.Text;
  Mail.Host := edtHost.Text;
  Mail.Port := edtPort.Text;
  Mail.SetTLS := chkTLS.Checked;
  Mail.SetSSL := chkSSL.Checked;
  Mail.Username := edtUserName.Text;
  Mail.Password := edtPassword.Text;
end;

procedure TfrmDemoBoleto.AplicarConfiguracoesEmailNaTela(IniConfig: TMemIniFile);
begin
  edtFrom.Text := IniConfig.ReadString('EMAIL', 'FromEmail', '');
  edtFromName.Text := IniConfig.ReadString('EMAIL', 'FromName', '');
  edtHost.Text := IniConfig.ReadString('EMAIL', 'Host', '');
  edtPort.Text := IniConfig.ReadString('EMAIL', 'Port', '');
  chkTLS.Checked := IniConfig.ReadBool('EMAIL', 'TLS', True);
  chkSSL.Checked := IniConfig.ReadBool('EMAIL', 'SSL', True);
  edtUserName.Text := IniConfig.ReadString('EMAIL', 'UserName', '');
  edtPassword.Text := IniConfig.ReadString('EMAIL', 'PassWord', '');
  chkMostrarSenha.Checked := IniConfig.ReadBool('EMAIL', 'MostrarSenha', False);
  chkMostrarSenhaClick(chkMostrarSenha);

  AplicarConfiguracoesComponenteEmail;

end;

procedure TfrmDemoBoleto.btnLerRetornoClick(Sender: TObject);
var Boleto  : TACBrBoleto;
    Retorno : TListadeBoletos;
    I       : Integer;
    RetText : TStringList;
begin
  Boleto := FACBrBoleto;

  Boleto.DirArqRetorno  := ExtractFilePath(edtPathRetorno.Text);
  Boleto.NomeArqRetorno := ExtractFileName(edtPathRetorno.Text);

  Boleto.ListadeBoletos.Clear;
  Boleto.LerRetorno();

  Retorno := Boleto.ListadeBoletos;
  RetText := TStringList.Create;
  try
    for I := 0 to Pred(Retorno.Count) do
    begin
      RetText.Add( 'Nosso Número :: '    + Retorno[i].NossoNumero);
      RetText.Add( 'Valor Documento :: ' + CurrToStr(Retorno[i].ValorDocumento));
      RetText.Add( 'Valor Pago :: '      + CurrToStr(Retorno[i].ValorPago));
      RetText.Add( 'Data Ocorrencia :: ' + DateToStr(Retorno[i].DataOcorrencia));
      //[...] demais propriedades do titulo a gosto
    end;
    RetText.SaveToFile( PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoProcessado.txt' );
    ShowMessage('Retorno processado em: '+ PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoProcessado.txt' );
  finally
    RetText.Free;
  end;
end;

procedure TfrmDemoBoleto.btnImpressaoHTMLClick(Sender: TObject);
begin
  if not Assigned(FACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create(MOTOR_NAO_SELECIONADO);

  FACBrBoleto.ACBrBoletoFC.NomeArquivo := ExtractFilePath(Application.ExeName) + 'teste.html';
  FACBrBoleto.GerarHTML;
end;

procedure TfrmDemoBoleto.btnImpressaoPDFClick(Sender: TObject);
begin
  if not Assigned(FACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create(MOTOR_NAO_SELECIONADO);

  FACBrBoleto.ACBrBoletoFC.CalcularNomeArquivoPDFIndividual := False;
  FACBrBoleto.ACBrBoletoFC.PdfSenha := edtSenhaPDF.Text;
  FACBrBoleto.GerarPDF;
end;

procedure TfrmDemoBoleto.btnBoletoIndividualClick(Sender: TObject);
var
  Titulo : TACBrTitulo;
  VQtdeCarcA, VQtdeCarcB, VQtdeCarcC :Integer;
  VLinha, logo : string;
  i: Integer;
begin
  Titulo := FACBrBoleto.CriarTituloNaLista;

  Titulo.Vencimento        := StrToDate(edtVencimento.Text);
  Titulo.DataDocumento     := StrToDate(edtDataDoc.Text);
  Titulo.NumeroDocumento   := edtNumeroDoc.Text;
  Titulo.EspecieDoc        := edtEspecieDoc.Text;
  Titulo.EspecieMod        := edtEspecieMod.Text;

  if cbxAceite.ItemIndex = 0 then
     Titulo.Aceite := atSim
  else
     Titulo.Aceite := atNao;

  Titulo.DataProcessamento := Now;
  Titulo.Carteira          := edtCarteira.Text;
  Titulo.NossoNumero       := edtNossoNro.Text;
  Titulo.ValorDocumento    := StrToCurr(edtValorDoc.Text);
  Titulo.Sacado.NomeSacado := edtNome.Text;
  Titulo.Sacado.CNPJCPF    := OnlyNumber(edtCPFCNPJ.Text);
  Titulo.Sacado.Logradouro := edtEndereco.Text;
  Titulo.Sacado.Numero     := edtNumero.Text;
  Titulo.Sacado.Bairro     := edtBairro.Text;
  Titulo.Sacado.Cidade     := edtCidade.Text;
  Titulo.Sacado.UF         := edtUF.Text;
  Titulo.Sacado.CEP        := OnlyNumber(edtCEP.Text);
  Titulo.ValorAbatimento   := StrToCurrDef(edtValorAbatimento.Text,0);
  Titulo.LocalPagamento    := edtLocalPag.Text;
  Titulo.ValorMoraJuros    := StrToCurrDef(edtMoraJuros.Text,0);
  Titulo.ValorDesconto     := StrToCurrDef(edtValorDesconto.Text,0);
  Titulo.ValorAbatimento   := StrToCurrDef(edtValorAbatimento.Text,0);
  Titulo.DataMoraJuros     := StrToDateDef(edtDataMora.Text, 0);
  Titulo.DataDesconto      := StrToDateDef(edtDataDesconto.Text, 0);
  Titulo.TipoDesconto      := tdNaoConcederDesconto;
  Titulo.DataAbatimento    := StrToDateDef(edtDataAbatimento.Text, 0);
  Titulo.DataProtesto      := StrToDateDef(edtDataProtesto.Text, 0);
  Titulo.PercentualMulta   := StrToCurrDef(edtMulta.Text,0);
  Titulo.CodigoMoraJuros   := cjValorMensal;
  Titulo.Mensagem.Text     := memMensagem.Text;
  Titulo.OcorrenciaOriginal.Tipo := toRemessaRegistrar;
  Titulo.Instrucao1        := trim(edtInstrucoes1.Text);
  Titulo.Instrucao2        := trim(edtInstrucoes2.Text);

  Titulo.QtdePagamentoParcial   := 1;
  Titulo.TipoPagamento          := tpNao_Aceita_Valor_Divergente;
  Titulo.PercentualMinPagamento := 0;
  Titulo.PercentualMaxPagamento := 0;
  Titulo.ValorMinPagamento      := 0;
  Titulo.ValorMaxPagamento      := 0;
  if cxbEMV.Checked then
    Titulo.QrCode.emv := '00020101021226870014br.gov.bcb.pix2565qrcodepix-h.bb.com.br/pix/v2/22657e83-ecac-4631-a767-65e16fc56bff5204000053039865802BR5925EMPRORT AMBIENTAL        6008BRASILIA62070503***6304BD3D';

 // FACBrBoleto.AdicionarMensagensPadroes(Titulo,Mensagem);

  if cbxLayOut.ItemIndex = 6 then
  begin
    for i:=0 to 3 do
    begin
      VLinha := '.';

      VQtdeCarcA := length('Descrição Produto/Serviço ' + IntToStr(I));
      VQtdeCarcB := Length('Valor:');
      VQtdeCarcC := 85 - (VQtdeCarcA + VQtdeCarcB);

      VLinha := PadLeft(VLinha,VQtdeCarcC,'.');

      Titulo.Detalhamento.Add('Descrição Produto/Serviço ' + IntToStr(I) + ' '+ VLinha + ' Valor:   '+  PadRight(FormatCurr('R$ ###,##0.00', StrToCurr(edtValorDoc.Text) * 0.25),18,' ') );
    end;
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add('');
    Titulo.Detalhamento.Add('Desconto ........................................................................... Valor: R$ 0,00' );
  end;

  //if FileExists(ExtractFileDir(ParamStr(0)) + '\acbr_logo.jpg') then
    // logo := ExtractFileDir(ParamStr(0)) + '\acbr_logo.jpg';

  //Titulo.ArquivoLogoEmp := logo;  // logo da empresa
  Titulo.Verso := ((cbxImprimirVersoFatura.Checked) and ( cbxImprimirVersoFatura.Enabled = true ));

end;

procedure TfrmDemoBoleto.Button5Click(Sender: TObject);
var
  Titulo: TACBrTitulo;
  I: Integer;
  NrTitulos: String;
  Valor : Currency;
begin

  NrTitulos := '10';
  InputQuery('Geração Lote','Quantidade a Gerar :',NrTitulos);

  for I := 0 to Pred(StrToIntDef(NrTitulos,0)) do
  begin
    Valor := StrToFloatDef(edtValorDoc.Text,1);
    Valor := Valor + Random;
    edtValorDoc.Text  := CurrToStr(Valor);
    edtNossoNro.Text  := IntToStr(StrToIntDef(edtNossoNro.Text,0)+1);
    edtNumeroDoc.Text := IntToStr(StrToIntDef(edtNumeroDoc.Text,0)+1);
    btnBoletoIndividual.Click;
  end;
end;

procedure TfrmDemoBoleto.btnGerarRemessaClick(Sender: TObject);
var NumRemessa : string;
begin
  FACBrBoleto.DirArqRemessa := edtPathRemessa.Text;
  NumRemessa := '1';
  InputQuery('Num. Remessa','Informe o Numero da Remessa :',NumRemessa);

  FACBrBoleto.GerarRemessa(StrToInt64Def(NumRemessa,0));
end;

procedure TfrmDemoBoleto.btnImpressaoSpoolerClick(Sender: TObject);
//var
//  i: Integer; 
begin
  if not Assigned(FACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create(MOTOR_NAO_SELECIONADO);

  FACBrBoleto.ACBrBoletoFC.PdfSenha := edtSenhaPDF.Text;
  FACBrBoleto.Imprimir;

  //Método para impressao de cada titulo de forma individual
   {for i:= 0 to FACBrBoleto1.ListadeBoletos.Count -1 do
   begin
     FACBrBoleto1.ListadeBoletos[i].Imprimir();

   end; }
end;

procedure TfrmDemoBoleto.FormCreate(Sender: TObject);
var
  I: TACBrBolLayOut;
  CurrentStyle : longint;
begin
  FACBrBoleto := TACBrBoleto.Create(Self);
  FACBrMail   := TACBrMail.Create(FACBrBoleto);
  {$IFDEF GERADOR_FORTES_REPORT}
    FACBrBoletoFCRL   := TACBrBoletoFCFortes.Create(FACBrBoleto);
    cbxMotorRelatorio.AddItem('Fortes Reports', FACBrBoletoFCRL);
  {$ENDIF}

  {$IFDEF GERADOR_FAST_REPORT}
    FACBrBoletoFCFR   := TACBrBoletoFCFR.Create(FACBrBoleto);
    cbxMotorRelatorio.AddItem('Fast Reports', FACBrBoletoFCFR);
  {$ENDIF}

  {$IFDEF GERADOR_FPDF}
    FACBrBoletoFPDF   := TACBrBoletoFPDF.Create(FACBrBoleto);
    cbxMotorRelatorio.AddItem('FDPF', FACBrBoletoFPDF);
  {$ENDIF}




  CurrentStyle := GetWindowLong(edtCNABLVLote.Handle, GWL_STYLE);
  CurrentStyle := CurrentStyle or ES_NUMBER;
  SetWindowLong(edtCNABLVLote.Handle, GWL_STYLE, CurrentStyle);
  SetWindowLong(edtCNABLVArquivo.Handle, GWL_STYLE, CurrentStyle);

   edtDataDoc.Text    := DateToStr(Now);
   edtVencimento.Text := DateToStr(IncMonth(StrToDate(edtDataDoc.Text),1));
   edtDataMora.Text   := DateToStr(StrToDate(edtVencimento.Text)+1);

  cbxLayOut.Items.Clear;
  For I := Low(TACBrBolLayOut) to High(TACBrBolLayOut) do
    cbxLayOut.Items.Add(GetEnumName(TypeInfo(TACBrBolLayOut), Integer(I)));
  cbxLayOut.ItemIndex := 0;

  CarregarBancos;
  CarregarTipoDistribuicao;
  CarregarCaracteristicaTitulo;
  CarregarResponsavelEmissao;
  CarregarTipoCarteira;
  CarregarTipoDocumento;
  CarregarSSLLib;
  LerIniComponente;
  AplicarConfiguracoesComponenteATela;
  edtPathRemessa.Text := ExtractFilePath(ParamStr(0))+'Remessa';
  edtPathRetorno.Text := ExtractFilePath(ParamStr(0))+'Retorno';

  if cbxMotorRelatorio.Items.Count > 0 then
  begin
    cbxMotorRelatorio.ItemIndex := 0;
    cbxMotorRelatorio.OnChange(cbxMotorRelatorio);
  end;
end;

procedure TfrmDemoBoleto.carregarBancos;
var
  Banco: TACBrTipoCobranca;
begin
  cbxBanco.Items.clear;
	for Banco := Low(TACBrTipoCobranca) to High(TACBrTipoCobranca) do
    cbxBanco.Items.AddObject( GetEnumName(TypeInfo(TACBrTipoCobranca), integer(Banco) ), TObject(integer(Banco)) );
end;

procedure TfrmDemoBoleto.CarregarCaracteristicaTitulo;
var
  Caracteristica: TACBrCaracTitulo;
begin
  cbxCaracteristicaTitulo.Items.clear;
	for Caracteristica := Low(TACBrCaracTitulo) to High(TACBrCaracTitulo) do
    cbxCaracteristicaTitulo.Items.Add( GetEnumName(TypeInfo(TACBrCaracTitulo), integer(Caracteristica) ) );
end;

procedure TfrmDemoBoleto.CarregarResponsavelEmissao;
var
  ResponsavelEmissao: TACBrResponEmissao;
begin
  cbxResponsavelEmissao.Items.clear;
	for ResponsavelEmissao := Low(TACBrResponEmissao) to High(TACBrResponEmissao) do
    cbxResponsavelEmissao.Items.Add( GetEnumName(TypeInfo(TACBrResponEmissao), integer(ResponsavelEmissao) ) );
end;

procedure TfrmDemoBoleto.CarregarSSLLib;
var
  SSLLib: TSSLHttpLib;
begin
  cbxSSLLib.Items.clear;
	for SSLLib := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbxSSLLib.Items.Add( GetEnumName(TypeInfo(TSSLHttpLib), integer(SSLLib) ) );
end;

procedure TfrmDemoBoleto.CarregarTipoCarteira;
var
  TipoCarteira: TACBrTipoCarteira;
begin
  cbxTipoCarteira.Items.clear;
	for TipoCarteira := Low(TACBrTipoCarteira) to High(TACBrTipoCarteira) do
    cbxTipoCarteira.Items.Add( GetEnumName(TypeInfo(TACBrTipoCarteira), integer(TipoCarteira) ) );
end;

procedure TfrmDemoBoleto.CarregarTipoDistribuicao;
var
  Distribuicao: TACBrIdentDistribuicao;
begin
  cbxTipoDistribuicao.Items.clear;
	for Distribuicao := Low(TACBrIdentDistribuicao) to High(TACBrIdentDistribuicao) do
    cbxTipoDistribuicao.Items.Add( GetEnumName(TypeInfo(TACBrIdentDistribuicao), integer(Distribuicao) ) );
end;

procedure TfrmDemoBoleto.CarregarTipoDocumento;
var
  TipoDocumento: TACBrTipoDocumento;
begin
  cbxTipoDocumento.Items.clear;
	cbxTipoDocumento.Items.Add('Tradicional');
  cbxTipoDocumento.Items.Add('Escritural');
end;

procedure TfrmDemoBoleto.Button3Click(Sender: TObject);
begin
  FACBrBoleto.ListadeBoletos.Clear;
end;

procedure TfrmDemoBoleto.cbxLayOutChange(Sender: TObject);
begin
  FACBrBoleto.ACBrBoletoFC.LayOut := TACBrBolLayOut( cbxLayOut.ItemIndex );

  cbxImprimirVersoFatura.Enabled := (cbxLayOut.ItemIndex = 6); // lFaturaDetal
  if cbxLayOut.ItemIndex <> 6 then
   cbxImprimirVersoFatura.Checked := false;
end;

procedure TfrmDemoBoleto.chkMostrarSenhaClick(Sender: TObject);
begin
  if chkMostrarSenha.Checked then
    edtPassword.PasswordChar := #0
  else
    edtPassword.PasswordChar := '@';
end;

procedure TfrmDemoBoleto.btnEnviarEmailClick(Sender: TObject);
var
  SL: TStringList;
  //i: Integer;
begin
  AplicarConfiguracoesComponenteEmail;
  SL := TStringList.Create;
  try
    SL.Add('Olá,');
    SL.Add('Atenção, Boleto está em Anexo');
    FACBrBoleto.EnviarEmail(edtEmail.Text ,'Teste de Envio de Email', SL, True);

    //Método para envio e-mail de forma individual para cada título
    {for i := 0 to FACBrBoleto.ListadeBoletos.Count -1 do
    begin
      if (FACBrBoleto.ListadeBoletos[i].Sacado.Email <> '') then
        FACBrBoleto.ListadeBoletos[i].EnviarEmail(FACBrBoleto.ListadeBoletos[i].Sacado.Email ,'Teste de Envio de Email', SL, True);

    end;}

  finally
    SL.Free;
  end;
end;


procedure TfrmDemoBoleto.btnWSConsultaClick(Sender: TObject);
var
  FiltrosAPI : TACBrBoletoWSFiltroConsulta;
  Boleto : TACBrBoleto;
  SLRetorno : TStringList;
  Retorno : TListaACBrBoletoRetornoWS;
  RetornoDetalhe : TACBrBoletoRetornoWS;
  I: Integer;
begin
  //Exemplo utilizando como Banco do Brasil API
  Boleto     := FACBrBoleto;
  FiltrosAPI := Boleto.Configuracoes.WebService.Filtro;

  FiltrosAPI.Clear;
  FiltrosAPI.indicadorSituacao        := isbBaixado;
  FiltrosAPI.dataMovimento.DataInicio := IncDay(Date,-1);
  FiltrosAPI.dataMovimento.DataFinal  := Date;
  //FiltrosAPI.indiceContinuidade       := 300;

  case cbbWSConsulta.ItemIndex of
    0 : Boleto.Configuracoes.WebService.Operacao := tpConsulta;
    1 : Boleto.Configuracoes.WebService.Operacao := tpConsultaDetalhe;
  end;

  Boleto.Enviar;

  case Boleto.Configuracoes.WebService.Operacao of
    tpConsulta:
      begin
        Retorno := Boleto.ListaConsultaRetornoWeb;
        if Retorno.Count > 0 then
        begin
          SLRetorno := TStringList.Create;
          try
            SLRetorno.Add('Cod_Retorno='+ Retorno[i].CodRetorno + sLineBreak +
                               'Msg_Retorno='+ Retorno[i].MsgRetorno + sLineBreak +
                               'Ori_Retorno='+ Retorno[i].OriRetorno + sLineBreak +
                               'HTTP_Result='+ IntToStr(Retorno[i].HTTPResultCode) + sLineBreak +
                               'JSON='+ Retorno[i].JSON);
            SLRetorno.Add('indicadorContinuidade=' + BoolToStr(Retorno[0].indicadorContinuidade));
            SLRetorno.Add('proximoIndice=' + IntToStr(Retorno[0].proximoIndice));
            SLRetorno.Add(' ');
            SLRetorno.Add(' ');
            for I := 0 to Pred(Retorno.Count) do
            begin
              SLRetorno.Add('[Boletos Index = '             + FormatFloat('000',I)+']');
              SLRetorno.Add('numeroBoleto = '               + Retorno[I].DadosRet.TituloRet.NossoNumero);
              SLRetorno.Add('SeuNumero = '                  + Retorno[I].DadosRet.TituloRet.SeuNumero);
              SLRetorno.Add('dataRegistro = '               + DateToStr(Retorno[I].DadosRet.TituloRet.DataRegistro));
              SLRetorno.Add('dataVencimento = '             + DateToStr(Retorno[I].DadosRet.TituloRet.Vencimento));
              SLRetorno.Add('valorOriginal = '              + DateToStr(Retorno[I].DadosRet.TituloRet.ValorDocumento));
              SLRetorno.Add('carteiraConvenio = '           + Retorno[I].DadosRet.TituloRet.Carteira);
              SLRetorno.Add('variacaoCarteiraConvenio = '   + intToStr(Retorno[I].DadosRet.TituloRet.Modalidade));
              SLRetorno.Add('codigoEstadoTituloCobranca = ' + Retorno[I].DadosRet.TituloRet.codigoEstadoTituloCobranca);
              SLRetorno.Add('estadoTituloCobranca = '       + Retorno[I].DadosRet.TituloRet.estadoTituloCobranca);
              SLRetorno.Add('contrato = '                   + Retorno[I].DadosRet.TituloRet.Contrato);
              SLRetorno.Add('dataMovimento = '              + DateToStr(Retorno[I].DadosRet.TituloRet.dataMovimento));
              SLRetorno.Add('dataCredito = '                + DateToStr(Retorno[I].DadosRet.TituloRet.dataCredito));
              SLRetorno.Add('valorAtual = '                 + CurrToStr(Retorno[I].DadosRet.TituloRet.valorAtual));
              SLRetorno.Add('valorPago = '                  + CurrToStr(Retorno[I].DadosRet.TituloRet.ValorPago));
              SLRetorno.Add('  ---  ');
            end;
            SLRetorno.SaveToFile( PathWithDelim(ExtractFilePath(Application.ExeName))+formatDateTime('yyyy.mm.dd.hh.nn.ss.zzz',now)+'-RetornoConsulta.txt' );
          finally
            SLRetorno.Free;
          end;
        end;

      end;
    tpConsultaDetalhe:
      begin
        if Boleto.TotalListaRetornoWeb > 0 then
        begin
          RetornoDetalhe := Boleto.ListaRetornoWeb[i];
          SLRetorno := TStringList.Create;
          try
            SLRetorno.Add('Cod_Retorno='+ RetornoDetalhe.CodRetorno + sLineBreak +
                               'Msg_Retorno='+ RetornoDetalhe.MsgRetorno + sLineBreak +
                               'Ori_Retorno='+ RetornoDetalhe.OriRetorno + sLineBreak +
                               'HTTP_Result='+ IntToStr(RetornoDetalhe.HTTPResultCode) + sLineBreak +
                               'JSON='+ RetornoDetalhe.JSON);
            SLRetorno.Add('indicadorContinuidade=' + BoolToStr(RetornoDetalhe.indicadorContinuidade));
            SLRetorno.Add('proximoIndice=' + IntToStr(RetornoDetalhe.proximoIndice));
            SLRetorno.Add(' ');
            SLRetorno.Add(' ');
            for I := 0 to Pred(Boleto.ListadeBoletos.Count) do
            begin
              RetornoDetalhe := Boleto.ListaRetornoWeb[i];
              SLRetorno.Add('[Boletos Index = '             + FormatFloat('000',I)+']');
              SLRetorno.Add('numeroBoletoBB = '             + RetornoDetalhe.DadosRet.TituloRet.NossoNumero);
              SLRetorno.Add('dataRegistro = '               + DateToStr(RetornoDetalhe.DadosRet.TituloRet.DataRegistro));
              SLRetorno.Add('dataVencimento = '             + DateToStr(RetornoDetalhe.DadosRet.TituloRet.Vencimento));
              SLRetorno.Add('valorOriginal = '              + CurrToStr(RetornoDetalhe.DadosRet.TituloRet.ValorDocumento));
              SLRetorno.Add('carteiraConvenio = '           + RetornoDetalhe.DadosRet.TituloRet.Carteira);
              SLRetorno.Add('variacaoCarteiraConvenio = '   + intToStr(RetornoDetalhe.DadosRet.TituloRet.Modalidade));
              SLRetorno.Add('codigoEstadoTituloCobranca = ' + RetornoDetalhe.DadosRet.TituloRet.codigoEstadoTituloCobranca);
              SLRetorno.Add('estadoTituloCobranca = '       + RetornoDetalhe.DadosRet.TituloRet.estadoTituloCobranca);
              SLRetorno.Add('contrato = '                   + RetornoDetalhe.DadosRet.TituloRet.Contrato);
              SLRetorno.Add('dataMovimento = '              + DateToStr(RetornoDetalhe.DadosRet.TituloRet.dataMovimento));
              SLRetorno.Add('dataCredito = '                + DateToStr(RetornoDetalhe.DadosRet.TituloRet.dataCredito));
              SLRetorno.Add('valorAtual = '                 + CurrToStr(RetornoDetalhe.DadosRet.TituloRet.valorAtual));
              SLRetorno.Add('valorPago = '                  + CurrToStr(RetornoDetalhe.DadosRet.TituloRet.ValorPago));
              SLRetorno.Add('  ---  ');
            end;
            SLRetorno.SaveToFile( PathWithDelim(ExtractFilePath(Application.ExeName))+formatDateTime('yyyy.mm.dd.hh.nn.ss.zzz',now)+'-RetornoConsultaDetalhe.txt' );
          finally
            SLRetorno.Free;
          end;
        end;
      end;
  end;
  showMessage('Fim');
end;

{
--Utiliza WebService dos Bancos para realizar o Registro dos Boletos--
Até o momento disponível para Caixa Economica, Banco do Brasil e Itau
É necessario realizar a configuração previa para acesso ao WebService
No Object Inspector verifique as propriedades: CedenteWS e Configuracoes/WebService
Verifique no arquivo "configWebService.txt" quais as configurações necessárias para cada Banco
}
procedure TfrmDemoBoleto.btnWSRegistrarClick(Sender: TObject);
var
  SLRemessa: TStringList;
  i, j: Integer;
  Boleto : TACBrBoleto;
begin
  Boleto := FACBrBoleto;

  //Função de Envio
  Boleto.Configuracoes.WebService.Operacao := tpInclui;
  Boleto.Enviar; // <<< retorna como false se o httpresult code for diferente de 200,201,202
  //Verifica Lista com os retornos

  if Boleto.TotalListaRetornoWeb > 0 then
  begin
    SLRemessa := TStringList.Create;
    try
      for i:= 0 to Pred(Boleto.TotalListaRetornoWeb) do
      begin
        //Ler todos os campos da classe Retorno
        SLRemessa.Add('Cod_Retorno='+ Boleto.ListaRetornoWeb[i].CodRetorno + sLineBreak +
                      'Msg_Retorno='+ Boleto.ListaRetornoWeb[i].MsgRetorno + sLineBreak +
                      'Ori_Retorno='+ Boleto.ListaRetornoWeb[i].OriRetorno + sLineBreak +
                      'HTTP_Result='+ IntToStr(Boleto.ListaRetornoWeb[i].HTTPResultCode) + sLineBreak +
                      'JSON='+ Boleto.ListaRetornoWeb[i].JSON);
         for j:= 0 to Pred(Boleto.ListaRetornoWeb[i].ListaRejeicao.Count) do
         begin
           SLRemessa.Add('[Rejeicao'   +IntToStr(j)+']' + sLineBreak +
                         'Campo='      + Boleto.ListaRetornoWeb[i].ListaRejeicao[j].Campo + sLineBreak +
                         'Codigo='     + Boleto.ListaRetornoWeb[i].ListaRejeicao[j].Codigo + sLineBreak +
                         'Versao='     + Boleto.ListaRetornoWeb[i].ListaRejeicao[j].Versao + sLineBreak +
                         'Mensagem='   + Boleto.ListaRetornoWeb[i].ListaRejeicao[j].Mensagem + sLineBreak +
                         'Ocorrencia=' + Boleto.ListaRetornoWeb[i].ListaRejeicao[j].Ocorrencia + sLineBreak +
                         'Valor='      + Boleto.ListaRetornoWeb[i].ListaRejeicao[j].Valor + sLineBreak );
         end;

         SLRemessa.Add('HEADER'          + sLineBreak +
                     'Versao='           + Boleto.ListaRetornoWeb[i].Header.Versao + sLineBreak +
                     'Autenticacao='     + Boleto.ListaRetornoWeb[i].Header.Autenticacao + sLineBreak +
                     'Usuario_Servico='  + Boleto.ListaRetornoWeb[i].Header.Usuario_Servico + sLineBreak +
                     'Usuario='          + Boleto.ListaRetornoWeb[i].Header.Usuario + sLineBreak +
                     'Operacao='         + TipoOperacaoToStr(Boleto.ListaRetornoWeb[i].Header.Operacao) + sLineBreak +
                     'Indice='           + IntToStr(Boleto.ListaRetornoWeb[i].Header.Indice) + sLineBreak +
                     'Sistema_Origem='   + Boleto.ListaRetornoWeb[i].Header.Sistema_Origem + sLineBreak +
                     'Agencia='          + IntToStr(Boleto.ListaRetornoWeb[i].Header.Agencia) + sLineBreak +
                     'ID_Origem='        + Boleto.ListaRetornoWeb[i].Header.Id_Origem + sLineBreak +
                     'Data_Hora='        +FormatDateTime('dd/mm/yyyy hh:nn:ss',Boleto.ListaRetornoWeb[i].Header.Data_Hora) + sLineBreak +
                     'ID_Processo='      + Boleto.ListaRetornoWeb[i].Header.Id_Processo + sLineBreak +
                     'DADOS'             + sLineBreak +
                     'Excessao='         +Boleto.ListaRetornoWeb[i].DadosRet.Excecao + sLineBreak +
                     'CONTROLE_NEGOCIAL' + sLineBreak +
                     'Origem_Retorno='   + Boleto.ListaRetornoWeb[i].DadosRet.ControleNegocial.OriRetorno + sLineBreak +
                     'NSU='              + Boleto.ListaRetornoWeb[i].DadosRet.ControleNegocial.NSU + sLineBreak +
                     'Cod_Retorno='      + Boleto.ListaRetornoWeb[i].DadosRet.ControleNegocial.CodRetorno + sLineBreak +
                     'Msg_Retorno='      + Boleto.ListaRetornoWeb[i].DadosRet.ControleNegocial.Retorno + sLineBreak +
                     'COMPROVANTE'       + sLineBreak +
                     'Data='             +  FormatDateTime('dd/mm/yyyy', Boleto.ListaRetornoWeb[i].DadosRet.Comprovante.Data) + sLineBreak +
                     'Hora='             +  Boleto.ListaRetornoWeb[i].DadosRet.Comprovante.Hora + sLineBreak +
                     'ID_BOLETO'         + sLineBreak +
                     'Codigo_Barras='    + Boleto.ListaRetornoWeb[i].DadosRet.IDBoleto.CodBarras + sLineBreak +
                     'Linha_Digitavel='  + Boleto.ListaRetornoWeb[i].DadosRet.IDBoleto.LinhaDig + sLineBreak +
                     'Nosso_Numero='     + Boleto.ListaRetornoWeb[i].DadosRet.IDBoleto.NossoNum + sLineBreak +
                     'URL='              + Boleto.ListaRetornoWeb[i].DadosRet.IDBoleto.URL + sLineBreak +
                     'CONSULTA_BOLETO'   + sLineBreak +
                     'Numero_Documento=' + Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.NumeroDocumento + sLineBreak +
                     'Data_Vencimento='  + FormatDateTime('dd/mm/yyyy',Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.Vencimento) + sLineBreak +
                     'Valor='            + CurrToStr(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.ValorDocumento) + sLineBreak
                      );
        if NaoEstaVazio(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.CodBarras) then
        begin
         SLRemessa.Add('TITULO_RETORNO'            + sLineBreak  +
          'vencimento_titulo='                     +FormatDateTime('dd/mm/yyyy',Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.Vencimento)+ sLineBreak +
          'tipo_carteira_titulo='                  +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.Carteira+ sLineBreak +
          'nosso_numero='                          +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.NossoNumero+ sLineBreak +
          'seu_numero='                            +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.SeuNumero+ sLineBreak +
          'especie='                               +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.EspecieDoc+ sLineBreak +
          'codigo_barras='                         +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.CodBarras+ sLineBreak +
          'numero_linha_digitavel='                +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.LinhaDig+ sLineBreak +
          'local_pagamento='                       +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.Mensagem.Text+ sLineBreak +
          'data_processamento='                    +FormatDateTime('dd/mm/yyyy',Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.DataProcessamento)+ sLineBreak +
          'data_emissao='                          +FormatDateTime('dd/mm/yyyy',Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.DataDocumento)+ sLineBreak +
          'uso_banco='                             +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.UsoBanco+ sLineBreak +
          'valor_titulo='                          +CurrToStr(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.ValorDocumento)+ sLineBreak +
          'valor_desconto='                        +CurrToStr(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.ValorDesconto)+ sLineBreak +
          'valor_outra_deducao='                   +CurrToStr(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.ValorDespesaCobranca)+ sLineBreak +
          'valor_juro_multa='                      +CurrToStr(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.ValorMoraJuros)+ sLineBreak +
          'valor_outro_acrescimo='                 +CurrToStr(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.ValorOutrosCreditos)+ sLineBreak +
          'valor_total_cobrado='                   +CurrToStr(Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.ValorPago) + sLineBreak +
          'texto_informacao_cliente_beneficiario=' +Boleto.ListaRetornoWeb[i].DadosRet.TituloRet.Informativo.Text  );

        end;
      end;
      SLRemessa.SaveToFile( PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoRegistro.txt' );
    finally
      SLRemessa.Free;
    end;
    ShowMessage('Retorno Envio gerado em: '+ PathWithDelim(ExtractFilePath(Application.ExeName))+'RetornoRegistro.txt' );

  end;

end;

procedure TfrmDemoBoleto.btnConfigLerClick(Sender: TObject);
begin
  LerIniComponente(True);
  AplicarConfiguracoesComponenteATela;
end;

procedure TfrmDemoBoleto.btnConfigGravarClick(Sender: TObject);
var teste : TStringList;
begin
  AplicarConfiguracoesAoComponente;
  GravarIniComponente;
end;

procedure TfrmDemoBoleto.btnImpressaoPDFIndividualClick(Sender: TObject);
var Index : Cardinal;
begin
  if not Assigned(FACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create(MOTOR_NAO_SELECIONADO);

  for Index := 0 to Pred(FACBrBoleto.ListadeBoletos.Count) do
  begin
    FACBrBoleto.ACBrBoletoFC.CalcularNomeArquivoPDFIndividual := True;
    FACBrBoleto.ACBrBoletoFC.PdfSenha := edtSenhaPDF.Text;
    FACBrBoleto.GerarPDF(Index);
  end;
end;

procedure TfrmDemoBoleto.btnImpressaoStreamClick(Sender: TObject);
var LMeuStream : TStream;
    xPath : string;
begin
  if not Assigned(FACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create(MOTOR_NAO_SELECIONADO);

  xPath := ExtractFilePath(Application.ExeName) + 'testeStream.pdf';
  InputQuery('Salvando Boleto em Stream','Caminho + Arquivo + Extenção a salvar o Stream',xPath);
  LMeuStream := TFileStream.Create(xPath,fmCreate or fmOpenWrite);
  try
    FACBrBoleto.ACBrBoletoFC.PdfSenha := edtSenhaPDF.Text;
    FACBrBoleto.Imprimir(LMeuStream);
  finally
    LMeuStream.Free;
  end;
end;

procedure TfrmDemoBoleto.btnImprimirTesteClick(Sender: TObject);
var
  LNrTitulos : Cardinal;
  LValor : Currency;
  I : Integer;
begin
  if not Assigned(FACBrBoleto.ACBrBoletoFC) then
    raise Exception.Create(MOTOR_NAO_SELECIONADO);

  FACBrBoleto.ListadeBoletos.Clear;
  LNrTitulos := 10;
  memMensagem.Lines.Add('Escrevo hoje para falar sobre um grupo especial de criaturas que roubaram nossos corações e nos fizeram sorrir com suas travessuras hilárias - os pinguins de Madagascar.');
  memMensagem.Lines.Add('Essas adoráveis aves têm conquistado o mundo com sua personalidade cativante e aventuras malucas, e há algo de mágico em sua presença.');
  for I := 1 to Pred(LNrTitulos) do
  begin
    LValor := StrToFloatDef(edtValorDoc.Text,1);
    LValor := LValor + Random;
    edtValorDoc.Text  := CurrToStr(LValor);
    edtNossoNro.Text  := IntToStr(StrToIntDef(edtNossoNro.Text,0)+1);
    edtNumeroDoc.Text := IntToStr(StrToIntDef(edtNumeroDoc.Text,0)+1);

    btnBoletoIndividual.Click;
  end;

  FACBrBoleto.ACBrBoletoFC.PdfSenha := edtSenhaPDF.Text;
  FACBrBoleto.Imprimir;
end;

procedure TfrmDemoBoleto.btnRetornoClick(Sender: TObject);
begin
  if dlgFile.Execute then
    edtPathRetorno.Text := dlgFile.FileName;
end;

procedure TfrmDemoBoleto.Button1Click(Sender: TObject);
begin
  if dlgSave.Execute then
    edtPathLog.Text := PathWithDelim(ExtractFilePath(dlgSave.FileName));
end;

procedure TfrmDemoBoleto.cbxMotorRelatorioChange(Sender: TObject);
var
  LSelectedItemIndex: Integer;
  LSelectedObject: TObject;
begin
  LSelectedItemIndex := cbxMotorRelatorio.ItemIndex;

  if LSelectedItemIndex <> -1 then
    LSelectedObject := TObject(cbxMotorRelatorio.Items.Objects[LSelectedItemIndex]);

  if Assigned(LSelectedObject) then
  begin
    FACBrBoleto.ACBrBoletoFC := TACBrBoletoFCClass(LSelectedObject);
    FACBrBoleto.ACBrBoletoFC.DirLogo := edtPathLogoMarca.Text;
    {$IFDEF GERADOR_FAST_REPORT}
      if FACBrBoleto.ACBrBoletoFC is TACBrBoletoFCFR then
        TACBrBoletoFCFR(FACBrBoleto.ACBrBoletoFC).FastReportFile := edtPathFR3.Text;
    {$ENDIF}
  end;

  GravarIniComponente;
end;

procedure TfrmDemoBoleto.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FACBrBoleto.Free;
end;

end.
