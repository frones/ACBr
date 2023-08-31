{******************************************************************************}
{ Projeto: Aplicação de demonstração ACBrPIXCD Matera                          }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

{$I ACBr.inc}

unit PixCDMatera;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, ACBrPIXCD, ACBrPIXPSPMatera, ACBrCEP,
  ACBrOpenSSLUtils, Clipbrd, ACBrImage, ACBrDelphiZXingQRCode, ACBrUtil.Math,
  ACBrSocket, ACBrBase, ImgList;

const
  cURL_ACBR = 'https://projetoacbr.com.br/tef/';

type

  { TfrPixCDMatera }

  TfrPixCDMatera = class(TForm)
    ACBrCEP1: TACBrCEP;
    ACBrOpenSSLUtils1: TACBrOpenSSLUtils;
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPMatera1: TACBrPSPMatera;
    btAcharArqCertificado: TSpeedButton;
    btAcharChavePrivada: TSpeedButton;
    btChavePIXConsultar: TBitBtn;
    btChavePIXCriar: TSpeedButton;
    btChavePIXExcluir: TBitBtn;
    btChavePIXIncluir: TBitBtn;
    btCobCopiaECola: TSpeedButton;
    btConsultarCob: TBitBtn;
    btContaConsultar: TBitBtn;
    btContaCriar: TBitBtn;
    btContaCriarExternalID: TSpeedButton;
    btContaCriarLimparDados: TBitBtn;
    btContaCriarPreencherDados: TBitBtn;
    btContaCriarRepresentanteFoto: TSpeedButton;
    btContaCriarRepresentanteRGFotoFrente: TSpeedButton;
    btContaCriarRepresentanteRGFotoVerso: TSpeedButton;
    btContaInativar: TBitBtn;
    btCriarCobranca: TBitBtn;
    btFluxoCancelarCobranca: TBitBtn;
    btFluxoCancelarConsulta: TBitBtn;
    btFluxoCopiaECola: TSpeedButton;
    btFluxoEstornarPagto: TBitBtn;
    btFluxoFecharVenda: TBitBtn;
    btFluxoNovaVenda: TBitBtn;
    btFluxoPagar: TBitBtn;
    btFluxoTentarNovamente: TBitBtn;
    btLerParametros: TBitBtn;
    btLogArquivo: TSpeedButton;
    btLogGerencialLimpar: TBitBtn;
    btLogOperacoesLimpar: TBitBtn;
    btProxyVerSenha: TSpeedButton;
    btQRCodeCriarLimparDados: TBitBtn;
    btQRCodeCriarPreencherDados: TBitBtn;
    btQRCodeGerarExternalID: TSpeedButton;
    btSalvarParametros: TBitBtn;
    cbAmbiente: TComboBox;
    cbContaCriarTipoCliente: TComboBox;
    cbCriarContaTipoConta: TComboBox;
    cbLogNivel: TComboBox;
    chkQRCodeADshowToPayer: TCheckBox;
    cbQRCodeTipoCobranca: TComboBox;
    edAccountId: TEdit;
    edArqCertificado: TEdit;
    edArqChavePrivada: TEdit;
    edChavePIX: TEdit;
    edChavePIXConsultar: TEdit;
    edChavePIXExcluir: TEdit;
    edChavePIXExcluirAccountId: TEdit;
    edChavePIXIncluir: TEdit;
    edChavePIXIncluirAccountId: TEdit;
    edCobCopiaECola: TEdit;
    edConsultarCobAccountID: TEdit;
    edConsultarCobTransactionID: TEdit;
    edContaConsultarAccountId: TEdit;
    edContaCriarBairro: TEdit;
    edContaCriarCelular: TEdit;
    edContaCriarCEP: TEdit;
    edContaCriarCidade: TEdit;
    edContaCriarComplemento: TEdit;
    edContaCriarCPF_CNPJ: TEdit;
    edContaCriarEmail: TEdit;
    edContaCriarExternalID: TEdit;
    edContaCriarFundacao: TDateTimePicker;
    edContaCriarLogradouro: TEdit;
    edContaCriarNascimento: TDateTimePicker;
    edContaCriarNomeCliente: TEdit;
    edContaCriarNomeEmpresa: TEdit;
    edContaCriarNumero: TEdit;
    edContaCriarRepresentanteBairro: TEdit;
    edContaCriarRepresentanteCelular: TEdit;
    edContaCriarRepresentanteCEP: TEdit;
    edContaCriarRepresentanteCidade: TEdit;
    edContaCriarRepresentanteCPF: TEdit;
    edContaCriarRepresentanteEmail: TEdit;
    edContaCriarRepresentanteFoto: TEdit;
    edContaCriarRepresentanteLogradouro: TEdit;
    edContaCriarRepresentanteMae: TEdit;
    edContaCriarRepresentanteNome: TEdit;
    edContaCriarRepresentanteNome1: TEdit;
    edContaCriarRepresentanteNumero: TEdit;
    edContaCriarRepresentanteRGFotoFrente: TEdit;
    edContaCriarRepresentanteRGFotoVerso: TEdit;
    edContaCriarRepresentanteUF: TEdit;
    edContaCriarUF: TEdit;
    edContaInativarAccountId: TEdit;
    edCriarContaFundacao1: TDateTimePicker;
    edCriarContaRepresentanteNome2: TEdit;
    edCriarContaRepresentanteNome3: TEdit;
    edCriarContaRepresentanteNome4: TEdit;
    edCriarContaRepresentanteNome5: TEdit;
    edCriarContaRepresentanteNome6: TEdit;
    edCriarContaRepresentanteNome7: TEdit;
    edCriarContaRepresentanteNome8: TEdit;
    edFluxoClienteDoc1: TEdit;
    edFluxoClienteNome1: TEdit;
    edFluxoCopiaECola: TEdit;
    edLogArquivo: TEdit;
    edProxyHost: TEdit;
    edProxyPorta: TSpinEdit;
    edProxySenha: TEdit;
    edProxyUsuario: TEdit;
    edPSPClientID: TEdit;
    edPSPClientSecret: TEdit;
    edPSPSecretKey: TEdit;
    edQRCodeAccountId: TEdit;
    edQRCodeADcontent: TEdit;
    edQRCodeADname: TEdit;
    edQRCodeCallBack: TEdit;
    edQRCodeChavePIX: TEdit;
    edQRCodediscountsdate: TDateTimePicker;
    edQRCodediscountsmodality: TEdit;
    edQRCodediscountsvaluePerc: TEdit;
    edQRCodedueDate: TDateTimePicker;
    edQRCodeExternalID: TEdit;
    edQRCodefinesmodality: TEdit;
    edQRCodefinesvaluePerc: TEdit;
    edQRCodeinterestsmodality: TEdit;
    edQRCodeinterestsvaluePerc: TEdit;
    edQRCodepayerCEP: TEdit;
    edQRCodepayercity: TEdit;
    edQRCodePayercpfcnpj: TEdit;
    edQRCodePayerName: TEdit;
    edQRCodepayerstreet: TEdit;
    edQRCodepayeruf: TEdit;
    edQRCoderecipientComment: TEdit;
    edQRCodereductionmodality: TEdit;
    edQRCodereductionvaluePerc: TEdit;
    edQRCodeValor: TEdit;
    edTimeout: TSpinEdit;
    gbChavePIXConsultar: TGroupBox;
    gbChavePIXExcluir: TGroupBox;
    gbChavePIXIncluir: TGroupBox;
    gbCobranca: TGroupBox;
    gbContaCriarEndereco: TGroupBox;
    gbDadosAdicionais: TGroupBox;
    gbFluxoCliente1: TGroupBox;
    gbFluxoStatus: TGroupBox;
    gbFluxoTotal: TGroupBox;
    gbLog: TGroupBox;
    gbProxy: TGroupBox;
    gbPSP: TGroupBox;
    gbQRCodeDetalhesCobranca: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    ImageList1: TImageList;
    imCobQRCode: TImage;
    imErroCertificado: TImage;
    imErroChavePrivada: TImage;
    imFluxoQRCode: TImage;
    lbAmbiente: TLabel;
    lbArqCertificado: TLabel;
    lbArqChavePrivada: TLabel;
    lbChavePIXConsultar: TLabel;
    lbChavePIXConsultar1: TLabel;
    lbChavePIXConsultar2: TLabel;
    lbChavePIXConsultar3: TLabel;
    lbChavePIXConsultar4: TLabel;
    lbChavePIXExcluir: TLabel;
    lbChavePIXExcluirAccountId: TLabel;
    lbChavePIXIncluir: TLabel;
    lbChavePIXIncluir1: TLabel;
    lbChavePIXIncluirAccountId: TLabel;
    lbChavePIXIncluirAccountId1: TLabel;
    lbClientID: TLabel;
    lbClientSecret: TLabel;
    lbCobCopiaECola: TLabel;
    lbConsultarCobAccountID: TLabel;
    lbConsultarCobTransactionID: TLabel;
    lbContaConsultarAccountId: TLabel;
    lbContaConsultarAccountId1: TLabel;
    lbContaConsultarAccountId2: TLabel;
    lbContaCriar: TLabel;
    lbContaCriarBairro: TLabel;
    lbContaCriarCelular: TLabel;
    lbContaCriarCEP: TLabel;
    lbContaCriarCidade: TLabel;
    lbContaCriarComplemento: TLabel;
    lbContaCriarCPF_CNPJ: TLabel;
    lbContaCriarCPF_CNPJ1: TLabel;
    lbContaCriarExternalID: TLabel;
    lbContaCriarFundacao: TLabel;
    lbContaCriarFundacao1: TLabel;
    lbContaCriarFundacao2: TLabel;
    lbContaCriarLogradouro: TLabel;
    lbContaCriarNascimento: TLabel;
    lbContaCriarNomeCliente: TLabel;
    lbContaCriarNomeCliente1: TLabel;
    lbContaCriarNomeEmpresa: TLabel;
    lbContaCriarNumero: TLabel;
    lbContaCriarRepresentante: TLabel;
    lbContaCriarRepresentante1: TLabel;
    lbContaCriarRepresentanteBairro: TLabel;
    lbContaCriarRepresentanteCelular: TLabel;
    lbContaCriarRepresentanteCEP: TLabel;
    lbContaCriarRepresentanteCEP1: TLabel;
    lbContaCriarRepresentanteCidade: TLabel;
    lbContaCriarRepresentanteCidade1: TLabel;
    lbContaCriarRepresentanteCPF: TLabel;
    lbContaCriarRepresentanteEmail: TLabel;
    lbContaCriarRepresentanteFoto: TLabel;
    lbContaCriarRepresentanteLogradouro: TLabel;
    lbContaCriarRepresentanteLogradouro1: TLabel;
    lbContaCriarRepresentanteMae: TLabel;
    lbContaCriarRepresentanteNumero: TLabel;
    lbContaCriarRepresentanteRGFrente: TLabel;
    lbContaCriarRepresentanteRGVerso: TLabel;
    lbContaCriarRepresentanteUF: TLabel;
    lbContaCriarRepresentanteUF1: TLabel;
    lbContaCriarTipoCliente: TLabel;
    lbContaCriarTipoConta: TLabel;
    lbContaCriarUF: TLabel;
    lbContaInativarAccountId: TLabel;
    lbCriarContaFundacao1: TLabel;
    lbCriarContaRepresentante2: TLabel;
    lbCriarContaRepresentante3: TLabel;
    lbCriarContaRepresentante4: TLabel;
    lbCriarContaRepresentante5: TLabel;
    lbCriarContaRepresentante6: TLabel;
    lbCriarContaRepresentante7: TLabel;
    lbCriarContaRepresentante8: TLabel;
    lbErroCertificado: TLabel;
    lbErroChavePrivada: TLabel;
    lbExpiracao: TLabel;
    lbFluxoClienteDoc1: TLabel;
    lbFluxoClienteNome1: TLabel;
    lbFluxoCopiaECola: TLabel;
    lbItemPreco: TLabel;
    lbItemPreco1: TLabel;
    lbItemPreco2: TLabel;
    lbItemPreco3: TLabel;
    lbItemPreco4: TLabel;
    lbItemPreco5: TLabel;
    lbItemPreco6: TLabel;
    lbItemPreco7: TLabel;
    lbItemPreco8: TLabel;
    lbItemPreco9: TLabel;
    lbLog: TLabel;
    lbLog1: TLabel;
    lbLogArquivo: TLabel;
    lbLogNivel: TLabel;
    lbProxyHost: TLabel;
    lbProxyPorta: TLabel;
    lbProxySenha: TLabel;
    lbProxyUsuario: TLabel;
    lbQRCodeExternalID: TLabel;
    lbSecretKey: TLabel;
    lbTimeout: TLabel;
    lbUrlTEF: TLabel;
    mmContaConsultarResposta: TMemo;
    mmLogGerencial: TMemo;
    mmLogOperacoes: TMemo;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pnQRCodeInfoAdicionais: TPanel;
    Panel2: TPanel;
    pnQRCodeResult: TPanel;
    Panel4: TPanel;
    pBotoesConfiguracao: TPanel;
    pcContaCriarDadosAdicionais: TPageControl;
    pConfPIX: TPanel;
    pcTestes: TPageControl;
    pcTestes1: TPageControl;
    pgPrincipal: TPageControl;
    pnChavePIX: TPanel;
    pnChavePIXExcluir: TPanel;
    pnChavePIXIncluir: TPanel;
    pnChavesPIXConsultar: TPanel;
    pnCobranca: TPanel;
    pnContaConsultar: TPanel;
    pnContaCriarCorporate: TPanel;
    pnContaCriarPerson: TPanel;
    pnContaCriarRodape: TPanel;
    pnContaInativar: TPanel;
    pnFluxoBackground: TPanel;
    pnFluxoBotoes: TPanel;
    pnFluxoBotoesErroConsultar: TPanel;
    pnFluxoBotoesPrincipais: TPanel;
    pnFluxoBotoesRight: TPanel;
    pnFluxoBotoesRight1: TPanel;
    pnFluxoCliente1: TPanel;
    pnFluxoCopiaECola: TPanel;
    pnFluxoDiv2: TPanel;
    pnFluxoDiv3: TPanel;
    pnFluxoDiv4: TPanel;
    pnFluxoDiv7: TPanel;
    pnFluxoPagto: TPanel;
    pnFluxoQRCode: TPanel;
    pnFluxoRodape: TPanel;
    pnFluxoStatus: TPanel;
    pnFluxoTotal: TPanel;
    pnLog: TPanel;
    pnLogs: TPanel;
    pnLogs1: TPanel;
    pnLogsRodape: TPanel;
    pnLogsRodape1: TPanel;
    pnProxy: TPanel;
    pnPSP: TPanel;
    pnPSPMatera: TPanel;
    seCobrancaExpiracao: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tsChavePIX: TTabSheet;
    tsConfig: TTabSheet;
    tsConsultarCobranca: TTabSheet;
    tsContaConsultar: TTabSheet;
    tsContaCriar: TTabSheet;
    tsContaCriarCorporate: TTabSheet;
    tsContaCriarPerson: TTabSheet;
    tsContaEChaves: TTabSheet;
    tsContaInativar: TTabSheet;
    tsFluxoPagto: TTabSheet;
    tsGerarQRCodes: TTabSheet;
    tsMatera: TTabSheet;
    tsPIX: TTabSheet;
    tsTestes: TTabSheet;
    edValor: TEdit;
    procedure btAcharArqCertificadoClick(Sender: TObject);
    procedure btAcharChavePrivadaClick(Sender: TObject);
    procedure btChavePIXConsultarClick(Sender: TObject);
    procedure btChavePIXCriarClick(Sender: TObject);
    procedure btChavePIXExcluirClick(Sender: TObject);
    procedure btChavePIXIncluirClick(Sender: TObject);
    procedure btCobCopiaEColaClick(Sender: TObject);
    procedure btConsultarCobClick(Sender: TObject);
    procedure btContaConsultarClick(Sender: TObject);
    procedure btContaCriarClick(Sender: TObject);
    procedure btContaCriarLimparDadosClick(Sender: TObject);
    procedure btContaCriarPreencherDadosClick(Sender: TObject);
    procedure btContaInativarClick(Sender: TObject);
    procedure btCriarCobrancaClick(Sender: TObject);
    procedure btFluxoPagarClick(Sender: TObject);
    procedure btLogGerencialLimparClick(Sender: TObject);
    procedure btLogOperacoesLimparClick(Sender: TObject);
    procedure btProxyVerSenhaClick(Sender: TObject);
    procedure btQRCodeCriarLimparDadosClick(Sender: TObject);
    procedure btQRCodeCriarPreencherDadosClick(Sender: TObject);
    procedure btQRCodeGerarExternalIDClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure cbAmbienteChange(Sender: TObject);
    procedure cbContaCriarTipoClienteChange(Sender: TObject);
    procedure cbQRCodeTipoCobrancaChange(Sender: TObject);
    procedure edArqCertificadoExit(Sender: TObject);
    procedure edArqChavePrivadaExit(Sender: TObject);
    procedure edContaCriarCEPChange(Sender: TObject);
    procedure lbUrlTEFClick(Sender: TObject);
    procedure edOnlyNumbersKeyPress(Sender: TObject; var aKey: Char);
    procedure FormCreate(Sender: TObject);
    procedure btContaCriarExternalIDClick(Sender: TObject);
    procedure pgPrincipalChange(Sender: TObject);
  private
    function GetNomeArquivoConfig: String;
    function FormatarJson(aJson: String): String;
    function RemoverPathAplicacao(const AFileName: String): String;
    function AdicionarPathAplicacao(const AFileName: String): String;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;
    
    procedure AdicionarLinhaLog(aMsg: String);
    procedure LigarAlertasdeErrosDeConfiguracao;
    procedure TratarException(Sender: TObject; E: Exception);

    procedure InicializarComponentesDefault;

    procedure ValidarChavePrivada;
    procedure ValidarCertificado;
  public
    property NomeArquivoConfig: String read GetNomeArquivoConfig;
  end;

var
  frPixCDMatera: TfrPixCDMatera;

implementation

uses 
  {$IFDEF FPC}
    fpjson, jsonparser, jsonscanner, Jsons,
  {$ELSE}
    Jsons,
  {$ENDIF}
  IniFiles, synacode, synautil, TypInfo, pcnConversao, ACBrPIXUtil,
  ACBrValidador, ACBrSchemasMatera,
  ACBrUtil.Compatibilidade,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.Base;

{$R *.dfm}

{ TfrPixCDMatera }

procedure TfrPixCDMatera.lbUrlTEFClick(Sender: TObject);
begin
  OpenURL(cURL_ACBR);
end;

procedure TfrPixCDMatera.edOnlyNumbersKeyPress(Sender: TObject; var aKey: Char);
begin
  if (not CharInSet(aKey, [#8,#13,'0'..'9'])) then
    aKey := #0;
end;

function TfrPixCDMatera.GetNomeArquivoConfig: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

function TfrPixCDMatera.FormatarJson(aJson: String): String;
{$IFDEF FPC}
var
  jpar: TJSONParser;
  jd: TJSONData;
{$ENDIF}
begin
  Result := aJson;
  {$IFDEF FPC}
  Result := Trim(aJson);
  if EstaVazio(Result) then
    Exit;
  try
    try
      jpar := TJSONParser.Create(Result, [joUTF8]);
      jd := jpar.Parse;
      Result := jd.FormatJSON([], 2);
    finally
      jpar.Free;
      if Assigned(jd) then
        jd.Free;
    end;
  except
    Result := aJson;
  end;
  {$ENDIF}
end;

function TfrPixCDMatera.RemoverPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (Pos(ApplicationPath, s) = 1) then
    Result := ExtractFileName(s)
  else
    Result := s;
end;

function TfrPixCDMatera.AdicionarPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (s = '') then
    Result := s
  else if (ExtractFilePath(AFileName) <> '') then
    Result := s
  else
    Result := ApplicationPath + s;
end;

procedure TfrPixCDMatera.FormCreate(Sender: TObject);
begin
  Application.OnException := TratarException;
  pgPrincipal.ActivePageIndex := 0;
  InicializarComponentesDefault;
  LerConfiguracao;
end;

procedure TfrPixCDMatera.btContaCriarExternalIDClick(Sender: TObject);
begin
  edContaCriarExternalID.Text := CriarTxId;
end;

procedure TfrPixCDMatera.pgPrincipalChange(Sender: TObject);
begin
  pnLogs.Visible:=not (pgPrincipal.ActivePage = tsConfig);
  Splitter1.Visible:=pnLogs.Visible;
end;

procedure TfrPixCDMatera.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
  AplicarConfiguracao;
end;

procedure TfrPixCDMatera.cbAmbienteChange(Sender: TObject);
begin
  btContaCriarPreencherDados.Enabled := (cbAmbiente.ItemIndex <> 1);
end;

procedure TfrPixCDMatera.cbContaCriarTipoClienteChange(Sender: TObject);
begin
  pnContaCriarCorporate.Visible := (cbContaCriarTipoCliente.ItemIndex = 1);
  pnContaCriarPerson.Visible := (cbContaCriarTipoCliente.ItemIndex <> 1);
end;

procedure TfrPixCDMatera.cbQRCodeTipoCobrancaChange(Sender: TObject);
begin
  gbQRCodeDetalhesCobranca.Visible := (cbQRCodeTipoCobranca.ItemIndex = 1);
end;

procedure TfrPixCDMatera.edArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificado;
end;

procedure TfrPixCDMatera.edArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePrivada;
end;

procedure TfrPixCDMatera.edContaCriarCEPChange(Sender: TObject);
begin
  if (Length(edContaCriarCEP.Text) > 5) then
  begin
    edContaCriarCEP.Text := FormatarMascaraDinamica(OnlyNumber(edContaCriarCEP.Text), '*****-***');
    edContaCriarCEP.SelStart := Length(edContaCriarCEP.Text);
  end;
end;

procedure TfrPixCDMatera.btAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edArqCertificado.Text;
  if OpenDialog1.Execute then
    edArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificado;
end;

procedure TfrPixCDMatera.btAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePrivada;
end;

procedure TfrPixCDMatera.btChavePIXConsultarClick(Sender: TObject);
var
  wAccountId: String;
begin
  wAccountId := Trim(edChavePIXConsultar.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edChavePIXConsultar.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ChavePIXConsultar(' + wAccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ChavesPIXConsultar(wAccountId);

    mmLogGerencial.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.ChavesPIXResposta.AsJSON));
  except
    On E: Exception do
      mmLogGerencial.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btChavePIXCriarClick(Sender: TObject);
begin
  edChavePIXIncluir.Text := CriarTxId;
end;

procedure TfrPixCDMatera.btChavePIXExcluirClick(Sender: TObject);
var
  wAccountId, wChavePIX: String;
begin
  wAccountId := Trim(edChavePIXExcluirAccountId.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edChavePIXExcluirAccountId.SetFocus;
    Exit;
  end;

  wChavePIX := Trim(edChavePIXExcluir.Text);
  if EstaVazio(wChavePIX) then
  begin
    MessageDlg('Preencha a Chave PIX', mtError, [mbOK], 0);
    edChavePIXExcluir.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ChavePIXExcluir(' +
      wAccountId + ', ' + wChavePIX + ')' + sLineBreak);

    if ACBrPSPMatera1.ChavePIXExcluir(wAccountId, wChavePIX) then
      mmLogGerencial.Lines.Add('  CHAVE PIX EXCLUÍDA COM SUCESSO!');
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add('  ERRO AO EXCLUIR CHAVE PIX!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

procedure TfrPixCDMatera.btChavePIXIncluirClick(Sender: TObject);
var
  wAccountId, wChavePIX: String;
begin
  wAccountId := Trim(edChavePIXIncluirAccountId.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edChavePIXIncluirAccountId.SetFocus;
    Exit;
  end;

  wChavePIX := Trim(edChavePIXIncluir.Text);
  if EstaVazio(wChavePIX) then
  begin
    MessageDlg('Preencha a Chave PIX', mtError, [mbOK], 0);
    edChavePIXIncluir.SetFocus;
    Exit;
  end;

  ACBrPSPMatera1.Clear;

  // Preenchendo dados da Chave PIX
  with ACBrPSPMatera1.ChavePIXSolicitacao do
  begin
    externalIdentifier := wChavePIX;
    alias_.type_ := malEVP;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ChavePIXIncluir(' + wAccountId + ')' + sLineBreak);

    if ACBrPSPMatera1.ChavePIXIncluir(wAccountId) then
      mmLogGerencial.Lines.Add('  CHAVE PIX CADASTRADA COM SUCESSO!' + sLineBreak);

    with ACBrPSPMatera1.ChavePIXResposta do
      mmLogGerencial.Lines.Add(' Resposta:' + sLineBreak + FormatarJson(AsJSON));
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add(E.Message);
      mmLogGerencial.Lines.Add('  ERRO AO CADASTRAR CHAVE PIX!');
      mmLogGerencial.Lines.Add(FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
    end;
  end;
end;

procedure TfrPixCDMatera.btCobCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edCobCopiaECola.Text);
end;

procedure TfrPixCDMatera.btConsultarCobClick(Sender: TObject);
var
  wAccountId, wTransactionID: String;
begin
  wAccountId := Trim(edConsultarCobAccountID.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edConsultarCobAccountID.SetFocus;
    Exit;
  end;

  wTransactionID := Trim(edConsultarCobTransactionID.Text);
  if EstaVazio(wTransactionID) then
  begin
    MessageDlg('Preencha o Transaction Id', mtError, [mbOK], 0);
    edConsultarCobTransactionID.SetFocus;
    Exit;
  end;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarTransacao(' +
      wAccountId + ', ' + wTransactionID + ')' + sLineBreak);

    ACBrPSPMatera1.ConsultarTransacao(wAccountId, wTransactionID);

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.TransacoesResposta.AsJSON));
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btContaConsultarClick(Sender: TObject);
var
  wAccountId: String;
begin
  wAccountId := Trim(edContaConsultarAccountId.Text);
  mmContaConsultarResposta.Lines.Clear;
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edContaConsultarAccountId.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ContaConsultar(' + wAccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ContaConsultar(wAccountId);
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add(E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;

  mmContaConsultarResposta.Lines.Add(' Status da conta: ' +
    MateraAccountStatusToString(ACBrPSPMatera1.ContaResposta.accountStatus) + sLineBreak);
  mmContaConsultarResposta.Lines.Add(' Resposta: ' + sLineBreak + sLineBreak +
    FormatarJson(ACBrPSPMatera1.ContaResposta.AsJSON));
  mmContaConsultarResposta.SelStart := 0;
end;

procedure TfrPixCDMatera.btContaCriarClick(Sender: TObject);
var
  wBase64: String;
  wFs: TFileStream; 
  wFile: AnsiString;
begin
  ACBrPSPMatera1.Clear;

  // Preenchendo dados da Conta
  with ACBrPSPMatera1.ContaSolicitacao do
  begin
    externalIdentifier := edContaCriarExternalID.Text;
    clientType := TMateraClientType(cbContaCriarTipoCliente.ItemIndex);
    accountType := TMateraAccountType(cbCriarContaTipoConta.ItemIndex);

    // Preenchendo dados do cliente da conta
    with client do
    begin
      name := edContaCriarNomeCliente.Text;
      email := edContaCriarEmail.Text;
      taxIdentifier.taxId := edContaCriarCPF_CNPJ.Text;
      taxIdentifier.country := 'BRA';
      mobilePhone.country := 'BRA';
      mobilePhone.phoneNumber := edContaCriarCelular.Text;
    end;

    // Preenchendo dados do endereço da conta
    with billingAddress do
    begin
      logradouro := edContaCriarLogradouro.Text;
      numero := edContaCriarNumero.Text;
      complemento := edContaCriarComplemento.Text;
      bairro := edContaCriarBairro.Text;
      cidade := edContaCriarCidade.Text;
      estado := edContaCriarUF.Text;
      cep := edContaCriarCEP.Text;
      pais := 'BRA';
    end;

    // Conta corporativa? ...Preenche dados da empresa
    if (clientType = mctCorporate) then
    with additionalDetailsCorporate do
    begin
      establishmentDate := edContaCriarFundacao.Date;
      companyName := edContaCriarNomeEmpresa.Text;
      businessLine := 47;
      establishmentForm := '1';

      // Preenchendo o representando da empresa
      with representatives.New do
      begin
        name := edContaCriarRepresentanteNome.Text;
        mother := edContaCriarRepresentanteMae.Text;
        birthDate := edContaCriarNascimento.Date;
        email := edContaCriarRepresentanteEmail.Text;
        taxIdentifier.taxId := edContaCriarRepresentanteCPF.Text;
        taxIdentifier.country := 'BRA';
        mobilePhone.country := 'BRA';
        mobilePhone.phoneNumber := edContaCriarRepresentanteCelular.Text;

        with mailAddress do
        begin
          cep := edContaCriarRepresentanteCEP.Text;
          logradouro := edContaCriarRepresentanteLogradouro.Text;
          numero := edContaCriarRepresentanteNumero.Text;
          bairro := edContaCriarRepresentanteBairro.Text;
          cidade := edContaCriarRepresentanteCidade.Text;
          estado := edContaCriarRepresentanteUF.Text;
          pais := 'BRA';
        end;

        // Preenchendo a foto do representante
        if NaoEstaVazio(edContaCriarRepresentanteFoto.Text) then
        with documents.New do
        begin
          // Lê conteúdo do arquivo e converte para Base64
          wFs := TFileStream.Create(edContaCriarRepresentanteFoto.Text, fmOpenRead or fmShareDenyWrite);
          try
            wFs.Position := 0;
            wFile := ReadStrFromStream(wFs, wFs.Size);
            wBase64 := EncodeBase64(wFile);
          finally
            wFs.Free;
          end;

          type_ := mdtPicture;
          content := wBase64;
        end;

        // Preenchendo foto do RG(Frente) do representante
        if NaoEstaVazio(edContaCriarRepresentanteRGFotoFrente.Text) then
        with documents.New do
        begin
          // Lê conteúdo do arquivo e converte para Base64
          wFs := TFileStream.Create(edContaCriarRepresentanteRGFotoFrente.Text, fmOpenRead or fmShareDenyWrite);
          try
            wFs.Position := 0;
            wFile := ReadStrFromStream(wFs, wFs.Size);
            wBase64 := EncodeBase64(wFile);
          finally
            wFs.Free;
          end;

          type_ := mdtIdentityFront;
          content := wBase64;
        end;

        // Preenchendo os documentos do representante
        if NaoEstaVazio(edContaCriarRepresentanteRGFotoVerso.Text) then
        with documents.New do
        begin
          // Lê conteúdo do arquivo e converte para Base64
          wFs := TFileStream.Create(edContaCriarRepresentanteRGFotoVerso.Text, fmOpenRead or fmShareDenyWrite);
          try
            wFs.Position := 0;
            wFile := ReadStrFromStream(wFs, wFs.Size);
            wBase64 := EncodeBase64(wFile);
          finally
            wFs.Free;
          end;

          type_ := mdtIdentityBack;
          content := wBase64;
        end;
      end;
    end;
  end;

  try  
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ContaIncluir' + sLineBreak);

    if ACBrPSPMatera1.ContaIncluir then
      mmLogGerencial.Lines.Add('  CONTA CRIADA COM SUCESSO!' + sLineBreak);
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add(E.Message);
      mmLogGerencial.Lines.Add(' ERRO:');
      mmLogGerencial.Lines.Add(FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;

  with ACBrPSPMatera1.ContaResposta do
    mmLogGerencial.Lines.Add(' Resposta:' + sLineBreak + FormatarJson(ACBrPSPMatera1.ContaResposta.AsJSON));
end;

procedure TfrPixCDMatera.btContaCriarLimparDadosClick(Sender: TObject);
begin
  edContaCriarExternalID.Text := EmptyStr;
  cbCriarContaTipoConta.ItemIndex := 3;
  cbContaCriarTipoCliente.ItemIndex := 1;
  cbContaCriarTipoClienteChange(Nil);
  edContaCriarNomeCliente.Text := EmptyStr;
  edContaCriarCPF_CNPJ.Text := EmptyStr;
  edContaCriarCelular.Text := EmptyStr;
  edContaCriarEmail.Text := EmptyStr;
  edContaCriarCEP.Text := EmptyStr;
  edContaCriarLogradouro.Text := EmptyStr;
  edContaCriarNumero.Text := EmptyStr;
  edContaCriarComplemento.Text := EmptyStr;
  edContaCriarBairro.Text := EmptyStr;
  edContaCriarCidade.Text := EmptyStr;
  edContaCriarUF.Text := EmptyStr;

  edContaCriarFundacao.Date := 0;
  edContaCriarNomeEmpresa.Text := EmptyStr;
  edContaCriarNascimento.Date := 0;
  edContaCriarRepresentanteNome.Text := EmptyStr;
  edContaCriarRepresentanteMae.Text := EmptyStr;
  edContaCriarRepresentanteCPF.Text := EmptyStr;
  edContaCriarRepresentanteEmail.Text := EmptyStr;
  edContaCriarRepresentanteCelular.Text := EmptyStr;
  edContaCriarRepresentanteCEP.Text := EmptyStr;
  edContaCriarRepresentanteLogradouro.Text := EmptyStr;
  edContaCriarRepresentanteNumero.Text := EmptyStr;
  edContaCriarRepresentanteBairro.Text := EmptyStr;
  edContaCriarRepresentanteCidade.Text := EmptyStr;
  edContaCriarRepresentanteUF.Text := EmptyStr;
  edContaCriarRepresentanteFoto.Text := EmptyStr;
  edContaCriarRepresentanteRGFotoFrente.Text := EmptyStr;
  edContaCriarRepresentanteRGFotoVerso.Text := EmptyStr;
end;

procedure TfrPixCDMatera.btContaCriarPreencherDadosClick(Sender: TObject);
begin
  edContaCriarExternalID.Text := CriarTxId;
  cbContaCriarTipoCliente.ItemIndex := 1;
  cbCriarContaTipoConta.ItemIndex := 3;
  cbContaCriarTipoClienteChange(Nil);
  edContaCriarNomeCliente.Text := 'Pessoa Jurídica';
  edContaCriarCPF_CNPJ.Text := '81667817000110';
  edContaCriarCelular.Text := '12922223893';
  edContaCriarEmail.Text := 'pessoajuridica@mp.com.br';
  edContaCriarCEP.Text := '13720000';
  edContaCriarLogradouro.Text := 'Rua Sacramento';
  edContaCriarNumero.Text := '15';
  edContaCriarComplemento.Text := 'Casa';
  edContaCriarBairro.Text := 'Centro';
  edContaCriarCidade.Text := 'São Paulo';
  edContaCriarUF.Text := 'SP';
  
  edContaCriarFundacao.Date := EncodeDate(1990, 5, 29);
  edContaCriarNomeEmpresa.Text := 'Nome da Empresa';
  edContaCriarNascimento.Date := EncodeDate(1990, 5, 28);
  edContaCriarRepresentanteNome.Text := 'Representante 1';
  edContaCriarRepresentanteMae.Text := 'Mãe do Representante';
  edContaCriarRepresentanteCPF.Text := '13585366864';
  edContaCriarRepresentanteEmail.Text := 'representante.pj@mp.com.br';
  edContaCriarRepresentanteCelular.Text := '12922223893';
  edContaCriarRepresentanteCEP.Text := '01309030';
  edContaCriarRepresentanteLogradouro.Text := 'Rua Fernando de Albuquerque';
  edContaCriarRepresentanteNumero.Text := '88';
  edContaCriarRepresentanteBairro.Text := 'Consolação';
  edContaCriarRepresentanteCidade.Text := 'São Paulo';
  edContaCriarRepresentanteUF.Text := 'SP';
  edContaCriarRepresentanteFoto.Text := 'foto.png';
  edContaCriarRepresentanteRGFotoFrente.Text := 'fotorgfrente.png';
  edContaCriarRepresentanteRGFotoVerso.Text := 'fotorgverso.png';
end;

procedure TfrPixCDMatera.btContaInativarClick(Sender: TObject);
var
  wAccountId: String;
begin
  wAccountId := Trim(edContaInativarAccountId.Text);

  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edContaInativarAccountId.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ContaInativar(' + wAccountId + ')' + sLineBreak);

    if ACBrPSPMatera1.ContaInativar(wAccountId) then
      mmLogGerencial.Lines.Add('  CONTA INATIVADA COM SUCESSO!');
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add('  ERRO AO INATIVAR CONTA!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

procedure TfrPixCDMatera.btCriarCobrancaClick(Sender: TObject);
var
  wQRCode, wAccountId, wChavePIX : String;
  wValor: Double;
begin
  ACBrPSPMatera1.Clear;

  if EstaVazio(edQRCodeAccountId.Text) then
    edQRCodeAccountId.Text := edAccountId.Text;

  wAccountId := Trim(edQRCodeAccountId.Text);
  if EstaVazio(wAccountId) then
  begin

    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edQRCodeAccountId.SetFocus;
    Exit;
  end;

  wChavePIX := Trim(edQRCodeChavePIX.Text);
  if EstaVazio(wChavePIX) then
  begin
    MessageDlg('Preencha a Chave PIX', mtError, [mbOK], 0);
    edQRCodeChavePIX.SetFocus;
    Exit;
  end;

  wValor := StrToFloatDef(edQRCodeValor.Text, 1);

  // Preenchendo dados do QRcode

  with ACBrPSPMatera1.QRCodeSolicitacao do
  begin
    externalIdentifier := edQRCodeExternalID.Text;
    totalAmount := RoundABNT(wValor, -2);
    currency:= 'BRL';

    paymentInfo.transactionType := 'InstantPayment';
    with paymentInfo.instantPayment do
    begin
      alias_.name := wChavePIX;
      alias_.type_ := malEVP;

      // Cobrança Normal
      if (cbQRCodeTipoCobranca.ItemIndex = 0) then
        expiration := StrToInt(seCobrancaExpiracao.Text)
      else
      // Cobrança com Vencimento
      begin
        expiration := 0;
        dynamicQRCodeType := mqtBillingDueDate;

        with billingDueDate do
        begin
          dueDate := edQRCodedueDate.DateTime;

          with payerInformation do
          begin
            cpfCnpj := edQRCodePayercpfcnpj.Text;
            name := edQRCodePayerName.Text;
            addressing.street := edQRCodepayerstreet.Text;
            addressing.city := edQRCodepayercity.Text;
            addressing.uf := edQRCodepayeruf.Text;
            addressing.cep := edQRCodepayerCEP.Text;
          end;

          interests.valuePerc := StrToCurr(edQRCodeinterestsvaluePerc.Text);
          interests.modality := StrToInt(edQRCodeinterestsmodality.Text);

          fines.valuePerc := StrToCurr(edQRCodefinesvaluePerc.Text);
          fines.modality := StrToInt(edQRCodefinesmodality.Text);

          reduction.valuePerc := StrToCurr(edQRCodereductionvaluePerc.Text);
          reduction.modality := StrToInt(edQRCodereductionmodality.Text);

          discounts.fixedDateDiscountList.modality := StrToInt(edQRCodediscountsmodality.Text);
          discounts.fixedDateDiscountList.fixedDateDiscounts.clear;

          with discounts.fixedDateDiscountList.fixedDateDiscounts.New do
          begin
            valuePerc := StrToCurr(edQRCodediscountsvaluePerc.Text);
            date := edQRCodediscountsdate.DateTime;
          end;

          daysAfterDueDate := 10;
        end;
      end;

      with qrCodeImageGenerationSpecification do
      begin
        errorCorrectionLevel:='M';
        imageWidth:=400;
        generateImageRendering:=True;
      end;

      with additionalInformation.New do
      begin
        name:= edQRCodeADname.Text;
        content:= edQRCodeADcontent.Text;
        showToPlayer:= chkQRCodeADshowToPayer.Checked;
      end;
    end;

    recipients.clear;
    with recipients.New do
    begin
      account.accountID:=wAccountId;
      amount := RoundABNT(wValor, -2);
      currency:='BRL';
      mediatorfee := 2;
      recipientComment := edQRCoderecipientComment.Text;
    end;
    callbackAddress := edQRCodeCallBack.Text;
  end;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.GerarQRCode(' + wAccountId + ')' + sLineBreak);
               
    pnQRCodeResult.Visible := ACBrPSPMatera1.GerarQRCode;

    wQRCode := Trim(ACBrPSPMatera1.QRCodeResposta.instantPayment.textContent);
    PintarQRCode(wQRCode, imCobQRCode.Picture.Bitmap, qrUTF8BOM);
    edCobCopiaECola.Text := wQRCode;

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.QRCodeResposta.AsJSON));
  except
    On E: Exception do
    begin
      mmLogOperacoes.Lines.Add('  ERRO AO GERAR QRCODE!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

procedure TfrPixCDMatera.btFluxoPagarClick(Sender: TObject);
var
  wQRCode, wAccountId, wChavePIX : String;
  wValor: Double;
begin
  ACBrPSPMatera1.Clear;
  wAccountId := Trim(edAccountId.Text);
  wChavePIX := Trim(edChavePIX.Text);

  wValor := StringToFloatDef(edValor.Text, 0);

  // Preenchendo dados do QRcode
  with ACBrPSPMatera1.QRCodeSolicitacao do
  begin
    externalIdentifier := CriarTxId;
    totalAmount := RoundABNT(wValor, -2);
    currency:= 'BRL';

    with paymentInfo do
    begin
      transactionType := 'InstantPayment';

      with instantPayment do
      begin
        alias_.name := wChavePIX;
        alias_.type_ := malEVP;
        expiration := 600;

        with qrCodeImageGenerationSpecification do
        begin
          errorCorrectionLevel:='M';
          imageWidth:=400;
          generateImageRendering:=False;
        end;

        with additionalInformation.New do
        begin
          name:= edFluxoClienteDoc1.Text;
          content:= edFluxoClienteNome1.Text;
          showToPlayer:= True;
        end;
      end;

      recipients.clear;
      with recipients.New do
      begin
        account.accountID:=wAccountId;
        amount := RoundABNT(wValor, -2);
        currency:='BRL';
        mediatorfee := 2;
        recipientComment := EmptyStr;
      end;
    end;
  end;

  try
    ACBrPSPMatera1.GerarQRCode;
    wQRCode := Trim(ACBrPSPMatera1.QRCodeResposta.instantPayment.textContent);
    PintarQRCode(wQRCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
    edFluxoCopiaECola.Text := wQRCode;
    edFluxoCopiaECola.Visible := True;
  except
    On E: Exception do
    begin
      mmLogOperacoes.Lines.Add('  ERRO AO GERAR QRCODE!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

procedure TfrPixCDMatera.btLogGerencialLimparClick(Sender: TObject);
begin
  mmLogGerencial.Clear;
end;

procedure TfrPixCDMatera.btLogOperacoesLimparClick(Sender: TObject);
begin
  mmLogOperacoes.Clear;
end;

procedure TfrPixCDMatera.btProxyVerSenhaClick(Sender: TObject);
begin 
  {$IfDef FPC}
  if btProxyVerSenha.Down then
    edProxySenha.EchoMode := emNormal
  else
    edProxySenha.EchoMode := emPassword;
  {$Else}
  if btProxyVerSenha.Down then
    edProxySenha.PasswordChar := #0
  else
    edProxySenha.PasswordChar := '*';
  {$EndIf}
end;

procedure TfrPixCDMatera.btQRCodeCriarLimparDadosClick(Sender: TObject);
begin
  edQRCodeAccountId.Text := EmptyStr;
  edQRCodeChavePIX.Text := EmptyStr;
  edQRCodeValor.Text := EmptyStr;
  edQRCodeADname.Text := EmptyStr;
  edQRCodeADcontent.Text := EmptyStr;
  chkQRCodeADshowToPayer.Checked := False;
  edQRCodeCallBack.Text := EmptyStr;
  edQRCoderecipientComment.Text := EmptyStr;
  cbQRCodeTipoCobranca.ItemIndex := 0;
  edQRCodedueDate.DateTime := Now;
  edQRCodeinterestsvaluePerc.Text := EmptyStr;
  edQRCodeinterestsmodality.Text := EmptyStr;
  edQRCodefinesvaluePerc.Text := EmptyStr;
  edQRCodefinesmodality.Text := EmptyStr;
  edQRCodereductionvaluePerc.Text := EmptyStr;
  edQRCodereductionmodality.Text := EmptyStr;
  edQRCodediscountsvaluePerc.Text := EmptyStr;
  edQRCodediscountsmodality.Text := EmptyStr;
  edQRCodediscountsdate.DateTime := Now;
  edQRCodePayercpfcnpj.Text := EmptyStr;
  edQRCodePayerName.Text := EmptyStr;
  edQRCodepayerCEP.Text := EmptyStr;
  edQRCodepayercity.Text := EmptyStr;
  edQRCodepayeruf.Text := EmptyStr;
  edQRCodepayerstreet.Text := EmptyStr;
end;

procedure TfrPixCDMatera.btQRCodeCriarPreencherDadosClick(Sender: TObject);
begin
  edQRCodeExternalID.Text := CriarTxId;
  edQRCodeChavePIX.Text := edChavePIX.Text;
  edQRCodeAccountId.Text := edAccountId.Text;
  edQRCodeValor.Text := '5,00';
  edQRCodeADname.Text := 'ID DO PEDIDO';
  edQRCodeADcontent.Text := '123456';
  chkQRCodeADshowToPayer.Checked := True;
  edQRCodeCallBack.Text := 'https://testemockqr.requestcatcher.com/';
  edQRCoderecipientComment.Text := 'Comentario 123';

  // cobrança
  edQRCodedueDate.DateTime := Now + 30;
  edQRCodeinterestsvaluePerc.Text := '1';
  edQRCodeinterestsmodality.Text := '1';
  edQRCodefinesvaluePerc.Text := '2';
  edQRCodefinesmodality.Text := '1';
  edQRCodereductionvaluePerc.Text := '1';
  edQRCodereductionmodality.Text := '1';
  edQRCodediscountsvaluePerc.Text := '1';
  edQRCodediscountsmodality.Text := '1';
  edQRCodediscountsdate.DateTime := Now + 25;
  edQRCodePayercpfcnpj.Text := '00971484074';
  edQRCodePayerName.Text := 'Nome do Pagador';
  edQRCodepayerCEP.Text := '13010210';
  edQRCodepayercity.Text := 'Mococa';
  edQRCodepayeruf.Text := 'SP';
  edQRCodepayerstreet.Text := 'Rua Sacramento';

end;

procedure TfrPixCDMatera.btQRCodeGerarExternalIDClick(Sender: TObject);
begin
  edQRCodeExternalID.Text := CriarTxId;
end;

procedure TfrPixCDMatera.LerConfiguracao;
var
  wIni: TIniFile;
begin
  AdicionarLinhaLog('- LerConfiguracao: '+ NomeArquivoConfig);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    cbAmbiente.ItemIndex := wIni.ReadInteger('PIX','Ambiente', 0);
    edTimeout.Value := wIni.ReadInteger('PIX', 'TimeOut', ChttpTimeOutDef);

    seCobrancaExpiracao.Value := wIni.ReadInteger('Cobranca', 'Expiracao', seCobrancaExpiracao.Value);

    edProxyHost.Text := wIni.ReadString('Proxy', 'Host', '');
    edProxyPorta.Text := wIni.ReadString('Proxy', 'Porta', '');
    edProxyUsuario.Text := wIni.ReadString('Proxy', 'User', '');
    edProxySenha.Text := StrCrypt(DecodeBase64(wIni.ReadString('Proxy', 'Pass', '')), CURL_ACBR);

    edLogArquivo.Text := wIni.ReadString('Log', 'Arquivo', '');
    cbLogNivel.ItemIndex := wIni.ReadInteger('Log', 'Nivel', 1);

    edPSPClientID.Text := wIni.ReadString('Matera', 'ClientID', '');
    edPSPClientSecret.Text := wIni.ReadString('Matera', 'ClientSecret', '');
    edPSPSecretKey.Text := wIni.ReadString('Matera', 'SecretKey', '');
    edArqCertificado.Text := wIni.ReadString('Matera', 'ArqCertificado', '');
    edArqChavePrivada.Text := wIni.ReadString('Matera', 'ArqChavePrivada', '');
    edAccountId.Text:= wIni.ReadString('Matera', 'accountId', '');
    edChavePIX.Text := wIni.ReadString('Matera', 'chavePIX', '');
  finally
    wIni.Free;
  end;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPixCDMatera.GravarConfiguracao;
var
  wIni: TIniFile;
begin
  AdicionarLinhaLog('- LerConfiguracao: ' + NomeArquivoConfig);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    wIni.WriteInteger('PIX','Ambiente', cbAmbiente.ItemIndex);
    wIni.WriteInteger('PIX', 'TimeOut', edTimeout.Value);

    wIni.WriteInteger('Cobranca', 'Expiracao', seCobrancaExpiracao.Value);

    wIni.WriteString('Proxy', 'Host', edProxyHost.Text);
    wIni.WriteString('Proxy', 'Porta', edProxyPorta.Text);
    wIni.WriteString('Proxy', 'User', edProxyUsuario.Text);
    wIni.WriteString('Proxy', 'Pass', EncodeBase64(StrCrypt(edProxySenha.Text, CURL_ACBR)));

    wIni.WriteString('Log', 'Arquivo', edLogArquivo.Text);
    wIni.WriteInteger('Log', 'Nivel', cbLogNivel.ItemIndex);

    wIni.WriteString('Matera', 'ClientID', edPSPClientID.Text);
    wIni.WriteString('Matera', 'SecretKey', edPSPSecretKey.Text);
    wIni.WriteString('Matera', 'ArqCertificado', edArqCertificado.Text);
    wIni.WriteString('Matera', 'ArqChavePrivada', edArqChavePrivada.Text);

    wIni.WriteString('Matera', 'accountID', edAccountId.Text);
    wIni.WriteString('Matera', 'ChavePIX', edChavePIX.Text);

  finally
     wIni.Free;
  end;

  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPixCDMatera.AplicarConfiguracao;
begin
  AdicionarLinhaLog('- AplicarConfiguracao');
  ACBrPixCD1.Ambiente := TACBrPixCDAmbiente(cbAmbiente.ItemIndex);
  ACBrPixCD1.TimeOut := edTimeout.Value;

  ACBrPixCD1.Proxy.Host := edProxyHost.Text;
  ACBrPixCD1.Proxy.Port := edProxyPorta.Text;
  ACBrPixCD1.Proxy.User := edProxyUsuario.Text;
  ACBrPixCD1.Proxy.Pass := edProxySenha.Text;

  ACBrPixCD1.ArqLOG := edLogArquivo.Text;
  ACBrPixCD1.NivelLog := cbLogNivel.ItemIndex;

  ACBrPSPMatera1.ClientID := edPSPClientID.Text;
  ACBrPSPMatera1.ClientSecret := edPSPClientSecret.Text;
  ACBrPSPMatera1.SecretKey := edPSPSecretKey.Text;
  ACBrPSPMatera1.ArquivoCertificado := edArqCertificado.Text;
  ACBrPSPMatera1.ArquivoChavePrivada := edArqChavePrivada.Text;

end;

procedure TfrPixCDMatera.AdicionarLinhaLog(aMsg: String);
begin
  if Assigned(mmLogGerencial) then
    mmLogGerencial.Lines.Add(aMsg);
  if Assigned(mmLogOperacoes) then
    mmLogOperacoes.Lines.Add(aMsg);
end;

procedure TfrPixCDMatera.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');

  if (pgPrincipal.ActivePage <> tsTestes) then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TfrPixCDMatera.LigarAlertasdeErrosDeConfiguracao;
begin
  {edtRecebedorNomeChange(Nil);
  edtRecebedorCEPChange(Nil);
  cbxPSPAtualChange(Nil);
  mQREChange(Nil);
  cbxAmbienteChange(Nil)

  edtItauChavePIXChange(Nil);
  edtItauClientIDChange(Nil);
  edtItauClientSecretChange(Nil);}
end;

procedure TfrPixCDMatera.InicializarComponentesDefault;
var
  i: TACBrPixCDAmbiente;
  j: TMateraClientType;
  k: TMateraAccountType;
begin
  ACBrPixCD1.PSP := ACBrPSPMatera1;
  lbErroCertificado.Caption := EmptyStr;
  lbErroChavePrivada.Caption := EmptyStr;
  pnContaCriarPerson.Parent := gbDadosAdicionais;
  pnContaCriarCorporate.Parent := gbDadosAdicionais;

  cbAmbiente.Items.Clear;
  for i := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
     cbAmbiente.Items.Add(GetEnumName(TypeInfo(TACBrPixCDAmbiente), Integer(i)));

  cbContaCriarTipoCliente.Items.Clear;
  for j := Low(TMateraClientType) to High(TMateraClientType) do
     cbContaCriarTipoCliente.Items.Add(GetEnumName(TypeInfo(TMateraClientType), Integer(j)));
  cbContaCriarTipoCliente.ItemIndex := 1;
  cbContaCriarTipoClienteChange(Nil);

  cbCriarContaTipoConta.Items.Clear;
  for k := Low(TMateraAccountType) to High(TMateraAccountType) do
     cbCriarContaTipoConta.Items.Add(GetEnumName(TypeInfo(TMateraAccountType), Integer(k)));
end;

procedure TfrPixCDMatera.ValidarChavePrivada;
var
  a, e: String;
begin
  if EstaVazio(edArqChavePrivada.Text) then
    Exit;

  a := AdicionarPathAplicacao(edArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo não especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbErroChavePrivada.Caption := e;
  imErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TfrPixCDMatera.ValidarCertificado;
var
  a, e: String;
begin
  if EstaVazio(edArqCertificado.Text) then
    Exit;

  a := AdicionarPathAplicacao(edArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo não especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbErroCertificado.Caption := e;
  imErroCertificado.Visible := (e <> 'OK');
end;

end.

