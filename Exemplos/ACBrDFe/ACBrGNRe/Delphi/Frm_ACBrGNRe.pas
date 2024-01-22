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

unit Frm_ACBrGNRe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrDFeReport, ACBrBase, ACBrDFe, ACBrMail,
  ACBrGNREGuiaClass, ACBrGNReGuiaRLClass, ACBrGNRE2;

type
  TfrmACBrGNRe = class(TForm)
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
    sbPathGNRE: TSpeedButton;
    Label35: TLabel;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathGNRE: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathGNRE: TEdit;
    cbxSepararPorModelo: TCheckBox;
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
    btnEnviarEmail: TButton;
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
    ACBrGNRE1: TACBrGNRE;
    ACBrGNREGuiaRL1: TACBrGNREGuiaRL;
    btnGerarMDFe: TButton;
    btnCriarEnviar: TButton;
    btnImprimirTXT: TButton;
    btnGerarPDF: TButton;
    btnConsultaConfigUF: TButton;
    btnConsultarRecibo: TButton;
    btnImprimirXML: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathGNREClick(Sender: TObject);
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
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure ACBrGNRE1GerarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure ACBrGNRE1StatusChange(Sender: TObject);
    procedure btnGerarMDFeClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnImprimirTXTClick(Sender: TObject);
    procedure btnGerarPDFClick(Sender: TObject);
    procedure btnConsultaConfigUFClick(Sender: TObject);
    procedure btnConsultarReciboClick(Sender: TObject);
    procedure btnImprimirXMLClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    Procedure AlimentarComponente;
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmACBrGNRe: TfrmACBrGNRe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  pcnAuxiliar, pcnConversao, pgnreConversao, ACBrDFeSSL, ACBrDFeOpenSSL,
  ACBrUtil.DateTime, ACBrUtil.FilesIO, ACBrUtil.Base, ACBrUtil.XMLHTML,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrGNRe }

procedure TfrmACBrGNRe.AlimentarComponente;
begin
  ACBrGNRE1.Guias.Clear;

   {
  with ACBrGNRE1.Guias.Add.GNRE do
  begin
    c01_UfFavorecida := 'PR';
    c02_receita := 100099;
    c28_tipoDocOrigem := 10;
    c04_docOrigem := '999999';
    c06_valorPrincipal := 100.50;
    c14_dataVencimento := Now;
    c15_convenio := '16461313';
    c17_inscricaoEstadualEmitente := '9023725557';
    c33_dataPagamento := Now;
  end;

  with ACBrGNRE1.Guias.Add.GNRE do
  begin
    c01_UfFavorecida := 'PR';
    c02_receita := 100099;
    c28_tipoDocOrigem := 10;
    c04_docOrigem := '888888';
    c06_valorPrincipal := 200.33;
    c14_dataVencimento := Now;
    c15_convenio := '16461313';
    c17_inscricaoEstadualEmitente := '9023725557';
    c33_dataPagamento := Now;
  end;
  }

  with ACBrGNRE1.Guias.Add.GNRE do
  begin
    c01_UfFavorecida := 'PR';
    tipoGNRE := tgSimples;
    c02_receita := 100099;
    c03_idContribuinteEmitente := edtEmitCNPJ.Text;
    c04_docOrigem := '777777';
    c06_valorPrincipal := 120.50;
    c10_valorTotal := 120.50;
    c14_dataVencimento := Now;
    c15_convenio := '16461313';

    c16_razaoSocialEmitente := edtEmitRazao.Text;
    c17_inscricaoEstadualEmitente := '9023725557';
    c18_enderecoEmitente := edtEmitLogradouro.Text + ', ' + edtEmitNumero.Text;
    c19_municipioEmitente := Copy(edtEmitCodCidade.Text, 3, 5);
    c20_ufEnderecoEmitente := edtEmitUF.Text;
    c21_cepEmitente := edtEmitCEP.Text;
    c22_telefoneEmitente := edtEmitFone.Text;
    c27_tipoIdentificacaoEmitente := 1;
    c28_tipoDocOrigem := 10;
    c26_produto := 20;

    c33_dataPagamento := Now;

    c34_tipoIdentificacaoDestinatario := 1;
    c35_idContribuinteDestinatario := '12345678000123';
    c36_inscricaoEstadualDestinatario := '12345678';
    c37_razaoSocialDestinatario := 'Nome do Destinatario';
    c38_municipioDestinatario := 'Municipio do Destinatario';

    referencia.periodo := 1;
    referencia.mes := '04';
    referencia.ano := 2019;
    referencia.parcela := 1;

    with camposExtras.New do
    begin
      CampoExtra.codigo := 123;
      CampoExtra.tipo := '1';
      CampoExtra.valor := '50.00';
    end;

    c42_identificadorGuia := '001';
  end;
end;

procedure TfrmACBrGNRe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrGNRE1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrGNRE1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrGNRE1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrGNRE1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrGNRE1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrGNRe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrGNRE1.SSL.CertCNPJ);
end;

procedure TfrmACBrGNRe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrGNRE1.SSL.CertDataVenc));
end;

procedure TfrmACBrGNRe.btnEnviarEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione a GNRE';
  OpenDialog1.DefaultExt := '*-gnre.XML';
  OpenDialog1.Filter := 'Arquivos GNRE (*-gnre.XML)|*-gnre.XML|Arquivos XML (*.XML)|*.XML|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ACBrGNRE1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrGNRE1.Guias.Clear;
  ACBrGNRE1.Guias.LoadFromFile(OpenDialog1.FileName);
  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); // especifique um email valido
    //CC.Add('email_2@provedor.com.br');    // especifique um email valido
    ConfigurarEmail;
    ACBrGNRE1.Guias.Items[0].EnviarEmail(Para
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

procedure TfrmACBrGNRe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrGNRE1.SSL.UseCertificateHTTP;
  ACBrGNRE1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrGNRE1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrGNRE1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrGNRe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrGNRE1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrGNRE1.SSL.CertCertificadora);
end;

procedure TfrmACBrGNRe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrGNRE1.SSL do
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

procedure TfrmACBrGNRe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrGNRE1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrGNRe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrGNRe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrGNRE1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrGNRe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrGNRE1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrGNRE1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrGNRe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrGNRE1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrGNRe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrGNRE1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrGNRe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrGNRE1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrGNRe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrGNRE1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrGNRe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrGNRE1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrGNRe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoGNRE;
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
  for K := Low(TVersaoGNRE) to High(TVersaoGNRE) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoGNRE), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBrGNRe.GravarConfiguracao;
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
    Ini.WriteBool(  'Arquivos', 'EmissaoPathGNRE',  cbxEmissaoPathGNRE.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathGNRE',         edtPathGNRE.Text);

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

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrGNRe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrGNRe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrGNRe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrGNRe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrGNRe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrGNRe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrGNRe.LerConfiguracao;
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
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoGNRE), integer(cbVersaoDF.ItemIndex) ));

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
    cbxEmissaoPathGNRE.Checked  := Ini.ReadBool(  'Arquivos', 'EmissaoPathGNRE',   false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathGNRE.Text            := Ini.ReadString('Arquivos', 'PathGNRE',          '');

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

    ConfigurarComponente;
    ConfigurarEmail;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrGNRe.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrGNRE1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrGNRE1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrGNRE1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrGNRE1.SSL.DescarregarCertificado;

  with ACBrGNRE1.Configuracoes.Geral do
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
    VersaoDF         := TVersaoGNRE(cbVersaoDF.ItemIndex);
  end;

  with ACBrGNRE1.Configuracoes.WebServices do
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
      edtIntervalo.Text := IntToStr(ACBrGNRE1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrGNRE1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrGNRE1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathGNRE  := cbxEmissaoPathGNRE.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathGNRE         := edtPathGNRE.Text;
    PathMensal       := GetPathGNRE(0);
    PathSalvar       := PathMensal;
  end;
end;

procedure TfrmACBrGNRe.ConfigurarEmail;
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
  ACBrMail1.FromName := 'Projeto ACBr - ACBrGNRE';
end;

procedure TfrmACBrGNRe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');
end;

procedure TfrmACBrGNRe.PathClick(Sender: TObject);
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

procedure TfrmACBrGNRe.sbPathGNREClick(Sender: TObject);
begin
  PathClick(edtPathGNRE);
end;

procedure TfrmACBrGNRe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrGNRe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrGNRE1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrGNRe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  AddRow: Boolean;
begin
  ACBrGNRE1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrGNRE1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrGNRE1.SSL.ListaCertificados[I] do
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

procedure TfrmACBrGNRe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrGNRe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TfrmACBrGNRe.ACBrGNRE1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
  Tratado := True;
end;

procedure TfrmACBrGNRe.ACBrGNRE1StatusChange(Sender: TObject);

  procedure MostrarStatus(const AMsg: string);
  begin
    if AMsg = '' then
    begin
      if ( frmStatus <> nil ) then
        frmStatus.Hide
    end
    else
    begin
      if ( frmStatus = nil ) then
        frmStatus := TfrmStatus.Create(Application);

      frmStatus.lblStatus.Caption := AMsg;
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
  end;

begin
  case ACBrGNRE1.Status of
    stGNREIdle: MostrarStatus('');
    stGNRERecepcao: MostrarStatus('Enviando dados do GNRE...');
    stGNRERetRecepcao: MostrarStatus('Recebendo dados do GNRE...');
    stGNREConsulta: MostrarStatus('Consultando Lote GNRE...');
  end;

  Application.ProcessMessages;
end;

procedure TfrmACBrGNRe.btnGerarMDFeClick(Sender: TObject);
begin
  ACBrGNRE1.Guias.Clear;
  AlimentarComponente;
  ACBrGNRE1.Guias.Items[0].GravarXML;

  ShowMessage('Arquivo gerado em: '+ACBrGNRE1.Guias.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: '+ACBrGNRE1.Guias.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrGNRE1.Guias.Items[0].NomeArq);
  LoadXML(MemoResp.Lines.Text, WBResposta);
  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrGNRe.btnCriarEnviarClick(Sender: TObject);
var
  i: Integer;
begin
  ACBrGNRE1.Guias.Clear;
  AlimentarComponente;
  ACBrGNRE1.Enviar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  LoadXML(MemoResp.Lines.Text, WBResposta);

  pgRespostas.ActivePageIndex := 4;
  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Envio GNRE');
  MemoDados.Lines.Add('ambiente: '  + TpAmbToStr(ACBrGNRE1.WebServices.Retorno.ambiente));
  MemoDados.Lines.Add('codigo: '    + IntToStr(ACBrGNRE1.WebServices.Retorno.codigo));
  MemoDados.Lines.Add('descricao: ' + ACBrGNRE1.WebServices.Retorno.descricao);
  MemoDados.Lines.Add('Recibo: '    + ACBrGNRE1.WebServices.Retorno.numeroRecibo);
  MemoDados.Lines.Add('Protocolo: ' + ACBrGNRE1.WebServices.Retorno.protocolo);
  MemoDados.Lines.Add('');

  for i := 0 to ACBrGNRE1.WebServices.Retorno.GNRERetorno.resGuia.Count -1 do
    MemoDados.Lines.Add('Guia salva em: ' + ACBrGNRE1.WebServices.Retorno.GNRERetorno.resGuia[i].NomeArq);

  // Para versão 1 temos o retorno no formato txt se deseja salvar no banco de dados
  // deve ler a propriedade
  //     guiatxt := ACBrGNRE1.WebServices.Retorno.GNRERetorno.resGuia[i].TXT;

  // Para versão 2 temos o retorno no formato xml se deseja salvar no banco de dados
  // deve ler a propriedade
  //     guiaxml := ACBrGNRE1.WebServices.Retorno.GNRERetorno.resGuia[i].XML;

  // onde i varia de zero até a quantidade -1 de guias retornadas.

  ACBrGNRE1.Guias.Clear;
end;

procedure TfrmACBrGNRe.btnImprimirTXTClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a Guia';
  OpenDialog1.DefaultExt := '*-gnre.txt';
  OpenDialog1.Filter := 'Arquivos GNRE (*-gnre.txt)|*-gnre.txt|Arquivos TXT (*.txt)|*.txt|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrGNRE1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrGNRE1.GuiasRetorno.Clear;
    ACBrGNRE1.GuiasRetorno.LoadFromFile(OpenDialog1.FileName);
    ACBrGNRE1.GuiasRetorno.Imprimir;
  end;
end;

procedure TfrmACBrGNRe.btnImprimirXMLClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a Guia';
  OpenDialog1.DefaultExt := '*-guia.xml';
  OpenDialog1.Filter := 'Arquivos Guia (*-guia.xml)|*-guia.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrGNRE1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrGNRE1.GuiasRetorno.Clear;
    ACBrGNRE1.GuiasRetorno.LoadFromFile(OpenDialog1.FileName);
    ACBrGNRE1.GuiasRetorno.Imprimir;
  end;
end;

procedure TfrmACBrGNRe.btnGerarPDFClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a Guia';
  OpenDialog1.DefaultExt := '*-guia.xml';
  OpenDialog1.Filter := 'Arquivos Guia (*-guia.xml)|*-guia.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrGNRE1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrGNRE1.GuiasRetorno.Clear;
    ACBrGNRE1.GuiasRetorno.LoadFromFile(OpenDialog1.FileName);
    ACBrGNRE1.GuiasRetorno.ImprimirPDF;
  end;
end;

procedure TfrmACBrGNRe.btnConsultaConfigUFClick(Sender: TObject);
var
  aUF, aReceita : String;
begin
  aUF := '';
  if not(InputQuery('Consulta Configuração UF', 'UF', aUF)) then
    exit;

  aReceita := '';
  if not(InputQuery('Consulta Configuração UF', 'Receita', aReceita)) then
    exit;

  ACBrGNRE1.WebServices.ConsultaUF.Uf := aUF;
  ACBrGNRE1.WebServices.ConsultaUF.receita := StrToIntDef(aReceita, 0);
  ACBrGNRE1.WebServices.ConsultaUF.Executar;

  MemoResp.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.ConsultaUF.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.ConsultaUF.RetWS);
  LoadXML(MemoResp.Lines.Text, WBResposta);

  pgRespostas.ActivePageIndex := 4;
  MemoDados.Lines.Add('');
  MemoDados.Lines.Add('Consulta Configuração UF');
  MemoDados.Lines.Add('ambiente: '           + TpAmbToStr(ACBrGNRE1.WebServices.ConsultaUF.ambiente));
  MemoDados.Lines.Add('codigo: '             + IntToStr(ACBrGNRE1.WebServices.ConsultaUF.codigo));
  MemoDados.Lines.Add('descricao: '          + ACBrGNRE1.WebServices.ConsultaUF.descricao);
  MemoDados.Lines.Add('Uf: '                 + ACBrGNRE1.WebServices.ConsultaUF.Uf);
  MemoDados.Lines.Add('exigeUfFavorecida : ' + IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeUfFavorecida = 'S', 'SIM', 'NÃO'));
  MemoDados.Lines.Add('exigeReceita: '       + IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeReceita = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeContribuinteEmitente: '+ IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeContribuinteEmitente = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeDataVencimento: '     + IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeDataVencimento = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeConvenio: '+ IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeConvenio = 'S', 'SIM', 'NÃO'));
//  MemoDados.Lines.Add('exigeDataPagamento: '+ IIF(ACBrGNRE1.WebServices.ConsultaUF.exigeDataPagamento = 'S', 'SIM', 'NÃO'));
end;

procedure TfrmACBrGNRe.btnConsultarReciboClick(Sender: TObject);
var
  aux, BaixarPDF: String;
begin
  aux := '';
  if not(InputQuery('Consultar Recibo Lote', 'Número do Recibo', aux)) then
    exit;

  BaixarPDF := '';
  if not(InputQuery('Consultar Recibo Lote', 'Baixar PDF da Guia (digite S para Sim)', BaixarPDF)) then
    exit;

  ACBrGNRE1.WebServices.Retorno.numeroRecibo := aux;
  ACBrGNRE1.WebServices.Retorno.IncluirPDFGuias := (UpperCase(BaixarPDF) = 'S');
  ACBrGNRE1.WebServices.Retorno.Executar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrGNRE1.WebServices.Retorno.RetWS);
  LoadXML(MemoResp.Lines.Text, WBResposta);
end;

end.
