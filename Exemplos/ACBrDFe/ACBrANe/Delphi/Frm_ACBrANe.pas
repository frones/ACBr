unit Frm_ACBrANe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrBase, ACBrUtil, ACBrDFe,
  ACBrMail, ACBrANe;

type
  TfrmACBrANe = class(TForm)
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
    sbPathANe: TSpeedButton;
    Label35: TLabel;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathANe: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathANe: TEdit;
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
    ACBrANe1: TACBrANe;
    rgAverbar: TRadioGroup;
    Label30: TLabel;
    Label7: TLabel;
    edtUsuarioATM: TEdit;
    edtSenhaATM: TEdit;
    Label33: TLabel;
    edtCodATM: TEdit;
    Label34: TLabel;
    cbSeguradora: TComboBox;
    btnGerarANe: TButton;
    btnCriarEnviar: TButton;
    btnEnviarANeEmail: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathANeClick(Sender: TObject);
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
    procedure btnGerarANeClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnEnviarANeEmailClick(Sender: TObject);
    procedure ACBrANe1GerarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure ACBrANe1StatusChange(Sender: TObject);
  private
    { Private declarations }
    DocNFeCTe: String;

    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    Procedure AlimentarComponente(ANomeArq: String);
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmACBrANe: TfrmACBrANe;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  ACBrDFeConfiguracoes, ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  pcnAuxiliar, pcnConversao, pcaConversao,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrMDFe }

procedure TfrmACBrANe.AlimentarComponente(ANomeArq: String);
begin
  ACBrANe1.Documentos.Clear;

  with ACBrANe1.Documentos.Add.ANe do
  begin
    // ATM
    Usuario := ACBrANe1.Configuracoes.Geral.Usuario;
    Senha   := ACBrANe1.Configuracoes.Geral.Senha;
    codatm  := ACBrANe1.Configuracoes.Geral.CodATM;

    // ELT
    NomeArq := ExtractFileName(ANomeArq);
    CNPJ    := ACBrANe1.Configuracoes.Geral.CNPJEmitente;

    // ATM e ELT
    xmlDFe  := DocNFeCTe;
  end;
end;

procedure TfrmACBrANe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrANe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrANe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrANe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrANe1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrANe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrANe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrANe1.SSL.CertCNPJ);
end;

procedure TfrmACBrANe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrANe1.SSL.CertDataVenc));
end;

procedure TfrmACBrANe.btnHTTPSClick(Sender: TObject);
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

  OldUseCert := ACBrANe1.SSL.UseCertificateHTTP;
  ACBrANe1.SSL.UseCertificateHTTP := False;

  try
    MemoResp.Lines.Text := ACBrANe1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrANe1.SSL.UseCertificateHTTP := OldUseCert;
  end;

  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrANe.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage(ACBrANe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
             'Certificadora: ' + ACBrANe1.SSL.CertCertificadora);
end;

procedure TfrmACBrANe.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrANe1.SSL do
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

procedure TfrmACBrANe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrANe1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrANe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrANe.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrANe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrANe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrANe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrANe1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrANe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrANe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrANe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrANe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrANe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrANe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrANe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrANe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrANe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrANe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrANe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoANe;
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
  for K := Low(TVersaoANe) to High(TVersaoANe) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoANe), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 2;
end;

procedure TfrmACBrANe.GravarConfiguracao;
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
    Ini.WriteString( 'Geral', 'UsuarioATM',       edtUsuarioATM.Text);
    Ini.WriteString( 'Geral', 'SenhaATM',         edtSenhaATM.Text);
    Ini.WriteString( 'Geral', 'CodigoATM',        edtCodATM.Text);
    Ini.WriteInteger('Geral', 'Averbar',          rgAverbar.ItemIndex);
    Ini.WriteString( 'Geral', 'Seguradora',       cbSeguradora.Text);

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
    Ini.WriteBool(  'Arquivos', 'EmissaoPathANe',   cbxEmissaoPathANe.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathANe',          edtPathANe.Text);

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
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrANe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrANe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrANe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrANe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrANe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrANe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrANe.LerConfiguracao;
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
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoANe), integer(cbVersaoDF.ItemIndex) ));
    edtUsuarioATM.Text        := Ini.ReadString( 'Geral', 'UsuarioATM',     '');
    edtSenhaATM.Text          := Ini.ReadString( 'Geral', 'SenhaATM',       '');
    edtCodATM.Text            := Ini.ReadString( 'Geral', 'CodigoATM',      '');
    rgAverbar.ItemIndex       := Ini.ReadInteger('Geral', 'Averbar',        1);

    cbSeguradora.ItemIndex    := cbSeguradora.Items.IndexOf(Ini.ReadString('Geral', 'Seguradora', 'ATM'));

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
    cbxEmissaoPathANe.Checked   := Ini.ReadBool(  'Arquivos', 'EmissaoPathANe',   false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathANe.Text             := Ini.ReadString('Arquivos', 'PathANe',          '');

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
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrANe.ConfigurarComponente;
var
  Ok: Boolean;
begin
  ACBrANe1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrANe1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrANe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrANe1.SSL.DescarregarCertificado;

  with ACBrANe1.Configuracoes.Geral do
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
    VersaoDF         := TVersaoANe(cbVersaoDF.ItemIndex);
    Usuario          := edtUsuarioATM.Text;
    Senha            := edtSenhaATM.Text;
    CodATM           := edtCodATM.Text;
    Salvar           := ckSalvar.Checked;
    Seguradora       := TSeguradora(cbSeguradora.ItemIndex);
    CNPJEmitente     := edtEmitCNPJ.Text;

    case rgAverbar.ItemIndex of
      0: TipoDoc := tdNFe;
      1: TipoDoc := tdCTe;
      2: TipoDoc := tdMDFe;
    end;
  end;

  with ACBrANe1.Configuracoes.WebServices do
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
      edtIntervalo.Text := IntToStr(ACBrANe1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrANe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrANe1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathANe   := cbxEmissaoPathANe.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSalvar       := edtPathLogs.Text;
    PathSchemas      := edtPathSchemas.Text;
    PathANe          := edtPathANe.Text;
  end;
end;

procedure TfrmACBrANe.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  ACBrUtil.WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ACBrUtil.ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');
end;

procedure TfrmACBrANe.PathClick(Sender: TObject);
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

procedure TfrmACBrANe.sbPathANeClick(Sender: TObject);
begin
  PathClick(edtPathANe);
end;

procedure TfrmACBrANe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrANe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrANe1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrANe.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
//  ASerie: String;
  AddRow: Boolean;
begin
  ACBrANe1.SSL.LerCertificadosStore;
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

  for I := 0 to ACBrANe1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrANe1.SSL.ListaCertificados[I] do
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

procedure TfrmACBrANe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrANe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

procedure TfrmACBrANe.btnGerarANeClick(Sender: TObject);
var
  vAux : String;
begin
  if not(InputQuery('WebServices Enviar', 'Numero da Averbação', vAux)) then
    exit;

  ACBrANe1.Documentos.Clear;
  AlimentarComponente('');

  ACBrANe1.Documentos.GerarANe;
  ACBrANe1.Documentos.Items[0].GravarXML('', '');

  ShowMessage('Arquivo gerado em: '+ACBrANe1.Documentos.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: '+ACBrANe1.Documentos.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrANe1.Documentos.Items[0].NomeArq);
  LoadXML(MemoResp.Lines.Text, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmACBrANe.btnCriarEnviarClick(Sender: TObject);
var
  Documento: TStringList;
  i: Integer;
begin
  OpenDialog1.Title := 'Selecione o NFe/CTe';
  OpenDialog1.DefaultExt := '*-CTe.xml';
  OpenDialog1.Filter := 'Arquivos ANe (*-CTe.xml)|*-CTe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrANe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    Documento := TStringList.Create;
    Documento.LoadFromFile(OpenDialog1.FileName);

    DocNFeCTe := Documento.Text;

    Documento.Free;
  end;

  ACBrANe1.Documentos.Clear;
  AlimentarComponente(OpenDialog1.FileName);
  ACBrANe1.Enviar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrANe1.WebServices.ANeAverbar.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrANe1.WebServices.ANeAverbar.RetWS);
  LoadXML(MemoResp.Lines.Text, WBResposta);

  pgRespostas.ActivePageIndex := 5;

  case ACBrANe1.Configuracoes.Geral.Seguradora of
    tsELT:
      begin
        MemoDados.Lines.Add('');
        MemoDados.Lines.Add('Retorno da solicitação de Averbação');
        MemoDados.Lines.Add('CNPJ     : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.CNPJ);
        MemoDados.Lines.Add('CTE      : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.CTE);
        MemoDados.Lines.Add('Data/Hora: '+ DateTimeToStr(ACBrANe1.WebServices.ANeAverbar.ANeRetorno.DataHora));
        MemoDados.Lines.Add('Arquivo  : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.fname);
        MemoDados.Lines.Add('Doc      : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Doc);
        MemoDados.Lines.Add('Codigo   : '+ IntToStr(ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Codigo));
        MemoDados.Lines.Add('Resultado: '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Resultado);
        MemoDados.Lines.Add('Protocolo: '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Protocolo);
        MemoDados.Lines.Add('Status   : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.status);
      end
  else
    begin
      MemoDados.Lines.Add('');
      MemoDados.Lines.Add('Retorno da solicitação de Averbação');
      MemoDados.Lines.Add('Numero   : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Numero);
      MemoDados.Lines.Add('Serie    : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Serie);
      MemoDados.Lines.Add('Filial   : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Filial);
      MemoDados.Lines.Add('CNPJ     : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.CNPJCli);
      MemoDados.Lines.Add('Tipo Doc : '+ IntToStr(ACBrANe1.WebServices.ANeAverbar.ANeRetorno.TpDoc));
      MemoDados.Lines.Add('Inf Adic : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.InfAdic);
      if ACBrANe1.Configuracoes.Geral.TipoDoc = tdMDFe then
      begin
        MemoDados.Lines.Add('Chancel. : '+ DateTimeToStr(ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Declarado.dhChancela));
        MemoDados.Lines.Add('Protocolo: '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Declarado.Protocolo);
      end
      else
      begin
        MemoDados.Lines.Add('Averbado : '+ DateTimeToStr(ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.dhAverbacao));
        MemoDados.Lines.Add('Protocolo: '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.Protocolo);
      end;
      MemoDados.Lines.Add(' ');
      MemoDados.Lines.Add('Dados do Seguro');
      MemoDados.Lines.Add(' ');

      for i := 0 to ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro.Count -1 do
      begin
        MemoDados.Lines.Add('Numero Averbação: '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].NumeroAverbacao);
        MemoDados.Lines.Add('CNPJ Seguradora : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].CNPJSeguradora);
        MemoDados.Lines.Add('Nome Seguradora : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].NomeSeguradora);
        MemoDados.Lines.Add('Numero Apolice  : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].NumApolice);
        MemoDados.Lines.Add('Tipo Mov        : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].TpMov);
        MemoDados.Lines.Add('Tipo DDR        : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].TpDDR);
        MemoDados.Lines.Add('Valor Averbado  : '+ FloatToStr(ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].ValorAverbado));
        MemoDados.Lines.Add('Ramo Averbado   : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Averbado.DadosSeguro[i].RamoAverbado);
      end;

      MemoDados.Lines.Add(' ');
      MemoDados.Lines.Add('Informações');
      MemoDados.Lines.Add(' ');

      for i := 0 to ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Infos.Info.Count -1 do
      begin
        MemoDados.Lines.Add('Codigo   : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Infos.Info[i].Codigo);
        MemoDados.Lines.Add('Descrição: '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Infos.Info[i].Descricao);
      end;

      MemoDados.Lines.Add(' ');
      MemoDados.Lines.Add('Erros');
      MemoDados.Lines.Add(' ');

      for i := 0 to ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Erros.Erro.Count -1 do
      begin
        MemoDados.Lines.Add('Codigo         : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Erros.Erro[i].Codigo);
        MemoDados.Lines.Add('Descrição      : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Erros.Erro[i].Descricao);
        MemoDados.Lines.Add('Valor Esperado : '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Erros.Erro[i].ValorEsperado);
        MemoDados.Lines.Add('Valor Informado: '+ ACBrANe1.WebServices.ANeAverbar.ANeRetorno.Erros.Erro[i].ValorInformado);
      end;
    end;
  end;

  ACBrANe1.Documentos.Clear;
end;

procedure TfrmACBrANe.btnEnviarANeEmailClick(Sender: TObject);
var
  Para : String;
  CC   : Tstrings;
begin
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o ANe';
  OpenDialog1.DefaultExt := '*-ANe.xml';
  OpenDialog1.Filter := 'Arquivos ANe (*-ANe.xml)|*-ANe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrANe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrANe1.Documentos.Clear;
    ACBrANe1.Documentos.LoadFromFile(OpenDialog1.FileName);
    CC:=TstringList.Create;
    CC.Add('email_1@provedor.com'); //especifique um email válido
    CC.Add('email_2@provedor.com.br'); //especifique um email válido

    ACBrANe1.Documentos.Items[0].EnviarEmail(Para
                                             , edtEmailAssunto.Text
                                             , mmEmailMsg.Lines
                                             , False // Enviar PDF junto
                                             , nil   // Lista com emails que serão enviado cópias - TStrings
                                             , nil   // Lista de anexos - TStrings
                                             );
    CC.Free;
  end;
end;

procedure TfrmACBrANe.ACBrANe1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
  Tratado := True;
end;

procedure TfrmACBrANe.ACBrANe1StatusChange(Sender: TObject);
begin
 case ACBrANe1.Status of
  stANeIdle : begin
                if ( frmStatus <> nil ) then frmStatus.Hide;
              end;
  stANeAverbacao : begin
                     if ( frmStatus = nil ) then
                       frmStatus := TfrmStatus.Create(Application);
                     frmStatus.lblStatus.Caption := 'Enviando dados do ANe...';
                     frmStatus.Show;
                     frmStatus.BringToFront;
                   end;
  stANeRetAverbacao : begin
                        if ( frmStatus = nil ) then
                          frmStatus := TfrmStatus.Create(Application);
                        frmStatus.lblStatus.Caption := 'Recebendo dados do ANe...';
                        frmStatus.Show;
                        frmStatus.BringToFront;
                      end;
  stANeEmail : begin
                 if ( frmStatus = nil ) then
                   frmStatus := TfrmStatus.Create(Application);
                 frmStatus.lblStatus.Caption := 'Enviando ANe por e-mail...';
                 frmStatus.Show;
                 frmStatus.BringToFront;
               end;
 end;

 Application.ProcessMessages;
end;

end.
