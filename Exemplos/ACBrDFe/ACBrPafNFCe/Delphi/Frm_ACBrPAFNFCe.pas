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

unit Frm_ACBrPAFNFCe;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Buttons,
  Spin,
  ACBrBase,
  ACBrDFe,
  ACBrPAFNFCe,
  ACBrNFe;

type
  TfrmACBrPAFNFCe = class(TForm)
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
    cbSSLLib: TComboBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbXmlSignLib: TComboBox;
    TabSheet4: TTabSheet;
    GroupBox3: TGroupBox;
    sbtnPathSalvar: TSpeedButton;
    Label31: TLabel;
    Label42: TLabel;
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
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
    TabSheet13: TTabSheet;
    sbPathGTIN: TSpeedButton;
    Label35: TLabel;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathGTIN: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathGTIN: TEdit;
    cbxSepararPorModelo: TCheckBox;
    btnSalvarConfig: TBitBtn;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    pgcBotoes: TPageControl;
    tsConsultas: TTabSheet;
    pgRespostas: TPageControl;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    OpenDialog1: TOpenDialog;
    btnAssinarArquivo: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure btnDataValidadeClick(Sender: TObject);
    procedure btnNumSerieClick(Sender: TObject);
    procedure btnSubNameClick(Sender: TObject);
    procedure btnCNPJClick(Sender: TObject);
    procedure btnIssuerNameClick(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure sbPathGTINClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnNumSerieClick(Sender: TObject);
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

    procedure btnAssinarArquivoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
      { Private declarations }
    FACBrPAFNFCe: TACBrPAFNFCe;
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure AtualizarSSLLibsCombo;
    procedure FACBrPAFNFCeGerarLog(const ALogLine: string; var Tratado: Boolean);
  public
      { Public declarations }
  end;

var
  frmACBrPAFNFCe: TfrmACBrPAFNFCe;

implementation

uses
  math,
  TypInfo,
  blcksock,
  FileCtrl,
  IniFiles,
  ACBrUtil.Base,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrDFeSSL,
  pcnConversao,
  ACBrPAFNFCe_Comum,
  Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}
    { TfrmACBrGTIN }

procedure TfrmACBrPAFNFCe.FACBrPAFNFCeGerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmACBrPAFNFCe.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(FACBrPAFNFCe.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(FACBrPAFNFCe.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(FACBrPAFNFCe.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(FACBrPAFNFCe.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (FACBrPAFNFCe.Configuracoes.Geral.SSLHttpLib in [ httpWinHttp, httpOpenSSL ]);
end;

procedure TfrmACBrPAFNFCe.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(FACBrPAFNFCe.SSL.CertCNPJ);
end;

procedure TfrmACBrPAFNFCe.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(FACBrPAFNFCe.SSL.CertDataVenc));
end;

procedure TfrmACBrPAFNFCe.btnIssuerNameClick(Sender: TObject);
begin
  ShowMessage(FACBrPAFNFCe.SSL.CertIssuerName + sLineBreak + sLineBreak +
      'Certificadora: ' + FACBrPAFNFCe.SSL.CertCertificadora);
end;

procedure TfrmACBrPAFNFCe.btnAssinarArquivoClick(Sender: TObject);
var
  sArquivo: String;
begin
  OpenDialog1.Title      := 'Selecione o arquivo';
  OpenDialog1.DefaultExt := '*.txt';
  OpenDialog1.Filter     := 'Arquivos TXT (*.txt)|*.txt|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  sArquivo := '';
  if OpenDialog1.Execute then
    sArquivo := OpenDialog1.FileName;
  if (sArquivo = '') then
    Exit;

  if (not FileExists(sArquivo)) then
  begin
    ShowMessage('Arquivo não localizado.');
    Exit;
  end;

  FACBrPAFNFCe.MenuFiscal.Inicializar;
  FACBrPAFNFCe.MenuFiscal.LoadFromFile(sArquivo);
  FACBrPAFNFCe.MenuFiscal.NumeroArquivo      := tnroArq_Registros;
  FACBrPAFNFCe.MenuFiscal.DataGeracaoArquivo := Now();
  FACBrPAFNFCe.MenuFiscal.HoraGeracaoArquivo := FACBrPAFNFCe.MenuFiscal.DataGeracaoArquivo;
  FACBrPAFNFCe.MenuFiscal.ArquiteturaBD      := tarqBD_Interno;
  FACBrPAFNFCe.MenuFiscal.ArquiteturaSistema := tarqSist_Interno;
  FACBrPAFNFCe.MenuFiscal.SaveToFile(sArquivo + '.xml');

    //  FACBrPAFNFCe.ssl.Assinar(FACBrPAFNFCe.MenuFiscal.XMLOriginal);
end;

procedure TfrmACBrPAFNFCe.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(FACBrPAFNFCe.SSL.CertNumeroSerie);
end;

procedure TfrmACBrPAFNFCe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrPAFNFCe.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(FACBrPAFNFCe.SSL.CertSubjectName + sLineBreak + sLineBreak +
      'Razão Social: ' + FACBrPAFNFCe.SSL.CertRazaoSocial);
end;

procedure TfrmACBrPAFNFCe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> - 1 then
      FACBrPAFNFCe.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrPAFNFCe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> - 1 then
      FACBrPAFNFCe.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrPAFNFCe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> - 1 then
      FACBrPAFNFCe.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrPAFNFCe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> - 1 then
    FACBrPAFNFCe.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrPAFNFCe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> - 1 then
      FACBrPAFNFCe.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrPAFNFCe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
begin
  FACBrPAFNFCe := TACBrPAFNFCe.Create(nil);
  with FACBrPAFNFCe do
  begin
      //    OnStatusChange                                := ACBrPAFNFCe1StatusChange;
    OnGerarLog                                    := FACBrPAFNFCeGerarLog;
    Configuracoes.Geral.SSLLib                    := libNone;
    Configuracoes.Geral.SSLCryptLib               := cryNone;
    Configuracoes.Geral.SSLHttpLib                := httpNone;
    Configuracoes.Geral.SSLXmlSignLib             := xsNone;
    Configuracoes.Geral.FormatoAlerta             := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';
    Configuracoes.WebServices.UF                  := 'SP';
    Configuracoes.WebServices.AguardarConsultaRet := 0;
    Configuracoes.WebServices.QuebradeLinha       := '|';
    Configuracoes.RespTec.IdCSRT                  := 0;
  end;

  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add(GetEnumName(TypeInfo(TSSLLib), Integer(T)));
  cbSSLLib.ItemIndex := 0;

  cbCryptLib.Items.Clear;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add(GetEnumName(TypeInfo(TSSLCryptLib), Integer(U)));
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Items.Clear;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add(GetEnumName(TypeInfo(TSSLHttpLib), Integer(V)));
  cbHttpLib.ItemIndex := 0;

  cbXmlSignLib.Items.Clear;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add(GetEnumName(TypeInfo(TSSLXmlSignLib), Integer(X)));
  cbXmlSignLib.ItemIndex := 0;

  cbSSLType.Items.Clear;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add(GetEnumName(TypeInfo(TSSLType), Integer(Y)));
  cbSSLType.ItemIndex := 0;

  LerConfiguracao;
end;

procedure TfrmACBrPAFNFCe.FormDestroy(Sender: TObject);
begin
  if (FACBrPAFNFCe <> nil) then
    FreeAndNil(FACBrPAFNFCe);
end;

procedure TfrmACBrPAFNFCe.GravarConfiguracao;
var
  IniFile: String;
  Ini    : TIniFile;
begin
  IniFile := ChangeFileExt(
    application.ExeName,
    '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteInteger(
      'Certificado',
      'SSLLib',
      cbSSLLib.ItemIndex);
    Ini.WriteInteger(
      'Certificado',
      'CryptLib',
      cbCryptLib.ItemIndex);
    Ini.WriteInteger(
      'Certificado',
      'HttpLib',
      cbHttpLib.ItemIndex);
    Ini.WriteInteger(
      'Certificado',
      'XmlSignLib',
      cbXmlSignLib.ItemIndex);
    Ini.WriteString(
      'Certificado',
      'Caminho',
      edtCaminho.Text);
    Ini.WriteString(
      'Certificado',
      'Senha',
      edtSenha.Text);
    Ini.WriteString(
      'Certificado',
      'NumSerie',
      edtNumSerie.Text);

    Ini.WriteBool(
      'Geral',
      'AtualizarXML',
      cbxAtualizarXML.Checked);
    Ini.WriteBool(
      'Geral',
      'ExibirErroSchema',
      cbxExibirErroSchema.Checked);
    Ini.WriteString(
      'Geral',
      'FormatoAlerta',
      edtFormatoAlerta.Text);
    Ini.WriteBool(
      'Geral',
      'RetirarAcentos',
      cbxRetirarAcentos.Checked);
    Ini.WriteBool(
      'Geral',
      'Salvar',
      ckSalvar.Checked);
    Ini.WriteString(
      'Geral',
      'PathSalvar',
      edtPathLogs.Text);
    Ini.WriteString(
      'Geral',
      'PathSchemas',
      edtPathSchemas.Text);

    Ini.WriteString(
      'WebService',
      'UF',
      cbUF.Text);
    Ini.WriteInteger(
      'WebService',
      'Ambiente',
      rgTipoAmb.ItemIndex);
    Ini.WriteBool(
      'WebService',
      'Visualizar',
      cbxVisualizar.Checked);
    Ini.WriteBool(
      'WebService',
      'SalvarSOAP',
      cbxSalvarSOAP.Checked);
    Ini.WriteBool(
      'WebService',
      'AjustarAut',
      cbxAjustarAut.Checked);
    Ini.WriteString(
      'WebService',
      'Aguardar',
      edtAguardar.Text);
    Ini.WriteString(
      'WebService',
      'Tentativas',
      edtTentativas.Text);
    Ini.WriteString(
      'WebService',
      'Intervalo',
      edtIntervalo.Text);
    Ini.WriteInteger(
      'WebService',
      'TimeOut',
      seTimeOut.Value);
    Ini.WriteInteger(
      'WebService',
      'SSLType',
      cbSSLType.ItemIndex);

    Ini.WriteString(
      'Proxy',
      'Host',
      edtProxyHost.Text);
    Ini.WriteString(
      'Proxy',
      'Porta',
      edtProxyPorta.Text);
    Ini.WriteString(
      'Proxy',
      'User',
      edtProxyUser.Text);
    Ini.WriteString(
      'Proxy',
      'Pass',
      edtProxySenha.Text);

    Ini.WriteBool(
      'Arquivos',
      'Salvar',
      cbxSalvarArqs.Checked);
    Ini.WriteBool(
      'Arquivos',
      'PastaMensal',
      cbxPastaMensal.Checked);
    Ini.WriteBool(
      'Arquivos',
      'AddLiteral',
      cbxAdicionaLiteral.Checked);
    Ini.WriteBool(
      'Arquivos',
      'EmissaoPathGTIN',
      cbxEmissaoPathGTIN.Checked);
    Ini.WriteBool(
      'Arquivos',
      'SalvarPathEvento',
      cbxSalvaPathEvento.Checked);
    Ini.WriteBool(
      'Arquivos',
      'SepararPorCNPJ',
      cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(
      'Arquivos',
      'SepararPorModelo',
      cbxSepararPorModelo.Checked);
    Ini.WriteString(
      'Arquivos',
      'PathGTIN',
      edtPathGTIN.Text);

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrPAFNFCe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrPAFNFCe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrPAFNFCe.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrPAFNFCe.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [ fsBold, fsUnderline ];
end;

procedure TfrmACBrPAFNFCe.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [ fsBold ];
end;

procedure TfrmACBrPAFNFCe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrPAFNFCe.LerConfiguracao;
var
  IniFile: String;
  Ini    : TIniFile;
begin
  IniFile := ChangeFileExt(
    application.ExeName,
    '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    cbSSLLib.ItemIndex := Ini.ReadInteger(
      'Certificado',
      'SSLLib',
      0);
    cbCryptLib.ItemIndex := Ini.ReadInteger(
      'Certificado',
      'CryptLib',
      0);
    cbHttpLib.ItemIndex := Ini.ReadInteger(
      'Certificado',
      'HttpLib',
      0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger(
      'Certificado',
      'XmlSignLib',
      0);
    edtCaminho.Text := Ini.ReadString(
      'Certificado',
      'Caminho',
      '');
    edtSenha.Text := Ini.ReadString(
      'Certificado',
      'Senha',
      '');
    edtNumSerie.Text := Ini.ReadString(
      'Certificado',
      'NumSerie',
      '');

    cbxAtualizarXML.Checked := Ini.ReadBool(
      'Geral',
      'AtualizarXML',
      True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(
      'Geral',
      'ExibirErroSchema',
      True);
    edtFormatoAlerta.Text := Ini.ReadString(
      'Geral',
      'FormatoAlerta',
      'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');

    ckSalvar.Checked := Ini.ReadBool(
      'Geral',
      'Salvar',
      True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(
      'Geral',
      'RetirarAcentos',
      True);
    edtPathLogs.Text := Ini.ReadString(
      'Geral',
      'PathSalvar',
      PathWithDelim(ExtractFilePath(application.ExeName)) + 'Logs');
    edtPathSchemas.Text := Ini.ReadString(
      'Geral',
      'PathSchemas',
      PathWithDelim(ExtractFilePath(application.ExeName)) + 'Schemas\');

    cbUF.ItemIndex := cbUF.Items.IndexOf(Ini.ReadString('WebService', 'UF', 'SP'));

    rgTipoAmb.ItemIndex := Ini.ReadInteger(
      'WebService',
      'Ambiente',
      0);
    cbxVisualizar.Checked := Ini.ReadBool(
      'WebService',
      'Visualizar',
      False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(
      'WebService',
      'SalvarSOAP',
      False);
    cbxAjustarAut.Checked := Ini.ReadBool(
      'WebService',
      'AjustarAut',
      False);
    edtAguardar.Text := Ini.ReadString(
      'WebService',
      'Aguardar',
      '0');
    edtTentativas.Text := Ini.ReadString(
      'WebService',
      'Tentativas',
      '5');
    edtIntervalo.Text := Ini.ReadString(
      'WebService',
      'Intervalo',
      '0');
    seTimeOut.Value := Ini.ReadInteger(
      'WebService',
      'TimeOut',
      5000);
    cbSSLType.ItemIndex := Ini.ReadInteger(
      'WebService',
      'SSLType',
      0);

    edtProxyHost.Text := Ini.ReadString(
      'Proxy',
      'Host',
      '');
    edtProxyPorta.Text := Ini.ReadString(
      'Proxy',
      'Porta',
      '');
    edtProxyUser.Text := Ini.ReadString(
      'Proxy',
      'User',
      '');
    edtProxySenha.Text := Ini.ReadString(
      'Proxy',
      'Pass',
      '');

    cbxSalvarArqs.Checked := Ini.ReadBool(
      'Arquivos',
      'Salvar',
      False);
    cbxPastaMensal.Checked := Ini.ReadBool(
      'Arquivos',
      'PastaMensal',
      False);
    cbxAdicionaLiteral.Checked := Ini.ReadBool(
      'Arquivos',
      'AddLiteral',
      False);
    cbxEmissaoPathGTIN.Checked := Ini.ReadBool(
      'Arquivos',
      'EmissaoPathGTIN',
      False);
    cbxSalvaPathEvento.Checked := Ini.ReadBool(
      'Arquivos',
      'SalvarPathEvento',
      False);
    cbxSepararPorCNPJ.Checked := Ini.ReadBool(
      'Arquivos',
      'SepararPorCNPJ',
      False);
    cbxSepararPorModelo.Checked := Ini.ReadBool(
      'Arquivos',
      'SepararPorModelo',
      False);
    edtPathGTIN.Text := Ini.ReadString(
      'Arquivos',
      'PathGTIN',
      '');

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrPAFNFCe.ConfigurarComponente;
var
  PathMensal: string;
  Ok        : Boolean;
begin
  FACBrPAFNFCe.SSL.DescarregarCertificado;

  FACBrPAFNFCe.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  FACBrPAFNFCe.Configuracoes.Certificados.Senha       := AnsiString(edtSenha.Text);
  FACBrPAFNFCe.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  with FACBrPAFNFCe.Configuracoes.Geral do
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
      //    VersaoDF         := ve100;
  end;

  with FACBrPAFNFCe.Configuracoes.WebServices do
  begin
    UF       := cbUF.Text;
    Ambiente := StrToTpAmb(
      Ok,
      IntToStr(rgTipoAmb.ItemIndex + 1));
    Visualizar := cbxVisualizar.Checked;
    Salvar     := cbxSalvarSOAP.Checked;

    AjustaAguardaConsultaRet := cbxAjustarAut.Checked;

    if NaoEstaVazio(edtAguardar.Text) then
      AguardarConsultaRet := ifThen(
        StrToInt(edtAguardar.Text) < 1000,
        StrToInt(edtAguardar.Text) * 1000,
        StrToInt(edtAguardar.Text))
    else
      edtAguardar.Text := IntToStr(AguardarConsultaRet);

    if NaoEstaVazio(edtTentativas.Text) then
      Tentativas := StrToInt(edtTentativas.Text)
    else
      edtTentativas.Text := IntToStr(Tentativas);

    if NaoEstaVazio(edtIntervalo.Text) then
      IntervaloTentativas := ifThen(
        StrToInt(edtIntervalo.Text) < 1000,
        StrToInt(edtIntervalo.Text) * 1000,
        StrToInt(edtIntervalo.Text))
    else
      edtIntervalo.Text := IntToStr(FACBrPAFNFCe.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  FACBrPAFNFCe.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with FACBrPAFNFCe.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
      //    PathGTIN         := edtPathGTIN.Text;
      //    PathMensal       := GetPathGTIN(0);
    PathSalvar := PathMensal;
  end;
end;

procedure TfrmACBrPAFNFCe.PathClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(TEdit(Sender).Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := TEdit(Sender).Text;

  if SelectDirectory(Dir, [ sdAllowCreate, sdPerformCreate, sdPrompt ], SELDIRHELP) then
    TEdit(Sender).Text := Dir;
end;

procedure TfrmACBrPAFNFCe.sbPathGTINClick(Sender: TObject);
begin
  PathClick(edtPathGTIN);
end;

procedure TfrmACBrPAFNFCe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title      := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter     := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrPAFNFCe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := FACBrPAFNFCe.SSL.SelecionarCertificado;
end;

procedure TfrmACBrPAFNFCe.sbtnNumSerieClick(Sender: TObject);
var
  I                        : Integer;
  ASerie                   : String;
  AddRow                   : Boolean;
  LfrmSelecionarCertificado: TfrmSelecionarCertificado;
begin
  LfrmSelecionarCertificado := TfrmSelecionarCertificado.Create(self);
  try

    FACBrPAFNFCe.SSL.LerCertificadosStore;
    AddRow := False;

    with LfrmSelecionarCertificado.StringGrid1 do
    begin
      ColWidths[ 0 ] := 220;
      ColWidths[ 1 ] := 250;
      ColWidths[ 2 ] := 120;
      ColWidths[ 3 ] := 80;
      ColWidths[ 4 ] := 150;

      Cells[ 0, 0 ] := 'Num.Série';
      Cells[ 1, 0 ] := 'Razão Social';
      Cells[ 2, 0 ] := 'CNPJ';
      Cells[ 3, 0 ] := 'Validade';
      Cells[ 4, 0 ] := 'Certificadora';
    end;

    for I := 0 to FACBrPAFNFCe.SSL.ListaCertificados.Count - 1 do
    begin
      with FACBrPAFNFCe.SSL.ListaCertificados[ I ] do
      begin
        ASerie := NumeroSerie;

        if (CNPJ <> '') then
        begin
          with LfrmSelecionarCertificado.StringGrid1 do
          begin
            if AddRow then
              RowCount := RowCount + 1;

            Cells[ 0, RowCount - 1 ] := NumeroSerie;
            Cells[ 1, RowCount - 1 ] := RazaoSocial;
            Cells[ 2, RowCount - 1 ] := CNPJ;
            Cells[ 3, RowCount - 1 ] := FormatDateBr(DataVenc);
            Cells[ 4, RowCount - 1 ] := Certificadora;

            AddRow := True;
          end;
        end;
      end;
    end;

    if LfrmSelecionarCertificado.ShowModal = mrOK then
      edtNumSerie.Text := LfrmSelecionarCertificado.StringGrid1.Cells[ 0, LfrmSelecionarCertificado.StringGrid1.Row ];

  finally
    LfrmSelecionarCertificado.Free;
  end;
end;

procedure TfrmACBrPAFNFCe.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrPAFNFCe.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

end.
