{$I ACBr.inc}

unit Frm_Demo_ACBrNFSe;

interface

uses
  IniFiles, ShellAPI, SynMemo, SynHighlighterXML,
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Spin, pcnConversao, pnfsConversao, unit2,
  ACBrNFSe, ACBrNFSeDANFSeClass, ACBrNFSeDANFSeRLClass, pnfsNFSe, ACBrMail,
  blcksock, ACBrDFe, ACBrUtil;

type

  { TfrmDemo_ACBrNFSe }

  TfrmDemo_ACBrNFSe = class(TForm)
    ACBrNFSeDANFSeRL1: TACBrNFSeDANFSeRL;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    cbAssinar: TCheckBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbSSLLib: TComboBox;
    cbSSLType: TComboBox;
    cbXmlSignLib: TComboBox;
    ckSalvarSOAP: TCheckBox;
    ckVisualizar: TCheckBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lCryptLib: TLabel;
    lHttpLib: TLabel;
    lSSLLib: TLabel;
    lSSLLib1: TLabel;
    lTimeOut: TLabel;
    lXmlSign: TLabel;
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    gbConfiguracoes: TGroupBox;
    PageControl1: TPageControl;
    sbtnGetCert1: TSpeedButton;
    seTimeOut: TSpinEdit;
    TabSheet1: TTabSheet;
    gbCertificado: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label25: TLabel;
    sbtnGetCert: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    TabSheet2: TTabSheet;
    gbGeral: TGroupBox;
    Label7: TLabel;
    sbtnLogoMarca: TSpeedButton;
    sbtnPathSalvar: TSpeedButton;
    edtLogoMarca: TEdit;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    TabSheet3: TTabSheet;
    gbWebService: TGroupBox;
    rgTipoAmb: TRadioGroup;
    gbProxy: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    TabSheet4: TTabSheet;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edtEmitCNPJ: TEdit;
    edtEmitIM: TEdit;
    edtEmitRazao: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitCEP: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitComp: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCidade: TEdit;
    edtEmitUF: TEdit;
    TabSheet7: TTabSheet;
    gbEmail: TGroupBox;
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
    Panel2: TPanel;
    Panel3: TPanel;
    btnImprimir: TButton;
    btnConsultarLote: TButton;
    btnCancNFSe: TButton;
    btnGerarEnviarLote: TButton;
    btnGerarRPS: TButton;
    btnConsultarSitLote: TButton;
    pgRespostas: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
    WBResposta: TSynMemo;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    trvwNFSe: TTreeView;
    TabSheet10: TTabSheet;
    memoRespWS: TMemo;
    Dados: TTabSheet;
    MemoDados: TMemo;
    OpenDialog1: TOpenDialog;
    lblSchemas: TLabel;
    edtSchemas: TEdit;
    sbtSchemas: TSpeedButton;
    Label32: TLabel;
    edtPrestLogo: TEdit;
    sbtnPrestLogo: TSpeedButton;
    Label33: TLabel;
    edtPrefeitura: TEdit;
    btnConsultarNFSeRPS: TButton;
    btnConsultarNFSePeriodo: TButton;
    cbCidades: TComboBox;
    Label6: TLabel;
    edtSenhaWeb: TEdit;
    edtCodCidade: TEdit;
    Label29: TLabel;
    ACBrNFSe1: TACBrNFSe;
    Label20: TLabel;
    edtUserWeb: TEdit;
    btnGerarEnviarNFSe: TButton;
    btnEnviaremail: TButton;
    Label31: TLabel;
    edtEmailRemetente: TEdit;
    Label30: TLabel;
    btnLinkNFSe: TButton;
    btnGerarLoteRPS: TButton;
    btnGerarEnviarSincrono: TButton;
    Button1: TButton;
    btnSubsNFSe: TButton;
    ACBrMail1: TACBrMail;
    Label34: TLabel;
    edtArqINI: TEdit;
    sbtArqINI: TSpeedButton;
    cbEmailTLS: TCheckBox;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCert1Click(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnLogoMarcaClick(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure btnGerarEnviarLoteClick(Sender: TObject);
    procedure btnConsultarLoteClick(Sender: TObject);
    procedure btnCancNFSeClick(Sender: TObject);
    procedure btnConsultarSitLoteClick(Sender: TObject);
    procedure btnGerarRPSClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure ACBrNFSe1StatusChange(Sender: TObject);
    procedure ACBrNFSe1GerarLog(const Mensagem: string);
    procedure sbtSchemasClick(Sender: TObject);
    procedure sbtnPrestLogoClick(Sender: TObject);
    procedure btnConsultarNFSeRPSClick(Sender: TObject);
    procedure btnConsultarNFSePeriodoClick(Sender: TObject);
    procedure cbCidadesChange(Sender: TObject);
    function RoundTo5(Valor: double; Casas: integer): double;
    procedure btnGerarEnviarNFSeClick(Sender: TObject);
    procedure btnEnviaremailClick(Sender: TObject);
    procedure btnLinkNFSeClick(Sender: TObject);
    procedure btnGerarLoteRPSClick(Sender: TObject);
    procedure btnGerarEnviarSincronoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnSubsNFSeClick(Sender: TObject);
    procedure sbtArqINIClick(Sender: TObject);
  private
    { Private declarations }
    Ok: boolean;
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfiguraComponente;
    procedure LoadXML(MyMemo: TMemo; MyWebBrowser: TSynMemo);
    procedure AlimentaComponente(NumNFSe: string);
    procedure CarregarIniCidades;
    procedure AtualizaSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmDemo_ACBrNFSe: TfrmDemo_ACBrNFSe;

implementation

uses
  FileCtrl, DateUtils, Math,
  ufrmStatus, ACBrDFeSSL, typinfo,
  ACBrNFSeNotasFiscais, ACBrDFeUtil;

const
  SELDIRHELP = 1000;

{$R *.lfm}

procedure TfrmDemo_ACBrNFSe.GravarConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteString('Emitente', 'CNPJ', edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IM', edtEmitIM.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia', edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone', edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP', edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro', edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero', edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro', edtEmitBairro.Text);
    Ini.WriteString('Emitente', 'CodCidade', edtCodCidade.Text);
    Ini.WriteString('Emitente', 'Cidade', edtEmitCidade.Text);
    Ini.WriteString('Emitente', 'UF', edtEmitUF.Text);

    Ini.WriteInteger('Certificado', 'SSLLib', cbSSLLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'CryptLib', cbCryptLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'HttpLib', cbHttpLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString('Certificado', 'Caminho', edtCaminho.Text);
    Ini.WriteString('Certificado', 'Senha', edtSenha.Text);
    Ini.WriteString('Certificado', 'NumSerie', edtNumSerie.Text);

    Ini.WriteString('Geral', 'Schemas', edtSchemas.Text);
    Ini.WriteString('Geral', 'LogoMarca', edtLogoMarca.Text);
    Ini.WriteString('Geral', 'PrestLogo', edtPrestLogo.Text);
    Ini.WriteBool('Geral', 'Salvar', ckSalvar.Checked);
    Ini.WriteString('Geral', 'PathSalvar', edtPathLogs.Text);
    Ini.WriteString('Geral', 'Prefeitura', edtPrefeitura.Text);
    Ini.WriteString('Geral', 'PathINI', edtArqINI.Text);

    Ini.WriteInteger('WebService', 'Ambiente', rgTipoAmb.ItemIndex);
    Ini.WriteBool('WebService', 'Visualizar', ckVisualizar.Checked);
    Ini.WriteString('WebService', 'SenhaWeb', edtSenhaWeb.Text);
    Ini.WriteString('WebService', 'UserWeb', edtUserWeb.Text);
    Ini.WriteBool('WebService', 'SalvarSoap', ckSalvarSoap.Checked);
    Ini.WriteInteger('WebService', 'TimeOut', seTimeOut.Value);
    Ini.WriteInteger('WebService', 'SSLType', cbSSLType.ItemIndex);

    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', edtProxySenha.Text);

    Ini.WriteString('Email', 'Host', edtSmtpHost.Text);
    Ini.WriteString('Email', 'Port', edtSmtpPort.Text);
    Ini.WriteString('Email', 'User', edtSmtpUser.Text);
    Ini.WriteString('Email', 'Pass', edtSmtpPass.Text);
    Ini.WriteString('Email', 'Assunto', edtEmailAssunto.Text);
    Ini.WriteBool('Email', 'SSL', cbEmailSSL.Checked);
    Ini.WriteBool('Email', 'TLS', cbEmailTLS.Checked);
    Ini.WriteString('Email', 'Remetente', edtEmailRemetente.Text);

    StreamMemo := TMemoryStream.Create;
    mmEmailMsg.Lines.SaveToStream(StreamMemo);
    StreamMemo.Seek(0, soFromBeginning);
    Ini.WriteBinaryStream('Email', 'Mensagem', StreamMemo);

    StreamMemo.Free;
  finally
    Ini.Free;
  end;
end;

procedure TfrmDemo_ACBrNFSe.LerConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    cbSSLLib.ItemIndex := Ini.ReadInteger('Certificado', 'SSLLib', 0);
    cbCryptLib.ItemIndex := Ini.ReadInteger('Certificado', 'CryptLib', 0);
    cbHttpLib.ItemIndex := Ini.ReadInteger('Certificado', 'HttpLib', 0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger('Certificado', 'XmlSignLib', 0);
    edtCaminho.Text := Ini.ReadString('Certificado', 'Caminho', '');
    edtSenha.Text := Ini.ReadString('Certificado', 'Senha', '');
    edtNumSerie.Text := Ini.ReadString('Certificado', 'NumSerie', '');

    edtEmitCNPJ.Text := Ini.ReadString('Emitente', 'CNPJ', '');
    edtEmitIM.Text := Ini.ReadString('Emitente', 'IM', '');
    edtEmitRazao.Text := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text := Ini.ReadString('Emitente', 'Fantasia', '');
    edtEmitFone.Text := Ini.ReadString('Emitente', 'Fone', '');
    edtEmitCEP.Text := Ini.ReadString('Emitente', 'CEP', '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro', '');
    edtEmitNumero.Text := Ini.ReadString('Emitente', 'Numero', '');
    edtEmitComp.Text := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text := Ini.ReadString('Emitente', 'Bairro', '');
    edtEmitCidade.Text := Ini.ReadString('Emitente', 'Cidade', '');
    edtEmitUF.Text := Ini.ReadString('Emitente', 'UF', '');
    edtCodCidade.Text := Ini.ReadString('Emitente', 'CodCidade', '');
    cbCidades.ItemIndex := cbCidades.Items.IndexOf(edtEmitCidade.Text +
      '/' + edtCodCidade.Text + '/' + edtEmitUF.Text);

    edtSchemas.Text := Ini.ReadString('Geral', 'Schemas', '');
    edtLogoMarca.Text := Ini.ReadString('Geral', 'LogoMarca', '');
    edtPrestLogo.Text := Ini.ReadString('Geral', 'PrestLogo', '');
    ckSalvar.Checked := Ini.ReadBool('Geral', 'Salvar', True);
    edtPathLogs.Text := Ini.ReadString('Geral', 'PathSalvar', '');
    edtPrefeitura.Text := Ini.ReadString('Geral', 'Prefeitura', '');
    edtArqINI.Text := Ini.ReadString('Geral', 'PathINI', '');

    rgTipoAmb.ItemIndex := Ini.ReadInteger('WebService', 'Ambiente', 0);
    ckVisualizar.Checked := Ini.ReadBool('WebService', 'Visualizar', False);
    edtSenhaWeb.Text := Ini.ReadString('WebService', 'SenhaWeb', '');
    edtUserWeb.Text := Ini.ReadString('WebService', 'UserWeb', '');
    ckSalvarSoap.Checked := Ini.ReadBool('WebService', 'SalvarSoap', False);
    seTimeOut.Value := Ini.ReadInteger('WebService', 'TimeOut', 5000);
    cbSSLType.ItemIndex := Ini.ReadInteger('WebService', 'SSLType', 0);

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass', '');

    edtSmtpHost.Text := Ini.ReadString('Email', 'Host', '');
    edtSmtpPort.Text := Ini.ReadString('Email', 'Port', '');
    edtSmtpUser.Text := Ini.ReadString('Email', 'User', '');
    edtSmtpPass.Text := Ini.ReadString('Email', 'Pass', '');
    edtEmailAssunto.Text := Ini.ReadString('Email', 'Assunto', '');
    cbEmailSSL.Checked := Ini.ReadBool('Email', 'SSL', False);
    cbEmailTLS.Checked := Ini.ReadBool('Email', 'TLS', False);
    edtEmailRemetente.Text := Ini.ReadString('Email', 'Remetente', '');

    StreamMemo := TMemoryStream.Create;
    Ini.ReadBinaryStream('Email', 'Mensagem', StreamMemo);
    mmEmailMsg.Lines.LoadFromStream(StreamMemo);
    StreamMemo.Free;

  finally
    Ini.Free;
  end;
end;

procedure TfrmDemo_ACBrNFSe.ConfiguraComponente;
var
  PathMensal: string;
begin
  ACBrNFSe1.Configuracoes.Certificados.ArquivoPFX := edtCaminho.Text;
  ACBrNFSe1.Configuracoes.Certificados.Senha := edtSenha.Text;
  ACBrNFSe1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrNFSe1.Configuracoes.Certificados.VerificarValidade := True;
  ACBrNFSe1.Configuracoes.Arquivos.AdicionarLiteral := True;
  ACBrNFSe1.Configuracoes.Arquivos.EmissaoPathNFSe := True;
  ACBrNFSe1.Configuracoes.Arquivos.SepararPorMes := True;
  ACBrNFSe1.Configuracoes.Arquivos.SepararPorCNPJ := False;
  ACBrNFSe1.Configuracoes.Arquivos.PathGer := edtPathLogs.Text;
  ACBrNFSe1.Configuracoes.Arquivos.PathSchemas := edtSchemas.Text;

  PathMensal := ACBrNFSe1.Configuracoes.Arquivos.GetPathGer(0);

  ACBrNFSe1.Configuracoes.Arquivos.PathCan := PathMensal;
  ACBrNFSe1.Configuracoes.Arquivos.PathSalvar := PathMensal;
  ACBrNFSe1.Configuracoes.Arquivos.Salvar := True;

  ACBrNFSe1.Configuracoes.Geral.PathIniCidades := edtArqINI.Text;
  ACBrNFSe1.Configuracoes.Geral.PathIniProvedor := edtArqINI.Text;
  ACBrNFSe1.Configuracoes.Geral.Salvar := ckSalvar.Checked;
  ACBrNFSe1.Configuracoes.Geral.CodigoMunicipio := StrToIntDef(edtCodCidade.Text, 0);
  ACBrNFSe1.Configuracoes.Geral.SenhaWeb := edtSenhaWeb.Text;
  ACBrNFSe1.Configuracoes.Geral.UserWeb := edtUserWeb.Text;

  ACBrNFSe1.Configuracoes.Geral.Emitente.CNPJ := edtEmitCNPJ.Text;
  ACBrNFSe1.Configuracoes.Geral.Emitente.InscMun := edtEmitIM.Text;
  ACBrNFSe1.Configuracoes.Geral.Emitente.RazSocial := edtEmitRazao.Text;

  ACBrNFSe1.Configuracoes.Geral.Emitente.WebUser := edtUserWeb.Text;
  ACBrNFSe1.Configuracoes.Geral.Emitente.WebSenha := edtSenhaWeb.Text;
  ACBrNFSe1.Configuracoes.Geral.Emitente.WebFraseSecr := '';

  ACBrNFSe1.Configuracoes.WebServices.Salvar := ckSalvarSoap.Checked;
  ACBrNFSe1.Configuracoes.WebServices.Ambiente :=
    StrToTpAmb(Ok, IntToStr(rgTipoAmb.ItemIndex + 1));
  ACBrNFSe1.Configuracoes.WebServices.Visualizar := ckVisualizar.Checked;
  ACBrNFSe1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
  ACBrNFSe1.Configuracoes.WebServices.ProxyPort := edtProxyPorta.Text;
  ACBrNFSe1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
  ACBrNFSe1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;
  ACBrNFSe1.Configuracoes.WebServices.TimeOut := seTimeOut.Value;

  with ACBrNFSe1.Configuracoes.Geral do
  begin
    SSLLib := TSSLLib(cbSSLLib.ItemIndex);
    SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
    SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
    SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
    SetConfigMunicipio;
  end;

  if ACBrNFSe1.DANFSe <> nil then
  begin
    ACBrNFSe1.DANFSe.Logo := edtLogoMarca.Text;
    ACBrNFSe1.DANFSe.PrestLogo := edtPrestLogo.Text;
    ACBrNFSe1.DANFSe.Prefeitura := edtPrefeitura.Text;
    ACBrNFSe1.DANFSe.PathPDF := PathMensal;

    //  TTipoDANFSE = ( tpPadrao, tpIssDSF, tpFiorilli );
    ACBrNFSe1.DANFSe.TipoDANFSE := tpPadrao;
  end;

  ACBrNFSe1.MAIL.Host := edtSmtpHost.Text;
  ACBrNFSe1.MAIL.Port := edtSmtpPort.Text;
  ACBrNFSe1.MAIL.Username := edtSmtpUser.Text;
  ACBrNFSe1.MAIL.Password := edtSmtpPass.Text;
  ACBrNFSe1.MAIL.From := edtEmailRemetente.Text;
  ACBrNFSe1.MAIL.FromName := edtEmitRazao.Text;
  ACBrNFSe1.MAIL.SetTLS := cbEmailTLS.Checked;
  ACBrNFSe1.MAIL.SetSSL := cbEmailSSL.Checked;
  ACBrNFSe1.MAIL.UseThread := False;
  ACBrNFSe1.MAIL.ReadingConfirmation := False;

  lblSchemas.Caption := ACBrNFSe1.Configuracoes.Geral.xProvedor;
end;

procedure TfrmDemo_ACBrNFSe.AtualizaSSLLibsCombo;
begin
  cbSSLLib.ItemIndex := integer(ACBrNFSe1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex := integer(ACBrNFSe1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex := integer(ACBrNFSe1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := integer(ACBrNFSe1.Configuracoes.Geral.SSLXmlSignLib);
end;

procedure TfrmDemo_ACBrNFSe.LoadXML(MyMemo: TMemo; MyWebBrowser: TSynMemo);
var
  vText: string;
begin
  vText := MyMemo.Text;

  // formata resposta
  vText := StringReplace(vText, '>', '>' + LineEnding + '    ', [rfReplaceAll]);
  vText := StringReplace(vText, '<', LineEnding + '  <', [rfReplaceAll]);
  vText := StringReplace(vText, '>' + LineEnding + '    ' + LineEnding +
    '  <', '>' + LineEnding + '  <', [rfReplaceAll]);
  vText := StringReplace(vText, '  </ret', '</ret', []);

  // exibe resposta
  MyWebBrowser.Text := Trim(vText);
end;

procedure TfrmDemo_ACBrNFSe.AlimentaComponente(NumNFSe: string);
var
  ValorISS: double;
begin
  ACBrNFSe1.NotasFiscais.Clear;

  with ACBrNFSe1 do
  begin
    NotasFiscais.NumeroLote := '1';
    NotasFiscais.Transacao := True;

    with NotasFiscais.Add.NFSe do
    begin
      IdentificacaoRps.Numero := FormatFloat('#########0', StrToInt(NumNFSe));

      // Para o provedor ISS.NET em ambiente de HomologaÁ„o mudar a sÈrie para '8'
      IdentificacaoRps.Serie := 'UNICA';

      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      IdentificacaoRps.Tipo := trRPS;

      DataEmissao := Now;

     (*
     TnfseNaturezaOperacao = ( no1, no2, no3, no4, no5, no6, no7,
                               no50, no51, no52, no53, no54, no55, no56, no57, no58, no59,
                               no60, no61, no62, no63, no64, no65, no66, no67, no68, no69,
                               no70, no71, no72, no78, no79,
                               no101, no111, no121, no201, no301,
                               no501, no511, no541, no551, no601, no701 );
     *)
      NaturezaOperacao := no1;
      //     NaturezaOperacao := no51;

      // TnfseRegimeEspecialTributacao = ( retNenhum, retMicroempresaMunicipal, retEstimativa, retSociedadeProfissionais, retCooperativa, retMicroempresarioIndividual, retMicroempresarioEmpresaPP );
      //     RegimeEspecialTributacao := retNenhum;
      RegimeEspecialTributacao := retEstimativa;

      // TnfseSimNao = ( snSim, snNao );
      OptanteSimplesNacional := snNao;

      // TnfseSimNao = ( snSim, snNao );
      IncentivadorCultural := snNao;

      // TnfseSimNao = ( snSim, snNao );
      // snSim = Ambiente de ProduÁ„o
      // snNao = Ambiente de HomologaÁ„o
      Producao := snNao;

      // TnfseStatusRPS = ( srNormal, srCancelado );
      Status := srNormal;

      // Somente Os provedores Betha, FISSLex e SimplISS permitem incluir no RPS
      // a TAG: OutrasInformacoes os demais essa TAG È gerada e preenchida pelo
      // WebService do provedor.
      OutrasInformacoes := 'Pagamento a Vista';

      // Usado quando o RPS for substituir outro
      //     RpsSubstituido.Numero := FormatFloat('#########0', i);
      //     RpsSubstituido.Serie  := 'UNICA';
      // TnfseTipoRPS = ( trRPS, trNFConjugada, trCupom );
      ///     RpsSubstituido.Tipo   := trRPS;

      Servico.Valores.ValorServicos := 1685.50;
      Servico.Valores.ValorDeducoes := 0.00;
      Servico.Valores.ValorPis := 0.00;
      Servico.Valores.ValorCofins := 0.00;
      Servico.Valores.ValorInss := 0.00;
      Servico.Valores.ValorIr := 0.00;
      Servico.Valores.ValorCsll := 0.00;

      // TnfseSituacaoTributaria = ( stRetencao, stNormal, stSubstituicao );
      // stRetencao = snSim
      // stNormal   = snNao

      // Neste exemplo n„o temos ISS Retido ( stNormal = N„o )
      // Logo o valor do ISS Retido È igual a zero.
      Servico.Valores.IssRetido := stNormal;
      Servico.Valores.ValorIssRetido := 0.00;

      Servico.Valores.OutrasRetencoes := 0.00;
      Servico.Valores.DescontoIncondicionado := 0.00;
      Servico.Valores.DescontoCondicionado := 0.00;

      Servico.Valores.BaseCalculo :=
        Servico.Valores.ValorServicos - Servico.Valores.ValorDeducoes -
        Servico.Valores.DescontoIncondicionado;
      // No caso do provedor Ginfes devemos informar a aliquota j· dividida por 100
      // para outros provedores devemos informar por exemplo 3, mas ao fazer o calculo
      // do valor do ISS devemos dividir por 100
      Servico.Valores.Aliquota := 2;

      // Valor do ISS calculado multiplicando-se a base de calculo pela aliquota
      ValorISS := Servico.Valores.BaseCalculo * Servico.Valores.Aliquota / 100;

      // A funÁ„o RoundTo5 È usada para arredondar valores, sendo que o segundo
      // parametro se refere ao numero de casas decimais.
      // exemplos: RoundTo5(50.532, -2) ==> 50.53
      // exemplos: RoundTo5(50.535, -2) ==> 50.54
      // exemplos: RoundTo5(50.536, -2) ==> 50.54

      Servico.Valores.ValorIss := RoundTo5(ValorISS, -2);

      Servico.Valores.ValorLiquidoNfse :=
        Servico.Valores.ValorServicos - Servico.Valores.ValorPis -
        Servico.Valores.ValorCofins - Servico.Valores.ValorInss -
        Servico.Valores.ValorIr - Servico.Valores.ValorCsll -
        Servico.Valores.OutrasRetencoes - Servico.Valores.ValorIssRetido -
        Servico.Valores.DescontoIncondicionado -
        Servico.Valores.DescontoCondicionado;

      // TnfseResponsavelRetencao = ( ptTomador, rtPrestador );
      Servico.ResponsavelRetencao := ptTomador;

      Servico.ItemListaServico := '14.01';

      // Para o provedor ISS.NET em ambiente de HomologaÁ„o
      // o Codigo CNAE tem que ser '6511102'
      // Servico.CodigoCnae                := '123'; // InformaÁ„o Opcional
      Servico.CodigoTributacaoMunicipio := '3314799';
      Servico.Discriminacao := 'discriminacao I;discriminacao II';

      // Para o provedor ISS.NET em ambiente de HomologaÁ„o
      // o Codigo do Municipio tem que ser '999'
      Servico.CodigoMunicipio := edtCodCidade.Text;

      // Informar A Exigibilidade ISS para fintelISS [1/2/3/4/5/6/7]
      Servico.ExigibilidadeISS := exiExigivel;

      // Informar para Saatri
      Servico.CodigoPais := 1058; // Brasil
      Servico.MunicipioIncidencia := StrToIntDef(edtCodCidade.Text, 0);

      // Somente o provedor SimplISS permite infomar mais de 1 serviÁo
      with Servico.ItemServico.Add do
      begin
        Descricao := 'SERVICO 1';
        Quantidade := 1;
        ValorUnitario := 15.00;
      end;

      Prestador.Cnpj := edtEmitCNPJ.Text;
      Prestador.InscricaoMunicipal := edtEmitIM.Text;

      // Para o provedor ISSDigital deve-se informar tambÈm:
      Prestador.Senha := 'senha';
      Prestador.FraseSecreta := 'frase secreta';
      Prestador.cUF := 33;

      PrestadorServico.Endereco.CodigoMunicipio := edtCodCidade.Text;
      PrestadorServico.RazaoSocial := edtEmitRazao.Text;

      Tomador.IdentificacaoTomador.CpfCnpj := '99999999000191';
      Tomador.IdentificacaoTomador.InscricaoMunicipal := '1733160024';

      Tomador.RazaoSocial := 'INSCRICAO DE TESTE';

      Tomador.Endereco.Endereco := 'RUA PRINCIPAL';
      Tomador.Endereco.Numero := '100';
      Tomador.Endereco.Complemento := 'APTO 11';
      Tomador.Endereco.Bairro := 'CENTRO';
      Tomador.Endereco.CodigoMunicipio := edtCodCidade.Text;
      Tomador.Endereco.UF := edtEmitUF.Text;
      Tomador.Endereco.CodigoPais := 1058; // Brasil
      Tomador.Endereco.CEP := edtEmitCEP.Text;
      //Provedor Equiplano È obrigatÛrio o pais e IE
      Tomador.Endereco.xPais := 'BRASIL';
      Tomador.IdentificacaoTomador.InscricaoEstadual := '123456';

      Tomador.Contato.Telefone := '1122223333';
      Tomador.Contato.Email := 'nome@provedor.com.br';

      // Usado quando houver um intermediario na prestaÁ„o do serviÁo
      //     IntermediarioServico.RazaoSocial        := 'razao';
      //     IntermediarioServico.CpfCnpj            := '00000000000';
      //     IntermediarioServico.InscricaoMunicipal := '12547478';


      // Usado quando o serviÁo for uma obra
      //     ConstrucaoCivil.CodigoObra := '88888';
      //     ConstrucaoCivil.Art        := '433';

    end;
  end;

end;

procedure TfrmDemo_ACBrNFSe.CarregarIniCidades;
var
  IniCidades: TIniFile;
  IniSessions: TStringList;
  ListCidades: TStringList;
  ArqCidades: ansistring;

  Cidade, UF: string;
  i: integer;
begin
  ArqCidades := PathWithDelim(Trim(edtArqINI.Text)) + 'Cidades.ini';
  if not ((edtArqINI.Text <> '') or FileExists(ArqCidades)) then
    Exit;

  IniCidades := TIniFile.Create(ArqCidades);

  try
    IniSessions := TStringList.Create;
    IniCidades.ReadSections(IniSessions);

    if (IniSessions.Count > 0) then
    begin
      cbCidades.Items.Clear;
      ListCidades := TStringList.Create;
      try
        ListCidades.BeginUpdate;
        try
          ListCidades.Clear;
          for i := 0 to IniSessions.Count - 1 do
          begin
            if (StrToIntDef(IniSessions[i], 0) > 0) then
            begin
              Cidade := ACBrStr(IniCidades.ReadString(IniSessions[i], 'Nome', ''));
              if (Cidade <> '') then
              begin
                UF := IniCidades.ReadString(IniSessions[i], 'UF', '');
                ListCidades.Add(Cidade + '/' + IniSessions[i] + '/' + UF);
              end;
            end;
          end;
        finally
          ListCidades.EndUpdate;
        end;

        ListCidades.Sort;
        cbCidades.Items.AddStrings(ListCidades);
      finally
        ListCidades.Free;
      end;
    end;
  finally
    IniCidades.Free;
    IniSessions.Free;
  end;
end;

procedure TfrmDemo_ACBrNFSe.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
  begin
    edtCaminho.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmDemo_ACBrNFSe.sbtnGetCert1Click(Sender: TObject);
var
  I: integer;
begin
  frSelecionarCertificado := TfrSelecionarCertificado.Create(Self);
  try
    ACBrNFSe1.SSL.LerCertificadosStore;

    for I := 0 to ACBrNFSe1.SSL.ListaCertificados.Count - 1 do
    begin
      with ACBrNFSe1.SSL.ListaCertificados[I] do
      begin
        if (CNPJ <> '') then
        begin
          with frSelecionarCertificado.StringGrid1 do
          begin
            RowCount := RowCount + 1;
            Cells[0, RowCount - 1] := NumeroSerie;
            Cells[1, RowCount - 1] := RazaoSocial;
            Cells[2, RowCount - 1] := CNPJ;
            Cells[3, RowCount - 1] := FormatDateBr(DataVenc);
            Cells[4, RowCount - 1] := Certificadora;
          end;
        end;
      end;
    end;

    frSelecionarCertificado.ShowModal;

    if frSelecionarCertificado.ModalResult = mrOk then
      edtNumSerie.Text := frSelecionarCertificado.StringGrid1.Cells[0,
        frSelecionarCertificado.StringGrid1.Row];

  finally
    frSelecionarCertificado.Free;
  end;
end;

procedure TfrmDemo_ACBrNFSe.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrNFSe.Button8Click(Sender: TObject);
begin
  ACBrNFSe1.SSL.CarregarCertificado;
end;

procedure TfrmDemo_ACBrNFSe.Button9Click(Sender: TObject);
var
  Acao: string;
  OldUseCert: boolean;
begin
  Acao := '<?xml version="1.0" encoding="UTF-8" standalone="no"?>' +
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ' +
    'xmlns:cli="http://cliente.bean.master.sigep.bsb.correios.com.br/"> ' +
    ' <soapenv:Header/>' + ' <soapenv:Body>' + ' <cli:consultaCEP>' +
    ' <cep>18270-170</cep>' + ' </cli:consultaCEP>' + ' </soapenv:Body>' +
    ' </soapenv:Envelope>';

  OldUseCert := ACBrNFSe1.SSL.UseCertificateHTTP;
  ACBrNFSe1.SSL.UseCertificateHTTP := False;
  try
    WBResposta.Text := ACBrNFSe1.SSL.Enviar(Acao,
      'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl',
      '');
  finally
    ACBrNFSe1.SSL.UseCertificateHTTP := OldUseCert;
  end;
  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrNFSe.Button11Click(Sender: TObject);
begin
  ACBrNFSe1.SSL.DescarregarCertificado;
end;

procedure TfrmDemo_ACBrNFSe.Button12Click(Sender: TObject);
var
  Erro, AName: string;
begin
  with ACBrNFSe1.SSL do
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

procedure TfrmDemo_ACBrNFSe.Button2Click(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrNFSe1.SSL.CertDataVenc));
end;

procedure TfrmDemo_ACBrNFSe.Button10Click(Sender: TObject);
begin
  ShowMessage(ACBrNFSe1.SSL.CertIssuerName + sLineBreak + sLineBreak +
    'Certificadora: ' + ACBrNFSe1.SSL.CertCertificadora);
end;

procedure TfrmDemo_ACBrNFSe.Button3Click(Sender: TObject);
begin
  ShowMessage(ACBrNFSe1.SSL.CertSubjectName + sLineBreak + sLineBreak +
    'Raz„o Social: ' + ACBrNFSe1.SSL.CertRazaoSocial);
end;

procedure TfrmDemo_ACBrNFSe.Button4Click(Sender: TObject);
begin
  ShowMessage(ACBrNFSe1.SSL.CertCNPJ);
end;

procedure TfrmDemo_ACBrNFSe.Button5Click(Sender: TObject);
begin
  ShowMessage(ACBrNFSe1.SSL.CertNumeroSerie);
end;

procedure TfrmDemo_ACBrNFSe.Button7Click(Sender: TObject);
var
  Ahash: ansistring;
begin
  Ahash := ACBrNFSe1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add(Ahash);
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmDemo_ACBrNFSe.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrNFSe.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrNFSe.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
    ACBrNFSe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmDemo_ACBrNFSe.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrNFSe1.Configuracoes.Geral.SSLXmlSignLib :=
        TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrNFSe.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrNFSe1.SSL.SelecionarCertificado;
end;

procedure TfrmDemo_ACBrNFSe.sbtnLogoMarcaClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
  begin
    edtLogoMarca.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmDemo_ACBrNFSe.sbtnPathSalvarClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(edtPathLogs.Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := edtPathLogs.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], SELDIRHELP) then
    edtPathLogs.Text := Dir;
end;

procedure TfrmDemo_ACBrNFSe.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
begin
  PageControl1.ActivePageIndex := 0;

  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add(GetEnumName(TypeInfo(TSSLLib), integer(T)));
  cbSSLLib.ItemIndex := 0;

  cbCryptLib.Items.Clear;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add(GetEnumName(TypeInfo(TSSLCryptLib), integer(U)));
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Items.Clear;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add(GetEnumName(TypeInfo(TSSLHttpLib), integer(V)));
  cbHttpLib.ItemIndex := 0;

  cbXmlSignLib.Items.Clear;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add(GetEnumName(TypeInfo(TSSLXmlSignLib), integer(X)));
  cbXmlSignLib.ItemIndex := 0;

  cbSSLType.Items.Clear;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add(GetEnumName(TypeInfo(TSSLType), integer(Y)));
  cbSSLType.ItemIndex := 0;

  LerConfiguracao;
  CarregarIniCidades;
  ConfiguraComponente;
end;

procedure TfrmDemo_ACBrNFSe.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  LerConfiguracao;
  ConfiguraComponente;
end;

procedure TfrmDemo_ACBrNFSe.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmDemo_ACBrNFSe.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/35');
end;

procedure TfrmDemo_ACBrNFSe.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmDemo_ACBrNFSe.btnGerarEnviarLoteClick(Sender: TObject);
var
  sQtde, sAux, vNumLote: String;
  iQtde, iAux, I: Integer;
begin
  if not(InputQuery('Gerar e Enviar Lote', 'Quantidade de RPS', sQtde)) then
    exit;

  if not(InputQuery('Gerar e Enviar Lote', 'Numero do RPS', sAux)) then
    exit;

  if not(InputQuery('Gerar e Enviar Lote', 'Numero do Lote', vNumLote)) then
    exit;

  iQtde := StrToIntDef(sQtde, 1);
  iAux := StrToIntDef(sAux, 1);

  ACBrNFSe1.NotasFiscais.Clear;
  for I := 1 to iQtde do
  begin
    sAux := IntToStr(iAux);
    AlimentaComponente(sAux);
    inc(iAux);
  end;

  ACBrNFSe1.Enviar(vNumLote);

  for I := 0 to iQtde - 1 do
  begin
    MemoDados.Lines.Add('Nome XML: ' + ACBrNFSe1.NotasFiscais.Items[I].NomeArq);
    MemoDados.Lines.Add('Nota Numero: ' + ACBrNFSe1.NotasFiscais.Items[I]
      .NFSe.Numero);
    MemoDados.Lines.Add('CÛdigo de VerificaÁ„o: ' + ACBrNFSe1.NotasFiscais.Items
      [I].NFSe.CodigoVerificacao);
  end;

  ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmDemo_ACBrNFSe.btnConsultarLoteClick(Sender: TObject);
var
  Lote, Protocolo: string;
begin

  OpenDialog1.Title := 'Selecione o RPS';
  OpenDialog1.DefaultExt := '*-rps.xml';
  OpenDialog1.Filter :=
    'Arquivos RPS (*-rps.xml)|*-rps.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSe1.NotasFiscais.Clear;
    ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);
  end;

  if not (InputQuery('Consultar Lote', 'N√∫mero do Lote', Lote)) then
    exit;
  if not (InputQuery('Consultar Lote', 'N√∫mero do Protocolo', Protocolo)) then
    exit;

  ACBrNFSe1.ConsultarLoteRps(Lote, Protocolo);

  MemoResp.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsLote.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsLote.RetWS);
  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrNFSe.btnCancNFSeClick(Sender: TObject);
var
  Codigo, Motivo: string;
begin

  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSe1.NotasFiscais.Clear;
    ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);

    // Codigo de Cancelamento
    // 1 - Erro de emiss√£o
    // 2 - Servi√ßo n√£o concluido
    // 3 - RPS Cancelado na Emiss√£o

    if not (InputQuery('Cancelar NFSe', 'C√≥digo de Cancelamento', Codigo)) then
      exit;

    //Provedor Equiplano √© obrigat√≥rio o motivo de cancelamento
    //if not(InputQuery('Cancelar NFSe', 'Motivo de Cancelamento', Motivo))
    // then exit;
    //ACBrNFSe1.NotasFiscais.Items[0].NFSe.MotivoCancelamento:= Motivo;

    //   ACBrNFSe1.WebServices.CancelaNFSe(Codigo, '1', '03310700000170', '0306223', '0');
    ACBrNFSe1.CancelarNFSe(Codigo);

    MemoDados.Lines.Add('Arquivo Carregado de: ' +
      ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    MemoDados.Lines.Add('Retorno do Cancelamento:');

    MemoDados.Lines.Add('C√≥d. Cancelamento: ' +
      ACBrNFSe1.WebServices.CancNfse.CodigoCancelamento);
    if ACBrNFSe1.WebServices.CancNfse.DataHora <> 0 then
      MemoDados.Lines.Add('Data / Hora      : ' +
        DateTimeToStr(ACBrNFSe1.WebServices.CancNfse.DataHora));
    LoadXML(MemoResp, WBResposta);

    pgRespostas.ActivePageIndex := 1;
  end;

end;

procedure TfrmDemo_ACBrNFSe.btnConsultarSitLoteClick(Sender: TObject);
var
  Protocolo: string;
begin
  if not (InputQuery('Consultar Situa√ß√£o do Lote', 'N√∫mero do Protocolo',
    Protocolo)) then
    exit;

  ACBrNFSe1.ConsultarSituacao(edtEmitCNPJ.Text, edtEmitIM.Text);

  MemoResp.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsSitLoteRPS.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsSitLoteRPS.RetWS);
  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrNFSe.btnGerarRPSClick(Sender: TObject);
var
  vAux: string;
begin
  if not (InputQuery('Gerar RPS', 'Numero do RPS', vAux)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentaComponente(vAux);
  // ACBrNFSe1.NotasFiscais.Items[0].SaveToFile;

  ShowMessage('Arquivo gerado em: ' + ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
  MemoDados.Lines.Add('Arquivo gerado em: ' + ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
  LoadXML(MemoResp, WBResposta);

  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrNFSe.btnImprimirClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSe1.NotasFiscais.Clear;
    ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName, False);
    ACBrNFSe1.Configuracoes.Arquivos.NomeLongoNFSe := True;
    ACBrNFSe1.NotasFiscais.Imprimir;
    ACBrNFSe1.NotasFiscais.ImprimirPDF;

    MemoDados.Lines.Add('Arquivo Carregado de: ' +
      ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    MemoDados.Lines.Add('Nota Numero: ' + ACBrNFSe1.NotasFiscais.Items[0].NFSe.Numero);
    MemoDados.Lines.Add('C√≥digo de Verifica√ß√£o: ' +
      ACBrNFSe1.NotasFiscais.Items[0].NFSe.CodigoVerificacao);
    MemoDados.Lines.Add('Data de Emiss√£o: ' + DateToStr(
      ACBrNFSe1.NotasFiscais.Items[0].NFSe.DataEmissao));
    MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    LoadXML(MemoResp, WBResposta);
    pgRespostas.ActivePageIndex := 1;
  end;
end;

procedure TfrmDemo_ACBrNFSe.ACBrNFSe1StatusChange(Sender: TObject);
begin
  case ACBrNFSe1.Status of
    stNFSeIdle:
    begin
      if (frmStatus <> nil) then
        frmStatus.Hide;
    end;
    stNFSeRecepcao:
    begin
      if (frmStatus = nil) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando dados da NFSe...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stNFSeConsulta:
    begin
      if (frmStatus = nil) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Consultando...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stNFSeCancelamento:
    begin
      if (frmStatus = nil) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando cancelamento de NFSe...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
    stNFSeEmail:
    begin
      if (frmStatus = nil) then
        frmStatus := TfrmStatus.Create(Application);
      frmStatus.lblStatus.Caption := 'Enviando Email...';
      frmStatus.Show;
      frmStatus.BringToFront;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TfrmDemo_ACBrNFSe.ACBrNFSe1GerarLog(const Mensagem: string);
begin
  memoLog.Lines.Add(Mensagem);
end;

procedure TfrmDemo_ACBrNFSe.sbtSchemasClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(edtSchemas.Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := edtSchemas.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], SELDIRHELP) then
    edtSchemas.Text := Dir;
end;

procedure TfrmDemo_ACBrNFSe.sbtnPrestLogoClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Logo';
  OpenDialog1.DefaultExt := '*.bmp';
  OpenDialog1.Filter := 'Arquivos BMP (*.bmp)|*.bmp|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
  begin
    edtPrestLogo.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmDemo_ACBrNFSe.btnConsultarNFSeRPSClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Rps';
  OpenDialog1.DefaultExt := '*-Rps.xml';
  OpenDialog1.Filter :=
    'Arquivos Rps (*-Rps.xml)|*-Rps.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSe1.NotasFiscais.Clear;
    ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

    ACBrNFSe1.ConsultarNFSeporRps(
      ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero,
      ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie,
      TipoRPSToStr(ACBrNFSe1.NotasFiscais.Items[0].NFSe.IdentificacaoRps.Tipo));

    MemoResp.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsNfseRps.RetWS);
    memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsNfseRps.RetWS);
    LoadXML(MemoResp, WBResposta);
  end;
end;

procedure TfrmDemo_ACBrNFSe.btnConsultarNFSePeriodoClick(Sender: TObject);
var
  DataInicial, DataFinal: string;
begin
  if not (InputQuery('Consultar NFSe por PerÌodo', 'Data Inicial (DD/MM/AAAA):',
    DataInicial)) then
    exit;
  if not (InputQuery('Consultar NFSe por PerÌodo', 'Data Final (DD/MM/AAAA):',
    DataFinal)) then
    exit;

  ACBrNFSe1.ConsultarNFSe(StrToDate(DataInicial), StrToDate(DataFinal));

  MemoResp.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsNfse.RetWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrNFSe1.WebServices.ConsNfse.RetWS);
  LoadXML(MemoResp, WBResposta);
end;

procedure TfrmDemo_ACBrNFSe.cbCidadesChange(Sender: TObject);
var
  Tamanho: integer;
begin
  Tamanho := Length(Trim(cbCidades.Text));

  edtEmitCidade.Text := Copy(cbCidades.Text, 1, Tamanho - 11);
  edtEmitUF.Text := Copy(cbCidades.Text, Tamanho - 1, 2);
  edtCodCidade.Text := Copy(cbCidades.Text, Tamanho - 9, 7);
end;

// Fun√ß√£o criada para arredondar valores quando a n Casa for maior ou igual a 5
// pois a fun√ß√£o RoundTo arredonda quando a n Casa for maior ou igual a 6
function TfrmDemo_ACBrNFSe.RoundTo5(Valor: double; Casas: integer): double;
var
  xValor, xDecimais: string;
  p, nCasas: integer;
  nValor: double;
begin
  nValor := Valor;
  xValor := Trim(FloatToStr(Valor));
  p := pos(',', xValor);
  if Casas < 0 then
    nCasas := -Casas
  else
    nCasas := Casas;
  if p > 0 then
  begin
    xDecimais := Copy(xValor, p + 1, length(xValor));
    if length(xDecimais) > nCasas then
    begin
      if xDecimais[nCasas + 1] >= '5' then
        SetRoundMode(rmUP)
      else
        SetRoundMode(rmNearest);
    end;
    nValor := RoundTo(Valor, Casas);
  end;
  Result := nValor;
end;

procedure TfrmDemo_ACBrNFSe.btnGerarEnviarNFSeClick(Sender: TObject);
var
  vNumRPS, sNomeArq: string;
begin
  //**************************************************************************

  // A function Gerar s√≥ esta disponivel para alguns provedores.

  //**************************************************************************

  if not (InputQuery('Gerar e Enviar NFSe', 'Numero do RPS', vNumRPS)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentaComponente(vNumRPS);

  ACBrNFSe1.Gerar(StrToInt(vNumRPS));
  sNomeArq := ACBrNFSe1.NotasFiscais.Items[0].NomeArq;

  ACBrNFSe1.NotasFiscais.Clear;
  ACBrNFSe1.NotasFiscais.LoadFromFile(sNomeArq);
  ACBrNFSe1.NotasFiscais.Imprimir;

  MemoDados.Lines.Add('Arquivo Carregado de: ' +
    ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
  MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
  LoadXML(MemoResp, WBResposta);
  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrNFSe.btnEnviaremailClick(Sender: TObject);
var
  vAux: string;
  sCC: TStrings;
begin
  OpenDialog1.Title := 'Selecione a NFSe';
  OpenDialog1.DefaultExt := '*-NFSe.xml';
  OpenDialog1.Filter :=
    'Arquivos NFSe (*-NFSe.xml)|*-NFSe.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrNFSe1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    ACBrNFSe1.NotasFiscais.Clear;
    ACBrNFSe1.NotasFiscais.LoadFromFile(OpenDialog1.FileName);

    if not (InputQuery('Enviar e-mail', 'Destinat·rio', vAux)) then
      exit;

    sCC := TStringList.Create;
    sCC.Clear;  // Usando para add outros e-mail como CÙpia

    ACBrNFSe1.NotasFiscais.Items[0].EnviarEmail(vAux
      , edtEmailAssunto.Text
      , mmEmailMsg.Lines
      , False //Enviar PDF junto
      ,
      nil //Lista com emails que ser√£o enviado c√≥pias - TStrings
      , nil // Lista de anexos - TStrings
      );

    sCC.Free;

    MemoDados.Lines.Add('Arquivo Carregado de: ' +
      ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    MemoResp.Lines.LoadFromFile(ACBrNFSe1.NotasFiscais.Items[0].NomeArq);
    LoadXML(MemoResp, WBResposta);
    pgRespostas.ActivePageIndex := 1;
  end;
end;

procedure TfrmDemo_ACBrNFSe.btnLinkNFSeClick(Sender: TObject);
var
  vNumNFSe, sCodVerif, sIM, sLink: string;
begin
  if not (InputQuery('Gerar o Link da NFSe', 'Numero da NFSe', vNumNFSe)) then
    exit;
  if not (InputQuery('Gerar o Link da NFSe', 'Codigo de Verificacao', sCodVerif)) then
    exit;
  if not (InputQuery('Gerar o Link da NFSe', 'Inscri√ß√£o Municipal', sIM)) then
    exit;

  sLink := ACBrNFSe1.LinkNFSe(StrToIntDef(vNumNFSe, 0), sCodVerif);

  MemoResp.Lines.Add('Link Gerado: ' + sLink);
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmDemo_ACBrNFSe.btnGerarLoteRPSClick(Sender: TObject);
var
  vAux, vNumLote: string;
begin
  //**************************************************************************

  // A function GerarLote apenas gera o XML do lote, assina se necess√°rio
  // e valida, salvando o arquivo com o nome: <lote>-lot-rps.xml na pasta Ger
  // N√£o ocorre o envio para nenhum webservice.

  //**************************************************************************

  if not (InputQuery('Gerar e Enviar Lote', 'Numero do RPS', vAux)) then
    exit;

  if not (InputQuery('Gerar e Enviar Lote', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentaComponente(vAux);
  ACBrNFSe1.GerarLote(vNumLote);

  ShowMessage('Arquivo gerado em: ' + ACBrNFSe1.NotasFiscais.Items[0].NomeArq);

  ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmDemo_ACBrNFSe.btnGerarEnviarSincronoClick(Sender: TObject);
var
  vAux, vNumLote: string;
begin
  //**************************************************************************

  // A function EnviarSincrono s√≥ esta disponivel para alguns provedores.

  //**************************************************************************

  if not (InputQuery('Gerar e Enviar Lote - Sincrono', 'Numero do RPS', vAux)) then
    exit;

  if not (InputQuery('Gerar e Enviar Lote - Sincrono', 'Numero do Lote', vNumLote)) then
    exit;

  ACBrNFSe1.NotasFiscais.Clear;
  AlimentaComponente(vAux);
  ACBrNFSe1.EnviarSincrono(vNumLote);

  ACBrNFSe1.NotasFiscais.Clear;
end;

procedure TfrmDemo_ACBrNFSe.Button1Click(Sender: TObject);
var
  vAux, provedor: string;
begin
  if not (InputQuery('Informe o c√≥digo IBGE da cidade com 7 digitos',
    'C√≥digo:', vAux)) then
    exit;

  //provedor := CodCidadeToProvedor(StrToIntDef(vAux, 0));

  ShowMessage('Provedor: ' + provedor);
end;

procedure TfrmDemo_ACBrNFSe.btnSubsNFSeClick(Sender: TObject);
var
  Codigo, vAux, sNumNFSe: string;
begin
  if not (InputQuery('Substituir NFS-e', 'Numero do novo RPS', vAux)) then
    exit;
  ACBrNFSe1.NotasFiscais.Clear;
  AlimentaComponente(vAux);

  // Codigo de Cancelamento
  // 1 - Erro de emiss√£o
  // 2 - Servi√ßo n√£o concluido
  // 3 - RPS Cancelado na Emiss√£o

  if not (InputQuery('Substituir NFSe', 'C√≥digo de Cancelamento', Codigo)) then
    exit;

  if not (InputQuery('Substituir NFS-e', 'Numero da NFS-e', sNumNFSe)) then
    exit;

  ACBrNFSe1.SubstituirNFSe(Codigo, sNumNFSe);

  MemoDados.Lines.Add('Retorno da Substitui√ß√£o:');
  MemoDados.Lines.Add('C√≥d. Cancelamento: ' +
    ACBrNFSe1.WebServices.SubNfse.CodigoCancelamento);
  LoadXML(MemoResp, WBResposta);
  pgRespostas.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrNFSe.sbtArqINIClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(edtArqINI.Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := edtArqINI.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], SELDIRHELP) then
    edtArqINI.Text := Dir;
end;

end.
