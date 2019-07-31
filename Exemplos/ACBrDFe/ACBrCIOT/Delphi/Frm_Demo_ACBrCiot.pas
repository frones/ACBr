{$I ACBr.inc}

unit Frm_Demo_ACBrCIOT;

interface

uses
  IniFiles, ShellAPI,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, OleCtrls, SHDocVw, StdCtrls, Buttons, ExtCtrls,
  ACBrCIOT, ACBrMail, ACBrBase, ACBrDFe, ACBrDFeSSL, Spin;

type
  TfrmDemo_ACBrCIOT = class(TForm)
    pnlCIOT1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    btnSalvarConfig: TBitBtn;
    pnlCIOT2: TPanel;
    pnlCIOT3: TPanel;
    btnCriarEnviar: TButton;
    btnGerarCIOT: TButton;
    pcRespostas: TPageControl;
    tsRespostas: TTabSheet;
    MemoResp: TMemo;
    tsXMLResposta: TTabSheet;
    WBResposta: TWebBrowser;
    tsLog: TTabSheet;
    memoLog: TMemo;
    tsCIOT: TTabSheet;
    trvwCIOT: TTreeView;
    tsRetorno: TTabSheet;
    memoRespWS: TMemo;
    tsDados: TTabSheet;
    MemoDados: TMemo;
    OpenDialog1: TOpenDialog;
    btnEnviarCIOTEmail: TButton;
    ACBrMail1: TACBrMail;
    PageControl3: TPageControl;
    TabSheet11: TTabSheet;
    PageControl4: TPageControl;
    TabSheet12: TTabSheet;
    lSSLLib: TLabel;
    lCryptLib: TLabel;
    lHttpLib: TLabel;
    lXmlSign: TLabel;
    gbCertificado: TGroupBox;
    Label33: TLabel;
    Label34: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label35: TLabel;
    sbtnListaCert: TSpeedButton;
    sbtnGetCert: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    btnDataValidade: TButton;
    btnNumSerie: TButton;
    btnSubjectname: TButton;
    btnCNPJ: TButton;
    btnIssuerName: TButton;
    GroupBox6: TGroupBox;
    edtHash: TEdit;
    btnSHA256: TButton;
    cbAssinar: TCheckBox;
    btnHTTPS: TButton;
    btnLeituraX509: TButton;
    cbSSLLib: TComboBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbXmlSignLib: TComboBox;
    TabSheet13: TTabSheet;
    GroupBox7: TGroupBox;
    sbtnPathSalvar: TSpeedButton;
    Label36: TLabel;
    Label37: TLabel;
    Label39: TLabel;
    Label42: TLabel;
    sbPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    cbFormaEmissao: TComboBox;
    edtFormatoAlerta: TEdit;
    cbxRetirarAcentos: TCheckBox;
    cbVersaoDF: TComboBox;
    edtPathSchemas: TEdit;
    TabSheet14: TTabSheet;
    GroupBox8: TGroupBox;
    Label43: TLabel;
    lTimeOut: TLabel;
    lSSLLib1: TLabel;
    cbxVisualizar: TCheckBox;
    cbUF: TComboBox;
    rgTipoAmb: TRadioGroup;
    cbxSalvarSOAP: TCheckBox;
    seTimeOut: TSpinEdit;
    cbSSLType: TComboBox;
    GroupBox9: TGroupBox;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    gbxRetornoEnvio: TGroupBox;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    cbxAjustarAut: TCheckBox;
    edtTentativas: TEdit;
    edtIntervalo: TEdit;
    edtAguardar: TEdit;
    TabSheet15: TTabSheet;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
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
    TabSheet16: TTabSheet;
    sbPathCIOT: TSpeedButton;
    Label64: TLabel;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathCIOT: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathCIOT: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet17: TTabSheet;
    Label70: TLabel;
    sbtnLogoMarca: TSpeedButton;
    edtLogoMarca: TEdit;
    rgTipoDACIOT: TRadioGroup;
    TabSheet18: TTabSheet;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    edtSmtpHost: TEdit;
    edtSmtpPort: TEdit;
    edtSmtpUser: TEdit;
    edtSmtpPass: TEdit;
    edtEmailAssunto: TEdit;
    cbEmailSSL: TCheckBox;
    mmEmailMsg: TMemo;
    Label30: TLabel;
    edtUsuarioWebService: TEdit;
    Label31: TLabel;
    edtSenhaWebService: TEdit;
    rgOperacao: TRadioGroup;
    Label1: TLabel;
    edtHashIntegrador: TEdit;
    Label2: TLabel;
    cbbIntegradora: TComboBox;
    ACBrCIOT1: TACBrCIOT;
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnLogoMarcaClick(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnGerarCIOTClick(Sender: TObject);
    procedure ACBrCIOT1StatusChange(Sender: TObject);
    procedure btnEnviarCIOTEmailClick(Sender: TObject);
    procedure sbPathSchemasClick(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure btnDataValidadeClick(Sender: TObject);
    procedure btnNumSerieClick(Sender: TObject);
    procedure btnSubjectnameClick(Sender: TObject);
    procedure btnCNPJClick(Sender: TObject);
    procedure btnIssuerNameClick(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure sbtnListaCertClick(Sender: TObject);
    procedure btnSHA256Click(Sender: TObject);
    procedure btnHTTPSClick(Sender: TObject);
    procedure btnLeituraX509Click(Sender: TObject);
    procedure sbPathCIOTClick(Sender: TObject);
    procedure ACBrCIOT1GerarLog(const ALogLine: string; var Tratado: Boolean);
    {
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    }
  private
    { Private declarations }

    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure LoadXML(MyMemo: TMemo; MyWebBrowser: TWebBrowser);
    procedure GerarCIOT;
    procedure AtualizaSSLLibsCombo;

  public
    { Public declarations }
  end;

var
  frmDemo_ACBrCIOT: TfrmDemo_ACBrCIOT;

implementation

uses
 FileCtrl, DateUtils, TypInfo, Math, BlckSock, Grids, Unit2,
 ufrmStatus,
 pcnConversao, pcnConversaoCIOT,
 ACBrCIOTContratos, ACBrUtil; //, ACBrDFeConfiguracoes, ACBrDFeOpenSSL;

const
  SELDIRHELP = 1000;

{$R *.dfm}

(*
procedure TForm1.lblMouseEnter(Sender: TObject);
begin
 TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TForm1.lblMouseLeave(Sender: TObject);
begin
 TLabel(Sender).Font.Style := [fsBold];
end;
*)
procedure TfrmDemo_ACBrCIOT.GravarConfiguracao;
Var IniFile : String;
    Ini     : TIniFile;
    StreamMemo : TMemoryStream;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );
  try
      Ini.WriteInteger( 'Certificado','SSLLib' , cbSSLLib.ItemIndex);
      Ini.WriteInteger( 'Certificado','CryptLib' , cbCryptLib.ItemIndex);
      Ini.WriteInteger( 'Certificado','HttpLib' , cbHttpLib.ItemIndex);
      Ini.WriteInteger( 'Certificado','XmlSignLib' , cbXmlSignLib.ItemIndex);
      Ini.WriteString( 'Certificado','Caminho' ,edtCaminho.Text);
      Ini.WriteString( 'Certificado','Senha'   ,edtSenha.Text);
      Ini.WriteString( 'Certificado','NumSerie',edtNumSerie.Text);

      Ini.WriteString( 'Geral','FormatoAlerta'  ,edtFormatoAlerta.Text);
      Ini.WriteInteger( 'Geral','FormaEmissao',cbFormaEmissao.ItemIndex);
      Ini.WriteInteger( 'Geral','VersaoDF',cbVersaoDF.ItemIndex);
      Ini.WriteInteger( 'Geral','Integradorra',cbbIntegradora.ItemIndex);
      Ini.WriteString( 'Geral','UsuarioWebS'  ,edtUsuarioWebService.Text);
      Ini.WriteString( 'Geral','SenhaWebS'  ,edtSenhaWebService.Text);
      Ini.WriteString( 'Geral','HashIntegrador' ,edtHashIntegrador.Text);
      Ini.WriteBool(   'Geral','RetirarAcentos'      ,cbxRetirarAcentos.Checked);
      Ini.WriteBool(   'Geral','Salvar'      ,ckSalvar.Checked);
      Ini.WriteString( 'Geral','PathSalvar'  ,edtPathLogs.Text);
      Ini.WriteString( 'Geral','PathSchemas'  ,edtPathSchemas.Text);

      Ini.WriteString( 'WebService','UF'        ,cbUF.Text);
      Ini.WriteInteger( 'WebService','Ambiente'  ,rgTipoAmb.ItemIndex);
      Ini.WriteBool(   'WebService','Visualizar',cbxVisualizar.Checked);
      Ini.WriteBool(   'WebService','SalvarSOAP',cbxSalvarSOAP.Checked);
      Ini.WriteBool(   'WebService','AjustarAut',cbxAjustarAut.Checked);
      Ini.WriteString( 'WebService','Aguardar'    ,edtAguardar.Text);
      Ini.WriteString( 'WebService','Tentativas'  ,edtTentativas.Text);
      Ini.WriteString( 'WebService','Intervalo'  ,edtIntervalo.Text);
      Ini.WriteInteger( 'WebService','TimeOut'   ,seTimeOut.Value);
      Ini.WriteInteger( 'WebService','SSLType' , cbSSLType.ItemIndex);

      Ini.WriteString( 'Proxy','Host'   ,edtProxyHost.Text);
      Ini.WriteString( 'Proxy','Porta'  ,edtProxyPorta.Text);
      Ini.WriteString( 'Proxy','User'   ,edtProxyUser.Text);
      Ini.WriteString( 'Proxy','Pass'   ,edtProxySenha.Text);

      Ini.WriteBool(   'Arquivos','Salvar'          ,cbxSalvarArqs.Checked);
      Ini.WriteBool(   'Arquivos','PastaMensal'     ,cbxPastaMensal.Checked);
      Ini.WriteBool(   'Arquivos','AddLiteral'      ,cbxAdicionaLiteral.Checked);
      Ini.WriteBool(   'Arquivos','EmissaoPathCIOT'  ,cbxEmissaoPathCIOT.Checked);
      Ini.WriteBool(   'Arquivos','SepararPorCNPJ'  ,cbxSepararPorCNPJ.Checked);
      Ini.WriteBool(   'Arquivos','SepararPorModelo',cbxSepararPorModelo.Checked);
      Ini.WriteString( 'Arquivos','PathCIOT'    ,edtPathCIOT.Text);

      Ini.WriteString( 'Emitente','CNPJ'       ,edtEmitCNPJ.Text);
      Ini.WriteString( 'Emitente','IE'         ,edtEmitIE.Text);
      Ini.WriteString( 'Emitente','RazaoSocial',edtEmitRazao.Text);
      Ini.WriteString( 'Emitente','Fantasia'   ,edtEmitFantasia.Text);
      Ini.WriteString( 'Emitente','Fone'       ,edtEmitFone.Text);
      Ini.WriteString( 'Emitente','CEP'        ,edtEmitCEP.Text);
      Ini.WriteString( 'Emitente','Logradouro' ,edtEmitLogradouro.Text);
      Ini.WriteString( 'Emitente','Numero'     ,edtEmitNumero.Text);
      Ini.WriteString( 'Emitente','Complemento',edtEmitComp.Text);
      Ini.WriteString( 'Emitente','Bairro'     ,edtEmitBairro.Text);
      Ini.WriteString( 'Emitente','CodCidade'  ,edtEmitCodCidade.Text);
      Ini.WriteString( 'Emitente','Cidade'     ,edtEmitCidade.Text);
      Ini.WriteString( 'Emitente','UF'         ,edtEmitUF.Text);

      Ini.WriteString( 'Email','Host'    ,edtSmtpHost.Text);
      Ini.WriteString( 'Email','Port'    ,edtSmtpPort.Text);
      Ini.WriteString( 'Email','User'    ,edtSmtpUser.Text);
      Ini.WriteString( 'Email','Pass'    ,edtSmtpPass.Text);
      Ini.WriteString( 'Email','Assunto' ,edtEmailAssunto.Text);
      Ini.WriteBool(   'Email','SSL'     ,cbEmailSSL.Checked );
      StreamMemo := TMemoryStream.Create;
      mmEmailMsg.Lines.SaveToStream(StreamMemo);
      StreamMemo.Seek(0,soFromBeginning);
      Ini.WriteBinaryStream( 'Email','Mensagem',StreamMemo);
      StreamMemo.Free;

      Ini.WriteInteger( 'DACIOT','Tipo'       ,rgTipoDACIOT.ItemIndex);
      Ini.WriteString( 'DACIOT','LogoMarca'   ,edtLogoMarca.Text);

  finally
     Ini.Free;
  end;
end;

procedure TfrmDemo_ACBrCIOT.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  Ok: Boolean;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    cbSSLLib.ItemIndex     := Ini.ReadInteger( 'Certificado','SSLLib' ,0);
    cbCryptLib.ItemIndex   := Ini.ReadInteger( 'Certificado','CryptLib' , 0);
    cbHttpLib.ItemIndex    := Ini.ReadInteger( 'Certificado','HttpLib' , 0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger( 'Certificado','XmlSignLib' , 0);
    edtCaminho.Text        := Ini.ReadString( 'Certificado','Caminho' ,'');
    edtSenha.Text          := Ini.ReadString( 'Certificado','Senha'   ,'');
    edtNumSerie.Text       := Ini.ReadString( 'Certificado','NumSerie','');

    ACBrCIOT1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
    ACBrCIOT1.Configuracoes.Certificados.Senha       := edtSenha.Text;
    ACBrCIOT1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

    edtFormatoAlerta.Text     := Ini.ReadString( 'Geral','FormatoAlerta'  ,'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex  := Ini.ReadInteger( 'Geral','FormaEmissao',0);
    cbVersaoDF.ItemIndex      := Ini.ReadInteger( 'Geral','VersaoDF',0);
    cbbIntegradora.ItemIndex  := Ini.ReadInteger( 'Geral','Integradorra',1);
    edtUsuarioWebService.Text := Ini.ReadString( 'Geral','UsuarioWebS','');
    edtSenhaWebService.Text   := Ini.ReadString( 'Geral','SenhaWebS'  ,'');
    edtHashIntegrador.Text    := Ini.ReadString( 'Geral','HashIntegrador' ,'');
    ckSalvar.Checked          := Ini.ReadBool(   'Geral','Salvar'      ,True);
    edtPathLogs.Text          := Ini.ReadString( 'Geral','PathSalvar'  ,PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString( 'Geral','PathSchemas'  ,PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'{+GetEnumName(TypeInfo(TVersaoRegBol), integer(cbVersaoDF.ItemIndex) )});

    cbxRetirarAcentos.Checked := Ini.ReadBool(   'Geral','RetirarAcentos',True);

    ACBrCIOT1.SSL.DescarregarCertificado;

    with ACBrCIOT1.Configuracoes.Geral do
    begin
      SSLLib        := TSSLLib(cbSSLLib.ItemIndex);
      SSLCryptLib   := TSSLCryptLib(cbCryptLib.ItemIndex);
      SSLHttpLib    := TSSLHttpLib(cbHttpLib.ItemIndex);
      SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

      AtualizaSSLLibsCombo;

      RetirarAcentos := cbxRetirarAcentos.Checked;
      FormatoAlerta  := edtFormatoAlerta.Text;
      FormaEmissao   := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);
      VersaoDF       := TVersaoCIOT(cbVersaoDF.ItemIndex);
      Integradora    := StrToEnumIntegradora(Ok, cbbIntegradora.text);
      Usuario        := edtUsuarioWebService.Text;
      Senha          := edtSenhaWebService.Text;
      HashIntegrador := edtHashIntegrador.Text;
      Salvar         := ckSalvar.Checked;
    end;

    cbUF.ItemIndex        := cbUF.Items.IndexOf(Ini.ReadString( 'WebService','UF','SP'));
    rgTipoAmb.ItemIndex   := Ini.ReadInteger( 'WebService','Ambiente'  ,0);
    cbxVisualizar.Checked := Ini.ReadBool(    'WebService','Visualizar',False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(    'WebService','SalvarSOAP',False);
    cbxAjustarAut.Checked := Ini.ReadBool(   'WebService','AjustarAut' ,False);
    edtAguardar.Text      := Ini.ReadString( 'WebService','Aguardar'  ,'0');
    edtTentativas.Text    := Ini.ReadString( 'WebService','Tentativas','5');
    edtIntervalo.Text     := Ini.ReadString( 'WebService','Intervalo' ,'0');
    seTimeOut.Value       := Ini.ReadInteger('WebService','TimeOut'  ,5000);
    cbSSLType.ItemIndex   := Ini.ReadInteger('WebService','SSLType' , 0);

    edtProxyHost.Text  := Ini.ReadString( 'Proxy','Host'   ,'');
    edtProxyPorta.Text := Ini.ReadString( 'Proxy','Porta'  ,'');
    edtProxyUser.Text  := Ini.ReadString( 'Proxy','User'   ,'');
    edtProxySenha.Text := Ini.ReadString( 'Proxy','Pass'   ,'');

    with ACBrCIOT1.Configuracoes.WebServices do
    begin
      UF         := cbUF.Text;
      Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
      Visualizar := cbxVisualizar.Checked;
      Salvar     := cbxSalvarSOAP.Checked;

      AjustaAguardaConsultaRet := cbxAjustarAut.Checked;

      if NaoEstaVazio(edtAguardar.Text)then
        AguardarConsultaRet := ifThen(StrToInt(edtAguardar.Text)<1000,StrToInt(edtAguardar.Text)*1000,StrToInt(edtAguardar.Text))
      else
        edtAguardar.Text := IntToStr(AguardarConsultaRet);

      if NaoEstaVazio(edtTentativas.Text) then
        Tentativas := StrToInt(edtTentativas.Text)
      else
        edtTentativas.Text := IntToStr(Tentativas);

      if NaoEstaVazio(edtIntervalo.Text) then
        IntervaloTentativas := ifThen(StrToInt(edtIntervalo.Text)<1000,StrToInt(edtIntervalo.Text)*1000,StrToInt(edtIntervalo.Text))
      else
        edtIntervalo.Text := IntToStr(ACBrCIOT1.Configuracoes.WebServices.IntervaloTentativas);

      TimeOut := seTimeOut.Value;

      ProxyHost := edtProxyHost.Text;
      ProxyPort := edtProxyPorta.Text;
      ProxyUser := edtProxyUser.Text;
      ProxyPass := edtProxySenha.Text;
    end;

    ACBrCIOT1.SSL.SSLType := TSSLType( cbSSLType.ItemIndex );

    cbxSalvarArqs.Checked        := Ini.ReadBool(   'Arquivos','Salvar'     ,false);
    cbxPastaMensal.Checked       := Ini.ReadBool(   'Arquivos','PastaMensal',false);
    cbxAdicionaLiteral.Checked   := Ini.ReadBool(   'Arquivos','AddLiteral' ,false);
    cbxEmissaoPathCIOT.Checked   := Ini.ReadBool(   'Arquivos','EmissaoPathCIOT',false);
    cbxSepararPorCNPJ.Checked    := Ini.ReadBool(   'Arquivos','SepararPorCNPJ',false);
    cbxSepararPorModelo.Checked  := Ini.ReadBool(   'Arquivos','SepararPorModelo',false);
    edtPathCIOT.Text             := Ini.ReadString( 'Arquivos','PathCIOT'    ,'');

    with ACBrCIOT1.Configuracoes.Arquivos do
    begin
      Salvar            := cbxSalvarArqs.Checked;
      SepararPorMes     := cbxPastaMensal.Checked;
      AdicionarLiteral  := cbxAdicionaLiteral.Checked;
      EmissaoPathCIOT   := cbxEmissaoPathCIOT.Checked;
      SepararPorCNPJ    := cbxSepararPorCNPJ.Checked;
      SepararPorModelo  := cbxSepararPorModelo.Checked;
      PathSalvar        := edtPathLogs.Text;
      PathSchemas       := edtPathSchemas.Text;
      PathCIOT          := edtPathCIOT.Text;
    end;

    edtEmitCNPJ.Text       := Ini.ReadString( 'Emitente','CNPJ'       ,'');
    edtEmitIE.Text         := Ini.ReadString( 'Emitente','IE'         ,'');
    edtEmitRazao.Text      := Ini.ReadString( 'Emitente','RazaoSocial','');
    edtEmitFantasia.Text   := Ini.ReadString( 'Emitente','Fantasia'   ,'');
    edtEmitFone.Text       := Ini.ReadString( 'Emitente','Fone'       ,'');
    edtEmitCEP.Text        := Ini.ReadString( 'Emitente','CEP'        ,'');
    edtEmitLogradouro.Text := Ini.ReadString( 'Emitente','Logradouro' ,'');
    edtEmitNumero.Text     := Ini.ReadString( 'Emitente','Numero'     ,'');
    edtEmitComp.Text       := Ini.ReadString( 'Emitente','Complemento','');
    edtEmitBairro.Text     := Ini.ReadString( 'Emitente','Bairro'     ,'');
    edtEmitCodCidade.Text  := Ini.ReadString( 'Emitente','CodCidade'  ,'');
    edtEmitCidade.Text     :=Ini.ReadString(  'Emitente','Cidade'     ,'');
    edtEmitUF.Text         := Ini.ReadString( 'Emitente','UF'         ,'');

    edtSmtpHost.Text      := Ini.ReadString( 'Email','Host'   ,'');
    edtSmtpPort.Text      := Ini.ReadString( 'Email','Port'   ,'');
    edtSmtpUser.Text      := Ini.ReadString( 'Email','User'   ,'');
    edtSmtpPass.Text      := Ini.ReadString( 'Email','Pass'   ,'');
    edtEmailAssunto.Text  := Ini.ReadString( 'Email','Assunto','');
    cbEmailSSL.Checked    := Ini.ReadBool(   'Email','SSL'    ,False);

    StreamMemo := TMemoryStream.Create;
    Ini.ReadBinaryStream( 'Email','Mensagem',StreamMemo);
    mmEmailMsg.Lines.LoadFromStream(StreamMemo);
    StreamMemo.Free;

    rgTipoDACIOT.ItemIndex := Ini.ReadInteger('DACIOT', 'Tipo'     , 0);
    edtLogoMarca.Text      := Ini.ReadString( 'DACIOT', 'LogoMarca', '');
      (*
      if ACBrRegBol1.DANFE <> nil then
       begin
         ACBrRegBol1.DANFE.TipoDANFE  := StrToTpImp(OK,IntToStr(rgTipoDanfe.ItemIndex+1));
         ACBrRegBol1.DANFE.Logo       := edtLogoMarca.Text;
       end;
      *)
  finally
    Ini.Free;
  end;
end;

procedure TfrmDemo_ACBrCIOT.LoadXML(MyMemo: TMemo;
  MyWebBrowser: TWebBrowser);
begin
  MyMemo.Lines.SaveToFile(ExtractFileDir(application.ExeName)+'temp.xml');
  MyWebBrowser.Navigate(ExtractFileDir(application.ExeName)+'temp.xml');
end;

procedure TfrmDemo_ACBrCIOT.GerarCIOT;
begin
  with ACBrCIOT1.Contratos.Add.CIOT do
  begin
    case rgOperacao.ItemIndex of
      0: Integradora.Operacao := opObterPdf; //Busca e retorna uma Operação de Transporte em PDF.
      1: Integradora.Operacao := opAdicionar; //Adicionar uma operação de transporte
      2: Integradora.Operacao := opRetificar; //Retificar uma operação de transporte.
      3: Integradora.Operacao := opCancelar; //Cancelar uma operação de transporte
      4: Integradora.Operacao := opAdicionarViagem; //Adicionar uma Viagem a uma Operação de Transporte existente, desde que a mesma não tenha ultrapassado o prazo do fim da viagem, esteja cancelada ou encerrada.
      5: Integradora.Operacao := opAdicionarPagamento; //Adicionar um registro para Pagamentos em uma Operação de Transporte.
      6: Integradora.Operacao := opCancelarPagamento; //Cancelar um pagamento programado para uma operação de transporte.
      7: Integradora.Operacao := opEncerrar; //Encerrar uma operação de transporte existente que não esteja cancelada.
    end;

    (****************  DADOS DO CONTRATO  **************)
    OperacaoTransporte.TipoViagem := TAC_Agregado;
    OperacaoTransporte.Integrador := edtHashIntegrador.text;
    OperacaoTransporte.EmissaoGratuita := True;
    OperacaoTransporte.BloquearNaoEquiparado := False;
    OperacaoTransporte.MatrizCNPJ := edtEmitCNPJ.text;
    OperacaoTransporte.FilialCNPJ := '';
    OperacaoTransporte.IdOperacaoCliente := '1'; //Id / Chave primária da Tabela do banco de dados do CIOT
    OperacaoTransporte.DataInicioViagem := Now;
    OperacaoTransporte.DataFimViagem := Now;
    OperacaoTransporte.CodigoNCMNaturezaCarga := 0;
    OperacaoTransporte.PesoCarga := 10;
    OperacaoTransporte.TipoEmbalagem := Nenhum; //utilizado somente para as viagens do tipo Padrão

    //Somente para TipoViagem TAC_Agregado
    with OperacaoTransporte.Viagens.Add do
    begin
      DocumentoViagem := 'CTe';
      CodigoMunicipioOrigem := 4212908; //Pinhalzinho SC
      CodigoMunicipioDestino := 4217303; //Saudades SC

      Valores.TotalOperacao := 50;
      Valores.TotalViagem := 50;
      Valores.TotalDeAdiantamento := 10;
      Valores.TotalDeQuitacao := 10;
      Valores.Combustivel := 20;
      Valores.Pedagio := 10;
      Valores.OutrosCreditos := 1;
      Valores.JustificativaOutrosCreditos := 'Teste';
      Valores.Seguro := 10;
      Valores.OutrosDebitos := 1;
      Valores.JustificativaOutrosDebitos := 'Teste outros Debitos';

      TipoPagamento := TransferenciaBancaria;

      with NotasFiscais.Add do
      begin
        Numero := '12345';
        Serie := '1';
        Data := Date;
        ValorTotal := 100;

        ValorDaMercadoriaPorUnidade := 100;
        CodigoNCMNaturezaCarga := 5501;
        DescricaoDaMercadoria := 'Produto Teste';
        UnidadeDeMedidaDaMercadoria := umKg;
        TipoDeCalculo := SemQuebra;
        ValorDoFretePorUnidadeDeMercadoria := 0; //Se tiver quebra deve ser informado
        QuantidadeDaMercadoriaNoEmbarque := 1;

        ToleranciaDePerdaDeMercadoria.Tipo := tpPorcentagem;
        ToleranciaDePerdaDeMercadoria.Valor := 2; //Valor da tolerância admitido.

        DiferencaDeFrete.Tipo := Integral;
        DiferencaDeFrete.Base := QuantidadeDesembarque;

        DiferencaDeFrete.Tolerancia.Tipo := tpPorcentagem;
        DiferencaDeFrete.Tolerancia.Valor := 5; //Valor da tolerância admitido(Nenhum: 0; Porcentagem: 0.00 – 100.00; Absoluto: Livre)

        DiferencaDeFrete.MargemGanho.Tipo := tpPorcentagem;
        DiferencaDeFrete.MargemGanho.Valor := 5;

        DiferencaDeFrete.MargemPerda.Tipo := tpPorcentagem;
        DiferencaDeFrete.MargemPerda.Valor := 5;
      end;
    end;

    //Não esperado para TipoViagem Frota.
    with OperacaoTransporte.Impostos do
    begin
      IRRF := 0;
      SestSenat := 0;
      INSS := 0;
      ISSQN := 0;
      OutrosImpostos := 0;
      DescricaoOutrosImpostos := '';
    end;

    with OperacaoTransporte.Pagamentos.Add do
    begin
      IdPagamentoCliente := '1';
      DataDeLiberacao := Date;
      Valor := 10;
      TipoPagamento := TransferenciaBancaria; //TransferenciaBancaria(EmissaoGratuita = true); eFRETE (EmissaoGratuita = false)
      Categoria := tcpSemCategoria;//Para os TipoViagem Frota e TAC_Agregado são suportadas as Categorias Frota e SemCategoria. Para o TipoViagem Padrão todas as categorias são suportadas.
      Documento := ''; //Documento relacionado a viagem.
      InformacaoAdicional := '';
      //CNPJ que deve ser gerada a Nota Fiscal do abastecimento,
      //sendo da mesma raíz do CNPJ da matriz do contratante,
      //apenas aplicável para Categoria Frota (Abastecimento)
      CnpjFilialAbastecimento := OperacaoTransporte.MatrizCNPJ;

      InformacoesBancarias.InstituicaoBancaria := '756'; //Bancoob
      InformacoesBancarias.Agencia := '';
      InformacoesBancarias.Conta := '';
    end;

    //TAC ou seu equiparado, que efetuar o transporte rodoviário de cargas por
    //conta de terceiros e mediante remuneração, indicado no cadastramento da Operação de Transporte.
    //Para o TipoViagem Frota o Contratado será a própria empresa que está declarando a operação.
    with OperacaoTransporte.Contratado do
    begin
      CpfOuCnpj := '12345678910';
      RNTRC := '12345678';
    end;

    with OperacaoTransporte.Motorista do
    begin
      CpfOuCnpj := '12345678910';
      CNH := '12345678910';
      Celular.DDD := 49;
      Celular.Numero := 123456789;
    end;

    //Destinatário da carga.
    //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
    //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
    //Não esperado para TipoViagem Frota.
    with OperacaoTransporte.Destinatario do
    begin
      NomeOuRazaoSocial := '';
      CpfOuCnpj := '';

      EMail := '';
      ResponsavelPeloPagamento := False;

      Endereco.Bairro := 'teste';
      Endereco.Rua := '';
      Endereco.Numero := '';
      Endereco.Complemento := '';
      Endereco.CEP := '';
      Endereco.CodigoMunicipio := 0;

      Telefones.Celular.DDD := 0;
      Telefones.Celular.Numero := 0;

      Telefones.Fixo.DDD := 0;
      Telefones.Fixo.Numero := 0;

      Telefones.Fax.DDD := 0;
      Telefones.Fax.Numero := 0;
    end;

    with OperacaoTransporte.Contratante do
    begin
      RNTRC := '12345678';
      NomeOuRazaoSocial := 'teste';
      CpfOuCnpj := '12345678910';

      EMail := 'teste@teste.com.br';
      ResponsavelPeloPagamento := False;

      Endereco.Bairro := 'Bela Vista';
      Endereco.Rua := 'Rua Vitória';
      Endereco.Numero := '';
      Endereco.Complemento := '';
      Endereco.CEP := '89870000';
      Endereco.CodigoMunicipio := 4212908;

      Telefones.Celular.DDD := 0;
      Telefones.Celular.Numero := 0;

      Telefones.Fixo.DDD := 49;
      Telefones.Fixo.Numero := 33661011;

      Telefones.Fax.DDD := 0;
      Telefones.Fax.Numero := 0;
    end;

    //É o transportador que contratar outro transportador para realização do
    //transporte de cargas para o qual fora anteriormente contratado,
    //indicado no cadastramento da Operação de Transporte.
    //Não esperado para TipoViagem Frota.
    with OperacaoTransporte.Subcontratante do
    begin
      NomeOuRazaoSocial := '';
      CpfOuCnpj := '';

      EMail := '';
      ResponsavelPeloPagamento := False;

      Endereco.Bairro := '';
      Endereco.Rua := '';
      Endereco.Numero := '';
      Endereco.Complemento := '';
      Endereco.CEP := '';
      Endereco.CodigoMunicipio := 0;

      Telefones.Celular.DDD := 0;
      Telefones.Celular.Numero := 0;

      Telefones.Fixo.DDD := 0;
      Telefones.Fixo.Numero := 0;

      Telefones.Fax.DDD := 0;
      Telefones.Fax.Numero := 0;
    end;

    // Aquele que receberá as mercadorias transportadas em consignação,
    //indicado no cadastramento da Operação de Transporte ou nos respectivos documentos fiscais.
    //Não esperado para TipoViagem Frota
    with OperacaoTransporte.Consignatario do
    begin
      NomeOuRazaoSocial := '';
      CpfOuCnpj := '';

      EMail := '';
      ResponsavelPeloPagamento := False;

      Endereco.Bairro := '';
      Endereco.Rua := '';
      Endereco.Numero := '';
      Endereco.Complemento := '';
      Endereco.CEP := '';
      Endereco.CodigoMunicipio := 0;

      Telefones.Celular.DDD := 0;
      Telefones.Celular.Numero := 0;

      Telefones.Fixo.DDD := 0;
      Telefones.Fixo.Numero := 0;

      Telefones.Fax.DDD := 0;
      Telefones.Fax.Numero := 0;
    end;

    //Pessoa (física ou jurídica) que contratou o frete pela transportadora.
    //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
    //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
    with OperacaoTransporte.TomadorServico do
    begin
      NomeOuRazaoSocial := '';
      CpfOuCnpj := '';

      EMail := '';
      ResponsavelPeloPagamento := False;

      Endereco.Bairro := '';
      Endereco.Rua := '';
      Endereco.Numero := '';
      Endereco.Complemento := '';
      Endereco.CEP := '';
      Endereco.CodigoMunicipio := 0;

      Telefones.Celular.DDD := 0;
      Telefones.Celular.Numero := 0;

      Telefones.Fixo.DDD := 0;
      Telefones.Fixo.Numero := 0;

      Telefones.Fax.DDD := 0;
      Telefones.Fax.Numero := 0;
    end;

    with OperacaoTransporte.Veiculos.Add do
    begin
      Placa := 'AAA1234';
    end;

    //Informar um CIOT (se existente) que esteja relacionado à operação de transporte.
    //Por exemplo: No caso da presença de um Subcontratante na operação de transporte
    //informar o CIOT onde o Subcontratante foi o Contratado
    OperacaoTransporte.CodigoIdentificacaoOperacaoPrincipal := '';

    OperacaoTransporte.ObservacoesAoTransportador := 'teste de obsevação ao transportador';
    OperacaoTransporte.ObservacoesAoCredenciado := 'teste de obsevação ao Credenciado';
    OperacaoTransporte.EntregaDocumentacao := ''; //Ver como funciona
    OperacaoTransporte.QuantidadeSaques := 0; //Quantidade saques que serão realizados pelo Contratado na operação de transporte.
    OperacaoTransporte.QuantidadeTransferencias := 0; //Quantidade de Transferências  Bancárias que serão solicitadas pelo Contratado na operação de transporte.
  end;

//  ACBrCIOT1.Contratos.GerarCIOT;
end;

procedure TfrmDemo_ACBrCIOT.sbtnCaminhoCertClick(Sender: TObject);
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

procedure TfrmDemo_ACBrCIOT.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrCIOT1.SSL.SelecionarCertificado;
end;

procedure TfrmDemo_ACBrCIOT.sbtnLogoMarcaClick(Sender: TObject);
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

procedure TfrmDemo_ACBrCIOT.sbtnPathSalvarClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(edtPathLogs.Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := edtPathLogs.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
    edtPathLogs.Text := Dir;
end;

procedure TfrmDemo_ACBrCIOT.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoCIOT;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  Integradora: TCIOTIntegradora;
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
//  cbFormaEmissao.Items[0] := 'teNormal';
  cbFormaEmissao.ItemIndex := 0;

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoCIOT) to High(TVersaoCIOT) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoCIOT), integer(K) ) );
//  cbVersaoDF.Items[0] := 've120';
  cbVersaoDF.ItemIndex := 0;

  cbbIntegradora.Items.Clear;
  for Integradora := Low(TCIOTIntegradora) to High(TCIOTIntegradora) do
     cbbIntegradora.Items.Add( GetEnumName(TypeInfo(TCIOTIntegradora), integer(Integradora) ) );
//  cbVersaoDF.Items[0] := 've120';
  cbbIntegradora.ItemIndex := 0;

  LerConfiguracao;

  ACBrCIOT1.Configuracoes.WebServices.Salvar := true;
end;

procedure TfrmDemo_ACBrCIOT.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  LerConfiguracao;
end;

procedure TfrmDemo_ACBrCIOT.lblColaboradorClick(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/5', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrCIOT.lblPatrocinadorClick(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/35', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrCIOT.lblDoar1Click(Sender: TObject);
begin
  ShellExecute(0, Nil, 'http://acbr.sourceforge.net/drupal/?q=node/14', Nil, Nil, SW_NORMAL);
end;

procedure TfrmDemo_ACBrCIOT.ACBrCIOT1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
end;

procedure TfrmDemo_ACBrCIOT.ACBrCIOT1StatusChange(Sender: TObject);
begin
  case ACBrCIOT1.Status of
    stCIOTIdle:
      begin
        if frmStatus <> nil then
          frmStatus.Hide;
      end;
    stCIOTEnviar:
      begin
        if frmStatus = nil then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados do Contrato...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;
    stCIOTRetEnviar:
      begin
        if frmStatus = nil then
          frmStatus := TfrmStatus.Create(Application);

          frmStatus.lblStatus.Caption := 'Recebendo dados do CIOT...';
          frmStatus.Show;
          frmStatus.BringToFront;
      end;
    stCIOTEmail:
      begin
        if frmStatus = nil then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando CIOT por e-mail...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;
  end;
  Application.ProcessMessages;
end;

procedure TfrmDemo_ACBrCIOT.btnGerarCIOTClick(Sender: TObject);
var
 vAux : String;
begin
 if not(InputQuery('WebServices Enviar', 'ID do Contrato', vAux))
  then exit;

 ACBrCIOT1.Contratos.Clear;
 GerarCIOT;
 ACBrCIOT1.Contratos.Items[0].GravarXML('', '');

 ShowMessage('Arquivo gerado em: '+ACBrCIOT1.Contratos.Items[0].NomeArq);
 MemoDados.Lines.Add('Arquivo gerado em: '+ACBrCIOT1.Contratos.Items[0].NomeArq);
 MemoResp.Lines.LoadFromFile(ACBrCIOT1.Contratos.Items[0].NomeArq);
 LoadXML(MemoResp, WBResposta);
 pcRespostas.ActivePageIndex := 1;
end;

procedure TfrmDemo_ACBrCIOT.btnCriarEnviarClick(Sender: TObject);
begin
   ACBrCIOT1.Contratos.Clear;
   GerarCIOT;
   ACBrCIOT1.Enviar;

   MemoResp.Lines.Text   := UTF8Encode(ACBrCIOT1.WebServices.CIOTEnviar.RetWS);
   memoRespWS.Lines.Text := UTF8Encode(ACBrCIOT1.WebServices.CIOTEnviar.RetWS);
   LoadXML(MemoResp, WBResposta);
 {
 pcRespostas.ActivePageIndex := 5;
 MemoDados.Lines.Add('');
 MemoDados.Lines.Add('Retorno da solicitação de Averbação');
 MemoDados.Lines.Add('Numero   : '+ ACBrCIOT1.WebServices.CIOTEnviar.RetornoEnvio.Numero);
 MemoDados.Lines.Add('Serie    : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Serie);
 MemoDados.Lines.Add('Filial   : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Filial);
 MemoDados.Lines.Add('CNPJ     : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.CNPJCli);
 MemoDados.Lines.Add('Tipo Doc : '+ IntToStr(ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.TpDoc));
 MemoDados.Lines.Add('Inf Adic : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.InfAdic);
 MemoDados.Lines.Add('Averbado : '+ DateTimeToStr(ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.dhAverbacao));
 MemoDados.Lines.Add('Protocolo: '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.Protocolo);
 MemoDados.Lines.Add(' ');
 MemoDados.Lines.Add('Dados do Seguro');
 MemoDados.Lines.Add(' ');
 for i := 0 to ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro.Count -1 do
 begin
   MemoDados.Lines.Add('Numero Averbação: '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].NumeroAverbacao);
   MemoDados.Lines.Add('CNPJ Seguradora : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].CNPJSeguradora);
   MemoDados.Lines.Add('Nome Seguradora : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].NomeSeguradora);
   MemoDados.Lines.Add('Numero Apolice  : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].NumApolice);
   MemoDados.Lines.Add('Tipo Mov        : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].TpMov);
   MemoDados.Lines.Add('Tipo DDR        : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].TpDDR);
   MemoDados.Lines.Add('Valor Averbado  : '+ FloatToStr(ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].ValorAverbado));
   MemoDados.Lines.Add('Ramo Averbado   : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Averbado.DadosSeguro[i].RamoAverbado);
 end;
 MemoDados.Lines.Add(' ');
 MemoDados.Lines.Add('Informações');
 MemoDados.Lines.Add(' ');
 for i := 0 to ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Infos.Info.Count -1 do
 begin
   MemoDados.Lines.Add('Codigo   : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Infos.Info[i].Codigo);
   MemoDados.Lines.Add('Descrição: '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Infos.Info[i].Descricao);
 end;
 MemoDados.Lines.Add(' ');
 MemoDados.Lines.Add('Erros');
 MemoDados.Lines.Add(' ');
 for i := 0 to ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Erros.Erro.Count -1 do
 begin
   MemoDados.Lines.Add('Codigo         : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Erros.Erro[i].Codigo);
   MemoDados.Lines.Add('Descrição      : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Erros.Erro[i].Descricao);
   MemoDados.Lines.Add('Valor Esperado : '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Erros.Erro[i].ValorEsperado);
   MemoDados.Lines.Add('Valor Informado: '+ ACBrCIOT1.WebServices.RegBolAverbar.RegBolRetorno.Erros.Erro[i].ValorInformado);
 end;
 }
  ACBrCIOT1.Contratos.Clear;
end;

procedure TfrmDemo_ACBrCIOT.btnEnviarCIOTEmailClick(Sender: TObject);
var
 Para : String;
 CC   : Tstrings;
begin
 if not(InputQuery('Enviar Email', 'Email de destino', Para))
  then exit;

  OpenDialog1.Title := 'Selecione o CIOT';
  OpenDialog1.DefaultExt := '*-CIOT.xml';
  OpenDialog1.Filter := 'Arquivos RegBol (*-CIOT.xml)|*-CIOT.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCIOT1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
   ACBrCIOT1.Contratos.Clear;
   ACBrCIOT1.Contratos.LoadFromFile(OpenDialog1.FileName);
   CC:=TstringList.Create;
   CC.Add('email_1@provedor.com'); //especifique um email válido
   CC.Add('email_2@provedor.com.br'); //especifique um email válido

   ACBrCIOT1.Contratos.Items[0].EnviarEmail(Para
                                             , edtEmailAssunto.Text
                                             , mmEmailMsg.Lines
                                             , False //Enviar PDF junto
                                             , nil //Lista com emails que serão enviado cópias - TStrings
                                             , nil // Lista de RegBolxos - TStrings
                                              );

   CC.Free;
  end;
end;

procedure TfrmDemo_ACBrCIOT.sbPathSchemasClick(Sender: TObject);
var
 Dir : string;
begin
 if Length(edtPathSchemas.Text) <= 0
  then Dir := ExtractFileDir(application.ExeName)
  else Dir := edtPathSchemas.Text;

 if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP)
  then edtPathSchemas.Text := Dir;
end;

procedure TfrmDemo_ACBrCIOT.AtualizaSSLLibsCombo;
begin
 cbSSLLib.ItemIndex := Integer( ACBrCIOT1.Configuracoes.Geral.SSLLib );
 cbCryptLib.ItemIndex := Integer( ACBrCIOT1.Configuracoes.Geral.SSLCryptLib );
 cbHttpLib.ItemIndex := Integer( ACBrCIOT1.Configuracoes.Geral.SSLHttpLib );
 cbXmlSignLib.ItemIndex := Integer( ACBrCIOT1.Configuracoes.Geral.SSLXmlSignLib );

 cbSSLType.Enabled := (ACBrCIOT1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmDemo_ACBrCIOT.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCIOT.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCIOT.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCIOT.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizaSSLLibsCombo;
  end;
end;

procedure TfrmDemo_ACBrCIOT.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage( FormatDateBr(ACBrCIOT1.SSL.CertDataVenc) );
end;

procedure TfrmDemo_ACBrCIOT.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage( ACBrCIOT1.SSL.CertNumeroSerie );
end;

procedure TfrmDemo_ACBrCIOT.btnSubjectnameClick(Sender: TObject);
begin
  ShowMessage( ACBrCIOT1.SSL.CertSubjectName + sLineBreak + sLineBreak +
               'Razão Social: '+ACBrCIOT1.SSL.CertRazaoSocial);
end;

procedure TfrmDemo_ACBrCIOT.btnCNPJClick(Sender: TObject);
begin
  ShowMessage( ACBrCIOT1.SSL.CertCNPJ );
end;

procedure TfrmDemo_ACBrCIOT.btnIssuerNameClick(Sender: TObject);
begin
 ShowMessage( ACBrCIOT1.SSL.CertIssuerName + sLineBreak + sLineBreak +
              'Certificadora: '+ACBrCIOT1.SSL.CertCertificadora);
end;

procedure TfrmDemo_ACBrCIOT.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrCIOT1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmDemo_ACBrCIOT.sbtnListaCertClick(Sender: TObject);
var
  I: Integer;
  ASerie: String;
  AddRow: Boolean;
begin
  frSelecionarCertificado := TfrSelecionarCertificado.Create(Self);
  try
    ACBrCIOT1.SSL.LerCertificadosStore;
    AddRow := False;

    with frSelecionarCertificado.StringGrid1 do
    begin
      ColWidths[0] := 220;
      ColWidths[1] := 250;
      ColWidths[2] := 120;
      ColWidths[3] := 80;
      ColWidths[4] := 150;
      Cells[ 0, 0 ] := 'Num.Série';
      Cells[ 1, 0 ] := 'Razão Social';
      Cells[ 2, 0 ] := 'CNPJ';
      Cells[ 3, 0 ] := 'Validade';
      Cells[ 4, 0 ] := 'Certificadora';
    end;

    For I := 0 to ACBrCIOT1.SSL.ListaCertificados.Count-1 do
    begin
      with ACBrCIOT1.SSL.ListaCertificados[I] do
      begin
        ASerie := NumeroSerie;
        if (CNPJ <> '') then
        begin
          with frSelecionarCertificado.StringGrid1 do
          begin
            if Addrow then
              RowCount := RowCount + 1;
              
            Cells[ 0, RowCount-1] := NumeroSerie;
            Cells[ 1, RowCount-1] := RazaoSocial;
            Cells[ 2, RowCount-1] := CNPJ;
            Cells[ 3, RowCount-1] := FormatDateBr(DataVenc);
            Cells[ 4, RowCount-1] := Certificadora;
            AddRow := True;
          end;
        end;
      end;
    end;

    frSelecionarCertificado.ShowModal;

    if frSelecionarCertificado.ModalResult = mrOK then
      edtNumSerie.Text := frSelecionarCertificado.StringGrid1.Cells[ 0,
                            frSelecionarCertificado.StringGrid1.Row];

  finally
     frSelecionarCertificado.Free;
  end;
end;

procedure TfrmDemo_ACBrCIOT.btnSHA256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrCIOT1.SSL.CalcHash(edtHash.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pcRespostas.ActivePageIndex := 0;
end;

procedure TfrmDemo_ACBrCIOT.btnHTTPSClick(Sender: TObject);
var
  Acao: String;
  OldUseCert: Boolean;
begin
  Acao := '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'+
     '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" '+
     'xmlns:cli="http://cliente.bean.master.sigep.bsb.correios.com.br/"> '+
     ' <soapenv:Header/>'+
     ' <soapenv:Body>' +
     ' <cli:consultaCEP>' +
     ' <cep>18270-170</cep>' +
     ' </cli:consultaCEP>' +
     ' </soapenv:Body>' +
     ' </soapenv:Envelope>';

  OldUseCert := ACBrCIOT1.SSL.UseCertificateHTTP;
  ACBrCIOT1.SSL.UseCertificateHTTP := False;
  try
    MemoResp.Lines.Text := ACBrCIOT1.SSL.Enviar(Acao, 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl', '');
  finally
    ACBrCIOT1.SSL.UseCertificateHTTP := OldUseCert;
  end;
  pcRespostas.ActivePageIndex := 0;
end;

procedure TfrmDemo_ACBrCIOT.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrCIOT1.SSL do
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
    pcRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmDemo_ACBrCIOT.sbPathCIOTClick(Sender: TObject);
var
 Dir : string;
begin
 if Length(edtPathCIOT.Text) <= 0
  then Dir := ExtractFileDir(application.ExeName)
  else Dir := edtPathCIOT.Text;

 if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP)
  then edtPathCIOT.Text := Dir;
end;

end.
